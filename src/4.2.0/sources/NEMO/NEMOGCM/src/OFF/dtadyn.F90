MODULE dtadyn
   !!======================================================================
   !!                       ***  MODULE  dtadyn  ***
   !! Off-line : interpolation of the physical fields
   !!======================================================================
   !! History :   OPA  ! 1992-01 (M. Imbard) Original code
   !!             8.0  ! 1998-04 (L.Bopp MA Foujols) slopes for isopyc. 
   !!              -   ! 1998-05 (L. Bopp) read output of coupled run
   !!             8.2  ! 2001-01 (M. Levy et M. Benjelloul) add netcdf FORMAT
   !!   NEMO      1.0  ! 2005-03 (O. Aumont and A. El Moussaoui) F90
   !!              -   ! 2005-12 (C. Ethe) Adapted for DEGINT
   !!             3.0  ! 2007-06 (C. Ethe) use of iom module
   !!             3.3  ! 2010-11 (C. Ethe) Full reorganization of the off-line: phasing with the on-line
   !!             3.4  ! 2011-05 (C. Ethe) Use of fldread
   !!             4.1  ! 2019-08 (A. Coward, D. Storkey) split dta_dyn_sf_swp -> dta_dyn_sf_atf and dta_dyn_sf_interp
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dta_dyn_init : initialization, namelist read, and SAVEs control
   !!   dta_dyn      : Interpolation of the fields
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers variables
   USE c1d             ! 1D configuration: ln_c1d
   USE dom_oce         ! ocean domain: variables
#if defined key_qco 
   USE domqco          ! variable volume
#else
   USE domvvl
#endif
   USE zdf_oce         ! ocean vertical physics: variables
   USE sbc_oce         ! surface module: variables
   USE trc_oce         ! share ocean/biogeo variables
   USE phycst          ! physical constants
   USE trabbl          ! active tracer: bottom boundary layer
   USE ldfslp          ! lateral diffusion: iso-neutral slopes
   USE sbcrnf          ! river runoffs
   USE ldftra          ! ocean tracer   lateral physics
   USE zdfmxl          ! vertical physics: mixed layer depth
   USE eosbn2          ! equation of state - Brunt Vaisala frequency
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE zpshde          ! z-coord. with partial steps: horizontal derivatives
   USE in_out_manager  ! I/O manager
   USE iom             ! I/O library
   USE lib_mpp         ! distributed memory computing library
   USE prtctl          ! print control
   USE fldread         ! read input fields 
   USE timing          ! Timing
   USE trc, ONLY : ln_rsttr, lrst_trc

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dta_dyn_init       ! called by nemo_init
   PUBLIC   dta_dyn            ! called by nemo_gcm
   PUBLIC   dta_dyn_atf        ! called by nemo_gcm
#if ! defined key_qco
   PUBLIC   dta_dyn_sf_interp  ! called by nemo_gcm
#endif
#if defined key_sed_off
   PUBLIC   dta_dyn_sed_init   ! called by nemo_init
   PUBLIC   dta_dyn_sed        ! called by nemo_gcm
#endif

   CHARACTER(len=100) ::   cn_dir          !: Root directory for location of ssr files
   LOGICAL            ::   ln_dynrnf       !: read runoff data in file (T) or set to zero (F)
   LOGICAL            ::   ln_dynrnf_depth       !: read runoff data in file (T) or set to zero (F)


   INTEGER  , PARAMETER ::   jpfld = 20     ! maximum number of fields to read
   INTEGER  , SAVE      ::   jf_tem         ! index of temperature
   INTEGER  , SAVE      ::   jf_sal         ! index of salinity
   INTEGER  , SAVE      ::   jf_uwd         ! index of u-transport
   INTEGER  , SAVE      ::   jf_vwd         ! index of v-transport
   INTEGER  , SAVE      ::   jf_wwd         ! index of w-transport
   INTEGER  , SAVE      ::   jf_avt         ! index of Kz
   INTEGER  , SAVE      ::   jf_mld         ! index of mixed layer deptht
   INTEGER  , SAVE      ::   jf_emp         ! index of water flux
   INTEGER  , SAVE      ::   jf_empb        ! index of water flux
   INTEGER  , SAVE      ::   jf_qsr         ! index of solar radiation
   INTEGER  , SAVE      ::   jf_wnd         ! index of wind speed
   INTEGER  , SAVE      ::   jf_ice         ! index of sea ice cover
   INTEGER  , SAVE      ::   jf_rnf         ! index of river runoff
   INTEGER  , SAVE      ::   jf_fmf         ! index of downward salt flux
   INTEGER  , SAVE      ::   jf_ubl         ! index of u-bbl coef
   INTEGER  , SAVE      ::   jf_vbl         ! index of v-bbl coef
   INTEGER  , SAVE      ::   jf_div         ! index of e3t


   TYPE(FLD), ALLOCATABLE, SAVE, DIMENSION(:) :: sf_dyn  ! structure of input fields (file informations, fields read)
   !                                               ! 
   REAL(wp) , ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) :: uslpdta    ! zonal isopycnal slopes
   REAL(wp) , ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) :: vslpdta    ! meridional isopycnal slopes
   REAL(wp) , ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) :: wslpidta   ! zonal diapycnal slopes
   REAL(wp) , ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) :: wslpjdta   ! meridional diapycnal slopes

   INTEGER, SAVE  :: nprevrec, nsecdyn

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   
   !!----------------------------------------------------------------------
   !! NEMO/OFF 4.0 , NEMO Consortium (2018)
   !! $Id: dtadyn.F90 15090 2021-07-06 14:25:18Z cetlod $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dta_dyn( kt, Kbb, Kmm, Kaa )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dta_dyn  ***
      !!
      !! ** Purpose :  Prepares dynamics and physics fields from a NEMO run
      !!               for an off-line simulation of passive tracers
      !!
      !! ** Method : calculates the position of data 
      !!             - computes slopes if needed
      !!             - interpolates data if needed
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt             ! ocean time-step index
      INTEGER, INTENT(in) ::   Kbb, Kmm, Kaa  ! ocean time level indices
      !
      INTEGER             ::   ji, jj, jk
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)   ::   zemp
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   zhdivtr
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start( 'dta_dyn')
      !
      nsecdyn = nsec_year + nsec1jan000   ! number of seconds between Jan. 1st 00h of nit000 year and the middle of time step
      !
      IF( kt == nit000 ) THEN    ;    nprevrec = 0
      ELSE                       ;    nprevrec = sf_dyn(jf_tem)%nrec(2,sf_dyn(jf_tem)%naa)
      ENDIF
      CALL fld_read( kt, 1, sf_dyn )      !=  read data at kt time step   ==!
      !
      IF( l_ldfslp .AND. .NOT.ln_c1d )   CALL  dta_dyn_slp( kt, Kbb, Kmm )    ! Computation of slopes
      !
      ts(:,:,:,jp_tem,Kmm) = sf_dyn(jf_tem)%fnow(:,:,:)  * tmask(:,:,:)    ! temperature
      ts(:,:,:,jp_sal,Kmm) = sf_dyn(jf_sal)%fnow(:,:,:)  * tmask(:,:,:)    ! salinity
      wndm(:,:)            = sf_dyn(jf_wnd)%fnow(:,:,1)  * tmask(:,:,1)    ! wind speed - needed for gas exchange
      fmmflx(:,:)          = sf_dyn(jf_fmf)%fnow(:,:,1)  * tmask(:,:,1)    ! downward salt flux (v3.5+)
      fr_i(:,:)            = sf_dyn(jf_ice)%fnow(:,:,1)  * tmask(:,:,1)    ! Sea-ice fraction
      qsr (:,:)            = sf_dyn(jf_qsr)%fnow(:,:,1)  * tmask(:,:,1)    ! solar radiation
      emp (:,:)            = sf_dyn(jf_emp)%fnow(:,:,1)  * tmask(:,:,1)    ! E-P
      IF( ln_dynrnf ) THEN 
         rnf (:,:)         = sf_dyn(jf_rnf)%fnow(:,:,1) * tmask(:,:,1)    ! E-P
         IF( ln_dynrnf_depth .AND. .NOT. ln_linssh )    CALL  dta_dyn_rnf( Kmm )
      ENDIF
      !
      uu(:,:,:,Kmm)        = sf_dyn(jf_uwd)%fnow(:,:,:) * umask(:,:,:)    ! effective u-transport
      vv(:,:,:,Kmm)        = sf_dyn(jf_vwd)%fnow(:,:,:) * vmask(:,:,:)    ! effective v-transport
      ww(:,:,:)            = sf_dyn(jf_wwd)%fnow(:,:,:) * tmask(:,:,:)    ! effective v-transport
      !
      IF( .NOT.ln_linssh ) THEN
         ALLOCATE( zemp(jpi,jpj) , zhdivtr(jpi,jpj,jpk) )
         zhdivtr(:,:,:) = sf_dyn(jf_div)%fnow(:,:,:)  * tmask(:,:,:)    ! effective u-transport
         emp_b  (:,:)   = sf_dyn(jf_empb)%fnow(:,:,1) * tmask(:,:,1)    ! E-P
         zemp   (:,:)   = ( 0.5_wp * ( emp(:,:) + emp_b(:,:) ) + rnf(:,:) ) * tmask(:,:,1)
#if defined key_qco
         CALL dta_dyn_ssh( kt, zhdivtr, ssh(:,:,Kbb), zemp, ssh(:,:,Kaa) )
         CALL dom_qco_r3c( ssh(:,:,Kaa), r3t(:,:,Kaa), r3u(:,:,Kaa), r3v(:,:,Kaa) )
#else
         CALL dta_dyn_ssh( kt, zhdivtr, ssh(:,:,Kbb), zemp, ssh(:,:,Kaa), e3t(:,:,:,Kaa) )  !=  ssh, vertical scale factor
#endif
         DEALLOCATE( zemp , zhdivtr )
         !                                           Write in the tracer restart file
         !                                          *********************************
         IF( lrst_trc ) THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'dta_dyn_ssh : ssh field written in tracer restart file at it= ', kt,' date= ', ndastp
            IF(lwp) WRITE(numout,*) '~~~~~~~~~~~'
            CALL iom_rstput( kt, nitrst, numrtw, 'sshn', ssh(:,:,Kaa) )
            CALL iom_rstput( kt, nitrst, numrtw, 'sshb', ssh(:,:,Kmm) )
         ENDIF
      ENDIF
      !
      CALL eos    ( ts(:,:,:,:,Kmm), rhd, rhop, gdept_0(:,:,:) ) ! In any case, we need rhop
      CALL eos_rab( ts(:,:,:,:,Kmm), rab_n, Kmm )       ! now    local thermal/haline expension ratio at T-points
      CALL bn2    ( ts(:,:,:,:,Kmm), rab_n, rn2, Kmm )  ! before Brunt-Vaisala frequency need for zdfmxl

      rn2b(:,:,:) = rn2(:,:,:)         ! needed for zdfmxl
      CALL zdf_mxl( kt, Kmm )          ! In any case, we need mxl
      !
      hmld(:,:)       = sf_dyn(jf_mld)%fnow(:,:,1) * tmask(:,:,1)    ! mixed layer depht
      avt(:,:,:)      = sf_dyn(jf_avt)%fnow(:,:,:) * tmask(:,:,:)    ! vertical diffusive coefficient 
      avs(:,:,:)      = avt(:,:,:)
      !
      IF( ln_trabbl .AND. .NOT.ln_c1d ) THEN       ! diffusive Bottom boundary layer param 
         ahu_bbl(:,:) = sf_dyn(jf_ubl)%fnow(:,:,1) * umask(:,:,1)    ! bbl diffusive coef
         ahv_bbl(:,:) = sf_dyn(jf_vbl)%fnow(:,:,1) * vmask(:,:,1)
      ENDIF
      !
      !
      CALL eos( ts(:,:,:,:,Kmm), rhd, rhop, gdept_0(:,:,:) ) ! In any case, we need rhop
      !
      IF(sn_cfctl%l_prtctl) THEN                 ! print control
         CALL prt_ctl(tab3d_1=ts(:,:,:,jp_tem,Kmm), clinfo1=' tn      - : ', mask1=tmask )
         CALL prt_ctl(tab3d_1=ts(:,:,:,jp_sal,Kmm), clinfo1=' sn      - : ', mask1=tmask )
         CALL prt_ctl(tab3d_1=uu(:,:,:,Kmm)               , clinfo1=' uu(:,:,:,Kmm)      - : ', mask1=umask )
         CALL prt_ctl(tab3d_1=vv(:,:,:,Kmm)               , clinfo1=' vv(:,:,:,Kmm)      - : ', mask1=vmask )
         CALL prt_ctl(tab3d_1=ww               , clinfo1=' ww      - : ', mask1=tmask )
         CALL prt_ctl(tab3d_1=avt              , clinfo1=' kz      - : ', mask1=tmask )
         CALL prt_ctl(tab3d_1=uslp             , clinfo1=' slp  - u : ', tab3d_2=vslp, clinfo2=' v : ' )
         CALL prt_ctl(tab3d_1=wslpi            , clinfo1=' slp  - wi: ', tab3d_2=wslpj, clinfo2=' wj: ' )
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop( 'dta_dyn')
      !
   END SUBROUTINE dta_dyn


   SUBROUTINE dta_dyn_init( Kbb, Kmm, Kaa )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dta_dyn_init  ***
      !!
      !! ** Purpose :   Initialisation of the dynamical data     
      !! ** Method  : - read the data namdta_dyn namelist
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   Kbb, Kmm, Kaa  ! ocean time level indices
      !
      INTEGER  :: ierr, ierr0, ierr1, ierr2, ierr3   ! return error code
      INTEGER  :: ifpr                               ! dummy loop indice
      INTEGER  :: jfld                               ! dummy loop arguments
      INTEGER  :: inum, idv, idimv                   ! local integer
      INTEGER  :: ios                                ! Local integer output status for namelist read
      INTEGER  :: ji, jj, jk
      !!
      CHARACTER(len=100)            ::  cn_dir        !   Root directory for location of core files
      TYPE(FLD_N), DIMENSION(jpfld) ::  slf_d         ! array of namelist informations on the fields to read
      TYPE(FLD_N) :: sn_uwd, sn_vwd, sn_wwd, sn_empb, sn_emp  ! informations about the fields to be read
      TYPE(FLD_N) :: sn_tem , sn_sal , sn_avt   !   "                 "
      TYPE(FLD_N) :: sn_mld, sn_qsr, sn_wnd , sn_ice , sn_fmf   !   "               "
      TYPE(FLD_N) :: sn_ubl, sn_vbl, sn_rnf    !   "              "
      TYPE(FLD_N) :: sn_div  ! informations about the fields to be read
      !!
      NAMELIST/namdta_dyn/cn_dir, ln_dynrnf, ln_dynrnf_depth, &
         &                sn_uwd, sn_vwd, sn_wwd, sn_emp,               &
         &                sn_avt, sn_tem, sn_sal, sn_mld , sn_qsr ,     &
         &                sn_wnd, sn_ice, sn_fmf,                       &
         &                sn_ubl, sn_vbl, sn_rnf,                       &
         &                sn_empb, sn_div 
      !!----------------------------------------------------------------------
      !
      READ  ( numnam_ref, namdta_dyn, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namdta_dyn in reference namelist' )
      READ  ( numnam_cfg, namdta_dyn, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namdta_dyn in configuration namelist' )
      IF(lwm) WRITE ( numond, namdta_dyn )
      !                                         ! store namelist information in an array
      !                                         ! Control print
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'dta_dyn : offline dynamics '
         WRITE(numout,*) '~~~~~~~ '
         WRITE(numout,*) '   Namelist namdta_dyn'
         WRITE(numout,*) '      runoffs option enabled (T) or not (F)            ln_dynrnf        = ', ln_dynrnf
         WRITE(numout,*) '      runoffs is spread in vertical                    ln_dynrnf_depth  = ', ln_dynrnf_depth
         WRITE(numout,*)
      ENDIF
      ! 
      jf_uwd  = 1     ;   jf_vwd  = 2    ;   jf_wwd = 3    ;   jf_emp = 4    ;   jf_avt = 5
      jf_tem  = 6     ;   jf_sal  = 7    ;   jf_mld = 8    ;   jf_qsr = 9
      jf_wnd  = 10    ;   jf_ice  = 11   ;   jf_fmf = 12   ;   jfld   = jf_fmf
      !
      slf_d(jf_uwd)  = sn_uwd    ;   slf_d(jf_vwd)  = sn_vwd   ;   slf_d(jf_wwd) = sn_wwd
      slf_d(jf_emp)  = sn_emp    ;   slf_d(jf_avt)  = sn_avt
      slf_d(jf_tem)  = sn_tem    ;   slf_d(jf_sal)  = sn_sal   ;   slf_d(jf_mld) = sn_mld
      slf_d(jf_qsr)  = sn_qsr    ;   slf_d(jf_wnd)  = sn_wnd   ;   slf_d(jf_ice) = sn_ice
      slf_d(jf_fmf)  = sn_fmf
      !
      IF( .NOT.ln_linssh ) THEN
               jf_div  = jfld + 1   ;         jf_empb  = jfld + 2    ;   jfld = jf_empb
         slf_d(jf_div) = sn_div     ;   slf_d(jf_empb) = sn_empb
      ENDIF
      !
      IF( ln_trabbl ) THEN
               jf_ubl  = jfld + 1   ;         jf_vbl  = jfld + 2     ;   jfld = jf_vbl
         slf_d(jf_ubl) = sn_ubl     ;   slf_d(jf_vbl) = sn_vbl
      ENDIF
      !
      IF( ln_dynrnf ) THEN
               jf_rnf  = jfld + 1   ;     jfld  = jf_rnf
         slf_d(jf_rnf) = sn_rnf    
      ELSE
         rnf(:,:) = 0._wp
      ENDIF

      ALLOCATE( sf_dyn(jfld), STAT=ierr )         ! set sf structure
      IF( ierr > 0 )  THEN
         CALL ctl_stop( 'dta_dyn: unable to allocate sf structure' )   ;   RETURN
      ENDIF
      !                                         ! fill sf with slf_i and control print
      CALL fld_fill( sf_dyn, slf_d, cn_dir, 'dta_dyn_init', 'Data in file', 'namdta_dyn' )
      sf_dyn(jf_uwd)%cltype = 'U'   ;   sf_dyn(jf_uwd)%zsgn = -1._wp  
      sf_dyn(jf_vwd)%cltype = 'V'   ;   sf_dyn(jf_vwd)%zsgn = -1._wp  
      !
      IF( ln_trabbl ) THEN
         sf_dyn(jf_ubl)%cltype = 'U'   ;   sf_dyn(jf_ubl)%zsgn =  1._wp  
         sf_dyn(jf_vbl)%cltype = 'V'   ;   sf_dyn(jf_vbl)%zsgn =  1._wp  
      END IF
      !
      ! Open file for each variable to get his number of dimension
      DO ifpr = 1, jfld
         CALL fld_def( sf_dyn(ifpr) )
         CALL iom_open( sf_dyn(ifpr)%clname, sf_dyn(ifpr)%num )
         idv   = iom_varid( sf_dyn(ifpr)%num , slf_d(ifpr)%clvar )        ! id of the variable sdjf%clvar
         idimv = iom_file ( sf_dyn(ifpr)%num )%ndims(idv)                 ! number of dimension for variable sdjf%clvar
         CALL iom_close( sf_dyn(ifpr)%num )                               ! close file if already open
         ierr1=0
         IF( idimv == 3 ) THEN    ! 2D variable
                                      ALLOCATE( sf_dyn(ifpr)%fnow(jpi,jpj,1)    , STAT=ierr0 )
            IF( slf_d(ifpr)%ln_tint ) ALLOCATE( sf_dyn(ifpr)%fdta(jpi,jpj,1,2)  , STAT=ierr1 )
         ELSE                     ! 3D variable
                                      ALLOCATE( sf_dyn(ifpr)%fnow(jpi,jpj,jpk)  , STAT=ierr0 )
            IF( slf_d(ifpr)%ln_tint ) ALLOCATE( sf_dyn(ifpr)%fdta(jpi,jpj,jpk,2), STAT=ierr1 )
         ENDIF
         IF( ierr0 + ierr1 > 0 ) THEN
            CALL ctl_stop( 'dta_dyn_init : unable to allocate sf_dyn array structure' )   ;   RETURN
         ENDIF
      END DO
      !
      IF( l_ldfslp .AND. .NOT.ln_c1d ) THEN                  ! slopes 
         IF( sf_dyn(jf_tem)%ln_tint ) THEN      ! time interpolation
            ALLOCATE( uslpdta (jpi,jpj,jpk,2), vslpdta (jpi,jpj,jpk,2),    &
            &         wslpidta(jpi,jpj,jpk,2), wslpjdta(jpi,jpj,jpk,2), STAT=ierr2 )
            !
            IF( ierr2 > 0 )  THEN
               CALL ctl_stop( 'dta_dyn_init : unable to allocate slope arrays' )   ;   RETURN
            ENDIF
         ENDIF
      ENDIF
      !
      IF( .NOT.ln_linssh ) THEN
        IF( .NOT. sf_dyn(jf_uwd)%ln_clim .AND. ln_rsttr .AND.    &                     ! Restart: read in restart file
           iom_varid( numrtr, 'sshn', ldstop = .FALSE. ) > 0 ) THEN
           IF(lwp) WRITE(numout,*) ' ssh(:,:,Kmm) forcing fields read in the restart file for initialisation'
           CALL iom_get( numrtr, jpdom_auto, 'sshn', ssh(:,:,Kmm)   )
           CALL iom_get( numrtr, jpdom_auto, 'sshb', ssh(:,:,Kbb)   )
        ELSE
           IF(lwp) WRITE(numout,*) ' ssh(:,:,Kmm) forcing fields read in the restart file for initialisation'
           CALL iom_open( 'restart', inum )
           CALL iom_get( inum, jpdom_auto, 'sshn', ssh(:,:,Kmm)   )
           CALL iom_get( inum, jpdom_auto, 'sshb', ssh(:,:,Kbb)   )
           CALL iom_close( inum )                                        ! close file
        ENDIF
        !
#if defined key_qco
        CALL dom_qco_r3c( ssh(:,:,Kbb), r3t(:,:,Kbb), r3u(:,:,Kbb), r3v(:,:,Kbb) )
        CALL dom_qco_r3c( ssh(:,:,Kmm), r3t(:,:,Kmm), r3u(:,:,Kmm), r3v(:,:,Kmm) )
#else
        DO jk = 1, jpkm1
           e3t(:,:,jk,Kmm) = e3t_0(:,:,jk) * ( 1._wp + ssh(:,:,Kmm) * r1_ht_0(:,:) * tmask(:,:,jk) )
           e3t(:,:,jk,Kbb) = e3t_0(:,:,jk) * ( 1._wp + ssh(:,:,Kbb) * r1_ht_0(:,:) * tmask(:,:,jk) )
        ENDDO

        CALL dta_dyn_sf_interp( nit000, Kmm )
        CALL dta_dyn_sf_interp( nit000, Kbb )
#endif
      ENDIF
      !
      ncpl_qsr_freq = sf_dyn(jf_qsr)%freqh  * 3600   !  Get qsr frequency ( needed if diurnal cycle in TOP
      !
      CALL dta_dyn_rnf_init( Kmm )
      !
      CALL dta_dyn( nit000, Kbb, Kmm, Kaa )
      !
   END SUBROUTINE dta_dyn_init

   SUBROUTINE dta_dyn_atf( kt, Kbb, Kmm, Kaa )
     !!---------------------------------------------------------------------
      !!                    ***  ROUTINE dta_dyn_swp  ***
      !!
      !! ** Purpose :   Asselin time filter of now SSH
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) :: kt             ! time step
      INTEGER, INTENT(in) :: Kbb, Kmm, Kaa  ! ocean time level indices
      !
      !!---------------------------------------------------------------------

      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'dta_dyn_atf : Asselin time filter of sea surface height'
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~ '
      ENDIF

      ssh(:,:,Kmm) = ssh(:,:,Kmm) + rn_atfp * ( ssh(:,:,Kbb) - 2 * ssh(:,:,Kmm) + ssh(:,:,Kaa))  

      !
   END SUBROUTINE dta_dyn_atf
   
   
#if ! defined key_qco  

   SUBROUTINE dta_dyn_sf_interp( kt, Kmm )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE dta_dyn_sf_interp  ***
      !!
      !! ** Purpose :   Calculate scale factors at U/V/W points and depths
      !!                given the after e3t field
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) :: kt   ! time step
      INTEGER, INTENT(in) :: Kmm  ! ocean time level indices
      !
      INTEGER             :: ji, jj, jk
      REAL(wp)            :: zcoef
      !!---------------------------------------------------------------------

      ! Horizontal scale factor interpolations
      ! --------------------------------------
      CALL dom_vvl_interpol( e3t(:,:,:,Kmm), e3u(:,:,:,Kmm), 'U' )
      CALL dom_vvl_interpol( e3t(:,:,:,Kmm), e3v(:,:,:,Kmm), 'V' )

      ! Vertical scale factor interpolations
      ! ------------------------------------
      CALL dom_vvl_interpol( e3t(:,:,:,Kmm), e3w (:,:,:,Kmm), 'W' )

      ! t- and w- points depth
      ! ----------------------
      gdept(:,:,1,Kmm) = 0.5_wp * e3w(:,:,1,Kmm)
      gdepw(:,:,1,Kmm) = 0.0_wp
      !
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 2, jpk )
         zcoef = (tmask(ji,jj,jk) - wmask(ji,jj,jk))
         gdepw(ji,jj,jk,Kmm) = gdepw(ji,jj,jk-1,Kmm) + e3t(ji,jj,jk-1,Kmm)
         gdept(ji,jj,jk,Kmm) =      zcoef  * ( gdepw(ji,jj,jk  ,Kmm) + 0.5 * e3w(ji,jj,jk,Kmm))  &
            &                + (1-zcoef) * ( gdept(ji,jj,jk-1,Kmm) + e3w(ji,jj,jk,Kmm))
      END_3D
      !
   END SUBROUTINE dta_dyn_sf_interp

#endif

   SUBROUTINE dta_dyn_ssh( kt, phdivtr, psshb,  pemp, pssha, pe3ta )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE dta_dyn_wzv  ***
      !!                   
      !! ** Purpose :   compute the after ssh (ssh(:,:,Kaa)) and the now vertical velocity
      !!
      !! ** Method  : Using the incompressibility hypothesis, 
      !!        - the ssh increment is computed by integrating the horizontal divergence 
      !!          and multiply by the time step.
      !!
      !!        - compute the after scale factor : repartition of ssh INCREMENT proportionnaly
      !!                                           to the level thickness ( z-star case )
      !!
      !!        - the vertical velocity is computed by integrating the horizontal divergence  
      !!          from the bottom to the surface minus the scale factor evolution.
      !!          The boundary conditions are w=0 at the bottom (no flux)
      !!
      !! ** action  :   ssh(:,:,Kaa) / e3t(:,:,k,Kaa) / ww
      !!
      !! Reference  : Leclair, M., and G. Madec, 2009, Ocean Modelling.
      !!----------------------------------------------------------------------
      INTEGER,                                   INTENT(in )    :: kt        !  time-step
      REAL(wp), DIMENSION(jpi,jpj,jpk)          , INTENT(in )   :: phdivtr   ! horizontal divergence transport
      REAL(wp), DIMENSION(jpi,jpj)    , OPTIONAL, INTENT(in )   :: psshb     ! now ssh
      REAL(wp), DIMENSION(jpi,jpj)    , OPTIONAL, INTENT(in )   :: pemp      ! evaporation minus precipitation
      REAL(wp), DIMENSION(jpi,jpj)    , OPTIONAL, INTENT(inout) :: pssha     ! after ssh
      REAL(wp), DIMENSION(jpi,jpj,jpk), OPTIONAL, INTENT(out)   :: pe3ta     ! after vertical scale factor
      !
      INTEGER                       :: jk
      REAL(wp), DIMENSION(jpi,jpj)  :: zhdiv  
      REAL(wp)  :: z2dt  
      !!----------------------------------------------------------------------
      !
      z2dt = 2._wp * rn_Dt
      !
      zhdiv(:,:) = 0._wp
      DO jk = 1, jpkm1
         zhdiv(:,:) = zhdiv(:,:) +  phdivtr(:,:,jk) * tmask(:,:,jk)
      END DO
      !                                                ! Sea surface  elevation time-stepping
      pssha(:,:) = ( psshb(:,:) - z2dt * ( r1_rho0 * pemp(:,:)  + zhdiv(:,:) ) ) * ssmask(:,:)
      !
      IF( PRESENT( pe3ta ) ) THEN                      ! After acale factors at t-points ( z_star coordinate )
      DO jk = 1, jpkm1
            pe3ta(:,:,jk) = e3t_0(:,:,jk) * ( 1._wp + pssha(:,:) * r1_ht_0(:,:) * tmask(:,:,jk) )
      END DO
      ENDIF
      !
   END SUBROUTINE dta_dyn_ssh

   SUBROUTINE dta_dyn_rnf_init( Kmm )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dta_dyn_rnf_init  ***
      !!
      !! ** Purpose :   Initialisation of the runoffs if (ln_rnf=T)
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) :: Kmm  ! ocean time level indices
      !
      INTEGER  :: inum                   ! local integer
      INTEGER  :: ji, jj, jk
      REAL(wp) :: zcoef
      INTEGER  :: nkrnf_max
      REAL(wp) :: hrnf_max

      IF( ln_dynrnf .AND. ln_dynrnf_depth ) THEN       ! read depht over which runoffs are distributed
         IF(lwp) WRITE(numout,*) 
         IF(lwp) WRITE(numout,*) ' read in the file depht over which runoffs are distributed'
         CALL iom_open ( "runoffs", inum )                           ! open file
         CALL iom_get  ( inum, jpdom_global, 'rodepth', h_rnf )   ! read the river mouth array
         CALL iom_close( inum )                                        ! close file
         !
         nk_rnf(:,:) = 0                               ! set the number of level over which river runoffs are applied
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            IF( h_rnf(ji,jj) > 0._wp ) THEN
               jk = 2
               DO WHILE ( jk /= mbkt(ji,jj) .AND. gdept_0(ji,jj,jk) < h_rnf(ji,jj) ) ;  jk = jk + 1
               END DO
               nk_rnf(ji,jj) = jk
            ELSEIF( h_rnf(ji,jj) == -1._wp   ) THEN   ;  nk_rnf(ji,jj) = 1
            ELSEIF( h_rnf(ji,jj) == -999._wp ) THEN   ;  nk_rnf(ji,jj) = mbkt(ji,jj)
            ELSE
               CALL ctl_stop( 'sbc_rnf_init: runoff depth not positive, and not -999 or -1, rnf value in file fort.999'  )
               WRITE(999,*) 'ji, jj, h_rnf(ji,jj) :', ji, jj, h_rnf(ji,jj)
            ENDIF
         END_2D
         !
         ! set the associated depth
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            h_rnf(ji,jj) = 0._wp
            DO jk = 1, nk_rnf(ji,jj)
               h_rnf(ji,jj) = h_rnf(ji,jj) + e3t(ji,jj,jk,Kmm)
            END DO
         END_2D
      ELSE                                       ! runoffs applied at the surface
         nk_rnf(:,:) = 1
         h_rnf (:,:) = e3t(:,:,1,Kmm)
      ENDIF
      nkrnf_max = MAXVAL( nk_rnf(:,:) )
      hrnf_max = MAXVAL( h_rnf(:,:) )
      IF( lk_mpp )  THEN
         CALL mpp_max( 'dtadyn', nkrnf_max )                 ! max over the  global domain
         CALL mpp_max( 'dtadyn', hrnf_max )                 ! max over the  global domain
      ENDIF
      IF(lwp) WRITE(numout,*) ' '
      IF(lwp) WRITE(numout,*) ' max depht of runoff : ', hrnf_max,'    max level  : ', nkrnf_max
      IF(lwp) WRITE(numout,*) ' '
      !
   END SUBROUTINE dta_dyn_rnf_init

   SUBROUTINE dta_dyn_rnf( Kmm )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dta_dyn_rnf  ***
      !!
      !! ** Purpose :   update the horizontal divergence with the runoff inflow
      !!
      !!----------------------------------------------------------------------
      !!
      INTEGER, INTENT(in) :: Kmm  ! ocean time level index
      !
      INTEGER  ::   ji, jj, jk    ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      !  update the depth over which runoffs are distributed
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         h_rnf(ji,jj) = 0._wp
         DO jk = 1, nk_rnf(ji,jj)                           ! recalculates h_rnf to be the depth in metres
             h_rnf(ji,jj) = h_rnf(ji,jj) + e3t(ji,jj,jk,Kmm)   ! to the bottom of the relevant grid box
         END DO
      END_2D
      !
   END SUBROUTINE dta_dyn_rnf

   SUBROUTINE dta_dyn_slp( kt, Kbb, Kmm )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE dta_dyn_slp  ***
      !!
      !! ** Purpose : Computation of slope
      !!
      !!---------------------------------------------------------------------
      INTEGER,  INTENT(in) :: kt       ! time step
      INTEGER,  INTENT(in) :: Kbb, Kmm ! ocean time level indices
      !
      INTEGER  ::   ji, jj     ! dummy loop indices
      REAL(wp) ::   ztinta     ! ratio applied to after  records when doing time interpolation
      REAL(wp) ::   ztintb     ! ratio applied to before records when doing time interpolation
      INTEGER  ::   iswap 
      REAL(wp), DIMENSION(jpi,jpj,jpk)      ::   zuslp, zvslp, zwslpi, zwslpj
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts) ::   zts
      !!---------------------------------------------------------------------
      !
      IF( sf_dyn(jf_tem)%ln_tint ) THEN    ! Computes slopes (here avt is used as workspace)
         !
         IF( kt == nit000 ) THEN
            IF(lwp) WRITE(numout,*) ' Compute new slopes at kt = ', kt
            zts(:,:,:,jp_tem) = sf_dyn(jf_tem)%fdta(:,:,:,sf_dyn(jf_tem)%nbb) * tmask(:,:,:)   ! temperature
            zts(:,:,:,jp_sal) = sf_dyn(jf_sal)%fdta(:,:,:,sf_dyn(jf_sal)%nbb) * tmask(:,:,:)   ! salinity 
            avt(:,:,:)        = sf_dyn(jf_avt)%fdta(:,:,:,sf_dyn(jf_avt)%nbb) * tmask(:,:,:)   ! vertical diffusive coef.
            CALL compute_slopes( kt, zts, zuslp, zvslp, zwslpi, zwslpj, Kbb, Kmm )
            uslpdta (:,:,:,1) = zuslp (:,:,:) 
            vslpdta (:,:,:,1) = zvslp (:,:,:) 
            wslpidta(:,:,:,1) = zwslpi(:,:,:) 
            wslpjdta(:,:,:,1) = zwslpj(:,:,:) 
            !
            zts(:,:,:,jp_tem) = sf_dyn(jf_tem)%fdta(:,:,:,sf_dyn(jf_tem)%naa) * tmask(:,:,:)   ! temperature
            zts(:,:,:,jp_sal) = sf_dyn(jf_sal)%fdta(:,:,:,sf_dyn(jf_sal)%naa) * tmask(:,:,:)   ! salinity 
            avt(:,:,:)        = sf_dyn(jf_avt)%fdta(:,:,:,sf_dyn(jf_avt)%naa) * tmask(:,:,:)   ! vertical diffusive coef.
            CALL compute_slopes( kt, zts, zuslp, zvslp, zwslpi, zwslpj, Kbb, Kmm )
            uslpdta (:,:,:,2) = zuslp (:,:,:) 
            vslpdta (:,:,:,2) = zvslp (:,:,:) 
            wslpidta(:,:,:,2) = zwslpi(:,:,:) 
            wslpjdta(:,:,:,2) = zwslpj(:,:,:) 
         ELSE
           ! 
           iswap = 0
           IF( sf_dyn(jf_tem)%nrec(2,sf_dyn(jf_tem)%naa) - nprevrec /= 0 )  iswap = 1
           IF( nsecdyn > sf_dyn(jf_tem)%nrec(2,sf_dyn(jf_tem)%nbb) .AND. iswap == 1 )  THEN    ! read/update the after data
              IF(lwp) WRITE(numout,*) ' Compute new slopes at kt = ', kt
              uslpdta (:,:,:,1) =  uslpdta (:,:,:,2)         ! swap the data
              vslpdta (:,:,:,1) =  vslpdta (:,:,:,2)  
              wslpidta(:,:,:,1) =  wslpidta(:,:,:,2) 
              wslpjdta(:,:,:,1) =  wslpjdta(:,:,:,2) 
              !
              zts(:,:,:,jp_tem) = sf_dyn(jf_tem)%fdta(:,:,:,sf_dyn(jf_tem)%naa) * tmask(:,:,:)   ! temperature
              zts(:,:,:,jp_sal) = sf_dyn(jf_sal)%fdta(:,:,:,sf_dyn(jf_sal)%naa) * tmask(:,:,:)   ! salinity 
              avt(:,:,:)        = sf_dyn(jf_avt)%fdta(:,:,:,sf_dyn(jf_avt)%naa) * tmask(:,:,:)   ! vertical diffusive coef.
              CALL compute_slopes( kt, zts, zuslp, zvslp, zwslpi, zwslpj, Kbb, Kmm )
              !
              uslpdta (:,:,:,2) = zuslp (:,:,:) 
              vslpdta (:,:,:,2) = zvslp (:,:,:) 
              wslpidta(:,:,:,2) = zwslpi(:,:,:) 
              wslpjdta(:,:,:,2) = zwslpj(:,:,:) 
            ENDIF
         ENDIF
      ENDIF
      !
      IF( sf_dyn(jf_tem)%ln_tint )  THEN
         ztinta =  REAL( nsecdyn - sf_dyn(jf_tem)%nrec(2,sf_dyn(jf_tem)%nbb), wp )  &
            &    / REAL( sf_dyn(jf_tem)%nrec(2,sf_dyn(jf_tem)%naa) - sf_dyn(jf_tem)%nrec(2,sf_dyn(jf_tem)%nbb), wp )
         ztintb =  1. - ztinta
         IF( l_ldfslp .AND. .NOT.ln_c1d ) THEN    ! Computes slopes (here avt is used as workspace)
            uslp (:,:,:) = ztintb * uslpdta (:,:,:,1)  + ztinta * uslpdta (:,:,:,2)  
            vslp (:,:,:) = ztintb * vslpdta (:,:,:,1)  + ztinta * vslpdta (:,:,:,2)  
            wslpi(:,:,:) = ztintb * wslpidta(:,:,:,1)  + ztinta * wslpidta(:,:,:,2)  
            wslpj(:,:,:) = ztintb * wslpjdta(:,:,:,1)  + ztinta * wslpjdta(:,:,:,2)  
         ENDIF
      ELSE
         zts(:,:,:,jp_tem) = sf_dyn(jf_tem)%fnow(:,:,:) * tmask(:,:,:)   ! temperature
         zts(:,:,:,jp_sal) = sf_dyn(jf_sal)%fnow(:,:,:) * tmask(:,:,:)   ! salinity 
         avt(:,:,:)        = sf_dyn(jf_avt)%fnow(:,:,:) * tmask(:,:,:)   ! vertical diffusive coef.
         CALL compute_slopes( kt, zts, zuslp, zvslp, zwslpi, zwslpj, Kbb, Kmm )
         !
         IF( l_ldfslp .AND. .NOT.ln_c1d ) THEN    ! Computes slopes (here avt is used as workspace)
            uslp (:,:,:) = zuslp (:,:,:)
            vslp (:,:,:) = zvslp (:,:,:)
            wslpi(:,:,:) = zwslpi(:,:,:)
            wslpj(:,:,:) = zwslpj(:,:,:)
         ENDIF
      ENDIF
      !
   END SUBROUTINE dta_dyn_slp

   SUBROUTINE compute_slopes( kt, pts, puslp, pvslp, pwslpi, pwslpj, Kbb, Kmm )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE dta_dyn_slp  ***
      !!
      !! ** Purpose :   Computation of slope
      !!---------------------------------------------------------------------
      INTEGER ,                              INTENT(in ) :: kt       ! time step
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts), INTENT(inout) :: pts      ! temperature/salinity
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(out) :: puslp    ! zonal isopycnal slopes
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(out) :: pvslp    ! meridional isopycnal slopes
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(out) :: pwslpi   ! zonal diapycnal slopes
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(out) :: pwslpj   ! meridional diapycnal slopes
      INTEGER ,                              INTENT(in ) :: Kbb, Kmm ! ocean time level indices
      !!---------------------------------------------------------------------
      !
      IF( l_ldfslp .AND. .NOT.ln_c1d ) THEN    ! Computes slopes (here avt is used as workspace)
         CALL eos    ( pts, rhd, rhop, gdept_0(:,:,:) )
         CALL eos_rab( pts, rab_n, Kmm )       ! now local thermal/haline expension ratio at T-points
         CALL bn2    ( pts, rab_n, rn2, Kmm  ) ! now    Brunt-Vaisala

      ! Partial steps: before Horizontal DErivative
      IF( ln_zps  .AND. .NOT. ln_isfcav)                            &
         &            CALL zps_hde    ( kt, Kmm, jpts, pts, gtsu, gtsv,  &  ! Partial steps: before horizontal gradient
         &                                        rhd, gru , grv    )  ! of t, s, rd at the last ocean level
      IF( ln_zps .AND.        ln_isfcav)                            &
         &            CALL zps_hde_isf( kt, Kmm, jpts, pts, gtsu, gtsv, gtui, gtvi, &  ! Partial steps for top cell (ISF)
         &                                        rhd, gru , grv , grui, grvi )  ! of t, s, rd at the first ocean level

         rn2b(:,:,:) = rn2(:,:,:)                ! needed for zdfmxl
         CALL zdf_mxl( kt, Kmm )                 ! mixed layer depth
         CALL ldf_slp( kt, rhd, rn2, Kbb, Kmm )  ! slopes
         puslp (:,:,:) = uslp (:,:,:)
         pvslp (:,:,:) = vslp (:,:,:)
         pwslpi(:,:,:) = wslpi(:,:,:)
         pwslpj(:,:,:) = wslpj(:,:,:)
     ELSE
         puslp (:,:,:) = 0.            ! to avoid warning when compiling
         pvslp (:,:,:) = 0.
         pwslpi(:,:,:) = 0.
         pwslpj(:,:,:) = 0.
     ENDIF
      !
   END SUBROUTINE compute_slopes

#if defined key_sed_off

   SUBROUTINE dta_dyn_sed( kt, Kmm )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dta_dyn  ***
      !!
      !! ** Purpose :  Prepares dynamics and physics fields from a NEMO run
      !!               for an off-line simulation of passive tracers
      !!
      !! ** Method : calculates the position of data
      !!             - computes slopes if needed
      !!             - interpolates data if needed
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time-step index
      INTEGER, INTENT(in) ::   Kmm  ! ocean time level index
      !
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start( 'dta_dyn_sed')
      !
      nsecdyn = nsec_year + nsec1jan000   ! number of seconds between Jan. 1st 00h of nit000 year and the middle of time step
      !
      IF( kt == nit000 ) THEN    ;    nprevrec = 0
      ELSE                       ;    nprevrec = sf_dyn(jf_tem)%nrec(2,sf_dyn(jf_tem)%naa)
      ENDIF
      CALL fld_read( kt, 1, sf_dyn )      !=  read data at kt time step   ==!
      !
      ts(:,:,:,jp_tem,Kmm) = sf_dyn(jf_tem)%fnow(:,:,:)  * tmask(:,:,:)    ! temperature
      ts(:,:,:,jp_sal,Kmm) = sf_dyn(jf_sal)%fnow(:,:,:)  * tmask(:,:,:)    ! salinity
      !
      CALL eos    ( ts(:,:,:,:,Kmm), rhd, rhop, gdept_0(:,:,:) ) ! In any case, we need rhop

      IF(sn_cfctl%l_prtctl) THEN                     ! print control
         CALL prt_ctl(tab3d_1=ts(:,:,:,jp_tem,Kmm), clinfo1=' tn      - : ', mask1=tmask )
         CALL prt_ctl(tab3d_1=ts(:,:,:,jp_sal,Kmm), clinfo1=' sn      - : ', mask1=tmask )
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop( 'dta_dyn_sed')
      !
   END SUBROUTINE dta_dyn_sed


   SUBROUTINE dta_dyn_sed_init( Kmm )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dta_dyn_init  ***
      !!
      !! ** Purpose :   Initialisation of the dynamical data
      !! ** Method  : - read the data namdta_dyn namelist
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) :: Kmm                   ! ocean time level index
      !
      INTEGER  :: ierr, ierr0, ierr1, ierr2, ierr3   ! return error code
      INTEGER  :: ifpr                               ! dummy loop indice
      INTEGER  :: jfld                               ! dummy loop arguments
      INTEGER  :: inum, idv, idimv                   ! local integer
      INTEGER  :: ios                                ! Local integer output status for namelist read
      !!
      CHARACTER(len=100)            ::  cn_dir        !   Root directory for location of core files
      TYPE(FLD_N), DIMENSION(2) ::  slf_d         ! array of namelist informations on the fields to read
      TYPE(FLD_N) :: sn_tem , sn_sal   !   "                 "
      !!
      NAMELIST/namdta_dyn/cn_dir, ln_dynrnf, ln_dynrnf_depth,  &
         &                sn_tem, sn_sal
      !!----------------------------------------------------------------------
      !
      READ  ( numnam_ref, namdta_dyn, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namdta_dyn in reference namelist' )
      READ  ( numnam_cfg, namdta_dyn, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namdta_dyn in configuration namelist' )
      IF(lwm) WRITE ( numond, namdta_dyn )
      !                                         ! store namelist information in an array
      !                                         ! Control print
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'dta_dyn : offline dynamics '
         WRITE(numout,*) '~~~~~~~ '
         WRITE(numout,*) '   Namelist namdta_dyn'
         WRITE(numout,*) '      runoffs option enabled (T) or not (F)            ln_dynrnf        = ', ln_dynrnf
         WRITE(numout,*) '      runoffs is spread in vertical                    ln_dynrnf_depth  = ', ln_dynrnf_depth
         WRITE(numout,*)
      ENDIF
      !
      jf_tem  = 1     ;   jf_sal  = 2    ;   jfld   = jf_sal
      !
      slf_d(jf_tem)  = sn_tem    ;   slf_d(jf_sal)  = sn_sal
      !
      ALLOCATE( sf_dyn(jfld), STAT=ierr )         ! set sf structure
      IF( ierr > 0 )  THEN
         CALL ctl_stop( 'dta_dyn: unable to allocate sf structure' )   ;   RETURN
      ENDIF
      !                                         ! fill sf with slf_i and control print
      CALL fld_fill( sf_dyn, slf_d, cn_dir, 'dta_dyn_init', 'Data in file', 'namdta_dyn' )
      !
      ! Open file for each variable to get his number of dimension
      DO ifpr = 1, jfld
         CALL fld_def( sf_dyn(ifpr) )
         CALL iom_open( sf_dyn(ifpr)%clname, sf_dyn(ifpr)%num )
         idv   = iom_varid( sf_dyn(ifpr)%num , slf_d(ifpr)%clvar )        ! id of the variable sdjf%clvar
         idimv = iom_file ( sf_dyn(ifpr)%num )%ndims(idv)                 ! number of dimension for variable sdjf%clvar
         CALL iom_close( sf_dyn(ifpr)%num )                               ! close file if already open
         ierr1=0
         IF( idimv == 3 ) THEN    ! 2D variable
                                      ALLOCATE( sf_dyn(ifpr)%fnow(jpi,jpj,1)    , STAT=ierr0 )
            IF( slf_d(ifpr)%ln_tint ) ALLOCATE( sf_dyn(ifpr)%fdta(jpi,jpj,1,2)  , STAT=ierr1 )
         ELSE                     ! 3D variable
                                      ALLOCATE( sf_dyn(ifpr)%fnow(jpi,jpj,jpk)  , STAT=ierr0 )
            IF( slf_d(ifpr)%ln_tint ) ALLOCATE( sf_dyn(ifpr)%fdta(jpi,jpj,jpk,2), STAT=ierr1 )
         ENDIF
         IF( ierr0 + ierr1 > 0 ) THEN
            CALL ctl_stop( 'dta_dyn_init : unable to allocate sf_dyn array structure' )   ;   RETURN
         ENDIF
      END DO
      !
      CALL dta_dyn_sed( nit000, Kmm )
      !
   END SUBROUTINE dta_dyn_sed_init
#endif
   !!======================================================================
END MODULE dtadyn
