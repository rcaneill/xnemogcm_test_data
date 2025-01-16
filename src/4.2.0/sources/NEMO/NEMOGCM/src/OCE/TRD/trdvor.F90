MODULE trdvor
   !!======================================================================
   !!                       ***  MODULE  trdvor  ***
   !! Ocean diagnostics:  momentum trends
   !!=====================================================================
   !! History :  1.0  !  2006-01  (L. Brunier, A-M. Treguier) Original code 
   !!            2.0  !  2008-04  (C. Talandier) New trends organization
   !!            3.5  !  2012-02  (G. Madec) regroup beta.V computation with pvo trend
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   trd_vor      : momentum trends averaged over the depth
   !!   trd_vor_zint : vorticity vertical integration
   !!   trd_vor_init : initialization step
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean space and time domain variables
   USE trd_oce         ! trends: ocean variables
   USE zdf_oce         ! ocean vertical physics
   USE sbc_oce         ! surface boundary condition: ocean
   USE phycst          ! Define parameters for the routines
   USE ldfdyn          ! ocean active tracers: lateral physics
   USE dianam          ! build the name of file (routine)
   USE zdfmxl          ! mixed layer depth
   !
   USE in_out_manager  ! I/O manager
   USE ioipsl          ! NetCDF library
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp         ! MPP library

   IMPLICIT NONE
   PRIVATE

   INTERFACE trd_vor_zint
      MODULE PROCEDURE trd_vor_zint_2d, trd_vor_zint_3d
   END INTERFACE

   PUBLIC   trd_vor        ! routine called by trddyn.F90
   PUBLIC   trd_vor_init   ! routine called by opa.F90
   PUBLIC   trd_vor_alloc  ! routine called by nemogcm.F90

   INTEGER ::   nh_t, nmoydpvor, nidvor, nhoridvor, ndimvor1, icount   ! needs for IOIPSL output
   INTEGER, SAVE, ALLOCATABLE, DIMENSION(:) ::   ndexvor1   ! needed for IOIPSL output
   INTEGER ::   ndebug     ! (0/1) set it to 1 in case of problem to have more print

   REAL(wp), SAVE, ALLOCATABLE, DIMENSION(:,:)   ::   vor_avr      ! average
   REAL(wp), SAVE, ALLOCATABLE, DIMENSION(:,:)   ::   vor_avrb     ! before vorticity (kt-1)
   REAL(wp), SAVE, ALLOCATABLE, DIMENSION(:,:)   ::   vor_avrbb    ! vorticity at begining of the nn_write-1 timestep averaging period
   REAL(wp), SAVE, ALLOCATABLE, DIMENSION(:,:)   ::   vor_avrbn    ! after vorticity at time step after the
   REAL(wp), SAVE, ALLOCATABLE, DIMENSION(:,:)   ::   rotot        ! begining of the NN_WRITE-1 timesteps
   REAL(wp), SAVE, ALLOCATABLE, DIMENSION(:,:)   ::   vor_avrtot   !
   REAL(wp), SAVE, ALLOCATABLE, DIMENSION(:,:)   ::   vor_avrres   !
   REAL(wp), SAVE, ALLOCATABLE, DIMENSION(:,:,:) ::   vortrd       ! curl of trends
         
   CHARACTER(len=12) ::   cvort

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: trdvor.F90 15033 2021-06-21 10:24:45Z smasson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION trd_vor_alloc()
      !!----------------------------------------------------------------------------
      !!                  ***  ROUTINE trd_vor_alloc  ***
      !!----------------------------------------------------------------------------
      ALLOCATE( vor_avr   (jpi,jpj) , vor_avrb(jpi,jpj) , vor_avrbb (jpi,jpj) ,   &
         &      vor_avrbn (jpi,jpj) , rotot   (jpi,jpj) , vor_avrtot(jpi,jpj) ,   &
         &      vor_avrres(jpi,jpj) , vortrd  (jpi,jpj,jpltot_vor) ,              &
         &      ndexvor1  (jpi*jpj)                                ,   STAT= trd_vor_alloc )
         !
      CALL mpp_sum ( 'trdvor', trd_vor_alloc )
      IF( trd_vor_alloc /= 0 )   CALL ctl_stop( 'STOP', 'trd_vor_alloc: failed to allocate arrays' )
   END FUNCTION trd_vor_alloc


   SUBROUTINE trd_vor( putrd, pvtrd, ktrd, kt, Kmm )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trd_vor  ***
      !! 
      !! ** Purpose :  computation of cumulated trends over analysis period
      !!               and make outputs (NetCDF format)
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:,:), INTENT(inout) ::   putrd, pvtrd   ! U and V trends 
      INTEGER                   , INTENT(in   ) ::   ktrd           ! trend index
      INTEGER                   , INTENT(in   ) ::   kt             ! time step
      INTEGER                   , INTENT(in   ) ::   Kmm            ! time level index
      !
      INTEGER ::   ji, jj   ! dummy loop indices
      REAL(wp), DIMENSION(jpi,jpj) ::   ztswu, ztswv    ! 2D workspace 
      !!----------------------------------------------------------------------

      CALL lbc_lnk( 'trdvor', putrd, 'U', -1.0_wp , pvtrd, 'V', -1.0_wp )      ! lateral boundary condition

      SELECT CASE( ktrd ) 
      CASE( jpdyn_hpg )   ;   CALL trd_vor_zint( putrd, pvtrd, jpvor_prg, Kmm )   ! Hydrostatique Pressure Gradient 
      CASE( jpdyn_keg )   ;   CALL trd_vor_zint( putrd, pvtrd, jpvor_keg, Kmm )   ! KE Gradient 
      CASE( jpdyn_rvo )   ;   CALL trd_vor_zint( putrd, pvtrd, jpvor_rvo, Kmm )   ! Relative Vorticity 
      CASE( jpdyn_pvo )   ;   CALL trd_vor_zint( putrd, pvtrd, jpvor_pvo, Kmm )   ! Planetary Vorticity Term 
      CASE( jpdyn_ldf )   ;   CALL trd_vor_zint( putrd, pvtrd, jpvor_ldf, Kmm )   ! Horizontal Diffusion 
      CASE( jpdyn_zad )   ;   CALL trd_vor_zint( putrd, pvtrd, jpvor_zad, Kmm )   ! Vertical Advection 
      CASE( jpdyn_spg )   ;   CALL trd_vor_zint( putrd, pvtrd, jpvor_spg, Kmm )   ! Surface Pressure Grad. 
      CASE( jpdyn_zdf )                                                           ! Vertical Diffusion 
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )                                                               ! wind stress trends
            ztswu(ji,jj) = 0.5 * ( utau_b(ji,jj) + utau(ji,jj) ) / ( e3u(ji,jj,1,Kmm) * rho0 )
            ztswv(ji,jj) = 0.5 * ( vtau_b(ji,jj) + vtau(ji,jj) ) / ( e3v(ji,jj,1,Kmm) * rho0 )
         END_2D
         CALL trd_vor_zint( putrd, pvtrd, jpvor_zdf, Kmm )                             ! zdf trend including surf./bot. stresses 
         CALL trd_vor_zint( ztswu, ztswv, jpvor_swf, Kmm )                             ! surface wind stress 
      CASE( jpdyn_bfr )
         CALL trd_vor_zint( putrd, pvtrd, jpvor_bfr, Kmm )                             ! Bottom stress
         !
      CASE( jpdyn_atf )       ! last trends: perform the output of 2D vorticity trends
         CALL trd_vor_iom( kt, Kmm )
      END SELECT
      !
   END SUBROUTINE trd_vor


   SUBROUTINE trd_vor_zint_2d( putrdvor, pvtrdvor, ktrd, Kmm )
      !!----------------------------------------------------------------------------
      !!                  ***  ROUTINE trd_vor_zint  ***
      !!
      !! ** Purpose :   computation of vertically integrated vorticity budgets
      !!              from ocean surface down to control surface (NetCDF output)
      !!
      !! ** Method/usage :   integration done over nn_write-1 time steps
      !!
      !! ** Action :   trends :
      !!                  vortrd (,, 1) = Pressure Gradient Trend
      !!                  vortrd (,, 2) = KE Gradient Trend
      !!                  vortrd (,, 3) = Relative Vorticity Trend
      !!                  vortrd (,, 4) = Coriolis Term Trend
      !!                  vortrd (,, 5) = Horizontal Diffusion Trend
      !!                  vortrd (,, 6) = Vertical Advection Trend
      !!                  vortrd (,, 7) = Vertical Diffusion Trend
      !!                  vortrd (,, 8) = Surface Pressure Grad. Trend
      !!                  vortrd (,, 9) = Beta V
      !!                  vortrd (,,10) = forcing term
      !!		  vortrd (,,11) = bottom friction term
      !!                  rotot(,) : total cumulative trends over nn_write-1 time steps
      !!                  vor_avrtot(,) : first membre of vrticity equation
      !!                  vor_avrres(,) : residual = dh/dt entrainment
      !!
      !!      trends output in netCDF format using ioipsl
      !!----------------------------------------------------------------------
      INTEGER                     , INTENT(in) ::   ktrd       ! ocean trend index
      INTEGER                     , INTENT(in) ::   Kmm        ! time level index
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) ::   putrdvor   ! u vorticity trend 
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) ::   pvtrdvor   ! v vorticity trend
      !
      INTEGER ::   ji, jj       ! dummy loop indices
      INTEGER ::   ikbu, ikbv   ! local integers
      REAL(wp), DIMENSION(jpi,jpj) :: zudpvor, zvdpvor  ! total cmulative trends
      !!----------------------------------------------------------------------

      !  =====================================
      !  I vertical integration of 2D trends
      !  =====================================

      SELECT CASE( ktrd ) 
      !
      CASE( jpvor_bfr )        ! bottom friction
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            ikbu = mbkv(ji,jj)
            ikbv = mbkv(ji,jj)            
            zudpvor(ji,jj) = putrdvor(ji,jj) * e3u(ji,jj,ikbu,Kmm) * e1u(ji,jj) * umask(ji,jj,ikbu)
            zvdpvor(ji,jj) = pvtrdvor(ji,jj) * e3v(ji,jj,ikbv,Kmm) * e2v(ji,jj) * vmask(ji,jj,ikbv)
         END_2D
         !
      CASE( jpvor_swf )        ! wind stress
         zudpvor(:,:) = putrdvor(:,:) * e3u(:,:,1,Kmm) * e1u(:,:) * umask(:,:,1)
         zvdpvor(:,:) = pvtrdvor(:,:) * e3v(:,:,1,Kmm) * e2v(:,:) * vmask(:,:,1)
         !
      END SELECT

      ! Average except for Beta.V
      zudpvor(:,:) = zudpvor(:,:) * r1_hu(:,:,Kmm)
      zvdpvor(:,:) = zvdpvor(:,:) * r1_hv(:,:,Kmm)
   
      ! Curl
      DO_2D( 0, 0, 0, 0 )
         vortrd(ji,jj,ktrd) = (    zvdpvor(ji+1,jj) - zvdpvor(ji,jj)       &
            &                  - ( zudpvor(ji,jj+1) - zudpvor(ji,jj) )   ) &
            &                  / ( e1f(ji,jj) * e2f(ji,jj) ) * fmask(ji,jj,1)
      END_2D

      IF( ndebug /= 0 ) THEN
         IF(lwp) WRITE(numout,*) ' debuging trd_vor_zint: I done'
         CALL FLUSH(numout)
      ENDIF
      !
   END SUBROUTINE trd_vor_zint_2d


   SUBROUTINE trd_vor_zint_3d( putrdvor, pvtrdvor, ktrd , Kmm )
      !!----------------------------------------------------------------------------
      !!                  ***  ROUTINE trd_vor_zint  ***
      !!
      !! ** Purpose :   computation of vertically integrated vorticity budgets
      !!              from ocean surface down to control surface (NetCDF output)
      !!
      !! ** Method/usage :   integration done over nn_write-1 time steps
      !!
      !! ** Action :     trends :
      !!                  vortrd (,,1) = Pressure Gradient Trend
      !!                  vortrd (,,2) = KE Gradient Trend
      !!                  vortrd (,,3) = Relative Vorticity Trend
      !!                  vortrd (,,4) = Coriolis Term Trend
      !!                  vortrd (,,5) = Horizontal Diffusion Trend
      !!                  vortrd (,,6) = Vertical Advection Trend
      !!                  vortrd (,,7) = Vertical Diffusion Trend
      !!                  vortrd (,,8) = Surface Pressure Grad. Trend
      !!                  vortrd (,,9) = Beta V
      !!                  vortrd (,,10) = forcing term
      !!		  vortrd (,,11) = bottom friction term
      !!                  rotot(,) : total cumulative trends over nn_write-1 time steps
      !!                  vor_avrtot(,) : first membre of vrticity equation
      !!                  vor_avrres(,) : residual = dh/dt entrainment
      !!
      !!      trends output in netCDF format using ioipsl
      !!----------------------------------------------------------------------
      !
      INTEGER                         , INTENT(in) ::   ktrd       ! ocean trend index
      INTEGER                         , INTENT(in) ::   Kmm        ! time level index
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(in) ::   putrdvor   ! u vorticity trend 
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(in) ::   pvtrdvor   ! v vorticity trend
      !
      INTEGER ::   ji, jj, jk   ! dummy loop indices
      REAL(wp), DIMENSION(jpi,jpj) :: zudpvor, zvdpvor  ! total cmulative trends
      !!----------------------------------------------------------------------

      !  =====================================
      !  I vertical integration of 3D trends
      !  =====================================
      ! putrdvor and pvtrdvor terms
      DO jk = 1,jpk
        zudpvor(:,:) = zudpvor(:,:) + putrdvor(:,:,jk) * e3u(:,:,jk,Kmm) * e1u(:,:) * umask(:,:,jk)
        zvdpvor(:,:) = zvdpvor(:,:) + pvtrdvor(:,:,jk) * e3v(:,:,jk,Kmm) * e2v(:,:) * vmask(:,:,jk)
      END DO

      ! Planetary vorticity: 2nd computation (Beta.V term) store the vertical sum
      ! as Beta.V term need intergration, not average
      IF( ktrd == jpvor_pvo ) THEN 
         DO_2D( 0, 0, 0, 0 )
            vortrd(ji,jj,jpvor_bev) = (    zvdpvor(ji+1,jj) - zvdpvor(ji,jj)     &
               &                       - ( zudpvor(ji,jj+1) - zudpvor(ji,jj) ) ) &
               &                           / ( e1f(ji,jj) * e2f(ji,jj) ) * r1_hu(ji,jj,Kmm) * fmask(ji,jj,1)
         END_2D
      ENDIF
      !
      ! Average 
      zudpvor(:,:) = zudpvor(:,:) * r1_hu(:,:,Kmm)
      zvdpvor(:,:) = zvdpvor(:,:) * r1_hv(:,:,Kmm)
      !
      ! Curl
      DO_2D( 0, 0, 0, 0 )
         vortrd(ji,jj,ktrd) = (    zvdpvor(ji+1,jj) - zvdpvor(ji,jj)     &
            &                  - ( zudpvor(ji,jj+1) - zudpvor(ji,jj) ) ) &
            &                         / ( e1f(ji,jj) * e2f(ji,jj) ) * fmask(ji,jj,1)
      END_2D
   
      IF( ndebug /= 0 ) THEN
         IF(lwp) WRITE(numout,*) ' debuging trd_vor_zint: I done'
         CALL FLUSH(numout)
      ENDIF
      !
   END SUBROUTINE trd_vor_zint_3d


   SUBROUTINE trd_vor_iom( kt , Kmm )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trd_vor  ***
      !! 
      !! ** Purpose :  computation of cumulated trends over analysis period
      !!               and make outputs (NetCDF format)
      !!----------------------------------------------------------------------
      INTEGER                   , INTENT(in   ) ::   kt             ! time step
      INTEGER                   , INTENT(in   ) ::   Kmm            ! time level index
      !
      INTEGER  ::   ji, jj, jk, jl   ! dummy loop indices
      INTEGER  ::   it, itmod        ! local integers
      REAL(wp) ::   zmean            ! local scalars
      REAL(wp), DIMENSION(jpi,jpj) :: zuu, zvv
      !!----------------------------------------------------------------------

      !  =================
      !  I. Initialization
      !  =================
     
     
      ! I.1 set before values of vertically average u and v
      ! ---------------------------------------------------

      IF( kt > nit000 )   vor_avrb(:,:) = vor_avr(:,:)

      ! I.2 vertically integrated vorticity
      !  ----------------------------------

      vor_avr   (:,:) = 0._wp
      zuu       (:,:) = 0._wp
      zvv       (:,:) = 0._wp
      vor_avrtot(:,:) = 0._wp
      vor_avrres(:,:) = 0._wp
      
      ! Vertically averaged velocity
      DO jk = 1, jpk - 1
         zuu(:,:) = zuu(:,:) + e1u(:,:) * uu(:,:,jk,Kmm) * e3u(:,:,jk,Kmm)
         zvv(:,:) = zvv(:,:) + e2v(:,:) * vv(:,:,jk,Kmm) * e3v(:,:,jk,Kmm)
      END DO
 
      zuu(:,:) = zuu(:,:) * r1_hu(:,:,Kmm)
      zvv(:,:) = zvv(:,:) * r1_hv(:,:,Kmm)

      ! Curl
      DO_2D( 0, 0, 0, 0 )
         vor_avr(ji,jj) = (  ( zvv(ji+1,jj) - zvv(ji,jj) )    &
            &              - ( zuu(ji,jj+1) - zuu(ji,jj) ) )  &
            &             / ( e1f(ji,jj) * e2f(ji,jj) ) * fmask(ji,jj,1)
      END_2D
      
      !  =================================
      !   II. Cumulated trends
      !  =================================

      ! II.1 set `before' mixed layer values for kt = nit000+1
      ! ------------------------------------------------------
      IF( kt == nit000+1 ) THEN
         vor_avrbb(:,:) = vor_avrb(:,:)
         vor_avrbn(:,:) = vor_avr (:,:)
      ENDIF

      ! II.2 cumulated trends over analysis period (kt=2 to nn_write)
      ! ----------------------
      ! trends cumulated over nn_write-2 time steps

      IF( kt >= nit000+2 ) THEN
         nmoydpvor = nmoydpvor + 1
         DO jl = 1, jpltot_vor
            IF( jl /= 9 ) THEN
               rotot(:,:) = rotot(:,:) + vortrd(:,:,jl)
            ENDIF
         END DO
      ENDIF

      !  =============================================
      !   III. Output in netCDF + residual computation
      !  =============================================
      
      ! define time axis
      it    = kt
      itmod = kt - nit000 + 1

      IF( MOD( it, nn_trd ) == 0 ) THEN

         ! III.1 compute total trend
         ! ------------------------
         zmean = 1._wp / (  REAL( nmoydpvor, wp ) * 2._wp * rn_Dt  )
         vor_avrtot(:,:) = (  vor_avr(:,:) - vor_avrbn(:,:) + vor_avrb(:,:) - vor_avrbb(:,:) ) * zmean


         ! III.2 compute residual
         ! ---------------------
         zmean = 1._wp / REAL( nmoydpvor, wp )
         vor_avrres(:,:) = vor_avrtot(:,:) - rotot(:,:) / zmean

         ! Boundary conditions
         CALL lbc_lnk( 'trdvor', vor_avrtot, 'F', 1.0_wp , vor_avrres, 'F', 1.0_wp )


         ! III.3 time evolution array swap
         ! ------------------------------
         vor_avrbb(:,:) = vor_avrb(:,:)
         vor_avrbn(:,:) = vor_avr (:,:)
         !
         nmoydpvor = 0
         !
      ENDIF

      ! III.4 write trends to output
      ! ---------------------------

      IF( kt >=  nit000+1 ) THEN

         IF( lwp .AND. MOD( itmod, nn_trd ) == 0 ) THEN
            WRITE(numout,*) ''
            WRITE(numout,*) 'trd_vor : write trends in the NetCDF file at kt = ', kt
            WRITE(numout,*) '~~~~~~~  '
         ENDIF
 
         CALL histwrite( nidvor,"sovortPh",it,vortrd(:,:,jpvor_prg),ndimvor1,ndexvor1)  ! grad Ph
         CALL histwrite( nidvor,"sovortEk",it,vortrd(:,:,jpvor_keg),ndimvor1,ndexvor1)  ! Energy
         CALL histwrite( nidvor,"sovozeta",it,vortrd(:,:,jpvor_rvo),ndimvor1,ndexvor1)  ! rel vorticity
         CALL histwrite( nidvor,"sovortif",it,vortrd(:,:,jpvor_pvo),ndimvor1,ndexvor1)  ! coriolis
         CALL histwrite( nidvor,"sovodifl",it,vortrd(:,:,jpvor_ldf),ndimvor1,ndexvor1)  ! lat diff
         CALL histwrite( nidvor,"sovoadvv",it,vortrd(:,:,jpvor_zad),ndimvor1,ndexvor1)  ! vert adv
         CALL histwrite( nidvor,"sovodifv",it,vortrd(:,:,jpvor_zdf),ndimvor1,ndexvor1)  ! vert diff
         CALL histwrite( nidvor,"sovortPs",it,vortrd(:,:,jpvor_spg),ndimvor1,ndexvor1)  ! grad Ps
         CALL histwrite( nidvor,"sovortbv",it,vortrd(:,:,jpvor_bev),ndimvor1,ndexvor1)  ! beta.V
         CALL histwrite( nidvor,"sovowind",it,vortrd(:,:,jpvor_swf),ndimvor1,ndexvor1) ! wind stress
         CALL histwrite( nidvor,"sovobfri",it,vortrd(:,:,jpvor_bfr),ndimvor1,ndexvor1) ! bottom friction
         CALL histwrite( nidvor,"1st_mbre",it,vor_avrtot    ,ndimvor1,ndexvor1) ! First membre
         CALL histwrite( nidvor,"sovorgap",it,vor_avrres    ,ndimvor1,ndexvor1) ! gap between 1st and 2 nd mbre
         !
         IF( ndebug /= 0 ) THEN
            WRITE(numout,*) ' debuging trd_vor: III.4 done'
            CALL FLUSH(numout)
         ENDIF
         !
      ENDIF
      !
      IF( MOD( it, nn_trd ) == 0 ) rotot(:,:)=0
      !
      IF( kt == nitend )   CALL histclo( nidvor )
      !
   END SUBROUTINE trd_vor_iom


   SUBROUTINE trd_vor_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trd_vor_init  ***
      !! 
      !! ** Purpose :   computation of vertically integrated T and S budgets
      !!      from ocean surface down to control surface (NetCDF output)
      !!----------------------------------------------------------------------
      REAL(wp) ::   zjulian, zsto, zout
      CHARACTER (len=40) ::   clhstnam
      CHARACTER (len=40) ::   clop
      !!----------------------------------------------------------------------

      !  ===================
      !   I. initialization
      !  ===================

      cvort='averaged-vor'

      ! Open specifier
      ndebug = 0      ! set it to 1 in case of problem to have more Print

      IF(lwp) THEN
         WRITE(numout,*) ' '
         WRITE(numout,*) ' trd_vor_init: vorticity trends'
         WRITE(numout,*) ' ~~~~~~~~~~~~'
         WRITE(numout,*) ' '
         WRITE(numout,*) '               ##########################################################################'
         WRITE(numout,*) '                CAUTION: The interpretation of the vorticity trends is'
         WRITE(numout,*) '                not obvious, please contact Anne-Marie TREGUIER at: treguier@ifremer.fr '
         WRITE(numout,*) '               ##########################################################################'
         WRITE(numout,*) ' '
      ENDIF

      IF( trd_vor_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'trd_vor_init : unable to allocate trdvor arrays' )


      ! cumulated trends array init
      nmoydpvor = 0
      rotot(:,:)=0
      vor_avrtot(:,:)=0
      vor_avrres(:,:)=0

      IF( ndebug /= 0 ) THEN
         WRITE(numout,*) ' debuging trd_vor_init: I. done'
         CALL FLUSH(numout)
      ENDIF

      !  =================================
      !   II. netCDF output initialization
      !  =================================

      !-----------------------------------------
      ! II.1 Define frequency of output and means
      ! -----------------------------------------
      IF( ln_mskland )   THEN   ;   clop = "only(x)"   ! put 1.e+20 on land (very expensive!!)
      ELSE                      ;   clop = "x"         ! no use of the mask value (require less cpu time)
      ENDIF
#if defined key_diainstant
      zsto = nn_write*rn_Dt
      clop = "inst("//TRIM(clop)//")"
#else
      zsto = rn_Dt
      clop = "ave("//TRIM(clop)//")"
#endif
      zout = nn_trd*rn_Dt

      IF(lwp) WRITE(numout,*) '               netCDF initialization'

      ! II.2 Compute julian date from starting date of the run
      ! ------------------------
      CALL ymds2ju( nyear, nmonth, nday, rn_Dt, zjulian )
      zjulian = zjulian - adatrj   !   set calendar origin to the beginning of the experiment
      IF(lwp) WRITE(numout,*)' '  
      IF(lwp) WRITE(numout,*)'               Date 0 used :',nit000,    &
         &                   ' YEAR ', nyear,' MONTH '      , nmonth,   &
         &                   ' DAY ' , nday, 'Julian day : ', zjulian

      ! II.3 Define the T grid trend file (nidvor)
      ! ---------------------------------
      CALL dia_nam( clhstnam, nn_trd, 'vort' )                  ! filename
      IF(lwp) WRITE(numout,*) ' Name of NETCDF file ', clhstnam
      CALL histbeg( clhstnam, jpi, glamf, jpj, gphif,1, jpi,   &  ! Horizontal grid : glamt and gphit
         &          1, jpj, nit000-1, zjulian, rn_Dt, nh_t, nidvor, domain_id=nidom, snc4chunks=snc4set )
      CALL wheneq( jpi*jpj, fmask, 1, 1., ndexvor1, ndimvor1 )    ! surface

      ! Declare output fields as netCDF variables
      CALL histdef( nidvor, "sovortPh", cvort//"grad Ph" , "s-2",        & ! grad Ph
         &          jpi,jpj,nh_t,1,1,1,-99,32,clop,zsto,zout)
      CALL histdef( nidvor, "sovortEk", cvort//"Energy", "s-2",          & ! Energy
         &          jpi,jpj,nh_t,1,1,1,-99,32,clop,zsto,zout)
      CALL histdef( nidvor, "sovozeta", cvort//"rel vorticity", "s-2",   & ! rel vorticity
         &          jpi,jpj,nh_t,1,1,1,-99,32,clop,zsto,zout)
      CALL histdef( nidvor, "sovortif", cvort//"coriolis", "s-2",        & ! coriolis
         &          jpi,jpj,nh_t,1,1,1,-99,32,clop,zsto,zout)
      CALL histdef( nidvor, "sovodifl", cvort//"lat diff ", "s-2",       & ! lat diff
         &          jpi,jpj,nh_t,1,1,1,-99,32,clop,zsto,zout)
      CALL histdef( nidvor, "sovoadvv", cvort//"vert adv", "s-2",        & ! vert adv
         &          jpi,jpj,nh_t,1,1,1,-99,32,clop,zsto,zout)
      CALL histdef( nidvor, "sovodifv", cvort//"vert diff" , "s-2",      & ! vert diff
         &          jpi,jpj,nh_t,1,1,1,-99,32,clop,zsto,zout)
      CALL histdef( nidvor, "sovortPs", cvort//"grad Ps", "s-2",         & ! grad Ps
         &          jpi,jpj,nh_t,1,1,1,-99,32,clop,zsto,zout)
      CALL histdef( nidvor, "sovortbv", cvort//"Beta V", "s-2",          & ! beta.V
         &          jpi,jpj,nh_t,1,1,1,-99,32,clop,zsto,zout)
      CALL histdef( nidvor, "sovowind", cvort//"wind stress", "s-2",     & ! wind stress
         &          jpi,jpj,nh_t,1,1,1,-99,32,clop,zsto,zout)
      CALL histdef( nidvor, "sovobfri", cvort//"bottom friction", "s-2", & ! bottom friction
         &          jpi,jpj,nh_t,1,1,1,-99,32,clop,zsto,zout)
      CALL histdef( nidvor, "1st_mbre", cvort//"1st mbre", "s-2",        & ! First membre
         &          jpi,jpj,nh_t,1,1,1,-99,32,clop,zsto,zout)
      CALL histdef( nidvor, "sovorgap", cvort//"gap", "s-2",             & ! gap between 1st and 2 nd mbre
         &          jpi,jpj,nh_t,1,1,1,-99,32,clop,zsto,zout)
      CALL histend( nidvor, snc4set )

      IF( ndebug /= 0 ) THEN
         WRITE(numout,*) ' debuging trd_vor_init: II. done'
         CALL FLUSH(numout)
      ENDIF
      !
   END SUBROUTINE trd_vor_init

   !!======================================================================
END MODULE trdvor
