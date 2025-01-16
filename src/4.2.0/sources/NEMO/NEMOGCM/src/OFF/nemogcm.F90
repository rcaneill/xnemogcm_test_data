MODULE nemogcm
   !!======================================================================
   !!                       ***  MODULE nemogcm   ***
   !! Off-line Ocean   : passive tracer evolution, dynamics read in files
   !!======================================================================
   !! History :  3.3  ! 2010-05  (C. Ethe)  Full reorganization of the off-line: phasing with the on-line
   !!            3.4  ! 2011-01  (C. Ethe, A. R. Porter, STFC Daresbury) dynamical allocation
   !!            4.0  ! 2016-10  (C. Ethe, G. Madec, S. Flavoni)  domain configuration / user defined interface
   !!            4.1  ! 2019-08  (A. Coward, D. Storkey) rewrite in preparation for new timestepping scheme
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   nemo_gcm      : off-line: solve ocean tracer only
   !!   nemo_gcm      : solve ocean dynamics, tracer, biogeochemistry and/or sea-ice
   !!   nemo_init     : initialization of the NEMO system
   !!   nemo_ctl      : initialisation of the contol print
   !!   nemo_closefile: close remaining open files
   !!   nemo_alloc    : dynamical allocation
   !!   istate_init   : simple initialization to zero of ocean fields
   !!   stp_ctl       : reduced step control (no dynamics in off-line)
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space domain variables
   USE oce            ! dynamics and tracers variables
   USE trc_oce        ! Shared ocean/passive tracers variables
   USE c1d            ! 1D configuration
   USE domain         ! domain initialization from coordinate & bathymetry (dom_init routine)
   USE closea         ! treatment of closed seas (for ln_closea)
   USE usrdef_nam     ! user defined configuration
   USE eosbn2         ! equation of state            (eos bn2 routine)
#if defined key_qco
   USE domqco         ! tools for scale factor         (dom_qco_r3c  routine)
#endif
   USE bdyini         ! open boundary cond. setting        (bdy_init routine)
   !              ! ocean physics
   USE ldftra         ! lateral diffusivity setting    (ldf_tra_init routine)
   USE ldfslp         ! slopes of neutral surfaces     (ldf_slp_init routine)
   USE traqsr         ! solar radiation penetration    (tra_qsr_init routine)
   USE trabbl         ! bottom boundary layer          (tra_bbl_init routine)
   USE traldf         ! lateral physics                (tra_ldf_init routine)
   USE sbcmod         ! surface boundary condition     (sbc_init     routine)
   USE phycst         ! physical constant                   (par_cst routine)
   USE zdfphy         ! vertical physics manager       (zdf_phy_init routine)
   USE dtadyn         ! Lecture and Interpolation of the dynamical fields
   USE trcini         ! Initilization of the passive tracers
   USE daymod         ! calendar                            (day     routine)
   USE trcstp         ! passive tracer time-stepping        (trc_stp routine)
   USE dtadyn         ! Lecture and interpolation of the dynamical fields
   !              ! Passive tracers needs
   USE trc            ! passive tracer : variables
   USE trcnam         ! passive tracer : namelist
   USE trcrst         ! passive tracer restart
   USE sbc_oce , ONLY : ln_rnf
   USE sbcrnf         ! surface boundary condition : runoffs
   !              ! I/O & MPP
   USE iom            ! I/O library
   USE in_out_manager ! I/O manager
   USE mppini         ! shared/distributed memory setting (mpp_init routine)
   USE lib_mpp        ! distributed memory computing
#if defined key_xios
   USE xios           ! xIOserver
#endif 
   USE prtctl         ! Print control                    (prt_ctl_init routine)
   USE timing         ! Timing
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)
#if defined key_qco
   USE stpmlf , ONLY : Nbb, Nnn, Naa, Nrhs   ! time level indices
#else
   USE step    , ONLY : Nbb, Nnn, Naa, Nrhs   ! time level indices
#endif
   USE halo_mng

   IMPLICIT NONE
   PRIVATE
   
   PUBLIC   nemo_gcm   ! called by nemo.F90

   CHARACTER (len=64) ::   cform_aaa="( /, 'AAAAAAAA', / ) "   ! flag for output listing
#if ! defined key_mpi_off
   ! need MPI_Wtime
   INCLUDE 'mpif.h'
#endif

   !!----------------------------------------------------------------------
   !! NEMO/OFF 4.0 , NEMO Consortium (2018)
   !! $Id: nemogcm.F90 15446 2021-10-26 14:34:38Z cetlod $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE nemo_gcm
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE nemo_gcm  ***
      !!
      !! ** Purpose :   NEMO solves the primitive equations on an orthogonal
      !!              curvilinear mesh on the sphere.
      !!
      !! ** Method  : - model general initialization
      !!              - launch the time-stepping (dta_dyn and trc_stp)
      !!              - finalize the run by closing files and communications
      !!
      !! References : Madec, Delecluse,Imbard, and Levy, 1997:  internal report, IPSL.
      !!              Madec, 2008, internal report, IPSL.
      !!----------------------------------------------------------------------
      INTEGER :: istp       ! time step index
      REAL(wp)::   zstptiming   ! elapsed time for 1 time step
      !!----------------------------------------------------------------------

      CALL nemo_init  ! Initializations

      ! check that all process are still there... If some process have an error,
      ! they will never enter in step and other processes will wait until the end of the cpu time!
      CALL mpp_max( 'nemogcm', nstop )

      !                            !-----------------------!
      !                            !==   time stepping   ==!
      !                            !-----------------------!
      istp = nit000
      !
      IF( ln_rnf )   CALL sbc_rnf(istp)   ! runoffs initialization 
      ! 
      CALL iom_init( cxios_context )      ! iom_put initialization (must be done after nemo_init for AGRIF+XIOS+OASIS)
      ! 
      DO WHILE ( istp <= nitend .AND. nstop == 0 )    !==  OFF time-stepping  ==!
         IF( ln_timing ) THEN
            zstptiming = MPI_Wtime()
            IF ( istp == ( nit000 + 1 ) ) elapsed_time = zstptiming
            IF ( istp ==         nitend ) elapsed_time = zstptiming - elapsed_time
         ENDIF
         !
      IF((istp == nitrst) .AND. lwxios) THEN
         CALL iom_swap(      cw_toprst_cxt          )
         CALL iom_init_closedef(cw_toprst_cxt)
         CALL iom_setkt( istp - nit000 + 1,      cw_toprst_cxt          )
      ENDIF

         IF( istp /= nit000 )   CALL day        ( istp )         ! Calendar (day was already called at nit000 in day_init)
                                CALL iom_setkt  ( istp - nit000 + 1, cxios_context )   ! say to iom that we are at time step kstp
#if ! defined key_sed_off
                                CALL dta_dyn    ( istp, Nbb, Nnn, Naa )       ! Interpolation of the dynamical fields
         IF( .NOT.ln_linssh ) THEN
                                CALL dta_dyn_atf( istp, Nbb, Nnn, Naa )       ! time filter of sea  surface height and vertical scale factors
# if defined key_qco
                                CALL dom_qco_r3c( ssh(:,:,Nnn), r3t_f, r3u_f, r3v_f )
# endif
         ENDIF
                                CALL trc_stp    ( istp, Nbb, Nnn, Nrhs, Naa ) ! time-stepping
         ! Swap time levels
         Nrhs = Nbb
         Nbb  = Nnn
         Nnn  = Naa
         Naa  = Nrhs
         !
# if ! defined key_qco
         IF( .NOT.ln_linssh )   CALL dta_dyn_sf_interp( istp, Nnn )  ! calculate now grid parameters
# endif  

#else
                                CALL dta_dyn_sed( istp,      Nnn      )       ! Interpolation of the dynamical fields
                                CALL trc_stp    ( istp, Nbb, Nnn, Nrhs, Naa ) ! time-stepping
         ! Swap time levels
         Nnn = Nbb
         Naa = Nbb
#endif
         CALL stp_ctl    ( istp )             ! Time loop: control and print
         istp = istp + 1

         IF( lwp .AND. ln_timing )   WRITE(numtime,*) 'timing step ', istp-1, ' : ', MPI_Wtime() - zstptiming

      END DO
      !
#if defined key_xios
      CALL iom_context_finalize( cxios_context ) ! needed for XIOS+AGRIF
#endif

      !                            !------------------------!
      !                            !==  finalize the run  ==!
      !                            !------------------------!
      IF(lwp) WRITE(numout,cform_aaa)                 ! Flag AAAAAAA

      IF( nstop /= 0 .AND. lwp ) THEN                 ! error print
         WRITE(ctmp1,*) '   ==>>>   nemo_gcm: a total of ', nstop, ' errors have been found'
         WRITE(ctmp2,*) '           Look for "E R R O R" messages in all existing ocean_output* files'
         CALL ctl_stop( ' ', ctmp1, ' ', ctmp2 )
      ENDIF
      !
      IF( ln_timing )   CALL timing_finalize
      !
      CALL nemo_closefile
      !
#if defined key_xios
                     CALL xios_finalize   ! end mpp communications with xios
#else
      IF( lk_mpp )   CALL mppstop         ! end mpp communications
#endif
      !
      IF(lwm) THEN
         IF( nstop == 0 ) THEN   ;   STOP 0
         ELSE                    ;   STOP 123
         ENDIF
      ENDIF
      !
   END SUBROUTINE nemo_gcm


   SUBROUTINE nemo_init
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE nemo_init  ***
      !!
      !! ** Purpose :   initialization of the nemo model in off-line mode
      !!----------------------------------------------------------------------
      INTEGER ::   ios, ilocal_comm   ! local integers
      !!
      NAMELIST/namctl/ sn_cfctl, ln_timing, ln_diacfl,                                &
         &             nn_isplt,  nn_jsplt,  nn_ictls, nn_ictle, nn_jctls, nn_jctle
      NAMELIST/namcfg/ ln_read_cfg, cn_domcfg, ln_closea, ln_write_cfg, cn_domcfg_out, ln_use_jattr
      !!----------------------------------------------------------------------
      !
      cxios_context = 'nemo'
      nn_hls = 1
      !
      !                             !-------------------------------------------------!
      !                             !     set communicator & select the local rank    !
      !                             !  must be done as soon as possible to get narea  !
      !                             !-------------------------------------------------!
      !
#if defined key_xios
      CALL xios_initialize( "for_xios_mpi_id", return_comm=ilocal_comm )   ! nemo local communicator given by xios
      CALL mpp_start( ilocal_comm )
#else
      CALL mpp_start( )
#endif
      !
      narea = mpprank + 1               ! mpprank: the rank of proc (0 --> mppsize -1 )
      lwm = (narea == 1)                ! control of output namelists
      !
      !                             !---------------------------------------------------------------!
      !                             ! Open output files, reference and configuration namelist files !
      !                             !---------------------------------------------------------------!
      !
      ! open ocean.output as soon as possible to get all output prints (including errors messages)
      IF( lwm )   CALL ctl_opn(     numout,        'ocean.output', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, -1, .FALSE. )
      ! open reference and configuration namelist files
                  CALL load_nml( numnam_ref,        'namelist_ref',                                           -1, lwm )
                  CALL load_nml( numnam_cfg,        'namelist_cfg',                                           -1, lwm )
      IF( lwm )   CALL ctl_opn(     numond, 'output.namelist.dyn', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, -1, .FALSE. )
      ! open /dev/null file to be able to supress output write easily
      IF( Agrif_Root() ) THEN
                  CALL ctl_opn(     numnul,           '/dev/null', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, -1, .FALSE. )
#ifdef key_agrif
      ELSE
                  numnul = Agrif_Parent(numnul)   
#endif
      ENDIF
      !
      !                             !--------------------!
      !                             ! Open listing units !  -> need sn_cfctl from namctl to define lwp
      !                             !--------------------!
      !
      READ  ( numnam_ref, namctl, IOSTAT = ios, ERR = 901 )
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namctl in reference namelist' )
      READ  ( numnam_cfg, namctl, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namctl in configuration namelist' )
      !
      ! finalize the definition of namctl variables
      IF( narea < sn_cfctl%procmin .OR. narea > sn_cfctl%procmax .OR. MOD( narea - sn_cfctl%procmin, sn_cfctl%procincr ) /= 0 )   &
         &   CALL nemo_set_cfctl( sn_cfctl, .FALSE. )
      !
      lwp = (narea == 1) .OR. sn_cfctl%l_oceout    ! control of all listing output print
      !
      IF(lwp) THEN                            ! open listing units
         !
         IF( .NOT. lwm )   &           ! alreay opened for narea == 1
            &     CALL ctl_opn(     numout,        'ocean.output', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, -1, .FALSE., narea )
         !
         WRITE(numout,*)
         WRITE(numout,*) '   CNRS - NERC - Met OFFICE - MERCATOR-ocean - CMCC'
         WRITE(numout,*) '                       NEMO team'
         WRITE(numout,*) '                   Off-line TOP Model'
         WRITE(numout,*) '                NEMO version 4.0  (2019) '
         WRITE(numout,*)
         WRITE(numout,*) "           ._      ._      ._      ._      ._    "
         WRITE(numout,*) "       _.-._)`\_.-._)`\_.-._)`\_.-._)`\_.-._)`\_ "
         WRITE(numout,*)
         WRITE(numout,*) "           o         _,           _,             "
         WRITE(numout,*) "            o      .' (        .-' /             "
         WRITE(numout,*) "           o     _/..._'.    .'   /              "
         WRITE(numout,*) "      (    o .-'`      ` '-./  _.'               "
         WRITE(numout,*) "       )    ( o)           ;= <_         (       "
         WRITE(numout,*) "      (      '-.,\\__ __.-;`\   '.        )      "
         WRITE(numout,*) "       )  )       \) |`\ \)  '.   \      (   (   "
         WRITE(numout,*) "      (  (           \_/       '-._\      )   )  "
         WRITE(numout,*) "       )  )                        `     (   (   "
         WRITE(numout,*) "     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ "
         WRITE(numout,*)
         !
         WRITE(numout,cform_aaa)                                        ! Flag AAAAAAA
         !
      ENDIF
      !
      IF(lwm) WRITE( numond, namctl )
      !
      !                             !------------------------------------!
      !                             !  Set global domain size parameters !
      !                             !------------------------------------!
      !     
      READ  ( numnam_ref, namcfg, IOSTAT = ios, ERR = 903 )
903   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namcfg in reference namelist' )
      READ  ( numnam_cfg, namcfg, IOSTAT = ios, ERR = 904 )
904   IF( ios >  0 )   CALL ctl_nam ( ios , 'namcfg in configuration namelist' )   
      !
      IF( ln_read_cfg ) THEN              ! Read sizes in domain configuration file
         CALL domain_cfg ( cn_cfg, nn_cfg, Ni0glo, Nj0glo, jpkglo, l_Iperio, l_Jperio, l_NFold, c_NFtype )
      ELSE                                ! user-defined namelist
         CALL usr_def_nam( cn_cfg, nn_cfg, Ni0glo, Nj0glo, jpkglo, l_Iperio, l_Jperio, l_NFold, c_NFtype )
      ENDIF
      !
      IF(lwm)   WRITE( numond, namcfg )
      l_offline = .true.                  ! passive tracers are run offline
      !
      !                             !-----------------------------------------!
      !                             ! mpp parameters and domain decomposition !
      !                             !-----------------------------------------!
      !
      CALL mpp_init

#if defined key_loop_fusion
      IF( nn_hls == 1 ) THEN
         CALL ctl_stop( 'STOP', 'nemogcm : Loop fusion can be used only with extra-halo' )
      ENDIF
#endif

      CALL halo_mng_init()
      ! Now we know the dimensions of the grid and numout has been set: we can allocate arrays
      CALL nemo_alloc()

      ! Initialise time level indices
      Nbb = 1; Nnn = 2; Naa = 3; Nrhs = Naa

      !                             !-------------------------------!
      !                             !  NEMO general initialization  !
      !                             !-------------------------------!

      CALL nemo_ctl                          ! Control prints
      !
      !                                      ! General initialization
      IF( ln_timing    )   CALL timing_init
      IF( ln_timing    )   CALL timing_start( 'nemo_init')
      !
                           CALL     phy_cst         ! Physical constants
                           CALL     eos_init        ! Equation of state
      IF( ln_c1d       )   CALL     c1d_init        ! 1D column configuration
                           CALL     dom_init( Nbb, Nnn, Naa ) ! Domain
      IF( sn_cfctl%l_prtctl )   &
         &                 CALL prt_ctl_init        ! Print control

                           CALL  istate_init( Nnn, Naa )    ! ocean initial state (Dynamics and tracers)

                           CALL     sbc_init( Nbb, Nnn, Naa )    ! Forcings : surface module
                           CALL     bdy_init    ! Open boundaries initialisation
                           
                           CALL zdf_phy_init( Nnn )    ! Vertical physics

      !                                      ! Tracer physics
                           CALL ldf_tra_init    ! Lateral ocean tracer physics
                           CALL ldf_eiv_init    ! Eddy induced velocity param. must be done after ldf_tra_init
                           CALL tra_ldf_init    ! lateral mixing
      IF( l_ldfslp     )   CALL ldf_slp_init    ! slope of lateral mixing
      IF( ln_traqsr    )   CALL tra_qsr_init    ! penetrative solar radiation
      IF( ln_trabbl    )   CALL tra_bbl_init    ! advective (and/or diffusive) bottom boundary layer scheme

      !                                      ! Passive tracers
                           CALL trc_nam_run    ! Needed to get restart parameters for passive tracers
                           CALL trc_rst_cal( nit000, 'READ' )   ! calendar
#if defined key_sed_off
                           CALL dta_dyn_sed_init(  Nnn      )        ! Initialization for the dynamics
#else
                           CALL dta_dyn_init( Nbb, Nnn, Naa )        ! Initialization for the dynamics
#endif
                           CALL     trc_init( Nbb, Nnn, Naa )        ! Passive tracers initialization
                           
      IF(lwp) WRITE(numout,cform_aaa)           ! Flag AAAAAAA
      !
      IF( ln_timing    )   CALL timing_stop( 'nemo_init')
      !
   END SUBROUTINE nemo_init


   SUBROUTINE nemo_ctl
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE nemo_ctl  ***
      !!
      !! ** Purpose :   control print setting
      !!
      !! ** Method  : - print namctl and namcfg information and check some consistencies
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN                  ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'nemo_ctl: Control prints'
         WRITE(numout,*) '~~~~~~~~'
         WRITE(numout,*) '   Namelist namctl'
         WRITE(numout,*) '                              sn_cfctl%l_runstat = ', sn_cfctl%l_runstat
         WRITE(numout,*) '                              sn_cfctl%l_trcstat = ', sn_cfctl%l_trcstat
         WRITE(numout,*) '                              sn_cfctl%l_oceout  = ', sn_cfctl%l_oceout
         WRITE(numout,*) '                              sn_cfctl%l_layout  = ', sn_cfctl%l_layout
         WRITE(numout,*) '                              sn_cfctl%l_prtctl  = ', sn_cfctl%l_prtctl
         WRITE(numout,*) '                              sn_cfctl%l_prttrc  = ', sn_cfctl%l_prttrc
         WRITE(numout,*) '                              sn_cfctl%l_oasout  = ', sn_cfctl%l_oasout
         WRITE(numout,*) '                              sn_cfctl%procmin   = ', sn_cfctl%procmin  
         WRITE(numout,*) '                              sn_cfctl%procmax   = ', sn_cfctl%procmax  
         WRITE(numout,*) '                              sn_cfctl%procincr  = ', sn_cfctl%procincr 
         WRITE(numout,*) '                              sn_cfctl%ptimincr  = ', sn_cfctl%ptimincr 
         WRITE(numout,*) '      timing by routine               ln_timing  = ', ln_timing
         WRITE(numout,*) '      CFL diagnostics                 ln_diacfl  = ', ln_diacfl
      ENDIF

      IF( .NOT.ln_read_cfg )   ln_closea = .false.   ! dealing possible only with a domcfg file
      IF(lwp) THEN                  ! control print
         WRITE(numout,*)
         WRITE(numout,*) '   Namelist namcfg'
         WRITE(numout,*) '      read domain configuration file              ln_read_cfg      = ', ln_read_cfg
         WRITE(numout,*) '         filename to be read                         cn_domcfg     = ', TRIM(cn_domcfg)
         WRITE(numout,*) '         keep closed seas in the domain (if exist)   ln_closea     = ', ln_closea
         WRITE(numout,*) '      create a configuration definition file      ln_write_cfg     = ', ln_write_cfg
         WRITE(numout,*) '         filename to be written                      cn_domcfg_out = ', TRIM(cn_domcfg_out)
         WRITE(numout,*) '      use file attribute if exists as i/p j-start ln_use_jattr     = ', ln_use_jattr
      ENDIF
      !
      IF( 1._wp /= SIGN(1._wp,-0._wp)  )   CALL ctl_stop( 'nemo_ctl: The intrinsec SIGN function follows f2003 standard.',  &
         &                                                'Compile with key_nosignedzero enabled' )
      !
   END SUBROUTINE nemo_ctl


   SUBROUTINE nemo_closefile
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE nemo_closefile  ***
      !!
      !! ** Purpose :   Close the files
      !!----------------------------------------------------------------------
      !
      IF( lk_mpp )   CALL mppsync
      !
      CALL iom_close                                 ! close all input/output files managed by iom_*
      !
      IF( numstp     /= -1 )   CLOSE( numstp     )   ! time-step file
      IF( lwm.AND.numond  /= -1 )   CLOSE( numond          )   ! oce output namelist
      !
      numout = 6                                     ! redefine numout in case it is used after this point...
      !
   END SUBROUTINE nemo_closefile


   SUBROUTINE nemo_alloc
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE nemo_alloc  ***
      !!
      !! ** Purpose :   Allocate all the dynamic arrays of the OCE modules
      !!
      !! ** Method  :
      !!----------------------------------------------------------------------
      USE diawri ,   ONLY : dia_wri_alloc
      USE dom_oce,   ONLY : dom_oce_alloc
      USE zdf_oce,   ONLY : zdf_oce_alloc
      USE trc_oce,   ONLY : trc_oce_alloc
      USE bdy_oce,   ONLY : bdy_oce_alloc
      !
      INTEGER :: ierr
      !!----------------------------------------------------------------------
      !
      ierr =        oce_alloc    ()          ! ocean 
      ierr = ierr + dia_wri_alloc()
      ierr = ierr + dom_oce_alloc()          ! ocean domain
      ierr = ierr + zdf_oce_alloc()          ! ocean vertical physics
      ierr = ierr + trc_oce_alloc()          ! shared TRC / TRA arrays
      ierr = ierr + bdy_oce_alloc()    ! bdy masks (incl. initialization)      
      !
      CALL mpp_sum( 'nemogcm', ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'nemo_alloc: unable to allocate standard ocean arrays' )
      !
   END SUBROUTINE nemo_alloc

   SUBROUTINE nemo_set_cfctl(sn_cfctl, setto )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE nemo_set_cfctl  ***
      !!
      !! ** Purpose :   Set elements of the output control structure to setto.
     !!
      !! ** Method  :   Note this routine can be used to switch on/off some
      !!                types of output for selected areas.
      !!----------------------------------------------------------------------
      TYPE(sn_ctl), INTENT(inout) :: sn_cfctl
      LOGICAL     , INTENT(in   ) :: setto
      !!----------------------------------------------------------------------
      sn_cfctl%l_runstat = setto
      sn_cfctl%l_trcstat = setto
      sn_cfctl%l_oceout  = setto
      sn_cfctl%l_layout  = setto
      sn_cfctl%l_prtctl  = setto
      sn_cfctl%l_prttrc  = setto
      sn_cfctl%l_oasout  = setto
   END SUBROUTINE nemo_set_cfctl

   SUBROUTINE istate_init( Kmm, Kaa )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE istate_init  ***
      !!
      !! ** Purpose :   Initialization to zero of the dynamics and tracers.
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   Kmm, Kaa  ! ocean time level indices
      !
      !     now fields         !     after fields      !
      uu   (:,:,:,Kmm)   = 0._wp   ;   uu(:,:,:,Kaa) = 0._wp   !
      vv   (:,:,:,Kmm)   = 0._wp   ;   vv(:,:,:,Kaa) = 0._wp   !
      ww   (:,:,:)   = 0._wp   !                       !
      hdiv (:,:,:)   = 0._wp   !                       !
      ts  (:,:,:,:,Kmm) = 0._wp   !                       !
      !
      rhd  (:,:,:) = 0.e0
      rhop (:,:,:) = 0.e0
      rn2  (:,:,:) = 0.e0
      !
   END SUBROUTINE istate_init


   SUBROUTINE stp_ctl( kt )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE stp_ctl  ***
      !!
      !! ** Purpose :   Control the run
      !!
      !! ** Method  : - Save the time step in numstp
      !!
      !! ** Actions :   'time.step' file containing the last ocean time-step
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in   ) ::   kt      ! ocean time-step index
      !!----------------------------------------------------------------------
      !
      IF( kt == nit000 .AND. lwm ) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'stp_ctl : time-stepping control'
         WRITE(numout,*) '~~~~~~~'
         ! open time.step file
         CALL ctl_opn( numstp, 'time.step', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, numout, lwp, narea )
      ENDIF
      !
      IF(lwm) WRITE ( numstp, '(1x, i8)' )   kt      !* save the current time step in numstp
      IF(lwm) REWIND( numstp )                       ! --------------------------
      !
   END SUBROUTINE stp_ctl
   !!======================================================================
END MODULE nemogcm
