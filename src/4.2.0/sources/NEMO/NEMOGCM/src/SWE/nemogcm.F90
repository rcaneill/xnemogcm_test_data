MODULE nemogcm
   !!======================================================================
   !!                       ***  MODULE nemogcm   ***
   !! Ocean system   : NEMO GCM (ocean dynamics, on-line tracers, biochemistry and sea-ice)
   !!======================================================================
   !! History :  4.0  !  2020-05  (A. Nasser, G. Madec)  Original code from 4.0.2
   !!             -   !  2020-10  (S. Techene, G. Madec)  cleanning
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   nemo_gcm      : solve ocean dynamics, tracer, biogeochemistry and/or sea-ice
   !!   nemo_init     : initialization of the NEMO system
   !!   nemo_ctl      : initialisation of the contol print
   !!   nemo_closefile: close remaining open files
   !!   nemo_alloc    : dynamical allocation
   !!----------------------------------------------------------------------
   USE step_oce       ! module used in the ocean time stepping module (step.F90)
   !
   USE phycst         ! physical constant                  (par_cst routine)
   USE domain         ! domain initialization   (dom_init & dom_cfg routines)
   USE usrdef_nam     ! user defined configuration
   USE bdyini         ! open boundary cond. setting       (bdy_init routine)
   USE istate         ! initial state setting          (istate_init routine)
   USE wet_dry , ONLY : ln_wd_il, ln_wd_dl, ln_wd_dl_bc   ! switch off wetting and drying
   USE isf_oce , ONLY : ln_isf           ! ice shelf
   USE trd_oce , ONLY : l_trddyn         ! dynamical trend logical
   USE dia25h  , ONLY : ln_dia25h        ! 25h mean output
#if defined key_RK3
   USE stprk3         ! NEMO time-stepping               (stp_RK3   routine)
#else
   USE stpmlf         ! NEMO time-stepping               (stp_MLF   routine)
#endif
   !
   USE lib_mpp        ! distributed memory computing
   USE mppini         ! shared/distributed memory setting (mpp_init routine)
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)
   USE halo_mng       ! halo manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   nemo_gcm    ! called by model.F90
   PUBLIC   nemo_init   ! needed by AGRIF
   PUBLIC   nemo_alloc  ! needed by TAM

   CHARACTER(lc) ::   cform_aaa="( /, 'AAAAAAAA', / ) "     ! flag for output listing

#if ! defined key_mpi_off
   ! need MPI_Wtime
   INCLUDE 'mpif.h'
#endif

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: nemogcm.F90 12614 2020-03-26 14:59:52Z gm $
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
      !!              - launch the time-stepping (stp routine)
      !!              - finalize the run by closing files and communications
      !!
      !! References : Madec, Delecluse, Imbard, and Levy, 1997:  internal report, IPSL.
      !!              Madec, 2008, internal report, IPSL.
      !!----------------------------------------------------------------------
      INTEGER ::   istp   ! time step index
      REAL(wp)::   zstptiming   ! elapsed time for 1 time step
      !!----------------------------------------------------------------------
      !
      !                            !-----------------------!
      CALL nemo_init               !==  Initialisations  ==!
      !                            !-----------------------!
      ! check that all process are still there... If some process have an error,
      ! they will never enter in step and other processes will wait until the end of the cpu time!
      !
      !                                 ! SWE case: only with key_qco
#if ! defined key_qco  
      CALL ctl_stop( 'nemo_gcm (SWE): shallow water model requires key_qco' )
#endif
      !
      CALL mpp_max( 'nemogcm', nstop )

      IF(lwp) WRITE(numout,cform_aaa)   ! Flag AAAAAAA

      !                            !-----------------------!
      !                            !==   time stepping   ==!
      !                            !-----------------------!
      !
      !                                               !== set the model time-step  ==!
      !
      istp = nit000
      !
      !                                               !==  Standard time-stepping  ==!
      !
      DO WHILE( istp <= nitend .AND. nstop == 0 )
         !
         ncom_stp = istp
         IF( ln_timing ) THEN
            zstptiming = MPI_Wtime()
            IF ( istp == ( nit000 + 1 ) ) elapsed_time = zstptiming
            IF ( istp ==         nitend ) elapsed_time = zstptiming - elapsed_time
         ENDIF
         ! 
#if defined key_RK3
         CALL stp_RK3    ( istp )
#else
         CALL stp_MLF     ( istp )
#endif
         istp = istp + 1
         !
         IF( lwp .AND. ln_timing )   WRITE(numtime,*) 'timing step ', istp-1, ' : ', MPI_Wtime() - zstptiming
         !
      END DO
      !
      !
      !                            !------------------------!
      !                            !==  finalize the run  ==!
      !                            !------------------------!
      IF(lwp) WRITE(numout,cform_aaa)        ! Flag AAAAAAA
      !
      IF( nstop /= 0 .AND. lwp ) THEN        ! error print
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
                       CALL xios_finalize  ! end mpp communications with xios
#else
      IF( lk_mpp   )   CALL mppstop      ! end mpp communications
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
      !! ** Purpose :   initialization of the NEMO GCM
      !!----------------------------------------------------------------------
      INTEGER ::   ios, ilocal_comm   ! local integers
      !!
      NAMELIST/namctl/ sn_cfctl, ln_timing, ln_diacfl, nn_isplt, nn_jsplt , nn_ictls,   &
         &                                             nn_ictle, nn_jctls , nn_jctle
      NAMELIST/namcfg/ ln_read_cfg, cn_domcfg, ln_closea, ln_write_cfg, cn_domcfg_out, ln_use_jattr
      !!----------------------------------------------------------------------
      !
      cxios_context = 'nemo'
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
                  CALL ctl_opn(     numnul,           '/dev/null', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, -1, .FALSE. )
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
      IF(lwp) THEN                      ! open listing units
         !
         IF( .NOT. lwm )   &            ! alreay opened for narea == 1
            &            CALL ctl_opn( numout, 'ocean.output', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, -1, .FALSE., narea )
         !
         WRITE(numout,*)
         WRITE(numout,*) '   CNRS - NERC - Met OFFICE - MERCATOR-ocean - CMCC'
         WRITE(numout,*) '                       NEMO team'
         WRITE(numout,*) '            Ocean General Circulation Model'
         WRITE(numout,*) '                NEMO version 4.0  (2020) '
         WRITE(numout,*)
         WRITE(numout,*) '                 Shallow Water Equation'
         WRITE(numout,*) '                 ======================'
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
         WRITE(numout,*) "       )  ) jgs                     `    (   (   "
         WRITE(numout,*) "     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ "
         WRITE(numout,*)
         !
         WRITE(numout,cform_aaa)    ! Flag AAAAAAA
         !
         !                          ! Control print of the working precision
         WRITE(numout,*)
         IF( wp == dp ) THEN   ;   WRITE(numout,*) "par_kind : wp = Working precision = dp = double-precision"
         ELSE                  ;   WRITE(numout,*) "par_kind : wp = Working precision = sp = single-precision"
         ENDIF
                                   WRITE(numout,*) "~~~~~~~~                                 ****************"
                                   WRITE(numout,*)
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
      IF( ln_read_cfg ) THEN            ! Read sizes in domain configuration file
         CALL domain_cfg ( cn_cfg, nn_cfg, Ni0glo, Nj0glo, jpkglo, l_Iperio, l_Jperio, l_NFold, c_NFtype )
      ELSE                              ! user-defined namelist
         CALL usr_def_nam( cn_cfg, nn_cfg, Ni0glo, Nj0glo, jpkglo, l_Iperio, l_Jperio, l_NFold, c_NFtype )
      ENDIF
      !
      IF(lwm)   WRITE( numond, namcfg )
      !
      !                             !-----------------------------------------!
      !                             ! mpp parameters and domain decomposition !
      !                             !-----------------------------------------!
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
      Nbb = 1   ;   Nnn = 2   ;   Naa = 3   ;   Nrhs = Naa
      
      !                             !-------------------------------!
      !                             !  NEMO general initialization  !
      !                             !-------------------------------!

      CALL nemo_ctl                          ! Control prints of namctl and namcfg
      !
      !                                      ! General initialization
      IF( ln_timing    )   CALL timing_init     ! timing
      IF( ln_timing    )   CALL timing_start( 'nemo_init')
      !
                           CALL     phy_cst         ! Physical constants
                           ln_wd_il = .FALSE.   ;   ln_wd_dl = .FALSE.   ;   ln_wd_dl_bc = .FALSE.  ! No wetting and drying
      !
      !                                             ! SWE: Set rho0 and associated variables (eosbn2 not used)
                           rho0        = 1026._wp                 !: volumic mass of reference     [kg/m3]
                           rcp         = 3991.86795711963_wp      !: heat capacity     [J/K]
                           rho0_rcp    = rho0 * rcp 
                           r1_rho0     = 1._wp / rho0
                           r1_rcp      = 1._wp / rcp
                           r1_rho0_rcp = 1._wp / rho0_rcp 
      !
                           CALL     dom_init( Nbb, Nnn, Naa ) ! Domain

      IF( sn_cfctl%l_prtctl )   &
         &                 CALL prt_ctl_init        ! Print control

                           CALL  istate_init( Nbb, Nnn, Naa )    ! ocean initial state (Dynamics and tracers)

      !                                      ! external forcing 
                           CALL     sbc_init( Nbb, Nnn, Naa )    ! surface boundary conditions (including sea-ice)

      !                                      ! Ocean physics                                    
      !                                         ! Vertical physics
                           ln_zdfddm = .FALSE.   ;   ln_zad_Aimp = .FALSE.   ;   ln_zdfosm = .FALSE.   ! used in diawri
                           avt(:,:,:) = 0._wp    ;   avm(:,:,:) = 0._wp                                ! used in diawri
      !                                         ! Lateral physics
                           CALL ldf_dyn_init      ! Lateral ocean momentum physics
                           
      !                                      ! Dynamics
                           CALL dyn_adv_init         ! advection (vector or flux form)
                           CALL dyn_vor_init         ! vorticity term including Coriolis
                           CALL dyn_ldf_init         ! lateral mixing

      !                                      ! Icebergs
                           ln_icebergs = .false.
      !                                      ! ice shelf
                           ln_isf = .false.

      !                                      ! Diagnostics
      IF( ln_diacfl    )   CALL dia_cfl_init    ! Initialise CFL diagnostics

                           l_trddyn  = .FALSE.  ! No trend diagnostics
                           ln_dia25h = .FALSE.  ! No 25h mean diagnostics (zdf_phy not used)- used in diawri

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
      !
      IF( .NOT.ln_read_cfg )   ln_closea = .false.   ! dealing possible only with a domcfg file
      IF(lwp) THEN                  ! control print
         WRITE(numout,*)
         WRITE(numout,*) '   Namelist namcfg'
         WRITE(numout,*) '      read domain configuration file                ln_read_cfg      = ', ln_read_cfg
         WRITE(numout,*) '         filename to be read                           cn_domcfg     = ', TRIM(cn_domcfg)
         WRITE(numout,*) '         keep closed seas in the domain (if exist)     ln_closea     = ', ln_closea
         WRITE(numout,*) '      create a configuration definition file        ln_write_cfg     = ', ln_write_cfg
         WRITE(numout,*) '         filename to be written                        cn_domcfg_out = ', TRIM(cn_domcfg_out)
         WRITE(numout,*) '      use file attribute if exists as i/p j-start   ln_use_jattr     = ', ln_use_jattr
      ENDIF
      !
      IF( 1._wp /= SIGN(1._wp,-0._wp)  )   CALL ctl_stop( 'nemo_ctl: The intrinsec SIGN function follows f2003 standard.',  &
         &                                                'Compile with key_nosignedzero enabled:',   &
         &                                                '--> add -Dkey_nosignedzero to the definition of %CPP in your arch file' )
      !
#if defined key_agrif
      IF( ln_timing )   CALL ctl_stop( 'AGRIF not implemented with ln_timing = true')
#endif
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
      IF( numstp          /= -1 )   CLOSE( numstp          )   ! time-step file
      IF( numrun          /= -1 )   CLOSE( numrun          )   ! run statistics file
      IF( lwm.AND.numond  /= -1 )   CLOSE( numond          )   ! oce output namelist
      IF( lwm.AND.numoni  /= -1 )   CLOSE( numoni          )   ! ice output namelist
      IF( numevo_ice      /= -1 )   CLOSE( numevo_ice      )   ! ice variables (temp. evolution)
      IF( numout          /=  6 )   CLOSE( numout          )   ! standard model output file
      IF( numdct_vol      /= -1 )   CLOSE( numdct_vol      )   ! volume transports
      IF( numdct_heat     /= -1 )   CLOSE( numdct_heat     )   ! heat transports
      IF( numdct_salt     /= -1 )   CLOSE( numdct_salt     )   ! salt transports
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
      USE diawri    , ONLY : dia_wri_alloc
      USE dom_oce   , ONLY : dom_oce_alloc
      !
      INTEGER :: ierr
      !!----------------------------------------------------------------------
      !
      ierr =        oce_SWE_alloc()    ! ocean 
      ierr = ierr + dia_wri_alloc()
      ierr = ierr + dom_oce_alloc()    ! ocean domain
      ierr = ierr + zdf_oce_alloc()    ! ocean vertical physics
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

   !!======================================================================
END MODULE nemogcm
