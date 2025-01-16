MODULE nemogcm
   !!======================================================================
   !!                       ***  MODULE nemogcm   ***
   !! Ocean system   : NEMO GCM (ocean dynamics, on-line tracers, biochemistry and sea-ice)
   !!======================================================================
   !! History :  3.6  ! 2015-12  (A. Ryan) Original code   (from OCE/) 
   !!            4.0  ! 2016-11  (G. Madec, S. Flavoni)  domain configuration / user defined interface
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   nemo_gcm      : solve ocean dynamics, tracer, biogeochemistry and/or sea-ice
   !!   nemo_init     : initialization of the NEMO system
   !!   nemo_ctl      : initialisation of the contol print
   !!   nemo_closefile: close remaining open files
   !!   nemo_alloc    : dynamical allocation
   !!----------------------------------------------------------------------
   USE step_oce       ! module used in the ocean time stepping module (step.F90)
   USE domain         ! domain initialization   (dom_init & dom_cfg routines)
   USE istate         ! initial state setting          (istate_init routine)
   USE phycst         ! physical constant                  (par_cst routine)
   USE step           ! NEMO time-stepping                 (stp     routine)
   USE cpl_oasis3     ! OASIS3 coupling
   USE diaobs         ! Observation diagnostics       (dia_obs_init routine)
#if defined key_nemocice_decomp
   USE ice_domain_size, only: nx_global, ny_global
#endif
   !           ! Stand Alone Observation operator modules
   USE sao_data
   USE sao_intp
   !
   USE prtctl         ! Print control
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! distributed memory computing
   USE mppini         ! shared/distributed memory setting (mpp_init routine)
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)
#if defined key_xios
   USE xios           ! xIOserver
#endif
   USE halo_mng

   IMPLICIT NONE
   PRIVATE

   PUBLIC   nemo_gcm    ! called by model.F90
   PUBLIC   nemo_init   ! needed by AGRIF
   PUBLIC   nemo_alloc  ! needed by TAM

   CHARACTER(lc) ::   cform_aaa="( /, 'AAAAAAAA', / ) "     ! flag for output listing

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: nemogcm.F90 15267 2021-09-17 09:04:34Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE nemo_gcm
         !!----------------------------------------------------------------------
         !!                    ***  SUBROUTINE offline_obs_oper ***
         !!
         !! ** Purpose : To use NEMO components to interpolate model fields
         !!              to observation space.
         !!
         !! ** Method : 1. Initialise NEMO
         !!             2. Initialise offline obs_oper
         !!             3. Cycle through match ups
         !!             4. Write results to file
         !!----------------------------------------------------------------------
         !
         CALL nemo_init       ! Initialise NEMO
         !
         CALL sao_data_init   ! Initialise Stand Alone Observation operator data
         !
         CALL dia_obs_init    ! Initialise obs_operator
         !
         CALL sao_interp      ! Interpolate to observation space
         !
         CALL dia_obs_wri     ! Pipe to output files
         !
         CALL dia_obs_dealloc ! Reset the obs_oper between
         !
         IF(lk_mpp)   CALL mppstop  ! Safely stop MPI (end mpp communications)
         !
   END SUBROUTINE nemo_gcm


   SUBROUTINE nemo_init
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE nemo_init  ***
      !!
      !! ** Purpose :   initialization of the NEMO GCM
      !!----------------------------------------------------------------------
      INTEGER ::   ios, ilocal_comm   ! local integer
      !
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
      IF( Agrif_Root() ) THEN
         IF( lk_oasis ) THEN
            CALL cpl_init( "oceanx", ilocal_comm )                               ! nemo local communicator given by oasis
            CALL xios_initialize( "not used"       , local_comm =ilocal_comm )   ! send nemo communicator to xios
         ELSE
            CALL xios_initialize( "for_xios_mpi_id", return_comm=ilocal_comm )   ! nemo local communicator given by xios
         ENDIF
      ENDIF
      CALL mpp_start( ilocal_comm )
#else
      IF( lk_oasis ) THEN
         IF( Agrif_Root() ) THEN
            CALL cpl_init( "oceanx", ilocal_comm )          ! nemo local communicator given by oasis
         ENDIF
         CALL mpp_start( ilocal_comm )
      ELSE
         CALL mpp_start( )
      ENDIF
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
      IF(lwp) THEN                      ! open listing units
         !
         IF( .NOT. lwm )   &            ! alreay opened for narea == 1
            &            CALL ctl_opn( numout, 'ocean.output', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, -1, .FALSE., narea )
         !
         WRITE(numout,*)
         WRITE(numout,*) '   CNRS - NERC - Met OFFICE - MERCATOR-ocean - CMCC'
         WRITE(numout,*) '                       NEMO team'
         WRITE(numout,*) '            Stand Alone Observation operator'
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
         WRITE(numout,*) "       )  ) jgs                     `    (   (   "
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

      CALL halo_mng_init()
      ! Now we know the dimensions of the grid and numout has been set: we can allocate arrays
      CALL nemo_alloc()

      !                             !-------------------------------!
      !                             !  NEMO general initialization  !
      !                             !-------------------------------!

      CALL nemo_ctl                          ! Control prints
      !
      !                                         ! General initialization
      IF( ln_timing    )   CALL timing_init     ! timing
      IF( ln_timing    )   CALL timing_start( 'nemo_init')
      !
                           CALL phy_cst            ! Physical constants
                           CALL eos_init           ! Equation of state
                           CALL dom_init( Nbb, Nnn, Naa )    ! Domain


      IF( sn_cfctl%l_prtctl )   &
         &                 CALL prt_ctl_init       ! Print control

                           CALL istate_init        ! ocean initial state (Dynamics and tracers)
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
      USE diawri    , ONLY: dia_wri_alloc
      USE dom_oce   , ONLY: dom_oce_alloc
      !
      INTEGER :: ierr
      !!----------------------------------------------------------------------
      !
      ierr =        oce_alloc       ()          ! ocean
      ierr = ierr + dia_wri_alloc   ()
      ierr = ierr + dom_oce_alloc   ()          ! ocean domain
      !
      CALL mpp_sum( 'nemogcm', ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'nemo_alloc : unable to allocate standard ocean arrays' )
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

