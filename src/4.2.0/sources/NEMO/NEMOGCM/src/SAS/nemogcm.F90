MODULE nemogcm
   !!======================================================================
   !!                       ***  MODULE nemogcm   ***
   !! StandAlone Surface module : surface fluxes + sea-ice + iceberg floats + ABL
   !!======================================================================
   !! History :  3.6  ! 2011-11  (S. Alderson, G. Madec) original code
   !!             -   ! 2013-06  (I. Epicoco, S. Mocavero, CMCC) nemo_northcomms: setup avoiding MPI communication 
   !!             -   ! 2014-12  (G. Madec) remove KPP scheme and cross-land advection (cla)
   !!            4.0  ! 2016-10  (G. Madec, S. Flavoni)  domain configuration / user defined interface
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   nemo_gcm      : solve ocean dynamics, tracer, biogeochemistry and/or sea-ice
   !!   nemo_init     : initialization of the NEMO system
   !!   nemo_ctl      : initialisation of the contol print
   !!   nemo_closefile: close remaining open files
   !!   nemo_alloc    : dynamical allocation
   !!----------------------------------------------------------------------
   USE step_oce       ! module used in the ocean time stepping module
   USE sbc_oce        ! surface boundary condition: ocean
   USE phycst         ! physical constant                  (par_cst routine)
   USE domain         ! domain initialization   (dom_init & dom_cfg routines)
   USE closea         ! treatment of closed seas (for ln_closea)
   USE usrdef_nam     ! user defined configuration
   USE daymod         ! calendar
   USE restart        ! open  restart file
   USE step           ! NEMO time-stepping                 (stp     routine)
   USE cpl_oasis3     !
   USE sbcssm         !
   USE icbini         ! handle bergs, initialisation
   USE icbstp         ! handle bergs, calving, themodynamics and transport
   USE bdyini         ! open boundary cond. setting       (bdy_init routine). mandatory for sea-ice
   USE bdydta         ! open boundary cond. setting   (bdy_dta_init routine). mandatory for sea-ice
   USE diu_layers     ! diurnal bulk SST and coolskin
   USE step_diu       ! diurnal bulk SST timestepping (called from here if run offline)
   USE icb_oce        ! icebergs
   !
   USE prtctl         ! Print control
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! distributed memory computing
   USE mppini         ! shared/distributed memory setting (mpp_init routine)
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)
#if defined key_xios
   USE xios           ! xIOserver
#endif
#if defined key_agrif && defined key_si3
   USE agrif_ice_update ! ice update
#endif
   USE halo_mng

   IMPLICIT NONE
   PRIVATE

   PUBLIC   nemo_gcm    ! called by model.F90
   PUBLIC   nemo_init   ! needed by AGRIF

   CHARACTER(lc) ::   cform_aaa="( /, 'AAAAAAAA', / ) "     ! flag for output listing

#if ! defined key_mpi_off
   ! need MPI_Wtime
   INCLUDE 'mpif.h'
#endif

   !!----------------------------------------------------------------------
   !! NEMO/SAS 4.0 , NEMO Consortium (2018)
   !! $Id: nemogcm.F90 15267 2021-09-17 09:04:34Z smasson $
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
#if defined key_agrif
      CALL Agrif_Init_Grids()      ! AGRIF: set the meshes
#endif
      !                            !-----------------------!
      CALL nemo_init               !==  Initialisations  ==!
      !                            !-----------------------!
#if defined key_agrif
      Kbb_a = Nbb; Kmm_a = Nnn; Krhs_a = Nrhs   ! agrif_oce module copies of time level indices
      CALL Agrif_Declare_Var       !  "      "   "   "      "  DYN/TRA
# if defined key_top
      CALL Agrif_Declare_Var_top   !  "      "   "   "      "  TOP
# endif
#endif
      ! check that all process are still there... If some process have an error,
      ! they will never enter in step and other processes will wait until the end of the cpu time!
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
#if defined key_agrif
      !                                               !==  AGRIF time-stepping  ==!
      CALL Agrif_Regrid()
      !
#if defined key_si3
      ! Recursive update from highest nested level to lowest:
      Kbb_a = Nbb; Kmm_a = Nnn; Krhs_a = Nrhs   ! agrif_oce module copies of time level indices
      CALL Agrif_step_child_adj(Agrif_update_ice)
#endif
      !
      DO WHILE( istp <= nitend .AND. nstop == 0 )
         CALL stp
         istp = istp + 1
      END DO
      !
# else
      !
      IF( .NOT.ln_diurnal_only ) THEN                 !==  Standard time-stepping  ==!
         !
         DO WHILE( istp <= nitend .AND. nstop == 0 )

            ncom_stp = istp
            IF( ln_timing ) THEN
               zstptiming = MPI_Wtime()
               IF ( istp == ( nit000 + 1 ) ) elapsed_time = zstptiming
               IF ( istp ==         nitend ) elapsed_time = zstptiming - elapsed_time
            ENDIF
            
            CALL stp        ( istp ) 
            istp = istp + 1

            IF( lwp .AND. ln_timing )   WRITE(numtime,*) 'timing step ', istp-1, ' : ', MPI_Wtime() - zstptiming

         END DO
         !
      ELSE                                            !==  diurnal SST time-steeping only  ==!
         !
         DO WHILE( istp <= nitend .AND. nstop == 0 )
            CALL stp_diurnal( istp )   ! time step only the diurnal SST 
            istp = istp + 1
         END DO
         !
      ENDIF
      !
#endif
      !
      IF( ln_icebergs )   CALL icb_end( nitend )

      !                            !------------------------!
      !                            !==  finalize the run  ==!
      !                            !------------------------!
      IF(lwp) WRITE(numout,cform_aaa)        ! Flag AAAAAAA
      !
      IF( nstop /= 0 .AND. lwp ) THEN        ! error print
         WRITE(ctmp1,*) '   ==>>>   nemo_gcm: a total of ', nstop, ' errors have been found'
         IF( ngrdstop > 0 ) THEN
            WRITE(ctmp9,'(i2)') ngrdstop
            WRITE(ctmp2,*) '           E R R O R detected in Agrif grid '//TRIM(ctmp9)
            WRITE(ctmp3,*) '           Look for "E R R O R" messages in all existing '//TRIM(ctmp9)//'_ocean_output* files'
            CALL ctl_stop( ' ', ctmp1, ' ', ctmp2, ' ', ctmp3 )
         ELSE
            WRITE(ctmp2,*) '           Look for "E R R O R" messages in all existing ocean_output* files'
            CALL ctl_stop( ' ', ctmp1, ' ', ctmp2 )
         ENDIF
      ENDIF
      !
      IF( ln_timing )   CALL timing_finalize
      !
      CALL nemo_closefile
      !
#if defined key_xios
                                    CALL xios_finalize  ! end mpp communications with xios
      IF( lk_oasis     )            CALL cpl_finalize   ! end coupling and mpp communications with OASIS
#else
      IF    ( lk_oasis ) THEN   ;   CALL cpl_finalize   ! end coupling and mpp communications with OASIS
      ELSEIF( lk_mpp   ) THEN   ;   CALL mppstop        ! end mpp communications
      ENDIF
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
      NAMELIST/namctl/ sn_cfctl, ln_timing, ln_diacfl,                                &
         &             nn_isplt,  nn_jsplt,  nn_ictls, nn_ictle, nn_jctls, nn_jctle            
      NAMELIST/namcfg/ ln_read_cfg, cn_domcfg, ln_closea, ln_write_cfg, cn_domcfg_out, ln_use_jattr
      !!----------------------------------------------------------------------
      !
      IF( lk_oasis ) THEN   ;   cxios_context = 'sas'    ! when coupling SAS to OCE
      ELSE                  ;   cxios_context = 'nemo'   ! 
      ENDIF
      nn_hls = 1
      !
      l_SAS = .TRUE.   ! used in domain:dom_nam
      !
      !                             !-------------------------------------------------!
      !                             !     set communicator & select the local rank    !
      !                             !  must be done as soon as possible to get narea  !
      !                             !-------------------------------------------------!
      !
#if defined key_xios
      IF( Agrif_Root() ) THEN
         IF( lk_oasis ) THEN
            CALL cpl_init( "sas", ilocal_comm )                                  ! nemo local communicator given by oasis 
            CALL xios_initialize( "not used",local_comm=ilocal_comm )            ! send nemo communicator to xios
         ELSE
            CALL xios_initialize( "for_xios_mpi_id",return_comm=ilocal_comm )    ! nemo local communicator given by xios
         ENDIF
      ENDIF
      CALL mpp_start( ilocal_comm )
#else
      IF( lk_oasis ) THEN
         IF( Agrif_Root() ) THEN
            CALL cpl_init( "sas", ilocal_comm )             ! nemo local communicator given by oasis
         ENDIF
         CALL mpp_start( ilocal_comm )
      ELSE
         CALL mpp_start( )
      ENDIF
#endif
      !
      narea = mpprank + 1                                   ! mpprank: the rank of proc (0 --> mppsize -1 )
      lwm = (narea == 1)                ! control of output namelists
      !
      !                             !---------------------------------------------------------------!
      !                             ! Open output files, reference and configuration namelist files !
      !                             !---------------------------------------------------------------!
      !
      ! open ocean.output as soon as possible to get all output prints (including errors messages)
      IF( lk_oasis ) THEN
         IF( lwm )   CALL ctl_opn(     numout,               'sas.output', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, -1, .FALSE. )
         ! open reference and configuration namelist files
                     CALL load_nml( numnam_ref,        'namelist_sas_ref',                                           -1, lwm )
                     CALL load_nml( numnam_cfg,        'namelist_sas_cfg',                                           -1, lwm )
         IF( lwm )   CALL ctl_opn(      numond, 'output.namelist_sas.dyn', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, -1, .FALSE. )
      ELSE
         IF( lwm )   CALL ctl_opn(      numout,            'ocean.output', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, -1, .FALSE. )
         ! open reference and configuration namelist files
                     CALL load_nml( numnam_ref,            'namelist_ref',                                           -1, lwm )
                     CALL load_nml( numnam_cfg,            'namelist_cfg',                                           -1, lwm )
         IF( lwm )   CALL ctl_opn(      numond,     'output.namelist.dyn', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, -1, .FALSE. )
      ENDIF
      ! open /dev/null file to be able to supress output write easily
      IF( Agrif_Root() ) THEN
                     CALL ctl_opn(     numnul,               '/dev/null', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, -1, .FALSE. )
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
         IF( .NOT. lwm ) THEN           ! alreay opened for narea == 1
            IF(lk_oasis) THEN   ;   CALL ctl_opn( numout,   'sas.output','REPLACE','FORMATTED','SEQUENTIAL',-1,-1, .FALSE., narea )
            ELSE                ;   CALL ctl_opn( numout, 'ocean.output','REPLACE','FORMATTED','SEQUENTIAL',-1,-1, .FALSE., narea )
            ENDIF
         ENDIF
         !
         WRITE(numout,*)
         WRITE(numout,*) '   CNRS - NERC - Met OFFICE - MERCATOR-ocean - CMCC'
         WRITE(numout,*) '                       NEMO team'
         WRITE(numout,*) '            Ocean General Circulation Model'
         WRITE(numout,*) '                NEMO version 4.0  (2019) '
         WRITE(numout,*) '             StandAlone Surface version (SAS) '
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
#if defined key_agrif
      Kbb_a = Nbb; Kmm_a = Nnn; Krhs_a = Nrhs   ! agrif_oce module copies of time level indices
#endif 

      !                             !-------------------------------!
      !                             !  NEMO general initialization  !
      !                             !-------------------------------!

      CALL nemo_ctl                          ! Control prints
      !
      !                                      ! General initialization
      IF( ln_timing    )   CALL timing_init ( 'timing_sas.output' )
      IF( ln_timing    )   CALL timing_start( 'nemo_init')

                           CALL phy_cst         ! Physical constants
                           CALL eos_init        ! Equation of seawater
#if defined key_agrif
     CALL Agrif_Declare_Var_ini   !  "      "   "   "      "  DOM
#endif
                           CALL dom_init( Nbb, Nnn, Naa ) ! Domain
      IF( sn_cfctl%l_prtctl )   &
         &                 CALL prt_ctl_init        ! Print control
      
      IF( ln_rstart )      CALL rst_read_open
                           CALL day_init        ! model calendar (using both namelist and restart infos)

#if defined key_agrif
      uu(:,:,:,:) = 0.0_wp   ;   vv(:,:,:,:) = 0.0_wp   ;   ts(:,:,:,:,:) = 0.0_wp   ! needed for interp done at initialization phase
#endif 
      !                                      ! external forcing 
                           CALL sbc_init( Nbb, Nnn, Naa )  ! Forcings : surface module 

      ! ==> clem: open boundaries init. is mandatory for sea-ice because ice BDY is not decoupled from  
      !           the environment of ocean BDY. Therefore bdy is called in both OCE and SAS modules. 
      !           This is not clean and should be changed in the future. 
                           CALL bdy_init
      ! ==>
                           CALL icb_init( rn_Dt, nit000)   ! initialise icebergs instance

      ! compatibility check
      IF( ln_icebergs .AND. ln_M2016 ) THEN
         IF( lwp ) WRITE(numout,*) '         ==>>>   ln_iceberg and ln_M2016 not compatible with SAS (need 3d data)'
         CALL ctl_stop('ln_iceberg and ln_M2016 not compatible with SAS (need 3d data)')
      END IF
      !
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
      IF( .NOT.ln_read_cfg )   ln_closea = .FALSE.   ! dealing possible only with a domcfg file
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
      USE bdy_oce   , ONLY : ln_bdy, bdy_oce_alloc
      USE oce       ! mandatory for sea-ice because needed for bdy arrays
      !
      INTEGER :: ierr
      !!----------------------------------------------------------------------
      !
      ierr =        dia_wri_alloc()
      ierr = ierr + dom_oce_alloc()          ! ocean domain
      ierr = ierr + oce_alloc    ()          ! (ts...) needed for agrif and/or SI3 and bdy
      ierr = ierr + bdy_oce_alloc()          ! bdy masks (incl. initialization)
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

