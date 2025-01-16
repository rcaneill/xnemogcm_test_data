MODULE nemogcm
   !!======================================================================
   !!                       ***  MODULE nemogcm   ***
   !! Ocean system   : NEMO GCM (ocean dynamics, on-line tracers, biochemistry and sea-ice)
   !!======================================================================
   !! History :  OPA  ! 1990-10  (C. Levy, G. Madec)  Original code
   !!            7.0  ! 1991-11  (M. Imbard, C. Levy, G. Madec)
   !!            7.1  ! 1993-03  (M. Imbard, C. Levy, G. Madec, O. Marti, M. Guyon, A. Lazar,
   !!                             P. Delecluse, C. Perigaud, G. Caniaux, B. Colot, C. Maes) release 7.1
   !!             -   ! 1992-06  (L.Terray)  coupling implementation
   !!             -   ! 1993-11  (M.A. Filiberti) IGLOO sea-ice
   !!            8.0  ! 1996-03  (M. Imbard, C. Levy, G. Madec, O. Marti, M. Guyon, A. Lazar,
   !!                             P. Delecluse, L.Terray, M.A. Filiberti, J. Vialar, A.M. Treguier, M. Levy) release 8.0
   !!            8.1  ! 1997-06  (M. Imbard, G. Madec)
   !!            8.2  ! 1999-11  (M. Imbard, H. Goosse)  LIM sea-ice model
   !!                 ! 1999-12  (V. Thierry, A-M. Treguier, M. Imbard, M-A. Foujols)  OPEN-MP
   !!                 ! 2000-07  (J-M Molines, M. Imbard)  Open Boundary Conditions  (CLIPPER)
   !!   NEMO     1.0  ! 2002-08  (G. Madec)  F90: Free form and modules
   !!             -   ! 2004-06  (R. Redler, NEC CCRLE, Germany) add OASIS[3/4] coupled interfaces
   !!             -   ! 2004-08  (C. Talandier) New trends organization
   !!             -   ! 2005-06  (C. Ethe) Add the 1D configuration possibility
   !!             -   ! 2005-11  (V. Garnier) Surface pressure gradient organization
   !!             -   ! 2006-03  (L. Debreu, C. Mazauric)  Agrif implementation
   !!             -   ! 2006-04  (G. Madec, R. Benshila)  Step reorganization
   !!             -   ! 2007-07  (J. Chanut, A. Sellar) Unstructured open boundaries (BDY)
   !!            3.2  ! 2009-08  (S. Masson)  open/write in the listing file in mpp
   !!            3.3  ! 2010-05  (K. Mogensen, A. Weaver, M. Martin, D. Lea) Assimilation interface
   !!             -   ! 2010-10  (C. Ethe, G. Madec) reorganisation of initialisation phase
   !!            3.3.1! 2011-01  (A. R. Porter, STFC Daresbury) dynamical allocation
   !!            3.4  ! 2011-11  (C. Harris) decomposition changes for running with CICE
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   nemo_gcm       : solve ocean dynamics, tracer, biogeochemistry and/or sea-ice
   !!   nemo_init      : initialization of the NEMO system
   !!   nemo_ctl       : initialisation of the contol print
   !!   nemo_closefile : close remaining open files
   !!   nemo_alloc     : dynamical allocation
   !!   nemo_partition : calculate MPP domain decomposition
   !!   factorise      : calculate the factors of the no. of MPI processes
   !!----------------------------------------------------------------------
   USE step_oce        ! module used in the ocean time stepping module
   USE domcfg          ! domain configuration               (dom_cfg routine)
   USE mppini          ! shared/distributed memory setting (mpp_init routine)
   USE domain          ! domain initialization             (dom_init routine)
#if defined key_nemocice_decomp
   USE ice_domain_size, only: nx_global, ny_global
#endif
   USE istate          ! initial state setting          (istate_init routine)
   USE phycst          ! physical constant                  (par_cst routine)
   USE diaobs          ! Observation diagnostics       (dia_obs_init routine)
   USE lib_fortran     ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)
   USE step            ! NEMO time-stepping                 (stp     routine)
   USE icbini          ! handle bergs, initialisation
   USE icbstp          ! handle bergs, calving, themodynamics and transport
   USE cpl_oasis3      ! OASIS3 coupling
   USE lib_mpp         ! distributed memory computing
#if defined key_iomput
   USE xios
#endif
   USE ooo_data        ! Offline obs_oper data
   USE ooo_read        ! Offline obs_oper read routines
   USE ooo_intp        ! Offline obs_oper interpolation

   IMPLICIT NONE
   PRIVATE

   PUBLIC   nemo_gcm    ! called by nemo.f90
   PUBLIC   nemo_init   ! needed by AGRIF
   PUBLIC   nemo_alloc  ! needed by TAM

   CHARACTER(lc) ::   cform_aaa="( /, 'AAAAAAAA', / ) "     ! flag for output listing

   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id$
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
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
         !!
         !!----------------------------------------------------------------------
         !! Class 4 output stream switch
         USE obs_fbm, ONLY: ln_cl4
         !! Initialise NEMO
         CALL nemo_init
         !! Initialise Offline obs_oper data
         CALL ooo_data_init( ln_cl4 )
         !! Loop over various model counterparts
         DO jimatch = 1, cl4_match_len
            IF (jimatch .GT. 1) THEN
               !! Initialise obs_oper
               CALL dia_obs_init
            END IF
            !! Interpolate to observation space
            CALL ooo_interp
            !! Pipe to output files
            CALL dia_obs_wri
            !! Reset the obs_oper between
            CALL dia_obs_dealloc
         END DO
         !! Safely stop MPI
         IF(lk_mpp) CALL mppstop  ! end mpp communications
   END SUBROUTINE nemo_gcm

   SUBROUTINE nemo_init
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE nemo_init  ***
      !!
      !! ** Purpose :   initialization of the NEMO GCM
      !!----------------------------------------------------------------------
      INTEGER ::   ji            ! dummy loop indices
      INTEGER ::   ilocal_comm   ! local integer
      CHARACTER(len=80), DIMENSION(16) ::   cltxt
      !!
      NAMELIST/namctl/ ln_ctl, nn_print, nn_ictls, nn_ictle,   &
         &             nn_isplt, nn_jsplt, nn_jctls, nn_jctle,   &
         &             nn_bench, nn_timing
      NAMELIST/namcfg/ cp_cfg, cp_cfz, jp_cfg, jpidta, jpjdta, jpkdta, jpiglo, jpjglo, &
         &             jpizoom, jpjzoom, jperio, ln_use_jattr
      !!----------------------------------------------------------------------
      !
      cltxt = ''
      cxios_context = 'nemo'
      !
      !                             ! Open reference namelist and configuration namelist files
      CALL ctl_opn( numnam_ref, 'namelist_ref', 'OLD', 'FORMATTED', 'SEQUENTIAL', -1, 6, .FALSE. )
      CALL ctl_opn( numnam_cfg, 'namelist_cfg', 'OLD', 'FORMATTED', 'SEQUENTIAL', -1, 6, .FALSE. )
      !
      REWIND( numnam_ref )              ! Namelist namctl in reference namelist : Control prints & Benchmark
      READ  ( numnam_ref, namctl, IOSTAT = ios, ERR = 901 )
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namctl in reference namelist', .TRUE. )

      REWIND( numnam_cfg )              ! Namelist namctl in confguration namelist : Control prints & Benchmark
      READ  ( numnam_cfg, namctl, IOSTAT = ios, ERR = 902 )
902   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namctl in configuration namelist', .TRUE. )

      !
      REWIND( numnam_ref )              ! Namelist namcfg in reference namelist : Control prints & Benchmark
      READ  ( numnam_ref, namcfg, IOSTAT = ios, ERR = 903 )
903   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namcfg in reference namelist', .TRUE. )

      REWIND( numnam_cfg )              ! Namelist namcfg in confguration namelist : Control prints & Benchmark
      READ  ( numnam_cfg, namcfg, IOSTAT = ios, ERR = 904 )
904   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namcfg in configuration namelist', .TRUE. )   

      !                             !--------------------------------------------!
      !                             !  set communicator & select the local node  !
      !                             !  NB: mynode also opens output.namelist.dyn !
      !                             !      on unit number numond on first proc   !
      !                             !--------------------------------------------!
#if defined key_iomput
      IF( Agrif_Root() ) THEN
         IF( lk_oasis ) THEN
            CALL cpl_init( ilocal_comm )                               ! nemo local communicator given by oasis
            CALL xios_initialize( "oceanx",local_comm=ilocal_comm )    ! send nemo communicator to xios
         ELSE
            CALL  xios_initialize( "for_xios_mpi_id",return_comm=ilocal_comm )    ! nemo local communicator given by xios
         ENDIF
      ENDIF
      ENDIF
      narea = mynode( cltxt, 'output.namelist.dyn', numnam_ref, numnam_cfg, numond , nstop, ilocal_comm )   ! Nodes selection
#else
      IF( lk_oasis ) THEN
         IF( Agrif_Root() ) THEN
            CALL cpl_init( ilocal_comm )                               ! nemo local communicator given by oasis
         ENDIF
         narea = mynode( cltxt, 'output.namelist.dyn', numnam_ref, numnam_cfg, numond , nstop, ilocal_comm )   ! Nodes selection (control print return in cltxt)
      ELSE
         ilocal_comm = 0
         narea = mynode( cltxt, 'output.namelist.dyn', numnam_ref, numnam_cfg, numond , nstop )                ! Nodes selection (control print return in cltxt)
      ENDIF
#endif
      narea = narea + 1                                     ! mynode return the rank of proc (0 --> jpnij -1 )

      lwm = (narea == 1)                                    ! control of output namelists
      lwp = (narea == 1) .OR. ln_ctl                        ! control of all listing output print

      IF(lwm) THEN
         ! write merged namelists from earlier to output namelist now that the
         ! file has been opened in call to mynode. nammpp has already been
         ! written in mynode (if lk_mpp_mpi)
         WRITE( numond, namctl )
         WRITE( numond, namcfg )
      ENDIF

      ! If dimensions of processor grid weren't specified in the namelist file
      ! then we calculate them here now that we have our communicator size
      IF( (jpni < 1) .OR. (jpnj < 1) )THEN
#if   defined key_mpp_mpi
         IF( Agrif_Root() ) CALL nemo_partition(mppsize)
#else
         jpni  = 1
         jpnj  = 1
         jpnij = jpni*jpnj
#endif
      END IF

      ! Calculate domain dimensions given calculated jpni and jpnj
      ! This used to be done in par_oce.F90 when they were parameters rather
      ! than variables
      IF( Agrif_Root() ) THEN
#if defined key_nemocice_decomp
         jpi = ( nx_global+2-2*jpreci + (jpni-1) ) / jpni + 2*jpreci ! first  dim.
         jpj = ( ny_global+2-2*jprecj + (jpnj-1) ) / jpnj + 2*jprecj ! second dim. 
#else
         jpi = ( jpiglo-2*jpreci + (jpni-1) ) / jpni + 2*jpreci   ! first  dim.
         jpj = ( jpjglo-2*jprecj + (jpnj-1) ) / jpnj + 2*jprecj   ! second dim.
#endif
         jpk = jpkdta                                             ! third dim
         jpim1 = jpi-1                                            ! inner domain indices
         jpjm1 = jpj-1                                            !   "           "
         jpkm1 = jpk-1                                            !   "           "
         jpij  = jpi*jpj                                          !  jpi x j
      ENDIF

      IF(lwp) THEN                            ! open listing units
         !
         CALL ctl_opn( numout, 'ocean.output', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, 6, .FALSE., narea )
         !
         WRITE(numout,*)
         WRITE(numout,*) '   CNRS - NERC - Met OFFICE - MERCATOR-ocean - INGV - CMCC'
         WRITE(numout,*) '                       NEMO team'
         WRITE(numout,*) '            Ocean General Circulation Model'
         WRITE(numout,*) '                  version 3.6  (2015) '
         WRITE(numout,*)
         WRITE(numout,*)
         DO ji = 1, SIZE(cltxt)
            IF( TRIM(cltxt(ji)) /= '' )   WRITE(numout,*) cltxt(ji)      ! control print of mynode
         END DO
         WRITE(numout,cform_aaa)                                         ! Flag AAAAAAA
         !
      ENDIF

      ! Now we know the dimensions of the grid and numout has been set we can
      ! allocate arrays
      CALL nemo_alloc()

      !                             !-------------------------------!
      !                             !  NEMO general initialization  !
      !                             !-------------------------------!

      CALL nemo_ctl                          ! Control prints & Benchmark

      !                                      ! Domain decomposition
      IF( jpni*jpnj == jpnij ) THEN   ;   CALL mpp_init      ! standard cutting out
      ELSE                            ;   CALL mpp_init2     ! eliminate land processors
      ENDIF
      !
      IF( nn_timing == 1 )  CALL timing_init
      !
      !                                      ! General initialization
                            CALL     phy_cst    ! Physical constants
                            CALL     eos_init   ! Equation of state
                            CALL     dom_cfg    ! Domain configuration
                            CALL     dom_init   ! Domain

      IF( ln_nnogather )    CALL nemo_northcomms   ! Initialise the northfold neighbour lists (must be done after the masks are defined)

      IF( ln_ctl        )   CALL prt_ctl_init   ! Print control

                            CALL  istate_init   ! ocean initial state (Dynamics and tracers)

      IF( lk_diaobs     ) THEN                  ! Observation & model comparison
                            CALL dia_obs_init            ! Initialize observational data
                            CALL dia_obs( nit000 - 1 )   ! Observation operator for restart
      ENDIF
   END SUBROUTINE nemo_init


   SUBROUTINE nemo_ctl
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE nemo_ctl  ***
      !!
      !! ** Purpose :   control print setting
      !!
      !! ** Method  : - print namctl information and check some consistencies
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN                  ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'nemo_ctl: Control prints & Benchmark'
         WRITE(numout,*) '~~~~~~~ '
         WRITE(numout,*) '   Namelist namctl'
         WRITE(numout,*) '      run control (for debugging)     ln_ctl     = ', ln_ctl
         WRITE(numout,*) '      level of print                  nn_print   = ', nn_print
         WRITE(numout,*) '      Start i indice for SUM control  nn_ictls   = ', nn_ictls
         WRITE(numout,*) '      End i indice for SUM control    nn_ictle   = ', nn_ictle
         WRITE(numout,*) '      Start j indice for SUM control  nn_jctls   = ', nn_jctls
         WRITE(numout,*) '      End j indice for SUM control    nn_jctle   = ', nn_jctle
         WRITE(numout,*) '      number of proc. following i     nn_isplt   = ', nn_isplt
         WRITE(numout,*) '      number of proc. following j     nn_jsplt   = ', nn_jsplt
         WRITE(numout,*) '      benchmark parameter (0/1)       nn_bench   = ', nn_bench
         WRITE(numout,*) '      timing activated    (0/1)       nn_timing  = ', nn_timing
      ENDIF
      !
      nprint    = nn_print          ! convert DOCTOR namelist names into OLD names
      nictls    = nn_ictls
      nictle    = nn_ictle
      njctls    = nn_jctls
      njctle    = nn_jctle
      isplt     = nn_isplt
      jsplt     = nn_jsplt
      nbench    = nn_bench
      !                             ! Parameter control
      !
      IF( ln_ctl ) THEN                 ! sub-domain area indices for the control prints
         IF( lk_mpp .AND. jpnij > 1 ) THEN
            isplt = jpni   ;   jsplt = jpnj   ;   ijsplt = jpni*jpnj   ! the domain is forced to the real split domain
         ELSE
            IF( isplt == 1 .AND. jsplt == 1  ) THEN
               CALL ctl_warn( ' - isplt & jsplt are equal to 1',   &
                  &           ' - the print control will be done over the whole domain' )
            ENDIF
            ijsplt = isplt * jsplt            ! total number of processors ijsplt
         ENDIF
         IF(lwp) WRITE(numout,*)'          - The total number of processors over which the'
         IF(lwp) WRITE(numout,*)'            print control will be done is ijsplt : ', ijsplt
         !
         !                              ! indices used for the SUM control
         IF( nictls+nictle+njctls+njctle == 0 )   THEN    ! print control done over the default area
            lsp_area = .FALSE.
         ELSE                                             ! print control done over a specific  area
            lsp_area = .TRUE.
            IF( nictls < 1 .OR. nictls > jpiglo )   THEN
               CALL ctl_warn( '          - nictls must be 1<=nictls>=jpiglo, it is forced to 1' )
               nictls = 1
            ENDIF
            IF( nictle < 1 .OR. nictle > jpiglo )   THEN
               CALL ctl_warn( '          - nictle must be 1<=nictle>=jpiglo, it is forced to jpiglo' )
               nictle = jpiglo
            ENDIF
            IF( njctls < 1 .OR. njctls > jpjglo )   THEN
               CALL ctl_warn( '          - njctls must be 1<=njctls>=jpjglo, it is forced to 1' )
               njctls = 1
            ENDIF
            IF( njctle < 1 .OR. njctle > jpjglo )   THEN
               CALL ctl_warn( '          - njctle must be 1<=njctle>=jpjglo, it is forced to jpjglo' )
               njctle = jpjglo
            ENDIF
         ENDIF
      ENDIF
      !
      IF( nbench == 1 ) THEN              ! Benchmark
         SELECT CASE ( cp_cfg )
         CASE ( 'gyre' )   ;   CALL ctl_warn( ' The Benchmark is activated ' )
         CASE DEFAULT      ;   CALL ctl_stop( ' The Benchmark is based on the GYRE configuration:',   &
            &                                 ' key_gyre must be used or set nbench = 0' )
         END SELECT
      ENDIF
      !
      IF( lk_c1d .AND. .NOT.lk_iomput )   CALL ctl_stop( 'nemo_ctl: The 1D configuration must be used ',   &
         &                                               'with the IOM Input/Output manager. '         ,   &
         &                                               'Compile with key_iomput enabled' )
      !
      IF( 1_wp /= SIGN(1._wp,-0._wp)  )   CALL ctl_stop( 'nemo_ctl: The intrinsec SIGN function follows ',  &
         &                                               'f2003 standard. '                              ,  &
         &                                               'Compile with key_nosignedzero enabled' )
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
      IF( numstp      /= -1 )   CLOSE( numstp      )   ! time-step file
      IF( numsol      /= -1 )   CLOSE( numsol      )   ! solver file
      IF( numnam      /= -1 )   CLOSE( numnam      )   ! oce namelist
      IF( numnam_ice  /= -1 )   CLOSE( numnam_ice  )   ! ice namelist
      IF( numevo_ice  /= -1 )   CLOSE( numevo_ice  )   ! ice variables (temp. evolution)
      IF( numout      /=  6 )   CLOSE( numout      )   ! standard model output file
      IF( numdct_vol  /= -1 )   CLOSE( numdct_vol  )   ! volume transports
      IF( numdct_heat /= -1 )   CLOSE( numdct_heat )   ! heat transports
      IF( numdct_salt /= -1 )   CLOSE( numdct_salt )   ! salt transports

      !
      numout = 6                                     ! redefine numout in case it is used after this point...
      !
   END SUBROUTINE nemo_closefile


   SUBROUTINE nemo_alloc
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE nemo_alloc  ***
      !!
      !! ** Purpose :   Allocate all the dynamic arrays of the OPA modules
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
      ierr = ierr + lib_mpp_alloc   (numout)    ! mpp exchanges
      !
      IF( lk_mpp    )   CALL mpp_sum( ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'nemo_alloc : unable to allocate standard ocean arrays' )
      !
   END SUBROUTINE nemo_alloc


   SUBROUTINE nemo_partition( num_pes )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE nemo_partition  ***
      !!
      !! ** Purpose :
      !!
      !! ** Method  :
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) :: num_pes ! The number of MPI processes we have
      !
      INTEGER, PARAMETER :: nfactmax = 20
      INTEGER :: nfact ! The no. of factors returned
      INTEGER :: ierr  ! Error flag
      INTEGER :: ji
      INTEGER :: idiff, mindiff, imin ! For choosing pair of factors that are closest in value
      INTEGER, DIMENSION(nfactmax) :: ifact ! Array of factors
      !!----------------------------------------------------------------------

      ierr = 0

      CALL factorise( ifact, nfactmax, nfact, num_pes, ierr )

      IF( nfact <= 1 ) THEN
         WRITE (numout, *) 'WARNING: factorisation of number of PEs failed'
         WRITE (numout, *) '       : using grid of ',num_pes,' x 1'
         jpnj = 1
         jpni = num_pes
      ELSE
         ! Search through factors for the pair that are closest in value
         mindiff = 1000000
         imin    = 1
         DO ji = 1, nfact-1, 2
            idiff = ABS( ifact(ji) - ifact(ji+1) )
            IF( idiff < mindiff ) THEN
               mindiff = idiff
               imin = ji
            ENDIF
         END DO
         jpnj = ifact(imin)
         jpni = ifact(imin + 1)
      ENDIF
      !
      jpnij = jpni*jpnj
      !
   END SUBROUTINE nemo_partition


   SUBROUTINE factorise( kfax, kmaxfax, knfax, kn, kerr )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE factorise  ***
      !!
      !! ** Purpose :   return the prime factors of n.
      !!                knfax factors are returned in array kfax which is of
      !!                maximum dimension kmaxfax.
      !! ** Method  :
      !!----------------------------------------------------------------------
      INTEGER                    , INTENT(in   ) ::   kn, kmaxfax
      INTEGER                    , INTENT(  out) ::   kerr, knfax
      INTEGER, DIMENSION(kmaxfax), INTENT(  out) ::   kfax
      !
      INTEGER :: ifac, jl, inu
      INTEGER, PARAMETER :: ntest = 14
      INTEGER :: ilfax(ntest)

      ! lfax contains the set of allowed factors.
      data (ilfax(jl),jl=1,ntest) / 16384, 8192, 4096, 2048, 1024, 512, 256,  &
         &                            128,   64,   32,   16,    8,   4,   2  /
      !!----------------------------------------------------------------------

      ! Clear the error flag and initialise output vars
      kerr = 0
      kfax = 1
      knfax = 0

      ! Find the factors of n.
      IF( kn == 1 )   GOTO 20

      ! nu holds the unfactorised part of the number.
      ! knfax holds the number of factors found.
      ! l points to the allowed factor list.
      ! ifac holds the current factor.

      inu   = kn
      knfax = 0

      DO jl = ntest, 1, -1
         !
         ifac = ilfax(jl)
         IF( ifac > inu )   CYCLE

         ! Test whether the factor will divide.

         IF( MOD(inu,ifac) == 0 ) THEN
            !
            knfax = knfax + 1            ! Add the factor to the list
            IF( knfax > kmaxfax ) THEN
               kerr = 6
               write (*,*) 'FACTOR: insufficient space in factor array ', knfax
               return
            ENDIF
            kfax(knfax) = ifac
            ! Store the other factor that goes with this one
            knfax = knfax + 1
            kfax(knfax) = inu / ifac
            !WRITE (*,*) 'ARPDBG, factors ',knfax-1,' & ',knfax,' are ', kfax(knfax-1),' and ',kfax(knfax)
         ENDIF
         !
      END DO

   20 CONTINUE      ! Label 20 is the exit point from the factor search loop.
      !
   END SUBROUTINE factorise

#if defined key_mpp_mpi
   SUBROUTINE nemo_northcomms
      !!======================================================================
      !!                     ***  ROUTINE  nemo_northcomms  ***
      !! nemo_northcomms    :  Setup for north fold exchanges with explicit peer to peer messaging
      !!=====================================================================
      !!----------------------------------------------------------------------
      !!
      !! ** Purpose :   Initialization of the northern neighbours lists.
      !!----------------------------------------------------------------------
      !!    1.0  ! 2011-10  (A. C. Coward, NOCS & J. Donners, PRACE)
      !!----------------------------------------------------------------------

      INTEGER ::   ji, jj, jk, ij, jtyp    ! dummy loop indices
      INTEGER ::   ijpj                    ! number of rows involved in north-fold exchange
      INTEGER ::   northcomms_alloc        ! allocate return status
      REAL(wp), ALLOCATABLE, DIMENSION ( :,: ) ::   znnbrs     ! workspace
      LOGICAL,  ALLOCATABLE, DIMENSION ( : )   ::   lrankset   ! workspace

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'nemo_northcomms : Initialization of the northern neighbours lists'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~'

      !!----------------------------------------------------------------------
      ALLOCATE( znnbrs(jpi,jpj), stat = northcomms_alloc )
      ALLOCATE( lrankset(jpnij), stat = northcomms_alloc )
      IF( northcomms_alloc /= 0 ) THEN
         WRITE(numout,cform_war)
         WRITE(numout,*) 'northcomms_alloc : failed to allocate arrays'
         CALL ctl_stop( 'STOP', 'nemo_northcomms : unable to allocate temporary arrays' )
      ENDIF
      nsndto = 0
      isendto = -1
      ijpj   = 4
      !
      ! This routine has been called because ln_nnogather has been set true ( nammpp )
      ! However, these first few exchanges have to use the mpi_allgather method to
      ! establish the neighbour lists to use in subsequent peer to peer exchanges.
      ! Consequently, set l_north_nogather to be false here and set it true only after
      ! the lists have been established.
      !
      l_north_nogather = .FALSE.
      !
      ! Exchange and store ranks on northern rows

      DO jtyp = 1,4

         lrankset = .FALSE.
         znnbrs = narea
         SELECT CASE (jtyp)
            CASE(1)
               CALL lbc_lnk( znnbrs, 'T', 1. )      ! Type 1: T,W-points
            CASE(2)
               CALL lbc_lnk( znnbrs, 'U', 1. )      ! Type 2: U-point
            CASE(3)
               CALL lbc_lnk( znnbrs, 'V', 1. )      ! Type 3: V-point
            CASE(4)
               CALL lbc_lnk( znnbrs, 'F', 1. )      ! Type 4: F-point
         END SELECT

         IF ( njmppt(narea) .EQ. MAXVAL( njmppt ) ) THEN
            DO jj = nlcj-ijpj+1, nlcj
               ij = jj - nlcj + ijpj
               DO ji = 1,jpi
                  IF ( INT(znnbrs(ji,jj)) .NE. 0 .AND. INT(znnbrs(ji,jj)) .NE. narea ) &
               &     lrankset(INT(znnbrs(ji,jj))) = .true.
               END DO
            END DO

            DO jj = 1,jpnij
               IF ( lrankset(jj) ) THEN
                  nsndto(jtyp) = nsndto(jtyp) + 1
                  IF ( nsndto(jtyp) .GT. jpmaxngh ) THEN
                     CALL ctl_stop( ' Too many neighbours in nemo_northcomms ', &
                  &                 ' jpmaxngh will need to be increased ')
                  ENDIF
                  isendto(nsndto(jtyp),jtyp) = jj-1   ! narea converted to MPI rank
               ENDIF
            END DO
         ENDIF

      END DO

      !
      ! Type 5: I-point
      !
      ! ICE point exchanges may involve some averaging. The neighbours list is
      ! built up using two exchanges to ensure that the whole stencil is covered.
      ! lrankset should not be reset between these 'J' and 'K' point exchanges

      jtyp = 5
      lrankset = .FALSE.
      znnbrs = narea
      CALL lbc_lnk( znnbrs, 'J', 1. ) ! first ice U-V point

      IF ( njmppt(narea) .EQ. MAXVAL( njmppt ) ) THEN
         DO jj = nlcj-ijpj+1, nlcj
            ij = jj - nlcj + ijpj
            DO ji = 1,jpi
               IF ( INT(znnbrs(ji,jj)) .NE. 0 .AND. INT(znnbrs(ji,jj)) .NE. narea ) &
            &     lrankset(INT(znnbrs(ji,jj))) = .true.
         END DO
        END DO
      ENDIF

      znnbrs = narea
      CALL lbc_lnk( znnbrs, 'K', 1. ) ! second ice U-V point

      IF ( njmppt(narea) .EQ. MAXVAL( njmppt )) THEN
         DO jj = nlcj-ijpj+1, nlcj
            ij = jj - nlcj + ijpj
            DO ji = 1,jpi
               IF ( INT(znnbrs(ji,jj)) .NE. 0 .AND.  INT(znnbrs(ji,jj)) .NE. narea ) &
            &       lrankset( INT(znnbrs(ji,jj))) = .true.
            END DO
         END DO

         DO jj = 1,jpnij
            IF ( lrankset(jj) ) THEN
               nsndto(jtyp) = nsndto(jtyp) + 1
               IF ( nsndto(jtyp) .GT. jpmaxngh ) THEN
                  CALL ctl_stop( ' Too many neighbours in nemo_northcomms ', &
               &                 ' jpmaxngh will need to be increased ')
               ENDIF
               isendto(nsndto(jtyp),jtyp) = jj-1   ! narea converted to MPI rank
            ENDIF
         END DO
         !
         ! For northern row areas, set l_north_nogather so that all subsequent exchanges
         ! can use peer to peer communications at the north fold
         !
         l_north_nogather = .TRUE.
         !
      ENDIF
      DEALLOCATE( znnbrs )
      DEALLOCATE( lrankset )

   END SUBROUTINE nemo_northcomms
#else
   SUBROUTINE nemo_northcomms      ! Dummy routine
      WRITE(*,*) 'nemo_northcomms: You should not have seen this print! error?'
   END SUBROUTINE nemo_northcomms
#endif
   !!======================================================================
END MODULE nemogcm
