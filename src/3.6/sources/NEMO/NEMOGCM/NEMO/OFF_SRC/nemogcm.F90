MODULE nemogcm
   !!======================================================================
   !!                       ***  MODULE nemogcm   ***
   !! Off-line Ocean   : passive tracer evolution, dynamics read in files
   !!======================================================================
   !! History :  3.3  ! 2010-05  (C. Ethe)  Full reorganization of the off-line: phasing with the on-line
   !!            4.0  ! 2011-01  (C. Ethe, A. R. Porter, STFC Daresbury) dynamical allocation
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   nemo_gcm        : off-line: solve ocean tracer only
   !!   nemo_init       : initialization of the nemo model
   !!   nemo_ctl        : initialisation of algorithm flag 
   !!   nemo_closefile  : close remaining files
   !!----------------------------------------------------------------------
   USE dom_oce         ! ocean space domain variables
   USE oce             ! dynamics and tracers variables
   USE c1d             ! 1D configuration
   USE domcfg          ! domain configuration               (dom_cfg routine)
   USE domain          ! domain initialization from coordinate & bathymetry (dom_init routine)
   USE domrea          ! domain initialization from mesh_mask            (dom_init routine)
   USE eosbn2          ! equation of state            (eos bn2 routine)
   !              ! ocean physics
   USE ldftra          ! lateral diffusivity setting    (ldf_tra_init routine)
   USE ldfslp          ! slopes of neutral surfaces     (ldf_slp_init routine)
   USE traqsr          ! solar radiation penetration    (tra_qsr_init routine)
   USE trabbl          ! bottom boundary layer          (tra_bbl_init routine)
   USE zdfini          ! vertical physics: initialization
   USE sbcmod          ! surface boundary condition       (sbc_init     routine)
   USE phycst          ! physical constant                  (par_cst routine)
   USE dtadyn          ! Lecture and Interpolation of the dynamical fields
   USE trcini          ! Initilization of the passive tracers
   USE daymod          ! calendar                         (day     routine)
   USE trcstp          ! passive tracer time-stepping      (trc_stp routine)
   USE dtadyn          ! Lecture and interpolation of the dynamical fields
   !                   ! I/O & MPP
   USE iom             ! I/O library
   USE in_out_manager  ! I/O manager
   USE mppini          ! shared/distributed memory setting (mpp_init routine)
   USE lib_mpp         ! distributed memory computing
#if defined key_iomput
   USE xios
#endif 
   USE prtctl          ! Print control                    (prt_ctl_init routine)
   USE timing          ! Timing
   USE lib_fortran     ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)
   USE lbcnfd, ONLY: isendto, nsndto, nfsloop, nfeloop ! Setup of north fold exchanges

   USE trc
   USE trcnam
   USE trcrst

   IMPLICIT NONE
   PRIVATE
   
   PUBLIC   nemo_gcm   ! called by nemo.F90

   CHARACTER (len=64) ::   cform_aaa="( /, 'AAAAAAAA', / ) "   ! flag for output listing

   !!----------------------------------------------------------------------
   !! NEMO/OFF 3.3 , NEMO Consortium (2010)
   !! $Id: nemogcm.F90 7522 2017-01-02 10:06:49Z cetlod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE nemo_gcm
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE nemo_gcm  ***
      !!
      !! ** Purpose :   nemo solves the primitive equations on an orthogonal
      !!      curvilinear mesh on the sphere.
      !!
      !! ** Method  : - model general initialization
      !!              - launch the time-stepping (dta_dyn and trc_stp)
      !!              - finalize the run by closing files and communications
      !!
      !! References : Madec, Delecluse,Imbard, and Levy, 1997:  internal report, IPSL.
      !!              Madec, 2008, internal report, IPSL.
      !!----------------------------------------------------------------------
      INTEGER :: istp, indic       ! time step index
      !!----------------------------------------------------------------------

      CALL nemo_init  ! Initializations

      ! check that all process are still there... If some process have an error,
      ! they will never enter in step and other processes will wait until the end of the cpu time!
      IF( lk_mpp )   CALL mpp_max( nstop )

      !                            !-----------------------!
      !                            !==   time stepping   ==!
      !                            !-----------------------!
      istp = nit000
      ! 
      DO WHILE ( istp <= nitend .AND. nstop == 0 )    ! time stepping
         !
         IF( istp == nit000 )  CALL iom_init( cxios_context )            ! iom_put initialization
         IF( istp /= nit000 )   CALL day        ( istp )         ! Calendar (day was already called at nit000 in day_init)
                                CALL iom_setkt  ( istp - nit000 + 1, cxios_context )   ! say to iom that we are at time step kstp
                                CALL trc_rst_opn( istp )         ! Open tracer                                !   restart file
                                CALL dta_dyn    ( istp )         ! Interpolation of the dynamical fields
                                CALL trc_stp    ( istp )         ! time-stepping
         IF( lk_vvl )           CALL dta_dyn_swp( istp )         ! swap of sea surface height and vertical scale factors
                                CALL stp_ctl    ( istp, indic )  ! Time loop: control and print
         istp = istp + 1
         IF( lk_mpp )   CALL mpp_max( nstop )
      END DO
#if defined key_iomput
      CALL iom_context_finalize( cxios_context ) ! needed for XIOS+AGRIF
#endif

      !                            !------------------------!
      !                            !==  finalize the run  ==!
      !                            !------------------------!
      IF(lwp) WRITE(numout,cform_aaa)                 ! Flag AAAAAAA

      IF( nstop /= 0 .AND. lwp ) THEN                 ! error print
         WRITE(numout,cform_err)
         WRITE(numout,*) nstop, ' error have been found'
      ENDIF
      !
      IF( nn_timing == 1 )   CALL timing_finalize
      !
      CALL nemo_closefile
      !
# if defined key_iomput
      CALL xios_finalize             ! end mpp communications
# else
      IF( lk_mpp )   CALL mppstop       ! end mpp communications
# endif
      !
   END SUBROUTINE nemo_gcm


   SUBROUTINE nemo_init
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE nemo_init ***
      !!
      !! ** Purpose :   initialization of the nemo model in off-line mode
      !!----------------------------------------------------------------------
      INTEGER ::   ji            ! dummy loop indices
      INTEGER ::   ilocal_comm   ! local integer
      INTEGER ::   ios
      LOGICAL ::   llexist
      CHARACTER(len=80), DIMENSION(16) ::   cltxt
      !!
      NAMELIST/namctl/ ln_ctl  , nn_print, nn_ictls, nn_ictle,   &
         &             nn_isplt, nn_jsplt, nn_jctls, nn_jctle,   &
         &             nn_bench, nn_timing
      NAMELIST/namcfg/ cp_cfg, cp_cfz, jp_cfg, jpidta, jpjdta, jpkdta, jpiglo, jpjglo, &
         &             jpizoom, jpjzoom, jperio, ln_use_jattr
      !!----------------------------------------------------------------------
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

      !
      !                             !--------------------------------------------!
      !                             !  set communicator & select the local node  !
      !                             !  NB: mynode also opens output.namelist.dyn !
      !                             !      on unit number numond on first proc   !
      !                             !--------------------------------------------!
#if defined key_iomput
      CALL  xios_initialize( "for_xios_mpi_id",return_comm=ilocal_comm )
      narea = mynode( cltxt, 'output.namelist.dyn', numnam_ref, numnam_cfg, numond , nstop, ilocal_comm )   ! Nodes selection
#else
      ilocal_comm = 0
      narea = mynode( cltxt, 'output.namelist.dyn', numnam_ref, numnam_cfg, numond , nstop )                ! Nodes selection (control print return in cltxt)
#endif

      narea = narea + 1                       ! mynode return the rank of proc (0 --> jpnij -1 )

      lwm = (narea == 1)                      ! control of output namelists
      lwp = (narea == 1) .OR. ln_ctl          ! control of all listing output print

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
         CALL nemo_partition(mppsize)
#else
         jpni = 1
         jpnj = 1
         jpnij = jpni*jpnj
#endif
      END IF

      ! Calculate domain dimensions given calculated jpni and jpnj
      ! This used to be done in par_oce.F90 when they were parameters rather
      ! than variables
      jpi = ( jpiglo-2*jpreci + (jpni-1) ) / jpni + 2*jpreci   ! first  dim.
      jpj = ( jpjglo-2*jprecj + (jpnj-1) ) / jpnj + 2*jprecj   ! second dim.
      jpk = jpkdta                                             ! third dim
      jpim1 = jpi-1                                            ! inner domain indices
      jpjm1 = jpj-1                                            !   "           "
      jpkm1 = jpk-1                                            !   "           "
      jpij  = jpi*jpj                                          !  jpi x j


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

      !                             !--------------------------------!
      !                             !  Model general initialization  !
      !                             !--------------------------------!

      CALL nemo_ctl                           ! Control prints & Benchmark

      !                                      ! Domain decomposition
      IF( jpni*jpnj == jpnij ) THEN   ;   CALL mpp_init      ! standard cutting out
      ELSE                            ;   CALL mpp_init2     ! eliminate land processors
      ENDIF
      !
      IF( nn_timing == 1 )  CALL timing_init
      !

      !                                      ! General initialization
      IF( nn_timing == 1 )  CALL timing_start( 'nemo_init')
      !
                            CALL  phy_cst    ! Physical constants
                            CALL  eos_init   ! Equation of state
      IF( lk_c1d        )   CALL  c1d_init   ! 1D column configuration
                            CALL  dom_cfg    ! Domain configuration
      !
      !
      INQUIRE( FILE='coordinates.nc', EXIST = llexist )   ! Check if coordinate file exist
      !
      IF( llexist )  THEN ;  CALL  dom_init   !  compute the grid from coordinates and bathymetry
      ELSE                ;  CALL  dom_rea    !  read grid from the meskmask
      ENDIF
                            CALL  istate_init   ! ocean initial state (Dynamics and tracers)

      IF( ln_nnogather )    CALL  nemo_northcomms   ! Initialise the northfold neighbour lists (must be done after the masks are defined)

      IF( ln_ctl       )    CALL  prt_ctl_init   ! Print control

                            CALL     sbc_init   ! Forcings : surface module

#if ! defined key_degrad
                            CALL ldf_tra_init   ! Lateral ocean tracer physics
#endif
      IF( lk_ldfslp )       CALL ldf_slp_init   ! slope of lateral mixing

                            CALL tra_qsr_init   ! penetrative solar radiation qsr
      IF( lk_trabbl   )     CALL tra_bbl_init   ! advective (and/or diffusive) bottom boundary layer scheme

                            CALL trc_nam_run    ! Needed to get restart parameters for passive tracers
                            CALL trc_rst_cal( nit000, 'READ' )   ! calendar
                            CALL dta_dyn_init   ! Initialization for the dynamics
                            CALL     trc_init   ! Passive tracers initialization
      !                                         ! in various advection and diffusion routines
      IF(lwp) WRITE(numout,cform_aaa)           ! Flag AAAAAAA
      !
      IF( nn_timing == 1 )  CALL timing_stop( 'nemo_init')
      !
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
      IF(lwp) THEN                  ! Parameter print
         WRITE(numout,*)
         WRITE(numout,*) 'nemo_flg: Control prints & Benchmark'
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
     IF(lwp) THEN                  ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'namcfg  : configuration initialization through namelist read'
         WRITE(numout,*) '~~~~~~~ '
         WRITE(numout,*) '   Namelist namcfg'
         WRITE(numout,*) '      configuration name              cp_cfg      = ', TRIM(cp_cfg)
         WRITE(numout,*) '      configuration resolution        jp_cfg      = ', jp_cfg
         WRITE(numout,*) '      1st lateral dimension ( >= jpi ) jpidta     = ', jpidta
         WRITE(numout,*) '      2nd    "         "    ( >= jpj ) jpjdta     = ', jpjdta
         WRITE(numout,*) '      3nd    "         "               jpkdta     = ', jpkdta
         WRITE(numout,*) '      1st dimension of global domain in i jpiglo  = ', jpiglo
         WRITE(numout,*) '      2nd    -                  -    in j jpjglo  = ', jpjglo
         WRITE(numout,*) '      left bottom i index of the zoom (in data domain) jpizoom = ', jpizoom
         WRITE(numout,*) '      left bottom j index of the zoom (in data domain) jpizoom = ', jpjzoom
         WRITE(numout,*) '      lateral cond. type (between 0 and 6) jperio = ', jperio   
         WRITE(numout,*) '      use file attribute if exists as i/p j-start ln_use_jattr = ', ln_use_jattr
      ENDIF
      !                             ! Parameter control
      !
      IF( ln_ctl ) THEN                 ! sub-domain area indices for the control prints
         IF( lk_mpp ) THEN
            isplt = jpni   ;   jsplt = jpnj   ;   ijsplt = jpni*jpnj   ! the domain is forced to the real splitted domain
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
      IF( nbench == 1 )   THEN            ! Benchmark 
         SELECT CASE ( cp_cfg )
         CASE ( 'gyre' )   ;   CALL ctl_warn( ' The Benchmark is activated ' )
         CASE DEFAULT      ;   CALL ctl_stop( ' The Benchmark is based on the GYRE configuration:',   &
            &                                 ' cp_cfg="gyre" in namelsit &namcfg or set nbench = 0' )
         END SELECT
      ENDIF
      !
      IF( lk_c1d .AND. .NOT.lk_iomput )   CALL ctl_stop( 'nemo_ctl: The 1D configuration must be used ',   &
         &                                               'with the IOM Input/Output manager. '        ,   &
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
      IF ( lk_mpp ) CALL mppsync
      !
      CALL iom_close                                 ! close all input/output files managed by iom_*
      !
      IF( numstp     /= -1 )   CLOSE( numstp     )   ! time-step file
      IF( numnam_ref /= -1 )   CLOSE( numnam_ref )   ! oce reference namelist
      IF( numnam_cfg /= -1 )   CLOSE( numnam_cfg )   ! oce configuration namelist
      IF( numout     /=  6 )   CLOSE( numout     )   ! standard model output file
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
      USE diawri,       ONLY: dia_wri_alloc
      USE dom_oce,      ONLY: dom_oce_alloc
      USE zdf_oce,      ONLY: zdf_oce_alloc
      USE ldftra_oce,   ONLY: ldftra_oce_alloc
      USE trc_oce,      ONLY: trc_oce_alloc
      !
      INTEGER :: ierr
      !!----------------------------------------------------------------------
      !
      ierr =        oce_alloc       ()          ! ocean 
      ierr = ierr + dia_wri_alloc   ()
      ierr = ierr + dom_oce_alloc   ()          ! ocean domain
      ierr = ierr + ldftra_oce_alloc()          ! ocean lateral  physics : tracers
      ierr = ierr + zdf_oce_alloc   ()          ! ocean vertical physics
      !
      ierr = ierr + trc_oce_alloc   ()          ! shared TRC / TRA arrays
      !
      IF( lk_mpp    )   CALL mpp_sum( ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'nemo_alloc: unable to allocate standard ocean arrays' )
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
      !
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
      !! nemo_northcomms    :  Setup for north fold exchanges with explicit 
      !!                       point-to-point messaging
      !!=====================================================================
      !!----------------------------------------------------------------------
      !!
      !! ** Purpose :   Initialization of the northern neighbours lists.
      !!----------------------------------------------------------------------
      !!    1.0  ! 2011-10  (A. C. Coward, NOCS & J. Donners, PRACE)
      !!    2.0  ! 2013-06 Setup avoiding MPI communication (I. Epicoco, S.
      !Mocavero, CMCC) 
      !!----------------------------------------------------------------------

      INTEGER  ::   sxM, dxM, sxT, dxT, jn
      INTEGER  ::   njmppmax

      njmppmax = MAXVAL( njmppt )

      !initializes the north-fold communication variables
      isendto(:) = 0
      nsndto = 0

      !if I am a process in the north
      IF ( njmpp == njmppmax ) THEN
          !sxM is the first point (in the global domain) needed to compute the
          !north-fold for the current process
          sxM = jpiglo - nimppt(narea) - nlcit(narea) + 1
          !dxM is the last point (in the global domain) needed to compute the
          !north-fold for the current process
          dxM = jpiglo - nimppt(narea) + 2

          !loop over the other north-fold processes to find the processes
          !managing the points belonging to the sxT-dxT range

          DO jn = 1, jpni
                !sxT is the first point (in the global domain) of the jn
                !process
                sxT = nfiimpp(jn, jpnj)
                !dxT is the last point (in the global domain) of the jn
                !process
                dxT = nfiimpp(jn, jpnj) + nfilcit(jn, jpnj) - 1
                IF ((sxM .gt. sxT) .AND. (sxM .lt. dxT)) THEN
                   nsndto = nsndto + 1
                     isendto(nsndto) = jn
                ELSEIF ((sxM .le. sxT) .AND. (dxM .ge. dxT)) THEN
                   nsndto = nsndto + 1
                     isendto(nsndto) = jn
                ELSEIF ((dxM .lt. dxT) .AND. (sxT .lt. dxM)) THEN
                   nsndto = nsndto + 1
                     isendto(nsndto) = jn
                END IF
          END DO
          nfsloop = 1
          nfeloop = nlci
          DO jn = 2,jpni-1
           IF(nfipproc(jn,jpnj) .eq. (narea - 1)) THEN
              IF (nfipproc(jn - 1 ,jpnj) .eq. -1) THEN
                 nfsloop = nldi
              ENDIF
              IF (nfipproc(jn + 1,jpnj) .eq. -1) THEN
                 nfeloop = nlei
              ENDIF
           ENDIF
        END DO

      ENDIF
      l_north_nogather = .TRUE.
   END SUBROUTINE nemo_northcomms
#else
   SUBROUTINE nemo_northcomms      ! Dummy routine
      WRITE(*,*) 'nemo_northcomms: You should not have seen this print! error?'
   END SUBROUTINE nemo_northcomms
#endif

   SUBROUTINE istate_init
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE istate_init  ***
      !!
      !! ** Purpose :   Initialization to zero of the dynamics and tracers.
      !!----------------------------------------------------------------------
      !
      !     now fields         !     after fields      !
      un   (:,:,:)   = 0._wp   ;   ua(:,:,:) = 0._wp   !
      vn   (:,:,:)   = 0._wp   ;   va(:,:,:) = 0._wp   !
      wn   (:,:,:)   = 0._wp   !                       !
      hdivn(:,:,:)   = 0._wp   !                       !
      tsn  (:,:,:,:) = 0._wp   !                       !
      !
      rhd  (:,:,:) = 0.e0
      rhop (:,:,:) = 0.e0
      rn2  (:,:,:) = 0.e0
      !
   END SUBROUTINE istate_init

   SUBROUTINE stp_ctl( kt, kindic )
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
      INTEGER, INTENT(inout) ::   kindic  ! indicator of solver convergence
      !!----------------------------------------------------------------------
      !
      IF( kt == nit000 .AND. lwp ) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'stp_ctl : time-stepping control'
         WRITE(numout,*) '~~~~~~~'
         ! open time.step file
         CALL ctl_opn( numstp, 'time.step', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, numout, lwp, narea )
      ENDIF
      !
      IF(lwp) WRITE ( numstp, '(1x, i8)' )   kt      !* save the current time step in numstp
      IF(lwp) REWIND( numstp )                       ! --------------------------
      !
   END SUBROUTINE stp_ctl
   !!======================================================================
END MODULE nemogcm
