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
   !!            3.4  ! 2011-10  (A. C. Coward, NOCS & J. Donners, PRACE) add nemo_northcomms
   !!             -   ! 2011-11  (C. Harris) decomposition changes for running with CICE
   !!            3.6  ! 2012-05  (C. Calone, J. Simeon, G. Madec, C. Ethe) Add grid coarsening 
   !!             -   ! 2013-06  (I. Epicoco, S. Mocavero, CMCC) nemo_northcomms: setup avoiding MPI communication 
   !!             -   ! 2014-12  (G. Madec) remove KPP scheme and cross-land advection (cla)
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   nemo_gcm      : solve ocean dynamics, tracer, biogeochemistry and/or sea-ice
   !!   nemo_init     : initialization of the NEMO system
   !!   nemo_ctl      : initialisation of the contol print
   !!   nemo_closefile: close remaining open files
   !!   nemo_alloc    : dynamical allocation
   !!   nemo_partition: calculate MPP domain decomposition
   !!   factorise     : calculate the factors of the no. of MPI processes
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space and time domain variables
   USE in_out_manager ! I/O manager
   USE iom            !
   USE domcfg         ! domain configuration               (dom_cfg routine)
   USE mppini         ! shared/distributed memory setting (mpp_init routine)
   USE domain         ! domain initialization             (dom_init routine)
   USE phycst         ! physical constant                  (par_cst routine)
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)
   USE lib_mpp        ! distributed memory computing


   USE agrif_connect
   USE agrif_dom_update
   USE agrif_recompute_scales

   USE halo_mng

   IMPLICIT NONE
   PRIVATE

   PUBLIC   nemo_gcm    ! called by model.F90
   PUBLIC   nemo_init   ! needed by AGRIF
   PUBLIC   nemo_alloc  ! needed by TAM

   CHARACTER(lc) ::   cform_aaa="( /, 'AAAAAAAA', / ) "     ! flag for output listing

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.7 , NEMO Consortium (2015)
   !! $Id: nemogcm.F90 6152 2015-12-21 22:33:57Z acc $
   !! Software governed by the CeCILL licence     (./LICENSE)
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
      INTEGER ::   istp       ! time step index
      !!----------------------------------------------------------------------
      !
#if defined key_agrif
      CALL Agrif_Init_Grids()      ! AGRIF: set the meshes
#endif
      !                            !-----------------------!
      CALL nemo_init               !==  Initialisations  ==!
      !                            !-----------------------!

#if defined key_agrif    
      CALL Agrif_Regrid()
      
      CALL Agrif_Step_Child(agrif_boundary_connections)

      CALL Agrif_Step_Child(agrif_recompute_scalefactors)
      
      CALL Agrif_Step_Child_adj(agrif_update_all)
      
      CALL Agrif_Step_Child(cfg_write)
#endif

      ! check that all process are still there... If some process have an error,
      ! they will never enter in step and other processes will wait until the end of the cpu time!
      IF( lk_mpp )   CALL mpp_max( 'nemogcm',nstop )

      IF(lwp) WRITE(numout,cform_aaa)   ! Flag AAAAAAA

      !                            !-----------------------!
      !                            !==  finalize the run  ==!
      !                            !------------------------!
      !
      IF( nstop /= 0 .AND. lwp ) THEN        ! error print
         ngrdstop = Agrif_Fixed()
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
      !
      CALL nemo_closefile
      !
      !
   END SUBROUTINE nemo_gcm


   SUBROUTINE nemo_init
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE nemo_init  ***
      !!
      !! ** Purpose :   initialization of the NEMO GCM
      !!----------------------------------------------------------------------
      INTEGER  ::   ji                 ! dummy loop indices
      INTEGER  ::   ios, ilocal_comm   ! local integers
      CHARACTER(len=120), DIMENSION(60) ::   cltxt, cltxt2, clnam
      !!
      NAMELIST/namctl/ sn_cfctl, ln_timing, ln_diacfl,                                &
         &             nn_isplt,  nn_jsplt,  nn_ictls, nn_ictle, nn_jctls, nn_jctle  
      NAMELIST/namcfg/ ln_e3_dep, ln_dept_mid,                                         &
         &             cp_cfg, cp_cfz, jp_cfg, jpidta, jpjdta, jpkdta, Ni0glo, Nj0glo, &
         &             jpkglo, jperio, ln_use_jattr, ln_domclo
      !!----------------------------------------------------------------------
      !

      !
      !                             !-------------------------------------------------!
      !                             !     set communicator & select the local rank    !
      !                             !  must be done as soon as possible to get narea  !
      !                             !-------------------------------------------------!
      !
#if defined key_iomput
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
!      IF( lk_oasis ) THEN
!         IF( Agrif_Root() ) THEN
!            CALL cpl_init( "oceanx", ilocal_comm )          ! nemo local communicator given by oasis
!         ENDIF
!         CALL mpp_start( ilocal_comm )
!      ELSE
         CALL mpp_start( )
!      ENDIF
#endif
      !
      narea = mpprank + 1               ! mpprank: the rank of proc (0 --> mppsize -1 )
      lwm = (narea == 1)                ! control of output namelists

      cltxt = ''
      !
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

      READ  ( numnam_ref, namcfg, IOSTAT = ios, ERR = 903 )
903   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namcfg in reference namelist')
      READ  ( numnam_cfg, namcfg, IOSTAT = ios, ERR = 904 )
904   IF( ios >  0 )   CALL ctl_nam ( ios , 'namcfg in configuration namelist') 

      lwp = (narea == 1)                                    ! control of all listing output print

      IF(lwm) THEN
         ! write merged namelists from earlier to output namelist now that the
         ! file has been opened in call to mynode. nammpp has already been
         ! written in mynode (if lk_mpp_mpi)
         WRITE( numond, namcfg )
      ENDIF

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
         
         ! Print the working precision to ocean.output
         IF (wp == dp) THEN
            WRITE(numout,*) "Working precision = double-precision"
         ELSE
            WRITE(numout,*) "Working precision = single-precision"
         ENDIF
         WRITE(numout,*)
         !
         WRITE(numout,cform_aaa)                                        ! Flag AAAAAAA
         !
      ENDIF
      !
      IF(lwm) WRITE( numond, namctl )
      !
      !                             !-----------------------------------------!
      !                             ! mpp parameters and domain decomposition !
      !                             !-----------------------------------------!
      CALL mpp_init
      CALL halo_mng_init()
      ! Now we know the dimensions of the grid and numout has been set: we can allocate arrays

 !    IF( Agrif_Root() ) THEN
 !        jpi = ( jpiglo     -2*jpreci + (jpni-1) ) / jpni + 2*jpreci    ! first  dim.
 !        jpj = ( jpjglo     -2*jprecj + (jpnj-1) ) / jpnj + 2*jprecj    ! second dim.
 !     ENDIF
         jpk = jpkglo                                             ! third dim
         jpim1 = jpi-1                                            ! inner domain indices
         jpjm1 = jpj-1                                            !   "           "
         jpkm1 = jpk-1                                            !   "           "
         jpij  = jpi*jpj                                          !  jpi x j

#if defined key_agrif
      CALL Agrif_Declare_Var
#endif

      ! Now we know the dimensions of the grid and numout has been set: we can allocate arrays
      CALL nemo_alloc()

      !                             !-------------------------------!
      !                             !  NEMO general initialization  !
      !                             !-------------------------------!

      !                                      ! Domain decomposition
        !
      !                                      ! General initialization
                            CALL     phy_cst    ! Physical constants
                            CALL     dom_cfg    ! Domain configuration
                            CALL     dom_init   ! Domain
      !
   END SUBROUTINE nemo_init

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
      IF( lwm.AND.numond  /= -1 )   CLOSE( numond          )   ! oce output namelist
      IF( numout          /=  6 )   CLOSE( numout          )   ! standard model output file
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
      USE dom_oce   , ONLY: dom_oce_alloc
      !
      INTEGER :: ierr
      !!----------------------------------------------------------------------
      !
      ierr = 0
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

   SUBROUTINE nemo_partition( num_pes )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE nemo_partition  ***
      !!
      !! ** Purpose :
      !!
      !! ** Method  :
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   num_pes   ! The number of MPI processes we have
      !
      INTEGER, PARAMETER :: nfactmax = 20
      INTEGER :: nfact ! The no. of factors returned
      INTEGER :: ierr  ! Error flag
      INTEGER :: ji
      INTEGER :: idiff, mindiff, imin ! For choosing pair of factors that are closest in value
      INTEGER, DIMENSION(nfactmax) :: ifact ! Array of factors
      !!----------------------------------------------------------------------
      !
      ierr = 0
      !
      CALL factorise( ifact, nfactmax, nfact, num_pes, ierr )
      !
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
      INTEGER, DIMENSION(ntest) ::   ilfax
      !!----------------------------------------------------------------------
      !
      ! lfax contains the set of allowed factors.
      ilfax(:) = (/(2**jl,jl=ntest,1,-1)/)
      !
      ! Clear the error flag and initialise output vars
      kerr  = 0
      kfax  = 1
      knfax = 0
      !
      ! Find the factors of n.
      IF( kn == 1 )   GOTO 20

      ! nu holds the unfactorised part of the number.
      ! knfax holds the number of factors found.
      ! l points to the allowed factor list.
      ! ifac holds the current factor.
      !
      inu   = kn
      knfax = 0
      !
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
      !
   20 CONTINUE      ! Label 20 is the exit point from the factor search loop.
      !
   END SUBROUTINE factorise

   !!======================================================================
END MODULE nemogcm

