MODULE domrea
   !!==============================================================================
   !!                       ***  MODULE domrea   ***
   !! Ocean initialization : domain initialization
   !!==============================================================================

   !!----------------------------------------------------------------------
   !!   dom_init       : initialize the space and time domain
   !!   dom_nam        : read and contral domain namelists
   !!   dom_ctl        : control print for the ocean domain
   !!----------------------------------------------------------------------
   !! * Modules used
   USE oce             ! 
   USE dom_oce         ! ocean space and time domain
   USE phycst          ! physical constants
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! distributed memory computing library

   USE iom
   USE domstp          ! domain: set the time-step

   USE lbclnk          ! lateral boundary condition - MPP exchanges
   USE trc_oce         ! shared ocean/biogeochemical variables
   USE wrk_nemo  
   
   IMPLICIT NONE
   PRIVATE

   !! * Routine accessibility
   PUBLIC dom_rea       ! called by opa.F90

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OFF 3.3 , NEMO Consortium (2010)
   !! $Id: domrea.F90 8617 2017-10-12 08:18:54Z cetlod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE dom_rea
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dom_rea  ***
      !!                    
      !! ** Purpose :   Domain initialization. Call the routines that are 
      !!      required to create the arrays which define the space and time
      !!      domain of the ocean model.
      !!
      !! ** Method  :
      !!      - dom_stp: defined the model time step
      !!      - dom_rea: read the meshmask file if nmsh=1
      !!
      !! History :
      !!        !  90-10  (C. Levy - G. Madec)  Original code
      !!        !  91-11  (G. Madec)
      !!        !  92-01  (M. Imbard) insert time step initialization
      !!        !  96-06  (G. Madec) generalized vertical coordinate 
      !!        !  97-02  (G. Madec) creation of domwri.F
      !!        !  01-05  (E.Durand - G. Madec) insert closed sea
      !!   8.5  !  02-08  (G. Madec)  F90: Free form and module
      !!----------------------------------------------------------------------
      !! * Local declarations
      INTEGER ::   jk                ! dummy loop argument
      INTEGER ::   iconf = 0         ! temporary integers
      !!----------------------------------------------------------------------

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'dom_init : domain initialization'
         WRITE(numout,*) '~~~~~~~~'
      ENDIF

      CALL dom_nam      ! read namelist ( namrun, namdom, namcla )
      CALL dom_msk      ! Masks
      CALL dom_hgr      ! Horizontal grid
      CALL dom_zgr      ! Vertical mesh and bathymetry option
      !
      e12t    (:,:) = e1t(:,:) * e2t(:,:)
      e1e2t   (:,:) = e1t(:,:) * e2t(:,:)
      e12u    (:,:) = e1u(:,:) * e2u(:,:)
      e12v    (:,:) = e1v(:,:) * e2v(:,:)
      r1_e12t (:,:) = 1._wp    / e12t(:,:)
      r1_e12u (:,:) = 1._wp    / e12u(:,:)
      r1_e12v (:,:) = 1._wp    / e12v(:,:)
      re2u_e1u(:,:) = e2u(:,:) / e1u(:,:)
      re1v_e2v(:,:) = e1v(:,:) / e2v(:,:)
      !
      CALL dom_stp      ! Time step
      CALL dom_ctl      ! Domain control

   END SUBROUTINE dom_rea

   SUBROUTINE dom_nam
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE dom_nam  ***
      !!                    
      !! ** Purpose :   read domaine namelists and print the variables.
      !!
      !! ** input   : - namrun namelist
      !!              - namdom namelist
      !!              - namcla namelist
      !!----------------------------------------------------------------------
      USE ioipsl
      INTEGER  ::   ios                 ! Local integer output status for namelist read
      NAMELIST/namrun/ cn_ocerst_indir, cn_ocerst_outdir, nn_stocklist, ln_rst_list,               &
         &             nn_no   , cn_exp    , cn_ocerst_in, cn_ocerst_out, ln_rstart , nn_rstctl,   &
         &             nn_it000, nn_itend  , nn_date0    , nn_leapy     , nn_istate , nn_stock ,   &
         &             nn_write, ln_dimgnnn, ln_mskland  , ln_cfmeta    , ln_clobber, nn_chunksz, nn_euler
      NAMELIST/namdom/ nn_bathy , rn_bathy, rn_e3zps_min, rn_e3zps_rat, nn_msh    , rn_hmin,   &
         &             nn_acc   , rn_atfp     , rn_rdt      , rn_rdtmin ,            &
         &             rn_rdtmax, rn_rdth     , nn_baro     , nn_closea , ln_crs, &
         &             jphgr_msh, &
         &             ppglam0, ppgphi0, ppe1_deg, ppe2_deg, ppe1_m, ppe2_m, &
         &             ppsur, ppa0, ppa1, ppkth, ppacr, ppdzmin, pphmax, ldbletanh, &
         &             ppa2, ppkth2, ppacr2
      NAMELIST/namcla/ nn_cla
#if defined key_netcdf4
      NAMELIST/namnc4/ nn_nchunks_i, nn_nchunks_j, nn_nchunks_k, ln_nc4zip
#endif
      !!----------------------------------------------------------------------

      REWIND( numnam_ref )              ! Namelist namrun in reference namelist : Parameters of the run
      READ  ( numnam_ref, namrun, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namrun in reference namelist', lwp )

      REWIND( numnam_cfg )              ! Namelist namrun in configuration namelist : Parameters of the run
      READ  ( numnam_cfg, namrun, IOSTAT = ios, ERR = 902 )
902   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namrun in configuration namelist', lwp )
      IF(lwm) WRITE ( numond, namrun )
      !
      IF(lwp) THEN                  ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'dom_nam  : domain initialization through namelist read'
         WRITE(numout,*) '~~~~~~~ '
         WRITE(numout,*) '   Namelist namrun'  
         WRITE(numout,*) '      job number                      nn_no      = ', nn_no
         WRITE(numout,*) '      experiment name for output      cn_exp     = ', cn_exp
         WRITE(numout,*) '      restart logical                 ln_rstart  = ', ln_rstart
         WRITE(numout,*) '      control of time step            nn_rstctl  = ', nn_rstctl
         WRITE(numout,*) '      number of the first time step   nn_it000   = ', nn_it000
         WRITE(numout,*) '      number of the last time step    nn_itend   = ', nn_itend
         WRITE(numout,*) '      initial calendar date aammjj    nn_date0   = ', nn_date0
         WRITE(numout,*) '      leap year calendar (0/1)        nn_leapy   = ', nn_leapy
         WRITE(numout,*) '      initial state output            nn_istate  = ', nn_istate
         WRITE(numout,*) '      frequency of restart file       nn_stock   = ', nn_stock
         WRITE(numout,*) '      frequency of output file        nn_write   = ', nn_write
         WRITE(numout,*) '      multi file dimgout              ln_dimgnnn = ', ln_dimgnnn
         WRITE(numout,*) '      mask land points                ln_mskland = ', ln_mskland
         WRITE(numout,*) '      additional CF standard metadata ln_cfmeta  = ', ln_cfmeta
         WRITE(numout,*) '      overwrite an existing file      ln_clobber = ', ln_clobber
         WRITE(numout,*) '      NetCDF chunksize (bytes)        nn_chunksz = ', nn_chunksz
      ENDIF
      no = nn_no                    ! conversion DOCTOR names into model names (this should disappear soon)
      cexper = cn_exp
      nrstdt = nn_rstctl
      nit000 = nn_it000
      nitend = nn_itend
      ndate0 = nn_date0
      nleapy = nn_leapy
      ninist = nn_istate
      nstock = nn_stock
      nstocklist = nn_stocklist
      nwrite = nn_write
      !                             ! control of output frequency
      IF ( nstock == 0 .OR. nstock > nitend ) THEN
         WRITE(ctmp1,*) 'nstock = ', nstock, ' it is forced to ', nitend
         CALL ctl_warn( ctmp1 )
         nstock = nitend
      ENDIF
      IF ( nwrite == 0 ) THEN
         WRITE(ctmp1,*) 'nwrite = ', nwrite, ' it is forced to ', nitend
         CALL ctl_warn( ctmp1 )
         nwrite = nitend
      ENDIF

      ! parameters correspondting to nit000 - 1 (as we start the step loop with a call to day)
      ndastp = ndate0 - 1        ! ndate0 read in the namelist in dom_nam, we assume that we start run at 00:00
      adatrj = ( REAL( nit000-1, wp ) * rdttra(1) ) / rday

#if defined key_agrif
      IF( Agrif_Root() ) THEN
#endif
      SELECT CASE ( nleapy )        ! Choose calendar for IOIPSL
      CASE (  1 ) 
         CALL ioconf_calendar('gregorian')
         IF(lwp) WRITE(numout,*) '   The IOIPSL calendar is "gregorian", i.e. leap year'
      CASE (  0 )
         CALL ioconf_calendar('noleap')
         IF(lwp) WRITE(numout,*) '   The IOIPSL calendar is "noleap", i.e. no leap year'
      CASE ( 30 )
         CALL ioconf_calendar('360d')
         IF(lwp) WRITE(numout,*) '   The IOIPSL calendar is "360d", i.e. 360 days in a year'
      END SELECT
#if defined key_agrif
      ENDIF
#endif

      REWIND( numnam_ref )              ! Namelist namdom in reference namelist : space & time domain (bathymetry, mesh, timestep)
      READ  ( numnam_ref, namdom, IOSTAT = ios, ERR = 903)
903   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namdom in reference namelist', lwp )

      REWIND( numnam_cfg )              ! Namelist namdom in configuration namelist : space & time domain (bathymetry, mesh, timestep)
      READ  ( numnam_cfg, namdom, IOSTAT = ios, ERR = 904 )
904   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namdom in configuration namelist', lwp )
      IF(lwm) WRITE ( numond, namdom )


      IF(lwp) THEN
         WRITE(numout,*) 
         WRITE(numout,*) '   Namelist namdom : space & time domain'
         WRITE(numout,*) '      flag read/compute bathymetry      nn_bathy     = ', nn_bathy
         WRITE(numout,*) '      Depth (if =0 bathy=jpkm1)         rn_bathy     = ', rn_bathy
         WRITE(numout,*) '      min depth of the ocean    (>0) or    rn_hmin   = ', rn_hmin
         WRITE(numout,*) '      minimum thickness of partial      rn_e3zps_min = ', rn_e3zps_min, ' (m)'
         WRITE(numout,*) '         step level                     rn_e3zps_rat = ', rn_e3zps_rat
         WRITE(numout,*) '      create mesh/mask file(s)          nn_msh       = ', nn_msh
         WRITE(numout,*) '           = 0   no file created                 '
         WRITE(numout,*) '           = 1   mesh_mask                       '
         WRITE(numout,*) '           = 2   mesh and mask                   '
         WRITE(numout,*) '           = 3   mesh_hgr, msh_zgr and mask      '
         WRITE(numout,*) '      ocean time step                      rn_rdt    = ', rn_rdt
         WRITE(numout,*) '      asselin time filter parameter        rn_atfp   = ', rn_atfp
         WRITE(numout,*) '      time-splitting: nb of sub time-step  nn_baro   = ', nn_baro
         WRITE(numout,*) '      acceleration of converge             nn_acc    = ', nn_acc
         WRITE(numout,*) '        nn_acc=1: surface tracer rdt       rn_rdtmin = ', rn_rdtmin
         WRITE(numout,*) '                  bottom  tracer rdt       rdtmax    = ', rn_rdtmax
         WRITE(numout,*) '                  depth of transition      rn_rdth   = ', rn_rdth
         WRITE(numout,*) '      suppression of closed seas (=0)      nn_closea = ', nn_closea
         WRITE(numout,*) '      type of horizontal mesh jphgr_msh           = ', jphgr_msh
         WRITE(numout,*) '      longitude of first raw and column T-point ppglam0 = ', ppglam0
         WRITE(numout,*) '      latitude  of first raw and column T-point ppgphi0 = ', ppgphi0
         WRITE(numout,*) '      zonal      grid-spacing (degrees) ppe1_deg        = ', ppe1_deg
         WRITE(numout,*) '      meridional grid-spacing (degrees) ppe2_deg        = ', ppe2_deg
         WRITE(numout,*) '      zonal      grid-spacing (degrees) ppe1_m          = ', ppe1_m
         WRITE(numout,*) '      meridional grid-spacing (degrees) ppe2_m          = ', ppe2_m
         WRITE(numout,*) '      ORCA r4, r2 and r05 coefficients  ppsur           = ', ppsur
         WRITE(numout,*) '                                        ppa0            = ', ppa0
         WRITE(numout,*) '                                        ppa1            = ', ppa1
         WRITE(numout,*) '                                        ppkth           = ', ppkth
         WRITE(numout,*) '                                        ppacr           = ', ppacr
         WRITE(numout,*) '      Minimum vertical spacing ppdzmin                  = ', ppdzmin
         WRITE(numout,*) '      Maximum depth pphmax                              = ', pphmax
         WRITE(numout,*) '      Use double tanf function for vertical coordinates ldbletanh = ', ldbletanh
         WRITE(numout,*) '      Double tanh function parameters ppa2              = ', ppa2
         WRITE(numout,*) '                                      ppkth2            = ', ppkth2
         WRITE(numout,*) '                                      ppacr2            = ', ppacr2
      ENDIF

      ntopo     = nn_bathy          ! conversion DOCTOR names into model names (this should disappear soon)
      e3zps_min = rn_e3zps_min
      e3zps_rat = rn_e3zps_rat
      nmsh      = nn_msh
      nacc      = nn_acc
      atfp      = rn_atfp
      rdt       = rn_rdt
      rdtmin    = rn_rdtmin
      rdtmax    = rn_rdtmin
      rdth      = rn_rdth

      REWIND( numnam_ref )              ! Namelist namcla in reference namelist : Cross land advection
      READ  ( numnam_ref, namcla, IOSTAT = ios, ERR = 905)
905   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namcla in reference namelist', lwp )

      REWIND( numnam_cfg )              ! Namelist namcla in configuration namelist : Cross land advection
      READ  ( numnam_cfg, namcla, IOSTAT = ios, ERR = 906 )
906   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namcla in configuration namelist', lwp )
      IF(lwm) WRITE( numond, namcla )

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) '   Namelist namcla'
         WRITE(numout,*) '      cross land advection                 nn_cla    = ', nn_cla
      ENDIF

#if defined key_netcdf4
      !                             ! NetCDF 4 case   ("key_netcdf4" defined)
      REWIND( numnam_ref )              ! Namelist namnc4 in reference namelist : NETCDF
      READ  ( numnam_ref, namnc4, IOSTAT = ios, ERR = 907)
907   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namnc4 in reference namelist', lwp )

      REWIND( numnam_cfg )              ! Namelist namnc4 in configuration namelist : NETCDF
      READ  ( numnam_cfg, namnc4, IOSTAT = ios, ERR = 908 )
908   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namnc4 in configuration namelist', lwp )
      IF(lwm) WRITE( numond, namnc4 )
      IF(lwp) THEN                        ! control print
         WRITE(numout,*)
         WRITE(numout,*) '   Namelist namnc4 - Netcdf4 chunking parameters'
         WRITE(numout,*) '      number of chunks in i-dimension      nn_nchunks_i   = ', nn_nchunks_i
         WRITE(numout,*) '      number of chunks in j-dimension      nn_nchunks_j   = ', nn_nchunks_j
         WRITE(numout,*) '      number of chunks in k-dimension      nn_nchunks_k   = ', nn_nchunks_k
         WRITE(numout,*) '      apply netcdf4/hdf5 chunking & compression ln_nc4zip = ', ln_nc4zip
      ENDIF

      ! Put the netcdf4 settings into a simple structure (snc4set, defined in in_out_manager module)
      ! Note the chunk size in the unlimited (time) dimension will be fixed at 1
      snc4set%ni   = nn_nchunks_i
      snc4set%nj   = nn_nchunks_j
      snc4set%nk   = nn_nchunks_k
      snc4set%luse = ln_nc4zip
#else
      snc4set%luse = .FALSE.        ! No NetCDF 4 case
#endif
      !
   END SUBROUTINE dom_nam

   SUBROUTINE dom_msk
      !!---------------------------------------------------------------------
      !!                 ***  ROUTINE dom_msk  ***
      !! ** Purpose :  Read the NetCDF file(s) which contain(s) all the
      !!      ocean mask informations and defines the interior domain T-mask.
      !!
      !! ** Method  :  Read in a file all the arrays generated in routines
      !!               dommsk:   'mask.nc' file
      !!              The interior ocean/land mask is computed from tmask
      !!              setting to zero the duplicated row and lines due to
      !!              MPP exchange halos, est-west cyclic and north fold
      !!              boundary conditions.
      !!
      !! ** Action :   tmask_i  : interiorland/ocean mask at t-point
      !!               tpol     : ???
      !!----------------------------------------------------------------------
      !
      INTEGER  ::  inum   ! local integers
      INTEGER  ::   ji, jj, jk                   ! dummy loop indices
      INTEGER  ::   iif, iil, ijf, ijl       ! local integers
      REAL(wp), POINTER, DIMENSION(:,:) :: zmbk
      !
      !!---------------------------------------------------------------------
      


      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'dom_rea : read NetCDF mesh and mask information file(s)'
      IF(lwp) WRITE(numout,*) '~~~~~~~'

      CALL wrk_alloc( jpi, jpj, zmbk )
      zmbk(:,:) = 0._wp

      IF(lwp) WRITE(numout,*) '          one file in "mesh_mask.nc" '
      CALL iom_open( 'mask', inum )

         !                                                         ! masks (inum2) 
      CALL iom_get( inum, jpdom_data, 'tmask', tmask )
      CALL iom_get( inum, jpdom_data, 'umask', umask )
      CALL iom_get( inum, jpdom_data, 'vmask', vmask )
      CALL iom_get( inum, jpdom_data, 'fmask', fmask )

      CALL lbc_lnk( tmask, 'T', 1._wp )    ! Lateral boundary conditions
      CALL lbc_lnk( umask, 'U', 1._wp )      
      CALL lbc_lnk( vmask, 'V', 1._wp )
      CALL lbc_lnk( fmask, 'F', 1._wp )

#if defined key_c1d
      ! set umask and vmask equal tmask in 1D configuration
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) '**********  1D configuration : set umask and vmask equal tmask ********'
      IF(lwp) WRITE(numout,*) '**********                                                     ********'

      umask(:,:,:) = tmask(:,:,:)
      vmask(:,:,:) = tmask(:,:,:)
#endif

#if defined key_degrad
      CALL iom_get( inum, jpdom_data, 'facvolt', facvol )
#endif

      CALL iom_get( inum, jpdom_data, 'mbathy', zmbk )              ! number of ocean t-points
      mbathy (:,:) = INT( zmbk(:,:) )
      misfdep(:,:) = 1                                               ! ice shelf case not yet done
      
      CALL zgr_bot_level                                             ! mbk. arrays (deepest ocean t-, u- & v-points

      !                                     ! ============================
      !                                     !        close the files 
      !                                     ! ============================

      !
      ! Interior domain mask (used for global sum)
      ! --------------------
      ssmask(:,:)  = tmask(:,:,1)
      tmask_i(:,:) = tmask(:,:,1)
      iif = jpreci                        ! thickness of exchange halos in i-axis
      iil = nlci - jpreci + 1
      ijf = jprecj                        ! thickness of exchange halos in j-axis
      ijl = nlcj - jprecj + 1
      !
      tmask_i( 1 :iif,   :   ) = 0._wp    ! first columns
      tmask_i(iil:jpi,   :   ) = 0._wp    ! last  columns (including mpp extra columns)
      tmask_i(   :   , 1 :ijf) = 0._wp    ! first rows
      tmask_i(   :   ,ijl:jpj) = 0._wp    ! last  rows (including mpp extra rows)
      !
      !                                   ! north fold mask
      tpol(1:jpiglo) = 1._wp
      !                                
      IF( jperio == 3 .OR. jperio == 4 )   tpol(jpiglo/2+1:jpiglo) = 0._wp    ! T-point pivot
      IF( jperio == 5 .OR. jperio == 6 )   tpol(     1    :jpiglo) = 0._wp    ! F-point pivot
      IF( jperio == 3 .OR. jperio == 4 ) THEN      ! T-point pivot: only half of the nlcj-1 row
         IF( mjg(ijl-1) == jpjglo-1 ) THEN
            DO ji = iif+1, iil-1
               tmask_i(ji,ijl-1) = tmask_i(ji,ijl-1) * tpol(mig(ji))
            END DO
         ENDIF
      ENDIF 
      !
      ! (ISF) MIN(1,SUM(umask)) is here to check if you have effectively at
      ! least 1 wet u point
      DO jj = 1, jpjm1
         DO ji = 1, fs_jpim1   ! vector loop
            umask_i(ji,jj)  = ssmask(ji,jj) * ssmask(ji+1,jj  )  * MIN(1._wp,SUM(umask(ji,jj,:)))
            vmask_i(ji,jj)  = ssmask(ji,jj) * ssmask(ji  ,jj+1)  * MIN(1._wp,SUM(vmask(ji,jj,:)))
         END DO
         DO ji = 1, jpim1      ! NO vector opt.
            fmask_i(ji,jj) =  ssmask(ji,jj  ) * ssmask(ji+1,jj  )   &
               &            * ssmask(ji,jj+1) * ssmask(ji+1,jj+1) * MIN(1._wp,SUM(fmask(ji,jj,:)))
         END DO
      END DO
      CALL lbc_lnk( umask_i, 'U', 1._wp )      ! Lateral boundary conditions
      CALL lbc_lnk( vmask_i, 'V', 1._wp )
      CALL lbc_lnk( fmask_i, 'F', 1._wp )

      ! 3. Ocean/land mask at wu-, wv- and w points 
      !----------------------------------------------
      wmask (:,:,1) = tmask(:,:,1) ! ????????
      wumask(:,:,1) = umask(:,:,1) ! ????????
      wvmask(:,:,1) = vmask(:,:,1) ! ????????
      DO jk = 2, jpk
         wmask (:,:,jk) = tmask(:,:,jk) * tmask(:,:,jk-1)
         wumask(:,:,jk) = umask(:,:,jk) * umask(:,:,jk-1)   
         wvmask(:,:,jk) = vmask(:,:,jk) * vmask(:,:,jk-1)
      END DO
      !
      CALL wrk_dealloc( jpi, jpj, zmbk )
      !
      CALL iom_close( inum )
      !
   END SUBROUTINE dom_msk

   SUBROUTINE zgr_bot_level
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE zgr_bot_level  ***
      !!
      !! ** Purpose :   defines the vertical index of ocean bottom (mbk. arrays)
      !!
      !! ** Method  :   computes from mbathy with a minimum value of 1 over land
      !!
      !! ** Action  :   mbkt, mbku, mbkv :   vertical indices of the deeptest
      !!                                     ocean level at t-, u- & v-points
      !!                                     (min value = 1 over land)
      !!----------------------------------------------------------------------
      !
      INTEGER ::   ji, jj   ! dummy loop indices
      REAL(wp), POINTER, DIMENSION(:,:) :: zmbk
      !!----------------------------------------------------------------------

      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) '    zgr_bot_level : ocean bottom k-index of T-, U-, V- and W-levels '
      IF(lwp) WRITE(numout,*) '    ~~~~~~~~~~~~~'
      !
      CALL wrk_alloc( jpi, jpj, zmbk )
      !
      mbkt(:,:) = MAX( mbathy(:,:) , 1 )    ! bottom k-index of T-level (=1 over land)
      mikt(:,:) = 1 ; miku(:,:) = 1; mikv(:,:) = 1; ! top k-index of T-level (=1 over open ocean; >1 beneath ice shelf)
      !                                     ! bottom k-index of W-level = mbkt+1
      DO jj = 1, jpjm1                      ! bottom k-index of u- (v-) level
         DO ji = 1, jpim1
            mbku(ji,jj) = MIN(  mbkt(ji+1,jj  ) , mbkt(ji,jj)  )
            mbkv(ji,jj) = MIN(  mbkt(ji  ,jj+1) , mbkt(ji,jj)  )
         END DO
      END DO
      ! converte into REAL to use lbc_lnk ; impose a min value of 1 as a zero can be set in lbclnk 
      zmbk(:,:) = REAL( mbku(:,:), wp )   ;   CALL lbc_lnk(zmbk,'U',1.)   ;   mbku  (:,:) = MAX( INT( zmbk(:,:) ), 1 )
      zmbk(:,:) = REAL( mbkv(:,:), wp )   ;   CALL lbc_lnk(zmbk,'V',1.)   ;   mbkv  (:,:) = MAX( INT( zmbk(:,:) ), 1 )
      !
      CALL wrk_dealloc( jpi, jpj, zmbk )
      !
   END SUBROUTINE zgr_bot_level

   SUBROUTINE dom_hgr
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dom_hgr  ***
      !!                   
      !! ** Purpose :  Read the NetCDF file(s) which contain(s) all the
      !!      ocean horizontal mesh informations 
      !!
      !! ** Method  :   Read in a file all the arrays generated in routines
      !!                domhgr:   'mesh_hgr.nc' file
      !!----------------------------------------------------------------------
      !!
      INTEGER ::   ji, jj   ! dummy loop indices
      INTEGER  ::  inum    ! local integers
      !!----------------------------------------------------------------------

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'dom_grd_hgr : read NetCDF mesh and mask information file(s)'
      IF(lwp) WRITE(numout,*) '~~~~~~~'

      IF(lwp) WRITE(numout,*) '          one file in "mesh_mask.nc" '
      CALL iom_open( 'mesh_hgr', inum )

      !                                                         ! horizontal mesh (inum3)
      CALL iom_get( inum, jpdom_data, 'glamt', glamt )
      CALL iom_get( inum, jpdom_data, 'glamu', glamu )
      CALL iom_get( inum, jpdom_data, 'glamv', glamv )
      CALL iom_get( inum, jpdom_data, 'glamf', glamf )

      CALL iom_get( inum, jpdom_data, 'gphit', gphit )
      CALL iom_get( inum, jpdom_data, 'gphiu', gphiu )
      CALL iom_get( inum, jpdom_data, 'gphiv', gphiv )
      CALL iom_get( inum, jpdom_data, 'gphif', gphif )

      CALL iom_get( inum, jpdom_data, 'e1t', e1t )
      CALL iom_get( inum, jpdom_data, 'e1u', e1u )
      CALL iom_get( inum, jpdom_data, 'e1v', e1v )
      
      CALL iom_get( inum, jpdom_data, 'e2t', e2t )
      CALL iom_get( inum, jpdom_data, 'e2u', e2u )
      CALL iom_get( inum, jpdom_data, 'e2v', e2v )

      CALL iom_get( inum, jpdom_data, 'ff', ff )


      ! Control printing : Grid informations (if not restart)
      ! ----------------

      IF(lwp .AND. .NOT.ln_rstart ) THEN
         WRITE(numout,*)
         WRITE(numout,*) '          longitude and e1 scale factors'
         WRITE(numout,*) '          ------------------------------'
         WRITE(numout,9300) ( ji, glamt(ji,1), glamu(ji,1),   &
            glamv(ji,1), glamf(ji,1),   &
            e1t(ji,1), e1u(ji,1),   &
            e1v(ji,1), ji = 1, jpi,10)

         WRITE(numout,*)
         WRITE(numout,*) '          latitude and e2 scale factors'
         WRITE(numout,*) '          -----------------------------'
         WRITE(numout,9300) ( jj, gphit(1,jj), gphiu(1,jj),   &
            &                     gphiv(1,jj), gphif(1,jj),   &
            &                     e2t  (1,jj), e2u  (1,jj),   &
            &                     e2v  (1,jj), jj = 1, jpj, 10 )
      ENDIF

      !                                     ! ============================
      !                                     !        close the files 
      !                                     ! ============================
      CALL iom_close( inum )
      !
9300     FORMAT( 1x, i4, f8.2,1x, f8.2,1x, f8.2,1x, f8.2, 1x,    &
            f19.10, 1x, f19.10, 1x, f19.10 )
   END SUBROUTINE dom_hgr


   SUBROUTINE dom_zgr
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE dom_zgr  ***
      !!                   
      !! ** Purpose :  Read the NetCDF file(s) which contain(s) all the
      !!      ocean horizontal mesh informations and/or set the depth of model levels 
      !!      and the resulting vertical scale factors.
      !!
      !! ** Method  : - reference 1D vertical coordinate (gdep._1d, e3._1d)
      !!              - read/set ocean depth and ocean levels (bathy, mbathy)
      !!              - vertical coordinate (gdep., e3.) depending on the 
      !!                coordinate chosen :
      !!                   ln_zco=T   z-coordinate  
      !!                   ln_zps=T   z-coordinate with partial steps
      !!                   ln_zco=T   s-coordinate 
      !!
      !! ** Action  :   define gdep., e3., mbathy and bathy
      !!----------------------------------------------------------------------
      INTEGER  ::  ioptio = 0   ! temporary integer
      INTEGER  ::  inum, ios
      INTEGER  ::  ji, jj, jk, ik
      REAL(wp) ::  zrefdep
      !!
      NAMELIST/namzgr/ ln_zco, ln_zps, ln_sco, ln_isfcav
      REAL(wp), POINTER, DIMENSION(:,:) :: zprt, zprw
      !!----------------------------------------------------------------------

      REWIND( numnam_ref )              ! Namelist namzgr in reference namelist : Vertical coordinate
      READ  ( numnam_ref, namzgr, IOSTAT = ios, ERR = 901 )
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namzgr in reference namelist', lwp )

      REWIND( numnam_cfg )              ! Namelist namzgr in configuration namelist : Vertical coordinate
      READ  ( numnam_cfg, namzgr, IOSTAT = ios, ERR = 902 )
902   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namzgr in configuration namelist', lwp )
      IF(lwm) WRITE ( numond, namzgr )

      IF(lwp) THEN                     ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'dom_zgr : vertical coordinate'
         WRITE(numout,*) '~~~~~~~'
         WRITE(numout,*) '          Namelist namzgr : set vertical coordinate'
         WRITE(numout,*) '             z-coordinate - full steps      ln_zco    = ', ln_zco
         WRITE(numout,*) '             z-coordinate - partial steps   ln_zps    = ', ln_zps
         WRITE(numout,*) '             s- or hybrid z-s-coordinate    ln_sco    = ', ln_sco
         WRITE(numout,*) '             ice shelf cavity               ln_isfcav = ', ln_isfcav
      ENDIF

      ioptio = 0                       ! Check Vertical coordinate options
      IF( ln_zco ) ioptio = ioptio + 1
      IF( ln_zps ) ioptio = ioptio + 1
      IF( ln_sco ) ioptio = ioptio + 1
      IF( ln_isfcav ) ioptio = 33
      IF ( ioptio /= 1  )   CALL ctl_stop( ' none or several vertical coordinate options used' )
      IF ( ioptio == 33 )   CALL ctl_stop( ' isf cavity with off line module not yet done    ' )

      IF(lwp) WRITE(numout,*) '          one file in "mesh_mask.nc" '
      CALL iom_open( 'mesh_zgr', inum )

      CALL iom_get( inum, jpdom_unknown, 'gdept_1d', gdept_1d ) ! depth
      CALL iom_get( inum, jpdom_unknown, 'gdepw_1d', gdepw_1d )
      IF( ln_zco .OR. ln_zps ) THEN
         CALL iom_get( inum, jpdom_unknown, 'e3t_1d'  , e3t_1d   )    ! reference scale factors
         CALL iom_get( inum, jpdom_unknown, 'e3w_1d'  , e3w_1d   )
      ENDIF

!!gm BUG in s-coordinate this does not work!
      ! deepest/shallowest W level Above/Below ~10m
      zrefdep = 10._wp - ( 0.1_wp * MINVAL(e3w_1d) )                 ! ref. depth with tolerance (10% of minimum layer thickness)
      nlb10 = MINLOC( gdepw_1d, mask = gdepw_1d > zrefdep, dim = 1 ) ! shallowest W level Below ~10m
      nla10 = nlb10 - 1                                              ! deepest    W level Above ~10m
!!gm end bug

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) '              Reference z-coordinate depth and scale factors:'
         WRITE(numout, "(9x,' level   gdept    gdepw     e3t      e3w  ')" )
         WRITE(numout, "(10x, i4, 4f9.2)" ) ( jk, gdept_1d(jk), gdepw_1d(jk), e3t_1d(jk), e3w_1d(jk), jk = 1, jpk )
      ENDIF

      DO jk = 1, jpk
         IF( e3w_1d  (jk) <= 0._wp .OR. e3t_1d  (jk) <= 0._wp )   CALL ctl_stop( ' e3w_1d or e3t_1d =< 0 ' )
         IF( gdepw_1d(jk) <  0._wp .OR. gdept_1d(jk) <  0._wp )   CALL ctl_stop( ' gdepw_1d or gdept_1d < 0 ' )
      END DO

      IF( lk_vvl ) THEN
          CALL iom_get( inum, jpdom_data, 'e3t_0', e3t_0(:,:,:) )
          CALL iom_get( inum, jpdom_data, 'e3u_0', e3u_0(:,:,:) )
          CALL iom_get( inum, jpdom_data, 'e3v_0', e3v_0(:,:,:) )
          CALL iom_get( inum, jpdom_data, 'e3w_0', e3w_0(:,:,:) )
          CALL iom_get( inum, jpdom_data, 'gdept_0', gdept_0(:,:,:) )
          CALL iom_get( inum, jpdom_data, 'gdepw_0', gdepw_0(:,:,:) )
          ht_0(:,:) = 0.0_wp                       ! Reference ocean depth at  T-points
          DO jk = 1, jpk
             ht_0(:,:) = ht_0(:,:) + e3t_0(:,:,jk) * tmask(:,:,jk)
          END DO
      ELSE
         IF( ln_sco ) THEN                                         ! s-coordinate
            CALL iom_get( inum, jpdom_data, 'hbatt', hbatt )
            CALL iom_get( inum, jpdom_data, 'hbatu', hbatu )
            CALL iom_get( inum, jpdom_data, 'hbatv', hbatv )
            CALL iom_get( inum, jpdom_data, 'hbatf', hbatf )
            
            CALL iom_get( inum, jpdom_unknown, 'gsigt', gsigt ) ! scaling coef.
            CALL iom_get( inum, jpdom_unknown, 'gsigw', gsigw )
            CALL iom_get( inum, jpdom_unknown, 'gsi3w', gsi3w ) 
            CALL iom_get( inum, jpdom_unknown, 'esigt', esigt )
            CALL iom_get( inum, jpdom_unknown, 'esigw', esigw )

            CALL iom_get( inum, jpdom_data, 'e3t_0', fse3t_n(:,:,:) ) ! scale factors
            CALL iom_get( inum, jpdom_data, 'e3u_0', fse3u_n(:,:,:) )
            CALL iom_get( inum, jpdom_data, 'e3v_0', fse3v_n(:,:,:) )
            CALL iom_get( inum, jpdom_data, 'e3w_0', fse3w_n(:,:,:) )
         ENDIF

 
         IF( ln_zps ) THEN                                           ! z-coordinate - partial steps
            !
            IF( iom_varid( inum, 'e3t_0', ldstop = .FALSE. ) > 0 ) THEN
               CALL iom_get( inum, jpdom_data, 'e3t_0', fse3t_n(:,:,:) )
               CALL iom_get( inum, jpdom_data, 'e3u_0', fse3u_n(:,:,:) )
               CALL iom_get( inum, jpdom_data, 'e3v_0', fse3v_n(:,:,:) )
               CALL iom_get( inum, jpdom_data, 'e3w_0', fse3w_n(:,:,:) )
            ELSE                                                        ! 2D bottom scale factors
               CALL iom_get( inum, jpdom_data, 'e3t_ps', e3tp )
               CALL iom_get( inum, jpdom_data, 'e3w_ps', e3wp )
               !                                                        ! deduces the 3D scale factors
               DO jk = 1, jpk
                  fse3t_n(:,:,jk) = e3t_1d(jk)                                    ! set to the ref. factors
                  fse3u_n(:,:,jk) = e3t_1d(jk)
                  fse3v_n(:,:,jk) = e3t_1d(jk)
                  fse3w_n(:,:,jk) = e3w_1d(jk)
               END DO
               DO jj = 1,jpj                                                  ! adjust the deepest values
                  DO ji = 1,jpi
                     ik = mbkt(ji,jj)
                     fse3t_n(ji,jj,ik) = e3tp(ji,jj) * tmask(ji,jj,1) + e3t_1d(1) * ( 1._wp - tmask(ji,jj,1) )
                     fse3w_n(ji,jj,ik) = e3wp(ji,jj) * tmask(ji,jj,1) + e3w_1d(1) * ( 1._wp - tmask(ji,jj,1) )
                  END DO
               END DO
               DO jk = 1,jpk                         ! Computed as the minimum of neighbooring scale factors
                  DO jj = 1, jpjm1
                     DO ji = 1, jpim1
                        fse3u_n(ji,jj,jk) = MIN( fse3t_n(ji,jj,jk), fse3t_n(ji+1,jj,jk) )
                        fse3v_n(ji,jj,jk) = MIN( fse3t_n(ji,jj,jk), fse3t_n(ji,jj+1,jk) )
                     END DO
                  END DO
               END DO
               CALL lbc_lnk( fse3u_n(:,:,:) , 'U', 1._wp )   ;   CALL lbc_lnk( fse3uw_n(:,:,:), 'U', 1._wp )   ! lateral boundary conditions
               CALL lbc_lnk( fse3v_n(:,:,:) , 'V', 1._wp )   ;   CALL lbc_lnk( fse3vw_n(:,:,:), 'V', 1._wp )
               !
               DO jk = 1, jpk                        ! set to z-scale factor if zero (i.e. along closed boundaries)
                  WHERE( fse3u_n(:,:,jk) == 0._wp )   fse3u_n(:,:,jk) = e3t_1d(jk)
                  WHERE( fse3v_n(:,:,jk) == 0._wp )   fse3v_n(:,:,jk) = e3t_1d(jk)
               END DO
            END IF

            IF( iom_varid( inum, 'gdept_0', ldstop = .FALSE. ) > 0 ) THEN   ! 3D depth of t- and w-level
               CALL iom_get( inum, jpdom_data, 'gdept_0', fsdept_n(:,:,:) )
               CALL iom_get( inum, jpdom_data, 'gdepw_0', fsdepw_n(:,:,:) )
            ELSE                                                           ! 2D bottom depth
               CALL wrk_alloc( jpi, jpj, zprt, zprw )
               !
               CALL iom_get( inum, jpdom_data, 'hdept', zprt )
               CALL iom_get( inum, jpdom_data, 'hdepw', zprw )
               !
               DO jk = 1, jpk                                              ! deduces the 3D depth
                  fsdept_n(:,:,jk) = gdept_1d(jk)
                  fsdepw_n(:,:,jk) = gdepw_1d(jk)
               END DO
               DO jj = 1, jpj
                  DO ji = 1, jpi
                     ik = mbkt(ji,jj)
                     IF( ik > 0 ) THEN
                        fsdepw_n(ji,jj,ik+1) = zprw(ji,jj)
                        fsdept_n(ji,jj,ik  ) = zprt(ji,jj)
                        fsdept_n(ji,jj,ik+1) = fsdept_n(ji,jj,ik) + fse3t_n(ji,jj,ik)
                     ENDIF
                  END DO
               END DO
               CALL wrk_dealloc( jpi, jpj, zprt, zprw )
            ENDIF
            !
         ENDIF

         IF( ln_zco ) THEN           ! Vertical coordinates and scales factors
            DO jk = 1, jpk
               fse3t_n(:,:,jk) = e3t_1d(jk)                              ! set to the ref. factors
               fse3u_n(:,:,jk) = e3t_1d(jk)
               fse3v_n(:,:,jk) = e3t_1d(jk)
               fse3w_n(:,:,jk) = e3w_1d(jk)
               fsdept_n(:,:,jk) = gdept_1d(jk)
               fsdepw_n(:,:,jk) = gdepw_1d(jk)
            END DO
         ENDIF
         !
      ENDIF
      !                                     ! ============================
      !                                     !        close the files 
      !                                     ! ============================
      CALL iom_close( inum )
      !
      !
   END SUBROUTINE dom_zgr

   SUBROUTINE dom_ctl
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE dom_ctl  ***
      !!
      !! ** Purpose :   Domain control.
      !!
      !! ** Method  :   compute and print extrema of masked scale factors
      !!
      !! History :
      !!   8.5  !  02-08  (G. Madec)    Original code
      !!----------------------------------------------------------------------
      !! * Local declarations
      INTEGER ::   iimi1, ijmi1, iimi2, ijmi2, iima1, ijma1, iima2, ijma2
      INTEGER, DIMENSION(2) ::   iloc      ! 
      REAL(wp) ::   ze1min, ze1max, ze2min, ze2max
      !!----------------------------------------------------------------------

      ! Extrema of the scale factors

      IF(lwp)WRITE(numout,*)
      IF(lwp)WRITE(numout,*) 'dom_ctl : extrema of the masked scale factors'
      IF(lwp)WRITE(numout,*) '~~~~~~~'

      IF (lk_mpp) THEN
         CALL mpp_minloc( e1t(:,:), tmask(:,:,1), ze1min, iimi1,ijmi1 )
         CALL mpp_minloc( e2t(:,:), tmask(:,:,1), ze2min, iimi2,ijmi2 )
         CALL mpp_maxloc( e1t(:,:), tmask(:,:,1), ze1max, iima1,ijma1 )
         CALL mpp_maxloc( e2t(:,:), tmask(:,:,1), ze2max, iima2,ijma2 )
      ELSE
         ze1min = MINVAL( e1t(:,:), mask = tmask(:,:,1) == 1.e0 )    
         ze2min = MINVAL( e2t(:,:), mask = tmask(:,:,1) == 1.e0 )    
         ze1max = MAXVAL( e1t(:,:), mask = tmask(:,:,1) == 1.e0 )    
         ze2max = MAXVAL( e2t(:,:), mask = tmask(:,:,1) == 1.e0 )    

         iloc  = MINLOC( e1t(:,:), mask = tmask(:,:,1) == 1.e0 )
         iimi1 = iloc(1) + nimpp - 1
         ijmi1 = iloc(2) + njmpp - 1
         iloc  = MINLOC( e2t(:,:), mask = tmask(:,:,1) == 1.e0 )
         iimi2 = iloc(1) + nimpp - 1
         ijmi2 = iloc(2) + njmpp - 1
         iloc  = MAXLOC( e1t(:,:), mask = tmask(:,:,1) == 1.e0 )
         iima1 = iloc(1) + nimpp - 1
         ijma1 = iloc(2) + njmpp - 1
         iloc  = MAXLOC( e2t(:,:), mask = tmask(:,:,1) == 1.e0 )
         iima2 = iloc(1) + nimpp - 1
         ijma2 = iloc(2) + njmpp - 1
      ENDIF

      IF(lwp) THEN
         WRITE(numout,"(14x,'e1t maxi: ',1f10.2,' at i = ',i5,' j= ',i5)") ze1max, iima1, ijma1
         WRITE(numout,"(14x,'e1t mini: ',1f10.2,' at i = ',i5,' j= ',i5)") ze1min, iimi1, ijmi1
         WRITE(numout,"(14x,'e2t maxi: ',1f10.2,' at i = ',i5,' j= ',i5)") ze2max, iima2, ijma2
         WRITE(numout,"(14x,'e2t mini: ',1f10.2,' at i = ',i5,' j= ',i5)") ze2min, iimi2, ijmi2
      ENDIF

   END SUBROUTINE dom_ctl

   !!======================================================================
END MODULE domrea

