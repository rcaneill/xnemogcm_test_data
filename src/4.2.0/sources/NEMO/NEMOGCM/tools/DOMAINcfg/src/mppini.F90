MODULE mppini
   !!======================================================================
   !!                       ***  MODULE mppini   ***
   !! Ocean initialization : distributed memory computing initialization
   !!======================================================================
   !! History :  6.0  !  1994-11  (M. Guyon)  Original code
   !!  OPA       7.0  !  1995-04  (J. Escobar, M. Imbard)
   !!            8.0  !  1998-05  (M. Imbard, J. Escobar, L. Colombet )  SHMEM and MPI versions
   !!  NEMO      1.0  !  2004-01  (G. Madec, J.M Molines)  F90 : free form , north fold jpni > 1
   !!            3.4  !  2011-10  (A. C. Coward, NOCS & J. Donners, PRACE)  add init_nfdcom
   !!            3.   !  2013-06  (I. Epicoco, S. Mocavero, CMCC)  init_nfdcom: setup avoiding MPI communication 
   !!            4.0  !  2016-06  (G. Madec)  use domain configuration file instead of bathymetry file
   !!            4.0  !  2017-06  (J.M. Molines, T. Lovato) merge of mppini and mppini_2
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!  mpp_init       : Lay out the global domain over processors with/without land processor elimination
   !!      init_ioipsl: IOIPSL initialization in mpp 
   !!      init_nfdcom: Setup for north fold exchanges with explicit point-to-point messaging
   !!      init_doloop: set the starting/ending indices of DO-loop used in do_loop_substitute 
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space and time domain
   ! USE bdy_oce        ! open BounDarY  
   !
   USE lbcnfd  , ONLY : isendto, nsndto ! Setup of north fold exchanges 
   USE lib_mpp        ! distribued memory computing library
   USE iom            ! nemo I/O library 
   USE ioipsl         ! I/O IPSL library
   USE in_out_manager ! I/O Manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   mpp_init       ! called by nemogcm.F90
   PUBLIC   mpp_getnum     ! called by prtctl
   PUBLIC   mpp_basesplit  ! called by prtctl
   PUBLIC   mpp_is_ocean   ! called by prtctl
   
   INTEGER ::   numbot = -1   ! 'bottom_level' local logical unit
   INTEGER ::   numbdy = -1   ! 'bdy_msk'      local logical unit
   
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: mppini.F90 13305 2020-07-14 17:12:25Z acc $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

#if ! defined key_mpp_mpi
   !!----------------------------------------------------------------------
   !!   Default option :                            shared memory computing
   !!----------------------------------------------------------------------

   SUBROUTINE mpp_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE mpp_init  ***
      !!
      !! ** Purpose :   Lay out the global domain over processors.
      !!
      !! ** Method  :   Shared memory computing, set the local processor
      !!              variables to the value of the global domain
      !!----------------------------------------------------------------------
      !
      jpiglo = Ni0glo
      jpjglo = Nj0glo
      jpimax = jpiglo
      jpjmax = jpjglo
      jpi    = jpiglo
      jpj    = jpjglo
      jpk    = jpkglo
      jpim1  = jpi-1                         ! inner domain indices
      jpjm1  = jpj-1                         !   "           "
      jpkm1  = MAX( 1, jpk-1 )               !   "           "
      !
      CALL init_doloop                       ! set start/end indices or do-loop depending on the halo width value (nn_hls) 
      !
      jpij   = jpi*jpj
      jpni   = 1
      jpnj   = 1
      jpnij  = jpni*jpnj
      nn_hls = 1
      nimpp  = 1
      njmpp  = 1
      nbondi = 2
      nbondj = 2
      nidom  = FLIO_DOM_NONE
      npolj = 0
      IF( jperio == 3 .OR. jperio == 4 )   npolj = 3
      IF( jperio == 5 .OR. jperio == 6 )   npolj = 5
      l_Iperio = jpni == 1 .AND. (jperio == 1 .OR. jperio == 4 .OR. jperio == 6 .OR. jperio == 7)
      l_Jperio = jpnj == 1 .AND. (jperio == 2 .OR. jperio == 7)
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'mpp_init : NO massively parallel processing'
         WRITE(numout,*) '~~~~~~~~ '
         WRITE(numout,*) '   l_Iperio = ', l_Iperio, '    l_Jperio = ', l_Jperio 
         WRITE(numout,*) '     npolj  = ',   npolj , '      njmpp  = ', njmpp
      ENDIF
      !
      IF(  jpni /= 1 .OR. jpnj /= 1 .OR. jpnij /= 1 )                                     &
         CALL ctl_stop( 'mpp_init: equality  jpni = jpnj = jpnij = 1 is not satisfied',   &
            &           'the domain is lay out for distributed memory computing!' )
         !
#if defined key_agrif
      CALL agrif_nemo_init()

      IF( .NOT. Agrif_Root() ) THEN       ! AGRIF children: specific setting (cf. agrif_user.F90)
         print *,'nbcellsx = ',nbcellsx,nbghostcells_x_w,nbghostcells_x_e
         print *,'nbcellsy = ',nbcellsy,nbghostcells_y_s,nbghostcells_y_n
         IF( Ni0glo /= nbcellsx + nbghostcells_x_w + nbghostcells_x_e ) THEN
            IF(lwp) THEN
               WRITE(numout,*)
               WRITE(numout,*) 'Ni0glo should be: ', nbcellsx + nbghostcells_x_w  + nbghostcells_x_e
            ENDIF        
            CALL ctl_stop( 'STOP', &
                'mpp_init: Agrif children requires Ni0glo == nbcellsx + nbghostcells_x_w + nbghostcells_x_e' )
         ENDIF   
         IF( Nj0glo /= nbcellsy + nbghostcells_y_s + nbghostcells_y_n ) THEN
            IF(lwp) THEN
               WRITE(numout,*)
               WRITE(numout,*) 'Nj0glo shoud be: ', nbcellsy + nbghostcells_y_s + nbghostcells_y_n
            ENDIF        
            CALL ctl_stop( 'STOP', &
                'mpp_init: Agrif children requires Nj0glo == nbcellsy + nbghostcells_y_s + nbghostcells_y_n' )
         ENDIF   
         IF( ln_use_jattr )   CALL ctl_stop( 'STOP', 'mpp_init:Agrif children requires ln_use_jattr = .false. ' )
    ENDIF
#endif
   END SUBROUTINE mpp_init

#else
   !!----------------------------------------------------------------------
   !!   'key_mpp_mpi'                     MPI massively parallel processing
   !!----------------------------------------------------------------------


   SUBROUTINE mpp_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE mpp_init  ***
      !!                    
      !! ** Purpose :   Lay out the global domain over processors.
      !!      If land processors are to be eliminated, this program requires the
      !!      presence of the domain configuration file. Land processors elimination
      !!      is performed if jpni x jpnj /= jpnij. In this case, using the MPP_PREP
      !!      preprocessing tool, help for defining the best cutting out.
      !!
      !! ** Method  :   Global domain is distributed in smaller local domains.
      !!      Periodic condition is a function of the local domain position
      !!      (global boundary or neighbouring domain) and of the global
      !!      periodic
      !!      Type :         jperio global periodic condition
      !!
      !! ** Action : - set domain parameters
      !!                    nimpp     : longitudinal index 
      !!                    njmpp     : latitudinal  index
      !!                    narea     : number for local area
      !!                    nbondi    : mark for "east-west local boundary"
      !!                    nbondj    : mark for "north-south local boundary"
      !!                    nproc     : number for local processor
      !!                    noea      : number for local neighboring processor
      !!                    nowe      : number for local neighboring processor
      !!                    noso      : number for local neighboring processor
      !!                    nono      : number for local neighboring processor
      !!----------------------------------------------------------------------
      INTEGER ::   ji, jj, jn, jproc, jarea   ! dummy loop indices
      INTEGER ::   inijmin
      INTEGER ::   inum                       ! local logical unit
      INTEGER ::   idir, ifreq                ! local integers
      INTEGER ::   ii, il1, ili, imil         !   -       -
      INTEGER ::   ij, il2, ilj, ijm1         !   -       -
      INTEGER ::   iino, ijno, iiso, ijso     !   -       -
      INTEGER ::   iiea, ijea, iiwe, ijwe     !   -       -
      INTEGER ::   iarea0                     !   -       -
      INTEGER ::   ierr, ios                  ! 
      INTEGER ::   inbi, inbj, iimax,  ijmax, icnt1, icnt2
      LOGICAL ::   llbest, llauto
      LOGICAL ::   llwrtlay
      LOGICAL ::   ln_listonly
      INTEGER, ALLOCATABLE, DIMENSION(:)     ::   iin, ii_nono, ii_noea          ! 1D workspace
      INTEGER, ALLOCATABLE, DIMENSION(:)     ::   ijn, ii_noso, ii_nowe          !  -     -
      INTEGER, ALLOCATABLE, DIMENSION(:,:) ::   iimppt, ijpi, ibondi, ipproc   ! 2D workspace
      INTEGER, ALLOCATABLE, DIMENSION(:,:) ::   ijmppt, ijpj, ibondj, ipolj    !  -     -
      INTEGER, ALLOCATABLE, DIMENSION(:,:) ::   iie0, iis0, iono, ioea         !  -     -
      INTEGER, ALLOCATABLE, DIMENSION(:,:) ::   ije0, ijs0, ioso, iowe         !  -     -
      LOGICAL, ALLOCATABLE, DIMENSION(:,:) ::   llisoce                        !  -     -
!      NAMELIST/nambdy/ ln_bdy, nb_bdy, ln_coords_file, cn_coords_file,           &
!           &             ln_mask_file, cn_mask_file, cn_dyn2d, nn_dyn2d_dta,     &
!           &             cn_dyn3d, nn_dyn3d_dta, cn_tra, nn_tra_dta,             &  
!           &             ln_tra_dmp, ln_dyn3d_dmp, rn_time_dmp, rn_time_dmp_out, &
!           &             cn_ice, nn_ice_dta,                                     &
!           &             ln_vol, nn_volctl, nn_rimwidth
      NAMELIST/nammpp/ jpni, jpnj, nn_hls, ln_nnogather, ln_listonly
      !!----------------------------------------------------------------------
      !
      llwrtlay = lwm .OR. sn_cfctl%l_layout
      !
      !  0. read namelists parameters
      ! -----------------------------------
      !
      READ  ( numnam_ref, nammpp, IOSTAT = ios, ERR = 901 )
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'nammpp in reference namelist' )
      READ  ( numnam_cfg, nammpp, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'nammpp in configuration namelist' )   
      !
      nn_hls = MAX(1, nn_hls)   ! nn_hls must be > 0
      IF(lwp) THEN
            WRITE(numout,*) '   Namelist nammpp'
         IF( jpni < 1 .OR. jpnj < 1  ) THEN
            WRITE(numout,*) '      jpni and jpnj will be calculated automatically'
         ELSE
            WRITE(numout,*) '      processor grid extent in i                            jpni = ', jpni
            WRITE(numout,*) '      processor grid extent in j                            jpnj = ', jpnj
         ENDIF
            WRITE(numout,*) '      avoid use of mpi_allgather at the north fold  ln_nnogather = ', ln_nnogather
            WRITE(numout,*) '      halo width (applies to both rows and columns)       nn_hls = ', nn_hls
      ENDIF
      !
      IF(lwm)   WRITE( numond, nammpp )
      !
!!!------------------------------------
!!!  nn_hls shloud be read in nammpp
!!!------------------------------------
      jpiglo = Ni0glo + 2 * nn_hls
      jpjglo = Nj0glo + 2 * nn_hls
      !
      ! do we need to take into account bdy_msk?
!      READ  ( numnam_ref, nambdy, IOSTAT = ios, ERR = 903)
!903   IF( ios /= 0 )   CALL ctl_nam ( ios , 'nambdy in reference namelist (mppini)' )
!      READ  ( numnam_cfg, nambdy, IOSTAT = ios, ERR = 904 )
!904   IF( ios >  0 )   CALL ctl_nam ( ios , 'nambdy in configuration namelist (mppini)' )
      !
      IF(               ln_read_cfg ) CALL iom_open( cn_domcfg,    numbot )
!      IF( ln_bdy .AND. ln_mask_file ) CALL iom_open( cn_mask_file, numbdy )
      !
      IF( ln_listonly )   CALL bestpartition( MAX(mppsize,jpni*jpnj), ldlist = .TRUE. )   ! must be done by all core
      !
      !  1. Dimension arrays for subdomains
      ! -----------------------------------
      !
      ! If dimensions of processors grid weren't specified in the namelist file
      ! then we calculate them here now that we have our communicator size
      IF(lwp) THEN
         WRITE(numout,*) 'mpp_init:'
         WRITE(numout,*) '~~~~~~~~ '
         WRITE(numout,*)
      ENDIF
      IF( jpni < 1 .OR. jpnj < 1 ) THEN
         CALL bestpartition( mppsize, jpni, jpnj )           ! best mpi decomposition for mppsize mpi processes
         llauto = .TRUE.
         llbest = .TRUE.
      ELSE
         llauto = .FALSE.
         CALL bestpartition( mppsize, inbi, inbj, icnt2 )    ! best mpi decomposition for mppsize mpi processes
         ! largest subdomain size for mpi decoposition jpni*jpnj given in the namelist
         CALL mpp_basesplit( jpiglo, jpjglo, nn_hls, jpni, jpnj, jpimax, jpjmax )
         ! largest subdomain size for mpi decoposition inbi*inbj given by bestpartition
         CALL mpp_basesplit( jpiglo, jpjglo, nn_hls, inbi, inbj,  iimax,  ijmax )
         icnt1 = jpni*jpnj - mppsize   ! number of land subdomains that should be removed to use mppsize mpi processes
         IF(lwp) THEN
            WRITE(numout,9000) '   The chosen domain decomposition ', jpni, ' x ', jpnj, ' with ', icnt1, ' land subdomains'
            WRITE(numout,9002) '      - uses a total of ',  mppsize,' mpi process'
            WRITE(numout,9000) '      - has mpi subdomains with a maximum size of (jpi = ', jpimax, ', jpj = ', jpjmax,   &
               &                                                                ', jpi*jpj = ', jpimax*jpjmax, ')'
            WRITE(numout,9000) '   The best domain decompostion ', inbi, ' x ', inbj, ' with ', icnt2, ' land subdomains'
            WRITE(numout,9002) '      - uses a total of ',  inbi*inbj-icnt2,' mpi process'
            WRITE(numout,9000) '      - has mpi subdomains with a maximum size of (jpi = ',  iimax, ', jpj = ',  ijmax,   &
               &                                                             ', jpi*jpj = ',  iimax* ijmax, ')'
         ENDIF
         IF( iimax*ijmax < jpimax*jpjmax ) THEN   ! chosen subdomain size is larger that the best subdomain size
            llbest = .FALSE.
            IF ( inbi*inbj-icnt2 < mppsize ) THEN
               WRITE(ctmp1,*) '   ==> You could therefore have smaller mpi subdomains with less mpi processes'
            ELSE
               WRITE(ctmp1,*) '   ==> You could therefore have smaller mpi subdomains with the same number of mpi processes'
            ENDIF
            CALL ctl_warn( ' ', ctmp1, ' ', '    ---   YOU ARE WASTING CPU...   ---', ' ' )
         ELSE IF ( iimax*ijmax == jpimax*jpjmax .AND. (inbi*inbj-icnt2) <  mppsize) THEN
            llbest = .FALSE.
            WRITE(ctmp1,*) '   ==> You could therefore have the same mpi subdomains size with less mpi processes'
            CALL ctl_warn( ' ', ctmp1, ' ', '    ---   YOU ARE WASTING CPU...   ---', ' ' )
         ELSE
            llbest = .TRUE.
         ENDIF
      ENDIF
      
      ! look for land mpi subdomains...
      ALLOCATE( llisoce(jpni,jpnj) )
      CALL mpp_is_ocean( llisoce )
      inijmin = COUNT( llisoce )   ! number of oce subdomains

      IF( mppsize < inijmin ) THEN   ! too many oce subdomains: can happen only if jpni and jpnj are prescribed...
         WRITE(ctmp1,9001) '   With this specified domain decomposition: jpni = ', jpni, ' jpnj = ', jpnj
         WRITE(ctmp2,9002) '   we can eliminate only ', jpni*jpnj - inijmin, ' land mpi subdomains therefore '
         WRITE(ctmp3,9001) '   the number of ocean mpi subdomains (', inijmin,') exceed the number of MPI processes:', mppsize
         WRITE(ctmp4,*) '   ==>>> There is the list of best domain decompositions you should use: '
         CALL ctl_stop( ctmp1, ctmp2, ctmp3, ' ', ctmp4, ' ' )
         CALL bestpartition( mppsize, ldlist = .TRUE. )   ! must be done by all core
      ENDIF

      IF( mppsize > jpni*jpnj ) THEN   ! not enough mpi subdomains for the total number of mpi processes
         IF(lwp) THEN
            WRITE(numout,9003) '   The number of mpi processes: ', mppsize
            WRITE(numout,9003) '   exceeds the maximum number of subdomains (ocean+land) = ', jpni*jpnj
            WRITE(numout,9001) '   defined by the following domain decomposition: jpni = ', jpni, ' jpnj = ', jpnj
            WRITE(numout,   *) '   You should: '
           IF( llauto ) THEN
               WRITE(numout,*) '     - either prescribe your domain decomposition with the namelist variables'
               WRITE(numout,*) '       jpni and jpnj to match the number of mpi process you want to use, '
               WRITE(numout,*) '       even IF it not the best choice...'
               WRITE(numout,*) '     - or keep the automatic and optimal domain decomposition by picking up one'
               WRITE(numout,*) '       of the number of mpi process proposed in the list bellow'
            ELSE
               WRITE(numout,*) '     - either properly prescribe your domain decomposition with jpni and jpnj'
               WRITE(numout,*) '       in order to be consistent with the number of mpi process you want to use'
               WRITE(numout,*) '       even IF it not the best choice...'
               WRITE(numout,*) '     - or use the automatic and optimal domain decomposition and pick up one of'
               WRITE(numout,*) '       the domain decomposition proposed in the list bellow'
            ENDIF
            WRITE(numout,*)
         ENDIF
         CALL bestpartition( mppsize, ldlist = .TRUE. )   ! must be done by all core
      ENDIF

      jpnij = mppsize   ! force jpnij definition <-- remove as much land subdomains as needed to reach this condition
      IF( mppsize > inijmin ) THEN
         WRITE(ctmp1,9003) '   The number of mpi processes: ', mppsize
         WRITE(ctmp2,9003) '   exceeds the maximum number of ocean subdomains = ', inijmin
         WRITE(ctmp3,9002) '   we suppressed ', jpni*jpnj - mppsize, ' land subdomains '
         WRITE(ctmp4,9002) '   BUT we had to keep ', mppsize - inijmin, ' land subdomains that are useless...'
         CALL ctl_warn( ctmp1, ctmp2, ctmp3, ctmp4, ' ', '    --- YOU ARE WASTING CPU... ---', ' ' )
      ELSE   ! mppsize = inijmin
         IF(lwp) THEN
            IF(llbest) WRITE(numout,*) '   ==> you use the best mpi decomposition'
            WRITE(numout,*)
            WRITE(numout,9003) '   Number of mpi processes: ', mppsize
            WRITE(numout,9003) '   Number of ocean subdomains = ', inijmin
            WRITE(numout,9003) '   Number of suppressed land subdomains = ', jpni*jpnj - inijmin
            WRITE(numout,*)
         ENDIF
      ENDIF
9000  FORMAT (a, i4, a, i4, a, i7, a)
9001  FORMAT (a, i4, a, i4)
9002  FORMAT (a, i4, a)
9003  FORMAT (a, i5)

      ALLOCATE(  nfimpp(jpni ) , nfproc(jpni ) ,   nfjpi(jpni ) ,                     &
         &       nimppt(jpnij) , ibonit(jpnij) ,  jpiall(jpnij) ,  jpjall(jpnij) ,    &
         &       njmppt(jpnij) , ibonjt(jpnij) , nis0all(jpnij) , njs0all(jpnij) ,    &
         &                                       nie0all(jpnij) , nje0all(jpnij) ,    &
         &       iin(jpnij), ii_nono(jpnij), ii_noea(jpnij),   &
         &       ijn(jpnij), ii_noso(jpnij), ii_nowe(jpnij),   &
         &       iimppt(jpni,jpnj), ijpi(jpni,jpnj), ibondi(jpni,jpnj), ipproc(jpni,jpnj),   &
         &       ijmppt(jpni,jpnj), ijpj(jpni,jpnj), ibondj(jpni,jpnj),  ipolj(jpni,jpnj),   &
         &         iie0(jpni,jpnj), iis0(jpni,jpnj),   iono(jpni,jpnj),   ioea(jpni,jpnj),   &
         &         ije0(jpni,jpnj), ijs0(jpni,jpnj),   ioso(jpni,jpnj),   iowe(jpni,jpnj),   &
         &       STAT=ierr )
      CALL mpp_sum( 'mppini', ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'mpp_init: unable to allocate standard ocean arrays' )
      
#if defined key_agrif
      CALL agrif_nemo_init()
      IF( .NOT. Agrif_Root() ) THEN       ! AGRIF children: specific setting (cf. agrif_user.F90)
         IF( Ni0glo /= nbcellsx + nbghostcells_x_w + nbghostcells_x_e ) THEN
            IF(lwp) THEN
               WRITE(numout,*)
               WRITE(numout,*) 'Ni0glo should be: ', nbcellsx + nbghostcells_x_w  + nbghostcells_x_e
            ENDIF        
            CALL ctl_stop( 'STOP', & 
                 'mpp_init: Agrif children requires Ni0glo == nbcellsx + nbghostcells_x_w + nbghostcells_x_e' )
         ENDIF   
         IF( Nj0glo /= nbcellsy  + nbghostcells_y_s + nbghostcells_y_n ) THEN
            IF(lwp) THEN
               WRITE(numout,*)
               WRITE(numout,*) 'Nj0glo shoud be: ', nbcellsy  + nbghostcells_y_s + nbghostcells_y_n
            ENDIF        
            CALL ctl_stop( 'STOP', &
            	'mpp_init: Agrif children requires Nj0glo == nbcellsy  + nbghostcells_y_s + nbghostcells_y_n' )
         ENDIF   
         IF( ln_use_jattr )   CALL ctl_stop( 'STOP', 'mpp_init:Agrif children requires ln_use_jattr = .false. ' )
      ENDIF
#endif
      !
      !  2. Index arrays for subdomains
      ! -----------------------------------
      !

      CALL mpp_basesplit( jpiglo, jpjglo, nn_hls, jpni, jpnj, jpimax, jpjmax, iimppt, ijmppt, ijpi, ijpj )
      CALL mpp_getnum( llisoce, ipproc, iin, ijn )
      !
      !DO jn = 1, jpni
      !   jproc = ipproc(jn,jpnj)
      !   ii = iin(jproc+1)
      !   ij = ijn(jproc+1)
      !   nfproc(jn) = jproc
      !   nfimpp(jn) = iimppt(ii,ij)
      !   nfjpi (jn) =   ijpi(ii,ij)
      !END DO
      nfproc(:) = ipproc(:,jpnj) 
      nfimpp(:) = iimppt(:,jpnj) 
      nfjpi (:) =   ijpi(:,jpnj)
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'MPI Message Passing MPI - domain lay out over processors'
         WRITE(numout,*)
         WRITE(numout,*) '   defines mpp subdomains'
         WRITE(numout,*) '      jpni = ', jpni  
         WRITE(numout,*) '      jpnj = ', jpnj
         WRITE(numout,*) '     jpnij = ', jpnij
         WRITE(numout,*)
         WRITE(numout,*) '      sum ijpi(i,1) = ', sum(ijpi(:,1)), ' jpiglo = ', jpiglo
         WRITE(numout,*) '      sum ijpj(1,j) = ', sum(ijpj(1,:)), ' jpjglo = ', jpjglo
      ENDIF
     
      ! 3. Subdomain description in the Regular Case
      ! --------------------------------------------
      ! specific cases where there is no communication -> must do the periodicity by itself
      ! Warning: because of potential land-area suppression, do not use nbond[ij] == 2  
      l_Iperio = jpni == 1 .AND. (jperio == 1 .OR. jperio == 4 .OR. jperio == 6 .OR. jperio == 7)
      l_Jperio = jpnj == 1 .AND. (jperio == 2 .OR. jperio == 7)
      
      DO jarea = 1, jpni*jpnj
         !
         iarea0 = jarea - 1
         ii = 1 + MOD(iarea0,jpni)
         ij = 1 +     iarea0/jpni
         ili = ijpi(ii,ij)
         ilj = ijpj(ii,ij)
         ibondi(ii,ij) = 0                         ! default: has e-w neighbours
         IF( ii   ==    1 )   ibondi(ii,ij) = -1   ! first column, has only e neighbour
         IF( ii   == jpni )   ibondi(ii,ij) =  1   ! last column,  has only w neighbour
         IF( jpni ==    1 )   ibondi(ii,ij) =  2   ! has no e-w neighbour
         ibondj(ii,ij) = 0                         ! default: has n-s neighbours
         IF( ij   ==    1 )   ibondj(ii,ij) = -1   ! first row, has only n neighbour
         IF( ij   == jpnj )   ibondj(ii,ij) =  1   ! last row,  has only s neighbour
         IF( jpnj ==    1 )   ibondj(ii,ij) =  2   ! has no n-s neighbour

         ! Subdomain neighbors (get their zone number): default definition
         ioso(ii,ij) = iarea0 - jpni
         iowe(ii,ij) = iarea0 - 1
         ioea(ii,ij) = iarea0 + 1
         iono(ii,ij) = iarea0 + jpni
         iis0(ii,ij) =  1  + nn_hls
         iie0(ii,ij) = ili - nn_hls
         ijs0(ii,ij) =  1  + nn_hls
         ije0(ii,ij) = ilj - nn_hls

         ! East-West periodicity: change ibondi, ioea, iowe
         IF( jperio == 1 .OR. jperio == 4 .OR. jperio == 6 .OR. jperio == 7 ) THEN
            IF( jpni  /= 1 )   ibondi(ii,ij) = 0                        ! redefine: all have e-w neighbours
            IF( ii ==    1 )   iowe(ii,ij) = iarea0 +        (jpni-1)   ! redefine: first column, address of w neighbour
            IF( ii == jpni )   ioea(ii,ij) = iarea0 -        (jpni-1)   ! redefine: last column,  address of e neighbour
         ENDIF

         ! Simple North-South periodicity: change ibondj, ioso, iono
         IF( jperio == 2 .OR. jperio == 7 ) THEN
            IF( jpnj  /= 1 )   ibondj(ii,ij) = 0                        ! redefine: all have n-s neighbours
            IF( ij ==    1 )   ioso(ii,ij) = iarea0 + jpni * (jpnj-1)   ! redefine: first row, address of s neighbour
            IF( ij == jpnj )   iono(ii,ij) = iarea0 - jpni * (jpnj-1)   ! redefine: last row,  address of n neighbour
         ENDIF

         ! North fold: define ipolj, change iono. Warning: we do not change ibondj...
         ipolj(ii,ij) = 0
         IF( jperio == 3 .OR. jperio == 4 ) THEN
            ijm1 = jpni*(jpnj-1)
            imil = ijm1+(jpni+1)/2
            IF( jarea > ijm1 ) ipolj(ii,ij) = 3
            IF( MOD(jpni,2) == 1 .AND. jarea == imil ) ipolj(ii,ij) = 4
            IF( ipolj(ii,ij) == 3 ) iono(ii,ij) = jpni*jpnj-jarea+ijm1   ! MPI rank of northern neighbour
         ENDIF
         IF( jperio == 5 .OR. jperio == 6 ) THEN
            ijm1 = jpni*(jpnj-1)
            imil = ijm1+(jpni+1)/2
            IF( jarea > ijm1) ipolj(ii,ij) = 5
            IF( MOD(jpni,2) == 1 .AND. jarea == imil ) ipolj(ii,ij) = 6
            IF( ipolj(ii,ij) == 5) iono(ii,ij) = jpni*jpnj-jarea+ijm1    ! MPI rank of northern neighbour
         ENDIF
         !
      END DO

      ! 4. deal with land subdomains
      ! ----------------------------
      !
      ! neighbour treatment: change ibondi, ibondj if next to a land zone
      DO jarea = 1, jpni*jpnj
         ii = 1 + MOD( jarea-1  , jpni )
         ij = 1 +     (jarea-1) / jpni
         ! land-only area with an active n neigbour
         IF ( ipproc(ii,ij) == -1 .AND. 0 <= iono(ii,ij) .AND. iono(ii,ij) <= jpni*jpnj-1 ) THEN
            iino = 1 + MOD( iono(ii,ij) , jpni )                    ! ii index of this n neigbour
            ijno = 1 +      iono(ii,ij) / jpni                      ! ij index of this n neigbour
            ! In case of north fold exchange: I am the n neigbour of my n neigbour!! (#1057)
            ! --> for northern neighbours of northern row processors (in case of north-fold)
            !     need to reverse the LOGICAL direction of communication 
            idir = 1                                           ! we are indeed the s neigbour of this n neigbour
            IF( ij == jpnj .AND. ijno == jpnj )   idir = -1    ! both are on the last row, we are in fact the n neigbour
            IF( ibondj(iino,ijno) == idir     )   ibondj(iino,ijno) =   2     ! this n neigbour had only a s/n neigbour -> no more
            IF( ibondj(iino,ijno) == 0        )   ibondj(iino,ijno) = -idir   ! this n neigbour had both, n-s neighbours -> keep 1
         ENDIF
         ! land-only area with an active s neigbour
         IF( ipproc(ii,ij) == -1 .AND. 0 <= ioso(ii,ij) .AND. ioso(ii,ij) <= jpni*jpnj-1 ) THEN
            iiso = 1 + MOD( ioso(ii,ij) , jpni )                    ! ii index of this s neigbour
            ijso = 1 +      ioso(ii,ij) / jpni                      ! ij index of this s neigbour
            IF( ibondj(iiso,ijso) == -1 )   ibondj(iiso,ijso) = 2   ! this s neigbour had only a n neigbour    -> no more neigbour
            IF( ibondj(iiso,ijso) ==  0 )   ibondj(iiso,ijso) = 1   ! this s neigbour had both, n-s neighbours -> keep s neigbour
         ENDIF
         ! land-only area with an active e neigbour
         IF( ipproc(ii,ij) == -1 .AND. 0 <= ioea(ii,ij) .AND. ioea(ii,ij) <= jpni*jpnj-1 ) THEN
            iiea = 1 + MOD( ioea(ii,ij) , jpni )                    ! ii index of this e neigbour
            ijea = 1 +      ioea(ii,ij) / jpni                      ! ij index of this e neigbour
            IF( ibondi(iiea,ijea) == 1 )   ibondi(iiea,ijea) =  2   ! this e neigbour had only a w neigbour    -> no more neigbour
            IF( ibondi(iiea,ijea) == 0 )   ibondi(iiea,ijea) = -1   ! this e neigbour had both, e-w neighbours -> keep e neigbour
         ENDIF
         ! land-only area with an active w neigbour
         IF( ipproc(ii,ij) == -1 .AND. 0 <= iowe(ii,ij) .AND. iowe(ii,ij) <= jpni*jpnj-1) THEN
            iiwe = 1 + MOD( iowe(ii,ij) , jpni )                    ! ii index of this w neigbour
            ijwe = 1 +      iowe(ii,ij) / jpni                      ! ij index of this w neigbour
            IF( ibondi(iiwe,ijwe) == -1 )   ibondi(iiwe,ijwe) = 2   ! this w neigbour had only a e neigbour    -> no more neigbour
            IF( ibondi(iiwe,ijwe) ==  0 )   ibondi(iiwe,ijwe) = 1   ! this w neigbour had both, e-w neighbours -> keep w neigbour
         ENDIF
      END DO
      
      ! 5. Subdomain print
      ! ------------------
      IF(lwp) THEN
         ifreq = 4
         il1 = 1
         DO jn = 1, (jpni-1)/ifreq+1
            il2 = MIN(jpni,il1+ifreq-1)
            WRITE(numout,*)
            WRITE(numout,9400) ('***',ji=il1,il2-1)
            DO jj = jpnj, 1, -1
               WRITE(numout,9403) ('   ',ji=il1,il2-1)
               WRITE(numout,9402) jj, (ijpi(ji,jj),ijpj(ji,jj),ji=il1,il2)
               WRITE(numout,9404) (ipproc(ji,jj),ji=il1,il2)
               WRITE(numout,9403) ('   ',ji=il1,il2-1)
               WRITE(numout,9400) ('***',ji=il1,il2-1)
            END DO
            WRITE(numout,9401) (ji,ji=il1,il2)
            il1 = il1+ifreq
         END DO
 9400    FORMAT('           ***'   ,20('*************',a3)    )
 9403    FORMAT('           *     ',20('         *   ',a3)    )
 9401    FORMAT('              '   ,20('   ',i3,'          ') )
 9402    FORMAT('       ',i3,' *  ',20(i3,'  x',i3,'   *   ') )
 9404    FORMAT('           *  '   ,20('     ' ,i4,'   *   ') )
      ENDIF
         
      ! just to save nono etc for all proc
      ! warning ii*ij (zone) /= nproc (processors)!
      ! ioso = zone number, ii_noso = proc number
      ii_noso(:) = -1
      ii_nono(:) = -1
      ii_noea(:) = -1
      ii_nowe(:) = -1 
      DO jproc = 1, jpnij
         ii = iin(jproc)
         ij = ijn(jproc)
         IF( 0 <= ioso(ii,ij) .AND. ioso(ii,ij) <= (jpni*jpnj-1) ) THEN
            iiso = 1 + MOD( ioso(ii,ij) , jpni )
            ijso = 1 +      ioso(ii,ij) / jpni
            ii_noso(jproc) = ipproc(iiso,ijso)
         ENDIF
         IF( 0 <= iowe(ii,ij) .AND. iowe(ii,ij) <= (jpni*jpnj-1) ) THEN
          iiwe = 1 + MOD( iowe(ii,ij) , jpni )
          ijwe = 1 +      iowe(ii,ij) / jpni
          ii_nowe(jproc) = ipproc(iiwe,ijwe)
         ENDIF
         IF( 0 <= ioea(ii,ij) .AND. ioea(ii,ij) <= (jpni*jpnj-1) ) THEN
            iiea = 1 + MOD( ioea(ii,ij) , jpni )
            ijea = 1 +      ioea(ii,ij) / jpni
            ii_noea(jproc)= ipproc(iiea,ijea)
         ENDIF
         IF( 0 <= iono(ii,ij) .AND. iono(ii,ij) <= (jpni*jpnj-1) ) THEN
            iino = 1 + MOD( iono(ii,ij) , jpni )
            ijno = 1 +      iono(ii,ij) / jpni
            ii_nono(jproc)= ipproc(iino,ijno)
         ENDIF
      END DO
    
      ! 6. Change processor name
      ! ------------------------
      ii = iin(narea)
      ij = ijn(narea)
      !
      ! set default neighbours
      noso = ii_noso(narea)
      nowe = ii_nowe(narea)
      noea = ii_noea(narea)
      nono = ii_nono(narea)
      jpi    = ijpi(ii,ij)  
!!$      Nis0  = iis0(ii,ij)
!!$      Nie0  = iie0(ii,ij)
      jpj    = ijpj(ii,ij)  
!!$      Njs0  = ijs0(ii,ij)
!!$      Nje0  = ije0(ii,ij)
      nbondi = ibondi(ii,ij)
      nbondj = ibondj(ii,ij)
      nimpp = iimppt(ii,ij)  
      njmpp = ijmppt(ii,ij)
      jpk = jpkglo                              ! third dim
      !
      CALL init_doloop                          ! set start/end indices of do-loop, depending on the halo width value (nn_hls) 
      !
      jpim1 = jpi-1                             ! inner domain indices
      jpjm1 = jpj-1                             !   "           "
      jpkm1 = MAX( 1, jpk-1 )                   !   "           "
      jpij  = jpi*jpj                           !  jpi x j
      DO jproc = 1, jpnij
         ii = iin(jproc)
         ij = ijn(jproc)
         jpiall (jproc) = ijpi(ii,ij)
         nis0all(jproc) = iis0(ii,ij)
         nie0all(jproc) = iie0(ii,ij)
         jpjall (jproc) = ijpj(ii,ij)
         njs0all(jproc) = ijs0(ii,ij)
         nje0all(jproc) = ije0(ii,ij)
         ibonit(jproc) = ibondi(ii,ij)
         ibonjt(jproc) = ibondj(ii,ij)
         nimppt(jproc) = iimppt(ii,ij)  
         njmppt(jproc) = ijmppt(ii,ij) 
      END DO

      ! Save processor layout in ascii file
      IF (llwrtlay) THEN
         CALL ctl_opn( inum, 'layout.dat', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE., narea )
         WRITE(inum,'(a)') '   jpnij   jpimax  jpjmax    jpk  jpiglo  jpjglo'//&
   &           ' ( local:    narea     jpi     jpj )'
         WRITE(inum,'(6i8,a,3i8,a)') jpnij,jpimax,jpjmax,jpk,jpiglo,jpjglo,&
   &           ' ( local: ',narea,jpi,jpj,' )'
         WRITE(inum,'(a)') 'nproc   jpi  jpj Nis0 Njs0 Nie0 Nje0 nimp njmp nono noso nowe noea nbondi nbondj '

         DO jproc = 1, jpnij
            WRITE(inum,'(13i5,2i7)')   jproc-1,  jpiall(jproc),  jpjall(jproc),   &
               &                                nis0all(jproc), njs0all(jproc),   &
               &                                nie0all(jproc), nje0all(jproc),   &
               &                                nimppt (jproc), njmppt (jproc),   & 
               &                                ii_nono(jproc), ii_noso(jproc),   &
               &                                ii_nowe(jproc), ii_noea(jproc),   &
               &                                ibonit (jproc), ibonjt (jproc) 
         END DO
      END IF

      !                          ! north fold parameter
      ! Defined npolj, either 0, 3 , 4 , 5 , 6
      ! In this case the important thing is that npolj /= 0
      ! Because if we go through these line it is because jpni >1 and thus
      ! we must use lbcnorthmpp, which tests only npolj =0 or npolj /= 0
      npolj = 0
      ij = ijn(narea)
      IF( jperio == 3 .OR. jperio == 4 ) THEN
         IF( ij == jpnj )   npolj = 3
      ENDIF
      IF( jperio == 5 .OR. jperio == 6 ) THEN
         IF( ij == jpnj )   npolj = 5
      ENDIF
      !
      nproc = narea-1
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) '   resulting internal parameters : '
         WRITE(numout,*) '      nproc  = ', nproc
         WRITE(numout,*) '      nowe   = ', nowe  , '   noea  =  ', noea
         WRITE(numout,*) '      nono   = ', nono  , '   noso  =  ', noso
         WRITE(numout,*) '      nbondi = ', nbondi
         WRITE(numout,*) '      nbondj = ', nbondj
         WRITE(numout,*) '      npolj  = ', npolj
         WRITE(numout,*) '    l_Iperio = ', l_Iperio
         WRITE(numout,*) '    l_Jperio = ', l_Jperio
         WRITE(numout,*) '      nimpp  = ', nimpp
         WRITE(numout,*) '      njmpp  = ', njmpp
      ENDIF

      !                          ! Prepare mpp north fold
      IF( jperio >= 3 .AND. jperio <= 6 .AND. jpni > 1 ) THEN
         CALL mpp_ini_north
         IF (lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) '   ==>>>   North fold boundary prepared for jpni >1'
            ! additional prints in layout.dat
         ENDIF
         IF (llwrtlay) THEN
            WRITE(inum,*)
            WRITE(inum,*)
            WRITE(inum,*) 'number of subdomains located along the north fold : ', ndim_rank_north
            WRITE(inum,*) 'Rank of the subdomains located along the north fold : ', ndim_rank_north
            DO jproc = 1, ndim_rank_north, 5
               WRITE(inum,*) nrank_north( jproc:MINVAL( (/jproc+4,ndim_rank_north/) ) )
            END DO
         ENDIF
      ENDIF
      !
      CALL init_ioipsl       ! Prepare NetCDF output file (if necessary)
      !      
      IF (( jperio >= 3 .AND. jperio <= 6 .AND. jpni > 1 ).AND.( ln_nnogather )) THEN
         CALL init_nfdcom     ! northfold neighbour lists
         IF (llwrtlay) THEN
            WRITE(inum,*)
            WRITE(inum,*)
            WRITE(inum,*) 'north fold exchanges with explicit point-to-point messaging :'
            WRITE(inum,*) 'nsndto : ', nsndto
            WRITE(inum,*) 'isendto : ', isendto
         ENDIF
      ENDIF
      !
      IF (llwrtlay) CLOSE(inum)   
      !
      DEALLOCATE(iin, ijn, ii_nono, ii_noea, ii_noso, ii_nowe,    &
         &       iimppt, ijmppt, ibondi, ibondj, ipproc, ipolj,   &
         &       ijpi, ijpj, iie0, ije0, iis0, ijs0,              &
         &       iono, ioea, ioso, iowe, llisoce)
      !
    END SUBROUTINE mpp_init


    SUBROUTINE mpp_basesplit( kiglo, kjglo, khls, knbi, knbj, kimax, kjmax, kimppt, kjmppt, klci, klcj)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE mpp_basesplit  ***
      !!                    
      !! ** Purpose :   Lay out the global domain over processors.
      !!
      !! ** Method  :   Global domain is distributed in smaller local domains.
      !!
      !! ** Action : - set for all knbi*knbj domains:
      !!                    kimppt     : longitudinal index
      !!                    kjmppt     : latitudinal  index
      !!                    klci       : first dimension
      !!                    klcj       : second dimension
      !!----------------------------------------------------------------------
      INTEGER,                                 INTENT(in   ) ::   kiglo, kjglo
      INTEGER,                                 INTENT(in   ) ::   khls
      INTEGER,                                 INTENT(in   ) ::   knbi, knbj
      INTEGER,                                 INTENT(  out) ::   kimax, kjmax
      INTEGER, DIMENSION(knbi,knbj), OPTIONAL, INTENT(  out) ::   kimppt, kjmppt
      INTEGER, DIMENSION(knbi,knbj), OPTIONAL, INTENT(  out) ::   klci, klcj
      !
      INTEGER ::   ji, jj
      INTEGER ::   i2hls 
      INTEGER ::   iresti, irestj, irm, ijpjmin
      !!----------------------------------------------------------------------
      i2hls = 2*khls
      !
#if defined key_nemocice_decomp
      kimax = ( nx_global+2-i2hls + (knbi-1) ) / knbi + i2hls    ! first  dim.
      kjmax = ( ny_global+2-i2hls + (knbj-1) ) / knbj + i2hls    ! second dim. 
#else
      kimax = ( kiglo - i2hls + (knbi-1) ) / knbi + i2hls    ! first  dim.
      kjmax = ( kjglo - i2hls + (knbj-1) ) / knbj + i2hls    ! second dim.
#endif
      IF( .NOT. PRESENT(kimppt) ) RETURN
      !
      !  1. Dimension arrays for subdomains
      ! -----------------------------------
      !  Computation of local domain sizes klci() klcj()
      !  These dimensions depend on global sizes knbi,knbj and kiglo,kjglo
      !  The subdomains are squares lesser than or equal to the global
      !  dimensions divided by the number of processors minus the overlap array.
      !
      iresti = 1 + MOD( kiglo - i2hls - 1 , knbi )
      irestj = 1 + MOD( kjglo - i2hls - 1 , knbj )
      !
      !  Need to use kimax and kjmax here since jpi and jpj not yet defined
#if defined key_nemocice_decomp
      ! Change padding to be consistent with CICE
      klci(1:knbi-1,:       ) = kimax
      klci(  knbi  ,:       ) = kiglo - (knbi - 1) * (kimax - i2hls)
      klcj(:       ,1:knbj-1) = kjmax
      klcj(:       ,  knbj  ) = kjglo - (knbj - 1) * (kjmax - i2hls)
#else
      klci(1:iresti      ,:) = kimax
      klci(iresti+1:knbi ,:) = kimax-1
      IF( MINVAL(klci) < 2*i2hls ) THEN
         WRITE(ctmp1,*) '   mpp_basesplit: minimum value of jpi must be >= ', 2*i2hls
         WRITE(ctmp2,*) '   We have ', MINVAL(klci)
        CALL ctl_stop( 'STOP', ctmp1, ctmp2 )
      ENDIF
      IF( jperio == 3 .OR. jperio == 4 .OR. jperio == 5 .OR. jperio == 6 ) THEN
         ! minimize the size of the last row to compensate for the north pole folding coast
         IF( jperio == 3 .OR. jperio == 4 )   ijpjmin = 2+3*khls   ! V and F folding must be outside of southern halos
         IF( jperio == 5 .OR. jperio == 6 )   ijpjmin = 1+3*khls   ! V and F folding must be outside of southern halos
         irm = knbj - irestj                                       ! total number of lines to be removed
         klcj(:,knbj) = MAX( ijpjmin, kjmax-irm )                  ! we must have jpj >= ijpjmin in the last row
         irm = irm - ( kjmax - klcj(1,knbj) )                      ! remaining number of lines to remove 
         irestj = knbj - 1 - irm
         klcj(:, irestj+1:knbj-1) = kjmax-1
      ELSE
         klcj(:, irestj+1:knbj  ) = kjmax-1
      ENDIF
      klcj(:,1:irestj) = kjmax
      IF( MINVAL(klcj) < 2*i2hls ) THEN
         WRITE(ctmp1,*) '   mpp_basesplit: minimum value of jpj must be >= ', 2*i2hls
         WRITE(ctmp2,*) '   We have ', MINVAL(klcj)
         CALL ctl_stop( 'STOP', ctmp1, ctmp2 )
      ENDIF
#endif

      !  2. Index arrays for subdomains
      ! -------------------------------
      kimppt(:,:) = 1
      kjmppt(:,:) = 1
      !
      IF( knbi > 1 ) THEN
         DO jj = 1, knbj
            DO ji = 2, knbi
               kimppt(ji,jj) = kimppt(ji-1,jj) + klci(ji-1,jj) - i2hls
            END DO
         END DO
      ENDIF
      !
      IF( knbj > 1 )THEN
         DO jj = 2, knbj
            DO ji = 1, knbi
               kjmppt(ji,jj) = kjmppt(ji,jj-1) + klcj(ji,jj-1) - i2hls
            END DO
         END DO
      ENDIF
      
   END SUBROUTINE mpp_basesplit


   SUBROUTINE bestpartition( knbij, knbi, knbj, knbcnt, ldlist )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE bestpartition  ***
      !!
      !! ** Purpose :
      !!
      !! ** Method  :
      !!----------------------------------------------------------------------
      INTEGER,           INTENT(in   ) ::   knbij         ! total number if subdomains               (knbi*knbj)
      INTEGER, OPTIONAL, INTENT(  out) ::   knbi, knbj    ! number if subdomains along i and j (knbi and knbj)
      INTEGER, OPTIONAL, INTENT(  out) ::   knbcnt        ! number of land subdomains
      LOGICAL, OPTIONAL, INTENT(in   ) ::   ldlist        ! .true.: print the list the best domain decompositions (with land)
      !
      INTEGER :: ji, jj, ii, iitarget
      INTEGER :: iszitst, iszjtst
      INTEGER :: isziref, iszjref
      INTEGER :: inbij, iszij
      INTEGER :: inbimax, inbjmax, inbijmax, inbijold
      INTEGER :: isz0, isz1
      INTEGER, DIMENSION(  :), ALLOCATABLE :: indexok
      INTEGER, DIMENSION(  :), ALLOCATABLE :: inbi0, inbj0, inbij0   ! number of subdomains along i,j
      INTEGER, DIMENSION(  :), ALLOCATABLE :: iszi0, iszj0, iszij0   ! max size of the subdomains along i,j
      INTEGER, DIMENSION(  :), ALLOCATABLE :: inbi1, inbj1, inbij1   ! number of subdomains along i,j
      INTEGER, DIMENSION(  :), ALLOCATABLE :: iszi1, iszj1, iszij1   ! max size of the subdomains along i,j
      LOGICAL :: llist
      LOGICAL, DIMENSION(:,:), ALLOCATABLE :: llmsk2d                 ! max size of the subdomains along i,j
      LOGICAL, DIMENSION(:,:), ALLOCATABLE :: llisoce              !  -     -
      REAL(wp)::   zpropland
      !!----------------------------------------------------------------------
      !
      llist = .FALSE.
      IF( PRESENT(ldlist) ) llist = ldlist

      CALL mpp_init_landprop( zpropland )                      ! get the proportion of land point over the gloal domain
      inbij = NINT( REAL(knbij, wp) / ( 1.0 - zpropland ) )    ! define the largest possible value for jpni*jpnj
      !
      IF( llist ) THEN   ;   inbijmax = inbij*2
      ELSE               ;   inbijmax = inbij
      ENDIF
      !
      ALLOCATE(inbi0(inbijmax),inbj0(inbijmax),iszi0(inbijmax),iszj0(inbijmax))
      !
      inbimax = 0
      inbjmax = 0
      isziref = Ni0glo*Nj0glo+1
      iszjref = Ni0glo*Nj0glo+1
      !
      ! get the list of knbi that gives a smaller jpimax than knbi-1
      ! get the list of knbj that gives a smaller jpjmax than knbj-1
      DO ji = 1, inbijmax      
#if defined key_nemocice_decomp
         iszitst = ( nx_global+2-2*nn_hls + (ji-1) ) / ji + 2*nn_hls    ! first  dim.
#else
         iszitst = ( Ni0glo + (ji-1) ) / ji
#endif
         IF( iszitst < isziref ) THEN
            isziref = iszitst
            inbimax = inbimax + 1
            inbi0(inbimax) = ji
            iszi0(inbimax) = isziref
         ENDIF
#if defined key_nemocice_decomp
         iszjtst = ( ny_global+2-2*nn_hls + (ji-1) ) / ji + 2*nn_hls    ! first  dim.
#else
         iszjtst = ( Nj0glo + (ji-1) ) / ji
#endif
         IF( iszjtst < iszjref ) THEN
            iszjref = iszjtst
            inbjmax = inbjmax + 1
            inbj0(inbjmax) = ji
            iszj0(inbjmax) = iszjref
         ENDIF
      END DO

      ! combine these 2 lists to get all possible knbi*knbj <  inbijmax
      ALLOCATE( llmsk2d(inbimax,inbjmax) )
      DO jj = 1, inbjmax
         DO ji = 1, inbimax
            IF ( inbi0(ji) * inbj0(jj) <= inbijmax ) THEN   ;   llmsk2d(ji,jj) = .TRUE.
            ELSE                                            ;   llmsk2d(ji,jj) = .FALSE.
            ENDIF
         END DO
      END DO
      isz1 = COUNT(llmsk2d)
      ALLOCATE( inbi1(isz1), inbj1(isz1), iszi1(isz1), iszj1(isz1) )
      ii = 0
      DO jj = 1, inbjmax
         DO ji = 1, inbimax
            IF( llmsk2d(ji,jj) .EQV. .TRUE. ) THEN
               ii = ii + 1
               inbi1(ii) = inbi0(ji)
               inbj1(ii) = inbj0(jj)
               iszi1(ii) = iszi0(ji)
               iszj1(ii) = iszj0(jj)
            END IF
         END DO
      END DO
      DEALLOCATE( inbi0, inbj0, iszi0, iszj0 )
      DEALLOCATE( llmsk2d )

      ALLOCATE( inbij1(isz1), iszij1(isz1) )
      inbij1(:) = inbi1(:) * inbj1(:)
      iszij1(:) = iszi1(:) * iszj1(:)

      ! if there is no land and no print
      IF( .NOT. llist .AND. numbot == -1 .AND. numbdy == -1 ) THEN
         ! get the smaller partition which gives the smallest subdomain size
         ii = MINLOC(inbij1, mask = iszij1 == MINVAL(iszij1), dim = 1)
         knbi = inbi1(ii)
         knbj = inbj1(ii)
         IF(PRESENT(knbcnt))   knbcnt = 0
         DEALLOCATE( inbi1, inbj1, inbij1, iszi1, iszj1, iszij1 )
         RETURN
      ENDIF

      ! extract only the partitions which reduce the subdomain size in comparison with smaller partitions
      ALLOCATE( indexok(isz1) )                                 ! to store indices of the best partitions
      isz0 = 0                                                  ! number of best partitions     
      inbij = 1                                                 ! start with the min value of inbij1 => 1
      iszij = Ni0glo*Nj0glo+1                                   ! default: larger than global domain
      DO WHILE( inbij <= inbijmax )                             ! if we did not reach the max of inbij1
         ii = MINLOC(iszij1, mask = inbij1 == inbij, dim = 1)   ! warning: send back the first occurence if multiple results
         IF ( iszij1(ii) < iszij ) THEN
            isz0 = isz0 + 1
            indexok(isz0) = ii
            iszij = iszij1(ii)
         ENDIF
         inbij = MINVAL(inbij1, mask = inbij1 > inbij)   ! warning: return largest integer value if mask = .false. everywhere
      END DO
      DEALLOCATE( inbij1, iszij1 )

      ! keep only the best partitions (sorted by increasing order of subdomains number and decreassing subdomain size)
      ALLOCATE( inbi0(isz0), inbj0(isz0), iszi0(isz0), iszj0(isz0) )
      DO ji = 1, isz0
         ii = indexok(ji)
         inbi0(ji) = inbi1(ii)
         inbj0(ji) = inbj1(ii)
         iszi0(ji) = iszi1(ii)
         iszj0(ji) = iszj1(ii)
      END DO
      DEALLOCATE( indexok, inbi1, inbj1, iszi1, iszj1 )

      IF( llist ) THEN
         IF(lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) '                  For your information:'
            WRITE(numout,*) '  list of the best partitions including land supression'
            WRITE(numout,*) '  -----------------------------------------------------'
            WRITE(numout,*)
         END IF
         ji = isz0   ! initialization with the largest value
         ALLOCATE( llisoce(inbi0(ji), inbj0(ji)) )
         CALL mpp_is_ocean( llisoce )   ! Warning: must be call by all cores (call mpp_sum)
         inbijold = COUNT(llisoce)
         DEALLOCATE( llisoce )
         DO ji =isz0-1,1,-1
            ALLOCATE( llisoce(inbi0(ji), inbj0(ji)) )
            CALL mpp_is_ocean( llisoce )   ! Warning: must be call by all cores (call mpp_sum)
            inbij = COUNT(llisoce)
            DEALLOCATE( llisoce )
            IF(lwp .AND. inbij < inbijold) THEN
               WRITE(numout,'(a, i6, a, i6, a, f4.1, a, i9, a, i6, a, i6, a)')                                 &
                  &   'nb_cores oce: ', inbij, ', land domains excluded: ', inbi0(ji)*inbj0(ji) - inbij,       &
                  &   ' (', REAL(inbi0(ji)*inbj0(ji) - inbij,wp) / REAL(inbi0(ji)*inbj0(ji),wp) *100.,         &
                  &   '%), largest oce domain: ', iszi0(ji)*iszj0(ji), ' ( ', iszi0(ji),' x ', iszj0(ji), ' )'
               inbijold = inbij
            END IF
         END DO
         DEALLOCATE( inbi0, inbj0, iszi0, iszj0 )
         IF(lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*)  '  -----------------------------------------------------------'
         ENDIF
         CALL mppsync
         CALL mppstop( ld_abort = .TRUE. )
      ENDIF
      
      DEALLOCATE( iszi0, iszj0 )
      inbij = inbijmax + 1        ! default: larger than possible
      ii = isz0+1                 ! start from the end of the list (smaller subdomains)
      DO WHILE( inbij > knbij )   ! while the number of ocean subdomains exceed the number of procs
         ii = ii -1 
         ALLOCATE( llisoce(inbi0(ii), inbj0(ii)) )
         CALL mpp_is_ocean( llisoce )            ! must be done by all core
         inbij = COUNT(llisoce)
         DEALLOCATE( llisoce )
      END DO
      knbi = inbi0(ii)
      knbj = inbj0(ii)
      IF(PRESENT(knbcnt))   knbcnt = knbi * knbj - inbij
      DEALLOCATE( inbi0, inbj0 )
      !
   END SUBROUTINE bestpartition
   
   
   SUBROUTINE mpp_init_landprop( propland )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE mpp_init_landprop  ***
      !!
      !! ** Purpose : the the proportion of land points in the surface land-sea mask
      !!
      !! ** Method  : read iproc strips (of length Ni0glo) of the land-sea mask
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(  out) :: propland    ! proportion of land points in the global domain (between 0 and 1)
      !
      INTEGER, DIMENSION(jpni*jpnj) ::   kusedom_1d
      INTEGER :: inboce, iarea
      INTEGER :: iproc, idiv, ijsz
      INTEGER :: ijstr
      LOGICAL, ALLOCATABLE, DIMENSION(:,:) ::   lloce
      !!----------------------------------------------------------------------
      ! do nothing if there is no land-sea mask
      IF( numbot == -1 .and. numbdy == -1 ) THEN
         propland = 0.
         RETURN
      ENDIF

      ! number of processes reading the bathymetry file 
      iproc = MINVAL( (/mppsize, Nj0glo/2, 100/) )  ! read a least 2 lines, no more that 100 processes reading at the same time
      
      ! we want to read iproc strips of the land-sea mask. -> pick up iproc processes every idiv processes starting at 1
      IF( iproc == 1 ) THEN   ;   idiv = mppsize
      ELSE                    ;   idiv = ( mppsize - 1 ) / ( iproc - 1 )
      ENDIF

      iarea = (narea-1)/idiv   ! involed process number (starting counting at 0)
      IF( MOD( narea-1, idiv ) == 0 .AND. iarea < iproc ) THEN   ! beware idiv can be = to 1
         !
         ijsz = Nj0glo / iproc                                               ! width of the stripe to read
         IF( iarea < MOD(Nj0glo,iproc) ) ijsz = ijsz + 1
         ijstr = iarea*(Nj0glo/iproc) + MIN(iarea, MOD(Nj0glo,iproc)) + 1    ! starting j position of the reading
         !
         ALLOCATE( lloce(Ni0glo, ijsz) )                                     ! allocate the strip
         CALL readbot_strip( ijstr, ijsz, lloce )
         inboce = COUNT(lloce)                                               ! number of ocean point in the stripe
         DEALLOCATE(lloce)
         !
      ELSE
         inboce = 0
      ENDIF
      CALL mpp_sum( 'mppini', inboce )   ! total number of ocean points over the global domain
      !
      propland = REAL( Ni0glo*Nj0glo - inboce, wp ) / REAL( Ni0glo*Nj0glo, wp ) 
      !
   END SUBROUTINE mpp_init_landprop
   
   
   SUBROUTINE mpp_is_ocean( ldisoce )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE mpp_is_ocean  ***
      !!
      !! ** Purpose : Check for a mpi domain decomposition inbi x inbj which
      !!              subdomains, including 1 halo (even if nn_hls>1), contain
      !!              at least 1 ocean point.
      !!              We must indeed ensure that each subdomain that is a neighbour
      !!              of a land subdomain as only land points on its boundary
      !!              (inside the inner subdomain) with the land subdomain.
      !!              This is needed to get the proper bondary conditions on
      !!              a subdomain with a closed boundary.
      !!
      !! ** Method  : read inbj strips (of length Ni0glo) of the land-sea mask
      !!----------------------------------------------------------------------
      LOGICAL, DIMENSION(:,:), INTENT(  out) ::   ldisoce        ! .true. if a sub domain constains 1 ocean point 
      !
      INTEGER :: idiv, iimax, ijmax, iarea
      INTEGER :: inbi, inbj, inx, iny, inry, isty
      INTEGER :: ji, jn
      INTEGER, ALLOCATABLE, DIMENSION(:,:) ::   inboce           ! number oce oce pint in each mpi subdomain
      INTEGER, ALLOCATABLE, DIMENSION(:  ) ::   inboce_1d
      INTEGER, ALLOCATABLE, DIMENSION(:,:) ::   iimppt, ijpi
      INTEGER, ALLOCATABLE, DIMENSION(:,:) ::   ijmppt, ijpj
      LOGICAL, ALLOCATABLE, DIMENSION(:,:) ::   lloce            ! lloce(i,j) = .true. if the point (i,j) is ocean 
      !!----------------------------------------------------------------------
      ! do nothing if there is no land-sea mask
      IF( numbot == -1 .AND. numbdy == -1 ) THEN
         ldisoce(:,:) = .TRUE.
         RETURN
      ENDIF
      !
      inbi = SIZE( ldisoce, dim = 1 )
      inbj = SIZE( ldisoce, dim = 2 )
      !
      ! we want to read inbj strips of the land-sea mask. -> pick up inbj processes every idiv processes starting at 1
      IF           ( inbj == 1 ) THEN   ;   idiv = mppsize
      ELSE IF ( mppsize < inbj ) THEN   ;   idiv = 1
      ELSE                              ;   idiv = ( mppsize - 1 ) / ( inbj - 1 )
      ENDIF
      !
      ALLOCATE( inboce(inbi,inbj), inboce_1d(inbi*inbj) )
      inboce(:,:) = 0          ! default no ocean point found
      !
      DO jn = 0, (inbj-1)/mppsize   ! if mppsize < inbj : more strips than mpi processes (because of potential land domains)
         !
         iarea = (narea-1)/idiv + jn * mppsize + 1                     ! involed process number (starting counting at 1)
         IF( MOD( narea-1, idiv ) == 0 .AND. iarea <= inbj ) THEN      ! beware idiv can be = to 1
            !
            ALLOCATE( iimppt(inbi,inbj), ijmppt(inbi,inbj), ijpi(inbi,inbj), ijpj(inbi,inbj) )
            CALL mpp_basesplit( Ni0glo, Nj0glo, 0, inbi, inbj, iimax, ijmax, iimppt, ijmppt, ijpi, ijpj )
            !
            inx = Ni0glo + 2   ;   iny = ijpj(1,iarea) + 2             ! strip size + 1 halo on each direction (even if nn_hls>1)
            ALLOCATE( lloce(inx, iny) )                                ! allocate the strip
            inry = iny - COUNT( (/ iarea == 1, iarea == inbj /) )      ! number of point to read in y-direction
            isty = 1 + COUNT( (/ iarea == 1 /) )                       ! read from the first or the second line?
            CALL readbot_strip( ijmppt(1,iarea) - 2 + isty, inry, lloce(2:inx-1, isty:inry+isty-1) )   ! read the strip
            ! 
            IF( iarea == 1    ) THEN                                   ! the first line was not read
               IF( jperio == 2 .OR. jperio == 7 ) THEN                 !   north-south periodocity
                  CALL readbot_strip( Nj0glo, 1, lloce(2:inx-1, 1) )   !   read the last line -> first line of lloce
               ELSE
                  lloce(2:inx-1,  1) = .FALSE.                         !   closed boundary
               ENDIF
            ENDIF
            IF( iarea == inbj ) THEN                                   ! the last line was not read
               IF( jperio == 2 .OR. jperio == 7 ) THEN                 !   north-south periodocity
                  CALL readbot_strip( 1, 1, lloce(2:inx-1,iny) )       !      read the first line -> last line of lloce
               ELSEIF( jperio == 3 .OR. jperio == 4 ) THEN             !   north-pole folding T-pivot, T-point 
                  lloce(2,iny) = lloce(2,iny-2)                        !      here we have 1 halo (even if nn_hls>1)
                  DO ji = 3,inx-1
                     lloce(ji,iny  ) = lloce(inx-ji+2,iny-2)           !      ok, we have at least 3 lines
                  END DO
                  DO ji = inx/2+2,inx-1
                     lloce(ji,iny-1) = lloce(inx-ji+2,iny-1)
                  END DO
               ELSEIF( jperio == 5 .OR. jperio == 6 ) THEN             !   north-pole folding F-pivot, T-point, 1 halo
                  lloce(inx/2+1,iny-1) = lloce(inx/2,iny-1)            !      here we have 1 halo (even if nn_hls>1)
                  lloce(inx  -1,iny-1) = lloce(2    ,iny-1)
                  DO ji = 2,inx-1
                     lloce(ji,iny) = lloce(inx-ji+1,iny-1)
                  END DO
               ELSE                                                    !   closed boundary
                  lloce(2:inx-1,iny) = .FALSE.
               ENDIF
            ENDIF
            !                                                          ! first and last column were not read
            IF( jperio == 1 .OR. jperio == 4 .OR. jperio == 6 .OR. jperio == 7 ) THEN
               lloce(1,:) = lloce(inx-1,:)   ;   lloce(inx,:) = lloce(2,:)   ! east-west periodocity
            ELSE
               lloce(1,:) = .FALSE.          ;   lloce(inx,:) = .FALSE.      ! closed boundary
            ENDIF
            !
            DO  ji = 1, inbi
               inboce(ji,iarea) = COUNT( lloce(iimppt(ji,1):iimppt(ji,1)+ijpi(ji,1)+1,:) )   ! lloce as 2 points more than Ni0glo
            END DO
            !
            DEALLOCATE(lloce)
            DEALLOCATE(iimppt, ijmppt, ijpi, ijpj)
            !
         ENDIF
      END DO
   
      inboce_1d = RESHAPE(inboce, (/ inbi*inbj /))
      CALL mpp_sum( 'mppini', inboce_1d )
      inboce = RESHAPE(inboce_1d, (/inbi, inbj/))
      ldisoce(:,:) = inboce(:,:) /= 0
      DEALLOCATE(inboce, inboce_1d)
      !
   END SUBROUTINE mpp_is_ocean
   
   
   SUBROUTINE readbot_strip( kjstr, kjcnt, ldoce )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE readbot_strip  ***
      !!
      !! ** Purpose : Read relevant bathymetric information in order to
      !!              provide a land/sea mask used for the elimination
      !!              of land domains, in an mpp computation.
      !!
      !! ** Method  : read stipe of size (Ni0glo,...)
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   kjstr       ! starting j position of the reading
      INTEGER                         , INTENT(in   ) ::   kjcnt       ! number of lines to read
      LOGICAL, DIMENSION(Ni0glo,kjcnt), INTENT(  out) ::   ldoce       ! ldoce(i,j) = .true. if the point (i,j) is ocean 
      !
      INTEGER                           ::   inumsave                ! local logical unit
      REAL(wp), DIMENSION(Ni0glo,kjcnt) ::   zbot, zbdy 
      !!----------------------------------------------------------------------
      !
      inumsave = numout   ;   numout = numnul   !   redirect all print to /dev/null
      !
      IF( numbot /= -1 ) THEN   
         CALL iom_get( numbot, jpdom_unknown, 'bottom_level', zbot, kstart = (/1,kjstr/), kcount = (/Ni0glo, kjcnt/) )
      ELSE
         zbot(:,:) = 1._wp                      ! put a non-null value
      ENDIF
      !
      IF( numbdy /= -1 ) THEN                   ! Adjust with bdy_msk if it exists    
         CALL iom_get ( numbdy, jpdom_unknown,     'bdy_msk', zbdy, kstart = (/1,kjstr/), kcount = (/Ni0glo, kjcnt/) )
         zbot(:,:) = zbot(:,:) * zbdy(:,:)
      ENDIF
      !
      ldoce(:,:) = zbot(:,:) > 0._wp
      numout = inumsave
      !
   END SUBROUTINE readbot_strip


   SUBROUTINE mpp_getnum( ldisoce, kproc, kipos, kjpos )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE mpp_getnum  ***
      !!
      !! ** Purpose : give a number to each MPI subdomains (starting at 0)
      !!
      !! ** Method  : start from bottom left. First skip land subdomain, and finally use them if needed
      !!----------------------------------------------------------------------
      LOGICAL, DIMENSION(:,:), INTENT(in   ) ::   ldisoce     ! F if land process
      INTEGER, DIMENSION(:,:), INTENT(  out) ::   kproc       ! subdomain number (-1 if supressed, starting at 0)
      INTEGER, DIMENSION(  :), INTENT(  out) ::   kipos       ! i-position of the subdomain (from 1 to jpni)
      INTEGER, DIMENSION(  :), INTENT(  out) ::   kjpos       ! j-position of the subdomain (from 1 to jpnj)
      !
      INTEGER :: ii, ij, jarea, iarea0
      INTEGER :: icont, i2add , ini, inj, inij
      !!----------------------------------------------------------------------
      !
      ini = SIZE(ldisoce, dim = 1)
      inj = SIZE(ldisoce, dim = 2)
      inij = SIZE(kipos)
      !
      ! specify which subdomains are oce subdomains; other are land subdomains
      kproc(:,:) = -1
      icont = -1
      DO jarea = 1, ini*inj
         iarea0 = jarea - 1
         ii = 1 + MOD(iarea0,ini)
         ij = 1 +     iarea0/ini
         IF( ldisoce(ii,ij) ) THEN
            icont = icont + 1
            kproc(ii,ij) = icont
            kipos(icont+1) = ii
            kjpos(icont+1) = ij
         ENDIF
      END DO
      ! if needed add some land subdomains to reach inij active subdomains
      i2add = inij - COUNT( ldisoce )
      DO jarea = 1, ini*inj
         iarea0 = jarea - 1
         ii = 1 + MOD(iarea0,ini)
         ij = 1 +     iarea0/ini
         IF( .NOT. ldisoce(ii,ij) .AND. i2add > 0 ) THEN
            icont = icont + 1
            kproc(ii,ij) = icont
            kipos(icont+1) = ii
            kjpos(icont+1) = ij
            i2add = i2add - 1
         ENDIF
      END DO
      !
   END SUBROUTINE mpp_getnum


   SUBROUTINE init_ioipsl
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE init_ioipsl  ***
      !!
      !! ** Purpose :   
      !!
      !! ** Method  :   
      !!
      !! History :
      !!   9.0  !  04-03  (G. Madec )  MPP-IOIPSL 
      !!   " "  !  08-12  (A. Coward)  addition in case of jpni*jpnj < jpnij
      !!----------------------------------------------------------------------
      INTEGER, DIMENSION(2) ::   iglo, iloc, iabsf, iabsl, ihals, ihale, idid
      !!----------------------------------------------------------------------

      ! The domain is split only horizontally along i- or/and j- direction
      ! So we need at the most only 1D arrays with 2 elements.
      ! Set idompar values equivalent to the jpdom_local_noextra definition
      ! used in IOM. This works even if jpnij .ne. jpni*jpnj.
      iglo( :) = (/ Ni0glo, Nj0glo /)
      iloc( :) = (/ Ni_0  , Nj_0   /)
      iabsf(:) = (/ Nis0  , Njs0   /) + (/ nimpp, njmpp /) - 1 - nn_hls   ! corresponds to mig0(Nis0) but mig0 is not yet defined!
      iabsl(:) = iabsf(:) + iloc(:) - 1
      ihals(:) = (/ 0     , 0      /)
      ihale(:) = (/ 0     , 0      /)
      idid( :) = (/ 1     , 2      /)

      IF(lwp) THEN
          WRITE(numout,*)
          WRITE(numout,*) 'mpp init_ioipsl :   iloc  = ', iloc
          WRITE(numout,*) '~~~~~~~~~~~~~~~     iabsf = ', iabsf
          WRITE(numout,*) '                    ihals = ', ihals
          WRITE(numout,*) '                    ihale = ', ihale
      ENDIF
      !
      CALL flio_dom_set ( jpnij, nproc, idid, iglo, iloc, iabsf, iabsl, ihals, ihale, 'BOX', nidom)
      !
   END SUBROUTINE init_ioipsl  


   SUBROUTINE init_nfdcom
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE  init_nfdcom  ***
      !! ** Purpose :   Setup for north fold exchanges with explicit 
      !!                point-to-point messaging
      !!
      !! ** Method :   Initialization of the northern neighbours lists.
      !!----------------------------------------------------------------------
      !!    1.0  ! 2011-10  (A. C. Coward, NOCS & J. Donners, PRACE)
      !!    2.0  ! 2013-06 Setup avoiding MPI communication (I. Epicoco, S. Mocavero, CMCC) 
      !!----------------------------------------------------------------------
      INTEGER  ::   sxM, dxM, sxT, dxT, jn
      !!----------------------------------------------------------------------
      !
      !initializes the north-fold communication variables
      isendto(:) = 0
      nsndto     = 0
      !
      IF ( njmpp == MAXVAL( njmppt ) ) THEN      ! if I am a process in the north
         !
         !sxM is the first point (in the global domain) needed to compute the north-fold for the current process
         sxM = jpiglo - nimppt(narea) - jpiall(narea) + 1
         !dxM is the last point (in the global domain) needed to compute the north-fold for the current process
         dxM = jpiglo - nimppt(narea) + 2
         !
         ! loop over the other north-fold processes to find the processes
         ! managing the points belonging to the sxT-dxT range
         !
         DO jn = 1, jpni
            !
            sxT = nfimpp(jn)                    ! sxT = 1st  point (in the global domain) of the jn process
            dxT = nfimpp(jn) + nfjpi(jn) - 1    ! dxT = last point (in the global domain) of the jn process
            !
            IF    ( sxT < sxM  .AND.  sxM < dxT ) THEN
               nsndto          = nsndto + 1
               isendto(nsndto) = jn
            ELSEIF( sxM <= sxT  .AND.  dxM >= dxT ) THEN
               nsndto          = nsndto + 1
               isendto(nsndto) = jn
            ELSEIF( dxM <  dxT  .AND.  sxT <  dxM ) THEN
               nsndto          = nsndto + 1
               isendto(nsndto) = jn
            ENDIF
            !
         END DO
         !
      ENDIF
      l_north_nogather = .TRUE.
      !
   END SUBROUTINE init_nfdcom

#endif

   SUBROUTINE init_doloop
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE init_doloop  ***
      !!
      !! ** Purpose :   set the starting/ending indices of DO-loop
      !!              These indices are used in do_loop_substitute.h90
      !!----------------------------------------------------------------------
      !
      Nis0 =   1+nn_hls   ;   Nis1 = Nis0-1   ;   Nis2 = MAX(  1, Nis0-2)
      Njs0 =   1+nn_hls   ;   Njs1 = Njs0-1   ;   Njs2 = MAX(  1, Njs0-2)  
      !                                                 
      Nie0 = jpi-nn_hls   ;   Nie1 = Nie0+1   ;   Nie2 = MIN(jpi, Nie0+2)
      Nje0 = jpj-nn_hls   ;   Nje1 = Nje0+1   ;   Nje2 = MIN(jpj, Nje0+2)
      !
      IF( nn_hls == 1 ) THEN          !* halo size of 1
         !
         Nis1nxt2 = Nis0   ;   Njs1nxt2 = Njs0
         Nie1nxt2 = Nie0   ;   Nje1nxt2 = Nje0
         !
      ELSE                            !* larger halo size... 
         !
         Nis1nxt2 = Nis1   ;   Njs1nxt2 = Njs1
         Nie1nxt2 = Nie1   ;   Nje1nxt2 = Nje1
         !
      ENDIF
      !
      Ni_0 = Nie0 - Nis0 + 1
      Nj_0 = Nje0 - Njs0 + 1
      Ni_1 = Nie1 - Nis1 + 1
      Nj_1 = Nje1 - Njs1 + 1
      Ni_2 = Nie2 - Nis2 + 1
      Nj_2 = Nje2 - Njs2 + 1
      !
   END SUBROUTINE init_doloop
   
   !!======================================================================
END MODULE mppini
