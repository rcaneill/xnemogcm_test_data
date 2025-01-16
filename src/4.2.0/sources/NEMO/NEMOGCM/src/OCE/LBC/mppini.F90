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
   USE bdy_oce        ! open BounDarY
   !
   USE lbcnfd         ! Setup of north fold exchanges
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
   !! $Id: mppini.F90 15302 2021-09-29 15:00:15Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

#if defined key_mpi_off
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
      nn_comm = 1
      nn_hls  = 1
      jpiglo  = Ni0glo + 2 * nn_hls
      jpjglo  = Nj0glo + 2 * nn_hls
      jpimax  = jpiglo
      jpjmax  = jpjglo
      jpi     = jpiglo
      jpj     = jpjglo
      jpk     = MAX( 2, jpkglo )
      jpij    = jpi*jpj
      jpni    = 1
      jpnj    = 1
      jpnij   = jpni*jpnj
      nimpp   = 1
      njmpp   = 1
      nidom   = FLIO_DOM_NONE
      !
      mpiSnei(:,:) = -1
      mpiRnei(:,:) = -1
      l_SelfPerio(1:2) = l_Iperio                  !  west,  east periodicity by itself
      l_SelfPerio(3:4) = l_Jperio                  ! south, north periodicity by itself
      l_SelfPerio(5:8) = l_Iperio .AND. l_Jperio   ! corners bi-periodicity by itself
      l_IdoNFold = l_NFold                         ! is this process doing North fold?
      !
      CALL init_doloop                       ! set start/end indices or do-loop depending on the halo width value (nn_hls)
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'mpp_init : NO massively parallel processing'
         WRITE(numout,*) '~~~~~~~~ '
         WRITE(numout,*) '   l_Iperio = ', l_Iperio, '    l_Jperio = ', l_Jperio
         WRITE(numout,*) '     njmpp  = ', njmpp
      ENDIF
      !
#if defined key_agrif
      call agrif_nemo_init()
#endif
   END SUBROUTINE mpp_init

#else
   !!----------------------------------------------------------------------
   !!                   MPI massively parallel processing
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
      !!      (global boundary or neighbouring domain) and of the global periodic
      !!
      !! ** Action : - set domain parameters
      !!                    nimpp     : longitudinal index
      !!                    njmpp     : latitudinal  index
      !!                    narea     : number for local area
      !!                    mpinei    : number of neighboring domains (starting at 0, -1 if no neighbourg)
      !!----------------------------------------------------------------------
      INTEGER ::   ji, jj, jn, jp, jh
      INTEGER ::   ii, ij, ii2, ij2
      INTEGER ::   inijmin   ! number of oce subdomains
      INTEGER ::   inum, inum0
      INTEGER ::   ifreq, il1, imil, il2, ijm1
      INTEGER ::   ierr, ios
      INTEGER ::   inbi, inbj, iimax, ijmax, icnt1, icnt2
      INTEGER, DIMENSION(16*n_hlsmax) :: ichanged
      INTEGER, ALLOCATABLE, DIMENSION(:    ) ::   iin, ijn
      INTEGER, ALLOCATABLE, DIMENSION(:,:  ) ::   iimppt, ijpi, ipproc
      INTEGER, ALLOCATABLE, DIMENSION(:,:  ) ::   ijmppt, ijpj
      INTEGER, ALLOCATABLE, DIMENSION(:,:  ) ::   impi
      INTEGER, ALLOCATABLE, DIMENSION(:,:,:) ::   inei
      LOGICAL ::   llbest, llauto
      LOGICAL ::   llwrtlay
      LOGICAL ::   llmpi_Iperio, llmpi_Jperio, llmpiNFold
      LOGICAL ::   ln_listonly
      LOGICAL, ALLOCATABLE, DIMENSION(:,:  ) ::   llisOce  ! is not land-domain only?
      LOGICAL, ALLOCATABLE, DIMENSION(:,:,:) ::   llnei    ! are neighbourgs existing?
      NAMELIST/nambdy/ ln_bdy, nb_bdy, ln_coords_file, cn_coords_file,           &
           &             ln_mask_file, cn_mask_file, cn_dyn2d, nn_dyn2d_dta,     &
           &             cn_dyn3d, nn_dyn3d_dta, cn_tra, nn_tra_dta,             &
           &             ln_tra_dmp, ln_dyn3d_dmp, rn_time_dmp, rn_time_dmp_out, &
           &             cn_ice, nn_ice_dta,                                     &
           &             ln_vol, nn_volctl, nn_rimwidth
      NAMELIST/nammpp/ jpni, jpnj, nn_hls, ln_nnogather, ln_listonly, nn_comm
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
         IF( jpni < 1 .OR. jpnj < 1 ) THEN
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
      jpiglo = Ni0glo + 2 * nn_hls
      jpjglo = Nj0glo + 2 * nn_hls
      !
      ! do we need to take into account bdy_msk?
      READ  ( numnam_ref, nambdy, IOSTAT = ios, ERR = 903)
903   IF( ios /= 0 )   CALL ctl_nam ( ios , 'nambdy in reference namelist (mppini)' )
      READ  ( numnam_cfg, nambdy, IOSTAT = ios, ERR = 904 )
904   IF( ios >  0 )   CALL ctl_nam ( ios , 'nambdy in configuration namelist (mppini)' )
      !
      IF(               ln_read_cfg ) CALL iom_open( cn_domcfg,    numbot )
      IF( ln_bdy .AND. ln_mask_file ) CALL iom_open( cn_mask_file, numbdy )
      !
      IF( ln_listonly )   CALL bestpartition( MAX(mppsize,jpni*jpnj), ldlist = .TRUE. )   ! must be done by all core
      !
      !  1. Dimension arrays for subdomains
      ! -----------------------------------
      !
      ! If dimensions of MPI processes grid weren't specified in the namelist file
      ! then we calculate them here now that we have our communicator size
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'mpp_init:'
         WRITE(numout,*) '~~~~~~~~ '
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
      ALLOCATE( llisOce(jpni,jpnj) )
      CALL mpp_is_ocean( llisOce )
      inijmin = COUNT( llisOce )   ! number of oce subdomains

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

      ALLOCATE( nfimpp(jpni), nfproc(jpni), nfjpi(jpni),   &
         &      iin(jpnij), ijn(jpnij),   &
         &      iimppt(jpni,jpnj), ijmppt(jpni,jpnj), ijpi(jpni,jpnj), ijpj(jpni,jpnj), ipproc(jpni,jpnj),   &
         &      inei(8,jpni,jpnj), llnei(8,jpni,jpnj),   &
         &      impi(8,jpnij),   &
         &      STAT=ierr )
      CALL mpp_sum( 'mppini', ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'mpp_init: unable to allocate standard ocean arrays' )

#if defined key_agrif
         CALL agrif_nemo_init()
#endif
      !
      !  2. Index arrays for subdomains
      ! -----------------------------------
      !
      CALL mpp_basesplit( jpiglo, jpjglo, nn_hls, jpni, jpnj, jpimax, jpjmax, iimppt, ijmppt, ijpi, ijpj )
      CALL mpp_getnum( llisOce, ipproc, iin, ijn )
      !
      ii = iin(narea)
      ij = ijn(narea)
      jpi   = ijpi(ii,ij)
      jpj   = ijpj(ii,ij)
      jpk   = MAX( 2, jpkglo )
      jpij  = jpi*jpj
      nimpp = iimppt(ii,ij)
      njmpp = ijmppt(ii,ij)
      !
      CALL init_doloop    ! set start/end indices of do-loop, depending on the halo width value (nn_hls)
      CALL init_locglo    ! define now functions needed to convert indices from/to global to/from local domains
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'MPI Message Passing MPI - domain lay out over processors'
         WRITE(numout,*)
         WRITE(numout,*) '   defines mpp subdomains'
         WRITE(numout,*) '      jpni = ', jpni
         WRITE(numout,*) '      jpnj = ', jpnj
         WRITE(numout,*) '     jpnij = ', jpnij
         WRITE(numout,*) '     nimpp = ', nimpp
         WRITE(numout,*) '     njmpp = ', njmpp
         WRITE(numout,*)
         WRITE(numout,*) '      sum ijpi(i,1) = ', sum(ijpi(:,1)), ' jpiglo = ', jpiglo
         WRITE(numout,*) '      sum ijpj(1,j) = ', SUM(ijpj(1,:)), ' jpjglo = ', jpjglo
         
         ! Subdomain grid print
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
      !
      ! Store informations for the north pole folding communications
      nfproc(:) = ipproc(:,jpnj)
      nfimpp(:) = iimppt(:,jpnj)
      nfjpi (:) =   ijpi(:,jpnj)
      !
      ! 3. Define Western, Eastern, Southern and Northern neighbors + corners in the subdomain grid reference
      ! ------------------------------------------------------------------------------------------------------
      !
      ! note that North fold is has specific treatment for its MPI communications.
      ! This must not be treated as a "usual" communication with a northern neighbor.
      !    -> North fold processes have no Northern neighbor in the definition done bellow
      !
      llmpi_Iperio = jpni > 1 .AND. l_Iperio                         ! do i-periodicity with an MPI communication?
      llmpi_Jperio = jpnj > 1 .AND. l_Jperio                         ! do j-periodicity with an MPI communication?
      !
      l_SelfPerio(1:2) = l_Iperio .AND. jpni == 1                    !  west,  east periodicity by itself
      l_SelfPerio(3:4) = l_Jperio .AND. jpnj == 1                    ! south, north periodicity by itself
      l_SelfPerio(5:8) = l_SelfPerio(jpwe) .AND. l_SelfPerio(jpso)   ! corners bi-periodicity by itself
      !
      ! define neighbors mapping (1/2): default definition: ignore if neighbours are land-only subdomains or not
      DO jj = 1, jpnj
         DO ji = 1, jpni
            !
            IF ( llisOce(ji,jj) ) THEN                     ! this subdomain has some ocean: it has neighbours
               !
               inum0 = ji - 1 + ( jj - 1 ) * jpni             ! index in the subdomains grid. start at 0
               !
               ! Is there a neighbor?
               llnei(jpwe,ji,jj) = ji >   1  .OR. llmpi_Iperio           ! West  nei exists if not the first column or llmpi_Iperio
               llnei(jpea,ji,jj) = ji < jpni .OR. llmpi_Iperio           ! East  nei exists if not the last  column or llmpi_Iperio
               llnei(jpso,ji,jj) = jj >   1  .OR. llmpi_Jperio           ! South nei exists if not the first line   or llmpi_Jperio
               llnei(jpno,ji,jj) = jj < jpnj .OR. llmpi_Jperio           ! North nei exists if not the last  line   or llmpi_Jperio
               llnei(jpsw,ji,jj) = llnei(jpwe,ji,jj) .AND. llnei(jpso,ji,jj)   ! So-We nei exists if both South and West nei exist
               llnei(jpse,ji,jj) = llnei(jpea,ji,jj) .AND. llnei(jpso,ji,jj)   ! So-Ea nei exists if both South and East nei exist
               llnei(jpnw,ji,jj) = llnei(jpwe,ji,jj) .AND. llnei(jpno,ji,jj)   ! No-We nei exists if both North and West nei exist
               llnei(jpne,ji,jj) = llnei(jpea,ji,jj) .AND. llnei(jpno,ji,jj)   ! No-Ea nei exists if both North and East nei exist
               !
               ! Which index (starting at 0) have neighbors in the subdomains grid?
               IF( llnei(jpwe,ji,jj) )   inei(jpwe,ji,jj) =            inum0 -    1 + jpni        * COUNT( (/ ji ==    1 /) )
               IF( llnei(jpea,ji,jj) )   inei(jpea,ji,jj) =            inum0 +    1 - jpni        * COUNT( (/ ji == jpni /) )
               IF( llnei(jpso,ji,jj) )   inei(jpso,ji,jj) =            inum0 - jpni + jpni * jpnj * COUNT( (/ jj ==    1 /) )
               IF( llnei(jpno,ji,jj) )   inei(jpno,ji,jj) =            inum0 + jpni - jpni * jpnj * COUNT( (/ jj == jpnj /) )
               IF( llnei(jpsw,ji,jj) )   inei(jpsw,ji,jj) = inei(jpso,ji,jj) -    1 + jpni        * COUNT( (/ ji ==    1 /) )
               IF( llnei(jpse,ji,jj) )   inei(jpse,ji,jj) = inei(jpso,ji,jj) +    1 - jpni        * COUNT( (/ ji == jpni /) )
               IF( llnei(jpnw,ji,jj) )   inei(jpnw,ji,jj) = inei(jpno,ji,jj) -    1 + jpni        * COUNT( (/ ji ==    1 /) )
               IF( llnei(jpne,ji,jj) )   inei(jpne,ji,jj) = inei(jpno,ji,jj) +    1 - jpni        * COUNT( (/ ji == jpni /) )
               !
            ELSE                                           ! land-only domain has no neighbour
               llnei(:,ji,jj) = .FALSE.
            ENDIF
            !
         END DO
      END DO
      !
      ! define neighbors mapping (2/2): check if neighbours are not land-only subdomains
      DO jj = 1, jpnj
         DO ji = 1, jpni
            DO jn = 1, 8
               IF( llnei(jn,ji,jj) ) THEN   ! if a neighbour is existing -> this should not be a land-only domain
                  ii = 1 + MOD( inei(jn,ji,jj) , jpni )
                  ij = 1 +      inei(jn,ji,jj) / jpni
                  llnei(jn,ji,jj) = llisOce( ii, ij )
               ENDIF
            END DO
         END DO
      END DO
      !
      ! update index of the neighbours in the subdomains grid
      WHERE( .NOT. llnei )   inei = -1
      !
      ! Save processor layout in ascii file
      IF (llwrtlay) THEN
         CALL ctl_opn( inum, 'layout.dat', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE., narea )
         WRITE(inum,'(a)') '  jpnij jpimax jpjmax    jpk jpiglo jpjglo ( local:   narea    jpi    jpj )'
         WRITE(inum,'(6i7,a,3i7,a)') jpnij,jpimax,jpjmax,jpk,jpiglo,jpjglo,' ( local: ',narea,jpi,jpj,' )'
         WRITE(inum,*) 
         WRITE(inum,       *) '------------------------------------'
         WRITE(inum,'(a,i2)') ' Mapping of the default neighnourgs '
         WRITE(inum,       *) '------------------------------------'
         WRITE(inum,*) 
         WRITE(inum,'(a)') '  rank    ii    ij   jpi   jpj nimpp njmpp mpiwe mpiea mpiso mpino mpisw mpise mpinw mpine'
         DO jp = 1, jpnij
            ii = iin(jp)
            ij = ijn(jp)
            WRITE(inum,'(15i6)')  jp-1, ii, ij, ijpi(ii,ij), ijpj(ii,ij), iimppt(ii,ij), ijmppt(ii,ij), inei(:,ii,ij)
         END DO
      ENDIF

      !
      ! 4. Define Western, Eastern, Southern and Northern neighbors + corners for each mpi process
      ! ------------------------------------------------------------------------------------------
      ! 
      ! rewrite information from "subdomain grid" to mpi process list
      ! Warning, for example:
      !    position of the northern neighbor in the "subdomain grid"
      !    position of the northern neighbor in the "mpi process list"
      
      ! default definition: no neighbors
      impi(:,:) = -1   ! (starting at 0, -1 if no neighbourg)
      
      DO jp = 1, jpnij
         ii = iin(jp)
         ij = ijn(jp)
         DO jn = 1, 8
            IF( llnei(jn,ii,ij) ) THEN   ! must be tested as some land-domain can be kept to fit mppsize
               ii2 = 1 + MOD( inei(jn,ii,ij) , jpni )
               ij2 = 1 +      inei(jn,ii,ij) / jpni
               impi(jn,jp) = ipproc( ii2, ij2 )
            ENDIF
         END DO
      END DO

      !
      ! 4. keep information for the local process
      ! -----------------------------------------
      !
      ! set default neighbours
      mpinei(:) = impi(:,narea)
      DO jh = 1, n_hlsmax
         mpiSnei(jh,:) = impi(:,narea)   ! default definition
         mpiRnei(jh,:) = impi(:,narea)
      END DO
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) '   resulting internal parameters : '
         WRITE(numout,*) '      narea = ', narea
         WRITE(numout,*) '      mpi nei  west = ', mpinei(jpwe)  , '   mpi nei  east = ', mpinei(jpea)
         WRITE(numout,*) '      mpi nei south = ', mpinei(jpso)  , '   mpi nei north = ', mpinei(jpno)
         WRITE(numout,*) '      mpi nei so-we = ', mpinei(jpsw)  , '   mpi nei so-ea = ', mpinei(jpse)
         WRITE(numout,*) '      mpi nei no-we = ', mpinei(jpnw)  , '   mpi nei no-ea = ', mpinei(jpne)
      ENDIF
      !
      CALL mpp_ini_nc(nn_hls)    ! Initialize communicator for neighbourhood collective communications
      DO jh = 1, n_hlsmax
         mpi_nc_com4(jh) = mpi_nc_com4(nn_hls)   ! default definition
         mpi_nc_com8(jh) = mpi_nc_com8(nn_hls)
      END DO
      !                          ! Exclude exchanges which contain only land points
      !
      IF( jpnij > 1 ) CALL init_excl_landpt
      !
      !                          ! Prepare mpp north fold
      !
      llmpiNFold =          jpni  > 1 .AND. l_NFold           ! is the North fold done with an MPI communication?
      l_IdoNFold = ijn(narea) == jpnj .AND. l_NFold           ! is this process doing North fold?
      !
      IF( llmpiNFold )   CALL init_nfdcom( llwrtlay, inum )   ! init northfold communication, must be done after init_excl_landpt
      !
      !                          !  Save processor layout changes in ascii file
      !
      DO jh = 1, n_hlsmax    ! different halo size
         DO ji = 1, 8
            ichanged(16*(jh-1)  +ji) = COUNT( mpinei(ji:ji) /= mpiSnei(jh,ji:ji) )
            ichanged(16*(jh-1)+8+ji) = COUNT( mpinei(ji:ji) /= mpiRnei(jh,ji:ji) )
         END DO
      END DO
      CALL mpp_sum( "mpp_init", ichanged )   ! must be called by all processes
      IF (llwrtlay) THEN
         WRITE(inum,*) 
         WRITE(inum,       *) '----------------------------------------------------------------------'
         WRITE(inum,'(a,i2)') ' Mapping of the neighnourgs once excluding comm with only land points '
         WRITE(inum,       *) '----------------------------------------------------------------------'
         DO jh = 1, n_hlsmax    ! different halo size
            WRITE(inum,*) 
            WRITE(inum,'(a,i2)') 'halo size: ', jh
            WRITE(inum,       *) '---------'
            WRITE(inum,'(a)') '  rank    ii    ij mpiwe mpiea mpiso mpino mpisw mpise mpinw mpine'
            WRITE(inum,   '(11i6,a)')  narea-1, iin(narea), ijn(narea),   mpinei(:), ' <- Org'
            WRITE(inum,'(18x,8i6,a,i1,a)')   mpiSnei(jh,:), ' <- Send ', COUNT( mpinei(:) /= mpiSnei(jh,:) ), ' modif'
            WRITE(inum,'(18x,8i6,a,i1,a)')   mpiRnei(jh,:), ' <- Recv ', COUNT( mpinei(:) /= mpiRnei(jh,:) ), ' modif'
            WRITE(inum,*) ' total changes among all mpi tasks:'
            WRITE(inum,*) '       mpiwe mpiea mpiso mpino mpisw mpise mpinw mpine'
            WRITE(inum,'(a,8i6)') ' Send: ', ichanged(jh*16-15:jh*16-8)
            WRITE(inum,'(a,8i6)') ' Recv: ', ichanged(jh*16 -7:jh*16  )
         END DO
      ENDIF
      !
      CALL init_ioipsl           ! Prepare NetCDF output file (if necessary)
      !
      IF (llwrtlay) CLOSE(inum)
      !
      DEALLOCATE(iin, ijn, iimppt, ijmppt, ijpi, ijpj, ipproc, inei, llnei, impi, llisOce)
      !
    END SUBROUTINE mpp_init

#endif

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
      IF( MINVAL(klci) < 3*khls ) THEN
         WRITE(ctmp1,*) '   mpp_basesplit: minimum value of jpi must be >= ', 3*khls
         WRITE(ctmp2,*) '   We have ', MINVAL(klci)
         CALL ctl_stop( 'STOP', ctmp1, ctmp2 )
      ENDIF
      IF( l_NFold ) THEN
         ! minimize the size of the last row to compensate for the north pole folding coast
         IF( c_NFtype == 'T' )   ijpjmin = 2+3*khls   ! V and F folding must be outside of southern halos
         IF( c_NFtype == 'F' )   ijpjmin = 1+3*khls   ! V and F folding must be outside of southern halos
         irm = knbj - irestj                          ! total number of lines to be removed
         klcj(:,knbj) = MAX( ijpjmin, kjmax-irm )     ! we must have jpj >= ijpjmin in the last row
         irm = irm - ( kjmax - klcj(1,knbj) )         ! remaining number of lines to remove
         irestj = knbj - 1 - irm
         klcj(:, irestj+1:knbj-1) = kjmax-1
      ELSE
         klcj(:, irestj+1:knbj  ) = kjmax-1
      ENDIF
      klcj(:,1:irestj) = kjmax
      IF( MINVAL(klcj) < 3*khls ) THEN
         WRITE(ctmp1,*) '   mpp_basesplit: minimum value of jpj must be >= ', 3*khls
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
      INTEGER,           INTENT(in   ) ::   knbij         ! total number of subdomains (knbi*knbj)
      INTEGER, OPTIONAL, INTENT(  out) ::   knbi, knbj    ! number if subdomains along i and j (knbi and knbj)
      INTEGER, OPTIONAL, INTENT(  out) ::   knbcnt        ! number of land subdomains
      LOGICAL, OPTIONAL, INTENT(in   ) ::   ldlist        ! .true.: print the list the best domain decompositions (with land)
      !
      INTEGER :: ji, jj, ii, iitarget
      INTEGER :: iszitst, iszjtst
      INTEGER :: isziref, iszjref
      INTEGER :: iszimin, iszjmin
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
      LOGICAL, DIMENSION(:,:), ALLOCATABLE :: llisOce              !  -     -
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
      isziref = jpiglo*jpjglo+1   ! define a value that is larger than the largest possible
      iszjref = jpiglo*jpjglo+1
      !
      ! WARNING, see also init_excl_landpt: minimum subdomain size defined here according to nn_hls (and not n_hlsmax)
      ! --> If, one day, we want to use local halos largers than nn_hls, we must replace nn_hls by n_hlsmax
      !
      iszimin = 3*nn_hls          ! minimum size of the MPI subdomain so halos are always adressing neighbor inner domain
      iszjmin = 3*nn_hls
      IF( c_NFtype == 'T' )   iszjmin = MAX(iszjmin, 2+3*nn_hls)   ! V and F folding must be outside of southern halos
      IF( c_NFtype == 'F' )   iszjmin = MAX(iszjmin, 1+3*nn_hls)   ! V and F folding must be outside of southern halos
      !
      ! get the list of knbi that gives a smaller jpimax than knbi-1
      ! get the list of knbj that gives a smaller jpjmax than knbj-1
      DO ji = 1, inbijmax
#if defined key_nemocice_decomp
         iszitst = ( nx_global+2-2*nn_hls + (ji-1) ) / ji + 2*nn_hls    ! first  dim.
#else
         iszitst = ( Ni0glo + (ji-1) ) / ji + 2*nn_hls   ! max subdomain i-size
#endif
         IF( iszitst < isziref .AND. iszitst >= iszimin ) THEN
            isziref = iszitst
            inbimax = inbimax + 1
            inbi0(inbimax) = ji
            iszi0(inbimax) = isziref
         ENDIF
#if defined key_nemocice_decomp
         iszjtst = ( ny_global+2-2*nn_hls + (ji-1) ) / ji + 2*nn_hls    ! first  dim.
#else
         iszjtst = ( Nj0glo + (ji-1) ) / ji + 2*nn_hls   ! max subdomain j-size
#endif
         IF( iszjtst < iszjref .AND. iszjtst >= iszjmin ) THEN
            iszjref = iszjtst
            inbjmax = inbjmax + 1
            inbj0(inbjmax) = ji
            iszj0(inbjmax) = iszjref
         ENDIF
      END DO
      IF( inbimax == 0 ) THEN
         WRITE(ctmp1,'(a,i2,a,i2)') '   mpp_ini bestpartition: Ni0glo (', Ni0glo, ') is too small to be used with nn_hls = ', nn_hls
         CALL ctl_stop( 'STOP', ctmp1 )
      ENDIF
      IF( inbjmax == 0 ) THEN
         WRITE(ctmp1,'(a,i2,a,i2)') '   mpp_ini bestpartition: Nj0glo (', Nj0glo, ') is too small to be used with nn_hls = ', nn_hls
         CALL ctl_stop( 'STOP', ctmp1 )
      ENDIF

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
            ENDIF
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
      iszij = jpiglo*jpjglo+1                                   ! default: larger than global domain
      DO WHILE( inbij <= inbijmax )                             ! if we did not reach the max of inbij1
         ii = MINLOC(iszij1, mask = inbij1 == inbij, dim = 1)   ! warning: send back the first occurence if multiple results
         IF ( iszij1(ii) < iszij ) THEN
            ii = MINLOC( iszi1+iszj1, mask = iszij1 == iszij1(ii) .AND. inbij1 == inbij, dim = 1)  ! select the smaller perimeter if multiple min
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
         ENDIF
         ji = isz0   ! initialization with the largest value
         ALLOCATE( llisOce(inbi0(ji), inbj0(ji)) )
         CALL mpp_is_ocean( llisOce )   ! Warning: must be call by all cores (call mpp_sum)
         inbijold = COUNT(llisOce)
         DEALLOCATE( llisOce )
         DO ji =isz0-1,1,-1
            ALLOCATE( llisOce(inbi0(ji), inbj0(ji)) )
            CALL mpp_is_ocean( llisOce )   ! Warning: must be call by all cores (call mpp_sum)
            inbij = COUNT(llisOce)
            DEALLOCATE( llisOce )
            IF(lwp .AND. inbij < inbijold) THEN
               WRITE(numout,'(a, i6, a, i6, a, f4.1, a, i9, a, i6, a, i6, a)')                                 &
                  &   'nb_cores oce: ', inbij, ', land domains excluded: ', inbi0(ji)*inbj0(ji) - inbij,       &
                  &   ' (', REAL(inbi0(ji)*inbj0(ji) - inbij,wp) / REAL(inbi0(ji)*inbj0(ji),wp) *100.,         &
                  &   '%), largest oce domain: ', iszi0(ji)*iszj0(ji), ' ( ', iszi0(ji),' x ', iszj0(ji), ' )'
               inbijold = inbij
            ENDIF
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
         ALLOCATE( llisOce(inbi0(ii), inbj0(ii)) )
         CALL mpp_is_ocean( llisOce )            ! must be done by all core
         inbij = COUNT(llisOce)
         DEALLOCATE( llisOce )
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
         CALL read_mask( 1, ijstr, Ni0glo, ijsz, lloce )
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


   SUBROUTINE mpp_is_ocean( ldIsOce )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE mpp_is_ocean  ***
      !!
      !! ** Purpose : Check for a mpi domain decomposition inbi x inbj which
      !!              subdomains, including 1 halo (even if nn_hls>1), contain
      !!              at least 1 ocean point.
      !!              We must indeed ensure that each subdomain that is a neighbour
      !!              of a land subdomain, has only land points on its boundary
      !!              (inside the inner subdomain) with the land subdomain.
      !!              This is needed to get the proper bondary conditions on
      !!              a subdomain with a closed boundary.
      !!
      !! ** Method  : read inbj strips (of length Ni0glo) of the land-sea mask
      !!----------------------------------------------------------------------
      LOGICAL, DIMENSION(:,:), INTENT(  out) ::   ldIsOce        ! .true. if a sub domain constains 1 ocean point
      !
      INTEGER :: idiv, iimax, ijmax, iarea
      INTEGER :: inbi, inbj, inx, iny, inry, isty
      INTEGER :: ji, jj, jn
      INTEGER, ALLOCATABLE, DIMENSION(:,:) ::   inboce           ! number oce oce pint in each mpi subdomain
      INTEGER, ALLOCATABLE, DIMENSION(:  ) ::   inboce_1d
      INTEGER, ALLOCATABLE, DIMENSION(:,:) ::   iimppt, ijpi
      INTEGER, ALLOCATABLE, DIMENSION(:,:) ::   ijmppt, ijpj
      LOGICAL, ALLOCATABLE, DIMENSION(:,:) ::   lloce            ! lloce(i,j) = .true. if the point (i,j) is ocean
      !!----------------------------------------------------------------------
      ! do nothing if there is no land-sea mask
      IF( numbot == -1 .AND. numbdy == -1 ) THEN
         ldIsOce(:,:) = .TRUE.
         RETURN
      ENDIF
      !
      inbi = SIZE( ldIsOce, dim = 1 )
      inbj = SIZE( ldIsOce, dim = 2 )
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
            CALL read_mask( 1, ijmppt(1,iarea) - 2 + isty, Ni0glo, inry, lloce(2:inx-1, isty:inry+isty-1) )   ! read the strip
            !
            IF( iarea == 1    ) THEN                                   ! the first line was not read
               IF( l_Jperio ) THEN                                     !   north-south periodocity
                  CALL read_mask( 1, Nj0glo, Ni0glo, 1, lloce(2:inx-1, 1) )   !   read the last line -> first line of lloce
               ELSE
                  lloce(2:inx-1,  1) = .FALSE.                         !   closed boundary
               ENDIF
            ENDIF
            IF( iarea == inbj ) THEN                                   ! the last line was not read
               IF( l_Jperio ) THEN                                     !   north-south periodocity
                  CALL read_mask( 1, 1, Ni0glo, 1, lloce(2:inx-1,iny) )   !      read the first line -> last line of lloce
               ELSEIF( c_NFtype == 'T' ) THEN                          !   north-pole folding T-pivot, T-point
                  lloce(2,iny) = lloce(2,iny-2)                        !      here we have 1 halo (even if nn_hls>1)
                  DO ji = 3,inx-1
                     lloce(ji,iny  ) = lloce(inx-ji+2,iny-2)           !      ok, we have at least 3 lines
                  END DO
                  DO ji = inx/2+2,inx-1
                     lloce(ji,iny-1) = lloce(inx-ji+2,iny-1)
                  END DO
               ELSEIF( c_NFtype == 'F' ) THEN                          !   north-pole folding F-pivot, T-point, 1 halo
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
            IF( l_Iperio ) THEN
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
      ldIsOce(:,:) = inboce(:,:) /= 0
      DEALLOCATE(inboce, inboce_1d)
      !
#if defined key_xios
      ! Only when using XIOS: XIOS does a domain decomposition only in bands (for IO performances).
      !                       XIOS is crashing if one of these bands contains only land-domains which have been suppressed.
      ! -> solution (before a fix of xios): force to keep at least one land-domain by band of mpi domains
      DO jj = 1, inbj
         IF( COUNT( ldIsOce(:,jj) ) == 0 )   ldIsOce(1,jj) = .TRUE.   ! for to keep 1st MPI domain in the row of domains
      END DO
#endif      
      !
   END SUBROUTINE mpp_is_ocean


   SUBROUTINE read_mask( kistr, kjstr, kicnt, kjcnt, ldoce )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE read_mask  ***
      !!
      !! ** Purpose : Read relevant bathymetric information in order to
      !!              provide a land/sea mask used for the elimination
      !!              of land domains, in an mpp computation.
      !!
      !! ** Method  : read stipe of size (Ni0glo,...)
      !!----------------------------------------------------------------------
      INTEGER                        , INTENT(in   ) ::   kistr, kjstr   ! starting i and j position of the reading
      INTEGER                        , INTENT(in   ) ::   kicnt, kjcnt   ! number of points to read in i and j directions
      LOGICAL, DIMENSION(kicnt,kjcnt), INTENT(  out) ::   ldoce          ! ldoce(i,j) = .true. if the point (i,j) is ocean
      !
      INTEGER                          ::   inumsave                     ! local logical unit
      REAL(wp), DIMENSION(kicnt,kjcnt) ::   zbot, zbdy
      !!----------------------------------------------------------------------
      !
      inumsave = numout   ;   numout = numnul   !   redirect all print to /dev/null
      !
      IF( numbot /= -1 ) THEN
         CALL iom_get( numbot, jpdom_unknown, 'bottom_level', zbot, kstart = (/kistr,kjstr/), kcount = (/kicnt, kjcnt/) )
      ELSE
         zbot(:,:) = 1._wp                      ! put a non-null value
      ENDIF
      !
      IF( numbdy /= -1 ) THEN                   ! Adjust with bdy_msk if it exists
         CALL iom_get ( numbdy, jpdom_unknown,     'bdy_msk', zbdy, kstart = (/kistr,kjstr/), kcount = (/kicnt, kjcnt/) )
         zbot(:,:) = zbot(:,:) * zbdy(:,:)
      ENDIF
      !
      ldoce(:,:) = NINT(zbot(:,:)) > 0
      numout = inumsave
      !
   END SUBROUTINE read_mask


   SUBROUTINE mpp_getnum( ldIsOce, kproc, kipos, kjpos )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE mpp_getnum  ***
      !!
      !! ** Purpose : give a number to each MPI subdomains (starting at 0)
      !!
      !! ** Method  : start from bottom left. First skip land subdomain, and finally use them if needed
      !!----------------------------------------------------------------------
      LOGICAL, DIMENSION(:,:), INTENT(in   ) ::   ldIsOce     ! F if land process
      INTEGER, DIMENSION(:,:), INTENT(  out) ::   kproc       ! subdomain number (-1 if not existing, starting at 0)
      INTEGER, DIMENSION(  :), INTENT(  out) ::   kipos       ! i-position of the subdomain (from 1 to jpni)
      INTEGER, DIMENSION(  :), INTENT(  out) ::   kjpos       ! j-position of the subdomain (from 1 to jpnj)
      !
      INTEGER :: ii, ij, jarea, iarea0
      INTEGER :: icont, i2add , ini, inj, inij
      !!----------------------------------------------------------------------
      !
      ini = SIZE(ldIsOce, dim = 1)
      inj = SIZE(ldIsOce, dim = 2)
      inij = SIZE(kipos)
      !
      ! specify which subdomains are oce subdomains; other are land subdomains
      kproc(:,:) = -1
      icont = -1
      DO jarea = 1, ini*inj
         iarea0 = jarea - 1
         ii = 1 + MOD(iarea0,ini)
         ij = 1 +     iarea0/ini
         IF( ldIsOce(ii,ij) ) THEN
            icont = icont + 1
            kproc(ii,ij) = icont
            kipos(icont+1) = ii
            kjpos(icont+1) = ij
         ENDIF
      END DO
      ! if needed add some land subdomains to reach inij active subdomains
      i2add = inij - COUNT( ldIsOce )
      DO jarea = 1, ini*inj
         iarea0 = jarea - 1
         ii = 1 + MOD(iarea0,ini)
         ij = 1 +     iarea0/ini
         IF( .NOT. ldIsOce(ii,ij) .AND. i2add > 0 ) THEN
            icont = icont + 1
            kproc(ii,ij) = icont
            kipos(icont+1) = ii
            kjpos(icont+1) = ij
            i2add = i2add - 1
         ENDIF
      END DO
      !
   END SUBROUTINE mpp_getnum


   SUBROUTINE init_excl_landpt
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE   ***
      !!
      !! ** Purpose : exclude exchanges which contain only land points
      !!
      !! ** Method  : if a send or receive buffer constains only land point we
      !!              flag off the corresponding communication
      !!              Warning: this selection depend on the halo size -> loop on halo size
      !!
      !!----------------------------------------------------------------------
      INTEGER ::   inumsave
      INTEGER ::   jh
      INTEGER ::   ipi, ipj
      INTEGER ::   iiwe, iiea, iist, iisz 
      INTEGER ::   ijso, ijno, ijst, ijsz 
      REAL(wp), DIMENSION(:,:), ALLOCATABLE ::   zmsk
      LOGICAL , DIMENSION(Ni_0,Nj_0,1)      ::   lloce
      !!----------------------------------------------------------------------
      !
      ! read the land-sea mask on the inner domain
      CALL read_mask( nimpp, njmpp, Ni_0, Nj_0, lloce(:,:,1) )
      !
      ! Here we look only at communications excluding the NP folding.
      !   --> we switch off lbcnfd at this stage (init_nfdcom called after init_excl_landpt)...
      l_IdoNFold = .FALSE.
      !
      ! WARNING, see also bestpartition: minimum subdomain size defined in bestpartition according to nn_hls.
      ! If, one day, we want to use local halos largers than nn_hls, we must replace nn_hls by n_hlsmax in bestpartition
      !
      DO jh = 1, MIN(nn_hls, n_hlsmax)   ! different halo size
         !
         ipi = Ni_0 + 2*jh   ! local domain size
         ipj = Nj_0 + 2*jh
         !
         ALLOCATE( zmsk(ipi,ipj) )
         zmsk(jh+1:jh+Ni_0,jh+1:jh+Nj_0) = REAL(COUNT(lloce, dim = 3), wp)   ! define inner domain -> need REAL to use lbclnk
         CALL lbc_lnk('mppini', zmsk, 'T', 1._wp, khls = jh)                 ! fill halos
         ! Beware, coastal F points can be used in the code -> we may need communications for these points F points even if tmask = 0
         ! -> the mask we must use here is equal to 1 as soon as one of the 4 neighbours is oce (sum of the mask, not multiplication)
         zmsk(jh+1:jh+Ni_0,jh+1:jh+Nj_0) = zmsk(jh+1:jh+Ni_0,jh+1  :jh+Nj_0  ) + zmsk(jh+1+1:jh+Ni_0+1,jh+1  :jh+Nj_0  )   &
            &                            + zmsk(jh+1:jh+Ni_0,jh+1+1:jh+Nj_0+1) + zmsk(jh+1+1:jh+Ni_0+1,jh+1+1:jh+Nj_0+1)
         CALL lbc_lnk('mppini', zmsk, 'T', 1._wp, khls = jh)                 ! fill halos again!
         !        
         iiwe = jh   ;   iiea = Ni_0   ! bottom-left corner - 1 of the sent data
         ijso = jh   ;   ijno = Nj_0
         IF( nn_comm == 1 ) THEN 
            iist =  0   ;   iisz = ipi
            ijst = jh   ;   ijsz = Nj_0
         ELSE
            iist = jh   ;   iisz = Ni_0
            ijst = jh   ;   ijsz = Nj_0
         ENDIF
IF( nn_comm == 1 ) THEN       ! SM: NOT WORKING FOR NEIGHBOURHOOD COLLECTIVE COMMUNICATIONS, I DON'T KNOW WHY... 
         ! do not send if we send only land points
         IF( NINT(SUM( zmsk(iiwe+1:iiwe+jh  ,ijst+1:ijst+ijsz) )) == 0 )   mpiSnei(jh,jpwe) = -1
         IF( NINT(SUM( zmsk(iiea+1:iiea+jh  ,ijst+1:ijst+ijsz) )) == 0 )   mpiSnei(jh,jpea) = -1
         IF( NINT(SUM( zmsk(iist+1:iist+iisz,ijso+1:ijso+jh  ) )) == 0 )   mpiSnei(jh,jpso) = -1
         IF( NINT(SUM( zmsk(iist+1:iist+iisz,ijno+1:ijno+jh  ) )) == 0 )   mpiSnei(jh,jpno) = -1
         IF( NINT(SUM( zmsk(iiwe+1:iiwe+jh  ,ijso+1:ijso+jh  ) )) == 0 )   mpiSnei(jh,jpsw) = -1
         IF( NINT(SUM( zmsk(iiea+1:iiea+jh  ,ijso+1:ijso+jh  ) )) == 0 )   mpiSnei(jh,jpse) = -1
         IF( NINT(SUM( zmsk(iiwe+1:iiwe+jh  ,ijno+1:ijno+jh  ) )) == 0 )   mpiSnei(jh,jpnw) = -1
         IF( NINT(SUM( zmsk(iiea+1:iiea+jh  ,ijno+1:ijno+jh  ) )) == 0 )   mpiSnei(jh,jpne) = -1
         !
         iiwe = iiwe-jh   ;   iiea = iiea+jh   ! bottom-left corner - 1 of the received data
         ijso = ijso-jh   ;   ijno = ijno+jh
         ! do not send if we send only land points
         IF( NINT(SUM( zmsk(iiwe+1:iiwe+jh  ,ijst+1:ijst+ijsz) )) == 0 )   mpiRnei(jh,jpwe) = -1
         IF( NINT(SUM( zmsk(iiea+1:iiea+jh  ,ijst+1:ijst+ijsz) )) == 0 )   mpiRnei(jh,jpea) = -1
         IF( NINT(SUM( zmsk(iist+1:iist+iisz,ijso+1:ijso+jh  ) )) == 0 )   mpiRnei(jh,jpso) = -1
         IF( NINT(SUM( zmsk(iist+1:iist+iisz,ijno+1:ijno+jh  ) )) == 0 )   mpiRnei(jh,jpno) = -1
         IF( NINT(SUM( zmsk(iiwe+1:iiwe+jh  ,ijso+1:ijso+jh  ) )) == 0 )   mpiRnei(jh,jpsw) = -1
         IF( NINT(SUM( zmsk(iiea+1:iiea+jh  ,ijso+1:ijso+jh  ) )) == 0 )   mpiRnei(jh,jpse) = -1
         IF( NINT(SUM( zmsk(iiwe+1:iiwe+jh  ,ijno+1:ijno+jh  ) )) == 0 )   mpiRnei(jh,jpnw) = -1
         IF( NINT(SUM( zmsk(iiea+1:iiea+jh  ,ijno+1:ijno+jh  ) )) == 0 )   mpiRnei(jh,jpne) = -1
ENDIF
         !
         ! Specific (and rare) problem in corner treatment because we do 1st West-East comm, next South-North comm
         IF( nn_comm == 1 ) THEN
            IF( mpiSnei(jh,jpwe) > -1 )   mpiSnei(jh, (/jpsw,jpnw/) ) = -1   ! SW and NW corners already sent through West nei
            IF( mpiSnei(jh,jpea) > -1 )   mpiSnei(jh, (/jpse,jpne/) ) = -1   ! SE and NE corners already sent through East nei
            IF( mpiRnei(jh,jpso) > -1 )   mpiRnei(jh, (/jpsw,jpse/) ) = -1   ! SW and SE corners will be received through South nei
            IF( mpiRnei(jh,jpno) > -1 )   mpiRnei(jh, (/jpnw,jpne/) ) = -1   ! NW and NE corners will be received through North nei
        ENDIF
         !
         DEALLOCATE( zmsk )
         !
         CALL mpp_ini_nc(jh)    ! Initialize/Update communicator for neighbourhood collective communications
         !
      END DO

   END SUBROUTINE init_excl_landpt


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
      CALL flio_dom_set ( jpnij, narea-1, idid, iglo, iloc, iabsf, iabsl, ihals, ihale, 'BOX', nidom)
      !
   END SUBROUTINE init_ioipsl


   SUBROUTINE init_nfdcom( ldwrtlay, knum )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE  init_nfdcom  ***
      !! ** Purpose :   Setup for north fold exchanges with explicit
      !!                point-to-point messaging
      !!
      !! ** Method :   Initialization of the northern neighbours lists.
      !!----------------------------------------------------------------------
      !!    1.0  ! 2011-10  (A. C. Coward, NOCS & J. Donners, PRACE)
      !!    2.0  ! 2013-06 Setup avoiding MPI communication (I. Epicoco, S. Mocavero, CMCC)
      !!    3.0  ! 2021-09 complete rewrite using informations from gather north fold
      !!----------------------------------------------------------------------
      LOGICAL, INTENT(in   ) ::   ldwrtlay   ! true if additional prints in layout.dat
      INTEGER, INTENT(in   ) ::   knum       ! layout.dat unit
      !
      REAL(wp), DIMENSION(jpi,jpj,2,4) ::   zinfo
      INTEGER , DIMENSION(10) ::   irknei ! too many elements but safe...
      INTEGER                 ::   ji, jj, jg, jn   ! dummy loop indices
      INTEGER                 ::   iitmp
      LOGICAL                 ::   lnew
      !!----------------------------------------------------------------------
      !
      IF (lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) '   ==>>>   North fold boundary prepared for jpni >1'
      ENDIF
      !
      CALL mpp_ini_northgather   ! we need to init the nfd with gathering in all cases as it is used to define the no-gather case
      !
      IF(ldwrtlay) THEN      ! additional prints in layout.dat
         WRITE(knum,*)
         WRITE(knum,*)
         WRITE(knum,*) 'Number of subdomains located along the north fold : ', ndim_rank_north
         WRITE(knum,*) 'Rank of the subdomains located along the north fold : ', ndim_rank_north
         DO jn = 1, ndim_rank_north, 5
            WRITE(knum,*) nrank_north( jn:MINVAL( (/jn+4,ndim_rank_north/) ) )
         END DO
      ENDIF
      
      nfd_nbnei = 0   ! defaul def (useless?)
      IF( ln_nnogather ) THEN
         !
         ! Use the "gather nfd" to know how to do the nfd: for ji point, which process send data from which of its ji-index?
         ! Note that nfd is perfectly symetric: I receive data from X <=> I send data to X  (-> no deadlock)
         !
         zinfo(:,:,:,:) = HUGE(0._wp)   ! default def to make sure we don't use the halos
         DO jg = 1, 4   ! grid type: T, U, V, F
            DO jj = nn_hls+1, jpj-nn_hls                ! inner domain (warning do_loop_substitute not yet defined)
               DO ji = nn_hls+1, jpi-nn_hls             ! inner domain (warning do_loop_substitute not yet defined)
                  zinfo(ji,jj,1,jg) = REAL(narea, wp)   ! mpi_rank + 1 (as default lbc_lnk fill is 0
                  zinfo(ji,jj,2,jg) = REAL(ji, wp)      ! ji of this proc
               END DO
            END DO
         END DO
         !
         ln_nnogather = .FALSE.   ! force "classical" North pole folding to fill all halos -> should be no more HUGE values...
         CALL lbc_lnk( 'mppini', zinfo(:,:,:,1), 'T', 1._wp )   ! Do 4 calls instead of 1 to save memory as the nogather version
         CALL lbc_lnk( 'mppini', zinfo(:,:,:,2), 'U', 1._wp )   ! creates buffer arrays with jpiglo as the first dimension
         CALL lbc_lnk( 'mppini', zinfo(:,:,:,3), 'V', 1._wp )   ! 
         CALL lbc_lnk( 'mppini', zinfo(:,:,:,4), 'F', 1._wp )   ! 
         ln_nnogather = .TRUE.
         
         IF( l_IdoNFold ) THEN   ! only the procs involed in the NFD must take care of this
            
            ALLOCATE( nfd_rksnd(jpi,4), nfd_jisnd(jpi,4) )          ! neighbour rand and remote ji-index for each grid (T, U, V, F)
            nfd_rksnd(:,:) = NINT( zinfo(:, jpj, 1, :) ) - 1        ! neighbour MPI rank
            nfd_jisnd(:,:) = NINT( zinfo(:, jpj, 2, :) ) - nn_hls   ! neighbour ji index (shifted as we don't send the halos)
            WHERE( nfd_rksnd == -1 )   nfd_jisnd = 1                ! use ji=1 if no neighbour, see mpp_nfd_generic.h90
            
            nfd_nbnei = 1                ! Number of neighbour sending data for the nfd. We have at least 1 neighbour!
            irknei(1) = nfd_rksnd(1,1)   ! which is the 1st one (I can be neighbour of myself, exclude land-proc are also ok)
            DO jg = 1, 4
               DO ji = 1, jpi     ! we must be able to fill the full line including halos
                  lnew = .TRUE.   ! new neighbour?
                  DO jn = 1, nfd_nbnei
                     IF( irknei(jn) == nfd_rksnd(ji,jg) )   lnew = .FALSE.   ! already found
                  END DO
                  IF( lnew ) THEN
                     jn = nfd_nbnei + 1
                     nfd_nbnei = jn
                     irknei(jn) = nfd_rksnd(ji,jg)
                  ENDIF
               END DO
            END DO
            
            ALLOCATE( nfd_rknei(nfd_nbnei) )
            nfd_rknei(:) = irknei(1:nfd_nbnei)
            ! re-number nfd_rksnd according to the indexes of nfd_rknei
            DO jg = 1, 4
               DO ji = 1, jpi
                  iitmp = nfd_rksnd(ji,jg)   ! must store a copy of nfd_rksnd(ji,jg) to make sure we don't change it twice
                  DO jn = 1, nfd_nbnei
                     IF( iitmp == nfd_rknei(jn) )   nfd_rksnd(ji,jg) = jn
                  END DO
               END DO
            END DO
            
            IF( ldwrtlay ) THEN
               WRITE(knum,*)
               WRITE(knum,*) 'north fold exchanges with explicit point-to-point messaging :'
               WRITE(knum,*) '   number of neighbours for the NF: nfd_nbnei : ', nfd_nbnei
               IF( nfd_nbnei > 0 )   WRITE(knum,*) '   neighbours MPI ranks                       : ', nfd_rknei
            ENDIF
            
         ENDIF   ! l_IdoNFold
         !
      ENDIF   ! ln_nnogather
      !
   END SUBROUTINE init_nfdcom


   SUBROUTINE init_doloop
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE init_doloop  ***
      !!
      !! ** Purpose :   set the starting/ending indices of DO-loop
      !!              These indices are used in do_loop_substitute.h90
      !!----------------------------------------------------------------------
      !
      Nis0 =   1+nn_hls
      Njs0 =   1+nn_hls
      Nie0 = jpi-nn_hls
      Nje0 = jpj-nn_hls
      !
      Ni_0 = Nie0 - Nis0 + 1
      Nj_0 = Nje0 - Njs0 + 1
      !
      jpkm1 = jpk-1                             !   "           "
      !
   END SUBROUTINE init_doloop

   
   SUBROUTINE init_locglo
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE init_locglo  ***
      !!
      !! ** Purpose :   initialization of global domain <--> local domain indices
      !!
      !! ** Method  :
      !!
      !! ** Action  : - mig , mjg : local  domain indices ==> global domain, including halos, indices
      !!              - mig0, mjg0: local  domain indices ==> global domain, excluding halos, indices
      !!              - mi0 , mi1 : global domain indices ==> local  domain indices
      !!              - mj0 , mj1   (if global point not in the local domain ==> mi0>mi1 and/or mj0>mj1)
      !!----------------------------------------------------------------------
      INTEGER ::   ji, jj   ! dummy loop argument
      !!----------------------------------------------------------------------
      !
      ALLOCATE( mig(jpi), mjg(jpj), mig0(jpi), mjg0(jpj) )
      ALLOCATE( mi0(jpiglo), mi1(jpiglo), mj0(jpjglo), mj1(jpjglo) )
      !
      DO ji = 1, jpi                 ! local domain indices ==> global domain indices, including halos
        mig(ji) = ji + nimpp - 1
      END DO
      DO jj = 1, jpj
        mjg(jj) = jj + njmpp - 1
      END DO
      !                              ! local domain indices ==> global domain indices, excluding halos
      !
      mig0(:) = mig(:) - nn_hls
      mjg0(:) = mjg(:) - nn_hls
      !                              ! global domain, including halos, indices ==> local domain indices
      !                                   ! (return (m.0,m.1)=(1,0) if data domain gridpoint is to the west/south of the
      !                                   ! local domain, or (m.0,m.1)=(jp.+1,jp.) to the east/north of local domain.
      DO ji = 1, jpiglo
        mi0(ji) = MAX( 1 , MIN( ji - nimpp + 1, jpi+1 ) )
        mi1(ji) = MAX( 0 , MIN( ji - nimpp + 1, jpi   ) )
      END DO
      DO jj = 1, jpjglo
        mj0(jj) = MAX( 1 , MIN( jj - njmpp + 1, jpj+1 ) )
        mj1(jj) = MAX( 0 , MIN( jj - njmpp + 1, jpj   ) )
      END DO
      !
   END SUBROUTINE init_locglo
   
   !!======================================================================
END MODULE mppini
