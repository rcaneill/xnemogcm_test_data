MODULE prtctl
   !!======================================================================
   !!                       ***  MODULE prtctl   ***
   !! Ocean system : print all SUM trends for each processor domain
   !!======================================================================
   !! History :  9.0  !  05-07  (C. Talandier) original code
   !!            3.4  !  11-11  (C. Harris) decomposition changes for running with CICE
   !!----------------------------------------------------------------------
   USE dom_oce          ! ocean space and time domain variables
   USE domutl, ONLY : is_tile
   USE in_out_manager   ! I/O manager
   USE mppini           ! distributed memory computing
   USE lib_mpp          ! distributed memory computing

   IMPLICIT NONE
   PRIVATE

   INTEGER , DIMENSION(  :), ALLOCATABLE ::   numprt_oce, numprt_top
   INTEGER , DIMENSION(  :), ALLOCATABLE ::   nall_ictls, nall_ictle   ! first, last indoor index for each i-domain
   INTEGER , DIMENSION(  :), ALLOCATABLE ::   nall_jctls, nall_jctle   ! first, last indoor index for each j-domain
   REAL(wp), DIMENSION(  :), ALLOCATABLE ::   t_ctl , s_ctl            ! previous tracer trend values
   REAL(wp), DIMENSION(  :), ALLOCATABLE ::   u_ctl , v_ctl            ! previous velocity trend values
   REAL(wp), DIMENSION(:,:), ALLOCATABLE ::   tra_ctl                  ! previous top trend values
   !
   PUBLIC prt_ctl         ! called by all subroutines
   PUBLIC prt_ctl_info    ! called by all subroutines
   PUBLIC prt_ctl_init    ! called by nemogcm.F90 and prt_ctl_trc_init

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: prtctl.F90 15148 2021-07-27 09:40:32Z gsamson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE prt_ctl (tab2d_1, tab3d_1, tab4d_1, tab2d_2, tab3d_2, mask1, mask2,   &
      &                 clinfo, clinfo1, clinfo2, clinfo3, kdim )
      !!
      REAL(wp),         DIMENSION(:,:)    , INTENT(in), OPTIONAL ::   tab2d_1
      REAL(wp),         DIMENSION(:,:,:)  , INTENT(in), OPTIONAL ::   tab3d_1
      REAL(wp),         DIMENSION(:,:,:,:), INTENT(in), OPTIONAL ::   tab4d_1
      REAL(wp),         DIMENSION(:,:)    , INTENT(in), OPTIONAL ::   tab2d_2
      REAL(wp),         DIMENSION(:,:,:)  , INTENT(in), OPTIONAL ::   tab3d_2
      REAL(wp),         DIMENSION(:,:,:)  , INTENT(in), OPTIONAL ::   mask1
      REAL(wp),         DIMENSION(:,:,:)  , INTENT(in), OPTIONAL ::   mask2
      CHARACTER(len=*), DIMENSION(:)      , INTENT(in), OPTIONAL ::   clinfo    ! information about the tab3d array
      CHARACTER(len=*)                    , INTENT(in), OPTIONAL ::   clinfo1
      CHARACTER(len=*)                    , INTENT(in), OPTIONAL ::   clinfo2
      CHARACTER(len=*)                    , INTENT(in), OPTIONAL ::   clinfo3
      INTEGER                             , INTENT(in), OPTIONAL ::   kdim
      !
      IF(     PRESENT(tab2d_2) ) THEN
         CALL prt_ctl_t(ktab2d_1 = is_tile(tab2d_1), ktab3d_1 = 0, ktab4d_1 = 0, ktab2d_2 = is_tile(tab2d_2), ktab3d_2 = 0,   &
            &            tab2d_1 =    REAL(tab2d_1, 2*wp),                        tab2d_2 =    REAL(tab2d_2, 2*wp),           &
            &           mask1 = mask1, mask2 = mask2, &
            &           clinfo = clinfo, clinfo1 = clinfo1, clinfo2 = clinfo2, clinfo3 = clinfo3 )
      ELSEIF( PRESENT(tab3d_2) ) THEN     
         CALL prt_ctl_t(ktab2d_1 = 0, ktab3d_1 = is_tile(tab3d_1), ktab4d_1 = 0, ktab2d_2 = 0, ktab3d_2 = is_tile(tab3d_2),       &
            &                          tab3d_1 = REAL(tab3d_1, 2*wp),                           tab3d_2 =    REAL(tab3d_2, 2*wp), &
            &           mask1 = mask1, mask2 = mask2, &
            &           clinfo = clinfo, clinfo1 = clinfo1, clinfo2 = clinfo2, clinfo3 = clinfo3, kdim = kdim )
      ELSEIF( PRESENT(tab2d_1) ) THEN     
         CALL prt_ctl_t(ktab2d_1 = is_tile(tab2d_1), ktab3d_1 = 0, ktab4d_1 = 0, ktab2d_2 = 0, ktab3d_2 = 0,   &
            &           tab2d_1 = REAL(tab2d_1,2*wp),  &
            &           mask1 = mask1,  &
            &           clinfo = clinfo, clinfo1 = clinfo1, clinfo3 = clinfo3 )
      ELSEIF( PRESENT(tab3d_1) ) THEN     
         CALL prt_ctl_t(ktab2d_1 = 0, ktab3d_1 = is_tile(tab3d_1), ktab4d_1 = 0, ktab2d_2 = 0, ktab3d_2 = 0,   &
            &                          tab3d_1 =    REAL(tab3d_1, 2*wp),  &
            &           mask1 = mask1,  &
            &           clinfo = clinfo, clinfo1 = clinfo1, clinfo3 = clinfo3, kdim = kdim )
      ELSEIF( PRESENT(tab4d_1) ) THEN     
         CALL prt_ctl_t(ktab2d_1 = 0, ktab3d_1 = 0, ktab4d_1 = is_tile(tab4d_1), ktab2d_2 = 0, ktab3d_2 = 0,   &
            &                                        tab4d_1 =    REAL(tab4d_1, 2*wp),  &
            &           mask1 = mask1,  &
            &           clinfo = clinfo, clinfo1 = clinfo1, clinfo3 = clinfo3, kdim = kdim )
      ENDIF

   END SUBROUTINE prt_ctl


   SUBROUTINE prt_ctl_t (tab2d_1, ktab2d_1, tab3d_1, ktab3d_1, tab4d_1, ktab4d_1, tab2d_2, ktab2d_2, tab3d_2, ktab3d_2,  &
      &                  mask1, mask2, clinfo, clinfo1, clinfo2, clinfo3, kdim )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE prt_ctl  ***
      !!
      !! ** Purpose : - print sum control of 2D or 3D arrays over the same area
      !!                in mono and mpp case. This way can be usefull when
      !!                debugging a new parametrization in mono or mpp.
      !!
      !! ** Method  : 2 possibilities exist when setting the sn_cfctl%prtctl parameter to
      !!                .true. in the ocean namelist:
      !!              - to debug a MPI run .vs. a mono-processor one;
      !!                the control print will be done over each sub-domain.
      !!                The nictl[se] and njctl[se] parameters in the namelist must
      !!                be set to zero and [ij]splt to the corresponding splitted
      !!                domain in MPI along respectively i-, j- directions.
      !!              - to debug a mono-processor run over the whole domain/a specific area;
      !!                in the first case the nictl[se] and njctl[se] parameters must be set
      !!                to zero else to the indices of the area to be controled. In both cases
      !!                isplt and jsplt must be set to 1.
      !!              - All arguments of the above calling sequence are optional so their
      !!                name must be explicitly typed if used. For instance if the 3D
      !!                array tn(:,:,:) must be passed through the prt_ctl subroutine,
      !!                it must look like: CALL prt_ctl(tab3d_1=tn).
      !!
      !!                    tab2d_1 : first 2D array
      !!                    tab3d_1 : first 3D array
      !!                    tab4d_1 : first 4D array
      !!                    mask1   : mask (3D) to apply to the tab[23]d_1 array
      !!                    clinfo1 : information about the tab[23]d_1 array
      !!                    tab2d_2 : second 2D array
      !!                    tab3d_2 : second 3D array
      !!                    mask2   : mask (3D) to apply to the tab[23]d_2 array
      !!                    clinfo2 : information about the tab[23]d_2 array
      !!                    kdim    : k- direction for 3D arrays
      !!                    clinfo3 : additional information
      !!----------------------------------------------------------------------
      INTEGER                             , INTENT(in)           ::   ktab2d_1, ktab3d_1, ktab4d_1, ktab2d_2, ktab3d_2
      REAL(2*wp),         DIMENSION(A2D_T(ktab2d_1))    , INTENT(in), OPTIONAL ::   tab2d_1
      REAL(2*wp),         DIMENSION(A2D_T(ktab3d_1),:)  , INTENT(in), OPTIONAL ::   tab3d_1
      REAL(2*wp),         DIMENSION(A2D_T(ktab4d_1),:,:), INTENT(in), OPTIONAL ::   tab4d_1
      REAL(2*wp),         DIMENSION(A2D_T(ktab2d_2))    , INTENT(in), OPTIONAL ::   tab2d_2
      REAL(2*wp),         DIMENSION(A2D_T(ktab3d_2),:)  , INTENT(in), OPTIONAL ::   tab3d_2
      REAL(wp),           DIMENSION(:,:,:)  , INTENT(in), OPTIONAL ::   mask1
      REAL(wp),           DIMENSION(:,:,:)  , INTENT(in), OPTIONAL ::   mask2
      CHARACTER(len=*), DIMENSION(:)      , INTENT(in), OPTIONAL ::   clinfo    ! information about the tab3d array
      CHARACTER(len=*)                    , INTENT(in), OPTIONAL ::   clinfo1
      CHARACTER(len=*)                    , INTENT(in), OPTIONAL ::   clinfo2
      CHARACTER(len=*)                    , INTENT(in), OPTIONAL ::   clinfo3
      INTEGER                             , INTENT(in), OPTIONAL ::   kdim
      !
      CHARACTER(len=30) :: cl1, cl2
      CHARACTER(len=6) :: clfmt
      INTEGER ::  jn, jl, kdir
      INTEGER ::  iis, iie, jjs, jje
      INTEGER ::  itra, inum
      REAL(2*wp) :: zsum1, zsum2, zvctl1, zvctl2
      !!----------------------------------------------------------------------
      !
      ! Arrays, scalars initialization
      cl1  = ''
      cl2  = ''
      kdir = jpkm1
      itra = 1

      ! Control of optional arguments
      IF( PRESENT(clinfo1) )   cl1  = clinfo1
      IF( PRESENT(clinfo2) )   cl2  = clinfo2
      IF( PRESENT(kdim)    )   kdir = kdim
      IF( PRESENT(tab4d_1) )   itra = SIZE(tab4d_1,dim=4)

      IF( wp == sp )   clfmt = 'D23.16'   ! 16 significant numbers
      IF( wp == dp )   clfmt = 'D41.34'   ! 34 significant numbers
      
      ! Loop over each sub-domain, i.e. the total number of processors ijsplt
      DO jl = 1, SIZE(nall_ictls)

         ! define shoter names...
         iis = MAX( nall_ictls(jl), ntsi )
         iie = MIN( nall_ictle(jl), ntei )
         jjs = MAX( nall_jctls(jl), ntsj )
         jje = MIN( nall_jctle(jl), ntej )

         IF( PRESENT(clinfo) ) THEN   ;   inum = numprt_top(jl)
         ELSE                         ;   inum = numprt_oce(jl)
         ENDIF

         ! Compute the sum control only where the tile domain and control print area overlap
         IF( iie >= iis .AND. jje >= jjs ) THEN
            DO jn = 1, itra

               IF( PRESENT(clinfo3) ) THEN
                  IF    ( clinfo3 == 'tra-ta' )   THEN
                     zvctl1 = t_ctl(jl)
                  ELSEIF( clinfo3 == 'tra'    )   THEN
                     zvctl1 = t_ctl(jl)
                     zvctl2 = s_ctl(jl)
                  ELSEIF( clinfo3 == 'dyn'    )   THEN
                     zvctl1 = u_ctl(jl)
                     zvctl2 = v_ctl(jl)
                  ELSE
                     zvctl1 = tra_ctl(jn,jl)
                  ENDIF
               ENDIF

               ! 2D arrays
               IF( PRESENT(tab2d_1) ) THEN
                  IF( PRESENT(mask1) ) THEN   ;   zsum1 = SUM( tab2d_1(iis:iie,jjs:jje) * mask1(iis:iie,jjs:jje,1) )
                  ELSE                        ;   zsum1 = SUM( tab2d_1(iis:iie,jjs:jje)                            )
                  ENDIF
               ENDIF
               IF( PRESENT(tab2d_2) ) THEN
                  IF( PRESENT(mask2) ) THEN   ;   zsum2 = SUM( tab2d_2(iis:iie,jjs:jje) * mask2(iis:iie,jjs:jje,1) )
                  ELSE                        ;   zsum2 = SUM( tab2d_2(iis:iie,jjs:jje)                            )
                  ENDIF
               ENDIF

               ! 3D arrays
               IF( PRESENT(tab3d_1) ) THEN
                  IF( PRESENT(mask1) ) THEN   ;   zsum1 = SUM( tab3d_1(iis:iie,jjs:jje,1:kdir) * mask1(iis:iie,jjs:jje,1:kdir) )
                  ELSE                        ;   zsum1 = SUM( tab3d_1(iis:iie,jjs:jje,1:kdir)                                 )
                  ENDIF
               ENDIF
               IF( PRESENT(tab3d_2) ) THEN
                  IF( PRESENT(mask2) ) THEN   ;   zsum2 = SUM( tab3d_2(iis:iie,jjs:jje,1:kdir) * mask2(iis:iie,jjs:jje,1:kdir) )
                  ELSE                        ;   zsum2 = SUM( tab3d_2(iis:iie,jjs:jje,1:kdir)                                 )
                  ENDIF
               ENDIF

               ! 4D arrays
               IF( PRESENT(tab4d_1) ) THEN
                  IF( PRESENT(mask1) ) THEN   ;   zsum1 = SUM( tab4d_1(iis:iie,jjs:jje,1:kdir,jn) * mask1(iis:iie,jjs:jje,1:kdir) )
                  ELSE                        ;   zsum1 = SUM( tab4d_1(iis:iie,jjs:jje,1:kdir,jn)                                 )
                  ENDIF
               ENDIF

               ! Print the result
               IF( PRESENT(clinfo ) )   cl1  = clinfo(jn)
               IF( PRESENT(clinfo3) )   THEN
                  !
                  IF( PRESENT(tab2d_2) .OR. PRESENT(tab3d_2) ) THEN
                     WRITE(inum, "(3x,a,' : ',"//clfmt//",3x,a,' : ',"//clfmt//")") cl1, zsum1 - zvctl1, cl2, zsum2 - zvctl2
                  ELSE
                     WRITE(inum, "(3x,a,' : ',"//clfmt//"                       )") cl1, zsum1 - zvctl1
                  ENDIF
                  !
                  SELECT CASE( clinfo3 )
                  CASE ( 'tra-ta' )
                     t_ctl(jl) = zsum1
                  CASE ( 'tra' )
                     t_ctl(jl) = zsum1
                     s_ctl(jl) = zsum2
                  CASE ( 'dyn' )
                     u_ctl(jl) = zsum1
                     v_ctl(jl) = zsum2
                  CASE default
                     tra_ctl(jn,jl) = zsum1
                  END SELECT
               ELSEIF ( PRESENT(tab2d_2) .OR. PRESENT(tab3d_2) )   THEN
                  WRITE(inum, "(3x,a,' : ',"//clfmt//",3x,a,' : ',"//clfmt//")") cl1, zsum1, cl2, zsum2
               ELSE
                  WRITE(inum, "(3x,a,' : ',"//clfmt//"                       )") cl1, zsum1
               ENDIF

            END DO
         ENDIF
         IF( jpnij == 1 ) CALL FLUSH(inum)
      END DO
      !
   END SUBROUTINE prt_ctl_t


   SUBROUTINE prt_ctl_info (clinfo, ivar, cdcomp )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE prt_ctl_info  ***
      !!
      !! ** Purpose : - print information without any computation
      !!
      !! ** Action  : - input arguments
      !!                    clinfo : information about the ivar
      !!                    ivar   : value to print
      !!----------------------------------------------------------------------
      CHARACTER(len=*),           INTENT(in) ::   clinfo
      INTEGER         , OPTIONAL, INTENT(in) ::   ivar
      CHARACTER(len=3), OPTIONAL, INTENT(in) ::   cdcomp   ! only 'top' is accepted
      !
      CHARACTER(len=3) :: clcomp
      INTEGER ::  jl, inum
      !!----------------------------------------------------------------------
      !
      IF( PRESENT(cdcomp) ) THEN   ;   clcomp = cdcomp
      ELSE                         ;   clcomp = 'oce'
      ENDIF
      !
      DO jl = 1, SIZE(nall_ictls)
         !
         IF( clcomp == 'oce' )   inum = numprt_oce(jl)
         IF( clcomp == 'top' )   inum = numprt_top(jl)
         !
         IF ( PRESENT(ivar) ) THEN   ;   WRITE(inum,*) clinfo, ivar
         ELSE                        ;   WRITE(inum,*) clinfo
         ENDIF
         !
      END DO
      !
   END SUBROUTINE prt_ctl_info


   SUBROUTINE prt_ctl_init( cdcomp, kntra )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE prt_ctl_init  ***
      !!
      !! ** Purpose :   open ASCII files & compute indices
      !!----------------------------------------------------------------------
      CHARACTER(len=3), OPTIONAL, INTENT(in   ) ::   cdcomp   ! only 'top' is accepted
      INTEGER         , OPTIONAL, INTENT(in   ) ::   kntra    ! only for 'top': number of tracers
      !
      INTEGER ::   ji, jj, jl
      INTEGER ::   inum, idg, idg2
      INTEGER ::   ijsplt, iimax, ijmax
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::    iimppt, ijmppt, ijpi, ijpj, iproc
      INTEGER, DIMENSION(  :), ALLOCATABLE ::     iipos,  ijpos
      LOGICAL, DIMENSION(:,:), ALLOCATABLE ::   llisoce
      CHARACTER(len=64) :: clfile_out
      CHARACTER(LEN=64) :: clfmt, clfmt2, clfmt3, clfmt4
      CHARACTER(len=32) :: clname, cl_run
      CHARACTER(len= 3) :: clcomp
      !!----------------------------------------------------------------------
      !
      clname = 'output'
      IF( PRESENT(cdcomp) ) THEN
         clname = TRIM(clname)//'.'//TRIM(cdcomp)
         clcomp = cdcomp
      ELSE
         clcomp = 'oce'
      ENDIF
      !
      IF( jpnij > 1 ) THEN   ! MULTI processor run
         cl_run = 'MULTI processor run'
         idg = MAX( INT(LOG10(REAL(MAX(1,jpnij-1),wp))) + 1, 4 )    ! how many digits to we need to write? min=4, max=9
         WRITE(clfmt, "('(a,i', i1, '.', i1, ')')") idg, idg        ! '(a,ix.x)'
         WRITE(clfile_out,clfmt) 'mpp.'//trim(clname)//'_', narea - 1
         ijsplt = 1
      ELSE                   ! MONO processor run
         cl_run = 'MONO processor run '
         IF(lwp) THEN                  ! control print
            WRITE(numout,*)
            WRITE(numout,*) 'prt_ctl_init: sn_cfctl%l_prtctl parameters'
            WRITE(numout,*) '~~~~~~~~~~~~~'
         ENDIF
         IF( nn_ictls+nn_ictle+nn_jctls+nn_jctle == 0 )   THEN    ! print control done over the default area
            nn_isplt = MAX(1, nn_isplt)            ! number of processors following i-direction
            nn_jsplt = MAX(1, nn_jsplt)            ! number of processors following j-direction
            ijsplt = nn_isplt * nn_jsplt           ! total number of processors ijsplt
            IF( ijsplt == 1 )   CALL ctl_warn( 'nn_isplt & nn_jsplt are equal to 1 -> control sum done over the whole domain' )
            IF(lwp) WRITE(numout,*) '      number of proc. following i     nn_isplt   = ', nn_isplt
            IF(lwp) WRITE(numout,*) '      number of proc. following j     nn_jsplt   = ', nn_jsplt
            idg = MAX( INT(LOG10(REAL(MAX(1,ijsplt-1),wp))) + 1, 4 )    ! how many digits to we need to write? min=4, max=9
            WRITE(clfmt, "('(a,i', i1, '.', i1, ')')") idg, idg         ! '(a,ix.x)'
            IF( ijsplt == 1 ) WRITE(clfile_out,clfmt) 'mono.'//trim(clname)//'_', 0
         ELSE                                             ! print control done over a specific  area
            ijsplt = 1
            IF( nn_ictls < 1 .OR. nn_ictls > Ni0glo )   THEN
               CALL ctl_warn( '          - nictls must be 1<=nictls>=Ni0glo, it is forced to 1' )
               nn_ictls = 1
            ENDIF
            IF( nn_ictle < 1 .OR. nn_ictle > Ni0glo )   THEN
               CALL ctl_warn( '          - nictle must be 1<=nictle>=Ni0glo, it is forced to Ni0glo' )
               nn_ictle = Ni0glo
            ENDIF
            IF( nn_jctls < 1 .OR. nn_jctls > Nj0glo )   THEN
               CALL ctl_warn( '          - njctls must be 1<=njctls>=Nj0glo, it is forced to 1' )
               nn_jctls = 1
            ENDIF
            IF( nn_jctle < 1 .OR. nn_jctle > Nj0glo )   THEN
               CALL ctl_warn( '          - njctle must be 1<=njctle>=Nj0glo, it is forced to Nj0glo' )
               nn_jctle = Nj0glo
            ENDIF
            WRITE(numout,*) '      Start i indice for SUM control  nn_ictls   = ', nn_ictls
            WRITE(numout,*) '      End i indice for SUM control    nn_ictle   = ', nn_ictle
            WRITE(numout,*) '      Start j indice for SUM control  nn_jctls   = ', nn_jctls
            WRITE(numout,*) '      End j indice for SUM control    nn_jctle   = ', nn_jctle
            idg = MAXVAL( (/ nn_ictls,nn_ictle,nn_jctls,nn_jctle /) )   ! temporary use of idg to store the largest index
            idg = MAX( INT(LOG10(REAL(idg,wp))) + 1, 4 )                ! how many digits to we need to write? min=4, max=9
            WRITE(clfmt, "('(4(a,i', i1, '.', i1, '))')") idg, idg         ! '(4(a,ix.x))'
            WRITE(clfile_out,clfmt) 'mono.'//trim(clname)//'_', nn_ictls, '_', nn_ictle, '_', nn_jctls, '_', nn_jctle
         ENDIF
      ENDIF

      ! Allocate arrays
      IF( .NOT. ALLOCATED(nall_ictls) ) ALLOCATE( nall_ictls(ijsplt), nall_ictle(ijsplt), nall_jctls(ijsplt), nall_jctle(ijsplt) )

      IF( jpnij > 1 ) THEN   ! MULTI processor run
         !
         nall_ictls(1) = Nis0
         nall_ictle(1) = Nie0
         nall_jctls(1) = Njs0
         nall_jctle(1) = Nje0
         !
      ELSE                   ! MONO processor run
         !
         IF( nn_ictls+nn_ictle+nn_jctls+nn_jctle == 0 )   THEN    ! print control done over the default area
            !
            ALLOCATE(  iimppt(nn_isplt,nn_jsplt), ijmppt(nn_isplt,nn_jsplt),  ijpi(nn_isplt,nn_jsplt),  ijpj(nn_isplt,nn_jsplt),   &
               &      llisoce(nn_isplt,nn_jsplt),  iproc(nn_isplt,nn_jsplt), iipos(nn_isplt*nn_jsplt), ijpos(nn_isplt*nn_jsplt) )
            CALL mpp_basesplit( jpiglo, jpjglo, nn_hls, nn_isplt, nn_jsplt, iimax, ijmax, iimppt, ijmppt, ijpi, ijpj )
            CALL mpp_is_ocean( llisoce )
            CALL mpp_getnum( llisoce, iproc, iipos, ijpos )
            !
            DO jj = 1,nn_jsplt
               DO ji = 1, nn_isplt
                  jl = iproc(ji,jj) + 1
                  nall_ictls(jl) = iimppt(ji,jj) - 1 +      1      + nn_hls
                  nall_ictle(jl) = iimppt(ji,jj) - 1 + ijpi(ji,jj) - nn_hls
                  nall_jctls(jl) = ijmppt(ji,jj) - 1 +      1      + nn_hls
                  nall_jctle(jl) = ijmppt(ji,jj) - 1 + ijpj(ji,jj) - nn_hls
               END DO
            END DO
            !
            DEALLOCATE( iimppt, ijmppt, ijpi, ijpj, llisoce, iproc, iipos, ijpos )
            !
         ELSE                                             ! print control done over a specific  area
            !
            nall_ictls(1) = nn_ictls + nn_hls
            nall_ictle(1) = nn_ictle + nn_hls
            nall_jctls(1) = nn_jctls + nn_hls
            nall_jctle(1) = nn_jctle + nn_hls
            !
         ENDIF
      ENDIF

      ! Initialization
      IF( clcomp == 'oce' ) THEN
         ALLOCATE( t_ctl(ijsplt), s_ctl(ijsplt), u_ctl(ijsplt), v_ctl(ijsplt), numprt_oce(ijsplt) )
         t_ctl(:) = 0.e0
         s_ctl(:) = 0.e0
         u_ctl(:) = 0.e0
         v_ctl(:) = 0.e0
      ENDIF
      IF( clcomp == 'top' ) THEN
         ALLOCATE( tra_ctl(kntra,ijsplt), numprt_top(ijsplt) )
         tra_ctl(:,:) = 0.e0
      ENDIF

      DO jl = 1,ijsplt

         IF( ijsplt > 1 ) WRITE(clfile_out,clfmt) 'mono.'//trim(clname)//'_', jl-1

         CALL ctl_opn( inum, clfile_out, 'REPLACE', 'FORMATTED', 'SEQUENTIAL', 1, numout, .FALSE. )
         IF( clcomp == 'oce' )   numprt_oce(jl) = inum
         IF( clcomp == 'top' )   numprt_top(jl) = inum
         WRITE(inum,*)
         WRITE(inum,*) '   CNRS - NERC - Met OFFICE - MERCATOR-ocean - CMCC'
         WRITE(inum,*) '                       NEMO team'
         WRITE(inum,*) '            Ocean General Circulation Model'
         IF( clcomp == 'oce' )   WRITE(inum,*) '                NEMO version 4.x  (2020) '
         IF( clcomp == 'top' )   WRITE(inum,*) '                 TOP vversion x (2020) '
         WRITE(inum,*)
         IF( ijsplt > 1 )   &
            &   WRITE(inum,*) '              MPI-subdomain number: ', jl-1
         IF(  jpnij > 1 )   &
            &   WRITE(inum,*) '              MPI-subdomain number: ', narea-1
         WRITE(inum,*)
         WRITE(inum,'(19x,a20)') cl_run
         WRITE(inum,*)
         WRITE(inum,*) 'prt_ctl :  Sum control indices'
         WRITE(inum,*) '~~~~~~~'
         WRITE(inum,*)
         !
         ! clfmt2: '              ----- jctle = XXX (YYY) -----'             -> '(18x, 13a1, a9, iM, a2, iN, a2, 13a1)'
         ! clfmt3: '              |                           |'             -> '(18x, a1, Nx, a1)'
         ! clfmt4: '        ictls = XXX (YYY)           ictle = XXX (YYY)'   -> '(Nx, a9, iM, a2, iP, a2, Qx, a9, iM, a2, iP, a2)'
         !         '              |                           |'
         !         '              ----- jctle = XXX (YYY) -----'
         ! clfmt5: '   njmpp = XXX'                                          -> '(Nx, a9, iM)'
         ! clfmt6: '           nimpp = XXX'                                  -> '(Nx, a9, iM)'
         !
         idg = MAXVAL( (/ nall_ictls(jl), nall_ictle(jl), nall_jctls(jl), nall_jctle(jl) /) )   ! temporary use of idg
         idg = INT(LOG10(REAL(idg,wp))) + 1                                                     ! how many digits do we use?
         idg2 = MAXVAL( (/ mig0(nall_ictls(jl)), mig0(nall_ictle(jl)), mjg0(nall_jctls(jl)), mjg0(nall_jctle(jl)) /) )
         idg2 = INT(LOG10(REAL(idg2,wp))) + 1                                                   ! how many digits do we use?
         WRITE(clfmt2, "('(18x, 13a1, a9, i', i1, ', a2, i',i1,', a2, 13a1)')") idg, idg2
         WRITE(clfmt3, "('(18x, a1, ', i2,'x, a1)')") 13+9+idg+2+idg2+2+13 - 2
         WRITE(clfmt4, "('(', i2,'x, a9, i', i1,', a2, i', i1,', a2, ', i2,'x, a9, i', i1,', a2, i', i1,', a2)')") &
            &          18-7, idg, idg2, 13+9+idg+2+idg2+2+13 - (2+idg+2+idg2+2+8), idg, idg2
         WRITE(inum,clfmt2) ('-', ji=1,13), ' jctle = ', nall_jctle(jl), ' (', mjg0(nall_jctle(jl)), ') ', ('-', ji=1,13)
         WRITE(inum,clfmt3) '|', '|'
         WRITE(inum,clfmt3) '|', '|'
         WRITE(inum,clfmt3) '|', '|'
         WRITE(inum,clfmt4)                 ' ictls = ', nall_ictls(jl), ' (', mig0(nall_ictls(jl)), ') ',   &
            &                               ' ictle = ', nall_ictle(jl), ' (', mig0(nall_ictle(jl)), ') '
         WRITE(inum,clfmt3) '|', '|'
         WRITE(inum,clfmt3) '|', '|'
         WRITE(inum,clfmt3) '|', '|'
         WRITE(inum,clfmt2) ('-', ji=1,13), ' jctls = ', nall_jctls(jl), ' (', mjg0(nall_jctls(jl)), ') ', ('-', ji=1,13)
         WRITE(inum,*)
         WRITE(inum,*)
         !
      END DO
      !
   END SUBROUTINE prt_ctl_init


   !!======================================================================
END MODULE prtctl
