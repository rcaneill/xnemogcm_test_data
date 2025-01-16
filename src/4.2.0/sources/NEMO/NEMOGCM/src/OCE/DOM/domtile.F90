MODULE domtile
   !!======================================================================
   !!                       ***  MODULE domtile  ***
   !! Tiling utilities
   !!======================================================================
   !! History : 4.2  !  2020-12  (D. Calvert)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dom_tile       : Set/initialise the current tile and domain indices
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space and time domain
   !
   USE prtctl         ! Print control (prt_ctl_info routine)
   USE lib_mpp , ONLY : ctl_stop, ctl_warn
   USE in_out_manager ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC dom_tile         ! called by step.F90
   PUBLIC dom_tile_start   ! called by various
   PUBLIC dom_tile_stop    ! "      "
   PUBLIC dom_tile_init    ! called by domain.F90

   LOGICAL, ALLOCATABLE, DIMENSION(:) ::   l_tilefin    ! whether a tile is finished or not

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.2 , NEMO Consortium (2020)
   !! $Id: domtile.F90 13982 2020-12-04 10:57:05Z hadcv $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dom_tile_init
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE dom_tile_init  ***
      !!
      !! ** Purpose :   Initialise tile domain variables
      !!
      !! ** Action  : - ntsi, ntsj     : start of internal part of domain
      !!              - ntei, ntej     : end of internal part of domain
      !!              - ntile          : current tile number
      !!              - nijtile        : total number of tiles
      !!              - nthl, nthr     : modifier on DO loop macro bound offset (left, right)
      !!              - nthb, ntht     :              "         "               (bottom, top)
      !!              - l_istiled      : whether tiling is currently active or not
      !!              - l_tilefin      : whether a tile is finished or not
      !!----------------------------------------------------------------------
      INTEGER ::   jt                                     ! dummy loop argument
      INTEGER ::   iitile, ijtile                         ! Local integers
      !!----------------------------------------------------------------------
      IF( ln_tile .AND. nn_hls /= 2 ) CALL ctl_stop('dom_tile_init: Tiling is only supported for nn_hls = 2')

      ntile = 0                     ! Initialise to full domain
      nijtile = 1
      ntsi = Nis0
      ntsj = Njs0
      ntei = Nie0
      ntej = Nje0
      nthl = 0
      nthr = 0
      nthb = 0
      ntht = 0
      l_istiled = .FALSE.

      IF( ln_tile ) THEN            ! Calculate tile domain indices
         iitile = Ni_0 / nn_ltile_i       ! Number of tiles
         ijtile = Nj_0 / nn_ltile_j
         IF( MOD( Ni_0, nn_ltile_i ) /= 0 ) iitile = iitile + 1
         IF( MOD( Nj_0, nn_ltile_j ) /= 0 ) ijtile = ijtile + 1

         nijtile = iitile * ijtile
         ALLOCATE( ntsi_a(0:nijtile), ntsj_a(0:nijtile), ntei_a(0:nijtile), ntej_a(0:nijtile), l_tilefin(nijtile) )

         l_tilefin(:) = .FALSE.

         ntsi_a(0) = Nis0                 ! Full domain
         ntsj_a(0) = Njs0
         ntei_a(0) = Nie0
         ntej_a(0) = Nje0

         DO jt = 1, nijtile               ! Tile domains
            ntsi_a(jt) = Nis0 + nn_ltile_i * MOD(jt - 1, iitile)
            ntsj_a(jt) = Njs0 + nn_ltile_j * ((jt - 1) / iitile)
            ntei_a(jt) = MIN(ntsi_a(jt) + nn_ltile_i - 1, Nie0)
            ntej_a(jt) = MIN(ntsj_a(jt) + nn_ltile_j - 1, Nje0)
         ENDDO
      ENDIF

      IF(lwp) THEN                  ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'dom_tile : Domain tiling decomposition'
         WRITE(numout,*) '~~~~~~~~'
         IF( ln_tile ) THEN
            WRITE(numout,*) iitile, 'tiles in i'
            WRITE(numout,*) '    Starting indices'
            WRITE(numout,*) '        ', (ntsi_a(jt), jt=1, iitile)
            WRITE(numout,*) '    Ending indices'
            WRITE(numout,*) '        ', (ntei_a(jt), jt=1, iitile)
            WRITE(numout,*) ijtile, 'tiles in j'
            WRITE(numout,*) '    Starting indices'
            WRITE(numout,*) '        ', (ntsj_a(jt), jt=1, nijtile, iitile)
            WRITE(numout,*) '    Ending indices'
            WRITE(numout,*) '        ', (ntej_a(jt), jt=1, nijtile, iitile)
         ELSE
            WRITE(numout,*) 'No domain tiling'
            WRITE(numout,*) '    i indices =', ntsi, ':', ntei
            WRITE(numout,*) '    j indices =', ntsj, ':', ntej
         ENDIF
      ENDIF
   END SUBROUTINE dom_tile_init


   SUBROUTINE dom_tile( ktsi, ktsj, ktei, ktej, ktile, ldhold, cstr )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE dom_tile  ***
      !!
      !! ** Purpose :   Set the current tile and its domain indices
      !!
      !! ** Action  : - ktsi, ktsj     : start of internal part of domain
      !!              - ktei, ktej     : end of internal part of domain
      !!              - nthl, nthr     : modifier on DO loop macro bound offset (left, right)
      !!              - nthb, ntht     :              "         "               (bottom, top)
      !!              - ktile          : set the current tile number (ntile)
      !!----------------------------------------------------------------------
      INTEGER, INTENT(out) :: ktsi, ktsj, ktei, ktej      ! Tile domain indices
      INTEGER, INTENT(in)  :: ktile                       ! Tile number
      LOGICAL, INTENT(in), OPTIONAL :: ldhold             ! Pause/resume (.true.) or set (.false.) current tile
      CHARACTER(len=*), INTENT(in), OPTIONAL   :: cstr    ! Debug information (added to warnings)
      CHARACTER(len=23) :: clstr
      LOGICAL :: llhold
      CHARACTER(len=11)   :: charout
      INTEGER :: iitile
      !!----------------------------------------------------------------------
      llhold = .FALSE.
      IF( PRESENT(ldhold) ) llhold = ldhold
      clstr = ''
      IF( PRESENT(cstr) ) clstr = TRIM(' ('//TRIM(cstr)//')')

      IF( .NOT. ln_tile ) CALL ctl_stop('Cannot use dom_tile with ln_tile = .false.')
      IF( .NOT. llhold ) THEN
         IF( .NOT. l_istiled ) THEN
            CALL ctl_warn('Cannot call dom_tile when tiling is inactive'//clstr)
            RETURN
         ENDIF

         IF( ntile /= 0 ) l_tilefin(ntile) = .TRUE.         ! If setting a new tile, the current tile is complete

         ntile = ktile                                      ! Set the new tile
         IF(sn_cfctl%l_prtctl) THEN
            WRITE(charout, FMT="('ntile =', I4)") ntile
            CALL prt_ctl_info( charout )
         ENDIF
      ENDIF

      ktsi = ntsi_a(ktile)                                  ! Set the domain indices
      ktsj = ntsj_a(ktile)
      ktei = ntei_a(ktile)
      ktej = ntej_a(ktile)

      ! Calculate the modifying factor on DO loop bounds (1 = do not work on points that have already been processed by a neighbouring tile)
      nthl = 0 ; nthr = 0 ; nthb = 0 ; ntht = 0
      iitile = Ni_0 / nn_ltile_i
      IF( MOD( Ni_0, nn_ltile_i ) /= 0 ) iitile = iitile + 1
      IF( ktsi > Nis0 ) THEN ; IF( l_tilefin(ktile - 1     ) ) nthl = 1 ; ENDIF    ! Left adjacent tile
      IF( ktei < Nie0 ) THEN ; IF( l_tilefin(ktile + 1     ) ) nthr = 1 ; ENDIF    ! Right  "  "
      IF( ktsj > Njs0 ) THEN ; IF( l_tilefin(ktile - iitile) ) nthb = 1 ; ENDIF    ! Bottom "  "
      IF( ktej < Nje0 ) THEN ; IF( l_tilefin(ktile + iitile) ) ntht = 1 ; ENDIF    ! Top    "  "
   END SUBROUTINE dom_tile


   SUBROUTINE dom_tile_start( ldhold, cstr )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE dom_tile_start  ***
      !!
      !! ** Purpose : Start or resume the use of tiling
      !!
      !! ** Method  : dom_tile_start & dom_tile_stop are used to declare a tiled region of code.
      !!
      !!              Tiling is active/inactive (l_istiled = .true./.false.) within/outside of this code region.
      !!              After enabling tiling, no tile will initially be set (the full domain will be used) and dom_tile must
      !!              be called to set a specific tile to work on. Furthermore, all tiles will be marked as incomplete
      !!              (ln_tilefin(:) = .false.).
      !!
      !!              Tiling can be paused/resumed within the tiled code region by calling dom_tile_stop/dom_tile_start
      !!              with ldhold = .true.. This can be used to temporarily revert back to using the full domain.
      !!
      !!                 CALL dom_tile_start                                  ! Enable tiling
      !!                    CALL dom_tile(ntsi, ntei, ntsj, ntej, ktile=n)    ! Set current tile "n"
      !!                    ...
      !!                    CALL dom_tile_stop(.TRUE.)                        ! Pause tiling (temporarily disable)
      !!                    ...
      !!                    CALL dom_tile_start(.TRUE.)                       ! Resume tiling
      !!                 CALL dom_tile_stop                                   ! Disable tiling
      !!----------------------------------------------------------------------
      LOGICAL, INTENT(in), OPTIONAL :: ldhold            ! Resume (.true.) or start (.false.)
      LOGICAL :: llhold
      CHARACTER(len=*), INTENT(in), OPTIONAL   :: cstr   ! Debug information (added to warnings)
      CHARACTER(len=23) :: clstr
      !!----------------------------------------------------------------------
      llhold = .FALSE.
      IF( PRESENT(ldhold) ) llhold = ldhold
      clstr = ''
      IF( PRESENT(cstr) ) clstr = TRIM(' ('//TRIM(cstr)//')')

      IF( .NOT. ln_tile ) CALL ctl_stop('Cannot resume/start tiling as ln_tile = .false.')
      IF( l_istiled ) THEN
         CALL ctl_warn('Cannot resume/start tiling as it is already active'//clstr)
         RETURN
      ! TODO: [tiling] this warning will always be raised outside a tiling loop (cannot check for pause rather than stop)
      ELSE IF( llhold .AND. ntile == 0 ) THEN
         CALL ctl_warn('Cannot resume tiling as it is not paused'//clstr)
         RETURN
      ENDIF

      ! Whether resumed or started, the tiling is made active. If resumed, the domain indices for the current tile are used.
      IF( llhold ) CALL dom_tile(ntsi, ntsj, ntei, ntej, ktile=ntile, ldhold=.TRUE., cstr='dom_tile_start'//clstr)
      l_istiled = .TRUE.
   END SUBROUTINE dom_tile_start


   SUBROUTINE dom_tile_stop( ldhold, cstr )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE dom_tile_stop  ***
      !!
      !! ** Purpose : End or pause the use of tiling
      !!
      !! ** Method  : See dom_tile_start
      !!----------------------------------------------------------------------
      LOGICAL, INTENT(in), OPTIONAL :: ldhold            ! Pause (.true.) or stop (.false.)
      LOGICAL :: llhold
      CHARACTER(len=*), INTENT(in), OPTIONAL   :: cstr   ! Debug information (added to warnings)
      CHARACTER(len=23) :: clstr
      !!----------------------------------------------------------------------
      llhold = .FALSE.
      IF( PRESENT(ldhold) ) llhold = ldhold
      clstr = ''
      IF( PRESENT(cstr) ) clstr = TRIM(' ('//TRIM(cstr)//')')

      IF( .NOT. ln_tile ) CALL ctl_stop('Cannot pause/stop tiling as ln_tile = .false.')
      IF( .NOT. l_istiled ) THEN
         CALL ctl_warn('Cannot pause/stop tiling as it is inactive'//clstr)
         RETURN
      ENDIF

      ! Whether paused or stopped, the tiling is made inactive and the full domain indices are used.
      ! If stopped, there is no active tile (ntile = 0) and the finished tile indicators are reset
      CALL dom_tile(ntsi, ntsj, ntei, ntej, ktile=0, ldhold=llhold, cstr='dom_tile_stop'//clstr)
      IF( .NOT. llhold ) l_tilefin(:) = .FALSE.
      l_istiled = .FALSE.
   END SUBROUTINE dom_tile_stop
   !!======================================================================
END MODULE domtile
