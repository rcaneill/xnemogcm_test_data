MODULE domutl
   !!======================================================================
   !!                       ***  MODULE  domutl  ***
   !! Grid utilities:
   !!======================================================================
   !! History : 4.2  !  2020-04  (S. Masson)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dom_ngb       : find the closest grid point from a given lon/lat position
   !!   dom_uniq      : identify unique point of a grid (TUVF)
   !!----------------------------------------------------------------------
   !
   USE dom_oce        ! ocean space and time domain
   !
   USE in_out_manager ! I/O manager
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp        ! for mppsum

   IMPLICIT NONE
   PRIVATE

   INTERFACE is_tile
      MODULE PROCEDURE is_tile_2d_sp, is_tile_3d_sp, is_tile_4d_sp, is_tile_2d_dp, is_tile_3d_dp, is_tile_4d_dp
   END INTERFACE is_tile

   PUBLIC dom_ngb    ! routine called in iom.F90 module
   PUBLIC dom_uniq   ! Called by dommsk and domwri
   PUBLIC is_tile

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.2 , NEMO Consortium (2020)
   !! $Id: domutl.F90 14834 2021-05-11 09:24:44Z hadcv $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dom_ngb( plon, plat, kii, kjj, cdgrid, kkk )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dom_ngb  ***
      !!
      !! ** Purpose :   find the closest grid point from a given lon/lat position
      !!
      !! ** Method  :   look for minimum distance in cylindrical projection
      !!                -> not good if located at too high latitude...
      !!----------------------------------------------------------------------
      REAL(wp)        , INTENT(in   ) ::   plon, plat   ! longitude,latitude of the point
      INTEGER         , INTENT(  out) ::   kii, kjj     ! i-,j-index of the closes grid point
      INTEGER         , INTENT(in   ), OPTIONAL :: kkk  ! k-index of the mask level used
      CHARACTER(len=1), INTENT(in   ) ::   cdgrid       ! grid name 'T', 'U', 'V', 'W'
      !
      INTEGER :: ik         ! working level
      INTEGER , DIMENSION(2) ::   iloc
      REAL(wp)               ::   zlon, zmini
      REAL(wp), DIMENSION(jpi,jpj) ::   zglam, zgphi, zdist
      LOGICAL , DIMENSION(jpi,jpj) ::   llmsk
      !!--------------------------------------------------------------------
      !
      ik = 1
      IF ( PRESENT(kkk) ) ik=kkk
      !
      SELECT CASE( cdgrid )
      CASE( 'U' ) ;   zglam(:,:) = glamu(:,:)   ;   zgphi(:,:) = gphiu(:,:)   ;   llmsk(:,:) = tmask_i(:,:) * umask(:,:,ik) == 1._wp
      CASE( 'V' ) ;   zglam(:,:) = glamv(:,:)   ;   zgphi(:,:) = gphiv(:,:)   ;   llmsk(:,:) = tmask_i(:,:) * vmask(:,:,ik) == 1._wp
      CASE( 'F' ) ;   zglam(:,:) = glamf(:,:)   ;   zgphi(:,:) = gphif(:,:)   ;   llmsk(:,:) = tmask_i(:,:) * fmask(:,:,ik) == 1._wp
      CASE DEFAULT;   zglam(:,:) = glamt(:,:)   ;   zgphi(:,:) = gphit(:,:)   ;   llmsk(:,:) = tmask_i(:,:) * tmask(:,:,ik) == 1._wp
      END SELECT
      !
      zlon       = MOD( plon       + 720., 360. )                                     ! plon between    0 and 360
      zglam(:,:) = MOD( zglam(:,:) + 720., 360. )                                     ! glam between    0 and 360
      IF( zlon > 270. )   zlon = zlon - 360.                                          ! zlon between  -90 and 270
      IF( zlon <  90. )   WHERE( zglam(:,:) > 180. ) zglam(:,:) = zglam(:,:) - 360.   ! glam between -180 and 180
      zglam(:,:) = zglam(:,:) - zlon
      !
      zgphi(:,:) = zgphi(:,:) - plat
      zdist(:,:) = zglam(:,:) * zglam(:,:) + zgphi(:,:) * zgphi(:,:)
      !
      CALL mpp_minloc( 'domngb', zdist(:,:), llmsk, zmini, iloc, ldhalo = .TRUE. )
      kii = iloc(1)
      kjj = iloc(2)
      !
   END SUBROUTINE dom_ngb


   SUBROUTINE dom_uniq( puniq, cdgrd )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dom_uniq  ***
      !!
      !! ** Purpose :   identify unique point of a grid (TUVF)
      !!
      !! ** Method  :   1) aplly lbc_lnk on an array with different values for each element
      !!                2) check which elements have been changed
      !!----------------------------------------------------------------------
      CHARACTER(len=1)        , INTENT(in   ) ::   cdgrd   !
      REAL(wp), DIMENSION(:,:), INTENT(inout) ::   puniq   !
      !
      REAL(wp)                       ::  zshift   ! shift value link to the process number
      INTEGER                        ::  ji       ! dummy loop indices
      LOGICAL , DIMENSION(jpi,jpj,1) ::   lluniq  ! store whether each point is unique or not
      REAL(wp), DIMENSION(jpi,jpj  ) ::   ztstref
      !!----------------------------------------------------------------------
      !
      ! build an array with different values for each element
      ! in mpp: make sure that these values are different even between process
      ! -> apply a shift value according to the process number
      zshift = jpimax * jpjmax * ( narea - 1 )
      ztstref(:,:) = RESHAPE( (/ (zshift + REAL(ji,wp), ji = 1, jpi*jpj) /), (/ jpi, jpj /) )
      !
      puniq(:,:) = ztstref(:,:)                    ! default definition
      CALL lbc_lnk( 'domwri', puniq, cdgrd, 1. )   ! apply boundary conditions
      lluniq(:,:,1) = puniq(:,:) == ztstref(:,:)   ! check which values have not been changed
      !
      puniq(:,:) = REAL( COUNT( lluniq(:,:,:), dim = 3 ), wp )
      !
   END SUBROUTINE dom_uniq


   INTEGER FUNCTION is_tile_2d_sp( pt )
      REAL(sp), DIMENSION(:,:), INTENT(in) ::   pt

      IF( l_istiled .AND. (SIZE(pt, 1) < jpi .OR. SIZE(pt, 2) < jpj) ) THEN
         is_tile_2d_sp = 1
      ELSE
         is_tile_2d_sp = 0
      ENDIF
   END FUNCTION is_tile_2d_sp


   INTEGER FUNCTION is_tile_2d_dp( pt )
      REAL(dp), DIMENSION(:,:), INTENT(in) ::   pt

      IF( l_istiled .AND. (SIZE(pt, 1) < jpi .OR. SIZE(pt, 2) < jpj) ) THEN
         is_tile_2d_dp = 1
      ELSE
         is_tile_2d_dp = 0
      ENDIF
   END FUNCTION is_tile_2d_dp


   INTEGER FUNCTION is_tile_3d_sp( pt )
      REAL(sp), DIMENSION(:,:,:), INTENT(in) ::   pt

      IF( l_istiled .AND. (SIZE(pt, 1) < jpi .OR. SIZE(pt, 2) < jpj) ) THEN
         is_tile_3d_sp = 1
      ELSE
         is_tile_3d_sp = 0
      ENDIF
   END FUNCTION is_tile_3d_sp


   INTEGER FUNCTION is_tile_3d_dp( pt )
      REAL(dp), DIMENSION(:,:,:), INTENT(in) ::   pt

      IF( l_istiled .AND. (SIZE(pt, 1) < jpi .OR. SIZE(pt, 2) < jpj) ) THEN
         is_tile_3d_dp = 1
      ELSE
         is_tile_3d_dp = 0
      ENDIF
   END FUNCTION is_tile_3d_dp


   INTEGER FUNCTION is_tile_4d_sp( pt )
      REAL(sp), DIMENSION(:,:,:,:), INTENT(in) ::   pt

      IF( l_istiled .AND. (SIZE(pt, 1) < jpi .OR. SIZE(pt, 2) < jpj) ) THEN
         is_tile_4d_sp = 1
      ELSE
         is_tile_4d_sp = 0
      ENDIF
   END FUNCTION is_tile_4d_sp


   INTEGER FUNCTION is_tile_4d_dp( pt )
      REAL(dp), DIMENSION(:,:,:,:), INTENT(in) ::   pt

      IF( l_istiled .AND. (SIZE(pt, 1) < jpi .OR. SIZE(pt, 2) < jpj) ) THEN
         is_tile_4d_dp = 1
      ELSE
         is_tile_4d_dp = 0
      ENDIF
   END FUNCTION is_tile_4d_dp
   !!======================================================================
END MODULE domutl
