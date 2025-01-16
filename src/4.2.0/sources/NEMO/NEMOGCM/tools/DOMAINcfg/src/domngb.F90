MODULE domngb
   !!======================================================================
   !!                       ***  MODULE  domngb  ***
   !! Grid search:  find the closest grid point from a given on/lat position
   !!======================================================================
   !! History : 3.2  !  2009-11  (S. Masson)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dom_ngb       : find the closest grid point from a given lon/lat position
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space and time domain
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! for mppsum
   USE phycst

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dom_ngb   ! routine called in iom.F90 and domclo.F90 module
   PUBLIC   dist

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: domngb.F90 10425 2018-12-19 21:54:16Z smasson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dom_ngb( plon, plat, kii, kjj, rdist, cdgrid, kkk )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dom_ngb  ***
      !!
      !! ** Purpose :   find the closest grid point from a given lon/lat position
      !!
      !! ** Method  :   look for minimum distance in cylindrical projection 
      !!                -> not good if located at too high latitude...
      !!----------------------------------------------------------------------
      REAL(wp)        , INTENT(in   ) ::   plon, plat   ! longitude,latitude of the point
      REAL(wp)        , INTENT(  out) ::   rdist        ! distance between the located point and the source
      INTEGER         , INTENT(  out) ::   kii, kjj     ! i-,j-index of the closes grid point
      INTEGER         , INTENT(in   ), OPTIONAL :: kkk  ! k-index of the mask level used
      CHARACTER(len=1), INTENT(in   ) ::   cdgrid       ! grid name 'T', 'U', 'V', 'W'
      !
      INTEGER :: ik         ! working level
      INTEGER , DIMENSION(2) ::   iloc
      REAL(wp), DIMENSION(jpi,jpj) ::   zglam, zgphi, zmask, zdist
      !!--------------------------------------------------------------------
      !
      zmask(:,:) = 0._wp
      ik = 1
      IF ( PRESENT(kkk) ) ik=kkk
      SELECT CASE( cdgrid )
      CASE( 'U' )  ; zglam(:,:) = glamu(:,:) ; zgphi(:,:) = gphiu(:,:) ; zmask(Nis0:Nie0,Njs0:Nje0) = umask(Nis0:Nie0,Njs0:Nje0,ik)
      CASE( 'V' )  ; zglam(:,:) = glamv(:,:) ; zgphi(:,:) = gphiv(:,:) ; zmask(Nis0:Nie0,Njs0:Nje0) = vmask(Nis0:Nie0,Njs0:Nje0,ik)
      CASE( 'F' )  ; zglam(:,:) = glamf(:,:) ; zgphi(:,:) = gphif(:,:) ; zmask(Nis0:Nie0,Njs0:Nje0) = fmask(Nis0:Nie0,Njs0:Nje0,ik)
      CASE DEFAULT ; zglam(:,:) = glamt(:,:) ; zgphi(:,:) = gphit(:,:) ; zmask(Nis0:Nie0,Njs0:Nje0) = tmask(Nis0:Nie0,Njs0:Nje0,ik)
      END SELECT

      zdist = dist(plon, plat, zglam, zgphi) 

      IF( lk_mpp ) THEN  
         CALL mpp_minloc( 'domngb', zdist(:,:), zmask, rdist, iloc)
         kii = iloc(1) ; kjj = iloc(2)
      ELSE
         iloc(:) = MINLOC( zdist(:,:), mask = zmask(:,:) == 1.e0 )
         rdist = MINVAL( zdist(:,:) )
         kii = iloc(1) + nimpp - 1
         kjj = iloc(2) + njmpp - 1
      ENDIF
      !
   END SUBROUTINE dom_ngb

   FUNCTION dist(plonsrc, platsrc, plontrg, plattrg)
      REAL(wp),                     INTENT(in) :: plonsrc, platsrc ! lat/lon of the source point
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: plontrg, plattrg ! lat/lon of the target (2d in this case)

      REAL(wp) :: zxs, zys, zzs
      REAL(wp), DIMENSION(jpi,jpj) :: zxt, zyt, zzt

      REAL(wp), DIMENSION(jpi,jpj) :: dist ! distance between src and trg

      zxs = COS( rad * platsrc ) * COS( rad * plonsrc )
      zys = COS( rad * platsrc ) * SIN( rad * plonsrc )
      zzs = SIN( rad * platsrc )

      zxt = COS( rad * plattrg ) * COS( rad * plontrg )
      zyt = COS( rad * plattrg ) * SIN( rad * plontrg )
      zzt = SIN( rad * plattrg )

      dist(:,:) = ( zxs - zxt(:,:) )**2   &
         &      + ( zys - zyt(:,:) )**2   &
         &      + ( zzs - zzt(:,:) )**2

      dist(:,:) = ra * SQRT( dist(:,:) )

   END FUNCTION dist

   !!======================================================================
END MODULE domngb
