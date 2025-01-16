MODULE isfdynatf
   !!=========================================================================
   !!                       ***  MODULE  isfnxt  ***
   !! Ice shelf update: compute the dynatf ice shelf contribution
   !!=========================================================================
   !! History :  OPA  !  2019-09  (P. Mathiot)  Original code
   !!-------------------------------------------------------------------------
  
   !!-------------------------------------------------------------------------
   !!   isfnxt       : apply correction needed for the ice shelf to ensure conservation
   !!-------------------------------------------------------------------------

   USE isf_oce

   USE phycst , ONLY: r1_rho0         ! physical constant
   USE dom_oce                        ! time and space domain
   USE oce, ONLY : ssh                ! sea-surface height for qco substitution

   USE in_out_manager

   IMPLICIT NONE

   PRIVATE

   PUBLIC isf_dynatf
   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"

CONTAINS

   SUBROUTINE isf_dynatf ( kt, Kmm, pe3t_f, pcoef )
      !!--------------------------------------------------------------------
      !!                  ***  ROUTINE isf_dynatf  ***
      !!
      !! ** Purpose : compute the ice shelf volume filter correction for cavity, param, ice sheet coupling case
      !!
      !!-------------------------- OUT -------------------------------------
      INTEGER                         , INTENT(in   ) :: kt       ! ocean time step
      INTEGER                         , INTENT(in   ) :: Kmm      ! ocean time level index
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) :: pe3t_f   ! time filtered scale factor to be corrected
      !
      REAL(wp)                        , INTENT(in   ) :: pcoef    ! rn_atfp * rn_Dt * r1_rho0
      !!--------------------------------------------------------------------
      INTEGER :: jk  ! loop index
      !!--------------------------------------------------------------------
      !
      ! ice shelf cavity
      IF ( ln_isfcav_mlt ) CALL isf_dynatf_mlt(Kmm, pe3t_f, misfkt_cav, misfkb_cav, rhisf_tbl_cav, rfrac_tbl_cav, fwfisf_cav, fwfisf_cav_b, pcoef)
      !
      ! ice shelf parametrised
      IF ( ln_isfpar_mlt ) CALL isf_dynatf_mlt(Kmm, pe3t_f, misfkt_par, misfkb_par, rhisf_tbl_par, rfrac_tbl_par, fwfisf_par, fwfisf_par_b, pcoef)
      !
      IF ( ln_isfcpl .AND. ln_rstart .AND. kt == nit000+1 ) THEN
         DO jk = 1, jpkm1
            pe3t_f(:,:,jk) =   pe3t_f(:,:,jk) - pcoef * risfcpl_vol(:,:,jk) * r1_e1e2t(:,:)
         END DO
      END IF
      !
   END SUBROUTINE isf_dynatf

   SUBROUTINE isf_dynatf_mlt ( Kmm, pe3t_f, ktop, kbot, phtbl, pfrac, pfwf, pfwf_b, pcoef )
      !!--------------------------------------------------------------------
      !!                  ***  ROUTINE isf_dynatf_mlt  ***
      !!
      !! ** Purpose : compute the ice shelf volume filter correction for cavity or param
      !!
      !!-------------------------- IN  -------------------------------------
      INTEGER                         , INTENT(in   ) :: Kmm             ! ocean time level index
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) :: pe3t_f          ! time-filtered scale factor to be corrected
      INTEGER , DIMENSION(jpi,jpj)    , INTENT(in   ) :: ktop , kbot     ! top and bottom level of tbl
      REAL(wp), DIMENSION(jpi,jpj)    , INTENT(in   ) :: pfrac, phtbl    ! fraction of bottom cell included in tbl, tbl thickness
      REAL(wp), DIMENSION(jpi,jpj)    , INTENT(in   ) :: pfwf , pfwf_b   ! now/before fwf
      REAL(wp),                         INTENT(in   ) :: pcoef           ! rn_atfp * rn_Dt * r1_rho0
      !!----------------------------------------------------------------------
      INTEGER :: ji,jj,jk
      REAL(wp), DIMENSION(jpi,jpj) :: zfwfinc
      !!----------------------------------------------------------------------
      !
      ! compute fwf conservation correction
      zfwfinc(:,:) = pcoef * ( pfwf_b(:,:) - pfwf(:,:) ) / ( ht(:,:) + 1._wp - ssmask(:,:) ) * r1_rho0
      !
      ! add the increment
      DO jk = 1, jpkm1
         pe3t_f(:,:,jk) = pe3t_f(:,:,jk) + tmask(:,:,jk) * zfwfinc(:,:)   &
            &                              * e3t(:,:,jk,Kmm)
      END DO
      !
   END SUBROUTINE isf_dynatf_mlt

END MODULE isfdynatf
