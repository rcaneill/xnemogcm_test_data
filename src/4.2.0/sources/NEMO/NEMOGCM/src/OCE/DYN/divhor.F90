MODULE divhor
   !!==============================================================================
   !!                       ***  MODULE  divhor  ***
   !! Ocean diagnostic variable : now horizontal divergence
   !!==============================================================================
   !! History :  1.0  ! 2002-09  (G. Madec, E. Durand)  Free form, F90
   !!             -   ! 2005-01  (J. Chanut) Unstructured open boundaries
   !!             -   ! 2003-08  (G. Madec)  merged of cur and div, free form, F90
   !!             -   ! 2005-01  (J. Chanut, A. Sellar) unstructured open boundaries
   !!            3.3  ! 2010-09  (D.Storkey and E.O'Dea) bug fixes for BDY module
   !!             -   ! 2010-10  (R. Furner, G. Madec) runoff and cla added directly here
   !!            3.7  ! 2014-01  (G. Madec) suppression of velocity curl from in-core memory
   !!             -   ! 2014-12  (G. Madec) suppression of cross land advection option
   !!             -   ! 2015-10  (G. Madec) add velocity and rnf flag in argument of div_hor
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   div_hor    : Compute the horizontal divergence field
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE sbc_oce, ONLY : ln_rnf      ! river runoff
   USE sbcrnf , ONLY : sbc_rnf_div ! river runoff 
   USE isf_oce, ONLY : ln_isf      ! ice shelf
   USE isfhdiv, ONLY : isf_hdiv    ! ice shelf
#if defined key_asminc   
   USE asminc          ! Assimilation increment
#endif
   !
   USE in_out_manager  ! I/O manager
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp         ! MPP library
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   div_hor    ! routine called by step.F90 and istate.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: divhor.F90 15150 2021-07-27 10:38:24Z smasson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE div_hor( kt, Kbb, Kmm )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE div_hor  ***
      !!                    
      !! ** Purpose :   compute the horizontal divergence at now time-step
      !!
      !! ** Method  :   the now divergence is computed as :
      !!         hdiv = 1/(e1e2t*e3t) ( di[e2u*e3u un] + dj[e1v*e3v vn] )
      !!      and correct with runoff inflow (div_rnf) and cross land flow (div_cla) 
      !!
      !! ** Action  : - update hdiv, the now horizontal divergence
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt        ! ocean time-step index
      INTEGER, INTENT(in) ::   Kbb, Kmm  ! ocean time level indices
      !
      INTEGER  ::   ji, jj, jk    ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('div_hor')
      !
      IF( kt == nit000 ) THEN
         IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                       ! Do only on the first tile
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'div_hor : horizontal velocity divergence '
            IF(lwp) WRITE(numout,*) '~~~~~~~   '
         ENDIF
         DO_3D_OVR( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpk )
            hdiv(ji,jj,jk) = 0._wp    ! initialize hdiv for the halos at the first time step
         END_3D
      ENDIF
      !
      DO_3D_OVR( nn_hls-1, nn_hls, nn_hls-1, nn_hls, 1, jpkm1 )                                          !==  Horizontal divergence  ==!
         ! round brackets added to fix the order of floating point operations
         ! needed to ensure halo 1 - halo 2 compatibility
         hdiv(ji,jj,jk) = (  ( e2u(ji  ,jj) * e3u(ji  ,jj,jk,Kmm) * uu(ji  ,jj,jk,Kmm)     &
            &                - e2u(ji-1,jj) * e3u(ji-1,jj,jk,Kmm) * uu(ji-1,jj,jk,Kmm)     &
            &                )                                                             & ! bracket for halo 1 - halo 2 compatibility
            &              + ( e1v(ji,jj  ) * e3v(ji,jj  ,jk,Kmm) * vv(ji,jj  ,jk,Kmm)     &
            &                - e1v(ji,jj-1) * e3v(ji,jj-1,jk,Kmm) * vv(ji,jj-1,jk,Kmm)     &
            &                )                                                             & ! bracket for halo 1 - halo 2 compatibility
            &             )  * r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kmm)
      END_3D
      !
      IF( ln_rnf )   CALL sbc_rnf_div( hdiv, Kmm )                               !==  runoffs    ==!   (update hdiv field)
      !
#if defined key_asminc 
      IF( ln_sshinc .AND. ln_asmiau )   CALL ssh_asm_div( kt, Kbb, Kmm, hdiv )   !==  SSH assimilation  ==!   (update hdiv field)
      ! 
#endif
      IF( ln_isf )                      CALL isf_hdiv( kt, Kmm, hdiv )           !==  ice shelf         ==!   (update hdiv field)
      !
      IF( nn_hls==1 )   CALL lbc_lnk( 'divhor', hdiv, 'T', 1.0_wp )   !   (no sign change)
      !                                                               ! needed for ww in sshwzv
      IF( ln_timing )   CALL timing_stop('div_hor')
      !
   END SUBROUTINE div_hor
   
   !!======================================================================
END MODULE divhor
