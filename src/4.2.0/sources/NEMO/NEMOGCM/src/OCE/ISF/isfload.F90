MODULE isfload
   !!======================================================================
   !!                       ***  MODULE  isfload  ***
   !! Ice Shelves :   compute ice shelf load (needed for the hpg)
   !!======================================================================
   !! History :  4.1  !  2019-09  (P. Mathiot) original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   isf_load      : compute ice shelf load
   !!----------------------------------------------------------------------

   USE isf_oce, ONLY: cn_isfload, rn_isfload_T, rn_isfload_S ! ice shelf variables

   USE dom_oce                                      ! vertical scale factor
   USE eosbn2 , ONLY: eos                           ! eos routine

   USE lib_mpp, ONLY: ctl_stop                               ! ctl_stop routine
   USE in_out_manager                                        ! 

   IMPLICIT NONE

   PRIVATE

   PUBLIC   isf_load   ! called by isfstp.F90
   !
   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: sbcisf.F90 10536 2019-01-16 19:21:09Z mathiot $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE isf_load ( Kmm, pisfload )
      !!--------------------------------------------------------------------
      !!                  ***  SUBROUTINE isf_load  ***
      !!
      !! ** Purpose : compute the ice shelf load
      !!
      !!--------------------------------------------------------------------
      INTEGER,                      INTENT(in   ) ::   Kmm        ! ocean time level index      
      REAL(wp), DIMENSION(jpi,jpj), INTENT(  out) ::   pisfload   ! ice shelf load
      !!----------------------------------------------------------------------
      !
      ! quality test: ice shelf in a stratify/uniform ocean should not drive any flow.
      !               the smaller the residual flow is, the better it is.
      !
      ! type of ice shelf cavity
      SELECT CASE ( cn_isfload )
      CASE ( 'uniform' )
         CALL isf_load_uniform ( Kmm, pisfload )
      CASE DEFAULT
         CALL ctl_stop('STOP','method cn_isfload to compute ice shelf load does not exist (isomip), check your namelist')
      END SELECT
      !
   END SUBROUTINE isf_load

   
   SUBROUTINE isf_load_uniform( Kmm, pload )
      !!--------------------------------------------------------------------
      !!                  ***  SUBROUTINE isf_load  ***
      !!
      !! ** Purpose : compute the ice shelf load
      !!
      !! ** Method  : The ice shelf is assumed to be in hydro static equilibrium
      !!              in water at -1.9 C and 34.4 PSU. Weight of the ice shelf is
      !!              integrated from top to bottom.
      !!
      !!--------------------------------------------------------------------
      INTEGER,                      INTENT(in   ) ::   Kmm     ! ocean time level index      
      REAL(wp), DIMENSION(jpi,jpj), INTENT(  out) ::   pload   ! ice shelf load
      !
      INTEGER  :: ji, jj, jk
      INTEGER  :: ikt
      REAL(wp), DIMENSION(jpi,jpj)      :: zrhdtop_isf ! water density    displaced by the ice shelf (at the interface)
      REAL(wp), DIMENSION(jpi,jpj,jpts) :: zts_top     ! water properties displaced by the ice shelf   
      REAL(wp), DIMENSION(jpi,jpj,jpk)  :: zrhd        ! water density    displaced by the ice shelf
      !!----------------------------------------------------------------------
      !
      !                                !- assume water displaced by the ice shelf is at T=rn_isfload_T and S=rn_isfload_S (rude)
      zts_top(:,:,jp_tem) = rn_isfload_T   ;   zts_top(:,:,jp_sal) = rn_isfload_S
      !
      DO jk = 1, jpk                   !- compute density of the water displaced by the ice shelf
#if defined key_qco && key_isf
         CALL eos( zts_top(:,:,:), gdept_0(:,:,jk), zrhd(:,:,jk) )
#else 
         CALL eos( zts_top(:,:,:), gdept(:,:,jk,Kmm), zrhd(:,:,jk) )
#endif
      END DO
      !
      !                                !- compute rhd at the ice/oce interface (ice shelf side)
      CALL eos( zts_top , risfdep, zrhdtop_isf )
      !
      !                                !- Surface value + ice shelf gradient
      pload(:,:) = 0._wp                      ! compute pressure due to ice shelf load 
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         ikt = mikt(ji,jj)
         !
         IF ( ikt > 1 ) THEN
            !                                 ! top layer of the ice shelf
#if defined key_qco && key_isf
            pload(ji,jj) = pload(ji,jj) + zrhd(ji,jj,1) * e3w_0(ji,jj,1) 
            ! 
            DO jk = 2, ikt-1                  ! core layers of the ice shelf 
               pload(ji,jj) = pload(ji,jj) + (zrhd(ji,jj,jk-1) + zrhd(ji,jj,jk)) * e3w_0(ji,jj,jk) 
            END DO 
            !                                 ! deepest part of the ice shelf (between deepest T point and ice/ocean interface 
            pload(ji,jj) = pload(ji,jj) + ( zrhdtop_isf(ji,jj) +    zrhd(ji,jj,ikt-1) )   & 
               &                        * (     risfdep(ji,jj) - gdept_0(ji,jj,ikt-1) ) 
#else
            pload(ji,jj) = pload(ji,jj)   &
               &         + zrhd (ji,jj,1) * e3w(ji,jj,1,Kmm)
            !
            DO jk = 2, ikt-1                  ! core layers of the ice shelf
               pload(ji,jj) = pload(ji,jj) + (zrhd(ji,jj,jk-1) + zrhd(ji,jj,jk))   &
                  &                        *   e3w(ji,jj,jk,Kmm)
            END DO
            !                                 ! deepest part of the ice shelf (between deepest T point and ice/ocean interface
            pload(ji,jj) = pload(ji,jj) + ( zrhdtop_isf(ji,jj) +  zrhd(ji,jj,ikt-1)     )   &
               &                        * (     risfdep(ji,jj) - gdept(ji,jj,ikt-1,Kmm) )
#endif
            !
         END IF
      END_2D
      !
   END SUBROUTINE isf_load_uniform
   
   !!======================================================================
END MODULE isfload
