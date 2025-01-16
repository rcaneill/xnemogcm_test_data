MODULE zdfsh2
   !!======================================================================
   !!                       ***  MODULE  zdfsh2  ***
   !! Ocean physics:  shear production term of TKE
   !!=====================================================================
   !! History :   -   !  2014-10  (A. Barthelemy, G. Madec)  original code
   !!   NEMO     4.0  !  2017-04  (G. Madec)  remove u-,v-pts avm
   !!   NEMO     4.2  !  2020-12  (G. Madec, E. Clementi) add Stokes Drift Shear
   !                  !           for wave coupling
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   zdf_sh2       : compute mixing the shear production term of TKE
   !!----------------------------------------------------------------------
   USE oce
   USE dom_oce        ! domain: ocean
   USE sbcwave        ! Surface Waves (add Stokes shear)
   USE sbc_oce , ONLY: ln_stshear  !Stoked Drift shear contribution
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   zdf_sh2        ! called by zdftke, zdfglf, and zdfric

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: zdfsh2.F90 15293 2021-09-27 15:43:00Z clem $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE zdf_sh2( Kbb, Kmm, p_avm, p_sh2  )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE zdf_sh2  ***
      !!
      !! ** Purpose :   Compute the shear production term of a TKE equation
      !!
      !! ** Method  : - a stable discretization of this term is linked to the
      !!                time-space discretization of the vertical diffusion
      !!                of the OGCM. NEMO uses C-grid, a leap-frog environment
      !!                and an implicit computation of vertical mixing term,
      !!                so the shear production at w-point is given by:
      !!                   sh2 = mi[   mi(avm) * dk[ub]/e3ub * dk[un]/e3un   ]
      !!                       + mj[   mj(avm) * dk[vb]/e3vb * dk[vn]/e3vn   ]
      !!                NB: wet-point only horizontal averaging of shear
      !!
      !! ** Action  : - p_sh2 shear prod. term at w-point (inner domain only)
      !!                                                   *****
      !! References :   Bruchard, OM 2002
      !! ---------------------------------------------------------------------
      INTEGER                              , INTENT(in   ) ::   Kbb, Kmm             ! ocean time level indices
      REAL(wp), DIMENSION(:,:,:)           , INTENT(in   ) ::   p_avm                ! vertical eddy viscosity (w-points)
      REAL(wp), DIMENSION(A2D(nn_hls),jpk) , INTENT(  out) ::   p_sh2                ! shear production of TKE (w-points)
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop arguments
      REAL(wp), DIMENSION(A2D(nn_hls)) ::   zsh2u, zsh2v   ! 2D workspace
      !!--------------------------------------------------------------------
      !
      DO jk = 2, jpkm1                 !* Shear production at uw- and vw-points (energy conserving form)
         IF ( cpl_sdrftx .AND. ln_stshear )  THEN       ! Surface Stokes Drift available  ===>>>  shear + stokes drift contibution
            DO_2D( nn_hls, nn_hls-1, nn_hls, nn_hls-1 )
               zsh2u(ji,jj) = ( p_avm(ji+1,jj,jk) + p_avm(ji,jj,jk) )        &
                  &         * ( uu (ji,jj,jk-1,Kmm) -   uu (ji,jj,jk,Kmm)    &
                  &           + usd(ji,jj,jk-1) -   usd(ji,jj,jk) )  &
                  &         * ( uu (ji,jj,jk-1,Kbb) -   uu (ji,jj,jk,Kbb) )  &
                  &         / ( e3uw(ji,jj,jk,Kmm) * e3uw(ji,jj,jk,Kbb) ) * wumask(ji,jj,jk)
               zsh2v(ji,jj) = ( p_avm(ji,jj+1,jk) + p_avm(ji,jj,jk) )         &
                  &         * ( vv (ji,jj,jk-1,Kmm) -   vv (ji,jj,jk,Kmm)     &
                  &           + vsd(ji,jj,jk-1) -   vsd(ji,jj,jk) )   &
                  &         * ( vv (ji,jj,jk-1,Kbb) -   vv (ji,jj,jk,Kbb) )   &
                  &/ ( e3vw(ji,jj,jk,Kmm) * e3vw(ji,jj,jk,Kbb) ) * wvmask(ji,jj,jk)
            END_2D
         ELSE
            DO_2D( nn_hls, nn_hls-1, nn_hls, nn_hls-1 )     !* 2 x shear production at uw- and vw-points (energy conserving form)
               zsh2u(ji,jj) = ( p_avm(ji+1,jj,jk) + p_avm(ji,jj,jk) ) &
                  &         * (   uu(ji,jj,jk-1,Kmm) -   uu(ji,jj,jk,Kmm) ) &
                  &         * (   uu(ji,jj,jk-1,Kbb) -   uu(ji,jj,jk,Kbb) ) &
                  &         / ( e3uw(ji,jj,jk  ,Kmm) * e3uw(ji,jj,jk,Kbb) ) &
                  &         * wumask(ji,jj,jk)
               zsh2v(ji,jj) = ( p_avm(ji,jj+1,jk) + p_avm(ji,jj,jk) ) &
                  &         * (   vv(ji,jj,jk-1,Kmm) -   vv(ji,jj,jk,Kmm) ) &
                  &         * (   vv(ji,jj,jk-1,Kbb) -   vv(ji,jj,jk,Kbb) ) &
                  &         / ( e3vw(ji,jj,jk  ,Kmm) * e3vw(ji,jj,jk,Kbb) ) &
                  &         * wvmask(ji,jj,jk)
            END_2D
         ENDIF
         DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )     !* shear production at w-point ! coast mask: =2 at the coast ; =1 otherwise (NB: wmask useless as zsh2 are masked)
            p_sh2(ji,jj,jk) = 0.25 * (   ( zsh2u(ji-1,jj) + zsh2u(ji,jj) ) * ( 2. - umask(ji-1,jj,jk) * umask(ji,jj,jk) )   &
               &                       + ( zsh2v(ji,jj-1) + zsh2v(ji,jj) ) * ( 2. - vmask(ji,jj-1,jk) * vmask(ji,jj,jk) )   )
         END_2D
      END DO
      DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 ) ! set p_sh2 to 0 at the surface and bottom for output purpose
         p_sh2(ji,jj,1)   = 0._wp
         p_sh2(ji,jj,jpk) = 0._wp
      END_2D
      !
   END SUBROUTINE zdf_sh2

   !!======================================================================
END MODULE zdfsh2
