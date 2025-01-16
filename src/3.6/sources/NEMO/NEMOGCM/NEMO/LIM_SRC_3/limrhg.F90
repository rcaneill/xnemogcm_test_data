MODULE limrhg
   !!======================================================================
   !!                     ***  MODULE  limrhg  ***
   !!   Ice rheology : sea ice rheology
   !!======================================================================
   !! History :   -   !  2007-03  (M.A. Morales Maqueda, S. Bouillon) Original code
   !!            3.0  !  2008-03  (M. Vancoppenolle) LIM3
   !!             -   !  2008-11  (M. Vancoppenolle, S. Bouillon, Y. Aksenov) add surface tilt in ice rheolohy 
   !!            3.3  !  2009-05  (G.Garric) addition of the lim2_evp cas
   !!            3.4  !  2011-01  (A. Porter)  dynamical allocation 
   !!            3.5  !  2012-08  (R. Benshila)  AGRIF 
   !!            3.6  !  2016-06  (C. Rousset) Rewriting (conserves energy)
   !!----------------------------------------------------------------------
#if defined key_lim3 || (  defined key_lim2 && ! defined key_lim2_vp )
   !!----------------------------------------------------------------------
   !!   'key_lim3'               OR                     LIM-3 sea-ice model
   !!   'key_lim2' AND NOT 'key_lim2_vp'            EVP LIM-2 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_rhg       : computes ice velocities
   !!----------------------------------------------------------------------
   USE phycst         ! Physical constant
   USE oce     , ONLY :  snwice_mass, snwice_mass_b
   USE par_oce        ! Ocean parameters
   USE dom_oce        ! Ocean domain
   USE sbc_oce        ! Surface boundary condition: ocean fields
   USE sbc_ice        ! Surface boundary condition: ice fields
#if defined key_lim3
   USE ice            ! LIM-3: ice variables
   USE dom_ice        ! LIM-3: ice domain
   USE limitd_me      ! LIM-3: 
#else
   USE ice_2          ! LIM-2: ice variables
   USE dom_ice_2      ! LIM-2: ice domain
#endif
   USE lbclnk         ! Lateral Boundary Condition / MPP link
   USE lib_mpp        ! MPP library
   USE wrk_nemo       ! work arrays
   USE in_out_manager ! I/O manager
   USE prtctl         ! Print control
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)  
#if defined key_agrif && defined key_lim2
   USE agrif_lim2_interp
#endif
#if defined key_bdy
   USE bdyice_lim
#endif

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_rhg        ! routine called by lim_dyn (or lim_dyn_2)

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: limrhg.F90 6964 2016-09-30 12:41:39Z clem $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_rhg( k_j1, k_jpj )
      !!-------------------------------------------------------------------
      !!                 ***  SUBROUTINE lim_rhg  ***
      !!                          EVP-C-grid
      !!
      !! ** purpose : determines sea ice drift from wind stress, ice-ocean
      !!  stress and sea-surface slope. Ice-ice interaction is described by 
      !!  a non-linear elasto-viscous-plastic (EVP) law including shear 
      !!  strength and a bulk rheology (Hunke and Dukowicz, 2002).	
      !!
      !!  The points in the C-grid look like this, dear reader
      !!
      !!                              (ji,jj)
      !!                                 |
      !!                                 |
      !!                      (ji-1,jj)  |  (ji,jj)
      !!                             ---------   
      !!                            |         |
      !!                            | (ji,jj) |------(ji,jj)
      !!                            |         |
      !!                             ---------   
      !!                     (ji-1,jj-1)     (ji,jj-1)
      !!
      !! ** Inputs  : - wind forcing (stress), oceanic currents
      !!                ice total volume (vt_i) per unit area
      !!                snow total volume (vt_s) per unit area
      !!
      !! ** Action  : - compute u_ice, v_ice : the components of the 
      !!                sea-ice velocity vector
      !!              - compute delta_i, shear_i, divu_i, which are inputs
      !!                of the ice thickness distribution
      !!
      !! ** Steps   : 1) Compute ice snow mass, ice strength 
      !!              2) Compute wind, oceanic stresses, mass terms and
      !!                 coriolis terms of the momentum equation
      !!              3) Solve the momentum equation (iterative procedure)
      !!              4) Recompute invariants of the strain rate tensor
      !!                 which are inputs of the ITD, store stress
      !!                 for the next time step
      !!              5) Control prints of residual (convergence)
      !!                 and charge ellipse.
      !!                 The user should make sure that the parameters
      !!                 nn_nevp, elastic time scale and rn_creepl maintain stress state
      !!                 on the charge ellipse for plastic flow
      !!                 e.g. in the Canadian Archipelago
      !!
      !! ** Notes   : Boundary condition for ice is chosen no-slip 
      !!              but can be adjusted with param rn_shlat
      !!
      !! References : Hunke and Dukowicz, JPO97
      !!              Bouillon et al., Ocean Modelling 2009
      !!-------------------------------------------------------------------
      INTEGER, INTENT(in) ::   k_j1    ! southern j-index for ice computation
      INTEGER, INTENT(in) ::   k_jpj   ! northern j-index for ice computation
      !!
      INTEGER ::   ji, jj   ! dummy loop indices
      INTEGER ::   jter     ! local integers
      CHARACTER (len=50) ::   charout

      REAL(wp) ::   zdtevp, z1_dtevp                                         ! time step for subcycling
      REAL(wp) ::   ecc2, z1_ecc2                                            ! square of yield ellipse eccenticity
      REAL(wp) ::   zbeta, zalph1, z1_alph1, zalph2, z1_alph2                ! alpha and beta from Bouillon 2009 and 2013
      REAL(wp) ::   zm1, zm2, zm3, zmassU, zmassV                            ! ice/snow mass
      REAL(wp) ::   zdelta, zp_delf, zds2, zdt, zdt2, zdiv, zdiv2            ! temporary scalars
      REAL(wp) ::   zTauO, zTauE, zCor                                       ! temporary scalars

      REAL(wp) ::   zsig1, zsig2                                             ! internal ice stress
      REAL(wp) ::   zresm                                                    ! Maximal error on ice velocity
      REAL(wp) ::   zintb, zintn                                             ! dummy argument
      
      REAL(wp), POINTER, DIMENSION(:,:) ::   zpresh                          ! temporary array for ice strength
      REAL(wp), POINTER, DIMENSION(:,:) ::   z1_e1t0, z1_e2t0                ! scale factors
      REAL(wp), POINTER, DIMENSION(:,:) ::   zp_delt                         ! P/delta at T points
      !
      REAL(wp), POINTER, DIMENSION(:,:) ::   zaU   , zaV                     ! ice fraction on U/V points
      REAL(wp), POINTER, DIMENSION(:,:) ::   zmU_t, zmV_t                    ! ice/snow mass/dt on U/V points
      REAL(wp), POINTER, DIMENSION(:,:) ::   zmf                             ! coriolis parameter at T points
      REAL(wp), POINTER, DIMENSION(:,:) ::   zTauU_ia , ztauV_ia             ! ice-atm. stress at U-V points
      REAL(wp), POINTER, DIMENSION(:,:) ::   zspgU , zspgV                   ! surface pressure gradient at U/V points
      REAL(wp), POINTER, DIMENSION(:,:) ::   v_oceU, u_oceV, v_iceU, u_iceV  ! ocean/ice u/v component on V/U points                           
      REAL(wp), POINTER, DIMENSION(:,:) ::   zfU   , zfV                     ! internal stresses
      
      REAL(wp), POINTER, DIMENSION(:,:) ::   zds                             ! shear
      REAL(wp), POINTER, DIMENSION(:,:) ::   zs1, zs2, zs12                  ! stress tensor components
      REAL(wp), POINTER, DIMENSION(:,:) ::   zu_ice, zv_ice, zresr           ! check convergence
      REAL(wp), POINTER, DIMENSION(:,:) ::   zpice                           ! array used for the calculation of ice surface slope:
                                                                             !   ocean surface (ssh_m) if ice is not embedded
                                                                             !   ice top surface if ice is embedded   
      REAL(wp), POINTER, DIMENSION(:,:) ::   zswitchU, zswitchV              ! dummy arrays
      REAL(wp), POINTER, DIMENSION(:,:) ::   zmaskU, zmaskV                  ! mask for ice presence
      REAL(wp), POINTER, DIMENSION(:,:) ::   zfmask, zwf                     ! mask at F points for the ice

      REAL(wp), PARAMETER               ::   zepsi  = 1.0e-20_wp             ! tolerance parameter
      REAL(wp), PARAMETER               ::   zmmin  = 1._wp                  ! ice mass (kg/m2) below which ice velocity equals ocean velocity
      REAL(wp), PARAMETER               ::   zshlat = 2._wp                  ! boundary condition for sea-ice velocity (2=no slip ; 0=free slip)
      !!-------------------------------------------------------------------

      CALL wrk_alloc( jpi,jpj, zpresh, z1_e1t0, z1_e2t0, zp_delt )
      CALL wrk_alloc( jpi,jpj, zaU, zaV, zmU_t, zmV_t, zmf, zTauU_ia, ztauV_ia )
      CALL wrk_alloc( jpi,jpj, zspgU, zspgV, v_oceU, u_oceV, v_iceU, u_iceV, zfU, zfV )
      CALL wrk_alloc( jpi,jpj, zds, zs1, zs2, zs12, zu_ice, zv_ice, zresr, zpice )
      CALL wrk_alloc( jpi,jpj, zswitchU, zswitchV, zmaskU, zmaskV, zfmask, zwf )

#if  defined key_lim2 && ! defined key_lim2_vp
# if defined key_agrif
      USE ice_2, vt_s => hsnm
      USE ice_2, vt_i => hicm
# else
      vt_s => hsnm
      vt_i => hicm
# endif
      at_i(:,:) = 1. - frld(:,:)
#endif
#if defined key_agrif && defined key_lim2 
      CALL agrif_rhg_lim2_load      ! First interpolation of coarse values
#endif
      !
      !------------------------------------------------------------------------------!
      ! 0) mask at F points for the ice (on the whole domain, not only k_j1,k_jpj) 
      !------------------------------------------------------------------------------!
      ! ocean/land mask
      DO jj = 1, jpjm1
         DO ji = 1, jpim1      ! NO vector opt.
            zfmask(ji,jj) = tmask(ji,jj,1) * tmask(ji+1,jj,1) * tmask(ji,jj+1,1) * tmask(ji+1,jj+1,1)
         END DO
      END DO
      CALL lbc_lnk( zfmask, 'F', 1._wp )

      ! Lateral boundary conditions on velocity (modify zfmask)
      zwf(:,:) = zfmask(:,:)
      DO jj = 2, jpjm1
         DO ji = fs_2, fs_jpim1   ! vector opt.
            IF( zfmask(ji,jj) == 0._wp ) THEN
               zfmask(ji,jj) = zshlat * MIN( 1._wp , MAX( zwf(ji+1,jj), zwf(ji,jj+1), zwf(ji-1,jj), zwf(ji,jj-1) ) )
            ENDIF
         END DO
      END DO
      DO jj = 2, jpjm1
         IF( zfmask(1,jj) == 0._wp ) THEN
            zfmask(1  ,jj) = zshlat * MIN( 1._wp , MAX( zwf(2,jj), zwf(1,jj+1), zwf(1,jj-1) ) )
         ENDIF
         IF( zfmask(jpi,jj) == 0._wp ) THEN
            zfmask(jpi,jj) = zshlat * MIN( 1._wp , MAX( zwf(jpi,jj+1), zwf(jpim1,jj), zwf(jpi,jj-1) ) )
         ENDIF
      END DO
      DO ji = 2, jpim1
         IF( zfmask(ji,1) == 0._wp ) THEN
            zfmask(ji,1  ) = zshlat * MIN( 1._wp , MAX( zwf(ji+1,1), zwf(ji,2), zwf(ji-1,1) ) )
         ENDIF
         IF( zfmask(ji,jpj) == 0._wp ) THEN
            zfmask(ji,jpj) = zshlat * MIN( 1._wp , MAX( zwf(ji+1,jpj), zwf(ji-1,jpj), zwf(ji,jpjm1) ) )
         ENDIF
      END DO
      CALL lbc_lnk( zfmask, 'F', 1._wp )

      !------------------------------------------------------------------------------!
      ! 1) define some variables and initialize arrays
      !------------------------------------------------------------------------------!
      ! ecc2: square of yield ellipse eccenticrity
      ecc2    = rn_ecc * rn_ecc
      z1_ecc2 = 1._wp / ecc2

      ! Time step for subcycling
      zdtevp   = rdt_ice / REAL( nn_nevp )
      z1_dtevp = 1._wp / zdtevp

      ! alpha parameters (Bouillon 2009)
#if defined key_lim3
      zalph1 = ( 2._wp * rn_relast * rdt_ice ) * z1_dtevp
#else
      zalph1 = ( 2._wp * telast ) * z1_dtevp
#endif
      zalph2 = zalph1 * z1_ecc2

      z1_alph1 = 1._wp / ( zalph1 + 1._wp )
      z1_alph2 = 1._wp / ( zalph2 + 1._wp )

      ! Initialise stress tensor 
      zs1 (:,:) = stress1_i (:,:) 
      zs2 (:,:) = stress2_i (:,:)
      zs12(:,:) = stress12_i(:,:)

      ! Ice strength
#if defined key_lim3
      CALL lim_itd_me_icestrength( nn_icestr )
      zpresh(:,:) = tmask(:,:,1) *  strength(:,:)
#else
      zpresh(:,:) = tmask(:,:,1) *  pstar * vt_i(:,:) * EXP( -c_rhg * (1. - at_i(:,:) ) )
#endif

      ! scale factors
      DO jj = k_j1+1, k_jpj-1
         DO ji = fs_2, fs_jpim1
            z1_e1t0(ji,jj) = 1._wp / ( e1t(ji+1,jj  ) + e1t(ji,jj  ) )
            z1_e2t0(ji,jj) = 1._wp / ( e2t(ji  ,jj+1) + e2t(ji,jj  ) )
         END DO
      END DO
            
      !
      !------------------------------------------------------------------------------!
      ! 2) Wind / ocean stress, mass terms, coriolis terms
      !------------------------------------------------------------------------------!

      IF( nn_ice_embd == 2 ) THEN             !== embedded sea ice: compute representative ice top surface ==!
         !                                            
         ! average interpolation coeff as used in dynspg = (1/nn_fsbc) * {SUM[n/nn_fsbc], n=0,nn_fsbc-1}
         !                                               = (1/nn_fsbc)^2 * {SUM[n], n=0,nn_fsbc-1}
         zintn = REAL( nn_fsbc - 1 ) / REAL( nn_fsbc ) * 0.5_wp     
         !
         ! average interpolation coeff as used in dynspg = (1/nn_fsbc) * {SUM[1-n/nn_fsbc], n=0,nn_fsbc-1}
         !                                               = (1/nn_fsbc)^2 * (nn_fsbc^2 - {SUM[n], n=0,nn_fsbc-1})
         zintb = REAL( nn_fsbc + 1 ) / REAL( nn_fsbc ) * 0.5_wp
         !
         zpice(:,:) = ssh_m(:,:) + ( zintn * snwice_mass(:,:) + zintb * snwice_mass_b(:,:) ) * r1_rau0
         !
      ELSE                                    !== non-embedded sea ice: use ocean surface for slope calculation ==!
         zpice(:,:) = ssh_m(:,:)
      ENDIF

      DO jj = k_j1+1, k_jpj-1
         DO ji = fs_2, fs_jpim1

            ! ice fraction at U-V points
            zaU(ji,jj) = 0.5_wp * ( at_i(ji,jj) * e12t(ji,jj) + at_i(ji+1,jj) * e12t(ji+1,jj) ) * r1_e12u(ji,jj) * umask(ji,jj,1)
            zaV(ji,jj) = 0.5_wp * ( at_i(ji,jj) * e12t(ji,jj) + at_i(ji,jj+1) * e12t(ji,jj+1) ) * r1_e12v(ji,jj) * vmask(ji,jj,1)

            ! Ice/snow mass at U-V points
            zm1 = ( rhosn * vt_s(ji  ,jj  ) + rhoic * vt_i(ji  ,jj  ) )
            zm2 = ( rhosn * vt_s(ji+1,jj  ) + rhoic * vt_i(ji+1,jj  ) )
            zm3 = ( rhosn * vt_s(ji  ,jj+1) + rhoic * vt_i(ji  ,jj+1) )
            zmassU = 0.5_wp * ( zm1 * e12t(ji,jj) + zm2 * e12t(ji+1,jj) ) * r1_e12u(ji,jj) * umask(ji,jj,1)
            zmassV = 0.5_wp * ( zm1 * e12t(ji,jj) + zm3 * e12t(ji,jj+1) ) * r1_e12v(ji,jj) * vmask(ji,jj,1)

            ! Ocean currents at U-V points
            v_oceU(ji,jj)   = 0.5_wp * ( ( v_oce(ji  ,jj) + v_oce(ji  ,jj-1) ) * e1t(ji+1,jj)    &
               &                       + ( v_oce(ji+1,jj) + v_oce(ji+1,jj-1) ) * e1t(ji  ,jj) ) * z1_e1t0(ji,jj) * umask(ji,jj,1)
            
            u_oceV(ji,jj)   = 0.5_wp * ( ( u_oce(ji,jj  ) + u_oce(ji-1,jj  ) ) * e2t(ji,jj+1)    &
               &                       + ( u_oce(ji,jj+1) + u_oce(ji-1,jj+1) ) * e2t(ji,jj  ) ) * z1_e2t0(ji,jj) * vmask(ji,jj,1)

            ! Coriolis at T points (m*f)
            zmf(ji,jj)      = zm1 * fcor(ji,jj)

            ! m/dt
            zmU_t(ji,jj)    = zmassU * z1_dtevp
            zmV_t(ji,jj)    = zmassV * z1_dtevp

            ! Drag ice-atm.
            zTauU_ia(ji,jj) = zaU(ji,jj) * utau_ice(ji,jj)
            zTauV_ia(ji,jj) = zaV(ji,jj) * vtau_ice(ji,jj)

            ! Surface pressure gradient (- m*g*GRAD(ssh)) at U-V points
            zspgU(ji,jj)    = - zmassU * grav * ( zpice(ji+1,jj) - zpice(ji,jj) ) * r1_e1u(ji,jj)
            zspgV(ji,jj)    = - zmassV * grav * ( zpice(ji,jj+1) - zpice(ji,jj) ) * r1_e2v(ji,jj)

            ! masks
            zmaskU(ji,jj) = 1._wp - MAX( 0._wp, SIGN( 1._wp, -zmassU ) )  ! 0 if no ice
            zmaskV(ji,jj) = 1._wp - MAX( 0._wp, SIGN( 1._wp, -zmassV ) )  ! 0 if no ice

            ! switches
            zswitchU(ji,jj) = MAX( 0._wp, SIGN( 1._wp, zmassU - zmmin ) ) ! 0 if ice mass < zmmin
            zswitchV(ji,jj) = MAX( 0._wp, SIGN( 1._wp, zmassV - zmmin ) ) ! 0 if ice mass < zmmin

         END DO
      END DO
      CALL lbc_lnk( zmf, 'T', 1. )
      !
      !------------------------------------------------------------------------------!
      ! 3) Solution of the momentum equation, iterative procedure
      !------------------------------------------------------------------------------!
      !
      !                                               !----------------------!
      DO jter = 1 , nn_nevp                           !    loop over jter    !
         !                                            !----------------------!        
         IF(ln_ctl) THEN   ! Convergence test
            DO jj = k_j1, k_jpj-1
               zu_ice(:,jj) = u_ice(:,jj) ! velocity at previous time step
               zv_ice(:,jj) = v_ice(:,jj)
            END DO
         ENDIF

         ! --- divergence, tension & shear (Appendix B of Hunke & Dukowicz, 2002) --- !
         DO jj = k_j1, k_jpj-1         ! loops start at 1 since there is no boundary condition (lbc_lnk) at i=1 and j=1 for F points
            DO ji = 1, jpim1

               ! shear at F points
               zds(ji,jj) = ( ( u_ice(ji,jj+1) * r1_e1u(ji,jj+1) - u_ice(ji,jj) * r1_e1u(ji,jj) ) * e1f(ji,jj) * e1f(ji,jj)   &
                  &         + ( v_ice(ji+1,jj) * r1_e2v(ji+1,jj) - v_ice(ji,jj) * r1_e2v(ji,jj) ) * e2f(ji,jj) * e2f(ji,jj)   &
                  &         ) * r1_e12f(ji,jj) * zfmask(ji,jj)

            END DO
         END DO
         CALL lbc_lnk( zds, 'F', 1. )

         DO jj = k_j1+1, k_jpj-1
            DO ji = 2, jpim1 ! no vector loop

               ! shear**2 at T points (doc eq. A16)
               zds2 = ( zds(ji,jj  ) * zds(ji,jj  ) * e12f(ji,jj  ) + zds(ji-1,jj  ) * zds(ji-1,jj  ) * e12f(ji-1,jj  )  &
                  &   + zds(ji,jj-1) * zds(ji,jj-1) * e12f(ji,jj-1) + zds(ji-1,jj-1) * zds(ji-1,jj-1) * e12f(ji-1,jj-1)  &
                  &   ) * 0.25_wp * r1_e12t(ji,jj)
              
               ! divergence at T points
               zdiv  = ( e2u(ji,jj) * u_ice(ji,jj) - e2u(ji-1,jj) * u_ice(ji-1,jj)   &
                  &    + e1v(ji,jj) * v_ice(ji,jj) - e1v(ji,jj-1) * v_ice(ji,jj-1)   &
                  &    ) * r1_e12t(ji,jj)
               zdiv2 = zdiv * zdiv
               
               ! tension at T points
               zdt  = ( ( u_ice(ji,jj) * r1_e2u(ji,jj) - u_ice(ji-1,jj) * r1_e2u(ji-1,jj) ) * e2t(ji,jj) * e2t(ji,jj)   &
                  &   - ( v_ice(ji,jj) * r1_e1v(ji,jj) - v_ice(ji,jj-1) * r1_e1v(ji,jj-1) ) * e1t(ji,jj) * e1t(ji,jj)   &
                  &   ) * r1_e12t(ji,jj)
               zdt2 = zdt * zdt
               
               ! delta at T points
               zdelta = SQRT( zdiv2 + ( zdt2 + zds2 ) * usecc2 )  

               ! P/delta at T points
               zp_delt(ji,jj) = zpresh(ji,jj) / ( zdelta + rn_creepl )
               
               ! stress at T points
               zs1(ji,jj) = ( zs1(ji,jj) * zalph1 + zp_delt(ji,jj) * ( zdiv - zdelta ) ) * z1_alph1
               zs2(ji,jj) = ( zs2(ji,jj) * zalph2 + zp_delt(ji,jj) * ( zdt * z1_ecc2 ) ) * z1_alph2
             
            END DO
         END DO
         CALL lbc_lnk( zp_delt, 'T', 1. )

         DO jj = k_j1, k_jpj-1
            DO ji = 1, jpim1

               ! P/delta at F points
               zp_delf = 0.25_wp * ( zp_delt(ji,jj) + zp_delt(ji+1,jj) + zp_delt(ji,jj+1) + zp_delt(ji+1,jj+1) )
               
               ! stress at F points
               zs12(ji,jj)= ( zs12(ji,jj) * zalph2 + zp_delf * ( zds(ji,jj) * z1_ecc2 ) * 0.5_wp ) * z1_alph2

            END DO
         END DO
         CALL lbc_lnk_multi( zs1, 'T', 1., zs2, 'T', 1., zs12, 'F', 1. )
 
         ! --- Ice internal stresses (Appendix C of Hunke and Dukowicz, 2002) --- !
         DO jj = k_j1+1, k_jpj-1
            DO ji = fs_2, fs_jpim1               

               ! U points
               zfU(ji,jj) = 0.5_wp * ( ( zs1(ji+1,jj) - zs1(ji,jj) ) * e2u(ji,jj)                                             &
                  &                  + ( zs2(ji+1,jj) * e2t(ji+1,jj) * e2t(ji+1,jj) - zs2(ji,jj) * e2t(ji,jj) * e2t(ji,jj)    &
                  &                    ) * r1_e2u(ji,jj)                                                                      &
                  &                  + ( zs12(ji,jj) * e1f(ji,jj) * e1f(ji,jj) - zs12(ji,jj-1) * e1f(ji,jj-1) * e1f(ji,jj-1)  &
                  &                    ) * 2._wp * r1_e1u(ji,jj)                                                              &
                  &                  ) * r1_e12u(ji,jj)

               ! V points
               zfV(ji,jj) = 0.5_wp * ( ( zs1(ji,jj+1) - zs1(ji,jj) ) * e1v(ji,jj)                                             &
                  &                  - ( zs2(ji,jj+1) * e1t(ji,jj+1) * e1t(ji,jj+1) - zs2(ji,jj) * e1t(ji,jj) * e1t(ji,jj)    &
                  &                    ) * r1_e1v(ji,jj)                                                                      &
                  &                  + ( zs12(ji,jj) * e2f(ji,jj) * e2f(ji,jj) - zs12(ji-1,jj) * e2f(ji-1,jj) * e2f(ji-1,jj)  &
                  &                    ) * 2._wp * r1_e2v(ji,jj)                                                              &
                  &                  ) * r1_e12v(ji,jj)

               ! u_ice at V point
               u_iceV(ji,jj) = 0.5_wp * ( ( u_ice(ji,jj  ) + u_ice(ji-1,jj  ) ) * e2t(ji,jj+1)     &
                  &                     + ( u_ice(ji,jj+1) + u_ice(ji-1,jj+1) ) * e2t(ji,jj  ) ) * z1_e2t0(ji,jj) * vmask(ji,jj,1)
               
               ! v_ice at U point
               v_iceU(ji,jj) = 0.5_wp * ( ( v_ice(ji  ,jj) + v_ice(ji  ,jj-1) ) * e1t(ji+1,jj)     &
                  &                     + ( v_ice(ji+1,jj) + v_ice(ji+1,jj-1) ) * e1t(ji  ,jj) ) * z1_e1t0(ji,jj) * umask(ji,jj,1)

            END DO
         END DO
         !
         ! --- Computation of ice velocity --- !
         !  Bouillon et al. 2013 (eq 47-48) => unstable unless alpha, beta are chosen wisely and large nn_nevp
         !  Bouillon et al. 2009 (eq 34-35) => stable
         IF( MOD(jter,2) .EQ. 0 ) THEN ! even iterations
            
            DO jj = k_j1+1, k_jpj-1
               DO ji = fs_2, fs_jpim1

                  ! tau_io/(v_oce - v_ice)
                  zTauO = zaV(ji,jj) * rhoco * SQRT( ( v_ice (ji,jj) - v_oce (ji,jj) ) * ( v_ice (ji,jj) - v_oce (ji,jj) )  &
                     &                             + ( u_iceV(ji,jj) - u_oceV(ji,jj) ) * ( u_iceV(ji,jj) - u_oceV(ji,jj) ) )

                  ! Coriolis at V-points (energy conserving formulation)
                  zCor  = - 0.25_wp * r1_e2v(ji,jj) *  &
                     &    ( zmf(ji,jj  ) * ( e2u(ji,jj  ) * u_ice(ji,jj  ) + e2u(ji-1,jj  ) * u_ice(ji-1,jj  ) )  &
                     &    + zmf(ji,jj+1) * ( e2u(ji,jj+1) * u_ice(ji,jj+1) + e2u(ji-1,jj+1) * u_ice(ji-1,jj+1) ) )

                  ! Sum of external forces (explicit solution) = F + tau_ia + Coriolis + spg + tau_io
                  zTauE = zfV(ji,jj) + zTauV_ia(ji,jj) + zCor + zspgV(ji,jj) + zTauO * ( v_oce(ji,jj) - v_ice(ji,jj) )
                  
                  ! ice velocity using implicit formulation (cf Madec doc & Bouillon 2009)
                  v_ice(ji,jj) = ( ( zmV_t(ji,jj) * v_ice(ji,jj) + zTauE + zTauO * v_ice(ji,jj)  &  ! F + tau_ia + Coriolis + spg + tau_io(only ocean part)
                     &             ) / MAX( zepsi, zmV_t(ji,jj) + zTauO ) * zswitchV(ji,jj)      &  ! m/dt + tau_io(only ice part)
                     &             + v_oce(ji,jj) * ( 1._wp - zswitchV(ji,jj) )                  &  ! v_ice = v_oce if mass < zmmin
                     &           ) * zmaskV(ji,jj)
               END DO
            END DO
            CALL lbc_lnk( v_ice, 'V', -1. )
            
#if defined key_agrif && defined key_lim2
            CALL agrif_rhg_lim2( jter, nn_nevp, 'V' )
#endif
#if defined key_bdy
            CALL bdy_ice_lim_dyn( 'V' )
#endif         

            DO jj = k_j1+1, k_jpj-1
               DO ji = fs_2, fs_jpim1
                               
                  ! tau_io/(u_oce - u_ice)
                  zTauO = zaU(ji,jj) * rhoco * SQRT( ( u_ice (ji,jj) - u_oce (ji,jj) ) * ( u_ice (ji,jj) - u_oce (ji,jj) )  &
                     &                             + ( v_iceU(ji,jj) - v_oceU(ji,jj) ) * ( v_iceU(ji,jj) - v_oceU(ji,jj) ) )

                  ! Coriolis at U-points (energy conserving formulation)
                  zCor  =   0.25_wp * r1_e1u(ji,jj) *  &
                     &    ( zmf(ji  ,jj) * ( e1v(ji  ,jj) * v_ice(ji  ,jj) + e1v(ji  ,jj-1) * v_ice(ji  ,jj-1) )  &
                     &    + zmf(ji+1,jj) * ( e1v(ji+1,jj) * v_ice(ji+1,jj) + e1v(ji+1,jj-1) * v_ice(ji+1,jj-1) ) )
                  
                  ! Sum of external forces (explicit solution) = F + tau_ia + Coriolis + spg + tau_io
                  zTauE = zfU(ji,jj) + zTauU_ia(ji,jj) + zCor + zspgU(ji,jj) + zTauO * ( u_oce(ji,jj) - u_ice(ji,jj) )

                  ! ice velocity using implicit formulation (cf Madec doc & Bouillon 2009)
                  u_ice(ji,jj) = ( ( zmU_t(ji,jj) * u_ice(ji,jj) + zTauE + zTauO * u_ice(ji,jj)  &  ! F + tau_ia + Coriolis + spg + tau_io(only ocean part)
                     &             ) / MAX( zepsi, zmU_t(ji,jj) + zTauO ) * zswitchU(ji,jj)      &  ! m/dt + tau_io(only ice part)
                     &             + u_oce(ji,jj) * ( 1._wp - zswitchU(ji,jj) )                  &  ! v_ice = v_oce if mass < zmmin 
                     &           ) * zmaskU(ji,jj)
               END DO
            END DO
            CALL lbc_lnk( u_ice, 'U', -1. )
            
#if defined key_agrif && defined key_lim2
            CALL agrif_rhg_lim2( jter, nn_nevp, 'U' )
#endif
#if defined key_bdy
            CALL bdy_ice_lim_dyn( 'U' )
#endif         

         ELSE ! odd iterations

            DO jj = k_j1+1, k_jpj-1
               DO ji = fs_2, fs_jpim1
                               
                  ! tau_io/(u_oce - u_ice)
                  zTauO = zaU(ji,jj) * rhoco * SQRT( ( u_ice (ji,jj) - u_oce (ji,jj) ) * ( u_ice (ji,jj) - u_oce (ji,jj) )  &
                     &                             + ( v_iceU(ji,jj) - v_oceU(ji,jj) ) * ( v_iceU(ji,jj) - v_oceU(ji,jj) ) )

                  ! Coriolis at U-points (energy conserving formulation)
                  zCor  =   0.25_wp * r1_e1u(ji,jj) *  &
                     &    ( zmf(ji  ,jj) * ( e1v(ji  ,jj) * v_ice(ji  ,jj) + e1v(ji  ,jj-1) * v_ice(ji  ,jj-1) )  &
                     &    + zmf(ji+1,jj) * ( e1v(ji+1,jj) * v_ice(ji+1,jj) + e1v(ji+1,jj-1) * v_ice(ji+1,jj-1) ) )
                  
                  ! Sum of external forces (explicit solution) = F + tau_ia + Coriolis + spg + tau_io
                  zTauE = zfU(ji,jj) + zTauU_ia(ji,jj) + zCor + zspgU(ji,jj) + zTauO * ( u_oce(ji,jj) - u_ice(ji,jj) )

                  ! ice velocity using implicit formulation (cf Madec doc & Bouillon 2009)
                  u_ice(ji,jj) = ( ( zmU_t(ji,jj) * u_ice(ji,jj) + zTauE + zTauO * u_ice(ji,jj)  &  ! F + tau_ia + Coriolis + spg + tau_io(only ocean part)
                     &             ) / MAX( zepsi, zmU_t(ji,jj) + zTauO ) * zswitchU(ji,jj)      &  ! m/dt + tau_io(only ice part)
                     &             + u_oce(ji,jj) * ( 1._wp - zswitchU(ji,jj) )                  &  ! v_ice = v_oce if mass < zmmin 
                     &           ) * zmaskU(ji,jj)
               END DO
            END DO
            CALL lbc_lnk( u_ice, 'U', -1. )
            
#if defined key_agrif && defined key_lim2
            CALL agrif_rhg_lim2( jter, nn_nevp, 'U' )
#endif
#if defined key_bdy
            CALL bdy_ice_lim_dyn( 'U' )
#endif         

           DO jj = k_j1+1, k_jpj-1
               DO ji = fs_2, fs_jpim1

                  ! tau_io/(v_oce - v_ice)
                  zTauO = zaV(ji,jj) * rhoco * SQRT( ( v_ice (ji,jj) - v_oce (ji,jj) ) * ( v_ice (ji,jj) - v_oce (ji,jj) )  &
                     &                             + ( u_iceV(ji,jj) - u_oceV(ji,jj) ) * ( u_iceV(ji,jj) - u_oceV(ji,jj) ) )

                  ! Coriolis at V-points (energy conserving formulation)
                  zCor  = - 0.25_wp * r1_e2v(ji,jj) *  &
                     &    ( zmf(ji,jj  ) * ( e2u(ji,jj  ) * u_ice(ji,jj  ) + e2u(ji-1,jj  ) * u_ice(ji-1,jj  ) )  &
                     &    + zmf(ji,jj+1) * ( e2u(ji,jj+1) * u_ice(ji,jj+1) + e2u(ji-1,jj+1) * u_ice(ji-1,jj+1) ) )

                  ! Sum of external forces (explicit solution) = F + tau_ia + Coriolis + spg + tau_io
                  zTauE = zfV(ji,jj) + zTauV_ia(ji,jj) + zCor + zspgV(ji,jj) + zTauO * ( v_oce(ji,jj) - v_ice(ji,jj) )
                  
                  ! ice velocity using implicit formulation (cf Madec doc & Bouillon 2009)
                  v_ice(ji,jj) = ( ( zmV_t(ji,jj) * v_ice(ji,jj) + zTauE + zTauO * v_ice(ji,jj)  &  ! F + tau_ia + Coriolis + spg + tau_io(only ocean part)
                     &             ) / MAX( zepsi, zmV_t(ji,jj) + zTauO ) * zswitchV(ji,jj)      &  ! m/dt + tau_io(only ice part)
                     &             + v_oce(ji,jj) * ( 1._wp - zswitchV(ji,jj) )                  &  ! v_ice = v_oce if mass < zmmin
                     &           ) * zmaskV(ji,jj)
               END DO
            END DO
            CALL lbc_lnk( v_ice, 'V', -1. )
            
#if defined key_agrif && defined key_lim2
            CALL agrif_rhg_lim2( jter, nn_nevp, 'V' )
#endif
#if defined key_bdy
            CALL bdy_ice_lim_dyn( 'V' )
#endif         

         ENDIF
         
         IF(ln_ctl) THEN   ! Convergence test
            DO jj = k_j1+1, k_jpj-1
               zresr(:,jj) = MAX( ABS( u_ice(:,jj) - zu_ice(:,jj) ), ABS( v_ice(:,jj) - zv_ice(:,jj) ) )
            END DO
            zresm = MAXVAL( zresr( 1:jpi, k_j1+1:k_jpj-1 ) )
            IF( lk_mpp )   CALL mpp_max( zresm )   ! max over the global domain
         ENDIF
         !
         !                                                ! ==================== !
      END DO                                              !  end loop over jter  !
      !                                                   ! ==================== !
      !
      !------------------------------------------------------------------------------!
      ! 4) Recompute delta, shear and div (inputs for mechanical redistribution) 
      !------------------------------------------------------------------------------!
      DO jj = k_j1, k_jpj-1 
         DO ji = 1, jpim1

            ! shear at F points
            zds(ji,jj) = ( ( u_ice(ji,jj+1) * r1_e1u(ji,jj+1) - u_ice(ji,jj) * r1_e1u(ji,jj) ) * e1f(ji,jj) * e1f(ji,jj)   &
               &         + ( v_ice(ji+1,jj) * r1_e2v(ji+1,jj) - v_ice(ji,jj) * r1_e2v(ji,jj) ) * e2f(ji,jj) * e2f(ji,jj)   &
               &         ) * r1_e12f(ji,jj) * zfmask(ji,jj)

         END DO
      END DO           
      CALL lbc_lnk( zds, 'F', 1. )
      
      DO jj = k_j1+1, k_jpj-1 
         DO ji = 2, jpim1 ! no vector loop
            
            ! tension**2 at T points
            zdt  = ( ( u_ice(ji,jj) * r1_e2u(ji,jj) - u_ice(ji-1,jj) * r1_e2u(ji-1,jj) ) * e2t(ji,jj) * e2t(ji,jj)   &
               &   - ( v_ice(ji,jj) * r1_e1v(ji,jj) - v_ice(ji,jj-1) * r1_e1v(ji,jj-1) ) * e1t(ji,jj) * e1t(ji,jj)   &
               &   ) * r1_e12t(ji,jj)
            zdt2 = zdt * zdt
            
            ! shear**2 at T points (doc eq. A16)
            zds2 = ( zds(ji,jj  ) * zds(ji,jj  ) * e12f(ji,jj  ) + zds(ji-1,jj  ) * zds(ji-1,jj  ) * e12f(ji-1,jj  )  &
               &   + zds(ji,jj-1) * zds(ji,jj-1) * e12f(ji,jj-1) + zds(ji-1,jj-1) * zds(ji-1,jj-1) * e12f(ji-1,jj-1)  &
               &   ) * 0.25_wp * r1_e12t(ji,jj)
            
            ! shear at T points
            shear_i(ji,jj) = SQRT( zdt2 + zds2 )

            ! divergence at T points
            divu_i(ji,jj) = ( e2u(ji,jj) * u_ice(ji,jj) - e2u(ji-1,jj) * u_ice(ji-1,jj)   &
               &            + e1v(ji,jj) * v_ice(ji,jj) - e1v(ji,jj-1) * v_ice(ji,jj-1)   &
               &            ) * r1_e12t(ji,jj)
            
            ! delta at T points
            zdelta         = SQRT( divu_i(ji,jj) * divu_i(ji,jj) + ( zdt2 + zds2 ) * usecc2 )  
            rswitch        = 1._wp - MAX( 0._wp, SIGN( 1._wp, -zdelta ) ) ! 0 if delta=0
            delta_i(ji,jj) = zdelta + rn_creepl * rswitch

         END DO
      END DO
      CALL lbc_lnk_multi( shear_i, 'T', 1., divu_i, 'T', 1., delta_i, 'T', 1. )
      
      ! --- Store the stress tensor for the next time step --- !
      stress1_i (:,:) = zs1 (:,:)
      stress2_i (:,:) = zs2 (:,:)
      stress12_i(:,:) = zs12(:,:)

      !
      !------------------------------------------------------------------------------!
      ! 5) Control prints of residual and charge ellipse
      !------------------------------------------------------------------------------!
      !
      ! print the residual for convergence
      IF(ln_ctl) THEN
         WRITE(charout,FMT="('lim_rhg  : res =',D23.16, ' iter =',I4)") zresm, jter
         CALL prt_ctl_info(charout)
         CALL prt_ctl(tab2d_1=u_ice, clinfo1=' lim_rhg  : u_ice :', tab2d_2=v_ice, clinfo2=' v_ice :')
      ENDIF

      ! print charge ellipse
      ! This can be desactivated once the user is sure that the stress state
      ! lie on the charge ellipse. See Bouillon et al. 08 for more details
      IF(ln_ctl) THEN
         CALL prt_ctl_info('lim_rhg  : numit  :',ivar1=numit)
         CALL prt_ctl_info('lim_rhg  : nwrite :',ivar1=nwrite)
         CALL prt_ctl_info('lim_rhg  : MOD    :',ivar1=MOD(numit,nwrite))
         IF( MOD(numit,nwrite) .EQ. 0 ) THEN
            WRITE(charout,FMT="('lim_rhg  :', I4, I6, I1, I1, A10)") 1000, numit, 0, 0, ' ch. ell. '
            CALL prt_ctl_info(charout)
            DO jj = k_j1+1, k_jpj-1
               DO ji = 2, jpim1
                  IF (zpresh(ji,jj) > 1.0) THEN
                     zsig1 = ( zs1(ji,jj) + (zs2(ji,jj)**2 + 4*zs12(ji,jj)**2 )**0.5 ) / ( 2*zpresh(ji,jj) ) 
                     zsig2 = ( zs1(ji,jj) - (zs2(ji,jj)**2 + 4*zs12(ji,jj)**2 )**0.5 ) / ( 2*zpresh(ji,jj) )
                     WRITE(charout,FMT="('lim_rhg  :', I4, I4, D23.16, D23.16, D23.16, D23.16, A10)")
                     CALL prt_ctl_info(charout)
                  ENDIF
               END DO
            END DO
            WRITE(charout,FMT="('lim_rhg  :', I4, I6, I1, I1, A10)") 2000, numit, 0, 0, ' ch. ell. '
            CALL prt_ctl_info(charout)
         ENDIF
      ENDIF
      !
      CALL wrk_dealloc( jpi,jpj, zpresh, z1_e1t0, z1_e2t0, zp_delt )
      CALL wrk_dealloc( jpi,jpj, zaU, zaV, zmU_t, zmV_t, zmf, zTauU_ia, ztauV_ia )
      CALL wrk_dealloc( jpi,jpj, zspgU, zspgV, v_oceU, u_oceV, v_iceU, u_iceV, zfU, zfV )
      CALL wrk_dealloc( jpi,jpj, zds, zs1, zs2, zs12, zu_ice, zv_ice, zresr, zpice )
      CALL wrk_dealloc( jpi,jpj, zswitchU, zswitchV, zmaskU, zmaskV, zfmask, zwf )

   END SUBROUTINE lim_rhg

#else
   !!----------------------------------------------------------------------
   !!   Default option          Dummy module           NO LIM sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_rhg( k1 , k2 )         ! Dummy routine
      WRITE(*,*) 'lim_rhg: You should not have seen this print! error?', k1, k2
   END SUBROUTINE lim_rhg
#endif

   !!==============================================================================
END MODULE limrhg
