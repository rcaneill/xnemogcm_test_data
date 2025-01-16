MODULE icedyn_rhg_vp
   !!======================================================================
   !!                     ***  MODULE  icedyn_rhg_vp  ***
   !!   Sea-Ice dynamics : Viscous-plastic rheology with LSR technique
   !!======================================================================
   !!
   !! History :   -   !  1997-20  (J. Zhang, M. Losch) Original code, implementation into mitGCM
   !!            4.0  !  2020-09  (M. Vancoppenolle) Adaptation to SI3
   !!
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_dyn_rhg_vp : computes ice velocities from VP rheolog with LSR solvery
   !!----------------------------------------------------------------------
   USE phycst         ! Physical constants
   USE dom_oce        ! Ocean domain
   USE sbc_oce , ONLY : ln_ice_embd, nn_fsbc, ssh_m
   USE sbc_ice , ONLY : utau_ice, vtau_ice, snwice_mass, snwice_mass_b
   USE ice            ! sea-ice: ice variables
   USE icevar         ! ice_var_sshdyn
   USE icedyn_rdgrft  ! sea-ice: ice strength
   USE bdy_oce , ONLY : ln_bdy 
   USE bdyice 
#if defined key_agrif
   USE agrif_ice_interp
#endif
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O manager library
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! fortran utilities (glob_sum + no signed zero)
   USE lbclnk         ! lateral boundary conditions (or mpp links)
   USE prtctl         ! Print control

   USE netcdf         ! NetCDF library for convergence test
   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_dyn_rhg_vp   ! called by icedyn_rhg.F90

   INTEGER  ::   nn_nvp              ! total number of VP iterations (n_out_vp*n_inn_vp)
   LOGICAL  ::   lp_zebra_vp =.TRUE. ! activate zebra (solve the linear system problem every odd j-band, then one every even one)
   REAL(wp) ::   zrelaxu_vp=0.95     ! U-relaxation factor (MV: can probably be merged with V-factor once ok)
   REAL(wp) ::   zrelaxv_vp=0.95     ! V-relaxation factor 
   REAL(wp) ::   zuerr_max_vp=0.80   ! maximum velocity error, above which a forcing error is considered and solver is stopped
   REAL(wp) ::   zuerr_min_vp=1.e-06 ! minimum velocity error, beyond which convergence is assumed

   !! for convergence tests
   INTEGER ::   ncvgid        ! netcdf file id
   INTEGER ::   nvarid_ures, nvarid_vres, nvarid_velres
   INTEGER ::   nvarid_uerr_max, nvarid_verr_max, nvarid_velerr_max
   INTEGER ::   nvarid_umad, nvarid_vmad, nvarid_velmad
   INTEGER ::   nvarid_umad_outer, nvarid_vmad_outer, nvarid_velmad_outer
   INTEGER ::   nvarid_mke

   REAL(wp), DIMENSION(:,:), ALLOCATABLE ::   fimask   ! mask at F points for the ice
   
   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/ICE 4.0 , NEMO Consortium (2018)
   !! $Id: icedyn_rhg_vp.F90 13279 2020-07-09 10:39:43Z clem $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
   
CONTAINS

   SUBROUTINE ice_dyn_rhg_vp( kt, pshear_i, pdivu_i, pdelta_i )
      !!-------------------------------------------------------------------
      !!
      !!                 ***  SUBROUTINE ice_dyn_rhg_vp  ***
      !!                             VP-LSR-C-grid
      !!
      !! ** Purpose : determines sea ice drift from wind stress, ice-ocean
      !!  stress and sea-surface slope. Internal forces assume viscous-plastic rheology (Hibler, 1979)
      !! 
      !! ** Method
      !!  
      !!  The resolution algorithm  follows from Zhang and Hibler (1997) and Losch (2010)
      !!  with elements from Lemieux and Tremblay (2008) and Lemieux and Tremblay (2009)
      !!  
      !!  The components of the momentum equations are arranged following the ideas of Zhang and Hibler (1997)
      !!
      !!  f1(u) = g1(v)
      !!  f2(v) = g2(u)
      !!
      !!  The right-hand side (RHS) is explicit
      !!  The left-hand side (LHS) is implicit
      !!  Coriolis is part of explicit terms, whereas ice-ocean drag is implicit
      !!
      !!  Two iteration levels (outer and inner loops) are used to solve the equations
      !!
      !!  The outer loop (OL, typically 10 iterations) is there to deal with the (strong) non-linearities in the equation
      !!
      !!  The inner loop (IL, typically 1500 iterations) is there to solve the linear problem with a line-successive-relaxation algorithm
      !!
      !!  The velocity used in the non-linear terms uses a "modified euler time step" (not sure its the correct term), 
      !!! with uk = ( uk-1 + uk-2 ) / 2.
      !!  
      !!  * Spatial discretization 
      !!
      !!  Assumes a C-grid
      !!
      !!  The points in the C-grid look like this, my darling
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
      !! ** Action  : 
      !!             
      !! ** Steps   :
      !!            
      !! ** Notes   : 
      !!             
      !! References : Zhang and Hibler, JGR 1997; Losch et al., OM 2010., Lemieux et al., 2008, 2009, ...  
      !!             
      !!             
      !!-------------------------------------------------------------------
      !!
      INTEGER                 , INTENT(in   ) ::   kt                                    ! time step
      REAL(wp), DIMENSION(:,:), INTENT(  out) ::   pshear_i  , pdivu_i   , pdelta_i      !
      !!
      LOGICAL ::   ll_u_iterate, ll_v_iterate   ! continue iteration or not
      !
      INTEGER ::   ji, ji2, jj, jj2, jn          ! dummy loop indices
      INTEGER ::   i_out, i_inn, i_inn_tot  ! 
      INTEGER ::   ji_min, jj_min      !
      INTEGER ::   nn_zebra_vp         ! number of zebra steps

      !
      REAL(wp) ::   zrhoco                                              ! rho0 * rn_cio
      REAL(wp) ::   ecc2, z1_ecc2                                       ! square of yield ellipse eccenticity
      REAL(wp) ::   zglob_area                                          ! global ice area for diagnostics
      REAL(wp) ::   zkt                                                 ! isotropic tensile strength for landfast ice
      REAL(wp) ::   zm1, zm2, zm3, zmassU, zmassV                       ! ice/snow mass and volume
      REAL(wp) ::   zds2, zdt, zdt2, zdiv, zdiv2                        ! temporary scalars
      REAL(wp) ::   zvisc_f                                        ! 
      REAL(wp) ::   zu_cV, zv_cU                                        ! 
      REAL(wp) ::   zfac, zfac1, zfac2, zfac3
      REAL(wp) ::   zt12U, zt11U, zt22U, zt21U, zt122U, zt121U
      REAL(wp) ::   zt12V, zt11V, zt22V, zt21V, zt122V, zt121V
      REAL(wp) ::   zAA3, zw, ztau, zuerr_max, zverr_max
      !
      REAL(wp), DIMENSION(jpi,jpj) ::   za_iU  , za_iV                  ! ice fraction on U/V points
      REAL(wp), DIMENSION(jpi,jpj) ::   zmU_t, zmV_t                    ! Acceleration term contribution to RHS
      REAL(wp), DIMENSION(jpi,jpj) ::   zmassU_t, zmassV_t              ! Mass per unit area divided by time step
      !
      REAL(wp), DIMENSION(jpi,jpj) ::   zdelta                          ! Delta at T-points      (now value)
      REAL(wp), DIMENSION(jpi,jpj) ::   zten_i, zshear                  ! Tension, shear
      REAL(wp), DIMENSION(jpi,jpj) ::   zvisc_t                         ! Bulk viscosity (P/delta*) at T points
      REAL(wp), DIMENSION(jpi,jpj) ::   zvisc_t_prev                    ! Bulk viscosity (next to last iterate) - for yield curve diag
      REAL(wp), DIMENSION(jpi,jpj) ::   zzt, zet                        ! Viscosity pre-factors at T points
      REAL(wp), DIMENSION(jpi,jpj) ::   zef                             ! Viscosity pre-factor at F point
      !
      REAL(wp), DIMENSION(jpi,jpj) ::   zmt                             ! Mass per unit area at t-point
      REAL(wp), DIMENSION(jpi,jpj) ::   zmf                             ! Coriolis factor (m*f) at t-point 
      REAL(wp), DIMENSION(jpi,jpj) ::   v_oceU, u_oceV, v_iceU, u_iceV  ! ocean/ice u/v component on V/U points
      REAL(wp), DIMENSION(jpi,jpj) ::   zu_c, zv_c                      ! "current" ice velocity (m/s), average of previous two OL iterates
      REAL(wp), DIMENSION(jpi,jpj) ::   zu_b, zv_b                      ! velocity at previous sub-iterate
      REAL(wp), DIMENSION(jpi,jpj) ::   zuerr, zverr                    ! absolute U/Vvelocity difference between current and previous sub-iterates
      REAL(wp), DIMENSION(jpi,jpj) ::   zvel_res                        ! Residual of the linear system at last iteration
      REAL(wp), DIMENSION(jpi,jpj) ::   zvel_diff                       ! Absolute velocity difference @last outer iteration
      !
      REAL(wp), DIMENSION(jpi,jpj) ::   zds                             ! shear
      REAL(wp), DIMENSION(jpi,jpj) ::   zsshdyn                         ! array used for the calculation of ice surface slope:
      !                                                                 !    ocean surface (ssh_m) if ice is not embedded
      !                                                                 !    ice bottom surface if ice is embedded   
      REAL(wp), DIMENSION(jpi,jpj) ::   zCwU, zCwV                      ! ice-ocean drag pre-factor (rho*c*module(u))
      REAL(wp), DIMENSION(jpi,jpj) ::   zspgU, zspgV                    ! surface pressure gradient at U/V points
      REAL(wp), DIMENSION(jpi,jpj) ::   zCorU, zCorV                    ! Coriolis stress array
      REAL(wp), DIMENSION(jpi,jpj) ::   ztaux_ai, ztauy_ai              ! ice-atm. stress at U-V points
      REAL(wp), DIMENSION(jpi,jpj) ::   ztaux_oi_rhsu, ztauy_oi_rhsv    ! ice-ocean stress RHS contribution at U-V points
      REAL(wp), DIMENSION(jpi,jpj) ::   zs1_rhsu, zs2_rhsu, zs12_rhsu   ! internal stress contributions to RHSU
      REAL(wp), DIMENSION(jpi,jpj) ::   zs1_rhsv, zs2_rhsv, zs12_rhsv   ! internal stress contributions to RHSV
      REAL(wp), DIMENSION(jpi,jpj) ::   zf_rhsu, zf_rhsv                ! U- and V- components of internal force RHS contributions
      REAL(wp), DIMENSION(jpi,jpj) ::   zrhsu, zrhsv                    ! U and V RHS 
      REAL(wp), DIMENSION(jpi,jpj) ::   zAU, zBU, zCU, zDU, zEU         ! Linear system coefficients, U equation
      REAL(wp), DIMENSION(jpi,jpj) ::   zAV, zBV, zCV, zDV, zEV         ! Linear system coefficients, V equation
      REAL(wp), DIMENSION(jpi,jpj) ::   zFU, zFU_prime, zBU_prime       ! Rearranged linear system coefficients, U equation
      REAL(wp), DIMENSION(jpi,jpj) ::   zFV, zFV_prime, zBV_prime       ! Rearranged linear system coefficients, V equation
!!!      REAL(wp), DIMENSION(jpi,jpj) ::   ztaux_bi, ztauy_bi              ! ice-OceanBottom stress at U-V points (landfast)
!!!      REAL(wp), DIMENSION(jpi,jpj) ::   ztaux_base, ztauy_base          ! ice-bottom stress at U-V points (landfast)
     !
      REAL(wp), DIMENSION(jpi,jpj) ::   zmsk, zmsk00
      REAL(wp), DIMENSION(jpi,jpj) ::   zmsk01x, zmsk01y                ! mask for lots of ice (1), little ice (0)
      REAL(wp), DIMENSION(jpi,jpj) ::   zmsk00x, zmsk00y                ! mask for ice presence (1), no ice (0)
      !
      REAL(wp), PARAMETER          ::   zepsi  = 1.0e-20_wp             ! tolerance parameter
      REAL(wp), PARAMETER          ::   zmmin  = 1._wp                  ! ice mass (kg/m2)  below which ice velocity becomes very small
      REAL(wp), PARAMETER          ::   zamin  = 0.001_wp               ! ice concentration below which ice velocity becomes very small
      !! --- diags
      REAL(wp)                     ::   zsig1, zsig2, zsig12, z1_strength, zfac_x, zfac_y
      REAL(wp), DIMENSION(jpi,jpj) ::   zs1, zs2, zs12, zs12f           ! stress tensor components
      REAL(wp), ALLOCATABLE, DIMENSION(:,:) ::   zsig_I, zsig_II, zsig1_p, zsig2_p
      REAL(wp), ALLOCATABLE, DIMENSION(:,:) ::   ztaux_oi, ztauy_oi
      REAL(wp), ALLOCATABLE, DIMENSION(:,:) ::   zdiag_xmtrp_ice, zdiag_ymtrp_ice ! X/Y-component of ice mass transport (kg/s, SIMIP)
      REAL(wp), ALLOCATABLE, DIMENSION(:,:) ::   zdiag_xmtrp_snw, zdiag_ymtrp_snw ! X/Y-component of snow mass transport (kg/s, SIMIP)
      REAL(wp), ALLOCATABLE, DIMENSION(:,:) ::   zdiag_xatrp, zdiag_yatrp         ! X/Y-component of area transport (m2/s, SIMIP)
                        
      
      !!----------------------------------------------------------------------------------------------------------------------

      IF( kt == nit000 .AND. lwp )   WRITE(numout,*) '-- ice_dyn_rhg_vp: VP sea-ice rheology (LSR solver)'
      IF( lwp )   WRITE(numout,*) '-- ice_dyn_rhg_vp: VP sea-ice rheology (LSR solver)'
            
      !------------------------------------------------------------------------------!
      !
      ! --- Initialization
      !
      !------------------------------------------------------------------------------!
      IF ( lp_zebra_vp ) THEN; nn_zebra_vp = 2
                         ELSE; nn_zebra_vp = 1; ENDIF 

         !!clem
         nn_zebra_vp=1
         !!clem
         
      nn_nvp = nn_vp_nout * nn_vp_ninn ! maximum number of iterations

      IF( lwp )   WRITE(numout,*) ' lp_zebra_vp : ', lp_zebra_vp
      IF( lwp )   WRITE(numout,*) ' nn_zebra_vp : ', nn_zebra_vp
      IF( lwp )   WRITE(numout,*) ' nn_nvp      : ', nn_nvp
      

      ! for diagnostics and convergence tests
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         zmsk00(ji,jj) = MAX( 0._wp , SIGN( 1._wp , at_i(ji,jj) - epsi06  ) ) ! 1 if ice    , 0 if no ice
         zmsk  (ji,jj) = MAX( 0._wp , SIGN( 1._wp , at_i(ji,jj) - epsi10  ) ) ! 1 if ice    , 0 if no ice
      END_2D
      
      !---------------------------
      ! -- F-mask (code from EVP)
      !---------------------------
      IF( kt == nit000 ) THEN
         ! MartinV: 
         ! In EVP routine, fimask is applied on shear at F-points, in order to enforce the lateral boundary condition (no-slip, ..., free-slip)
         ! I am not sure the same recipe applies here
         
         ! - ocean/land mask
         ALLOCATE( fimask(jpi,jpj) )
         IF( rn_ishlat == 0._wp ) THEN
            DO_2D( 0, 0, 0, 0 )
               fimask(ji,jj) = tmask(ji,jj,1) * tmask(ji+1,jj,1) * tmask(ji,jj+1,1) * tmask(ji+1,jj+1,1)
            END_2D
         ELSE
            DO_2D( 0, 0, 0, 0 )
               fimask(ji,jj) = tmask(ji,jj,1) * tmask(ji+1,jj,1) * tmask(ji,jj+1,1) * tmask(ji+1,jj+1,1)
               ! Lateral boundary conditions on velocity (modify fimask)
               IF( fimask(ji,jj) == 0._wp ) THEN
                  fimask(ji,jj) = rn_ishlat * MIN( 1._wp , MAX( umask(ji,jj,1), umask(ji,jj+1,1), &
                     &                                          vmask(ji,jj,1), vmask(ji+1,jj,1) ) )
               ENDIF
            END_2D
         ENDIF       
         CALL lbc_lnk( 'icedyn_rhg_vp', fimask, 'F', 1._wp )
      ENDIF

      ! Initialise convergence checks
      IF( nn_rhg_chkcvg /= 0 ) THEN     
         ! ice area for global mean kinetic energy (m2)
         zglob_area = glob_sum( 'ice_rhg_vp', at_i(:,:) * e1e2t(:,:) * tmask(:,:,1) )
      ENDIF

      ! Landfast param from Lemieux(2016): add isotropic tensile strength (following Konig Beatty and Holland, 2010)
      ! MV: Not working yet...
      IF( ln_landfast_L16 ) THEN   ;   zkt = rn_lf_tensile
      ELSE                         ;   zkt = 0._wp
      ENDIF

      !------------------------------------------------------------------------------!
      !
      ! --- Time-independent quantities
      !
      !------------------------------------------------------------------------------!
      zrhoco = rho0 * rn_cio 

      ! ecc2: square of yield ellipse eccentricity
      ecc2    = rn_ecc * rn_ecc
      z1_ecc2 = 1._wp / ecc2
               
      
      CALL ice_strength ! strength at T points
      
      
      !----------------------------------------------------------------------------------------------------------
      ! -- Time-independent pre-factors for acceleration, ocean drag, coriolis, atmospheric drag, surface tilt
      !----------------------------------------------------------------------------------------------------------
      ! Compute all terms & factors independent of velocities, or only depending on velocities at previous time step      
      
      ! sea surface height
      !    embedded sea ice: compute representative ice top surface
      !    non-embedded sea ice: use ocean surface for slope calculation
      zsshdyn(:,:) = ice_var_sshdyn( ssh_m, snwice_mass, snwice_mass_b)    

      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         zmt(ji,jj) = rhos * vt_s(ji,jj) + rhoi * vt_i(ji,jj)       ! Snow and ice mass at T-point
         zmf(ji,jj) = zmt(ji,jj) * ff_t(ji,jj)                      ! Coriolis factor at T points (m*f)
      END_2D
      
      DO_2D( nn_hls, nn_hls-1, nn_hls-1, nn_hls )

         ! Ice fraction at U-V points
         za_iU(ji,jj)    = 0.5_wp * ( at_i(ji,jj) * e1e2t(ji,jj) + at_i(ji+1,jj) * e1e2t(ji+1,jj) ) * r1_e1e2u(ji,jj) * umask(ji,jj,1)

         ! Snow and ice mass at U-V points
         zm1             = zmt(ji,jj)
         zm2             = zmt(ji+1,jj)
         zmassU          = 0.5_wp * ( zm1 * e1e2t(ji,jj) + zm2 * e1e2t(ji+1,jj) ) * r1_e1e2u(ji,jj) * umask(ji,jj,1)
         
         ! Mass per unit area divided by time step
         zmassU_t(ji,jj) = zmassU * r1_Dt_ice
         
         ! Acceleration term contribution to RHS (depends on velocity at previous time step)            
         zmU_t(ji,jj)    = zmassU_t(ji,jj) * u_ice(ji,jj)
         
         ! Ocean currents at U-V points
         ! (brackets added to fix the order of floating point operations for halo 1 - halo 2 compatibility)
         v_oceU(ji,jj)   = 0.25_wp * ( (v_oce(ji,jj) + v_oce(ji,jj-1)) + (v_oce(ji+1,jj) + v_oce(ji+1,jj-1)) ) * umask(ji,jj,1)
         
         ! Wind stress
         ztaux_ai(ji,jj) = za_iU(ji,jj) * utau_ice(ji,jj)
         
         ! Force due to sea surface tilt(- m*g*GRAD(ssh))
         zspgU(ji,jj)    = - zmassU * grav * ( zsshdyn(ji+1,jj) - zsshdyn(ji,jj) ) * r1_e1u(ji,jj)
         !!spgU(ji,jj)    = - grav * ( zsshdyn(ji,jj) ) * r1_e1u(ji,jj)
         
         ! Mask for ice presence (1) or absence (0)
         zmsk00x(ji,jj)  = 1._wp - MAX( 0._wp, SIGN( 1._wp, -zmassU ) )  ! 0 if no ice
         
         ! Mask for lots of ice (1) or little ice (0)
         IF ( zmassU <= zmmin .AND. za_iU(ji,jj) <= zamin ) THEN   ;   zmsk01x(ji,jj) = 0._wp
         ELSE                                                      ;   zmsk01x(ji,jj) = 1._wp   ;   ENDIF

      END_2D      
         
            
      DO_2D( nn_hls-1, nn_hls, nn_hls, nn_hls-1 ) ! 2->jpj-1; 2->jpi-1

         ! Ice fraction at U-V points
         za_iV(ji,jj)    = 0.5_wp * ( at_i(ji,jj) * e1e2t(ji,jj) + at_i(ji,jj+1) * e1e2t(ji,jj+1) ) * r1_e1e2v(ji,jj) * vmask(ji,jj,1)

         ! Snow and ice mass at U-V points
         zm1             = zmt(ji,jj)
         zm3             = zmt(ji,jj+1)
         zmassV          = 0.5_wp * ( zm1 * e1e2t(ji,jj) + zm3 * e1e2t(ji,jj+1) ) * r1_e1e2v(ji,jj) * vmask(ji,jj,1)
         
         ! Mass per unit area divided by time step
         zmassV_t(ji,jj) = zmassV * r1_Dt_ice
         
         ! Acceleration term contribution to RHS (depends on velocity at previous time step)            
         zmV_t(ji,jj)    = zmassV_t(ji,jj) * v_ice(ji,jj)
         
         ! Ocean currents at U-V points
         ! (brackets added to fix the order of floating point operations for halo 1 - halo 2 compatibility)
         u_oceV(ji,jj)   = 0.25_wp * ( (u_oce(ji,jj) + u_oce(ji-1,jj)) + (u_oce(ji,jj+1) + u_oce(ji-1,jj+1)) ) * vmask(ji,jj,1)
         
         ! Wind stress
         ztauy_ai(ji,jj) = za_iV(ji,jj) * vtau_ice(ji,jj)
         
         ! Force due to sea surface tilt(- m*g*GRAD(ssh))
         zspgV(ji,jj)    = - zmassV * grav * ( zsshdyn(ji,jj+1) - zsshdyn(ji,jj) ) * r1_e2v(ji,jj)
         
         ! Mask for ice presence (1) or absence (0)
         zmsk00y(ji,jj)  = 1._wp - MAX( 0._wp, SIGN( 1._wp, -zmassV ) )  ! 0 if no ice
         
         ! Mask for lots of ice (1) or little ice (0)
         IF ( zmassV <= zmmin .AND. za_iV(ji,jj) <= zamin ) THEN   ;   zmsk01y(ji,jj) = 0._wp
         ELSE                                                      ;   zmsk01y(ji,jj) = 1._wp   ;   ENDIF              

      END_2D  
            
      !------------------------------------------------------------------------------!
      !
      ! --- Start outer loop
      !
      !------------------------------------------------------------------------------!
      
      zu_c(:,:) = u_ice(:,:)
      zv_c(:,:) = v_ice(:,:)
      
      i_inn_tot = 0

      DO i_out = 1, nn_vp_nout

         ! Velocities used in the non linear terms are the average of the past two iterates
         ! u_it = 0.5 * ( u_{it-1} + u_{it-2} )
         ! Also used in Hibler and Ackley (1983); Zhang and Hibler (1997); Lemieux and Tremblay (2009)
         zu_c(:,:) = 0.5_wp * ( u_ice(:,:) + zu_c(:,:) )
         zv_c(:,:) = 0.5_wp * ( v_ice(:,:) + zv_c(:,:) )
         
         !------------------------------------------------------------------------------!
         !
         ! --- Right-hand side (RHS) of the linear problem
         !
         !------------------------------------------------------------------------------!
         ! In the outer loop, one needs to update all RHS terms
         ! with explicit velocity dependencies (viscosities, coriolis, ocean stress)
         ! as a function of "current" velocities (uc, vc)
      
         !------------------------------------------
         ! -- Strain rates, viscosities and P/Delta
         !------------------------------------------

         ! --- divergence, tension & shear (Appendix B of Hunke & Dukowicz, 2002) --- !
         DO_2D( nn_hls, nn_hls-1, nn_hls, nn_hls-1 ) ! 1->jpi-1
         
            ! loops start at 1 since there is no boundary condition (lbc_lnk) at i=1 and j=1 for F points
            ! shear at F points
            zds(ji,jj) = ( ( zu_c(ji,jj+1) * r1_e1u(ji,jj+1) - zu_c(ji,jj) * r1_e1u(ji,jj) ) * e1f(ji,jj) * e1f(ji,jj)   &
               &         + ( zv_c(ji+1,jj) * r1_e2v(ji+1,jj) - zv_c(ji,jj) * r1_e2v(ji,jj) ) * e2f(ji,jj) * e2f(ji,jj)   &
               &         ) * r1_e1e2f(ji,jj) * fimask(ji,jj)

         END_2D

         DO_2D( 0, 0, 0, 0 )
            ! loop to jpi,jpj to avoid making a communication for zs1,zs2,zs12

            ! shear**2 at T points (doc eq. A16)
            zds2  = ( zds(ji,jj  ) * zds(ji,jj  ) * e1e2f(ji,jj  ) + zds(ji-1,jj  ) * zds(ji-1,jj  ) * e1e2f(ji-1,jj  )  &
               &    + zds(ji,jj-1) * zds(ji,jj-1) * e1e2f(ji,jj-1) + zds(ji-1,jj-1) * zds(ji-1,jj-1) * e1e2f(ji-1,jj-1)  &
               &    ) * 0.25_wp * r1_e1e2t(ji,jj)
              
            ! divergence at T points
            ! (brackets added to fix the order of floating point operations for halo 1 - halo 2 compatibility)
            zdiv  = ( (e2u(ji,jj) * zu_c(ji,jj) - e2u(ji-1,jj) * zu_c(ji-1,jj))   &
               &    + (e1v(ji,jj) * zv_c(ji,jj) - e1v(ji,jj-1) * zv_c(ji,jj-1))   &
               &    ) * r1_e1e2t(ji,jj)
            zdiv2 = zdiv * zdiv
               
            ! tension at T points
            zdt   = ( ( zu_c(ji,jj) * r1_e2u(ji,jj) - zu_c(ji-1,jj) * r1_e2u(ji-1,jj) ) * e2t(ji,jj) * e2t(ji,jj)   &
               &    - ( zv_c(ji,jj) * r1_e1v(ji,jj) - zv_c(ji,jj-1) * r1_e1v(ji,jj-1) ) * e1t(ji,jj) * e1t(ji,jj)   &
               &    ) * r1_e1e2t(ji,jj)
            zdt2  = zdt * zdt
               
            ! delta at T points
            zdelta(ji,jj)  = SQRT( zdiv2 + ( zdt2 + zds2 ) * z1_ecc2 ) * zmsk(ji,jj) 
               
            ! P/delta at T points
            zvisc_t(ji,jj) = strength(ji,jj) / ( zdelta(ji,jj) + rn_creepl ) * zmsk(ji,jj)

            ! Temporary zzt and zet factors at T-points
            zzt(ji,jj)     = zvisc_t(ji,jj) * r1_e1e2t(ji,jj)
            zet(ji,jj)     = zzt(ji,jj)     * z1_ecc2 
                          
         END_2D
         CALL lbc_lnk( 'icedyn_rhg_vp', zdelta, 'T', 1.0_wp, zvisc_t, 'T', 1.0_wp, zzt, 'T', 1.0_wp, zet, 'T', 1.0_wp )
         
         ! Store bulk viscosity at last outer iteration for yield curve diagnostic
         IF ( i_out == nn_vp_nout .AND. ( iom_use('sig1_pnorm') .OR. iom_use('sig2_pnorm') ) ) THEN
            zvisc_t_prev(:,:) = zvisc_t(:,:)
         ENDIF

         DO_2D( nn_hls, nn_hls-1, nn_hls, nn_hls-1 )! 1-> jpj-1; 1->jpi-1
         
            ! P/delta* at F points
            ! (brackets added to fix the order of floating point operations for halo 1 - halo 2 compatibility)
            zvisc_f = 0.25_wp * ( (zvisc_t(ji,jj) + zvisc_t(ji+1,jj)) + (zvisc_t(ji,jj+1) + zvisc_t(ji+1,jj+1)) )
            
            ! Temporary zef factor at F-point
            zef(ji,jj) = zvisc_f * r1_e1e2f(ji,jj) * z1_ecc2 * fimask(ji,jj) * 0.5_wp
            
         END_2D
         
         !---------------------------------------------------
         ! -- Ocean-ice drag and Coriolis RHS contributions
         !---------------------------------------------------

         DO_2D( nn_hls, nn_hls-1, nn_hls-1, nn_hls )
         
            !--- ice u-velocity @V points, v-velocity @U points (for non-linear drag computation)
            ! (brackets added to fix the order of floating point operations for halo 1 - halo 2 compatibility)
            zv_cU            = 0.25_wp * ( (zv_c(ji,jj) + zv_c(ji,jj-1)) + (zv_c(ji+1,jj) + zv_c(ji+1,jj-1)) ) * umask(ji,jj,1)
                
            !--- non-linear drag coefficients (need to be updated at each outer loop, see Lemieux and Tremblay JGR09, p.3, beginning of Section 3)
            zCwU(ji,jj)          = za_iU(ji,jj) * zrhoco * SQRT( ( zu_c (ji,jj) - u_oce (ji,jj) ) * ( zu_c (ji,jj) - u_oce (ji,jj) )  &
              &                                                + ( zv_cU - v_oceU(ji,jj) ) * ( zv_cU - v_oceU(ji,jj) ) )
                 
            !--- Ocean-ice drag contributions to RHS 
            ztaux_oi_rhsu(ji,jj) = zCwU(ji,jj) * u_oce(ji,jj)
                
            !--- U-component of Coriolis Force (energy conserving formulation)
            zCorU(ji,jj)         =   0.25_wp * r1_e1u(ji,jj) *  &
                       &             ( (zmf(ji  ,jj) * ( e1v(ji  ,jj) * zv_c(ji  ,jj) + e1v(ji  ,jj-1) * zv_c(ji  ,jj-1) ))  &
                       &             + (zmf(ji+1,jj) * ( e1v(ji+1,jj) * zv_c(ji+1,jj) + e1v(ji+1,jj-1) * zv_c(ji+1,jj-1) )) )
                           
         END_2D

         DO_2D( nn_hls-1, nn_hls, nn_hls, nn_hls-1 )
         
            !--- ice u-velocity @V points, v-velocity @U points (for non-linear drag computation)
            ! (brackets added to fix the order of floating point operations for halo 1 - halo 2 compatibility)
            zu_cV            = 0.25_wp * ( (zu_c(ji,jj) + zu_c(ji-1,jj)) + (zu_c(ji,jj+1) + zu_c(ji-1,jj+1)) ) * vmask(ji,jj,1)
                
            !--- non-linear drag coefficients (need to be updated at each outer loop, see Lemieux and Tremblay JGR09, p.3, beginning of Section 3)
            zCwV(ji,jj)          = za_iV(ji,jj) * zrhoco * SQRT( ( zv_c (ji,jj) - v_oce (ji,jj) ) * ( zv_c (ji,jj) - v_oce (ji,jj) )  &
              &                                                + ( zu_cV - u_oceV(ji,jj) ) * ( zu_cV - u_oceV(ji,jj) ) )
                 
            !--- Ocean-ice drag contributions to RHS 
            ztauy_oi_rhsv(ji,jj) = zCwV(ji,jj) * v_oce(ji,jj)
                
            !--- V-component of Coriolis Force (energy conserving formulation)
            zCorV(ji,jj)         = - 0.25_wp * r1_e2v(ji,jj) *  &
                       &             ( (zmf(ji,jj  ) * ( e2u(ji,jj  ) * zu_c(ji,jj  ) + e2u(ji-1,jj  ) * zu_c(ji-1,jj  ) ))  &
                       &             + (zmf(ji,jj+1) * ( e2u(ji,jj+1) * zu_c(ji,jj+1) + e2u(ji-1,jj+1) * zu_c(ji-1,jj+1) )) )

         END_2D
         
         !-------------------------------------
         ! -- Internal stress RHS contribution
         !-------------------------------------

         ! --- Stress contributions at T-points
         DO_2D( nn_hls, nn_hls, nn_hls-1, nn_hls ) ! 2 -> jpj; 2,jpi !!! CHECK !!!
                     
            ! sig1 contribution to RHS of U-equation at T-points
            zs1_rhsu(ji,jj) =   zzt(ji,jj) * ( e1v(ji,jj)    * zv_c(ji,jj) - e1v(ji,jj-1)    * zv_c(ji,jj-1) )   &
                            &                - zvisc_t(ji,jj) * zdelta(ji,jj)
                                            
            ! sig2 contribution to RHS of U-equation at T-points            
            zs2_rhsu(ji,jj) = - zet(ji,jj) * ( r1_e1v(ji,jj) * zv_c(ji,jj) - r1_e1v(ji,jj-1) * zv_c(ji,jj-1) ) * e1t(ji,jj) * e1t(ji,jj) 
         END_2D
         
         DO_2D( nn_hls-1, nn_hls, nn_hls, nn_hls ) ! 2 -> jpj; 2,jpi !!! CHECK !!!
            ! sig1 contribution to RHS of V-equation at T-points
            zs1_rhsv(ji,jj) =   zzt(ji,jj) * ( e2u(ji,jj)    * zu_c(ji,jj) - e2u(ji-1,jj)    * zu_c(ji-1,jj) )   & 
                            &                - zvisc_t(ji,jj) * zdelta(ji,jj)

            ! sig2 contribution to RHS of V-equation  at T-points
            zs2_rhsv(ji,jj) =   zet(ji,jj) * ( r1_e2u(ji,jj) * zu_c(ji,jj) - r1_e2u(ji-1,jj) * zu_c(ji-1,jj) ) * e2t(ji,jj) * e2t(ji,jj)

         END_2D
                  
         ! --- Stress contributions at F-points         
         ! MV NOTE: I applied fimask on zds, by mimetism on EVP, but without deep understanding of what I was doing
         ! My guess is that this is the way to enforce boundary conditions on strain rate tensor

         DO_2D( nn_hls, nn_hls-1, nn_hls, nn_hls-1 ) ! 1->jpi-1
         
            ! sig12 contribution to RHS of U equation at F-points 
            zs12_rhsu(ji,jj) =   zef(ji,jj)  * ( r1_e2v(ji+1,jj) * zv_c(ji+1,jj) + r1_e2v(ji,jj) * zv_c(ji,jj) ) * e2f(ji,jj) * e2f(ji,jj) * fimask(ji,jj)

            ! sig12 contribution to RHS of V equation at F-points
            zs12_rhsv(ji,jj) =   zef(ji,jj)  * ( r1_e1u(ji,jj+1) * zu_c(ji,jj+1) + r1_e1u(ji,jj) * zu_c(ji,jj) ) * e1f(ji,jj) * e1f(ji,jj) * fimask(ji,jj)

         END_2D
         
         ! --- Internal force contributions to RHS, taken as divergence of stresses (Appendix C of Hunke and Dukowicz, 2002)
         ! OPT: merge with next loop and use intermediate scalars for zf_rhsu
         DO_2D( nn_hls, nn_hls-1, nn_hls-1, nn_hls-1 ) ! 2->jpj-1; 2->jpi-1
         
            ! --- U component of internal force contribution to RHS at U points
            zf_rhsu(ji,jj) = 0.5_wp * r1_e1e2u(ji,jj) * & 
               (    e2u(ji,jj)    * ( zs1_rhsu(ji+1,jj) - zs1_rhsu(ji,jj) )                                                                 &
               &      +         r1_e2u(ji,jj) * ( e2t(ji+1,jj) * e2t(ji+1,jj) * zs2_rhsu(ji+1,jj) - e2t(ji,jj)   * e2t(ji,jj)   * zs2_rhsu(ji,jj) )     &
               &      + 2._wp * r1_e1u(ji,jj) * ( e1f(ji,jj)   * e1f(ji,jj)   * zs12_rhsu(ji,jj)  - e1f(ji,jj-1) * e1f(ji,jj-1) * zs12_rhsu(ji,jj-1) ) )
         END_2D
         
         DO_2D( nn_hls-1, nn_hls-1, nn_hls, nn_hls-1 ) ! 2->jpj-1; 2->jpi-1
            
            ! --- V component of internal force contribution to RHS at V points
            zf_rhsv(ji,jj) = 0.5_wp * r1_e1e2v(ji,jj) * &
               &           (    e1v(ji,jj)    * ( zs1_rhsv(ji,jj+1) - zs1_rhsv(ji,jj) )                                                                 &
               &      -         r1_e1v(ji,jj) * ( e1t(ji,jj+1) * e1t(ji,jj+1) * zs2_rhsv(ji,jj+1) - e1t(ji,jj)   * e1t(ji,jj)   * zs2_rhsv(ji,jj) )     &
               &      + 2._wp * r1_e2v(ji,jj) * ( e2f(ji,jj)   * e2f(ji,jj)   * zs12_rhsv(ji,jj)  - e2f(ji-1,jj) * e2f(ji-1,jj) * zs12_rhsv(ji-1,jj) ) )

         END_2D
         
         !---------------------------
         ! -- Sum RHS contributions
         !---------------------------
         ! OPT: could use intermediate scalars to reduce memory access
         DO_2D( nn_hls, nn_hls-1, nn_hls-1, nn_hls-1 ) ! 2->jpj-1; 2->jpi-1
            zrhsu(ji,jj) = zmU_t(ji,jj) + ztaux_ai(ji,jj) + ztaux_oi_rhsu(ji,jj) + zspgU(ji,jj) + zCorU(ji,jj) + zf_rhsu(ji,jj)
         END_2D
         
         DO_2D( nn_hls-1, nn_hls-1, nn_hls, nn_hls-1 ) ! 2->jpj-1; 2->jpi-1
            zrhsv(ji,jj) = zmV_t(ji,jj) + ztauy_ai(ji,jj) + ztauy_oi_rhsv(ji,jj) + zspgV(ji,jj) + zCorV(ji,jj) + zf_rhsv(ji,jj)
         END_2D
         
         !---------------------------------------------------------------------------------------!
         !
         ! --- Linear system matrix
         !
         !---------------------------------------------------------------------------------------!
      
         ! Linear system matrix contains all implicit contributions 
         ! 1) internal forces, 2) acceleration, 3) ice-ocean drag

         ! The linear system equation is written as follows
         ! AU * u_{i-1,j} + BU * u_{i,j}   + CU * u_{i+1,j}
         !                = DU * u_{i,j-1} + EU * u_{i,j+1} + RHS 			(! my convention, not the same as ZH97 )         
         
         ! MV Note 1: martin losch applies boundary condition to BU in mitGCM - check whether it is necessary here ?
         ! MV Note 2: "T" factor calculations can be optimized by putting things out of the loop 
         !         only zzt and zet are iteration-dependent, other only depend on scale factors
                  
         DO_2D( nn_hls, nn_hls-1, nn_hls-1, nn_hls-1 )
         
            !-------------------------------------
            ! -- Internal forces LHS contribution
            !-------------------------------------
            !
            ! --- U-component
            !
            ! "T" factors (intermediate results)
            !
            zfac       = 0.5_wp * r1_e1e2u(ji,jj)
            zfac1      =         zfac * e2u(ji,jj)
            zfac2      =         zfac * r1_e2u(ji,jj)
            zfac3      = 2._wp * zfac * r1_e1u(ji,jj)

            zt11U      =   zfac1 * zzt(ji,jj)
            zt12U      =   zfac1 * zzt(ji+1,jj)
         
            zt21U      =   zfac2 * zet(ji,jj)   * e2t(ji,jj)   * e2t(ji,jj)   * e2t(ji,jj)   * e2t(ji,jj)
            zt22U      =   zfac2 * zet(ji+1,jj) * e2t(ji+1,jj) * e2t(ji+1,jj) * e2t(ji+1,jj) * e2t(ji+1,jj)
         
            zt121U     =   zfac3 * zef(ji,jj-1) * e1f(ji,jj-1) * e1f(ji,jj-1) * e1f(ji,jj-1) * e1f(ji,jj-1)
            zt122U     =   zfac3 * zef(ji,jj)   * e1f(ji,jj)   * e1f(ji,jj)   * e1f(ji,jj)   * e1f(ji,jj)
               
            !
            ! Linear system coefficients
            !
            zBU(ji,jj) =   ( zt11U + zt12U ) * e2u(ji,jj)   + ( zt21U + zt22U ) * r1_e2u(ji,jj)   + ( zt121U + zt122U ) * r1_e1u(ji,jj)
            zCU(ji,jj) = -   zt12U           * e2u(ji+1,jj) -   zt22U           * r1_e2u(ji+1,jj)
         
            zDU(ji,jj) =     zt121U * r1_e1u(ji,jj-1)
            zEU(ji,jj) =     zt122U * r1_e1u(ji,jj+1)
 
            !-----------------------------------------------------
            ! -- Ocean-ice drag and acceleration LHS contribution
            !-----------------------------------------------------
            zBU(ji,jj) = zBU(ji,jj) + zCwU(ji,jj) + zmassU_t(ji,jj)
         
         END_2D
         
         DO_2D( nn_hls-1, nn_hls-1, nn_hls, nn_hls-1 )
         
            !-------------------------------------
            ! -- Internal forces LHS contribution
            !-------------------------------------
            !
            ! --- V-component
            !
            ! "T" factors (intermediate results)
            !
            zfac       = 0.5_wp * r1_e1e2v(ji,jj)
            zfac1      =         zfac * e1v(ji,jj)
            zfac2      =         zfac * r1_e1v(ji,jj)
            zfac3      = 2._wp * zfac * r1_e2v(ji,jj)

            zt11V      =   zfac1 * zzt(ji,jj)
            zt12V      =   zfac1 * zzt(ji,jj+1)

            zt21V      =   zfac2 * zet(ji,jj)   * e1t(ji,jj)   * e1t(ji,jj)   * e1t(ji,jj)   * e1t(ji,jj)
            zt22V      =   zfac2 * zet(ji,jj+1) * e1t(ji,jj+1) * e1t(ji,jj+1) * e1t(ji,jj+1) * e1t(ji,jj+1)
         
            zt121V     =   zfac3 * zef(ji-1,jj) * e2f(ji-1,jj) * e2f(ji-1,jj) * e2f(ji-1,jj) * e2f(ji-1,jj)
            zt122V     =   zfac3 * zef(ji,jj)   * e2f(ji,jj)   * e2f(ji,jj)   * e2f(ji,jj)   * e2f(ji,jj)

            !
            ! Linear system coefficients
            !
            zBV(ji,jj) =   ( zt11V + zt12V ) * e1v(ji,jj)   + ( zt21V + zt22V ) * r1_e1v(ji,jj)   + ( zt122V + zt121V ) * r1_e2v(ji,jj)
            zCV(ji,jj) = -   zt12V           * e1v(ji,jj+1) -   zt22V           * r1_e1v(ji,jj+1)

            zDV(ji,jj) =     zt121V * r1_e2v(ji-1,jj)
            zEV(ji,jj) =     zt122V * r1_e2v(ji+1,jj)
                  
            !-----------------------------------------------------
            ! -- Ocean-ice drag and acceleration LHS contribution
            !-----------------------------------------------------
            zBV(ji,jj) = zBV(ji,jj) + zCwV(ji,jj) + zmassV_t(ji,jj)
         
         END_2D

         DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
            ! **U**
            zfac       = 0.5_wp * r1_e1e2u(ji,jj)
            zfac1      =         zfac * e2u(ji,jj)
            zfac2      =         zfac * r1_e2u(ji,jj)
            zt11U      =   zfac1 * zzt(ji,jj)
            zt21U      =   zfac2 * zet(ji,jj)   * e2t(ji,jj)   * e2t(ji,jj)   * e2t(ji,jj)   * e2t(ji,jj)
            !
            zAU(ji,jj) = -   zt11U           * e1u(ji-1,jj) -   zt21U           * r1_e1u(ji-1,jj) !!clem: because of this fuck we need to start at jpi=2

            ! **V**
            zfac       = 0.5_wp * r1_e1e2v(ji,jj)
            zfac1      =         zfac * e1v(ji,jj)
            zfac2      =         zfac * r1_e1v(ji,jj)
            zt11V      =   zfac1 * zzt(ji,jj)
            zt21V      =   zfac2 * zet(ji,jj)   * e1t(ji,jj)   * e1t(ji,jj)   * e1t(ji,jj)   * e1t(ji,jj)
            !
            zAV(ji,jj) = -   zt11V           * e1v(ji,jj-1) -   zt21V           * r1_e1v(ji,jj-1) !!clem: because of this fuck we need to start at jpj=2
         END_2D

!!         CALL lbc_lnk( 'icedyn_rhg_vp', zAU, 'U', -1._wp, zBU, 'U', -1._wp, zCU, 'U', -1._wp, zDU, 'U', -1._wp, zEU, 'U', -1._wp, &
!!            &                           zAV, 'V', -1._wp, zBV, 'V', -1._wp, zCV, 'V', -1._wp, zDV, 'V', -1._wp, zEV, 'V', -1._wp )
         
         !------------------------------------------------------------------------------!
         !
         ! --- Inner loop: solve linear system, check convergence
         !
         !------------------------------------------------------------------------------!
               
         ! Inner loop solves the linear problem .. requires 1500 iterations
         ll_u_iterate = .TRUE.
         ll_v_iterate = .TRUE.

         DO i_inn = 1, nn_vp_ninn ! inner loop iterations

            !--- mitgcm computes initial value of residual here...

            i_inn_tot  = i_inn_tot + 1
            ! l_full_nf_update = i_inn_tot == nn_nvp   ! false: disable full North fold update (performances) for iter = 1 to nn_nevp-1

            zu_b(:,:)       = u_ice(:,:) ! velocity at previous inner-iterate
            zv_b(:,:)       = v_ice(:,:)

            IF ( ll_u_iterate .OR. ll_v_iterate )   THEN

                                           ! ---------------------------- !
               IF ( ll_u_iterate ) THEN    ! --- Solve for u-velocity --- !
                                           ! ---------------------------- !

                  ! What follows could be subroutinized...
      
                  ! Thomas Algorithm  for tridiagonal solver
                  ! A*u(i-1,j)+B*u(i,j)+C*u(i+1,j) = F
                  
                  DO jn = 1, nn_zebra_vp ! "zebra" loop (! red-black-sor!!! )
                  
                     ! OPT: could be even better optimized with a true red-black SOR
      
                     IF ( jn == 1 ) THEN   ;   jj_min = ntsj-(nn_hls-1)
                     ELSE                  ;   jj_min = ntsj-(nn_hls-1)+1
                     ENDIF

                     DO jj = jj_min, jpj - 1, nn_zebra_vp
!!                     DO jj = jj_min, ntej+(nn_hls-1), nn_zebra_vp
                        
                        !------------------------
                        ! Independent term (zFU)
                        !------------------------
                        !!                        DO ji = ntsi-(nn_hls), ntei+(nn_hls-1)
                        DO ji = 1, jpi-1
                           
                           ! note: these are key lines linking information between processors
                           ! u_ice/v_ice need to be lbc_linked

                           ! sub-domain boundary condition substitution
                           ! see Zhang and Hibler, 1997, Appendix B
                           zAA3 = 0._wp
!!$                           IF ( ji == 2 )         zAA3 = zAA3 - zAU(ji,jj) * u_ice(ji-1,jj)
!!$                           IF ( ji == jpi - 1 )   zAA3 = zAA3 - zCU(ji,jj) * u_ice(ji+1,jj)

                           ! right hand side
                           zFU(ji,jj) = ( zrhsu(ji,jj) &                                      ! right-hand side terms
                               &      +   zAA3         &                                      ! boundary condition translation
                               &      +   zDU(ji,jj) * u_ice(ji,jj-1)   &                     ! internal force, j-1
                               &      +   zEU(ji,jj) * u_ice(ji,jj+1) ) * umask(ji,jj,1)      ! internal force, j+1

                        END DO

                     END DO
                     
                     !!CALL lbc_lnk( 'icedyn_rhg_vp', zFU, 'U', -1._wp )
                     !---------------
                     ! Forward sweep
                     !---------------   
                     DO jj = jj_min, jpj - 1, nn_zebra_vp
                        !!                     DO jj = jj_min, ntej+(nn_hls-1), nn_zebra_vp
      
!!$                        zBU_prime(2,jj)     = zBU(2,jj)
!!$                        zFU_prime(2,jj)     = zFU(2,jj)

                        DO ji = 2, jpi-1
                           !!                        DO ji = ntsi-(nn_hls-1), ntei+(nn_hls-1)

                           zfac             = SIGN( 1._wp , zBU(ji-1,jj) ) * MAX( 0._wp , SIGN( 1._wp , ABS( zBU(ji-1,jj) ) - epsi20 ) )
                           zw               = zfac * zAU(ji,jj) / MAX ( ABS( zBU(ji-1,jj) ) , epsi20 ) 
                           zBU_prime(ji,jj) = zBU(ji,jj) - zw * zCU(ji-1,jj)
                           zFU_prime(ji,jj) = zFU(ji,jj) - zw * zFU(ji-1,jj)

                        END DO

                     END DO
                                                                                                     
                     !-----------------------------
                     ! Backward sweep & relaxation
                     !-----------------------------

                     DO jj = jj_min, jpj - 1, nn_zebra_vp
                        !!DO jj = jj_min, ntej+(nn_hls-1), nn_zebra_vp
                    
                        ! --- Backward sweep 

                        ! last row 
!!$                        zfac = SIGN( 1._wp , zBU_prime(jpi-1,jj) ) * MAX( 0._wp , SIGN( 1._wp , ABS( zBU_prime(jpi-1,jj) ) - epsi20 ) )
!!$                        u_ice(jpi-1,jj)    = zfac * zFU_prime(jpi-1,jj) / MAX( ABS ( zBU_prime(jpi-1,jj) ) , epsi20 ) & 
!!$                                           &            * umask(jpi-1,jj,1)

                        !!clem => should be backward but then no repro!!!
                        !!DO ji = jpi - 1 , 2, -1 ! all other rows    !  ---> original backward loop
                        !!DO ji = ntei+(nn_hls-1), ntsi-(nn_hls-1), -1
                        DO ji = 2, jpi - 1 ! all other rows    ! 
                           zfac = SIGN( 1._wp , zBU_prime(ji,jj) ) * MAX( 0._wp , SIGN( 1._wp , ABS( zBU_prime(ji,jj) ) - epsi20 ) )
                           u_ice(ji,jj)    = zfac * ( zFU_prime(ji,jj) - zCU(ji,jj) * u_ice(ji+1,jj) ) * umask(ji,jj,1)   & 
                                           &                  / MAX ( ABS ( zBU_prime(ji,jj) ) , epsi20 ) 
                        END DO

                        !--- Relaxation and masking (for low-ice/no-ice cases)
                        DO ji = 2, jpi - 1    
                           !!DO ji = ntsi-(nn_hls-1), ntei+(nn_hls-1)
                        
                           u_ice(ji,jj) = zu_b(ji,jj) + zrelaxu_vp * ( u_ice(ji,jj) - zu_b(ji,jj) ) ! relaxation
                           
                           u_ice(ji,jj) =   zmsk00x(ji,jj)                                        &   ! masking
                              &         * (           zmsk01x(ji,jj)   * u_ice(ji,jj)             &
                              &           + ( 1._wp - zmsk01x(ji,jj) ) * u_oce(ji,jj) * 0.01_wp     ) * umask(ji,jj,1)
                           
                        END DO

                     END DO ! jj
                     
                  END DO ! zebra loop

               ENDIF !   ll_u_iterate

               !                           ! ---------------------------- !
               IF ( ll_v_iterate ) THEN    ! --- Solve for V-velocity --- !
               !                           ! ---------------------------- !
                                          
                  ! MV OPT: what follows could be subroutinized...                  
                  ! Thomas Algorithm  for tridiagonal solver
                  ! A*v(i,j-1)+B*v(i,j)+C*v(i,j+1) = F
                  ! It is intentional to have a ji then jj loop for V-velocity
                  !!! ZH97 explain it is critical for convergence speed

                  DO jn = 1, nn_zebra_vp ! "zebra" loop
      
                     IF ( jn == 1 ) THEN   ;   ji_min = 2
                     ELSE                  ;   ji_min = 3
                     ENDIF

                     DO ji = ji_min, jpi - 1, nn_zebra_vp 
                     
                        !------------------------
                        ! Independent term (zFV)
                        !------------------------
                        DO jj = 1, jpj - 1

                           ! subdomain boundary condition substitution (check it is correctly applied !!!)
                           ! see Zhang and Hibler, 1997, Appendix B
                           zAA3 = 0._wp
!!$                           IF ( jj == 2 )       zAA3 = zAA3 - zAV(ji,jj) * v_ice(ji,jj-1)
!!$                           IF ( jj == jpj - 1 ) zAA3 = zAA3 - zCV(ji,jj) * v_ice(ji,jj+1)
     
                           ! right hand side
                           zFV(ji,jj) = ( zrhsv(ji,jj) &                                   ! right-hand side terms
                               &        + zAA3                                           & ! boundary condition translation
                               &        + zDV(ji,jj) * v_ice(ji-1,jj)                    & ! internal force, j-1
                               &        + zEV(ji,jj) * v_ice(ji+1,jj) ) * vmask(ji,jj,1)   ! internal force, j+1

                        END DO
                        
                     END DO

                     !---------------
                     ! Forward sweep
                     !---------------
                     DO ji = ji_min, jpi - 1, nn_zebra_vp 
                     
!!$                        zBV_prime(ji,2)     = zBV(ji,2)
!!$                        zFV_prime(ji,2)     = zFV(ji,2)

                        DO jj = 2, jpj - 1 

                           zfac             = SIGN( 1._wp , zBV(ji,jj-1) ) * MAX( 0._wp , SIGN( 1._wp , ABS( zBV(ji,jj-1) ) - epsi20 ) )
                           zw               = zfac * zAV(ji,jj) / MAX ( ABS( zBV(ji,jj-1) ) , epsi20 )
                           zBV_prime(ji,jj) = zBV(ji,jj) - zw * zCV(ji,jj-1)
                           zFV_prime(ji,jj) = zFV(ji,jj) - zw * zFV(ji,jj-1) 

                        END DO

                     END DO

                     !-----------------------------
                     ! Backward sweep & relaxation
                     !-----------------------------
                     DO ji = ji_min, jpi - 1, nn_zebra_vp 
                    
                        ! --- Backward sweep 
!!$                        ! last row
!!$                        zfac = SIGN( 1._wp , zBV_prime(ji,jpj-1) ) * MAX( 0._wp , SIGN( 1._wp , ABS( zBV_prime(ji,jpj-1) ) - epsi20 ) )
!!$                        v_ice(ji,jpj-1)  = zfac * zFV_prime(ji,jpj-1) / MAX ( ABS(zBV_prime(ji,jpj-1) ) , epsi20 ) & 
!!$                                         &         * vmask(ji,jpj-1,1)  ! last row

                        ! other rows
                        !!clem => should be backward but then no repro!!!
                        !!DO jj = jpj-1, 2, -1 ! original back loop
                        DO jj = 2, jpj-1
                           zfac = SIGN( 1._wp , zBV_prime(ji,jj) ) * MAX( 0._wp , SIGN( 1._wp , ABS( zBV_prime(ji,jj) ) - epsi20 ) )
                           v_ice(ji,jj)   = zfac * ( zFV_prime(ji,jj) - zCV(ji,jj) * v_ice(ji,jj+1) ) * vmask(ji,jj,1) &
                                          &  / MAX ( ABS( zBV_prime(ji,jj) ) , epsi20 )       
                        END DO            
                                                   
                        ! --- Relaxation & masking 
                        DO jj = 2, jpj - 1
                        
                            v_ice(ji,jj) = zv_b(ji,jj) + zrelaxv_vp * ( v_ice(ji,jj) - zv_b(ji,jj) )    ! relaxation
                            
                            v_ice(ji,jj) =   zmsk00y(ji,jj)                                        &      ! masking
                              &         * (           zmsk01y(ji,jj)   * v_ice(ji,jj)              &
                              &           + ( 1._wp - zmsk01y(ji,jj) ) * v_oce(ji,jj) * 0.01_wp    ) * vmask(ji,jj,1)

                        END DO ! jj
                        
                     END DO ! ji

                     
                  END DO ! zebra loop
                                    
               ENDIF !   ll_v_iterate

               CALL lbc_lnk( 'icedyn_rhg_vp', u_ice, 'U', -1._wp, v_ice, 'V', -1._wp )

               ! I suspect the communication should go into the zebra loop if we want reproducibility
                              
               !--------------------------------------------------------------------------------------
               ! -- Check convergence based on maximum velocity difference, continue or stop the loop
               !--------------------------------------------------------------------------------------

               !------
               ! on U
               !------
               ! MV OPT: if the number of iterations to convergence is really variable, and keep the convergence check
               ! then we must optimize the use of the mpp_max, which is prohibitive                            
               zuerr_max  = 0._wp
                               
               IF ( ll_u_iterate .AND. MOD ( i_inn, nn_vp_chkcvg ) == 0 ) THEN

                  ! - Maximum U-velocity difference               
                  zuerr(:,:) = 0._wp
                  DO_2D( 0, 0, 0, 0 )
                  
                     zuerr(ji,jj) = ABS ( ( u_ice(ji,jj) - zu_b(ji,jj) ) ) * umask(ji,jj,1) 
                  
                  END_2D
         
                  zuerr_max = MAXVAL( zuerr )
                  CALL mpp_max( 'icedyn_rhg_vp', zuerr_max )   ! max over the global domain - damned!

                  ! - Stop if max error is too large ("safeguard against bad forcing" of original Zhang routine)
                  IF ( i_inn > 1 .AND. zuerr_max > zuerr_max_vp ) THEN
                      IF ( lwp ) WRITE(numout,*) " VP rheology error was too large : ", zuerr_max, " in outer U-iteration ", i_out, " after ", i_inn, " iterations, we stopped "
                      ll_u_iterate = .FALSE.
                  ENDIF
                  
                  ! - Stop if error small enough
                  IF ( zuerr_max < zuerr_min_vp ) THEN                                        
                      IF ( lwp ) WRITE(numout,*) " VP rheology nicely done in outer U-iteration ", i_out, " after ", i_inn, " iterations, finished! "
                      ll_u_iterate = .FALSE.
                  ENDIF
                                               
               ENDIF ! ll_u_iterate

               !------
               ! on V
               !------
               zverr_max = 0._wp
               
               IF ( ll_v_iterate .AND. MOD ( i_inn, nn_vp_chkcvg ) == 0 ) THEN
               
                  ! - Maximum V-velocity difference
                  zverr(:,:)   = 0._wp   
                  DO_2D( 0, 0, 0, 0 )
                 
                        zverr(ji,jj) = ABS ( ( v_ice(ji,jj) - zv_b(ji,jj) ) ) * vmask(ji,jj,1)
                  
                  END_2D
                           
                  zverr_max = MAXVAL( zverr )
                  CALL mpp_max( 'icedyn_rhg_vp', zverr_max )   ! max over the global domain - damned!
                  
                  ! - Stop if error is too large ("safeguard against bad forcing" of original Zhang routine)
                  IF ( i_inn > 1 .AND. zverr_max > zuerr_max_vp ) THEN
                      IF ( lwp ) WRITE(numout,*) " VP rheology error was too large : ", zverr_max, " in outer V-iteration ", i_out, " after ", i_inn, " iterations, we stopped "
                      ll_v_iterate = .FALSE.
                  ENDIF
                  
                  ! - Stop if error small enough
                  IF ( zverr_max < zuerr_min_vp ) THEN                                        
                      IF ( lwp ) WRITE(numout,*) " VP rheology nicely done in outer V-iteration ", i_out, " after ", i_inn, " iterations, finished! "
                      ll_v_iterate = .FALSE.
                  ENDIF
                  
               ENDIF ! ll_v_iterate

            ENDIF ! ---    end ll_u_iterate or ll_v_iterate
               
            !---------------------------------------------------------------------------------------
            !
            ! --- Calculate extra convergence diagnostics and save them
            !
            !---------------------------------------------------------------------------------------
            IF( nn_rhg_chkcvg/=0 .AND. MOD ( i_inn - 1, nn_vp_chkcvg ) == 0 ) THEN

               CALL rhg_cvg_vp( kt, i_out, i_inn, i_inn_tot, nn_vp_nout, nn_vp_ninn, nn_nvp,        &
                      &         u_ice, v_ice, zu_b, zv_b, zu_c, zv_c,                               &
                      &         zmt, za_iU, za_iV, zuerr_max, zverr_max, zglob_area,                &
                      &         zrhsu, zAU, zBU, zCU, zDU, zEU, zFU,                                &
                      &         zrhsv, zAV, zBV, zCV, zDV, zEV, zFV,                                &
                                zvel_res, zvel_diff )

            ENDIF

         END DO ! i_inn, end of inner loop

      END DO ! End of outer loop (i_out) =============================================================================================

      IF( nn_rhg_chkcvg/=0  ) THEN
          
         IF( iom_use('velo_res') )   CALL iom_put( 'velo_res', zvel_res  )   ! linear system residual  @last inner&outer iteration
         IF( iom_use('velo_ero') )   CALL iom_put( 'velo_ero', zvel_diff )   ! abs velocity difference @last outer iteration
         IF( iom_use('uice_eri') )   CALL iom_put( 'uice_eri', zuerr     )   ! abs velocity difference @last inner iteration
         IF( iom_use('vice_eri') )   CALL iom_put( 'vice_eri', zverr     )   ! abs velocity difference @last inner iteration
        
      ENDIF ! nn_rhg_chkcvg

      !------------------------------------------------------------------------------!
      !
      ! --- Recompute delta, shear and div (inputs for mechanical redistribution) 
      !
      !------------------------------------------------------------------------------!
      !
      DO_2D( nn_hls, nn_hls-1, nn_hls, nn_hls-1 ) ! 1->jpj-1; 1->jpi-1

            ! shear at F points
            zds(ji,jj) = ( ( u_ice(ji,jj+1) * r1_e1u(ji,jj+1) - u_ice(ji,jj) * r1_e1u(ji,jj) ) * e1f(ji,jj) * e1f(ji,jj)   &
               &         + ( v_ice(ji+1,jj) * r1_e2v(ji+1,jj) - v_ice(ji,jj) * r1_e2v(ji,jj) ) * e2f(ji,jj) * e2f(ji,jj)   &
               &         ) * r1_e1e2f(ji,jj) * fimask(ji,jj)

      END_2D      
      
      DO_2D( 0, 0, 0, 0 ) ! 2->jpj-1; 2->jpi-1
            
            ! shear**2 at T points (doc eq. A16)
            zds2 = ( zds(ji,jj  ) * zds(ji,jj  ) * e1e2f(ji,jj  ) + zds(ji-1,jj  ) * zds(ji-1,jj  ) * e1e2f(ji-1,jj  )  &
               &   + zds(ji,jj-1) * zds(ji,jj-1) * e1e2f(ji,jj-1) + zds(ji-1,jj-1) * zds(ji-1,jj-1) * e1e2f(ji-1,jj-1)  &
               &   ) * 0.25_wp * r1_e1e2t(ji,jj)
            
            ! tension**2 at T points
            zdt  = ( ( u_ice(ji,jj) * r1_e2u(ji,jj) - u_ice(ji-1,jj) * r1_e2u(ji-1,jj) ) * e2t(ji,jj) * e2t(ji,jj)   &
               &   - ( v_ice(ji,jj) * r1_e1v(ji,jj) - v_ice(ji,jj-1) * r1_e1v(ji,jj-1) ) * e1t(ji,jj) * e1t(ji,jj)   &
               &   ) * r1_e1e2t(ji,jj)
            zdt2 = zdt * zdt
            
            zten_i(ji,jj) = zdt * zmsk(ji,jj)
            
            ! maximum shear rate at T points (includes tension, output only)
            pshear_i(ji,jj) = SQRT( zdt2 + zds2 ) * zmsk(ji,jj)
            
            ! shear at T-points
            zshear(ji,jj)   = SQRT( zds2 ) * zmsk(ji,jj)

            ! divergence at T points
            pdivu_i(ji,jj) = ( e2u(ji,jj) * u_ice(ji,jj) - e2u(ji-1,jj) * u_ice(ji-1,jj)   &
               &             + e1v(ji,jj) * v_ice(ji,jj) - e1v(ji,jj-1) * v_ice(ji,jj-1)   &
               &             ) * r1_e1e2t(ji,jj) * zmsk(ji,jj)
            
            ! delta at T points
            zfac               = SQRT( pdivu_i(ji,jj) * pdivu_i(ji,jj) + ( zdt2 + zds2 ) * z1_ecc2 ) * zmsk(ji,jj) ! delta
            zdelta(ji,jj)      = zfac
            
            ! delta* at T points
            rswitch            =   1._wp - MAX( 0._wp, SIGN( 1._wp, -zfac ) ) ! 0 if delta=0
            pdelta_i(ji,jj)    = zfac + rn_creepl ! * rswitch
           
      END_2D

      CALL lbc_lnk( 'icedyn_rhg_vp', pshear_i, 'T', 1._wp, pdivu_i, 'T', 1._wp, pdelta_i, 'T', 1._wp, &
              &                      zdelta  , 'T', 1._wp, zten_i , 'T', 1._wp, zshear  , 'T', 1._wp )

      ! --- Sea ice stresses at T-points --- !
      IF ( iom_use('normstr')    .OR. iom_use('sheastr')    .OR. &
     &     iom_use('intstrx')    .OR. iom_use('intstry')    .OR. &
     &     iom_use('sig1_pnorm') .OR. iom_use('sig2_pnorm') ) THEN
      
         ! sigma1, sigma2, sigma12 are some recombination of the stresses (HD MWR002, Bouillon et al., OM2013)
         ! not to be confused with stress tensor components, stress invariants, or stress principal components

         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls ) ! 2->jpj-1; 2->jpi-1

            zvisc_t(ji,jj)     =   strength(ji,jj) / pdelta_i(ji,jj) ! update viscosity
            zfac               =   zvisc_t(ji,jj)
            zs1(ji,jj)         =   zfac * ( pdivu_i(ji,jj) - zdelta(ji,jj) )
            zs2(ji,jj)         =   zfac * z1_ecc2 * zten_i(ji,jj)
            zs12(ji,jj)        =   zfac * z1_ecc2 * zshear(ji,jj) * 0.5_wp 

         END_2D

!!$         CALL lbc_lnk( 'icedyn_rhg_vp', zs1, 'T', 1., zs2, 'T', 1., zs12, 'T', 1. )
      
      ENDIF
      
      ! --- Shear (s12) at F-points --- !
      IF ( iom_use('intstrx') .OR. iom_use('intstry') ) THEN

         DO_2D( nn_hls, nn_hls-1, nn_hls, nn_hls-1 ) ! 1->jpj-1; 1->jpi-1
            
               ! P/delta* at F points
               zvisc_f = 0.25_wp * ( zvisc_t(ji,jj) + zvisc_t(ji+1,jj) + zvisc_t(ji,jj+1) + zvisc_t(ji+1,jj+1) )
               
               ! s12 at F-points 
               zs12f(ji,jj) = zvisc_f * z1_ecc2 * zds(ji,jj)
               
         END_2D

         CALL lbc_lnk( 'icedyn_rhg_vp', zs12f, 'F', 1. )
         
      ENDIF
      
      !------------------------------------------------------------------------------!
      !
      ! --- Diagnostics
      !
      !------------------------------------------------------------------------------!
      !

      ! --- Ice-ocean, ice-atm. & ice-ocean bottom (landfast) stresses --- !
      IF(  iom_use('utau_oi') .OR. iom_use('vtau_oi') .OR. iom_use('utau_ai') .OR. iom_use('vtau_ai') .OR. &
         & iom_use('utau_bi') .OR. iom_use('vtau_bi') ) THEN

         ALLOCATE( ztaux_oi(jpi,jpj) , ztauy_oi(jpi,jpj) )

         !--- Recalculate oceanic stress at last inner iteration
         DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 ) ! 2->jpj-1; 2->jpi-1

                !--- ice u-velocity @V points, v-velocity @U points (for non-linear drag computation)
                zu_cV            = 0.25_wp * ( u_ice(ji,jj) + u_ice(ji-1,jj) + u_ice(ji,jj+1) + u_ice(ji-1,jj+1) ) * vmask(ji,jj,1)
                zv_cU            = 0.25_wp * ( v_ice(ji,jj) + v_ice(ji,jj-1) + v_ice(ji+1,jj) + v_ice(ji+1,jj-1) ) * umask(ji,jj,1)
                
                !--- non-linear drag coefficients (need to be updated at each outer loop, see Lemieux and Tremblay JGR09, p.3, beginning of Section 3)
                zCwU(ji,jj)          = za_iU(ji,jj) * zrhoco * SQRT( ( u_ice(ji,jj) - u_oce (ji,jj) ) * ( u_ice(ji,jj) - u_oce (ji,jj) )  &
                  &                                                + ( zv_cU - v_oceU(ji,jj) ) * ( zv_cU - v_oceU(ji,jj) ) )
                zCwV(ji,jj)          = za_iV(ji,jj) * zrhoco * SQRT( ( v_ice(ji,jj) - v_oce (ji,jj) ) * ( v_ice(ji,jj) - v_oce (ji,jj) )  &
                  &                                                + ( zu_cV - u_oceV(ji,jj) ) * ( zu_cV - u_oceV(ji,jj) ) )
                 
                !--- Ocean-ice stress
                ztaux_oi(ji,jj) = zCwU(ji,jj) * ( u_oce(ji,jj) - u_ice(ji,jj) )
                ztauy_oi(ji,jj) = zCwV(ji,jj) * ( v_oce(ji,jj) - v_ice(ji,jj) )
                
         END_2D
         
         !
         CALL lbc_lnk( 'icedyn_rhg_vp', ztaux_oi, 'U', -1., ztauy_oi, 'V', -1., ztaux_ai, 'U', -1., ztauy_ai, 'V', -1. ) !, &
!            &                          ztaux_bi, 'U', -1., ztauy_bi, 'V', -1. )
         !
         CALL iom_put( 'utau_oi' , ztaux_oi * zmsk00 )
         CALL iom_put( 'vtau_oi' , ztauy_oi * zmsk00 )
         CALL iom_put( 'utau_ai' , ztaux_ai * zmsk00 )
         CALL iom_put( 'vtau_ai' , ztauy_ai * zmsk00 )
!        CALL iom_put( 'utau_bi' , ztaux_bi * zmsk00 )
!        CALL iom_put( 'vtau_bi' , ztauy_bi * zmsk00 )

         DEALLOCATE( ztaux_oi , ztauy_oi )

      ENDIF
       
      ! --- Divergence, shear and strength --- !
      IF( iom_use('icediv') )   CALL iom_put( 'icediv' , pdivu_i  * zmsk00 )   ! divergence
      IF( iom_use('iceshe') )   CALL iom_put( 'iceshe' , pshear_i * zmsk00 )   ! maximum shear rate
      IF( iom_use('icedlt') )   CALL iom_put( 'icedlt' , zdelta   * zmsk00 )   ! delta
      IF( iom_use('icestr') )   CALL iom_put( 'icestr' , strength * zmsk00 )   ! strength

      ! --- Stress tensor invariants (SIMIP diags) --- !
      IF( iom_use('normstr') .OR. iom_use('sheastr') ) THEN
         !
         ALLOCATE( zsig_I(jpi,jpj) , zsig_II(jpi,jpj) )
         !         
         ! Stress invariants (sigma_I, sigma_II, Coon 1974, Feltham 2008)
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls ) ! 2->jpj-1; 2->jpi-1
            zsig_I(ji,jj)    =   0.5_wp * zs1(ji,jj)
            zsig_II(ji,jj)   =   0.5_wp * SQRT ( zs2(ji,jj) * zs2(ji,jj) + 4. * zs12(ji,jj) * zs12(ji,jj) )
         END_2D

!!$         CALL lbc_lnk( 'icedyn_rhg_vp', zsig_I, 'T', 1., zsig_II, 'T', 1.)
         
         IF( iom_use('normstr') )   CALL iom_put( 'normstr' ,   zsig_I(:,:)  * zmsk00(:,:) ) ! Normal stress
         IF( iom_use('sheastr') )   CALL iom_put( 'sheastr' ,   zsig_II(:,:) * zmsk00(:,:) ) ! Maximum shear stress
         
         DEALLOCATE ( zsig_I, zsig_II )
         
      ENDIF

      ! --- Normalized stress tensor principal components --- !
      ! These are used to plot the normalized yield curve (Lemieux & Dupont, GMD 2020)
      ! To plot the yield curve and evaluate physical convergence, they have two recommendations
      ! Recommendation 1 : Use ice strength, not replacement pressure
      ! Recommendation 2 : Need to use deformations at PREVIOUS iterate for viscosities (see p. 1765)
      ! R2 means we need to recompute stresses

      IF( iom_use('sig1_pnorm') .OR. iom_use('sig2_pnorm') ) THEN
         !
         ALLOCATE( zsig1_p(jpi,jpj) , zsig2_p(jpi,jpj) , zsig_I(jpi,jpj) , zsig_II(jpi,jpj) )
         !         
         DO_2D( 0, 0, 0, 0 ) ! clem: check bounds
         
               ! Ice stresses computed with **viscosities** (P/delta) at **previous** iterates 
               !                        and **deformations** at current iterates
               !                        following Lemieux & Dupont (2020)
               zfac             =   zvisc_t_prev(ji,jj)
               zsig1            =   zfac * ( pdivu_i(ji,jj) - zdelta(ji,jj) )  
               zsig2            =   zfac * z1_ecc2 * zten_i(ji,jj)
               zsig12           =   zfac * z1_ecc2 * zshear(ji,jj) * 0.5_wp
               
               ! Stress invariants (sigma_I, sigma_II, Coon 1974, Feltham 2008), T-point
               zsig_I(ji,jj)    =   0.5_wp * zsig1                                          ! normal stress
               zsig_II(ji,jj)   =   0.5_wp * SQRT ( zsig2 * zsig2 + 4. *zsig12 * zsig12 )   ! max shear stress

               ! Normalized  principal stresses (used to display the ellipse)
               z1_strength      =   1._wp / MAX ( 1._wp , strength(ji,jj) )
               zsig1_p(ji,jj)   =   ( zsig_I(ji,jj) + zsig_II(ji,jj) ) * z1_strength
               zsig2_p(ji,jj)   =   ( zsig_I(ji,jj) - zsig_II(ji,jj) ) * z1_strength
               
         END_2D
         !
         ! CALL lbc_lnk( 'icedyn_rhg_vp', zsig1_p, 'T', 1., zsig2_p, 'T', 1.)
         !
         CALL iom_put( 'sig1_pnorm' , zsig1_p ) 
         CALL iom_put( 'sig2_pnorm' , zsig2_p ) 

         DEALLOCATE( zsig1_p , zsig2_p , zsig_I , zsig_II )
         
      ENDIF

      ! --- SIMIP, terms of tendency for momentum equation  --- !
      IF(  iom_use('dssh_dx') .OR. iom_use('dssh_dy') .OR. &
         & iom_use('corstrx') .OR. iom_use('corstry') ) THEN

         ! --- Recalculate Coriolis stress at last inner iteration
         DO_2D( 0, 0, 0, 0 ) ! clem: check bounds
                ! --- U-component 
                zCorU(ji,jj)         =   0.25_wp * r1_e1u(ji,jj) *  &
                           &             ( zmf(ji  ,jj) * ( e1v(ji  ,jj) * v_ice(ji  ,jj) + e1v(ji  ,jj-1) * v_ice(ji  ,jj-1) )  &
                           &             + zmf(ji+1,jj) * ( e1v(ji+1,jj) * v_ice(ji+1,jj) + e1v(ji+1,jj-1) * v_ice(ji+1,jj-1) ) )
                zCorV(ji,jj)         = - 0.25_wp * r1_e2v(ji,jj) *  &
                           &             ( zmf(ji,jj  ) * ( e2u(ji,jj  ) * u_ice(ji,jj  ) + e2u(ji-1,jj  ) * u_ice(ji-1,jj  ) )  &
                           &             + zmf(ji,jj+1) * ( e2u(ji,jj+1) * u_ice(ji,jj+1) + e2u(ji-1,jj+1) * u_ice(ji-1,jj+1) ) )
         END_2D
         !
         CALL lbc_lnk( 'icedyn_rhg_vp', zspgU, 'U', -1., zspgV, 'V', -1., &
            &                           zCorU, 'U', -1., zCorV, 'V', -1. )
         !
         CALL iom_put( 'dssh_dx' , zspgU * zmsk00 )   ! Sea-surface tilt term in force balance (x)
         CALL iom_put( 'dssh_dy' , zspgV * zmsk00 )   ! Sea-surface tilt term in force balance (y)
         CALL iom_put( 'corstrx' , zCorU * zmsk00 )   ! Coriolis force term in force balance (x)
         CALL iom_put( 'corstry' , zCorV * zmsk00 )   ! Coriolis force term in force balance (y)

      ENDIF
      
      IF ( iom_use('intstrx') .OR. iom_use('intstry') ) THEN

         ! Recalculate internal forces (divergence of stress tensor) at last inner iteration
         DO_2D( 0, 0, 0, 0 ) ! clem: check bounds

               zfU(ji,jj) = 0.5_wp * ( ( zs1(ji+1,jj) - zs1(ji,jj) ) * e2u(ji,jj)                                             &
                  &                  + ( zs2(ji+1,jj) * e2t(ji+1,jj) * e2t(ji+1,jj) - zs2(ji,jj) * e2t(ji,jj) * e2t(ji,jj)    &
                  &                    ) * r1_e2u(ji,jj)                                                                      &
                  &                  + ( zs12f(ji,jj) * e1f(ji,jj) * e1f(ji,jj) - zs12f(ji,jj-1) * e1f(ji,jj-1) * e1f(ji,jj-1)  &
                  &                    ) * 2._wp * r1_e1u(ji,jj)                                                              &
                  &                  ) * r1_e1e2u(ji,jj)

               zfV(ji,jj) = 0.5_wp * ( ( zs1(ji,jj+1) - zs1(ji,jj) ) * e1v(ji,jj)                                             &
                  &                  - ( zs2(ji,jj+1) * e1t(ji,jj+1) * e1t(ji,jj+1) - zs2(ji,jj) * e1t(ji,jj) * e1t(ji,jj)    &
                  &                    ) * r1_e1v(ji,jj)                                                                      &
                  &                  + ( zs12f(ji,jj) * e2f(ji,jj) * e2f(ji,jj) - zs12f(ji-1,jj) * e2f(ji-1,jj) * e2f(ji-1,jj)  &
                  &                    ) * 2._wp * r1_e2v(ji,jj)                                                              &
                  &                  ) * r1_e1e2v(ji,jj)

         END_2D
            
         CALL lbc_lnk( 'icedyn_rhg_vp', zfU, 'U', -1., zfV, 'V', -1. )
         
         CALL iom_put( 'intstrx' , zfU   * zmsk00 )   ! Internal force term in force balance (x)
         CALL iom_put( 'intstry' , zfV   * zmsk00 )   ! Internal force term in force balance (y)
         
      ENDIF

      ! --- Ice & snow mass and ice area transports
      IF(  iom_use('xmtrpice') .OR. iom_use('ymtrpice') .OR. &
         & iom_use('xmtrpsnw') .OR. iom_use('ymtrpsnw') .OR. iom_use('xatrp') .OR. iom_use('yatrp') ) THEN
         !
         ALLOCATE( zdiag_xmtrp_ice(jpi,jpj) , zdiag_ymtrp_ice(jpi,jpj) , &
            &      zdiag_xmtrp_snw(jpi,jpj) , zdiag_ymtrp_snw(jpi,jpj) , zdiag_xatrp(jpi,jpj) , zdiag_yatrp(jpi,jpj) )
         !
         DO_2D( 0, 0, 0, 0 ) ! clem: check bounds

               zfac_x = 0.5 * u_ice(ji,jj) * e2u(ji,jj) * zmsk00(ji,jj)
               zfac_y = 0.5 * v_ice(ji,jj) * e1v(ji,jj) * zmsk00(ji,jj)

               zdiag_xmtrp_ice(ji,jj) = rhoi * zfac_x * ( vt_i(ji+1,jj) + vt_i(ji,jj) ) ! ice mass transport, X-component
               zdiag_ymtrp_ice(ji,jj) = rhoi * zfac_y * ( vt_i(ji,jj+1) + vt_i(ji,jj) ) !        ''           Y-   ''

               zdiag_xmtrp_snw(ji,jj) = rhos * zfac_x * ( vt_s(ji+1,jj) + vt_s(ji,jj) ) ! snow mass transport, X-component
               zdiag_ymtrp_snw(ji,jj) = rhos * zfac_y * ( vt_s(ji,jj+1) + vt_s(ji,jj) ) !          ''          Y-   ''

               zdiag_xatrp(ji,jj)     = zfac_x * ( at_i(ji+1,jj) + at_i(ji,jj) )        ! area transport,      X-component
               zdiag_yatrp(ji,jj)     = zfac_y * ( at_i(ji,jj+1) + at_i(ji,jj) )        !        ''            Y-   ''

         END_2D
         
         CALL lbc_lnk( 'icedyn_rhg_vp', zdiag_xmtrp_ice, 'U', -1., zdiag_ymtrp_ice, 'V', -1., &
            &                           zdiag_xmtrp_snw, 'U', -1., zdiag_ymtrp_snw, 'V', -1., &
            &                           zdiag_xatrp    , 'U', -1., zdiag_yatrp    , 'V', -1. )

         CALL iom_put( 'xmtrpice' , zdiag_xmtrp_ice )   ! X-component of sea-ice mass transport (kg/s)
         CALL iom_put( 'ymtrpice' , zdiag_ymtrp_ice )   ! Y-component of sea-ice mass transport 
         CALL iom_put( 'xmtrpsnw' , zdiag_xmtrp_snw )   ! X-component of snow mass transport (kg/s)
         CALL iom_put( 'ymtrpsnw' , zdiag_ymtrp_snw )   ! Y-component of snow mass transport
         CALL iom_put( 'xatrp'    , zdiag_xatrp     )   ! X-component of ice area transport
         CALL iom_put( 'yatrp'    , zdiag_yatrp     )   ! Y-component of ice area transport

         DEALLOCATE( zdiag_xmtrp_ice , zdiag_ymtrp_ice , &
            &        zdiag_xmtrp_snw , zdiag_ymtrp_snw , zdiag_xatrp , zdiag_yatrp )

      ENDIF

   END SUBROUTINE ice_dyn_rhg_vp
   
   
   SUBROUTINE rhg_cvg_vp( kt, kitout, kitinn, kitinntot, kitoutmax, kitinnmax, kitinntotmax , &
                  &       pu, pv, pub, pvb, pub_outer, pvb_outer                     , &
                  &       pmt, pat_iu, pat_iv, puerr_max, pverr_max, pglob_area      , &
                  &       prhsu, pAU, pBU, pCU, pDU, pEU, pFU                        , &
                  &       prhsv, pAV, pBV, pCV, pDV, pEV, pFV                        , &   
                  &       pvel_res, pvel_diff                                            )
      !!
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE rhg_cvg_vp  ***
      !!                     
      !! ** Purpose :   check convergence of VP ice rheology
      !!
      !! ** Method  :   create a file ice_cvg.nc containing a few convergence diagnostics
      !!                This routine is called every sub-iteration, so it is cpu expensive
      !!
      !!                Calculates / stores
      !!                   - maximum absolute U-V difference (uice_cvg, u_dif, v_dif, m/s)
      !!                   - residuals in U, V and UV-mean taken as square-root of area-weighted mean square residual (u_res, v_res, vel_res, N/m2)
      !!                   - mean kinetic energy (mke_ice, J/m2)
      !!
      !! ** Note    :   for the first sub-iteration, uice_cvg is set to 0 (too large otherwise)   
      !!
      !!----------------------------------------------------------------------
      !!
      INTEGER ,                 INTENT(in) ::   kt, kitout, kitinn, kitinntot    ! ocean model iterate, outer, inner and total n-iterations
      INTEGER ,                 INTENT(in) ::   kitoutmax, kitinnmax             ! max number of outer & inner iterations
      INTEGER ,                 INTENT(in) ::   kitinntotmax                     ! max number of total sub-iterations
      REAL(wp), DIMENSION(:,:), INTENT(in) ::   pu, pv, pub, pvb                 ! now & sub-iter-before velocities
      REAL(wp), DIMENSION(:,:), INTENT(in) ::   pub_outer, pvb_outer             ! velocities @before outer iterations
      REAL(wp), DIMENSION(:,:), INTENT(in) ::   pmt, pat_iu, pat_iv              ! mass at T-point, ice concentration at U&V
      REAL(wp),                 INTENT(in) ::   puerr_max, pverr_max             ! absolute mean velocity difference
      REAL(wp),                 INTENT(in) ::   pglob_area                       ! global ice area
      REAL(wp), DIMENSION(:,:), INTENT(in) ::   prhsu, pAU, pBU, pCU, pDU, pEU, pFU ! linear system coefficients 
      REAL(wp), DIMENSION(:,:), INTENT(in) ::   prhsv, pAV, pBV, pCV, pDV, pEV, pFV
      REAL(wp), DIMENSION(:,:), INTENT(inout) ::  pvel_res                       ! velocity residual @last inner iteration
      REAL(wp), DIMENSION(:,:), INTENT(inout) ::  pvel_diff                      ! velocity difference @last outer iteration
      !!

      INTEGER           ::   idtime, istatus, ix_dim, iy_dim
      INTEGER           ::   ji, jj          ! dummy loop indices
      INTEGER           ::   it_inn_file, it_out_file
      REAL(wp)          ::   zu_res_mean, zv_res_mean, zvel_res_mean                  ! mean residuals of the linear system
      REAL(wp)          ::   zu_mad, zv_mad, zvel_mad                                 ! mean absolute deviation, sub-iterates
      REAL(wp)          ::   zu_mad_outer, zv_mad_outer, zvel_mad_outer               ! mean absolute deviation, outer-iterates
      REAL(wp)          ::   zvel_err_max, zmke, zu, zv                               ! local scalars
      REAL(wp)          ::   z1_pglob_area                                            ! inverse global ice area

      REAL(wp), DIMENSION(jpi,jpj) ::   zu_res, zv_res, zvel2                         ! local arrays
      REAL(wp), DIMENSION(jpi,jpj) ::   zu_diff, zv_diff                              ! local arrays
                                                                             
      CHARACTER(len=20) ::   clname
      !!----------------------------------------------------------------------


      IF( lwp ) THEN

         WRITE(numout,*)
         WRITE(numout,*) 'rhg_cvg_vp : ice rheology convergence control'
         WRITE(numout,*) '~~~~~~~~~~~'
         WRITE(numout,*) ' kt          =  : ', kt
         WRITE(numout,*) ' kitout      =  : ', kitout
         WRITE(numout,*) ' kitinn      =  : ', kitinn
         WRITE(numout,*) ' kitinntot   =  : ', kitinntot
         WRITE(numout,*) ' kitoutmax (nn_vp_nout) =  ', kitoutmax
         WRITE(numout,*) ' kitinnmax (nn_vp_ninn) =  ', kitinnmax
         WRITE(numout,*) ' kitinntotmax (nn_nvp)  =  ', kitinntotmax
         WRITE(numout,*)

      ENDIF

      z1_pglob_area = 1._wp / pglob_area      ! inverse global ice area

      ! create file
      IF( kt == nit000 .AND. kitinntot == 1 ) THEN
         !
         IF( lwm ) THEN

            clname = 'ice_cvg.nc'
            IF( .NOT. Agrif_Root() )   clname = TRIM(Agrif_CFixed())//"_"//TRIM(clname)
            istatus = NF90_CREATE( TRIM(clname), NF90_CLOBBER, ncvgid )

            istatus = NF90_DEF_DIM( ncvgid, 'time'  , NF90_UNLIMITED, idtime )
            istatus = NF90_DEF_DIM( ncvgid, 'x'     , jpi, ix_dim )
            istatus = NF90_DEF_DIM( ncvgid, 'y'     , jpj, iy_dim )

            istatus = NF90_DEF_VAR( ncvgid, 'u_res'         , NF90_DOUBLE  , (/ idtime /), nvarid_ures )
            istatus = NF90_DEF_VAR( ncvgid, 'v_res'         , NF90_DOUBLE  , (/ idtime /), nvarid_vres )
            istatus = NF90_DEF_VAR( ncvgid, 'vel_res'       , NF90_DOUBLE  , (/ idtime /), nvarid_velres )

            istatus = NF90_DEF_VAR( ncvgid, 'uerr_max_sub'  , NF90_DOUBLE  , (/ idtime /), nvarid_uerr_max )
            istatus = NF90_DEF_VAR( ncvgid, 'verr_max_sub'  , NF90_DOUBLE  , (/ idtime /), nvarid_verr_max )
            istatus = NF90_DEF_VAR( ncvgid, 'velerr_max_sub', NF90_DOUBLE  , (/ idtime /), nvarid_velerr_max )

            istatus = NF90_DEF_VAR( ncvgid, 'umad_sub'      , NF90_DOUBLE  , (/ idtime /), nvarid_umad )
            istatus = NF90_DEF_VAR( ncvgid, 'vmad_sub'      , NF90_DOUBLE  , (/ idtime /), nvarid_vmad )
            istatus = NF90_DEF_VAR( ncvgid, 'velmad_sub'    , NF90_DOUBLE  , (/ idtime /), nvarid_velmad )
            
            istatus = NF90_DEF_VAR( ncvgid, 'umad_outer'    , NF90_DOUBLE  , (/ idtime /), nvarid_umad_outer   )
            istatus = NF90_DEF_VAR( ncvgid, 'vmad_outer'    , NF90_DOUBLE  , (/ idtime /), nvarid_vmad_outer   )
            istatus = NF90_DEF_VAR( ncvgid, 'velmad_outer'  , NF90_DOUBLE  , (/ idtime /), nvarid_velmad_outer )

            istatus = NF90_DEF_VAR( ncvgid, 'mke_ice', NF90_DOUBLE  , (/ idtime /), nvarid_mke )

            istatus = NF90_ENDDEF(ncvgid)

         ENDIF
         !
      ENDIF

      !------------------------------------------------------------
      !
      ! Max absolute velocity difference with previous sub-iterate
      ! ( zvel_err_max )
      !
      !------------------------------------------------------------
      !
      ! This comes from the criterion used to stop the iterative procedure
      zvel_err_max   = 0.5_wp * ( puerr_max + pverr_max ) ! average of U- and V- maximum error over the whole domain

      !----------------------------------------------
      !
      ! Mean-absolute-deviation (sub-iterates)
      ! ( zu_mad, zv_mad, zvel_mad)
      !
      !----------------------------------------------
      !
      ! U
      
      DO_2D( 0, 0, 0, 0 ) !clem check bounds
      
         zu_diff(ji,jj) = ABS ( ( pu(ji,jj) - pub(ji,jj) ) ) * e1e2u(ji,jj) * pat_iu(ji,jj) * umask(ji,jj,1) * z1_pglob_area     
         zv_diff(ji,jj) = ABS ( ( pv(ji,jj) - pvb(ji,jj) ) ) * e1e2v(ji,jj) * pat_iv(ji,jj) * vmask(ji,jj,1) * z1_pglob_area
      
      END_2D

      ! global sum & U-V average
      zu_mad   = glob_sum( 'icedyn_rhg_vp : ', zu_diff )
      zv_mad   = glob_sum( 'icedyn_rhg_vp : ', zv_diff )

      zvel_mad = 0.5_wp * ( zu_mad + zv_mad )

      !-----------------------------------------------
      !
      ! Mean-absolute-deviation (outer-iterates)
      ! ( zu_mad_outer, zv_mad_outer, zvel_mad_outer)
      !
      !-----------------------------------------------
      !
      IF ( kitinn == kitinnmax ) THEN ! only work at the end of outer iterates 
         
         DO_2D( 0, 0, 0, 0 ) !clem check bounds
         
            zu_diff(ji,jj) = ABS ( ( pu(ji,jj) - pub_outer(ji,jj) ) ) * e1e2u(ji,jj) * pat_iu(ji,jj) * umask(ji,jj,1) * &
                              &    z1_pglob_area            
            zv_diff(ji,jj) = ABS ( ( pv(ji,jj) - pvb_outer(ji,jj) ) ) * e1e2v(ji,jj) * pat_iv(ji,jj) * vmask(ji,jj,1) * &
                              &    z1_pglob_area         
         END_2D
         
         ! Global ice-concentration, grid-cell-area weighted mean

         zu_mad_outer   = glob_sum( 'icedyn_rhg_vp : ', zu_diff )
         zv_mad_outer   = glob_sum( 'icedyn_rhg_vp : ', zv_diff )
   
         ! Average of both U & V
         zvel_mad_outer = 0.5_wp * ( zu_mad_outer + zv_mad_outer )
                  
      ENDIF

      ! --- Spatially-resolved absolute difference to send back to main routine 
      ! (last iteration only, T-point)

      IF ( kitinntot == kitinntotmax) THEN

         DO_2D( 0, 0, 0, 0 ) !clem check bounds

               zu_diff(ji,jj) = ( ABS ( ( pu(ji-1,jj) - pub_outer(ji-1,jj) ) ) * umask(ji-1,jj,1) &
    &                           + ABS ( ( pu(ji,jj  ) - pub_outer(ji,jj)   ) ) * umask(ji,jj,1) ) &
    &                           / ( umask(ji-1,jj,1) + umask(ji,jj,1) )

               zv_diff(ji,jj) = ( ABS ( ( pv(ji,jj-1)   - pvb_outer(ji,jj-1) ) ) * vmask(ji,jj-1,1) &
    &                           + ABS ( ( pv(ji,jj  ) - pvb_outer(ji,jj)     ) ) * vmask(ji,jj,1)   &
    &                           / ( vmask(ji,jj-1,1) + vmask(ji,jj,1) ) )
     
               pvel_diff(ji,jj) = 0.5_wp * ( zu_diff(ji,jj) + zv_diff(ji,jj) )
  
         END_2D    
         CALL lbc_lnk( 'icedyn_rhg_cvg_vp', pvel_diff,  'T',  1._wp )

      ELSE

         pvel_diff(:,:) = 0._wp

      ENDIF

      !---------------------------------------
      !
      ! ---  Mean residual & kinetic energy
      !
      !---------------------------------------

      IF ( kitinntot == 1 ) THEN

         zu_res_mean   = 0._wp
         zv_res_mean   = 0._wp
         zvel_res_mean = 0._wp
         zmke          = 0._wp

      ELSE

         ! * Mean residual (N/m2)
         ! Here we take the residual of the linear system (N/m2), 
         ! We define it as in mitgcm: global area-weighted mean of square-root residual
         ! Local residual r = Ax - B expresses to which extent the momentum balance is verified 
         ! i.e., how close we are to a solution
         
         DO_2D( 0, 0, 0, 0 ) !clem check bounds

               zu_res(ji,jj)  = ( prhsu(ji,jj) + pDU(ji,jj) * pu(ji,jj-1) + pEU(ji,jj) * pu(ji,jj+1)               &
                  &             - pAU(ji,jj) * pu(ji-1,jj) - pBU(ji,jj) * pu(ji,jj) - pCU(ji,jj) * pu(ji+1,jj) )
               zv_res(ji,jj)  = ( prhsv(ji,jj) + pDV(ji,jj) * pv(ji-1,jj) + pEV(ji,jj) * pv(ji+1,jj)               &
                  &             - pAV(ji,jj) * pv(ji,jj-1) - pBV(ji,jj) * pv(ji,jj) - pCV(ji,jj) * pv(ji,jj+1) )

!              zu_res(ji,jj)  = pFU(ji,jj) - pAU(ji,jj) * pu(ji-1,jj) - pBU(ji,jj) * pu(ji,jj) - pCU(ji,jj) * pu(ji+1,jj)
!              zv_res(ji,jj)  = pFV(ji,jj) - pAV(ji,jj) * pv(ji,jj-1) - pBV(ji,jj) * pv(ji,jj) - pCV(ji,jj) * pv(ji,jj+1)
   
               zu_res(ji,jj)  = SQRT( zu_res(ji,jj) * zu_res(ji,jj) ) * umask(ji,jj,1) * pat_iu(ji,jj) * e1e2u(ji,jj) * z1_pglob_area
               zv_res(ji,jj)  = SQRT( zv_res(ji,jj) * zv_res(ji,jj) ) * vmask(ji,jj,1) * pat_iv(ji,jj) * e1e2v(ji,jj) * z1_pglob_area
   
         END_2D
         
         ! Global ice-concentration, grid-cell-area weighted mean   
         zu_res_mean   = glob_sum( 'ice_rhg_vp', zu_res(:,:) )
         zv_res_mean   = glob_sum( 'ice_rhg_vp', zv_res(:,:) )
         zvel_res_mean = 0.5_wp * ( zu_res_mean + zv_res_mean )
   
         ! --- Global mean kinetic energy per unit area (J/m2)
         zvel2(:,:) = 0._wp

         DO_2D( 0, 0, 0, 0 ) !clem check bounds
                 
               zu     = 0.5_wp * ( pu(ji-1,jj) + pu(ji,jj) ) ! u-vel at T-point
               zv     = 0.5_wp * ( pv(ji,jj-1) + pv(ji,jj) )
               zvel2(ji,jj)  = zu*zu + zv*zv              ! square of ice velocity at T-point  

         END_2D
                   
         zmke = 0.5_wp * glob_sum( 'ice_rhg_vp', pmt(:,:) * e1e2t(:,:) * zvel2(:,:) ) / pglob_area
   
      ENDIF ! kitinntot

      !--- Spatially-resolved residual at last iteration to send back to main routine (last iteration only)
      !--- Calculation @T-point

      IF ( kitinntot == kitinntotmax) THEN

         DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )

               zu_res(ji,jj)  = ( prhsu(ji,jj) + pDU(ji,jj) * pu(ji,jj-1) + pEU(ji,jj) * pu(ji,jj+1)               &
                  &             - pAU(ji,jj) * pu(ji-1,jj) - pBU(ji,jj) * pu(ji,jj) - pCU(ji,jj) * pu(ji+1,jj) )
               zv_res(ji,jj)  = ( prhsv(ji,jj) + pDV(ji,jj) * pv(ji-1,jj) + pEV(ji,jj) * pv(ji+1,jj)               &
                  &             - pAV(ji,jj) * pv(ji,jj-1) - pBV(ji,jj) * pv(ji,jj) - pCV(ji,jj) * pv(ji,jj+1) )

               zu_res(ji,jj)  = SQRT( zu_res(ji,jj) * zu_res(ji,jj) ) * umask(ji,jj,1) 
               zv_res(ji,jj)  = SQRT( zv_res(ji,jj) * zv_res(ji,jj) ) * vmask(ji,jj,1) 

         END_2D
         
         IF( nn_hls == 1 )   CALL lbc_lnk( 'icedyn_rhg_cvg_vp', zu_res,  'U',  1., zv_res , 'V',  1. )

         DO_2D( 0, 0, 0, 0 ) !clem check bounds
        
               pvel_res(ji,jj) = 0.25_wp * ( zu_res(ji-1,jj) + zu_res(ji,jj) + zv_res(ji,jj-1) + zv_res(ji,jj) )
         
         END_2D
         CALL lbc_lnk( 'icedyn_rhg_cvg_vp', pvel_res, 'T', 1. )

      ELSE

         pvel_res(:,:) = 0._wp

      ENDIF
                  
      !                                                ! ==================== !

      it_inn_file =  ( kt - nit000 ) * kitinntotmax + kitinntot ! time step in the file
      it_out_file =  ( kt - nit000 ) * kitoutmax    + kitout

      ! write variables
      IF( lwm ) THEN

         istatus = NF90_PUT_VAR( ncvgid, nvarid_ures  , (/zu_res_mean/), (/it_inn_file/), (/1/) )        ! Residuals of the linear system, area weighted mean
         istatus = NF90_PUT_VAR( ncvgid, nvarid_vres  , (/zv_res_mean/), (/it_inn_file/), (/1/) )        !
         istatus = NF90_PUT_VAR( ncvgid, nvarid_velres, (/zvel_res_mean/), (/it_inn_file/), (/1/) )      !

         istatus = NF90_PUT_VAR( ncvgid, nvarid_uerr_max  , (/puerr_max/), (/it_inn_file/), (/1/) )      ! Max velocit_inn_filey error, sub-it_inn_fileerates
         istatus = NF90_PUT_VAR( ncvgid, nvarid_verr_max  , (/pverr_max/), (/it_inn_file/), (/1/) )      ! 
         istatus = NF90_PUT_VAR( ncvgid, nvarid_velerr_max, (/zvel_err_max/), (/it_inn_file/), (/1/) )   ! 

         istatus = NF90_PUT_VAR( ncvgid, nvarid_umad    , (/zu_mad/)  , (/it_inn_file/), (/1/) )         ! velocit_inn_filey MAD, area/sic-weighted, sub-it_inn_fileerates
         istatus = NF90_PUT_VAR( ncvgid, nvarid_vmad    , (/zv_mad/)  , (/it_inn_file/), (/1/) )         ! 
         istatus = NF90_PUT_VAR( ncvgid, nvarid_velmad  , (/zvel_mad/), (/it_inn_file/), (/1/) )         ! 

         istatus = NF90_PUT_VAR( ncvgid, nvarid_mke, (/zmke/), (/kitinntot/), (/1/) )                    ! mean kinetic energy

         IF ( kitinn == kitinnmax ) THEN ! only print outer mad at the end of inner loop

            istatus = NF90_PUT_VAR( ncvgid, nvarid_umad_outer    , (/zu_mad_outer/)  , (/it_out_file/), (/1/) )   ! velocity MAD, area/sic-weighted, outer-iterates
            istatus = NF90_PUT_VAR( ncvgid, nvarid_vmad_outer    , (/zv_mad_outer/)  , (/it_out_file/), (/1/) )   !
            istatus = NF90_PUT_VAR( ncvgid, nvarid_velmad_outer  , (/zvel_mad_outer/), (/it_out_file/), (/1/) )   !

         ENDIF

         IF( kt == nitend - nn_fsbc + 1 .AND. kitinntot == kitinntotmax )    istatus = NF90_CLOSE( ncvgid )
      ENDIF
      
   END SUBROUTINE rhg_cvg_vp
   

   
#else
   !!----------------------------------------------------------------------
   !!   Default option         Empty module           NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!==============================================================================
END MODULE icedyn_rhg_vp

