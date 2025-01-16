MODULE zdfosm
   !!======================================================================
   !!                       ***  MODULE  zdfosm  ***
   !! Ocean physics:  vertical mixing coefficient compute from the OSMOSIS
   !!                 turbulent closure parameterization
   !!=====================================================================
   !!  History : NEMO 4.0  ! A. Grant, G. Nurser
   !! 15/03/2017  Changed calculation of pycnocline thickness in unstable conditions and stable conditions AG
   !! 15/03/2017  Calculation of pycnocline gradients for stable conditions changed. Pycnocline gradients now depend on stability of the OSBL. A.G
   !! 06/06/2017  (1) Checks on sign of buoyancy jump in calculation of  OSBL depth.  A.G.
   !!             (2) Removed variable zbrad0, zbradh and zbradav since they are not used.
   !!             (3) Approximate treatment for shear turbulence.
   !!                        Minimum values for zustar and zustke.
   !!                        Add velocity scale, zvstr, that tends to zustar for large Langmuir numbers.
   !!                        Limit maximum value for Langmuir number.
   !!                        Use zvstr in definition of stability parameter zhol.
   !!             (4) Modified parametrization of entrainment flux, changing original coefficient 0.0485 for Langmuir contribution to 0.135 * zla
   !!             (5) For stable boundary layer add factor that depends on length of timestep to 'slow' collapse and growth. Make sure buoyancy jump not negative.
   !!             (6) For unstable conditions when growth is over multiple levels, limit change to maximum of one level per cycle through loop.
   !!             (7) Change lower limits for loops that calculate OSBL averages from 1 to 2. Large gradients between levels 1 and 2 can cause problems.
   !!             (8) Change upper limits from ibld-1 to ibld.
   !!             (9) Calculation of pycnocline thickness in unstable conditions. Check added to ensure that buoyancy jump is positive before calculating Ri.
   !!            (10) Thickness of interface layer at base of the stable OSBL set by Richardson number. Gives continuity in transition from unstable OSBL.
   !!            (11) Checks that buoyancy jump is poitive when calculating pycnocline profiles.
   !!            (12) Replace zwstrl with zvstr in calculation of eddy viscosity.
   !! 27/09/2017 (13) Calculate Stokes drift and Stokes penetration depth from wave information
   !!            (14) Buoyancy flux due to entrainment changed to include contribution from shear turbulence.
   !! 28/09/2017 (15) Calculation of Stokes drift moved into separate do-loops to allow for different options for the determining the Stokes drift to be added.
   !!            (16) Calculation of Stokes drift from windspeed for PM spectrum (for testing, commented out)
   !!            (17) Modification to Langmuir velocity scale to include effects due to the Stokes penetration depth (for testing, commented out)
   !! ??/??/2018 (18) Revision to code structure, selected using key_osmldpth1. Inline code moved into subroutines. Changes to physics made,
   !!                  (a) Pycnocline temperature and salinity profies changed for unstable layers
   !!                  (b) The stable OSBL depth parametrization changed.
   !! 16/05/2019 (19) Fox-Kemper parametrization of restratification through mixed layer eddies added to revised code.
   !! 23/05/19   (20) Old code where key_osmldpth1` is *not* set removed, together with the key key_osmldpth1
   !!             4.2  !  2021-05  (S. Mueller)  Efficiency improvements, source-code clarity enhancements, and adaptation to tiling
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   'ln_zdfosm'                                          OSMOSIS scheme
   !!----------------------------------------------------------------------
   !!   zdf_osm        : update momentum and tracer Kz from osm scheme
   !!      zdf_osm_vertical_average             : compute vertical averages over boundary layers
   !!      zdf_osm_velocity_rotation            : rotate velocity components
   !!         zdf_osm_velocity_rotation_2d      :    rotation of 2d fields
   !!         zdf_osm_velocity_rotation_3d      :    rotation of 3d fields
   !!      zdf_osm_osbl_state                   : determine the state of the OSBL
   !!      zdf_osm_external_gradients           : calculate gradients below the OSBL
   !!      zdf_osm_calculate_dhdt               : calculate rate of change of hbl
   !!      zdf_osm_timestep_hbl                 : hbl timestep
   !!      zdf_osm_pycnocline_thickness         : calculate thickness of pycnocline
   !!      zdf_osm_diffusivity_viscosity        : compute eddy diffusivity and viscosity profiles
   !!      zdf_osm_fgr_terms                    : compute flux-gradient relationship terms
   !!         zdf_osm_pycnocline_buoyancy_profiles : calculate pycnocline buoyancy profiles
   !!      zdf_osm_zmld_horizontal_gradients    : calculate horizontal buoyancy gradients for use with Fox-Kemper parametrization
   !!      zdf_osm_osbl_state_fk                : determine state of OSBL and MLE layers
   !!      zdf_osm_mle_parameters               : timestep MLE depth and calculate MLE fluxes
   !!   zdf_osm_init   : initialization, namelist read, and parameters control
   !!      zdf_osm_alloc                        : memory allocation
   !!   osm_rst        : read (or initialize) and write osmosis restart fields
   !!   tra_osm        : compute and add to the T & S trend the non-local flux
   !!   trc_osm        : compute and add to the passive tracer trend the non-local flux (TBD)
   !!   dyn_osm        : compute and add to u & v trensd the non-local flux
   !!   zdf_osm_iomput : iom_put wrapper that accepts arrays without halo
   !!      zdf_osm_iomput_2d                    : iom_put wrapper for 2D fields
   !!      zdf_osm_iomput_3d                    : iom_put wrapper for 3D fields
   !!----------------------------------------------------------------------
   USE oce                       ! Ocean dynamics and active tracers
   !                             ! Uses ww from previous time step (which is now wb) to calculate hbl
   USE dom_oce                   ! Ocean space and time domain
   USE zdf_oce                   ! Ocean vertical physics
   USE sbc_oce                   ! Surface boundary condition: ocean
   USE sbcwave                   ! Surface wave parameters
   USE phycst                    ! Physical constants
   USE eosbn2                    ! Equation of state
   USE traqsr                    ! Details of solar radiation absorption
   USE zdfdrg, ONLY : rCdU_bot   ! Bottom friction velocity
   USE zdfddm                    ! Double diffusion mixing (avs array)
   USE iom                       ! I/O library
   USE lib_mpp                   ! MPP library
   USE trd_oce                   ! Ocean trends definition
   USE trdtra                    ! Tracers trends
   USE in_out_manager            ! I/O manager
   USE lbclnk                    ! Ocean lateral boundary conditions (or mpp link)
   USE prtctl                    ! Print control
   USE lib_fortran               ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)

   IMPLICIT NONE
   PRIVATE

   ! Public subroutines
   PUBLIC zdf_osm        ! Routine called by step.F90
   PUBLIC zdf_osm_init   ! Routine called by nemogcm.F90
   PUBLIC osm_rst        ! Routine called by step.F90
   PUBLIC tra_osm        ! Routine called by step.F90
   PUBLIC trc_osm        ! Routine called by trcstp.F90
   PUBLIC dyn_osm        ! Routine called by step.F90

   ! Public variables
   LOGICAL,  PUBLIC                                      ::   ln_osm_mle   !: Flag to activate the Mixed Layer Eddy (MLE)
   !                                                                       !     parameterisation, needed by tra_mle_init in
   !                                                                       !     tramle.F90
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   ghamu        !: Non-local u-momentum flux
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   ghamv        !: Non-local v-momentum flux
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   ghamt        !: Non-local temperature flux (gamma/<ws>o)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   ghams        !: Non-local salinity flux (gamma/<ws>o)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   hbl          !: Boundary layer depth
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   hml          !: ML depth
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   hmle         !: Depth of layer affexted by mixed layer eddies in Fox-Kemper parametrization
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   dbdx_mle     !: Zonal buoyancy gradient in ML
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   dbdy_mle     !: Meridional buoyancy gradient in ML
   INTEGER,  PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   mld_prof     !: Level of base of MLE layer

   INTERFACE zdf_osm_velocity_rotation
      !!---------------------------------------------------------------------
      !!              ***  INTERFACE zdf_velocity_rotation  ***
      !!---------------------------------------------------------------------
      MODULE PROCEDURE zdf_osm_velocity_rotation_2d
      MODULE PROCEDURE zdf_osm_velocity_rotation_3d
   END INTERFACE
   !
   INTERFACE zdf_osm_iomput
      !!---------------------------------------------------------------------
      !!                 ***  INTERFACE zdf_osm_iomput  ***
      !!---------------------------------------------------------------------
      MODULE PROCEDURE zdf_osm_iomput_2d
      MODULE PROCEDURE zdf_osm_iomput_3d
   END INTERFACE

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   etmean      ! Averaging operator for avt
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   dh          ! Depth of pycnocline
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   r1_ft       ! Inverse of the modified Coriolis parameter at t-pts
   ! Layer indices
   INTEGER,  ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   nbld        ! Level of boundary layer base
   INTEGER,  ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   nmld        ! Level of mixed-layer depth (pycnocline top)
   ! Layer type
   INTEGER,  ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   n_ddh       ! Type of shear layer
   !                                                              !    n_ddh=0: active shear layer
   !                                                              !    n_ddh=1: shear layer not active
   !                                                              !    n_ddh=2: shear production low
   ! Layer flags
   LOGICAL,  ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   l_conv      ! Unstable/stable bl
   LOGICAL,  ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   l_shear     ! Shear layers
   LOGICAL,  ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   l_coup      ! Coupling to bottom
   LOGICAL,  ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   l_pyc       ! OSBL pycnocline present
   LOGICAL,  ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   l_flux      ! Surface flux extends below OSBL into MLE layer
   LOGICAL,  ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   l_mle       ! MLE layer increases in hickness.
   ! Scales
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   swth0       ! Surface heat flux (Kinematic)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   sws0        ! Surface freshwater flux
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   swb0        ! Surface buoyancy flux
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   suw0        ! Surface u-momentum flux
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   sustar      ! Friction velocity
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   scos_wind   ! Cos angle of surface stress
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   ssin_wind   ! Sin angle of surface stress
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   swthav      ! Heat flux - bl average
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   swsav       ! Freshwater flux - bl average
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   swbav       ! Buoyancy flux - bl average
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   sustke      ! Surface Stokes drift
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   dstokes     ! Penetration depth of the Stokes drift
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   swstrl      ! Langmuir velocity scale
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   swstrc      ! Convective velocity scale
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   sla         ! Trubulent Langmuir number
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   svstr       ! Velocity scale that tends to sustar for large Langmuir number
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   shol        ! Stability parameter for boundary layer
   ! Layer averages: BL
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_t_bl     ! Temperature average
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_s_bl     ! Salinity average
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_u_bl     ! Velocity average (u)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_v_bl     ! Velocity average (v)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_b_bl     ! Buoyancy average
   ! Difference between layer average and parameter at the base of the layer: BL
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_dt_bl    ! Temperature difference
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_ds_bl    ! Salinity difference
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_du_bl    ! Velocity difference (u)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_dv_bl    ! Velocity difference (v)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_db_bl    ! Buoyancy difference
   ! Layer averages: ML
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_t_ml     ! Temperature average
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_s_ml     ! Salinity average
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_u_ml     ! Velocity average (u)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_v_ml     ! Velocity average (v)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_b_ml     ! Buoyancy average
   ! Difference between layer average and parameter at the base of the layer: ML
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_dt_ml    ! Temperature difference
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_ds_ml    ! Salinity difference
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_du_ml    ! Velocity difference (u)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_dv_ml    ! Velocity difference (v)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_db_ml    ! Buoyancy difference
   ! Layer averages: MLE
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_t_mle    ! Temperature average
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_s_mle    ! Salinity average
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_u_mle    ! Velocity average (u)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_v_mle    ! Velocity average (v)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   av_b_mle    ! Buoyancy average
   ! Diagnostic output
   REAL(WP), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   osmdia2d    ! Auxiliary array for diagnostic output
   REAL(WP), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   osmdia3d    ! Auxiliary array for diagnostic output
   LOGICAL  ::   ln_dia_pyc_scl = .FALSE.                         ! Output of pycnocline scalar-gradient profiles
   LOGICAL  ::   ln_dia_pyc_shr = .FALSE.                         ! Output of pycnocline velocity-shear  profiles

   !                                               !!* namelist namzdf_osm *
   LOGICAL  ::   ln_use_osm_la                      ! Use namelist rn_osm_la
   REAL(wp) ::   rn_osm_la                          ! Turbulent Langmuir number
   REAL(wp) ::   rn_osm_dstokes                     ! Depth scale of Stokes drift
   REAL(wp) ::   rn_zdfosm_adjust_sd   = 1.0_wp     ! Factor to reduce Stokes drift by
   REAL(wp) ::   rn_osm_hblfrac        = 0.1_wp     ! For nn_osm_wave = 3/4 specify fraction in top of hbl
   LOGICAL  ::   ln_zdfosm_ice_shelter              ! Flag to activate ice sheltering
   REAL(wp) ::   rn_osm_hbl0           = 10.0_wp    ! Initial value of hbl for 1D runs
   INTEGER  ::   nn_ave                             ! = 0/1 flag for horizontal average on avt
   INTEGER  ::   nn_osm_wave = 0                    ! = 0/1/2 flag for getting stokes drift from La# / PM wind-waves/Inputs into
   !                                                !    sbcwave
   INTEGER  ::   nn_osm_SD_reduce                   ! = 0/1/2 flag for getting effective stokes drift from surface value
   LOGICAL  ::   ln_dia_osm                         ! Use namelist  rn_osm_la
   LOGICAL  ::   ln_kpprimix           = .TRUE.     ! Shear instability mixing
   REAL(wp) ::   rn_riinfty            = 0.7_wp     ! Local Richardson Number limit for shear instability
   REAL(wp) ::   rn_difri              = 0.005_wp   ! Maximum shear mixing at Rig = 0    (m2/s)
   LOGICAL  ::   ln_convmix            = .TRUE.     ! Convective instability mixing
   REAL(wp) ::   rn_difconv            = 1.0_wp     ! Diffusivity when unstable below BL  (m2/s)
   ! OSMOSIS mixed layer eddy parametrization constants
   INTEGER  ::   nn_osm_mle                         ! = 0/1 flag for horizontal average on avt
   REAL(wp) ::   rn_osm_mle_ce                      ! MLE coefficient
   !    Parameters used in nn_osm_mle = 0 case
   REAL(wp) ::   rn_osm_mle_lf                      ! Typical scale of mixed layer front
   REAL(wp) ::   rn_osm_mle_time                    ! Time scale for mixing momentum across the mixed layer
   !    Parameters used in nn_osm_mle = 1 case
   REAL(wp) ::   rn_osm_mle_lat                     ! Reference latitude for a 5 km scale of ML front
   LOGICAL  ::   ln_osm_hmle_limit                  ! If true arbitrarily restrict hmle to rn_osm_hmle_limit*zmld
   REAL(wp) ::   rn_osm_hmle_limit                  ! If ln_osm_hmle_limit true arbitrarily restrict hmle to rn_osm_hmle_limit*zmld
   REAL(wp) ::   rn_osm_mle_rho_c                   ! Density criterion for definition of MLD used by FK
   REAL(wp) ::   rb_c                               ! ML buoyancy criteria = g rho_c /rho0 where rho_c is defined in zdfmld
   REAL(wp) ::   rc_f                               ! MLE coefficient (= rn_ce / (5 km * fo) ) in nn_osm_mle=1 case
   REAL(wp) ::   rn_osm_mle_thresh                  ! Threshold buoyancy for deepening of MLE layer below OSBL base
   REAL(wp) ::   rn_osm_bl_thresh                   ! Threshold buoyancy for deepening of OSBL base
   REAL(wp) ::   rn_osm_mle_tau                     ! Adjustment timescale for MLE

   ! General constants
   REAL(wp) ::   epsln     = 1.0e-20_wp      ! A small positive number to ensure no div by zero
   REAL(wp) ::   depth_tol = 1.0e-6_wp       ! A small-ish positive number to give a hbl slightly shallower than gdepw
   REAL(wp) ::   pthird    = 1.0_wp/3.0_wp   ! 1/3
   REAL(wp) ::   p2third   = 2.0_wp/3.0_wp   ! 2/3

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: zdfosm.F90 14921 2021-05-28 12:19:26Z smueller $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION zdf_osm_alloc()
      !!----------------------------------------------------------------------
      !!                 ***  FUNCTION zdf_osm_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER ::   ierr
      !!----------------------------------------------------------------------
      !
      zdf_osm_alloc = 0
      !
      ALLOCATE( ghamu(jpi,jpj,jpk), ghamv(jpi,jpj,jpk), ghamt(jpi,jpj,jpk), ghams(jpi,jpj,jpk), hbl(jpi,jpj), hml(jpi,jpj),   &
         &      hmle(jpi,jpj),      dbdx_mle(jpi,jpj),  dbdy_mle(jpi,jpj),  mld_prof(jpi,jpj),  STAT=ierr )
      zdf_osm_alloc = zdf_osm_alloc + ierr
      !
      ALLOCATE( etmean(A2D(nn_hls-1),jpk), dh(jpi,jpj), r1_ft(A2D(nn_hls-1)), STAT=ierr )
      zdf_osm_alloc = zdf_osm_alloc + ierr
      !
      ALLOCATE( nbld(jpi,jpj), nmld(A2D(nn_hls-1)), STAT=ierr )
      zdf_osm_alloc = zdf_osm_alloc + ierr
      !
      ALLOCATE( n_ddh(A2D(nn_hls-1)), STAT=ierr )
      zdf_osm_alloc = zdf_osm_alloc + ierr
      !
      ALLOCATE( l_conv(A2D(nn_hls-1)), l_shear(A2D(nn_hls-1)), l_coup(A2D(nn_hls-1)), l_pyc(A2D(nn_hls-1)),   &
         &      l_flux(A2D(nn_hls-1)), l_mle(A2D(nn_hls-1)),   STAT=ierr )
      zdf_osm_alloc = zdf_osm_alloc + ierr
      !
      ALLOCATE( swth0(A2D(nn_hls-1)),  sws0(A2D(nn_hls-1)),      swb0(A2D(nn_hls-1)),      suw0(A2D(nn_hls-1)),      &
         &      sustar(A2D(nn_hls-1)), scos_wind(A2D(nn_hls-1)), ssin_wind(A2D(nn_hls-1)), swthav(A2D(nn_hls-1)),    &
         &      swsav(A2D(nn_hls-1)),  swbav(A2D(nn_hls-1)),     sustke(A2D(nn_hls-1)),    dstokes(A2D(nn_hls-1)),   &
         &      swstrl(A2D(nn_hls-1)), swstrc(A2D(nn_hls-1)),    sla(A2D(nn_hls-1)),       svstr(A2D(nn_hls-1)),     &
         &      shol(A2D(nn_hls-1)),   STAT=ierr )
      zdf_osm_alloc = zdf_osm_alloc + ierr
      !
      ALLOCATE( av_t_bl(jpi,jpj), av_s_bl(jpi,jpj), av_u_bl(jpi,jpj), av_v_bl(jpi,jpj),   &
         &      av_b_bl(jpi,jpj), STAT=ierr)
      zdf_osm_alloc = zdf_osm_alloc + ierr
      !
      ALLOCATE( av_dt_bl(jpi,jpj), av_ds_bl(jpi,jpj), av_du_bl(jpi,jpj), av_dv_bl(jpi,jpj),   &
         &      av_db_bl(jpi,jpj), STAT=ierr)
      zdf_osm_alloc = zdf_osm_alloc + ierr
      !
      ALLOCATE( av_t_ml(jpi,jpj), av_s_ml(jpi,jpj), av_u_ml(jpi,jpj), av_v_ml(jpi,jpj),   &
         &      av_b_ml(jpi,jpj), STAT=ierr)
      zdf_osm_alloc = zdf_osm_alloc + ierr
      !
      ALLOCATE( av_dt_ml(jpi,jpj), av_ds_ml(jpi,jpj), av_du_ml(jpi,jpj), av_dv_ml(jpi,jpj),   &
         &      av_db_ml(jpi,jpj), STAT=ierr)
      zdf_osm_alloc = zdf_osm_alloc + ierr
      !
      ALLOCATE( av_t_mle(jpi,jpj), av_s_mle(jpi,jpj), av_u_mle(jpi,jpj), av_v_mle(jpi,jpj),   &
         &      av_b_mle(jpi,jpj), STAT=ierr)
      zdf_osm_alloc = zdf_osm_alloc + ierr
      !
      IF ( ln_dia_osm ) THEN
         ALLOCATE( osmdia2d(jpi,jpj), osmdia3d(jpi,jpj,jpk), STAT=ierr )
         zdf_osm_alloc = zdf_osm_alloc + ierr
      END IF
      !
      CALL mpp_sum ( 'zdfosm', zdf_osm_alloc )
      IF( zdf_osm_alloc /= 0 ) CALL ctl_warn( 'zdf_osm_alloc: failed to allocate zdf_osm arrays' )
      !
   END FUNCTION zdf_osm_alloc

   SUBROUTINE zdf_osm( kt, Kbb, Kmm, Krhs, p_avm,   &
      &                p_avt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE zdf_osm  ***
      !!
      !! ** Purpose :   Compute the vertical eddy viscosity and diffusivity
      !!      coefficients and non local mixing using the OSMOSIS scheme
      !!
      !! ** Method :   The boundary layer depth hosm is diagnosed at tracer points
      !!      from profiles of buoyancy, and shear, and the surface forcing.
      !!      Above hbl (sigma=-z/hbl <1) the mixing coefficients are computed from
      !!
      !!                      Kx =  hosm  Wx(sigma) G(sigma)
      !!
      !!             and the non local term ghamt = Cs / Ws(sigma) / hosm
      !!      Below hosm  the coefficients are the sum of mixing due to internal waves
      !!      shear instability and double diffusion.
      !!
      !!      -1- Compute the now interior vertical mixing coefficients at all depths.
      !!      -2- Diagnose the boundary layer depth.
      !!      -3- Compute the now boundary layer vertical mixing coefficients.
      !!      -4- Compute the now vertical eddy vicosity and diffusivity.
      !!      -5- Smoothing
      !!
      !!        N.B. The computation is done from jk=2 to jpkm1
      !!             Surface value of avt are set once a time to zero
      !!             in routine zdf_osm_init.
      !!
      !! ** Action  :   update the non-local terms ghamts
      !!                update avt (before vertical eddy coef.)
      !!
      !! References : Large W.G., Mc Williams J.C. and Doney S.C.
      !!         Reviews of Geophysics, 32, 4, November 1994
      !!         Comments in the code refer to this paper, particularly
      !!         the equation number. (LMD94, here after)
      !!----------------------------------------------------------------------
      INTEGER                   , INTENT(in   ) ::  kt               ! Ocean time step
      INTEGER                   , INTENT(in   ) ::  Kbb, Kmm, Krhs   ! Ocean time level indices
      REAL(wp), DIMENSION(:,:,:), INTENT(inout) ::  p_avm, p_avt     ! Momentum and tracer Kz (w-points)
      !!
      INTEGER ::   ji, jj, jk, jl, jm, jkflt   ! Dummy loop indices
      !!
      REAL(wp) ::   zthermal, zbeta
      REAL(wp) ::   zesh2, zri, zfri   ! Interior Richardson mixing
      !! Scales
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zrad0       ! Surface solar temperature flux (deg m/s)
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zradh       ! Radiative flux at bl base (Buoyancy units)
      REAL(wp)                           ::   zradav      ! Radiative flux, bl average (Buoyancy Units)
      REAL(wp)                           ::   zvw0        ! Surface v-momentum flux
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zwb0tot     ! Total surface buoyancy flux including insolation
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zwb_ent     ! Buoyancy entrainment flux
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zwb_min
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zwb_fk_b    ! MLE buoyancy flux averaged over OSBL
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zwb_fk      ! Max MLE buoyancy flux
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zdiff_mle   ! Extra MLE vertical diff
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zvel_mle    ! Velocity scale for dhdt with stable ML and FK
      !! Mixed-layer variables
      INTEGER,  DIMENSION(A2D(nn_hls-1)) ::   jk_nlev  ! Number of levels
      INTEGER,  DIMENSION(A2D(nn_hls-1)) ::   jk_ext   ! Offset for external level
      !!
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zhbl   ! BL depth - grid
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zhml   ! ML depth - grid
      !!
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zhmle   ! MLE depth - grid
      REAL(wp), DIMENSION(A2D(nn_hls))   ::   zmld    ! ML depth on grid
      !!
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zdh                          ! Pycnocline depth - grid
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zdhdt                        ! BL depth tendency
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zdtdz_bl_ext, zdsdz_bl_ext   ! External temperature/salinity gradients
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zdbdz_bl_ext                 ! External buoyancy gradients
      REAL(wp), DIMENSION(A2D(nn_hls))   ::   zdtdx, zdtdy, zdsdx, zdsdy   ! Horizontal gradients for Fox-Kemper parametrization
      !!
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zdbds_mle   ! Magnitude of horizontal buoyancy gradient
      !! Flux-gradient relationship variables
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zshear   ! Shear production
      !!
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zhbl_t   ! Holds boundary layer depth updated by full timestep
      !! For calculating Ri#-dependent mixing
      REAL(wp), DIMENSION(A2D(nn_hls)) ::   z2du     ! u-shear^2
      REAL(wp), DIMENSION(A2D(nn_hls)) ::   z2dv     ! v-shear^2
      REAL(wp)                         ::   zrimix   ! Spatial form of ri#-induced diffusion
      !! Temporary variables
      REAL(wp)                                 ::   znd              ! Temporary non-dimensional depth
      REAL(wp)                                 ::   zz0, zz1, zfac
      REAL(wp)                                 ::   zus_x, zus_y     ! Temporary Stokes drift
      REAL(wp), DIMENSION(A2D(nn_hls-1),jpk)   ::   zviscos          ! Viscosity
      REAL(wp), DIMENSION(A2D(nn_hls-1),jpk)   ::   zdiffut          ! t-diffusivity
      REAL(wp)                                 ::   zabsstke
      REAL(wp)                                 ::   zsqrtpi, z_two_thirds, zthickness
      REAL(wp)                                 ::   z2k_times_thickness, zsqrt_depth, zexp_depth, zf, zexperfc
      !! For debugging
      REAL(wp), PARAMETER ::   pp_large = -1e10_wp
      !!----------------------------------------------------------------------
      !
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         nmld(ji,jj)   = 0
         sustke(ji,jj) = pp_large
         l_pyc(ji,jj)  = .FALSE.
         l_flux(ji,jj) = .FALSE.
         l_mle(ji,jj)  = .FALSE.
      END_2D
      ! Mixed layer
      ! No initialization of zhbl or zhml (or zdh?)
      zhbl(:,:) = pp_large
      zhml(:,:) = pp_large
      zdh(:,:)  = pp_large
      !
      IF ( ln_osm_mle ) THEN   ! Only initialise arrays if needed
         zdtdx(:,:)  = pp_large ; zdtdy(:,:)    = pp_large ; zdsdx(:,:)     = pp_large
         zdsdy(:,:)  = pp_large
         zwb_fk(:,:) = pp_large ; zvel_mle(:,:) = pp_large
         zhmle(:,:)  = pp_large ; zmld(:,:)     = pp_large
         DO_2D_OVR( nn_hls, nn_hls, nn_hls, nn_hls )
            dbdx_mle(ji,jj) = pp_large
            dbdy_mle(ji,jj) = pp_large
         END_2D
      ENDIF
      zhbl_t(:,:)   = pp_large
      !
      zdiffut(:,:,:) = 0.0_wp
      zviscos(:,:,:) = 0.0_wp
      !
      DO_3D_OVR( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpk )
         ghamt(ji,jj,jk) = pp_large
         ghams(ji,jj,jk) = pp_large
         ghamu(ji,jj,jk) = pp_large
         ghamv(ji,jj,jk) = pp_large
      END_3D
      DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 1, jpk )
         ghamt(ji,jj,jk) = 0.0_wp
         ghams(ji,jj,jk) = 0.0_wp
         ghamu(ji,jj,jk) = 0.0_wp
         ghamv(ji,jj,jk) = 0.0_wp
      END_3D
      !
      zdiff_mle(:,:) = 0.0_wp
      !
      ! Ensure only positive hbl values are accessed when using extended halo
      ! (nn_hls==2)
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         hbl(ji,jj) = MAX( hbl(ji,jj), epsln )
      END_2D
      !
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Calculate boundary layer scales
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !
      ! Turbulent surface fluxes and fluxes averaged over depth of the OSBL
      zz0 =           rn_abs   ! Assume two-band radiation model for depth of OSBL - surface equi-partition in 2-bands
      zz1 =  1.0_wp - rn_abs
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         zrad0(ji,jj)  = qsr(ji,jj) * r1_rho0_rcp   ! Surface downward irradiance (so always +ve)
         zradh(ji,jj)  = zrad0(ji,jj) *                                &   ! Downwards irradiance at base of boundary layer
            &            ( zz0 * EXP( -1.0_wp * hbl(ji,jj) / rn_si0 ) + zz1 * EXP( -1.0_wp * hbl(ji,jj) / rn_si1 ) )
         zradav        = zrad0(ji,jj) *                                              &            ! Downwards irradiance averaged
            &            ( zz0 * ( 1.0_wp - EXP( -hbl(ji,jj)/rn_si0 ) ) * rn_si0 +   &            !    over depth of the OSBL
            &              zz1 * ( 1.0_wp - EXP( -hbl(ji,jj)/rn_si1 ) ) * rn_si1 ) / hbl(ji,jj)
         swth0(ji,jj)  = - qns(ji,jj) * r1_rho0_rcp * tmask(ji,jj,1)   ! Upwards surface Temperature flux for non-local term
         swthav(ji,jj) = 0.5_wp * swth0(ji,jj) - ( 0.5_wp * ( zrad0(ji,jj) + zradh(ji,jj) ) -   &   ! Turbulent heat flux averaged
            &                                                 zradav )                              !    over depth of OSBL
      END_2D
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         sws0(ji,jj)    = -1.0_wp * ( ( emp(ji,jj) - rnf(ji,jj) ) * ts(ji,jj,1,jp_sal,Kmm) +   &   ! Upwards surface salinity flux
            &                         sfx(ji,jj) ) * r1_rho0 * tmask(ji,jj,1)                      !    for non-local term
         zthermal       = rab_n(ji,jj,1,jp_tem)
         zbeta          = rab_n(ji,jj,1,jp_sal)
         swb0(ji,jj)    = grav * zthermal * swth0(ji,jj) - grav * zbeta * sws0(ji,jj)   ! Non radiative upwards surface buoyancy flux
         zwb0tot(ji,jj) = swb0(ji,jj) - grav * zthermal * ( zrad0(ji,jj) - zradh(ji,jj) )   ! Total upwards surface buoyancy flux
         swsav(ji,jj)   = 0.5_wp * sws0(ji,jj)                              ! Turbulent salinity flux averaged over depth of the OBSL
         swbav(ji,jj)   = grav  * zthermal * swthav(ji,jj) -            &   ! Turbulent buoyancy flux averaged over the depth of the
            &             grav  * zbeta * swsav(ji,jj)                      ! OBSBL
      END_2D
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         suw0(ji,jj)    = -0.5_wp * (utau(ji-1,jj) + utau(ji,jj)) * r1_rho0 * tmask(ji,jj,1)   ! Surface upward velocity fluxes
         zvw0           = -0.5_wp * (vtau(ji,jj-1) + vtau(ji,jj)) * r1_rho0 * tmask(ji,jj,1)
         sustar(ji,jj)  = MAX( SQRT( SQRT( suw0(ji,jj) * suw0(ji,jj) + zvw0 * zvw0 ) ),   &   ! Friction velocity (sustar), at
            &                  1e-8_wp )                                                      !    T-point : LMD94 eq. 2
         scos_wind(ji,jj) = -1.0_wp * suw0(ji,jj) / ( sustar(ji,jj) * sustar(ji,jj) )
         ssin_wind(ji,jj) = -1.0_wp * zvw0        / ( sustar(ji,jj) * sustar(ji,jj) )
      END_2D
      ! Calculate Stokes drift in direction of wind (sustke) and Stokes penetration depth (dstokes)
      SELECT CASE (nn_osm_wave)
         ! Assume constant La#=0.3
      CASE(0)
         DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
            zus_x = scos_wind(ji,jj) * sustar(ji,jj) / 0.3_wp**2
            zus_y = ssin_wind(ji,jj) * sustar(ji,jj) / 0.3_wp**2
            ! Linearly
            sustke(ji,jj)  = MAX( SQRT( zus_x * zus_x + zus_y * zus_y ), 1e-8_wp )
            dstokes(ji,jj) = rn_osm_dstokes
         END_2D
         ! Assume Pierson-Moskovitz wind-wave spectrum
      CASE(1)
         DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
            ! Use wind speed wndm included in sbc_oce module
            sustke(ji,jj)  = MAX ( 0.016_wp * wndm(ji,jj), 1e-8_wp )
            dstokes(ji,jj) = MAX ( 0.12_wp * wndm(ji,jj)**2 / grav, 5e-1_wp )
         END_2D
         ! Use ECMWF wave fields as output from SBCWAVE
      CASE(2)
         zfac =  2.0_wp * rpi / 16.0_wp
         !
         DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
            IF ( hsw(ji,jj) > 1e-4_wp ) THEN
               ! Use  wave fields
               zabsstke       = SQRT( ut0sd(ji,jj)**2 + vt0sd(ji,jj)**2 )
               sustke(ji,jj)  = MAX( ( scos_wind(ji,jj) * ut0sd(ji,jj) + ssin_wind(ji,jj)  * vt0sd(ji,jj) ), 1e-8_wp )
               dstokes(ji,jj) = MAX( zfac * hsw(ji,jj) * hsw(ji,jj) / ( MAX( zabsstke * wmp(ji,jj), 1e-7 ) ), 5e-1_wp )
            ELSE
               ! Assume masking issue (e.g. ice in ECMWF reanalysis but not in model run)
               ! .. so default to Pierson-Moskowitz
               sustke(ji,jj)  = MAX( 0.016_wp * wndm(ji,jj), 1e-8_wp )
               dstokes(ji,jj) = MAX( 0.12_wp * wndm(ji,jj)**2 / grav, 5e-1_wp )
            END IF
         END_2D
      END SELECT
      !
      IF (ln_zdfosm_ice_shelter) THEN
         ! Reduce both Stokes drift and its depth scale by ocean fraction to represent sheltering by ice
         DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
            sustke(ji,jj)  = sustke(ji,jj)  * ( 1.0_wp - fr_i(ji,jj) )
            dstokes(ji,jj) = dstokes(ji,jj) * ( 1.0_wp - fr_i(ji,jj) )
         END_2D
      END IF
      !
      SELECT CASE (nn_osm_SD_reduce)
         ! Reduce surface Stokes drift by a constant factor or following Breivik (2016) + van Roekel (2012) or Grant (2020).
      CASE(0)
         ! The Langmur number from the ECMWF model (or from PM) appears to give La<0.3 for wind-driven seas.
         ! The coefficient rn_zdfosm_adjust_sd = 0.8 gives La=0.3 in this situation.
         ! It could represent the effects of the spread of wave directions around the mean wind. The effect of this adjustment needs to be tested.
         IF(nn_osm_wave > 0) THEN
            DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
               sustke(ji,jj) = rn_zdfosm_adjust_sd * sustke(ji,jj)
            END_2D
         END IF
      CASE(1)
         ! Van Roekel (2012): consider average SD over top 10% of boundary layer
         ! Assumes approximate depth profile of SD from Breivik (2016)
         zsqrtpi = SQRT(rpi)
         z_two_thirds = 2.0_wp / 3.0_wp
         DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
            zthickness = rn_osm_hblfrac*hbl(ji,jj)
            z2k_times_thickness =  zthickness * 2.0_wp / MAX( ABS( 5.97_wp * dstokes(ji,jj) ), 1e-7_wp )
            zsqrt_depth = SQRT( z2k_times_thickness )
            zexp_depth  = EXP( -1.0_wp * z2k_times_thickness )
            sustke(ji,jj) = sustke(ji,jj) * ( 1.0_wp - zexp_depth -   &
               &                              z_two_thirds * ( zsqrtpi * zsqrt_depth * z2k_times_thickness * ERFC(zsqrt_depth) +   &
               &                                               1.0_wp - ( 1.0_wp + z2k_times_thickness ) * zexp_depth ) ) /        &
               &            z2k_times_thickness
         END_2D
      CASE(2)
         ! Grant (2020): Match to exponential with same SD and d/dz(Sd) at depth 10% of boundary layer
         ! Assumes approximate depth profile of SD from Breivik (2016)
         zsqrtpi = SQRT(rpi)
         DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
            zthickness = rn_osm_hblfrac*hbl(ji,jj)
            z2k_times_thickness =  zthickness * 2.0_wp / MAX( ABS( 5.97_wp * dstokes(ji,jj) ), 1e-7_wp )
            IF( z2k_times_thickness < 50.0_wp ) THEN
               zsqrt_depth = SQRT( z2k_times_thickness )
               zexperfc    = zsqrtpi * zsqrt_depth * ERFC(zsqrt_depth) * EXP( z2k_times_thickness )
            ELSE
               ! Asymptotic expansion of sqrt(pi)*zsqrt_depth*EXP(z2k_times_thickness)*ERFC(zsqrt_depth) for large
               !    z2k_times_thickness
               ! See Abramowitz and Stegun, Eq. 7.1.23
               ! zexperfc = 1._wp - (1/2)/(z2k_times_thickness) + (3/4)/(z2k_times_thickness**2) - (15/8)/(z2k_times_thickness**3)
               zexperfc = ( ( -1.875_wp / z2k_times_thickness + 0.75_wp ) / z2k_times_thickness - 0.5_wp ) /   &
                  &       z2k_times_thickness + 1.0_wp
            END IF
            zf = z2k_times_thickness * ( 1.0_wp / zexperfc - 1.0_wp )
            dstokes(ji,jj) = 5.97_wp * zf * dstokes(ji,jj)
            sustke(ji,jj)  = sustke(ji,jj) * EXP( z2k_times_thickness * ( 1.0_wp / ( 2.0_wp * zf ) - 1.0_wp ) ) *   &
               &             ( 1.0_wp - zexperfc )
         END_2D
      END SELECT
      !
      ! Langmuir velocity scale (swstrl), La # (sla)
      ! Mixed scale (svstr), convective velocity scale (swstrc)
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         ! Langmuir velocity scale (swstrl), at T-point
         swstrl(ji,jj) = ( sustar(ji,jj) * sustar(ji,jj) * sustke(ji,jj) )**pthird
         sla(ji,jj)    = MAX( MIN( SQRT( sustar(ji,jj) / ( swstrl(ji,jj) + epsln ) )**3, 4.0_wp ), 0.2_wp )
         IF ( sla(ji,jj) > 0.45_wp ) dstokes(ji,jj) = MIN( dstokes(ji,jj), 0.5_wp * hbl(ji,jj) )
         ! Velocity scale that tends to sustar for large Langmuir numbers
         svstr(ji,jj)  = ( swstrl(ji,jj)**3 + ( 1.0_wp - EXP( -0.5_wp * sla(ji,jj)**2 ) ) * sustar(ji,jj) * sustar(ji,jj) *   &
            &                                 sustar(ji,jj) )**pthird
         !
         ! Limit maximum value of Langmuir number as approximate treatment for shear turbulence
         ! Note sustke and swstrl are not amended
         !
         ! Get convective velocity (swstrc), stabilty scale (shol) and logical conection flag l_conv
         IF ( swbav(ji,jj) > 0.0_wp ) THEN
            swstrc(ji,jj) = ( 2.0_wp * swbav(ji,jj) * 0.9_wp * hbl(ji,jj) )**pthird
            shol(ji,jj)   = -0.9_wp * hbl(ji,jj) * 2.0_wp * swbav(ji,jj) / ( svstr(ji,jj)**3 + epsln )
         ELSE
            swstrc(ji,jj) = 0.0_wp
            shol(ji,jj)   = -1.0_wp * hbl(ji,jj) * 2.0_wp * swbav(ji,jj) / ( svstr(ji,jj)**3  + epsln )
         ENDIF
      END_2D
      !
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Mixed-layer model - calculate averages over the boundary layer, and the change in the boundary layer depth
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ! BL must be always 4 levels deep.
      ! For calculation of lateral buoyancy gradients for FK in
      ! zdf_osm_zmld_horizontal_gradients need halo values for nbld
      !
      ! agn 23/6/20: not clear all this is needed, as hbl checked after it is re-calculated anyway
      ! ##########################################################################
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         hbl(ji,jj) = MAX(hbl(ji,jj), gdepw(ji,jj,4,Kmm) )
      END_2D
      DO_2D_OVR( nn_hls, nn_hls, nn_hls, nn_hls )
         nbld(ji,jj) = 4
      END_2D
      DO_3D_OVR( nn_hls, nn_hls, nn_hls, nn_hls, 5, jpkm1 )
         IF ( MAX( hbl(ji,jj), gdepw(ji,jj,4,Kmm) ) >= gdepw(ji,jj,jk,Kmm) ) THEN
            nbld(ji,jj) = MIN(mbkt(ji,jj)-2, jk)
         ENDIF
      END_3D
      ! ##########################################################################
      !
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         zhbl(ji,jj) = gdepw(ji,jj,nbld(ji,jj),Kmm)
         nmld(ji,jj) = MAX( 3, nbld(ji,jj) - MAX( INT( dh(ji,jj) / e3t(ji,jj,nbld(ji,jj)-1,Kmm) ), 1 ) )
         zhml(ji,jj) = gdepw(ji,jj,nmld(ji,jj),Kmm)
         zdh(ji,jj) = zhbl(ji,jj) - zhml(ji,jj)
      END_2D
      !
      ! Averages over well-mixed and boundary layer, note BL averages use jk_ext=2 everywhere
      jk_nlev(:,:) = nbld(A2D(nn_hls-1))
      jk_ext(:,:) = 1   ! ag 19/03
      CALL zdf_osm_vertical_average( Kbb,      Kmm,      jk_nlev,  av_t_bl,  av_s_bl,    &
         &                           av_b_bl,  av_u_bl,  av_v_bl,  jk_ext,   av_dt_bl,   &
         &                           av_ds_bl, av_db_bl, av_du_bl, av_dv_bl )
      jk_nlev(:,:) = nmld(A2D(nn_hls-1)) - 1
      jk_ext(:,:) = nbld(A2D(nn_hls-1)) - nmld(A2D(nn_hls-1)) + jk_ext(:,:) + 1   ! ag 19/03
      CALL zdf_osm_vertical_average( Kbb,      Kmm,      jk_nlev,  av_t_ml,  av_s_ml,    &
         &                           av_b_ml,  av_u_ml,  av_v_ml,  jk_ext,   av_dt_ml,   &
         &                           av_ds_ml, av_db_ml, av_du_ml, av_dv_ml )
      ! Velocity components in frame aligned with surface stress
      CALL zdf_osm_velocity_rotation( av_u_ml,  av_v_ml  )
      CALL zdf_osm_velocity_rotation( av_du_ml, av_dv_ml )
      CALL zdf_osm_velocity_rotation( av_u_bl,  av_v_bl  )
      CALL zdf_osm_velocity_rotation( av_du_bl, av_dv_bl )
      !
      ! Determine the state of the OSBL, stable/unstable, shear/no shear
      CALL zdf_osm_osbl_state( Kmm, zwb_ent, zwb_min, zshear, zhbl,     &
         &                     zhml, zdh )
      !
      IF ( ln_osm_mle ) THEN
         ! Fox-Kemper Scheme
         DO_2D_OVR( nn_hls, nn_hls, nn_hls, nn_hls )
            mld_prof(ji,jj) = 4
         END_2D
         DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 5, jpkm1 )
            IF ( hmle(ji,jj) >= gdepw(ji,jj,jk,Kmm) ) mld_prof(ji,jj) = MIN( mbkt(ji,jj), jk)
         END_3D
         jk_nlev(:,:) = mld_prof(A2D(nn_hls-1))
         CALL zdf_osm_vertical_average( Kbb,      Kmm,      jk_nlev,  av_t_mle, av_s_mle,   &
            &                           av_b_mle, av_u_mle, av_v_mle )
         !
         DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
            zhmle(ji,jj) = gdepw(ji,jj,mld_prof(ji,jj),Kmm)
         END_2D
         !
         ! Calculate fairly-well-mixed depth zmld & its index mld_prof + lateral zmld-averaged gradients
         CALL zdf_osm_zmld_horizontal_gradients( Kmm, zmld, zdtdx, zdtdy, zdsdx,   &
            &                                    zdsdy, zdbds_mle )
         ! Calculate max vertical FK flux zwb_fk & set logical descriptors
         CALL zdf_osm_osbl_state_fk( Kmm, zwb_fk, zhbl, zhmle, zwb_ent,   &
            &                        zdbds_mle )
         ! Recalculate hmle, zmle, zvel_mle, zdiff_mle & redefine mld_proc to be index for new hmle
         CALL zdf_osm_mle_parameters( Kmm, zmld, zhmle, zvel_mle, zdiff_mle,   &
            &                         zdbds_mle, zhbl, zwb0tot )
      ELSE    ! ln_osm_mle
         ! FK not selected, Boundary Layer only.
         DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
            l_pyc(ji,jj)  = .TRUE.
            l_flux(ji,jj) = .FALSE.
            l_mle(ji,jj)  = .FALSE.
            IF ( l_conv(ji,jj) .AND. av_db_bl(ji,jj) < rn_osm_bl_thresh ) l_pyc(ji,jj) = .FALSE.
         END_2D
      ENDIF   ! ln_osm_mle
      !
      !! External gradient below BL needed both with and w/o FK
      jk_ext(:,:) = nbld(A2D(nn_hls-1)) + 1
      CALL zdf_osm_external_gradients( Kmm, jk_ext, zdtdz_bl_ext, zdsdz_bl_ext, zdbdz_bl_ext )   ! ag 19/03
      !
      ! Test if pycnocline well resolved
      !      DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )                                         Removed with ag 19/03 changes. A change in eddy diffusivity/viscosity
      !         IF (l_conv(ji,jj) ) THEN                                  should account for this.
      !            ztmp = 0.2 * zhbl(ji,jj) / e3w(ji,jj,nbld(ji,jj),Kmm)
      !            IF ( ztmp > 6 ) THEN
      !               ! pycnocline well resolved
      !               jk_ext(ji,jj) = 1
      !            ELSE
      !               ! pycnocline poorly resolved
      !               jk_ext(ji,jj) = 0
      !            ENDIF
      !         ELSE
      !            ! Stable conditions
      !            jk_ext(ji,jj) = 0
      !         ENDIF
      !      END_2D
      !
      ! Recalculate bl averages using jk_ext & ml averages .... note no rotation of u & v here..
      jk_nlev(:,:) = nbld(A2D(nn_hls-1))
      jk_ext(:,:) = 1   ! ag 19/03
      CALL zdf_osm_vertical_average( Kbb,      Kmm,      jk_nlev,  av_t_bl,  av_s_bl,    &
         &                           av_b_bl,  av_u_bl,  av_v_bl,  jk_ext,   av_dt_bl,   &
         &                           av_ds_bl, av_db_bl, av_du_bl, av_dv_bl )
      jk_nlev(:,:) = nmld(A2D(nn_hls-1)) - 1
      jk_ext(:,:) = nbld(A2D(nn_hls-1)) - nmld(A2D(nn_hls-1)) + jk_ext(:,:) + 1   ! ag 19/03
      CALL zdf_osm_vertical_average( Kbb,      Kmm,      jk_nlev,  av_t_ml,  av_s_ml,    &
         &                           av_b_ml,  av_u_ml,  av_v_ml,  jk_ext,   av_dt_ml,   &
         &                           av_ds_ml, av_db_ml, av_du_ml, av_dv_ml )   ! ag 19/03
      !
      ! Rate of change of hbl
      CALL zdf_osm_calculate_dhdt( zdhdt, zhbl, zdh, zwb_ent, zwb_min,   &
         &                         zdbdz_bl_ext, zwb_fk_b, zwb_fk, zvel_mle )
      ! Test if surface boundary layer coupled to bottom
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         l_coup(ji,jj) = .FALSE.   ! ag 19/03
         zhbl_t(ji,jj) = hbl(ji,jj) + ( zdhdt(ji,jj) - ww(ji,jj,nbld(ji,jj)) ) * rn_Dt   ! Certainly need ww here, so subtract it
         ! Adjustment to represent limiting by ocean bottom
         IF ( mbkt(ji,jj) > 2 ) THEN   ! To ensure mbkt(ji,jj) - 2 > 0 so no incorrect array access
            IF ( zhbl_t(ji,jj) > gdepw(ji, jj,mbkt(ji,jj)-2,Kmm) ) THEN
               zhbl_t(ji,jj) = MIN( zhbl_t(ji,jj), gdepw(ji,jj,mbkt(ji,jj)-2,Kmm) )   ! ht(:,:))
               l_pyc(ji,jj)  = .FALSE.
               l_coup(ji,jj) = .TRUE.   ! ag 19/03
            END IF
         END IF
      END_2D
      !
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         nmld(ji,jj) = nbld(ji,jj)           ! use nmld to hold previous blayer index
         nbld(ji,jj) = 4
      END_2D
      !
      DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 4, jpkm1 )
         IF ( zhbl_t(ji,jj) >= gdepw(ji,jj,jk,Kmm) ) THEN
            nbld(ji,jj) = jk
         END IF
      END_3D
      !
      !
      ! Step through model levels taking account of buoyancy change to determine the effect on dhdt
      !
      CALL zdf_osm_timestep_hbl( Kmm, zdhdt, zhbl, zhbl_t, zwb_ent,   &
         &                       zwb_fk_b )
      ! Is external level in bounds?
      !
      ! Recalculate BL averages and differences using new BL depth
      jk_nlev(:,:) = nbld(A2D(nn_hls-1))
      jk_ext(:,:) = 1   ! ag 19/03
      CALL zdf_osm_vertical_average( Kbb,      Kmm,      jk_nlev,  av_t_bl,  av_s_bl,    &
         &                           av_b_bl,  av_u_bl,  av_v_bl,  jk_ext,   av_dt_bl,   &
         &                           av_ds_bl, av_db_bl, av_du_bl, av_dv_bl )
      !
      CALL zdf_osm_pycnocline_thickness( Kmm, zdh, zhml, zdhdt, zhbl,   &
         &                               zwb_ent, zdbdz_bl_ext, zwb_fk_b )
      !
      ! Reset l_pyc before calculating terms in the flux-gradient relationship
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         IF ( av_db_bl(ji,jj) < rn_osm_bl_thresh .OR. nbld(ji,jj) >= mbkt(ji,jj) - 2 .OR.   &
            & nbld(ji,jj) - nmld(ji,jj) == 1   .OR. zdhdt(ji,jj) < 0.0_wp ) THEN   ! ag 19/03
            l_pyc(ji,jj) = .FALSE.   ! ag 19/03
            IF ( nbld(ji,jj) >= mbkt(ji,jj) -2 ) THEN
               nmld(ji,jj) = nbld(ji,jj) - 1                                               ! ag 19/03
               zdh(ji,jj)  = gdepw(ji,jj,nbld(ji,jj),Kmm) - gdepw(ji,jj,nmld(ji,jj),Kmm)   ! ag 19/03
               zhml(ji,jj) = gdepw(ji,jj,nmld(ji,jj),Kmm)                                  ! ag 19/03
               dh(ji,jj)   = zdh(ji,jj)                                                    ! ag 19/03  
               hml(ji,jj)  = hbl(ji,jj) - dh(ji,jj)                                        ! ag 19/03
            ENDIF
         ENDIF                                              ! ag 19/03
      END_2D
      !
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )               ! Limit delta for shallow boundary layers for calculating
         dstokes(ji,jj) = MIN ( dstokes(ji,jj), hbl(ji,jj) / 3.0_wp )   !    flux-gradient terms
      END_2D
      !                                                       
      !
      ! Average over the depth of the mixed layer in the convective boundary layer
      !      jk_ext = nbld - nmld + 1
      ! Recalculate ML averages and differences using new ML depth
      jk_nlev(:,:) = nmld(A2D(nn_hls-1)) - 1
      jk_ext(:,:) = nbld(A2D(nn_hls-1)) - nmld(A2D(nn_hls-1)) + jk_ext(:,:) + 1   ! ag 19/03
      CALL zdf_osm_vertical_average( Kbb,      Kmm,      jk_nlev,  av_t_ml,  av_s_ml,    &
         &                           av_b_ml,  av_u_ml,  av_v_ml,  jk_ext,   av_dt_ml,   &
         &                           av_ds_ml, av_db_ml, av_du_ml, av_dv_ml )
      !
      jk_ext(:,:) = nbld(A2D(nn_hls-1)) + 1
      CALL zdf_osm_external_gradients( Kmm, jk_ext, zdtdz_bl_ext, zdsdz_bl_ext, zdbdz_bl_ext )
      ! Rotate mean currents and changes onto wind aligned co-ordinates
      CALL zdf_osm_velocity_rotation( av_u_ml,  av_v_ml  )
      CALL zdf_osm_velocity_rotation( av_du_ml, av_dv_ml )
      CALL zdf_osm_velocity_rotation( av_u_bl,  av_v_bl  )
      CALL zdf_osm_velocity_rotation( av_du_bl, av_dv_bl )
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Eddy viscosity/diffusivity and non-gradient terms in the flux-gradient relationship
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      CALL zdf_osm_diffusivity_viscosity( Kbb, Kmm, zdiffut, zviscos, zhbl,    &
         &                                zhml, zdh, zdhdt, zshear, zwb_ent,   &
         &                                zwb_min )
      !
      ! Calculate non-gradient components of the flux-gradient relationships
      ! --------------------------------------------------------------------
      jk_ext(:,:) = 1   ! ag 19/03
      CALL zdf_osm_fgr_terms( Kmm, jk_ext, zhbl, zhml, zdh,                              &
         &                    zdhdt, zshear, zdtdz_bl_ext, zdsdz_bl_ext, zdbdz_bl_ext,   &
         &                    zdiffut, zviscos )
      !
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Need to put in code for contributions that are applied explicitly to
      ! the prognostic variables
      !  1. Entrainment flux
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !
      ! Rotate non-gradient velocity terms back to model reference frame
      jk_nlev(:,:) = nbld(A2D(nn_hls-1))
      CALL zdf_osm_velocity_rotation( ghamu, ghamv, .FALSE.,  2, jk_nlev )
      !
      ! KPP-style Ri# mixing
      IF ( ln_kpprimix ) THEN
         jkflt = jpk
         DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
            IF ( nbld(ji,jj) < jkflt ) jkflt = nbld(ji,jj)
         END_2D
         DO jk = jkflt+1, jpkm1
            ! Shear production at uw- and vw-points (energy conserving form)
            DO_2D( nn_hls, nn_hls-1, nn_hls, nn_hls-1 )
               z2du(ji,jj) = 0.5_wp * ( uu(ji,jj,jk-1,Kmm) - uu(ji,jj,jk,Kmm) ) * ( uu(ji,jj,jk-1,Kbb) - uu(ji,jj,jk,Kbb) ) *   &
                  &          wumask(ji,jj,jk) / ( e3uw(ji,jj,jk,Kmm) * e3uw(ji,jj,jk,Kbb) )
               z2dv(ji,jj) = 0.5_wp * ( vv(ji,jj,jk-1,Kmm) - vv(ji,jj,jk,Kmm) ) * ( vv(ji,jj,jk-1,Kbb) - vv(ji,jj,jk,Kbb) ) *   &
                  &          wvmask(ji,jj,jk) / ( e3vw(ji,jj,jk,Kmm) * e3vw(ji,jj,jk,Kbb) )
            END_2D
            DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
               IF ( jk > nbld(ji,jj) ) THEN
                  ! Shear prod. at w-point weightened by mask
                  zesh2 = ( z2du(ji-1,jj) + z2du(ji,jj) ) / MAX( 1.0_wp , umask(ji-1,jj,jk) + umask(ji,jj,jk) ) +   &
                     &    ( z2dv(ji,jj-1) + z2dv(ji,jj) ) / MAX( 1.0_wp , vmask(ji,jj-1,jk) + vmask(ji,jj,jk) )
                  ! Local Richardson number
                  zri     = MAX( rn2b(ji,jj,jk), 0.0_wp ) / MAX( zesh2, epsln )
                  zfri    = MIN( zri / rn_riinfty, 1.0_wp )
                  zfri    = ( 1.0_wp - zfri * zfri )
                  zrimix  =  zfri * zfri  * zfri * wmask(ji, jj, jk)
                  zdiffut(ji,jj,jk) = MAX( zdiffut(ji,jj,jk), zrimix*rn_difri )
                  zviscos(ji,jj,jk) = MAX( zviscos(ji,jj,jk), zrimix*rn_difri )
               END IF
            END_2D
         END DO
      END IF   ! ln_kpprimix = .true.
      !
      ! KPP-style set diffusivity large if unstable below BL
      IF ( ln_convmix) THEN
         DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
            DO jk = nbld(ji,jj) + 1, jpkm1
               IF ( MIN( rn2(ji,jj,jk), rn2b(ji,jj,jk) ) <= -1e-12_wp ) zdiffut(ji,jj,jk) = MAX( rn_difconv, zdiffut(ji,jj,jk) )
            END DO
         END_2D
      END IF   ! ln_convmix = .true.
      !
      IF ( ln_osm_mle ) THEN   ! Set up diffusivity and non-gradient mixing
         DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
            IF ( l_flux(ji,jj) ) THEN   ! MLE mixing extends below boundary layer
               ! Calculate MLE flux contribution from surface fluxes
               DO jk = 1, nbld(ji,jj)
                  znd = gdepw(ji,jj,jk,Kmm) / MAX( zhbl(ji,jj), epsln )
                  ghamt(ji,jj,jk) = ghamt(ji,jj,jk) - ( swth0(ji,jj) - zrad0(ji,jj) + zradh(ji,jj) ) * ( 1.0_wp - znd )
                  ghams(ji,jj,jk) = ghams(ji,jj,jk) - sws0(ji,jj) * ( 1.0_wp - znd )
               END DO
               DO jk = 1, mld_prof(ji,jj)
                  znd = gdepw(ji,jj,jk,Kmm) / MAX( zhmle(ji,jj), epsln )
                  ghamt(ji,jj,jk) = ghamt(ji,jj,jk) + ( swth0(ji,jj) - zrad0(ji,jj) + zradh(ji,jj) ) * ( 1.0_wp - znd )
                  ghams(ji,jj,jk) = ghams(ji,jj,jk) + sws0(ji,jj) * ( 1.0_wp -znd )
               END DO
               ! Viscosity for MLEs
               DO jk = 1, mld_prof(ji,jj)
                  znd = -1.0_wp * gdepw(ji,jj,jk,Kmm) / MAX( zhmle(ji,jj), epsln )
                  zdiffut(ji,jj,jk) = zdiffut(ji,jj,jk) + zdiff_mle(ji,jj) * ( 1.0_wp - ( 2.0_wp * znd + 1.0_wp )**2 ) *   &
                     &                                    ( 1.0_wp + 5.0_wp / 21.0_wp * ( 2.0_wp * znd + 1.0_wp )**2 )
               END DO
            ELSE   ! Surface transports limited to OSBL
               ! Viscosity for MLEs
               DO jk = 1, mld_prof(ji,jj)
                  znd = -1.0_wp * gdepw(ji,jj,jk,Kmm) / MAX( zhmle(ji,jj), epsln )
                  zdiffut(ji,jj,jk) = zdiffut(ji,jj,jk) + zdiff_mle(ji,jj) * ( 1.0_wp - ( 2.0_wp * znd + 1.0_wp )**2 ) *   &
                     &                                    ( 1.0_wp + 5.0_wp / 21.0_wp * ( 2.0_wp * znd + 1.0_wp )**2 )
               END DO
            END IF
         END_2D
      ENDIF
      !
      ! Lateral boundary conditions on zvicos (sign unchanged), needed to caclulate viscosities on u and v grids
      ! CALL lbc_lnk( 'zdfosm', zviscos(:,:,:), 'W', 1.0_wp )
      ! GN 25/8: need to change tmask --> wmask
      DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 2, jpkm1 )
         p_avt(ji,jj,jk) = MAX( zdiffut(ji,jj,jk), avtb(jk) ) * tmask(ji,jj,jk)
         p_avm(ji,jj,jk) = MAX( zviscos(ji,jj,jk), avmb(jk) ) * tmask(ji,jj,jk)
      END_3D
      !
      IF ( ln_dia_osm ) THEN
         SELECT CASE (nn_osm_wave)
            ! Stokes drift set by assumimg onstant La#=0.3 (=0) or Pierson-Moskovitz spectrum (=1)
         CASE(0:1)
            CALL zdf_osm_iomput( "us_x", tmask(A2D(0),1) * sustke(A2D(0)) * scos_wind(A2D(0)) )   ! x surface Stokes drift
            CALL zdf_osm_iomput( "us_y", tmask(A2D(0),1) * sustke(A2D(0)) * scos_wind(A2D(0)) )   ! y surface Stokes drift
            CALL zdf_osm_iomput( "wind_wave_abs_power", 1000.0_wp * rho0 * tmask(A2D(0),1) * sustar(A2D(0))**2 * sustke(A2D(0)) )
            ! Stokes drift read in from sbcwave  (=2).
         CASE(2:3)
            CALL zdf_osm_iomput( "us_x",   ut0sd(A2D(0)) * umask(A2D(0),1) )                         ! x surface Stokes drift
            CALL zdf_osm_iomput( "us_y",   vt0sd(A2D(0)) * vmask(A2D(0),1) )                         ! y surface Stokes drift
            CALL zdf_osm_iomput( "wmp",    wmp(A2D(0)) * tmask(A2D(0),1) )                           ! Wave mean period
            CALL zdf_osm_iomput( "hsw",    hsw(A2D(0)) * tmask(A2D(0),1) )                           ! Significant wave height
            CALL zdf_osm_iomput( "wmp_NP", ( 2.0_wp * rpi * 1.026_wp / ( 0.877_wp * grav ) ) *   &   ! Wave mean period from NP
               &                           wndm(A2D(0)) * tmask(A2D(0),1) )                          !    spectrum
            CALL zdf_osm_iomput( "hsw_NP", ( 0.22_wp / grav ) * wndm(A2D(0))**2 * tmask(A2D(0),1) )  ! Significant wave height from
            !                                                                                        !    NP spectrum
            CALL zdf_osm_iomput( "wndm",   wndm(A2D(0)) * tmask(A2D(0),1) )                          ! U_10
            CALL zdf_osm_iomput( "wind_wave_abs_power", 1000.0_wp * rho0 * tmask(A2D(0),1) * sustar(A2D(0))**2 *   &
               &                                        SQRT( ut0sd(A2D(0))**2 + vt0sd(A2D(0))**2 ) )
         END SELECT
         CALL zdf_osm_iomput( "zwth0",           tmask(A2D(0),1) * swth0(A2D(0))     )      ! <Tw_0>
         CALL zdf_osm_iomput( "zws0",            tmask(A2D(0),1) * sws0(A2D(0))      )      ! <Sw_0>
         CALL zdf_osm_iomput( "zwb0",            tmask(A2D(0),1) * swb0(A2D(0))      )      ! <Sw_0>
         CALL zdf_osm_iomput( "zwbav",           tmask(A2D(0),1) * swth0(A2D(0))     )      ! Upward BL-avged turb buoyancy flux
         CALL zdf_osm_iomput( "ibld",            tmask(A2D(0),1) * nbld(A2D(0))      )      ! Boundary-layer max k
         CALL zdf_osm_iomput( "zdt_bl",          tmask(A2D(0),1) * av_dt_bl(A2D(0))  )      ! dt at ml base
         CALL zdf_osm_iomput( "zds_bl",          tmask(A2D(0),1) * av_ds_bl(A2D(0))  )      ! ds at ml base
         CALL zdf_osm_iomput( "zdb_bl",          tmask(A2D(0),1) * av_db_bl(A2D(0))  )      ! db at ml base
         CALL zdf_osm_iomput( "zdu_bl",          tmask(A2D(0),1) * av_du_bl(A2D(0))  )      ! du at ml base
         CALL zdf_osm_iomput( "zdv_bl",          tmask(A2D(0),1) * av_dv_bl(A2D(0))  )      ! dv at ml base
         CALL zdf_osm_iomput( "dh",              tmask(A2D(0),1) * dh(A2D(0))        )      ! Initial boundary-layer depth
         CALL zdf_osm_iomput( "hml",             tmask(A2D(0),1) * hml(A2D(0))       )      ! Initial boundary-layer depth
         CALL zdf_osm_iomput( "zdt_ml",          tmask(A2D(0),1) * av_dt_ml(A2D(0))  )      ! dt at ml base
         CALL zdf_osm_iomput( "zds_ml",          tmask(A2D(0),1) * av_ds_ml(A2D(0))  )      ! ds at ml base
         CALL zdf_osm_iomput( "zdb_ml",          tmask(A2D(0),1) * av_db_ml(A2D(0))  )      ! db at ml base
         CALL zdf_osm_iomput( "dstokes",         tmask(A2D(0),1) * dstokes(A2D(0))   )      ! Stokes drift penetration depth
         CALL zdf_osm_iomput( "zustke",          tmask(A2D(0),1) * sustke(A2D(0))    )      ! Stokes drift magnitude at T-points
         CALL zdf_osm_iomput( "zwstrc",          tmask(A2D(0),1) * swstrc(A2D(0))    )      ! Convective velocity scale
         CALL zdf_osm_iomput( "zwstrl",          tmask(A2D(0),1) * swstrl(A2D(0))    )      ! Langmuir velocity scale
         CALL zdf_osm_iomput( "zustar",          tmask(A2D(0),1) * sustar(A2D(0))    )      ! Friction velocity scale
         CALL zdf_osm_iomput( "zvstr",           tmask(A2D(0),1) * svstr(A2D(0))     )      ! Mixed velocity scale
         CALL zdf_osm_iomput( "zla",             tmask(A2D(0),1) * sla(A2D(0))       )      ! Langmuir #
         CALL zdf_osm_iomput( "wind_power",      1000.0_wp * rho0 * tmask(A2D(0),1) *   &   ! BL depth internal to zdf_osm routine
            &                                    sustar(A2D(0))**3 )
         CALL zdf_osm_iomput( "wind_wave_power", 1000.0_wp * rho0 * tmask(A2D(0),1) *   &
            &                                    sustar(A2D(0))**2 * sustke(A2D(0))  )
         CALL zdf_osm_iomput( "zhbl",            tmask(A2D(0),1) * zhbl(A2D(0))      )      ! BL depth internal to zdf_osm routine
         CALL zdf_osm_iomput( "zhml",            tmask(A2D(0),1) * zhml(A2D(0))      )      ! ML depth internal to zdf_osm routine
         CALL zdf_osm_iomput( "imld",            tmask(A2D(0),1) * nmld(A2D(0))      )      ! Index for ML depth internal to zdf_osm
         !                                                                                  !    routine
         CALL zdf_osm_iomput( "jp_ext",          tmask(A2D(0),1) * jk_ext(A2D(0))    )      ! =1 if pycnocline resolved internal to
         !                                                                                  !    zdf_osm routine
         CALL zdf_osm_iomput( "j_ddh",           tmask(A2D(0),1) * n_ddh(A2D(0))     )      ! Index forpyc thicknessh internal to
         !                                                                                  !    zdf_osm routine
         CALL zdf_osm_iomput( "zshear",          tmask(A2D(0),1) * zshear(A2D(0))    )      ! Shear production of TKE internal to
         !                                                                                  !    zdf_osm routine
         CALL zdf_osm_iomput( "zdh",             tmask(A2D(0),1) * zdh(A2D(0))       )      ! Pyc thicknessh internal to zdf_osm
         !                                                                                  !    routine
         CALL zdf_osm_iomput( "zhol",            tmask(A2D(0),1) * shol(A2D(0))      )      ! ML depth internal to zdf_osm routine
         CALL zdf_osm_iomput( "zwb_ent",         tmask(A2D(0),1) * zwb_ent(A2D(0))   )      ! Upward turb buoyancy entrainment flux
         CALL zdf_osm_iomput( "zt_ml",           tmask(A2D(0),1) * av_t_ml(A2D(0))   )      ! Average T in ML
         CALL zdf_osm_iomput( "zmld",            tmask(A2D(0),1) * zmld(A2D(0))      )      ! FK target layer depth
         CALL zdf_osm_iomput( "zwb_fk",          tmask(A2D(0),1) * zwb_fk(A2D(0))    )      ! FK b flux
         CALL zdf_osm_iomput( "zwb_fk_b",        tmask(A2D(0),1) * zwb_fk_b(A2D(0))  )      ! FK b flux averaged over ML
         CALL zdf_osm_iomput( "mld_prof",        tmask(A2D(0),1) * mld_prof(A2D(0))  )      ! FK layer max k
         CALL zdf_osm_iomput( "zdtdx",           umask(A2D(0),1) * zdtdx(A2D(0))     )      ! FK dtdx at u-pt
         CALL zdf_osm_iomput( "zdtdy",           vmask(A2D(0),1) * zdtdy(A2D(0))     )      ! FK dtdy at v-pt
         CALL zdf_osm_iomput( "zdsdx",           umask(A2D(0),1) * zdsdx(A2D(0))     )      ! FK dtdx at u-pt
         CALL zdf_osm_iomput( "zdsdy",           vmask(A2D(0),1) * zdsdy(A2D(0))     )      ! FK dsdy at v-pt
         CALL zdf_osm_iomput( "dbdx_mle",        umask(A2D(0),1) * dbdx_mle(A2D(0))  )      ! FK dbdx at u-pt
         CALL zdf_osm_iomput( "dbdy_mle",        vmask(A2D(0),1) * dbdy_mle(A2D(0))  )      ! FK dbdy at v-pt
         CALL zdf_osm_iomput( "zdiff_mle",       tmask(A2D(0),1) * zdiff_mle(A2D(0)) )      ! FK diff in MLE at t-pt
         CALL zdf_osm_iomput( "zvel_mle",        tmask(A2D(0),1) * zdiff_mle(A2D(0)) )      ! FK diff in MLE at t-pt
      END IF
      !
      ! Lateral boundary conditions on ghamu and ghamv, currently on W-grid (sign unchanged), needed to caclulate gham[uv] on u and
      !    v grids
      IF ( .NOT. l_istiled .OR. ntile == nijtile ) THEN   ! Finalise ghamu, ghamv, hbl, and hmle only after full domain has been
         !                                                !    processed
         IF ( nn_hls == 1 ) CALL lbc_lnk( 'zdfosm', ghamu, 'W', 1.0_wp,   &
            &                                       ghamv, 'W', 1.0_wp )
         DO jk = 2, jpkm1
            DO jj = Njs0, Nje0
               DO ji = Nis0, Nie0
                  ghamu(ji,jj,jk) = ( ghamu(ji,jj,jk) + ghamu(ji+1,jj,jk) ) /   &
                     &              MAX( 1.0_wp, tmask(ji,jj,jk) + tmask (ji+1,jj,jk) ) * umask(ji,jj,jk)
                  ghamv(ji,jj,jk) = ( ghamv(ji,jj,jk) + ghamv(ji,jj+1,jk) ) /   &
                     &              MAX( 1.0_wp, tmask(ji,jj,jk) + tmask (ji,jj+1,jk) ) * vmask(ji,jj,jk)
                  ghamt(ji,jj,jk) = ghamt(ji,jj,jk) * tmask(ji,jj,jk)
                  ghams(ji,jj,jk) = ghams(ji,jj,jk) * tmask(ji,jj,jk)
               END DO
            END DO
         END DO
         ! Lateral boundary conditions on final outputs for hbl, on T-grid (sign unchanged)
         CALL lbc_lnk( 'zdfosm', hbl,  'T', 1.0_wp,   &
            &                    hmle, 'T', 1.0_wp )
         !
         CALL zdf_osm_iomput( "ghamt", tmask * ghamt       )   ! <Tw_NL>
         CALL zdf_osm_iomput( "ghams", tmask * ghams       )   ! <Sw_NL>
         CALL zdf_osm_iomput( "ghamu", umask * ghamu       )   ! <uw_NL>
         CALL zdf_osm_iomput( "ghamv", vmask * ghamv       )   ! <vw_NL>
         CALL zdf_osm_iomput( "hbl",   tmask(:,:,1) * hbl  )   ! Boundary-layer depth
         CALL zdf_osm_iomput( "hmle",  tmask(:,:,1) * hmle )   ! FK layer depth
      END IF
      !
   END SUBROUTINE zdf_osm

   SUBROUTINE zdf_osm_vertical_average( Kbb, Kmm, knlev, pt, ps,   &
      &                                 pb, pu, pv, kp_ext, pdt,   &
      &                                 pds, pdb, pdu, pdv )
      !!---------------------------------------------------------------------
      !!                ***  ROUTINE zdf_vertical_average  ***
      !!
      !! ** Purpose : Determines vertical averages from surface to knlev,
      !!              and optionally the differences between these vertical
      !!              averages and values at an external level
      !!
      !! ** Method  : Averages are calculated from the surface to knlev.
      !!              The external level used to calculate differences is
      !!              knlev+kp_ext
      !!----------------------------------------------------------------------
      INTEGER,                            INTENT(in   )           ::   Kbb, Kmm   ! Ocean time-level indices
      INTEGER,  DIMENSION(A2D(nn_hls-1)), INTENT(in   )           ::   knlev      ! Number of levels to average over.
      REAL(wp), DIMENSION(jpi,jpj),       INTENT(  out)           ::   pt, ps     ! Average temperature and salinity
      REAL(wp), DIMENSION(jpi,jpj),       INTENT(  out)           ::   pb         ! Average buoyancy
      REAL(wp), DIMENSION(jpi,jpj),       INTENT(  out)           ::   pu, pv     ! Average current components
      INTEGER,  DIMENSION(A2D(nn_hls-1)), INTENT(in   ), OPTIONAL ::   kp_ext     ! External-level offsets
      REAL(wp), DIMENSION(jpi,jpj),       INTENT(  out), OPTIONAL ::   pdt        ! Difference between average temperature,
      REAL(wp), DIMENSION(jpi,jpj),       INTENT(  out), OPTIONAL ::   pds        !    salinity,
      REAL(wp), DIMENSION(jpi,jpj),       INTENT(  out), OPTIONAL ::   pdb        !    buoyancy, and
      REAL(wp), DIMENSION(jpi,jpj),       INTENT(  out), OPTIONAL ::   pdu, pdv   !    velocity components and the OSBL
      !!
      INTEGER                              ::   jk, jkflt, jkmax, ji, jj   ! Loop indices
      INTEGER                              ::   ibld_ext                   ! External-layer index
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zthick                     ! Layer thickness
      REAL(wp)                             ::   zthermal                   ! Thermal expansion coefficient
      REAL(wp)                             ::   zbeta                      ! Haline contraction coefficient
      !!----------------------------------------------------------------------
      !
      ! Averages over depth of boundary layer
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         pt(ji,jj) = 0.0_wp
         ps(ji,jj) = 0.0_wp
         pu(ji,jj) = 0.0_wp
         pv(ji,jj) = 0.0_wp
      END_2D
      zthick(:,:) = epsln
      jkflt = jpk
      jkmax = 0
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         IF ( knlev(ji,jj) < jkflt ) jkflt = knlev(ji,jj)
         IF ( knlev(ji,jj) > jkmax ) jkmax = knlev(ji,jj)
      END_2D
      DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 2, jkflt )   ! Upper, flat part of layer
         zthick(ji,jj) = zthick(ji,jj) + e3t(ji,jj,jk,Kmm)
         pt(ji,jj)     = pt(ji,jj)     + e3t(ji,jj,jk,Kmm) * ts(ji,jj,jk,jp_tem,Kmm)
         ps(ji,jj)     = ps(ji,jj)     + e3t(ji,jj,jk,Kmm) * ts(ji,jj,jk,jp_sal,Kmm)
         pu(ji,jj)     = pu(ji,jj)     + e3t(ji,jj,jk,Kmm) *                                        &
            &                               ( uu(ji,jj,jk,Kbb) + uu(ji - 1,jj,jk,Kbb) ) /           &
            &                               MAX( 1.0_wp , umask(ji,jj,jk) + umask(ji - 1,jj,jk) )
         pv(ji,jj)     = pv(ji,jj)     + e3t(ji,jj,jk,Kmm) *                                        &
            &                               ( vv(ji,jj,jk,Kbb) + vv(ji,jj - 1,jk,Kbb) ) /           &
            &                               MAX( 1.0_wp , vmask(ji,jj,jk) + vmask(ji,jj - 1,jk) )         
      END_3D
      DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, jkflt+1, jkmax )   ! Lower, non-flat part of layer
         IF ( knlev(ji,jj) >= jk ) THEN
            zthick(ji,jj) = zthick(ji,jj) + e3t(ji,jj,jk,Kmm)
            pt(ji,jj)     = pt(ji,jj)     + e3t(ji,jj,jk,Kmm) * ts(ji,jj,jk,jp_tem,Kmm)
            ps(ji,jj)     = ps(ji,jj)     + e3t(ji,jj,jk,Kmm) * ts(ji,jj,jk,jp_sal,Kmm)
            pu(ji,jj)     = pu(ji,jj)     + e3t(ji,jj,jk,Kmm) *                                        &
               &                               ( uu(ji,jj,jk,Kbb) + uu(ji - 1,jj,jk,Kbb) ) /           &
               &                               MAX( 1.0_wp , umask(ji,jj,jk) + umask(ji - 1,jj,jk) )
            pv(ji,jj)     = pv(ji,jj)     + e3t(ji,jj,jk,Kmm) *                                        &
               &                               ( vv(ji,jj,jk,Kbb) + vv(ji,jj - 1,jk,Kbb) ) /           &
               &                               MAX( 1.0_wp , vmask(ji,jj,jk) + vmask(ji,jj - 1,jk) )
         END IF
      END_3D
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         pt(ji,jj) = pt(ji,jj) / zthick(ji,jj)
         ps(ji,jj) = ps(ji,jj) / zthick(ji,jj)
         pu(ji,jj) = pu(ji,jj) / zthick(ji,jj)
         pv(ji,jj) = pv(ji,jj) / zthick(ji,jj)
         zthermal  = rab_n(ji,jj,1,jp_tem)   ! ideally use nbld not 1??
         zbeta     = rab_n(ji,jj,1,jp_sal)
         pb(ji,jj) = grav * zthermal * pt(ji,jj) - grav * zbeta * ps(ji,jj)
      END_2D
      !
      ! Differences between vertical averages and values at an external layer
      IF ( PRESENT( kp_ext ) ) THEN
         DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
            ibld_ext = knlev(ji,jj) + kp_ext(ji,jj)
            IF ( ibld_ext <= mbkt(ji,jj)-1 ) THEN   ! ag 09/03
               ! Two external levels are available
               pdt(ji,jj) = pt(ji,jj) - ts(ji,jj,ibld_ext,jp_tem,Kmm)
               pds(ji,jj) = ps(ji,jj) - ts(ji,jj,ibld_ext,jp_sal,Kmm)
               pdu(ji,jj) = pu(ji,jj) - ( uu(ji,jj,ibld_ext,Kbb) + uu(ji-1,jj,ibld_ext,Kbb ) ) /              &
                  &                        MAX(1.0_wp , umask(ji,jj,ibld_ext ) + umask(ji-1,jj,ibld_ext ) )
               pdv(ji,jj) = pv(ji,jj) - ( vv(ji,jj,ibld_ext,Kbb) + vv(ji,jj-1,ibld_ext,Kbb ) ) /              &
                  &                        MAX(1.0_wp , vmask(ji,jj,ibld_ext ) + vmask(ji,jj-1,ibld_ext ) )
               zthermal   = rab_n(ji,jj,1,jp_tem)   ! ideally use nbld not 1??
               zbeta      = rab_n(ji,jj,1,jp_sal)
               pdb(ji,jj) = grav * zthermal * pdt(ji,jj) - grav * zbeta * pds(ji,jj)
            ELSE
               pdt(ji,jj) = 0.0_wp
               pds(ji,jj) = 0.0_wp
               pdu(ji,jj) = 0.0_wp
               pdv(ji,jj) = 0.0_wp
               pdb(ji,jj) = 0.0_wp
            ENDIF
         END_2D
      END IF
      !
   END SUBROUTINE zdf_osm_vertical_average

   SUBROUTINE zdf_osm_velocity_rotation_2d( pu, pv, fwd )
      !!---------------------------------------------------------------------
      !!            ***  ROUTINE zdf_velocity_rotation_2d  ***
      !!
      !! ** Purpose : Rotates frame of reference of velocity components pu and
      !!              pv (2d)
      !!
      !! ** Method : The velocity components are rotated into (fwd=.TRUE.) or
      !!             from (fwd=.FALSE.) the frame specified by scos_wind and
      !!             ssin_wind
      !!
      !!----------------------------------------------------------------------      
      REAL(wp),           INTENT(inout), DIMENSION(jpi,jpj) ::   pu, pv   ! Components of current
      LOGICAL,  OPTIONAL, INTENT(in   )                     ::   fwd      ! Forward (default) or reverse rotation
      !!
      INTEGER  ::   ji, jj       ! Loop indices
      REAL(wp) ::   ztmp, zfwd   ! Auxiliary variables
      !!----------------------------------------------------------------------      
      !
      zfwd = 1.0_wp
      IF( PRESENT(fwd) .AND. ( .NOT. fwd ) ) zfwd = -1.0_wp
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         ztmp      = pu(ji,jj)
         pu(ji,jj) = pu(ji,jj) * scos_wind(ji,jj) + zfwd * pv(ji,jj) * ssin_wind(ji,jj)
         pv(ji,jj) = pv(ji,jj) * scos_wind(ji,jj) - zfwd * ztmp      * ssin_wind(ji,jj)
      END_2D
      !
   END SUBROUTINE zdf_osm_velocity_rotation_2d

   SUBROUTINE zdf_osm_velocity_rotation_3d( pu, pv, fwd, ktop, knlev )
      !!---------------------------------------------------------------------
      !!            ***  ROUTINE zdf_velocity_rotation_3d  ***
      !!
      !! ** Purpose : Rotates frame of reference of velocity components pu and
      !!              pv (3d)
      !!
      !! ** Method : The velocity components are rotated into (fwd=.TRUE.) or
      !!             from (fwd=.FALSE.) the frame specified by scos_wind and
      !!             ssin_wind; optionally, the rotation can be restricted at
      !!             each water column to span from the a minimum index ktop to
      !!             the depth index specified in array knlev
      !!
      !!----------------------------------------------------------------------      
      REAL(wp),           INTENT(inout), DIMENSION(jpi,jpj,jpk)   ::   pu, pv   ! Components of current
      LOGICAL,  OPTIONAL, INTENT(in   )                           ::   fwd      ! Forward (default) or reverse rotation
      INTEGER,  OPTIONAL, INTENT(in   )                           ::   ktop     ! Minimum depth index
      INTEGER,  OPTIONAL, INTENT(in   ), DIMENSION(A2D(nn_hls-1)) ::   knlev    ! Array of maximum depth indices
      !!
      INTEGER  ::   ji, jj, jk, jktop, jkmax   ! Loop indices
      REAL(wp) ::   ztmp, zfwd                 ! Auxiliary variables
      LOGICAL  ::   llkbot                     ! Auxiliary variable
      !!----------------------------------------------------------------------      
      !
      zfwd = 1.0_wp
      IF( PRESENT(fwd) .AND. ( .NOT. fwd ) ) zfwd = -1.0_wp
      jktop = 1
      IF( PRESENT(ktop) ) jktop = ktop
      IF( PRESENT(knlev) ) THEN
         jkmax = 0
         DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
            IF ( knlev(ji,jj) > jkmax ) jkmax = knlev(ji,jj)
         END_2D
         llkbot = .FALSE.
      ELSE
         jkmax = jpk
         llkbot = .TRUE.
      END IF
      DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, jktop, jkmax )
         IF ( llkbot .OR. knlev(ji,jj) >= jk ) THEN
            ztmp         = pu(ji,jj,jk)
            pu(ji,jj,jk) = pu(ji,jj,jk) * scos_wind(ji,jj) + zfwd * pv(ji,jj,jk) * ssin_wind(ji,jj)
            pv(ji,jj,jk) = pv(ji,jj,jk) * scos_wind(ji,jj) - zfwd * ztmp         * ssin_wind(ji,jj)
         END IF
      END_3D
      !
   END SUBROUTINE zdf_osm_velocity_rotation_3d

   SUBROUTINE zdf_osm_osbl_state( Kmm, pwb_ent, pwb_min, pshear, phbl,   &
      &                           phml, pdh )
      !!---------------------------------------------------------------------
      !!                 ***  ROUTINE zdf_osm_osbl_state  ***
      !!
      !! ** Purpose : Determines the state of the OSBL, stable/unstable,
      !!              shear/ noshear. Also determines shear production,
      !!              entrainment buoyancy flux and interfacial Richardson
      !!              number
      !!
      !! ** Method  :
      !!
      !!----------------------------------------------------------------------
      INTEGER,                            INTENT(in   ) ::   Kmm       ! Ocean time-level index
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(  out) ::   pwb_ent   ! Buoyancy fluxes at base
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(  out) ::   pwb_min   !    of well-mixed layer
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(  out) ::   pshear    ! Production of TKE due to shear across the pycnocline
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   phbl      ! BL depth
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   phml      ! ML depth
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   pdh       ! Pycnocline depth
      !!
      INTEGER :: jj, ji   ! Loop indices
      !!
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zekman
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zri_p, zri_b   ! Richardson numbers
      REAL(wp)                           ::   zshear_u, zshear_v, zwb_shr
      REAL(wp)                           ::   zwcor, zrf_conv, zrf_shear, zrf_langmuir, zr_stokes
      !!
      REAL(wp), PARAMETER ::   pp_a_shr         = 0.4_wp,  pp_b_shr    = 6.5_wp,  pp_a_wb_s = 0.8_wp
      REAL(wp), PARAMETER ::   pp_alpha_c       = 0.2_wp,  pp_alpha_lc = 0.03_wp
      REAL(wp), PARAMETER ::   pp_alpha_ls      = 0.06_wp, pp_alpha_s  = 0.15_wp
      REAL(wp), PARAMETER ::   pp_ri_p_thresh   = 27.0_wp
      REAL(wp), PARAMETER ::   pp_ri_c          = 0.25_wp
      REAL(wp), PARAMETER ::   pp_ek            = 4.0_wp
      REAL(wp), PARAMETER ::   pp_large         = -1e10_wp
      !!----------------------------------------------------------------------
      !
      ! Initialise arrays
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         l_conv(ji,jj)  = .FALSE.
         l_shear(ji,jj) = .FALSE.
         n_ddh(ji,jj)   = 1
      END_2D
      ! Initialise INTENT(  out) arrays
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         pwb_ent(ji,jj) = pp_large
         pwb_min(ji,jj) = pp_large
      END_2D
      !
      ! Determins stability and set flag l_conv
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         IF ( shol(ji,jj) < 0.0_wp ) THEN
            l_conv(ji,jj) = .TRUE.
         ELSE
            l_conv(ji,jj) = .FALSE.
         ENDIF
      END_2D
      !
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         pshear(ji,jj) = 0.0_wp
      END_2D
      zekman(:,:) = EXP( -1.0_wp * pp_ek * ABS( ff_t(A2D(nn_hls-1)) ) * phbl(A2D(nn_hls-1)) /   &
         &               MAX( sustar(A2D(nn_hls-1)), 1.e-8 ) )
      !
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         IF ( l_conv(ji,jj) ) THEN
            IF ( av_db_bl(ji,jj) > 0.0_wp ) THEN
               zri_p(ji,jj) = MAX (  SQRT( av_db_bl(ji,jj) * pdh(ji,jj) / MAX( av_du_bl(ji,jj)**2 + av_dv_bl(ji,jj)**2,     &
                  &                                                          1e-8_wp ) ) * ( phbl(ji,jj) / pdh(ji,jj) ) *   &
                  &                  ( svstr(ji,jj) / MAX( sustar(ji,jj), 1e-6_wp ) )**2 /                                  &
                  &                  MAX( zekman(ji,jj), 1.0e-6_wp ), 5.0_wp )
               IF ( ff_t(ji,jj) >= 0.0_wp ) THEN   ! Northern hemisphere
                  zri_b(ji,jj) = av_db_ml(ji,jj) * pdh(ji,jj) / ( MAX( av_du_ml(ji,jj), 1e-5_wp )**2 +   &
                     &                                          MAX( -1.0_wp * av_dv_ml(ji,jj), 1e-5_wp)**2 )
               ELSE                                ! Southern hemisphere
                  zri_b(ji,jj) = av_db_ml(ji,jj) * pdh(ji,jj) / ( MAX( av_du_ml(ji,jj), 1e-5_wp )**2 +   &
                     &                                          MAX(           av_dv_ml(ji,jj), 1e-5_wp)**2 )
               END IF
               pshear(ji,jj) = pp_a_shr * zekman(ji,jj) *                                                   &
                  &            ( MAX( sustar(ji,jj)**2 * av_du_ml(ji,jj) / phbl(ji,jj), 0.0_wp ) +          &
                  &              pp_b_shr * MAX( -1.0_wp * ff_t(ji,jj) * sustke(ji,jj) * dstokes(ji,jj) *   &
                  &                            av_dv_ml(ji,jj) / phbl(ji,jj), 0.0_wp ) )
               ! Stability dependence
               pshear(ji,jj) = pshear(ji,jj) * EXP( -0.75_wp * MAX( 0.0_wp, ( zri_b(ji,jj) - pp_ri_c ) / pp_ri_c ) )
               !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
               ! Test ensures n_ddh=0 is not selected. Change to zri_p<27 when  !
               ! full code available                                          !
               !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
               IF ( pshear(ji,jj) > 1e-10 ) THEN
                  IF ( zri_p(ji,jj) < pp_ri_p_thresh .AND.   &
                     & MIN( hu(ji,jj,Kmm), hu(ji-1,jj,Kmm), hv(ji,jj,Kmm), hv(ji,jj-1,Kmm) ) > 100.0_wp ) THEN
                     ! Growing shear layer
                     n_ddh(ji,jj) = 0
                     l_shear(ji,jj) = .TRUE.
                  ELSE
                     n_ddh(ji,jj) = 1
                     !             IF ( zri_b <= 1.5 .and. pshear(ji,jj) > 0._wp ) THEN
                     ! Shear production large enough to determine layer charcteristics, but can't maintain a shear layer
                     l_shear(ji,jj) = .TRUE.
                     !             ELSE
                  END IF
               ELSE
                  n_ddh(ji,jj) = 2
                  l_shear(ji,jj) = .FALSE.
               END IF
               ! Shear production may not be zero, but is small and doesn't determine characteristics of pycnocline
               !               pshear(ji,jj) = 0.5 * pshear(ji,jj)
               !               l_shear(ji,jj) = .FALSE.
               !            ENDIF
            ELSE   ! av_db_bl test, note pshear set to zero
               n_ddh(ji,jj) = 2
               l_shear(ji,jj) = .FALSE.
            ENDIF
         ENDIF
      END_2D
      !
      ! Calculate entrainment buoyancy flux due to surface fluxes.
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         IF ( l_conv(ji,jj) ) THEN
            zwcor        = ABS( ff_t(ji,jj) ) * phbl(ji,jj) + epsln
            zrf_conv     = TANH( ( swstrc(ji,jj) / zwcor )**0.69_wp )
            zrf_shear    = TANH( ( sustar(ji,jj) / zwcor )**0.69_wp )
            zrf_langmuir = TANH( ( swstrl(ji,jj) / zwcor )**0.69_wp )
            IF ( nn_osm_SD_reduce > 0 ) THEN
               ! Effective Stokes drift already reduced from surface value
               zr_stokes = 1.0_wp
            ELSE
               ! Effective Stokes drift only reduced by factor rn_zdfodm_adjust_sd,
               ! requires further reduction where BL is deep
               zr_stokes = 1.0 - EXP( -25.0_wp * dstokes(ji,jj) / hbl(ji,jj) * ( 1.0_wp + 4.0_wp * dstokes(ji,jj) / hbl(ji,jj) ) )
            END IF
            pwb_ent(ji,jj) = -2.0_wp * pp_alpha_c * zrf_conv * swbav(ji,jj) -                                          &
               &             pp_alpha_s * zrf_shear * sustar(ji,jj)**3 / phml(ji,jj) +                                 &
               &             zr_stokes * ( pp_alpha_s * EXP( -1.5_wp * sla(ji,jj) ) * zrf_shear * sustar(ji,jj)**3 -   &
               &                           zrf_langmuir * pp_alpha_lc * swstrl(ji,jj)**3 ) / phml(ji,jj)
         ENDIF
      END_2D
      !
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         IF ( l_shear(ji,jj) ) THEN
            IF ( l_conv(ji,jj) ) THEN
               ! Unstable OSBL
               zwb_shr = -1.0_wp * pp_a_wb_s * zri_b(ji,jj) * pshear(ji,jj)
               IF ( n_ddh(ji,jj) == 0 ) THEN
                  ! Developing shear layer, additional shear production possible.

                  !    pshear_u = MAX( zustar(ji,jj)**2 * MAX( av_du_ml(ji,jj), 0._wp ) /  phbl(ji,jj), 0._wp )
                  !    pshear(ji,jj) = pshear(ji,jj) + pshear_u * ( 1.0 - MIN( zri_p(ji,jj) / pp_ri_p_thresh, 1.d0 )**2 )
                  !    pshear(ji,jj) = MIN( pshear(ji,jj), pshear_u )

                  !    zwb_shr = zwb_shr - 0.25 * MAX ( pshear_u, 0._wp) * ( 1.0 - MIN( zri_p(ji,jj) / pp_ri_p_thresh, 1._wp )**2 )
                  !    zwb_shr = MAX( zwb_shr, -0.25 * pshear_u )
               ENDIF
               pwb_ent(ji,jj) = pwb_ent(ji,jj) + zwb_shr
               !           pwb_min(ji,jj) = pwb_ent(ji,jj) + pdh(ji,jj) / phbl(ji,jj) * zwb0(ji,jj)
            ELSE   ! IF ( l_conv ) THEN - ENDIF
               ! Stable OSBL  - shear production not coded for first attempt.
            ENDIF   ! l_conv
         END IF   ! l_shear
         IF ( l_conv(ji,jj) ) THEN
            ! Unstable OSBL
            pwb_min(ji,jj) = pwb_ent(ji,jj) + pdh(ji,jj) / phbl(ji,jj) * 2.0_wp * swbav(ji,jj)
         END IF  ! l_conv
      END_2D
      !
   END SUBROUTINE zdf_osm_osbl_state

   SUBROUTINE zdf_osm_external_gradients( Kmm, kbase, pdtdz, pdsdz, pdbdz )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE zdf_osm_external_gradients  ***
      !!
      !! ** Purpose : Calculates the gradients below the OSBL
      !!
      !! ** Method  : Uses nbld and ibld_ext to determine levels to calculate the gradient.
      !!
      !!----------------------------------------------------------------------   
      INTEGER,                            INTENT(in   ) ::   Kmm            ! Ocean time-level index
      INTEGER,  DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   kbase          ! OSBL base layer index
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(  out) ::   pdtdz, pdsdz   ! External gradients of temperature, salinity
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(  out) ::   pdbdz          !    and buoyancy
      !!
      INTEGER  ::   ji, jj, jkb, jkb1
      REAL(wp) ::   zthermal, zbeta
      !!
      REAL(wp), PARAMETER ::   pp_large = -1e10_wp
      !!----------------------------------------------------------------------   
      !
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         pdtdz(ji,jj) = pp_large
         pdsdz(ji,jj) = pp_large
         pdbdz(ji,jj) = pp_large
      END_2D
      !
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         IF ( kbase(ji,jj)+1 < mbkt(ji,jj) ) THEN
            zthermal = rab_n(ji,jj,1,jp_tem)   ! Ideally use nbld not 1??
            zbeta    = rab_n(ji,jj,1,jp_sal)
            jkb = kbase(ji,jj)
            jkb1 = MIN( jkb + 1, mbkt(ji,jj) )
            pdtdz(ji,jj) = -1.0_wp * ( ts(ji,jj,jkb1,jp_tem,Kmm) - ts(ji,jj,jkb,jp_tem,Kmm ) ) / e3w(ji,jj,jkb1,Kmm)
            pdsdz(ji,jj) = -1.0_wp * ( ts(ji,jj,jkb1,jp_sal,Kmm) - ts(ji,jj,jkb,jp_sal,Kmm ) ) / e3w(ji,jj,jkb1,Kmm)
            pdbdz(ji,jj) = grav * zthermal * pdtdz(ji,jj) - grav * zbeta * pdsdz(ji,jj)
         ELSE
            pdtdz(ji,jj) = 0.0_wp
            pdsdz(ji,jj) = 0.0_wp
            pdbdz(ji,jj) = 0.0_wp
         END IF
      END_2D
      !
   END SUBROUTINE zdf_osm_external_gradients

   SUBROUTINE zdf_osm_calculate_dhdt( pdhdt, phbl, pdh, pwb_ent, pwb_min,   &
      &                               pdbdz_bl_ext, pwb_fk_b, pwb_fk, pvel_mle )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE zdf_osm_calculate_dhdt  ***
      !!
      !! ** Purpose : Calculates the rate at which hbl changes.
      !!
      !! ** Method  :
      !!
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(  out) ::   pdhdt          ! Rate of change of hbl
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   phbl           ! BL depth
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   pdh            ! Pycnocline depth
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   pwb_ent        ! Buoyancy entrainment flux
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   pwb_min
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   pdbdz_bl_ext   ! External buoyancy gradients
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(  out) ::   pwb_fk_b       ! MLE buoyancy flux averaged over OSBL
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   pwb_fk         ! Max MLE buoyancy flux
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   pvel_mle       ! Vvelocity scale for dhdt with stable ML and FK
      !!
      INTEGER  ::   jj, ji
      REAL(wp) ::   zgamma_b_nd, zgamma_dh_nd, zpert, zpsi, zari
      REAL(wp) ::   zvel_max, zddhdt
      !!
      REAL(wp), PARAMETER ::   pp_alpha_b = 0.3_wp
      REAL(wp), PARAMETER ::   pp_ddh     = 2.5_wp, pp_ddh_2 = 3.5_wp   ! Also in pycnocline_depth
      REAL(wp), PARAMETER ::   pp_large   = -1e10_wp
      !!----------------------------------------------------------------------
      !
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         pdhdt(ji,jj)    = pp_large
         pwb_fk_b(ji,jj) = pp_large
      END_2D
      !
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         !
         IF ( l_shear(ji,jj) ) THEN
            !
            IF ( l_conv(ji,jj) ) THEN   ! Convective
               !
               IF ( ln_osm_mle ) THEN
                  IF ( hmle(ji,jj) > hbl(ji,jj) ) THEN   ! Fox-Kemper buoyancy flux average over OSBL
                     pwb_fk_b(ji,jj) = pwb_fk(ji,jj) * ( 1.0_wp + hmle(ji,jj) / ( 6.0_wp * hbl(ji,jj) ) *   &
                        &                                         ( -1.0_wp + ( 1.0_wp - 2.0_wp * hbl(ji,jj) / hmle(ji,jj) )**3 ) )
                  ELSE
                     pwb_fk_b(ji,jj) = 0.5_wp * pwb_fk(ji,jj) * hmle(ji,jj) / hbl(ji,jj)
                  ENDIF
                  zvel_max = ( svstr(ji,jj)**3 + 0.5_wp * swstrc(ji,jj)**3 )**p2third / hbl(ji,jj)
                  IF ( ( pwb_ent(ji,jj) + 2.0_wp * pwb_fk_b(ji,jj) ) < 0.0_wp ) THEN   ! OSBL is deepening,
                     !                                                                 !    entrainment > restratification
                     IF ( av_db_bl(ji,jj) > 1e-15_wp ) THEN
                        zgamma_b_nd = MAX( pdbdz_bl_ext(ji,jj), 0.0_wp ) * pdh(ji,jj) /   &
                           &          ( zvel_max + MAX( av_db_bl(ji,jj), 1e-15_wp ) )
                        zpsi = ( 1.0_wp - 0.5_wp * pdh(ji,jj) / phbl(ji,jj) ) *                                                &
                           &   ( swb0(ji,jj) - MIN( ( pwb_min(ji,jj) + 2.0_wp * pwb_fk_b(ji,jj) ), 0.0_wp ) ) * pdh(ji,jj) /   &
                           &   phbl(ji,jj)
                        zpsi = zpsi + 1.75_wp * ( 1.0_wp - 0.5_wp * pdh(ji,jj) / phbl(ji,jj) ) *   &
                           &          ( pdh(ji,jj) / phbl(ji,jj) + zgamma_b_nd ) *   &
                           &          MIN( ( pwb_min(ji,jj) + 2.0_wp * pwb_fk_b(ji,jj) ), 0.0_wp )
                        zpsi = pp_alpha_b * MAX( zpsi, 0.0_wp )
                        pdhdt(ji,jj) = -1.0_wp * ( pwb_ent(ji,jj) + 2.0_wp * pwb_fk_b(ji,jj) ) /      &
                           &                      ( zvel_max + MAX( av_db_bl(ji,jj), 1e-15_wp ) ) +   &
                           &            zpsi / ( zvel_max + MAX( av_db_bl(ji,jj), 1e-15_wp ) )
                        IF ( n_ddh(ji,jj) == 1 ) THEN
                           IF ( ( swstrc(ji,jj) / svstr(ji,jj) )**3 <= 0.5_wp ) THEN
                              zari = MIN( 1.5_wp * av_db_bl(ji,jj) /                                                   &
                                 &        ( phbl(ji,jj) * ( MAX( pdbdz_bl_ext(ji,jj), 0.0_wp ) +                       &
                                 &                               av_db_bl(ji,jj)**2 / MAX( 4.5_wp * svstr(ji,jj)**2,   &
                                 &                                                       1e-12_wp ) ) ), 0.2_wp )
                           ELSE
                              zari = MIN( 1.5_wp * av_db_bl(ji,jj) /                                                    &
                                 &        ( phbl(ji,jj) * ( MAX( pdbdz_bl_ext(ji,jj), 0.0_wp ) +                        &
                                 &                               av_db_bl(ji,jj)**2 / MAX( 4.5_wp * swstrc(ji,jj)**2,   &
                                 &                                                       1e-12_wp ) ) ), 0.2_wp )
                           ENDIF
                           ! Relaxation to dh_ref = zari * hbl
                           zddhdt = -1.0_wp * pp_ddh_2 * ( 1.0_wp - pdh(ji,jj) / ( zari * phbl(ji,jj) ) ) * pwb_ent(ji,jj) /   &
                              &     ( zvel_max + MAX( av_db_bl(ji,jj), 1e-15_wp ) )
                        ELSE IF ( n_ddh(ji,jj) == 0 ) THEN   ! Growing shear layer
                           zddhdt = -1.0_wp * pp_ddh * ( 1.0_wp - 1.6_wp * pdh(ji,jj) / phbl(ji,jj) ) * pwb_ent(ji,jj) /   &
                              &     ( zvel_max + MAX( av_db_bl(ji,jj), 1e-15_wp ) )
                           zddhdt = EXP( -4.0_wp * ABS( ff_t(ji,jj) ) * phbl(ji,jj) / MAX( sustar(ji,jj), 1e-8_wp ) ) * zddhdt
                        ELSE
                           zddhdt = 0.0_wp
                        ENDIF   ! n_ddh
                        pdhdt(ji,jj) = pdhdt(ji,jj) + pp_alpha_b * ( 1.0_wp - 0.5_wp * pdh(ji,jj) / phbl(ji,jj) ) *   &
                           &                            av_db_ml(ji,jj) * MAX( zddhdt, 0.0_wp ) /   &
                           &                            ( zvel_max + MAX( av_db_bl(ji,jj), 1e-15_wp ) )
                     ELSE   ! av_db_bl >0
                        pdhdt(ji,jj) = -1.0_wp * ( pwb_ent(ji,jj) + 2.0_wp * pwb_fk_b(ji,jj) ) /  MAX( zvel_max, 1e-15_wp )
                     ENDIF
                  ELSE   ! pwb_min + 2*pwb_fk_b < 0
                     ! OSBL shoaling due to restratification flux. This is the velocity defined in Fox-Kemper et al (2008)
                     pdhdt(ji,jj) = -1.0_wp * MIN( pvel_mle(ji,jj), hbl(ji,jj) / 10800.0_wp )
                  ENDIF
               ELSE   ! Fox-Kemper not used.
                  zvel_max = -1.0_wp * ( 1.0_wp + 1.0_wp * ( svstr(ji,jj)**3 + 0.5_wp * swstrc(ji,jj)**3 )**pthird *     &
                     &                                                         rn_Dt / hbl(ji,jj) ) * pwb_ent(ji,jj) /   &
                     &       MAX( ( svstr(ji,jj)**3 + 0.5_wp * swstrc(ji,jj)**3 )**pthird, epsln )
                  pdhdt(ji,jj) = -1.0_wp * pwb_ent(ji,jj) / ( zvel_max + MAX( av_db_bl(ji,jj), 1e-15_wp ) )
                  ! added ajgn 23 July as temporay fix
               ENDIF   ! ln_osm_mle
               !
            ELSE   ! l_conv - Stable
               !
               pdhdt(ji,jj) = ( 0.06_wp + 0.52_wp * shol(ji,jj) / 2.0_wp ) * svstr(ji,jj)**3 / hbl(ji,jj) + swbav(ji,jj)
               IF ( pdhdt(ji,jj) < 0.0_wp ) THEN   ! For long timsteps factor in brackets slows the rapid collapse of the OSBL
                  zpert = 2.0_wp * ( 1.0_wp + 0.0_wp * 2.0_wp * svstr(ji,jj) * rn_Dt / hbl(ji,jj) ) * svstr(ji,jj)**2 / hbl(ji,jj)
               ELSE
                  zpert = MAX( svstr(ji,jj)**2 / hbl(ji,jj), av_db_bl(ji,jj) )
               ENDIF
               pdhdt(ji,jj) = 2.0_wp * pdhdt(ji,jj) / MAX( zpert, epsln )
               pdhdt(ji,jj) = MAX( pdhdt(ji,jj), -1.0_wp * hbl(ji,jj) / 5400.0_wp )
               !
            ENDIF   ! l_conv
            !
         ELSE   ! l_shear
            !
            IF ( l_conv(ji,jj) ) THEN   ! Convective
               !
               IF ( ln_osm_mle ) THEN
                  IF ( hmle(ji,jj) > hbl(ji,jj) ) THEN   ! Fox-Kemper buoyancy flux average over OSBL
                     pwb_fk_b(ji,jj) = pwb_fk(ji,jj) *                       &
                        ( 1.0_wp + hmle(ji,jj) / ( 6.0_wp * hbl(ji,jj) ) *   &
                        &          ( -1.0_wp + ( 1.0_wp - 2.0_wp * hbl(ji,jj) / hmle(ji,jj))**3) )
                  ELSE
                     pwb_fk_b(ji,jj) = 0.5_wp * pwb_fk(ji,jj) * hmle(ji,jj) / hbl(ji,jj)
                  ENDIF
                  zvel_max = ( swstrl(ji,jj)**3 + 0.5_wp * swstrc(ji,jj)**3 )**p2third / hbl(ji,jj)
                  IF ( ( pwb_ent(ji,jj) + 2.0_wp * pwb_fk_b(ji,jj) ) < 0.0_wp ) THEN   ! OSBL is deepening,
                     !                                                                 !    entrainment > restratification
                     IF ( av_db_bl(ji,jj) > 0.0_wp .AND. pdbdz_bl_ext(ji,jj) > 0.0_wp ) THEN
                        pdhdt(ji,jj) = -1.0_wp * ( pwb_ent(ji,jj) + 2.0_wp * pwb_fk_b(ji,jj) ) /   &
                           &            ( zvel_max + MAX( av_db_bl(ji,jj), 1e-15_wp ) )
                     ELSE
                        pdhdt(ji,jj) = -1.0_wp * ( pwb_ent(ji,jj) + 2.0_wp * pwb_fk_b(ji,jj) ) / MAX( zvel_max, 1e-15_wp )
                     ENDIF
                  ELSE   ! OSBL shoaling due to restratification flux. This is the velocity defined in Fox-Kemper et al (2008)
                     pdhdt(ji,jj) = -1.0_wp * MIN( pvel_mle(ji,jj), hbl(ji,jj) / 10800.0_wp )
                  ENDIF
               ELSE   ! Fox-Kemper not used
                  zvel_max = -1.0_wp * pwb_ent(ji,jj) / MAX( ( svstr(ji,jj)**3 + 0.5_wp * swstrc(ji,jj)**3 )**pthird, epsln )
                  pdhdt(ji,jj) = -1.0_wp * pwb_ent(ji,jj) / ( zvel_max + MAX( av_db_bl(ji,jj), 1e-15_wp ) )
                  ! added ajgn 23 July as temporay fix
               ENDIF  ! ln_osm_mle
               !
            ELSE                        ! Stable
               !
               pdhdt(ji,jj) = ( 0.06_wp + 0.52_wp * shol(ji,jj) / 2.0_wp ) * svstr(ji,jj)**3 / hbl(ji,jj) + swbav(ji,jj)
               IF ( pdhdt(ji,jj) < 0.0_wp ) THEN
                  ! For long timsteps factor in brackets slows the rapid collapse of the OSBL
                  zpert = 2.0_wp * svstr(ji,jj)**2 / hbl(ji,jj)
               ELSE
                  zpert = MAX( svstr(ji,jj)**2 / hbl(ji,jj), av_db_bl(ji,jj) )
               ENDIF
               pdhdt(ji,jj) = 2.0_wp * pdhdt(ji,jj) / MAX(zpert, epsln)
               pdhdt(ji,jj) = MAX( pdhdt(ji,jj), -1.0_wp * hbl(ji,jj) / 5400.0_wp )
               !
            ENDIF  ! l_conv
            !
         ENDIF ! l_shear
         !
      END_2D
      !
   END SUBROUTINE zdf_osm_calculate_dhdt

   SUBROUTINE zdf_osm_timestep_hbl( Kmm, pdhdt, phbl, phbl_t, pwb_ent,   &
      &                             pwb_fk_b )
      !!---------------------------------------------------------------------
      !!                ***  ROUTINE zdf_osm_timestep_hbl  ***
      !!
      !! ** Purpose : Increments hbl.
      !!
      !! ** Method  : If the change in hbl exceeds one model level the change is
      !!              is calculated by moving down the grid, changing the
      !!              buoyancy jump. This is to ensure that the change in hbl
      !!              does not overshoot a stable layer.
      !!
      !!----------------------------------------------------------------------
      INTEGER,                            INTENT(in   ) ::   Kmm        ! Ocean time-level index
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(inout) ::   pdhdt      ! Rates of change of hbl
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(inout) ::   phbl       ! BL depth
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   phbl_t     ! BL depth
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   pwb_ent    ! Buoyancy entrainment flux
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   pwb_fk_b   ! MLE buoyancy flux averaged over OSBL
      !!
      INTEGER  ::   jk, jj, ji, jm
      REAL(wp) ::   zhbl_s, zvel_max, zdb
      REAL(wp) ::   zthermal, zbeta
      !!----------------------------------------------------------------------
      !
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         IF ( nbld(ji,jj) - nmld(ji,jj) > 1 ) THEN
            !
            ! If boundary layer changes by more than one level, need to check for stable layers between initial and final depths.
            !
            zhbl_s   = hbl(ji,jj)
            jm       = nmld(ji,jj)
            zthermal = rab_n(ji,jj,1,jp_tem)
            zbeta    = rab_n(ji,jj,1,jp_sal)
            !
            IF ( l_conv(ji,jj) ) THEN   ! Unstable
               !
               IF( ln_osm_mle ) THEN
                  zvel_max = ( swstrl(ji,jj)**3 + swstrc(ji,jj)**3 )**p2third / hbl(ji,jj)
               ELSE
                  zvel_max = -1.0_wp * ( 1.0_wp + 1.0_wp * ( svstr(ji,jj)**3 + 0.5_wp * swstrc(ji,jj)**3 )**pthird * rn_Dt /   &
                     &                                     hbl(ji,jj) ) * pwb_ent(ji,jj) /                                     &
                     &       ( svstr(ji,jj)**3 + 0.5_wp * swstrc(ji,jj)**3 )**pthird
               ENDIF
               DO jk = nmld(ji,jj), nbld(ji,jj)
                  zdb = MAX( grav * ( zthermal * ( av_t_bl(ji,jj) - ts(ji,jj,jm,jp_tem,Kmm) ) -   &
                     &                zbeta    * ( av_s_bl(ji,jj) - ts(ji,jj,jm,jp_sal,Kmm) ) ), 0.0_wp ) + zvel_max
                  !
                  IF ( ln_osm_mle ) THEN
                     zhbl_s = zhbl_s + MIN( rn_Dt * ( ( -1.0_wp * pwb_ent(ji,jj) - 2.0_wp * pwb_fk_b(ji,jj) ) / zdb ) /   &
                        &                   REAL( nbld(ji,jj) - nmld(ji,jj), KIND=wp ), e3w(ji,jj,jm,Kmm) )
                  ELSE
                     zhbl_s = zhbl_s + MIN( rn_Dt * ( -1.0_wp * pwb_ent(ji,jj) / zdb ) /   &
                        &                   REAL( nbld(ji,jj) - nmld(ji,jj), KIND=wp ), e3w(ji,jj,jm,Kmm) )
                  ENDIF
                  !                    zhbl_s = MIN(zhbl_s,  gdepw(ji,jj, mbkt(ji,jj) + 1,Kmm) - depth_tol)
                  IF ( zhbl_s >= gdepw(ji,jj,mbkt(ji,jj) + 1,Kmm) ) THEN
                     zhbl_s = MIN( zhbl_s, gdepw(ji,jj, mbkt(ji,jj) + 1, Kmm ) - depth_tol )
                     l_pyc(ji,jj) = .FALSE.
                  ENDIF
                  IF ( zhbl_s >= gdepw(ji,jj,jm+1,Kmm) ) jm = jm + 1
               END DO
               hbl(ji,jj)  = zhbl_s
               nbld(ji,jj) = jm
            ELSE   ! Stable
               DO jk = nmld(ji,jj), nbld(ji,jj)
                  zdb = MAX(  grav * ( zthermal * ( av_t_bl(ji,jj) - ts(ji,jj,jm,jp_tem,Kmm) ) -               &
                     &                 zbeta    * ( av_s_bl(ji,jj) - ts(ji,jj,jm,jp_sal,Kmm) ) ), 0.0_wp ) +   &
                     &  2.0_wp * svstr(ji,jj)**2 / zhbl_s
                  !
                  ! Alan is thuis right? I have simply changed hbli to hbl
                  shol(ji,jj)  = -1.0_wp * zhbl_s / ( ( svstr(ji,jj)**3 + epsln ) / swbav(ji,jj) )
                  pdhdt(ji,jj) = -1.0_wp * ( swbav(ji,jj) - 0.04_wp / 2.0_wp * swstrl(ji,jj)**3 / zhbl_s -   &
                     &                       0.15_wp / 2.0_wp * ( 1.0_wp - EXP( -1.5_wp * sla(ji,jj) ) ) *   &
                     &                                 sustar(ji,jj)**3 / zhbl_s ) *                         &
                     &           ( 0.725_wp + 0.225_wp * EXP( -7.5_wp * shol(ji,jj) ) )
                  pdhdt(ji,jj) = pdhdt(ji,jj) + swbav(ji,jj)
                  zhbl_s = zhbl_s + MIN( pdhdt(ji,jj) / zdb * rn_Dt / REAL( nbld(ji,jj) - nmld(ji,jj), KIND=wp ),   &
                     &                   e3w(ji,jj,jm,Kmm) )
                  
                  !                    zhbl_s = MIN(zhbl_s, gdepw(ji,jj, mbkt(ji,jj) + 1,Kmm) - depth_tol)
                  IF ( zhbl_s >= mbkt(ji,jj) + 1 ) THEN
                     zhbl_s      = MIN( zhbl_s,  gdepw(ji,jj,mbkt(ji,jj)+1,Kmm) - depth_tol )
                     l_pyc(ji,jj) = .FALSE.
                  ENDIF
                  IF ( zhbl_s >= gdepw(ji,jj,jm,Kmm) ) jm = jm + 1
               END DO
            ENDIF   ! IF ( l_conv )
            hbl(ji,jj)  = MAX( zhbl_s, gdepw(ji,jj,4,Kmm) )
            nbld(ji,jj) = MAX( jm, 4 )
         ELSE
            ! change zero or one model level.
            hbl(ji,jj) = MAX( phbl_t(ji,jj), gdepw(ji,jj,4,Kmm) )
         ENDIF
         phbl(ji,jj) = gdepw(ji,jj,nbld(ji,jj),Kmm)
      END_2D
      !
   END SUBROUTINE zdf_osm_timestep_hbl

   SUBROUTINE zdf_osm_pycnocline_thickness( Kmm, pdh, phml, pdhdt, phbl,   &
      &                                     pwb_ent, pdbdz_bl_ext, pwb_fk_b )
      !!---------------------------------------------------------------------
      !!            ***  ROUTINE zdf_osm_pycnocline_thickness  ***
      !!
      !! ** Purpose : Calculates thickness of the pycnocline
      !!
      !! ** Method  : The thickness is calculated from a prognostic equation
      !!              that relaxes the pycnocine thickness to a diagnostic
      !!              value. The time change is calculated assuming the
      !!              thickness relaxes exponentially. This is done to deal
      !!              with large timesteps.
      !!
      !!----------------------------------------------------------------------
      INTEGER,                            INTENT(in   ) ::   Kmm            ! Ocean time-level index
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(inout) ::   pdh            ! Pycnocline thickness
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(inout) ::   phml           ! ML depth
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   pdhdt          ! BL depth tendency
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   phbl           ! BL depth
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   pwb_ent        ! Buoyancy entrainment flux
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   pdbdz_bl_ext   ! External buoyancy gradients
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   pwb_fk_b       ! MLE buoyancy flux averaged over OSBL
      !!
      INTEGER  ::   jj, ji
      INTEGER  ::   inhml
      REAL(wp) ::   zari, ztau, zdh_ref, zddhdt, zvel_max
      REAL(wp) ::   ztmp   ! Auxiliary variable
      !!
      REAL, PARAMETER ::   pp_ddh = 2.5_wp, pp_ddh_2 = 3.5_wp   ! Also in pycnocline_depth
      !!----------------------------------------------------------------------
      !
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         !
         IF ( l_shear(ji,jj) ) THEN
            !
            IF ( l_conv(ji,jj) ) THEN
               !
               IF ( av_db_bl(ji,jj) > 1e-15_wp ) THEN
                  IF ( n_ddh(ji,jj) == 0 ) THEN
                     zvel_max = ( svstr(ji,jj)**3 + 0.5_wp * swstrc(ji,jj)**3 )**p2third / hbl(ji,jj)
                     ! ddhdt for pycnocline determined in osm_calculate_dhdt
                     zddhdt = -1.0_wp * pp_ddh * ( 1.0_wp - 1.6_wp * pdh(ji,jj) / phbl(ji,jj) ) * pwb_ent(ji,jj) /   &
                        &     ( zvel_max + MAX( av_db_bl(ji,jj), 1e-15 ) )
                     zddhdt = EXP( -4.0_wp * ABS( ff_t(ji,jj) ) * phbl(ji,jj) / MAX( sustar(ji,jj), 1e-8 ) ) * zddhdt
                     ! Maximum limit for how thick the shear layer can grow relative to the thickness of the boundary layer
                     dh(ji,jj) = MIN( dh(ji,jj) + zddhdt * rn_Dt, 0.625_wp * hbl(ji,jj) )
                  ELSE   ! Need to recalculate because hbl has been updated
                     IF ( ( swstrc(ji,jj) / svstr(ji,jj) )**3 <= 0.5_wp ) THEN
                        ztmp = svstr(ji,jj)
                     ELSE
                        ztmp = swstrc(ji,jj)
                     END IF
                     zari = MIN( 1.5_wp * av_db_bl(ji,jj) / ( phbl(ji,jj) * ( MAX( pdbdz_bl_ext(ji,jj), 0.0_wp ) +        &
                        &                                                   av_db_bl(ji,jj)**2 / MAX( 4.5_wp * ztmp**2,   &
                        &                                                                           1e-12_wp ) ) ), 0.2_wp )
                     ztau = MAX( av_db_bl(ji,jj) * ( zari * hbl(ji,jj) ) /   &
                        &        ( pp_ddh_2 * MAX( -1.0_wp * pwb_ent(ji,jj), 1e-12_wp ) ), 2.0_wp * rn_Dt )
                     dh(ji,jj) = dh(ji,jj) * EXP( -1.0_wp * rn_Dt / ztau ) +   &
                        &        zari * phbl(ji,jj) * ( 1.0_wp - EXP( -1.0_wp * rn_Dt / ztau ) )
                     IF ( dh(ji,jj) >= hbl(ji,jj) ) dh(ji,jj) = zari * phbl(ji,jj)
                  END IF
               ELSE
                  ztau = MAX( MAX( hbl(ji,jj) / ( svstr(ji,jj)**3 + 0.5_wp * swstrc(ji,jj)**3 )**pthird, epsln), 2.0_wp * rn_Dt )
                  dh(ji,jj) = dh(ji,jj) * EXP( -1.0_wp * rn_Dt / ztau ) +   &
                     &        0.2_wp * phbl(ji,jj) * ( 1.0_wp - EXP( -1.0_wp * rn_Dt / ztau ) )
                  IF ( dh(ji,jj) > hbl(ji,jj) ) dh(ji,jj) = 0.2_wp * hbl(ji,jj)
               END IF
               !
            ELSE   ! l_conv
               ! Initially shear only for entraining OSBL. Stable code will be needed if extended to stable OSBL
               ztau = hbl(ji,jj) / MAX(svstr(ji,jj), epsln)
               IF ( pdhdt(ji,jj) >= 0.0_wp ) THEN   ! Probably shouldn't include wm here
                  ! Boundary layer deepening
                  IF ( av_db_bl(ji,jj) > 0.0_wp ) THEN
                     ! Pycnocline thickness set by stratification - use same relationship as for neutral conditions
                     zari    = MIN( 4.5_wp * ( svstr(ji,jj)**2 ) / MAX( av_db_bl(ji,jj) * phbl(ji,jj), epsln ) + 0.01_wp, 0.2_wp )
                     zdh_ref = MIN( zari, 0.2_wp ) * hbl(ji,jj)
                  ELSE
                     zdh_ref = 0.2_wp * hbl(ji,jj)
                  ENDIF
               ELSE   ! IF(dhdt < 0)
                  zdh_ref = 0.2_wp * hbl(ji,jj)
               ENDIF   ! IF (dhdt >= 0)
               dh(ji,jj) = dh(ji,jj) * EXP( -1.0_wp * rn_Dt / ztau ) + zdh_ref * ( 1.0_wp - EXP( -1.0_wp * rn_Dt / ztau ) )
               IF ( pdhdt(ji,jj) < 0.0_wp .AND. dh(ji,jj) >= hbl(ji,jj) ) dh(ji,jj) = zdh_ref   ! Can be a problem with dh>hbl for
               !                                                                                !    rapid collapse
            ENDIF
            !
         ELSE   ! l_shear = .FALSE., calculate ddhdt here
            !
            IF ( l_conv(ji,jj) ) THEN
               !
               IF( ln_osm_mle ) THEN
                  IF ( ( pwb_ent(ji,jj) + 2.0_wp * pwb_fk_b(ji,jj) ) < 0.0_wp ) THEN   ! OSBL is deepening. Note wb_fk_b is zero if
                     !                                                                 !    ln_osm_mle=F
                     IF ( av_db_bl(ji,jj) > 0.0_wp .AND. pdbdz_bl_ext(ji,jj) > 0.0_wp ) THEN
                        IF ( ( swstrc(ji,jj) / MAX( svstr(ji,jj), epsln) )**3 <= 0.5_wp ) THEN   ! Near neutral stability
                           ztmp = svstr(ji,jj)
                        ELSE   ! Unstable
                           ztmp = swstrc(ji,jj)
                        END IF
                        zari = MIN( 1.5_wp * av_db_bl(ji,jj) /                               &
                           &        ( phbl(ji,jj) * ( MAX( pdbdz_bl_ext(ji,jj), 0.0_wp ) +   &
                           &                          av_db_bl(ji,jj)**2 / MAX( 4.5_wp * ztmp**2 , 1e-12_wp ) ) ), 0.2_wp )
                     ELSE
                        zari = 0.2_wp
                     END IF
                  ELSE
                     zari = 0.2_wp
                  END IF
                  ztau    = 0.2_wp * hbl(ji,jj) / MAX( epsln, ( svstr(ji,jj)**3 + 0.5_wp * swstrc(ji,jj)**3 )**pthird )
                  zdh_ref = zari * hbl(ji,jj)
               ELSE   ! ln_osm_mle
                  IF ( av_db_bl(ji,jj) > 0.0_wp .AND. pdbdz_bl_ext(ji,jj) > 0.0_wp ) THEN
                     IF ( ( swstrc(ji,jj) / MAX( svstr(ji,jj), epsln ) )**3 <= 0.5_wp ) THEN   ! Near neutral stability
                        ztmp = svstr(ji,jj)
                     ELSE   ! Unstable
                        ztmp = swstrc(ji,jj)
                     END IF
                     zari    = MIN( 1.5_wp * av_db_bl(ji,jj) /                               &
                        &           ( phbl(ji,jj) * ( MAX( pdbdz_bl_ext(ji,jj), 0.0_wp ) +   &
                        &                             av_db_bl(ji,jj)**2 / MAX( 4.5_wp * ztmp**2 , 1e-12_wp ) ) ), 0.2_wp )
                  ELSE
                     zari    = 0.2_wp
                  END IF
                  ztau    = hbl(ji,jj) / MAX( epsln, ( svstr(ji,jj)**3 + 0.5_wp * swstrc(ji,jj)**3 )**pthird )
                  zdh_ref = zari * hbl(ji,jj)
               END IF   ! ln_osm_mle
               dh(ji,jj) = dh(ji,jj) * EXP( -1.0_wp * rn_Dt / ztau ) + zdh_ref * ( 1.0_wp - EXP( -1.0_wp * rn_Dt / ztau ) )
               !               IF ( pdhdt(ji,jj) < 0._wp .and. dh(ji,jj) >= hbl(ji,jj) ) dh(ji,jj) = zdh_ref
               IF ( dh(ji,jj) >= hbl(ji,jj) ) dh(ji,jj) = zdh_ref
               ! Alan: this hml is never defined or used
            ELSE   ! IF (l_conv)
               !
               ztau = hbl(ji,jj) / MAX( svstr(ji,jj), epsln )
               IF ( pdhdt(ji,jj) >= 0.0_wp ) THEN   ! Probably shouldn't include wm here
                  ! Boundary layer deepening
                  IF ( av_db_bl(ji,jj) > 0.0_wp ) THEN
                     ! Pycnocline thickness set by stratification - use same relationship as for neutral conditions.
                     zari    = MIN( 4.5_wp * ( svstr(ji,jj)**2 ) / MAX( av_db_bl(ji,jj) * phbl(ji,jj), epsln ) + 0.01_wp , 0.2_wp )
                     zdh_ref = MIN( zari, 0.2_wp ) * hbl(ji,jj)
                  ELSE
                     zdh_ref = 0.2_wp * hbl(ji,jj)
                  END IF
               ELSE   ! IF(dhdt < 0)
                  zdh_ref = 0.2_wp * hbl(ji,jj)
               END IF   ! IF (dhdt >= 0)
               dh(ji,jj) = dh(ji,jj) * EXP( -1.0_wp * rn_Dt / ztau ) + zdh_ref * ( 1.0_wp - EXP( -1.0_wp * rn_Dt / ztau ) )
               IF ( pdhdt(ji,jj) < 0.0_wp .AND. dh(ji,jj) >= hbl(ji,jj) ) dh(ji,jj) = zdh_ref   ! Can be a problem with dh>hbl for
               !                                                                                !    rapid collapse
            END IF   ! IF (l_conv)
            !
         END IF   ! l_shear
         !
         hml(ji,jj)  = hbl(ji,jj) - dh(ji,jj)
         inhml       = MAX( INT( dh(ji,jj) / MAX( e3t(ji,jj,nbld(ji,jj)-1,Kmm), 1e-3_wp ) ), 1 )
         nmld(ji,jj) = MAX( nbld(ji,jj) - inhml, 3 )
         phml(ji,jj) = gdepw(ji,jj,nmld(ji,jj),Kmm)
         pdh(ji,jj)  = phbl(ji,jj) - phml(ji,jj)
         !
      END_2D
      !
   END SUBROUTINE zdf_osm_pycnocline_thickness

   SUBROUTINE zdf_osm_pycnocline_buoyancy_profiles( Kmm, kp_ext, pdbdz, palpha, pdh,   &
      &                                             phbl, pdbdz_bl_ext, phml, pdhdt )
      !!---------------------------------------------------------------------
      !!       ***  ROUTINE zdf_osm_pycnocline_buoyancy_profiles  ***
      !!
      !! ** Purpose : calculate pycnocline buoyancy profiles
      !!
      !! ** Method  : 
      !!
      !!----------------------------------------------------------------------
      INTEGER,                                 INTENT(in   ) ::   Kmm            ! Ocean time-level index
      INTEGER,  DIMENSION(A2D(nn_hls-1)),      INTENT(in   ) ::   kp_ext         ! External-level offsets
      REAL(wp), DIMENSION(A2D(nn_hls-1),jpk),  INTENT(  out) ::   pdbdz          ! Gradients in the pycnocline
      REAL(wp), DIMENSION(A2D(nn_hls-1)),      INTENT(  out) ::   palpha
      REAL(wp), DIMENSION(A2D(nn_hls-1)),      INTENT(in   ) ::   pdh            ! Pycnocline thickness
      REAL(wp), DIMENSION(A2D(nn_hls-1)),      INTENT(in   ) ::   phbl           ! BL depth
      REAL(wp), DIMENSION(A2D(nn_hls-1)),      INTENT(in   ) ::   pdbdz_bl_ext   ! External buoyancy gradients
      REAL(wp), DIMENSION(A2D(nn_hls-1)),      INTENT(in   ) ::   phml           ! ML depth
      REAL(wp), DIMENSION(A2D(nn_hls-1)),      INTENT(in   ) ::   pdhdt          ! Rates of change of hbl
      !!
      INTEGER  ::   jk, jj, ji
      REAL(wp) ::   zbgrad
      REAL(wp) ::   zgamma_b_nd, znd
      REAL(wp) ::   zzeta_m
      REAL(wp) ::   ztmp   ! Auxiliary variable
      !!
      REAL(wp), PARAMETER ::   pp_gamma_b = 2.25_wp
      REAL(wp), PARAMETER ::   pp_large   = -1e10_wp
      !!----------------------------------------------------------------------
      !
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )      
         pdbdz(ji,jj,:) = pp_large
         palpha(ji,jj)  = pp_large
      END_2D
      !
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         !
         IF ( nbld(ji,jj) + kp_ext(ji,jj) < mbkt(ji,jj) ) THEN
            !
            IF ( l_conv(ji,jj) ) THEN   ! Convective conditions
               !
               IF ( l_pyc(ji,jj) ) THEN
                  !
                  zzeta_m = 0.1_wp + 0.3_wp / ( 1.0_wp + EXP( -3.5_wp * LOG10( -1.0_wp * shol(ji,jj) ) ) )
                  palpha(ji,jj) = 2.0_wp * ( 1.0_wp - ( 0.80_wp * zzeta_m + 0.5_wp * SQRT( 3.14159_wp / pp_gamma_b ) ) *   &
                     &                                pdbdz_bl_ext(ji,jj) * pdh(ji,jj) / av_db_ml(ji,jj) ) /                &
                     &            ( 0.723_wp + SQRT( 3.14159_wp / pp_gamma_b ) )
                  palpha(ji,jj) = MAX( palpha(ji,jj), 0.0_wp )
                  ztmp = 1.0_wp / MAX( pdh(ji,jj), epsln )
                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                  ! Commented lines in this section are not needed in new code, once tested !
                  ! can be removed                                                          !
                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                  ! ztgrad = zalpha * av_dt_ml(ji,jj) * ztmp + zdtdz_bl_ext(ji,jj)
                  ! zsgrad = zalpha * av_ds_ml(ji,jj) * ztmp + zdsdz_bl_ext(ji,jj)
                  zbgrad = palpha(ji,jj) * av_db_ml(ji,jj) * ztmp + pdbdz_bl_ext(ji,jj)
                  zgamma_b_nd = pdbdz_bl_ext(ji,jj) * pdh(ji,jj) / MAX( av_db_ml(ji,jj), epsln )
                  DO jk = 2, nbld(ji,jj)
                     znd = -1.0_wp * ( gdepw(ji,jj,jk,Kmm) - phbl(ji,jj) ) * ztmp
                     IF ( znd <= zzeta_m ) THEN
                        ! zdtdz(ji,jj,jk) = zdtdz_bl_ext(ji,jj) + zalpha * av_dt_ml(ji,jj) * ztmp * &
                        ! &        EXP( -6.0 * ( znd -zzeta_m )**2 )
                        ! zdsdz(ji,jj,jk) = zdsdz_bl_ext(ji,jj) + zalpha * av_ds_ml(ji,jj) * ztmp * &
                        ! & EXP( -6.0 * ( znd -zzeta_m )**2 )
                        pdbdz(ji,jj,jk) = pdbdz_bl_ext(ji,jj) + palpha(ji,jj) * av_db_ml(ji,jj) * ztmp * &
                           & EXP( -6.0_wp * ( znd -zzeta_m )**2 )
                     ELSE
                        ! zdtdz(ji,jj,jk) =  ztgrad * EXP( -pp_gamma_b * ( znd - zzeta_m )**2 )
                        ! zdsdz(ji,jj,jk) =  zsgrad * EXP( -pp_gamma_b * ( znd - zzeta_m )**2 )
                        pdbdz(ji,jj,jk) =  zbgrad * EXP( -1.0_wp * pp_gamma_b * ( znd - zzeta_m )**2 )
                     END IF
                  END DO
               END IF   ! If no pycnocline pycnocline gradients set to zero
               !
            ELSE   ! Stable conditions
               ! If pycnocline profile only defined when depth steady of increasing.
               IF ( pdhdt(ji,jj) > 0.0_wp ) THEN   ! Depth increasing, or steady.
                  IF ( av_db_bl(ji,jj) > 0.0_wp ) THEN
                     IF ( shol(ji,jj) >= 0.5_wp ) THEN   ! Very stable - 'thick' pycnocline
                        ztmp = 1.0_wp / MAX( phbl(ji,jj), epsln )
                        zbgrad = av_db_bl(ji,jj) * ztmp
                        DO jk = 2, nbld(ji,jj)
                           znd = gdepw(ji,jj,jk,Kmm) * ztmp
                           pdbdz(ji,jj,jk) = zbgrad * EXP( -15.0_wp * ( znd - 0.9_wp )**2 )
                        END DO
                     ELSE   ! Slightly stable - 'thin' pycnoline - needed when stable layer begins to form.
                        ztmp = 1.0_wp / MAX( pdh(ji,jj), epsln )
                        zbgrad = av_db_bl(ji,jj) * ztmp
                        DO jk = 2, nbld(ji,jj)
                           znd = -1.0_wp * ( gdepw(ji,jj,jk,Kmm) - phml(ji,jj) ) * ztmp
                           pdbdz(ji,jj,jk) = zbgrad * EXP( -1.75_wp * ( znd + 0.75_wp )**2 )
                        END DO
                     END IF   ! IF (shol >=0.5)
                  END IF      ! IF (av_db_bl> 0.)
               END IF         ! IF (pdhdt >= 0) pdhdt < 0 not considered since pycnocline profile is zero and profile arrays are
               !              !    intialized to zero
               !
            END IF            ! IF (l_conv)
            !
         END IF   ! IF ( nbld(ji,jj) < mbkt(ji,jj) )
         !
      END_2D
      !
      IF ( ln_dia_pyc_scl ) THEN   ! Output of pycnocline gradient profiles
         CALL zdf_osm_iomput( "zdbdz_pyc", wmask(A2D(0),:) * pdbdz(A2D(0),:) )
      END IF
      !
   END SUBROUTINE zdf_osm_pycnocline_buoyancy_profiles

   SUBROUTINE zdf_osm_diffusivity_viscosity( Kbb, Kmm, pdiffut, pviscos, phbl,   &
      &                                      phml, pdh, pdhdt, pshear,           &
      &                                      pwb_ent, pwb_min )
      !!---------------------------------------------------------------------
      !!           ***  ROUTINE zdf_osm_diffusivity_viscosity  ***
      !!
      !! ** Purpose : Determines the eddy diffusivity and eddy viscosity
      !!              profiles in the mixed layer and the pycnocline.
      !!
      !! ** Method  :
      !!
      !!----------------------------------------------------------------------
      INTEGER,                                 INTENT(in   ) ::   Kbb, Kmm       ! Ocean time-level indices
      REAL(wp), DIMENSION(A2D(nn_hls-1),jpk),  INTENT(inout) ::   pdiffut        ! t-diffusivity
      REAL(wp), DIMENSION(A2D(nn_hls-1),jpk),  INTENT(inout) ::   pviscos        ! Viscosity
      REAL(wp), DIMENSION(A2D(nn_hls-1)),      INTENT(in   ) ::   phbl           ! BL depth
      REAL(wp), DIMENSION(A2D(nn_hls-1)),      INTENT(in   ) ::   phml           ! ML depth
      REAL(wp), DIMENSION(A2D(nn_hls-1)),      INTENT(in   ) ::   pdh            ! Pycnocline depth
      REAL(wp), DIMENSION(A2D(nn_hls-1)),      INTENT(in   ) ::   pdhdt          ! BL depth tendency
      REAL(wp), DIMENSION(A2D(nn_hls-1)),      INTENT(in   ) ::   pshear         ! Shear production
      REAL(wp), DIMENSION(A2D(nn_hls-1)),      INTENT(in   ) ::   pwb_ent        ! Buoyancy entrainment flux
      REAL(wp), DIMENSION(A2D(nn_hls-1)),      INTENT(in   ) ::   pwb_min
      !!
      INTEGER ::   ji, jj, jk   ! Loop indices
      !! Scales used to calculate eddy diffusivity and viscosity profiles
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zdifml_sc,    zvisml_sc
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zdifpyc_n_sc, zdifpyc_s_sc
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zvispyc_n_sc, zvispyc_s_sc
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zbeta_d_sc,   zbeta_v_sc
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zb_coup,      zc_coup_vis,  zc_coup_dif
      !!
      REAL(wp) ::   zvel_sc_pyc, zvel_sc_ml, zstab_fac, zz_b
      REAL(wp) ::   za_cubic, zb_d_cubic, zc_d_cubic, zd_d_cubic,   &   ! Coefficients in cubic polynomial specifying diffusivity
         &                    zb_v_cubic, zc_v_cubic, zd_v_cubic        ! and viscosity in pycnocline
      REAL(wp) ::   zznd_ml, zznd_pyc, ztmp
      REAL(wp) ::   zmsku, zmskv
      !!
      REAL(wp), PARAMETER ::   pp_dif_ml     = 0.8_wp,  pp_vis_ml  = 0.375_wp
      REAL(wp), PARAMETER ::   pp_dif_pyc    = 0.15_wp, pp_vis_pyc = 0.142_wp
      REAL(wp), PARAMETER ::   pp_vispyc_shr = 0.15_wp
      !!----------------------------------------------------------------------
      !
      zb_coup(:,:) = 0.0_wp
      !
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         IF ( l_conv(ji,jj) ) THEN
            !
            zvel_sc_pyc = ( 0.15_wp * svstr(ji,jj)**3 + swstrc(ji,jj)**3 + 4.25_wp * pshear(ji,jj) * phbl(ji,jj) )**pthird
            zvel_sc_ml  = ( svstr(ji,jj)**3 + 0.5_wp * swstrc(ji,jj)**3 )**pthird
            zstab_fac   = ( phml(ji,jj) / zvel_sc_ml *   &
               &            ( 1.4_wp - 0.4_wp / ( 1.0_wp + EXP(-3.5_wp * LOG10( -1.0_wp * shol(ji,jj) ) ) )**1.25_wp ) )**2
            !
            zdifml_sc(ji,jj) = pp_dif_ml * phml(ji,jj) * zvel_sc_ml
            zvisml_sc(ji,jj) = pp_vis_ml * zdifml_sc(ji,jj)
            !
            IF ( l_pyc(ji,jj) ) THEN
               zdifpyc_n_sc(ji,jj) = pp_dif_pyc * zvel_sc_ml * pdh(ji,jj)
               zvispyc_n_sc(ji,jj) = 0.09_wp * zvel_sc_pyc * ( 1.0_wp - phbl(ji,jj) / pdh(ji,jj) )**2 *   &
                  &                  ( 0.005_wp  * ( av_u_ml(ji,jj) - av_u_bl(ji,jj) )**2 +     &
                  &                    0.0075_wp * ( av_v_ml(ji,jj) - av_v_bl(ji,jj) )**2 ) /   &
                  &                  pdh(ji,jj)
               zvispyc_n_sc(ji,jj) = pp_vis_pyc * zvel_sc_ml * pdh(ji,jj) + zvispyc_n_sc(ji,jj) * zstab_fac
               !
               IF ( l_shear(ji,jj) .AND. n_ddh(ji,jj) /= 2 ) THEN
                  ztmp = pp_vispyc_shr * ( pshear(ji,jj) * phbl(ji,jj) )**pthird * phbl(ji,jj)
                  zdifpyc_n_sc(ji,jj) = zdifpyc_n_sc(ji,jj) + ztmp
                  zvispyc_n_sc(ji,jj) = zvispyc_n_sc(ji,jj) + ztmp
               ENDIF
               !
               zdifpyc_s_sc(ji,jj) = pwb_ent(ji,jj) + 0.0025_wp * zvel_sc_pyc * ( phbl(ji,jj) / pdh(ji,jj) - 1.0_wp ) *   &
                  &                                   ( av_b_ml(ji,jj) - av_b_bl(ji,jj) )
               zvispyc_s_sc(ji,jj) = 0.09_wp * ( pwb_min(ji,jj) + 0.0025_wp * zvel_sc_pyc *                 &
                  &                                               ( phbl(ji,jj) / pdh(ji,jj) - 1.0_wp ) *   &
                  &                                               ( av_b_ml(ji,jj) - av_b_bl(ji,jj) ) )
               zdifpyc_s_sc(ji,jj) = 0.09_wp * zdifpyc_s_sc(ji,jj) * zstab_fac
               zvispyc_s_sc(ji,jj) = zvispyc_s_sc(ji,jj) * zstab_fac
               !
               zdifpyc_s_sc(ji,jj) = MAX( zdifpyc_s_sc(ji,jj), -0.5_wp * zdifpyc_n_sc(ji,jj) )
               zvispyc_s_sc(ji,jj) = MAX( zvispyc_s_sc(ji,jj), -0.5_wp * zvispyc_n_sc(ji,jj) )
               
               zbeta_d_sc(ji,jj) = 1.0_wp - ( ( zdifpyc_n_sc(ji,jj) + 1.4_wp * zdifpyc_s_sc(ji,jj) ) /   &
                  &                           ( zdifml_sc(ji,jj) + epsln ) )**p2third
               zbeta_v_sc(ji,jj) = 1.0_wp - 2.0_wp * ( zvispyc_n_sc(ji,jj) + zvispyc_s_sc(ji,jj) ) / ( zvisml_sc(ji,jj) + epsln )
            ELSE
               zdifpyc_n_sc(ji,jj) = pp_dif_pyc * zvel_sc_ml * pdh(ji,jj)   ! ag 19/03
               zdifpyc_s_sc(ji,jj) = 0.0_wp   ! ag 19/03
               zvispyc_n_sc(ji,jj) = pp_vis_pyc * zvel_sc_ml * pdh(ji,jj)   ! ag 19/03
               zvispyc_s_sc(ji,jj) = 0.0_wp   ! ag 19/03
               IF(l_coup(ji,jj) ) THEN   ! ag 19/03
                  ! code from SUBROUTINE tke_tke zdftke.F90; uses bottom drag velocity rCdU_bot(ji,jj) = -Cd|ub|
                  !     already calculated at T-points in SUBROUTINE zdf_drg from zdfdrg.F90
                  !  Gives friction velocity sqrt bottom drag/rho_0 i.e. u* = SQRT(rCdU_bot*ub)
                  ! wet-cell averaging ..
                  zmsku = 0.5_wp * ( 2.0_wp - umask(ji-1,jj,mbkt(ji,jj)) * umask(ji,jj,mbkt(ji,jj)) )
                  zmskv = 0.5_wp * ( 2.0_wp - vmask(ji,jj-1,mbkt(ji,jj)) * vmask(ji,jj,mbkt(ji,jj)) )
                  zb_coup(ji,jj) = 0.4_wp * SQRT(-1.0_wp * rCdU_bot(ji,jj) *   &
                     &             SQRT(  ( zmsku*( uu(ji,jj,mbkt(ji,jj),Kbb)+uu(ji-1,jj,mbkt(ji,jj),Kbb) ) )**2   &
                     &                  + ( zmskv*( vv(ji,jj,mbkt(ji,jj),Kbb)+vv(ji,jj-1,mbkt(ji,jj),Kbb) ) )**2  ) )
                  
                  zz_b = -1.0_wp * gdepw(ji,jj,mbkt(ji,jj)+1,Kmm)   ! ag 19/03
                  zc_coup_vis(ji,jj) = -0.5_wp * ( 0.5_wp * zvisml_sc(ji,jj) / phml(ji,jj) - zb_coup(ji,jj) ) /   &
                     &                 ( phml(ji,jj) + zz_b )   ! ag 19/03
                  zz_b = -1.0_wp * phml(ji,jj) + gdepw(ji,jj,mbkt(ji,jj)+1,Kmm)   ! ag 19/03
                  zbeta_v_sc(ji,jj) = 1.0_wp - 2.0_wp * ( zb_coup(ji,jj) * zz_b + zc_coup_vis(ji,jj) * zz_b**2 ) /   &
                     &                                  zvisml_sc(ji,jj)   ! ag 19/03
                  zbeta_d_sc(ji,jj) = 1.0_wp - ( ( zb_coup(ji,jj) * zz_b + zc_coup_vis(ji,jj) * zz_b**2 ) /   &
                     &                           zdifml_sc(ji,jj) )**p2third
                  zc_coup_dif(ji,jj) = 0.5_wp * ( -zdifml_sc(ji,jj) / phml(ji,jj) * ( 1.0_wp - zbeta_d_sc(ji,jj) )**1.5_wp +   &
                     &                 1.5_wp * ( zdifml_sc(ji,jj) / phml(ji,jj) ) * zbeta_d_sc(ji,jj) *   &
                     &                          SQRT( 1.0_wp - zbeta_d_sc(ji,jj) ) - zb_coup(ji,jj) ) / zz_b   ! ag 19/03
               ELSE   ! ag 19/03
                  zbeta_d_sc(ji,jj) = 1.0_wp - ( ( zdifpyc_n_sc(ji,jj) + 1.4_wp * zdifpyc_s_sc(ji,jj) ) /   &
                     &                           ( zdifml_sc(ji,jj) + epsln ) )**p2third   ! ag 19/03
                  zbeta_v_sc(ji,jj) = 1.0_wp - 2.0_wp * ( zvispyc_n_sc(ji,jj) + zvispyc_s_sc(ji,jj) ) /   &
                     &                         ( zvisml_sc(ji,jj) + epsln )   ! ag 19/03
               ENDIF   ! ag 19/03
            ENDIF      ! ag 19/03
         ELSE
            zdifml_sc(ji,jj) = svstr(ji,jj) * phbl(ji,jj) * MAX( EXP ( -1.0_wp * ( shol(ji,jj) / 0.6_wp )**2 ), 0.2_wp)
            zvisml_sc(ji,jj) = zdifml_sc(ji,jj)
         END IF
      END_2D
      !
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         IF ( l_conv(ji,jj) ) THEN
            DO jk = 2, nmld(ji,jj)   ! Mixed layer diffusivity
               zznd_ml = gdepw(ji,jj,jk,Kmm) / phml(ji,jj)
               pdiffut(ji,jj,jk) = zdifml_sc(ji,jj) * zznd_ml * ( 1.0_wp - zbeta_d_sc(ji,jj) * zznd_ml )**1.5
               pviscos(ji,jj,jk) = zvisml_sc(ji,jj) * zznd_ml * ( 1.0_wp - zbeta_v_sc(ji,jj) * zznd_ml ) *   &
                  &                ( 1.0_wp - 0.5_wp * zznd_ml**2 )
            END DO
            !
            ! Coupling to bottom
            !
            IF ( l_coup(ji,jj) ) THEN                                                         ! ag 19/03
               DO jk = mbkt(ji,jj), nmld(ji,jj), -1                                           ! ag 19/03
                  zz_b = -1.0_wp * ( gdepw(ji,jj,jk,Kmm) - gdepw(ji,jj,mbkt(ji,jj)+1,Kmm) )   ! ag 19/03
                  pviscos(ji,jj,jk) = zb_coup(ji,jj) * zz_b + zc_coup_vis(ji,jj) * zz_b**2    ! ag 19/03
                  pdiffut(ji,jj,jk) = zb_coup(ji,jj) * zz_b + zc_coup_dif(ji,jj) * zz_b**2    ! ag 19/03
               END DO                                                                         ! ag 19/03
            ENDIF                                                                             ! ag 19/03
            ! Pycnocline
            IF ( l_pyc(ji,jj) ) THEN 
               ! Diffusivity and viscosity profiles in the pycnocline given by
               ! cubic polynomial. Note, if l_pyc TRUE can't be coupled to seabed.
               za_cubic = 0.5_wp
               zb_d_cubic = -1.75_wp * zdifpyc_s_sc(ji,jj) / zdifpyc_n_sc(ji,jj)
               zd_d_cubic = ( pdh(ji,jj) * zdifml_sc(ji,jj) / phml(ji,jj) * SQRT( 1.0_wp - zbeta_d_sc(ji,jj) ) *   &
                  &           ( 2.5_wp * zbeta_d_sc(ji,jj) - 1.0_wp ) - 0.85_wp * zdifpyc_s_sc(ji,jj) ) /            &
                  &           MAX( zdifpyc_n_sc(ji,jj), 1.0e-8_wp )
               zd_d_cubic = zd_d_cubic - zb_d_cubic - 2.0_wp * ( 1.0_wp - za_cubic  - zb_d_cubic )
               zc_d_cubic = 1.0_wp - za_cubic - zb_d_cubic - zd_d_cubic
               zb_v_cubic = -1.75_wp * zvispyc_s_sc(ji,jj) / zvispyc_n_sc(ji,jj)
               zd_v_cubic = ( 0.5_wp * zvisml_sc(ji,jj) * pdh(ji,jj) / phml(ji,jj) - 0.85_wp * zvispyc_s_sc(ji,jj) ) /   &
                  &           MAX( zvispyc_n_sc(ji,jj), 1.0e-8_wp )
               zd_v_cubic = zd_v_cubic - zb_v_cubic - 2.0_wp * ( 1.0_wp - za_cubic - zb_v_cubic )
               zc_v_cubic = 1.0_wp - za_cubic - zb_v_cubic - zd_v_cubic
               DO jk = nmld(ji,jj) , nbld(ji,jj)
                  zznd_pyc = -1.0_wp * ( gdepw(ji,jj,jk,Kmm) - phbl(ji,jj) ) / MAX(pdh(ji,jj), 1.0e-6_wp )
                  ztmp = ( 1.75_wp * zznd_pyc - 0.15_wp * zznd_pyc**2 - 0.2_wp * zznd_pyc**3 )
                  !
                  pdiffut(ji,jj,jk) = zdifpyc_n_sc(ji,jj) *   &
                     &                ( za_cubic + zb_d_cubic * zznd_pyc + zc_d_cubic * zznd_pyc**2 + zd_d_cubic * zznd_pyc**3 )
                  !
                  pdiffut(ji,jj,jk) = pdiffut(ji,jj,jk) + zdifpyc_s_sc(ji,jj) * ztmp
                  pviscos(ji,jj,jk) = zvispyc_n_sc(ji,jj) *   &
                     &                ( za_cubic + zb_v_cubic * zznd_pyc + zc_v_cubic * zznd_pyc**2 + zd_v_cubic * zznd_pyc**3 )
                  pviscos(ji,jj,jk) = pviscos(ji,jj,jk) + zvispyc_s_sc(ji,jj) * ztmp
               END DO
   !                  IF ( pdhdt(ji,jj) > 0._wp ) THEN
   !                     zdiffut(ji,jj,nbld(ji,jj)+1) = MAX( 0.5 * pdhdt(ji,jj) * e3w(ji,jj,nbld(ji,jj)+1,Kmm), 1.0e-6 )
   !                     zviscos(ji,jj,nbld(ji,jj)+1) = MAX( 0.5 * pdhdt(ji,jj) * e3w(ji,jj,nbld(ji,jj)+1,Kmm), 1.0e-6 )
   !                  ELSE
   !                     zdiffut(ji,jj,nbld(ji,jj)) = 0._wp
   !                     zviscos(ji,jj,nbld(ji,jj)) = 0._wp
   !                  ENDIF
            ENDIF
         ELSE
            ! Stable conditions
            DO jk = 2, nbld(ji,jj)
               zznd_ml = gdepw(ji,jj,jk,Kmm) / phbl(ji,jj)
               pdiffut(ji,jj,jk) = 0.75_wp * zdifml_sc(ji,jj) * zznd_ml * ( 1.0_wp - zznd_ml )**1.5_wp
               pviscos(ji,jj,jk) = 0.375_wp * zvisml_sc(ji,jj) * zznd_ml * ( 1.0_wp - zznd_ml ) * ( 1.0_wp - zznd_ml**2 )
            END DO
            !
            IF ( pdhdt(ji,jj) > 0.0_wp ) THEN
               pdiffut(ji,jj,nbld(ji,jj)) = MAX( pdhdt(ji,jj), 1.0e-6_wp) * e3w(ji, jj, nbld(ji,jj), Kmm)
               pviscos(ji,jj,nbld(ji,jj)) = pdiffut(ji,jj,nbld(ji,jj))
            ENDIF
         ENDIF   ! End if ( l_conv )
         !
      END_2D
      CALL zdf_osm_iomput( "pb_coup", tmask(A2D(0),1) * zb_coup(A2D(0)) )   ! BBL-coupling velocity scale
      !
   END SUBROUTINE zdf_osm_diffusivity_viscosity

   SUBROUTINE zdf_osm_fgr_terms( Kmm, kp_ext, phbl, phml, pdh,                              &
      &                          pdhdt, pshear, pdtdz_bl_ext, pdsdz_bl_ext, pdbdz_bl_ext,   &
      &                          pdiffut, pviscos )
      !!---------------------------------------------------------------------
      !!                 ***  ROUTINE zdf_osm_fgr_terms ***
      !!
      !! ** Purpose : Compute non-gradient terms in flux-gradient relationship
      !!
      !! ** Method  :
      !!
      !!----------------------------------------------------------------------
      INTEGER,                                 INTENT(in   ) ::   Kmm            ! Time-level index
      INTEGER,  DIMENSION(A2D(nn_hls-1)),      INTENT(in   ) ::   kp_ext         ! Offset for external level
      REAL(wp), DIMENSION(A2D(nn_hls-1)),      INTENT(in   ) ::   phbl           ! BL depth
      REAL(wp), DIMENSION(A2D(nn_hls-1)),      INTENT(in   ) ::   phml           ! ML depth
      REAL(wp), DIMENSION(A2D(nn_hls-1)),      INTENT(in   ) ::   pdh            ! Pycnocline depth
      REAL(wp), DIMENSION(A2D(nn_hls-1)),      INTENT(in   ) ::   pdhdt          ! BL depth tendency
      REAL(wp), DIMENSION(A2D(nn_hls-1)),      INTENT(in   ) ::   pshear         ! Shear production
      REAL(wp), DIMENSION(A2D(nn_hls-1)),      INTENT(in   ) ::   pdtdz_bl_ext   ! External temperature gradients
      REAL(wp), DIMENSION(A2D(nn_hls-1)),      INTENT(in   ) ::   pdsdz_bl_ext   ! External salinity gradients
      REAL(wp), DIMENSION(A2D(nn_hls-1)),      INTENT(in   ) ::   pdbdz_bl_ext   ! External buoyancy gradients
      REAL(wp), DIMENSION(A2D(nn_hls-1),jpk),  INTENT(in   ) ::   pdiffut        ! t-diffusivity
      REAL(wp), DIMENSION(A2D(nn_hls-1),jpk),  INTENT(in   ) ::   pviscos        ! Viscosity
      !!
      REAL(wp), DIMENSION(A2D(nn_hls-1))     ::   zalpha_pyc   !
      REAL(wp), DIMENSION(A2D(nn_hls-1),jpk) ::   zdbdz_pyc    ! Parametrised gradient of buoyancy in the pycnocline
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   z3ddz_pyc_1, z3ddz_pyc_2   ! Pycnocline gradient/shear profiles
      !!
      INTEGER                            ::   ji, jj, jk, jkm_bld, jkf_mld, jkm_mld   ! Loop indices
      INTEGER                            ::   istat                                   ! Memory allocation status
      REAL(wp)                           ::   zznd_d, zznd_ml, zznd_pyc, znd          ! Temporary non-dimensional depths
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zsc_wth_1,zsc_ws_1                      ! Temporary scales
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zsc_uw_1, zsc_uw_2                      ! Temporary scales
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zsc_vw_1, zsc_vw_2                      ! Temporary scales
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   ztau_sc_u                               ! Dissipation timescale at base of WML
      REAL(wp)                           ::   zbuoy_pyc_sc, zdelta_pyc                !
      REAL(wp)                           ::   zl_c,zl_l,zl_eps                        ! Used to calculate turbulence length scale
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   za_cubic, zb_cubic                      ! Coefficients in cubic polynomial specifying
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zc_cubic, zd_cubic                      !    diffusivity in pycnocline
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zwt_pyc_sc_1, zws_pyc_sc_1              !
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zzeta_pyc                               !
      REAL(wp)                           ::   zomega, zvw_max                         !
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zuw_bse,zvw_bse                         ! Momentum, heat, and salinity fluxes
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zwth_ent,zws_ent                        !    at the top of the pycnocline
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   zsc_wth_pyc, zsc_ws_pyc                 ! Scales for pycnocline transport term
      REAL(wp)                           ::   ztmp                                    !
      REAL(wp)                           ::   ztgrad, zsgrad, zbgrad                  ! Variables used to calculate pycnocline
      !!                                                                              !    gradients
      REAL(wp)                           ::   zugrad, zvgrad                          ! Variables for calculating pycnocline shear
      REAL(wp)                           ::   zdtdz_pyc                               ! Parametrized gradient of temperature in
      !!                                                                              !    pycnocline
      REAL(wp)                           ::   zdsdz_pyc                               ! Parametrised gradient of salinity in
      !!                                                                              !    pycnocline
      REAL(wp)                           ::   zdudz_pyc                               ! u-shear across the pycnocline
      REAL(wp)                           ::   zdvdz_pyc                               ! v-shear across the pycnocline
      !!----------------------------------------------------------------------
      !
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      !  Pycnocline gradients for scalars and velocity
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      CALL zdf_osm_pycnocline_buoyancy_profiles( Kmm, kp_ext, zdbdz_pyc, zalpha_pyc, pdh,    &
         &                                       phbl, pdbdz_bl_ext, phml, pdhdt )
      !
      ! Auxiliary indices
      ! -----------------
      jkm_bld = 0
      jkf_mld = jpk
      jkm_mld = 0
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         IF ( nbld(ji,jj) > jkm_bld ) jkm_bld = nbld(ji,jj)
         IF ( nmld(ji,jj) < jkf_mld ) jkf_mld = nmld(ji,jj)
         IF ( nmld(ji,jj) > jkm_mld ) jkm_mld = nmld(ji,jj)
      END_2D
      !
      ! Stokes term in scalar flux, flux-gradient relationship
      ! ------------------------------------------------------
      WHERE ( l_conv(A2D(nn_hls-1)) )
         zsc_wth_1(:,:) = swstrl(A2D(nn_hls-1))**3 * swth0(A2D(nn_hls-1)) /   &
            &             ( svstr(A2D(nn_hls-1))**3 + 0.5_wp * swstrc(A2D(nn_hls-1))**3 + epsln )
         zsc_ws_1(:,:)  = swstrl(A2D(nn_hls-1))**3 * sws0(A2D(nn_hls-1))  /   &
            &             ( svstr(A2D(nn_hls-1))**3 + 0.5_wp * swstrc(A2D(nn_hls-1))**3 + epsln )
      ELSEWHERE
         zsc_wth_1(:,:) = 2.0_wp * swthav(A2D(nn_hls-1))
         zsc_ws_1(:,:)  = 2.0_wp * swsav(A2D(nn_hls-1))
      ENDWHERE
      DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 2, MAX( jkm_mld, jkm_bld ) )
         IF ( l_conv(ji,jj) ) THEN
            IF ( jk <= nmld(ji,jj) ) THEN
               zznd_d = gdepw(ji,jj,jk,Kmm) / dstokes(ji,jj)
               ghamt(ji,jj,jk) = ghamt(ji,jj,jk) + 1.35_wp * EXP( -1.0_wp * zznd_d ) *   &
                  &                                ( 1.0_wp - EXP( -2.0_wp * zznd_d ) ) * zsc_wth_1(ji,jj)
               ghams(ji,jj,jk) = ghams(ji,jj,jk) + 1.35_wp * EXP( -1.0_wp * zznd_d ) *   &
                  &                                ( 1.0_wp - EXP( -2.0_wp * zznd_d ) ) * zsc_ws_1(ji,jj)
            END IF
         ELSE   ! Stable conditions
            IF ( jk <= nbld(ji,jj) ) THEN
               zznd_d = gdepw(ji,jj,jk,Kmm) / dstokes(ji,jj)
               ghamt(ji,jj,jk) = ghamt(ji,jj,jk) + 2.15_wp * EXP( -0.85_wp * zznd_d ) *   &
                  &                                ( 1.0_wp - EXP( -4.0_wp * zznd_d ) ) * zsc_wth_1(ji,jj)
               ghams(ji,jj,jk) = ghams(ji,jj,jk) + 2.15_wp * EXP( -0.85_wp * zznd_d ) *   &
                  &                                ( 1.0_wp - EXP( -4.0_wp * zznd_d ) ) * zsc_ws_1(ji,jj)
            END IF
         END IF   ! Check on l_conv
      END_3D
      !
      IF ( ln_dia_osm ) THEN
         CALL zdf_osm_iomput( "ghamu_00", wmask(A2D(0),:) * ghamu(A2D(0),:) )
         CALL zdf_osm_iomput( "ghamv_00", wmask(A2D(0),:) * ghamv(A2D(0),:) )
      END IF
      !
      ! Stokes term in flux-gradient relationship (note in zsc_uw_n don't use
      ! svstr since term needs to go to zero as swstrl goes to zero)
      ! ---------------------------------------------------------------------
      WHERE ( l_conv(A2D(nn_hls-1)) )
         zsc_uw_1(:,:) = ( swstrl(A2D(nn_hls-1))**3 +                                                &
            &              0.5_wp * swstrc(A2D(nn_hls-1))**3 )**pthird * sustke(A2D(nn_hls-1)) /   &
            &              MAX( ( 1.0_wp - 1.0_wp * 6.5_wp * sla(A2D(nn_hls-1))**( 8.0_wp / 3.0_wp ) ), 0.2_wp )
         zsc_uw_2(:,:) = ( swstrl(A2D(nn_hls-1))**3 +                                                &
            &              0.5_wp * swstrc(A2D(nn_hls-1))**3 )**pthird * sustke(A2D(nn_hls-1)) /   &
            &              MIN( sla(A2D(nn_hls-1))**( 8.0_wp / 3.0_wp ) + epsln, 0.12_wp )
         zsc_vw_1(:,:) = ff_t(A2D(nn_hls-1)) * phml(A2D(nn_hls-1)) * sustke(A2D(nn_hls-1))**3 *   &
            &            MIN( sla(A2D(nn_hls-1))**( 8.0_wp / 3.0_wp ), 0.12_wp ) /                    &
            &            ( ( svstr(A2D(nn_hls-1))**3 + 0.5_wp * swstrc(A2D(nn_hls-1))**3 )**( 2.0_wp / 3.0_wp ) + epsln )
      ELSEWHERE
         zsc_uw_1(:,:) = sustar(A2D(nn_hls-1))**2
         zsc_vw_1(:,:) = ff_t(A2D(nn_hls-1)) * phbl(A2D(nn_hls-1)) * sustke(A2D(nn_hls-1))**3 *   &
            &            MIN( sla(A2D(nn_hls-1))**( 8.0_wp / 3.0_wp ), 0.12_wp ) / ( svstr(A2D(nn_hls-1))**2 + epsln )
      ENDWHERE
      DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 2, MAX( jkm_mld, jkm_bld ) )
         IF ( l_conv(ji,jj) ) THEN
            IF ( jk <= nmld(ji,jj) ) THEN
               zznd_d = gdepw(ji,jj,jk,Kmm) / dstokes(ji,jj)
               ghamu(ji,jj,jk) = ghamu(ji,jj,jk) + ( -0.05_wp   * EXP( -0.4_wp * zznd_d ) * zsc_uw_1(ji,jj) +     &
                  &                                  0.00125_wp * EXP( -1.0_wp * zznd_d ) * zsc_uw_2(ji,jj) ) *   &
                  &                                ( 1.0_wp - EXP( -2.0_wp * zznd_d ) )
               ghamv(ji,jj,jk) = ghamv(ji,jj,jk) - 0.65_wp *  0.15_wp * EXP( -1.0_wp * zznd_d ) *                 &
                  &                                ( 1.0_wp - EXP( -2.0_wp * zznd_d ) ) * zsc_vw_1(ji,jj)
            END IF
         ELSE   ! Stable conditions
            IF ( jk <= nbld(ji,jj) ) THEN   ! Corrected to nbld
               zznd_d = gdepw(ji,jj,jk,Kmm) / dstokes(ji,jj)
               ghamu(ji,jj,jk) = ghamu(ji,jj,jk) - 0.75_wp * 1.3_wp * EXP( -0.5_wp * zznd_d ) *             &
                  &                                ( 1.0_wp - EXP( -4.0_wp * zznd_d ) ) * zsc_uw_1(ji,jj)
            END IF
         END IF
      END_3D
      !
      ! Buoyancy term in flux-gradient relationship [note : includes ROI ratio
      ! (X0.3) and pressure (X0.5)]
      ! ----------------------------------------------------------------------
      WHERE ( l_conv(A2D(nn_hls-1)) )
         zsc_wth_1(:,:) = swbav(A2D(nn_hls-1)) * swth0(A2D(nn_hls-1)) * ( 1.0_wp + EXP( 0.2_wp * shol(A2D(nn_hls-1)) ) ) *   &
            &             phml(A2D(nn_hls-1)) / ( svstr(A2D(nn_hls-1))**3 + 0.5_wp * swstrc(A2D(nn_hls-1))**3 + epsln )
         zsc_ws_1(:,:)  = swbav(A2D(nn_hls-1)) * sws0(A2D(nn_hls-1))  * ( 1.0_wp + EXP( 0.2_wp * shol(A2D(nn_hls-1)) ) ) *   &
            &             phml(A2D(nn_hls-1)) / ( svstr(A2D(nn_hls-1))**3 + 0.5_wp * swstrc(A2D(nn_hls-1))**3 + epsln )
      ELSEWHERE
         zsc_wth_1(:,:) = 0.0_wp
         zsc_ws_1(:,:)  = 0.0_wp
      ENDWHERE
      DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 2, MAX( jkm_mld, jkm_bld ) )
         IF ( l_conv(ji,jj) ) THEN
            IF ( jk <= nmld(ji,jj) ) THEN
               zznd_ml = gdepw(ji,jj,jk,Kmm) / phml(ji,jj)
               ! Calculate turbulent time scale
               zl_c   = 0.9_wp * ( 1.0_wp - EXP( -5.0_wp * ( zznd_ml + zznd_ml**3 / 3.0_wp ) ) ) *                         &
                  &     ( 1.0_wp - EXP( -15.0_wp * ( 1.2_wp - zznd_ml ) ) )
               zl_l   = 2.0_wp * ( 1.0_wp - EXP( -2.0_wp * ( zznd_ml + zznd_ml**3 / 3.0_wp ) ) ) *                         &
                  &     ( 1.0_wp - EXP( -8.0_wp  * ( 1.15_wp - zznd_ml ) ) ) * ( 1.0_wp + dstokes(ji,jj) / phml (ji,jj) )
               zl_eps = zl_l + ( zl_c - zl_l ) / ( 1.0_wp + EXP( -3.0_wp * LOG10( -1.0_wp * shol(ji,jj) ) ) )**( 3.0_wp / 2.0_wp )
               ! Non-gradient buoyancy terms
               ghamt(ji,jj,jk) = ghamt(ji,jj,jk) + 0.3_wp * 0.4_wp * zsc_wth_1(ji,jj) * zl_eps / ( 0.15_wp + zznd_ml )
               ghams(ji,jj,jk) = ghams(ji,jj,jk) + 0.3_wp * 0.4_wp *  zsc_ws_1(ji,jj) * zl_eps / ( 0.15_wp + zznd_ml )
            END IF
         ELSE   ! Stable conditions
            IF ( jk <= nbld(ji,jj) ) THEN
               ghamt(ji,jj,jk) = ghamt(ji,jj,jk) + zsc_wth_1(ji,jj)
               ghams(ji,jj,jk) = ghams(ji,jj,jk) +  zsc_ws_1(ji,jj)
            END IF
         END IF
      END_3D
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         IF ( l_conv(ji,jj) .AND. l_pyc(ji,jj) ) THEN
            ztau_sc_u(ji,jj)    = phml(ji,jj) / ( svstr(ji,jj)**3 + swstrc(ji,jj)**3 )**pthird *                             &
               &                ( 1.4_wp - 0.4_wp / ( 1.0_wp + EXP( -3.5_wp * LOG10( -1.0_wp * shol(ji,jj) ) ) )**1.5_wp )
            zwth_ent(ji,jj)     = -0.003_wp * ( 0.15_wp * svstr(ji,jj)**3 + swstrc(ji,jj)**3 )**pthird *   &
               &                ( 1.0_wp - pdh(ji,jj) / phbl(ji,jj) ) * av_dt_ml(ji,jj)
            zws_ent(ji,jj)      = -0.003_wp * ( 0.15_wp * svstr(ji,jj)**3 + swstrc(ji,jj)**3 )**pthird *   &
               &                ( 1.0_wp - pdh(ji,jj) / phbl(ji,jj) ) * av_ds_ml(ji,jj)
            IF ( dh(ji,jj) < 0.2_wp * hbl(ji,jj) ) THEN
               zbuoy_pyc_sc        = 2.0_wp * MAX( av_db_ml(ji,jj), 0.0_wp ) / pdh(ji,jj)
               zdelta_pyc          = ( svstr(ji,jj)**3 + swstrc(ji,jj)**3 )**pthird /   &
                  &                       SQRT( MAX( zbuoy_pyc_sc, ( svstr(ji,jj)**3 + swstrc(ji,jj)**3 )**p2third / pdh(ji,jj)**2 ) )
               zwt_pyc_sc_1(ji,jj) = 0.325_wp * ( zalpha_pyc(ji,jj) * av_dt_ml(ji,jj) / pdh(ji,jj) + pdtdz_bl_ext(ji,jj) ) *   &
                  &                     zdelta_pyc**2 / pdh(ji,jj)
               zws_pyc_sc_1(ji,jj) = 0.325_wp * ( zalpha_pyc(ji,jj) * av_ds_ml(ji,jj) / pdh(ji,jj) + pdsdz_bl_ext(ji,jj) ) *   &
                  &                     zdelta_pyc**2 / pdh(ji,jj)
               zzeta_pyc(ji,jj)    = 0.15_wp - 0.175_wp / ( 1.0_wp + EXP( -3.5_wp * LOG10( -1.0_wp * shol(ji,jj) ) ) )
            END IF
         END IF
      END_2D
      DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 2, jkm_bld )
         IF ( l_conv(ji,jj) .AND. l_pyc(ji,jj) .AND. ( jk <= nbld(ji,jj) ) ) THEN
            zznd_pyc = -1.0_wp * ( gdepw(ji,jj,jk,Kmm) - phbl(ji,jj) ) / pdh(ji,jj)
            ghamt(ji,jj,jk) = ghamt(ji,jj,jk) -                                                                                &
               &              0.045_wp * ( ( zwth_ent(ji,jj) * zdbdz_pyc(ji,jj,jk) ) * ztau_sc_u(ji,jj)**2 ) *                 &
               &                         MAX( ( 1.75_wp * zznd_pyc -0.15_wp * zznd_pyc**2 - 0.2_wp * zznd_pyc**3 ), 0.0_wp )
            ghams(ji,jj,jk) = ghams(ji,jj,jk) -                                                                                &
               &              0.045_wp * ( ( zws_ent(ji,jj)  * zdbdz_pyc(ji,jj,jk) ) * ztau_sc_u(ji,jj)**2 ) *                 &
               &                         MAX( ( 1.75_wp * zznd_pyc -0.15_wp * zznd_pyc**2 - 0.2_wp * zznd_pyc**3 ), 0.0_wp )
            IF ( dh(ji,jj) < 0.2_wp * hbl(ji,jj) .AND. nbld(ji,jj) - nmld(ji,jj) > 3 ) THEN
               ghamt(ji,jj,jk) = ghamt(ji,jj,jk) + 0.05_wp  * zwt_pyc_sc_1(ji,jj) *                              &
                  &                                EXP( -0.25_wp * ( zznd_pyc / zzeta_pyc(ji,jj) )**2 ) *        &
                  &                                pdh(ji,jj) / ( svstr(ji,jj)**3 + swstrc(ji,jj)**3 )**pthird
               ghams(ji,jj,jk) = ghams(ji,jj,jk) + 0.05_wp  * zws_pyc_sc_1(ji,jj) *                              &
                  &                                EXP( -0.25_wp * ( zznd_pyc / zzeta_pyc(ji,jj) )**2 ) *        &
                  &                                pdh(ji,jj) / ( svstr(ji,jj)**3 + swstrc(ji,jj)**3 )**pthird
            END IF
         END IF   ! End of pycnocline
      END_3D
      !
      IF ( ln_dia_osm ) THEN
         CALL zdf_osm_iomput( "zwth_ent", tmask(A2D(0),1) * zwth_ent(A2D(0)) )   ! Upward turb. temperature entrainment flux
         CALL zdf_osm_iomput( "zws_ent",  tmask(A2D(0),1) * zws_ent(A2D(0))  )   ! Upward turb. salinity entrainment flux
      END IF
      !
      zsc_vw_1(:,:) = 0.0_wp
      WHERE ( l_conv(A2D(nn_hls-1)) )
         zsc_uw_1(:,:) = -1.0_wp * swb0(A2D(nn_hls-1)) * sustar(A2D(nn_hls-1))**2 * phml(A2D(nn_hls-1)) /   &
            &            ( svstr(A2D(nn_hls-1))**3 + 0.5_wp * swstrc(A2D(nn_hls-1))**3 + epsln )
         zsc_uw_2(:,:) =           swb0(A2D(nn_hls-1)) * sustke(A2D(nn_hls-1))    * phml(A2D(nn_hls-1)) /   &
            &            ( svstr(A2D(nn_hls-1))**3 + 0.5_wp * swstrc(A2D(nn_hls-1))**3 + epsln )**( 2.0_wp / 3.0_wp )
      ELSEWHERE
         zsc_uw_1(:,:) = 0.0_wp
      ENDWHERE
      DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 2, MAX( jkm_mld, jkm_bld ) )
         IF ( l_conv(ji,jj) ) THEN
            IF ( jk <= nmld(ji,jj) ) THEN
               zznd_d = gdepw(ji,jj,jk,Kmm) / dstokes(ji,jj)
               ghamu(ji,jj,jk) = ghamu(ji,jj,jk) + 0.3_wp * 0.5_wp *   &
                  &                                ( zsc_uw_1(ji,jj) + 0.125_wp * EXP( -0.5_wp * zznd_d ) *       &
                  &                                  (   1.0_wp - EXP( -0.5_wp * zznd_d ) ) * zsc_uw_2(ji,jj) )
               ghamv(ji,jj,jk) = ghamv(ji,jj,jk) + zsc_vw_1(ji,jj)
            END IF
         ELSE   ! Stable conditions
            IF ( jk <= nbld(ji,jj) ) THEN
               ghamu(ji,jj,jk) = ghamu(ji,jj,jk) + zsc_uw_1(ji,jj)
               ghamv(ji,jj,jk) = ghamv(ji,jj,jk) + zsc_vw_1(ji,jj)
            END IF
         ENDIF
      END_3D
      !
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         IF ( l_conv(ji,jj) .AND. l_pyc(ji,jj) ) THEN
            IF ( n_ddh(ji,jj) == 0 ) THEN
               ! Place holding code. Parametrization needs checking for these conditions.
               zomega = ( 0.15_wp * swstrl(ji,jj)**3 + swstrc(ji,jj)**3 + 4.75_wp * ( pshear(ji,jj) * phbl(ji,jj) ) )**pthird
               zuw_bse(ji,jj) = -0.0035_wp * zomega * ( 1.0_wp - pdh(ji,jj) / phbl(ji,jj) ) * av_du_ml(ji,jj)
               zvw_bse(ji,jj) = -0.0075_wp * zomega * ( 1.0_wp - pdh(ji,jj) / phbl(ji,jj) ) * av_dv_ml(ji,jj)
            ELSE
               zomega = ( 0.15_wp * swstrl(ji,jj)**3 + swstrc(ji,jj)**3 + 4.75_wp * ( pshear(ji,jj) * phbl(ji,jj) ) )**pthird
               zuw_bse(ji,jj) = -0.0035_wp * zomega * ( 1.0_wp - pdh(ji,jj) / phbl(ji,jj) ) * av_du_ml(ji,jj)
               zvw_bse(ji,jj) = -0.0075_wp * zomega * ( 1.0_wp - pdh(ji,jj) / phbl(ji,jj) ) * av_dv_ml(ji,jj)
            ENDIF
            zb_cubic(ji,jj) = pdh(ji,jj) / phbl(ji,jj) * suw0(ji,jj) - ( 2.0_wp + pdh(ji,jj) / phml(ji,jj) ) * zuw_bse(ji,jj)
            za_cubic(ji,jj) = zuw_bse(ji,jj) - zb_cubic(ji,jj)
            zvw_max = 0.7_wp * ff_t(ji,jj) * ( sustke(ji,jj) * dstokes(ji,jj) + 0.7_wp * sustar(ji,jj) * phml(ji,jj) )
            zd_cubic(ji,jj) = zvw_max * pdh(ji,jj) / phml(ji,jj) - ( 2.0_wp + pdh(ji,jj) / phml(ji,jj) ) * zvw_bse(ji,jj)
            zc_cubic(ji,jj) = zvw_bse(ji,jj) - zd_cubic(ji,jj)
         END IF
      END_2D
      DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, jkf_mld, jkm_bld )   ! Need ztau_sc_u to be available. Change to array.
         IF ( l_conv(ji,jj) .AND. l_pyc(ji,jj) .AND. ( jk >= nmld(ji,jj) ) .AND. ( jk <= nbld(ji,jj) ) ) THEN
            zznd_pyc = -1.0_wp * ( gdepw(ji,jj,jk,Kmm) - phbl(ji,jj) ) / pdh(ji,jj)
            ghamu(ji,jj,jk) = ghamu(ji,jj,jk) - 0.045_wp * ( ztau_sc_u(ji,jj)**2 ) * zuw_bse(ji,jj) *                 &
               &                                ( za_cubic(ji,jj) * zznd_pyc**2 + zb_cubic(ji,jj) * zznd_pyc**3 ) *   &
               &                                ( 0.75_wp + 0.25_wp * zznd_pyc )**2 * zdbdz_pyc(ji,jj,jk)
            ghamv(ji,jj,jk) = ghamv(ji,jj,jk) - 0.045_wp * ( ztau_sc_u(ji,jj)**2 ) * zvw_bse(ji,jj) *                 &
               &                                ( zc_cubic(ji,jj) * zznd_pyc**2 + zd_cubic(ji,jj) * zznd_pyc**3 ) *   &
               &                                ( 0.75_wp + 0.25_wp * zznd_pyc )**2 * zdbdz_pyc(ji,jj,jk)
         END IF   ! l_conv .AND. l_pyc
      END_3D
      !
      IF ( ln_dia_osm ) THEN
         CALL zdf_osm_iomput( "ghamu_0",    wmask(A2D(0),:) * ghamu(A2D(0),:)  )
         CALL zdf_osm_iomput( "zsc_uw_1_0", tmask(A2D(0),1) * zsc_uw_1(A2D(0)) )
      END IF
      !
      ! Transport term in flux-gradient relationship [note : includes ROI ratio
      ! (X0.3) ]
      ! -----------------------------------------------------------------------
      WHERE ( l_conv(A2D(nn_hls-1)) )
         zsc_wth_1(:,:) = swth0(A2D(nn_hls-1)) / ( 1.0_wp - 0.56_wp * EXP( shol(A2D(nn_hls-1)) ) )
         zsc_ws_1(:,:)  = sws0(A2D(nn_hls-1))  / ( 1.0_wp - 0.56_wp * EXP( shol(A2D(nn_hls-1)) ) )
         WHERE ( l_pyc(A2D(nn_hls-1)) )   ! Pycnocline scales
            zsc_wth_pyc(:,:) = -0.003_wp * swstrc(A2D(nn_hls-1)) * ( 1.0_wp - pdh(A2D(nn_hls-1)) / phbl(A2D(nn_hls-1)) ) *   &
               &               av_dt_ml(A2D(nn_hls-1))
            zsc_ws_pyc(:,:)  = -0.003_wp * swstrc(A2D(nn_hls-1)) * ( 1.0_wp - pdh(A2D(nn_hls-1)) / phbl(A2D(nn_hls-1)) ) *   &
               &               av_ds_ml(A2D(nn_hls-1))
         END WHERE
      ELSEWHERE
         zsc_wth_1(:,:) = 2.0_wp * swthav(A2D(nn_hls-1))
         zsc_ws_1(:,:)  =          sws0(A2D(nn_hls-1))
      END WHERE
      DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 1, MAX( jkm_mld, jkm_bld ) )
         IF ( l_conv(ji,jj) ) THEN
            IF ( ( jk > 1 ) .AND. ( jk <= nmld(ji,jj) ) ) THEN
               zznd_ml = gdepw(ji,jj,jk,Kmm) / phml(ji,jj)
               ghamt(ji,jj,jk) = ghamt(ji,jj,jk) + 0.3_wp * zsc_wth_1(ji,jj) *                                  &
                  &                                ( -2.0_wp + 2.75_wp * ( ( 1.0_wp + 0.6_wp * zznd_ml**4 ) -   &
                  &                                                        EXP( -6.0_wp * zznd_ml ) ) ) *       &
                  &                                ( 1.0_wp - EXP( -15.0_wp * ( 1.0_wp - zznd_ml ) ) )
               ghams(ji,jj,jk) = ghams(ji,jj,jk) + 0.3_wp * zsc_ws_1(ji,jj) *                                   &
                  &                                ( -2.0_wp + 2.75_wp * ( ( 1.0_wp + 0.6_wp * zznd_ml**4 ) -   &
                  &                                EXP( -6.0_wp * zznd_ml ) ) ) * ( 1.0_wp - EXP( -15.0_wp * ( 1.0_wp - zznd_ml ) ) )
            END IF
            !
            ! may need to comment out lpyc block
            IF ( l_pyc(ji,jj) .AND. ( jk >= nmld(ji,jj) ) .AND. ( jk <= nbld(ji,jj) ) ) THEN   ! Pycnocline
               zznd_pyc = -1.0_wp * ( gdepw(ji,jj,jk,Kmm) - phbl(ji,jj) ) / pdh(ji,jj)
               ghamt(ji,jj,jk) = ghamt(ji,jj,jk) + 4.0_wp * zsc_wth_pyc(ji,jj) *   &
                  &                                ( 0.48_wp - EXP( -1.5_wp * ( zznd_pyc - 0.3_wp )**2 ) )
               ghams(ji,jj,jk) = ghams(ji,jj,jk) + 4.0_wp * zsc_ws_pyc(ji,jj)  *   &
                  &                                ( 0.48_wp - EXP( -1.5_wp * ( zznd_pyc - 0.3_wp )**2 ) )
            END IF
         ELSE
            IF( pdhdt(ji,jj) > 0. ) THEN
               IF ( ( jk > 1 ) .AND. ( jk <= nbld(ji,jj) ) ) THEN
                  zznd_d = gdepw(ji,jj,jk,Kmm) / dstokes(ji,jj)
                  znd    = gdepw(ji,jj,jk,Kmm) / phbl(ji,jj)
                  ghamt(ji,jj,jk) = ghamt(ji,jj,jk) + 0.3_wp * ( -4.06_wp * EXP( -2.0_wp * zznd_d ) * ( 1.0_wp - EXP( -4.0_wp * zznd_d ) ) +   &
                     7.5_wp * EXP ( -10.0_wp * ( 0.95_wp - znd )**2 ) * ( 1.0_wp - znd ) ) * zsc_wth_1(ji,jj)
                  ghams(ji,jj,jk) = ghams(ji,jj,jk) + 0.3_wp * ( -4.06_wp * EXP( -2.0_wp * zznd_d ) * ( 1.0_wp - EXP( -4.0_wp * zznd_d ) ) +   &
                     7.5_wp * EXP ( -10.0_wp * ( 0.95_wp - znd )**2 ) * ( 1.0_wp - znd ) ) * zsc_ws_1(ji,jj)
               END IF
            ENDIF
         ENDIF
      END_3D
      !
      WHERE ( l_conv(A2D(nn_hls-1)) )
         zsc_uw_1(:,:) = sustar(A2D(nn_hls-1))**2
         zsc_vw_1(:,:) = ff_t(A2D(nn_hls-1)) * sustke(A2D(nn_hls-1)) * phml(A2D(nn_hls-1))
      ELSEWHERE
         zsc_uw_1(:,:) = sustar(A2D(nn_hls-1))**2
         zsc_uw_2(:,:) = ( 2.25_wp - 3.0_wp * ( 1.0_wp - EXP( -1.25_wp * 2.0_wp ) ) ) * ( 1.0_wp - EXP( -4.0_wp * 2.0_wp ) ) *   &
            &            zsc_uw_1(:,:)
         zsc_vw_1(:,:) = ff_t(A2D(nn_hls-1)) * sustke(A2D(nn_hls-1)) * phbl(A2D(nn_hls-1))
         zsc_vw_2(:,:) = -0.11_wp * SIN( 3.14159_wp * ( 2.0_wp + 0.4_wp ) ) * EXP( -1.0_wp * ( 1.5_wp + 2.0_wp )**2 ) *   &
            &            zsc_vw_1(:,:)
      ENDWHERE
      DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 2, MAX( jkm_mld, jkm_bld ) )
         IF ( l_conv(ji,jj) ) THEN
            IF ( jk <= nmld(ji,jj) ) THEN
               zznd_ml = gdepw(ji,jj,jk,Kmm) / phml(ji,jj)
               zznd_d  = gdepw(ji,jj,jk,Kmm) / dstokes(ji,jj)
               ghamu(ji,jj,jk) = ghamu(ji,jj,jk) +   &
                  &              0.3_wp * ( -2.0_wp + 2.5_wp * ( 1.0_wp + 0.1_wp * zznd_ml**4 ) - EXP( -8.0_wp * zznd_ml ) ) *   &
                  &              zsc_uw_1(ji,jj)
               ghamv(ji,jj,jk) = ghamv(ji,jj,jk) +   &
                  &              0.3_wp * 0.1_wp * ( EXP( -1.0_wp * zznd_d ) + EXP( -5.0_wp * ( 1.0_wp - zznd_ml ) ) ) *   &
                  &              zsc_vw_1(ji,jj)
            END IF
         ELSE
            IF ( jk <= nbld(ji,jj) ) THEN
               znd    = gdepw(ji,jj,jk,Kmm) / phbl(ji,jj)
               zznd_d = gdepw(ji,jj,jk,Kmm) / dstokes(ji,jj)
               IF ( zznd_d <= 2.0_wp ) THEN
                  ghamu(ji,jj,jk) = ghamu(ji,jj,jk) + 0.5_wp * 0.3_wp *                                              &
                     &                                ( 2.25_wp - 3.0_wp * ( 1.0_wp - EXP( -1.25_wp * zznd_d ) ) *   &
                     &                                  ( 1.0_wp - EXP( -2.0_wp * zznd_d ) ) ) * zsc_uw_1(ji,jj)
               ELSE
                  ghamu(ji,jj,jk) = ghamu(ji,jj,jk) + 0.5_wp * 0.3_wp *   &
                     &                                ( 1.0_wp - EXP( -5.0_wp * ( 1.0_wp - znd ) ) ) * zsc_uw_2(ji,jj)
               ENDIF
               ghamv(ji,jj,jk) = ghamv(ji,jj,jk) + 0.3_wp * 0.15_wp * SIN( 3.14159_wp * ( 0.65_wp * zznd_d ) ) *   &
                  &                                EXP( -0.25_wp * zznd_d**2 ) * zsc_vw_1(ji,jj)
               ghamv(ji,jj,jk) = ghamv(ji,jj,jk) + 0.3_wp * 0.15_wp * EXP( -5.0 * ( 1.0 - znd ) ) *   &
                  &                                ( 1.0 - EXP( -20.0 * ( 1.0 - znd ) ) ) * zsc_vw_2(ji,jj)
            END IF
         END IF
      END_3D
      !
      IF ( ln_dia_osm ) THEN
         CALL zdf_osm_iomput( "ghamu_f",    wmask(A2D(0),:) * ghamu(A2D(0),:)  )
         CALL zdf_osm_iomput( "ghamv_f",    wmask(A2D(0),:) * ghamv(A2D(0),:)  )
         CALL zdf_osm_iomput( "zsc_uw_1_f", tmask(A2D(0),1) * zsc_uw_1(A2D(0)) )
         CALL zdf_osm_iomput( "zsc_vw_1_f", tmask(A2D(0),1) * zsc_vw_1(A2D(0)) )
         CALL zdf_osm_iomput( "zsc_uw_2_f", tmask(A2D(0),1) * zsc_uw_2(A2D(0)) )
         CALL zdf_osm_iomput( "zsc_vw_2_f", tmask(A2D(0),1) * zsc_vw_2(A2D(0)) )
      END IF
      !
      ! Make surface forced velocity non-gradient terms go to zero at the base
      ! of the mixed layer.
      !
      ! Make surface forced velocity non-gradient terms go to zero at the base
      ! of the boundary layer.
      DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 2, jkm_bld )
         IF ( ( .NOT. l_conv(ji,jj) ) .AND. ( jk <= nbld(ji,jj) ) ) THEN
            znd = -1.0_wp * ( gdepw(ji,jj,jk,Kmm) - phbl(ji,jj) ) / phbl(ji,jj)   ! ALMG to think about
            IF ( znd >= 0.0_wp ) THEN
               ghamu(ji,jj,jk) = ghamu(ji,jj,jk) * ( 1.0_wp - EXP( -10.0_wp * znd**2 ) )
               ghamv(ji,jj,jk) = ghamv(ji,jj,jk) * ( 1.0_wp - EXP( -10.0_wp * znd**2 ) )
            ELSE
               ghamu(ji,jj,jk) = 0.0_wp
               ghamv(ji,jj,jk) = 0.0_wp
            ENDIF
         END IF
      END_3D
      !
      ! Pynocline contributions
      !
      IF ( ln_dia_pyc_scl .OR. ln_dia_pyc_shr ) THEN   ! Allocate arrays for output of pycnocline gradient/shear profiles
         ALLOCATE( z3ddz_pyc_1(A2D(nn_hls),jpk), z3ddz_pyc_2(A2D(nn_hls),jpk), STAT=istat )
         IF ( istat /= 0 ) CALL ctl_stop( 'zdf_osm: failed to allocate temporary arrays' )
         z3ddz_pyc_1(:,:,:) = 0.0_wp
         z3ddz_pyc_2(:,:,:) = 0.0_wp
      END IF
      DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 2, jkm_bld )
         IF ( l_conv (ji,jj) ) THEN
            ! Unstable conditions. Shouldn;t be needed with no pycnocline code.
            !                  zugrad = 0.7 * av_du_ml(ji,jj) / zdh(ji,jj) + 0.3 * zustar(ji,jj)*zustar(ji,jj) / &
            !                       &      ( ( ( zvstr(ji,jj)**3 + 0.5 * zwstrc(ji,jj)**3 )**pthird * zhml(ji,jj) ) * &
            !                      &      MIN(zla(ji,jj)**(8.0/3.0) + epsln, 0.12 ))
            !Alan is this right?
            !                  zvgrad = ( 0.7 * av_dv_ml(ji,jj) + &
            !                       &    2.0 * ff_t(ji,jj) * zustke(ji,jj) * dstokes(ji,jj) / &
            !                       &          ( ( zvstr(ji,jj)**3 + 0.5 * zwstrc(ji,jj)**3 )**pthird  + epsln ) &
            !                       &      )/ (zdh(ji,jj)  + epsln )
            !                  DO jk = 2, nbld(ji,jj) - 1 + ibld_ext
            !                     znd = -( gdepw(ji,jj,jk,Kmm) - zhbl(ji,jj) ) / (zdh(ji,jj) + epsln ) - zzeta_v
            !                     IF ( znd <= 0.0 ) THEN
            !                        zdudz(ji,jj,jk) = 1.25 * zugrad * EXP( 3.0 * znd )
            !                        zdvdz(ji,jj,jk) = 1.25 * zvgrad * EXP( 3.0 * znd )
            !                     ELSE
            !                        zdudz(ji,jj,jk) = 1.25 * zugrad * EXP( -2.0 * znd )
            !                        zdvdz(ji,jj,jk) = 1.25 * zvgrad * EXP( -2.0 * znd )
            !                     ENDIF
            !                  END DO
         ELSE   ! Stable conditions
            IF ( nbld(ji,jj) + kp_ext(ji,jj) < mbkt(ji,jj) ) THEN
               ! Pycnocline profile only defined when depth steady of increasing.
               IF ( pdhdt(ji,jj) > 0.0_wp ) THEN   ! Depth increasing, or steady.
                  IF ( av_db_bl(ji,jj) > 0.0_wp ) THEN
                     IF ( shol(ji,jj) >= 0.5_wp ) THEN   ! Very stable - 'thick' pycnocline
                        ztmp = 1.0_wp / MAX( phbl(ji,jj), epsln )
                        ztgrad = av_dt_bl(ji,jj) * ztmp
                        zsgrad = av_ds_bl(ji,jj) * ztmp
                        zbgrad = av_db_bl(ji,jj) * ztmp
                        IF ( jk <= nbld(ji,jj) ) THEN
                           znd = gdepw(ji,jj,jk,Kmm) * ztmp
                           zdtdz_pyc =  ztgrad * EXP( -15.0_wp * ( znd - 0.9_wp )**2 )
                           zdsdz_pyc =  zsgrad * EXP( -15.0_wp * ( znd - 0.9_wp )**2 )
                           ghamt(ji,jj,jk) = ghamt(ji,jj,jk) + pdiffut(ji,jj,jk) * zdtdz_pyc
                           ghams(ji,jj,jk) = ghams(ji,jj,jk) + pdiffut(ji,jj,jk) * zdsdz_pyc
                           IF ( ln_dia_pyc_scl ) THEN
                              z3ddz_pyc_1(ji,jj,jk) = zdtdz_pyc
                              z3ddz_pyc_2(ji,jj,jk) = zdsdz_pyc
                           END IF
                        END IF
                     ELSE   ! Slightly stable - 'thin' pycnoline - needed when stable layer begins to form.
                        ztmp = 1.0_wp / MAX( pdh(ji,jj), epsln )
                        ztgrad = av_dt_bl(ji,jj) * ztmp
                        zsgrad = av_ds_bl(ji,jj) * ztmp
                        zbgrad = av_db_bl(ji,jj) * ztmp
                        IF ( jk <= nbld(ji,jj) ) THEN
                           znd = -1.0_wp * ( gdepw(ji,jj,jk,Kmm) - phml(ji,jj) ) * ztmp
                           zdtdz_pyc =  ztgrad * EXP( -1.75_wp * ( znd + 0.75_wp )**2 )
                           zdsdz_pyc =  zsgrad * EXP( -1.75_wp * ( znd + 0.75_wp )**2 )
                           ghamt(ji,jj,jk) = ghamt(ji,jj,jk) + pdiffut(ji,jj,jk) * zdtdz_pyc
                           ghams(ji,jj,jk) = ghams(ji,jj,jk) + pdiffut(ji,jj,jk) * zdsdz_pyc
                           IF ( ln_dia_pyc_scl ) THEN
                              z3ddz_pyc_1(ji,jj,jk) = zdtdz_pyc
                              z3ddz_pyc_2(ji,jj,jk) = zdsdz_pyc
                           END IF
                        END IF
                     ENDIF   ! IF (shol >=0.5)
                  ENDIF      ! IF (av_db_bl> 0.)
               ENDIF         ! IF (zdhdt >= 0) zdhdt < 0 not considered since pycnocline profile is zero and profile arrays are
               !             !    intialized to zero
            END IF
         END IF
      END_3D
      IF ( ln_dia_pyc_scl ) THEN   ! Output of pycnocline gradient profiles
         CALL zdf_osm_iomput( "zdtdz_pyc", wmask(A2D(0),:) * z3ddz_pyc_1(A2D(0),:) )
         CALL zdf_osm_iomput( "zdsdz_pyc", wmask(A2D(0),:) * z3ddz_pyc_2(A2D(0),:) )
      END IF
      DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 2, jkm_bld )
         IF ( .NOT. l_conv (ji,jj) ) THEN
            IF ( nbld(ji,jj) + kp_ext(ji,jj) < mbkt(ji,jj) ) THEN
               zugrad = 3.25_wp * av_du_bl(ji,jj) / phbl(ji,jj)
               zvgrad = 2.75_wp * av_dv_bl(ji,jj) / phbl(ji,jj)
               IF ( jk <= nbld(ji,jj) ) THEN
                  znd = gdepw(ji,jj,jk,Kmm) / phbl(ji,jj)
                  IF ( znd < 1.0 ) THEN
                     zdudz_pyc = zugrad * EXP( -40.0_wp * ( znd - 1.0_wp )**2 )
                  ELSE
                     zdudz_pyc = zugrad * EXP( -20.0_wp * ( znd - 1.0_wp )**2 )
                  ENDIF
                  zdvdz_pyc = zvgrad * EXP( -20.0_wp * ( znd - 0.85_wp )**2 )
                  ghamu(ji,jj,jk) = ghamu(ji,jj,jk) + pviscos(ji,jj,jk) * zdudz_pyc
                  ghamv(ji,jj,jk) = ghamv(ji,jj,jk) + pviscos(ji,jj,jk) * zdvdz_pyc
                  IF ( ln_dia_pyc_shr ) THEN
                     z3ddz_pyc_1(ji,jj,jk) = zdudz_pyc
                     z3ddz_pyc_2(ji,jj,jk) = zdvdz_pyc
                  END IF
               END IF
            END IF
         END IF
      END_3D
      IF ( ln_dia_pyc_shr ) THEN   ! Output of pycnocline shear profiles
         CALL zdf_osm_iomput( "zdudz_pyc", wmask(A2D(0),:) * z3ddz_pyc_1(A2D(0),:) )
         CALL zdf_osm_iomput( "zdvdz_pyc", wmask(A2D(0),:) * z3ddz_pyc_2(A2D(0),:) )
      END IF
      IF ( ln_dia_osm ) THEN
         CALL zdf_osm_iomput( "ghamu_b", wmask(A2D(0),:) * ghamu(A2D(0),:) )
         CALL zdf_osm_iomput( "ghamv_b", wmask(A2D(0),:) * ghamv(A2D(0),:) )
      END IF
      IF ( ln_dia_pyc_scl .OR. ln_dia_pyc_shr ) THEN   ! Deallocate arrays used for output of pycnocline gradient/shear profiles
         DEALLOCATE( z3ddz_pyc_1, z3ddz_pyc_2 )
      END IF
      !
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         ghamt(ji,jj,nbld(ji,jj)) = 0.0_wp
         ghams(ji,jj,nbld(ji,jj)) = 0.0_wp
         ghamu(ji,jj,nbld(ji,jj)) = 0.0_wp
         ghamv(ji,jj,nbld(ji,jj)) = 0.0_wp
      END_2D
      !
      IF ( ln_dia_osm ) THEN
         CALL zdf_osm_iomput( "ghamu_1", wmask(A2D(0),:) * ghamu(A2D(0),:)   )
         CALL zdf_osm_iomput( "ghamv_1", wmask(A2D(0),:) * ghamv(A2D(0),:)   )
         CALL zdf_osm_iomput( "zviscos", wmask(A2D(0),:) * pviscos(A2D(0),:) )
      END IF
      !
   END SUBROUTINE zdf_osm_fgr_terms

   SUBROUTINE zdf_osm_zmld_horizontal_gradients( Kmm, pmld, pdtdx, pdtdy, pdsdx,   &
      &                                          pdsdy, pdbds_mle )
      !!----------------------------------------------------------------------
      !!          ***  ROUTINE zdf_osm_zmld_horizontal_gradients  ***
      !!
      !! ** Purpose : Calculates horizontal gradients of buoyancy for use with
      !!              Fox-Kemper parametrization
      !!
      !! ** Method  :
      !!
      !! References: Fox-Kemper et al., JPO, 38, 1145-1165, 2008
      !!             Fox-Kemper and Ferrari, JPO, 38, 1166-1179, 2008
      !!
      !!----------------------------------------------------------------------
      INTEGER,                            INTENT(in   ) ::   Kmm          ! Time-level index
      REAL(wp), DIMENSION(A2D(nn_hls)),   INTENT(  out) ::   pmld         ! == Estimated FK BLD used for MLE horizontal gradients == !
      REAL(wp), DIMENSION(A2D(nn_hls)),   INTENT(inout) ::   pdtdx        ! Horizontal gradient for Fox-Kemper parametrization
      REAL(wp), DIMENSION(A2D(nn_hls)),   INTENT(inout) ::   pdtdy        ! Horizontal gradient for Fox-Kemper parametrization
      REAL(wp), DIMENSION(A2D(nn_hls)),   INTENT(inout) ::   pdsdx        ! Horizontal gradient for Fox-Kemper parametrization
      REAL(wp), DIMENSION(A2D(nn_hls)),   INTENT(inout) ::   pdsdy        ! Horizontal gradient for Fox-Kemper parametrization
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(inout) ::   pdbds_mle    ! Magnitude of horizontal buoyancy gradient
      !!
      INTEGER                               ::   ji, jj, jk   ! Dummy loop indices
      INTEGER,  DIMENSION(A2D(nn_hls))      ::   jk_mld_prof  ! Base level of MLE layer
      INTEGER                               ::   ikt, ikmax   ! Local integers      
      REAL(wp)                              ::   zc
      REAL(wp)                              ::   zN2_c        ! Local buoyancy difference from 10m value
      REAL(wp), DIMENSION(A2D(nn_hls))      ::   ztm
      REAL(wp), DIMENSION(A2D(nn_hls))      ::   zsm
      REAL(wp), DIMENSION(A2D(nn_hls),jpts) ::   ztsm_midu
      REAL(wp), DIMENSION(A2D(nn_hls),jpts) ::   ztsm_midv
      REAL(wp), DIMENSION(A2D(nn_hls),jpts) ::   zabu
      REAL(wp), DIMENSION(A2D(nn_hls),jpts) ::   zabv
      REAL(wp), DIMENSION(A2D(nn_hls))      ::   zmld_midu
      REAL(wp), DIMENSION(A2D(nn_hls))      ::   zmld_midv
      !!----------------------------------------------------------------------
      !
      ! ==  MLD used for MLE  ==!
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         jk_mld_prof(ji,jj) = nlb10    ! Initialization to the number of w ocean point
         pmld(ji,jj)        = 0.0_wp   ! Here hmlp used as a dummy variable, integrating vertically N^2
      END_2D
      zN2_c = grav * rn_osm_mle_rho_c * r1_rho0   ! Convert density criteria into N^2 criteria
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, nlb10, jpkm1 )
         ikt = mbkt(ji,jj)
         pmld(ji,jj) = pmld(ji,jj) + MAX( rn2b(ji,jj,jk), 0.0_wp ) * e3w(ji,jj,jk,Kmm)
         IF( pmld(ji,jj) < zN2_c ) jk_mld_prof(ji,jj) = MIN( jk , ikt ) + 1   ! Mixed layer level
      END_3D
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         jk_mld_prof(ji,jj) = MAX( jk_mld_prof(ji,jj), nbld(ji,jj) )   ! Ensure jk_mld_prof .ge. nbld
         pmld(ji,jj)     = gdepw(ji,jj,jk_mld_prof(ji,jj),Kmm)
      END_2D
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         mld_prof(ji,jj) = jk_mld_prof(ji,jj)
      END_2D
      !
      ikmax = MIN( MAXVAL( jk_mld_prof(A2D(nn_hls)) ), jpkm1 )   ! Max level of the computation
      ztm(:,:) = 0.0_wp
      zsm(:,:) = 0.0_wp
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, ikmax )
         zc = e3t(ji,jj,jk,Kmm) * REAL( MIN( MAX( 0, jk_mld_prof(ji,jj) - jk ), 1  ), KIND=wp )   ! zc being 0 outside the ML
         !                                                                                        !    t-points
         ztm(ji,jj) = ztm(ji,jj) + zc * ts(ji,jj,jk,jp_tem,Kmm)
         zsm(ji,jj) = zsm(ji,jj) + zc * ts(ji,jj,jk,jp_sal,Kmm)
      END_3D
      ! Average temperature and salinity
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         ztm(ji,jj) = ztm(ji,jj) / MAX( e3t(ji,jj,1,Kmm), pmld(ji,jj) )
         zsm(ji,jj) = zsm(ji,jj) / MAX( e3t(ji,jj,1,Kmm), pmld(ji,jj) )
      END_2D
      ! Calculate horizontal gradients at u & v points
      zmld_midu(:,:)   =  0.0_wp
      ztsm_midu(:,:,:) = 10.0_wp
      DO_2D( nn_hls, nn_hls-1, nn_hls-1, nn_hls-1 )
         pdtdx(ji,jj)            = ( ztm(ji+1,jj) - ztm(ji,jj) )  * umask(ji,jj,1) / e1u(ji,jj)
         pdsdx(ji,jj)            = ( zsm(ji+1,jj) - zsm(ji,jj) )  * umask(ji,jj,1) / e1u(ji,jj)
         zmld_midu(ji,jj)        = 0.25_wp * ( pmld(ji+1,jj) + pmld(ji,jj))
         ztsm_midu(ji,jj,jp_tem) =  0.5_wp * ( ztm( ji+1,jj)  + ztm( ji,jj) )
         ztsm_midu(ji,jj,jp_sal) =  0.5_wp * ( zsm( ji+1,jj)  + zsm( ji,jj) )
      END_2D
      zmld_midv(:,:)   =  0.0_wp
      ztsm_midv(:,:,:) = 10.0_wp
      DO_2D( nn_hls-1, nn_hls-1, nn_hls, nn_hls-1 )
         pdtdy(ji,jj)            = ( ztm(ji,jj+1) - ztm(ji,jj) ) * vmask(ji,jj,1) / e1v(ji,jj)
         pdsdy(ji,jj)            = ( zsm(ji,jj+1) - zsm(ji,jj) ) * vmask(ji,jj,1) / e1v(ji,jj)
         zmld_midv(ji,jj)        = 0.25_wp * ( pmld(ji,jj+1) + pmld( ji,jj) )
         ztsm_midv(ji,jj,jp_tem) =  0.5_wp * ( ztm( ji,jj+1)  + ztm( ji,jj) )
         ztsm_midv(ji,jj,jp_sal) =  0.5_wp * ( zsm( ji,jj+1)  + zsm( ji,jj) )
      END_2D
      CALL eos_rab( ztsm_midu, zmld_midu, zabu, Kmm )
      CALL eos_rab( ztsm_midv, zmld_midv, zabv, Kmm )
      DO_2D_OVR( nn_hls, nn_hls-1, nn_hls-1, nn_hls-1 )
         dbdx_mle(ji,jj) = grav * ( pdtdx(ji,jj) * zabu(ji,jj,jp_tem) - pdsdx(ji,jj) * zabu(ji,jj,jp_sal) )
      END_2D
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls, nn_hls-1 )
         dbdy_mle(ji,jj) = grav * ( pdtdy(ji,jj) * zabv(ji,jj,jp_tem) - pdsdy(ji,jj) * zabv(ji,jj,jp_sal) )
      END_2D
      DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         pdbds_mle(ji,jj) = SQRT( 0.5_wp * ( dbdx_mle(ji,  jj) * dbdx_mle(ji,  jj) + dbdy_mle(ji,jj  ) * dbdy_mle(ji,jj  ) +   &
            &                                dbdx_mle(ji-1,jj) * dbdx_mle(ji-1,jj) + dbdy_mle(ji,jj-1) * dbdy_mle(ji,jj-1) ) )
      END_2D
      !
   END SUBROUTINE zdf_osm_zmld_horizontal_gradients

   SUBROUTINE zdf_osm_osbl_state_fk( Kmm, pwb_fk, phbl, phmle, pwb_ent,   &
      &                              pdbds_mle )
      !!---------------------------------------------------------------------
      !!               ***  ROUTINE zdf_osm_osbl_state_fk  ***
      !!
      !! ** Purpose : Determines the state of the OSBL and MLE layer. Info is
      !!              returned in the logicals l_pyc, l_flux and ldmle. Used
      !!              with Fox-Kemper scheme.
      !!                l_pyc  :: determines whether pycnocline flux-grad
      !!                          relationship needs to be determined
      !!                l_flux :: determines whether effects of surface flux
      !!                          extend below the base of the OSBL
      !!                ldmle  :: determines whether the layer with MLE is
      !!                          increasing with time or if base is relaxing
      !!                          towards hbl
      !!
      !! ** Method  :
      !!
      !!----------------------------------------------------------------------      
      INTEGER,                            INTENT(in   ) ::   Kmm         ! Time-level index
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(inout) ::   pwb_fk
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   phbl        ! BL depth
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   phmle       ! MLE depth
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   pwb_ent     ! Buoyancy entrainment flux
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   pdbds_mle   ! Magnitude of horizontal buoyancy gradient
      !!
      INTEGER                            ::   ji, jj, jk        ! Dummy loop indices
      REAL(wp), DIMENSION(A2D(nn_hls-1)) ::   znd_param
      REAL(wp)                           ::   zthermal, zbeta
      REAL(wp)                           ::   zbuoy
      REAL(wp)                           ::   ztmp
      REAL(wp)                           ::   zpe_mle_layer
      REAL(wp)                           ::   zpe_mle_ref
      REAL(wp)                           ::   zdbdz_mle_int
      !!----------------------------------------------------------------------      
      !
      znd_param(:,:) = 0.0_wp
      !
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         ztmp =  r1_ft(ji,jj) *  MIN( 111.e3_wp , e1u(ji,jj) ) / rn_osm_mle_lf
         pwb_fk(ji,jj) = rn_osm_mle_ce * hmle(ji,jj) * hmle(ji,jj) * ztmp * pdbds_mle(ji,jj) * pdbds_mle(ji,jj)
      END_2D
      !
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         !
         IF ( l_conv(ji,jj) ) THEN
            IF ( phmle(ji,jj) > 1.2_wp * phbl(ji,jj) ) THEN
               av_t_mle(ji,jj) = ( av_t_mle(ji,jj) * phmle(ji,jj) - av_t_bl(ji,jj) * phbl(ji,jj) ) / ( phmle(ji,jj) - phbl(ji,jj) )
               av_s_mle(ji,jj) = ( av_s_mle(ji,jj) * phmle(ji,jj) - av_s_bl(ji,jj) * phbl(ji,jj) ) / ( phmle(ji,jj) - phbl(ji,jj) )
               av_b_mle(ji,jj) = ( av_b_mle(ji,jj) * phmle(ji,jj) - av_b_bl(ji,jj) * phbl(ji,jj) ) / ( phmle(ji,jj) - phbl(ji,jj) )
               zdbdz_mle_int = ( av_b_bl(ji,jj) - ( 2.0_wp * av_b_mle(ji,jj) - av_b_bl(ji,jj) ) ) / ( phmle(ji,jj) - phbl(ji,jj) )
               ! Calculate potential energies of actual profile and reference profile
               zpe_mle_layer = 0.0_wp
               zpe_mle_ref   = 0.0_wp
               zthermal = rab_n(ji,jj,1,jp_tem)
               zbeta    = rab_n(ji,jj,1,jp_sal)
               DO jk = nbld(ji,jj), mld_prof(ji,jj)
                  zbuoy         = grav * ( zthermal * ts(ji,jj,jk,jp_tem,Kmm) - zbeta * ts(ji,jj,jk,jp_sal,Kmm) )
                  zpe_mle_layer = zpe_mle_layer + zbuoy * gdepw(ji,jj,jk,Kmm) * e3w(ji,jj,jk,Kmm)
                  zpe_mle_ref   = zpe_mle_ref   + ( av_b_bl(ji,jj) - zdbdz_mle_int * ( gdepw(ji,jj,jk,Kmm) - phbl(ji,jj) ) ) *   &
                     &                            gdepw(ji,jj,jk,Kmm) * e3w(ji,jj,jk,Kmm)
               END DO
               ! Non-dimensional parameter to diagnose the presence of thermocline
               znd_param(ji,jj) = ( zpe_mle_layer - zpe_mle_ref ) * ABS( ff_t(ji,jj) ) /   &
                  &               ( MAX( pwb_fk(ji,jj), 1e-10 ) * phmle(ji,jj) )
            END IF
         END IF
         !
      END_2D
      !
      ! Diagnosis
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         !
         IF ( l_conv(ji,jj) ) THEN
            IF ( -2.0_wp * pwb_fk(ji,jj) / pwb_ent(ji,jj) > 0.5_wp ) THEN
               IF ( phmle(ji,jj) > 1.2_wp * phbl(ji,jj) ) THEN   ! MLE layer growing
                  IF ( znd_param (ji,jj) > 100.0_wp ) THEN   ! Thermocline present
                     l_flux(ji,jj) = .FALSE.
                     l_mle(ji,jj)  = .FALSE.
                  ELSE   ! Thermocline not present
                     l_flux(ji,jj) = .TRUE.
                     l_mle(ji,jj)  = .TRUE.
                  ENDIF  ! znd_param > 100
                  !
                  IF ( av_db_bl(ji,jj) < rn_osm_bl_thresh ) THEN
                     l_pyc(ji,jj) = .FALSE.
                  ELSE
                     l_pyc(ji,jj) = .TRUE.
                  ENDIF
               ELSE   ! MLE layer restricted to OSBL or just below
                  IF ( av_db_bl(ji,jj) < rn_osm_bl_thresh ) THEN   ! Weak stratification MLE layer can grow
                     l_pyc(ji,jj)  = .FALSE.
                     l_flux(ji,jj) = .TRUE.
                     l_mle(ji,jj)  = .TRUE.
                  ELSE   ! Strong stratification
                     l_pyc(ji,jj)  = .TRUE.
                     l_flux(ji,jj) = .FALSE.
                     l_mle(ji,jj)  = .FALSE.
                  END IF   ! av_db_bl < rn_mle_thresh_bl and
               END IF   ! phmle > 1.2 phbl
            ELSE
               l_pyc(ji,jj)  = .TRUE.
               l_flux(ji,jj) = .FALSE.
               l_mle(ji,jj)  = .FALSE.
               IF ( av_db_bl(ji,jj) < rn_osm_bl_thresh ) l_pyc(ji,jj) = .FALSE.
            END IF   !  -2.0 * pwb_fk(ji,jj) / pwb_ent > 0.5
         ELSE   ! Stable Boundary Layer
            l_pyc(ji,jj)  = .FALSE.
            l_flux(ji,jj) = .FALSE.
            l_mle(ji,jj)  = .FALSE.
         END IF   ! l_conv
         !
      END_2D
      !
   END SUBROUTINE zdf_osm_osbl_state_fk

   SUBROUTINE zdf_osm_mle_parameters( Kmm, pmld, phmle, pvel_mle, pdiff_mle,   &
      &                               pdbds_mle, phbl, pwb0tot )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE zdf_osm_mle_parameters  ***
      !!
      !! ** Purpose : Timesteps the mixed layer eddy depth, hmle and calculates
      !!              the mixed layer eddy fluxes for buoyancy, heat and
      !!              salinity.
      !!
      !! ** Method  :
      !!
      !! References: Fox-Kemper et al., JPO, 38, 1145-1165, 2008
      !!             Fox-Kemper and Ferrari, JPO, 38, 1166-1179, 2008
      !!
      !!----------------------------------------------------------------------
      INTEGER,                            INTENT(in   ) ::   Kmm         ! Time-level index
      REAL(wp), DIMENSION(A2D(nn_hls)),   INTENT(in   ) ::   pmld        ! == Estimated FK BLD used for MLE horiz gradients == !
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(inout) ::   phmle       ! MLE depth
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(inout) ::   pvel_mle    ! Velocity scale for dhdt with stable ML and FK
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(inout) ::   pdiff_mle   ! Extra MLE vertical diff
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   pdbds_mle   ! Magnitude of horizontal buoyancy gradient
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   phbl        ! BL depth
      REAL(wp), DIMENSION(A2D(nn_hls-1)), INTENT(in   ) ::   pwb0tot     ! Total surface buoyancy flux including insolation
      !!
      INTEGER  ::   ji, jj, jk   ! Dummy loop indices
      REAL(wp) ::   ztmp
      REAL(wp) ::   zdbdz
      REAL(wp) ::   zdtdz
      REAL(wp) ::   zdsdz
      REAL(wp) ::   zthermal
      REAL(wp) ::   zbeta
      REAL(wp) ::   zbuoy
      REAL(wp) ::   zdb_mle
      !!----------------------------------------------------------------------
      !
      ! Calculate vertical buoyancy, heat and salinity fluxes due to MLE
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         IF ( l_conv(ji,jj) ) THEN
            ztmp =  r1_ft(ji,jj) * MIN( 111e3_wp, e1u(ji,jj) ) / rn_osm_mle_lf
            ! This velocity scale, defined in Fox-Kemper et al (2008), is needed for calculating dhdt
            pvel_mle(ji,jj)  = pdbds_mle(ji,jj) * ztmp * hmle(ji,jj) * tmask(ji,jj,1)
            pdiff_mle(ji,jj) = 5e-4_wp * rn_osm_mle_ce * ztmp * pdbds_mle(ji,jj) * phmle(ji,jj)**2
         END IF
      END_2D
      ! Timestep mixed layer eddy depth
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         IF ( l_mle(ji,jj) ) THEN   ! MLE layer growing
            ! Buoyancy gradient at base of MLE layer
            zthermal = rab_n(ji,jj,1,jp_tem)
            zbeta    = rab_n(ji,jj,1,jp_sal)
            zbuoy = grav * ( zthermal * ts(ji,jj,mld_prof(ji,jj)+2,jp_tem,Kmm) -   &
               &             zbeta    * ts(ji,jj,mld_prof(ji,jj)+2,jp_sal,Kmm) )
            zdb_mle = av_b_bl(ji,jj) - zbuoy
            ! Timestep hmle
            hmle(ji,jj) = hmle(ji,jj) + pwb0tot(ji,jj) * rn_Dt / zdb_mle
         ELSE
            IF ( phmle(ji,jj) > phbl(ji,jj) ) THEN
               hmle(ji,jj) = hmle(ji,jj) - ( hmle(ji,jj) - hbl(ji,jj) ) * rn_Dt / rn_osm_mle_tau
            ELSE
               hmle(ji,jj) = hmle(ji,jj) - 10.0_wp * ( hmle(ji,jj) - hbl(ji,jj) ) * rn_Dt / rn_osm_mle_tau
            END IF
         END IF
         hmle(ji,jj) = MAX( MIN( hmle(ji,jj), ht(ji,jj) ), gdepw(ji,jj,4,Kmm) )
         IF ( ln_osm_hmle_limit ) hmle(ji,jj) = MIN( hmle(ji,jj), rn_osm_hmle_limit*hbl(ji,jj) )
         hmle(ji,jj) = pmld(ji,jj)   ! For now try just set hmle to pmld
      END_2D
      !
      DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 5, jpkm1 )
         IF ( hmle(ji,jj) >= gdepw(ji,jj,jk,Kmm) ) mld_prof(ji,jj) = MIN( mbkt(ji,jj), jk )
      END_3D
      DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
         phmle(ji,jj) = gdepw(ji,jj,mld_prof(ji,jj),Kmm)
      END_2D
      !
   END SUBROUTINE zdf_osm_mle_parameters

   SUBROUTINE zdf_osm_init( Kmm )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zdf_osm_init  ***
      !!
      !! ** Purpose :   Initialization of the vertical eddy diffivity and
      !!      viscosity when using a osm turbulent closure scheme
      !!
      !! ** Method  :   Read the namosm namelist and check the parameters
      !!      called at the first timestep (nit000)
      !!
      !! ** input   :   Namlists namzdf_osm and namosm_mle
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in   ) ::   Kmm   ! Time level
      !!
      INTEGER  ::   ios            ! Local integer
      INTEGER  ::   ji, jj, jk     ! Dummy loop indices
      REAL(wp) ::   z1_t2
      !!
      REAL(wp), PARAMETER ::   pp_large = -1e10_wp
      !!
      NAMELIST/namzdf_osm/ ln_use_osm_la,    rn_osm_la,      rn_osm_dstokes,      nn_ave,                nn_osm_wave,        &
         &                 ln_dia_osm,       rn_osm_hbl0,    rn_zdfosm_adjust_sd, ln_kpprimix,           rn_riinfty,         &
         &                 rn_difri,         ln_convmix,     rn_difconv,          nn_osm_wave,           nn_osm_SD_reduce,   &
         &                 ln_osm_mle,       rn_osm_hblfrac, rn_osm_bl_thresh,    ln_zdfosm_ice_shelter
      !! Namelist for Fox-Kemper parametrization
      NAMELIST/namosm_mle/ nn_osm_mle,       rn_osm_mle_ce,     rn_osm_mle_lf,  rn_osm_mle_time,  rn_osm_mle_lat,   &
         &                 rn_osm_mle_rho_c, rn_osm_mle_thresh, rn_osm_mle_tau, ln_osm_hmle_limit, rn_osm_hmle_limit
      !!----------------------------------------------------------------------
      !
      READ  ( numnam_ref, namzdf_osm, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namzdf_osm in reference namelist' )

      READ  ( numnam_cfg, namzdf_osm, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 ) CALL ctl_nam ( ios , 'namzdf_osm in configuration namelist' )
      IF(lwm) WRITE ( numond, namzdf_osm )

      IF(lwp) THEN                    ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'zdf_osm_init : OSMOSIS Parameterisation'
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namzdf_osm : set osm mixing parameters'
         WRITE(numout,*) '      Use  rn_osm_la                                     ln_use_osm_la         = ', ln_use_osm_la
         WRITE(numout,*) '      Use  MLE in OBL, i.e. Fox-Kemper param             ln_osm_mle            = ', ln_osm_mle
         WRITE(numout,*) '      Turbulent Langmuir number                          rn_osm_la             = ', rn_osm_la
         WRITE(numout,*) '      Stokes drift reduction factor                      rn_zdfosm_adjust_sd   = ', rn_zdfosm_adjust_sd
         WRITE(numout,*) '      Initial hbl for 1D runs                            rn_osm_hbl0           = ', rn_osm_hbl0
         WRITE(numout,*) '      Depth scale of Stokes drift                        rn_osm_dstokes        = ', rn_osm_dstokes
         WRITE(numout,*) '      Horizontal average flag                            nn_ave                = ', nn_ave
         WRITE(numout,*) '      Stokes drift                                       nn_osm_wave           = ', nn_osm_wave
         SELECT CASE (nn_osm_wave)
         CASE(0)
            WRITE(numout,*) '      Calculated assuming constant La#=0.3'
         CASE(1)
            WRITE(numout,*) '      Calculated from Pierson Moskowitz wind-waves'
         CASE(2)
            WRITE(numout,*) '      Calculated from ECMWF wave fields'
         END SELECT
         WRITE(numout,*) '      Stokes drift reduction                             nn_osm_SD_reduce      = ', nn_osm_SD_reduce
         WRITE(numout,*) '      Fraction of hbl to average SD over/fit'
         WRITE(numout,*) '      Exponential with nn_osm_SD_reduce = 1 or 2         rn_osm_hblfrac        = ', rn_osm_hblfrac
         SELECT CASE (nn_osm_SD_reduce)
         CASE(0)
            WRITE(numout,*) '     No reduction'
         CASE(1)
            WRITE(numout,*) '     Average SD over upper rn_osm_hblfrac of BL'
         CASE(2)
            WRITE(numout,*) '     Fit exponential to slope rn_osm_hblfrac of BL'
         END SELECT
         WRITE(numout,*) '     Reduce surface SD and depth scale under ice         ln_zdfosm_ice_shelter = ', ln_zdfosm_ice_shelter
         WRITE(numout,*) '     Output osm diagnostics                              ln_dia_osm            = ', ln_dia_osm
         WRITE(numout,*) '         Threshold used to define BL                     rn_osm_bl_thresh      = ', rn_osm_bl_thresh,   &
            &            'm^2/s'
         WRITE(numout,*) '     Use KPP-style shear instability mixing              ln_kpprimix           = ', ln_kpprimix
         WRITE(numout,*) '     Local Richardson Number limit for shear instability rn_riinfty            = ', rn_riinfty
         WRITE(numout,*) '     Maximum shear diffusivity at Rig = 0 (m2/s)         rn_difri              = ', rn_difri
         WRITE(numout,*) '     Use large mixing below BL when unstable             ln_convmix            = ', ln_convmix
         WRITE(numout,*) '     Diffusivity when unstable below BL (m2/s)           rn_difconv            = ', rn_difconv
      ENDIF
      !
      !                              ! Check wave coupling settings !
      !                         ! Further work needed - see ticket #2447 !
      IF ( nn_osm_wave == 2 ) THEN
         IF (.NOT. ( ln_wave .AND. ln_sdw )) &
            & CALL ctl_stop( 'zdf_osm_init : ln_zdfosm and nn_osm_wave=2, ln_wave and ln_sdw must be true' )
      END IF
      !
      ! Flags associated with diagnostic output
      IF ( ln_dia_osm .AND. ( iom_use("zdudz_pyc") .OR. iom_use("zdvdz_pyc") ) )                            ln_dia_pyc_shr = .TRUE.
      IF ( ln_dia_osm .AND. ( iom_use("zdtdz_pyc") .OR. iom_use("zdsdz_pyc") .OR. iom_use("zdbdz_pyc" ) ) ) ln_dia_pyc_scl = .TRUE.
      !
      ! Allocate zdfosm arrays
      IF( zdf_osm_alloc() /= 0 ) CALL ctl_stop( 'STOP', 'zdf_osm_init : unable to allocate arrays' )
      !
      IF( ln_osm_mle ) THEN   ! Initialise Fox-Kemper parametrization
         READ  ( numnam_ref, namosm_mle, IOSTAT = ios, ERR = 903)
903      IF( ios /= 0 ) CALL ctl_nam( ios, 'namosm_mle in reference namelist' )
         READ  ( numnam_cfg, namosm_mle, IOSTAT = ios, ERR = 904 )
904      IF( ios >  0 ) CALL ctl_nam( ios, 'namosm_mle in configuration namelist' )
         IF(lwm) WRITE ( numond, namosm_mle )
         !
         IF(lwp) THEN   ! Namelist print
            WRITE(numout,*)
            WRITE(numout,*) 'zdf_osm_init : initialise mixed layer eddy (MLE)'
            WRITE(numout,*) '~~~~~~~~~~~~~'
            WRITE(numout,*) '   Namelist namosm_mle : '
            WRITE(numout,*) '      MLE type: =0 standard Fox-Kemper ; =1 new formulation   nn_osm_mle        = ', nn_osm_mle
            WRITE(numout,*) '      Magnitude of the MLE (typical value: 0.06 to 0.08)      rn_osm_mle_ce     = ', rn_osm_mle_ce
            WRITE(numout,*) '      Scale of ML front (ML radius of deform.) (nn_osm_mle=0) rn_osm_mle_lf     = ', rn_osm_mle_lf,    &
               &            'm'
            WRITE(numout,*) '      Maximum time scale of MLE                (nn_osm_mle=0) rn_osm_mle_time   = ',   &
               &            rn_osm_mle_time, 's'
            WRITE(numout,*) '      Reference latitude (deg) of MLE coef.    (nn_osm_mle=1) rn_osm_mle_lat    = ', rn_osm_mle_lat,   &
               &            'deg'
            WRITE(numout,*) '      Density difference used to define ML for FK             rn_osm_mle_rho_c  = ', rn_osm_mle_rho_c
            WRITE(numout,*) '      Threshold used to define MLE for FK                     rn_osm_mle_thresh = ',   &
               &            rn_osm_mle_thresh, 'm^2/s'
            WRITE(numout,*) '      Timescale for OSM-FK                                    rn_osm_mle_tau    = ', rn_osm_mle_tau, 's'
            WRITE(numout,*) '      Switch to limit hmle                                    ln_osm_hmle_limit = ', ln_osm_hmle_limit
            WRITE(numout,*) '      hmle limit (fraction of zmld) (ln_osm_hmle_limit = .T.) rn_osm_hmle_limit = ', rn_osm_hmle_limit
         END IF
      END IF
      !
      IF(lwp) THEN
         WRITE(numout,*)
         IF ( ln_osm_mle ) THEN
            WRITE(numout,*) '   ==>>>   Mixed Layer Eddy induced transport added to OSMOSIS BL calculation'
            IF( nn_osm_mle == 0 )   WRITE(numout,*) '              Fox-Kemper et al 2010 formulation'
            IF( nn_osm_mle == 1 )   WRITE(numout,*) '              New formulation'
         ELSE
            WRITE(numout,*) '   ==>>>   Mixed Layer induced transport NOT added to OSMOSIS BL calculation'
         END IF
      END IF
      !
      IF( ln_osm_mle ) THEN   ! MLE initialisation
         !
         rb_c = grav * rn_osm_mle_rho_c / rho0   ! Mixed Layer buoyancy criteria
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '      ML buoyancy criteria = ', rb_c, ' m/s2 '
         IF(lwp) WRITE(numout,*) '      associated ML density criteria defined in zdfmxl = ', rn_osm_mle_rho_c, 'kg/m3'
         !
         IF( nn_osm_mle == 1 ) THEN
            rc_f = rn_osm_mle_ce / ( 5e3_wp * 2.0_wp * omega * SIN( rad * rn_osm_mle_lat ) )
         END IF
         ! 1/(f^2+tau^2)^1/2 at t-point (needed in both nn_osm_mle case)
         z1_t2 = 2e-5_wp
         DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
            r1_ft(ji,jj) = MIN( 1.0_wp / ( ABS( ff_t(ji,jj)) + epsln ), ABS( ff_t(ji,jj) ) / z1_t2**2 )
         END_2D
         ! z1_t2 = 1._wp / ( rn_osm_mle_time * rn_osm_mle_timeji,jj )
         ! r1_ft(:,:) = 1._wp / SQRT(  ff_t(:,:) * ff_t(:,:) + z1_t2  )
         !
      END IF
      !
      CALL osm_rst( nit000, Kmm,  'READ' )   ! Read or initialize hbl, dh, hmle
      !
      IF ( ln_zdfddm ) THEN
         IF(lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) '    Double diffusion mixing on temperature and salinity '
            WRITE(numout,*) '    CAUTION : done in routine zdfosm, not in routine zdfddm '
         END IF
      END IF
      !
      ! Set constants not in namelist
      ! -----------------------------
      IF(lwp) THEN
         WRITE(numout,*)
      END IF
      !
      dstokes(:,:) = pp_large
      IF (nn_osm_wave == 0) THEN
         dstokes(:,:) = rn_osm_dstokes
      END IF
      !
      ! Horizontal average : initialization of weighting arrays
      ! -------------------
      SELECT CASE ( nn_ave )
      CASE ( 0 )                ! no horizontal average
         IF(lwp) WRITE(numout,*) '          no horizontal average on avt'
         IF(lwp) WRITE(numout,*) '          only in very high horizontal resolution !'
         ! Weighting mean arrays etmean
         !           ( 1  1 )
         ! avt = 1/4 ( 1  1 )
         !
         etmean(:,:,:) = 0.0_wp
         !
         DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 1, jpkm1 )
            etmean(ji,jj,jk) = tmask(ji,jj,jk) / MAX( 1.0_wp, umask(ji-1,jj,  jk) + umask(ji,jj,jk) +   &
               &                                              vmask(ji,  jj-1,jk) + vmask(ji,jj,jk) )
         END_3D
      CASE ( 1 )                ! horizontal average
         IF(lwp) WRITE(numout,*) '          horizontal average on avt'
         ! Weighting mean arrays etmean
         !           ( 1/2  1  1/2 )
         ! avt = 1/8 ( 1    2  1   )
         !           ( 1/2  1  1/2 )
         etmean(:,:,:) = 0.0_wp
         !
         DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 1, jpkm1 )
            etmean(ji,jj,jk) = tmask(ji, jj,jk) / MAX( 1.0_wp, 2.0_wp *   tmask(ji,jj,jk) +                               &
               &                                               0.5_wp * ( tmask(ji-1,jj+1,jk) + tmask(ji-1,jj-1,jk) +     &
               &                                                          tmask(ji+1,jj+1,jk) + tmask(ji+1,jj-1,jk) ) +   &
               &                                               1.0_wp * ( tmask(ji-1,jj,  jk) + tmask(ji,  jj+1,jk) +     &
               &                                                          tmask(ji,  jj-1,jk) + tmask(ji+1,jj,  jk) ) )
         END_3D
      CASE DEFAULT
         WRITE(ctmp1,*) '          bad flag value for nn_ave = ', nn_ave
         CALL ctl_stop( ctmp1 )
      END SELECT
      !
      ! Initialization of vertical eddy coef. to the background value
      ! -------------------------------------------------------------
      DO jk = 1, jpk
         avt(:,:,jk) = avtb(jk) * tmask(:,:,jk)
      END DO
      !
      ! Zero the surface flux for non local term and osm mixed layer depth
      ! ------------------------------------------------------------------
      ghamt(:,:,:) = 0.0_wp
      ghams(:,:,:) = 0.0_wp
      ghamu(:,:,:) = 0.0_wp
      ghamv(:,:,:) = 0.0_wp
      !
      IF ( ln_dia_osm ) THEN   ! Initialise auxiliary arrays for diagnostic output
         osmdia2d(:,:)   = 0.0_wp
         osmdia3d(:,:,:) = 0.0_wp
      END IF
      !
   END SUBROUTINE zdf_osm_init

   SUBROUTINE osm_rst( kt, Kmm, cdrw )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE osm_rst  ***
      !!
      !! ** Purpose :   Read or write BL fields in restart file
      !!
      !! ** Method  :   use of IOM library. If the restart does not contain
      !!                required fields, they are recomputed from stratification
      !!
      !!----------------------------------------------------------------------
      INTEGER         , INTENT(in   ) ::   kt     ! Ocean time step index
      INTEGER         , INTENT(in   ) ::   Kmm    ! Ocean time level index (middle)
      CHARACTER(len=*), INTENT(in   ) ::   cdrw   ! "READ"/"WRITE" flag
      !!
      INTEGER  ::   id1, id2, id3                 ! iom enquiry index
      INTEGER  ::   ji, jj, jk                    ! Dummy loop indices
      INTEGER  ::   iiki, ikt                     ! Local integer
      REAL(wp) ::   zhbf                          ! Tempory scalars
      REAL(wp) ::   zN2_c                         ! Local scalar
      REAL(wp) ::   rho_c = 0.01_wp               ! Density criterion for mixed layer depth
      INTEGER, DIMENSION(jpi,jpj) ::   imld_rst   ! Level of mixed-layer depth (pycnocline top)
      !!----------------------------------------------------------------------
      !
      !!-----------------------------------------------------------------------------
      ! If READ/WRITE Flag is 'READ', try to get hbl from restart file. If successful then return
      !!-----------------------------------------------------------------------------
      IF( TRIM(cdrw) == 'READ' .AND. ln_rstart) THEN
         id1 = iom_varid( numror, 'wn', ldstop = .FALSE. )
         IF( id1 > 0 ) THEN   ! 'wn' exists; read
            CALL iom_get( numror, jpdom_auto, 'wn', ww )
            WRITE(numout,*) ' ===>>>> :  wn read from restart file'
         ELSE
            ww(:,:,:) = 0.0_wp
            WRITE(numout,*) ' ===>>>> :  wn not in restart file, set to zero initially'
         END IF
         !
         id1 = iom_varid( numror, 'hbl', ldstop = .FALSE. )
         id2 = iom_varid( numror, 'dh',  ldstop = .FALSE. )
         IF( id1 > 0 .AND. id2 > 0 ) THEN   ! 'hbl' exists; read and return
            CALL iom_get( numror, jpdom_auto, 'hbl', hbl  )
            CALL iom_get( numror, jpdom_auto, 'dh',  dh   )
            hml(:,:) = hbl(:,:) - dh(:,:)   ! Initialise ML depth
            WRITE(numout,*) ' ===>>>> :  hbl & dh read from restart file'
            IF( ln_osm_mle ) THEN
               id3 = iom_varid( numror, 'hmle', ldstop = .FALSE. )
               IF( id3 > 0 ) THEN
                  CALL iom_get( numror, jpdom_auto, 'hmle', hmle )
                  WRITE(numout,*) ' ===>>>> :  hmle read from restart file'
               ELSE
                  WRITE(numout,*) ' ===>>>> :  hmle not found, set to hbl'
                  hmle(:,:) = hbl(:,:)   ! Initialise MLE depth
               END IF
            END IF
            RETURN
         ELSE   ! 'hbl' & 'dh' not in restart file, recalculate
            WRITE(numout,*) ' ===>>>> : previous run without osmosis scheme, hbl computed from stratification'
         END IF
      END IF
      !
      !!-----------------------------------------------------------------------------
      ! If READ/WRITE Flag is 'WRITE', write hbl into the restart file, then return
      !!-----------------------------------------------------------------------------
      IF ( TRIM(cdrw) == 'WRITE' ) THEN
         IF(lwp) WRITE(numout,*) '---- osm-rst ----'
         CALL iom_rstput( kt, nitrst, numrow, 'wn',  ww  )
         CALL iom_rstput( kt, nitrst, numrow, 'hbl', hbl )
         CALL iom_rstput( kt, nitrst, numrow, 'dh',  dh  )
         IF ( ln_osm_mle ) THEN
            CALL iom_rstput( kt, nitrst, numrow, 'hmle', hmle )
         END IF
         RETURN
      END IF
      !
      !!-----------------------------------------------------------------------------
      ! Getting hbl, no restart file with hbl, so calculate from surface stratification
      !!-----------------------------------------------------------------------------
      IF( lwp ) WRITE(numout,*) ' ===>>>> : calculating hbl computed from stratification'
      ! w-level of the mixing and mixed layers
      CALL eos_rab( ts(:,:,:,:,Kmm), rab_n, Kmm )
      CALL bn2( ts(:,:,:,:,Kmm), rab_n, rn2, Kmm )
      imld_rst(:,:) = nlb10            ! Initialization to the number of w ocean point
      hbl(:,:) = 0.0_wp                ! Here hbl used as a dummy variable, integrating vertically N^2
      zN2_c = grav * rho_c * r1_rho0   ! Convert density criteria into N^2 criteria
      !
      hbl(:,:)  = 0.0_wp   ! Here hbl used as a dummy variable, integrating vertically N^2
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 )
         ikt = mbkt(ji,jj)
         hbl(ji,jj) = hbl(ji,jj) + MAX( rn2(ji,jj,jk) , 0.0_wp ) * e3w(ji,jj,jk,Kmm)
         IF ( hbl(ji,jj) < zN2_c ) imld_rst(ji,jj) = MIN( jk , ikt ) + 1   ! Mixed layer level
      END_3D
      !
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         iiki = MAX( 4, imld_rst(ji,jj) )
         hbl(ji,jj) = gdepw(ji,jj,iiki,Kmm  )   ! Turbocline depth
         dh(ji,jj)  = e3t(ji,jj,iiki-1,Kmm  )   ! Turbocline depth
         hml(ji,jj) = hbl(ji,jj) - dh(ji,jj)
      END_2D
      !
      WRITE(numout,*) ' ===>>>> : hbl computed from stratification'
      !
      IF( ln_osm_mle ) THEN
         hmle(:,:) = hbl(:,:)            ! Initialise MLE depth.
         WRITE(numout,*) ' ===>>>> : hmle set = to hbl'
      END IF
      !
      ww(:,:,:) = 0.0_wp
      WRITE(numout,*) ' ===>>>> :  wn not in restart file, set to zero initially'
      !
   END SUBROUTINE osm_rst

   SUBROUTINE tra_osm( kt, Kmm, pts, Krhs )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_osm  ***
      !!
      !! ** Purpose :   compute and add to the tracer trend the non-local tracer flux
      !!
      !! ** Method  :   ???
      !!
      !!----------------------------------------------------------------------
      INTEGER                                  , INTENT(in   ) ::   kt          ! Time step index
      INTEGER                                  , INTENT(in   ) ::   Kmm, Krhs   ! Time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts,jpt), INTENT(inout) ::   pts         ! Active tracers and RHS of tracer equation
      !!
      INTEGER                                 ::   ji, jj, jk
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   ztrdt, ztrds   ! 3D workspace
      !!----------------------------------------------------------------------
      !
      IF ( kt == nit000 ) THEN
         IF ( ntile == 0 .OR. ntile == 1 ) THEN   ! Do only on the first tile
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'tra_osm : OSM non-local tracer fluxes'
            IF(lwp) WRITE(numout,*) '~~~~~~~   '
         END IF
      END IF
      !
      IF ( l_trdtra ) THEN   ! Save ta and sa trends
         ALLOCATE( ztrdt(jpi,jpj,jpk), ztrds(jpi,jpj,jpk) )
         ztrdt(:,:,:) = pts(:,:,:,jp_tem,Krhs)
         ztrds(:,:,:) = pts(:,:,:,jp_sal,Krhs)
      END IF
      !
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         pts(ji,jj,jk,jp_tem,Krhs) =  pts(ji,jj,jk,jp_tem,Krhs)                      &
            &                 - (  ghamt(ji,jj,jk  )  &
            &                    - ghamt(ji,jj,jk+1) ) /e3t(ji,jj,jk,Kmm)
         pts(ji,jj,jk,jp_sal,Krhs) =  pts(ji,jj,jk,jp_sal,Krhs)                      &
            &                 - (  ghams(ji,jj,jk  )  &
            &                    - ghams(ji,jj,jk+1) ) / e3t(ji,jj,jk,Kmm)
      END_3D
      !
      IF ( l_trdtra ) THEN   ! Save the non-local tracer flux trends for diagnostics
         ztrdt(:,:,:) = pts(:,:,:,jp_tem,Krhs) - ztrdt(:,:,:)
         ztrds(:,:,:) = pts(:,:,:,jp_sal,Krhs) - ztrds(:,:,:)
         CALL trd_tra( kt, Kmm, Krhs, 'TRA', jp_tem, jptra_osm, ztrdt )
         CALL trd_tra( kt, Kmm, Krhs, 'TRA', jp_sal, jptra_osm, ztrds )
         DEALLOCATE( ztrdt, ztrds )
      END IF
      !
      IF ( sn_cfctl%l_prtctl ) THEN
         CALL prt_ctl( tab3d_1=pts(:,:,:,jp_tem,Krhs), clinfo1=' osm  - Ta: ', mask1=tmask,   &
            &          tab3d_2=pts(:,:,:,jp_sal,Krhs), clinfo2=       ' Sa: ', mask2=tmask, clinfo3='tra' )
      END IF
      !
   END SUBROUTINE tra_osm

   SUBROUTINE trc_osm( kt )   ! Dummy routine
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trc_osm  ***
      !!
      !! ** Purpose :   compute and add to the passive tracer trend the non-local
      !!                 passive tracer flux
      !!
      !!
      !! ** Method  :   ???
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) :: kt
      !!----------------------------------------------------------------------
      !
      WRITE(*,*) 'trc_osm: Not written yet', kt
      !
   END SUBROUTINE trc_osm

   SUBROUTINE dyn_osm( kt, Kmm, puu, pvv, Krhs )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_osm  ***
      !!
      !! ** Purpose :   compute and add to the velocity trend the non-local flux
      !! copied/modified from tra_osm
      !!
      !! ** Method  :   ???
      !!
      !!----------------------------------------------------------------------
      INTEGER                             , INTENT(in   ) ::   kt          ! Ocean time step index
      INTEGER                             , INTENT(in   ) ::   Kmm, Krhs   ! Ocean time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpt), INTENT(inout) ::   puu, pvv    ! Ocean velocities and RHS of momentum equation
      !!
      INTEGER :: ji, jj, jk   ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      IF ( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'dyn_osm : OSM non-local velocity'
         IF(lwp) WRITE(numout,*) '~~~~~~~   '
      END IF
      !
      ! Code saving tracer trends removed, replace with trdmxl_oce
      !
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )   ! Add non-local u and v fluxes
         puu(ji,jj,jk,Krhs) =  puu(ji,jj,jk,Krhs) - ( ghamu(ji,jj,jk  ) -   &
            &                                         ghamu(ji,jj,jk+1) ) / e3u(ji,jj,jk,Kmm)
         pvv(ji,jj,jk,Krhs) =  pvv(ji,jj,jk,Krhs) - ( ghamv(ji,jj,jk  ) -   &
            &                                         ghamv(ji,jj,jk+1) ) / e3v(ji,jj,jk,Kmm)
      END_3D
      !
      ! Code for saving tracer trends removed
      !
   END SUBROUTINE dyn_osm

   SUBROUTINE zdf_osm_iomput_2d( cdname, posmdia2d )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE zdf_osm_iomput_2d  ***
      !!
      !! ** Purpose :   Wrapper for subroutine iom_put that accepts 2D arrays
      !!                with and without halo
      !!
      !!----------------------------------------------------------------------
      CHARACTER(LEN=*),         INTENT(in   ) ::   cdname
      REAL(wp), DIMENSION(:,:), INTENT(in   ) ::   posmdia2d
      !!----------------------------------------------------------------------
      !
      IF ( ln_dia_osm .AND. iom_use( cdname ) ) THEN
         IF ( SIZE( posmdia2d, 1 ) == ntei-ntsi+1 .AND. SIZE( posmdia2d, 2 ) == ntej-ntsj+1 ) THEN   ! Halo absent
            osmdia2d(A2D(0)) = posmdia2d(:,:)
            CALL iom_put( cdname, osmdia2d(A2D(nn_hls)) )
         ELSE   ! Halo present
            CALL iom_put( cdname, osmdia2d )
         END IF
      END IF
      !
   END SUBROUTINE zdf_osm_iomput_2d

   SUBROUTINE zdf_osm_iomput_3d( cdname, posmdia3d )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE zdf_osm_iomput_3d  ***
      !!
      !! ** Purpose :   Wrapper for subroutine iom_put that accepts 3D arrays
      !!                with and without halo
      !!
      !!----------------------------------------------------------------------
      CHARACTER(LEN=*),           INTENT(in   ) ::   cdname
      REAL(wp), DIMENSION(:,:,:), INTENT(in   ) ::   posmdia3d
      !!----------------------------------------------------------------------
      !
      IF ( ln_dia_osm .AND. iom_use( cdname ) ) THEN
         IF ( SIZE( posmdia3d, 1 ) == ntei-ntsi+1 .AND. SIZE( posmdia3d, 2 ) == ntej-ntsj+1 ) THEN   ! Halo absent
            osmdia3d(A2D(0),:) = posmdia3d(:,:,:)
            CALL iom_put( cdname, osmdia3d(A2D(nn_hls),:) )
         ELSE   ! Halo present
            CALL iom_put( cdname, osmdia3d )
         END IF
      END IF
      !
   END SUBROUTINE zdf_osm_iomput_3d

   !!======================================================================

END MODULE zdfosm
