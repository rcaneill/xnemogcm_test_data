MODULE sbc_phy
   !!======================================================================
   !!                       ***  MODULE  sbc_phy  ***
   !! A set of functions to compute air themodynamics parameters
   !!                     needed by Aerodynamic Bulk Formulas
   !!=====================================================================
   !!            4.x  !  2020 L. Brodeau from AeroBulk package (https://github.com/brodeau/aerobulk/)
   !!----------------------------------------------------------------------

   !!   virt_temp     : virtual (aka sensible) temperature (potential or absolute)
   !!   rho_air       : density of (moist) air (depends on T_air, q_air and SLP
   !!   visc_air      : kinematic viscosity (aka Nu_air) of air from temperature
   !!   L_vap         : latent heat of vaporization of water as a function of temperature
   !!   cp_air        : specific heat of (moist) air (depends spec. hum. q_air)
   !!   gamma_moist   : adiabatic lapse-rate of moist air
   !!   One_on_L      : 1. / ( Obukhov length )
   !!   Ri_bulk       : bulk Richardson number aka BRN
   !!   q_sat         : saturation humidity as a function of SLP and temperature
   !!   q_air_rh      : specific humidity as a function of RH (fraction, not %), t_air and SLP

   USE dom_oce        ! ocean space and time domain
   USE phycst         ! physical constants

   IMPLICIT NONE
   PUBLIC !! Haleluja that was the solution for AGRIF

   INTEGER , PARAMETER, PUBLIC  :: nb_iter0 = 5 ! Default number of itterations in bulk-param algorithms (can be overriden b.m.o `nb_iter` optional argument)

   !!  (mainly removed from sbcblk.F90)
   REAL(wp), PARAMETER, PUBLIC :: rCp_dry = 1005.0_wp             !: Specic heat of dry air, constant pressure       [J/K/kg]
   REAL(wp), PARAMETER, PUBLIC :: rCp_vap = 1860.0_wp             !: Specic heat of water vapor, constant pressure   [J/K/kg]
   REAL(wp), PARAMETER, PUBLIC :: R_dry   = 287.05_wp             !: Specific gas constant for dry air               [J/K/kg]
   REAL(wp), PARAMETER, PUBLIC :: R_vap   = 461.495_wp            !: Specific gas constant for water vapor           [J/K/kg]
   REAL(wp), PARAMETER, PUBLIC :: reps0   = R_dry/R_vap           !: ratio of gas constant for dry air and water vapor => ~ 0.622
   REAL(wp), PARAMETER, PUBLIC :: rctv0   = R_vap/R_dry - 1._wp   !: for virtual temperature (== (1-eps)/eps) => ~ 0.608
   REAL(wp), PARAMETER, PUBLIC :: rCp_air = 1000.5_wp             !: specific heat of air (only used for ice fluxes now...)
   REAL(wp), PARAMETER, PUBLIC :: albo    = 0.066_wp              !: ocean albedo assumed to be constant
   REAL(wp), PARAMETER, PUBLIC :: R_gas   = 8.314510_wp           !: Universal molar gas constant                    [J/mol/K] 
   REAL(wp), PARAMETER, PUBLIC :: rmm_dryair = 28.9647e-3_wp      !: dry air molar mass / molecular weight           [kg/mol]
   REAL(wp), PARAMETER, PUBLIC :: rmm_water  = 18.0153e-3_wp      !: water   molar mass / molecular weight           [kg/mol]
   REAL(wp), PARAMETER, PUBLIC :: rmm_ratio  = rmm_water / rmm_dryair
   REAL(wp), PARAMETER, PUBLIC :: rgamma_dry = R_gas / ( rmm_dryair * rCp_dry )  !: Poisson constant for dry air
   REAL(wp), PARAMETER, PUBLIC :: rpref      = 1.e5_wp            !: reference air pressure for exner function       [Pa]
   !
   REAL(wp), PARAMETER, PUBLIC :: rho0_a  = 1.2_wp      !: Approx. of density of air                       [kg/m^3]
   REAL(wp), PARAMETER, PUBLIC :: rho0_w  = 1025._wp    !: Density of sea-water  (ECMWF->1025)             [kg/m^3]
   REAL(wp), PARAMETER, PUBLIC :: radrw = rho0_a/rho0_w !: Density ratio
   REAL(wp), PARAMETER, PUBLIC :: sq_radrw = SQRT(rho0_a/rho0_w)
   REAL(wp), PARAMETER, PUBLIC :: rCp0_w  = 4190._wp    !: Specific heat capacity of seawater (ECMWF 4190) [J/K/kg]
   REAL(wp), PARAMETER, PUBLIC :: rnu0_w  = 1.e-6_wp    !: kinetic viscosity of water                      [m^2/s]
   REAL(wp), PARAMETER, PUBLIC :: rk0_w   = 0.6_wp      !: thermal conductivity of water (at 20C)          [W/m/K]
   !
   REAL(wp), PARAMETER, PUBLIC :: emiss_w = 0.98_wp     !: Long-wave (thermal) emissivity of sea-water []
   !
   REAL(wp), PARAMETER, PUBLIC :: emiss_i = 0.996_wp    !:  "   for ice and snow => but Rees 1993 suggests can be lower in winter on fresh snow... 0.72 ...

   REAL(wp), PARAMETER, PUBLIC :: wspd_thrshld_ice = 0.2_wp !: minimum scalar wind speed accepted over sea-ice... [m/s]

   !
   REAL(wp), PARAMETER, PUBLIC :: rdct_qsat_salt = 0.98_wp  !: reduction factor on specific humidity at saturation (q_sat(T_s)) due to salt
   REAL(wp), PARAMETER, PUBLIC :: rtt0 = 273.16_wp        !: triple point of temperature    [K]
   !
   REAL(wp), PARAMETER, PUBLIC :: rcst_cs = -16._wp*9.80665_wp*rho0_w*rCp0_w*rnu0_w*rnu0_w*rnu0_w/(rk0_w*rk0_w) !: for cool-skin parameterizations... (grav = 9.80665_wp)
   !                              => see eq.(14) in Fairall et al. 1996   (eq.(6) of Zeng aand Beljaars is WRONG! (typo?)

   REAL(wp), PARAMETER, PUBLIC :: z0_sea_max = 0.0025_wp   !: maximum realistic value for roughness length of sea-surface... [m]

   REAL(wp), PUBLIC, SAVE ::   pp_cldf = 0.81    !: cloud fraction over sea ice, summer CLIO value   [-]


   REAL(wp), PARAMETER, PUBLIC :: Cx_min = 0.1E-3_wp ! smallest value allowed for bulk transfer coefficients (usually in stable conditions with now wind)

   REAL(wp), PARAMETER :: &
                                !! Constants for Goff formula in the presence of ice:
      &      rAg_i = -9.09718_wp, &
      &      rBg_i = -3.56654_wp, &
      &      rCg_i = 0.876793_wp, &
      &      rDg_i = LOG10(6.1071_wp)

   REAL(wp), PARAMETER :: rc_louis  = 5._wp
   REAL(wp), PARAMETER :: rc2_louis = rc_louis * rc_louis
   REAL(wp), PARAMETER :: ram_louis = 2. * rc_louis
   REAL(wp), PARAMETER :: rah_louis = 3. * rc_louis


   INTERFACE virt_temp
      MODULE PROCEDURE virt_temp_vctr, virt_temp_sclr
   END INTERFACE virt_temp

   INTERFACE pres_temp
      MODULE PROCEDURE pres_temp_vctr, pres_temp_sclr
   END INTERFACE pres_temp

   INTERFACE theta_exner
      MODULE PROCEDURE theta_exner_vctr, theta_exner_sclr
   END INTERFACE theta_exner

   INTERFACE visc_air
      MODULE PROCEDURE visc_air_vctr, visc_air_sclr
   END INTERFACE visc_air

   INTERFACE gamma_moist
      MODULE PROCEDURE gamma_moist_vctr, gamma_moist_sclr
   END INTERFACE gamma_moist

   INTERFACE e_sat
      MODULE PROCEDURE e_sat_vctr, e_sat_sclr
   END INTERFACE e_sat

   INTERFACE e_sat_ice
      MODULE PROCEDURE e_sat_ice_vctr, e_sat_ice_sclr
   END INTERFACE e_sat_ice

   INTERFACE de_sat_dt_ice
      MODULE PROCEDURE de_sat_dt_ice_vctr, de_sat_dt_ice_sclr
   END INTERFACE de_sat_dt_ice

   INTERFACE Ri_bulk
      MODULE PROCEDURE Ri_bulk_vctr, Ri_bulk_sclr
   END INTERFACE Ri_bulk

   INTERFACE q_sat
      MODULE PROCEDURE q_sat_vctr, q_sat_sclr
   END INTERFACE q_sat

   INTERFACE dq_sat_dt_ice
      MODULE PROCEDURE dq_sat_dt_ice_vctr, dq_sat_dt_ice_sclr
   END INTERFACE dq_sat_dt_ice

   INTERFACE L_vap
      MODULE PROCEDURE L_vap_vctr, L_vap_sclr
   END INTERFACE L_vap

   INTERFACE rho_air
      MODULE PROCEDURE rho_air_vctr, rho_air_sclr
   END INTERFACE rho_air

   INTERFACE cp_air
      MODULE PROCEDURE cp_air_vctr, cp_air_sclr
   END INTERFACE cp_air

   INTERFACE alpha_sw
      MODULE PROCEDURE alpha_sw_vctr, alpha_sw_sclr
   END INTERFACE alpha_sw

   INTERFACE bulk_formula
      MODULE PROCEDURE bulk_formula_vctr, bulk_formula_sclr
   END INTERFACE bulk_formula

   INTERFACE qlw_net
      MODULE PROCEDURE qlw_net_vctr, qlw_net_sclr
   END INTERFACE qlw_net

   INTERFACE f_m_louis
      MODULE PROCEDURE f_m_louis_vctr, f_m_louis_sclr
   END INTERFACE f_m_louis

   INTERFACE f_h_louis
      MODULE PROCEDURE f_h_louis_vctr, f_h_louis_sclr
   END INTERFACE f_h_louis


   PUBLIC virt_temp
   PUBLIC pres_temp
   PUBLIC theta_exner
   PUBLIC rho_air
   PUBLIC visc_air
   PUBLIC L_vap
   PUBLIC cp_air
   PUBLIC gamma_moist
   PUBLIC One_on_L
   PUBLIC Ri_bulk
   PUBLIC q_sat
   PUBLIC q_air_rh
   PUBLIC dq_sat_dt_ice
   !
   PUBLIC update_qnsol_tau
   PUBLIC alpha_sw
   PUBLIC bulk_formula
   PUBLIC qlw_net
   !
   PUBLIC f_m_louis, f_h_louis
   PUBLIC z0_from_Cd
   PUBLIC Cd_from_z0
   PUBLIC UN10_from_ustar
   PUBLIC UN10_from_CD
   PUBLIC z0tq_LKB

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: sbcblk.F90 10535 2019-01-16 17:36:47Z clem $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS


   FUNCTION virt_temp_sclr( pta, pqa )
      !!------------------------------------------------------------------------
      !!
      !! Compute the (absolute/potential) VIRTUAL temperature, based on the
      !! (absolute/potential) temperature and specific humidity
      !!
      !! If input temperature is absolute then output virtual temperature is absolute
      !! If input temperature is potential then output virtual temperature is potential
      !!
      !! Author: L. Brodeau, June 2019 / AeroBulk
      !!         (https://github.com/brodeau/aerobulk/)
      !!------------------------------------------------------------------------
      REAL(wp)             :: virt_temp_sclr !: virtual temperature [K]
      REAL(wp), INTENT(in) :: pta       !: absolute or potential air temperature [K]
      REAL(wp), INTENT(in) :: pqa       !: specific humidity of air   [kg/kg]
      !!-------------------------------------------------------------------
      !
      virt_temp_sclr = pta * (1._wp + rctv0*pqa)
      !!
      !! This is exactly the same thing as:
      !! virt_temp_sclr = pta * ( pwa + reps0) / (reps0*(1.+pwa))
      !! with wpa (mixing ration) defined as : pwa = pqa/(1.-pqa)
      !
   END FUNCTION virt_temp_sclr

   FUNCTION virt_temp_vctr( pta, pqa )

      REAL(wp), DIMENSION(jpi,jpj)             :: virt_temp_vctr !: virtual temperature [K]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pta !: absolute or potential air temperature [K]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pqa !: specific humidity of air   [kg/kg]

      virt_temp_vctr(:,:) = pta(:,:) * (1._wp + rctv0*pqa(:,:))

   END FUNCTION virt_temp_vctr


   FUNCTION pres_temp_sclr( pqspe, pslp, pz, ptpot, pta, l_ice )

      !!-------------------------------------------------------------------------------
      !!                           ***  FUNCTION pres_temp  ***
      !!
      !! ** Purpose : compute air pressure using barometric equation
      !!              from either potential or absolute air temperature
      !! ** Author: G. Samson, Feb 2021
      !!-------------------------------------------------------------------------------

      REAL(wp)                          :: pres_temp_sclr    ! air pressure              [Pa]
      REAL(wp), INTENT(in )             :: pqspe             ! air specific humidity     [kg/kg]
      REAL(wp), INTENT(in )             :: pslp              ! sea-level pressure        [Pa]
      REAL(wp), INTENT(in )             :: pz                ! height above surface      [m]
      REAL(wp), INTENT(in )  , OPTIONAL :: ptpot             ! air potential temperature [K]
      REAL(wp), INTENT(inout), OPTIONAL :: pta               ! air absolute temperature  [K]
      REAL(wp)                          :: ztpot, zta, zpa, zxm, zmask, zqsat
      INTEGER                           :: it, niter = 3     ! iteration indice and number
      LOGICAL , INTENT(in)   , OPTIONAL :: l_ice             ! sea-ice presence
      LOGICAL                           :: lice              ! sea-ice presence

      IF( PRESENT(ptpot) ) THEN
        zmask = 1._wp
        ztpot = ptpot
        zta   = 0._wp
      ELSE
        zmask = 0._wp 
        ztpot = 0._wp
        zta   = pta
      ENDIF

      lice = .FALSE.
      IF( PRESENT(l_ice) ) lice = l_ice

      zpa = pslp              ! air pressure first guess [Pa]
      DO it = 1, niter
         zta   = ztpot * ( zpa / rpref )**rgamma_dry * zmask + (1._wp - zmask) * zta
         zqsat = q_sat( zta, zpa, l_ice=lice )                                   ! saturation specific humidity [kg/kg]
         zxm   = (1._wp - pqspe/zqsat) * rmm_dryair + pqspe/zqsat * rmm_water    ! moist air molar mass [kg/mol]
         zpa   = pslp * EXP( -grav * zxm * pz / ( R_gas * zta ) )
      END DO

      pres_temp_sclr = zpa
      IF(( PRESENT(pta) ).AND.( PRESENT(ptpot) )) pta = zta

   END FUNCTION pres_temp_sclr


   FUNCTION pres_temp_vctr( pqspe, pslp, pz, ptpot, pta, l_ice )

      !!-------------------------------------------------------------------------------
      !!                           ***  FUNCTION pres_temp  ***
      !!
      !! ** Purpose : compute air pressure using barometric equation
      !!              from either potential or absolute air temperature
      !! ** Author: G. Samson, Feb 2021
      !!-------------------------------------------------------------------------------

      REAL(wp), DIMENSION(jpi,jpj)                          :: pres_temp_vctr    ! air pressure              [Pa]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in )             :: pqspe             ! air specific humidity     [kg/kg]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in )             :: pslp              ! sea-level pressure        [Pa]
      REAL(wp),                     INTENT(in )             :: pz                ! height above surface      [m]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in )  , OPTIONAL :: ptpot             ! air potential temperature [K]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout), OPTIONAL :: pta               ! air absolute temperature  [K]
      INTEGER                                               :: ji, jj            ! loop indices
      LOGICAL                     , INTENT(in)   , OPTIONAL :: l_ice             ! sea-ice presence
      LOGICAL                                               :: lice              ! sea-ice presence
 
      lice = .FALSE.
      IF( PRESENT(l_ice) ) lice = l_ice

      IF( PRESENT(ptpot) ) THEN
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            pres_temp_vctr(ji,jj) = pres_temp_sclr( pqspe(ji,jj), pslp(ji,jj), pz, ptpot=ptpot(ji,jj), pta=pta(ji,jj), l_ice=lice )
         END_2D
      ELSE
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            pres_temp_vctr(ji,jj) = pres_temp_sclr( pqspe(ji,jj), pslp(ji,jj), pz, pta=pta(ji,jj), l_ice=lice )
         END_2D
      ENDIF

   END FUNCTION pres_temp_vctr


   FUNCTION theta_exner_sclr( pta, ppa )

      !!-------------------------------------------------------------------------------
      !!                           ***  FUNCTION theta_exner  ***
      !!
      !! ** Purpose : compute air/surface potential temperature from absolute temperature
      !!              and pressure using Exner function
      !! ** Author: G. Samson, Feb 2021
      !!-------------------------------------------------------------------------------

      REAL(wp)             :: theta_exner_sclr   ! air/surface potential temperature [K]
      REAL(wp), INTENT(in) :: pta                ! air/surface absolute temperature  [K]
      REAL(wp), INTENT(in) :: ppa                ! air/surface pressure              [Pa]
      
      theta_exner_sclr = pta * ( rpref / ppa ) ** rgamma_dry

   END FUNCTION theta_exner_sclr

   FUNCTION theta_exner_vctr( pta, ppa )

      !!-------------------------------------------------------------------------------
      !!                           ***  FUNCTION theta_exner  ***
      !!
      !! ** Purpose : compute air/surface potential temperature from absolute temperature
      !!              and pressure using Exner function
      !! ** Author: G. Samson, Feb 2021
      !!-------------------------------------------------------------------------------

      REAL(wp), DIMENSION(jpi,jpj)             :: theta_exner_vctr   ! air/surface potential temperature [K]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pta                ! air/surface absolute temperature  [K]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: ppa                ! air/surface pressure              [Pa]
      INTEGER                                  :: ji, jj             ! loop indices

      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         theta_exner_vctr(ji,jj) = theta_exner_sclr( pta(ji,jj), ppa(ji,jj) )
      END_2D

   END FUNCTION theta_exner_vctr


   FUNCTION rho_air_vctr( ptak, pqa, ppa )
      !!-------------------------------------------------------------------------------
      !!                           ***  FUNCTION rho_air_vctr  ***
      !!
      !! ** Purpose : compute density of (moist) air using the eq. of state of the atmosphere
      !!
      !! ** Author: L. Brodeau, June 2016 / AeroBulk (https://github.com/brodeau/aerobulk/)
      !!-------------------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) ::   ptak      ! air temperature             [K]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) ::   pqa       ! air specific humidity   [kg/kg]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) ::   ppa      ! pressure in                [Pa]
      REAL(wp), DIMENSION(jpi,jpj)             ::   rho_air_vctr   ! density of moist air   [kg/m^3]
      !!-------------------------------------------------------------------------------

      rho_air_vctr = MAX( ppa / (R_dry*ptak * ( 1._wp + rctv0*pqa )) , 0.8_wp )

   END FUNCTION rho_air_vctr

   FUNCTION rho_air_sclr( ptak, pqa, ppa )
      !!-------------------------------------------------------------------------------
      !!                           ***  FUNCTION rho_air_sclr  ***
      !!
      !! ** Purpose : compute density of (moist) air using the eq. of state of the atmosphere
      !!
      !! ** Author: L. Brodeau, June 2016 / AeroBulk (https://github.com/brodeau/aerobulk/)
      !!-------------------------------------------------------------------------------
      REAL(wp), INTENT(in) :: ptak           ! air temperature             [K]
      REAL(wp), INTENT(in) :: pqa            ! air specific humidity   [kg/kg]
      REAL(wp), INTENT(in) :: ppa           ! pressure in                [Pa]
      REAL(wp)             :: rho_air_sclr   ! density of moist air   [kg/m^3]
      !!-------------------------------------------------------------------------------
      rho_air_sclr = MAX( ppa / (R_dry*ptak * ( 1._wp + rctv0*pqa )) , 0.8_wp )

   END FUNCTION rho_air_sclr


   FUNCTION visc_air_sclr(ptak)
      !!----------------------------------------------------------------------------------
      !! Air kinetic viscosity (m^2/s) given from air temperature in Kelvin
      !!
      !! ** Author: L. Brodeau, june 2016 / AeroBulk (https://github.com/brodeau/aerobulk/)
      !!----------------------------------------------------------------------------------
      REAL(wp)             :: visc_air_sclr   ! kinetic viscosity (m^2/s)
      REAL(wp), INTENT(in) :: ptak       ! air temperature in (K)
      !
      REAL(wp) ::   ztc, ztc2   ! local scalar
      !!----------------------------------------------------------------------------------
      !
      ztc  = ptak - rt0   ! air temp, in deg. C
      ztc2 = ztc*ztc
      visc_air_sclr = 1.326e-5*(1. + 6.542E-3*ztc + 8.301e-6*ztc2 - 4.84e-9*ztc2*ztc)
      !
   END FUNCTION visc_air_sclr

   FUNCTION visc_air_vctr(ptak)

      REAL(wp), DIMENSION(jpi,jpj)             ::   visc_air_vctr   ! kinetic viscosity (m^2/s)
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) ::   ptak       ! air temperature in (K)
      INTEGER  ::   ji, jj      ! dummy loop indices

      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         visc_air_vctr(ji,jj) = visc_air_sclr( ptak(ji,jj) )
      END_2D

   END FUNCTION visc_air_vctr


   FUNCTION L_vap_vctr( psst )
      !!---------------------------------------------------------------------------------
      !!                           ***  FUNCTION L_vap_vctr  ***
      !!
      !! ** Purpose : Compute the latent heat of vaporization of water from temperature
      !!
      !! ** Author: L. Brodeau, june 2016 / AeroBulk (https://github.com/brodeau/aerobulk/)
      !!----------------------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj)             ::   L_vap_vctr   ! latent heat of vaporization   [J/kg]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) ::   psst   ! water temperature                [K]
      !!----------------------------------------------------------------------------------
      !
      L_vap_vctr = (  2.501_wp - 0.00237_wp * ( psst(:,:) - rt0)  ) * 1.e6_wp
      !
   END FUNCTION L_vap_vctr

   FUNCTION L_vap_sclr( psst )
      !!---------------------------------------------------------------------------------
      !!                           ***  FUNCTION L_vap_sclr  ***
      !!
      !! ** Purpose : Compute the latent heat of vaporization of water from temperature
      !!
      !! ** Author: L. Brodeau, june 2016 / AeroBulk (https://github.com/brodeau/aerobulk/)
      !!----------------------------------------------------------------------------------
      REAL(wp)             ::   L_vap_sclr   ! latent heat of vaporization   [J/kg]
      REAL(wp), INTENT(in) ::   psst         ! water temperature                [K]
      !!----------------------------------------------------------------------------------
      !
      L_vap_sclr = (  2.501_wp - 0.00237_wp * ( psst - rt0)  ) * 1.e6_wp
      !
   END FUNCTION L_vap_sclr


   FUNCTION cp_air_vctr( pqa )
      !!-------------------------------------------------------------------------------
      !!                           ***  FUNCTION cp_air_vctr  ***
      !!
      !! ** Purpose : Compute specific heat (Cp) of moist air
      !!
      !! ** Author: L. Brodeau, june 2016 / AeroBulk (https://github.com/brodeau/aerobulk/)
      !!-------------------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) ::   pqa           ! air specific humidity         [kg/kg]
      REAL(wp), DIMENSION(jpi,jpj)             ::   cp_air_vctr   ! specific heat of moist air   [J/K/kg]
      !!-------------------------------------------------------------------------------

      cp_air_vctr = rCp_dry + rCp_vap * pqa

   END FUNCTION cp_air_vctr

   FUNCTION cp_air_sclr( pqa )
      !!-------------------------------------------------------------------------------
      !!                           ***  FUNCTION cp_air_sclr  ***
      !!
      !! ** Purpose : Compute specific heat (Cp) of moist air
      !!
      !! ** Author: L. Brodeau, june 2016 / AeroBulk (https://github.com/brodeau/aerobulk/)
      !!-------------------------------------------------------------------------------
      REAL(wp), INTENT(in) :: pqa           ! air specific humidity         [kg/kg]
      REAL(wp)             :: cp_air_sclr   ! specific heat of moist air   [J/K/kg]
      !!-------------------------------------------------------------------------------

      cp_air_sclr = rCp_dry + rCp_vap * pqa

   END FUNCTION cp_air_sclr


   FUNCTION gamma_moist_sclr( ptak, pqa )
      !!----------------------------------------------------------------------------------
      !! ** Purpose : Compute the moist adiabatic lapse-rate.
      !!     => http://glossary.ametsoc.org/wiki/Moist-adiabatic_lapse_rate
      !!     => http://www.geog.ucsb.edu/~joel/g266_s10/lecture_notes/chapt03/oh10_3_01/oh10_3_01.html
      !!
      !! ** Author: L. Brodeau, June 2016 / AeroBulk (https://github.com/brodeau/aerobulk/)
      !!----------------------------------------------------------------------------------
      REAL(wp)             :: gamma_moist_sclr !                           [K/m]
      REAL(wp), INTENT(in) ::   ptak           ! absolute air temperature  [K] !#LB: double check it's absolute !!!
      REAL(wp), INTENT(in) ::   pqa            ! specific humidity     [kg/kg]
      !
      REAL(wp) :: zta, zqa, zwa, ziRT, zLvap        ! local scalars
      !!----------------------------------------------------------------------------------
      zta = MAX( ptak,  180._wp) ! prevents screw-up over masked regions where field == 0.
      zqa = MAX( pqa,  1.E-6_wp) !    "                   "                     "
      !!
      zwa = zqa / (1._wp - zqa)   ! w is mixing ratio w = q/(1-q) | q = w/(1+w)
      ziRT = 1._wp / (R_dry*zta)    ! 1/RT
      zLvap = L_vap_sclr( ptak )
      !!
      gamma_moist_sclr = grav * ( 1._wp + zLvap*zwa*ziRT ) / ( rCp_dry + zLvap*zLvap*zwa*reps0*ziRT/zta )
      !!
   END FUNCTION gamma_moist_sclr

   FUNCTION gamma_moist_vctr( ptak, pqa )

      REAL(wp), DIMENSION(jpi,jpj)             ::   gamma_moist_vctr
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) ::   ptak
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) ::   pqa
      INTEGER  :: ji, jj

      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         gamma_moist_vctr(ji,jj) = gamma_moist_sclr( ptak(ji,jj), pqa(ji,jj) )
      END_2D

   END FUNCTION gamma_moist_vctr


   FUNCTION One_on_L( ptha, pqa, pus, pts, pqs )
      !!------------------------------------------------------------------------
      !!
      !! Evaluates the 1./(Obukhov length) from air temperature,
      !! air specific humidity, and frictional scales u*, t* and q*
      !!
      !! Author: L. Brodeau, June 2019 / AeroBulk
      !!         (https://github.com/brodeau/aerobulk/)
      !!------------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj)             :: One_on_L     !: 1./(Obukhov length) [m^-1]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: ptha         !: reference potential temperature of air [K]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pqa          !: reference specific humidity of air   [kg/kg]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pus          !: u*: friction velocity [m/s]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pts, pqs     !: \theta* and q* friction aka turb. scales for temp. and spec. hum.
      !
      INTEGER  ::   ji, jj         ! dummy loop indices
      REAL(wp) ::     zqa          ! local scalar
      !!-------------------------------------------------------------------
      !
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         zqa = (1._wp + rctv0*pqa(ji,jj))
         !
         ! The main concern is to know whether, the vertical turbulent flux of virtual temperature, < u' theta_v' > is estimated with:
         !  a/  -u* [ theta* (1 + 0.61 q) + 0.61 theta q* ] => this is the one that seems correct! chose this one!
         !                      or
         !  b/  -u* [ theta*              + 0.61 theta q* ]
         !
         One_on_L(ji,jj) = grav*vkarmn*( pts(ji,jj)*zqa + rctv0*ptha(ji,jj)*pqs(ji,jj) ) &
            &               / MAX( pus(ji,jj)*pus(ji,jj)*ptha(ji,jj)*zqa , 1.E-9_wp )
      END_2D
      !
      One_on_L = SIGN( MIN(ABS(One_on_L),200._wp), One_on_L ) ! (prevent FPE from stupid values over masked regions...)
      !
   END FUNCTION One_on_L


   FUNCTION Ri_bulk_sclr( pz, psst, ptha, pssq, pqa, pub,  pta_layer, pqa_layer )
      !!----------------------------------------------------------------------------------
      !! Bulk Richardson number according to "wide-spread equation"...
      !!
      !!    Reminder: the Richardson number is the ratio "buoyancy" / "shear"
      !!
      !! ** Author: L. Brodeau, June 2019 / AeroBulk (https://github.com/brodeau/aerobulk/)
      !!----------------------------------------------------------------------------------
      REAL(wp)             :: Ri_bulk_sclr
      REAL(wp), INTENT(in) :: pz    ! height above the sea (aka "delta z")  [m]
      REAL(wp), INTENT(in) :: psst  ! potential SST                         [K]
      REAL(wp), INTENT(in) :: ptha  ! pot. air temp. at height "pz"         [K]
      REAL(wp), INTENT(in) :: pssq  ! 0.98*q_sat(SST)                   [kg/kg]
      REAL(wp), INTENT(in) :: pqa   ! air spec. hum. at height "pz"     [kg/kg]
      REAL(wp), INTENT(in) :: pub   ! bulk wind speed                     [m/s]
      REAL(wp), INTENT(in), OPTIONAL :: pta_layer ! when possible, a better guess of absolute temperature WITHIN the layer [K]
      REAL(wp), INTENT(in), OPTIONAL :: pqa_layer ! when possible, a better guess of specific humidity    WITHIN the layer [kg/kg]
      !!
      LOGICAL  :: l_ptqa_l_prvd = .FALSE.
      REAL(wp) :: zqa, zta, zgamma, zdthv, ztv, zsstv  ! local scalars
      REAL(wp) :: ztptv
      !!-------------------------------------------------------------------
      IF( PRESENT(pta_layer) .AND. PRESENT(pqa_layer) ) l_ptqa_l_prvd = .TRUE.
      !
      zsstv = virt_temp_sclr( psst, pssq )   ! virtual potential SST
      ztptv = virt_temp_sclr( ptha, pqa  )   ! virtual potential air temperature
      zdthv = ztptv - zsstv                  ! air-sea delta of "virtual potential temperature"
      !
      Ri_bulk_sclr = grav * zdthv * pz / ( ztptv * pub * pub )      ! the usual definition of Ri_bulk_sclr
      !
   END FUNCTION Ri_bulk_sclr

   FUNCTION Ri_bulk_vctr( pz, psst, ptha, pssq, pqa, pub,  pta_layer, pqa_layer )

      REAL(wp), DIMENSION(jpi,jpj)             :: Ri_bulk_vctr
      REAL(wp)                    , INTENT(in) :: pz    ! height above the sea (aka "delta z")  [m]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: psst  ! SST                                   [K]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: ptha  ! pot. air temp. at height "pz"         [K]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pssq  ! 0.98*q_sat(SST)                   [kg/kg]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pqa   ! air spec. hum. at height "pz"     [kg/kg]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pub   ! bulk wind speed                     [m/s]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in), OPTIONAL :: pta_layer ! when possible, a better guess of absolute temperature WITHIN the layer [K]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in), OPTIONAL :: pqa_layer ! when possible, a better guess of specific humidity    WITHIN the layer [kg/kg]
      !!
      LOGICAL  :: l_ptqa_l_prvd = .FALSE.
      INTEGER  ::   ji, jj

      IF( PRESENT(pta_layer) .AND. PRESENT(pqa_layer) ) l_ptqa_l_prvd = .TRUE.
      IF( l_ptqa_l_prvd ) THEN
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            Ri_bulk_vctr(ji,jj) = Ri_bulk_sclr( pz, psst(ji,jj), ptha(ji,jj), pssq(ji,jj), pqa(ji,jj), pub(ji,jj), &
               &                                pta_layer=pta_layer(ji,jj ),  pqa_layer=pqa_layer(ji,jj ) )
         END_2D
      ELSE
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            Ri_bulk_vctr(ji,jj) = Ri_bulk_sclr( pz, psst(ji,jj), ptha(ji,jj), pssq(ji,jj), pqa(ji,jj), pub(ji,jj) )
         END_2D
      END IF

   END FUNCTION Ri_bulk_vctr


   FUNCTION e_sat_sclr( ptak )
      !!----------------------------------------------------------------------------------
      !!                   ***  FUNCTION e_sat_sclr  ***
      !!                  < SCALAR argument version >
      !! ** Purpose : water vapor at saturation in [Pa]
      !!              Based on accurate estimate by Goff, 1957
      !!
      !! ** Author: L. Brodeau, june 2016 / AeroBulk (https://github.com/brodeau/aerobulk/)
      !!
      !!    Note: what rt0 should be here, is 273.16 (triple point of water) and not 273.15 like here
      !!----------------------------------------------------------------------------------
      REAL(wp)             ::   e_sat_sclr   ! water vapor at saturation   [kg/kg]
      REAL(wp), INTENT(in) ::   ptak    ! air temperature                  [K]
      REAL(wp) ::   zta, ztmp   ! local scalar
      !!----------------------------------------------------------------------------------
      zta = MAX( ptak , 180._wp )   ! air temp., prevents fpe0 errors dute to unrealistically low values over masked regions...
      ztmp = rt0 / zta   !#LB: rt0 or rtt0 ???? (273.15 vs 273.16 )
      !
      ! Vapour pressure at saturation [Pa] : WMO, (Goff, 1957)
      e_sat_sclr = 100.*( 10.**( 10.79574*(1. - ztmp) - 5.028*LOG10(zta/rt0)        &
         &    + 1.50475*10.**(-4)*(1. - 10.**(-8.2969*(zta/rt0 - 1.)) )  &
         &    + 0.42873*10.**(-3)*(10.**(4.76955*(1. - ztmp)) - 1.) + 0.78614) )
      !
   END FUNCTION e_sat_sclr

   FUNCTION e_sat_vctr(ptak)
      REAL(wp), DIMENSION(jpi,jpj)             :: e_sat_vctr !: vapour pressure at saturation  [Pa]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: ptak    !: temperature (K)
      INTEGER  ::   ji, jj         ! dummy loop indices
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
      e_sat_vctr(ji,jj) = e_sat_sclr(ptak(ji,jj))
      END_2D
   END FUNCTION e_sat_vctr


   FUNCTION e_sat_ice_sclr(ptak)
      !!---------------------------------------------------------------------------------
      !! Same as "e_sat" but over ice rather than water!
      !!---------------------------------------------------------------------------------
      REAL(wp)             :: e_sat_ice_sclr !: vapour pressure at saturation in presence of ice [Pa]
      REAL(wp), INTENT(in) :: ptak
      !!
      REAL(wp) :: zta, zle, ztmp
      !!---------------------------------------------------------------------------------
      zta = MAX( ptak , 180._wp )   ! air temp., prevents fpe0 errors dute to unrealistically low values over masked regions...
      ztmp = rtt0/zta
      !!
      zle  = rAg_i*(ztmp - 1._wp) + rBg_i*LOG10(ztmp) + rCg_i*(1._wp - zta/rtt0) + rDg_i
      !!
      e_sat_ice_sclr = 100._wp * 10._wp**zle
   
   END FUNCTION e_sat_ice_sclr

   FUNCTION e_sat_ice_vctr(ptak)
      !! Same as "e_sat" but over ice rather than water!
      REAL(wp), DIMENSION(jpi,jpj) :: e_sat_ice_vctr !: vapour pressure at saturation in presence of ice [Pa]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: ptak
      INTEGER  :: ji, jj
      !!----------------------------------------------------------------------------------
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         e_sat_ice_vctr(ji,jj) = e_sat_ice_sclr( ptak(ji,jj) )
      END_2D

   END FUNCTION e_sat_ice_vctr


   FUNCTION de_sat_dt_ice_sclr(ptak)
      !!---------------------------------------------------------------------------------
      !! d [ e_sat_ice ] / dT   (derivative / temperature)
      !! Analytical exact formulation: double checked!!!
      !!  => DOUBLE-check possible / finite-difference version with "./bin/test_phymbl.x"
      !!---------------------------------------------------------------------------------
      REAL(wp)             :: de_sat_dt_ice_sclr !:  [Pa/K]
      REAL(wp), INTENT(in) :: ptak
      !!
      REAL(wp) :: zta, zde
      !!---------------------------------------------------------------------------------
      zta = MAX( ptak , 180._wp )   ! air temp., prevents fpe0 errors dute to unrealistically low values over masked regions...
      !!
      zde = -(rAg_i*rtt0)/(zta*zta) - rBg_i/(zta*LOG(10._wp)) - rCg_i/rtt0
      !!
      de_sat_dt_ice_sclr = LOG(10._wp) * zde * e_sat_ice_sclr(zta)
   END FUNCTION de_sat_dt_ice_sclr

   FUNCTION de_sat_dt_ice_vctr(ptak)
      !! Same as "e_sat" but over ice rather than water!
      REAL(wp), DIMENSION(jpi,jpj) :: de_sat_dt_ice_vctr !: vapour pressure at saturation in presence of ice [Pa]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: ptak
      INTEGER  :: ji, jj
      !!----------------------------------------------------------------------------------
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         de_sat_dt_ice_vctr(ji,jj) = de_sat_dt_ice_sclr( ptak(ji,jj) )
      END_2D

   END FUNCTION de_sat_dt_ice_vctr


   FUNCTION q_sat_sclr( pta, ppa,  l_ice )
      !!---------------------------------------------------------------------------------
      !!                           ***  FUNCTION q_sat_sclr  ***
      !!
      !! ** Purpose : Conputes specific humidity of air at saturation
      !!
      !! ** Author: L. Brodeau, june 2016 / AeroBulk (https://github.com/brodeau/aerobulk/)
      !!----------------------------------------------------------------------------------
      REAL(wp) :: q_sat_sclr
      REAL(wp), INTENT(in) :: pta  !: absolute temperature of air [K]
      REAL(wp), INTENT(in) :: ppa  !: atmospheric pressure        [Pa]
      LOGICAL,  INTENT(in), OPTIONAL :: l_ice  !: we are above ice
      REAL(wp) :: ze_s
      LOGICAL  :: lice
      !!----------------------------------------------------------------------------------
      lice = .FALSE.
      IF( PRESENT(l_ice) ) lice = l_ice
      IF( lice ) THEN
         ze_s = e_sat_ice( pta )
      ELSE
         ze_s = e_sat( pta ) ! Vapour pressure at saturation (Goff) :
      END IF
      q_sat_sclr = reps0*ze_s/(ppa - (1._wp - reps0)*ze_s)

   END FUNCTION q_sat_sclr

   FUNCTION q_sat_vctr( pta, ppa,  l_ice )

      REAL(wp), DIMENSION(jpi,jpj) :: q_sat_vctr
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pta  !: absolute temperature of air [K]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: ppa  !: atmospheric pressure        [Pa]
      LOGICAL,  INTENT(in), OPTIONAL :: l_ice  !: we are above ice
      LOGICAL  :: lice
      INTEGER  :: ji, jj
      !!----------------------------------------------------------------------------------
      lice = .FALSE.
      IF( PRESENT(l_ice) ) lice = l_ice
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         q_sat_vctr(ji,jj) = q_sat_sclr( pta(ji,jj) , ppa(ji,jj), l_ice=lice )
      END_2D

   END FUNCTION q_sat_vctr


   FUNCTION dq_sat_dt_ice_sclr( pta, ppa )
      !!---------------------------------------------------------------------------------
      !!     ***  FUNCTION dq_sat_dt_ice_sclr  ***
      !!    => d [ q_sat_ice(T) ] / dT
      !! Analytical exact formulation: double checked!!!
      !!  => DOUBLE-check possible / finite-difference version with "./bin/test_phymbl.x"
      !!----------------------------------------------------------------------------------
      REAL(wp) :: dq_sat_dt_ice_sclr
      REAL(wp), INTENT(in) :: pta  !: absolute temperature of air [K]
      REAL(wp), INTENT(in) :: ppa  !: atmospheric pressure        [Pa]
      REAL(wp) :: ze_s, zde_s_dt, ztmp
      !!----------------------------------------------------------------------------------
      ze_s     =  e_sat_ice_sclr( pta ) ! Vapour pressure at saturation  in presence of ice (Goff)
      zde_s_dt = de_sat_dt_ice(   pta )
      !
      ztmp = (reps0 - 1._wp)*ze_s + ppa
      !
      dq_sat_dt_ice_sclr = reps0*ppa*zde_s_dt / ( ztmp*ztmp )
      !
   END FUNCTION dq_sat_dt_ice_sclr

   FUNCTION dq_sat_dt_ice_vctr( pta, ppa )

      REAL(wp), DIMENSION(jpi,jpj) :: dq_sat_dt_ice_vctr
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pta  !: absolute temperature of air [K]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: ppa  !: atmospheric pressure        [Pa]
      INTEGER  :: ji, jj
      !!----------------------------------------------------------------------------------
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         dq_sat_dt_ice_vctr(ji,jj) = dq_sat_dt_ice_sclr( pta(ji,jj) , ppa(ji,jj) )
      END_2D

   END FUNCTION dq_sat_dt_ice_vctr


   FUNCTION q_air_rh(prha, ptak, ppa)
      !!----------------------------------------------------------------------------------
      !! Specific humidity of air out of Relative Humidity
      !!
      !! ** Author: L. Brodeau, june 2016 / AeroBulk (https://github.com/brodeau/aerobulk/)
      !!----------------------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj)             :: q_air_rh
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: prha        !: relative humidity      [fraction, not %!!!]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: ptak        !: air temperature        [K]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: ppa        !: atmospheric pressure   [Pa]
      !
      INTEGER  ::   ji, jj      ! dummy loop indices
      REAL(wp) ::   ze      ! local scalar
      !!----------------------------------------------------------------------------------
      !
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         ze = prha(ji,jj)*e_sat_sclr(ptak(ji,jj))
         q_air_rh(ji,jj) = ze*reps0/(ppa(ji,jj) - (1. - reps0)*ze)
      END_2D
      !
   END FUNCTION q_air_rh


   SUBROUTINE UPDATE_QNSOL_TAU( pzu, pTs, pqs, pTa, pqa, pust, ptst, pqst, pwnd, pUb, ppa, prlw, prhoa, &
      &                         pQns, pTau,  &
      &                         Qlat)
      !!----------------------------------------------------------------------------------
      !! Purpose: returns the non-solar heat flux to the ocean aka "Qlat + Qsen + Qlw"
      !!          and the module of the wind stress => pTau = Tau
      !! ** Author: L. Brodeau, Sept. 2019 / AeroBulk (https://github.com/brodeau/aerobulk/)
      !!----------------------------------------------------------------------------------
      REAL(wp),                     INTENT(in)  :: pzu  ! height above the sea-level where all this takes place (normally 10m)
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pTs  ! water temperature at the air-sea interface [K]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pqs  ! satur. spec. hum. at T=pTs   [kg/kg]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pTa  ! potential air temperature at z=pzu [K]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pqa  ! specific humidity at z=pzu [kg/kg]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pust ! u*
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: ptst ! t*
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pqst ! q*
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pwnd ! wind speed module at z=pzu [m/s]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pUb  ! bulk wind speed at z=pzu (inc. pot. effect of gustiness etc) [m/s]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: ppa ! sea-level atmospheric pressure [Pa]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: prlw ! downwelling longwave radiative flux [W/m^2]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: prhoa ! air density [kg/m3]
      !
      REAL(wp), DIMENSION(jpi,jpj), INTENT(out) :: pQns ! non-solar heat flux to the ocean aka "Qlat + Qsen + Qlw" [W/m^2]]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(out) :: pTau ! module of the wind stress [N/m^2]
      !
      REAL(wp), DIMENSION(jpi,jpj), OPTIONAL, INTENT(out) :: Qlat
      !
      REAL(wp) :: zdt, zdq, zCd, zCh, zCe, zz0, zQlat, zQsen, zQlw
      INTEGER  ::   ji, jj     ! dummy loop indices
      !!----------------------------------------------------------------------------------
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )

         zdt = pTa(ji,jj) - pTs(ji,jj) ;  zdt = SIGN( MAX(ABS(zdt),1.E-6_wp), zdt )
         zdq = pqa(ji,jj) - pqs(ji,jj) ;  zdq = SIGN( MAX(ABS(zdq),1.E-9_wp), zdq )
         zz0 = pust(ji,jj)/pUb(ji,jj)
         zCd = zz0*zz0
         zCh = zz0*ptst(ji,jj)/zdt
         zCe = zz0*pqst(ji,jj)/zdq

         CALL BULK_FORMULA( pzu, pTs(ji,jj), pqs(ji,jj), pTa(ji,jj), pqa(ji,jj), zCd, zCh, zCe, &
            &              pwnd(ji,jj), pUb(ji,jj), ppa(ji,jj), prhoa(ji,jj), &
            &              pTau(ji,jj), zQsen, zQlat )

         zQlw = qlw_net_sclr( prlw(ji,jj), pTs(ji,jj) ) ! Net longwave flux

         pQns(ji,jj) = zQlat + zQsen + zQlw

         IF( PRESENT(Qlat) ) Qlat(ji,jj) = zQlat

      END_2D

   END SUBROUTINE UPDATE_QNSOL_TAU


   SUBROUTINE BULK_FORMULA_SCLR( pzu, pTs, pqs, pTa, pqa, &
      &                          pCd, pCh, pCe,           &
      &                          pwnd, pUb, ppa, prhoa,   &
      &                          pTau, pQsen, pQlat,      &
      &                          pEvap, pfact_evap        )
      !!----------------------------------------------------------------------------------
      REAL(wp), INTENT(in)  :: pzu   ! height above the sea-level where all this takes place (normally 10m)
      REAL(wp), INTENT(in)  :: pTs   ! water temperature at the air-sea interface [K]
      REAL(wp), INTENT(in)  :: pqs   ! satur. spec. hum. at T=pTs   [kg/kg]
      REAL(wp), INTENT(in)  :: pTa   ! potential air temperature at z=pzu [K]
      REAL(wp), INTENT(in)  :: pqa   ! specific humidity at z=pzu [kg/kg]
      REAL(wp), INTENT(in)  :: pCd
      REAL(wp), INTENT(in)  :: pCh
      REAL(wp), INTENT(in)  :: pCe
      REAL(wp), INTENT(in)  :: pwnd  ! wind speed module at z=pzu [m/s]
      REAL(wp), INTENT(in)  :: pUb   ! bulk wind speed at z=pzu (inc. pot. effect of gustiness etc) [m/s]
      REAL(wp), INTENT(in)  :: ppa   ! sea-level atmospheric pressure [Pa]
      REAL(wp), INTENT(in)  :: prhoa ! Air density at z=pzu [kg/m^3]
      !!
      REAL(wp), INTENT(out) :: pTau  ! module of the wind stress [N/m^2]
      REAL(wp), INTENT(out) :: pQsen !  [W/m^2]
      REAL(wp), INTENT(out) :: pQlat !  [W/m^2]
      !!
      REAL(wp), INTENT(out), OPTIONAL :: pEvap ! Evaporation [kg/m^2/s]
      REAL(wp), INTENT(in) , OPTIONAL :: pfact_evap  ! ABOMINATION: corrective factor for evaporation (doing this against my will! /laurent)
      !!
      REAL(wp) :: ztaa, zgamma, zrho, zUrho, zevap, zfact_evap
      INTEGER  :: jq
      !!----------------------------------------------------------------------------------
      zfact_evap = 1._wp
      IF( PRESENT(pfact_evap) ) zfact_evap = pfact_evap

      zUrho = pUb*MAX(prhoa, 1._wp)     ! rho*U10

      pTau = zUrho * pCd * pwnd ! Wind stress module

      zevap = zUrho * pCe * (pqa - pqs)
      pQsen = zUrho * pCh * (pTa - pTs) * cp_air(pqa)
      pQlat = L_vap(pTs) * zevap

      IF( PRESENT(pEvap) ) pEvap = - zfact_evap * zevap

   END SUBROUTINE BULK_FORMULA_SCLR

   SUBROUTINE BULK_FORMULA_VCTR( pzu, pTs, pqs, pTa, pqa, &
      &                          pCd, pCh, pCe,           &
      &                          pwnd, pUb, ppa, prhoa,   &
      &                          pTau, pQsen, pQlat,      &
      &                          pEvap, pfact_evap )
      !!----------------------------------------------------------------------------------
      REAL(wp),                     INTENT(in)  :: pzu   ! height above the sea-level where all this takes place (normally 10m)
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pTs   ! water temperature at the air-sea interface [K]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pqs   ! satur. spec. hum. at T=pTs   [kg/kg]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pTa   ! potential air temperature at z=pzu [K]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pqa   ! specific humidity at z=pzu [kg/kg]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pCd
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pCh
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pCe
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pwnd  ! wind speed module at z=pzu [m/s]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pUb   ! bulk wind speed at z=pzu (inc. pot. effect of gustiness etc) [m/s]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: ppa   ! sea-level atmospheric pressure [Pa]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: prhoa ! Air density at z=pzu [kg/m^3] 
      !!
      REAL(wp), DIMENSION(jpi,jpj), INTENT(out) :: pTau  ! module of the wind stress [N/m^2]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(out) :: pQsen !  [W/m^2]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(out) :: pQlat !  [W/m^2]
      !!
      REAL(wp), DIMENSION(jpi,jpj), INTENT(out), OPTIONAL :: pEvap ! Evaporation [kg/m^2/s]
      REAL(wp),                     INTENT(in) , OPTIONAL :: pfact_evap  ! ABOMINATION: corrective factor for evaporation (doing this against my will! /laurent)
      !!
      REAL(wp) :: ztaa, zgamma, zrho, zUrho, zevap, zfact_evap
      INTEGER  :: ji, jj
      !!----------------------------------------------------------------------------------
      zfact_evap = 1._wp
      IF( PRESENT(pfact_evap) ) zfact_evap = pfact_evap

      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )

         CALL BULK_FORMULA_SCLR( pzu, pTs(ji,jj), pqs(ji,jj), pTa(ji,jj), pqa(ji,jj), &
            &                    pCd(ji,jj), pCh(ji,jj), pCe(ji,jj),                  &
            &                    pwnd(ji,jj), pUb(ji,jj), ppa(ji,jj), prhoa(ji,jj),   &
            &                    pTau(ji,jj), pQsen(ji,jj), pQlat(ji,jj),             &
            &                    pEvap=zevap, pfact_evap=zfact_evap                   )

         IF( PRESENT(pEvap) ) pEvap(ji,jj) = zevap
      END_2D

   END SUBROUTINE BULK_FORMULA_VCTR


   FUNCTION alpha_sw_vctr( psst )
      !!---------------------------------------------------------------------------------
      !!                           ***  FUNCTION alpha_sw_vctr  ***
      !!
      !! ** Purpose : ROUGH estimate of the thermal expansion coefficient of sea-water at the surface (P =~ 1010 hpa)
      !!
      !! ** Author: L. Brodeau, june 2016 / AeroBulk (https://github.com/brodeau/aerobulk/)
      !!----------------------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj)             ::   alpha_sw_vctr   ! thermal expansion coefficient of sea-water [1/K]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) ::   psst   ! water temperature                [K]
      !!----------------------------------------------------------------------------------
      alpha_sw_vctr = 2.1e-5_wp * MAX(psst(:,:)-rt0 + 3.2_wp, 0._wp)**0.79

   END FUNCTION alpha_sw_vctr

   FUNCTION alpha_sw_sclr( psst )
      !!---------------------------------------------------------------------------------
      !!                           ***  FUNCTION alpha_sw_sclr  ***
      !!
      !! ** Purpose : ROUGH estimate of the thermal expansion coefficient of sea-water at the surface (P =~ 1010 hpa)
      !!
      !! ** Author: L. Brodeau, june 2016 / AeroBulk (https://github.com/brodeau/aerobulk/)
      !!----------------------------------------------------------------------------------
      REAL(wp)             ::   alpha_sw_sclr   ! thermal expansion coefficient of sea-water [1/K]
      REAL(wp), INTENT(in) ::   psst   ! sea-water temperature                   [K]
      !!----------------------------------------------------------------------------------
      alpha_sw_sclr = 2.1e-5_wp * MAX(psst-rt0 + 3.2_wp, 0._wp)**0.79

   END FUNCTION alpha_sw_sclr


   FUNCTION qlw_net_sclr( pdwlw, pts,  l_ice )
      !!---------------------------------------------------------------------------------
      !!                           ***  FUNCTION qlw_net_sclr  ***
      !!
      !! ** Purpose : Estimate of the net longwave flux at the surface
      !!----------------------------------------------------------------------------------
      REAL(wp) :: qlw_net_sclr
      REAL(wp), INTENT(in) :: pdwlw !: downwelling longwave (aka infrared, aka thermal) radiation [W/m^2]
      REAL(wp), INTENT(in) :: pts   !: surface temperature [K]
      LOGICAL,  INTENT(in), OPTIONAL :: l_ice  !: we are above ice
      REAL(wp) :: zemiss, zt2
      LOGICAL  :: lice
      !!----------------------------------------------------------------------------------
      lice = .FALSE.
      IF( PRESENT(l_ice) ) lice = l_ice
      IF( lice ) THEN
         zemiss = emiss_i
      ELSE
         zemiss = emiss_w
      END IF
      zt2 = pts*pts
      qlw_net_sclr = zemiss*( pdwlw - stefan*zt2*zt2)  ! zemiss used both as the IR albedo and IR emissivity...

   END FUNCTION qlw_net_sclr

   FUNCTION qlw_net_vctr( pdwlw, pts,  l_ice )

      REAL(wp), DIMENSION(jpi,jpj) :: qlw_net_vctr
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pdwlw !: downwelling longwave (aka infrared, aka thermal) radiation [W/m^2]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pts   !: surface temperature [K]
      LOGICAL,  INTENT(in), OPTIONAL :: l_ice  !: we are above ice
      LOGICAL  :: lice
      INTEGER  :: ji, jj
      !!----------------------------------------------------------------------------------
      lice = .FALSE.
      IF( PRESENT(l_ice) ) lice = l_ice
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         qlw_net_vctr(ji,jj) = qlw_net_sclr( pdwlw(ji,jj) , pts(ji,jj), l_ice=lice )
      END_2D

   END FUNCTION qlw_net_vctr


   FUNCTION z0_from_Cd( pzu, pCd,  ppsi )

      REAL(wp), DIMENSION(jpi,jpj) :: z0_from_Cd        !: roughness length [m]
      REAL(wp)                    , INTENT(in) :: pzu   !: reference height zu [m]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pCd   !: (neutral or non-neutral) drag coefficient []
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in), OPTIONAL :: ppsi !: "Psi_m(pzu/L)" stability correction profile for momentum []
      !!
      !! If pCd is the NEUTRAL-STABILITY drag coefficient then ppsi must be 0 or not given
      !! If pCd is the drag coefficient (in stable or unstable conditions) then pssi must be provided
      !!----------------------------------------------------------------------------------
      IF( PRESENT(ppsi) ) THEN
         !! Cd provided is the actual Cd (not the neutral-stability CdN) :
         z0_from_Cd = pzu * EXP( - ( vkarmn/SQRT(pCd(:,:)) + ppsi(:,:) ) ) !LB: ok, double-checked!
      ELSE
         !! Cd provided is the neutral-stability Cd, aka CdN :
         z0_from_Cd = pzu * EXP( - vkarmn/SQRT(pCd(:,:)) )            !LB: ok, double-checked!
      END IF

   END FUNCTION z0_from_Cd


   FUNCTION Cd_from_z0( pzu, pz0,  ppsi )

      REAL(wp), DIMENSION(jpi,jpj) :: Cd_from_z0        !: (neutral or non-neutral) drag coefficient []
      REAL(wp)                    , INTENT(in) :: pzu   !: reference height zu [m]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pz0   !: roughness length [m]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in), OPTIONAL :: ppsi !: "Psi_m(pzu/L)" stability correction profile for momentum []
      !!
      !! If we want to return the NEUTRAL-STABILITY drag coefficient then ppsi must be 0 or not given
      !! If we want to return the stability-corrected Cd (i.e. in stable or unstable conditions) then pssi must be provided
      !!----------------------------------------------------------------------------------
      IF( PRESENT(ppsi) ) THEN
         !! The Cd we return is the actual Cd (not the neutral-stability CdN) :
         Cd_from_z0 = 1._wp / ( LOG( pzu / pz0(:,:) ) - ppsi(:,:) )
      ELSE
         !! The Cd we return is the neutral-stability Cd, aka CdN :
         Cd_from_z0 = 1._wp /   LOG( pzu / pz0(:,:) )
      END IF
      Cd_from_z0 = vkarmn2 * Cd_from_z0 * Cd_from_z0

   END FUNCTION Cd_from_z0


   FUNCTION f_m_louis_sclr( pzu, pRib, pCdn, pz0 )
      !!----------------------------------------------------------------------------------
      !!  Stability correction function for MOMENTUM
      !!                 Louis (1979)
      !!----------------------------------------------------------------------------------
      REAL(wp)             :: f_m_louis_sclr ! term "f_m" in Eq.(6) when option "Louis" rather than "Psi(zeta) is chosen, Lupkes & Gryanik (2015),
      REAL(wp), INTENT(in) :: pzu     ! reference height (height for pwnd)  [m]
      REAL(wp), INTENT(in) :: pRib    ! Bulk Richardson number
      REAL(wp), INTENT(in) :: pCdn    ! neutral drag coefficient
      REAL(wp), INTENT(in) :: pz0     ! roughness length                      [m]
      !!----------------------------------------------------------------------------------
      REAL(wp) :: ztu, zts, zstab
      !!----------------------------------------------------------------------------------
      zstab = 0.5 + SIGN(0.5_wp, pRib) ; ! Unstable (Ri<0) => zstab = 0 | Stable (Ri>0) => zstab = 1
      !
      ztu = pRib / ( 1._wp + 3._wp * rc2_louis * pCdn * SQRT( ABS( -pRib * ( pzu / pz0 + 1._wp) ) ) ) ! ABS is just here for when it's stable conditions and ztu is not used anyways
      zts = pRib / SQRT( ABS( 1._wp + pRib ) ) ! ABS is just here for when it's UNstable conditions and zts is not used anyways
      !
      f_m_louis_sclr = (1._wp - zstab) *         ( 1._wp - ram_louis * ztu )  &  ! Unstable Eq.(A6)
         &               +      zstab  * 1._wp / ( 1._wp + ram_louis * zts )     ! Stable   Eq.(A7)
      !
   END FUNCTION f_m_louis_sclr

   FUNCTION f_m_louis_vctr( pzu, pRib, pCdn, pz0 )

      REAL(wp), DIMENSION(jpi,jpj)             :: f_m_louis_vctr
      REAL(wp),                     INTENT(in) :: pzu
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pRib
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pCdn
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pz0
      INTEGER  :: ji, jj

      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         f_m_louis_vctr(ji,jj) = f_m_louis_sclr( pzu, pRib(ji,jj), pCdn(ji,jj), pz0(ji,jj) )
      END_2D

   END FUNCTION f_m_louis_vctr


   FUNCTION f_h_louis_sclr( pzu, pRib, pChn, pz0 )
      !!----------------------------------------------------------------------------------
      !!  Stability correction function for HEAT
      !!                 Louis (1979)
      !!----------------------------------------------------------------------------------
      REAL(wp)             :: f_h_louis_sclr ! term "f_h" in Eq.(6) when option "Louis" rather than "Psi(zeta) is chosen, Lupkes & Gryanik (2015),
      REAL(wp), INTENT(in) :: pzu     ! reference height (height for pwnd)  [m]
      REAL(wp), INTENT(in) :: pRib    ! Bulk Richardson number
      REAL(wp), INTENT(in) :: pChn    ! neutral heat transfer coefficient
      REAL(wp), INTENT(in) :: pz0     ! roughness length                      [m]
      !!----------------------------------------------------------------------------------
      REAL(wp) :: ztu, zts, zstab
      !!----------------------------------------------------------------------------------
      zstab = 0.5 + SIGN(0.5_wp, pRib) ; ! Unstable (Ri<0) => zstab = 0 | Stable (Ri>0) => zstab = 1
      !
      ztu = pRib / ( 1._wp + 3._wp * rc2_louis * pChn * SQRT( ABS(-pRib * ( pzu / pz0 + 1._wp) ) ) )
      zts = pRib / SQRT( ABS( 1._wp + pRib ) )
      !
      f_h_louis_sclr = (1._wp - zstab) *         ( 1._wp - rah_louis * ztu )  &  ! Unstable Eq.(A6)
         &              +       zstab  * 1._wp / ( 1._wp + rah_louis * zts )     ! Stable   Eq.(A7)  !#LB: in paper it's "ram_louis" and not "rah_louis" typo or what????
      !
   END FUNCTION f_h_louis_sclr

   FUNCTION f_h_louis_vctr( pzu, pRib, pChn, pz0 )

      REAL(wp), DIMENSION(jpi,jpj)             :: f_h_louis_vctr
      REAL(wp),                     INTENT(in) :: pzu
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pRib
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pChn
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pz0
      INTEGER  :: ji, jj

      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         f_h_louis_vctr(ji,jj) = f_h_louis_sclr( pzu, pRib(ji,jj), pChn(ji,jj), pz0(ji,jj) )
      END_2D

   END FUNCTION f_h_louis_vctr


   FUNCTION UN10_from_ustar( pzu, pUzu, pus, ppsi )
      !!----------------------------------------------------------------------------------
      !!  Provides the neutral-stability wind speed at 10 m
      !!----------------------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj)             :: UN10_from_ustar  !: neutral stability wind speed at 10m [m/s]
      REAL(wp),                     INTENT(in) :: pzu   !: measurement heigh of wind speed   [m]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pUzu  !: bulk wind speed at height pzu m   [m/s]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pus   !: friction velocity                 [m/s]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: ppsi !: "Psi_m(pzu/L)" stability correction profile for momentum []
      !!----------------------------------------------------------------------------------
      UN10_from_ustar(:,:) = pUzu(:,:) - pus(:,:)/vkarmn * ( LOG(pzu/10._wp) - ppsi(:,:) )
      !!
   END FUNCTION UN10_from_ustar


   FUNCTION UN10_from_CD( pzu, pUb, pCd, ppsi )
      !!----------------------------------------------------------------------------------
      !!  Provides the neutral-stability wind speed at 10 m
      !!----------------------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj)             :: UN10_from_CD  !: [m/s]
      REAL(wp),                     INTENT(in) :: pzu  !: measurement heigh of bulk wind speed
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pUb  !: bulk wind speed at height pzu m   [m/s]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pCd  !: drag coefficient
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: ppsi !: "Psi_m(pzu/L)" stability correction profile for momentum []
      !!----------------------------------------------------------------------------------
      !! Reminder: UN10 = u*/vkarmn * log(10/z0)
      !!     and: u* = sqrt(Cd) * Ub
      !!                                  u*/vkarmn * log(   10   /       z0    )
      UN10_from_CD(:,:) = SQRT(pCd(:,:))*pUb/vkarmn * LOG( 10._wp / z0_from_Cd( pzu, pCd(:,:), ppsi=ppsi(:,:) ) )
      !!
   END FUNCTION UN10_from_CD


   FUNCTION z0tq_LKB( iflag, pRer, pz0 )
      !!---------------------------------------------------------------------------------
      !!       ***  FUNCTION z0tq_LKB  ***
      !!
      !! ** Purpose : returns the "temperature/humidity roughness lengths"
      !!              * iflag==1 => temperature => returns: z_{0t}
      !!              * iflag==2 => humidity    => returns: z_{0q}
      !!              from roughness reynold number "pRer" (i.e. [z_0 u*]/Nu_{air})
      !!              between 0 and 1000. Out of range "pRer" indicated by prt=-999.
      !!              and roughness length (for momentum)
      !!
      !!              Based on Liu et al. (1979) JAS 36 1722-1723s
      !!
      !!              Note: this is what is used into COARE 2.5 to estimate z_{0t} and z_{0q}
      !!
      !! ** Author: L. Brodeau, April 2020 / AeroBulk (https://github.com/brodeau/aerobulk/)
      !!----------------------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj)             :: z0tq_LKB
      INTEGER,                      INTENT(in) :: iflag     !: 1 => dealing with temperature; 2 => dealing with humidity
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pRer      !: roughness Reynolds number  [z_0 u*]/Nu_{air}
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pz0       !: roughness length (for momentum) [m]
      !-------------------------------------------------------------------
      ! Scalar Re_r relation from Liu et al.
      REAL(wp), DIMENSION(8,2), PARAMETER :: &
         & XA = RESHAPE( (/ 0.177, 1.376, 1.026, 1.625, 4.661, 34.904, 1667.19, 5.88e5,  &
         &                  0.292, 1.808, 1.393, 1.956, 4.994, 30.709, 1448.68, 2.98e5 /), (/8,2/) )
      !!
      REAL(wp), DIMENSION(8,2), PARAMETER :: &
         & XB = RESHAPE( (/ 0., 0.929, -0.599, -1.018, -1.475, -2.067, -2.907, -3.935,  &
         &                  0., 0.826, -0.528, -0.870, -1.297, -1.845, -2.682, -3.616 /), (/8,2/) )
      !!
      REAL(wp), DIMENSION(0:8), PARAMETER :: &
         & XRAN = (/ 0., 0.11, 0.825, 3.0, 10.0, 30.0, 100., 300., 1000. /)
      !-------------------------------------------------------------------
      !
      !-------------------------------------------------------------------
      ! Scalar Re_r relation from Moana Wave data.
      !
      !      real*8 A(9,2),B(9,2),RAN(9),pRer,prt
      !      integer iflag
      !      DATA A/0.177,2.7e3,1.03,1.026,1.625,4.661,34.904,1667.19,5.88E5,
      !     &       0.292,3.7e3,1.4,1.393,1.956,4.994,30.709,1448.68,2.98E5/
      !      DATA B/0.,4.28,0,-0.599,-1.018,-1.475,-2.067,-2.907,-3.935,
      !     &       0.,4.28,0,-0.528,-0.870,-1.297,-1.845,-2.682,-3.616/
      !      DATA RAN/0.11,.16,1.00,3.0,10.0,30.0,100.,300.,1000./
      !-------------------------------------------------------------------

      LOGICAL  :: lfound=.FALSE.
      REAL(wp) :: zrr
      INTEGER  :: ji, jj, jm

      z0tq_LKB(:,:) = -999._wp

      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )

      zrr    = pRer(ji,jj)
      lfound = .FALSE.

      IF( (zrr > 0.).AND.(zrr < 1000.) ) THEN
         jm = 0
         DO WHILE ( .NOT. lfound )
            jm = jm + 1
            lfound = ( (zrr > XRAN(jm-1)) .AND. (zrr <= XRAN(jm)) )
         END DO

         z0tq_LKB(ji,jj) = XA(jm,iflag)*zrr**XB(jm,iflag) * pz0(ji,jj)/zrr

      END IF

      END_2D

      z0tq_LKB(:,:) = MIN( MAX(ABS(z0tq_LKB(:,:)), 1.E-9) , 0.05_wp )

   END FUNCTION z0tq_LKB



END MODULE sbc_phy
