MODULE sbcblk_skin_coare
   !!======================================================================
   !!                   ***  MODULE  sbcblk_skin_coare  ***
   !!
   !!   Module that gathers the cool-skin and warm-layer parameterization used
   !!   in the COARE family of bulk parameterizations.
   !!
   !!   Based on the last update for version COARE 3.6 (Fairall et al., 2019)
   !!
   !!   Module 'sbcblk_skin_coare' also maintained and developed in AeroBulk (as
   !!            'mod_skin_coare')
   !!    (https://github.com/brodeau/aerobulk)   !!
   !! ** Author: L. Brodeau, November 2019 / AeroBulk (https://github.com/brodeau/aerobulk)
   !!----------------------------------------------------------------------
   !! History :  4.0  !  2019-11  (L.Brodeau)   Original code
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE phycst          ! physical constants
   USE sbc_oce         ! Surface boundary condition: ocean fields

   USE sbc_phy         ! Catalog of functions for physical/meteorological parameters in the marine boundary layer

   USE sbcdcy          !#LB: to know hour of dawn and dusk: rdawn_dcy and rdusk_dcy (needed in WL_COARE)

   USE lib_mpp         ! distribued memory computing library
   USE in_out_manager  ! I/O manager
   USE lib_fortran     ! to use key_nosignedzero

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: CS_COARE, WL_COARE
   !! * Substitutions
#  include "do_loop_substitute.h90"

   !! Cool-skin related parameters:
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:), PUBLIC :: dT_cs !: dT due to cool-skin effect
   !                                                      ! => temperature difference between air-sea interface (z=0)
   !                                                      !    and right below viscous layer (z=delta)

   !! Warm-layer related parameters:
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:), PUBLIC :: dT_wl !: dT due to warm-layer effect
   !                                                      ! => difference between "almost surface (right below
   !                                                      !    viscous layer, z=delta) and depth of bulk SST (z=gdept_1d(1))
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:), PUBLIC :: Hz_wl !: depth (aka thickness) of warm-layer [m]
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:), PUBLIC :: Qnt_ac !: time integral / accumulated heat stored by the warm layer
   !                                                      !         Qxdt => [J/m^2] (reset to zero every midnight)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:), PUBLIC :: Tau_ac !: time integral / accumulated momentum
   !                                                      !         Tauxdt => [N.s/m^2] (reset to zero every midnight)

   REAL(wp), PARAMETER, PUBLIC :: Hwl_max = 20._wp    !: maximum depth of warm layer (adjustable)
   !
   REAL(wp), PARAMETER :: rich   = 0.65_wp   !: critical Richardson number
   !
   REAL(wp), PARAMETER :: zfr0   = 0.5_wp    !: initial value of solar flux absorption
   !
   !!----------------------------------------------------------------------
CONTAINS


   SUBROUTINE CS_COARE( pQsw, pQnsol, pustar, pSST, pQlat )
      !!---------------------------------------------------------------------
      !!
      !! Cool-skin parameterization, based on Fairall et al., 1996,
      !! revisited for COARE 3.6 (Fairall et al., 2019)
      !!
      !! Fairall, C. W., Bradley, E. F., Godfrey, J. S., Wick, G. A.,
      !! Edson, J. B., and Young, G. S. ( 1996), Cool‐skin and warm‐layer
      !! effects on sea surface temperature, J. Geophys. Res., 101( C1), 1295-1308,
      !! doi:10.1029/95JC03190.
      !!
      !!------------------------------------------------------------------
      !!
      !!  **   INPUT:
      !!     *pQsw*       surface net solar radiation into the ocean     [W/m^2] => >= 0 !
      !!     *pQnsol*     surface net non-solar heat flux into the ocean [W/m^2] => normally < 0 !
      !!     *pustar*     friction velocity u*                           [m/s]
      !!     *pSST*       bulk SST (taken at depth gdept_1d(1))          [K]
      !!     *pQlat*      surface latent heat flux                       [K]
      !!------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pQsw   ! net solar a.k.a shortwave radiation into the ocean (after albedo) [W/m^2]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pQnsol ! non-solar heat flux to the ocean [W/m^2]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pustar  ! friction velocity, temperature and humidity (u*,t*,q*)
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pSST ! bulk SST [K]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pQlat  ! latent heat flux [W/m^2]
      !!---------------------------------------------------------------------
      INTEGER  :: ji, jj, jc
      REAL(wp) :: zQabs, zdlt, zfr, zalfa, zqlat, zus
      !!---------------------------------------------------------------------
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )

         zQabs = pQnsol(ji,jj) ! first guess of heat flux absorbed within the viscous sublayer of thicknes delta,
         !                     !   => we DO not miss a lot assuming 0 solar flux absorbed in the tiny layer of thicknes zdlt...

         zalfa = alpha_sw(pSST(ji,jj)) ! (crude) thermal expansion coefficient of sea-water [1/K]
         zqlat = pQlat(ji,jj)
         zus   = pustar(ji,jj)


         zdlt = delta_skin_layer( zalfa, zQabs, zqlat, zus )

         DO jc = 1, 4 ! because implicit in terms of zdlt...
            zfr = MAX( 0.137_wp + 11._wp*zdlt &
               &       - 6.6E-5_wp/zdlt*(1._wp - EXP(-zdlt/8.E-4_wp)) &
               &      , 0.01_wp ) ! Solar absorption, Eq.16 (Fairall al. 1996b)
            !                  !LB: why 0.065 and not 0.137 like in the paper??? Beljaars & Zeng use 0.065, not 0.137 !
            zQabs = pQnsol(ji,jj) + zfr*pQsw(ji,jj)
            zdlt = delta_skin_layer( zalfa, zQabs, zqlat, zus )
         END DO

         dT_cs(ji,jj) = zQabs*zdlt/rk0_w   ! temperature increment, yes dT_cs can actually > 0, if Qabs > 0 (rare but possible!)

      END_2D

   END SUBROUTINE CS_COARE


   SUBROUTINE WL_COARE( pQsw, pQnsol, pTau, pSST, iwait )
      !!---------------------------------------------------------------------
      !!
      !!  Warm-Layer scheme according to COARE 3.6 (Fairall et al, 2019)
      !!     ------------------------------------------------------------------
      !!
      !!  **   INPUT:
      !!     *pQsw*       surface net solar radiation into the ocean     [W/m^2] => >= 0 !
      !!     *pQnsol*     surface net non-solar heat flux into the ocean [W/m^2] => normally < 0 !
      !!     *pTau*       surface wind stress                            [N/m^2]
      !!     *pSST*       bulk SST  (taken at depth gdept_1d(1))         [K]
      !!     *iwait*      if /= 0 then wait before updating accumulated fluxes, we are within a converging itteration loop...
      !!---------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pQsw     ! surface net solar radiation into the ocean [W/m^2]     => >= 0 !
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pQnsol   ! surface net non-solar heat flux into the ocean [W/m^2] => normally < 0 !
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pTau     ! wind stress [N/m^2]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pSST     ! bulk SST at depth gdept_1d(1) [K]
      INTEGER ,                     INTENT(in)  :: iwait    ! if /= 0 then wait before updating accumulated fluxes
      !!
      INTEGER :: ji,jj
      !
      REAL(wp) :: zdTwl, zHwl, zQabs, zfr
      REAL(wp) :: zqac, ztac
      REAL(wp) :: zalfa, zcd1, zcd2, flg
      !!---------------------------------------------------------------------

      REAL(wp) :: ztime, znoon, zmidn
      INTEGER  :: jl

      LOGICAL :: l_exit, l_destroy_wl

      !! INITIALIZATION:
      zQabs  = 0._wp       ! total heat flux absorped in warm layer
      zfr    = zfr0        ! initial value of solar flux absorption !#LB: save it and use previous value !!!

      IF( .NOT. ln_dm2dc ) CALL sbc_dcy_param() ! we need to call sbc_dcy_param (sbcdcy.F90) because rdawn_dcy and rdusk_dcy are unkonwn otherwize!

      ztime = REAL(nsec_day,wp)/(24._wp*3600._wp) ! time of current time step since 00:00 for current day (UTC) -> ztime = 0 -> 00:00 / ztime = 0.5 -> 12:00 ...

      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )

         l_exit       = .FALSE.
         l_destroy_wl = .FALSE.

         zdTwl =  dT_wl(ji,jj)                          ! value of previous time step as first guess
         zHwl  = MAX( MIN(Hz_wl(ji,jj),Hwl_max),0.1_wp) !   "                  "           "

         zqac = Qnt_ac(ji,jj) ! previous time step Qnt_ac
         ztac = Tau_ac(ji,jj)

         !*****  variables for warm layer  ***
         zalfa = alpha_sw( pSST(ji,jj) ) ! (crude) thermal expansion coefficient of sea-water [1/K] (SST accurate enough!)

         zcd1 = SQRT(2._wp*rich*rCp0_w/(zalfa*grav*rho0_w))        !mess-o-constants 1
         zcd2 = SQRT(2._wp*zalfa*grav/(rich*rho0_w))/(rCp0_w**1.5) !mess-o-constants 2


         znoon = MOD( 0.5_wp*(rdawn_dcy(ji,jj)+rdusk_dcy(ji,jj)), 1._wp )   ! 0<rnoon<1. => rnoon*24 = UTC time of local noon
         zmidn = MOD(              znoon-0.5_wp                 , 1._wp )
         zmidn = MOD( zmidn + 0.125_wp , 1._wp ) ! 3 hours past the local midnight

         IF( (ztime >= zmidn) .AND. (ztime < rdawn_dcy(ji,jj)) ) THEN
            ! Dawn reset to 0!
            l_exit       = .TRUE.
            l_destroy_wl = .TRUE.
         ENDIF

         IF( .NOT. l_exit ) THEN
            !! Initial test on initial guess of absorbed heat flux in warm-layer:
            zQabs = frac_solar_abs(zHwl)*pQsw(ji,jj) + pQnsol(ji,jj) ! first guess of tot. heat flux absorbed in warm layer
            !                                                        ! => #LB: depends of zfr, which is wild guess... Wrong!!!
            IF( (ABS(zdTwl) < 1.E-6_wp) .AND. (zQabs <= 0._wp) ) THEN
               ! We have not started to build a WL yet (dT==0) and there's no way it can occur now
               ! since zQabs <= 0._wp
               ! => no need to go further
               l_exit = .TRUE.
            ENDIF

         ENDIF

         ! Okay test on updated absorbed flux:
         !#LB: remove??? has a strong influence !!!
         IF( (.NOT. l_exit).AND.(Qnt_ac(ji,jj) + zQabs*rn_Dt <= 0._wp) ) THEN
            l_exit       = .TRUE.
            l_destroy_wl = .TRUE.
         ENDIF


         IF( .NOT. l_exit) THEN

            ! Two possibilities at this point:
            ! 1/ A warm layer already exists (dT>0) but it is cooling down because Qabs<0
            ! 2/ Regardless of WL formed (dT==0 or dT>0), we are in the process to initiate one or warm further it !

            ztac = Tau_ac(ji,jj) + MAX(.002_wp , pTau(ji,jj))*rn_Dt      ! updated momentum integral
            !PRINT *, '#LBD: updated value for Tac=',  REAL(ztac,4)

            !! We update the value of absorbtion and zQabs:
            !! some part is useless if Qsw=0 !!!
            DO jl = 1, 5
               zQabs = frac_solar_abs(zHwl)*pQsw(ji,jj) + pQnsol(ji,jj)
               zqac  = Qnt_ac(ji,jj) + zQabs*rn_Dt ! updated heat absorbed
               IF( zqac <= 0._wp ) EXIT
               zHwl = MAX( MIN( Hwl_max , zcd1*ztac/SQRT(zqac)) , 0.1_wp ) ! Warm-layer depth
            END DO

            IF( zqac <= 0._wp ) THEN
               l_destroy_wl = .TRUE.
               l_exit       = .TRUE.
            ELSE
               zdTwl = zcd2*zqac**1.5/ztac * MAX(zqac/ABS(zqac),0._wp)  !! => IF(zqac>0._wp): zdTwl=zcd2*zqac**1.5/ztac ; ELSE: zdTwl=0. / ! normally: zqac > 0 !
               !PRINT *, '#LBD: updated preliminary value for dT_wl=',  REAL(zdTwl,4)
               ! Warm layer correction
               flg = 0.5_wp + SIGN( 0.5_wp , gdept_1d(1)-zHwl )               ! => 1 when gdept_1d(1)>zHwl (zdTwl = zdTwl) | 0 when gdept_1d(1)<zHwl (zdTwl = zdTwl*gdept_1d(1)/zHwl)
               zdTwl = zdTwl * ( flg + (1._wp-flg)*gdept_1d(1)/zHwl )
            ENDIF

         ENDIF !IF( .NOT. l_exit)

         IF( l_destroy_wl ) THEN
            zdTwl = 0._wp
            zfr   = 0.75_wp
            zHwl  = Hwl_max
            zqac  = 0._wp
            ztac  = 0._wp
         ENDIF

         IF( iwait == 0 ) THEN
            !! Iteration loop within bulk algo is over, time to update what needs to be updated:
            dT_wl(ji,jj)  = zdTwl
            Hz_wl(ji,jj)  = zHwl
            Qnt_ac(ji,jj) = zqac ! Updating Qnt_ac, heat integral
            Tau_ac(ji,jj) = ztac
         ENDIF

      END_2D

   END SUBROUTINE WL_COARE




   FUNCTION delta_skin_layer( palpha, pQd, pQlat, pustar_a )
      !!---------------------------------------------------------------------
      !! Computes the thickness (m) of the viscous skin layer.
      !! Based on Fairall et al., 1996
      !!
      !! Fairall, C. W., Bradley, E. F., Godfrey, J. S., Wick, G. A.,
      !! Edson, J. B., and Young, G. S. ( 1996), Cool‐skin and warm‐layer
      !! effects on sea surface temperature, J. Geophys. Res., 101( C1), 1295-1308,
      !! doi:10.1029/95JC03190.
      !!
      !! L. Brodeau, october 2019
      !!---------------------------------------------------------------------
      REAL(wp)                :: delta_skin_layer
      REAL(wp), INTENT(in)    :: palpha   ! thermal expansion coefficient of sea-water (SST accurate enough!)
      REAL(wp), INTENT(in)    :: pQd ! < 0 !!! part of the net heat flux actually absorbed in the WL [W/m^2]
      !                              !  => term "Q + Rs*fs" in eq.6 of Fairall et al. 1996
      REAL(wp), INTENT(in)    :: pQlat    ! latent heat flux [W/m^2]
      REAL(wp), INTENT(in)    :: pustar_a ! friction velocity in the air (u*) [m/s]
      !!---------------------------------------------------------------------
      REAL(wp) :: zusw, zusw2, zlamb, zQd, ztf, ztmp
      !!---------------------------------------------------------------------

      zQd = pQd + 0.026*MIN(pQlat,0._wp)*rCp0_w/rLevap/palpha   ! #LB: Double check sign + division by palpha !!! units are okay!

      ztf = 0.5_wp + SIGN(0.5_wp, zQd)  ! Qabs < 0 => cooling of the viscous layer => ztf = 0 (regular case)
      !                                 ! Qabs > 0 => warming of the viscous layer => ztf = 1 (ex: weak evaporation and strong positive sensible heat flux)
      !
      zusw  = MAX(pustar_a, 1.E-4_wp) * sq_radrw    ! u* in the water
      zusw2 = zusw*zusw
      !
      zlamb = 6._wp*( 1._wp + MAX(palpha*rcst_cs/(zusw2*zusw2)*zQd, 0._wp)**0.75 )**(-1./3.) ! see Eq.(14) in Fairall et al., 1996
      !  => zlamb is not used when Qd > 0, and since rcst_cs < 0, we just use this "MAX" to prevent FPE errors (something_negative)**0.75
      !
      ztmp = rnu0_w/zusw
      delta_skin_layer = (1._wp-ztf) *     zlamb*ztmp           &  ! regular case, Qd < 0, see Eq.(12) in Fairall et al., 1996
         &               +   ztf     * MIN(6._wp*ztmp , 0.007_wp)  ! when Qd > 0
   END FUNCTION delta_skin_layer


   FUNCTION frac_solar_abs( pHwl )
      !!---------------------------------------------------------------------
      !! Fraction of solar heat flux absorbed inside warm layer
      !!---------------------------------------------------------------------
      REAL(wp)             :: frac_solar_abs
      REAL(wp), INTENT(in) :: pHwl   ! thickness of warm-layer [m]
      !!---------------------------------------------------------------------
      frac_solar_abs = 1._wp - ( 0.28*0.014  *(1._wp - EXP(-pHwl/0.014)) &
         &                       + 0.27*0.357*(1._wp - EXP(-pHwl/0.357)) &
         &                       + 0.45*12.82*(1-EXP(-pHwl/12.82)) ) / pHwl
   END FUNCTION frac_solar_abs

   !!======================================================================
END MODULE sbcblk_skin_coare
