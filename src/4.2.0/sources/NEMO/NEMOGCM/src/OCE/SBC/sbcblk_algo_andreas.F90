!!! TO DO: consistent psi_m and psi_h needed!!! For now is those of NCAR !!!
!!
MODULE sbcblk_algo_andreas
   !!======================================================================
   !!                   ***  MODULE  sbcblk_algo_andreas  ***
   !! Computes:
   !!   * bulk transfer coefficients C_D, C_E and C_H
   !!   * air temp. and spec. hum. adjusted from zt (2m) to zu (10m) if needed
   !!   * the effective bulk wind speed at 10m Ubzu
   !!  according to Andreas et al. (2015)
   !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   !!       Andreas, E.L., Mahrt, L. and Vickers, D. (2015),
   !!       An improved bulk air–sea surface flux algorithm,
   !!       including spray‐mediated transfer.
   !!       Q.J.R. Meteorol. Soc., 141: 642-654. doi:10.1002/qj.2424
   !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   !!
   !!   * bulk transfer coefficients C_D, C_E and C_H
   !!   * air temp. and spec. hum. adjusted from zt (2m) to zu (10m) if needed
   !!   * the effective bulk wind speed at z=zu: Ubzu
   !!   => all these are used in bulk formulas in sbcblk.F90
   !!
   !!    Using the bulk formulation/param. of Large & Yeager 2008
   !!
   !!       Routine turb_andreas maintained and developed in AeroBulk
   !!                     (https://github.com/brodeau/aerobulk/)
   !!
   !! ** Author: L. Brodeau, August 2020 / AeroBulk (https://github.com/brodeau/aerobulk)
   !!----------------------------------------------------------------------
   !! History :  4.x  !  2020-08  (L.Brodeau)   Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   USE dom_oce         ! ocean space and time domain
   USE phycst          ! physical constants
   USE sbc_phy         ! Catalog of functions for physical/meteorological parameters in the marine boundary layer

   IMPLICIT NONE
   PRIVATE

   !! Important (Brodeau fix):
   REAL(wp), PARAMETER :: rRi_max = 0.15_wp   ! Bulk Ri above which the algorithm fucks up!
   !                                          ! (increasing (>0) Ri means that surface layer increasingly stable and/or wind increasingly weak)
   REAL(wp), PARAMETER :: rCs_min = 0.35E-3_wp ! minimum value to tolarate for CE and CH ! Must be larger than "Cx_min" !!!

   PUBLIC :: TURB_ANDREAS, psi_m_andreas, psi_h_andreas

   !! * Substitutions
#  include "do_loop_substitute.h90"

   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE turb_andreas( zt, zu, sst, t_zt, ssq, q_zt, U_zu, &
      &                     Cd, Ch, Ce, t_zu, q_zu, Ubzu,       &
      &                     nb_iter, CdN, ChN, CeN               )
      !!----------------------------------------------------------------------------------
      !!                      ***  ROUTINE  turb_andreas  ***
      !!
      !! ** Purpose :   Computes turbulent transfert coefficients of surface
      !!                fluxes according to Large & Yeager (2004) and Large & Yeager (2008)
      !!                If relevant (zt /= zu), adjust temperature and humidity from height zt to zu
      !!                Returns the effective bulk wind speed at zu to be used in the bulk formulas
      !!
      !! INPUT :
      !! -------
      !!    *  zt   : height for temperature and spec. hum. of air            [m]
      !!    *  zu   : height for wind speed (usually 10m)                     [m]
      !!    *  sst  : bulk SST                                                [K]
      !!    *  t_zt : potential air temperature at zt                         [K]
      !!    *  ssq  : specific humidity at saturation at SST                  [kg/kg]
      !!    *  q_zt : specific humidity of air at zt                          [kg/kg]
      !!    *  U_zu : scalar wind speed at zu                                 [m/s]
      !!
      !! OUTPUT :
      !! --------
      !!    *  Cd     : drag coefficient
      !!    *  Ch     : sensible heat coefficient
      !!    *  Ce     : evaporation coefficient
      !!    *  t_zu   : pot. air temperature adjusted at wind height zu       [K]
      !!    *  q_zu   : specific humidity of air        //                    [kg/kg]
      !!    *  Ubzu   : bulk wind speed at zu                                 [m/s]
      !!
      !!
      !! ** Author: L. Brodeau, June 2019 / AeroBulk (https://github.com/brodeau/aerobulk/)
      !!----------------------------------------------------------------------------------
      REAL(wp), INTENT(in   )                     ::   zt       ! height for t_zt and q_zt                    [m]
      REAL(wp), INTENT(in   )                     ::   zu       ! height for U_zu                             [m]
      REAL(wp), INTENT(in   ), DIMENSION(jpi,jpj) ::   sst      ! sea surface temperature                [Kelvin]
      REAL(wp), INTENT(in   ), DIMENSION(jpi,jpj) ::   t_zt     ! potential air temperature              [Kelvin]
      REAL(wp), INTENT(in   ), DIMENSION(jpi,jpj) ::   ssq      ! sea surface specific humidity           [kg/kg]
      REAL(wp), INTENT(in   ), DIMENSION(jpi,jpj) ::   q_zt     ! specific air humidity at zt             [kg/kg]
      REAL(wp), INTENT(in   ), DIMENSION(jpi,jpj) ::   U_zu     ! relative wind module at zu                [m/s]
      REAL(wp), INTENT(  out), DIMENSION(jpi,jpj) ::   Cd       ! transfer coefficient for momentum         (tau)
      REAL(wp), INTENT(  out), DIMENSION(jpi,jpj) ::   Ch       ! transfer coefficient for sensible heat (Q_sens)
      REAL(wp), INTENT(  out), DIMENSION(jpi,jpj) ::   Ce       ! transfert coefficient for evaporation   (Q_lat)
      REAL(wp), INTENT(  out), DIMENSION(jpi,jpj) ::   t_zu     ! pot. air temp. adjusted at zu               [K]
      REAL(wp), INTENT(  out), DIMENSION(jpi,jpj) ::   q_zu     ! spec. humidity adjusted at zu           [kg/kg]
      REAL(wp), INTENT(  out), DIMENSION(jpi,jpj) ::   Ubzu    ! bulk wind speed at zu                     [m/s]
      !
      INTEGER , INTENT(in   ), OPTIONAL                     :: nb_iter  ! number of iterations
      REAL(wp), INTENT(  out), OPTIONAL, DIMENSION(jpi,jpj) ::   CdN
      REAL(wp), INTENT(  out), OPTIONAL, DIMENSION(jpi,jpj) ::   ChN
      REAL(wp), INTENT(  out), OPTIONAL, DIMENSION(jpi,jpj) ::   CeN
      !
      INTEGER :: nbit, jit                    ! iterations...
      LOGICAL :: l_zt_equal_zu = .FALSE.      ! if q and t are given at same height as U
      !!
      REAL(wp), DIMENSION(jpi,jpj) ::   u_star, t_star, q_star
      REAL(wp), DIMENSION(jpi,jpj) ::   z0       ! roughness length (momentum) [m]
      REAL(wp), DIMENSION(jpi,jpj) ::   UN10     ! Neutral wind speed at zu [m/s]
      REAL(wp), DIMENSION(jpi,jpj) ::   zeta_u   ! stability parameter at height zu
      REAL(wp), DIMENSION(jpi,jpj) ::   ztmp0, ztmp1, ztmp2
      REAL(wp), DIMENSION(jpi,jpj) ::   RiB       ! square root of Cd
      !!
      !!----------------------------------------------------------------------------------
      nbit = nb_iter0
      IF( PRESENT(nb_iter) ) nbit = nb_iter

      l_zt_equal_zu = ( ABS(zu - zt) < 0.01_wp ) ! testing "zu == zt" is risky with double precision

      Ubzu = MAX( 0.25_wp , U_zu )   !  relative wind speed at zu (normally 10m), we don't want to fall under 0.5 m/s

      !! First guess:
      UN10 = Ubzu
      Cd   = 1.1E-3_wp
      Ch   = 1.1E-3_wp
      Ce   = 1.1E-3_wp
      t_zu = t_zt
      q_zu = q_zt

      !! First guess of turbulent scales for scalars:
      ztmp0  = SQRT(Cd)
      t_star = Ch/ztmp0*(t_zu - sst) ! theta*
      q_star = Ce/ztmp0*(q_zu - ssq) ! q*

      ! Bulk Richardson number:
      RiB(:,:) = Ri_bulk( zu, sst, t_zu, ssq, q_zu, Ubzu )


      !! ITERATION BLOCK
      DO jit = 1, nbit

         WHERE ( RiB < rRi_max )
            !! Normal condition case:
            u_star = U_STAR_ANDREAS( UN10 )
         ELSEWHERE
            !! Extremely stable + weak wind !!!
            !!  => for we force u* to be consistent with minimum value for CD:
            !!  (otherwize algorithm becomes nonsense...)
            u_star = SQRT(Cx_min) * Ubzu     ! Cd does not go below Cx_min !
         ENDWHERE

         !! Stability parameter :
         zeta_u = zu*One_on_L( t_zu, q_zu, u_star, t_star, q_star )   ! zu * 1/L

         !! Drag coefficient:
         ztmp0 = u_star/Ubzu

         Cd = MAX( ztmp0*ztmp0 , Cx_min )

         !! Roughness length:
         z0 = MIN( z0_from_Cd( zu, Cd,  ppsi=psi_m_andreas(zeta_u) ) , z0_sea_max )

         !! z0t and z0q, based on LKB, just like into COARE 2.5:
         ztmp0 = z0 * u_star / visc_air(t_zu) ! Re_r
         ztmp1 = z0tq_LKB( 1, ztmp0, z0 )     ! z0t
         ztmp2 = z0tq_LKB( 2, ztmp0, z0 )     ! z0q

         !! Turbulent scales at zu :
         ztmp0 = psi_h_andreas(zeta_u)  ! lolo: zeta_u for scalars???
         t_star  = (t_zu - sst)*vkarmn/(LOG(zu) - LOG(ztmp1) - ztmp0)  ! theta* (ztmp1 == z0t in rhs term)
         q_star  = (q_zu - ssq)*vkarmn/(LOG(zu) - LOG(ztmp2) - ztmp0)  !   q*   (ztmp2 == z0q in rhs term)

         IF( (.NOT. l_zt_equal_zu).AND.( jit > 1 ) ) THEN
            !! Re-updating temperature and humidity at zu if zt /= zu:
            ztmp0 = zeta_u/zu*zt   ! zeta_t
            ztmp0 = LOG(zt/zu) + psi_h_andreas(zeta_u) - psi_h_andreas(ztmp0)
            t_zu = t_zt - t_star/vkarmn*ztmp0
            q_zu = q_zt - q_star/vkarmn*ztmp0
            RiB  = Ri_bulk( zu, sst, t_zu, ssq, q_zu, Ubzu ) !LOLO
         ENDIF

         !! Update neutral-stability wind at zu:
         UN10 = MAX( 0.1_wp , UN10_from_ustar( zu, Ubzu, u_star, psi_m_andreas(zeta_u) ) ) ! UN10

      END DO !DO jit = 1, nbit

      ! Compute transfer coefficients at zu:
      ztmp0 = u_star/Ubzu

      Cd = MAX( ztmp0*ztmp0 , Cx_min )   ! the earlier use of Cx_min on u* should make use of Cx_min here unnecessary!

      ztmp1 = t_zu - sst ;  ztmp1 = SIGN( MAX(ABS(ztmp1),1.E-6_wp), ztmp1 )  ! dt_zu
      ztmp2 = q_zu - ssq ;  ztmp2 = SIGN( MAX(ABS(ztmp2),1.E-9_wp), ztmp2 )  ! dq_zu
      Ch   = MAX( ztmp0*t_star/ztmp1 , rCs_min )
      Ce   = MAX( ztmp0*q_star/ztmp2 , rCs_min )

      !! Neutral-stability coefficients:
      ztmp0 = 1._wp/LOG(zu/z0)
      ztmp1 = z0 * u_star / visc_air(t_zu)  ! Re_r
      
      IF(PRESENT(CdN)) CdN = vkarmn2*ztmp0*ztmp0
      IF(PRESENT(ChN)) ChN = vkarmn2*ztmp0/LOG(zu/z0tq_LKB( 1, ztmp1, z0 ))
      IF(PRESENT(CeN)) CeN = vkarmn2*ztmp0/LOG(zu/z0tq_LKB( 2, ztmp1, z0 ))
      
   END SUBROUTINE turb_andreas


   FUNCTION U_STAR_ANDREAS( pun10 )
      !!----------------------------------------------------------------------------------
      !! Estimate of the friction velocity as a function of the neutral-stability wind
      !! speed at at 10m
      !!
      !! Origin: Eq.(2.2) of Andreas et al. (2015)
      !!
      !! ** Author: L. Brodeau, April 2020 / AeroBulk (https://github.com/brodeau/aerobulk/)
      !!----------------------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pun10          !: neutral-stability scalar wind speed at 10m (m/s)
      REAL(wp), DIMENSION(jpi,jpj)             :: u_star_andreas !: friction velocity    [m/s]
      !
      INTEGER  ::     ji, jj ! dummy loop indices
      REAL(wp) :: za, zt, zw ! local scalars
      !!----------------------------------------------------------------------------------
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            zw  = pun10(ji,jj)
            za = zw - 8.271_wp
            zt = za + SQRT( 0.12_wp*za*za + 0.181_wp )
            u_star_andreas(ji,jj) =   0.239_wp + 0.0433_wp * zt
      END_2D
   END FUNCTION U_STAR_ANDREAS


   FUNCTION psi_m_andreas( pzeta )
      !!----------------------------------------------------------------------------------
      !!      Universal profile stability function for momentum
      !!  TO DO !!!!!!!!!!!!!!!!!!!!!
      !! LOLO: paper says Paulson 1970 when unstable and Grachev et al 2007 for STABLE
      !!
      !! pzeta : stability paramenter, z/L where z is altitude measurement
      !!         and L is M-O length
      !!
      !! ** Author: L. Brodeau, April 2020 / AeroBulk (https://github.com/brodeau/aerobulk/)
      !!----------------------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj) :: psi_m_andreas
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pzeta
      !
      REAL(wp), PARAMETER :: zam  = 5._wp      ! a_m (just below Eq.(9b)
      REAL(wp), PARAMETER :: zbm = zam/6.5_wp  ! b_m (just below Eq.(9b)
      !
      REAL(wp), PARAMETER :: z1o3 = 1._wp/3._wp
      REAL(wp), PARAMETER :: zsr3 = SQRT(3._wp)
      !
      INTEGER  ::   ji, jj    ! dummy loop indices
      REAL(wp) :: zta, zx2, zx, zpsi_unst, zbbm, zpsi_stab,  zstab   ! local scalars
      !!----------------------------------------------------------------------------------
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            !
            zta = MIN( pzeta(ji,jj) , 15._wp ) !! Very stable conditions (L positif and big!)
            !
            !! *** Unstable: Paulson (1970): #LOLO: DOUBLE CHECK IT IS PAULSON!!!!!
            zx2 = SQRT( ABS(1._wp - 16._wp*zta) )  ! (1 - 16z)^0.5
            zx2 = MAX( zx2 , 1._wp )
            zx  = SQRT(zx2)                          ! (1 - 16z)^0.25
            zpsi_unst = 2._wp*LOG(ABS( (1._wp + zx )*0.5_wp ))   &
               &            + LOG(ABS( (1._wp + zx2)*0.5_wp ))   &
               &          - 2._wp*ATAN(zx) + rpi*0.5_wp
            !
            !! *** Stable: Grachev et al 2007 (SHEBA) [Eq.(12) Grachev et al 2007]:
            zx   = ABS(1._wp + zta)**z1o3
            zbbm = ABS( (1._wp - zbm)/zbm )**z1o3 ! B_m
            !
            zpsi_stab = -3.*zam/zbm*(zx - 1._wp) + zam*zbbm/(2.*zbm) * ( &
               &        2.*LOG(ABS( (   zx     +   zbbm         )/(1._wp        +   zbbm   ) )) &
               &         - LOG(ABS( (zx*zx - zx*zbbm + zbbm*zbbm)/(1._wp - zbbm + zbbm*zbbm) )) &
               & + 2.*zsr3*( ATAN( (2.*zx - zbbm)/(zsr3*zbbm) ) - ATAN( (2._wp - zbbm)/(zsr3*zbbm) ) ) )
            !
            !
            zstab = 0.5_wp + SIGN(0.5_wp, zta) ! zta > 0 => zstab = 1
            !
            psi_m_andreas(ji,jj) =       zstab  * zpsi_stab &  ! (zta > 0) Stable
               &              + (1._wp - zstab) * zpsi_unst    ! (zta < 0) Unstable
            !
      END_2D
   END FUNCTION psi_m_andreas


   FUNCTION psi_h_andreas( pzeta )
      !!----------------------------------------------------------------------------------
      !! Universal profile stability function for temperature and humidity
      !!
      !!    TO DO
      !!       !! LOLO: paper says Paulson 1970 when unstable and Grachev et al 2007 for STABLE
      !!
      !! pzeta : stability paramenter, z/L where z is altitude measurement
      !!         and L is M-O length
      !!
      !! ** Author: L. Brodeau, June 2016 / AeroBulk (https://github.com/brodeau/aerobulk/)
      !!----------------------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj) :: psi_h_andreas
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pzeta
      !
      REAL(wp), PARAMETER ::  zah = 5._wp       ! a_h (just below Eq.(9b)
      REAL(wp), PARAMETER ::  zbh = 5._wp       ! b_h (just below Eq.(9b)
      REAL(wp), PARAMETER ::  zch = 3._wp       ! c_h (just below Eq.(9b)
      REAL(wp), PARAMETER :: zbbh = SQRT(5._wp) ! B_h (just below Eq.(13)
      !
      INTEGER  ::   ji, jj     ! dummy loop indices
      REAL(wp) :: zta, zz, zx2, zpsi_unst, zpsi_stab, zstab  ! local scalars
      !!----------------------------------------------------------------------------------
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            !
            zta = MIN( pzeta(ji,jj) , 15._wp ) !! Very stable conditions (L positif and large!)
            !
            !! *** Unstable: Paulson (1970): #LOLO: DOUBLE CHECK IT IS PAULSON!!!!!
            zx2 = SQRT( ABS(1._wp - 16._wp*zta) )  ! (1 -16z)^0.5
            zx2 = MAX( zx2 , 1._wp )
            zpsi_unst = 2._wp*LOG( 0.5_wp*(1._wp + zx2) )
            !
            !! *** Stable: Grachev et al 2007 (SHEBA) [Eq.(13) Grachev et al 2007]:
            zz = 2.*zta + zch
            zpsi_stab = - 0.5*zbh*LOG(ABS(1._wp + zch*zta + zta*zta)) &
               &        +  (-zah/zbbh + 0.5*zbh*zch/zbbh)  &
               &          *( LOG(ABS((zz  - zbbh)/(zz  + zbbh))) &
               &           - LOG(ABS((zch - zbbh)/(zch + zbbh)))    )
            !
            zstab = 0.5_wp + SIGN(0.5_wp, zta) ! zta > 0 => zstab = 1
            !
            psi_h_andreas(ji,jj) =            zstab  * zpsi_stab &  ! (zta > 0) Stable
               &                   + (1._wp - zstab) * zpsi_unst    ! (zta < 0) Unstable
            !
      END_2D
   END FUNCTION psi_h_andreas

   !!======================================================================
END MODULE sbcblk_algo_andreas
