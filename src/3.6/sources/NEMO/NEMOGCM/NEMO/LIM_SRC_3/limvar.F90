MODULE limvar
   !!======================================================================
   !!                       ***  MODULE limvar ***
   !!                 Different sets of ice model variables 
   !!                   how to switch from one to another
   !!
   !!                 There are three sets of variables
   !!                 VGLO : global variables of the model
   !!                        - v_i (jpi,jpj,jpl)
   !!                        - v_s (jpi,jpj,jpl)
   !!                        - a_i (jpi,jpj,jpl)
   !!                        - t_s (jpi,jpj,jpl)
   !!                        - e_i (jpi,jpj,nlay_i,jpl)
   !!                        - smv_i(jpi,jpj,jpl)
   !!                        - oa_i (jpi,jpj,jpl)
   !!                 VEQV : equivalent variables sometimes used in the model
   !!                        - ht_i(jpi,jpj,jpl)
   !!                        - ht_s(jpi,jpj,jpl)
   !!                        - t_i (jpi,jpj,nlay_i,jpl)
   !!                        ...
   !!                 VAGG : aggregate variables, averaged/summed over all
   !!                        thickness categories
   !!                        - vt_i(jpi,jpj)
   !!                        - vt_s(jpi,jpj)
   !!                        - at_i(jpi,jpj)
   !!                        - et_s(jpi,jpj)  !total snow heat content
   !!                        - et_i(jpi,jpj)  !total ice thermal content 
   !!                        - smt_i(jpi,jpj) !mean ice salinity
   !!                        - tm_i (jpi,jpj) !mean ice temperature
   !!======================================================================
   !! History :   -   ! 2006-01 (M. Vancoppenolle) Original code
   !!            3.4  ! 2011-02 (G. Madec) dynamical allocation
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3'                                      LIM3 sea-ice model
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean parameters
   USE phycst         ! physical constants (ocean directory) 
   USE sbc_oce        ! Surface boundary condition: ocean fields
   USE ice            ! ice variables
   USE thd_ice        ! ice variables (thermodynamics)
   USE dom_ice        ! ice domain
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE wrk_nemo       ! work arrays
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)  

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_var_agg          
   PUBLIC   lim_var_glo2eqv      
   PUBLIC   lim_var_eqv2glo      
   PUBLIC   lim_var_salprof      
   PUBLIC   lim_var_bv           
   PUBLIC   lim_var_salprof1d    
   PUBLIC   lim_var_zapsmall
   PUBLIC   lim_var_itd

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 3.5 , UCL - NEMO Consortium (2011)
   !! $Id: limvar.F90 7814 2017-03-20 16:21:42Z clem $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_var_agg( kn )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_var_agg  ***
      !!
      !! ** Purpose :   aggregates ice-thickness-category variables to all-ice variables
      !!              i.e. it turns VGLO into VAGG
      !! ** Method  :
      !!
      !! ** Arguments : n = 1, at_i vt_i only
      !!                n = 2 everything
      !!
      !! note : you could add an argument when you need only at_i, vt_i
      !!        and when you need everything
      !!------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kn     ! =1 at_i & vt only ; = what is needed
      !
      INTEGER  ::   ji, jj, jk, jl   ! dummy loop indices
      !!------------------------------------------------------------------

      !--------------------
      ! Compute variables
      !--------------------
      ! integrated values
      vt_i (:,:) = SUM( v_i, dim=3 )
      vt_s (:,:) = SUM( v_s, dim=3 )
      at_i (:,:) = SUM( a_i, dim=3 )
      et_s(:,:)  = SUM( SUM( e_s(:,:,:,:), dim=4 ), dim=3 )
      et_i(:,:)  = SUM( SUM( e_i(:,:,:,:), dim=4 ), dim=3 )
      !
      DO jj = 1, jpj
         DO ji = 1, jpi
            ato_i(ji,jj) = MAX( 1._wp - at_i(ji,jj), 0._wp )   ! open water fraction
         END DO
      END DO

      IF( kn > 1 ) THEN
         !
         ! mean ice/snow thickness
         DO jj = 1, jpj
            DO ji = 1, jpi
               rswitch      = MAX( 0._wp , SIGN( 1._wp , at_i(ji,jj) - epsi10 ) ) 
               htm_i(ji,jj) = vt_i(ji,jj) / MAX( at_i(ji,jj) , epsi10 ) * rswitch
               htm_s(ji,jj) = vt_s(ji,jj) / MAX( at_i(ji,jj) , epsi10 ) * rswitch
            ENDDO
         ENDDO

         ! mean temperature (K), salinity and age
         smt_i(:,:) = 0._wp
         tm_i(:,:)  = 0._wp
         tm_su(:,:) = 0._wp
         om_i (:,:) = 0._wp
         DO jl = 1, jpl
            
            DO jj = 1, jpj
               DO ji = 1, jpi
                  rswitch      = MAX( 0._wp , SIGN( 1._wp , at_i(ji,jj) - epsi10 ) )
                  tm_su(ji,jj) = tm_su(ji,jj) + rswitch * ( t_su(ji,jj,jl) - rt0 ) * a_i(ji,jj,jl) / MAX( at_i(ji,jj) , epsi10 )
                  om_i (ji,jj) = om_i (ji,jj) + rswitch *   oa_i(ji,jj,jl)                         / MAX( at_i(ji,jj) , epsi10 )
               END DO
            END DO
            
            DO jk = 1, nlay_i
               DO jj = 1, jpj
                  DO ji = 1, jpi
                     rswitch = MAX( 0._wp , SIGN( 1._wp , vt_i(ji,jj) - epsi10 ) )
                     tm_i(ji,jj)  = tm_i(ji,jj)  + r1_nlay_i * rswitch * ( t_i(ji,jj,jk,jl) - rt0 ) * v_i(ji,jj,jl)  &
                        &            / MAX( vt_i(ji,jj) , epsi10 )
                     smt_i(ji,jj) = smt_i(ji,jj) + r1_nlay_i * rswitch * s_i(ji,jj,jk,jl) * v_i(ji,jj,jl)  &
                        &            / MAX( vt_i(ji,jj) , epsi10 )
                  END DO
               END DO
            END DO
         END DO
         tm_i  = tm_i  + rt0
         tm_su = tm_su + rt0
         !
      ENDIF
      !
   END SUBROUTINE lim_var_agg


   SUBROUTINE lim_var_glo2eqv
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_var_glo2eqv ***
      !!
      !! ** Purpose :   computes equivalent variables as function of global variables 
      !!              i.e. it turns VGLO into VEQV
      !!------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk, jl   ! dummy loop indices
      REAL(wp) ::   zq_i, zaaa, zbbb, zccc, zdiscrim     ! local scalars
      REAL(wp) ::   ztmelts, zq_s, zfac1, zfac2   !   -      -
      !!------------------------------------------------------------------

      !-------------------------------------------------------
      ! Ice thickness, snow thickness, ice salinity, ice age
      !-------------------------------------------------------
      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               rswitch = MAX( 0._wp , SIGN( 1._wp, a_i(ji,jj,jl) - epsi20 ) )   !0 if no ice and 1 if yes
               ht_i(ji,jj,jl) = v_i (ji,jj,jl) / MAX( a_i(ji,jj,jl) , epsi20 ) * rswitch
            END DO
         END DO
      END DO
      ! Force the upper limit of ht_i to always be < hi_max (99 m).
      DO jj = 1, jpj
         DO ji = 1, jpi
            rswitch = MAX( 0._wp , SIGN( 1._wp, ht_i(ji,jj,jpl) - epsi20 ) )
            ht_i(ji,jj,jpl) = MIN( ht_i(ji,jj,jpl) , hi_max(jpl) )
            a_i (ji,jj,jpl) = v_i(ji,jj,jpl) / MAX( ht_i(ji,jj,jpl) , epsi20 ) * rswitch
         END DO
      END DO

      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               rswitch = MAX( 0._wp , SIGN( 1._wp, a_i(ji,jj,jl) - epsi20 ) )   !0 if no ice and 1 if yes
               ht_s(ji,jj,jl) = v_s (ji,jj,jl) / MAX( a_i(ji,jj,jl) , epsi20 ) * rswitch
               o_i(ji,jj,jl)  = oa_i(ji,jj,jl) / MAX( a_i(ji,jj,jl) , epsi20 ) * rswitch
            END DO
         END DO
      END DO
      
      IF(  nn_icesal == 2  )THEN
         DO jl = 1, jpl
            DO jj = 1, jpj
               DO ji = 1, jpi
                  rswitch = MAX( 0._wp , SIGN( 1._wp, v_i(ji,jj,jl) - epsi20 ) )   !0 if no ice and 1 if yes
                  sm_i(ji,jj,jl) = smv_i(ji,jj,jl) / MAX( v_i(ji,jj,jl) , epsi20 ) * rswitch
                  !                                      ! bounding salinity
                  sm_i(ji,jj,jl) = MAX( sm_i(ji,jj,jl), rn_simin )
               END DO
            END DO
         END DO
      ENDIF

      CALL lim_var_salprof      ! salinity profile

      !-------------------
      ! Ice temperatures
      !-------------------
      DO jl = 1, jpl
         DO jk = 1, nlay_i
            DO jj = 1, jpj
               DO ji = 1, jpi
                  !                                                              ! Energy of melting q(S,T) [J.m-3]
                  rswitch = MAX( 0.0 , SIGN( 1.0 , v_i(ji,jj,jl) - epsi20 ) )     ! rswitch = 0 if no ice and 1 if yes
                  zq_i    = rswitch * e_i(ji,jj,jk,jl) / MAX( v_i(ji,jj,jl) , epsi20 ) * REAL(nlay_i,wp) 
                  ztmelts = -tmut * s_i(ji,jj,jk,jl) + rt0              ! Ice layer melt temperature
                  !
                  zaaa       =  cpic                  ! Conversion q(S,T) -> T (second order equation)
                  zbbb       =  ( rcp - cpic ) * ( ztmelts - rt0 ) + zq_i * r1_rhoic - lfus
                  zccc       =  lfus * (ztmelts-rt0)
                  zdiscrim   =  SQRT( MAX(zbbb*zbbb - 4._wp*zaaa*zccc , 0._wp) )
                  t_i(ji,jj,jk,jl) = rt0 + rswitch *( - zbbb - zdiscrim ) / ( 2.0 *zaaa )
                  t_i(ji,jj,jk,jl) = MIN( ztmelts, MAX( rt0 - 100._wp, t_i(ji,jj,jk,jl) ) )  ! -100 < t_i < ztmelts
               END DO
            END DO
         END DO
      END DO

      !--------------------
      ! Snow temperatures
      !--------------------
      zfac1 = 1._wp / ( rhosn * cpic )
      zfac2 = lfus / cpic  
      DO jl = 1, jpl
         DO jk = 1, nlay_s
            DO jj = 1, jpj
               DO ji = 1, jpi
                  !Energy of melting q(S,T) [J.m-3]
                  rswitch = MAX( 0._wp , SIGN( 1._wp , v_s(ji,jj,jl) - epsi20 ) )     ! rswitch = 0 if no ice and 1 if yes
                  zq_s  = rswitch * e_s(ji,jj,jk,jl) / MAX( v_s(ji,jj,jl) , epsi20 ) * REAL(nlay_s,wp)
                  !
                  t_s(ji,jj,jk,jl) = rt0 + rswitch * ( - zfac1 * zq_s + zfac2 )
                  t_s(ji,jj,jk,jl) = MIN( rt0, MAX( rt0 - 100._wp , t_s(ji,jj,jk,jl) ) )     ! -100 < t_s < rt0
               END DO
            END DO
         END DO
      END DO

      !-------------------
      ! Mean temperature
      !-------------------
      ! integrated values
      vt_i (:,:) = SUM( v_i, dim=3 )
      vt_s (:,:) = SUM( v_s, dim=3 )
      at_i (:,:) = SUM( a_i, dim=3 )

      tm_i(:,:) = 0._wp
      DO jl = 1, jpl
         DO jk = 1, nlay_i
            DO jj = 1, jpj
               DO ji = 1, jpi
                  rswitch = MAX( 0._wp , SIGN( 1._wp , vt_i(ji,jj) - epsi10 ) )
                  tm_i(ji,jj) = tm_i(ji,jj) + r1_nlay_i * rswitch * ( t_i(ji,jj,jk,jl) - rt0 ) * v_i(ji,jj,jl)  &
                     &            / MAX( vt_i(ji,jj) , epsi10 )
               END DO
            END DO
         END DO
      END DO
      tm_i = tm_i + rt0
      !
   END SUBROUTINE lim_var_glo2eqv


   SUBROUTINE lim_var_eqv2glo
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_var_eqv2glo ***
      !!
      !! ** Purpose :   computes global variables as function of equivalent variables
      !!                i.e. it turns VEQV into VGLO
      !! ** Method  :
      !!
      !! ** History :  (01-2006) Martin Vancoppenolle, UCL-ASTR
      !!------------------------------------------------------------------
      !
      v_i(:,:,:)   = ht_i(:,:,:) * a_i(:,:,:)
      v_s(:,:,:)   = ht_s(:,:,:) * a_i(:,:,:)
      smv_i(:,:,:) = sm_i(:,:,:) * v_i(:,:,:)
      !
   END SUBROUTINE lim_var_eqv2glo


   SUBROUTINE lim_var_salprof
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_var_salprof ***
      !!
      !! ** Purpose :   computes salinity profile in function of bulk salinity     
      !!
      !! ** Method  : If bulk salinity greater than zsi1, 
      !!              the profile is assumed to be constant (S_inf)
      !!              If bulk salinity lower than zsi0,
      !!              the profile is linear with 0 at the surface (S_zero)
      !!              If it is between zsi0 and zsi1, it is a
      !!              alpha-weighted linear combination of s_inf and s_zero
      !!
      !! ** References : Vancoppenolle et al., 2007
      !!------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk, jl   ! dummy loop index
      REAL(wp) ::   zfac0, zfac1, zsal
      REAL(wp) ::   zswi0, zswi01, zargtemp , zs_zero   
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   z_slope_s, zalpha
      REAL(wp), PARAMETER :: zsi0 = 3.5_wp
      REAL(wp), PARAMETER :: zsi1 = 4.5_wp
      !!------------------------------------------------------------------

      CALL wrk_alloc( jpi, jpj, jpl, z_slope_s, zalpha )

      !---------------------------------------
      ! Vertically constant, constant in time
      !---------------------------------------
      IF(  nn_icesal == 1  )  THEN
         s_i (:,:,:,:) = rn_icesal
         sm_i(:,:,:)   = rn_icesal
      ENDIF

      !-----------------------------------
      ! Salinity profile, varying in time
      !-----------------------------------
      IF(  nn_icesal == 2  ) THEN
         !
         DO jk = 1, nlay_i
            s_i(:,:,jk,:)  = sm_i(:,:,:)
         END DO
         !
         DO jl = 1, jpl                               ! Slope of the linear profile 
            DO jj = 1, jpj
               DO ji = 1, jpi
                  rswitch = MAX( 0._wp , SIGN( 1._wp , ht_i(ji,jj,jl) - epsi20 ) )
                  z_slope_s(ji,jj,jl) = rswitch * 2._wp * sm_i(ji,jj,jl) / MAX( epsi20 , ht_i(ji,jj,jl) )
               END DO
            END DO
         END DO
         !
         zfac0 = 1._wp / ( zsi0 - zsi1 )       ! Weighting factor between zs_zero and zs_inf
         zfac1 = zsi1  / ( zsi1 - zsi0 )
         !
         zalpha(:,:,:) = 0._wp
         DO jl = 1, jpl
            DO jj = 1, jpj
               DO ji = 1, jpi
                  ! zswi0 = 1 if sm_i le zsi0 and 0 otherwise
                  zswi0  = MAX( 0._wp   , SIGN( 1._wp  , zsi0 - sm_i(ji,jj,jl) ) ) 
                  ! zswi01 = 1 if sm_i is between zsi0 and zsi1 and 0 othws 
                  zswi01 = ( 1._wp - zswi0 ) * MAX( 0._wp   , SIGN( 1._wp  , zsi1 - sm_i(ji,jj,jl) ) ) 
                  ! If 2.sm_i GE sss_m then rswitch = 1
                  ! this is to force a constant salinity profile in the Baltic Sea
                  rswitch = MAX( 0._wp , SIGN( 1._wp , 2._wp * sm_i(ji,jj,jl) - sss_m(ji,jj) ) )
                  zalpha(ji,jj,jl) = zswi0  + zswi01 * ( sm_i(ji,jj,jl) * zfac0 + zfac1 )
                  zalpha(ji,jj,jl) = zalpha(ji,jj,jl) * ( 1._wp - rswitch )
               END DO
            END DO
         END DO

         ! Computation of the profile
         DO jl = 1, jpl
            DO jk = 1, nlay_i
               DO jj = 1, jpj
                  DO ji = 1, jpi
                     !                                      ! linear profile with 0 at the surface
                     zs_zero = z_slope_s(ji,jj,jl) * ( REAL(jk,wp) - 0.5_wp ) * ht_i(ji,jj,jl) * r1_nlay_i
                     !                                      ! weighting the profile
                     s_i(ji,jj,jk,jl) = zalpha(ji,jj,jl) * zs_zero + ( 1._wp - zalpha(ji,jj,jl) ) * sm_i(ji,jj,jl)
                     !                                      ! bounding salinity
                     s_i(ji,jj,jk,jl) = MIN( rn_simax, MAX( s_i(ji,jj,jk,jl), rn_simin ) )
                  END DO
               END DO
            END DO
         END DO
         !
      ENDIF ! nn_icesal

      !-------------------------------------------------------
      ! Vertically varying salinity profile, constant in time
      !-------------------------------------------------------

      IF(  nn_icesal == 3  ) THEN      ! Schwarzacher (1959) multiyear salinity profile (mean = 2.30)
         !
         sm_i(:,:,:) = 2.30_wp
         !
         DO jl = 1, jpl
            DO jk = 1, nlay_i
               zargtemp  = ( REAL(jk,wp) - 0.5_wp ) * r1_nlay_i
               zsal =  1.6_wp * (  1._wp - COS( rpi * zargtemp**(0.407_wp/(0.573_wp+zargtemp)) )  )
               s_i(:,:,jk,jl) =  zsal
            END DO
         END DO
         !
      ENDIF ! nn_icesal
      !
      CALL wrk_dealloc( jpi, jpj, jpl, z_slope_s, zalpha )
      !
   END SUBROUTINE lim_var_salprof


   SUBROUTINE lim_var_bv
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_var_bv ***
      !!
      !! ** Purpose :   computes mean brine volume (%) in sea ice
      !!
      !! ** Method  : e = - 0.054 * S (ppt) / T (C)
      !!
      !! References : Vancoppenolle et al., JGR, 2007
      !!------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk, jl   ! dummy loop indices
      !!------------------------------------------------------------------
      !
      bvm_i(:,:)   = 0._wp
      bv_i (:,:,:) = 0._wp
      DO jl = 1, jpl
         DO jk = 1, nlay_i
            DO jj = 1, jpj
               DO ji = 1, jpi
                  rswitch        = ( 1._wp - MAX( 0._wp , SIGN( 1._wp , (t_i(ji,jj,jk,jl) - rt0) + epsi10 ) )  )
                  bv_i(ji,jj,jl) = bv_i(ji,jj,jl) - rswitch * tmut * s_i(ji,jj,jk,jl) * r1_nlay_i  &
                     &                            / MIN( t_i(ji,jj,jk,jl) - rt0, - epsi10 )
               END DO
            END DO
         END DO
         
         DO jj = 1, jpj
            DO ji = 1, jpi
               rswitch      = MAX( 0._wp , SIGN( 1._wp , vt_i(ji,jj) - epsi10 ) )
               bvm_i(ji,jj) = bvm_i(ji,jj) + rswitch * bv_i(ji,jj,jl) * v_i(ji,jj,jl) / MAX( vt_i(ji,jj), epsi10 )
            END DO
         END DO
      END DO
      !
   END SUBROUTINE lim_var_bv


   SUBROUTINE lim_var_salprof1d( kideb, kiut )
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE lim_thd_salprof1d  ***
      !!
      !! ** Purpose :   1d computation of the sea ice salinity profile
      !!                Works with 1d vectors and is used by thermodynamic modules
      !!-------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kideb, kiut   ! thickness category index
      !
      INTEGER  ::   ji, jk    ! dummy loop indices
      INTEGER  ::   ii, ij    ! local integers
      REAL(wp) ::   zfac0, zfac1, zargtemp, zsal   ! local scalars
      REAL(wp) ::   zalpha, zswi0, zswi01, zs_zero              !   -      -
      !
      REAL(wp), POINTER, DIMENSION(:) ::   z_slope_s
      REAL(wp), PARAMETER :: zsi0 = 3.5_wp
      REAL(wp), PARAMETER :: zsi1 = 4.5_wp
      !!---------------------------------------------------------------------

      CALL wrk_alloc( jpij, z_slope_s )

      !---------------------------------------
      ! Vertically constant, constant in time
      !---------------------------------------
      IF( nn_icesal == 1 )   s_i_1d(:,:) = rn_icesal

      !------------------------------------------------------
      ! Vertically varying salinity profile, varying in time
      !------------------------------------------------------

      IF(  nn_icesal == 2  ) THEN
         !
         DO ji = kideb, kiut          ! Slope of the linear profile zs_zero
            rswitch = MAX( 0._wp , SIGN( 1._wp , ht_i_1d(ji) - epsi20 ) )
            z_slope_s(ji) = rswitch * 2._wp * sm_i_1d(ji) / MAX( epsi20 , ht_i_1d(ji) )
         END DO

         ! Weighting factor between zs_zero and zs_inf
         !---------------------------------------------
         zfac0 = 1._wp / ( zsi0 - zsi1 )
         zfac1 = zsi1 / ( zsi1 - zsi0 )
         DO jk = 1, nlay_i
            DO ji = kideb, kiut
               ii =  MOD( npb(ji) - 1 , jpi ) + 1
               ij =     ( npb(ji) - 1 ) / jpi + 1
               ! zswi0 = 1 if sm_i le zsi0 and 0 otherwise
               zswi0  = MAX( 0._wp , SIGN( 1._wp  , zsi0 - sm_i_1d(ji) ) ) 
               ! zswi01 = 1 if sm_i is between zsi0 and zsi1 and 0 othws 
               zswi01 = ( 1._wp - zswi0 ) * MAX( 0._wp , SIGN( 1._wp , zsi1 - sm_i_1d(ji) ) ) 
               ! if 2.sm_i GE sss_m then rswitch = 1
               ! this is to force a constant salinity profile in the Baltic Sea
               rswitch = MAX( 0._wp , SIGN( 1._wp , 2._wp * sm_i_1d(ji) - sss_m(ii,ij) ) )
               !
               zalpha = (  zswi0 + zswi01 * ( sm_i_1d(ji) * zfac0 + zfac1 )  ) * ( 1._wp - rswitch )
               !
               zs_zero = z_slope_s(ji) * ( REAL(jk,wp) - 0.5_wp ) * ht_i_1d(ji) * r1_nlay_i
               ! weighting the profile
               s_i_1d(ji,jk) = zalpha * zs_zero + ( 1._wp - zalpha ) * sm_i_1d(ji)
               ! bounding salinity
               s_i_1d(ji,jk) = MIN( rn_simax, MAX( s_i_1d(ji,jk), rn_simin ) )
            END DO 
         END DO 

      ENDIF 

      !-------------------------------------------------------
      ! Vertically varying salinity profile, constant in time
      !-------------------------------------------------------

      IF( nn_icesal == 3 ) THEN      ! Schwarzacher (1959) multiyear salinity profile (mean = 2.30)
         !
         sm_i_1d(:) = 2.30_wp
         !
         DO jk = 1, nlay_i
            zargtemp  = ( REAL(jk,wp) - 0.5_wp ) * r1_nlay_i
            zsal =  1.6_wp * ( 1._wp - COS( rpi * zargtemp**( 0.407_wp / ( 0.573_wp + zargtemp ) ) ) )
            DO ji = kideb, kiut
               s_i_1d(ji,jk) = zsal
            END DO
         END DO
         !
      ENDIF
      !
      CALL wrk_dealloc( jpij, z_slope_s )
      !
   END SUBROUTINE lim_var_salprof1d

   SUBROUTINE lim_var_zapsmall
      !!-------------------------------------------------------------------
      !!                   ***  ROUTINE lim_var_zapsmall ***
      !!
      !! ** Purpose :   Remove too small sea ice areas and correct fluxes
      !!
      !! history : LIM3.5 - 01-2014 (C. Rousset) original code
      !!-------------------------------------------------------------------
      INTEGER  ::   ji, jj, jl, jk   ! dummy loop indices
      REAL(wp) ::   zsal, zvi, zvs, zei, zes
      !!-------------------------------------------------------------------
      at_i (:,:) = 0._wp
      DO jl = 1, jpl
         at_i(:,:) = at_i(:,:) + a_i(:,:,jl)
      END DO

      DO jl = 1, jpl

         !-----------------------------------------------------------------
         ! Zap ice energy and use ocean heat to melt ice
         !-----------------------------------------------------------------
         DO jk = 1, nlay_i
            DO jj = 1 , jpj
               DO ji = 1 , jpi
                  rswitch          = MAX( 0._wp , SIGN( 1._wp, a_i(ji,jj,jl) - epsi10 ) )
                  rswitch          = MAX( 0._wp , SIGN( 1._wp, at_i(ji,jj  ) - epsi10 ) ) * rswitch
                  rswitch          = MAX( 0._wp , SIGN( 1._wp, v_i(ji,jj,jl) - epsi10 ) ) * rswitch
                  rswitch          = MAX( 0._wp , SIGN( 1._wp, v_i(ji,jj,jl) * rswitch  &
                     &                                       / MAX( a_i(ji,jj,jl), epsi10 ) - epsi10 ) ) * rswitch
                  zei              = e_i(ji,jj,jk,jl)
                  e_i(ji,jj,jk,jl) = e_i(ji,jj,jk,jl) * rswitch
                  t_i(ji,jj,jk,jl) = t_i(ji,jj,jk,jl) * rswitch + rt0 * ( 1._wp - rswitch )
                  ! update exchanges with ocean
                  hfx_res(ji,jj)   = hfx_res(ji,jj) + ( e_i(ji,jj,jk,jl) - zei ) * r1_rdtice ! W.m-2 <0
               END DO
            END DO
         END DO

         DO jj = 1 , jpj
            DO ji = 1 , jpi
               rswitch = MAX( 0._wp , SIGN( 1._wp, a_i(ji,jj,jl) - epsi10 ) )
               rswitch = MAX( 0._wp , SIGN( 1._wp, at_i(ji,jj  ) - epsi10 ) ) * rswitch
               rswitch = MAX( 0._wp , SIGN( 1._wp, v_i(ji,jj,jl) - epsi10 ) ) * rswitch
               rswitch = MAX( 0._wp , SIGN( 1._wp, v_i(ji,jj,jl) * rswitch  &
                  &                              / MAX( a_i(ji,jj,jl), epsi10 ) - epsi10 ) ) * rswitch
               zsal = smv_i(ji,jj,  jl)
               zvi  = v_i  (ji,jj,  jl)
               zvs  = v_s  (ji,jj,  jl)
               zes  = e_s  (ji,jj,1,jl)
               !-----------------------------------------------------------------
               ! Zap snow energy 
               !-----------------------------------------------------------------
               t_s(ji,jj,1,jl) = t_s(ji,jj,1,jl) * rswitch + rt0 * ( 1._wp - rswitch )
               e_s(ji,jj,1,jl) = e_s(ji,jj,1,jl) * rswitch

               !-----------------------------------------------------------------
               ! zap ice and snow volume, add water and salt to ocean
               !-----------------------------------------------------------------
               ato_i(ji,jj)    = a_i  (ji,jj,jl) * ( 1._wp - rswitch ) + ato_i(ji,jj)
               a_i  (ji,jj,jl) = a_i  (ji,jj,jl) * rswitch
               v_i  (ji,jj,jl) = v_i  (ji,jj,jl) * rswitch
               v_s  (ji,jj,jl) = v_s  (ji,jj,jl) * rswitch
               t_su (ji,jj,jl) = t_su (ji,jj,jl) * rswitch + t_bo(ji,jj) * ( 1._wp - rswitch )
               oa_i (ji,jj,jl) = oa_i (ji,jj,jl) * rswitch
               smv_i(ji,jj,jl) = smv_i(ji,jj,jl) * rswitch

               ! update exchanges with ocean
               sfx_res(ji,jj)  = sfx_res(ji,jj) - ( smv_i(ji,jj,jl) - zsal ) * rhoic * r1_rdtice
               wfx_res(ji,jj)  = wfx_res(ji,jj) - ( v_i(ji,jj,jl)   - zvi  ) * rhoic * r1_rdtice
               wfx_snw(ji,jj)  = wfx_snw(ji,jj) - ( v_s(ji,jj,jl)   - zvs  ) * rhosn * r1_rdtice
               hfx_res(ji,jj)  = hfx_res(ji,jj) + ( e_s(ji,jj,1,jl) - zes ) * r1_rdtice ! W.m-2 <0
            END DO
         END DO
      END DO 

      ! to be sure that at_i is the sum of a_i(jl)
      at_i (:,:) = 0._wp
      DO jl = 1, jpl
         at_i(:,:) = at_i(:,:) + a_i(:,:,jl)
      END DO

      ! open water = 1 if at_i=0
      DO jj = 1, jpj
         DO ji = 1, jpi
            rswitch      = MAX( 0._wp , SIGN( 1._wp, - at_i(ji,jj) ) )
            ato_i(ji,jj) = rswitch + (1._wp - rswitch ) * ato_i(ji,jj)
         END DO
      END DO

      !
   END SUBROUTINE lim_var_zapsmall

   SUBROUTINE lim_var_itd( zhti, zhts, zai, zht_i, zht_s, za_i )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_var_itd   ***
      !!
      !! ** Purpose :  converting 1-cat ice to multiple ice categories
      !!
      !!                  ice thickness distribution follows a gaussian law
      !!               around the concentration of the most likely ice thickness
      !!                           (similar as limistate.F90)
      !!
      !! ** Method:   Iterative procedure
      !!                
      !!               1) Try to fill the jpl ice categories (bounds hi_max(0:jpl)) with a gaussian
      !!
      !!               2) Check whether the distribution conserves area and volume, positivity and
      !!                  category boundaries
      !!              
      !!               3) If not (input ice is too thin), the last category is empty and
      !!                  the number of categories is reduced (jpl-1)
      !!
      !!               4) Iterate until ok (SUM(itest(:) = 4)
      !!
      !! ** Arguments : zhti: 1-cat ice thickness
      !!                zhts: 1-cat snow depth
      !!                zai : 1-cat ice concentration
      !!
      !! ** Output    : jpl-cat 
      !!
      !!  (Example of application: BDY forcings when input are cell averaged)  
      !!
      !!-------------------------------------------------------------------
      !! History : LIM3.5 - 2012    (M. Vancoppenolle)  Original code
      !!                    2014    (C. Rousset)        Rewriting
      !!-------------------------------------------------------------------
      !! Local variables
      INTEGER  :: ji, jk, jl             ! dummy loop indices
      INTEGER  :: ijpij, i_fill, jl0  
      REAL(wp) :: zarg, zV, zconv, zdh, zdv
      REAL(wp), DIMENSION(:),   INTENT(in)    ::   zhti, zhts, zai    ! input ice/snow variables
      REAL(wp), DIMENSION(:,:), INTENT(inout) ::   zht_i, zht_s, za_i ! output ice/snow variables
      INTEGER , POINTER, DIMENSION(:)         ::   itest
 
      CALL wrk_alloc( 4, itest )
      !--------------------------------------------------------------------
      ! initialisation of variables
      !--------------------------------------------------------------------
      ijpij = SIZE(zhti,1)
      zht_i(1:ijpij,1:jpl) = 0._wp
      zht_s(1:ijpij,1:jpl) = 0._wp
      za_i (1:ijpij,1:jpl) = 0._wp

      ! ----------------------------------------
      ! distribution over the jpl ice categories
      ! ----------------------------------------
      DO ji = 1, ijpij
         
         IF( zhti(ji) > 0._wp ) THEN

            ! find which category (jl0) the input ice thickness falls into
            jl0 = jpl
            DO jl = 1, jpl
               IF ( ( zhti(ji) >= hi_max(jl-1) ) .AND. ( zhti(ji) < hi_max(jl) ) ) THEN
                  jl0 = jl
                  CYCLE
               ENDIF
            END DO

            ! initialisation of tests
            itest(:)  = 0
         
            i_fill = jpl + 1                                             !====================================
            DO WHILE ( ( SUM( itest(:) ) /= 4 ) .AND. ( i_fill >= 2 ) )  ! iterative loop on i_fill categories
               ! iteration                                               !====================================
               i_fill = i_fill - 1
               
               ! initialisation of ice variables for each try
               zht_i(ji,1:jpl) = 0._wp
               za_i (ji,1:jpl) = 0._wp
               itest(:)        = 0      
               
               ! *** case very thin ice: fill only category 1
               IF ( i_fill == 1 ) THEN
                  zht_i(ji,1) = zhti(ji)
                  za_i (ji,1) = zai (ji)
                  
               ! *** case ice is thicker: fill categories >1
               ELSE

                  ! Fill ice thicknesses in the (i_fill-1) cat by hmean 
                  DO jl = 1, i_fill - 1
                     zht_i(ji,jl) = hi_mean(jl)
                  END DO
                  
                  ! Concentrations in the (i_fill-1) categories 
                  za_i(ji,jl0) = zai(ji) / SQRT(REAL(jpl))
                  DO jl = 1, i_fill - 1
                     IF ( jl /= jl0 ) THEN
                        zarg        = ( zht_i(ji,jl) - zhti(ji) ) / ( zhti(ji) * 0.5_wp )
                        za_i(ji,jl) =   za_i (ji,jl0) * EXP(-zarg**2)
                     ENDIF
                  END DO
                  
                  ! Concentration in the last (i_fill) category
                  za_i(ji,i_fill) = zai(ji) - SUM( za_i(ji,1:i_fill-1) )
                  
                  ! Ice thickness in the last (i_fill) category
                  zV = SUM( za_i(ji,1:i_fill-1) * zht_i(ji,1:i_fill-1) )
                  zht_i(ji,i_fill) = ( zhti(ji) * zai(ji) - zV ) / MAX( za_i(ji,i_fill), epsi10 ) 
                  
                  ! clem: correction if concentration of upper cat is greater than lower cat
                  !       (it should be a gaussian around jl0 but sometimes it is not)
                  IF ( jl0 /= jpl ) THEN
                     DO jl = jpl, jl0+1, -1
                        IF ( za_i(ji,jl) > za_i(ji,jl-1) ) THEN
                           zdv = zht_i(ji,jl) * za_i(ji,jl)
                           zht_i(ji,jl    ) = 0._wp
                           za_i (ji,jl    ) = 0._wp
                           za_i (ji,1:jl-1) = za_i(ji,1:jl-1) + zdv / MAX( REAL(jl-1) * zhti(ji), epsi10 )
                        END IF
                     ENDDO
                  ENDIF
               
               ENDIF ! case ice is thick or thin
            
               !---------------------
               ! Compatibility tests
               !--------------------- 
               ! Test 1: area conservation
               zconv = ABS( zai(ji) - SUM( za_i(ji,1:jpl) ) )
               IF ( zconv < epsi06 ) itest(1) = 1
            
               ! Test 2: volume conservation
               zconv = ABS( zhti(ji)*zai(ji) - SUM( za_i(ji,1:jpl)*zht_i(ji,1:jpl) ) )
               IF ( zconv < epsi06 ) itest(2) = 1
               
               ! Test 3: thickness of the last category is in-bounds ?
               IF ( zht_i(ji,i_fill) >= hi_max(i_fill-1) ) itest(3) = 1
               
               ! Test 4: positivity of ice concentrations
               itest(4) = 1
               DO jl = 1, i_fill
                  IF ( za_i(ji,jl) < 0._wp ) itest(4) = 0
               END DO
               !                                         !============================
            END DO                                       ! end iteration on categories
               !                                         !============================
         ENDIF ! if zhti > 0
      END DO ! i loop

      ! ------------------------------------------------
      ! Adding Snow in each category where za_i is not 0
      ! ------------------------------------------------ 
      DO jl = 1, jpl
         DO ji = 1, ijpij
            IF( za_i(ji,jl) > 0._wp ) THEN
               zht_s(ji,jl) = zht_i(ji,jl) * ( zhts(ji) / zhti(ji) )
               ! In case snow load is in excess that would lead to transformation from snow to ice
               ! Then, transfer the snow excess into the ice (different from limthd_dh)
               zdh = MAX( 0._wp, ( rhosn * zht_s(ji,jl) + ( rhoic - rau0 ) * zht_i(ji,jl) ) * r1_rau0 ) 
               ! recompute ht_i, ht_s avoiding out of bounds values
               zht_i(ji,jl) = MIN( hi_max(jl), zht_i(ji,jl) + zdh )
               zht_s(ji,jl) = MAX( 0._wp, zht_s(ji,jl) - zdh * rhoic * r1_rhosn )
            ENDIF
         ENDDO
      ENDDO

      CALL wrk_dealloc( 4, itest )
      !
    END SUBROUTINE lim_var_itd


#else
   !!----------------------------------------------------------------------
   !!   Default option         Dummy module          NO  LIM3 sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_var_agg          ! Empty routines
   END SUBROUTINE lim_var_agg
   SUBROUTINE lim_var_glo2eqv      ! Empty routines
   END SUBROUTINE lim_var_glo2eqv
   SUBROUTINE lim_var_eqv2glo      ! Empty routines
   END SUBROUTINE lim_var_eqv2glo
   SUBROUTINE lim_var_salprof      ! Empty routines
   END SUBROUTINE lim_var_salprof
   SUBROUTINE lim_var_bv           ! Emtpy routines
   END SUBROUTINE lim_var_bv
   SUBROUTINE lim_var_salprof1d    ! Emtpy routines
   END SUBROUTINE lim_var_salprof1d
   SUBROUTINE lim_var_zapsmall
   END SUBROUTINE lim_var_zapsmall
   SUBROUTINE lim_var_itd
   END SUBROUTINE lim_var_itd
#endif

   !!======================================================================
END MODULE limvar
