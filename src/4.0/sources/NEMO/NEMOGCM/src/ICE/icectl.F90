MODULE icectl
   !!======================================================================
   !!                     ***  MODULE  icectl  ***
   !!   sea-ice : controls and prints
   !!======================================================================
   !! History :  3.5  !  2015-01  (M. Vancoppenolle) Original code
   !!            3.7  !  2016-10  (C. Rousset)       Add routine ice_prt3D
   !!            4.0  !  2018     (many people)      SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!    ice_cons_hsm     : conservation tests on heat, salt and mass
   !!    ice_cons_final   : conservation tests on heat, salt and mass at end of time step
   !!    ice_ctl          : control prints in case of crash
   !!    ice_prt          : control prints at a given grid point
   !!    ice_prt3D        : control prints of ice arrays
   !!----------------------------------------------------------------------
   USE phycst         ! physical constants
   USE oce            ! ocean dynamics and tracers
   USE dom_oce        ! ocean space and time domain
   USE ice            ! sea-ice: variables
   USE ice1D          ! sea-ice: thermodynamics variables
   USE sbc_oce        ! Surface boundary condition: ocean fields
   USE sbc_ice        ! Surface boundary condition: ice   fields
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! fortran utilities (glob_sum + no signed zero)
   USE timing         ! Timing
   USE prtctl         ! Print control

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_cons_hsm
   PUBLIC   ice_cons_final
   PUBLIC   ice_ctl
   PUBLIC   ice_prt
   PUBLIC   ice_prt3D

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/ICE 4.0 , NEMO Consortium (2018)
   !! $Id: icectl.F90 10993 2019-05-17 13:07:59Z clem $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ice_cons_hsm( icount, cd_routine, pdiag_v, pdiag_s, pdiag_t, pdiag_fv, pdiag_fs, pdiag_ft )
      !!-------------------------------------------------------------------
      !!                       ***  ROUTINE ice_cons_hsm ***
      !!
      !! ** Purpose : Test the conservation of heat, salt and mass for each ice routine
      !!                     + test if ice concentration and volume are > 0
      !!
      !! ** Method  : This is an online diagnostics which can be activated with ln_icediachk=true
      !!              It prints in ocean.output if there is a violation of conservation at each time-step
      !!              The thresholds (zv_sill, zs_sill, zt_sill) which determine violations are set to
      !!              a minimum of 1 mm of ice (over the ice area) that is lost/gained spuriously during 100 years.
      !!              For salt and heat thresholds, ice is considered to have a salinity of 10 
      !!              and a heat content of 3e5 J/kg (=latent heat of fusion) 
      !!-------------------------------------------------------------------
      INTEGER         , INTENT(in)    ::   icount        ! called at: =0 the begining of the routine, =1  the end
      CHARACTER(len=*), INTENT(in)    ::   cd_routine    ! name of the routine
      REAL(wp)        , INTENT(inout) ::   pdiag_v, pdiag_s, pdiag_t, pdiag_fv, pdiag_fs, pdiag_ft
      !!
      REAL(wp) ::   zv, zs, zt, zfs, zfv, zft
      REAL(wp) ::   zvmin, zamin, zamax, zeimin, zesmin, zsmin
      REAL(wp) ::   zvtrp, zetrp
      REAL(wp) ::   zarea, zv_sill, zs_sill, zt_sill
      REAL(wp), PARAMETER ::   zconv = 1.e-9 ! convert W to GW and kg to Mt
      !!-------------------------------------------------------------------
      !
      IF( icount == 0 ) THEN
         !                          ! water flux
         pdiag_fv = glob_sum( 'icectl',                                                                       &
            &                 -( wfx_bog(:,:) + wfx_bom(:,:) + wfx_sum(:,:) + wfx_sni(:,:) +                  &
            &                    wfx_opw(:,:) + wfx_res(:,:) + wfx_dyn(:,:) + wfx_lam(:,:) + wfx_pnd(:,:)  +  &
            &                    wfx_snw_sni(:,:) + wfx_snw_sum(:,:) + wfx_snw_dyn(:,:) + wfx_snw_sub(:,:) +  &
            &                    wfx_ice_sub(:,:) + wfx_spr(:,:)  &
            &                  ) * e1e2t(:,:) ) * zconv
         !
         !                          ! salt flux
         pdiag_fs = glob_sum( 'icectl',                                                                     &
            &                  ( sfx_bri(:,:) + sfx_bog(:,:) + sfx_bom(:,:) + sfx_sum(:,:) + sfx_sni(:,:) +  &
            &                    sfx_opw(:,:) + sfx_res(:,:) + sfx_dyn(:,:) + sfx_sub(:,:) + sfx_lam(:,:)    &
            &                  ) *  e1e2t(:,:) ) * zconv 
         !
         !                          ! heat flux
         pdiag_ft = glob_sum( 'icectl',                                                                    &
            &                  ( hfx_sum(:,:) + hfx_bom(:,:) + hfx_bog(:,:) + hfx_dif(:,:) + hfx_opw(:,:) + hfx_snw(:,:)  & 
            &                  - hfx_thd(:,:) - hfx_dyn(:,:) - hfx_res(:,:) - hfx_sub(:,:) - hfx_spr(:,:)   &
            &                  ) *  e1e2t(:,:) ) * zconv

         pdiag_v = glob_sum( 'icectl', SUM( v_i * rhoi + v_s * rhos, dim=3 ) * e1e2t * zconv )

         pdiag_s = glob_sum( 'icectl', SUM( sv_i * rhoi            , dim=3 ) * e1e2t * zconv )

         pdiag_t = glob_sum( 'icectl', (  SUM( SUM( e_i(:,:,1:nlay_i,:), dim=4 ), dim=3 )     &
            &                 + SUM( SUM( e_s(:,:,1:nlay_s,:), dim=4 ), dim=3 ) ) * e1e2t ) * zconv

      ELSEIF( icount == 1 ) THEN

         ! water flux
         zfv = glob_sum( 'icectl',                                                                        &
            &             -( wfx_bog(:,:) + wfx_bom(:,:) + wfx_sum(:,:) + wfx_sni(:,:) +                  &
            &                wfx_opw(:,:) + wfx_res(:,:) + wfx_dyn(:,:) + wfx_lam(:,:) + wfx_pnd(:,:)  +  &
            &                wfx_snw_sni(:,:) + wfx_snw_sum(:,:) + wfx_snw_dyn(:,:) + wfx_snw_sub(:,:) +  &
            &                wfx_ice_sub(:,:) + wfx_spr(:,:)  &
            &              ) * e1e2t(:,:) ) * zconv - pdiag_fv

         ! salt flux
         zfs = glob_sum( 'icectl',                                                                       &
            &              ( sfx_bri(:,:) + sfx_bog(:,:) + sfx_bom(:,:) + sfx_sum(:,:) + sfx_sni(:,:) +  &
            &                sfx_opw(:,:) + sfx_res(:,:) + sfx_dyn(:,:) + sfx_sub(:,:) + sfx_lam(:,:)    & 
            &              ) * e1e2t(:,:) ) * zconv - pdiag_fs

         ! heat flux
         zft = glob_sum( 'icectl',                                                                      &
            &              ( hfx_sum(:,:) + hfx_bom(:,:) + hfx_bog(:,:) + hfx_dif(:,:) + hfx_opw(:,:) + hfx_snw(:,:)  & 
            &              - hfx_thd(:,:) - hfx_dyn(:,:) - hfx_res(:,:) - hfx_sub(:,:) - hfx_spr(:,:)   &
            &              ) * e1e2t(:,:) ) * zconv - pdiag_ft
 
         ! outputs
         zv = ( ( glob_sum( 'icectl', SUM( v_i * rhoi + v_s * rhos, dim=3 ) * e1e2t ) * zconv  &
            &     - pdiag_v ) * r1_rdtice - zfv ) * rday

         zs = ( ( glob_sum( 'icectl', SUM( sv_i * rhoi            , dim=3 ) * e1e2t ) * zconv  &
            &     - pdiag_s ) * r1_rdtice + zfs ) * rday

         zt = ( glob_sum( 'icectl',                                                                &
            &             (  SUM( SUM( e_i(:,:,1:nlay_i,:), dim=4 ), dim=3 )                       &
            &              + SUM( SUM( e_s(:,:,1:nlay_s,:), dim=4 ), dim=3 ) ) * e1e2t ) * zconv   &
            &   - pdiag_t ) * r1_rdtice + zft

         ! zvtrp and zetrp must be close to 0 if the advection scheme is conservative
         zvtrp = glob_sum( 'icectl', ( diag_trp_vi * rhoi + diag_trp_vs * rhos ) * e1e2t  ) * zconv * rday 
         zetrp = glob_sum( 'icectl', ( diag_trp_ei        + diag_trp_es        ) * e1e2t  ) * zconv

         zamax  = glob_max( 'icectl', SUM( a_i, dim=3 ) )
         zvmin  = glob_min( 'icectl', v_i )
         zamin  = glob_min( 'icectl', a_i )
         zsmin  = glob_min( 'icectl', sv_i )
         zeimin = glob_min( 'icectl', SUM( e_i, dim=3 ) )
         zesmin = glob_min( 'icectl', SUM( e_s, dim=3 ) )

         ! set threshold values and calculate the ice area (+epsi10 to set a threshold > 0 when there is no ice) 
         zarea   = glob_sum( 'icectl', SUM( a_i + epsi10, dim=3 ) * e1e2t ) * zconv ! in 1.e9 m2
         zv_sill = zarea * 2.5e-5
         zs_sill = zarea * 25.e-5
         zt_sill = zarea * 10.e-5

         IF(lwp) THEN
            ! check conservation issues
            IF ( ABS( zv ) > zv_sill )   WRITE(numout,*) 'violation volume [Mt/day]     (',cd_routine,') = ',zv
            IF ( ABS( zs ) > zs_sill )   WRITE(numout,*) 'violation saline [psu*Mt/day] (',cd_routine,') = ',zs
            IF ( ABS( zt ) > zt_sill )   WRITE(numout,*) 'violation enthalpy [GW]       (',cd_routine,') = ',zt
            ! check maximum ice concentration
            IF ( zamax > MAX( rn_amax_n,rn_amax_s)+epsi10 .AND. cd_routine /= 'icedyn_adv' .AND. cd_routine /= 'icedyn_rdgrft' )  &
               &                         WRITE(numout,*) 'violation a_i>amax            (',cd_routine,') = ',zamax
            ! check negative values
            IF ( zvmin  < 0. )           WRITE(numout,*) 'violation v_i<0  [m]          (',cd_routine,') = ',zvmin
            IF ( zamin  < 0. )           WRITE(numout,*) 'violation a_i<0               (',cd_routine,') = ',zamin
            IF ( zsmin  < 0. )           WRITE(numout,*) 'violation s_i<0               (',cd_routine,') = ',zsmin
            IF ( zeimin < 0. )           WRITE(numout,*) 'violation e_i<0               (',cd_routine,') = ',zeimin
            IF ( zesmin < 0. )           WRITE(numout,*) 'violation e_s<0               (',cd_routine,') = ',zesmin
!clem: the following check fails (I think...)
!            IF ( ABS(zvtrp ) > zv_sill .AND. cd_routine == 'icedyn_adv' ) THEN
!                                           WRITE(numout,*) 'violation vtrp [Mt/day]       (',cd_routine,') = ',zvtrp
!                                           WRITE(numout,*) 'violation etrp [GW]           (',cd_routine,') = ',zetrp
!            ENDIF
         ENDIF
         !
      ENDIF

   END SUBROUTINE ice_cons_hsm


   SUBROUTINE ice_cons_final( cd_routine )
      !!-------------------------------------------------------------------
      !!                     ***  ROUTINE ice_cons_final ***
      !!
      !! ** Purpose : Test the conservation of heat, salt and mass at the end of each ice time-step
      !!
      !! ** Method  : This is an online diagnostics which can be activated with ln_icediachk=true
      !!              It prints in ocean.output if there is a violation of conservation at each time-step
      !!              The thresholds (zv_sill, zs_sill, zt_sill) which determine the violation are set to
      !!              a minimum of 1 mm of ice (over the ice area) that is lost/gained spuriously during 100 years.
      !!              For salt and heat thresholds, ice is considered to have a salinity of 10 
      !!              and a heat content of 3e5 J/kg (=latent heat of fusion) 
      !!-------------------------------------------------------------------
      CHARACTER(len=*), INTENT(in)    :: cd_routine    ! name of the routine
      REAL(wp)                        :: zqmass, zhfx, zsfx, zvfx
      REAL(wp)                        :: zarea, zv_sill, zs_sill, zt_sill
      REAL(wp), PARAMETER             :: zconv = 1.e-9 ! convert W to GW and kg to Mt
      !!-------------------------------------------------------------------

      ! water flux
      zvfx  = glob_sum( 'icectl', ( wfx_ice + wfx_snw + wfx_spr + wfx_sub + diag_vice + diag_vsnw ) * e1e2t ) * zconv * rday

      ! salt flux
      zsfx  = glob_sum( 'icectl', ( sfx + diag_sice ) * e1e2t ) * zconv * rday

      ! heat flux
      ! clem: not the good formulation
      !!zhfx  = glob_sum( 'icectl', ( qt_oce_ai - qt_atm_oi + diag_heat + hfx_thd + hfx_dyn + hfx_res + hfx_sub + hfx_spr  &
      !!   &                        ) * e1e2t ) * zconv

      ! set threshold values and calculate the ice area (+epsi10 to set a threshold > 0 when there is no ice) 
      zarea   = glob_sum( 'icectl', SUM( a_i + epsi10, dim=3 ) * e1e2t ) * zconv ! in 1.e9 m2
      zv_sill = zarea * 2.5e-5
      zs_sill = zarea * 25.e-5
      zt_sill = zarea * 10.e-5

      IF(lwp) THEN
         IF( ABS( zvfx ) > zv_sill )   WRITE(numout,*) 'violation vfx  [Mt/day]       (',cd_routine,') = ',zvfx
         IF( ABS( zsfx ) > zs_sill )   WRITE(numout,*) 'violation sfx  [psu*Mt/day]   (',cd_routine,') = ',zsfx
         !!IF( ABS( zhfx ) > zt_sill )   WRITE(numout,*) 'violation hfx  [GW]           (',cd_routine,') = ',zhfx
      ENDIF
      !
   END SUBROUTINE ice_cons_final

   
   SUBROUTINE ice_ctl( kt )
      !!-------------------------------------------------------------------
      !!                   ***  ROUTINE ice_ctl *** 
      !!                 
      !! ** Purpose :   Alerts in case of model crash
      !!-------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt      ! ocean time step
      INTEGER  ::   ji, jj, jk,  jl   ! dummy loop indices
      INTEGER  ::   inb_altests       ! number of alert tests (max 20)
      INTEGER  ::   ialert_id         ! number of the current alert
      REAL(wp) ::   ztmelts           ! ice layer melting point
      CHARACTER (len=30), DIMENSION(20) ::   cl_alname   ! name of alert
      INTEGER           , DIMENSION(20) ::   inb_alp     ! number of alerts positive
      !!-------------------------------------------------------------------

      inb_altests = 10
      inb_alp(:)  =  0

      ! Alert if incompatible volume and concentration
      ialert_id = 2 ! reference number of this alert
      cl_alname(ialert_id) = ' Incompat vol and con         '    ! name of the alert

      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF(  v_i(ji,jj,jl) /= 0._wp   .AND.   a_i(ji,jj,jl) == 0._wp   ) THEN
                  !WRITE(numout,*) ' ALERTE 2 :   Incompatible volume and concentration '
                  !WRITE(numout,*) ' at_i     ', at_i(ji,jj)
                  !WRITE(numout,*) ' Point - category', ji, jj, jl
                  !WRITE(numout,*) ' a_i *** a_i_b   ', a_i      (ji,jj,jl), a_i_b  (ji,jj,jl)
                  !WRITE(numout,*) ' v_i *** v_i_b   ', v_i      (ji,jj,jl), v_i_b  (ji,jj,jl)
                  inb_alp(ialert_id) = inb_alp(ialert_id) + 1
               ENDIF
            END DO
         END DO
      END DO

      ! Alerte if very thick ice
      ialert_id = 3 ! reference number of this alert
      cl_alname(ialert_id) = ' Very thick ice               ' ! name of the alert
      jl = jpl 
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF(   h_i(ji,jj,jl)  >  50._wp   ) THEN
               !CALL ice_prt( kt, ji, jj, 2, ' ALERTE 3 :   Very thick ice ' )
               inb_alp(ialert_id) = inb_alp(ialert_id) + 1
            ENDIF
         END DO
      END DO

      ! Alert if very fast ice
      ialert_id = 4 ! reference number of this alert
      cl_alname(ialert_id) = ' Very fast ice               ' ! name of the alert
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF(   MAX( ABS( u_ice(ji,jj) ), ABS( v_ice(ji,jj) ) ) > 1.5  .AND.  &
               &  at_i(ji,jj) > 0._wp   ) THEN
               !CALL ice_prt( kt, ji, jj, 1, ' ALERTE 4 :   Very fast ice ' )
               !WRITE(numout,*) ' ice strength             : ', strength(ji,jj)
               !WRITE(numout,*) ' oceanic stress utau      : ', utau(ji,jj) 
               !WRITE(numout,*) ' oceanic stress vtau      : ', vtau(ji,jj)
               !WRITE(numout,*) ' sea-ice stress utau_ice  : ', utau_ice(ji,jj) 
               !WRITE(numout,*) ' sea-ice stress vtau_ice  : ', vtau_ice(ji,jj)
               !WRITE(numout,*) ' sst                      : ', sst_m(ji,jj)
               !WRITE(numout,*) ' sss                      : ', sss_m(ji,jj)
               !WRITE(numout,*) 
               inb_alp(ialert_id) = inb_alp(ialert_id) + 1
            ENDIF
         END DO
      END DO

      ! Alert if there is ice on continents
      ialert_id = 6 ! reference number of this alert
      cl_alname(ialert_id) = ' Ice on continents           ' ! name of the alert
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF(   tmask(ji,jj,1) <= 0._wp   .AND.   at_i(ji,jj) > 0._wp   ) THEN 
               !CALL ice_prt( kt, ji, jj, 1, ' ALERTE 6 :   Ice on continents ' )
               !WRITE(numout,*) ' masks s, u, v        : ', tmask(ji,jj,1), umask(ji,jj,1), vmask(ji,jj,1) 
               !WRITE(numout,*) ' sst                  : ', sst_m(ji,jj)
               !WRITE(numout,*) ' sss                  : ', sss_m(ji,jj)
               !WRITE(numout,*) ' at_i(ji,jj)          : ', at_i(ji,jj)
               !WRITE(numout,*) ' v_ice(ji,jj)         : ', v_ice(ji,jj)
               !WRITE(numout,*) ' v_ice(ji,jj-1)       : ', v_ice(ji,jj-1)
               !WRITE(numout,*) ' u_ice(ji-1,jj)       : ', u_ice(ji-1,jj)
               !WRITE(numout,*) ' u_ice(ji,jj)         : ', v_ice(ji,jj)
               !
               inb_alp(ialert_id) = inb_alp(ialert_id) + 1
            ENDIF
         END DO
      END DO

!
!     ! Alert if very fresh ice
      ialert_id = 7 ! reference number of this alert
      cl_alname(ialert_id) = ' Very fresh ice               ' ! name of the alert
      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF( s_i(ji,jj,jl) < 0.1 .AND. a_i(ji,jj,jl) > 0._wp ) THEN
!                 CALL ice_prt(kt,ji,jj,1, ' ALERTE 7 :   Very fresh ice ' )
!                 WRITE(numout,*) ' sst                  : ', sst_m(ji,jj)
!                 WRITE(numout,*) ' sss                  : ', sss_m(ji,jj)
!                 WRITE(numout,*) 
                  inb_alp(ialert_id) = inb_alp(ialert_id) + 1
               ENDIF
            END DO
         END DO
      END DO
!

!     ! Alert if too old ice
      ialert_id = 9 ! reference number of this alert
      cl_alname(ialert_id) = ' Very old   ice               ' ! name of the alert
      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF ( ( ( ABS( o_i(ji,jj,jl) ) > rdt_ice ) .OR. &
                      ( ABS( o_i(ji,jj,jl) ) < 0._wp) ) .AND. &
                             ( a_i(ji,jj,jl) > 0._wp ) ) THEN
                  !CALL ice_prt( kt, ji, jj, 1, ' ALERTE 9 :   Wrong ice age ')
                  inb_alp(ialert_id) = inb_alp(ialert_id) + 1
               ENDIF
            END DO
         END DO
      END DO
 
      ! Alert on salt flux
      ialert_id = 5 ! reference number of this alert
      cl_alname(ialert_id) = ' High salt flux               ' ! name of the alert
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF( ABS( sfx (ji,jj) ) > 1.0e-2 ) THEN  ! = 1 psu/day for 1m ocean depth
               !CALL ice_prt( kt, ji, jj, 3, ' ALERTE 5 :   High salt flux ' )
               !DO jl = 1, jpl
                  !WRITE(numout,*) ' Category no: ', jl
                  !WRITE(numout,*) ' a_i        : ', a_i      (ji,jj,jl) , ' a_i_b      : ', a_i_b  (ji,jj,jl)   
                  !WRITE(numout,*) ' v_i        : ', v_i      (ji,jj,jl) , ' v_i_b      : ', v_i_b  (ji,jj,jl)   
                  !WRITE(numout,*) ' '
               !END DO
               inb_alp(ialert_id) = inb_alp(ialert_id) + 1
            ENDIF
         END DO
      END DO

      ! Alert if qns very big
      ialert_id = 8 ! reference number of this alert
      cl_alname(ialert_id) = ' fnsolar very big             ' ! name of the alert
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF( ABS( qns(ji,jj) ) > 1500._wp .AND. at_i(ji,jj) > 0._wp ) THEN
               !
               !WRITE(numout,*) ' ALERTE 8 :   Very high non-solar heat flux'
               !WRITE(numout,*) ' ji, jj    : ', ji, jj
               !WRITE(numout,*) ' qns       : ', qns(ji,jj)
               !WRITE(numout,*) ' sst       : ', sst_m(ji,jj)
               !WRITE(numout,*) ' sss       : ', sss_m(ji,jj)
               !
               !CALL ice_prt( kt, ji, jj, 2, '   ')
               inb_alp(ialert_id) = inb_alp(ialert_id) + 1
               !
            ENDIF
         END DO
      END DO
      !+++++
 
      ! Alert if very warm ice
      ialert_id = 10 ! reference number of this alert
      cl_alname(ialert_id) = ' Very warm ice                ' ! name of the alert
      inb_alp(ialert_id) = 0
      DO jl = 1, jpl
         DO jk = 1, nlay_i
            DO jj = 1, jpj
               DO ji = 1, jpi
                  ztmelts    =  -rTmlt * sz_i(ji,jj,jk,jl) + rt0
                  IF( t_i(ji,jj,jk,jl) >= ztmelts  .AND.  v_i(ji,jj,jl) > 1.e-10   &
                     &                             .AND.  a_i(ji,jj,jl) > 0._wp   ) THEN
                     !WRITE(numout,*) ' ALERTE 10 :   Very warm ice'
                     !WRITE(numout,*) ' ji, jj, jk, jl : ', ji, jj, jk, jl
                     !WRITE(numout,*) ' t_i : ', t_i(ji,jj,jk,jl)
                     !WRITE(numout,*) ' e_i : ', e_i(ji,jj,jk,jl)
                     !WRITE(numout,*) ' sz_i: ', sz_i(ji,jj,jk,jl)
                     !WRITE(numout,*) ' ztmelts : ', ztmelts
                     inb_alp(ialert_id) = inb_alp(ialert_id) + 1
                  ENDIF
               END DO
            END DO
         END DO
      END DO

      ! sum of the alerts on all processors
      IF( lk_mpp ) THEN
         DO ialert_id = 1, inb_altests
            CALL mpp_sum('icectl', inb_alp(ialert_id))
         END DO
      ENDIF

      ! print alerts
      IF( lwp ) THEN
         ialert_id = 1                                 ! reference number of this alert
         cl_alname(ialert_id) = ' NO alerte 1      '   ! name of the alert
         WRITE(numout,*) ' time step ',kt
         WRITE(numout,*) ' All alerts at the end of ice model '
         DO ialert_id = 1, inb_altests
            WRITE(numout,*) ialert_id, cl_alname(ialert_id)//' : ', inb_alp(ialert_id), ' times ! '
         END DO
      ENDIF
     !
   END SUBROUTINE ice_ctl
 
   
   SUBROUTINE ice_prt( kt, ki, kj, kn, cd1 )
      !!-------------------------------------------------------------------
      !!                   ***  ROUTINE ice_prt *** 
      !!                 
      !! ** Purpose :   Writes global ice state on the (i,j) point 
      !!                in ocean.ouput 
      !!                3 possibilities exist 
      !!                n = 1/-1 -> simple ice state (plus Mechanical Check if -1)
      !!                n = 2    -> exhaustive state
      !!                n = 3    -> ice/ocean salt fluxes
      !!
      !! ** input   :   point coordinates (i,j) 
      !!                n : number of the option
      !!-------------------------------------------------------------------
      INTEGER         , INTENT(in) ::   kt            ! ocean time step
      INTEGER         , INTENT(in) ::   ki, kj, kn    ! ocean gridpoint indices
      CHARACTER(len=*), INTENT(in) ::   cd1           !
      !!
      INTEGER :: jl, ji, jj
      !!-------------------------------------------------------------------

      DO ji = mi0(ki), mi1(ki)
         DO jj = mj0(kj), mj1(kj)

            WRITE(numout,*) ' time step ',kt,' ',cd1             ! print title

            !----------------
            !  Simple state
            !----------------
            
            IF ( kn == 1 .OR. kn == -1 ) THEN
               WRITE(numout,*) ' ice_prt - Point : ',ji,jj
               WRITE(numout,*) ' ~~~~~~~~~~~~~~ '
               WRITE(numout,*) ' Simple state '
               WRITE(numout,*) ' masks s,u,v   : ', tmask(ji,jj,1), umask(ji,jj,1), vmask(ji,jj,1)
               WRITE(numout,*) ' lat - long    : ', gphit(ji,jj), glamt(ji,jj)
               WRITE(numout,*) ' - Ice drift   '
               WRITE(numout,*) '   ~~~~~~~~~~~ '
               WRITE(numout,*) ' u_ice(i-1,j)  : ', u_ice(ji-1,jj)
               WRITE(numout,*) ' u_ice(i  ,j)  : ', u_ice(ji,jj)
               WRITE(numout,*) ' v_ice(i  ,j-1): ', v_ice(ji,jj-1)
               WRITE(numout,*) ' v_ice(i  ,j)  : ', v_ice(ji,jj)
               WRITE(numout,*) ' strength      : ', strength(ji,jj)
               WRITE(numout,*)
               WRITE(numout,*) ' - Cell values '
               WRITE(numout,*) '   ~~~~~~~~~~~ '
               WRITE(numout,*) ' cell area     : ', e1e2t(ji,jj)
               WRITE(numout,*) ' at_i          : ', at_i(ji,jj)       
               WRITE(numout,*) ' vt_i          : ', vt_i(ji,jj)       
               WRITE(numout,*) ' vt_s          : ', vt_s(ji,jj)       
               DO jl = 1, jpl
                  WRITE(numout,*) ' - Category (', jl,')'
                  WRITE(numout,*) ' a_i           : ', a_i(ji,jj,jl)
                  WRITE(numout,*) ' h_i           : ', h_i(ji,jj,jl)
                  WRITE(numout,*) ' h_s           : ', h_s(ji,jj,jl)
                  WRITE(numout,*) ' v_i           : ', v_i(ji,jj,jl)
                  WRITE(numout,*) ' v_s           : ', v_s(ji,jj,jl)
                  WRITE(numout,*) ' e_s           : ', e_s(ji,jj,1:nlay_s,jl)
                  WRITE(numout,*) ' e_i           : ', e_i(ji,jj,1:nlay_i,jl)
                  WRITE(numout,*) ' t_su          : ', t_su(ji,jj,jl)
                  WRITE(numout,*) ' t_snow        : ', t_s(ji,jj,1:nlay_s,jl)
                  WRITE(numout,*) ' t_i           : ', t_i(ji,jj,1:nlay_i,jl)
                  WRITE(numout,*) ' s_i           : ', s_i(ji,jj,jl)
                  WRITE(numout,*) ' sv_i          : ', sv_i(ji,jj,jl)
                  WRITE(numout,*)
               END DO
            ENDIF
            IF( kn == -1 ) THEN
               WRITE(numout,*) ' Mechanical Check ************** '
               WRITE(numout,*) ' Check what means ice divergence '
               WRITE(numout,*) ' Total ice concentration ', at_i (ji,jj)
               WRITE(numout,*) ' Total lead fraction     ', ato_i(ji,jj)
               WRITE(numout,*) ' Sum of both             ', ato_i(ji,jj) + at_i(ji,jj)
               WRITE(numout,*) ' Sum of both minus 1     ', ato_i(ji,jj) + at_i(ji,jj) - 1.00
            ENDIF
            

            !--------------------
            !  Exhaustive state
            !--------------------
            
            IF ( kn .EQ. 2 ) THEN
               WRITE(numout,*) ' ice_prt - Point : ',ji,jj
               WRITE(numout,*) ' ~~~~~~~~~~~~~~ '
               WRITE(numout,*) ' Exhaustive state '
               WRITE(numout,*) ' lat - long ', gphit(ji,jj), glamt(ji,jj)
               WRITE(numout,*) 
               WRITE(numout,*) ' - Cell values '
               WRITE(numout,*) '   ~~~~~~~~~~~ '
               WRITE(numout,*) ' cell area     : ', e1e2t(ji,jj)
               WRITE(numout,*) ' at_i          : ', at_i(ji,jj)       
               WRITE(numout,*) ' vt_i          : ', vt_i(ji,jj)       
               WRITE(numout,*) ' vt_s          : ', vt_s(ji,jj)       
               WRITE(numout,*) ' u_ice(i-1,j)  : ', u_ice(ji-1,jj)
               WRITE(numout,*) ' u_ice(i  ,j)  : ', u_ice(ji,jj)
               WRITE(numout,*) ' v_ice(i  ,j-1): ', v_ice(ji,jj-1)
               WRITE(numout,*) ' v_ice(i  ,j)  : ', v_ice(ji,jj)
               WRITE(numout,*) ' strength      : ', strength(ji,jj)
               WRITE(numout,*) ' u_ice_b       : ', u_ice_b(ji,jj)    , ' v_ice_b       : ', v_ice_b(ji,jj)  
               WRITE(numout,*)
               
               DO jl = 1, jpl
                  WRITE(numout,*) ' - Category (',jl,')'
                  WRITE(numout,*) '   ~~~~~~~~         ' 
                  WRITE(numout,*) ' h_i        : ', h_i(ji,jj,jl)              , ' h_s        : ', h_s(ji,jj,jl)
                  WRITE(numout,*) ' t_i        : ', t_i(ji,jj,1:nlay_i,jl)
                  WRITE(numout,*) ' t_su       : ', t_su(ji,jj,jl)             , ' t_s        : ', t_s(ji,jj,1:nlay_s,jl)
                  WRITE(numout,*) ' s_i        : ', s_i(ji,jj,jl)              , ' o_i        : ', o_i(ji,jj,jl)
                  WRITE(numout,*) ' a_i        : ', a_i(ji,jj,jl)              , ' a_i_b      : ', a_i_b(ji,jj,jl)   
                  WRITE(numout,*) ' v_i        : ', v_i(ji,jj,jl)              , ' v_i_b      : ', v_i_b(ji,jj,jl)   
                  WRITE(numout,*) ' v_s        : ', v_s(ji,jj,jl)              , ' v_s_b      : ', v_s_b(ji,jj,jl)  
                  WRITE(numout,*) ' e_i1       : ', e_i(ji,jj,1,jl)            , ' ei1        : ', e_i_b(ji,jj,1,jl) 
                  WRITE(numout,*) ' e_i2       : ', e_i(ji,jj,2,jl)            , ' ei2_b      : ', e_i_b(ji,jj,2,jl)  
                  WRITE(numout,*) ' e_snow     : ', e_s(ji,jj,1,jl)            , ' e_snow_b   : ', e_s_b(ji,jj,1,jl) 
                  WRITE(numout,*) ' sv_i       : ', sv_i(ji,jj,jl)             , ' sv_i_b     : ', sv_i_b(ji,jj,jl)   
                  WRITE(numout,*) ' oa_i       : ', oa_i(ji,jj,jl)             , ' oa_i_b     : ', oa_i_b(ji,jj,jl)
               END DO !jl
               
               WRITE(numout,*)
               WRITE(numout,*) ' - Heat / FW fluxes '
               WRITE(numout,*) '   ~~~~~~~~~~~~~~~~ '
               WRITE(numout,*) ' - Heat fluxes in and out the ice ***'
               WRITE(numout,*) ' qsr_ini       : ', (1._wp-at_i_b(ji,jj)) * qsr(ji,jj) + SUM( a_i_b(ji,jj,:) * qsr_ice(ji,jj,:) )
               WRITE(numout,*) ' qns_ini       : ', (1._wp-at_i_b(ji,jj)) * qns(ji,jj) + SUM( a_i_b(ji,jj,:) * qns_ice(ji,jj,:) )
               WRITE(numout,*)
               WRITE(numout,*) 
               WRITE(numout,*) ' sst        : ', sst_m(ji,jj)  
               WRITE(numout,*) ' sss        : ', sss_m(ji,jj)  
               WRITE(numout,*) 
               WRITE(numout,*) ' - Stresses '
               WRITE(numout,*) '   ~~~~~~~~ '
               WRITE(numout,*) ' utau_ice   : ', utau_ice(ji,jj) 
               WRITE(numout,*) ' vtau_ice   : ', vtau_ice(ji,jj)
               WRITE(numout,*) ' utau       : ', utau    (ji,jj) 
               WRITE(numout,*) ' vtau       : ', vtau    (ji,jj)
            ENDIF
            
            !---------------------
            ! Salt / heat fluxes
            !---------------------
            
            IF ( kn .EQ. 3 ) THEN
               WRITE(numout,*) ' ice_prt - Point : ',ji,jj
               WRITE(numout,*) ' ~~~~~~~~~~~~~~ '
               WRITE(numout,*) ' - Salt / Heat Fluxes '
               WRITE(numout,*) '   ~~~~~~~~~~~~~~~~ '
               WRITE(numout,*) ' lat - long ', gphit(ji,jj), glamt(ji,jj)
               WRITE(numout,*)
               WRITE(numout,*) ' - Heat fluxes at bottom interface ***'
               WRITE(numout,*) ' qsr       : ', qsr(ji,jj)
               WRITE(numout,*) ' qns       : ', qns(ji,jj)
               WRITE(numout,*)
               WRITE(numout,*) ' hfx_mass     : ', hfx_thd(ji,jj) + hfx_dyn(ji,jj) + hfx_snw(ji,jj) + hfx_res(ji,jj)
               WRITE(numout,*) ' qt_atm_oi    : ', qt_atm_oi(ji,jj)
               WRITE(numout,*) ' qt_oce_ai    : ', qt_oce_ai(ji,jj)
               WRITE(numout,*) ' dhc          : ', diag_heat(ji,jj)              
               WRITE(numout,*)
               WRITE(numout,*) ' hfx_dyn      : ', hfx_dyn(ji,jj)
               WRITE(numout,*) ' hfx_thd      : ', hfx_thd(ji,jj)
               WRITE(numout,*) ' hfx_res      : ', hfx_res(ji,jj)
               WRITE(numout,*) ' qsb_ice_bot  : ', qsb_ice_bot(ji,jj) 
               WRITE(numout,*) ' qlead        : ', qlead(ji,jj) * r1_rdtice
               WRITE(numout,*)
               WRITE(numout,*) ' - Salt fluxes at bottom interface ***'
               WRITE(numout,*) ' emp       : ', emp    (ji,jj)
               WRITE(numout,*) ' sfx       : ', sfx    (ji,jj)
               WRITE(numout,*) ' sfx_res   : ', sfx_res(ji,jj)
               WRITE(numout,*) ' sfx_bri   : ', sfx_bri(ji,jj)
               WRITE(numout,*) ' sfx_dyn   : ', sfx_dyn(ji,jj)
               WRITE(numout,*)
               WRITE(numout,*) ' - Momentum fluxes '
               WRITE(numout,*) ' utau      : ', utau(ji,jj) 
               WRITE(numout,*) ' vtau      : ', vtau(ji,jj)
            ENDIF 
            WRITE(numout,*) ' '
            !
         END DO
      END DO
      !
   END SUBROUTINE ice_prt

   SUBROUTINE ice_prt3D( cd_routine )
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE ice_prt3D ***
      !!
      !! ** Purpose : CTL prints of ice arrays in case ln_ctl is activated 
      !!
      !!-------------------------------------------------------------------
      CHARACTER(len=*), INTENT(in)  :: cd_routine    ! name of the routine
      INTEGER                       :: jk, jl        ! dummy loop indices
      
      CALL prt_ctl_info(' ========== ')
      CALL prt_ctl_info( cd_routine )
      CALL prt_ctl_info(' ========== ')
      CALL prt_ctl_info(' - Cell values : ')
      CALL prt_ctl_info('   ~~~~~~~~~~~~~ ')
      CALL prt_ctl(tab2d_1=e1e2t      , clinfo1=' cell area   :')
      CALL prt_ctl(tab2d_1=at_i       , clinfo1=' at_i        :')
      CALL prt_ctl(tab2d_1=ato_i      , clinfo1=' ato_i       :')
      CALL prt_ctl(tab2d_1=vt_i       , clinfo1=' vt_i        :')
      CALL prt_ctl(tab2d_1=vt_s       , clinfo1=' vt_s        :')
      CALL prt_ctl(tab2d_1=divu_i     , clinfo1=' divu_i      :')
      CALL prt_ctl(tab2d_1=delta_i    , clinfo1=' delta_i     :')
      CALL prt_ctl(tab2d_1=stress1_i  , clinfo1=' stress1_i   :')
      CALL prt_ctl(tab2d_1=stress2_i  , clinfo1=' stress2_i   :')
      CALL prt_ctl(tab2d_1=stress12_i , clinfo1=' stress12_i  :')
      CALL prt_ctl(tab2d_1=strength   , clinfo1=' strength    :')
      CALL prt_ctl(tab2d_1=delta_i    , clinfo1=' delta_i     :')
      CALL prt_ctl(tab2d_1=u_ice      , clinfo1=' u_ice       :', tab2d_2=v_ice      , clinfo2=' v_ice       :')
       
      DO jl = 1, jpl
         CALL prt_ctl_info(' ')
         CALL prt_ctl_info(' - Category : ', ivar1=jl)
         CALL prt_ctl_info('   ~~~~~~~~~~')
         CALL prt_ctl(tab2d_1=h_i        (:,:,jl)        , clinfo1= ' h_i         : ')
         CALL prt_ctl(tab2d_1=h_s        (:,:,jl)        , clinfo1= ' h_s         : ')
         CALL prt_ctl(tab2d_1=t_su       (:,:,jl)        , clinfo1= ' t_su        : ')
         CALL prt_ctl(tab2d_1=t_s        (:,:,1,jl)      , clinfo1= ' t_snow      : ')
         CALL prt_ctl(tab2d_1=s_i        (:,:,jl)        , clinfo1= ' s_i         : ')
         CALL prt_ctl(tab2d_1=o_i        (:,:,jl)        , clinfo1= ' o_i         : ')
         CALL prt_ctl(tab2d_1=a_i        (:,:,jl)        , clinfo1= ' a_i         : ')
         CALL prt_ctl(tab2d_1=v_i        (:,:,jl)        , clinfo1= ' v_i         : ')
         CALL prt_ctl(tab2d_1=v_s        (:,:,jl)        , clinfo1= ' v_s         : ')
         CALL prt_ctl(tab2d_1=e_i        (:,:,1,jl)      , clinfo1= ' e_i1        : ')
         CALL prt_ctl(tab2d_1=e_s        (:,:,1,jl)      , clinfo1= ' e_snow      : ')
         CALL prt_ctl(tab2d_1=sv_i       (:,:,jl)        , clinfo1= ' sv_i        : ')
         CALL prt_ctl(tab2d_1=oa_i       (:,:,jl)        , clinfo1= ' oa_i        : ')
         
         DO jk = 1, nlay_i
            CALL prt_ctl_info(' - Layer : ', ivar1=jk)
            CALL prt_ctl(tab2d_1=t_i(:,:,jk,jl) , clinfo1= ' t_i       : ')
         END DO
      END DO
      
      CALL prt_ctl_info(' ')
      CALL prt_ctl_info(' - Heat / FW fluxes : ')
      CALL prt_ctl_info('   ~~~~~~~~~~~~~~~~~~ ')
      CALL prt_ctl(tab2d_1=sst_m  , clinfo1= ' sst   : ', tab2d_2=sss_m     , clinfo2= ' sss       : ')
      CALL prt_ctl(tab2d_1=qsr    , clinfo1= ' qsr   : ', tab2d_2=qns       , clinfo2= ' qns       : ')
      CALL prt_ctl(tab2d_1=emp    , clinfo1= ' emp   : ', tab2d_2=sfx       , clinfo2= ' sfx       : ')
      
      CALL prt_ctl_info(' ')
      CALL prt_ctl_info(' - Stresses : ')
      CALL prt_ctl_info('   ~~~~~~~~~~ ')
      CALL prt_ctl(tab2d_1=utau       , clinfo1= ' utau      : ', tab2d_2=vtau       , clinfo2= ' vtau      : ')
      CALL prt_ctl(tab2d_1=utau_ice   , clinfo1= ' utau_ice  : ', tab2d_2=vtau_ice   , clinfo2= ' vtau_ice  : ')
      
   END SUBROUTINE ice_prt3D

#else
   !!----------------------------------------------------------------------
   !!   Default option         Empty Module           No SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE icectl
