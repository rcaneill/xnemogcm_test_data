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
   !!    ice_cons_hsm     : conservation tests on heat, salt and mass during a  time step (global)
   !!    ice_cons_final   : conservation tests on heat, salt and mass at end of time step (global)
   !!    ice_cons2D       : conservation tests on heat, salt and mass at each gridcell
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
   USE iom            ! I/O manager library
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! fortran utilities (glob_sum + no signed zero)
   USE timing         ! Timing
   USE prtctl         ! Print control

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_cons_hsm
   PUBLIC   ice_cons_final
   PUBLIC   ice_cons2D
   PUBLIC   ice_ctl
   PUBLIC   ice_prt
   PUBLIC   ice_prt3D
   PUBLIC   ice_drift_wri
   PUBLIC   ice_drift_init

   ! thresold rates for conservation
   !    these values are changed by the namelist parameter rn_icechk, so that threshold = zchk * rn_icechk
   REAL(wp), PARAMETER ::   rchk_m   = 2.5e-7   ! kg/m2/s <=> 1e-6 m of ice per hour spuriously gained/lost
   REAL(wp), PARAMETER ::   rchk_s   = 2.5e-6   ! g/m2/s  <=> 1e-6 m of ice per hour spuriously gained/lost (considering s=10g/kg)
   REAL(wp), PARAMETER ::   rchk_t   = 7.5e-2   ! W/m2    <=> 1e-6 m of ice per hour spuriously gained/lost (considering Lf=3e5J/kg)

   ! for drift outputs
   CHARACTER(LEN=50)   ::   clname="icedrift_diagnostics.ascii"   ! ascii filename
   INTEGER             ::   numicedrift                           ! outfile unit
   REAL(wp)            ::   rdiag_icemass, rdiag_icesalt, rdiag_iceheat
   REAL(wp)            ::   rdiag_adv_icemass, rdiag_adv_icesalt, rdiag_adv_iceheat

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/ICE 4.0 , NEMO Consortium (2018)
   !! $Id: icectl.F90 15377 2021-10-14 20:50:18Z clem $
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
      !!              The thresholds (rchk_m, rchk_s, rchk_t) determine violations
      !!              For salt and heat thresholds, ice is considered to have a salinity of 10
      !!              and a heat content of 3e5 J/kg (=latent heat of fusion)
      !!-------------------------------------------------------------------
      INTEGER         , INTENT(in)    ::   icount        ! called at: =0 the begining of the routine, =1  the end
      CHARACTER(len=*), INTENT(in)    ::   cd_routine    ! name of the routine
      REAL(wp)        , INTENT(inout) ::   pdiag_v, pdiag_s, pdiag_t, pdiag_fv, pdiag_fs, pdiag_ft
      !!
      REAL(wp) ::   zdiag_mass, zdiag_salt, zdiag_heat
      REAL(wp), DIMENSION(jpi,jpj,10)     ::   ztmp3
      REAL(wp), DIMENSION(jpi,jpj,jpl,8)  ::   ztmp4
      REAL(wp), DIMENSION(10)             ::   zchk3         
      REAL(wp), DIMENSION(8)              ::   zchk4         
      !!-------------------------------------------------------------------
      !
      ! -- quantities -- !
      ztmp3(:,:,1) = SUM( v_i * rhoi + v_s * rhos + ( v_ip + v_il ) * rhow, dim=3 ) * e1e2t        ! volume
      ztmp3(:,:,2) = SUM( sv_i * rhoi, dim=3 ) * e1e2t                                             ! salt
      ztmp3(:,:,3) = ( SUM( SUM( e_i, dim=4 ), dim=3 ) + SUM( SUM( e_s, dim=4 ), dim=3 ) ) * e1e2t ! heat
      !
      ! -- fluxes -- !
      ztmp3(:,:,4) = ( wfx_bog + wfx_bom + wfx_sum + wfx_sni + wfx_opw + wfx_res + wfx_dyn + wfx_lam + wfx_pnd &  ! mass
         &          + wfx_snw_sni + wfx_snw_sum + wfx_snw_dyn + wfx_snw_sub + wfx_ice_sub + wfx_spr ) * e1e2t
      ztmp3(:,:,5) = ( sfx_bri + sfx_bog + sfx_bom + sfx_sum + sfx_sni + sfx_opw &                                ! salt
         &          + sfx_res + sfx_dyn + sfx_sub + sfx_lam ) * e1e2t
      ztmp3(:,:,6) = ( hfx_sum + hfx_bom + hfx_bog + hfx_dif + hfx_opw + hfx_snw &                                ! heat
         &          - hfx_thd - hfx_dyn - hfx_res - hfx_sub - hfx_spr ) * e1e2t
      !
      ! -- global sum -- !
      zchk3(1:6) = glob_sum_vec( 'icectl', ztmp3(:,:,1:6) )

      IF( icount == 0 ) THEN
         !
         pdiag_v  = zchk3(1)
         pdiag_s  = zchk3(2)
         pdiag_t  = zchk3(3)
         pdiag_fv = zchk3(4)
         pdiag_fs = zchk3(5)
         pdiag_ft = zchk3(6)
         !
      ELSEIF( icount == 1 ) THEN
         !
         ! -- mass, salt and heat diags -- !
         zdiag_mass = ( zchk3(1) - pdiag_v ) * r1_Dt_ice + ( zchk3(4) - pdiag_fv )
         zdiag_salt = ( zchk3(2) - pdiag_s ) * r1_Dt_ice + ( zchk3(5) - pdiag_fs )
         zdiag_heat = ( zchk3(3) - pdiag_t ) * r1_Dt_ice + ( zchk3(6) - pdiag_ft )

         ! -- max concentration diag -- !
         ztmp3(:,:,7) = SUM( a_i, dim=3 )
         zchk3(7)     = glob_max( 'icectl', ztmp3(:,:,7) )

         ! -- advection scheme is conservative? -- !
         ztmp3(:,:,8 ) = diag_adv_mass * e1e2t 
         ztmp3(:,:,9 ) = diag_adv_heat * e1e2t 
         ztmp3(:,:,10) = SUM( a_i + epsi10, dim=3 ) * e1e2t ! ice area (+epsi10 to set a threshold > 0 when there is no ice)
         zchk3(8:10)   = glob_sum_vec( 'icectl', ztmp3(:,:,8:10) )
         
         ! -- min diags -- !
         ztmp4(:,:,:,1) = v_i
         ztmp4(:,:,:,2) = v_s
         ztmp4(:,:,:,3) = v_ip
         ztmp4(:,:,:,4) = v_il
         ztmp4(:,:,:,5) = a_i
         ztmp4(:,:,:,6) = sv_i
         ztmp4(:,:,:,7) = SUM( e_i, dim=3 )
         ztmp4(:,:,:,8) = SUM( e_s, dim=3 )
         zchk4(1:8)     = glob_min_vec( 'icectl', ztmp4(:,:,:,1:8) )

         IF( lwp ) THEN
            ! check conservation issues
            IF( ABS(zdiag_mass) > rchk_m * rn_icechk_glo * zchk3(10) ) &
               &                   WRITE(numout,*)   cd_routine,' : violation mass cons. [kg] = ',zdiag_mass * rDt_ice
            IF( ABS(zdiag_salt) > rchk_s * rn_icechk_glo * zchk3(10) ) &
               &                   WRITE(numout,*)   cd_routine,' : violation salt cons. [g]  = ',zdiag_salt * rDt_ice
            IF( ABS(zdiag_heat) > rchk_t * rn_icechk_glo * zchk3(10) ) &
               &                   WRITE(numout,*)   cd_routine,' : violation heat cons. [J]  = ',zdiag_heat * rDt_ice
            ! check negative values
            IF( zchk4(1) < 0. )   WRITE(numout,*)   cd_routine,' : violation v_i  < 0        = ',zchk4(1)
            IF( zchk4(2) < 0. )   WRITE(numout,*)   cd_routine,' : violation v_s  < 0        = ',zchk4(2)
            IF( zchk4(3) < 0. )   WRITE(numout,*)   cd_routine,' : violation v_ip < 0        = ',zchk4(3)
            IF( zchk4(4) < 0. )   WRITE(numout,*)   cd_routine,' : violation v_il < 0        = ',zchk4(4)
            IF( zchk4(5) < 0. )   WRITE(numout,*)   cd_routine,' : violation a_i  < 0        = ',zchk4(5)
            IF( zchk4(6) < 0. )   WRITE(numout,*)   cd_routine,' : violation s_i  < 0        = ',zchk4(6)
            IF( zchk4(7) < 0. )   WRITE(numout,*)   cd_routine,' : violation e_i  < 0        = ',zchk4(7)
            IF( zchk4(8) < 0. )   WRITE(numout,*)   cd_routine,' : violation e_s  < 0        = ',zchk4(8)
            ! check maximum ice concentration
            IF( zchk3(7)>MAX(rn_amax_n,rn_amax_s)+epsi10 .AND. cd_routine /= 'icedyn_adv' .AND. cd_routine /= 'icedyn_rdgrft' ) &
               &                  WRITE(numout,*)   cd_routine,' : violation a_i > amax      = ',zchk3(7)
            ! check if advection scheme is conservative
            IF( ABS(zchk3(8)) > rchk_m * rn_icechk_glo * zchk3(10) .AND. cd_routine == 'icedyn_adv' ) &
               &                  WRITE(numout,*)   cd_routine,' : violation adv scheme [kg] = ',zchk3(8) * rDt_ice
            IF( ABS(zchk3(9)) > rchk_t * rn_icechk_glo * zchk3(10) .AND. cd_routine == 'icedyn_adv' ) &
               &                  WRITE(numout,*)   cd_routine,' : violation adv scheme [J]  = ',zchk3(9) * rDt_ice
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
      !!              The thresholds (rchk_m, rchk_s, rchk_t) determine the violations
      !!              For salt and heat thresholds, ice is considered to have a salinity of 10
      !!              and a heat content of 3e5 J/kg (=latent heat of fusion)
      !!-------------------------------------------------------------------
      CHARACTER(len=*), INTENT(in) ::   cd_routine    ! name of the routine
      !!
      REAL(wp), DIMENSION(jpi,jpj,4)     ::   ztmp
      REAL(wp), DIMENSION(4)             ::   zchk         
      !!-------------------------------------------------------------------

      ztmp(:,:,1) = ( wfx_ice + wfx_snw + wfx_spr + wfx_sub + wfx_pnd + diag_vice + diag_vsnw + diag_vpnd - diag_adv_mass ) * e1e2t ! mass diag
      ztmp(:,:,2) = ( sfx + diag_sice - diag_adv_salt ) * e1e2t                                                                     ! salt
      ztmp(:,:,3) = ( qt_oce_ai - qt_atm_oi + diag_heat - diag_adv_heat ) * e1e2t                                                   ! heat
      ! equivalent to this:
      !! ( -diag_heat + hfx_sum + hfx_bom + hfx_bog + hfx_dif + hfx_opw + hfx_snw &
      !!   &                                        - hfx_thd - hfx_dyn - hfx_res - hfx_sub - hfx_spr ) * e1e2t )
      ztmp(:,:,4) =  SUM( a_i + epsi10, dim=3 ) * e1e2t      ! ice area (+epsi10 to set a threshold > 0 when there is no ice)

      ! global sums
      zchk(1:4)   = glob_sum_vec( 'icectl', ztmp(:,:,1:4) )
      
      IF( lwp ) THEN
         IF( ABS(zchk(1)) > rchk_m * rn_icechk_glo * zchk(4) ) &
            &                   WRITE(numout,*) cd_routine,' : violation mass cons. [kg] = ',zchk(1) * rDt_ice
         IF( ABS(zchk(2)) > rchk_s * rn_icechk_glo * zchk(4) ) &
            &                   WRITE(numout,*) cd_routine,' : violation salt cons. [g]  = ',zchk(2) * rDt_ice
         IF( ABS(zchk(3)) > rchk_t * rn_icechk_glo * zchk(4) ) &
            &                   WRITE(numout,*) cd_routine,' : violation heat cons. [J]  = ',zchk(3) * rDt_ice
      ENDIF
      !
   END SUBROUTINE ice_cons_final

   SUBROUTINE ice_cons2D( icount, cd_routine, pdiag_v, pdiag_s, pdiag_t, pdiag_fv, pdiag_fs, pdiag_ft )
      !!-------------------------------------------------------------------
      !!                       ***  ROUTINE ice_cons2D ***
      !!
      !! ** Purpose : Test the conservation of heat, salt and mass for each ice routine
      !!                     + test if ice concentration and volume are > 0
      !!
      !! ** Method  : This is an online diagnostics which can be activated with ln_icediachk=true
      !!              It stops the code if there is a violation of conservation at any gridcell
      !!-------------------------------------------------------------------
      INTEGER         , INTENT(in) ::   icount        ! called at: =0 the begining of the routine, =1  the end
      CHARACTER(len=*), INTENT(in) ::   cd_routine    ! name of the routine
      REAL(wp)        , DIMENSION(jpi,jpj), INTENT(inout) ::   pdiag_v, pdiag_s, pdiag_t, pdiag_fv, pdiag_fs, pdiag_ft
      !!
      REAL(wp), DIMENSION(jpi,jpj) ::   zdiag_mass, zdiag_salt, zdiag_heat, &
         &                              zdiag_amin, zdiag_vmin, zdiag_smin, zdiag_emin !!, zdiag_amax
      INTEGER ::   jl, jk
      LOGICAL ::   ll_stop_m = .FALSE.
      LOGICAL ::   ll_stop_s = .FALSE.
      LOGICAL ::   ll_stop_t = .FALSE.
      CHARACTER(len=120) ::   clnam   ! filename for the output
      !!-------------------------------------------------------------------
      !
      IF( icount == 0 ) THEN

         pdiag_v = SUM( v_i  * rhoi + v_s * rhos + ( v_ip + v_il ) * rhow, dim=3 )
         pdiag_s = SUM( sv_i * rhoi , dim=3 )
         pdiag_t = SUM( SUM( e_i, dim=4 ), dim=3 ) + SUM( SUM( e_s, dim=4 ), dim=3 )

         ! mass flux
         pdiag_fv = wfx_bog + wfx_bom + wfx_sum + wfx_sni + wfx_opw + wfx_res + wfx_dyn + wfx_lam + wfx_pnd  +  &
            &       wfx_snw_sni + wfx_snw_sum + wfx_snw_dyn + wfx_snw_sub + wfx_ice_sub + wfx_spr
         ! salt flux
         pdiag_fs = sfx_bri + sfx_bog + sfx_bom + sfx_sum + sfx_sni + sfx_opw + sfx_res + sfx_dyn + sfx_sub + sfx_lam
         ! heat flux
         pdiag_ft =   hfx_sum + hfx_bom + hfx_bog + hfx_dif + hfx_opw + hfx_snw  &
            &       - hfx_thd - hfx_dyn - hfx_res - hfx_sub - hfx_spr

      ELSEIF( icount == 1 ) THEN

         ! -- mass diag -- !
         zdiag_mass =   ( SUM( v_i * rhoi + v_s * rhos + ( v_ip + v_il ) * rhow, dim=3 ) - pdiag_v ) * r1_Dt_ice    &
            &         + ( wfx_bog + wfx_bom + wfx_sum + wfx_sni + wfx_opw + wfx_res + wfx_dyn + wfx_lam + wfx_pnd + &
            &             wfx_snw_sni + wfx_snw_sum + wfx_snw_dyn + wfx_snw_sub + wfx_ice_sub + wfx_spr )           &
            &         - pdiag_fv
         IF( MAXVAL( ABS(zdiag_mass) ) > rchk_m * rn_icechk_cel )   ll_stop_m = .TRUE.
         !
         ! -- salt diag -- !
         zdiag_salt =   ( SUM( sv_i * rhoi , dim=3 ) - pdiag_s ) * r1_Dt_ice                                                  &
            &         + ( sfx_bri + sfx_bog + sfx_bom + sfx_sum + sfx_sni + sfx_opw + sfx_res + sfx_dyn + sfx_sub + sfx_lam ) &
            &         - pdiag_fs
         IF( MAXVAL( ABS(zdiag_salt) ) > rchk_s * rn_icechk_cel )   ll_stop_s = .TRUE.
         !
         ! -- heat diag -- !
         zdiag_heat =   ( SUM( SUM( e_i, dim=4 ), dim=3 ) + SUM( SUM( e_s, dim=4 ), dim=3 ) - pdiag_t ) * r1_Dt_ice &
            &         + (  hfx_sum + hfx_bom + hfx_bog + hfx_dif + hfx_opw + hfx_snw                                &
            &            - hfx_thd - hfx_dyn - hfx_res - hfx_sub - hfx_spr )                                        &
            &         - pdiag_ft
         IF( MAXVAL( ABS(zdiag_heat) ) > rchk_t * rn_icechk_cel )   ll_stop_t = .TRUE.
         !
         ! -- other diags -- !
         ! a_i < 0
         zdiag_amin(:,:) = 0._wp
         DO jl = 1, jpl
            WHERE( a_i(:,:,jl) < 0._wp )   zdiag_amin(:,:) = 1._wp
         ENDDO
         ! v_i < 0
         zdiag_vmin(:,:) = 0._wp
         DO jl = 1, jpl
            WHERE( v_i(:,:,jl) < 0._wp )   zdiag_vmin(:,:) = 1._wp
         ENDDO
         ! s_i < 0
         zdiag_smin(:,:) = 0._wp
         DO jl = 1, jpl
            WHERE( s_i(:,:,jl) < 0._wp )   zdiag_smin(:,:) = 1._wp
         ENDDO
         ! e_i < 0
         zdiag_emin(:,:) = 0._wp
         DO jl = 1, jpl
            DO jk = 1, nlay_i
               WHERE( e_i(:,:,jk,jl) < 0._wp )   zdiag_emin(:,:) = 1._wp
            ENDDO
         ENDDO
         ! a_i > amax
         !WHERE( SUM( a_i, dim=3 ) > ( MAX( rn_amax_n, rn_amax_s ) + epsi10 )   ;   zdiag_amax(:,:) = 1._wp
         !ELSEWHERE                                                             ;   zdiag_amax(:,:) = 0._wp
         !END WHERE

         IF( ll_stop_m .OR. ll_stop_s .OR. ll_stop_t ) THEN
            clnam = 'diag_ice_conservation_'//cd_routine
            CALL ice_cons_wri( clnam, zdiag_mass, zdiag_salt, zdiag_heat, zdiag_amin, zdiag_vmin, zdiag_smin, zdiag_emin )
         ENDIF

         IF( ll_stop_m )   CALL ctl_stop( 'STOP', cd_routine//': ice mass conservation issue' )
         IF( ll_stop_s )   CALL ctl_stop( 'STOP', cd_routine//': ice salt conservation issue' )
         IF( ll_stop_t )   CALL ctl_stop( 'STOP', cd_routine//': ice heat conservation issue' )

      ENDIF

   END SUBROUTINE ice_cons2D

   SUBROUTINE ice_cons_wri( cdfile_name, pdiag_mass, pdiag_salt, pdiag_heat, pdiag_amin, pdiag_vmin, pdiag_smin, pdiag_emin )
      !!---------------------------------------------------------------------
      !!                 ***  ROUTINE ice_cons_wri  ***
      !!
      !! ** Purpose :   create a NetCDF file named cdfile_name which contains
      !!                the instantaneous fields when conservation issue occurs
      !!
      !! ** Method  :   NetCDF files using ioipsl
      !!----------------------------------------------------------------------
      CHARACTER(len=*), INTENT( in ) ::   cdfile_name      ! name of the file created
      REAL(wp), DIMENSION(:,:), INTENT( in ) ::   pdiag_mass, pdiag_salt, pdiag_heat, &
         &                                        pdiag_amin, pdiag_vmin, pdiag_smin, pdiag_emin !!, pdiag_amax
      !!
      INTEGER ::   inum
      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'ice_cons_wri : single instantaneous ice state'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~  named :', cdfile_name, '...nc'
      IF(lwp) WRITE(numout,*)

      CALL iom_open( TRIM(cdfile_name), inum, ldwrt = .TRUE., kdlev = jpl, cdcomp = 'ICE' )

      CALL iom_rstput( 0, 0, inum, 'cons_mass', pdiag_mass(:,:) , ktype = jp_r8 )    ! ice mass spurious lost/gain
      CALL iom_rstput( 0, 0, inum, 'cons_salt', pdiag_salt(:,:) , ktype = jp_r8 )    ! ice salt spurious lost/gain
      CALL iom_rstput( 0, 0, inum, 'cons_heat', pdiag_heat(:,:) , ktype = jp_r8 )    ! ice heat spurious lost/gain
      ! other diags
      CALL iom_rstput( 0, 0, inum, 'aneg_count', pdiag_amin(:,:) , ktype = jp_r8 )    !
      CALL iom_rstput( 0, 0, inum, 'vneg_count', pdiag_vmin(:,:) , ktype = jp_r8 )    !
      CALL iom_rstput( 0, 0, inum, 'sneg_count', pdiag_smin(:,:) , ktype = jp_r8 )    !
      CALL iom_rstput( 0, 0, inum, 'eneg_count', pdiag_emin(:,:) , ktype = jp_r8 )    !
      ! mean state
      CALL iom_rstput( 0, 0, inum, 'icecon'    , SUM(a_i ,dim=3) , ktype = jp_r8 )    !
      CALL iom_rstput( 0, 0, inum, 'icevol'    , SUM(v_i ,dim=3) , ktype = jp_r8 )    !
      CALL iom_rstput( 0, 0, inum, 'snwvol'    , SUM(v_s ,dim=3) , ktype = jp_r8 )    !
      CALL iom_rstput( 0, 0, inum, 'pndvol'    , SUM(v_ip,dim=3) , ktype = jp_r8 )    !
      CALL iom_rstput( 0, 0, inum, 'lidvol'    , SUM(v_il,dim=3) , ktype = jp_r8 )    !

      CALL iom_close( inum )

   END SUBROUTINE ice_cons_wri

   SUBROUTINE ice_ctl( kt )
      !!-------------------------------------------------------------------
      !!                   ***  ROUTINE ice_ctl ***
      !!
      !! ** Purpose :   control checks
      !!-------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt      ! ocean time step
      INTEGER  ::   ja, ji, jj, jk, jl ! dummy loop indices
      INTEGER  ::   ialert_id          ! number of the current alert
      REAL(wp) ::   ztmelts            ! ice layer melting point
      CHARACTER (len=30), DIMENSION(20) ::   cl_alname   ! name of alert
      INTEGER           , DIMENSION(20) ::   inb_alp     ! number of alerts positive
      !!-------------------------------------------------------------------
      inb_alp(:) = 0
      ialert_id = 0

      ! Alert if very high salinity
      ialert_id = ialert_id + 1 ! reference number of this alert
      cl_alname(ialert_id) = ' Very high salinity ' ! name of the alert
      DO jl = 1, jpl
         DO_2D( 0, 0, 0, 0 )
            IF( v_i(ji,jj,jl) > epsi10  ) THEN
               IF( sv_i(ji,jj,jl) / v_i(ji,jj,jl) > rn_simax ) THEN
                  WRITE(numout,*) ' ALERTE :   Very high salinity ',sv_i(ji,jj,jl)/v_i(ji,jj,jl)
                  WRITE(numout,*) ' at i,j,l = ',ji,jj,jl
                  inb_alp(ialert_id) = inb_alp(ialert_id) + 1
               ENDIF
            ENDIF
         END_2D
      END DO

      ! Alert if very low salinity
      ialert_id = ialert_id + 1 ! reference number of this alert
      cl_alname(ialert_id) = ' Very low salinity ' ! name of the alert
      DO jl = 1, jpl
         DO_2D( 0, 0, 0, 0 )
            IF( v_i(ji,jj,jl) > epsi10  ) THEN
               IF( sv_i(ji,jj,jl) / v_i(ji,jj,jl) < rn_simin ) THEN
                  WRITE(numout,*) ' ALERTE :   Very low salinity ',sv_i(ji,jj,jl),v_i(ji,jj,jl)
                  WRITE(numout,*) ' at i,j,l = ',ji,jj,jl
                  inb_alp(ialert_id) = inb_alp(ialert_id) + 1
               ENDIF
            ENDIF
         END_2D
      END DO

      ! Alert if very cold ice
      ialert_id = ialert_id + 1 ! reference number of this alert
      cl_alname(ialert_id) = ' Very cold ice ' ! name of the alert
      DO jl = 1, jpl
         DO_3D( 0, 0, 0, 0, 1, nlay_i )
            ztmelts    =  -rTmlt * sz_i(ji,jj,jk,jl) + rt0
            IF( t_i(ji,jj,jk,jl) < -50.+rt0  .AND.  v_i(ji,jj,jl) > epsi10 ) THEN
               WRITE(numout,*) ' ALERTE :   Very cold ice ',(t_i(ji,jj,jk,jl)-rt0)
               WRITE(numout,*) ' at i,j,k,l = ',ji,jj,jk,jl
              inb_alp(ialert_id) = inb_alp(ialert_id) + 1
            ENDIF
         END_3D
      END DO

      ! Alert if very warm ice
      ialert_id = ialert_id + 1 ! reference number of this alert
      cl_alname(ialert_id) = ' Very warm ice ' ! name of the alert
      DO jl = 1, jpl
         DO_3D( 0, 0, 0, 0, 1, nlay_i )
            ztmelts    =  -rTmlt * sz_i(ji,jj,jk,jl) + rt0
            IF( t_i(ji,jj,jk,jl) > ztmelts  .AND.  v_i(ji,jj,jl) > epsi10 ) THEN
               WRITE(numout,*) ' ALERTE :   Very warm ice',(t_i(ji,jj,jk,jl)-rt0)
               WRITE(numout,*) ' at i,j,k,l = ',ji,jj,jk,jl
              inb_alp(ialert_id) = inb_alp(ialert_id) + 1
            ENDIF
         END_3D
      END DO

      ! Alerte if very thick ice
      ialert_id = ialert_id + 1 ! reference number of this alert
      cl_alname(ialert_id) = ' Very thick ice ' ! name of the alert
      jl = jpl
      DO_2D( 0, 0, 0, 0 )
         IF( h_i(ji,jj,jl) > 50._wp ) THEN
            WRITE(numout,*) ' ALERTE :   Very thick ice ',h_i(ji,jj,jl)
            WRITE(numout,*) ' at i,j,l = ',ji,jj,jl
            inb_alp(ialert_id) = inb_alp(ialert_id) + 1
         ENDIF
      END_2D

      ! Alerte if very thin ice
      ialert_id = ialert_id + 1 ! reference number of this alert
      cl_alname(ialert_id) = ' Very thin ice ' ! name of the alert
      jl = 1
      DO_2D( 0, 0, 0, 0 )
         IF( h_i(ji,jj,jl) < rn_himin ) THEN
            WRITE(numout,*) ' ALERTE :   Very thin ice ',h_i(ji,jj,jl)
            WRITE(numout,*) ' at i,j,l = ',ji,jj,jl
            inb_alp(ialert_id) = inb_alp(ialert_id) + 1
         ENDIF
      END_2D

      ! Alert if very fast ice
      ialert_id = ialert_id + 1 ! reference number of this alert
      cl_alname(ialert_id) = ' Very fast ice ' ! name of the alert
      DO_2D( 0, 0, 0, 0 )
         IF( MAX( ABS( u_ice(ji,jj) ), ABS( v_ice(ji,jj) ) ) > 2. ) THEN
            WRITE(numout,*) ' ALERTE :   Very fast ice ',MAX( ABS( u_ice(ji,jj) ), ABS( v_ice(ji,jj) ) )
            WRITE(numout,*) ' at i,j = ',ji,jj
            inb_alp(ialert_id) = inb_alp(ialert_id) + 1
         ENDIF
      END_2D

      ! Alert if there is ice on continents
      ialert_id = ialert_id + 1 ! reference number of this alert
      cl_alname(ialert_id) = ' Ice on continents ' ! name of the alert
      DO_2D( 0, 0, 0, 0 )
         IF( tmask(ji,jj,1) == 0._wp .AND. ( at_i(ji,jj) > 0._wp .OR. vt_i(ji,jj) > 0._wp ) ) THEN
            WRITE(numout,*) ' ALERTE :   Ice on continents ',at_i(ji,jj),vt_i(ji,jj)
            WRITE(numout,*) ' at i,j = ',ji,jj
            inb_alp(ialert_id) = inb_alp(ialert_id) + 1
         ENDIF
      END_2D

      ! Alert if incompatible ice concentration and volume
      ialert_id = ialert_id + 1 ! reference number of this alert
      cl_alname(ialert_id) = ' Incompatible ice conc and vol ' ! name of the alert
      DO_2D( 0, 0, 0, 0 )
         IF(  ( vt_i(ji,jj) == 0._wp .AND. at_i(ji,jj) >  0._wp ) .OR. &
            & ( vt_i(ji,jj) >  0._wp .AND. at_i(ji,jj) == 0._wp ) ) THEN
            WRITE(numout,*) ' ALERTE :   Incompatible ice conc and vol ',at_i(ji,jj),vt_i(ji,jj)
            WRITE(numout,*) ' at i,j = ',ji,jj
            inb_alp(ialert_id) = inb_alp(ialert_id) + 1
         ENDIF
      END_2D

      ! sum of the alerts on all processors
      IF( lk_mpp ) THEN
         DO ja = 1, ialert_id
            CALL mpp_sum('icectl', inb_alp(ja))
         END DO
      ENDIF

      ! print alerts
      IF( lwp ) THEN
         WRITE(numout,*) ' time step ',kt
         WRITE(numout,*) ' All alerts at the end of ice model '
         DO ja = 1, ialert_id
            WRITE(numout,*) ja, cl_alname(ja)//' : ', inb_alp(ja), ' times ! '
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
      !!                n = 1/-1 -> simple ice state
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
               WRITE(numout,*) ' - Cell values '
               WRITE(numout,*) '   ~~~~~~~~~~~ '
               WRITE(numout,*) ' at_i          : ', at_i(ji,jj)
               WRITE(numout,*) ' ato_i         : ', ato_i(ji,jj)
               WRITE(numout,*) ' vt_i          : ', vt_i(ji,jj)
               WRITE(numout,*) ' vt_s          : ', vt_s(ji,jj)
               DO jl = 1, jpl
                  WRITE(numout,*) ' - Category (', jl,')'
                  WRITE(numout,*) '   ~~~~~~~~~~~ '
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
               WRITE(numout,*) ' at_i          : ', at_i(ji,jj)
               WRITE(numout,*) ' vt_i          : ', vt_i(ji,jj)
               WRITE(numout,*) ' vt_s          : ', vt_s(ji,jj)
               WRITE(numout,*) ' u_ice(i-1,j)  : ', u_ice(ji-1,jj)
               WRITE(numout,*) ' u_ice(i  ,j)  : ', u_ice(ji,jj)
               WRITE(numout,*) ' v_ice(i  ,j-1): ', v_ice(ji,jj-1)
               WRITE(numout,*) ' v_ice(i  ,j)  : ', v_ice(ji,jj)
               WRITE(numout,*) ' strength      : ', strength(ji,jj)
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
               WRITE(numout,*) ' qlead        : ', qlead(ji,jj) * r1_Dt_ice
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
      !! ** Purpose : CTL prints of ice arrays in case sn_cfctl%prtctl is activated
      !!
      !!-------------------------------------------------------------------
      CHARACTER(len=*), INTENT(in) ::   cd_routine    ! name of the routine
      INTEGER                      ::   jk, jl        ! dummy loop indices

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
         CALL prt_ctl_info(' - Category : ', ivar=jl)
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
         CALL prt_ctl(tab2d_1=e_s        (:,:,1,jl)      , clinfo1= ' e_snow      : ')
         CALL prt_ctl(tab2d_1=sv_i       (:,:,jl)        , clinfo1= ' sv_i        : ')
         CALL prt_ctl(tab2d_1=oa_i       (:,:,jl)        , clinfo1= ' oa_i        : ')

         DO jk = 1, nlay_i
            CALL prt_ctl_info(' - Layer : ', ivar=jk)
            CALL prt_ctl(tab2d_1=t_i(:,:,jk,jl) , clinfo1= ' t_i       : ')
            CALL prt_ctl(tab2d_1=e_i(:,:,jk,jl) , clinfo1= ' e_i       : ')
         END DO
      END DO

      CALL prt_ctl_info(' ')
      CALL prt_ctl_info(' - Stresses : ')
      CALL prt_ctl_info('   ~~~~~~~~~~ ')
      CALL prt_ctl(tab2d_1=utau       , clinfo1= ' utau      : ', tab2d_2=vtau       , clinfo2= ' vtau      : ')
      CALL prt_ctl(tab2d_1=utau_ice   , clinfo1= ' utau_ice  : ', tab2d_2=vtau_ice   , clinfo2= ' vtau_ice  : ')

   END SUBROUTINE ice_prt3D


   SUBROUTINE ice_drift_wri( kt )
      !!-------------------------------------------------------------------
      !!                     ***  ROUTINE ice_drift_wri ***
      !!
      !! ** Purpose : conservation of mass, salt and heat
      !!              write the drift in a ascii file at each time step
      !!              and the total run drifts
      !!-------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ice time-step index
      !
      REAL(wp), DIMENSION(jpi,jpj,6) ::   ztmp
      REAL(wp), DIMENSION(6)         ::   zchk
      !!-------------------------------------------------------------------
      !
      IF( kt == nit000 .AND. lwp ) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'ice_drift_wri: sea-ice drifts'
         WRITE(numout,*) '~~~~~~~~~~~~~'
      ENDIF
      !
      ! -- 2D budgets (must be close to 0) -- !
      ztmp(:,:,1) =  wfx_ice  (:,:) + wfx_snw  (:,:) + wfx_spr  (:,:) + wfx_sub(:,:) + wfx_pnd(:,:) &
         &         + diag_vice(:,:) + diag_vsnw(:,:) + diag_vpnd(:,:) - diag_adv_mass(:,:)
      ztmp(:,:,2) = sfx(:,:) + diag_sice(:,:) - diag_adv_salt(:,:)
      ztmp(:,:,3) = qt_oce_ai(:,:) - qt_atm_oi(:,:) + diag_heat(:,:) - diag_adv_heat(:,:)

      ! write outputs
      CALL iom_put( 'icedrift_mass', ztmp(:,:,1) )
      CALL iom_put( 'icedrift_salt', ztmp(:,:,2) )
      CALL iom_put( 'icedrift_heat', ztmp(:,:,3) )

      ! -- 1D budgets -- !
      ztmp(:,:,1) = ztmp(:,:,1) * e1e2t * rDt_ice         ! mass
      ztmp(:,:,2) = ztmp(:,:,2) * e1e2t * rDt_ice * 1.e-3 ! salt
      ztmp(:,:,3) = ztmp(:,:,3) * e1e2t                   ! heat

      ztmp(:,:,4) = diag_adv_mass * e1e2t * rDt_ice
      ztmp(:,:,5) = diag_adv_salt * e1e2t * rDt_ice * 1.e-3
      ztmp(:,:,6) = diag_adv_heat * e1e2t

      ! global sums
      zchk(1:6) = glob_sum_vec( 'icectl', ztmp(:,:,1:6) )
      
      !                    ! write out to file
      IF( lwp ) THEN
         ! check global drift (must be close to 0)
         WRITE(numicedrift,FMT='(2x,i6,3x,a19,4x,f25.5)') kt, 'mass drift     [kg]', zchk(1)
         WRITE(numicedrift,FMT='(11x,     a19,4x,f25.5)')     'salt drift     [kg]', zchk(2)
         WRITE(numicedrift,FMT='(11x,     a19,4x,f25.5)')     'heat drift     [W] ', zchk(3)
         ! check drift from advection scheme (can be /=0 with bdy but not sure why)
         WRITE(numicedrift,FMT='(11x,     a19,4x,f25.5)')     'mass drift adv [kg]', zchk(4)
         WRITE(numicedrift,FMT='(11x,     a19,4x,f25.5)')     'salt drift adv [kg]', zchk(5)
         WRITE(numicedrift,FMT='(11x,     a19,4x,f25.5)')     'heat drift adv [W] ', zchk(6)
      ENDIF
      !                    ! drifts
      rdiag_icemass = rdiag_icemass + zchk(1)
      rdiag_icesalt = rdiag_icesalt + zchk(2)
      rdiag_iceheat = rdiag_iceheat + zchk(3)
      rdiag_adv_icemass = rdiag_adv_icemass + zchk(4)
      rdiag_adv_icesalt = rdiag_adv_icesalt + zchk(5)
      rdiag_adv_iceheat = rdiag_adv_iceheat + zchk(6)
      !
      !                    ! output drifts and close ascii file
      IF( kt == nitend - nn_fsbc + 1 .AND. lwp ) THEN
         ! to ascii file
         WRITE(numicedrift,*) '******************************************'
         WRITE(numicedrift,FMT='(3x,a23,6x,E10.2)') 'Run mass drift     [kg]', rdiag_icemass
         WRITE(numicedrift,FMT='(3x,a23,6x,E10.2)') 'Run mass drift adv [kg]', rdiag_adv_icemass
         WRITE(numicedrift,*) '******************************************'
         WRITE(numicedrift,FMT='(3x,a23,6x,E10.2)') 'Run salt drift     [kg]', rdiag_icesalt
         WRITE(numicedrift,FMT='(3x,a23,6x,E10.2)') 'Run salt drift adv [kg]', rdiag_adv_icesalt
         WRITE(numicedrift,*) '******************************************'
         WRITE(numicedrift,FMT='(3x,a23,6x,E10.2)') 'Run heat drift     [W] ', rdiag_iceheat
         WRITE(numicedrift,FMT='(3x,a23,6x,E10.2)') 'Run heat drift adv [W] ', rdiag_adv_iceheat
         CLOSE( numicedrift )
         !
         ! to ocean output
         WRITE(numout,*)
         WRITE(numout,*) 'ice_drift_wri: ice drifts information for the run '
         WRITE(numout,*) '~~~~~~~~~~~~~'
         ! check global drift (must be close to 0)
         WRITE(numout,*) '   sea-ice mass drift     [kg] = ', rdiag_icemass
         WRITE(numout,*) '   sea-ice salt drift     [kg] = ', rdiag_icesalt
         WRITE(numout,*) '   sea-ice heat drift     [W]  = ', rdiag_iceheat
         ! check drift from advection scheme (can be /=0 with bdy but not sure why)
         WRITE(numout,*) '   sea-ice mass drift adv [kg] = ', rdiag_adv_icemass
         WRITE(numout,*) '   sea-ice salt drift adv [kg] = ', rdiag_adv_icesalt
         WRITE(numout,*) '   sea-ice heat drift adv [W]  = ', rdiag_adv_iceheat
      ENDIF
      !
   END SUBROUTINE ice_drift_wri

   SUBROUTINE ice_drift_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE ice_drift_init  ***
      !!
      !! ** Purpose :   create output file, initialise arrays
      !!----------------------------------------------------------------------
      !
      IF( .NOT.ln_icediachk ) RETURN ! exit
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'ice_drift_init: Output ice drifts to ',TRIM(clname), ' file'
         WRITE(numout,*) '~~~~~~~~~~~~~'
         WRITE(numout,*)
         !
         ! create output ascii file
         CALL ctl_opn( numicedrift, clname, 'UNKNOWN', 'FORMATTED', 'SEQUENTIAL', 1, numout, lwp, narea )
         WRITE(numicedrift,*) 'Timestep  Drifts'
         WRITE(numicedrift,*) '******************************************'
      ENDIF
      !
      rdiag_icemass = 0._wp
      rdiag_icesalt = 0._wp
      rdiag_iceheat = 0._wp
      rdiag_adv_icemass = 0._wp
      rdiag_adv_icesalt = 0._wp
      rdiag_adv_iceheat = 0._wp
      !
   END SUBROUTINE ice_drift_init

#else
   !!----------------------------------------------------------------------
   !!   Default option         Empty Module           No SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE icectl
