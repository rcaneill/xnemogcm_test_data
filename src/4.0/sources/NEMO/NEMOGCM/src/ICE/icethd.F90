MODULE icethd
   !!======================================================================
   !!                  ***  MODULE icethd   ***
   !!   sea-ice : master routine for thermodynamics
   !!======================================================================
   !! History :  1.0  !  2000-01  (M.A. Morales Maqueda, H. Goosse, T. Fichefet) original code 1D
   !!            4.0  !  2018     (many people)       SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_thd       : thermodynamics of sea ice
   !!   ice_thd_init  : initialisation of sea-ice thermodynamics
   !!----------------------------------------------------------------------
   USE phycst         ! physical constants
   USE dom_oce        ! ocean space and time domain variables
   USE ice            ! sea-ice: variables
!!gm list trop longue ==>>> why not passage en argument d'appel ?
   USE sbc_oce , ONLY : sss_m, sst_m, e3t_m, utau, vtau, ssu_m, ssv_m, frq_m, qns_tot, qsr_tot, sprecip, ln_cpl
   USE sbc_ice , ONLY : qsr_oce, qns_oce, qemp_oce, qsr_ice, qns_ice, dqns_ice, evap_ice, qprec_ice, qevap_ice, &
      &                 qml_ice, qcn_ice, qtr_ice_top
   USE ice1D          ! sea-ice: thermodynamics variables
   USE icethd_zdf     ! sea-ice: vertical heat diffusion
   USE icethd_dh      ! sea-ice: ice-snow growth and melt
   USE icethd_da      ! sea-ice: lateral melting
   USE icethd_sal     ! sea-ice: salinity
   USE icethd_ent     ! sea-ice: enthalpy redistribution
   USE icethd_do      ! sea-ice: growth in open water
   USE icethd_pnd     ! sea-ice: melt ponds
   USE iceitd         ! sea-ice: remapping thickness distribution
   USE icetab         ! sea-ice: 1D <==> 2D transformation
   USE icevar         ! sea-ice: operations
   USE icectl         ! sea-ice: control print
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! fortran utilities (glob_sum + no signed zero)
   USE lbclnk         ! lateral boundary conditions (or mpp links)
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_thd         ! called by limstp module
   PUBLIC   ice_thd_init    ! called by ice_init

   !!** namelist (namthd) **
   LOGICAL ::   ln_icedH         ! activate ice thickness change from growing/melting (T) or not (F)
   LOGICAL ::   ln_icedA         ! activate lateral melting param. (T) or not (F)
   LOGICAL ::   ln_icedO         ! activate ice growth in open-water (T) or not (F)
   LOGICAL ::   ln_icedS         ! activate gravity drainage and flushing (T) or not (F)

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/ICE 4.0 , NEMO Consortium (2018)
   !! $Id: icethd.F90 10993 2019-05-17 13:07:59Z clem $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ice_thd( kt )
      !!-------------------------------------------------------------------
      !!                ***  ROUTINE ice_thd  ***       
      !!  
      !! ** Purpose : This routine manages ice thermodynamics
      !!         
      !! ** Action : - computation of oceanic sensible heat flux at the ice base
      !!                              energy budget in the leads
      !!                              net fluxes on top of ice and of ocean
      !!             - selection of grid cells with ice
      !!                - call ice_thd_zdf  for vertical heat diffusion
      !!                - call ice_thd_dh   for vertical ice growth and melt
      !!                - call ice_thd_pnd  for melt ponds
      !!                - call ice_thd_ent  for enthalpy remapping
      !!                - call ice_thd_sal  for ice desalination
      !!                - call ice_thd_temp to  retrieve temperature from ice enthalpy
      !!                - call ice_thd_mono for extra lateral ice melt if active virtual thickness distribution
      !!                - call ice_thd_da   for lateral ice melt
      !!             - back to the geographic grid
      !!                - call ice_thd_rem  for remapping thickness distribution
      !!                - call ice_thd_do   for ice growth in leads
      !!-------------------------------------------------------------------
      INTEGER, INTENT(in) :: kt    ! number of iteration
      !
      INTEGER  :: ji, jj, jk, jl   ! dummy loop indices
      REAL(wp) :: zfric_u, zqld, zqfr, zqfr_neg
      REAL(wp), PARAMETER :: zfric_umin = 0._wp           ! lower bound for the friction velocity (cice value=5.e-04)
      REAL(wp), PARAMETER :: zch        = 0.0057_wp       ! heat transfer coefficient
      REAL(wp), DIMENSION(jpi,jpj) ::   zu_io, zv_io, zfric   ! ice-ocean velocity (m/s) and frictional velocity (m2/s2)
      !
      !!-------------------------------------------------------------------
      ! controls
      IF( ln_timing    )   CALL timing_start('icethd')                                                             ! timing
      IF( ln_icediachk )   CALL ice_cons_hsm(0, 'icethd', rdiag_v, rdiag_s, rdiag_t, rdiag_fv, rdiag_fs, rdiag_ft) ! conservation

      IF( kt == nit000 .AND. lwp ) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'ice_thd: sea-ice thermodynamics'
         WRITE(numout,*) '~~~~~~~'
      ENDIF
      
      !---------------------------------------------!
      ! computation of friction velocity at T points
      !---------------------------------------------!
      IF( ln_icedyn ) THEN
         zu_io(:,:) = u_ice(:,:) - ssu_m(:,:)
         zv_io(:,:) = v_ice(:,:) - ssv_m(:,:)
         DO jj = 2, jpjm1 
            DO ji = fs_2, fs_jpim1
               zfric(ji,jj) = rn_cio * ( 0.5_wp *  &
                  &                    (  zu_io(ji,jj) * zu_io(ji,jj) + zu_io(ji-1,jj) * zu_io(ji-1,jj)   &
                  &                     + zv_io(ji,jj) * zv_io(ji,jj) + zv_io(ji,jj-1) * zv_io(ji,jj-1) ) ) * tmask(ji,jj,1)
            END DO
         END DO
      ELSE      !  if no ice dynamics => transmit directly the atmospheric stress to the ocean
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1
               zfric(ji,jj) = r1_rau0 * SQRT( 0.5_wp *  &
                  &                         (  utau(ji,jj) * utau(ji,jj) + utau(ji-1,jj) * utau(ji-1,jj)   &
                  &                          + vtau(ji,jj) * vtau(ji,jj) + vtau(ji,jj-1) * vtau(ji,jj-1) ) ) * tmask(ji,jj,1)
            END DO
         END DO
      ENDIF
      CALL lbc_lnk( 'icethd', zfric, 'T',  1. )
      !
      !--------------------------------------------------------------------!
      ! Partial computation of forcing for the thermodynamic sea ice model
      !--------------------------------------------------------------------!
      DO jj = 1, jpj
         DO ji = 1, jpi
            rswitch  = tmask(ji,jj,1) * MAX( 0._wp , SIGN( 1._wp , at_i(ji,jj) - epsi10 ) ) ! 0 if no ice
            !
            !           !  solar irradiance transmission at the mixed layer bottom and used in the lead heat budget
            !           !  practically no "direct lateral ablation"
            !           
            !           !  net downward heat flux from the ice to the ocean, expressed as a function of ocean 
            !           !  temperature and turbulent mixing (McPhee, 1992)
            !
            ! --- Energy received in the lead from atm-oce exchanges, zqld is defined everywhere (J.m-2) --- !
            zqld =  tmask(ji,jj,1) * rdt_ice *  &
               &    ( ( 1._wp - at_i_b(ji,jj) ) * qsr_oce(ji,jj) * frq_m(ji,jj) +  &
               &      ( 1._wp - at_i_b(ji,jj) ) * qns_oce(ji,jj) + qemp_oce(ji,jj) )

            ! --- Energy needed to bring ocean surface layer until its freezing (mostly<0 but >0 if supercooling, J.m-2) --- !
            zqfr     = rau0 * rcp * e3t_m(ji,jj) * ( t_bo(ji,jj) - ( sst_m(ji,jj) + rt0 ) ) * tmask(ji,jj,1)  ! both < 0 (t_bo < sst) and > 0 (t_bo > sst)
            zqfr_neg = MIN( zqfr , 0._wp )                                                                    ! only < 0

            ! --- Sensible ocean-to-ice heat flux (mostly>0 but <0 if supercooling, W/m2)
            zfric_u            = MAX( SQRT( zfric(ji,jj) ), zfric_umin ) 
            qsb_ice_bot(ji,jj) = rswitch * rau0 * rcp * zch * zfric_u * ( ( sst_m(ji,jj) + rt0 ) - t_bo(ji,jj) ) ! W.m-2

            qsb_ice_bot(ji,jj) = rswitch * MIN( qsb_ice_bot(ji,jj), - zqfr_neg * r1_rdtice / MAX( at_i(ji,jj), epsi10 ) )
            ! upper bound for qsb_ice_bot: the heat retrieved from the ocean must be smaller than the heat necessary to reach 
            !                              the freezing point, so that we do not have SST < T_freeze
            !                              This implies: - ( qsb_ice_bot(ji,jj) * at_i(ji,jj) * rtdice ) - zqfr >= 0

            !-- Energy Budget of the leads (J.m-2), source of ice growth in open water. Must be < 0 to form ice
            qlead(ji,jj) = MIN( 0._wp , zqld - ( qsb_ice_bot(ji,jj) * at_i(ji,jj) * rdt_ice ) - zqfr )

            ! If there is ice and leads are warming => transfer energy from the lead budget and use it for bottom melting 
            ! If the grid cell is fully covered by ice (no leads) => transfer energy from the lead budget to the ice bottom budget
            IF( ( zqld >= 0._wp .AND. at_i(ji,jj) > 0._wp ) .OR. at_i(ji,jj) >= (1._wp - epsi10) ) THEN
               fhld (ji,jj) = rswitch * zqld * r1_rdtice / MAX( at_i(ji,jj), epsi10 ) ! divided by at_i since this is (re)multiplied by a_i in icethd_dh.F90
               qlead(ji,jj) = 0._wp
            ELSE
               fhld (ji,jj) = 0._wp
            ENDIF
            !
            ! Net heat flux on top of the ice-ocean [W.m-2]
            ! ---------------------------------------------
            qt_atm_oi(ji,jj) = qns_tot(ji,jj) + qsr_tot(ji,jj) 
         END DO
      END DO
      
      ! In case we bypass open-water ice formation
      IF( .NOT. ln_icedO )  qlead(:,:) = 0._wp
      ! In case we bypass growing/melting from top and bottom
      IF( .NOT. ln_icedH ) THEN
         qsb_ice_bot(:,:) = 0._wp
         fhld       (:,:) = 0._wp
      ENDIF

      ! ---------------------------------------------------------------------
      ! Net heat flux on top of the ocean after ice thermo (1st step) [W.m-2]
      ! ---------------------------------------------------------------------
      !     First  step here              :  non solar + precip - qlead - qsensible
      !     Second step in icethd_dh      :  heat remaining if total melt (zq_rema) 
      !     Third  step in iceupdate.F90  :  heat from ice-ocean mass exchange (zf_mass) + solar
      qt_oce_ai(:,:) = ( 1._wp - at_i_b(:,:) ) * qns_oce(:,:) + qemp_oce(:,:)  &  ! Non solar heat flux received by the ocean               
         &             - qlead(:,:) * r1_rdtice                                &  ! heat flux taken from the ocean where there is open water ice formation
         &             - at_i (:,:) * qsb_ice_bot(:,:)                         &  ! heat flux taken by sensible flux
         &             - at_i (:,:) * fhld       (:,:)                            ! heat flux taken during bottom growth/melt 
      !                                                                           !    (fhld should be 0 while bott growth)
      !-------------------------------------------------------------------------------------------!
      ! Thermodynamic computation (only on grid points covered by ice) => loop over ice categories
      !-------------------------------------------------------------------------------------------!
      DO jl = 1, jpl

         ! select ice covered grid points
         npti = 0 ; nptidx(:) = 0
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF ( a_i(ji,jj,jl) > epsi10 ) THEN     
                  npti         = npti  + 1
                  nptidx(npti) = (jj - 1) * jpi + ji
               ENDIF
            END DO
         END DO

         IF( npti > 0 ) THEN  ! If there is no ice, do nothing.
            !                                                                
                              CALL ice_thd_1d2d( jl, 1 )            ! --- Move to 1D arrays --- !
            !                                                       ! --- & Change units of e_i, e_s from J/m2 to J/m3 --- !
            !
            s_i_new   (1:npti) = 0._wp ; dh_s_tot(1:npti) = 0._wp  ! --- some init --- !  (important to have them here) 
            dh_i_sum  (1:npti) = 0._wp ; dh_i_bom(1:npti) = 0._wp ; dh_i_itm  (1:npti) = 0._wp 
            dh_i_sub  (1:npti) = 0._wp ; dh_i_bog(1:npti) = 0._wp
            dh_snowice(1:npti) = 0._wp ; dh_s_mlt(1:npti) = 0._wp
            !                                      
                              CALL ice_thd_zdf                      ! --- Ice-Snow temperature --- !
            !
            IF( ln_icedH ) THEN                                     ! --- Growing/Melting --- !
                              CALL ice_thd_dh                           ! Ice-Snow thickness   
                              CALL ice_thd_pnd                          ! Melt ponds formation
                              CALL ice_thd_ent( e_i_1d(1:npti,:) )      ! Ice enthalpy remapping
            ENDIF
                              CALL ice_thd_sal( ln_icedS )          ! --- Ice salinity --- !    
            !
                              CALL ice_thd_temp                     ! --- Temperature update --- !
            !
            IF( ln_icedH .AND. ln_virtual_itd ) &
               &              CALL ice_thd_mono                     ! --- Extra lateral melting if virtual_itd --- !
            !
            IF( ln_icedA )    CALL ice_thd_da                       ! --- Lateral melting --- !
            !
                              CALL ice_thd_1d2d( jl, 2 )            ! --- Change units of e_i, e_s from J/m3 to J/m2 --- !
            !                                                       ! --- & Move to 2D arrays --- !
         ENDIF
         !
      END DO
      !
      IF( ln_icediachk )   CALL ice_cons_hsm(1, 'icethd', rdiag_v, rdiag_s, rdiag_t, rdiag_fv, rdiag_fs, rdiag_ft)
      !                   
      IF( jpl > 1  )          CALL ice_itd_rem( kt )                ! --- Transport ice between thickness categories --- !
      !
      IF( ln_icedO )          CALL ice_thd_do                       ! --- Frazil ice growth in leads --- !
      !
      ! controls
      IF( ln_icectl )   CALL ice_prt    (kt, iiceprt, jiceprt, 1, ' - ice thermodyn. - ') ! prints
      IF( ln_ctl    )   CALL ice_prt3D  ('icethd')                                        ! prints
      IF( ln_timing )   CALL timing_stop('icethd')                                        ! timing
      !
   END SUBROUTINE ice_thd 

 
   SUBROUTINE ice_thd_temp
      !!-----------------------------------------------------------------------
      !!                   ***  ROUTINE ice_thd_temp *** 
      !!                 
      !! ** Purpose :   Computes sea ice temperature (Kelvin) from enthalpy
      !!
      !! ** Method  :   Formula (Bitz and Lipscomb, 1999)
      !!-------------------------------------------------------------------
      INTEGER  ::   ji, jk   ! dummy loop indices
      REAL(wp) ::   ztmelts, zbbb, zccc  ! local scalar 
      !!-------------------------------------------------------------------
      ! Recover ice temperature
      DO jk = 1, nlay_i
         DO ji = 1, npti
            ztmelts       = -rTmlt * sz_i_1d(ji,jk)
            ! Conversion q(S,T) -> T (second order equation)
            zbbb          = ( rcp - rcpi ) * ztmelts + e_i_1d(ji,jk) * r1_rhoi - rLfus
            zccc          = SQRT( MAX( zbbb * zbbb - 4._wp * rcpi * rLfus * ztmelts, 0._wp ) )
            t_i_1d(ji,jk) = rt0 - ( zbbb + zccc ) * 0.5_wp * r1_rcpi
            
            ! mask temperature
            rswitch       = 1._wp - MAX( 0._wp , SIGN( 1._wp , - h_i_1d(ji) ) ) 
            t_i_1d(ji,jk) = rswitch * t_i_1d(ji,jk) + ( 1._wp - rswitch ) * rt0
         END DO 
      END DO 
      !
   END SUBROUTINE ice_thd_temp


   SUBROUTINE ice_thd_mono
      !!-----------------------------------------------------------------------
      !!                   ***  ROUTINE ice_thd_mono *** 
      !!                 
      !! ** Purpose :   Lateral melting in case virtual_itd
      !!                          ( dA = A/2h dh )
      !!-----------------------------------------------------------------------
      INTEGER  ::   ji                 ! dummy loop indices
      REAL(wp) ::   zhi_bef            ! ice thickness before thermo
      REAL(wp) ::   zdh_mel, zda_mel   ! net melting
      REAL(wp) ::   zvi, zvs           ! ice/snow volumes 
      !!-----------------------------------------------------------------------
      !
      DO ji = 1, npti
         zdh_mel = MIN( 0._wp, dh_i_itm(ji) + dh_i_sum(ji) + dh_i_bom(ji) + dh_snowice(ji) + dh_i_sub(ji) )
         IF( zdh_mel < 0._wp .AND. a_i_1d(ji) > 0._wp )  THEN
            zvi          = a_i_1d(ji) * h_i_1d(ji)
            zvs          = a_i_1d(ji) * h_s_1d(ji)
            ! lateral melting = concentration change
            zhi_bef     = h_i_1d(ji) - zdh_mel
            rswitch     = MAX( 0._wp , SIGN( 1._wp , zhi_bef - epsi20 ) )
            zda_mel     = rswitch * a_i_1d(ji) * zdh_mel / ( 2._wp * MAX( zhi_bef, epsi20 ) )
            a_i_1d(ji)  = MAX( epsi20, a_i_1d(ji) + zda_mel ) 
            ! adjust thickness
            h_i_1d(ji) = zvi / a_i_1d(ji)            
            h_s_1d(ji) = zvs / a_i_1d(ji)            
            ! retrieve total concentration
            at_i_1d(ji) = a_i_1d(ji)
         END IF
      END DO
      !
   END SUBROUTINE ice_thd_mono


   SUBROUTINE ice_thd_1d2d( kl, kn )
      !!-----------------------------------------------------------------------
      !!                   ***  ROUTINE ice_thd_1d2d *** 
      !!                 
      !! ** Purpose :   move arrays from 1d to 2d and the reverse
      !!-----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kl   ! index of the ice category 
      INTEGER, INTENT(in) ::   kn   ! 1= from 2D to 1D   ;   2= from 1D to 2D
      !
      INTEGER ::   jk   ! dummy loop indices
      !!-----------------------------------------------------------------------
      !
      SELECT CASE( kn )
      !                    !---------------------!
      CASE( 1 )            !==  from 2D to 1D  ==!
         !                 !---------------------!
         CALL tab_2d_1d( npti, nptidx(1:npti), at_i_1d(1:npti), at_i             )
         CALL tab_2d_1d( npti, nptidx(1:npti), a_i_1d (1:npti), a_i (:,:,kl)     )
         CALL tab_2d_1d( npti, nptidx(1:npti), h_i_1d (1:npti), h_i (:,:,kl)     )
         CALL tab_2d_1d( npti, nptidx(1:npti), h_s_1d (1:npti), h_s (:,:,kl)     )
         CALL tab_2d_1d( npti, nptidx(1:npti), t_su_1d(1:npti), t_su(:,:,kl)     )
         CALL tab_2d_1d( npti, nptidx(1:npti), s_i_1d (1:npti), s_i (:,:,kl)     )
         DO jk = 1, nlay_s
            CALL tab_2d_1d( npti, nptidx(1:npti), t_s_1d(1:npti,jk), t_s(:,:,jk,kl)    )
            CALL tab_2d_1d( npti, nptidx(1:npti), e_s_1d(1:npti,jk), e_s(:,:,jk,kl)    )
         END DO
         DO jk = 1, nlay_i
            CALL tab_2d_1d( npti, nptidx(1:npti), t_i_1d (1:npti,jk), t_i (:,:,jk,kl)  )
            CALL tab_2d_1d( npti, nptidx(1:npti), e_i_1d (1:npti,jk), e_i (:,:,jk,kl)  )
            CALL tab_2d_1d( npti, nptidx(1:npti), sz_i_1d(1:npti,jk), sz_i(:,:,jk,kl)  )
         END DO
         CALL tab_2d_1d( npti, nptidx(1:npti), a_ip_1d     (1:npti), a_ip     (:,:,kl) )
         CALL tab_2d_1d( npti, nptidx(1:npti), h_ip_1d     (1:npti), h_ip     (:,:,kl) )
         CALL tab_2d_1d( npti, nptidx(1:npti), a_ip_frac_1d(1:npti), a_ip_frac(:,:,kl) )
         !
         CALL tab_2d_1d( npti, nptidx(1:npti), qprec_ice_1d  (1:npti), qprec_ice            )
         CALL tab_2d_1d( npti, nptidx(1:npti), qsr_ice_1d    (1:npti), qsr_ice (:,:,kl)     )
         CALL tab_2d_1d( npti, nptidx(1:npti), qns_ice_1d    (1:npti), qns_ice (:,:,kl)     )
         CALL tab_2d_1d( npti, nptidx(1:npti), evap_ice_1d   (1:npti), evap_ice(:,:,kl)     )
         CALL tab_2d_1d( npti, nptidx(1:npti), dqns_ice_1d   (1:npti), dqns_ice(:,:,kl)     )
         CALL tab_2d_1d( npti, nptidx(1:npti), t_bo_1d       (1:npti), t_bo                 )
         CALL tab_2d_1d( npti, nptidx(1:npti), sprecip_1d    (1:npti), sprecip              ) 
         CALL tab_2d_1d( npti, nptidx(1:npti), qsb_ice_bot_1d(1:npti), qsb_ice_bot          )
         CALL tab_2d_1d( npti, nptidx(1:npti), fhld_1d       (1:npti), fhld                 )
         
         CALL tab_2d_1d( npti, nptidx(1:npti), qml_ice_1d    (1:npti), qml_ice    (:,:,kl) )
         CALL tab_2d_1d( npti, nptidx(1:npti), qcn_ice_1d    (1:npti), qcn_ice    (:,:,kl) )
         CALL tab_2d_1d( npti, nptidx(1:npti), qtr_ice_top_1d(1:npti), qtr_ice_top(:,:,kl) )
         !
         CALL tab_2d_1d( npti, nptidx(1:npti), wfx_snw_sni_1d(1:npti), wfx_snw_sni   )
         CALL tab_2d_1d( npti, nptidx(1:npti), wfx_snw_sum_1d(1:npti), wfx_snw_sum   )
         CALL tab_2d_1d( npti, nptidx(1:npti), wfx_sub_1d    (1:npti), wfx_sub       )
         CALL tab_2d_1d( npti, nptidx(1:npti), wfx_snw_sub_1d(1:npti), wfx_snw_sub   )
         CALL tab_2d_1d( npti, nptidx(1:npti), wfx_ice_sub_1d(1:npti), wfx_ice_sub   )
         CALL tab_2d_1d( npti, nptidx(1:npti), wfx_err_sub_1d(1:npti), wfx_err_sub   )
         !
         CALL tab_2d_1d( npti, nptidx(1:npti), wfx_bog_1d (1:npti), wfx_bog          )
         CALL tab_2d_1d( npti, nptidx(1:npti), wfx_bom_1d (1:npti), wfx_bom          )
         CALL tab_2d_1d( npti, nptidx(1:npti), wfx_sum_1d (1:npti), wfx_sum          )
         CALL tab_2d_1d( npti, nptidx(1:npti), wfx_sni_1d (1:npti), wfx_sni          )
         CALL tab_2d_1d( npti, nptidx(1:npti), wfx_res_1d (1:npti), wfx_res          )
         CALL tab_2d_1d( npti, nptidx(1:npti), wfx_spr_1d (1:npti), wfx_spr          )
         CALL tab_2d_1d( npti, nptidx(1:npti), wfx_lam_1d (1:npti), wfx_lam          )
         CALL tab_2d_1d( npti, nptidx(1:npti), wfx_pnd_1d (1:npti), wfx_pnd          )
         !
         CALL tab_2d_1d( npti, nptidx(1:npti), sfx_bog_1d (1:npti), sfx_bog          )
         CALL tab_2d_1d( npti, nptidx(1:npti), sfx_bom_1d (1:npti), sfx_bom          )
         CALL tab_2d_1d( npti, nptidx(1:npti), sfx_sum_1d (1:npti), sfx_sum          )
         CALL tab_2d_1d( npti, nptidx(1:npti), sfx_sni_1d (1:npti), sfx_sni          )
         CALL tab_2d_1d( npti, nptidx(1:npti), sfx_bri_1d (1:npti), sfx_bri          )
         CALL tab_2d_1d( npti, nptidx(1:npti), sfx_res_1d (1:npti), sfx_res          )
         CALL tab_2d_1d( npti, nptidx(1:npti), sfx_sub_1d (1:npti), sfx_sub          )
         CALL tab_2d_1d( npti, nptidx(1:npti), sfx_lam_1d (1:npti), sfx_lam          )
         !
         CALL tab_2d_1d( npti, nptidx(1:npti), hfx_thd_1d    (1:npti), hfx_thd       )
         CALL tab_2d_1d( npti, nptidx(1:npti), hfx_spr_1d    (1:npti), hfx_spr       )
         CALL tab_2d_1d( npti, nptidx(1:npti), hfx_sum_1d    (1:npti), hfx_sum       )
         CALL tab_2d_1d( npti, nptidx(1:npti), hfx_bom_1d    (1:npti), hfx_bom       )
         CALL tab_2d_1d( npti, nptidx(1:npti), hfx_bog_1d    (1:npti), hfx_bog       )
         CALL tab_2d_1d( npti, nptidx(1:npti), hfx_dif_1d    (1:npti), hfx_dif       )
         CALL tab_2d_1d( npti, nptidx(1:npti), hfx_opw_1d    (1:npti), hfx_opw       )
         CALL tab_2d_1d( npti, nptidx(1:npti), hfx_snw_1d    (1:npti), hfx_snw       )
         CALL tab_2d_1d( npti, nptidx(1:npti), hfx_sub_1d    (1:npti), hfx_sub       )
         CALL tab_2d_1d( npti, nptidx(1:npti), hfx_res_1d    (1:npti), hfx_res       )
         CALL tab_2d_1d( npti, nptidx(1:npti), hfx_err_dif_1d(1:npti), hfx_err_dif   )
         CALL tab_2d_1d( npti, nptidx(1:npti), hfx_err_rem_1d(1:npti), hfx_err_rem   )
         CALL tab_2d_1d( npti, nptidx(1:npti), qt_oce_ai_1d  (1:npti), qt_oce_ai     )
         !
         ! ocean surface fields
         CALL tab_2d_1d( npti, nptidx(1:npti), sst_1d(1:npti), sst_m )
         CALL tab_2d_1d( npti, nptidx(1:npti), sss_1d(1:npti), sss_m )
         !
         ! to update ice age
         CALL tab_2d_1d( npti, nptidx(1:npti), o_i_1d (1:npti), o_i (:,:,kl) )
         CALL tab_2d_1d( npti, nptidx(1:npti), oa_i_1d(1:npti), oa_i(:,:,kl) )
         !
         ! --- Change units of e_i, e_s from J/m2 to J/m3 --- !
         DO jk = 1, nlay_i
            WHERE( h_i_1d(1:npti)>0._wp ) e_i_1d(1:npti,jk) = e_i_1d(1:npti,jk) / (h_i_1d(1:npti) * a_i_1d(1:npti)) * nlay_i
         END DO
         DO jk = 1, nlay_s
            WHERE( h_s_1d(1:npti)>0._wp ) e_s_1d(1:npti,jk) = e_s_1d(1:npti,jk) / (h_s_1d(1:npti) * a_i_1d(1:npti)) * nlay_s
         END DO
         !
         !                 !---------------------!
      CASE( 2 )            !==  from 1D to 2D  ==!
         !                 !---------------------!
         ! --- Change units of e_i, e_s from J/m3 to J/m2 --- !
         DO jk = 1, nlay_i
            e_i_1d(1:npti,jk) = e_i_1d(1:npti,jk) * h_i_1d(1:npti) * a_i_1d(1:npti) * r1_nlay_i
         END DO
         DO jk = 1, nlay_s
            e_s_1d(1:npti,jk) = e_s_1d(1:npti,jk) * h_s_1d(1:npti) * a_i_1d(1:npti) * r1_nlay_s
         END DO
         !
         ! Change thickness to volume (replaces routine ice_var_eqv2glo)
         v_i_1d (1:npti) = h_i_1d (1:npti) * a_i_1d (1:npti)
         v_s_1d (1:npti) = h_s_1d (1:npti) * a_i_1d (1:npti)
         sv_i_1d(1:npti) = s_i_1d (1:npti) * v_i_1d (1:npti)
         v_ip_1d(1:npti) = h_ip_1d(1:npti) * a_ip_1d(1:npti)
         oa_i_1d(1:npti) = o_i_1d (1:npti) * a_i_1d (1:npti)
         
         CALL tab_1d_2d( npti, nptidx(1:npti), at_i_1d(1:npti), at_i             )
         CALL tab_1d_2d( npti, nptidx(1:npti), a_i_1d (1:npti), a_i (:,:,kl)     )
         CALL tab_1d_2d( npti, nptidx(1:npti), h_i_1d (1:npti), h_i (:,:,kl)     )
         CALL tab_1d_2d( npti, nptidx(1:npti), h_s_1d (1:npti), h_s (:,:,kl)     )
         CALL tab_1d_2d( npti, nptidx(1:npti), t_su_1d(1:npti), t_su(:,:,kl)     )
         CALL tab_1d_2d( npti, nptidx(1:npti), s_i_1d (1:npti), s_i (:,:,kl)     )
         DO jk = 1, nlay_s
            CALL tab_1d_2d( npti, nptidx(1:npti), t_s_1d(1:npti,jk), t_s(:,:,jk,kl)    )
            CALL tab_1d_2d( npti, nptidx(1:npti), e_s_1d(1:npti,jk), e_s(:,:,jk,kl)    )
         END DO
         DO jk = 1, nlay_i
            CALL tab_1d_2d( npti, nptidx(1:npti), t_i_1d (1:npti,jk), t_i (:,:,jk,kl)  )
            CALL tab_1d_2d( npti, nptidx(1:npti), e_i_1d (1:npti,jk), e_i (:,:,jk,kl)  )
            CALL tab_1d_2d( npti, nptidx(1:npti), sz_i_1d(1:npti,jk), sz_i(:,:,jk,kl)  )
         END DO
         CALL tab_1d_2d( npti, nptidx(1:npti), a_ip_1d     (1:npti), a_ip     (:,:,kl) )
         CALL tab_1d_2d( npti, nptidx(1:npti), h_ip_1d     (1:npti), h_ip     (:,:,kl) )
         CALL tab_1d_2d( npti, nptidx(1:npti), a_ip_frac_1d(1:npti), a_ip_frac(:,:,kl) )
         !
         CALL tab_1d_2d( npti, nptidx(1:npti), wfx_snw_sni_1d(1:npti), wfx_snw_sni )
         CALL tab_1d_2d( npti, nptidx(1:npti), wfx_snw_sum_1d(1:npti), wfx_snw_sum )
         CALL tab_1d_2d( npti, nptidx(1:npti), wfx_sub_1d    (1:npti), wfx_sub     )
         CALL tab_1d_2d( npti, nptidx(1:npti), wfx_snw_sub_1d(1:npti), wfx_snw_sub )
         CALL tab_1d_2d( npti, nptidx(1:npti), wfx_ice_sub_1d(1:npti), wfx_ice_sub )
         CALL tab_1d_2d( npti, nptidx(1:npti), wfx_err_sub_1d(1:npti), wfx_err_sub )
         !
         CALL tab_1d_2d( npti, nptidx(1:npti), wfx_bog_1d (1:npti), wfx_bog        )
         CALL tab_1d_2d( npti, nptidx(1:npti), wfx_bom_1d (1:npti), wfx_bom        )
         CALL tab_1d_2d( npti, nptidx(1:npti), wfx_sum_1d (1:npti), wfx_sum        )
         CALL tab_1d_2d( npti, nptidx(1:npti), wfx_sni_1d (1:npti), wfx_sni        )
         CALL tab_1d_2d( npti, nptidx(1:npti), wfx_res_1d (1:npti), wfx_res        )
         CALL tab_1d_2d( npti, nptidx(1:npti), wfx_spr_1d (1:npti), wfx_spr        )
         CALL tab_1d_2d( npti, nptidx(1:npti), wfx_lam_1d (1:npti), wfx_lam        )
         CALL tab_1d_2d( npti, nptidx(1:npti), wfx_pnd_1d (1:npti), wfx_pnd        )
         !
         CALL tab_1d_2d( npti, nptidx(1:npti), sfx_bog_1d (1:npti), sfx_bog        )
         CALL tab_1d_2d( npti, nptidx(1:npti), sfx_bom_1d (1:npti), sfx_bom        )
         CALL tab_1d_2d( npti, nptidx(1:npti), sfx_sum_1d (1:npti), sfx_sum        )
         CALL tab_1d_2d( npti, nptidx(1:npti), sfx_sni_1d (1:npti), sfx_sni        )
         CALL tab_1d_2d( npti, nptidx(1:npti), sfx_bri_1d (1:npti), sfx_bri        )
         CALL tab_1d_2d( npti, nptidx(1:npti), sfx_res_1d (1:npti), sfx_res        )
         CALL tab_1d_2d( npti, nptidx(1:npti), sfx_sub_1d (1:npti), sfx_sub        )
         CALL tab_1d_2d( npti, nptidx(1:npti), sfx_lam_1d (1:npti), sfx_lam        )
         !
         CALL tab_1d_2d( npti, nptidx(1:npti), hfx_thd_1d    (1:npti), hfx_thd     )
         CALL tab_1d_2d( npti, nptidx(1:npti), hfx_spr_1d    (1:npti), hfx_spr     )
         CALL tab_1d_2d( npti, nptidx(1:npti), hfx_sum_1d    (1:npti), hfx_sum     )
         CALL tab_1d_2d( npti, nptidx(1:npti), hfx_bom_1d    (1:npti), hfx_bom     )
         CALL tab_1d_2d( npti, nptidx(1:npti), hfx_bog_1d    (1:npti), hfx_bog     )
         CALL tab_1d_2d( npti, nptidx(1:npti), hfx_dif_1d    (1:npti), hfx_dif     )
         CALL tab_1d_2d( npti, nptidx(1:npti), hfx_opw_1d    (1:npti), hfx_opw     )
         CALL tab_1d_2d( npti, nptidx(1:npti), hfx_snw_1d    (1:npti), hfx_snw     )
         CALL tab_1d_2d( npti, nptidx(1:npti), hfx_sub_1d    (1:npti), hfx_sub     )
         CALL tab_1d_2d( npti, nptidx(1:npti), hfx_res_1d    (1:npti), hfx_res     )
         CALL tab_1d_2d( npti, nptidx(1:npti), hfx_err_dif_1d(1:npti), hfx_err_dif )
         CALL tab_1d_2d( npti, nptidx(1:npti), hfx_err_rem_1d(1:npti), hfx_err_rem )
         CALL tab_1d_2d( npti, nptidx(1:npti), qt_oce_ai_1d  (1:npti), qt_oce_ai   )
         !
         CALL tab_1d_2d( npti, nptidx(1:npti), qns_ice_1d    (1:npti), qns_ice    (:,:,kl) )
         CALL tab_1d_2d( npti, nptidx(1:npti), qtr_ice_bot_1d(1:npti), qtr_ice_bot(:,:,kl) )
         ! effective conductivity and 1st layer temperature (ln_cndflx=T)
         CALL tab_1d_2d( npti, nptidx(1:npti), cnd_ice_1d(1:npti), cnd_ice(:,:,kl) )
         CALL tab_1d_2d( npti, nptidx(1:npti), t1_ice_1d (1:npti), t1_ice (:,:,kl) )
         ! SIMIP diagnostics         
         CALL tab_1d_2d( npti, nptidx(1:npti), t_si_1d       (1:npti), t_si       (:,:,kl) )
         CALL tab_1d_2d( npti, nptidx(1:npti), qcn_ice_bot_1d(1:npti), qcn_ice_bot(:,:,kl) )
         CALL tab_1d_2d( npti, nptidx(1:npti), qcn_ice_top_1d(1:npti), qcn_ice_top(:,:,kl) )
         ! extensive variables
         CALL tab_1d_2d( npti, nptidx(1:npti), v_i_1d (1:npti), v_i (:,:,kl) )
         CALL tab_1d_2d( npti, nptidx(1:npti), v_s_1d (1:npti), v_s (:,:,kl) )
         CALL tab_1d_2d( npti, nptidx(1:npti), sv_i_1d(1:npti), sv_i(:,:,kl) )
         CALL tab_1d_2d( npti, nptidx(1:npti), v_ip_1d(1:npti), v_ip(:,:,kl) )
         CALL tab_1d_2d( npti, nptidx(1:npti), oa_i_1d(1:npti), oa_i(:,:,kl) )
         !
      END SELECT
      !
   END SUBROUTINE ice_thd_1d2d


   SUBROUTINE ice_thd_init
      !!-------------------------------------------------------------------
      !!                   ***  ROUTINE ice_thd_init *** 
      !!                 
      !! ** Purpose :   Physical constants and parameters associated with
      !!                ice thermodynamics
      !!
      !! ** Method  :   Read the namthd namelist and check the parameters
      !!                called at the first timestep (nit000)
      !!
      !! ** input   :   Namelist namthd
      !!-------------------------------------------------------------------
      INTEGER  ::   ios   ! Local integer output status for namelist read
      !!
      NAMELIST/namthd/ ln_icedH, ln_icedA, ln_icedO, ln_icedS
      !!-------------------------------------------------------------------
      !
      REWIND( numnam_ice_ref )              ! Namelist namthd in reference namelist : Ice thermodynamics
      READ  ( numnam_ice_ref, namthd, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namthd in reference namelist', lwp )
      REWIND( numnam_ice_cfg )              ! Namelist namthd in configuration namelist : Ice thermodynamics
      READ  ( numnam_ice_cfg, namthd, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namthd in configuration namelist', lwp )
      IF(lwm) WRITE( numoni, namthd )
      !
      IF(lwp) THEN                          ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'ice_thd_init: Ice Thermodynamics'
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namthd:'
         WRITE(numout,*) '      activate ice thick change from top/bot (T) or not (F)   ln_icedH  = ', ln_icedH
         WRITE(numout,*) '      activate lateral melting (T) or not (F)                 ln_icedA  = ', ln_icedA
         WRITE(numout,*) '      activate ice growth in open-water (T) or not (F)        ln_icedO  = ', ln_icedO
         WRITE(numout,*) '      activate gravity drainage and flushing (T) or not (F)   ln_icedS  = ', ln_icedS
     ENDIF
      !
                       CALL ice_thd_zdf_init   ! set ice heat diffusion parameters
      IF( ln_icedA )   CALL ice_thd_da_init    ! set ice lateral melting parameters
      IF( ln_icedO )   CALL ice_thd_do_init    ! set ice growth in open water parameters
                       CALL ice_thd_sal_init   ! set ice salinity parameters
                       CALL ice_thd_pnd_init   ! set melt ponds parameters
      !
   END SUBROUTINE ice_thd_init

#else
   !!----------------------------------------------------------------------
   !!   Default option         Dummy module          NO  SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE icethd
