MODULE icedia
   !!======================================================================
   !!                       ***  MODULE icedia  ***
   !!  Sea-Ice:   global budgets 
   !!======================================================================
   !! History :  3.4  !  2012-10  (C. Rousset)       original code
   !!            4.0  !  2018     (many people)      SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!    ice_dia      : diagnostic of the sea-ice global heat content, salt content and volume conservation
   !!    ice_dia_init : initialization of budget calculation
   !!    ice_dia_rst  : read/write budgets restart
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean domain
   USE phycst         ! physical constant
   USE daymod         ! model calendar
   USE sbc_oce , ONLY : sfx, nn_fsbc   ! surface boundary condition: ocean fields
   USE ice            ! sea-ice: variables
   USE icerst         ! sea-ice: restart
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O manager library
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! fortran utilities (glob_sum + no signed zero)
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_dia        ! called by icestp.F90
   PUBLIC   ice_dia_init   ! called in icestp.F90

   REAL(wp), DIMENSION(:,:), ALLOCATABLE ::   vol_loc_ini, sal_loc_ini, tem_loc_ini ! initial volume, salt and heat contents
   REAL(wp)                              ::   frc_sal, frc_voltop, frc_volbot, frc_temtop, frc_tembot  ! global forcing trends
   
   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/ICE 4.0 , NEMO Consortium (2018)
   !! $Id: icedia.F90 10425 2018-12-19 21:54:16Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION ice_dia_alloc()
      !!---------------------------------------------------------------------!
      !!                ***  ROUTINE ice_dia_alloc ***
      !!---------------------------------------------------------------------!
      ALLOCATE( vol_loc_ini(jpi,jpj), sal_loc_ini(jpi,jpj), tem_loc_ini(jpi,jpj), STAT=ice_dia_alloc )

      CALL mpp_sum ( 'icedia', ice_dia_alloc )
      IF( ice_dia_alloc /= 0 )   CALL ctl_stop( 'STOP',  'ice_dia_alloc: failed to allocate arrays'  )
      !
   END FUNCTION ice_dia_alloc

   SUBROUTINE ice_dia( kt )
      !!---------------------------------------------------------------------------
      !!                  ***  ROUTINE ice_dia  ***
      !!     
      !! ** Purpose:   Compute the sea-ice global heat content, salt content 
      !!             and volume conservation
      !!---------------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time step 
      !!
      REAL(wp)   ::   zbg_ivol, zbg_item, zbg_area, zbg_isal
      REAL(wp)   ::   zbg_svol, zbg_stem
      REAL(wp)   ::   z_frc_voltop, z_frc_temtop, z_frc_sal
      REAL(wp)   ::   z_frc_volbot, z_frc_tembot  
      REAL(wp)   ::   zdiff_vol, zdiff_sal, zdiff_tem  
      !!---------------------------------------------------------------------------
      IF( ln_timing )   CALL timing_start('ice_dia')

      IF( kt == nit000 .AND. lwp ) THEN
         WRITE(numout,*)
         WRITE(numout,*)'icedia: output ice diagnostics (integrated over the domain)'
         WRITE(numout,*)'~~~~~~'
      ENDIF

!!gm glob_sum includes a " * tmask_i ", so remove  " * tmask(:,:,1) "

      ! ----------------------- !
      ! 1 -  Contents !
      ! ----------------------- !
      zbg_ivol = glob_sum( 'icedia', vt_i(:,:) * e1e2t(:,:) ) * 1.e-9                  ! ice volume (km3)
      zbg_svol = glob_sum( 'icedia', vt_s(:,:) * e1e2t(:,:) ) * 1.e-9                  ! snow volume (km3)
      zbg_area = glob_sum( 'icedia', at_i(:,:) * e1e2t(:,:) ) * 1.e-6                  ! area (km2)
      zbg_isal = glob_sum( 'icedia', SUM( sv_i(:,:,:), dim=3 ) * e1e2t(:,:) ) * 1.e-9  ! salt content (pss*km3)
      zbg_item = glob_sum( 'icedia', et_i * e1e2t(:,:) ) * 1.e-20                      ! heat content (1.e20 J)
      zbg_stem = glob_sum( 'icedia', et_s * e1e2t(:,:) ) * 1.e-20                      ! heat content (1.e20 J)
      
      ! ---------------------------!
      ! 2 - Trends due to forcing  !
      ! ---------------------------!
      z_frc_volbot = r1_rau0 * glob_sum( 'icedia', -( wfx_ice(:,:) + wfx_snw(:,:) + wfx_err_sub(:,:) ) * e1e2t(:,:) ) * 1.e-9   ! freshwater flux ice/snow-ocean 
      z_frc_voltop = r1_rau0 * glob_sum( 'icedia', -( wfx_sub(:,:) + wfx_spr(:,:) )                    * e1e2t(:,:) ) * 1.e-9   ! freshwater flux ice/snow-atm
      z_frc_sal    = r1_rau0 * glob_sum( 'icedia', -      sfx(:,:)                                     * e1e2t(:,:) ) * 1.e-9   ! salt fluxes ice/snow-ocean
      z_frc_tembot =           glob_sum( 'icedia',  qt_oce_ai(:,:)                                     * e1e2t(:,:) ) * 1.e-20  ! heat on top of ocean (and below ice)
      z_frc_temtop =           glob_sum( 'icedia',  qt_atm_oi(:,:)                                     * e1e2t(:,:) ) * 1.e-20  ! heat on top of ice-coean
      !
      frc_voltop  = frc_voltop  + z_frc_voltop  * rdt_ice ! km3
      frc_volbot  = frc_volbot  + z_frc_volbot  * rdt_ice ! km3
      frc_sal     = frc_sal     + z_frc_sal     * rdt_ice ! km3*pss
      frc_temtop  = frc_temtop  + z_frc_temtop  * rdt_ice ! 1.e20 J
      frc_tembot  = frc_tembot  + z_frc_tembot  * rdt_ice ! 1.e20 J
            
      ! ----------------------- !
      ! 3 -  Content variations !
      ! ----------------------- !
      zdiff_vol = r1_rau0 * glob_sum( 'icedia', ( rhoi*vt_i(:,:) + rhos*vt_s(:,:) - vol_loc_ini(:,:) ) * e1e2t(:,:) ) * 1.e-9   ! freshwater trend (km3) 
      zdiff_sal = r1_rau0 * glob_sum( 'icedia', ( rhoi* SUM( sv_i(:,:,:), dim=3 ) - sal_loc_ini(:,:) ) * e1e2t(:,:) ) * 1.e-9   ! salt content trend (km3*pss)
      zdiff_tem =           glob_sum( 'icedia', ( et_i(:,:) + et_s(:,:)           - tem_loc_ini(:,:) ) * e1e2t(:,:) ) * 1.e-20  ! heat content trend (1.e20 J)
      !                               + SUM( qevap_ice * a_i_b, dim=3 )       !! clem: I think this term should not be there (but needs a check)

      ! ----------------------- !
      ! 4 -  Drifts             !
      ! ----------------------- !
      zdiff_vol = zdiff_vol - ( frc_voltop + frc_volbot )
      zdiff_sal = zdiff_sal - frc_sal
      zdiff_tem = zdiff_tem - ( frc_tembot - frc_temtop )

      ! ----------------------- !
      ! 5 - Diagnostics writing !
      ! ----------------------- !
!!gm I don't understand the division by the ocean surface (i.e. glob_sum( 'icedia', e1e2t(:,:) ) * 1.e-20 * kt*rdt )
!!   and its multiplication bu kt ! is it really what we want ? what is this quantity ?
!!   IF it is really what we want, compute it at kt=nit000, not 3 time by time-step !
!!   kt*rdt  : you mean rdtice ?
!!gm
      !
      IF( iom_use('ibgvolume')    )   CALL iom_put( 'ibgvolume' , zdiff_vol     )   ! ice/snow volume  drift            (km3 equivalent ocean water)         
      IF( iom_use('ibgsaltco')    )   CALL iom_put( 'ibgsaltco' , zdiff_sal     )   ! ice salt content drift            (psu*km3 equivalent ocean water)
      IF( iom_use('ibgheatco')    )   CALL iom_put( 'ibgheatco' , zdiff_tem     )   ! ice/snow heat content drift       (1.e20 J)
      IF( iom_use('ibgheatfx')    )   CALL iom_put( 'ibgheatfx' ,               &   ! ice/snow heat flux drift          (W/m2)
         &                                                     zdiff_tem /glob_sum( 'icedia', e1e2t(:,:) * 1.e-20 * kt*rdt ) )

      IF( iom_use('ibgfrcvoltop') )   CALL iom_put( 'ibgfrcvoltop' , frc_voltop )   ! vol  forcing ice/snw-atm          (km3 equivalent ocean water) 
      IF( iom_use('ibgfrcvolbot') )   CALL iom_put( 'ibgfrcvolbot' , frc_volbot )   ! vol  forcing ice/snw-ocean        (km3 equivalent ocean water) 
      IF( iom_use('ibgfrcsal')    )   CALL iom_put( 'ibgfrcsal'    , frc_sal    )   ! sal - forcing                     (psu*km3 equivalent ocean water)   
      IF( iom_use('ibgfrctemtop') )   CALL iom_put( 'ibgfrctemtop' , frc_temtop )   ! heat on top of ice/snw/ocean      (1.e20 J)   
      IF( iom_use('ibgfrctembot') )   CALL iom_put( 'ibgfrctembot' , frc_tembot )   ! heat on top of ocean(below ice)   (1.e20 J)   
      IF( iom_use('ibgfrchfxtop') )   CALL iom_put( 'ibgfrchfxtop' ,            &   ! heat on top of ice/snw/ocean      (W/m2) 
         &                                                          frc_temtop / glob_sum( 'icedia', e1e2t(:,:) ) * 1.e-20 * kt*rdt  )
      IF( iom_use('ibgfrchfxbot') )   CALL iom_put( 'ibgfrchfxbot' ,            &   ! heat on top of ocean(below ice)   (W/m2) 
         &                                                          frc_tembot / glob_sum( 'icedia', e1e2t(:,:) ) * 1.e-20 * kt*rdt  )

      IF( iom_use('ibgvol_tot' )  )   CALL iom_put( 'ibgvol_tot'  , zbg_ivol     )   ! ice volume                       (km3)
      IF( iom_use('sbgvol_tot' )  )   CALL iom_put( 'sbgvol_tot'  , zbg_svol     )   ! snow volume                      (km3)
      IF( iom_use('ibgarea_tot')  )   CALL iom_put( 'ibgarea_tot' , zbg_area     )   ! ice area                         (km2)
      IF( iom_use('ibgsalt_tot')  )   CALL iom_put( 'ibgsalt_tot' , zbg_isal     )   ! ice salinity content             (pss*km3)
      IF( iom_use('ibgheat_tot')  )   CALL iom_put( 'ibgheat_tot' , zbg_item     )   ! ice heat content                 (1.e20 J)
      IF( iom_use('sbgheat_tot')  )   CALL iom_put( 'sbgheat_tot' , zbg_stem     )   ! snow heat content                (1.e20 J)
      !
      IF( lrst_ice )   CALL ice_dia_rst( 'WRITE', kt_ice )
      !
      IF( ln_timing )   CALL timing_stop('ice_dia')
      !
   END SUBROUTINE ice_dia


   SUBROUTINE ice_dia_init
      !!---------------------------------------------------------------------------
      !!                  ***  ROUTINE ice_dia_init  ***
      !!     
      !! ** Purpose: Initialization for the heat salt volume budgets
      !!	
      !! ** Method : Compute initial heat content, salt content and volume
      !!
      !! ** Action : - Compute initial heat content, salt content and volume
      !!             - Initialize forcing trends
      !!             - Compute coefficients for conversion
      !!---------------------------------------------------------------------------
      INTEGER            ::   ios, ierror   ! local integer
      !!
      NAMELIST/namdia/ ln_icediachk, ln_icediahsb, ln_icectl, iiceprt, jiceprt  
      !!----------------------------------------------------------------------
      !
      REWIND( numnam_ice_ref )      ! Namelist namdia in reference namelist : Parameters for ice
      READ  ( numnam_ice_ref, namdia, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namdia in reference namelist', lwp )
      REWIND( numnam_ice_cfg )      ! Namelist namdia in configuration namelist : Parameters for ice
      READ  ( numnam_ice_cfg, namdia, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namdia in configuration namelist', lwp )
      IF(lwm) WRITE ( numoni, namdia )
      !
      IF(lwp) THEN                  ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'ice_dia_init: ice diagnostics'
         WRITE(numout,*) ' ~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namdia:'
         WRITE(numout,*) '      Diagnose online heat/mass/salt budget      ln_icediachk = ', ln_icediachk
         WRITE(numout,*) '      Output          heat/mass/salt budget      ln_icediahsb = ', ln_icediahsb
         WRITE(numout,*) '      control prints for a given grid point      ln_icectl    = ', ln_icectl
         WRITE(numout,*) '         chosen grid point position         (iiceprt,jiceprt) = (', iiceprt,',', jiceprt,')'
      ENDIF
      !      
      IF( ln_icediahsb ) THEN
         IF( ice_dia_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'ice_dia_init : unable to allocate arrays' )   ! allocate tke arrays
         CALL ice_dia_rst( 'READ' )   ! read or initialize all required files
      ENDIF
      !
   END SUBROUTINE ice_dia_init


   SUBROUTINE ice_dia_rst( cdrw, kt )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE icedia_rst  ***
      !!                     
      !! ** Purpose :   Read or write DIA file in restart file
      !!
      !! ** Method  :   use of IOM library
      !!----------------------------------------------------------------------
      CHARACTER(len=*) , INTENT(in) ::   cdrw   ! "READ"/"WRITE" flag
      INTEGER, OPTIONAL, INTENT(in) ::   kt     ! ice time-step
      !
      INTEGER  ::   iter    ! local integer
      REAL(wp) ::   ziter   ! local scalar
      !!----------------------------------------------------------------------
      !
      IF( TRIM(cdrw) == 'READ' ) THEN        ! Read/initialise 
         IF( ln_rstart ) THEN                   !* Read the restart file
            !
            CALL iom_get( numrir, 'kt_ice' , ziter )
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'ice_dia_rst read at time step = ', ziter
            IF(lwp) WRITE(numout,*) '~~~~~~~~~~'
            CALL iom_get( numrir, 'frc_voltop' , frc_voltop  )
            CALL iom_get( numrir, 'frc_volbot' , frc_volbot  )
            CALL iom_get( numrir, 'frc_temtop' , frc_temtop  )
            CALL iom_get( numrir, 'frc_tembot' , frc_tembot  )
            CALL iom_get( numrir, 'frc_sal'    , frc_sal     )
            CALL iom_get( numrir, jpdom_autoglo, 'vol_loc_ini', vol_loc_ini )
            CALL iom_get( numrir, jpdom_autoglo, 'tem_loc_ini', tem_loc_ini )
            CALL iom_get( numrir, jpdom_autoglo, 'sal_loc_ini', sal_loc_ini )
         ELSE
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) ' ice_dia at initial state '
            IF(lwp) WRITE(numout,*) '~~~~~~~'
            ! set trends to 0
            frc_voltop  = 0._wp                                          
            frc_volbot  = 0._wp                                          
            frc_temtop  = 0._wp                                                 
            frc_tembot  = 0._wp                                                 
            frc_sal     = 0._wp                                                 
            ! record initial ice volume, salt and temp
            vol_loc_ini(:,:) = rhoi * vt_i(:,:) + rhos * vt_s(:,:)  ! ice/snow volume (kg/m2)
            tem_loc_ini(:,:) = et_i(:,:) + et_s(:,:)                ! ice/snow heat content (J)
            sal_loc_ini(:,:) = rhoi * SUM( sv_i(:,:,:), dim=3 )     ! ice salt content (pss*kg/m2)
         ENDIF
         !
      ELSEIF( TRIM(cdrw) == 'WRITE' ) THEN   ! Create restart file
         !                                   ! -------------------
         iter = kt + nn_fsbc - 1   ! ice restarts are written at kt == nitrst - nn_fsbc + 1
         !
         IF( iter == nitrst ) THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'ice_dia_rst write at time step = ', kt
            IF(lwp) WRITE(numout,*) '~~~~~~~~~~~'
         ENDIF
         !
         ! Write in numriw (if iter == nitrst)
         ! ------------------ 
         CALL iom_rstput( iter, nitrst, numriw, 'frc_voltop' , frc_voltop  )
         CALL iom_rstput( iter, nitrst, numriw, 'frc_volbot' , frc_volbot  )
         CALL iom_rstput( iter, nitrst, numriw, 'frc_temtop' , frc_temtop  )
         CALL iom_rstput( iter, nitrst, numriw, 'frc_tembot' , frc_tembot  )
         CALL iom_rstput( iter, nitrst, numriw, 'frc_sal'    , frc_sal     )
         CALL iom_rstput( iter, nitrst, numriw, 'vol_loc_ini', vol_loc_ini )
         CALL iom_rstput( iter, nitrst, numriw, 'tem_loc_ini', tem_loc_ini )
         CALL iom_rstput( iter, nitrst, numriw, 'sal_loc_ini', sal_loc_ini )
         !
      ENDIF
      !
   END SUBROUTINE ice_dia_rst
 
#else
   !!----------------------------------------------------------------------
   !!   Default option :         Empty module         NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE icedia
