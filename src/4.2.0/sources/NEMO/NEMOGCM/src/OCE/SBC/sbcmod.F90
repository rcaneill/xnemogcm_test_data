MODULE sbcmod
   !!======================================================================
   !!                       ***  MODULE  sbcmod  ***
   !! Surface module :  provide to the ocean its surface boundary condition
   !!======================================================================
   !! History :  3.0  ! 2006-07  (G. Madec)  Original code
   !!            3.1  ! 2008-08  (S. Masson, A. Caubel, E. Maisonnave, G. Madec) coupled interface
   !!            3.3  ! 2010-04  (M. Leclair, G. Madec)  Forcing averaged over 2 time steps
   !!            3.3  ! 2010-10  (S. Masson)  add diurnal cycle
   !!            3.3  ! 2010-09  (D. Storkey) add ice boundary conditions (BDY)
   !!             -   ! 2010-11  (G. Madec) ice-ocean stress always computed at each ocean time-step
   !!             -   ! 2010-10  (J. Chanut, C. Bricaud, G. Madec)  add the surface pressure forcing
   !!            3.4  ! 2011-11  (C. Harris) CICE added as an option
   !!            3.5  ! 2012-11  (A. Coward, G. Madec) Rethink of heat, mass and salt surface fluxes
   !!            3.6  ! 2014-11  (P. Mathiot, C. Harris) add ice shelves melting
   !!            4.0  ! 2016-06  (L. Brodeau) new general bulk formulation
   !!            4.0  ! 2019-03  (F. Lemarié & G. Samson)  add ABL compatibility (ln_abl=TRUE)
   !!            4.2  ! 2020-12  (G. Madec, E. Clementi) modified wave forcing and coupling
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   sbc_init      : read namsbc namelist
   !!   sbc           : surface ocean momentum, heat and freshwater boundary conditions
   !!   sbc_final     : Finalize CICE ice model (if used)
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
   USE dom_oce        ! ocean space and time domain
   USE closea         ! closed seas
   USE phycst         ! physical constants
   USE sbc_phy, ONLY : pp_cldf
   USE sbc_oce        ! Surface boundary condition: ocean fields
   USE trc_oce        ! shared ocean-passive tracers variables
   USE sbc_ice        ! Surface boundary condition: ice fields
   USE sbcdcy         ! surface boundary condition: diurnal cycle
   USE sbcssm         ! surface boundary condition: sea-surface mean variables
   USE sbcflx         ! surface boundary condition: flux formulation
   USE sbcblk         ! surface boundary condition: bulk formulation
   USE sbcabl         ! atmospheric boundary layer
   USE sbcice_if      ! surface boundary condition: ice-if sea-ice model
#if defined key_si3
   USE icestp         ! surface boundary condition: SI3 sea-ice model
#endif
   USE sbcice_cice    ! surface boundary condition: CICE sea-ice model
   USE sbccpl         ! surface boundary condition: coupled formulation
   USE cpl_oasis3     ! OASIS routines for coupling
   USE sbcclo         ! surface boundary condition: closed sea correction
   USE sbcssr         ! surface boundary condition: sea surface restoring
   USE sbcrnf         ! surface boundary condition: runoffs
   USE sbcapr         ! surface boundary condition: atmo pressure
   USE sbcfwb         ! surface boundary condition: freshwater budget
   USE icbstp         ! Icebergs
   USE icb_oce  , ONLY : ln_passive_mode      ! iceberg interaction mode
   USE traqsr         ! active tracers: light penetration
   USE sbcwave        ! Wave module
   USE bdy_oce   , ONLY: ln_bdy
   USE usrdef_sbc     ! user defined: surface boundary condition
   USE closea         ! closed sea
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   !
   USE prtctl         ! Print control                    (prt_ctl routine)
   USE iom            ! IOM library
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE timing         ! Timing
   USE wet_dry
   USE diu_bulk, ONLY:   ln_diurnal_only   ! diurnal SST diagnostic

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sbc        ! routine called by step.F90
   PUBLIC   sbc_init   ! routine called by opa.F90

   INTEGER ::   nsbc   ! type of surface boundary condition (deduced from namsbc informations)
   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: sbcmod.F90 15372 2021-10-14 15:47:24Z davestorkey $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE sbc_init( Kbb, Kmm, Kaa )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE sbc_init ***
      !!
      !! ** Purpose :   Initialisation of the ocean surface boundary computation
      !!
      !! ** Method  :   Read the namsbc namelist and set derived parameters
      !!                Call init routines for all other SBC modules that have one
      !!
      !! ** Action  : - read namsbc parameters
      !!              - nsbc: type of sbc
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   Kbb, Kmm, Kaa         ! ocean time level indices
      INTEGER ::   ios, icpt                         ! local integer
      LOGICAL ::   ll_purecpl, ll_opa, ll_not_nemo   ! local logical
      !!
      NAMELIST/namsbc/ nn_fsbc  ,                                                    &
         &             ln_usr   , ln_flx   , ln_blk   , ln_abl,                      &
         &             ln_cpl   , ln_mixcpl, nn_components,                          &
         &             nn_ice   , ln_ice_embd,                                       &
         &             ln_traqsr, ln_dm2dc ,                                         &
         &             ln_rnf   , nn_fwb     , ln_ssr   , ln_apr_dyn,                &
         &             ln_wave  , nn_lsm
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'sbc_init : surface boundary condition setting'
         WRITE(numout,*) '~~~~~~~~ '
      ENDIF
      !
      !                       !**  read Surface Module namelist
      READ  ( numnam_ref, namsbc, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namsbc in reference namelist' )
      READ  ( numnam_cfg, namsbc, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namsbc in configuration namelist' )
      IF(lwm) WRITE( numond, namsbc )
      !
#if ! defined key_mpi_off
      ncom_fsbc = nn_fsbc    ! make nn_fsbc available for lib_mpp
#endif
#if ! defined key_si3
      IF( nn_ice == 2 )    nn_ice = 0  ! without key key_si3 you cannot use si3...
#endif
      !
      !
      IF(lwp) THEN                  !* Control print
         WRITE(numout,*) '   Namelist namsbc (partly overwritten with CPP key setting)'
         WRITE(numout,*) '      frequency update of sbc (and ice)             nn_fsbc       = ', nn_fsbc
         WRITE(numout,*) '      Type of air-sea fluxes : '
         WRITE(numout,*) '         user defined formulation                   ln_usr        = ', ln_usr
         WRITE(numout,*) '         flux         formulation                   ln_flx        = ', ln_flx
         WRITE(numout,*) '         bulk         formulation                   ln_blk        = ', ln_blk
         WRITE(numout,*) '         ABL          formulation                   ln_abl        = ', ln_abl
         WRITE(numout,*) '         Surface wave (forced or coupled)           ln_wave       = ', ln_wave
         WRITE(numout,*) '      Type of coupling (Ocean/Ice/Atmosphere) : '
         WRITE(numout,*) '         ocean-atmosphere coupled formulation       ln_cpl        = ', ln_cpl
         WRITE(numout,*) '         mixed forced-coupled     formulation       ln_mixcpl     = ', ln_mixcpl
!!gm  lk_oasis is controlled by key_oasis3  ===>>>  It shoud be removed from the namelist
         WRITE(numout,*) '         OASIS coupling (with atm or sas)           lk_oasis      = ', lk_oasis
         WRITE(numout,*) '         components of your executable              nn_components = ', nn_components
         WRITE(numout,*) '      Sea-ice : '
         WRITE(numout,*) '         ice management in the sbc (=0/1/2/3)       nn_ice        = ', nn_ice
         WRITE(numout,*) '         ice embedded into ocean                    ln_ice_embd   = ', ln_ice_embd
         WRITE(numout,*) '      Misc. options of sbc : '
         WRITE(numout,*) '         Light penetration in temperature Eq.       ln_traqsr     = ', ln_traqsr
         WRITE(numout,*) '            daily mean to diurnal cycle qsr            ln_dm2dc   = ', ln_dm2dc
         WRITE(numout,*) '         Sea Surface Restoring on SST and/or SSS    ln_ssr        = ', ln_ssr
         WRITE(numout,*) '         FreshWater Budget control  (=0/1/2)        nn_fwb        = ', nn_fwb
         WRITE(numout,*) '         Patm gradient added in ocean & ice Eqs.    ln_apr_dyn    = ', ln_apr_dyn
         WRITE(numout,*) '         runoff / runoff mouths                     ln_rnf        = ', ln_rnf
         WRITE(numout,*) '         nb of iterations if land-sea-mask applied  nn_lsm        = ', nn_lsm
      ENDIF
      !
      IF( .NOT.ln_usr ) THEN     ! the model calendar needs some specificities (except in user defined case)
         IF( MOD( rday , rn_Dt ) /= 0. )   CALL ctl_stop( 'the time step must devide the number of second of in a day' )
         IF( MOD( rday , 2.  ) /= 0. )   CALL ctl_stop( 'the number of second of in a day must be an even number'    )
         IF( MOD( rn_Dt  , 2.  ) /= 0. )   CALL ctl_stop( 'the time step (in second) must be an even number'           )
      ENDIF
      !                       !**  check option consistency
      !
      IF(lwp) WRITE(numout,*)       !* Single / Multi - executable (NEMO / OCE+SAS)
      SELECT CASE( nn_components )
      CASE( jp_iam_nemo )
         IF(lwp) WRITE(numout,*) '   ==>>>   NEMO configured as a single executable (i.e. including both OCE and Surface module)'
      CASE( jp_iam_oce  )
         IF(lwp) WRITE(numout,*) '   ==>>>   Multi executable configuration. Here, OCE component'
         IF( .NOT.lk_oasis )   CALL ctl_stop( 'sbc_init : OCE-SAS coupled via OASIS, but key_oasis3 disabled' )
         IF( ln_cpl        )   CALL ctl_stop( 'sbc_init : OCE-SAS coupled via OASIS, but ln_cpl = T in OCE'   )
         IF( ln_mixcpl     )   CALL ctl_stop( 'sbc_init : OCE-SAS coupled via OASIS, but ln_mixcpl = T in OCE' )
      CASE( jp_iam_sas  )
         IF(lwp) WRITE(numout,*) '   ==>>>   Multi executable configuration. Here, SAS component'
         IF( .NOT.lk_oasis )   CALL ctl_stop( 'sbc_init : OCE-SAS coupled via OASIS, but key_oasis3 disabled' )
         IF( ln_mixcpl     )   CALL ctl_stop( 'sbc_init : OCE-SAS coupled via OASIS, but ln_mixcpl = T in OCE' )
      CASE DEFAULT
         CALL ctl_stop( 'sbc_init : unsupported value for nn_components' )
      END SELECT
      !                             !* coupled options
      IF( ln_cpl ) THEN
         IF( .NOT. lk_oasis )   CALL ctl_stop( 'sbc_init : coupled mode with an atmosphere model (ln_cpl=T)',   &
            &                                  '           required to defined key_oasis3' )
      ENDIF
      IF( ln_mixcpl ) THEN
         IF( .NOT. lk_oasis )   CALL ctl_stop( 'sbc_init : mixed forced-coupled mode (ln_mixcpl=T) ',   &
            &                                  '           required to defined key_oasis3' )
         IF( .NOT.ln_cpl    )   CALL ctl_stop( 'sbc_init : mixed forced-coupled mode (ln_mixcpl=T) requires ln_cpl = T' )
         IF( nn_components /= jp_iam_nemo )    &
            &                   CALL ctl_stop( 'sbc_init : the mixed forced-coupled mode (ln_mixcpl=T) ',   &
            &                                   '          not yet working with sas-opa coupling via oasis' )
      ENDIF
      !                             !* sea-ice
      SELECT CASE( nn_ice )
      CASE( 0 )                        !- no ice in the domain
      CASE( 1 )                        !- Ice-cover climatology ("Ice-if" model)
      CASE( 2 )                        !- SI3  ice model
         IF( .NOT.( ln_blk .OR. ln_cpl .OR. ln_abl .OR. ln_usr ) )   &
            &                   CALL ctl_stop( 'sbc_init : SI3 sea-ice model requires ln_blk or ln_cpl or ln_abl or ln_usr = T' )
      CASE( 3 )                        !- CICE ice model
         IF( .NOT.( ln_blk .OR. ln_cpl .OR. ln_abl .OR. ln_usr ) )   &
            &                   CALL ctl_stop( 'sbc_init : CICE sea-ice model requires ln_blk or ln_cpl or ln_abl or ln_usr = T' )
         IF( lk_agrif                                )   &
            &                   CALL ctl_stop( 'sbc_init : CICE sea-ice model not currently available with AGRIF' )
      CASE DEFAULT                     !- not supported
      END SELECT
      IF( ln_diurnal .AND. .NOT. (ln_blk.OR.ln_abl) )   CALL ctl_stop( "sbc_init: diurnal flux processing only implemented for bulk forcing" )
      !
      !                       !**  allocate and set required variables
      !
      !                             !* allocate sbc arrays
      IF( sbc_oce_alloc() /= 0 )   CALL ctl_stop( 'sbc_init : unable to allocate sbc_oce arrays' )
#if ! defined key_si3 && ! defined key_cice
      IF( sbc_ice_alloc() /= 0 )   CALL ctl_stop( 'sbc_init : unable to allocate sbc_ice arrays' )
#endif
      !
      !
      IF( sbc_ssr_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'sbc_init : unable to allocate sbc_ssr arrays' )
      IF( .NOT.ln_ssr ) THEN               !* Initialize qrp and erp if no restoring
         qrp(:,:) = 0._wp
         erp(:,:) = 0._wp
      ENDIF
      !
      IF( nn_ice == 0 ) THEN        !* No sea-ice in the domain : ice fraction is always zero
         IF( nn_components /= jp_iam_oce )   fr_i(:,:) = 0._wp    ! except for OCE in SAS-OCE coupled case
      ENDIF
      !
      sfx   (:,:) = 0._wp           !* salt flux due to freezing/melting
      fmmflx(:,:) = 0._wp           !* freezing minus melting flux
      cloud_fra(:,:) = pp_cldf      !* cloud fraction over sea ice (used in si3)

      taum(:,:) = 0._wp             !* wind stress module (needed in GLS in case of reduced restart)

      !                          ! Choice of the Surface Boudary Condition (set nsbc)
      nday_qsr = -1   ! allow initialization at the 1st call !LB: now warm-layer of COARE* calls "sbc_dcy_param" of sbcdcy.F90!
      IF( ln_dm2dc ) THEN           !* daily mean to diurnal cycle
         !LB:nday_qsr = -1   ! allow initialization at the 1st call
         IF( .NOT.( ln_flx .OR. ln_blk .OR. ln_abl ) .AND. nn_components /= jp_iam_oce )   &
            &   CALL ctl_stop( 'qsr diurnal cycle from daily values requires flux, bulk or abl formulation' )
      ENDIF
      !                             !* Choice of the Surface Boudary Condition
      !                             (set nsbc)
      !
      ll_purecpl  = ln_cpl .AND. .NOT.ln_mixcpl
      ll_opa      = nn_components == jp_iam_oce
      ll_not_nemo = nn_components /= jp_iam_nemo
      icpt = 0
      !
      IF( ln_usr          ) THEN   ;   nsbc = jp_usr     ; icpt = icpt + 1   ;   ENDIF       ! user defined         formulation
      IF( ln_flx          ) THEN   ;   nsbc = jp_flx     ; icpt = icpt + 1   ;   ENDIF       ! flux                 formulation
      IF( ln_blk          ) THEN   ;   nsbc = jp_blk     ; icpt = icpt + 1   ;   ENDIF       ! bulk                 formulation
      IF( ln_abl          ) THEN   ;   nsbc = jp_abl     ; icpt = icpt + 1   ;   ENDIF       ! ABL                  formulation
      IF( ll_purecpl      ) THEN   ;   nsbc = jp_purecpl ; icpt = icpt + 1   ;   ENDIF       ! Pure Coupled         formulation
      IF( ll_opa          ) THEN   ;   nsbc = jp_none    ; icpt = icpt + 1   ;   ENDIF       ! opa coupling via SAS module
      !
      IF( icpt /= 1 )    CALL ctl_stop( 'sbc_init : choose ONE and only ONE sbc option' )
      !
      IF(lwp) THEN                     !- print the choice of surface flux formulation
         WRITE(numout,*)
         SELECT CASE( nsbc )
         CASE( jp_usr     )   ;   WRITE(numout,*) '   ==>>>   user defined forcing formulation'
         CASE( jp_flx     )   ;   WRITE(numout,*) '   ==>>>   flux formulation'
         CASE( jp_blk     )   ;   WRITE(numout,*) '   ==>>>   bulk formulation'
         CASE( jp_abl     )   ;   WRITE(numout,*) '   ==>>>   ABL  formulation'
         CASE( jp_purecpl )   ;   WRITE(numout,*) '   ==>>>   pure coupled formulation'
!!gm abusive use of jp_none ??   ===>>> need to be check and changed by adding a jp_sas parameter
         CASE( jp_none    )   ;   WRITE(numout,*) '   ==>>>   OCE coupled to SAS via oasis'
            IF( ln_mixcpl )       WRITE(numout,*) '               + forced-coupled mixed formulation'
         END SELECT
         IF( ll_not_nemo  )       WRITE(numout,*) '               + OASIS coupled SAS'
      ENDIF
      !
      !                             !* OASIS initialization
      !
      IF( lk_oasis )   CALL sbc_cpl_init( nn_ice )   ! Must be done before: (1) first time step
      !                                              !                      (2) the use of nn_fsbc
      !     nn_fsbc initialization if OCE-SAS coupling via OASIS
      !     SAS time-step has to be declared in OASIS (mandatory) -> nn_fsbc has to be modified accordingly
      IF( nn_components /= jp_iam_nemo ) THEN
         IF( nn_components == jp_iam_oce )   nn_fsbc = cpl_freq('O_SFLX') / NINT(rn_Dt)
         IF( nn_components == jp_iam_sas )   nn_fsbc = cpl_freq('I_SFLX') / NINT(rn_Dt)
         !
         IF(lwp)THEN
            WRITE(numout,*)
            WRITE(numout,*)"   OCE-SAS coupled via OASIS : nn_fsbc re-defined from OASIS namcouple ", nn_fsbc
            WRITE(numout,*)
         ENDIF
      ENDIF
      !
      !                             !* check consistency between model timeline and nn_fsbc
      IF( ln_rst_list .OR. nn_stock /= -1 ) THEN   ! we will do restart files
         IF( MOD( nitend - nit000 + 1, nn_fsbc) /= 0 ) THEN
            WRITE(ctmp1,*) 'sbc_init : experiment length (', nitend - nit000 + 1, ') is NOT a multiple of nn_fsbc (', nn_fsbc, ')'
            CALL ctl_stop( ctmp1, 'Impossible to properly do model restart' )
         ENDIF
         IF( .NOT. ln_rst_list .AND. MOD( nn_stock, nn_fsbc) /= 0 ) THEN   ! we don't use nn_stock if ln_rst_list
            WRITE(ctmp1,*) 'sbc_init : nn_stock (', nn_stock, ') is NOT a multiple of nn_fsbc (', nn_fsbc, ')'
            CALL ctl_stop( ctmp1, 'Impossible to properly do model restart' )
         ENDIF
      ENDIF
      !
      IF( MOD( rday, REAL(nn_fsbc, wp) * rn_Dt ) /= 0 )   &
         &  CALL ctl_warn( 'sbc_init : nn_fsbc is NOT a multiple of the number of time steps in a day' )
      !
      IF( ln_dm2dc .AND. NINT(rday) / ( nn_fsbc * NINT(rn_Dt) ) < 8  )   &
         &   CALL ctl_warn( 'sbc_init : diurnal cycle for qsr: the sampling of the diurnal cycle is too small...' )
      !

      !                       !**  associated modules : initialization
      !
                          CALL sbc_ssm_init ( Kbb, Kmm ) ! Sea-surface mean fields initialization
      !
      IF( l_sbc_clo   )   CALL sbc_clo_init              ! closed sea surface initialisation
      !
      IF( ln_blk      )   CALL sbc_blk_init              ! bulk formulae initialization

      IF( ln_abl      )   CALL sbc_abl_init              ! Atmospheric Boundary Layer (ABL)

      IF( ln_ssr      )   CALL sbc_ssr_init              ! Sea-Surface Restoring initialization
      !
      !
                          CALL sbc_rnf_init( Kmm )       ! Runof initialization
      !
      IF( ln_apr_dyn )    CALL sbc_apr_init              ! Atmo Pressure Forcing initialization
      !
#if defined key_si3
      IF( lk_agrif .AND. nn_ice == 0 ) THEN            ! allocate ice arrays in case agrif + ice-model + no-ice in child grid
                          IF( sbc_ice_alloc() /= 0 )   CALL ctl_stop('STOP', 'sbc_ice_alloc : unable to allocate arrays' )
      ELSEIF( nn_ice == 2 ) THEN
                          CALL ice_init( Kbb, Kmm, Kaa )         ! ICE initialization
      ENDIF
#endif
      IF( nn_ice == 3 )   CALL cice_sbc_init( nsbc, Kbb, Kmm )   ! CICE initialization
      !
      IF( ln_wave     ) THEN
                          CALL sbc_wave_init                     ! surface wave initialisation
      ELSE
                          IF(lwp) WRITE(numout,*)
                          IF(lwp) WRITE(numout,*) '   No surface waves : all wave related logical set to false'
                          ln_sdw       = .false.
                          ln_stcor     = .false.
                          ln_cdgw      = .false.
                          ln_tauoc     = .false.
                          ln_wave_test = .false.
                          ln_charn     = .false.
                          ln_taw       = .false.
                          ln_phioc     = .false.
                          ln_bern_srfc = .false.
                          ln_breivikFV_2016 = .false.
                          ln_vortex_force = .false.
                          ln_stshear  = .false.
      ENDIF
      !
   END SUBROUTINE sbc_init


   SUBROUTINE sbc( kt, Kbb, Kmm )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE sbc  ***
      !!
      !! ** Purpose :   provide at each time-step the ocean surface boundary
      !!                condition (momentum, heat and freshwater fluxes)
      !!
      !! ** Method  :   blah blah  to be written ?????????
      !!                CAUTION : never mask the surface stress field (tke sbc)
      !!
      !! ** Action  : - set the ocean surface boundary condition at before and now
      !!                time step, i.e.
      !!                utau_b, vtau_b, qns_b, qsr_b, emp_n, sfx_b, qrp_b, erp_b
      !!                utau  , vtau  , qns  , qsr  , emp  , sfx  , qrp  , erp
      !!              - updte the ice fraction : fr_i
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      INTEGER, INTENT(in) ::   Kbb, Kmm   ! ocean time level indices
      INTEGER  ::   jj, ji          ! dummy loop argument
      !
      LOGICAL ::   ll_sas, ll_opa   ! local logical
      !
      REAL(wp) ::     zthscl        ! wd  tanh scale
      REAL(wp), DIMENSION(jpi,jpj) ::  zwdht, zwght  ! wd dep over wd limit, wgt
      REAL(wp), DIMENSION(jpi,jpj) ::  z2d           ! temporary array used for iom_put

      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('sbc')
      !
      !                                            ! ---------------------------------------- !
      IF( kt /= nit000 ) THEN                      !          Swap of forcing fields          !
         !                                         ! ---------------------------------------- !
         utau_b(:,:) = utau(:,:)                         ! Swap the ocean forcing fields
         vtau_b(:,:) = vtau(:,:)                         ! (except at nit000 where before fields
         qns_b (:,:) = qns (:,:)                         !  are set at the end of the routine)
         emp_b (:,:) = emp (:,:)
         sfx_b (:,:) = sfx (:,:)
         IF( ln_rnf ) THEN
            rnf_b    (:,:  ) = rnf    (:,:  )
            rnf_tsc_b(:,:,:) = rnf_tsc(:,:,:)
         ENDIF
        !
      ENDIF
      !                                            ! ---------------------------------------- !
      !                                            !        forcing field computation         !
      !                                            ! ---------------------------------------- !
      !
      ll_sas = nn_components == jp_iam_sas               ! component flags
      ll_opa = nn_components == jp_iam_oce
      !
      IF( .NOT.ll_sas )   CALL sbc_ssm ( kt, Kbb, Kmm )  ! mean ocean sea surface variables (sst_m, sss_m, ssu_m, ssv_m)
      !
      !                                            !==  sbc formulation  ==!
      !
      !
      SELECT CASE( nsbc )                                ! Compute ocean surface boundary condition
      !                                                  ! (i.e. utau,vtau, qns, qsr, emp, sfx)
      CASE( jp_usr   )     ;   CALL usrdef_sbc_oce( kt, Kbb )                        ! user defined formulation
      CASE( jp_flx     )   ;   CALL sbc_flx       ( kt )                             ! flux formulation
      CASE( jp_blk     )
         IF( ll_sas    )       CALL sbc_cpl_rcv   ( kt, nn_fsbc, nn_ice, Kbb, Kmm )   ! OCE-SAS coupling: SAS receiving fields from OCE
!!!!!!!!!!! ATTENTION:ln_wave is not only used for oasis coupling !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IF( ln_wave )   THEN
             IF ( lk_oasis )  CALL sbc_cpl_rcv ( kt, nn_fsbc, nn_ice, Kbb, Kmm )   ! OCE-wave coupling
             CALL sbc_wave ( kt, Kmm )
         ENDIF
                               CALL sbc_blk       ( kt )                    ! bulk formulation for the ocean
                               !
      CASE( jp_abl     )
         IF( ll_sas    )       CALL sbc_cpl_rcv   ( kt, nn_fsbc, nn_ice, Kbb, Kmm )   ! OCE-SAS coupling: SAS receiving fields from OCE
                               CALL sbc_abl       ( kt )                    ! ABL  formulation for the ocean
                               !
      CASE( jp_purecpl )   ;   CALL sbc_cpl_rcv   ( kt, nn_fsbc, nn_ice, Kbb, Kmm )   ! pure coupled formulation
      CASE( jp_none    )
         IF( ll_opa    )       CALL sbc_cpl_rcv   ( kt, nn_fsbc, nn_ice, Kbb, Kmm )  ! OCE-SAS coupling: OCE receiving fields from SAS
      END SELECT
      !
      IF( ln_mixcpl )          CALL sbc_cpl_rcv   ( kt, nn_fsbc, nn_ice, Kbb, Kmm )  ! forced-coupled mixed formulation after forcing
      !
      IF( ln_wave .AND. ln_tauoc )  THEN            ! Wave stress reduction
         DO_2D( 0, 0, 0, 0)
            utau(ji,jj) = utau(ji,jj) * ( tauoc_wave(ji,jj) + tauoc_wave(ji-1,jj) ) * 0.5_wp
            vtau(ji,jj) = vtau(ji,jj) * ( tauoc_wave(ji,jj) + tauoc_wave(ji,jj-1) ) * 0.5_wp
         END_2D
         !
         CALL lbc_lnk( 'sbcwave', utau, 'U', -1. )
         CALL lbc_lnk( 'sbcwave', vtau, 'V', -1. )
         !
         taum(:,:) = taum(:,:)*tauoc_wave(:,:)
         !
         IF( kt == nit000 )   CALL ctl_warn( 'sbc: You are subtracting the wave stress to the ocean.',   &
            &                                'If not requested select ln_tauoc=.false.' )
         !
      ELSEIF( ln_wave .AND. ln_taw ) THEN                  ! Wave stress reduction
         utau(:,:) = utau(:,:) - tawx(:,:) + twox(:,:)
         vtau(:,:) = vtau(:,:) - tawy(:,:) + twoy(:,:)
         CALL lbc_lnk( 'sbcwave', utau, 'U', -1. )
         CALL lbc_lnk( 'sbcwave', vtau, 'V', -1. )
         !
         DO_2D( 0, 0, 0, 0)
             taum(ji,jj) = sqrt((.5*(utau(ji-1,jj)+utau(ji,jj)))**2 + (.5*(vtau(ji,jj-1)+vtau(ji,jj)))**2)
         END_2D
         !
         IF( kt == nit000 )   CALL ctl_warn( 'sbc: You are subtracting the wave stress to the ocean.',   &
            &                                'If not requested select ln_taw=.false.' )
         !
      ENDIF
      CALL lbc_lnk( 'sbcmod', taum(:,:), 'T', 1. )
      !
      IF( ln_icebergs ) THEN  ! save pure stresses (with no ice-ocean stress) for use by icebergs
         utau_icb(:,:) = utau(:,:) ; vtau_icb(:,:) = vtau(:,:) 
      ENDIF
      !
      !                                            !==  Misc. Options  ==!
      !
      SELECT CASE( nn_ice )                                       ! Update heat and freshwater fluxes over sea-ice areas
      CASE(  1 )   ;         CALL sbc_ice_if   ( kt, Kbb, Kmm )   ! Ice-cover climatology ("Ice-if" model)
#if defined key_si3
      CASE(  2 )   ;         CALL ice_stp  ( kt, Kbb, Kmm, nsbc ) ! SI3 ice model
#endif
      CASE(  3 )   ;         CALL sbc_ice_cice ( kt, nsbc )       ! CICE ice model
      END SELECT

      IF( ln_icebergs    )   CALL icb_stp( kt, Kmm )              ! compute icebergs

      ! Icebergs do not melt over the haloes.
      ! So emp values over the haloes are no more consistent with the inner domain values.
      ! A lbc_lnk is therefore needed to ensure reproducibility and restartability.
      ! see ticket #2113 for discussion about this lbc_lnk.
      ! The lbc_lnk is also needed for SI3 with nn_hls > 1 as emp is not yet defined for these points in iceupdate.F90
      IF( (ln_icebergs .AND. .NOT. ln_passive_mode) .OR. (nn_ice == 2 .AND. nn_hls == 2) ) THEN
         CALL lbc_lnk( 'sbcmod', emp, 'T', 1.0_wp )
      ENDIF

      IF( ln_rnf         )   CALL sbc_rnf( kt )                   ! add runoffs to fresh water fluxes

      IF( ln_ssr         )   CALL sbc_ssr( kt )                        ! add SST/SSS damping term

      IF( nn_fwb    /= 0 )   CALL sbc_fwb( kt, nn_fwb, nn_fsbc, Kmm )  ! control the freshwater budget

      ! Special treatment of freshwater fluxes over closed seas in the model domain
      ! Should not be run if ln_diurnal_only
      IF( l_sbc_clo      )   CALL sbc_clo( kt )

!!$!RBbug do not understand why see ticket 667
!!$!clem: it looks like it is necessary for the north fold (in certain circumstances). Don't know why.
!!$      CALL lbc_lnk( 'sbcmod', emp, 'T', 1.0_wp )
      IF( ll_wd ) THEN     ! If near WAD point limit the flux for now
         zthscl = atanh(rn_wd_sbcfra)                     ! taper frac default is .999
         zwdht(:,:) = ssh(:,:,Kmm) + ht_0(:,:) - rn_wdmin1   ! do this calc of water
                                                     ! depth above wd limit once
         WHERE( zwdht(:,:) <= 0.0 )
            taum(:,:) = 0.0
            utau(:,:) = 0.0
            vtau(:,:) = 0.0
            qns (:,:) = 0.0
            qsr (:,:) = 0.0
            emp (:,:) = min(emp(:,:),0.0) !can allow puddles to grow but not shrink
            sfx (:,:) = 0.0
         END WHERE
         zwght(:,:) = tanh(zthscl*zwdht(:,:))
         WHERE( zwdht(:,:) > 0.0  .and. zwdht(:,:) < rn_wd_sbcdep ) !  5 m hard limit here is arbitrary
            qsr  (:,:) =  qsr(:,:)  * zwght(:,:)
            qns  (:,:) =  qns(:,:)  * zwght(:,:)
            taum (:,:) =  taum(:,:) * zwght(:,:)
            utau (:,:) =  utau(:,:) * zwght(:,:)
            vtau (:,:) =  vtau(:,:) * zwght(:,:)
            sfx  (:,:) =  sfx(:,:)  * zwght(:,:)
            emp  (:,:) =  emp(:,:)  * zwght(:,:)
         END WHERE
      ENDIF
      !
      IF( kt == nit000 ) THEN                          !   set the forcing field at nit000 - 1    !
         !                                             ! ---------------------------------------- !
         IF( ln_rstart .AND. .NOT.l_1st_euler ) THEN            !* Restart: read in restart file
            IF(lwp) WRITE(numout,*) '          nit000-1 surface forcing fields read in the restart file'
            CALL iom_get( numror, jpdom_auto, 'utau_b', utau_b )   ! i-stress
            CALL iom_get( numror, jpdom_auto, 'vtau_b', vtau_b )   ! j-stress
            CALL iom_get( numror, jpdom_auto,  'qns_b',  qns_b )   ! non solar heat flux
            CALL iom_get( numror, jpdom_auto,  'emp_b',  emp_b )   ! freshwater flux
            ! NB: The 3D heat content due to qsr forcing (qsr_hc_b) is treated in traqsr
            ! To ensure restart capability with 3.3x/3.4 restart files    !! to be removed in v3.6
            IF( iom_varid( numror, 'sfx_b', ldstop = .FALSE. ) > 0 ) THEN
               CALL iom_get( numror, jpdom_auto, 'sfx_b', sfx_b )  ! before salt flux (T-point)
            ELSE
               sfx_b (:,:) = sfx(:,:)
            ENDIF
         ELSE                                                   !* no restart: set from nit000 values
            IF(lwp) WRITE(numout,*) '          nit000-1 surface forcing fields set to nit000'
            utau_b(:,:) = utau(:,:)
            vtau_b(:,:) = vtau(:,:)
            qns_b (:,:) = qns (:,:)
            emp_b (:,:) = emp (:,:)
            sfx_b (:,:) = sfx (:,:)
         ENDIF
      ENDIF
      !                                                ! ---------------------------------------- !
      IF( lrst_oce ) THEN                              !      Write in the ocean restart file     !
         !                                             ! ---------------------------------------- !
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'sbc : ocean surface forcing fields written in ocean restart file ',   &
            &                    'at it= ', kt,' date= ', ndastp
         IF(lwp) WRITE(numout,*) '~~~~'
         CALL iom_rstput( kt, nitrst, numrow, 'utau_b' , utau )
         CALL iom_rstput( kt, nitrst, numrow, 'vtau_b' , vtau )
         CALL iom_rstput( kt, nitrst, numrow, 'qns_b'  , qns  )
         ! The 3D heat content due to qsr forcing is treated in traqsr
         ! CALL iom_rstput( kt, nitrst, numrow, 'qsr_b'  , qsr  )
         CALL iom_rstput( kt, nitrst, numrow, 'emp_b'  , emp  )
         CALL iom_rstput( kt, nitrst, numrow, 'sfx_b'  , sfx  )
      ENDIF
      !                                                ! ---------------------------------------- !
      !                                                !        Outputs and control print         !
      !                                                ! ---------------------------------------- !
      IF( MOD( kt-1, nn_fsbc ) == 0 ) THEN
         IF( iom_use("empmr") ) THEN
            DO_2D( 0, 0, 0, 0 )
               z2d(ji,jj) =  emp(ji,jj) - rnf(ji,jj)
            END_2D
            CALL iom_put( "empmr"  , z2d      )                ! upward water flux
         ENDIF
         IF( iom_use("empbmr") ) THEN
            DO_2D( 0, 0, 0, 0 )
               z2d(ji,jj) =  emp_b(ji,jj) - rnf(ji,jj)
            END_2D
            CALL iom_put( "empbmr" , z2d      )                ! before upward water flux ( needed to recalculate the time evolution of ssh in offline )
         ENDIF
         CALL iom_put( "saltflx", sfx         )                ! downward salt flux (includes virtual salt flux beneath ice in linear free surface case)
         CALL iom_put( "fmmflx" , fmmflx      )                ! Freezing-melting water flux
         IF( iom_use("qt") ) THEN
            DO_2D( 0, 0, 0, 0 )
               z2d(ji,jj) =  qns(ji,jj) + qsr(ji,jj)
            END_2D
            CALL iom_put( "qt"  , z2d         )                ! total heat flux
         ENDIF
         CALL iom_put( "qns"    , qns         )                ! solar heat flux
         CALL iom_put( "qsr"    , qsr         )                ! solar heat flux
         IF( nn_ice > 0 .OR. ll_opa )   CALL iom_put( "ice_cover", fr_i )   ! ice fraction
         CALL iom_put( "taum"   , taum        )                ! wind stress module
         CALL iom_put( "wspd"   , wndm        )                ! wind speed  module over free ocean or leads in presence of sea-ice
         CALL iom_put( "qrp"    , qrp         )                ! heat flux damping
         CALL iom_put( "erp"    , erp         )                ! freshwater flux damping
      ENDIF
      !
      IF(sn_cfctl%l_prtctl) THEN     ! print mean trends (used for debugging)
         CALL prt_ctl(tab2d_1=fr_i                , clinfo1=' fr_i     - : ', mask1=tmask )
         CALL prt_ctl(tab2d_1=(emp-rnf)           , clinfo1=' emp-rnf  - : ', mask1=tmask )
         CALL prt_ctl(tab2d_1=(sfx-rnf)           , clinfo1=' sfx-rnf  - : ', mask1=tmask )
         CALL prt_ctl(tab2d_1=qns                 , clinfo1=' qns      - : ', mask1=tmask )
         CALL prt_ctl(tab2d_1=qsr                 , clinfo1=' qsr      - : ', mask1=tmask )
         CALL prt_ctl(tab3d_1=tmask               , clinfo1=' tmask    - : ', mask1=tmask, kdim=jpk )
         CALL prt_ctl(tab3d_1=ts(:,:,:,jp_tem,Kmm), clinfo1=' sst      - : ', mask1=tmask, kdim=1   )
         CALL prt_ctl(tab3d_1=ts(:,:,:,jp_sal,Kmm), clinfo1=' sss      - : ', mask1=tmask, kdim=1   )
         CALL prt_ctl(tab2d_1=utau                , clinfo1=' utau     - : ', mask1=umask,                      &
            &         tab2d_2=vtau                , clinfo2=' vtau     - : ', mask2=vmask )
      ENDIF

      IF( kt == nitend )   CALL sbc_final         ! Close down surface module if necessary
      !
      IF( ln_timing )   CALL timing_stop('sbc')
      !
   END SUBROUTINE sbc


   SUBROUTINE sbc_final
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE sbc_final  ***
      !!
      !! ** Purpose :   Finalize CICE (if used)
      !!---------------------------------------------------------------------
      !
      IF( nn_ice == 3 )   CALL cice_sbc_final
      !
   END SUBROUTINE sbc_final

   !!======================================================================
END MODULE sbcmod
