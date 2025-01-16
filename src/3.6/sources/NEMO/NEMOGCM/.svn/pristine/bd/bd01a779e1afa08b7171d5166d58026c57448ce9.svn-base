MODULE limthd_dh
   !!======================================================================
   !!                       ***  MODULE limthd_dh ***
   !!  LIM-3 :   thermodynamic growth and decay of the ice 
   !!======================================================================
   !! History :  LIM  ! 2003-05 (M. Vancoppenolle) Original code in 1D
   !!                 ! 2005-06 (M. Vancoppenolle) 3D version 
   !!            3.2  ! 2009-07 (M. Vancoppenolle, Y. Aksenov, G. Madec) bug correction in wfx_snw & wfx_ice
   !!            3.4  ! 2011-02 (G. Madec) dynamical allocation
   !!            3.5  ! 2012-10 (G. Madec & co) salt flux + bug fixes 
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3'                                      LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_thd_dh    : vertical accr./abl. and lateral ablation of sea ice
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean parameters
   USE phycst         ! physical constants (OCE directory) 
   USE sbc_oce        ! Surface boundary condition: ocean fields
   USE ice            ! LIM variables
   USE thd_ice        ! LIM thermodynamics
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE wrk_nemo       ! work arrays
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)  
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_thd_dh      ! called by lim_thd
   PUBLIC   lim_thd_snwblow ! called in sbcblk/sbcclio/sbccpl and here

   INTERFACE lim_thd_snwblow
      MODULE PROCEDURE lim_thd_snwblow_1d, lim_thd_snwblow_2d
   END INTERFACE

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2010)
   !! $Id$
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_thd_dh( kideb, kiut )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_thd_dh  ***
      !!
      !! ** Purpose :   determines variations of ice and snow thicknesses.
      !!
      !! ** Method  :   Ice/Snow surface melting arises from imbalance in surface fluxes
      !!              Bottom accretion/ablation arises from flux budget
      !!              Snow thickness can increase by precipitation and decrease by sublimation
      !!              If snow load excesses Archmiede limit, snow-ice is formed by
      !!              the flooding of sea-water in the snow
      !!
      !!                 1) Compute available flux of heat for surface ablation
      !!                 2) Compute snow and sea ice enthalpies
      !!                 3) Surface ablation and sublimation
      !!                 4) Bottom accretion/ablation
      !!                 5) Case of Total ablation
      !!                 6) Snow ice formation
      !!
      !! References : Bitz and Lipscomb, 1999, J. Geophys. Res.
      !!              Fichefet T. and M. Maqueda 1997, J. Geophys. Res., 102(C6), 12609-12646   
      !!              Vancoppenolle, Fichefet and Bitz, 2005, Geophys. Res. Let. 
      !!              Vancoppenolle et al.,2009, Ocean Modelling
      !!------------------------------------------------------------------
      INTEGER , INTENT(in) ::   kideb, kiut   ! Start/End point on which the  the computation is applied
      !! 
      INTEGER  ::   ji , jk        ! dummy loop indices
      INTEGER  ::   ii, ij         ! 2D corresponding indices to ji
      INTEGER  ::   iter

      REAL(wp) ::   ztmelts             ! local scalar
      REAL(wp) ::   zdum       
      REAL(wp) ::   zfracs       ! fractionation coefficient for bottom salt entrapment
      REAL(wp) ::   zs_snic      ! snow-ice salinity
      REAL(wp) ::   zswi1        ! switch for computation of bottom salinity
      REAL(wp) ::   zswi12       ! switch for computation of bottom salinity
      REAL(wp) ::   zswi2        ! switch for computation of bottom salinity
      REAL(wp) ::   zgrr         ! bottom growth rate
      REAL(wp) ::   zt_i_new     ! bottom formation temperature

      REAL(wp) ::   zQm          ! enthalpy exchanged with the ocean (J/m2), >0 towards the ocean
      REAL(wp) ::   zEi          ! specific enthalpy of sea ice (J/kg)
      REAL(wp) ::   zEw          ! specific enthalpy of exchanged water (J/kg)
      REAL(wp) ::   zdE          ! specific enthalpy difference (J/kg)
      REAL(wp) ::   zfmdt        ! exchange mass flux x time step (J/m2), >0 towards the ocean
      REAL(wp) ::   zsstK        ! SST in Kelvin

      REAL(wp), POINTER, DIMENSION(:) ::   zqprec      ! energy of fallen snow                       (J.m-3)
      REAL(wp), POINTER, DIMENSION(:) ::   zq_su       ! heat for surface ablation                   (J.m-2)
      REAL(wp), POINTER, DIMENSION(:) ::   zq_bo       ! heat for bottom ablation                    (J.m-2)
      REAL(wp), POINTER, DIMENSION(:) ::   zq_rema     ! remaining heat at the end of the routine    (J.m-2)
      REAL(wp), POINTER, DIMENSION(:) ::   zf_tt       ! Heat budget to determine melting or freezing(W.m-2)
      REAL(wp), POINTER, DIMENSION(:) ::   zevap_rema  ! remaining mass flux from sublimation        (kg.m-2)

      REAL(wp), POINTER, DIMENSION(:) ::   zdh_s_mel   ! snow melt 
      REAL(wp), POINTER, DIMENSION(:) ::   zdh_s_pre   ! snow precipitation 
      REAL(wp), POINTER, DIMENSION(:) ::   zdh_s_sub   ! snow sublimation

      REAL(wp), POINTER, DIMENSION(:,:) ::   zdeltah
      REAL(wp), POINTER, DIMENSION(:,:) ::   zh_i      ! ice layer thickness
      INTEGER , POINTER, DIMENSION(:,:) ::   icount    ! number of layers vanished by melting 

      REAL(wp), POINTER, DIMENSION(:) ::   zqh_i       ! total ice heat content  (J.m-2)
      REAL(wp), POINTER, DIMENSION(:) ::   zsnw        ! distribution of snow after wind blowing

      REAL(wp) :: zswitch_sal

      ! Heat conservation 
      INTEGER  ::   num_iter_max

      !!------------------------------------------------------------------

      ! Discriminate between varying salinity (nn_icesal=2) and prescribed cases (other values)
      SELECT CASE( nn_icesal )                  ! varying salinity or not
         CASE( 1, 3 ) ;   zswitch_sal = 0       ! prescribed salinity profile
         CASE( 2 )    ;   zswitch_sal = 1       ! varying salinity profile
      END SELECT

      CALL wrk_alloc( jpij, zqprec, zq_su, zq_bo, zf_tt, zq_rema, zsnw, zevap_rema )
      CALL wrk_alloc( jpij, zdh_s_mel, zdh_s_pre, zdh_s_sub, zqh_i )
      CALL wrk_alloc( jpij, nlay_i, zdeltah, zh_i )
      CALL wrk_alloc( jpij, nlay_i, icount )
       
      dh_i_surf  (:) = 0._wp ; dh_i_bott  (:) = 0._wp ; dh_snowice(:) = 0._wp ; dh_i_sub(:) = 0._wp
      dsm_i_se_1d(:) = 0._wp ; dsm_i_si_1d(:) = 0._wp   

      zqprec   (:) = 0._wp ; zq_su    (:) = 0._wp ; zq_bo    (:) = 0._wp ; zf_tt(:) = 0._wp
      zq_rema  (:) = 0._wp ; zsnw     (:) = 0._wp ; zevap_rema(:) = 0._wp ;
      zdh_s_mel(:) = 0._wp ; zdh_s_pre(:) = 0._wp ; zdh_s_sub(:) = 0._wp ; zqh_i(:) = 0._wp

      zdeltah(:,:) = 0._wp ; zh_i(:,:) = 0._wp       
      icount (:,:) = 0


      ! Initialize enthalpy at nlay_i+1
      DO ji = kideb, kiut
         q_i_1d(ji,nlay_i+1) = 0._wp
      END DO

      ! initialize layer thicknesses and enthalpies
      h_i_old (:,0:nlay_i+1) = 0._wp
      qh_i_old(:,0:nlay_i+1) = 0._wp
      DO jk = 1, nlay_i
         DO ji = kideb, kiut
            h_i_old (ji,jk) = ht_i_1d(ji) * r1_nlay_i
            qh_i_old(ji,jk) = q_i_1d(ji,jk) * h_i_old(ji,jk)
         ENDDO
      ENDDO
      !
      !------------------------------------------------------------------------------!
      !  1) Calculate available heat for surface and bottom ablation                 !
      !------------------------------------------------------------------------------!
      !
      DO ji = kideb, kiut
         zdum       = qns_ice_1d(ji) + ( 1._wp - i0(ji) ) * qsr_ice_1d(ji) - fc_su(ji) 
         zf_tt(ji)  = fc_bo_i(ji) + fhtur_1d(ji) + fhld_1d(ji) 

         zq_su (ji) = MAX( 0._wp, zdum      * rdt_ice ) * MAX( 0._wp , SIGN( 1._wp, t_su_1d(ji) - rt0 ) )
         zq_bo (ji) = MAX( 0._wp, zf_tt(ji) * rdt_ice )
      END DO

      !
      !------------------------------------------------------------------------------!
      ! If snow temperature is above freezing point, then snow melts 
      ! (should not happen but sometimes it does)
      !------------------------------------------------------------------------------!
      DO ji = kideb, kiut
         IF( t_s_1d(ji,1) > rt0 ) THEN !!! Internal melting
            ! Contribution to heat flux to the ocean [W.m-2], < 0  
            hfx_res_1d(ji) = hfx_res_1d(ji) + q_s_1d(ji,1) * ht_s_1d(ji) * a_i_1d(ji) * r1_rdtice
            ! Contribution to mass flux
            wfx_snw_1d(ji) = wfx_snw_1d(ji) + rhosn * ht_s_1d(ji) * a_i_1d(ji) * r1_rdtice
            ! updates
            ht_s_1d(ji)   = 0._wp
            q_s_1d (ji,1) = 0._wp
            t_s_1d (ji,1) = rt0
         END IF
      END DO

      !------------------------------------------------------------!
      !  2) Computing layer thicknesses and enthalpies.            !
      !------------------------------------------------------------!
      !
      DO jk = 1, nlay_i
         DO ji = kideb, kiut
            zh_i(ji,jk) = ht_i_1d(ji) * r1_nlay_i
            zqh_i(ji)   = zqh_i(ji) + q_i_1d(ji,jk) * zh_i(ji,jk)
         END DO
      END DO
      !
      !------------------------------------------------------------------------------|
      !  3) Surface ablation and sublimation                                         |
      !------------------------------------------------------------------------------|
      !
      !-------------------------
      ! 3.1 Snow precips / melt
      !-------------------------
      ! Snow accumulation in one thermodynamic time step
      ! snowfall is partitionned between leads and ice
      ! if snow fall was uniform, a fraction (1-at_i) would fall into leads
      ! but because of the winds, more snow falls on leads than on sea ice
      ! and a greater fraction (1-at_i)^beta of the total mass of snow 
      ! (beta < 1) falls in leads.
      ! In reality, beta depends on wind speed, 
      ! and should decrease with increasing wind speed but here, it is 
      ! considered as a constant. an average value is 0.66
      ! Martin Vancoppenolle, December 2006

      CALL lim_thd_snwblow( 1. - at_i_1d(kideb:kiut), zsnw(kideb:kiut) ) ! snow distribution over ice after wind blowing

      zdeltah(:,:) = 0._wp
      DO ji = kideb, kiut
         !-----------
         ! Snow fall
         !-----------
         ! thickness change
         zdh_s_pre(ji) = zsnw(ji) * sprecip_1d(ji) * rdt_ice * r1_rhosn / at_i_1d(ji)
         ! enthalpy of the precip (>0, J.m-3)
         zqprec   (ji) = - qprec_ice_1d(ji)   
         IF( sprecip_1d(ji) == 0._wp ) zqprec(ji) = 0._wp
         ! heat flux from snow precip (>0, W.m-2)
         hfx_spr_1d(ji) = hfx_spr_1d(ji) + zdh_s_pre(ji) * a_i_1d(ji) * zqprec(ji) * r1_rdtice
         ! mass flux, <0
         wfx_spr_1d(ji) = wfx_spr_1d(ji) - rhosn * a_i_1d(ji) * zdh_s_pre(ji) * r1_rdtice

         !---------------------
         ! Melt of falling snow
         !---------------------
         ! thickness change
         rswitch        = MAX( 0._wp , SIGN( 1._wp , zqprec(ji) - epsi20 ) )
         zdeltah (ji,1) = - rswitch * zq_su(ji) / MAX( zqprec(ji) , epsi20 )
         zdeltah (ji,1) = MAX( - zdh_s_pre(ji), zdeltah(ji,1) ) ! bound melting 
         ! heat used to melt snow (W.m-2, >0)
         hfx_snw_1d(ji) = hfx_snw_1d(ji) - zdeltah(ji,1) * a_i_1d(ji) * zqprec(ji) * r1_rdtice
         ! snow melting only = water into the ocean (then without snow precip), >0
         wfx_snw_1d(ji) = wfx_snw_1d(ji) - rhosn * a_i_1d(ji) * zdeltah(ji,1) * r1_rdtice    
         ! updates available heat + precipitations after melting
         zq_su     (ji) = MAX( 0._wp , zq_su (ji) + zdeltah(ji,1) * zqprec(ji) )      
         zdh_s_pre (ji) = zdh_s_pre(ji) + zdeltah(ji,1)

         ! update thickness
         ht_s_1d(ji) = MAX( 0._wp , ht_s_1d(ji) + zdh_s_pre(ji) )
      END DO

      ! If heat still available (zq_su > 0), then melt more snow
      zdeltah(:,:) = 0._wp
      DO jk = 1, nlay_s
         DO ji = kideb, kiut
            ! thickness change
            rswitch          = 1._wp - MAX( 0._wp, SIGN( 1._wp, - ht_s_1d(ji) ) ) 
            rswitch          = rswitch * ( MAX( 0._wp, SIGN( 1._wp, q_s_1d(ji,jk) - epsi20 ) ) ) 
            zdeltah  (ji,jk) = - rswitch * zq_su(ji) / MAX( q_s_1d(ji,jk), epsi20 )
            zdeltah  (ji,jk) = MAX( zdeltah(ji,jk) , - ht_s_1d(ji) ) ! bound melting
            zdh_s_mel(ji)    = zdh_s_mel(ji) + zdeltah(ji,jk)    
            ! heat used to melt snow(W.m-2, >0)
            hfx_snw_1d(ji)   = hfx_snw_1d(ji) - zdeltah(ji,jk) * a_i_1d(ji) * q_s_1d(ji,jk) * r1_rdtice 
            ! snow melting only = water into the ocean (then without snow precip)
            wfx_snw_1d(ji)   = wfx_snw_1d(ji) - rhosn * a_i_1d(ji) * zdeltah(ji,jk) * r1_rdtice
            ! updates available heat + thickness
            zq_su (ji)  = MAX( 0._wp , zq_su (ji) + zdeltah(ji,jk) * q_s_1d(ji,jk) )
            ht_s_1d(ji) = MAX( 0._wp , ht_s_1d(ji) + zdeltah(ji,jk) )
         END DO
      END DO

      !------------------------------
      ! 3.2 Sublimation (part1: snow) 
      !------------------------------
      ! qla_ice is always >=0 (upwards), heat goes to the atmosphere, therefore snow sublimates
      ! clem comment: not counted in mass/heat exchange in limsbc since this is an exchange with atm. (not ocean)
      zdeltah(:,:) = 0._wp
      DO ji = kideb, kiut
         zdh_s_sub(ji)  = MAX( - ht_s_1d(ji) , - evap_ice_1d(ji) * r1_rhosn * rdt_ice )
         ! remaining evap in kg.m-2 (used for ice melting later on)
         zevap_rema(ji)  = evap_ice_1d(ji) * rdt_ice + zdh_s_sub(ji) * rhosn
         ! Heat flux by sublimation [W.m-2], < 0 (sublimate first snow that had fallen, then pre-existing snow)
         zdeltah(ji,1)  = MAX( zdh_s_sub(ji), - zdh_s_pre(ji) )
         hfx_sub_1d(ji) = hfx_sub_1d(ji) + ( zdeltah(ji,1) * zqprec(ji) + ( zdh_s_sub(ji) - zdeltah(ji,1) ) * q_s_1d(ji,1)  &
            &                              ) * a_i_1d(ji) * r1_rdtice
         ! Mass flux by sublimation
         wfx_sub_1d(ji) =  wfx_sub_1d(ji) - rhosn * a_i_1d(ji) * zdh_s_sub(ji) * r1_rdtice
         ! new snow thickness
         ht_s_1d(ji)    =  MAX( 0._wp , ht_s_1d(ji) + zdh_s_sub(ji) )
         ! update precipitations after sublimation and correct sublimation
         zdh_s_pre(ji) = zdh_s_pre(ji) + zdeltah(ji,1)
         zdh_s_sub(ji) = zdh_s_sub(ji) - zdeltah(ji,1)
      END DO
      
      ! --- Update snow diags --- !
      DO ji = kideb, kiut
         dh_s_tot(ji) = zdh_s_mel(ji) + zdh_s_pre(ji) + zdh_s_sub(ji)
      END DO

      !-------------------------------------------
      ! 3.3 Update temperature, energy
      !-------------------------------------------
      ! new temp and enthalpy of the snow (remaining snow precip + remaining pre-existing snow)
      DO jk = 1, nlay_s
         DO ji = kideb,kiut
            rswitch       = MAX( 0._wp , SIGN( 1._wp, ht_s_1d(ji) - epsi20 ) )
            q_s_1d(ji,jk) = rswitch / MAX( ht_s_1d(ji), epsi20 ) *           &
              &            ( ( zdh_s_pre(ji)               ) * zqprec(ji) +  &
              &              ( ht_s_1d(ji) - zdh_s_pre(ji) ) * rhosn * ( cpic * ( rt0 - t_s_1d(ji,jk) ) + lfus ) )
         END DO
      END DO

      !--------------------------
      ! 3.4 Surface ice ablation 
      !--------------------------
      zdeltah(:,:) = 0._wp ! important
      DO jk = 1, nlay_i
         DO ji = kideb, kiut
            ztmelts           = - tmut * s_i_1d(ji,jk) + rt0          ! Melting point of layer k [K]
            
            IF( t_i_1d(ji,jk) >= ztmelts ) THEN !!! Internal melting

               zEi            = - q_i_1d(ji,jk) * r1_rhoic            ! Specific enthalpy of layer k [J/kg, <0]       
               zdE            = 0._wp                                 ! Specific enthalpy difference   (J/kg, <0)
                                                                      ! set up at 0 since no energy is needed to melt water...(it is already melted)
               zdeltah(ji,jk) = MIN( 0._wp , - zh_i(ji,jk) )          ! internal melting occurs when the internal temperature is above freezing     
                                                                      ! this should normally not happen, but sometimes, heat diffusion leads to this
               zfmdt          = - zdeltah(ji,jk) * rhoic              ! Mass flux x time step > 0
                         
               dh_i_surf(ji)  = dh_i_surf(ji) + zdeltah(ji,jk)        ! Cumulate surface melt
               
               zfmdt          = - rhoic * zdeltah(ji,jk)              ! Recompute mass flux [kg/m2, >0]

               ! Contribution to heat flux to the ocean [W.m-2], <0 (ice enthalpy zEi is "sent" to the ocean) 
               hfx_res_1d(ji) = hfx_res_1d(ji) + zfmdt * a_i_1d(ji) * zEi * r1_rdtice
               
               ! Contribution to salt flux (clem: using sm_i_1d and not s_i_1d(jk) is ok)
               sfx_res_1d(ji) = sfx_res_1d(ji) - rhoic * a_i_1d(ji) * zdeltah(ji,jk) * sm_i_1d(ji) * r1_rdtice
               
               ! Contribution to mass flux
               wfx_res_1d(ji) =  wfx_res_1d(ji) - rhoic * a_i_1d(ji) * zdeltah(ji,jk) * r1_rdtice

            ELSE                                !!! Surface melting
               
               zEi            = - q_i_1d(ji,jk) * r1_rhoic            ! Specific enthalpy of layer k [J/kg, <0]
               zEw            =    rcp * ( ztmelts - rt0 )            ! Specific enthalpy of resulting meltwater [J/kg, <0]
               zdE            =    zEi - zEw                          ! Specific enthalpy difference < 0
               
               zfmdt          = - zq_su(ji) / zdE                     ! Mass flux to the ocean [kg/m2, >0]
               
               zdeltah(ji,jk) = - zfmdt * r1_rhoic                    ! Melt of layer jk [m, <0]
               
               zdeltah(ji,jk) = MIN( 0._wp , MAX( zdeltah(ji,jk) , - zh_i(ji,jk) ) )    ! Melt of layer jk cannot exceed the layer thickness [m, <0]
               
               zq_su(ji)      = MAX( 0._wp , zq_su(ji) - zdeltah(ji,jk) * rhoic * zdE ) ! update available heat
               
               dh_i_surf(ji)  = dh_i_surf(ji) + zdeltah(ji,jk)        ! Cumulate surface melt
               
               zfmdt          = - rhoic * zdeltah(ji,jk)              ! Recompute mass flux [kg/m2, >0]
               
               zQm            = zfmdt * zEw                           ! Energy of the melt water sent to the ocean [J/m2, <0]
               
               ! Contribution to salt flux >0 (clem: using sm_i_1d and not s_i_1d(jk) is ok)
               sfx_sum_1d(ji) = sfx_sum_1d(ji) - rhoic * a_i_1d(ji) * zdeltah(ji,jk) * sm_i_1d(ji) * r1_rdtice
               
               ! Contribution to heat flux [W.m-2], < 0
               hfx_thd_1d(ji) = hfx_thd_1d(ji) + zfmdt * a_i_1d(ji) * zEw * r1_rdtice
               
               ! Total heat flux used in this process [W.m-2], > 0  
               hfx_sum_1d(ji) = hfx_sum_1d(ji) - zfmdt * a_i_1d(ji) * zdE * r1_rdtice
               
               ! Contribution to mass flux
               wfx_sum_1d(ji) =  wfx_sum_1d(ji) - rhoic * a_i_1d(ji) * zdeltah(ji,jk) * r1_rdtice
               
            END IF
            ! ----------------------
            ! Sublimation part2: ice
            ! ----------------------
            zdum      = MAX( - ( zh_i(ji,jk) + zdeltah(ji,jk) ) , - zevap_rema(ji) * r1_rhoic )
            zdeltah(ji,jk) = zdeltah(ji,jk) + zdum
            dh_i_sub(ji)  = dh_i_sub(ji) + zdum
            ! Salt flux > 0 (clem2016: flux is sent to the ocean for simplicity but salt should remain in the ice except if all ice is melted.
            !                          It must be corrected at some point)
            sfx_sub_1d(ji) = sfx_sub_1d(ji) - rhoic * a_i_1d(ji) * zdum * sm_i_1d(ji) * r1_rdtice
            ! Heat flux [W.m-2], < 0
            hfx_sub_1d(ji) = hfx_sub_1d(ji) + zdum * q_i_1d(ji,jk) * a_i_1d(ji) * r1_rdtice
            ! Mass flux > 0
            wfx_sub_1d(ji) =  wfx_sub_1d(ji) - rhoic * a_i_1d(ji) * zdum * r1_rdtice
            ! update remaining mass flux
            zevap_rema(ji)  = zevap_rema(ji) + zdum * rhoic
            
            ! record which layers have disappeared (for bottom melting) 
            !    => icount=0 : no layer has vanished
            !    => icount=5 : 5 layers have vanished
            rswitch       = MAX( 0._wp , SIGN( 1._wp , - ( zh_i(ji,jk) + zdeltah(ji,jk) ) ) ) 
            icount(ji,jk) = NINT( rswitch )
            zh_i(ji,jk)   = MAX( 0._wp , zh_i(ji,jk) + zdeltah(ji,jk) )
                        
            ! update heat content (J.m-2) and layer thickness
            qh_i_old(ji,jk) = qh_i_old(ji,jk) + zdeltah(ji,jk) * q_i_1d(ji,jk)
            h_i_old (ji,jk) = h_i_old (ji,jk) + zdeltah(ji,jk)
         END DO
      END DO
      ! update ice thickness
      DO ji = kideb, kiut
         ht_i_1d(ji) =  MAX( 0._wp , ht_i_1d(ji) + dh_i_surf(ji) + dh_i_sub(ji) )
      END DO

      ! remaining "potential" evap is sent to ocean
      DO ji = kideb, kiut
         ii = MOD( npb(ji) - 1, jpi ) + 1 ; ij = ( npb(ji) - 1 ) / jpi + 1
         wfx_err_sub(ii,ij) = wfx_err_sub(ii,ij) - zevap_rema(ji) * a_i_1d(ji) * r1_rdtice  ! <=0 (net evap for the ocean in kg.m-2.s-1)
      END DO

      !
      !------------------------------------------------------------------------------!
      ! 4) Basal growth / melt                                                       !
      !------------------------------------------------------------------------------!
      !
      !------------------
      ! 4.1 Basal growth 
      !------------------
      ! Basal growth is driven by heat imbalance at the ice-ocean interface,
      ! between the inner conductive flux  (fc_bo_i), from the open water heat flux 
      ! (fhld) and the turbulent ocean flux (fhtur). 
      ! fc_bo_i is positive downwards. fhtur and fhld are positive to the ice 

      ! If salinity varies in time, an iterative procedure is required, because
      ! the involved quantities are inter-dependent.
      ! Basal growth (dh_i_bott) depends upon new ice specific enthalpy (zEi),
      ! which depends on forming ice salinity (s_i_new), which depends on dh/dt (dh_i_bott)
      ! -> need for an iterative procedure, which converges quickly

      num_iter_max = 1
      IF( nn_icesal == 2 ) num_iter_max = 5

      ! Iterative procedure
      DO iter = 1, num_iter_max
         DO ji = kideb, kiut
            IF(  zf_tt(ji) < 0._wp  ) THEN

               ! New bottom ice salinity (Cox & Weeks, JGR88 )
               !--- zswi1  if dh/dt < 2.0e-8
               !--- zswi12 if 2.0e-8 < dh/dt < 3.6e-7 
               !--- zswi2  if dh/dt > 3.6e-7
               zgrr               = MIN( 1.0e-3, MAX ( dh_i_bott(ji) * r1_rdtice , epsi10 ) )
               zswi2              = MAX( 0._wp , SIGN( 1._wp , zgrr - 3.6e-7 ) )
               zswi12             = MAX( 0._wp , SIGN( 1._wp , zgrr - 2.0e-8 ) ) * ( 1.0 - zswi2 )
               zswi1              = 1. - zswi2 * zswi12
               zfracs             = MIN ( zswi1  * 0.12 + zswi12 * ( 0.8925 + 0.0568 * LOG( 100.0 * zgrr ) )   &
                  &               + zswi2  * 0.26 / ( 0.26 + 0.74 * EXP ( - 724300.0 * zgrr ) )  , 0.5 )

               ii = MOD( npb(ji) - 1, jpi ) + 1 ; ij = ( npb(ji) - 1 ) / jpi + 1

               s_i_new(ji)        = zswitch_sal * zfracs * sss_m(ii,ij)  &  ! New ice salinity
                                  + ( 1. - zswitch_sal ) * sm_i_1d(ji) 
               ! New ice growth
               ztmelts            = - tmut * s_i_new(ji) + rt0          ! New ice melting point (K)

               zt_i_new           = zswitch_sal * t_bo_1d(ji) + ( 1. - zswitch_sal) * t_i_1d(ji, nlay_i)
               
               zEi                = cpic * ( zt_i_new - ztmelts ) &     ! Specific enthalpy of forming ice (J/kg, <0)      
                  &               - lfus * ( 1.0 - ( ztmelts - rt0 ) / ( zt_i_new - rt0 ) )   &
                  &               + rcp  * ( ztmelts-rt0 )          

               zEw                = rcp  * ( t_bo_1d(ji) - rt0 )         ! Specific enthalpy of seawater (J/kg, < 0)

               zdE                = zEi - zEw                           ! Specific enthalpy difference (J/kg, <0)

               dh_i_bott(ji)      = rdt_ice * MAX( 0._wp , zf_tt(ji) / ( zdE * rhoic ) )

               q_i_1d(ji,nlay_i+1) = -zEi * rhoic                        ! New ice energy of melting (J/m3, >0)
               
            ENDIF
         END DO
      END DO

      ! Contribution to Energy and Salt Fluxes
      DO ji = kideb, kiut
         IF(  zf_tt(ji) < 0._wp  ) THEN
            ! New ice growth
                                    
            zfmdt          = - rhoic * dh_i_bott(ji)             ! Mass flux x time step (kg/m2, < 0)

            ztmelts        = - tmut * s_i_new(ji) + rt0          ! New ice melting point (K)
            
            zt_i_new       = zswitch_sal * t_bo_1d(ji) + ( 1. - zswitch_sal) * t_i_1d(ji, nlay_i)
            
            zEi            = cpic * ( zt_i_new - ztmelts ) &     ! Specific enthalpy of forming ice (J/kg, <0)      
               &               - lfus * ( 1.0 - ( ztmelts - rt0 ) / ( zt_i_new - rt0 ) )   &
               &               + rcp  * ( ztmelts-rt0 )          
            
            zEw            = rcp  * ( t_bo_1d(ji) - rt0 )         ! Specific enthalpy of seawater (J/kg, < 0)
            
            zdE            = zEi - zEw                           ! Specific enthalpy difference (J/kg, <0)
            
            ! Contribution to heat flux to the ocean [W.m-2], >0  
            hfx_thd_1d(ji) = hfx_thd_1d(ji) + zfmdt * a_i_1d(ji) * zEw * r1_rdtice

            ! Total heat flux used in this process [W.m-2], <0  
            hfx_bog_1d(ji) = hfx_bog_1d(ji) - zfmdt * a_i_1d(ji) * zdE * r1_rdtice
            
            ! Contribution to salt flux, <0
            sfx_bog_1d(ji) = sfx_bog_1d(ji) - rhoic * a_i_1d(ji) * dh_i_bott(ji) * s_i_new(ji) * r1_rdtice

            ! Contribution to mass flux, <0
            wfx_bog_1d(ji) =  wfx_bog_1d(ji) - rhoic * a_i_1d(ji) * dh_i_bott(ji) * r1_rdtice

            ! update heat content (J.m-2) and layer thickness
            qh_i_old(ji,nlay_i+1) = qh_i_old(ji,nlay_i+1) + dh_i_bott(ji) * q_i_1d(ji,nlay_i+1)
            h_i_old (ji,nlay_i+1) = h_i_old (ji,nlay_i+1) + dh_i_bott(ji)
         ENDIF
      END DO

      !----------------
      ! 4.2 Basal melt
      !----------------
      zdeltah(:,:) = 0._wp ! important
      DO jk = nlay_i, 1, -1
         DO ji = kideb, kiut
            IF(  zf_tt(ji)  >  0._wp  .AND. jk > icount(ji,jk) ) THEN   ! do not calculate where layer has already disappeared by surface melting 

               ztmelts = - tmut * s_i_1d(ji,jk) + rt0  ! Melting point of layer jk (K)

               IF( t_i_1d(ji,jk) >= ztmelts ) THEN !!! Internal melting

                  zEi               = - q_i_1d(ji,jk) * r1_rhoic    ! Specific enthalpy of melting ice (J/kg, <0)
                  zdE               = 0._wp                         ! Specific enthalpy difference   (J/kg, <0)
                                                                    ! set up at 0 since no energy is needed to melt water...(it is already melted)
                  zdeltah   (ji,jk) = MIN( 0._wp , - zh_i(ji,jk) )  ! internal melting occurs when the internal temperature is above freezing     
                                                                    ! this should normally not happen, but sometimes, heat diffusion leads to this

                  dh_i_bott (ji)    = dh_i_bott(ji) + zdeltah(ji,jk)

                  zfmdt             = - zdeltah(ji,jk) * rhoic      ! Mass flux x time step > 0

                  ! Contribution to heat flux to the ocean [W.m-2], <0 (ice enthalpy zEi is "sent" to the ocean) 
                  hfx_res_1d(ji) = hfx_res_1d(ji) + zfmdt * a_i_1d(ji) * zEi * r1_rdtice

                  ! Contribution to salt flux (clem: using sm_i_1d and not s_i_1d(jk) is ok)
                  sfx_res_1d(ji) = sfx_res_1d(ji) - rhoic * a_i_1d(ji) * zdeltah(ji,jk) * sm_i_1d(ji) * r1_rdtice
                                    
                  ! Contribution to mass flux
                  wfx_res_1d(ji) =  wfx_res_1d(ji) - rhoic * a_i_1d(ji) * zdeltah(ji,jk) * r1_rdtice

                  ! update heat content (J.m-2) and layer thickness
                  qh_i_old(ji,jk) = qh_i_old(ji,jk) + zdeltah(ji,jk) * q_i_1d(ji,jk)
                  h_i_old (ji,jk) = h_i_old (ji,jk) + zdeltah(ji,jk)

               ELSE                               !!! Basal melting

                  zEi             = - q_i_1d(ji,jk) * r1_rhoic ! Specific enthalpy of melting ice (J/kg, <0)
                  zEw             = rcp * ( ztmelts - rt0 )    ! Specific enthalpy of meltwater (J/kg, <0)
                  zdE             = zEi - zEw                  ! Specific enthalpy difference   (J/kg, <0)

                  zfmdt           = - zq_bo(ji) / zdE          ! Mass flux x time step (kg/m2, >0)

                  zdeltah(ji,jk)  = - zfmdt * r1_rhoic         ! Gross thickness change

                  zdeltah(ji,jk)  = MIN( 0._wp , MAX( zdeltah(ji,jk), - zh_i(ji,jk) ) ) ! bound thickness change
                  
                  zq_bo(ji)       = MAX( 0._wp , zq_bo(ji) - zdeltah(ji,jk) * rhoic * zdE ) ! update available heat. MAX is necessary for roundup errors

                  dh_i_bott(ji)   = dh_i_bott(ji) + zdeltah(ji,jk)    ! Update basal melt

                  zfmdt           = - zdeltah(ji,jk) * rhoic          ! Mass flux x time step > 0

                  zQm             = zfmdt * zEw         ! Heat exchanged with ocean

                  ! Contribution to heat flux to the ocean [W.m-2], <0  
                  hfx_thd_1d(ji)  = hfx_thd_1d(ji) + zfmdt * a_i_1d(ji) * zEw * r1_rdtice

                  ! Contribution to salt flux (clem: using sm_i_1d and not s_i_1d(jk) is ok)
                  sfx_bom_1d(ji)  = sfx_bom_1d(ji) - rhoic *  a_i_1d(ji) * zdeltah(ji,jk) * sm_i_1d(ji) * r1_rdtice
                  
                  ! Total heat flux used in this process [W.m-2], >0  
                  hfx_bom_1d(ji)  = hfx_bom_1d(ji) - zfmdt * a_i_1d(ji) * zdE * r1_rdtice
                  
                  ! Contribution to mass flux
                  wfx_bom_1d(ji)  =  wfx_bom_1d(ji) - rhoic * a_i_1d(ji) * zdeltah(ji,jk) * r1_rdtice

                  ! update heat content (J.m-2) and layer thickness
                  qh_i_old(ji,jk) = qh_i_old(ji,jk) + zdeltah(ji,jk) * q_i_1d(ji,jk)
                  h_i_old (ji,jk) = h_i_old (ji,jk) + zdeltah(ji,jk)
               ENDIF
           
            ENDIF
         END DO
      END DO

      !-------------------------------------------
      ! Update temperature, energy
      !-------------------------------------------
      DO ji = kideb, kiut
         ht_i_1d(ji) =  MAX( 0._wp , ht_i_1d(ji) + dh_i_bott(ji) )
      END DO  

      !-------------------------------------------
      ! 5. What to do with remaining energy
      !-------------------------------------------
      ! If heat still available for melting and snow remains, then melt more snow
      !-------------------------------------------
      zdeltah(:,:) = 0._wp ! important
      DO ji = kideb, kiut
         zq_rema(ji)     = zq_su(ji) + zq_bo(ji) 
         rswitch         = 1._wp - MAX( 0._wp, SIGN( 1._wp, - ht_s_1d(ji) ) )   ! =1 if snow
         rswitch         = rswitch * MAX( 0._wp, SIGN( 1._wp, q_s_1d(ji,1) - epsi20 ) )
         zdeltah  (ji,1) = - rswitch * zq_rema(ji) / MAX( q_s_1d(ji,1), epsi20 )
         zdeltah  (ji,1) = MIN( 0._wp , MAX( zdeltah(ji,1) , - ht_s_1d(ji) ) ) ! bound melting
         dh_s_tot (ji)   = dh_s_tot(ji)  + zdeltah(ji,1)
         ht_s_1d   (ji)  = ht_s_1d(ji)   + zdeltah(ji,1)
        
         zq_rema(ji)     = zq_rema(ji) + zdeltah(ji,1) * q_s_1d(ji,1)                ! update available heat (J.m-2)
         ! heat used to melt snow
         hfx_snw_1d(ji)  = hfx_snw_1d(ji) - zdeltah(ji,1) * a_i_1d(ji) * q_s_1d(ji,1) * r1_rdtice ! W.m-2 (>0)
         ! Contribution to mass flux
         wfx_snw_1d(ji)  =  wfx_snw_1d(ji) - rhosn * a_i_1d(ji) * zdeltah(ji,1) * r1_rdtice
         !    
         ii = MOD( npb(ji) - 1, jpi ) + 1 ; ij = ( npb(ji) - 1 ) / jpi + 1
         ! Remaining heat flux (W.m-2) is sent to the ocean heat budget
         hfx_out(ii,ij)  = hfx_out(ii,ij) + ( zq_rema(ji) * a_i_1d(ji) ) * r1_rdtice

         IF( ln_icectl .AND. zq_rema(ji) < 0. .AND. lwp ) WRITE(numout,*) 'ALERTE zq_rema <0 = ', zq_rema(ji)
      END DO
      
      !
      !------------------------------------------------------------------------------|
      !  6) Snow-Ice formation                                                       |
      !------------------------------------------------------------------------------|
      ! When snow load excesses Archimede's limit, snow-ice interface goes down under sea-level, 
      ! flooding of seawater transforms snow into ice dh_snowice is positive for the ice
      DO ji = kideb, kiut
         !
         dh_snowice(ji) = MAX(  0._wp , ( rhosn * ht_s_1d(ji) + (rhoic-rau0) * ht_i_1d(ji) ) / ( rhosn+rau0-rhoic )  )

         ht_i_1d(ji)    = ht_i_1d(ji) + dh_snowice(ji)
         ht_s_1d(ji)    = ht_s_1d(ji) - dh_snowice(ji)

         ! Salinity of snow ice
         ii = MOD( npb(ji) - 1, jpi ) + 1 ; ij = ( npb(ji) - 1 ) / jpi + 1
         zs_snic = zswitch_sal * sss_m(ii,ij) * ( rhoic - rhosn ) * r1_rhoic + ( 1. - zswitch_sal ) * sm_i_1d(ji)

         ! entrapment during snow ice formation
         ! new salinity difference stored (to be used in limthd_sal.F90)
         IF (  nn_icesal == 2  ) THEN
            rswitch = MAX( 0._wp , SIGN( 1._wp , ht_i_1d(ji) - epsi20 ) )
            ! salinity dif due to snow-ice formation
            dsm_i_si_1d(ji) = ( zs_snic - sm_i_1d(ji) ) * dh_snowice(ji) / MAX( ht_i_1d(ji), epsi20 ) * rswitch     
            ! salinity dif due to bottom growth 
            IF (  zf_tt(ji)  < 0._wp ) THEN
               dsm_i_se_1d(ji) = ( s_i_new(ji) - sm_i_1d(ji) ) * dh_i_bott(ji) / MAX( ht_i_1d(ji), epsi20 ) * rswitch
            ENDIF
         ENDIF

         ! Contribution to energy flux to the ocean [J/m2], >0 (if sst<0)
         zfmdt          = ( rhosn - rhoic ) * MAX( dh_snowice(ji), 0._wp )    ! <0
         zsstK          = sst_m(ii,ij) + rt0                                
         zEw            = rcp * ( zsstK - rt0 )
         zQm            = zfmdt * zEw 
         
         ! Contribution to heat flux
         hfx_thd_1d(ji) = hfx_thd_1d(ji) + zfmdt * a_i_1d(ji) * zEw * r1_rdtice 

         ! Contribution to salt flux
         sfx_sni_1d(ji) = sfx_sni_1d(ji) + sss_m(ii,ij) * a_i_1d(ji) * zfmdt * r1_rdtice 

         ! virtual salt flux to keep salinity constant
         IF( nn_icesal == 1 .OR. nn_icesal == 3 )  THEN
            sfx_bri_1d(ji) = sfx_bri_1d(ji) - sss_m(ii,ij) * a_i_1d(ji) * zfmdt                  * r1_rdtice  & ! put back sss_m into the ocean
               &                            - sm_i_1d(ji)  * a_i_1d(ji) * dh_snowice(ji) * rhoic * r1_rdtice    ! and get  sm_i  from the ocean 
         ENDIF
          
         ! Contribution to mass flux
         ! All snow is thrown in the ocean, and seawater is taken to replace the volume
         wfx_sni_1d(ji) = wfx_sni_1d(ji) - a_i_1d(ji) * dh_snowice(ji) * rhoic * r1_rdtice
         wfx_snw_1d(ji) = wfx_snw_1d(ji) + a_i_1d(ji) * dh_snowice(ji) * rhosn * r1_rdtice

         ! update heat content (J.m-2) and layer thickness
         qh_i_old(ji,0) = qh_i_old(ji,0) + dh_snowice(ji) * q_s_1d(ji,1) + zfmdt * zEw
         h_i_old (ji,0) = h_i_old (ji,0) + dh_snowice(ji)
         
      END DO

      !
      !-------------------------------------------
      ! Update temperature, energy
      !-------------------------------------------
      DO ji = kideb, kiut
         rswitch     =  1.0 - MAX( 0._wp , SIGN( 1._wp , - ht_i_1d(ji) ) ) 
         t_su_1d(ji) =  rswitch * t_su_1d(ji) + ( 1.0 - rswitch ) * rt0
      END DO

      DO jk = 1, nlay_s
         DO ji = kideb,kiut
            ! mask enthalpy
            rswitch       = 1._wp - MAX(  0._wp , SIGN( 1._wp, - ht_s_1d(ji) )  )
            q_s_1d(ji,jk) = rswitch * q_s_1d(ji,jk)
            ! recalculate t_s_1d from q_s_1d
            t_s_1d(ji,jk) = rt0 + rswitch * ( - q_s_1d(ji,jk) / ( rhosn * cpic ) + lfus / cpic )
         END DO
      END DO

      ! --- ensure that a_i = 0 where ht_i = 0 ---
      WHERE( ht_i_1d == 0._wp ) a_i_1d = 0._wp
      
      CALL wrk_dealloc( jpij, zqprec, zq_su, zq_bo, zf_tt, zq_rema, zsnw, zevap_rema )
      CALL wrk_dealloc( jpij, zdh_s_mel, zdh_s_pre, zdh_s_sub, zqh_i )
      CALL wrk_dealloc( jpij, nlay_i, zdeltah, zh_i )
      CALL wrk_dealloc( jpij, nlay_i, icount )
      !
      !
   END SUBROUTINE lim_thd_dh


   !!--------------------------------------------------------------------------
   !! INTERFACE lim_thd_snwblow
   !! ** Purpose :   Compute distribution of precip over the ice
   !!--------------------------------------------------------------------------
   SUBROUTINE lim_thd_snwblow_2d( pin, pout )
      REAL(wp), DIMENSION(:,:), INTENT(in   ) :: pin   ! previous fraction lead ( pfrld or (1. - a_i_b) )
      REAL(wp), DIMENSION(:,:), INTENT(inout) :: pout
      pout = ( 1._wp - ( pin )**rn_betas )
   END SUBROUTINE lim_thd_snwblow_2d

   SUBROUTINE lim_thd_snwblow_1d( pin, pout )
      REAL(wp), DIMENSION(:), INTENT(in   ) :: pin
      REAL(wp), DIMENSION(:), INTENT(inout) :: pout
      pout = ( 1._wp - ( pin )**rn_betas )
   END SUBROUTINE lim_thd_snwblow_1d

   
#else
   !!----------------------------------------------------------------------
   !!   Default option                               NO  LIM3 sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_thd_dh          ! Empty routine
   END SUBROUTINE lim_thd_dh
#endif

   !!======================================================================
END MODULE limthd_dh
