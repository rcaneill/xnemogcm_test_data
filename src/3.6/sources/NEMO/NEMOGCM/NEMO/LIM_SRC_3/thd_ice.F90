MODULE thd_ice
   !!======================================================================
   !!                       ***  MODULE thd_ice  ***
   !! LIM sea-ice :   Ice thermodynamics in 1D
   !!=====================================================================
   !! History :  3.0  !  2002-11  (C. Ethe)  F90: Free form and module
   !!----------------------------------------------------------------------
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE ice, ONLY :   nlay_i, nlay_s

   IMPLICIT NONE
   PRIVATE

   PUBLIC thd_ice_alloc ! Routine called by nemogcm.F90

   !!---------------------------
   !! * Share Module variables
   !!---------------------------
   !                               !!! ** ice-thermo namelist (namicethd) **
   REAL(wp), PUBLIC ::   rn_himin    !: minimum ice thickness
   REAL(wp), PUBLIC ::   rn_maxfrazb !: maximum portion of frazil ice collecting at the ice bottom
   REAL(wp), PUBLIC ::   rn_vfrazb   !: threshold drift speed for collection of bottom frazil ice
   REAL(wp), PUBLIC ::   rn_Cfrazb   !: squeezing coefficient for collection of bottom frazil ice
   REAL(wp), PUBLIC ::   rn_hnewice  !: thickness for new ice formation (m)

   LOGICAL , PUBLIC ::   ln_frazil   !: use of frazil ice collection as function of wind (T) or not (F)

   !!-----------------------------
   !! * Share 1D Module variables
   !!-----------------------------
   !: In ice thermodynamics, to spare memory, the vectors are folded
   !: from 1D to 2D vectors. The following variables, with ending _1d
   !: are the variables corresponding to 2d vectors

   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   npb    !: address vector for 1d vertical thermo computations
   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   nplm   !: address vector for mono-category lateral melting
   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   npac   !: address vector for new ice formation

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qlead_1d     
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   ftr_ice_1d   
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qsr_ice_1d  
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   fr1_i0_1d   
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   fr2_i0_1d   
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qns_ice_1d  
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   t_bo_1d     
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   rn_amax_1d

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_sum_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_bom_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_bog_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_dif_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_opw_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_snw_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_err_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_err_rem_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_err_dif_1d

   ! heat flux associated with ice-atmosphere mass exchange
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_sub_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_spr_1d

   ! heat flux associated with ice-ocean mass exchange
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_thd_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_res_1d

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_snw_1d 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_sub_1d

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_bog_1d    
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_bom_1d   
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_sum_1d  
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_sni_1d 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_opw_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_res_1d 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_spr_1d

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sfx_bri_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sfx_bog_1d    
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sfx_bom_1d    
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sfx_sum_1d    
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sfx_sni_1d    
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sfx_opw_1d   
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sfx_res_1d  

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sfx_sub_1d

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sprecip_1d    !: <==> the 2D  sprecip
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   frld_1d       !: <==> the 2D  frld
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   at_i_1d        !: <==> the 2D  at_i
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   fhtur_1d      !: <==> the 2D  fhtur
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   fhld_1d       !: <==> the 2D  fhld
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dqns_ice_1d   !: <==> the 2D  dqns_ice
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   evap_ice_1d   !: <==> the 2D  evap_ice
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qprec_ice_1d  !: <==> the 2D  qprec_ice
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qevap_ice_1d  !: <==> the 3D  qevap_ice
   !                                                     ! to reintegrate longwave flux inside the ice thermodynamics
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   i0            !: fraction of radiation transmitted to the ice
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dsm_i_fl_1d   !: Ice salinity variations due to flushing
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dsm_i_gd_1d   !: Ice salinity variations due to gravity drainage
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dsm_i_se_1d   !: Ice salinity variations due to basal salt entrapment
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dsm_i_si_1d   !: Ice salinity variations due to lateral accretion    
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hicol_1d      !: Ice collection thickness accumulated in leads

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   t_su_1d       !: <==> the 2D  t_su
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   a_i_1d        !: <==> the 2D  a_i
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   ht_i_1d       !: <==> the 2D  ht_s
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   ht_s_1d       !: <==> the 2D  ht_i
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   fc_su         !: Surface Conduction flux 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   fc_bo_i       !: Bottom  Conduction flux 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dh_s_tot      !: Snow accretion/ablation        [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dh_i_surf     !: Ice surface accretion/ablation [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dh_i_sub      !: Ice surface sublimation [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dh_i_bott     !: Ice bottom accretion/ablation  [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dh_snowice    !: Snow ice formation             [m of ice]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sm_i_1d       !: Ice bulk salinity [ppt]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   s_i_new       !: Salinity of new ice at the bottom

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   t_s_1d   !: corresponding to the 2D var  t_s
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   t_i_1d   !: corresponding to the 2D var  t_i
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   s_i_1d   !: profiled ice salinity
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   q_i_1d   !:    Ice  enthalpy per unit volume
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   q_s_1d   !:    Snow enthalpy per unit volume

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   qh_i_old !: ice heat content (q*h, J.m-2)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   h_i_old  !: ice thickness layer (m)

   INTEGER , PUBLIC ::   jiindex_1d   ! 1D index of debugging point

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: thd_ice.F90 6399 2016-03-22 17:17:23Z clem $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   FUNCTION thd_ice_alloc()
      !!---------------------------------------------------------------------!
      !!                ***  ROUTINE thd_ice_alloc ***
      !!---------------------------------------------------------------------!
      INTEGER ::   thd_ice_alloc   ! return value
      INTEGER ::   ierr(3)
      !!---------------------------------------------------------------------!

      ALLOCATE( npb      (jpij) , nplm      (jpij) , npac       (jpij) ,   &
         &      qlead_1d (jpij) , ftr_ice_1d(jpij) , qsr_ice_1d (jpij) ,   &
         &      fr1_i0_1d(jpij) , fr2_i0_1d (jpij) , qns_ice_1d(jpij)  ,   &
         &      t_bo_1d   (jpij) ,                                         &
         &      hfx_sum_1d(jpij) , hfx_bom_1d(jpij) ,hfx_bog_1d(jpij) ,    & 
         &      hfx_dif_1d(jpij) , hfx_opw_1d(jpij) ,                      &
         &      rn_amax_1d(jpij) ,                                         &
         &      hfx_thd_1d(jpij) , hfx_spr_1d(jpij) ,                      &
         &      hfx_snw_1d(jpij) , hfx_sub_1d(jpij) , hfx_err_1d(jpij) ,   &
         &      hfx_res_1d(jpij) , hfx_err_rem_1d(jpij) , hfx_err_dif_1d(jpij) , STAT=ierr(1) )
      !
      ALLOCATE( sprecip_1d (jpij) , frld_1d    (jpij) , at_i_1d    (jpij) ,                     &
         &      fhtur_1d   (jpij) , wfx_snw_1d (jpij) , wfx_spr_1d (jpij) ,                     &
         &      fhld_1d    (jpij) , wfx_sub_1d (jpij) , wfx_bog_1d (jpij) , wfx_bom_1d(jpij) ,  &
         &      wfx_sum_1d(jpij)  , wfx_sni_1d (jpij) , wfx_opw_1d (jpij) , wfx_res_1d(jpij) ,  &
         &      dqns_ice_1d(jpij) , evap_ice_1d (jpij),                                         &
         &      qprec_ice_1d(jpij), qevap_ice_1d(jpij), i0         (jpij) ,                     &  
         &      sfx_bri_1d (jpij) , sfx_bog_1d (jpij) , sfx_bom_1d (jpij) , sfx_sum_1d (jpij),  &
         &      sfx_sni_1d (jpij) , sfx_opw_1d (jpij) , sfx_res_1d (jpij) , sfx_sub_1d (jpij),  &
         &      dsm_i_fl_1d(jpij) , dsm_i_gd_1d(jpij) , dsm_i_se_1d(jpij) ,                     &     
         &      dsm_i_si_1d(jpij) , hicol_1d    (jpij)                     , STAT=ierr(2) )
      !
      ALLOCATE( t_su_1d   (jpij) , a_i_1d   (jpij) , ht_i_1d  (jpij) ,    &   
         &      ht_s_1d   (jpij) , fc_su    (jpij) , fc_bo_i  (jpij) ,    &    
         &      dh_s_tot  (jpij) , dh_i_surf(jpij) , dh_i_sub (jpij) ,    &    
         &      dh_i_bott (jpij) ,dh_snowice(jpij) , sm_i_1d  (jpij) , s_i_new  (jpij) ,    &
         &      t_s_1d(jpij,nlay_s) , t_i_1d(jpij,nlay_i) , s_i_1d(jpij,nlay_i) ,  &            
         &      q_i_1d(jpij,nlay_i+1) , q_s_1d(jpij,nlay_s) ,                        &
         &      qh_i_old(jpij,0:nlay_i+1), h_i_old(jpij,0:nlay_i+1) , STAT=ierr(3))
      !
      thd_ice_alloc = MAXVAL( ierr )

      IF( thd_ice_alloc /= 0 )   CALL ctl_warn( 'thd_ice_alloc: failed to allocate arrays.' )
      !
   END FUNCTION thd_ice_alloc
   
   !!======================================================================
END MODULE thd_ice
