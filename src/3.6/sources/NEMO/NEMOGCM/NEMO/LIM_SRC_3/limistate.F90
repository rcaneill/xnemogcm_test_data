MODULE limistate
   !!======================================================================
   !!                     ***  MODULE  limistate  ***
   !!              Initialisation of diagnostics ice variables
   !!======================================================================
   !! History :  2.0  ! 2004-01 (C. Ethe, G. Madec)  Original code
   !!            4.0  ! 2011-02 (G. Madec) dynamical allocation
   !!             -   ! 2014    (C. Rousset) add N/S initializations
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3' :                                    LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_istate      :  Initialisation of diagnostics ice variables
   !!   lim_istate_init :  initialization of ice state and namelist read
   !!----------------------------------------------------------------------
   USE phycst           ! physical constant
   USE oce              ! dynamics and tracers variables
   USE dom_oce          ! ocean domain
   USE sbc_oce          ! Surface boundary condition: ocean fields
   USE sbc_ice          ! Surface boundary condition: ice fields
   USE eosbn2           ! equation of state
   USE ice              ! sea-ice variables
   USE par_oce          ! ocean parameters
   USE dom_ice          ! sea-ice domain
   USE limvar           ! lim_var_salprof
   USE in_out_manager   ! I/O manager
   USE lib_mpp          ! MPP library
   USE lib_fortran      ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)  
   USE wrk_nemo         ! work arrays

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_istate      ! routine called by lim_init.F90

   !                          !!** init namelist (namiceini) **
   REAL(wp) ::   rn_thres_sst   ! threshold water temperature for initial sea ice
   REAL(wp) ::   rn_hts_ini_n   ! initial snow thickness in the north
   REAL(wp) ::   rn_hts_ini_s   ! initial snow thickness in the south
   REAL(wp) ::   rn_hti_ini_n   ! initial ice thickness in the north
   REAL(wp) ::   rn_hti_ini_s   ! initial ice thickness in the south
   REAL(wp) ::   rn_ati_ini_n   ! initial leads area in the north
   REAL(wp) ::   rn_ati_ini_s   ! initial leads area in the south
   REAL(wp) ::   rn_smi_ini_n   ! initial salinity 
   REAL(wp) ::   rn_smi_ini_s   ! initial salinity
   REAL(wp) ::   rn_tmi_ini_n   ! initial temperature
   REAL(wp) ::   rn_tmi_ini_s   ! initial temperature

   LOGICAL  ::  ln_iceini    ! initialization or not
   !!----------------------------------------------------------------------
   !!   LIM 3.0,  UCL-LOCEAN-IPSL (2008)
   !! $Id: limistate.F90 6696 2016-06-13 14:55:42Z clem $
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_istate
      !!-------------------------------------------------------------------
      !!                    ***  ROUTINE lim_istate  ***
      !!
      !! ** Purpose :   defined the sea-ice initial state
      !!
      !! ** Method  :   
      !!                This routine will put some ice where ocean
      !!                is at the freezing point, then fill in ice 
      !!                state variables using prescribed initial 
      !!                values in the namelist            
      !!
      !! ** Steps   :   
      !!                1) Read namelist
      !!                2) Basal temperature; ice and hemisphere masks
      !!                3) Fill in the ice thickness distribution using gaussian
      !!                4) Fill in space-dependent arrays for state variables
      !!                5) Diagnostic arrays
      !!                6) Lateral boundary conditions
      !!
      !! ** Notes   : o_i, t_su, t_s, t_i, s_i must be filled everywhere, even
      !!              where there is no ice (clem: I do not know why, is it mandatory?) 
      !!
      !! History :
      !!   2.0  !  01-04  (C. Ethe, G. Madec)  Original code
      !!   3.0  !  2007   (M. Vancoppenolle)   Rewrite for ice cats
      !!   4.0  !  09-11  (M. Vancoppenolle)   Enhanced version for ice cats
      !!--------------------------------------------------------------------

      !! * Local variables
      INTEGER    :: ji, jj, jk, jl             ! dummy loop indices
      REAL(wp)   :: ztmelts, zdh
      INTEGER    :: i_hemis, i_fill, jl0  
      REAL(wp)   :: ztest_1, ztest_2, ztest_3, ztest_4, ztests, zsigma, zarg, zA, zV, zA_cons, zV_cons, zconv
      REAL(wp), POINTER, DIMENSION(:)     :: zht_i_ini, zat_i_ini, zvt_i_ini, zht_s_ini, zsm_i_ini, ztm_i_ini
      REAL(wp), POINTER, DIMENSION(:,:)   :: zh_i_ini, za_i_ini, zv_i_ini
      REAL(wp), POINTER, DIMENSION(:,:)   :: zswitch    ! ice indicator
      INTEGER,  POINTER, DIMENSION(:,:)   :: zhemis   ! hemispheric index
      !--------------------------------------------------------------------

      CALL wrk_alloc( jpi, jpj, zswitch )
      CALL wrk_alloc( jpi, jpj, zhemis )
      CALL wrk_alloc( jpl,   2, zh_i_ini,  za_i_ini,  zv_i_ini )
      CALL wrk_alloc(   2,      zht_i_ini, zat_i_ini, zvt_i_ini, zht_s_ini, zsm_i_ini, ztm_i_ini )

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'lim_istate : Ice initialization '
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ '

      !--------------------------------------------------------------------
      ! 1) Read namelist
      !--------------------------------------------------------------------

      CALL lim_istate_init     !  reading the initials parameters of the ice

      ! surface temperature
      DO jl = 1, jpl ! loop over categories
         t_su  (:,:,jl) = rt0 * tmask(:,:,1)
         tn_ice(:,:,jl) = rt0 * tmask(:,:,1)
      END DO

      ! basal temperature (considered at freezing point)
      CALL eos_fzp( sss_m(:,:), t_bo(:,:) )
      t_bo(:,:) = ( t_bo(:,:) + rt0 ) * tmask(:,:,1) 

      IF( ln_iceini ) THEN

      !--------------------------------------------------------------------
      ! 2) Basal temperature, ice mask and hemispheric index
      !--------------------------------------------------------------------

      DO jj = 1, jpj                                       ! ice if sst <= t-freez + ttest
         DO ji = 1, jpi
            IF( ( sst_m(ji,jj)  - ( t_bo(ji,jj) - rt0 ) ) * tmask(ji,jj,1) >= rn_thres_sst ) THEN 
               zswitch(ji,jj) = 0._wp * tmask(ji,jj,1)    ! no ice
            ELSE                                                                                   
               zswitch(ji,jj) = 1._wp * tmask(ji,jj,1)    !    ice
            ENDIF
         END DO
      END DO


      ! Hemispheric index
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF( fcor(ji,jj) >= 0._wp ) THEN    
               zhemis(ji,jj) = 1 ! Northern hemisphere
            ELSE
               zhemis(ji,jj) = 2 ! Southern hemisphere
            ENDIF
         END DO
      END DO

      !--------------------------------------------------------------------
      ! 3) Initialization of sea ice state variables
      !--------------------------------------------------------------------

      !-----------------------------
      ! 3.1) Hemisphere-dependent arrays
      !-----------------------------
      ! assign initial thickness, concentration, snow depth and salinity to an hemisphere-dependent array
      zht_i_ini(1) = rn_hti_ini_n ; zht_i_ini(2) = rn_hti_ini_s  ! ice thickness
      zht_s_ini(1) = rn_hts_ini_n ; zht_s_ini(2) = rn_hts_ini_s  ! snow depth
      zat_i_ini(1) = rn_ati_ini_n ; zat_i_ini(2) = rn_ati_ini_s  ! ice concentration
      zsm_i_ini(1) = rn_smi_ini_n ; zsm_i_ini(2) = rn_smi_ini_s  ! bulk ice salinity
      ztm_i_ini(1) = rn_tmi_ini_n ; ztm_i_ini(2) = rn_tmi_ini_s  ! temperature (ice and snow)

      zvt_i_ini(:) = zht_i_ini(:) * zat_i_ini(:)   ! ice volume

      !---------------------------------------------------------------------
      ! 3.2) Distribute ice concentration and thickness into the categories
      !---------------------------------------------------------------------
      ! a gaussian distribution for ice concentration is used
      ! then we check whether the distribution fullfills
      ! volume and area conservation, positivity and ice categories bounds
      DO i_hemis = 1, 2

      ztest_1 = 0 ; ztest_2 = 0 ; ztest_3 = 0 ; ztest_4 = 0

      ! note for the great nemo engineers: 
      ! only very few of the WRITE statements are necessary for the reference version
      ! they were one day useful, but now i personally doubt of their
      ! potential for bringing anything useful

      DO i_fill = jpl, 1, -1
         IF ( ( ztest_1 + ztest_2 + ztest_3 + ztest_4 ) .NE. 4 ) THEN
            !----------------------------
            ! fill the i_fill categories
            !----------------------------
            ! *** 1 category to fill
            IF ( i_fill .EQ. 1 ) THEN
               zh_i_ini(1,i_hemis)       = zht_i_ini(i_hemis)
               za_i_ini(1,i_hemis)       = zat_i_ini(i_hemis)
               zh_i_ini(2:jpl,i_hemis)   = 0._wp
               za_i_ini(2:jpl,i_hemis)   = 0._wp
            ELSE

               ! *** >1 categores to fill
               !--- Ice thicknesses in the i_fill - 1 first categories
               DO jl = 1, i_fill - 1
                  zh_i_ini(jl,i_hemis) = hi_mean(jl)
               END DO
               
               !--- jl0: most likely index where cc will be maximum
               DO jl = 1, jpl
                  IF ( ( zht_i_ini(i_hemis) >  hi_max(jl-1) ) .AND. &
                     & ( zht_i_ini(i_hemis) <= hi_max(jl)   ) ) THEN
                     jl0 = jl
                  ENDIF
               END DO
               jl0 = MIN(jl0, i_fill)
               
               !--- Concentrations
               za_i_ini(jl0,i_hemis)      = zat_i_ini(i_hemis) / SQRT(REAL(jpl))
               DO jl = 1, i_fill - 1
                  IF ( jl .NE. jl0 ) THEN
                     zsigma               = 0.5 * zht_i_ini(i_hemis)
                     zarg                 = ( zh_i_ini(jl,i_hemis) - zht_i_ini(i_hemis) ) / zsigma
                     za_i_ini(jl,i_hemis) = za_i_ini(jl0,i_hemis) * EXP(-zarg**2)
                  ENDIF
               END DO
               
               zA = 0. ! sum of the areas in the jpl categories 
               DO jl = 1, i_fill - 1
                 zA = zA + za_i_ini(jl,i_hemis)
               END DO
               za_i_ini(i_fill,i_hemis)   = zat_i_ini(i_hemis) - zA ! ice conc in the last category
               IF ( i_fill .LT. jpl ) za_i_ini(i_fill+1:jpl, i_hemis) = 0._wp
         
               !--- Ice thickness in the last category
               zV = 0. ! sum of the volumes of the N-1 categories
               DO jl = 1, i_fill - 1
                  zV = zV + za_i_ini(jl,i_hemis)*zh_i_ini(jl,i_hemis)
               END DO
               zh_i_ini(i_fill,i_hemis) = ( zvt_i_ini(i_hemis) - zV ) / za_i_ini(i_fill,i_hemis) 
               IF ( i_fill .LT. jpl ) zh_i_ini(i_fill+1:jpl, i_hemis) = 0._wp

               !--- volumes
               zv_i_ini(:,i_hemis) = za_i_ini(:,i_hemis) * zh_i_ini(:,i_hemis)
               IF ( i_fill .LT. jpl ) zv_i_ini(i_fill+1:jpl, i_hemis) = 0._wp

            ENDIF ! i_fill

            !---------------------
            ! Compatibility tests
            !---------------------
            ! Test 1: area conservation
            zA_cons = SUM(za_i_ini(:,i_hemis)) ; zconv = ABS(zat_i_ini(i_hemis) - zA_cons )
            IF ( zconv .LT. 1.0e-6 ) THEN
               ztest_1 = 1
            ELSE 
               ztest_1 = 0
            ENDIF

            ! Test 2: volume conservation
            zV_cons = SUM(zv_i_ini(:,i_hemis))
            zconv = ABS(zvt_i_ini(i_hemis) - zV_cons)

            IF ( zconv .LT. 1.0e-6 ) THEN
               ztest_2 = 1
            ELSE
               ztest_2 = 0
            ENDIF

            ! Test 3: thickness of the last category is in-bounds ?
            IF ( zh_i_ini(i_fill, i_hemis) > hi_max(i_fill-1) ) THEN
               ztest_3 = 1
            ELSE
               ztest_3 = 0
            ENDIF

            ! Test 4: positivity of ice concentrations
            ztest_4 = 1
            DO jl = 1, jpl
               IF ( za_i_ini(jl,i_hemis) .LT. 0._wp ) THEN 
                  ztest_4 = 0
               ENDIF
            END DO

         ENDIF ! ztest_1 + ztest_2 + ztest_3 + ztest_4
 
         ztests = ztest_1 + ztest_2 + ztest_3 + ztest_4

      END DO ! i_fill

      IF(lwp) THEN 
         WRITE(numout,*) ' ztests : ', ztests
         IF ( ztests .NE. 4 ) THEN
            WRITE(numout,*)
            WRITE(numout,*) ' !!!! ALERT                  !!! '
            WRITE(numout,*) ' !!!! Something is wrong in the LIM3 initialization procedure '
            WRITE(numout,*)
            WRITE(numout,*) ' *** ztests is not equal to 4 '
            WRITE(numout,*) ' *** ztest_i (i=1,4) = ', ztest_1, ztest_2, ztest_3, ztest_4
            WRITE(numout,*) ' zat_i_ini : ', zat_i_ini(i_hemis)
            WRITE(numout,*) ' zht_i_ini : ', zht_i_ini(i_hemis)
         ENDIF ! ztests .NE. 4
      ENDIF
      
      END DO ! i_hemis

      !---------------------------------------------------------------------
      ! 3.3) Space-dependent arrays for ice state variables
      !---------------------------------------------------------------------

      ! Ice concentration, thickness and volume, ice salinity, ice age, surface temperature
      DO jl = 1, jpl ! loop over categories
         DO jj = 1, jpj
            DO ji = 1, jpi
               a_i(ji,jj,jl)   = zswitch(ji,jj) * za_i_ini (jl,zhemis(ji,jj))  ! concentration
               ht_i(ji,jj,jl)  = zswitch(ji,jj) * zh_i_ini(jl,zhemis(ji,jj))   ! ice thickness
               ht_s(ji,jj,jl)  = ht_i(ji,jj,jl) * ( zht_s_ini( zhemis(ji,jj) ) / zht_i_ini( zhemis(ji,jj) ) )  ! snow depth
               sm_i(ji,jj,jl)  = zswitch(ji,jj) * zsm_i_ini(zhemis(ji,jj))     ! salinity
               o_i(ji,jj,jl)   = zswitch(ji,jj) * 1._wp                        ! age (1 day)
               t_su(ji,jj,jl)  = zswitch(ji,jj) * ztm_i_ini(zhemis(ji,jj)) + ( 1._wp - zswitch(ji,jj) ) * rt0 ! surf temp

               ! This case below should not be used if (ht_s/ht_i) is ok in namelist
               ! In case snow load is in excess that would lead to transformation from snow to ice
               ! Then, transfer the snow excess into the ice (different from limthd_dh)
               zdh = MAX( 0._wp, ( rhosn * ht_s(ji,jj,jl) + ( rhoic - rau0 ) * ht_i(ji,jj,jl) ) * r1_rau0 ) 
               ! recompute ht_i, ht_s avoiding out of bounds values
               ht_i(ji,jj,jl) = MIN( hi_max(jl), ht_i(ji,jj,jl) + zdh )
               ht_s(ji,jj,jl) = MAX( 0._wp, ht_s(ji,jj,jl) - zdh * rhoic * r1_rhosn )

               ! ice volume, salt content, age content
               v_i(ji,jj,jl)   = ht_i(ji,jj,jl) * a_i(ji,jj,jl)              ! ice volume
               v_s(ji,jj,jl)   = ht_s(ji,jj,jl) * a_i(ji,jj,jl)              ! snow volume
               smv_i(ji,jj,jl) = MIN( sm_i(ji,jj,jl) , sss_m(ji,jj) ) * v_i(ji,jj,jl) ! salt content
               oa_i(ji,jj,jl)  = o_i(ji,jj,jl) * a_i(ji,jj,jl)               ! age content
            END DO
         END DO
      END DO

      ! for constant salinity in time
      IF( nn_icesal == 1 .OR. nn_icesal == 3 )  THEN
         CALL lim_var_salprof
         smv_i = sm_i * v_i
      ENDIF

      ! Snow temperature and heat content
      DO jk = 1, nlay_s
         DO jl = 1, jpl ! loop over categories
            DO jj = 1, jpj
               DO ji = 1, jpi
                   t_s(ji,jj,jk,jl) = zswitch(ji,jj) * ztm_i_ini(zhemis(ji,jj)) + ( 1._wp - zswitch(ji,jj) ) * rt0
                   ! Snow energy of melting
                   e_s(ji,jj,jk,jl) = zswitch(ji,jj) * rhosn * ( cpic * ( rt0 - t_s(ji,jj,jk,jl) ) + lfus )

                   ! Mutliply by volume, and divide by number of layers to get heat content in J/m2
                   e_s(ji,jj,jk,jl) = e_s(ji,jj,jk,jl) * v_s(ji,jj,jl) * r1_nlay_s
               END DO
            END DO
         END DO
      END DO

      ! Ice salinity, temperature and heat content
      DO jk = 1, nlay_i
         DO jl = 1, jpl ! loop over categories
            DO jj = 1, jpj
               DO ji = 1, jpi
                   t_i(ji,jj,jk,jl) = zswitch(ji,jj) * ztm_i_ini(zhemis(ji,jj)) + ( 1._wp - zswitch(ji,jj) ) * rt0 
                   s_i(ji,jj,jk,jl) = zswitch(ji,jj) * zsm_i_ini(zhemis(ji,jj)) !+ ( 1._wp - zswitch(ji,jj) ) * rn_simin
                   ztmelts          = - tmut * s_i(ji,jj,jk,jl) + rt0 !Melting temperature in K

                   ! heat content per unit volume
                   e_i(ji,jj,jk,jl) = zswitch(ji,jj) * rhoic * (   cpic    * ( ztmelts - t_i(ji,jj,jk,jl) ) &
                      +   lfus    * ( 1._wp - (ztmelts-rt0) / MIN((t_i(ji,jj,jk,jl)-rt0),-epsi20) ) &
                      -   rcp     * ( ztmelts - rt0 ) )

                   ! Mutliply by ice volume, and divide by number of layers to get heat content in J/m2
                   e_i(ji,jj,jk,jl) = e_i(ji,jj,jk,jl) * v_i(ji,jj,jl) * r1_nlay_i
               END DO
            END DO
         END DO
      END DO

      tn_ice (:,:,:) = t_su (:,:,:)

      ELSE 
         ! if ln_iceini=false
         a_i  (:,:,:) = 0._wp
         v_i  (:,:,:) = 0._wp
         v_s  (:,:,:) = 0._wp
         smv_i(:,:,:) = 0._wp
         oa_i (:,:,:) = 0._wp
         ht_i (:,:,:) = 0._wp
         ht_s (:,:,:) = 0._wp
         sm_i (:,:,:) = 0._wp
         o_i  (:,:,:) = 0._wp

         e_i(:,:,:,:) = 0._wp
         e_s(:,:,:,:) = 0._wp

         DO jl = 1, jpl
            DO jk = 1, nlay_i
               t_i(:,:,jk,jl) = rt0 * tmask(:,:,1)
            END DO
            DO jk = 1, nlay_s
               t_s(:,:,jk,jl) = rt0 * tmask(:,:,1)
            END DO
         END DO
      
      ENDIF ! ln_iceini
      
      at_i (:,:) = 0.0_wp
      DO jl = 1, jpl
         at_i (:,:) = at_i (:,:) + a_i (:,:,jl)
      END DO
      !
      !--------------------------------------------------------------------
      ! 4) Global ice variables for output diagnostics                    | 
      !--------------------------------------------------------------------
      u_ice (:,:)     = 0._wp
      v_ice (:,:)     = 0._wp
      stress1_i(:,:)  = 0._wp
      stress2_i(:,:)  = 0._wp
      stress12_i(:,:) = 0._wp

      !--------------------------------------------------------------------
      ! 5) Moments for advection
      !--------------------------------------------------------------------

      sxopw (:,:) = 0._wp 
      syopw (:,:) = 0._wp 
      sxxopw(:,:) = 0._wp 
      syyopw(:,:) = 0._wp 
      sxyopw(:,:) = 0._wp

      sxice (:,:,:)  = 0._wp   ;   sxsn (:,:,:)  = 0._wp   ;   sxa  (:,:,:)  = 0._wp
      syice (:,:,:)  = 0._wp   ;   sysn (:,:,:)  = 0._wp   ;   sya  (:,:,:)  = 0._wp
      sxxice(:,:,:)  = 0._wp   ;   sxxsn(:,:,:)  = 0._wp   ;   sxxa (:,:,:)  = 0._wp
      syyice(:,:,:)  = 0._wp   ;   syysn(:,:,:)  = 0._wp   ;   syya (:,:,:)  = 0._wp
      sxyice(:,:,:)  = 0._wp   ;   sxysn(:,:,:)  = 0._wp   ;   sxya (:,:,:)  = 0._wp

      sxc0  (:,:,:)  = 0._wp   ;   sxe  (:,:,:,:)= 0._wp   
      syc0  (:,:,:)  = 0._wp   ;   sye  (:,:,:,:)= 0._wp   
      sxxc0 (:,:,:)  = 0._wp   ;   sxxe (:,:,:,:)= 0._wp   
      syyc0 (:,:,:)  = 0._wp   ;   syye (:,:,:,:)= 0._wp   
      sxyc0 (:,:,:)  = 0._wp   ;   sxye (:,:,:,:)= 0._wp   

      sxsal  (:,:,:)  = 0._wp
      sysal  (:,:,:)  = 0._wp
      sxxsal (:,:,:)  = 0._wp
      syysal (:,:,:)  = 0._wp
      sxysal (:,:,:)  = 0._wp

      sxage  (:,:,:)  = 0._wp
      syage  (:,:,:)  = 0._wp
      sxxage (:,:,:)  = 0._wp
      syyage (:,:,:)  = 0._wp
      sxyage (:,:,:)  = 0._wp


      CALL wrk_dealloc( jpi, jpj, zswitch )
      CALL wrk_dealloc( jpi, jpj, zhemis )
      CALL wrk_dealloc( jpl,   2, zh_i_ini,  za_i_ini,  zv_i_ini )
      CALL wrk_dealloc(   2,      zht_i_ini, zat_i_ini, zvt_i_ini, zht_s_ini, zsm_i_ini, ztm_i_ini )

   END SUBROUTINE lim_istate

   SUBROUTINE lim_istate_init
      !!-------------------------------------------------------------------
      !!                   ***  ROUTINE lim_istate_init  ***
      !!        
      !! ** Purpose : Definition of initial state of the ice 
      !!
      !! ** Method : Read the namiceini namelist and check the parameter 
      !!       values called at the first timestep (nit000)
      !!
      !! ** input : 
      !!        Namelist namiceini
      !!
      !! history :
      !!  8.5  ! 03-08 (C. Ethe) original code 
      !!  8.5  ! 07-11 (M. Vancoppenolle) rewritten initialization
      !!-----------------------------------------------------------------------------
      NAMELIST/namiceini/ ln_iceini, rn_thres_sst, rn_hts_ini_n, rn_hts_ini_s, rn_hti_ini_n, rn_hti_ini_s,  &
         &                                      rn_ati_ini_n, rn_ati_ini_s, rn_smi_ini_n, rn_smi_ini_s, rn_tmi_ini_n, rn_tmi_ini_s
      INTEGER :: ios                 ! Local integer output status for namelist read
      !!-----------------------------------------------------------------------------
      !
      REWIND( numnam_ice_ref )              ! Namelist namiceini in reference namelist : Ice initial state
      READ  ( numnam_ice_ref, namiceini, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namiceini in reference namelist', lwp )

      REWIND( numnam_ice_cfg )              ! Namelist namiceini in configuration namelist : Ice initial state
      READ  ( numnam_ice_cfg, namiceini, IOSTAT = ios, ERR = 902 )
902   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namiceini in configuration namelist', lwp )
      IF(lwm) WRITE ( numoni, namiceini )

      ! Define the initial parameters
      ! -------------------------

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'lim_istate_init : ice parameters inititialisation '
         WRITE(numout,*) '~~~~~~~~~~~~~~~'
         WRITE(numout,*) '   initialization with ice (T) or not (F)       ln_iceini     = ', ln_iceini
         WRITE(numout,*) '   threshold water temp. for initial sea-ice    rn_thres_sst  = ', rn_thres_sst
         WRITE(numout,*) '   initial snow thickness in the north          rn_hts_ini_n  = ', rn_hts_ini_n
         WRITE(numout,*) '   initial snow thickness in the south          rn_hts_ini_s  = ', rn_hts_ini_s 
         WRITE(numout,*) '   initial ice thickness  in the north          rn_hti_ini_n  = ', rn_hti_ini_n
         WRITE(numout,*) '   initial ice thickness  in the south          rn_hti_ini_s  = ', rn_hti_ini_s
         WRITE(numout,*) '   initial ice concentr.  in the north          rn_ati_ini_n  = ', rn_ati_ini_n
         WRITE(numout,*) '   initial ice concentr.  in the north          rn_ati_ini_s  = ', rn_ati_ini_s
         WRITE(numout,*) '   initial  ice salinity  in the north          rn_smi_ini_n  = ', rn_smi_ini_n
         WRITE(numout,*) '   initial  ice salinity  in the south          rn_smi_ini_s  = ', rn_smi_ini_s
         WRITE(numout,*) '   initial  ice/snw temp  in the north          rn_tmi_ini_n  = ', rn_tmi_ini_n
         WRITE(numout,*) '   initial  ice/snw temp  in the south          rn_tmi_ini_s  = ', rn_tmi_ini_s
      ENDIF

   END SUBROUTINE lim_istate_init

#else
   !!----------------------------------------------------------------------
   !!   Default option :         Empty module          NO LIM sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_istate          ! Empty routine
   END SUBROUTINE lim_istate
#endif

   !!======================================================================
END MODULE limistate
