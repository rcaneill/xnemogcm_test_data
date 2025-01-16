MODULE iceistate
   !!======================================================================
   !!                     ***  MODULE  iceistate  ***
   !!   sea-ice : Initialization of ice variables
   !!======================================================================
   !! History :  2.0  !  2004-01  (C. Ethe, G. Madec) Original code
   !!            3.0  !  2007     (M. Vancoppenolle)  Rewrite for ice cats
   !!            4.0  !  2018     (many people)       SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_istate       :  initialization of diagnostics ice variables
   !!   ice_istate_init  :  initialization of ice state and namelist read
   !!----------------------------------------------------------------------
   USE phycst         ! physical constant
   USE oce            ! dynamics and tracers variables
   USE dom_oce        ! ocean domain
   USE sbc_oce , ONLY : sst_m, sss_m, ln_ice_embd 
   USE sbc_ice , ONLY : tn_ice, snwice_mass, snwice_mass_b
   USE eosbn2         ! equation of state
   USE domvvl         ! Variable volume
   USE ice            ! sea-ice variables
   USE icevar         ! ice_var_salprof
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O manager library
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! fortran utilities (glob_sum + no signed zero)
   USE fldread        ! read input fields

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_istate        ! called by icestp.F90
   PUBLIC   ice_istate_init   ! called by icestp.F90

   INTEGER , PARAMETER ::   jpfldi = 6           ! maximum number of files to read
   INTEGER , PARAMETER ::   jp_hti = 1           ! index of ice thickness (m)    at T-point
   INTEGER , PARAMETER ::   jp_hts = 2           ! index of snow thicknes (m)    at T-point
   INTEGER , PARAMETER ::   jp_ati = 3           ! index of ice fraction (%) at T-point
   INTEGER , PARAMETER ::   jp_tsu = 4           ! index of ice surface temp (K)    at T-point
   INTEGER , PARAMETER ::   jp_tmi = 5           ! index of ice temp at T-point
   INTEGER , PARAMETER ::   jp_smi = 6           ! index of ice sali at T-point
   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   si  ! structure of input fields (file informations, fields read)
   !
   !                             !! ** namelist (namini) **
   LOGICAL  ::   ln_iceini        ! initialization or not
   LOGICAL  ::   ln_iceini_file   ! Ice initialization state from 2D netcdf file
   REAL(wp) ::   rn_thres_sst     ! threshold water temperature for initial sea ice
   REAL(wp) ::   rn_hts_ini_n     ! initial snow thickness in the north
   REAL(wp) ::   rn_hts_ini_s     ! initial snow thickness in the south
   REAL(wp) ::   rn_hti_ini_n     ! initial ice thickness in the north
   REAL(wp) ::   rn_hti_ini_s     ! initial ice thickness in the south
   REAL(wp) ::   rn_ati_ini_n     ! initial leads area in the north
   REAL(wp) ::   rn_ati_ini_s     ! initial leads area in the south
   REAL(wp) ::   rn_smi_ini_n     ! initial salinity 
   REAL(wp) ::   rn_smi_ini_s     ! initial salinity
   REAL(wp) ::   rn_tmi_ini_n     ! initial temperature
   REAL(wp) ::   rn_tmi_ini_s     ! initial temperature
   
   !!----------------------------------------------------------------------
   !! NEMO/ICE 4.0 , NEMO Consortium (2018)
   !! $Id: iceistate.F90 11228 2019-07-09 12:21:55Z clem $
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ice_istate
      !!-------------------------------------------------------------------
      !!                    ***  ROUTINE ice_istate  ***
      !!
      !! ** Purpose :   defined the sea-ice initial state
      !!
      !! ** Method  :   This routine will put some ice where ocean
      !!                is at the freezing point, then fill in ice 
      !!                state variables using prescribed initial 
      !!                values in the namelist            
      !!
      !! ** Steps   :   1) Set initial surface and basal temperatures
      !!                2) Recompute or read sea ice state variables
      !!                3) Fill in the ice thickness distribution using gaussian
      !!                4) Fill in space-dependent arrays for state variables
      !!                5) snow-ice mass computation
      !!                6) store before fields
      !!
      !! ** Notes   : o_i, t_su, t_s, t_i, sz_i must be filled everywhere, even
      !!              where there is no ice (clem: I do not know why, is it mandatory?) 
      !!--------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk, jl         ! dummy loop indices
      INTEGER  ::   i_hemis, i_fill, jl0   ! local integers
      REAL(wp) ::   ztmelts, zdh
      REAL(wp) ::   zarg, zV, zconv, zdv, zfac
      INTEGER , DIMENSION(4)           ::   itest
      REAL(wp), DIMENSION(jpi,jpj)     ::   z2d
      REAL(wp), DIMENSION(jpi,jpj)     ::   zswitch    ! ice indicator
      REAL(wp), DIMENSION(jpi,jpj)     ::   zht_i_ini, zat_i_ini, zvt_i_ini            !data from namelist or nc file
      REAL(wp), DIMENSION(jpi,jpj)     ::   zts_u_ini, zht_s_ini, zsm_i_ini, ztm_i_ini !data from namelist or nc file
      REAL(wp), DIMENSION(jpi,jpj,jpl) ::   zh_i_ini , za_i_ini                        !data by cattegories to fill
      !--------------------------------------------------------------------

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'ice_istate: sea-ice initialization '
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~'

      !--------------------------------------------------------------------
      ! 1) Set surface and bottom temperatures to initial values
      !--------------------------------------------------------------------
      !
      ! init surface temperature
      DO jl = 1, jpl
         t_su   (:,:,jl) = rt0 * tmask(:,:,1)  ! temp at the surface
         cnd_ice(:,:,jl) = 0._wp               ! initialisation of the effective conductivity at the top of ice/snow (ln_cndflx=T)
      END DO
      !
      ! init basal temperature (considered at freezing point)   [Kelvin]
      CALL eos_fzp( sss_m(:,:), t_bo(:,:) )
      t_bo(:,:) = ( t_bo(:,:) + rt0 ) * tmask(:,:,1) 

      IF( ln_iceini ) THEN
         !-----------------------------------------------------------
         ! 2) Compute or read sea ice variables ===> single category
         !-----------------------------------------------------------
         !
         !                             !---------------!
         IF( ln_iceini_file )THEN      ! Read a file   !
            !                          !---------------!
            !
            zht_i_ini(:,:)  = si(jp_hti)%fnow(:,:,1)
            zht_s_ini(:,:)  = si(jp_hts)%fnow(:,:,1)
            zat_i_ini(:,:)  = si(jp_ati)%fnow(:,:,1)
            zts_u_ini(:,:)  = si(jp_tsu)%fnow(:,:,1)
            ztm_i_ini(:,:)  = si(jp_tmi)%fnow(:,:,1)
            zsm_i_ini(:,:)  = si(jp_smi)%fnow(:,:,1)
            !
            WHERE( zat_i_ini(:,:) > 0._wp ) ; zswitch(:,:) = tmask(:,:,1) 
            ELSEWHERE                       ; zswitch(:,:) = 0._wp
            END WHERE
            zvt_i_ini(:,:) = zht_i_ini(:,:) * zat_i_ini(:,:)
            !
            !                          !---------------!
         ELSE                          ! Read namelist !
            !                          !---------------!
            ! no ice if sst <= t-freez + ttest
            WHERE( ( sst_m(:,:) - (t_bo(:,:) - rt0) ) * tmask(:,:,1) >= rn_thres_sst )   ;   zswitch(:,:) = 0._wp 
            ELSEWHERE                                                                    ;   zswitch(:,:) = tmask(:,:,1)
            END WHERE
            !
            ! assign initial thickness, concentration, snow depth and salinity to an hemisphere-dependent array
            WHERE( ff_t(:,:) >= 0._wp )
               zht_i_ini(:,:) = rn_hti_ini_n * zswitch(:,:)
               zht_s_ini(:,:) = rn_hts_ini_n * zswitch(:,:)
               zat_i_ini(:,:) = rn_ati_ini_n * zswitch(:,:)
               zts_u_ini(:,:) = rn_tmi_ini_n * zswitch(:,:)
               zsm_i_ini(:,:) = rn_smi_ini_n * zswitch(:,:)
               ztm_i_ini(:,:) = rn_tmi_ini_n * zswitch(:,:)
            ELSEWHERE
               zht_i_ini(:,:) = rn_hti_ini_s * zswitch(:,:)
               zht_s_ini(:,:) = rn_hts_ini_s * zswitch(:,:)
               zat_i_ini(:,:) = rn_ati_ini_s * zswitch(:,:)
               zts_u_ini(:,:) = rn_tmi_ini_s * zswitch(:,:)
               zsm_i_ini(:,:) = rn_smi_ini_s * zswitch(:,:)
               ztm_i_ini(:,:) = rn_tmi_ini_s * zswitch(:,:)
            END WHERE
            zvt_i_ini(:,:) = zht_i_ini(:,:) * zat_i_ini(:,:)
            !
         ENDIF
         
         !------------------------------------------------------------------
         ! 3) Distribute ice concentration and thickness into the categories
         !------------------------------------------------------------------
         ! a gaussian distribution for ice concentration is used
         ! then we check whether the distribution fullfills
         ! volume and area conservation, positivity and ice categories bounds

         IF( jpl == 1 ) THEN
            !
            zh_i_ini(:,:,1) = zht_i_ini(:,:)
            za_i_ini(:,:,1) = zat_i_ini(:,:)            
            !
         ELSE
            zh_i_ini(:,:,:) = 0._wp 
            za_i_ini(:,:,:) = 0._wp
            !
            DO jj = 1, jpj
               DO ji = 1, jpi
                  !
                  IF( zat_i_ini(ji,jj) > 0._wp .AND. zht_i_ini(ji,jj) > 0._wp )THEN

                     ! find which category (jl0) the input ice thickness falls into
                     jl0 = jpl
                     DO jl = 1, jpl
                        IF ( ( zht_i_ini(ji,jj) >  hi_max(jl-1) ) .AND. ( zht_i_ini(ji,jj) <= hi_max(jl) ) ) THEN
                           jl0 = jl
                           CYCLE
                        ENDIF
                     END DO
                     !
                     itest(:) = 0
                     i_fill   = jpl + 1                                            !------------------------------------
                     DO WHILE ( ( SUM( itest(:) ) /= 4 ) .AND. ( i_fill >= 2 ) )   ! iterative loop on i_fill categories
                        !                                                          !------------------------------------
                        i_fill = i_fill - 1
                        !
                        zh_i_ini(ji,jj,:) = 0._wp 
                        za_i_ini(ji,jj,:) = 0._wp
                        itest(:) = 0
                        !
                        IF ( i_fill == 1 ) THEN      !-- case very thin ice: fill only category 1
                           zh_i_ini(ji,jj,1) = zht_i_ini(ji,jj)
                           za_i_ini(ji,jj,1) = zat_i_ini(ji,jj)
                        ELSE                         !-- case ice is thicker: fill categories >1
                           ! thickness
                           DO jl = 1, i_fill-1
                              zh_i_ini(ji,jj,jl) = hi_mean(jl)
                           END DO
                           !
                           ! concentration
                           za_i_ini(ji,jj,jl0) = zat_i_ini(ji,jj) / SQRT(REAL(jpl))
                           DO jl = 1, i_fill - 1
                              IF( jl /= jl0 )THEN
                                 zarg               = ( zh_i_ini(ji,jj,jl) - zht_i_ini(ji,jj) ) / ( 0.5_wp * zht_i_ini(ji,jj) )
                                 za_i_ini(ji,jj,jl) = za_i_ini(ji,jj,jl0) * EXP(-zarg**2)
                              ENDIF
                           END DO

                           ! last category
                           za_i_ini(ji,jj,i_fill) = zat_i_ini(ji,jj) - SUM( za_i_ini(ji,jj,1:i_fill-1) )
                           zV = SUM( za_i_ini(ji,jj,1:i_fill-1) * zh_i_ini(ji,jj,1:i_fill-1) )
                           zh_i_ini(ji,jj,i_fill) = ( zvt_i_ini(ji,jj) - zV ) / MAX( za_i_ini(ji,jj,i_fill), epsi10 ) 

                           ! correction if concentration of upper cat is greater than lower cat
                           !   (it should be a gaussian around jl0 but sometimes it is not)
                           IF ( jl0 /= jpl ) THEN
                              DO jl = jpl, jl0+1, -1
                                 IF ( za_i_ini(ji,jj,jl) > za_i_ini(ji,jj,jl-1) ) THEN
                                    zdv = zh_i_ini(ji,jj,jl) * za_i_ini(ji,jj,jl)
                                    zh_i_ini(ji,jj,jl    ) = 0._wp
                                    za_i_ini(ji,jj,jl    ) = 0._wp
                                    za_i_ini(ji,jj,1:jl-1) = za_i_ini(ji,jj,1:jl-1)  &
                                       &                     + zdv / MAX( REAL(jl-1) * zht_i_ini(ji,jj), epsi10 )
                                 END IF
                              ENDDO
                           ENDIF
                           !
                        ENDIF
                        !
                        ! Compatibility tests
                        zconv = ABS( zat_i_ini(ji,jj) - SUM( za_i_ini(ji,jj,1:jpl) ) )           ! Test 1: area conservation
                        IF ( zconv < epsi06 ) itest(1) = 1
                        !
                        zconv = ABS(       zat_i_ini(ji,jj)       * zht_i_ini(ji,jj)   &         ! Test 2: volume conservation
                           &        - SUM( za_i_ini (ji,jj,1:jpl) * zh_i_ini (ji,jj,1:jpl) ) )
                        IF ( zconv < epsi06 ) itest(2) = 1
                        !
                        IF ( zh_i_ini(ji,jj,i_fill) >= hi_max(i_fill-1) ) itest(3) = 1           ! Test 3: thickness of the last category is in-bounds ?
                        !
                        itest(4) = 1
                        DO jl = 1, i_fill
                           IF ( za_i_ini(ji,jj,jl) < 0._wp ) itest(4) = 0                        ! Test 4: positivity of ice concentrations
                        END DO
                        !                                                          !----------------------------
                     END DO                                                        ! end iteration on categories
                     !                                                             !----------------------------
                     IF( lwp .AND. SUM(itest) /= 4 ) THEN 
                        WRITE(numout,*)
                        WRITE(numout,*) ' !!!! ALERT itest is not equal to 4      !!! '
                        WRITE(numout,*) ' !!!! Something is wrong in the SI3 initialization procedure '
                        WRITE(numout,*)
                        WRITE(numout,*) ' *** itest_i (i=1,4) = ', itest(:)
                        WRITE(numout,*) ' zat_i_ini : ', zat_i_ini(ji,jj)
                        WRITE(numout,*) ' zht_i_ini : ', zht_i_ini(ji,jj)
                     ENDIF
                     !
                  ENDIF
                  !
               END DO
            END DO
         ENDIF
         
         !---------------------------------------------------------------------
         ! 4) Fill in sea ice arrays
         !---------------------------------------------------------------------
         !
         ! Ice concentration, thickness and volume, ice salinity, ice age, surface temperature
         DO jl = 1, jpl ! loop over categories
            DO jj = 1, jpj
               DO ji = 1, jpi
                  a_i(ji,jj,jl)  = zswitch(ji,jj) * za_i_ini(ji,jj,jl)                       ! concentration
                  h_i(ji,jj,jl)  = zswitch(ji,jj) * zh_i_ini(ji,jj,jl)                       ! ice thickness
                  s_i(ji,jj,jl)  = zswitch(ji,jj) * zsm_i_ini(ji,jj)                         ! salinity
                  o_i(ji,jj,jl)  = 0._wp                                                     ! age (0 day)
                  t_su(ji,jj,jl) = zswitch(ji,jj) * zts_u_ini(ji,jj) + ( 1._wp - zswitch(ji,jj) ) * rt0 ! surf temp
                  !
                  IF( zht_i_ini(ji,jj) > 0._wp )THEN
                    h_s(ji,jj,jl)= h_i(ji,jj,jl) * ( zht_s_ini(ji,jj) / zht_i_ini(ji,jj) )  ! snow depth
                  ELSE
                    h_s(ji,jj,jl)= 0._wp
                  ENDIF
                  !
                  ! This case below should not be used if (h_s/h_i) is ok in namelist
                  ! In case snow load is in excess that would lead to transformation from snow to ice
                  ! Then, transfer the snow excess into the ice (different from icethd_dh)
                  zdh = MAX( 0._wp, ( rhos * h_s(ji,jj,jl) + ( rhoi - rau0 ) * h_i(ji,jj,jl) ) * r1_rau0 ) 
                  ! recompute h_i, h_s avoiding out of bounds values
                  h_i(ji,jj,jl) = MIN( hi_max(jl), h_i(ji,jj,jl) + zdh )
                  h_s(ji,jj,jl) = MAX( 0._wp, h_s(ji,jj,jl) - zdh * rhoi * r1_rhos )
                  !
                  ! ice volume, salt content, age content
                  v_i (ji,jj,jl) = h_i(ji,jj,jl) * a_i(ji,jj,jl)              ! ice volume
                  v_s (ji,jj,jl) = h_s(ji,jj,jl) * a_i(ji,jj,jl)              ! snow volume
                  sv_i(ji,jj,jl) = MIN( s_i(ji,jj,jl) , sss_m(ji,jj) ) * v_i(ji,jj,jl) ! salt content
                  oa_i(ji,jj,jl) = o_i(ji,jj,jl) * a_i(ji,jj,jl)               ! age content
               END DO
            END DO
         END DO
         !
         IF( nn_icesal /= 2 )  THEN         ! for constant salinity in time
            CALL ice_var_salprof
            sv_i = s_i * v_i
         ENDIF
         !  
         ! Snow temperature and heat content
         DO jk = 1, nlay_s
            DO jl = 1, jpl ! loop over categories
               DO jj = 1, jpj
                  DO ji = 1, jpi
                     t_s(ji,jj,jk,jl) = zswitch(ji,jj) * ztm_i_ini(ji,jj) + ( 1._wp - zswitch(ji,jj) ) * rt0
                     ! Snow energy of melting
                     e_s(ji,jj,jk,jl) = zswitch(ji,jj) * rhos * ( rcpi * ( rt0 - t_s(ji,jj,jk,jl) ) + rLfus )
                     !
                     ! Mutliply by volume, and divide by number of layers to get heat content in J/m2
                     e_s(ji,jj,jk,jl) = e_s(ji,jj,jk,jl) * v_s(ji,jj,jl) * r1_nlay_s
                  END DO
               END DO
            END DO
         END DO
         !
         ! Ice salinity, temperature and heat content
         DO jk = 1, nlay_i
            DO jl = 1, jpl ! loop over categories
               DO jj = 1, jpj
                  DO ji = 1, jpi
                     t_i (ji,jj,jk,jl) = zswitch(ji,jj) * ztm_i_ini(ji,jj) + ( 1._wp - zswitch(ji,jj) ) * rt0 
                     sz_i(ji,jj,jk,jl) = zswitch(ji,jj) * zsm_i_ini(ji,jj) + ( 1._wp - zswitch(ji,jj) ) * rn_simin
                     ztmelts          = - rTmlt * sz_i(ji,jj,jk,jl) + rt0 !Melting temperature in K
                     !
                     ! heat content per unit volume
                     e_i(ji,jj,jk,jl) = zswitch(ji,jj) * rhoi * (   rcpi    * ( ztmelts - t_i(ji,jj,jk,jl) )           &
                        &             + rLfus * ( 1._wp - (ztmelts-rt0) / MIN( (t_i(ji,jj,jk,jl)-rt0) , -epsi20 )  )   &
                        &             - rcp  * ( ztmelts - rt0 ) )
                     !
                     ! Mutliply by ice volume, and divide by number of layers to get heat content in J/m2
                     e_i(ji,jj,jk,jl) = e_i(ji,jj,jk,jl) * v_i(ji,jj,jl) * r1_nlay_i
                  END DO
               END DO
            END DO
         END DO
         !
         tn_ice (:,:,:) = t_su (:,:,:)
         t1_ice (:,:,:) = t_i (:,:,1,:)   ! initialisation of 1st layer temp for coupled simu

         ! Melt pond volume and fraction
         IF ( ln_pnd_CST .OR. ln_pnd_H12 ) THEN   ;   zfac = 1._wp
         ELSE                                     ;   zfac = 0._wp
         ENDIF 
         DO jl = 1, jpl
            a_ip_frac(:,:,jl) = rn_apnd * zswitch(:,:) * zfac
            h_ip     (:,:,jl) = rn_hpnd * zswitch(:,:) * zfac
         END DO
         a_ip(:,:,:) = a_ip_frac(:,:,:) * a_i (:,:,:) 
         v_ip(:,:,:) = h_ip     (:,:,:) * a_ip(:,:,:)
         !
      ELSE ! if ln_iceini=false
         a_i  (:,:,:) = 0._wp
         v_i  (:,:,:) = 0._wp
         v_s  (:,:,:) = 0._wp
         sv_i (:,:,:) = 0._wp
         oa_i (:,:,:) = 0._wp
         h_i  (:,:,:) = 0._wp
         h_s  (:,:,:) = 0._wp
         s_i  (:,:,:) = 0._wp
         o_i  (:,:,:) = 0._wp
         !
         e_i(:,:,:,:) = 0._wp
         e_s(:,:,:,:) = 0._wp
         !
         DO jl = 1, jpl
            DO jk = 1, nlay_i
               t_i(:,:,jk,jl) = rt0 * tmask(:,:,1)
            END DO
            DO jk = 1, nlay_s
               t_s(:,:,jk,jl) = rt0 * tmask(:,:,1)
            END DO
         END DO

         tn_ice (:,:,:) = t_i (:,:,1,:)
         t1_ice (:,:,:) = t_i (:,:,1,:)   ! initialisation of 1st layer temp for coupled simu
         
         a_ip(:,:,:)      = 0._wp
         v_ip(:,:,:)      = 0._wp
         a_ip_frac(:,:,:) = 0._wp
         h_ip     (:,:,:) = 0._wp
         !
      ENDIF ! ln_iceini
      !
      at_i (:,:) = 0.0_wp
      DO jl = 1, jpl
         at_i (:,:) = at_i (:,:) + a_i (:,:,jl)
      END DO
      !
      ! --- set ice velocities --- !
      u_ice (:,:) = 0._wp
      v_ice (:,:) = 0._wp
      ! fields needed for ice_dyn_adv_umx
      l_split_advumx(1) = .FALSE.
      !
      !----------------------------------------------
      ! 5) Snow-ice mass (case ice is fully embedded)
      !----------------------------------------------
      snwice_mass  (:,:) = tmask(:,:,1) * SUM( rhos * v_s(:,:,:) + rhoi * v_i(:,:,:), dim=3  )   ! snow+ice mass
      snwice_mass_b(:,:) = snwice_mass(:,:)
      !
      IF( ln_ice_embd ) THEN            ! embedded sea-ice: deplete the initial ssh below sea-ice area
         !
         sshn(:,:) = sshn(:,:) - snwice_mass(:,:) * r1_rau0
         sshb(:,:) = sshb(:,:) - snwice_mass(:,:) * r1_rau0
         !
         IF( .NOT.ln_linssh ) THEN
            !
            WHERE( ht_0(:,:) > 0 )   ;   z2d(:,:) = 1._wp + sshn(:,:)*tmask(:,:,1) / ht_0(:,:)
            ELSEWHERE                ;   z2d(:,:) = 1._wp   ;   END WHERE
            !
            DO jk = 1,jpkm1                     ! adjust initial vertical scale factors                
               e3t_n(:,:,jk) = e3t_0(:,:,jk) * z2d(:,:)
               e3t_b(:,:,jk) = e3t_n(:,:,jk)
               e3t_a(:,:,jk) = e3t_n(:,:,jk)
            END DO
            !
            ! Reconstruction of all vertical scale factors at now and before time-steps
            ! =========================================================================
            ! Horizontal scale factor interpolations
            ! --------------------------------------
            CALL dom_vvl_interpol( e3t_b(:,:,:), e3u_b(:,:,:), 'U' )
            CALL dom_vvl_interpol( e3t_b(:,:,:), e3v_b(:,:,:), 'V' )
            CALL dom_vvl_interpol( e3t_n(:,:,:), e3u_n(:,:,:), 'U' )
            CALL dom_vvl_interpol( e3t_n(:,:,:), e3v_n(:,:,:), 'V' )
            CALL dom_vvl_interpol( e3u_n(:,:,:), e3f_n(:,:,:), 'F' )
            ! Vertical scale factor interpolations
            ! ------------------------------------
            CALL dom_vvl_interpol( e3t_n(:,:,:), e3w_n (:,:,:), 'W'  )
            CALL dom_vvl_interpol( e3u_n(:,:,:), e3uw_n(:,:,:), 'UW' )
            CALL dom_vvl_interpol( e3v_n(:,:,:), e3vw_n(:,:,:), 'VW' )
            CALL dom_vvl_interpol( e3u_b(:,:,:), e3uw_b(:,:,:), 'UW' )
            CALL dom_vvl_interpol( e3v_b(:,:,:), e3vw_b(:,:,:), 'VW' )
            ! t- and w- points depth
            ! ----------------------
            !!gm not sure of that....
            gdept_n(:,:,1) = 0.5_wp * e3w_n(:,:,1)
            gdepw_n(:,:,1) = 0.0_wp
            gde3w_n(:,:,1) = gdept_n(:,:,1) - sshn(:,:)
            DO jk = 2, jpk
               gdept_n(:,:,jk) = gdept_n(:,:,jk-1) + e3w_n(:,:,jk  )
               gdepw_n(:,:,jk) = gdepw_n(:,:,jk-1) + e3t_n(:,:,jk-1)
               gde3w_n(:,:,jk) = gdept_n(:,:,jk  ) - sshn (:,:)
            END DO
         ENDIF
      ENDIF
      
      !------------------------------------
      ! 6) store fields at before time-step
      !------------------------------------
      ! it is only necessary for the 1st interpolation by Agrif
      a_i_b  (:,:,:)   = a_i  (:,:,:)
      e_i_b  (:,:,:,:) = e_i  (:,:,:,:)
      v_i_b  (:,:,:)   = v_i  (:,:,:)
      v_s_b  (:,:,:)   = v_s  (:,:,:)
      e_s_b  (:,:,:,:) = e_s  (:,:,:,:)
      sv_i_b (:,:,:)   = sv_i (:,:,:)
      oa_i_b (:,:,:)   = oa_i (:,:,:)
      u_ice_b(:,:)     = u_ice(:,:)
      v_ice_b(:,:)     = v_ice(:,:)
      ! total concentration is needed for Lupkes parameterizations
      at_i_b (:,:)     = at_i (:,:) 

!!clem: output of initial state should be written here but it is impossible because
!!      the ocean and ice are in the same file
!!      CALL dia_wri_state( 'output.init' )
      !
   END SUBROUTINE ice_istate


   SUBROUTINE ice_istate_init
      !!-------------------------------------------------------------------
      !!                   ***  ROUTINE ice_istate_init  ***
      !!        
      !! ** Purpose :   Definition of initial state of the ice 
      !!
      !! ** Method  :   Read the namini namelist and check the parameter 
      !!              values called at the first timestep (nit000)
      !!
      !! ** input   :  Namelist namini
      !!
      !!-----------------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer output status for namelist read
      INTEGER ::   ifpr, ierror
      !
      CHARACTER(len=256) ::  cn_dir          ! Root directory for location of ice files
      TYPE(FLD_N)                    ::   sn_hti, sn_hts, sn_ati, sn_tsu, sn_tmi, sn_smi
      TYPE(FLD_N), DIMENSION(jpfldi) ::   slf_i                 ! array of namelist informations on the fields to read
      !
      NAMELIST/namini/ ln_iceini, ln_iceini_file, rn_thres_sst, rn_hts_ini_n, rn_hts_ini_s,  &
         &             rn_hti_ini_n, rn_hti_ini_s, rn_ati_ini_n, rn_ati_ini_s, rn_smi_ini_n, &
         &             rn_smi_ini_s, rn_tmi_ini_n, rn_tmi_ini_s,                             &
         &             sn_hti, sn_hts, sn_ati, sn_tsu, sn_tmi, sn_smi, cn_dir
      !!-----------------------------------------------------------------------------
      !
      REWIND( numnam_ice_ref )              ! Namelist namini in reference namelist : Ice initial state
      READ  ( numnam_ice_ref, namini, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namini in reference namelist', lwp )
      REWIND( numnam_ice_cfg )              ! Namelist namini in configuration namelist : Ice initial state
      READ  ( numnam_ice_cfg, namini, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namini in configuration namelist', lwp )
      IF(lwm) WRITE ( numoni, namini )
      !
      slf_i(jp_hti) = sn_hti  ;  slf_i(jp_hts) = sn_hts
      slf_i(jp_ati) = sn_ati  ;  slf_i(jp_tsu) = sn_tsu
      slf_i(jp_tmi) = sn_tmi  ;  slf_i(jp_smi) = sn_smi
      !
      IF(lwp) THEN                          ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'ice_istate_init: ice parameters inititialisation '
         WRITE(numout,*) '~~~~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namini:'
         WRITE(numout,*) '      initialization with ice (T) or not (F)                 ln_iceini       = ', ln_iceini
         WRITE(numout,*) '      ice initialization from a netcdf file                  ln_iceini_file  = ', ln_iceini_file
         WRITE(numout,*) '      max delta ocean temp. above Tfreeze with initial ice   rn_thres_sst    = ', rn_thres_sst
         WRITE(numout,*) '      initial snow thickness in the north                    rn_hts_ini_n    = ', rn_hts_ini_n
         WRITE(numout,*) '      initial snow thickness in the south                    rn_hts_ini_s    = ', rn_hts_ini_s 
         WRITE(numout,*) '      initial ice thickness  in the north                    rn_hti_ini_n    = ', rn_hti_ini_n
         WRITE(numout,*) '      initial ice thickness  in the south                    rn_hti_ini_s    = ', rn_hti_ini_s
         WRITE(numout,*) '      initial ice concentr.  in the north                    rn_ati_ini_n    = ', rn_ati_ini_n
         WRITE(numout,*) '      initial ice concentr.  in the north                    rn_ati_ini_s    = ', rn_ati_ini_s
         WRITE(numout,*) '      initial  ice salinity  in the north                    rn_smi_ini_n    = ', rn_smi_ini_n
         WRITE(numout,*) '      initial  ice salinity  in the south                    rn_smi_ini_s    = ', rn_smi_ini_s
         WRITE(numout,*) '      initial  ice/snw temp  in the north                    rn_tmi_ini_n    = ', rn_tmi_ini_n
         WRITE(numout,*) '      initial  ice/snw temp  in the south                    rn_tmi_ini_s    = ', rn_tmi_ini_s
      ENDIF
      !
      IF( ln_iceini_file ) THEN                      ! Ice initialization using input file
         !
         ! set si structure
         ALLOCATE( si(jpfldi), STAT=ierror )
         IF( ierror > 0 ) THEN
            CALL ctl_stop( 'Ice_ini in iceistate: unable to allocate si structure' )   ;   RETURN
         ENDIF
         !
         DO ifpr = 1, jpfldi
            ALLOCATE( si(ifpr)%fnow(jpi,jpj,1) )
            ALLOCATE( si(ifpr)%fdta(jpi,jpj,1,2) )
         END DO
         !
         ! fill si with slf_i and control print
         CALL fld_fill( si, slf_i, cn_dir, 'ice_istate', 'ice istate ini', 'numnam_ice' )
         !
         CALL fld_read( nit000, 1, si )                ! input fields provided at the current time-step
         !
      ENDIF
      !
   END SUBROUTINE ice_istate_init

#else
   !!----------------------------------------------------------------------
   !!   Default option :         Empty module         NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE iceistate
