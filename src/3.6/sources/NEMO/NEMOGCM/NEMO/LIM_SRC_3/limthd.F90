MODULE limthd
   !!======================================================================
   !!                  ***  MODULE limthd   ***
   !!  LIM-3 :   ice thermodynamic
   !!======================================================================
   !! History :  LIM  ! 2000-01 (M.A. Morales Maqueda, H. Goosse, T. Fichefet) LIM-1
   !!            2.0  ! 2002-07 (C. Ethe, G. Madec)  LIM-2 (F90 rewriting)
   !!            3.0  ! 2005-11 (M. Vancoppenolle)  LIM-3 : Multi-layer thermodynamics + salinity variations
   !!             -   ! 2007-04 (M. Vancoppenolle) add lim_thd_glohec, lim_thd_con_dh and lim_thd_con_dif
   !!            3.2  ! 2009-07 (M. Vancoppenolle, Y. Aksenov, G. Madec) bug correction in wfx_snw
   !!            3.3  ! 2010-11 (G. Madec) corrected snow melting heat (due to factor betas)
   !!            4.0  ! 2011-02 (G. Madec) dynamical allocation
   !!             -   ! 2012-05 (C. Rousset) add penetration solar flux
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3'                                      LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_thd       : thermodynamic of sea ice
   !!   lim_thd_init  : initialisation of sea-ice thermodynamic
   !!----------------------------------------------------------------------
   USE phycst         ! physical constants
   USE dom_oce        ! ocean space and time domain variables
   USE ice            ! LIM: sea-ice variables
   USE sbc_oce        ! Surface boundary condition: ocean fields
   USE sbc_ice        ! Surface boundary condition: ice fields
   USE thd_ice        ! LIM thermodynamic sea-ice variables
   USE dom_ice        ! LIM sea-ice domain
   USE limthd_dif     ! LIM: thermodynamics, vertical diffusion
   USE limthd_dh      ! LIM: thermodynamics, ice and snow thickness variation
   USE limthd_sal     ! LIM: thermodynamics, ice salinity
   USE limthd_ent     ! LIM: thermodynamics, ice enthalpy redistribution
   USE limthd_lac     ! LIM-3 lateral accretion
   USE limitd_th      ! remapping thickness distribution
   USE limtab         ! LIM: 1D <==> 2D transformation
   USE limvar         ! LIM: sea-ice variables
   USE lbclnk         ! lateral boundary condition - MPP links
   USE lib_mpp        ! MPP library
   USE wrk_nemo       ! work arrays
   USE in_out_manager ! I/O manager
   USE prtctl         ! Print control
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)  
   USE timing         ! Timing
   USE limcons        ! conservation tests
   USE limctl

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_thd         ! called by limstp module
   PUBLIC   lim_thd_init    ! called by sbc_lim_init

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/LIM3 3.3 , UCL - NEMO Consortium (2010)
   !! $Id: limthd.F90 7607 2017-01-25 15:37:31Z cetlod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_thd( kt )
      !!-------------------------------------------------------------------
      !!                ***  ROUTINE lim_thd  ***       
      !!  
      !! ** Purpose : This routine manages ice thermodynamics
      !!         
      !! ** Action : - Initialisation of some variables
      !!             - Some preliminary computation (oceanic heat flux
      !!               at the ice base, snow acc.,heat budget of the leads)
      !!             - selection of the icy points and put them in an array
      !!             - call lim_thd_dif  for vertical heat diffusion
      !!             - call lim_thd_dh   for vertical ice growth and melt
      !!             - call lim_thd_ent  for enthalpy remapping
      !!             - call lim_thd_sal  for ice desalination
      !!             - call lim_thd_temp to  retrieve temperature from ice enthalpy
      !!             - back to the geographic grid
      !!     
      !! ** References : 
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) :: kt    ! number of iteration
      !!
      INTEGER  :: ji, jj, jk, jl   ! dummy loop indices
      INTEGER  :: nbpb             ! nb of icy pts for vertical thermo calculations
      INTEGER  :: ii, ij           ! temporary dummy loop index
      REAL(wp) :: zfric_u, zqld, zqfr
      REAL(wp) :: zvi_b, zsmv_b, zei_b, zfs_b, zfw_b, zft_b 
      REAL(wp), PARAMETER :: zfric_umin = 0._wp           ! lower bound for the friction velocity (cice value=5.e-04)
      REAL(wp), PARAMETER :: zch        = 0.0057_wp       ! heat transfer coefficient
      !
      !!-------------------------------------------------------------------

      IF( nn_timing == 1 )  CALL timing_start('limthd')

      ! conservation test
      IF( ln_limdiahsb ) CALL lim_cons_hsm(0, 'limthd', zvi_b, zsmv_b, zei_b, zfw_b, zfs_b, zft_b)

      CALL lim_var_glo2eqv
      !------------------------------------------------------------------------!
      ! 1) Initialization of some variables                                    !
      !------------------------------------------------------------------------!
      ftr_ice(:,:,:) = 0._wp  ! part of solar radiation transmitted through the ice

      !--------------------
      ! 1.2) Heat content    
      !--------------------
      ! Change the units of heat content; from J/m2 to J/m3
      DO jl = 1, jpl
         DO jk = 1, nlay_i
            DO jj = 1, jpj
               DO ji = 1, jpi
                  !0 if no ice and 1 if yes
                  rswitch = MAX(  0._wp , SIGN( 1._wp , v_i(ji,jj,jl) - epsi20 )  )
                  !Energy of melting q(S,T) [J.m-3]
                  e_i(ji,jj,jk,jl) = rswitch * e_i(ji,jj,jk,jl) / MAX( v_i(ji,jj,jl) , epsi20 ) * REAL( nlay_i )
               END DO
            END DO
         END DO
         DO jk = 1, nlay_s
            DO jj = 1, jpj
               DO ji = 1, jpi
                  !0 if no ice and 1 if yes
                  rswitch = MAX(  0._wp , SIGN( 1._wp , v_s(ji,jj,jl) - epsi20 )  )
                  !Energy of melting q(S,T) [J.m-3]
                  e_s(ji,jj,jk,jl) = rswitch * e_s(ji,jj,jk,jl) / MAX( v_s(ji,jj,jl) , epsi20 ) * REAL( nlay_s )
               END DO
            END DO
         END DO
      END DO

      ! 2) Partial computation of forcing for the thermodynamic sea ice model.      !
      !-----------------------------------------------------------------------------!
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
            ! --- Energy received in the lead, zqld is defined everywhere (J.m-2) --- !
            zqld =  tmask(ji,jj,1) * rdt_ice *  &
               &    ( pfrld(ji,jj) * qsr_oce(ji,jj) * frq_m(ji,jj) + pfrld(ji,jj) * qns_oce(ji,jj) + qemp_oce(ji,jj) )

            ! --- Energy needed to bring ocean surface layer until its freezing (<0, J.m-2) --- !
            zqfr = tmask(ji,jj,1) * rau0 * rcp * fse3t_m(ji,jj) * ( t_bo(ji,jj) - ( sst_m(ji,jj) + rt0 ) )

            ! --- Energy from the turbulent oceanic heat flux (W/m2) --- !
            zfric_u      = MAX( SQRT( ust2s(ji,jj) ), zfric_umin ) 
            fhtur(ji,jj) = MAX( 0._wp, rswitch * rau0 * rcp * zch  * zfric_u * ( ( sst_m(ji,jj) + rt0 ) - t_bo(ji,jj) ) ) ! W.m-2
            fhtur(ji,jj) = rswitch * MIN( fhtur(ji,jj), - zqfr * r1_rdtice / MAX( at_i(ji,jj), epsi10 ) )
            ! upper bound for fhtur: the heat retrieved from the ocean must be smaller than the heat necessary to reach 
            !                        the freezing point, so that we do not have SST < T_freeze
            !                        This implies: - ( fhtur(ji,jj) * at_i(ji,jj) * rtdice ) - zqfr >= 0

            !-- Energy Budget of the leads (J.m-2). Must be < 0 to form ice
            qlead(ji,jj) = MIN( 0._wp , zqld - ( fhtur(ji,jj) * at_i(ji,jj) * rdt_ice ) - zqfr )

            ! If there is ice and leads are warming, then transfer energy from the lead budget and use it for bottom melting 
            IF( zqld > 0._wp ) THEN
               fhld (ji,jj) = rswitch * zqld * r1_rdtice / MAX( at_i(ji,jj), epsi10 ) ! divided by at_i since this is (re)multiplied by a_i in limthd_dh.F90
               qlead(ji,jj) = 0._wp
            ELSE
               fhld (ji,jj) = 0._wp
            ENDIF
            !
            ! -----------------------------------------
            ! Net heat flux on top of ice-ocean [W.m-2]
            ! -----------------------------------------
            hfx_in(ji,jj) = qns_tot(ji,jj) + qsr_tot(ji,jj) 

            ! -----------------------------------------------------------------------------
            ! Net heat flux on top of the ocean after ice thermo (1st step) [W.m-2]
            ! -----------------------------------------------------------------------------
            !     First  step here              :  non solar + precip - qlead - qturb
            !     Second step in limthd_dh      :  heat remaining if total melt (zq_rema) 
            !     Third  step in limsbc         :  heat from ice-ocean mass exchange (zf_mass) + solar
            hfx_out(ji,jj) =   pfrld(ji,jj) * qns_oce(ji,jj) + qemp_oce(ji,jj)  &  ! Non solar heat flux received by the ocean               
               &             - qlead(ji,jj) * r1_rdtice                         &  ! heat flux taken from the ocean where there is open water ice formation
               &             - at_i(ji,jj) * fhtur(ji,jj)                       &  ! heat flux taken by turbulence
               &             - at_i(ji,jj) *  fhld(ji,jj)                          ! heat flux taken during bottom growth/melt 
                                                                                   !    (fhld should be 0 while bott growth)
         END DO
      END DO

      !------------------------------------------------------------------------------!
      ! 3) Select icy points and fulfill arrays for the vectorial grid.            
      !------------------------------------------------------------------------------!

      DO jl = 1, jpl      !loop over ice categories

         IF( kt == nit000 .AND. lwp ) THEN
            WRITE(numout,*) ' lim_thd : transfer to 1D vectors. Category no : ', jl 
            WRITE(numout,*) ' ~~~~~~~~'
         ENDIF

         nbpb = 0
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF ( a_i(ji,jj,jl) > epsi10 ) THEN     
                  nbpb      = nbpb  + 1
                  npb(nbpb) = (jj - 1) * jpi + ji
               ENDIF
            END DO
         END DO

         ! debug point to follow
         jiindex_1d = 0
         IF( ln_icectl ) THEN
            DO ji = mi0(iiceprt), mi1(iiceprt)
               DO jj = mj0(jiceprt), mj1(jiceprt)
                  jiindex_1d = (jj - 1) * jpi + ji
                  WRITE(numout,*) ' lim_thd : Category no : ', jl 
               END DO
            END DO
         ENDIF

         !------------------------------------------------------------------------------!
         ! 4) Thermodynamic computation
         !------------------------------------------------------------------------------!

         IF( lk_mpp )   CALL mpp_ini_ice( nbpb , numout )

         IF( nbpb > 0 ) THEN  ! If there is no ice, do nothing.

            !-------------------------!
            ! --- Move to 1D arrays ---
            !-------------------------!
            CALL lim_thd_1d2d( nbpb, jl, 1 )

            !--------------------------------------!
            ! --- Ice/Snow Temperature profile --- !
            !--------------------------------------!
            CALL lim_thd_dif( 1, nbpb )

            !---------------------------------!
            ! --- Ice/Snow thickness ---      !
            !---------------------------------!
            CALL lim_thd_dh( 1, nbpb )    

            ! --- Ice enthalpy remapping --- !
            CALL lim_thd_ent( 1, nbpb, q_i_1d(1:nbpb,:) ) 
                                            
            !---------------------------------!
            ! --- Ice salinity ---            !
            !---------------------------------!
            CALL lim_thd_sal( 1, nbpb )    

            !---------------------------------!
            ! --- temperature update ---      !
            !---------------------------------!
            CALL lim_thd_temp( 1, nbpb )

            !------------------------------------!
            ! --- lateral melting if monocat --- !
            !------------------------------------!
            IF ( ( nn_monocat == 1 .OR. nn_monocat == 4 ) .AND. jpl == 1 ) THEN
               CALL lim_thd_lam( 1, nbpb )
            END IF

            !-------------------------!
            ! --- Move to 2D arrays ---
            !-------------------------!
            CALL lim_thd_1d2d( nbpb, jl, 2 )

            !
            IF( lk_mpp )   CALL mpp_comm_free( ncomm_ice ) !RB necessary ??
         ENDIF
         !
      END DO !jl

      !------------------------------------------------------------------------------!
      ! 5) Global variables, diagnostics
      !------------------------------------------------------------------------------!

      !------------------------
      ! Ice heat content              
      !------------------------
      ! Enthalpies are global variables we have to readjust the units (heat content in J/m2)
      DO jl = 1, jpl
         DO jk = 1, nlay_i
            e_i(:,:,jk,jl) = e_i(:,:,jk,jl) * a_i(:,:,jl) * ht_i(:,:,jl) * r1_nlay_i
         END DO
      END DO

      !------------------------
      ! Snow heat content              
      !------------------------
      ! Enthalpies are global variables we have to readjust the units (heat content in J/m2)
      DO jl = 1, jpl
         DO jk = 1, nlay_s
            e_s(:,:,jk,jl) = e_s(:,:,jk,jl) * a_i(:,:,jl) * ht_s(:,:,jl) * r1_nlay_s
         END DO
      END DO
 
      !----------------------------------
      ! Change thickness to volume
      !----------------------------------
      v_i(:,:,:)   = ht_i(:,:,:) * a_i(:,:,:)
      v_s(:,:,:)   = ht_s(:,:,:) * a_i(:,:,:)
      smv_i(:,:,:) = sm_i(:,:,:) * v_i(:,:,:)

      ! update ice age (in case a_i changed, i.e. becomes 0 or lateral melting in monocat)
      DO jl  = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               rswitch = MAX( 0._wp , SIGN( 1._wp, a_i_b(ji,jj,jl) - epsi10 ) )
               oa_i(ji,jj,jl) = rswitch * oa_i(ji,jj,jl) * a_i(ji,jj,jl) / MAX( a_i_b(ji,jj,jl), epsi10 )
            END DO
         END DO
      END DO

      CALL lim_var_zapsmall

      !--------------------------------------------
      ! Diagnostic thermodynamic growth rates
      !--------------------------------------------
      IF( ln_icectl )   CALL lim_prt( kt, iiceprt, jiceprt, 1, ' - ice thermodyn. - ' )   ! control print

      IF(ln_ctl) THEN            ! Control print
         CALL prt_ctl_info(' ')
         CALL prt_ctl_info(' - Cell values : ')
         CALL prt_ctl_info('   ~~~~~~~~~~~~~ ')
         CALL prt_ctl(tab2d_1=e12t , clinfo1=' lim_thd  : cell area :')
         CALL prt_ctl(tab2d_1=at_i , clinfo1=' lim_thd  : at_i      :')
         CALL prt_ctl(tab2d_1=vt_i , clinfo1=' lim_thd  : vt_i      :')
         CALL prt_ctl(tab2d_1=vt_s , clinfo1=' lim_thd  : vt_s      :')
         DO jl = 1, jpl
            CALL prt_ctl_info(' ')
            CALL prt_ctl_info(' - Category : ', ivar1=jl)
            CALL prt_ctl_info('   ~~~~~~~~~~')
            CALL prt_ctl(tab2d_1=a_i   (:,:,jl)   , clinfo1= ' lim_thd  : a_i      : ')
            CALL prt_ctl(tab2d_1=ht_i  (:,:,jl)   , clinfo1= ' lim_thd  : ht_i     : ')
            CALL prt_ctl(tab2d_1=ht_s  (:,:,jl)   , clinfo1= ' lim_thd  : ht_s     : ')
            CALL prt_ctl(tab2d_1=v_i   (:,:,jl)   , clinfo1= ' lim_thd  : v_i      : ')
            CALL prt_ctl(tab2d_1=v_s   (:,:,jl)   , clinfo1= ' lim_thd  : v_s      : ')
            CALL prt_ctl(tab2d_1=e_s   (:,:,1,jl) , clinfo1= ' lim_thd  : e_s      : ')
            CALL prt_ctl(tab2d_1=t_su  (:,:,jl)   , clinfo1= ' lim_thd  : t_su     : ')
            CALL prt_ctl(tab2d_1=t_s   (:,:,1,jl) , clinfo1= ' lim_thd  : t_snow   : ')
            CALL prt_ctl(tab2d_1=sm_i  (:,:,jl)   , clinfo1= ' lim_thd  : sm_i     : ')
            CALL prt_ctl(tab2d_1=smv_i (:,:,jl)   , clinfo1= ' lim_thd  : smv_i    : ')
            DO jk = 1, nlay_i
               CALL prt_ctl_info(' ')
               CALL prt_ctl_info(' - Layer : ', ivar1=jk)
               CALL prt_ctl_info('   ~~~~~~~')
               CALL prt_ctl(tab2d_1=t_i(:,:,jk,jl) , clinfo1= ' lim_thd  : t_i      : ')
               CALL prt_ctl(tab2d_1=e_i(:,:,jk,jl) , clinfo1= ' lim_thd  : e_i      : ')
            END DO
         END DO
      ENDIF
      !
      !
      IF( ln_limdiahsb ) CALL lim_cons_hsm(1, 'limthd', zvi_b, zsmv_b, zei_b, zfw_b, zfs_b, zft_b)

      !------------------------------------------------------------------------------|
      !  6) Transport of ice between thickness categories.                           |
      !------------------------------------------------------------------------------|
      ! Given thermodynamic growth rates, transport ice between thickness categories.
      IF( ln_limdiahsb ) CALL lim_cons_hsm(0, 'limitd_th_rem', zvi_b, zsmv_b, zei_b, zfw_b, zfs_b, zft_b)

      IF( jpl > 1 )      CALL lim_itd_th_rem( 1, jpl, kt )

      IF( ln_limdiahsb ) CALL lim_cons_hsm(1, 'limitd_th_rem', zvi_b, zsmv_b, zei_b, zfw_b, zfs_b, zft_b)

      !------------------------------------------------------------------------------|
      !  7) Add frazil ice growing in leads.
      !------------------------------------------------------------------------------|
      IF( ln_limdiahsb ) CALL lim_cons_hsm(0, 'limthd_lac', zvi_b, zsmv_b, zei_b, zfw_b, zfs_b, zft_b)

      CALL lim_thd_lac
      
      IF( ln_limdiahsb ) CALL lim_cons_hsm(1, 'limthd_lac', zvi_b, zsmv_b, zei_b, zfw_b, zfs_b, zft_b)

      ! Control print
      IF(ln_ctl) THEN
         CALL lim_var_glo2eqv

         CALL prt_ctl_info(' ')
         CALL prt_ctl_info(' - Cell values : ')
         CALL prt_ctl_info('   ~~~~~~~~~~~~~ ')
         CALL prt_ctl(tab2d_1=e12t , clinfo1=' lim_itd_th  : cell area :')
         CALL prt_ctl(tab2d_1=at_i , clinfo1=' lim_itd_th  : at_i      :')
         CALL prt_ctl(tab2d_1=vt_i , clinfo1=' lim_itd_th  : vt_i      :')
         CALL prt_ctl(tab2d_1=vt_s , clinfo1=' lim_itd_th  : vt_s      :')
         DO jl = 1, jpl
            CALL prt_ctl_info(' ')
            CALL prt_ctl_info(' - Category : ', ivar1=jl)
            CALL prt_ctl_info('   ~~~~~~~~~~')
            CALL prt_ctl(tab2d_1=a_i   (:,:,jl)   , clinfo1= ' lim_itd_th  : a_i      : ')
            CALL prt_ctl(tab2d_1=ht_i  (:,:,jl)   , clinfo1= ' lim_itd_th  : ht_i     : ')
            CALL prt_ctl(tab2d_1=ht_s  (:,:,jl)   , clinfo1= ' lim_itd_th  : ht_s     : ')
            CALL prt_ctl(tab2d_1=v_i   (:,:,jl)   , clinfo1= ' lim_itd_th  : v_i      : ')
            CALL prt_ctl(tab2d_1=v_s   (:,:,jl)   , clinfo1= ' lim_itd_th  : v_s      : ')
            CALL prt_ctl(tab2d_1=e_s   (:,:,1,jl) , clinfo1= ' lim_itd_th  : e_s      : ')
            CALL prt_ctl(tab2d_1=t_su  (:,:,jl)   , clinfo1= ' lim_itd_th  : t_su     : ')
            CALL prt_ctl(tab2d_1=t_s   (:,:,1,jl) , clinfo1= ' lim_itd_th  : t_snow   : ')
            CALL prt_ctl(tab2d_1=sm_i  (:,:,jl)   , clinfo1= ' lim_itd_th  : sm_i     : ')
            CALL prt_ctl(tab2d_1=smv_i (:,:,jl)   , clinfo1= ' lim_itd_th  : smv_i    : ')
            DO jk = 1, nlay_i
               CALL prt_ctl_info(' ')
               CALL prt_ctl_info(' - Layer : ', ivar1=jk)
               CALL prt_ctl_info('   ~~~~~~~')
               CALL prt_ctl(tab2d_1=t_i(:,:,jk,jl) , clinfo1= ' lim_itd_th  : t_i      : ')
               CALL prt_ctl(tab2d_1=e_i(:,:,jk,jl) , clinfo1= ' lim_itd_th  : e_i      : ')
            END DO
         END DO
      ENDIF
      !
      IF( nn_timing == 1 )  CALL timing_stop('limthd')

   END SUBROUTINE lim_thd 

 
   SUBROUTINE lim_thd_temp( kideb, kiut )
      !!-----------------------------------------------------------------------
      !!                   ***  ROUTINE lim_thd_temp *** 
      !!                 
      !! ** Purpose :   Computes sea ice temperature (Kelvin) from enthalpy
      !!
      !! ** Method  :   Formula (Bitz and Lipscomb, 1999)
      !!-------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kideb, kiut   ! bounds for the spatial loop
      !!
      INTEGER  ::   ji, jk   ! dummy loop indices
      REAL(wp) ::   ztmelts, zaaa, zbbb, zccc, zdiscrim  ! local scalar 
      !!-------------------------------------------------------------------
      ! Recover ice temperature
      DO jk = 1, nlay_i
         DO ji = kideb, kiut
            ztmelts       =  -tmut * s_i_1d(ji,jk) + rt0
            ! Conversion q(S,T) -> T (second order equation)
            zaaa          =  cpic
            zbbb          =  ( rcp - cpic ) * ( ztmelts - rt0 ) + q_i_1d(ji,jk) * r1_rhoic - lfus
            zccc          =  lfus * ( ztmelts - rt0 )
            zdiscrim      =  SQRT( MAX( zbbb * zbbb - 4._wp * zaaa * zccc, 0._wp ) )
            t_i_1d(ji,jk) =  rt0 - ( zbbb + zdiscrim ) / ( 2._wp * zaaa )
            
            ! mask temperature
            rswitch       =  1._wp - MAX( 0._wp , SIGN( 1._wp , - ht_i_1d(ji) ) ) 
            t_i_1d(ji,jk) =  rswitch * t_i_1d(ji,jk) + ( 1._wp - rswitch ) * rt0
         END DO 
      END DO 

   END SUBROUTINE lim_thd_temp

   SUBROUTINE lim_thd_lam( kideb, kiut )
      !!-----------------------------------------------------------------------
      !!                   ***  ROUTINE lim_thd_lam *** 
      !!                 
      !! ** Purpose :   Lateral melting in case monocategory
      !!                          ( dA = A/2h dh )
      !!-----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kideb, kiut        ! bounds for the spatial loop
      INTEGER             ::   ji                 ! dummy loop indices
      REAL(wp)            ::   zhi_bef            ! ice thickness before thermo
      REAL(wp)            ::   zdh_mel, zda_mel   ! net melting
      REAL(wp)            ::   zvi, zvs           ! ice/snow volumes 

      DO ji = kideb, kiut
         zdh_mel = MIN( 0._wp, dh_i_surf(ji) + dh_i_bott(ji) + dh_snowice(ji) + dh_i_sub(ji) )
         IF( zdh_mel < 0._wp .AND. a_i_1d(ji) > 0._wp )  THEN
            zvi          = a_i_1d(ji) * ht_i_1d(ji)
            zvs          = a_i_1d(ji) * ht_s_1d(ji)
            ! lateral melting = concentration change
            zhi_bef     = ht_i_1d(ji) - zdh_mel
            rswitch     = MAX( 0._wp , SIGN( 1._wp , zhi_bef - epsi20 ) )
            zda_mel     = rswitch * a_i_1d(ji) * zdh_mel / ( 2._wp * MAX( zhi_bef, epsi20 ) )
            a_i_1d(ji)  = MAX( epsi20, a_i_1d(ji) + zda_mel ) 
            ! adjust thickness
            ht_i_1d(ji) = zvi / a_i_1d(ji)            
            ht_s_1d(ji) = zvs / a_i_1d(ji)            
            ! retrieve total concentration
            at_i_1d(ji) = a_i_1d(ji)
         END IF
      END DO
      
   END SUBROUTINE lim_thd_lam

   SUBROUTINE lim_thd_1d2d( nbpb, jl, kn )
      !!-----------------------------------------------------------------------
      !!                   ***  ROUTINE lim_thd_1d2d *** 
      !!                 
      !! ** Purpose :   move arrays from 1d to 2d and the reverse
      !!-----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kn       ! 1= from 2D to 1D
                                        ! 2= from 1D to 2D
      INTEGER, INTENT(in) ::   nbpb     ! size of 1D arrays
      INTEGER, INTENT(in) ::   jl       ! ice cat
      INTEGER             ::   jk       ! dummy loop indices

      SELECT CASE( kn )

      CASE( 1 )

         CALL tab_2d_1d( nbpb, at_i_1d     (1:nbpb), at_i            , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, a_i_1d      (1:nbpb), a_i(:,:,jl)     , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, ht_i_1d     (1:nbpb), ht_i(:,:,jl)    , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, ht_s_1d     (1:nbpb), ht_s(:,:,jl)    , jpi, jpj, npb(1:nbpb) )
         
         CALL tab_2d_1d( nbpb, t_su_1d     (1:nbpb), t_su(:,:,jl)    , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, sm_i_1d     (1:nbpb), sm_i(:,:,jl)    , jpi, jpj, npb(1:nbpb) )
         DO jk = 1, nlay_s
            CALL tab_2d_1d( nbpb, t_s_1d(1:nbpb,jk), t_s(:,:,jk,jl)  , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, q_s_1d(1:nbpb,jk), e_s(:,:,jk,jl)  , jpi, jpj, npb(1:nbpb) )
         END DO
         DO jk = 1, nlay_i
            CALL tab_2d_1d( nbpb, t_i_1d(1:nbpb,jk), t_i(:,:,jk,jl)  , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, q_i_1d(1:nbpb,jk), e_i(:,:,jk,jl)  , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, s_i_1d(1:nbpb,jk), s_i(:,:,jk,jl)  , jpi, jpj, npb(1:nbpb) )
         END DO
         
         CALL tab_2d_1d( nbpb, qprec_ice_1d(1:nbpb), qprec_ice(:,:) , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, qevap_ice_1d(1:nbpb), qevap_ice(:,:,jl) , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, qsr_ice_1d (1:nbpb), qsr_ice(:,:,jl) , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, fr1_i0_1d  (1:nbpb), fr1_i0          , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, fr2_i0_1d  (1:nbpb), fr2_i0          , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, qns_ice_1d (1:nbpb), qns_ice(:,:,jl) , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, ftr_ice_1d (1:nbpb), ftr_ice(:,:,jl) , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, evap_ice_1d (1:nbpb), evap_ice(:,:,jl), jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, dqns_ice_1d(1:nbpb), dqns_ice(:,:,jl), jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, t_bo_1d     (1:nbpb), t_bo            , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, sprecip_1d (1:nbpb), sprecip         , jpi, jpj, npb(1:nbpb) ) 
         CALL tab_2d_1d( nbpb, fhtur_1d   (1:nbpb), fhtur           , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, qlead_1d   (1:nbpb), qlead           , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, fhld_1d    (1:nbpb), fhld            , jpi, jpj, npb(1:nbpb) )
         
         CALL tab_2d_1d( nbpb, wfx_snw_1d (1:nbpb), wfx_snw         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, wfx_sub_1d (1:nbpb), wfx_sub         , jpi, jpj, npb(1:nbpb) )
         
         CALL tab_2d_1d( nbpb, wfx_bog_1d (1:nbpb), wfx_bog         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, wfx_bom_1d (1:nbpb), wfx_bom         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, wfx_sum_1d (1:nbpb), wfx_sum         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, wfx_sni_1d (1:nbpb), wfx_sni         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, wfx_res_1d (1:nbpb), wfx_res         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, wfx_spr_1d (1:nbpb), wfx_spr         , jpi, jpj, npb(1:nbpb) )
         
         CALL tab_2d_1d( nbpb, sfx_bog_1d (1:nbpb), sfx_bog         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, sfx_bom_1d (1:nbpb), sfx_bom         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, sfx_sum_1d (1:nbpb), sfx_sum         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, sfx_sni_1d (1:nbpb), sfx_sni         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, sfx_bri_1d (1:nbpb), sfx_bri         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, sfx_res_1d (1:nbpb), sfx_res         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, sfx_sub_1d (1:nbpb), sfx_sub         , jpi, jpj,npb(1:nbpb) )
 
         CALL tab_2d_1d( nbpb, hfx_thd_1d (1:nbpb), hfx_thd         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, hfx_spr_1d (1:nbpb), hfx_spr         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, hfx_sum_1d (1:nbpb), hfx_sum         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, hfx_bom_1d (1:nbpb), hfx_bom         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, hfx_bog_1d (1:nbpb), hfx_bog         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, hfx_dif_1d (1:nbpb), hfx_dif         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, hfx_opw_1d (1:nbpb), hfx_opw         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, hfx_snw_1d (1:nbpb), hfx_snw         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, hfx_sub_1d (1:nbpb), hfx_sub         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, hfx_err_1d (1:nbpb), hfx_err         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, hfx_res_1d (1:nbpb), hfx_res         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, hfx_err_dif_1d (1:nbpb), hfx_err_dif , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d( nbpb, hfx_err_rem_1d (1:nbpb), hfx_err_rem , jpi, jpj, npb(1:nbpb) )

      CASE( 2 )

         CALL tab_1d_2d( nbpb, at_i          , npb, at_i_1d    (1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, ht_i(:,:,jl)  , npb, ht_i_1d    (1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, ht_s(:,:,jl)  , npb, ht_s_1d    (1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, a_i (:,:,jl)  , npb, a_i_1d     (1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, t_su(:,:,jl)  , npb, t_su_1d    (1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, sm_i(:,:,jl)  , npb, sm_i_1d    (1:nbpb)   , jpi, jpj )
         DO jk = 1, nlay_s
            CALL tab_1d_2d( nbpb, t_s(:,:,jk,jl), npb, t_s_1d     (1:nbpb,jk), jpi, jpj)
            CALL tab_1d_2d( nbpb, e_s(:,:,jk,jl), npb, q_s_1d     (1:nbpb,jk), jpi, jpj)
         END DO
         DO jk = 1, nlay_i
            CALL tab_1d_2d( nbpb, t_i(:,:,jk,jl), npb, t_i_1d     (1:nbpb,jk), jpi, jpj)
            CALL tab_1d_2d( nbpb, e_i(:,:,jk,jl), npb, q_i_1d     (1:nbpb,jk), jpi, jpj)
            CALL tab_1d_2d( nbpb, s_i(:,:,jk,jl), npb, s_i_1d     (1:nbpb,jk), jpi, jpj)
         END DO
         CALL tab_1d_2d( nbpb, qlead         , npb, qlead_1d  (1:nbpb)   , jpi, jpj )
         
         CALL tab_1d_2d( nbpb, wfx_snw       , npb, wfx_snw_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, wfx_sub       , npb, wfx_sub_1d(1:nbpb)   , jpi, jpj )
         
         CALL tab_1d_2d( nbpb, wfx_bog       , npb, wfx_bog_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, wfx_bom       , npb, wfx_bom_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, wfx_sum       , npb, wfx_sum_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, wfx_sni       , npb, wfx_sni_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, wfx_res       , npb, wfx_res_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, wfx_spr       , npb, wfx_spr_1d(1:nbpb)   , jpi, jpj )
         
         CALL tab_1d_2d( nbpb, sfx_bog       , npb, sfx_bog_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, sfx_bom       , npb, sfx_bom_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, sfx_sum       , npb, sfx_sum_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, sfx_sni       , npb, sfx_sni_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, sfx_res       , npb, sfx_res_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, sfx_bri       , npb, sfx_bri_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, sfx_sub       , npb, sfx_sub_1d(1:nbpb)   , jpi, jpj )        
 
         CALL tab_1d_2d( nbpb, hfx_thd       , npb, hfx_thd_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, hfx_spr       , npb, hfx_spr_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, hfx_sum       , npb, hfx_sum_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, hfx_bom       , npb, hfx_bom_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, hfx_bog       , npb, hfx_bog_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, hfx_dif       , npb, hfx_dif_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, hfx_opw       , npb, hfx_opw_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, hfx_snw       , npb, hfx_snw_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, hfx_sub       , npb, hfx_sub_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, hfx_err       , npb, hfx_err_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, hfx_res       , npb, hfx_res_1d(1:nbpb)   , jpi, jpj )
         CALL tab_1d_2d( nbpb, hfx_err_rem   , npb, hfx_err_rem_1d(1:nbpb), jpi, jpj )
         CALL tab_1d_2d( nbpb, hfx_err_dif   , npb, hfx_err_dif_1d(1:nbpb), jpi, jpj )
         !
         CALL tab_1d_2d( nbpb, qns_ice(:,:,jl), npb, qns_ice_1d(1:nbpb) , jpi, jpj)
         CALL tab_1d_2d( nbpb, ftr_ice(:,:,jl), npb, ftr_ice_1d(1:nbpb) , jpi, jpj )
      END SELECT

   END SUBROUTINE lim_thd_1d2d


   SUBROUTINE lim_thd_init
      !!-----------------------------------------------------------------------
      !!                   ***  ROUTINE lim_thd_init *** 
      !!                 
      !! ** Purpose :   Physical constants and parameters linked to the ice 
      !!              thermodynamics
      !!
      !! ** Method  :   Read the namicethd namelist and check the ice-thermo
      !!              parameter values called at the first timestep (nit000)
      !!
      !! ** input   :   Namelist namicether
      !!-------------------------------------------------------------------
      INTEGER  ::   ios                 ! Local integer output status for namelist read
      NAMELIST/namicethd/ rn_hnewice, ln_frazil, rn_maxfrazb, rn_vfrazb, rn_Cfrazb,                       &
         &                rn_himin, rn_betas, rn_kappa_i, nn_conv_dif, rn_terr_dif, nn_ice_thcon,         &
         &                rn_cdsn, nn_monocat, ln_it_qnsice
      !!-------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'lim_thd : Ice Thermodynamics'
         WRITE(numout,*) '~~~~~~~'
      ENDIF
      !
      REWIND( numnam_ice_ref )              ! Namelist namicethd in reference namelist : Ice thermodynamics
      READ  ( numnam_ice_ref, namicethd, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namicethd in reference namelist', lwp )

      REWIND( numnam_ice_cfg )              ! Namelist namicethd in configuration namelist : Ice thermodynamics
      READ  ( numnam_ice_cfg, namicethd, IOSTAT = ios, ERR = 902 )
902   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namicethd in configuration namelist', lwp )
      IF(lwm) WRITE ( numoni, namicethd )
      !
      IF ( ( jpl > 1 ) .AND. ( nn_monocat == 1 ) ) THEN 
         nn_monocat = 0
         IF(lwp) WRITE(numout, *) '   nn_monocat must be 0 in multi-category case '
      ENDIF

      !
      IF(lwp) THEN                          ! control print
         WRITE(numout,*)
         WRITE(numout,*)'   Namelist of ice parameters for ice thermodynamic computation '
         WRITE(numout,*)'      ice thick. for lateral accretion                        rn_hnewice   = ', rn_hnewice
         WRITE(numout,*)'      Frazil ice thickness as a function of wind or not       ln_frazil    = ', ln_frazil
         WRITE(numout,*)'      Maximum proportion of frazil ice collecting at bottom   rn_maxfrazb  = ', rn_maxfrazb
         WRITE(numout,*)'      Thresold relative drift speed for collection of frazil  rn_vfrazb    = ', rn_vfrazb
         WRITE(numout,*)'      Squeezing coefficient for collection of frazil          rn_Cfrazb    = ', rn_Cfrazb
         WRITE(numout,*)'      minimum ice thickness                                   rn_himin     = ', rn_himin 
         WRITE(numout,*)'      numerical carac. of the scheme for diffusion in ice '
         WRITE(numout,*)'      coefficient for ice-lead partition of snowfall          rn_betas     = ', rn_betas
         WRITE(numout,*)'      extinction radiation parameter in sea ice               rn_kappa_i   = ', rn_kappa_i
         WRITE(numout,*)'      maximal n. of iter. for heat diffusion computation      nn_conv_dif  = ', nn_conv_dif
         WRITE(numout,*)'      maximal err. on T for heat diffusion computation        rn_terr_dif  = ', rn_terr_dif
         WRITE(numout,*)'      switch for comp. of thermal conductivity in the ice     nn_ice_thcon = ', nn_ice_thcon
         WRITE(numout,*)'      thermal conductivity of the snow                        rn_cdsn      = ', rn_cdsn
         WRITE(numout,*)'      check heat conservation in the ice/snow                 con_i        = ', con_i
         WRITE(numout,*)'      virtual ITD mono-category parameterizations (1) or not  nn_monocat   = ', nn_monocat
         WRITE(numout,*)'      iterate the surface non-solar flux (T) or not (F)       ln_it_qnsice = ', ln_it_qnsice
      ENDIF
      !
   END SUBROUTINE lim_thd_init

#else
   !!----------------------------------------------------------------------
   !!   Default option         Dummy module          NO  LIM3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE limthd
