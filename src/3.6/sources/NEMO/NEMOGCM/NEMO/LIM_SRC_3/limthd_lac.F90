MODULE limthd_lac
   !!======================================================================
   !!                       ***  MODULE limthd_lac   ***
   !!                lateral thermodynamic growth of the ice 
   !!======================================================================
   !! History :  LIM  ! 2005-12 (M. Vancoppenolle)  Original code
   !!             -   ! 2006-01 (M. Vancoppenolle)  add ITD
   !!            3.0  ! 2007-07 (M. Vancoppenolle)  Mass and energy conservation tested
   !!            4.0  ! 2011-02 (G. Madec) dynamical allocation
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3'                                      LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_lat_acr   : lateral accretion of ice
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean parameters
   USE dom_oce        ! domain variables
   USE phycst         ! physical constants
   USE sbc_oce        ! Surface boundary condition: ocean fields
   USE sbc_ice        ! Surface boundary condition: ice fields
   USE thd_ice        ! LIM thermodynamics
   USE dom_ice        ! LIM domain
   USE ice            ! LIM variables
   USE limtab         ! LIM 2D <==> 1D
   USE limcons        ! LIM conservation
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE wrk_nemo       ! work arrays
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)  
   USE limthd_ent
   USE limvar

   IMPLICIT NONE
   PRIVATE

   PUBLIC lim_thd_lac     ! called by lim_thd

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: limthd_lac.F90 6316 2016-02-15 13:35:37Z cetlod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_thd_lac
      !!-------------------------------------------------------------------
      !!               ***   ROUTINE lim_thd_lac  ***
      !!  
      !! ** Purpose : Computation of the evolution of the ice thickness and 
      !!      concentration as a function of the heat balance in the leads.
      !!      It is only used for lateral accretion
      !!       
      !! ** Method  : Ice is formed in the open water when ocean lose heat
      !!      (heat budget of open water Bl is negative) .
      !!      Computation of the increase of 1-A (ice concentration) fol-
      !!      lowing the law :
      !!      (dA/dt)acc = F[ (1-A)/(1-a) ] * [ Bl / (Li*h0) ]
      !!       where - h0 is the thickness of ice created in the lead
      !!             - a is a minimum fraction for leads
      !!             - F is a monotonic non-increasing function defined as:
      !!                  F(X)=( 1 - X**exld )**(1.0/exld)
      !!             - exld is the exponent closure rate (=2 default val.)
      !! 
      !! ** Action : - Adjustment of snow and ice thicknesses and heat
      !!                content in brine pockets
      !!             - Updating ice internal temperature
      !!             - Computation of variation of ice volume and mass
      !!             - Computation of frldb after lateral accretion and 
      !!               update ht_s_1d, ht_i_1d and tbif_1d(:,:)      
      !!------------------------------------------------------------------------
      INTEGER ::   ji,jj,jk,jl      ! dummy loop indices
      INTEGER ::   nbpac            ! local integers 
      INTEGER ::   ii, ij, iter     !   -       -
      REAL(wp)  ::   ztmelts, zdv, zfrazb, zweight, zde                         ! local scalars
      REAL(wp) ::   zgamafr, zvfrx, zvgx, ztaux, ztwogp, zf                     !   -      -
      REAL(wp) ::   ztenagm, zvfry, zvgy, ztauy, zvrel2, zfp, zsqcd , zhicrit   !   -      -
      CHARACTER (len = 15) :: fieldid

      REAL(wp) ::   zQm          ! enthalpy exchanged with the ocean (J/m2, >0 towards ocean)
      REAL(wp) ::   zEi          ! sea ice specific enthalpy (J/kg)
      REAL(wp) ::   zEw          ! seawater specific enthalpy (J/kg)
      REAL(wp) ::   zfmdt        ! mass flux x time step (kg/m2, >0 towards ocean)
     
      REAL(wp) ::   zv_newfra
  
      INTEGER , POINTER, DIMENSION(:) ::   jcat        ! indexes of categories where new ice grows
      REAL(wp), POINTER, DIMENSION(:) ::   zswinew     ! switch for new ice or not

      REAL(wp), POINTER, DIMENSION(:) ::   zv_newice   ! volume of accreted ice
      REAL(wp), POINTER, DIMENSION(:) ::   za_newice   ! fractional area of accreted ice
      REAL(wp), POINTER, DIMENSION(:) ::   zh_newice   ! thickness of accreted ice
      REAL(wp), POINTER, DIMENSION(:) ::   ze_newice   ! heat content of accreted ice
      REAL(wp), POINTER, DIMENSION(:) ::   zs_newice   ! salinity of accreted ice
      REAL(wp), POINTER, DIMENSION(:) ::   zo_newice   ! age of accreted ice
      REAL(wp), POINTER, DIMENSION(:) ::   zdv_res     ! residual volume in case of excessive heat budget
      REAL(wp), POINTER, DIMENSION(:) ::   zda_res     ! residual area in case of excessive heat budget
      REAL(wp), POINTER, DIMENSION(:) ::   zat_i_1d    ! total ice fraction    
      REAL(wp), POINTER, DIMENSION(:) ::   zv_frazb    ! accretion of frazil ice at the ice bottom
      REAL(wp), POINTER, DIMENSION(:) ::   zvrel_1d    ! relative ice / frazil velocity (1D vector)

      REAL(wp), POINTER, DIMENSION(:,:) ::   zv_b      ! old volume of ice in category jl
      REAL(wp), POINTER, DIMENSION(:,:) ::   za_b      ! old area of ice in category jl
      REAL(wp), POINTER, DIMENSION(:,:) ::   za_i_1d   ! 1-D version of a_i
      REAL(wp), POINTER, DIMENSION(:,:) ::   zv_i_1d   ! 1-D version of v_i
      REAL(wp), POINTER, DIMENSION(:,:) ::   zsmv_i_1d ! 1-D version of smv_i

      REAL(wp), POINTER, DIMENSION(:,:,:) ::   ze_i_1d !: 1-D version of e_i

      REAL(wp), POINTER, DIMENSION(:,:) ::   zvrel     ! relative ice / frazil velocity

      REAL(wp) :: zcai = 1.4e-3_wp                     ! ice-air drag (clem: should be dependent on coupling/forcing used)
      !!-----------------------------------------------------------------------!

      CALL wrk_alloc( jpij, jcat )   ! integer
      CALL wrk_alloc( jpij, zswinew, zv_newice, za_newice, zh_newice, ze_newice, zs_newice, zo_newice )
      CALL wrk_alloc( jpij, zdv_res, zda_res, zat_i_1d, zv_frazb, zvrel_1d )
      CALL wrk_alloc( jpij,jpl, zv_b, za_b, za_i_1d, zv_i_1d, zsmv_i_1d )
      CALL wrk_alloc( jpij,nlay_i,jpl, ze_i_1d )
      CALL wrk_alloc( jpi,jpj, zvrel )

      CALL lim_var_agg(1)
      CALL lim_var_glo2eqv
      !------------------------------------------------------------------------------|
      ! 2) Convert units for ice internal energy
      !------------------------------------------------------------------------------|
      DO jl = 1, jpl
         DO jk = 1, nlay_i
            DO jj = 1, jpj
               DO ji = 1, jpi
                  !Energy of melting q(S,T) [J.m-3]
                  rswitch          = MAX(  0._wp , SIGN( 1._wp , v_i(ji,jj,jl) - epsi20 )  )   !0 if no ice
                  e_i(ji,jj,jk,jl) = rswitch * e_i(ji,jj,jk,jl) / MAX( v_i(ji,jj,jl), epsi20 ) * REAL( nlay_i, wp )
               END DO
            END DO
         END DO
      END DO

      !------------------------------------------------------------------------------!
      ! 3) Collection thickness of ice formed in leads and polynyas
      !------------------------------------------------------------------------------!    
      ! hicol is the thickness of new ice formed in open water
      ! hicol can be either prescribed (frazswi = 0) or computed (frazswi = 1)
      ! Frazil ice forms in open water, is transported by wind
      ! accumulates at the edge of the consolidated ice edge
      ! where it forms aggregates of a specific thickness called
      ! collection thickness.

      ! Note : the following algorithm currently breaks vectorization
      ! 

      zvrel(:,:) = 0._wp

      ! Default new ice thickness
      WHERE( qlead(:,:) < 0._wp ) ; hicol = rn_hnewice
      ELSEWHERE                   ; hicol = 0._wp
      END WHERE

      IF( ln_frazil ) THEN

         !--------------------
         ! Physical constants
         !--------------------
         hicol(:,:) = 0._wp

         zhicrit = 0.04 ! frazil ice thickness
         ztwogp  = 2. * rau0 / ( grav * 0.3 * ( rau0 - rhoic ) ) ! reduced grav
         zsqcd   = 1.0 / SQRT( 1.3 * zcai ) ! 1/SQRT(airdensity*drag)
         zgamafr = 0.03

         DO jj = 2, jpj
            DO ji = 2, jpi
               IF ( qlead(ji,jj) < 0._wp ) THEN
                  !-------------
                  ! Wind stress
                  !-------------
                  ! C-grid wind stress components
                  ztaux         = ( utau_ice(ji-1,jj  ) * umask(ji-1,jj  ,1)   &
                     &          +   utau_ice(ji  ,jj  ) * umask(ji  ,jj  ,1) ) * 0.5_wp
                  ztauy         = ( vtau_ice(ji  ,jj-1) * vmask(ji  ,jj-1,1)   &
                     &          +   vtau_ice(ji  ,jj  ) * vmask(ji  ,jj  ,1) ) * 0.5_wp
                  ! Square root of wind stress
                  ztenagm       =  SQRT( SQRT( ztaux * ztaux + ztauy * ztauy ) )

                  !---------------------
                  ! Frazil ice velocity
                  !---------------------
                  rswitch = MAX( 0._wp, SIGN( 1._wp , ztenagm - epsi10 ) )
                  zvfrx   = rswitch * zgamafr * zsqcd * ztaux / MAX( ztenagm, epsi10 )
                  zvfry   = rswitch * zgamafr * zsqcd * ztauy / MAX( ztenagm, epsi10 )

                  !-------------------
                  ! Pack ice velocity
                  !-------------------
                  ! C-grid ice velocity
                  rswitch = MAX(  0._wp, SIGN( 1._wp , at_i(ji,jj) )  )
                  zvgx    = rswitch * ( u_ice(ji-1,jj  ) * umask(ji-1,jj  ,1)  + u_ice(ji,jj) * umask(ji,jj,1) ) * 0.5_wp
                  zvgy    = rswitch * ( v_ice(ji  ,jj-1) * vmask(ji  ,jj-1,1)  + v_ice(ji,jj) * vmask(ji,jj,1) ) * 0.5_wp

                  !-----------------------------------
                  ! Relative frazil/pack ice velocity
                  !-----------------------------------
                  ! absolute relative velocity
                  zvrel2 = MAX(  ( zvfrx - zvgx ) * ( zvfrx - zvgx )   &
                     &         + ( zvfry - zvgy ) * ( zvfry - zvgy ) , 0.15 * 0.15 )
                  zvrel(ji,jj) = SQRT( zvrel2 )

                  !---------------------
                  ! Iterative procedure
                  !---------------------
                  hicol(ji,jj) = zhicrit +   ( zhicrit + 0.1 )    &
                     &                   / ( ( zhicrit + 0.1 ) * ( zhicrit + 0.1 ) -  zhicrit * zhicrit ) * ztwogp * zvrel2

                  iter = 1
                  DO WHILE ( iter < 20 ) 
                     zf  = ( hicol(ji,jj) - zhicrit ) * ( hicol(ji,jj) * hicol(ji,jj) - zhicrit * zhicrit ) -   &
                        &    hicol(ji,jj) * zhicrit * ztwogp * zvrel2
                     zfp = ( hicol(ji,jj) - zhicrit ) * ( 3.0 * hicol(ji,jj) + zhicrit ) - zhicrit * ztwogp * zvrel2

                     hicol(ji,jj) = hicol(ji,jj) - zf/zfp
                     iter = iter + 1
                  END DO

               ENDIF ! end of selection of pixels where ice forms

            END DO 
         END DO 
         ! 
         CALL lbc_lnk( zvrel(:,:), 'T', 1. )
         CALL lbc_lnk( hicol(:,:), 'T', 1. )

      ENDIF ! End of computation of frazil ice collection thickness

      !------------------------------------------------------------------------------!
      ! 4) Identify grid points where new ice forms
      !------------------------------------------------------------------------------!

      !-------------------------------------
      ! Select points for new ice formation
      !-------------------------------------
      ! This occurs if open water energy budget is negative
      nbpac = 0
      npac(:) = 0
      !
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF ( qlead(ji,jj)  <  0._wp ) THEN
               nbpac = nbpac + 1
               npac( nbpac ) = (jj - 1) * jpi + ji
            ENDIF
         END DO
      END DO

      ! debug point to follow
      jiindex_1d = 0
      IF( ln_icectl ) THEN
         DO ji = mi0(iiceprt), mi1(iiceprt)
            DO jj = mj0(jiceprt), mj1(jiceprt)
               IF ( qlead(ji,jj)  <  0._wp ) THEN
                  jiindex_1d = (jj - 1) * jpi + ji
               ENDIF
            END DO
         END DO
      ENDIF
   
      IF( ln_icectl ) WRITE(numout,*) 'lim_thd_lac : nbpac = ', nbpac

      !------------------------------
      ! Move from 2-D to 1-D vectors
      !------------------------------
      ! If ocean gains heat do nothing. Otherwise compute new ice formation

      IF ( nbpac > 0 ) THEN

         CALL tab_2d_1d( nbpac, zat_i_1d  (1:nbpac)     , at_i         , jpi, jpj, npac(1:nbpac) )
         DO jl = 1, jpl
            CALL tab_2d_1d( nbpac, za_i_1d  (1:nbpac,jl), a_i  (:,:,jl), jpi, jpj, npac(1:nbpac) )
            CALL tab_2d_1d( nbpac, zv_i_1d  (1:nbpac,jl), v_i  (:,:,jl), jpi, jpj, npac(1:nbpac) )
            CALL tab_2d_1d( nbpac, zsmv_i_1d(1:nbpac,jl), smv_i(:,:,jl), jpi, jpj, npac(1:nbpac) )
            DO jk = 1, nlay_i
               CALL tab_2d_1d( nbpac, ze_i_1d(1:nbpac,jk,jl), e_i(:,:,jk,jl) , jpi, jpj, npac(1:nbpac) )
            END DO
         END DO

         CALL tab_2d_1d( nbpac, qlead_1d  (1:nbpac)     , qlead     , jpi, jpj, npac(1:nbpac) )
         CALL tab_2d_1d( nbpac, t_bo_1d   (1:nbpac)     , t_bo      , jpi, jpj, npac(1:nbpac) )
         CALL tab_2d_1d( nbpac, sfx_opw_1d(1:nbpac)     , sfx_opw   , jpi, jpj, npac(1:nbpac) )
         CALL tab_2d_1d( nbpac, wfx_opw_1d(1:nbpac)     , wfx_opw   , jpi, jpj, npac(1:nbpac) )
         CALL tab_2d_1d( nbpac, hicol_1d  (1:nbpac)     , hicol     , jpi, jpj, npac(1:nbpac) )
         CALL tab_2d_1d( nbpac, zvrel_1d  (1:nbpac)     , zvrel     , jpi, jpj, npac(1:nbpac) )

         CALL tab_2d_1d( nbpac, hfx_thd_1d(1:nbpac)     , hfx_thd   , jpi, jpj, npac(1:nbpac) )
         CALL tab_2d_1d( nbpac, hfx_opw_1d(1:nbpac)     , hfx_opw   , jpi, jpj, npac(1:nbpac) )
         CALL tab_2d_1d( nbpac, rn_amax_1d(1:nbpac)     , rn_amax_2d, jpi, jpj, npac(1:nbpac) )

         !------------------------------------------------------------------------------!
         ! 5) Compute thickness, salinity, enthalpy, age, area and volume of new ice
         !------------------------------------------------------------------------------!

         !-----------------------------------------
         ! Keep old ice areas and volume in memory
         !-----------------------------------------
         zv_b(1:nbpac,:) = zv_i_1d(1:nbpac,:) 
         za_b(1:nbpac,:) = za_i_1d(1:nbpac,:)

         !----------------------
         ! Thickness of new ice
         !----------------------
         zh_newice(1:nbpac) = hicol_1d(1:nbpac)

         !----------------------
         ! Salinity of new ice 
         !----------------------
         SELECT CASE ( nn_icesal )
         CASE ( 1 )                    ! Sice = constant 
            zs_newice(1:nbpac) = rn_icesal
         CASE ( 2 )                    ! Sice = F(z,t) [Vancoppenolle et al (2005)]
            DO ji = 1, nbpac
               ii =   MOD( npac(ji) - 1 , jpi ) + 1
               ij =      ( npac(ji) - 1 ) / jpi + 1
               zs_newice(ji) = MIN(  4.606 + 0.91 / zh_newice(ji) , rn_simax , 0.5 * sss_m(ii,ij)  )
            END DO
         CASE ( 3 )                    ! Sice = F(z) [multiyear ice]
            zs_newice(1:nbpac) =   2.3
         END SELECT

         !-------------------------
         ! Heat content of new ice
         !-------------------------
         ! We assume that new ice is formed at the seawater freezing point
         DO ji = 1, nbpac
            ztmelts       = - tmut * zs_newice(ji) + rt0                  ! Melting point (K)
            ze_newice(ji) =   rhoic * (  cpic * ( ztmelts - t_bo_1d(ji) )                             &
               &                       + lfus * ( 1.0 - ( ztmelts - rt0 ) / MIN( t_bo_1d(ji) - rt0, -epsi10 ) )   &
               &                       - rcp  *         ( ztmelts - rt0 )  )
         END DO

         !----------------
         ! Age of new ice
         !----------------
         DO ji = 1, nbpac
            zo_newice(ji) = 0._wp
         END DO

         !-------------------
         ! Volume of new ice
         !-------------------
         DO ji = 1, nbpac

            zEi           = - ze_newice(ji) * r1_rhoic             ! specific enthalpy of forming ice [J/kg]

            zEw           = rcp * ( t_bo_1d(ji) - rt0 )            ! specific enthalpy of seawater at t_bo_1d [J/kg]
                                                                   ! clem: we suppose we are already at the freezing point (condition qlead<0 is satisfyied) 
                                                                   
            zdE           = zEi - zEw                              ! specific enthalpy difference [J/kg]
                                              
            zfmdt         = - qlead_1d(ji) / zdE                   ! Fm.dt [kg/m2] (<0) 
                                                                   ! clem: we use qlead instead of zqld (limthd) because we suppose we are at the freezing point   
            zv_newice(ji) = - zfmdt * r1_rhoic

            zQm           = zfmdt * zEw                            ! heat to the ocean >0 associated with mass flux  

            ! Contribution to heat flux to the ocean [W.m-2], >0  
            hfx_thd_1d(ji) = hfx_thd_1d(ji) + zfmdt * zEw * r1_rdtice
            ! Total heat flux used in this process [W.m-2]  
            hfx_opw_1d(ji) = hfx_opw_1d(ji) - zfmdt * zdE * r1_rdtice
            ! mass flux
            wfx_opw_1d(ji) = wfx_opw_1d(ji) - zv_newice(ji) * rhoic * r1_rdtice
            ! salt flux
            sfx_opw_1d(ji) = sfx_opw_1d(ji) - zv_newice(ji) * rhoic * zs_newice(ji) * r1_rdtice
         END DO
         
         zv_frazb(:) = 0._wp
         IF( ln_frazil ) THEN
            ! A fraction zfrazb of frazil ice is accreted at the ice bottom
            DO ji = 1, nbpac
               rswitch       = 1._wp - MAX( 0._wp, SIGN( 1._wp , - zat_i_1d(ji) ) )
               zfrazb        = rswitch * ( TANH( rn_Cfrazb * ( zvrel_1d(ji) - rn_vfrazb ) ) + 1.0 ) * 0.5 * rn_maxfrazb
               zv_frazb(ji)  =         zfrazb   * zv_newice(ji)
               zv_newice(ji) = ( 1.0 - zfrazb ) * zv_newice(ji)
            END DO
         END IF
         
         !-----------------
         ! Area of new ice
         !-----------------
         DO ji = 1, nbpac
            za_newice(ji) = zv_newice(ji) / zh_newice(ji)
         END DO

         !------------------------------------------------------------------------------!
         ! 6) Redistribute new ice area and volume into ice categories                  !
         !------------------------------------------------------------------------------!

         !------------------------
         ! 6.1) lateral ice growth
         !------------------------
         ! If lateral ice growth gives an ice concentration gt 1, then
         ! we keep the excessive volume in memory and attribute it later to bottom accretion
         DO ji = 1, nbpac
            IF ( za_newice(ji) >  ( rn_amax_1d(ji) - zat_i_1d(ji) ) ) THEN
               zda_res(ji)   = za_newice(ji) - ( rn_amax_1d(ji) - zat_i_1d(ji) )
               zdv_res(ji)   = zda_res  (ji) * zh_newice(ji) 
               za_newice(ji) = za_newice(ji) - zda_res  (ji)
               zv_newice(ji) = zv_newice(ji) - zdv_res  (ji)
            ELSE
               zda_res(ji) = 0._wp
               zdv_res(ji) = 0._wp
            ENDIF
         END DO

         ! find which category to fill
         zat_i_1d(:) = 0._wp
         DO jl = 1, jpl
            DO ji = 1, nbpac
               IF( zh_newice(ji) > hi_max(jl-1) .AND. zh_newice(ji) <= hi_max(jl) ) THEN
                  za_i_1d (ji,jl) = za_i_1d (ji,jl) + za_newice(ji)
                  zv_i_1d (ji,jl) = zv_i_1d (ji,jl) + zv_newice(ji)
                  jcat    (ji)    = jl
               ENDIF
               zat_i_1d(ji) = zat_i_1d(ji) + za_i_1d  (ji,jl)
            END DO
         END DO

         ! Heat content
         DO ji = 1, nbpac
            jl = jcat(ji)                                                    ! categroy in which new ice is put
            zswinew  (ji) = MAX( 0._wp , SIGN( 1._wp , - za_b(ji,jl) ) )   ! 0 if old ice
         END DO

         DO jk = 1, nlay_i
            DO ji = 1, nbpac
               jl = jcat(ji)
               rswitch = MAX( 0._wp, SIGN( 1._wp , zv_i_1d(ji,jl) - epsi20 ) )
               ze_i_1d(ji,jk,jl) = zswinew(ji)   *   ze_newice(ji) +                                                    &
                  &        ( 1.0 - zswinew(ji) ) * ( ze_newice(ji) * zv_newice(ji) + ze_i_1d(ji,jk,jl) * zv_b(ji,jl) )  &
                  &        * rswitch / MAX( zv_i_1d(ji,jl), epsi20 )
            END DO
         END DO

         !------------------------------------------------
         ! 6.2) bottom ice growth + ice enthalpy remapping
         !------------------------------------------------
         DO jl = 1, jpl

            ! for remapping
            h_i_old (1:nbpac,0:nlay_i+1) = 0._wp
            qh_i_old(1:nbpac,0:nlay_i+1) = 0._wp
            DO jk = 1, nlay_i
               DO ji = 1, nbpac
                  h_i_old (ji,jk) = zv_i_1d(ji,jl) * r1_nlay_i
                  qh_i_old(ji,jk) = ze_i_1d(ji,jk,jl) * h_i_old(ji,jk)
               END DO
            END DO

            ! new volumes including lateral/bottom accretion + residual
            DO ji = 1, nbpac
               rswitch        = MAX( 0._wp, SIGN( 1._wp , zat_i_1d(ji) - epsi20 ) )
               zv_newfra      = rswitch * ( zdv_res(ji) + zv_frazb(ji) ) * za_i_1d(ji,jl) / MAX( zat_i_1d(ji) , epsi20 )
               za_i_1d(ji,jl) = rswitch * za_i_1d(ji,jl)               
               zv_i_1d(ji,jl) = zv_i_1d(ji,jl) + zv_newfra
               ! for remapping
               h_i_old (ji,nlay_i+1) = zv_newfra
               qh_i_old(ji,nlay_i+1) = ze_newice(ji) * zv_newfra
            ENDDO
            ! --- Ice enthalpy remapping --- !
            CALL lim_thd_ent( 1, nbpac, ze_i_1d(1:nbpac,:,jl) ) 
         ENDDO

         !-----------------
         ! Update salinity
         !-----------------
         DO jl = 1, jpl
            DO ji = 1, nbpac
               zdv   = zv_i_1d(ji,jl) - zv_b(ji,jl)
               zsmv_i_1d(ji,jl) = zsmv_i_1d(ji,jl) + zdv * zs_newice(ji)
            END DO
         END DO

         !------------------------------------------------------------------------------!
         ! 7) Change 2D vectors to 1D vectors 
         !------------------------------------------------------------------------------!
         DO jl = 1, jpl
            CALL tab_1d_2d( nbpac, a_i (:,:,jl), npac(1:nbpac), za_i_1d (1:nbpac,jl), jpi, jpj )
            CALL tab_1d_2d( nbpac, v_i (:,:,jl), npac(1:nbpac), zv_i_1d (1:nbpac,jl), jpi, jpj )
            CALL tab_1d_2d( nbpac, smv_i (:,:,jl), npac(1:nbpac), zsmv_i_1d(1:nbpac,jl) , jpi, jpj )
            DO jk = 1, nlay_i
               CALL tab_1d_2d( nbpac, e_i(:,:,jk,jl), npac(1:nbpac), ze_i_1d(1:nbpac,jk,jl), jpi, jpj )
            END DO
         END DO
         CALL tab_1d_2d( nbpac, sfx_opw, npac(1:nbpac), sfx_opw_1d(1:nbpac), jpi, jpj )
         CALL tab_1d_2d( nbpac, wfx_opw, npac(1:nbpac), wfx_opw_1d(1:nbpac), jpi, jpj )

         CALL tab_1d_2d( nbpac, hfx_thd, npac(1:nbpac), hfx_thd_1d(1:nbpac), jpi, jpj )
         CALL tab_1d_2d( nbpac, hfx_opw, npac(1:nbpac), hfx_opw_1d(1:nbpac), jpi, jpj )
         !
      ENDIF ! nbpac > 0

      !------------------------------------------------------------------------------!
      ! 8) Change units for e_i
      !------------------------------------------------------------------------------!    
      DO jl = 1, jpl
         DO jk = 1, nlay_i
            DO jj = 1, jpj
               DO ji = 1, jpi
                  ! heat content in J/m2
                  e_i(ji,jj,jk,jl) = e_i(ji,jj,jk,jl) * v_i(ji,jj,jl) * r1_nlay_i 
               END DO
            END DO
         END DO
      END DO

      !
      CALL wrk_dealloc( jpij, jcat )   ! integer
      CALL wrk_dealloc( jpij, zswinew, zv_newice, za_newice, zh_newice, ze_newice, zs_newice, zo_newice )
      CALL wrk_dealloc( jpij, zdv_res, zda_res, zat_i_1d, zv_frazb, zvrel_1d )
      CALL wrk_dealloc( jpij,jpl, zv_b, za_b, za_i_1d, zv_i_1d, zsmv_i_1d )
      CALL wrk_dealloc( jpij,nlay_i,jpl, ze_i_1d )
      CALL wrk_dealloc( jpi,jpj, zvrel )
      !
   END SUBROUTINE lim_thd_lac

#else
   !!----------------------------------------------------------------------
   !!   Default option                               NO  LIM3 sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_thd_lac           ! Empty routine
   END SUBROUTINE lim_thd_lac
#endif

   !!======================================================================
END MODULE limthd_lac
