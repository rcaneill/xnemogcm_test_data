MODULE limitd_th
   !!======================================================================
   !!                       ***  MODULE limitd_th ***
   !!   LIM3 ice model : ice thickness distribution: Thermodynamics
   !!======================================================================
   !! History :   -   !          (W. H. Lipscomb and E.C. Hunke) CICE (c) original code
   !!            3.0  ! 2005-12  (M. Vancoppenolle) adaptation to LIM-3
   !!             -   ! 2006-06  (M. Vancoppenolle) adaptation to include salt, age
   !!             -   ! 2007-04  (M. Vancoppenolle) Mass conservation checked
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3' :                                   LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_itd_th_rem   :
   !!   lim_itd_th_reb   :
   !!   lim_itd_fitline  :
   !!   lim_itd_shiftice :
   !!----------------------------------------------------------------------
   USE dom_ice          ! LIM-3 domain
   USE par_oce          ! ocean parameters
   USE dom_oce          ! ocean domain
   USE phycst           ! physical constants (ocean directory) 
   USE thd_ice          ! LIM-3 thermodynamic variables
   USE ice              ! LIM-3 variables
   USE limvar           ! LIM-3 variables
   USE prtctl           ! Print control
   USE in_out_manager   ! I/O manager
   USE lib_mpp          ! MPP library
   USE wrk_nemo         ! work arrays
   USE lib_fortran      ! to use key_nosignedzero
   USE limcons          ! conservation tests

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_itd_th_rem
   PUBLIC   lim_itd_th_reb

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2010)
   !! $Id: limitd_th.F90 5407 2015-06-11 19:13:22Z smasson $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_itd_th_rem( klbnd, kubnd, kt )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_itd_th_rem ***
      !!
      !! ** Purpose :   computes the redistribution of ice thickness
      !!              after thermodynamic growth of ice thickness
      !!
      !! ** Method  : Linear remapping 
      !!
      !! References : W.H. Lipscomb, JGR 2001
      !!------------------------------------------------------------------
      INTEGER , INTENT (in) ::   klbnd   ! Start thickness category index point
      INTEGER , INTENT (in) ::   kubnd   ! End point on which the  the computation is applied
      INTEGER , INTENT (in) ::   kt      ! Ocean time step 
      !
      INTEGER  ::   ji, jj, jl     ! dummy loop index
      INTEGER  ::   ii, ij         ! 2D corresponding indices to ji
      INTEGER  ::   nd             ! local integer
      REAL(wp) ::   zx1, zwk1, zdh0, zetamin, zdamax   ! local scalars
      REAL(wp) ::   zx2, zwk2, zda0, zetamax           !   -      -
      REAL(wp) ::   zx3        
      CHARACTER (len = 15) :: fieldid

      INTEGER , POINTER, DIMENSION(:,:,:) ::   zdonor   ! donor category index

      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zdhice      ! ice thickness increment
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   g0          ! coefficients for fitting the line of the ITD
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   g1          ! coefficients for fitting the line of the ITD
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   hL          ! left boundary for the ITD for each thickness
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   hR          ! left boundary for the ITD for each thickness
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zht_i_b     ! old ice thickness
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   dummy_es
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zdaice, zdvice          ! local increment of ice area and volume
      REAL(wp), POINTER, DIMENSION(:)     ::   zvetamin, zvetamax      ! maximum values for etas
      INTEGER , POINTER, DIMENSION(:)     ::   nind_i, nind_j          ! compressed indices for i/j directions
      INTEGER                             ::   nbrem                   ! number of cells with ice to transfer
      REAL(wp)                            ::   zslope                  ! used to compute local thermodynamic "speeds"
      REAL(wp), POINTER, DIMENSION(:,:)   ::   zhb0, zhb1              ! category boundaries for thinnes categories
      REAL(wp), POINTER, DIMENSION(:,:)   ::   vt_i_init, vt_i_final   !  ice volume summed over categories
      REAL(wp), POINTER, DIMENSION(:,:)   ::   vt_s_init, vt_s_final   !  snow volume summed over categories
      REAL(wp), POINTER, DIMENSION(:,:)   ::   et_i_init, et_i_final   !  ice energy summed over categories
      REAL(wp), POINTER, DIMENSION(:,:)   ::   et_s_init, et_s_final   !  snow energy summed over categories
      INTEGER , POINTER, DIMENSION(:,:)   ::   zremap_flag      ! compute remapping or not ????
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zhbnew           ! new boundaries of ice categories
      !!------------------------------------------------------------------

      CALL wrk_alloc( jpi,jpj, zremap_flag )
      CALL wrk_alloc( jpi,jpj,jpl-1, zdonor )
      CALL wrk_alloc( jpi,jpj,jpl, zdhice, g0, g1, hL, hR, zht_i_b, dummy_es )
      CALL wrk_alloc( jpi,jpj,jpl-1, zdaice, zdvice )   
      CALL wrk_alloc( jpi,jpj,jpl+1, zhbnew, kkstart = 0 )   
      CALL wrk_alloc( (jpi+1)*(jpj+1), zvetamin, zvetamax )   
      CALL wrk_alloc( (jpi+1)*(jpj+1), nind_i, nind_j ) 
      CALL wrk_alloc( jpi,jpj, zhb0,zhb1,vt_i_init,vt_i_final,vt_s_init,vt_s_final,et_i_init,et_i_final,et_s_init,et_s_final )

      !!----------------------------------------------------------------------------------------------
      !! 0) Conservation checkand changes in each ice category
      !!----------------------------------------------------------------------------------------------
      IF( con_i ) THEN
         CALL lim_column_sum (jpl,   v_i, vt_i_init)
         CALL lim_column_sum (jpl,   v_s, vt_s_init)
         CALL lim_column_sum_energy (jpl, nlay_i,   e_i, et_i_init)
         dummy_es(:,:,:) = e_s(:,:,1,:)
         CALL lim_column_sum (jpl, dummy_es(:,:,:) , et_s_init)
      ENDIF

      !!----------------------------------------------------------------------------------------------
      !! 1) Compute thickness and changes in each ice category
      !!----------------------------------------------------------------------------------------------
      IF( kt == nit000 .AND. lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'lim_itd_th_rem  : Remapping the ice thickness distribution'
         WRITE(numout,*) '~~~~~~~~~~~~~~~'
         WRITE(numout,*) ' klbnd :       ', klbnd
         WRITE(numout,*) ' kubnd :       ', kubnd
      ENDIF

      zdhice(:,:,:) = 0._wp
      DO jl = klbnd, kubnd
         DO jj = 1, jpj
            DO ji = 1, jpi
               rswitch           = MAX( 0.0, SIGN( 1.0, a_i(ji,jj,jl) - epsi10 ) )     !0 if no ice and 1 if yes
               ht_i(ji,jj,jl)    = v_i(ji,jj,jl) / MAX( a_i(ji,jj,jl), epsi10 ) * rswitch
               rswitch           = MAX( 0.0, SIGN( 1.0, a_i_b(ji,jj,jl) - epsi10) )
               zht_i_b(ji,jj,jl) = v_i_b(ji,jj,jl) / MAX( a_i_b(ji,jj,jl), epsi10 ) * rswitch
               IF( a_i(ji,jj,jl) > epsi10 )   zdhice(ji,jj,jl) = ht_i(ji,jj,jl) - zht_i_b(ji,jj,jl) ! clem: useless IF statement? 
            END DO
         END DO
      END DO

      !-----------------------------------------------------------------------------------------------
      !  2) Compute fractional ice area in each grid cell
      !-----------------------------------------------------------------------------------------------
      at_i(:,:) = 0._wp
      DO jl = klbnd, kubnd
         at_i(:,:) = at_i(:,:) + a_i(:,:,jl)
      END DO

      !-----------------------------------------------------------------------------------------------
      !  3) Identify grid cells with ice
      !-----------------------------------------------------------------------------------------------
      nbrem = 0
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF ( at_i(ji,jj) > epsi10 ) THEN
               nbrem         = nbrem + 1
               nind_i(nbrem) = ji
               nind_j(nbrem) = jj
               zremap_flag(ji,jj) = 1
            ELSE
               zremap_flag(ji,jj) = 0
            ENDIF
         END DO
      END DO

      !-----------------------------------------------------------------------------------------------
      !  4) Compute new category boundaries
      !-----------------------------------------------------------------------------------------------
      !- 4.1 Compute category boundaries
      zhbnew(:,:,:) = 0._wp

      DO jl = klbnd, kubnd - 1
         DO ji = 1, nbrem
            ii = nind_i(ji)
            ij = nind_j(ji)
            !
            zhbnew(ii,ij,jl) = hi_max(jl)
            IF    ( a_i_b(ii,ij,jl) > epsi10 .AND. a_i_b(ii,ij,jl+1) > epsi10 ) THEN
               !interpolate between adjacent category growth rates
               zslope           = ( zdhice(ii,ij,jl+1) - zdhice(ii,ij,jl) ) / ( zht_i_b(ii,ij,jl+1) - zht_i_b(ii,ij,jl) )
               zhbnew(ii,ij,jl) = hi_max(jl) + zdhice(ii,ij,jl) + zslope * ( hi_max(jl) - zht_i_b(ii,ij,jl) )
            ELSEIF( a_i_b(ii,ij,jl) > epsi10) THEN
               zhbnew(ii,ij,jl) = hi_max(jl) + zdhice(ii,ij,jl)
            ELSEIF( a_i_b(ii,ij,jl+1) > epsi10) THEN
               zhbnew(ii,ij,jl) = hi_max(jl) + zdhice(ii,ij,jl+1)
            ENDIF
         END DO

         !- 4.2 Check that each zhbnew lies between adjacent values of ice thickness
         DO ji = 1, nbrem
            ii = nind_i(ji)
            ij = nind_j(ji)

            ! clem: we do not want ht_i to be too close to either HR or HL otherwise a division by nearly 0 is possible 
            ! in lim_itd_fitline in the case (HR-HL) = 3(Hice - HL) or = 3(HR - Hice)
            IF    ( a_i(ii,ij,jl  ) > epsi10 .AND. ht_i(ii,ij,jl  ) > ( zhbnew(ii,ij,jl) - epsi10 ) ) THEN
               zremap_flag(ii,ij) = 0
            ELSEIF( a_i(ii,ij,jl+1) > epsi10 .AND. ht_i(ii,ij,jl+1) < ( zhbnew(ii,ij,jl) + epsi10 ) ) THEN
               zremap_flag(ii,ij) = 0
            ENDIF

            !- 4.3 Check that each zhbnew does not exceed maximal values hi_max  
            IF( zhbnew(ii,ij,jl) < hi_max(jl-1) ) zremap_flag(ii,ij) = 0
            IF( zhbnew(ii,ij,jl) > hi_max(jl+1) ) zremap_flag(ii,ij) = 0
            ! clem bug: why is not the following instead?
            !!IF( zhbnew(ii,ij,jl) < hi_max(jl-1) ) zremap_flag(ii,ij) = 0
            !!IF( zhbnew(ii,ij,jl) > hi_max(jl  ) ) zremap_flag(ii,ij) = 0
 
         END DO

      END DO

      !-----------------------------------------------------------------------------------------------
      !  5) Identify cells where ITD is to be remapped
      !-----------------------------------------------------------------------------------------------
      nbrem = 0
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF( zremap_flag(ji,jj) == 1 ) THEN
               nbrem         = nbrem + 1
               nind_i(nbrem) = ji
               nind_j(nbrem) = jj
            ENDIF
         END DO 
      END DO 

      !-----------------------------------------------------------------------------------------------
      !  6) Fill arrays with lowermost / uppermost boundaries of 'new' categories
      !-----------------------------------------------------------------------------------------------
      DO jj = 1, jpj
         DO ji = 1, jpi
            zhb0(ji,jj) = hi_max(0)
            zhb1(ji,jj) = hi_max(1)

            IF( a_i(ji,jj,kubnd) > epsi10 ) THEN
               zhbnew(ji,jj,kubnd) = MAX( hi_max(kubnd-1), 3._wp * ht_i(ji,jj,kubnd) - 2._wp * zhbnew(ji,jj,kubnd-1) )
            ELSE
!clem bug               zhbnew(ji,jj,kubnd) = hi_max(kubnd)  
               zhbnew(ji,jj,kubnd) = hi_max(kubnd-1) ! not used anyway
            ENDIF

            ! clem: we do not want ht_i_b to be too close to either HR or HL otherwise a division by nearly 0 is possible 
            ! in lim_itd_fitline in the case (HR-HL) = 3(Hice - HL) or = 3(HR - Hice)
            IF    ( zht_i_b(ji,jj,klbnd) < ( zhb0(ji,jj) + epsi10 ) )  THEN
               zremap_flag(ji,jj) = 0
            ELSEIF( zht_i_b(ji,jj,klbnd) > ( zhb1(ji,jj) - epsi10 ) )  THEN
               zremap_flag(ji,jj) = 0
            ENDIF

         END DO
      END DO

      !-----------------------------------------------------------------------------------------------
      !  7) Compute g(h) 
      !-----------------------------------------------------------------------------------------------
      !- 7.1 g(h) for category 1 at start of time step
      CALL lim_itd_fitline( klbnd, zhb0, zhb1, zht_i_b(:,:,klbnd), g0(:,:,klbnd), g1(:,:,klbnd), hL(:,:,klbnd),   &
         &                  hR(:,:,klbnd), zremap_flag )

      !- 7.2 Area lost due to melting of thin ice (first category,  klbnd)
      DO ji = 1, nbrem
         ii = nind_i(ji) 
         ij = nind_j(ji) 

         IF( a_i(ii,ij,klbnd) > epsi10 ) THEN

            zdh0 = zdhice(ii,ij,klbnd) !decrease of ice thickness in the lower category

            IF( zdh0 < 0.0 ) THEN      !remove area from category 1
               zdh0 = MIN( -zdh0, hi_max(klbnd) )
               !Integrate g(1) from 0 to dh0 to estimate area melted
               zetamax = MIN( zdh0, hR(ii,ij,klbnd) ) - hL(ii,ij,klbnd)

               IF( zetamax > 0.0 ) THEN
                  zx1    = zetamax
                  zx2    = 0.5 * zetamax * zetamax 
                  zda0   = g1(ii,ij,klbnd) * zx2 + g0(ii,ij,klbnd) * zx1                        ! ice area removed
                  zdamax = a_i(ii,ij,klbnd) * (1.0 - ht_i(ii,ij,klbnd) / zht_i_b(ii,ij,klbnd) ) ! Constrain new thickness <= ht_i                
                  zda0   = MIN( zda0, zdamax )                                                  ! ice area lost due to melting 
                                                                                                !     of thin ice (zdamax > 0)
                  ! Remove area, conserving volume
                  ht_i(ii,ij,klbnd) = ht_i(ii,ij,klbnd) * a_i(ii,ij,klbnd) / ( a_i(ii,ij,klbnd) - zda0 )
                  a_i(ii,ij,klbnd)  = a_i(ii,ij,klbnd) - zda0
                  v_i(ii,ij,klbnd)  = a_i(ii,ij,klbnd) * ht_i(ii,ij,klbnd) ! clem-useless ?
               ENDIF

            ELSE ! if ice accretion zdh0 > 0
               ! zhbnew was 0, and is shifted to the right to account for thin ice growth in openwater (F0 = f1)
               zhbnew(ii,ij,klbnd-1) = MIN( zdh0, hi_max(klbnd) ) 
            ENDIF

         ENDIF

      END DO

      !- 7.3 g(h) for each thickness category  
      DO jl = klbnd, kubnd
         CALL lim_itd_fitline( jl, zhbnew(:,:,jl-1), zhbnew(:,:,jl), ht_i(:,:,jl),  &
            &                  g0(:,:,jl), g1(:,:,jl), hL(:,:,jl), hR(:,:,jl), zremap_flag )
      END DO

      !-----------------------------------------------------------------------------------------------
      !  8) Compute area and volume to be shifted across each boundary
      !-----------------------------------------------------------------------------------------------

      DO jl = klbnd, kubnd - 1
         DO jj = 1, jpj
            DO ji = 1, jpi
               zdonor(ji,jj,jl) = 0
               zdaice(ji,jj,jl) = 0.0
               zdvice(ji,jj,jl) = 0.0
            END DO
         END DO

         DO ji = 1, nbrem
            ii = nind_i(ji)
            ij = nind_j(ji)

            IF (zhbnew(ii,ij,jl) > hi_max(jl)) THEN ! transfer from jl to jl+1
               ! left and right integration limits in eta space
               zvetamin(ji) = MAX( hi_max(jl), hL(ii,ij,jl) ) - hL(ii,ij,jl)
               zvetamax(ji) = MIN( zhbnew(ii,ij,jl), hR(ii,ij,jl) ) - hL(ii,ij,jl)
               zdonor(ii,ij,jl) = jl

            ELSE                                    ! zhbnew(jl) <= hi_max(jl) ; transfer from jl+1 to jl
               ! left and right integration limits in eta space
               zvetamin(ji) = 0.0
               zvetamax(ji) = MIN( hi_max(jl), hR(ii,ij,jl+1) ) - hL(ii,ij,jl+1)
               zdonor(ii,ij,jl) = jl + 1

            ENDIF

            zetamax = MAX( zvetamax(ji), zvetamin(ji) ) ! no transfer if etamax < etamin
            zetamin = zvetamin(ji)

            zx1  = zetamax - zetamin
            zwk1 = zetamin * zetamin
            zwk2 = zetamax * zetamax
            zx2  = 0.5 * ( zwk2 - zwk1 )
            zwk1 = zwk1 * zetamin
            zwk2 = zwk2 * zetamax
            zx3  = 1.0 / 3.0 * ( zwk2 - zwk1 )
            nd   = zdonor(ii,ij,jl)
            zdaice(ii,ij,jl) = g1(ii,ij,nd)*zx2 + g0(ii,ij,nd)*zx1
            zdvice(ii,ij,jl) = g1(ii,ij,nd)*zx3 + g0(ii,ij,nd)*zx2 + zdaice(ii,ij,jl)*hL(ii,ij,nd)

         END DO
      END DO

      !!----------------------------------------------------------------------------------------------
      !! 9) Shift ice between categories
      !!----------------------------------------------------------------------------------------------
      CALL lim_itd_shiftice ( klbnd, kubnd, zdonor, zdaice, zdvice )

      !!----------------------------------------------------------------------------------------------
      !! 10) Make sure ht_i >= minimum ice thickness hi_min
      !!----------------------------------------------------------------------------------------------

      DO ji = 1, nbrem
         ii = nind_i(ji)
         ij = nind_j(ji)
         IF ( a_i(ii,ij,1) > epsi10 .AND. ht_i(ii,ij,1) < rn_himin ) THEN
            a_i (ii,ij,1) = a_i(ii,ij,1) * ht_i(ii,ij,1) / rn_himin 
            ht_i(ii,ij,1) = rn_himin
         ENDIF
      END DO

      !!----------------------------------------------------------------------------------------------
      !! 11) Conservation check
      !!----------------------------------------------------------------------------------------------
      IF ( con_i ) THEN
         CALL lim_column_sum (jpl,   v_i, vt_i_final)
         fieldid = ' v_i : limitd_th '
         CALL lim_cons_check (vt_i_init, vt_i_final, 1.0e-6, fieldid) 

         CALL lim_column_sum_energy (jpl, nlay_i,  e_i, et_i_final)
         fieldid = ' e_i : limitd_th '
         CALL lim_cons_check (et_i_init, et_i_final, 1.0e-3, fieldid) 

         CALL lim_column_sum (jpl,   v_s, vt_s_final)
         fieldid = ' v_s : limitd_th '
         CALL lim_cons_check (vt_s_init, vt_s_final, 1.0e-6, fieldid) 

         dummy_es(:,:,:) = e_s(:,:,1,:)
         CALL lim_column_sum (jpl, dummy_es(:,:,:) , et_s_final)
         fieldid = ' e_s : limitd_th '
         CALL lim_cons_check (et_s_init, et_s_final, 1.0e-3, fieldid) 
      ENDIF

      CALL wrk_dealloc( jpi,jpj, zremap_flag )
      CALL wrk_dealloc( jpi,jpj,jpl-1, zdonor )
      CALL wrk_dealloc( jpi,jpj,jpl, zdhice, g0, g1, hL, hR, zht_i_b, dummy_es )
      CALL wrk_dealloc( jpi,jpj,jpl-1, zdaice, zdvice )   
      CALL wrk_dealloc( jpi,jpj,jpl+1, zhbnew, kkstart = 0 )   
      CALL wrk_dealloc( (jpi+1)*(jpj+1), zvetamin, zvetamax )   
      CALL wrk_dealloc( (jpi+1)*(jpj+1), nind_i, nind_j ) 
      CALL wrk_dealloc( jpi,jpj, zhb0,zhb1,vt_i_init,vt_i_final,vt_s_init,vt_s_final,et_i_init,et_i_final,et_s_init,et_s_final )

   END SUBROUTINE lim_itd_th_rem


   SUBROUTINE lim_itd_fitline( num_cat, HbL, Hbr, hice, g0, g1, hL, hR, zremap_flag )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_itd_fitline ***
      !!
      !! ** Purpose :   fit g(h) with a line using area, volume constraints
      !!
      !! ** Method  :   Fit g(h) with a line, satisfying area and volume constraints.
      !!              To reduce roundoff errors caused by large values of g0 and g1,
      !!              we actually compute g(eta), where eta = h - hL, and hL is the
      !!              left boundary.
      !!------------------------------------------------------------------
      INTEGER                     , INTENT(in   ) ::   num_cat      ! category index
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in   ) ::   HbL, HbR     ! left and right category boundaries
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in   ) ::   hice         ! ice thickness
      REAL(wp), DIMENSION(jpi,jpj), INTENT(  out) ::   g0, g1       ! coefficients in linear equation for g(eta)
      REAL(wp), DIMENSION(jpi,jpj), INTENT(  out) ::   hL           ! min value of range over which g(h) > 0
      REAL(wp), DIMENSION(jpi,jpj), INTENT(  out) ::   hR           ! max value of range over which g(h) > 0
      INTEGER , DIMENSION(jpi,jpj), INTENT(in   ) ::   zremap_flag  !
      !
      INTEGER  ::   ji,jj        ! horizontal indices
      REAL(wp) ::   zh13         ! HbL + 1/3 * (HbR - HbL)
      REAL(wp) ::   zh23         ! HbL + 2/3 * (HbR - HbL)
      REAL(wp) ::   zdhr         ! 1 / (hR - hL)
      REAL(wp) ::   zwk1, zwk2   ! temporary variables
      !!------------------------------------------------------------------
      !
      DO jj = 1, jpj
         DO ji = 1, jpi
            !
            IF( zremap_flag(ji,jj) == 1 .AND. a_i(ji,jj,num_cat) > epsi10   &
               &                        .AND. hice(ji,jj)        > 0._wp )  THEN

               ! Initialize hL and hR
               hL(ji,jj) = HbL(ji,jj)
               hR(ji,jj) = HbR(ji,jj)

               ! Change hL or hR if hice falls outside central third of range
               zh13 = 1.0 / 3.0 * ( 2.0 * hL(ji,jj) + hR(ji,jj) )
               zh23 = 1.0 / 3.0 * ( hL(ji,jj) + 2.0 * hR(ji,jj) )

               IF    ( hice(ji,jj) < zh13 ) THEN   ;   hR(ji,jj) = 3._wp * hice(ji,jj) - 2._wp * hL(ji,jj)
               ELSEIF( hice(ji,jj) > zh23 ) THEN   ;   hL(ji,jj) = 3._wp * hice(ji,jj) - 2._wp * hR(ji,jj)
               ENDIF

               ! Compute coefficients of g(eta) = g0 + g1*eta
               zdhr = 1._wp / (hR(ji,jj) - hL(ji,jj))
               zwk1 = 6._wp * a_i(ji,jj,num_cat) * zdhr
               zwk2 = ( hice(ji,jj) - hL(ji,jj) ) * zdhr
               g0(ji,jj) = zwk1 * ( 2._wp / 3._wp - zwk2 )
               g1(ji,jj) = 2._wp * zdhr * zwk1 * ( zwk2 - 0.5 )
               !
            ELSE  ! remap_flag = .false. or a_i < epsi10 
               hL(ji,jj) = 0._wp
               hR(ji,jj) = 0._wp
               g0(ji,jj) = 0._wp
               g1(ji,jj) = 0._wp
            ENDIF
            !
         END DO
      END DO
      !
   END SUBROUTINE lim_itd_fitline


   SUBROUTINE lim_itd_shiftice( klbnd, kubnd, zdonor, zdaice, zdvice )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_itd_shiftice ***
      !!
      !! ** Purpose :   shift ice across category boundaries, conserving everything
      !!              ( area, volume, energy, age*vol, and mass of salt )
      !!
      !! ** Method  :
      !!------------------------------------------------------------------
      INTEGER                           , INTENT(in   ) ::   klbnd    ! Start thickness category index point
      INTEGER                           , INTENT(in   ) ::   kubnd    ! End point on which the  the computation is applied
      INTEGER , DIMENSION(jpi,jpj,jpl-1), INTENT(in   ) ::   zdonor   ! donor category index
      REAL(wp), DIMENSION(jpi,jpj,jpl-1), INTENT(inout) ::   zdaice   ! ice area transferred across boundary
      REAL(wp), DIMENSION(jpi,jpj,jpl-1), INTENT(inout) ::   zdvice   ! ice volume transferred across boundary

      INTEGER ::   ji, jj, jl, jl2, jl1, jk   ! dummy loop indices
      INTEGER ::   ii, ij                     ! indices when changing from 2D-1D is done

      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zaTsfn
      REAL(wp), POINTER, DIMENSION(:,:)   ::   zworka            ! temporary array used here

      REAL(wp) ::   zdvsnow, zdesnow   ! snow volume and energy transferred
      REAL(wp) ::   zdeice             ! ice energy transferred
      REAL(wp) ::   zdsm_vice          ! ice salinity times volume transferred
      REAL(wp) ::   zdo_aice           ! ice age times volume transferred
      REAL(wp) ::   zdaTsf             ! aicen*Tsfcn transferred

      INTEGER, POINTER, DIMENSION(:) ::   nind_i, nind_j   ! compressed indices for i/j directions

      INTEGER  ::   nbrem             ! number of cells with ice to transfer
      !!------------------------------------------------------------------

      CALL wrk_alloc( jpi,jpj,jpl, zaTsfn )
      CALL wrk_alloc( jpi,jpj, zworka )
      CALL wrk_alloc( (jpi+1)*(jpj+1), nind_i, nind_j )

      !----------------------------------------------------------------------------------------------
      ! 1) Define a variable equal to a_i*T_su
      !----------------------------------------------------------------------------------------------

      DO jl = klbnd, kubnd
         zaTsfn(:,:,jl) = a_i(:,:,jl) * t_su(:,:,jl)
      END DO

      !-------------------------------------------------------------------------------
      ! 2) Transfer volume and energy between categories
      !-------------------------------------------------------------------------------

      DO jl = klbnd, kubnd - 1
         nbrem = 0
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF (zdaice(ji,jj,jl) > 0.0 ) THEN ! daice(n) can be < puny
                  nbrem = nbrem + 1
                  nind_i(nbrem) = ji
                  nind_j(nbrem) = jj
               ENDIF
            END DO
         END DO

         DO ji = 1, nbrem 
            ii = nind_i(ji)
            ij = nind_j(ji)

            jl1 = zdonor(ii,ij,jl)
            rswitch       = MAX( 0._wp , SIGN( 1._wp , v_i(ii,ij,jl1) - epsi10 ) )
            zworka(ii,ij) = zdvice(ii,ij,jl) / MAX( v_i(ii,ij,jl1), epsi10 ) * rswitch
            IF( jl1 == jl) THEN   ;   jl2 = jl1+1
            ELSE                  ;   jl2 = jl 
            ENDIF

            !--------------
            ! Ice areas
            !--------------
            a_i(ii,ij,jl1) = a_i(ii,ij,jl1) - zdaice(ii,ij,jl)
            a_i(ii,ij,jl2) = a_i(ii,ij,jl2) + zdaice(ii,ij,jl)

            !--------------
            ! Ice volumes
            !--------------
            v_i(ii,ij,jl1) = v_i(ii,ij,jl1) - zdvice(ii,ij,jl) 
            v_i(ii,ij,jl2) = v_i(ii,ij,jl2) + zdvice(ii,ij,jl)

            !--------------
            ! Snow volumes
            !--------------
            zdvsnow        = v_s(ii,ij,jl1) * zworka(ii,ij)
            v_s(ii,ij,jl1) = v_s(ii,ij,jl1) - zdvsnow
            v_s(ii,ij,jl2) = v_s(ii,ij,jl2) + zdvsnow 

            !--------------------
            ! Snow heat content  
            !--------------------
            zdesnow            = e_s(ii,ij,1,jl1) * zworka(ii,ij)
            e_s(ii,ij,1,jl1)   = e_s(ii,ij,1,jl1) - zdesnow
            e_s(ii,ij,1,jl2)   = e_s(ii,ij,1,jl2) + zdesnow

            !--------------
            ! Ice age 
            !--------------
            zdo_aice           = oa_i(ii,ij,jl1) * zdaice(ii,ij,jl)
            oa_i(ii,ij,jl1)    = oa_i(ii,ij,jl1) - zdo_aice
            oa_i(ii,ij,jl2)    = oa_i(ii,ij,jl2) + zdo_aice

            !--------------
            ! Ice salinity
            !--------------
            zdsm_vice          = smv_i(ii,ij,jl1) * zworka(ii,ij)
            smv_i(ii,ij,jl1)   = smv_i(ii,ij,jl1) - zdsm_vice
            smv_i(ii,ij,jl2)   = smv_i(ii,ij,jl2) + zdsm_vice

            !---------------------
            ! Surface temperature
            !---------------------
            zdaTsf             = t_su(ii,ij,jl1) * zdaice(ii,ij,jl)
            zaTsfn(ii,ij,jl1)  = zaTsfn(ii,ij,jl1) - zdaTsf
            zaTsfn(ii,ij,jl2)  = zaTsfn(ii,ij,jl2) + zdaTsf 

         END DO

         !------------------
         ! Ice heat content
         !------------------

         DO jk = 1, nlay_i
            DO ji = 1, nbrem
               ii = nind_i(ji)
               ij = nind_j(ji)

               jl1 = zdonor(ii,ij,jl)
               IF (jl1 == jl) THEN
                  jl2 = jl+1
               ELSE             ! n1 = n+1
                  jl2 = jl 
               ENDIF

               zdeice = e_i(ii,ij,jk,jl1) * zworka(ii,ij)
               e_i(ii,ij,jk,jl1) =  e_i(ii,ij,jk,jl1) - zdeice
               e_i(ii,ij,jk,jl2) =  e_i(ii,ij,jk,jl2) + zdeice 
            END DO
         END DO

      END DO                   ! boundaries, 1 to ncat-1

      !-----------------------------------------------------------------
      ! Update ice thickness and temperature
      !-----------------------------------------------------------------

      DO jl = klbnd, kubnd
         DO jj = 1, jpj
            DO ji = 1, jpi 
               IF ( a_i(ji,jj,jl) > epsi10 ) THEN 
                  ht_i(ji,jj,jl)  =  v_i   (ji,jj,jl) / a_i(ji,jj,jl) 
                  t_su(ji,jj,jl)  =  zaTsfn(ji,jj,jl) / a_i(ji,jj,jl) 
               ELSE
                  ht_i(ji,jj,jl)  = 0._wp
                  t_su(ji,jj,jl)  = rt0
               ENDIF
            END DO
         END DO
      END DO
      !
      CALL wrk_dealloc( jpi,jpj,jpl, zaTsfn )
      CALL wrk_dealloc( jpi,jpj, zworka )
      CALL wrk_dealloc( (jpi+1)*(jpj+1), nind_i, nind_j )
      !
   END SUBROUTINE lim_itd_shiftice
   

   SUBROUTINE lim_itd_th_reb( klbnd, kubnd )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_itd_th_reb ***
      !!
      !! ** Purpose : rebin - rebins thicknesses into defined categories
      !!
      !! ** Method  :
      !!------------------------------------------------------------------
      INTEGER , INTENT (in) ::   klbnd   ! Start thickness category index point
      INTEGER , INTENT (in) ::   kubnd   ! End point on which the  the computation is applied
      !
      INTEGER ::   ji,jj, jl   ! dummy loop indices
      INTEGER ::   zshiftflag          ! = .true. if ice must be shifted
      CHARACTER (len = 15) :: fieldid

      INTEGER , POINTER, DIMENSION(:,:,:) ::   zdonor           ! donor category index
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zdaice, zdvice   ! ice area and volume transferred

      REAL(wp), POINTER, DIMENSION(:,:) ::   vt_i_init, vt_i_final   ! ice volume summed over categories
      REAL(wp), POINTER, DIMENSION(:,:) ::   vt_s_init, vt_s_final   ! snow volume summed over categories
      !!------------------------------------------------------------------
      
      CALL wrk_alloc( jpi,jpj,jpl, zdonor )   ! interger
      CALL wrk_alloc( jpi,jpj,jpl, zdaice, zdvice )
      CALL wrk_alloc( jpi,jpj, vt_i_init, vt_i_final, vt_s_init, vt_s_final )
      !     
      IF( con_i ) THEN                 ! conservation check
         CALL lim_column_sum (jpl,   v_i, vt_i_init)
         CALL lim_column_sum (jpl,   v_s, vt_s_init)
      ENDIF

      !
      !------------------------------------------------------------------------------
      ! 1) Compute ice thickness.
      !------------------------------------------------------------------------------
      DO jl = klbnd, kubnd
         DO jj = 1, jpj
            DO ji = 1, jpi 
               rswitch = MAX( 0._wp , SIGN( 1._wp, a_i(ji,jj,jl) - epsi10 ) )
               ht_i(ji,jj,jl) = v_i (ji,jj,jl) / MAX( a_i(ji,jj,jl) , epsi10 ) * rswitch
            END DO
         END DO
      END DO

      !------------------------------------------------------------------------------
      ! 2) If a category thickness is not in bounds, shift the
      ! entire area, volume, and energy to the neighboring category
      !------------------------------------------------------------------------------
      !-------------------------
      ! Initialize shift arrays
      !-------------------------
      DO jl = klbnd, kubnd
         zdonor(:,:,jl) = 0
         zdaice(:,:,jl) = 0._wp
         zdvice(:,:,jl) = 0._wp
      END DO

      !-------------------------
      ! Move thin categories up
      !-------------------------

      DO jl = klbnd, kubnd - 1  ! loop over category boundaries

         !---------------------------------------
         ! identify thicknesses that are too big
         !---------------------------------------
         zshiftflag = 0

         DO jj = 1, jpj 
            DO ji = 1, jpi 
               IF( a_i(ji,jj,jl) > epsi10 .AND. ht_i(ji,jj,jl) > hi_max(jl) ) THEN 
                  zshiftflag        = 1
                  zdonor(ji,jj,jl)  = jl 
                  ! begin TECLIM change
                  !zdaice(ji,jj,jl)  = a_i(ji,jj,jl) * 0.5_wp
                  !zdvice(ji,jj,jl)  = v_i(ji,jj,jl)-zdaice(ji,jj,jl)*(hi_max(jl)+hi_max(jl-1)) * 0.5_wp
                  ! end TECLIM change 
                  ! clem: how much of a_i you send in cat sup is somewhat arbitrary
                  zdaice(ji,jj,jl)  = a_i(ji,jj,jl) * ( ht_i(ji,jj,jl) - hi_max(jl) + epsi20 ) / ht_i(ji,jj,jl)  
                  zdvice(ji,jj,jl)  = v_i(ji,jj,jl) - ( a_i(ji,jj,jl) - zdaice(ji,jj,jl) ) * ( hi_max(jl) - epsi20 )
               ENDIF
            END DO
         END DO
         IF(lk_mpp)   CALL mpp_max( zshiftflag )

         IF( zshiftflag == 1 ) THEN            ! Shift ice between categories
            CALL lim_itd_shiftice( klbnd, kubnd, zdonor, zdaice, zdvice )
            ! Reset shift parameters
            zdonor(:,:,jl) = 0
            zdaice(:,:,jl) = 0._wp
            zdvice(:,:,jl) = 0._wp
         ENDIF
         !
      END DO

      !----------------------------
      ! Move thick categories down
      !----------------------------

      DO jl = kubnd - 1, 1, -1       ! loop over category boundaries

         !-----------------------------------------
         ! Identify thicknesses that are too small
         !-----------------------------------------
         zshiftflag = 0

         DO jj = 1, jpj
            DO ji = 1, jpi
               IF( a_i(ji,jj,jl+1) > epsi10 .AND. ht_i(ji,jj,jl+1) <= hi_max(jl) ) THEN
                  !
                  zshiftflag = 1
                  zdonor(ji,jj,jl) = jl + 1
                  zdaice(ji,jj,jl) = a_i(ji,jj,jl+1) 
                  zdvice(ji,jj,jl) = v_i(ji,jj,jl+1)
               ENDIF
            END DO
         END DO

         IF(lk_mpp)   CALL mpp_max( zshiftflag )
         
         IF( zshiftflag == 1 ) THEN            ! Shift ice between categories
            CALL lim_itd_shiftice( klbnd, kubnd, zdonor, zdaice, zdvice )
            ! Reset shift parameters
            zdonor(:,:,jl) = 0
            zdaice(:,:,jl) = 0._wp
            zdvice(:,:,jl) = 0._wp
         ENDIF

      END DO

      !------------------------------------------------------------------------------
      ! 3) Conservation check
      !------------------------------------------------------------------------------

      IF( con_i ) THEN
         CALL lim_column_sum (jpl,   v_i, vt_i_final)
         fieldid = ' v_i : limitd_reb '
         CALL lim_cons_check (vt_i_init, vt_i_final, 1.0e-6, fieldid) 

         CALL lim_column_sum (jpl,   v_s, vt_s_final)
         fieldid = ' v_s : limitd_reb '
         CALL lim_cons_check (vt_s_init, vt_s_final, 1.0e-6, fieldid) 
      ENDIF
      !
      CALL wrk_dealloc( jpi,jpj,jpl, zdonor )
      CALL wrk_dealloc( jpi,jpj,jpl, zdaice, zdvice )
      CALL wrk_dealloc( jpi,jpj, vt_i_init, vt_i_final, vt_s_init, vt_s_final )

   END SUBROUTINE lim_itd_th_reb

#else
   !!----------------------------------------------------------------------
   !!   Default option            Dummy module         NO LIM sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_itd_th_rem
   END SUBROUTINE lim_itd_th_rem
   SUBROUTINE lim_itd_fitline
   END SUBROUTINE lim_itd_fitline
   SUBROUTINE lim_itd_shiftice
   END SUBROUTINE lim_itd_shiftice
   SUBROUTINE lim_itd_th_reb
   END SUBROUTINE lim_itd_th_reb
#endif
   !!======================================================================
END MODULE limitd_th
