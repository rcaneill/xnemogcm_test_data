MODULE limthd_dif
   !!======================================================================
   !!                       ***  MODULE limthd_dif ***
   !!                       heat diffusion in sea ice 
   !!                   computation of surface and inner T  
   !!======================================================================
   !! History :  LIM  ! 02-2003 (M. Vancoppenolle) original 1D code
   !!                 ! 06-2005 (M. Vancoppenolle) 3d version
   !!                 ! 11-2006 (X Fettweis) Vectorization by Xavier
   !!                 ! 04-2007 (M. Vancoppenolle) Energy conservation
   !!            4.0  ! 2011-02 (G. Madec) dynamical allocation
   !!             -   ! 2012-05 (C. Rousset) add penetration solar flux
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3'                                      LIM3 sea-ice model
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean parameters
   USE phycst         ! physical constants (ocean directory) 
   USE ice            ! LIM-3 variables
   USE thd_ice        ! LIM-3: thermodynamics
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE wrk_nemo       ! work arrays
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)  

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_thd_dif   ! called by lim_thd

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: limthd_dif.F90 7607 2017-01-25 15:37:31Z cetlod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_thd_dif( kideb , kiut )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_thd_dif  ***
      !! ** Purpose :
      !!           This routine determines the time evolution of snow and sea-ice 
      !!           temperature profiles.
      !! ** Method  :
      !!           This is done by solving the heat equation diffusion with
      !!           a Neumann boundary condition at the surface and a Dirichlet one
      !!           at the bottom. Solar radiation is partially absorbed into the ice.
      !!           The specific heat and thermal conductivities depend on ice salinity
      !!           and temperature to take into account brine pocket melting. The 
      !!           numerical
      !!           scheme is an iterative Crank-Nicolson on a non-uniform multilayer grid 
      !!           in the ice and snow system.
      !!
      !!           The successive steps of this routine are
      !!           1.  Thermal conductivity at the interfaces of the ice layers
      !!           2.  Internal absorbed radiation
      !!           3.  Scale factors due to non-uniform grid
      !!           4.  Kappa factors
      !!           Then iterative procedure begins
      !!           5.  specific heat in the ice
      !!           6.  eta factors
      !!           7.  surface flux computation
      !!           8.  tridiagonal system terms
      !!           9.  solving the tridiagonal system with Gauss elimination
      !!           Iterative procedure ends according to a criterion on evolution
      !!           of temperature
      !!
      !! ** Arguments :
      !!           kideb , kiut : Starting and ending points on which the 
      !!                         the computation is applied
      !!
      !! ** Inputs / Ouputs : (global commons)
      !!           surface temperature : t_su_1d
      !!           ice/snow temperatures   : t_i_1d, t_s_1d
      !!           ice salinities          : s_i_1d
      !!           number of layers in the ice/snow: nlay_i, nlay_s
      !!           profile of the ice/snow layers : z_i, z_s
      !!           total ice/snow thickness : ht_i_1d, ht_s_1d
      !!
      !! ** External : 
      !!
      !! ** References :
      !!
      !! ** History :
      !!           (02-2003) Martin Vancoppenolle, Louvain-la-Neuve, Belgium
      !!           (06-2005) Martin Vancoppenolle, 3d version
      !!           (11-2006) Vectorized by Xavier Fettweis (UCL-ASTR)
      !!           (04-2007) Energy conservation tested by M. Vancoppenolle
      !!------------------------------------------------------------------
      INTEGER , INTENT(in) ::   kideb, kiut   ! Start/End point on which the  the computation is applied

      !! * Local variables
      INTEGER ::   ji          ! spatial loop index
      INTEGER ::   ii, ij      ! temporary dummy loop index
      INTEGER ::   numeq       ! current reference number of equation
      INTEGER ::   jk       ! vertical dummy loop index 
      INTEGER ::   nconv       ! number of iterations in iterative procedure
      INTEGER ::   minnumeqmin, maxnumeqmax
      
      INTEGER, POINTER, DIMENSION(:) ::   numeqmin   ! reference number of top equation
      INTEGER, POINTER, DIMENSION(:) ::   numeqmax   ! reference number of bottom equation
      
      REAL(wp) ::   zg1s      =  2._wp        ! for the tridiagonal system
      REAL(wp) ::   zg1       =  2._wp        !
      REAL(wp) ::   zgamma    =  18009._wp    ! for specific heat
      REAL(wp) ::   zbeta     =  0.117_wp     ! for thermal conductivity (could be 0.13)
      REAL(wp) ::   zraext_s  =  10._wp       ! extinction coefficient of radiation in the snow
      REAL(wp) ::   zkimin    =  0.10_wp      ! minimum ice thermal conductivity
      REAL(wp) ::   ztsu_err  =  1.e-5_wp     ! range around which t_su is considered as 0°C 
      REAL(wp) ::   ztmelt_i    ! ice melting temperature
      REAL(wp) ::   zerritmax   ! current maximal error on temperature 
      REAL(wp) ::   zhsu
      
      REAL(wp), POINTER, DIMENSION(:)     ::   isnow       ! switch for presence (1) or absence (0) of snow
      REAL(wp), POINTER, DIMENSION(:)     ::   ztsub       ! old surface temperature (before the iterative procedure )
      REAL(wp), POINTER, DIMENSION(:)     ::   ztsubit     ! surface temperature at previous iteration
      REAL(wp), POINTER, DIMENSION(:)     ::   zh_i        ! ice layer thickness
      REAL(wp), POINTER, DIMENSION(:)     ::   zh_s        ! snow layer thickness
      REAL(wp), POINTER, DIMENSION(:)     ::   zfsw        ! solar radiation absorbed at the surface
      REAL(wp), POINTER, DIMENSION(:)     ::   zqns_ice_b  ! solar radiation absorbed at the surface
      REAL(wp), POINTER, DIMENSION(:)     ::   zf          ! surface flux function
      REAL(wp), POINTER, DIMENSION(:)     ::   dzf         ! derivative of the surface flux function
      REAL(wp), POINTER, DIMENSION(:)     ::   zerrit      ! current error on temperature
      REAL(wp), POINTER, DIMENSION(:)     ::   zdifcase    ! case of the equation resolution (1->4)
      REAL(wp), POINTER, DIMENSION(:)     ::   zftrice     ! solar radiation transmitted through the ice
      REAL(wp), POINTER, DIMENSION(:)     ::   zihic
      
      REAL(wp), POINTER, DIMENSION(:,:)   ::   ztcond_i    ! Ice thermal conductivity
      REAL(wp), POINTER, DIMENSION(:,:)   ::   zradtr_i    ! Radiation transmitted through the ice
      REAL(wp), POINTER, DIMENSION(:,:)   ::   zradab_i    ! Radiation absorbed in the ice
      REAL(wp), POINTER, DIMENSION(:,:)   ::   zkappa_i    ! Kappa factor in the ice
      REAL(wp), POINTER, DIMENSION(:,:)   ::   ztib        ! Old temperature in the ice
      REAL(wp), POINTER, DIMENSION(:,:)   ::   zeta_i      ! Eta factor in the ice
      REAL(wp), POINTER, DIMENSION(:,:)   ::   ztitemp     ! Temporary temperature in the ice to check the convergence
      REAL(wp), POINTER, DIMENSION(:,:)   ::   zspeche_i   ! Ice specific heat
      REAL(wp), POINTER, DIMENSION(:,:)   ::   z_i         ! Vertical cotes of the layers in the ice
      REAL(wp), POINTER, DIMENSION(:,:)   ::   zradtr_s    ! Radiation transmited through the snow
      REAL(wp), POINTER, DIMENSION(:,:)   ::   zradab_s    ! Radiation absorbed in the snow
      REAL(wp), POINTER, DIMENSION(:,:)   ::   zkappa_s    ! Kappa factor in the snow
      REAL(wp), POINTER, DIMENSION(:,:)   ::   zeta_s      ! Eta factor in the snow
      REAL(wp), POINTER, DIMENSION(:,:)   ::   ztstemp     ! Temporary temperature in the snow to check the convergence
      REAL(wp), POINTER, DIMENSION(:,:)   ::   ztsb        ! Temporary temperature in the snow
      REAL(wp), POINTER, DIMENSION(:,:)   ::   z_s         ! Vertical cotes of the layers in the snow
      REAL(wp), POINTER, DIMENSION(:,:)   ::   zindterm    ! 'Ind'ependent term
      REAL(wp), POINTER, DIMENSION(:,:)   ::   zindtbis    ! Temporary 'ind'ependent term
      REAL(wp), POINTER, DIMENSION(:,:)   ::   zdiagbis    ! Temporary 'dia'gonal term
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   ztrid       ! Tridiagonal system terms
      
      ! diag errors on heat
      REAL(wp), POINTER, DIMENSION(:)     :: zdq, zq_ini, zhfx_err
      
      ! Mono-category
      REAL(wp)                            :: zepsilon      ! determines thres. above which computation of G(h) is done
      REAL(wp)                            :: zratio_s      ! dummy factor
      REAL(wp)                            :: zratio_i      ! dummy factor
      REAL(wp)                            :: zh_thres      ! thickness thres. for G(h) computation
      REAL(wp)                            :: zhe           ! dummy factor
      REAL(wp)                            :: zkimean       ! mean sea ice thermal conductivity
      REAL(wp)                            :: zfac          ! dummy factor
      REAL(wp)                            :: zihe          ! dummy factor
      REAL(wp)                            :: zheshth       ! dummy factor
      
      REAL(wp), POINTER, DIMENSION(:)     :: zghe          ! G(he), th. conduct enhancement factor, mono-cat
      
      !!------------------------------------------------------------------     
      ! 
      CALL wrk_alloc( jpij, numeqmin, numeqmax )
      CALL wrk_alloc( jpij, isnow, ztsub, ztsubit, zh_i, zh_s, zfsw )
      CALL wrk_alloc( jpij, zf, dzf, zqns_ice_b, zerrit, zdifcase, zftrice, zihic, zghe )
      CALL wrk_alloc( jpij,nlay_i+1, ztcond_i, zradtr_i, zradab_i, zkappa_i, ztib, zeta_i, ztitemp, z_i, zspeche_i, kjstart=0 )
      CALL wrk_alloc( jpij,nlay_s+1,           zradtr_s, zradab_s, zkappa_s, ztsb, zeta_s, ztstemp, z_s, kjstart=0 )
      CALL wrk_alloc( jpij,nlay_i+3, zindterm, zindtbis, zdiagbis  )
      CALL wrk_alloc( jpij,nlay_i+3,3, ztrid )

      CALL wrk_alloc( jpij, zdq, zq_ini, zhfx_err )

      ! --- diag error on heat diffusion - PART 1 --- !
      zdq(:) = 0._wp ; zq_ini(:) = 0._wp      
      DO ji = kideb, kiut
         zq_ini(ji) = ( SUM( q_i_1d(ji,1:nlay_i) ) * ht_i_1d(ji) * r1_nlay_i +  &
            &           SUM( q_s_1d(ji,1:nlay_s) ) * ht_s_1d(ji) * r1_nlay_s ) 
      END DO

      !------------------------------------------------------------------------------!
      ! 1) Initialization                                                            !
      !------------------------------------------------------------------------------!
      DO ji = kideb , kiut
         isnow(ji)= 1._wp - MAX( 0._wp , SIGN(1._wp, - ht_s_1d(ji) ) )  ! is there snow or not
         ! layer thickness
         zh_i(ji) = ht_i_1d(ji) * r1_nlay_i
         zh_s(ji) = ht_s_1d(ji) * r1_nlay_s
      END DO

      !--------------------
      ! Ice / snow layers
      !--------------------

      z_s(:,0) = 0._wp   ! vert. coord. of the up. lim. of the 1st snow layer
      z_i(:,0) = 0._wp   ! vert. coord. of the up. lim. of the 1st ice layer

      DO jk = 1, nlay_s            ! vert. coord of the up. lim. of the layer-th snow layer
         DO ji = kideb , kiut
            z_s(ji,jk) = z_s(ji,jk-1) + ht_s_1d(ji) * r1_nlay_s
         END DO
      END DO

      DO jk = 1, nlay_i            ! vert. coord of the up. lim. of the layer-th ice layer
         DO ji = kideb , kiut
            z_i(ji,jk) = z_i(ji,jk-1) + ht_i_1d(ji) * r1_nlay_i
         END DO
      END DO
      !
      !------------------------------------------------------------------------------|
      ! 2) Radiation								                                 |
      !------------------------------------------------------------------------------|
      !
      !-------------------
      ! Computation of i0
      !-------------------
      ! i0 describes the fraction of solar radiation which does not contribute
      ! to the surface energy budget but rather penetrates inside the ice.
      ! We assume that no radiation is transmitted through the snow
      ! If there is no no snow
      ! zfsw    = (1-i0).qsr_ice   is absorbed at the surface 
      ! zftrice = io.qsr_ice       is below the surface 
      ! ftr_ice = io.qsr_ice.exp(-k(h_i)) transmitted below the ice 
      ! fr1_i0_1d = i0 for a thin ice cover, fr1_i0_2d = i0 for a thick ice cover
      zhsu = 0.1_wp ! threshold for the computation of i0
      DO ji = kideb , kiut
         ! switches
         isnow(ji) = 1._wp - MAX( 0._wp , SIGN( 1._wp , - ht_s_1d(ji) ) ) 
         ! hs > 0, isnow = 1
         zihic(ji) = MAX( 0._wp , 1._wp - ( ht_i_1d(ji) / zhsu ) )     

         i0(ji)    = ( 1._wp - isnow(ji) ) * ( fr1_i0_1d(ji) + zihic(ji) * fr2_i0_1d(ji) )
      END DO

      !-------------------------------------------------------
      ! Solar radiation absorbed / transmitted at the surface
      ! Derivative of the non solar flux
      !-------------------------------------------------------
      DO ji = kideb , kiut
         zfsw   (ji)    =  qsr_ice_1d(ji) * ( 1 - i0(ji) )   ! Shortwave radiation absorbed at surface
         zftrice(ji)    =  qsr_ice_1d(ji) *       i0(ji)     ! Solar radiation transmitted below the surface layer
         dzf    (ji)    = dqns_ice_1d(ji)                    ! derivative of incoming nonsolar flux 
         zqns_ice_b(ji) = qns_ice_1d(ji)                     ! store previous qns_ice_1d value
      END DO

      !---------------------------------------------------------
      ! Transmission - absorption of solar radiation in the ice
      !---------------------------------------------------------

      DO ji = kideb, kiut           ! snow initialization
         zradtr_s(ji,0) = zftrice(ji)     ! radiation penetrating through snow
      END DO

      DO jk = 1, nlay_s          ! Radiation through snow
         DO ji = kideb, kiut
            !                             ! radiation transmitted below the layer-th snow layer
            zradtr_s(ji,jk) = zradtr_s(ji,0) * EXP( - zraext_s * ( MAX ( 0._wp , z_s(ji,jk) ) ) )
            !                             ! radiation absorbed by the layer-th snow layer
            zradab_s(ji,jk) = zradtr_s(ji,jk-1) - zradtr_s(ji,jk)
         END DO
      END DO

      DO ji = kideb, kiut           ! ice initialization
         zradtr_i(ji,0) = zradtr_s(ji,nlay_s) * isnow(ji) + zftrice(ji) * ( 1._wp - isnow(ji) )
      END DO

      DO jk = 1, nlay_i          ! Radiation through ice
         DO ji = kideb, kiut
            !                             ! radiation transmitted below the layer-th ice layer
            zradtr_i(ji,jk) = zradtr_i(ji,0) * EXP( - rn_kappa_i * ( MAX ( 0._wp , z_i(ji,jk) ) ) )
            !                             ! radiation absorbed by the layer-th ice layer
            zradab_i(ji,jk) = zradtr_i(ji,jk-1) - zradtr_i(ji,jk)
         END DO
      END DO

      DO ji = kideb, kiut           ! Radiation transmitted below the ice
         ftr_ice_1d(ji) = zradtr_i(ji,nlay_i) 
      END DO

      !------------------------------------------------------------------------------|
      !  3) Iterative procedure begins                                               |
      !------------------------------------------------------------------------------|
      !
      DO ji = kideb, kiut        ! Old surface temperature
         ztsub  (ji) =  t_su_1d(ji)                              ! temperature at the beg of iter pr.
         ztsubit(ji) =  t_su_1d(ji)                              ! temperature at the previous iter
         t_su_1d(ji) =  MIN( t_su_1d(ji), rt0 - ztsu_err )       ! necessary
         zerrit (ji) =  1000._wp                                 ! initial value of error
      END DO

      DO jk = 1, nlay_s       ! Old snow temperature
         DO ji = kideb , kiut
            ztsb(ji,jk) =  t_s_1d(ji,jk)
         END DO
      END DO

      DO jk = 1, nlay_i       ! Old ice temperature
         DO ji = kideb , kiut
            ztib(ji,jk) =  t_i_1d(ji,jk)
         END DO
      END DO

      nconv     =  0           ! number of iterations
      zerritmax =  1000._wp    ! maximal value of error on all points

      DO WHILE ( zerritmax > rn_terr_dif .AND. nconv < nn_conv_dif )
         !
         nconv = nconv + 1
         !
         !------------------------------------------------------------------------------|
         ! 4) Sea ice thermal conductivity                                              |
         !------------------------------------------------------------------------------|
         !
         IF( nn_ice_thcon == 0 ) THEN      ! Untersteiner (1964) formula
            DO ji = kideb , kiut
               ztcond_i(ji,0) = rcdic + zbeta * s_i_1d(ji,1) / MIN( -epsi10, t_i_1d(ji,1) - rt0 )
               ztcond_i(ji,0) = MAX( ztcond_i(ji,0), zkimin )
            END DO
            DO jk = 1, nlay_i-1
               DO ji = kideb , kiut
                  ztcond_i(ji,jk) = rcdic + zbeta * ( s_i_1d(ji,jk) + s_i_1d(ji,jk+1) ) /  &
                     MIN(-2.0_wp * epsi10, t_i_1d(ji,jk) + t_i_1d(ji,jk+1) - 2.0_wp * rt0)
                  ztcond_i(ji,jk) = MAX( ztcond_i(ji,jk), zkimin )
               END DO
            END DO
         ENDIF

         IF( nn_ice_thcon == 1 ) THEN      ! Pringle et al formula included: 2.11 + 0.09 S/T - 0.011.T
            DO ji = kideb , kiut
               ztcond_i(ji,0) = rcdic + 0.090_wp * s_i_1d(ji,1) / MIN( -epsi10, t_i_1d(ji,1) - rt0 )   &
                  &                   - 0.011_wp * ( t_i_1d(ji,1) - rt0 )  
               ztcond_i(ji,0) = MAX( ztcond_i(ji,0), zkimin )
            END DO
            DO jk = 1, nlay_i-1
               DO ji = kideb , kiut
                  ztcond_i(ji,jk) = rcdic +                                                                       & 
                     &                 0.09_wp * ( s_i_1d(ji,jk) + s_i_1d(ji,jk+1) )                              &
                     &                 / MIN( -2._wp * epsi10, t_i_1d(ji,jk) + t_i_1d(ji,jk+1) - 2.0_wp * rt0 )   &
                     &               - 0.0055_wp * ( t_i_1d(ji,jk) + t_i_1d(ji,jk+1) - 2.0 * rt0 )  
                  ztcond_i(ji,jk) = MAX( ztcond_i(ji,jk), zkimin )
               END DO
            END DO
            DO ji = kideb , kiut
               ztcond_i(ji,nlay_i) = rcdic + 0.090_wp * s_i_1d(ji,nlay_i) / MIN( -epsi10, t_bo_1d(ji) - rt0 )   &
                  &                        - 0.011_wp * ( t_bo_1d(ji) - rt0 )  
               ztcond_i(ji,nlay_i) = MAX( ztcond_i(ji,nlay_i), zkimin )
            END DO
         ENDIF
         
         !
         !------------------------------------------------------------------------------|
         !  5) G(he) - enhancement of thermal conductivity in mono-category case        |
         !------------------------------------------------------------------------------|
         !
         ! Computation of effective thermal conductivity G(h)
         ! Used in mono-category case only to simulate an ITD implicitly
         ! Fichefet and Morales Maqueda, JGR 1997

         zghe(:) = 1._wp

         SELECT CASE ( nn_monocat )

         CASE (1,3) ! LIM3

            zepsilon = 0.1_wp
            zh_thres = EXP( 1._wp ) * zepsilon * 0.5_wp

            DO ji = kideb, kiut
   
               ! Mean sea ice thermal conductivity
               zkimean  = SUM( ztcond_i(ji,0:nlay_i) ) / REAL( nlay_i+1, wp )

               ! Effective thickness he (zhe)
               zfac     = 1._wp / ( rn_cdsn + zkimean )
               zratio_s = rn_cdsn   * zfac
               zratio_i = zkimean * zfac
               zhe      = zratio_s * ht_i_1d(ji) + zratio_i * ht_s_1d(ji)

               ! G(he)
               rswitch  = MAX( 0._wp , SIGN( 1._wp , zhe - zh_thres ) )  ! =0 if zhe < zh_thres, if >
               zghe(ji) = ( 1._wp - rswitch ) + rswitch * 0.5_wp * ( 1._wp + LOG( 2._wp * zhe / zepsilon ) )
   
               ! Impose G(he) < 2.
               zghe(ji) = MIN( zghe(ji), 2._wp )

            END DO

         END SELECT

         !
         !------------------------------------------------------------------------------|
         !  6) kappa factors                                                            |
         !------------------------------------------------------------------------------|
         !
         !--- Snow
         DO ji = kideb, kiut
            zfac                  =  1. / MAX( epsi10 , zh_s(ji) )
            zkappa_s(ji,0)        = zghe(ji) * rn_cdsn * zfac
            zkappa_s(ji,nlay_s)   = zghe(ji) * rn_cdsn * zfac
         END DO

         DO jk = 1, nlay_s-1
            DO ji = kideb , kiut
               zkappa_s(ji,jk)    = zghe(ji) * 2.0 * rn_cdsn / MAX( epsi10, 2.0 * zh_s(ji) )
            END DO
         END DO

         !--- Ice
         DO jk = 1, nlay_i-1
            DO ji = kideb , kiut
               zkappa_i(ji,jk)    = zghe(ji) * 2.0 * ztcond_i(ji,jk) / MAX( epsi10 , 2.0 * zh_i(ji) )
            END DO
         END DO

         !--- Snow-ice interface
         DO ji = kideb , kiut
            zfac                  = 1./ MAX( epsi10 , zh_i(ji) )
            zkappa_i(ji,0)        = zghe(ji) * ztcond_i(ji,0) * zfac
            zkappa_i(ji,nlay_i)   = zghe(ji) * ztcond_i(ji,nlay_i) * zfac
            zkappa_s(ji,nlay_s)   = zghe(ji) * zghe(ji) * 2.0 * rn_cdsn * ztcond_i(ji,0) / & 
           &                        MAX( epsi10, ( zghe(ji) * ztcond_i(ji,0) * zh_s(ji) + zghe(ji) * rn_cdsn * zh_i(ji) ) )
            zkappa_i(ji,0)        = zkappa_s(ji,nlay_s) * isnow(ji) + zkappa_i(ji,0) * ( 1._wp - isnow(ji) )
         END DO

         !
         !------------------------------------------------------------------------------|
         ! 7) Sea ice specific heat, eta factors                                        |
         !------------------------------------------------------------------------------|
         !
         DO jk = 1, nlay_i
            DO ji = kideb , kiut
               ztitemp(ji,jk)   = t_i_1d(ji,jk)
               zspeche_i(ji,jk) = cpic + zgamma * s_i_1d(ji,jk) / MAX( ( t_i_1d(ji,jk) - rt0 ) * ( ztib(ji,jk) - rt0 ), epsi10 )
               zeta_i(ji,jk)    = rdt_ice / MAX( rhoic * zspeche_i(ji,jk) * zh_i(ji), epsi10 )
            END DO
         END DO

         DO jk = 1, nlay_s
            DO ji = kideb , kiut
               ztstemp(ji,jk) = t_s_1d(ji,jk)
               zeta_s(ji,jk)  = rdt_ice / MAX( rhosn * cpic * zh_s(ji), epsi10 )
            END DO
         END DO

         !
         !------------------------------------------------------------------------------|
         ! 8) surface flux computation                                                  |
         !------------------------------------------------------------------------------|
         !
         IF ( ln_it_qnsice ) THEN 
            DO ji = kideb , kiut
               ! update of the non solar flux according to the update in T_su
               qns_ice_1d(ji) = qns_ice_1d(ji) + dqns_ice_1d(ji) * ( t_su_1d(ji) - ztsubit(ji) )
            END DO
         ENDIF

         ! Update incoming flux
         DO ji = kideb , kiut
            ! update incoming flux
            zf(ji)    =          zfsw(ji)              & ! net absorbed solar radiation
               &         + qns_ice_1d(ji)                ! non solar total flux (LWup, LWdw, SH, LH)
         END DO

         !
         !------------------------------------------------------------------------------|
         ! 9) tridiagonal system terms                                                  |
         !------------------------------------------------------------------------------|
         !
         !!layer denotes the number of the layer in the snow or in the ice
         !!numeq denotes the reference number of the equation in the tridiagonal
         !!system, terms of tridiagonal system are indexed as following :
         !!1 is subdiagonal term, 2 is diagonal and 3 is superdiagonal one

         !!ice interior terms (top equation has the same form as the others)

         DO numeq=1,nlay_i+3
            DO ji = kideb , kiut
               ztrid(ji,numeq,1) = 0.
               ztrid(ji,numeq,2) = 0.
               ztrid(ji,numeq,3) = 0.
               zindterm(ji,numeq)= 0.
               zindtbis(ji,numeq)= 0.
               zdiagbis(ji,numeq)= 0.
            ENDDO
         ENDDO

         DO numeq = nlay_s + 2, nlay_s + nlay_i 
            DO ji = kideb , kiut
               jk                 = numeq - nlay_s - 1
               ztrid(ji,numeq,1)  =  - zeta_i(ji,jk) * zkappa_i(ji,jk-1)
               ztrid(ji,numeq,2)  =  1.0 + zeta_i(ji,jk) * ( zkappa_i(ji,jk-1) + zkappa_i(ji,jk) )
               ztrid(ji,numeq,3)  =  - zeta_i(ji,jk) * zkappa_i(ji,jk)
               zindterm(ji,numeq) =  ztib(ji,jk) + zeta_i(ji,jk) * zradab_i(ji,jk)
            END DO
         ENDDO

         numeq =  nlay_s + nlay_i + 1
         DO ji = kideb , kiut
            !!ice bottom term
            ztrid(ji,numeq,1)  =  - zeta_i(ji,nlay_i)*zkappa_i(ji,nlay_i-1)   
            ztrid(ji,numeq,2)  =  1.0 + zeta_i(ji,nlay_i) * ( zkappa_i(ji,nlay_i) * zg1 + zkappa_i(ji,nlay_i-1) )
            ztrid(ji,numeq,3)  =  0.0
            zindterm(ji,numeq) =  ztib(ji,nlay_i) + zeta_i(ji,nlay_i) *  &
               &                  ( zradab_i(ji,nlay_i) + zkappa_i(ji,nlay_i) * zg1 *  t_bo_1d(ji) ) 
         ENDDO


         DO ji = kideb , kiut
            IF ( ht_s_1d(ji) > 0.0 ) THEN
               !
               !------------------------------------------------------------------------------|
               !  snow-covered cells                                                          |
               !------------------------------------------------------------------------------|
               !
               !!snow interior terms (bottom equation has the same form as the others)
               DO numeq = 3, nlay_s + 1
                  jk                  =  numeq - 1
                  ztrid(ji,numeq,1)   =  - zeta_s(ji,jk) * zkappa_s(ji,jk-1)
                  ztrid(ji,numeq,2)   =  1.0 + zeta_s(ji,jk) * ( zkappa_s(ji,jk-1) + zkappa_s(ji,jk) )
                  ztrid(ji,numeq,3)   =  - zeta_s(ji,jk)*zkappa_s(ji,jk)
                  zindterm(ji,numeq)  =  ztsb(ji,jk) + zeta_s(ji,jk) * zradab_s(ji,jk)
               END DO

               !!case of only one layer in the ice (ice equation is altered)
               IF ( nlay_i.eq.1 ) THEN
                  ztrid(ji,nlay_s+2,3)    =  0.0
                  zindterm(ji,nlay_s+2)   =  zindterm(ji,nlay_s+2) + zkappa_i(ji,1) * t_bo_1d(ji) 
               ENDIF

               IF ( t_su_1d(ji) < rt0 ) THEN

                  !------------------------------------------------------------------------------|
                  !  case 1 : no surface melting - snow present                                  |
                  !------------------------------------------------------------------------------|
                  zdifcase(ji)    =  1.0
                  numeqmin(ji)    =  1
                  numeqmax(ji)    =  nlay_i + nlay_s + 1

                  !!surface equation
                  ztrid(ji,1,1)  = 0.0
                  ztrid(ji,1,2)  = dzf(ji) - zg1s * zkappa_s(ji,0)
                  ztrid(ji,1,3)  = zg1s * zkappa_s(ji,0)
                  zindterm(ji,1) = dzf(ji) * t_su_1d(ji) - zf(ji)

                  !!first layer of snow equation
                  ztrid(ji,2,1)  =  - zkappa_s(ji,0) * zg1s * zeta_s(ji,1)
                  ztrid(ji,2,2)  =  1.0 + zeta_s(ji,1) * ( zkappa_s(ji,1) + zkappa_s(ji,0) * zg1s )
                  ztrid(ji,2,3)  =  - zeta_s(ji,1)* zkappa_s(ji,1)
                  zindterm(ji,2) =  ztsb(ji,1) + zeta_s(ji,1) * zradab_s(ji,1)

               ELSE 
                  !
                  !------------------------------------------------------------------------------|
                  !  case 2 : surface is melting - snow present                                  |
                  !------------------------------------------------------------------------------|
                  !
                  zdifcase(ji)    =  2.0
                  numeqmin(ji)    =  2
                  numeqmax(ji)    =  nlay_i + nlay_s + 1

                  !!first layer of snow equation
                  ztrid(ji,2,1)  =  0.0
                  ztrid(ji,2,2)  =  1.0 + zeta_s(ji,1) * ( zkappa_s(ji,1) + zkappa_s(ji,0) * zg1s )
                  ztrid(ji,2,3)  =  - zeta_s(ji,1)*zkappa_s(ji,1) 
                  zindterm(ji,2) = ztsb(ji,1) + zeta_s(ji,1) *  &
                     &             ( zradab_s(ji,1) + zkappa_s(ji,0) * zg1s * t_su_1d(ji) ) 
               ENDIF
            ELSE
               !
               !------------------------------------------------------------------------------|
               !  cells without snow                                                          |
               !------------------------------------------------------------------------------|
               !
               IF ( t_su_1d(ji) < rt0 ) THEN
                  !
                  !------------------------------------------------------------------------------|
                  !  case 3 : no surface melting - no snow                                       |
                  !------------------------------------------------------------------------------|
                  !
                  zdifcase(ji)      =  3.0
                  numeqmin(ji)      =  nlay_s + 1
                  numeqmax(ji)      =  nlay_i + nlay_s + 1

                  !!surface equation	
                  ztrid(ji,numeqmin(ji),1)   =  0.0
                  ztrid(ji,numeqmin(ji),2)   =  dzf(ji) - zkappa_i(ji,0)*zg1    
                  ztrid(ji,numeqmin(ji),3)   =  zkappa_i(ji,0)*zg1
                  zindterm(ji,numeqmin(ji))  =  dzf(ji)*t_su_1d(ji) - zf(ji)

                  !!first layer of ice equation
                  ztrid(ji,numeqmin(ji)+1,1) =  - zkappa_i(ji,0) * zg1 * zeta_i(ji,1)
                  ztrid(ji,numeqmin(ji)+1,2) =  1.0 + zeta_i(ji,1) * ( zkappa_i(ji,1) + zkappa_i(ji,0) * zg1 )
                  ztrid(ji,numeqmin(ji)+1,3) =  - zeta_i(ji,1) * zkappa_i(ji,1)  
                  zindterm(ji,numeqmin(ji)+1)=  ztib(ji,1) + zeta_i(ji,1) * zradab_i(ji,1)  

                  !!case of only one layer in the ice (surface & ice equations are altered)

                  IF ( nlay_i == 1 ) THEN
                     ztrid(ji,numeqmin(ji),1)    =  0.0
                     ztrid(ji,numeqmin(ji),2)    =  dzf(ji) - zkappa_i(ji,0) * 2.0
                     ztrid(ji,numeqmin(ji),3)    =  zkappa_i(ji,0) * 2.0
                     ztrid(ji,numeqmin(ji)+1,1)  =  -zkappa_i(ji,0) * 2.0 * zeta_i(ji,1)
                     ztrid(ji,numeqmin(ji)+1,2)  =  1.0 + zeta_i(ji,1) * ( zkappa_i(ji,0) * 2.0 + zkappa_i(ji,1) )
                     ztrid(ji,numeqmin(ji)+1,3)  =  0.0

                     zindterm(ji,numeqmin(ji)+1) =  ztib(ji,1) + zeta_i(ji,1) *  &
                        &                          ( zradab_i(ji,1) + zkappa_i(ji,1) * t_bo_1d(ji) )
                  ENDIF

               ELSE

                  !
                  !------------------------------------------------------------------------------|
                  ! case 4 : surface is melting - no snow                                        |
                  !------------------------------------------------------------------------------|
                  !
                  zdifcase(ji)    =  4.0
                  numeqmin(ji)    =  nlay_s + 2
                  numeqmax(ji)    =  nlay_i + nlay_s + 1

                  !!first layer of ice equation
                  ztrid(ji,numeqmin(ji),1)      =  0.0
                  ztrid(ji,numeqmin(ji),2)      =  1.0 + zeta_i(ji,1) * ( zkappa_i(ji,1) + zkappa_i(ji,0) * zg1 )  
                  ztrid(ji,numeqmin(ji),3)      =  - zeta_i(ji,1) * zkappa_i(ji,1)
                  zindterm(ji,numeqmin(ji))     =  ztib(ji,1) + zeta_i(ji,1) *  &
                     &                             ( zradab_i(ji,1) + zkappa_i(ji,0) * zg1 * t_su_1d(ji) ) 

                  !!case of only one layer in the ice (surface & ice equations are altered)
                  IF ( nlay_i == 1 ) THEN
                     ztrid(ji,numeqmin(ji),1)  =  0.0
                     ztrid(ji,numeqmin(ji),2)  =  1.0 + zeta_i(ji,1) * ( zkappa_i(ji,0) * 2.0 + zkappa_i(ji,1) )
                     ztrid(ji,numeqmin(ji),3)  =  0.0
                     zindterm(ji,numeqmin(ji)) =  ztib(ji,1) + zeta_i(ji,1) * ( zradab_i(ji,1) + zkappa_i(ji,1) * t_bo_1d(ji) )  &
                        &                       + t_su_1d(ji) * zeta_i(ji,1) * zkappa_i(ji,0) * 2.0
                  ENDIF

               ENDIF
            ENDIF

         END DO

         !
         !------------------------------------------------------------------------------|
         ! 10) tridiagonal system solving                                               |
         !------------------------------------------------------------------------------|
         !

         ! Solve the tridiagonal system with Gauss elimination method.
         ! Thomas algorithm, from Computational fluid Dynamics, J.D. ANDERSON, 
         ! McGraw-Hill 1984.	

         maxnumeqmax = 0
         minnumeqmin = nlay_i+5

         DO ji = kideb , kiut
            zindtbis(ji,numeqmin(ji)) =  zindterm(ji,numeqmin(ji))
            zdiagbis(ji,numeqmin(ji)) =  ztrid(ji,numeqmin(ji),2)
            minnumeqmin               =  MIN(numeqmin(ji),minnumeqmin)
            maxnumeqmax               =  MAX(numeqmax(ji),maxnumeqmax)
         END DO

         DO jk = minnumeqmin+1, maxnumeqmax
            DO ji = kideb , kiut
               numeq               =  min(max(numeqmin(ji)+1,jk),numeqmax(ji))
               zdiagbis(ji,numeq)  =  ztrid(ji,numeq,2)  - ztrid(ji,numeq,1) * ztrid(ji,numeq-1,3)  / zdiagbis(ji,numeq-1)
               zindtbis(ji,numeq)  =  zindterm(ji,numeq) - ztrid(ji,numeq,1) * zindtbis(ji,numeq-1) / zdiagbis(ji,numeq-1)
            END DO
         END DO

         DO ji = kideb , kiut
            ! ice temperatures
            t_i_1d(ji,nlay_i)    =  zindtbis(ji,numeqmax(ji)) / zdiagbis(ji,numeqmax(ji))
         END DO

         DO numeq = nlay_i + nlay_s, nlay_s + 2, -1
            DO ji = kideb , kiut
               jk    =  numeq - nlay_s - 1
               t_i_1d(ji,jk)  =  ( zindtbis(ji,numeq) - ztrid(ji,numeq,3) * t_i_1d(ji,jk+1) ) / zdiagbis(ji,numeq)
            END DO
         END DO

         DO ji = kideb , kiut
            ! snow temperatures      
            IF (ht_s_1d(ji) > 0._wp) &
               t_s_1d(ji,nlay_s)     =  ( zindtbis(ji,nlay_s+1) - ztrid(ji,nlay_s+1,3) * t_i_1d(ji,1) )  &
               &                        / zdiagbis(ji,nlay_s+1) * MAX( 0.0, SIGN( 1.0, ht_s_1d(ji) ) ) 

            ! surface temperature
            isnow(ji)   = 1._wp - MAX( 0._wp , SIGN( 1._wp , -ht_s_1d(ji) ) )
            ztsubit(ji) = t_su_1d(ji)
            IF( t_su_1d(ji) < rt0 ) &
               t_su_1d(ji) = ( zindtbis(ji,numeqmin(ji)) - ztrid(ji,numeqmin(ji),3) *  &
               &             ( isnow(ji) * t_s_1d(ji,1) + ( 1._wp - isnow(ji) ) * t_i_1d(ji,1) ) ) / zdiagbis(ji,numeqmin(ji))  
         END DO

         !
         !--------------------------------------------------------------------------
         !  11) Has the scheme converged ?, end of the iterative procedure         |
         !--------------------------------------------------------------------------
         !
         ! check that nowhere it has started to melt
         ! zerrit(ji) is a measure of error, it has to be under terr_dif
         DO ji = kideb , kiut
            t_su_1d(ji) =  MAX(  MIN( t_su_1d(ji) , rt0 ) , 190._wp  )
            zerrit(ji)  =  ABS( t_su_1d(ji) - ztsubit(ji) )     
         END DO

         DO jk  =  1, nlay_s
            DO ji = kideb , kiut
               t_s_1d(ji,jk) = MAX(  MIN( t_s_1d(ji,jk), rt0 ), 190._wp  )
               zerrit(ji)    = MAX( zerrit(ji), ABS( t_s_1d(ji,jk) - ztstemp(ji,jk) ) )
            END DO
         END DO

         DO jk  =  1, nlay_i
            DO ji = kideb , kiut
               ztmelt_i      = -tmut * s_i_1d(ji,jk) + rt0 
               t_i_1d(ji,jk) =  MAX( MIN( t_i_1d(ji,jk), ztmelt_i ), 190._wp )
               zerrit(ji)    =  MAX( zerrit(ji), ABS( t_i_1d(ji,jk) - ztitemp(ji,jk) ) )
            END DO
         END DO

         ! Compute spatial maximum over all errors
         ! note that this could be optimized substantially by iterating only the non-converging points
         zerritmax = 0._wp
         DO ji = kideb, kiut
            zerritmax = MAX( zerritmax, zerrit(ji) )   
         END DO
         IF( lk_mpp ) CALL mpp_max( zerritmax, kcom=ncomm_ice )

      END DO  ! End of the do while iterative procedure

      IF( ln_icectl .AND. lwp ) THEN
         WRITE(numout,*) ' zerritmax : ', zerritmax
         WRITE(numout,*) ' nconv     : ', nconv
      ENDIF

      !
      !-------------------------------------------------------------------------!
      !   12) Fluxes at the interfaces                                          !
      !-------------------------------------------------------------------------!
      DO ji = kideb, kiut
         !                                ! surface ice conduction flux
         isnow(ji)       = 1._wp - MAX( 0._wp, SIGN( 1._wp, -ht_s_1d(ji) ) )
         fc_su(ji)       =  -           isnow(ji)   * zkappa_s(ji,0) * zg1s * (t_s_1d(ji,1) - t_su_1d(ji))   &
            &               - ( 1._wp - isnow(ji) ) * zkappa_i(ji,0) * zg1  * (t_i_1d(ji,1) - t_su_1d(ji))
         !                                ! bottom ice conduction flux
         fc_bo_i(ji)     =  - zkappa_i(ji,nlay_i) * ( zg1*(t_bo_1d(ji) - t_i_1d(ji,nlay_i)) )
      END DO

      ! --- computes sea ice energy of melting compulsory for limthd_dh --- !
      CALL lim_thd_enmelt( kideb, kiut )

      ! --- diagnose the change in non-solar flux due to surface temperature change --- !
      IF ( ln_it_qnsice ) THEN
         DO ji = kideb, kiut
            hfx_err_dif_1d(ji) = hfx_err_dif_1d(ji) - ( qns_ice_1d(ji)  - zqns_ice_b(ji) ) * a_i_1d(ji) 
         END DO
      END IF

      ! --- diag conservation imbalance on heat diffusion - PART 2 --- !
      DO ji = kideb, kiut
         zdq(ji)        = - zq_ini(ji) + ( SUM( q_i_1d(ji,1:nlay_i) ) * ht_i_1d(ji) * r1_nlay_i +  &
            &                              SUM( q_s_1d(ji,1:nlay_s) ) * ht_s_1d(ji) * r1_nlay_s )
         IF( t_su_1d(ji) < rt0 ) THEN  ! case T_su < 0degC
            zhfx_err(ji) = qns_ice_1d(ji) + qsr_ice_1d(ji) - zradtr_i(ji,nlay_i) - fc_bo_i(ji) + zdq(ji) * r1_rdtice 
         ELSE                          ! case T_su = 0degC
            zhfx_err(ji) = fc_su(ji) + i0(ji) * qsr_ice_1d(ji) - zradtr_i(ji,nlay_i) - fc_bo_i(ji) + zdq(ji) * r1_rdtice 
         ENDIF
         hfx_err_1d(ji) = hfx_err_1d(ji) + zhfx_err(ji) * a_i_1d(ji)

         ! total heat that is sent to the ocean (i.e. not used in the heat diffusion equation)
         hfx_err_dif_1d(ji) = hfx_err_dif_1d(ji) + zhfx_err(ji) * a_i_1d(ji)
      END DO 

      !-----------------------------------------
      ! Heat flux used to warm/cool ice in W.m-2
      !-----------------------------------------
      DO ji = kideb, kiut
         IF( t_su_1d(ji) < rt0 ) THEN  ! case T_su < 0degC
            hfx_dif_1d(ji) = hfx_dif_1d(ji)  +   &
               &            ( qns_ice_1d(ji) + qsr_ice_1d(ji) - zradtr_i(ji,nlay_i) - fc_bo_i(ji) ) * a_i_1d(ji)
         ELSE                          ! case T_su = 0degC
            hfx_dif_1d(ji) = hfx_dif_1d(ji) +    &
               &             ( fc_su(ji) + i0(ji) * qsr_ice_1d(ji) - zradtr_i(ji,nlay_i) - fc_bo_i(ji) ) * a_i_1d(ji)
         ENDIF
         ! correction on the diagnosed heat flux due to non-convergence of the algorithm used to solve heat equation
         hfx_dif_1d(ji) = hfx_dif_1d(ji) - zhfx_err(ji) * a_i_1d(ji)
      END DO   
      !
      CALL wrk_dealloc( jpij, numeqmin, numeqmax )
      CALL wrk_dealloc( jpij, isnow, ztsub, ztsubit, zh_i, zh_s, zfsw )
      CALL wrk_dealloc( jpij, zf, dzf, zqns_ice_b, zerrit, zdifcase, zftrice, zihic, zghe )
      CALL wrk_dealloc( jpij,nlay_i+1, ztcond_i, zradtr_i, zradab_i, zkappa_i, ztib, zeta_i, ztitemp, z_i, zspeche_i, kjstart = 0 )
      CALL wrk_dealloc( jpij,nlay_s+1,           zradtr_s, zradab_s, zkappa_s, ztsb, zeta_s, ztstemp, z_s, kjstart = 0 )
      CALL wrk_dealloc( jpij,nlay_i+3, zindterm, zindtbis, zdiagbis )
      CALL wrk_dealloc( jpij,nlay_i+3,3, ztrid )
      CALL wrk_dealloc( jpij, zdq, zq_ini, zhfx_err )

   END SUBROUTINE lim_thd_dif

   SUBROUTINE lim_thd_enmelt( kideb, kiut )
      !!-----------------------------------------------------------------------
      !!                   ***  ROUTINE lim_thd_enmelt *** 
      !!                 
      !! ** Purpose :   Computes sea ice energy of melting q_i (J.m-3) from temperature
      !!
      !! ** Method  :   Formula (Bitz and Lipscomb, 1999)
      !!-------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kideb, kiut   ! bounds for the spatial loop
      !
      INTEGER  ::   ji, jk   ! dummy loop indices
      REAL(wp) ::   ztmelts  ! local scalar 
      !!-------------------------------------------------------------------
      !
      DO jk = 1, nlay_i             ! Sea ice energy of melting
         DO ji = kideb, kiut
            ztmelts      = - tmut  * s_i_1d(ji,jk) + rt0
            t_i_1d(ji,jk) = MIN( t_i_1d(ji,jk), ztmelts ) ! Force t_i_1d to be lower than melting point
                                                          !   (sometimes dif scheme produces abnormally high temperatures)   
            q_i_1d(ji,jk) = rhoic * ( cpic * ( ztmelts - t_i_1d(ji,jk) )                           &
               &                    + lfus * ( 1.0 - ( ztmelts-rt0 ) / ( t_i_1d(ji,jk) - rt0 ) )   &
               &                    - rcp  *         ( ztmelts-rt0 )  ) 
         END DO
      END DO
      DO jk = 1, nlay_s             ! Snow energy of melting
         DO ji = kideb, kiut
            q_s_1d(ji,jk) = rhosn * ( cpic * ( rt0 - t_s_1d(ji,jk) ) + lfus )
         END DO
      END DO
      !
   END SUBROUTINE lim_thd_enmelt

#else
   !!----------------------------------------------------------------------
   !!                   Dummy Module                 No LIM-3 sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_thd_dif          ! Empty routine
   END SUBROUTINE lim_thd_dif
#endif
   !!======================================================================
END MODULE limthd_dif
