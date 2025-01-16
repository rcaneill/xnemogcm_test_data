MODULE domvvl
   !!======================================================================
   !!                       ***  MODULE domvvl   ***
   !! Ocean :
   !!======================================================================
   !! History :  2.0  !  2006-06  (B. Levier, L. Marie)  original code
   !!            3.1  !  2009-02  (G. Madec, M. Leclair, R. Benshila)  pure z* coordinate
   !!            3.3  !  2011-10  (M. Leclair) totally rewrote domvvl: vvl option includes z_star and z_tilde coordinates
   !!            3.6  !  2014-11  (P. Mathiot) add ice shelf capability
   !!            4.1  !  2019-08  (A. Coward, D. Storkey) rename dom_vvl_sf_swp -> dom_vvl_sf_update for new timestepping
   !!             -   !  2020-02  (G. Madec, S. Techene) introduce ssh to h0 ratio
   !!----------------------------------------------------------------------

   USE oce             ! ocean dynamics and tracers
   USE phycst          ! physical constant
   USE dom_oce         ! ocean space and time domain
   USE sbc_oce         ! ocean surface boundary condition
   USE wet_dry         ! wetting and drying
   USE usrdef_istate   ! user defined initial state (wad only)
   USE restart         ! ocean restart
   !
   USE in_out_manager  ! I/O manager
   USE iom             ! I/O manager library
   USE lib_mpp         ! distributed memory computing library
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   !                                                      !!* Namelist nam_vvl
   LOGICAL , PUBLIC :: ln_vvl_zstar           = .FALSE.    ! zstar  vertical coordinate
   LOGICAL , PUBLIC :: ln_vvl_ztilde          = .FALSE.    ! ztilde vertical coordinate
   LOGICAL , PUBLIC :: ln_vvl_layer           = .FALSE.    ! level  vertical coordinate
   LOGICAL , PUBLIC :: ln_vvl_ztilde_as_zstar = .FALSE.    ! ztilde vertical coordinate
   LOGICAL , PUBLIC :: ln_vvl_zstar_at_eqtor  = .FALSE.    ! ztilde vertical coordinate
   LOGICAL , PUBLIC :: ln_vvl_kepe            = .FALSE.    ! kinetic/potential energy transfer
   !
   INTEGER          :: nn_vvl_interp = 0                   ! scale factors anomaly interpolation method at U-V-F points
                                                           ! =0 linear with no bottom correction over steps (old)
                                                           ! =1 linear with bottom correction over steps
                                                           ! =2 "qco like", i.e. proportional to thicknesses at rest
   !
   !                                                       ! conservation: not used yet
   REAL(wp)         :: rn_ahe3                             ! thickness diffusion coefficient
   REAL(wp)         :: rn_rst_e3t                          ! ztilde to zstar restoration timescale [days]
   REAL(wp)         :: rn_lf_cutoff                        ! cutoff frequency for low-pass filter  [days]
   REAL(wp)         :: rn_zdef_max                         ! maximum fractional e3t deformation
   LOGICAL , PUBLIC :: ln_vvl_dbg = .FALSE.                ! debug control prints

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: un_td, vn_td                ! thickness diffusion transport
   REAL(wp)        , ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: hdiv_lf                     ! low frequency part of hz divergence
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: tilde_e3t_b, tilde_e3t_n    ! baroclinic scale factors
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: tilde_e3t_a, dtilde_e3t_a   ! baroclinic scale factors
   REAL(wp)        , ALLOCATABLE, SAVE, DIMENSION(:,:)   :: frq_rst_e3t                 ! retoring period for scale factors
   REAL(wp)        , ALLOCATABLE, SAVE, DIMENSION(:,:)   :: frq_rst_hdv                 ! retoring period for low freq. divergence

#if defined key_qco   ||   defined key_linssh
   !!----------------------------------------------------------------------
   !!   'key_qco'                        Quasi-Eulerian vertical coordinate
   !!       OR         EMPTY MODULE
   !!   'key_linssh'                        Fix in time vertical coordinate
   !!----------------------------------------------------------------------
#else
   !!----------------------------------------------------------------------
   !!   Default key      Old management of time varying vertical coordinate
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dom_vvl_init     : define initial vertical scale factors, depths and column thickness
   !!   dom_vvl_sf_nxt   : Compute next vertical scale factors
   !!   dom_vvl_sf_update   : Swap vertical scale factors and update the vertical grid
   !!   dom_vvl_interpol : Interpolate vertical scale factors from one grid point to another
   !!   dom_vvl_rst      : read/write restart file
   !!   dom_vvl_ctl      : Check the vvl options
   !!----------------------------------------------------------------------

   PUBLIC  dom_vvl_init       ! called by domain.F90
   PUBLIC  dom_vvl_zgr        ! called by isfcpl.F90
   PUBLIC  dom_vvl_sf_nxt     ! called by step.F90
   PUBLIC  dom_vvl_sf_update  ! called by step.F90
   PUBLIC  dom_vvl_interpol   ! called by dynnxt.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: domvvl.F90 15471 2021-11-04 16:28:56Z jchanut $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION dom_vvl_alloc()
      !!----------------------------------------------------------------------
      !!                ***  FUNCTION dom_vvl_alloc  ***
      !!----------------------------------------------------------------------
      IF( ln_vvl_zstar )   dom_vvl_alloc = 0
      IF( ln_vvl_ztilde .OR. ln_vvl_layer ) THEN
         ALLOCATE( tilde_e3t_b(jpi,jpj,jpk)  , tilde_e3t_n(jpi,jpj,jpk) , tilde_e3t_a(jpi,jpj,jpk) ,   &
            &      dtilde_e3t_a(jpi,jpj,jpk) , un_td  (jpi,jpj,jpk)     , vn_td  (jpi,jpj,jpk)     ,   &
            &      STAT = dom_vvl_alloc        )
         CALL mpp_sum ( 'domvvl', dom_vvl_alloc )
         IF( dom_vvl_alloc /= 0 )   CALL ctl_stop( 'STOP', 'dom_vvl_alloc: failed to allocate arrays' )
         un_td = 0._wp
         vn_td = 0._wp
      ENDIF
      IF( ln_vvl_ztilde ) THEN
         ALLOCATE( frq_rst_e3t(jpi,jpj) , frq_rst_hdv(jpi,jpj) , hdiv_lf(jpi,jpj,jpk) , STAT= dom_vvl_alloc )
         CALL mpp_sum ( 'domvvl', dom_vvl_alloc )
         IF( dom_vvl_alloc /= 0 )   CALL ctl_stop( 'STOP', 'dom_vvl_alloc: failed to allocate arrays' )
      ENDIF
      !
   END FUNCTION dom_vvl_alloc


   SUBROUTINE dom_vvl_init( Kbb, Kmm, Kaa )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE dom_vvl_init  ***
      !!
      !! ** Purpose :  Initialization of all scale factors, depths
      !!               and water column heights
      !!
      !! ** Method  :  - use restart file and/or initialize
      !!               - interpolate scale factors
      !!
      !! ** Action  : - e3t_(n/b) and tilde_e3t_(n/b)
      !!              - Regrid: e3[u/v](:,:,:,Kmm)
      !!                        e3[u/v](:,:,:,Kmm)
      !!                        e3w(:,:,:,Kmm)
      !!                        e3[u/v]w_b
      !!                        e3[u/v]w_n
      !!                        gdept(:,:,:,Kmm), gdepw(:,:,:,Kmm) and gde3w
      !!              - h(t/u/v)_0
      !!              - frq_rst_e3t and frq_rst_hdv
      !!
      !! Reference  : Leclair, M., and G. Madec, 2011, Ocean Modelling.
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) :: Kbb, Kmm, Kaa
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'dom_vvl_init : Variable volume activated'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~'
      !
      CALL dom_vvl_ctl     ! choose vertical coordinate (z_star, z_tilde or layer)
      !
      !                    ! Allocate module arrays
      IF( dom_vvl_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'dom_vvl_init : unable to allocate arrays' )
      !
      !                    ! Read or initialize e3t_(b/n), tilde_e3t_(b/n) and hdiv_lf
      CALL dom_vvl_rst( nit000, Kbb, Kmm, 'READ' )
      e3t(:,:,jpk,Kaa) = e3t_0(:,:,jpk)  ! last level always inside the sea floor set one for all
      !
      CALL dom_vvl_zgr(Kbb, Kmm, Kaa) ! interpolation scale factor, depth and water column
      !
   END SUBROUTINE dom_vvl_init


   SUBROUTINE dom_vvl_zgr(Kbb, Kmm, Kaa)
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE dom_vvl_init  ***
      !!
      !! ** Purpose :  Interpolation of all scale factors,
      !!               depths and water column heights
      !!
      !! ** Method  :  - interpolate scale factors
      !!
      !! ** Action  : - e3t_(n/b) and tilde_e3t_(n/b)
      !!              - Regrid: e3(u/v)_n
      !!                        e3(u/v)_b
      !!                        e3w_n
      !!                        e3(u/v)w_b
      !!                        e3(u/v)w_n
      !!                        gdept_n, gdepw_n and gde3w_n
      !!              - h(t/u/v)_0
      !!              - frq_rst_e3t and frq_rst_hdv
      !!
      !! Reference  : Leclair, M., and G. Madec, 2011, Ocean Modelling.
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) :: Kbb, Kmm, Kaa
      !!----------------------------------------------------------------------
      INTEGER ::   ji, jj, jk
      INTEGER ::   ii0, ii1, ij0, ij1
      REAL(wp)::   zcoef
      !!----------------------------------------------------------------------
      !
      !                    !== Set of all other vertical scale factors  ==!  (now and before)
      !                                ! Horizontal interpolation of e3t
      CALL dom_vvl_interpol( e3t(:,:,:,Kbb), e3u(:,:,:,Kbb), 'U' )    ! from T to U
      CALL dom_vvl_interpol( e3t(:,:,:,Kmm), e3u(:,:,:,Kmm), 'U' )
      CALL dom_vvl_interpol( e3t(:,:,:,Kbb), e3v(:,:,:,Kbb), 'V' )    ! from T to V
      CALL dom_vvl_interpol( e3t(:,:,:,Kmm), e3v(:,:,:,Kmm), 'V' )
      CALL dom_vvl_interpol( e3u(:,:,:,Kmm), e3f(:,:,:), 'F' )    ! from U to F
      !                                ! Vertical interpolation of e3t,u,v
      CALL dom_vvl_interpol( e3t(:,:,:,Kmm), e3w (:,:,:,Kmm), 'W'  )  ! from T to W
      CALL dom_vvl_interpol( e3t(:,:,:,Kbb), e3w (:,:,:,Kbb), 'W'  )
      CALL dom_vvl_interpol( e3u(:,:,:,Kmm), e3uw(:,:,:,Kmm), 'UW' )  ! from U to UW
      CALL dom_vvl_interpol( e3u(:,:,:,Kbb), e3uw(:,:,:,Kbb), 'UW' )
      CALL dom_vvl_interpol( e3v(:,:,:,Kmm), e3vw(:,:,:,Kmm), 'VW' )  ! from V to UW
      CALL dom_vvl_interpol( e3v(:,:,:,Kbb), e3vw(:,:,:,Kbb), 'VW' )

      ! We need to define e3[tuv]_a for AGRIF initialisation (should not be a problem for the restartability...)
      e3t(:,:,:,Kaa) = e3t(:,:,:,Kmm)
      e3u(:,:,:,Kaa) = e3u(:,:,:,Kmm)
      e3v(:,:,:,Kaa) = e3v(:,:,:,Kmm)
      !
      !                    !==  depth of t and w-point  ==!   (set the isf depth as it is in the initial timestep)
      gdept(:,:,1,Kmm) = 0.5_wp * e3w(:,:,1,Kmm)       ! reference to the ocean surface (used for MLD and light penetration)
      gdepw(:,:,1,Kmm) = 0.0_wp
      gde3w(:,:,1) = gdept(:,:,1,Kmm) - ssh(:,:,Kmm)  ! reference to a common level z=0 for hpg
      gdept(:,:,1,Kbb) = 0.5_wp * e3w(:,:,1,Kbb)
      gdepw(:,:,1,Kbb) = 0.0_wp
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 2, jpk )                     ! vertical sum
         !    zcoef = tmask - wmask    ! 0 everywhere tmask = wmask, ie everywhere expect at jk = mikt
         !                             ! 1 everywhere from mbkt to mikt + 1 or 1 (if no isf)
         !                             ! 0.5 where jk = mikt
!!gm ???????   BUG ?  gdept(:,:,:,Kmm) as well as gde3w  does not include the thickness of ISF ??
         zcoef = ( tmask(ji,jj,jk) - wmask(ji,jj,jk) )
         gdepw(ji,jj,jk,Kmm) = gdepw(ji,jj,jk-1,Kmm) + e3t(ji,jj,jk-1,Kmm)
         gdept(ji,jj,jk,Kmm) =      zcoef  * ( gdepw(ji,jj,jk  ,Kmm) + 0.5 * e3w(ji,jj,jk,Kmm))  &
            &                + (1-zcoef) * ( gdept(ji,jj,jk-1,Kmm) +       e3w(ji,jj,jk,Kmm))
         gde3w(ji,jj,jk) = gdept(ji,jj,jk,Kmm) - ssh(ji,jj,Kmm)
         gdepw(ji,jj,jk,Kbb) = gdepw(ji,jj,jk-1,Kbb) + e3t(ji,jj,jk-1,Kbb)
         gdept(ji,jj,jk,Kbb) =      zcoef  * ( gdepw(ji,jj,jk  ,Kbb) + 0.5 * e3w(ji,jj,jk,Kbb))  &
            &                + (1-zcoef) * ( gdept(ji,jj,jk-1,Kbb) +       e3w(ji,jj,jk,Kbb))
      END_3D
      !
      !                    !==  thickness of the water column  !!   (ocean portion only)
      ht(:,:) = e3t(:,:,1,Kmm) * tmask(:,:,1)   !!gm  BUG  :  this should be 1/2 * e3w(k=1) ....
      hu(:,:,Kbb) = e3u(:,:,1,Kbb) * umask(:,:,1)
      hu(:,:,Kmm) = e3u(:,:,1,Kmm) * umask(:,:,1)
      hv(:,:,Kbb) = e3v(:,:,1,Kbb) * vmask(:,:,1)
      hv(:,:,Kmm) = e3v(:,:,1,Kmm) * vmask(:,:,1)
      DO jk = 2, jpkm1
         ht(:,:) = ht(:,:) + e3t(:,:,jk,Kmm) * tmask(:,:,jk)
         hu(:,:,Kbb) = hu(:,:,Kbb) + e3u(:,:,jk,Kbb) * umask(:,:,jk)
         hu(:,:,Kmm) = hu(:,:,Kmm) + e3u(:,:,jk,Kmm) * umask(:,:,jk)
         hv(:,:,Kbb) = hv(:,:,Kbb) + e3v(:,:,jk,Kbb) * vmask(:,:,jk)
         hv(:,:,Kmm) = hv(:,:,Kmm) + e3v(:,:,jk,Kmm) * vmask(:,:,jk)
      END DO
      !
      !                    !==  inverse of water column thickness   ==!   (u- and v- points)
      r1_hu(:,:,Kbb) = ssumask(:,:) / ( hu(:,:,Kbb) + 1._wp - ssumask(:,:) )    ! _i mask due to ISF
      r1_hu(:,:,Kmm) = ssumask(:,:) / ( hu(:,:,Kmm) + 1._wp - ssumask(:,:) )
      r1_hv(:,:,Kbb) = ssvmask(:,:) / ( hv(:,:,Kbb) + 1._wp - ssvmask(:,:) )
      r1_hv(:,:,Kmm) = ssvmask(:,:) / ( hv(:,:,Kmm) + 1._wp - ssvmask(:,:) )

      !                    !==   z_tilde coordinate case  ==!   (Restoring frequencies)
      IF( ln_vvl_ztilde ) THEN
!!gm : idea: add here a READ in a file of custumized restoring frequency
         !                                   ! Values in days provided via the namelist
         !                                   ! use rsmall to avoid possible division by zero errors with faulty settings
         frq_rst_e3t(:,:) = 2._wp * rpi / ( MAX( rn_rst_e3t  , rsmall ) * 86400.0_wp )
         frq_rst_hdv(:,:) = 2._wp * rpi / ( MAX( rn_lf_cutoff, rsmall ) * 86400.0_wp )
         !
         IF( ln_vvl_ztilde_as_zstar ) THEN   ! z-star emulation using z-tile
            frq_rst_e3t(:,:) = 0._wp               !Ignore namelist settings
            frq_rst_hdv(:,:) = 1._wp / rn_Dt
         ENDIF
         IF ( ln_vvl_zstar_at_eqtor ) THEN   ! use z-star in vicinity of the Equator
            DO_2D( 1, 1, 1, 1 )
!!gm  case |gphi| >= 6 degrees is useless   initialized just above by default
               IF( ABS(gphit(ji,jj)) >= 6.) THEN
                  ! values outside the equatorial band and transition zone (ztilde)
                  frq_rst_e3t(ji,jj) =  2.0_wp * rpi / ( MAX( rn_rst_e3t  , rsmall ) * 86400.e0_wp )
                  frq_rst_hdv(ji,jj) =  2.0_wp * rpi / ( MAX( rn_lf_cutoff, rsmall ) * 86400.e0_wp )
               ELSEIF( ABS(gphit(ji,jj)) <= 2.5) THEN    ! Equator strip ==> z-star
                  ! values inside the equatorial band (ztilde as zstar)
                  frq_rst_e3t(ji,jj) =  0.0_wp
                  frq_rst_hdv(ji,jj) =  1.0_wp / rn_Dt
               ELSE                                      ! transition band (2.5 to 6 degrees N/S)
                  !                                      ! (linearly transition from z-tilde to z-star)
                  frq_rst_e3t(ji,jj) = 0.0_wp + (frq_rst_e3t(ji,jj)-0.0_wp)*0.5_wp   &
                     &            * (  1.0_wp - COS( rad*(ABS(gphit(ji,jj))-2.5_wp)  &
                     &                                          * 180._wp / 3.5_wp ) )
                  frq_rst_hdv(ji,jj) = (1.0_wp / rn_Dt)                                &
                     &            + (  frq_rst_hdv(ji,jj)-(1.e0_wp / rn_Dt) )*0.5_wp   &
                     &            * (  1._wp  - COS( rad*(ABS(gphit(ji,jj))-2.5_wp)  &
                     &                                          * 180._wp / 3.5_wp ) )
               ENDIF
            END_2D
            IF( cn_cfg == "orca" .OR. cn_cfg == "ORCA" ) THEN
               IF( nn_cfg == 3 ) THEN   ! ORCA2: Suppress ztilde in the Foxe Basin for ORCA2
                  ii0 = 103 + nn_hls - 1   ;   ii1 = 111 + nn_hls - 1
                  ij0 = 128 + nn_hls       ;   ij1 = 135 + nn_hls
                  frq_rst_e3t( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) =  0.0_wp
                  frq_rst_hdv( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) =  1.e0_wp / rn_Dt
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      !
   END SUBROUTINE dom_vvl_zgr


   SUBROUTINE dom_vvl_sf_nxt( kt, Kbb, Kmm, Kaa, kcall )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE dom_vvl_sf_nxt  ***
      !!
      !! ** Purpose :  - compute the after scale factors used in tra_zdf, dynnxt,
      !!                 tranxt and dynspg routines
      !!
      !! ** Method  :  - z_star case:  Repartition of ssh INCREMENT proportionnaly to the level thickness.
      !!               - z_tilde_case: after scale factor increment =
      !!                                    high frequency part of horizontal divergence
      !!                                  + retsoring towards the background grid
      !!                                  + thickness difusion
      !!                               Then repartition of ssh INCREMENT proportionnaly
      !!                               to the "baroclinic" level thickness.
      !!
      !! ** Action  :  - hdiv_lf    : restoring towards full baroclinic divergence in z_tilde case
      !!               - tilde_e3t_a: after increment of vertical scale factor
      !!                              in z_tilde case
      !!               - e3(t/u/v)_a
      !!
      !! Reference  : Leclair, M., and Madec, G. 2011, Ocean Modelling.
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in )           ::   kt             ! time step
      INTEGER, INTENT( in )           ::   Kbb, Kmm, Kaa  ! time step
      INTEGER, INTENT( in ), OPTIONAL ::   kcall          ! optional argument indicating call sequence
      !
      INTEGER                ::   ji, jj, jk            ! dummy loop indices
      INTEGER , DIMENSION(3) ::   ijk_max, ijk_min      ! temporary integers
      REAL(wp)               ::   z_tmin, z_tmax        ! local scalars
      LOGICAL                ::   ll_do_bclinic         ! local logical
      REAL(wp), DIMENSION(jpi,jpj)     ::   zht, z_scale, zwu, zwv, zhdiv
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   ze3t
      LOGICAL , DIMENSION(:,:,:), ALLOCATABLE ::   llmsk
      !!----------------------------------------------------------------------
      !
      IF( ln_linssh )   RETURN      ! No calculation in linear free surface
      !
      IF( ln_timing )   CALL timing_start('dom_vvl_sf_nxt')
      !
      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'dom_vvl_sf_nxt : compute after scale factors'
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~'
      ENDIF

      ll_do_bclinic = .TRUE.
      IF( PRESENT(kcall) ) THEN
         IF( kcall == 2 .AND. ln_vvl_ztilde )   ll_do_bclinic = .FALSE.
      ENDIF

      ! ******************************* !
      ! After acale factors at t-points !
      ! ******************************* !
      !                                                ! --------------------------------------------- !
      !                                                ! z_star coordinate and barotropic z-tilde part !
      !                                                ! --------------------------------------------- !
      !
      z_scale(:,:) = ( ssh(:,:,Kaa) - ssh(:,:,Kbb) ) * ssmask(:,:) / ( ht_0(:,:) + ssh(:,:,Kmm) + 1. - ssmask(:,:) )
      DO jk = 1, jpkm1
         ! formally this is the same as e3t(:,:,:,Kaa) = e3t_0*(1+ssha/ht_0)
         e3t(:,:,jk,Kaa) = e3t(:,:,jk,Kbb) + e3t(:,:,jk,Kmm) * z_scale(:,:) * tmask(:,:,jk)
      END DO
      !
      IF( (ln_vvl_ztilde .OR. ln_vvl_layer) .AND. ll_do_bclinic ) THEN   ! z_tilde or layer coordinate !
         !                                                               ! ------baroclinic part------ !
         ! I - initialization
         ! ==================

         ! 1 - barotropic divergence
         ! -------------------------
         zhdiv(:,:) = 0._wp
         zht(:,:)   = 0._wp
         DO jk = 1, jpkm1
            zhdiv(:,:) = zhdiv(:,:) + e3t(:,:,jk,Kmm) * hdiv(:,:,jk)
            zht  (:,:) = zht  (:,:) + e3t(:,:,jk,Kmm) * tmask(:,:,jk)
         END DO
         zhdiv(:,:) = zhdiv(:,:) / ( zht(:,:) + 1. - tmask_i(:,:) )

         ! 2 - Low frequency baroclinic horizontal divergence  (z-tilde case only)
         ! --------------------------------------------------
         IF( ln_vvl_ztilde ) THEN
            IF( kt > nit000 ) THEN
               DO jk = 1, jpkm1
                  hdiv_lf(:,:,jk) = hdiv_lf(:,:,jk) - rn_Dt * frq_rst_hdv(:,:)   &
                     &          * ( hdiv_lf(:,:,jk) - e3t(:,:,jk,Kmm) * ( hdiv(:,:,jk) - zhdiv(:,:) ) )
               END DO
            ENDIF
         ENDIF

         ! II - after z_tilde increments of vertical scale factors
         ! =======================================================
         tilde_e3t_a(:,:,:) = 0._wp  ! tilde_e3t_a used to store tendency terms

         ! 1 - High frequency divergence term
         ! ----------------------------------
         IF( ln_vvl_ztilde ) THEN     ! z_tilde case
            DO jk = 1, jpkm1
               tilde_e3t_a(:,:,jk) = tilde_e3t_a(:,:,jk) - ( e3t(:,:,jk,Kmm) * ( hdiv(:,:,jk) - zhdiv(:,:) ) - hdiv_lf(:,:,jk) )
            END DO
         ELSE                         ! layer case
            DO jk = 1, jpkm1
               tilde_e3t_a(:,:,jk) = tilde_e3t_a(:,:,jk) -   e3t(:,:,jk,Kmm) * ( hdiv(:,:,jk) - zhdiv(:,:) ) * tmask(:,:,jk)
            END DO
         ENDIF

         ! 2 - Restoring term (z-tilde case only)
         ! ------------------
         IF( ln_vvl_ztilde ) THEN
            DO jk = 1, jpk
               tilde_e3t_a(:,:,jk) = tilde_e3t_a(:,:,jk) - frq_rst_e3t(:,:) * tilde_e3t_b(:,:,jk)
            END DO
         ENDIF

         ! 3 - Thickness diffusion term
         ! ----------------------------
         zwu(:,:) = 0._wp
         zwv(:,:) = 0._wp
         DO_3D( nn_hls, nn_hls-1, nn_hls, nn_hls-1, 1, jpkm1 )       ! a - first derivative: diffusive fluxes
            un_td(ji,jj,jk) = rn_ahe3 * umask(ji,jj,jk) * e2_e1u(ji,jj)           &
               &            * ( tilde_e3t_b(ji,jj,jk) - tilde_e3t_b(ji+1,jj  ,jk) )
            vn_td(ji,jj,jk) = rn_ahe3 * vmask(ji,jj,jk) * e1_e2v(ji,jj)           &
               &            * ( tilde_e3t_b(ji,jj,jk) - tilde_e3t_b(ji  ,jj+1,jk) )
            zwu(ji,jj) = zwu(ji,jj) + un_td(ji,jj,jk)
            zwv(ji,jj) = zwv(ji,jj) + vn_td(ji,jj,jk)
         END_3D
         DO_2D( nn_hls, nn_hls-1, nn_hls, nn_hls-1 )                 ! b - correction for last oceanic u-v points
            un_td(ji,jj,mbku(ji,jj)) = un_td(ji,jj,mbku(ji,jj)) - zwu(ji,jj)
            vn_td(ji,jj,mbkv(ji,jj)) = vn_td(ji,jj,mbkv(ji,jj)) - zwv(ji,jj)
         END_2D
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )   ! c - second derivative: divergence of diffusive fluxes
            tilde_e3t_a(ji,jj,jk) = tilde_e3t_a(ji,jj,jk) + (   un_td(ji-1,jj  ,jk) - un_td(ji,jj,jk)    &
               &                                          +     vn_td(ji  ,jj-1,jk) - vn_td(ji,jj,jk)    &
               &                                            ) * r1_e1e2t(ji,jj)
         END_3D
         !                               ! d - thickness diffusion transport: boundary conditions
         !                             (stored for tracer advction and continuity equation)
         IF( nn_hls == 1 ) CALL lbc_lnk( 'domvvl', un_td , 'U' , -1._wp, vn_td , 'V' , -1._wp)
         ! 4 - Time stepping of baroclinic scale factors
         ! ---------------------------------------------
         CALL lbc_lnk( 'domvvl', tilde_e3t_a(:,:,:), 'T', 1._wp )
         tilde_e3t_a(:,:,:) = tilde_e3t_b(:,:,:) + rDt * tmask(:,:,:) * tilde_e3t_a(:,:,:)

         ! Maximum deformation control
         ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~
         ALLOCATE( ze3t(jpi,jpj,jpk), llmsk(jpi,jpj,jpk) )
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            ze3t(ji,jj,jk) = tilde_e3t_a(ji,jj,jk) / e3t_0(ji,jj,jk) * tmask(ji,jj,jk) * tmask_i(ji,jj)
         END_3D
         !
         llmsk(     1:nn_hls,:,:) = .FALSE.   ! exclude halos from the checked region
         llmsk(Nie0+1:   jpi,:,:) = .FALSE.
         llmsk(:,     1:nn_hls,:) = .FALSE.
         llmsk(:,Nje0+1:   jpj,:) = .FALSE.
         !
         llmsk(Nis0:Nie0,Njs0:Nje0,:) = tmask(Nis0:Nie0,Njs0:Nje0,:) == 1._wp                  ! define only the inner domain
         z_tmax = MAXVAL( ze3t(:,:,:), mask = llmsk )   ;   CALL mpp_max( 'domvvl', z_tmax )   ! max over the global domain
         z_tmin = MINVAL( ze3t(:,:,:), mask = llmsk )   ;   CALL mpp_min( 'domvvl', z_tmin )   ! min over the global domain
         ! - ML - test: for the moment, stop simulation for too large e3_t variations
         IF( ( z_tmax >  rn_zdef_max ) .OR. ( z_tmin < - rn_zdef_max ) ) THEN
            CALL mpp_maxloc( 'domvvl', ze3t, llmsk, z_tmax, ijk_max )
            CALL mpp_minloc( 'domvvl', ze3t, llmsk, z_tmin, ijk_min )
            IF (lwp) THEN
               WRITE(numout, *) 'MAX( tilde_e3t_a(:,:,:) / e3t_0(:,:,:) ) =', z_tmax
               WRITE(numout, *) 'at i, j, k=', ijk_max
               WRITE(numout, *) 'MIN( tilde_e3t_a(:,:,:) / e3t_0(:,:,:) ) =', z_tmin
               WRITE(numout, *) 'at i, j, k=', ijk_min
               CALL ctl_stop( 'STOP', 'MAX( ABS( tilde_e3t_a(:,:,: ) ) / e3t_0(:,:,:) ) too high')
            ENDIF
         ENDIF
         DEALLOCATE( ze3t, llmsk )
         ! - ML - end test
         ! - ML - Imposing these limits will cause a baroclinicity error which is corrected for below
         tilde_e3t_a(:,:,:) = MIN( tilde_e3t_a(:,:,:),   rn_zdef_max * e3t_0(:,:,:) )
         tilde_e3t_a(:,:,:) = MAX( tilde_e3t_a(:,:,:), - rn_zdef_max * e3t_0(:,:,:) )

         !
         ! "tilda" change in the after scale factor
         ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         DO jk = 1, jpkm1
            dtilde_e3t_a(:,:,jk) = tilde_e3t_a(:,:,jk) - tilde_e3t_b(:,:,jk)
         END DO
         ! III - Barotropic repartition of the sea surface height over the baroclinic profile
         ! ==================================================================================
         ! add ( ssh increment + "baroclinicity error" ) proportionly to e3t(n)
         ! - ML - baroclinicity error should be better treated in the future
         !        i.e. locally and not spread over the water column.
         !        (keep in mind that the idea is to reduce Eulerian velocity as much as possible)
         zht(:,:) = 0.
         DO jk = 1, jpkm1
            zht(:,:)  = zht(:,:) + tilde_e3t_a(:,:,jk) * tmask(:,:,jk)
         END DO
         z_scale(:,:) =  - zht(:,:) / ( ht_0(:,:) + ssh(:,:,Kmm) + 1. - ssmask(:,:) )
         DO jk = 1, jpkm1
            dtilde_e3t_a(:,:,jk) = dtilde_e3t_a(:,:,jk) + e3t(:,:,jk,Kmm) * z_scale(:,:) * tmask(:,:,jk)
         END DO

      ENDIF

      IF( ln_vvl_ztilde .OR. ln_vvl_layer )  THEN   ! z_tilde or layer coordinate !
      !                                           ! ---baroclinic part--------- !
         DO jk = 1, jpkm1
            e3t(:,:,jk,Kaa) = e3t(:,:,jk,Kaa) + dtilde_e3t_a(:,:,jk) * tmask(:,:,jk)
         END DO
      ENDIF

      IF( ln_vvl_dbg .AND. .NOT. ll_do_bclinic ) THEN   ! - ML - test: control prints for debuging
         !
         IF( lwp ) WRITE(numout, *) 'kt =', kt
         IF ( ln_vvl_ztilde .OR. ln_vvl_layer ) THEN
            z_tmax = MAXVAL( tmask(:,:,1) * tmask_i(:,:) * ABS( zht(:,:) ) )
            CALL mpp_max( 'domvvl', z_tmax )                             ! max over the global domain
            IF( lwp    ) WRITE(numout, *) kt,' MAXVAL(abs(SUM(tilde_e3t_a))) =', z_tmax
         END IF
         !
         zht(:,:) = 0.0_wp
         DO jk = 1, jpkm1
            zht(:,:) = zht(:,:) + e3t(:,:,jk,Kmm) * tmask(:,:,jk)
         END DO
         z_tmax = MAXVAL( tmask(:,:,1) * tmask_i(:,:) * ABS( ht_0(:,:) + ssh(:,:,Kmm) - zht(:,:) ) )
         CALL mpp_max( 'domvvl', z_tmax )                                ! max over the global domain
         IF( lwp    ) WRITE(numout, *) kt,' MAXVAL(abs(ht_0+sshn-SUM(e3t(:,:,:,Kmm)))) =', z_tmax
         !
         zht(:,:) = 0.0_wp
         DO jk = 1, jpkm1
            zht(:,:) = zht(:,:) + e3t(:,:,jk,Kaa) * tmask(:,:,jk)
         END DO
         z_tmax = MAXVAL( tmask(:,:,1) * tmask_i(:,:) * ABS( ht_0(:,:) + ssh(:,:,Kaa) - zht(:,:) ) )
         CALL mpp_max( 'domvvl', z_tmax )                                ! max over the global domain
         IF( lwp    ) WRITE(numout, *) kt,' MAXVAL(abs(ht_0+ssha-SUM(e3t(:,:,:,Kaa)))) =', z_tmax
         !
         zht(:,:) = 0.0_wp
         DO jk = 1, jpkm1
            zht(:,:) = zht(:,:) + e3t(:,:,jk,Kbb) * tmask(:,:,jk)
         END DO
         z_tmax = MAXVAL( tmask(:,:,1) * tmask_i(:,:) * ABS( ht_0(:,:) + ssh(:,:,Kbb) - zht(:,:) ) )
         CALL mpp_max( 'domvvl', z_tmax )                                ! max over the global domain
         IF( lwp    ) WRITE(numout, *) kt,' MAXVAL(abs(ht_0+sshb-SUM(e3t(:,:,:,Kbb)))) =', z_tmax
         !
         z_tmax = MAXVAL( tmask(:,:,1) *  ABS( ssh(:,:,Kbb) ) )
         CALL mpp_max( 'domvvl', z_tmax )                                ! max over the global domain
         IF( lwp    ) WRITE(numout, *) kt,' MAXVAL(abs(ssh(:,:,Kbb)))) =', z_tmax
         !
         z_tmax = MAXVAL( tmask(:,:,1) *  ABS( ssh(:,:,Kmm) ) )
         CALL mpp_max( 'domvvl', z_tmax )                                ! max over the global domain
         IF( lwp    ) WRITE(numout, *) kt,' MAXVAL(abs(ssh(:,:,Kmm)))) =', z_tmax
         !
         z_tmax = MAXVAL( tmask(:,:,1) *  ABS( ssh(:,:,Kaa) ) )
         CALL mpp_max( 'domvvl', z_tmax )                                ! max over the global domain
         IF( lwp    ) WRITE(numout, *) kt,' MAXVAL(abs(ssh(:,:,Kaa)))) =', z_tmax
      END IF

#if defined key_agrif
      ! *********************************** !
      ! After scale factors at w- points    !
      ! *********************************** !
      ! At some point, "after" depths at T-points may be required 
      ! for AGRIF vertical remap. To prevent from saving an
      ! additional array, re-compute depths from e3w when needed
      CALL dom_vvl_interpol( e3t(:,:,:,Kaa), e3w(:,:,:,Kaa), 'W'  )
#endif
      ! *********************************** !
      ! After scale factors at u- v- points !
      ! *********************************** !

      CALL dom_vvl_interpol( e3t(:,:,:,Kaa), e3u(:,:,:,Kaa), 'U' )
      CALL dom_vvl_interpol( e3t(:,:,:,Kaa), e3v(:,:,:,Kaa), 'V' )

      ! *********************************** !
      ! After depths at u- v points         !
      ! *********************************** !

      hu(:,:,Kaa) = e3u(:,:,1,Kaa) * umask(:,:,1)
      hv(:,:,Kaa) = e3v(:,:,1,Kaa) * vmask(:,:,1)
      DO jk = 2, jpkm1
         hu(:,:,Kaa) = hu(:,:,Kaa) + e3u(:,:,jk,Kaa) * umask(:,:,jk)
         hv(:,:,Kaa) = hv(:,:,Kaa) + e3v(:,:,jk,Kaa) * vmask(:,:,jk)
      END DO
      !                                        ! Inverse of the local depth
!!gm BUG ?  don't understand the use of umask_i here .....
      r1_hu(:,:,Kaa) = ssumask(:,:) / ( hu(:,:,Kaa) + 1._wp - ssumask(:,:) )
      r1_hv(:,:,Kaa) = ssvmask(:,:) / ( hv(:,:,Kaa) + 1._wp - ssvmask(:,:) )
      !
      IF( ln_timing )   CALL timing_stop('dom_vvl_sf_nxt')
      !
   END SUBROUTINE dom_vvl_sf_nxt


   SUBROUTINE dom_vvl_sf_update( kt, Kbb, Kmm, Kaa )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE dom_vvl_sf_update  ***
      !!
      !! ** Purpose :  for z tilde case: compute time filter and swap of scale factors
      !!               compute all depths and related variables for next time step
      !!               write outputs and restart file
      !!
      !! ** Method  :  - swap of e3t with trick for volume/tracer conservation (ONLY FOR Z TILDE CASE)
      !!               - reconstruct scale factor at other grid points (interpolate)
      !!               - recompute depths and water height fields
      !!
      !! ** Action  :  - tilde_e3t_(b/n) ready for next time step
      !!               - Recompute:
      !!                    e3(u/v)_b
      !!                    e3w(:,:,:,Kmm)
      !!                    e3(u/v)w_b
      !!                    e3(u/v)w_n
      !!                    gdept(:,:,:,Kmm), gdepw(:,:,:,Kmm)  and gde3w
      !!                    h(u/v) and h(u/v)r
      !!
      !! Reference  : Leclair, M., and G. Madec, 2009, Ocean Modelling.
      !!              Leclair, M., and G. Madec, 2011, Ocean Modelling.
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt              ! time step
      INTEGER, INTENT( in ) ::   Kbb, Kmm, Kaa   ! time level indices
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zcoef        ! local scalar
      !!----------------------------------------------------------------------
      !
      IF( ln_linssh )   RETURN      ! No calculation in linear free surface
      !
      IF( ln_timing )   CALL timing_start('dom_vvl_sf_update')
      !
      IF( kt == nit000 )   THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'dom_vvl_sf_update : - interpolate scale factors and compute depths for next time step'
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~~~~'
      ENDIF
      !
      ! Time filter and swap of scale factors
      ! =====================================
      ! - ML - e3(t/u/v)_b are allready computed in dynnxt.
      IF( ln_vvl_ztilde .OR. ln_vvl_layer ) THEN
         IF( l_1st_euler ) THEN
            tilde_e3t_b(:,:,:) = tilde_e3t_n(:,:,:)
         ELSE
            tilde_e3t_b(:,:,:) = tilde_e3t_n(:,:,:) &
            &         + rn_atfp * ( tilde_e3t_b(:,:,:) - 2.0_wp * tilde_e3t_n(:,:,:) + tilde_e3t_a(:,:,:) )
         ENDIF
         tilde_e3t_n(:,:,:) = tilde_e3t_a(:,:,:)
      ENDIF

      ! Compute all missing vertical scale factor and depths
      ! ====================================================
      ! Horizontal scale factor interpolations
      ! --------------------------------------
      ! - ML - e3u(:,:,:,Kbb) and e3v(:,:,:,Kbb) are already computed in dynnxt
      ! - JC - hu(:,:,:,Kbb), hv(:,:,:,:,Kbb), hur_b, hvr_b also

      CALL dom_vvl_interpol( e3u(:,:,:,Kmm), e3f(:,:,:), 'F'  )

      ! Vertical scale factor interpolations
      CALL dom_vvl_interpol( e3t(:,:,:,Kmm),  e3w(:,:,:,Kmm), 'W'  )
      CALL dom_vvl_interpol( e3u(:,:,:,Kmm), e3uw(:,:,:,Kmm), 'UW' )
      CALL dom_vvl_interpol( e3v(:,:,:,Kmm), e3vw(:,:,:,Kmm), 'VW' )
      CALL dom_vvl_interpol( e3t(:,:,:,Kbb),  e3w(:,:,:,Kbb), 'W'  )
      CALL dom_vvl_interpol( e3u(:,:,:,Kbb), e3uw(:,:,:,Kbb), 'UW' )
      CALL dom_vvl_interpol( e3v(:,:,:,Kbb), e3vw(:,:,:,Kbb), 'VW' )

      ! t- and w- points depth (set the isf depth as it is in the initial step)
      gdept(:,:,1,Kmm) = 0.5_wp * e3w(:,:,1,Kmm)
      gdepw(:,:,1,Kmm) = 0.0_wp
      gde3w(:,:,1) = gdept(:,:,1,Kmm) - ssh(:,:,Kmm)
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 2, jpk )
        !    zcoef = (tmask(ji,jj,jk) - wmask(ji,jj,jk))   ! 0 everywhere tmask = wmask, ie everywhere expect at jk = mikt
                                                           ! 1 for jk = mikt
         zcoef = (tmask(ji,jj,jk) - wmask(ji,jj,jk))
         gdepw(ji,jj,jk,Kmm) = gdepw(ji,jj,jk-1,Kmm) + e3t(ji,jj,jk-1,Kmm)
         gdept(ji,jj,jk,Kmm) =    zcoef  * ( gdepw(ji,jj,jk  ,Kmm) + 0.5 * e3w(ji,jj,jk,Kmm) )  &
             &             + (1-zcoef) * ( gdept(ji,jj,jk-1,Kmm) +       e3w(ji,jj,jk,Kmm) )
         gde3w(ji,jj,jk) = gdept(ji,jj,jk,Kmm) - ssh(ji,jj,Kmm)
      END_3D

      ! Local depth and Inverse of the local depth of the water
      ! -------------------------------------------------------
      !
      ht(:,:) = e3t(:,:,1,Kmm) * tmask(:,:,1)
      DO jk = 2, jpkm1
         ht(:,:) = ht(:,:) + e3t(:,:,jk,Kmm) * tmask(:,:,jk)
      END DO

      ! write restart file
      ! ==================
      IF( lrst_oce  )   CALL dom_vvl_rst( kt, Kbb, Kmm, 'WRITE' )
      !
      IF( ln_timing )   CALL timing_stop('dom_vvl_sf_update')
      !
   END SUBROUTINE dom_vvl_sf_update


   SUBROUTINE dom_vvl_interpol( pe3_in, pe3_out, pout )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE dom_vvl__interpol  ***
      !!
      !! ** Purpose :   interpolate scale factors from one grid point to another
      !!
      !! ** Method  :   e3_out = e3_0 + interpolation(e3_in - e3_0)
      !!                - horizontal interpolation: grid cell surface averaging
      !!                - vertical interpolation: simple averaging
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(in   ) ::  pe3_in    ! input e3 to be interpolated
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) ::  pe3_out   ! output interpolated e3
      CHARACTER(LEN=*)                , INTENT(in   ) ::  pout      ! grid point of out scale factors
      !                                                             !   =  'U', 'V', 'W, 'F', 'UW' or 'VW'
      !
      INTEGER ::   ji, jj, jk                                       ! dummy loop indices
      INTEGER ::   iku, ikum1, ikv, ikvm1, ikf, ikfm1
      REAL(wp) ::  zlnwd                                            ! =1./0. when ln_wd_il = T/F
      REAL(wp), DIMENSION(jpi,jpj) :: zssh                          ! work array to retrieve ssh (nn_vvl_interp > 1)
      !!----------------------------------------------------------------------
      !
      IF(ln_wd_il) THEN
        zlnwd = 1.0_wp
      ELSE
        zlnwd = 0.0_wp
      END IF
      !
      SELECT CASE ( pout )    !==  type of interpolation  ==!
         !
      CASE( 'U' )                   !* from T- to U-point : hor. surface weighted mean
         SELECT CASE ( nn_vvl_interp )
         CASE ( 0 ) 
            !
            DO_3D( 1, 0, 1, 0, 1, jpk )
               pe3_out(ji,jj,jk) = 0.5_wp * (  umask(ji,jj,jk) * (1.0_wp - zlnwd) + zlnwd ) * r1_e1e2u(ji,jj)   &
                  &                       * (   e1e2t(ji  ,jj) * ( pe3_in(ji  ,jj,jk) - e3t_0(ji  ,jj,jk) )     &
                  &                           + e1e2t(ji+1,jj) * ( pe3_in(ji+1,jj,jk) - e3t_0(ji+1,jj,jk) ) )
            END_3D
            !
         CASE ( 1 )
            !
            DO_3D( 1, 0, 1, 0, 1, jpk )
               pe3_out(ji,jj,jk) = 0.5_wp * (  umask(ji,jj,jk) * (1.0_wp - zlnwd) + zlnwd ) * r1_e1e2u(ji,jj)   &
                  &                       * (   e1e2t(ji  ,jj) * ( pe3_in(ji  ,jj,jk) - e3t_0(ji  ,jj,jk) )     &
                  &                           + e1e2t(ji+1,jj) * ( pe3_in(ji+1,jj,jk) - e3t_0(ji+1,jj,jk) ) )
            END_3D
            !
            ! Bottom correction:
            DO_2D( 1, 0, 1, 0 )
               iku    = mbku(ji  ,jj)
               ikum1  = iku - 1
               pe3_out(ji,jj,iku) = ( umask(ji,jj,iku) * (1.0_wp - zlnwd) + zlnwd )    & 
                  &     * ( 0.5_wp *  r1_e1e2u(ji,jj)                                  &
                  &     * (    e1e2t(ji  ,jj) * ( SUM(tmask(ji  ,jj,:)*(pe3_in(ji  ,jj,:) - e3t_0(ji  ,jj,:))) )   &               
                  &          + e1e2t(ji+1,jj) * ( SUM(tmask(ji+1,jj,:)*(pe3_in(ji+1,jj,:) - e3t_0(ji+1,jj,:))) ) ) &
                  &     - SUM(pe3_out(ji,jj,1:ikum1)))
            END_2D
            !
         CASE ( 2 ) 
            zssh(:,:) = SUM(tmask(:,:,:)*(pe3_in(:,:,:)-e3t_0(:,:,:)), DIM=3)
            !
            DO_3D( 1, 0, 1, 0, 1, jpk )
               pe3_out(ji,jj,jk) = 0.5_wp * (  umask(ji,jj,jk) * (1.0_wp - zlnwd) + zlnwd ) * r1_e1e2u(ji,jj)    &
                  &                       * (   e1e2t(ji  ,jj) * zssh(ji  ,jj) + e1e2t(ji+1,jj) * zssh(ji+1,jj)) &
                  &                       * e3u_0(ji,jj,jk) / ( hu_0(ji,jj) + 1._wp - ssumask(ji,jj) )
            END_3D   
            !
         END SELECT
         !
         CALL lbc_lnk( 'domvvl', pe3_out(:,:,:), 'U', 1._wp )
         pe3_out(:,:,:) = pe3_out(:,:,:) + e3u_0(:,:,:)
         !
      CASE( 'V' )                   !* from T- to V-point : hor. surface weighted mean
         SELECT CASE ( nn_vvl_interp )
         CASE ( 0 ) 
            !
            DO_3D( 1, 0, 1, 0, 1, jpk )
               pe3_out(ji,jj,jk) = 0.5_wp * ( vmask(ji,jj,jk)  * (1.0_wp - zlnwd) + zlnwd ) * r1_e1e2v(ji,jj)   &
                  &                       * (   e1e2t(ji,jj  ) * ( pe3_in(ji,jj  ,jk) - e3t_0(ji,jj  ,jk) )     &
                  &                           + e1e2t(ji,jj+1) * ( pe3_in(ji,jj+1,jk) - e3t_0(ji,jj+1,jk) ) )
            END_3D
            !
         CASE ( 1 )
                        !
            DO_3D( 1, 0, 1, 0, 1, jpk )
               pe3_out(ji,jj,jk) = 0.5_wp * ( vmask(ji,jj,jk)  * (1.0_wp - zlnwd) + zlnwd ) * r1_e1e2v(ji,jj)   &
                  &                       * (   e1e2t(ji,jj  ) * ( pe3_in(ji,jj  ,jk) - e3t_0(ji,jj  ,jk) )     &
                  &                           + e1e2t(ji,jj+1) * ( pe3_in(ji,jj+1,jk) - e3t_0(ji,jj+1,jk) ) )
            END_3D
            !
            ! Bottom correction:
            DO_2D( 1, 0, 1, 0 )
               ikv    = mbkv(ji  ,jj)
               ikvm1  = ikv - 1
               pe3_out(ji,jj,ikv) = ( vmask(ji,jj,ikv) * (1.0_wp - zlnwd) + zlnwd )    & 
                  &     * ( 0.5_wp *  r1_e1e2v(ji,jj)                                  &
                  &     * (    e1e2t(ji,jj  ) * ( SUM(tmask(ji,jj  ,:)*(pe3_in(ji,jj  ,:) - e3t_0(ji,jj  ,:))) )   &               
                  &          + e1e2t(ji,jj+1) * ( SUM(tmask(ji,jj+1,:)*(pe3_in(ji,jj+1,:) - e3t_0(ji,jj+1,:))) ) ) &
                  &     - SUM(pe3_out(ji,jj,1:ikvm1)))
            END_2D
            !
            CASE ( 2 ) 
            zssh(:,:) = SUM(tmask(:,:,:)*(pe3_in(:,:,:)-e3t_0(:,:,:)), DIM=3)
            !
            DO_3D( 1, 0, 1, 0, 1, jpk )
               pe3_out(ji,jj,jk) = 0.5_wp * (  vmask(ji,jj,jk) * (1.0_wp - zlnwd) + zlnwd ) * r1_e1e2v(ji,jj)    &
                  &                       * (   e1e2t(ji  ,jj) * zssh(ji  ,jj) + e1e2t(ji,jj+1) * zssh(ji,jj+1)) &
                  &                       * e3v_0(ji,jj,jk) / ( hv_0(ji,jj) + 1._wp - ssvmask(ji,jj) )
            END_3D 
            !
         END SELECT
         !
         CALL lbc_lnk( 'domvvl', pe3_out(:,:,:), 'V', 1._wp )
         pe3_out(:,:,:) = pe3_out(:,:,:) + e3v_0(:,:,:)
         !
      CASE( 'F' )                   !* from U-point to F-point : hor. surface weighted mean
         SELECT CASE ( nn_vvl_interp )
         CASE ( 0 )
            !
            DO_3D( 0, 0, 0, 0, 1, jpk )
               pe3_out(ji,jj,jk) = 0.5_wp * (  umask(ji,jj,jk) * umask(ji,jj+1,jk) * (1.0_wp - zlnwd) + zlnwd ) &
                  &                       *    r1_e1e2f(ji,jj)                                                  &
                  &                       * (   e1e2u(ji,jj  ) * ( pe3_in(ji,jj  ,jk) - e3u_0(ji,jj  ,jk) )     &
                  &                           + e1e2u(ji,jj+1) * ( pe3_in(ji,jj+1,jk) - e3u_0(ji,jj+1,jk) ) )
            END_3D
            !
         CASE ( 1 )
            !
            DO_3D( 0, 0, 0, 0, 1, jpk )
               pe3_out(ji,jj,jk) = 0.5_wp * (  umask(ji,jj,jk) * umask(ji,jj+1,jk) * (1.0_wp - zlnwd) + zlnwd ) &
                  &                       *    r1_e1e2f(ji,jj)                                                  &
                  &                       * (   e1e2u(ji,jj  ) * ( pe3_in(ji,jj  ,jk) - e3u_0(ji,jj  ,jk) )     &
                  &                           + e1e2u(ji,jj+1) * ( pe3_in(ji,jj+1,jk) - e3u_0(ji,jj+1,jk) ) )
            END_3D
            !
            ! Bottom correction:
            DO_2D( 0, 0, 0, 0 )
               ikf    = MIN(mbku(ji  ,jj),mbku(ji,jj+1))
               ikfm1  = ikf - 1
               pe3_out(ji,jj,ikf) = ( umask(ji,jj,ikf) * umask(ji,jj+1,ikf) * (1.0_wp - zlnwd) + zlnwd )           & 
                  &     * ( 0.5_wp *  r1_e1e2f(ji,jj)                                                              &
                  &     * (    e1e2u(ji,jj  ) * ( SUM(umask(ji,jj  ,:)*(pe3_in(ji,jj  ,:) - e3u_0(ji,jj  ,:))) )   &               
                  &          + e1e2u(ji,jj+1) * ( SUM(umask(ji,jj+1,:)*(pe3_in(ji,jj+1,:) - e3u_0(ji,jj+1,:))) ) ) &
                  &     - SUM(pe3_out(ji,jj,1:ikfm1)))
            END_2D
            !
         CASE ( 2 ) 
            zssh(:,:) = SUM(umask(:,:,:)*(pe3_in(:,:,:)-e3u_0(:,:,:)), DIM=3)
            !
            DO_3D( 0, 0, 0, 0, 1, jpk )
               pe3_out(ji,jj,jk) =  (  umask(ji,jj,jk)* umask(ji,jj+1,jk) * (1.0_wp - zlnwd) + zlnwd )   &
                  &                 * 0.5_wp * r1_e1e2f(ji,jj)                                           &
                  &                 * (e1e2u(ji  ,jj) * zssh(ji  ,jj) + e1e2u(ji,jj+1) * zssh(ji,jj+1))  &
                  &                 * e3f_0(ji,jj,jk) / ( hf_0(ji,jj) + 1._wp - ssumask(ji,jj)*ssumask(ji,jj+1) )
            END_3D
            !
         END SELECT
         !
         CALL lbc_lnk( 'domvvl', pe3_out(:,:,:), 'F', 1._wp )
         pe3_out(:,:,:) = pe3_out(:,:,:) + e3f_0(:,:,:)
         !
      CASE( 'W' )                   !* from T- to W-point : vertical simple mean
         !
         pe3_out(:,:,1) = e3w_0(:,:,1) + pe3_in(:,:,1) - e3t_0(:,:,1)
         ! - ML - The use of mask in this formulea enables the special treatment of the last w-point without indirect adressing
!!gm BUG? use here wmask in case of ISF ?  to be checked
         DO jk = 2, jpk
            pe3_out(:,:,jk) = e3w_0(:,:,jk) + ( 1.0_wp - 0.5_wp * ( tmask(:,:,jk) * (1.0_wp - zlnwd) + zlnwd ) )   &
               &                            * ( pe3_in(:,:,jk-1) - e3t_0(:,:,jk-1) )                               &
               &                            +            0.5_wp * ( tmask(:,:,jk) * (1.0_wp - zlnwd) + zlnwd )     &
               &                            * ( pe3_in(:,:,jk  ) - e3t_0(:,:,jk  ) )
         END DO
         !
      CASE( 'UW' )                  !* from U- to UW-point : vertical simple mean
         !
         pe3_out(:,:,1) = e3uw_0(:,:,1) + pe3_in(:,:,1) - e3u_0(:,:,1)
         ! - ML - The use of mask in this formaula enables the special treatment of the last w- point without indirect adressing
!!gm BUG? use here wumask in case of ISF ?  to be checked
         DO jk = 2, jpk
            pe3_out(:,:,jk) = e3uw_0(:,:,jk) + ( 1.0_wp - 0.5_wp * ( umask(:,:,jk) * (1.0_wp - zlnwd) + zlnwd ) )  &
               &                             * ( pe3_in(:,:,jk-1) - e3u_0(:,:,jk-1) )                              &
               &                             +            0.5_wp * ( umask(:,:,jk) * (1.0_wp - zlnwd) + zlnwd )    &
               &                             * ( pe3_in(:,:,jk  ) - e3u_0(:,:,jk  ) )
         END DO
         !
      CASE( 'VW' )                  !* from V- to VW-point : vertical simple mean
         !
         pe3_out(:,:,1) = e3vw_0(:,:,1) + pe3_in(:,:,1) - e3v_0(:,:,1)
         ! - ML - The use of mask in this formaula enables the special treatment of the last w- point without indirect adressing
!!gm BUG? use here wvmask in case of ISF ?  to be checked
         DO jk = 2, jpk
            pe3_out(:,:,jk) = e3vw_0(:,:,jk) + ( 1.0_wp - 0.5_wp * ( vmask(:,:,jk) * (1.0_wp - zlnwd) + zlnwd ) )  &
               &                             * ( pe3_in(:,:,jk-1) - e3v_0(:,:,jk-1) )                              &
               &                             +            0.5_wp * ( vmask(:,:,jk) * (1.0_wp - zlnwd) + zlnwd )    &
               &                             * ( pe3_in(:,:,jk  ) - e3v_0(:,:,jk  ) )
         END DO
      END SELECT
      !
   END SUBROUTINE dom_vvl_interpol


   SUBROUTINE dom_vvl_rst( kt, Kbb, Kmm, cdrw )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE dom_vvl_rst  ***
      !!
      !! ** Purpose :   Read or write VVL file in restart file
      !!
      !! ** Method  : * restart comes from a linear ssh simulation :
      !!                   an attempt to read e3t_n stops simulation
      !!              * restart comes from a z-star, z-tilde, or layer :
      !!                   read e3t_n and e3t_b
      !!              * restart comes from a z-star :
      !!                   set tilde_e3t_n, tilde_e3t_n, and hdiv_lf to 0
      !!              * restart comes from layer :
      !!                   read tilde_e3t_n and tilde_e3t_b
      !!                   set hdiv_lf to 0
      !!              * restart comes from a z-tilde:
      !!                   read tilde_e3t_n, tilde_e3t_b, and hdiv_lf
      !!
      !!              NB: if l_1st_euler = T (ln_1st_euler or ssh_b not found)
      !!                   Kbb fields set to Kmm ones
      !!----------------------------------------------------------------------
      INTEGER         , INTENT(in) ::   kt        ! ocean time-step
      INTEGER         , INTENT(in) ::   Kbb, Kmm  ! ocean time level indices
      CHARACTER(len=*), INTENT(in) ::   cdrw      ! "READ"/"WRITE" flag
      !
      INTEGER ::   ji, jj, jk      ! dummy loop indices
      INTEGER ::   id2, id3, id4, id5   ! local integers
      !!----------------------------------------------------------------------
      !
      !                                      !=====================!
      IF( TRIM(cdrw) == 'READ' ) THEN        !  Read / initialise  !
         !                                   !=====================!
         !
         IF( ln_rstart ) THEN                   !==  Read the restart file  ==!
            !
            CALL rst_read_open                                          !*  open the restart file if necessary
            !                                         ! --------- !
            !                                         ! all cases !
            !                                         ! --------- !
            !
            id2 = iom_varid( numror, 'e3t_n'      , ldstop = .FALSE. )  !*  check presence
            id3 = iom_varid( numror, 'tilde_e3t_b', ldstop = .FALSE. )
            id4 = iom_varid( numror, 'tilde_e3t_n', ldstop = .FALSE. )
            id5 = iom_varid( numror, 'hdiv_lf'    , ldstop = .FALSE. )
            !
            !                                                           !*  scale factors
            !  hot restart case with zstar coordinate:
            IF ( id2 > 0 ) THEN
               IF(lwp) WRITE(numout,*)    '          Kmm scale factor read in the restart file'
               CALL iom_get( numror, jpdom_auto, 'e3t_n', e3t(:,:,:,Kmm) )
               WHERE ( tmask(:,:,:) == 0.0_wp )
                  e3t(:,:,:,Kmm) = e3t_0(:,:,:)
               END WHERE
            ELSE
               DO jk = 1, jpk
                  e3t(:,:,jk,Kmm) =  e3t_0(:,:,jk) * ( 1._wp + ssh(:,:,Kmm) * r1_ht_0(:,:) * tmask(:,:,jk) )
               END DO
            ENDIF

            IF( l_1st_euler ) THEN                       ! euler
               IF(lwp) WRITE(numout,*) '          Euler first time step : e3t(Kbb) = e3t(Kmm)'
               e3t(:,:,:,Kbb) = e3t(:,:,:,Kmm)
            ELSE                                         ! leap frog
               IF(lwp) WRITE(numout,*) '          Kbb scale factor read in the restart file'
               CALL iom_get( numror, jpdom_auto, 'e3t_b', e3t(:,:,:,Kbb) )
               WHERE ( tmask(:,:,:) == 0.0_wp )
                  e3t(:,:,:,Kbb) = e3t_0(:,:,:)
               END WHERE
            ENDIF
            !                                         ! ------------ !
            IF( ln_vvl_zstar ) THEN                   !  z_star case !
               !                                      ! ------------ !
               IF( MIN( id3, id4 ) > 0 ) THEN
                  CALL ctl_stop( 'dom_vvl_rst: z_star cannot restart from a z_tilde or layer run' )
               ENDIF
               !                                      ! ------------------------ !
            ELSE                                      !  z_tilde and layer cases !
               !                                      ! ------------------------ !
               !
               IF( id4 > 0 ) THEN                                       !*  scale factor increments
                  IF(lwp) WRITE(numout,*)    '          Kmm scale factor increments read in the restart file'
                  CALL iom_get( numror, jpdom_auto, 'tilde_e3t_n', tilde_e3t_n(:,:,:) )
                  IF( l_1st_euler ) THEN                 ! euler
                     IF(lwp) WRITE(numout,*) '          Euler first time step : tilde_e3t(Kbb) = tilde_e3t(Kmm)'
                     tilde_e3t_b(:,:,:) = tilde_e3t_n(:,:,:)
                  ELSE                                   ! leap frog
                     IF(lwp) WRITE(numout,*) '          Kbb scale factor increments read in the restart file'
                     CALL iom_get( numror, jpdom_auto, 'tilde_e3t_b', tilde_e3t_b(:,:,:) )
                  ENDIF
               ELSE
                  tilde_e3t_b(:,:,:) = 0.0_wp
                  tilde_e3t_n(:,:,:) = 0.0_wp
               ENDIF
               !                                      ! ------------ !
               IF( ln_vvl_ztilde ) THEN               ! z_tilde case !
                  !                                   ! ------------ !
                  IF( id5 > 0 ) THEN  ! required array exists
                     CALL iom_get( numror, jpdom_auto, 'hdiv_lf', hdiv_lf(:,:,:) )
                  ELSE                ! array is missing
                     hdiv_lf(:,:,:) = 0.0_wp
                  ENDIF
               ENDIF
            ENDIF
            !
         ELSE                                   !==  Initialize at "rest" with ssh  ==!
            !
            DO jk = 1, jpk
               e3t(:,:,jk,Kmm) =  e3t_0(:,:,jk) * ( 1._wp + ssh(:,:,Kmm) * r1_ht_0(:,:) * tmask(:,:,jk) )
            END DO
            e3t(:,:,:,Kbb) = e3t(:,:,:,Kmm)
            !
            IF( ln_vvl_ztilde .OR. ln_vvl_layer) THEN
               tilde_e3t_b(:,:,:) = 0._wp
               tilde_e3t_n(:,:,:) = 0._wp
               IF( ln_vvl_ztilde ) hdiv_lf(:,:,:) = 0._wp
            ENDIF
         ENDIF
         !                                       !=======================!
      ELSEIF( TRIM(cdrw) == 'WRITE' ) THEN       !  Create restart file  !
         !                                       !=======================!
         !
         IF(lwp) WRITE(numout,*) '---- dom_vvl_rst ----'
         !                                           ! --------- !
         !                                           ! all cases !
         !                                           ! --------- !
         CALL iom_rstput( kt, nitrst, numrow, 'e3t_b', e3t(:,:,:,Kbb) )
         CALL iom_rstput( kt, nitrst, numrow, 'e3t_n', e3t(:,:,:,Kmm) )
         !                                           ! ----------------------- !
         IF( ln_vvl_ztilde .OR. ln_vvl_layer ) THEN  ! z_tilde and layer cases !
            !                                        ! ----------------------- !
            CALL iom_rstput( kt, nitrst, numrow, 'tilde_e3t_b', tilde_e3t_b(:,:,:))
            CALL iom_rstput( kt, nitrst, numrow, 'tilde_e3t_n', tilde_e3t_n(:,:,:))
         END IF
         !                                           ! -------------!
         IF( ln_vvl_ztilde ) THEN                    ! z_tilde case !
            !                                        ! ------------ !
            CALL iom_rstput( kt, nitrst, numrow, 'hdiv_lf', hdiv_lf(:,:,:))
         ENDIF
         !
      ENDIF
      !
   END SUBROUTINE dom_vvl_rst


   SUBROUTINE dom_vvl_ctl
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE dom_vvl_ctl  ***
      !!
      !! ** Purpose :   Control the consistency between namelist options
      !!                for vertical coordinate
      !!----------------------------------------------------------------------
      INTEGER ::   ioptio, ios
      !!
      NAMELIST/nam_vvl/ ln_vvl_zstar, ln_vvl_ztilde, ln_vvl_layer, ln_vvl_ztilde_as_zstar, &
         &              ln_vvl_zstar_at_eqtor      , rn_ahe3     , rn_rst_e3t            , &
         &              rn_lf_cutoff               , rn_zdef_max , ln_vvl_dbg            , &  ! not yet implemented: ln_vvl_kepe
         &              nn_vvl_interp
      !!----------------------------------------------------------------------
      !
      READ  ( numnam_ref, nam_vvl, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'nam_vvl in reference namelist' )
      READ  ( numnam_cfg, nam_vvl, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 ) CALL ctl_nam ( ios , 'nam_vvl in configuration namelist' )
      IF(lwm) WRITE ( numond, nam_vvl )
      !
      IF(lwp) THEN                    ! Namelist print
         WRITE(numout,*)
         WRITE(numout,*) 'dom_vvl_ctl : choice/control of the variable vertical coordinate'
         WRITE(numout,*) '~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist nam_vvl : chose a vertical coordinate'
         WRITE(numout,*) '      zstar                      ln_vvl_zstar   = ', ln_vvl_zstar
         WRITE(numout,*) '      ztilde                     ln_vvl_ztilde  = ', ln_vvl_ztilde
         WRITE(numout,*) '      layer                      ln_vvl_layer   = ', ln_vvl_layer
         WRITE(numout,*) '      ztilde as zstar   ln_vvl_ztilde_as_zstar  = ', ln_vvl_ztilde_as_zstar
         WRITE(numout,*) '      ztilde near the equator    ln_vvl_zstar_at_eqtor  = ', ln_vvl_zstar_at_eqtor
         WRITE(numout,*) '      !'
         WRITE(numout,*) '      thickness diffusion coefficient                      rn_ahe3      = ', rn_ahe3
         WRITE(numout,*) '      maximum e3t deformation fractional change            rn_zdef_max  = ', rn_zdef_max
         IF( ln_vvl_ztilde_as_zstar ) THEN
            WRITE(numout,*) '      ztilde running in zstar emulation mode (ln_vvl_ztilde_as_zstar=T) '
            WRITE(numout,*) '         ignoring namelist timescale parameters and using:'
            WRITE(numout,*) '            hard-wired : z-tilde to zstar restoration timescale (days)'
            WRITE(numout,*) '                         rn_rst_e3t     = 0.e0'
            WRITE(numout,*) '            hard-wired : z-tilde cutoff frequency of low-pass filter (days)'
            WRITE(numout,*) '                         rn_lf_cutoff   = 1.0/rn_Dt'
         ELSE
            WRITE(numout,*) '      z-tilde to zstar restoration timescale (days)        rn_rst_e3t   = ', rn_rst_e3t
            WRITE(numout,*) '      z-tilde cutoff frequency of low-pass filter (days)   rn_lf_cutoff = ', rn_lf_cutoff
         ENDIF
         WRITE(numout,*) '         debug prints flag                                 ln_vvl_dbg   = ', ln_vvl_dbg
         WRITE(numout,*) '         Method to compute scale factors anomaly at U/V/F points  nn_vvl_interp   = ', nn_vvl_interp
      ENDIF
      !
      ioptio = 0                      ! Parameter control
      IF( ln_vvl_ztilde_as_zstar )   ln_vvl_ztilde = .true.
      IF( ln_vvl_zstar           )   ioptio = ioptio + 1
      IF( ln_vvl_ztilde          )   ioptio = ioptio + 1
      IF( ln_vvl_layer           )   ioptio = ioptio + 1
      !
      IF( ioptio /= 1 )   CALL ctl_stop( 'Choose ONE vertical coordinate in namelist nam_vvl' )
      !
      IF( .NOT. ln_vvl_zstar .AND. (nn_vvl_interp==2 ) )  CALL ctl_stop( 'nn_vvl_interp must be < 2 if ln_vvl_zstar=F' )
      !
      IF(lwp) THEN                   ! Print the choice
         WRITE(numout,*)
         IF( ln_vvl_zstar           ) WRITE(numout,*) '      ==>>>   zstar vertical coordinate is used'
         IF( ln_vvl_ztilde          ) WRITE(numout,*) '      ==>>>   ztilde vertical coordinate is used'
         IF( ln_vvl_layer           ) WRITE(numout,*) '      ==>>>   layer vertical coordinate is used'
         IF( ln_vvl_ztilde_as_zstar ) WRITE(numout,*) '      ==>>>   to emulate a zstar coordinate'
      ENDIF
      !
#if defined key_agrif
      IF( (.NOT.Agrif_Root()).AND.(.NOT.ln_vvl_zstar) )   CALL ctl_stop( 'AGRIF is implemented with zstar coordinate only' )
#endif
      !
   END SUBROUTINE dom_vvl_ctl

#endif

   !!======================================================================
END MODULE domvvl
