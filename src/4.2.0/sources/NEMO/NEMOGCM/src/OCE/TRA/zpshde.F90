MODULE zpshde
   !!======================================================================
   !!                       ***  MODULE zpshde   ***
   !! z-coordinate + partial step : Horizontal Derivative at ocean bottom level
   !!======================================================================
   !! History :  OPA  !  2002-04  (A. Bozec)  Original code
   !!   NEMO     1.0  !  2002-08  (G. Madec E. Durand)  Optimization and Free form
   !!             -   !  2004-03  (C. Ethe)  adapted for passive tracers
   !!            3.3  !  2010-05  (C. Ethe, G. Madec)  merge TRC-TRA
   !!            3.6  !  2014-11  (P. Mathiot) Add zps_hde_isf (needed to open a cavity)
   !!======================================================================

   !!----------------------------------------------------------------------
   !!   zps_hde      :  Horizontal DErivative of T, S and rd at the last
   !!                   ocean level (Z-coord. with Partial Steps)
   !!----------------------------------------------------------------------
   USE oce             ! ocean: dynamics and tracers variables
   USE dom_oce         ! domain: ocean variables
   USE domutl, ONLY : is_tile
   USE phycst          ! physical constants
   USE eosbn2          ! ocean equation of state
   USE in_out_manager  ! I/O manager
   USE lbclnk          ! lateral boundary conditions (or mpp link)
   USE lib_mpp         ! MPP library
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   zps_hde     ! routine called by step.F90
   PUBLIC   zps_hde_isf ! routine called by step.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: zpshde.F90 14834 2021-05-11 09:24:44Z hadcv $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE zps_hde( kt, Kmm, kjpt, pta, pgtu, pgtv,  &
      &                               prd, pgru, pgrv )
      !!
      INTEGER                     , INTENT(in   )           ::  kt          ! ocean time-step index
      INTEGER                     , INTENT(in   )           ::  Kmm         ! ocean time level index
      INTEGER                     , INTENT(in   )           ::  kjpt        ! number of tracers
      REAL(wp), DIMENSION(:,:,:,:), INTENT(in   )           ::  pta         ! 4D tracers fields
      REAL(wp), DIMENSION(:,:,:)  , INTENT(  out)           ::  pgtu, pgtv  ! hor. grad. of ptra at u- & v-pts
      REAL(wp), DIMENSION(:,:,:)  , INTENT(in   ), OPTIONAL ::  prd         ! 3D density anomaly fields
      REAL(wp), DIMENSION(:,:)    , INTENT(  out), OPTIONAL ::  pgru, pgrv  ! hor. grad of prd at u- & v-pts (bottom)
      !
      INTEGER :: itrd, itgr
      !!
      IF( PRESENT(prd)  ) THEN ; itrd = is_tile(prd)  ; ELSE ; itrd = 0 ; ENDIF
      IF( PRESENT(pgru) ) THEN ; itgr = is_tile(pgru) ; ELSE ; itgr = 0 ; ENDIF

      CALL zps_hde_t( kt, Kmm, kjpt, pta, is_tile(pta), pgtu, pgtv, is_tile(pgtu), &
         &                           prd, itrd,         pgru, pgrv, itgr )
   END SUBROUTINE zps_hde


   SUBROUTINE zps_hde_t( kt, Kmm, kjpt, pta, ktta, pgtu, pgtv, ktgt,   &
      &                                 prd, ktrd, pgru, pgrv, ktgr )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE zps_hde  ***
      !!
      !! ** Purpose :   Compute the horizontal derivative of T, S and rho
      !!      at u- and v-points with a linear interpolation for z-coordinate
      !!      with partial steps.
      !!
      !! ** Method  :   In z-coord with partial steps, scale factors on last
      !!      levels are different for each grid point, so that T, S and rd
      !!      points are not at the same depth as in z-coord. To have horizontal
      !!      gradients again, we interpolate T and S at the good depth :
      !!      Linear interpolation of T, S
      !!         Computation of di(tb) and dj(tb) by vertical interpolation:
      !!          di(t) = t~ - t(i,j,k) or t(i+1,j,k) - t~
      !!          dj(t) = t~ - t(i,j,k) or t(i,j+1,k) - t~
      !!         This formulation computes the two cases:
      !!                 CASE 1                   CASE 2
      !!         k-1  ___ ___________   k-1   ___ ___________
      !!                    Ti  T~                  T~  Ti+1
      !!                  _____                        _____
      !!         k        |   |Ti+1     k           Ti |   |
      !!                  |   |____                ____|   |
      !!              ___ |   |   |           ___  |   |   |
      !!
      !!      case 1->   e3w(i+1,:,:,Kmm) >= e3w(i,:,:,Kmm) ( and e3w(:,j+1,:,Kmm) >= e3w(:,j,:,Kmm) ) then
      !!          t~ = t(i+1,j  ,k) + (e3w(i+1,j,k,Kmm) - e3w(i,j,k,Kmm)) * dk(Ti+1)/e3w(i+1,j,k,Kmm)
      !!        ( t~ = t(i  ,j+1,k) + (e3w(i,j+1,k,Kmm) - e3w(i,j,k,Kmm)) * dk(Tj+1)/e3w(i,j+1,k,Kmm)  )
      !!          or
      !!      case 2->   e3w(i+1,:,:,Kmm) <= e3w(i,:,:,Kmm) ( and e3w(:,j+1,:,Kmm) <= e3w(:,j,:,Kmm) ) then
      !!          t~ = t(i,j,k) + (e3w(i,j,k,Kmm) - e3w(i+1,j,k,Kmm)) * dk(Ti)/e3w(i,j,k,Kmm)
      !!        ( t~ = t(i,j,k) + (e3w(i,j,k,Kmm) - e3w(i,j+1,k,Kmm)) * dk(Tj)/e3w(i,j,k,Kmm) )
      !!          Idem for di(s) and dj(s)
      !!
      !!      For rho, we call eos which will compute rd~(t~,s~) at the right
      !!      depth zh from interpolated T and S for the different formulations
      !!      of the equation of state (eos).
      !!      Gradient formulation for rho :
      !!          di(rho) = rd~ - rd(i,j,k)   or   rd(i+1,j,k) - rd~
      !!
      !! ** Action  : compute for top interfaces
      !!              - pgtu, pgtv: horizontal gradient of tracer at u- & v-points
      !!              - pgru, pgrv: horizontal gradient of rho (if present) at u- & v-points
      !!----------------------------------------------------------------------
      INTEGER                                , INTENT(in   )           ::  kt          ! ocean time-step index
      INTEGER                                , INTENT(in   )           ::  Kmm         ! ocean time level index
      INTEGER                                , INTENT(in   )           ::  kjpt        ! number of tracers
      INTEGER                                , INTENT(in   )           ::  ktta, ktgt, ktrd, ktgr
      REAL(wp), DIMENSION(A2D_T(ktta),JPK,KJPT), INTENT(in   )           ::  pta         ! 4D tracers fields
      REAL(wp), DIMENSION(A2D_T(ktgt)    ,KJPT), INTENT(  out)           ::  pgtu, pgtv  ! hor. grad. of ptra at u- & v-pts
      REAL(wp), DIMENSION(A2D_T(ktrd),JPK     ), INTENT(in   ), OPTIONAL ::  prd         ! 3D density anomaly fields
      REAL(wp), DIMENSION(A2D_T(ktgr)         ), INTENT(  out), OPTIONAL ::  pgru, pgrv  ! hor. grad of prd at u- & v-pts (bottom)
      !
      INTEGER  ::   ji, jj, jn                  ! Dummy loop indices
      INTEGER  ::   iku, ikv, ikum1, ikvm1      ! partial step level (ocean bottom level) at u- and v-points
      REAL(wp) ::   ze3wu, ze3wv, zmaxu, zmaxv  ! local scalars
      REAL(wp), DIMENSION(A2D(nn_hls))      ::   zri, zrj, zhi, zhj   ! NB: 3rd dim=1 to use eos
      REAL(wp), DIMENSION(A2D(nn_hls),kjpt) ::   zti, ztj             !
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start( 'zps_hde')
      !
      pgtu(:,:,:) = 0._wp   ;   zti (:,:,:) = 0._wp   ;   zhi (:,:) = 0._wp
      pgtv(:,:,:) = 0._wp   ;   ztj (:,:,:) = 0._wp   ;   zhj (:,:) = 0._wp
      !
      DO jn = 1, kjpt      !==   Interpolation of tracers at the last ocean level   ==!
         !
         DO_2D( nn_hls, nn_hls-1, nn_hls, nn_hls-1 )              ! Gradient of density at the last level
            iku = mbku(ji,jj)   ;   ikum1 = MAX( iku - 1 , 1 )    ! last and before last ocean level at u- & v-points
            ikv = mbkv(ji,jj)   ;   ikvm1 = MAX( ikv - 1 , 1 )    ! if level first is a p-step, ik.m1=1
!!gm BUG ? when applied to before fields, e3w(:,:,k,Kbb) should be used....
            ze3wu = e3w(ji+1,jj  ,iku,Kmm) - e3w(ji,jj,iku,Kmm)
            ze3wv = e3w(ji  ,jj+1,ikv,Kmm) - e3w(ji,jj,ikv,Kmm)
            !
            ! i- direction
            IF( ze3wu >= 0._wp ) THEN      ! case 1
               zmaxu =  ze3wu / e3w(ji+1,jj,iku,Kmm)
               ! interpolated values of tracers
               zti (ji,jj,jn) = pta(ji+1,jj,iku,jn) + zmaxu * ( pta(ji+1,jj,ikum1,jn) - pta(ji+1,jj,iku,jn) )
               ! gradient of  tracers
               pgtu(ji,jj,jn) = umask(ji,jj,1) * ( zti(ji,jj,jn) - pta(ji,jj,iku,jn) )
            ELSE                           ! case 2
               zmaxu = -ze3wu / e3w(ji,jj,iku,Kmm)
               ! interpolated values of tracers
               zti (ji,jj,jn) = pta(ji,jj,iku,jn) + zmaxu * ( pta(ji,jj,ikum1,jn) - pta(ji,jj,iku,jn) )
               ! gradient of tracers
               pgtu(ji,jj,jn) = umask(ji,jj,1) * ( pta(ji+1,jj,iku,jn) - zti(ji,jj,jn) )
            ENDIF
            !
            ! j- direction
            IF( ze3wv >= 0._wp ) THEN      ! case 1
               zmaxv =  ze3wv / e3w(ji,jj+1,ikv,Kmm)
               ! interpolated values of tracers
               ztj (ji,jj,jn) = pta(ji,jj+1,ikv,jn) + zmaxv * ( pta(ji,jj+1,ikvm1,jn) - pta(ji,jj+1,ikv,jn) )
               ! gradient of tracers
               pgtv(ji,jj,jn) = vmask(ji,jj,1) * ( ztj(ji,jj,jn) - pta(ji,jj,ikv,jn) )
            ELSE                           ! case 2
               zmaxv =  -ze3wv / e3w(ji,jj,ikv,Kmm)
               ! interpolated values of tracers
               ztj (ji,jj,jn) = pta(ji,jj,ikv,jn) + zmaxv * ( pta(ji,jj,ikvm1,jn) - pta(ji,jj,ikv,jn) )
               ! gradient of tracers
               pgtv(ji,jj,jn) = vmask(ji,jj,1) * ( pta(ji,jj+1,ikv,jn) - ztj(ji,jj,jn) )
            ENDIF
         END_2D
      END DO
      !
      IF (nn_hls==1) CALL lbc_lnk( 'zpshde', pgtu(:,:,:), 'U', -1.0_wp , pgtv(:,:,:), 'V', -1.0_wp )   ! Lateral boundary cond.
      !
      IF( PRESENT( prd ) ) THEN    !==  horizontal derivative of density anomalies (rd)  ==!    (optional part)
         pgru(:,:) = 0._wp
         pgrv(:,:) = 0._wp                ! depth of the partial step level
         DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
            iku = mbku(ji,jj)
            ikv = mbkv(ji,jj)
            ze3wu  = e3w(ji+1,jj  ,iku,Kmm) - e3w(ji,jj,iku,Kmm)
            ze3wv  = e3w(ji  ,jj+1,ikv,Kmm) - e3w(ji,jj,ikv,Kmm)
            IF( ze3wu >= 0._wp ) THEN   ;   zhi(ji,jj) = gdept(ji  ,jj,iku,Kmm)     ! i-direction: case 1
            ELSE                        ;   zhi(ji,jj) = gdept(ji+1,jj,iku,Kmm)     ! -     -      case 2
            ENDIF
            IF( ze3wv >= 0._wp ) THEN   ;   zhj(ji,jj) = gdept(ji,jj  ,ikv,Kmm)     ! j-direction: case 1
            ELSE                        ;   zhj(ji,jj) = gdept(ji,jj+1,ikv,Kmm)     ! -     -      case 2
            ENDIF
         END_2D
         !
         CALL eos( zti, zhi, zri )        ! interpolated density from zti, ztj
         CALL eos( ztj, zhj, zrj )        ! at the partial step depth output in  zri, zrj
         !
         DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )              ! Gradient of density at the last level
            iku = mbku(ji,jj)
            ikv = mbkv(ji,jj)
            ze3wu  = e3w(ji+1,jj  ,iku,Kmm) - e3w(ji,jj,iku,Kmm)
            ze3wv  = e3w(ji  ,jj+1,ikv,Kmm) - e3w(ji,jj,ikv,Kmm)
            IF( ze3wu >= 0._wp ) THEN   ;   pgru(ji,jj) = umask(ji,jj,1) * ( zri(ji  ,jj    ) - prd(ji,jj,iku) )   ! i: 1
            ELSE                        ;   pgru(ji,jj) = umask(ji,jj,1) * ( prd(ji+1,jj,iku) - zri(ji,jj    ) )   ! i: 2
            ENDIF
            IF( ze3wv >= 0._wp ) THEN   ;   pgrv(ji,jj) = vmask(ji,jj,1) * ( zrj(ji,jj      ) - prd(ji,jj,ikv) )   ! j: 1
            ELSE                        ;   pgrv(ji,jj) = vmask(ji,jj,1) * ( prd(ji,jj+1,ikv) - zrj(ji,jj    ) )   ! j: 2
            ENDIF
         END_2D
         IF (nn_hls==1) CALL lbc_lnk( 'zpshde', pgru , 'U', -1.0_wp , pgrv , 'V', -1.0_wp )   ! Lateral boundary conditions
         !
      END IF
      !
      IF( ln_timing )   CALL timing_stop( 'zps_hde')
      !
   END SUBROUTINE zps_hde_t


   SUBROUTINE zps_hde_isf( kt, Kmm, kjpt, pta, pgtu, pgtv, pgtui, pgtvi,  &
      &                                   prd, pgru, pgrv, pgrui, pgrvi )
      !!
      INTEGER                     , INTENT(in   )           ::  kt           ! ocean time-step index
      INTEGER                     , INTENT(in   )           ::  Kmm          ! ocean time level index
      INTEGER                     , INTENT(in   )           ::  kjpt         ! number of tracers
      REAL(wp), DIMENSION(:,:,:,:), INTENT(in   )           ::  pta          ! 4D tracers fields
      REAL(wp), DIMENSION(:,:,:)  , INTENT(  out)           ::  pgtu, pgtv   ! hor. grad. of ptra at u- & v-pts
      REAL(wp), DIMENSION(:,:,:)  , INTENT(  out)           ::  pgtui, pgtvi ! hor. grad. of stra at u- & v-pts (ISF)
      REAL(wp), DIMENSION(:,:,:)  , INTENT(in   ), OPTIONAL ::  prd          ! 3D density anomaly fields
      REAL(wp), DIMENSION(:,:)    , INTENT(  out), OPTIONAL ::  pgru, pgrv   ! hor. grad of prd at u- & v-pts (bottom)
      REAL(wp), DIMENSION(:,:)    , INTENT(  out), OPTIONAL ::  pgrui, pgrvi ! hor. grad of prd at u- & v-pts (top)
      !
      INTEGER :: itrd, itgr, itgri
      !!
      IF( PRESENT(prd)   ) THEN ; itrd  = is_tile(prd)   ; ELSE ; itrd  = 0 ; ENDIF
      IF( PRESENT(pgru)  ) THEN ; itgr  = is_tile(pgru)  ; ELSE ; itgr  = 0 ; ENDIF
      IF( PRESENT(pgrui) ) THEN ; itgri = is_tile(pgrui) ; ELSE ; itgri = 0 ; ENDIF

      CALL zps_hde_isf_t( kt, Kmm, kjpt, pta, is_tile(pta), pgtu, pgtv, is_tile(pgtu), pgtui, pgtvi, is_tile(pgtui),  &
      &                                  prd, itrd,         pgru, pgrv, itgr,          pgrui, pgrvi, itgri )
   END SUBROUTINE zps_hde_isf


   SUBROUTINE zps_hde_isf_t( kt, Kmm, kjpt, pta, ktta, pgtu, pgtv, ktgt, pgtui, pgtvi, ktgti,  &
      &                                     prd, ktrd, pgru, pgrv, ktgr, pgrui, pgrvi, ktgri )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE zps_hde_isf  ***
      !!
      !! ** Purpose :   Compute the horizontal derivative of T, S and rho
      !!      at u- and v-points with a linear interpolation for z-coordinate
      !!      with partial steps for top (ice shelf) and bottom.
      !!
      !! ** Method  :   In z-coord with partial steps, scale factors on last
      !!      levels are different for each grid point, so that T, S and rd
      !!      points are not at the same depth as in z-coord. To have horizontal
      !!      gradients again, we interpolate T and S at the good depth :
      !!      For the bottom case:
      !!      Linear interpolation of T, S
      !!         Computation of di(tb) and dj(tb) by vertical interpolation:
      !!          di(t) = t~ - t(i,j,k) or t(i+1,j,k) - t~
      !!          dj(t) = t~ - t(i,j,k) or t(i,j+1,k) - t~
      !!         This formulation computes the two cases:
      !!                 CASE 1                   CASE 2
      !!         k-1  ___ ___________   k-1   ___ ___________
      !!                    Ti  T~                  T~  Ti+1
      !!                  _____                        _____
      !!         k        |   |Ti+1     k           Ti |   |
      !!                  |   |____                ____|   |
      !!              ___ |   |   |           ___  |   |   |
      !!
      !!      case 1->   e3w(i+1,j,k,Kmm) >= e3w(i,j,k,Kmm) ( and e3w(i,j+1,k,Kmm) >= e3w(i,j,k,Kmm) ) then
      !!          t~ = t(i+1,j  ,k) + (e3w(i+1,j  ,k,Kmm) - e3w(i,j,k,Kmm)) * dk(Ti+1)/e3w(i+1,j  ,k,Kmm)
      !!        ( t~ = t(i  ,j+1,k) + (e3w(i  ,j+1,k,Kmm) - e3w(i,j,k,Kmm)) * dk(Tj+1)/e3w(i  ,j+1,k,Kmm)  )
      !!          or
      !!      case 2->   e3w(i+1,j,k,Kmm) <= e3w(i,j,k,Kmm) ( and e3w(i,j+1,k,Kmm) <= e3w(i,j,k,Kmm) ) then
      !!          t~ = t(i,j,k) + (e3w(i,j,k,Kmm) - e3w(i+1,j  ,k,Kmm)) * dk(Ti)/e3w(i,j,k,Kmm)
      !!        ( t~ = t(i,j,k) + (e3w(i,j,k,Kmm) - e3w(i  ,j+1,k,Kmm)) * dk(Tj)/e3w(i,j,k,Kmm) )
      !!          Idem for di(s) and dj(s)
      !!
      !!      For rho, we call eos which will compute rd~(t~,s~) at the right
      !!      depth zh from interpolated T and S for the different formulations
      !!      of the equation of state (eos).
      !!      Gradient formulation for rho :
      !!          di(rho) = rd~ - rd(i,j,k)   or   rd(i+1,j,k) - rd~
      !!
      !!      For the top case (ice shelf): As for the bottom case but upside down
      !!
      !! ** Action  : compute for top and bottom interfaces
      !!              - pgtu, pgtv, pgtui, pgtvi: horizontal gradient of tracer at u- & v-points
      !!              - pgru, pgrv, pgrui, pgtvi: horizontal gradient of rho (if present) at u- & v-points
      !!----------------------------------------------------------------------
      INTEGER                                , INTENT(in   )           ::  kt           ! ocean time-step index
      INTEGER                                , INTENT(in   )           ::  Kmm          ! ocean time level index
      INTEGER                                , INTENT(in   )           ::  kjpt         ! number of tracers
      INTEGER                                , INTENT(in   )           ::  ktta, ktgt, ktgti, ktrd, ktgr, ktgri
      REAL(wp), DIMENSION(A2D_T(ktta),JPK,KJPT), INTENT(in   )           ::  pta          ! 4D tracers fields
      REAL(wp), DIMENSION(A2D_T(ktgt)    ,KJPT), INTENT(  out)           ::  pgtu, pgtv   ! hor. grad. of ptra at u- & v-pts
      REAL(wp), DIMENSION(A2D_T(ktgti)   ,KJPT), INTENT(  out)           ::  pgtui, pgtvi ! hor. grad. of stra at u- & v-pts (ISF)
      REAL(wp), DIMENSION(A2D_T(ktrd),JPK     ), INTENT(in   ), OPTIONAL ::  prd          ! 3D density anomaly fields
      REAL(wp), DIMENSION(A2D_T(ktgr)         ), INTENT(  out), OPTIONAL ::  pgru, pgrv   ! hor. grad of prd at u- & v-pts (bottom)
      REAL(wp), DIMENSION(A2D_T(ktgri)        ), INTENT(  out), OPTIONAL ::  pgrui, pgrvi ! hor. grad of prd at u- & v-pts (top)
      !
      INTEGER  ::   ji, jj, jn      ! Dummy loop indices
      INTEGER  ::   iku, ikv, ikum1, ikvm1,ikup1, ikvp1   ! partial step level (ocean bottom level) at u- and v-points
      REAL(wp) ::  ze3wu, ze3wv, zmaxu, zmaxv             ! temporary scalars
      REAL(wp), DIMENSION(A2D(nn_hls))      ::  zri, zrj, zhi, zhj   ! NB: 3rd dim=1 to use eos
      REAL(wp), DIMENSION(A2D(nn_hls),kjpt) ::  zti, ztj             !
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start( 'zps_hde_isf')
      !
      pgtu (:,:,:) = 0._wp   ;   pgtv (:,:,:) =0._wp
      pgtui(:,:,:) = 0._wp   ;   pgtvi(:,:,:) =0._wp
      zti  (:,:,:) = 0._wp   ;   ztj  (:,:,:) =0._wp
      zhi  (:,:  ) = 0._wp   ;   zhj  (:,:  ) =0._wp
      !
      DO jn = 1, kjpt      !==   Interpolation of tracers at the last ocean level   ==!
         !
         DO_2D( nn_hls, nn_hls-1, nn_hls, nn_hls-1 )

            iku = mbku(ji,jj); ikum1 = MAX( iku - 1 , 1 )    ! last and before last ocean level at u- & v-points
            ikv = mbkv(ji,jj); ikvm1 = MAX( ikv - 1 , 1 )    ! if level first is a p-step, ik.m1=1
            ze3wu = gdept(ji+1,jj,iku,Kmm) - gdept(ji,jj,iku,Kmm)
            ze3wv = gdept(ji,jj+1,ikv,Kmm) - gdept(ji,jj,ikv,Kmm)
            !
            ! i- direction
            IF( ze3wu >= 0._wp ) THEN      ! case 1
               zmaxu =  ze3wu / e3w(ji+1,jj,iku,Kmm)
               ! interpolated values of tracers
               zti (ji,jj,jn) = pta(ji+1,jj,iku,jn) + zmaxu * ( pta(ji+1,jj,ikum1,jn) - pta(ji+1,jj,iku,jn) )
               ! gradient of  tracers
               pgtu(ji,jj,jn) = ssumask(ji,jj) * ( zti(ji,jj,jn) - pta(ji,jj,iku,jn) )
            ELSE                           ! case 2
               zmaxu = -ze3wu / e3w(ji,jj,iku,Kmm)
               ! interpolated values of tracers
               zti (ji,jj,jn) = pta(ji,jj,iku,jn) + zmaxu * ( pta(ji,jj,ikum1,jn) - pta(ji,jj,iku,jn) )
               ! gradient of tracers
               pgtu(ji,jj,jn) = ssumask(ji,jj) * ( pta(ji+1,jj,iku,jn) - zti(ji,jj,jn) )
            ENDIF
            !
            ! j- direction
            IF( ze3wv >= 0._wp ) THEN      ! case 1
               zmaxv =  ze3wv / e3w(ji,jj+1,ikv,Kmm)
               ! interpolated values of tracers
               ztj (ji,jj,jn) = pta(ji,jj+1,ikv,jn) + zmaxv * ( pta(ji,jj+1,ikvm1,jn) - pta(ji,jj+1,ikv,jn) )
               ! gradient of tracers
               pgtv(ji,jj,jn) = ssvmask(ji,jj) * ( ztj(ji,jj,jn) - pta(ji,jj,ikv,jn) )
            ELSE                           ! case 2
               zmaxv =  -ze3wv / e3w(ji,jj,ikv,Kmm)
               ! interpolated values of tracers
               ztj (ji,jj,jn) = pta(ji,jj,ikv,jn) + zmaxv * ( pta(ji,jj,ikvm1,jn) - pta(ji,jj,ikv,jn) )
               ! gradient of tracers
               pgtv(ji,jj,jn) = ssvmask(ji,jj) * ( pta(ji,jj+1,ikv,jn) - ztj(ji,jj,jn) )
            ENDIF

         END_2D
      END DO
      !
      IF (nn_hls==1) CALL lbc_lnk( 'zpshde', pgtu(:,:,:), 'U', -1.0_wp , pgtv(:,:,:), 'V', -1.0_wp )   ! Lateral boundary cond.

      ! horizontal derivative of density anomalies (rd)
      IF( PRESENT( prd ) ) THEN         ! depth of the partial step level
         pgru(:,:)=0.0_wp   ; pgrv(:,:)=0.0_wp ;
         !
         DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )

            iku = mbku(ji,jj)
            ikv = mbkv(ji,jj)
            ze3wu = gdept(ji+1,jj,iku,Kmm) - gdept(ji,jj,iku,Kmm)
            ze3wv = gdept(ji,jj+1,ikv,Kmm) - gdept(ji,jj,ikv,Kmm)
            !
            IF( ze3wu >= 0._wp ) THEN   ;   zhi(ji,jj) = gdept(ji  ,jj,iku,Kmm)    ! i-direction: case 1
            ELSE                        ;   zhi(ji,jj) = gdept(ji+1,jj,iku,Kmm)    ! -     -      case 2
            ENDIF
            IF( ze3wv >= 0._wp ) THEN   ;   zhj(ji,jj) = gdept(ji,jj  ,ikv,Kmm)    ! j-direction: case 1
            ELSE                        ;   zhj(ji,jj) = gdept(ji,jj+1,ikv,Kmm)    ! -     -      case 2
            ENDIF

         END_2D

         ! Compute interpolated rd from zti, ztj for the 2 cases at the depth of the partial
         ! step and store it in  zri, zrj for each  case
         CALL eos( zti, zhi, zri )
         CALL eos( ztj, zhj, zrj )

         DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
            iku = mbku(ji,jj)
            ikv = mbkv(ji,jj)
            ze3wu = gdept(ji+1,jj,iku,Kmm) - gdept(ji,jj,iku,Kmm)
            ze3wv = gdept(ji,jj+1,ikv,Kmm) - gdept(ji,jj,ikv,Kmm)

            IF( ze3wu >= 0._wp ) THEN   ;   pgru(ji,jj) = ssumask(ji,jj) * ( zri(ji  ,jj    ) - prd(ji,jj,iku) )   ! i: 1
            ELSE                        ;   pgru(ji,jj) = ssumask(ji,jj) * ( prd(ji+1,jj,iku) - zri(ji,jj    ) )   ! i: 2
            ENDIF
            IF( ze3wv >= 0._wp ) THEN   ;   pgrv(ji,jj) = ssvmask(ji,jj) * ( zrj(ji,jj      ) - prd(ji,jj,ikv) )   ! j: 1
            ELSE                        ;   pgrv(ji,jj) = ssvmask(ji,jj) * ( prd(ji,jj+1,ikv) - zrj(ji,jj    ) )   ! j: 2
            ENDIF

         END_2D

         IF (nn_hls==1) CALL lbc_lnk( 'zpshde', pgru , 'U', -1.0_wp , pgrv , 'V', -1.0_wp )   ! Lateral boundary conditions
         !
      END IF
      !
      !     !==  (ISH)  compute grui and gruvi  ==!
      !
      DO jn = 1, kjpt      !==   Interpolation of tracers at the last ocean level   ==!            !
         DO_2D( nn_hls, nn_hls-1, nn_hls, nn_hls-1 )
            iku = miku(ji,jj); ikup1 = miku(ji,jj) + 1
            ikv = mikv(ji,jj); ikvp1 = mikv(ji,jj) + 1
            !
            ! (ISF) case partial step top and bottom in adjacent cell in vertical
            ! cannot used e3w because if 2 cell water column, we have ps at top and bottom
            ! in this case e3w(i,j,k,Kmm) - e3w(i,j+1,k,Kmm) is not the distance between Tj~ and Tj
            ! the only common depth between cells (i,j) and (i,j+1) is gdepw_0
            ze3wu  =  gdept(ji,jj,iku,Kmm) - gdept(ji+1,jj,iku,Kmm)
            ze3wv  =  gdept(ji,jj,ikv,Kmm) - gdept(ji,jj+1,ikv,Kmm)

            ! i- direction
            IF( ze3wu >= 0._wp ) THEN      ! case 1
               zmaxu = ze3wu / e3w(ji+1,jj,ikup1,Kmm)
               ! interpolated values of tracers
               zti(ji,jj,jn) = pta(ji+1,jj,iku,jn) + zmaxu * ( pta(ji+1,jj,ikup1,jn) - pta(ji+1,jj,iku,jn) )
               ! gradient of tracers
               pgtui(ji,jj,jn) = ssumask(ji,jj) * ( zti(ji,jj,jn) - pta(ji,jj,iku,jn) )
            ELSE                           ! case 2
               zmaxu = - ze3wu / e3w(ji,jj,ikup1,Kmm)
               ! interpolated values of tracers
               zti(ji,jj,jn) = pta(ji,jj,iku,jn) + zmaxu * ( pta(ji,jj,ikup1,jn) - pta(ji,jj,iku,jn) )
               ! gradient of  tracers
               pgtui(ji,jj,jn) = ssumask(ji,jj) * ( pta(ji+1,jj,iku,jn) - zti(ji,jj,jn) )
            ENDIF
            !
            ! j- direction
            IF( ze3wv >= 0._wp ) THEN      ! case 1
               zmaxv =  ze3wv / e3w(ji,jj+1,ikvp1,Kmm)
               ! interpolated values of tracers
               ztj(ji,jj,jn) = pta(ji,jj+1,ikv,jn) + zmaxv * ( pta(ji,jj+1,ikvp1,jn) - pta(ji,jj+1,ikv,jn) )
               ! gradient of tracers
               pgtvi(ji,jj,jn) = ssvmask(ji,jj) * ( ztj(ji,jj,jn) - pta(ji,jj,ikv,jn) )
            ELSE                           ! case 2
               zmaxv =  - ze3wv / e3w(ji,jj,ikvp1,Kmm)
               ! interpolated values of tracers
               ztj(ji,jj,jn) = pta(ji,jj,ikv,jn) + zmaxv * ( pta(ji,jj,ikvp1,jn) - pta(ji,jj,ikv,jn) )
               ! gradient of tracers
               pgtvi(ji,jj,jn) = ssvmask(ji,jj) * ( pta(ji,jj+1,ikv,jn) - ztj(ji,jj,jn) )
            ENDIF

         END_2D
         !
      END DO
      IF (nn_hls==1) CALL lbc_lnk( 'zpshde', pgtui(:,:,:), 'U', -1.0_wp , pgtvi(:,:,:), 'V', -1.0_wp )   ! Lateral boundary cond.

      IF( PRESENT( prd ) ) THEN    !==  horizontal derivative of density anomalies (rd)  ==!    (optional part)
         !
         pgrui(:,:)  =0.0_wp; pgrvi(:,:)  =0.0_wp;
         DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )

            iku = miku(ji,jj)
            ikv = mikv(ji,jj)
            ze3wu  =  gdept(ji,jj,iku,Kmm) - gdept(ji+1,jj,iku,Kmm)
            ze3wv  =  gdept(ji,jj,ikv,Kmm) - gdept(ji,jj+1,ikv,Kmm)
            !
            IF( ze3wu >= 0._wp ) THEN   ;   zhi(ji,jj) = gdept(ji  ,jj,iku,Kmm)    ! i-direction: case 1
            ELSE                        ;   zhi(ji,jj) = gdept(ji+1,jj,iku,Kmm)    ! -     -      case 2
            ENDIF

            IF( ze3wv >= 0._wp ) THEN   ;   zhj(ji,jj) = gdept(ji,jj  ,ikv,Kmm)    ! j-direction: case 1
            ELSE                        ;   zhj(ji,jj) = gdept(ji,jj+1,ikv,Kmm)    ! -     -      case 2
            ENDIF

         END_2D
         !
         CALL eos( zti, zhi, zri )        ! interpolated density from zti, ztj
         CALL eos( ztj, zhj, zrj )        ! at the partial step depth output in  zri, zrj
         !
         DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
            iku = miku(ji,jj)
            ikv = mikv(ji,jj)
            ze3wu  =  gdept(ji,jj,iku,Kmm) - gdept(ji+1,jj,iku,Kmm)
            ze3wv  =  gdept(ji,jj,ikv,Kmm) - gdept(ji,jj+1,ikv,Kmm)

            IF( ze3wu >= 0._wp ) THEN ; pgrui(ji,jj) = ssumask(ji,jj) * ( zri(ji  ,jj      ) - prd(ji,jj,iku) ) ! i: 1
            ELSE                      ; pgrui(ji,jj) = ssumask(ji,jj) * ( prd(ji+1,jj  ,iku) - zri(ji,jj    ) ) ! i: 2
            ENDIF
            IF( ze3wv >= 0._wp ) THEN ; pgrvi(ji,jj) = ssvmask(ji,jj) * ( zrj(ji  ,jj      ) - prd(ji,jj,ikv) ) ! j: 1
            ELSE                      ; pgrvi(ji,jj) = ssvmask(ji,jj) * ( prd(ji  ,jj+1,ikv) - zrj(ji,jj    ) ) ! j: 2
            ENDIF

         END_2D
         IF (nn_hls==1) CALL lbc_lnk( 'zpshde', pgrui, 'U', -1.0_wp , pgrvi, 'V', -1.0_wp )   ! Lateral boundary conditions
         !
      END IF
      !
      IF( ln_timing )   CALL timing_stop( 'zps_hde_isf')
      !
   END SUBROUTINE zps_hde_isf_t

   !!======================================================================
END MODULE zpshde
