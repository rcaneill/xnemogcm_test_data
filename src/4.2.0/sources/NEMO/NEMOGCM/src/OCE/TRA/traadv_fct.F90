MODULE traadv_fct
   !!==============================================================================
   !!                       ***  MODULE  traadv_fct  ***
   !! Ocean  tracers:  horizontal & vertical advective trend (2nd/4th order Flux Corrected Transport method)
   !!==============================================================================
   !! History :  3.7  !  2015-09  (L. Debreu, G. Madec)  original code (inspired from traadv_tvd.F90)
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!  tra_adv_fct    : update the tracer trend with a 3D advective trends using a 2nd or 4th order FCT scheme
   !!                   with sub-time-stepping in the vertical direction
   !!  nonosc         : compute monotonic tracer fluxes by a non-oscillatory algorithm
   !!  interp_4th_cpt : 4th order compact scheme for the vertical component of the advection
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and active tracers
   USE dom_oce        ! ocean space and time domain
   USE trc_oce        ! share passive tracers/Ocean variables
   USE trd_oce        ! trends: ocean variables
   USE trdtra         ! tracers trends
   USE diaptr         ! poleward transport diagnostics
   USE diaar5         ! AR5 diagnostics
   USE phycst  , ONLY : rho0_rcp
   USE zdf_oce , ONLY : ln_zad_Aimp
   !
   USE in_out_manager ! I/O manager
   USE iom            !
   USE lib_mpp        ! MPP library
   USE lbclnk         ! ocean lateral boundary condition (or mpp link)
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_adv_fct        ! called by traadv.F90
   PUBLIC   interp_4th_cpt     ! called by traadv_cen.F90

   LOGICAL  ::   l_trd   ! flag to compute trends
   LOGICAL  ::   l_ptr   ! flag to compute poleward transport
   LOGICAL  ::   l_hst   ! flag to compute heat/salt transport
   REAL(wp) ::   r1_6 = 1._wp / 6._wp   ! =1/6

   !                                        ! tridiag solver associated indices:
   INTEGER, PARAMETER ::   np_NH   = 0   ! Neumann homogeneous boundary condition
   INTEGER, PARAMETER ::   np_CEN2 = 1   ! 2nd order centered  boundary condition

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: traadv_fct.F90 14857 2021-05-12 16:47:25Z hadcv $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tra_adv_fct( kt, kit000, cdtype, p2dt, pU, pV, pW,       &
      &                    Kbb, Kmm, pt, kjpt, Krhs, kn_fct_h, kn_fct_v )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_adv_fct  ***
      !!
      !! **  Purpose :   Compute the now trend due to total advection of tracers
      !!               and add it to the general trend of tracer equations
      !!
      !! **  Method  : - 2nd or 4th FCT scheme on the horizontal direction
      !!               (choice through the value of kn_fct)
      !!               - on the vertical the 4th order is a compact scheme
      !!               - corrected flux (monotonic correction)
      !!
      !! ** Action : - update pt(:,:,:,:,Krhs)  with the now advective tracer trends
      !!             - send trends to trdtra module for further diagnostics (l_trdtra=T)
      !!             - poleward advective heat and salt transport (ln_diaptr=T)
      !!----------------------------------------------------------------------
      INTEGER                                  , INTENT(in   ) ::   kt              ! ocean time-step index
      INTEGER                                  , INTENT(in   ) ::   Kbb, Kmm, Krhs  ! ocean time level indices
      INTEGER                                  , INTENT(in   ) ::   kit000          ! first time step index
      CHARACTER(len=3)                         , INTENT(in   ) ::   cdtype          ! =TRA or TRC (tracer indicator)
      INTEGER                                  , INTENT(in   ) ::   kjpt            ! number of tracers
      INTEGER                                  , INTENT(in   ) ::   kn_fct_h        ! order of the FCT scheme (=2 or 4)
      INTEGER                                  , INTENT(in   ) ::   kn_fct_v        ! order of the FCT scheme (=2 or 4)
      REAL(wp)                                 , INTENT(in   ) ::   p2dt            ! tracer time-step
      ! TEMP: [tiling] This can be A2D(nn_hls) after all lbc_lnks removed in the nn_hls = 2 case
      REAL(wp), DIMENSION(jpi,jpj,jpk         ), INTENT(in   ) ::   pU, pV, pW      ! 3 ocean volume flux components
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt,jpt), INTENT(inout) ::   pt              ! tracers and RHS of tracer equation
      !
      INTEGER  ::   ji, jj, jk, jn                           ! dummy loop indices
      REAL(wp) ::   ztra                                     ! local scalar
      REAL(wp) ::   zfp_ui, zfp_vj, zfp_wk, zC2t_u, zC4t_u   !   -      -
      REAL(wp) ::   zfm_ui, zfm_vj, zfm_wk, zC2t_v, zC4t_v   !   -      -
      REAL(wp), DIMENSION(A2D(nn_hls),jpk)        ::   zwi, zwx, zwy, zwz, ztu, ztv, zltu, zltv, ztw
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   ztrdx, ztrdy, ztrdz, zptry
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   zwinf, zwdia, zwsup
      LOGICAL  ::   ll_zAimp                                 ! flag to apply adaptive implicit vertical advection
      !!----------------------------------------------------------------------
      !
#if defined key_loop_fusion
      CALL tra_adv_fct_lf ( kt, nit000, cdtype, p2dt, pU, pV, pW, Kbb, Kmm, pt, kjpt, Krhs, kn_fct_h, kn_fct_v )
#else
      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                       ! Do only on the first tile
         IF( kt == kit000 )  THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'tra_adv_fct : FCT advection scheme on ', cdtype
            IF(lwp) WRITE(numout,*) '~~~~~~~~~~~'
         ENDIF
         !
         l_trd = .FALSE.            ! set local switches
         l_hst = .FALSE.
         l_ptr = .FALSE.
         ll_zAimp = .FALSE.
         IF( ( cdtype == 'TRA' .AND. l_trdtra  ) .OR. ( cdtype =='TRC' .AND. l_trdtrc ) )      l_trd = .TRUE.
         IF(   cdtype == 'TRA' .AND. ( iom_use( 'sophtadv' ) .OR. iom_use( 'sophtadv' ) ) )    l_ptr = .TRUE.
         IF(   cdtype == 'TRA' .AND. ( iom_use("uadv_heattr") .OR. iom_use("vadv_heattr") .OR.  &
            &                         iom_use("uadv_salttr") .OR. iom_use("vadv_salttr")  ) )  l_hst = .TRUE.
         !
      ENDIF

      !! -- init to 0
      zwi(:,:,:) = 0._wp
      zwx(:,:,:) = 0._wp
      zwy(:,:,:) = 0._wp
      zwz(:,:,:) = 0._wp
      ztu(:,:,:) = 0._wp
      ztv(:,:,:) = 0._wp
      zltu(:,:,:) = 0._wp
      zltv(:,:,:) = 0._wp
      ztw(:,:,:) = 0._wp
      !
      IF( l_trd .OR. l_hst )  THEN
         ALLOCATE( ztrdx(A2D(nn_hls),jpk), ztrdy(A2D(nn_hls),jpk), ztrdz(A2D(nn_hls),jpk) )
         ztrdx(:,:,:) = 0._wp   ;    ztrdy(:,:,:) = 0._wp   ;   ztrdz(:,:,:) = 0._wp
      ENDIF
      !
      IF( l_ptr ) THEN
         ALLOCATE( zptry(A2D(nn_hls),jpk) )
         zptry(:,:,:) = 0._wp
      ENDIF
      !
      ! If adaptive vertical advection, check if it is needed on this PE at this time
      IF( ln_zad_Aimp ) THEN
         IF( MAXVAL( ABS( wi(A2D(1),:) ) ) > 0._wp ) ll_zAimp = .TRUE.
      END IF
      ! If active adaptive vertical advection, build tridiagonal matrix
      IF( ll_zAimp ) THEN
         ALLOCATE(zwdia(A2D(nn_hls),jpk), zwinf(A2D(nn_hls),jpk), zwsup(A2D(nn_hls),jpk))
         DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 1, jpkm1 )
            zwdia(ji,jj,jk) =  1._wp + p2dt * ( MAX( wi(ji,jj,jk) , 0._wp ) - MIN( wi(ji,jj,jk+1) , 0._wp ) )   &
            &                               / e3t(ji,jj,jk,Krhs)
            zwinf(ji,jj,jk) =  p2dt * MIN( wi(ji,jj,jk  ) , 0._wp ) / e3t(ji,jj,jk,Krhs)
            zwsup(ji,jj,jk) = -p2dt * MAX( wi(ji,jj,jk+1) , 0._wp ) / e3t(ji,jj,jk,Krhs)
         END_3D
      END IF
      !
      DO jn = 1, kjpt            !==  loop over the tracers  ==!
         !
         !        !==  upstream advection with initial mass fluxes & intermediate update  ==!
         !                    !* upstream tracer flux in the i and j direction
         DO_3D( nn_hls, nn_hls-1, nn_hls, nn_hls-1, 1, jpkm1 )
            ! upstream scheme
            zfp_ui = pU(ji,jj,jk) + ABS( pU(ji,jj,jk) )
            zfm_ui = pU(ji,jj,jk) - ABS( pU(ji,jj,jk) )
            zfp_vj = pV(ji,jj,jk) + ABS( pV(ji,jj,jk) )
            zfm_vj = pV(ji,jj,jk) - ABS( pV(ji,jj,jk) )
            zwx(ji,jj,jk) = 0.5 * ( zfp_ui * pt(ji,jj,jk,jn,Kbb) + zfm_ui * pt(ji+1,jj  ,jk,jn,Kbb) )
            zwy(ji,jj,jk) = 0.5 * ( zfp_vj * pt(ji,jj,jk,jn,Kbb) + zfm_vj * pt(ji  ,jj+1,jk,jn,Kbb) )
         END_3D
         !                               !* upstream tracer flux in the k direction *!
         DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 2, jpkm1 )      ! Interior value ( multiplied by wmask)
            zfp_wk = pW(ji,jj,jk) + ABS( pW(ji,jj,jk) )
            zfm_wk = pW(ji,jj,jk) - ABS( pW(ji,jj,jk) )
            zwz(ji,jj,jk) = 0.5 * ( zfp_wk * pt(ji,jj,jk,jn,Kbb) + zfm_wk * pt(ji,jj,jk-1,jn,Kbb) ) * wmask(ji,jj,jk)
         END_3D
         IF( ln_linssh ) THEN               ! top ocean value (only in linear free surface as zwz has been w-masked)
            IF( ln_isfcav ) THEN                        ! top of the ice-shelf cavities and at the ocean surface
               DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
                  zwz(ji,jj, mikt(ji,jj) ) = pW(ji,jj,mikt(ji,jj)) * pt(ji,jj,mikt(ji,jj),jn,Kbb)   ! linear free surface
               END_2D
            ELSE                                        ! no cavities: only at the ocean surface
               DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
                  zwz(ji,jj,1) = pW(ji,jj,1) * pt(ji,jj,1,jn,Kbb)
               END_2D
            ENDIF
         ENDIF
         !
         DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 1, jpkm1 )   !* trend and after field with monotonic scheme
            !                               ! total intermediate advective trends
            ztra = - (  zwx(ji,jj,jk) - zwx(ji-1,jj  ,jk  )   &
               &      + zwy(ji,jj,jk) - zwy(ji  ,jj-1,jk  )   &
               &      + zwz(ji,jj,jk) - zwz(ji  ,jj  ,jk+1) ) * r1_e1e2t(ji,jj)
            !                               ! update and guess with monotonic sheme
            pt(ji,jj,jk,jn,Krhs) =                   pt(ji,jj,jk,jn,Krhs) +       ztra   &
               &                                  / e3t(ji,jj,jk,Kmm ) * tmask(ji,jj,jk)
            zwi(ji,jj,jk)    = ( e3t(ji,jj,jk,Kbb) * pt(ji,jj,jk,jn,Kbb) + p2dt * ztra ) &
               &                                  / e3t(ji,jj,jk,Krhs) * tmask(ji,jj,jk)
         END_3D

         IF ( ll_zAimp ) THEN
            CALL tridia_solver( zwdia, zwsup, zwinf, zwi, zwi , 0 )
            !
            ztw(:,:,1) = 0._wp ; ztw(:,:,jpk) = 0._wp ;
            DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 2, jpkm1 )       ! Interior value ( multiplied by wmask)
               zfp_wk = wi(ji,jj,jk) + ABS( wi(ji,jj,jk) )
               zfm_wk = wi(ji,jj,jk) - ABS( wi(ji,jj,jk) )
               ztw(ji,jj,jk) =  0.5 * e1e2t(ji,jj) * ( zfp_wk * zwi(ji,jj,jk) + zfm_wk * zwi(ji,jj,jk-1) ) * wmask(ji,jj,jk)
               zwz(ji,jj,jk) = zwz(ji,jj,jk) + ztw(ji,jj,jk) ! update vertical fluxes
            END_3D
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               pt(ji,jj,jk,jn,Krhs) = pt(ji,jj,jk,jn,Krhs) - ( ztw(ji,jj,jk) - ztw(ji  ,jj  ,jk+1) ) &
                  &                                        * r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kmm)
            END_3D
            !
         END IF
         !
         IF( l_trd .OR. l_hst )  THEN             ! trend diagnostics (contribution of upstream fluxes)
            ztrdx(:,:,:) = zwx(:,:,:)   ;   ztrdy(:,:,:) = zwy(:,:,:)   ;   ztrdz(:,:,:) = zwz(:,:,:)
         END IF
         !                             ! "Poleward" heat and salt transports (contribution of upstream fluxes)
         IF( l_ptr )   zptry(:,:,:) = zwy(:,:,:)
         !
         !        !==  anti-diffusive flux : high order minus low order  ==!
         !
         SELECT CASE( kn_fct_h )    !* horizontal anti-diffusive fluxes
         !
         CASE(  2  )                   !- 2nd order centered
            DO_3D( nn_hls, nn_hls-1, nn_hls, nn_hls-1, 1, jpkm1 )
               zwx(ji,jj,jk) = 0.5_wp * pU(ji,jj,jk) * ( pt(ji,jj,jk,jn,Kmm) + pt(ji+1,jj,jk,jn,Kmm) ) - zwx(ji,jj,jk)
               zwy(ji,jj,jk) = 0.5_wp * pV(ji,jj,jk) * ( pt(ji,jj,jk,jn,Kmm) + pt(ji,jj+1,jk,jn,Kmm) ) - zwy(ji,jj,jk)
            END_3D
            !
         CASE(  4  )                   !- 4th order centered
            zltu(:,:,jpk) = 0._wp            ! Bottom value : flux set to zero
            zltv(:,:,jpk) = 0._wp
            DO jk = 1, jpkm1                 ! Laplacian
               DO_2D( 1, 0, 1, 0 )                 ! 1st derivative (gradient)
                  ztu(ji,jj,jk) = ( pt(ji+1,jj  ,jk,jn,Kmm) - pt(ji,jj,jk,jn,Kmm) ) * umask(ji,jj,jk)
                  ztv(ji,jj,jk) = ( pt(ji  ,jj+1,jk,jn,Kmm) - pt(ji,jj,jk,jn,Kmm) ) * vmask(ji,jj,jk)
               END_2D
               DO_2D( 0, 0, 0, 0 )                 ! 2nd derivative * 1/ 6
                  zltu(ji,jj,jk) = (  ztu(ji,jj,jk) + ztu(ji-1,jj,jk)  ) * r1_6
                  zltv(ji,jj,jk) = (  ztv(ji,jj,jk) + ztv(ji,jj-1,jk)  ) * r1_6
               END_2D
            END DO
            ! NOTE [ comm_cleanup ] : need to change sign to ensure halo 1 - halo 2 compatibility
            CALL lbc_lnk( 'traadv_fct', zltu, 'T', -1.0_wp , zltv, 'T', -1.0_wp, ld4only= .TRUE. )   ! Lateral boundary cond. (unchanged sgn)
            !
            DO_3D( nn_hls, nn_hls-1, nn_hls, nn_hls-1, 1, jpkm1 )
               zC2t_u = pt(ji,jj,jk,jn,Kmm) + pt(ji+1,jj  ,jk,jn,Kmm)   ! 2 x C2 interpolation of T at u- & v-points
               zC2t_v = pt(ji,jj,jk,jn,Kmm) + pt(ji  ,jj+1,jk,jn,Kmm)
               !                                                        ! C4 minus upstream advective fluxes
               ! round brackets added to fix the order of floating point operations
               ! needed to ensure halo 1 - halo 2 compatibility
               zwx(ji,jj,jk) =  0.5_wp * pU(ji,jj,jk) * ( zC2t_u + ( zltu(ji,jj,jk) - zltu(ji+1,jj,jk)   &
                             &                                     )                                     & ! bracket for halo 1 - halo 2 compatibility
                             &                          ) - zwx(ji,jj,jk)
               zwy(ji,jj,jk) =  0.5_wp * pV(ji,jj,jk) * ( zC2t_v + ( zltv(ji,jj,jk) - zltv(ji,jj+1,jk)   &
                             &                                     )                                     & ! bracket for halo 1 - halo 2 compatibility
                             &                          ) - zwy(ji,jj,jk)
            END_3D
            !
         CASE(  41 )                   !- 4th order centered       ==>>   !!gm coding attempt   need to be tested
            ztu(:,:,jpk) = 0._wp             ! Bottom value : flux set to zero
            ztv(:,:,jpk) = 0._wp
            DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 1, jpkm1 )    ! 1st derivative (gradient)
               ztu(ji,jj,jk) = ( pt(ji+1,jj  ,jk,jn,Kmm) - pt(ji,jj,jk,jn,Kmm) ) * umask(ji,jj,jk)
               ztv(ji,jj,jk) = ( pt(ji  ,jj+1,jk,jn,Kmm) - pt(ji,jj,jk,jn,Kmm) ) * vmask(ji,jj,jk)
            END_3D
            IF (nn_hls==1) CALL lbc_lnk( 'traadv_fct', ztu, 'U', -1.0_wp , ztv, 'V', -1.0_wp, ld4only= .TRUE. )   ! Lateral boundary cond. (unchanged sgn)
            !
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )    ! Horizontal advective fluxes
               zC2t_u = pt(ji,jj,jk,jn,Kmm) + pt(ji+1,jj  ,jk,jn,Kmm)   ! 2 x C2 interpolation of T at u- & v-points (x2)
               zC2t_v = pt(ji,jj,jk,jn,Kmm) + pt(ji  ,jj+1,jk,jn,Kmm)
               !                                                  ! C4 interpolation of T at u- & v-points (x2)
               zC4t_u =  zC2t_u + r1_6 * ( ztu(ji-1,jj  ,jk) - ztu(ji+1,jj  ,jk) )
               zC4t_v =  zC2t_v + r1_6 * ( ztv(ji  ,jj-1,jk) - ztv(ji  ,jj+1,jk) )
               !                                                  ! C4 minus upstream advective fluxes
               zwx(ji,jj,jk) =  0.5_wp * pU(ji,jj,jk) * zC4t_u - zwx(ji,jj,jk)
               zwy(ji,jj,jk) =  0.5_wp * pV(ji,jj,jk) * zC4t_v - zwy(ji,jj,jk)
            END_3D
            IF (nn_hls==2) CALL lbc_lnk( 'traadv_fct', zwx, 'U', -1.0_wp , zwy, 'V', -1.0_wp )   ! Lateral boundary cond. (unchanged sgn)
            !
         END SELECT
         !
         SELECT CASE( kn_fct_v )    !* vertical anti-diffusive fluxes (w-masked interior values)
         !
         CASE(  2  )                   !- 2nd order centered
            DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 2, jpkm1 )
               zwz(ji,jj,jk) =  (  pW(ji,jj,jk) * 0.5_wp * ( pt(ji,jj,jk,jn,Kmm) + pt(ji,jj,jk-1,jn,Kmm) )   &
                  &              - zwz(ji,jj,jk)  ) * wmask(ji,jj,jk)
            END_3D
            !
         CASE(  4  )                   !- 4th order COMPACT
            CALL interp_4th_cpt( pt(:,:,:,jn,Kmm) , ztw )   ! zwt = COMPACT interpolation of T at w-point
            DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 2, jpkm1 )
               zwz(ji,jj,jk) = ( pW(ji,jj,jk) * ztw(ji,jj,jk) - zwz(ji,jj,jk) ) * wmask(ji,jj,jk)
            END_3D
            !
         END SELECT
         IF( ln_linssh ) THEN    ! top ocean value: high order = upstream  ==>>  zwz=0
            zwz(:,:,1) = 0._wp   ! only ocean surface as interior zwz values have been w-masked
         ENDIF
         !
         IF (nn_hls==1) THEN
            CALL lbc_lnk( 'traadv_fct', zwi, 'T', 1.0_wp, zwx, 'U', -1.0_wp , zwy, 'V', -1.0_wp, zwz, 'T', 1.0_wp )
         ELSE
            CALL lbc_lnk( 'traadv_fct', zwi, 'T', 1.0_wp)
         END IF
         !
         IF ( ll_zAimp ) THEN
            DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 1, jpkm1 )    !* trend and after field with monotonic scheme
               !                                                ! total intermediate advective trends
               ztra = - (  zwx(ji,jj,jk) - zwx(ji-1,jj  ,jk  )   &
                  &      + zwy(ji,jj,jk) - zwy(ji  ,jj-1,jk  )   &
                  &      + zwz(ji,jj,jk) - zwz(ji  ,jj  ,jk+1) ) * r1_e1e2t(ji,jj)
               ztw(ji,jj,jk) = zwi(ji,jj,jk) + p2dt * ztra / e3t(ji,jj,jk,Krhs) * tmask(ji,jj,jk)
            END_3D
            !
            CALL tridia_solver( zwdia, zwsup, zwinf, ztw, ztw , 0 )
            !
            DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 2, jpkm1 )       ! Interior value ( multiplied by wmask)
               zfp_wk = wi(ji,jj,jk) + ABS( wi(ji,jj,jk) )
               zfm_wk = wi(ji,jj,jk) - ABS( wi(ji,jj,jk) )
               zwz(ji,jj,jk) = zwz(ji,jj,jk) + 0.5 * e1e2t(ji,jj) * ( zfp_wk * ztw(ji,jj,jk) + zfm_wk * ztw(ji,jj,jk-1) ) * wmask(ji,jj,jk)
            END_3D
         END IF
         !
         !        !==  monotonicity algorithm  ==!
         !
         CALL nonosc( Kmm, pt(:,:,:,jn,Kbb), zwx, zwy, zwz, zwi, p2dt )
         !
         !        !==  final trend with corrected fluxes  ==!
         !
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            ztra = - (  zwx(ji,jj,jk) - zwx(ji-1,jj  ,jk  )   &
               &      + zwy(ji,jj,jk) - zwy(ji  ,jj-1,jk  )   &
               &      + zwz(ji,jj,jk) - zwz(ji  ,jj  ,jk+1) ) * r1_e1e2t(ji,jj)
            pt(ji,jj,jk,jn,Krhs) = pt(ji,jj,jk,jn,Krhs) + ztra / e3t(ji,jj,jk,Kmm)
            zwi(ji,jj,jk) = zwi(ji,jj,jk) + p2dt * ztra / e3t(ji,jj,jk,Krhs) * tmask(ji,jj,jk)
         END_3D
         !
         IF ( ll_zAimp ) THEN
            !
            ztw(:,:,1) = 0._wp ; ztw(:,:,jpk) = 0._wp
            DO_3D( 0, 0, 0, 0, 2, jpkm1 )      ! Interior value ( multiplied by wmask)
               zfp_wk = wi(ji,jj,jk) + ABS( wi(ji,jj,jk) )
               zfm_wk = wi(ji,jj,jk) - ABS( wi(ji,jj,jk) )
               ztw(ji,jj,jk) = - 0.5 * e1e2t(ji,jj) * ( zfp_wk * zwi(ji,jj,jk) + zfm_wk * zwi(ji,jj,jk-1) ) * wmask(ji,jj,jk)
               zwz(ji,jj,jk) = zwz(ji,jj,jk) + ztw(ji,jj,jk) ! Update vertical fluxes for trend diagnostic
            END_3D
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               pt(ji,jj,jk,jn,Krhs) = pt(ji,jj,jk,jn,Krhs) - ( ztw(ji,jj,jk) - ztw(ji  ,jj  ,jk+1) ) &
                  &                                        * r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kmm)
            END_3D
         END IF
         IF( l_trd .OR. l_hst ) THEN   ! trend diagnostics // heat/salt transport
            ztrdx(:,:,:) = ztrdx(:,:,:) + zwx(:,:,:)  ! <<< add anti-diffusive fluxes
            ztrdy(:,:,:) = ztrdy(:,:,:) + zwy(:,:,:)  !     to upstream fluxes
            ztrdz(:,:,:) = ztrdz(:,:,:) + zwz(:,:,:)  !
            !
            IF( l_trd ) THEN              ! trend diagnostics
               CALL trd_tra( kt, Kmm, Krhs, cdtype, jn, jptra_xad, ztrdx, pU, pt(:,:,:,jn,Kmm) )
               CALL trd_tra( kt, Kmm, Krhs, cdtype, jn, jptra_yad, ztrdy, pV, pt(:,:,:,jn,Kmm) )
               CALL trd_tra( kt, Kmm, Krhs, cdtype, jn, jptra_zad, ztrdz, pW, pt(:,:,:,jn,Kmm) )
            ENDIF
            !                             ! heat/salt transport
            IF( l_hst )   CALL dia_ar5_hst( jn, 'adv', ztrdx(:,:,:), ztrdy(:,:,:) )
            !
         ENDIF
         IF( l_ptr ) THEN              ! "Poleward" transports
            zptry(:,:,:) = zptry(:,:,:) + zwy(:,:,:)  ! <<< add anti-diffusive fluxes
            CALL dia_ptr_hst( jn, 'adv', zptry(:,:,:) )
         ENDIF
         !
      END DO                     ! end of tracer loop
      !
      IF ( ll_zAimp ) THEN
         DEALLOCATE( zwdia, zwinf, zwsup )
      ENDIF
      IF( l_trd .OR. l_hst ) THEN
         DEALLOCATE( ztrdx, ztrdy, ztrdz )
      ENDIF
      IF( l_ptr ) THEN
         DEALLOCATE( zptry )
      ENDIF
      !
#endif
   END SUBROUTINE tra_adv_fct


   SUBROUTINE nonosc( Kmm, pbef, paa, pbb, pcc, paft, p2dt )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE nonosc  ***
      !!
      !! **  Purpose :   compute monotonic tracer fluxes from the upstream
      !!       scheme and the before field by a nonoscillatory algorithm
      !!
      !! **  Method  :   ... ???
      !!       warning : pbef and paft must be masked, but the boundaries
      !!       conditions on the fluxes are not necessary zalezak (1979)
      !!       drange (1995) multi-dimensional forward-in-time and upstream-
      !!       in-space based differencing for fluid
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   Kmm             ! time level index
      REAL(wp)                        , INTENT(in   ) ::   p2dt            ! tracer time-step
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(in   ) ::   pbef            ! before field
      REAL(wp), DIMENSION(A2D(nn_hls)    ,jpk), INTENT(in   ) ::   paft            ! after field
      REAL(wp), DIMENSION(A2D(nn_hls)    ,jpk), INTENT(inout) ::   paa, pbb, pcc   ! monotonic fluxes in the 3 directions
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      INTEGER  ::   ikm1         ! local integer
      REAL(dp) ::   zpos, zneg, zbt, za, zb, zc, zbig, zrtrn    ! local scalars
      REAL(dp) ::   zau, zbu, zcu, zav, zbv, zcv, zup, zdo            !   -      -
      REAL(dp), DIMENSION(A2D(nn_hls),jpk) :: zbetup, zbetdo, zbup, zbdo
      !!----------------------------------------------------------------------
      !
      zbig  = 1.e+40_dp
      zrtrn = 1.e-15_dp
      zbetup(:,:,:) = 0._dp   ;   zbetdo(:,:,:) = 0._dp

      ! Search local extrema
      ! --------------------
      ! max/min of pbef & paft with large negative/positive value (-/+zbig) inside land
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpk )
         zbup(ji,jj,jk) = MAX( pbef(ji,jj,jk) * tmask(ji,jj,jk) - zbig * ( 1._wp - tmask(ji,jj,jk) ),   &
            &                  paft(ji,jj,jk) * tmask(ji,jj,jk) - zbig * ( 1._wp - tmask(ji,jj,jk) )  )
         zbdo(ji,jj,jk) = MIN( pbef(ji,jj,jk) * tmask(ji,jj,jk) + zbig * ( 1._wp - tmask(ji,jj,jk) ),   &
            &                  paft(ji,jj,jk) * tmask(ji,jj,jk) + zbig * ( 1._wp - tmask(ji,jj,jk) )  )
      END_3D

      DO jk = 1, jpkm1
         ikm1 = MAX(jk-1,1)
         DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )

            ! search maximum in neighbourhood
            zup = MAX(  zbup(ji  ,jj  ,jk  ),   &
               &        zbup(ji-1,jj  ,jk  ), zbup(ji+1,jj  ,jk  ),   &
               &        zbup(ji  ,jj-1,jk  ), zbup(ji  ,jj+1,jk  ),   &
               &        zbup(ji  ,jj  ,ikm1), zbup(ji  ,jj  ,jk+1)  )

            ! search minimum in neighbourhood
            zdo = MIN(  zbdo(ji  ,jj  ,jk  ),   &
               &        zbdo(ji-1,jj  ,jk  ), zbdo(ji+1,jj  ,jk  ),   &
               &        zbdo(ji  ,jj-1,jk  ), zbdo(ji  ,jj+1,jk  ),   &
               &        zbdo(ji  ,jj  ,ikm1), zbdo(ji  ,jj  ,jk+1)  )

            ! positive part of the flux
            zpos = MAX( 0., paa(ji-1,jj  ,jk  ) ) - MIN( 0., paa(ji  ,jj  ,jk  ) )   &
               & + MAX( 0., pbb(ji  ,jj-1,jk  ) ) - MIN( 0., pbb(ji  ,jj  ,jk  ) )   &
               & + MAX( 0., pcc(ji  ,jj  ,jk+1) ) - MIN( 0., pcc(ji  ,jj  ,jk  ) )

            ! negative part of the flux
            zneg = MAX( 0., paa(ji  ,jj  ,jk  ) ) - MIN( 0., paa(ji-1,jj  ,jk  ) )   &
               & + MAX( 0., pbb(ji  ,jj  ,jk  ) ) - MIN( 0., pbb(ji  ,jj-1,jk  ) )   &
               & + MAX( 0., pcc(ji  ,jj  ,jk  ) ) - MIN( 0., pcc(ji  ,jj  ,jk+1) )

            ! up & down beta terms
            zbt = e1e2t(ji,jj) * e3t(ji,jj,jk,Kmm) / p2dt
            zbetup(ji,jj,jk) = ( zup            - paft(ji,jj,jk) ) / ( zpos + zrtrn ) * zbt
            zbetdo(ji,jj,jk) = ( paft(ji,jj,jk) - zdo            ) / ( zneg + zrtrn ) * zbt
         END_2D
      END DO
      IF (nn_hls==1) CALL lbc_lnk( 'traadv_fct', zbetup, 'T', 1.0_wp , zbetdo, 'T', 1.0_wp, ld4only= .TRUE. )   ! lateral boundary cond. (unchanged sign)

      ! 3. monotonic flux in the i & j direction (paa & pbb)
      ! ----------------------------------------
      DO_3D( 1, 0, 1, 0, 1, jpkm1 )
         zau = MIN( 1._wp, zbetdo(ji,jj,jk), zbetup(ji+1,jj,jk) )
         zbu = MIN( 1._wp, zbetup(ji,jj,jk), zbetdo(ji+1,jj,jk) )
         zcu =       ( 0.5  + SIGN( 0.5_wp , paa(ji,jj,jk) ) )
         paa(ji,jj,jk) = paa(ji,jj,jk) * ( zcu * zau + ( 1._wp - zcu) * zbu )

         zav = MIN( 1._wp, zbetdo(ji,jj,jk), zbetup(ji,jj+1,jk) )
         zbv = MIN( 1._wp, zbetup(ji,jj,jk), zbetdo(ji,jj+1,jk) )
         zcv =       ( 0.5  + SIGN( 0.5_wp , pbb(ji,jj,jk) ) )
         pbb(ji,jj,jk) = pbb(ji,jj,jk) * ( zcv * zav + ( 1._wp - zcv) * zbv )

      ! monotonic flux in the k direction, i.e. pcc
      ! -------------------------------------------
         za = MIN( 1., zbetdo(ji,jj,jk+1), zbetup(ji,jj,jk) )
         zb = MIN( 1., zbetup(ji,jj,jk+1), zbetdo(ji,jj,jk) )
         zc =       ( 0.5  + SIGN( 0.5_wp , pcc(ji,jj,jk+1) ) )
         pcc(ji,jj,jk+1) = pcc(ji,jj,jk+1) * ( zc * za + ( 1._wp - zc) * zb )
      END_3D
      !
   END SUBROUTINE nonosc


   SUBROUTINE interp_4th_cpt_org( pt_in, pt_out )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interp_4th_cpt_org  ***
      !!
      !! **  Purpose :   Compute the interpolation of tracer at w-point
      !!
      !! **  Method  :   4th order compact interpolation
      !!----------------------------------------------------------------------
      REAL(wp),DIMENSION(jpi,jpj,jpk), INTENT(in   ) ::   pt_in    ! now tracer fields
      REAL(wp),DIMENSION(jpi,jpj,jpk), INTENT(  out) ::   pt_out   ! now tracer field interpolated at w-pts
      !
      INTEGER :: ji, jj, jk   ! dummy loop integers
      REAL(wp),DIMENSION(jpi,jpj,jpk) :: zwd, zwi, zws, zwrm, zwt
      !!----------------------------------------------------------------------

      DO_3D( 1, 1, 1, 1, 3, jpkm1 )       !==  build the three diagonal matrix  ==!
         zwd (ji,jj,jk) = 4._wp
         zwi (ji,jj,jk) = 1._wp
         zws (ji,jj,jk) = 1._wp
         zwrm(ji,jj,jk) = 3._wp * ( pt_in(ji,jj,jk-1) + pt_in(ji,jj,jk) )
         !
         IF( tmask(ji,jj,jk+1) == 0._wp) THEN   ! Switch to second order centered at bottom
            zwd (ji,jj,jk) = 1._wp
            zwi (ji,jj,jk) = 0._wp
            zws (ji,jj,jk) = 0._wp
            zwrm(ji,jj,jk) = 0.5 * ( pt_in(ji,jj,jk-1) + pt_in(ji,jj,jk) )
         ENDIF
      END_3D
      !
      jk = 2                                    ! Switch to second order centered at top
      DO_2D( 1, 1, 1, 1 )
         zwd (ji,jj,jk) = 1._wp
         zwi (ji,jj,jk) = 0._wp
         zws (ji,jj,jk) = 0._wp
         zwrm(ji,jj,jk) = 0.5 * ( pt_in(ji,jj,jk-1) + pt_in(ji,jj,jk) )
      END_2D
      !
      !                       !==  tridiagonal solve  ==!
      DO_2D( 1, 1, 1, 1 )           ! first recurrence
         zwt(ji,jj,2) = zwd(ji,jj,2)
      END_2D
      DO_3D( 1, 1, 1, 1, 3, jpkm1 )
         zwt(ji,jj,jk) = zwd(ji,jj,jk) - zwi(ji,jj,jk) * zws(ji,jj,jk-1) /zwt(ji,jj,jk-1)
      END_3D
      !
      DO_2D( 1, 1, 1, 1 )           ! second recurrence:    Zk = Yk - Ik / Tk-1  Zk-1
         pt_out(ji,jj,2) = zwrm(ji,jj,2)
      END_2D
      DO_3D( 1, 1, 1, 1, 3, jpkm1 )
         pt_out(ji,jj,jk) = zwrm(ji,jj,jk) - zwi(ji,jj,jk) / zwt(ji,jj,jk-1) *pt_out(ji,jj,jk-1)
      END_3D

      DO_2D( 1, 1, 1, 1 )           ! third recurrence: Xk = (Zk - Sk Xk+1 ) / Tk
         pt_out(ji,jj,jpkm1) = pt_out(ji,jj,jpkm1) / zwt(ji,jj,jpkm1)
      END_2D
      DO_3DS( 1, 1, 1, 1, jpk-2, 2, -1 )
         pt_out(ji,jj,jk) = ( pt_out(ji,jj,jk) - zws(ji,jj,jk) * pt_out(ji,jj,jk+1) ) / zwt(ji,jj,jk)
      END_3D
      !
   END SUBROUTINE interp_4th_cpt_org


   SUBROUTINE interp_4th_cpt( pt_in, pt_out )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interp_4th_cpt  ***
      !!
      !! **  Purpose :   Compute the interpolation of tracer at w-point
      !!
      !! **  Method  :   4th order compact interpolation
      !!----------------------------------------------------------------------
      REAL(wp),DIMENSION(jpi,jpj,jpk), INTENT(in   ) ::   pt_in    ! field at t-point
      REAL(wp),DIMENSION(A2D(nn_hls)    ,jpk), INTENT(  out) ::   pt_out   ! field interpolated at w-point
      !
      INTEGER ::   ji, jj, jk   ! dummy loop integers
      INTEGER ::   ikt, ikb     ! local integers
      REAL(wp),DIMENSION(A2D(nn_hls),jpk) :: zwd, zwi, zws, zwrm, zwt
      !!----------------------------------------------------------------------
      !
      !                      !==  build the three diagonal matrix & the RHS  ==!
      !
      DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 3, jpkm1 )    ! interior (from jk=3 to jpk-1)
         zwd (ji,jj,jk) = 3._wp * wmask(ji,jj,jk) + 1._wp                 !       diagonal
         zwi (ji,jj,jk) =         wmask(ji,jj,jk)                         ! lower diagonal
         zws (ji,jj,jk) =         wmask(ji,jj,jk)                         ! upper diagonal
         zwrm(ji,jj,jk) = 3._wp * wmask(ji,jj,jk)                     &   ! RHS
            &           *       ( pt_in(ji,jj,jk) + pt_in(ji,jj,jk-1) )
      END_3D
      !
!!gm
!      SELECT CASE( kbc )               !* boundary condition
!      CASE( np_NH   )   ! Neumann homogeneous at top & bottom
!      CASE( np_CEN2 )   ! 2nd order centered  at top & bottom
!      END SELECT
!!gm
      !
      IF ( ln_isfcav ) THEN            ! set level two values which may not be set in ISF case
         zwd(:,:,2) = 1._wp  ;  zwi(:,:,2) = 0._wp  ;  zws(:,:,2) = 0._wp  ;  zwrm(:,:,2) = 0._wp
      END IF
      !
      DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )              ! 2nd order centered at top & bottom
         ikt = mikt(ji,jj) + 1            ! w-point below the 1st  wet point
         ikb = MAX(mbkt(ji,jj), 2)        !     -   above the last wet point
         !
         zwd (ji,jj,ikt) = 1._wp          ! top
         zwi (ji,jj,ikt) = 0._wp
         zws (ji,jj,ikt) = 0._wp
         zwrm(ji,jj,ikt) = 0.5_wp * ( pt_in(ji,jj,ikt-1) + pt_in(ji,jj,ikt) )
         !
         zwd (ji,jj,ikb) = 1._wp          ! bottom
         zwi (ji,jj,ikb) = 0._wp
         zws (ji,jj,ikb) = 0._wp
         zwrm(ji,jj,ikb) = 0.5_wp * ( pt_in(ji,jj,ikb-1) + pt_in(ji,jj,ikb) )
      END_2D
      !
      !                       !==  tridiagonal solver  ==!
      !
      DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )           !* 1st recurrence:   Tk = Dk - Ik Sk-1 / Tk-1
         zwt(ji,jj,2) = zwd(ji,jj,2)
      END_2D
      DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 3, jpkm1 )
         zwt(ji,jj,jk) = zwd(ji,jj,jk) - zwi(ji,jj,jk) * zws(ji,jj,jk-1) /zwt(ji,jj,jk-1)
      END_3D
      !
      DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )           !* 2nd recurrence:    Zk = Yk - Ik / Tk-1  Zk-1
         pt_out(ji,jj,2) = zwrm(ji,jj,2)
      END_2D
      DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 3, jpkm1 )
         pt_out(ji,jj,jk) = zwrm(ji,jj,jk) - zwi(ji,jj,jk) / zwt(ji,jj,jk-1) *pt_out(ji,jj,jk-1)
      END_3D

      DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )           !* 3d recurrence:    Xk = (Zk - Sk Xk+1 ) / Tk
         pt_out(ji,jj,jpkm1) = pt_out(ji,jj,jpkm1) / zwt(ji,jj,jpkm1)
      END_2D
      DO_3DS( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, jpk-2, 2, -1 )
         pt_out(ji,jj,jk) = ( pt_out(ji,jj,jk) - zws(ji,jj,jk) * pt_out(ji,jj,jk+1) ) / zwt(ji,jj,jk)
      END_3D
      !
   END SUBROUTINE interp_4th_cpt


   SUBROUTINE tridia_solver( pD, pU, pL, pRHS, pt_out , klev )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tridia_solver  ***
      !!
      !! **  Purpose :   solve a symmetric 3diagonal system
      !!
      !! **  Method  :   solve M.t_out = RHS(t)  where M is a tri diagonal matrix ( jpk*jpk )
      !!
      !!             ( D_1 U_1  0   0   0  )( t_1 )   ( RHS_1 )
      !!             ( L_2 D_2 U_2  0   0  )( t_2 )   ( RHS_2 )
      !!             (  0  L_3 D_3 U_3  0  )( t_3 ) = ( RHS_3 )
      !!             (        ...          )( ... )   ( ...  )
      !!             (  0   0   0  L_k D_k )( t_k )   ( RHS_k )
      !!
      !!        M is decomposed in the product of an upper and lower triangular matrix.
      !!        The tri-diagonals matrix is given as input 3D arrays:   pD, pU, pL
      !!        (i.e. the Diagonal, the Upper diagonal, and the Lower diagonal).
      !!        The solution is pta.
      !!        The 3d array zwt is used as a work space array.
      !!----------------------------------------------------------------------
      REAL(wp),DIMENSION(A2D(nn_hls),jpk), INTENT(in   ) ::   pD, pU, PL    ! 3-diagonal matrix
      REAL(wp),DIMENSION(A2D(nn_hls),jpk), INTENT(in   ) ::   pRHS          ! Right-Hand-Side
      REAL(wp),DIMENSION(A2D(nn_hls),jpk), INTENT(  out) ::   pt_out        !!gm field at level=F(klev)
      INTEGER                    , INTENT(in   ) ::   klev          ! =1 pt_out at w-level
      !                                                             ! =0 pt at t-level
      INTEGER ::   ji, jj, jk   ! dummy loop integers
      INTEGER ::   kstart       ! local indices
      REAL(wp),DIMENSION(A2D(nn_hls),jpk) ::   zwt   ! 3D work array
      !!----------------------------------------------------------------------
      !
      kstart =  1  + klev
      !
      DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )                         !* 1st recurrence:   Tk = Dk - Ik Sk-1 / Tk-1
         zwt(ji,jj,kstart) = pD(ji,jj,kstart)
      END_2D
      DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, kstart+1, jpkm1 )
         zwt(ji,jj,jk) = pD(ji,jj,jk) - pL(ji,jj,jk) * pU(ji,jj,jk-1) /zwt(ji,jj,jk-1)
      END_3D
      !
      DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )                        !* 2nd recurrence:    Zk = Yk - Ik / Tk-1  Zk-1
         pt_out(ji,jj,kstart) = pRHS(ji,jj,kstart)
      END_2D
      DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, kstart+1, jpkm1 )
         pt_out(ji,jj,jk) = pRHS(ji,jj,jk) - pL(ji,jj,jk) / zwt(ji,jj,jk-1) *pt_out(ji,jj,jk-1)
      END_3D

      DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )                       !* 3d recurrence:    Xk = (Zk - Sk Xk+1 ) / Tk
         pt_out(ji,jj,jpkm1) = pt_out(ji,jj,jpkm1) / zwt(ji,jj,jpkm1)
      END_2D
      DO_3DS( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, jpk-2, kstart, -1 )
         pt_out(ji,jj,jk) = ( pt_out(ji,jj,jk) - pU(ji,jj,jk) * pt_out(ji,jj,jk+1) ) / zwt(ji,jj,jk)
      END_3D
      !
   END SUBROUTINE tridia_solver

#if defined key_loop_fusion
#define tracer_flux_i(out,zfp,zfm,ji,jj,jk) \
        zfp = pU(ji,jj,jk) + ABS( pU(ji,jj,jk) ) ; \
        zfm = pU(ji,jj,jk) - ABS( pU(ji,jj,jk) ) ; \
        out = 0.5 * ( zfp * pt(ji,jj,jk,jn,Kbb) + zfm * pt(ji+1,jj,jk,jn,Kbb) )

#define tracer_flux_j(out,zfp,zfm,ji,jj,jk) \
        zfp = pV(ji,jj,jk) + ABS( pV(ji,jj,jk) ) ; \
        zfm = pV(ji,jj,jk) - ABS( pV(ji,jj,jk) ) ; \
        out = 0.5 * ( zfp * pt(ji,jj,jk,jn,Kbb) + zfm * pt(ji,jj+1,jk,jn,Kbb) )

   SUBROUTINE tra_adv_fct_lf( kt, kit000, cdtype, p2dt, pU, pV, pW,       &
      &                    Kbb, Kmm, pt, kjpt, Krhs, kn_fct_h, kn_fct_v )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_adv_fct  ***
      !!
      !! **  Purpose :   Compute the now trend due to total advection of tracers
      !!               and add it to the general trend of tracer equations
      !!
      !! **  Method  : - 2nd or 4th FCT scheme on the horizontal direction
      !!               (choice through the value of kn_fct)
      !!               - on the vertical the 4th order is a compact scheme
      !!               - corrected flux (monotonic correction)
      !!
      !! ** Action : - update pt(:,:,:,:,Krhs)  with the now advective tracer trends
      !!             - send trends to trdtra module for further diagnostics (l_trdtra=T)
      !!             - poleward advective heat and salt transport (ln_diaptr=T)
      !!----------------------------------------------------------------------
      INTEGER                                  , INTENT(in   ) ::   kt              ! ocean time-step index
      INTEGER                                  , INTENT(in   ) ::   Kbb, Kmm, Krhs  ! ocean time level indices
      INTEGER                                  , INTENT(in   ) ::   kit000          ! first time step index
      CHARACTER(len=3)                         , INTENT(in   ) ::   cdtype          ! =TRA or TRC (tracer indicator)
      INTEGER                                  , INTENT(in   ) ::   kjpt            ! number of tracers
      INTEGER                                  , INTENT(in   ) ::   kn_fct_h        ! order of the FCT scheme (=2 or 4)
      INTEGER                                  , INTENT(in   ) ::   kn_fct_v        ! order of the FCT scheme (=2 or 4)
      REAL(wp)                                 , INTENT(in   ) ::   p2dt            ! tracer time-step
      REAL(wp), DIMENSION(jpi,jpj,jpk         ), INTENT(in   ) ::   pU, pV, pW      ! 3 ocean volume flux components
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt,jpt), INTENT(inout) ::   pt              ! tracers and RHS of tracer equation
      !
      INTEGER  ::   ji, jj, jk, jn                           ! dummy loop indices
      REAL(wp) ::   ztra                                     ! local scalar
      REAL(wp) ::   zwx_im1, zfp_ui, zfp_ui_m1, zfp_vj, zfp_vj_m1, zfp_wk, zC2t_u, zC4t_u   !   -      -
      REAL(wp) ::   zwy_jm1, zfm_ui, zfm_ui_m1, zfm_vj, zfm_vj_m1, zfm_wk, zC2t_v, zC4t_v   !   -      -
      REAL(wp) ::   ztu, ztv, ztu_im1, ztu_ip1, ztv_jm1, ztv_jp1
      REAL(wp), DIMENSION(jpi,jpj,jpk)        ::   zwi, zwx_3d, zwy_3d, zwz, ztw, zltu_3d, zltv_3d
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   ztrdx, ztrdy, ztrdz, zptry
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   zwinf, zwdia, zwsup
      LOGICAL  ::   ll_zAimp                                 ! flag to apply adaptive implicit vertical advection
      !!----------------------------------------------------------------------
      !
      IF( kt == kit000 )  THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'tra_adv_fct_lf : FCT advection scheme on ', cdtype
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~'
      ENDIF
      !! -- init to 0
      zwx_3d(:,:,:) = 0._wp
      zwy_3d(:,:,:) = 0._wp
      zwz(:,:,:) = 0._wp
      zwi(:,:,:) = 0._wp
      !
      l_trd = .FALSE.            ! set local switches
      l_hst = .FALSE.
      l_ptr = .FALSE.
      ll_zAimp = .FALSE.
      IF( ( cdtype == 'TRA' .AND. l_trdtra  ) .OR. ( cdtype =='TRC' .AND. l_trdtrc ) )      l_trd = .TRUE.
      IF(   cdtype == 'TRA' .AND. ( iom_use( 'sophtadv' ) .OR. iom_use( 'sophtadv' ) ) )    l_ptr = .TRUE.
      IF(   cdtype == 'TRA' .AND. ( iom_use("uadv_heattr") .OR. iom_use("vadv_heattr") .OR.  &
         &                         iom_use("uadv_salttr") .OR. iom_use("vadv_salttr")  ) )  l_hst = .TRUE.
      !
      IF( l_trd .OR. l_hst )  THEN
         ALLOCATE( ztrdx(jpi,jpj,jpk), ztrdy(jpi,jpj,jpk), ztrdz(jpi,jpj,jpk) )
         ztrdx(:,:,:) = 0._wp   ;    ztrdy(:,:,:) = 0._wp   ;   ztrdz(:,:,:) = 0._wp
      ENDIF
      !
      IF( l_ptr ) THEN
         ALLOCATE( zptry(jpi,jpj,jpk) )
         zptry(:,:,:) = 0._wp
      ENDIF
      !
      ! If adaptive vertical advection, check if it is needed on this PE at this time
      IF( ln_zad_Aimp ) THEN
         IF( MAXVAL( ABS( wi(:,:,:) ) ) > 0._wp ) ll_zAimp = .TRUE.
      END IF
      ! If active adaptive vertical advection, build tridiagonal matrix
      IF( ll_zAimp ) THEN
         ALLOCATE(zwdia(jpi,jpj,jpk), zwinf(jpi,jpj,jpk),zwsup(jpi,jpj,jpk))
         DO_3D( 1, 1, 1, 1, 1, jpkm1 )
            zwdia(ji,jj,jk) =  1._wp + p2dt * ( MAX( wi(ji,jj,jk) , 0._wp ) - MIN( wi(ji,jj,jk+1) , 0._wp ) )   &
            &                               / e3t(ji,jj,jk,Krhs)
            zwinf(ji,jj,jk) =  p2dt * MIN( wi(ji,jj,jk  ) , 0._wp ) / e3t(ji,jj,jk,Krhs)
            zwsup(ji,jj,jk) = -p2dt * MAX( wi(ji,jj,jk+1) , 0._wp ) / e3t(ji,jj,jk,Krhs)
         END_3D
      END IF
      !
      DO jn = 1, kjpt            !==  loop over the tracers  ==!
         !
         !        !==  upstream advection with initial mass fluxes & intermediate update  ==!
         !                               !* upstream tracer flux in the k direction *!
         DO_3D( 1, 1, 1, 1, 2, jpkm1 )      ! Interior value ( multiplied by wmask)
            zfp_wk = pW(ji,jj,jk) + ABS( pW(ji,jj,jk) )
            zfm_wk = pW(ji,jj,jk) - ABS( pW(ji,jj,jk) )
            zwz(ji,jj,jk) = 0.5 * ( zfp_wk * pt(ji,jj,jk,jn,Kbb) + zfm_wk * pt(ji,jj,jk-1,jn,Kbb) ) * wmask(ji,jj,jk)
         END_3D
         IF( ln_linssh ) THEN               ! top ocean value (only in linear free surface as zwz has been w-masked)
            IF( ln_isfcav ) THEN                        ! top of the ice-shelf cavities and at the ocean surface
               DO_2D( 1, 1, 1, 1 )
                  zwz(ji,jj, mikt(ji,jj) ) = pW(ji,jj,mikt(ji,jj)) * pt(ji,jj,mikt(ji,jj),jn,Kbb)   ! linear free surface
               END_2D
            ELSE                                        ! no cavities: only at the ocean surface
               DO_2D( 1, 1, 1, 1 )
                  zwz(ji,jj,1) = pW(ji,jj,1) * pt(ji,jj,1,jn,Kbb)
               END_2D
            ENDIF
         ENDIF
         !
         !                    !* upstream tracer flux in the i and j direction
         DO jk = 1, jpkm1
            DO jj = 1, jpj-1
               tracer_flux_i(zwx_3d(1,jj,jk),zfp_ui,zfm_ui,1,jj,jk)
               tracer_flux_j(zwy_3d(1,jj,jk),zfp_vj,zfm_vj,1,jj,jk)
            END DO
            DO ji = 1, jpi-1
               tracer_flux_i(zwx_3d(ji,1,jk),zfp_ui,zfm_ui,ji,1,jk)
               tracer_flux_j(zwy_3d(ji,1,jk),zfp_vj,zfm_vj,ji,1,jk)
            END DO
            DO_2D( 1, 1, 1, 1 )
               tracer_flux_i(zwx_3d(ji,jj,jk),zfp_ui,zfm_ui,ji,jj,jk)
               tracer_flux_i(zwx_im1,zfp_ui_m1,zfm_ui_m1,ji-1,jj,jk)
               tracer_flux_j(zwy_3d(ji,jj,jk),zfp_vj,zfm_vj,ji,jj,jk)
               tracer_flux_j(zwy_jm1,zfp_vj_m1,zfm_vj_m1,ji,jj-1,jk)
               ztra = - ( zwx_3d(ji,jj,jk) - zwx_im1 + zwy_3d(ji,jj,jk) - zwy_jm1 + zwz(ji,jj,jk) - zwz(ji,jj,jk+1) ) * r1_e1e2t(ji,jj)
               !                               ! update and guess with monotonic sheme
               pt(ji,jj,jk,jn,Krhs) =                   pt(ji,jj,jk,jn,Krhs) +       ztra   &
                  &                                  / e3t(ji,jj,jk,Kmm ) * tmask(ji,jj,jk)
               zwi(ji,jj,jk)    = ( e3t(ji,jj,jk,Kbb) * pt(ji,jj,jk,jn,Kbb) + p2dt * ztra ) &
                  &                                  / e3t(ji,jj,jk,Krhs) * tmask(ji,jj,jk)
            END_2D
         END DO

         IF ( ll_zAimp ) THEN
            CALL tridia_solver( zwdia, zwsup, zwinf, zwi, zwi , 0 )
            !
            ztw(:,:,1) = 0._wp ; ztw(:,:,jpk) = 0._wp ;
            DO_3D( 1, 1, 1, 1, 2, jpkm1 )       ! Interior value ( multiplied by wmask)
               zfp_wk = wi(ji,jj,jk) + ABS( wi(ji,jj,jk) )
               zfm_wk = wi(ji,jj,jk) - ABS( wi(ji,jj,jk) )
               ztw(ji,jj,jk) =  0.5 * e1e2t(ji,jj) * ( zfp_wk * zwi(ji,jj,jk) + zfm_wk * zwi(ji,jj,jk-1) ) * wmask(ji,jj,jk)
               zwz(ji,jj,jk) = zwz(ji,jj,jk) + ztw(ji,jj,jk) ! update vertical fluxes
            END_3D
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               pt(ji,jj,jk,jn,Krhs) = pt(ji,jj,jk,jn,Krhs) - ( ztw(ji,jj,jk) - ztw(ji  ,jj  ,jk+1) ) &
                  &                                        * r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kmm)
            END_3D
            !
         END IF
         !
         IF( l_trd .OR. l_hst )  THEN             ! trend diagnostics (contribution of upstream fluxes)
            ztrdx(:,:,:) = zwx_3d(:,:,:)   ;   ztrdy(:,:,:) = zwy_3d(:,:,:)   ;   ztrdz(:,:,:) = zwz(:,:,:)
         END IF
         !                             ! "Poleward" heat and salt transports (contribution of upstream fluxes)
         IF( l_ptr )   zptry(:,:,:) = zwy_3d(:,:,:)
         !
         !        !==  anti-diffusive flux : high order minus low order  ==!
         !
         SELECT CASE( kn_fct_h )    !* horizontal anti-diffusive fluxes
         !
         CASE(  2  )                   !- 2nd order centered
            DO_3D( 2, 1, 2, 1, 1, jpkm1 )
               zwx_3d(ji,jj,jk) = 0.5_wp * pU(ji,jj,jk) * ( pt(ji,jj,jk,jn,Kmm) + pt(ji+1,jj,jk,jn,Kmm) ) - zwx_3d(ji,jj,jk)
               zwy_3d(ji,jj,jk) = 0.5_wp * pV(ji,jj,jk) * ( pt(ji,jj,jk,jn,Kmm) + pt(ji,jj+1,jk,jn,Kmm) ) - zwy_3d(ji,jj,jk)
            END_3D
            !
         CASE(  4  )                   !- 4th order centered
            zltu_3d(:,:,jpk) = 0._wp            ! Bottom value : flux set to zero
            zltv_3d(:,:,jpk) = 0._wp
            !                          ! Laplacian
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )                 ! 2nd derivative * 1/ 6
                  !             ! 1st derivative (gradient)
                  ztu = ( pt(ji+1,jj,jk,jn,Kmm) - pt(ji,jj,jk,jn,Kmm) ) * umask(ji,jj,jk)
                  ztu_im1 = ( pt(ji,jj,jk,jn,Kmm) - pt(ji-1,jj,jk,jn,Kmm) ) * umask(ji-1,jj,jk)
                  ztv = ( pt(ji,jj+1,jk,jn,Kmm) - pt(ji,jj,jk,jn,Kmm) ) * vmask(ji,jj,jk)
                  ztv_jm1 = ( pt(ji,jj,jk,jn,Kmm) - pt(ji,jj-1,jk,jn,Kmm) ) * vmask(ji,jj-1,jk)
                  !             ! 2nd derivative * 1/ 6
                  zltu_3d(ji,jj,jk) = (  ztu + ztu_im1  ) * r1_6
                  zltv_3d(ji,jj,jk) = (  ztv + ztv_jm1  ) * r1_6
               END_2D
            END DO
            ! NOTE [ comm_cleanup ] : need to change sign to ensure halo 1 - halo 2 compatibility
            CALL lbc_lnk( 'traadv_fct', zltu_3d, 'T', -1.0_wp , zltv_3d, 'T', -1.0_wp )   ! Lateral boundary cond. (unchanged sgn)
            !
            DO_3D( 2, 1, 2, 1, 1, jpkm1 )
               zC2t_u = pt(ji,jj,jk,jn,Kmm) + pt(ji+1,jj  ,jk,jn,Kmm)   ! 2 x C2 interpolation of T at u- & v-points
               zC2t_v = pt(ji,jj,jk,jn,Kmm) + pt(ji  ,jj+1,jk,jn,Kmm)
               !                                                        ! C4 minus upstream advective fluxes
               ! round brackets added to fix the order of floating point operations
               ! needed to ensure halo 1 - halo 2 compatibility
               zwx_3d(ji,jj,jk) =  0.5_wp * pU(ji,jj,jk) * ( zC2t_u + ( zltu_3d(ji,jj,jk) - zltu_3d(ji+1,jj,jk)   &
                             &                                        )                                           & ! bracket for halo 1 - halo 2 compatibility
                             &                             ) - zwx_3d(ji,jj,jk)
               zwy_3d(ji,jj,jk) =  0.5_wp * pV(ji,jj,jk) * ( zC2t_v + ( zltv_3d(ji,jj,jk) - zltv_3d(ji,jj+1,jk)   &
                             &                                        )                                           & ! bracket for halo 1 - halo 2 compatibility
                             &                             ) - zwy_3d(ji,jj,jk)
            END_3D
            !
         CASE(  41 )                   !- 4th order centered       ==>>   !!gm coding attempt   need to be tested
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )    ! Horizontal advective fluxes
               ztu_im1 = ( pt(ji  ,jj  ,jk,jn,Kmm) - pt(ji-1,jj,jk,jn,Kmm) ) * umask(ji-1,jj,jk)
               ztu_ip1 = ( pt(ji+2,jj  ,jk,jn,Kmm) - pt(ji+1,jj,jk,jn,Kmm) ) * umask(ji+1,jj,jk)

               ztv_jm1 = ( pt(ji,jj  ,jk,jn,Kmm) - pt(ji,jj-1,jk,jn,Kmm) ) * vmask(ji,jj-1,jk)
               ztv_jp1 = ( pt(ji,jj+2,jk,jn,Kmm) - pt(ji,jj+1,jk,jn,Kmm) ) * vmask(ji,jj+1,jk)
               zC2t_u = pt(ji,jj,jk,jn,Kmm) + pt(ji+1,jj  ,jk,jn,Kmm)   ! 2 x C2 interpolation of T at u- & v-points (x2)
               zC2t_v = pt(ji,jj,jk,jn,Kmm) + pt(ji  ,jj+1,jk,jn,Kmm)
               !                                                  ! C4 interpolation of T at u- & v-points (x2)
               zC4t_u =  zC2t_u + r1_6 * ( ztu_im1 - ztu_ip1 )
               zC4t_v =  zC2t_v + r1_6 * ( ztv_jm1 - ztv_jp1 )
               !                                                  ! C4 minus upstream advective fluxes
               zwx_3d(ji,jj,jk) =  0.5_wp * pU(ji,jj,jk) * zC4t_u - zwx_3d(ji,jj,jk)
               zwy_3d(ji,jj,jk) =  0.5_wp * pV(ji,jj,jk) * zC4t_v - zwy_3d(ji,jj,jk)
            END_3D
            CALL lbc_lnk( 'traadv_fct', zwx_3d, 'U', -1.0_wp , zwy_3d, 'V', -1.0_wp )   ! Lateral boundary cond. (unchanged sgn)
            !
         END SELECT
         !
         SELECT CASE( kn_fct_v )    !* vertical anti-diffusive fluxes (w-masked interior values)
         !
         CASE(  2  )                   !- 2nd order centered
            DO_3D( 1, 1, 1, 1, 2, jpkm1 )
               zwz(ji,jj,jk) =  (  pW(ji,jj,jk) * 0.5_wp * ( pt(ji,jj,jk,jn,Kmm) + pt(ji,jj,jk-1,jn,Kmm) )   &
                  &              - zwz(ji,jj,jk)  ) * wmask(ji,jj,jk)
            END_3D
            !
         CASE(  4  )                   !- 4th order COMPACT
            CALL interp_4th_cpt( pt(:,:,:,jn,Kmm) , ztw )   ! zwt = COMPACT interpolation of T at w-point
            DO_3D( 1, 1, 1, 1, 2, jpkm1 )
               zwz(ji,jj,jk) = ( pW(ji,jj,jk) * ztw(ji,jj,jk) - zwz(ji,jj,jk) ) * wmask(ji,jj,jk)
            END_3D
            !
         END SELECT
         IF( ln_linssh ) THEN    ! top ocean value: high order = upstream  ==>>  zwz=0
            zwz(:,:,1) = 0._wp   ! only ocean surface as interior zwz values have been w-masked
         ENDIF
         !
         CALL lbc_lnk( 'traadv_fct', zwi, 'T', 1.0_wp)
         !
         IF ( ll_zAimp ) THEN
            DO_3D( 1, 1, 1, 1, 1, jpkm1 )    !* trend and after field with monotonic scheme
               !                                                ! total intermediate advective trends
               ztra = - (  zwx_3d(ji,jj,jk) - zwx_3d(ji-1,jj  ,jk  )   &
                  &      + zwy_3d(ji,jj,jk) - zwy_3d(ji  ,jj-1,jk  )   &
                  &      + zwz(ji,jj,jk) - zwz(ji  ,jj  ,jk+1) ) * r1_e1e2t(ji,jj)
               ztw(ji,jj,jk) = zwi(ji,jj,jk) + p2dt * ztra / e3t(ji,jj,jk,Krhs) * tmask(ji,jj,jk)
            END_3D
            !
            CALL tridia_solver( zwdia, zwsup, zwinf, ztw, ztw , 0 )
            !
            DO_3D( 1, 1, 1, 1, 2, jpkm1 )       ! Interior value ( multiplied by wmask)
               zfp_wk = wi(ji,jj,jk) + ABS( wi(ji,jj,jk) )
               zfm_wk = wi(ji,jj,jk) - ABS( wi(ji,jj,jk) )
               zwz(ji,jj,jk) = zwz(ji,jj,jk) + 0.5 * e1e2t(ji,jj) * ( zfp_wk * ztw(ji,jj,jk) + zfm_wk * ztw(ji,jj,jk-1) ) * wmask(ji,jj,jk)
            END_3D
         END IF
         !
         !        !==  monotonicity algorithm  ==!
         !
         CALL nonosc( Kmm, pt(:,:,:,jn,Kbb), zwx_3d, zwy_3d, zwz, zwi, p2dt )
         !
         !        !==  final trend with corrected fluxes  ==!
         !
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            ztra = - (  zwx_3d(ji,jj,jk) - zwx_3d(ji-1,jj  ,jk  )   &
               &      + zwy_3d(ji,jj,jk) - zwy_3d(ji  ,jj-1,jk  )   &
               &      + zwz(ji,jj,jk) - zwz(ji  ,jj  ,jk+1) ) * r1_e1e2t(ji,jj)
            pt(ji,jj,jk,jn,Krhs) = pt(ji,jj,jk,jn,Krhs) + ztra / e3t(ji,jj,jk,Kmm)
            zwi(ji,jj,jk) = zwi(ji,jj,jk) + p2dt * ztra / e3t(ji,jj,jk,Krhs) * tmask(ji,jj,jk)
         END_3D
         !
         IF ( ll_zAimp ) THEN
            !
            ztw(:,:,1) = 0._wp ; ztw(:,:,jpk) = 0._wp
            DO_3D( 0, 0, 0, 0, 2, jpkm1 )      ! Interior value ( multiplied by wmask)
               zfp_wk = wi(ji,jj,jk) + ABS( wi(ji,jj,jk) )
               zfm_wk = wi(ji,jj,jk) - ABS( wi(ji,jj,jk) )
               ztw(ji,jj,jk) = - 0.5 * e1e2t(ji,jj) * ( zfp_wk * zwi(ji,jj,jk) + zfm_wk * zwi(ji,jj,jk-1) ) * wmask(ji,jj,jk)
               zwz(ji,jj,jk) = zwz(ji,jj,jk) + ztw(ji,jj,jk) ! Update vertical fluxes for trend diagnostic
            END_3D
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               pt(ji,jj,jk,jn,Krhs) = pt(ji,jj,jk,jn,Krhs) - ( ztw(ji,jj,jk) - ztw(ji  ,jj  ,jk+1) ) &
                  &                                        * r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kmm)
            END_3D
         END IF
         ! NOT TESTED - NEED l_trd OR l_hst TRUE
         IF( l_trd .OR. l_hst ) THEN   ! trend diagnostics // heat/salt transport
            ztrdx(:,:,:) = ztrdx(:,:,:) + zwx_3d(:,:,:)  ! <<< add anti-diffusive fluxes
            ztrdy(:,:,:) = ztrdy(:,:,:) + zwy_3d(:,:,:)  !     to upstream fluxes
            ztrdz(:,:,:) = ztrdz(:,:,:) + zwz(:,:,:)  !
            !
            IF( l_trd ) THEN              ! trend diagnostics
               CALL trd_tra( kt, Kmm, Krhs, cdtype, jn, jptra_xad, ztrdx, pU, pt(:,:,:,jn,Kmm) )
               CALL trd_tra( kt, Kmm, Krhs, cdtype, jn, jptra_yad, ztrdy, pV, pt(:,:,:,jn,Kmm) )
               CALL trd_tra( kt, Kmm, Krhs, cdtype, jn, jptra_zad, ztrdz, pW, pt(:,:,:,jn,Kmm) )
            ENDIF
            !                             ! heat/salt transport
            IF( l_hst )   CALL dia_ar5_hst( jn, 'adv', ztrdx(:,:,:), ztrdy(:,:,:) )
            !
         ENDIF
         ! NOT TESTED - NEED l_ptr TRUE
         IF( l_ptr ) THEN              ! "Poleward" transports
            zptry(:,:,:) = zptry(:,:,:) + zwy_3d(:,:,:)  ! <<< add anti-diffusive fluxes
            CALL dia_ptr_hst( jn, 'adv', zptry(:,:,:) )
         ENDIF
         !
      END DO                     ! end of tracer loop
      !
      IF ( ll_zAimp ) THEN
         DEALLOCATE( zwdia, zwinf, zwsup )
      ENDIF
      IF( l_trd .OR. l_hst ) THEN
         DEALLOCATE( ztrdx, ztrdy, ztrdz )
      ENDIF
      IF( l_ptr ) THEN
         DEALLOCATE( zptry )
      ENDIF
      !
   END SUBROUTINE tra_adv_fct_lf
#endif
   !!======================================================================
END MODULE traadv_fct
