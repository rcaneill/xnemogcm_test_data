MODULE traadv_qck_lf
   !!==============================================================================
   !!                       ***  MODULE  traadv_qck  ***
   !! Ocean tracers:  horizontal & vertical advective trend
   !!==============================================================================
   !! History :  3.0  !  2008-07  (G. Reffray)  Original code
   !!            3.3  !  2010-05  (C.Ethe, G. Madec)  merge TRC-TRA + switch from velocity to transport
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_adv_qck    : update the tracer trend with the horizontal advection
   !!                    trends using a 3rd order finite difference scheme
   !!   tra_adv_qck_i  : apply QUICK scheme in i-direction
   !!   tra_adv_qck_j  : apply QUICK scheme in j-direction
   !!   tra_adv_cen2_k : 2nd centered scheme for the vertical advection
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and active tracers
   USE dom_oce         ! ocean space and time domain
   USE trc_oce         ! share passive tracers/Ocean variables
   USE trd_oce         ! trends: ocean variables
   USE trdtra          ! trends manager: tracers
   USE diaptr          ! poleward transport diagnostics
   USE iom
   !
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! distribued memory computing
   USE lbclnk          ! ocean lateral boundary condition (or mpp link)
   USE lib_fortran     ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_adv_qck_lf   ! routine called by step.F90

   REAL(wp) :: r1_6 = 1./ 6.   ! 1/6 ratio

   LOGICAL  ::   l_trd   ! flag to compute trends
   LOGICAL  ::   l_ptr   ! flag to compute poleward transport


   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: traadv_qck.F90 14776 2021-04-30 12:33:41Z mocavero $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tra_adv_qck_lf ( kt, kit000, cdtype, p2dt, pU, pV, pW, Kbb, Kmm, pt, kjpt, Krhs )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_adv_qck  ***
      !!
      !! ** Purpose :   Compute the now trend due to the advection of tracers
      !!      and add it to the general trend of passive tracer equations.
      !!
      !! ** Method :   The advection is evaluated by a third order scheme
      !!             For a positive velocity u :              u(i)>0
      !!                                          |--FU--|--FC--|--FD--|------|
      !!                                             i-1    i      i+1   i+2
      !!
      !!             For a negative velocity u :              u(i)<0
      !!                                          |------|--FD--|--FC--|--FU--|
      !!                                             i-1    i      i+1   i+2
      !!             where  FU is the second upwind point
      !!                    FD is the first douwning point
      !!                    FC is the central point (or the first upwind point)
      !!
      !!      Flux(i) = u(i) * { 0.5(FC+FD)  -0.5C(i)(FD-FC)  -((1-C(i))/6)(FU+FD-2FC) }
      !!                with C(i)=|u(i)|dx(i)/dt (=Courant number)
      !!
      !!         dt = 2*rdtra and the scalar values are tb and sb
      !!
      !!       On the vertical, the simple centered scheme used pt(:,:,:,:,Kmm)
      !!
      !!               The fluxes are bounded by the ULTIMATE limiter to
      !!             guarantee the monotonicity of the solution and to
      !!            prevent the appearance of spurious numerical oscillations
      !!
      !! ** Action : - update pt(:,:,:,:,Krhs)  with the now advective tracer trends
      !!             - send trends to trdtra module for further diagnostcs (l_trdtra=T)
      !!             - poleward advective heat and salt transport (ln_diaptr=T)
      !!
      !! ** Reference : Leonard (1979, 1991)
      !!----------------------------------------------------------------------
      INTEGER                                  , INTENT(in   ) ::   kt              ! ocean time-step index
      INTEGER                                  , INTENT(in   ) ::   Kbb, Kmm, Krhs  ! ocean time level indices
      INTEGER                                  , INTENT(in   ) ::   kit000          ! first time step index
      CHARACTER(len=3)                         , INTENT(in   ) ::   cdtype          ! =TRA or TRC (tracer indicator)
      INTEGER                                  , INTENT(in   ) ::   kjpt            ! number of tracers
      REAL(wp)                                 , INTENT(in   ) ::   p2dt            ! tracer time-step
      ! TEMP: [tiling] This can be A2D(nn_hls) if using XIOS (subdomain support)
      REAL(wp), DIMENSION(jpi,jpj,jpk         ), INTENT(in   ) ::   pU, pV, pW      ! 3 ocean volume transport components
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt,jpt), INTENT(inout) ::   pt              ! tracers and RHS of tracer equation
      !!----------------------------------------------------------------------
      !
      IF( ntile == 0 .OR. ntile == 1 )  THEN                       ! Do only on the first tile
         IF( kt == kit000 )  THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'tra_adv_qck : 3rd order quickest advection scheme on ', cdtype
            IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~'
            IF(lwp) WRITE(numout,*)
         ENDIF
         !
         l_trd = .FALSE.
         l_ptr = .FALSE.
         IF( ( cdtype == 'TRA' .AND. l_trdtra ) .OR. ( cdtype == 'TRC' .AND. l_trdtrc ) )   l_trd = .TRUE.
         IF(   cdtype == 'TRA' .AND. ( iom_use( 'sophtadv' ) .OR. iom_use( 'sophtadv' ) ) ) l_ptr = .TRUE.
      ENDIF
      !
      !        ! horizontal fluxes are computed with the QUICKEST + ULTIMATE scheme
      CALL tra_adv_qck_i_lf( kt, cdtype, p2dt, pU, Kbb, Kmm, pt, kjpt, Krhs )
      CALL tra_adv_qck_j_lf( kt, cdtype, p2dt, pV, Kbb, Kmm, pt, kjpt, Krhs )

      !        ! vertical fluxes are computed with the 2nd order centered scheme
      CALL tra_adv_cen2_k_lf( kt, cdtype, pW, Kmm, pt, kjpt, Krhs )
      !
   END SUBROUTINE tra_adv_qck_lf


   SUBROUTINE tra_adv_qck_i_lf( kt, cdtype, p2dt, pU, Kbb, Kmm, pt, kjpt, Krhs )
      !!----------------------------------------------------------------------
      !!
      !!----------------------------------------------------------------------
      INTEGER                                  , INTENT(in   ) ::   kt         ! ocean time-step index
      INTEGER                                  , INTENT(in   ) ::   Kbb, Kmm, Krhs  ! ocean time level indices
      CHARACTER(len=3)                         , INTENT(in   ) ::   cdtype     ! =TRA or TRC (tracer indicator)
      INTEGER                                  , INTENT(in   ) ::   kjpt       ! number of tracers
      REAL(wp)                                 , INTENT(in   ) ::   p2dt       ! tracer time-step
      ! TEMP: [tiling] This can be A2D(nn_hls) if using XIOS (subdomain support)
      REAL(wp), DIMENSION(jpi,jpj,jpk         ), INTENT(in   ) ::   pU        ! i-velocity components
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt,jpt), INTENT(inout) ::   pt              ! active tracers and RHS of tracer equation
      !!
      INTEGER  ::   ji, jj, jk, jn   ! dummy loop indices
      REAL(wp) ::   ztra, zbtr, zdir, zdx, zmsk  ! local scalars
      REAL(wp) ::   zzfc, zzfd, zzfu, zzfu_ip1   !   -     -
      REAL(wp), DIMENSION(A2D(nn_hls),jpk) ::   zwx, zfu, zfc, zfd
      !----------------------------------------------------------------------
      !
      !                                                          ! ===========
      DO jn = 1, kjpt                                            ! tracer loop
         !                                                       ! ===========
         zfu(:,:,:) = 0._wp     ;   zfc(:,:,:) = 0._wp
         zfd(:,:,:) = 0._wp     ;   zwx(:,:,:) = 0._wp
         !
!!gm why not using a SHIFT instruction...
         DO_3D( 1, 0, 0, 0, 1, jpkm1 )     !--- Computation of the ustream and downstream value of the tracer and the mask
            zzfc = pt(ji-1,jj,jk,jn,Kbb)        ! Upstream   in the x-direction for the tracer
            zzfd = pt(ji+2,jj,jk,jn,Kbb)        ! Downstream in the x-direction for the tracer
            !
            ! Horizontal advective fluxes
            ! ---------------------------
            zdir = 0.5 + SIGN( 0.5_wp, pU(ji,jj,jk) )   ! if pU > 0 : zdir = 1 otherwise zdir = 0
            zfu(ji,jj,jk) = zdir * zzfc + ( 1. - zdir ) * zzfd  ! FU in the x-direction for T
            !
            zdir = 0.5 + SIGN( 0.5_wp, pU(ji,jj,jk) )   ! if pU > 0 : zdir = 1 otherwise zdir = 0
            zdx = ( zdir * e1t(ji,jj) + ( 1. - zdir ) * e1t(ji+1,jj) ) * e2u(ji,jj) * e3u(ji,jj,jk,Kmm)
            zwx(ji,jj,jk)  = ABS( pU(ji,jj,jk) ) * p2dt / zdx    ! (0<zc_cfl<1 : Courant number on x-direction)
            zfc(ji,jj,jk)  = zdir * pt(ji  ,jj,jk,jn,Kbb) + ( 1. - zdir ) * pt(ji+1,jj,jk,jn,Kbb)  ! FC in the x-direction for T
            zfd(ji,jj,jk)  = zdir * pt(ji+1,jj,jk,jn,Kbb) + ( 1. - zdir ) * pt(ji  ,jj,jk,jn,Kbb)  ! FD in the x-direction for T
         END_3D
         !--- Lateral boundary conditions

         !--- QUICKEST scheme
         CALL quickest( zfu, zfd, zfc, zwx )
         !
         ! Mask at the T-points in the x-direction (mask=0 or mask=1)
         DO_3D( 1, 0, 0, 0, 1, jpkm1 )
            zzfu = tmask(ji-1,jj,jk) + tmask(ji,jj,jk) + tmask(ji+1,jj,jk) - 2.
            zzfu_ip1 = tmask(ji,jj,jk) + tmask(ji+1,jj,jk) + tmask(ji+2,jj,jk) - 2.
            !
            ! Tracer flux on the x-direction
            zdir = 0.5 + SIGN( 0.5_wp, pU(ji,jj,jk) )   ! if pU > 0 : zdir = 1 otherwise zdir = 0
            !--- If the second ustream point is a land point
            !--- the flux is computed by the 1st order UPWIND scheme
            zmsk = zdir * zzfu + ( 1. - zdir ) * zzfu_ip1
            zwx(ji,jj,jk) = zmsk * zwx(ji,jj,jk) + ( 1. - zmsk ) * zfc(ji,jj,jk)
            zwx(ji,jj,jk) = zwx(ji,jj,jk) * pU(ji,jj,jk)
         END_3D
         !
         ! Computation of the trend
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            zbtr = r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kmm)
            ! horizontal advective trends
            ztra = - zbtr * ( zwx(ji,jj,jk) - zwx(ji-1,jj,jk) )
            !--- add it to the general tracer trends
            pt(ji,jj,jk,jn,Krhs) = pt(ji,jj,jk,jn,Krhs) + ztra
         END_3D
         !                                 ! trend diagnostics
         IF( l_trd )   CALL trd_tra( kt, Kmm, Krhs, cdtype, jn, jptra_xad, zwx, pU, pt(:,:,:,jn,Kmm) )
         !
      END DO
      !
   END SUBROUTINE tra_adv_qck_i_lf


   SUBROUTINE tra_adv_qck_j_lf( kt, cdtype, p2dt, pV, Kbb, Kmm, pt, kjpt, Krhs )
      !!----------------------------------------------------------------------
      !!
      !!----------------------------------------------------------------------
      INTEGER                                  , INTENT(in   ) ::   kt         ! ocean time-step index
      INTEGER                                  , INTENT(in   ) ::   Kbb, Kmm, Krhs  ! ocean time level indices
      CHARACTER(len=3)                         , INTENT(in   ) ::   cdtype     ! =TRA or TRC (tracer indicator)
      INTEGER                                  , INTENT(in   ) ::   kjpt       ! number of tracers
      REAL(wp)                                 , INTENT(in   ) ::   p2dt       ! tracer time-step
      ! TEMP: [tiling] This can be A2D(nn_hls) if using XIOS (subdomain support)
      REAL(wp), DIMENSION(jpi,jpj,jpk         ), INTENT(in   ) ::   pV        ! j-velocity components
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt,jpt), INTENT(inout) ::   pt              ! active tracers and RHS of tracer equation
      !!
      INTEGER  :: ji, jj, jk, jn                ! dummy loop indices
      REAL(wp) :: ztra, zbtr, zdir, zdx, zmsk   ! local scalars
      REAL(wp) :: zzfc, zzfd, zzfu, zzfu_jp1    !   -     -
      REAL(wp), DIMENSION(A2D(nn_hls),jpk) ::   zwy, zfu, zfc, zfd   ! 3D workspace
      !----------------------------------------------------------------------
      !
      !                                                          ! ===========
      DO jn = 1, kjpt                                            ! tracer loop
         !                                                       ! ===========
         zfu(:,:,:) = 0.0     ;   zfc(:,:,:) = 0.0
         zfd(:,:,:) = 0.0     ;   zwy(:,:,:) = 0.0
         !
         !--- Computation of the ustream and downstream value of the tracer and the mask
         DO_3D( 0, 0, 1, 0, 1, jpkm1 )
            ! Upstream in the x-direction for the tracer
            zzfc = pt(ji,jj-1,jk,jn,Kbb)
            ! Downstream in the x-direction for the tracer
            zzfd = pt(ji,jj+2,jk,jn,Kbb)
            !
            ! Horizontal advective fluxes
            ! ---------------------------
            !
            zdir = 0.5 + SIGN( 0.5_wp, pV(ji,jj,jk) )   ! if pU > 0 : zdir = 1 otherwise zdir = 0
            zfu(ji,jj,jk) = zdir * zzfc + ( 1. - zdir ) * zzfd  ! FU in the x-direction for T
            !
            zdir = 0.5 + SIGN( 0.5_wp, pV(ji,jj,jk) )   ! if pU > 0 : zdir = 1 otherwise zdir = 0
            zdx = ( zdir * e2t(ji,jj) + ( 1. - zdir ) * e2t(ji,jj+1) ) * e1v(ji,jj) * e3v(ji,jj,jk,Kmm)
            zwy(ji,jj,jk)  = ABS( pV(ji,jj,jk) ) * p2dt / zdx    ! (0<zc_cfl<1 : Courant number on x-direction)
            zfc(ji,jj,jk)  = zdir * pt(ji,jj  ,jk,jn,Kbb) + ( 1. - zdir ) * pt(ji,jj+1,jk,jn,Kbb)  ! FC in the x-direction for T
            zfd(ji,jj,jk)  = zdir * pt(ji,jj+1,jk,jn,Kbb) + ( 1. - zdir ) * pt(ji,jj  ,jk,jn,Kbb)  ! FD in the x-direction for T
         END_3D

         !--- QUICKEST scheme
         CALL quickest( zfu, zfd, zfc, zwy )
         !
         ! Mask at the T-points in the x-direction (mask=0 or mask=1)
         DO_3D( 0, 0, 1, 0, 1, jpkm1 )
            zzfu = tmask(ji,jj-1,jk) + tmask(ji,jj,jk) + tmask(ji,jj+1,jk) - 2.
            zzfu_jp1 = tmask(ji,jj,jk) + tmask(ji,jj+1,jk) + tmask(ji,jj+2,jk) - 2.
            !
            ! Tracer flux on the x-direction
            zdir = 0.5 + SIGN( 0.5_wp, pV(ji,jj,jk) )   ! if pU > 0 : zdir = 1 otherwise zdir = 0
            !--- If the second ustream point is a land point
            !--- the flux is computed by the 1st order UPWIND scheme
            zmsk = zdir * zzfu + ( 1. - zdir ) * zzfu_jp1
            zwy(ji,jj,jk) = zmsk * zwy(ji,jj,jk) + ( 1. - zmsk ) * zfc(ji,jj,jk)
            zwy(ji,jj,jk) = zwy(ji,jj,jk) * pV(ji,jj,jk)
         END_3D
         !
         ! Computation of the trend
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            zbtr = r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kmm)
            ! horizontal advective trends
            ztra = - zbtr * ( zwy(ji,jj,jk) - zwy(ji,jj-1,jk) )
            !--- add it to the general tracer trends
            pt(ji,jj,jk,jn,Krhs) = pt(ji,jj,jk,jn,Krhs) + ztra
         END_3D
         !                                 ! trend diagnostics
         IF( l_trd )   CALL trd_tra( kt, Kmm, Krhs, cdtype, jn, jptra_yad, zwy, pV, pt(:,:,:,jn,Kmm) )
         !                                 ! "Poleward" heat and salt transports (contribution of upstream fluxes)
         IF( l_ptr )   CALL dia_ptr_hst( jn, 'adv', zwy(:,:,:) )
         !
      END DO
      !
   END SUBROUTINE tra_adv_qck_j_lf


   SUBROUTINE tra_adv_cen2_k_lf( kt, cdtype, pW, Kmm, pt, kjpt, Krhs )
      !!----------------------------------------------------------------------
      !!
      !!----------------------------------------------------------------------
      INTEGER                                  , INTENT(in   ) ::   kt       ! ocean time-step index
      INTEGER                                  , INTENT(in   ) ::   Kmm, Krhs  ! ocean time level indices
      CHARACTER(len=3)                         , INTENT(in   ) ::   cdtype   ! =TRA or TRC (tracer indicator)
      INTEGER                                  , INTENT(in   ) ::   kjpt     ! number of tracers
      ! TEMP: [tiling] This can be A2D(nn_hls) if using XIOS (subdomain support)
      REAL(wp), DIMENSION(jpi,jpj,jpk         ), INTENT(in   ) ::   pW      ! vertical velocity
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt,jpt), INTENT(inout) ::   pt              ! active tracers and RHS of tracer equation
      !
      INTEGER  ::   ji, jj, jk, jn   ! dummy loop indices
      REAL(wp), DIMENSION(A2D(nn_hls),jpk) ::   zwz   ! 3D workspace
      !!----------------------------------------------------------------------
      !
      zwz(:,:, 1 ) = 0._wp       ! surface & bottom values set to zero for all tracers
      zwz(:,:,jpk) = 0._wp
      !
      !                                                          ! ===========
      DO jn = 1, kjpt                                            ! tracer loop
         !                                                       ! ===========
         !
         DO_3D( 0, 0, 0, 0, 2, jpkm1 )       !* Interior point   (w-masked 2nd order centered flux)
            zwz(ji,jj,jk) = 0.5 * pW(ji,jj,jk) * ( pt(ji,jj,jk-1,jn,Kmm) + pt(ji,jj,jk,jn,Kmm) ) * wmask(ji,jj,jk)
         END_3D
         IF( ln_linssh ) THEN                !* top value   (only in linear free surf. as zwz is multiplied by wmask)
            IF( ln_isfcav ) THEN                  ! ice-shelf cavities (top of the ocean)
               DO_2D( 0, 0, 0, 0 )
                  zwz(ji,jj, mikt(ji,jj) ) = pW(ji,jj,mikt(ji,jj)) * pt(ji,jj,mikt(ji,jj),jn,Kmm)   ! linear free surface
               END_2D
            ELSE                                   ! no ocean cavities (only ocean surface)
               DO_2D( 0, 0, 0, 0 )
                  zwz(ji,jj,1) = pW(ji,jj,1) * pt(ji,jj,1,jn,Kmm)
               END_2D
            ENDIF
         ENDIF
         !
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )   !==  Tracer flux divergence added to the general trend  ==!
            pt(ji,jj,jk,jn,Krhs) = pt(ji,jj,jk,jn,Krhs) - ( zwz(ji,jj,jk) - zwz(ji,jj,jk+1) )   &
               &                                * r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kmm)
         END_3D
         !                                 ! Send trends for diagnostic
         IF( l_trd )  CALL trd_tra( kt, Kmm, Krhs, cdtype, jn, jptra_zad, zwz, pW, pt(:,:,:,jn,Kmm) )
         !
      END DO
      !
   END SUBROUTINE tra_adv_cen2_k_lf


   SUBROUTINE quickest( pfu, pfd, pfc, puc )
      !!----------------------------------------------------------------------
      !!
      !! ** Purpose :  Computation of advective flux with Quickest scheme
      !!
      !! ** Method :
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(A2D(nn_hls),jpk), INTENT(in   ) ::   pfu   ! second upwind point
      REAL(wp), DIMENSION(A2D(nn_hls),jpk), INTENT(in   ) ::   pfd   ! first douwning point
      REAL(wp), DIMENSION(A2D(nn_hls),jpk), INTENT(in   ) ::   pfc   ! the central point (or the first upwind point)
      REAL(wp), DIMENSION(A2D(nn_hls),jpk), INTENT(inout) ::   puc   ! input as Courant number ; output as flux
      !!
      INTEGER  ::  ji, jj, jk               ! dummy loop indices
      REAL(wp) ::  zcoef1, zcoef2, zcoef3   ! local scalars
      REAL(wp) ::  zc, zcurv, zfho          !   -      -
      !----------------------------------------------------------------------
      !
      DO_3D( 2, 2, 2, 2, 1, jpkm1 )
         zc     = puc(ji,jj,jk)                         ! Courant number
         zcurv  = pfd(ji,jj,jk) + pfu(ji,jj,jk) - 2. * pfc(ji,jj,jk)
         zcoef1 = 0.5 *      ( pfc(ji,jj,jk) + pfd(ji,jj,jk) )
         zcoef2 = 0.5 * zc * ( pfd(ji,jj,jk) - pfc(ji,jj,jk) )
         zcoef3 = ( 1. - ( zc * zc ) ) * r1_6 * zcurv
         zfho   = zcoef1 - zcoef2 - zcoef3              !  phi_f QUICKEST
         !
         zcoef1 = pfd(ji,jj,jk) - pfu(ji,jj,jk)
         zcoef2 = ABS( zcoef1 )
         zcoef3 = ABS( zcurv )
         IF( zcoef3 >= zcoef2 ) THEN
            zfho = pfc(ji,jj,jk)
         ELSE
            zcoef3 = pfu(ji,jj,jk) + ( ( pfc(ji,jj,jk) - pfu(ji,jj,jk) ) / MAX( zc, 1.e-9 ) )    ! phi_REF
            IF( zcoef1 >= 0. ) THEN
               zfho = MAX( pfc(ji,jj,jk), zfho )
               zfho = MIN( zfho, MIN( zcoef3, pfd(ji,jj,jk) ) )
            ELSE
               zfho = MIN( pfc(ji,jj,jk), zfho )
               zfho = MAX( zfho, MAX( zcoef3, pfd(ji,jj,jk) ) )
            ENDIF
         ENDIF
         puc(ji,jj,jk) = zfho
      END_3D
      !
   END SUBROUTINE quickest

   !!======================================================================
END MODULE traadv_qck_lf
