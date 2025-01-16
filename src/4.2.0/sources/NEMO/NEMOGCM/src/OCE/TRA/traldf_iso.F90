MODULE traldf_iso
   !!======================================================================
   !!                   ***  MODULE  traldf_iso  ***
   !! Ocean  tracers:  horizontal component of the lateral tracer mixing trend
   !!======================================================================
   !! History :  OPA  ! 1994-08  (G. Madec, M. Imbard)
   !!            8.0  ! 1997-05  (G. Madec)  split into traldf and trazdf
   !!            NEMO ! 2002-08  (G. Madec)  Free form, F90
   !!            1.0  ! 2005-11  (G. Madec)  merge traldf and trazdf :-)
   !!            3.3  ! 2010-09  (C. Ethe, G. Madec) Merge TRA-TRC
   !!            3.7  ! 2014-01  (G. Madec, S. Masson)  restructuration/simplification of aht/aeiv specification
   !!             -   ! 2014-02  (F. Lemarie, G. Madec)  triad operator (Griffies) + Method of Stabilizing Correction
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_ldf_iso   : update the tracer trend with the horizontal component of a iso-neutral laplacian operator
   !!                   and with the vertical part of the isopycnal or geopotential s-coord. operator
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and active tracers
   USE dom_oce        ! ocean space and time domain
   USE domutl, ONLY : is_tile
   USE trc_oce        ! share passive tracers/Ocean variables
   USE zdf_oce        ! ocean vertical physics
   USE ldftra         ! lateral diffusion: tracer eddy coefficients
   USE ldfslp         ! iso-neutral slopes
   USE diaptr         ! poleward transport diagnostics
   USE diaar5         ! AR5 diagnostics
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O library
   USE phycst         ! physical constants
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_ldf_iso   ! routine called by step.F90

   LOGICAL  ::   l_ptr   ! flag to compute poleward transport
   LOGICAL  ::   l_hst   ! flag to compute heat transport

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: traldf_iso.F90 14834 2021-05-11 09:24:44Z hadcv $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tra_ldf_iso( kt, Kmm, kit000, cdtype, pahu, pahv,             &
      &                                             pgu , pgv , pgui, pgvi, &
      &                                             pt, pt2, pt_rhs, kjpt, kpass )
      !!
      INTEGER                     , INTENT(in   ) ::   kt         ! ocean time-step index
      INTEGER                     , INTENT(in   ) ::   kit000     ! first time step index
      CHARACTER(len=3)            , INTENT(in   ) ::   cdtype     ! =TRA or TRC (tracer indicator)
      INTEGER                     , INTENT(in   ) ::   kjpt       ! number of tracers
      INTEGER                     , INTENT(in   ) ::   kpass      ! =1/2 first or second passage
      INTEGER                     , INTENT(in   ) ::   Kmm        ! ocean time level index
      REAL(wp), DIMENSION(:,:,:)  , INTENT(in   ) ::   pahu, pahv ! eddy diffusivity at u- and v-points  [m2/s]
      REAL(wp), DIMENSION(:,:,:)  , INTENT(in   ) ::   pgu, pgv   ! tracer gradient at pstep levels
      REAL(wp), DIMENSION(:,:,:)  , INTENT(in   ) ::   pgui, pgvi ! tracer gradient at top   levels
      REAL(wp), DIMENSION(:,:,:,:), INTENT(in   ) ::   pt         ! tracer (kpass=1) or laplacian of tracer (kpass=2)
      REAL(wp), DIMENSION(:,:,:,:), INTENT(in   ) ::   pt2        ! tracer (only used in kpass=2)
      REAL(wp), DIMENSION(:,:,:,:), INTENT(inout) ::   pt_rhs     ! tracer trend
      !!
      CALL tra_ldf_iso_t( kt, Kmm, kit000, cdtype, pahu, pahv, is_tile(pahu),                             &
         &                                         pgu , pgv , is_tile(pgu) , pgui, pgvi, is_tile(pgui),  &
         &                                         pt, is_tile(pt), pt2, is_tile(pt2), pt_rhs, is_tile(pt_rhs), kjpt, kpass )
   END SUBROUTINE tra_ldf_iso


  SUBROUTINE tra_ldf_iso_t( kt, Kmm, kit000, cdtype, pahu, pahv, ktah,                    &
      &                                              pgu , pgv , ktg , pgui, pgvi, ktgi,  &
      &                                              pt, ktt, pt2, ktt2, pt_rhs, ktt_rhs, kjpt, kpass )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_ldf_iso  ***
      !!
      !! ** Purpose :   Compute the before horizontal tracer (t & s) diffusive
      !!      trend for a laplacian tensor (ezxcept the dz[ dz[.] ] term) and
      !!      add it to the general trend of tracer equation.
      !!
      !! ** Method  :   The horizontal component of the lateral diffusive trends
      !!      is provided by a 2nd order operator rotated along neural or geopo-
      !!      tential surfaces to which an eddy induced advection can be added
      !!      It is computed using before fields (forward in time) and isopyc-
      !!      nal or geopotential slopes computed in routine ldfslp.
      !!
      !!      1st part :  masked horizontal derivative of T  ( di[ t ] )
      !!      ========    with partial cell update if ln_zps=T
      !!                  with top     cell update if ln_isfcav
      !!
      !!      2nd part :  horizontal fluxes of the lateral mixing operator
      !!      ========
      !!         zftu =  pahu e2u*e3u/e1u di[ tb ]
      !!               - pahu e2u*uslp    dk[ mi(mk(tb)) ]
      !!         zftv =  pahv e1v*e3v/e2v dj[ tb ]
      !!               - pahv e2u*vslp    dk[ mj(mk(tb)) ]
      !!      take the horizontal divergence of the fluxes:
      !!         difft = 1/(e1e2t*e3t) {  di-1[ zftu ] +  dj-1[ zftv ]  }
      !!      Add this trend to the general trend (ta,sa):
      !!         ta = ta + difft
      !!
      !!      3rd part: vertical trends of the lateral mixing operator
      !!      ========  (excluding the vertical flux proportional to dk[t] )
      !!      vertical fluxes associated with the rotated lateral mixing:
      !!         zftw = - {  mi(mk(pahu)) * e2t*wslpi di[ mi(mk(tb)) ]
      !!                   + mj(mk(pahv)) * e1t*wslpj dj[ mj(mk(tb)) ]  }
      !!      take the horizontal divergence of the fluxes:
      !!         difft = 1/(e1e2t*e3t) dk[ zftw ]
      !!      Add this trend to the general trend (ta,sa):
      !!         pt_rhs = pt_rhs + difft
      !!
      !! ** Action :   Update pt_rhs arrays with the before rotated diffusion
      !!----------------------------------------------------------------------
      INTEGER                                   , INTENT(in   ) ::   kt         ! ocean time-step index
      INTEGER                                   , INTENT(in   ) ::   kit000     ! first time step index
      CHARACTER(len=3)                          , INTENT(in   ) ::   cdtype     ! =TRA or TRC (tracer indicator)
      INTEGER                                   , INTENT(in   ) ::   kjpt       ! number of tracers
      INTEGER                                   , INTENT(in   ) ::   kpass      ! =1/2 first or second passage
      INTEGER                                   , INTENT(in   ) ::   Kmm        ! ocean time level index
      INTEGER                                   , INTENT(in   ) ::   ktah, ktg, ktgi, ktt, ktt2, ktt_rhs
      REAL(wp), DIMENSION(A2D_T(ktah)   ,JPK)     , INTENT(in   ) ::   pahu, pahv ! eddy diffusivity at u- and v-points  [m2/s]
      REAL(wp), DIMENSION(A2D_T(ktg)        ,KJPT), INTENT(in   ) ::   pgu, pgv   ! tracer gradient at pstep levels
      REAL(wp), DIMENSION(A2D_T(ktgi)       ,KJPT), INTENT(in   ) ::   pgui, pgvi ! tracer gradient at top   levels
      REAL(wp), DIMENSION(A2D_T(ktt)    ,JPK,KJPT), INTENT(in   ) ::   pt         ! tracer (kpass=1) or laplacian of tracer (kpass=2)
      REAL(wp), DIMENSION(A2D_T(ktt2)   ,JPK,KJPT), INTENT(in   ) ::   pt2        ! tracer (only used in kpass=2)
      REAL(wp), DIMENSION(A2D_T(ktt_rhs),JPK,KJPT), INTENT(inout) ::   pt_rhs     ! tracer trend
      !
      INTEGER  ::  ji, jj, jk, jn   ! dummy loop indices
      INTEGER  ::  ikt
      INTEGER  ::  ierr, iij        ! local integer
      REAL(wp) ::  zmsku, zahu_w, zabe1, zcof1, zcoef3   ! local scalars
      REAL(wp) ::  zmskv, zahv_w, zabe2, zcof2, zcoef4   !   -      -
      REAL(wp) ::  zcoef0, ze3w_2, zsign                 !   -      -
      REAL(wp), DIMENSION(A2D(nn_hls))     ::   zdkt, zdk1t, z2d
      REAL(wp), DIMENSION(A2D(nn_hls),jpk) ::   zdit, zdjt, zftu, zftv, ztfw
      !!----------------------------------------------------------------------
      !
      IF( kpass == 1 .AND. kt == kit000 )  THEN
         IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                       ! Do only on the first tile
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'tra_ldf_iso : rotated laplacian diffusion operator on ', cdtype
            IF(lwp) WRITE(numout,*) '~~~~~~~~~~~'
         ENDIF
         !
         DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 1, jpk )
            akz     (ji,jj,jk) = 0._wp
            ah_wslp2(ji,jj,jk) = 0._wp
         END_3D
      ENDIF
      !
      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                           ! Do only on the first tile
         l_hst = .FALSE.
         l_ptr = .FALSE.
         IF( cdtype == 'TRA' .AND. ( iom_use( 'sophtldf' ) .OR. iom_use( 'sopstldf' ) ) )     l_ptr = .TRUE.
         IF( cdtype == 'TRA' .AND. ( iom_use("uadv_heattr") .OR. iom_use("vadv_heattr") .OR. &
            &                        iom_use("uadv_salttr") .OR. iom_use("vadv_salttr")  ) )   l_hst = .TRUE.
      ENDIF
      !
      ! Define pt_rhs halo points for multi-point haloes in bilaplacian case
      IF( nldf_tra == np_blp_i .AND. kpass == 1 ) THEN ; iij = nn_hls
      ELSE                                             ; iij = 1
      ENDIF

      !
      IF( kpass == 1 ) THEN   ;   zsign =  1._wp      ! bilaplacian operator require a minus sign (eddy diffusivity >0)
      ELSE                    ;   zsign = -1._wp
      ENDIF

      !!----------------------------------------------------------------------
      !!   0 - calculate  ah_wslp2 and akz
      !!----------------------------------------------------------------------
      !
      IF( kpass == 1 ) THEN                  !==  first pass only  ==!
         !
         DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 2, jpkm1 )
            !
            zmsku = wmask(ji,jj,jk) / MAX(   umask(ji  ,jj,jk-1) + umask(ji-1,jj,jk)          &
               &                           + umask(ji-1,jj,jk-1) + umask(ji  ,jj,jk) , 1._wp  )
            zmskv = wmask(ji,jj,jk) / MAX(   vmask(ji,jj  ,jk-1) + vmask(ji,jj-1,jk)          &
               &                           + vmask(ji,jj-1,jk-1) + vmask(ji,jj  ,jk) , 1._wp  )
               !
            ! round brackets added to fix the order of floating point operations
            ! needed to ensure halo 1 - halo 2 compatibility
            zahu_w = ( (  pahu(ji  ,jj,jk-1) + pahu(ji-1,jj,jk)                    &
               &       )                                                           & ! bracket for halo 1 - halo 2 compatibility
               &       + ( pahu(ji-1,jj,jk-1) + pahu(ji  ,jj,jk)                   &
               &         )                                                         & ! bracket for halo 1 - halo 2 compatibility
               &     ) * zmsku
            zahv_w = ( (  pahv(ji,jj  ,jk-1) + pahv(ji,jj-1,jk)                    &
               &       )                                                           & ! bracket for halo 1 - halo 2 compatibility
               &       + ( pahv(ji,jj-1,jk-1) + pahv(ji,jj  ,jk)                   &
               &         )                                                         & ! bracket for halo 1 - halo 2 compatibility
               &     ) * zmskv
               !
            ah_wslp2(ji,jj,jk) = zahu_w * wslpi(ji,jj,jk) * wslpi(ji,jj,jk)   &
               &               + zahv_w * wslpj(ji,jj,jk) * wslpj(ji,jj,jk)
         END_3D
         !
         IF( ln_traldf_msc ) THEN                ! stabilizing vertical diffusivity coefficient
            DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 2, jpkm1 )
               ! round brackets added to fix the order of floating point operations
               ! needed to ensure halo 1 - halo 2 compatibility
               akz(ji,jj,jk) = 0.25_wp * (                                                                     &
                  &            ( ( pahu(ji  ,jj,jk) + pahu(ji  ,jj,jk-1) ) / ( e1u(ji  ,jj) * e1u(ji  ,jj) )   &
                  &            + ( pahu(ji-1,jj,jk) + pahu(ji-1,jj,jk-1) ) / ( e1u(ji-1,jj) * e1u(ji-1,jj) )   &
                  &            )                                                                               & ! bracket for halo 1 - halo 2 compatibility
                  &            + ( ( pahv(ji,jj  ,jk) + pahv(ji,jj  ,jk-1) ) / ( e2v(ji,jj  ) * e2v(ji,jj  ) ) &
                  &              + ( pahv(ji,jj-1,jk) + pahv(ji,jj-1,jk-1) ) / ( e2v(ji,jj-1) * e2v(ji,jj-1) ) &
                  &              )                                                                             & ! bracket for halo 1 - halo 2 compatibility
                  &                      )
            END_3D
            !
            IF( ln_traldf_blp ) THEN                ! bilaplacian operator
               DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 2, jpkm1 )
                  akz(ji,jj,jk) = 16._wp   &
                     &   * ah_wslp2   (ji,jj,jk)   &
                     &   * (  akz     (ji,jj,jk)   &
                     &      + ah_wslp2(ji,jj,jk)   &
                     &        / ( e3w(ji,jj,jk,Kmm) * e3w(ji,jj,jk,Kmm) )  )
               END_3D
            ELSEIF( ln_traldf_lap ) THEN              ! laplacian operator
               DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 2, jpkm1 )
                  ze3w_2 = e3w(ji,jj,jk,Kmm) * e3w(ji,jj,jk,Kmm)
                  zcoef0 = rDt * (  akz(ji,jj,jk) + ah_wslp2(ji,jj,jk) / ze3w_2  )
                  akz(ji,jj,jk) = MAX( zcoef0 - 0.5_wp , 0._wp ) * ze3w_2 * r1_Dt
               END_3D
           ENDIF
           !
         ELSE                                    ! 33 flux set to zero with akz=ah_wslp2 ==>> computed in full implicit
            DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 1, jpk )
               akz(ji,jj,jk) = ah_wslp2(ji,jj,jk)
            END_3D
         ENDIF
      ENDIF
      !
      !                                                          ! ===========
      DO jn = 1, kjpt                                            ! tracer loop
         !                                                       ! ===========
         !
         !!----------------------------------------------------------------------
         !!   I - masked horizontal derivative
         !!----------------------------------------------------------------------
         zdit(:,:,:) = 0._wp
         zdjt(:,:,:) = 0._wp

         ! Horizontal tracer gradient
         DO_3D( iij, iij-1, iij, iij-1, 1, jpkm1 )
            zdit(ji,jj,jk) = ( pt(ji+1,jj  ,jk,jn) - pt(ji,jj,jk,jn) ) * umask(ji,jj,jk)
            zdjt(ji,jj,jk) = ( pt(ji  ,jj+1,jk,jn) - pt(ji,jj,jk,jn) ) * vmask(ji,jj,jk)
         END_3D
         IF( ln_zps ) THEN      ! botton and surface ocean correction of the horizontal gradient
            DO_2D( iij, iij-1, iij, iij-1 )            ! bottom correction (partial bottom cell)
               zdit(ji,jj,mbku(ji,jj)) = pgu(ji,jj,jn)
               zdjt(ji,jj,mbkv(ji,jj)) = pgv(ji,jj,jn)
            END_2D
            IF( ln_isfcav ) THEN      ! first wet level beneath a cavity
               DO_2D( iij, iij-1, iij, iij-1 )
                  IF( miku(ji,jj) > 1 )   zdit(ji,jj,miku(ji,jj)) = pgui(ji,jj,jn)
                  IF( mikv(ji,jj) > 1 )   zdjt(ji,jj,mikv(ji,jj)) = pgvi(ji,jj,jn)
               END_2D
            ENDIF
         ENDIF
         !
         !!----------------------------------------------------------------------
         !!   II - horizontal trend  (full)
         !!----------------------------------------------------------------------
         !
         DO jk = 1, jpkm1                                 ! Horizontal slab
            !
            DO_2D( iij, iij, iij, iij )
               !                             !== Vertical tracer gradient
               zdk1t(ji,jj) = ( pt(ji,jj,jk,jn) - pt(ji,jj,jk+1,jn) ) * wmask(ji,jj,jk+1)     ! level jk+1
               !
               IF( jk == 1 ) THEN   ;   zdkt(ji,jj) = zdk1t(ji,jj)                            ! surface: zdkt(jk=1)=zdkt(jk=2)
               ELSE                 ;   zdkt(ji,jj) = ( pt(ji,jj,jk-1,jn) - pt(ji,jj,jk,jn) ) * wmask(ji,jj,jk)
               ENDIF
            END_2D
            !
            DO_2D( iij, iij-1, iij, iij-1 )           !==  Horizontal fluxes
               zabe1 = pahu(ji,jj,jk) * e2_e1u(ji,jj) * e3u(ji,jj,jk,Kmm)
               zabe2 = pahv(ji,jj,jk) * e1_e2v(ji,jj) * e3v(ji,jj,jk,Kmm)
               !
               zmsku = 1. / MAX(  wmask(ji+1,jj,jk  ) + wmask(ji,jj,jk+1)   &
                  &             + wmask(ji+1,jj,jk+1) + wmask(ji,jj,jk  ), 1. )
               !
               zmskv = 1. / MAX(  wmask(ji,jj+1,jk  ) + wmask(ji,jj,jk+1)   &
                  &             + wmask(ji,jj+1,jk+1) + wmask(ji,jj,jk  ), 1. )
               !
               zcof1 = - pahu(ji,jj,jk) * e2u(ji,jj) * uslp(ji,jj,jk) * zmsku
               zcof2 = - pahv(ji,jj,jk) * e1v(ji,jj) * vslp(ji,jj,jk) * zmskv
               !
               ! round brackets added to fix the order of floating point operations
               ! needed to ensure halo 1 - halo 2 compatibility
               zftu(ji,jj,jk ) = (  zabe1 * zdit(ji,jj,jk)                       &
                  &               + zcof1 * ( ( zdkt (ji+1,jj) + zdk1t(ji,jj)    &
                  &                           )                                  & ! bracket for halo 1 - halo 2 compatibility
                  &                         + ( zdk1t(ji+1,jj) + zdkt (ji,jj)    &
                  &                           )                                  & ! bracket for halo 1 - halo 2 compatibility
                  &                         ) ) * umask(ji,jj,jk)
               zftv(ji,jj,jk) = (  zabe2 * zdjt(ji,jj,jk)                        &
                  &              + zcof2 * ( ( zdkt (ji,jj+1) + zdk1t(ji,jj)     &
                  &                           )                                  & ! bracket for halo 1 - halo 2 compatibility
                  &                         + ( zdk1t(ji,jj+1) + zdkt (ji,jj)    &
                  &                           )                                  & ! bracket for halo 1 - halo 2 compatibility
                  &                         ) ) * vmask(ji,jj,jk)
            END_2D
            !
            DO_2D( iij-1, iij-1, iij-1, iij-1 )           !== horizontal divergence and add to pta
               ! round brackets added to fix the order of floating point operations
               ! needed to ensure halo 1 - halo 2 compatibility
               pt_rhs(ji,jj,jk,jn) = pt_rhs(ji,jj,jk,jn)                         &
                  &       + zsign * ( ( zftu(ji,jj,jk) - zftu(ji-1,jj,jk)        &
                  &                   )                                          & ! bracket for halo 1 - halo 2 compatibility
                  &                 + ( zftv(ji,jj,jk) - zftv(ji,jj-1,jk)        &
                  &                   )                                          & ! bracket for halo 1 - halo 2 compatibility
                  &                 ) * r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kmm)
            END_2D
         END DO                                        !   End of slab

         !!----------------------------------------------------------------------
         !!   III - vertical trend (full)
         !!----------------------------------------------------------------------
         !
         ! Vertical fluxes
         ! ---------------
         !                          ! Surface and bottom vertical fluxes set to zero
         ztfw(:,:, 1 ) = 0._wp      ;      ztfw(:,:,jpk) = 0._wp

         DO_3D( iij-1, iij-1, iij-1, iij-1, 2, jpkm1 )    ! interior (2=<jk=<jpk-1)
            !
            zmsku = wmask(ji,jj,jk) / MAX(   umask(ji  ,jj,jk-1) + umask(ji-1,jj,jk)          &
               &                           + umask(ji-1,jj,jk-1) + umask(ji  ,jj,jk) , 1._wp  )
            zmskv = wmask(ji,jj,jk) / MAX(   vmask(ji,jj  ,jk-1) + vmask(ji,jj-1,jk)          &
               &                           + vmask(ji,jj-1,jk-1) + vmask(ji,jj  ,jk) , 1._wp  )
               !
            zahu_w = (   pahu(ji  ,jj,jk-1) + pahu(ji-1,jj,jk)    &
               &       + pahu(ji-1,jj,jk-1) + pahu(ji  ,jj,jk)  ) * zmsku
            zahv_w = (   pahv(ji,jj  ,jk-1) + pahv(ji,jj-1,jk)    &
               &       + pahv(ji,jj-1,jk-1) + pahv(ji,jj  ,jk)  ) * zmskv
               !
            zcoef3 = - zahu_w * e2t(ji,jj) * zmsku * wslpi (ji,jj,jk)   !wslpi & j are already w-masked
            zcoef4 = - zahv_w * e1t(ji,jj) * zmskv * wslpj (ji,jj,jk)
            !
            ! round brackets added to fix the order of floating point operations
            ! needed to ensure halo 1 - halo 2 compatibility
            ztfw(ji,jj,jk) = zcoef3 * ( ( zdit(ji  ,jj  ,jk-1) + zdit(ji-1,jj  ,jk)    &
                  &                     )                                              & ! bracket for halo 1 - halo 2 compatibility
                  &                   + ( zdit(ji-1,jj  ,jk-1) + zdit(ji  ,jj  ,jk)    &
                  &                     )                                              & ! bracket for halo 1 - halo 2 compatibility
                  &                   )                                                &
                  &        + zcoef4 * ( ( zdjt(ji  ,jj  ,jk-1) + zdjt(ji  ,jj-1,jk)    &
                  &                     )                                              & ! bracket for halo 1 - halo 2 compatibility
                  &                   + ( zdjt(ji  ,jj-1,jk-1) + zdjt(ji  ,jj  ,jk)    &
                  &                     )                                              & ! bracket for halo 1 - halo 2 compatibility
                  &                   )
         END_3D
         !                                !==  add the vertical 33 flux  ==!
         IF( ln_traldf_lap ) THEN               ! laplacian case: eddy coef = ah_wslp2 - akz
            DO_3D( iij-1, iij-1, iij-1, iij-1, 2, jpkm1 )
               ztfw(ji,jj,jk) = ztfw(ji,jj,jk) + e1e2t(ji,jj) / e3w(ji,jj,jk,Kmm) * wmask(ji,jj,jk)   &
                  &                            * ( ah_wslp2(ji,jj,jk) - akz(ji,jj,jk) )               &
                  &                            * (  pt(ji,jj,jk-1,jn) -  pt(ji,jj,jk,jn) )
            END_3D
            !
         ELSE                                   ! bilaplacian
            SELECT CASE( kpass )
            CASE(  1  )                            ! 1st pass : eddy coef = ah_wslp2
               DO_3D( iij-1, iij-1, iij-1, iij-1, 2, jpkm1 )
                  ztfw(ji,jj,jk) =   &
                     &  ztfw(ji,jj,jk) + ah_wslp2(ji,jj,jk) * e1e2t(ji,jj)   &
                     &           * ( pt(ji,jj,jk-1,jn) - pt(ji,jj,jk,jn) ) / e3w(ji,jj,jk,Kmm) * wmask(ji,jj,jk)
               END_3D
            CASE(  2  )                         ! 2nd pass : eddy flux = ah_wslp2 and akz applied on pt  and pt2 gradients, resp.
               DO_3D( 0, 0, 0, 0, 2, jpkm1 )
                  ztfw(ji,jj,jk) = ztfw(ji,jj,jk) + e1e2t(ji,jj) / e3w(ji,jj,jk,Kmm) * wmask(ji,jj,jk)                  &
                     &                            * (  ah_wslp2(ji,jj,jk) * ( pt (ji,jj,jk-1,jn) - pt (ji,jj,jk,jn) )   &
                     &                            +         akz(ji,jj,jk) * ( pt2(ji,jj,jk-1,jn) - pt2(ji,jj,jk,jn) )   )
               END_3D
            END SELECT
         ENDIF
         !
         DO_3D( iij-1, iij-1, iij-1, iij-1, 1, jpkm1 )    !==  Divergence of vertical fluxes added to pta  ==!
            pt_rhs(ji,jj,jk,jn) = pt_rhs(ji,jj,jk,jn) + zsign * (  ztfw (ji,jj,jk) - ztfw(ji,jj,jk+1)  ) * r1_e1e2t(ji,jj)   &
               &                                             / e3t(ji,jj,jk,Kmm)
         END_3D
         !
         IF( ( kpass == 1 .AND. ln_traldf_lap ) .OR.  &     !==  first pass only (  laplacian)  ==!
             ( kpass == 2 .AND. ln_traldf_blp ) ) THEN      !==  2nd   pass      (bilaplacian)  ==!
            !
            !                             ! "Poleward" diffusive heat or salt transports (T-S case only)
               ! note sign is reversed to give down-gradient diffusive transports )
            IF( l_ptr )  CALL dia_ptr_hst( jn, 'ldf', -zftv(:,:,:)  )
            !                          ! Diffusive heat transports
            IF( l_hst )  CALL dia_ar5_hst( jn, 'ldf', -zftu(:,:,:), -zftv(:,:,:) )
            !
         ENDIF                                                    !== end pass selection  ==!
         !
         !                                                        ! ===============
      END DO                                                      ! end tracer loop
      !
   END SUBROUTINE tra_ldf_iso_t

   !!==============================================================================
END MODULE traldf_iso
