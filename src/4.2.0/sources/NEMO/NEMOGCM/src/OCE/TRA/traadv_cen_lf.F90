MODULE traadv_cen_lf
   !!======================================================================
   !!                     ***  MODULE  traadv_cen  ***
   !! Ocean  tracers:   advective trend (2nd/4th order centered)
   !!======================================================================
   !! History :  3.7  ! 2014-05  (G. Madec)  original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_adv_cen   : update the tracer trend with the advection trends using a centered or scheme (2nd or 4th order)
   !!                   NB: on the vertical it is actually a 4th order COMPACT scheme which is used
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space and time domain
   USE eosbn2         ! equation of state
   USE traadv_fct     ! acces to routine interp_4th_cpt
   USE trd_oce        ! trends: ocean variables
   USE trdtra         ! trends manager: tracers
   USE diaptr         ! poleward transport diagnostics
   USE diaar5         ! AR5 diagnostics
   !
   USE in_out_manager ! I/O manager
   USE iom            ! IOM library
   USE trc_oce        ! share passive tracers/Ocean variables
   USE lib_mpp        ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_adv_cen_lf   ! called by traadv.F90

   REAL(wp) ::   r1_6 = 1._wp / 6._wp   ! =1/6

   LOGICAL ::   l_trd   ! flag to compute trends
   LOGICAL ::   l_ptr   ! flag to compute poleward transport
   LOGICAL ::   l_hst   ! flag to compute heat/salt transport

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: traadv_cen.F90 14776 2021-04-30 12:33:41Z mocavero $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tra_adv_cen_lf( kt, kit000, cdtype, pU, pV, pW,     &
      &                    Kmm, pt, kjpt, Krhs, kn_cen_h, kn_cen_v )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_adv_cen  ***
      !!
      !! ** Purpose :   Compute the now trend due to the advection of tracers
      !!      and add it to the general trend of passive tracer equations.
      !!
      !! ** Method  :   The advection is evaluated by a 2nd or 4th order scheme
      !!               using now fields (leap-frog scheme).
      !!       kn_cen_h = 2  ==>> 2nd order centered scheme on the horizontal
      !!                = 4  ==>> 4th order    -        -       -      -
      !!       kn_cen_v = 2  ==>> 2nd order centered scheme on the vertical
      !!                = 4  ==>> 4th order COMPACT  scheme     -      -
      !!
      !! ** Action : - update pt(:,:,:,:,Krhs)  with the now advective tracer trends
      !!             - send trends to trdtra module for further diagnostcs (l_trdtra=T)
      !!             - poleward advective heat and salt transport (l_diaptr=T)
      !!----------------------------------------------------------------------
      INTEGER                                  , INTENT(in   ) ::   kt              ! ocean time-step index
      INTEGER                                  , INTENT(in   ) ::   Kmm, Krhs       ! ocean time level indices
      INTEGER                                  , INTENT(in   ) ::   kit000          ! first time step index
      CHARACTER(len=3)                         , INTENT(in   ) ::   cdtype          ! =TRA or TRC (tracer indicator)
      INTEGER                                  , INTENT(in   ) ::   kjpt            ! number of tracers
      INTEGER                                  , INTENT(in   ) ::   kn_cen_h        ! =2/4 (2nd or 4th order scheme)
      INTEGER                                  , INTENT(in   ) ::   kn_cen_v        ! =2/4 (2nd or 4th order scheme)
      ! TEMP: [tiling] This can be A2D(nn_hls) if using XIOS (subdomain support)
      REAL(wp), DIMENSION(jpi,jpj,jpk         ), INTENT(in   ) ::   pU, pV, pW      ! 3 ocean volume flux components
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt,jpt), INTENT(inout) ::   pt              ! tracers and RHS of tracer equation
      !
      INTEGER  ::   ji, jj, jk, jn   ! dummy loop indices
      INTEGER  ::   ierr             ! local integer
      REAL(wp) ::   zC2t_u, zC4t_u   ! local scalars
      REAL(wp) ::   zC2t_v, zC4t_v   !   -      -
      REAL(wp) ::   ztu_im1, ztu_ip1 !   -      -
      REAL(wp) ::   ztv_jm1, ztv_jp1 !   -      -
      REAL(wp), DIMENSION(A2D(nn_hls),jpk) ::   zwx, zwy, zwz, ztw
      !!----------------------------------------------------------------------
      !
      IF( ntile == 0 .OR. ntile == 1 )  THEN                       ! Do only on the first tile
         IF( kt == kit000 )  THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'tra_adv_cen : centered advection scheme on ', cdtype, ' order h/v =', kn_cen_h,'/', kn_cen_v
            IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~ '
         ENDIF
         !                          ! set local switches
         l_trd = .FALSE.
         l_hst = .FALSE.
         l_ptr = .FALSE.
         IF( ( cdtype == 'TRA' .AND. l_trdtra ) .OR. ( cdtype == 'TRC' .AND. l_trdtrc ) )       l_trd = .TRUE.
         IF(   cdtype == 'TRA' .AND. ( iom_use( 'sophtadv' ) .OR. iom_use( 'sophtadv' ) )  )    l_ptr = .TRUE.
         IF(   cdtype == 'TRA' .AND. ( iom_use("uadv_heattr") .OR. iom_use("vadv_heattr") .OR. &
            &                          iom_use("uadv_salttr") .OR. iom_use("vadv_salttr")  ) )  l_hst = .TRUE.
      ENDIF
      !
      !
      zwz(:,:, 1 ) = 0._wp       ! surface & bottom vertical flux set to zero for all tracers
      zwz(:,:,jpk) = 0._wp
      !
      DO jn = 1, kjpt            !==  loop over the tracers  ==!
         !
         SELECT CASE( kn_cen_h )       !--  Horizontal fluxes  --!
         !
         CASE(  2  )                         !* 2nd order centered
            DO_3D( 1, 0, 1, 0, 1, jpkm1 )
               zwx(ji,jj,jk) = 0.5_wp * pU(ji,jj,jk) * ( pt(ji,jj,jk,jn,Kmm) + pt(ji+1,jj  ,jk,jn,Kmm) )
               zwy(ji,jj,jk) = 0.5_wp * pV(ji,jj,jk) * ( pt(ji,jj,jk,jn,Kmm) + pt(ji  ,jj+1,jk,jn,Kmm) )
            END_3D
            !
         CASE(  4  )                         !* 4th order centered
            DO_3D( 1, 0, 1, 0, 1, jpkm1 )           ! Horizontal advective fluxes
               ztu_im1 = ( pt(ji,jj  ,jk,jn,Kmm) - pt(ji-1,jj,jk,jn,Kmm) ) * umask(ji-1,jj,jk)
               ztu_ip1 = ( pt(ji+2,jj  ,jk,jn,Kmm) - pt(ji+1,jj,jk,jn,Kmm) ) * umask(ji+1,jj,jk)
               ztv_jm1 = ( pt(ji,jj,jk,jn,Kmm) - pt(ji,jj-1,jk,jn,Kmm) ) * vmask(ji,jj-1,jk)
               ztv_jp1 = ( pt(ji,jj+2,jk,jn,Kmm) - pt(ji,jj+1,jk,jn,Kmm) ) * vmask(ji,jj+1,jk)
               !
               zC2t_u = pt(ji,jj,jk,jn,Kmm) + pt(ji+1,jj  ,jk,jn,Kmm)   ! C2 interpolation of T at u- & v-points (x2)
               zC2t_v = pt(ji,jj,jk,jn,Kmm) + pt(ji  ,jj+1,jk,jn,Kmm)
               !                                                  ! C4 interpolation of T at u- & v-points (x2)
               zC4t_u =  zC2t_u + r1_6 * ( ztu_im1 - ztu_ip1 )
               zC4t_v =  zC2t_v + r1_6 * ( ztv_jm1 - ztv_jp1 )
               !                                                  ! C4 fluxes
               zwx(ji,jj,jk) =  0.5_wp * pU(ji,jj,jk) * zC4t_u
               zwy(ji,jj,jk) =  0.5_wp * pV(ji,jj,jk) * zC4t_v
            END_3D
            !
         CASE DEFAULT
            CALL ctl_stop( 'traadv_cen: wrong value for nn_cen' )
         END SELECT
         !
         SELECT CASE( kn_cen_v )       !--  Vertical fluxes  --!   (interior)
         !
         CASE(  2  )                         !* 2nd order centered
            DO_3D( 0, 0, 0, 0, 2, jpk )
               zwz(ji,jj,jk) = 0.5 * pW(ji,jj,jk) * ( pt(ji,jj,jk,jn,Kmm) + pt(ji,jj,jk-1,jn,Kmm) ) * wmask(ji,jj,jk)
            END_3D
            !
         CASE(  4  )                         !* 4th order compact
            CALL interp_4th_cpt( pt(:,:,:,jn,Kmm) , ztw )      ! ztw = interpolated value of T at w-point
            DO_3D( 0, 0, 0, 0, 2, jpkm1 )
               zwz(ji,jj,jk) = pW(ji,jj,jk) * ztw(ji,jj,jk) * wmask(ji,jj,jk)
            END_3D
            !
         END SELECT
         !
         IF( ln_linssh ) THEN                !* top value   (linear free surf. only as zwz is multiplied by wmask)
            IF( ln_isfcav ) THEN                  ! ice-shelf cavities (top of the ocean)
               DO_2D( 1, 1, 1, 1 )
                  zwz(ji,jj, mikt(ji,jj) ) = pW(ji,jj,mikt(ji,jj)) * pt(ji,jj,mikt(ji,jj),jn,Kmm)
               END_2D
            ELSE                                   ! no ice-shelf cavities (only ocean surface)
               DO_2D( 1, 1, 1, 1 )
                  zwz(ji,jj,1) = pW(ji,jj,1) * pt(ji,jj,1,jn,Kmm)
               END_2D
            ENDIF
         ENDIF
         !
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )   !--  Divergence of advective fluxes  --!
            pt(ji,jj,jk,jn,Krhs) = pt(ji,jj,jk,jn,Krhs)    &
               &             - (  zwx(ji,jj,jk) - zwx(ji-1,jj  ,jk  )    &
               &                + zwy(ji,jj,jk) - zwy(ji  ,jj-1,jk  )    &
               &                + zwz(ji,jj,jk) - zwz(ji  ,jj  ,jk+1)  ) &
               &                * r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kmm)
         END_3D
         !                               ! trend diagnostics
         IF( l_trd ) THEN
            CALL trd_tra( kt, Kmm, Krhs, cdtype, jn, jptra_xad, zwx, pU, pt(:,:,:,jn,Kmm) )
            CALL trd_tra( kt, Kmm, Krhs, cdtype, jn, jptra_yad, zwy, pV, pt(:,:,:,jn,Kmm) )
            CALL trd_tra( kt, Kmm, Krhs, cdtype, jn, jptra_zad, zwz, pW, pt(:,:,:,jn,Kmm) )
         ENDIF
         !                                 ! "Poleward" heat and salt transports
         IF( l_ptr )   CALL dia_ptr_hst( jn, 'adv', zwy(:,:,:) )
         !                                 !  heat and salt transport
         IF( l_hst )   CALL dia_ar5_hst( jn, 'adv', zwx(:,:,:), zwy(:,:,:) )
         !
      END DO
      !
   END SUBROUTINE tra_adv_cen_lf

   !!======================================================================
END MODULE traadv_cen_lf
