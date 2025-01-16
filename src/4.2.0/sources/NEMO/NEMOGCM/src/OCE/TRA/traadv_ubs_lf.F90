MODULE traadv_ubs_lf
   !!==============================================================================
   !!                       ***  MODULE  traadv_ubs  ***
   !! Ocean active tracers:  horizontal & vertical advective trend
   !!==============================================================================
   !! History :  1.0  !  2006-08  (L. Debreu, R. Benshila)  Original code
   !!            3.3  !  2010-05  (C. Ethe, G. Madec)  merge TRC-TRA + switch from velocity to transport
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_adv_ubs : update the tracer trend with the horizontal
   !!                 advection trends using a third order biaised scheme
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and active tracers
   USE dom_oce        ! ocean space and time domain
   USE trc_oce        ! share passive tracers/Ocean variables
   USE trd_oce        ! trends: ocean variables
   USE traadv_fct      ! acces to routine interp_4th_cpt
   USE trdtra         ! trends manager: tracers
   USE diaptr         ! poleward transport diagnostics
   USE diaar5         ! AR5 diagnostics
   !
   USE iom            ! I/O library
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! massively parallel library
   USE lbclnk         ! ocean lateral boundary condition (or mpp link)
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_adv_ubs_lf   ! routine called by traadv module

   LOGICAL :: l_trd   ! flag to compute trends
   LOGICAL :: l_ptr   ! flag to compute poleward transport
   LOGICAL :: l_hst   ! flag to compute heat transport


   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: traadv_ubs.F90 14776 2021-04-30 12:33:41Z mocavero $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tra_adv_ubs_lf( kt, kit000, cdtype, p2dt, pU, pV, pW,          &
      &                    Kbb, Kmm, pt, kjpt, Krhs, kn_ubs_v )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_adv_ubs  ***
      !!
      !! ** Purpose :   Compute the now trend due to the advection of tracers
      !!      and add it to the general trend of passive tracer equations.
      !!
      !! ** Method  :   The 3rd order Upstream Biased Scheme (UBS) is based on an
      !!      upstream-biased parabolic interpolation (Shchepetkin and McWilliams 2005)
      !!      It is only used in the horizontal direction.
      !!      For example the i-component of the advective fluxes are given by :
      !!                !  e2u e3u un ( mi(Tn) - zltu(i  ) )   if un(i) >= 0
      !!          ztu = !  or
      !!                !  e2u e3u un ( mi(Tn) - zltu(i+1) )   if un(i) < 0
      !!      where zltu is the second derivative of the before temperature field:
      !!          zltu = 1/e3t di[ e2u e3u / e1u di[Tb] ]
      !!        This results in a dissipatively dominant (i.e. hyper-diffusive)
      !!      truncation error. The overall performance of the advection scheme
      !!      is similar to that reported in (Farrow and Stevens, 1995).
      !!        For stability reasons, the first term of the fluxes which corresponds
      !!      to a second order centered scheme is evaluated using the now velocity
      !!      (centered in time) while the second term which is the diffusive part
      !!      of the scheme, is evaluated using the before velocity (forward in time).
      !!      Note that UBS is not positive. Do not use it on passive tracers.
      !!                On the vertical, the advection is evaluated using a FCT scheme,
      !!      as the UBS have been found to be too diffusive.
      !!                kn_ubs_v argument controles whether the FCT is based on
      !!      a 2nd order centrered scheme (kn_ubs_v=2) or on a 4th order compact
      !!      scheme (kn_ubs_v=4).
      !!
      !! ** Action : - update pt(:,:,:,:,Krhs)  with the now advective tracer trends
      !!             - send trends to trdtra module for further diagnostcs (l_trdtra=T)
      !!             - poleward advective heat and salt transport (ln_diaptr=T)
      !!
      !! Reference : Shchepetkin, A. F., J. C. McWilliams, 2005, Ocean Modelling, 9, 347-404.
      !!             Farrow, D.E., Stevens, D.P., 1995, J. Phys. Ocean. 25, 1731�1741.
      !!----------------------------------------------------------------------
      INTEGER                                  , INTENT(in   ) ::   kt              ! ocean time-step index
      INTEGER                                  , INTENT(in   ) ::   Kbb, Kmm, Krhs  ! ocean time level indices
      INTEGER                                  , INTENT(in   ) ::   kit000          ! first time step index
      CHARACTER(len=3)                         , INTENT(in   ) ::   cdtype          ! =TRA or TRC (tracer indicator)
      INTEGER                                  , INTENT(in   ) ::   kjpt            ! number of tracers
      INTEGER                                  , INTENT(in   ) ::   kn_ubs_v        ! number of tracers
      REAL(wp)                                 , INTENT(in   ) ::   p2dt            ! tracer time-step
      ! TEMP: [tiling] This can be A2D(nn_hls) if using XIOS (subdomain support)
      REAL(wp), DIMENSION(jpi,jpj,jpk         ), INTENT(in   ) ::   pU, pV, pW      ! 3 ocean volume transport components
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt,jpt), INTENT(inout) ::   pt              ! tracers and RHS of tracer equation
      !
      INTEGER  ::   ji, jj, jk, jn   ! dummy loop indices
      REAL(wp) ::   ztra, zbtr, zcoef, zcoef_ip1, zcoef_jp1        ! local scalars
      REAL(wp) ::   zfp_ui, zfm_ui, zcenut, ztak, zfp_wk, zfm_wk   !   -      -
      REAL(wp) ::   zfp_vj, zfm_vj, zcenvt, zeeu, zeev, z_hdivn    !   -      -
      REAL(wp) ::   zeeu_im1, zeeu_ip1, zeev_jm1, zeev_jp1
      REAL(wp) ::   zztu, zztu_im1, zztu_ip1
      REAL(wp) ::   zztv, zztv_jm1, zztv_jp1
      REAL(wp) ::   zzltu, zzltu_ip1, zzltv, zzltv_jp1
      REAL(wp), DIMENSION(A2D(nn_hls),jpk) ::   ztu, ztv, zltu, zltv, zti, ztw     ! 3D workspace
      !!----------------------------------------------------------------------
      !
      IF( ntile == 0 .OR. ntile == 1 )  THEN                       ! Do only on the first tile
         IF( kt == kit000 )  THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'tra_adv_ubs :  horizontal UBS advection scheme on ', cdtype
            IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~'
         ENDIF
         !
         l_trd = .FALSE.
         l_hst = .FALSE.
         l_ptr = .FALSE.
         IF( ( cdtype == 'TRA' .AND. l_trdtra ) .OR. ( cdtype == 'TRC' .AND. l_trdtrc ) )      l_trd = .TRUE.
         IF(   cdtype == 'TRA' .AND. ( iom_use( 'sophtadv' ) .OR. iom_use( 'sophtadv' ) )  )   l_ptr = .TRUE.
         IF(   cdtype == 'TRA' .AND. ( iom_use("uadv_heattr") .OR. iom_use("vadv_heattr") .OR. &
            &                          iom_use("uadv_salttr") .OR. iom_use("vadv_salttr")  ) ) l_hst = .TRUE.
      ENDIF
      !
      ztw (:,:, 1 ) = 0._wp      ! surface & bottom value : set to zero for all tracers
      zltu(:,:,jpk) = 0._wp   ;   zltv(:,:,jpk) = 0._wp
      ztw (:,:,jpk) = 0._wp   ;   zti (:,:,jpk) = 0._wp
      !                                                          ! ===========
      DO jn = 1, kjpt                                            ! tracer loop
         !                                                       ! ===========
         !                        !==  horizontal laplacian of before tracer ==!
         DO_3D( 1, 0, 1, 0, 1, jpkm1 )                   ! Second derivative (divergence)
            ! First derivative (masked gradient)
            zeeu_im1 = e2_e1u(ji-1,jj  ) * e3u(ji-1,jj  ,jk,Kmm) * umask(ji-1,jj  ,jk)
            zeeu     = e2_e1u(ji  ,jj  ) * e3u(ji  ,jj  ,jk,Kmm) * umask(ji  ,jj  ,jk)
            zeeu_ip1 = e2_e1u(ji+1,jj  ) * e3u(ji+1,jj  ,jk,Kmm) * umask(ji+1,jj  ,jk)
            zeev_jm1 = e1_e2v(ji  ,jj-1) * e3v(ji  ,jj-1,jk,Kmm) * vmask(ji  ,jj-1,jk)
            zeev     = e1_e2v(ji  ,jj  ) * e3v(ji  ,jj  ,jk,Kmm) * vmask(ji  ,jj  ,jk)
            zeev_jp1 = e1_e2v(ji  ,jj+1) * e3v(ji  ,jj+1,jk,Kmm) * vmask(ji  ,jj+1,jk)
            !
            zztu_im1 = zeeu_im1 * ( pt(ji  ,jj,jk,jn,Kbb) - pt(ji-1,jj,jk,jn,Kbb) )
            zztu     = zeeu     * ( pt(ji+1,jj,jk,jn,Kbb) - pt(ji  ,jj,jk,jn,Kbb) )
            zztu_ip1 = zeeu_ip1 * ( pt(ji+2,jj,jk,jn,Kbb) - pt(ji+1,jj,jk,jn,Kbb) )
            !
            zztv_jm1 = zeev_jm1 * ( pt(ji,jj  ,jk,jn,Kbb) - pt(ji,jj-1,jk,jn,Kbb) )
            zztv     = zeev     * ( pt(ji,jj+1,jk,jn,Kbb) - pt(ji,jj  ,jk,jn,Kbb) )
            zztv_jp1 = zeev_jp1 * ( pt(ji,jj+2,jk,jn,Kbb) - pt(ji,jj+1,jk,jn,Kbb) )
            ! Second derivative (divergence)
            zcoef     = 1._wp / ( 6._wp * e3t(ji  ,jj  ,jk,Kmm) )
            zcoef_ip1 = 1._wp / ( 6._wp * e3t(ji+1,jj  ,jk,Kmm) )
            zcoef_jp1 = 1._wp / ( 6._wp * e3t(ji  ,jj+1,jk,Kmm) )
            !
            zzltu     = (  zztu     - zztu_im1  ) * zcoef
            zzltu_ip1 = (  zztu_ip1 - zztu      ) * zcoef_ip1
            zzltv     = (  zztv     - zztv_jm1  ) * zcoef
            zzltv_jp1 = (  zztv_jp1 - zztv      ) * zcoef_jp1
            !
            !                     !==  Horizontal advective fluxes  ==!     (UBS)
            zfp_ui = pU(ji,jj,jk) + ABS( pU(ji,jj,jk) )        ! upstream transport (x2)
            zfm_ui = pU(ji,jj,jk) - ABS( pU(ji,jj,jk) )
            zfp_vj = pV(ji,jj,jk) + ABS( pV(ji,jj,jk) )
            zfm_vj = pV(ji,jj,jk) - ABS( pV(ji,jj,jk) )
            !                                                  ! 2nd order centered advective fluxes (x2)
            zcenut = pU(ji,jj,jk) * ( pt(ji,jj,jk,jn,Kmm) + pt(ji+1,jj  ,jk,jn,Kmm) )
            zcenvt = pV(ji,jj,jk) * ( pt(ji,jj,jk,jn,Kmm) + pt(ji  ,jj+1,jk,jn,Kmm) )
            !                                                  ! UBS advective fluxes
            ztu(ji,jj,jk) = 0.5 * ( zcenut - zfp_ui * zzltu - zfm_ui * zzltu_ip1 )
            ztv(ji,jj,jk) = 0.5 * ( zcenvt - zfp_vj * zzltv - zfm_vj * zzltv_jp1 )
         END_3D
         !
         DO_3D( 0, 0, 0, 0, 1, jpk )
            zltu(ji,jj,jk) = pt(ji,jj,jk,jn,Krhs)      ! store the initial trends before its update
         END_3D
         !
         !                           !==  add the horizontal advective trend  ==!
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            pt(ji,jj,jk,jn,Krhs) = pt(ji,jj,jk,jn,Krhs)                &
               &             - (  ztu(ji,jj,jk) - ztu(ji-1,jj  ,jk)    &
               &                + ztv(ji,jj,jk) - ztv(ji  ,jj-1,jk)  ) &
               &                * r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kmm)
         END_3D
         !
         DO_3D( 0, 0, 0, 0, 1, jpk )
            zltu(ji,jj,jk) = pt(ji,jj,jk,jn,Krhs) - zltu(ji,jj,jk)  ! Horizontal advective trend used in vertical 2nd order FCT case
         END_3D                                                     ! and/or in trend diagnostic (l_trd=T)
         !
         IF( l_trd ) THEN                  ! trend diagnostics
             CALL trd_tra( kt, Kmm, Krhs, cdtype, jn, jptra_xad, ztu, pU, pt(:,:,:,jn,Kmm) )
             CALL trd_tra( kt, Kmm, Krhs, cdtype, jn, jptra_yad, ztv, pV, pt(:,:,:,jn,Kmm) )
         END IF
         !
         !                                ! "Poleward" heat and salt transports (contribution of upstream fluxes)
         IF( l_ptr )  CALL dia_ptr_hst( jn, 'adv', ztv(:,:,:) )
         !                                !  heati/salt transport
         IF( l_hst )  CALL dia_ar5_hst( jn, 'adv', ztu(:,:,:), ztv(:,:,:) )
         !
         !
         !                       !== vertical advective trend  ==!
         !
         SELECT CASE( kn_ubs_v )       ! select the vertical advection scheme
         !
         CASE(  2  )                   ! 2nd order FCT
            !
            IF( l_trd ) THEN
               DO_3D( 0, 0, 0, 0, 1, jpkm1 )
                  zltv(ji,jj,jk) = pt(ji,jj,jk,jn,Krhs)          ! store pt(:,:,:,:,Krhs) if trend diag.
               END_3D
            ENDIF
            !
            !                               !*  upstream advection with initial mass fluxes & intermediate update  ==!
            DO_3D( 1, 1, 1, 1, 2, jpkm1 )
               zfp_wk = pW(ji,jj,jk) + ABS( pW(ji,jj,jk) )
               zfm_wk = pW(ji,jj,jk) - ABS( pW(ji,jj,jk) )
               ztw(ji,jj,jk) = 0.5_wp * (  zfp_wk * pt(ji,jj,jk,jn,Kbb) + zfm_wk * pt(ji,jj,jk-1,jn,Kbb)  ) * wmask(ji,jj,jk)
            END_3D
            IF( ln_linssh ) THEN                ! top ocean value (only in linear free surface as ztw has been w-masked)
               IF( ln_isfcav ) THEN                   ! top of the ice-shelf cavities and at the ocean surface
                  DO_2D( 1, 1, 1, 1 )
                     ztw(ji,jj, mikt(ji,jj) ) = pW(ji,jj,mikt(ji,jj)) * pt(ji,jj,mikt(ji,jj),jn,Kbb)   ! linear free surface
                  END_2D
               ELSE                                   ! no cavities: only at the ocean surface
                  DO_2D( 1, 1, 1, 1 )
                     ztw(ji,jj,1) = pW(ji,jj,1) * pt(ji,jj,1,jn,Kbb)
                  END_2D
               ENDIF
            ENDIF
            !
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )   !* trend and after field with monotonic scheme
               ztak = - ( ztw(ji,jj,jk) - ztw(ji,jj,jk+1) )    &
                  &     * r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kmm)
               pt(ji,jj,jk,jn,Krhs) =   pt(ji,jj,jk,jn,Krhs) +  ztak
               zti(ji,jj,jk)    = ( pt(ji,jj,jk,jn,Kbb) + p2dt * ( ztak + zltu(ji,jj,jk) ) ) * tmask(ji,jj,jk)
            END_3D
            !
            !                          !*  anti-diffusive flux : high order minus low order
            DO_3D( 1, 1, 1, 1, 2, jpkm1 )
               ztw(ji,jj,jk) = (   0.5_wp * pW(ji,jj,jk) * ( pt(ji,jj,jk,jn,Kmm) + pt(ji,jj,jk-1,jn,Kmm) )   &
                  &              - ztw(ji,jj,jk)   ) * wmask(ji,jj,jk)
            END_3D
            !                                            ! top ocean value: high order == upstream  ==>>  zwz=0
            IF( ln_linssh )   ztw(:,:, 1 ) = 0._wp       ! only ocean surface as interior zwz values have been w-masked
            !
            CALL nonosc_z( Kmm, pt(:,:,:,jn,Kbb), ztw, zti, p2dt )      !  monotonicity algorithm
            !
         CASE(  4  )                               ! 4th order COMPACT
            CALL interp_4th_cpt( pt(:,:,:,jn,Kmm) , ztw )         ! 4th order compact interpolation of T at w-point
            DO_3D( 0, 0, 0, 0, 2, jpkm1 )
               ztw(ji,jj,jk) = pW(ji,jj,jk) * ztw(ji,jj,jk) * wmask(ji,jj,jk)
            END_3D
            IF( ln_linssh ) THEN
               DO_2D( 1, 1, 1, 1 )
                  ztw(ji,jj,1) = pW(ji,jj,1) * pt(ji,jj,1,jn,Kmm)     !!gm ISF & 4th COMPACT doesn't work
               END_2D
            ENDIF
            !
         END SELECT
         !
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )   !  final trend with corrected fluxes
            pt(ji,jj,jk,jn,Krhs) = pt(ji,jj,jk,jn,Krhs) - ( ztw(ji,jj,jk) - ztw(ji,jj,jk+1) )    &
               &                                        * r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kmm)
         END_3D
         !
         IF( l_trd )  THEN               ! vertical advective trend diagnostics
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )                 ! (compute -w.dk[ptn]= -dk[w.ptn] + ptn.dk[w])
               zltv(ji,jj,jk) = pt(ji,jj,jk,jn,Krhs) - zltv(ji,jj,jk)                          &
                  &           + pt(ji,jj,jk,jn,Kmm) * (  pW(ji,jj,jk) - pW(ji,jj,jk+1)  )   &
                  &                              * r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kmm)
            END_3D
            CALL trd_tra( kt, Kmm, Krhs, cdtype, jn, jptra_zad, zltv )
         ENDIF
         !
      END DO
      !
   END SUBROUTINE tra_adv_ubs_lf


   SUBROUTINE nonosc_z( Kmm, pbef, pcc, paft, p2dt )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE nonosc_z  ***
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
      INTEGER , INTENT(in   )                         ::   Kmm    ! time level index
      REAL(wp), INTENT(in   )                         ::   p2dt   ! tracer time-step
      REAL(wp),                DIMENSION(jpi,jpj,jpk) ::   pbef   ! before field
      REAL(wp), INTENT(inout), DIMENSION(A2D(nn_hls)    ,jpk) ::   paft   ! after field
      REAL(wp), INTENT(inout), DIMENSION(A2D(nn_hls)    ,jpk) ::   pcc    ! monotonic flux in the k direction
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      INTEGER  ::   ikm1         ! local integer
      REAL(wp) ::   zpos, zneg, zbt, za, zb, zc, zbig, zrtrn   ! local scalars
      REAL(wp), DIMENSION(A2D(nn_hls),jpk) ::   zbetup, zbetdo         ! 3D workspace
      !!----------------------------------------------------------------------
      !
      zbig  = 1.e+38_wp
      zrtrn = 1.e-15_wp
      zbetup(:,:,:) = 0._wp   ;   zbetdo(:,:,:) = 0._wp
      !
      ! Search local extrema
      ! --------------------
      !                    ! large negative value (-zbig) inside land
      DO_3D( 0, 0, 0, 0, 1, jpk )
         pbef(ji,jj,jk) = pbef(ji,jj,jk) * tmask(ji,jj,jk) - zbig * ( 1.e0 - tmask(ji,jj,jk) )
         paft(ji,jj,jk) = paft(ji,jj,jk) * tmask(ji,jj,jk) - zbig * ( 1.e0 - tmask(ji,jj,jk) )
      END_3D
      !
      DO jk = 1, jpkm1     ! search maximum in neighbourhood
         ikm1 = MAX(jk-1,1)
         DO_2D( 0, 0, 0, 0 )
            zbetup(ji,jj,jk) = MAX(  pbef(ji  ,jj  ,jk  ), paft(ji  ,jj  ,jk  ),   &
               &                     pbef(ji  ,jj  ,ikm1), pbef(ji  ,jj  ,jk+1),   &
               &                     paft(ji  ,jj  ,ikm1), paft(ji  ,jj  ,jk+1)  )
         END_2D
      END DO
      !                    ! large positive value (+zbig) inside land
      DO_3D( 0, 0, 0, 0, 1, jpk )
         pbef(ji,jj,jk) = pbef(ji,jj,jk) * tmask(ji,jj,jk) + zbig * ( 1.e0 - tmask(ji,jj,jk) )
         paft(ji,jj,jk) = paft(ji,jj,jk) * tmask(ji,jj,jk) + zbig * ( 1.e0 - tmask(ji,jj,jk) )
      END_3D
      !
      DO jk = 1, jpkm1     ! search minimum in neighbourhood
         ikm1 = MAX(jk-1,1)
         DO_2D( 0, 0, 0, 0 )
            zbetdo(ji,jj,jk) = MIN(  pbef(ji  ,jj  ,jk  ), paft(ji  ,jj  ,jk  ),   &
               &                     pbef(ji  ,jj  ,ikm1), pbef(ji  ,jj  ,jk+1),   &
               &                     paft(ji  ,jj  ,ikm1), paft(ji  ,jj  ,jk+1)  )
         END_2D
      END DO
      !                    ! restore masked values to zero
      DO_3D( 0, 0, 0, 0, 1, jpk )
         pbef(ji,jj,jk) = pbef(ji,jj,jk) * tmask(ji,jj,jk)
         paft(ji,jj,jk) = paft(ji,jj,jk) * tmask(ji,jj,jk)
      END_3D
      !
      ! Positive and negative part of fluxes and beta terms
      ! ---------------------------------------------------
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         ! positive & negative part of the flux
         zpos = MAX( 0., pcc(ji  ,jj  ,jk+1) ) - MIN( 0., pcc(ji  ,jj  ,jk  ) )
         zneg = MAX( 0., pcc(ji  ,jj  ,jk  ) ) - MIN( 0., pcc(ji  ,jj  ,jk+1) )
         ! up & down beta terms
         zbt = e1e2t(ji,jj) * e3t(ji,jj,jk,Kmm) / p2dt
         zbetup(ji,jj,jk) = ( zbetup(ji,jj,jk) - paft(ji,jj,jk) ) / (zpos+zrtrn) * zbt
         zbetdo(ji,jj,jk) = ( paft(ji,jj,jk) - zbetdo(ji,jj,jk) ) / (zneg+zrtrn) * zbt
      END_3D
      !
      ! monotonic flux in the k direction, i.e. pcc
      ! -------------------------------------------
      DO_3D( 0, 0, 0, 0, 2, jpkm1 )
         za = MIN( 1., zbetdo(ji,jj,jk), zbetup(ji,jj,jk-1) )
         zb = MIN( 1., zbetup(ji,jj,jk), zbetdo(ji,jj,jk-1) )
         zc = 0.5 * ( 1.e0 + SIGN( 1.0_wp, pcc(ji,jj,jk) ) )
         pcc(ji,jj,jk) = pcc(ji,jj,jk) * ( zc * za + ( 1.e0 - zc) * zb )
      END_3D
      !
   END SUBROUTINE nonosc_z

   !!======================================================================
END MODULE traadv_ubs_lf
