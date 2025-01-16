MODULE traadv_mus
   !!======================================================================
   !!                       ***  MODULE  traadv_mus  ***
   !! Ocean  tracers:  horizontal & vertical advective trend
   !!======================================================================
   !! History :       !  2000-06  (A.Estublier)  for passive tracers
   !!                 !  2001-08  (E.Durand, G.Madec)  adapted for T & S
   !!   NEMO     1.0  !  2002-06  (G. Madec)  F90: Free form and module
   !!            3.2  !  2010-05  (C. Ethe, G. Madec)  merge TRC-TRA + switch from velocity to transport
   !!            3.4  !  2012-06  (P. Oddo, M. Vichi) include the upstream where needed
   !!            3.7  !  2015-09  (G. Madec) add the ice-shelf cavities boundary condition
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_adv_mus   : update the tracer trend with the horizontal
   !!                   and vertical advection trends using MUSCL scheme
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and active tracers
   USE trc_oce        ! share passive tracers/Ocean variables
   USE dom_oce        ! ocean space and time domain
   USE trd_oce        ! trends: ocean variables
   USE trdtra         ! tracers trends manager
   USE sbcrnf         ! river runoffs
   USE diaptr         ! poleward transport diagnostics
   USE diaar5         ! AR5 diagnostics

   !
   USE iom            ! XIOS library
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! distribued memory computing
   USE lbclnk         ! ocean lateral boundary condition (or mpp link)
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_adv_mus   ! routine called by traadv.F90

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   upsmsk   !: mixed upstream/centered scheme near some straits
   !                                                           !  and in closed seas (orca 2 and 1 configurations)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   xind     !: mixed upstream/centered index

   LOGICAL  ::   l_trd   ! flag to compute trends
   LOGICAL  ::   l_ptr   ! flag to compute poleward transport
   LOGICAL  ::   l_hst   ! flag to compute heat/salt transport

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: traadv_mus.F90 15139 2021-07-23 12:52:21Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tra_adv_mus( kt, kit000, cdtype, p2dt, pU, pV, pW,             &
      &                    Kbb, Kmm, pt, kjpt, Krhs, ld_msc_ups )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE tra_adv_mus  ***
      !!
      !! ** Purpose :   Compute the now trend due to total advection of tracers
      !!              using a MUSCL scheme (Monotone Upstream-centered Scheme for
      !!              Conservation Laws) and add it to the general tracer trend.
      !!
      !! ** Method  : MUSCL scheme plus centered scheme at ocean boundaries
      !!              ld_msc_ups=T :
      !!
      !! ** Action : - update pt(:,:,:,:,Krhs)  with the now advective tracer trends
      !!             - send trends to trdtra module for further diagnostcs (l_trdtra=T)
      !!             - poleward advective heat and salt transport (ln_diaptr=T)
      !!
      !! References : Estubier, A., and M. Levy, Notes Techn. Pole de Modelisation
      !!              IPSL, Sept. 2000 (http://www.lodyc.jussieu.fr/opa)
      !!----------------------------------------------------------------------
      INTEGER                                  , INTENT(in   ) ::   kt              ! ocean time-step index
      INTEGER                                  , INTENT(in   ) ::   Kbb, Kmm, Krhs  ! ocean time level indices
      INTEGER                                  , INTENT(in   ) ::   kit000          ! first time step index
      CHARACTER(len=3)                         , INTENT(in   ) ::   cdtype          ! =TRA or TRC (tracer indicator)
      INTEGER                                  , INTENT(in   ) ::   kjpt            ! number of tracers
      LOGICAL                                  , INTENT(in   ) ::   ld_msc_ups      ! use upstream scheme within muscl
      REAL(wp)                                 , INTENT(in   ) ::   p2dt            ! tracer time-step
      ! TEMP: [tiling] This can be A2D(nn_hls) after all lbc_lnks removed in the nn_hls = 2 case in tra_adv_fct
      REAL(wp), DIMENSION(jpi,jpj,jpk         ), INTENT(in   ) ::   pU, pV, pW      ! 3 ocean volume flux components
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt,jpt), INTENT(inout) ::   pt              ! tracers and RHS of tracer equation
      !
      INTEGER  ::   ji, jj, jk, jn   ! dummy loop indices
      INTEGER  ::   ierr             ! local integer
      REAL(wp) ::   zu, z0u, zzwx, zw , zalpha   ! local scalars
      REAL(wp) ::   zv, z0v, zzwy, z0w           !   -      -
      REAL(wp), DIMENSION(A2D(nn_hls),jpk) ::   zwx, zslpx   ! 3D workspace
      REAL(wp), DIMENSION(A2D(nn_hls),jpk) ::   zwy, zslpy   ! -      -
      !!----------------------------------------------------------------------
      !
      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                       ! Do only on the first tile
         IF( kt == kit000 )  THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'tra_adv : MUSCL advection scheme on ', cdtype
            IF(lwp) WRITE(numout,*) '        : mixed up-stream           ', ld_msc_ups
            IF(lwp) WRITE(numout,*) '~~~~~~~'
            IF(lwp) WRITE(numout,*)
            !
            ! Upstream / MUSCL scheme indicator
            !
            ALLOCATE( xind(jpi,jpj,jpk), STAT=ierr )
            xind(:,:,:) = 1._wp              ! set equal to 1 where up-stream is not needed
            !
            IF( ld_msc_ups ) THEN            ! define the upstream indicator (if asked)
               ALLOCATE( upsmsk(jpi,jpj), STAT=ierr )
               upsmsk(:,:) = 0._wp                             ! not upstream by default
               !
               DO jk = 1, jpkm1
                  xind(:,:,jk) = 1._wp                              &                 ! =>1 where up-stream is not needed
                     &         - MAX ( rnfmsk(:,:) * rnfmsk_z(jk),  &                 ! =>0 near runoff mouths (& closed sea outflows)
                     &                 upsmsk(:,:)                ) * tmask(:,:,jk)   ! =>0 in some user defined area
               END DO
            ENDIF
            !
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
      DO jn = 1, kjpt            !==  loop over the tracers  ==!
         !
         !                          !* Horizontal advective fluxes
         !
         !                                !-- first guess of the slopes
         zwx(:,:,jpk) = 0._wp                   ! bottom values
         zwy(:,:,jpk) = 0._wp
         DO_3D( nn_hls, nn_hls-1, nn_hls, nn_hls-1, 1, jpkm1 )
            zwx(ji,jj,jk) = umask(ji,jj,jk) * ( pt(ji+1,jj,jk,jn,Kbb) - pt(ji,jj,jk,jn,Kbb) )
            zwy(ji,jj,jk) = vmask(ji,jj,jk) * ( pt(ji,jj+1,jk,jn,Kbb) - pt(ji,jj,jk,jn,Kbb) )
         END_3D
         !                                !-- Slopes of tracer
         zslpx(:,:,jpk) = 0._wp                 ! bottom values
         zslpy(:,:,jpk) = 0._wp
         DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 1, jpkm1 )
            zslpx(ji,jj,jk) =                       ( zwx(ji,jj,jk) + zwx(ji-1,jj  ,jk) )   &
               &            * ( 0.25 + SIGN( 0.25_wp, zwx(ji,jj,jk) * zwx(ji-1,jj  ,jk) ) )
            zslpy(ji,jj,jk) =                       ( zwy(ji,jj,jk) + zwy(ji  ,jj-1,jk) )   &
               &            * ( 0.25 + SIGN( 0.25_wp, zwy(ji,jj,jk) * zwy(ji  ,jj-1,jk) ) )
         END_3D
         !
         DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 1, jpkm1 )    !-- Slopes limitation
            zslpx(ji,jj,jk) = SIGN( 1.0_wp, zslpx(ji,jj,jk) ) * MIN(    ABS( zslpx(ji  ,jj,jk) ),   &
               &                                                     2.*ABS( zwx  (ji-1,jj,jk) ),   &
               &                                                     2.*ABS( zwx  (ji  ,jj,jk) ) )
            zslpy(ji,jj,jk) = SIGN( 1.0_wp, zslpy(ji,jj,jk) ) * MIN(    ABS( zslpy(ji,jj  ,jk) ),   &
               &                                                     2.*ABS( zwy  (ji,jj-1,jk) ),   &
               &                                                     2.*ABS( zwy  (ji,jj  ,jk) ) )
         END_3D
         ! NOTE [ comm_cleanup ] : need to change sign to ensure halo 1 - halo 2 compatibility
         IF ( nn_hls==1 ) CALL lbc_lnk( 'traadv_mus', zslpx, 'T', -1.0_wp , zslpy, 'T', -1.0_wp )   ! lateral boundary conditions   (changed sign)
         !
         DO_3D( 1, 0, 1, 0, 1, jpkm1 )    !-- MUSCL horizontal advective fluxes
            ! MUSCL fluxes
            z0u = SIGN( 0.5_wp, pU(ji,jj,jk) )
            zalpha = 0.5 - z0u
            zu  = z0u - 0.5 * pU(ji,jj,jk) * p2dt * r1_e1e2u(ji,jj) / e3u(ji,jj,jk,Kmm)
            zzwx = pt(ji+1,jj,jk,jn,Kbb) + xind(ji,jj,jk) * zu * zslpx(ji+1,jj,jk)
            zzwy = pt(ji  ,jj,jk,jn,Kbb) + xind(ji,jj,jk) * zu * zslpx(ji  ,jj,jk)
            zwx(ji,jj,jk) = pU(ji,jj,jk) * ( zalpha * zzwx + (1.-zalpha) * zzwy )
            !
            z0v = SIGN( 0.5_wp, pV(ji,jj,jk) )
            zalpha = 0.5 - z0v
            zv  = z0v - 0.5 * pV(ji,jj,jk) * p2dt * r1_e1e2v(ji,jj) / e3v(ji,jj,jk,Kmm)
            zzwx = pt(ji,jj+1,jk,jn,Kbb) + xind(ji,jj,jk) * zv * zslpy(ji,jj+1,jk)
            zzwy = pt(ji,jj  ,jk,jn,Kbb) + xind(ji,jj,jk) * zv * zslpy(ji,jj  ,jk)
            zwy(ji,jj,jk) = pV(ji,jj,jk) * ( zalpha * zzwx + (1.-zalpha) * zzwy )
         END_3D
         !
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )    !-- Tracer advective trend
            pt(ji,jj,jk,jn,Krhs) = pt(ji,jj,jk,jn,Krhs) - ( zwx(ji,jj,jk) - zwx(ji-1,jj  ,jk  )       &
            &                                     + zwy(ji,jj,jk) - zwy(ji  ,jj-1,jk  ) )     &
            &                                   * r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kmm)
         END_3D
         !                                ! trend diagnostics
         IF( l_trd )  THEN
            CALL trd_tra( kt, Kmm, Krhs, cdtype, jn, jptra_xad, zwx, pU, pt(:,:,:,jn,Kbb) )
            CALL trd_tra( kt, Kmm, Krhs, cdtype, jn, jptra_yad, zwy, pV, pt(:,:,:,jn,Kbb) )
         END IF
         !                                 ! "Poleward" heat and salt transports
         IF( l_ptr )  CALL dia_ptr_hst( jn, 'adv', zwy(:,:,:) )
         !                                 !  heat transport
         IF( l_hst )  CALL dia_ar5_hst( jn, 'adv', zwx(:,:,:), zwy(:,:,:) )
         !
         !                          !* Vertical advective fluxes
         !
         !                                !-- first guess of the slopes
         zwx(:,:, 1 ) = 0._wp                   ! surface & bottom boundary conditions
         zwx(:,:,jpk) = 0._wp
         DO_3D( 0, 0, 0, 0, 2, jpkm1 )                ! interior values
            zwx(ji,jj,jk) = tmask(ji,jj,jk) * ( pt(ji,jj,jk-1,jn,Kbb) - pt(ji,jj,jk,jn,Kbb) )
         END_3D
         !                                !-- Slopes of tracer
         zslpx(:,:,1) = 0._wp                   ! surface values
         DO_3D( 0, 0, 0, 0, 2, jpkm1 )
            zslpx(ji,jj,jk) =                        ( zwx(ji,jj,jk) + zwx(ji,jj,jk+1) )  &
               &            * (  0.25 + SIGN( 0.25_wp, zwx(ji,jj,jk) * zwx(ji,jj,jk+1) )  )
         END_3D
         DO_3D( 0, 0, 0, 0, 2, jpkm1 )    !-- Slopes limitation
            zslpx(ji,jj,jk) = SIGN( 1.0_wp, zslpx(ji,jj,jk) ) * MIN(    ABS( zslpx(ji,jj,jk  ) ),   &
               &                                                     2.*ABS( zwx  (ji,jj,jk+1) ),   &
               &                                                     2.*ABS( zwx  (ji,jj,jk  ) )  )
         END_3D
         DO_3D( 0, 0, 0, 0, 1, jpk-2 )    !-- vertical advective flux
            z0w = SIGN( 0.5_wp, pW(ji,jj,jk+1) )
            zalpha = 0.5 + z0w
            zw  = z0w - 0.5 * pW(ji,jj,jk+1) * p2dt * r1_e1e2t(ji,jj) / e3w(ji,jj,jk+1,Kmm)
            zzwx = pt(ji,jj,jk+1,jn,Kbb) + xind(ji,jj,jk) * zw * zslpx(ji,jj,jk+1)
            zzwy = pt(ji,jj,jk  ,jn,Kbb) + xind(ji,jj,jk) * zw * zslpx(ji,jj,jk  )
            zwx(ji,jj,jk+1) = pW(ji,jj,jk+1) * ( zalpha * zzwx + (1.-zalpha) * zzwy ) * wmask(ji,jj,jk)
         END_3D
         IF( ln_linssh ) THEN                   ! top values, linear free surface only
            IF( ln_isfcav ) THEN                      ! ice-shelf cavities (top of the ocean)
               DO_2D( 0, 0, 0, 0 )
                  zwx(ji,jj, mikt(ji,jj) ) = pW(ji,jj,mikt(ji,jj)) * pt(ji,jj,mikt(ji,jj),jn,Kbb)
               END_2D
            ELSE                                      ! no cavities: only at the ocean surface
               DO_2D( 0, 0, 0, 0 )
                  zwx(ji,jj,1) = pW(ji,jj,1) * pt(ji,jj,1,jn,Kbb)
               END_2D
            ENDIF
         ENDIF
         !
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )     !-- vertical advective trend
            pt(ji,jj,jk,jn,Krhs) =  pt(ji,jj,jk,jn,Krhs) - ( zwx(ji,jj,jk) - zwx(ji,jj,jk+1) )   &
               &                                      * r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kmm)
         END_3D
         !                                ! send trends for diagnostic
         IF( l_trd )  CALL trd_tra( kt, Kmm, Krhs, cdtype, jn, jptra_zad, zwx, pW, pt(:,:,:,jn,Kbb) )
         !
      END DO                     ! end of tracer loop
      !
   END SUBROUTINE tra_adv_mus

   !!======================================================================
END MODULE traadv_mus
