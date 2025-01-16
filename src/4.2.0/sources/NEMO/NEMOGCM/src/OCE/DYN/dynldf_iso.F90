MODULE dynldf_iso
   !!======================================================================
   !!                     ***  MODULE  dynldf_iso  ***
   !! Ocean dynamics:   lateral viscosity trend (rotated laplacian operator)
   !!======================================================================
   !! History :  OPA  !  97-07  (G. Madec)  Original code
   !!  NEMO      1.0  !  2002-08  (G. Madec)  F90: Free form and module
   !!             -   !  2004-08  (C. Talandier) New trends organization
   !!            2.0  !  2005-11  (G. Madec)  s-coordinate: horizontal diffusion
   !!            3.7  !  2014-01  (F. Lemarie, G. Madec)  restructuration/simplification of ahm specification,
   !!                 !                                   add velocity dependent coefficient and optional read in file
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dyn_ldf_iso  : update the momentum trend with the horizontal part
   !!                  of the lateral diffusion using isopycnal or horizon-
   !!                  tal s-coordinate laplacian operator.
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE ldfdyn          ! lateral diffusion: eddy viscosity coef.
   USE ldftra          ! lateral physics: eddy diffusivity
   USE zdf_oce         ! ocean vertical physics
   USE ldfslp          ! iso-neutral slopes 
   !
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! MPP library
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE prtctl          ! Print control
#if defined key_loop_fusion
   USE dynldf_iso_lf, ONLY: dyn_ldf_iso_lf   ! lateral mixing - loop fusion version (dyn_ldf_iso routine )
#endif

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dyn_ldf_iso           ! called by step.F90
   PUBLIC   dyn_ldf_iso_alloc     ! called by nemogcm.F90

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   akzu, akzv   !: vertical component of rotated lateral viscosity

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: dynldf_iso.F90 15094 2021-07-06 16:24:19Z clem $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION dyn_ldf_iso_alloc()
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_ldf_iso_alloc  ***
      !!----------------------------------------------------------------------
      dyn_ldf_iso_alloc = 0
      IF( .NOT. ALLOCATED( akzu ) ) THEN
         ALLOCATE( akzu(jpi,jpj,jpk), akzv(jpi,jpj,jpk), STAT=dyn_ldf_iso_alloc )
            !
         IF( dyn_ldf_iso_alloc /= 0 )   CALL ctl_warn('dyn_ldf_iso_alloc: array allocate failed.')
      ENDIF
   END FUNCTION dyn_ldf_iso_alloc


   SUBROUTINE dyn_ldf_iso( kt, Kbb, Kmm, puu, pvv, Krhs )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE dyn_ldf_iso  ***
      !!                       
      !! ** Purpose :   Compute the before trend of the rotated laplacian
      !!      operator of lateral momentum diffusion except the diagonal
      !!      vertical term that will be computed in dynzdf module. Add it
      !!      to the general trend of momentum equation.
      !!
      !! ** Method :
      !!        The momentum lateral diffusive trend is provided by a 2nd
      !!      order operator rotated along neutral or geopotential surfaces
      !!      (in s-coordinates).
      !!      It is computed using before fields (forward in time) and isopyc-
      !!      nal or geopotential slopes computed in routine ldfslp.
      !!      Here, u and v components are considered as 2 independent scalar
      !!      fields. Therefore, the property of splitting divergent and rota-
      !!      tional part of the flow of the standard, z-coordinate laplacian
      !!      momentum diffusion is lost.
      !!      horizontal fluxes associated with the rotated lateral mixing:
      !!      u-component:
      !!         ziut = ( ahmt + rn_ahm_b ) e2t * e3t / e1t  di[ uu ]
      !!               -  ahmt              e2t * mi-1(uslp) dk[ mi(mk(uu)) ]
      !!         zjuf = ( ahmf + rn_ahm_b ) e1f * e3f / e2f  dj[ uu ]
      !!               -  ahmf              e1f * mi(vslp)   dk[ mj(mk(uu)) ]
      !!      v-component:
      !!         zivf = ( ahmf + rn_ahm_b ) e2t * e3t / e1t  di[ vv ]
      !!               -  ahmf              e2t * mj(uslp)   dk[ mi(mk(vv)) ]
      !!         zjvt = ( ahmt + rn_ahm_b ) e1f * e3f / e2f  dj[ vv ]
      !!               -  ahmt              e1f * mj-1(vslp) dk[ mj(mk(vv)) ]
      !!      take the horizontal divergence of the fluxes:
      !!         diffu = 1/(e1u*e2u*e3u) {  di  [ ziut ] + dj-1[ zjuf ]  }
      !!         diffv = 1/(e1v*e2v*e3v) {  di-1[ zivf ] + dj  [ zjvt ]  }
      !!      Add this trend to the general trend (uu(rhs),vv(rhs)):
      !!         uu(rhs) = uu(rhs) + diffu
      !!      CAUTION: here the isopycnal part is with a coeff. of aht. This
      !!      should be modified for applications others than orca_r2 (!!bug)
      !!
      !! ** Action :
      !!       -(puu(:,:,:,Krhs),pvv(:,:,:,Krhs)) updated with the before geopotential harmonic mixing trend
      !!       -(akzu,akzv) to accompt for the diagonal vertical component
      !!                    of the rotated operator in dynzdf module
      !!----------------------------------------------------------------------
      INTEGER                             , INTENT( in )  ::  kt               ! ocean time-step index
      INTEGER                             , INTENT( in )  ::  Kbb, Kmm, Krhs   ! ocean time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpt), INTENT(inout) ::  puu, pvv         ! ocean velocities and RHS of momentum equation
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zabe1, zmskt, zmkt, zuav, zuwslpi, zuwslpj   ! local scalars
      REAL(wp) ::   zabe2, zmskf, zmkf, zvav, zvwslpi, zvwslpj   !   -      -
      REAL(wp) ::   zcof0, zcof1, zcof2, zcof3, zcof4, zaht_0    !   -      -
      REAL(wp), DIMENSION(A2D(nn_hls))      ::   ziut, zivf, zdku, zdk1u  ! 2D workspace
      REAL(wp), DIMENSION(A2D(nn_hls))      ::   zjuf, zjvt, zdkv, zdk1v  !  -      -
      REAL(wp), DIMENSION(A1Di(nn_hls),jpk) ::   zfuw, zdiu, zdju, zdj1u  !  -      -
      REAL(wp), DIMENSION(A1Di(nn_hls),jpk) ::   zfvw, zdiv, zdjv, zdj1v  !  -      -
      !!----------------------------------------------------------------------
      !
#if defined key_loop_fusion
      CALL dyn_ldf_iso_lf( kt, Kbb, Kmm, puu, pvv, Krhs    )
#else

      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                       ! Do only on the first tile
         IF( kt == nit000 ) THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'dyn_ldf_iso : iso-neutral laplacian diffusive operator or '
            IF(lwp) WRITE(numout,*) '~~~~~~~~~~~   s-coordinate horizontal diffusive operator'
            !                                      ! allocate dyn_ldf_iso arrays
            IF( dyn_ldf_iso_alloc() /= 0 )   CALL ctl_stop('STOP', 'dyn_ldf_iso: failed to allocate arrays')
            !
            DO_2D_OVR( 0, 0, 0, 0 )
               akzu(ji,jj,1)   = 0._wp
               akzu(ji,jj,jpk) = 0._wp
               akzv(ji,jj,1)   = 0._wp
               akzv(ji,jj,jpk) = 0._wp
            END_2D
            !
         ENDIF
      ENDIF

!!gm bug is dyn_ldf_iso called before tra_ldf_iso ....   <<<<<===== TO BE CHECKED
      ! s-coordinate: Iso-level diffusion on momentum but not on tracer
      IF( ln_dynldf_hor .AND. ln_traldf_iso ) THEN
         !
         DO_3D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 1, jpk )      ! set the slopes of iso-level
            uslp (ji,jj,jk) = - ( gdept(ji+1,jj,jk,Kbb) - gdept(ji ,jj ,jk,Kbb) ) * r1_e1u(ji,jj) * umask(ji,jj,jk)
            vslp (ji,jj,jk) = - ( gdept(ji,jj+1,jk,Kbb) - gdept(ji ,jj ,jk,Kbb) ) * r1_e2v(ji,jj) * vmask(ji,jj,jk)
            wslpi(ji,jj,jk) = - ( gdepw(ji+1,jj,jk,Kbb) - gdepw(ji-1,jj,jk,Kbb) ) * r1_e1t(ji,jj) * tmask(ji,jj,jk) * 0.5
            wslpj(ji,jj,jk) = - ( gdepw(ji,jj+1,jk,Kbb) - gdepw(ji,jj-1,jk,Kbb) ) * r1_e2t(ji,jj) * tmask(ji,jj,jk) * 0.5
         END_3D
         ! Lateral boundary conditions on the slopes
         IF (nn_hls == 1) CALL lbc_lnk( 'dynldf_iso', uslp , 'U', -1.0_wp, vslp , 'V', -1.0_wp, wslpi, 'W', -1.0_wp, wslpj, 'W', -1.0_wp )
         !
      ENDIF
         
      zaht_0 = 0.5_wp * rn_Ud * rn_Ld                  ! aht_0 from namtra_ldf = zaht_max
      
      !                                                ! ===============
      DO jk = 1, jpkm1                                 ! Horizontal slab
         !                                             ! ===============

         ! Vertical u- and v-shears at level jk and jk+1
         ! ---------------------------------------------
         ! surface boundary condition: zdku(jk=1)=zdku(jk=2)
         !                             zdkv(jk=1)=zdkv(jk=2)

         DO_2D( 1, 1, 1, 1 )
            zdk1u(ji,jj) = ( puu(ji,jj,jk,Kbb) -puu(ji,jj,jk+1,Kbb) ) * umask(ji,jj,jk+1)
            zdk1v(ji,jj) = ( pvv(ji,jj,jk,Kbb) -pvv(ji,jj,jk+1,Kbb) ) * vmask(ji,jj,jk+1)
         END_2D

         IF( jk == 1 ) THEN
            zdku(:,:) = zdk1u(:,:)
            zdkv(:,:) = zdk1v(:,:)
         ELSE
            DO_2D( 1, 1, 1, 1 )
               zdku(ji,jj) = ( puu(ji,jj,jk-1,Kbb) - puu(ji,jj,jk,Kbb) ) * umask(ji,jj,jk)
               zdkv(ji,jj) = ( pvv(ji,jj,jk-1,Kbb) - pvv(ji,jj,jk,Kbb) ) * vmask(ji,jj,jk)
            END_2D
         ENDIF

         !                               -----f-----
         ! Horizontal fluxes on U             |  
         ! --------------------===        t   u   t
         !                                    |  
         ! i-flux at t-point             -----f-----

         IF( ln_zps ) THEN      ! z-coordinate - partial steps : min(e3u)
            DO_2D( 0, 1, 0, 0 )
               zabe1 = ( ahmt(ji,jj,jk)+rn_ahm_b ) * e2t(ji,jj)   &
                  &    * MIN( e3u(ji  ,jj,jk,Kmm),                &
                  &           e3u(ji-1,jj,jk,Kmm) ) * r1_e1t(ji,jj)

               zmskt = 1._wp / MAX(   umask(ji-1,jj,jk  )+umask(ji,jj,jk+1)     &
                  &                 + umask(ji-1,jj,jk+1)+umask(ji,jj,jk  ) , 1._wp )

               zcof1 = - zaht_0 * e2t(ji,jj) * zmskt * 0.5  * ( uslp(ji-1,jj,jk) + uslp(ji,jj,jk) )

               ziut(ji,jj) = (  zabe1 * ( puu(ji,jj,jk,Kbb) - puu(ji-1,jj,jk,Kbb) )    &
                  &           + zcof1 * ( zdku (ji,jj) + zdk1u(ji-1,jj)      &
                  &                      +zdk1u(ji,jj) + zdku (ji-1,jj) )  ) * tmask(ji,jj,jk)
            END_2D
         ELSE                   ! other coordinate system (zco or sco) : e3t
            DO_2D( 0, 1, 0, 0 )
               zabe1 = ( ahmt(ji,jj,jk)+rn_ahm_b )   &
                  &     * e2t(ji,jj) * e3t(ji,jj,jk,Kmm) * r1_e1t(ji,jj)

               zmskt = 1._wp / MAX(   umask(ji-1,jj,jk  ) + umask(ji,jj,jk+1)     &
                  &                 + umask(ji-1,jj,jk+1) + umask(ji,jj,jk  ) , 1._wp )

               zcof1 = - zaht_0 * e2t(ji,jj) * zmskt * 0.5  * ( uslp(ji-1,jj,jk) + uslp(ji,jj,jk) )

               ziut(ji,jj) = (  zabe1 * ( puu(ji,jj,jk,Kbb) - puu(ji-1,jj,jk,Kbb) )   &
                  &           + zcof1 * ( zdku (ji,jj) + zdk1u(ji-1,jj)     &
                  &                      +zdk1u(ji,jj) + zdku (ji-1,jj) )  ) * tmask(ji,jj,jk)
            END_2D
         ENDIF

         ! j-flux at f-point
         DO_2D( 1, 0, 1, 0 )
            zabe2 = ( ahmf(ji,jj,jk) + rn_ahm_b )   &
               &     * e1f(ji,jj) * e3f(ji,jj,jk) * r1_e2f(ji,jj)

            zmskf = 1._wp / MAX(   umask(ji,jj+1,jk  )+umask(ji,jj,jk+1)     &
               &                 + umask(ji,jj+1,jk+1)+umask(ji,jj,jk  ) , 1._wp )

            zcof2 = - zaht_0 * e1f(ji,jj) * zmskf * 0.5  * ( vslp(ji+1,jj,jk) + vslp(ji,jj,jk) )

            zjuf(ji,jj) = (  zabe2 * ( puu(ji,jj+1,jk,Kbb) - puu(ji,jj,jk,Kbb) )   &
               &           + zcof2 * ( zdku (ji,jj+1) + zdk1u(ji,jj)     &
               &                      +zdk1u(ji,jj+1) + zdku (ji,jj) )  ) * fmask(ji,jj,jk)
         END_2D

         !                                |   t   |
         ! Horizontal fluxes on V         |       |
         ! --------------------===        f---v---f
         !                                |       |
         ! i-flux at f-point              |   t   |

         DO_2D( 1, 0, 0, 0 )
            zabe1 = ( ahmf(ji,jj,jk) + rn_ahm_b )   &
               &     * e2f(ji,jj) * e3f(ji,jj,jk) * r1_e1f(ji,jj)

            zmskf = 1._wp / MAX(  vmask(ji+1,jj,jk  )+vmask(ji,jj,jk+1)     &
               &                + vmask(ji+1,jj,jk+1)+vmask(ji,jj,jk  ) , 1._wp )

            zcof1 = - zaht_0 * e2f(ji,jj) * zmskf * 0.5 * ( uslp(ji,jj+1,jk) + uslp(ji,jj,jk) )

            zivf(ji,jj) = (  zabe1 * ( pvv(ji+1,jj,jk,Kbb) - pvv(ji,jj,jk,Kbb) )    &
               &           + zcof1 * (  zdkv (ji,jj) + zdk1v(ji+1,jj)      &
               &                      + zdk1v(ji,jj) + zdkv (ji+1,jj) )  ) * fmask(ji,jj,jk)
         END_2D

         ! j-flux at t-point
         IF( ln_zps ) THEN      ! z-coordinate - partial steps : min(e3u)
            DO_2D( 1, 0, 0, 1 )
               zabe2 = ( ahmt(ji,jj,jk)+rn_ahm_b ) * e1t(ji,jj)   &
                  &     * MIN( e3v(ji,jj  ,jk,Kmm),                 &
                  &            e3v(ji,jj-1,jk,Kmm) ) * r1_e2t(ji,jj)

               zmskt = 1._wp / MAX(  vmask(ji,jj-1,jk  )+vmask(ji,jj,jk+1)     &
                  &                + vmask(ji,jj-1,jk+1)+vmask(ji,jj,jk  ) , 1._wp )

               zcof2 = - zaht_0 * e1t(ji,jj) * zmskt * 0.5 * ( vslp(ji,jj-1,jk) + vslp(ji,jj,jk) )

               zjvt(ji,jj) = (  zabe2 * ( pvv(ji,jj,jk,Kbb) - pvv(ji,jj-1,jk,Kbb) )    &
                  &           + zcof2 * ( zdkv (ji,jj-1) + zdk1v(ji,jj)      &
                  &                      +zdk1v(ji,jj-1) + zdkv (ji,jj) )  ) * tmask(ji,jj,jk)
            END_2D
         ELSE                   ! other coordinate system (zco or sco) : e3t
            DO_2D( 1, 0, 0, 1 )
               zabe2 = ( ahmt(ji,jj,jk)+rn_ahm_b )   &
                  &     * e1t(ji,jj) * e3t(ji,jj,jk,Kmm) * r1_e2t(ji,jj)

               zmskt = 1./MAX(  vmask(ji,jj-1,jk  )+vmask(ji,jj,jk+1)   &
                  &           + vmask(ji,jj-1,jk+1)+vmask(ji,jj,jk  ), 1. )

               zcof2 = - zaht_0 * e1t(ji,jj) * zmskt * 0.5 * ( vslp(ji,jj-1,jk) + vslp(ji,jj,jk) )

               zjvt(ji,jj) = (  zabe2 * ( pvv(ji,jj,jk,Kbb) - pvv(ji,jj-1,jk,Kbb) )   &
                  &           + zcof2 * ( zdkv (ji,jj-1) + zdk1v(ji,jj)     &
                  &                      +zdk1v(ji,jj-1) + zdkv (ji,jj) )  ) * tmask(ji,jj,jk)
            END_2D
         ENDIF


         ! Second derivative (divergence) and add to the general trend
         ! -----------------------------------------------------------
         DO_2D( 0, 0, 0, 0 )      !!gm Question vectop possible??? !!bug
            puu(ji,jj,jk,Krhs) = puu(ji,jj,jk,Krhs) + (  ziut(ji+1,jj) - ziut(ji,jj  )    &
               &                           + zjuf(ji  ,jj) - zjuf(ji,jj-1)  ) * r1_e1e2u(ji,jj)   &
               &                           / e3u(ji,jj,jk,Kmm)
            pvv(ji,jj,jk,Krhs) = pvv(ji,jj,jk,Krhs) + (  zivf(ji,jj  ) - zivf(ji-1,jj)    &
               &                           + zjvt(ji,jj+1) - zjvt(ji,jj  )  ) * r1_e1e2v(ji,jj)   &
               &                           / e3v(ji,jj,jk,Kmm)
         END_2D
         !                                             ! ===============
      END DO                                           !   End of slab
      !                                                ! ===============

      ! print sum trends (used for debugging)
      IF(sn_cfctl%l_prtctl)   CALL prt_ctl( tab3d_1=puu(:,:,:,Krhs), clinfo1=' ldfh - Ua: ', mask1=umask, &
         &                                  tab3d_2=pvv(:,:,:,Krhs), clinfo2=       ' Va: ', mask2=vmask, clinfo3='dyn' )


      !                                                ! ===============
      DO jj = ntsj, ntej                               !  Vertical slab
         !                                             ! ===============

 
         ! I. vertical trends associated with the lateral mixing
         ! =====================================================
         !  (excluding the vertical flux proportional to dk[t]


         ! I.1 horizontal momentum gradient
         ! --------------------------------

         DO jk = 1, jpk
            DO ji = ntsi, ntei + nn_hls
               ! i-gradient of u at jj
               zdiu (ji,jk) = tmask(ji,jj  ,jk) * ( puu(ji,jj  ,jk,Kbb) - puu(ji-1,jj  ,jk,Kbb) )
               ! j-gradient of u and v at jj
               zdju (ji,jk) = fmask(ji,jj  ,jk) * ( puu(ji,jj+1,jk,Kbb) - puu(ji  ,jj  ,jk,Kbb) )
               zdjv (ji,jk) = tmask(ji,jj  ,jk) * ( pvv(ji,jj  ,jk,Kbb) - pvv(ji  ,jj-1,jk,Kbb) )
               ! j-gradient of u and v at jj+1
               zdj1u(ji,jk) = fmask(ji,jj-1,jk) * ( puu(ji,jj  ,jk,Kbb) - puu(ji  ,jj-1,jk,Kbb) )
               zdj1v(ji,jk) = tmask(ji,jj+1,jk) * ( pvv(ji,jj+1,jk,Kbb) - pvv(ji  ,jj  ,jk,Kbb) )
            END DO
         END DO
         DO jk = 1, jpk
            DO ji = ntsi - nn_hls, ntei
               ! i-gradient of v at jj
               zdiv (ji,jk) = fmask(ji,jj  ,jk) * ( pvv(ji+1,jj,jk,Kbb) - pvv(ji  ,jj  ,jk,Kbb) )
            END DO
         END DO


         ! I.2 Vertical fluxes
         ! -------------------

         ! Surface and bottom vertical fluxes set to zero
         DO ji = ntsi - nn_hls, ntei + nn_hls
            zfuw(ji, 1 ) = 0.e0
            zfvw(ji, 1 ) = 0.e0
            zfuw(ji,jpk) = 0.e0
            zfvw(ji,jpk) = 0.e0
         END DO

         ! interior (2=<jk=<jpk-1) on U field
         DO jk = 2, jpkm1
            DO ji = ntsi, ntei
               zcof0 = 0.5_wp * zaht_0 * umask(ji,jj,jk)
               !
               zuwslpi = zcof0 * ( wslpi(ji+1,jj,jk) + wslpi(ji,jj,jk) )
               zuwslpj = zcof0 * ( wslpj(ji+1,jj,jk) + wslpj(ji,jj,jk) )
               !
               zmkt = 1./MAX(  tmask(ji,jj,jk-1)+tmask(ji+1,jj,jk-1)      &
                             + tmask(ji,jj,jk  )+tmask(ji+1,jj,jk  ) , 1. )
               zmkf = 1./MAX(  fmask(ji,jj-1,jk-1) + fmask(ji,jj,jk-1)      &
                             + fmask(ji,jj-1,jk  ) + fmask(ji,jj,jk  ) , 1. )

               zcof3 = - e2u(ji,jj) * zmkt * zuwslpi
               zcof4 = - e1u(ji,jj) * zmkf * zuwslpj
               ! vertical flux on u field
               zfuw(ji,jk) = zcof3 * (  zdiu (ji,jk-1) + zdiu (ji+1,jk-1)      &
                  &                   + zdiu (ji,jk  ) + zdiu (ji+1,jk  )  )   &
                  &        + zcof4 * (  zdj1u(ji,jk-1) + zdju (ji  ,jk-1)      &
                  &                   + zdj1u(ji,jk  ) + zdju (ji  ,jk  )  )
               ! vertical mixing coefficient (akzu)
               ! Note: zcof0 include zaht_0, so divided by zaht_0 to obtain slp^2 * zaht_0
               akzu(ji,jj,jk) = ( zuwslpi * zuwslpi + zuwslpj * zuwslpj ) / zaht_0
            END DO
         END DO

         ! interior (2=<jk=<jpk-1) on V field
         DO jk = 2, jpkm1
            DO ji = ntsi, ntei
               zcof0 = 0.5_wp * zaht_0 * vmask(ji,jj,jk)
               !
               zvwslpi = zcof0 * ( wslpi(ji,jj+1,jk) + wslpi(ji,jj,jk) )
               zvwslpj = zcof0 * ( wslpj(ji,jj+1,jk) + wslpj(ji,jj,jk) )
               !
               zmkf = 1./MAX(  fmask(ji-1,jj,jk-1)+fmask(ji,jj,jk-1)      &
                  &          + fmask(ji-1,jj,jk  )+fmask(ji,jj,jk  ) , 1. )
               zmkt = 1./MAX(  tmask(ji,jj,jk-1)+tmask(ji,jj+1,jk-1)      &
                  &          + tmask(ji,jj,jk  )+tmask(ji,jj+1,jk  ) , 1. )

               zcof3 = - e2v(ji,jj) * zmkf * zvwslpi
               zcof4 = - e1v(ji,jj) * zmkt * zvwslpj
               ! vertical flux on v field
               zfvw(ji,jk) = zcof3 * (  zdiv (ji,jk-1) + zdiv (ji-1,jk-1)      &
                  &                   + zdiv (ji,jk  ) + zdiv (ji-1,jk  )  )   &
                  &        + zcof4 * (  zdjv (ji,jk-1) + zdj1v(ji  ,jk-1)      &
                  &                   + zdjv (ji,jk  ) + zdj1v(ji  ,jk  )  )
               ! vertical mixing coefficient (akzv)
               ! Note: zcof0 include zaht_0, so divided by zaht_0 to obtain slp^2 * zaht_0
               akzv(ji,jj,jk) = ( zvwslpi * zvwslpi + zvwslpj * zvwslpj ) / zaht_0
            END DO
         END DO


         ! I.3 Divergence of vertical fluxes added to the general tracer trend
         ! -------------------------------------------------------------------
         DO jk = 1, jpkm1
            DO ji = ntsi, ntei
               puu(ji,jj,jk,Krhs) = puu(ji,jj,jk,Krhs) + ( zfuw(ji,jk) - zfuw(ji,jk+1) ) * r1_e1e2u(ji,jj)   &
                  &               / e3u(ji,jj,jk,Kmm)
               pvv(ji,jj,jk,Krhs) = pvv(ji,jj,jk,Krhs) + ( zfvw(ji,jk) - zfvw(ji,jk+1) ) * r1_e1e2v(ji,jj)   &
                  &               / e3v(ji,jj,jk,Kmm)
            END DO
         END DO
         !                                             ! ===============
      END DO                                           !   End of slab
      !                                                ! ===============
#endif
   END SUBROUTINE dyn_ldf_iso

   !!======================================================================
END MODULE dynldf_iso
