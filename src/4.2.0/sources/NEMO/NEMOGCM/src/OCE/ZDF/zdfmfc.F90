 MODULE zdfmfc
   !!======================================================================
   !!                       ***  MODULE  zdfmfc  ***
   !! Ocean physics: Mass-Flux scheme parameterization of Convection:
   !!                Non-local transport for the convective ocean boundary
   !!                layer. Subgrid-scale large eddies are represented by a
   !!                mass-flux contribution (ln_zdfmfc = .TRUE.)
   !!======================================================================
   !! History : NEMO  !
   !!            3.6  !  2016-06  (H. Giordani, R. Bourdallé-Badie)  Original code
   !!            4.2  !  2020-12  (H. Giordani, R. Bourdallé-Badie)  adapt to NEM04.2
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   tra_mfc       : Compute the Mass Flux and trends of T/S
   !!   diag_mfc      : Modify diagonal of trazdf Matrix
   !!   rhs_mfc       : Modify RHS of trazdf Matrix
   !!   zdf_mfc_init  : initialization, namelist read, and parameters control
   !!----------------------------------------------------------------------
   !
   USE oce            ! ocean dynamics and active tracers
   USE dom_oce        ! ocean space and time domain
   USE domvvl         ! ocean space and time domain : variable volume layer
   USE domzgr
   USE zdf_oce        ! ocean vertical physics
   USE sbc_oce        ! surface boundary condition: ocean
   USE phycst         ! physical constants
   USE eosbn2         ! equation of state (eos routine)
   USE zdfmxl         ! mixed layer
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp        ! MPP manager
   USE prtctl         ! Print control
   USE in_out_manager ! I/O manager
   USE iom            ! I/O manager library
   USE timing         ! Timing
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined) 

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_mfc         ! routine called in step module
   PUBLIC   diag_mfc        ! routine called in trazdf module
   PUBLIC   rhs_mfc         ! routine called in trazdf module
   PUBLIC   zdf_mfc_init    ! routine called in nemo module
   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  edmfa, edmfb, edmfc   !: diagonal term of the matrix.
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::  edmftra               !: y term for matrix inversion
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  edmfm               !: y term for matrix inversion
   !
   !! ** Namelist  namzdf_edmf  **
   REAL(wp) ::   rn_cemf           ! entrain of T/S
   REAL(wp) ::   rn_cwmf           ! detrain of T/S
   REAL(wp) ::   rn_cent           ! entrain of the convective mass flux
   REAL(wp) ::   rn_cdet           ! detrain of the convective mass flux
   REAL(wp) ::   rn_cap            ! Factor of computation for convective area (negative => area constant)
   REAL(wp) ::   App_max           ! Maximum of the convective area
   LOGICAL, PUBLIC, SAVE  ::   ln_edmfuv         !: EDMF flag for velocity  !
   !
   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.2 , NEMO Consortium (2018)
   !! $Id: zdfmfc.F90 13783 2020-20-02 15:30:22Z rbourdal $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION zdf_mfc_alloc()
      !!----------------------------------------------------------------------
      !!                ***  FUNCTION zdf_edmf_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( edmfa(jpi,jpj,jpk) , edmfb(jpi,jpj,jpk) , edmfc(jpi,jpj,jpk)      &
         &      , edmftra(jpi,jpj,jpk,2), edmfm(jpi,jpj,jpk) ,  STAT= zdf_mfc_alloc )
         !
      IF( lk_mpp             )   CALL mpp_sum ( 'zdfmfc', zdf_mfc_alloc )
      IF( zdf_mfc_alloc /= 0 )   CALL ctl_warn('zdf_mfc_alloc: failed to allocate arrays')
   END FUNCTION zdf_mfc_alloc


   SUBROUTINE tra_mfc( kt, Kmm, pts, Krhs )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE zdf_mfc  ***
      !!
      !! ** Purpose :      Compute a mass flux, depending on surface flux, over
      !!            the instable part of the water column.
      !!
      !! ** Method  :     Compute surface instability and mix tracers until stable level
      !!           
      !!
      !! ** Action  :      Compute convection plume and (ta,sa)-trends for trazdf (EDMF scheme)
      !!
      !! References :      
      !!                   Giordani, Bourdallé-Badie and Madec JAMES 2020
      !!----------------------------------------------------------------------
      !!----------------------------------------------------------------------
      INTEGER                                  , INTENT(in)    :: Kmm, Krhs ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts,jpt), INTENT(inout) :: pts       ! active tracers and RHS of tracer equation
      REAL(wp), DIMENSION(A2D(nn_hls),jpk,2) ::   ztsp         ! T/S of the plume
      REAL(wp), DIMENSION(A2D(nn_hls),jpk,2) ::   ztse         ! T/S at W point
      REAL(wp), DIMENSION(A2D(nn_hls),jpk) :: zrwp          !
      REAL(wp), DIMENSION(A2D(nn_hls),jpk) :: zrwp2         !
      REAL(wp), DIMENSION(A2D(nn_hls),jpk) :: zapp          !
      REAL(wp), DIMENSION(A2D(nn_hls),jpk) :: zedmf         !
      REAL(wp), DIMENSION(A2D(nn_hls),jpk) :: zepsT, zepsW  !
      !
      REAL(wp), DIMENSION(A2D(nn_hls)) :: zustar, zustar2   !
      REAL(wp), DIMENSION(A2D(nn_hls)) :: zuws, zvws, zsws, zfnet          !
      REAL(wp), DIMENSION(A2D(nn_hls)) :: zfbuo, zrautbm1, zrautb, zraupl
      REAL(wp), DIMENSION(A2D(nn_hls)) :: zwpsurf            !
      REAL(wp), DIMENSION(A2D(nn_hls)) :: zop0 , zsp0 !
      REAL(wp), DIMENSION(A2D(nn_hls)) :: zrwp_0, zrwp2_0  !
      REAL(wp), DIMENSION(A2D(nn_hls)) :: zapp0           !
      REAL(wp), DIMENSION(A2D(nn_hls)) :: zphp, zph, zphpm1, zphm1, zNHydro
      REAL(wp), DIMENSION(A2D(nn_hls)) :: zhcmo          !
      !
      REAL(wp), DIMENSION(A2D(nn_hls),jpk)   ::   zn2    ! N^2
      REAL(wp), DIMENSION(A2D(nn_hls),2  ) ::   zab, zabm1, zabp ! alpha and beta
     
      REAL(wp), PARAMETER :: zepsilon = 1.e-30                 ! local small value

      REAL(wp) :: zrho, zrhop
      REAL(wp) :: zcnh, znum, zden, zcoef1, zcoef2
      REAL(wp) :: zca, zcb, zcd, zrw, zxl, zcdet, zctre
      REAL(wp) :: zaw, zbw, zxw
      REAL(wp) :: alpha
     !
      INTEGER, INTENT(in   )    ::   kt   ! ocean time-step index      !
      !
      INTEGER  ::   ji, jj, jk  ! dummy  loop arguments   
      !
      !------------------------------------------------------------------
      ! Initialisation of coefficients
      !------------------------------------------------------------------
      zca          = 1._wp
      zcb          = 1._wp
      zcd          = 1._wp

      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         !------------------------------------------------------------------
         ! Surface boundary condition
         !------------------------------------------------------------------
         ! surface Stress
         !--------------------
         zuws(ji,jj) = utau(ji,jj) * r1_rho0
         zvws(ji,jj) = vtau(ji,jj) * r1_rho0
         zustar2(ji,jj) = SQRT(zuws(ji,jj)*zuws(ji,jj)+zvws(ji,jj)*zvws(ji,jj))
         zustar(ji,jj)  = SQRT(zustar2(ji,jj))

         ! Heat Flux
         !--------------------
         zfnet(ji,jj) = qns(ji,jj) + qsr(ji,jj)
         zfnet(ji,jj) = zfnet(ji,jj) / (rho0 * rcp)

         ! Water Flux
         !---------------------
         zsws(ji,jj) = emp(ji,jj)

         !-------------------------------------------
         ! Initialisation of prognostic variables
         !-------------------------------------------
         zrwp (ji,jj,:) =  0._wp ; zrwp2(ji,jj,:) =  0._wp ; zedmf(ji,jj,:) =  0._wp
         zph  (ji,jj)   =  0._wp ; zphm1(ji,jj)   =  0._wp ; zphpm1(ji,jj)  =  0._wp
         ztsp(ji,jj,:,:)=  0._wp

         ! Tracers inside plume (ztsp) and environment (ztse)
         ztsp(ji,jj,1,jp_tem) = pts(ji,jj,1,jp_tem,Kmm) * tmask(ji,jj,1)
         ztsp(ji,jj,1,jp_sal) = pts(ji,jj,1,jp_sal,Kmm) * tmask(ji,jj,1)
         ztse(ji,jj,1,jp_tem) = pts(ji,jj,1,jp_tem,Kmm) * tmask(ji,jj,1)
         ztse(ji,jj,1,jp_sal) = pts(ji,jj,1,jp_sal,Kmm) * tmask(ji,jj,1)
      END_2D

      CALL eos( ztse(:,:,1,:) ,  zrautb(:,:) )
      CALL eos( ztsp(:,:,1,:) ,  zraupl(:,:) )

      !-------------------------------------------
      ! Boundary Condition of Mass Flux (plume velo.; convective area, entrain/detrain)
      !-------------------------------------------
      zhcmo(:,:) = e3t(A1Di(nn_hls),A1Dj(nn_hls),1,Kmm)
      zfbuo(:,:)   = 0._wp
      WHERE ( ABS(zrautb(:,:)) > 1.e-20 ) zfbuo(:,:)   =   &
         &      grav * ( 2.e-4_wp *zfnet(:,:)              &
         &      - 7.6E-4_wp*pts(A2D(nn_hls),1,jp_sal,Kmm)  &
         &      * zsws(:,:)/zrautb(:,:)) * zhcmo(:,:)

      zedmf(:,:,1) = -0.065_wp*(ABS(zfbuo(:,:)))**(1._wp/3._wp)*SIGN(1.,zfbuo(:,:))
      zedmf(:,:,1) = MAX(0., zedmf(:,:,1))

      zwpsurf(:,:) = 2._wp/3._wp*zustar(:,:) + 2._wp/3._wp*ABS(zfbuo(:,:))**(1._wp/3._wp)
      zwpsurf(:,:) = MAX(1.e-5_wp,zwpsurf(:,:))
      zwpsurf(:,:) = MIN(1.,zwpsurf(:,:))

      zapp(:,:,:)  = App_max
      WHERE(zwpsurf .NE. 0.) zapp(:,:,1)   = MIN(MAX(0.,zedmf(:,:,1)/zwpsurf(:,:)), App_max)

      zedmf(:,:,1) = 0._wp 
      zrwp (:,:,1) = 0._wp 
      zrwp2(:,:,1) = 0._wp
      zepsT(:,:,:) = 0.001_wp
      zepsW(:,:,:) = 0.001_wp


      !--------------------------------------------------------------
      ! Compute plume properties 
      ! In the same loop on vert. levels computation of:
      !    - Vertical velocity: zWp
      !    - Convective Area: zAp
      !    - Tracers properties inside the plume (if necessary): ztp
      !---------------------------------------------------------------

      DO jk= 2, jpk

         ! Compute the buoyancy acceleration on T-points at jk-1
         zrautbm1(:,:) = zrautb(:,:)
         CALL eos( pts (:,:,jk  ,:,Kmm) ,  zrautb(:,:)   )
         CALL eos( ztsp(:,:,jk-1,:    ) ,  zraupl(:,:)   )

         DO_2D( 0, 0, 0, 0 )
            zphm1(ji,jj)  = zphm1(ji,jj)  + grav * zrautbm1(ji,jj) * e3t(ji,jj,jk-1, Kmm)
            zphpm1(ji,jj) = zphpm1(ji,jj) + grav * zraupl(ji,jj)   * e3t(ji,jj,jk-1, Kmm)
            zph(ji,jj)    = zphm1(ji,jj)  + grav * zrautb(ji,jj)   * e3t(ji,jj,jk  , Kmm)
            zph(ji,jj)    = MAX( zph(ji,jj), zepsilon)
         END_2D

         WHERE(zrautbm1 .NE. 0.) zfbuo(:,:)  =  grav * (zraupl(:,:) - zrautbm1(:,:)) / zrautbm1(:,:)

         DO_2D( 0, 0, 0, 0 )

            ! Compute Environment of Plume. Interpolation T/S (before time step) on W-points
            zrw              =  (gdept(ji,jj,jk,Kmm) - gdepw(ji,jj,jk,Kmm)) &
               &              / (gdept(ji,jj,jk,Kmm) - gdept(ji,jj,jk-1,Kmm))
            ztse(ji,jj,jk,:) = (pts(ji,jj,jk,:,Kmm) * zrw + pts(ji,jj,jk-1,:,Kmm)*(1._wp - zrw) )*tmask(ji,jj,jk)

            !---------------------------------------------------------------
            ! Compute the vertical velocity on W-points
            !---------------------------------------------------------------

            ! Non-hydrostatic pressure terms in the wp2 equation
            zcnh = 0.2_wp 
            znum = 0.5_wp  + zcnh - &
                   (zcnh*grav*zraupl(ji,jj)/zph(ji,jj)+zcb*zepsW(ji,jj,jk-1)) &
                   *e3t(ji,jj,jk-1,Kmm)*0.5_wp   
            zden = 0.5_wp + zcnh + &
                   (zcnh*grav*zraupl(ji,jj)/zph(ji,jj)+zcb*zepsW(ji,jj,jk-1)) &
                   *e3t(ji,jj,jk-1,Kmm)*0.5_wp   

            zcoef1 = zca*e3t(ji,jj,jk-1,Kmm) / zden
            zcoef2 = znum/zden

            ! compute wp2 
            zrwp2(ji,jj,jk) = zcoef1*zfbuo(ji,jj) &
                            + zcoef2*zrwp2(ji,jj,jk-1)
            zrwp2(ji,jj,jk) = MAX ( zrwp2(ji,jj,jk)*wmask(ji,jj,jk) , 0.)
            zrwp (ji,jj,jk) = SQRT( zrwp2(ji,jj,jk) )

            !----------------------------------------------------------------------------------
            ! Compute convective area on W-point
            ! Compute vertical profil of the convective area with mass conservation hypothesis
            ! If rn_cap negative => constant value on the water column.
            !----------------------------------------------------------------------------------
            IF( rn_cap .GT. 0. ) THEN

               zxw = MAX(zrwp(ji,jj,jk-1), zrwp(ji,jj,jk) )
               IF( zxw > 0. ) THEN

                  zxl = (zrwp(ji,jj,jk-1)-zrwp(ji,jj,jk))/(e3t(ji,jj,jk-1,Kmm)*zxw)
                  IF (zxl .LT. 0._wp) THEN
                     zctre  = -1.*rn_cap*zxl 
                     zcdet  =  0._wp
                  ELSE
                     zctre  =  0._wp
                     zcdet  =  rn_cap*zxl 
                  END IF
                     zapp(ji,jj,jk) = zapp(ji,jj,jk-1)*     &
                     &                (1._wp + (zxl + zctre - zcdet )*e3t(ji,jj,jk-1,Kmm))
                  ELSE
                     zapp(ji,jj,jk) = App_max
                  END IF
                  zapp(ji,jj,jk) = MIN( MAX(zapp(ji,jj,jk),0.), App_max)
               ELSE
                  zapp(ji,jj,jk) = -1. * rn_cap
               END IF

            ! Compute Mass Flux on W-point
            zedmf(ji,jj,jk)   = -zapp(ji,jj,jk) * zrwp(ji,jj,jk)* wmask(ji,jj,jk)

            ! Compute Entrainment coefficient
            IF(rn_cemf .GT. 0.) THEN
               zxw = 0.5_wp*(zrwp(ji,jj,jk-1)+ zrwp(ji,jj,jk) )
               zepsT(ji,jj,jk)  =  0.01_wp
               IF( zxw > 0.  ) THEN
                  zepsT(ji,jj,jk)  =  zepsT(ji,jj,jk) +                       &
                                   &  ABS( zrwp(ji,jj,jk-1)-zrwp(ji,jj,jk) )  &
                                   &  / ( e3t(ji,jj,jk-1,Kmm) * zxw )
                  zepsT(ji,jj,jk)  = zepsT(ji,jj,jk) * rn_cemf * wmask(ji,jj,jk)
               ENDIF
            ELSE
               zepsT(ji,jj,jk)  = -rn_cemf
            ENDIF

            ! Compute the detrend coef for velocity (on W-point and not T-points, bug ???)
            IF(rn_cwmf .GT. 0.) THEN
               zepsW(ji,jj,jk)  =  rn_cwmf * zepsT(ji,jj,jk)
            ELSE
               zepsW(ji,jj,jk)  = -rn_cwmf
            ENDIF

            !---------------------------------------------------------------
            ! Compute the plume properties on T-points
            !---------------------------------------------------------------
            IF(zrwp (ji,jj,jk) .LT. 1.e-12_wp .AND. zrwp (ji,jj,jk-1) .LT. 1.e-12_wp) THEN
               ztsp(ji,jj,jk-1,jp_tem) = pts(ji,jj,jk-1,jp_tem,Kmm)
               ztsp(ji,jj,jk-1,jp_sal) = pts(ji,jj,jk-1,jp_sal,Kmm)
            ENDIF

            zcoef1 =  (1._wp-zepsT(ji,jj,jk)*(1._wp-zrw)*e3w(ji,jj,jk,Kmm)*wmask(ji,jj,jk ) ) &
            &       / (1._wp+zepsT(ji,jj,jk)*zrw*e3w(ji,jj,jk,Kmm)*wmask(ji,jj,jk) )
            !
            zcoef2 =  zepsT(ji,jj,jk)*e3w(ji,jj,jk,Kmm)*wmask(ji,jj,jk)                       &
            &       / (1._wp+zepsT(ji,jj,jk)*zrw*e3w(ji,jj,jk,Kmm)*wmask(ji,jj,jk))
            !
            ztsp(ji,jj,jk,jp_tem) = (zcoef1 * ztsp(ji,jj,jk-1,jp_tem) +  &
            &                        zcoef2 * ztse(ji,jj,jk  ,jp_tem) )*tmask(ji,jj,jk)
            ztsp(ji,jj,jk,jp_sal) = (zcoef1 * ztsp(ji,jj,jk-1,jp_sal) +  &
            &                        zcoef2 * ztse(ji,jj,jk  ,jp_sal) )*tmask(ji,jj,jk)

         END_2D 
      END DO ! end of loop on jpk

      ! Compute Mass Flux on T-point
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         edmfm(ji,jj,jk) = (zedmf(ji,jj,jk+1)  + zedmf(ji,jj,jk) )*0.5_wp
      END_3D
      DO_2D( 0, 0, 0, 0 )
         edmfm(ji,jj,jpk) = zedmf(ji,jj,jpk)
      END_2D

      ! Save variable (on T point)
      CALL iom_put( "mf_Tp" , ztsp(:,:,:,jp_tem) )  ! Save plume temperature
      CALL iom_put( "mf_Sp" , ztsp(:,:,:,jp_sal) )  ! Save plume salinity
      CALL iom_put( "mf_mf" , edmfm(:,:,:)       )  ! Save Mass Flux
      ! Save variable (on W point)
      CALL iom_put( "mf_wp" , zrwp (:,:,:)       )  ! Save convective velocity in the plume
      CALL iom_put( "mf_app", zapp (:,:,:)       )  ! Save convective area

      !=================================================================================
      !  Computation of a tridiagonal matrix and right hand side terms of the linear system
      !=================================================================================
      DO_3D( 0, 0, 0, 0, 1, jpk )
         edmfa(ji,jj,jk)     = 0._wp
         edmfb(ji,jj,jk)     = 0._wp
         edmfc(ji,jj,jk)     = 0._wp
         edmftra(ji,jj,jk,:) = 0._wp
      END_3D

      !---------------------------------------------------------------
      ! Diagonal terms 
      !---------------------------------------------------------------
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         edmfa(ji,jj,jk) =  0._wp
         edmfb(ji,jj,jk) = -edmfm(ji,jj,jk  ) / e3w(ji,jj,jk+1,Kmm)
         edmfc(ji,jj,jk) =  edmfm(ji,jj,jk+1) / e3w(ji,jj,jk+1,Kmm)
      END_3D
      DO_2D( 0, 0, 0, 0 )
         edmfa(ji,jj,jpk)   = -edmfm(ji,jj,jpk-1) / e3w(ji,jj,jpk,Kmm)
         edmfb(ji,jj,jpk)   =  edmfm(ji,jj,jpk  ) / e3w(ji,jj,jpk,Kmm)
         edmfc(ji,jj,jpk)   =  0._wp
      END_2D

      !---------------------------------------------------------------
      ! right hand side term for Temperature
      !---------------------------------------------------------------
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
        edmftra(ji,jj,jk,1) = - edmfm(ji,jj,jk  ) * ztsp(ji,jj,jk  ,jp_tem) / e3w(ji,jj,jk+1,Kmm) &
                            & + edmfm(ji,jj,jk+1) * ztsp(ji,jj,jk+1,jp_tem) / e3w(ji,jj,jk+1,Kmm)
      END_3D
      DO_2D( 0, 0, 0, 0 )
         edmftra(ji,jj,jpk,1) = - edmfm(ji,jj,jpk-1) * ztsp(ji,jj,jpk-1,jp_tem) / e3w(ji,jj,jpk,Kmm) &
                              & + edmfm(ji,jj,jpk  ) * ztsp(ji,jj,jpk  ,jp_tem) / e3w(ji,jj,jpk,Kmm)
      END_2D

      !---------------------------------------------------------------
      ! Right hand side term for Salinity
      !---------------------------------------------------------------
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         edmftra(ji,jj,jk,2) =  - edmfm(ji,jj,jk  ) * ztsp(ji,jj,jk  ,jp_sal) / e3w(ji,jj,jk+1,Kmm) &
                             &  + edmfm(ji,jj,jk+1) * ztsp(ji,jj,jk+1,jp_sal) / e3w(ji,jj,jk+1,Kmm)
      END_3D
      DO_2D( 0, 0, 0, 0 )
         edmftra(ji,jj,jpk,2) = - edmfm(ji,jj,jpk-1) * ztsp(ji,jj,jpk-1,jp_sal) / e3w(ji,jj,jpk,Kmm) &
                              & + edmfm(ji,jj,jpk  ) * ztsp(ji,jj,jpk  ,jp_sal) / e3w(ji,jj,jpk,Kmm)
      END_2D
      !
   END SUBROUTINE tra_mfc

   
   SUBROUTINE diag_mfc( zdiagi, zdiagd, zdiags, p2dt, Kaa )

      REAL(wp), DIMENSION(A2D(nn_hls),jpk), INTENT(inout) ::  zdiagi, zdiagd, zdiags  ! inout: tridaig. terms
      REAL(wp)                            , INTENT(in   ) ::   p2dt                   ! tracer time-step
      INTEGER                             , INTENT(in   ) ::   Kaa                    ! ocean time level indices

      INTEGER  ::   ji, jj, jk  ! dummy  loop arguments   

         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            zdiagi(ji,jj,jk) = zdiagi(ji,jj,jk) + e3t(ji,jj,jk,Kaa) * p2dt *edmfa(ji,jj,jk)
            zdiags(ji,jj,jk) = zdiags(ji,jj,jk) + e3t(ji,jj,jk,Kaa) * p2dt *edmfc(ji,jj,jk)
            zdiagd(ji,jj,jk) = zdiagd(ji,jj,jk) + e3t(ji,jj,jk,Kaa) * p2dt *edmfb(ji,jj,jk)
         END_3D

   END SUBROUTINE diag_mfc

   SUBROUTINE rhs_mfc( zrhs, jjn )

      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) ::   zrhs                   ! inout: rhs trend 
      INTEGER                         , INTENT(in   ) ::   jjn                    ! tracer indices

      INTEGER  ::   ji, jj, jk  ! dummy  loop arguments   

      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         zrhs(ji,jj,jk) = zrhs(ji,jj,jk) + edmftra(ji,jj,jk,jjn)
      END_3D

   END SUBROUTINE rhs_mfc



   SUBROUTINE zdf_mfc_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zdf_mfc_init  ***
      !!                    
      !! ** Purpose :   Initialization of the vertical eddy diffivity and
      !!      mass flux
      !!
      !! ** Method  :   Read the namzdf_mfc namelist and check the parameters
      !!      called at the first timestep (nit000)
      !!
      !! ** input   :   Namlist namzdf_mfc
      !!
      !! ** Action  :   Increase by 1 the nstop flag is setting problem encounter
      !!
      !!----------------------------------------------------------------------
      !
      INTEGER ::   jk    ! dummy loop indices
      INTEGER ::   ios   ! Local integer output status for namelist read
      REAL(wp)::   zcr   ! local scalar
      !!
      NAMELIST/namzdf_mfc/ ln_edmfuv, rn_cemf, rn_cwmf, rn_cent, rn_cdet, rn_cap, App_max
      !!----------------------------------------------------------
      !
      !
!      REWIND( numnam_ref )              ! Namelist namzdf_mfc in reference namelist : Vertical eddy diffivity mass flux
      READ  ( numnam_ref, namzdf_mfc, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namzdf_edmf in reference namelist' )

!      REWIND( numnam_cfg )              ! Namelist namzdf_mfc in configuration namelist : Vertical eddy diffivity mass flux
      READ  ( numnam_cfg, namzdf_mfc, IOSTAT = ios, ERR = 902 )
902   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namzdf_edmf in configuration namelist' )
      IF(lwm) WRITE ( numond, namzdf_mfc )

      IF(lwp) THEN                     !* Control print
         WRITE(numout,*)
         WRITE(numout,*) 'zdf_mfc_init'
         WRITE(numout,*) '~~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namzdf_mfc : set eddy diffusivity Mass Flux Convection'
         WRITE(numout,*) '   Apply mass flux on velocities (Not yet avail.)     ln_edmfuv = ', ln_edmfuv
         WRITE(numout,*) '   Coeff for entrain/detrain T/S of plume (Neg => cte) rn_cemf  = ', rn_cemf
         WRITE(numout,*) '   Coeff for entrain/detrain Wp of plume  (Neg => cte) rn_cwmf  = ', rn_cwmf
         WRITE(numout,*) '   Coeff for entrain/detrain area of plume             rn_cap   = ', rn_cap
         WRITE(numout,*) '   Coeff for entrain area of plume                     rn_cent  = ', rn_cent
         WRITE(numout,*) '   Coeff for detrain area of plume                     rn_cdet  = ', rn_cdet
         WRITE(numout,*) '   Max convective area                                 App_max  = ', App_max
       ENDIF
                                     !* allocate edmf arrays
      IF( zdf_mfc_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'zdf_edmf_init : unable to allocate arrays' )
      edmfa(:,:,:)     = 0._wp
      edmfb(:,:,:)     = 0._wp
      edmfc(:,:,:)     = 0._wp
      edmftra(:,:,:,:) = 0._wp
      !
   END SUBROUTINE zdf_mfc_init

   !!======================================================================
 
   !!======================================================================
END MODULE zdfmfc



