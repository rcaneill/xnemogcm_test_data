MODULE diaptr
   !!======================================================================
   !!                       ***  MODULE  diaptr  ***
   !! Ocean physics:  Computes meridonal transports and zonal means
   !!=====================================================================
   !! History :  1.0  ! 2003-09  (C. Talandier, G. Madec)  Original code
   !!            2.0  ! 2006-01  (A. Biastoch)  Allow sub-basins computation
   !!            3.2  ! 2010-03  (O. Marti, S. Flavoni) Add fields
   !!            3.3  ! 2010-10  (G. Madec)  dynamical allocation
   !!            3.6  ! 2014-12  (C. Ethe) use of IOM
   !!            3.6  ! 2016-06  (T. Graham) Addition of diagnostics for CMIP6
   !!            4.0  ! 2010-08  ( C. Ethe, J. Deshayes ) Improvment
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dia_ptr      : Poleward Transport Diagnostics module
   !!   dia_ptr_init : Initialization, namelist read
   !!   ptr_sjk      : "zonal" mean computation of a field - tracer or flux array
   !!   ptr_sj       : "zonal" and vertical sum computation of a "meridional" flux array
   !!                   (Generic interface to ptr_sj_3d, ptr_sj_2d)
   !!----------------------------------------------------------------------
   USE oce              ! ocean dynamics and active tracers
   USE dom_oce          ! ocean space and time domain
   USE domtile
   USE phycst           ! physical constants
   !
   USE iom              ! IOM library
   USE in_out_manager   ! I/O manager
   USE lib_mpp          ! MPP library
   USE timing           ! preformance summary

   IMPLICIT NONE
   PRIVATE

   INTERFACE ptr_sum
      MODULE PROCEDURE ptr_sum_3d, ptr_sum_2d
   END INTERFACE

   INTERFACE ptr_sj
      MODULE PROCEDURE ptr_sj_3d, ptr_sj_2d
   END INTERFACE

   PUBLIC   dia_ptr        ! call in step module
   PUBLIC   dia_ptr_hst    ! called from tra_ldf/tra_adv routines

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   hstr_adv, hstr_ldf, hstr_eiv   !: Heat/Salt TRansports(adv, diff, Bolus.)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   hstr_ove, hstr_btr, hstr_vtr   !: heat Salt TRansports(overturn, baro, merional)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   pvtr_int, pzon_int             !: Other zonal integrals

   LOGICAL, PUBLIC    ::   l_diaptr       !: tracers  trend flag
   INTEGER, PARAMETER ::   jp_msk = 3
   INTEGER, PARAMETER ::   jp_vtr = 4

   REAL(wp) ::   rc_sv    = 1.e-6_wp   ! conversion from m3/s to Sverdrup
   REAL(wp) ::   rc_pwatt = 1.e-15_wp  ! conversion from W    to PW (further x rho0 x Cp)
   REAL(wp) ::   rc_ggram = 1.e-9_wp   ! conversion from g    to Gg  (further x rho0)

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: btmsk   ! T-point basin interior masks
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: btmsk34 ! mask out Southern Ocean (=0 south of 34°S)

   LOGICAL ::   ll_init = .TRUE.        !: tracers  trend flag

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: diaptr.F90 14834 2021-05-11 09:24:44Z hadcv $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   ! NOTE: [tiling] tiling sometimes changes the diagnostics very slightly, usually where there are few zonal points e.g. the northern Indian Ocean basin. The difference is usually very small, for one point in one diagnostic. Presumably this is because of the additional zonal integration step over tiles.
   SUBROUTINE dia_ptr( kt, Kmm, pvtr )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dia_ptr  ***
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in)           ::   kt     ! ocean time-step index
      INTEGER                         , INTENT(in)           ::   Kmm    ! time level index
      REAL(wp), DIMENSION(A2D(nn_hls),jpk)    , INTENT(in), OPTIONAL ::   pvtr   ! j-effective transport
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('dia_ptr')

      IF( kt == nit000 .AND. ll_init )   CALL dia_ptr_init    ! -> will define l_diaptr and nbasin
      !
      IF( l_diaptr ) THEN
         ! Calculate zonal integrals
         IF( PRESENT( pvtr ) ) THEN
            CALL dia_ptr_zint( Kmm, pvtr )
         ELSE
            CALL dia_ptr_zint( Kmm )
         ENDIF

         ! Calculate diagnostics only when zonal integrals have finished
         IF( .NOT. l_istiled .OR. ntile == nijtile ) CALL dia_ptr_iom(kt, Kmm, pvtr)
      ENDIF

      IF( ln_timing )   CALL timing_stop('dia_ptr')
      !
   END SUBROUTINE dia_ptr


   SUBROUTINE dia_ptr_iom( kt, Kmm, pvtr )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dia_ptr_iom  ***
      !!----------------------------------------------------------------------
      !! ** Purpose : Calculate diagnostics and send to XIOS
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in)           ::   kt     ! ocean time-step index
      INTEGER                         , INTENT(in)           ::   Kmm    ! time level index
      REAL(wp), DIMENSION(A2D(nn_hls),jpk)    , INTENT(in), OPTIONAL ::   pvtr   ! j-effective transport
      !
      INTEGER  ::   ji, jj, jk, jn   ! dummy loop indices
      REAL(wp), DIMENSION(jpi,jpj)     ::  z2d   ! 2D workspace
      REAL(wp), DIMENSION(jpj)      ::  zvsum, ztsum, zssum   ! 1D workspace
      !
      !overturning calculation
      REAL(wp), DIMENSION(:,:,:  ), ALLOCATABLE ::   sjk, r1_sjk, v_msf  ! i-mean i-k-surface and its inverse
      REAL(wp), DIMENSION(:,:,:  ), ALLOCATABLE ::   zt_jk, zs_jk        ! i-mean T and S, j-Stream-Function

      REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE ::   z4d1, z4d2
      REAL(wp), DIMENSION(:,:,:  ), ALLOCATABLE ::   z3dtr
      !!----------------------------------------------------------------------
      !
      ALLOCATE( z3dtr(jpi,jpj,nbasin) )

      IF( PRESENT( pvtr ) ) THEN
         IF( iom_use( 'zomsf' ) ) THEN    ! effective MSF
            ALLOCATE( z4d1(jpi,jpj,jpk,nbasin) )
            !
            DO jn = 1, nbasin                                    ! by sub-basins
               z4d1(1,:,:,jn) =  pvtr_int(:,:,jp_vtr,jn)                  ! zonal cumulative effective transport excluding closed seas
               DO jk = jpkm1, 1, -1
                  z4d1(1,:,jk,jn) = z4d1(1,:,jk+1,jn) - z4d1(1,:,jk,jn)    ! effective j-Stream-Function (MSF)
               END DO
               DO ji = 2, jpi
                  z4d1(ji,:,:,jn) = z4d1(1,:,:,jn)
               ENDDO
            END DO
            CALL iom_put( 'zomsf', z4d1 * rc_sv )
            !
            DEALLOCATE( z4d1 )
         ENDIF
         IF( iom_use( 'sopstove' ) .OR. iom_use( 'sophtove' ) ) THEN
            ALLOCATE( sjk(jpj,jpk,nbasin), r1_sjk(jpj,jpk,nbasin), v_msf(jpj,jpk,nbasin),   &
               &      zt_jk(jpj,jpk,nbasin), zs_jk(jpj,jpk,nbasin) )
            !
            DO jn = 1, nbasin
               sjk(:,:,jn) = pvtr_int(:,:,jp_msk,jn)
               r1_sjk(:,:,jn) = 0._wp
               WHERE( sjk(:,:,jn) /= 0._wp )   r1_sjk(:,:,jn) = 1._wp / sjk(:,:,jn)
               ! i-mean T and S, j-Stream-Function, basin
               zt_jk(:,:,jn) = pvtr_int(:,:,jp_tem,jn) * r1_sjk(:,:,jn)
               zs_jk(:,:,jn) = pvtr_int(:,:,jp_sal,jn) * r1_sjk(:,:,jn)
               v_msf(:,:,jn) = pvtr_int(:,:,jp_vtr,jn)
               hstr_ove(:,jp_tem,jn) = SUM( v_msf(:,:,jn)*zt_jk(:,:,jn), 2 )
               hstr_ove(:,jp_sal,jn) = SUM( v_msf(:,:,jn)*zs_jk(:,:,jn), 2 )
               !
            ENDDO
            DO jn = 1, nbasin
               z3dtr(1,:,jn) = hstr_ove(:,jp_tem,jn) * rc_pwatt  !  (conversion in PW)
               DO ji = 2, jpi
                  z3dtr(ji,:,jn) = z3dtr(1,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'sophtove', z3dtr )
            DO jn = 1, nbasin
               z3dtr(1,:,jn) = hstr_ove(:,jp_sal,jn) * rc_ggram !  (conversion in Gg)
               DO ji = 2, jpi
                  z3dtr(ji,:,jn) = z3dtr(1,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'sopstove', z3dtr )
            !
            DEALLOCATE( sjk, r1_sjk, v_msf, zt_jk, zs_jk )
         ENDIF

         IF( iom_use( 'sopstbtr' ) .OR. iom_use( 'sophtbtr' ) ) THEN
            ! Calculate barotropic heat and salt transport here
            ALLOCATE( sjk(jpj,1,nbasin), r1_sjk(jpj,1,nbasin) )
            !
            DO jn = 1, nbasin
               sjk(:,1,jn) = SUM( pvtr_int(:,:,jp_msk,jn), 2 )
               r1_sjk(:,1,jn) = 0._wp
               WHERE( sjk(:,1,jn) /= 0._wp )   r1_sjk(:,1,jn) = 1._wp / sjk(:,1,jn)
               !
               zvsum(:) =    SUM( pvtr_int(:,:,jp_vtr,jn), 2 )
               ztsum(:) =    SUM( pvtr_int(:,:,jp_tem,jn), 2 )
               zssum(:) =    SUM( pvtr_int(:,:,jp_sal,jn), 2 )
               hstr_btr(:,jp_tem,jn) = zvsum(:) * ztsum(:) * r1_sjk(:,1,jn)
               hstr_btr(:,jp_sal,jn) = zvsum(:) * zssum(:) * r1_sjk(:,1,jn)
               !
            ENDDO
            DO jn = 1, nbasin
               z3dtr(1,:,jn) = hstr_btr(:,jp_tem,jn) * rc_pwatt  !  (conversion in PW)
               DO ji = 2, jpi
                  z3dtr(ji,:,jn) = z3dtr(1,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'sophtbtr', z3dtr )
            DO jn = 1, nbasin
               z3dtr(1,:,jn) = hstr_btr(:,jp_sal,jn) * rc_ggram !  (conversion in Gg)
               DO ji = 2, jpi
                  z3dtr(ji,:,jn) = z3dtr(1,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'sopstbtr', z3dtr )
            !
            DEALLOCATE( sjk, r1_sjk )
         ENDIF
         !
         hstr_ove(:,:,:) = 0._wp       ! Zero before next timestep
         hstr_btr(:,:,:) = 0._wp
         pvtr_int(:,:,:,:) = 0._wp
      ELSE
         IF( iom_use( 'zotem' ) .OR. iom_use( 'zosal' ) .OR. iom_use( 'zosrf' )  ) THEN    ! i-mean i-k-surface
            ALLOCATE( z4d1(jpi,jpj,jpk,nbasin), z4d2(jpi,jpj,jpk,nbasin) )
            !
            DO jn = 1, nbasin
               z4d1(1,:,:,jn) = pzon_int(:,:,jp_msk,jn)
               DO ji = 2, jpi
                  z4d1(ji,:,:,jn) = z4d1(1,:,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'zosrf', z4d1 )
            !
            DO jn = 1, nbasin
               z4d2(1,:,:,jn) = pzon_int(:,:,jp_tem,jn) / MAX( z4d1(1,:,:,jn), 10.e-15 )
               DO ji = 2, jpi
                  z4d2(ji,:,:,jn) = z4d2(1,:,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'zotem', z4d2 )
            !
            DO jn = 1, nbasin
               z4d2(1,:,:,jn) = pzon_int(:,:,jp_sal,jn) / MAX( z4d1(1,:,:,jn), 10.e-15 )
               DO ji = 2, jpi
                  z4d2(ji,:,:,jn) = z4d2(1,:,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'zosal', z4d2 )
            !
            DEALLOCATE( z4d1, z4d2 )
         ENDIF
         !
         !                                ! Advective and diffusive heat and salt transport
         IF( iom_use( 'sophtadv' ) .OR. iom_use( 'sopstadv' ) ) THEN
            !
            DO jn = 1, nbasin
               z3dtr(1,:,jn) = hstr_adv(:,jp_tem,jn) * rc_pwatt  !  (conversion in PW)
               DO ji = 2, jpi
                  z3dtr(ji,:,jn) = z3dtr(1,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'sophtadv', z3dtr )
            DO jn = 1, nbasin
               z3dtr(1,:,jn) = hstr_adv(:,jp_sal,jn) * rc_ggram !  (conversion in Gg)
               DO ji = 2, jpi
                  z3dtr(ji,:,jn) = z3dtr(1,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'sopstadv', z3dtr )
         ENDIF
         !
         IF( iom_use( 'sophtldf' ) .OR. iom_use( 'sopstldf' ) ) THEN
            !
            DO jn = 1, nbasin
               z3dtr(1,:,jn) = hstr_ldf(:,jp_tem,jn) * rc_pwatt  !  (conversion in PW)
               DO ji = 2, jpi
                  z3dtr(ji,:,jn) = z3dtr(1,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'sophtldf', z3dtr )
            DO jn = 1, nbasin
               z3dtr(1,:,jn) = hstr_ldf(:,jp_sal,jn) * rc_ggram !  (conversion in Gg)
               DO ji = 2, jpi
                  z3dtr(ji,:,jn) = z3dtr(1,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'sopstldf', z3dtr )
         ENDIF
         !
         IF( iom_use( 'sophteiv' ) .OR. iom_use( 'sopsteiv' ) ) THEN
            !
            DO jn = 1, nbasin
               z3dtr(1,:,jn) = hstr_eiv(:,jp_tem,jn) * rc_pwatt  !  (conversion in PW)
               DO ji = 2, jpi
                  z3dtr(ji,:,jn) = z3dtr(1,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'sophteiv', z3dtr )
            DO jn = 1, nbasin
               z3dtr(1,:,jn) = hstr_eiv(:,jp_sal,jn) * rc_ggram !  (conversion in Gg)
               DO ji = 2, jpi
                  z3dtr(ji,:,jn) = z3dtr(1,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'sopsteiv', z3dtr )
         ENDIF
         !
         IF( iom_use( 'sopstvtr' ) .OR. iom_use( 'sophtvtr' ) ) THEN
             DO jn = 1, nbasin
                z3dtr(1,:,jn) = hstr_vtr(:,jp_tem,jn) * rc_pwatt  !  (conversion in PW)
                DO ji = 2, jpi
                   z3dtr(ji,:,jn) = z3dtr(1,:,jn)
                ENDDO
             ENDDO
             CALL iom_put( 'sophtvtr', z3dtr )
             DO jn = 1, nbasin
               z3dtr(1,:,jn) = hstr_vtr(:,jp_sal,jn) * rc_ggram !  (conversion in Gg)
               DO ji = 2, jpi
                  z3dtr(ji,:,jn) = z3dtr(1,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'sopstvtr', z3dtr )
         ENDIF
         !
         IF( iom_use( 'uocetr_vsum_cumul' ) ) THEN
            CALL iom_get_var(  'uocetr_vsum_op', z2d ) ! get uocetr_vsum_op from xml
            z2d(:,:) = ptr_ci_2d( z2d(:,:) )
            CALL iom_put( 'uocetr_vsum_cumul', z2d )
         ENDIF
         !
         hstr_adv(:,:,:) = 0._wp       ! Zero before next timestep
         hstr_ldf(:,:,:) = 0._wp
         hstr_eiv(:,:,:) = 0._wp
         hstr_vtr(:,:,:) = 0._wp
         pzon_int(:,:,:,:) = 0._wp
      ENDIF
      !
      DEALLOCATE( z3dtr )
      !
   END SUBROUTINE dia_ptr_iom


   SUBROUTINE dia_ptr_zint( Kmm, pvtr )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dia_ptr_zint ***
      !!----------------------------------------------------------------------
      !! ** Purpose : i and i-k sum operations on arrays
      !!
      !! ** Method  : - Call ptr_sjk (i sum) or ptr_sj (i-k sum) to perform the sum operation
      !!              - Call ptr_sum to add this result to the sum over tiles
      !!
      !! ** Action  : pvtr_int - terms for volume streamfunction, heat/salt transport barotropic/overturning terms
      !!              pzon_int - terms for i mean temperature/salinity
      !!----------------------------------------------------------------------
      INTEGER                     , INTENT(in)           :: Kmm          ! time level index
      REAL(wp), DIMENSION(A2D(nn_hls),jpk), INTENT(in), OPTIONAL :: pvtr         ! j-effective transport
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE            :: zmask        ! 3D workspace
      REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE          :: zts          ! 4D workspace
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE            :: sjk, v_msf   ! Zonal sum: i-k surface area, j-effective transport
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE            :: zt_jk, zs_jk ! Zonal sum: i-k surface area * (T, S)
      REAL(wp)                                           :: zsfc, zvfc   ! i-k surface area
      INTEGER  ::   ji, jj, jk, jn                                       ! dummy loop indices
      !!----------------------------------------------------------------------

      IF( PRESENT( pvtr ) ) THEN
         ! i sum of effective j transport excluding closed seas
         IF( iom_use( 'zomsf' ) .OR. iom_use( 'sopstove' ) .OR. iom_use( 'sophtove' ) ) THEN
            ALLOCATE( v_msf(A1Dj(nn_hls),jpk,nbasin) )

            DO jn = 1, nbasin
               v_msf(:,:,jn) = ptr_sjk( pvtr(:,:,:), btmsk34(:,:,jn) )
            ENDDO

            CALL ptr_sum( pvtr_int(:,:,jp_vtr,:), v_msf(:,:,:) )

            DEALLOCATE( v_msf )
         ENDIF

         ! i sum of j surface area, j surface area - temperature/salinity product on V grid
         IF(  iom_use( 'sopstove' ) .OR. iom_use( 'sophtove' ) .OR.   &
            & iom_use( 'sopstbtr' ) .OR. iom_use( 'sophtbtr' ) ) THEN
            ALLOCATE( zmask(A2D(nn_hls),jpk), zts(A2D(nn_hls),jpk,jpts), &
               &      sjk(A1Dj(nn_hls),jpk,nbasin), &
               &      zt_jk(A1Dj(nn_hls),jpk,nbasin), zs_jk(A1Dj(nn_hls),jpk,nbasin) )

            zmask(:,:,:) = 0._wp
            zts(:,:,:,:) = 0._wp

            DO_3D( 1, 1, 1, 0, 1, jpkm1 )
               zvfc = e1v(ji,jj) * e3v(ji,jj,jk,Kmm)
               zmask(ji,jj,jk)      = vmask(ji,jj,jk)      * zvfc
               zts(ji,jj,jk,jp_tem) = (ts(ji,jj,jk,jp_tem,Kmm)+ts(ji,jj+1,jk,jp_tem,Kmm)) * 0.5 * zvfc !Tracers averaged onto V grid
               zts(ji,jj,jk,jp_sal) = (ts(ji,jj,jk,jp_sal,Kmm)+ts(ji,jj+1,jk,jp_sal,Kmm)) * 0.5 * zvfc
            END_3D

            DO jn = 1, nbasin
               sjk(:,:,jn)   = ptr_sjk( zmask(:,:,:)     , btmsk(:,:,jn) )
               zt_jk(:,:,jn) = ptr_sjk( zts(:,:,:,jp_tem), btmsk(:,:,jn) )
               zs_jk(:,:,jn) = ptr_sjk( zts(:,:,:,jp_sal), btmsk(:,:,jn) )
            ENDDO

            CALL ptr_sum( pvtr_int(:,:,jp_msk,:), sjk(:,:,:)   )
            CALL ptr_sum( pvtr_int(:,:,jp_tem,:), zt_jk(:,:,:) )
            CALL ptr_sum( pvtr_int(:,:,jp_sal,:), zs_jk(:,:,:) )

            DEALLOCATE( zmask, zts, sjk, zt_jk, zs_jk )
         ENDIF
      ELSE
         ! i sum of j surface area - temperature/salinity product on T grid
         IF( iom_use( 'zotem' ) .OR. iom_use( 'zosal' ) .OR. iom_use( 'zosrf' )  ) THEN
            ALLOCATE( zmask(A2D(nn_hls),jpk), zts(A2D(nn_hls),jpk,jpts), &
               &      sjk(A1Dj(nn_hls),jpk,nbasin), &
               &      zt_jk(A1Dj(nn_hls),jpk,nbasin), zs_jk(A1Dj(nn_hls),jpk,nbasin) )

            zmask(:,:,:) = 0._wp
            zts(:,:,:,:) = 0._wp

            DO_3D( 1, 1, 1, 1, 1, jpkm1 )
               zsfc = e1t(ji,jj) * e3t(ji,jj,jk,Kmm)
               zmask(ji,jj,jk)      = tmask(ji,jj,jk)      * zsfc
               zts(ji,jj,jk,jp_tem) = ts(ji,jj,jk,jp_tem,Kmm) * zsfc
               zts(ji,jj,jk,jp_sal) = ts(ji,jj,jk,jp_sal,Kmm) * zsfc
            END_3D

            DO jn = 1, nbasin
               sjk(:,:,jn)   = ptr_sjk( zmask(:,:,:)     , btmsk(:,:,jn) )
               zt_jk(:,:,jn) = ptr_sjk( zts(:,:,:,jp_tem), btmsk(:,:,jn) )
               zs_jk(:,:,jn) = ptr_sjk( zts(:,:,:,jp_sal), btmsk(:,:,jn) )
            ENDDO

            CALL ptr_sum( pzon_int(:,:,jp_msk,:), sjk(:,:,:)   )
            CALL ptr_sum( pzon_int(:,:,jp_tem,:), zt_jk(:,:,:) )
            CALL ptr_sum( pzon_int(:,:,jp_sal,:), zs_jk(:,:,:) )

            DEALLOCATE( zmask, zts, sjk, zt_jk, zs_jk )
         ENDIF

         ! i-k sum of j surface area - temperature/salinity product on V grid
         IF( iom_use( 'sopstvtr' ) .OR. iom_use( 'sophtvtr' ) ) THEN
            ALLOCATE( zts(A2D(nn_hls),jpk,jpts) )

            zts(:,:,:,:) = 0._wp

            DO_3D( 1, 1, 1, 0, 1, jpkm1 )
               zvfc = e1v(ji,jj) * e3v(ji,jj,jk,Kmm)
               zts(ji,jj,jk,jp_tem) = (ts(ji,jj,jk,jp_tem,Kmm)+ts(ji,jj+1,jk,jp_tem,Kmm)) * 0.5 * zvfc  !Tracers averaged onto V grid
               zts(ji,jj,jk,jp_sal) = (ts(ji,jj,jk,jp_sal,Kmm)+ts(ji,jj+1,jk,jp_sal,Kmm)) * 0.5 * zvfc
            END_3D

            CALL dia_ptr_hst( jp_tem, 'vtr', zts(:,:,:,jp_tem) )
            CALL dia_ptr_hst( jp_sal, 'vtr', zts(:,:,:,jp_sal) )

            DEALLOCATE( zts )
         ENDIF
      ENDIF
   END SUBROUTINE dia_ptr_zint


   SUBROUTINE dia_ptr_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dia_ptr_init  ***
      !!
      !! ** Purpose :   Initialization
      !!----------------------------------------------------------------------
      INTEGER ::  inum, jn           ! local integers
      !!
      REAL(wp), DIMENSION(jpi,jpj) :: zmsk
      !!----------------------------------------------------------------------

      ! l_diaptr is defined with iom_use
      !   --> dia_ptr_init must be done after the call to iom_init
      !   --> cannot be .TRUE. without cpp key: key_xios -->  nbasin define by iom_init is initialized
      l_diaptr = iom_use( 'zomsf'    ) .OR. iom_use( 'zotem'    ) .OR. iom_use( 'zosal'    ) .OR.  &
         &       iom_use( 'zosrf'    ) .OR. iom_use( 'sopstove' ) .OR. iom_use( 'sophtove' ) .OR.  &
         &       iom_use( 'sopstbtr' ) .OR. iom_use( 'sophtbtr' ) .OR. iom_use( 'sophtadv' ) .OR.  &
         &       iom_use( 'sopstadv' ) .OR. iom_use( 'sophtldf' ) .OR. iom_use( 'sopstldf' ) .OR.  &
         &       iom_use( 'sophteiv' ) .OR. iom_use( 'sopsteiv' ) .OR. iom_use( 'sopstvtr' ) .OR.  &
         &       iom_use( 'sophtvtr' ) .OR. iom_use( 'uocetr_vsum_cumul' )

      IF(lwp) THEN                     ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'dia_ptr_init : poleward transport and msf initialization'
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '      Poleward heat & salt transport (T) or not (F)      l_diaptr  = ', l_diaptr
      ENDIF

      IF( l_diaptr ) THEN
         !
         IF( dia_ptr_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'dia_ptr_init : unable to allocate arrays' )
         !
         rc_pwatt = rc_pwatt * rho0_rcp          ! conversion from K.s-1 to PetaWatt
         rc_ggram = rc_ggram * rho0              ! conversion from m3/s to Gg/s

         IF( lk_mpp )   CALL mpp_ini_znl( numout )     ! Define MPI communicator for zonal sum

         btmsk(:,:,1) = tmask_i(:,:)
         IF( nbasin == 5 ) THEN   ! nbasin has been initialized in iom_init to define the axis "basin"
            CALL iom_open( 'subbasins', inum )
            CALL iom_get( inum, jpdom_global, 'atlmsk', btmsk(:,:,2) )   ! Atlantic basin
            CALL iom_get( inum, jpdom_global, 'pacmsk', btmsk(:,:,3) )   ! Pacific  basin
            CALL iom_get( inum, jpdom_global, 'indmsk', btmsk(:,:,4) )   ! Indian   basin
            CALL iom_close( inum )
            btmsk(:,:,5) = MAX ( btmsk(:,:,3), btmsk(:,:,4) )            ! Indo-Pacific basin
         ENDIF
         DO jn = 2, nbasin
            btmsk(:,:,jn) = btmsk(:,:,jn) * tmask_i(:,:)                 ! interior domain only
         END DO
         ! JD : modification so that overturning streamfunction is available in Atlantic at 34S to compare with observations
         WHERE( gphit(:,:)*tmask_i(:,:) < -34._wp)
           zmsk(:,:) = 0._wp      ! mask out Southern Ocean
         ELSE WHERE
           zmsk(:,:) = ssmask(:,:)
         END WHERE
         btmsk34(:,:,1) = btmsk(:,:,1)
         DO jn = 2, nbasin
            btmsk34(:,:,jn) = btmsk(:,:,jn) * zmsk(:,:)                  ! interior domain only
         ENDDO

         ! Initialise arrays to zero because diatpr is called before they are first calculated
         ! Note that this means diagnostics will not be exactly correct when model run is restarted.
         hstr_adv(:,:,:) = 0._wp
         hstr_ldf(:,:,:) = 0._wp
         hstr_eiv(:,:,:) = 0._wp
         hstr_ove(:,:,:) = 0._wp
         hstr_btr(:,:,:) = 0._wp           !
         hstr_vtr(:,:,:) = 0._wp           !
         pvtr_int(:,:,:,:) = 0._wp
         pzon_int(:,:,:,:) = 0._wp
         !
         ll_init = .FALSE.
         !
      ENDIF
      !
   END SUBROUTINE dia_ptr_init


   SUBROUTINE dia_ptr_hst( ktra, cptr, pvflx )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dia_ptr_hst ***
      !!----------------------------------------------------------------------
      !! Wrapper for heat and salt transport calculations to calculate them for each basin
      !! Called from all advection and/or diffusion routines
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in )  :: ktra  ! tracer index
      CHARACTER(len=3)                , INTENT(in)   :: cptr  ! transport type  'adv'/'ldf'/'eiv'
      REAL(wp), DIMENSION(A2D(nn_hls),jpk)    , INTENT(in)   :: pvflx ! 3D input array of advection/diffusion
      REAL(wp), DIMENSION(A1Dj(nn_hls),nbasin)                 :: zsj   !
      INTEGER                                        :: jn    !

      DO jn = 1, nbasin
         zsj(:,jn) = ptr_sj( pvflx(:,:,:), btmsk(:,:,jn) )
      ENDDO
      !
      IF( cptr == 'adv' ) THEN
         IF( ktra == jp_tem )  CALL ptr_sum( hstr_adv(:,jp_tem,:), zsj(:,:) )
         IF( ktra == jp_sal )  CALL ptr_sum( hstr_adv(:,jp_sal,:), zsj(:,:) )
      ELSE IF( cptr == 'ldf' ) THEN
         IF( ktra == jp_tem )  CALL ptr_sum( hstr_ldf(:,jp_tem,:), zsj(:,:) )
         IF( ktra == jp_sal )  CALL ptr_sum( hstr_ldf(:,jp_sal,:), zsj(:,:) )
      ELSE IF( cptr == 'eiv' ) THEN
         IF( ktra == jp_tem )  CALL ptr_sum( hstr_eiv(:,jp_tem,:), zsj(:,:) )
         IF( ktra == jp_sal )  CALL ptr_sum( hstr_eiv(:,jp_sal,:), zsj(:,:) )
      ELSE IF( cptr == 'vtr' ) THEN
         IF( ktra == jp_tem )  CALL ptr_sum( hstr_vtr(:,jp_tem,:), zsj(:,:) )
         IF( ktra == jp_sal )  CALL ptr_sum( hstr_vtr(:,jp_sal,:), zsj(:,:) )
      ENDIF
      !
   END SUBROUTINE dia_ptr_hst


   SUBROUTINE ptr_sum_2d( phstr, pva )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE ptr_sum_2d ***
      !!----------------------------------------------------------------------
      !! ** Purpose : Add two 2D arrays with (j,nbasin) dimensions
      !!
      !! ** Method  : - phstr = phstr + pva
      !!              - Call mpp_sum if the final tile
      !!
      !! ** Action  : phstr
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpj,nbasin) , INTENT(inout)         ::  phstr  !
      REAL(wp), DIMENSION(A1Dj(nn_hls),nbasin), INTENT(in)            ::  pva    !
      INTEGER                                               ::  jj
#if ! defined key_mpi_off
      INTEGER, DIMENSION(1)           ::  ish1d
      INTEGER, DIMENSION(2)           ::  ish2d
      REAL(wp), DIMENSION(jpj*nbasin) ::  zwork
#endif

      DO jj = ntsj, ntej
         phstr(jj,:) = phstr(jj,:)  + pva(jj,:)
      END DO

#if ! defined key_mpi_off
      IF( .NOT. l_istiled .OR. ntile == nijtile ) THEN
         ish1d(1) = jpj*nbasin
         ish2d(1) = jpj ; ish2d(2) = nbasin
         zwork(:) = RESHAPE( phstr(:,:), ish1d )
         CALL mpp_sum( 'diaptr', zwork, ish1d(1), ncomm_znl )
         phstr(:,:) = RESHAPE( zwork, ish2d )
      ENDIF
#endif
   END SUBROUTINE ptr_sum_2d


   SUBROUTINE ptr_sum_3d( phstr, pva )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE ptr_sum_3d ***
      !!----------------------------------------------------------------------
      !! ** Purpose : Add two 3D arrays with (j,k,nbasin) dimensions
      !!
      !! ** Method  : - phstr = phstr + pva
      !!              - Call mpp_sum if the final tile
      !!
      !! ** Action  : phstr
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpj,jpk,nbasin) , INTENT(inout)     ::  phstr  !
      REAL(wp), DIMENSION(A1Dj(nn_hls),jpk,nbasin), INTENT(in)        ::  pva    !
      INTEGER                                               ::  jj, jk
#if ! defined key_mpi_off
      INTEGER, DIMENSION(1)              ::  ish1d
      INTEGER, DIMENSION(3)              ::  ish3d
      REAL(wp), DIMENSION(jpj*jpk*nbasin)  ::  zwork
#endif

      DO jk = 1, jpk
         DO jj = ntsj, ntej
            phstr(jj,jk,:) = phstr(jj,jk,:)  + pva(jj,jk,:)
         END DO
      END DO

#if ! defined key_mpi_off
      IF( .NOT. l_istiled .OR. ntile == nijtile ) THEN
         ish1d(1) = jpj*jpk*nbasin
         ish3d(1) = jpj ; ish3d(2) = jpk ; ish3d(3) = nbasin
         zwork(:) = RESHAPE( phstr(:,:,:), ish1d )
         CALL mpp_sum( 'diaptr', zwork, ish1d(1), ncomm_znl )
         phstr(:,:,:) = RESHAPE( zwork, ish3d )
      ENDIF
#endif
   END SUBROUTINE ptr_sum_3d


   FUNCTION dia_ptr_alloc()
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dia_ptr_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER               ::   dia_ptr_alloc   ! return value
      INTEGER, DIMENSION(2) ::   ierr
      !!----------------------------------------------------------------------
      ierr(:) = 0
      !
      ! nbasin has been initialized in iom_init to define the axis "basin"
      !
      IF( .NOT. ALLOCATED( btmsk ) ) THEN
         ALLOCATE( btmsk(jpi,jpj,nbasin)    , btmsk34(jpi,jpj,nbasin),   &
            &      hstr_adv(jpj,jpts,nbasin), hstr_eiv(jpj,jpts,nbasin), &
            &      hstr_ove(jpj,jpts,nbasin), hstr_btr(jpj,jpts,nbasin), &
            &      hstr_ldf(jpj,jpts,nbasin), hstr_vtr(jpj,jpts,nbasin), STAT=ierr(1)  )
            !
         ALLOCATE( pvtr_int(jpj,jpk,jpts+2,nbasin), &
            &      pzon_int(jpj,jpk,jpts+1,nbasin), STAT=ierr(2) )
         !
         dia_ptr_alloc = MAXVAL( ierr )
         CALL mpp_sum( 'diaptr', dia_ptr_alloc )
      ENDIF
      !
   END FUNCTION dia_ptr_alloc


   FUNCTION ptr_sj_3d( pvflx, pmsk )   RESULT ( p_fval )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE ptr_sj_3d  ***
      !!
      !! ** Purpose :   i-k sum computation of a j-flux array
      !!
      !! ** Method  : - i-k sum of pvflx using the interior 2D vmask (vmask_i).
      !!              pvflx is supposed to be a masked flux (i.e. * vmask*e1v*e3v)
      !!
      !! ** Action  : - p_fval: i-k-mean poleward flux of pvflx
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in), DIMENSION(A2D(nn_hls),jpk)  ::   pvflx  ! mask flux array at V-point
      REAL(wp), INTENT(in), DIMENSION(jpi,jpj)  ::   pmsk   ! Optional 2D basin mask
      !
      INTEGER                  ::   ji, jj, jk   ! dummy loop arguments
      REAL(wp), DIMENSION(A1Dj(nn_hls)) :: p_fval  ! function value
      !!--------------------------------------------------------------------
      !
      p_fval(:) = 0._wp
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         p_fval(jj) = p_fval(jj) + pvflx(ji,jj,jk) * pmsk(ji,jj) * tmask_i(ji,jj)
      END_3D
   END FUNCTION ptr_sj_3d


   FUNCTION ptr_sj_2d( pvflx, pmsk )   RESULT ( p_fval )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE ptr_sj_2d  ***
      !!
      !! ** Purpose :   "zonal" and vertical sum computation of a j-flux array
      !!
      !! ** Method  : - i-k sum of pvflx using the interior 2D vmask (vmask_i).
      !!      pvflx is supposed to be a masked flux (i.e. * vmask*e1v*e3v)
      !!
      !! ** Action  : - p_fval: i-k-mean poleward flux of pvflx
      !!----------------------------------------------------------------------
      REAL(wp) , INTENT(in), DIMENSION(A2D(nn_hls))     ::   pvflx  ! mask flux array at V-point
      REAL(wp) , INTENT(in), DIMENSION(jpi,jpj) ::   pmsk   ! Optional 2D basin mask
      !
      INTEGER                  ::   ji,jj       ! dummy loop arguments
      REAL(wp), DIMENSION(A1Dj(nn_hls)) :: p_fval ! function value
      !!--------------------------------------------------------------------
      !
      p_fval(:) = 0._wp
      DO_2D( 0, 0, 0, 0 )
         p_fval(jj) = p_fval(jj) + pvflx(ji,jj) * pmsk(ji,jj) * tmask_i(ji,jj)
      END_2D
   END FUNCTION ptr_sj_2d

   FUNCTION ptr_ci_2d( pva )   RESULT ( p_fval )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE ptr_ci_2d  ***
      !!
      !! ** Purpose :   "meridional" cumulated sum computation of a j-flux array
      !!
      !! ** Method  : - j cumulated sum of pva using the interior 2D vmask (umask_i).
      !!
      !! ** Action  : - p_fval: j-cumulated sum of pva
      !!----------------------------------------------------------------------
      REAL(wp) , INTENT(in), DIMENSION(jpi,jpj)  ::   pva   ! mask flux array at V-point
      !
      INTEGER                  ::   ji,jj,jc       ! dummy loop arguments
      INTEGER                  ::   ijpj        ! ???
      REAL(wp), DIMENSION(jpi,jpj) :: p_fval ! function value
      !!--------------------------------------------------------------------
      !
      ijpj = jpj  ! ???
      p_fval(:,:) = 0._wp
      DO jc = 1, jpnj ! looping over all processors in j axis
         DO_2D( 0, 0, 0, 0 )
            p_fval(ji,jj) = p_fval(ji,jj-1) + pva(ji,jj) * tmask_i(ji,jj)
         END_2D
      END DO
      !
   END FUNCTION ptr_ci_2d



   FUNCTION ptr_sjk( pta, pmsk )   RESULT ( p_fval )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE ptr_sjk  ***
      !!
      !! ** Purpose :   i-sum computation of an array
      !!
      !! ** Method  : - i-sum of field using the interior 2D vmask (pmsk).
      !!
      !! ** Action  : - p_fval: i-sum of masked field
      !!----------------------------------------------------------------------
      !!
      IMPLICIT none
      REAL(wp) , INTENT(in), DIMENSION(A2D(nn_hls),jpk) ::   pta    ! mask flux array at V-point
      REAL(wp) , INTENT(in), DIMENSION(jpi,jpj) ::   pmsk   ! Optional 2D basin mask
      !!
      INTEGER                           :: ji, jj, jk ! dummy loop arguments
      REAL(wp), DIMENSION(A1Dj(nn_hls),jpk) :: p_fval     ! return function value
      !!--------------------------------------------------------------------
      !
      p_fval(:,:) = 0._wp
      !
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         p_fval(jj,jk) = p_fval(jj,jk) + pta(ji,jj,jk) * pmsk(ji,jj) * tmask_i(ji,jj)
      END_3D
   END FUNCTION ptr_sjk


   !!======================================================================
END MODULE diaptr
