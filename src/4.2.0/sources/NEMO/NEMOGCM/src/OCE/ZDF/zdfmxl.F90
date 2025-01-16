MODULE zdfmxl
   !!======================================================================
   !!                       ***  MODULE  zdfmxl  ***
   !! Ocean physics: mixed layer depth 
   !!======================================================================
   !! History :  1.0  ! 2003-08  (G. Madec)  original code
   !!            3.2  ! 2009-07  (S. Masson, G. Madec)  IOM + merge of DO-loop
   !!            3.7  ! 2012-03  (G. Madec)  make public the density criteria for trdmxl 
   !!             -   ! 2014-02  (F. Roquet)  mixed layer depth calculated using N2 instead of rhop 
   !!----------------------------------------------------------------------
   !!   zdf_mxl      : Compute the turbocline and mixed layer depths.
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers variables
   USE isf_oce        ! ice shelf
   USE dom_oce        ! ocean space and time domain variables
   USE trc_oce  , ONLY: l_offline         ! ocean space and time domain variables
   USE zdf_oce        ! ocean vertical physics
   !
   USE in_out_manager ! I/O manager
   USE prtctl         ! Print control
   USE phycst         ! physical constants
   USE iom            ! I/O library
   USE lib_mpp        ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   zdf_mxl, zdf_mxl_turb, zdf_mxl_alloc   ! called by zdfphy.F90

   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   nmln    !: number of level in the mixed layer (used by LDF, ZDF, TRD, TOP)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   hmld    !: mixing layer depth (turbocline)      [m]   (used by TOP)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   hmlp    !: mixed layer depth  (rho=rho0+zdcrit) [m]   (used by LDF)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   hmlpt   !: depth of the last T-point inside the mixed layer [m] (used by LDF)

   REAL(wp), PUBLIC ::   rho_c = 0.01_wp    !: density criterion for mixed layer depth
   REAL(wp), PUBLIC ::   avt_c = 5.e-4_wp   ! Kz criterion for the turbocline depth

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: zdfmxl.F90 15249 2021-09-13 09:59:09Z hadcv $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION zdf_mxl_alloc()
      !!----------------------------------------------------------------------
      !!               ***  FUNCTION zdf_mxl_alloc  ***
      !!----------------------------------------------------------------------
      zdf_mxl_alloc = 0      ! set to zero if no array to be allocated
      IF( .NOT. ALLOCATED( nmln ) ) THEN
         ALLOCATE( nmln(jpi,jpj), hmld(jpi,jpj), hmlp(jpi,jpj), hmlpt(jpi,jpj), STAT= zdf_mxl_alloc )
         !
         CALL mpp_sum ( 'zdfmxl', zdf_mxl_alloc )
         IF( zdf_mxl_alloc /= 0 )   CALL ctl_stop( 'STOP', 'zdf_mxl_alloc: failed to allocate arrays.' )
         !
      ENDIF
   END FUNCTION zdf_mxl_alloc


   SUBROUTINE zdf_mxl( kt, Kmm )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zdfmxl  ***
      !!                   
      !! ** Purpose :   Compute the mixed layer depth with density criteria.
      !!
      !! ** Method  :   The mixed layer depth is the shallowest W depth with 
      !!      the density of the corresponding T point (just bellow) bellow a
      !!      given value defined locally as rho(10m) + rho_c
      !!
      !! ** Action  :   nmln, hmlp, hmlpt
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time-step index
      INTEGER, INTENT(in) ::   Kmm  ! ocean time level index
      !
      INTEGER  ::   ji, jj, jk      ! dummy loop indices
      INTEGER  ::   iik, ikt        ! local integer
      REAL(wp) ::   zN2_c           ! local scalar
      !!----------------------------------------------------------------------
      !
      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                       ! Do only on the first tile
         IF( kt == nit000 ) THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'zdf_mxl : mixed layer depth'
            IF(lwp) WRITE(numout,*) '~~~~~~~ '
         ENDIF
      ENDIF
      !
      ! w-level of the mixing and mixed layers
      DO_2D_OVR( nn_hls, nn_hls, nn_hls, nn_hls )
         nmln(ji,jj)  = nlb10                  ! Initialization to the number of w ocean point
         hmlp(ji,jj)  = 0._wp                  ! here hmlp used as a dummy variable, integrating vertically N^2
      END_2D
      zN2_c = grav * rho_c * r1_rho0      ! convert density criteria into N^2 criteria
      DO_3D_OVR( nn_hls, nn_hls, nn_hls, nn_hls, nlb10, jpkm1 )   ! Mixed layer level: w-level
         ikt = mbkt(ji,jj)
         hmlp(ji,jj) =   &
            & hmlp(ji,jj) + MAX( rn2b(ji,jj,jk) , 0._wp ) * e3w(ji,jj,jk,Kmm)
         IF( hmlp(ji,jj) < zN2_c )   nmln(ji,jj) = MIN( jk , ikt ) + 1   ! Mixed layer level
      END_3D
      ! depth of the mixed layer
      DO_2D_OVR( nn_hls, nn_hls, nn_hls, nn_hls )
         iik = nmln(ji,jj)
         hmlp (ji,jj) = gdepw(ji,jj,iik  ,Kmm) * ssmask(ji,jj)    ! Mixed layer depth
         hmlpt(ji,jj) = gdept(ji,jj,iik-1,Kmm) * ssmask(ji,jj)    ! depth of the last T-point inside the mixed layer
      END_2D
      !
      IF( .NOT.l_offline .AND. iom_use("mldr10_1") ) THEN
         IF( .NOT. l_istiled .OR. ntile == nijtile ) THEN         ! Do only on the last tile
            IF( ln_isfcav ) THEN  ;  CALL iom_put( "mldr10_1", hmlp - risfdep)   ! mixed layer thickness
            ELSE                  ;  CALL iom_put( "mldr10_1", hmlp )            ! mixed layer depth
            END IF
         ENDIF
      ENDIF
      !
      IF(sn_cfctl%l_prtctl)   CALL prt_ctl( tab2d_1=REAL(nmln,wp), clinfo1=' nmln : ', tab2d_2=hmlp, clinfo2=' hmlp : ' )
      !
   END SUBROUTINE zdf_mxl


   SUBROUTINE zdf_mxl_turb( kt, Kmm )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zdf_mxl_turb  ***
      !!
      !! ** Purpose :   Compute the turbocline depth.
      !!
      !! ** Method  :   The turbocline depth is the depth at which the vertical
      !!      eddy diffusivity coefficient (resulting from the vertical physics
      !!      alone, not the isopycnal part, see trazdf.F) fall below a given
      !!      value defined locally (avt_c here taken equal to 5 cm/s2 by default)
      !!
      !! ** Action  :   hmld
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time-step index
      INTEGER, INTENT(in) ::   Kmm  ! ocean time level index
      !
      INTEGER  ::   ji, jj, jk      ! dummy loop indices
      INTEGER  ::   iik             ! local integer
      INTEGER, DIMENSION(A2D(nn_hls)) ::   imld   ! 2D workspace
      !!----------------------------------------------------------------------
      !
      ! w-level of the turbocline and mixing layer (iom_use)
      imld(:,:) = mbkt(A2D(nn_hls)) + 1                ! Initialization to the number of w ocean point
      DO_3DS( 1, 1, 1, 1, jpkm1, nlb10, -1 )   ! from the bottom to nlb10
         IF( avt (ji,jj,jk) < avt_c * wmask(ji,jj,jk) )   imld(ji,jj) = jk      ! Turbocline
      END_3D
      ! depth of the mixing layer
      DO_2D_OVR( 1, 1, 1, 1 )
         iik = imld(ji,jj)
         hmld (ji,jj) = gdepw(ji,jj,iik  ,Kmm) * ssmask(ji,jj)    ! Turbocline depth
      END_2D
      !
      IF( .NOT.l_offline .AND. iom_use("mldkz5") ) THEN
         IF( .NOT. l_istiled .OR. ntile == nijtile ) THEN         ! Do only on the last tile
            IF( ln_isfcav ) THEN  ;  CALL iom_put( "mldkz5"  , hmld - risfdep )   ! turbocline thickness
            ELSE                  ;  CALL iom_put( "mldkz5"  , hmld )             ! turbocline depth
            END IF
         ENDIF
      ENDIF
      !
   END SUBROUTINE zdf_mxl_turb
   !!======================================================================
END MODULE zdfmxl
