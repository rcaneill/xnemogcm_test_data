MODULE p2zexp
   !!======================================================================
   !!                         ***  MODULE p2zsed  ***
   !! TOP :   LOBSTER Compute loss of organic matter in the sediments
   !!======================================================================
   !! History :    -   !  1999    (O. Aumont, C. Le Quere)  original code
   !!              -   !  2001-05 (O. Aumont, E. Kestenare) add sediment computations
   !!             1.0  !  2005-06 (A.-S. Kremeur) new temporal integration for sedpoc
   !!             2.0  !  2007-12  (C. Deltel, G. Madec)  F90
   !!             3.5  !  2012-03  (C. Ethe)  Merge PISCES-LOBSTER
   !!----------------------------------------------------------------------
   !!   p2z_exp        :  Compute loss of organic matter in the sediments
   !!----------------------------------------------------------------------
   USE oce_trc         !
   USE trc
   USE sms_pisces
   USE p2zsed
   USE lbclnk
   USE prtctl          ! Print control for debbuging
   USE trd_oce
   USE trdtrc
   USE iom

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p2z_exp    
   PUBLIC   p2z_exp_init 
   PUBLIC   p2z_exp_alloc

   !
   REAL(wp), ALLOCATABLE, DIMENSION(:,:)   ::   dminl     !: fraction of sinking POC released in sediments
   REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   dmin3     !: fraction of sinking POC released at each level
   REAL(wp), ALLOCATABLE, DIMENSION(:,:)   ::   sedpocb   !: mass of POC in sediments
   REAL(wp), ALLOCATABLE, DIMENSION(:,:)   ::   sedpocn   !: mass of POC in sediments
   REAL(wp), ALLOCATABLE, DIMENSION(:,:)   ::   cmask     !: Coastal mask area
   REAL(wp)                                ::   areacot   !: surface coastal area

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p2zexp.F90 15090 2021-07-06 14:25:18Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p2z_exp( kt, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p2z_exp  ***
      !!
      !! ** Purpose :   MODELS EXPORT OF BIOGENIC MATTER (POC ''SOFT
      !!              TISSUE'') AND ITS DISTRIBUTION IN WATER COLUMN
      !!
      !! ** Method  : - IN THE SURFACE LAYER POC IS PRODUCED ACCORDING TO
      !!              NURTRIENTS AVAILABLE AND GROWTH CONDITIONS. NUTRIENT UPTAKE
      !!              KINETICS FOLLOW MICHAELIS-MENTON FORMULATION. 
      !!              THE TOTAL PARTICLE AMOUNT PRODUCED, IS DISTRIBUTED IN THE WATER
      !!              COLUMN BELOW THE SURFACE LAYER.
      !!---------------------------------------------------------------------
      !!
      INTEGER, INTENT( in ) ::   kt             ! ocean time-step index      
      INTEGER, INTENT( in ) ::   Kmm, Krhs      ! time level indices
      !!
      INTEGER  ::   ji, jj, jk, jl, ikt
      REAL(wp) ::   zgeolpoc, zfact, zwork, ze3t, zsedpocd, zmaskt
      REAL(wp), DIMENSION(jpi,jpj)   ::  zsedpoca
      CHARACTER (len=25) :: charout
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p2z_exp')
      !
      IF( kt == nittrc000 )   CALL p2z_exp_init( Kmm )

      zsedpoca(:,:) = 0.


      ! VERTICAL DISTRIBUTION OF NEWLY PRODUCED BIOGENIC
      ! POC IN THE WATER COLUMN
      ! (PARTS OF NEWLY FORMED MATTER REMAINING IN THE DIFFERENT
      ! LAYERS IS DETERMINED BY DMIN3 DEFINED IN sms_p2z.F90
      ! ----------------------------------------------------------------------
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         ze3t = 1. / e3t(ji,jj,jk,Kmm)
         tr(ji,jj,jk,jpno3,Krhs) = tr(ji,jj,jk,jpno3,Krhs) + ze3t * dmin3(ji,jj,jk) * xksi(ji,jj)
      END_3D

      ! Find the last level of the water column
      ! Compute fluxes due to sinking particles (slow)
   

      zgeolpoc = 0.e0         !     Initialization
      ! Release of nutrients from the "simple" sediment
      DO_2D( 0, 0, 0, 0 )
         ikt = mbkt(ji,jj) 
         tr(ji,jj,ikt,jpno3,Krhs) = tr(ji,jj,ikt,jpno3,Krhs) + sedlam * sedpocn(ji,jj) / e3t(ji,jj,ikt,Kmm) 
         ! Deposition of organic matter in the sediment
         zwork = vsed * tr(ji,jj,ikt,jpdet,Kmm)
         zsedpoca(ji,jj) = ( zwork + dminl(ji,jj) * xksi(ji,jj)   &
            &           - sedlam * sedpocn(ji,jj) - sedlostpoc * sedpocn(ji,jj) ) * rn_Dt
         zgeolpoc = zgeolpoc + sedlostpoc * sedpocn(ji,jj) * e1e2t(ji,jj)
      END_2D

      DO_2D( 0, 0, 0, 0 )
         tr(ji,jj,1,jpno3,Krhs) = tr(ji,jj,1,jpno3,Krhs) + zgeolpoc * cmask(ji,jj) / areacot / e3t(ji,jj,1,Kmm)
      END_2D

      CALL lbc_lnk( 'p2zexp', sedpocn, 'T', 1.0_wp )
 
      ! Oa & Ek: diagnostics depending on jpdia2d !          left as example
      IF( lk_iomput )  CALL iom_put( "SEDPOC" , sedpocn )

      
      ! Time filter and swap of arrays
      ! ------------------------------
      IF( l_1st_euler ) THEN        ! Euler time-stepping at first time-step
        !                           ! (only swap)
        sedpocn(:,:) = zsedpoca(:,:)
        !                                              
      ELSE
        !
        DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
           zsedpocd = zsedpoca(ji,jj) - 2. * sedpocn(ji,jj) + sedpocb(ji,jj)      ! time laplacian on tracers
           sedpocb(ji,jj) = sedpocn(ji,jj) + rn_atfp * zsedpocd                     ! sedpocb <-- filtered sedpocn
           sedpocn(ji,jj) = zsedpoca(ji,jj)                                       ! sedpocn <-- sedpoca
        END_2D
        ! 
      ENDIF
      !
      IF( lrst_trc ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'p2z_exp : POC in sediment fields written in ocean restart file ',   &
            &                    'at it= ', kt,' date= ', ndastp
         IF(lwp) WRITE(numout,*) '~~~~'
         CALL iom_rstput( kt, nitrst, numrtw, 'SEDB'//ctrcnm(jpdet), sedpocb(:,:) )
         CALL iom_rstput( kt, nitrst, numrtw, 'SEDN'//ctrcnm(jpdet), sedpocn(:,:) )
      ENDIF
      !
      IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('exp')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )  CALL timing_stop('p2z_exp')
      !
   END SUBROUTINE p2z_exp


   SUBROUTINE p2z_exp_init( Kmm )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE p4z_exp_init  ***
      !! ** purpose :   specific initialisation for export
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in)  ::  Kmm      ! time level index
      INTEGER  ::   ji, jj, jk
      REAL(wp) ::   zmaskt, zfluo, zfluu
      REAL(wp), DIMENSION(jpi,jpj    ) :: zrro
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zdm0
      !!---------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) ' p2z_exp: LOBSTER export'
         WRITE(numout,*) ' ~~~~~~~'
         WRITE(numout,*) '  compute remineralisation-damping arrays for tracers'
      ENDIF
      !

      ! Calculate vertical distribution of newly formed biogenic poc
      ! in the water column in the case of max. possible bottom depth
      ! ------------------------------------------------------------
      zdm0 = 0._wp
      zrro = 1._wp
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, jpkb, jpkm1 )
         zfluo = ( gdepw(ji,jj,jk  ,Kmm) / gdepw(ji,jj,jpkb,Kmm) )**xhr
         zfluu = ( gdepw(ji,jj,jk+1,Kmm) / gdepw(ji,jj,jpkb,Kmm) )**xhr
         IF( zfluo.GT.1. )   zfluo = 1._wp
         zdm0(ji,jj,jk) = zfluo - zfluu
         IF( jk <= jpkb-1 )   zdm0(ji,jj,jk) = 0._wp
         zrro(ji,jj) = zrro(ji,jj) - zdm0(ji,jj,jk)
      END_3D
      !
      zdm0(:,:,jpk) = zrro(:,:)

      ! Calculate vertical distribution of newly formed biogenic poc
      ! in the water column with realistic topography (first "dry" layer
      ! contains total fraction, which has passed to the upper layers)
      ! ----------------------------------------------------------------------
      dminl(:,:)   = 0._wp
      dmin3(:,:,:) = zdm0
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpk )
         IF( tmask(ji,jj,jk) == 0._wp ) THEN
            dminl(ji,jj) = dminl(ji,jj) + dmin3(ji,jj,jk)
            dmin3(ji,jj,jk) = 0._wp
         ENDIF
      END_3D

      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         IF( tmask(ji,jj,1) == 0 )   dmin3(ji,jj,1) = 0._wp
      END_2D

      ! Coastal mask 
      cmask(:,:) = 0._wp
      DO_2D( 0, 0, 0, 0 )
         IF( tmask(ji,jj,1) /= 0. ) THEN
            zmaskt = tmask(ji+1,jj,1) * tmask(ji-1,jj,1) * tmask(ji,jj+1,1) * tmask(ji,jj-1,1) 
            IF( zmaskt == 0. )   cmask(ji,jj) = 1._wp
         END IF
      END_2D
      CALL lbc_lnk( 'p2zexp', cmask , 'T', 1.0_wp )      ! lateral boundary conditions on cmask   (sign unchanged)
      areacot = glob_sum( 'p2zexp', e1e2t(:,:) * cmask(:,:) )
      !
      IF( ln_rsttr ) THEN
         CALL iom_get( numrtr, jpdom_auto, 'SEDB'//ctrcnm(jpdet), sedpocb(:,:) )
         CALL iom_get( numrtr, jpdom_auto, 'SEDN'//ctrcnm(jpdet), sedpocn(:,:) )
      ELSE
         sedpocb(:,:) = 0._wp
         sedpocn(:,:) = 0._wp
      ENDIF
      !
   END SUBROUTINE p2z_exp_init

   INTEGER FUNCTION p2z_exp_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p2z_exp_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( cmask(jpi,jpj) , dminl(jpi,jpj) , dmin3(jpi,jpj,jpk), &
         &      sedpocb(jpi,jpj) , sedpocn(jpi,jpj),   STAT=p2z_exp_alloc )
      IF( p2z_exp_alloc /= 0 ) CALL ctl_stop( 'STOP', 'p2z_exp_alloc : failed to allocate arrays.' )
      !
   END FUNCTION p2z_exp_alloc

   !!======================================================================
END MODULE p2zexp
