MODULE p2zsed
   !!======================================================================
   !!                         ***  MODULE p2zsed  ***
   !! TOP :   PISCES Compute loss of organic matter in the sediments
   !!======================================================================
   !! History :    -   !  1995-06 (M. Levy)  original code
   !!              -   !  2000-12 (E. Kestenare)  clean up
   !!             2.0  !  2007-12  (C. Deltel, G. Madec)  F90 + simplifications
   !!----------------------------------------------------------------------
   !!   p2z_sed        :  Compute loss of organic matter in the sediments
   !!----------------------------------------------------------------------
   USE oce_trc         !
   USE trd_oce         !
   USE trdtrc          !
   USE trc             !
   USE sms_pisces      !
   !
   USE lbclnk          !
   USE iom             !
   USE prtctl          ! Print control for debbuging

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p2z_sed         ! called in ???
   PUBLIC   p2z_sed_init    ! called in ???

   REAL(wp), PUBLIC ::   sedlam      !: time coefficient of POC remineralization in sediments
   REAL(wp), PUBLIC ::   sedlostpoc  !: mass of POC lost in sediments 
   REAL(wp), PUBLIC ::   vsed        !: detritus sedimentation speed [m/s] 
   REAL(wp), PUBLIC ::   xhr         !: coeff for martin''s remineralisation profile

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p2zsed.F90 15090 2021-07-06 14:25:18Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p2z_sed( kt, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p2z_sed  ***
      !!
      !! ** Purpose :   compute the now trend due to the vertical sedimentation of
      !!              detritus and add it to the general trend of detritus equations
      !!
      !! ** Method  :   this ROUTINE compute not exactly the advection but the
      !!              transport term, i.e.  dz(wt) and dz(ws)., dz(wtr)
      !!              using an upstream scheme
      !!              the now vertical advection of tracers is given by:
      !!                      dz(tr(:,:,:,:,Kmm) ww) = 1/bt dk+1( e1t e2t vsed (tr(:,:,:,:,Kmm)) )
      !!              add this trend now to the general trend of tracer (ta,sa,tr(:,:,:,:,Krhs)):
      !!                             tr(:,:,:,:,Krhs) = tr(:,:,:,:,Krhs) + dz(tr(:,:,:,:,Kmm) ww)
      !!        
      !!              IF 'key_diabio' is defined, the now vertical advection
      !!              trend of passive tracers is saved for futher diagnostics.
      !!---------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt         ! ocean time-step index      
      INTEGER, INTENT( in ) ::   Kmm, Krhs  ! time level indices
      !
      INTEGER  ::   ji, jj, jk, jl, ierr
      CHARACTER (len=25) :: charout
      REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: zw2d
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zwork, ztra
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p2z_sed')
      !
      IF( kt == nittrc000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) ' p2z_sed: LOBSTER sedimentation'
         IF(lwp) WRITE(numout,*) ' ~~~~~~~'
      ENDIF

      ! sedimentation of detritus  : upstream scheme
      ! --------------------------------------------

      ! for detritus sedimentation only - jpdet
      zwork(:,:,1  ) = 0.e0      ! surface value set to zero
      zwork(:,:,jpk) = 0.e0      ! bottom value  set to zero

      ! tracer flux at w-point: we use -vsed (downward flux)  with simplification : no e1*e2
      DO jk = 2, jpkm1
         zwork(:,:,jk) = -vsed * tr(:,:,jk-1,jpdet,Kmm)
      END DO

      ! tracer flux divergence at t-point added to the general trend
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 ) 
         ztra(ji,jj,jk)  = - ( zwork(ji,jj,jk) - zwork(ji,jj,jk+1) ) / e3t(ji,jj,jk,Kmm)
         tr(ji,jj,jk,jpdet,Krhs) = tr(ji,jj,jk,jpdet,Krhs) + ztra(ji,jj,jk) 
      END_3D

      IF( lk_iomput )  THEN
         IF( iom_use( "TDETSED" ) ) THEN
            ALLOCATE( zw2d(jpi,jpj) )
            zw2d(:,:) =  ztra(:,:,1) * e3t(:,:,1,Kmm) * 86400._wp
            DO jk = 2, jpkm1
               zw2d(:,:) = zw2d(:,:) + ztra(:,:,jk) * e3t(:,:,jk,Kmm) * 86400._wp
            END DO
            CALL iom_put( "TDETSED", zw2d )
            DEALLOCATE( zw2d )
         ENDIF
      ENDIF
      !

      IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('sed')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p2z_sed')
      !
   END SUBROUTINE p2z_sed


   SUBROUTINE p2z_sed_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p2z_sed_init  ***
      !!
      !! ** Purpose :   Parameters from aphotic layers to sediment
      !!
      !! ** Method  :   Read the namlobsed namelist and check the parameters
      !!
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer
      !!
      NAMELIST/namlobsed/ sedlam, sedlostpoc, vsed, xhr
      !!----------------------------------------------------------------------
      !
      READ  ( numnatp_ref, namlobsed, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namlosed in reference namelist' )
      READ  ( numnatp_cfg, namlobsed, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namlobsed in configuration namelist' )
      IF(lwm) WRITE ( numonp, namlobsed )
      !
      IF(lwp) THEN
          WRITE(numout,*) '   Namelist namlobsed'
          WRITE(numout,*) '      time coeff of POC in sediments                sedlam    =', sedlam
          WRITE(numout,*) '      Sediment geol loss for POC                    sedlostpoc=', sedlostpoc
          WRITE(numout,*) '      detritus sedimentation speed                  vsed      =', 86400 * vsed  , ' d'
          WRITE(numout,*) '      coeff for martin''s remineralistion           xhr       =', xhr
          WRITE(numout,*) ' '
      ENDIF
      !
   END SUBROUTINE p2z_sed_init

   !!======================================================================
END MODULE p2zsed
