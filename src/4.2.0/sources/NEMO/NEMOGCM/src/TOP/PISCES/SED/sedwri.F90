MODULE sedwri
   !!======================================================================
   !!                     ***  MODULE  sedwri  ***
   !!         Sediment diagnostics :  write sediment output files
   !!======================================================================
   USE sed
   USE sedarr
   USE lib_mpp         ! distribued memory computing library
   USE iom
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)

   IMPLICIT NONE
   PRIVATE

   !! * Accessibility
   PUBLIC sed_wri 

   !! $Id: sedwri.F90 15450 2021-10-27 14:32:08Z cetlod $
CONTAINS

   !!----------------------------------------------------------------------
   !!                                                   NetCDF output file
   !!----------------------------------------------------------------------
   SUBROUTINE sed_wri( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_wri  ***
      !!
      !! ** Purpose :  output of sediment passive tracer
      !!
      !!   History :
      !!        !  06-07  (C. Ethe)  original
      !!----------------------------------------------------------------------

      INTEGER, INTENT(in) :: kt

      INTEGER  :: ji, jj, jk, js, jw, jn
      INTEGER  :: it
      CHARACTER(len = 20)  ::  cltra 
      REAL(wp)  :: zrate
      REAL(wp), DIMENSION(jpoce, jpksed)     :: zdta
      REAL(wp), DIMENSION(jpoce, jptrased+1) :: zflx
      REAL(wp), DIMENSION(jpi, jpj, jpksed, jptrased)   :: trcsedi
      REAL(wp), DIMENSION(jpi, jpj, jpksed, jpdia3dsed) :: flxsedi3d
      REAL(wp), DIMENSION(jpi, jpj, jpdia2dsed) :: flxsedi2d

      !!-------------------------------------------------------------------


      ! Initialisation
      ! ----------------- 

      ! 1.  Initilisations
      ! -----------------------------------------------------------------
      IF( ln_timing )  CALL timing_start('sed_wri')
!
      IF (lwp) WRITE(numsed,*) ' '
      IF (lwp) WRITE(numsed,*) 'sed_wri kt = ', kt
      IF (lwp) WRITE(numsed,*) ' '
      
      ! Initialize variables
      ! --------------------

      trcsedi(:,:,:,:)   = 0.0
      flxsedi3d(:,:,:,:) = 0.0
      flxsedi2d(:,:,:)   = 0.0

      ! 2.  Back to 2D geometry
      ! -----------------------------------------------------------------
      DO jn = 1, jpsol
         CALL unpack_arr( jpoce, trcsedi(1:jpi,1:jpj,1:jpksed,jn) , iarroce(1:jpoce), &
         &                       solcp(1:jpoce,1:jpksed,jn ) )
      END DO
      
      DO jn = 1, jpwat
         CALL unpack_arr( jpoce, trcsedi(1:jpi,1:jpj,1:jpksed,jpsol + jn) , iarroce(1:jpoce), &
         &                       pwcp(1:jpoce,1:jpksed,jn  )  )
      END DO      

      ! porosity
      zdta(:,:) = 0.
      DO jk = 1, jpksed
         DO ji = 1, jpoce
            zdta(ji,jk) = -LOG10( hipor(ji,jk) / ( densSW(ji) + rtrn ) + rtrn )
         ENDDO
      ENDDO

      CALL unpack_arr( jpoce, flxsedi3d(1:jpi,1:jpj,1:jpksed,1)  , iarroce(1:jpoce), &
         &                   zdta(1:jpoce,1:jpksed)  )
      
      CALL unpack_arr( jpoce, flxsedi3d(1:jpi,1:jpj,1:jpksed,2)  , iarroce(1:jpoce), &
         &                   co3por(1:jpoce,1:jpksed)  )

      CALL unpack_arr( jpoce, flxsedi3d(1:jpi,1:jpj,1:jpksed,3)  , iarroce(1:jpoce), &
         &                   saturco3(1:jpoce,1:jpksed)  )

      
!      flxsedi3d = 0.
      zflx(:,:) = 0.    
      ! Calculation of fluxes mol/cm2/s
      DO jw = 1, jpwat
         DO ji = 1, jpoce
            zflx(ji,jw) = ( pwcp(ji,1,jw) - pwcp_dta(ji,jw) ) &
               &         * 1.e3 * ( 1.e-2 * dzkbot(ji) ) / 1.E4 / rDt_trc
         ENDDO
      ENDDO

      ! Calculation of fluxes g/cm2/s
      DO js = 1, jpsol
         zrate =  1.0 / rDt_trc
         DO ji = 1, jpoce
            zflx(ji,jpwat+js) = zflx(ji,jpwat+js) + ( tosed(ji,js) - fromsed(ji,js) ) * zrate
         ENDDO
      ENDDO

      ! Calculation of accumulation rate per dt
      DO js = 1, jpsol
         zrate =  1.0 / rDt_trc
         DO ji = 1, jpoce
            zflx(ji,jptrased+1) = zflx(ji,jptrased+1) + ( tosed(ji,js) - fromsed(ji,js) ) * zrate
         ENDDO
      ENDDO

      DO jn = 1, jpdia2dsed - 2 
         CALL unpack_arr( jpoce, flxsedi2d(1:jpi,1:jpj,jn), iarroce(1:jpoce), zflx(1:jpoce,jn)  )
      END DO

      zflx(:,1) = dzdep(:) / dtsed
      CALL unpack_arr( jpoce, flxsedi2d(1:jpi,1:jpj,jpdia2dsed-1), iarroce(1:jpoce), zflx(1:jpoce,1) )

      CALL unpack_arr( jpoce, flxsedi2d(1:jpi,1:jpj,jpdia2dsed), iarroce(1:jpoce), rstepros(1:jpoce) )
      !
!      CALL lbc_lnk( 'sedwri', trcsedi(:,:,:,:), 'T', 1._wp )
!      CALL lbc_lnk( 'sedwri', flxsedi3d(:,:,:,:), 'T', 1._wp )
!      CALL lbc_lnk( 'sedwri', flxsedi2d(:,:,:), 'T', 1._wp )

      ! Start writing data
      ! ---------------------
      DO jn = 1, jptrased
         cltra = sedtrcd(jn) ! short title for 3D diagnostic
         CALL iom_put( cltra, trcsedi(:,:,:,jn) )
      END DO

      DO jn = 1, jpdia3dsed
         cltra = seddia3d(jn) ! short title for 3D diagnostic
         CALL iom_put( cltra, flxsedi3d(:,:,:,jn) )
      END DO

      DO jn = 1, jpdia2dsed
         cltra = seddia2d(jn) ! short title for 2D diagnostic
         CALL iom_put( cltra, flxsedi2d(:,:,jn) )
      END DO

      IF( ln_timing )  CALL timing_stop('sed_wri')

   END SUBROUTINE sed_wri

END MODULE sedwri
