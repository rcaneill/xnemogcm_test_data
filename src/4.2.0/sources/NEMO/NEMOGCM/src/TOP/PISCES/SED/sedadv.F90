MODULE sedadv
   !!======================================================================
   !!              ***  MODULE  sedadv  ***
   !!    Sediment : vertical advection and burial
   !!=====================================================================
   !! * Modules used
   !!----------------------------------------------------------------------
   !!   sed_adv :
   !!----------------------------------------------------------------------
   USE sed     ! sediment global variable
   USE lib_mpp         ! distribued memory computing library

   IMPLICIT NONE
   PRIVATE

   PUBLIC sed_adv

   REAL(wp) :: cpor
   REAL(wp) :: por1clay 
   REAL(wp) :: eps = 1.e-13

   !! $Id: sedadv.F90 15450 2021-10-27 14:32:08Z cetlod $
CONTAINS

   SUBROUTINE sed_adv( kt )
      !!-------------------------------------------------------------------------
      !!                  ***  ROUTINE sed_adv  ***
      !!
      !! ** Purpose : vertical solid sediment advection and burial 
      !!
      !! ** Method  : At each grid point the 1-dimensional solid sediment column
      !!              is shifted according the rain added to the top layer and
      !!              the gaps produced through redissolution so that in the end
      !!              the original sediment mixed layer geometry is reestablished.
      !!
      !!
      !!   History :
      !!        !  98-08 (E. Maier-Reimer, Christoph Heinze )  Original code
      !!        !  04-10 (N. Emprin, M. Gehlen ) F90
      !!        !  06-04 (C. Ethe)  Re-organization
      !!-------------------------------------------------------------------------
      !!* Arguments
      INTEGER, INTENT(in) ::  &
         kt                     ! time step
      ! * local variables
      INTEGER :: ji, jk, js 
      INTEGER :: jn
      
      REAL(wp), DIMENSION(jpoce,jpksed,jpsol+jpads) :: zsolcp
      REAL(wp) :: solfu, zfilled, zwb, fulsed, uebers, seddef

      !------------------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_adv')
!
      IF( kt == nitsed000 ) THEN
         IF (lwp) THEN
            WRITE(numsed,*) ' '
            WRITE(numsed,*) ' sed_adv : vertical sediment advection  '
            WRITE(numsed,*) ' '
         ENDIF
      ENDIF

      ! Allocation of the temporary arrays
      ! ----------------------------------
      zsolcp(:,:,:) = 0._wp
      DO js = 1, jpsol
         zsolcp(:,:,js) = solcp(:,:,js)
      END DO
      DO jk = 2, jpksed
         zsolcp(:,jk,jpsol+1) = pwcp(:,jk,jwnh4) * adsnh4 
         zsolcp(:,jk,jpsol+2) = pwcp(:,jk,jwfe2) * adsfe2
      END DO

      ! Initialization of data for mass balance calculation
      !---------------------------------------------------
      fromsed(:,:) = 0.
      tosed  (:,:) = 0. 

      solfu = 0.0
      DO jk = 2, jpksed
         solfu = solfu + dz(jk) * por1(jk)
      END DO
      ! Initiate gap 
      !--------------

      ! Initiate burial rates
      !-----------------------
      DO ji = 1, jpoce
         DO jk = 2, jpksed-1
            zfilled = 0._wp
            DO js = 1, jpsol
               zfilled = zfilled + zsolcp(ji,jk,js) / dens_sol(js)
            END DO
            zwb = MAX(0._wp, (zfilled - 1._wp) / (zfilled + rtrn) )


            DO js = 1, jpsol + jpads
               uebers = zwb * zsolcp(ji,jk,js)
               zsolcp(ji,jk,js) = zsolcp(ji,jk,js) - uebers
               zsolcp(ji,jk+1,js) = zsolcp(ji,jk+1,js) + uebers * dz(jk) * por1(jk) / ( dz(jk+1) * por1(jk+1) )
            END DO
         END DO

         zfilled = 0._wp
         DO js = 1, jpsol
            zfilled = zfilled + zsolcp(ji,jpksed,js) / dens_sol(js)
         END DO
         zwb = MAX(0._wp, (zfilled - 1._wp) / (zfilled + rtrn) )
         DO js = 1, jpsol + jpads
            uebers = zwb * zsolcp(ji,jpksed,js)
            zsolcp(ji,jpksed,js) = zsolcp(ji,jpksed,js) - uebers
            tosed(ji,js) = uebers * dz(jpksed) * por1(jpksed)
         END DO
      END DO

      DO ji = 1, jpoce
         fulsed = 0._wp
         DO jk = 2, jpksed 
            zfilled = 0._wp
            DO js = 1, jpsol
               zfilled = zfilled + zsolcp(ji,jk,js) / dens_sol(js)
            END DO
            fulsed = fulsed + zfilled * dz(jk) * por1(jk)
         END DO 

         seddef = solfu - fulsed

         zsolcp(ji,jpksed,jsclay) = zsolcp(ji,jpksed,jsclay) + seddef / ( por1(jpksed) * dz(jpksed) )    &
         &         * dens_sol(jsclay)
         fromsed(ji,jsclay) = seddef * dens_sol(jsclay) 

         DO jk = jpksed, 3, -1
            zfilled = 0._wp
            DO js = 1, jpsol
               zfilled = zfilled + zsolcp(ji,jk,js) / dens_sol(js)
            END DO
            zwb = MAX(0._wp, (zfilled - 1._wp) / (zfilled + rtrn) )
            DO js = 1, jpsol + jpads
               uebers = zwb * zsolcp(ji,jk,js)
               zsolcp(ji,jk,js) = zsolcp(ji,jk,js) - uebers
               zsolcp(ji,jk-1,js) = zsolcp(ji,jk-1,js) + uebers * dz(jk) * por1(jk) / ( dz(jk-1) * por1(jk-1) )
            END DO
         END DO

      END DO

      DO js = 1, jpsol
         solcp(:,:,js) = zsolcp(:,:,js)
      END DO
      DO jk = 2, jpksed
         pwcp(:,jk,jwnh4) = (pwcp(:,jk,jwnh4) + zsolcp(:,jk,jpsol+1) * por1(jk) / por(jk) ) * radssol(jk,jwnh4)
         IF (jpoce == 146 .and. slatit(145) > 0.) write(0,*) 'plante advection ',pwcp(145,jk,jwfe2)*1E6,zsolcp(145,jk,jpsol+2)*1E6,    &
         &         (pwcp(145,jk,jwfe2) + zsolcp(145,jk,jpsol+2) * por1(jk) / por(jk) ) * radssol(jk,jwfe2) * 1E6
         pwcp(:,jk,jwfe2) = (pwcp(:,jk,jwfe2) + zsolcp(:,jk,jpsol+2) * por1(jk) / por(jk) ) * radssol(jk,jwfe2)
      END DO

      rainrg(:,:) = 0.

      IF( ln_timing )  CALL timing_stop('sed_adv')

   END SUBROUTINE sed_adv


END MODULE sedadv
