MODULE trcwri_pisces
   !!======================================================================
   !!                       *** MODULE trcwri ***
   !!    PISCES :   Output of PISCES tracers
   !!======================================================================
   !! History :   1.0  !  2009-05 (C. Ethe)  Original code
   !!----------------------------------------------------------------------
#if defined key_top && defined key_xios 
   !!----------------------------------------------------------------------
   !! trc_wri_pisces   :  outputs of concentration fields
   !!----------------------------------------------------------------------
   USE trc         ! passive tracers common variables 
   USE sms_pisces  ! PISCES variables
   USE iom         ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC trc_wri_pisces 

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcwri_pisces.F90 15090 2021-07-06 14:25:18Z cetlod $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_wri_pisces( Kmm )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_wri_trc  ***
      !!
      !! ** Purpose :   output passive tracers fields 
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in)          :: Kmm      ! time level indices
      CHARACTER (len=20)           :: cltra
      REAL(wp)                     :: zfact
      INTEGER                      :: ji, jj, jk, jn
      REAL(wp), DIMENSION(jpi,jpj) :: zdic, zo2min, zdepo2min
      !!---------------------------------------------------------------------
 
      ! write the tracer concentrations in the file
      ! ---------------------------------------
      IF( ln_p2z ) THEN
         DO jn = jp_pcs0, jp_pcs1
            cltra = TRIM( ctrcnm(jn) )                  ! short title for tracer
            CALL iom_put( cltra, tr(:,:,:,jn,Kmm) )
         END DO
      ELSE
         DO jn = jp_pcs0, jp_pcs1
            zfact = 1.0e+6 
            IF( jn == jpno3 .OR. jn == jpnh4 ) zfact = rno3 * 1.0e+6 
            IF( jn == jppo4  )                 zfact = po4r * 1.0e+6
            cltra = TRIM( ctrcnm(jn) )                  ! short title for tracer
            IF( iom_use( cltra ) )  CALL iom_put( cltra, tr(:,:,:,jn,Kmm) * zfact )
         END DO

         IF( iom_use( "INTDIC" ) ) THEN                     !   DIC content in kg/m2
            zdic(:,:) = 0.
            DO jk = 1, jpkm1
               zdic(:,:) = zdic(:,:) + tr(:,:,jk,jpdic,Kmm) * e3t(:,:,jk,Kmm) * tmask(:,:,jk) * 12.
            ENDDO
            CALL iom_put( 'INTDIC', zdic )     
         ENDIF
         !
         IF( iom_use( "O2MIN" ) .OR. iom_use ( "ZO2MIN" ) ) THEN  ! Oxygen minimum concentration and depth 
            zo2min   (:,:) = tr(:,:,1,jpoxy,Kmm) * tmask(:,:,1)
            zdepo2min(:,:) = gdepw(:,:,1,Kmm)   * tmask(:,:,1)
            DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 2, jpkm1 ) 
               IF( tmask(ji,jj,jk) == 1 ) then
                  IF( tr(ji,jj,jk,jpoxy,Kmm) < zo2min(ji,jj) ) then
                     zo2min   (ji,jj) = tr(ji,jj,jk,jpoxy,Kmm)
                     zdepo2min(ji,jj) = gdepw(ji,jj,jk,Kmm)
                  ENDIF
               ENDIF
            END_3D
            !
            CALL iom_put('O2MIN' , zo2min     )                              ! oxygen minimum concentration
            CALL iom_put('ZO2MIN', zdepo2min  )                              ! depth of oxygen minimum concentration
             !
         ENDIF
     ENDIF
      !
   END SUBROUTINE trc_wri_pisces

#else
   !!----------------------------------------------------------------------
   !!  Dummy module :                                     No passive tracer
   !!----------------------------------------------------------------------
   PUBLIC trc_wri_pisces
CONTAINS
   SUBROUTINE trc_wri_pisces                     ! Empty routine  
   END SUBROUTINE trc_wri_pisces
#endif

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcwri_pisces.F90 15090 2021-07-06 14:25:18Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!======================================================================
END MODULE trcwri_pisces
