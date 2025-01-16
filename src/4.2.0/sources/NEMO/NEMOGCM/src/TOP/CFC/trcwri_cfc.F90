MODULE trcwri_cfc
   !!======================================================================
   !!                       *** MODULE trcwri ***
   !!    cfc :   Output of cfc tracers
   !!======================================================================
   !! History :   1.0  !  2009-05 (C. Ethe)  Original code
   !!----------------------------------------------------------------------
#if defined key_top && defined key_xios
   !!----------------------------------------------------------------------
   !! trc_wri_cfc   :  outputs of concentration fields
   !!----------------------------------------------------------------------
   USE trc         ! passive tracers common variables 
   USE iom         ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC trc_wri_cfc 

CONTAINS

   SUBROUTINE trc_wri_cfc( Kmm )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_wri_trc  ***
      !!
      !! ** Purpose :   output passive tracers fields 
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in)  :: Kmm   ! time level indices
      CHARACTER (len=20)   :: cltra
      INTEGER              :: jn
      !!---------------------------------------------------------------------
 
      ! write the tracer concentrations in the file
      ! ---------------------------------------
      DO jn = jp_cfc0, jp_cfc1
         cltra = TRIM( ctrcnm(jn) )                  ! short title for tracer
         CALL iom_put( cltra, tr(:,:,:,jn,Kmm) )
      END DO
      !
   END SUBROUTINE trc_wri_cfc

#else
   !!----------------------------------------------------------------------
   !!  Dummy module :                                     No passive tracer
   !!----------------------------------------------------------------------
   PUBLIC trc_wri_cfc
CONTAINS
   SUBROUTINE trc_wri_cfc                     ! Empty routine  
   END SUBROUTINE trc_wri_cfc
#endif

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcwri_cfc.F90 14239 2020-12-23 08:57:16Z smasson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!======================================================================
END MODULE trcwri_cfc
