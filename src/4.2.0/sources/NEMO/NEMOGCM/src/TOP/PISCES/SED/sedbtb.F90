MODULE sedbtb
   !!======================================================================
   !!              ***  MODULE  sedbtb  ***
   !!    Sediment : bioturbation of the solid components
   !!=====================================================================
   !! * Modules used
   USE sed     ! sediment global variable
   USE sed_oce
   USE sedmat  ! linear system of equations
   USE sedinorg
   USE sedorg
   USE sedini
   USE lib_mpp         ! distribued memory computing library

   IMPLICIT NONE
   PRIVATE

   PUBLIC sed_btb


   !! $Id: sedbtb.F90 15450 2021-10-27 14:32:08Z cetlod $
CONTAINS
   
   SUBROUTINE sed_btb( kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE sed_btb  ***
      !!
      !! ** Purpose :  performs bioturbation of the solid sediment components
      !!
      !! ** Method  :  ``diffusion'' of solid sediment components. 
      !!
      !!   History :
      !!        !  98-08 (E. Maier-Reimer, Christoph Heinze )  Original code
      !!        !  04-10 (N. Emprin, M. Gehlen ) F90
      !!        !  06-04 (C. Ethe)  Re-organization
      !!----------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT(in)  :: kt   ! time step
      ! * local variables
      INTEGER :: ji, jk, js
      REAL(wp), DIMENSION(jpoce,jpksed,jpsol) ::  zrearat  !   solution
      REAL(wp) :: zsolid1, zsolid2, zsolid3, zsumtot, zlimo2
      !------------------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_btb')

      IF( kt == nitsed000 ) THEN
         IF (lwp) WRITE(numsed,*) ' sed_btb : bioturbation of solid and adsorbed species  '
         IF (lwp) WRITE(numsed,*) ' '
      ENDIF


      ! Initializations
      !----------------
      zrearat = 0.

      ! Remineralization rates of the different POC pools
      zrearat(:,:,jspoc) = -reac_pocl
      zrearat(:,:,jspos) = -reac_pocs
      zrearat(:,:,jspor) = -reac_pocr

      ! right hand side of coefficient matrix
      !--------------------------------------
      CALL sed_mat_btbi( jpksed, jpsol, solcp, zrearat(:,:,:), dtsed )

      DO ji = 1, jpoce

         zsumtot = 0.
         DO jk = 2, jpksed
            zsolid1 = volc(ji,jk,jspoc) * solcp(ji,jk,jspoc)
            zsolid2 = volc(ji,jk,jspos) * solcp(ji,jk,jspos)
            zsolid3 = volc(ji,jk,jspor) * solcp(ji,jk,jspor)
            rearatpom(ji,jk)  = ( reac_pocl * zsolid1 + reac_pocs * zsolid2 + reac_pocr * zsolid3 )
            zsumtot = zsumtot + rearatpom(ji,jk) * volw3d(ji,jk) * 1.e-3 * 86400. * 365. * 1E3
         END DO

         !    4/ Computation of the bioturbation coefficient
         !       This parameterization is taken from Archer et al. (2002)
         ! --------------------------------------------------------------
         zlimo2   = max(0.01, pwcp(ji,1,jwoxy) / (pwcp(ji,1,jwoxy) + 20.E-6) )
         db(ji,:) = dbiot * zsumtot**0.85 * zlimo2 / (365.0 * 86400.0)

         ! ------------------------------------------------------
         !    Vertical variations of the bioturbation coefficient
         ! ------------------------------------------------------
         IF (ln_btbz) THEN
            DO jk = 1, jpksed
                  db(ji,jk) = db(ji,jk) * exp( -(profsedw(jk) / dbtbzsc)**2 )
            END DO
         ELSE
            DO jk = 1, jpksed
               IF (profsedw(jk) > dbtbzsc) THEN
                  db(ji,jk) = 0.0
               ENDIF
            END DO
         ENDIF

         ! Computation of the bioirrigation factor (from Archer, MUDS model)
         irrig(ji,:) = 0.0
         IF (ln_irrig) THEN
            DO jk = 1, jpksed
               irrig(ji,jk) = ( 7.63752 - 7.4465 * exp( -0.89603 * zsumtot ) ) * zlimo2
               irrig(ji,jk) = irrig(ji,jk) * exp( -(profsedw(jk) / xirrzsc) )
            END DO
         ENDIF
      END DO
 
      ! CALL inorganic and organic slow redow/chemistry processes
      ! ---------------------------------------------------------
      CALL sed_inorg( kt )

      IF( ln_timing )  CALL timing_stop('sed_btb')

   END SUBROUTINE sed_btb

END MODULE sedbtb
