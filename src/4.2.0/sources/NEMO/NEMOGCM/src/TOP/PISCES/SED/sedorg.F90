MODULE sedorg
   !!======================================================================
   !!              ***  MODULE  sedinorg  ***
   !!    Sediment : dissolution and reaction in pore water of 
   !!               inorganic species
   !!=====================================================================
   !! * Modules used
   USE sed     ! sediment global variable
   USE sed_oce
   USE sedini
   USE sedmat
   USE lib_mpp         ! distribued memory computing library
   USE lib_fortran

   IMPLICIT NONE
   PRIVATE

   PUBLIC sed_org

   !! $Id: seddsr.F90 5215 2015-04-15 16:11:56Z nicolasmartin $
CONTAINS
   
   SUBROUTINE sed_org( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_org  ***
      !! 
      !!  ** Purpose :  computes pore water dissolution and reaction
      !!
      !!  ** Methode :  implicit simultaneous computation of undersaturation
      !!               resulting from diffusive pore water transport and chemical
      !!               pore water reactions. Solid material is consumed according
      !!               to redissolution and remineralisation
      !!
      !!  ** Remarks :
      !!              - undersaturation : deviation from saturation concentration
      !!              - reaction rate   : sink of undersaturation from dissolution
      !!                                 of solid material 
      !!
      !!   History :
      !!        !  98-08 (E. Maier-Reimer, Christoph Heinze )  Original code
      !!        !  04-10 (N. Emprin, M. Gehlen ) f90
      !!        !  06-04 (C. Ethe)  Re-organization
      !!        !  19-08 (O. Aumont) Debugging and improvement of the model
      !!----------------------------------------------------------------------
      !! Arguments
      INTEGER, INTENT(in)  :: kt   ! time step
      ! --- local variables
      REAL(wp), DIMENSION(jpoce, jpksed) :: psms, preac
      !!
      !!----------------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_org')

      IF( kt == nitsed000 ) THEN
         IF (lwp) WRITE(numsed,*) ' sed_org : solute species which do not experience redox reactions '
         IF (lwp) WRITE(numsed,*) ' '
      ENDIF
!
      ! DIC in pore water
      preac(:,:) = 0.0_wp
      psms (:,:) = rearatpom(:,:)
      CALL sed_mat_dsri( jpksed, jwdic, preac, psms, dtsed, pwcp(:,:,jwdic) )
      
      ! Silicate in pore water
      psms (:,:) = 0.0_wp
      CALL sed_mat_dsri( jpksed, jwsil, preac, psms, dtsed, pwcp(:,:,jwsil) )

      ! Iron ligands in pore water
      psms (:,:) = ratligc * rearatpom(:,:)
      preac(:,:) = -reac_ligc
      CALL sed_mat_dsri( jpksed, jwlgw, preac, psms, dtsed, pwcp(:,:,jwlgw) )

      IF( ln_timing )  CALL timing_stop('sed_org')
!      
   END SUBROUTINE sed_org

END MODULE sedorg
