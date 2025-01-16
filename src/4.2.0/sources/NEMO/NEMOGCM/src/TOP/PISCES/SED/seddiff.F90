MODULE seddiff
   !!======================================================================
   !!              ***  MODULE  seddsr  ***
   !!    Sediment : dissolution and reaction in pore water related 
   !!    related to organic matter
   !!=====================================================================
   !! * Modules used
   USE sed     ! sediment global variable
   USE sedmat  ! linear system of equations
   USE sedini
   USE lib_mpp         ! distribued memory computing library
   USE lib_fortran

   IMPLICIT NONE
   PRIVATE

   PUBLIC sed_diff

   !! * Module variables

   !! $Id: seddsr.F90 5215 2015-04-15 16:11:56Z nicolasmartin $
CONTAINS
   
   SUBROUTINE sed_diff( kt, knt ) 
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_diff  ***
      !! 
      !!  ** Purpose :  computes pore water diffusion
      !!
      !!  ** Methode :  implicit computation of undersaturation
      !!               resulting from diffusive pore water transport.
      !!
      !!  ** Remarks :
      !!              - undersaturation : deviation from saturation concentration
      !!   History :
      !!        !  98-08 (E. Maier-Reimer, Christoph Heinze )  Original code
      !!        !  04-10 (N. Emprin, M. Gehlen ) f90
      !!        !  06-04 (C. Ethe)  Re-organization
      !!        !  19-08 (O. Aumont) Debugging and improvement of the model
      !!----------------------------------------------------------------------
      !! Arguments
      INTEGER, INTENT(in) ::   kt, knt       ! number of iteration
!      

   END SUBROUTINE sed_diff

END MODULE seddiff
