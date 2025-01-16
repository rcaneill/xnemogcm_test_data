MODULE sedfunc
   !!======================================================================
   !!              ***  MODULE  sedsol  ***
   !!    Sediment : dissolution and reaction in pore water related 
   !!    related to organic matter
   !!    Diffusion of solutes in pore water
   !!=====================================================================
   !! * Modules used
   USE sed     ! sediment global variable
   USE sed_oce
   USE sedini
   USE seddsr
   USE sedmat
   USE lib_mpp         ! distribued memory computing library
   USE lib_fortran

   IMPLICIT NONE
   PRIVATE

   PUBLIC sed_func

   !! * Module variables

   !! $Id: sedsol.F90 5215 2015-04-15 16:11:56Z nicolasmartin $
CONTAINS
   
   SUBROUTINE sed_func(  NEQ, X, fval0, accmask ) 
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_sol  ***
      !! 
      !!  ** Purpose :  computes pore water diffusion and reactions
      !!
      !!  ** Methode :  Computation of the redox and dissolution reactions 
      !!                in the sediment.
      !!                The main redox reactions are solved in sed_dsr whereas
      !!                the secondary reactions are solved in sed_dsr_redoxb.
      !!                Inorganic dissolution is solved in sed_inorg
      !!                A strand spliting approach is being used here (see 
      !!                sed_dsr_redoxb for more information). 
      !!                Diffusive fluxes are computed in sed_diff
      !!
      !!   History :
      !!        !  98-08 (E. Maier-Reimer, Christoph Heinze )  Original code
      !!        !  04-10 (N. Emprin, M. Gehlen ) f90
      !!        !  06-04 (C. Ethe)  Re-organization
      !!        !  19-08 (O. Aumont) Debugging and improvement of the model.
      !!                             The original method is replaced by a 
      !!                             Strand splitting method which deals 
      !!                             well with stiff reactions.
      !!----------------------------------------------------------------------
      !! Arguments
      INTEGER, INTENT(in) :: NEQ
      INTEGER, DIMENSION(jpoce), INTENT(in) :: accmask
      REAL, DIMENSION(jpoce,NEQ), INTENT(in) :: X
      REAL, DIMENSION(jpoce,NEQ), INTENT(out) :: fval0
      ! --- local variables
      INTEGER  :: ji, jk, js, jn   ! dummy looop indices
      !!
      !!----------------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_func')
!
      pwcpa(:,:,:) = 0.
      solcpa(:,:,:) = 0.

      do jn = 1, NEQ
         jk = jarr(jn,1)
         js = jarr(jn,2)
         IF (js <= jpwat) THEN
            pwcp(:,jk,js) = X(:,jn) * 1E-6 
         ELSE
            solcp(:,jk,js-jpwat) = X(:,jn) * 1E-6
         ENDIF
      END DO

      CALL sed_dsr( accmask )        ! Redox reactions
      ! Computes diffusive fluxes
      DO jn = 1, jpvode
         js = jsvode(jn)
         IF (js <= jpwat) CALL sed_mat_dsr( jpksed, js, accmask )
      END DO
      call sed_mat_btb( jpksed, jwnh4, accmask )
      call sed_mat_btb( jpksed, jwfe2, accmask )

      do jn = 1, NEQ
         jk = jarr(jn,1)
         js = jarr(jn,2)
         IF (js <= jpwat) THEN
            fval0(:,jn) = pwcpa(:,jk,js)  * 1E6
         ELSE
            fval0(:,jn) = solcpa(:,jk,js-jpwat) * 1E6
         ENDIF
      END DO

      IF( ln_timing )  CALL timing_stop('sed_func')
!      
   END SUBROUTINE sed_func

END MODULE sedfunc
