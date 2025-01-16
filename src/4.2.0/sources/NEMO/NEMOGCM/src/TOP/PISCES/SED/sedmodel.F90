MODULE sedmodel
   !!======================================================================
   !!                       ***  MODULE sedmodel   ***
   !!   Sediment model : Main routine of sediment model 
   !!======================================================================
   USE sed
   USE sedstp   ! time stepping
   USE sedinitrc

   IMPLICIT NONE
   PRIVATE

   !! * Routine accessibility
   PUBLIC sed_model  ! called by step.F90

CONTAINS

   SUBROUTINE sed_model ( kt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE sed_model  ***
      !!
      !! ** Purpose :   main routine of sediment model
      !!
      !!
      !! ** Method  : - model general initialization
      !!              - launch the time-stepping (stp routine)
      !!
      !!   History :
      !!        !  07-02 (C. Ethe)  Original
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt               ! number of iteration
      INTEGER, INTENT(in) ::   Kbb, Kmm, Krhs   ! time level indices


      IF( ln_timing )  CALL timing_start('sed_model')

      IF( kt == nittrc000 ) CALL sed_initrc( Kbb, Kmm )         ! Initialization of sediment model
                            CALL sed_stp( kt, Kbb, Kmm, Krhs )  ! Time stepping of Sediment model

      IF( ln_timing )  CALL timing_stop('sed_model')

   END SUBROUTINE sed_model

END MODULE sedmodel
