MODULE step_diu
   !!======================================================================
   !!                       ***  MODULE stp_diu  ***
   !! Time-stepping of diurnal cycle models
   !!======================================================================
   !! History :  3.7  ! 2015-11  (J. While)  Original code

   USE diu_layers      ! diurnal SST bulk and coolskin routines
   USE iom
   USE sbc_oce
   USE sbcmod           ! surface boundary condition       (sbc     routine)
   USE diaobs           ! Observation operator
   USE oce
   USE daymod
   USE restart          ! ocean restart                    (rst_wri routine)
   USE timing           ! Timing
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC   stp_diurnal   ! called by nemogcm.F90 or step.F90

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: step_diu.F90 12377 2020-02-12 14:39:06Z acc $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

   CONTAINS

   SUBROUTINE stp_diurnal( kstp ) 
      INTEGER, INTENT(in) ::   kstp   ! ocean time-step index 
      !!---------------------------------------------------------------------- 
      !!                     ***  ROUTINE stp_diurnal  *** 
      !!                       
      !! ** Purpose : - Time stepping of diurnal SST model only 
      !!   
      !! ** Method  : -1- Update forcings and data   
      !!              -2- Update ocean physics   
      !!              -3- Compute the t and s trends   
      !!              -4- Update t and s   
      !!              -5- Compute the momentum trends 
      !!              -6- Update the horizontal velocity 
      !!              -7- Compute the diagnostics variables (rd,N2, div,cur,w) 
      !!              -8- Outputs and diagnostics 
      !!---------------------------------------------------------------------- 
      INTEGER ::   jk       ! dummy loop indices
      INTEGER ::   indic    ! error indicator if < 0 
      REAL(wp), DIMENSION(jpi,jpj) :: z_fvel_bkginc, z_hflux_bkginc     
      INTEGER :: Nbb, Nnn, Naa, Nrhs    ! local definitions as placeholders for now
      !! --------------------------------------------------------------------- 
      
      IF(ln_diurnal_only) THEN
         indic = 0                                 ! reset to no error condition 
         IF( kstp /= nit000 )   CALL day( kstp )   ! Calendar (day was already called at nit000 in day_init) 
 
         CALL iom_setkt( kstp - nit000 + 1, cxios_context )   ! tell iom we are at time step kstp
         IF( ln_crs ) THEN
            CALL iom_setkt( kstp - nit000 + 1, TRIM(cxios_context)//"_crs" ) ! tell iom we are at time step kstp
         ENDIF
       
            CALL sbc    ( kstp, Nbb, Nnn )            ! Sea Boundary Conditions 
      ENDIF
     
      call diurnal_layers( kstp )                     ! coolskin and warm layer calculations

      IF( ln_diurnal_only ) THEN
         ! WILL HAVE TO INCREMENT Nbb and Nnn here in ln_diurnal_only case !
         IF( ln_diaobs )         CALL dia_obs( kstp, Nnn )    ! obs-minus-model (assimilation) diagnostics (call after dynamics update)
     
         !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
         ! Control and restarts 
         !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
         IF( kstp == nit000   )   CALL iom_close( numror )     ! close input  ocean restart file 
         IF( lrst_oce         )   CALL rst_write    ( kstp, Nbb, Nnn )   ! write output ocean restart file
     
         IF( ln_timing .AND.  kstp == nit000  )   CALL timing_reset 
      ENDIF
       
   END SUBROUTINE stp_diurnal  
   
END MODULE step_diu
