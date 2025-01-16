MODULE diu_layers
   !!======================================================================
   !!                       ***  MODULE diu_layers **
   !! Apply coolskin and warm layer calculations
   !!======================================================================
   !! History :  3.7  ! 2015-11  (J. While)  Original code

   USE diu_bulk     ! diurnal SST bulk routines  (diurnal_sst_takaya routine) 
   USE diu_coolskin ! diurnal cool skin correction (diurnal_sst_coolskin routine)   
   USE oce
   USE iom
   USE sbc_oce
   USE sbcmod       ! surface boundary condition       (sbc     routine)
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC diurnal_sst_bulk_init      ! called by nemogcm.F90
   PUBLIC diurnal_sst_coolskin_init  ! called by nemogcm.F90
   PUBLIC diurnal_layers             ! called by step.F90 or step_diu.F90

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: step_diu.F90 10922 2019-05-02 15:10:39Z acc $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

   CONTAINS

   SUBROUTINE diurnal_layers( kstp ) 
      INTEGER, INTENT(in) ::   kstp   ! ocean time-step index 
      !!---------------------------------------------------------------------- 
      !!                     ***  ROUTINE diurnal_layers  *** 
      !!                       
      !! ** Purpose : - Apply coolskin and warm layer calculations
      !!   
      !!---------------------------------------------------------------------- 
      
      ! Cool skin

      CALL diurnal_sst_coolskin_step( qns, taum, rhop(:,:,1), rn_Dt)

      CALL iom_put( "sst_wl"   , x_dsst               )    ! warm layer (write out before update below).
      CALL iom_put( "sst_cs"   , x_csdsst             )    ! cool skin

      ! Diurnal warm layer model       
      CALL diurnal_sst_takaya_step( kstp, qsr, qns, taum, rhop(:,:,1), rn_Dt) 

   END SUBROUTINE diurnal_layers  
   
END MODULE diu_layers
