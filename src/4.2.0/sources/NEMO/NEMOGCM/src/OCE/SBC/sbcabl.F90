MODULE sbcabl
   !!======================================================================
   !!                       ***  MODULE  sbcabl  ***
   !! Ocean forcing:  momentum, heat and freshwater flux formulation 
   !!                 derived from an ABL model 
   !!=====================================================================
   !! History :  4.0  !  2019-03  (F. Lemarié & G. Samson)  Original code
   !!----------------------------------------------------------------------
   USE sbc_oce, ONLY :   ght_abl, ghw_abl, e3t_abl, e3w_abl
   USE lib_mpp, ONLY :   ctl_stop

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sbc_abl_init       ! routine called in sbcmod module
   PUBLIC   sbc_abl            ! routine called in sbcmod module

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.7 , NEMO-consortium (2014)
   !! $Id: sbcabl.F90 6416 2016-04-01 12:22:17Z clem $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE sbc_abl_init
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE sbc_abl_init  ***
      !!
      !! ** Purposes :   dummy routine for compilation 
      !!
      !!----------------------------------------------------------------------
      CALL ctl_stop( 'STOP', 'ln_abl = .true. but ABL source directory was not included',   &
	     & '(Either switch to ln_abl = .false. or modify your cfg.txt file and recompile)' )
	  !!
   END SUBROUTINE sbc_abl_init

   
   SUBROUTINE sbc_abl( kt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE sbc_abl  ***
      !!
      !! ** Purposes :   dummy routine for compilation 
      !!
      !!---------------------------------------------------------------------
      INTEGER ,         INTENT(in) ::   kt   ! ocean time step
      !!
   END SUBROUTINE sbc_abl


   !!======================================================================
END MODULE sbcabl
