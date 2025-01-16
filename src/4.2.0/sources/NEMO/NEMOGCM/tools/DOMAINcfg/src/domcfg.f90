MODULE domcfg
   !!==============================================================================
   !!                       ***  MODULE domcfg   ***
   !! Ocean initialization : domain configuration initialization
   !!==============================================================================
   !! History :  1.0  ! 2003-09  (G. Madec)  Original code
   !!            3.2  ! 2009-07  (R. Benshila) Suppression of rigid-lid option
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dom_cfg        : initialize the domain configuration
   !!----------------------------------------------------------------------
   USE dom_oce         ! ocean space and time domain
   USE phycst          ! physical constants
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! distributed memory computing library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dom_cfg    ! called by opa.F90

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.2 , LODYC-IPSL  (2009)
   !! $Id: domcfg.F90 6140 2015-12-21 11:35:23Z timgraham $ 
   !! Software governed by the CeCILL licence     (./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dom_cfg
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dom_cfg  ***
      !!                    
      !! ** Purpose :   set the domain configuration
      !!
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN                   ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'dom_cfg : set the ocean configuration'
         WRITE(numout,*) '~~~~~~~ '
         WRITE(numout,*) '   ocean model configuration used :   cp_cfg = ', cp_cfg, ' jp_cfg = ', jp_cfg
         !
         WRITE(numout,*) '   global domain lateral boundaries'
         !
         IF( jperio == 0 )   WRITE(numout,*) '      jperio= 0, closed'
         IF( jperio == 1 )   WRITE(numout,*) '      jperio= 1, cyclic east-west'
         IF( jperio == 2 )   WRITE(numout,*) '      jperio= 2, equatorial symmetric'
         IF( jperio == 3 )   WRITE(numout,*) '      jperio= 3, north fold with T-point pivot'
         IF( jperio == 4 )   WRITE(numout,*) '      jperio= 4, cyclic east-west and north fold with T-point pivot'
         IF( jperio == 5 )   WRITE(numout,*) '      jperio= 5, north fold with F-point pivot'
         IF( jperio == 6 )   WRITE(numout,*) '      jperio= 6, cyclic east-west and north fold with F-point pivot'
      ENDIF
      !
      IF( jperio <  0 .OR. jperio > 6 )   CALL ctl_stop( 'jperio is out of range' )
      !
   END SUBROUTINE dom_cfg

   !!======================================================================
END MODULE domcfg
