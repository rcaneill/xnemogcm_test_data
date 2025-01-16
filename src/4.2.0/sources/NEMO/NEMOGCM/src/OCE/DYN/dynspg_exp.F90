MODULE dynspg_exp
   !!======================================================================
   !!                   ***  MODULE  dynspg_exp  ***
   !! Ocean dynamics:  surface pressure gradient trend, explicit scheme
   !!======================================================================
   !! History :  2.0  !  2005-11  (V. Garnier, G. Madec, L. Bessieres) Original code
   !!            3.2  !  2009-06  (G. Madec, M. Leclair, R. Benshila) introduce sshwzv module
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dyn_spg_exp  : update the momentum trend with the surface 
   !!                      pressure gradient in the free surface constant  
   !!                      volume case with vector optimization
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers 
   USE dom_oce         ! ocean space and time domain 
   USE sbc_oce         ! surface boundary condition: ocean
   USE phycst          ! physical constants
   !
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! distributed memory computing library
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE prtctl          ! Print control
   USE iom             ! I/O library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dyn_spg_exp   ! called in dynspg.F90 

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: dynspg_exp.F90 14064 2020-12-03 17:01:12Z ayoung $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dyn_spg_exp( kt, Kmm, puu, pvv, Krhs )
      !!----------------------------------------------------------------------
      !!                  ***  routine dyn_spg_exp  ***
      !!
      !! ** Purpose :   Compute the now trend due to the surface pressure
      !!              gradient in case of explicit free surface formulation and 
      !!              add it to the general trend of momentum equation.
      !!
      !! ** Method  :   Explicit free surface formulation. Add to the general
      !!              momentum trend the surface pressure gradient :
      !!                      (uu(rhs),vv(rhs)) = (uu(rhs),vv(rhs)) + (spgu,spgv)
      !!              where spgu = -1/rho0 d/dx(ps) = -g/e1u di( ssh(now) )
      !!                    spgv = -1/rho0 d/dy(ps) = -g/e2v dj( ssh(now) )
      !!
      !! ** Action :   (puu(:,:,:,Krhs),pvv(:,:,:,Krhs))   trend of horizontal velocity increased by 
      !!                         the surf. pressure gradient trend
      !!---------------------------------------------------------------------
      INTEGER                             , INTENT( in )  ::  kt        ! ocean time-step index
      INTEGER                             , INTENT( in )  ::  Kmm, Krhs ! ocean time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpt), INTENT(inout) ::  puu, pvv  ! ocean velocities and RHS of momentum equation
      !!
      INTEGER ::   ji, jj, jk   ! dummy loop indices
      REAL(wp), DIMENSION(jpi,jpj) ::   zpgu, zpgv   ! 2D workspace
      !!----------------------------------------------------------------------
      !
      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'dyn_spg_exp : surface pressure gradient trend'
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~   (explicit free surface)'
         !
         zpgu(:,:) = 0._wp   ;   zpgv(:,:) = 0._wp
         !
         IF( .NOT.ln_linssh .AND. lwp ) WRITE(numout,*) '      non linear free surface: spg is included in dynhpg'
      ENDIF
      !
      DO_2D( 0, 0, 0, 0 )
         zpgu(ji,jj) = - grav * ( ssh(ji+1,jj,Kmm) - ssh(ji,jj,Kmm) ) * r1_e1u(ji,jj)
         zpgv(ji,jj) = - grav * ( ssh(ji,jj+1,Kmm) - ssh(ji,jj,Kmm) ) * r1_e2v(ji,jj)
      END_2D
      !
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         puu(ji,jj,jk,Krhs) = puu(ji,jj,jk,Krhs) + zpgu(ji,jj)
         pvv(ji,jj,jk,Krhs) = pvv(ji,jj,jk,Krhs) + zpgv(ji,jj)
      END_3D
      !
   END SUBROUTINE dyn_spg_exp

   !!======================================================================
END MODULE dynspg_exp
