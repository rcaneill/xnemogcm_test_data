MODULE diacfl
   !!======================================================================
   !!                       ***  MODULE  diacfl  ***
   !! Output CFL diagnostics to ascii file
   !!======================================================================
   !! History :  3.4  !  2010-03  (E. Blockley)  Original code
   !!            3.6  !  2014-06  (T. Graham)  Removed CPP key & Updated to vn3.6
   !!            4.0  !  2017-09  (G. Madec)  style + comments
   !!----------------------------------------------------------------------
   !!   dia_cfl        : Compute and output Courant numbers at each timestep
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and active tracers
   USE dom_oce         ! ocean space and time domain
   USE domvvl          ! 
   !
   USE lib_mpp         ! distribued memory computing
   USE lbclnk          ! ocean lateral boundary condition (or mpp link)
   USE in_out_manager  ! I/O manager
   USE iom             ! 
   USE timing          ! Performance output

   IMPLICIT NONE
   PRIVATE

   CHARACTER(LEN=50) :: clname="cfl_diagnostics.ascii"    ! ascii filename
   INTEGER           :: numcfl                            ! outfile unit
   !
   INTEGER, DIMENSION(3) ::   nCu_loc, nCv_loc, nCw_loc   ! U, V, and W run max locations in the global domain
   REAL(wp)              ::   rCu_max, rCv_max, rCw_max   ! associated run max Courant number 

   PUBLIC   dia_cfl       ! routine called by step.F90
   PUBLIC   dia_cfl_init  ! routine called by nemogcm

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: diacfl.F90 14433 2021-02-11 08:06:49Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dia_cfl ( kt, Kmm )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dia_cfl  ***
      !!
      !! ** Purpose :  Compute the Courant numbers Cu=u*dt/dx and Cv=v*dt/dy
      !!               and output to ascii file 'cfl_diagnostics.ascii'
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time-step index
      INTEGER, INTENT(in) ::   Kmm  ! ocean time level index
      !
      INTEGER                          ::   ji, jj, jk                       ! dummy loop indices
      REAL(wp)                         ::   zCu_max, zCv_max, zCw_max        ! local scalars
      INTEGER , DIMENSION(3)           ::   iloc_u , iloc_v , iloc_w , iloc  ! workspace
      REAL(wp), DIMENSION(jpi,jpj,jpk) ::   zCu_cfl, zCv_cfl, zCw_cfl        ! workspace
      LOGICAL , DIMENSION(jpi,jpj,jpk) ::   llmsk
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('dia_cfl')
      !
      llmsk(     1:nn_hls,:,:) = .FALSE.   ! exclude halos from the checked region
      llmsk(Nie0+1:   jpi,:,:) = .FALSE.
      llmsk(:,     1:nn_hls,:) = .FALSE.
      llmsk(:,Nje0+1:   jpj,:) = .FALSE.
      !
      DO_3D( 0, 0, 0, 0, 1, jpk )      ! calculate Courant numbers
         zCu_cfl(ji,jj,jk) = ABS( uu(ji,jj,jk,Kmm) ) * rDt / e1u  (ji,jj)      ! for i-direction
         zCv_cfl(ji,jj,jk) = ABS( vv(ji,jj,jk,Kmm) ) * rDt / e2v  (ji,jj)      ! for j-direction
         zCw_cfl(ji,jj,jk) = ABS( ww(ji,jj,jk) ) * rDt / e3w(ji,jj,jk,Kmm)     ! for k-direction
      END_3D
      !
      ! write outputs
      IF( iom_use('cfl_cu') ) THEN
         llmsk(Nis0:Nie0,Njs0:Nje0,:) = umask(Nis0:Nie0,Njs0:Nje0,:) == 1._wp        ! define only the inner domain
         CALL iom_put( 'cfl_cu', MAXVAL( zCu_cfl, mask = llmsk, dim=3 ) )
      ENDIF
      IF( iom_use('cfl_cv') ) THEN
         llmsk(Nis0:Nie0,Njs0:Nje0,:) = vmask(Nis0:Nie0,Njs0:Nje0,:) == 1._wp        ! define only the inner domain
         CALL iom_put( 'cfl_cv', MAXVAL( zCv_cfl, mask = llmsk, dim=3 ) )
      ENDIF
      IF( iom_use('cfl_cw') ) THEN
         llmsk(Nis0:Nie0,Njs0:Nje0,:) = wmask(Nis0:Nie0,Njs0:Nje0,:) == 1._wp        ! define only the inner domain
         CALL iom_put( 'cfl_cw', MAXVAL( zCw_cfl, mask = llmsk, dim=3 ) )
      ENDIF

      !                    ! calculate maximum values and locations
      llmsk(Nis0:Nie0,Njs0:Nje0,:) = umask(Nis0:Nie0,Njs0:Nje0,:) == 1._wp        ! define only the inner domain
      CALL mpp_maxloc( 'diacfl', zCu_cfl, llmsk, zCu_max, iloc_u )
      llmsk(Nis0:Nie0,Njs0:Nje0,:) = vmask(Nis0:Nie0,Njs0:Nje0,:) == 1._wp        ! define only the inner domain
      CALL mpp_maxloc( 'diacfl', zCv_cfl, llmsk, zCv_max, iloc_v )
      llmsk(Nis0:Nie0,Njs0:Nje0,:) = wmask(Nis0:Nie0,Njs0:Nje0,:) == 1._wp        ! define only the inner domain
      CALL mpp_maxloc( 'diacfl', zCw_cfl, llmsk, zCw_max, iloc_w )
      !
      IF( lwp ) THEN       ! write out to file
         WRITE(numcfl,FMT='(2x,i6,3x,a6,4x,f7.4,1x,i4,1x,i4,1x,i4)') kt, 'Max Cu', zCu_max, iloc_u(1), iloc_u(2), iloc_u(3)
         WRITE(numcfl,FMT='(11x,     a6,4x,f7.4,1x,i4,1x,i4,1x,i4)')     'Max Cv', zCv_max, iloc_v(1), iloc_v(2), iloc_v(3)
         WRITE(numcfl,FMT='(11x,     a6,4x,f7.4,1x,i4,1x,i4,1x,i4)')     'Max Cw', zCw_max, iloc_w(1), iloc_w(2), iloc_w(3)
      ENDIF
      !
      !                    ! update maximum Courant numbers from whole run if applicable
      IF( zCu_max > rCu_max ) THEN   ;   rCu_max = zCu_max   ;   nCu_loc(:) = iloc_u(:)   ;   ENDIF
      IF( zCv_max > rCv_max ) THEN   ;   rCv_max = zCv_max   ;   nCv_loc(:) = iloc_v(:)   ;   ENDIF
      IF( zCw_max > rCw_max ) THEN   ;   rCw_max = zCw_max   ;   nCw_loc(:) = iloc_w(:)   ;   ENDIF

      !                    ! at end of run output max Cu and Cv and close ascii file
      IF( kt == nitend .AND. lwp ) THEN
         ! to ascii file
         WRITE(numcfl,*) '******************************************'
         WRITE(numcfl,FMT='(3x,a12,6x,f7.4,1x,i4,1x,i4,1x,i4)') 'Run Max Cu', rCu_max, nCu_loc(1), nCu_loc(2), nCu_loc(3)
         WRITE(numcfl,FMT='(3x,a8,11x,f15.1)') ' => dt/C', rDt/rCu_max
         WRITE(numcfl,*) '******************************************'
         WRITE(numcfl,FMT='(3x,a12,6x,f7.4,1x,i4,1x,i4,1x,i4)') 'Run Max Cv', rCv_max, nCv_loc(1), nCv_loc(2), nCv_loc(3)
         WRITE(numcfl,FMT='(3x,a8,11x,f15.1)') ' => dt/C', rDt/rCv_max
         WRITE(numcfl,*) '******************************************'
         WRITE(numcfl,FMT='(3x,a12,6x,f7.4,1x,i4,1x,i4,1x,i4)') 'Run Max Cw', rCw_max, nCw_loc(1), nCw_loc(2), nCw_loc(3)
         WRITE(numcfl,FMT='(3x,a8,11x,f15.1)') ' => dt/C', rDt/rCw_max
         CLOSE( numcfl ) 
         !
         ! to ocean output
         WRITE(numout,*)
         WRITE(numout,*) 'dia_cfl : Maximum Courant number information for the run '
         WRITE(numout,*) '~~~~~~~'
         WRITE(numout,*) '   Max Cu = ', rCu_max, ' at (i,j,k) = (',nCu_loc(1),nCu_loc(2),nCu_loc(3),') => dt/C = ', rDt/rCu_max
         WRITE(numout,*) '   Max Cv = ', rCv_max, ' at (i,j,k) = (',nCv_loc(1),nCv_loc(2),nCv_loc(3),') => dt/C = ', rDt/rCv_max
         WRITE(numout,*) '   Max Cw = ', rCw_max, ' at (i,j,k) = (',nCw_loc(1),nCw_loc(2),nCw_loc(3),') => dt/C = ', rDt/rCw_max
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('dia_cfl')
      !
   END SUBROUTINE dia_cfl


   SUBROUTINE dia_cfl_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dia_cfl_init  ***
      !!                   
      !! ** Purpose :   create output file, initialise arrays
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'dia_cfl : Outputting CFL diagnostics to ',TRIM(clname), ' file'
         WRITE(numout,*) '~~~~~~~'
         WRITE(numout,*)
         !
         ! create output ascii file
         CALL ctl_opn( numcfl, clname, 'UNKNOWN', 'FORMATTED', 'SEQUENTIAL', 1, numout, lwp, 1 )
         WRITE(numcfl,*) 'Timestep  Direction  Max C     i    j    k'
         WRITE(numcfl,*) '******************************************'
      ENDIF
      !
      rCu_max = 0._wp
      rCv_max = 0._wp
      rCw_max = 0._wp
      !
   END SUBROUTINE dia_cfl_init

   !!======================================================================
END MODULE diacfl
