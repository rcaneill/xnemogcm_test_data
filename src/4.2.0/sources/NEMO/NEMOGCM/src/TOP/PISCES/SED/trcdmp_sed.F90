MODULE trcdmp_sed
   !!======================================================================
   !!                       ***  MODULE  trcdmp  ***
   !! Ocean physics: internal restoring trend on passive tracers
   !!======================================================================
   !! History :  OPA  !  1991-03  (O. Marti, G. Madec)  Original code
   !!                 !  1996-01  (G. Madec) statement function for e3
   !!                 !  1997-05  (H. Loukos)  adapted for passive tracers
   !!    NEMO    9.0  !  2004-03  (C. Ethe)    free form + modules
   !!            3.2  !  2007-02  (C. Deltel)  Diagnose ML trends for passive tracers
   !!            3.3  !  2010-06  (C. Ethe, G. Madec) merge TRA-TRC 
   !!----------------------------------------------------------------------
#if  defined key_top
   !!----------------------------------------------------------------------
   !!   trc_dmp      : update the tracer trend with the internal damping
   !!   trc_dmp_init : initialization, namlist read, parameters control
   !!----------------------------------------------------------------------
   USE oce_trc         ! ocean dynamics and tracers variables
   USE trc             ! ocean passive tracers variables
   USE sed , ONLY : dtsed => dtsed      ! ocean dynamics and tracers variables
   USE trc             ! ocean passive tracers variables
   USE trcdta
   USE prtctl          ! Print control for debbuging
   USE iom


   IMPLICIT NONE
   PRIVATE

   PUBLIC trc_dmp_sed      
   PUBLIC trc_dmp_sed_alloc  
   PUBLIC trc_dmp_sed_ini    

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   restosed   ! restoring coeff. on tracers (s-1)

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: trcdmp.F90 7646 2017-02-06 09:25:03Z timgraham $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION trc_dmp_sed_alloc()
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trc_dmp_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( restosed(jpi,jpj,jpk) , STAT=trc_dmp_sed_alloc )
      !
      IF( trc_dmp_sed_alloc /= 0 )   CALL ctl_warn('trc_dmp_sed_alloc: failed to allocate array')
      !
   END FUNCTION trc_dmp_sed_alloc


   SUBROUTINE trc_dmp_sed( kt, Kbb, Kmm, Krhs )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trc_dmp_sed  ***
      !!                  
      !! ** Purpose :   Compute the passive tracer trend due to a newtonian damping
      !!      of the tracer field towards given data field and add it to the
      !!      general tracer trends.
      !!
      !! ** Method  :   Newtonian damping towards trdta computed 
      !!      and add to the general tracer trends:
      !!                     tr(Kmm) = tr(Krhs) + restotr * (trdta - tr(Kbb))
      !!         The trend is computed either throughout the water column
      !!      (nlmdmptr=0) or in area of weak vertical mixing (nlmdmptr=1) or
      !!      below the well mixed layer (nlmdmptr=2)
      !!
      !! ** Action  : - update the tracer trends tr(Krhs) with the newtonian 
      !!                damping trends.
      !!              - save the trends ('key_trdmxl_trc')
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt              ! ocean time-step index
      INTEGER, INTENT(in) ::   Kbb, Kmm, Krhs  ! time level index
      !
      INTEGER ::   ji, jj, jk, jn, jl, ikt   ! dummy loop indices
      CHARACTER (len=22) ::   charout
      REAL(wp), DIMENSION(jpi,jpj,jpk) ::   ztrcdta   ! 3D  workspace
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )  CALL timing_start('trc_dmp_sed')
      !
      !
      IF( nb_trcdta > 0 ) THEN  ! Initialisation of tracer from a file that may also be used for damping
         !
         DO jn = 1, jptra                                           ! tracer loop
            !                                                       ! ===========
            IF( ln_trc_ini(jn) ) THEN      ! update passive tracers arrays with input data read from file
               !
               jl = n_trc_index(jn) 
               CALL trc_dta( kt, jl, ztrcdta )   ! read tracer data at nit000
               !
               DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
                  ikt = mbkt(ji,jj)
                  tr(ji,jj,ikt,jn,Kbb) = ztrcdta(ji,jj,ikt) + ( tr(ji,jj,ikt,jn,Kbb) -  ztrcdta(ji,jj,ikt) )     &
                  &                  * exp( -restosed(ji,jj,ikt) * dtsed )
               END_2D
               ! 
            ENDIF
         END DO                                                     ! tracer loop
         !                                                          ! ===========
      ENDIF
      !
      !                                          ! print mean trends (used for debugging)
      IF( sn_cfctl%l_prttrc ) THEN
         WRITE(charout, FMT="('dmp ')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl( tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm,clinfo3='trd' )
      ENDIF
      !
      IF( ln_timing )  CALL timing_stop('trc_dmp_sed')
      !
   END SUBROUTINE trc_dmp_sed


   SUBROUTINE trc_dmp_sed_ini
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trc_dmp_ini  ***
      !! 
      !! ** Purpose :   Initialization for the newtonian damping 
      !!
      !! ** Method  :   read the nammbf namelist and check the parameters
      !!              called by trc_dmp at the first timestep (nittrc000)
      !!----------------------------------------------------------------------
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )  CALL timing_start('trc_dmp_sed_ini')

      IF (lwp) WRITE(numout,*) '   tracer damping throughout the water column'
      !
      IF( trc_dmp_sed_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'trc_dmp_sed_ini: unable to allocate arrays' )
      !
      IF( .NOT.ln_c1d ) THEN
         !Read in mask from file
          restosed(:,:,:) = 0.5 / rday
         !
      ENDIF
      IF( ln_timing )  CALL timing_stop('trc_dmp_sed_ini')
     !
   END SUBROUTINE trc_dmp_sed_ini

#else
   !!----------------------------------------------------------------------
   !!  Dummy module :                                     No passive tracer
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_dmp_sed( kt, Kbb, Kmm, Krhs )   ! Empty routine
      INTEGER, INTENT(in) :: kt
      INTEGER, INTENT(in) :: Kbb, Kmm, Krhs
      WRITE(*,*) 'trc_dmp_sed: You should not have seen this print! error?', kt
   END SUBROUTINE trc_dmp_sed
#endif

   !!======================================================================
END MODULE trcdmp_sed
