MODULE dynzad
   !!======================================================================
   !!                       ***  MODULE  dynzad  ***
   !! Ocean dynamics : vertical advection trend
   !!======================================================================
   !! History :  OPA  ! 1991-01  (G. Madec) Original code
   !!   NEMO     0.5  ! 2002-07  (G. Madec) Free form, F90
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dyn_zad       : vertical advection momentum trend
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
   USE dom_oce        ! ocean space and time domain
   USE sbc_oce        ! surface boundary condition: ocean
   USE trd_oce        ! trends: ocean variables
   USE trddyn         ! trend manager: dynamics
   USE sbcwave, ONLY: wsd   ! Surface Waves (add vertical Stokes-drift)
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE prtctl         ! Print control
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dyn_zad       ! routine called by dynadv.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: dynzad.F90 14834 2021-05-11 09:24:44Z hadcv $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dyn_zad ( kt, Kmm, puu, pvv, Krhs )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dynzad  ***
      !!
      !! ** Purpose :   Compute the now vertical momentum advection trend and
      !!      add it to the general trend of momentum equation.
      !!
      !! ** Method  :   The now vertical advection of momentum is given by:
      !!         w dz(u) = u(rhs) + 1/(e1e2u*e3u) mk+1[ mi(e1e2t*ww) dk(u) ]
      !!         w dz(v) = v(rhs) + 1/(e1e2v*e3v) mk+1[ mj(e1e2t*ww) dk(v) ]
      !!      Add this trend to the general trend (puu(:,:,:,Krhs),pvv(:,:,:,Krhs)):
      !!         (u(rhs),v(rhs)) = (u(rhs),v(rhs)) + w dz(u,v)
      !!
      !! ** Action  : - Update (puu(:,:,:,Krhs),pvv(:,:,:,Krhs)) with the vert. momentum adv. trends
      !!              - Send the trends to trddyn for diagnostics (l_trddyn=T)
      !!----------------------------------------------------------------------
      INTEGER                             , INTENT( in )  ::  kt               ! ocean time-step inedx
      INTEGER                             , INTENT( in )  ::  Kmm, Krhs        ! ocean time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpt), INTENT(inout) ::  puu, pvv         ! ocean velocities and RHS of momentum equation
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zua, zva     ! local scalars
      REAL(wp), DIMENSION(A2D(nn_hls))     ::   zww
      REAL(wp), DIMENSION(A2D(nn_hls),jpk) ::   zwuw, zwvw
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   ztrdu, ztrdv
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('dyn_zad')
      !
      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                       ! Do only on the first tile
         IF( kt == nit000 ) THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'dyn_zad : 2nd order vertical advection scheme'
         ENDIF
      ENDIF

      IF( l_trddyn )   THEN           ! Save puu(:,:,:,Krhs) and pvv(:,:,:,Krhs) trends
         ALLOCATE( ztrdu(jpi,jpj,jpk) , ztrdv(jpi,jpj,jpk) )
         ztrdu(:,:,:) = puu(:,:,:,Krhs)
         ztrdv(:,:,:) = pvv(:,:,:,Krhs)
      ENDIF

      DO jk = 2, jpkm1                ! Vertical momentum advection at level w and u- and v- vertical
         IF( ln_vortex_force ) THEN       ! vertical fluxes
            DO_2D( 0, 1, 0, 1 )
               zww(ji,jj) = 0.25_wp * e1e2t(ji,jj) * ( ww(ji,jj,jk) + wsd(ji,jj,jk) )
            END_2D
         ELSE
            DO_2D( 0, 1, 0, 1 )
               zww(ji,jj) = 0.25_wp * e1e2t(ji,jj) * ww(ji,jj,jk)
            END_2D
         ENDIF
         DO_2D( 0, 0, 0, 0 )              ! vertical momentum advection at w-point
            zwuw(ji,jj,jk) = ( zww(ji+1,jj  ) + zww(ji,jj) ) * ( puu(ji,jj,jk-1,Kmm) - puu(ji,jj,jk,Kmm) )
            zwvw(ji,jj,jk) = ( zww(ji  ,jj+1) + zww(ji,jj) ) * ( pvv(ji,jj,jk-1,Kmm) - pvv(ji,jj,jk,Kmm) )
         END_2D
      END DO
      !
      ! Surface and bottom advective fluxes set to zero
      DO_2D( 0, 0, 0, 0 )
         zwuw(ji,jj, 1 ) = 0._wp
         zwvw(ji,jj, 1 ) = 0._wp
         zwuw(ji,jj,jpk) = 0._wp
         zwvw(ji,jj,jpk) = 0._wp
      END_2D
      !
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )   ! Vertical momentum advection at u- and v-points
         puu(ji,jj,jk,Krhs) = puu(ji,jj,jk,Krhs) - ( zwuw(ji,jj,jk) + zwuw(ji,jj,jk+1) ) * r1_e1e2u(ji,jj)   &
            &                                      / e3u(ji,jj,jk,Kmm)
         pvv(ji,jj,jk,Krhs) = pvv(ji,jj,jk,Krhs) - ( zwvw(ji,jj,jk) + zwvw(ji,jj,jk+1) ) * r1_e1e2v(ji,jj)   &
            &                                      / e3v(ji,jj,jk,Kmm)
      END_3D

      IF( l_trddyn ) THEN             ! save the vertical advection trends for diagnostic
         ztrdu(:,:,:) = puu(:,:,:,Krhs) - ztrdu(:,:,:)
         ztrdv(:,:,:) = pvv(:,:,:,Krhs) - ztrdv(:,:,:)
         CALL trd_dyn( ztrdu, ztrdv, jpdyn_zad, kt, Kmm )
         DEALLOCATE( ztrdu, ztrdv )
      ENDIF
      !                               ! Control print
      IF(sn_cfctl%l_prtctl)   CALL prt_ctl( tab3d_1=puu(:,:,:,Krhs), clinfo1=' zad  - Ua: ', mask1=umask,   &
         &                                  tab3d_2=pvv(:,:,:,Krhs), clinfo2=       ' Va: ', mask2=vmask, clinfo3='dyn' )
      !
      IF( ln_timing )   CALL timing_stop('dyn_zad')
      !
   END SUBROUTINE dyn_zad

   !!======================================================================
END MODULE dynzad
