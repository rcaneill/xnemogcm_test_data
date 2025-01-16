MODULE stpmlf
   !!======================================================================
   !!                       ***  MODULE stpMLF  ***
   !! Time-stepping   : manager of the shallow water equation time stepping
   !!                   Modified Leap Frog scheme
   !!======================================================================
   !! History :  NEMO !  2020-03  (A. Nasser, G. Madec)  Original code from  4.0.2
   !!             -   !  2020-10  (S. Techene, G. Madec)  cleanning
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   stp_MLF       : MLF Shallow Water Eq. time-stepping
   !!----------------------------------------------------------------------
   USE stp_oce        ! modules used in nemo_init and stp_MLF
   !
   USE domqco         ! quasi-eulerian coordinate
   USE phycst         ! physical constants
   USE usrdef_nam     ! user defined namelist parameters
!!st   USE usrdef_sbc     ! user defined surface boundary cond
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC   stp_MLF   ! called by nemogcm.F90
   
   !                                          !**  time level indices  **!
   INTEGER, PUBLIC ::   Nbb, Nnn, Naa, Nrhs   !: used by nemo_init
      
   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: step.F90 12614 2020-03-26 14:59:52Z gm $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE stp_MLF( kstp )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE stp_MLF  ***
      !!
      !! ** Purpose : - Time stepping of shallow water (SHW) (momentum and ssh eqs.)
      !!
      !! ** Method  : -1- Update forcings
      !!              -2- Update the ssh at Naa
      !!              -3- Compute the momentum trends (Nrhs)
      !!              -4- Update the horizontal velocity
      !!              -5- Apply Asselin time filter to uu,vv,ssh
      !!              -6- Outputs and diagnostics
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kstp   ! ocean time-step index
      !
      INTEGER ::   ji, jj, jk             ! dummy loop indices
      INTEGER ::   indic                  ! error indicator if < 0
      REAL(wp)::   z1_2rho0               ! local scalars
      REAL(wp)::   zrhs_u, zue3a, zue3n, zue3b, zua   ! local scalars
      REAL(wp)::   zrhs_v, zve3a, zve3n, zve3b, zva   !   -      -
      !! ---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('stp_MLF')
      !
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! model timestep
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !
      IF( l_1st_euler ) THEN   ! start or restart with Euler 1st time-step
         rDt   = rn_Dt   
         r1_Dt = 1._wp / rDt
      ENDIF

      IF( kstp == nit000 )   ww(:,:,:) = 0._wp   ! initialize vertical velocity once for all to zero

      !
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! update I/O and calendar 
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                             indic = 0                ! reset to no error condition

      IF( kstp == nit000 ) THEN                       ! initialize IOM context (must be done after nemo_init for AGRIF+XIOS+OASIS)
                             CALL iom_init( cxios_context, ld_closedef=.FALSE. )   ! for model grid (including passible AGRIF zoom)
                             CALL iom_init_closedef
      ENDIF
      IF( kstp /= nit000 )   CALL day( kstp )         ! Calendar (day was already called at nit000 in day_init)
                             CALL iom_setkt( kstp - nit000 + 1,      cxios_context          )   ! tell IOM we are at time step kstp

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Update external forcing   (SWE: surface boundary condition only)
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

                             CALL sbc     ( kstp, Nbb, Nnn )                   ! Sea Boundary Condition

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Ocean physics update   (SWE: eddy viscosity only)
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      IF( l_ldfdyn_time )   CALL ldf_dyn( kstp, Nbb )                          ! eddy viscosity coeff. 

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      !  RHS of horizontal velocity
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

                         uu(:,:,:,Nrhs) = 0._wp            ! set dynamics trends to zero
                         vv(:,:,:,Nrhs) = 0._wp

                         CALL dyn_adv( kstp, Nbb, Nnn, uu, vv, Nrhs )   ! advection (VF or FF)	==> RHS
                         CALL dyn_vor( kstp,      Nnn, uu, vv, Nrhs )   ! vorticity           	==> RHS
                         CALL dyn_ldf( kstp, Nbb, Nnn, uu, vv, Nrhs )   ! lateral mixing                     ==> RHS

      z1_2rho0 = 0.5_wp * r1_rho0
      !
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         !                                                              ! horizontal pressure gradient
         zrhs_u =        - grav * ( ssh(ji+1,jj,Nnn) - ssh(ji,jj,Nnn) ) * r1_e1u(ji,jj)
         zrhs_v =        - grav * ( ssh(ji,jj+1,Nnn) - ssh(ji,jj,Nnn) ) * r1_e2v(ji,jj)
         !                                                              ! wind stress and layer friction
         zrhs_u = zrhs_u + z1_2rho0 * ( utau_b(ji,jj) + utau(ji,jj) ) / e3u(ji,jj,jk,Nnn)   &
            &                                  - rn_rfr * uu(ji,jj,jk,Nbb)
         zrhs_v = zrhs_v + z1_2rho0 * ( vtau_b(ji,jj) + vtau(ji,jj) ) / e3v(ji,jj,jk,Nnn)   &
            &                                  - rn_rfr * vv(ji,jj,jk,Nbb)
         !                                                              ! ==> RHS
         uu(ji,jj,jk,Nrhs) = uu(ji,jj,jk,Nrhs) + zrhs_u
         vv(ji,jj,jk,Nrhs) = vv(ji,jj,jk,Nrhs) + zrhs_v
      END_3D

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Time stepping of ssh Eq. (and update r3_Naa)
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !        ! Leap Frog time stepping ==> ssh_Naa and r3_Naa
                         CALL ssh_nxt( kstp, Nbb, Nnn, ssh   , Naa )    ! after ssh 
      !                                                                 ! after ssh/h_0 ratio explicit
                         CALL dom_qco_r3c( ssh(:,:,Naa), r3t(:,:,Naa), r3u(:,:,Naa), r3v(:,:,Naa), r3f(:,:) )
      !        ! Asselin filter ==> ssh_Nnn filtered
      IF ( .NOT.( l_1st_euler ) ) THEN   ! Time filtering of now ssh
         ssh(:,:,Nnn) = ssh(:,:,Nnn) + rn_atfp * ( ssh(:,:,Nbb) - 2._wp * ssh(:,:,Nnn) + ssh(:,:,Naa) )
      ENDIF

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Time stepping of dynamics (u,v)
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      IF( ln_dynadv_vec ) THEN      ! vector invariant form : applied on velocity
         IF( l_1st_euler ) THEN          ! Euler time stepping (no Asselin filter)
            DO_3D( 0,0, 0,0, 1,jpkm1)
               uu(ji,jj,jk,Naa) = uu(ji,jj,jk,Nbb) + rDt * uu(ji,jj,jk,Nrhs) * umask(ji,jj,jk)
               vv(ji,jj,jk,Naa) = vv(ji,jj,jk,Nbb) + rDt * vv(ji,jj,jk,Nrhs) * vmask(ji,jj,jk)
             END_3D          
         ELSE                            ! Leap Frog time stepping + Asselin filter         
            DO_3D( 0,0, 0,0, 1,jpkm1)
               zua = uu(ji,jj,jk,Nbb) + rDt * uu(ji,jj,jk,Nrhs) * umask(ji,jj,jk)
               zva = vv(ji,jj,jk,Nbb) + rDt * vv(ji,jj,jk,Nrhs) * vmask(ji,jj,jk)
               !                                                                  ! Asselin time filter on u,v (Nnn)
               uu(ji,jj,jk,Nnn) = uu(ji,jj,jk,Nnn) + rn_atfp * (uu(ji,jj,jk,Nbb) - 2._wp * uu(ji,jj,jk,Nnn) + zua)
               vv(ji,jj,jk,Nnn) = vv(ji,jj,jk,Nnn) + rn_atfp * (vv(ji,jj,jk,Nbb) - 2._wp * vv(ji,jj,jk,Nnn) + zva)
               !              
               uu(ji,jj,jk,Naa) = zua
               vv(ji,jj,jk,Naa) = zva
            END_3D
            !                            ! Update r3_Nnn
            CALL dom_qco_r3c( ssh(:,:,Nnn), r3t(:,:,Nnn), r3u(:,:,Nnn), r3v(:,:,Nnn) )   ! now ssh/h_0 ratio from filtrered ssh
         ENDIF
         !
      ELSE                          ! flux form : applied on thickness weighted velocity
         IF( l_1st_euler ) THEN          ! Euler time stepping (no Asselin filter)
            DO_3D( 0,0, 0,0, 1,jpkm1)
               zue3b = e3u(ji,jj,jk,Nbb) * uu(ji,jj,jk,Nbb)
               zve3b = e3v(ji,jj,jk,Nbb) * vv(ji,jj,jk,Nbb)
               !                                                ! Euler time stepping
               zue3a = zue3b + rDt * e3u(ji,jj,jk,Nnn) * uu(ji,jj,jk,Nrhs) * umask(ji,jj,jk)
               zve3a = zve3b + rDt * e3v(ji,jj,jk,Nnn) * vv(ji,jj,jk,Nrhs) * vmask(ji,jj,jk)
               !
               uu(ji,jj,jk,Naa) = zue3a / e3u(ji,jj,jk,Naa)    
               vv(ji,jj,jk,Naa) = zve3a / e3v(ji,jj,jk,Naa)
            END_3D
         ELSE                             ! Leap Frog time stepping + Asselin filter
            CALL dom_qco_r3c( ssh(:,:,Nnn), r3t_f(:,:), r3u_f(:,:), r3v_f(:,:) )   ! now ssh/h_0 ratio from filtrered ssh
            DO_3D( 0,0, 0,0, 1,jpkm1)
               zue3n = ( 1._wp + r3u(ji,jj,Nnn) ) * uu(ji,jj,jk,Nnn)
               zve3n = ( 1._wp + r3v(ji,jj,Nnn) ) * vv(ji,jj,jk,Nnn)
               zue3b = ( 1._wp + r3u(ji,jj,Nbb) ) * uu(ji,jj,jk,Nbb)
               zve3b = ( 1._wp + r3v(ji,jj,Nbb) ) * vv(ji,jj,jk,Nbb)
               !                                                ! LF time stepping
               zue3a = zue3b + rDt * ( 1._wp + r3u(ji,jj,Nnn) ) * uu(ji,jj,jk,Nrhs) * umask(ji,jj,jk)
               zve3a = zve3b + rDt * ( 1._wp + r3v(ji,jj,Nnn) ) * vv(ji,jj,jk,Nrhs) * vmask(ji,jj,jk)
               !                                                ! Asselin time filter on u,v (Nnn)
               uu(ji,jj,jk,Nnn) = ( zue3n + rn_atfp * ( zue3b - 2._wp * zue3n  + zue3a ) ) / ( 1._wp + r3u_f(ji,jj) )
               vv(ji,jj,jk,Nnn) = ( zve3n + rn_atfp * ( zve3b - 2._wp * zve3n  + zve3a ) ) / ( 1._wp + r3v_f(ji,jj) )
               !
               uu(ji,jj,jk,Naa) = zue3a / ( 1._wp + r3u(ji,jj,Naa) )    
               vv(ji,jj,jk,Naa) = zve3a / ( 1._wp + r3v(ji,jj,Naa) )
            END_3D
            !                             ! Update r3_Nnn with time filtered values
            r3t(:,:,Nnn) = r3t_f(:,:)
            r3u(:,:,Nnn) = r3u_f(:,:)
            r3v(:,:,Nnn) = r3v_f(:,:)
         ENDIF
      ENDIF

      CALL lbc_lnk( 'stp_MLF', uu(:,:,:,Nnn), 'U', -1., vv(:,:,:,Nnn), 'V', -1.,   &   !* local domain boundaries
         &                     uu(:,:,:,Naa), 'U', -1., vv(:,:,:,Naa), 'V', -1.    )     
      IF (nn_hls==2) CALL lbc_lnk( 'stp_MLF', r3u(:,:,Naa), 'U', 1., r3v(:,:,Naa), 'V', 1.)

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Set boundary conditions, time filter and swap time levels
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      ! Swap time levels
      Nrhs = Nbb
      Nbb = Nnn
      Nnn = Naa
      Naa = Nrhs

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! diagnostics and outputs
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      
      IF( ln_diacfl  )   CALL dia_cfl  ( kstp,      Nnn )      ! Courant number diagnostics
                         CALL dia_wri  ( kstp,      Nnn )      ! ocean model: outputs
      !
      IF( lrst_oce   )   CALL rst_write( kstp, Nbb, Nnn )      ! write output ocean restart file

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Control
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                         CALL stp_ctl  ( kstp, Nnn )

      IF( kstp == nit000 ) THEN                          ! 1st time step only
                                        CALL iom_close( numror )   ! close input  ocean restart file
         IF(lwm)                        CALL FLUSH    ( numond )   ! flush output namelist oce
         IF(lwm .AND. numoni /= -1 )    CALL FLUSH    ( numoni )   ! flush output namelist ice (if exist)
      ENDIF

      !
#if defined key_xios
      IF( kstp == nitend .OR. indic < 0 ) THEN
!!st : cxios_context needed ? because opened earlier ???         
         CALL iom_context_finalize( cxios_context ) ! needed for XIOS+AGRIF
      ENDIF
#endif
      !
      IF( l_1st_euler ) THEN         ! recover Leap-frog timestep
         rDt   = 2._wp * rn_Dt   
         r1_Dt = 1._wp / rDt
         l_1st_euler = .FALSE.      
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('stp_MLF')
      !
   END SUBROUTINE stp_MLF

   !!======================================================================
END MODULE stpmlf
