MODULE stprk3
   !!======================================================================
   !!                       ***  MODULE stprk3  ***
   !! Time-stepping   : manager of the shallow water equation time stepping
   !!                   3rd order Runge-Kutta scheme
   !!======================================================================
   !! History :  NEMO !  2020-03  (A. Nasser, G. Madec)  Original code from  4.0.2
   !!             -   !  2020-10  (S. Techene, G. Madec)  cleanning
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   stp_RK3       : RK3 Shallow Water Eq. time-stepping
   !!----------------------------------------------------------------------
   USE stp_oce        ! modules used in nemo_init and stp_RK3
   !
   USE domqco         ! quasi-eulerian coordinate
   USE phycst         ! physical constants
   USE usrdef_nam     ! user defined namelist parameters

   IMPLICIT NONE
   PRIVATE

   PUBLIC   stp_RK3   ! called by nemogcm.F90

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

   SUBROUTINE stp_RK3( kstp )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE stp_RK3  ***
      !!
      !! ** Purpose : - RK3 Time stepping scheme for shallow water Eq.
      !!
      !! ** Method  : 3rd order time stepping scheme which has 3 stages
      !!       * Update calendar and forcings
      !!       stage 1   : n ==> n+1/3 using variables at n
      !!                 - Compute the rhs of momentum
      !!                 - Time step ssh at Naa (n+1/3)
      !!                 - Time step u,v at Naa (n+1/3)
      !!                 - Swap time indices
      !!       stage 2   : n ==> n+1/2 using variables at n and n+1/3
      !!                 - Compute the rhs of momentum
      !!                 - Time step ssh at Naa (n+1/2)
      !!                 - Time step u,v at Naa (n+1/2)
      !!                 - Swap time indices
      !!       stage 3   : n ==> n+1 using variables at n and n+1/2
      !!                 - Compute the rhs of momentum
      !!                 - Time step ssh at Naa (n+1)
      !!                 - Time step u,v at Naa (n+1)
      !!                 - Swap time indices
      !!       * Outputs and diagnostics
      !!
      !!       NB: in stages 1 and 2 lateral mixing and forcing are not taken
      !!           into account in the momentum RHS execpt if key_RK3all is used
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in   ) ::   kstp   ! ocean time-step index
      !
      INTEGER ::   ji, jj, jk   ! dummy loop indice
      INTEGER ::   indic        ! error indicator if < 0
      REAL(wp)::   z1_2rho0,  z5_6,  z3_4  ! local scalars
      REAL(wp)::   zue3a, zue3b, zua, zrhs_u    ! local scalars
      REAL(wp)::   zve3a, zve3b, zva, zrhs_v    !   -      -
      !! ---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('stp_RK3')
      !
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! model timestep
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !
      IF ( kstp == nit000 )   ww(:,:,:) = 0._wp   ! initialize vertical velocity one for all to zero

      !
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! update I/O and calendar 
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                             indic = 0                ! reset to no error condition
                             
      IF( kstp == nit000 ) THEN                       ! initialize IOM context
                             CALL iom_init( cxios_context, ld_closedef=.FALSE. )   ! for model grid (including possible AGRIF zoom)
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

      !======================================================================
      !======================================================================
      !                     =====       RK3       =====
      !======================================================================
      !======================================================================

      
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      !  RK3 1st stage Ocean dynamics : u, v, ssh
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      rDt   = rn_Dt / 3._wp  
      r1_Dt = 1._wp / rDt
      !
      !                                 !==  RHS of the momentum Eq.  ==!
      !
      uu(:,:,:,Nrhs) = 0._wp                        ! set dynamics trends to zero
      vv(:,:,:,Nrhs) = 0._wp

      CALL dyn_adv( kstp, Nbb, Nbb, uu, vv, Nrhs )  ! advection (VF or FF)	==> RHS
      CALL dyn_vor( kstp,      Nbb, uu, vv, Nrhs )  ! vorticity           	==> RHS
#if defined key_RK3all 
      CALL dyn_ldf( kstp, Nbb, Nbb, uu, vv, Nrhs )  ! lateral mixing
#endif
      z5_6 = 5._wp/6._wp
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         !                                          ! horizontal pressure gradient
         zrhs_u =        - grav    * ( ssh(ji+1,jj,Nbb) - ssh(ji,jj,Nbb) ) * r1_e1u(ji,jj)
         zrhs_v =        - grav    * ( ssh(ji,jj+1,Nbb) - ssh(ji,jj,Nbb) ) * r1_e2v(ji,jj)
#if defined key_RK3all
         !                                          ! wind stress and layer friction
         zrhs_u = zrhs_u + r1_rho0 * ( z5_6*utau_b(ji,jj) + (1._wp - z5_6)*utau(ji,jj) ) / e3u(ji,jj,jk,Nbb)   &
            &            - rn_rfr  * uu(ji,jj,jk,Nbb)
         zrhs_v = zrhs_v + r1_rho0 * ( z5_6*vtau_b(ji,jj) + (1._wp - z5_6)*vtau(ji,jj) ) / e3v(ji,jj,jk,Nbb)   &
            &            - rn_rfr  * vv(ji,jj,jk,Nbb)
#endif
         !                                          ! ==> RHS
         uu(ji,jj,jk,Nrhs) = uu(ji,jj,jk,Nrhs) + zrhs_u
         vv(ji,jj,jk,Nrhs) = vv(ji,jj,jk,Nrhs) + zrhs_v
      END_3D
      !
      !                                 !==  Time stepping of ssh Eq.  ==!   (and update r3_Naa)
      !
      CALL ssh_nxt( kstp, Nbb, Nbb, ssh, Naa )      ! after ssh
      !                                             ! after ssh/h_0 ratio
      CALL dom_qco_r3c( ssh(:,:,Naa), r3t(:,:,Naa), r3u(:,:,Naa), r3v(:,:,Naa), r3f(:,:) )
      !
      !                                 !==  Time stepping of momentum Eq.  ==!
      !
      IF( ln_dynadv_vec ) THEN                      ! vector invariant form : applied on velocity
         DO_3D( 0, 0, 0, 0, 1,jpkm1)
            uu(ji,jj,jk,Naa) = uu(ji,jj,jk,Nbb) + rDt * uu(ji,jj,jk,Nrhs) * umask(ji,jj,jk)
            vv(ji,jj,jk,Naa) = vv(ji,jj,jk,Nbb) + rDt * vv(ji,jj,jk,Nrhs) * vmask(ji,jj,jk)
         END_3D          
      ELSE
         DO_3D( 0, 0, 0, 0, 1,jpkm1)                 ! flux form : applied on thickness weighted velocity
            zue3b = e3u(ji,jj,jk,Nbb) * uu(ji,jj,jk,Nbb)
            zve3b = e3v(ji,jj,jk,Nbb) * vv(ji,jj,jk,Nbb)
            zue3a = zue3b + rDt * e3u(ji,jj,jk,Nbb) * uu(ji,jj,jk,Nrhs) * umask(ji,jj,jk)
            zve3a = zve3b + rDt * e3v(ji,jj,jk,Nbb) * vv(ji,jj,jk,Nrhs) * vmask(ji,jj,jk)
            !
            uu(ji,jj,jk,Naa) = zue3a / e3u(ji,jj,jk,Naa)    
            vv(ji,jj,jk,Naa) = zve3a / e3v(ji,jj,jk,Naa)
         END_3D
      ENDIF
      !
      CALL lbc_lnk( 'stp_RK3', uu(:,:,:,Naa), 'U', -1., vv(:,:,:,Naa), 'V', -1. )
      IF (nn_hls==2) CALL lbc_lnk( 'stp_RK3', r3u(:,:,Naa), 'U', 1., r3v(:,:,Naa), 'V', 1.)
      !
      !                                 !==  Swap time levels  ==!
      Nrhs= Nnn
      Nnn = Naa
      Naa = Nrhs

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      !  RK3 2nd stage Ocean dynamics : hdiv, ssh, e3, u, v, w
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      rDt   = rn_Dt / 2._wp  
      r1_Dt = 1._wp / rDt
      !
      !                                 !==  RHS of the momentum Eq.  ==!
      !
      uu(:,:,:,Nrhs) = 0._wp                        ! set dynamics trends to zero
      vv(:,:,:,Nrhs) = 0._wp
      CALL dyn_adv( kstp, Nbb, Nnn, uu, vv, Nrhs )  ! advection (VF or FF)	==> RHS 
      CALL dyn_vor( kstp,      Nnn, uu, vv, Nrhs )  ! vorticity           	==> RHS
#if defined key_RK3all  
      CALL dyn_ldf( kstp, Nbb, Nbb, uu, vv, Nrhs )  ! lateral mixing
#endif
      !
      z3_4 = 3._wp/4._wp
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         !                                          ! horizontal pressure gradient
         zrhs_u =        - grav    * ( ssh(ji+1,jj,Nnn) - ssh(ji,jj,Nnn) ) * r1_e1u(ji,jj)
         zrhs_v =        - grav    * ( ssh(ji,jj+1,Nnn) - ssh(ji,jj,Nnn) ) * r1_e2v(ji,jj)
#if defined key_RK3all
         !                                          ! wind stress and layer friction
         zrhs_u = zrhs_u + r1_rho0 * ( z3_4*utau_b(ji,jj) + (1._wp - z3_4)*utau(ji,jj) ) / e3u(ji,jj,jk,Nnn)   &
            &            - rn_rfr  * uu(ji,jj,jk,Nbb)
         zrhs_v = zrhs_v + r1_rho0 * ( z3_4*vtau_b(ji,jj) + (1._wp - z3_4)*vtau(ji,jj) ) / e3v(ji,jj,jk,Nnn)   &
            &            - rn_rfr  * vv(ji,jj,jk,Nbb)
#endif
         !                                          ! ==> RHS
         uu(ji,jj,jk,Nrhs) = uu(ji,jj,jk,Nrhs) + zrhs_u
         vv(ji,jj,jk,Nrhs) = vv(ji,jj,jk,Nrhs) + zrhs_v
      END_3D
      !
      !                                 !==  Time stepping of ssh Eq.  ==!   (and update r3_Naa)
      !
      CALL ssh_nxt( kstp, Nbb, Nnn, ssh, Naa )      ! after ssh
      !                                             ! after ssh/h_0 ratio
      CALL dom_qco_r3c( ssh(:,:,Naa), r3t(:,:,Naa), r3u(:,:,Naa), r3v(:,:,Naa), r3f(:,:) )
      !
      !                                 !==  Time stepping of momentum Eq.  ==!
      !
      IF( ln_dynadv_vec ) THEN                      ! vector invariant form : applied on velocity
         DO_3D( 0, 0, 0, 0, 1,jpkm1)
            uu(ji,jj,jk,Naa) = uu(ji,jj,jk,Nbb) + rDt * uu(ji,jj,jk,Nrhs) * umask(ji,jj,jk)
            vv(ji,jj,jk,Naa) = vv(ji,jj,jk,Nbb) + rDt * vv(ji,jj,jk,Nrhs) * vmask(ji,jj,jk)
         END_3D          
      ELSE
         DO_3D( 0, 0, 0, 0, 1,jpkm1)                 ! flux form : applied on thickness weighted velocity
            zue3b = e3u(ji,jj,jk,Nbb) * uu(ji,jj,jk,Nbb)
            zve3b = e3v(ji,jj,jk,Nbb) * vv(ji,jj,jk,Nbb)
            zue3a = zue3b + rDt * e3u(ji,jj,jk,Nnn) * uu(ji,jj,jk,Nrhs) * umask(ji,jj,jk)
            zve3a = zve3b + rDt * e3v(ji,jj,jk,Nnn) * vv(ji,jj,jk,Nrhs) * vmask(ji,jj,jk)
            !
            uu(ji,jj,jk,Naa) = zue3a / e3u(ji,jj,jk,Naa)    
            vv(ji,jj,jk,Naa) = zve3a / e3v(ji,jj,jk,Naa)
         END_3D
      ENDIF
      !
      CALL lbc_lnk( 'stp_RK3', uu(:,:,:,Naa), 'U', -1., vv(:,:,:,Naa), 'V', -1. )
      IF (nn_hls==2) CALL lbc_lnk( 'stp_RK3', r3u(:,:,Naa), 'U', 1., r3v(:,:,Naa), 'V', 1.)
      !
      !                                 !==  Swap time levels  ==!
      Nrhs= Nnn
      Nnn = Naa
      Naa = Nrhs
       
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      !  RK3 3rd stage Ocean dynamics : hdiv, ssh, e3, u, v, w
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      rDt   = rn_Dt
      r1_Dt = 1._wp / rDt
      !
      !                                 !==  RHS of the momentum Eq.  ==!
      !
      uu(:,:,:,Nrhs) = 0._wp                        ! set dynamics trends to zero
      vv(:,:,:,Nrhs) = 0._wp
      !
      CALL dyn_adv( kstp, Nbb, Nnn, uu, vv, Nrhs )  ! advection (VF or FF)	==> RHS
      CALL dyn_vor( kstp,      Nnn, uu, vv, Nrhs )  ! vorticity           	==> RHS
      CALL dyn_ldf( kstp, Nbb, Nnn, uu, vv, Nrhs )  ! lateral mixing

      z1_2rho0 = 0.5_wp * r1_rho0
      DO_3D( 0, 0, 0, 0, 1,jpkm1 )
         !                                          ! horizontal pressure gradient
         zrhs_u =        - grav * ( ssh(ji+1,jj,Nnn) - ssh(ji,jj,Nnn) ) * r1_e1u(ji,jj)
         zrhs_v =        - grav * ( ssh(ji,jj+1,Nnn) - ssh(ji,jj,Nnn) ) * r1_e2v(ji,jj)
         !                                          ! wind stress and layer friction
         zrhs_u = zrhs_u + z1_2rho0 * ( utau_b(ji,jj) + utau(ji,jj) ) / e3u(ji,jj,jk,Nnn)   &
            &            - rn_rfr   * uu(ji,jj,jk,Nbb)
         zrhs_v = zrhs_v + z1_2rho0 * ( vtau_b(ji,jj) + vtau(ji,jj) ) / e3v(ji,jj,jk,Nnn)   &
            &            - rn_rfr   * vv(ji,jj,jk,Nbb)
         !                                          ! ==> RHS
         uu(ji,jj,jk,Nrhs) = uu(ji,jj,jk,Nrhs) + zrhs_u
         vv(ji,jj,jk,Nrhs) = vv(ji,jj,jk,Nrhs) + zrhs_v
      END_3D
      !
      !                                 !==  Time stepping of ssh Eq.  ==!   (and update r3_Naa)
      !
      CALL ssh_nxt( kstp, Nbb, Nnn, ssh, Naa )      ! after ssh
      !                                             ! after ssh/h_0 ratio
      CALL dom_qco_r3c( ssh(:,:,Naa), r3t(:,:,Naa), r3u(:,:,Naa), r3v(:,:,Naa), r3f(:,:) )
      !
      !                                 !==  Time stepping of momentum Eq.  ==!
      !
      IF( ln_dynadv_vec ) THEN                      ! vector invariant form : applied on velocity
         DO_3D( 0, 0, 0, 0, 1,jpkm1)
            uu(ji,jj,jk,Naa) = uu(ji,jj,jk,Nbb) + rDt * uu(ji,jj,jk,Nrhs) * umask(ji,jj,jk)
            vv(ji,jj,jk,Naa) = vv(ji,jj,jk,Nbb) + rDt * vv(ji,jj,jk,Nrhs) * vmask(ji,jj,jk)
         END_3D
         !
      ELSE                                          ! flux form : applied on thickness weighted velocity
         DO_3D( 0, 0, 0, 0, 1,jpkm1)
            zue3b = e3u(ji,jj,jk,Nbb) * uu(ji,jj,jk,Nbb)
            zve3b = e3v(ji,jj,jk,Nbb) * vv(ji,jj,jk,Nbb)
            zue3a = zue3b + rDt * e3u(ji,jj,jk,Nbb) * uu(ji,jj,jk,Nrhs) * umask(ji,jj,jk)
            zve3a = zve3b + rDt * e3v(ji,jj,jk,Nbb) * vv(ji,jj,jk,Nrhs) * vmask(ji,jj,jk)
            !
            uu(ji,jj,jk,Naa) = zue3a / e3u(ji,jj,jk,Naa)    
            vv(ji,jj,jk,Naa) = zve3a / e3v(ji,jj,jk,Naa)
         END_3D
      ENDIF
      !
      CALL lbc_lnk( 'stp_RK3', uu(:,:,:,Naa), 'U', -1., vv(:,:,:,Naa), 'V', -1. )
      IF (nn_hls==2) CALL lbc_lnk( 'stp_RK3', r3u(:,:,Naa), 'U', 1., r3v(:,:,Naa), 'V', 1.)
      !
      !                                 !==  Swap time levels  ==!
      !
      Nrhs = Nbb
      Nbb = Naa
      Naa = Nrhs

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! diagnostics and outputs at Nbb (i.e. the just computed time step)
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      
      IF( ln_diacfl  )   CALL dia_cfl      ( kstp,      Nbb )      ! Courant number diagnostics
                         CALL dia_wri      ( kstp,      Nbb )      ! ocean model: outputs
      !
      IF( lrst_oce   )   CALL rst_write    ( kstp, Nbb, Nbb )   ! write output ocean restart file

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Control
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                         CALL stp_ctl      ( kstp     , Nbb )

      IF( kstp == nit000 ) THEN                          ! 1st time step only
                                        CALL iom_close( numror )   ! close input  ocean restart file
         IF(lwm)                        CALL FLUSH    ( numond )   ! flush output namelist oce
         IF(lwm .AND. numoni /= -1 )    CALL FLUSH    ( numoni )   ! flush output namelist ice (if exist)
      ENDIF

      !
#if defined key_xios
      IF( kstp == nitend .OR. indic < 0 ) THEN 
         CALL iom_context_finalize( cxios_context )
      ENDIF
#endif
      !
      IF( ln_timing )   CALL timing_stop('stp_RK3')
      !
   END SUBROUTINE stp_RK3

   !!======================================================================
END MODULE stprk3
