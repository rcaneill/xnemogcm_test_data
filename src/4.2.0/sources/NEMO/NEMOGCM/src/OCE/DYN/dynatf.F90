MODULE dynatf
   !!=========================================================================
   !!                       ***  MODULE  dynatf  ***
   !! Ocean dynamics: time filtering
   !!=========================================================================
   !! History :  OPA  !  1987-02  (P. Andrich, D. L Hostis)  Original code
   !!                 !  1990-10  (C. Levy, G. Madec)
   !!            7.0  !  1993-03  (M. Guyon)  symetrical conditions
   !!            8.0  !  1997-02  (G. Madec & M. Imbard)  opa, release 8.0
   !!            8.2  !  1997-04  (A. Weaver)  Euler forward step
   !!             -   !  1997-06  (G. Madec)  lateral boudary cond., lbc routine
   !!    NEMO    1.0  !  2002-08  (G. Madec)  F90: Free form and module
   !!             -   !  2002-10  (C. Talandier, A-M. Treguier) Open boundary cond.
   !!            2.0  !  2005-11  (V. Garnier) Surface pressure gradient organization
   !!            2.3  !  2007-07  (D. Storkey) Calls to BDY routines.
   !!            3.2  !  2009-06  (G. Madec, R.Benshila)  re-introduce the vvl option
   !!            3.3  !  2010-09  (D. Storkey, E.O'Dea) Bug fix for BDY module
   !!            3.3  !  2011-03  (P. Oddo) Bug fix for time-splitting+(BDY-OBC) and not VVL
   !!            3.5  !  2013-07  (J. Chanut) Compliant with time splitting changes
   !!            3.6  !  2014-04  (G. Madec) add the diagnostic of the time filter trends
   !!            3.7  !  2015-11  (J. Chanut) Free surface simplification
   !!            4.1  !  2019-08  (A. Coward, D. Storkey) Rename dynnxt.F90 -> dynatf.F90. Now just does time filtering.
   !!-------------------------------------------------------------------------

   !!----------------------------------------------------------------------------------------------
   !!   dyn_atf       : apply Asselin time filtering to "now" velocities and vertical scale factors
   !!----------------------------------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
   USE dom_oce        ! ocean space and time domain
   USE sbc_oce        ! Surface boundary condition: ocean fields
   USE sbcrnf         ! river runoffs
   USE phycst         ! physical constants
   USE dynadv         ! dynamics: vector invariant versus flux form
   USE dynspg_ts      ! surface pressure gradient: split-explicit scheme
   USE domvvl         ! variable volume
   USE bdy_oce , ONLY : ln_bdy
   USE bdydta         ! ocean open boundary conditions
   USE bdydyn         ! ocean open boundary conditions
   USE bdyvol         ! ocean open boundary condition (bdy_vol routines)
   USE trd_oce        ! trends: ocean variables
   USE trddyn         ! trend manager: dynamics
   USE trdken         ! trend manager: kinetic energy
   USE isf_oce   , ONLY: ln_isf     ! ice shelf
   USE isfdynatf , ONLY: isf_dynatf ! ice shelf volume filter correction subroutine
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O manager library
   USE lbclnk         ! lateral boundary condition (or mpp link)
   USE lib_mpp        ! MPP library
   USE prtctl         ! Print control
   USE timing         ! Timing
   USE zdfdrg ,  ONLY : ln_drgice_imp, rCdU_top
#if defined key_agrif
   USE agrif_oce_interp
#endif

   IMPLICIT NONE
   PRIVATE

   PUBLIC    dyn_atf   ! routine called by step.F90

#if defined key_qco   ||   defined key_linssh
   !!----------------------------------------------------------------------
   !!   'key_qco'                        Quasi-Eulerian vertical coordinate
   !!       OR         EMPTY MODULE
   !!   'key_linssh'                        Fix in time vertical coordinate
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dyn_atf( kt, Kbb, Kmm, Kaa, puu, pvv, pe3t, pe3u, pe3v )
      INTEGER                             , INTENT(in   ) :: kt               ! ocean time-step index
      INTEGER                             , INTENT(in   ) :: Kbb, Kmm, Kaa    ! before and after time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpt), INTENT(inout) :: puu, pvv         ! velocities to be time filtered
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpt), INTENT(inout) :: pe3t, pe3u, pe3v ! scale factors to be time filtered

      WRITE(*,*) 'dyn_atf: You should not have seen this print! error?', kt
   END SUBROUTINE dyn_atf

#else

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: dynatf.F90 14834 2021-05-11 09:24:44Z hadcv $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dyn_atf ( kt, Kbb, Kmm, Kaa, puu, pvv, pe3t, pe3u, pe3v )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_atf  ***
      !!
      !! ** Purpose :   Finalize after horizontal velocity. Apply the boundary
      !!             condition on the after velocity and apply the Asselin time
      !!             filter to the now fields.
      !!
      !! ** Method  : * Ensure after velocities transport matches time splitting
      !!             estimate (ln_dynspg_ts=T)
      !!
      !!              * Apply lateral boundary conditions on after velocity
      !!             at the local domain boundaries through lbc_lnk call,
      !!             at the one-way open boundaries (ln_bdy=T),
      !!             at the AGRIF zoom   boundaries (lk_agrif=T)
      !!
      !!              * Apply the Asselin time filter to the now fields
      !!             arrays to start the next time step:
      !!                (puu(Kmm),pvv(Kmm)) = (puu(Kmm),pvv(Kmm))
      !!                                    + rn_atfp [ (puu(Kbb),pvv(Kbb)) + (puu(Kaa),pvv(Kaa)) - 2 (puu(Kmm),pvv(Kmm)) ]
      !!             Note that with flux form advection and non linear free surface,
      !!             the time filter is applied on thickness weighted velocity.
      !!             As a result, dyn_atf MUST be called after tra_atf.
      !!
      !! ** Action :   puu(Kmm),pvv(Kmm)   filtered now horizontal velocity
      !!----------------------------------------------------------------------
      INTEGER                             , INTENT(in   ) :: kt               ! ocean time-step index
      INTEGER                             , INTENT(in   ) :: Kbb, Kmm, Kaa    ! before and after time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpt), INTENT(inout) :: puu, pvv         ! velocities to be time filtered
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpt), INTENT(inout) :: pe3t, pe3u, pe3v ! scale factors to be time filtered
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zue3a, zue3n, zue3b, zcoef    ! local scalars
      REAL(wp) ::   zve3a, zve3n, zve3b           !   -      -
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)   ::   zue, zve, zwfld
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)   ::   zutau, zvtau
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   ze3t_f, ze3u_f, ze3v_f, zua, zva
      !!----------------------------------------------------------------------
      !
      IF( ln_timing    )   CALL timing_start('dyn_atf')
      IF( ln_dynspg_ts )   ALLOCATE( zue(jpi,jpj)     , zve(jpi,jpj)     )
      IF( l_trddyn     )   ALLOCATE( zua(jpi,jpj,jpk) , zva(jpi,jpj,jpk) )
      !
      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'dyn_atf : Asselin time filtering'
         IF(lwp) WRITE(numout,*) '~~~~~~~'
      ENDIF

      IF ( ln_dynspg_ts ) THEN
         ! Ensure below that barotropic velocities match time splitting estimate
         ! Compute actual transport and replace it with ts estimate at "after" time step
         zue(:,:) = pe3u(:,:,1,Kaa) * puu(:,:,1,Kaa) * umask(:,:,1)
         zve(:,:) = pe3v(:,:,1,Kaa) * pvv(:,:,1,Kaa) * vmask(:,:,1)
         DO jk = 2, jpkm1
            zue(:,:) = zue(:,:) + pe3u(:,:,jk,Kaa) * puu(:,:,jk,Kaa) * umask(:,:,jk)
            zve(:,:) = zve(:,:) + pe3v(:,:,jk,Kaa) * pvv(:,:,jk,Kaa) * vmask(:,:,jk)
         END DO
         DO jk = 1, jpkm1
            puu(:,:,jk,Kaa) = ( puu(:,:,jk,Kaa) - zue(:,:) * r1_hu(:,:,Kaa) + uu_b(:,:,Kaa) ) * umask(:,:,jk)
            pvv(:,:,jk,Kaa) = ( pvv(:,:,jk,Kaa) - zve(:,:) * r1_hv(:,:,Kaa) + vv_b(:,:,Kaa) ) * vmask(:,:,jk)
         END DO
         !
         IF( .NOT.ln_bt_fw ) THEN
            ! Remove advective velocity from "now velocities"
            ! prior to asselin filtering
            ! In the forward case, this is done below after asselin filtering
            ! so that asselin contribution is removed at the same time
            DO jk = 1, jpkm1
               puu(:,:,jk,Kmm) = ( puu(:,:,jk,Kmm) - un_adv(:,:)*r1_hu(:,:,Kmm) + uu_b(:,:,Kmm) )*umask(:,:,jk)
               pvv(:,:,jk,Kmm) = ( pvv(:,:,jk,Kmm) - vn_adv(:,:)*r1_hv(:,:,Kmm) + vv_b(:,:,Kmm) )*vmask(:,:,jk)
            END DO
         ENDIF
      ENDIF

      ! Update after velocity on domain lateral boundaries
      ! --------------------------------------------------
# if defined key_agrif
      CALL Agrif_dyn( kt )             !* AGRIF zoom boundaries
# endif
      !
      CALL lbc_lnk( 'dynatf', puu(:,:,:,Kaa), 'U', -1.0_wp, pvv(:,:,:,Kaa), 'V', -1.0_wp )     !* local domain boundaries
      !
      !                                !* BDY open boundaries
      IF( ln_bdy .AND. ln_dynspg_exp )   CALL bdy_dyn( kt, Kbb, puu, pvv, Kaa )
      IF( ln_bdy .AND. ln_dynspg_ts  )   CALL bdy_dyn( kt, Kbb, puu, pvv, Kaa, dyn3d_only=.true. )

!!$   Do we need a call to bdy_vol here??
      !
      IF( l_trddyn ) THEN             ! prepare the atf trend computation + some diagnostics
         !
         !                                  ! Kinetic energy and Conversion
         IF( ln_KE_trd  )   CALL trd_dyn( puu(:,:,:,Kaa), pvv(:,:,:,Kaa), jpdyn_ken, kt, Kmm )
         !
         IF( ln_dyn_trd ) THEN              ! 3D output: total momentum trends
            zua(:,:,:) = ( puu(:,:,:,Kaa) - puu(:,:,:,Kbb) ) * r1_Dt
            zva(:,:,:) = ( pvv(:,:,:,Kaa) - pvv(:,:,:,Kbb) ) * r1_Dt
            CALL iom_put( "utrd_tot", zua )        ! total momentum trends, except the asselin time filter
            CALL iom_put( "vtrd_tot", zva )
         ENDIF
         !
         zua(:,:,:) = puu(:,:,:,Kmm)             ! save the now velocity before the asselin filter
         zva(:,:,:) = pvv(:,:,:,Kmm)             ! (caution: there will be a shift by 1 timestep in the
         !                                  !  computation of the asselin filter trends)
      ENDIF

      ! Time filter and swap of dynamics arrays
      ! ------------------------------------------

      IF( .NOT. l_1st_euler ) THEN    !* Leap-Frog : Asselin time filter
         !                                ! =============!
         IF( ln_linssh ) THEN             ! Fixed volume !
            !                             ! =============!
            DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 )
               puu(ji,jj,jk,Kmm) = puu(ji,jj,jk,Kmm) + rn_atfp * ( puu(ji,jj,jk,Kbb) - 2._wp * puu(ji,jj,jk,Kmm) + puu(ji,jj,jk,Kaa) )
               pvv(ji,jj,jk,Kmm) = pvv(ji,jj,jk,Kmm) + rn_atfp * ( pvv(ji,jj,jk,Kbb) - 2._wp * pvv(ji,jj,jk,Kmm) + pvv(ji,jj,jk,Kaa) )
            END_3D
            !                             ! ================!
         ELSE                             ! Variable volume !
            !                             ! ================!
            ! Time-filtered scale factor at t-points
            ! ----------------------------------------------------
            ALLOCATE( ze3t_f(jpi,jpj,jpk), zwfld(jpi,jpj) )
            DO jk = 1, jpkm1
               ze3t_f(:,:,jk) = pe3t(:,:,jk,Kmm) + rn_atfp * ( pe3t(:,:,jk,Kbb) - 2._wp * pe3t(:,:,jk,Kmm) + pe3t(:,:,jk,Kaa) )
            END DO
            ! Add volume filter correction: compatibility with tracer advection scheme
            ! => time filter + conservation correction
            zcoef = rn_atfp * rn_Dt * r1_rho0
            zwfld(:,:) = emp_b(:,:) - emp(:,:)
            IF ( ln_rnf ) zwfld(:,:) =  zwfld(:,:) - ( rnf_b(:,:) - rnf(:,:) )

            DO jk = 1, jpkm1
               ze3t_f(:,:,jk) = ze3t_f(:,:,jk) - zcoef * zwfld(:,:) * tmask(:,:,jk) &
                              &                        * pe3t(:,:,jk,Kmm) / ( ht(:,:) + 1._wp - ssmask(:,:) )
            END DO
            !
            ! ice shelf melting (deal separately as it can be in depth)
            ! PM: we could probably define a generic subroutine to do the in depth correction
            !     to manage rnf, isf and possibly in the futur icb, tide water glacier (...)
            !     ...(kt, coef, ktop, kbot, hz, fwf_b, fwf)
            IF ( ln_isf ) CALL isf_dynatf( kt, Kmm, ze3t_f, rn_atfp * rn_Dt )
            !
            pe3t(:,:,1:jpkm1,Kmm) = ze3t_f(:,:,1:jpkm1)        ! filtered scale factor at T-points
            !
            IF( ln_dynadv_vec ) THEN      ! Asselin filter applied on velocity
               ! Before filtered scale factor at (u/v)-points
               CALL dom_vvl_interpol( pe3t(:,:,:,Kmm), pe3u(:,:,:,Kmm), 'U' )
               CALL dom_vvl_interpol( pe3t(:,:,:,Kmm), pe3v(:,:,:,Kmm), 'V' )
               DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 )
                  puu(ji,jj,jk,Kmm) = puu(ji,jj,jk,Kmm) + rn_atfp * ( puu(ji,jj,jk,Kbb) - 2._wp * puu(ji,jj,jk,Kmm) + puu(ji,jj,jk,Kaa) )
                  pvv(ji,jj,jk,Kmm) = pvv(ji,jj,jk,Kmm) + rn_atfp * ( pvv(ji,jj,jk,Kbb) - 2._wp * pvv(ji,jj,jk,Kmm) + pvv(ji,jj,jk,Kaa) )
               END_3D
               !
            ELSE                          ! Asselin filter applied on thickness weighted velocity
               !
               ALLOCATE( ze3u_f(jpi,jpj,jpk) , ze3v_f(jpi,jpj,jpk) )
               ! Now filtered scale factor at (u/v)-points stored in ze3u_f, ze3v_f
               CALL dom_vvl_interpol( pe3t(:,:,:,Kmm), ze3u_f, 'U' )
               CALL dom_vvl_interpol( pe3t(:,:,:,Kmm), ze3v_f, 'V' )
               DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 )
                  zue3a = pe3u(ji,jj,jk,Kaa) * puu(ji,jj,jk,Kaa)
                  zve3a = pe3v(ji,jj,jk,Kaa) * pvv(ji,jj,jk,Kaa)
                  zue3n = pe3u(ji,jj,jk,Kmm) * puu(ji,jj,jk,Kmm)
                  zve3n = pe3v(ji,jj,jk,Kmm) * pvv(ji,jj,jk,Kmm)
                  zue3b = pe3u(ji,jj,jk,Kbb) * puu(ji,jj,jk,Kbb)
                  zve3b = pe3v(ji,jj,jk,Kbb) * pvv(ji,jj,jk,Kbb)
                  !
                  puu(ji,jj,jk,Kmm) = ( zue3n + rn_atfp * ( zue3b - 2._wp * zue3n  + zue3a ) ) / ze3u_f(ji,jj,jk)
                  pvv(ji,jj,jk,Kmm) = ( zve3n + rn_atfp * ( zve3b - 2._wp * zve3n  + zve3a ) ) / ze3v_f(ji,jj,jk)
               END_3D
               pe3u(:,:,1:jpkm1,Kmm) = ze3u_f(:,:,1:jpkm1)
               pe3v(:,:,1:jpkm1,Kmm) = ze3v_f(:,:,1:jpkm1)
               !
               DEALLOCATE( ze3u_f , ze3v_f )
            ENDIF
            !
            DEALLOCATE( ze3t_f, zwfld )
         ENDIF
         !
         IF( ln_dynspg_ts .AND. ln_bt_fw ) THEN
            ! Revert filtered "now" velocities to time split estimate
            ! Doing it here also means that asselin filter contribution is removed
            zue(:,:) = pe3u(:,:,1,Kmm) * puu(:,:,1,Kmm) * umask(:,:,1)
            zve(:,:) = pe3v(:,:,1,Kmm) * pvv(:,:,1,Kmm) * vmask(:,:,1)
            DO jk = 2, jpkm1
               zue(:,:) = zue(:,:) + pe3u(:,:,jk,Kmm) * puu(:,:,jk,Kmm) * umask(:,:,jk)
               zve(:,:) = zve(:,:) + pe3v(:,:,jk,Kmm) * pvv(:,:,jk,Kmm) * vmask(:,:,jk)
            END DO
            DO jk = 1, jpkm1
               puu(:,:,jk,Kmm) = puu(:,:,jk,Kmm) - (zue(:,:) * r1_hu(:,:,Kmm) - uu_b(:,:,Kmm)) * umask(:,:,jk)
               pvv(:,:,jk,Kmm) = pvv(:,:,jk,Kmm) - (zve(:,:) * r1_hv(:,:,Kmm) - vv_b(:,:,Kmm)) * vmask(:,:,jk)
            END DO
         ENDIF
         !
      ENDIF ! .NOT. l_1st_euler
      !
      ! This is needed for dyn_ldf_blp to be restartable
      IF( nn_hls == 2 ) CALL lbc_lnk( 'dynatf', puu(:,:,:,Kmm), 'U', -1.0_wp, pvv(:,:,:,Kmm), 'V', -1.0_wp )
      ! Set "now" and "before" barotropic velocities for next time step:
      ! JC: Would be more clever to swap variables than to make a full vertical
      ! integration
      !
      IF(.NOT.ln_linssh ) THEN
         hu(:,:,Kmm) = pe3u(:,:,1,Kmm ) * umask(:,:,1)
         hv(:,:,Kmm) = pe3v(:,:,1,Kmm ) * vmask(:,:,1)
         DO jk = 2, jpkm1
            hu(:,:,Kmm) = hu(:,:,Kmm) + pe3u(:,:,jk,Kmm ) * umask(:,:,jk)
            hv(:,:,Kmm) = hv(:,:,Kmm) + pe3v(:,:,jk,Kmm ) * vmask(:,:,jk)
         END DO
         r1_hu(:,:,Kmm) = ssumask(:,:) / ( hu(:,:,Kmm) + 1._wp - ssumask(:,:) )
         r1_hv(:,:,Kmm) = ssvmask(:,:) / ( hv(:,:,Kmm) + 1._wp - ssvmask(:,:) )
      ENDIF
      !
      uu_b(:,:,Kaa) = pe3u(:,:,1,Kaa) * puu(:,:,1,Kaa) * umask(:,:,1)
      uu_b(:,:,Kmm) = pe3u(:,:,1,Kmm) * puu(:,:,1,Kmm) * umask(:,:,1)
      vv_b(:,:,Kaa) = pe3v(:,:,1,Kaa) * pvv(:,:,1,Kaa) * vmask(:,:,1)
      vv_b(:,:,Kmm) = pe3v(:,:,1,Kmm) * pvv(:,:,1,Kmm) * vmask(:,:,1)
      DO jk = 2, jpkm1
         uu_b(:,:,Kaa) = uu_b(:,:,Kaa) + pe3u(:,:,jk,Kaa) * puu(:,:,jk,Kaa) * umask(:,:,jk)
         uu_b(:,:,Kmm) = uu_b(:,:,Kmm) + pe3u(:,:,jk,Kmm) * puu(:,:,jk,Kmm) * umask(:,:,jk)
         vv_b(:,:,Kaa) = vv_b(:,:,Kaa) + pe3v(:,:,jk,Kaa) * pvv(:,:,jk,Kaa) * vmask(:,:,jk)
         vv_b(:,:,Kmm) = vv_b(:,:,Kmm) + pe3v(:,:,jk,Kmm) * pvv(:,:,jk,Kmm) * vmask(:,:,jk)
      END DO
      uu_b(:,:,Kaa) = uu_b(:,:,Kaa) * r1_hu(:,:,Kaa)
      vv_b(:,:,Kaa) = vv_b(:,:,Kaa) * r1_hv(:,:,Kaa)
      uu_b(:,:,Kmm) = uu_b(:,:,Kmm) * r1_hu(:,:,Kmm)
      vv_b(:,:,Kmm) = vv_b(:,:,Kmm) * r1_hv(:,:,Kmm)
      !
      IF( .NOT.ln_dynspg_ts ) THEN        ! output the barotropic currents
         CALL iom_put(  "ubar", uu_b(:,:,Kmm) )
         CALL iom_put(  "vbar", vv_b(:,:,Kmm) )
      ENDIF
      IF( l_trddyn ) THEN                ! 3D output: asselin filter trends on momentum
         zua(:,:,:) = ( puu(:,:,:,Kmm) - zua(:,:,:) ) * r1_Dt
         zva(:,:,:) = ( pvv(:,:,:,Kmm) - zva(:,:,:) ) * r1_Dt
         CALL trd_dyn( zua, zva, jpdyn_atf, kt, Kmm )
      ENDIF
      !
      IF ( iom_use("utau") ) THEN
         IF ( ln_drgice_imp.OR.ln_isfcav ) THEN
            ALLOCATE(zutau(jpi,jpj))
            DO_2D( 0, 0, 0, 0 )
               jk = miku(ji,jj)
               zutau(ji,jj) = utau(ji,jj) + 0.5_wp * rho0 * ( rCdU_top(ji+1,jj)+rCdU_top(ji,jj) ) * puu(ji,jj,jk,Kaa)
            END_2D
            CALL iom_put(  "utau", zutau(:,:) )
            DEALLOCATE(zutau)
         ELSE
            CALL iom_put(  "utau", utau(:,:) )
         ENDIF
      ENDIF
      !
      IF ( iom_use("vtau") ) THEN
         IF ( ln_drgice_imp.OR.ln_isfcav ) THEN
            ALLOCATE(zvtau(jpi,jpj))
            DO_2D( 0, 0, 0, 0 )
               jk = mikv(ji,jj)
               zvtau(ji,jj) = vtau(ji,jj) + 0.5_wp * rho0 * ( rCdU_top(ji,jj+1)+rCdU_top(ji,jj) ) * pvv(ji,jj,jk,Kaa)
            END_2D
            CALL iom_put(  "vtau", zvtau(:,:) )
            DEALLOCATE(zvtau)
         ELSE
            CALL iom_put(  "vtau", vtau(:,:) )
         ENDIF
      ENDIF
      !
      IF(sn_cfctl%l_prtctl)   CALL prt_ctl( tab3d_1=puu(:,:,:,Kaa), clinfo1=' nxt  - puu(:,:,:,Kaa): ', mask1=umask,   &
         &                                  tab3d_2=pvv(:,:,:,Kaa), clinfo2=' pvv(:,:,:,Kaa): '       , mask2=vmask )
      !
      IF( ln_dynspg_ts )   DEALLOCATE( zue, zve )
      IF( l_trddyn     )   DEALLOCATE( zua, zva )
      IF( ln_timing    )   CALL timing_stop('dyn_atf')
      !
   END SUBROUTINE dyn_atf

#endif

   !!=========================================================================
END MODULE dynatf
