#define DECAL_FEEDBACK    /* SEPARATION of INTERFACES */
#undef DECAL_FEEDBACK_2D  /* SEPARATION of INTERFACES (Barotropic mode) */
#undef VOL_REFLUX         /* VOLUME REFLUXING*/
 
MODULE agrif_oce_update
   !!======================================================================
   !!                   ***  MODULE  agrif_oce_update  ***
   !! AGRIF: update package for the ocean dynamics (OCE)
   !!======================================================================
   !! History :  2.0  !  2002-06  (L. Debreu)  Original code
   !!            3.2  !  2009-04  (R. Benshila) 
   !!            3.6  !  2014-09  (R. Benshila)
   !!            4.2  !  2021-11  (J. Chanut) 
   !!----------------------------------------------------------------------
#if defined key_agrif 
   !!----------------------------------------------------------------------
   !!   'key_agrif'                                              AGRIF zoom
   !!----------------------------------------------------------------------
   USE par_oce
   USE oce
   USE dom_oce
   USE zdf_oce        ! vertical physics: ocean variables 
   USE agrif_oce
   USE dom_oce
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE domvvl         ! Need interpolation routines 
   USE vremap         ! Vertical remapping
   USE lbclnk 
#if defined key_qco
   USE domqco
#endif
   IMPLICIT NONE
   PRIVATE

   PUBLIC   Agrif_Update_Tra, Agrif_Update_Dyn, Agrif_Update_vvl, Agrif_Update_ssh
   PUBLIC   Agrif_Check_parent_bat

   !! * Substitutions
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/NST 4.0 , NEMO Consortium (2018)
   !! $Id: agrif_oce_update.F90 15317 2021-10-01 16:09:36Z jchanut $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE Agrif_Update_Tra( )
      !!----------------------------------------------------------------------
      !!                   *** ROUTINE Agrif_Update_Tra ***
      !!----------------------------------------------------------------------
      ! 
      IF (Agrif_Root()) RETURN
      !
      IF (lwp.AND.lk_agrif_debug) Write(*,*) 'Update tracers  from grid Number', Agrif_Fixed()

      l_vremap                      = ln_vert_remap
      Agrif_UseSpecialValueInUpdate = .FALSE. 
      ! 
# if ! defined DECAL_FEEDBACK
      CALL Agrif_Update_Variable(ts_update_id, procname=updateTS)
! near boundary update:
!      CALL Agrif_Update_Variable(ts_update_id,locupdate=(/0,2/), procname=updateTS)
# else
      CALL Agrif_Update_Variable(ts_update_id, locupdate=(/1,0/),procname=updateTS)
! near boundary update:
!      CALL Agrif_Update_Variable(ts_update_id,locupdate=(/1,2/), procname=updateTS)
# endif
      !
      l_vremap                      = .FALSE.
      !
      !
   END SUBROUTINE Agrif_Update_Tra

   SUBROUTINE Agrif_Update_Dyn( )
      !!----------------------------------------------------------------------
      !!                   *** ROUTINE Agrif_Update_Dyn ***
      !!----------------------------------------------------------------------
      ! 
      IF (Agrif_Root()) RETURN
      !
      IF (lwp.AND.lk_agrif_debug) Write(*,*) 'Update momentum from grid Number', Agrif_Fixed()

      Agrif_UseSpecialValueInUpdate = .FALSE.
      Agrif_SpecialValueFineGrid    = 0._wp
      l_vremap                      = ln_vert_remap
      use_sign_north                = .TRUE.
      sign_north                    = -1._wp     
!
# if ! defined DECAL_FEEDBACK_2D
      CALL Agrif_Update_Variable(unb_update_id,locupdate1=(/  nn_shift_bar,-2/),locupdate2=(/  nn_shift_bar,-2/),procname = updateU2d)
      CALL Agrif_Update_Variable(vnb_update_id,locupdate1=(/  nn_shift_bar,-2/),locupdate2=(/  nn_shift_bar,-2/),procname = updateV2d)
# else
      CALL Agrif_Update_Variable(unb_update_id,locupdate1=(/  nn_shift_bar,-2/),locupdate2=(/1+nn_shift_bar,-2/),procname = updateU2d)
      CALL Agrif_Update_Variable(vnb_update_id,locupdate1=(/1+nn_shift_bar,-2/),locupdate2=(/  nn_shift_bar,-2/),procname = updateV2d)  
# endif
      ! 
      IF ( ln_dynspg_ts .AND. ln_bt_fw ) THEN
         ! Update time integrated transports
#  if ! defined DECAL_FEEDBACK_2D
         CALL Agrif_Update_Variable(ub2b_update_id,locupdate1=(/  nn_shift_bar,-2/),locupdate2=(/  nn_shift_bar,-2/),procname = updateub2b)
         CALL Agrif_Update_Variable(vb2b_update_id,locupdate1=(/  nn_shift_bar,-2/),locupdate2=(/  nn_shift_bar,-2/),procname = updatevb2b)
#  else
         CALL Agrif_Update_Variable(ub2b_update_id,locupdate1=(/  nn_shift_bar,-2/),locupdate2=(/1+nn_shift_bar,-2/),procname = updateub2b)
         CALL Agrif_Update_Variable(vb2b_update_id,locupdate1=(/1+nn_shift_bar,-2/),locupdate2=(/  nn_shift_bar,-2/),procname = updatevb2b)
#  endif
         IF (lk_agrif_fstep) THEN
            CALL Agrif_Update_Variable(ub2b_update_id,locupdate1=(/  nn_shift_bar+nn_dist_par_bc-1,-2/),locupdate2=(/  nn_shift_bar+nn_dist_par_bc  ,-2/),procname = updateumsk)
            CALL Agrif_Update_Variable(vb2b_update_id,locupdate1=(/  nn_shift_bar+nn_dist_par_bc  ,-2/),locupdate2=(/  nn_shift_bar+nn_dist_par_bc-1,-2/),procname = updatevmsk)
         ENDIF
      END IF

# if ! defined DECAL_FEEDBACK
      CALL Agrif_Update_Variable(un_update_id,procname = updateU)
      CALL Agrif_Update_Variable(vn_update_id,procname = updateV)
! near boundary update:
!      CALL Agrif_Update_Variable(un_update_id,locupdate=(/0,1/),procname = updateU)
!      CALL Agrif_Update_Variable(vn_update_id,locupdate=(/0,1/),procname = updateV)
# else
      CALL Agrif_Update_Variable(un_update_id,locupdate1=(/0,-1/),locupdate2=(/1,-2/),procname = updateU)
      CALL Agrif_Update_Variable(vn_update_id,locupdate1=(/1,-2/),locupdate2=(/0,-1/),procname = updateV)
! near boundary update:
!      CALL Agrif_Update_Variable(un_update_id,locupdate1=(/0,1/),locupdate2=(/1,1/),procname = updateU)
!      CALL Agrif_Update_Variable(vn_update_id,locupdate1=(/1,1/),locupdate2=(/0,1/),procname = updateV)
# endif
      !
      use_sign_north = .FALSE.
      l_vremap       = .FALSE.
      !
   END SUBROUTINE Agrif_Update_Dyn

   SUBROUTINE Agrif_Update_ssh( )
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_Update_ssh ***
      !!---------------------------------------------
      ! 
      IF (Agrif_Root()) RETURN
      !
      Agrif_UseSpecialValueInUpdate = .FALSE.
      Agrif_SpecialValueFineGrid    = 0._wp
# if ! defined DECAL_FEEDBACK_2D
      CALL Agrif_Update_Variable(sshn_id,locupdate=(/  nn_shift_bar,-2/), procname = updateSSH) 
# else
      CALL Agrif_Update_Variable(sshn_id,locupdate=(/1+nn_shift_bar,-2/), procname = updateSSH)
# endif
      IF (lk_agrif_fstep) THEN
         CALL Agrif_Update_Variable(sshn_id,locupdate=(/1+nn_shift_bar+nn_dist_par_bc-1,-2/),procname = updatetmsk)
      ENDIF
      !
#  if defined VOL_REFLUX
      IF ( ln_dynspg_ts.AND.ln_bt_fw ) THEN 
         use_sign_north = .TRUE.
         sign_north = -1._wp
         ! Refluxing on ssh:
#  if defined DECAL_FEEDBACK_2D
         CALL Agrif_Update_Variable(ub2b_update_id,locupdate1=(/nn_shift_bar,nn_shift_bar/),locupdate2=(/1+nn_shift_bar,1+nn_shift_bar/),procname = reflux_sshu)
         CALL Agrif_Update_Variable(vb2b_update_id,locupdate1=(/1+nn_shift_bar,1+nn_shift_bar/),locupdate2=(/nn_shift_bar,nn_shift_bar/),procname = reflux_sshv)
#  else
         CALL Agrif_Update_Variable(ub2b_update_id,locupdate1=(/-1+nn_shift_bar,-1+nn_shift_bar/),locupdate2=(/nn_shift_bar, nn_shift_bar/),procname = reflux_sshu)
         CALL Agrif_Update_Variable(vb2b_update_id,locupdate1=(/ nn_shift_bar, nn_shift_bar/),locupdate2=(/-1+nn_shift_bar,-1+nn_shift_bar/),procname = reflux_sshv)
#  endif
         use_sign_north = .FALSE.
      END IF
#  endif
      Agrif_UseSpecialValueInUpdate = .FALSE.
      !
   END SUBROUTINE Agrif_Update_ssh

   SUBROUTINE Agrif_Update_Tke( )
      !!---------------------------------------------
      !!   *** ROUINE Agrif_Update_Tke ***
      !!---------------------------------------------
      !!
      ! 
      IF (Agrif_Root()) RETURN
      !       
      Agrif_UseSpecialValueInUpdate = .TRUE.
      Agrif_SpecialValueFineGrid = 0._wp

      CALL Agrif_Update_Variable( en_id, locupdate=(/0,0/), procname=updateEN  )
      CALL Agrif_Update_Variable(avt_id, locupdate=(/0,0/), procname=updateAVT )
      CALL Agrif_Update_Variable(avm_id, locupdate=(/0,0/), procname=updateAVM )

      Agrif_UseSpecialValueInUpdate = .FALSE.
      
   END SUBROUTINE Agrif_Update_Tke

   SUBROUTINE Agrif_Update_vvl( )
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_Update_vvl ***
      !!---------------------------------------------
      !
      IF (Agrif_Root()) RETURN
      !
      IF (lwp.AND.lk_agrif_debug) Write(*,*) 'Update e3 from grid Number',Agrif_Fixed(), 'Step', Agrif_Nb_Step()
      !
#if defined key_qco
      !
      Agrif_UseSpecialValueInUpdate = .FALSE.
#if ! defined DECAL_FEEDBACK_2D
      CALL Agrif_Update_Variable(r3t_id,  locupdate=(/  nn_shift_bar,-2/), procname=update_r3t) 
      CALL Agrif_Update_Variable(r3f_id,  locupdate=(/  nn_shift_bar,-2/), procname=update_r3f) 
      CALL Agrif_Update_Variable(r3u_id, locupdate1=(/  nn_shift_bar,-2/), locupdate2=(/  nn_shift_bar,-2/), procname=update_r3u) 
      CALL Agrif_Update_Variable(r3v_id, locupdate1=(/  nn_shift_bar,-2/), locupdate2=(/  nn_shift_bar,-2/), procname=update_r3v) 
#else
      CALL Agrif_Update_Variable(r3t_id,  locupdate=(/1+nn_shift_bar,-2/), procname=update_r3t) 
      CALL Agrif_Update_Variable(r3f_id,  locupdate=(/1+nn_shift_bar,-2/), procname=update_r3f) 
      CALL Agrif_Update_Variable(r3u_id, locupdate1=(/  nn_shift_bar,-2/), locupdate2=(/1+nn_shift_bar,-2/), procname=update_r3u) 
      CALL Agrif_Update_Variable(r3v_id, locupdate1=(/1+nn_shift_bar,-2/), locupdate2=(/  nn_shift_bar,-2/), procname=update_r3v) 
#endif
      !
      ! Old way (update e3 at UVF-points everywhere on parent domain):
!      CALL Agrif_ChildGrid_To_ParentGrid()
!      CALL Agrif_Update_qco
!      CALL Agrif_ParentGrid_To_ChildGrid()
#elif defined key_linssh
      !
      ! DO NOTHING HERE
#else
      Agrif_UseSpecialValueInUpdate = .FALSE.
      l_vremap                      = ln_vert_remap
#if ! defined DECAL_FEEDBACK_2D
      CALL Agrif_Update_Variable(e3t_id,  locupdate=(/  nn_shift_bar,-2/), procname=update_e3t) 
      CALL Agrif_Update_Variable(e3f_id,  locupdate=(/  nn_shift_bar,-2/), procname=update_e3f) 
      CALL Agrif_Update_Variable(e3u_id, locupdate1=(/  nn_shift_bar,-2/), locupdate2=(/  nn_shift_bar,-2/), procname=update_e3u) 
      CALL Agrif_Update_Variable(e3v_id, locupdate1=(/  nn_shift_bar,-2/), locupdate2=(/  nn_shift_bar,-2/), procname=update_e3v) 
#else
      CALL Agrif_Update_Variable(e3t_id,  locupdate=(/1+nn_shift_bar,-2/), procname=update_e3t) 
      CALL Agrif_Update_Variable(e3f_id,  locupdate=(/1+nn_shift_bar,-2/), procname=update_e3f) 
      CALL Agrif_Update_Variable(e3u_id, locupdate1=(/  nn_shift_bar,-2/), locupdate2=(/1+nn_shift_bar,-2/), procname=update_e3u) 
      CALL Agrif_Update_Variable(e3v_id, locupdate1=(/1+nn_shift_bar,-2/), locupdate2=(/  nn_shift_bar,-2/), procname=update_e3v) 
#endif
      l_vremap                      = .FALSE. 
      !
! Old way (update e3 at UVF-points everywhere on parent domain):
!      CALL Agrif_ChildGrid_To_ParentGrid()
!      CALL dom_vvl_update_UVF
!      CALL Agrif_ParentGrid_To_ChildGrid()
#endif
      !
   END SUBROUTINE Agrif_Update_vvl

#if defined key_qco
   SUBROUTINE Agrif_Update_qco
      !!---------------------------------------------
      !!       *** ROUTINE dom_Update_qco ***
      !!---------------------------------------------
      !
      ! Save arrays prior update (needed for asselin correction)
      r3t(:,:,Krhs_a) = r3t(:,:,Kmm_a)
      r3u(:,:,Krhs_a) = r3u(:,:,Kmm_a)
      r3v(:,:,Krhs_a) = r3v(:,:,Kmm_a)

      ! Update r3x arrays from updated ssh
      CALL dom_qco_zgr( Kbb_a, Kmm_a )
      !
   END SUBROUTINE Agrif_Update_qco
#endif

#if ! defined key_qco   &&   ! defined key_linssh
   SUBROUTINE dom_vvl_update_UVF
      !!---------------------------------------------
      !!       *** ROUTINE dom_vvl_update_UVF ***
      !!---------------------------------------------
      !!
      INTEGER :: jk
      REAL(wp):: zcoef
      !!---------------------------------------------
      IF (lwp.AND.lk_agrif_debug) Write(*,*) 'Finalize e3 on grid Number', &
                  & Agrif_Fixed(), 'Step', Agrif_Nb_Step()

      ! Save "old" scale factor (prior update) for subsequent asselin correction
      ! of prognostic variables
      ! -----------------------
      !
      e3u(:,:,:,Krhs_a) = e3u(:,:,:,Kmm_a)
      e3v(:,:,:,Krhs_a) = e3v(:,:,:,Kmm_a)
      hu(:,:,Krhs_a) = hu(:,:,Kmm_a)
      hv(:,:,Krhs_a) = hv(:,:,Kmm_a)

      ! 1) NOW fields
      !--------------
      
         ! Vertical scale factor interpolations
         ! ------------------------------------
      CALL dom_vvl_interpol( e3t(:,:,:,Kmm_a), e3u(:,:,:,Kmm_a) ,  'U' )
      CALL dom_vvl_interpol( e3t(:,:,:,Kmm_a), e3v(:,:,:,Kmm_a) ,  'V' )
      CALL dom_vvl_interpol( e3u(:,:,:,Kmm_a), e3f(:,:,:) ,  'F' )

      CALL dom_vvl_interpol( e3u(:,:,:,Kmm_a), e3uw(:,:,:,Kmm_a), 'UW' )
      CALL dom_vvl_interpol( e3v(:,:,:,Kmm_a), e3vw(:,:,:,Kmm_a), 'VW' )

         ! Update total depths:
         ! --------------------
      hu(:,:,Kmm_a) = 0._wp                    ! Ocean depth at U-points
      hv(:,:,Kmm_a) = 0._wp                    ! Ocean depth at V-points
      DO jk = 1, jpkm1
         hu(:,:,Kmm_a) = hu(:,:,Kmm_a) + e3u(:,:,jk,Kmm_a) * umask(:,:,jk)
         hv(:,:,Kmm_a) = hv(:,:,Kmm_a) + e3v(:,:,jk,Kmm_a) * vmask(:,:,jk)
      END DO
      !                                        ! Inverse of the local depth
      r1_hu(:,:,Kmm_a) = ssumask(:,:) / ( hu(:,:,Kmm_a) + 1._wp - ssumask(:,:) )
      r1_hv(:,:,Kmm_a) = ssvmask(:,:) / ( hv(:,:,Kmm_a) + 1._wp - ssvmask(:,:) )


      ! 2) BEFORE fields:
      !------------------
      IF (.NOT.(lk_agrif_fstep.AND.(l_1st_euler) )) THEN
         !
         ! Vertical scale factor interpolations
         ! ------------------------------------
         CALL dom_vvl_interpol( e3t(:,:,:,Kbb_a), e3u(:,:,:,Kbb_a),  'U'  )
         CALL dom_vvl_interpol( e3t(:,:,:,Kbb_a), e3v(:,:,:,Kbb_a),  'V'  )

         CALL dom_vvl_interpol( e3u(:,:,:,Kbb_a), e3uw(:,:,:,Kbb_a), 'UW' )
         CALL dom_vvl_interpol( e3v(:,:,:,Kbb_a), e3vw(:,:,:,Kbb_a), 'VW' )

         ! Update total depths:
         ! --------------------
         hu(:,:,Kbb_a) = 0._wp                     ! Ocean depth at U-points
         hv(:,:,Kbb_a) = 0._wp                     ! Ocean depth at V-points
         DO jk = 1, jpkm1
            hu(:,:,Kbb_a) = hu(:,:,Kbb_a) + e3u(:,:,jk,Kbb_a) * umask(:,:,jk)
            hv(:,:,Kbb_a) = hv(:,:,Kbb_a) + e3v(:,:,jk,Kbb_a) * vmask(:,:,jk)
         END DO
         !                                     ! Inverse of the local depth
         r1_hu(:,:,Kbb_a) = ssumask(:,:) / ( hu(:,:,Kbb_a) + 1._wp - ssumask(:,:) )
         r1_hv(:,:,Kbb_a) = ssvmask(:,:) / ( hv(:,:,Kbb_a) + 1._wp - ssvmask(:,:) )
      ENDIF
      !
   END SUBROUTINE dom_vvl_update_UVF
#endif


   SUBROUTINE updateTS( tabres, i1, i2, j1, j2, k1, k2, n1, n2, before )
      !!----------------------------------------------------------------------
      !!           *** ROUTINE updateT ***
      !!---------------------------------------------
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2,n1,n2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before
      !!
      INTEGER  :: ji,jj,jk,jn
      INTEGER  :: N_in, N_out
      REAL(wp) :: ztb, ztnu, ztno, ze3b
      REAL(wp) :: h_in(k1:k2)
      REAL(wp) :: h_out(1:jpk)
      REAL(wp) :: tabin(k1:k2,1:jpts)
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk,1:jpts) :: tabres_child
      !!---------------------------------------------
      !
      IF (before) THEN
         DO jn = n1,n2-1
            DO jk=k1,k2-1
               DO jj=j1,j2
                  DO ji=i1,i2
                     tabres(ji,jj,jk,jn) = ts(ji,jj,jk,jn,Kmm_a) * e3t(ji,jj,jk,Kmm_a)  &
                                         & * e1e2t_frac(ji,jj)
                  END DO
               END DO
            END DO
         END DO

         IF ( l_vremap ) THEN
            DO jk=k1,k2-1
               DO jj=j1,j2
                  DO ji=i1,i2
                     tabres(ji,jj,jk,n2) = tmask(ji,jj,jk) * e3t(ji,jj,jk,Kmm_a)  &
                                         & * e1e2t_frac(ji,jj)
                  END DO
               END DO
            END DO
         ENDIF

      ELSE
         tabres_child(:,:,:,:) = 0._wp
         IF ( l_vremap ) THEN
            AGRIF_SpecialValue = 0._wp
            DO jj=j1,j2
               DO ji=i1,i2
                  N_in = 0
                  DO jk=k1,k2-1 !k2 = jpk of child grid
                     IF (tabres(ji,jj,jk,n2) <= 1.e-6_wp  ) EXIT
                     N_in = N_in + 1
                     DO jn=n1,n2-1
                        tabin(jk,jn) = tabres(ji,jj,jk,jn)/tabres(ji,jj,jk,n2)
                     END DO
                     h_in(N_in) = tabres(ji,jj,jk,n2)
                  ENDDO
                  N_out = 0
                  DO jk=1,jpkm1 ! jpk of parent grid
                     IF (tmask(ji,jj,jk) == 0 ) EXIT ! TODO: Will not work with ISF
                     N_out = N_out + 1
                     h_out(N_out) = e3t(ji,jj,jk,Kmm_a) 
                  ENDDO
                  IF (N_in*N_out > 0) THEN !Remove this?
                     CALL reconstructandremap(tabin(1:N_in,1:jpts),h_in(1:N_in),tabres_child(ji,jj,1:N_out,1:jpts),h_out(1:N_out),N_in,N_out,jpts)
                  ENDIF
               ENDDO
            ENDDO
         ELSE
            DO jn = 1, jpts
               DO jk = k1, k2-1
                  tabres_child(i1:i2,j1:j2,jk,jn) =  tabres(i1:i2,j1:j2,jk,jn) / e3t(i1:i2,j1:j2,jk,Kmm_a) * tmask(i1:i2,j1:j2,jk) 
               ENDDO
            ENDDO
         ENDIF

         IF (.NOT.(lk_agrif_fstep.AND.(l_1st_euler))) THEN
            ! Add asselin part
            DO jn = 1, jpts
               DO jk = 1, jpkm1
                  DO jj = j1, j2
                     DO ji = i1, i2
                        ze3b = e3t(ji,jj,jk,Kbb_a) & ! Recover e3tb before update
                             & - rn_atfp * ( e3t(ji,jj,jk,Kmm_a) - e3t(ji,jj,jk,Krhs_a) )
                        ztb  = ts(ji,jj,jk,jn,Kbb_a) * ze3b
                        ztnu = tabres_child(ji,jj,jk,jn) * e3t(ji,jj,jk,Kmm_a)
                        ztno = ts(ji,jj,jk,jn,Kmm_a) * e3t(ji,jj,jk,Krhs_a)
                        ts(ji,jj,jk,jn,Kbb_a) = ( ztb + rn_atfp * ( ztnu - ztno) )  & 
                                     &          / e3t(ji,jj,jk,Kbb_a)
                     END DO
                  END DO
               END DO
            END DO
         ENDIF
         DO jn = 1,jpts
            DO jk = 1, jpkm1
               DO jj = j1, j2
                  DO ji = i1, i2
                     ts(ji,jj,jk,jn,Kmm_a) = tabres_child(ji,jj,jk,jn)
                  END DO
               END DO
            END DO
         END DO
         !
         IF  ((l_1st_euler).AND.(Agrif_Nb_Step()==0) ) THEN
            ts(i1:i2,j1:j2,1:jpkm1,1:jpts,Kbb_a)  = ts(i1:i2,j1:j2,1:jpkm1,1:jpts,Kmm_a)
         ENDIF         
      ENDIF
      ! 
   END SUBROUTINE updateTS


   SUBROUTINE updateu( tabres, i1, i2, j1, j2, k1, k2, n1, n2, before )
      !!---------------------------------------------
      !!           *** ROUTINE updateu ***
      !!---------------------------------------------
      INTEGER                                     , INTENT(in   ) :: i1, i2, j1, j2, k1, k2, n1, n2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2), INTENT(inout) :: tabres
      LOGICAL                                     , INTENT(in   ) :: before
      !
      INTEGER ::   ji, jj, jk
      REAL(wp)::   zub, zunu, zuno, ze3b
      REAL(wp), DIMENSION(jpi,jpj) ::   zpgu   ! 2D workspace
! VERTICAL REFINEMENT BEGIN
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk) :: tabres_child
      REAL(wp) :: h_in(k1:k2)
      REAL(wp) :: h_out(1:jpk)
      INTEGER  :: N_in, N_out, N_in_save, N_out_save
      REAL(wp) :: zhmin, zd
      REAL(wp) :: tabin(k1:k2)
! VERTICAL REFINEMENT END
      !!---------------------------------------------
      ! 
      IF( before ) THEN
         DO jk=k1,k2
            tabres(i1:i2,j1:j2,jk,1) = e2u_frac(i1:i2,j1:j2) * e3u(i1:i2,j1:j2,jk,Kmm_a) & 
                                     &   * umask(i1:i2,j1:j2,jk) * uu(i1:i2,j1:j2,jk,Kmm_a)  
         END DO

         IF ( l_vremap ) THEN
            DO jk=k1,k2
               tabres(i1:i2,j1:j2,jk,2) =  e2u_frac(i1:i2,j1:j2) * e3u(i1:i2,j1:j2,jk,Kmm_a) &
                                     &   * umask(i1:i2,j1:j2,jk)
            END DO
         ENDIF

      ELSE

         tabres_child(:,:,:) = 0._wp

         IF ( l_vremap ) THEN

            DO jj=j1,j2
               DO ji=i1,i2
                  N_in = 0
                  h_in(:) = 0._wp
                  tabin(:) = 0._wp
                  DO jk=k1,k2-1 !k2=jpk of child grid
                     IF( tabres(ji,jj,jk,2) <= 1.e-6_wp ) EXIT
                     N_in = N_in + 1
                     tabin(jk) = tabres(ji,jj,jk,1)/tabres(ji,jj,jk,2)
                     h_in(N_in) = tabres(ji,jj,jk,2)
                  ENDDO
                  N_out = 0
                  DO jk=1,jpkm1
                     IF (umask(ji,jj,jk) == 0._wp) EXIT
                     N_out = N_out + 1
                     h_out(N_out) = e3u(ji,jj,jk,Kmm_a)
                  ENDDO
                  IF (N_in * N_out > 0) THEN
                     ! Deal with potentially different depths at velocity points:
                     N_in_save  = N_in
                     N_out_save = N_out
                     IF ( ABS(sum(h_out(1:N_out))-sum(h_in(1:N_in))) > 1.e-6_wp ) THEN
                        zhmin = MIN(sum(h_out(1:N_out)), sum(h_in(1:N_in)))
                        zd = 0._wp
                        DO jk=1, N_in_save
                           IF ( (zd +  h_in(jk)) > zhmin-1.e-6) THEN
                              N_in = jk
                              h_in(jk) = zhmin - zd
                              EXIT 
                           ENDIF
                           zd = zd + h_in(jk)
                        END DO
                        zd = 0._wp
                        DO jk=1, N_out_save
                           IF ( (zd +  h_out(jk)) > zhmin-1.e-6) THEN
                              N_out = jk
                              h_out(jk) = zhmin - zd
                              EXIT 
                           ENDIF
                           zd = zd + h_out(jk)
                        END DO
                     END IF
                     CALL reconstructandremap(tabin(1:N_in),h_in(1:N_in),tabres_child(ji,jj,1:N_out),h_out(1:N_out),N_in,N_out,1)
                     IF (N_out < N_out_save) tabres_child(ji,jj,N_out+1:N_out_save) = tabres_child(ji,jj,N_out)
                  ENDIF
               ENDDO
            ENDDO

         ELSE
            DO jk=k1,k2-1
               DO jj=j1,j2
                  DO ji=i1,i2
                     tabres_child(ji,jj,jk) = tabres(ji,jj,jk,1) /  e3u(ji,jj,jk,Kmm_a)
                  END DO
               END DO
            END DO
         ENDIF
         !
         DO jk=1,jpkm1
            DO jj=j1,j2
               DO ji=i1,i2
                  IF (.NOT.(lk_agrif_fstep.AND.(l_1st_euler))) THEN ! Add asselin part
                     ze3b = e3u(ji,jj,jk,Kbb_a) & ! Recover e3ub before update
                          & - rn_atfp * ( e3u(ji,jj,jk,Kmm_a) - e3u(ji,jj,jk,Krhs_a) )
                     zub  = uu(ji,jj,jk,Kbb_a) * ze3b 
                     zuno = uu(ji,jj,jk,Kmm_a) * e3u(ji,jj,jk,Krhs_a)
                     zunu = tabres_child(ji,jj,jk) * e3u(ji,jj,jk,Kmm_a)
                     uu(ji,jj,jk,Kbb_a) = ( zub + rn_atfp * ( zunu - zuno) ) &      
                                    & * umask(ji,jj,jk) / e3u(ji,jj,jk,Kbb_a)
                  ENDIF
                  !
                  uu(ji,jj,jk,Kmm_a) = tabres_child(ji,jj,jk) * umask(ji,jj,jk)
               END DO
            END DO
         END DO
         !
         ! Correct now and before transports:
         DO jj=j1,j2
            DO ji=i1,i2
               zpgu(ji,jj) = 0._wp
               DO jk=1,jpkm1
                  zpgu(ji,jj) = zpgu(ji,jj) + e3u(ji,jj,jk,Kmm_a) * uu(ji,jj,jk,Kmm_a)
               END DO
               !
               DO jk=1,jpkm1              
                  uu(ji,jj,jk,Kmm_a) = uu(ji,jj,jk,Kmm_a) + &
                      &  (uu_b(ji,jj,Kmm_a) - zpgu(ji,jj) * r1_hu(ji,jj,Kmm_a)) * umask(ji,jj,jk)           
               END DO
               !
               zpgu(ji,jj) = 0._wp
               DO jk=1,jpkm1
                  zpgu(ji,jj) = zpgu(ji,jj) + e3u(ji,jj,jk,Kbb_a) * uu(ji,jj,jk,Kbb_a)
               END DO
               !
               DO jk=1,jpkm1              
                  uu(ji,jj,jk,Kbb_a) = uu(ji,jj,jk,Kbb_a) + &
                      &  (uu_b(ji,jj,Kbb_a) - zpgu(ji,jj) * r1_hu(ji,jj,Kbb_a)) * umask(ji,jj,jk)           
               END DO
               !
            END DO
         END DO
         !
         IF  ((l_1st_euler).AND.(Agrif_Nb_Step()==0) ) THEN
            uu(i1:i2,j1:j2,1:jpkm1,Kbb_a)  = uu(i1:i2,j1:j2,1:jpkm1,Kmm_a)
         ENDIF
         !
      ENDIF
      ! 
   END SUBROUTINE updateu


   SUBROUTINE updatev( tabres, i1, i2, j1, j2, k1, k2, n1, n2, before )
      !!---------------------------------------------
      !!           *** ROUTINE updatev ***
      !!---------------------------------------------
      INTEGER                                     , INTENT(in   ) :: i1, i2, j1, j2, k1, k2, n1, n2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2), INTENT(inout) :: tabres
      LOGICAL                                     , INTENT(in   ) :: before
      !
      INTEGER  ::   ji, jj, jk
      REAL(wp) ::   zvb, zvnu, zvno, ze3b
      REAL(wp), DIMENSION(jpi,jpj) ::   zpgv   ! 2D workspace
! VERTICAL REFINEMENT BEGIN
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk) :: tabres_child
      REAL(wp) :: h_in(k1:k2)
      REAL(wp) :: h_out(1:jpk)
      INTEGER  :: N_in, N_out, N_in_save, N_out_save
      REAL(wp) :: zhmin, zd
      REAL(wp) :: tabin(k1:k2)
! VERTICAL REFINEMENT END
      !!---------------------------------------------      
      !
      IF( before ) THEN
         DO jk=k1,k2
            tabres(i1:i2,j1:j2,jk,1) = e1v_frac(i1:i2,j1:j2) * e3v(i1:i2,j1:j2,jk,Kmm_a) & 
                                     &   * vmask(i1:i2,j1:j2,jk) * vv(i1:i2,j1:j2,jk,Kmm_a)  
         END DO

         IF ( l_vremap ) THEN
            DO jk=k1,k2
               tabres(i1:i2,j1:j2,jk,2) = e1v_frac(i1:i2,j1:j2) * e3v(i1:i2,j1:j2,jk,Kmm_a) & 
                                     &   * vmask(i1:i2,j1:j2,jk)
            END DO
         ENDIF

      ELSE

         tabres_child(:,:,:) = 0._wp

         IF ( l_vremap ) THEN

            DO jj=j1,j2
               DO ji=i1,i2
                  N_in = 0
                  DO jk=k1,k2-1
                     IF (tabres(ji,jj,jk,2) <= 1.e-6_wp) EXIT
                     N_in = N_in + 1
                     tabin(jk) = tabres(ji,jj,jk,1)/tabres(ji,jj,jk,2)
                     h_in(N_in) = tabres(ji,jj,jk,2)
                  ENDDO
                  N_out = 0
                  DO jk=1,jpkm1
                     IF (vmask(ji,jj,jk) == 0._wp) EXIT
                     N_out = N_out + 1
                     h_out(N_out) = e3v(ji,jj,jk,Kmm_a)
                  ENDDO
                  IF (N_in * N_out > 0) THEN
                     ! Deal with potentially different depths at velocity points:
                     N_in_save  = N_in
                     N_out_save = N_out
                     IF ( ABS(sum(h_out(1:N_out))-sum(h_in(1:N_in))) > 1.e-6_wp ) THEN
                        zhmin = MIN(sum(h_out(1:N_out)), sum(h_in(1:N_in)))
                        zd = 0._wp
                        DO jk=1, N_in_save
                           IF ( (zd +  h_in(jk)) > zhmin-1.e-6) THEN
                              N_in = jk
                              h_in(jk) = zhmin - zd
                              EXIT 
                           ENDIF
                           zd = zd + h_in(jk)
                        END DO
                        zd = 0._wp
                        DO jk=1, N_out_save
                           IF ( (zd +  h_out(jk)) > zhmin-1.e-6) THEN
                              N_out = jk
                              h_out(jk) = zhmin - zd
                              EXIT 
                           ENDIF
                           zd = zd + h_out(jk)
                        END DO
                     END IF
                     CALL reconstructandremap(tabin(1:N_in),h_in(1:N_in),tabres_child(ji,jj,1:N_out),h_out(1:N_out),N_in,N_out,1)
                     IF (N_out < N_out_save) tabres_child(ji,jj,N_out+1:N_out_save) = tabres_child(ji,jj,N_out)
                  ENDIF
               ENDDO
            ENDDO

         ELSE
            DO jk=k1,k2-1
               DO jj=j1,j2
                  DO ji=i1,i2
                     tabres_child(ji,jj,jk) = tabres(ji,jj,jk,1) /  e3v(ji,jj,jk,Kmm_a)
                  END DO
               END DO
            END DO
         ENDIF
         !
         DO jk=1,jpkm1
            DO jj=j1,j2
               DO ji=i1,i2
                  IF (.NOT.(lk_agrif_fstep.AND.(l_1st_euler))) THEN ! Add asselin part
                     ze3b = e3v(ji,jj,jk,Kbb_a) & ! Recover e3vb before update
                          & - rn_atfp * ( e3v(ji,jj,jk,Kmm_a) - e3v(ji,jj,jk,Krhs_a) )
                     zvb  = vv(ji,jj,jk,Kbb_a) * ze3b 
                     zvno = vv(ji,jj,jk,Kmm_a) * e3v(ji,jj,jk,Krhs_a)
                     zvnu = tabres_child(ji,jj,jk) * e3v(ji,jj,jk,Kmm_a)
                     vv(ji,jj,jk,Kbb_a) = ( zvb + rn_atfp * ( zvnu - zvno) ) &      
                                    & * vmask(ji,jj,jk) / e3v(ji,jj,jk,Kbb_a)
                  ENDIF
                  !
                  vv(ji,jj,jk,Kmm_a) = tabres_child(ji,jj,jk) * vmask(ji,jj,jk)
               END DO
            END DO
         END DO
         !
         ! Correct now and before transports:
         DO jj=j1,j2
            DO ji=i1,i2
               zpgv(ji,jj) = 0._wp
               DO jk=1,jpkm1
                  zpgv(ji,jj) = zpgv(ji,jj) + e3v(ji,jj,jk,Kmm_a) * vv(ji,jj,jk,Kmm_a)
               END DO
               !
               DO jk=1,jpkm1              
                  vv(ji,jj,jk,Kmm_a) = vv(ji,jj,jk,Kmm_a) + &
                      &  (vv_b(ji,jj,Kmm_a) - zpgv(ji,jj) * r1_hv(ji,jj,Kmm_a)) * vmask(ji,jj,jk)           
               END DO
               !
               zpgv(ji,jj) = 0._wp
               DO jk=1,jpkm1
                  zpgv(ji,jj) = zpgv(ji,jj) + e3v(ji,jj,jk,Kbb_a) * vv(ji,jj,jk,Kbb_a)
               END DO
               !
               DO jk=1,jpkm1              
                  vv(ji,jj,jk,Kbb_a) = vv(ji,jj,jk,Kbb_a) + &
                      &  (vv_b(ji,jj,Kbb_a) - zpgv(ji,jj) * r1_hv(ji,jj,Kbb_a)) * vmask(ji,jj,jk)           
               END DO
               !
            END DO
         END DO
         !
         IF  ((l_1st_euler).AND.(Agrif_Nb_Step()==0) ) THEN
            vv(i1:i2,j1:j2,1:jpkm1,Kbb_a)  = vv(i1:i2,j1:j2,1:jpkm1,Kmm_a)
         ENDIF
         !
      ENDIF
      ! 
   END SUBROUTINE updatev


   SUBROUTINE updateu2d( tabres, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                      *** ROUTINE updateu2d ***
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   tabres
      LOGICAL                         , INTENT(in   ) ::   before
      !!
      REAL(wp), DIMENSION(jpi,jpj) ::   zpgu   ! 2D workspace
      !! 
      INTEGER  :: ji, jj, jk
      REAL(wp) :: zcorr
      !!---------------------------------------------
      !
      IF( before ) THEN
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj) = uu_b(ji,jj,Kmm_a) * hu(ji,jj,Kmm_a) * e2u_frac(ji,jj)
            END DO
         END DO
      ELSE
         DO jj=j1,j2
            DO ji=i1,i2
               !    
               ! Update barotropic velocities:
               IF ( .NOT.ln_dynspg_ts .OR. (ln_dynspg_ts.AND.(.NOT.ln_bt_fw)) ) THEN
                  IF (.NOT.(lk_agrif_fstep.AND.(l_1st_euler))) THEN ! Add asselin part
                     zcorr = (tabres(ji,jj) - uu_b(ji,jj,Kmm_a) * hu(ji,jj,Krhs_a)) * r1_hu(ji,jj,Kbb_a)
                     uu_b(ji,jj,Kbb_a) = uu_b(ji,jj,Kbb_a) + rn_atfp * zcorr * umask(ji,jj,1)
                  END IF
               ENDIF    
               uu_b(ji,jj,Kmm_a) = tabres(ji,jj) * r1_hu(ji,jj,Kmm_a) * umask(ji,jj,1)
               !       
            END DO
         END DO
         !
         ! Correct now and before 3d velocities (needed in case of interface shift)
         DO jj=j1,j2
            DO ji=i1,i2
               zpgu(ji,jj) = 0._wp
               DO jk=1,jpkm1
                  zpgu(ji,jj) = zpgu(ji,jj) + e3u(ji,jj,jk,Kmm_a) * uu(ji,jj,jk,Kmm_a)
               END DO
               !
               DO jk=1,jpkm1
                  uu(ji,jj,jk,Kmm_a) = uu(ji,jj,jk,Kmm_a) + &
                  &  (uu_b(ji,jj,Kmm_a) - zpgu(ji,jj) * r1_hu(ji,jj,Kmm_a))  * umask(ji,jj,jk)
               END DO
               !
               zpgu(ji,jj) = 0._wp
               DO jk=1,jpkm1
                  zpgu(ji,jj) = zpgu(ji,jj) + e3u(ji,jj,jk,Kbb_a) * uu(ji,jj,jk,Kbb_a)
               END DO
               !
               DO jk=1,jpkm1
                  uu(ji,jj,jk,Kbb_a) = uu(ji,jj,jk,Kbb_a) + &
                  &  (uu_b(ji,jj,Kbb_a) - zpgu(ji,jj) * r1_hu(ji,jj,Kbb_a)) * umask(ji,jj,jk)
               END DO
               !
            END DO
         END DO
         !
         IF  ((l_1st_euler).AND.(Agrif_Nb_Step()==0) ) THEN
            uu_b(i1:i2,j1:j2,Kbb_a)  = uu_b(i1:i2,j1:j2,Kmm_a)
         ENDIF
         !
      ENDIF
      !
   END SUBROUTINE updateu2d


   SUBROUTINE updatev2d( tabres, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                   *** ROUTINE updatev2d ***
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   tabres
      LOGICAL                         , INTENT(in   ) ::   before
      !
      REAL(wp), DIMENSION(jpi,jpj) ::   zpgv   ! 2D workspace
      ! 
      INTEGER  :: ji, jj, jk
      REAL(wp) :: zcorr
      !!----------------------------------------------------------------------
      !
      IF( before ) THEN
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj) = vv_b(ji,jj,Kmm_a) * hv(ji,jj,Kmm_a) * e1v_frac(ji,jj) 
            END DO
         END DO
      ELSE
         DO jj=j1,j2
            DO ji=i1,i2
               ! Update barotropic velocities:
               IF ( .NOT.ln_dynspg_ts .OR. (ln_dynspg_ts.AND.(.NOT.ln_bt_fw)) ) THEN
                  IF (.NOT.(lk_agrif_fstep.AND.(l_1st_euler))) THEN ! Add asselin part
                     zcorr = (tabres(ji,jj) - vv_b(ji,jj,Kmm_a) * hv(ji,jj,Krhs_a)) * r1_hv(ji,jj,Kbb_a)
                     vv_b(ji,jj,Kbb_a) = vv_b(ji,jj,Kbb_a) + rn_atfp * zcorr * vmask(ji,jj,1)
                  END IF
               ENDIF              
               vv_b(ji,jj,Kmm_a) = tabres(ji,jj) * r1_hv(ji,jj,Kmm_a) * vmask(ji,jj,1)
               !       
            END DO
         END DO
         !
         ! Correct now and before 3d velocities (in case of interface shift):
         DO jj=j1,j2
            DO ji=i1,i2
               zpgv(ji,jj) = 0._wp
               DO jk=1,jpkm1
                  zpgv(ji,jj) = zpgv(ji,jj) + e3v(ji,jj,jk,Kmm_a) * vv(ji,jj,jk,Kmm_a) 
               END DO
               !
               DO jk=1,jpkm1
                  vv(ji,jj,jk,Kmm_a) = vv(ji,jj,jk,Kmm_a) + &
                   &  (vv_b(ji,jj,Kmm_a) - zpgv(ji,jj) * r1_hv(ji,jj,Kmm_a)) * vmask(ji,jj,jk)
               END DO
               !
               zpgv(ji,jj) = 0._wp
               DO jk=1,jpkm1
                  zpgv(ji,jj) = zpgv(ji,jj) + e3v(ji,jj,jk,Kbb_a) * vv(ji,jj,jk,Kbb_a)
               END DO
               !
               DO jk=1,jpkm1
                  vv(ji,jj,jk,Kbb_a) = vv(ji,jj,jk,Kbb_a) + &
                      &  (vv_b(ji,jj,Kbb_a) - zpgv(ji,jj) * r1_hv(ji,jj,Kbb_a)) * vmask(ji,jj,jk)
               END DO
               !
            END DO
         END DO
         !
         IF  ((l_1st_euler).AND.(Agrif_Nb_Step()==0) ) THEN
            vv_b(i1:i2,j1:j2,Kbb_a)  = vv_b(i1:i2,j1:j2,Kmm_a)
         ENDIF
         !
      ENDIF
      ! 
   END SUBROUTINE updatev2d


   SUBROUTINE updateSSH( tabres, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                   *** ROUTINE updateSSH ***
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   tabres
      LOGICAL                         , INTENT(in   ) ::   before
      !!
      INTEGER :: ji, jj
      !!----------------------------------------------------------------------
      ! 
      IF( before ) THEN
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj) = e1e2t_frac(ji,jj) * ssh(ji,jj,Kmm_a) 
            END DO
         END DO
      ELSE
         !
         IF (.NOT.(lk_agrif_fstep.AND.(l_1st_euler))) THEN
            DO jj=j1,j2
               DO ji=i1,i2
                  ssh(ji,jj,Kbb_a) =   ssh(ji,jj,Kbb_a) &
                        & + rn_atfp * ( tabres(ji,jj) - ssh(ji,jj,Kmm_a) ) * tmask(ji,jj,1)
               END DO
            END DO
         ENDIF
         !
         DO jj=j1,j2
            DO ji=i1,i2
               ssh(ji,jj,Kmm_a) = tabres(ji,jj) * tmask(ji,jj,1)
            END DO
         END DO
         !
         IF  ((l_1st_euler).AND.(Agrif_Nb_Step()==0) ) THEN
            ssh(i1:i2,j1:j2,Kbb_a)  = ssh(i1:i2,j1:j2,Kmm_a)
         ENDIF
         !
      ENDIF
      !
   END SUBROUTINE updateSSH


   SUBROUTINE updatetmsk( tabres, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                   *** ROUTINE updatetmsk ***
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   tabres
      LOGICAL                         , INTENT(in   ) ::   before
      !!
      !!----------------------------------------------------------------------
      ! 
      IF( .NOT.before ) THEN
         tmask_upd(i1:i2,j1:j2)  = 1._wp 
      ENDIF
      !
   END SUBROUTINE updatetmsk


   SUBROUTINE updateumsk( tabres, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                   *** ROUTINE updateumsk ***
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   tabres
      LOGICAL                         , INTENT(in   ) ::   before
      !!
      !!----------------------------------------------------------------------
      ! 
      IF( .NOT.before ) THEN
         umask_upd(i1:i2,j1:j2)  = 1._wp 
      ENDIF
      !
   END SUBROUTINE updateumsk


   SUBROUTINE updatevmsk( tabres, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                   *** ROUTINE updatevmsk ***
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   tabres
      LOGICAL                         , INTENT(in   ) ::   before
      !!
      !!----------------------------------------------------------------------
      ! 
      IF( .NOT.before ) THEN
         vmask_upd(i1:i2,j1:j2)  = 1._wp 
      ENDIF
      !
   END SUBROUTINE updatevmsk


   SUBROUTINE updateub2b( tabres, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                      *** ROUTINE updateub2b ***
      !!----------------------------------------------------------------------
      INTEGER                            , INTENT(in) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   tabres
      LOGICAL                            , INTENT(in) ::   before
      !!
      INTEGER :: ji, jj
      REAL(wp) :: za1, zcor
      !!---------------------------------------------
      !
      IF (before) THEN
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj) = ub2_i_b(ji,jj) * e2u_frac(ji,jj)
            END DO
         END DO
      ELSE
         !
         za1 = 1._wp / REAL(Agrif_rhot(), wp)
         DO jj=j1,j2
            DO ji=i1,i2
               zcor=tabres(ji,jj) - ub2_b(ji,jj)
               ! Update time integrated fluxes also in case of multiply nested grids:
               ub2_i_b(ji,jj) = ub2_i_b(ji,jj) + za1 * zcor 
               ! Update corrective fluxes:
               IF (.NOT.(lk_agrif_fstep.AND.(l_1st_euler))) un_bf(ji,jj)  = un_bf(ji,jj) + zcor
               ! Update half step back fluxes:
               ub2_b(ji,jj) = tabres(ji,jj)
            END DO
         END DO
      ENDIF
      !
   END SUBROUTINE updateub2b

   SUBROUTINE reflux_sshu( tabres, i1, i2, j1, j2, before, nb, ndir )
      !!---------------------------------------------
      !!          *** ROUTINE reflux_sshu ***
      !!---------------------------------------------
      INTEGER, INTENT(in) :: i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before
      INTEGER, INTENT(in) :: nb, ndir
      !!
      LOGICAL :: western_side, eastern_side 
      INTEGER :: ji, jj
      REAL(wp) :: zcor
      !!---------------------------------------------
      !
      IF (before) THEN
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj) = ub2_i_b(ji,jj) * e2u_frac(ji,jj)
            END DO
         END DO
      ELSE
         !
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         !
         IF (western_side) THEN
            DO jj=j1,j2
               zcor = rn_Dt * r1_e1e2t(i1  ,jj) * e2u(i1,jj) * (ub2_b(i1,jj)-tabres(i1,jj)) 
               ssh(i1  ,jj,Kmm_a) = ssh(i1  ,jj,Kmm_a) + zcor
               IF (.NOT.(lk_agrif_fstep.AND.(l_1st_euler))) ssh(i1  ,jj,Kbb_a) = ssh(i1  ,jj,Kbb_a) + rn_atfp * zcor
            END DO
         ENDIF
         IF (eastern_side) THEN
            DO jj=j1,j2
               zcor = - rn_Dt * r1_e1e2t(i2+1,jj) * e2u(i2,jj) * (ub2_b(i2,jj)-tabres(i2,jj))
               ssh(i2+1,jj,Kmm_a) = ssh(i2+1,jj,Kmm_a) + zcor
               IF (.NOT.(lk_agrif_fstep.AND.(l_1st_euler))) ssh(i2+1,jj,Kbb_a) = ssh(i2+1,jj,Kbb_a) + rn_atfp * zcor
            END DO
         ENDIF
         !
      ENDIF
      !
   END SUBROUTINE reflux_sshu

   SUBROUTINE updatevb2b( tabres, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                      *** ROUTINE updatevb2b ***
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   tabres
      LOGICAL                         , INTENT(in   ) ::   before
      !!
      INTEGER :: ji, jj
      REAL(wp) :: za1, zcor
      !!---------------------------------------------
      !
      IF( before ) THEN
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj) = vb2_i_b(ji,jj) * e1v_frac(ji,jj) 
            END DO
         END DO
      ELSE
         !
         za1 = 1._wp / REAL(Agrif_rhot(), wp)
         DO jj=j1,j2
            DO ji=i1,i2
               zcor=tabres(ji,jj) - vb2_b(ji,jj)
               ! Update time integrated fluxes also in case of multiply nested grids:
               vb2_i_b(ji,jj) = vb2_i_b(ji,jj) + za1 * zcor 
               ! Update corrective fluxes:
               IF (.NOT.(lk_agrif_fstep.AND.(l_1st_euler)))  vn_bf(ji,jj)  = vn_bf(ji,jj) + zcor
               ! Update half step back fluxes:
               vb2_b(ji,jj) = tabres(ji,jj)
            END DO
         END DO
      ENDIF
      !
   END SUBROUTINE updatevb2b

   SUBROUTINE reflux_sshv( tabres, i1, i2, j1, j2, before, nb, ndir )
      !!---------------------------------------------
      !!          *** ROUTINE reflux_sshv ***
      !!---------------------------------------------
      INTEGER, INTENT(in) :: i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before
      INTEGER, INTENT(in) :: nb, ndir
      !!
      LOGICAL :: southern_side, northern_side 
      INTEGER :: ji, jj
      REAL(wp) :: zcor
      !!---------------------------------------------
      !
      IF (before) THEN
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj) = vb2_i_b(ji,jj) * e1v_frac(ji,jj) 
            END DO
         END DO
      ELSE
         !
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)
         !
         IF (southern_side) THEN
            DO ji=i1,i2
               zcor = rn_Dt * r1_e1e2t(ji,j1  ) * e1v(ji,j1  ) * (vb2_b(ji,j1)-tabres(ji,j1))
               ssh(ji,j1  ,Kmm_a) = ssh(ji,j1  ,Kmm_a) + zcor
               IF (.NOT.(lk_agrif_fstep.AND.(l_1st_euler))) ssh(ji,j1  ,Kbb_a) = ssh(ji,j1,Kbb_a) + rn_atfp * zcor
            END DO
         ENDIF
         IF (northern_side) THEN               
            DO ji=i1,i2
               zcor = - rn_Dt * r1_e1e2t(ji,j2+1) * e1v(ji,j2  ) * (vb2_b(ji,j2)-tabres(ji,j2))
               ssh(ji,j2+1,Kmm_a) = ssh(ji,j2+1,Kmm_a) + zcor
               IF (.NOT.(lk_agrif_fstep.AND.(l_1st_euler))) ssh(ji,j2+1,Kbb_a) = ssh(ji,j2+1,Kbb_a) + rn_atfp * zcor
            END DO
         ENDIF
         ! 
      ENDIF
      !
   END SUBROUTINE reflux_sshv


   SUBROUTINE updateEN( ptab, i1, i2, j1, j2, k1, k2, before )
      !!----------------------------------------------------------------------
      !!                      *** ROUTINE updateen ***
      !!----------------------------------------------------------------------
      INTEGER                               , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) ::   ptab
      LOGICAL                               , INTENT(in   ) ::   before
      !!----------------------------------------------------------------------
      !
      IF( before ) THEN
         ptab (i1:i2,j1:j2,k1:k2) = en(i1:i2,j1:j2,k1:k2)
      ELSE
         en(i1:i2,j1:j2,k1:k2) = ptab (i1:i2,j1:j2,k1:k2) 
      ENDIF
      !
   END SUBROUTINE updateEN


   SUBROUTINE updateAVT( ptab, i1, i2, j1, j2, k1, k2, before )
      !!----------------------------------------------------------------------
      !!                      *** ROUTINE updateavt ***
      !!----------------------------------------------------------------------
      INTEGER                               , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) ::   ptab
      LOGICAL                               , INTENT(in   ) ::   before
      !!----------------------------------------------------------------------
      !
      IF( before ) THEN   ;   ptab (i1:i2,j1:j2,k1:k2) = avt_k(i1:i2,j1:j2,k1:k2)
      ELSE                ;   avt_k(i1:i2,j1:j2,k1:k2) = ptab (i1:i2,j1:j2,k1:k2) 
      ENDIF
      !
   END SUBROUTINE updateAVT


   SUBROUTINE updateAVM( ptab, i1, i2, j1, j2, k1, k2, before )
      !!---------------------------------------------
      !!           *** ROUTINE updateavm ***
      !!----------------------------------------------------------------------
      INTEGER                               , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) ::   ptab
      LOGICAL                               , INTENT(in   ) ::   before
      !!----------------------------------------------------------------------
      !
      IF( before ) THEN   ;   ptab (i1:i2,j1:j2,k1:k2) = avm_k(i1:i2,j1:j2,k1:k2)
      ELSE                ;   avm_k(i1:i2,j1:j2,k1:k2) = ptab (i1:i2,j1:j2,k1:k2) 
      ENDIF
      !
   END SUBROUTINE updateAVM

#if ! defined key_qco   &&   ! defined key_linssh
   SUBROUTINE update_e3t(tabres, i1, i2, j1, j2, k1, k2, before )
      !!---------------------------------------------
      !!           *** ROUTINE update_e3t ***
      !!---------------------------------------------
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres 
      INTEGER                               , INTENT(in   ) :: i1, i2, j1, j2, k1, k2
      LOGICAL                               , INTENT(in   ) :: before
      !
      INTEGER :: ji, jj, jk
      REAL(wp) :: zcoef
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk) :: tabres_child
      !!---------------------------------------------
      !
      IF ( before ) THEN
         tabres(i1:i2,j1:j2,k2) = 0._wp
         IF ( .NOT.l_vremap ) THEN 
            DO jk = k1, k2-1 
               tabres(i1:i2,j1:j2,jk) = e1e2t_frac(i1:i2,j1:j2)          & 
                                      &      * e3t(i1:i2,j1:j2,jk,Kmm_a) & 
                                      &    * tmask(i1:i2,j1:j2,jk) 
            END DO
         ENDIF
      ELSE 
         !
         IF ( .NOT.l_vremap ) THEN ! Update e3t from parent thicknesses
            tabres_child(i1:i2,j1:j2,1:jpk) =  e3t_0(i1:i2,j1:j2,1:jpk)
            WHERE( tmask(i1:i2,j1:j2,k1:k2) /= 0._wp ) 
               tabres_child(i1:i2,j1:j2,k1:k2) = tabres(i1:i2,j1:j2,k1:k2)
            ENDWHERE
         ELSE                      ! Update e3t from ssh
            DO jk = 1, jpkm1
               tabres_child(i1:i2,j1:j2,jk) = e3t_0(i1:i2,j1:j2,jk) &  
                * (1._wp + ssh(i1:i2,j1:j2,Kmm_a)*r1_ht_0(i1:i2,j1:j2))
            END DO
         ENDIF
         !
         ! 1) Updates at BEFORE time step:
         ! -------------------------------
         !
         ! Save "old" scale factor (prior update) for subsequent asselin correction
         ! of prognostic variables
         e3t(i1:i2,j1:j2,1:jpkm1,Krhs_a) = e3t(i1:i2,j1:j2,1:jpkm1,Kmm_a)

         IF (.NOT.(lk_agrif_fstep.AND.(l_1st_euler) )) THEN
            DO jk = 1, jpkm1
               DO jj=j1,j2
                  DO ji=i1,i2
                     e3t(ji,jj,jk,Kbb_a) =  e3t(ji,jj,jk,Kbb_a) &
                           & + rn_atfp * ( tabres_child(ji,jj,jk) - e3t(ji,jj,jk,Kmm_a) )
                  END DO
               END DO
            END DO
            !
            e3w  (i1:i2,j1:j2,1,Kbb_a) = e3w_0(i1:i2,j1:j2,1) + e3t(i1:i2,j1:j2,1,Kbb_a) - e3t_0(i1:i2,j1:j2,1)
            gdepw(i1:i2,j1:j2,1,Kbb_a) = 0.0_wp
            gdept(i1:i2,j1:j2,1,Kbb_a) = 0.5_wp * e3w(i1:i2,j1:j2,1,Kbb_a)
            !
            DO jk = 2, jpkm1
               DO jj = j1,j2
                  DO ji = i1,i2            
                     zcoef = (tmask(ji,jj,jk) - wmask(ji,jj,jk))
                     e3w(ji,jj,jk,Kbb_a)  = e3w_0(ji,jj,jk) + ( 1.0_wp - 0.5_wp * tmask(ji,jj,jk) ) *        & 
                     &                                        ( e3t(ji,jj,jk-1,Kbb_a) - e3t_0(ji,jj,jk-1) )  &
                     &                                  +            0.5_wp * tmask(ji,jj,jk)   *        &
                     &                                        ( e3t(ji,jj,jk  ,Kbb_a) - e3t_0(ji,jj,jk  ) )
                     gdepw(ji,jj,jk,Kbb_a) = gdepw(ji,jj,jk-1,Kbb_a) + e3t(ji,jj,jk-1,Kbb_a)
                     gdept(ji,jj,jk,Kbb_a) =      zcoef  * ( gdepw(ji,jj,jk  ,Kbb_a) + 0.5_wp * e3w(ji,jj,jk,Kbb_a))  &
                         &               + (1-zcoef) * ( gdept(ji,jj,jk-1,Kbb_a) +       e3w(ji,jj,jk,Kbb_a)) 
                  END DO
               END DO
            END DO
            !
         ENDIF        
         !
         ! 2) Updates at NOW time step:
         ! ----------------------------
         !
         ! Update vertical scale factor at T-points:
         e3t(i1:i2,j1:j2,1:jpkm1,Kmm_a) = tabres_child(i1:i2,j1:j2,1:jpkm1)
         !
         ! Update total depth:
         ht(i1:i2,j1:j2) = 0._wp
         DO jk = 1, jpkm1
            ht(i1:i2,j1:j2) = ht(i1:i2,j1:j2) + e3t(i1:i2,j1:j2,jk,Kmm_a) * tmask(i1:i2,j1:j2,jk)
         END DO
         !
         ! Update vertical scale factor at W-points and depths:
         e3w (i1:i2,j1:j2,1,Kmm_a) = e3w_0(i1:i2,j1:j2,1) + e3t(i1:i2,j1:j2,1,Kmm_a) - e3t_0(i1:i2,j1:j2,1)
         gdept(i1:i2,j1:j2,1,Kmm_a) = 0.5_wp * e3w(i1:i2,j1:j2,1,Kmm_a)
         gdepw(i1:i2,j1:j2,1,Kmm_a) = 0.0_wp
         gde3w(i1:i2,j1:j2,1) = gdept(i1:i2,j1:j2,1,Kmm_a) - (ht(i1:i2,j1:j2)-ht_0(i1:i2,j1:j2)) ! Last term in the rhs is ssh
         !
         DO jk = 2, jpkm1
            DO jj = j1,j2
               DO ji = i1,i2            
               zcoef = (tmask(ji,jj,jk) - wmask(ji,jj,jk))
               e3w(ji,jj,jk,Kmm_a)  = e3w_0(ji,jj,jk) + ( 1.0_wp - 0.5_wp * tmask(ji,jj,jk) ) * ( e3t(ji,jj,jk-1,Kmm_a) - e3t_0(ji,jj,jk-1) )   &
               &                                  +            0.5_wp * tmask(ji,jj,jk)   * ( e3t(ji,jj,jk  ,Kmm_a) - e3t_0(ji,jj,jk  ) )
               gdepw(ji,jj,jk,Kmm_a) = gdepw(ji,jj,jk-1,Kmm_a) + e3t(ji,jj,jk-1,Kmm_a)
               gdept(ji,jj,jk,Kmm_a) =      zcoef  * ( gdepw(ji,jj,jk  ,Kmm_a) + 0.5_wp * e3w(ji,jj,jk,Kmm_a))  &
                   &               + (1-zcoef) * ( gdept(ji,jj,jk-1,Kmm_a) +       e3w(ji,jj,jk,Kmm_a)) 
               gde3w(ji,jj,jk) = gdept(ji,jj,jk,Kmm_a) - (ht(ji,jj)-ht_0(ji,jj)) ! Last term in the rhs is ssh
               END DO
            END DO
         END DO
         !
         IF  ((l_1st_euler).AND.(Agrif_Nb_Step()==0) ) THEN
            e3t (i1:i2,j1:j2,1:jpkm1,Kbb_a)  = e3t (i1:i2,j1:j2,1:jpkm1,Kmm_a)
            e3w (i1:i2,j1:j2,1:jpkm1,Kbb_a)  = e3w (i1:i2,j1:j2,1:jpkm1,Kmm_a)
            gdepw(i1:i2,j1:j2,1:jpkm1,Kbb_a) = gdepw(i1:i2,j1:j2,1:jpkm1,Kmm_a)
            gdept(i1:i2,j1:j2,1:jpkm1,Kbb_a) = gdept(i1:i2,j1:j2,1:jpkm1,Kmm_a)
         ENDIF
         !
      ENDIF
      !
   END SUBROUTINE update_e3t


   SUBROUTINE update_e3u(tabres, i1, i2, j1, j2, k1, k2, before )
      !!---------------------------------------------
      !!           *** ROUTINE update_e3u ***
      !!---------------------------------------------
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres 
      INTEGER                               , INTENT(in   ) :: i1, i2, j1, j2, k1, k2
      LOGICAL                               , INTENT(in   ) :: before
      !
      INTEGER :: ji, jj, jk
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk) :: tabres_child
      !!---------------------------------------------
      !
      IF ( before ) THEN
         tabres(i1:i2,j1:j2,k2) = 0._wp
         IF ( .NOT.l_vremap ) THEN
            DO jk = k1, k2-1
               tabres(i1:i2,j1:j2,jk) = e2u_frac(i1:i2,j1:j2) * e3u(i1:i2,j1:j2,jk,Kmm_a) * umask(i1:i2,j1:j2,jk) 
            END DO
         ELSE
            ! Retrieve sea level at U-points:
            DO jk = k1, k2-1
               tabres(i1:i2,j1:j2,k2) =   tabres(i1:i2,j1:j2,k2) + & 
                                      & e2u_frac(i1:i2,j1:j2) * e3u(i1:i2,j1:j2,jk,Kmm_a) * umask(i1:i2,j1:j2,jk) 
            END DO
            tabres(i1:i2,j1:j2,k2) = tabres(i1:i2,j1:j2,k2) - hu_0(i1:i2,j1:j2)
         ENDIF   
      ELSE 
         !
         IF ( .NOT.l_vremap ) THEN ! Update e3u from parent thicknesses
            tabres_child(i1:i2,j1:j2,1:jpk) =  e3u_0(i1:i2,j1:j2,1:jpk)
            WHERE( umask(i1:i2,j1:j2,k1:k2) /= 0._wp ) 
               tabres_child(i1:i2,j1:j2,k1:k2) = tabres(i1:i2,j1:j2,k1:k2)
            ENDWHERE
         ELSE                      ! Update e3u from ssh stored in tabres(:,:,k2)
            DO jk = 1, jpkm1
               tabres_child(i1:i2,j1:j2,jk) = e3u_0(i1:i2,j1:j2,jk) &  
                * (1._wp + tabres(i1:i2,j1:j2,k2)*r1_hu_0(i1:i2,j1:j2))
            END DO
         ENDIF
         !
         ! 1) Updates at BEFORE time step:
         ! -------------------------------
         !
         ! Save "old" scale factor (prior update) for subsequent asselin correction
         ! of prognostic variables
         e3u(i1:i2,j1:j2,1:jpkm1,Krhs_a) = e3u(i1:i2,j1:j2,1:jpkm1,Kmm_a)

         IF (.NOT.(lk_agrif_fstep.AND.(l_1st_euler) )) THEN
            DO jk = 1, jpkm1
               DO jj = j1, j2
                  DO ji = i1, i2
                     e3u(ji,jj,jk,Kbb_a) =  e3u(ji,jj,jk,Kbb_a) &
                           & + rn_atfp * ( tabres_child(ji,jj,jk) - e3u(ji,jj,jk,Kmm_a) )
                  END DO
               END DO
            END DO
            !
            ! Update total depth:
            hu(i1:i2,j1:j2,Kbb_a) = 0._wp
            DO jk = 1, jpkm1
               hu(i1:i2,j1:j2,Kbb_a) = hu(i1:i2,j1:j2,Kbb_a) + e3u(i1:i2,j1:j2,jk,Kbb_a) * umask(i1:i2,j1:j2,jk)
            END DO
            r1_hu(i1:i2,j1:j2,Kbb_a) = ssumask(i1:i2,j1:j2) / ( hu(i1:i2,j1:j2,Kbb_a) + 1._wp - ssumask(i1:i2,j1:j2) )
            !
            e3uw  (i1:i2,j1:j2,1,Kbb_a) = e3uw_0(i1:i2,j1:j2,1) + e3u(i1:i2,j1:j2,1,Kbb_a) - e3u_0(i1:i2,j1:j2,1)
            DO jk = 2, jpkm1
               DO jj = j1,j2
                  DO ji = i1,i2            
                     e3uw(ji,jj,jk,Kbb_a)  = e3uw_0(ji,jj,jk) + ( 1.0_wp - 0.5_wp * umask(ji,jj,jk) ) *        & 
                     &                                        ( e3u(ji,jj,jk-1,Kbb_a) - e3u_0(ji,jj,jk-1) )    &
                     &                                        +            0.5_wp * umask(ji,jj,jk)   *        &
                     &                                        ( e3u(ji,jj,jk  ,Kbb_a) - e3u_0(ji,jj,jk  ) )
                  END DO
               END DO
            END DO
            !
         ENDIF        
         !
         ! 2) Updates at NOW time step:
         ! ----------------------------
         !
         ! Update vertical scale factor at U-points:
         e3u(i1:i2,j1:j2,1:jpkm1,Kmm_a) = tabres_child(i1:i2,j1:j2,1:jpkm1)
         !
         ! Update total depth:
         hu(i1:i2,j1:j2,Kmm_a) = 0._wp
         DO jk = 1, jpkm1
            hu(i1:i2,j1:j2,Kmm_a) = hu(i1:i2,j1:j2,Kmm_a) + e3u(i1:i2,j1:j2,jk,Kmm_a) * umask(i1:i2,j1:j2,jk)
         END DO
         r1_hu(i1:i2,j1:j2,Kmm_a) = ssumask(i1:i2,j1:j2) / ( hu(i1:i2,j1:j2,Kmm_a) + 1._wp - ssumask(i1:i2,j1:j2) )
         !
         ! Update vertical scale factor at W-points and depths:
         e3uw (i1:i2,j1:j2,1,Kmm_a) = e3uw_0(i1:i2,j1:j2,1) + e3u(i1:i2,j1:j2,1,Kmm_a) - e3u_0(i1:i2,j1:j2,1)
         DO jk = 2, jpkm1
            DO jj = j1,j2
               DO ji = i1,i2            
                  e3uw(ji,jj,jk,Kmm_a)  = e3uw_0(ji,jj,jk) + ( 1.0_wp - 0.5_wp * umask(ji,jj,jk) ) *       &  
                  &                                        ( e3u(ji,jj,jk-1,Kmm_a) - e3u_0(ji,jj,jk-1) )   &
                  &                                        +            0.5_wp * umask(ji,jj,jk)   *       &
                  &                                        ( e3u(ji,jj,jk  ,Kmm_a) - e3u_0(ji,jj,jk  ) )
               END DO
            END DO
         END DO
         !
         IF  ((l_1st_euler).AND.(Agrif_Nb_Step()==0) ) THEN
            e3u (i1:i2,j1:j2,1:jpkm1,Kbb_a)  = e3u (i1:i2,j1:j2,1:jpkm1,Kmm_a)
            e3uw(i1:i2,j1:j2,1:jpkm1,Kbb_a)  = e3uw(i1:i2,j1:j2,1:jpkm1,Kmm_a)
            hu   (i1:i2,j1:j2,Kbb_a)         = hu   (i1:i2,j1:j2,Kmm_a)
            r1_hu(i1:i2,j1:j2,Kbb_a)         = r1_hu(i1:i2,j1:j2,Kmm_a)
         ENDIF
         !
      ENDIF
      !
   END SUBROUTINE update_e3u


   SUBROUTINE update_e3v(tabres, i1, i2, j1, j2, k1, k2, before )
      !!---------------------------------------------
      !!           *** ROUTINE update_e3v ***
      !!---------------------------------------------
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres 
      INTEGER                               , INTENT(in   ) :: i1, i2, j1, j2, k1, k2
      LOGICAL                               , INTENT(in   ) :: before
      !
      INTEGER :: ji, jj, jk
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk) :: tabres_child
      !!---------------------------------------------
      !
      IF ( before ) THEN
         tabres(i1:i2,j1:j2,k2) = 0._wp
         IF ( .NOT.l_vremap ) THEN
            DO jk = k1, k2-1
               tabres(i1:i2,j1:j2,jk) = e1v_frac(i1:i2,j1:j2) * e3v(i1:i2,j1:j2,jk,Kmm_a) * vmask(i1:i2,j1:j2,jk) 
            END DO
         ELSE
            ! Retrieve sea level at V-points:
            DO jk = k1, k2-1
               tabres(i1:i2,j1:j2,k2) =   tabres(i1:i2,j1:j2,k2) + & 
                                      & e1v_frac(i1:i2,j1:j2) * e3v(i1:i2,j1:j2,jk,Kmm_a) * vmask(i1:i2,j1:j2,jk) 
            END DO
            tabres(i1:i2,j1:j2,k2) = tabres(i1:i2,j1:j2,k2) - hv_0(i1:i2,j1:j2)
         ENDIF    
      ELSE 
         !
         IF ( .NOT.l_vremap ) THEN ! Update e3v from parent thicknesses
            tabres_child(i1:i2,j1:j2,1:jpk) =  e3v_0(i1:i2,j1:j2,1:jpk)
            WHERE( vmask(i1:i2,j1:j2,k1:k2) /= 0._wp ) 
               tabres_child(i1:i2,j1:j2,k1:k2) = tabres(i1:i2,j1:j2,k1:k2)
            ENDWHERE
         ELSE                      ! Update e3v from ssh stored in tabres(:,:,k2)
            DO jk = 1, jpkm1
               tabres_child(i1:i2,j1:j2,jk) = e3v_0(i1:i2,j1:j2,jk) &  
                * (1._wp + tabres(i1:i2,j1:j2,k2)*r1_hv_0(i1:i2,j1:j2))
            END DO
         ENDIF
         !
         ! 1) Updates at BEFORE time step:
         ! -------------------------------
         !
         ! Save "old" scale factor (prior update) for subsequent asselin correction
         ! of prognostic variables
         e3v(i1:i2,j1:j2,k1:k2,Krhs_a) = e3v(i1:i2,j1:j2,k1:k2,Kmm_a)

         IF (.NOT.(lk_agrif_fstep.AND.(l_1st_euler) )) THEN
            DO jk = 1, jpkm1
               DO jj = j1, j2
                  DO ji = i1, i2
                     e3v(ji,jj,jk,Kbb_a) =  e3v(ji,jj,jk,Kbb_a) &
                           & + rn_atfp * ( tabres_child(ji,jj,jk) - e3v(ji,jj,jk,Kmm_a) )
                  END DO
               END DO
            END DO
            !
            ! Update total depth:
            hv(i1:i2,j1:j2,Kbb_a) = 0._wp
            DO jk = 1, jpkm1
               hv(i1:i2,j1:j2,Kbb_a) = hv(i1:i2,j1:j2,Kbb_a) + e3v(i1:i2,j1:j2,jk,Kbb_a) * vmask(i1:i2,j1:j2,jk)
            END DO
            r1_hv(i1:i2,j1:j2,Kbb_a) = ssvmask(i1:i2,j1:j2) / ( hv(i1:i2,j1:j2,Kbb_a) + 1._wp - ssvmask(i1:i2,j1:j2) )
            !
            e3vw(i1:i2,j1:j2,1,Kbb_a) = e3vw_0(i1:i2,j1:j2,1) + e3v(i1:i2,j1:j2,1,Kbb_a) - e3v_0(i1:i2,j1:j2,1)
            DO jk = 2, jpkm1
               DO jj = j1,j2
                  DO ji = i1,i2            
                     e3vw(ji,jj,jk,Kbb_a)  = e3vw_0(ji,jj,jk) + ( 1.0_wp - 0.5_wp * vmask(ji,jj,jk) ) *        & 
                     &                                        ( e3v(ji,jj,jk-1,Kbb_a) - e3v_0(ji,jj,jk-1) )    &
                     &                                        +            0.5_wp * vmask(ji,jj,jk)   *        &
                     &                                        ( e3v(ji,jj,jk  ,Kbb_a) - e3v_0(ji,jj,jk  ) )
                  END DO
               END DO
            END DO
            !
         ENDIF        
         !
         ! 2) Updates at NOW time step:
         ! ----------------------------
         !
         ! Update vertical scale factor at U-points:
         e3v(i1:i2,j1:j2,1:jpkm1,Kmm_a) = tabres_child(i1:i2,j1:j2,1:jpkm1)
         !
         ! Update total depth:
         hv(i1:i2,j1:j2,Kmm_a) = 0._wp
         DO jk = 1, jpkm1
            hv(i1:i2,j1:j2,Kmm_a) = hv(i1:i2,j1:j2,Kmm_a) + e3v(i1:i2,j1:j2,jk,Kmm_a) * vmask(i1:i2,j1:j2,jk)
         END DO
         r1_hv(i1:i2,j1:j2,Kmm_a) = ssvmask(i1:i2,j1:j2) / ( hv(i1:i2,j1:j2,Kmm_a) + 1._wp - ssvmask(i1:i2,j1:j2) )
         !
         ! Update vertical scale factor at W-points and depths:
         e3vw (i1:i2,j1:j2,1,Kmm_a) = e3vw_0(i1:i2,j1:j2,1) + e3v(i1:i2,j1:j2,1,Kmm_a) - e3v_0(i1:i2,j1:j2,1)
         DO jk = 2, jpkm1
            DO jj = j1, j2
               DO ji = i1, i2            
                  e3vw(ji,jj,jk,Kmm_a)  = e3vw_0(ji,jj,jk) + ( 1.0_wp - 0.5_wp * vmask(ji,jj,jk) ) *       &  
                  &                                        ( e3v(ji,jj,jk-1,Kmm_a) - e3v_0(ji,jj,jk-1) )   &
                  &                                        +            0.5_wp * vmask(ji,jj,jk)   *       &
                  &                                        ( e3v(ji,jj,jk  ,Kmm_a) - e3v_0(ji,jj,jk  ) )
               END DO
            END DO
         END DO
         !
         IF  ((l_1st_euler).AND.(Agrif_Nb_Step()==0) ) THEN
            e3v  (i1:i2,j1:j2,1:jpkm1,Kbb_a)  = e3v  (i1:i2,j1:j2,1:jpkm1,Kmm_a)
            e3vw (i1:i2,j1:j2,1:jpkm1,Kbb_a)  = e3vw (i1:i2,j1:j2,1:jpkm1,Kmm_a)
            hv   (i1:i2,j1:j2,Kbb_a)          = hv   (i1:i2,j1:j2,Kmm_a)
            r1_hv(i1:i2,j1:j2,Kbb_a)          = r1_hv(i1:i2,j1:j2,Kmm_a)
         ENDIF
         !
      ENDIF
      !
   END SUBROUTINE update_e3v


   SUBROUTINE update_e3f(tabres, i1, i2, j1, j2, k1, k2, before )
      !!---------------------------------------------
      !!           *** ROUTINE update_e3f ***
      !!---------------------------------------------
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres 
      INTEGER                               , INTENT(in   ) :: i1, i2, j1, j2, k1, k2
      LOGICAL                               , INTENT(in   ) :: before
      !
      INTEGER :: ji, jj, jk
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk) :: tabres_child
      !!---------------------------------------------
      !
      IF ( before ) THEN
         tabres(i1:i2,j1:j2,k2) = 0._wp
         IF ( .NOT.l_vremap ) THEN
            DO jk = k1, k2-1
               tabres(i1:i2,j1:j2,jk) = e3f(i1:i2,j1:j2,jk) * fe3mask(i1:i2,j1:j2,jk) 
            END DO
         ELSE
            ! Retrieve sea level at F-points:
            DO jk = k1, k2-1
               tabres(i1:i2,j1:j2,k2) = tabres(i1:i2,j1:j2,k2) + & 
                                      &    e3f(i1:i2,j1:j2,jk) * fe3mask(i1:i2,j1:j2,jk) 
            END DO
            tabres(i1:i2,j1:j2,k2) = tabres(i1:i2,j1:j2,k2) - hf_0(i1:i2,j1:j2)
         ENDIF 
      ELSE 
         !
         IF ( .NOT.l_vremap ) THEN ! Update e3f from parent thicknesses
            tabres_child(i1:i2,j1:j2,1:jpkm1) =  e3f_0(i1:i2,j1:j2,1:jpkm1)
            WHERE( fe3mask(i1:i2,j1:j2,k1:k2) /= 0._wp ) 
               tabres_child(i1:i2,j1:j2,k1:k2) = tabres(i1:i2,j1:j2,k1:k2)
            ENDWHERE
         ELSE                      ! Update e3f from ssh stored in tabres(:,:,k2)
            DO jk = 1, jpkm1
               tabres_child(i1:i2,j1:j2,jk) = e3f_0(i1:i2,j1:j2,jk) &  
                * (1._wp + tabres(i1:i2,j1:j2,k2)*r1_hf_0(i1:i2,j1:j2))
            END DO
         ENDIF
         !
         ! Update vertical scale factor at F-points:
         e3f(i1:i2,j1:j2,1:jpkm1) = tabres_child(i1:i2,j1:j2,1:jpkm1)
         !
      ENDIF
      !
   END SUBROUTINE update_e3f
#endif

#if defined key_qco
   SUBROUTINE update_r3t(tabres, i1, i2, j1, j2, before )
      !!---------------------------------------------
      !!           *** ROUTINE update_r3t ***
      !!---------------------------------------------
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres 
      INTEGER                         , INTENT(in   ) :: i1, i2, j1, j2
      LOGICAL                         , INTENT(in   ) :: before
      !
      !!---------------------------------------------
      IF ( before ) THEN 
         tabres(i1:i2,j1:j2) = e1e2t_frac(i1:i2,j1:j2)       & 
                             &    *   r3t(i1:i2,j1:j2,Kmm_a) & 
                             &    *  ht_0(i1:i2,j1:j2)       &
                             &    * tmask(i1:i2,j1:j2,1) 
      ELSE 
         !
         tabres(i1:i2,j1:j2) = tabres(i1:i2,j1:j2) * r1_ht_0(i1:i2,j1:j2)
         !
         ! 1) Update at BEFORE time step:
         ! ------------------------------
         ! Save "old" array (prior update) for subsequent asselin correction
         ! of prognostic variables
         r3t(i1:i2,j1:j2,Krhs_a) = r3t(i1:i2,j1:j2,Kmm_a)

         IF (.NOT.(lk_agrif_fstep.AND.(l_1st_euler) )) THEN
            r3t(i1:i2,j1:j2,Kbb_a) =  r3t(i1:i2,j1:j2,Kbb_a) &
                   & + rn_atfp * ( tabres(i1:i2,j1:j2) - r3t(i1:i2,j1:j2,Kmm_a) )
         ENDIF   
         !
         ! 2) Updates at NOW time step:
         ! ----------------------------
         r3t(i1:i2,j1:j2,Kmm_a) = tabres(i1:i2,j1:j2)
         !
         ! 3) Special case for euler startup only:
         ! ---------------------------------------
         IF  ( (l_1st_euler).AND.(Agrif_Nb_Step()==0) ) THEN
            r3t(i1:i2,j1:j2,Kbb_a)  = r3t(i1:i2,j1:j2,Kmm_a)
         ENDIF
         !
      ENDIF
   END SUBROUTINE update_r3t


   SUBROUTINE update_r3u(tabres, i1, i2, j1, j2, before )
      !!---------------------------------------------
      !!           *** ROUTINE update_r3u ***
      !!---------------------------------------------
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres 
      INTEGER                         , INTENT(in   ) :: i1, i2, j1, j2
      LOGICAL                         , INTENT(in   ) :: before
      !
      !!---------------------------------------------
      IF ( before ) THEN 
         tabres(i1:i2,j1:j2) = e2u_frac(i1:i2,j1:j2)       & 
                             &  *   r3u(i1:i2,j1:j2,Kmm_a) & 
                             &  *  hu_0(i1:i2,j1:j2)       &
                             &  * umask(i1:i2,j1:j2,1) 
      ELSE 
         !
         tabres(i1:i2,j1:j2) = tabres(i1:i2,j1:j2) * r1_hu_0(i1:i2,j1:j2)
         !
         ! 1) Update at BEFORE time step:
         ! ------------------------------
         ! Save "old" array (prior update) for subsequent asselin correction
         ! of prognostic variables
         r3u(i1:i2,j1:j2,Krhs_a) = r3u(i1:i2,j1:j2,Kmm_a)

         IF (.NOT.(lk_agrif_fstep.AND.(l_1st_euler) )) THEN
            r3u(i1:i2,j1:j2,Kbb_a) =  r3u(i1:i2,j1:j2,Kbb_a) &
                   & + rn_atfp * ( tabres(i1:i2,j1:j2) - r3u(i1:i2,j1:j2,Kmm_a) )
         ENDIF   
         !
         ! 2) Updates at NOW time step:
         ! ----------------------------
         r3u(i1:i2,j1:j2,Kmm_a) = tabres(i1:i2,j1:j2)
         !
         ! 3) Special case for euler startup only:
         ! ---------------------------------------
         IF  ( (l_1st_euler).AND.(Agrif_Nb_Step()==0) ) THEN
            r3u(i1:i2,j1:j2,Kbb_a)  = r3u(i1:i2,j1:j2,Kmm_a)
         ENDIF
         !
      ENDIF
   END SUBROUTINE update_r3u


   SUBROUTINE update_r3v(tabres, i1, i2, j1, j2, before )
      !!---------------------------------------------
      !!           *** ROUTINE update_r3v ***
      !!---------------------------------------------
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres 
      INTEGER                         , INTENT(in   ) :: i1, i2, j1, j2
      LOGICAL                         , INTENT(in   ) :: before
      !
      !!---------------------------------------------
      IF ( before ) THEN 
         tabres(i1:i2,j1:j2) = e1v_frac(i1:i2,j1:j2)       & 
                             &  *   r3v(i1:i2,j1:j2,Kmm_a) & 
                             &  *  hv_0(i1:i2,j1:j2)       &
                             &  * vmask(i1:i2,j1:j2,1) 
      ELSE 
         !
         tabres(i1:i2,j1:j2) = tabres(i1:i2,j1:j2) * r1_hv_0(i1:i2,j1:j2)
         !
         ! 1) Update at BEFORE time step:
         ! ------------------------------
         ! Save "old" array (prior update) for subsequent asselin correction
         ! of prognostic variables
         r3v(i1:i2,j1:j2,Krhs_a) = r3v(i1:i2,j1:j2,Kmm_a)

         IF (.NOT.(lk_agrif_fstep.AND.(l_1st_euler) )) THEN
            r3v(i1:i2,j1:j2,Kbb_a) =  r3v(i1:i2,j1:j2,Kbb_a) &
                   & + rn_atfp * ( tabres(i1:i2,j1:j2) - r3v(i1:i2,j1:j2,Kmm_a) )
         ENDIF   
         !
         ! 2) Updates at NOW time step:
         ! ----------------------------
         r3v(i1:i2,j1:j2,Kmm_a) = tabres(i1:i2,j1:j2)
         !
         ! 3) Special case for euler startup only:
         ! ---------------------------------------
         IF  ( (l_1st_euler).AND.(Agrif_Nb_Step()==0) ) THEN
            r3v(i1:i2,j1:j2,Kbb_a)  = r3v(i1:i2,j1:j2,Kmm_a)
         ENDIF
         !
      ENDIF
   END SUBROUTINE update_r3v


   SUBROUTINE update_r3f(tabres, i1, i2, j1, j2, before )
      !!---------------------------------------------
      !!           *** ROUTINE update_r3f ***
      !!---------------------------------------------
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres 
      INTEGER                         , INTENT(in   ) :: i1, i2, j1, j2
      LOGICAL                         , INTENT(in   ) :: before
      !
      !!---------------------------------------------
      IF ( before ) THEN 
         tabres(i1:i2,j1:j2) =       r3f(i1:i2,j1:j2)   & 
                             & *    hf_0(i1:i2,j1:j2)   &
                             & * fe3mask(i1:i2,j1:j2,1) 
      ELSE 
         !
         r3f(i1:i2,j1:j2) = tabres(i1:i2,j1:j2) * r1_hf_0(i1:i2,j1:j2)
         !
      ENDIF
   END SUBROUTINE update_r3f

#endif 

   SUBROUTINE Agrif_Check_parent_bat( )
      !!----------------------------------------------------------------------
      !!                   *** ROUTINE Agrif_Check_parent_bat ***
      !!----------------------------------------------------------------------
      ! 
      IF (( .NOT.ln_agrif_2way ).OR.(.NOT.ln_chk_bathy) & 
      & .OR.(Agrif_Root())) RETURN
      !
      Agrif_UseSpecialValueInUpdate = .FALSE.
      l_vremap                      = ln_vert_remap
      !
      IF(lwp) WRITE(numout,*) ' '
      IF(lwp) WRITE(numout,*) 'AGRIF: Check parent volume at Level:', Agrif_Level()
      !
# if ! defined DECAL_FEEDBACK
      CALL Agrif_Update_Variable(e3t_id,procname = check_parent_e3t0)
      CALL Agrif_Update_Variable(e3u_id,procname = check_parent_e3u0)
      CALL Agrif_Update_Variable(e3v_id,procname = check_parent_e3v0)
# else
      CALL Agrif_Update_Variable(e3t0_interp_id,locupdate=(/1,0/),procname = check_parent_e3t0)
# endif
      !
      l_vremap                      = .FALSE. 
      kindic_agr = Agrif_Parent(kindic_agr)
      CALL mpp_sum( 'Agrif_Check_parent_bat', kindic_agr )

      IF( kindic_agr /= 0 ) THEN
         CALL ctl_stop('==> Averaged Bathymetry does not match parent volume') 
      ELSE
         IF(lwp) WRITE(numout,*) '==> Averaged Bathymetry matches parent ' 
         IF(lwp) WRITE(numout,*) ''
      ENDIF
      !
   END SUBROUTINE Agrif_Check_parent_bat

  
   SUBROUTINE check_parent_e3t0(ptab, i1, i2, j1, j2, k1, k2, before )
      !!---------------------------------------------
      !!     *** ROUTINE check_parent__e3t0 ***
      !!---------------------------------------------
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2) :: ptab
      INTEGER, INTENT(in) :: i1, i2, j1, j2, k1, k2
      LOGICAL, INTENT(in) :: before
      INTEGER :: ji, jj, jk
      REAL(wp), DIMENSION(i1:i2,j1:j2) ::   zh0   ! 2D workspace
      !
      !!---------------------------------------------
      !
      IF( before ) THEN
         DO jk=k1,k2-1
            ptab(i1:i2,j1:j2,jk) =  e3t_0(i1:i2,j1:j2,jk) * tmask(i1:i2,j1:j2,jk) &
                          &  * e1e2t_frac(i1:i2,j1:j2)
         END DO
      ELSE
         kindic_agr = 0
         !
         DO jj=j1,j2
            DO ji=i1,i2
               IF ( ssmask(ji,jj).NE.0._wp ) THEN
                  IF ( l_vremap ) THEN ! Check total depths:
                     zh0(ji,jj) = 0._wp
                     DO jk=k1,k2-1
                        zh0(ji,jj) = zh0(ji,jj) + ptab(ji,jj,jk)   
                     END DO  
                     IF (ABS(zh0(ji,jj)-ht_0(ji,jj)).GE.1.e-6) THEN 
                        kindic_agr = kindic_agr + 1 
                     ENDIF
                  ELSE                 ! Check individual cells volumes:
                     DO jk=k1,k2-1
                        IF  (ABS((ptab(ji,jj,jk)-e3t_0(ji,jj,jk))*tmask(ji,jj,jk)).GE.1.e-6)  THEN 
                           kindic_agr = kindic_agr + 1 
                        ENDIF
                     END DO
                  ENDIF
               ENDIF
            END DO
         END DO
         !
      ENDIF
      !
   END SUBROUTINE check_parent_e3t0


   SUBROUTINE check_parent_e3u0(ptab, i1, i2, j1, j2, k1, k2, before )
      !!---------------------------------------------
      !!     *** ROUTINE check_parent_e3u0 ***
      !!---------------------------------------------
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2) :: ptab
      INTEGER, INTENT(in) :: i1, i2, j1, j2, k1, k2
      LOGICAL, INTENT(in) :: before
      INTEGER :: ji, jj, jk, ikbot
      !
      !!---------------------------------------------
      !
      IF( before ) THEN
         DO jk=k1,k2-1
            ptab(i1:i2,j1:j2,jk) =  e3u_0(i1:i2,j1:j2,jk) * umask(i1:i2,j1:j2,jk) &
                          &  * e2u_frac(i1:i2,j1:j2)
         END DO
      ELSE
         kindic_agr = 0
         !
         DO jj=j1,j2
            DO ji=i1,i2
               IF ( ssumask(ji,jj).NE.0._wp ) THEN
                  IF ( l_vremap ) THEN ! Assume depths can differ: do not check
                  ELSE                 ! Check individual cells area:
                     DO jk=k1,k2-1
                       IF (ptab(ji,jj,jk)>1.e-6) ikbot = jk
                     ENDDO
                     DO jk=k1,k2-1
                        IF  (ABS((ptab(ji,jj,jk)-e3u_0(ji,jj,jk))*umask(ji,jj,jk)).GE.1.e-6)  THEN 
                           kindic_agr = kindic_agr + 1 
                           print *, 'erro u-pt', mig0(ji), mjg0(jj), jk, mbku(ji,jj), ikbot, ptab(ji,jj,jk), e3u_0(ji,jj,jk)
                        ENDIF
                     END DO
                  ENDIF
               ENDIF
            END DO
         END DO
         !
      ENDIF
      !
   END SUBROUTINE check_parent_e3u0


   SUBROUTINE check_parent_e3v0(ptab, i1, i2, j1, j2, k1, k2, before )
      !!---------------------------------------------
      !!     *** ROUTINE check_parent_e3v0 ***
      !!---------------------------------------------
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2) :: ptab
      INTEGER, INTENT(in) :: i1, i2, j1, j2, k1, k2
      LOGICAL, INTENT(in) :: before
      INTEGER :: ji, jj, jk
      !
      !!---------------------------------------------
      !
      IF( before ) THEN
         DO jk=k1,k2-1
            ptab(i1:i2,j1:j2,jk) =  e3v_0(i1:i2,j1:j2,jk) * vmask(i1:i2,j1:j2,jk) &
                          &  * e1v_frac(i1:i2,j1:j2)
         END DO
      ELSE
         kindic_agr = 0
         !
         DO jj=j1,j2
            DO ji=i1,i2
               IF ( ssvmask(ji,jj).NE.0._wp ) THEN
                  IF ( l_vremap ) THEN ! Assume depths can differ: do not check
                  ELSE                 ! Check individual cells volumes:
                     DO jk=k1,k2-1
                        IF  (ABS((ptab(ji,jj,jk)-e3v_0(ji,jj,jk))*vmask(ji,jj,jk)).GE.1.e-6)  THEN 
                           kindic_agr = kindic_agr + 1 
                           print *, 'erro v-pt', mig0(ji), mjg0(jj), mbkv(ji,jj), ptab(ji,jj,jk), e3v_0(ji,jj,jk)
                        ENDIF
                     END DO
                  ENDIF
               ENDIF
            END DO
         END DO
         !
      ENDIF
      !
   END SUBROUTINE check_parent_e3v0
#else
   !!----------------------------------------------------------------------
   !!   Empty module                                          no AGRIF zoom
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE agrif_oce_update_empty
      WRITE(*,*)  'agrif_oce_update : You should not have seen this print! error?'
   END SUBROUTINE agrif_oce_update_empty
#endif

   !!======================================================================
END MODULE agrif_oce_update

