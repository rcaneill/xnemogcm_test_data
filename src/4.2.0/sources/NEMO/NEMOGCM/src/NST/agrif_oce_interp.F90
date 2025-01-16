#define PARENT_EXT_BDY
MODULE agrif_oce_interp
   !!======================================================================
   !!                   ***  MODULE  agrif_oce_interp  ***
   !! AGRIF: interpolation package for the ocean dynamics (OCE)
   !!======================================================================
   !! History :  2.0  !  2002-06  (L. Debreu)  Original cade
   !!            3.2  !  2009-04  (R. Benshila) 
   !!            3.6  !  2014-09  (R. Benshila) 
   !!----------------------------------------------------------------------
#if defined key_agrif
   !!----------------------------------------------------------------------
   !!   'key_agrif'                                              AGRIF zoom
   !!----------------------------------------------------------------------
   !!   Agrif_tra     :
   !!   Agrif_dyn     : 
   !!   Agrif_ssh     :
   !!   Agrif_dyn_ts  :
   !!   Agrif_dta_ts  :
   !!   Agrif_ssh_ts  :
   !!   Agrif_avm     : 
   !!   interpu       :
   !!   interpv       :
   !!----------------------------------------------------------------------
   USE par_oce
   USE oce
   USE dom_oce      
   USE zdf_oce
   USE agrif_oce
   USE phycst
!!!   USE dynspg_ts, ONLY: un_adv, vn_adv
   !
   USE in_out_manager
   USE agrif_oce_sponge
   USE lib_mpp
   USE vremap
   USE lbclnk
 
   IMPLICIT NONE
   PRIVATE

   PUBLIC   Agrif_dyn, Agrif_ssh, Agrif_dyn_ts, Agrif_dyn_ts_flux, Agrif_ssh_ts, Agrif_dta_ts
   PUBLIC   Agrif_tra, Agrif_avm
   PUBLIC   interpun , interpvn
   PUBLIC   interptsn, interpsshn, interpavm
   PUBLIC   interpunb, interpvnb , interpub2b, interpvb2b
   PUBLIC   interpglamt, interpgphit
   PUBLIC   interpht0, interpmbkt, interpe3t0_vremap
   PUBLIC   interp_e1e2t_frac, interp_e2u_frac, interp_e1v_frac
   PUBLIC   agrif_istate_oce, agrif_istate_ssh   ! called by icestate.F90 and domvvl.F90
   PUBLIC   agrif_check_bat

   INTEGER ::   bdy_tinterp = 0

   !! * Substitutions
#  include "domzgr_substitute.h90"
   !! NEMO/NST 4.0 , NEMO Consortium (2018)
   !! $Id: agrif_oce_interp.F90 15119 2021-07-13 14:43:22Z jchanut $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE Agrif_istate_oce( Kbb, Kmm, Kaa )
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE agrif_istate_oce ***
      !!
      !!                 set initial t, s, u, v, ssh from parent
      !!----------------------------------------------------------------------
      !
      IMPLICIT NONE
      !
      INTEGER, INTENT(in)  :: Kbb, Kmm, Kaa
      INTEGER :: jn
      !!----------------------------------------------------------------------
      IF(lwp) WRITE(numout,*) ' '
      IF(lwp) WRITE(numout,*) 'Agrif_istate_oce : interp child initial state from parent'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~~~'
      IF(lwp) WRITE(numout,*) ' '

      IF ( .NOT.Agrif_Parent(l_1st_euler) ) & 
         & CALL ctl_stop('AGRIF hot start requires to force Euler first step on parent')

      l_ini_child           = .TRUE.
      Agrif_SpecialValue    = 0.0_wp
      Agrif_UseSpecialValue = .TRUE.
      l_vremap              = ln_vert_remap

      ts(:,:,:,:,Kbb) = 0.0_wp
      uu(:,:,:,Kbb)   = 0.0_wp
      vv(:,:,:,Kbb)   = 0.0_wp 
       
      Krhs_a = Kbb   ;   Kmm_a = Kbb

      CALL Agrif_Init_Variable(tsini_id, procname=interptsn)

      Agrif_UseSpecialValue = ln_spc_dyn
      use_sign_north = .TRUE.
      sign_north = -1._wp
      CALL Agrif_Init_Variable(uini_id , procname=interpun )
      CALL Agrif_Init_Variable(vini_id , procname=interpvn )
      use_sign_north = .FALSE.

      Agrif_UseSpecialValue = .FALSE.
      l_ini_child           = .FALSE.
      l_vremap              = .FALSE. 

      Krhs_a = Kaa   ;   Kmm_a = Kmm

      DO jn = 1, jpts
         ts(:,:,:,jn,Kbb) = ts(:,:,:,jn,Kbb) * tmask(:,:,:)
      END DO
      uu(:,:,:,Kbb) = uu(:,:,:,Kbb) * umask(:,:,:)     
      vv(:,:,:,Kbb) = vv(:,:,:,Kbb) * vmask(:,:,:) 

      CALL lbc_lnk( 'agrif_istate_oce', uu(:,:,:  ,Kbb), 'U', -1.0_wp , vv(:,:,:,Kbb), 'V', -1.0_wp )
      CALL lbc_lnk( 'agrif_istate_oce', ts(:,:,:,:,Kbb), 'T',  1.0_wp )

   END SUBROUTINE Agrif_istate_oce


   SUBROUTINE Agrif_istate_ssh( Kbb, Kmm, Kaa, ghosts_only )
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE agrif_istate_ssh ***
      !!
      !!                    set initial ssh from parent
      !!----------------------------------------------------------------------
      !
      IMPLICIT NONE
      !
      INTEGER, INTENT(in)  :: Kbb, Kmm, Kaa 
      LOGICAL, INTENT(in), OPTIONAL :: ghosts_only
      LOGICAL :: l_do_all
      !!----------------------------------------------------------------------
      IF(lwp) WRITE(numout,*) ' '
      IF(lwp) WRITE(numout,*) 'Agrif_istate_ssh : interp child ssh from parent'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~~~'
      IF(lwp) WRITE(numout,*) ' '

      IF ( .NOT.Agrif_Parent(l_1st_euler) ) & 
         & CALL ctl_stop('AGRIF hot start requires to force Euler first step on parent')

      l_do_all = .TRUE.
      IF (present(ghosts_only)) l_do_all = .FALSE.

      Krhs_a = Kbb   ;   Kmm_a = Kbb
      !
      Agrif_SpecialValue    = 0._wp
      Agrif_UseSpecialValue = .TRUE.
      l_ini_child           = .TRUE.
      !
      IF (l_do_all) THEN
         CALL Agrif_Init_Variable(sshini_id, procname=interpsshn)
      ELSE
         CALL Agrif_Bc_Variable(sshini_id, calledweight=1._wp, procname=interpsshn)
      ENDIF
      !
      Agrif_UseSpecialValue = .FALSE.
      l_ini_child           = .FALSE.
      !
      Krhs_a = Kaa   ;   Kmm_a = Kmm
      !
      CALL lbc_lnk( 'Agrif_istate_ssh', ssh(:,:,Kbb), 'T', 1._wp )
      !
      ssh(:,:,Kmm) = ssh(:,:,Kbb)
      ssh(:,:,Kaa) = 0._wp

   END SUBROUTINE Agrif_istate_ssh


   SUBROUTINE Agrif_tra
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE Agrif_tra  ***
      !!----------------------------------------------------------------------
      !
      IF( Agrif_Root() )   RETURN
      !
      Agrif_SpecialValue    = 0._wp
      Agrif_UseSpecialValue = l_spc_tra
      l_vremap 		    = ln_vert_remap
      !
      CALL Agrif_Bc_variable( ts_interp_id, procname=interptsn )
      !
      Agrif_UseSpecialValue = .FALSE.
      l_vremap              = .FALSE.
      !
   END SUBROUTINE Agrif_tra


   SUBROUTINE Agrif_dyn( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE Agrif_DYN  ***
      !!----------------------------------------------------------------------  
      INTEGER, INTENT(in) ::   kt
      !
      INTEGER  ::   ji, jj, jk       ! dummy loop indices
      INTEGER  ::   ibdy1, jbdy1, ibdy2, jbdy2
      REAL(wp) ::   zflag  
      REAL(wp), DIMENSION(jpi,jpj) ::   zub, zvb
      REAL(wp), DIMENSION(jpi,jpj) ::   zhub, zhvb
      !!----------------------------------------------------------------------  
      !
      IF( Agrif_Root() )   RETURN
      !
      Agrif_SpecialValue    = 0.0_wp
      Agrif_UseSpecialValue = ln_spc_dyn
      l_vremap              = ln_vert_remap
      !
      use_sign_north = .TRUE.
      sign_north = -1.0_wp
      CALL Agrif_Bc_variable( un_interp_id, procname=interpun )
      CALL Agrif_Bc_variable( vn_interp_id, procname=interpvn )

      IF( .NOT.ln_dynspg_ts ) THEN ! Get transports
         ubdy(:,:) = 0._wp    ;  vbdy(:,:) = 0._wp
         utint_stage(:,:) = 0 ;  vtint_stage(:,:) = 0
         CALL Agrif_Bc_variable( unb_interp_id, procname=interpunb )
         CALL Agrif_Bc_variable( vnb_interp_id, procname=interpvnb )
      ENDIF

      use_sign_north = .FALSE.
      !
      Agrif_UseSpecialValue = .FALSE.
      l_vremap              = .FALSE.
      !
      ! Ensure below that vertically integrated transports match
      ! either transports out of time splitting procedure (ln_dynspg_ts=.TRUE.)
      ! or parent grid transports (ln_dynspg_ts=.FALSE.)
      !
      ! --- West --- !
      IF( lk_west ) THEN
         ibdy1 = nn_hls + 2                  ! halo + land + 1
         ibdy2 = nn_hls + nbghostcells + nn_shift_bar*Agrif_Rhox()   ! halo + land + nbghostcells
         !
         IF( .NOT.ln_dynspg_ts ) THEN  ! Store transport
            DO ji = mi0(ibdy1), mi1(ibdy2)
               DO jj = 1, jpj
                  uu_b(ji,jj,Krhs_a) = ubdy(ji,jj) * r1_hu(ji,jj,Krhs_a)
                  vv_b(ji,jj,Krhs_a) = vbdy(ji,jj) * r1_hv(ji,jj,Krhs_a)
               END DO
            END DO
         ENDIF
         !
         DO ji = mi0(ibdy1), mi1(ibdy2)
            zub(ji,:)  = 0._wp  
            zhub(ji,:) = 0._wp
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  zflag = 0.5_wp - SIGN(0.5_wp, rn_hcri - e3u(ji,jj,jk,Krhs_a))
                  zub(ji,jj)  =  zub(ji,jj) + zflag * e3u(ji,jj,jk,Krhs_a)  * uu(ji,jj,jk,Krhs_a) * umask(ji,jj,jk)
                  zhub(ji,jj) = zhub(ji,jj) + zflag * e3u(ji,jj,jk,Krhs_a)  * umask(ji,jj,jk)
               END DO
            END DO
            DO jj=1,jpj
!!               zub(ji,jj) = zub(ji,jj) * r1_hu(ji,jj,Krhs_a)
               zub(ji,jj) = zub(ji,jj) / ( zhub(ji,jj) + 1._wp - ssumask(ji,jj))
            END DO 
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  zflag = 0.5_wp - SIGN(0.5_wp, rn_hcri - e3u(ji,jj,jk,Krhs_a))
                  uu(ji,jj,jk,Krhs_a) = zflag * ( uu(ji,jj,jk,Krhs_a) + uu_b(ji,jj,Krhs_a) - zub(ji,jj) ) * umask(ji,jj,jk)
               END DO
            END DO
         END DO
         !   
         DO ji = mi0(ibdy1), mi1(ibdy2)
            zvb(ji,:)  = 0._wp
            zhvb(ji,:) = 0._wp
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  zflag = 0.5_wp - SIGN(0.5_wp, rn_hcri - e3v(ji,jj,jk,Krhs_a))
                  zvb(ji,jj)  =  zvb(ji,jj) + zflag * e3v(ji,jj,jk,Krhs_a) * vv(ji,jj,jk,Krhs_a) * vmask(ji,jj,jk)
                  zhvb(ji,jj) = zhvb(ji,jj) + zflag * e3v(ji,jj,jk,Krhs_a) * vmask(ji,jj,jk)                  
               END DO
            END DO
            DO jj = 1, jpj
!!               zvb(ji,jj) = zvb(ji,jj) * r1_hv(ji,jj,Krhs_a)
               zvb(ji,jj) = zvb(ji,jj) / ( zhvb(ji,jj) + 1._wp - ssvmask(ji,jj))
            END DO
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  zflag = 0.5_wp - SIGN(0.5_wp, rn_hcri - e3v(ji,jj,jk,Krhs_a))
                  vv(ji,jj,jk,Krhs_a) = zflag * ( vv(ji,jj,jk,Krhs_a) + vv_b(ji,jj,Krhs_a) - zvb(ji,jj) )*vmask(ji,jj,jk)
               END DO
            END DO
         END DO
         !
      ENDIF

      ! --- East --- !
      IF( lk_east) THEN
         ibdy1 = jpiglo - ( nn_hls + nbghostcells ) - nn_shift_bar*Agrif_Rhox()    
         ibdy2 = jpiglo - ( nn_hls + 2 )                 
         !
         IF( .NOT.ln_dynspg_ts ) THEN 
            DO ji = mi0(ibdy1), mi1(ibdy2)
               DO jj = 1, jpj
                  uu_b(ji,jj,Krhs_a) = ubdy(ji,jj) * r1_hu(ji,jj,Krhs_a)
               END DO
            END DO
         ENDIF
         !
         DO ji = mi0(ibdy1), mi1(ibdy2)
            zub(ji,:)  = 0._wp 
            zhub(ji,:) = 0._wp   
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  zflag = 0.5_wp - SIGN(0.5_wp, rn_hcri - e3u(ji,jj,jk,Krhs_a))
                  zub(ji,jj)  =  zub(ji,jj) + zflag * e3u(ji,jj,jk,Krhs_a)  * uu(ji,jj,jk,Krhs_a) * umask(ji,jj,jk)
                  zhub(ji,jj) = zhub(ji,jj) + zflag * e3u(ji,jj,jk,Krhs_a)  * umask(ji,jj,jk)
               END DO
            END DO
            DO jj=1,jpj
!!               zub(ji,jj) = zub(ji,jj) * r1_hu(ji,jj,Krhs_a)
               zub(ji,jj) = zub(ji,jj) / ( zhub(ji,jj) + 1._wp - ssumask(ji,jj))
            END DO
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  zflag = 0.5_wp - SIGN(0.5_wp, rn_hcri - e3u(ji,jj,jk,Krhs_a))
                  uu(ji,jj,jk,Krhs_a) = zflag * ( uu(ji,jj,jk,Krhs_a) + uu_b(ji,jj,Krhs_a) - zub(ji,jj) ) * umask(ji,jj,jk)
               END DO
            END DO
         END DO
         !
         ibdy1 = jpiglo - ( nn_hls + nbghostcells - 1 ) - nn_shift_bar*Agrif_Rhox() 
         ibdy2 = jpiglo - ( nn_hls + 1 )     
         !
         IF( .NOT.ln_dynspg_ts ) THEN 
            DO ji = mi0(ibdy1), mi1(ibdy2)
               DO jj = 1, jpj
                  vv_b(ji,jj,Krhs_a) = vbdy(ji,jj) * r1_hv(ji,jj,Krhs_a)
               END DO
            END DO
         ENDIF
         !
         DO ji = mi0(ibdy1), mi1(ibdy2)
            zvb(ji,:)  = 0._wp
            zhvb(ji,:) = 0._wp
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  zflag = 0.5_wp - SIGN(0.5_wp, rn_hcri - e3v(ji,jj,jk,Krhs_a))
                  zvb(ji,jj)  =  zvb(ji,jj) + zflag * e3v(ji,jj,jk,Krhs_a) * vv(ji,jj,jk,Krhs_a) * vmask(ji,jj,jk)
                  zhvb(ji,jj) = zhvb(ji,jj) + zflag * e3v(ji,jj,jk,Krhs_a) * vmask(ji,jj,jk)
               END DO
            END DO
            DO jj = 1, jpj
!!               zvb(ji,jj) = zvb(ji,jj) * r1_hv(ji,jj,Krhs_a)
               zvb(ji,jj) = zvb(ji,jj) / ( zhvb(ji,jj) + 1._wp - ssvmask(ji,jj))
            END DO
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  zflag = 0.5_wp - SIGN(0.5_wp, rn_hcri - e3v(ji,jj,jk,Krhs_a))
                  vv(ji,jj,jk,Krhs_a) = zflag * ( vv(ji,jj,jk,Krhs_a) + vv_b(ji,jj,Krhs_a) - zvb(ji,jj) ) * vmask(ji,jj,jk)
               END DO
            END DO
         END DO
         !
      ENDIF

      ! --- South --- !
      IF( lk_south ) THEN
         jbdy1 = nn_hls + 2                 
         jbdy2 = nn_hls + nbghostcells + nn_shift_bar*Agrif_Rhoy()   
         !
         IF( .NOT.ln_dynspg_ts ) THEN
            DO jj = mj0(jbdy1), mj1(jbdy2)
               DO ji = 1, jpi
                  uu_b(ji,jj,Krhs_a) = ubdy(ji,jj) * r1_hu(ji,jj,Krhs_a)
                  vv_b(ji,jj,Krhs_a) = vbdy(ji,jj) * r1_hv(ji,jj,Krhs_a)
               END DO
            END DO
         ENDIF
         !
         DO jj = mj0(jbdy1), mj1(jbdy2)
            zvb(:,jj) = 0._wp
            zhvb(:,jj) = 0._wp
            DO jk=1,jpkm1
               DO ji=1,jpi
                  zflag = 0.5_wp - SIGN(0.5_wp, rn_hcri - e3v(ji,jj,jk,Krhs_a))
                  zvb(ji,jj)  =  zvb(ji,jj) + zflag * e3v(ji,jj,jk,Krhs_a) * vv(ji,jj,jk,Krhs_a) * vmask(ji,jj,jk)
                  zhvb(ji,jj) = zhvb(ji,jj) + zflag * e3v(ji,jj,jk,Krhs_a) * vmask(ji,jj,jk)
               END DO
            END DO
            DO ji = 1, jpi
!!               zvb(ji,jj) = zvb(ji,jj) * r1_hv(ji,jj,Krhs_a)
               zvb(ji,jj) = zvb(ji,jj) / ( zhvb(ji,jj) + 1._wp - ssvmask(ji,jj))
            END DO
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  zflag = 0.5_wp - SIGN(0.5_wp, rn_hcri - e3v(ji,jj,jk,Krhs_a))
                  vv(ji,jj,jk,Krhs_a) = zflag * ( vv(ji,jj,jk,Krhs_a) + vv_b(ji,jj,Krhs_a) - zvb(ji,jj) ) * vmask(ji,jj,jk)
               END DO
            END DO
         END DO
         !
         DO jj = mj0(jbdy1), mj1(jbdy2)
            zub(:,jj) = 0._wp
            zhub(:,jj) = 0._wp
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  zflag = 0.5_wp - SIGN(0.5_wp, rn_hcri - e3u(ji,jj,jk,Krhs_a))
                  zub(ji,jj)  =  zub(ji,jj) + zflag * e3u(ji,jj,jk,Krhs_a) * uu(ji,jj,jk,Krhs_a) * umask(ji,jj,jk)
                  zhub(ji,jj) = zhub(ji,jj) + zflag * e3u(ji,jj,jk,Krhs_a) * umask(ji,jj,jk)
               END DO
            END DO
            DO ji = 1, jpi
!!               zub(ji,jj) = zub(ji,jj) * r1_hu(ji,jj,Krhs_a)
               zub(ji,jj) = zub(ji,jj) / ( zhub(ji,jj) + 1._wp - ssumask(ji,jj))
            END DO
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  zflag = 0.5_wp - SIGN(0.5_wp, rn_hcri - e3u(ji,jj,jk,Krhs_a))
                  uu(ji,jj,jk,Krhs_a) = zflag * ( uu(ji,jj,jk,Krhs_a) + uu_b(ji,jj,Krhs_a) - zub(ji,jj) ) * umask(ji,jj,jk)
               END DO
            END DO
         END DO
         !
      ENDIF

      ! --- North --- !
      IF( lk_north ) THEN
         jbdy1 = jpjglo - ( nn_hls + nbghostcells ) - nn_shift_bar*Agrif_Rhoy() 
         jbdy2 = jpjglo - ( nn_hls + 2 )
         !
         IF( .NOT.ln_dynspg_ts ) THEN
            DO jj = mj0(jbdy1), mj1(jbdy2)
               DO ji = 1, jpi
                  vv_b(ji,jj,Krhs_a) = vbdy(ji,jj) * r1_hv(ji,jj,Krhs_a)
               END DO
            END DO
         ENDIF
         !
         DO jj = mj0(jbdy1), mj1(jbdy2)
            zvb(:,jj) = 0._wp 
            zhvb(:,jj) = 0._wp 
            DO jk=1,jpkm1
               DO ji=1,jpi
                  zflag = 0.5_wp - SIGN(0.5_wp, rn_hcri - e3v(ji,jj,jk,Krhs_a))
                  zvb(ji,jj) = zvb(ji,jj) + zflag * e3v(ji,jj,jk,Krhs_a) * vv(ji,jj,jk,Krhs_a) * vmask(ji,jj,jk)
                  zhvb(ji,jj) = zhvb(ji,jj) + zflag * e3v(ji,jj,jk,Krhs_a) * vmask(ji,jj,jk)
               END DO
            END DO
            DO ji = 1, jpi
!!               zvb(ji,jj) = zvb(ji,jj) * r1_hv(ji,jj,Krhs_a)
               zvb(ji,jj) = zvb(ji,jj) / ( zhvb(ji,jj) + 1._wp - ssvmask(ji,jj))
            END DO
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  zflag = 0.5_wp - SIGN(0.5_wp, rn_hcri - e3v(ji,jj,jk,Krhs_a))
                  vv(ji,jj,jk,Krhs_a) = zflag * ( vv(ji,jj,jk,Krhs_a) + vv_b(ji,jj,Krhs_a) - zvb(ji,jj) ) * vmask(ji,jj,jk)
               END DO
            END DO
         END DO
         !
         jbdy1 = jpjglo - ( nn_hls + nbghostcells -1 ) - nn_shift_bar*Agrif_Rhoy()  
         jbdy2 = jpjglo - ( nn_hls + 1 )
         !
         IF( .NOT.ln_dynspg_ts ) THEN
            DO jj = mj0(jbdy1), mj1(jbdy2)
               DO ji = 1, jpi
                  uu_b(ji,jj,Krhs_a) = ubdy(ji,jj) * r1_hu(ji,jj,Krhs_a)
               END DO
            END DO
         ENDIF
         !
         DO jj = mj0(jbdy1), mj1(jbdy2)
            zub(:,jj) = 0._wp
            zhub(:,jj) = 0._wp
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  zflag = 0.5_wp - SIGN(0.5_wp, rn_hcri - e3u(ji,jj,jk,Krhs_a))
                  zub(ji,jj)  =  zub(ji,jj) + zflag * e3u(ji,jj,jk,Krhs_a) * uu(ji,jj,jk,Krhs_a) * umask(ji,jj,jk)
                  zhub(ji,jj) = zhub(ji,jj) + zflag * e3u(ji,jj,jk,Krhs_a) * umask(ji,jj,jk)
               END DO
            END DO
            DO ji = 1, jpi
               !!zub(ji,jj) = zub(ji,jj) * r1_hu(ji,jj,Krhs_a)
               zub(ji,jj) = zub(ji,jj) / ( zhub(ji,jj) + 1._wp - ssumask(ji,jj))
            END DO
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  zflag = 0.5_wp - SIGN(0.5_wp, rn_hcri - e3u(ji,jj,jk,Krhs_a))
                  uu(ji,jj,jk,Krhs_a) = zflag * ( uu(ji,jj,jk,Krhs_a) + uu_b(ji,jj,Krhs_a) - zub(ji,jj) ) * umask(ji,jj,jk)
               END DO
            END DO
         END DO
         !
      ENDIF
      !
   END SUBROUTINE Agrif_dyn


   SUBROUTINE Agrif_dyn_ts( jn )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE Agrif_dyn_ts  ***
      !!----------------------------------------------------------------------  
      INTEGER, INTENT(in) ::   jn
      !!
      INTEGER :: ji, jj
      INTEGER :: istart, iend, jstart, jend
      !!----------------------------------------------------------------------  
      !
      IF( Agrif_Root() ) THEN
#if defined PARENT_EXT_BDY
         ! Assume persistance for barotropic mode well inside overlapping zone
         ua_e(:,:) =            umask_upd(:,:)  * uu_b(:,:,Kmm_a)              &
                   &                            * hu(:,:,Kmm_a) * hur_e(:,:)   &
                   & + (1._wp - umask_upd(:,:)) * ua_e(:,:)
         va_e(:,:) =            vmask_upd(:,:)  * vv_b(:,:,Kmm_a)              &
                   &                            * hv(:,:,Kmm_a) * hvr_e(:,:)   &
                   & + (1._wp - vmask_upd(:,:)) * va_e(:,:)
#endif
      ELSE
         !
         !--- West ---!
         IF( lk_west ) THEN
            istart = nn_hls + 2                              ! halo + land + 1
            iend   = nn_hls + nbghostcells  + nn_shift_bar*Agrif_Rhox()              ! halo + land + nbghostcells
            DO ji = mi0(istart), mi1(iend)
               DO jj=1,jpj
                  va_e(ji,jj) = vbdy(ji,jj) * hvr_e(ji,jj)
                  ua_e(ji,jj) = ubdy(ji,jj) * hur_e(ji,jj)
               END DO
            END DO
         ENDIF
         !
         !--- East ---!
         IF( lk_east ) THEN
            istart = jpiglo - ( nn_hls + nbghostcells -1 ) - nn_shift_bar*Agrif_Rhox() 
            iend   = jpiglo - ( nn_hls + 1 )                
            DO ji = mi0(istart), mi1(iend)
   
               DO jj=1,jpj
                  va_e(ji,jj) = vbdy(ji,jj) * hvr_e(ji,jj)
               END DO
            END DO
            istart = jpiglo - ( nn_hls + nbghostcells ) - nn_shift_bar*Agrif_Rhox() 
            iend   = jpiglo - ( nn_hls + 2 )                
            DO ji = mi0(istart), mi1(iend)
               DO jj=1,jpj
                  ua_e(ji,jj) = ubdy(ji,jj) * hur_e(ji,jj)
               END DO
            END DO
         ENDIF 
         !
         !--- South ---!
         IF( lk_south ) THEN
            jstart = nn_hls + 2                              
            jend   = nn_hls + nbghostcells + nn_shift_bar*Agrif_Rhoy()           
            DO jj = mj0(jstart), mj1(jend)
   
               DO ji=1,jpi
                  ua_e(ji,jj) = ubdy(ji,jj) * hur_e(ji,jj)
                  va_e(ji,jj) = vbdy(ji,jj) * hvr_e(ji,jj)
               END DO
            END DO
         ENDIF       
         !
         !--- North ---!
         IF( lk_north ) THEN
            jstart = jpjglo - ( nn_hls + nbghostcells -1 ) - nn_shift_bar*Agrif_Rhoy()     
            jend   = jpjglo - ( nn_hls + 1 )                
            DO jj = mj0(jstart), mj1(jend)
               DO ji=1,jpi
                  ua_e(ji,jj) = ubdy(ji,jj) * hur_e(ji,jj)
               END DO
            END DO
            jstart = jpjglo - ( nn_hls + nbghostcells ) - nn_shift_bar*Agrif_Rhoy() 
            jend   = jpjglo - ( nn_hls + 2 )                
            DO jj = mj0(jstart), mj1(jend)
               DO ji=1,jpi
                  va_e(ji,jj) = vbdy(ji,jj) * hvr_e(ji,jj)
               END DO
            END DO
         ENDIF 
         !
      ENDIF
   END SUBROUTINE Agrif_dyn_ts

   
   SUBROUTINE Agrif_dyn_ts_flux( jn, zu, zv )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE Agrif_dyn_ts_flux  ***
      !!----------------------------------------------------------------------  
      INTEGER, INTENT(in) ::   jn
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) ::   zu, zv
      !!
      INTEGER :: ji, jj
      INTEGER :: istart, iend, jstart, jend
      !!----------------------------------------------------------------------  
      !
      IF( Agrif_Root() ) THEN
#if defined PARENT_EXT_BDY
         ! Assume persistance for barotropic mode well inside overlapping zone
         zu(:,:) =              umask_upd(:,:)  * uu_b(:,:,Kmm_a)             &
                   &                            *   hu(:,:,Kmm_a) * e2u(:,:)  &
                   & + (1._wp - umask_upd(:,:)) *   zu(:,:)
         zv(:,:) =              vmask_upd(:,:)  * vv_b(:,:,Kmm_a)             &
                   &                            *   hv(:,:,Kmm_a) * e1v(:,:)  &
                   & + (1._wp - vmask_upd(:,:)) *   zv(:,:)
#endif
      ELSE 
         !
         !--- West ---!
         IF( lk_west ) THEN
            istart = nn_hls + 2                              
            iend   = nn_hls + nbghostcells + nn_shift_bar*Agrif_Rhox() 
            DO ji = mi0(istart), mi1(iend)
               DO jj=1,jpj
                  zv(ji,jj) = vbdy(ji,jj) * e1v(ji,jj)
                  zu(ji,jj) = ubdy(ji,jj) * e2u(ji,jj)
               END DO
            END DO
         ENDIF
         !
         !--- East ---!
         IF( lk_east ) THEN
            istart = jpiglo - ( nn_hls + nbghostcells -1 ) - nn_shift_bar*Agrif_Rhox()
            iend   = jpiglo - ( nn_hls + 1 )                 
            DO ji = mi0(istart), mi1(iend)
               DO jj=1,jpj
                  zv(ji,jj) = vbdy(ji,jj) * e1v(ji,jj)
               END DO
            END DO
            istart = jpiglo - ( nn_hls + nbghostcells ) - nn_shift_bar*Agrif_Rhox() 
            iend   = jpiglo - ( nn_hls + 2 )                 
            DO ji = mi0(istart), mi1(iend)
               DO jj=1,jpj
                  zu(ji,jj) = ubdy(ji,jj) * e2u(ji,jj)
               END DO
            END DO
         ENDIF
         !
         !--- South ---!
         IF( lk_south ) THEN
            jstart = nn_hls + 2                              
            jend   = nn_hls + nbghostcells + nn_shift_bar*Agrif_Rhoy() 
            DO jj = mj0(jstart), mj1(jend)
               DO ji=1,jpi
                  zu(ji,jj) = ubdy(ji,jj) * e2u(ji,jj)
                  zv(ji,jj) = vbdy(ji,jj) * e1v(ji,jj)
               END DO
            END DO
         ENDIF
         !
         !--- North ---!
         IF( lk_north ) THEN
            jstart = jpjglo - ( nn_hls + nbghostcells -1 ) - nn_shift_bar*Agrif_Rhoy() 
            jend   = jpjglo - ( nn_hls + 1 )                
            DO jj = mj0(jstart), mj1(jend)
               DO ji=1,jpi
                  zu(ji,jj) = ubdy(ji,jj) * e2u(ji,jj)
               END DO
            END DO
            jstart = jpjglo - ( nn_hls + nbghostcells ) - nn_shift_bar*Agrif_Rhoy() 
            jend   = jpjglo - ( nn_hls + 2 )               
            DO jj = mj0(jstart), mj1(jend)
               DO ji=1,jpi
                  zv(ji,jj) = vbdy(ji,jj) * e1v(ji,jj)
               END DO
            END DO
         ENDIF
      ENDIF
      !
   END SUBROUTINE Agrif_dyn_ts_flux

   
   SUBROUTINE Agrif_dta_ts( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE Agrif_dta_ts  ***
      !!----------------------------------------------------------------------  
      INTEGER, INTENT(in) ::   kt
      !!
      LOGICAL :: ll_int_cons
      !!----------------------------------------------------------------------  
      !
      IF( Agrif_Root() )   RETURN
      !
      ll_int_cons = ln_bt_fw ! Assume conservative temporal integration in the forward case only
      !
      ! Enforce volume conservation if no time refinement:  
      IF ( Agrif_rhot()==1 ) ll_int_cons=.TRUE.  
      !
      ! Interpolate barotropic fluxes
      Agrif_SpecialValue = 0._wp
      Agrif_UseSpecialValue = ln_spc_dyn 

      use_sign_north = .TRUE.
      sign_north = -1.

      !
      ! Set bdy time interpolation stage to 0 (latter incremented locally do deal with corners)
      utint_stage(:,:) = 0
      vtint_stage(:,:) = 0
      !
      IF( ll_int_cons ) THEN  ! Conservative interpolation
         Agrif_UseSpecialValue = .FALSE. ! To ensure divergence conservation 
         !
         IF ( lk_tint2d_constant ) THEN
            CALL Agrif_Bc_variable( ub2b_interp_id, calledweight=1._wp, procname=interpub2b_const )
            CALL Agrif_Bc_variable( vb2b_interp_id, calledweight=1._wp, procname=interpvb2b_const ) 
            ! Divergence conserving correction terms:
! JC: Disable this until we found a workaround around masked corners:
!            IF ( Agrif_Rhox()>1 ) CALL Agrif_Bc_variable(    ub2b_cor_id, calledweight=1._wp, procname=ub2b_cor )
!            IF ( Agrif_Rhoy()>1 ) CALL Agrif_Bc_variable(    vb2b_cor_id, calledweight=1._wp, procname=vb2b_cor )
         ELSE
            ! order matters here !!!!!!
            CALL Agrif_Bc_variable( ub2b_interp_id, calledweight=1._wp, procname=interpub2b ) ! Time integrated
            CALL Agrif_Bc_variable( vb2b_interp_id, calledweight=1._wp, procname=interpvb2b ) 
            !
            bdy_tinterp = 1
            CALL Agrif_Bc_variable( unb_interp_id , calledweight=1._wp, procname=interpunb  ) ! After
            CALL Agrif_Bc_variable( vnb_interp_id , calledweight=1._wp, procname=interpvnb  )  
            !
            bdy_tinterp = 2
            CALL Agrif_Bc_variable( unb_interp_id , calledweight=0._wp, procname=interpunb  ) ! Before
            CALL Agrif_Bc_variable( vnb_interp_id , calledweight=0._wp, procname=interpvnb  )   
         ENDIF
      ELSE ! Linear interpolation
         !
         ubdy(:,:) = 0._wp   ;   vbdy(:,:) = 0._wp 
         CALL Agrif_Bc_variable( unb_interp_id, procname=interpunb )
         CALL Agrif_Bc_variable( vnb_interp_id, procname=interpvnb )
      ENDIF
      !
      Agrif_UseSpecialValue = .FALSE.
      use_sign_north = .FALSE.
      !
      ! Set ssh forcing over ghost zone:
      ! No temporal interpolation here
      IF (lk_div_cons)  CALL Agrif_Bc_variable( sshn_frc_id, calledweight=1._wp, procname=interpsshn_frc )
      ! 
   END SUBROUTINE Agrif_dta_ts


   SUBROUTINE Agrif_ssh( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE Agrif_ssh  ***
      !!----------------------------------------------------------------------  
      INTEGER, INTENT(in) ::   kt
      !
      INTEGER  :: ji, jj
      INTEGER  :: istart, iend, jstart, jend
      !!----------------------------------------------------------------------  
      !
      IF( Agrif_Root() )   RETURN
      !      
      ! Linear time interpolation of sea level
      !
      Agrif_SpecialValue    = 0._wp
      Agrif_UseSpecialValue = l_spc_ssh 
      CALL Agrif_Bc_variable(sshn_id, procname=interpsshn )
      Agrif_UseSpecialValue = .FALSE.
      !
      ! --- West --- !
      IF(lk_west) THEN
         istart = nn_hls + 2                                                          ! halo + land + 1
         iend   = nn_hls + nbghostcells + nn_shift_bar*Agrif_Rhox()               ! halo + land + nbghostcells
         IF (lk_div_cons) iend = istart
         DO ji = mi0(istart), mi1(iend)
            DO jj = 1, jpj
               ssh(ji,jj,Krhs_a) = hbdy(ji,jj)
            END DO
         END DO
      ENDIF
      !
      ! --- East --- !
      IF(lk_east) THEN
         istart = jpiglo - ( nn_hls + nbghostcells -1 ) - nn_shift_bar*Agrif_Rhox()       ! halo + land + nbghostcells - 1
         iend   = jpiglo - ( nn_hls + 1 )                                              ! halo + land + 1            - 1
         IF (lk_div_cons) istart = iend
         DO ji = mi0(istart), mi1(iend)
            DO jj = 1, jpj
               ssh(ji,jj,Krhs_a) = hbdy(ji,jj)
            END DO
         END DO
      ENDIF
      !
      ! --- South --- !
      IF(lk_south) THEN
         jstart = nn_hls + 2                                                          ! halo + land + 1
         jend   = nn_hls + nbghostcells + nn_shift_bar*Agrif_Rhoy()               ! halo + land + nbghostcells
         IF (lk_div_cons) jend = jstart
         DO jj = mj0(jstart), mj1(jend)
            DO ji = 1, jpi
               ssh(ji,jj,Krhs_a) = hbdy(ji,jj)
            END DO
         END DO
      ENDIF
      !
      ! --- North --- !
      IF(lk_north) THEN
         jstart = jpjglo - ( nn_hls + nbghostcells -1 ) - nn_shift_bar*Agrif_Rhoy()     ! halo + land + nbghostcells - 1
         jend   = jpjglo - ( nn_hls + 1 )                                            ! halo + land + 1            - 1
         IF (lk_div_cons) jstart = jend
         DO jj = mj0(jstart), mj1(jend)
            DO ji = 1, jpi
               ssh(ji,jj,Krhs_a) = hbdy(ji,jj)
            END DO
         END DO
      ENDIF
      !
   END SUBROUTINE Agrif_ssh


   SUBROUTINE Agrif_ssh_ts( jn )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE Agrif_ssh_ts  ***
      !!----------------------------------------------------------------------  
      INTEGER, INTENT(in) ::   jn
      !!
      INTEGER :: ji, jj
      INTEGER  :: istart, iend, jstart, jend
      !!----------------------------------------------------------------------  
      !
      IF( Agrif_Root() ) THEN 
#if defined PARENT_EXT_BDY
         ! Assume persistence well inside overlapping domain 
         ssha_e(:,:) =            tmask_upd(:,:)  * ssh(:,:,Kmm_a) &
                     & + (1._wp - tmask_upd(:,:)) * ssha_e(:,:)
#endif
      ELSE 
         !
         ! --- West --- !
         IF(lk_west) THEN
            istart = nn_hls + 2                                                        ! halo + land + 1
            iend   = nn_hls + nbghostcells + nn_shift_bar*Agrif_Rhox()             ! halo + land + nbghostcells
            IF (lk_div_cons) iend = istart
            DO ji = mi0(istart), mi1(iend)
               DO jj = 1, jpj
                  ssha_e(ji,jj) = hbdy(ji,jj)
               END DO
            END DO
         ENDIF
         !
         ! --- East --- !
         IF(lk_east) THEN
            istart = jpiglo - ( nn_hls + nbghostcells -1 ) - nn_shift_bar*Agrif_Rhox()    ! halo + land + nbghostcells - 1
            iend   = jpiglo - ( nn_hls + 1 )                                           ! halo + land + 1            - 1
            IF (lk_div_cons) istart = iend
            DO ji = mi0(istart), mi1(iend)
               DO jj = 1, jpj
                  ssha_e(ji,jj) = hbdy(ji,jj)
               END DO
            END DO
         ENDIF
         !
         ! --- South --- !
         IF(lk_south) THEN
            jstart = nn_hls + 2                                                        ! halo + land + 1
            jend   = nn_hls + nbghostcells + nn_shift_bar*Agrif_Rhoy()             ! halo + land + nbghostcells
            IF (lk_div_cons) jend   = jstart
            DO jj = mj0(jstart), mj1(jend)
               DO ji = 1, jpi
                  ssha_e(ji,jj) = hbdy(ji,jj)
               END DO
            END DO
         ENDIF
         !
         ! --- North --- !
         IF(lk_north) THEN
            jstart = jpjglo - ( nn_hls + nbghostcells -1 ) - nn_shift_bar*Agrif_Rhoy()    ! halo + land + nbghostcells - 1
            jend   = jpjglo - ( nn_hls + 1 )                                           ! halo + land + 1            - 1
            IF (lk_div_cons) jstart = jend
            DO jj = mj0(jstart), mj1(jend)
               DO ji = 1, jpi
                  ssha_e(ji,jj) = hbdy(ji,jj)
               END DO
            END DO
         ENDIF
      ENDIF
      !
   END SUBROUTINE Agrif_ssh_ts

   
   SUBROUTINE Agrif_avm
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE Agrif_avm  ***
      !!----------------------------------------------------------------------  
      REAL(wp) ::   zalpha
      !!----------------------------------------------------------------------  
      !
      IF( Agrif_Root() )   RETURN
      !
      zalpha = 1._wp ! JC: proper time interpolation impossible  
                     ! => use last available value from parent 
      !
      Agrif_SpecialValue    = 0.e0
      Agrif_UseSpecialValue = .TRUE.
      l_vremap              = ln_vert_remap
      !
      CALL Agrif_Bc_variable( avm_id, calledweight=zalpha, procname=interpavm )       
      !
      Agrif_UseSpecialValue = .FALSE.
      l_vremap              = .FALSE.
      !
   END SUBROUTINE Agrif_avm


   SUBROUTINE interptsn( ptab, i1, i2, j1, j2, k1, k2, n1, n2, before )
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2), INTENT(inout) ::   ptab
      INTEGER                                     , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2, n1, n2
      LOGICAL                                     , INTENT(in   ) ::   before
      !
      INTEGER  ::   ji, jj, jk, jn  ! dummy loop indices
      INTEGER  ::   N_in, N_out
      INTEGER  :: item
      ! vertical interpolation:
      REAL(wp) :: zhtot, zwgt
      REAL(wp), DIMENSION(k1:k2,1:jpts) :: tabin, tabin_i
      REAL(wp), DIMENSION(k1:k2) :: z_in, h_in
      REAL(wp), DIMENSION(1:jpk) :: h_out, z_out
      !!----------------------------------------------------------------------

      IF( before ) THEN

         item = Kmm_a
         IF( l_ini_child )   Kmm_a = Kbb_a  

         DO jn = 1,jpts
            DO jk=k1,k2-1
               DO jj=j1,j2
                 DO ji=i1,i2
                       ptab(ji,jj,jk,jn) = ts(ji,jj,jk,jn,Kmm_a)
                 END DO
              END DO
           END DO
         END DO

         IF( l_vremap .OR. l_ini_child .OR. ln_zps ) THEN

            ! Fill cell depths (i.e. gdept) to be interpolated
            ! Warning: these are masked, hence extrapolated prior interpolation.
            DO jj=j1,j2
               DO ji=i1,i2
                  ptab(ji,jj,k1,jpts+1) = 0.5_wp * tmask(ji,jj,k1) * e3w(ji,jj,k1,Kmm_a)
                  DO jk=k1+1,k2-1
                     ptab(ji,jj,jk,jpts+1) = tmask(ji,jj,jk) * &
                        & ( ptab(ji,jj,jk-1,jpts+1) + e3w(ji,jj,jk,Kmm_a) ) 
                  END DO
               END DO
            END DO
         
            ! Save ssh at last level:
            IF (.NOT.ln_linssh) THEN
               ptab(i1:i2,j1:j2,k2,jpts+1) = ssh(i1:i2,j1:j2,Kmm_a)*tmask(i1:i2,j1:j2,1) 
            END IF      
         ENDIF
         Kmm_a = item

      ELSE 
         item = Krhs_a
         IF( l_ini_child )   Krhs_a = Kbb_a  

         IF( l_vremap .OR. l_ini_child ) THEN
            IF (ln_linssh) THEN
               ptab(i1:i2,j1:j2,k2,n2) = 0._wp 

            ELSE ! Assuming parent volume follows child:
               ptab(i1:i2,j1:j2,k2,n2) = ssh(i1:i2,j1:j2,Krhs_a)          
            ENDIF

            DO jj=j1,j2
               DO ji=i1,i2
                  ts(ji,jj,:,:,Krhs_a) = 0._wp  
                  !
                  ! Build vertical grids:
                  ! N_in = mbkt_parent(ji,jj)
                  ! Input grid (account for partial cells if any):
                  N_in = k2-1
                  z_in(1) = ptab(ji,jj,1,n2) - ptab(ji,jj,k2,n2)
                  DO jk=2,k2
                     z_in(jk) = ptab(ji,jj,jk,n2) - ptab(ji,jj,k2,n2)
                     IF (( z_in(jk) <= z_in(jk-1) ).OR.(z_in(jk)>ht_0(ji,jj))) EXIT
                  END DO
                  N_in = jk-1
                  DO jk=1, N_in
                     tabin(jk,1:jpts) = ptab(ji,jj,jk,1:jpts)
                  END DO

                  IF (ssmask(ji,jj)==1._wp) THEN
                     N_out = mbkt(ji,jj)
                  ELSE
                     N_out = 0
                  ENDIF

                  IF (N_in*N_out > 0) THEN
                     IF ( l_vremap ) THEN
                        DO jk = 1, N_in
                           h_in(jk) = e3t0_parent(ji,jj,jk) * & 
                             &       (1._wp + ptab(ji,jj,k2,n2)/(ht0_parent(ji,jj)*ssmask(ji,jj) + 1._wp - ssmask(ji,jj)))
                        END DO
                        z_in(1) = 0.5_wp * h_in(1)
                        DO jk=2,N_in
                           z_in(jk) = z_in(jk-1) + 0.5_wp * ( h_in(jk) + h_in(jk-1) )
                        END DO
                        z_in(1:N_in) = z_in(1:N_in)  - ptab(ji,jj,k2,n2)
                     ENDIF                              

                     ! Output (Child) grid:
                     DO jk=1,N_out
                        h_out(jk) = e3t(ji,jj,jk,Krhs_a)
                     END DO
                     z_out(1) = 0.5_wp * e3w(ji,jj,1,Krhs_a) 
                     DO jk=2,N_out
                        z_out(jk) = z_out(jk-1) + e3w(ji,jj,jk,Krhs_a) 
                     END DO
                     IF (.NOT.ln_linssh) z_out(1:N_out) = z_out(1:N_out)  - ssh(ji,jj,Krhs_a)

                     IF( l_ini_child ) THEN
                        CALL remap_linear(tabin(1:N_in,1:jpts),z_in(1:N_in),ts(ji,jj,1:N_out,1:jpts,Krhs_a),          &
                                      &   z_out(1:N_out),N_in,N_out,jpts)  
                     ELSE 
                        CALL reconstructandremap(tabin(1:N_in,1:jpts),h_in(1:N_in),ts(ji,jj,1:N_out,1:jpts,Krhs_a),   &
                                      &   h_out(1:N_out),N_in,N_out,jpts)  
                     ENDIF
                  ENDIF
               END DO
            END DO
            Krhs_a = item
 
         ELSE
         
            IF ( Agrif_Parent(ln_zps) ) THEN ! Account for partial cells 
                                             ! linear vertical interpolation
               DO jj=j1,j2
                  DO ji=i1,i2
                     !
                     N_in  = mbkt(ji,jj)
                     N_out = mbkt(ji,jj)
                     z_in(1) = ptab(ji,jj,1,n2)
                     tabin(1,1:jpts) = ptab(ji,jj,1,1:jpts)
                     DO jk=2, N_in
                        z_in(jk) = ptab(ji,jj,jk,n2)
                        tabin(jk,1:jpts) = ptab(ji,jj,jk,1:jpts)
                     END DO
                     IF (.NOT.ln_linssh) z_in(1:N_in) = z_in(1:N_in) - ptab(ji,jj,k2,n2)
                     z_out(1) = 0.5_wp * e3w(ji,jj,1,Krhs_a)
                     DO jk=2, N_out
                        z_out(jk) = z_out(jk-1) + e3w(ji,jj,jk,Krhs_a) 
                     END DO
                     IF (.NOT.ln_linssh) z_out(1:N_out) = z_out(1:N_out) - ssh(ji,jj,Krhs_a)
                     CALL remap_linear(tabin(1:N_in,1:jpts),z_in(1:N_in),ptab(ji,jj,1:N_out,1:jpts), &
                                   &   z_out(1:N_out),N_in,N_out,jpts)  
                  END DO
               END DO
            ENDIF

            DO jn =1, jpts
               ts(i1:i2,j1:j2,1:jpkm1,jn,Krhs_a) = ptab(i1:i2,j1:j2,1:jpkm1,jn)*tmask(i1:i2,j1:j2,1:jpkm1)
            END DO
         ENDIF

      ENDIF
      !
   END SUBROUTINE interptsn

   
   SUBROUTINE interpsshn( ptab, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpsshn  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      !
      !!----------------------------------------------------------------------  
      !
      IF( before) THEN
         ptab(i1:i2,j1:j2) = ssh(i1:i2,j1:j2,Kmm_a)
      ELSE
         IF( l_ini_child ) THEN
            ssh(i1:i2,j1:j2,Krhs_a) = ptab(i1:i2,j1:j2) * tmask(i1:i2,j1:j2,1)
         ELSE
            hbdy(i1:i2,j1:j2) = ptab(i1:i2,j1:j2) * tmask(i1:i2,j1:j2,1)
         ENDIF
      ENDIF
      !
   END SUBROUTINE interpsshn

   
   SUBROUTINE interpsshn_frc( ptab, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpsshn  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      !
      !!----------------------------------------------------------------------  
      !
      IF( before) THEN
         ptab(i1:i2,j1:j2) = ssh_frc(i1:i2,j1:j2)
      ELSE
         ssh_frc(i1:i2,j1:j2) = ptab(i1:i2,j1:j2)
      ENDIF
      !
   END SUBROUTINE interpsshn_frc


   SUBROUTINE interpun( ptab, i1, i2, j1, j2, k1, k2, m1, m2, before )
      !!----------------------------------------------------------------------
      !!                  *** ROUTINE interpun ***
      !!---------------------------------------------    
      !!
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2,m1,m2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,m1:m2), INTENT(inout) :: ptab
      LOGICAL, INTENT(in) :: before
      !!
      INTEGER :: ji,jj,jk
      REAL(wp) :: zrhoy
      ! vertical interpolation:
      REAL(wp), DIMENSION(i1:i2,j1:j2) :: zsshu
      REAL(wp), DIMENSION(k1:k2) :: tabin, h_in, z_in
      REAL(wp), DIMENSION(1:jpk) :: h_out, z_out
      INTEGER  :: N_in, N_out, item
      !!---------------------------------------------    
      !
      IF (before) THEN 

         item = Kmm_a
         IF( l_ini_child )   Kmm_a = Kbb_a     

         DO jk=k1,k2-1
            DO jj=j1,j2
               DO ji=i1,i2
                  ptab(ji,jj,jk,1) = (e2u(ji,jj) * e3u(ji,jj,jk,Kmm_a) * uu(ji,jj,jk,Kmm_a)*umask(ji,jj,jk)) 
                  !!IF( l_vremap .OR. l_ini_child) THEN
                  !!   ! Interpolate thicknesses (masked for subsequent extrapolation)
                  !!   ptab(ji,jj,jk,2) = umask(ji,jj,jk) * e2u(ji,jj) * e3u(ji,jj,jk,Kmm_a)
                  !!ENDIF
               END DO
            END DO
         END DO

         Kmm_a = item
         
      ELSE
         zrhoy = Agrif_rhoy()

         IF( l_vremap ) THEN

            zsshu(i1:i2,j1:j2) = 0._wp 
            
            IF ( .NOT.ln_linssh ) THEN
               zsshu(i1:i2,j1:j2) = hu(i1:i2,j1:j2,Krhs_a) - hu_0(i1:i2,j1:j2)   
            ENDIF   

            DO ji=i1,i2
               DO jj=j1,j2
                  uu(ji,jj,:,Krhs_a) = 0._wp
                  N_in  = mbku_parent(ji,jj)
                  N_out = mbku(ji,jj)
                  IF (N_in*N_out > 0) THEN

                     DO jk=1,N_in
                        h_in(jk)  = e3u0_parent(ji,jj,jk) * & 
                             &       (1._wp + zsshu(ji,jj)/(hu0_parent(ji,jj)*ssumask(ji,jj) + 1._wp - ssumask(ji,jj)))
                        tabin(jk) = ptab(ji,jj,jk,1) / (e2u(ji,jj)*zrhoy*h_in(jk))
                     END DO
                     
                     DO jk=1, N_out
                        h_out(jk) = e3u(ji,jj,jk,Krhs_a)
                     END DO

                     IF( l_ini_child ) THEN
                        z_in(1) = 0.5_wp * h_in(1)
                        DO jk=2,N_in
                           z_in(jk) = z_in(jk-1) + 0.5_wp * (h_in(jk)+h_in(jk-1))
                        END DO
                        !
                        z_out(1) = 0.5_wp * h_out(1)
                        DO jk=2,N_out
                           z_out(jk) = z_out(jk-1) + 0.5_wp * (h_out(jk-1) + h_out(jk)) 
                        END DO  

                        CALL remap_linear       (tabin(1:N_in),z_in(1:N_in),uu(ji,jj,1:N_out,Krhs_a),z_out(1:N_out),N_in,N_out,1)
                     ELSE
                        CALL reconstructandremap(tabin(1:N_in),h_in(1:N_in),uu(ji,jj,1:N_out,Krhs_a),h_out(1:N_out),N_in,N_out,1)
                     ENDIF   
                  ENDIF
               END DO
            END DO
         ELSE
            DO jk = 1, jpkm1
               uu(i1:i2,j1:j2,jk,Krhs_a) = ptab(i1:i2,j1:j2,jk,1) / ( zrhoy * e2u(i1:i2,j1:j2) * e3u(i1:i2,j1:j2,jk,Krhs_a) )
            END DO
         ENDIF

      ENDIF
      ! 
   END SUBROUTINE interpun

   
   SUBROUTINE interpvn( ptab, i1, i2, j1, j2, k1, k2, m1, m2, before )
      !!----------------------------------------------------------------------
      !!                  *** ROUTINE interpvn ***
      !!----------------------------------------------------------------------
      !
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2,m1,m2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,m1:m2), INTENT(inout) :: ptab
      LOGICAL, INTENT(in) :: before
      !
      INTEGER :: ji,jj,jk
      REAL(wp) :: zrhox
      ! vertical interpolation:
      REAL(wp), DIMENSION(i1:i2,j1:j2) :: zsshv
      REAL(wp), DIMENSION(k1:k2) :: tabin, h_in, z_in
      REAL(wp), DIMENSION(1:jpk) :: h_out, z_out
      INTEGER  :: N_in, N_out, item
      !!---------------------------------------------    
      !      
      IF (before) THEN   

         item = Kmm_a
         IF( l_ini_child )   Kmm_a = Kbb_a     
       
         DO jk=k1,k2-1
            DO jj=j1,j2
               DO ji=i1,i2
                  ptab(ji,jj,jk,1) = (e1v(ji,jj) * e3v(ji,jj,jk,Kmm_a) * vv(ji,jj,jk,Kmm_a)*vmask(ji,jj,jk))
                  !!IF( l_vremap .OR. l_ini_child) THEN
                  !!   ! Interpolate thicknesses (masked for subsequent extrapolation)
                  !!   ptab(ji,jj,jk,2) = vmask(ji,jj,jk) * e1v(ji,jj) * e3v(ji,jj,jk,Kmm_a)
                  !!ENDIF
               END DO
            END DO
         END DO

         Kmm_a = item

      ELSE       
         zrhox = Agrif_rhox()

         IF( l_vremap ) THEN

            zsshv(i1:i2,j1:j2) = 0._wp 
            
            IF ( .NOT.ln_linssh ) THEN
               zsshv(i1:i2,j1:j2) = hv(i1:i2,j1:j2,Krhs_a) - hv_0(i1:i2,j1:j2)   
            ENDIF   

            DO ji=i1,i2
               DO jj=j1,j2
                  vv(ji,jj,:,Krhs_a) = 0._wp
                  N_in  = mbkv_parent(ji,jj)
                  N_out = mbkv(ji,jj)
                  IF (N_in*N_out > 0) THEN

                     DO jk=1,N_in
                        h_in(jk)  = e3v0_parent(ji,jj,jk) * & 
                             &       (1._wp + zsshv(ji,jj)/(hv0_parent(ji,jj)*ssvmask(ji,jj) + 1._wp - ssvmask(ji,jj)))
                        tabin(jk) = ptab(ji,jj,jk,1) / (e1v(ji,jj)*zrhox*h_in(jk))
                     END DO
                     
                     DO jk=1, N_out
                        h_out(jk) = e3v(ji,jj,jk,Krhs_a)
                     END DO

                     IF( l_ini_child ) THEN
                        z_in(1) = 0.5_wp * h_in(1)
                        DO jk=2,N_in
                           z_in(jk) = z_in(jk-1) + 0.5_wp * (h_in(jk)+h_in(jk-1))
                        END DO
                        !
                        z_out(1) = 0.5_wp * h_out(1)
                        DO jk=2,N_out
                           z_out(jk) = z_out(jk-1) + 0.5_wp * (h_out(jk-1) + h_out(jk)) 
                        END DO  

                        CALL remap_linear       (tabin(1:N_in),z_in(1:N_in),vv(ji,jj,1:N_out,Krhs_a),z_out(1:N_out),N_in,N_out,1)
                     ELSE
                        CALL reconstructandremap(tabin(1:N_in),h_in(1:N_in),vv(ji,jj,1:N_out,Krhs_a),h_out(1:N_out),N_in,N_out,1)
                     ENDIF   
                  ENDIF
               END DO
            END DO
         ELSE
            DO jk = 1, jpkm1
               vv(i1:i2,j1:j2,jk,Krhs_a) = ptab(i1:i2,j1:j2,jk,1) / ( zrhox * e1v(i1:i2,j1:j2) * e3v(i1:i2,j1:j2,jk,Krhs_a) )
            END DO
         ENDIF
      ENDIF
      !        
   END SUBROUTINE interpvn

   SUBROUTINE interpunb( ptab, i1, i2, j1, j2, before)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpunb  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      !
      INTEGER  ::   ji, jj
      REAL(wp) ::   zrhoy, zrhot, zt0, zt1, ztcoeff
      !!----------------------------------------------------------------------  
      !
      IF( before ) THEN 
         ptab(i1:i2,j1:j2) = e2u(i1:i2,j1:j2) * hu(i1:i2,j1:j2,Kmm_a) * uu_b(i1:i2,j1:j2,Kmm_a)
      ELSE
         zrhoy = Agrif_Rhoy()
         zrhot = Agrif_rhot()
         ! Time indexes bounds for integration
         zt0 = REAL(Agrif_NbStepint()  , wp) / zrhot
         zt1 = REAL(Agrif_NbStepint()+1, wp) / zrhot      
         ! 
         DO ji = i1, i2
            DO jj = j1, j2
               IF ( utint_stage(ji,jj) < (bdy_tinterp + 1) ) THEN
                  IF    ( utint_stage(ji,jj) == 1  ) THEN
                     ztcoeff = zrhot * (  zt1**2._wp * (       zt1 - 1._wp)        &
                        &               - zt0**2._wp * (       zt0 - 1._wp)        )
                  ELSEIF( utint_stage(ji,jj) == 2  ) THEN
                     ztcoeff = zrhot * (  zt1        * (       zt1 - 1._wp)**2._wp &
                        &               - zt0        * (       zt0 - 1._wp)**2._wp )
                  ELSEIF( utint_stage(ji,jj) == 0  ) THEN                
                     ztcoeff = 1._wp
                  ELSE
                     ztcoeff = 0._wp
                  ENDIF
                  !   
                  ubdy(ji,jj) = ubdy(ji,jj) + ztcoeff * ptab(ji,jj)
                  !            
                  IF (( utint_stage(ji,jj) == 2 ).OR.( utint_stage(ji,jj) == 0 )) THEN
                     ubdy(ji,jj) = ubdy(ji,jj) / (zrhoy*e2u(ji,jj)) * umask(ji,jj,1)
                  ENDIF
                  !
                  utint_stage(ji,jj) = utint_stage(ji,jj) + 1
               ENDIF
            END DO
         END DO
      END IF
      ! 
   END SUBROUTINE interpunb


   SUBROUTINE interpvnb( ptab, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpvnb  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      !
      INTEGER  ::   ji, jj
      REAL(wp) ::   zrhox, zrhot, zt0, zt1, ztcoeff   
      !!----------------------------------------------------------------------  
      ! 
      IF( before ) THEN 
         ptab(i1:i2,j1:j2) = e1v(i1:i2,j1:j2) * hv(i1:i2,j1:j2,Kmm_a) * vv_b(i1:i2,j1:j2,Kmm_a)
      ELSE
         zrhox = Agrif_Rhox()
         zrhot = Agrif_rhot()
         ! Time indexes bounds for integration
         zt0 = REAL(Agrif_NbStepint()  , wp) / zrhot
         zt1 = REAL(Agrif_NbStepint()+1, wp) / zrhot 
         !     
         DO ji = i1, i2
            DO jj = j1, j2
               IF ( vtint_stage(ji,jj) < (bdy_tinterp + 1) ) THEN
                  IF    ( vtint_stage(ji,jj) == 1  ) THEN
                     ztcoeff = zrhot * (  zt1**2._wp * (       zt1 - 1._wp)        &
                        &               - zt0**2._wp * (       zt0 - 1._wp)        )
                  ELSEIF( vtint_stage(ji,jj) == 2  ) THEN
                     ztcoeff = zrhot * (  zt1        * (       zt1 - 1._wp)**2._wp &
                        &               - zt0        * (       zt0 - 1._wp)**2._wp )
                  ELSEIF( vtint_stage(ji,jj) == 0  ) THEN                
                     ztcoeff = 1._wp
                  ELSE
                     ztcoeff = 0._wp
                  ENDIF
                  !   
                  vbdy(ji,jj) = vbdy(ji,jj) + ztcoeff * ptab(ji,jj)
                  !            
                  IF (( vtint_stage(ji,jj) == 2 ).OR.( vtint_stage(ji,jj) == 0 )) THEN
                     vbdy(ji,jj) = vbdy(ji,jj) / (zrhox*e1v(ji,jj)) * vmask(ji,jj,1)
                  ENDIF
                  !
                  vtint_stage(ji,jj) = vtint_stage(ji,jj) + 1
               ENDIF
            END DO
         END DO          
      ENDIF
      !
   END SUBROUTINE interpvnb


   SUBROUTINE interpub2b( ptab, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpub2b  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      !
      INTEGER  ::   ji,jj
      REAL(wp) ::   zrhot, zt0, zt1, zat
      !!----------------------------------------------------------------------  
      IF( before ) THEN
!         IF ( ln_bt_fw ) THEN
            ptab(i1:i2,j1:j2) = e2u(i1:i2,j1:j2) * ub2_b(i1:i2,j1:j2)
!         ELSE
!            ptab(i1:i2,j1:j2) = e2u(i1:i2,j1:j2) * un_adv(i1:i2,j1:j2)
!         ENDIF
      ELSE
         zrhot = Agrif_rhot()
         ! Time indexes bounds for integration
         zt0 = REAL(Agrif_NbStepint()  , wp) / zrhot
         zt1 = REAL(Agrif_NbStepint()+1, wp) / zrhot
         ! Polynomial interpolation coefficients:
         zat = zrhot * (  zt1**2._wp * (-2._wp*zt1 + 3._wp)    &
            &           - zt0**2._wp * (-2._wp*zt0 + 3._wp)    ) 
         !
         ubdy(i1:i2,j1:j2) = zat * ptab(i1:i2,j1:j2) 
         !
         ! Update interpolation stage:
         utint_stage(i1:i2,j1:j2) = 1
      ENDIF
      ! 
   END SUBROUTINE interpub2b
   
   SUBROUTINE interpub2b_const( ptab, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpub2b_const  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      !
      REAL(wp) :: zrhoy
      !!----------------------------------------------------------------------  
      IF( before ) THEN
!         IF ( ln_bt_fw ) THEN
            ptab(i1:i2,j1:j2) = e2u(i1:i2,j1:j2) * ub2_b(i1:i2,j1:j2) &
                                * umask(i1:i2,j1:j2,1)
!         ELSE
!            ptab(i1:i2,j1:j2) = e2u(i1:i2,j1:j2) * un_adv(i1:i2,j1:j2)
!         ENDIF
      ELSE
         zrhoy = Agrif_Rhoy()
         !
         ubdy(i1:i2,j1:j2) = ptab(i1:i2,j1:j2) & 
                           & / (zrhoy*e2u(i1:i2,j1:j2)) * umask(i1:i2,j1:j2,1)
         !
      ENDIF
      ! 
   END SUBROUTINE interpub2b_const


   SUBROUTINE ub2b_cor( ptab, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE ub2b_cor  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      !
      INTEGER  :: ji, jj
      INTEGER  :: imin, imax, jmin, jmax
      REAL(wp) :: zrhox, zrhoy, zx
      !!----------------------------------------------------------------------  
      IF( before ) THEN
         ptab(:,:) = 0._wp
         imin = MAX(i1, 2) ; imax = MIN(i2, jpi-1)
         jmin = MAX(j1, 2) ; jmax = MIN(j2, jpj-1)
         DO ji=imin,imax
            DO jj=jmin,jmax
               ptab(ji,jj) = 0.25_wp *(vmask(ji,jj  ,1)                        & 
                           &       * ( vb2_b(ji+1,jj  )*e1v(ji+1,jj  )         & 
                           &          -vb2_b(ji-1,jj  )*e1v(ji-1,jj  ) )       &
                           &          -vmask(ji,jj-1,1)                        & 
                           &       * ( vb2_b(ji+1,jj-1)*e1v(ji+1,jj-1)         &
                           &          -vb2_b(ji-1,jj-1)*e1v(ji-1,jj-1) ) )      
            END DO
         END DO 
      ELSE
         !
         zrhox = Agrif_Rhox() 
         zrhoy = Agrif_Rhoy()
         DO ji=i1,i2
            DO jj=j1,j2
               IF (utint_stage(ji,jj)==0) THEN 
                  zx = 2._wp*MOD(ABS(mig0(ji)-nbghostcells_x_w), INT(Agrif_Rhox()))/zrhox - 1._wp  
                  ubdy(ji,jj) = ubdy(ji,jj) + 0.25_wp*(1._wp-zx*zx) * ptab(ji,jj) & 
                              &         / zrhoy *r1_e2u(ji,jj) * umask(ji,jj,1) 
                  utint_stage(ji,jj) = 1 
               ENDIF
            END DO
         END DO 
         !
      ENDIF
      ! 
   END SUBROUTINE ub2b_cor


   SUBROUTINE interpvb2b( ptab, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpvb2b  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      !
      INTEGER ::   ji,jj
      REAL(wp) ::   zrhot, zt0, zt1, zat
      !!----------------------------------------------------------------------  
      !
      IF( before ) THEN
!         IF ( ln_bt_fw ) THEN
            ptab(i1:i2,j1:j2) = e1v(i1:i2,j1:j2) * vb2_b(i1:i2,j1:j2)
!         ELSE
!            ptab(i1:i2,j1:j2) = e1v(i1:i2,j1:j2) * vn_adv(i1:i2,j1:j2)
!         ENDIF
      ELSE      
         zrhot = Agrif_rhot()
         ! Time indexes bounds for integration
         zt0 = REAL(Agrif_NbStepint()  , wp) / zrhot
         zt1 = REAL(Agrif_NbStepint()+1, wp) / zrhot
         ! Polynomial interpolation coefficients:
         zat = zrhot * (  zt1**2._wp * (-2._wp*zt1 + 3._wp)    &
            &           - zt0**2._wp * (-2._wp*zt0 + 3._wp)    ) 
         !
         vbdy(i1:i2,j1:j2) = zat * ptab(i1:i2,j1:j2)
         !
         ! update interpolation stage:
         vtint_stage(i1:i2,j1:j2) = 1
      ENDIF
      !      
   END SUBROUTINE interpvb2b


   SUBROUTINE interpvb2b_const( ptab, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpub2b_const  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      !
      REAL(wp) :: zrhox
      !!----------------------------------------------------------------------  
      IF( before ) THEN
!         IF ( ln_bt_fw ) THEN
            ptab(i1:i2,j1:j2) = e1v(i1:i2,j1:j2) * vb2_b(i1:i2,j1:j2) &
                                * vmask(i1:i2,j1:j2,1)
!         ELSE
!            ptab(i1:i2,j1:j2) = e1v(i1:i2,j1:j2) * vn_adv(i1:i2,j1:j2)
!         ENDIF
      ELSE
         zrhox = Agrif_Rhox()
         !
         vbdy(i1:i2,j1:j2) = ptab(i1:i2,j1:j2) &
                           & / (zrhox*e1v(i1:i2,j1:j2)) * vmask(i1:i2,j1:j2,1)
         !
      ENDIF
      ! 
   END SUBROUTINE interpvb2b_const

 
   SUBROUTINE vb2b_cor( ptab, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE vb2b_cor  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      !
      INTEGER  :: ji, jj
      INTEGER  :: imin, imax, jmin, jmax
      REAL(wp) :: zrhox, zrhoy, zy, zslope1, zslope2
      !!----------------------------------------------------------------------  
      IF( before ) THEN
         ptab(:,:) = 0._wp
         imin = MAX(i1, 2) ; imax = MIN(i2, jpi-1)
         jmin = MAX(j1, 2) ; jmax = MIN(j2, jpj-1)
         DO ji=imin,imax
            DO jj=jmin,jmax
               ptab(ji,jj) = 0.25_wp *(umask(ji  ,jj,1)                      & 
                           &       * ( ub2_b(ji  ,jj+1)*e2u(ji  ,jj+1)       & 
                           &          -ub2_b(ji  ,jj-1)*e2u(ji  ,jj-1) )     &
                           &          -umask(ji-1,jj,1)                      & 
                           &       * ( ub2_b(ji-1,jj+1)*e2u(ji-1,jj+1)       &
                           &          -ub2_b(ji-1,jj-1)*e2u(ji-1,jj-1) ) )   
            END DO
         END DO 
      ELSE
         !
         zrhox = Agrif_Rhox() 
         zrhoy = Agrif_Rhoy()
         DO ji=i1,i2
            DO jj=j1,j2
               IF (vtint_stage(ji,jj)==0) THEN 
                  zy = 2._wp*MOD(ABS(mjg0(jj)-nbghostcells_y_s), INT(Agrif_Rhoy()))/zrhoy - 1._wp  
                  vbdy(ji,jj) = vbdy(ji,jj) + 0.25_wp*(1._wp-zy*zy) * ptab(ji,jj) & 
                              &         / zrhox * r1_e1v(ji,jj) * vmask(ji,jj,1) 
                  vtint_stage(ji,jj) = 1 
               ENDIF
            END DO
         END DO 
         !
      ENDIF
      ! 
   END SUBROUTINE vb2b_cor


   SUBROUTINE interpe3t0_vremap( ptab, i1, i2, j1, j2, k1, k2, before )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpe3t0_vremap  ***
      !!----------------------------------------------------------------------  
      INTEGER                              , INTENT(in   ) :: i1, i2, j1, j2, k1, k2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: ptab
      LOGICAL                              , INTENT(in   ) :: before
      !
      INTEGER :: ji, jj, jk
      REAL(wp) :: zh
      !!----------------------------------------------------------------------  
      !    
      IF( before ) THEN
         IF ( ln_zps ) THEN
            DO jk = k1, k2
               DO jj = j1, j2
                  DO ji = i1, i2
                     ptab(ji, jj, jk) = e3t_1d(jk)
                  END DO
               END DO
            END DO
         ELSE
            ptab(i1:i2,j1:j2,k1:k2) = e3t_0(i1:i2,j1:j2,k1:k2)
         ENDIF
      ELSE
         !
         DO jk = k1, k2
            DO jj = j1, j2
               DO ji = i1, i2
                  e3t0_parent(ji,jj,jk) = ptab(ji,jj,jk)
               END DO
            END DO
         END DO

         ! Retrieve correct scale factor at the bottom:
         DO jj = j1, j2
            DO ji = i1, i2
               IF ( mbkt_parent(ji,jj) > 1 ) THEN
                  zh = 0._wp
                  DO jk = 1, mbkt_parent(ji, jj)-1
                     zh = zh + e3t0_parent(ji,jj,jk)
                  END DO
                  e3t0_parent(ji,jj,mbkt_parent(ji,jj)) = ht0_parent(ji, jj) - zh
               ENDIF  
            END DO
         END DO
         
      ENDIF
      ! 
   END SUBROUTINE interpe3t0_vremap


   SUBROUTINE interpglamt( ptab, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpglamt  ***
      !!----------------------------------------------------------------------  
      INTEGER                        , INTENT(in   ) :: i1, i2, j1, j2
      REAL(wp),DIMENSION(i1:i2,j1:j2), INTENT(inout) :: ptab
      LOGICAL                        , INTENT(in   ) :: before
      !
      INTEGER :: ji, jj, jk
      REAL(wp):: ztst
      !!----------------------------------------------------------------------  
      !    
      IF( before ) THEN
         ptab(i1:i2,j1:j2) = glamt(i1:i2,j1:j2)
      ELSE
         ztst = MAXVAL(ABS(glamt(i1:i2,j1:j2)))*1.e-4
         DO jj = j1, j2
            DO ji = i1, i2
               IF( ABS( ptab(ji,jj) - glamt(ji,jj) ) > ztst ) THEN
                  WRITE(numout,*) ' Agrif error for glamt: parent, child, i, j ', ptab(ji,jj), glamt(ji,jj), mig0(ji), mig0(jj)
!                  kindic_agr = kindic_agr + 1
               ENDIF
            END DO
         END DO
      ENDIF
      ! 
   END SUBROUTINE interpglamt


   SUBROUTINE interpgphit( ptab, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpgphit  ***
      !!----------------------------------------------------------------------  
      INTEGER                        , INTENT(in   ) :: i1, i2, j1, j2
      REAL(wp),DIMENSION(i1:i2,j1:j2), INTENT(inout) :: ptab
      LOGICAL                        , INTENT(in   ) :: before
      !
      INTEGER :: ji, jj, jk
      REAL(wp):: ztst
      !!----------------------------------------------------------------------  
      !    
      IF( before ) THEN
         ptab(i1:i2,j1:j2) = gphit(i1:i2,j1:j2)
      ELSE
         ztst = MAXVAL(ABS(gphit(i1:i2,j1:j2)))*1.e-4
         DO jj = j1, j2
            DO ji = i1, i2
               IF( ABS( ptab(ji,jj) - gphit(ji,jj) ) > ztst ) THEN
                  WRITE(numout,*) ' Agrif error for gphit: parent, child, i, j ', ptab(ji,jj), gphit(ji,jj), mig0(ji), mig0(jj)
!                  kindic_agr = kindic_agr + 1
               ENDIF
            END DO
         END DO
      ENDIF
      ! 
   END SUBROUTINE interpgphit


   SUBROUTINE interpavm( ptab, i1, i2, j1, j2, k1, k2, m1, m2, before )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interavm  ***
      !!----------------------------------------------------------------------  
      INTEGER                                    , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2, m1, m2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2,m1:m2), INTENT(inout) ::   ptab
      LOGICAL                                    , INTENT(in   ) ::   before
      !
      INTEGER  :: ji, jj, jk
      INTEGER  :: N_in, N_out
      REAL(wp), DIMENSION(k1:k2) :: tabin, z_in
      REAL(wp), DIMENSION(1:jpk) :: z_out
      !!----------------------------------------------------------------------  
      !      
      IF (before) THEN         
         DO jk=k1,k2
            DO jj=j1,j2
              DO ji=i1,i2
                    ptab(ji,jj,jk,1) = avm_k(ji,jj,jk)
              END DO
           END DO
         END DO

         IF( l_vremap ) THEN
            ! Interpolate interfaces 
            ! Warning: these are masked, hence extrapolated prior interpolation.
            DO jk=k1,k2
               DO jj=j1,j2
                  DO ji=i1,i2
                      ptab(ji,jj,jk,2) = tmask(ji,jj,jk) * gdepw(ji,jj,jk,Kmm_a)
                  END DO
               END DO
            END DO
        
           ! Save ssh at last level:
            IF (.NOT.ln_linssh) THEN
               ptab(i1:i2,j1:j2,k2,2) = ssh(i1:i2,j1:j2,Kmm_a)*tmask(i1:i2,j1:j2,1) 
            ELSE
               ptab(i1:i2,j1:j2,k2,2) = 0._wp
            END IF      
          ENDIF

      ELSE 

         IF( l_vremap ) THEN
            IF (ln_linssh) ptab(i1:i2,j1:j2,k2,2) = 0._wp 
            avm_k(i1:i2,j1:j2,1:jpkm1) = 0._wp
               
            DO jj = j1, j2
               DO ji =i1, i2
                  N_in = mbkt_parent(ji,jj)
                  N_out = mbkt(ji,jj)
                  IF (N_in*N_out > 0) THEN
                     DO jk = 1, N_in  ! Parent vertical grid               
                        z_in(jk)  = ptab(ji,jj,jk,2) - ptab(ji,jj,k2,2)
                        tabin(jk) = ptab(ji,jj,jk,1)
                     END DO
                     DO jk = 1, N_out        ! Child vertical grid
                        z_out(jk) = gdepw(ji,jj,jk,Kmm_a) - ssh(ji,jj,Kmm_a)
                     END DO
                     IF (.NOT.ln_linssh) z_out(1:N_out) = z_out(1:N_out)  - ssh(ji,jj,Kmm_a)

                     CALL remap_linear(tabin(1:N_in),z_in(1:N_in),avm_k(ji,jj,1:N_out),z_out(1:N_out),N_in,N_out,1)
                  ENDIF
               END DO
            END DO
         ELSE
            avm_k(i1:i2,j1:j2,1:jpkm1) = ptab (i1:i2,j1:j2,1:jpkm1,1)
         ENDIF
      ENDIF
      !
   END SUBROUTINE interpavm

   
   SUBROUTINE interpmbkt( ptab, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpmbkt  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      !
      !!----------------------------------------------------------------------  
      !
      IF( before) THEN
         ptab(i1:i2,j1:j2) = REAL(mbkt(i1:i2,j1:j2),wp)
      ELSE
         mbkt_parent(i1:i2,j1:j2) = NINT(ptab(i1:i2,j1:j2))
      ENDIF
      !
   END SUBROUTINE interpmbkt

   
   SUBROUTINE interpht0( ptab, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpht0  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      !
      !!----------------------------------------------------------------------  
      !
      IF( before) THEN
         ptab(i1:i2,j1:j2) = ht_0(i1:i2,j1:j2) * ssmask(i1:i2,j1:j2)
      ELSE
         ht0_parent(i1:i2,j1:j2) = ptab(i1:i2,j1:j2) * ssmask(i1:i2,j1:j2)
      ENDIF
      !
   END SUBROUTINE interpht0


   SUBROUTINE interp_e1e2t_frac(tabres, i1, i2, j1, j2, before )
      !
      !!----------------------------------------------------------------------
      !!               *** ROUTINE interp_e1e2t_frac ***
      !!----------------------------------------------------------------------
      INTEGER                        , INTENT(in   ) :: i1, i2, j1, j2
      REAL(wp),DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres
      LOGICAL                        , INTENT(in   ) :: before
      !!
      !!----------------------------------------------------------------------

      IF (before) THEN
         tabres(i1:i2,j1:j2) = e1e2t(i1:i2,j1:j2)
      ELSE
         WHERE (tabres(i1:i2,j1:j2)/=0._wp)
            e1e2t_frac(i1:i2,j1:j2) = e1e2t(i1:i2,j1:j2) &
                 & / tabres(i1:i2,j1:j2) * Agrif_Rhox() * Agrif_Rhoy()
         ELSEWHERE
            e1e2t_frac(i1:i2,j1:j2) = 1._wp
         END WHERE
      ENDIF
      !
   END SUBROUTINE interp_e1e2t_frac


   SUBROUTINE interp_e2u_frac(tabres, i1, i2, j1, j2, before )
      !
      !!----------------------------------------------------------------------
      !!               *** ROUTINE interp_e2u_frac ***
      !!----------------------------------------------------------------------
      INTEGER                        , INTENT(in   ) :: i1, i2, j1, j2
      REAL(wp),DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres
      LOGICAL                        , INTENT(in   ) :: before
      !!
      !!----------------------------------------------------------------------

      IF (before) THEN
         tabres(i1:i2,j1:j2) = e2u(i1:i2,j1:j2)
      ELSE
         WHERE (tabres(i1:i2,j1:j2)/=0._wp)
            e2u_frac(i1:i2,j1:j2) = e2u(i1:i2,j1:j2) &
                 & / tabres(i1:i2,j1:j2) * Agrif_Rhoy()
         ELSE WHERE
            e2u_frac(i1:i2,j1:j2) = 1._wp
         END WHERE
      ENDIF
      !
   END SUBROUTINE interp_e2u_frac


   SUBROUTINE interp_e1v_frac(tabres, i1, i2, j1, j2, before )
      !
      !!----------------------------------------------------------------------
      !!               *** ROUTINE interp_e1v_frac ***
      !!----------------------------------------------------------------------
      INTEGER                        , INTENT(in   ) :: i1, i2, j1, j2
      REAL(wp),DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres
      LOGICAL                        , INTENT(in   ) :: before
      !!
      !!----------------------------------------------------------------------

      IF (before) THEN
         tabres(i1:i2,j1:j2) = e1v(i1:i2,j1:j2)
      ELSE
         WHERE (tabres(i1:i2,j1:j2)/=0._wp)
            e1v_frac(i1:i2,j1:j2) = e1v(i1:i2,j1:j2) &
                 & / tabres(i1:i2,j1:j2) * Agrif_Rhox()
         ELSE WHERE
            e1v_frac(i1:i2,j1:j2) = 1._wp
         END WHERE
      ENDIF
      !
   END SUBROUTINE interp_e1v_frac


   SUBROUTINE Agrif_check_bat( iindic )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE Agrif_check_bat  ***
      !!----------------------------------------------------------------------  
      INTEGER, INTENT(inout) ::   iindic
      !!
      INTEGER :: ji, jj, jk
      INTEGER  :: istart, iend, jstart, jend, ispon
      !!----------------------------------------------------------------------  
      !
      !
      ! --- West --- !
      IF(lk_west) THEN
         ispon  = (nn_sponge_len+2) * Agrif_irhox()
         istart = nn_hls + 2                                  ! halo + land + 1
         iend   = nn_hls + nbghostcells + ispon           ! halo + land + nbghostcells + sponge
         jstart = nn_hls + 2 
         jend   = jpjglo - nn_hls - 1 
         DO ji = mi0(istart), mi1(iend)
            DO jj = mj0(jstart), mj1(jend)
               IF ( ABS(ht0_parent(ji,jj)-ht_0(ji,jj)) > 1.e-3 ) iindic = iindic + 1
               IF ( .NOT.ln_vert_remap) THEN
                  DO jk = 1, jpkm1
                     IF ( ABS(e3t0_parent(ji,jj,jk)-e3t_0(ji,jj,jk))*tmask(ji,jj,jk) > 1.e-3 ) iindic = iindic + 1
                  END DO 
               ENDIF
            END DO
            DO jj = mj0(jstart), mj1(jend-1)
               IF ( ABS(hv0_parent(ji,jj)-hv_0(ji,jj)) > 1.e-3 ) iindic = iindic + 1
               IF ( .NOT.ln_vert_remap) THEN
                  DO jk = 1, jpkm1
                     IF ( ABS(e3v0_parent(ji,jj,jk)-e3v_0(ji,jj,jk))*vmask(ji,jj,jk) > 1.e-3 ) iindic = iindic + 1
                  END DO 
               ENDIF
            END DO
         END DO
         DO ji = mi0(istart), mi1(iend-1)
            DO jj = mj0(jstart), mj1(jend)
               IF ( ABS(hu0_parent(ji,jj)-hu_0(ji,jj)) > 1.e-3 ) iindic = iindic + 1
               IF ( .NOT.ln_vert_remap) THEN
                  DO jk = 1, jpkm1
                     IF ( ABS(e3u0_parent(ji,jj,jk)-e3u_0(ji,jj,jk))*umask(ji,jj,jk) > 1.e-3 ) iindic = iindic + 1
                  END DO 
               ENDIF
            END DO
         END DO
      ENDIF
      !
      ! --- East --- !
      IF(lk_east) THEN
         ispon  = (nn_sponge_len+2) * Agrif_irhox() 
         istart = jpiglo - ( nn_hls + nbghostcells + ispon -1 )  ! halo + land + nbghostcells + sponge - 1
         iend   = jpiglo - nn_hls - 1                            ! halo + land + 1                     - 1
         jstart = nn_hls + 2 
         jend   = jpjglo - nn_hls - 1
         DO ji = mi0(istart), mi1(iend)
            DO jj = mj0(jstart), mj1(jend)
               IF ( ABS(ht0_parent(ji,jj)-ht_0(ji,jj)) > 1.e-3 ) iindic = iindic + 1
               IF ( .NOT.ln_vert_remap) THEN
                  DO jk = 1, jpkm1
                     IF ( ABS(e3t0_parent(ji,jj,jk)-e3t_0(ji,jj,jk))*tmask(ji,jj,jk) > 1.e-3 ) iindic = iindic + 1
                  END DO 
               ENDIF
            END DO
            DO jj = mj0(jstart), mj1(jend-1)
               IF ( ABS(hv0_parent(ji,jj)-hv_0(ji,jj)) > 1.e-3 ) iindic = iindic + 1
               IF ( .NOT.ln_vert_remap) THEN
                  DO jk = 1, jpkm1
                     IF ( ABS(e3v0_parent(ji,jj,jk)-e3v_0(ji,jj,jk))*vmask(ji,jj,jk) > 1.e-3 ) iindic = iindic + 1
                  END DO 
               ENDIF
            END DO
         END DO
         DO ji = mi0(istart), mi1(iend-1)
            DO jj = mj0(jstart), mj1(jend)
               IF ( ABS(hu0_parent(ji,jj)-hu_0(ji,jj)) > 1.e-3 ) iindic = iindic + 1
               IF ( .NOT.ln_vert_remap) THEN
                  DO jk = 1, jpkm1
                     IF ( ABS(e3u0_parent(ji,jj,jk)-e3u_0(ji,jj,jk))*umask(ji,jj,jk) > 1.e-3 ) iindic = iindic + 1
                  END DO 
               ENDIF
            END DO
         END DO
      ENDIF
      !
      ! --- South --- !
      IF(lk_south) THEN
         ispon  = (nn_sponge_len+2) * Agrif_irhoy()  
         jstart = nn_hls + 2                                 ! halo + land + 1
         jend   = nn_hls + nbghostcells + ispon          ! halo + land + nbghostcells + sponge
         istart = nn_hls + 2 
         iend   = jpiglo - nn_hls - 1 
         DO jj = mj0(jstart), mj1(jend)
            DO ji = mi0(istart), mi1(iend)
               IF ( ABS(ht0_parent(ji,jj)-ht_0(ji,jj)) > 1.e-3 ) iindic = iindic + 1
               IF ( .NOT.ln_vert_remap) THEN
                  DO jk = 1, jpkm1
                     IF ( ABS(e3t0_parent(ji,jj,jk)-e3t_0(ji,jj,jk))*tmask(ji,jj,jk) > 1.e-3 ) iindic = iindic + 1
                  END DO 
               ENDIF
            END DO
            DO ji = mi0(istart), mi1(iend-1)
               IF ( ABS(hu0_parent(ji,jj)-hu_0(ji,jj)) > 1.e-3 ) iindic = iindic + 1
               IF ( .NOT.ln_vert_remap) THEN
                  DO jk = 1, jpkm1
                     IF ( ABS(e3u0_parent(ji,jj,jk)-e3u_0(ji,jj,jk))*umask(ji,jj,jk) > 1.e-3 ) iindic = iindic + 1
                  END DO 
               ENDIF
            END DO
         END DO
         DO jj = mj0(jstart), mj1(jend-1)
            DO ji = mi0(istart), mi1(iend)
               IF ( ABS(hv0_parent(ji,jj)-hv_0(ji,jj)) > 1.e-3 ) iindic = iindic + 1
               IF ( .NOT.ln_vert_remap) THEN
                  DO jk = 1, jpkm1
                     IF ( ABS(e3v0_parent(ji,jj,jk)-e3v_0(ji,jj,jk))*vmask(ji,jj,jk) > 1.e-3 ) iindic = iindic + 1
                  END DO 
               ENDIF
            END DO
         END DO
      ENDIF
      !
      ! --- North --- !
      IF(lk_north) THEN
         ispon  = (nn_sponge_len+2) * Agrif_irhoy() 
         jstart = jpjglo - ( nn_hls + nbghostcells + ispon - 1)  ! halo + land + nbghostcells +sponge - 1
         jend   = jpjglo - nn_hls - 1                            ! halo + land + 1            - 1
         istart = nn_hls + 2 
         iend   = jpiglo - nn_hls - 1 
         DO jj = mj0(jstart), mj1(jend)
            DO ji = mi0(istart), mi1(iend)
               IF ( ABS(ht0_parent(ji,jj)-ht_0(ji,jj)) > 1.e-3 ) iindic = iindic + 1
               IF ( .NOT.ln_vert_remap) THEN
                  DO jk = 1, jpkm1
                     IF ( ABS(e3t0_parent(ji,jj,jk)-e3t_0(ji,jj,jk))*tmask(ji,jj,jk) > 1.e-3 ) iindic = iindic + 1
                  END DO 
               ENDIF
            END DO
            DO ji = mi0(istart), mi1(iend-1)
               IF ( ABS(hu0_parent(ji,jj)-hu_0(ji,jj)) > 1.e-3 ) iindic = iindic + 1
               IF ( .NOT.ln_vert_remap) THEN
                  DO jk = 1, jpkm1
                     IF ( ABS(e3u0_parent(ji,jj,jk)-e3u_0(ji,jj,jk))*umask(ji,jj,jk) > 1.e-3 ) iindic = iindic + 1
                  END DO 
               ENDIF
            END DO
         END DO
         DO jj = mj0(jstart), mj1(jend-1)
            DO ji = mi0(istart), mi1(iend)
               IF ( ABS(hv0_parent(ji,jj)-hv_0(ji,jj)) > 1.e-3 ) iindic = iindic + 1
               IF ( .NOT.ln_vert_remap) THEN
                  DO jk = 1, jpkm1
                     IF ( ABS(e3v0_parent(ji,jj,jk)-e3v_0(ji,jj,jk))*vmask(ji,jj,jk) > 1.e-3 ) iindic = iindic + 1
                  END DO 
               ENDIF
            END DO
         END DO
      ENDIF
      !
   END SUBROUTINE Agrif_check_bat
   
#else
   !!----------------------------------------------------------------------
   !!   Empty module                                          no AGRIF zoom
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE Agrif_OCE_Interp_empty
      WRITE(*,*)  'agrif_oce_interp : You should not have seen this print! error?'
   END SUBROUTINE Agrif_OCE_Interp_empty
#endif

   !!======================================================================
END MODULE agrif_oce_interp
