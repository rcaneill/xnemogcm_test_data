#undef UPD_HIGH   /* MIX HIGH UPDATE */
#if defined key_agrif
   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/NST 4.0 , NEMO Consortium (2018)
   !! $Id: agrif_user.F90 15265 2021-09-16 11:13:13Z jchanut $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
   SUBROUTINE agrif_user
   END SUBROUTINE agrif_user

   
   SUBROUTINE agrif_before_regridding
   END SUBROUTINE agrif_before_regridding

   
   SUBROUTINE Agrif_InitWorkspace
   END SUBROUTINE Agrif_InitWorkspace

   
   SUBROUTINE Agrif_InitValues
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_InitValues ***
      !!----------------------------------------------------------------------
      USE nemogcm
      !!----------------------------------------------------------------------
      !
      CALL nemo_init       !* Initializations of each fine grid
      Kbb_a = Nbb; Kmm_a = Nnn; Krhs_a = Nrhs   ! agrif_oce module copies of time level indices
      !
      !                    !* Agrif initialization
      CALL Agrif_InitValues_cont
# if defined key_top
      CALL Agrif_InitValues_cont_top
# endif
# if defined key_si3
      CALL Agrif_InitValues_cont_ice
# endif
      !    
   END SUBROUTINE Agrif_initvalues

   
   SUBROUTINE agrif_declare_var_ini
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE agrif_declare_var_ini ***
      !!----------------------------------------------------------------------
      USE agrif_util
      USE agrif_oce
      USE par_oce
      USE zdf_oce 
      USE oce
      USE dom_oce
      !
      IMPLICIT NONE
      !
      INTEGER :: ind1, ind2, ind3, imaxrho
      INTEGER :: nbghostcellsfine_tot_x, nbghostcellsfine_tot_y
      INTEGER :: its
      External :: nemo_mapping
      !!----------------------------------------------------------------------

! In case of East-West periodicity, prevent AGRIF interpolation at east and west boundaries
! The procnames will not be called at these boundaries
      IF ( .NOT. lk_west ) THEN
         CALL Agrif_Set_NearCommonBorderX(.TRUE.)
      ENDIF

      IF ( .NOT. lk_east ) THEN
         CALL Agrif_Set_DistantCommonBorderX(.TRUE.)
      ENDIF

      IF ( .NOT. lk_south ) THEN
         CALL Agrif_Set_NearCommonBorderY(.TRUE.)
      ENDIF

      IF ( .NOT. lk_north ) THEN
         CALL Agrif_Set_DistantCommonBorderY(.TRUE.)
      ENDIF

      ! 1. Declaration of the type of variable which have to be interpolated
      !---------------------------------------------------------------------
!      ind1 =              nbghostcells 
      ind2 = nn_hls + 1 + nbghostcells_x_w
      ind3 = nn_hls + 1 + nbghostcells_y_s
      nbghostcellsfine_tot_x = MAX(nbghostcells_x_w,nbghostcells_x_e)+1
      nbghostcellsfine_tot_y = MAX(nbghostcells_y_s,nbghostcells_y_n)+1
      ind1 = MAX(nbghostcellsfine_tot_x, nbghostcellsfine_tot_y)
      imaxrho = MAX(Agrif_irhox(), Agrif_irhoy())

      CALL agrif_declare_variable((/2,2,0  /),(/ind2  ,ind3,  0  /),(/'x','y','N'    /),(/1,1,1  /),(/jpi,jpj,jpk    /),        e3t_id)
      CALL agrif_declare_variable((/1,2,0  /),(/ind2-1,ind3,  0  /),(/'x','y','N'    /),(/1,1,1  /),(/jpi,jpj,jpk    /),        e3u_id)
      CALL agrif_declare_variable((/2,1,0  /),(/ind2  ,ind3-1,0  /),(/'x','y','N'    /),(/1,1,1  /),(/jpi,jpj,jpk    /),        e3v_id)
      CALL agrif_declare_variable((/1,1,0  /),(/ind2-1,ind3-1,0  /),(/'x','y','N'    /),(/1,1,1  /),(/jpi,jpj,jpk    /),        e3f_id)
#if defined key_qco
      CALL agrif_declare_variable((/2,2    /),(/ind2  ,ind3      /),(/'x','y'        /),(/1,1    /),(/jpi,jpj        /),        r3t_id)
      CALL agrif_declare_variable((/1,2    /),(/ind2-1,ind3      /),(/'x','y'        /),(/1,1    /),(/jpi,jpj        /),        r3u_id)
      CALL agrif_declare_variable((/2,1    /),(/ind2  ,ind3-1    /),(/'x','y'        /),(/1,1    /),(/jpi,jpj        /),        r3v_id)
      CALL agrif_declare_variable((/1,1    /),(/ind2-1,ind3-1    /),(/'x','y'        /),(/1,1    /),(/jpi,jpj        /),        r3f_id)
#endif
      CALL agrif_declare_variable((/2,2,0  /),(/ind2  ,ind3  ,0  /),(/'x','y','N'    /),(/1,1,1  /),(/jpi,jpj,jpk    /),e3t0_interp_id)
      CALL agrif_declare_variable((/2,2    /),(/ind2  ,ind3      /),(/'x','y'        /),(/1,1    /),(/jpi,jpj        /),       mbkt_id)
      CALL agrif_declare_variable((/2,2    /),(/ind2  ,ind3      /),(/'x','y'        /),(/1,1    /),(/jpi,jpj        /),        ht0_id)
      CALL agrif_declare_variable((/2,2    /),(/ind2  ,ind3      /),(/'x','y'        /),(/1,1    /),(/jpi,jpj        /), e1e2t_frac_id)   
      CALL agrif_declare_variable((/1,2    /),(/ind2-1,ind3      /),(/'x','y'        /),(/1,1    /),(/jpi,jpj        /),   e2u_frac_id)   
      CALL agrif_declare_variable((/2,1    /),(/ind2  ,ind3-1    /),(/'x','y'        /),(/1,1    /),(/jpi,jpj        /),   e1v_frac_id)   

      ! Initial or restart velues
      its = jpts+1
      CALL agrif_declare_variable((/2,2,0,0/),(/ind2  ,ind3  ,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/jpi,jpj,jpk,its/), tsini_id)
      CALL agrif_declare_variable((/1,2,0,0/),(/ind2-1,ind3  ,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/jpi,jpj,jpk,2  /),  uini_id) 
      CALL agrif_declare_variable((/2,1,0,0/),(/ind2  ,ind3-1,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/jpi,jpj,jpk,2  /),  vini_id)
      CALL agrif_declare_variable((/2,2    /),(/ind2  ,ind3      /),(/'x','y'        /),(/1,1    /),(/jpi,jpj        /),sshini_id)
      ! 
     
      ! 2. Type of interpolation
      !-------------------------
      CALL Agrif_Set_bcinterp(        e3t_id,interp =AGRIF_constant)
      CALL Agrif_Set_bcinterp(        e3u_id,interp =AGRIF_constant)
      CALL Agrif_Set_bcinterp(        e3v_id,interp =AGRIF_constant)
      CALL Agrif_Set_bcinterp(        e3f_id,interp =AGRIF_constant)
#if defined key_qco
      CALL Agrif_Set_bcinterp(        r3t_id,interp =AGRIF_constant)
      CALL Agrif_Set_bcinterp(        r3u_id,interp =AGRIF_constant)
      CALL Agrif_Set_bcinterp(        r3v_id,interp =AGRIF_constant)
      CALL Agrif_Set_bcinterp(        r3f_id,interp =AGRIF_constant)
#endif
      CALL Agrif_Set_bcinterp(e3t0_interp_id,interp =AGRIF_linear  )
      CALL Agrif_Set_interp  (e3t0_interp_id,interp =AGRIF_linear  )
      CALL Agrif_Set_bcinterp(       mbkt_id,interp =AGRIF_constant)
      CALL Agrif_Set_interp  (       mbkt_id,interp =AGRIF_constant)
      CALL Agrif_Set_bcinterp(        ht0_id,interp =AGRIF_constant)
      CALL Agrif_Set_interp  (        ht0_id,interp =AGRIF_constant)
      CALL Agrif_Set_bcinterp( e1e2t_frac_id,interp =AGRIF_constant)
      CALL Agrif_Set_interp  ( e1e2t_frac_id,interp =AGRIF_constant)
      CALL Agrif_Set_bcinterp(   e2u_frac_id,interp =AGRIF_constant)
      CALL Agrif_Set_interp  (   e2u_frac_id,interp =AGRIF_constant)
      CALL Agrif_Set_bcinterp(   e1v_frac_id,interp =AGRIF_constant)
      CALL Agrif_Set_interp  (   e1v_frac_id,interp =AGRIF_constant)


      ! Initial fields
      CALL Agrif_Set_bcinterp( tsini_id,interp =AGRIF_linear  )
      CALL Agrif_Set_interp  ( tsini_id,interp =AGRIF_linear  )
      CALL Agrif_Set_bcinterp(  uini_id,interp =AGRIF_linear  )
      CALL Agrif_Set_interp  (  uini_id,interp =AGRIF_linear  )
      CALL Agrif_Set_bcinterp(  vini_id,interp =AGRIF_linear  )
      CALL Agrif_Set_interp  (  vini_id,interp =AGRIF_linear  )
      IF ( lk_div_cons ) THEN
         CALL Agrif_Set_bcinterp(sshini_id,interp =AGRIF_constant)
      ELSE
         CALL Agrif_Set_bcinterp(sshini_id,interp =AGRIF_linear  )
      ENDIF
      CALL Agrif_Set_interp  (sshini_id,interp =AGRIF_linear  )

       ! 3. Location of interpolation
      !-----------------------------
      CALL Agrif_Set_bc(  e3t_id, (/-nn_sponge_len*imaxrho-2,ind1-1/) )  
      CALL Agrif_Set_bc(  e3u_id, (/-nn_sponge_len*imaxrho-2,ind1-1/) )  
      CALL Agrif_Set_bc(  e3v_id, (/-nn_sponge_len*imaxrho-2,ind1-1/) )  
      CALL Agrif_Set_bc(  e3f_id, (/-nn_sponge_len*imaxrho-2,ind1-1/) )  
#if defined key_qco
      CALL Agrif_Set_bc(  r3t_id, (/-nn_sponge_len*imaxrho-2,ind1-1/) )  
      CALL Agrif_Set_bc(  r3u_id, (/-nn_sponge_len*imaxrho-2,ind1-1/) )  
      CALL Agrif_Set_bc(  r3v_id, (/-nn_sponge_len*imaxrho-2,ind1-1/) )  
      CALL Agrif_Set_bc(  r3f_id, (/-nn_sponge_len*imaxrho-2,ind1-1/) )  
#endif      
      ! extend the interpolation zone by 1 more point than necessary:
      ! RB check here
      CALL Agrif_Set_bc( e3t0_interp_id, (/-nn_sponge_len*imaxrho-2,ind1-1/) )
      CALL Agrif_Set_bc(        mbkt_id, (/-nn_sponge_len*imaxrho-2,ind1-1/) )
      CALL Agrif_Set_bc(         ht0_id, (/-nn_sponge_len*imaxrho-2,ind1-1/) )
      CALL Agrif_Set_bc(  e1e2t_frac_id, (/-nn_sponge_len*imaxrho-2,ind1-1/) )
      CALL Agrif_Set_bc(    e2u_frac_id, (/-nn_sponge_len*imaxrho-2,ind1-1/) )
      CALL Agrif_Set_bc(    e1v_frac_id, (/-nn_sponge_len*imaxrho-2,ind1-1/) )

      CALL Agrif_Set_bc(       tsini_id, (/0,ind1-1/) ) ! if west,  rhox=3 and nbghost=3: columns 2 to 4
      CALL Agrif_Set_bc(        uini_id, (/0,ind1-1/) ) 
      CALL Agrif_Set_bc(        vini_id, (/0,ind1-1/) )
      CALL Agrif_Set_bc(      sshini_id, (/-imaxrho*nn_shift_bar,ind1-1/) )

      ! 4. Update type
      !--------------- 
# if defined UPD_HIGH
      CALL Agrif_Set_Updatetype(e3t0_interp_id, update = Agrif_Update_Full_Weighting)
#if defined key_qco
      CALL Agrif_Set_Updatetype(        r3t_id,update  = Agrif_Update_Full_Weighting)
      CALL Agrif_Set_Updatetype(        r3u_id,update1 = Agrif_Update_Average       , update2 = Agrif_Update_Full_Weighting)
      CALL Agrif_Set_Updatetype(        r3v_id,update1 = Agrif_Update_Full_Weighting, update2 = Agrif_Update_Average       )
      CALL Agrif_Set_Updatetype(        r3f_id,update  = Agrif_Update_Copy          )
#endif
#else
      CALL Agrif_Set_Updatetype(e3t0_interp_id, update = Agrif_Update_Average)
#if defined key_qco
      CALL Agrif_Set_Updatetype(        r3t_id,update  = AGRIF_Update_Average)
      CALL Agrif_Set_Updatetype(        r3u_id,update1 = Agrif_Update_Copy   , update2 = Agrif_Update_Average)
      CALL Agrif_Set_Updatetype(        r3v_id,update1 = Agrif_Update_Average, update2 = Agrif_Update_Copy   )
      CALL Agrif_Set_Updatetype(        r3f_id,update  = Agrif_Update_Copy   )
#endif 
#endif      

      CALL Agrif_Set_ExternalMapping(nemo_mapping)
      !
   END SUBROUTINE agrif_declare_var_ini


   SUBROUTINE Agrif_Init_Domain
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_Init_Domain ***
      !!----------------------------------------------------------------------
      USE agrif_oce_update
      USE agrif_oce_interp
      USE agrif_oce_sponge
      USE Agrif_Util
      USE oce 
      USE dom_oce
      USE zdf_oce
      USE nemogcm
      USE agrif_oce
      !
      USE lbclnk
      USE lib_mpp
      USE in_out_manager
      !
      IMPLICIT NONE
      !
      !
      LOGICAL :: check_namelist
      CHARACTER(len=15) :: cl_check1, cl_check2, cl_check3, cl_check4 
      REAL(wp), DIMENSION(jpi,jpj) ::   zk   ! workspace
      INTEGER :: ji, jj, jk
      INTEGER :: jpk_parent, ierr
      !!----------------------------------------------------------------------
    
     ! CALL Agrif_Declare_Var_ini

      IF( agrif_oce_alloc()  > 0 )   CALL ctl_warn('agrif agrif_oce_alloc: allocation of arrays failed')

      ! Build consistent parent bathymetry and number of levels
      ! on the child grid 
      Agrif_UseSpecialValue = .TRUE.
      ht0_parent( :,:) = 0._wp
      mbkt_parent(:,:) = 0
      !
!     CALL Agrif_Bc_variable(ht0_id ,calledweight=1.,procname=interpht0 )
!     CALL Agrif_Bc_variable(mbkt_id,calledweight=1.,procname=interpmbkt)
      CALL Agrif_Init_Variable(ht0_id,        procname=interpht0 )
      CALL Agrif_Init_Variable(mbkt_id,       procname=interpmbkt)
      CALL Agrif_Init_variable(e1e2t_frac_id, procname=interp_e1e2t_frac) 
      CALL Agrif_Init_variable(  e2u_frac_id, procname=interp_e2u_frac) 
      CALL Agrif_Init_variable(  e1v_frac_id, procname=interp_e1v_frac) 
      !
      ! Assume step wise change of bathymetry near interface
      ! TODO: Switch to linear interpolation of bathymetry in the s-coordinate case
      !       and no refinement
      DO_2D( 1, 0, 1, 0 )
         mbku_parent(ji,jj) = MIN( mbkt_parent(ji+1,jj  ), mbkt_parent(ji,jj) )
         mbkv_parent(ji,jj) = MIN( mbkt_parent(ji  ,jj+1), mbkt_parent(ji,jj) )
      END_2D
      IF ( ln_sco.AND.Agrif_Parent(ln_sco) ) THEN 
         DO_2D( 1, 0, 1, 0 )
            hu0_parent(ji,jj) = 0.5_wp * ( ht0_parent(ji,jj)+ht0_parent(ji+1,jj) ) * ssumask(ji,jj)
            hv0_parent(ji,jj) = 0.5_wp * ( ht0_parent(ji,jj)+ht0_parent(ji,jj+1) ) * ssvmask(ji,jj)
         END_2D
      ELSE
         DO_2D( 1, 0, 1, 0 )
            hu0_parent(ji,jj) = MIN( ht0_parent(ji,jj), ht0_parent(ji+1,jj) )
            hv0_parent(ji,jj) = MIN( ht0_parent(ji,jj), ht0_parent(ji,jj+1) )
         END_2D
      ENDIF
      !
      CALL lbc_lnk( 'Agrif_Init_Domain', hu0_parent, 'U', 1.0_wp, hv0_parent, 'V', 1.0_wp )
      DO_2D( 0, 0, 0, 0 )
         zk(ji,jj) = REAL( mbku_parent(ji,jj), wp )
      END_2D
      CALL lbc_lnk( 'Agrif_InitValues_Domain', zk, 'U', 1.0_wp )
      mbku_parent(:,:) = MAX( NINT( zk(:,:) ), 1 )
      DO_2D( 0, 0, 0, 0 )
         zk(ji,jj) = REAL( mbkv_parent(ji,jj), wp )
      END_2D
      CALL lbc_lnk( 'Agrif_InitValues_Domain', zk, 'V', 1.0_wp )
      mbkv_parent(:,:) = MAX( NINT( zk(:,:) ), 1 )  
      !
      ! Build "intermediate" parent vertical grid on child domain
      jpk_parent = Agrif_parent( jpk )
      ALLOCATE(e3t0_parent(jpi,jpj,jpk_parent), &
         &     e3u0_parent(jpi,jpj,jpk_parent), &
         &     e3v0_parent(jpi,jpj,jpk_parent), STAT = ierr) 
      IF( ierr  > 0 )   CALL ctl_warn('Agrif_Init_Domain: allocation of arrays failed')
       
      ! Retrieve expected parent scale factors on child grid:
      Agrif_UseSpecialValue = .FALSE.
      e3t0_parent(:,:,:) = 0._wp
      CALL Agrif_Init_Variable(e3t0_interp_id, procname=interpe3t0_vremap)
      !
      ! Deduce scale factors at U and V points:
      DO_3D( 0, 0, 0, 0, 1, jpk_parent )
         e3u0_parent(ji,jj,jk) = 0.5_wp * (e3t0_parent(ji,jj,jk) + e3t0_parent(ji+1,jj  ,jk))
         e3v0_parent(ji,jj,jk) = 0.5_wp * (e3t0_parent(ji,jj,jk) + e3t0_parent(ji  ,jj+1,jk))
      END_3D

      ! Assume a step at the bottom except if (pure) s-coordinates
      IF ( .NOT.Agrif_Parent(ln_sco) ) THEN 
         DO_2D( 1, 0, 1, 0 )
            jk = mbku_parent(ji,jj)
            e3u0_parent(ji,jj,jk) = MIN(e3t0_parent(ji,jj,jk), e3t0_parent(ji+1,jj  ,jk))
            jk = mbkv_parent(ji,jj)
            e3v0_parent(ji,jj,jk) = MIN(e3t0_parent(ji,jj,jk), e3t0_parent(ji  ,jj+1,jk))
         END_2D
      ENDIF

      CALL lbc_lnk( 'Agrif_Init_Domain', e3u0_parent, 'U', 1.0_wp, e3v0_parent, 'V', 1.0_wp )

      ! check if masks and bathymetries match
      IF(ln_chk_bathy) THEN
         Agrif_UseSpecialValue = .FALSE.
         !
         IF(lwp) WRITE(numout,*) ' '
         IF(lwp) WRITE(numout,*) 'AGRIF: Check Bathymetry and masks near bdys. Level: ', Agrif_Level()
         !
         kindic_agr = 0
         !         
         CALL Agrif_check_bat( kindic_agr )           
         !
         CALL mpp_sum( 'agrif_InitValues_Domain', kindic_agr )
         IF( kindic_agr /= 0 ) THEN
            CALL ctl_stop('==> Child Bathymetry is NOT correct near boundaries.')
         ELSE
            IF(lwp) WRITE(numout,*) '==> Child Bathymetry is ok near boundaries.'
            IF(lwp) WRITE(numout,*) ' '
         ENDIF  
      ENDIF
      !
      WHERE (ssumask(:,:) == 0._wp) mbku_parent(:,:) = 0
      WHERE (ssvmask(:,:) == 0._wp) mbkv_parent(:,:) = 0
      WHERE (ssmask(:,:)  == 0._wp) mbkt_parent(:,:) = 0
      !
      IF ( .NOT.ln_vert_remap ) DEALLOCATE(e3t0_parent, e3u0_parent, e3v0_parent)

   END SUBROUTINE Agrif_Init_Domain


   SUBROUTINE Agrif_InitValues_cont
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_InitValues_cont ***
      !!
      !! ** Purpose ::   Declaration of variables to be interpolated
      !!----------------------------------------------------------------------
      USE agrif_oce_update
      USE agrif_oce_interp
      USE agrif_oce_sponge
      USE Agrif_Util
      USE oce 
      USE dom_oce
      USE zdf_oce
      USE nemogcm
      USE agrif_oce
      !
      USE lbclnk
      USE lib_mpp
      USE in_out_manager
      !
      IMPLICIT NONE
      !
      LOGICAL :: check_namelist
      CHARACTER(len=15) :: cl_check1, cl_check2, cl_check3, cl_check4 

      ! 1. Declaration of the type of variable which have to be interpolated
      !---------------------------------------------------------------------
      CALL agrif_declare_var

      ! 2. First interpolations of potentially non zero fields
      !-------------------------------------------------------
      Agrif_SpecialValue    = 0._wp
      Agrif_UseSpecialValue = l_spc_tra 
      l_vremap              = ln_vert_remap
      CALL Agrif_Bc_variable(ts_interp_id,calledweight=1.,procname=interptsn)
      CALL Agrif_Sponge
      tabspongedone_tsn = .FALSE.
      CALL Agrif_Bc_variable(ts_sponge_id,calledweight=1.,procname=interptsn_sponge)
      ! reset tsa to zero
      ts(:,:,:,:,Krhs_a) = 0._wp

      Agrif_UseSpecialValue = ln_spc_dyn
      use_sign_north = .TRUE.
      sign_north = -1.
      CALL Agrif_Bc_variable(un_interp_id,calledweight=1.,procname=interpun)
      CALL Agrif_Bc_variable(vn_interp_id,calledweight=1.,procname=interpvn)
      tabspongedone_u = .FALSE.
      tabspongedone_v = .FALSE.
      CALL Agrif_Bc_variable(un_sponge_id,calledweight=1.,procname=interpun_sponge)
      tabspongedone_u = .FALSE.
      tabspongedone_v = .FALSE.
      CALL Agrif_Bc_variable(vn_sponge_id,calledweight=1.,procname=interpvn_sponge)
      IF (nn_shift_bar>0) THEN
         CALL Agrif_Sponge_2d
         tabspongedone_u = .FALSE.
         tabspongedone_v = .FALSE.
         CALL Agrif_Bc_variable(unb_sponge_id,calledweight=1.,procname=interpunb_sponge)
         tabspongedone_u = .FALSE.
         tabspongedone_v = .FALSE.
         CALL Agrif_Bc_variable(vnb_sponge_id,calledweight=1.,procname=interpvnb_sponge)
      ENDIF
      use_sign_north = .FALSE.
      uu(:,:,:,Krhs_a) = 0._wp
      vv(:,:,:,Krhs_a) = 0._wp

      Agrif_UseSpecialValue = l_spc_ssh 
      CALL Agrif_Bc_variable(sshn_id,calledweight=1., procname=interpsshn )
      hbdy(:,:) = 0._wp
      ssh(:,:,Krhs_a) = 0._wp

      IF ( ln_dynspg_ts ) THEN
         Agrif_UseSpecialValue = ln_spc_dyn
         use_sign_north = .TRUE.
         sign_north = -1.
         CALL Agrif_Bc_variable(ub2b_interp_id,calledweight=1.,procname=interpub2b)   ! must be called before unb_id to define ubdy
         CALL Agrif_Bc_variable(vb2b_interp_id,calledweight=1.,procname=interpvb2b)   ! must be called before vnb_id to define vbdy
         CALL Agrif_Bc_variable( unb_interp_id,calledweight=1.,procname=interpunb )
         CALL Agrif_Bc_variable( vnb_interp_id,calledweight=1.,procname=interpvnb )
         use_sign_north = .FALSE.
         ubdy(:,:) = 0._wp
         vbdy(:,:) = 0._wp
      ELSEIF ( ln_dynspg_EXP ) THEN 
         Agrif_UseSpecialValue = ln_spc_dyn
         use_sign_north = .TRUE.
         sign_north = -1.
         ubdy(:,:) = 0._wp
         vbdy(:,:) = 0._wp
         CALL Agrif_Bc_variable( unb_interp_id,calledweight=1.,procname=interpunb )
         CALL Agrif_Bc_variable( vnb_interp_id,calledweight=1.,procname=interpvnb )
         use_sign_north = .FALSE.
         ubdy(:,:) = 0._wp
         vbdy(:,:) = 0._wp
      ENDIF
      Agrif_UseSpecialValue = .FALSE. 
      l_vremap              = .FALSE.

      !-----------------
      check_namelist = .TRUE.

      IF( check_namelist ) THEN 
         ! Check free surface scheme
         IF ( ( Agrif_Parent(ln_dynspg_ts ).AND.ln_dynspg_exp ).OR.&
            & ( Agrif_Parent(ln_dynspg_exp).AND.ln_dynspg_ts ) ) THEN
            WRITE(cl_check1,*)  Agrif_Parent( ln_dynspg_ts )
            WRITE(cl_check2,*)  ln_dynspg_ts
            WRITE(cl_check3,*)  Agrif_Parent( ln_dynspg_exp )
            WRITE(cl_check4,*)  ln_dynspg_exp
            CALL ctl_stop( 'Incompatible free surface scheme between grids' ,  &
                  &               'parent grid ln_dynspg_ts  :'//cl_check1  ,  & 
                  &               'child  grid ln_dynspg_ts  :'//cl_check2  ,  &
                  &               'parent grid ln_dynspg_exp :'//cl_check3  ,  &
                  &               'child  grid ln_dynspg_exp :'//cl_check4  ,  &
                  &               'those logicals should be identical' )                 
            STOP
         ENDIF

         ! Check if identical linear free surface option
         IF ( ( Agrif_Parent(ln_linssh ).AND.(.NOT.ln_linssh )).OR.&
            & ( (.NOT.Agrif_Parent(ln_linssh)).AND.ln_linssh ) ) THEN
            WRITE(cl_check1,*)  Agrif_Parent(ln_linssh )
            WRITE(cl_check2,*)  ln_linssh
            CALL ctl_stop( 'Incompatible linearized fs option between grids',  &
                  &               'parent grid ln_linssh  :'//cl_check1     ,  &
                  &               'child  grid ln_linssh  :'//cl_check2     ,  &
                  &               'those logicals should be identical' )                  
            STOP
         ENDIF
      ENDIF

   END SUBROUTINE Agrif_InitValues_cont

   SUBROUTINE agrif_declare_var
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE agrif_declare_var ***
      !!----------------------------------------------------------------------
      USE agrif_util
      USE agrif_oce
      USE par_oce
      USE zdf_oce 
      USE oce
      !
      IMPLICIT NONE
      !
      INTEGER :: ind1, ind2, ind3, imaxrho
      !!----------------------------------------------------------------------

      ! 1. Declaration of the type of variable which have to be interpolated
      !---------------------------------------------------------------------
      ind1 =              nbghostcells - 1 ! Remove one land cell in ghosts 
      ind2 = nn_hls + 1 + nbghostcells_x_w
      ind3 = nn_hls + 1 + nbghostcells_y_s
      imaxrho = MAX(Agrif_irhox(), Agrif_irhoy())

      CALL agrif_declare_variable((/2,2,0,0/),(/ind2,ind3,0,0/)  ,(/'x','y','N','N'/),(/1,1,1,1/),(/jpi,jpj,jpk,jpts+1/),ts_interp_id)
      CALL agrif_declare_variable((/2,2,0,0/),(/ind2,ind3,0,0/)  ,(/'x','y','N','N'/),(/1,1,1,1/),(/jpi,jpj,jpk,jpts+1/),ts_update_id)
      CALL agrif_declare_variable((/2,2,0,0/),(/ind2,ind3,0,0/)  ,(/'x','y','N','N'/),(/1,1,1,1/),(/jpi,jpj,jpk,jpts+1/),ts_sponge_id)
      CALL agrif_declare_variable((/1,2,0,0/),(/ind2-1,ind3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/jpi,jpj,jpk,2/),un_interp_id)
      CALL agrif_declare_variable((/2,1,0,0/),(/ind2,ind3-1,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/jpi,jpj,jpk,2/),vn_interp_id)
      CALL agrif_declare_variable((/1,2,0,0/),(/ind2-1,ind3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/jpi,jpj,jpk,2/),un_update_id)
      CALL agrif_declare_variable((/2,1,0,0/),(/ind2,ind3-1,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/jpi,jpj,jpk,2/),vn_update_id)
      CALL agrif_declare_variable((/1,2,0,0/),(/ind2-1,ind3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/jpi,jpj,jpk,2/),un_sponge_id)
      CALL agrif_declare_variable((/2,1,0,0/),(/ind2,ind3-1,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/jpi,jpj,jpk,2/),vn_sponge_id)

      CALL agrif_declare_variable((/2,2/),(/ind2,ind3/),(/'x','y'/),(/1,1/),(/jpi,jpj/)  ,sshn_id)
      CALL agrif_declare_variable((/2,2/),(/ind2,ind3/),(/'x','y'/),(/1,1/),(/jpi,jpj/)  ,sshn_frc_id)
      CALL agrif_declare_variable((/1,2/),(/ind2-1,ind3/),(/'x','y'/),(/1,1/),(/jpi,jpj/), unb_interp_id)
      CALL agrif_declare_variable((/2,1/),(/ind2,ind3-1/),(/'x','y'/),(/1,1/),(/jpi,jpj/), vnb_interp_id)
      CALL agrif_declare_variable((/1,2/),(/ind2-1,ind3/),(/'x','y'/),(/1,1/),(/jpi,jpj/),ub2b_interp_id)
      CALL agrif_declare_variable((/2,1/),(/ind2,ind3-1/),(/'x','y'/),(/1,1/),(/jpi,jpj/),vb2b_interp_id)
      CALL agrif_declare_variable((/1,2/),(/ind2-1,ind3/),(/'x','y'/),(/1,1/),(/jpi,jpj/), unb_sponge_id)
      CALL agrif_declare_variable((/2,1/),(/ind2,ind3-1/),(/'x','y'/),(/1,1/),(/jpi,jpj/), vnb_sponge_id)
      CALL agrif_declare_variable((/1,2/),(/ind2-1,ind3/),(/'x','y'/),(/1,1/),(/jpi,jpj/),ub2b_update_id)
      CALL agrif_declare_variable((/2,1/),(/ind2,ind3-1/),(/'x','y'/),(/1,1/),(/jpi,jpj/),vb2b_update_id)
      CALL agrif_declare_variable((/1,2/),(/ind2-1,ind3/),(/'x','y'/),(/1,1/),(/jpi,jpj/), unb_update_id)
      CALL agrif_declare_variable((/2,1/),(/ind2,ind3-1/),(/'x','y'/),(/1,1/),(/jpi,jpj/), vnb_update_id)
      CALL agrif_declare_variable((/2,2/),(/ind2,ind3/),(/'x','y'/),(/1,1/),(/jpi,jpj/)  ,ub2b_cor_id)
      CALL agrif_declare_variable((/2,2/),(/ind2,ind3/),(/'x','y'/),(/1,1/),(/jpi,jpj/)  ,vb2b_cor_id)
!      CALL agrif_declare_variable((/2,2/),(/ind2,ind3/),(/'x','y'/),(/1,1/),(/jpi,jpj/),glamt_id)
!      CALL agrif_declare_variable((/2,2/),(/ind2,ind3/),(/'x','y'/),(/1,1/),(/jpi,jpj/),gphit_id)


      IF( ln_zdftke.OR.ln_zdfgls ) THEN  ! logical not known at this point
!         CALL agrif_declare_variable((/2,2,0/),(/ind3,ind3,0/),(/'x','y','N'/),(/1,1,1/),(/jpi,jpj,jpk/), en_id)
!         CALL agrif_declare_variable((/2,2,0/),(/ind3,ind3,0/),(/'x','y','N'/),(/1,1,1/),(/jpi,jpj,jpk/),avt_id)
         CALL agrif_declare_variable((/2,2,0,0/),(/ind2,ind3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/jpi,jpj,jpk,2/),avm_id)
      ENDIF
     
      ! 2. Type of interpolation
      !-------------------------
      l_spc_tra = .TRUE. ! No extrapolation
      CALL Agrif_Set_bcinterp( ts_interp_id,interp =AGRIF_constant)
      CALL Agrif_Set_bcinterp( ts_sponge_id,interp =AGRIF_constant)
!      l_spc_tra = .TRUE. ! Use extrapolation
!      CALL Agrif_Set_bcinterp( ts_interp_id,interp =AGRIF_linear)
!      CALL Agrif_Set_bcinterp( ts_sponge_id,interp =AGRIF_linear)

      IF ( lk_div_cons ) THEN
         l_spc_ssh = .FALSE.
         CALL Agrif_Set_bcinterp(sshn_id,interp=AGRIF_constant)
         CALL Agrif_Set_bcinterp(ub2b_cor_id,interp=AGRIF_constant)
         CALL Agrif_Set_bcinterp(vb2b_cor_id,interp=AGRIF_constant)
! JC: Disable this until we found a workaround for masked corners:
! Revert to zero order interpolation meanwhile
!      CALL Agrif_Set_bcinterp(ub2b_interp_id,interp1=Agrif_linear,interp2=AGRIF_linearconserv)
!      CALL Agrif_Set_bcinterp(vb2b_interp_id,interp1=AGRIF_linearconserv,interp2=Agrif_linear)
         CALL Agrif_Set_bcinterp(ub2b_interp_id,interp1=Agrif_linear,interp2=AGRIF_constant)
         CALL Agrif_Set_bcinterp(vb2b_interp_id,interp1=AGRIF_constant,interp2=Agrif_linear)

         CALL Agrif_Set_bcinterp(unb_interp_id,interp1=Agrif_linear,interp2=AGRIF_constant )
         CALL Agrif_Set_bcinterp(vnb_interp_id,interp1=AGRIF_constant,interp2=Agrif_linear )
         CALL Agrif_Set_bcinterp(unb_sponge_id,interp1=Agrif_linear,interp2=AGRIF_constant )
         CALL Agrif_Set_bcinterp(vnb_sponge_id,interp1=AGRIF_constant,interp2=Agrif_linear )

         CALL Agrif_Set_bcinterp(sshn_frc_id,interp=AGRIF_constant)
         
      ELSE
         l_spc_ssh = .TRUE.
         CALL Agrif_Set_bcinterp(sshn_id,interp =AGRIF_linear)
         CALL Agrif_Set_bcinterp(ub2b_interp_id,interp1=Agrif_linear,interp2=AGRIF_ppm) 
         CALL Agrif_Set_bcinterp(vb2b_interp_id,interp1=AGRIF_ppm,interp2=Agrif_linear)

         CALL Agrif_Set_bcinterp(unb_interp_id,interp1=Agrif_linear,interp2=AGRIF_ppm   )
         CALL Agrif_Set_bcinterp(vnb_interp_id,interp1=AGRIF_ppm   ,interp2=Agrif_linear)
         CALL Agrif_Set_bcinterp(unb_sponge_id,interp1=Agrif_linear,interp2=AGRIF_ppm   )
         CALL Agrif_Set_bcinterp(vnb_sponge_id,interp1=AGRIF_ppm   ,interp2=Agrif_linear)
      ENDIF

      CALL Agrif_Set_bcinterp(un_interp_id,interp1=Agrif_linear,interp2=AGRIF_ppm   )
      CALL Agrif_Set_bcinterp(vn_interp_id,interp1=AGRIF_ppm   ,interp2=Agrif_linear)

      CALL Agrif_Set_bcinterp(un_sponge_id,interp1=Agrif_linear,interp2=AGRIF_ppm   )
      CALL Agrif_Set_bcinterp(vn_sponge_id,interp1=AGRIF_ppm   ,interp2=Agrif_linear)

!      CALL Agrif_Set_bcinterp(un_interp_id,interp1=Agrif_linear,interp2=AGRIF_constant   )
!      CALL Agrif_Set_bcinterp(vn_interp_id,interp1=AGRIF_constant   ,interp2=Agrif_linear)

!      CALL Agrif_Set_bcinterp(un_sponge_id,interp1=Agrif_linear,interp2=AGRIF_constant   )
!      CALL Agrif_Set_bcinterp(vn_sponge_id,interp1=AGRIF_constant   ,interp2=Agrif_linear)

      IF( ln_zdftke.OR.ln_zdfgls )  CALL Agrif_Set_bcinterp( avm_id, interp=AGRIF_linear )
    

!      CALL Agrif_Set_bcinterp(gphit_id,interp=AGRIF_constant)
!      CALL Agrif_Set_bcinterp(glamt_id,interp=AGRIF_constant)

      ! 3. Location of interpolation
      !-----------------------------
      CALL Agrif_Set_bc(  ts_interp_id, (/0,ind1-1/) ) ! if west,  rhox=3 and nbghost=3: columns 2 to 4
      CALL Agrif_Set_bc(  un_interp_id, (/0,ind1-1/) ) 
      CALL Agrif_Set_bc(  vn_interp_id, (/0,ind1-1/) )

      CALL Agrif_Set_bc(  ts_sponge_id, (/-nn_sponge_len*imaxrho-1,0/) )  ! if west,  rhox=3, nn_sponge_len=2 
      CALL Agrif_Set_bc(  un_sponge_id, (/-nn_sponge_len*imaxrho-1,0/) )  ! and nbghost=3: 
      CALL Agrif_Set_bc(  vn_sponge_id, (/-nn_sponge_len*imaxrho-1,0/) )  ! columns 4 to 11

      CALL Agrif_Set_bc(       sshn_id, (/-imaxrho*nn_shift_bar,ind1-1/) )
      CALL Agrif_Set_bc(   sshn_frc_id, (/-imaxrho*nn_shift_bar,ind1-1/) )
      CALL Agrif_Set_bc( unb_interp_id, (/-imaxrho*nn_shift_bar,ind1-1/) )
      CALL Agrif_Set_bc( vnb_interp_id, (/-imaxrho*nn_shift_bar,ind1-1/) )
      CALL Agrif_Set_bc(ub2b_interp_id, (/-imaxrho*nn_shift_bar,ind1-1/) )
      CALL Agrif_Set_bc(vb2b_interp_id, (/-imaxrho*nn_shift_bar,ind1-1/) )
      CALL Agrif_Set_bc( unb_sponge_id, (/-(nn_sponge_len+nn_shift_bar)*imaxrho,-imaxrho*nn_shift_bar/) )
      CALL Agrif_Set_bc( vnb_sponge_id, (/-(nn_sponge_len+nn_shift_bar)*imaxrho,-imaxrho*nn_shift_bar/) )
      CALL Agrif_Set_bc(   ub2b_cor_id, (/-imaxrho*nn_shift_bar,ind1/) )
      CALL Agrif_Set_bc(   vb2b_cor_id, (/-imaxrho*nn_shift_bar,ind1/) )
      IF( ln_zdftke.OR.ln_zdfgls ) CALL Agrif_Set_bc( avm_id, (/0,ind1-1/) )
!!$      CALL Agrif_Set_bc(glamt_id, (/0,ind1-1/) )  
!!$      CALL Agrif_Set_bc(gphit_id, (/0,ind1-1/) )  

      ! 4. Update type
      !--------------- 

# if defined UPD_HIGH
      CALL Agrif_Set_Updatetype(  ts_interp_id,update  = Agrif_Update_Full_Weighting)
      CALL Agrif_Set_Updatetype(  un_update_id,update1 = Agrif_Update_Average       , update2 = Agrif_Update_Full_Weighting)
      CALL Agrif_Set_Updatetype(  vn_update_id,update1 = Agrif_Update_Full_Weighting, update2 = Agrif_Update_Average       )

      CALL Agrif_Set_Updatetype( unb_update_id,update1 = Agrif_Update_Average       , update2 = Agrif_Update_Full_Weighting) 
      CALL Agrif_Set_Updatetype( vnb_update_id,update1 = Agrif_Update_Full_Weighting, update2 = Agrif_Update_Average       )
      CALL Agrif_Set_Updatetype(ub2b_update_id,update1 = Agrif_Update_Average       , update2 = Agrif_Update_Full_Weighting)
      CALL Agrif_Set_Updatetype(vb2b_update_id,update1 = Agrif_Update_Full_Weighting, update2 = Agrif_Update_Average       )
      CALL Agrif_Set_Updatetype(       sshn_id,update  = Agrif_Update_Full_Weighting)
      CALL Agrif_Set_Updatetype(   sshn_frc_id,update  = Agrif_Update_Full_Weighting)
      CALL Agrif_Set_Updatetype(        e3t_id,update  = Agrif_Update_Full_Weighting)
      CALL Agrif_Set_Updatetype(        e3u_id,update1 = Agrif_Update_Average       , update2 = Agrif_Update_Full_Weighting)
      CALL Agrif_Set_Updatetype(        e3v_id,update1 = Agrif_Update_Full_Weighting, update2 = Agrif_Update_Average       )
      CALL Agrif_Set_Updatetype(        e3f_id,update  = Agrif_Update_Copy          )

  !    IF( ln_zdftke.OR.ln_zdfgls ) THEN
!         CALL Agrif_Set_Updatetype( en_id, update = AGRIF_Update_Full_Weighting)
!         CALL Agrif_Set_Updatetype(avt_id, update = AGRIF_Update_Full_Weighting)
!         CALL Agrif_Set_Updatetype(avm_id, update = AGRIF_Update_Full_Weighting)
   !   ENDIF

#else
      CALL Agrif_Set_Updatetype(  ts_update_id,update  = AGRIF_Update_Average)
      CALL Agrif_Set_Updatetype(  un_update_id,update1 = Agrif_Update_Copy   , update2 = Agrif_Update_Average)
      CALL Agrif_Set_Updatetype(  vn_update_id,update1 = Agrif_Update_Average, update2 = Agrif_Update_Copy   )

      CALL Agrif_Set_Updatetype( unb_update_id,update1 = Agrif_Update_Copy   , update2 = Agrif_Update_Average)
      CALL Agrif_Set_Updatetype( vnb_update_id,update1 = Agrif_Update_Average, update2 = Agrif_Update_Copy   )
      CALL Agrif_Set_Updatetype(ub2b_update_id,update1 = Agrif_Update_Copy   , update2 = Agrif_Update_Average)
      CALL Agrif_Set_Updatetype(vb2b_update_id,update1 = Agrif_Update_Average, update2 = Agrif_Update_Copy   )
      CALL Agrif_Set_Updatetype(       sshn_id,update  = AGRIF_Update_Average)
      CALL Agrif_Set_Updatetype(   sshn_frc_id,update  = AGRIF_Update_Average)
      CALL Agrif_Set_Updatetype(        e3t_id,update  = AGRIF_Update_Average)
      CALL Agrif_Set_Updatetype(        e3u_id,update1 = Agrif_Update_Copy   , update2 = Agrif_Update_Average)
      CALL Agrif_Set_Updatetype(        e3v_id,update1 = Agrif_Update_Average, update2 = Agrif_Update_Copy   )
      CALL Agrif_Set_Updatetype(        e3f_id,update  = Agrif_Update_Copy   )

 !     IF( ln_zdftke.OR.ln_zdfgls ) THEN
!         CALL Agrif_Set_Updatetype( en_id, update = AGRIF_Update_Average)
!         CALL Agrif_Set_Updatetype(avt_id, update = AGRIF_Update_Average)
!         CALL Agrif_Set_Updatetype(avm_id, update = AGRIF_Update_Average)
 !     ENDIF

#endif
      !
   END SUBROUTINE agrif_declare_var

#if defined key_si3
   SUBROUTINE Agrif_InitValues_cont_ice
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_InitValues_cont_ice ***
      !!----------------------------------------------------------------------
      USE Agrif_Util
      USE sbc_oce, ONLY : nn_fsbc  ! clem: necessary otherwise Agrif_Parent(nn_fsbc) = nn_fsbc
      USE ice
      USE agrif_ice
      USE in_out_manager
      USE agrif_ice_interp
      USE lib_mpp
      !
      IMPLICIT NONE
      !
      !!----------------------------------------------------------------------
      ! Controls

      ! clem: For some reason, nn_fsbc(child)/=1 does not work properly (signal can be largely degraded by the agrif zoom)
      !          the run must satisfy CFL=Uice/(dx/dt) < 0.6/nn_fsbc(child)
      !          therefore, if nn_fsbc(child)>1 one must reduce the time-step in proportion to nn_fsbc(child), which is not acceptable
      !       If a solution is found, the following stop could be removed because the rest of the code take nn_fsbc(child) into account     
      IF( nn_fsbc > 1 )  CALL ctl_stop('nn_fsbc(child) must be set to 1 otherwise agrif and sea-ice may not work properly')

      ! stop if rhot * nn_fsbc(parent) /= N * nn_fsbc(child) with N being integer
      IF( MOD( Agrif_irhot() * Agrif_Parent(nn_fsbc), nn_fsbc ) /= 0 )  THEN
         CALL ctl_stop('rhot * nn_fsbc(parent) /= N * nn_fsbc(child), therefore nn_fsbc(child) should be set to 1 or nn_fsbc(parent)')
      ENDIF
      ! First Interpolations (using "after" ice subtime step => nbstep_ice=1)
      !----------------------------------------------------------------------
      nbstep_ice = ( Agrif_irhot() * Agrif_Parent(nn_fsbc) / nn_fsbc ) ! clem: to have calledweight=1 in interp (otherwise the western border of the zoom is wrong)
      CALL agrif_interp_ice('U') ! interpolation of ice velocities
      CALL agrif_interp_ice('V') ! interpolation of ice velocities
      CALL agrif_interp_ice('T') ! interpolation of ice tracers 
      nbstep_ice = 0   
      !
   END SUBROUTINE Agrif_InitValues_cont_ice

   
   SUBROUTINE agrif_declare_var_ice
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE agrif_declare_var_ice ***
      !!----------------------------------------------------------------------
      USE Agrif_Util
      USE ice
      USE par_oce, ONLY : nbghostcells, nbghostcells_x_w, nbghostcells_y_s
      !
      IMPLICIT NONE
      !
      INTEGER :: ind1, ind2, ind3
      INTEGER :: ipl
      !!----------------------------------------------------------------------
      !
      ! 1. Declaration of the type of variable which have to be interpolated (parent=>child)
      !       agrif_declare_variable(position,1st point index,--,--,dimensions,name)
      !           ex.:  position=> 1,1 = not-centered (in i and j)
      !                            2,2 =     centered (    -     )
      !                 index   => 1,1 = one ghost line
      !                            2,2 = two ghost lines
      !-------------------------------------------------------------------------------------
      ind1 =              nbghostcells - 1 ! Remove one land cell in ghosts 
      ind2 = nn_hls + 1 + nbghostcells_x_w
      ind3 = nn_hls + 1 + nbghostcells_y_s
      ipl = jpl*(9+nlay_s+nlay_i)
      CALL agrif_declare_variable((/2,2,0/),(/ind2,ind3,0/),(/'x','y','N'/),(/1,1,1/),(/jpi,jpj,ipl/),tra_ice_id)
      CALL agrif_declare_variable((/1,2/)  ,(/ind2-1,ind3/),(/'x','y'    /),(/1,1  /),(/jpi,jpj    /),  u_ice_id)
      CALL agrif_declare_variable((/2,1/)  ,(/ind2,ind3-1/),(/'x','y'    /),(/1,1  /),(/jpi,jpj    /),  v_ice_id)

      CALL agrif_declare_variable((/2,2,0/),(/ind3,ind3,0/),(/'x','y','N'/),(/1,1,1/),(/jpi,jpj,ipl/),tra_iceini_id)
      CALL agrif_declare_variable((/1,2/)  ,(/ind2-1,ind3/),(/'x','y'    /),(/1,1  /),(/jpi,jpj    /),  u_iceini_id)
      CALL agrif_declare_variable((/2,1/)  ,(/ind2,ind3-1/),(/'x','y'    /),(/1,1  /),(/jpi,jpj    /),  v_iceini_id)

      ! 2. Set interpolations (normal & tangent to the grid cell for velocities)
      !-----------------------------------
      CALL Agrif_Set_bcinterp(tra_ice_id, interp  = AGRIF_linear)
      CALL Agrif_Set_bcinterp(u_ice_id  , interp1 = Agrif_linear,interp2 = AGRIF_ppm   )
      CALL Agrif_Set_bcinterp(v_ice_id  , interp1 = AGRIF_ppm   ,interp2 = Agrif_linear)

      CALL Agrif_Set_bcinterp(tra_iceini_id, interp  = AGRIF_linear)
      CALL Agrif_Set_interp  (tra_iceini_id, interp  = AGRIF_linear)
      CALL Agrif_Set_bcinterp(u_iceini_id  , interp  = AGRIF_linear)
      CALL Agrif_Set_interp  (u_iceini_id  , interp  = AGRIF_linear)
      CALL Agrif_Set_bcinterp(v_iceini_id  , interp  = AGRIF_linear)
      CALL Agrif_Set_interp  (v_iceini_id  , interp  = AGRIF_linear)

      ! 3. Set location of interpolations
      !----------------------------------
      CALL Agrif_Set_bc(tra_ice_id,(/0,ind1-1/))
      CALL Agrif_Set_bc(u_ice_id  ,(/0,ind1-1/))
      CALL Agrif_Set_bc(v_ice_id  ,(/0,ind1-1/))

      CALL Agrif_Set_bc(tra_iceini_id,(/0,ind1-1/))
      CALL Agrif_Set_bc(u_iceini_id  ,(/0,ind1-1/))
      CALL Agrif_Set_bc(v_iceini_id  ,(/0,ind1-1/))

      ! 4. Set update type in case 2 ways (child=>parent) (normal & tangent to the grid cell for velocities)
      !--------------------------------------------------
# if defined UPD_HIGH
      CALL Agrif_Set_Updatetype(tra_ice_id, update  = Agrif_Update_Full_Weighting)
      CALL Agrif_Set_Updatetype(u_ice_id  , update1 = Agrif_Update_Average       , update2 = Agrif_Update_Full_Weighting)
      CALL Agrif_Set_Updatetype(v_ice_id  , update1 = Agrif_Update_Full_Weighting, update2 = Agrif_Update_Average       )
# else
      CALL Agrif_Set_Updatetype(tra_ice_id, update  = AGRIF_Update_Average)
      CALL Agrif_Set_Updatetype(u_ice_id  , update1 = Agrif_Update_Copy   , update2 = Agrif_Update_Average)
      CALL Agrif_Set_Updatetype(v_ice_id  , update1 = Agrif_Update_Average, update2 = Agrif_Update_Copy   )
# endif

   END SUBROUTINE agrif_declare_var_ice
#endif


# if defined key_top
   SUBROUTINE Agrif_InitValues_cont_top
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_InitValues_cont_top ***
      !!----------------------------------------------------------------------
      USE Agrif_Util
      USE agrif_oce
      USE oce 
      USE dom_oce
      USE nemogcm
      USE par_trc
      USE lib_mpp
      USE trc
      USE in_out_manager
      USE agrif_oce_sponge
      USE agrif_top_update
      USE agrif_top_interp
      USE agrif_top_sponge
      !
      IMPLICIT NONE
      !
      CHARACTER(len=10) :: cl_check1, cl_check2, cl_check3
      LOGICAL :: check_namelist
      !!----------------------------------------------------------------------

      ! 1. Declaration of the type of variable which have to be interpolated
      !---------------------------------------------------------------------
      CALL agrif_declare_var_top

      ! 2. First interpolations of potentially non zero fields
      !-------------------------------------------------------
      Agrif_SpecialValue    = 0._wp
      Agrif_UseSpecialValue = l_spc_top 
      l_vremap              = ln_vert_remap
      CALL Agrif_Bc_variable(trn_id,calledweight=1.,procname=interptrn)
      CALL Agrif_Sponge
      tabspongedone_trn = .FALSE.
      CALL Agrif_Bc_variable(trn_sponge_id,calledweight=1.,procname=interptrn_sponge)
      Agrif_UseSpecialValue = .FALSE.
      l_vremap              = .FALSE.
      ! reset tsa to zero
      tr(:,:,:,:,Krhs_a) = 0._wp

      ! 3. Some controls
      !-----------------
      check_namelist = .TRUE.

      IF( check_namelist ) THEN
         ! Check time steps
         IF( NINT(Agrif_Rhot()) * NINT(rdt) .NE. Agrif_Parent(rdt) ) THEN
            WRITE(cl_check1,*)  Agrif_Parent(rdt)
            WRITE(cl_check2,*)  rdt
            WRITE(cl_check3,*)  rdt*Agrif_Rhot()
            CALL ctl_stop( 'incompatible time step between grids',   &
               &               'parent grid value : '//cl_check1    ,   & 
               &               'child  grid value : '//cl_check2    ,   & 
               &               'value on child grid should be changed to  &
               &               :'//cl_check3  )
         ENDIF

         ! Check run length
         IF( Agrif_IRhot() * (Agrif_Parent(nitend)- &
            Agrif_Parent(nit000)+1) .NE. (nitend-nit000+1) ) THEN
            WRITE(cl_check1,*)  (Agrif_Parent(nit000)-1)*Agrif_IRhot() + 1
            WRITE(cl_check2,*)   Agrif_Parent(nitend)   *Agrif_IRhot()
            CALL ctl_warn( 'incompatible run length between grids'               ,   &
               &              ' nit000 on fine grid will be change to : '//cl_check1,   &
               &              ' nitend on fine grid will be change to : '//cl_check2    )
            nit000 = (Agrif_Parent(nit000)-1)*Agrif_IRhot() + 1
            nitend =  Agrif_Parent(nitend)   *Agrif_IRhot()
         ENDIF
      
         !
         IF (Agrif_Parent(ln_top_euler).OR.ln_top_euler) THEN
            CALL ctl_stop( 'AGRIF and ln_top_euler=T not implemented')
         ENDIF 
      ENDIF
      !
   END SUBROUTINE Agrif_InitValues_cont_top


   SUBROUTINE agrif_declare_var_top
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE agrif_declare_var_top ***
      !!----------------------------------------------------------------------
      USE agrif_util
      USE agrif_oce
      USE dom_oce
      USE trc
      !!
      IMPLICIT NONE
      !
      INTEGER :: ind1, ind2, ind3, imaxrho
      !!----------------------------------------------------------------------
!RB_CMEMS : declare here init for top      
      ! 1. Declaration of the type of variable which have to be interpolated
      !---------------------------------------------------------------------
      ind1 =              nbghostcells - 1 ! Remove one land cell in ghosts 
      ind2 = nn_hls + 1 + nbghostcells_x_w
      ind3 = nn_hls + 1 + nbghostcells_y_s
      imaxrho = MAX(Agrif_irhox(), Agrif_irhoy())

      CALL agrif_declare_variable((/2,2,0,0/),(/ind2,ind3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/jpi,jpj,jpk,jptra+1/),trn_id)
      CALL agrif_declare_variable((/2,2,0,0/),(/ind2,ind3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/jpi,jpj,jpk,jptra+1/),trn_sponge_id)

      ! 2. Type of interpolation
      !-------------------------
!      l_spc_top = .TRUE.
!      CALL Agrif_Set_bcinterp(trn_id,interp=AGRIF_linear)
!      CALL Agrif_Set_bcinterp(trn_sponge_id,interp=AGRIF_linear)
      l_spc_top = .FALSE.
      CALL Agrif_Set_bcinterp(trn_id,interp=AGRIF_constant)
      CALL Agrif_Set_bcinterp(trn_sponge_id,interp=AGRIF_constant)

      ! 3. Location of interpolation
      !-----------------------------
      CALL Agrif_Set_bc(trn_id,(/0,ind1-1/))
      CALL Agrif_Set_bc(trn_sponge_id,(/-nn_sponge_len*imaxrho-1,0/))

      ! 4. Update type
      !--------------- 
# if defined UPD_HIGH
      CALL Agrif_Set_Updatetype(trn_id, update = Agrif_Update_Full_Weighting)
#else
      CALL Agrif_Set_Updatetype(trn_id, update = AGRIF_Update_Average)
#endif
   !
   END SUBROUTINE agrif_declare_var_top
# endif
   

   SUBROUTINE Agrif_detect( kg, ksizex )
      !!----------------------------------------------------------------------
      !!                      *** ROUTINE Agrif_detect ***
      !!----------------------------------------------------------------------
      INTEGER, DIMENSION(2) :: ksizex
      INTEGER, DIMENSION(ksizex(1),ksizex(2)) :: kg 
      !!----------------------------------------------------------------------
      !
      RETURN
      !
   END SUBROUTINE Agrif_detect

   
   SUBROUTINE agrif_nemo_init
      !!----------------------------------------------------------------------
      !!                     *** ROUTINE agrif_init ***
      !!----------------------------------------------------------------------
      USE agrif_oce 
      USE agrif_ice
      USE dom_oce
      USE in_out_manager
      USE lib_mpp
      !
      IMPLICIT NONE
      !
      INTEGER  ::   ios                 ! Local integer output status for namelist read
      INTEGER  ::   imin, imax, jmin, jmax
      NAMELIST/namagrif/ ln_agrif_2way, ln_init_chfrpar, rn_sponge_tra, rn_sponge_dyn, rn_trelax_tra, rn_trelax_dyn, &
                       & ln_spc_dyn, ln_vert_remap, ln_chk_bathy
      !!--------------------------------------------------------------------------------------
      !
      IF ( .NOT.Agrif_Root() ) THEN
         !
         READ  ( numnam_ref, namagrif, IOSTAT = ios, ERR = 901)
901 IF( ios /= 0 )   CALL ctl_nam ( ios , 'namagrif in reference namelist' )
         READ  ( numnam_cfg, namagrif, IOSTAT = ios, ERR = 902 )
902 IF( ios >  0 )   CALL ctl_nam ( ios , 'namagrif in configuration namelist' )
         IF(lwm) WRITE ( numond, namagrif )
         !
         IF(lwp) THEN                    ! control print
            WRITE(numout,*)
            WRITE(numout,*) 'agrif_nemo_init : AGRIF parameters'
            WRITE(numout,*) '~~~~~~~~~~~~~~~'
            WRITE(numout,*) '   Namelist namagrif : set AGRIF parameters'
            WRITE(numout,*) '      Two way nesting activated ln_agrif_2way         = ', ln_agrif_2way
            WRITE(numout,*) '      child initial state from parent ln_init_chfrpar = ', ln_init_chfrpar
            WRITE(numout,*) '      ad. sponge coeft for tracers      rn_sponge_tra = ', rn_sponge_tra
            WRITE(numout,*) '      ad. sponge coeft for dynamics     rn_sponge_tra = ', rn_sponge_dyn
            WRITE(numout,*) '      ad. time relaxation for tracers   rn_trelax_tra = ', rn_trelax_tra
            WRITE(numout,*) '      ad. time relaxation for dynamics  rn_trelax_dyn = ', rn_trelax_dyn
            WRITE(numout,*) '      use special values for dynamics   ln_spc_dyn    = ', ln_spc_dyn
            WRITE(numout,*) '      vertical remapping                ln_vert_remap = ', ln_vert_remap
            WRITE(numout,*) '      check bathymetry                  ln_chk_bathy  = ', ln_chk_bathy
         ENDIF

         imin = Agrif_Ix()
         imax = Agrif_Ix() + nbcellsx/AGRIF_Irhox()
         jmin = Agrif_Iy()
         jmax = Agrif_Iy() + nbcellsy/AGRIF_Irhoy()
         lk_west  = .TRUE. ; lk_east  = .TRUE.
         lk_north = .TRUE. ; lk_south = .TRUE.

         ! Check zoom position along i:
         ! ----------------------------
         IF ( imin >= imax ) THEN
            CALL ctl_stop( 'STOP', 'AGRIF zoom imin must be < imax' )
         ENDIF

         IF ( Agrif_Parent(l_Iperio) ) THEN
            IF ( l_Iperio ) THEN ! Cyclic east-west zoom
               lk_west = .FALSE. ; lk_east = .FALSE.
               ! Checks:
               IF ( imin/=1-Agrif_Parent(nbghostcells_x_w) ) THEN
                  WRITE(ctmp1, 9000) ' AGRIF zoom is East-West cyclic, imin must = ', &
                  1 - Agrif_Parent(nbghostcells_x_w)
                  CALL ctl_stop( 'STOP', ctmp1 )
               ENDIF
               IF ( imax/=Agrif_Parent(Ni0glo)+1-Agrif_Parent(nbghostcells_x_w)) THEN
                  WRITE(ctmp1, 9000) ' AGRIF zoom is East-West cyclic, imax must = ', &
                  Agrif_Parent(Ni0glo) + 1 - Agrif_Parent(nbghostcells_x_w)
                  CALL ctl_stop( 'STOP', ctmp1 )
               ENDIF
            ELSE
               IF ( imax>Agrif_Parent(Ni0glo)-Agrif_Parent(nbghostcells_x_w)) THEN
                  WRITE(ctmp1, 9000) ' AGRIF zoom imax must be <= ', &
                  Agrif_Parent(Ni0glo) - Agrif_Parent(nbghostcells_x_w)
                  CALL ctl_stop( 'STOP', ctmp1 )
               ENDIF
            ENDIF
         ELSE
            IF ( imin<2-Agrif_Parent(nbghostcells_x_w) ) THEN
               WRITE(ctmp1, 9000) ' AGRIF zoom imin must be >= ', &
               2 - Agrif_Parent(nbghostcells_x_w)
               CALL ctl_stop( 'STOP', ctmp1 )
            ENDIF
            IF ( imax>Agrif_Parent(Ni0glo)-Agrif_Parent(nbghostcells_x_w)) THEN
               WRITE(ctmp1, 9000) ' AGRIF zoom imax must be <= ', &
               Agrif_Parent(Ni0glo) - Agrif_Parent(nbghostcells_x_w)
               CALL ctl_stop( 'STOP', ctmp1 )
            ENDIF
            IF ( imin==2-Agrif_Parent(nbghostcells_x_w) )                    lk_west = .FALSE.
            IF ( imax==Agrif_Parent(Ni0glo)-Agrif_Parent(nbghostcells_x_w) ) lk_east = .FALSE.  
         ENDIF

         ! Check zoom position along j:
         ! ----------------------------
         IF ( jmin >= jmax ) THEN
            CALL ctl_stop( 'STOP', 'AGRIF zoom jmin must be < jmax' )
         ENDIF

         IF ( Agrif_Parent(l_NFold) ) THEN
            IF ( l_NFold ) THEN ! North-Fold 
               lk_north = .FALSE.
               ! Checks:
               IF ( jmax/=Agrif_Parent(Nj0glo)+1-Agrif_Parent(nbghostcells_y_s)) THEN 
                  WRITE(ctmp1, 9000) ' AGRIF zoom has a North-Fold, jmax must = ', &
                  Agrif_Parent(Nj0glo) + 1 - Agrif_Parent(nbghostcells_y_s)
                  CALL ctl_stop( 'STOP', ctmp1 )
               ENDIF
            ENDIF
         ELSE
            IF ( jmax>Agrif_Parent(Nj0glo)-Agrif_Parent(nbghostcells_y_s)) THEN 
               WRITE(ctmp1, 9000) ' AGRIF zoom jmax must be <= ', &
               Agrif_Parent(Nj0glo) - Agrif_Parent(nbghostcells_y_s)
               CALL ctl_stop( 'STOP', ctmp1 )
            ENDIF
            IF ( jmax==Agrif_Parent(Nj0glo)-Agrif_Parent(nbghostcells_y_s) ) lk_north = .FALSE. 
         ENDIF

         IF ( jmin<2-Agrif_Parent(nbghostcells_y_s)) THEN 
            WRITE(ctmp1, 9000) ' AGRIF zoom jmin must be >= ', &
            2 - Agrif_Parent(nbghostcells_y_s)
            CALL ctl_stop( 'STOP', ctmp1 )
         ENDIF
         IF ( jmin==2-Agrif_Parent(nbghostcells_y_s) ) lk_south = .FALSE. 

      ELSE ! Root grid
         lk_west  = .FALSE. ; lk_east  = .FALSE.
         lk_north = .FALSE. ; lk_south = .FALSE.
      ENDIF
  
      ! Set ghost cells including over Parent grid: 
      nbghostcells_x_w = nbghostcells
      nbghostcells_x_e = nbghostcells
      nbghostcells_y_s = nbghostcells
      nbghostcells_y_n = nbghostcells

      IF (.NOT.lk_west ) nbghostcells_x_w = 1
      IF (.NOT.lk_east ) nbghostcells_x_e = 1
      IF (.NOT.lk_south) nbghostcells_y_s = 1
      IF (.NOT.lk_north) nbghostcells_y_n = 1

      IF ( l_Iperio ) THEN
         nbghostcells_x_w = 0 ; nbghostcells_x_e = 0
      ENDIF
      IF ( l_NFold ) THEN
         nbghostcells_y_n = 0
      ENDIF
      
      IF ( .NOT.Agrif_Root() ) THEN ! Check expected grid size: 
         IF( (.NOT.ln_vert_remap).AND.(jpkglo>Agrif_Parent(jpkglo)) )   CALL ctl_stop( 'STOP',    &
           &   'AGRIF children must have less or equal number of vertical levels without ln_vert_remap defined' ) 
         IF( Ni0glo /= nbcellsx + nbghostcells_x_w + nbghostcells_x_e ) CALL ctl_stop( 'STOP',    &
           &   'AGRIF children requires jpiglo == nbcellsx + nbghostcells_x_w + nbghostcells_x_e' )
         IF( Nj0glo /= nbcellsy + nbghostcells_y_s + nbghostcells_y_n ) CALL ctl_stop( 'STOP',    &
           &   'AGRIF children requires jpjglo == nbcellsy + nbghostcells_y_s + nbghostcells_y_n' )
         IF( ln_use_jattr )   CALL ctl_stop( 'STOP', 'AGRIF children requires ln_use_jattr = .false. ' )

         IF(lwp) THEN                     ! Control print
            WRITE(numout,*)
            WRITE(numout,*) 'AGRIF boundaries and ghost cells:'
            WRITE(numout,*) 'lk_west' , lk_west
            WRITE(numout,*) 'lk_east' , lk_east
            WRITE(numout,*) 'lk_south', lk_south
            WRITE(numout,*) 'lk_north', lk_north
            WRITE(numout,*) 'nbghostcells_y_s', nbghostcells_y_s
            WRITE(numout,*) 'nbghostcells_y_n', nbghostcells_y_n
            WRITE(numout,*) 'nbghostcells_x_w', nbghostcells_x_w
            WRITE(numout,*) 'nbghostcells_x_e', nbghostcells_x_e
         ENDIF
      ENDIF

9000  FORMAT (a, i4)
      !
      !
   END SUBROUTINE agrif_nemo_init

   
# if ! defined key_mpi_off
   SUBROUTINE Agrif_InvLoc( indloc, nprocloc, i, indglob )
      !!----------------------------------------------------------------------
      !!                     *** ROUTINE Agrif_InvLoc ***
      !!----------------------------------------------------------------------
      USE dom_oce
      !!
      IMPLICIT NONE
      !
      INTEGER :: indglob, indloc, nprocloc, i
      !!----------------------------------------------------------------------
      !
      SELECT CASE( i )
      CASE(1)        ;   indglob = mig(indloc)
      CASE(2)        ;   indglob = mjg(indloc)
      CASE DEFAULT   ;   indglob = indloc
      END SELECT
      !
   END SUBROUTINE Agrif_InvLoc

   
   SUBROUTINE Agrif_get_proc_info( imin, imax, jmin, jmax )
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_get_proc_info ***
      !!----------------------------------------------------------------------
      USE par_oce
      !!
      IMPLICIT NONE
      !
      INTEGER, INTENT(out) :: imin, imax
      INTEGER, INTENT(out) :: jmin, jmax
      !!----------------------------------------------------------------------
      !
      imin = mig( 1 )
      jmin = mjg( 1 )
      imax = mig(jpi)
      jmax = mjg(jpj)
      ! 
   END SUBROUTINE Agrif_get_proc_info

   
   SUBROUTINE Agrif_estimate_parallel_cost(imin, imax,jmin, jmax, nbprocs, grid_cost)
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_estimate_parallel_cost ***
      !!----------------------------------------------------------------------
      USE par_oce
      !!
      IMPLICIT NONE
      !
      INTEGER,  INTENT(in)  :: imin, imax
      INTEGER,  INTENT(in)  :: jmin, jmax
      INTEGER,  INTENT(in)  :: nbprocs
      REAL(wp), INTENT(out) :: grid_cost
      !!----------------------------------------------------------------------
      !
      grid_cost = REAL(imax-imin+1,wp)*REAL(jmax-jmin+1,wp) / REAL(nbprocs,wp)
      !
   END SUBROUTINE Agrif_estimate_parallel_cost

# endif

   SUBROUTINE nemo_mapping(ndim,ptx,pty,bounds,bounds_chunks,correction_required,nb_chunks)
      !!----------------------------------------------------------------------
      !!                   *** ROUTINE Nemo_mapping ***
      !!----------------------------------------------------------------------
      USE dom_oce
      !!
      IMPLICIT NONE
      !
      INTEGER :: ndim
      INTEGER :: ptx, pty
      INTEGER, DIMENSION(ndim,2,2) :: bounds
      INTEGER, DIMENSION(:,:,:,:), ALLOCATABLE :: bounds_chunks
      LOGICAL, DIMENSION(:), ALLOCATABLE :: correction_required
      INTEGER :: nb_chunks
      !
      INTEGER :: i

      IF (agrif_debug_interp) THEN
         DO i=1,ndim
            WRITE(*,*) 'direction = ',i,bounds(i,1,2),bounds(i,2,2)
         ENDDO
      ENDIF

      IF(( bounds(2,2,2) > jpjglo).AND. ( l_NFold )) THEN
         IF( bounds(2,1,2) <=jpjglo) THEN
            nb_chunks = 2
            ALLOCATE(bounds_chunks(nb_chunks,ndim,2,2))
            ALLOCATE(correction_required(nb_chunks))
            DO i = 1,nb_chunks
               bounds_chunks(i,:,:,:) = bounds
            END DO
        
      ! FIRST CHUNCK (for j<=jpjglo)   

      ! Original indices
            bounds_chunks(1,1,1,1) = bounds(1,1,2)
            bounds_chunks(1,1,2,1) = bounds(1,2,2)
            bounds_chunks(1,2,1,1) = bounds(2,1,2)
            bounds_chunks(1,2,2,1) = jpjglo

            bounds_chunks(1,1,1,2) = bounds(1,1,2)
            bounds_chunks(1,1,2,2) = bounds(1,2,2)
            bounds_chunks(1,2,1,2) = bounds(2,1,2)
            bounds_chunks(1,2,2,2) = jpjglo

      ! Correction required or not
            correction_required(1)=.FALSE.
       
      ! SECOND CHUNCK (for j>jpjglo)

      ! Original indices
            bounds_chunks(2,1,1,1) = bounds(1,1,2)
            bounds_chunks(2,1,2,1) = bounds(1,2,2)
            bounds_chunks(2,2,1,1) = jpjglo-2
            bounds_chunks(2,2,2,1) = bounds(2,2,2)

      ! Where to find them
      ! We use the relation TAB(ji,jj)=TAB(jpiglo-ji+2,jpjglo-2-(jj-jpjglo))

            IF( ptx == 2) THEN ! T, V points
               bounds_chunks(2,1,1,2) = jpiglo-bounds(1,2,2)+2
               bounds_chunks(2,1,2,2) = jpiglo-bounds(1,1,2)+2
            ELSE ! U, F points
               bounds_chunks(2,1,1,2) = jpiglo-bounds(1,2,2)+1
               bounds_chunks(2,1,2,2) = jpiglo-bounds(1,1,2)+1       
            ENDIF

            IF( pty == 2) THEN ! T, U points
               bounds_chunks(2,2,1,2) = jpjglo-2*nn_hls-(bounds(2,2,2) -jpjglo)
               bounds_chunks(2,2,2,2) = jpjglo-2*nn_hls-(jpjglo-2      -jpjglo)
            ELSE ! V, F points
               bounds_chunks(2,2,1,2) = jpjglo-2*nn_hls-1-(bounds(2,2,2) -jpjglo)
               bounds_chunks(2,2,2,2) = jpjglo-2*nn_hls-1-(jpjglo-2      -jpjglo)
            ENDIF
      ! Correction required or not
            correction_required(2)=.TRUE.

         ELSE
            nb_chunks = 1
            ALLOCATE(bounds_chunks(nb_chunks,ndim,2,2))
            ALLOCATE(correction_required(nb_chunks))
            DO i=1,nb_chunks
               bounds_chunks(i,:,:,:) = bounds
            END DO

            bounds_chunks(1,1,1,1) = bounds(1,1,2)
            bounds_chunks(1,1,2,1) = bounds(1,2,2)
            bounds_chunks(1,2,1,1) = bounds(2,1,2)
            bounds_chunks(1,2,2,1) = bounds(2,2,2)

            bounds_chunks(1,1,1,2) = jpiglo-bounds(1,2,2)+2
            bounds_chunks(1,1,2,2) = jpiglo-bounds(1,1,2)+2

            bounds_chunks(1,2,1,2) = jpjglo-2*nn_hls-(bounds(2,2,2)-jpjglo)
            bounds_chunks(1,2,2,2) = jpjglo-2*nn_hls-(bounds(2,1,2)-jpjglo)

            IF( ptx == 2) THEN ! T, V points
               bounds_chunks(1,1,1,2) = jpiglo-bounds(1,2,2)+2
               bounds_chunks(1,1,2,2) = jpiglo-bounds(1,1,2)+2
            ELSE ! U, F points
               bounds_chunks(1,1,1,2) = jpiglo-bounds(1,2,2)+1
               bounds_chunks(1,1,2,2) = jpiglo-bounds(1,1,2)+1    	
            ENDIF

            IF (pty == 2) THEN ! T, U points
               bounds_chunks(1,2,1,2) = jpjglo-2*nn_hls-(bounds(2,2,2) -jpjglo)
               bounds_chunks(1,2,2,2) = jpjglo-2*nn_hls-(bounds(2,1,2) -jpjglo)
            ELSE ! V, F points
               bounds_chunks(1,2,1,2) = jpjglo-2*nn_hls-1-(bounds(2,2,2) -jpjglo)
               bounds_chunks(1,2,2,2) = jpjglo-2*nn_hls-1-(bounds(2,1,2) -jpjglo)
            ENDIF

            correction_required(1)=.TRUE.          
         ENDIF

      ELSE IF ((bounds(1,1,2) < 1).AND.( l_Iperio )) THEN
         IF (bounds(1,2,2) > 0) THEN
            nb_chunks = 2
            ALLOCATE(correction_required(nb_chunks))
            correction_required=.FALSE.
            ALLOCATE(bounds_chunks(nb_chunks,ndim,2,2))
            DO i=1,nb_chunks
               bounds_chunks(i,:,:,:) = bounds
            END DO
              
            bounds_chunks(1,1,1,2) = bounds(1,1,2)+jpiglo-2*nn_hls
            bounds_chunks(1,1,2,2) = jpiglo-nn_hls
          
            bounds_chunks(1,1,1,1) = bounds(1,1,2)
            bounds_chunks(1,1,2,1) = nn_hls+1 
       
            bounds_chunks(2,1,1,2) = nn_hls+1 
            bounds_chunks(2,1,2,2) = bounds(1,2,2)
          
            bounds_chunks(2,1,1,1) = nn_hls+1 
            bounds_chunks(2,1,2,1) = bounds(1,2,2)
           
         ELSE
            nb_chunks = 1
            ALLOCATE(correction_required(nb_chunks))
            correction_required=.FALSE.
            ALLOCATE(bounds_chunks(nb_chunks,ndim,2,2))
            DO i=1,nb_chunks
               bounds_chunks(i,:,:,:) = bounds
            END DO    
            bounds_chunks(1,1,1,2) = bounds(1,1,2)+jpiglo-2*nn_hls
            bounds_chunks(1,1,2,2) = bounds(1,2,2)+jpiglo-2*nn_hls
          
            bounds_chunks(1,1,1,1) = bounds(1,1,2)
            bounds_chunks(1,1,2,1) = bounds(1,2,2)
         ENDIF
      ELSE
         nb_chunks=1  
         ALLOCATE(correction_required(nb_chunks))
         correction_required=.FALSE.
         ALLOCATE(bounds_chunks(nb_chunks,ndim,2,2))
         DO i=1,nb_chunks
            bounds_chunks(i,:,:,:) = bounds
         END DO
         bounds_chunks(1,1,1,2) = bounds(1,1,2)
         bounds_chunks(1,1,2,2) = bounds(1,2,2)
         bounds_chunks(1,2,1,2) = bounds(2,1,2)
         bounds_chunks(1,2,2,2) = bounds(2,2,2)
          
         bounds_chunks(1,1,1,1) = bounds(1,1,2)
         bounds_chunks(1,1,2,1) = bounds(1,2,2)
         bounds_chunks(1,2,1,1) = bounds(2,1,2)
         bounds_chunks(1,2,2,1) = bounds(2,2,2)              
      ENDIF
        
   END SUBROUTINE nemo_mapping

   FUNCTION agrif_external_switch_index(ptx,pty,i1,isens)

      USE dom_oce
      !
      IMPLICIT NONE

      INTEGER :: ptx, pty, i1, isens
      INTEGER :: agrif_external_switch_index
      !!----------------------------------------------------------------------

      IF( isens == 1 ) THEN
         IF( ptx == 2 ) THEN ! T, V points
            agrif_external_switch_index = jpiglo-i1+2
         ELSE ! U, F points
            agrif_external_switch_index = jpiglo-i1+1 
         ENDIF
      ELSE IF( isens ==2 ) THEN
         IF ( pty == 2 ) THEN ! T, U points
            agrif_external_switch_index = jpjglo-2*nn_hls-(i1 -jpjglo)
         ELSE ! V, F points
            agrif_external_switch_index = jpjglo-2*nn_hls-1-(i1 -jpjglo)
         ENDIF
      ENDIF

   END FUNCTION agrif_external_switch_index

   SUBROUTINE Correct_field(tab2d,i1,i2,j1,j2)
      !!----------------------------------------------------------------------
      !!                   *** ROUTINE Correct_field ***
      !!----------------------------------------------------------------------
      USE dom_oce
      USE agrif_oce
      !
      IMPLICIT NONE
      !
      INTEGER :: i1,i2,j1,j2
      REAL(wp), DIMENSION(i1:i2,j1:j2) :: tab2d
      !
      INTEGER :: i,j
      REAL(wp), DIMENSION(i1:i2,j1:j2) :: tab2dtemp
      !!----------------------------------------------------------------------

      tab2dtemp = tab2d

      IF( .NOT. use_sign_north ) THEN
         DO j=j1,j2
            DO i=i1,i2
               tab2d(i,j)=tab2dtemp(i2-(i-i1),j2-(j-j1))
            END DO
         END DO
      ELSE
         DO j=j1,j2
            DO i=i1,i2
               tab2d(i,j)=sign_north * tab2dtemp(i2-(i-i1),j2-(j-j1))
            END DO
         END DO
      ENDIF

   END SUBROUTINE Correct_field

#else
   SUBROUTINE Subcalledbyagrif
      !!----------------------------------------------------------------------
      !!                   *** ROUTINE Subcalledbyagrif ***
      !!----------------------------------------------------------------------
      WRITE(*,*) 'Impossible to be here'
   END SUBROUTINE Subcalledbyagrif
#endif
