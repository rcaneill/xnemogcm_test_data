MODULE agrif_all_update
   !!======================================================================
   !!                   ***  MODULE  agrif_all_update  ***
   !! AGRIF: Main update driver for ocean, ice and passive tracers
   !!======================================================================
   !! History :  4.0  !  2018-06  (J. Chanut)  Original code 
   !!----------------------------------------------------------------------
#if defined key_agrif 
   !!----------------------------------------------------------------------
   !!   'key_agrif'                                              AGRIF zoom
   !!----------------------------------------------------------------------
   USE dom_oce
   USE agrif_oce
   USE agrif_oce_update
#if defined key_top
   USE agrif_top_update
#endif
#if defined key_si3
   USE agrif_ice_update
#endif

   IMPLICIT NONE
   PRIVATE

   PUBLIC   Agrif_Update_All

   !!----------------------------------------------------------------------
   !! NEMO/NST 4.0 , NEMO Consortium (2018)
   !! $Id: agrif_all_update.F90 15119 2021-07-13 14:43:22Z jchanut $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE Agrif_Update_All( )
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_Update_All ***
      !!
      !! ** Purpose :: Update nested grids for all components (Ocean, Sea Ice, TOP)
      !!               Order of update matters here !
      !!----------------------------------------------------------------------
      IF (( .NOT.ln_agrif_2way ).OR.(Agrif_Root())) RETURN
      !
      IF (lwp.AND.lk_agrif_debug) Write(*,*) ' --> START AGRIF UPDATE from grid Number',Agrif_Fixed()
      !
      CALL Agrif_Update_ssh()                      ! Update sea level
      !
      IF (.NOT.ln_linssh) CALL Agrif_Update_vvl()  ! Update scale factors
      !
      CALL Agrif_Update_tra()                      ! Update temperature/salinity
      !
#if defined key_top
      CALL Agrif_Update_Trc()                      ! Update passive tracers
#endif
      !
      CALL Agrif_Update_dyn()                      ! Update dynamics
      !
! JC remove update because this precludes from perfect restartability
!!      CALL Agrif_Update_tke()                  ! Update tke 

#if defined key_si3
      CALL agrif_update_ice()                      ! Update sea ice
#endif
      !
      Agrif_UseSpecialValueInUpdate = .FALSE.
      !
      ! If zooms are crossing or are coincident with cyclic boundaries
      ! need to update ghost points on parent edges:
      IF ( (Agrif_Parent(l_Iperio).OR.Agrif_Parent(l_NFold)).AND. &
         & (( Agrif_Ix() <= 1  ).OR.( Agrif_Iy() + nbcellsy/AGRIF_Irhoy() >=  Agrif_Parent(Nj0glo) - 1 ))) THEN
         CALL Agrif_ChildGrid_To_ParentGrid()
         CALL finalize_lbc_for_agrif 
         CALL Agrif_ParentGrid_To_ChildGrid()
      ENDIF

      IF (lwp.AND.lk_agrif_debug) Write(*,*) ' <-- END AGRIF UPDATE from grid Number',Agrif_Fixed()

   END SUBROUTINE agrif_Update_All

   SUBROUTINE finalize_lbc_for_agrif
      !!---------------------------------------------
      !!  *** ROUTINE finalize lbc_for-agrif ***
      !!---------------------------------------------
      USE lbclnk 
#if defined key_qco
      USE domqco
#endif
      !
      CALL lbc_lnk( 'finalize_lbc_for_agrif', uu(:,:,:,       Kmm_a), 'U', -1._wp,  &
           &                                  vv(:,:,:,       Kmm_a), 'V', -1._wp,  &
           &                                  uu(:,:,:,       Kbb_a), 'U', -1._wp,  &
           &                                  vv(:,:,:,       Kbb_a), 'V', -1._wp,  &
           &                                  ts(:,:,:,jp_tem,Kmm_a), 'T',  1._wp,  & 
           &                                  ts(:,:,:,jp_sal,Kmm_a), 'T',  1._wp,  & 
           &                                  ts(:,:,:,jp_tem,Kbb_a), 'T',  1._wp,  & 
           &                                  ts(:,:,:,jp_sal,Kbb_a), 'T', 1._wp    )
      CALL lbc_lnk( 'finalize_lbc_for_agrif', ssh(:,:,  Kmm_a), 'T', 1._wp, &
           &                                  ssh(:,:,  Kbb_a), 'T', 1._wp, &
           &                                  uu_b(:,:, Kmm_a), 'U',-1._wp, &
           &                                  uu_b(:,:, Kbb_a), 'U',-1._wp, &
           &                                  vv_b(:,:, Kmm_a), 'V',-1._wp, &
           &                                  vv_b(:,:, Kbb_a), 'V',-1._wp, &
           &                                  ub2_b(:,:),   'U',-1._wp,     &
           &                                  ub2_i_b(:,:), 'U',-1._wp,     &
           &                                  vb2_b(:,:),   'V',-1._wp,     &
           &                                  vb2_i_b(:,:), 'V',-1._wp      ) 

#if defined key_qco
      CALL dom_qco_zgr( Kbb_a, Kmm_a ) 
#endif
#if defined key_si3
      CALL lbc_lnk( 'finalize_lbc_for_agrif',  a_i, 'T',1._wp,  v_i,'T',1._wp,                 &
           &                                   v_s, 'T',1._wp, sv_i,'T',1._wp, oa_i,'T',1._wp, &
           &                                   a_ip,'T',1._wp, v_ip,'T',1._wp, v_il,'T',1._wp )
      CALL lbc_lnk( 'finalize_lbc_for_agrif', t_su,'T',1._wp )
      CALL lbc_lnk( 'finalize_lbc_for_agrif',  e_s,'T',1._wp )
      CALL lbc_lnk( 'finalize_lbc_for_agrif',  e_i,'T',1._wp )
      CALL lbc_lnk( 'finalize_lbc_for_agrif', u_ice, 'U', -1._wp, v_ice, 'V', -1._wp )
#endif
#if defined key_top
      CALL lbc_lnk( 'finalize_lbc_for_agrif', tr(:,:,:,:,Kmm_a), 'T',1._wp )
      CALL lbc_lnk( 'finalize_lbc_for_agrif', tr(:,:,:,:,Kbb_a), 'T',1._wp )
#endif
      !
   END SUBROUTINE finalize_lbc_for_agrif 

#else
   !!----------------------------------------------------------------------
   !!   Empty module                                          no AGRIF zoom
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE Agrif_Update_all( )
      WRITE(*,*)  'Agrif_Update_All : You should not have seen this print! error?'
   END SUBROUTINE Agrif_Update_all
#endif

   !!======================================================================
END MODULE agrif_all_update

