MODULE agrif_oce
   !!======================================================================
   !!                       ***  MODULE agrif_oce  ***
   !! AGRIF :   define in memory AGRIF variables
   !!----------------------------------------------------------------------
   !! History :  2.0  ! 2007-12  (R. Benshila)  Original code
   !!----------------------------------------------------------------------
#if defined key_agrif
   !!----------------------------------------------------------------------
   !!   'key_agrif'                                              AGRIF zoom
   !!----------------------------------------------------------------------
   USE par_oce      ! ocean parameters
   USE dom_oce      ! domain parameters

   IMPLICIT NONE
   PRIVATE 

   PUBLIC agrif_oce_alloc ! routine called by nemo_init in nemogcm.F90
  
   !                                              !!* Namelist namagrif: AGRIF parameters
   LOGICAL , PUBLIC ::   ln_init_chfrpar = .FALSE. !: set child grids initial state from parent
   LOGICAL , PUBLIC ::   ln_agrif_2way = .TRUE.    !: activate two way nesting 
   LOGICAL , PUBLIC ::   ln_spc_dyn    = .FALSE.   !: use zeros (.false.) or not (.true.) in
                                                   !: bdys dynamical fields interpolation
   LOGICAL , PUBLIC ::   ln_vert_remap = .FALSE.   !: use vertical remapping
   REAL(wp), PUBLIC ::   rn_sponge_tra = 0.002     !: sponge coeff. for tracers
   REAL(wp), PUBLIC ::   rn_sponge_dyn = 0.002     !: sponge coeff. for dynamics
   REAL(wp), PUBLIC ::   rn_trelax_tra = 0.01      !: time relaxation parameter for tracers
   REAL(wp), PUBLIC ::   rn_trelax_dyn = 0.01      !: time relaxation parameter for momentum
   REAL(wp), PUBLIC ::   rn_hcri       = 0.05      !: minimum thickness (m) for flux blocking 

   LOGICAL , PUBLIC ::   ln_chk_bathy  = .FALSE.   !: check of parent bathymetry 
   !
   LOGICAL , PUBLIC ::   l_spc_tra    = .TRUE.     !: turn on extrapolation for active tracers
   LOGICAL , PUBLIC ::   l_spc_ssh    = .TRUE.     !: turn on extrapolation for ssh
   LOGICAL , PUBLIC ::   l_spc_top    = .TRUE.     !: turn on extrapolation for passive tracers
   !
   INTEGER , PUBLIC, PARAMETER ::   nn_sponge_len = 2 !: Sponge width (in number of parent grid points)
   INTEGER , PUBLIC, PARAMETER ::   nn_shift_bar  = 0 !: nb of coarse grid points by which we shift 2d interface
   INTEGER , PUBLIC, PARAMETER ::   nn_dist_par_bc= 7 !: position of parent open boundary from dynamlical interface (2d mode bdy) 

   LOGICAL , PUBLIC :: spongedoneT = .FALSE.       !: tracer   sponge layer indicator
   LOGICAL , PUBLIC :: spongedoneU = .FALSE.       !: dynamics sponge layer indicator
   LOGICAL , PUBLIC :: lk_agrif_fstep = .TRUE.     !: if true: first step
   LOGICAL , PUBLIC :: lk_agrif_debug = .FALSE.    !: if true: print debugging info
   LOGICAL , PUBLIC :: lk_div_cons    = .TRUE.     !: if true, volume conserving formulation in ghost zone
   LOGICAL , PUBLIC :: lk_tint2d_constant = .FALSE. !: Constant, conservative temporal interpolation of barotropic fluxes
   LOGICAL , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) :: tabspongedone_tsn
# if defined key_top
   LOGICAL , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) :: tabspongedone_trn
# endif
   LOGICAL , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) :: tabspongedone_u
   LOGICAL , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) :: tabspongedone_v
   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) :: utint_stage
   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) :: vtint_stage
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) :: fspu, fspv !: sponge arrays
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) :: fspt, fspf !:   "      "
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) :: fspu_2d,fspv_2d  !: sponge arrays (2d mode)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) :: fspt_2d, fspf_2d !:   "       "     "   "

   ! Barotropic arrays used to store open boundary data during time-splitting loop:
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  ubdy, vbdy, hbdy
   INTEGER , PUBLIC,              SAVE                 ::  Kbb_a, Kmm_a, Krhs_a   !: AGRIF module-specific copies of time-level indices

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   :: ht0_parent, hu0_parent, hv0_parent
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   :: e1e2t_frac, e2u_frac, e1v_frac 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: e3t0_parent, e3u0_parent, e3v0_parent
   INTEGER,  PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   :: mbkt_parent, mbku_parent, mbkv_parent


   INTEGER, PUBLIC :: ts_interp_id, ts_update_id                              ! AGRIF profile for tracers interpolation and update
   INTEGER, PUBLIC :: un_interp_id, vn_interp_id                              ! AGRIF profiles for interpolations
   INTEGER, PUBLIC :: un_update_id, vn_update_id                              ! AGRIF profiles for udpates
   INTEGER, PUBLIC :: ts_sponge_id, un_sponge_id, vn_sponge_id                ! AGRIF profiles for sponge layers (3d)
   INTEGER, PUBLIC :: unb_sponge_id, vnb_sponge_id                            ! AGRIF profiles for sponge layers (2d)
   INTEGER, PUBLIC :: tsini_id, uini_id, vini_id, sshini_id                   ! AGRIF profile for initialization
# if defined key_top
   INTEGER, PUBLIC :: trn_id, trn_sponge_id
# endif  
   INTEGER, PUBLIC :: unb_interp_id, vnb_interp_id, ub2b_interp_id, vb2b_interp_id
   INTEGER, PUBLIC :: ub2b_update_id, vb2b_update_id, unb_update_id, vnb_update_id
   INTEGER, PUBLIC :: ub2b_cor_id, vb2b_cor_id, sshn_id
   INTEGER, PUBLIC :: sshn_frc_id
   INTEGER, PUBLIC :: e3t_id, e3u_id, e3v_id, e3f_id
   INTEGER, PUBLIC :: r3t_id, r3u_id, r3v_id, r3f_id
   INTEGER, PUBLIC :: scales_t_id
   INTEGER, PUBLIC :: avt_id, avm_id, en_id                ! TKE related identificators
   INTEGER, PUBLIC :: mbkt_id, ht0_id, e3t0_interp_id
   INTEGER, PUBLIC :: e1e2t_frac_id, e2u_frac_id, e1v_frac_id 
   INTEGER, PUBLIC :: glamt_id, gphit_id
   INTEGER, PUBLIC :: kindic_agr

! Variables shared among grids:
!$AGRIF_DO_NOT_TREAT
   LOGICAL, PUBLIC :: use_sign_north
   REAL, PUBLIC    :: sign_north
   LOGICAL, PUBLIC :: l_ini_child = .FALSE.
   LOGICAL, PUBLIC :: l_vremap    = .FALSE.
!$AGRIF_END_DO_NOT_TREAT
   
   !!----------------------------------------------------------------------
   !! NEMO/NST 4.0 , NEMO Consortium (2018)
   !! $Id: agrif_oce.F90 14086 2020-12-04 11:37:14Z cetlod $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS 

   INTEGER FUNCTION agrif_oce_alloc()
      !!----------------------------------------------------------------------
      !!                ***  FUNCTION agrif_oce_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER, DIMENSION(2) :: ierr
      !!----------------------------------------------------------------------
      ierr(:) = 0
      !
      ALLOCATE( fspu(jpi,jpj), fspv(jpi,jpj),    	    &
         &      fspt(jpi,jpj), fspf(jpi,jpj),               &
         &      fspu_2d(jpi,jpj), fspv_2d(jpi,jpj),         &
         &      fspt_2d(jpi,jpj), fspf_2d(jpi,jpj),         &
         &      tabspongedone_tsn(jpi,jpj),                 &
         &      utint_stage(jpi,jpj), vtint_stage(jpi,jpj), &
# if defined key_top         
         &      tabspongedone_trn(jpi,jpj),                 &
# endif   
         &      ht0_parent(jpi,jpj), mbkt_parent(jpi,jpj),  &
         &      hu0_parent(jpi,jpj), mbku_parent(jpi,jpj),  &
         &      hv0_parent(jpi,jpj), mbkv_parent(jpi,jpj),  &
         &      e1e2t_frac(jpi,jpj),                        &
         &      e2u_frac(jpi,jpj),   e1v_frac(jpi,jpj),     &
         &      tabspongedone_u  (jpi,jpj),                 &
         &      tabspongedone_v  (jpi,jpj), STAT = ierr(1) )

      ALLOCATE( ubdy(jpi,jpj), vbdy(jpi,jpj), hbdy(jpi,jpj), STAT = ierr(2) )

      agrif_oce_alloc = MAXVAL(ierr)
      !
   END FUNCTION agrif_oce_alloc
#endif
   !!======================================================================
END MODULE agrif_oce
