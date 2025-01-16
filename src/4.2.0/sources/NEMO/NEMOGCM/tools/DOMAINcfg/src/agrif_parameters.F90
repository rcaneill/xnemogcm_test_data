MODULE agrif_parameters
	
   USE par_kind

   PUBLIC 

#if defined key_agrif
        LOGICAL :: ln_remove_closedseas=.FALSE.
        LOGICAL :: ln_vert_remap=.FALSE. ! =T is using volume conserving update
	INTEGER :: npt_copy      ! area (in coarse grid points) with piecewise
                                 ! constant bathymetry inside child zoom: should equal the sponge length
	INTEGER :: npt_connect   ! area (in coarse grid points) of coarse/child
                                 ! bathymetry blending
        INTEGER, PARAMETER :: npt_shift_bar = 2

	REAL(wp), PUBLIC, ALLOCATABLE, SAVE        , DIMENSION(:,:) ::   ztabramp
        REAL(wp), PUBLIC, ALLOCATABLE, SAVE        , DIMENSION(:,:) ::   e1e2t_frac
        REAL(wp), PUBLIC, ALLOCATABLE, SAVE        , DIMENSION(:,:) ::   e2u_frac
        REAL(wp), PUBLIC, ALLOCATABLE, SAVE        , DIMENSION(:,:) ::   e1v_frac
	LOGICAL,  PUBLIC, ALLOCATABLE, SAVE        , DIMENSION(:,:) ::   e3t_interp_done
!$AGRIF_DO_NOT_TREAT
        LOGICAL, PUBLIC :: l_set_hmin    = .FALSE.
!$AGRIF_END_DO_NOT_TREAT
#endif

END MODULE agrif_parameters
