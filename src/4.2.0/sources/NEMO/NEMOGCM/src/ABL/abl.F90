MODULE abl
   !!======================================================================
   !!                      ***  MODULE  abl  ***
   !! Abl        :  ABL dynamics and active tracers defined in memory 
   !!======================================================================
   !! History :  4.0  !  2019-03  (F. Lemarié & G. Samson)  Original code
   !!----------------------------------------------------------------------
   USE par_abl        ! abl parameters
   USE lib_mpp        ! MPP library
   USE dom_oce, ONLY: glamt, gphit               ! latitude/longitude
   USE dom_oce, ONLY: e1t, e1u, e1v, e1f         ! scale factors for horizontal grid
   USE dom_oce, ONLY: e2t, e2u, e2v, e2f         ! 
   USE dom_oce, ONLY: rn_Dt                      ! oceanic time-step
   USE sbc_oce, ONLY: ght_abl, ghw_abl, e3t_abl, e3w_abl, jpka   ! scale factors and altitudes of ABL grid points in the vertical 
  
   IMPLICIT NONE
   PRIVATE

   PUBLIC abl_alloc ! routine called by nemo_init in nemogcm.F90

   !! ABL dynamics and tracer fields                            ! 
   !! --------------------------                            ! 
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:,:)   ::   u_abl        !: i-horizontal velocity   [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:,:)   ::   v_abl        !: j-horizontal velocity   [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:,:,:) ::   tq_abl       !: 4D T-q fields           [Kelvin,kg/kg] 
   
   !! ABL TKE closure scheme                            ! 
   !! --------------------------        
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:)     ::   avm_abl      !: turbulent viscosity   [m2/s]
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:)     ::   avt_abl      !: turbulent diffusivity [m2/s]
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:)     ::   mxld_abl     !: dissipative mixing length    [m]
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:)     ::   mxlm_abl     !: master mixing length         [m]
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:,:)   ::   tke_abl      !: turbulent kinetic energy [m2/s2]
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:)       ::   fft_abl      !: Coriolis parameter    [1/s]
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:)       ::   pblh         !: PBL height            [m]

   !! ABL Land/sea mask and restoring term                           ! 
   !! --------------------------     
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:)       ::   msk_abl
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:)       ::   rest_eq
   !
   INTEGER , PUBLIC :: nt_n, nt_a       !: now / after indices (equal 1 or 2)
   ! 
   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id: abl.F90 4990 2014-12-15 16:42:49Z timgraham $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION abl_alloc()
      !!----------------------------------------------------------------------
      !!                   ***  FUNCTION abl_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER :: ierr
      !!----------------------------------------------------------------------
      !
      ALLOCATE( u_abl   (1:jpi,1:jpj,1:jpka,jptime     ), &
         &      v_abl   (1:jpi,1:jpj,1:jpka,jptime     ), &
         &      tq_abl  (1:jpi,1:jpj,1:jpka,jptime,jptq), &
         &      tke_abl (1:jpi,1:jpj,1:jpka,jptime     ), &
         &      avm_abl (1:jpi,1:jpj,1:jpka            ), &
         &      avt_abl (1:jpi,1:jpj,1:jpka            ), &
         &      mxld_abl(1:jpi,1:jpj,1:jpka            ), &
         &      mxlm_abl(1:jpi,1:jpj,1:jpka            ), &
         &      fft_abl (1:jpi,1:jpj                   ), &
         &      pblh    (1:jpi,1:jpj                   ), &
         &      msk_abl (1:jpi,1:jpj                   ), &
         &      rest_eq (1:jpi,1:jpj                   ), &
         &      e3t_abl (1:jpka), e3w_abl(1:jpka)       , &
         &      ght_abl (1:jpka), ghw_abl(1:jpka)       , STAT=ierr )
         !
      abl_alloc = ierr
      IF( abl_alloc /= 0 )   CALL ctl_warn('abl_alloc: failed to allocate arrays')
      !
   END FUNCTION abl_alloc

   !!======================================================================
END MODULE abl
