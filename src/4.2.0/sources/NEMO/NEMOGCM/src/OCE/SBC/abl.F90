MODULE abl
   !!======================================================================
   !!                      ***  MODULE  abl  ***
   !! Abl        :  ABL dynamics and active tracers defined in memory
   !!======================================================================
   USE par_kind        ! abl parameters

   IMPLICIT NONE
   PRIVATE
   !! --------------------------                            !
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:,:)   ::   u_abl        !: i-horizontal velocity   [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:,:)   ::   v_abl        !: j-horizontal velocity   [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:,:,:) ::   tq_abl       !: 4D T-q fields           [Kelvin,kg/kg]
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:)     ::   avm_abl      !: turbulent viscosity   [m2/s]
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:)     ::   avt_abl      !: turbulent diffusivity [m2/s]
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:)     ::   mxl_abl      !: mixing length         [m]
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:,:)   ::   tke_abl      !: turbulent kinetic energy [m2/s2]
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:)       ::   fft_abl      !: Coriolis parameter    [1/s]
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:)       ::   pblh         !: PBL height            [m]
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

END MODULE abl
