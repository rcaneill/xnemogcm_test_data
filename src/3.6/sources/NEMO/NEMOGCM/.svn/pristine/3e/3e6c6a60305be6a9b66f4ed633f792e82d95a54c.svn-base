MODULE thd_ice_2
#if defined key_lim2
   !!======================================================================
   !!                       ***  MODULE thd_ice_2  ***
   !! LIM 2.0 sea-ice :   Ice thermodynamics in 1D
   !!=====================================================================
   !! History :
   !!   2.0  !  02-11  (C. Ethe)  F90: Free form and module
   !!----------------------------------------------------------------------
   !! NEMO/LIM2 3.3 , UCL - NEMO Consortium (2010)
   !! $Id$
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
   USE par_ice_2
   USE ice_2

   IMPLICIT NONE
   PRIVATE

   PUBLIC thd_ice_alloc_2 ! Routine called by nemogcm.F90

   !! * Share Module variables
   REAL(wp) , PUBLIC ::   & !!! ** ice-thermo namelist (namicethd) **
      hmelt            ,  &  !: maximum melting at the bottom
      hicmin           ,  &  !: ice th. corr. to max. ener. in brine pocket
      hiclim           ,  &  !: minimum ice thickness
      amax             ,  &  !: maximum lead fraction
      swiqst           ,  &  !: energy stored in brine pocket (1) or not (0)
      sbeta            ,  &  !: numerical scheme for diffusion in ice 
      parlat           ,  &  !: percent. of energy used for lateral ablation
      hakspl           ,  &  !: slope of distr. for Hakkinen-Mellro's lat. melt
      hibspl           ,  &  !: slope of distribution for Hibler's lat. melt
      exld             ,  &  !: exponent for leads-closure rate
      hakdif           ,  &  !: coefficient for diffusions of ice and snow
      thth             ,  &  !: thick. for comp. of eq. thermal conduct
      hnzst            ,  &  !: thick. of the surf. layer in temp. comp.
      parsub           ,  &  !: switch for snow sublimation or not
      alphs                  !: coef. for snow density when snow-ice formation

   REAL(wp), PUBLIC, DIMENSION(2)  ::  &  !:   
      hiccrit                !: ice th. for lateral accretion in the NH (SH) (m)

   REAL(wp) , PUBLIC ::   &  !:
      uscomi,             &  !: inverse of minimum lead fraction
      cnscg                  !: ratio  rcpsn/rcpic

   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   &  !:
      npb     ,   &   !: number of points where computations has to be done
      npac            !: correspondance between the points

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   &  !: 
      qldif_1d    ,     &  !: corresponding to the 2D var  qldif
      qcmif_1d    ,     &  !: corresponding to the 2D var  qcmif
      thcm_1d     ,     &  !:    "                  "      thcm
      fstbif_1d   ,     &  !:    "                  "      fstric
      fltbif_1d   ,     &  !:    "                  "      ffltbif
      fscbq_1d    ,     &  !:    "                  "      fscmcbq
      qsr_ice_1d  ,     &  !:    "                  "      qsr_ice
      fr1_i0_1d   ,     &  !:    "                  "      fr1_i0
      fr2_i0_1d   ,     &  !:    "                  "      fr2_i0
      qns_ice_1d  ,     &  !:    "                  "      qns_ice
      qfvbq_1d    ,     &  !:    "                  "      qfvbq
      sist_1d     ,     &  !:    "                  "      sist
      tfu_1d      ,     &  !:    "                  "      tfu
      sprecip_1d  ,     &  !:    "                  "      sprecip
      h_snow_1d   ,     &  !:    "                  "      h_snow
      h_ice_1d    ,     &  !:    "                  "      h_ice
      frld_1d     ,     &  !:    "                  "      frld
      qstbif_1d   ,     &  !:    "                  "      qstoif
      fbif_1d     ,     &  !:    "                  "      fbif
      rdm_ice_1d  ,     &  !:    "                  "      rdm_ice
      rdq_ice_1d  ,     &  !:    "                  "      rdq_ice
      rdm_snw_1d  ,     &  !:    "                  "      rdm_snw
      rdq_snw_1d  ,     &  !:    "                  "      rdq_snw
      qlbbq_1d    ,     &  !:    "                  "      qlbsbq
      dmgwi_1d    ,     &  !:    "                  "      dmgwi
      dvsbq_1d    ,     &  !:    "                  "      rdvosif
      rdvomif_1d  ,     &  !:    "                  "      rdvomif
      dvbbq_1d    ,     &  !:    "                  "      rdvobif
      dvlbq_1d    ,     &  !:    "                  "      rdvolif
      dvnbq_1d    ,     &  !:    "                  "      rdvolif
      dqns_ice_1d ,     &  !:    "                  "      dqns_ice
      qla_ice_1d  ,     &  !:    "                  "      qla_ice
      dqla_ice_1d          !:    "                  "      dqla_ice

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   &  !:
      tbif_1d              !: corresponding to the 2D var  tbif

   !!----------------------------------------------------------------------
   !! NEMO/LIM2 3.3 , UCL - NEMO Consortium (2010)
   !! $Id$
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
 CONTAINS

   INTEGER FUNCTION thd_ice_alloc_2()
      !!----------------------------------------------------------------------
      USE lib_mpp        ! MPP library
      INTEGER :: ierr(5)
      !!----------------------------------------------------------------------
      !
      ierr(:) = 0
      !
      ALLOCATE( npb(jpij), npac(jpij),                             &
         &      qldif_1d(jpij), qcmif_1d(jpij), thcm_1d(jpij),     &
         &      fstbif_1d(jpij), fltbif_1d(jpij), fscbq_1d(jpij),  &
         &      qsr_ice_1d(jpij),fr1_i0_1d(jpij), fr2_i0_1d(jpij), Stat=ierr(1))
         !
      ALLOCATE( qns_ice_1d(jpij), qfvbq_1d(jpij), sist_1d(jpij), tfu_1d(jpij), &
         &      sprecip_1d(jpij), h_snow_1d(jpij),h_ice_1d(jpij),frld_1d(jpij),&
         &      qstbif_1d(jpij),  fbif_1d(jpij),  Stat=ierr(2))
         !
      ALLOCATE( rdm_ice_1d(jpij), rdq_ice_1d(jpij)                  , &
         &      rdm_snw_1d(jpij), rdq_snw_1d(jpij), qlbbq_1d(jpij)  , &
         &      dmgwi_1d(jpij)  , dvsbq_1d(jpij)  , rdvomif_1d(jpij), &
         &      dvbbq_1d(jpij)  , dvlbq_1d(jpij)  , dvnbq_1d(jpij)  , &
         &      Stat=ierr(3))
         !
      ALLOCATE( dqns_ice_1d(jpij) ,qla_ice_1d(jpij), dqla_ice_1d(jpij), &
         &      tbif_1d(jpij, jplayersp1), Stat=ierr(4))
         !
      thd_ice_alloc_2 = MAXVAL(ierr)
      IF( thd_ice_alloc_2 /= 0 )   CALL ctl_warn('thd_ice_alloc_2: failed to allocate arrays')
      !
   END FUNCTION thd_ice_alloc_2

#endif
   !!======================================================================
END MODULE thd_ice_2
