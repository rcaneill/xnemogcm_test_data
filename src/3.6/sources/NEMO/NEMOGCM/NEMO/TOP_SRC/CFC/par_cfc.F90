MODULE par_cfc
   !!======================================================================
   !!                        ***  par_cfc  ***
   !! TOP :   set the CFC parameters
   !!======================================================================
   !! History :   2.0  !  2007-12  (C. Ethe, G. Madec)  revised architecture
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: par_cfc.F90 8353 2017-07-19 14:41:00Z lovato $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
   USE par_pisces , ONLY : jp_pisces       !: number of tracers in PISCES
   USE par_pisces , ONLY : jp_pisces_2d    !: number of 2D diag in PISCES
   USE par_pisces , ONLY : jp_pisces_3d    !: number of 3D diag in PISCES
   USE par_pisces , ONLY : jp_pisces_trd   !: number of biological diag in PISCES
   
   USE par_my_trc , ONLY : jp_my_trc       !: number of tracers in MY_TRC
   USE par_my_trc , ONLY : jp_my_trc_2d    !: number of 2D diag in MY_TRC
   USE par_my_trc , ONLY : jp_my_trc_3d    !: number of 3D diag in MY_TRC
   USE par_my_trc , ONLY : jp_my_trc_trd   !: number of biological diag in MY_TRC

   IMPLICIT NONE

   INTEGER, PARAMETER ::   jp_lc      =  jp_pisces     + jp_my_trc     !: cumulative number of passive tracers
   INTEGER, PARAMETER ::   jp_lc_2d   =  jp_pisces_2d  + jp_my_trc_2d  !:
   INTEGER, PARAMETER ::   jp_lc_3d   =  jp_pisces_3d  + jp_my_trc_3d  !:
   INTEGER, PARAMETER ::   jp_lc_trd  =  jp_pisces_trd + jp_my_trc_trd !:
   
#if defined key_cfc
   !!---------------------------------------------------------------------
   !!   'key_cfc'   :                                          CFC tracers
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_cfc     = .TRUE.      !: CFC flag 
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc     =  2          !: number of passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc_2d  =  2 * jp_cfc !: additional 2d output arrays ('key_trc_diaadd')
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc_3d  =  0          !: additional 3d output arrays ('key_trc_diaadd')
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc_trd =  0          !: number of sms trends for CFC
   ! Enable trace gases according to total number jp_cfc
   LOGICAL, PUBLIC, PARAMETER ::   lp_cfc11   = .false.     !: use CFC11
   LOGICAL, PUBLIC, PARAMETER ::   lp_cfc12   = .true.      !: use CFC12
   LOGICAL, PUBLIC, PARAMETER ::   lp_sf6     = .true.      !: use SF6
#else
   !!---------------------------------------------------------------------
   !!   Default     :                                       No CFC tracers
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_cfc     = .FALSE.     !: CFC flag 
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc     =  0          !: No CFC tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc_2d  =  0          !: No CFC additional 2d output arrays 
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc_3d  =  0          !: No CFC additional 3d output arrays 
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc_trd =  0          !: number of sms trends for CFC
   LOGICAL, PUBLIC, PARAMETER ::   lp_cfc11   = .false.     !: use CFC11
   LOGICAL, PUBLIC, PARAMETER ::   lp_cfc12   = .false.     !: use CFC12
   LOGICAL, PUBLIC, PARAMETER ::   lp_sf6     = .false.     !: use SF6
#endif

   ! Starting/ending CFC do-loop indices (N.B. no CFC : jp_cfc0 > jp_cfc1 the do-loop are never done)
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc0     = jp_lc + 1       !: First index of CFC tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc1     = jp_lc + jp_cfc  !: Last  index of CFC tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc0_2d  = jp_lc_2d  + 1       !: First index of CFC tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc1_2d  = jp_lc_2d  + jp_cfc_2d  !: Last  index of CFC tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc0_3d  = jp_lc_3d  + 1       !: First index of CFC tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc1_3d  = jp_lc_3d  + jp_cfc_3d  !: Last  index of CFC tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc0_trd = jp_lc_trd + 1       !: First index of CFC tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc1_trd = jp_lc_trd + jp_cfc_trd  !: Last  index of CFC tracers

   !!======================================================================
END MODULE par_cfc
