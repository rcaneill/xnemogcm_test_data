MODULE par_age
   !!======================================================================
   !!                        ***  par_age  ***
   !! TOP :   set the AGE parameters
   !!======================================================================
   !! History :   2.0  !  2007-12  (C. Ethe, G. Madec)  revised architecture
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: par_age.F90 8353 2017-07-19 14:41:00Z lovato $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
   USE par_pisces , ONLY : jp_pisces       !: number of tracers in PISCES
   USE par_pisces , ONLY : jp_pisces_2d    !: number of 2D diag in PISCES
   USE par_pisces , ONLY : jp_pisces_3d    !: number of 3D diag in PISCES
   USE par_pisces , ONLY : jp_pisces_trd   !: number of biological diag in PISCES

   USE par_cfc    , ONLY : jp_cfc          !: number of tracers in CFC
   USE par_cfc    , ONLY : jp_cfc_2d       !: number of tracers in CFC
   USE par_cfc    , ONLY : jp_cfc_3d       !: number of tracers in CFC
   USE par_cfc    , ONLY : jp_cfc_trd      !: number of tracers in CFC

   USE par_c14b   , ONLY : jp_c14b         !: number of tracers in C14
   USE par_c14b   , ONLY : jp_c14b_2d      !: number of tracers in C14
   USE par_c14b   , ONLY : jp_c14b_3d      !: number of tracers in C14
   USE par_c14b   , ONLY : jp_c14b_trd     !: number of tracers in C14

   USE par_my_trc , ONLY : jp_my_trc       !: number of tracers in MY_TRC
   USE par_my_trc , ONLY : jp_my_trc_2d    !: number of 2D diag in MY_TRC
   USE par_my_trc , ONLY : jp_my_trc_3d    !: number of 3D diag in MY_TRC
   USE par_my_trc , ONLY : jp_my_trc_trd   !: number of biological diag in MY_TRC

   IMPLICIT NONE

   INTEGER, PARAMETER ::   jp_lm      =  jp_pisces     + jp_my_trc     + jp_cfc     + jp_c14b     !: 
   INTEGER, PARAMETER ::   jp_lm_2d   =  jp_pisces_2d  + jp_my_trc_2d  + jp_cfc_2d  + jp_c14b_2d  !:
   INTEGER, PARAMETER ::   jp_lm_3d   =  jp_pisces_3d  + jp_my_trc_3d  + jp_cfc_3d  + jp_c14b_3d  !:
   INTEGER, PARAMETER ::   jp_lm_trd  =  jp_pisces_trd + jp_my_trc_trd + jp_cfc_trd + jp_c14b_trd !:

#if defined key_age
   !!---------------------------------------------------------------------
   !!   'key_age'                     user defined tracers (AGE)
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_age     = .TRUE.   !: PTS flag 
   INTEGER, PUBLIC, PARAMETER ::   jp_age     =  1       !: number of PTS tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_age_2d  =  0       !: additional 2d output arrays ('key_trc_diaadd')
   INTEGER, PUBLIC, PARAMETER ::   jp_age_3d  =  0       !: additional 3d output arrays ('key_trc_diaadd')
   INTEGER, PUBLIC, PARAMETER ::   jp_age_trd =  0       !: number of sms trends for AGE

   ! assign an index in trc arrays for each PTS prognostic variables
   INTEGER, PUBLIC, PARAMETER ::   jpage1 = jp_lm + 1     !: 1st AGE tracer

#else
   !!---------------------------------------------------------------------
   !!   Default                           No user defined tracers (AGE)
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_age     = .FALSE.  !: AGE flag 
   INTEGER, PUBLIC, PARAMETER ::   jp_age     =  0       !: No AGE tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_age_2d  =  0       !: No AGE additional 2d output arrays 
   INTEGER, PUBLIC, PARAMETER ::   jp_age_3d  =  0       !: No AGE additional 3d output arrays 
   INTEGER, PUBLIC, PARAMETER ::   jp_age_trd =  0       !: number of sms trends for AGE
#endif

   ! Starting/ending PISCES do-loop indices (N.B. no PISCES : jpl_pcs < jpf_pcs the do-loop are never done)
   INTEGER, PUBLIC, PARAMETER ::   jp_age0     = jp_lm     + 1              !: First index of AGE passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_age1     = jp_lm     + jp_age      !: Last  index of AGE passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_age0_2d  = jp_lm_2d  + 1              !: First index of AGE passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_age1_2d  = jp_lm_2d  + jp_age_2d   !: Last  index of AGE passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_age0_3d  = jp_lm_3d  + 1              !: First index of AGE passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_age1_3d  = jp_lm_3d  + jp_age_3d   !: Last  index of AGE passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_age0_trd = jp_lm_trd + 1              !: First index of AGE passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_age1_trd = jp_lm_trd + jp_age_trd  !: Last  index of AGE passive tracers

   !!======================================================================
END MODULE par_age
