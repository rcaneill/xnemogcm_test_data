MODULE par_my_trc
   !!======================================================================
   !!                        ***  par_my_trc  ***
   !! TOP :   set the MY_TRC parameters
   !!======================================================================
   !! History :   2.0  !  2007-12  (C. Ethe, G. Madec)  revised architecture
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: par_my_trc.F90 8353 2017-07-19 14:41:00Z lovato $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
   USE par_pisces , ONLY : jp_pisces       !: number of tracers in PISCES
   USE par_pisces , ONLY : jp_pisces_2d    !: number of 2D diag in PISCES
   USE par_pisces , ONLY : jp_pisces_3d    !: number of 3D diag in PISCES
   USE par_pisces , ONLY : jp_pisces_trd   !: number of biological diag in PISCES

   IMPLICIT NONE

   INTEGER, PARAMETER ::   jp_lm      =  jp_pisces     !: 
   INTEGER, PARAMETER ::   jp_lm_2d   =  jp_pisces_2d  !:
   INTEGER, PARAMETER ::   jp_lm_3d   =  jp_pisces_3d  !:
   INTEGER, PARAMETER ::   jp_lm_trd  =  jp_pisces_trd !:

#if defined key_my_trc
   !!---------------------------------------------------------------------
   !!   'key_my_trc'                     user defined tracers (MY_TRC)
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_my_trc     = .TRUE.   !: PTS flag 
   INTEGER, PUBLIC, PARAMETER ::   jp_my_trc     =  1       !: number of PTS tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_my_trc_2d  =  0       !: additional 2d output arrays ('key_trc_diaadd')
   INTEGER, PUBLIC, PARAMETER ::   jp_my_trc_3d  =  0       !: additional 3d output arrays ('key_trc_diaadd')
   INTEGER, PUBLIC, PARAMETER ::   jp_my_trc_trd =  0       !: number of sms trends for MY_TRC

   ! assign an index in trc arrays for each PTS prognostic variables
   INTEGER, PUBLIC, PARAMETER ::   jpmyt1 = jp_lm + 1     !: 1st MY_TRC tracer

#else
   !!---------------------------------------------------------------------
   !!   Default                           No user defined tracers (MY_TRC)
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_my_trc     = .FALSE.  !: MY_TRC flag 
   INTEGER, PUBLIC, PARAMETER ::   jp_my_trc     =  0       !: No MY_TRC tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_my_trc_2d  =  0       !: No MY_TRC additional 2d output arrays 
   INTEGER, PUBLIC, PARAMETER ::   jp_my_trc_3d  =  0       !: No MY_TRC additional 3d output arrays 
   INTEGER, PUBLIC, PARAMETER ::   jp_my_trc_trd =  0       !: number of sms trends for MY_TRC
#endif

   ! Starting/ending PISCES do-loop indices (N.B. no PISCES : jpl_pcs < jpf_pcs the do-loop are never done)
   INTEGER, PUBLIC, PARAMETER ::   jp_myt0     = jp_lm     + 1              !: First index of MY_TRC passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_myt1     = jp_lm     + jp_my_trc      !: Last  index of MY_TRC passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_myt0_2d  = jp_lm_2d  + 1              !: First index of MY_TRC passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_myt1_2d  = jp_lm_2d  + jp_my_trc_2d   !: Last  index of MY_TRC passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_myt0_3d  = jp_lm_3d  + 1              !: First index of MY_TRC passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_myt1_3d  = jp_lm_3d  + jp_my_trc_3d   !: Last  index of MY_TRC passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_myt0_trd = jp_lm_trd + 1              !: First index of MY_TRC passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_myt1_trd = jp_lm_trd + jp_my_trc_trd  !: Last  index of MY_TRC passive tracers

   !!======================================================================
END MODULE par_my_trc
