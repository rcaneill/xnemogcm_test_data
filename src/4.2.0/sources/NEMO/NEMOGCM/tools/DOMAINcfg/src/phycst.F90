MODULE phycst
   !!======================================================================
   !!                    ***  MODULE  phycst  ***
   !!     Definition of of both ocean and ice parameters used in the code
   !!=====================================================================
   !! History :   OPA  !  1990-10  (C. Levy - G. Madec)  Original code
   !!             8.1  !  1991-11  (G. Madec, M. Imbard)  cosmetic changes
   !!   NEMO      1.0  !  2002-08  (G. Madec, C. Ethe)  F90, add ice constants
   !!              -   !  2006-08  (G. Madec)  style 
   !!             3.2  !  2006-08  (S. Masson, G. Madec)  suppress useless variables + style 
   !!             3.4  !  2011-11  (C. Harris)  minor changes for CICE constants 
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   phy_cst  : define and print physical constant and domain parameters
   !!----------------------------------------------------------------------
   USE par_oce          ! ocean parameters
   USE in_out_manager   ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   phy_cst     ! routine called by inipar.F90

   REAL(wp), PUBLIC ::   rpi      = 3.141592653589793_wp             !: pi
   REAL(wp), PUBLIC ::   rad      = 3.141592653589793_wp / 180._wp   !: conversion from degre into radian
   REAL(wp), PUBLIC ::   rsmall   = 0.5 * EPSILON( 1.e0 )            !: smallest real computer value
   
   REAL(wp), PUBLIC ::   rday     = 24.*60.*60.      !: day                                [s]
   REAL(wp), PUBLIC ::   rsiyea                      !: sideral year                       [s]
   REAL(wp), PUBLIC ::   rsiday                      !: sideral day                        [s]
   REAL(wp), PUBLIC ::   raamo    =  12._wp          !: number of months in one year
   REAL(wp), PUBLIC ::   rjjhh    =  24._wp          !: number of hours in one day
   REAL(wp), PUBLIC ::   rhhmm    =  60._wp          !: number of minutes in one hour
   REAL(wp), PUBLIC ::   rmmss    =  60._wp          !: number of seconds in one minute
   REAL(wp), PUBLIC ::   omega                       !: earth rotation parameter           [s-1]
   REAL(wp), PUBLIC ::   ra       = 6371229._wp      !: earth radius                       [m]
   REAL(wp), PUBLIC ::   grav     = 9.80665_wp       !: gravity                            [m/s2]   

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: phycst.F90 10068 2018-08-28 14:09:04Z nicolasmartin $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
   
CONTAINS
   
   SUBROUTINE phy_cst
      !!----------------------------------------------------------------------
      !!                       ***  ROUTINE phy_cst  ***
      !!
      !! ** Purpose :   set and print the constants
      !!----------------------------------------------------------------------

      rsiyea = 365.25_wp * rday * 2._wp * rpi / 6.283076_wp
      rsiday = rday / ( 1._wp + rday / rsiyea )
#if defined key_cice
      omega  = 7.292116e-05
#else
      omega  = 2._wp * rpi / rsiday 
#endif

   END SUBROUTINE phy_cst

   !!======================================================================
END MODULE phycst
