MODULE par_kind
   !!======================================================================
   !!                   ***  MODULE par_kind  ***
   !! Ocean :  define the kind of real for the whole model
   !!======================================================================
   !! History :  1.0  ! 2002-06  (G. Madec)  Original code
   !!            3.3  ! 2010-12  (G. Madec)  add a standard length of character strings
   !!----------------------------------------------------------------------

   IMPLICIT NONE
   PRIVATE

   ! Number model from which the SELECTED_*_KIND are requested:
   !             4 byte REAL       8 byte REAL
   ! CRAY:           -            precision = 13
   !                              exponent = 2465
   ! IEEE:      precision = 6     precision = 15
   !            exponent = 37     exponent = 307

   !                                                                !!** Floating point **
   INTEGER, PUBLIC, PARAMETER ::   sp = SELECTED_REAL_KIND( 6, 37)   !: single precision (real 4)
   INTEGER, PUBLIC, PARAMETER ::   dp = SELECTED_REAL_KIND(12,307)   !: double precision (real 8)
# if defined key_single
   INTEGER, PUBLIC, PARAMETER ::   wp = sp                              !: working precision
# else
   INTEGER, PUBLIC, PARAMETER ::   wp = dp                              !: working precision
# endif

   !                                                                !!** Integer **
   INTEGER, PUBLIC, PARAMETER ::   i4 = SELECTED_INT_KIND( 9)        !: single precision (integer 4)
   INTEGER, PUBLIC, PARAMETER ::   i8 = SELECTED_INT_KIND(14)        !: double precision (integer 8)
   
   !                                                                !!** Integer **
   INTEGER, PUBLIC, PARAMETER ::   lc  = 256                          !: Lenght of Character strings
   INTEGER, PUBLIC, PARAMETER ::   lca = 400                          !: Lenght of Character arrays

   !!----------------------------------------------------------------------
   !! NEMO 3.3 , NEMO Consortium (2018)
   !! $Id: par_kind.F90 14433 2021-02-11 08:06:49Z smasson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
END MODULE par_kind
