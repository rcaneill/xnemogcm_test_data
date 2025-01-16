MODULE trdtrc
   USE par_kind
   !!======================================================================
   !!                       ***  MODULE trdtrc  ***
   !!  Dummy module
   !!======================================================================
   !!----------------------------------------------------------------------
   !!   Dummy module                                             NO TOP use
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trd_trc( ptrtrd, kjn, ktrd, kt, Kmm )
      INTEGER ::   kt, kjn, ktrd   
      INTEGER ::   Kmm            ! time level index
      REAL(wp)::   ptrtrd(:,:,:)  
      WRITE(*,*) 'trd_trc : You should not have seen this print! error?', ptrtrd(1,1,1)
      WRITE(*,*) '  "      "      : You should not have seen this print! error?', kjn, ktrd, kt
   END SUBROUTINE trd_trc

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: trdtrc.F90 13226 2020-07-02 14:24:31Z orioltp $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!======================================================================
END MODULE trdtrc
