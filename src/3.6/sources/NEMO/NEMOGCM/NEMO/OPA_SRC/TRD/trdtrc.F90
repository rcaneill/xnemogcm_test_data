MODULE trdtrc
   !!======================================================================
   !!                       ***  MODULE trdtrc  ***
   !!  Dummy module
   !!======================================================================
   !!----------------------------------------------------------------------
   !!   Dummy module                                             NO TOP use
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trd_trc( ptrtrd, kjn, ktrd, kt )
      INTEGER ::   kt, kjn, ktrd   
      REAL    ::   ptrtrd(:,:,:)  
      WRITE(*,*) 'trd_trc : You should not have seen this print! error?', ptrtrd(1,1,1)
      WRITE(*,*) '  "      "      : You should not have seen this print! error?', kjn, ktrd, kt
   END SUBROUTINE trd_trc

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: trdtrc.F90 5215 2015-04-15 16:11:56Z nicolasmartin $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!======================================================================
END MODULE trdtrc
