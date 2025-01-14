#include "xios_fortran_prefix.hpp"

MODULE IMEM_CHECKER
   USE, INTRINSIC :: ISO_C_BINDING
   USE LOGICAL_BOOL_CONVERSION
   
   INTERFACE ! Ne pas appeler directement/Interface FORTRAN 2003 <-> C99
      
      SUBROUTINE cxios_mem_checker_get(mem) BIND(C)
         USE ISO_C_BINDING
         REAL     (kind = C_DOUBLE), VALUE        :: mem
      END SUBROUTINE cxios_mem_checker_get

      SUBROUTINE cxios_mem_checker_log(memid, memid_size, finalize) BIND(C)
         USE ISO_C_BINDING
         CHARACTER(kind = C_CHAR)  , DIMENSION(*) :: memid
         INTEGER  (kind = C_INT)   , VALUE        :: memid_size
         LOGICAL  (kind = C_BOOL)                 :: finalize
      END SUBROUTINE cxios_mem_checker_log
       
   END INTERFACE

   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   FUNCTION xios(mem_checker_get)() RESULT(mem)
      IMPLICIT NONE
      REAL                                       :: mem
      REAL(KIND=C_DOUBLE)                        :: mem_
      
      CALL cxios_mem_checker_get(mem_)
      mem=mem_
   END FUNCTION xios(mem_checker_get)
 
   SUBROUTINE xios(mem_checker_log)(mem_id, finalize)
   USE ISO_C_BINDING
      IMPLICIT NONE
      CHARACTER(len = *) , INTENT(IN) :: mem_id
      LOGICAL, OPTIONAL,INTENT(IN)    :: finalize
      LOGICAL (KIND=C_BOOL)           :: finalize_               

      finalize_=.FALSE.
      IF (PRESENT(finalize)) finalize_ = finalize
      CALL xios_logical_to_bool_0d(finalize_)
      CALL cxios_mem_checker_log(mem_id, len(mem_id), finalize_)

   END SUBROUTINE xios(mem_checker_log)

END MODULE IMEM_CHECKER
