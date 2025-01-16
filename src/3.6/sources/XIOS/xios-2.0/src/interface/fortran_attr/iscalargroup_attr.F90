! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE iscalargroup_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE iscalar
  USE scalargroup_interface_attr

CONTAINS

  SUBROUTINE xios(set_scalargroup_attr)  &
    ( scalargroup_id, group_ref, long_name, name, prec, scalar_ref, standard_name, unit, value )

    IMPLICIT NONE
      TYPE(txios(scalargroup))  :: scalargroup_hdl
      CHARACTER(LEN=*), INTENT(IN) ::scalargroup_id
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      INTEGER  , OPTIONAL, INTENT(IN) :: prec
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: scalar_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: value

      CALL xios(get_scalargroup_handle) &
      (scalargroup_id,scalargroup_hdl)
      CALL xios(set_scalargroup_attr_hdl_)   &
      ( scalargroup_hdl, group_ref, long_name, name, prec, scalar_ref, standard_name, unit, value  &
       )

  END SUBROUTINE xios(set_scalargroup_attr)

  SUBROUTINE xios(set_scalargroup_attr_hdl)  &
    ( scalargroup_hdl, group_ref, long_name, name, prec, scalar_ref, standard_name, unit, value  &
     )

    IMPLICIT NONE
      TYPE(txios(scalargroup)) , INTENT(IN) :: scalargroup_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      INTEGER  , OPTIONAL, INTENT(IN) :: prec
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: scalar_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: value

      CALL xios(set_scalargroup_attr_hdl_)  &
      ( scalargroup_hdl, group_ref, long_name, name, prec, scalar_ref, standard_name, unit, value  &
       )

  END SUBROUTINE xios(set_scalargroup_attr_hdl)

  SUBROUTINE xios(set_scalargroup_attr_hdl_)   &
    ( scalargroup_hdl, group_ref_, long_name_, name_, prec_, scalar_ref_, standard_name_, unit_  &
    , value_ )

    IMPLICIT NONE
      TYPE(txios(scalargroup)) , INTENT(IN) :: scalargroup_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_
      INTEGER  , OPTIONAL, INTENT(IN) :: prec_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: scalar_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: unit_
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: value_

      IF (PRESENT(group_ref_)) THEN
        CALL cxios_set_scalargroup_group_ref &
      (scalargroup_hdl%daddr, group_ref_, len(group_ref_))
      ENDIF

      IF (PRESENT(long_name_)) THEN
        CALL cxios_set_scalargroup_long_name &
      (scalargroup_hdl%daddr, long_name_, len(long_name_))
      ENDIF

      IF (PRESENT(name_)) THEN
        CALL cxios_set_scalargroup_name &
      (scalargroup_hdl%daddr, name_, len(name_))
      ENDIF

      IF (PRESENT(prec_)) THEN
        CALL cxios_set_scalargroup_prec &
      (scalargroup_hdl%daddr, prec_)
      ENDIF

      IF (PRESENT(scalar_ref_)) THEN
        CALL cxios_set_scalargroup_scalar_ref &
      (scalargroup_hdl%daddr, scalar_ref_, len(scalar_ref_))
      ENDIF

      IF (PRESENT(standard_name_)) THEN
        CALL cxios_set_scalargroup_standard_name &
      (scalargroup_hdl%daddr, standard_name_, len(standard_name_))
      ENDIF

      IF (PRESENT(unit_)) THEN
        CALL cxios_set_scalargroup_unit &
      (scalargroup_hdl%daddr, unit_, len(unit_))
      ENDIF

      IF (PRESENT(value_)) THEN
        CALL cxios_set_scalargroup_value &
      (scalargroup_hdl%daddr, value_)
      ENDIF

  END SUBROUTINE xios(set_scalargroup_attr_hdl_)

  SUBROUTINE xios(get_scalargroup_attr)  &
    ( scalargroup_id, group_ref, long_name, name, prec, scalar_ref, standard_name, unit, value )

    IMPLICIT NONE
      TYPE(txios(scalargroup))  :: scalargroup_hdl
      CHARACTER(LEN=*), INTENT(IN) ::scalargroup_id
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      INTEGER  , OPTIONAL, INTENT(OUT) :: prec
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: scalar_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: value

      CALL xios(get_scalargroup_handle) &
      (scalargroup_id,scalargroup_hdl)
      CALL xios(get_scalargroup_attr_hdl_)   &
      ( scalargroup_hdl, group_ref, long_name, name, prec, scalar_ref, standard_name, unit, value  &
       )

  END SUBROUTINE xios(get_scalargroup_attr)

  SUBROUTINE xios(get_scalargroup_attr_hdl)  &
    ( scalargroup_hdl, group_ref, long_name, name, prec, scalar_ref, standard_name, unit, value  &
     )

    IMPLICIT NONE
      TYPE(txios(scalargroup)) , INTENT(IN) :: scalargroup_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      INTEGER  , OPTIONAL, INTENT(OUT) :: prec
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: scalar_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: value

      CALL xios(get_scalargroup_attr_hdl_)  &
      ( scalargroup_hdl, group_ref, long_name, name, prec, scalar_ref, standard_name, unit, value  &
       )

  END SUBROUTINE xios(get_scalargroup_attr_hdl)

  SUBROUTINE xios(get_scalargroup_attr_hdl_)   &
    ( scalargroup_hdl, group_ref_, long_name_, name_, prec_, scalar_ref_, standard_name_, unit_  &
    , value_ )

    IMPLICIT NONE
      TYPE(txios(scalargroup)) , INTENT(IN) :: scalargroup_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_
      INTEGER  , OPTIONAL, INTENT(OUT) :: prec_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: scalar_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: unit_
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: value_

      IF (PRESENT(group_ref_)) THEN
        CALL cxios_get_scalargroup_group_ref &
      (scalargroup_hdl%daddr, group_ref_, len(group_ref_))
      ENDIF

      IF (PRESENT(long_name_)) THEN
        CALL cxios_get_scalargroup_long_name &
      (scalargroup_hdl%daddr, long_name_, len(long_name_))
      ENDIF

      IF (PRESENT(name_)) THEN
        CALL cxios_get_scalargroup_name &
      (scalargroup_hdl%daddr, name_, len(name_))
      ENDIF

      IF (PRESENT(prec_)) THEN
        CALL cxios_get_scalargroup_prec &
      (scalargroup_hdl%daddr, prec_)
      ENDIF

      IF (PRESENT(scalar_ref_)) THEN
        CALL cxios_get_scalargroup_scalar_ref &
      (scalargroup_hdl%daddr, scalar_ref_, len(scalar_ref_))
      ENDIF

      IF (PRESENT(standard_name_)) THEN
        CALL cxios_get_scalargroup_standard_name &
      (scalargroup_hdl%daddr, standard_name_, len(standard_name_))
      ENDIF

      IF (PRESENT(unit_)) THEN
        CALL cxios_get_scalargroup_unit &
      (scalargroup_hdl%daddr, unit_, len(unit_))
      ENDIF

      IF (PRESENT(value_)) THEN
        CALL cxios_get_scalargroup_value &
      (scalargroup_hdl%daddr, value_)
      ENDIF

  END SUBROUTINE xios(get_scalargroup_attr_hdl_)

  SUBROUTINE xios(is_defined_scalargroup_attr)  &
    ( scalargroup_id, group_ref, long_name, name, prec, scalar_ref, standard_name, unit, value )

    IMPLICIT NONE
      TYPE(txios(scalargroup))  :: scalargroup_hdl
      CHARACTER(LEN=*), INTENT(IN) ::scalargroup_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: group_ref
      LOGICAL(KIND=C_BOOL) :: group_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: long_name
      LOGICAL(KIND=C_BOOL) :: long_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name
      LOGICAL(KIND=C_BOOL) :: name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: prec
      LOGICAL(KIND=C_BOOL) :: prec_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: scalar_ref
      LOGICAL(KIND=C_BOOL) :: scalar_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: standard_name
      LOGICAL(KIND=C_BOOL) :: standard_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: unit
      LOGICAL(KIND=C_BOOL) :: unit_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: value
      LOGICAL(KIND=C_BOOL) :: value_tmp

      CALL xios(get_scalargroup_handle) &
      (scalargroup_id,scalargroup_hdl)
      CALL xios(is_defined_scalargroup_attr_hdl_)   &
      ( scalargroup_hdl, group_ref, long_name, name, prec, scalar_ref, standard_name, unit, value  &
       )

  END SUBROUTINE xios(is_defined_scalargroup_attr)

  SUBROUTINE xios(is_defined_scalargroup_attr_hdl)  &
    ( scalargroup_hdl, group_ref, long_name, name, prec, scalar_ref, standard_name, unit, value  &
     )

    IMPLICIT NONE
      TYPE(txios(scalargroup)) , INTENT(IN) :: scalargroup_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: group_ref
      LOGICAL(KIND=C_BOOL) :: group_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: long_name
      LOGICAL(KIND=C_BOOL) :: long_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name
      LOGICAL(KIND=C_BOOL) :: name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: prec
      LOGICAL(KIND=C_BOOL) :: prec_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: scalar_ref
      LOGICAL(KIND=C_BOOL) :: scalar_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: standard_name
      LOGICAL(KIND=C_BOOL) :: standard_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: unit
      LOGICAL(KIND=C_BOOL) :: unit_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: value
      LOGICAL(KIND=C_BOOL) :: value_tmp

      CALL xios(is_defined_scalargroup_attr_hdl_)  &
      ( scalargroup_hdl, group_ref, long_name, name, prec, scalar_ref, standard_name, unit, value  &
       )

  END SUBROUTINE xios(is_defined_scalargroup_attr_hdl)

  SUBROUTINE xios(is_defined_scalargroup_attr_hdl_)   &
    ( scalargroup_hdl, group_ref_, long_name_, name_, prec_, scalar_ref_, standard_name_, unit_  &
    , value_ )

    IMPLICIT NONE
      TYPE(txios(scalargroup)) , INTENT(IN) :: scalargroup_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: group_ref_
      LOGICAL(KIND=C_BOOL) :: group_ref__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: long_name_
      LOGICAL(KIND=C_BOOL) :: long_name__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name_
      LOGICAL(KIND=C_BOOL) :: name__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: prec_
      LOGICAL(KIND=C_BOOL) :: prec__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: scalar_ref_
      LOGICAL(KIND=C_BOOL) :: scalar_ref__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: standard_name_
      LOGICAL(KIND=C_BOOL) :: standard_name__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: unit_
      LOGICAL(KIND=C_BOOL) :: unit__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: value_
      LOGICAL(KIND=C_BOOL) :: value__tmp

      IF (PRESENT(group_ref_)) THEN
        group_ref__tmp = cxios_is_defined_scalargroup_group_ref &
      (scalargroup_hdl%daddr)
        group_ref_ = group_ref__tmp
      ENDIF

      IF (PRESENT(long_name_)) THEN
        long_name__tmp = cxios_is_defined_scalargroup_long_name &
      (scalargroup_hdl%daddr)
        long_name_ = long_name__tmp
      ENDIF

      IF (PRESENT(name_)) THEN
        name__tmp = cxios_is_defined_scalargroup_name &
      (scalargroup_hdl%daddr)
        name_ = name__tmp
      ENDIF

      IF (PRESENT(prec_)) THEN
        prec__tmp = cxios_is_defined_scalargroup_prec &
      (scalargroup_hdl%daddr)
        prec_ = prec__tmp
      ENDIF

      IF (PRESENT(scalar_ref_)) THEN
        scalar_ref__tmp = cxios_is_defined_scalargroup_scalar_ref &
      (scalargroup_hdl%daddr)
        scalar_ref_ = scalar_ref__tmp
      ENDIF

      IF (PRESENT(standard_name_)) THEN
        standard_name__tmp = cxios_is_defined_scalargroup_standard_name &
      (scalargroup_hdl%daddr)
        standard_name_ = standard_name__tmp
      ENDIF

      IF (PRESENT(unit_)) THEN
        unit__tmp = cxios_is_defined_scalargroup_unit &
      (scalargroup_hdl%daddr)
        unit_ = unit__tmp
      ENDIF

      IF (PRESENT(value_)) THEN
        value__tmp = cxios_is_defined_scalargroup_value &
      (scalargroup_hdl%daddr)
        value_ = value__tmp
      ENDIF

  END SUBROUTINE xios(is_defined_scalargroup_attr_hdl_)

END MODULE iscalargroup_attr
