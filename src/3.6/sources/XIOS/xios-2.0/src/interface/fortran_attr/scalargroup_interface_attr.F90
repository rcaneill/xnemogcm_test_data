! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "../fortran/xios_fortran_prefix.hpp"

MODULE scalargroup_interface_attr
  USE, INTRINSIC :: ISO_C_BINDING

  INTERFACE
    ! Do not call directly / interface FORTRAN 2003 <-> C99

    SUBROUTINE cxios_set_scalargroup_group_ref(scalargroup_hdl, group_ref, group_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: group_ref
      INTEGER  (kind = C_INT)     , VALUE        :: group_ref_size
    END SUBROUTINE cxios_set_scalargroup_group_ref

    SUBROUTINE cxios_get_scalargroup_group_ref(scalargroup_hdl, group_ref, group_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: group_ref
      INTEGER  (kind = C_INT)     , VALUE        :: group_ref_size
    END SUBROUTINE cxios_get_scalargroup_group_ref

    FUNCTION cxios_is_defined_scalargroup_group_ref(scalargroup_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_scalargroup_group_ref
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
    END FUNCTION cxios_is_defined_scalargroup_group_ref


    SUBROUTINE cxios_set_scalargroup_long_name(scalargroup_hdl, long_name, long_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: long_name
      INTEGER  (kind = C_INT)     , VALUE        :: long_name_size
    END SUBROUTINE cxios_set_scalargroup_long_name

    SUBROUTINE cxios_get_scalargroup_long_name(scalargroup_hdl, long_name, long_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: long_name
      INTEGER  (kind = C_INT)     , VALUE        :: long_name_size
    END SUBROUTINE cxios_get_scalargroup_long_name

    FUNCTION cxios_is_defined_scalargroup_long_name(scalargroup_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_scalargroup_long_name
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
    END FUNCTION cxios_is_defined_scalargroup_long_name


    SUBROUTINE cxios_set_scalargroup_name(scalargroup_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_set_scalargroup_name

    SUBROUTINE cxios_get_scalargroup_name(scalargroup_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_get_scalargroup_name

    FUNCTION cxios_is_defined_scalargroup_name(scalargroup_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_scalargroup_name
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
    END FUNCTION cxios_is_defined_scalargroup_name


    SUBROUTINE cxios_set_scalargroup_prec(scalargroup_hdl, prec) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: prec
    END SUBROUTINE cxios_set_scalargroup_prec

    SUBROUTINE cxios_get_scalargroup_prec(scalargroup_hdl, prec) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
      INTEGER (KIND=C_INT)             :: prec
    END SUBROUTINE cxios_get_scalargroup_prec

    FUNCTION cxios_is_defined_scalargroup_prec(scalargroup_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_scalargroup_prec
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
    END FUNCTION cxios_is_defined_scalargroup_prec


    SUBROUTINE cxios_set_scalargroup_scalar_ref(scalargroup_hdl, scalar_ref, scalar_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: scalar_ref
      INTEGER  (kind = C_INT)     , VALUE        :: scalar_ref_size
    END SUBROUTINE cxios_set_scalargroup_scalar_ref

    SUBROUTINE cxios_get_scalargroup_scalar_ref(scalargroup_hdl, scalar_ref, scalar_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: scalar_ref
      INTEGER  (kind = C_INT)     , VALUE        :: scalar_ref_size
    END SUBROUTINE cxios_get_scalargroup_scalar_ref

    FUNCTION cxios_is_defined_scalargroup_scalar_ref(scalargroup_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_scalargroup_scalar_ref
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
    END FUNCTION cxios_is_defined_scalargroup_scalar_ref


    SUBROUTINE cxios_set_scalargroup_standard_name(scalargroup_hdl, standard_name, standard_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: standard_name
      INTEGER  (kind = C_INT)     , VALUE        :: standard_name_size
    END SUBROUTINE cxios_set_scalargroup_standard_name

    SUBROUTINE cxios_get_scalargroup_standard_name(scalargroup_hdl, standard_name, standard_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: standard_name
      INTEGER  (kind = C_INT)     , VALUE        :: standard_name_size
    END SUBROUTINE cxios_get_scalargroup_standard_name

    FUNCTION cxios_is_defined_scalargroup_standard_name(scalargroup_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_scalargroup_standard_name
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
    END FUNCTION cxios_is_defined_scalargroup_standard_name


    SUBROUTINE cxios_set_scalargroup_unit(scalargroup_hdl, unit, unit_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: unit
      INTEGER  (kind = C_INT)     , VALUE        :: unit_size
    END SUBROUTINE cxios_set_scalargroup_unit

    SUBROUTINE cxios_get_scalargroup_unit(scalargroup_hdl, unit, unit_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: unit
      INTEGER  (kind = C_INT)     , VALUE        :: unit_size
    END SUBROUTINE cxios_get_scalargroup_unit

    FUNCTION cxios_is_defined_scalargroup_unit(scalargroup_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_scalargroup_unit
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
    END FUNCTION cxios_is_defined_scalargroup_unit


    SUBROUTINE cxios_set_scalargroup_value(scalargroup_hdl, value) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
      REAL (KIND=C_DOUBLE)      , VALUE :: value
    END SUBROUTINE cxios_set_scalargroup_value

    SUBROUTINE cxios_get_scalargroup_value(scalargroup_hdl, value) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
      REAL (KIND=C_DOUBLE)             :: value
    END SUBROUTINE cxios_get_scalargroup_value

    FUNCTION cxios_is_defined_scalargroup_value(scalargroup_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_scalargroup_value
      INTEGER (kind = C_INTPTR_T), VALUE :: scalargroup_hdl
    END FUNCTION cxios_is_defined_scalargroup_value

  END INTERFACE

END MODULE scalargroup_interface_attr
