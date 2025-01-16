MODULE FIELD_INTERFACE
   USE, INTRINSIC :: ISO_C_BINDING
   
   INTERFACE ! Ne pas appeler directement/Interface FORTRAN 2003 <-> C99
   
      SUBROUTINE cxios_field_handle_create(ret, idt, idt_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T)               :: ret
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: idt
         INTEGER  (kind = C_INT)     , VALUE        :: idt_size
      END SUBROUTINE cxios_field_handle_create

      SUBROUTINE cxios_field_valid_id(ret, idt, idt_size) BIND(C)
         USE ISO_C_BINDING
         LOGICAL  (kind = C_BOOL)                   :: ret
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: idt
         INTEGER  (kind = C_INT)     , VALUE        :: idt_size
      END SUBROUTINE cxios_field_valid_id

      SUBROUTINE cxios_field_is_active(field_hdl, at_current_timestep, ret) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: field_hdl
         LOGICAL  (kind = C_BOOL), VALUE            :: at_current_timestep
         LOGICAL  (kind = C_BOOL)                   :: ret
      END SUBROUTINE cxios_field_is_active

      SUBROUTINE cxios_field_get_domain_handle(ret, field_hdl, idx) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T)               :: ret
         INTEGER  (kind = C_INTPTR_T), VALUE        :: field_hdl
         INTEGER  (kind = C_INT)     , VALUE        :: idx
      END SUBROUTINE cxios_field_get_domain_handle

      SUBROUTINE cxios_field_get_axis_handle(ret, field_hdl, idx) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T)               :: ret
         INTEGER  (kind = C_INTPTR_T), VALUE        :: field_hdl
         INTEGER  (kind = C_INT)     , VALUE        :: idx
      END SUBROUTINE cxios_field_get_axis_handle

      SUBROUTINE cxios_field_get_scalar_handle(ret, field_hdl, idx) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T)               :: ret
         INTEGER  (kind = C_INTPTR_T), VALUE        :: field_hdl
         INTEGER  (kind = C_INT)     , VALUE        :: idx
      END SUBROUTINE cxios_field_get_scalar_handle

   END INTERFACE
   
END MODULE FIELD_INTERFACE
