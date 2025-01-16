!----------------------------------------------------------------------
! NEMO system team, System and Interface for oceanic RElocable Nesting
!----------------------------------------------------------------------
!
! MODULE: att
!
! DESCRIPTION:
!> @brief 
!> This module manage attribute of variable or file.
!>
!> @details
!>    define type TATT:<br/>
!> @code
!>    TYPE(TATT) :: tl_att
!> @endcode
!>
!>    the attribute value inside attribute structure will be
!>    character or real(8) 1D array.<br/>
!>    However the attribute value could be initialized with:<br/>
!>    - character
!>    - scalar (real(4), real(8), integer(4) or integer(8))
!>    - array 1D (real(4), real(8), integer(4) or integer(8))
!>
!>    to initialize an attribute structure :<br/>
!> @code
!>    tl_att=att_init('attname',value)
!> @endcode
!>    - value is a character, scalar value or table of value
!>
!>    to print attribute information of one or array of attribute structure:<br/>
!> @code
!>    CALL att_print(td_att) 
!> @endcode
!>
!>    to clean attribute structure:<br/>
!> @code
!>    CALL att_clean(td_att) 
!> @endcode
!>
!>    to copy attribute structure in another one (using different memory cell):<br/>
!> @code
!>    tl_att2=att_copy(tl_att1) 
!> @endcode
!>    @note as we use pointer for the value array of the attribute structure,
!>    the use of the assignment operator (=) to copy attribute structure 
!>    create a pointer on the same array. 
!>    This is not the case with this copy function.
!>
!>    to get attribute index, in an array of attribute structure:<br/>
!> @code
!>   il_index=att_get_index( td_att, cd_name )
!> @endcode
!>    - td_att array of attribute structure
!>    - cd_name attribute name
!> 
!>    to get attribute id, read from a file:<br/>
!>@code
!>  il_id=att_get_id( td_att, cd_name )
!>@endcode
!>    - td_att array of attribute structure
!>    - cd_name attribute name
!>
!>    to get attribute name
!>    - tl_att\%c_name
!> 
!>    to get character length or the number of value store in attribute
!>    - tl_att\%i_len
!>
!>    to get attribute value:<br/>
!>    - tl_att\%c_value    (for character attribute)
!>    - tl_att\%d_value(i) (otherwise)
!>    
!>    to get the type number (based on NETCDF type constants) of the 
!>    attribute:<br/>
!>    - tl_att\%i_type
!>
!>    to get attribute id (read from file):<br/>
!>    - tl_att\%i_id
!>
!> @author J.Paul
! REVISION HISTORY:
!> @date November, 2013 - Initial Version
!> @date November, 2014 
!> - Fix memory leaks bug
!> @date September, 2015
!> - manage useless (dummy) attributes
!
!> @note Software governed by the CeCILL licence     (./LICENSE)
!----------------------------------------------------------------------
MODULE att
   USE netcdf                          ! nf90 library
   USE global                          ! global variable
   USE kind                            ! F90 kind parameter
   USE logger                          ! log file manager
   USE fct                             ! basic useful function
   IMPLICIT NONE
   ! NOTE_avoid_public_variables_if_possible

   ! type and variable
   PUBLIC :: TATT       !< attribute structure

   PRIVATE :: cm_dumatt !< dummy attribute array

   ! function and subroutine
   PUBLIC :: att_init       !< initialize attribute structure
   PUBLIC :: att_print      !< print attribute structure
   PUBLIC :: att_clean      !< clean attribute strcuture
   PUBLIC :: att_copy       !< copy attribute structure
   PUBLIC :: att_get_index  !< get attribute index, in an array of attribute structure
   PUBLIC :: att_get_id     !< get attribute id, read from file
   PUBLIC :: att_get_dummy  !< fill dummy attribute array
   PUBLIC :: att_is_dummy   !< check if attribute is defined as dummy attribute

   PRIVATE :: att__clean_unit ! clean attribute strcuture
   PRIVATE :: att__clean_arr  ! clean array of attribute strcuture
   PRIVATE :: att__print_unit ! print information on one attribute
   PRIVATE :: att__print_arr  ! print information on a array of attribute
   PRIVATE :: att__init_c     ! initialize an attribute structure with character value
   PRIVATE :: att__init_dp    ! initialize an attribute structure with array of real(8) value
   PRIVATE :: att__init_dp_0d ! initialize an attribute structure with real(8) value
   PRIVATE :: att__init_sp    ! initialize an attribute structure with array of real(4) value
   PRIVATE :: att__init_sp_0d ! initialize an attribute structure with real(4) value
   PRIVATE :: att__init_i1    ! initialize an attribute structure with array of integer(1) value
   PRIVATE :: att__init_i1_0d ! initialize an attribute structure with integer(1) value
   PRIVATE :: att__init_i2    ! initialize an attribute structure with array of integer(2) value
   PRIVATE :: att__init_i2_0d ! initialize an attribute structure with integer(2) value
   PRIVATE :: att__init_i4    ! initialize an attribute structure with array of integer(4) value
   PRIVATE :: att__init_i4_0d ! initialize an attribute structure with integer(4) value
   PRIVATE :: att__init_i8    ! initialize an attribute structure with array of integer(8) value
   PRIVATE :: att__init_i8_0d ! initialize an attribute structure with integer(8) value
   PRIVATE :: att__copy_unit  ! copy attribute structure
   PRIVATE :: att__copy_arr   ! copy array of attribute structure

   TYPE TATT !< attribute structure
      CHARACTER(LEN=lc) :: c_name = ''       !< attribute name
      INTEGER(i4)       :: i_id   = 0        !< attribute id
      INTEGER(i4)       :: i_type = 0        !< attribute type
      INTEGER(i4)       :: i_len  = 0        !< number of value store in attribute
      CHARACTER(LEN=lc) :: c_value = 'none'  !< attribute value if type CHAR
      REAL(dp), DIMENSION(:), POINTER :: d_value => NULL() !< attribute value if type SHORT,INT,FLOAT or DOUBLE
   END TYPE TATT

   CHARACTER(LEN=lc), DIMENSION(ip_maxdumcfg), SAVE :: cm_dumatt !< dummy attribute

   INTERFACE att_init
      MODULE PROCEDURE att__init_c    
      MODULE PROCEDURE att__init_dp
      MODULE PROCEDURE att__init_dp_0d
      MODULE PROCEDURE att__init_sp
      MODULE PROCEDURE att__init_sp_0d
      MODULE PROCEDURE att__init_i1
      MODULE PROCEDURE att__init_i1_0d
      MODULE PROCEDURE att__init_i2
      MODULE PROCEDURE att__init_i2_0d
      MODULE PROCEDURE att__init_i4
      MODULE PROCEDURE att__init_i4_0d
      MODULE PROCEDURE att__init_i8
      MODULE PROCEDURE att__init_i8_0d
   END INTERFACE att_init

   INTERFACE att_print
      MODULE PROCEDURE att__print_unit ! print information on one attribute
      MODULE PROCEDURE att__print_arr  ! print information on a array of attribute
   END INTERFACE att_print

   INTERFACE att_clean
      MODULE PROCEDURE att__clean_unit  
      MODULE PROCEDURE att__clean_arr   
   END INTERFACE

   INTERFACE att_copy
      MODULE PROCEDURE att__copy_unit  ! copy attribute structure
      MODULE PROCEDURE att__copy_arr   ! copy array of attribute structure
   END INTERFACE

CONTAINS
   !-------------------------------------------------------------------
   !> @brief
   !> This subroutine copy a array of attribute structure in another one
   !> @details 
   !> see att__copy_unit
   !>
   !> @warning do not use on the output of a function who create or read an
   !> attribute (ex: tl_att=att_copy(att_init()) is forbidden).
   !> This will create memory leaks.
   !> @warning to avoid infinite loop, do not use any function inside 
   !> this subroutine
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !> @date November, 2014
   !> - use function instead of overload assignment operator 
   !> (to avoid memory leak) 
   !
   !> @param[in] td_att   array of attribute structure
   !> @return copy of input array of attribute structure
   !-------------------------------------------------------------------
   FUNCTION att__copy_arr( td_att )
      IMPLICIT NONE
      ! Argument
      TYPE(TATT), DIMENSION(:), INTENT(IN) :: td_att
      ! function
      TYPE(TATT), DIMENSION(SIZE(td_att(:))) :: att__copy_arr

      ! local variable
      ! loop indices
      INTEGER(i4) :: ji
      !----------------------------------------------------------------

      DO ji=1,SIZE(td_att(:))
         att__copy_arr(ji)=att_copy(td_att(ji))
      ENDDO

   END FUNCTION att__copy_arr
   !-------------------------------------------------------------------
   !> @brief
   !> This subroutine copy an attribute structure in another one.
   !> @details 
   !> attribute value are copied in a temporary array, so input and output
   !> attribute structure value do not point on the same "memory cell", and so
   !> on are independant. 
   !>
   !> @warning do not use on the output of a function who create or read an
   !> attribute (ex: tl_att=att_copy(att_init()) is forbidden).
   !> This will create memory leaks.
   !> @warning to avoid infinite loop, do not use any function inside 
   !> this subroutine
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !> @date November, 2014
   !> - use function instead of overload assignment operator (to avoid memory leak) 
   !>
   !> @param[in] td_att   attribute structure
   !> @return copy of input attribute structure
   !-------------------------------------------------------------------
   FUNCTION att__copy_unit( td_att )
      IMPLICIT NONE
      ! Argument
      TYPE(TATT), INTENT(IN)  :: td_att
      ! function
      TYPE(TATT) :: att__copy_unit

      ! local variable
      REAL(dp)         , DIMENSION(:), ALLOCATABLE :: dl_value
      !----------------------------------------------------------------

      ! copy attribute variable
      att__copy_unit%c_name  = TRIM(td_att%c_name)
      att__copy_unit%i_id    = td_att%i_id
      att__copy_unit%i_type  = td_att%i_type
      att__copy_unit%i_len   = td_att%i_len
      att__copy_unit%c_value = TRIM(td_att%c_value)

      ! copy attribute pointer in an independant variable
      IF( ASSOCIATED(att__copy_unit%d_value) ) DEALLOCATE(att__copy_unit%d_value)
      IF( ASSOCIATED(td_att%d_value) )THEN
         ALLOCATE( dl_value(td_att%i_len) )
         dl_value(:) = td_att%d_value(:)

         ALLOCATE( att__copy_unit%d_value(att__copy_unit%i_len) )
         att__copy_unit%d_value(:) = dl_value(:)

         DEALLOCATE( dl_value )
      ENDIF

   END FUNCTION att__copy_unit
   !-------------------------------------------------------------------
   !> @brief This function return attribute index, in a array of attribute structure,
   !> given attribute name.<br/>
   !> @details
   !> if attribute name do not exist, return 0.
   !>
   !> @author J.Paul
   !> @date Septempber, 2014 - Initial Version
   !
   !> @param[in] td_att    array of attribute structure
   !> @param[in] cd_name   attribute name
   !> @return attribute index
   !-------------------------------------------------------------------
   INTEGER(i4) FUNCTION att_get_index( td_att, cd_name )
      IMPLICIT NONE
      ! Argument
      TYPE(TATT),       DIMENSION(:), INTENT(IN) :: td_att
      CHARACTER(LEN=*),               INTENT(IN) :: cd_name

      ! local variable
      INTEGER(i4) :: il_size

      ! loop indices
      INTEGER(i4) :: ji
      !----------------------------------------------------------------
      att_get_index=0

      il_size=SIZE(td_att(:))
      DO ji=1,il_size
         IF( TRIM(td_att(ji)%c_name) == TRIM(cd_name) )THEN
            att_get_index=ji
            EXIT
         ENDIF
      ENDDO

   END FUNCTION att_get_index
   !-------------------------------------------------------------------
   !> @brief This function return attribute id, read from a file.<br/>
   !> @details
   !> if attribute name do not exist, return 0.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !> @date September, 2014 
   !> - bug fix with use of id read from attribute structure
   !>
   !> @param[in] td_att    array of attribute structure
   !> @param[in] cd_name   attribute name
   !> @return attribute id
   !-------------------------------------------------------------------
   INTEGER(i4) FUNCTION att_get_id( td_att, cd_name )
      IMPLICIT NONE
      ! Argument
      TYPE(TATT),       DIMENSION(:), INTENT(IN) :: td_att
      CHARACTER(LEN=*),               INTENT(IN) :: cd_name

      ! local variable
      INTEGER(i4) :: il_size

      ! loop indices
      INTEGER(i4) :: ji
      !----------------------------------------------------------------
      att_get_id=0

      il_size=SIZE(td_att(:))
      DO ji=1,il_size
         IF( TRIM(td_att(ji)%c_name) == TRIM(cd_name) )THEN
            att_get_id=td_att(ji)%i_id
            EXIT
         ENDIF
      ENDDO

   END FUNCTION att_get_id
   !-------------------------------------------------------------------
   !> @brief This function initialize an attribute structure with character
   !> value. 
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] cd_name   attribute name
   !> @param[in] cd_value  attribute value
   !> @return attribute structure
   !-------------------------------------------------------------------
   TYPE(TATT) FUNCTION att__init_c( cd_name, cd_value )
      IMPLICIT NONE
      ! Argument
      CHARACTER(LEN=*), INTENT(IN) :: cd_name
      CHARACTER(LEN=*), INTENT(IN) :: cd_value
      !----------------------------------------------------------------
 
      ! clean attribute
      CALL att_clean(att__init_c)

      CALL logger_trace( &
      &  " ATT INIT: attribute name: "//TRIM(ADJUSTL(cd_name))//&
      &  " attribute value "//TRIM(ADJUSTL(cd_value)) )

      att__init_c%c_name=TRIM(ADJUSTL(cd_name))
      att__init_c%i_type=NF90_CHAR

      att__init_c%c_value=TRIM(ADJUSTL(cd_value))
      att__init_c%i_len=LEN( TRIM(ADJUSTL(cd_value)) )

   END FUNCTION att__init_c
   !-------------------------------------------------------------------
   !> @brief This function initialize an attribute structure with array 
   !> of real(8) value.
   !> @details
   !> Optionaly you could specify the type of the variable to be saved.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] cd_name   attribute name
   !> @param[in] dd_value  attribute value
   !> @param[in] id_type   type of the variable to be saved
   !> @return attribute structure
   !-------------------------------------------------------------------
   TYPE(TATT) FUNCTION att__init_dp( cd_name, dd_value, id_type )
      IMPLICIT NONE

      ! Argument
      CHARACTER(LEN=*),               INTENT(IN) :: cd_name
      REAL(dp),         DIMENSION(:), INTENT(IN) :: dd_value
      INTEGER(i4)                   , INTENT(IN), OPTIONAL :: id_type

      ! local value
      INTEGER(i4)       :: il_len
      CHARACTER(LEN=lc) :: cl_value

      ! loop indices
      INTEGER(i4) :: ji
      !----------------------------------------------------------------

      ! clean attribute
      CALL att_clean(att__init_dp)

      ! array size
      il_len=size(dd_value(:))

      cl_value="(/"
      DO ji=1,il_len-1
         cl_value=TRIM(cl_value)//TRIM(fct_str(dd_value(ji)))//","
      ENDDO
      cl_value=TRIM(cl_value)//TRIM(fct_str(dd_value(il_len)))//"/)"

      CALL logger_trace( &
      &  " ATT INIT: attribute name: "//TRIM(ADJUSTL(cd_name))//&
      &  " attribute value "//TRIM(ADJUSTL(cl_value)) )

      att__init_dp%c_name=TRIM(ADJUSTL(cd_name))

      IF( PRESENT(id_type) )THEN
         att__init_dp%i_type=id_type
      ELSE
         att__init_dp%i_type=NF90_DOUBLE
      ENDIF

      IF( ASSOCIATED(att__init_dp%d_value) )THEN
         DEALLOCATE(att__init_dp%d_value)
      ENDIF
      ALLOCATE(att__init_dp%d_value(il_len))

      att__init_dp%d_value(:)=dd_value(:)
      att__init_dp%i_len=il_len

   END FUNCTION att__init_dp
   !-------------------------------------------------------------------
   !> @brief This function initialize an attribute structure with 
   !> real(8) value 
   !> @details
   !> Optionaly you could specify the type of the variable to be saved.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] cd_name   attribute name
   !> @param[in] dd_value  attribute value
   !> @param[in] id_type   type of the variable to be saved
   !> @return attribute structure
   !-------------------------------------------------------------------
   TYPE(TATT) FUNCTION att__init_dp_0d( cd_name, dd_value, id_type )
      IMPLICIT NONE
      ! Argument
      CHARACTER(LEN=*), INTENT(IN) :: cd_name
      REAL(dp),         INTENT(IN) :: dd_value
      INTEGER(i4)     , INTENT(IN), OPTIONAL :: id_type

      ! local value
      CHARACTER(LEN=lc) :: cl_value
      !----------------------------------------------------------------

      ! clean attribute
      CALL att_clean(att__init_dp_0d)
      
      cl_value="(/"//TRIM(fct_str(dd_value))//"/)"

      CALL logger_trace( &
      &  " ATT INIT: attribute name: "//TRIM(ADJUSTL(cd_name))//&
      &  " attribute value "//TRIM(ADJUSTL(cl_value)) )

      att__init_dp_0d%c_name=TRIM(ADJUSTL(cd_name))

      IF( PRESENT(id_type) )THEN
         att__init_dp_0d%i_type=id_type
      ELSE
         att__init_dp_0d%i_type=NF90_DOUBLE
      ENDIF

      IF( ASSOCIATED(att__init_dp_0d%d_value) )THEN
         DEALLOCATE(att__init_dp_0d%d_value)
      ENDIF
      ALLOCATE(att__init_dp_0d%d_value(1))

      att__init_dp_0d%d_value(1)=dd_value
      att__init_dp_0d%i_len=1

   END FUNCTION att__init_dp_0d
   !-------------------------------------------------------------------
   !> @brief This function initialize an attribute structure with array 
   !> of real(4) value.
   !> @details
   !> Optionaly you could specify the type of the variable to be saved.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] cd_name   attribute name
   !> @param[in] rd_value  attribute value
   !> @param[in] id_type   type of the variable to be saved
   !> @return attribute structure
   !-------------------------------------------------------------------
   TYPE(TATT) FUNCTION att__init_sp( cd_name, rd_value, id_type )
      IMPLICIT NONE
      ! Argument
      CHARACTER(LEN=*),               INTENT(IN) :: cd_name
      REAL(sp),         DIMENSION(:), INTENT(IN) :: rd_value
      INTEGER(i4)                   , INTENT(IN), OPTIONAL :: id_type

      ! local value
      INTEGER(i4)       :: il_len
      CHARACTER(LEN=lc) :: cl_value

      ! loop indices
      INTEGER(i4) :: ji
      !----------------------------------------------------------------

      ! clean attribute
      CALL att_clean(att__init_sp)
      
      ! array size
      il_len=size(rd_value(:))

      cl_value="(/"
      DO ji=1,il_len-1
         cl_value=TRIM(cl_value)//TRIM(fct_str(rd_value(ji)))//","
      ENDDO
      cl_value=TRIM(cl_value)//TRIM(fct_str(rd_value(il_len)))//"/)"

      CALL logger_trace( &
      &  " ATT INIT: attribute name: "//TRIM(ADJUSTL(cd_name))//&
      &  " attribute value "//TRIM(ADJUSTL(cl_value)) )      

      att__init_sp%c_name=TRIM(ADJUSTL(cd_name))

      IF( PRESENT(id_type) )THEN
         att__init_sp%i_type=id_type
      ELSE
         att__init_sp%i_type=NF90_FLOAT
      ENDIF

      IF( ASSOCIATED(att__init_sp%d_value) )THEN
         DEALLOCATE(att__init_sp%d_value)
      ENDIF
      ALLOCATE(att__init_sp%d_value(il_len))

      att__init_sp%d_value(:)=REAL(rd_value(:),dp)
      att__init_sp%i_len=il_len

   END FUNCTION att__init_sp
   !-------------------------------------------------------------------
   !> @brief This function initialize an attribute structure with 
   !> real(4) value. 
   !> @details
   !> Optionaly you could specify the type of the variable to be saved.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] cd_name   attribute name
   !> @param[in] rd_value  attribute value
   !> @param[in] id_type   type of the variable to be saved
   !> @return attribute structure
   !-------------------------------------------------------------------
   TYPE(TATT) FUNCTION att__init_sp_0d( cd_name, rd_value, id_type )
      IMPLICIT NONE
      ! Argument
      CHARACTER(LEN=*), INTENT(IN) :: cd_name
      REAL(sp),         INTENT(IN) :: rd_value
      INTEGER(i4)     , INTENT(IN), OPTIONAL :: id_type

      ! local value
      CHARACTER(LEN=lc) :: cl_value
      !----------------------------------------------------------------

      ! clean attribute
      CALL att_clean(att__init_sp_0d)
      
      cl_value="(/"//TRIM(fct_str(rd_value))//"/)"

      CALL logger_trace( &
      &  " ATT INIT: attribute name: "//TRIM(ADJUSTL(cd_name))//&
      &  " attribute value "//TRIM(ADJUSTL(cl_value)) )      

      att__init_sp_0d%c_name=TRIM(ADJUSTL(cd_name))

      IF( PRESENT(id_type) )THEN
         att__init_sp_0d%i_type=id_type
      ELSE
         att__init_sp_0d%i_type=NF90_FLOAT
      ENDIF

      IF( ASSOCIATED(att__init_sp_0d%d_value) )THEN
         DEALLOCATE(att__init_sp_0d%d_value)
      ENDIF
      ALLOCATE(att__init_sp_0d%d_value(1))

      att__init_sp_0d%d_value(1)=REAL(rd_value,dp)
      att__init_sp_0d%i_len=1

   END FUNCTION att__init_sp_0d
   !-------------------------------------------------------------------
   !> @brief This function initialize an attribute structure with array 
   !> of integer(1) value.
   !> @details
   !> Optionaly you could specify the type of the variable to be saved.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] cd_name   attribute name
   !> @param[in] bd_value  attribute value
   !> @param[in] id_type   type of the variable to be saved
   !> @return attribute structure
   !-------------------------------------------------------------------
   TYPE(TATT) FUNCTION att__init_i1( cd_name, bd_value, id_type )
      IMPLICIT NONE
      ! Argument
      CHARACTER(LEN=*),               INTENT(IN) :: cd_name
      INTEGER(i1),      DIMENSION(:), INTENT(IN) :: bd_value
      INTEGER(i4)                   , INTENT(IN), OPTIONAL :: id_type

      ! local value
      INTEGER(i4)       :: il_len
      CHARACTER(LEN=lc) :: cl_value

      ! loop indices
      INTEGER(i4) :: ji
      !----------------------------------------------------------------

      ! clean attribute
      CALL att_clean(att__init_i1)
      
      ! array size
      il_len=size(bd_value(:))

      cl_value="(/"
      DO ji=1,il_len-1
         cl_value=TRIM(cl_value)//TRIM(fct_str(bd_value(ji)))//","
      ENDDO
      cl_value=TRIM(cl_value)//TRIM(fct_str(bd_value(il_len)))//"/)"

      CALL logger_trace( &
      &  " ATT INIT: attribute name: "//TRIM(ADJUSTL(cd_name))//&
      &  " attribute value "//TRIM(ADJUSTL(cl_value)) )      

      att__init_i1%c_name=TRIM(ADJUSTL(cd_name))

      IF( PRESENT(id_type) )THEN
         att__init_i1%i_type=id_type
      ELSE
         att__init_i1%i_type=NF90_BYTE
      ENDIF

      IF( ASSOCIATED(att__init_i1%d_value) )THEN
         DEALLOCATE(att__init_i1%d_value)
      ENDIF
      ALLOCATE(att__init_i1%d_value(il_len))

      att__init_i1%d_value(:)=REAL(bd_value(:),dp)
      att__init_i1%i_len=il_len

   END FUNCTION att__init_i1
   !-------------------------------------------------------------------
   !> @brief This function initialize an attribute structure with 
   !> integer(1) value. 
   !> @details
   !> Optionaly you could specify the type of the variable to be saved.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] cd_name   attribute name
   !> @param[in] bd_value  attribute value
   !> @param[in] id_type   type of the variable to be saved
   !> @return attribute structure
   !-------------------------------------------------------------------
   TYPE(TATT) FUNCTION att__init_i1_0d( cd_name, bd_value, id_type )
      IMPLICIT NONE
      ! Argument
      CHARACTER(LEN=*), INTENT(IN) :: cd_name
      INTEGER(i1),      INTENT(IN) :: bd_value
      INTEGER(i4)     , INTENT(IN), OPTIONAL :: id_type

      !local value
      CHARACTER(LEN=lc) :: cl_value
      !----------------------------------------------------------------

      ! clean attribute
      CALL att_clean(att__init_i1_0d)
      
      cl_value="(/"//TRIM(fct_str(bd_value))//"/)"

      CALL logger_trace( &
      &  " ATT INIT: attribute name: "//TRIM(ADJUSTL(cd_name))//&
      &  " attibute value "//TRIM(ADJUSTL(cl_value)) )      

      att__init_i1_0d%c_name=TRIM(ADJUSTL(cd_name))

      IF( PRESENT(id_type) )THEN
         att__init_i1_0d%i_type=id_type
      ELSE
         att__init_i1_0d%i_type=NF90_BYTE
      ENDIF      

      IF( ASSOCIATED(att__init_i1_0d%d_value) )THEN
         DEALLOCATE(att__init_i1_0d%d_value)
      ENDIF
      ALLOCATE(att__init_i1_0d%d_value(1))

      att__init_i1_0d%d_value(1)=REAL(bd_value,dp)
      att__init_i1_0d%i_len=1

   END FUNCTION att__init_i1_0d
   !-------------------------------------------------------------------
   !> @brief This function initialize an attribute structure with array 
   !> of integer(2) value.
   !> @details
   !> Optionaly you could specify the type of the variable to be saved.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] cd_name   attribute name
   !> @param[in] sd_value  attribute value
   !> @param[in] id_type   type of the variable to be saved
   !> @return attribute structure
   !-------------------------------------------------------------------
   TYPE(TATT) FUNCTION att__init_i2( cd_name, sd_value, id_type )
      IMPLICIT NONE
      ! Argument
      CHARACTER(LEN=*),               INTENT(IN) :: cd_name
      INTEGER(i2),      DIMENSION(:), INTENT(IN) :: sd_value
      INTEGER(i4)                   , INTENT(IN), OPTIONAL :: id_type

      ! local value
      INTEGER(i4)       :: il_len
      CHARACTER(LEN=lc) :: cl_value

      ! loop indices
      INTEGER(i4) :: ji
      !----------------------------------------------------------------

      ! clean attribute
      CALL att_clean(att__init_i2)
      
      ! array size
      il_len=size(sd_value(:))

      cl_value="(/"
      DO ji=1,il_len-1
         cl_value=TRIM(cl_value)//TRIM(fct_str(sd_value(ji)))//","
      ENDDO
      cl_value=TRIM(cl_value)//TRIM(fct_str(sd_value(il_len)))//"/)"

      CALL logger_trace( &
      &  " ATT INIT: attribute name: "//TRIM(ADJUSTL(cd_name))//&
      &  " attribute value "//TRIM(ADJUSTL(cl_value)) )      

      att__init_i2%c_name=TRIM(ADJUSTL(cd_name))

      IF( PRESENT(id_type) )THEN
         att__init_i2%i_type=id_type
      ELSE
         att__init_i2%i_type=NF90_SHORT
      ENDIF

      IF( ASSOCIATED(att__init_i2%d_value) )THEN
         DEALLOCATE(att__init_i2%d_value)
      ENDIF
      ALLOCATE(att__init_i2%d_value(il_len))

      att__init_i2%d_value(:)=REAL(sd_value(:),dp)
      att__init_i2%i_len=il_len

   END FUNCTION att__init_i2
   !-------------------------------------------------------------------
   !> @brief This function initialize an attribute structure with 
   !> integer(2) value. 
   !> @details
   !> Optionaly you could specify the type of the variable to be saved.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] cd_name   attribute name
   !> @param[in] sd_value  attribute value
   !> @param[in] id_type   type of the variable to be saved
   !> @return attribute structure
   !-------------------------------------------------------------------
   TYPE(TATT) FUNCTION att__init_i2_0d( cd_name, sd_value, id_type )
      IMPLICIT NONE
      ! Argument
      CHARACTER(LEN=*), INTENT(IN) :: cd_name
      INTEGER(i2),      INTENT(IN) :: sd_value
      INTEGER(i4)     , INTENT(IN), OPTIONAL :: id_type

      !local value
      CHARACTER(LEN=lc) :: cl_value
      !----------------------------------------------------------------

      ! clean attribute
      CALL att_clean(att__init_i2_0d)
      
      cl_value="(/"//TRIM(fct_str(sd_value))//"/)"

      CALL logger_trace( &
      &  " ATT INIT: attribute name: "//TRIM(ADJUSTL(cd_name))//&
      &  " attibute value "//TRIM(ADJUSTL(cl_value)) )      

      att__init_i2_0d%c_name=TRIM(ADJUSTL(cd_name))

      IF( PRESENT(id_type) )THEN
         att__init_i2_0d%i_type=id_type
      ELSE
         att__init_i2_0d%i_type=NF90_SHORT
      ENDIF

      IF( ASSOCIATED(att__init_i2_0d%d_value) )THEN
         DEALLOCATE(att__init_i2_0d%d_value)
      ENDIF
      ALLOCATE(att__init_i2_0d%d_value(1))

      att__init_i2_0d%d_value(1)=REAL(sd_value,dp)
      att__init_i2_0d%i_len=1

   END FUNCTION att__init_i2_0d
   !-------------------------------------------------------------------
   !> @brief This function initialize an attribute structure with array 
   !> of integer(4) value.
   !> @details
   !> Optionaly you could specify the type of the variable to be saved.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] cd_name   attribute name
   !> @param[in] id_value  attribute value
   !> @param[in] id_type   type of the variable to be saved
   !> @return attribute structure
   !-------------------------------------------------------------------
   TYPE(TATT) FUNCTION att__init_i4( cd_name, id_value, id_type )
      IMPLICIT NONE
      ! Argument
      CHARACTER(LEN=*),               INTENT(IN) :: cd_name
      INTEGER(i4),      DIMENSION(:), INTENT(IN) :: id_value
      INTEGER(i4)                   , INTENT(IN), OPTIONAL :: id_type

      ! local value
      INTEGER(i4)       :: il_len
      CHARACTER(LEN=lc) :: cl_value

      ! loop indices
      INTEGER(i4) :: ji
      !----------------------------------------------------------------

      ! clean attribute
      CALL att_clean(att__init_i4)
      
      ! array size
      il_len=size(id_value(:))

      cl_value="(/"
      DO ji=1,il_len-1
         cl_value=TRIM(cl_value)//TRIM(fct_str(id_value(ji)))//","
      ENDDO
      cl_value=TRIM(cl_value)//TRIM(fct_str(id_value(il_len)))//"/)"

      CALL logger_trace( &
      &  " ATT INIT: attribute name: "//TRIM(ADJUSTL(cd_name))//&
      &  " attribute value "//TRIM(ADJUSTL(cl_value)) )      

      att__init_i4%c_name=TRIM(ADJUSTL(cd_name))

      IF( PRESENT(id_type) )THEN
         att__init_i4%i_type=id_type
      ELSE
         att__init_i4%i_type=NF90_INT
      ENDIF

      IF( ASSOCIATED(att__init_i4%d_value) )THEN
         DEALLOCATE(att__init_i4%d_value)
      ENDIF
      ALLOCATE(att__init_i4%d_value(il_len))

      att__init_i4%d_value(:)=REAL(id_value(:),dp)
      att__init_i4%i_len=il_len

   END FUNCTION att__init_i4
   !-------------------------------------------------------------------
   !> @brief This function initialize an attribute structure with 
   !> integer(4) value. 
   !> @details
   !> Optionaly you could specify the type of the variable to be saved.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !>
   !> @param[in] cd_name   attribute name
   !> @param[in] id_value  attribute value
   !> @param[in] id_type   type of the variable to be saved
   !> @return attribute structure
   !-------------------------------------------------------------------
   TYPE(TATT) FUNCTION att__init_i4_0d( cd_name, id_value, id_type )
      IMPLICIT NONE
      ! Argument
      CHARACTER(LEN=*), INTENT(IN) :: cd_name
      INTEGER(i4),      INTENT(IN) :: id_value
      INTEGER(i4)     , INTENT(IN), OPTIONAL :: id_type

      !local value
      CHARACTER(LEN=lc) :: cl_value
      !----------------------------------------------------------------

      ! clean attribute
      CALL att_clean(att__init_i4_0d)
      
      cl_value="(/"//TRIM(fct_str(id_value))//"/)"

      CALL logger_trace( &
      &  " ATT INIT: attribute name: "//TRIM(ADJUSTL(cd_name))//&
      &  " attibute value "//TRIM(ADJUSTL(cl_value)) )      

      att__init_i4_0d%c_name=TRIM(ADJUSTL(cd_name))

      IF( PRESENT(id_type) )THEN
         att__init_i4_0d%i_type=id_type
      ELSE
         att__init_i4_0d%i_type=NF90_INT
      ENDIF

      IF( ASSOCIATED(att__init_i4_0d%d_value) )THEN
         DEALLOCATE(att__init_i4_0d%d_value)
      ENDIF
      ALLOCATE(att__init_i4_0d%d_value(1))

      att__init_i4_0d%d_value(1)=REAL(id_value,dp)
      att__init_i4_0d%i_len=1

   END FUNCTION att__init_i4_0d
   !-------------------------------------------------------------------
   !> @brief This function initialize an attribute structure with array 
   !> of integer(8) value.
   !> @details
   !> Optionaly you could specify the type of the variable to be saved.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] cd_name   attribute name
   !> @param[in] kd_value  attribute value
   !> @param[in] id_type   type of the variable to be saved
   !> @return attribute structure
   !-------------------------------------------------------------------
   TYPE(TATT) FUNCTION att__init_i8( cd_name, kd_value, id_type )
      IMPLICIT NONE
      ! Argument
      CHARACTER(LEN=*),               INTENT(IN) :: cd_name
      INTEGER(i8),      DIMENSION(:), INTENT(IN) :: kd_value
      INTEGER(i4)                   , INTENT(IN), OPTIONAL :: id_type

      ! local value
      INTEGER(i4)       :: il_len
      CHARACTER(LEN=lc) :: cl_value

      ! loop indices
      INTEGER(i4) :: ji
      !----------------------------------------------------------------

      ! clean attribute
      CALL att_clean(att__init_i8)
      
      ! array size
      il_len=size(kd_value(:))

      cl_value="(/"
      DO ji=1,il_len
         cl_value=TRIM(cl_value)//TRIM(fct_str(kd_value(ji)))//","
      ENDDO
      cl_value=TRIM(cl_value)//TRIM(fct_str(kd_value(il_len)))//"/)"

      CALL logger_trace( &
      &  " ATT INIT: attribute name: "//TRIM(ADJUSTL(cd_name))//&
      &  " attibute value "//TRIM(ADJUSTL(cl_value)) )      

      att__init_i8%c_name=TRIM(ADJUSTL(cd_name))

      IF( PRESENT(id_type) )THEN
         att__init_i8%i_type=id_type
      ELSE
         att__init_i8%i_type=NF90_INT
      ENDIF

      IF( ASSOCIATED(att__init_i8%d_value) )THEN
         DEALLOCATE(att__init_i8%d_value)
      ENDIF
      ALLOCATE(att__init_i8%d_value(il_len))

      att__init_i8%d_value(:)=REAL(kd_value(:),dp)
      att__init_i8%i_len=il_len

   END FUNCTION att__init_i8
   !-------------------------------------------------------------------
   !> @brief This function initialize an attribute structure with 
   !> integer(8) value. 
   !> @details
   !> Optionaly you could specify the type of the variable to be saved.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] cd_name   attribute name
   !> @param[in] kd_value  attribute value
   !> @param[in] id_type   type of the variable to be saved
   !> @return attribute structure
   !-------------------------------------------------------------------
   TYPE(TATT) FUNCTION att__init_i8_0d( cd_name, kd_value, id_type )
      IMPLICIT NONE
      ! Argument
      CHARACTER(LEN=*), INTENT(IN) :: cd_name
      INTEGER(i8),      INTENT(IN) :: kd_value
      INTEGER(i4)     , INTENT(IN), OPTIONAL :: id_type

      ! local value
      CHARACTER(LEN=lc) :: cl_value
      !----------------------------------------------------------------

      ! clean attribute
      CALL att_clean(att__init_i8_0d)
      
      cl_value="(/"//TRIM(fct_str(kd_value))//"/)"

      CALL logger_trace( &
      &  " ATT INIT: attribute name: "//TRIM(ADJUSTL(cd_name))//&
      &  " attibute value "//TRIM(ADJUSTL(cl_value)) )      

      att__init_i8_0d%c_name=TRIM(ADJUSTL(cd_name))

      IF( PRESENT(id_type) )THEN
         att__init_i8_0d%i_type=id_type
      ELSE
         att__init_i8_0d%i_type=NF90_INT
      ENDIF

      IF( ASSOCIATED(att__init_i8_0d%d_value) )THEN
         DEALLOCATE(att__init_i8_0d%d_value)
      ENDIF
      ALLOCATE(att__init_i8_0d%d_value(1))

      att__init_i8_0d%d_value(1)=REAL(kd_value,dp)
      att__init_i8_0d%i_len=1

   END FUNCTION att__init_i8_0d
   !-------------------------------------------------------------------
   !> @brief This subroutine print informations of an array of attribute. 
   !>
   !> @author J.Paul
   !> @date June, 2014 - Initial Version
   !>
   !> @param[in] td_att array of attribute structure
   !-------------------------------------------------------------------
   SUBROUTINE att__print_arr(td_att)
      IMPLICIT NONE

      ! Argument      
      TYPE(TATT), DIMENSION(:), INTENT(IN) :: td_att

      ! loop indices
      INTEGER(i4) :: ji
      !----------------------------------------------------------------

      DO ji=1,SIZE(td_att(:))
         CALL att_print(td_att(ji))
      ENDDO

   END SUBROUTINE att__print_arr
   !-------------------------------------------------------------------
   !> @brief This subroutine print attribute information.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !> @date September, 2014 
   !> - take into account type of attribute.
   !
   !> @param[in] td_att attribute structure
   !-------------------------------------------------------------------
   SUBROUTINE att__print_unit(td_att)
      IMPLICIT NONE

      ! Argument      
      TYPE(TATT), INTENT(IN) :: td_att

      ! local vairbale
      CHARACTER(LEN=lc) :: cl_type
      CHARACTER(LEN=lc) :: cl_value

      INTEGER(i8)       :: kl_tmp
      INTEGER(i2)       :: sl_tmp
      INTEGER(i1)       :: bl_tmp
      REAL(sp)          :: rl_tmp
      REAL(dp)          :: dl_tmp

      ! loop indices
      INTEGER(i4) :: ji
      !----------------------------------------------------------------

         SELECT CASE( td_att%i_type )

            CASE(NF90_CHAR)
               cl_type='CHAR'
            CASE(NF90_BYTE)
               cl_type='BYTE'
            CASE(NF90_SHORT)
               cl_type='SHORT'
            CASE(NF90_INT)
               cl_type='INT'
            CASE(NF90_FLOAT)
               cl_type='FLOAT'
            CASE(NF90_DOUBLE)
               cl_type='DOUBLE'
            CASE DEFAULT
               cl_type=''

         END SELECT

         SELECT CASE( td_att%i_type )

            CASE(NF90_CHAR)

               cl_value=td_att%c_value

            CASE(NF90_BYTE)   
               IF( td_att%i_len > 1 )THEN
                  cl_value='(/'
                  DO ji=1,td_att%i_len-1
                     bl_tmp=INT(td_att%d_value(ji),i1)
                     cl_value=TRIM(cl_value)//TRIM(fct_str(bl_tmp))//','
                  ENDDO
                  bl_tmp=INT(td_att%d_value(td_att%i_len),i1)
                  cl_value=TRIM(cl_value)//TRIM(fct_str(bl_tmp))//'/)'
               ELSE
                  cl_value='(/'//TRIM(fct_str(td_att%d_value(1)))//'/)'
               ENDIF

            CASE(NF90_SHORT)   
               IF( td_att%i_len > 1 )THEN
                  cl_value='(/'
                  DO ji=1,td_att%i_len-1
                     sl_tmp=INT(td_att%d_value(ji),i2)
                     cl_value=TRIM(cl_value)//TRIM(fct_str(sl_tmp))//','
                  ENDDO
                  sl_tmp=INT(td_att%d_value(td_att%i_len),i2)
                  cl_value=TRIM(cl_value)//TRIM(fct_str(sl_tmp))//'/)'
               ELSE
                  cl_value='(/'//TRIM(fct_str(td_att%d_value(1)))//'/)'
               ENDIF

            CASE(NF90_INT)   
               IF( td_att%i_len > 1 )THEN
                  cl_value='(/'
                  DO ji=1,td_att%i_len-1
                     kl_tmp=INT(td_att%d_value(ji),i8)
                     cl_value=TRIM(cl_value)//TRIM(fct_str(kl_tmp))//','
                  ENDDO
                  kl_tmp=INT(td_att%d_value(td_att%i_len),i8)
                  cl_value=TRIM(cl_value)//TRIM(fct_str(kl_tmp))//'/)'
               ELSE
                  cl_value='(/'//TRIM(fct_str(td_att%d_value(1)))//'/)'
               ENDIF

            CASE(NF90_FLOAT)   
               IF( td_att%i_len > 1 )THEN
                  cl_value='(/'
                  DO ji=1,td_att%i_len-1
                     rl_tmp=REAL(td_att%d_value(ji),sp)
                     cl_value=TRIM(cl_value)//TRIM(fct_str(rl_tmp))//','
                  ENDDO
                  rl_tmp=REAL(td_att%d_value(td_att%i_len),sp)
                  cl_value=TRIM(cl_value)//TRIM(fct_str(rl_tmp))//'/)'
               ELSE
                  cl_value='(/'//TRIM(fct_str(td_att%d_value(1)))//'/)'
               ENDIF

            CASE(NF90_DOUBLE)   
               IF( td_att%i_len > 1 )THEN
                  cl_value='(/'
                  DO ji=1,td_att%i_len-1
                     dl_tmp=REAL(td_att%d_value(ji),dp)
                     cl_value=TRIM(cl_value)//TRIM(fct_str(dl_tmp))//','
                  ENDDO
                  dl_tmp=REAL(td_att%d_value(td_att%i_len),dp)
                  cl_value=TRIM(cl_value)//TRIM(fct_str(dl_tmp))//'/)'
               ELSE
                  cl_value='(/'//TRIM(fct_str(td_att%d_value(1)))//'/)'
               ENDIF

            CASE DEFAULT
               cl_value="none"

         END SELECT

         WRITE(*,'((3x,a,a),(/6x,a,i2.2),(a,a),(a,a))')&
         &        " attribute : ",TRIM(ADJUSTL(td_att%c_name)),      &
         &        " id : ",td_att%i_id,                         &
         &        " type : ",TRIM(ADJUSTL(cl_type)),            &
         &        " value : ",TRIM(ADJUSTL(cl_value))

   END SUBROUTINE att__print_unit
   !-------------------------------------------------------------------
   !> @brief 
   !>  This subroutine clean attribute strcuture.
   !
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[inout] td_att attribute strcuture
   !-------------------------------------------------------------------
   SUBROUTINE att__clean_unit( td_att )
      IMPLICIT NONE
      ! Argument
      TYPE(TATT),  INTENT(INOUT) :: td_att

      ! local variable
      TYPE(TATT) :: tl_att ! empty attribute structure
      !----------------------------------------------------------------

      CALL logger_trace( &
      &  " CLEAN: reset attribute "//TRIM(td_att%c_name) )

      IF( ASSOCIATED(td_att%d_value) )THEN
         ! clean value
         DEALLOCATE(td_att%d_value)
      ENDIF

      ! replace by empty structure
      td_att=att_copy(tl_att)

   END SUBROUTINE att__clean_unit
   !-------------------------------------------------------------------
   !> @brief 
   !>  This subroutine clean array of attribute strcuture.
   !
   !> @author J.Paul
   !> @date September, 2014 - Initial Version
   !
   !> @param[inout] td_att attribute strcuture
   !-------------------------------------------------------------------
   SUBROUTINE att__clean_arr( td_att )
      IMPLICIT NONE
      ! Argument
      TYPE(TATT), DIMENSION(:), INTENT(INOUT) :: td_att

      ! local variable
      ! loop indices
      INTEGER(i4) :: ji
      !----------------------------------------------------------------

      DO ji=SIZE(td_att(:)),1,-1
         CALL att_clean(td_att(ji) )
      ENDDO

   END SUBROUTINE att__clean_arr
   !-------------------------------------------------------------------
   !> @brief This subroutine fill dummy attribute array
   !
   !> @author J.Paul
   !> @date September, 2015 - Initial Version
   !> @date Marsh, 2016
   !> - close file (bugfix)
   !
   !> @param[in] cd_dummy dummy configuration file
   !-------------------------------------------------------------------
   SUBROUTINE att_get_dummy( cd_dummy )
      IMPLICIT NONE
      ! Argument
      CHARACTER(LEN=*), INTENT(IN) :: cd_dummy

      ! local variable
      INTEGER(i4)   :: il_fileid
      INTEGER(i4)   :: il_status

      LOGICAL       :: ll_exist

      ! loop indices
      ! namelist
      CHARACTER(LEN=lc), DIMENSION(ip_maxdumcfg) :: cn_dumvar
      CHARACTER(LEN=lc), DIMENSION(ip_maxdumcfg) :: cn_dumdim
      CHARACTER(LEN=lc), DIMENSION(ip_maxdumcfg) :: cn_dumatt

      !----------------------------------------------------------------
      NAMELIST /namdum/ &   !< dummy namelist
      &  cn_dumvar, &       !< variable  name
      &  cn_dumdim, &       !< dimension name
      &  cn_dumatt          !< attribute name
      !----------------------------------------------------------------

      ! init
      cm_dumatt(:)=''

      ! read namelist
      INQUIRE(FILE=TRIM(cd_dummy), EXIST=ll_exist)
      IF( ll_exist )THEN
    
         il_fileid=fct_getunit()
   
         OPEN( il_fileid, FILE=TRIM(cd_dummy), &
         &                FORM='FORMATTED',       &
         &                ACCESS='SEQUENTIAL',    &
         &                STATUS='OLD',           &
         &                ACTION='READ',          &
         &                IOSTAT=il_status)
         CALL fct_err(il_status)
         IF( il_status /= 0 )THEN
            CALL logger_fatal("DIM GET DUMMY: opening "//TRIM(cd_dummy))
         ENDIF
   
         READ( il_fileid, NML = namdum )
         cm_dumatt(:)=cn_dumatt(:)

         CLOSE( il_fileid )

      ENDIF
   
   END SUBROUTINE att_get_dummy
   !-------------------------------------------------------------------
   !> @brief This function check if attribute is defined as dummy attribute
   !> in configuraton file
   !>
   !> @author J.Paul
   !> @date September, 2015 - Initial Version
   !
   !> @param[in] td_att attribute structure
   !> @return true if attribute is dummy attribute
   !-------------------------------------------------------------------
   FUNCTION att_is_dummy(td_att)
      IMPLICIT NONE

      ! Argument      
      TYPE(TATT), INTENT(IN) :: td_att
      
      ! function
      LOGICAL :: att_is_dummy
      
      ! loop indices
      INTEGER(i4) :: ji
      !----------------------------------------------------------------

      att_is_dummy=.FALSE.
      DO ji=1,ip_maxdumcfg
         IF( fct_lower(td_att%c_name) == fct_lower(cm_dumatt(ji)) )THEN
            att_is_dummy=.TRUE.
            EXIT
         ENDIF
      ENDDO

   END FUNCTION att_is_dummy
END MODULE att

