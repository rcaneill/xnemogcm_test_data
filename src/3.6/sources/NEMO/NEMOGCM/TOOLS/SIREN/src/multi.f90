!----------------------------------------------------------------------
! NEMO system team, System and Interface for oceanic RElocable Nesting
!----------------------------------------------------------------------
!
! MODULE: multi
!
! DESCRIPTION:
!> This module manage multi file structure.
!
!> @details
!>    define type TMULTI:<br/>
!> @code
!>    TYPE(TMULTI) :: tl_multi
!> @endcode
!>
!>    to initialize a multi-file structure:<br/>
!> @code
!>    tl_multi=multi_init(cd_varfile(:))
!> @endcode
!>       - cd_varfile : array of variable with file path 
!>       ('var1:file1','var2:file2')<br/>
!>          file path could be replaced by a matrix of value.<br/>
!>          separators used to defined matrix are:
!>             - ',' for line
!>             - '/' for row
!>             - '\' for level<br/>
!>             Example:<br/>
!>                - 'var1:3,2,3/1,4,5'
!>                - 3,2,3/1,4,5  =>  
!>                      @f$ \left( \begin{array}{ccc}
!>                           3 & 2 & 3 \\
!>                           1 & 4 & 5 \end{array} \right) @f$<br/>
!> 
!>    to get the number of mpp file in mutli file structure:<br/>
!>    - tl_multi\%i_nmpp
!>
!>    to get the total number of variable in mutli file structure:<br/>
!>    - tl_multi\%i_nvar
!>
!>    @note number of variable and number of file could differ cause several variable
!>    could be in the same file.
!>
!>    to get array of mpp structure in mutli file structure:<br/>
!>    - tl_multi\%t_mpp(:)
!>
!>    to print information about multi structure:<br/>
!> @code
!>    CALL multi_print(td_multi)
!> @endcode
!>
!>    to clean multi file strucutre:<br/>
!> @code
!>    CALL multi_clean(td_multi)
!> @endcode
!>       - td_multi is multi file structure
!>
!> @author
!>  J.Paul
! REVISION HISTORY:
!> @date November, 2013 - Initial Version
!> @date October, 2014
!> - use mpp file structure instead of file
!> @date November, 2014 
!> - Fix memory leaks bug
!
!> @note Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
!----------------------------------------------------------------------
MODULE multi
   USE kind                            ! F90 kind parameter
   USE logger                          ! log file manager
   USE fct                             ! basic useful function
   USE dim                             ! dimension manager
   USE var                             ! variable manager
   USE file                            ! file manager
   USE iom                             ! I/O manager
   USE mpp                             ! MPP manager
   USE iom_mpp                         ! MPP I/O manager

   IMPLICIT NONE
   ! NOTE_avoid_public_variables_if_possible

   ! type and variable
   PUBLIC :: TMULTI       !< multi file structure

   ! function and subroutine
   PUBLIC :: multi_copy        !< copy multi structure
   PUBLIC :: multi_init        !< initialise multi structure
   PUBLIC :: multi_clean       !< clean multi strcuture
   PUBLIC :: multi_print       !< print information about milti structure

   PUBLIC :: multi__add_mpp    !< add file strucutre to multi file structure
   PRIVATE :: multi__copy_unit !< copy multi file structure

   TYPE TMULTI !< multi file structure
      ! general 
      INTEGER(i4)                         :: i_nmpp  = 0         !< number of mpp files 
      INTEGER(i4)                         :: i_nvar  = 0         !< total number of variables
      TYPE(TMPP) , DIMENSION(:), POINTER  :: t_mpp => NULL()     !< mpp files composing multi
   END TYPE

   INTERFACE multi_copy
      MODULE PROCEDURE multi__copy_unit   ! copy multi file structure
   END INTERFACE   

CONTAINS
   !-------------------------------------------------------------------
   !> @brief
   !> This function copy multi mpp structure in another one
   !> @details 
   !> file variable value are copied in a temporary array, 
   !> so input and output file structure value do not point on the same 
   !> "memory cell", and so on are independant. 
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
   !>    - use function instead of overload assignment operator (to avoid memory leak)
   !>
   !> @param[in] td_multi    mpp structure
   !> @return copy of input multi structure
   !-------------------------------------------------------------------
   FUNCTION multi__copy_unit( td_multi )
      IMPLICIT NONE
      ! Argument
      TYPE(TMULTI), INTENT(IN)  :: td_multi
      ! function
      TYPE(TMULTI) :: multi__copy_unit

      ! local variable
      TYPE(TMPP) :: tl_mpp

      ! loop indices
      INTEGER(i4) :: ji
      !----------------------------------------------------------------

      multi__copy_unit%i_nmpp = td_multi%i_nmpp
      multi__copy_unit%i_nvar = td_multi%i_nvar

      ! copy variable structure
      IF( ASSOCIATED(multi__copy_unit%t_mpp) )THEN
         CALL mpp_clean(multi__copy_unit%t_mpp(:))
         DEALLOCATE(multi__copy_unit%t_mpp)
      ENDIF
      IF( ASSOCIATED(td_multi%t_mpp) .AND. multi__copy_unit%i_nmpp > 0 )THEN
         ALLOCATE( multi__copy_unit%t_mpp(multi__copy_unit%i_nmpp) )
         DO ji=1,multi__copy_unit%i_nmpp
            tl_mpp = mpp_copy(td_multi%t_mpp(ji))
            multi__copy_unit%t_mpp(ji) = mpp_copy(tl_mpp)
         ENDDO
         ! clean
         CALL mpp_clean(tl_mpp)
      ENDIF

   END FUNCTION multi__copy_unit
   !-------------------------------------------------------------------
   !> @brief This subroutine initialize multi file structure.
   !>
   !> @details
   !> if variable name is 'all', add all the variable of the file in mutli file
   !> structure.
   !> @note if first character of filename is numeric, assume matrix is given as
   !> input.<br/>
   !> create pseudo file named 'data-*', with matrix read as variable value.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !> @date July, 2015 
   !> - check if variable to be read is in file
   !> @date January, 2016
   !> - read variable dimensions
   !>
   !> @param[in] cd_varfile   variable location information (from namelist) 
   !> @return multi file structure
   !-------------------------------------------------------------------
   FUNCTION multi_init(cd_varfile)
      IMPLICIT NONE

      ! Argument
      CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: cd_varfile

      ! function
      TYPE(TMULTI) :: multi_init

      ! local variable
      CHARACTER(LEN=lc)                :: cl_name
      CHARACTER(LEN=lc)                :: cl_lower
      CHARACTER(LEN=lc)                :: cl_file
      CHARACTER(LEN=lc)                :: cl_matrix

      INTEGER(i4)                      :: il_nvar
      INTEGER(i4)                      :: il_varid

      LOGICAL                          :: ll_dim

      TYPE(TDIM), DIMENSION(ip_maxdim) :: tl_dim

      TYPE(TVAR)                       :: tl_var

      TYPE(TMPP)                       :: tl_mpp

      ! loop indices
      INTEGER(i4) :: ji
      INTEGER(i4) :: jj
      INTEGER(i4) :: jk
      !----------------------------------------------------------------

      ji=1
      DO WHILE( TRIM(cd_varfile(ji)) /= '' )

         il_nvar=0
         cl_name=fct_split(cd_varfile(ji),1,':')
         cl_lower=fct_lower(cl_name)
         cl_file=fct_split(cd_varfile(ji),2,':')

         IF( LEN(TRIM(cl_file)) == lc )THEN
            CALL logger_fatal("MULTI INIT: file name too long (>"//&
            &          TRIM(fct_str(lc))//"). check namelist.")
         ENDIF

         IF( TRIM(cl_lower) /= '' )THEN
            IF( TRIM(cl_file) /= '' )THEN
               cl_matrix=''
               IF( fct_is_num(cl_file(1:1)) )THEN
                  cl_matrix=TRIM(cl_file)
                  WRITE(cl_file,'(a,i2.2)')'data-',ji

                  tl_var=var_init(TRIM(cl_name))
                  CALL var_read_matrix(tl_var, cl_matrix)

                  ! create mpp structure
                  tl_mpp=mpp_init(TRIM(cl_file), tl_var)

                  ! add variable
                  CALL mpp_add_var(tl_mpp,tl_var)

                  ! number of variable
                  il_nvar=il_nvar+1

               ELSE

                  ! 
                  tl_mpp=mpp_init( file_init(TRIM(cl_file)) )
                  ! define variable
                  IF( TRIM(fct_lower(cl_lower)) /= 'all' )THEN

                     ! check if variable is in file
                     il_varid=var_get_index(tl_mpp%t_proc(1)%t_var(:),cl_lower)
                     IF( il_varid == 0 )THEN
                        CALL logger_fatal("MULTI INIT: variable "//&
                           & TRIM(cl_name)//" not in file "//&
                           & TRIM(cl_file) )
                     ENDIF

                     ! get (global) variable dimension
                     tl_dim(jp_I)=dim_copy(tl_mpp%t_dim(jp_I))
                     tl_dim(jp_J)=dim_copy(tl_mpp%t_dim(jp_J))
                     tl_dim(jp_K)=dim_copy(tl_mpp%t_proc(1)%t_var(il_varid)%t_dim(jp_K))
                     tl_dim(jp_L)=dim_copy(tl_mpp%t_proc(1)%t_var(il_varid)%t_dim(jp_L))

                     ! clean all varible
                     CALL mpp_del_var(tl_mpp)

                     tl_var=var_init(TRIM(cl_lower), td_dim=tl_dim(:))

                     ! add variable
                     CALL mpp_add_var(tl_mpp,tl_var)

                     ! number of variable
                     il_nvar=il_nvar+1

                     ! clean structure
                     CALL var_clean(tl_var)

                  ELSE ! cl_lower == 'all'

                     DO jk=tl_mpp%t_proc(1)%i_nvar,1,-1

                        ! check if variable is dimension
                        ll_dim=.FALSE.
                        DO jj=1,ip_maxdim
                           IF( TRIM(tl_mpp%t_proc(1)%t_dim(jj)%c_name) == &
                           &   TRIM(tl_mpp%t_proc(1)%t_var(jk)%c_name) )THEN
                              ll_dim=.TRUE.
                              CALL logger_trace("MULTI INIT: "//&
                              &  TRIM(tl_mpp%t_proc(1)%t_var(jk)%c_name)//&
                              &  ' is var dimension')
                              EXIT
                           ENDIF
                        ENDDO
                        ! do not use variable dimension
                        IF( ll_dim )THEN
                           tl_var=var_init( &
                           &  TRIM(tl_mpp%t_proc(1)%t_var(jk)%c_name) )
                           ! delete variable
                           CALL mpp_del_var(tl_mpp,tl_var)
                           ! clean structure
                           CALL var_clean(tl_var)
                        ELSE
                           ! number of variable
                           il_nvar=il_nvar+1
                        ENDIF

                     ENDDO

                  ENDIF

               ENDIF

               CALL multi__add_mpp(multi_init, tl_mpp) 

               ! update total number of variable
               multi_init%i_nvar=multi_init%i_nvar+il_nvar

               ! clean
               CALL mpp_clean(tl_mpp)

            ELSE
               CALL logger_error("MULTI INIT: file name matching variable "//&
               &                 TRIM(cl_name)//" is empty. check namelist.")
            ENDIF
         ELSE
            CALL logger_error("MULTI INIT: variable name "//&
            &                 "is empty. check namelist.")
         ENDIF

         ji=ji+1
      ENDDO

   END FUNCTION multi_init
   !-------------------------------------------------------------------
   !> @brief This subroutine clean multi file strucutre.
   !
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] td_multi  multi file structure
   !-------------------------------------------------------------------
   SUBROUTINE multi_clean(td_multi)
      IMPLICIT NONE

      ! Argument      
      TYPE(TMULTI), INTENT(INOUT) :: td_multi

      ! local variable
      TYPE(TMULTI) :: tl_multi ! empty multi file structure

      ! loop indices
      !----------------------------------------------------------------

      CALL logger_info( " CLEAN: reset multi file " )

      IF( ASSOCIATED( td_multi%t_mpp ) )THEN
         CALL mpp_clean(td_multi%t_mpp(:))
         DEALLOCATE(td_multi%t_mpp)
      ENDIF

      ! replace by empty structure
      td_multi=multi_copy(tl_multi)

   END SUBROUTINE multi_clean
   !-------------------------------------------------------------------
   !> @brief This subroutine print some information about mpp strucutre.
   !
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] td_multi multi file structure
   !-------------------------------------------------------------------
   SUBROUTINE multi_print(td_multi)
      IMPLICIT NONE

      ! Argument      
      TYPE(TMULTI), INTENT(IN) :: td_multi

      ! local variable

      ! loop indices
      INTEGER(i4) :: ji
      INTEGER(i4) :: jj
      !----------------------------------------------------------------

      ! print file
      IF( td_multi%i_nmpp /= 0 .AND. ASSOCIATED(td_multi%t_mpp) )THEN
         WRITE(*,'(/a,i3)') 'MULTI: total number of file(s): ',&
         &  td_multi%i_nmpp
         WRITE(*,'(6x,a,i3)') ' total number of variable(s): ',&
         &  td_multi%i_nvar
         DO ji=1,td_multi%i_nmpp
            WRITE(*,'(3x,3a)') 'FILE ',TRIM(td_multi%t_mpp(ji)%c_name),&
            & ' CONTAINS'
            DO jj=1,td_multi%t_mpp(ji)%t_proc(1)%i_nvar
               IF( ASSOCIATED(td_multi%t_mpp(ji)%t_proc(1)%t_var) )THEN
                  WRITE(*,'(6x,a)') &
                  &  TRIM(td_multi%t_mpp(ji)%t_proc(1)%t_var(jj)%c_name)
               ENDIF
            ENDDO
         ENDDO
      ENDIF

   END SUBROUTINE multi_print
   !-------------------------------------------------------------------
   !> @brief
   !>    This subroutine add file to multi file structure.
   !>
   !> @detail
   !
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !> @date October, 2014
   !> - use mpp file structure instead of file
   !
   !> @param[inout] td_multi  multi mpp file strcuture
   !> @param[in]    td_mpp    mpp file strcuture
   !> @return mpp file id in multi mpp file structure
   !-------------------------------------------------------------------
   SUBROUTINE multi__add_mpp( td_multi, td_mpp )
      IMPLICIT NONE
      ! Argument
      TYPE(TMULTI), INTENT(INOUT) :: td_multi
      TYPE(TMPP)  , INTENT(IN)    :: td_mpp

      ! local variable
      INTEGER(i4) :: il_status
      INTEGER(i4) :: il_mppid
      
      TYPE(TMPP), DIMENSION(:), ALLOCATABLE :: tl_mpp

      ! loop indices
      INTEGER(i4) :: ji
      !----------------------------------------------------------------

      il_mppid=0
      IF( ASSOCIATED(td_multi%t_mpp) )THEN
         il_mppid=mpp_get_index(td_multi%t_mpp(:),TRIM(td_mpp%c_name))
      ENDIF

      IF( il_mppid /= 0 )THEN

            CALL logger_debug( " MULTI ADD FILE: mpp file "//TRIM(td_mpp%c_name)//&
            &               " already in multi mpp file structure")

            ! add new variable
            DO ji=1,td_mpp%t_proc(1)%i_nvar
               CALL mpp_add_var(td_multi%t_mpp(il_mppid), td_mpp%t_proc(1)%t_var(ji))
            ENDDO

      ELSE
 
         CALL logger_trace("MULTI ADD MPP: add mpp "//&
         &               TRIM(td_mpp%c_name)//" in multi mpp file structure")

         IF( td_multi%i_nmpp > 0 )THEN
            ! 
            ! already other mpp file in multi file structure
            ALLOCATE( tl_mpp(td_multi%i_nmpp), stat=il_status )
            IF(il_status /= 0 )THEN

               CALL logger_error( " MULTI ADD MPP FILE: not enough space to put &
               &               mpp file in multi mpp file structure")

            ELSE
               ! save temporary multi file structure
               tl_mpp(:)=mpp_copy(td_multi%t_mpp(:))

               CALL mpp_clean(td_multi%t_mpp(:))
               DEALLOCATE( td_multi%t_mpp )
               ALLOCATE( td_multi%t_mpp(td_multi%i_nmpp+1), stat=il_status)
               IF(il_status /= 0 )THEN

                  CALL logger_error( " MULTI ADD MPP FILE: not enough space "//&
                  &  "to put mpp file in multi mpp file structure ")

               ENDIF

               ! copy mpp file in multi mpp file before
               td_multi%t_mpp(1:td_multi%i_nmpp) = mpp_copy(tl_mpp(:))

               ! clean
               CALL mpp_clean(tl_mpp(:))
               DEALLOCATE(tl_mpp)
            ENDIF

         ELSE
            ! no file in multi file structure
            IF( ASSOCIATED(td_multi%t_mpp) )THEN
               CALL mpp_clean(td_multi%t_mpp(:))
               DEALLOCATE(td_multi%t_mpp)
            ENDIF
            ALLOCATE( td_multi%t_mpp(td_multi%i_nmpp+1), stat=il_status )
            IF(il_status /= 0 )THEN

               CALL logger_error( " MULTI ADD MPP FILE: not enough space "//&
               &  "to put mpp file in multi mpp file structure " )

            ENDIF
         ENDIF

         ! update number of mpp
         td_multi%i_nmpp=td_multi%i_nmpp+1

         ! add new mpp
         td_multi%t_mpp(td_multi%i_nmpp)=mpp_copy(td_mpp)

      ENDIF
   END SUBROUTINE multi__add_mpp
END MODULE multi

