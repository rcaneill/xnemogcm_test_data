!----------------------------------------------------------------------
! NEMO system team, System and Interface for oceanic RElocable Nesting
!----------------------------------------------------------------------
!
! PROGRAM: create_bathy
!
! DESCRIPTION:
!> @file
!> @brief 
!> This program creates fine grid bathymetry file.
!>
!> @details
!> @section sec1 method
!> Bathymetry could be extracted from fine grid Bathymetry file, interpolated
!> from coarse grid Bathymetry file, or manually written.
!>
!> @section sec2 how to
!>    to create fine grid bathymetry file:<br/>
!> @code{.sh}
!>    ./SIREN/bin/create_bathy create_bathy.nam
!> @endcode
!> <br/>    
!> \image html  bathy_40.png 
!> <center>\image latex bathy_30.png
!> </center>
!>
!> @note 
!>    you could find a template of the namelist in templates directory.
!>
!>    create_bathy.nam contains 7 namelists:<br/>
!>       - logger namelist (namlog)
!>       - config namelist (namcfg)
!>       - coarse grid namelist (namcrs)
!>       - fine grid namelist (namfin)
!>       - variable namelist (namvar)
!>       - nesting namelist (namnst)
!>       - output namelist (namout)
!>    
!>    * _logger namelist (namlog)_:<br/>
!>       - cn_logfile   : log filename
!>       - cn_verbosity : verbosity ('trace','debug','info',
!> 'warning','error','fatal','none')
!>       - in_maxerror  : maximum number of error allowed
!>
!>    * _config namelist (namcfg)_:<br/>
!>       - cn_varcfg : variable configuration file 
!> (see ./SIREN/cfg/variable.cfg)
!>       - cn_dimcfg : dimension configuration file. defines dimension allowed
!> (see ./SIREN/cfg/dimension.cfg).
!>       - cn_dumcfg : useless (dummy) configuration file, for useless 
!> dimension or variable (see ./SIREN/cfg/dummy.cfg).
!>
!>    * _coarse grid namelist (namcrs)_:<br/>
!>       - cn_coord0 : coordinate file
!>       - in_perio0 : NEMO periodicity index (see Model Boundary Condition in
!> [NEMO documentation](http://www.nemo-ocean.eu/About-NEMO/Reference-manuals))
!>
!>    * _fine grid namelist (namfin)_:<br/>
!>       - cn_coord1 : coordinate file
!>       - in_perio1 : periodicity index
!>       - ln_fillclosed : fill closed sea or not (default is .TRUE.)
!>
!>    * _variable namelist (namvar)_:<br/>
!>       - cn_varfile : list of variable, and corresponding file.<br/> 
!>          *cn_varfile* is the path and filename of the file where find
!>          variable.
!>          @note 
!>             *cn_varfile* could be a matrix of value, if you want to filled
!>             manually variable value.<br/>
!>             the variable array of value is split into equal subdomain.<br/>
!>             Each subdomain is filled with the corresponding value 
!>             of the matrix.<br/>          
!>             separators used to defined matrix are:
!>                - ',' for line
!>                - '/' for row
!>                Example:<br/>
!>                   3,2,3/1,4,5  =>  @f$ \left( \begin{array}{ccc}
!>                                         3 & 2 & 3 \\
!>                                         1 & 4 & 5 \end{array} \right) @f$
!>
!>          Examples: 
!>             - 'Bathymetry:gridT.nc'
!>             - 'Bathymetry:5000,5000,5000/5000,3000,5000/5000,5000,5000'
!>
!>       - cn_varinfo : list of variable and extra information about request(s) 
!>       to be used.<br/>
!>          each elements of *cn_varinfo* is a string character
!>          (separated by ',').<br/>
!>          it is composed of the variable name follow by ':', 
!>          then request(s) to be used on this variable.<br/> 
!>          request could be:
!>             - int = interpolation method
!>             - ext = extrapolation method
!>             - flt = filter method
!>             - min = minimum value
!>             - max = maximum value
!>             - unt = new units
!>             - unf = unit scale factor (linked to new units)
!>
!>                requests must be separated by ';'.<br/>
!>                order of requests does not matter.<br/>
!>
!>          informations about available method could be find in @ref interp,
!>          @ref extrap and @ref filter modules.<br/>
!>          Example: 'Bathymetry: flt=2*hamming(2,3); min=0'
!>          @note 
!>             If you do not specify a method which is required, 
!>             default one is apply.
!>          @warning 
!>             variable name must be __Bathymetry__ here.
!>
!>    * _nesting namelist (namnst)_:<br/>
!>       - in_rhoi  : refinement factor in i-direction
!>       - in_rhoj  : refinement factor in j-direction
!>       @note 
!>          coarse grid indices will be deduced from fine grid
!>          coordinate file.
!>
!>    * _output namelist (namout)_:<br/>
!>       - cn_fileout : output bathymetry file
!>
!> @author J.Paul
! REVISION HISTORY:
!> @date November, 2013 - Initial Version
!> @date Sepember, 2014 
!> - add header for user
!> - Bug fix, compute offset depending of grid point
!> @date June, 2015
!> - extrapolate all land points.
!> - allow to change unit.
!> @date September, 2015
!> - manage useless (dummy) variable, attributes, and dimension
!> @date January,2016
!> - add create_bathy_check_depth as in create_boundary
!> - add create_bathy_check_time  as in create_boundary
!> @date February, 2016
!> - do not closed sea for east-west cyclic domain
!> @date October, 2016
!> - dimension to be used select from configuration file
!
!> @todo
!> - check tl_multi is not empty
!>
!> @note Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
!----------------------------------------------------------------------
PROGRAM create_bathy

   USE global                          ! global variable
   USE kind                            ! F90 kind parameter
   USE logger                          ! log file manager
   USE fct                             ! basic useful function
   USE date                            ! date manager
   USE att                             ! attribute manager
   USE dim                             ! dimension manager
   USE var                             ! variable manager
   USE file                            ! file manager
   USE multi                           ! multi file manager
   USE iom                             ! I/O manager
   USE grid                            ! grid manager
   USE extrap                          ! extrapolation manager
   USE interp                          ! interpolation manager
   USE filter                          ! filter manager
   USE mpp                             ! MPP manager
   USE dom                             ! domain manager
   USE iom_mpp                         ! MPP I/O manager
   USE iom_dom                         ! DOM I/O manager

   IMPLICIT NONE

   ! local variable
   CHARACTER(LEN=lc)                                  :: cl_namelist
   CHARACTER(LEN=lc)                                  :: cl_date
   CHARACTER(LEN=lc)                                  :: cl_data

   INTEGER(i4)                                        :: il_narg
   INTEGER(i4)                                        :: il_status
   INTEGER(i4)                                        :: il_fileid
   INTEGER(i4)                                        :: il_attid
   INTEGER(i4)                                        :: il_imin0
   INTEGER(i4)                                        :: il_imax0
   INTEGER(i4)                                        :: il_jmin0
   INTEGER(i4)                                        :: il_jmax0
   INTEGER(i4)      , DIMENSION(ip_maxdim)            :: il_rho
   INTEGER(i4)      , DIMENSION(2,2)                  :: il_offset
   INTEGER(i4)      , DIMENSION(2,2)                  :: il_ind
   INTEGER(i4)      , DIMENSION(:,:)    , ALLOCATABLE :: il_mask

   LOGICAL                                            :: ll_exist
   LOGICAL                                            :: ll_fillclosed

   TYPE(TMPP)                                         :: tl_coord0
   TYPE(TMPP)                                         :: tl_coord1
   TYPE(TMPP)                                         :: tl_mpp
   TYPE(TFILE)                                        :: tl_fileout

   TYPE(TATT)                                         :: tl_att
   
   TYPE(TVAR)                                         :: tl_lon
   TYPE(TVAR)                                         :: tl_lat
   TYPE(TVAR)                                         :: tl_depth
   TYPE(TVAR)                                         :: tl_time

   TYPE(TVAR)                                         :: tl_tmp
   TYPE(TVAR)       , DIMENSION(:), ALLOCATABLE       :: tl_var
   
   TYPE(TDIM)       , DIMENSION(ip_maxdim)            :: tl_dim

   TYPE(TMULTI)                                       :: tl_multi

   REAL(dp)                                           :: dl_minbat

   ! loop indices
   INTEGER(i4) :: ji
   INTEGER(i4) :: jj
   INTEGER(i4) :: jk

   ! namelist variable
   ! namlog
   CHARACTER(LEN=lc)                       :: cn_logfile = 'create_bathy.log' 
   CHARACTER(LEN=lc)                       :: cn_verbosity = 'warning' 
   INTEGER(i4)                             :: in_maxerror = 5

   ! namcfg
   CHARACTER(LEN=lc)                       :: cn_varcfg = './cfg/variable.cfg' 
   CHARACTER(LEN=lc)                       :: cn_dimcfg = './cfg/dimension.cfg' 
   CHARACTER(LEN=lc)                       :: cn_dumcfg = './cfg/dummy.cfg' 

   ! namcrs
   CHARACTER(LEN=lc)                       :: cn_coord0 = '' 
   INTEGER(i4)                             :: in_perio0 = -1

   ! namfin
   CHARACTER(LEN=lc)                       :: cn_coord1 = ''
   INTEGER(i4)                             :: in_perio1 = -1
   LOGICAL                                 :: ln_fillclosed = .TRUE.

   ! namvar
   CHARACTER(LEN=lc), DIMENSION(ip_maxvar) :: cn_varfile = ''
   CHARACTER(LEN=lc), DIMENSION(ip_maxvar) :: cn_varinfo = ''

   ! namnst
   INTEGER(i4)                             :: in_rhoi  = 1
   INTEGER(i4)                             :: in_rhoj  = 1

   ! namout
   CHARACTER(LEN=lc)                       :: cn_fileout = 'bathy_fine.nc' 
   !-------------------------------------------------------------------

   NAMELIST /namlog/ &   !< logger namelist
   &  cn_logfile,    &   !< log file
   &  cn_verbosity,  &   !< log verbosity
   &  in_maxerror        !< logger maximum error

   NAMELIST /namcfg/ &   !< configuration namelist
   &  cn_varcfg, &       !< variable configuration file
   &  cn_dimcfg, &       !< dimension configuration file
   &  cn_dumcfg          !< dummy configuration file

   NAMELIST /namcrs/ &   !< coarse grid namelist
   &  cn_coord0,  &      !< coordinate file
   &  in_perio0          !< periodicity index
   
   NAMELIST /namfin/ &   !< fine grid namelist
   &  cn_coord1,   &     !< coordinate file
   &  in_perio1,   &     !< periodicity index
   &  ln_fillclosed      !< fill closed sea
 
   NAMELIST /namvar/ &   !< variable namelist
   &  cn_varfile, &      !< list of variable file
   &  cn_varinfo         !< list of variable and interpolation method to be used. (ex: 'votemper:linear','vosaline:cubic' )
   
   NAMELIST /namnst/ &   !< nesting namelist
   &  in_rhoi,    &      !< refinement factor in i-direction
   &  in_rhoj            !< refinement factor in j-direction

   NAMELIST /namout/ &   !< output namlist
   &  cn_fileout         !< fine grid bathymetry file
   !-------------------------------------------------------------------

   ! namelist
   ! get namelist
   il_narg=COMMAND_ARGUMENT_COUNT() !f03 intrinsec
   IF( il_narg/=1 )THEN
      PRINT *,"ERROR in create_bathy: need a namelist"
      STOP
   ELSE
      CALL GET_COMMAND_ARGUMENT(1,cl_namelist) !f03 intrinsec
   ENDIF
 
   ! read namelist
   INQUIRE(FILE=TRIM(cl_namelist), EXIST=ll_exist)
   IF( ll_exist )THEN
 
      il_fileid=fct_getunit()

      OPEN( il_fileid, FILE=TRIM(cl_namelist), &
      &                FORM='FORMATTED',       &
      &                ACCESS='SEQUENTIAL',    &
      &                STATUS='OLD',           &
      &                ACTION='READ',          &
      &                IOSTAT=il_status)
      CALL fct_err(il_status)
      IF( il_status /= 0 )THEN
         PRINT *,"ERROR in create_bathy: error opening "//TRIM(cl_namelist)
         STOP
      ENDIF

      READ( il_fileid, NML = namlog )
      ! define log file
      CALL logger_open(TRIM(cn_logfile),TRIM(cn_verbosity),in_maxerror)
      CALL logger_header()

      READ( il_fileid, NML = namcfg )
      ! get variable extra information
      CALL var_def_extra(TRIM(cn_varcfg))

      ! get dimension allowed
      CALL dim_def_extra(TRIM(cn_dimcfg))

      ! get dummy variable
      CALL var_get_dummy(TRIM(cn_dumcfg))
      ! get dummy dimension
      CALL dim_get_dummy(TRIM(cn_dumcfg))
      ! get dummy attribute
      CALL att_get_dummy(TRIM(cn_dumcfg))

      READ( il_fileid, NML = namcrs )
      READ( il_fileid, NML = namfin )
      READ( il_fileid, NML = namvar )
      ! add user change in extra information
      CALL var_chg_extra( cn_varinfo )
      ! match variable with file
      tl_multi=multi_init(cn_varfile)
 
      READ( il_fileid, NML = namnst )
      READ( il_fileid, NML = namout )

      CLOSE( il_fileid, IOSTAT=il_status )
      CALL fct_err(il_status)
      IF( il_status /= 0 )THEN
         CALL logger_error("CREATE BATHY: closing "//TRIM(cl_namelist))
      ENDIF

   ELSE

      PRINT *,"ERROR in create_bathy: can't find "//TRIM(cl_namelist)
      STOP

   ENDIF

   CALL multi_print(tl_multi)

   ! open files
   IF( cn_coord0 /= '' )THEN
      tl_coord0=mpp_init( file_init(TRIM(cn_coord0)), id_perio=in_perio0)
      CALL grid_get_info(tl_coord0)
   ELSE
      CALL logger_fatal("CREATE BATHY: no coarse grid coordinate found. "//&
      &     "check namelist")      
   ENDIF

   IF( TRIM(cn_coord1) /= '' )THEN
      tl_coord1=mpp_init( file_init(TRIM(cn_coord1)),id_perio=in_perio1)
      CALL grid_get_info(tl_coord1)
   ELSE
      CALL logger_fatal("CREATE BATHY: no fine grid coordinate found. "//&
      &     "check namelist")
   ENDIF

   ! do not closed sea for east-west cyclic domain
   ll_fillclosed=ln_fillclosed
   IF( tl_coord1%i_perio == 1 ) ll_fillclosed=.FALSE.

   ! check
   ! check output file do not already exist
   INQUIRE(FILE=TRIM(cn_fileout), EXIST=ll_exist)
   IF( ll_exist )THEN
      CALL logger_fatal("CREATE BATHY: output file "//TRIM(cn_fileout)//&
      &  " already exist.")
   ENDIF

   ! check namelist
   ! check refinement factor
   il_rho(:)=1
   IF( in_rhoi < 1 .OR. in_rhoj < 1 )THEN
      CALL logger_error("CREATE BATHY: invalid refinement factor."//&
      &  " check namelist "//TRIM(cl_namelist))
   ELSE
      il_rho(jp_I)=in_rhoi
      il_rho(jp_J)=in_rhoj
   ENDIF

   ! check domain indices
   ! compute coarse grid indices around fine grid
   il_ind(:,:)=grid_get_coarse_index( tl_coord0, tl_coord1, &
   &                                  id_rho=il_rho(:) )

   il_imin0=il_ind(jp_I,1) ; il_imax0=il_ind(jp_I,2)
   il_jmin0=il_ind(jp_J,1) ; il_jmax0=il_ind(jp_J,2)

   ! check domain validity
   CALL grid_check_dom(tl_coord0, il_imin0, il_imax0, il_jmin0, il_jmax0)

   ! check coincidence between coarse and fine grid
   CALL grid_check_coincidence( tl_coord0, tl_coord1, &
   &                            il_imin0, il_imax0, &
   &                            il_jmin0, il_jmax0, &
   &                            il_rho(:) )

   IF( .NOT. ASSOCIATED(tl_multi%t_mpp) )THEN
      CALL logger_error("CREATE BATHY: no mpp file to work on. "//&
      &                 "check cn_varfile in namelist.")
   ELSE

      ALLOCATE( tl_var( tl_multi%i_nvar ) )
      jk=0
      DO ji=1,tl_multi%i_nmpp
      
         WRITE(cl_data,'(a,i2.2)') 'data-',jk+1
         IF( .NOT. ASSOCIATED(tl_multi%t_mpp(ji)%t_proc(1)%t_var) )THEN

            CALL logger_fatal("CREATE BATHY: no variable to work on for "//&
            &                 "mpp file"//TRIM(tl_multi%t_mpp(ji)%c_name)//&
            &                 ". check cn_varfile in namelist.")

         ELSEIF( TRIM(tl_multi%t_mpp(ji)%c_name) == TRIM(cl_data) )THEN

            !- use input matrix to initialise variable
            DO jj=1,tl_multi%t_mpp(ji)%t_proc(1)%i_nvar
               jk=jk+1
               tl_tmp=var_copy(tl_multi%t_mpp(ji)%t_proc(1)%t_var(jj))
            
               tl_var(jk)=create_bathy_matrix(tl_tmp, tl_coord1)
            ENDDO
            ! clean
            CALL var_clean(tl_tmp)

         ELSE

            tl_mpp=mpp_init( file_init(TRIM(tl_multi%t_mpp(ji)%c_name)) )
            CALL grid_get_info(tl_mpp)

            ! open mpp file
            CALL iom_mpp_open(tl_mpp)

            ! get or check depth value
            CALL create_bathy_check_depth( tl_mpp, tl_depth )

            ! get or check time value
            CALL create_bathy_check_time( tl_mpp, tl_time )

            ! close mpp file
            CALL iom_mpp_close(tl_mpp)

            IF( ANY(tl_mpp%t_dim(1:2)%i_len /= tl_coord0%t_dim(1:2)%i_len).OR.&
            &   ALL(il_rho(:)==1) )THEN
               !- extract bathymetry from fine grid bathymetry 
               DO jj=1,tl_multi%t_mpp(ji)%t_proc(1)%i_nvar
                  jk=jk+1
                  tl_tmp=var_copy(tl_multi%t_mpp(ji)%t_proc(1)%t_var(jj))
               
                  tl_var(jk)=create_bathy_extract( tl_tmp, tl_mpp, &
                  &                                tl_coord1 )
               ENDDO
               ! clean
               CALL var_clean(tl_tmp)
            ELSE
               !- get bathymetry from coarse grid bathymetry 
               DO jj=1,tl_multi%t_mpp(ji)%t_proc(1)%i_nvar
                  jk=jk+1
                  tl_tmp=var_copy(tl_multi%t_mpp(ji)%t_proc(1)%t_var(jj))

                  il_offset(:,:)= grid_get_fine_offset(tl_coord0,    &
                  &                                    il_imin0, il_jmin0, &
                  &                                    il_imax0, il_jmax0, &
                  &                                    tl_coord1,          &
                  &                                    il_rho(:),          &
                  &                                    TRIM(tl_tmp%c_point) )

                  tl_var(jk)=create_bathy_get_var( tl_tmp, tl_mpp,     &
                  &                                il_imin0, il_jmin0, &
                  &                                il_imax0, il_jmax0, &
                  &                                il_offset(:,:),  &
                  &                                il_rho(:) )
               ENDDO
               ! clean
               CALL var_clean(tl_tmp)
            ENDIF

            ! clean structure
            CALL mpp_clean(tl_mpp)

         ENDIF
      ENDDO
   ENDIF

   ! use additional request
   DO jk=1,tl_multi%i_nvar

         ! change unit and apply factor
         CALL var_chg_unit(tl_var(jk))

         ! forced min and max value
         CALL var_limit_value(tl_var(jk))

         ! fill closed sea
         IF( ll_fillclosed )THEN
            ALLOCATE( il_mask(tl_var(jk)%t_dim(1)%i_len, &
            &                 tl_var(jk)%t_dim(2)%i_len) )

            ! split domain in N sea subdomain
            il_mask(:,:)=grid_split_domain(tl_var(jk))

            !  fill smallest domain
            CALL grid_fill_small_dom( tl_var(jk), il_mask(:,:) )

            DEALLOCATE( il_mask )
         ENDIF

         ! filter
         CALL filter_fill_value(tl_var(jk))

         ! check bathymetry
         dl_minbat=MINVAL(tl_var(jk)%d_value(:,:,:,:))
         IF( TRIM(tl_var(jk)%c_stdname) == 'bathymetry' .AND. &
         &   dl_minbat <= 0._dp  )THEN
            CALL logger_debug("CREATE BATHY: min value "//TRIM(fct_str(dl_minbat)))
            CALL logger_fatal("CREATE BATHY: Bathymetry has value <= 0")
         ENDIF

   ENDDO

   ! create file
   tl_fileout=file_init(TRIM(cn_fileout))

   ! add dimension
   tl_dim(:)=var_max_dim(tl_var(:))

   DO ji=1,ip_maxdim
      IF( tl_dim(ji)%l_use ) CALL file_add_dim(tl_fileout, tl_dim(ji))
   ENDDO

   ! add variables
   IF( ALL( tl_dim(1:2)%l_use ) )THEN

      ! open mpp files
      CALL iom_mpp_open(tl_coord1)

      ! add longitude
      tl_lon=iom_mpp_read_var(tl_coord1,'longitude')
      CALL file_add_var(tl_fileout, tl_lon)
      CALL var_clean(tl_lon)

      ! add latitude
      tl_lat=iom_mpp_read_var(tl_coord1,'latitude')
      CALL file_add_var(tl_fileout, tl_lat)
      CALL var_clean(tl_lat)

      ! close mpp files
      CALL iom_mpp_close(tl_coord1)

   ENDIF

   IF( tl_dim(3)%l_use )THEN
      ! add depth
      CALL file_add_var(tl_fileout, tl_depth)
      CALL var_clean(tl_depth)
   ENDIF

   IF( tl_dim(4)%l_use )THEN
      ! add time
      CALL file_add_var(tl_fileout, tl_time)
      CALL var_clean(tl_time)
   ENDIF

   ! add other variables
   DO jk=tl_multi%i_nvar,1,-1
      CALL file_add_var(tl_fileout, tl_var(jk))
      CALL var_clean(tl_var(jk))
   ENDDO
   DEALLOCATE(tl_var)

   ! add some attribute
   tl_att=att_init("Created_by","SIREN create_bathy")
   CALL file_add_att(tl_fileout, tl_att)

   cl_date=date_print(date_now())
   tl_att=att_init("Creation_date",cl_date)
   CALL file_add_att(tl_fileout, tl_att)

   ! add attribute periodicity
   il_attid=0
   IF( ASSOCIATED(tl_fileout%t_att) )THEN
      il_attid=att_get_index(tl_fileout%t_att(:),'periodicity')
   ENDIF
   IF( tl_coord1%i_perio >= 0 .AND. il_attid == 0 )THEN
      tl_att=att_init('periodicity',tl_coord1%i_perio)
      CALL file_add_att(tl_fileout,tl_att)
   ENDIF

   ! add attribute east west overlap
   il_attid=0
   IF( ASSOCIATED(tl_fileout%t_att) )THEN
      il_attid=att_get_index(tl_fileout%t_att(:),'ew_overlap')
   ENDIF
   IF( tl_coord1%i_ew >= 0 .AND. il_attid == 0 )THEN
      tl_att=att_init('ew_overlap',tl_coord1%i_ew)
      CALL file_add_att(tl_fileout,tl_att)
   ENDIF
   
   ! create file
   CALL iom_create(tl_fileout)

   ! write file
   CALL iom_write_file(tl_fileout)

   ! close file
   CALL iom_close(tl_fileout)

   ! clean
   CALL att_clean(tl_att)

   CALL file_clean(tl_fileout)
   CALL mpp_clean(tl_coord1)
   CALL mpp_clean(tl_coord0)
   CALL var_clean_extra()

   ! close log file
   CALL logger_footer()
   CALL logger_close()

CONTAINS
   !-------------------------------------------------------------------
   !> @brief
   !> This function create variable, filled with matrix value
   !> 
   !> @details 
   !> A variable is create with the same name that the input variable, 
   !> and with dimension of the coordinate file.<br/> 
   !> Then the variable array of value is split into equal subdomain.
   !> Each subdomain is filled with the corresponding value of the matrix.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !>
   !> @param[in] td_var    variable structure 
   !> @param[in] td_coord  coordinate file structure
   !> @return variable structure
   !-------------------------------------------------------------------
   FUNCTION create_bathy_matrix(td_var, td_coord)
      IMPLICIT NONE
      ! Argument
      TYPE(TVAR), INTENT(IN) :: td_var
      TYPE(TMPP), INTENT(IN) :: td_coord

      ! function
      TYPE(TVAR) :: create_bathy_matrix

      ! local variable
      INTEGER(i4)      , DIMENSION(2,2)                  :: il_xghost
      INTEGER(i4)      , DIMENSION(3)                    :: il_dim
      INTEGER(i4)      , DIMENSION(3)                    :: il_size
      INTEGER(i4)      , DIMENSION(3)                    :: il_rest

      INTEGER(i4)      , DIMENSION(:)      , ALLOCATABLE :: il_ishape
      INTEGER(i4)      , DIMENSION(:)      , ALLOCATABLE :: il_jshape
      INTEGER(i4)      , DIMENSION(:)      , ALLOCATABLE :: il_kshape

      REAL(dp)         , DIMENSION(:,:,:,:), ALLOCATABLE :: dl_value

      TYPE(TVAR)                                         :: tl_lon
      TYPE(TDIM)       , DIMENSION(ip_maxdim)            :: tl_dim

      TYPE(TMPP)                                         :: tl_coord

      ! loop indices
      INTEGER(i4) :: ji
      INTEGER(i4) :: jj
      INTEGER(i4) :: jk
      !----------------------------------------------------------------

      ! copy structure
      tl_coord=mpp_copy(td_coord)

      ! use only edge processor
      CALL mpp_get_contour(tl_coord)

      ! open useful processor
      CALL iom_mpp_open(tl_coord)

      ! read output grid
      tl_lon=iom_mpp_read_var(tl_coord,'longitude')

      ! look for ghost cell
      il_xghost(:,:)=grid_get_ghost( tl_coord )

      ! close processor
      CALL iom_mpp_close(tl_coord)
      ! clean
      CALL mpp_clean(tl_coord)

      ! remove ghost cell
      CALL grid_del_ghost(tl_lon, il_xghost(:,:))

      ! write value on grid
      ! get matrix dimension
      il_dim(:)=td_var%t_dim(1:3)%i_len
      ! output dimension
      tl_dim(:)=dim_copy(tl_lon%t_dim(:))
      ! clean
      CALL var_clean(tl_lon)

      ! split output domain in N subdomain depending of matrix dimension 
      il_size(:) = tl_dim(1:3)%i_len / il_dim(:)
      il_rest(:) = MOD(tl_dim(1:3)%i_len, il_dim(:))

      ALLOCATE( il_ishape(il_dim(1)+1) )
      il_ishape(:)=0
      DO ji=2,il_dim(1)+1
         il_ishape(ji)=il_ishape(ji-1)+il_size(1)
      ENDDO
      ! add rest to last cell
      il_ishape(il_dim(1)+1)=il_ishape(il_dim(1)+1)+il_rest(1)

      ALLOCATE( il_jshape(il_dim(2)+1) )
      il_jshape(:)=0
      DO jj=2,il_dim(2)+1
         il_jshape(jj)=il_jshape(jj-1)+il_size(2)
      ENDDO
      ! add rest to last cell
      il_jshape(il_dim(2)+1)=il_jshape(il_dim(2)+1)+il_rest(2)

      ALLOCATE( il_kshape(il_dim(3)+1) )
      il_kshape(:)=0
      DO jk=2,il_dim(3)+1
         il_kshape(jk)=il_kshape(jk-1)+il_size(3)
      ENDDO
      ! add rest to last cell
      il_kshape(il_dim(3)+1)=il_kshape(il_dim(3)+1)+il_rest(3)

      ! write ouput array of value 
      ALLOCATE(dl_value( tl_dim(1)%i_len, &
      &                  tl_dim(2)%i_len, &
      &                  tl_dim(3)%i_len, &
      &                  tl_dim(4)%i_len) )

      dl_value(:,:,:,:)=0

      DO jk=2,il_dim(3)+1
         DO jj=2,il_dim(2)+1
            DO ji=2,il_dim(1)+1
               
               dl_value( 1+il_ishape(ji-1):il_ishape(ji), &
               &         1+il_jshape(jj-1):il_jshape(jj), &
               &         1+il_kshape(jk-1):il_kshape(jk), &
               &         1 ) = td_var%d_value(ji-1,jj-1,jk-1,1)

            ENDDO
         ENDDO
      ENDDO

      ! initialise variable with value
      create_bathy_matrix=var_init(TRIM(td_var%c_name),dl_value(:,:,:,:))

      DEALLOCATE(dl_value)

      ! add ghost cell
      CALL grid_add_ghost(create_bathy_matrix, il_xghost(:,:))

      ! clean
      CALL dim_clean(tl_dim(:))

   END FUNCTION create_bathy_matrix
   !-------------------------------------------------------------------
   !> @brief
   !> This function extract variable from file over coordinate domain and
   !> return variable structure
   !> 
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !>
   !> @param[in] td_var    variable structure 
   !> @param[in] td_mpp    mpp file structure
   !> @param[in] td_coord  coordinate file structure
   !> @return variable structure
   !-------------------------------------------------------------------
   FUNCTION create_bathy_extract(td_var, td_mpp, &
   &                             td_coord)
      IMPLICIT NONE
      ! Argument
      TYPE(TVAR), INTENT(IN) :: td_var  
      TYPE(TMPP), INTENT(IN) :: td_mpp
      TYPE(TMPP), INTENT(IN) :: td_coord

      ! function
      TYPE(TVAR) :: create_bathy_extract

      ! local variable
      INTEGER(i4), DIMENSION(2,2) :: il_ind

      INTEGER(i4) :: il_imin
      INTEGER(i4) :: il_jmin
      INTEGER(i4) :: il_imax
      INTEGER(i4) :: il_jmax

      TYPE(TMPP)  :: tl_mpp

      TYPE(TATT)  :: tl_att

      TYPE(TVAR)  :: tl_var
      
      TYPE(TDOM)  :: tl_dom
      ! loop indices
      !----------------------------------------------------------------

      IF( .NOT. ASSOCIATED(td_mpp%t_proc) )THEN
         CALL logger_error("CREATE BATHY EXTRACT: no processor associated "//&
         &  "to mpp "//TRIM(td_mpp%c_name))
      ELSE

         !init
         tl_mpp=mpp_copy(td_mpp)

         ! compute file grid indices around coord grid
         il_ind(:,:)=grid_get_coarse_index(tl_mpp, td_coord )

         il_imin=il_ind(1,1) ; il_imax=il_ind(1,2)
         il_jmin=il_ind(2,1) ; il_jmax=il_ind(2,2)

         ! check grid coincidence
         CALL grid_check_coincidence( tl_mpp, td_coord, &
         &                            il_imin, il_imax, &
         &                            il_jmin, il_jmax, &
         &                            (/1, 1, 1/) )

         ! compute domain
         tl_dom=dom_init(tl_mpp,           &
         &               il_imin, il_imax, &
         &               il_jmin, il_jmax)

         ! open mpp files over domain
         CALL iom_dom_open(tl_mpp, tl_dom)

         ! read variable on domain
         tl_var=iom_dom_read_var(tl_mpp,TRIM(td_var%c_name),tl_dom)

         ! close mpp file
         CALL iom_dom_close(tl_mpp)

         ! add ghost cell
         CALL grid_add_ghost(tl_var,tl_dom%i_ghost(:,:))

         ! check result
         IF( ANY( tl_var%t_dim(:)%l_use .AND. &
         &        tl_var%t_dim(:)%i_len /= td_coord%t_dim(:)%i_len) )THEN
            CALL logger_debug("CREATE BATHY EXTRACT: "//&
            &        "dimensoin of variable "//TRIM(td_var%c_name)//" "//&
            &        TRIM(fct_str(tl_var%t_dim(1)%i_len))//","//&
            &        TRIM(fct_str(tl_var%t_dim(2)%i_len))//","//&
            &        TRIM(fct_str(tl_var%t_dim(3)%i_len))//","//&
            &        TRIM(fct_str(tl_var%t_dim(4)%i_len)) )
            CALL logger_debug("CREATE BATHY EXTRACT: "//&
            &        "dimensoin of coordinate file "//&
            &        TRIM(fct_str(td_coord%t_dim(1)%i_len))//","//&
            &        TRIM(fct_str(td_coord%t_dim(2)%i_len))//","//&
            &        TRIM(fct_str(td_coord%t_dim(3)%i_len))//","//&
            &        TRIM(fct_str(td_coord%t_dim(4)%i_len)) )
            CALL logger_fatal("CREATE BATHY EXTRACT: "//&
            &  "dimensoin of extracted "//&
            &  "variable and coordinate file dimension differ")
         ENDIF

         ! add attribute to variable
         tl_att=att_init('src_file',TRIM(fct_basename(tl_mpp%c_name)))
         CALL var_move_att(tl_var, tl_att)         

         tl_att=att_init('src_i_indices',(/tl_dom%i_imin, tl_dom%i_imax/))
         CALL var_move_att(tl_var, tl_att)

         tl_att=att_init('src_j_indices',(/tl_dom%i_jmin, tl_dom%i_jmax/))
         CALL var_move_att(tl_var, tl_att)

         ! save result
         create_bathy_extract=var_copy(tl_var)

         ! clean structure
         CALL att_clean(tl_att)
         CALL var_clean(tl_var)
         CALL mpp_clean(tl_mpp)
      ENDIF

   END FUNCTION create_bathy_extract
   !-------------------------------------------------------------------
   !> @brief
   !> This function get coarse grid variable, interpolate variable, and return
   !> variable structure over fine grid
   !> 
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !>
   !> @param[in] td_var    variable structure
   !> @param[in] td_mpp    mpp file structure
   !> @param[in] id_imin   i-direction lower left  corner indice 
   !> @param[in] id_imax   i-direction upper right corner indice 
   !> @param[in] id_jmin   j-direction lower left  corner indice
   !> @param[in] id_jmax   j-direction upper right corner indice 
   !> @param[in] id_offset offset between fine grid and coarse grid
   !> @param[in] id_rho    array of refinement factor
   !> @return variable structure
   !-------------------------------------------------------------------
   FUNCTION create_bathy_get_var(td_var, td_mpp,   &
            &                    id_imin, id_jmin, &
            &                    id_imax, id_jmax, &
            &                    id_offset,        &
            &                    id_rho )
      IMPLICIT NONE
      ! Argument
      TYPE(TVAR)                 , INTENT(IN) :: td_var  
      TYPE(TMPP)                 , INTENT(IN) :: td_mpp 
      INTEGER(i4)                , INTENT(IN) :: id_imin
      INTEGER(i4)                , INTENT(IN) :: id_imax
      INTEGER(i4)                , INTENT(IN) :: id_jmin
      INTEGER(i4)                , INTENT(IN) :: id_jmax
      INTEGER(i4), DIMENSION(:,:), INTENT(IN) :: id_offset
      INTEGER(i4), DIMENSION(:)  , INTENT(IN) :: id_rho

      ! function
      TYPE(TVAR) :: create_bathy_get_var

      ! local variable
      TYPE(TMPP)  :: tl_mpp
      TYPE(TATT)  :: tl_att
      TYPE(TVAR)  :: tl_var
      TYPE(TDOM)  :: tl_dom

      INTEGER(i4) :: il_size
      INTEGER(i4), DIMENSION(:), ALLOCATABLE :: il_rho

      ! loop indices
      !----------------------------------------------------------------
      IF( ANY(SHAPE(id_offset(:,:)) /= 2) )THEN
         CALL logger_error("CREATE BATHY GET VAR: invalid dimension of "//&
         &                 "offset array")
      ENDIF

      ! copy structure
      tl_mpp=mpp_copy(td_mpp)

      !- compute domain
      tl_dom=dom_init(tl_mpp,           &
      &               id_imin, id_imax, &
      &               id_jmin, id_jmax)

      !- add extra band (if possible) to compute interpolation
      CALL dom_add_extra(tl_dom)

      !- open mpp files over domain
      CALL iom_dom_open(tl_mpp, tl_dom)

      !- read variable value on domain
      tl_var=iom_dom_read_var(tl_mpp,TRIM(td_var%c_name),tl_dom)

      !- close mpp files
      CALL iom_dom_close(tl_mpp)

      il_size=SIZE(id_rho(:))
      ALLOCATE( il_rho(il_size) )
      il_rho(:)=id_rho(:)
      
      !- interpolate variable
      CALL create_bathy_interp(tl_var, il_rho(:), id_offset(:,:))

      !- remove extraband added to domain
      CALL dom_del_extra( tl_var, tl_dom, il_rho(:) )

      CALL dom_clean_extra( tl_dom )

      !- add ghost cell
      CALL grid_add_ghost(tl_var,tl_dom%i_ghost(:,:))
 
      !- add attribute to variable
      tl_att=att_init('src_file',TRIM(fct_basename(tl_mpp%c_name)))
      CALL var_move_att(tl_var, tl_att)

      tl_att=att_init('src_i_indices',(/tl_dom%i_imin, tl_dom%i_imax/))
      CALL var_move_att(tl_var, tl_att)

      tl_att=att_init('src_j_indices',(/tl_dom%i_jmin, tl_dom%i_jmax/))
      CALL var_move_att(tl_var, tl_att)

      IF( .NOT. ALL(id_rho(:)==1) )THEN
         tl_att=att_init("refinment_factor",(/id_rho(jp_I),id_rho(jp_J)/))
         CALL var_move_att(tl_var, tl_att)
      ENDIF

      DEALLOCATE( il_rho )

      !- save result
      create_bathy_get_var=var_copy(tl_var)

      !- clean structure
      CALL att_clean(tl_att)
      CALL var_clean(tl_var)
      CALL mpp_clean(tl_mpp)
 
   END FUNCTION create_bathy_get_var
   !-------------------------------------------------------------------
   !> @brief
   !> This subroutine interpolate variable
   !> 
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !>
   !> @param[inout] td_var variable structure 
   !> @param[in] id_rho    array of refinment factor
   !> @param[in] id_offset array of offset between fine and coarse grid
   !> @param[in] id_iext   i-direction size of extra bands (default=im_minext)
   !> @param[in] id_jext   j-direction size of extra bands (default=im_minext)
   !-------------------------------------------------------------------
   SUBROUTINE create_bathy_interp( td_var,         &
   &                               id_rho,          &
   &                               id_offset,       &
   &                               id_iext, id_jext)

      IMPLICIT NONE

      ! Argument
      TYPE(TVAR) ,                 INTENT(INOUT) :: td_var
      INTEGER(i4), DIMENSION(:)  , INTENT(IN   ) :: id_rho
      INTEGER(i4), DIMENSION(:,:), INTENT(IN   ) :: id_offset
      INTEGER(i4),                 INTENT(IN   ), OPTIONAL :: id_iext
      INTEGER(i4),                 INTENT(IN   ), OPTIONAL :: id_jext

      ! local variable
      TYPE(TVAR)  :: tl_mask

      INTEGER(i1), DIMENSION(:,:,:,:), ALLOCATABLE :: bl_mask

      INTEGER(i4) :: il_iext
      INTEGER(i4) :: il_jext

      ! loop indices
      !----------------------------------------------------------------

      !WARNING: two extrabands are required for cubic interpolation
      il_iext=3
      IF( PRESENT(id_iext) ) il_iext=id_iext

      il_jext=3
      IF( PRESENT(id_jext) ) il_jext=id_jext

      IF( il_iext < 2 .AND. td_var%c_interp(1) == 'cubic' )THEN
         CALL logger_warn("CREATE BATHY INTERP: at least extrapolation "//&
         &  "on two points are required with cubic interpolation ")
         il_iext=2
      ENDIF

      IF( il_jext < 2 .AND. td_var%c_interp(1) == 'cubic' )THEN
         CALL logger_warn("CREATE BATHY INTERP: at least extrapolation "//&
         &  "on two points are required with cubic interpolation ")
         il_jext=2
      ENDIF

      ! work on mask
      ! create mask
      ALLOCATE(bl_mask(td_var%t_dim(1)%i_len, &
      &                td_var%t_dim(2)%i_len, &
      &                td_var%t_dim(3)%i_len, &
      &                td_var%t_dim(4)%i_len) )

      bl_mask(:,:,:,:)=1
      WHERE(td_var%d_value(:,:,:,:)==td_var%d_fill) bl_mask(:,:,:,:)=0      

      SELECT CASE(TRIM(td_var%c_point))
      CASE DEFAULT ! 'T'
         tl_mask=var_init('tmask', bl_mask(:,:,:,:), td_dim=td_var%t_dim(:), &
         &                id_ew=td_var%i_ew )
      CASE('U','V','F')
         CALL logger_fatal("CREATE BATHY INTERP: can not computed "//&
         &                 "interpolation on "//TRIM(td_var%c_point)//&
         &                 " grid point (variable "//TRIM(td_var%c_name)//&
         &                 "). check namelist.")
      END SELECT

      DEALLOCATE(bl_mask)

      ! interpolate mask
      CALL interp_fill_value( tl_mask, id_rho(:), &
      &                       id_offset=id_offset(:,:) )

      ! work on variable
      ! add extraband
      CALL extrap_add_extrabands(td_var, il_iext, il_jext)

      ! extrapolate variable
      CALL extrap_fill_value( td_var )

      ! interpolate Bathymetry
      CALL interp_fill_value( td_var, id_rho(:), &
      &                       id_offset=id_offset(:,:) )

      ! remove extraband
      CALL extrap_del_extrabands(td_var, il_iext*id_rho(jp_I), il_jext*id_rho(jp_J))

      ! keep original mask 
      WHERE( tl_mask%d_value(:,:,:,:) == 0 )
         td_var%d_value(:,:,:,:)=td_var%d_fill
      END WHERE

      ! clean variable structure
      CALL var_clean(tl_mask)

   END SUBROUTINE create_bathy_interp
   !-------------------------------------------------------------------
   !> @brief
   !> This subroutine get depth variable value in an open mpp structure
   !> and check if agree with already input depth variable.
   !> 
   !> @details 
   !>
   !> @author J.Paul
   !> @date January, 2016 - Initial Version
   !>
   !> @param[in] td_mpp       mpp structure
   !> @param[inout] td_depth  depth variable structure 
   !-------------------------------------------------------------------
   SUBROUTINE create_bathy_check_depth( td_mpp, td_depth )

      IMPLICIT NONE

      ! Argument
      TYPE(TMPP) , INTENT(IN   ) :: td_mpp
      TYPE(TVAR) , INTENT(INOUT) :: td_depth

      ! local variable
      INTEGER(i4) :: il_varid
      TYPE(TVAR)  :: tl_depth
      ! loop indices
      !----------------------------------------------------------------

      ! get or check depth value
      IF( td_mpp%t_proc(1)%i_depthid /= 0 )THEN

         il_varid=td_mpp%t_proc(1)%i_depthid
         IF( ASSOCIATED(td_depth%d_value) )THEN

            tl_depth=iom_mpp_read_var(td_mpp, il_varid)

            IF( ANY( td_depth%d_value(:,:,:,:) /= &
            &        tl_depth%d_value(:,:,:,:) ) )THEN

               CALL logger_warn("CREATE BATHY: depth value from "//&
               &  TRIM(td_mpp%c_name)//" not conform "//&
               &  " to those from former file(s).")

            ENDIF
            CALL var_clean(tl_depth)

         ELSE
            td_depth=iom_mpp_read_var(td_mpp,il_varid)
         ENDIF

      ENDIF
      
   END SUBROUTINE create_bathy_check_depth
   !-------------------------------------------------------------------
   !> @brief
   !> This subroutine get date and time in an open mpp structure
   !> and check if agree with date and time already read.
   !> 
   !> @details 
   !>
   !> @author J.Paul
   !> @date January, 2016 - Initial Version
   !>
   !> @param[in] td_mpp      mpp structure
   !> @param[inout] td_time  time variable structure 
   !-------------------------------------------------------------------
   SUBROUTINE create_bathy_check_time( td_mpp, td_time )

      IMPLICIT NONE

      ! Argument
      TYPE(TMPP), INTENT(IN   ) :: td_mpp
      TYPE(TVAR), INTENT(INOUT) :: td_time

      ! local variable
      INTEGER(i4) :: il_varid
      TYPE(TVAR)  :: tl_time

      TYPE(TDATE) :: tl_date1
      TYPE(TDATE) :: tl_date2
      ! loop indices
      !----------------------------------------------------------------

      ! get or check depth value
      IF( td_mpp%t_proc(1)%i_timeid /= 0 )THEN

         il_varid=td_mpp%t_proc(1)%i_timeid
         IF( ASSOCIATED(td_time%d_value) )THEN

            tl_time=iom_mpp_read_var(td_mpp, il_varid)

            tl_date1=var_to_date(td_time)
            tl_date2=var_to_date(tl_time)
            IF( tl_date1 - tl_date2 /= 0 )THEN

               CALL logger_warn("CREATE BATHY: date from "//&
               &  TRIM(td_mpp%c_name)//" not conform "//&
               &  " to those from former file(s).")

            ENDIF
            CALL var_clean(tl_time)

         ELSE
            td_time=iom_mpp_read_var(td_mpp,il_varid)
         ENDIF

      ENDIF
      
   END SUBROUTINE create_bathy_check_time
END PROGRAM create_bathy
