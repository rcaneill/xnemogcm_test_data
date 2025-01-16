!----------------------------------------------------------------------
! NEMO system team, System and Interface for oceanic RElocable Nesting
!----------------------------------------------------------------------
!
!
! PROGRAM: create_coord
!
! DESCRIPTION:
!> @file
!> @brief 
!> This program creates fine grid coordinate file.
!>
!> @details
!> @section sec1 method
!>    All variables from the input coordinates coarse grid file, are extracted
!>    and interpolated to create fine grid coordinates files.<br/>
!>    @note 
!>       interpolation method could be different for each variable.
!>
!> @section sec2 how to
!>    to create fine grid coordinates files:<br/>
!> @code{.sh}
!>    ./SIREN/bin/create_coord create_coord.nam
!> @endcode
!>    
!> @note 
!>    you could find a template of the namelist in templates directory.
!>
!>    create_coord.nam contains 6 namelists:<br/>
!>       - logger namelist (namlog)
!>       - config namelist (namcfg)
!>       - coarse grid namelist (namcrs)
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
!>       - cn_dimcfg : dimension configuration file. define dimensions allowed 
!> (see ./SIREN/cfg/dimension.cfg).
!>       - cn_dumcfg : useless (dummy) configuration file, for useless 
!> dimension or variable (see ./SIREN/cfg/dummy.cfg).
!>
!>    * _coarse grid namelist (namcrs)_:<br/>
!>       - cn_coord0 : coordinate file
!>       - in_perio0 : NEMO periodicity index (see Model Boundary Condition in
!> [NEMO documentation](http://www.nemo-ocean.eu/About-NEMO/Reference-manuals))
!>
!>    * _variable namelist (namvar)_:<br/>
!>       - cn_varinfo : list of variable and extra information about request(s)
!> to be used.<br/>
!>          each elements of *cn_varinfo* is a string character 
!>          (separated by ',').<br/>
!>          it is composed of the variable name follow by ':', 
!>          then request(s) to be used on this variable.<br/> 
!>          request could be:
!>             - int = interpolation method
!>             - ext = extrapolation method
!> 
!>                requests must be separated by ';' .<br/>
!>                order of requests does not matter.<br/>
!>
!>          informations about available method could be find in @ref interp,
!>          @ref extrap and @ref filter modules.<br/>
!>
!>          Example: 'glamt: int=linear; ext=dist_weight', 
!>          'e1t: int=cubic/rhoi'<br/>
!>          @note 
!>             If you do not specify a method which is required, 
!>             default one is applied.
!>
!>    * _nesting namelist (namnst)_:<br/>
!>       you could define sub domain with coarse grid indices or with coordinates.
!>       @note if coordinates defined, SIREN does not take into account indices.
!>
!>       - in_imin0 : i-direction lower left  point indice 
!> of coarse grid subdomain to be used
!>       - in_imax0 : i-direction upper right point indice
!> of coarse grid subdomain to be used
!>       - in_jmin0 : j-direction lower left  point indice
!> of coarse grid subdomain to be used
!>       - in_jmax0 : j-direction upper right point indice
!> of coarse grid subdomain to be used
!>       - rn_lonmin0 : lower left  longitude of coarse grid subdomain to be used
!>       - rn_lonmax0 : upper right longitude of coarse grid subdomain to be used
!>       - rn_latmin0 : lower left  latitude  of coarse grid subdomain to be used
!>       - rn_latmax0 : upper right latitude  of coarse grid subdomain to be used
!>       - in_rhoi  : refinement factor in i-direction
!>       - in_rhoj  : refinement factor in j-direction<br/>
!>
!>       \image html  grid_zoom_40.png 
!>       <center> \image latex grid_zoom_40.png 
!>       </center>
!>
!>    * _output namelist (namout)_:
!>       - cn_fileout : output coordinate file name
!>
!> @author J.Paul
! REVISION HISTORY:
!> @date November, 2013 - Initial Version
!> @date September, 2014
!> - add header for user
!> - compute offset considering grid point
!> - add global attributes in output file
!> @date September, 2015
!> - manage useless (dummy) variable, attributes, and dimension
!> @date September, 2016
!> - allow to use coordinate to define subdomain
!> @date October, 2016
!> - dimension to be used select from configuration file
!>
!> @note Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
!----------------------------------------------------------------------
PROGRAM create_coord

   USE global                          ! global variable
   USE kind                            ! F90 kind parameter
   USE logger                          ! log file manager
   USE fct                             ! basic useful function
   USE date                            ! date manager
   USE att                             ! attribute manager
   USE dim                             ! dimension manager
   USE var                             ! variable manager
   USE file                            ! file manager
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
   CHARACTER(LEN=lc)                                    :: cl_namelist
   CHARACTER(LEN=lc)                                    :: cl_date

   INTEGER(i4)                                          :: il_narg
   INTEGER(i4)                                          :: il_status
   INTEGER(i4)                                          :: il_fileid
   INTEGER(i4)                                          :: il_attid
   INTEGER(i4)                                          :: il_ind
   INTEGER(i4)                                          :: il_nvar
   INTEGER(i4)                                          :: il_ew
   INTEGER(i4)                                          :: il_imin0
   INTEGER(i4)                                          :: il_imax0
   INTEGER(i4)                                          :: il_jmin0
   INTEGER(i4)                                          :: il_jmax0

   INTEGER(i4)      , DIMENSION(ip_maxdim)              :: il_rho
   INTEGER(i4)      , DIMENSION(2)                      :: il_index
   INTEGER(i4)      , DIMENSION(2,2,ip_npoint)          :: il_offset

   LOGICAL                                              :: ll_exist

   TYPE(TATT)                                           :: tl_att

   TYPE(TDOM)                                           :: tl_dom

   TYPE(TVAR)       , DIMENSION(:)        , ALLOCATABLE :: tl_var

   TYPE(TDIM)       , DIMENSION(ip_maxdim)              :: tl_dim

   TYPE(TMPP)                                           :: tl_coord0
   TYPE(TFILE)                                          :: tl_fileout

   ! loop indices
   INTEGER(i4) :: ji
   INTEGER(i4) :: jj

   ! namelist variable
   ! namlog
   CHARACTER(LEN=lc) :: cn_logfile = 'create_coord.log' 
   CHARACTER(LEN=lc) :: cn_verbosity = 'warning' 
   INTEGER(i4)       :: in_maxerror = 5

   ! namcfg
   CHARACTER(LEN=lc) :: cn_varcfg = './cfg/variable.cfg' 
   CHARACTER(LEN=lc) :: cn_dimcfg = './cfg/dimension.cfg' 
   CHARACTER(LEN=lc) :: cn_dumcfg = './cfg/dummy.cfg'

   ! namcrs
   CHARACTER(LEN=lc) :: cn_coord0 = '' 
   INTEGER(i4)       :: in_perio0 = -1

   ! namvar
   CHARACTER(LEN=lc), DIMENSION(ip_maxvar) :: cn_varinfo = ''

   !namnst
   REAL(sp)          :: rn_lonmin0 = -360.
   REAL(sp)          :: rn_lonmax0 = -360.
   REAL(sp)          :: rn_latmin0 = -360.
   REAL(sp)          :: rn_latmax0 = -360.
   INTEGER(i4)       :: in_imin0 = 0
   INTEGER(i4)       :: in_imax0 = 0
   INTEGER(i4)       :: in_jmin0 = 0
   INTEGER(i4)       :: in_jmax0 = 0
   INTEGER(i4)       :: in_rhoi  = 1
   INTEGER(i4)       :: in_rhoj  = 1

   !namout
   CHARACTER(LEN=lc) :: cn_fileout= 'coord_fine.nc'
   !-------------------------------------------------------------------

   NAMELIST /namlog/ &  !  logger namelist
   &  cn_logfile,    &  !< logger file name
   &  cn_verbosity,  &  !< logger verbosity
   &  in_maxerror       !< logger maximum error

   NAMELIST /namcfg/ &  !  config namelist
   &  cn_varcfg, &       !< variable configuration file
   &  cn_dimcfg, &       !< dimension configuration file
   &  cn_dumcfg          !< dummy configuration file

   NAMELIST /namcrs/ &  !  coarse grid namelist
   &  cn_coord0 , &     !< coordinate file
   &  in_perio0         !< periodicity index

   NAMELIST /namvar/ &  !  variable namelist
   &  cn_varinfo        !< list of variable and extra information about 
                        !< interpolation, extrapolation or filter method to be used. 
                        !< (ex: 'votemper:linear,hann,dist_weight','vosaline:cubic' ) 
   
   NAMELIST /namnst/ &  !  nesting namelist
   &  rn_lonmin0, &     !< lower left  coarse grid longitude
   &  rn_lonmax0, &     !< upper right coarse grid longitude
   &  rn_latmin0, &     !< lower left  coarse grid latitude
   &  rn_latmax0, &     !< upper right coarse grid latitude
   &  in_imin0,   &     !< i-direction lower left  point indice 
   &  in_imax0,   &     !< i-direction upper right point indice
   &  in_jmin0,   &     !< j-direction lower left  point indice
   &  in_jmax0,   &     !< j-direction upper right point indice
   &  in_rhoi,    &     !< refinement factor in i-direction
   &  in_rhoj           !< refinement factor in j-direction

   NAMELIST /namout/ &  !  output namelist
   &  cn_fileout        !< fine grid coordinate file   
   !-------------------------------------------------------------------

   ! namelist
   ! get namelist
   il_narg=COMMAND_ARGUMENT_COUNT() !f03 intrinsec
   IF( il_narg/=1 )THEN
      PRINT *,"ERROR in create_coord: need a namelist"
      STOP
   ELSE
      CALL GET_COMMAND_ARGUMENT(1,cl_namelist) !f03 intrinsec
   ENDIF
   
   ! read namelist
   INQUIRE(FILE=TRIM(cl_namelist), EXIST=ll_exist)
   IF( ll_exist )THEN
 
      il_fileid=fct_getunit()

      OPEN( il_fileid, FILE=TRIM(cl_namelist),   &
      &                FORM='FORMATTED',         &
      &                ACCESS='SEQUENTIAL',      &
      &                STATUS='OLD',             &
      &                ACTION='READ',            &
      &                IOSTAT=il_status)
      CALL fct_err(il_status)
      IF( il_status /= 0 )THEN
         PRINT *,"ERROR in create_coord: error opening "//TRIM(cl_namelist)
         STOP
      ENDIF

      READ( il_fileid, NML = namlog )
      ! define logger file
      CALL logger_open(TRIM(cn_logfile),TRIM(cn_verbosity),in_maxerror)
      CALL logger_header()

      READ( il_fileid, NML = namcfg )
      ! get variable extra information on configuration file
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
      READ( il_fileid, NML = namvar )
      ! add user change in extra information
      CALL var_chg_extra( cn_varinfo )

      READ( il_fileid, NML = namnst )
      READ( il_fileid, NML = namout )

      CLOSE( il_fileid, IOSTAT=il_status )
      CALL fct_err(il_status)
      IF( il_status /= 0 )THEN
         CALL logger_error("CREATE COORD: closing "//TRIM(cl_namelist))
      ENDIF

   ELSE

      PRINT *,"ERROR in create_coord: can't find "//TRIM(cl_namelist)
      STOP

   ENDIF

   ! open files
   IF( cn_coord0 /= '' )THEN
      tl_coord0=mpp_init( file_init(TRIM(cn_coord0)), id_perio=in_perio0)
      CALL grid_get_info(tl_coord0)
   ELSE
      CALL logger_fatal("CREATE COORD: no coarse grid coordinate found. "//&
      &     "check namelist")      
   ENDIF

   ! check
   ! check output file do not already exist
   INQUIRE(FILE=TRIM(cn_fileout), EXIST=ll_exist)
   IF( ll_exist )THEN
      CALL logger_fatal("CREATE COORD: output file "//TRIM(cn_fileout)//&
      &  " already exist.")
   ENDIF

   ! check nesting parameters
   il_index(:)=0
   IF( rn_lonmin0 >= -180. .AND. rn_lonmin0 <= 360 .AND. &
     & rn_latmin0 >= -90.  .AND. rn_latmin0 <= 90. )THEN

      il_index(:)=grid_get_closest(tl_coord0, &
         &                         REAL(rn_lonmin0,dp), REAL(rn_latmin0,dp), &
         &                         cd_pos='ll') 
      il_imin0=il_index(1)
      il_jmin0=il_index(2)
   ELSE
      il_imin0=in_imin0
      il_jmin0=in_jmin0
   ENDIF

   il_index(:)=0
   IF( rn_lonmax0 >= -180. .AND. rn_lonmax0 <= 360 .AND. &
     & rn_latmax0 >= -90.  .AND. rn_latmax0 <= 90. )THEN

      il_index(:)=grid_get_closest(tl_coord0, &
         &                         REAL(rn_lonmax0,dp), REAL(rn_latmax0,dp), &
         &                         cd_pos='ur') 
      il_imax0=il_index(1)
      il_jmax0=il_index(2)
   ELSE
      il_imax0=in_imax0
      il_jmax0=in_jmax0
   ENDIF

   ! forced indices for east west cyclic domain
   IF( rn_lonmin0 == rn_lonmax0 .AND. &
     & rn_lonmin0 /= -360. )THEN
      il_imin0=0
      il_imax0=0
   ENDIF

   IF( il_imin0 < 0 .OR. il_imax0 < 0 .OR. il_jmin0 < 0 .OR. il_jmax0 < 0)THEN
      CALL logger_fatal("CREATE COORD: invalid points indices."//&
      &  " check namelist "//TRIM(cl_namelist))
   ENDIF

   il_rho(:)=1
   IF( in_rhoi < 1 .OR. in_rhoj < 1 )THEN
      CALL logger_error("CREATE COORD: invalid refinement factor."//&
      &  " check namelist "//TRIM(cl_namelist))
   ELSE
      il_rho(jp_I)=in_rhoi
      il_rho(jp_J)=in_rhoj      

      il_offset(:,:,:)=create_coord_get_offset(il_rho(:))
   ENDIF

   ! check domain validity
   CALL grid_check_dom(tl_coord0, il_imin0, il_imax0, il_jmin0, il_jmax0 )

   ! compute domain
   tl_dom=dom_init( tl_coord0,         &
   &                il_imin0, il_imax0,&
   &                il_jmin0, il_jmax0 )

   ! add extra band (if need be) to compute interpolation
   CALL dom_add_extra(tl_dom)

   ! open mpp files
   CALL iom_dom_open(tl_coord0, tl_dom)

   il_nvar=tl_coord0%t_proc(1)%i_nvar
   ALLOCATE( tl_var(il_nvar) )
   DO ji=1,il_nvar

      tl_var(ji)=iom_dom_read_var(tl_coord0, &
      &                          TRIM(tl_coord0%t_proc(1)%t_var(ji)%c_name),&
      &                          tl_dom)

      SELECT CASE(TRIM(tl_var(ji)%c_point))
         CASE('T')
            jj=jp_T
         CASE('U')
            jj=jp_U
         CASE('V')
            jj=jp_V
         CASE('F')
            jj=jp_F
      END SELECT

      ! interpolate variables
      CALL create_coord_interp( tl_var(ji), il_rho(:), &
      &                         il_offset(:,:,jj) )

      ! remove extraband added to domain
      CALL dom_del_extra( tl_var(ji), tl_dom, il_rho(:), .true. )

      ! filter
      CALL filter_fill_value(tl_var(ji))      

   ENDDO

   ! clean
   CALL dom_clean_extra( tl_dom )

   ! close mpp files
   CALL iom_dom_close(tl_coord0)

   ! clean
   CALL mpp_clean(tl_coord0)

   ! create file
   tl_fileout=file_init(TRIM(cn_fileout))

   ! add dimension
   ! save biggest dimension
   tl_dim(:)=var_max_dim(tl_var(:))

   DO ji=1,ip_maxdim
      IF( tl_dim(ji)%l_use ) CALL file_add_dim(tl_fileout, tl_dim(ji))
   ENDDO

   ! add variables
   DO ji=il_nvar,1,-1
      CALL file_add_var(tl_fileout, tl_var(ji))
      CALL var_clean(tl_var(ji))
   ENDDO

   ! add some attribute
   tl_att=att_init("Created_by","SIREN create_coord")
   CALL file_add_att(tl_fileout, tl_att)

   cl_date=date_print(date_now())
   tl_att=att_init("Creation_date",cl_date)
   CALL file_add_att(tl_fileout, tl_att)

   tl_att=att_init("src_file",TRIM(fct_basename(cn_coord0)))
   CALL file_add_att(tl_fileout, tl_att)   

   tl_att=att_init("src_i_indices",(/tl_dom%i_imin,tl_dom%i_imax/))
   CALL file_add_att(tl_fileout, tl_att)   
   tl_att=att_init("src_j_indices",(/tl_dom%i_jmin,tl_dom%i_jmax/))
   CALL file_add_att(tl_fileout, tl_att)
   IF( .NOT. ALL(il_rho(:)==1) )THEN
      tl_att=att_init("refinment_factor",(/il_rho(jp_I),il_rho(jp_J)/))
      CALL file_add_att(tl_fileout, tl_att)
   ENDIF

   ! add attribute periodicity
   il_attid=0
   IF( ASSOCIATED(tl_fileout%t_att) )THEN
      il_attid=att_get_index(tl_fileout%t_att(:),'periodicity')
   ENDIF
   IF( tl_dom%i_perio >= 0 .AND. il_attid == 0 )THEN
      tl_att=att_init('periodicity',tl_dom%i_perio)
      CALL file_add_att(tl_fileout,tl_att)
   ENDIF

   ! add attribute east west overlap
   il_attid=0
   IF( ASSOCIATED(tl_fileout%t_att) )THEN
      il_attid=att_get_index(tl_fileout%t_att(:),'ew_overlap')
   ENDIF
   IF( il_attid == 0 )THEN
      il_ind=var_get_index(tl_fileout%t_var(:),'longitude')
      IF( il_ind == 0 )THEN
         il_ind=var_get_index(tl_fileout%t_var(:),'longitude_T')
      ENDIF
      il_ew=grid_get_ew_overlap(tl_fileout%t_var(il_ind))
      IF( il_ew >= 0 )THEN
         tl_att=att_init('ew_overlap',il_ew)
         CALL file_add_att(tl_fileout,tl_att)
      ENDIF
   ENDIF

   ! create file
   CALL iom_create(tl_fileout)

   ! write file
   CALL iom_write_file(tl_fileout)

   ! close file
   CALL iom_close(tl_fileout)

   ! clean
   CALL att_clean(tl_att)
   CALL var_clean(tl_var(:))
   DEALLOCATE( tl_var) 

   CALL file_clean(tl_fileout)
   CALL var_clean_extra()

   ! close log file
   CALL logger_footer()
   CALL logger_close() 

CONTAINS
   !-------------------------------------------------------------------
   !> @brief
   !> This function compute offset over Arakawa grid points, 
   !> given refinement factor.
   !> 
   !> @author J.Paul
   !> @date August, 2014 - Initial Version
   !>
   !> @param[in] id_rho array of refinement factor
   !> @return array of offset
   !-------------------------------------------------------------------
   FUNCTION create_coord_get_offset( id_rho )
      IMPLICIT NONE
      ! Argument      
      INTEGER(i4), DIMENSION(:), INTENT(IN) :: id_rho

      ! function
      INTEGER(i4), DIMENSION(2,2,ip_npoint) :: create_coord_get_offset
      ! local variable
      ! loop indices
      !----------------------------------------------------------------

      ! case 'T'
      create_coord_get_offset(jp_I,:,jp_T)=FLOOR(REAL(id_rho(jp_I)-1,dp)*0.5)
      create_coord_get_offset(jp_J,:,jp_T)=FLOOR(REAL(id_rho(jp_J)-1,dp)*0.5)
      ! case 'U'
      create_coord_get_offset(jp_I,1,jp_U)=0
      create_coord_get_offset(jp_I,2,jp_U)=id_rho(jp_I)-1
      create_coord_get_offset(jp_J,:,jp_U)=FLOOR(REAL(id_rho(jp_J)-1,dp)*0.5)
      ! case 'V'
      create_coord_get_offset(jp_I,:,jp_V)=FLOOR(REAL(id_rho(jp_I)-1,dp)*0.5)
      create_coord_get_offset(jp_J,1,jp_V)=0
      create_coord_get_offset(jp_J,2,jp_V)=id_rho(jp_J)-1
      ! case 'F'
      create_coord_get_offset(jp_I,1,jp_F)=0
      create_coord_get_offset(jp_I,2,jp_F)=id_rho(jp_I)-1
      create_coord_get_offset(jp_J,1,jp_F)=0
      create_coord_get_offset(jp_J,2,jp_F)=id_rho(jp_J)-1


   END FUNCTION create_coord_get_offset
   !-------------------------------------------------------------------
   !> @brief
   !> This subroutine interpolate variable, given refinment factor.
   !> 
   !> @details 
   !>  Optionaly, you could specify number of points 
   !>    to be extrapolated in i- and j-direction.<br/>
   !>  variable mask is first computed (using _FillValue) and interpolated.<br/>
   !>  variable is then extrapolated, and interpolated.<br/> 
   !>  Finally interpolated mask is applied on refined variable.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !>
   !> @param[inout] td_var variable strcuture 
   !> @param[in] id_rho    array of refinement factor
   !> @param[in] id_offset offset between fine grid and coarse grid
   !> @param[in] id_iext   number of points to be extrapolated in i-direction
   !> @param[in] id_jext   number of points to be extrapolated in j-direction
   !>
   !> @todo check if mask is really needed
   !-------------------------------------------------------------------
   SUBROUTINE create_coord_interp( td_var,          &
   &                               id_rho,          &
   &                               id_offset,       &
   &                               id_iext, id_jext)

      IMPLICIT NONE

      ! Argument
      TYPE(TVAR) ,                 INTENT(INOUT) :: td_var
      INTEGER(i4), DIMENSION(:)  , INTENT(IN   ) :: id_rho
      INTEGER(i4), DIMENSION(:,:), INTENT(IN)    :: id_offset
      INTEGER(i4),                 INTENT(IN   ), OPTIONAL :: id_iext
      INTEGER(i4),                 INTENT(IN   ), OPTIONAL :: id_jext

      ! local variable
      TYPE(TVAR)  :: tl_mask

      INTEGER(i1), DIMENSION(:,:,:,:), ALLOCATABLE :: bl_mask

      INTEGER(i4) :: il_iext
      INTEGER(i4) :: il_jext

      ! loop indices
      !----------------------------------------------------------------

      IF( ANY(SHAPE(id_offset(:,:)) /= 2) )THEN
         CALL logger_error("CREATE COORD INTERP: invalid dimension of "//&
         &                 "offset array")
      ENDIF

      !WARNING: two extrabands are required for cubic interpolation
      il_iext=2
      IF( PRESENT(id_iext) ) il_iext=id_iext

      il_jext=2
      IF( PRESENT(id_jext) ) il_jext=id_jext
      
      IF( il_iext < 2 .AND. td_var%c_interp(1) == 'cubic' )THEN
         CALL logger_warn("CREATE COORD INTERP: at least extrapolation "//&
         &  "on two points are required with cubic interpolation ")
         il_iext=2
      ENDIF

      IF( il_jext < 2 .AND. td_var%c_interp(1) == 'cubic' )THEN
         CALL logger_warn("CREATE COORD INTERP: at least extrapolation "//&
         &  "on two points are required with cubic interpolation ")
         il_jext=2
      ENDIF

      IF( ANY(id_rho(:)>1) )THEN
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
            tl_mask=var_init('tmask',bl_mask(:,:,:,:),td_dim=td_var%t_dim(:),&
            &                id_ew=td_var%i_ew )
         CASE('U')
            tl_mask=var_init('umask',bl_mask(:,:,:,:),td_dim=td_var%t_dim(:),&
            &                id_ew=td_var%i_ew )
         CASE('V')
            tl_mask=var_init('vmask',bl_mask(:,:,:,:),td_dim=td_var%t_dim(:),&
            &                id_ew=td_var%i_ew )
         CASE('F')
            tl_mask=var_init('fmask',bl_mask(:,:,:,:),td_dim=td_var%t_dim(:),&
            &                id_ew=td_var%i_ew )
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

         ! interpolate variable
         CALL interp_fill_value( td_var, id_rho(:), &
         &                       id_offset=id_offset(:,:))

         ! remove extraband
         CALL extrap_del_extrabands(td_var, il_iext*id_rho(jp_I), il_jext*id_rho(jp_J))

         ! keep original mask 
         WHERE( tl_mask%d_value(:,:,:,:) == 0 )
            td_var%d_value(:,:,:,:)=td_var%d_fill
         END WHERE
      ENDIF

      ! clean variable structure
      CALL var_clean(tl_mask)

   END SUBROUTINE create_coord_interp
END PROGRAM create_coord
