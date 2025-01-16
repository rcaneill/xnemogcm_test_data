program create_coordinates
use agrif_util
use par_oce
use user_allocate
use variables
use io_netcdf

  INTEGER :: narg,iargc
  CHARACTER(len=80) :: namelistname
  
call agrif_init_grids()

  narg = iargc()

  IF (narg == 0) THEN
     namelistname = 'namelist.input'
  ELSE
     CALL getarg(1,namelistname)
  ENDIF

  ! read input file (namelist.input)
  CALL read_namelist(namelistname)
  
CALL read_ncdf_var('glamt',TRIM(parent_coordinate_file),glamt)

call allocate_arrays()
stop
end
