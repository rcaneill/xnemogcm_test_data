MODULE ooo_write
   !!======================================================================
   !!                       ***  MODULE ooo_write  ***
   !!======================================================================

   USE in_out_manager
   USE netcdf
   USE obs_utils, ONLY: chkerr
   USE ooo_utils, ONLY: date_format, inst_converter, yyyymmdd_to_ref_date
   USE ooo_data

   IMPLICIT NONE
   PRIVATE

   PUBLIC ooo_wri_init
   PUBLIC ooo_wri_default
   PUBLIC ooo_wri_extra

   ! Type kinds for class 4 data.
   INTEGER, PARAMETER :: clsp = SELECTED_REAL_KIND( 6, 37) !: single precision
   INTEGER, PARAMETER :: cldp = SELECTED_REAL_KIND(12,307) !: double precision

   ! Missinge data indicators 
   INTEGER, PARAMETER    :: climdi = -99999   !: Integers
   REAL(clsp), PARAMETER :: clrmdi =  99999   !: Reals

   INTERFACE ooo_wri_extra
      MODULE PROCEDURE ooo_wri_extra_3d_index, ooo_wri_extra_4d, ooo_wri_extra_4d_index
   END INTERFACE

   !! $Id: ooo_write.F90 5215 2015-04-15 16:11:56Z nicolasmartin $
   CONTAINS

      SUBROUTINE ooo_wri_extra_3d_index(cdfilename, cdvarname, ndeps, nvars, &
                               &  nobs, kstart, kcount, pdata)
         !!----------------------------------------------------------------------
         !!                    ***  ROUTINE ooo_wri_extra_3d  ***
         !!
         !! ** Purpose : Write 3d variables to class 4 file.
         !!
         !!----------------------------------------------------------------------
         INTEGER, INTENT(IN) :: &
                 & nobs, &       !: number of observations/profiles
                 & nvars, &      !: number of physical parameters
                 & ndeps         !: number of depths
         CHARACTER(len=*), INTENT(IN) :: &
                 & cdfilename, & !: netcdf file name
                 & cdvarname     !: netcdf variable name
         INTEGER, DIMENSION(3), INTENT(IN) :: &
                 & kstart, &     !: start indices
                 & kcount        !: count indices
         REAL(KIND=cldp), DIMENSION(ndeps, nvars, nobs), INTENT(IN) :: &
                 & pdata         !: 3d data
         INTEGER :: &
                 & ncid, &       !:
                 & dimid, &      !:
                 & varid         !:
         CHARACTER(len=16), PARAMETER :: cpname = 'ooo_wri_extra_3d'
         ! Open netcdf file
         CALL chkerr(nf90_open(trim(cdfilename), nf90_write, ncid), cpname, __LINE__ )
         ! Write data
         CALL chkerr(nf90_inq_varid(ncid,TRIM(cdvarname), varid),cpname, __LINE__ )
         CALL chkerr(nf90_put_var(ncid, varid, pdata, kstart, kcount),cpname, __LINE__ )
         ! Close netcdf file
         CALL chkerr(nf90_close(ncid), cpname, __LINE__ )
      END SUBROUTINE ooo_wri_extra_3d_index

      SUBROUTINE ooo_wri_extra_4d_index(cdfilename, cdvarname, ndeps, nfcst, &
                               &  nvars, nobs, kstart, kcount, pdata)
         !!----------------------------------------------------------------------
         !!                    ***  ROUTINE ooo_wri_extra_4d  ***
         !!
         !! ** Purpose : Write 4d variables to class 4 file.
         !!
         !!----------------------------------------------------------------------
         INTEGER, INTENT(IN) :: &
                 & nobs, &       !: number of observations/profiles
                 & nvars, &      !: number of physical parameters
                 & ndeps, &      !: number of depths
                 & nfcst         !: number of forecasts
         CHARACTER(len=*), INTENT(IN) :: &
                 & cdfilename, & !: netcdf file name
                 & cdvarname     !: netcdf variable name
         INTEGER, DIMENSION(4), INTENT(IN) :: &
                 & kstart, &     !: start indices
                 & kcount        !: count indices
         REAL(KIND=cldp), DIMENSION(ndeps, nvars, nobs), INTENT(IN) :: &
                 & pdata         !: slice of 4d data
         INTEGER :: &
                 & ncid, &       !:
                 & dimid, &      !:
                 & varid         !:
         CHARACTER(len=22), PARAMETER :: cpname = 'ooo_wri_extra_4d_index'
         ! Open netcdf file
         CALL chkerr(nf90_open(trim(cdfilename), nf90_write, ncid), cpname, __LINE__ )
         ! Write data
         CALL chkerr(nf90_inq_varid(ncid,TRIM(cdvarname), varid),cpname, __LINE__ )
         CALL chkerr(nf90_put_var(ncid, varid, pdata, kstart, kcount),cpname, __LINE__ )
         ! Close netcdf file
         CALL chkerr(nf90_close(ncid), cpname, __LINE__ )
      END SUBROUTINE ooo_wri_extra_4d_index

      SUBROUTINE ooo_wri_extra_4d(cdfilename, cdvarname, ndeps, nfcst, &
                               &  nvars, nobs, pdata)
         !!----------------------------------------------------------------------
         !!                    ***  ROUTINE ooo_wri_extra_4d  ***
         !!
         !! ** Purpose : Write 4d variables to class 4 file.
         !!
         !!----------------------------------------------------------------------
         INTEGER, INTENT(IN) :: &
                 & nobs, &       !: number of observations/profiles
                 & nvars, &      !: number of physical parameters
                 & ndeps, &      !: number of depths
                 & nfcst         !: number of forecasts
         CHARACTER(len=*), INTENT(IN) :: &
                 & cdfilename, & !: netcdf file name
                 & cdvarname     !: netcdf variable name
         REAL(KIND=cldp), DIMENSION(ndeps, nfcst, nvars, nobs), INTENT(IN) :: &
                 & pdata         !: 4d data
         INTEGER :: &
                 & ncid, &       !:
                 & dimid, &      !:
                 & varid         !:
         CHARACTER(len=16), PARAMETER :: cpname = 'ooo_wri_extra_4d'
         ! Open netcdf file
         CALL chkerr(nf90_open(trim(cdfilename), nf90_write, ncid), cpname, __LINE__ )
         ! Write data
         CALL chkerr(nf90_inq_varid(ncid,TRIM(cdvarname), varid),cpname, __LINE__ )
         CALL chkerr(nf90_put_var(ncid, varid, pdata),cpname, __LINE__ )
         ! Close netcdf file
         CALL chkerr(nf90_close(ncid), cpname, __LINE__ )
      END SUBROUTINE ooo_wri_extra_4d

      SUBROUTINE ooo_wri_default(cdfilename, nobs, nvars, nfcst, ndeps, & 
                             &   cdtyp, cdwmo, cunit, cvnam, &
                             &   plam, pphi, pdep, ptim, pob, plead, &
                             &   kqc, pmjuld)
         !!----------------------------------------------------------------------
         !!                    ***  ROUTINE ooo_wri_default  ***
         !!
         !! ** Purpose : Write standard variables to class 4 file.
         !! ** Method  : Write the following variables;
         !!                 observation type
         !!                 observation wmo id
         !!                 variable units
         !!                 variable names
         !!                 observation latitudes
         !!                 observation longitudes
         !!                 observation depths
         !!                 observation times
         !!                 observation values
         !!                 observation qc flags
         !!                 model julian days
         !!                 
         !! ** Returns : 
         !!----------------------------------------------------------------------
         INTEGER, INTENT(IN) :: &
                 & nfcst, &      !: number of forecasts
                 & nobs, &       !: number of observations/profiles
                 & nvars, &      !: number of physical parameters
                 & ndeps         !: number of depths
         CHARACTER(len=*), INTENT(IN) :: &
                 & cdfilename    !: file name
         CHARACTER(LEN=128), DIMENSION(nobs), INTENT(IN) :: &
                 & cdtyp         !: Instrument type
         CHARACTER(LEN=8), DIMENSION(nobs), INTENT(IN) :: &
                 & cdwmo         ! WMO number
         CHARACTER(LEN=128), DIMENSION(nobs) :: &
                 & cdpad         !: Instrument type (padded)
         CHARACTER(LEN=8), DIMENSION(nvars), INTENT(IN) :: &
                 & cunit, &      !: variable units (e.g. psu)
                 & cvnam         !: variable names (e.g. vosaline)
         REAL(KIND=cldp), DIMENSION(nfcst), INTENT(IN) :: &
                 & plead         !: Leadtime
         REAL(KIND=cldp), DIMENSION(nobs), INTENT(IN) :: &
                 & pphi, &       !: Latitude
                 & plam          !: Longitude
         REAL(KIND=cldp), DIMENSION(ndeps, nobs), INTENT(IN) :: &
                 & pdep          !: Depth               
         REAL(KIND=cldp), DIMENSION(ndeps, nvars, nobs), INTENT(IN) :: &
                 & pob           !: T observation
         REAL(KIND=cldp), DIMENSION(nobs), INTENT(IN) :: &
                 & ptim          !: Time
         INTEGER(KIND=2), DIMENSION(ndeps, nvars, nobs), INTENT(IN) :: &
                 & kqc           !: Observation QC
         REAL, INTENT(IN) :: &
                 & pmjuld        !: Model Julian Day
         CHARACTER(len=128) :: &
                 & cvar          !: variable name placeholder
         INTEGER :: &
                 & ncid, &  !:
                 & dimid, & !:
                 & varid    !:
         CHARACTER(len=15), PARAMETER :: cpname = 'ooo_wri_default'
         ! Open netcdf file
         CALL chkerr(nf90_open(trim(cdfilename), nf90_write, ncid), cpname, __LINE__ )
         ! longitude
         cvar = 'longitude'
         CALL chkerr(nf90_inq_varid(ncid,TRIM(cvar), varid),cpname, __LINE__ )
         CALL chkerr(nf90_put_var(ncid, varid, plam),cpname, __LINE__ )
         ! latitude
         cvar = 'latitude'
         CALL chkerr(nf90_inq_varid(ncid,TRIM(cvar), varid),cpname, __LINE__ )
         CALL chkerr(nf90_put_var(ncid, varid, pphi),cpname, __LINE__ )
         ! depth
         cvar = 'depth'
         CALL chkerr(nf90_inq_varid(ncid,TRIM(cvar), varid),cpname, __LINE__ )
         CALL chkerr(nf90_put_var(ncid, varid, pdep),cpname, __LINE__ )
         ! varname
         cvar = 'varname'
         CALL chkerr(nf90_inq_varid(ncid,TRIM(cvar), varid),cpname, __LINE__ )
         CALL chkerr(nf90_put_var(ncid, varid, cvnam),cpname, __LINE__ )
         ! unitname
         cvar = 'unitname'
         CALL chkerr(nf90_inq_varid(ncid,TRIM(cvar), varid),cpname, __LINE__ )
         CALL chkerr(nf90_put_var(ncid, varid, cunit),cpname, __LINE__ )
         ! leadtime
         cvar = 'leadtime'
         CALL chkerr(nf90_inq_varid(ncid,TRIM(cvar), varid),cpname, __LINE__ )
         CALL chkerr(nf90_put_var(ncid, varid, plead),cpname, __LINE__ )
         ! observation
         cvar = 'observation'
         CALL chkerr(nf90_inq_varid(ncid,TRIM(cvar), varid),cpname, __LINE__ )
         CALL chkerr(nf90_put_var(ncid, varid, pob),cpname, __LINE__ )
         ! qc
         cvar = 'qc'
         CALL chkerr(nf90_inq_varid(ncid,TRIM(cvar), varid),cpname, __LINE__ )
         CALL chkerr(nf90_put_var(ncid, varid, kqc),cpname, __LINE__ )
         ! juld
         cvar = 'juld'
         CALL chkerr(nf90_inq_varid(ncid,TRIM(cvar), varid),cpname, __LINE__ )
         CALL chkerr(nf90_put_var(ncid, varid, ptim),cpname, __LINE__ )
         ! type
         cvar = 'type'
         CALL inst_converter(cdtyp,nobs,cdpad)
         CALL chkerr(nf90_inq_varid(ncid,TRIM(cvar), varid),cpname, __LINE__ )
         CALL chkerr(nf90_put_var(ncid, varid, cdpad),cpname, __LINE__ )
         ! id
         cvar = 'id'
         CALL chkerr(nf90_inq_varid(ncid,TRIM(cvar), varid),cpname, __LINE__ )
         CALL chkerr(nf90_put_var(ncid, varid, cdwmo),cpname, __LINE__ )
         ! Close netcdf file
         CALL chkerr(nf90_close(ncid), cpname, __LINE__ )
      END SUBROUTINE ooo_wri_default

      SUBROUTINE ooo_wri_init(cconf, csys, ckind, cversion, ccont, &
                            & cinst, cdate, nproc, nobs, nvars, &
                            & ndeps, nfcst, cdfilename)
         !!----------------------------------------------------------------------
         !!                    ***  ROUTINE ooo_wri_init  ***
         !!
         !! ** Purpose : Initialise a class 4 file.
         !!
         !! ** Method  :
         !! ** Returns : cdfilename
         !!----------------------------------------------------------------------
         CHARACTER(len=*), INTENT(IN) :: &
                 & cconf, &      !: model configuration e.g. orca025
                 & csys, &       !: model system e.g. FOAM
                 & ckind, &      !: observation kind e.g. profile
                 & cversion, &   !: model version e.g. 12.0
                 & ccont, &      !: contact email
                 & cinst, &      !: institution description
                 & cdate         !: e.g. yyyymmdd
         INTEGER, INTENT(IN) :: &
                 & nproc, &      !: processor number
                 & nobs, &       !: number of observations/profiles
                 & nvars, &      !: number of physical parameters
                 & ndeps, &      !: number of depths
                 & nfcst         !: number of forecast days
         CHARACTER(len=128), INTENT(OUT) :: &
                 & cdfilename    !: e.g. orca025
         CHARACTER(len=4) :: &
                 & cproc         !: character representation of nproc
         CHARACTER(len=128) :: &
                 & cattnam, &    !: attribute placeholder
                 & cattval, &    !: attribute placeholder
                 & cvar, &       !: variable name placeholder
                 & cqcdes, &     !: quality control description
                 & cqcval        !: quality control values
         INTEGER :: &
                 & istat, &      !: status of netcdf operation
                 & ncid, &       !: netcdf file id placeholder
                 & dimid_d, &    !: netcdf dimension id placeholder
                 & dimid_f, &    !: netcdf dimension id placeholder
                 & dimid_o, &    !: netcdf dimension id placeholder
                 & dimid_v, &    !: netcdf dimension id placeholder
                 & dimid_s, &    !: netcdf dimension id placeholder
                 & dimid_l, &    !: netcdf dimension id placeholder
                 & varid         !: netcdf variable id placeholder
         INTEGER, DIMENSION(2) :: &
                 & qc_flags      !: quality control flags
         CHARACTER(len=23) :: &
                 & ref_date      !: reference date holder
         INTEGER, DIMENSION(2) :: &
                 & dim2a, &      !: 2 dimensional settings
                 & dim2b, &      !:
                 & dim2c, &      !:
                 & dim2d         !:
         INTEGER, DIMENSION(3) :: &
                 & dim3a         !: 3 dimensional settings
         INTEGER, DIMENSION(4) :: &
                 & dim4a         !: 4 dimensional settings
         CHARACTER(len=12), PARAMETER :: cpname = 'ooo_wri_init'
         ! Global att variables
         CHARACTER(len=40) :: date_str
         CHARACTER(len=6)  :: noon

         WRITE(cproc, FMT="(I4.4)") nproc
         cdfilename = 'class4'
         cdfilename = TRIM(cdfilename)//'_'//TRIM(cdate)
         cdfilename = TRIM(cdfilename)//'_'//TRIM(csys)
         cdfilename = TRIM(cdfilename)//'_'//TRIM(cconf)
         cdfilename = TRIM(cdfilename)//'_'//TRIM(cversion)
         cdfilename = TRIM(cdfilename)//'_'//TRIM(ckind)
         cdfilename = TRIM(cdfilename)//'_'//TRIM(cproc)
         cdfilename = TRIM(cdfilename)//'.nc'
         ! QC attribute string settings
         IF (ckind .EQ. 'profile') THEN
            cqcval = '0 - good data. 9 - bad data.'
            cqcdes = ''
            qc_flags = (/0, 9/)
         ELSE IF (ckind .EQ. 'SST') THEN
            cqcval = '0 - good data. 1 - bad data.'
            cqcdes = 'In situ qc flag set to 0 if prob. of gross error < 0.5, &
                     &1 otherwise. AATSR qc flag set to 0 for best_quality, 1 otherwise.'
            qc_flags = (/0, 1/)
         ELSE IF (ckind .EQ. 'SLA') THEN
            cqcval = '0 - good data. 9 - bad data.'
            cqcdes = ''
            qc_flags = (/0, 9/)
         ELSE
            cqcval = ''
            cqcdes = ''
            qc_flags = (/0, 9/)
         END IF
         ! Check cdfilename exists
         istat = nf90_open(TRIM(cdfilename), nf90_nowrite, ncid)
         IF (istat /= nf90_noerr) THEN
            IF (lwp) WRITE(numout, *) TRIM(cdfilename), ' opened successfully.'
            CALL chkerr( nf90_create(TRIM(cdfilename), nf90_clobber, ncid), cpname, __LINE__ )
            !! Global Dimension section
            !  Partially hardwired for now
            CALL date_format(date_str)
            noon = '120000' ! hhmmss
            ref_date = ''
            CALL yyyymmdd_to_ref_date(TRIM(cdate), noon, ref_date)

            CALL chkerr( nf90_put_att(ncid, nf90_global, 'title', &
               & 'Forecast class 4 file'), cpname, __LINE__ )
            CALL chkerr( nf90_put_att(ncid, nf90_global, 'version', &
               & TRIM(cversion)), cpname, __LINE__ )
            CALL chkerr( nf90_put_att(ncid, nf90_global, 'creation_date', &
               & TRIM(date_str) ), cpname, __LINE__ )
            CALL chkerr( nf90_put_att(ncid, nf90_global, 'validity_time', &
               & TRIM(ref_date) ), cpname, __LINE__ )
            CALL chkerr( nf90_put_att(ncid, nf90_global, 'contact', &
               & TRIM(ccont) ), cpname, __LINE__ )
            CALL chkerr( nf90_put_att(ncid, nf90_global, 'obs_type', &
               & TRIM(ckind) ), cpname, __LINE__ )
            CALL chkerr( nf90_put_att(ncid, nf90_global, 'system', &
               & TRIM(csys)), cpname, __LINE__ )
            CALL chkerr( nf90_put_att(ncid, nf90_global, 'configuration', &
               & TRIM(cconf) ), cpname, __LINE__ ) 
            CALL chkerr( nf90_put_att(ncid, nf90_global, 'institution', &
               & TRIM(cinst) ), cpname, __LINE__ ) 

            !! Define Dimensions
            CALL chkerr(nf90_def_dim(ncid, 'numdeps', ndeps, dimid_d), cpname, __LINE__ )
            CALL chkerr(nf90_def_dim(ncid, 'numfcsts', nfcst, dimid_f), cpname, __LINE__ )
            CALL chkerr(nf90_def_dim(ncid, 'numvars', nvars, dimid_v), cpname, __LINE__ )
            CALL chkerr(nf90_def_dim(ncid, 'numobs', nobs, dimid_o), cpname, __LINE__ )
            CALL chkerr(nf90_def_dim(ncid, 'string_length8', 8, dimid_s), cpname, __LINE__ )
            CALL chkerr(nf90_def_dim(ncid, 'string_length128', 128, dimid_l), cpname, __LINE__ )
            !! Define possible dimension permutations
            ! 2d
            dim2a(:) = (/ dimid_d, dimid_o /) !: (/ ndeps, nobs  /) 
            dim2b(:) = (/ dimid_s, dimid_o /) !: (/ 8, nobs  /)
            dim2c(:) = (/ dimid_s, dimid_v /) !: (/ 8, nvars /)
            dim2d(:) = (/ dimid_l, dimid_o /) !: (/ 128, nobs  /)
            ! 3d
            dim3a(:) = (/ dimid_d, dimid_v, dimid_o/) !: (/ ndeps, nvars, nobs /)
            ! 4d
            dim4a(:) = (/ dimid_d, dimid_f, dimid_v, dimid_o /) !: (/ ndeps, nfcst, nvars, nobs /)

            !! Create the variables
            !  Forecast day
            cvar = 'leadtime'
            CALL chkerr(nf90_def_var(ncid, TRIM(cvar), nf90_float, dimid_f, varid), cpname, __LINE__ )
            cattnam = 'long_name'
            cattval = 'Model forecast day offset'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )
            cattnam = 'units'
            cattval = 'Hours'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )
            cattnam = 'comment'
            cattval = 'Hours between forecast production and validity time'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )

            !  longitude
            cvar = 'longitude'
            CALL chkerr(nf90_def_var(ncid, TRIM(cvar), nf90_float, dimid_o, varid), cpname, __LINE__ )
            cattnam = 'long_name'
            cattval = 'Longitudes'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )
            cattnam = 'units'
            cattval = 'Degrees'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )

            !  latitude
            cvar = 'latitude'
            CALL chkerr(nf90_def_var(ncid, TRIM(cvar), nf90_float, dimid_o, varid), cpname, __LINE__ )
            cattnam = 'long_name'
            cattval = 'Latitudes'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )
            cattnam = 'units'
            cattval = 'Degrees'

            !  depth
            cvar = 'depth'
            CALL chkerr(nf90_def_var(ncid, TRIM(cvar), nf90_float, dim2a, varid), cpname, __LINE__ )
            CALL chkerr(nf90_put_att(ncid, varid, '_FillValue', clrmdi), cpname, __LINE__ )
            cattnam = 'long_name'
            cattval = 'Depths'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )
            cattnam = 'units'
            cattval = 'metre'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )

            !  varname
            cvar = 'varname'
            CALL chkerr(nf90_def_var(ncid, TRIM(cvar), nf90_char, dim2c, varid), cpname, __LINE__ )
            cattnam = 'long_name'
            cattval = 'Variable name'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )

            !  unitname
            cvar = 'unitname'
            CALL chkerr(nf90_def_var(ncid, TRIM(cvar), nf90_char, dim2c, varid), cpname, __LINE__ )
            cattnam = 'long_name'
            cattval = 'Unit name'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )

            !  observation
            cvar = 'observation'
            CALL chkerr(nf90_def_var(ncid, TRIM(cvar), nf90_float, dim3a, varid), cpname, __LINE__ )
            CALL chkerr(nf90_put_att(ncid, varid, '_FillValue', clrmdi), cpname, __LINE__ )
            cattnam = 'long_name'
            cattval = 'Observation value'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )

            !  forecast
            cvar = 'forecast'
            CALL chkerr(nf90_def_var(ncid, TRIM(cvar), nf90_float, dim4a, varid), cpname, __LINE__ )
            CALL chkerr(nf90_put_att(ncid, varid, '_FillValue', clrmdi), cpname, __LINE__ )
            cattnam = 'long_name'
            cattval = 'Model forecast counterpart of obs. value'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )
            cattnam = 'comment'
            cattval = 'Model daily mean valid at noon used for calculation'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )

            !  persistence
            cvar = 'persistence'
            CALL chkerr(nf90_def_var(ncid, TRIM(cvar), nf90_float, dim4a, varid), cpname, __LINE__ )
            CALL chkerr(nf90_put_att(ncid, varid, '_FillValue', clrmdi), cpname, __LINE__ )
            cattnam = 'long_name'
            cattval = 'Model persistence counterpart of obs. value'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )
            cattnam = 'comment'
            cattval = 'Model daily mean valid at noon used for calculation'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )

            !  climatology
            cvar = 'climatology'
            CALL chkerr(nf90_def_var(ncid, TRIM(cvar), nf90_float, dim3a, varid), cpname, __LINE__ )
            CALL chkerr(nf90_put_att(ncid, varid, '_FillValue', clrmdi), cpname, __LINE__ )
            cattnam = 'long_name'
            cattval = 'Climatological value'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )
            cattnam = 'comment'
            cattval = 'Levitus monthly fields interpolated to the correct day'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )

            !  daym2
            cvar = 'best_estimate'
            CALL chkerr(nf90_def_var(ncid, TRIM(cvar), nf90_float, dim3a, varid), cpname, __LINE__ )
            CALL chkerr(nf90_put_att(ncid, varid, '_FillValue', clrmdi), cpname, __LINE__ )
            cattnam = 'long_name'
            cattval = 'Best estimate'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )
            cattnam = 'comment'
            cattval = 'FOAM daym2 field'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )

            !  daym1
            cvar = 'nrt_analysis'
            CALL chkerr(nf90_def_var(ncid, TRIM(cvar), nf90_float, dim3a, varid), cpname, __LINE__ )
            CALL chkerr(nf90_put_att(ncid, varid, '_FillValue', clrmdi), cpname, __LINE__ )
            cattnam = 'long_name'
            cattval = 'Near real time analysis'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )
            cattnam = 'comment'
            cattval = 'FOAM daym1 field'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )
         
            !  optional fields
            IF (TRIM(ckind) .EQ. 'SLA') THEN
               !  mdt
               cvar = 'mdt_reference'
               CALL chkerr(nf90_def_var(ncid, TRIM(cvar), nf90_float, dim3a, varid), cpname, __LINE__ )
               CALL chkerr(nf90_put_att(ncid, varid, '_FillValue', clrmdi), cpname, __LINE__ )
               cattnam = 'long_name'
               cattval = 'Mean dynamic topography'
               CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )
               cattnam = 'comment'
               cattval = 'MDT reference field'
               CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )

               !  altbias
               cvar = 'altimeter_bias'
               CALL chkerr(nf90_def_var(ncid, TRIM(cvar), nf90_float, dim3a, varid), cpname, __LINE__ )
               CALL chkerr(nf90_put_att(ncid, varid, '_FillValue', clrmdi), cpname, __LINE__ )
               cattnam = 'long_name'
               cattval = 'Altimeter bias'
               CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )
            ENDIF
         
            !  qc
            cvar = 'qc'
            CALL chkerr(nf90_def_var(ncid, TRIM(cvar), nf90_short, dim3a, varid), cpname, __LINE__ )
            CALL chkerr(nf90_put_att(ncid, varid, '_FillValue', NF90_FILL_SHORT), cpname, __LINE__ )
            cattnam = 'long_name'
            cattval = 'Quality flags'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )
            cattnam = 'flag_value'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), qc_flags), cpname, __LINE__ )
            cattnam = 'flag_meaning'
            cattval = cqcval
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )
            cattnam = 'comment'
            cattval = cqcdes
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )

!            !  juld
            cvar = 'juld'
            CALL chkerr(nf90_def_var(ncid, TRIM(cvar), nf90_double, dimid_o, varid), cpname, __LINE__ )
!            CALL chkerr(nf90_put_att(ncid, varid, '_FillValue', clrmdi), cpname, __LINE__ )
            cattnam = 'long_name'
            cattval = 'Observation time in Julian days'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )
            cattnam = 'units'
            CALL yyyymmdd_to_ref_date('19500101', '000000', ref_date)
            cattval = 'Days since '//ref_date
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )

            !  modeljuld 
            cvar = 'modeljuld'
            CALL chkerr(nf90_def_var(ncid, TRIM(cvar), nf90_float, dimid_f, varid), cpname, __LINE__ )
            cattnam = 'long_name'
            cattval = 'Model field date in Julian days'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )
            cattnam = 'units'
            cattval = 'Days since '//ref_date
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )

            !  type
            cvar = 'type'
            CALL chkerr(nf90_def_var(ncid, TRIM(cvar), nf90_char, dim2d, varid), cpname, __LINE__ )
            cattnam = 'long_name'
            cattval = 'Observation type'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )

            !  id
            cvar = 'id'
            CALL chkerr(nf90_def_var(ncid, TRIM(cvar), nf90_char, dim2b, varid), cpname, __LINE__ )
            cattnam = 'long_name'
            cattval = 'Observation id'
            CALL chkerr(nf90_put_att(ncid, varid, TRIM(cattnam), TRIM(cattval)), cpname, __LINE__ )

            ! Close netcdf file
            CALL chkerr(nf90_close(ncid), cpname, __LINE__ )
         ELSE
            IF (lwp) WRITE(numout, *) TRIM(cdfilename), 'already exists.'
            ! Close netcdf file
            CALL chkerr(nf90_close(ncid), cpname, __LINE__ )
         END IF
      END SUBROUTINE ooo_wri_init


END MODULE ooo_write
