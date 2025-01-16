
MODULE ooo_read
   !!==================================================================
   !!                    *** MODULE ooo_read ***
   !! Read routines : I/O for offline obs_oper
   !!==================================================================

   USE mppini
   USE lib_mpp
   USE in_out_manager
   USE par_kind, ONLY: lc
   USE netcdf
   USE oce,     ONLY: tsn, sshn
   USE dom_oce, ONLY: nlci, nlcj, nimpp, njmpp, tmask
   USE par_oce, ONLY: jpi, jpj, jpk
   USE obs_fbm, ONLY: fbimdi, fbrmdi, fbsp, fbdp
   USE ooo_data

   IMPLICIT NONE
   PRIVATE

   PUBLIC ooo_rea_dri

   !! $Id: ooo_read.F90 5215 2015-04-15 16:11:56Z nicolasmartin $
CONTAINS
   SUBROUTINE ooo_rea_dri(kfile)
      !!------------------------------------------------------------------------
      !!             *** ooo_rea_dri ***
      !!
      !! Purpose : To choose appropriate read method
      !! Method  : 
      !!
      !! Author  : A. Ryan Oct 2013
      !!
      !!------------------------------------------------------------------------
      INTEGER, INTENT(IN) :: &
              & kfile         !: File number
      CHARACTER(len=lc) :: &
              & cdfilename, & !: File name
              & cmatchname    !: Match name
      INTEGER :: &
              & kindex       !: File index to read
 
      !! Filename, index and match-up kind
      cdfilename = TRIM(ooo_files(kfile))
      cmatchname = TRIM(cl4_vars(kfile))
      kindex = nn_ooo_idx(kfile)

      !! Update model fields
      !! Class 4 variables: forecast, persistence,
      !!                    nrt_analysis, best_estimate
      !! Feedback variables: empty string
      IF ( (TRIM(cmatchname) == 'forecast') .OR. &
         & (TRIM(cmatchname) == 'persistence') .OR. &
         & (TRIM(cmatchname) == 'nrt_analysis') .OR. &
         & (TRIM(cmatchname) == 'best_estimate').OR. &
         & (TRIM(cmatchname) == '') ) THEN
         CALL ooo_read_file(TRIM(cdfilename), kindex)
         CALL ooo_read_juld(TRIM(cdfilename), kindex, cl4_modjuld)
      ELSE IF (TRIM(cmatchname) == 'climatology') THEN
         WRITE(numout,*) 'Interpolating climatologies'
      ELSE IF (TRIM(cmatchname) == 'altimeter') THEN
         CALL ooo_read_altbias(TRIM(cdfilename))
         CALL ooo_read_juld(TRIM(cdfilename), kindex, cl4_modjuld)
      END IF

   END SUBROUTINE ooo_rea_dri

   SUBROUTINE ooo_read_altbias(filename)
      !!------------------------------------------------------------------------
      !!                      *** ooo_read_altbias ***
      !!
      !! Purpose : To read altimeter bias and set tn,sn to missing values
      !! Method  : Use subdomain indices to create start and count matrices
      !!           for netcdf read.
      !!
      !! Author  : A. Ryan Sept 2012
      !!------------------------------------------------------------------------
      CHARACTER(len=*), INTENT(IN) :: filename
      INTEGER                      :: ncid, &
                                    & varid,&
                                    & istat,&
                                    & ntimes,&
                                    & tdim, &
                                    & xdim, &
                                    & ydim, &
                                    & zdim
      INTEGER                      :: ii, ij, ik
      INTEGER, DIMENSION(3)        :: start_s, &
                                    & count_s
      REAL(fbdp), DIMENSION(:,:),  ALLOCATABLE :: temp_sshn
      REAL(fbdp)                     :: fill_val

      IF (TRIM(filename) == 'nofile') THEN
         tsn(:,:,:,:) = fbrmdi
         sshn(:,:) = fbrmdi 
      ELSE
         ! Open Netcdf file to find dimension id
         istat = nf90_open(trim(filename),nf90_nowrite,ncid)
         istat = nf90_inq_dimid(ncid,'x',xdim)
         istat = nf90_inq_dimid(ncid,'y',ydim)
         istat = nf90_inq_dimid(ncid,'deptht',zdim)
         istat = nf90_inq_dimid(ncid,'time',tdim)
         istat = nf90_inquire_dimension(ncid, tdim, len=ntimes)
         ! Allocate temporary temperature array
         ALLOCATE(temp_sshn(nlci,nlcj))
         ! Create start and count arrays
         start_s = (/ nimpp, njmpp, 1 /)
         count_s = (/ nlci,  nlcj,  1 /)
          
         ! Altimeter bias
         istat = nf90_inq_varid(ncid,'altbias',varid)
         istat = nf90_get_att(ncid, varid, '_FillValue', fill_val)
         istat = nf90_get_var(ncid, varid, temp_sshn, start_s, count_s)
         WHERE(temp_sshn(:,:) == fill_val) temp_sshn(:,:) = fbrmdi
   
         ! Initialise tsn, sshn to fbrmdi
         tsn(:,:,:,:) = fbrmdi
         sshn(:,:) = fbrmdi 

         ! Fill sshn with altimeter bias 
         sshn(1:nlci,1:nlcj) = temp_sshn(:,:) * tmask(1:nlci,1:nlcj,1)

         ! Remove halo from tmask, sshn to prevent double obs counting
         IF (jpi > nlci) THEN
             tmask(nlci+1:,:,:) = 0
             sshn(nlci+1:,:) = 0
         END IF
         IF (jpj > nlcj) THEN
             tmask(:,nlcj+1:,:) = 0
             sshn(:,nlcj+1:) = 0
         END IF

         ! Deallocate arrays
         DEALLOCATE(temp_sshn)

         ! Close netcdf file
         istat = nf90_close(ncid)
      END IF
   
   END SUBROUTINE ooo_read_altbias

   SUBROUTINE ooo_read_file(filename, ifcst)
      !!------------------------------------------------------------------------
      !!             *** ooo_read_file ***
      !!
      !! Purpose : To fill tn and sn with dailymean field from netcdf files
      !! Method  : Use subdomain indices to create start and count matrices
      !!           for netcdf read.
      !!
      !! Author  : A. Ryan Oct 2010
      !!------------------------------------------------------------------------

      INTEGER,          INTENT(IN) :: ifcst
      CHARACTER(len=*), INTENT(IN) :: filename
      INTEGER                      :: ncid, &
                                    & varid,&
                                    & istat,&
                                    & ntimes,&
                                    & tdim, &
                                    & xdim, &
                                    & ydim, &
                                    & zdim
      INTEGER                      :: ii, ij, ik
      INTEGER, DIMENSION(4)        :: start_n, &
                                    & count_n
      INTEGER, DIMENSION(3)        :: start_s, &
                                    & count_s
      REAL(fbdp), DIMENSION(:,:,:),ALLOCATABLE :: temp_tn, &
                                              & temp_sn
      REAL(fbdp), DIMENSION(:,:),  ALLOCATABLE :: temp_sshn
      REAL(fbdp)                     :: fill_val

      ! DEBUG
      INTEGER :: istage

      IF (TRIM(filename) == 'nofile') THEN
         tsn(:,:,:,:) = fbrmdi
         sshn(:,:) = fbrmdi 
      ELSE
         WRITE(numout,*) "Opening :", TRIM(filename)
         ! Open Netcdf file to find dimension id
         istat = nf90_open(path=TRIM(filename), mode=nf90_nowrite, ncid=ncid)
         IF ( istat /= nf90_noerr ) THEN
             WRITE(numout,*) "WARNING: Could not open ", trim(filename)
             WRITE(numout,*) "ERROR: ", nf90_strerror(istat)
         ENDIF
         istat = nf90_inq_dimid(ncid,'x',xdim)
         istat = nf90_inq_dimid(ncid,'y',ydim)
         istat = nf90_inq_dimid(ncid,'deptht',zdim)
         istat = nf90_inq_dimid(ncid,'time_counter',tdim)
         istat = nf90_inquire_dimension(ncid, tdim, len=ntimes)
         IF (ifcst .LE. ntimes) THEN   
            ! Allocate temporary temperature array
            ALLOCATE(temp_tn(nlci,nlcj,jpk))
            ALLOCATE(temp_sn(nlci,nlcj,jpk))
            ALLOCATE(temp_sshn(nlci,nlcj))
      
            ! Set temp_tn, temp_sn to 0.
            temp_tn(:,:,:) = fbrmdi
            temp_sn(:,:,:) = fbrmdi
            temp_sshn(:,:) = fbrmdi
      
            ! Create start and count arrays
            start_n = (/ nimpp, njmpp, 1,   ifcst /)
            count_n = (/ nlci,  nlcj,  jpk, 1     /)
            start_s = (/ nimpp, njmpp, ifcst /)
            count_s = (/ nlci,  nlcj,  1     /)
             
            ! Read information into temporary arrays
            ! retrieve varid and read in temperature
            istat = nf90_inq_varid(ncid,'votemper',varid)
            istat = nf90_get_att(ncid, varid, '_FillValue', fill_val)
            istat = nf90_get_var(ncid, varid, temp_tn, start_n, count_n)
            WHERE(temp_tn(:,:,:) == fill_val) temp_tn(:,:,:) = fbrmdi
      
            ! retrieve varid and read in salinity
            istat = nf90_inq_varid(ncid,'vosaline',varid)
            istat = nf90_get_att(ncid, varid, '_FillValue', fill_val)
            istat = nf90_get_var(ncid, varid, temp_sn, start_n, count_n)
            WHERE(temp_sn(:,:,:) == fill_val) temp_sn(:,:,:) = fbrmdi
      
            ! retrieve varid and read in SSH
            istat = nf90_inq_varid(ncid,'sossheig',varid)
            IF (istat /= nf90_noerr) THEN
               ! Altimeter bias
               istat = nf90_inq_varid(ncid,'altbias',varid)
            END IF
      
            istat = nf90_get_att(ncid, varid, '_FillValue', fill_val)
            istat = nf90_get_var(ncid, varid, temp_sshn, start_s, count_s)
            WHERE(temp_sshn(:,:) == fill_val) temp_sshn(:,:) = fbrmdi
   
            ! Initialise tsn, sshn to fbrmdi
            tsn(:,:,:,:) = fbrmdi
            sshn(:,:) = fbrmdi 

            ! Mask out missing data index
            tsn(1:nlci,1:nlcj,1:jpk,1) = temp_tn(:,:,:) * tmask(1:nlci,1:nlcj,1:jpk)
            tsn(1:nlci,1:nlcj,1:jpk,2) = temp_sn(:,:,:) * tmask(1:nlci,1:nlcj,1:jpk)
            sshn(1:nlci,1:nlcj)        = temp_sshn(:,:) * tmask(1:nlci,1:nlcj,1)

            ! Remove halo from tmask, tsn, sshn to prevent double obs counting
            IF (jpi > nlci) THEN
                tmask(nlci+1:,:,:) = 0
                tsn(nlci+1:,:,:,1) = 0
                tsn(nlci+1:,:,:,2) = 0
                sshn(nlci+1:,:) = 0
            END IF
            IF (jpj > nlcj) THEN
                tmask(:,nlcj+1:,:) = 0
                tsn(:,nlcj+1:,:,1) = 0
                tsn(:,nlcj+1:,:,2) = 0
                sshn(:,nlcj+1:) = 0
            END IF

            ! Deallocate arrays
            DEALLOCATE(temp_tn, temp_sn, temp_sshn)
         ELSE   
            ! Mark all as missing data
            tsn(:,:,:,:) = fbrmdi
            sshn(:,:) = fbrmdi 
         ENDIF
         ! Close netcdf file
         WRITE(numout,*) "Closing :", TRIM(filename)
         istat = nf90_close(ncid)
      END IF
   END SUBROUTINE ooo_read_file

   SUBROUTINE ooo_read_juld(filename, ifcst, julian)
      USE calendar
      !!--------------------------------------------------------------------
      !!                 *** ooo_read_juld ***
      !!
      !!   Purpose : To read model julian day information from file
      !!   Author  : A. Ryan Nov 2010
      !!--------------------------------------------------------------------

      !! Routine arguments
      CHARACTER(len=*), INTENT(IN)  :: filename
      INTEGER,          INTENT(IN)  :: ifcst
      REAL,             INTENT(OUT) :: julian    !: Julian day

      !! Local variables
      INTEGER :: year,  &   !: Date information
               & month, &
               & day,   &
               & hour,  &
               & minute,&
               & second
      INTEGER :: istat, &   !: Netcdf variables
               & ncid,  &
               & dimid, &
               & varid, &
               & ntime      
      REAL,DIMENSION(:),ALLOCATABLE :: r_sec     !: Remainder seconds
      CHARACTER(len=120) :: time_str  !: time string

      time_str=''
      !! Read in time_counter and remainder seconds
      istat = nf90_open(trim(filename),nf90_nowrite,ncid)
      istat = nf90_inq_dimid(ncid,'time_counter',dimid)
      IF (istat /= nf90_noerr) THEN
          istat = nf90_inq_dimid(ncid,'time',dimid)
      ENDIF
      istat = nf90_inquire_dimension(ncid,dimid,len=ntime)
      istat = nf90_inq_varid(ncid,'time_counter',varid)
      IF (istat /= nf90_noerr) THEN
          istat = nf90_inq_dimid(ncid,'time',dimid)
      ENDIF
      istat = nf90_get_att(ncid,varid,'units',time_str)
      ALLOCATE(r_sec(ntime))
      istat = nf90_get_var(ncid,varid, r_sec)
      istat = nf90_close(ncid)

      !! Fill yyyy-mm-dd hh:mm:ss
      !! format(('seconds since ', I4.4,'-',I2.2,'-',I2.2,' ',I2.2,':',I2.2,':',I2.2))
      100 format((14x, I4.4,1x,I2.2,1x,I2.2,1x,I2.2,1x,I2.2,1x,I2.2))
      READ( time_str, 100 ) year, month, day, hour, minute, second

      CALL ymds2ju(year, month, day, r_sec(ifcst), julian)

      !! To take a comment from the ymds2ju subroutine

   !- In the case of the Gregorian calendar we have chosen to use
   !- the Lilian day numbers. This is the day counter which starts
   !- on the 15th October 1582.
   !- This is the day at which Pope Gregory XIII introduced the
   !- Gregorian calendar.
   !- Compared to the true Julian calendar, which starts some
   !- 7980 years ago, the Lilian days are smaler and are dealt with
   !- easily on 32 bit machines. With the true Julian days you can only
   !- the fraction of the day in the real part to a precision of
   !- a 1/4 of a day with 32 bits.
      
      !! The obs operator routines prefer to calculate Julian days since 
      !! 01/01/1950 00:00:00
      !! In order to convert to the 1950 version we must adjust by the number
      !! of days between 15th October 1582 and 1st Jan 1950

      julian = julian - 134123.
      
      DEALLOCATE(r_sec)
      
   END SUBROUTINE ooo_read_juld

END MODULE ooo_read 

