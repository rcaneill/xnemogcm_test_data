MODULE obs_read_prof
   !!======================================================================
   !!                       ***  MODULE obs_read_prof  ***
   !! Observation diagnostics: Read the T and S profile observations
   !!======================================================================

   !!----------------------------------------------------------------------
   !!   obs_rea_pro_dri : Driver for reading profile obs
   !!----------------------------------------------------------------------

   !! * Modules used   
   USE par_kind                 ! Precision variables
   USE par_oce                  ! Ocean parameters
   USE in_out_manager           ! I/O manager
   USE dom_oce                  ! Ocean space and time domain variables
   USE obs_mpp                  ! MPP support routines for observation diagnostics
   USE julian                   ! Julian date routines
   USE obs_utils                ! Observation operator utility functions
   USE obs_prep                 ! Prepare observation arrays
   USE obs_grid                 ! Grid search
   USE obs_sort                 ! Sorting observation arrays
   USE obs_profiles_def         ! Profile definitions
   USE obs_conv                 ! Various conversion routines
   USE obs_types                ! Observation type definitions
   USE netcdf                   ! NetCDF library
   USE obs_oper                 ! Observation operators
   USE lib_mpp                  ! For ctl_warn/stop
   USE obs_fbm                  ! Feedback routines

   IMPLICIT NONE

   !! * Routine accessibility
   PRIVATE

   PUBLIC obs_rea_prof  ! Read the profile observations 

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: obs_read_prof.F90 14275 2021-01-07 12:13:16Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE obs_rea_prof( profdata, knumfiles, cdfilenames, &
      &                     kvars, kextr, kstp, ddobsini, ddobsend, &
      &                     ldvar, ldignmis, ldsatt, &
      &                     ldmod, cdvars, kdailyavtypes )
      !!---------------------------------------------------------------------
      !!
      !!                   *** ROUTINE obs_rea_prof ***
      !!
      !! ** Purpose : Read from file the profile observations
      !!
      !! ** Method  : Read feedback data in and transform to NEMO internal 
      !!              profile data structure
      !!
      !! ** Action  : 
      !!
      !! References : 
      !!
      !! History :  
      !!      ! :  2009-09 (K. Mogensen) : New merged version of old routines
      !!      ! :  2015-08 (M. Martin) : Merged profile and velocity routines
      !!----------------------------------------------------------------------

      !! * Arguments
      TYPE(obs_prof), INTENT(OUT) :: &
         & profdata                     ! Profile data to be read
      INTEGER, INTENT(IN) :: knumfiles  ! Number of files to read
      CHARACTER(LEN=128), INTENT(IN) ::  &
         & cdfilenames(knumfiles)        ! File names to read in
      INTEGER, INTENT(IN) :: kvars      ! Number of variables in profdata
      INTEGER, INTENT(IN) :: kextr      ! Number of extra fields for each var
      INTEGER, INTENT(IN) :: kstp       ! Ocean time-step index
      LOGICAL, DIMENSION(kvars), INTENT(IN) :: ldvar     ! Observed variables switches
      LOGICAL, INTENT(IN) :: ldignmis   ! Ignore missing files
      LOGICAL, INTENT(IN) :: ldsatt     ! Compute salinity at all temperature points
      LOGICAL, INTENT(IN) :: ldmod      ! Initialize model from input data
      REAL(dp), INTENT(IN) :: ddobsini  ! Obs. ini time in YYYYMMDD.HHMMSS
      REAL(dp), INTENT(IN) :: ddobsend  ! Obs. end time in YYYYMMDD.HHMMSS
      CHARACTER(len=8), DIMENSION(kvars), INTENT(IN) :: cdvars
      INTEGER, DIMENSION(imaxavtypes), OPTIONAL :: &
         & kdailyavtypes                ! Types of daily average observations

      !! * Local declarations
      CHARACTER(LEN=15), PARAMETER :: cpname='obs_rea_prof'
      CHARACTER(len=8) :: clrefdate
      CHARACTER(len=8), DIMENSION(:), ALLOCATABLE :: clvarsin
      INTEGER :: jvar
      INTEGER :: ji
      INTEGER :: jj
      INTEGER :: jk
      INTEGER :: ij
      INTEGER :: iflag
      INTEGER :: inobf
      INTEGER :: i_file_id
      INTEGER :: inowin
      INTEGER :: iyea
      INTEGER :: imon
      INTEGER :: iday
      INTEGER :: ihou
      INTEGER :: imin
      INTEGER :: isec
      INTEGER :: iprof
      INTEGER :: iproftot
      INTEGER, DIMENSION(kvars) :: ivart0
      INTEGER, DIMENSION(kvars) :: ivart
      INTEGER :: ip3dt
      INTEGER :: ios
      INTEGER :: ioserrcount
      INTEGER, DIMENSION(kvars) :: ivartmpp
      INTEGER :: ip3dtmpp
      INTEGER :: itype
      INTEGER, DIMENSION(knumfiles) :: &
         & irefdate
      INTEGER, DIMENSION(ntyp1770+1,kvars) :: &
         & itypvar,    &
         & itypvarmpp
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: &
         & iobsi,    &
         & iobsj,    &
         & iproc
      INTEGER, DIMENSION(:), ALLOCATABLE :: &
         & iindx,    &
         & ifileidx, &
         & iprofidx
      INTEGER, DIMENSION(imaxavtypes) :: &
         & idailyavtypes
      INTEGER, DIMENSION(kvars) :: &
         & iv3dt
      REAL(wp), DIMENSION(:), ALLOCATABLE :: &
         & zphi, &
         & zlam
      REAL(dp), DIMENSION(:), ALLOCATABLE :: &
         & zdat
      REAL(dp), DIMENSION(knumfiles) :: &
         & djulini, &
         & djulend
      LOGICAL :: llvalprof
      LOGICAL :: lldavtimset
      LOGICAL :: llcycle
      TYPE(obfbdata), POINTER, DIMENSION(:) :: &
         & inpfiles

      ! Local initialization
      iprof = 0
      ivart0(:) = 0
      ip3dt = 0

      ! Daily average types
      lldavtimset = .FALSE.
      IF ( PRESENT(kdailyavtypes) ) THEN
         idailyavtypes(:) = kdailyavtypes(:)
         IF ( ANY (idailyavtypes(:) /= -1) ) lldavtimset = .TRUE.
      ELSE
         idailyavtypes(:) = -1
      ENDIF

      !-----------------------------------------------------------------------
      ! Count the number of files needed and allocate the obfbdata type
      !-----------------------------------------------------------------------

      inobf = knumfiles

      ALLOCATE( inpfiles(inobf) )

      prof_files : DO jj = 1, inobf

         !---------------------------------------------------------------------
         ! Prints
         !---------------------------------------------------------------------
         IF(lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) ' obs_rea_pro_dri : Reading from file = ', &
               & TRIM( TRIM( cdfilenames(jj) ) )
            WRITE(numout,*) ' ~~~~~~~~~~~~~~~'
            WRITE(numout,*)
         ENDIF

         !---------------------------------------------------------------------
         !  Initialization: Open file and get dimensions only
         !---------------------------------------------------------------------

         iflag = nf90_open( TRIM( cdfilenames(jj) ), nf90_nowrite, &
            &                      i_file_id )

         IF ( iflag /= nf90_noerr ) THEN

            IF ( ldignmis ) THEN
               inpfiles(jj)%nobs = 0
               CALL ctl_warn( 'File ' // TRIM( cdfilenames(jj) ) // &
                  &           ' not found' )
            ELSE 
               CALL ctl_stop( 'File ' // TRIM( cdfilenames(jj) ) // &
                  &           ' not found' )
            ENDIF

         ELSE 

            !------------------------------------------------------------------
            !  Close the file since it is opened in read_obfbdata
            !------------------------------------------------------------------

            iflag = nf90_close( i_file_id )

            !------------------------------------------------------------------
            !  Read the profile file into inpfiles
            !------------------------------------------------------------------
            CALL init_obfbdata( inpfiles(jj) )
            CALL read_obfbdata( TRIM( cdfilenames(jj) ), inpfiles(jj), &
               &                ldgrid = .TRUE. )

            IF ( inpfiles(jj)%nvar /= kvars ) THEN
               CALL ctl_stop( 'Feedback format error: ', &
                  &           ' unexpected number of vars in profile file' )
            ENDIF

            IF ( ldmod .AND. ( inpfiles(jj)%nadd == 0 ) ) THEN
               CALL ctl_stop( 'Model not in input data' )
            ENDIF

            IF ( jj == 1 ) THEN
               ALLOCATE( clvarsin( inpfiles(jj)%nvar ) )
               DO ji = 1, inpfiles(jj)%nvar
                 clvarsin(ji) = inpfiles(jj)%cname(ji)
                 IF ( clvarsin(ji) /= cdvars(ji) ) THEN
                    CALL ctl_stop( 'Feedback file variables do not match', &
                        &           ' expected variable names for this type' )
                 ENDIF
               END DO
            ELSE
               DO ji = 1, inpfiles(jj)%nvar
                  IF ( inpfiles(jj)%cname(ji) /= clvarsin(ji) ) THEN
                     CALL ctl_stop( 'Feedback file variables not consistent', &
                        &           ' with previous files for this type' )
                  ENDIF
               END DO
            ENDIF

            !------------------------------------------------------------------
            !  Change longitude (-180,180)
            !------------------------------------------------------------------

            DO ji = 1, inpfiles(jj)%nobs 

               IF ( inpfiles(jj)%plam(ji) < -180. ) &
                  &   inpfiles(jj)%plam(ji) = inpfiles(jj)%plam(ji) + 360.

               IF ( inpfiles(jj)%plam(ji) >  180. ) &
                  &   inpfiles(jj)%plam(ji) = inpfiles(jj)%plam(ji) - 360.

            END DO

            !------------------------------------------------------------------
            !  Calculate the date  (change eventually)
            !------------------------------------------------------------------
            clrefdate=inpfiles(jj)%cdjuldref(1:8)
            READ(clrefdate,'(I8)') irefdate(jj)

            CALL ddatetoymdhms( ddobsini, iyea, imon, iday, ihou, imin, isec )
            CALL greg2jul( isec, imin, ihou, iday, imon, iyea, djulini(jj), &
               &           krefdate = irefdate(jj) )
            CALL ddatetoymdhms( ddobsend, iyea, imon, iday, ihou, imin, isec )
            CALL greg2jul( isec, imin, ihou, iday, imon, iyea, djulend(jj), &
               &           krefdate = irefdate(jj) )

            ioserrcount=0
            IF ( lldavtimset ) THEN

               IF ( ANY ( idailyavtypes(:) /= -1 ) .AND. lwp) THEN
                  WRITE(numout,*)' Resetting time of daily averaged', &
                     &           ' observations to the end of the day'
               ENDIF

               DO ji = 1, inpfiles(jj)%nobs
                  READ( inpfiles(jj)%cdtyp(ji), '(I4)', IOSTAT = ios, ERR = 900 ) itype
900               IF ( ios /= 0 ) THEN
                     ! Set type to zero if there is a problem in the string conversion
                     itype = 0
                  ENDIF

                  IF ( ANY ( idailyavtypes(:) == itype ) ) THEN
                  !  for daily averaged data force the time
                  !  to be the last time-step of the day, but still within the day.
                     IF ( inpfiles(jj)%ptim(ji) >= 0. ) THEN
                        inpfiles(jj)%ptim(ji) = &
                           & INT(inpfiles(jj)%ptim(ji)) + 0.9999
                     ELSE
                        inpfiles(jj)%ptim(ji) = &
                           & INT(inpfiles(jj)%ptim(ji)) - 0.0001
                     ENDIF
                  ENDIF

               END DO

            ENDIF

            IF ( inpfiles(jj)%nobs > 0 ) THEN
               inpfiles(jj)%iproc(:,:) = -1
               inpfiles(jj)%iobsi(:,:) = -1
               inpfiles(jj)%iobsj(:,:) = -1
            ENDIF
            inowin = 0
            DO ji = 1, inpfiles(jj)%nobs
               IF ( BTEST(inpfiles(jj)%ioqc(ji),2 ) ) CYCLE
               llcycle = .TRUE.
               DO jvar = 1, kvars
                  IF ( .NOT. ( BTEST(inpfiles(jj)%ivqc(ji,jvar),2) ) ) THEN
                     llcycle = .FALSE.
                     EXIT
                  ENDIF
               END DO
               IF ( llcycle ) CYCLE
               IF ( ( inpfiles(jj)%ptim(ji) >  djulini(jj) ) .AND. &
                  & ( inpfiles(jj)%ptim(ji) <= djulend(jj) )       ) THEN
                  inowin = inowin + 1
               ENDIF
            END DO
            ALLOCATE( zlam(inowin)  )
            ALLOCATE( zphi(inowin)  )
            ALLOCATE( iobsi(inowin,kvars) )
            ALLOCATE( iobsj(inowin,kvars) )
            ALLOCATE( iproc(inowin,kvars) )
            inowin = 0
            DO ji = 1, inpfiles(jj)%nobs
               IF ( BTEST(inpfiles(jj)%ioqc(ji),2 ) ) CYCLE
               llcycle = .TRUE.
               DO jvar = 1, kvars
                  IF ( .NOT. ( BTEST(inpfiles(jj)%ivqc(ji,jvar),2) ) ) THEN
                     llcycle = .FALSE.
                     EXIT
                  ENDIF
               END DO
               IF ( llcycle ) CYCLE
               IF ( ( inpfiles(jj)%ptim(ji) >  djulini(jj) ) .AND. &
                  & ( inpfiles(jj)%ptim(ji) <= djulend(jj) )       ) THEN
                  inowin = inowin + 1
                  zlam(inowin) = inpfiles(jj)%plam(ji)
                  zphi(inowin) = inpfiles(jj)%pphi(ji)
               ENDIF
            END DO

            ! Assume anything other than velocity is on T grid
            IF ( TRIM( inpfiles(jj)%cname(1) ) == 'UVEL' ) THEN
               CALL obs_grid_search( inowin, zlam, zphi, iobsi(:,1), iobsj(:,1), &
                  &                  iproc(:,1), 'U' )
               CALL obs_grid_search( inowin, zlam, zphi, iobsi(:,2), iobsj(:,2), &
                  &                  iproc(:,2), 'V' )
            ELSE
               CALL obs_grid_search( inowin, zlam, zphi, iobsi(:,1), iobsj(:,1), &
                  &                  iproc(:,1), 'T' )
               IF ( kvars > 1 ) THEN
                  DO jvar = 2, kvars
                     iobsi(:,jvar) = iobsi(:,1)
                     iobsj(:,jvar) = iobsj(:,1)
                     iproc(:,jvar) = iproc(:,1)
                  END DO
               ENDIF
            ENDIF

            inowin = 0
            DO ji = 1, inpfiles(jj)%nobs
               IF ( BTEST(inpfiles(jj)%ioqc(ji),2 ) ) CYCLE
               llcycle = .TRUE.
               DO jvar = 1, kvars
                  IF ( .NOT. ( BTEST(inpfiles(jj)%ivqc(ji,jvar),2) ) ) THEN
                     llcycle = .FALSE.
                     EXIT
                  ENDIF
               END DO
               IF ( llcycle ) CYCLE
               IF ( ( inpfiles(jj)%ptim(ji) >  djulini(jj) ) .AND. &
                  & ( inpfiles(jj)%ptim(ji) <= djulend(jj) )       ) THEN
                  inowin = inowin + 1
                  DO jvar = 1, kvars
                     inpfiles(jj)%iproc(ji,jvar) = iproc(inowin,jvar)
                     inpfiles(jj)%iobsi(ji,jvar) = iobsi(inowin,jvar)
                     inpfiles(jj)%iobsj(ji,jvar) = iobsj(inowin,jvar)
                  END DO
                  IF ( kvars > 1 ) THEN
                     DO jvar = 2, kvars
                        IF ( inpfiles(jj)%iproc(ji,jvar) /= &
                           & inpfiles(jj)%iproc(ji,1) ) THEN
                           CALL ctl_stop( 'Error in obs_read_prof:', &
                              & 'observation on different processors for different vars')
                        ENDIF
                     END DO
                  ENDIF
               ENDIF
            END DO
            DEALLOCATE( zlam, zphi, iobsi, iobsj, iproc )

            DO ji = 1, inpfiles(jj)%nobs
               IF ( BTEST(inpfiles(jj)%ioqc(ji),2 ) ) CYCLE
               llcycle = .TRUE.
               DO jvar = 1, kvars
                  IF ( .NOT. ( BTEST(inpfiles(jj)%ivqc(ji,jvar),2) ) ) THEN
                     llcycle = .FALSE.
                     EXIT
                  ENDIF
               END DO
               IF ( llcycle ) CYCLE
               IF ( ( inpfiles(jj)%ptim(ji) >  djulini(jj) ) .AND. &
                  & ( inpfiles(jj)%ptim(ji) <= djulend(jj) )       ) THEN
                  IF ( narea == 1 ) THEN
                     IF ( inpfiles(jj)%iproc(ji,1) >  narea-1 ) CYCLE
                  ELSE
                     IF ( inpfiles(jj)%iproc(ji,1) /= narea-1 ) CYCLE
                  ENDIF
                  llvalprof = .FALSE.
                  DO jvar = 1, kvars
                     IF ( ldvar(jvar) ) THEN
                        DO ij = 1,inpfiles(jj)%nlev
                           IF ( inpfiles(jj)%pdep(ij,ji) >= 6000. ) &
                              & CYCLE
                           IF ( .NOT. BTEST(inpfiles(jj)%ivlqc(ij,ji,jvar),2) .AND. &
                              & .NOT. BTEST(inpfiles(jj)%idqc(ij,ji),2) ) THEN
                              ivart0(jvar) = ivart0(jvar) + 1
                           ENDIF
                        END DO
                     ENDIF
                  END DO
                  DO ij = 1,inpfiles(jj)%nlev
                     IF ( inpfiles(jj)%pdep(ij,ji) >= 6000. ) &
                        & CYCLE
                     DO jvar = 1, kvars
                        IF ( ( .NOT. BTEST(inpfiles(jj)%ivlqc(ij,ji,jvar),2) .AND. &
                           &   .NOT. BTEST(inpfiles(jj)%idqc(ij,ji),2) .AND. &
                           &    ldvar(jvar) ) ) THEN
                           ip3dt = ip3dt + 1
                           llvalprof = .TRUE.
                           EXIT
                        ENDIF
                     END DO
                  END DO

                  IF ( llvalprof ) iprof = iprof + 1

               ENDIF
            END DO

         ENDIF

      END DO prof_files

      !-----------------------------------------------------------------------
      ! Get the time ordered indices of the input data
      !-----------------------------------------------------------------------

      !---------------------------------------------------------------------
      !  Loop over input data files to count total number of profiles
      !---------------------------------------------------------------------
      iproftot = 0
      DO jj = 1, inobf
         DO ji = 1, inpfiles(jj)%nobs
            IF ( BTEST(inpfiles(jj)%ioqc(ji),2 ) ) CYCLE
            llcycle = .TRUE.
            DO jvar = 1, kvars
               IF ( .NOT. ( BTEST(inpfiles(jj)%ivqc(ji,jvar),2) ) ) THEN
                  llcycle = .FALSE.
                  EXIT
               ENDIF
            END DO
            IF ( llcycle ) CYCLE
            IF ( ( inpfiles(jj)%ptim(ji) >  djulini(jj) ) .AND. &
               & ( inpfiles(jj)%ptim(ji) <= djulend(jj) )       ) THEN
               iproftot = iproftot + 1
            ENDIF
         END DO
      END DO

      ALLOCATE( iindx(iproftot), ifileidx(iproftot), &
         &      iprofidx(iproftot), zdat(iproftot) )
      jk = 0
      DO jj = 1, inobf
         DO ji = 1, inpfiles(jj)%nobs
            IF ( BTEST(inpfiles(jj)%ioqc(ji),2 ) ) CYCLE
            llcycle = .TRUE.
            DO jvar = 1, kvars
               IF ( .NOT. ( BTEST(inpfiles(jj)%ivqc(ji,jvar),2) ) ) THEN
                  llcycle = .FALSE.
                  EXIT
               ENDIF
            END DO
            IF ( llcycle ) CYCLE
            IF ( ( inpfiles(jj)%ptim(ji) >  djulini(jj) ) .AND. &
               & ( inpfiles(jj)%ptim(ji) <= djulend(jj) )       ) THEN
               jk = jk + 1
               ifileidx(jk) = jj
               iprofidx(jk) = ji
               zdat(jk)     = inpfiles(jj)%ptim(ji)
            ENDIF
         END DO
      END DO
      CALL sort_dp_indx( iproftot, &
         &               zdat,     &
         &               iindx   )

      iv3dt(:) = -1
      IF (ldsatt) THEN
         iv3dt(:) = ip3dt
      ELSE
         iv3dt(:) = ivart0(:)
      ENDIF
      CALL obs_prof_alloc( profdata, kvars, kextr, iprof, iv3dt, &
         &                 kstp, jpi, jpj, jpk )

      ! * Read obs/positions, QC, all variable and assign to profdata

      profdata%nprof     = 0
      profdata%nvprot(:) = 0
      profdata%cvars(:)  = clvarsin(:)
      iprof = 0

      ip3dt = 0
      ivart(:) = 0
      itypvar   (:,:) = 0
      itypvarmpp(:,:) = 0

      ioserrcount = 0
      DO jk = 1, iproftot

         jj = ifileidx(iindx(jk))
         ji = iprofidx(iindx(jk))

         IF ( BTEST(inpfiles(jj)%ioqc(ji),2 ) ) CYCLE
         llcycle = .TRUE.
         DO jvar = 1, kvars
            IF ( .NOT. ( BTEST(inpfiles(jj)%ivqc(ji,jvar),2) ) ) THEN
               llcycle = .FALSE.
               EXIT
            ENDIF
         END DO
         IF ( llcycle ) CYCLE

         IF ( ( inpfiles(jj)%ptim(ji) >  djulini(jj) ) .AND.  &
            & ( inpfiles(jj)%ptim(ji) <= djulend(jj) ) ) THEN

            IF ( narea == 1 ) THEN
               IF ( inpfiles(jj)%iproc(ji,1) >  narea-1 ) CYCLE
            ELSE
               IF ( inpfiles(jj)%iproc(ji,1) /= narea-1 ) CYCLE
            ENDIF

            llvalprof = .FALSE.

            IF ( inpfiles(jj)%ioqc(ji) > 2 ) CYCLE

            IF ( BTEST(inpfiles(jj)%ioqc(ji),2 ) ) CYCLE
            llcycle = .TRUE.
            DO jvar = 1, kvars
               IF ( .NOT. ( BTEST(inpfiles(jj)%ivqc(ji,jvar),2) ) ) THEN
                  llcycle = .FALSE.
                  EXIT
               ENDIF
            END DO
            IF ( llcycle ) CYCLE

            loop_prof : DO ij = 1, inpfiles(jj)%nlev

               IF ( inpfiles(jj)%pdep(ij,ji) >= 6000. ) &
                  & CYCLE

               DO jvar = 1, kvars
                  IF ( .NOT. BTEST(inpfiles(jj)%ivlqc(ij,ji,jvar),2) .AND. &
                     & .NOT. BTEST(inpfiles(jj)%idqc(ij,ji),2) ) THEN

                     llvalprof = .TRUE. 
                     EXIT loop_prof

                  ENDIF
               END DO

            END DO loop_prof

            ! Set profile information

            IF ( llvalprof ) THEN

               iprof = iprof + 1

               CALL jul2greg( isec,                   &
                  &           imin,                   &
                  &           ihou,                   &
                  &           iday,                   &
                  &           imon,                   &
                  &           iyea,                   &
                  &           inpfiles(jj)%ptim(ji), &
                  &           irefdate(jj) )


               ! Profile time coordinates
               profdata%nyea(iprof) = iyea
               profdata%nmon(iprof) = imon
               profdata%nday(iprof) = iday
               profdata%nhou(iprof) = ihou
               profdata%nmin(iprof) = imin

               ! Profile space coordinates
               profdata%rlam(iprof) = inpfiles(jj)%plam(ji)
               profdata%rphi(iprof) = inpfiles(jj)%pphi(ji)

               ! Coordinate search parameters
               DO jvar = 1, kvars
                  profdata%mi  (iprof,jvar) = inpfiles(jj)%iobsi(ji,jvar)
                  profdata%mj  (iprof,jvar) = inpfiles(jj)%iobsj(ji,jvar)
               END DO

               ! Profile WMO number
               profdata%cwmo(iprof) = inpfiles(jj)%cdwmo(ji)

               ! Instrument type
               READ( inpfiles(jj)%cdtyp(ji), '(I4)', IOSTAT = ios, ERR = 901 ) itype
901            IF ( ios /= 0 ) THEN
                  IF (ioserrcount == 0) CALL ctl_warn ( 'Problem converting an instrument type to integer. Setting type to zero' )
                  ioserrcount = ioserrcount + 1
                  itype = 0
               ENDIF

               profdata%ntyp(iprof) = itype

               ! QC stuff

               profdata%nqc(iprof)     = inpfiles(jj)%ioqc(ji)
               profdata%nqcf(:,iprof)  = inpfiles(jj)%ioqcf(:,ji)
               profdata%ipqc(iprof)    = inpfiles(jj)%ipqc(ji)
               profdata%ipqcf(:,iprof) = inpfiles(jj)%ipqcf(:,ji)
               profdata%itqc(iprof)    = inpfiles(jj)%itqc(ji)
               profdata%itqcf(:,iprof) = inpfiles(jj)%itqcf(:,ji)
               profdata%ivqc(iprof,:)  = inpfiles(jj)%ivqc(ji,:)
               profdata%ivqcf(:,iprof,:) = inpfiles(jj)%ivqcf(:,ji,:)

               ! Bookkeeping data to match profiles
               profdata%npidx(iprof) = iprof
               profdata%npfil(iprof) = iindx(jk)

               ! Observation QC flag (whole profile)
               profdata%nqc(iprof)  = 0 !TODO

               loop_p : DO ij = 1, inpfiles(jj)%nlev

                  IF ( inpfiles(jj)%pdep(ij,ji) >= 6000. ) &
                     & CYCLE

                  IF (ldsatt) THEN

                     DO jvar = 1, kvars
                        IF ( ( .NOT. BTEST(inpfiles(jj)%ivlqc(ij,ji,jvar),2) .AND. &
                           &   .NOT. BTEST(inpfiles(jj)%idqc(ij,ji),2) .AND. &
                           &    ldvar(jvar) ) ) THEN
                           ip3dt = ip3dt + 1
                           EXIT
                        ELSE IF ( jvar == kvars ) THEN
                           CYCLE loop_p
                        ENDIF
                     END DO

                  ENDIF

                  DO jvar = 1, kvars
                  
                     IF ( ( .NOT. BTEST(inpfiles(jj)%ivlqc(ij,ji,jvar),2) .AND. &
                       &   .NOT. BTEST(inpfiles(jj)%idqc(ij,ji),2) .AND. &
                       &    ldvar(jvar) ) .OR. ldsatt ) THEN

                        IF (ldsatt) THEN

                           ivart(jvar) = ip3dt

                        ELSE

                           ivart(jvar) = ivart(jvar) + 1

                        ENDIF

                        ! Depth of jvar observation
                        profdata%var(jvar)%vdep(ivart(jvar)) = &
                           &                inpfiles(jj)%pdep(ij,ji)

                        ! Depth of jvar observation QC
                        profdata%var(jvar)%idqc(ivart(jvar)) = &
                           &                inpfiles(jj)%idqc(ij,ji)

                        ! Depth of jvar observation QC flags
                        profdata%var(jvar)%idqcf(:,ivart(jvar)) = &
                           &                inpfiles(jj)%idqcf(:,ij,ji)

                        ! Profile index
                        profdata%var(jvar)%nvpidx(ivart(jvar)) = iprof

                        ! Vertical index in original profile
                        profdata%var(jvar)%nvlidx(ivart(jvar)) = ij

                        ! Profile jvar value
                        IF ( .NOT. BTEST(inpfiles(jj)%ivlqc(ij,ji,jvar),2) .AND. &
                           & .NOT. BTEST(inpfiles(jj)%idqc(ij,ji),2) ) THEN
                           profdata%var(jvar)%vobs(ivart(jvar)) = &
                              &                inpfiles(jj)%pob(ij,ji,jvar)
                           IF ( ldmod ) THEN
                              profdata%var(jvar)%vmod(ivart(jvar)) = &
                                 &                inpfiles(jj)%padd(ij,ji,1,jvar)
                           ENDIF
                           ! Count number of profile var1 data as function of type
                           itypvar( profdata%ntyp(iprof) + 1, jvar ) = &
                              & itypvar( profdata%ntyp(iprof) + 1, jvar ) + 1
                        ELSE
                           profdata%var(jvar)%vobs(ivart(jvar)) = fbrmdi
                        ENDIF

                        ! Profile jvar qc
                        profdata%var(jvar)%nvqc(ivart(jvar)) = &
                           & inpfiles(jj)%ivlqc(ij,ji,jvar)

                        ! Profile jvar qc flags
                        profdata%var(jvar)%nvqcf(:,ivart(jvar)) = &
                           & inpfiles(jj)%ivlqcf(:,ij,ji,jvar)

                        ! Profile insitu T value
                        IF ( TRIM( inpfiles(jj)%cname(jvar) ) == 'POTM' ) THEN
                           profdata%var(jvar)%vext(ivart(jvar),1) = &
                              &                inpfiles(jj)%pext(ij,ji,1)
                        ENDIF

                     ENDIF
                  
                  END DO

               END DO loop_p

            ENDIF

         ENDIF

      END DO

      !-----------------------------------------------------------------------
      ! Sum up over processors
      !-----------------------------------------------------------------------

      DO jvar = 1, kvars
         CALL obs_mpp_sum_integer ( ivart0(jvar), ivartmpp(jvar) )
      END DO
      CALL obs_mpp_sum_integer ( ip3dt,   ip3dtmpp  )

      DO jvar = 1, kvars
         CALL obs_mpp_sum_integers( itypvar(:,jvar), itypvarmpp(:,jvar), ntyp1770 + 1 )
      END DO

      !-----------------------------------------------------------------------
      ! Output number of observations.
      !-----------------------------------------------------------------------
      IF(lwp) THEN
         WRITE(numout,*) 
         WRITE(numout,'(A)') ' Profile data'
         WRITE(numout,'(1X,A)') '------------'
         WRITE(numout,*) 
         DO jvar = 1, kvars
            WRITE(numout,'(1X,A)') 'Profile data, '//TRIM( profdata%cvars(jvar) )
            WRITE(numout,'(1X,A)') '------------------------'
            DO ji = 0, ntyp1770
               IF ( itypvarmpp(ji+1,jvar) > 0 ) THEN
                  WRITE(numout,'(1X,A3,1X,A48,A3,I8)') ctypshort(ji), &
                     & cwmonam1770(ji)(1:52),' = ', &
                     & itypvarmpp(ji+1,jvar)
               ENDIF
            END DO
            WRITE(numout,'(1X,A)') &
               & '---------------------------------------------------------------'
            WRITE(numout,'(1X,A55,I8)') &
               & 'Total profile data for variable '//TRIM( profdata%cvars(jvar) )// &
               & '             = ', ivartmpp(jvar)
            WRITE(numout,'(1X,A)') &
               & '---------------------------------------------------------------'
            WRITE(numout,*) 
         END DO
      ENDIF

      IF (ldsatt) THEN
         profdata%nvprot(:)    = ip3dt
         profdata%nvprotmpp(:) = ip3dtmpp
      ELSE
         DO jvar = 1, kvars
            profdata%nvprot(jvar)    = ivart(jvar)
            profdata%nvprotmpp(jvar) = ivartmpp(jvar)
         END DO
      ENDIF
      profdata%nprof        = iprof

      !-----------------------------------------------------------------------
      ! Model level search
      !-----------------------------------------------------------------------
      DO jvar = 1, kvars
         IF ( ldvar(jvar) ) THEN
            CALL obs_level_search( jpk, gdept_1d, &
               & profdata%nvprot(jvar), profdata%var(jvar)%vdep, &
               & profdata%var(jvar)%mvk )
         ENDIF
      END DO

      !-----------------------------------------------------------------------
      ! Set model equivalent to 99999
      !-----------------------------------------------------------------------
      IF ( .NOT. ldmod ) THEN
         DO jvar = 1, kvars
            profdata%var(jvar)%vmod(:) = fbrmdi
         END DO
      ENDIF
      !-----------------------------------------------------------------------
      ! Deallocate temporary data
      !-----------------------------------------------------------------------
      DEALLOCATE( ifileidx, iprofidx, zdat, clvarsin )

      !-----------------------------------------------------------------------
      ! Deallocate input data
      !-----------------------------------------------------------------------
      DO jj = 1, inobf
         CALL dealloc_obfbdata( inpfiles(jj) )
      END DO
      DEALLOCATE( inpfiles )

   END SUBROUTINE obs_rea_prof

END MODULE obs_read_prof
