MODULE sao_read
   !!======================================================================
   !!                      ***  MODULE sao_read  ***
   !! Read routines : I/O for Stand Alone Observation operator
   !!======================================================================
   USE mppini
   USE lib_mpp
   USE in_out_manager
   USE par_kind, ONLY: lc
   USE netcdf
   USE oce,     ONLY: tsn, sshn
   USE dom_oce, ONLY: nimpp, njmpp, tmask
   USE par_oce, ONLY: jpi, jpj, jpk
   !
   USE obs_fbm, ONLY: fbimdi, fbrmdi, fbsp, fbdp
   USE sao_data

   IMPLICIT NONE
   PRIVATE

   PUBLIC sao_rea_dri

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: sao_read.F90 13286 2020-07-09 15:48:29Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE sao_rea_dri( kfile )
      !!------------------------------------------------------------------------
      !!             *** sao_rea_dri ***
      !!
      !! Purpose : To choose appropriate read method
      !! Method  :
      !!
      !! Author  : A. Ryan Oct 2013
      !!
      !!------------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kfile         ! File number
      !
      CHARACTER(len=lc)   ::   cdfilename    ! File name
      INTEGER ::   kindex        ! File index to read
      !!------------------------------------------------------------------------
      !
      cdfilename = TRIM( sao_files(kfile) )
      kindex = nn_sao_idx(kfile)
      CALL sao_read_file( TRIM( cdfilename ), kindex )
      !
   END SUBROUTINE sao_rea_dri


   SUBROUTINE sao_read_file( filename, ifcst )
      !!------------------------------------------------------------------------
      !!                         ***  sao_read_file  ***
      !!
      !! Purpose : To fill tn and sn with dailymean field from netcdf files
      !! Method  : Use subdomain indices to create start and count matrices
      !!           for netcdf read.
      !!
      !! Author  : A. Ryan Oct 2010
      !!------------------------------------------------------------------------
      INTEGER,          INTENT(in) ::   ifcst
      CHARACTER(len=*), INTENT(in) ::   filename
      INTEGER                      ::   ncid, varid, istat, ntimes
      INTEGER                      ::   tdim, xdim, ydim, zdim
      INTEGER                      ::   ii, ij, ik
      INTEGER, DIMENSION(4)        ::   start_n, count_n
      INTEGER, DIMENSION(3)        ::   start_s, count_s
      REAL(fbdp)                   ::   fill_val
      REAL(fbdp), DIMENSION(:,:,:), ALLOCATABLE ::   temp_tn, temp_sn
      REAL(fbdp), DIMENSION(:,:)  , ALLOCATABLE ::   temp_sshn

      ! DEBUG
      INTEGER ::   istage
      !!------------------------------------------------------------------------

      IF (TRIM(filename) == 'nofile') THEN
         tsn (:,:,:,:) = fbrmdi
         sshn(:,:)     = fbrmdi
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
            ALLOCATE(temp_tn(jpi,jpj,jpk))
            ALLOCATE(temp_sn(jpi,jpj,jpk))
            ALLOCATE(temp_sshn(jpi,jpj))

            ! Set temp_tn, temp_sn to 0.
            temp_tn(:,:,:) = fbrmdi
            temp_sn(:,:,:) = fbrmdi
            temp_sshn(:,:) = fbrmdi

            ! Create start and count arrays
            start_n = (/ nimpp, njmpp,      1, ifcst /)
            count_n = (/   jpi,   jpj, jpk, 1        /)
            start_s = (/ nimpp, njmpp        , ifcst /)
            count_s = (/   jpi,   jpj,      1        /)

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
            tsn(1:jpi,1:jpj,1:jpk,1) = temp_tn(:,:,:) * tmask(1:jpi,1:jpj,1:jpk)
            tsn(1:jpi,1:jpj,1:jpk,2) = temp_sn(:,:,:) * tmask(1:jpi,1:jpj,1:jpk)
            sshn(1:jpi,1:jpj)        = temp_sshn(:,:) * tmask(1:jpi,1:jpj,1)
            
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
      !
   END SUBROUTINE sao_read_file
   
   !!------------------------------------------------------------------------
END MODULE sao_read
