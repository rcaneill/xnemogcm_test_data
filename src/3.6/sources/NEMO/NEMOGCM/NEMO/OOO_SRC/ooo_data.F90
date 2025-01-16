MODULE ooo_data
   !! =================================================================
   !!                    *** MODULE ooo_data ***
   !! =================================================================
   USE par_kind, ONLY: lc

   IMPLICIT NONE

   !! Public data

   INTEGER, PARAMETER :: MaxNumFiles = 1000

   !! Class 4 file settings
   INTEGER :: &
           & cl4_fcst_idx(MaxNumFiles), & !: forecast indices
           & cl4_match_len, &             !: number of match types
           & cl4_fcst_len                 !: number of forecast days
   CHARACTER(len=lc) :: &
           & cl4_vars(MaxNumFiles), &  !: class 4 variables
           & cl4_sys, &                !: class 4 system
           & cl4_cfg, &                !: class 4 configuration
           & cl4_date, &               !: class 4 date
           & cl4_vn,  &                !: class 4 version
           & cl4_prefix, &             !: class 4 prefix
           & cl4_contact, &            !: class 4 contact
           & cl4_inst                  !: class 4 institute
   REAL ::   cl4_modjuld               !: model Julian day
   REAL :: &
      & cl4_leadtime(MaxNumFiles)      !: Lead time data

   !! Offline obs_oper settings
   CHARACTER(len=lc) :: &
      & ooo_files(MaxNumFiles)         !: model files
   INTEGER            :: &
      & jifile, &                      !: current file list index
      & n_files, &                     !: number of files
      & jimatch, &                     !: current match
      & nn_ooo_idx(MaxNumFiles), &     !: time_counter indices
      & nn_ooo_freq                    !: read frequency in time steps
   CHARACTER(len=128) :: &
      & alt_file                       !: altimeter file
   !! $Id: ooo_data.F90 5215 2015-04-15 16:11:56Z nicolasmartin $
CONTAINS
   SUBROUTINE ooo_data_init( ld_cl4 )
      !!----------------------------------------------------------------------
      !!                    ***  SUBROUTINE ooo_data_init ***
      !!
      !! ** Purpose : To read namelists and initialise offline_oper run.
      !!
      !!----------------------------------------------------------------------
      USE in_out_manager
      INTEGER            :: &
         & jf                           !: file dummy loop index
      LOGICAL :: lmask(MaxNumFiles)     !: Logical mask used for counting
      LOGICAL, INTENT(IN) :: ld_cl4     !: Logical class 4 on/off

      ! Standard offline obs_oper information
      NAMELIST/namooo/ooo_files, nn_ooo_idx, nn_ooo_freq

      ! Class 4 file specifiers
      NAMELIST/namcl4/cl4_vars, cl4_sys, cl4_cfg, cl4_date, cl4_vn, &
         &            cl4_prefix, cl4_contact, cl4_inst, cl4_leadtime, &
         &            cl4_fcst_idx, cl4_fcst_len, cl4_match_len

      ! Standard offline obs_oper initialisation
      jimatch = 0                   !: match-up iteration variable 
      jifile = 1                    !: input file iteration variable 
      n_files = 0                   !: number of files to cycle through
      ooo_files(:) = ''             !: list of files to read in
      nn_ooo_idx(:) = 0             !: list of indices inside each file
      nn_ooo_freq = -1              !: input frequency in time steps

      ! Class 4 initialisation
      cl4_leadtime(:) = 0           !: Lead time axis value for each file
      cl4_fcst_len = 0              !: Length of the forecast dimension
      cl4_match_len = 1             !: Number of match types
      cl4_fcst_idx(:) = 0           !: output file forecast index
      cl4_vars(:) = ''              !: output file variable names
      cl4_sys = ''                  !: output file system
      cl4_cfg = ''                  !: output file configuration
      cl4_date = ''                 !: output file date string
      cl4_vn = ''                   !: output file version
      cl4_prefix = 'class4'         !: output file prefix
      cl4_contact = ''              !: output file contact details
      cl4_inst = ''                 !: output file institution

      ! Standard offline obs_oper settings
      READ(numnam, namooo)

      ! Read class 4 output settings
      IF (ld_cl4) THEN
         READ(numnam, namcl4)
      ENDIF

      ! count input files
      lmask(:) = .FALSE.
      WHERE (ooo_files(:) /= '') lmask(:) = .TRUE.
      n_files = COUNT(lmask)

      !! Initialise sub obs window frequency
      IF (nn_ooo_freq == -1) THEN
         !! Run length
         nn_ooo_freq = nitend - nit000 + 1
      ENDIF

      !! Print summary of settings
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'offline obs_oper : Initialization'
         WRITE(numout,*) '~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namooo : set offline obs_oper parameters' 
         DO jf = 1, n_files
            WRITE(numout,'(1X,2A)') '   Input forecast file name          forecastfile = ', &
               TRIM(ooo_files(jf))
            WRITE(numout,*) '   Input forecast file index        forecastindex = ', &
               nn_ooo_idx(jf)
            WRITE(numout,*) '   Output forecast leadtime index   leadtimeindex = ', &
               cl4_fcst_idx(jf)
            WRITE(numout,*) '   Output forecast leadtime value   leadtimevalue = ', &
               cl4_leadtime(jf)
            WRITE(numout,'(1X,2A)') '   Input class 4 variable       class 4 parameter = ', &
               TRIM(cl4_vars(jf))
         END DO
         WRITE(numout, '(1X,2A)') '   Input class 4 system            class 4 system = ', &
            TRIM(cl4_sys)
         WRITE(numout, '(1X,2A)') '   Input class 4 config            class 4 config = ', &
            TRIM(cl4_cfg)
         WRITE(numout, '(1X,2A)') '   Input class 4 date                class 4 date = ', &
            TRIM(cl4_date)
         WRITE(numout, '(1X,2A)') '   Input class 4 version          class 4 version = ', &
            TRIM(cl4_vn)
         WRITE(numout, '(1X,2A)') '   Input class 4 prefix            class 4 prefix = ', &
            TRIM(cl4_prefix)
         WRITE(numout, '(1X,2A)') '   Input class 4 contact          class 4 contact = ', &
            TRIM(cl4_contact)
         WRITE(numout, '(1X,2A)') '   Input class 4 institute      class 4 institute = ', &
            TRIM(cl4_inst)
      END IF

   END SUBROUTINE ooo_data_init

END MODULE ooo_data

