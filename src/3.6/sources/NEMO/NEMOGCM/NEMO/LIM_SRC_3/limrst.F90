MODULE limrst
   !!======================================================================
   !!                     ***  MODULE  limrst  ***
   !! Ice restart :  write the ice restart file
   !!======================================================================
   !! History:   -   ! 2005-04 (M. Vancoppenolle) Original code
   !!           3.0  ! 2008-03 (C. Ethe) restart files in using IOM interface
   !!           4.0  ! 2011-02 (G. Madec) dynamical allocation
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3' :                                   LIM sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_rst_opn   : open ice restart file
   !!   lim_rst_write : write of the restart file 
   !!   lim_rst_read  : read  the restart file 
   !!----------------------------------------------------------------------
   USE ice            ! sea-ice variables
   USE oce     , ONLY :  snwice_mass, snwice_mass_b
   USE dom_oce        ! ocean domain
   USE sbc_oce        ! Surface boundary condition: ocean fields
   USE sbc_ice        ! Surface boundary condition: ice fields
   USE in_out_manager ! I/O manager
   USE iom            ! I/O library
   USE lib_mpp        ! MPP library
   USE wrk_nemo       ! work arrays
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)  
   USE limctl

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_rst_opn    ! routine called by icestep.F90
   PUBLIC   lim_rst_write  ! routine called by icestep.F90
   PUBLIC   lim_rst_read   ! routine called by sbc_lim_init

   LOGICAL, PUBLIC ::   lrst_ice         !: logical to control the ice restart write 
   INTEGER, PUBLIC ::   numrir, numriw   !: logical unit for ice restart (read and write)

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: limrst.F90 7814 2017-03-20 16:21:42Z clem $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_rst_opn( kt )
      !!----------------------------------------------------------------------
      !!                    ***  lim_rst_opn  ***
      !!
      !! ** purpose  :   output of sea-ice variable in a netcdf file
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt       ! number of iteration
      !
      CHARACTER(LEN=20)   ::   clkt     ! ocean time-step define as a character
      CHARACTER(LEN=50)   ::   clname   ! ice output restart file name
      CHARACTER(len=256)  ::   clpath   ! full path to ice output restart file 
      !!----------------------------------------------------------------------
      !
      IF( kt == nit000 )   lrst_ice = .FALSE.   ! default definition

      ! in order to get better performances with NetCDF format, we open and define the ice restart file 
      ! one ice time step before writing the data (-> at nitrst - 2*nn_fsbc + 1), except if we write ice 
      ! restart files every ice time step or if an ice restart file was writen at nitend - 2*nn_fsbc + 1
      IF( kt == nitrst - 2*nn_fsbc + 1 .OR. nstock == nn_fsbc    &
         &                             .OR. ( kt == nitend - nn_fsbc + 1 .AND. .NOT. lrst_ice ) ) THEN
         IF( nitrst <= nitend .AND. nitrst > 0 ) THEN
            ! beware of the format used to write kt (default is i8.8, that should be large enough...)
            IF( nitrst > 99999999 ) THEN   ;   WRITE(clkt, *       ) nitrst
            ELSE                           ;   WRITE(clkt, '(i8.8)') nitrst
            ENDIF
            ! create the file
            clname = TRIM(cexper)//"_"//TRIM(ADJUSTL(clkt))//"_"//TRIM(cn_icerst_out)
            clpath = TRIM(cn_icerst_outdir) 
            IF( clpath(LEN_TRIM(clpath):) /= '/' ) clpath = TRIM(clpath)//'/'
            IF(lwp) THEN
               WRITE(numout,*)
               SELECT CASE ( jprstlib )
               CASE ( jprstdimg )
                  WRITE(numout,*) '             open ice restart binary file: ',TRIM(clpath)//clname
               CASE DEFAULT
                  WRITE(numout,*) '             open ice restart NetCDF file: ',TRIM(clpath)//clname
               END SELECT
               IF( kt == nitrst - 2*nn_fsbc + 1 ) THEN   
                  WRITE(numout,*)         '             kt = nitrst - 2*nn_fsbc + 1 = ', kt,' date= ', ndastp
               ELSE   ;   WRITE(numout,*) '             kt = '                         , kt,' date= ', ndastp
               ENDIF
            ENDIF
            !
            CALL iom_open( TRIM(clpath)//TRIM(clname), numriw, ldwrt = .TRUE., kiolib = jprstlib )
            lrst_ice = .TRUE.
         ENDIF
      ENDIF
      !
      IF( ln_icectl )   CALL lim_prt( kt, iiceprt, jiceprt, 1, ' - Beginning the time step - ' )   ! control print
   END SUBROUTINE lim_rst_opn


   SUBROUTINE lim_rst_write( kt )
      !!----------------------------------------------------------------------
      !!                    ***  lim_rst_write  ***
      !!
      !! ** purpose  :   output of sea-ice variable in a netcdf file
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt     ! number of iteration
      !!
      INTEGER ::   ji, jj, jk ,jl   ! dummy loop indices
      INTEGER ::   iter
      CHARACTER(len=15) ::   znam
      CHARACTER(len=2)  ::   zchar, zchar1
      REAL(wp), POINTER, DIMENSION(:,:) :: z2d
      !!----------------------------------------------------------------------

      CALL wrk_alloc( jpi, jpj, z2d )

      iter = kt + nn_fsbc - 1   ! ice restarts are written at kt == nitrst - nn_fsbc + 1

      IF( iter == nitrst ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'lim_rst_write : write ice restart file  kt =', kt
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~'         
      ENDIF

      ! Write in numriw (if iter == nitrst)
      ! ------------------ 
      !                                                                        ! calendar control
      CALL iom_rstput( iter, nitrst, numriw, 'nn_fsbc', REAL( nn_fsbc, wp ) )      ! time-step 
      CALL iom_rstput( iter, nitrst, numriw, 'kt_ice' , REAL( iter   , wp ) )      ! date

      ! Prognostic variables 
      DO jl = 1, jpl 
         WRITE(zchar,'(I2)') jl
         znam = 'v_i'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = v_i(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'v_s'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = v_s(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'smv_i'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = smv_i(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'oa_i'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = oa_i(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'a_i'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = a_i(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 't_su'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = t_su(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'tempt_sl1'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = e_s(:,:,1,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         DO jk = 1, nlay_i 
            WRITE(zchar1,'(I2)') jk
            znam = 'tempt'//'_il'//TRIM(ADJUSTL(zchar1))//'_htc'//TRIM(ADJUSTL(zchar))
            z2d(:,:) = e_i(:,:,jk,jl)
            CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         END DO
      END DO

      CALL iom_rstput( iter, nitrst, numriw, 'u_ice'        , u_ice      )
      CALL iom_rstput( iter, nitrst, numriw, 'v_ice'        , v_ice      )
      CALL iom_rstput( iter, nitrst, numriw, 'stress1_i'    , stress1_i  )
      CALL iom_rstput( iter, nitrst, numriw, 'stress2_i'    , stress2_i  )
      CALL iom_rstput( iter, nitrst, numriw, 'stress12_i'   , stress12_i )
      CALL iom_rstput( iter, nitrst, numriw, 'snwice_mass'  , snwice_mass )
      CALL iom_rstput( iter, nitrst, numriw, 'snwice_mass_b', snwice_mass_b )

      DO jl = 1, jpl 
         WRITE(zchar,'(I2)') jl
         znam = 'sxice'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = sxice(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'syice'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = syice(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxxice'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = sxxice(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'syyice'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = syyice(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxyice'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = sxyice(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxsn'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = sxsn(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sysn'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = sysn(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxxsn'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = sxxsn(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'syysn'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = syysn(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxysn'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = sxysn(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxa'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = sxa(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sya'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = sya(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxxa'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = sxxa(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'syya'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = syya(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxya'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = sxya(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxc0'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = sxc0(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'syc0'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = syc0(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxxc0'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = sxxc0(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'syyc0'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = syyc0(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxyc0'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = sxyc0(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxsal'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = sxsal(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sysal'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = sysal(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxxsal'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = sxxsal(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'syysal'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = syysal(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxysal'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = sxysal(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxage'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = sxage(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'syage'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = syage(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxxage'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = sxxage(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'syyage'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = syyage(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxyage'//'_htc'//TRIM(ADJUSTL(zchar))
         z2d(:,:) = sxyage(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
      END DO

      CALL iom_rstput( iter, nitrst, numriw, 'sxopw ' ,  sxopw  )
      CALL iom_rstput( iter, nitrst, numriw, 'syopw ' ,  syopw  )
      CALL iom_rstput( iter, nitrst, numriw, 'sxxopw' ,  sxxopw )
      CALL iom_rstput( iter, nitrst, numriw, 'syyopw' ,  syyopw )
      CALL iom_rstput( iter, nitrst, numriw, 'sxyopw' ,  sxyopw )

      DO jl = 1, jpl 
         WRITE(zchar,'(I2)') jl
         DO jk = 1, nlay_i 
            WRITE(zchar1,'(I2)') jk
            znam = 'sxe'//'_il'//TRIM(ADJUSTL(zchar1))//'_htc'//TRIM(ADJUSTL(zchar))
            z2d(:,:) = sxe(:,:,jk,jl)
            CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
            znam = 'sye'//'_il'//TRIM(ADJUSTL(zchar1))//'_htc'//TRIM(ADJUSTL(zchar))
            z2d(:,:) = sye(:,:,jk,jl)
            CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
            znam = 'sxxe'//'_il'//TRIM(ADJUSTL(zchar1))//'_htc'//TRIM(ADJUSTL(zchar))
            z2d(:,:) = sxxe(:,:,jk,jl)
            CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
            znam = 'syye'//'_il'//TRIM(ADJUSTL(zchar1))//'_htc'//TRIM(ADJUSTL(zchar))
            z2d(:,:) = syye(:,:,jk,jl)
            CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
            znam = 'sxye'//'_il'//TRIM(ADJUSTL(zchar1))//'_htc'//TRIM(ADJUSTL(zchar))
            z2d(:,:) = sxye(:,:,jk,jl)
            CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         END DO
      END DO

      IF( iter == nitrst ) THEN
         CALL iom_close( numriw )                         ! close the restart file
         lrst_ice = .FALSE.
      ENDIF
      !
      CALL wrk_dealloc( jpi, jpj, z2d )
      !
   END SUBROUTINE lim_rst_write


   SUBROUTINE lim_rst_read
      !!----------------------------------------------------------------------
      !!                    ***  lim_rst_read  ***
      !!
      !! ** purpose  :   read of sea-ice variable restart in a netcdf file
      !!----------------------------------------------------------------------
      INTEGER :: ji, jj, jk, jl
      REAL(wp) ::   zfice, ziter
      REAL(wp), POINTER, DIMENSION(:,:) ::   z2d
      CHARACTER(len=15) ::   znam
      CHARACTER(len=2)  ::   zchar, zchar1
      INTEGER           ::   jlibalt = jprstlib
      LOGICAL           ::   llok
      !!----------------------------------------------------------------------

      CALL wrk_alloc( jpi, jpj, z2d )

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'lim_rst_read : read ice NetCDF restart file'
         WRITE(numout,*) '~~~~~~~~~~~~~'
      ENDIF

      IF ( jprstlib == jprstdimg ) THEN
        ! eventually read netcdf file (monobloc)  for restarting on different number of processors
        ! if {cn_icerst_in}.nc exists, then set jlibalt to jpnf90
        INQUIRE( FILE = TRIM(cn_icerst_indir)//'/'//TRIM(cn_icerst_in)//'.nc', EXIST = llok )
        IF ( llok ) THEN ; jlibalt = jpnf90  ; ELSE ; jlibalt = jprstlib ; ENDIF
      ENDIF

      CALL iom_open ( TRIM(cn_icerst_indir)//'/'//cn_icerst_in, numrir, kiolib = jprstlib )

      CALL iom_get( numrir, 'nn_fsbc', zfice )
      CALL iom_get( numrir, 'kt_ice' , ziter )    
      IF(lwp) WRITE(numout,*) '   read ice restart file at time step    : ', ziter
      IF(lwp) WRITE(numout,*) '   in any case we force it to nit000 - 1 : ', nit000 - 1

      !Control of date

      IF( ( nit000 - NINT(ziter) ) /= 1 .AND. ABS( nrstdt ) == 1 )   &
         &     CALL ctl_stop( 'lim_rst_read ===>>>> : problem with nit000 in ice restart',  &
         &                   '   verify the file or rerun with the value 0 for the',        &
         &                   '   control of time parameter  nrstdt' )
      IF( NINT(zfice) /= nn_fsbc          .AND. ABS( nrstdt ) == 1 )   &
         &     CALL ctl_stop( 'lim_rst_read ===>>>> : problem with nn_fsbc in ice restart',  &
         &                   '   verify the file or rerun with the value 0 for the',         &
         &                   '   control of time parameter  nrstdt' )

      ! Prognostic variables 
      DO jl = 1, jpl 
         WRITE(zchar,'(I2)') jl
         znam = 'v_i'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         v_i(:,:,jl) = z2d(:,:)
         znam = 'v_s'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         v_s(:,:,jl) = z2d(:,:) 
         znam = 'smv_i'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         smv_i(:,:,jl) = z2d(:,:)
         znam = 'oa_i'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         oa_i(:,:,jl) = z2d(:,:)
         znam = 'a_i'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         a_i(:,:,jl) = z2d(:,:)
         znam = 't_su'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         t_su(:,:,jl) = z2d(:,:)
         znam = 'tempt_sl1'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         e_s(:,:,1,jl) = z2d(:,:)
         DO jk = 1, nlay_i 
            WRITE(zchar1,'(I2)') jk
            znam = 'tempt'//'_il'//TRIM(ADJUSTL(zchar1))//'_htc'//TRIM(ADJUSTL(zchar))
            CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
            e_i(:,:,jk,jl) = z2d(:,:)
         END DO
      END DO

      CALL iom_get( numrir, jpdom_autoglo, 'u_ice'     , u_ice      )
      CALL iom_get( numrir, jpdom_autoglo, 'v_ice'     , v_ice      )
      CALL iom_get( numrir, jpdom_autoglo, 'stress1_i' , stress1_i  )
      CALL iom_get( numrir, jpdom_autoglo, 'stress2_i' , stress2_i  )
      CALL iom_get( numrir, jpdom_autoglo, 'stress12_i', stress12_i )
      CALL iom_get( numrir, jpdom_autoglo, 'snwice_mass'  , snwice_mass )
      CALL iom_get( numrir, jpdom_autoglo, 'snwice_mass_b', snwice_mass_b )

      DO jl = 1, jpl 
         WRITE(zchar,'(I2)') jl
         znam = 'sxice'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxice(:,:,jl) = z2d(:,:)
         znam = 'syice'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         syice(:,:,jl) = z2d(:,:)
         znam = 'sxxice'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxxice(:,:,jl) = z2d(:,:)
         znam = 'syyice'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         syyice(:,:,jl) = z2d(:,:)
         znam = 'sxyice'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxyice(:,:,jl) = z2d(:,:)
         znam = 'sxsn'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxsn(:,:,jl) = z2d(:,:)
         znam = 'sysn'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sysn(:,:,jl) = z2d(:,:)
         znam = 'sxxsn'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxxsn(:,:,jl) = z2d(:,:)
         znam = 'syysn'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         syysn(:,:,jl) = z2d(:,:)
         znam = 'sxysn'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxysn(:,:,jl) = z2d(:,:)
         znam = 'sxa'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxa(:,:,jl) = z2d(:,:)
         znam = 'sya'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sya(:,:,jl) = z2d(:,:)
         znam = 'sxxa'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxxa(:,:,jl) = z2d(:,:)
         znam = 'syya'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         syya(:,:,jl) = z2d(:,:)
         znam = 'sxya'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxya(:,:,jl) = z2d(:,:)
         znam = 'sxc0'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxc0(:,:,jl) = z2d(:,:)
         znam = 'syc0'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         syc0(:,:,jl) = z2d(:,:)
         znam = 'sxxc0'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxxc0(:,:,jl) = z2d(:,:)
         znam = 'syyc0'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         syyc0(:,:,jl) = z2d(:,:)
         znam = 'sxyc0'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxyc0(:,:,jl) = z2d(:,:)
         znam = 'sxsal'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxsal(:,:,jl) = z2d(:,:)
         znam = 'sysal'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sysal(:,:,jl) = z2d(:,:)
         znam = 'sxxsal'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxxsal(:,:,jl) = z2d(:,:)
         znam = 'syysal'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         syysal(:,:,jl) = z2d(:,:)
         znam = 'sxysal'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxysal(:,:,jl) = z2d(:,:)
         znam = 'sxage'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxage(:,:,jl) = z2d(:,:)
         znam = 'syage'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         syage(:,:,jl) = z2d(:,:)
         znam = 'sxxage'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxxage(:,:,jl) = z2d(:,:)
         znam = 'syyage'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         syyage(:,:,jl) = z2d(:,:)
         znam = 'sxyage'//'_htc'//TRIM(ADJUSTL(zchar))
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxyage(:,:,jl)= z2d(:,:)
      END DO

      CALL iom_get( numrir, jpdom_autoglo, 'sxopw ' ,  sxopw  )
      CALL iom_get( numrir, jpdom_autoglo, 'syopw ' ,  syopw  )
      CALL iom_get( numrir, jpdom_autoglo, 'sxxopw' ,  sxxopw )
      CALL iom_get( numrir, jpdom_autoglo, 'syyopw' ,  syyopw )
      CALL iom_get( numrir, jpdom_autoglo, 'sxyopw' ,  sxyopw )

      DO jl = 1, jpl 
         WRITE(zchar,'(I2)') jl
         DO jk = 1, nlay_i 
            WRITE(zchar1,'(I2)') jk
            znam = 'sxe'//'_il'//TRIM(ADJUSTL(zchar1))//'_htc'//TRIM(ADJUSTL(zchar))
            CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
            sxe(:,:,jk,jl) = z2d(:,:)
            znam = 'sye'//'_il'//TRIM(ADJUSTL(zchar1))//'_htc'//TRIM(ADJUSTL(zchar))
            CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
            sye(:,:,jk,jl) = z2d(:,:)
            znam = 'sxxe'//'_il'//TRIM(ADJUSTL(zchar1))//'_htc'//TRIM(ADJUSTL(zchar))
            CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
            sxxe(:,:,jk,jl) = z2d(:,:)
            znam = 'syye'//'_il'//TRIM(ADJUSTL(zchar1))//'_htc'//TRIM(ADJUSTL(zchar))
            CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
            syye(:,:,jk,jl) = z2d(:,:)
            znam = 'sxye'//'_il'//TRIM(ADJUSTL(zchar1))//'_htc'//TRIM(ADJUSTL(zchar))
            CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
            sxye(:,:,jk,jl) = z2d(:,:)
         END DO
      END DO
      !
      ! clem: I do not understand why the following IF is needed
      !       I suspect something inconsistent in the main code with option nn_icesal=1
      IF( nn_icesal == 1 ) THEN
         DO jl = 1, jpl 
            sm_i(:,:,jl) = rn_icesal
            DO jk = 1, nlay_i 
               s_i(:,:,jk,jl) = rn_icesal
            END DO
         END DO
      ENDIF
      !
      !CALL iom_close( numrir ) !clem: closed in sbcice_lim.F90
      !
      CALL wrk_dealloc( jpi, jpj, z2d )
      !
   END SUBROUTINE lim_rst_read

#else
   !!----------------------------------------------------------------------
   !!   Default option :       Empty module            NO LIM sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_rst_read             ! Empty routine
   END SUBROUTINE lim_rst_read
   SUBROUTINE lim_rst_write            ! Empty routine
   END SUBROUTINE lim_rst_write
#endif

   !!======================================================================
END MODULE limrst
