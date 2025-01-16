MODULE ablrst
   !!======================================================================
   !!                     ***  MODULE  ablrst  ***
   !!       abl :  write/read the abl restart file
   !!======================================================================
   !! History:   4.0  !  2018     (some people)                         ABL
   !!----------------------------------------------------------------------
   !!   abl_rst_opn   : open  restart file
   !!   abl_rst_write : write restart file 
   !!   abl_rst_read  : read  restart file 
   !!----------------------------------------------------------------------
   USE abl            ! abl variables
   USE par_abl        ! abl parameters
   USE dom_oce        ! ocean domain
   USE sbc_oce , ONLY : nn_fsbc, jpka
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O manager library
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! fortran utilities (glob_sum + no signed zero)

   IMPLICIT NONE
   PRIVATE

   PUBLIC   abl_rst_opn     ! called by ablstp
   PUBLIC   abl_rst_write   ! called by ablstp
   PUBLIC   abl_rst_read    ! called by abl_init

   !!----------------------------------------------------------------------
   !! NEMO/ABL 4.0 , NEMO Consortium (2018)
   !! $Id: ablrst.F90 11413 2019-08-06 15:59:22Z gsamson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE abl_rst_opn( kt )
      !!----------------------------------------------------------------------
      !!                    ***  abl_rst_opn  ***
      !!
      !! ** purpose  :   open restart file
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt       ! number of iteration
      !
      CHARACTER(len=20)   ::   clkt     ! ocean time-step define as a character
      CHARACTER(len=50)   ::   clname   ! abl output restart file name
      CHARACTER(len=256)  ::   clpath   ! full path to abl output restart file 
      CHARACTER(LEN=52)   ::   clpname  ! abl output restart file name including prefix for AGRIF
      !!----------------------------------------------------------------------
      !
      IF( kt == nit000 )   lrst_abl = .FALSE.   ! default definition

      IF( ln_rst_list .OR. nn_stock /= -1 ) THEN
      ! in order to get better performances with NetCDF format, we open and define the abl restart file 
      ! one abl time step before writing the data (-> at nitrst - 2*nn_fsbc + 1), except if we write abl 
      ! restart files every abl time step or if an abl restart file was writen at nitend - 2*nn_fsbc + 1
      IF( kt == nitrst - 2*nn_fsbc + 1 .OR. nn_stock == nn_fsbc    &
         &                             .OR. ( kt == nitend - nn_fsbc + 1 .AND. .NOT. lrst_abl ) ) THEN
         IF( nitrst <= nitend .AND. nitrst > 0 ) THEN
            ! beware of the format used to write kt (default is i8.8, that should be large enough...)
            IF( nitrst > 99999999 ) THEN   ;   WRITE(clkt, *       ) nitrst
            ELSE                           ;   WRITE(clkt, '(i8.8)') nitrst
            ENDIF
            ! create the file
            clname = TRIM(cexper)//"_"//TRIM(ADJUSTL(clkt))//"_"//TRIM(cn_ablrst_out)
            clpath = TRIM(cn_ablrst_outdir) 
            IF( clpath(LEN_TRIM(clpath):) /= '/' ) clpath = TRIM(clpath)//'/'
            IF(lwp) THEN
               WRITE(numout,*)
               WRITE(numout,*) '             open abl restart NetCDF file: ',TRIM(clpath)//clname
               IF( kt == nitrst - 2*nn_fsbc + 1 ) THEN
                  WRITE(numout,*) '             kt = nitrst - 2*nn_fsbc + 1 = ', kt,' date= ', ndastp
               ELSE
                  WRITE(numout,*) '             kt = '                         , kt,' date= ', ndastp
               ENDIF
            ENDIF
            !
            IF(.NOT.lwxios) THEN
               CALL iom_open( TRIM(clpath)//TRIM(clname), numraw, ldwrt = .TRUE., kdlev = jpka, cdcomp = 'ABL' )
            ELSE
#if defined key_xios
               cw_ablrst_cxt = "rstwa_"//TRIM(ADJUSTL(clkt))
               IF( TRIM(Agrif_CFixed()) == '0' ) THEN
                  clpname = clname
               ELSE
                  clpname = TRIM(Agrif_CFixed())//"_"//clname
               ENDIF
               numraw = iom_xios_setid(TRIM(clpath)//TRIM(clpname))
               CALL iom_init( cw_ablrst_cxt, kdid = numraw, ld_closedef = .FALSE. )
               CALL iom_swap( cxios_context )
#else
               CALL ctl_stop( 'Can not use XIOS in rst_opn' )
#endif
            ENDIF
            lrst_abl = .TRUE.
         ENDIF
      ENDIF
      ENDIF
      !
   END SUBROUTINE abl_rst_opn


   SUBROUTINE abl_rst_write( kt )
      !!----------------------------------------------------------------------
      !!                    ***  abl_rst_write  ***
      !!
      !! ** purpose  :   write restart file
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt     ! number of iteration
      !!
      INTEGER ::   iter
      !!----------------------------------------------------------------------

      iter = kt + nn_fsbc - 1   ! abl restarts are written at kt == nitrst - nn_fsbc + 1

      IF( iter == nitrst ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'abl_rst_write : write abl restart file  kt =', kt
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~'         
      ENDIF

      ! Write in numraw (if iter == nitrst)
      ! ------------------
      !                                                                        ! calendar control
      CALL iom_rstput( iter, nitrst, numraw, 'nn_fsbc', REAL( nn_fsbc, wp ) )      ! time-step 
      CALL iom_rstput( iter, nitrst, numraw, 'kt_abl' , REAL( iter   , wp ) )      ! date

      IF(.NOT.lwxios) CALL iom_delay_rst( 'WRITE', 'ABL', numraw )   ! save only abl delayed global communication variables

      ! Prognostic (after timestep + swap time indices = now timestep) variables
      CALL iom_rstput( iter, nitrst, numraw,   'u_abl',   u_abl(:,:,:,nt_n      ) )
      CALL iom_rstput( iter, nitrst, numraw,   'v_abl',   v_abl(:,:,:,nt_n      ) )
      CALL iom_rstput( iter, nitrst, numraw,   't_abl',  tq_abl(:,:,:,nt_n,jp_ta) )
      CALL iom_rstput( iter, nitrst, numraw,   'q_abl',  tq_abl(:,:,:,nt_n,jp_qa) )
      CALL iom_rstput( iter, nitrst, numraw, 'tke_abl', tke_abl(:,:,:,nt_n      ) )
      CALL iom_rstput( iter, nitrst, numraw, 'avm_abl', avm_abl(:,:,:           ) )
      CALL iom_rstput( iter, nitrst, numraw, 'avt_abl', avt_abl(:,:,:           ) )
      CALL iom_rstput( iter, nitrst, numraw,'mxld_abl',mxld_abl(:,:,:           ) )
      CALL iom_rstput( iter, nitrst, numraw,    'pblh',    pblh(:,:             ) )
      !

      ! close restart file
      ! ------------------
      IF( iter == nitrst ) THEN
         IF(.NOT.lwxios) THEN
            CALL iom_close( numraw )
         ELSE
            CALL iom_context_finalize(      cw_ablrst_cxt          )
            iom_file(numraw)%nfid       = 0
            numraw = 0
         ENDIF
         lrst_abl = .FALSE.
      ENDIF
      !
   END SUBROUTINE abl_rst_write


   SUBROUTINE abl_rst_read
      !!----------------------------------------------------------------------
      !!                    ***  abl_rst_read  ***
      !!
      !! ** purpose  :   read restart file
      !!----------------------------------------------------------------------
      REAL(wp)          ::   zfabl, ziter
      !!----------------------------------------------------------------------

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'abl_rst_read: read abl NetCDF restart file'
         WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF

      lxios_sini = .FALSE.
      CALL iom_open ( TRIM(cn_ablrst_indir)//'/'//cn_ablrst_in, numrar )

      IF( lrxios) THEN
          cr_ablrst_cxt = 'abl_rst'
          IF(lwp) WRITE(numout,*) 'Enable restart reading by XIOS for ABL'
!         IF( TRIM(Agrif_CFixed()) == '0' ) THEN
!            clpname = cn_ablrst_in
!         ELSE
!            clpname = TRIM(Agrif_CFixed())//"_"//cn_ablrst_in
!         ENDIF
          CALL iom_init( cr_ablrst_cxt, kdid = numrar, ld_closedef = .TRUE. )
      ENDIF

      ! Time info
      CALL iom_get( numrar, 'nn_fsbc', zfabl )
      CALL iom_get( numrar, 'kt_abl' , ziter )    
      IF(lwp) WRITE(numout,*) '   read abl restart file at time step    : ', ziter
      IF(lwp) WRITE(numout,*) '   in any case we force it to nit000 - 1 : ', nit000 - 1

      ! Control of date
      IF( ( nit000 - NINT(ziter) ) /= 1 .AND. ABS( nrstdt ) == 1 )   &
         &     CALL ctl_stop( 'abl_rst_read ===>>>> : problem with nit000 in abl restart',  &
         &                   '   verify the file or rerun with the value 0 for the',        &
         &                   '   control of time parameter  nrstdt' )
      IF( NINT(zfabl) /= nn_fsbc          .AND. ABS( nrstdt ) == 1 )   &
         &     CALL ctl_stop( 'abl_rst_read ===>>>> : problem with nn_fsbc in abl restart',  &
         &                   '   verify the file or rerun with the value 0 for the',         &
         &                   '   control of time parameter  nrstdt' )

      ! --- mandatory fields --- ! 
      CALL iom_get( numrar, jpdom_auto,   'u_abl',   u_abl(:,:,:,nt_n      ), cd_type = 'U', psgn = -1._wp )
      CALL iom_get( numrar, jpdom_auto,   'v_abl',   v_abl(:,:,:,nt_n      ), cd_type = 'V', psgn = -1._wp )
      CALL iom_get( numrar, jpdom_auto,   't_abl',  tq_abl(:,:,:,nt_n,jp_ta) )
      CALL iom_get( numrar, jpdom_auto,   'q_abl',  tq_abl(:,:,:,nt_n,jp_qa) )
      CALL iom_get( numrar, jpdom_auto, 'tke_abl', tke_abl(:,:,:,nt_n      ) )
      CALL iom_get( numrar, jpdom_auto, 'avm_abl', avm_abl(:,:,:           ) )
      CALL iom_get( numrar, jpdom_auto, 'avt_abl', avt_abl(:,:,:           ) )
      CALL iom_get( numrar, jpdom_auto,'mxld_abl',mxld_abl(:,:,:           ) )
      CALL iom_get( numrar, jpdom_auto,    'pblh',    pblh(:,:             ) )

      IF(.NOT.lrxios) CALL iom_delay_rst( 'READ', 'ABL', numrar )   ! read only abl delayed global communication variables

   END SUBROUTINE abl_rst_read


   !!======================================================================
END MODULE ablrst
