MODULE trcnam_c14b
   !!======================================================================
   !!                         ***  MODULE trcnam_c14b  ***
   !! TOP :   initialisation of some run parameters for C14 chemical model
   !!======================================================================
   !! History :   2.0  !  2007-12  (C. Ethe, G. Madec) from trcnam.cfc.h90
   !!----------------------------------------------------------------------
#if defined key_c14b
   !!----------------------------------------------------------------------
   !!   'key_c14b'                                         C14 bomb tracer
   !!----------------------------------------------------------------------
   !! trc_nam_c14b      : C14 model initialisation
   !!----------------------------------------------------------------------
   USE oce_trc         ! Ocean variables
   USE par_trc         ! TOP parameters
   USE trc             ! TOP variables
   USE trcsms_c14b     ! C14b specific variable
   USE iom             ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_nam_c14b   ! called by trcnam.F90 module

   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: trcnam_c14b.F90 8353 2017-07-19 14:41:00Z lovato $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE trc_nam_c14b
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE trc_nam_c14b  ***
      !!                 
      !! ** Purpose :   Definition some run parameter for C14 model
      !!
      !! ** Method  :   Read the namc14 namelist and check the parameter 
      !!       values called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist namelist_c14b
      !!----------------------------------------------------------------------
      INTEGER ::  numnatb_ref = -1   ! Logical unit for reference c14b namelist
      INTEGER ::  numnatb_cfg = -1   ! Logical unit for configuration c14b namelist
      INTEGER ::  numonb      = -1   ! Logical unit for output namelist
      INTEGER :: ios                 ! Local integer output status for namelist read

      ! definition of additional diagnostic as a structure
      INTEGER :: jl, jn
      TYPE(DIAG), DIMENSION(jp_c14b_2d) :: c14dia2d
      TYPE(DIAG), DIMENSION(jp_c14b_3d) :: c14dia3d
      !!
      NAMELIST/namc14date/ ndate_beg_b, nyear_res_b
      NAMELIST/namc14dia/  c14dia2d, c14dia3d     ! additional diagnostics
      !!-------------------------------------------------------------------
      ctrcnm    (jp_c14b0) = 'C14B'
      ctrcln    (jp_c14b0) = 'Bomb C14 Concentration'
      ctrcun    (jp_c14b0) = 'ration'
      ln_trc_ini(jp_c14b0) = .false.
      ln_trc_wri(jp_c14b0) = .true.
      !                             ! Open namelist file
      CALL ctl_opn( numnatb_ref, 'namelist_c14b_ref'  ,     'OLD', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )
      CALL ctl_opn( numnatb_cfg, 'namelist_c14b_cfg'  ,     'OLD', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )   
      IF(lwm) CALL ctl_opn( numonb, 'output.namelist.c14', 'UNKNOWN', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )     
      REWIND( numnatb_ref )              ! Namelist namc14date in reference namelist : c14b parameters
      READ  ( numnatb_ref, namc14date, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namc14date in reference namelist', lwp )

      REWIND( numnatb_cfg )              ! Namelist namc14date in configuration namelist : c14b parameters
      READ  ( numnatb_cfg, namc14date, IOSTAT = ios, ERR = 902 )
902   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namc14date in configuration namelist', lwp )
      IF(lwm) WRITE ( numonb, namc14date )
      IF(lwp) THEN                  ! control print
         WRITE(numout,*)
         WRITE(numout,*) ' trc_nam: Read namdates, namelist for C14 chemical model'
         WRITE(numout,*) ' ~~~~~~~'
         WRITE(numout,*) '    initial calendar date (aammjj) for C14  ndate_beg_b = ', ndate_beg_b
         WRITE(numout,*) '    restoring time constant (year)          nyear_res_b = ', nyear_res_b
      ENDIF
      nyear_beg_b = ndate_beg_b / 10000
      IF(lwp) WRITE(numout,*) '    initial year (aa)                  nyear_beg_b = ', nyear_beg_b
      !
      IF( .NOT.lk_iomput .AND. ln_diatrc ) THEN
         !
         ! Namelist namc14dia
         ! -------------------
         REWIND( numnatb_ref )              ! Namelist namc14dia in reference namelist : c14b diagnostics
         READ  ( numnatb_ref, namc14dia, IOSTAT = ios, ERR = 903)
903      IF( ios /= 0 ) CALL ctl_nam ( ios , 'namc14dia in reference namelist', lwp )

         REWIND( numnatb_cfg )              ! Namelist namc14dia in configuration namelist : c14b diagnostics
         READ  ( numnatb_cfg, namc14dia, IOSTAT = ios, ERR = 904 )
904      IF( ios /= 0 ) CALL ctl_nam ( ios , 'namc14dia in configuration namelist', lwp )
         IF(lwm) WRITE ( numonb, namc14dia )

         DO jl = 1, jp_c14b_2d
            jn = jp_c14b0_2d + jl - 1
            ctrc2d(jn) = c14dia2d(jl)%sname
            ctrc2l(jn) = c14dia2d(jl)%lname
            ctrc2u(jn) = c14dia2d(jl)%units
         END DO

         DO jl = 1, jp_c14b_3d
            jn = jp_c14b0_3d + jl - 1
            ctrc3d(jn) = c14dia3d(jl)%sname
            ctrc3l(jn) = c14dia3d(jl)%lname
            ctrc3u(jn) = c14dia3d(jl)%units
         END DO

         IF(lwp) THEN                   ! control print
            WRITE(numout,*)
            WRITE(numout,*) ' Namelist : natadd'
            DO jl = 1, jp_c14b_3d
               jn = jp_c14b0_3d + jl - 1
               WRITE(numout,*) '  3d diag nb : ', jn, '    short name : ', ctrc3d(jn), &
                 &             '  long name  : ', ctrc3l(jn), '   unit : ', ctrc3u(jn)
            END DO
            WRITE(numout,*) ' '

            DO jl = 1, jp_c14b_2d
               jn = jp_c14b0_2d + jl - 1
               WRITE(numout,*) '  2d diag nb : ', jn, '    short name : ', ctrc2d(jn), &
                 &             '  long name  : ', ctrc2l(jn), '   unit : ', ctrc2u(jn)
            END DO
            WRITE(numout,*) ' '
         ENDIF
         !
      ENDIF

   IF(lwm) CALL FLUSH ( numonb )     ! flush output namelist C14b

   END SUBROUTINE trc_nam_c14b
   
#else
   !!----------------------------------------------------------------------
   !!  Dummy module :                                                No 14C
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_nam_c14b                      ! Empty routine
   END  SUBROUTINE  trc_nam_c14b
#endif  

   !!======================================================================
END MODULE trcnam_c14b
