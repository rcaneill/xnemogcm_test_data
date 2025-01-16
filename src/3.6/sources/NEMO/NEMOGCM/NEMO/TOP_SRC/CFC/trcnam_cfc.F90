MODULE trcnam_cfc
   !!======================================================================
   !!                         ***  MODULE trcnam_cfc  ***
   !! TOP :   initialisation of some run parameters for CFC chemical model
   !!======================================================================
   !! History :   2.0  !  2007-12  (C. Ethe, G. Madec) from trcnam.cfc.h90
   !!----------------------------------------------------------------------
#if defined key_cfc
   !!----------------------------------------------------------------------
   !!   'key_cfc'                                               CFC tracers
   !!----------------------------------------------------------------------
   !! trc_nam_cfc      : CFC model initialisation
   !!----------------------------------------------------------------------
   USE oce_trc         ! Ocean variables
   USE par_trc         ! TOP parameters
   USE trc             ! TOP variables
   USE trcsms_cfc      ! CFC specific variable
   USE iom             ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_nam_cfc   ! called by trcnam.F90 module

   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: trcnam_cfc.F90 8353 2017-07-19 14:41:00Z lovato $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE trc_nam_cfc
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE trc_nam_cfc  ***
      !!                 
      !! ** Purpose :   Definition some run parameter for CFC model
      !!
      !! ** Method  :   Read the namcfc namelist and check the parameter 
      !!       values called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist namcfc
      !!----------------------------------------------------------------------
      INTEGER ::  numnatc_ref = -1   ! Logical unit for reference CFC namelist
      INTEGER ::  numnatc_cfg = -1   ! Logical unit for configuration CFC namelist
      INTEGER ::  numonc      = -1   ! Logical unit for output namelist
      INTEGER :: ios                 ! Local integer output status for namelist read
      INTEGER :: jl, jn, cnt
      TYPE(DIAG), DIMENSION(jp_cfc_2d) :: cfcdia2d
      !!
      NAMELIST/namcfcdate/ ndate_beg, nyear_res, clnamecfc
      NAMELIST/namcfcdia/  cfcdia2d     ! additional diagnostics
      !!----------------------------------------------------------------------
      !                             ! Open namelist files
      CALL ctl_opn( numnatc_ref, 'namelist_cfc_ref'   ,     'OLD', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )
      CALL ctl_opn( numnatc_cfg, 'namelist_cfc_cfg'   ,     'OLD', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )
      IF(lwm) CALL ctl_opn( numonc, 'output.namelist.cfc', 'UNKNOWN', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )

      REWIND( numnatc_ref )              ! Namelist namcfcdate in reference namelist : CFC parameters
      READ  ( numnatc_ref, namcfcdate, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namcfcdate in reference namelist', lwp )

      REWIND( numnatc_cfg )              ! Namelist namcfcdate in configuration namelist : CFC parameters
      READ  ( numnatc_cfg, namcfcdate, IOSTAT = ios, ERR = 902 )
902   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namcfcdate in configuration namelist', lwp )
      IF(lwm) WRITE ( numonc, namcfcdate )

      jn = jp_cfc0 - 1
      ! Variables setting
      IF( lp_cfc11 ) THEN
         jn = jn + 1
         ctrcnm    (jn) = 'CFC11'
         ctrcln    (jn) = 'Chlorofluoro carbon 11 Concentration'
         ctrcun    (jn) = 'umolC/L'
         ln_trc_ini(jn) = .false.
         ln_trc_wri(jn) = .true.
      ENDIF
      !
      IF( lp_cfc12 ) THEN
         jn = jn + 1
         ctrcnm    (jn) = 'CFC12'
         ctrcln    (jn) = 'Chlorofluoro carbon 12 Concentration'
         ctrcun    (jn) = 'umolC/L'
         ln_trc_ini(jn) = .false.
         ln_trc_wri(jn) = .true.
      ENDIF
      !
      IF( lp_sf6 ) THEN
         jn = jn + 1
         ctrcnm    (jn) = 'SF6'
         ctrcln    (jn) = 'Sulfur hexafluoride Concentration'
         ctrcun    (jn) = 'umol/L'
         ln_trc_ini(jn) = .false.
         ln_trc_wri(jn) = .true.
      ENDIF

      IF(lwp) THEN                  ! control print
         WRITE(numout,*) ' '
         WRITE(numout,*) ' CFCs'
         WRITE(numout,*) ' '
         WRITE(numout,*) ' trc_nam: Read namdates, namelist for CFC chemical model'
         WRITE(numout,*) ' ~~~~~~~'
         WRITE(numout,*) '    initial calendar date (aammjj) for CFC  ndate_beg = ', ndate_beg
         WRITE(numout,*) '    restoring time constant (year)          nyear_res = ', nyear_res
         WRITE(numout,*) '    Atmospheric CFC concentrations file     clnamecfc = ', TRIM(clnamecfc)
         WRITE(numout,*) '    Compute dynamics for CFC-11             lp_cfc11  = ', lp_cfc11
         WRITE(numout,*) '    Compute dynamics for CFC-12             lp_cfc12  = ', lp_cfc12
         WRITE(numout,*) '    Compute dynamics for SF6                lp_sf6    = ', lp_sf6
      ENDIF
      nyear_beg = ndate_beg / 10000
      IF(lwp) WRITE(numout,*) '    initial year (aa)                       nyear_beg = ', nyear_beg
      !
      ! check consistency between CFC namelist and par_cfc setting
      if ( jn - jp_cfc0 + 1 .ne. jp_cfc )  &
      CALL ctl_stop( 'trc_nam_cfc: Number of selected CFCs is different from total CFC number (jp_cfc) specified in par_cfc.F90' )
      !

      IF( .NOT.lk_iomput .AND. ln_diatrc ) THEN
         !
         ! Namelist namcfcdia
         ! -------------------
         REWIND( numnatc_ref )              ! Namelist namcfcdia in reference namelist : CFC diagnostics
         READ  ( numnatc_ref, namcfcdia, IOSTAT = ios, ERR = 903)
903      IF( ios /= 0 ) CALL ctl_nam ( ios , 'namcfcdia in reference namelist', lwp )

         REWIND( numnatc_cfg )              ! Namelist namcfcdia in configuration namelist : CFC diagnostics
         READ  ( numnatc_cfg, namcfcdia, IOSTAT = ios, ERR = 904 )
904      IF( ios /= 0 ) CALL ctl_nam ( ios , 'namcfcdia in configuration namelist', lwp )
         IF(lwm) WRITE ( numonc, namcfcdia )

         DO jl = 1, jp_cfc_2d
            jn = jp_cfc0_2d + jl - 1
            ctrc2d(jn) = TRIM( cfcdia2d(jl)%sname )
            ctrc2l(jn) = TRIM( cfcdia2d(jl)%lname )
            ctrc2u(jn) = TRIM( cfcdia2d(jl)%units )
         END DO

         IF(lwp) THEN                   ! control print
            WRITE(numout,*)
            WRITE(numout,*) ' Namelist : natadd'
            DO jl = 1, jp_cfc_2d
               jn = jp_cfc0_2d + jl - 1
               WRITE(numout,*) '  2d diag nb : ', jn, '    short name : ', ctrc2d(jn), &
                 &             '  long name  : ', ctrc2l(jn), '   unit : ', ctrc2u(jn)
            END DO
            WRITE(numout,*) ' '
         ENDIF
         !
      ENDIF

   IF(lwm) CALL FLUSH ( numonc )     ! flush output namelist CFC

   END SUBROUTINE trc_nam_cfc
   
#else
   !!----------------------------------------------------------------------
   !!  Dummy module :                                                No CFC
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_nam_cfc                      ! Empty routine
   END  SUBROUTINE  trc_nam_cfc
#endif  

   !!======================================================================
END MODULE trcnam_cfc
