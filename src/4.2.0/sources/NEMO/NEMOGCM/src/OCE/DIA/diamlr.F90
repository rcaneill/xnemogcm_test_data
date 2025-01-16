MODULE diamlr
   !!======================================================================
   !!                       ***  MODULE  diamlr  ***
   !! Management of the IOM context for multiple-linear-regression analysis
   !!======================================================================
   !! History :  4.0  !  2019  (S. Mueller)   Original code
   !!----------------------------------------------------------------------

   USE par_oce        , ONLY :   wp, jpi, jpj
   USE phycst         , ONLY :   rpi
   USE dom_oce        , ONLY :   adatrj
   USE tide_mod
   !
   USE in_out_manager , ONLY :   lwp, numout, ln_timing
   USE iom            , ONLY :   iom_put, iom_use, iom_update_file_name
   USE timing         , ONLY :   timing_start, timing_stop
#if defined key_xios
   USE xios
#endif

   IMPLICIT NONE
   PRIVATE

   LOGICAL, PUBLIC ::   lk_diamlr = .FALSE.   !:         ===>>>   NOT a DOCTOR norm name :  use l_diamlr
   !                                                              lk_  is used only for logical controlled by a CPP key

   PUBLIC ::   dia_mlr_init, dia_mlr_iom_init, dia_mlr

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2019)
   !! $Id$
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS
   
   SUBROUTINE dia_mlr_init
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE dia_mlr_init  ***
      !!
      !! ** Purpose : initialisation of IOM context management for 
      !!              multiple-linear-regression analysis
      !!
      !!----------------------------------------------------------------------
      !
      lk_diamlr = .TRUE.
      !
      IF(lwp) THEN
         WRITE(numout, *)
         WRITE(numout, *) 'dia_mlr_init : initialisation of IOM context management for'
         WRITE(numout, *) '~~~~~~~~~~~~   multiple-linear-regression analysis'
      END IF
      !
   END SUBROUTINE dia_mlr_init


   SUBROUTINE dia_mlr_iom_init
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE dia_mlr_iom_init  ***
      !!
      !! ** Purpose : IOM context setup for multiple-linear-regression
      !!              analysis
      !!
      !!----------------------------------------------------------------------
#if defined key_xios

      TYPE(xios_fieldgroup)                       ::   slxhdl_fldgrp
      TYPE(xios_filegroup)                        ::   slxhdl_filgrp
      TYPE(xios_field), ALLOCATABLE, DIMENSION(:) ::   slxhdl_regs,    slxhdl_flds
      TYPE(xios_field)                            ::   slxhdl_fld
      TYPE(xios_file)                             ::   slxhdl_fil
      LOGICAL                                     ::   llxatt_enabled, llxatt_comment
      CHARACTER(LEN=256)                          ::   clxatt_expr,    clxatt_comment
      CHARACTER(LEN=32)                           ::   clxatt_name1,   clxatt_name2
      CHARACTER(LEN=32)                           ::   clxatt_gridref, clxatt_fieldref
      INTEGER, PARAMETER                          ::   jpscanmax = 999
      INTEGER                                     ::   ireg, ifld
      CHARACTER(LEN=3)                            ::   cl3i
      CHARACTER(LEN=6)                            ::   cl6a
      CHARACTER(LEN=7)                            ::   cl7a
      CHARACTER(LEN=1)                            ::   clgt
      CHARACTER(LEN=2)                            ::   clgd
      CHARACTER(LEN=25)                           ::   clfloat
      CHARACTER(LEN=32)                           ::   clrepl
      INTEGER                                     ::   jl, jm, jn
      INTEGER                                     ::   itide                       ! Number of available tidal components
      REAL(wp)                                    ::   ztide_phase                 ! Tidal-constituent phase at adatrj=0
      CHARACTER (LEN=4), DIMENSION(jpmax_harmo)   ::   ctide_selected = 'n/a '
      TYPE(tide_harmonic), DIMENSION(:), POINTER  ::   stideconst

      IF(lwp) THEN
         WRITE(numout, *)
         WRITE(numout, *) 'dia_mlr_iom_init : IOM context setup for multiple-linear-regression'
         WRITE(numout, *) '~~~~~~~~~~~~~~~~'
      END IF

      ! Get handles to multiple-linear-regression analysis configuration (field
      ! group 'diamrl_fields' and file group 'diamlr_files'); if no suitable
      ! configuration is found, disable diamlr
      IF ( lk_diamlr .AND. xios_is_valid_fieldgroup( "diamlr_fields" ) .AND. xios_is_valid_field( "diamlr_time" ) .AND.   &
         & xios_is_valid_filegroup( "diamlr_files" ) ) THEN
         CALL xios_get_handle("diamlr_fields", slxhdl_fldgrp)
         CALL xios_get_handle("diamlr_files",  slxhdl_filgrp)
      ELSE
         IF (lwp) THEN
            WRITE(numout, *) "diamlr: configuration not found or incomplete (field group 'diamlr_fields'"
            WRITE(numout, *) "        and/or file group 'diamlr_files' and/or field 'diamlr_time' missing);"
            WRITE(numout, *) "        disabling output for multiple-linear-regression analysis."
         END IF
         lk_diamlr = .FALSE.
      END IF

      ! Set up IOM context for multiple-linear-regression analysis
      IF ( lk_diamlr ) THEN

         ! Set up output files for grid types scalar, grid_T, grid_U, grid_V,
         ! and grid_W
         DO jm = 1, 5
            SELECT CASE( jm )
            CASE( 1 )
               cl6a = 'scalar'
            CASE( 2 )
               cl6a = 'grid_T'
            CASE( 3 )
               cl6a = 'grid_U'
            CASE( 4 )
               cl6a = 'grid_V'
            CASE( 5 )
               cl6a = 'grid_W'
            END SELECT
            CALL xios_add_child      ( slxhdl_filgrp, slxhdl_fil, "diamlr_file_"//cl6a )
            CALL xios_set_attr       ( slxhdl_fil, name_suffix="_diamlr_"//cl6a,   &
               &                       description="Intermediate output for multiple-linear-regression analysis - "//cl6a )
            CALL iom_update_file_name( "diamlr_file_"//cl6a )
         END DO

         ! Compile lists of active regressors and of fields selected for
         ! analysis (fields "diamlr_r<nnn>" and "diamlr_f<nnn>", where <nnn> is
         ! a 3-digit integer); also carry out placeholder substitution of tidal
         ! parameters in regressor expressions
         !
         ALLOCATE( slxhdl_regs( jpscanmax ), slxhdl_flds( jpscanmax ) )
         ireg = 0
         ifld = 0
         !
         IF ( ln_tide ) THEN
            ! Retrieve information (frequency, phase, nodal correction) about all
            ! available tidal constituents for placeholder substitution below
            ! Warning: we must use the same character length in an array constructor (at least for gcc compiler)
            ctide_selected(1:34) = (/ 'Mf  ', 'Mm  ', 'Ssa ', 'Mtm ', 'Msf ',         &
               &                      'Msqm', 'Sa  ', 'K1  ', 'O1  ', 'P1  ',         &
               &                      'Q1  ', 'J1  ', 'S1  ', 'M2  ', 'S2  ', 'N2  ', &
               &                      'K2  ', 'nu2 ', 'mu2 ', '2N2 ', 'L2  ',         &
               &                      'T2  ', 'eps2', 'lam2', 'R2  ', 'M3  ',         &
               &                      'MKS2', 'MN4 ', 'MS4 ', 'M4  ', 'N4  ',         &
               &                      'S4  ', 'M6  ', 'M8  ' /)
            CALL tide_init_harmonics(ctide_selected, stideconst)
            itide = size(stideconst)
         ELSE
            itide = 0
         ENDIF
         
         DO jm = 1, jpscanmax
            WRITE (cl3i, '(i3.3)') jm

            ! Look for regressor
            IF ( xios_is_valid_field( "diamlr_r"//cl3i ) ) THEN

               CALL xios_get_handle( "diamlr_r"//cl3i, slxhdl_regs(ireg+1) )
               ! Retrieve pre-configured value of "enabled" attribute and
               ! regressor expression
               CALL xios_get_attr  ( slxhdl_regs(ireg+1), enabled=llxatt_enabled, expr=clxatt_expr )
               ! If enabled, keep handle in list of active regressors; also
               ! substitute placeholders for tidal frequencies, phases, and
               ! nodal corrections in regressor expressions
               IF ( llxatt_enabled ) THEN

                  ! Substitution of placeholders for tidal-constituent
                  ! parameters (amplitudes, angular veloccities, nodal phase
                  ! correction) with values that have been obtained from the
                  ! tidal-forcing implementation (if enabled)
                  DO jn = 1, itide
                     ! Compute phase of tidal constituent (incl. current nodal
                     ! correction) at the start of the model run (i.e. for
                     ! adatrj=0)
                     ztide_phase = MOD( stideconst(jn)%u +  stideconst(jn)%v0 - adatrj * 86400.0_wp * stideconst(jn)%omega, &
                        & 2.0_wp * rpi )
                     clrepl = "__TDE_"//TRIM( stideconst(jn)%cname_tide )//"_omega__"
                     DO WHILE ( INDEX( clxatt_expr, TRIM( clrepl ) ) > 0 )
                        WRITE (clfloat, '(e25.18)') stideconst(jn)%omega
                        jl = INDEX( clxatt_expr, TRIM( clrepl ) )
                        clxatt_expr = clxatt_expr(1:jl - 1)//clfloat//   &
                           &          clxatt_expr(jl + LEN( TRIM( clrepl ) ):LEN( TRIM( clxatt_expr ) ))
                     END DO
                     clrepl = "__TDE_"//TRIM( stideconst(jn)%cname_tide )//"_phase__"
                     DO WHILE ( INDEX( clxatt_expr, TRIM( clrepl ) ) > 0 )
                        WRITE (clfloat, '(e25.18)') ztide_phase
                        jl = INDEX( clxatt_expr, TRIM( clrepl ) )
                        clxatt_expr = clxatt_expr(1:jl - 1)//clfloat//   &
                           &          clxatt_expr(jl + LEN( TRIM( clrepl ) ):LEN( TRIM( clxatt_expr ) ))
                     END DO
                     clrepl = "__TDE_"//TRIM( stideconst(jn)%cname_tide )//"_amplitude__"
                     DO WHILE (INDEX( clxatt_expr, TRIM( clrepl ) ) > 0 )
                        WRITE (clfloat, '(e25.18)') stideconst(jn)%f
                        jl = INDEX( clxatt_expr, TRIM( clrepl ) )
                        clxatt_expr = clxatt_expr(1:jl - 1)//clfloat//   &
                           &          clxatt_expr(jl + LEN( TRIM( clrepl ) ):LEN( TRIM( clxatt_expr ) ))
                     END DO
                  END DO

                  ! Set standard value for comment attribute, including possible
                  ! existing comment added in parantheses
                  CALL xios_is_defined_attr( slxhdl_regs(ireg+1), comment=llxatt_comment )
                  IF ( llxatt_comment ) THEN
                     CALL xios_get_attr( slxhdl_regs(ireg+1), comment=clxatt_comment )
                     clxatt_comment = "Regressor "//cl3i//" ("//TRIM( clxatt_comment )//") "
                  ELSE
                     clxatt_comment = "Regressor "//cl3i
                  END IF

                  ! Set name attribute (and overwrite possible pre-configured
                  ! name) with field id to enable id string retrieval from
                  ! stored handle below, re-set expression with possible
                  ! substitutions, and set or re-set comment attribute
                  CALL xios_set_attr  ( slxhdl_regs(ireg+1), name="diamlr_r"//cl3i, expr=TRIM( clxatt_expr ),   &
                     &                  comment=TRIM( clxatt_comment ) )

                  ireg = ireg + 1   ! Accept regressor in list of active regressors

               END IF
            END IF

            ! Look for field
            IF ( xios_is_valid_field( "diamlr_f"//cl3i ) ) THEN

               CALL xios_get_handle( "diamlr_f"//cl3i, slxhdl_flds(ifld+1) )
               ! Retrieve pre-configured value of "enabled" attribute
               CALL xios_get_attr  ( slxhdl_flds(ifld+1), enabled=llxatt_enabled )
               ! If enabled, keep handle in list of fields selected for analysis
               IF ( llxatt_enabled ) THEN
                  
                  ! Set name attribute (and overwrite possible pre-configured name)
                  ! with field id to enable id string retrieval from stored handle
                  ! below
                  CALL xios_set_attr  ( slxhdl_flds(ifld+1), name="diamlr_f"//cl3i )

                  ifld = ifld + 1   ! Accept field in list of fields selected for analysis

               END IF
            END IF

         END DO

         ! Output number of active regressors and fields selected for analysis
         IF ( lwp ) WRITE(numout,'(a,i3,a)' ) 'diamlr: ', ireg, ' active regressors found'
         IF ( lwp ) WRITE(numout,'(a,i3,a)' ) 'diamlr: ', ifld, ' fields selected for analysis'

         ! Set up output of minimum, maximum, and average values of the time
         ! variable available for the computation of regressors (diamlr_time)
         CALL xios_get_handle( "diamlr_file_scalar", slxhdl_fil )
         CALL xios_add_child ( slxhdl_fil, slxhdl_fld, "diamlr_time_average" )
!$AGRIF_DO_NOT_TREAT
         CALL xios_set_attr  ( slxhdl_fld, standard_name="diamlr_time",                          &
            &                  long_name="Elapsed model time at start of regression interval",   &
            &                  unit="s", operation="average", field_ref="diamlr_time",           &
            &                  grid_ref="diamlr_grid_2D_to_scalar" )
!$AGRIF_END_DO_NOT_TREAT
         CALL xios_add_child ( slxhdl_fil, slxhdl_fld, "diamlr_time_minimum" )
!$AGRIF_DO_NOT_TREAT
         CALL xios_set_attr  ( slxhdl_fld, standard_name="diamlr_time",                          &
            &                  long_name="Elapsed model time at start of regression interval",   &
            &                  unit="s", operation="minimum", field_ref="diamlr_time",           &
            &                  grid_ref="diamlr_grid_2D_to_scalar" )
!$AGRIF_END_DO_NOT_TREAT
         CALL xios_add_child ( slxhdl_fil, slxhdl_fld, "diamlr_time_maximum" )
!$AGRIF_DO_NOT_TREAT
         CALL xios_set_attr  ( slxhdl_fld, standard_name="diamlr_time",                          &
            &                  long_name="Elapsed model time at start of regression interval",   &
            &                  unit="s", operation="maximum", field_ref="diamlr_time",           &
            &                  grid_ref="diamlr_grid_2D_to_scalar" )
!$AGRIF_END_DO_NOT_TREAT

         ! For each active regressor:
         DO jm = 1, ireg

            !   i) set up 2-dimensional and 3-dimensional versions of the
            !      regressors; explicitely set "enabled" attribute; note, while
            !      the scalar versions of regressors are part of the
            !      configuration, the respective 2-dimensional versions take
            !      over the defining expression, while the scalar and
            !      3-dimensional versions are simply obtained via grid
            !      transformations from the 2-dimensional version.
            CALL xios_get_attr  ( slxhdl_regs( jm ), name=clxatt_name1, expr=clxatt_expr,              &
               &                  enabled=llxatt_enabled, comment=clxatt_comment )
            CALL xios_add_child ( slxhdl_fldgrp, slxhdl_fld, TRIM( clxatt_name1 )//"_grid_T_2D" )
            CALL xios_set_attr  ( slxhdl_fld, expr=TRIM( clxatt_expr ), grid_ref="diamlr_grid_T_2D",     &
               &                  field_ref="diamlr_time", enabled=llxatt_enabled )
            CALL xios_add_child ( slxhdl_fldgrp, slxhdl_fld, TRIM( clxatt_name1 )//"_grid_U_2D" )
            CALL xios_set_attr  ( slxhdl_fld, expr=TRIM( clxatt_expr ), grid_ref="diamlr_grid_U_2D",     &
               &                  field_ref="diamlr_time", enabled=llxatt_enabled )
            CALL xios_add_child ( slxhdl_fldgrp, slxhdl_fld, TRIM( clxatt_name1 )//"_grid_V_2D" )
            CALL xios_set_attr  ( slxhdl_fld, expr=TRIM( clxatt_expr ), grid_ref="diamlr_grid_V_2D",     &
               &                  field_ref="diamlr_time", enabled=llxatt_enabled )
            CALL xios_add_child ( slxhdl_fldgrp, slxhdl_fld, TRIM( clxatt_name1 )//"_grid_W_2D" )
            CALL xios_set_attr  ( slxhdl_fld, expr=TRIM( clxatt_expr ), grid_ref="diamlr_grid_W_2D",     &
               &                  field_ref="diamlr_time", enabled=llxatt_enabled )
            CALL xios_add_child ( slxhdl_fldgrp, slxhdl_fld, TRIM( clxatt_name1 )//"_grid_T_3D")
            CALL xios_set_attr  ( slxhdl_fld, expr="this", grid_ref="diamlr_grid_2D_to_grid_T_3D",            &
               &                  field_ref=TRIM( clxatt_name1 )//"_grid_T_2D", enabled=llxatt_enabled)
            CALL xios_add_child ( slxhdl_fldgrp, slxhdl_fld, TRIM( clxatt_name1 )//"_grid_U_3D")
            CALL xios_set_attr  ( slxhdl_fld, expr="this", grid_ref="diamlr_grid_2D_to_grid_U_3D",            &
               &                  field_ref=TRIM( clxatt_name1 )//"_grid_U_2D", enabled=llxatt_enabled)
            CALL xios_add_child ( slxhdl_fldgrp, slxhdl_fld, TRIM( clxatt_name1 )//"_grid_V_3D")
            CALL xios_set_attr  ( slxhdl_fld, expr="this", grid_ref="diamlr_grid_2D_to_grid_V_3D",            &
               &                  field_ref=TRIM( clxatt_name1 )//"_grid_V_2D", enabled=llxatt_enabled)
            CALL xios_add_child ( slxhdl_fldgrp, slxhdl_fld, TRIM( clxatt_name1 )//"_grid_W_3D")
            CALL xios_set_attr  ( slxhdl_fld, expr="this", grid_ref="diamlr_grid_2D_to_grid_W_3D",            &
               &                  field_ref=TRIM( clxatt_name1 )//"_grid_W_2D", enabled=llxatt_enabled)
            CALL xios_set_attr  ( slxhdl_regs(jm), expr="this", grid_ref="diamlr_grid_2D_to_scalar",   &
               &                  field_ref=TRIM( clxatt_name1 )//"_grid_T_2D", enabled=llxatt_enabled)

            !  ii) set up output of active regressors, including metadata
            CALL xios_get_handle( "diamlr_file_scalar", slxhdl_fil )
            ! Add regressor to output file
            CALL xios_add_child ( slxhdl_fil, slxhdl_fld, TRIM( clxatt_name1 ) )
            CALL xios_set_attr  ( slxhdl_fld, standard_name=TRIM( clxatt_comment ), long_name=TRIM( clxatt_expr ),   &
               &                  operation="average" )
               
            ! iii) set up the output of scalar products with itself and with
            !      other active regressors
            CALL xios_get_attr  ( slxhdl_regs(jm), name=clxatt_name1 )
            DO jn = 1, jm
               ! Field for product between regressors
               CALL xios_get_attr  ( slxhdl_regs(jn), name=clxatt_name2 )
               CALL xios_add_child ( slxhdl_fldgrp, slxhdl_fld, TRIM( clxatt_name1 )//"."//TRIM( clxatt_name2 ) )
               ! Set appropriate name attribute to avoid the possibility of
               ! using an inappropriate inherited name attribute as the variable
               ! name in the output file
               CALL xios_set_attr  ( slxhdl_fld,                                                        &
                  &                  name=TRIM( clxatt_name1 )//"."//TRIM( clxatt_name2 ),              &
                  &                  grid_ref="diamlr_grid_scalar",                                     &
                  &                  expr="this * "//TRIM( clxatt_name2 ),                              &
                  &                  field_ref=TRIM( clxatt_name1 ),                                    &
                  &                  enabled=llxatt_enabled,                                            &
                  &                  long_name="Scalar product of regressor "//TRIM( clxatt_name1 )//   &
                  &                     " and regressor "//TRIM( clxatt_name2 ),                        &
                  &                  standard_name=TRIM( clxatt_name1 )//"."//TRIM( clxatt_name2 ),     &
                  &                  operation="accumulate")
               ! Add regressor-product field to output file
               CALL xios_add_child ( slxhdl_fil, slxhdl_fld, TRIM( clxatt_name1 )//"."//TRIM( clxatt_name2 ) )
            END DO

            !  iv) set up definitions for the output of scalar products with
            !      fields selected for analysis
            DO jn = 1, ifld
               CALL xios_get_attr  ( slxhdl_flds(jn), name=clxatt_name2, field_ref=clxatt_fieldref )
               CALL xios_get_handle( TRIM( clxatt_fieldref ), slxhdl_fld )
               CALL xios_get_attr  ( slxhdl_fld, grid_ref=clxatt_gridref )
               clgt="T"
               IF ( INDEX( clxatt_gridref, "_U_" ) > 0 ) clgt="U"
               IF ( INDEX( clxatt_gridref, "_V_" ) > 0 ) clgt="V"
               IF ( INDEX( clxatt_gridref, "_W_" ) > 0 ) clgt="W"
               clgd="2D"
               cl7a=""
               IF ( INDEX( clxatt_gridref, "_3D" ) > 0 ) THEN
                  clgd="3D"
               ELSE
                  cl7a="diamlr_"
               END IF
               CALL xios_add_child ( slxhdl_fldgrp, slxhdl_fld, TRIM( clxatt_name2 )//"."//TRIM( clxatt_name1 ) )
               ! Set appropriate name attribute to avoid the possibility of
               ! using an inappropriate inherited name attribute as the variable
               ! name in the output file; use metadata (standard_name and
               ! long_name) to refer to the id of the analysed field
               CALL xios_set_attr  ( slxhdl_fld,                                                         &
                  &                  name=TRIM( clxatt_name2 )//"."//TRIM( clxatt_name1 ),               &
                  &                  expr="this * "//TRIM( clxatt_fieldref ),                            &
                  &                  grid_ref=cl7a//"grid_"//clgt//"_"//clgd,                                      &
                  &                  field_ref=TRIM( clxatt_name1 )//"_grid_"//clgt//"_"//clgd,          &
                  &                  enabled=llxatt_enabled,                                             &
                  &                  long_name="Scalar product of "//TRIM( clxatt_fieldref )//           &
                  &                     " and regressor "//TRIM( clxatt_name1 ),                         &
                  &                  standard_name=TRIM( clxatt_fieldref )//"."//TRIM( clxatt_name1 ),   &
                  &                  operation="accumulate" )
               CALL xios_get_handle( "diamlr_file_grid_"//clgt, slxhdl_fil )
               CALL xios_add_child ( slxhdl_fil, slxhdl_fld, TRIM( clxatt_name2 )//"."//TRIM( clxatt_name1 ) )
            END DO

         END DO

         ! Release list of active regressors and fields selected for analysis
         DEALLOCATE( slxhdl_regs, slxhdl_flds )

      END IF
#else
      IF( .FALSE. ) write(numout,*) 'dia_mlr_iom_init: should not see this'    ! useless statement to avoid compiler warnings
#endif

   END SUBROUTINE dia_mlr_iom_init


   SUBROUTINE dia_mlr
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE dia_mlr  ***
      !!
      !! ** Purpose : update time used in multiple-linear-regression analysis
      !!
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj) ::   zadatrj2d
      !!----------------------------------------------------------------------

      IF( ln_timing )   CALL timing_start('dia_mlr')

      ! Update time to the continuous time since the start of the model run
      ! (value of adatrj converted to time in units of seconds)
      !
      ! A 2-dimensional field of constant value is sent, and subsequently used directly 
      ! or transformed to a scalar or a constant 3-dimensional field as required.
      zadatrj2d(:,:) = adatrj*86400.0_wp
      IF ( iom_use('diamlr_time') ) CALL iom_put('diamlr_time', zadatrj2d)
      !
      IF( ln_timing )   CALL timing_stop('dia_mlr')
      !
   END SUBROUTINE dia_mlr

   !!======================================================================
END MODULE diamlr
