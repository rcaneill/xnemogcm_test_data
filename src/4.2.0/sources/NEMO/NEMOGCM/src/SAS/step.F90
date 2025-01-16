MODULE step
   !!======================================================================
   !!                       ***  MODULE step  ***
   !! Time-stepping    : manager of the ocean, tracer and ice time stepping
   !!                    version for standalone surface scheme
   !!======================================================================
   !! History :  OPA  !  1991-03  (G. Madec)  Original code
   !!             .   !    .                                                     
   !!             .   !    .                                                     
   !!   NEMO     3.5  !  2012-03  (S. Alderson)
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   stp             : OCE system time-stepping
   !!----------------------------------------------------------------------
   USE oce              ! ocean dynamics and tracers variables
   USE dom_oce          ! ocean space and time domain variables 
   USE daymod           ! calendar                         (day     routine)
   USE sbc_oce          ! surface boundary condition: fields
   USE sbcmod           ! surface boundary condition       (sbc     routine)
   USE sbcrnf           ! surface boundary condition: runoff variables
   USE sbccpl           ! surface boundary condition: coupled interface
   USE eosbn2           ! equation of state                (eos_bn2 routine)
   USE diawri           ! Standard run outputs             (dia_wri routine)
   USE bdy_oce   , ONLY: ln_bdy
   USE bdydta           ! mandatory for sea-ice
   USE stpctl           ! time stepping control            (stp_ctl routine)
   !
   USE in_out_manager   ! I/O manager
   USE prtctl           ! Print control                    (prt_ctl routine)
   USE iom              !
   USE lbclnk           !
   USE timing           ! Timing            
#if defined key_xios
   USE xios
#endif

#if defined key_agrif
   USE agrif_oce, ONLY: lk_agrif_debug
#if defined key_si3
   USE agrif_ice_update
#endif
#endif
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC   stp   ! called by nemogcm.F90

   !!----------------------------------------------------------------------
   !! time level indices
   !!----------------------------------------------------------------------
   INTEGER, PUBLIC :: Nbb, Nnn, Naa, Nrhs          !! used by nemo_init
   !!----------------------------------------------------------------------
   !! NEMO/SAS 4.0 , NEMO Consortium (2018)
   !! $Id: step.F90 14239 2020-12-23 08:57:16Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

#if defined key_agrif
   RECURSIVE SUBROUTINE stp( )
      INTEGER             ::   kstp   ! ocean time-step index
#else
   SUBROUTINE stp( kstp )
      INTEGER, INTENT(in) ::   kstp   ! ocean time-step index
#endif
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE stp  ***
      !!                      
      !! ** Purpose : - Time stepping of SBC (surface boundary)
      !! 
      !! ** Method  : -1- Update forcings and data  
      !!              -2- Outputs and diagnostics
      !!----------------------------------------------------------------------

#if defined key_agrif
      IF( nstop > 0 ) RETURN   ! avoid to go further if an error was detected during previous time step (child grid)
      kstp = nit000 + Agrif_Nb_Step()
      Kbb_a = Nbb; Kmm_a = Nnn; Krhs_a = Nrhs   ! agrif_oce module copies of time level indices
      IF( lk_agrif_debug ) THEN
         IF( Agrif_Root() .and. lwp)   WRITE(*,*) '---'
         IF(lwp)   WRITE(*,*) 'Grid Number', Agrif_Fixed(),' time step ', kstp, 'int tstep', Agrif_NbStepint()
      ENDIF
      IF( kstp == nit000 + 1 )   lk_agrif_fstep = .FALSE.
# if defined key_xios
      IF( Agrif_Nbstepint() == 0 )   CALL iom_swap( cxios_context )
# endif   
#endif   
      IF( kstp == nit000 )   CALL iom_init( cxios_context ) ! iom_put initialization (must be done after nemo_init for AGRIF+XIOS+OASIS)
                             CALL iom_setkt( kstp - nit000 + 1, cxios_context )   ! tell iom we are at time step kstp
      IF((kstp == nitrst) .AND. lwxios) THEN
         CALL iom_swap(      cw_ocerst_cxt          )
         CALL iom_init_closedef(cw_ocerst_cxt)
         CALL iom_setkt( kstp - nit000 + 1,      cw_ocerst_cxt          )
#if defined key_top
         CALL iom_swap(      cw_toprst_cxt          )
         CALL iom_init_closedef(cw_toprst_cxt)
         CALL iom_setkt( kstp - nit000 + 1,      cw_toprst_cxt          )
#endif
      ENDIF
      IF( kstp /= nit000 )   CALL day( kstp )             ! Calendar (day was already called at nit000 in day_init)

#if defined key_si3
      IF(((kstp + nn_fsbc - 1) == nitrst) .AND. lwxios) THEN
         CALL iom_swap(      cw_icerst_cxt          )
         CALL iom_init_closedef(cw_icerst_cxt)
         CALL iom_setkt( kstp - nit000 + 1,      cw_icerst_cxt          )
      ENDIF
#endif

      ! ==> clem: open boundaries is mandatory for sea-ice because ice BDY is not decoupled from  
      !           the environment of ocean BDY. Therefore bdy is called in both OCE and SAS modules.
      !           From SAS: ocean bdy data are wrong  (but we do not care) and ice bdy data are OK.  
      !           This is not clean and should be changed in the future. 
      ! ==>
      IF( ln_bdy     )       CALL bdy_dta( kstp,      Nnn )                   ! update dynamic & tracer data at open boundaries
                             CALL sbc    ( kstp, Nbb, Nnn )                   ! Sea Boundary Condition (including sea-ice)

                             CALL dia_wri( kstp,      Nnn )                   ! ocean model: outputs

#if defined key_agrif
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! AGRIF recursive integration
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<      
                             CALL Agrif_Integrate_ChildGrids( stp )
                             
#endif                             
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Control
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                             CALL stp_ctl( kstp, Nnn )

#if defined key_agrif
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! AGRIF update
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<      
      IF( Agrif_NbStepint() == 0 .AND. nstop == 0 ) THEN                       ! AGRIF Update from zoom N to zoom 1 then to Parent 
#if defined key_si3
                             CALL Agrif_Update_ice( )   ! update sea-ice
#endif
      ENDIF

#endif
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! File manipulation at the end of the first time step
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<                         
      IF( kstp == nit000   ) THEN
            CALL iom_close( numror )                          ! close input  ocean restart file
            IF( lrxios )     CALL iom_context_finalize(      cr_ocerst_cxt      )
      ENDIF
      
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Coupled mode
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      IF( lk_oasis .AND. nstop == 0 ) CALL sbc_cpl_snd( kstp, Nbb, Nnn )       ! coupled mode : field exchanges if OASIS-coupled ice

#if defined key_xios
      IF( kstp == nitrst ) THEN
         IF(.NOT.lwxios) THEN
            CALL iom_close( numrow )     
         ELSE
            CALL iom_context_finalize( cw_ocerst_cxt )
            iom_file(numrow)%nfid       = 0
            numrow = 0
         ENDIF
         lrst_oce = .FALSE.
      ENDIF
      IF( kstp == nitend .OR. nstop > 0 ) THEN
         CALL iom_context_finalize( cxios_context ) ! needed for XIOS+AGRIF
      ENDIF
#endif
      !
      IF( ln_timing .AND.  kstp == nit000  )   CALL timing_reset
      !
   END SUBROUTINE stp

   !!======================================================================
END MODULE step
