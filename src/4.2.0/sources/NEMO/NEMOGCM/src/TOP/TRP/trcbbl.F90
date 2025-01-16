MODULE trcbbl
  !!======================================================================
   !!                       ***  MODULE  trcbbl  ***
   !! Ocean passive tracers physics :  advective and/or diffusive bottom boundary 
   !!                                  layer scheme
   !!======================================================================
   !! History :  OPA  !  1996-06  (L. Mortier)  Original code
   !!            8.0  !  1997-11  (G. Madec)    Optimization
   !!   NEMO     1.0  !  2002-08  (G. Madec)  free form + modules
   !!             -   !  2004-01  (A. de Miranda, G. Madec, J.M. Molines ) add advective bbl
   !!            3.3  !  2009-11  (G. Madec)  merge trabbl and trabbl_adv + style + optimization 
   !!             -   !  2010-04  (G. Madec)  Campin & Goosse advective bbl 
   !!             -   !  2010-06  (C. Ethe, G. Madec)  merge TRA-TRC
   !!            4.0  !  2017-04  (G. Madec)  ln_trabbl namelist variable instead of a CPP key
   !!----------------------------------------------------------------------
#if  defined key_top
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   !!    trc_bbl      : update the tracer trends due to the bottom boundary layer (advective and/or diffusive)
   !!----------------------------------------------------------------------
   USE par_trc        ! need jptra, number of passive tracers
   USE oce_trc        ! ocean dynamics and passive tracers variables
   USE trc            ! ocean passive tracers variables
   USE trd_oce        ! trends: ocean variables
   USE trdtra         ! tracer trends
   USE trabbl         ! bottom boundary layer 
   USE prtctl         ! Print control for debbuging

   PUBLIC   trc_bbl   !  routine called by trctrp.F90

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcbbl.F90 14086 2020-12-04 11:37:14Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_bbl( kt, Kbb, Kmm, ptr, Krhs )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE bbl  ***
      !!                   
      !! ** Purpose :   Compute the before tracer (t & s) trend associated 
      !!     with the bottom boundary layer and add it to the general trend
      !!     of tracer equations.
      !!
      !!----------------------------------------------------------------------  
      INTEGER,                                    INTENT( in  ) :: kt              ! ocean time-step 
      INTEGER,                                    INTENT( in  ) :: Kbb, Kmm, Krhs  ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jptra,jpt), INTENT(inout) :: ptr             ! passive tracers and RHS of tracer equation
      INTEGER :: jn                   ! loop index
      CHARACTER (len=22) :: charout
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:) ::   ztrtrd
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_bbl')
      !
      IF( .NOT. l_offline ) THEN
         CALL bbl( kt, nittrc000, 'TRC', Kbb, Kmm )  ! Online coupling with dynamics  : Computation of bbl coef and bbl transport
         l_bbl = .FALSE.                             ! Offline coupling with dynamics : Read bbl coef and bbl transport from input files
      ENDIF

      IF( l_trdtrc )  THEN
         ALLOCATE( ztrtrd(jpi,jpj,jpk,jptra) ) ! temporary save of trends
         ztrtrd(:,:,:,:)  = ptr(:,:,:,:,Krhs)
      ENDIF

      !* Diffusive bbl :
      IF( nn_bbl_ldf == 1 ) THEN
         !
         CALL tra_bbl_dif( ptr(:,:,:,:,Kbb), ptr(:,:,:,:,Krhs), jptra, Kmm )  
         IF( sn_cfctl%l_prttrc )   THEN
            WRITE(charout, FMT="(' bbl_dif')")  ;  CALL prt_ctl_info( charout, cdcomp = 'top' )
            CALL prt_ctl( tab4d_1=ptr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm, clinfo3='trd' )
         ENDIF
         !
      ENDIF

      !* Advective bbl : bbl upstream advective trends added to the tracer trends
      IF( nn_bbl_adv /= 0 ) THEN
         !
         CALL tra_bbl_adv( ptr(:,:,:,:,Kbb), ptr(:,:,:,:,Krhs), jptra, Kmm )  
         IF( sn_cfctl%l_prttrc )   THEN
            WRITE(charout, FMT="(' bbl_adv')")  ;  CALL prt_ctl_info( charout, cdcomp = 'top' )
            CALL prt_ctl( tab4d_1=ptr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm, clinfo3='trd' )
         ENDIF
         !
      ENDIF

      IF( l_trdtrc )   THEN                      ! save the horizontal diffusive trends for further diagnostics
        DO jn = 1, jptra
           ztrtrd(:,:,:,jn) = ptr(:,:,:,jn,Krhs) - ztrtrd(:,:,:,jn)
           CALL trd_tra( kt, Kmm, Krhs, 'TRC', jn, jptra_bbl, ztrtrd(:,:,:,jn) )
        END DO
        DEALLOCATE( ztrtrd ) ! temporary save of trends
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('trc_bbl')
      !
   END SUBROUTINE trc_bbl

#endif

   !!======================================================================
END MODULE trcbbl
