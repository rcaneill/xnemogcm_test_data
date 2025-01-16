MODULE p4zsms
   !!======================================================================
   !!                         ***  MODULE p4zsms  ***
   !! TOP :   PISCES Source Minus Sink manager
   !!======================================================================
   !! History :   1.0  !  2004-03 (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!----------------------------------------------------------------------
   !!   p4z_sms        : Time loop of passive tracers sms
   !!----------------------------------------------------------------------
   USE oce_trc         ! shared variables between ocean and passive tracers
   USE trc             ! passive tracers common variables 
   USE sms_pisces      ! PISCES Source Minus Sink variables
   USE p4zbio          ! Biological model
   USE p4zche          ! Chemical model
   USE p4zlys          ! Calcite saturation
   USE p4zflx          ! Gas exchange
   USE p4zbc           ! External source of nutrients
   USE p4zsed          ! Sedimentation
   USE p4zint          ! time interpolation
   USE p4zrem          ! remineralisation
   USE iom             ! I/O manager
   USE trd_oce         ! Ocean trends variables
   USE trdtrc          ! TOP trends variables
   USE sedmodel        ! Sediment model
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE prtctl          ! print control for debugging

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_sms_init   ! called in trcini_pisces.F90
   PUBLIC   p4z_sms        ! called in trcsms_pisces.F90

   INTEGER ::    numco2, numnut, numnit      ! logical unit for co2 budget
   REAL(wp) ::   alkbudget, no3budget, silbudget, ferbudget, po4budget ! total budget of the different conservative elements
   REAL(wp) ::   xfact, xfact1, xfact2, xfact3

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   xnegtr     ! Array used to indicate negative tracer values

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p4zsms.F90 15459 2021-10-29 08:19:18Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_sms( kt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_sms  ***
      !!
      !! ** Purpose :   Managment of the call to Biological sources and sinks 
      !!              routines of PISCES bio-model
      !!
      !! ** Method  : - calls the various SMS subroutines
      !!              - calls the sediment module (if ln_sediment)  
      !!              - several calls of bio and sed (possible time-splitting)
      !!              - handles the potential negative concentrations (xnegtr)
      !!---------------------------------------------------------------------
      !
      INTEGER, INTENT( in ) ::   kt              ! ocean time-step index      
      INTEGER, INTENT( in ) ::   Kbb, Kmm, Krhs  ! time level index
      !!
      INTEGER ::   ji, jj, jk, jnt, jn, jl
      REAL(wp) ::  ztra
      CHARACTER (len=25) :: charout
      REAL(wp), ALLOCATABLE, DIMENSION(:,:    ) :: zw2d
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:  ) :: zw3d
      REAL(wp), DIMENSION(jpi,jpj,jpk,jp_pisces) :: ztrbbio

      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p4z_sms')
      !
      IF( kt == nittrc000 ) THEN
        !
        ALLOCATE( xnegtr(jpi,jpj,jpk) )
        !
        IF( .NOT. ln_rsttr ) THEN
            CALL p4z_che( Kbb, Kmm )                  ! initialize the chemical constants
            CALL ahini_for_at( hi, Kbb )              !  set PH at kt=nit000
            t_oce_co2_flx_cum = 0._wp
        ELSE
            CALL p4z_rst( nittrc000, Kbb, Kmm,  'READ' )  !* read or initialize all required fields
        ENDIF
        !
      ENDIF
      !
      IF( ln_pisdmp .AND. MOD( kt - 1, nn_pisdmp ) == 0 )   CALL p4z_dmp( kt, Kbb, Kmm )      ! Relaxation of some tracers
      !
      rfact = rDt_trc  ! time step of PISCES
      !
      IF( ( ln_top_euler .AND. kt == nittrc000 )  .OR. ( .NOT.ln_top_euler .AND. kt <= nittrc000 + 1 ) ) THEN
         rfactr  = 1. / rfact  ! inverse of the time step
         rfact2  = rfact / REAL( nrdttrc, wp )  ! time step of the biological SMS
         rfact2r = 1. / rfact2  ! Inverse of the biological time step
         xstep = rfact2 / rday         ! Time step duration for biology relative to a day
         xfact = 1.e+3 * rfact2r
         IF(lwp) WRITE(numout,*) 
         IF(lwp) WRITE(numout,*) '    Passive Tracer  time step    rfact  = ', rfact, ' rn_Dt = ', rn_Dt
         IF(lwp) write(numout,*) '    PISCES  Biology time step    rfact2 = ', rfact2
         IF(lwp) WRITE(numout,*)
      ENDIF

      IF( l_1st_euler .OR. ln_top_euler ) THEN
         DO jn = jp_pcs0, jp_pcs1              !   SMS on tracer without Asselin time-filter
            tr(:,:,:,jn,Kbb) = tr(:,:,:,jn,Kmm)
         END DO
      ENDIF

      DO jn = jp_pcs0, jp_pcs1              !   Store the tracer concentrations before entering PISCES
         ztrbbio(:,:,:,jn) = tr(:,:,:,jn,Kbb)
      END DO

      !
      IF( ll_bc )    CALL p4z_bc( kt, Kbb, Kmm, Krhs )   ! external sources of nutrients 
      !
#if ! defined key_sed_off
      CALL p4z_che(     Kbb, Kmm       ) ! computation of chemical constants
      CALL p4z_int( kt, Kbb, Kmm       ) ! computation of various rates for biogeochemistry
      !
      IF( nn_hls > 1 ) CALL lbc_lnk( 'p4zsms', hmld(:,:), 'T', 1._wp )  ! hmld defined only on first halo in zdfmxl
      !
      DO jnt = 1, nrdttrc          ! Potential time splitting if requested
         !
         CALL p4z_bio( kt, jnt, Kbb, Kmm, Krhs )   ! Biology
         CALL p4z_lys( kt, jnt, Kbb,      Krhs )   ! Compute CaCO3 saturation
         CALL p4z_sed( kt, jnt, Kbb, Kmm, Krhs )   ! Surface and Bottom boundary conditions
         CALL p4z_flx( kt, jnt, Kbb, Kmm, Krhs )   ! Compute surface fluxes
         !
         ! Handling of the negative concentrations
         ! The biological SMS may generate negative concentrations
         ! Trends are tested at each grid cell. If a negative concentrations 
         ! is created at a grid cell, all the sources and sinks at that grid 
         ! cell are scale to avoid that negative concentration. This approach 
         ! is quite simplistic but it conserves mass.
         ! ------------------------------------------------------------------
         xnegtr(:,:,:) = 1.e0
         DO jn = jp_pcs0, jp_pcs1
            DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpk)
               IF( ( tr(ji,jj,jk,jn,Kbb) + tr(ji,jj,jk,jn,Krhs) ) < 0.e0 ) THEN
                  ztra             = ABS( tr(ji,jj,jk,jn,Kbb) ) / ( ABS( tr(ji,jj,jk,jn,Krhs) ) + rtrn )
                  xnegtr(ji,jj,jk) = MIN( xnegtr(ji,jj,jk),  ztra )
               ENDIF
            END_3D
         END DO
         !                                ! where at least 1 tracer concentration becomes negative
         !                                ! 
         ! Concentrations are updated
         DO jn = jp_pcs0, jp_pcs1
           tr(:,:,:,jn,Kbb) = tr(:,:,:,jn,Kbb) + xnegtr(:,:,:) * tr(:,:,:,jn,Krhs)
         END DO
        !
        IF(  iom_use( 'INTdtAlk' ) .OR. iom_use( 'INTdtDIC' ) .OR. iom_use( 'INTdtFer' ) .OR.  &
          &  iom_use( 'INTdtDIN' ) .OR. iom_use( 'INTdtDIP' ) .OR. iom_use( 'INTdtSil' ) )  THEN
          !
          ALLOCATE( zw3d(jpi,jpj,jpk), zw2d(jpi,jpj) )
          zw3d(:,:,jpk) = 0.
          DO jk = 1, jpkm1
              zw3d(:,:,jk) = xnegtr(:,:,jk) * xfact * e3t(:,:,jk,Kmm) * tmask(:,:,jk)
          ENDDO
          !
          zw2d(:,:) = 0.
          DO jk = 1, jpkm1
             zw2d(:,:) = zw2d(:,:) + zw3d(:,:,jk) * tr(:,:,jk,jptal,Krhs)
          ENDDO
          CALL iom_put( 'INTdtAlk', zw2d )
          !
          zw2d(:,:) = 0.
          DO jk = 1, jpkm1
             zw2d(:,:) = zw2d(:,:) + zw3d(:,:,jk) * tr(:,:,jk,jpdic,Krhs)
          ENDDO
          CALL iom_put( 'INTdtDIC', zw2d )
          !
          zw2d(:,:) = 0.
          DO jk = 1, jpkm1
             zw2d(:,:) = zw2d(:,:) + zw3d(:,:,jk) * rno3 * ( tr(:,:,jk,jpno3,Krhs) + tr(:,:,jk,jpnh4,Krhs) )
          ENDDO
          CALL iom_put( 'INTdtDIN', zw2d )
          !
          zw2d(:,:) = 0.
          DO jk = 1, jpkm1
             zw2d(:,:) = zw2d(:,:) + zw3d(:,:,jk) * po4r * tr(:,:,jk,jppo4,Krhs)
          ENDDO
          CALL iom_put( 'INTdtDIP', zw2d )
          !
          zw2d(:,:) = 0.
          DO jk = 1, jpkm1
             zw2d(:,:) = zw2d(:,:) + zw3d(:,:,jk) * tr(:,:,jk,jpfer,Krhs)
          ENDDO
          CALL iom_put( 'INTdtFer', zw2d )
          !
          zw2d(:,:) = 0.
          DO jk = 1, jpkm1
             zw2d(:,:) = zw2d(:,:) + zw3d(:,:,jk) * tr(:,:,jk,jpsil,Krhs)
          ENDDO
          CALL iom_put( 'INTdtSil', zw2d )
          !
          DEALLOCATE( zw3d, zw2d )
        ENDIF
        !
        ! Trends are are reset to 0
         DO jn = jp_pcs0, jp_pcs1
            tr(:,:,:,jn,Krhs) = 0._wp
         END DO
         !
      END DO
      !
#endif
      !
      ! If ln_sediment is set to .true. then the sediment module is called
      IF( ln_sediment ) THEN 
         !
         CALL sed_model( kt, Kbb, Kmm, Krhs )     !  Main program of Sediment model
         !
      ENDIF
      !
      DO jn = jp_pcs0, jp_pcs1
         tr(:,:,:,jn,Krhs) = ( tr(:,:,:,jn,Kbb) - ztrbbio(:,:,:,jn) ) * rfactr
         tr(:,:,:,jn,Kbb ) = ztrbbio(:,:,:,jn)
         ztrbbio(:,:,:,jn) = 0._wp
      END DO
      !
      !
      IF( l_trdtrc ) THEN
         DO jn = jp_pcs0, jp_pcs1
           CALL trd_trc( tr(:,:,:,jn,Krhs), jn, jptra_sms, kt, Kmm )   ! save trends
         END DO
      END IF
      !  
      IF( lrst_trc )  CALL p4z_rst( kt, Kbb, Kmm,  'WRITE' )           !* Write PISCES informations in restart file 
      !

      IF( lk_iomput .OR. ln_check_mass )  CALL p4z_chk_mass( kt, Kmm ) ! Mass conservation checking

      IF( lwm .AND. kt == nittrc000    )  CALL FLUSH( numonp )         ! flush output namelist PISCES
      !
      IF( ln_timing )  CALL timing_stop('p4z_sms')
      !
   END SUBROUTINE p4z_sms


   SUBROUTINE p4z_sms_init
      !!----------------------------------------------------------------------
      !!                     ***  p4z_sms_init  ***  
      !!
      !! ** Purpose :   read the general PISCES namelist
      !!
      !! ** input   :   file 'namelist_pisces' containing the following
      !!                namelist: nampisbio, nampisdmp, nampismass 
      !!----------------------------------------------------------------------
      INTEGER :: ios                 ! Local integer output status for namelist read
      !!
      NAMELIST/nampisbio/ nrdttrc, wsbio, xkmort, feratz, feratm, wsbio2, wsbio2max,    &
         &                wsbio2scale, ldocp, ldocz, lthet, no3rat3, po4rat3
         !
      NAMELIST/nampisdmp/ ln_pisdmp, nn_pisdmp
      NAMELIST/nampismass/ ln_check_mass
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'p4z_sms_init : PISCES initialization'
         WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF

      READ  ( numnatp_ref, nampisbio, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'nampisbio in reference namelist' )
      READ  ( numnatp_cfg, nampisbio, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'nampisbio in configuration namelist' )
      IF(lwm) WRITE( numonp, nampisbio )
      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*) '   Namelist : nampisbio'
         WRITE(numout,*) '      frequency for the biology                 nrdttrc     =', nrdttrc
         WRITE(numout,*) '      POC sinking speed                         wsbio       =', wsbio
         WRITE(numout,*) '      half saturation constant for mortality    xkmort      =', xkmort 
         IF( ln_p5z ) THEN
            WRITE(numout,*) '      N/C in zooplankton                     no3rat3     =', no3rat3
            WRITE(numout,*) '      P/C in zooplankton                     po4rat3     =', po4rat3
         ENDIF
         WRITE(numout,*) '      Fe/C in microzooplankton                  feratz      =', feratz
         WRITE(numout,*) '      Fe/C in microzooplankton                  feratz      =', feratm
         WRITE(numout,*) '      Big particles sinking speed               wsbio2      =', wsbio2
         WRITE(numout,*) '      Big particles maximum sinking speed       wsbio2max   =', wsbio2max
         WRITE(numout,*) '      Big particles sinking speed length scale  wsbio2scale =', wsbio2scale
         IF( ln_ligand ) THEN
            IF( ln_p4z ) THEN
               WRITE(numout,*) '      Phyto ligand production per unit doc           ldocp  =', ldocp
               WRITE(numout,*) '      Zoo ligand production per unit doc             ldocz  =', ldocz
               WRITE(numout,*) '      Proportional loss of ligands due to Fe uptake  lthet  =', lthet
            ENDIF
         ENDIF
      ENDIF


      READ  ( numnatp_ref, nampisdmp, IOSTAT = ios, ERR = 905)
905   IF( ios /= 0 )   CALL ctl_nam ( ios , 'nampisdmp in reference namelist' )
      READ  ( numnatp_cfg, nampisdmp, IOSTAT = ios, ERR = 906 )
906   IF( ios >  0 )   CALL ctl_nam ( ios , 'nampisdmp in configuration namelist' )
      IF(lwm) WRITE( numonp, nampisdmp )
      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*)
         WRITE(numout,*) '   Namelist : nampisdmp --- relaxation to GLODAP'
         WRITE(numout,*) '      Relaxation of tracer to glodap mean value   ln_pisdmp =', ln_pisdmp
         WRITE(numout,*) '      Frequency of Relaxation                     nn_pisdmp =', nn_pisdmp
      ENDIF

      READ  ( numnatp_ref, nampismass, IOSTAT = ios, ERR = 907)
907   IF( ios /= 0 )   CALL ctl_nam ( ios , 'nampismass in reference namelist' )
      READ  ( numnatp_cfg, nampismass, IOSTAT = ios, ERR = 908 )
908   IF( ios >  0 )   CALL ctl_nam ( ios , 'nampismass in configuration namelist' )
      IF(lwm) WRITE( numonp, nampismass )

      IF(lwp) THEN                         ! control print
         WRITE(numout,*)
         WRITE(numout,*) '   Namelist : nampismass  --- mass conservation checking'
         WRITE(numout,*) '      Flag to check mass conservation of NO3/Si/TALK   ln_check_mass = ', ln_check_mass
      ENDIF
      !
   END SUBROUTINE p4z_sms_init


   SUBROUTINE p4z_rst( kt, Kbb, Kmm, cdrw )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE p4z_rst  ***
      !!
      !!  ** Purpose : Read or write specific PISCES variables in restart file:
      !!
      !!  WRITE(READ) mode:
      !!       kt        : number of time step since the begining of the experiment at the
      !!                   end of the current(previous) run
      !!---------------------------------------------------------------------
      INTEGER         , INTENT(in) ::   kt         ! ocean time-step
      INTEGER         , INTENT(in) ::   Kbb, Kmm   ! time level indices
      CHARACTER(len=*), INTENT(in) ::   cdrw       ! "READ"/"WRITE" flag
      !!---------------------------------------------------------------------
      !
      IF( TRIM(cdrw) == 'READ' ) THEN
         !
         ! Read the specific variable of PISCES
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) ' p4z_rst : Read specific variables from pisces model '
         IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~~~~'
         ! 
         ! Read the pH. If not in the restart file, then it is initialized from
         ! the initial conditions
         IF( iom_varid( numrtr, 'PH', ldstop = .FALSE. ) > 0 ) THEN
            CALL iom_get( numrtr, jpdom_auto, 'PH' , hi(:,:,:)  )
         ELSE
            CALL p4z_che( Kbb, Kmm )                  ! initialize the chemical constants
            CALL ahini_for_at( hi, Kbb )
         ENDIF
         CALL iom_get( numrtr, jpdom_auto, 'Silicalim', xksi(:,:) )

         ! Read the Si half saturation constant and the maximum Silica concentration
         IF( iom_varid( numrtr, 'Silicamax', ldstop = .FALSE. ) > 0 ) THEN
            CALL iom_get( numrtr, jpdom_auto, 'Silicamax' , xksimax(:,:)  )
         ELSE
            xksimax(:,:) = xksi(:,:)
         ENDIF

         ! Read the Fe3 consumption term by phytoplankton
         IF( iom_varid( numrtr, 'Consfe3', ldstop = .FALSE. ) > 0 ) THEN
            CALL iom_get( numrtr, jpdom_auto, 'Consfe3' , consfe3(:,:,:)  )
         ELSE
            consfe3(:,:,:) = 0._wp
         ENDIF


         ! Read the cumulative total flux. If not in the restart file, it is set to 0          
         IF( iom_varid( numrtr, 'tcflxcum', ldstop = .FALSE. ) > 0 ) THEN  ! cumulative total flux of carbon
            CALL iom_get( numrtr, 'tcflxcum' , t_oce_co2_flx_cum  )
         ELSE
            t_oce_co2_flx_cum = 0._wp
         ENDIF
         !
         ! PISCES size proxy
         IF( iom_varid( numrtr, 'sized', ldstop = .FALSE. ) > 0 ) THEN
            CALL iom_get( numrtr, jpdom_auto, 'sized' , sized(:,:,:)  )
            sized(:,:,:) = MAX( 1.0, sized(:,:,:) )
         ELSE
            sized(:,:,:) = 1.
         ENDIF
         !
         IF( iom_varid( numrtr, 'sizen', ldstop = .FALSE. ) > 0 ) THEN
            CALL iom_get( numrtr, jpdom_auto, 'sizen' , sizen(:,:,:)  )
            sizen(:,:,:) = MAX( 1.0, sizen(:,:,:) )
         ELSE
            sizen(:,:,:) = 1.
         ENDIF

         ! PISCES-QUOTA specific part
         IF( ln_p5z ) THEN
            ! Read the size of the different phytoplankton groups
            ! If not in the restart file, they are set to 1
            IF( iom_varid( numrtr, 'sizep', ldstop = .FALSE. ) > 0 ) THEN
               CALL iom_get( numrtr, jpdom_auto, 'sizep' , sizep(:,:,:)  )
               sizep(:,:,:) = MAX( 1.0, sizep(:,:,:) )
            ELSE
               sizep(:,:,:) = 1.
            ENDIF
        ENDIF
        !
      ELSEIF( TRIM(cdrw) == 'WRITE' ) THEN
         ! write the specific variables of PISCES
         IF( kt == nitrst ) THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'p4z_rst : write pisces restart file  kt =', kt
            IF(lwp) WRITE(numout,*) '~~~~~~~'
         ENDIF
         CALL iom_rstput( kt, nitrst, numrtw, 'PH', hi(:,:,:)           )
         CALL iom_rstput( kt, nitrst, numrtw, 'Silicalim', xksi(:,:)    )
         CALL iom_rstput( kt, nitrst, numrtw, 'Silicamax', xksimax(:,:) )
         CALL iom_rstput( kt, nitrst, numrtw, 'tcflxcum', t_oce_co2_flx_cum )
         CALL iom_rstput( kt, nitrst, numrtw, 'Consfe3', consfe3(:,:,:) ) ! Si max concentration
         CALL iom_rstput( kt, nitrst, numrtw, 'tcflxcum', t_oce_co2_flx_cum ) ! Cumulative CO2 flux
         CALL iom_rstput( kt, nitrst, numrtw, 'sizen', sizen(:,:,:) )  ! Size of nanophytoplankton
         CALL iom_rstput( kt, nitrst, numrtw, 'sized', sized(:,:,:) )  ! Size of diatoms
         IF( ln_p5z ) CALL iom_rstput( kt, nitrst, numrtw, 'sizep', sizep(:,:,:) )  ! Size of picophytoplankton
      ENDIF
      !
   END SUBROUTINE p4z_rst


   SUBROUTINE p4z_dmp( kt, Kbb, Kmm )
      !!----------------------------------------------------------------------
      !!                    ***  p4z_dmp  ***
      !!
      !! ** purpose  : Relaxation of the total budget of some elements
      !!               This routine avoids the model to drift far from the 
      !!               observed content in various elements
      !!               Elements that may be relaxed : Alk, P, N, Si
      !!----------------------------------------------------------------------
      !
      INTEGER, INTENT( in )  ::     kt            ! time step
      INTEGER, INTENT( in )  ::     Kbb, Kmm      ! time level indices
      !
      REAL(wp) ::  alkmean = 2426.     ! mean value of alkalinity ( Glodap ; for Goyet 2391. )
      REAL(wp) ::  po4mean = 2.174     ! mean value of phosphate
      REAL(wp) ::  no3mean = 31.00     ! mean value of nitrate
      REAL(wp) ::  silmean = 90.33     ! mean value of silicate
      !
      REAL(wp) :: zarea, zalksumn, zpo4sumn, zno3sumn, zsilsumn
      REAL(wp) :: zalksumb, zpo4sumb, zno3sumb, zsilsumb
      !!---------------------------------------------------------------------

      IF(lwp)  WRITE(numout,*)
      IF(lwp)  WRITE(numout,*) ' p4z_dmp : Restoring of nutrients at time-step kt = ', kt
      IF(lwp)  WRITE(numout,*)

      IF( cn_cfg == "ORCA" .OR. cn_cfg == "orca") THEN
         IF( .NOT. ln_c1d ) THEN      ! ORCA configuration (not 1D) !
            !                                                ! --------------------------- !
            ! set total alkalinity, phosphate, nitrate & silicate
            zarea          = 1._wp / glob_sum( 'p4zsms', cvol(:,:,:) ) * 1e6              

            zalksumn = glob_sum( 'p4zsms', tr(:,:,:,jptal,Kmm) * cvol(:,:,:)  ) * zarea
            zpo4sumn = glob_sum( 'p4zsms', tr(:,:,:,jppo4,Kmm) * cvol(:,:,:)  ) * zarea * po4r
            zno3sumn = glob_sum( 'p4zsms', tr(:,:,:,jpno3,Kmm) * cvol(:,:,:)  ) * zarea * rno3
            zsilsumn = glob_sum( 'p4zsms', tr(:,:,:,jpsil,Kmm) * cvol(:,:,:)  ) * zarea
 
            ! Correct the trn mean content of alkalinity
            IF(lwp) WRITE(numout,*) '       TALKN mean : ', zalksumn
            tr(:,:,:,jptal,Kmm) = tr(:,:,:,jptal,Kmm) * alkmean / zalksumn

            ! Correct the trn mean content of PO4
            IF(lwp) WRITE(numout,*) '       PO4N  mean : ', zpo4sumn
            tr(:,:,:,jppo4,Kmm) = tr(:,:,:,jppo4,Kmm) * po4mean / zpo4sumn

            ! Correct the trn mean content of NO3
            IF(lwp) WRITE(numout,*) '       NO3N  mean : ', zno3sumn
            tr(:,:,:,jpno3,Kmm) = tr(:,:,:,jpno3,Kmm) * no3mean / zno3sumn

            ! Correct the trn mean content of SiO3
            IF(lwp) WRITE(numout,*) '       SiO3N mean : ', zsilsumn
            tr(:,:,:,jpsil,Kmm) = MIN( 400.e-6,tr(:,:,:,jpsil,Kmm) * silmean / zsilsumn )
            !
            !
            IF( .NOT. ln_top_euler ) THEN
               zalksumb = glob_sum( 'p4zsms', tr(:,:,:,jptal,Kbb) * cvol(:,:,:)  ) * zarea
               zpo4sumb = glob_sum( 'p4zsms', tr(:,:,:,jppo4,Kbb) * cvol(:,:,:)  ) * zarea * po4r
               zno3sumb = glob_sum( 'p4zsms', tr(:,:,:,jpno3,Kbb) * cvol(:,:,:)  ) * zarea * rno3
               zsilsumb = glob_sum( 'p4zsms', tr(:,:,:,jpsil,Kbb) * cvol(:,:,:)  ) * zarea
 
               IF(lwp) WRITE(numout,*) ' '
               ! Correct the trb mean content of alkalinity
               IF(lwp) WRITE(numout,*) '       TALKB mean : ', zalksumb
               tr(:,:,:,jptal,Kbb) = tr(:,:,:,jptal,Kbb) * alkmean / zalksumb

               ! Correct the trb mean content of PO4
               IF(lwp) WRITE(numout,*) '       PO4B  mean : ', zpo4sumb
               tr(:,:,:,jppo4,Kbb) = tr(:,:,:,jppo4,Kbb) * po4mean / zpo4sumb

               ! Correct the trb mean content of NO3
               IF(lwp) WRITE(numout,*) '       NO3B  mean : ', zno3sumb
               tr(:,:,:,jpno3,Kbb) = tr(:,:,:,jpno3,Kbb) * no3mean / zno3sumb

               ! Correct the trb mean content of SiO3
               IF(lwp) WRITE(numout,*) '       SiO3B mean : ', zsilsumb
               tr(:,:,:,jpsil,Kbb) = MIN( 400.e-6,tr(:,:,:,jpsil,Kbb) * silmean / zsilsumb )
           ENDIF
        ENDIF
        !
      ENDIF
        !
   END SUBROUTINE p4z_dmp


   SUBROUTINE p4z_chk_mass( kt, Kmm )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_chk_mass  ***
      !!
      !! ** Purpose :  Mass conservation check 
      !!
      !!---------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt      ! ocean time-step index      
      INTEGER, INTENT( in ) ::   Kmm     ! time level indices
      REAL(wp)             ::  zrdenittot, zsdenittot, znitrpottot
      CHARACTER(LEN=100)   ::   cltxt
      INTEGER :: jk
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zwork
      !!----------------------------------------------------------------------
      !
      IF( kt == nittrc000 ) THEN 
         xfact1 = rfact2r * 12. / 1.e15 * ryyss    ! conversion molC/kt --> PgC/yr
         xfact2 = 1.e+3 * rno3 * 14. / 1.e12 * ryyss   ! conversion molC/l/s ----> TgN/m3/yr
         xfact3 = 1.e+3 * rfact2r * rno3   ! conversion molC/l/kt ----> molN/m3/s
         IF( ln_check_mass .AND. lwp) THEN      !   Open budget file of NO3, ALK, Si, Fer
            CALL ctl_opn( numco2, 'carbon.budget'  , 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, 6, .FALSE., narea )
            CALL ctl_opn( numnut, 'nutrient.budget', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, 6, .FALSE., narea )
            CALL ctl_opn( numnit, 'nitrogen.budget', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, 6, .FALSE., narea )
            cltxt='time-step   Alkalinity        Nitrate        Phosphorus         Silicate           Iron'
            IF( lwp ) WRITE(numnut,*)  TRIM(cltxt)
            IF( lwp ) WRITE(numnut,*) 
         ENDIF
      ENDIF

      ! Compute the budget of NO3
      IF( iom_use( "pno3tot" ) .OR. ( ln_check_mass .AND. kt == nitend )  ) THEN
         IF( ln_p4z ) THEN
            zwork(:,:,:) =    tr(:,:,:,jpno3,Kmm) + tr(:,:,:,jpnh4,Kmm)                      &
               &          +   tr(:,:,:,jpphy,Kmm) + tr(:,:,:,jpdia,Kmm)                      &
               &          +   tr(:,:,:,jppoc,Kmm) + tr(:,:,:,jpgoc,Kmm)  + tr(:,:,:,jpdoc,Kmm)  &        
               &          +   tr(:,:,:,jpzoo,Kmm) + tr(:,:,:,jpmes,Kmm) 
        ELSE
            zwork(:,:,:) =    tr(:,:,:,jpno3,Kmm) + tr(:,:,:,jpnh4,Kmm) + tr(:,:,:,jpnph,Kmm)   &
               &          +   tr(:,:,:,jpndi,Kmm) + tr(:,:,:,jpnpi,Kmm)                      & 
               &          +   tr(:,:,:,jppon,Kmm) + tr(:,:,:,jpgon,Kmm) + tr(:,:,:,jpdon,Kmm)   &
               &          + ( tr(:,:,:,jpzoo,Kmm) + tr(:,:,:,jpmes,Kmm) ) * no3rat3 
        ENDIF
        !
        no3budget = glob_sum( 'p4zsms', zwork(:,:,:) * cvol(:,:,:)  )  
        no3budget = no3budget / areatot
        CALL iom_put( "pno3tot", no3budget )
      ENDIF
      !
      ! Compute the budget of PO4
      IF( iom_use( "ppo4tot" ) .OR. ( ln_check_mass .AND. kt == nitend )  ) THEN
         IF( ln_p4z ) THEN
            zwork(:,:,:) =    tr(:,:,:,jppo4,Kmm)                                         &
               &          +   tr(:,:,:,jpphy,Kmm) + tr(:,:,:,jpdia,Kmm)                      &
               &          +   tr(:,:,:,jppoc,Kmm) + tr(:,:,:,jpgoc,Kmm)  + tr(:,:,:,jpdoc,Kmm)  &        
               &          +   tr(:,:,:,jpzoo,Kmm) + tr(:,:,:,jpmes,Kmm) 
        ELSE
            zwork(:,:,:) =    tr(:,:,:,jppo4,Kmm) + tr(:,:,:,jppph,Kmm)                      &
               &          +   tr(:,:,:,jppdi,Kmm) + tr(:,:,:,jpppi,Kmm)                      & 
               &          +   tr(:,:,:,jppop,Kmm) + tr(:,:,:,jpgop,Kmm) + tr(:,:,:,jpdop,Kmm)   &
               &          + ( tr(:,:,:,jpzoo,Kmm) + tr(:,:,:,jpmes,Kmm) ) * po4rat3 
        ENDIF
        !
        po4budget = glob_sum( 'p4zsms', zwork(:,:,:) * cvol(:,:,:)  )  
        po4budget = po4budget / areatot
        CALL iom_put( "ppo4tot", po4budget )
      ENDIF
      !
      ! Compute the budget of SiO3
      IF( iom_use( "psiltot" ) .OR. ( ln_check_mass .AND. kt == nitend )  ) THEN
         zwork(:,:,:) =  tr(:,:,:,jpsil,Kmm) + tr(:,:,:,jpgsi,Kmm) + tr(:,:,:,jpdsi,Kmm) 
         !
         silbudget = glob_sum( 'p4zsms', zwork(:,:,:) * cvol(:,:,:)  )  
         silbudget = silbudget / areatot
         CALL iom_put( "psiltot", silbudget )
      ENDIF
      !
      IF( iom_use( "palktot" ) .OR. ( ln_check_mass .AND. kt == nitend )  ) THEN
         zwork(:,:,:) =  tr(:,:,:,jpno3,Kmm) * rno3 + tr(:,:,:,jptal,Kmm) + tr(:,:,:,jpcal,Kmm) * 2.              
         !
         alkbudget = glob_sum( 'p4zsms', zwork(:,:,:) * cvol(:,:,:)  )         !
         alkbudget = alkbudget / areatot
         CALL iom_put( "palktot", alkbudget )
      ENDIF
      !
      ! Compute the budget of Iron
      IF( iom_use( "pfertot" ) .OR. ( ln_check_mass .AND. kt == nitend )  ) THEN
         zwork(:,:,:) =   tr(:,:,:,jpfer,Kmm) + tr(:,:,:,jpnfe,Kmm) + tr(:,:,:,jpdfe,Kmm)   &
            &         +   tr(:,:,:,jpbfe,Kmm) + tr(:,:,:,jpsfe,Kmm)                      &
            &         + ( tr(:,:,:,jpzoo,Kmm) * feratz + tr(:,:,:,jpmes,Kmm) ) * feratm   
         !
         ferbudget = glob_sum( 'p4zsms', zwork(:,:,:) * cvol(:,:,:)  )  
         ferbudget = ferbudget / areatot
         CALL iom_put( "pfertot", ferbudget )
      ENDIF
      !
      ! Global budget of N SMS : denitrification in the water column and in the sediment
      !                          nitrogen fixation by the diazotrophs
      ! --------------------------------------------------------------------------------
      IF( iom_use( "tnfix" ) .OR.  ( ln_check_mass .AND. kt == nitend )  ) THEN
         znitrpottot  = glob_sum ( 'p4zsms', nitrpot(:,:,:) * nitrfix * cvol(:,:,:) )
         CALL iom_put( "tnfix"  , znitrpottot * xfact3 )  ! Global  nitrogen fixation molC/l  to molN/m3 
      ENDIF
      !
      IF( iom_use( "tdenit" ) .OR.  ( ln_check_mass .AND. kt == nitend )  ) THEN
         zrdenittot = glob_sum ( 'p4zsms', denitr(:,:,:) * rdenit * xnegtr(:,:,:) * cvol(:,:,:) )
         zsdenittot = glob_sum ( 'p4zsms', sdenit(:,:) * e1e2t(:,:) * tmask(:,:,1) )
         CALL iom_put( "tdenit" , ( zrdenittot + zsdenittot ) * xfact3 )  ! Total denitrification molC/l to molN/m3 
      ENDIF
      !
      IF( ln_check_mass .AND. kt == nitend ) THEN   ! Compute the budget of NO3, ALK, Si, Fer
         t_atm_co2_flx  = t_atm_co2_flx / glob_sum( 'p4zsms', e1e2t(:,:) )
         t_oce_co2_flx  = t_oce_co2_flx         * xfact1 * (-1 )
         tpp            = tpp           * 1000. * xfact1
         t_oce_co2_exp  = t_oce_co2_exp * 1000. * xfact1
         IF( lwp ) WRITE(numco2,9000) ndastp, t_atm_co2_flx, t_oce_co2_flx, tpp, t_oce_co2_exp
         IF( lwp ) WRITE(numnut,9100) ndastp, alkbudget        * 1.e+06, &
             &                                no3budget * rno3 * 1.e+06, &
             &                                po4budget * po4r * 1.e+06, &
             &                                silbudget        * 1.e+06, &
             &                                ferbudget        * 1.e+09
         !
         IF( lwp ) WRITE(numnit,9200) ndastp, znitrpottot * xfact2  , &
            &                             zrdenittot  * xfact2  , &
            &                             zsdenittot  * xfact2
      ENDIF
      !
 9000  FORMAT(i8,f10.5,e18.10,f10.5,f10.5)
 9100  FORMAT(i8,5e18.10)
 9200  FORMAT(i8,3f10.5)
       !
   END SUBROUTINE p4z_chk_mass

   !!======================================================================
END MODULE p4zsms 
