MODULE p4zbc
   !!======================================================================
   !!                         ***  MODULE p4sbc  ***
   !! TOP :   PISCES surface boundary conditions of external inputs of nutrients
   !!======================================================================
   !! History :   3.5  !  2012-07 (O. Aumont, C. Ethe) Original code
   !!----------------------------------------------------------------------
   !!   p4z_bc        :  Read and interpolate time-varying nutrients fluxes
   !!   p4z_bc_init   :  Initialization of p4z_bc
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE iom             !  I/O manager
   USE fldread         !  time interpolation
   USE trcbc

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_bc
   PUBLIC   p4z_bc_init   

   LOGICAL , PUBLIC ::   ln_ironsed   !: boolean for Fe input from sediments
   LOGICAL , PUBLIC ::   ln_hydrofe   !: boolean for Fe input from hydrothermal vents
   REAL(wp), PUBLIC ::   sedfeinput   !: Coastal release of Iron
   REAL(wp), PUBLIC ::   icefeinput   !: Iron concentration in sea ice
   REAL(wp), PUBLIC ::   wdust        !: Sinking speed of the dust 
   REAL(wp), PUBLIC ::   mfrac        !: Mineral Content of the dust
   REAL(wp)         ::   hratio       !: Fe:3He ratio assumed for vent iron supply
   REAL(wp)         ::   distcoast    !: Distance off the coast for Iron from sediments
   REAL(wp), PUBLIC ::   lgw_rath     !: Weak ligand ratio from hydro sources

   LOGICAL , PUBLIC ::   ll_bc
   LOGICAL , PUBLIC ::   ll_dust      !: boolean for dust input from the atmosphere
   LOGICAL , PUBLIC ::   ll_river     !: boolean for river input of nutrients
   LOGICAL , PUBLIC ::   ll_ndepo     !: boolean for atmospheric deposition of N
   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf_dust      ! structure of input dust
   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf_ironsed   ! structure of input iron from sediment
   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf_hydrofe   ! structure of input iron from sediment

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   dust    !: dust fields
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   ironsed          !: Coastal supply of iron
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   hydrofe          !: Hydrothermal vent supply of iron

   REAL(wp), PUBLIC :: sedsilfrac, sedcalfrac

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p4zbc.F90 10869 2019-04-15 10:34:03Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_bc( kt, Kbb, Kmm, Krhs )
      !!----------------------------------------------------------------------
      !!                  ***  routine p4z_bc  ***
      !!
      !! ** purpose :   read and interpolate the external sources of nutrients
      !!
      !! ** method  :   read the files and interpolate the appropriate variables
      !!
      !! ** input   :   external netcdf files
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt              ! ocean time step
      INTEGER, INTENT(in) ::   Kbb, Kmm, Krhs  ! time level index
      !
      INTEGER  ::  ji, jj, jk, jl 
      REAL(wp) ::  zdep, zwflux, zironice
      REAL(wp) ::  zcoef, zwdust, zrivdin, zdustdep, zndep
      !
      CHARACTER (len=25) :: charout
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p4z_bc')
      
      ! Add the external input of nutrients from dust deposition in the water column
      ! The inputs at surface have already been added
      ! ----------------------------------------------------------
      IF( ll_dust )  THEN
         !
         CALL fld_read( kt, 1, sf_dust )
         dust(:,:) = MAX( rtrn, sf_dust(1)%fnow(:,:,1) )
         !
         ! Iron solubilization of particles in the water column
         ! dust in kg/m2/s ---> 1/55.85 to put in mol/Fe ;  wdust in m/d
         ! Dust are supposed to sink at wdust sinking speed. 3% of the iron 
         ! in dust is hypothesized to be soluble at a dissolution rate set to 
         ! 1/(250 days). The vertical distribution of iron in dust is computed 
         ! from a steady state assumption. Parameters are very uncertain and 
         ! are estimated from the literature quoted in Raiswell et al. (2011) 
         ! ------------------------------------------------------------------- 

         zwdust = 0.03 / ( wdust / rday ) / ( 250. * rday )

         ! Atmospheric input of Iron dissolves in the water column
         IF ( ln_trc_sbc(jpfer) ) THEN
            DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 2, jpkm1 )
               zdustdep = dust(ji,jj) * zwdust * rfact * EXP( -gdept(ji,jj,jk,Kmm) /( 250. * wdust ) )
               !
               tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) + zdustdep * mfrac / mMass_Fe 
            END_3D

            IF( lk_iomput ) THEN
                ! surface downward dust depo of iron
                jl = n_trc_indsbc(jpfer)
                CALL iom_put( "Irondep", ( rf_trsfac(jl) * sf_trcsbc(jl)%fnow(:,:,1) / rn_sbc_time ) * 1.e+3 * tmask(:,:,1) )

            ENDIF

         ENDIF

         ! Atmospheric input of PO4 dissolves in the water column
         IF ( ln_trc_sbc(jppo4) ) THEN
            DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 2, jpkm1 )
               zdustdep = dust(ji,jj) * zwdust * rfact * EXP( -gdept(ji,jj,jk,Kmm) /( 250. * wdust ) )
               !
               tr(ji,jj,jk,jppo4,Krhs) = tr(ji,jj,jk,jppo4,Krhs) + zdustdep * 1.e-3 / mMass_P
            END_3D
         ENDIF

         ! Atmospheric input of Si dissolves in the water column
         IF ( ln_trc_sbc(jpsil) ) THEN
            DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 2, jpkm1 )
               zdustdep = dust(ji,jj) * zwdust * rfact * EXP( -gdept(ji,jj,jk,Kmm) /( 250. * wdust ) )
               !
               tr(ji,jj,jk,jpsil,Krhs) = tr(ji,jj,jk,jpsil,Krhs) + zdustdep * 0.269 / mMass_Si
            END_3D
         ENDIF

         !
         IF( lk_iomput ) THEN
             ! dust concentration at surface
             CALL iom_put( "pdust"  , dust(:,:) / ( wdust / rday ) * tmask(:,:,1) ) ! dust concentration at surface
         ENDIF
      ENDIF

      ! -----------------------------------------
      ! Add the external input of nutrients from river
      ! ----------------------------------------------------------
      IF( ll_river ) THEN
          jl = n_trc_indcbc(jpno3)
          DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
             DO jk = 1, nk_rnf(ji,jj)
                zcoef = rn_rfact / ( e1e2t(ji,jj) * h_rnf(ji,jj) * rn_cbc_time ) * tmask(ji,jj,1)
                zrivdin = rf_trcfac(jl) * sf_trccbc(jl)%fnow(ji,jj,1) * zcoef
                tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) - rno3 * zrivdin * rfact
            ENDDO
          END_2D
      ENDIF
      
      ! Add the external input of nutrients from nitrogen deposition
      ! ----------------------------------------------------------
      IF( ll_ndepo ) THEN
         IF( ln_trc_sbc(jpno3) ) THEN
            jl = n_trc_indsbc(jpno3)
            DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
               zndep = rf_trsfac(jl) * sf_trcsbc(jl)%fnow(ji,jj,1) / e3t(ji,jj,1,Kmm) / rn_sbc_time
               tr(ji,jj,1,jptal,Krhs) = tr(ji,jj,1,jptal,Krhs) - rno3 * zndep * rfact
            END_2D
         ENDIF
         IF( ln_trc_sbc(jpnh4) ) THEN
            jl = n_trc_indsbc(jpnh4)
            DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
               zndep = rf_trsfac(jl) * sf_trcsbc(jl)%fnow(ji,jj,1) / e3t(ji,jj,1,Kmm) / rn_sbc_time
               tr(ji,jj,1,jptal,Krhs) = tr(ji,jj,1,jptal,Krhs) + rno3 * zndep * rfact
            END_2D
         ENDIF
      ENDIF
      !
      ! Iron input/uptake due to sea ice : Crude parameterization based on 
      ! Lancelot et al. Iron concentration in sea-ice is constant and set 
      ! in the namelist_pisces (icefeinput). ln_ironice is forced to false
      ! when nn_ice_tr = 1
      ! ----------------------------------------------------
      IF( ln_ironice ) THEN
         !
         ! Compute the iron flux between sea ice and sea water
         ! Simple parameterization assuming a fixed constant concentration in
         ! sea-ice (icefeinput)
         ! ------------------------------------------------------------------         
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            zdep     = rfact / e3t(ji,jj,1,Kmm)
            zwflux   = fmmflx(ji,jj) / 1000._wp
            zironice =  MAX( -0.99 * tr(ji,jj,1,jpfer,Kbb), -zwflux * icefeinput * zdep )
            tr(ji,jj,1,jpfer,Krhs) = tr(ji,jj,1,jpfer,Krhs) + zironice
         END_2D
         !
         ! iron flux from ice
         IF( lk_iomput ) &
         & CALL iom_put( "Ironice", MAX( -0.99 * tr(:,:,1,jpfer,Kbb), (-1.*fmmflx(:,:)/1000._wp )*icefeinput*1.e+3*tmask(:,:,1)) )
         !
      ENDIF

      ! Add the external input of iron from sediment mobilization
      ! ------------------------------------------------------
      IF( ln_ironsed .AND. .NOT.lk_sed ) THEN
          tr(:,:,:,jpfer,Krhs) = tr(:,:,:,jpfer,Krhs) + ironsed(:,:,:) * rfact
          !
          IF( lk_iomput )  CALL iom_put( "Ironsed", ironsed(:,:,:) * 1.e+3 * tmask(:,:,:) ) 
      ENDIF

      ! Add the external input of iron from hydrothermal vents
      ! ------------------------------------------------------
      IF( ln_hydrofe ) THEN
         CALL fld_read( kt, 1, sf_hydrofe )
         DO jk = 1, jpk
            hydrofe(:,:,jk) = ( MAX( rtrn, sf_hydrofe(1)%fnow(:,:,jk) ) * hratio ) &
              &              / ( e1e2t(:,:) * e3t(:,:,jk,Kmm) * ryyss + rtrn ) / 1000._wp &
              &              * tmask(:,:,jk)
         ENDDO
                         tr(:,:,:,jpfer,Krhs) = tr(:,:,:,jpfer,Krhs) + hydrofe(:,:,:) * rfact
         IF( ln_ligand ) tr(:,:,:,jplgw,Krhs) = tr(:,:,:,jplgw,Krhs) + ( hydrofe(:,:,:) * lgw_rath ) * rfact
         !
         IF( lk_iomput ) CALL iom_put( "HYDR", hydrofe(:,:,:) * 1.e+3 * tmask(:,:,:) ) ! hydrothermal iron input
      ENDIF
      IF( ln_timing )  CALL timing_stop('p4z_bc')
      !
   END SUBROUTINE p4z_bc


   SUBROUTINE p4z_bc_init( Kmm ) 
      !!----------------------------------------------------------------------
      !!                  ***  routine p4z_bc_init  ***
      !!
      !! ** purpose :   initialization of the external sources of nutrients
      !!
      !! ** method  :   read the files and compute the budget
      !!                called at the first timestep (nittrc000)
      !!
      !! ** input   :   external netcdf files
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   Kmm  ! time level index
      INTEGER  :: ji, jj, jk, jm
      INTEGER  :: ii0, ii1, ij0, ij1
      INTEGER  :: numiron
      INTEGER  :: ierr, ierr1, ierr2, ierr3
      INTEGER  :: ios                 ! Local integer output status for namelist read
      INTEGER  :: ik50                !  last level where depth less than 50 m
      REAL(wp) :: zexpide, zdenitide, zmaskt, zsurfc, zsurfp,ze3t, ze3t2, zcslp
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: zriver, zcmask
      !
      CHARACTER(len=100) ::  cn_dir          ! Root directory for location of ssr files
      TYPE(FLD_N) ::   sn_dust, sn_ironsed, sn_hydrofe   ! informations about the fields to be read
      !!
      NAMELIST/nampisbc/cn_dir, sn_dust, sn_ironsed, sn_hydrofe, &
        &                ln_ironsed, ln_ironice, ln_hydrofe,     &
        &                sedfeinput, distcoast, icefeinput, wdust, mfrac,   &
        &                hratio, lgw_rath
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'p4z_bc_init : initialization of the external sources of nutrients '
         WRITE(numout,*) '~~~~~~~~~~~~ '
      ENDIF
      !                            !* set file information
      READ  ( numnatp_ref, nampisbc, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'nampisbc in reference namelist' )
      READ  ( numnatp_cfg, nampisbc, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'nampisbc in configuration namelist' )
      IF(lwm) WRITE ( numonp, nampisbc )


      IF(lwp) THEN
         WRITE(numout,*) '   Namelist : nampissbc '
         WRITE(numout,*) '      Fe input from sediments                  ln_ironsed  = ', ln_ironsed
         WRITE(numout,*) '      Fe input from seaice                     ln_ironice  = ', ln_ironice
         WRITE(numout,*) '      fe input from hydrothermal vents         ln_hydrofe  = ', ln_hydrofe
         IF( ln_ironsed ) THEN
            WRITE(numout,*) '      coastal release of iron                  sedfeinput  = ', sedfeinput
            WRITE(numout,*) '      distance off the coast                   distcoast   = ', distcoast
         ENDIF
         IF( ln_ligand ) THEN
            WRITE(numout,*) '      Weak ligand ratio from sed hydro sources  lgw_rath   = ', lgw_rath
         ENDIF
         IF( ln_ironice ) THEN
            WRITE(numout,*) '      Iron concentration in sea ice            icefeinput  = ', icefeinput
         ENDIF
         IF( ln_trc_sbc(jpfer) ) THEN
            WRITE(numout,*) '      Mineral Fe content of the dust           mfrac       = ', mfrac
            WRITE(numout,*) '      sinking speed of the dust                wdust       = ', wdust
         ENDIF
         IF( ln_hydrofe ) THEN
            WRITE(numout,*) '      Fe to 3He ratio assumed for vent iron supply hratio  = ', hratio
         ENDIF
      END IF

      ll_bc    = ( ln_trcbc .AND. lltrcbc )  .OR. ln_hydrofe .OR. ln_ironsed .OR. ln_ironice 
      ll_dust  =  ln_trc_sbc(jpfer) .OR. ln_trc_sbc(jppo4) .OR. ln_trc_sbc(jpsil) .OR. ln_sediment
      ll_ndepo =  ln_trc_sbc(jpno3) .OR. ln_trc_sbc(jpnh4)   
      ll_river =  ln_trc_cbc(jpno3)  

      ! dust input from the atmosphere
      ! ------------------------------
      IF( ll_dust ) THEN
         !
         IF(lwp) WRITE(numout,*) '    initialize dust input from atmosphere '
         IF(lwp) WRITE(numout,*) '    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ '
         !
         ALLOCATE( dust(jpi,jpj) ) 
         !
         ALLOCATE( sf_dust(1), STAT=ierr )           !* allocate and fill sf_sst (forcing structure) with sn_sst
         IF( ierr > 0 )   CALL ctl_stop( 'STOP', 'p4z_bc_init: unable to allocate sf_dust structure' )
         !
         CALL fld_fill( sf_dust, (/ sn_dust /), cn_dir, 'p4z_bc_init', 'Atmospheric dust deposition', 'nampisbc' )
                                   ALLOCATE( sf_dust(1)%fnow(jpi,jpj,1)   )
         IF( sn_dust%ln_tint )     ALLOCATE( sf_dust(1)%fdta(jpi,jpj,1,2) )
         !
      END IF

      ! coastal and island masks
      ! ------------------------
      IF( ln_ironsed ) THEN     
         !
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '   ==>>>   ln_ironsed=T , computation of an island mask to enhance coastal supply of iron'
         !
         ALLOCATE( ironsed(jpi,jpj,jpk) )    ! allocation
         !
         CALL iom_open ( TRIM( sn_ironsed%clname ), numiron )
         ALLOCATE( zcmask(jpi,jpj,jpk) )
         CALL iom_get  ( numiron, jpdom_global, TRIM( sn_ironsed%clvar ), zcmask(:,:,:), 1 )
         CALL iom_close( numiron )
         !
         ik50 = 5        !  last level where depth less than 50 m
         DO jk = jpkm1, 1, -1
            IF( gdept_1d(jk) > 50. )   ik50 = jk - 1
         END DO
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) ' Level corresponding to 50m depth ',  ik50,' ', gdept_1d(ik50+1)
         DO_3D( 0, 0, 0, 0, 1, ik50 )
            ze3t   = e3t_0(ji,jj,jk)
            zsurfc =  e1u(ji,jj) * ( 1. - umask(ji  ,jj  ,jk) )   &
                    + e1u(ji,jj) * ( 1. - umask(ji-1,jj  ,jk) )   &
                    + e2v(ji,jj) * ( 1. - vmask(ji  ,jj  ,jk) )   &
                    + e2v(ji,jj) * ( 1. - vmask(ji  ,jj-1,jk) )
            zsurfp = zsurfc * ze3t / e1e2t(ji,jj)
            ! estimation of the coastal slope : 5 km off the coast
            ze3t2 = ze3t * ze3t
            zcslp = SQRT( ( distcoast*distcoast + ze3t2 ) / ze3t2 )
            !
            zcmask(ji,jj,jk) = zcmask(ji,jj,jk) + zcslp * zsurfp
         END_3D
         !
         CALL lbc_lnk( 'p4zbc', zcmask , 'T', 1.0_wp )      ! lateral boundary conditions on cmask   (sign unchanged)
         !
         DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpk )
            zexpide   = MIN( 8.,( gdept(ji,jj,jk,Kmm) / 500. )**(-1.5) )
            zdenitide = -0.9543 + 0.7662 * LOG( zexpide ) - 0.235 * LOG( zexpide )**2
            zcmask(ji,jj,jk) = zcmask(ji,jj,jk) * MIN( 1., EXP( zdenitide ) / 0.5 )
         END_3D
         ! Coastal supply of iron
         ! -------------------------
         ironsed(:,:,jpk) = 0._wp
         DO jk = 1, jpkm1
            ironsed(:,:,jk) = sedfeinput * zcmask(:,:,jk) / ( e3t_0(:,:,jk) * rday )
         END DO
         DEALLOCATE( zcmask)
      ENDIF
      !
      ! Iron from Hydrothermal vents
      ! ------------------------
      IF( ln_hydrofe ) THEN
         !
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '   ==>>>   ln_hydrofe=T , Input of iron from hydrothermal vents'
         !
         ALLOCATE( hydrofe(jpi,jpj,jpk) )    ! allocation
         !
         ALLOCATE( sf_hydrofe(1), STAT=ierr )           !* allocate and fill sf_sst (forcing structure) with sn_sst
         IF( ierr > 0 )   CALL ctl_stop( 'STOP', 'p4z_bc_init: unable to allocate sf_hydro structure' )
         !
         CALL fld_fill( sf_hydrofe, (/ sn_hydrofe /), cn_dir, 'p4z_bc_init', 'Input of iron from hydrothermal vents', 'nampisbc' )
                                   ALLOCATE( sf_hydrofe(1)%fnow(jpi,jpj,jpk)   )
         IF( sn_hydrofe%ln_tint )    ALLOCATE( sf_hydrofe(1)%fdta(jpi,jpj,jpk,2) )
         !
      ENDIF
      !
   END SUBROUTINE p4z_bc_init

   !!======================================================================
END MODULE p4zbc
