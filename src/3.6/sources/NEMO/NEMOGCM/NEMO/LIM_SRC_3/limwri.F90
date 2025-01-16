MODULE limwri
   !!======================================================================
   !!                     ***  MODULE  limwri  ***
   !!         Ice diagnostics :  write ice output files
   !!======================================================================
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3'                                      LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_wri      : write of the diagnostics variables in ouput file 
   !!   lim_wri_state : write for initial state or/and abandon
   !!----------------------------------------------------------------------
   USE ioipsl
   USE dianam          ! build name of file (routine)
   USE phycst
   USE dom_oce
   USE sbc_oce         ! Surface boundary condition: ocean fields
   USE sbc_ice         ! Surface boundary condition: ice fields
   USE ice
   USE limvar
   USE in_out_manager
   USE lbclnk
   USE lib_mpp         ! MPP library
   USE wrk_nemo        ! work arrays
   USE iom
   USE timing          ! Timing
   USE lib_fortran     ! Fortran utilities

   IMPLICIT NONE
   PRIVATE

   PUBLIC lim_wri        ! routine called by lim_step.F90
   PUBLIC lim_wri_state  ! called by dia_wri_state 

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: limwri.F90 6965 2016-09-30 13:34:19Z clem $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_wri( kindic )
      !!-------------------------------------------------------------------
      !!  This routine computes the average of some variables and write it
      !!  on the ouput files.
      !!  ATTENTION cette routine n'est valable que si le pas de temps est
      !!  egale a une fraction entiere de 1 jours.
      !!  Diff 1-D 3-D : suppress common also included in etat
      !!                 suppress cmoymo 11-18
      !!  modif : 03/06/98
      !!-------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kindic   ! if kindic < 0 there has been an error somewhere
      !
      INTEGER  ::  ji, jj, jk, jl  ! dummy loop indices
      REAL(wp) ::  z1_365
      REAL(wp) ::  z2da, z2db, ztmp
      REAL(wp), POINTER, DIMENSION(:,:,:) ::  zswi2
      REAL(wp), POINTER, DIMENSION(:,:)   ::  z2d, zswi    ! 2D workspace
      !!-------------------------------------------------------------------

      IF( nn_timing == 1 )  CALL timing_start('limwri')

      CALL wrk_alloc( jpi, jpj, jpl, zswi2 )
      CALL wrk_alloc( jpi, jpj     , z2d, zswi )

      !-----------------------------
      ! Mean category values
      !-----------------------------
      z1_365 = 1._wp / 365._wp

      ! brine volume
      CALL lim_var_bv 

      ! tresholds for outputs
      DO jj = 1, jpj
         DO ji = 1, jpi
            zswi(ji,jj)  = MAX( 0._wp , SIGN( 1._wp , at_i(ji,jj) - epsi06 ) )
         END DO
      END DO
      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               zswi2(ji,jj,jl)  = MAX( 0._wp , SIGN( 1._wp , a_i(ji,jj,jl) - epsi06 ) )
            END DO
         END DO
      END DO
      !
      ! fluxes 
      ! pfrld is the lead fraction at the previous time step (actually between TRP and THD)
      IF( iom_use('qsr_oce') )   CALL iom_put( "qsr_oce" , qsr_oce(:,:) * pfrld(:,:) )                                   !     solar flux at ocean surface
      IF( iom_use('qns_oce') )   CALL iom_put( "qns_oce" , qns_oce(:,:) * pfrld(:,:) + qemp_oce(:,:) )                   ! non-solar flux at ocean surface
      IF( iom_use('qsr_ice') )   CALL iom_put( "qsr_ice" , SUM( qsr_ice(:,:,:) * a_i_b(:,:,:), dim=3 ) )                 !     solar flux at ice surface
      IF( iom_use('qns_ice') )   CALL iom_put( "qns_ice" , SUM( qns_ice(:,:,:) * a_i_b(:,:,:), dim=3 ) + qemp_ice(:,:) ) ! non-solar flux at ice surface
      IF( iom_use('qtr_ice') )   CALL iom_put( "qtr_ice" , SUM( ftr_ice(:,:,:) * a_i_b(:,:,:), dim=3 ) )                 !     solar flux transmitted thru ice
      IF( iom_use('qt_oce' ) )   CALL iom_put( "qt_oce"  , ( qsr_oce(:,:) + qns_oce(:,:) ) * pfrld(:,:) + qemp_oce(:,:) )  
      IF( iom_use('qt_ice' ) )   CALL iom_put( "qt_ice"  , SUM( ( qns_ice(:,:,:) + qsr_ice(:,:,:) )   &
         &                                                      * a_i_b(:,:,:),dim=3 ) + qemp_ice(:,:) )
      IF( iom_use('qemp_oce') )  CALL iom_put( "qemp_oce" , qemp_oce(:,:) )  
      IF( iom_use('qemp_ice') )  CALL iom_put( "qemp_ice" , qemp_ice(:,:) )  
      IF( iom_use('emp_oce' ) )  CALL iom_put( "emp_oce"  , emp_oce(:,:) )   !emp over ocean (taking into account the snow blown away from the ice)
      IF( iom_use('emp_ice' ) )  CALL iom_put( "emp_ice"  , emp_ice(:,:) )   !emp over ice   (taking into account the snow blown away from the ice)

      ! velocity
      IF ( iom_use( "uice_ipa" ) .OR. iom_use( "vice_ipa" ) .OR. iom_use( "icevel" ) ) THEN 
         DO jj = 2 , jpjm1
            DO ji = 2 , jpim1
               z2da  = ( u_ice(ji,jj) * umask(ji,jj,1) + u_ice(ji-1,jj) * umask(ji-1,jj,1) ) * 0.5_wp
               z2db  = ( v_ice(ji,jj) * vmask(ji,jj,1) + v_ice(ji,jj-1) * vmask(ji,jj-1,1) ) * 0.5_wp
               z2d(ji,jj) = SQRT( z2da * z2da + z2db * z2db )
           END DO
         END DO
         CALL lbc_lnk( z2d, 'T', 1. )
         CALL iom_put( "uice_ipa"     , u_ice      )       ! ice velocity u component
         CALL iom_put( "vice_ipa"     , v_ice      )       ! ice velocity v component
         CALL iom_put( "icevel"       , z2d        )       ! ice velocity module
      ENDIF
      !
      IF ( iom_use( "miceage" ) )       CALL iom_put( "miceage"     , om_i * zswi * z1_365   )  ! mean ice age
      IF ( iom_use( "icethic_cea" ) )   CALL iom_put( "icethic_cea" , htm_i * zswi           )  ! ice thickness mean
      IF ( iom_use( "snowthic_cea" ) )  CALL iom_put( "snowthic_cea", htm_s * zswi           )  ! snow thickness mean
      IF ( iom_use( "micet" ) )         CALL iom_put( "micet"       , ( tm_i  - rt0 ) * zswi )  ! ice mean    temperature
      IF ( iom_use( "icest" ) )         CALL iom_put( "icest"       , ( tm_su - rt0 ) * zswi )  ! ice surface temperature
      IF ( iom_use( "icecolf" ) )       CALL iom_put( "icecolf"     , hicol                  )  ! frazil ice collection thickness
      !
      CALL iom_put( "isst"        , sst_m               )        ! sea surface temperature
      CALL iom_put( "isss"        , sss_m               )        ! sea surface salinity
      CALL iom_put( "iceconc"     , at_i  * zswi        )        ! ice concentration
      CALL iom_put( "icevolu"     , vt_i  * zswi        )        ! ice volume = mean ice thickness over the cell
      CALL iom_put( "icehc"       , et_i  * zswi        )        ! ice total heat content
      CALL iom_put( "isnowhc"     , et_s  * zswi        )        ! snow total heat content
      CALL iom_put( "ibrinv"      , bvm_i * zswi * 100. )        ! brine volume
      CALL iom_put( "utau_ice"    , utau_ice            )        ! wind stress over ice along i-axis at I-point
      CALL iom_put( "vtau_ice"    , vtau_ice            )        ! wind stress over ice along j-axis at I-point
      CALL iom_put( "snowpre"     , sprecip * 86400.    )        ! snow precipitation 
      CALL iom_put( "micesalt"    , smt_i   * zswi      )        ! mean ice salinity

      CALL iom_put( "icestr"      , strength * zswi )    ! ice strength
      CALL iom_put( "idive"       , divu_i * 1.0e8      )    ! divergence
      CALL iom_put( "ishear"      , shear_i * 1.0e8     )    ! shear
      CALL iom_put( "snowvol"     , vt_s   * zswi       )        ! snow volume
      
      CALL iom_put( "icetrp"      , diag_trp_vi * rday  )        ! ice volume transport
      CALL iom_put( "snwtrp"      , diag_trp_vs * rday  )        ! snw volume transport
      CALL iom_put( "saltrp"      , diag_trp_smv * rday * rhoic ) ! salt content transport
      CALL iom_put( "deitrp"      , diag_trp_ei         )        ! advected ice enthalpy (W/m2)
      CALL iom_put( "destrp"      , diag_trp_es         )        ! advected snw enthalpy (W/m2)

      CALL iom_put( "sfxbog"      , sfx_bog * rday      )        ! salt flux from bottom growth
      CALL iom_put( "sfxbom"      , sfx_bom * rday      )        ! salt flux from bottom melting
      CALL iom_put( "sfxsum"      , sfx_sum * rday      )        ! salt flux from surface melting
      CALL iom_put( "sfxsni"      , sfx_sni * rday      )        ! salt flux from snow ice formation
      CALL iom_put( "sfxopw"      , sfx_opw * rday      )        ! salt flux from open water formation
      CALL iom_put( "sfxdyn"      , sfx_dyn * rday      )        ! salt flux from ridging rafting
      CALL iom_put( "sfxres"      , sfx_res * rday      )        ! salt flux from limupdate (resultant)
      CALL iom_put( "sfxbri"      , sfx_bri * rday      )        ! salt flux from brines
      CALL iom_put( "sfxsub"      , sfx_sub * rday      )        ! salt flux from sublimation
      CALL iom_put( "sfx"         , sfx     * rday      )        ! total salt flux

      ztmp = rday / rhoic
      CALL iom_put( "vfxres"     , wfx_res * ztmp       )        ! daily prod./melting due to limupdate 
      CALL iom_put( "vfxopw"     , wfx_opw * ztmp       )        ! daily lateral thermodynamic ice production
      CALL iom_put( "vfxsni"     , wfx_sni * ztmp       )        ! daily snowice ice production
      CALL iom_put( "vfxbog"     , wfx_bog * ztmp       )        ! daily bottom thermodynamic ice production
      CALL iom_put( "vfxdyn"     , wfx_dyn * ztmp       )        ! daily dynamic ice production (rid/raft)
      CALL iom_put( "vfxsum"     , wfx_sum * ztmp       )        ! surface melt 
      CALL iom_put( "vfxbom"     , wfx_bom * ztmp       )        ! bottom melt 
      CALL iom_put( "vfxice"     , wfx_ice * ztmp       )        ! total ice growth/melt 

      IF ( iom_use( "vfxthin" ) ) THEN   ! ice production for open water + thin ice (<20cm) => comparable to observations  
         WHERE( htm_i(:,:) < 0.2 .AND. htm_i(:,:) > 0. ) ; z2d = wfx_bog
         ELSEWHERE                                       ; z2d = 0._wp
         END WHERE
         CALL iom_put( "vfxthin", ( wfx_opw + z2d ) * ztmp )
      ENDIF

      ztmp = rday / rhosn
      CALL iom_put( "vfxspr"     , wfx_spr * ztmp       )        ! precip (snow)
      CALL iom_put( "vfxsnw"     , wfx_snw * ztmp       )        ! total snw growth/melt 
      CALL iom_put( "vfxsub"     , wfx_sub * ztmp       )        ! sublimation (snow/ice) 
      CALL iom_put( "vfxsub_err" , wfx_err_sub * ztmp   )        ! "excess" of sublimation sent to ocean      
 
      CALL iom_put( "afxtot"     , afx_tot * rday       )        ! concentration tendency (total)
      CALL iom_put( "afxdyn"     , afx_dyn * rday       )        ! concentration tendency (dynamics)
      CALL iom_put( "afxthd"     , afx_thd * rday       )        ! concentration tendency (thermo)

      CALL iom_put ('hfxthd'     , hfx_thd(:,:)         )   !  
      CALL iom_put ('hfxdyn'     , hfx_dyn(:,:)         )   !  
      CALL iom_put ('hfxres'     , hfx_res(:,:)         )   !  
      CALL iom_put ('hfxout'     , hfx_out(:,:)         )   !  
      CALL iom_put ('hfxin'      , hfx_in(:,:)          )   !  
      CALL iom_put ('hfxsnw'     , hfx_snw(:,:)         )   !  
      CALL iom_put ('hfxsub'     , hfx_sub(:,:)         )   !  
      CALL iom_put ('hfxerr'     , hfx_err(:,:)         )   !  
      CALL iom_put ('hfxerr_rem' , hfx_err_rem(:,:)     )   !  
      
      CALL iom_put ('hfxsum'     , hfx_sum(:,:)         )   !  
      CALL iom_put ('hfxbom'     , hfx_bom(:,:)         )   !  
      CALL iom_put ('hfxbog'     , hfx_bog(:,:)         )   !  
      CALL iom_put ('hfxdif'     , hfx_dif(:,:)         )   !  
      CALL iom_put ('hfxopw'     , hfx_opw(:,:)         )   !  
      CALL iom_put ('hfxtur'     , fhtur(:,:) * SUM( a_i_b(:,:,:), dim=3 ) ) ! turbulent heat flux at ice base 
      CALL iom_put ('hfxdhc'     , diag_heat(:,:)       )   ! Heat content variation in snow and ice 
      CALL iom_put ('hfxspr'     , hfx_spr(:,:)         )   ! Heat content of snow precip 

      
      !--------------------------------
      ! Output values for each category
      !--------------------------------
      IF ( iom_use( "iceconc_cat"  ) )  CALL iom_put( "iceconc_cat"      , a_i   * zswi2   )        ! area for categories
      IF ( iom_use( "icethic_cat"  ) )  CALL iom_put( "icethic_cat"      , ht_i  * zswi2   )        ! thickness for categories
      IF ( iom_use( "snowthic_cat" ) )  CALL iom_put( "snowthic_cat"     , ht_s  * zswi2   )        ! snow depth for categories
      IF ( iom_use( "salinity_cat" ) )  CALL iom_put( "salinity_cat"     , sm_i  * zswi2   )        ! salinity for categories
      ! ice temperature
      IF ( iom_use( "icetemp_cat"  ) )  CALL iom_put( "icetemp_cat", ( SUM( t_i(:,:,:,:), dim=3 ) * r1_nlay_i - rt0 ) * zswi2 )
      ! snow temperature
      IF ( iom_use( "snwtemp_cat"  ) )  CALL iom_put( "snwtemp_cat", ( SUM( t_s(:,:,:,:), dim=3 ) * r1_nlay_s - rt0 ) * zswi2 )
      ! ice age
      IF ( iom_use( "iceage_cat"   ) )  CALL iom_put( "iceage_cat" , o_i * zswi2 * z1_365 )
      ! brine volume
      IF ( iom_use( "brinevol_cat" ) )  CALL iom_put( "brinevol_cat", bv_i * 100. * zswi2 )

      !     !  Create an output files (output.lim.abort.nc) if S < 0 or u > 20 m/s
      !     IF( kindic < 0 )   CALL lim_wri_state( 'output.abort' )
      !     not yet implemented
      
      CALL wrk_dealloc( jpi, jpj, jpl, zswi2 )
      CALL wrk_dealloc( jpi, jpj     , z2d, zswi )

      IF( nn_timing == 1 )  CALL timing_stop('limwri')
      
   END SUBROUTINE lim_wri

 
   SUBROUTINE lim_wri_state( kt, kid, kh_i )
      !!---------------------------------------------------------------------
      !!                 ***  ROUTINE lim_wri_state  ***
      !!        
      !! ** Purpose :   create a NetCDF file named cdfile_name which contains 
      !!      the instantaneous ice state and forcing fields for ice model
      !!        Used to find errors in the initial state or save the last
      !!      ocean state in case of abnormal end of a simulation
      !!
      !! History :
      !!   4.0  !  2013-06  (C. Rousset)
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in )   ::   kt               ! ocean time-step index)
      INTEGER, INTENT( in )   ::   kid , kh_i
      INTEGER                 ::   nz_i, jl
      REAL(wp), DIMENSION(jpl) :: jcat
      !!----------------------------------------------------------------------
      DO jl = 1, jpl
         jcat(jl) = REAL(jl)
      ENDDO
      
      CALL histvert( kid, "ncatice", "Ice Categories","", jpl, jcat, nz_i, "up")

      CALL histdef( kid, "sithic", "Ice thickness"           , "m"      ,   &
      &      jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "siconc", "Ice concentration"       , "%"      ,   &
      &      jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "sitemp", "Ice temperature"         , "C"      ,   &
      &      jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "sivelu", "i-Ice speed "            , "m/s"    ,   &
      &      jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "sivelv", "j-Ice speed "            , "m/s"    ,   &
      &      jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt ) 
      CALL histdef( kid, "sistru", "i-Wind stress over ice " , "Pa"     ,   &
      &      jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "sistrv", "j-Wind stress over ice " , "Pa"     ,   &
      &      jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt ) 
      CALL histdef( kid, "sisflx", "Solar flux over ocean"     , "w/m2" ,   &
      &      jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt ) 
      CALL histdef( kid, "sinflx", "Non-solar flux over ocean" , "w/m2" ,   &
      &      jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "isnowpre", "Snow precipitation"      , "kg/m2/s",   &
      &      jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt ) 
      CALL histdef( kid, "sisali", "Ice salinity"            , "PSU"    ,   &
      &      jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt ) 
      CALL histdef( kid, "sivolu", "Ice volume"              , "m"      ,   &
      &      jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt ) 
      CALL histdef( kid, "sidive", "Ice divergence"          , "10-8s-1",   &
      &      jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt ) 

      CALL histdef( kid, "vfxbog", "Ice bottom production"   , "m/s"    ,   &
      &      jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "vfxdyn", "Ice dynamic production"  , "m/s"    ,   &
      &      jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "vfxopw", "Ice open water prod"     , "m/s"    ,   &
      &       jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "vfxsni", "Snow ice production "    , "m/s"    ,   &
      &      jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "vfxres", "Ice prod from limupdate" , "m/s"    ,   &
      &      jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "vfxbom", "Ice bottom melt"         , "m/s"    ,   &
      &      jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "vfxsum", "Ice surface melt"        , "m/s"    ,   &
      &      jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt )

      CALL histdef( kid, "sithicat", "Ice thickness"         , "m"      ,   &
      &      jpi, jpj, kh_i, jpl, 1, jpl, nz_i, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "siconcat", "Ice concentration"     , "%"      ,   &
      &      jpi, jpj, kh_i, jpl, 1, jpl, nz_i, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "sisalcat", "Ice salinity"           , ""      ,   &
      &      jpi, jpj, kh_i, jpl, 1, jpl, nz_i, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "sitemcat", "Ice temperature"       , "C"      ,   &
      &      jpi, jpj, kh_i, jpl, 1, jpl, nz_i, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "snthicat", "Snw thickness"         , "m"      ,   &
      &      jpi, jpj, kh_i, jpl, 1, jpl, nz_i, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "sntemcat", "Snw temperature"       , "C"      ,   &
      &      jpi, jpj, kh_i, jpl, 1, jpl, nz_i, 32, "inst(x)", rdt, rdt )

      CALL histend( kid, snc4set )   ! end of the file definition

      CALL histwrite( kid, "sithic", kt, htm_i         , jpi*jpj, (/1/) )    
      CALL histwrite( kid, "siconc", kt, at_i          , jpi*jpj, (/1/) )
      CALL histwrite( kid, "sitemp", kt, tm_i - rt0    , jpi*jpj, (/1/) )
      CALL histwrite( kid, "sivelu", kt, u_ice          , jpi*jpj, (/1/) )
      CALL histwrite( kid, "sivelv", kt, v_ice          , jpi*jpj, (/1/) )
      CALL histwrite( kid, "sistru", kt, utau_ice       , jpi*jpj, (/1/) )
      CALL histwrite( kid, "sistrv", kt, vtau_ice       , jpi*jpj, (/1/) )
      CALL histwrite( kid, "sisflx", kt, qsr , jpi*jpj, (/1/) )
      CALL histwrite( kid, "sinflx", kt, qns , jpi*jpj, (/1/) )
      CALL histwrite( kid, "isnowpre", kt, sprecip        , jpi*jpj, (/1/) )
      CALL histwrite( kid, "sisali", kt, smt_i          , jpi*jpj, (/1/) )
      CALL histwrite( kid, "sivolu", kt, vt_i           , jpi*jpj, (/1/) )
      CALL histwrite( kid, "sidive", kt, divu_i*1.0e8   , jpi*jpj, (/1/) )

      CALL histwrite( kid, "vfxbog", kt, wfx_bog        , jpi*jpj, (/1/) )
      CALL histwrite( kid, "vfxdyn", kt, wfx_dyn        , jpi*jpj, (/1/) )
      CALL histwrite( kid, "vfxopw", kt, wfx_opw        , jpi*jpj, (/1/) )
      CALL histwrite( kid, "vfxsni", kt, wfx_sni        , jpi*jpj, (/1/) )
      CALL histwrite( kid, "vfxres", kt, wfx_res        , jpi*jpj, (/1/) )
      CALL histwrite( kid, "vfxbom", kt, wfx_bom        , jpi*jpj, (/1/) )
      CALL histwrite( kid, "vfxsum", kt, wfx_sum        , jpi*jpj, (/1/) )

      CALL histwrite( kid, "sithicat", kt, ht_i        , jpi*jpj*jpl, (/1/) )    
      CALL histwrite( kid, "siconcat", kt, a_i         , jpi*jpj*jpl, (/1/) )    
      CALL histwrite( kid, "sisalcat", kt, sm_i        , jpi*jpj*jpl, (/1/) )    
      CALL histwrite( kid, "sitemcat", kt, tm_i - rt0  , jpi*jpj*jpl, (/1/) )    
      CALL histwrite( kid, "snthicat", kt, ht_s        , jpi*jpj*jpl, (/1/) )    
      CALL histwrite( kid, "sntemcat", kt, tm_su - rt0 , jpi*jpj*jpl, (/1/) )    

      ! Close the file
      ! -----------------
      !CALL histclo( kid )

    END SUBROUTINE lim_wri_state

#else
   !!----------------------------------------------------------------------
   !!   Default option :         Empty module          NO LIM sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_wri          ! Empty routine
   END SUBROUTINE lim_wri
#endif

   !!======================================================================
END MODULE limwri
