MODULE domain
   !!==============================================================================
   !!                       ***  MODULE domain   ***
   !! Ocean initialization : domain initialization
   !!==============================================================================
   !! History :  OPA  !  1990-10  (C. Levy - G. Madec)  Original code
   !!                 !  1992-01  (M. Imbard) insert time step initialization
   !!                 !  1996-06  (G. Madec) generalized vertical coordinate 
   !!                 !  1997-02  (G. Madec) creation of domwri.F
   !!                 !  2001-05  (E.Durand - G. Madec) insert closed sea
   !!   NEMO     1.0  !  2002-08  (G. Madec)  F90: Free form and module
   !!            2.0  !  2005-11  (V. Garnier) Surface pressure gradient organization
   !!            3.3  !  2010-11  (G. Madec)  initialisation in C1D configuration
   !!            3.6  !  2013     ( J. Simeon, C. Calone, G. Madec, C. Ethe ) Online coarsening of outputs
   !!            3.7  !  2015-11  (G. Madec, A. Coward)  time varying zgr by default
   !!----------------------------------------------------------------------
   
   !!----------------------------------------------------------------------
   !!   dom_init       : initialize the space and time domain
   !!   dom_nam        : read and contral domain namelists
   !!   dom_ctl        : control print for the ocean domain
   !!----------------------------------------------------------------------
   USE dom_oce         ! domain: ocean
   USE phycst          ! physical constants
   USE domhgr          ! domain: set the horizontal mesh
   USE domzgr          ! domain: set the vertical mesh
   USE dommsk          ! domain: set the mask system
   USE domclo          ! domain: set closed sea mask
   !
   USE lib_mpp         !
   USE in_out_manager  ! I/O manager
   USE iom             ! 

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dom_init   ! called by opa.F90
   PUBLIC   dom_nam  ! called by opa.F90
   PUBLIC   cfg_write   ! called by opa.F90

   !!-------------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: domain.F90 6140 2015-12-21 11:35:23Z timgraham $
   !! Software governed by the CeCILL licence        (./LICENSE)
   !!-------------------------------------------------------------------------
CONTAINS

   SUBROUTINE dom_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dom_init  ***
      !!                    
      !! ** Purpose :   Domain initialization. Call the routines that are 
      !!              required to create the arrays which define the space 
      !!              and time domain of the ocean model.
      !!
      !! ** Method  : - dom_msk: compute the masks from the bathymetry file
      !!              - dom_hgr: compute or read the horizontal grid-point position
      !!                         and scale factors, and the coriolis factor
      !!              - dom_zgr: define the vertical coordinate and the bathymetry
      !!              - dom_stp: defined the model time step
      !!              - dom_wri: create the meshmask file if nmsh=1
      !!              - 1D configuration, move Coriolis, u and v at T-point
      !!----------------------------------------------------------------------
      INTEGER ::   jk          ! dummy loop indices
      INTEGER ::   iconf = 0   ! local integers
      REAL(wp), POINTER, DIMENSION(:,:) ::   z1_hu_0, z1_hv_0
      INTEGER , DIMENSION(jpi,jpj) ::   ik_top , ik_bot       ! top and bottom ocean level
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'dom_init : domain initialization'
         WRITE(numout,*) '~~~~~~~~'
      ENDIF
      !
      !                       !==  Reference coordinate system  ==!
      !
      CALL dom_glo                     ! global domain versus local domain
      CALL dom_nam               ! read namelist ( namrun, namdom )
         
      CALL dom_hgr               ! Horizontal mesh
      CALL dom_zgr( ik_top, ik_bot )  ! Vertical mesh and bathymetry
      CALL dom_msk( ik_top, ik_bot )  ! Masks
      IF ( ln_domclo ) CALL dom_clo               ! Closed seas and lake 
      !
      CALL dom_ctl                  ! print extrema of masked scale factors
      ! 
#if ! defined key_agrif
      CALL cfg_write                ! create the configuration file
#endif
      !
   END SUBROUTINE dom_init

   SUBROUTINE dom_glo
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE dom_glo  ***
      !!
      !! ** Purpose :   initialization of global domain <--> local domain indices
      !!
      !! ** Method  :   
      !!
      !! ** Action  : - mig , mjg : local  domain indices ==> global domain, including halos, indices
      !!              - mig0, mjg0: local  domain indices ==> global domain, excluding halos, indices
      !!              - mi0 , mi1 : global domain indices ==> local  domain indices
      !!              - mj0 , mj1   (if global point not in the local domain ==> mi0>mi1 and/or mj0>mj1)
      !!----------------------------------------------------------------------
      INTEGER ::   ji, jj   ! dummy loop argument
      !!----------------------------------------------------------------------
      !
      DO ji = 1, jpi                 ! local domain indices ==> global domain, including halos, indices
        mig(ji) = ji + nimpp - 1
      END DO
      DO jj = 1, jpj
        mjg(jj) = jj + njmpp - 1
      END DO
      !                              ! local domain indices ==> global domain, excluding halos, indices
      !
      mig0(:) = mig(:) - nn_hls
      mjg0(:) = mjg(:) - nn_hls  
      ! WARNING: to keep compatibility with the trunk that was including periodocity into the input data, 
      ! we must define mig0 and mjg0 as bellow.
      ! Once we decide to forget trunk compatibility, we must simply define mig0 and mjg0 as:
      mig0_oldcmp(:) = mig0(:) + COUNT( (/ jperio == 1 .OR. jperio == 4 .OR. jperio == 6 .OR. jperio == 7 /) )
      mjg0_oldcmp(:) = mjg0(:) + COUNT( (/ jperio == 2 .OR. jperio == 7 /) )
      !
      !                              ! global domain, including halos, indices ==> local domain indices
      !                                   ! (return (m.0,m.1)=(1,0) if data domain gridpoint is to the west/south of the 
      !                                   ! local domain, or (m.0,m.1)=(jp.+1,jp.) to the east/north of local domain. 
      DO ji = 1, jpiglo
        mi0(ji) = MAX( 1 , MIN( ji - nimpp + 1, jpi+1 ) )
        mi1(ji) = MAX( 0 , MIN( ji - nimpp + 1, jpi   ) )
      END DO
      DO jj = 1, jpjglo
        mj0(jj) = MAX( 1 , MIN( jj - njmpp + 1, jpj+1 ) )
        mj1(jj) = MAX( 0 , MIN( jj - njmpp + 1, jpj   ) )
      END DO
      IF(lwp) THEN                   ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'dom_glo : domain: global <<==>> local '
         WRITE(numout,*) '~~~~~~~ '
         WRITE(numout,*) '   global domain:   jpiglo = ', jpiglo, ' jpjglo = ', jpjglo, ' jpkglo = ', jpkglo
         WRITE(numout,*) '   local  domain:   jpi    = ', jpi   , ' jpj    = ', jpj   , ' jpk    = ', jpk
         WRITE(numout,*)
      ENDIF
      !
   END SUBROUTINE dom_glo

   SUBROUTINE dom_nam
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE dom_nam  ***
      !!                    
      !! ** Purpose :   read domaine namelists and print the variables.
      !!
      !! ** input   : - namrun namelist
      !!              - namdom namelist
      !!              - namnc4 namelist   ! "key_netcdf4" only
      !!----------------------------------------------------------------------
      USE ioipsl
      NAMELIST/namrun/ cn_exp   ,    &          
         &             nn_it000, nn_itend , nn_date0    , nn_time0     , nn_leapy  ,     &
         &             ln_mskland  , ln_clobber   , nn_chunksz,     &
         &             ln_cfmeta, ln_iscpl

      NAMELIST/namdom/ ln_read_cfg, nn_bathy, cn_domcfg, cn_topo, cn_bath, cn_lon, cn_lat, rn_scale, nn_interp, &
         &             cn_topolvl, cn_fisfd, cn_visfd, cn_bathlvl, cn_fcoord,                        & 
         &             rn_bathy , rn_e3zps_min, rn_e3zps_rat, nn_msh, rn_hmin,                       &
         &             rn_atfp , rn_rdt   ,  ln_crs      , jphgr_msh ,                               &
         &             ppglam0, ppgphi0, ppe1_deg, ppe2_deg, ppe1_m, ppe2_m,                         &
         &             ppsur, ppa0, ppa1, ppkth, ppacr, ppdzmin, pphmax, ldbletanh,                  &
         &             ppa2, ppkth2, ppacr2

      INTEGER  ::   ios                 ! Local integer output status for namelist read
      CHARACTER(256) :: c_iomsg
      !!----------------------------------------------------------------------

   
      READ  ( numnam_ref, namrun, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namrun in reference namelist')

      READ  ( numnam_cfg, namrun, IOSTAT = ios, IOMSG = c_iomsg, ERR = 902 )

902   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namrun in configuration namelist')
      IF(lwm) WRITE ( numond, namrun )
      !
      IF(lwp) THEN                  ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'dom_nam  : domain initialization through namelist read'
         WRITE(numout,*) '~~~~~~~ '
         WRITE(numout,*) '   Namelist namrun'
         WRITE(numout,*) '      experiment name for output      cn_exp     = ', cn_exp
         WRITE(numout,*) '      mask land points                ln_mskland = ', ln_mskland
         WRITE(numout,*) '      additional CF standard metadata ln_cfmeta  = ', ln_cfmeta
         WRITE(numout,*) '      overwrite an existing file      ln_clobber = ', ln_clobber
         WRITE(numout,*) '      NetCDF chunksize (bytes)        nn_chunksz = ', nn_chunksz
      ENDIF

      cexper = cn_exp
      nit000 = nn_it000
      nitend = nn_itend
      ndate0 = nn_date0
      nleapy = nn_leapy

      ! 
      cn_topo =''
      cn_bath =''
      cn_lon  =''
      cn_lat  =''
      rn_scale = 1.

      !REWIND( numnam_ref )              ! Namelist namdom in reference namelist : space & time domain (bathymetry, mesh, timestep)
      READ  ( numnam_ref, namdom, IOSTAT = ios, ERR = 903)
903   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namdom in reference namelist' )
  
      !
      !REWIND( numnam_cfg )              ! Namelist namdom in configuration namelist : space & time domain (bathymetry, mesh, timestep)
      READ  ( numnam_cfg, namdom, IOSTAT = ios, ERR = 904 )
904   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namdom in configuration namelist' )
      IF(lwm) WRITE ( numond, namdom )
      !
#if defined key_agrif
      IF (.NOT.Agrif_root()) THEN
         jphgr_msh = Agrif_Parent(jphgr_msh)
!         nn_bathy = Agrif_Parent(nn_bathy)
         rn_bathy = Agrif_Parent(rn_bathy)
         ppglam0 = Agrif_Parent(ppglam0)
         ppgphi0 = Agrif_Parent(ppgphi0) 
         ppe1_deg = Agrif_Parent(ppe1_deg)/Agrif_Rhox()
         ppe2_deg = Agrif_Parent(ppe2_deg)/Agrif_Rhoy()
         ppe1_m = Agrif_Parent(ppe1_m)/Agrif_Rhox()
         ppe2_m = Agrif_Parent(ppe2_m)/Agrif_Rhoy() 
      ENDIF
#endif


      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) '   Namelist namdom : space & time domain'
         WRITE(numout,*) '      flag read/compute bathymetry      nn_bathy     = ', nn_bathy
         IF( nn_bathy == 1 ) THEN
            WRITE(numout,*) '   read bathymetry from file      cn_topo      = ' ,TRIM(cn_topo)
            WRITE(numout,*) '   bathymetry name in file        cn_bath      = ' ,TRIM(cn_bath)
            WRITE(numout,*) '   read isf draft from file       cn_fisfd     = ' ,TRIM(cn_fisfd)
            WRITE(numout,*) '   isf draft name in file         cn_visfd     = ' ,TRIM(cn_visfd)
         ELSE IF( nn_bathy == 2 ) THEN
            WRITE(numout,*) '   compute bathymetry from file      cn_topo      = ' , cn_topo
            WRITE(numout,*) '   bathymetry name in file           cn_bath      = ' , cn_bath
            WRITE(numout,*) '   longitude name in file            cn_lon       = ' , cn_lon
            WRITE(numout,*) '   latitude  name in file            cn_lat       = ' , cn_lat
            WRITE(numout,*) '   bathmetry scale factor            rn_scale     = ' , rn_scale 
         ENDIF   
         WRITE(numout,*) '      Depth (if =0 bathy=jpkm1)         rn_bathy     = ', rn_bathy
         WRITE(numout,*) '      min depth of the ocean    (>0) or    rn_hmin   = ', rn_hmin
         WRITE(numout,*) '      min number of ocean level (<0)       '
         WRITE(numout,*) '      minimum thickness of partial      rn_e3zps_min = ', rn_e3zps_min, ' (m)'
         WRITE(numout,*) '         step level                     rn_e3zps_rat = ', rn_e3zps_rat
         WRITE(numout,*) '      create mesh/mask file(s)          nn_msh       = ', nn_msh
         WRITE(numout,*) '           = 0   no file created           '
         WRITE(numout,*) '           = 1   mesh_mask                 '
         WRITE(numout,*) '           = 2   mesh and mask             '
         WRITE(numout,*) '           = 3   mesh_hgr, msh_zgr and mask'
         WRITE(numout,*) '      type of horizontal mesh jphgr_msh           = ', jphgr_msh
         WRITE(numout,*) '      longitude of first raw and column T-point ppglam0 = ', ppglam0
         WRITE(numout,*) '      latitude  of first raw and column T-point ppgphi0 = ', ppgphi0
         WRITE(numout,*) '      zonal      grid-spacing (degrees) ppe1_deg        = ', ppe1_deg
         WRITE(numout,*) '      meridional grid-spacing (degrees) ppe2_deg        = ', ppe2_deg
         WRITE(numout,*) '      zonal      grid-spacing (degrees) ppe1_m          = ', ppe1_m
         WRITE(numout,*) '      meridional grid-spacing (degrees) ppe2_m          = ', ppe2_m
         WRITE(numout,*) '      ORCA r4, r2 and r05 coefficients  ppsur           = ', ppsur
         WRITE(numout,*) '                                        ppa0            = ', ppa0
         WRITE(numout,*) '                                        ppa1            = ', ppa1
         WRITE(numout,*) '                                        ppkth           = ', ppkth
         WRITE(numout,*) '                                        ppacr           = ', ppacr
         WRITE(numout,*) '      Minimum vertical spacing ppdzmin                  = ', ppdzmin
         WRITE(numout,*) '      Maximum depth pphmax                              = ', pphmax
         WRITE(numout,*) '      Use double tanf function for vertical coordinates ldbletanh = ', ldbletanh
         WRITE(numout,*) '      Double tanh function parameters ppa2              = ', ppa2
         WRITE(numout,*) '                                      ppkth2            = ', ppkth2
         WRITE(numout,*) '                                      ppacr2            = ', ppacr2
      ENDIF
      !
      ntopo     = nn_bathy          ! conversion DOCTOR names into model names (this should disappear soon)
      e3zps_min = rn_e3zps_min
      e3zps_rat = rn_e3zps_rat
      nmsh      = nn_msh
      atfp      = rn_atfp
      rdt       = rn_rdt

      snc4set%luse = .FALSE.        ! No NetCDF 4 case
      !
   END SUBROUTINE dom_nam


   SUBROUTINE dom_ctl
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE dom_ctl  ***
      !!
      !! ** Purpose :   Domain control.
      !!
      !! ** Method  :   compute and print extrema of masked scale factors
      !!----------------------------------------------------------------------
      INTEGER ::   iimi1, ijmi1, iimi2, ijmi2, iima1, ijma1, iima2, ijma2
      INTEGER, DIMENSION(2) ::   iloc   ! 
      REAL(wp) ::   ze1min, ze1max, ze2min, ze2max
      !!----------------------------------------------------------------------
      !
#undef CHECK_DOM
#ifdef CHECK_DOM
      IF(lk_mpp) THEN
         CALL mpp_minloc( 'dom_ctl', e1t(:,:), tmask_i(:,:), ze1min, iloc )
         iimi1 = iloc(1) ; ijmi1 = iloc(2)
         CALL mpp_minloc( 'dom_ctl', e2t(:,:), tmask_i(:,:), ze2min, iloc )
         iimi2 = iloc(1) ; ijmi2 = iloc(2)
         CALL mpp_maxloc( 'dom_ctl', e1t(:,:), tmask_i(:,:), ze1max, iloc )
         iima1 = iloc(1) ; ijma1 = iloc(2)
         CALL mpp_maxloc( 'dom_ctl', e2t(:,:), tmask_i(:,:), ze2max, iloc )
         iima2 = iloc(1) ; ijma2 = iloc(2)
      ELSE
         ze1min = MINVAL( e1t(:,:), mask = tmask_i(:,:) == 1._wp )    
         ze2min = MINVAL( e2t(:,:), mask = tmask_i(:,:) == 1._wp )    
         ze1max = MAXVAL( e1t(:,:), mask = tmask_i(:,:) == 1._wp )    
         ze2max = MAXVAL( e2t(:,:), mask = tmask_i(:,:) == 1._wp )    

         iloc  = MINLOC( e1t(:,:), mask = tmask_i(:,:) == 1._wp )
         iimi1 = iloc(1) + nimpp - 1
         ijmi1 = iloc(2) + njmpp - 1
         iloc  = MINLOC( e2t(:,:), mask = tmask_i(:,:) == 1._wp )
         iimi2 = iloc(1) + nimpp - 1
         ijmi2 = iloc(2) + njmpp - 1
         iloc  = MAXLOC( e1t(:,:), mask = tmask_i(:,:) == 1._wp )
         iima1 = iloc(1) + nimpp - 1
         ijma1 = iloc(2) + njmpp - 1
         iloc  = MAXLOC( e2t(:,:), mask = tmask_i(:,:) == 1._wp )
         iima2 = iloc(1) + nimpp - 1
         ijma2 = iloc(2) + njmpp - 1
      ENDIF
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'dom_ctl : extrema of the masked scale factors'
         WRITE(numout,*) '~~~~~~~'
         WRITE(numout,"(14x,'e1t maxi: ',1f10.2,' at i = ',i5,' j= ',i5)") ze1max, iima1, ijma1
         WRITE(numout,"(14x,'e1t mini: ',1f10.2,' at i = ',i5,' j= ',i5)") ze1min, iimi1, ijmi1
         WRITE(numout,"(14x,'e2t maxi: ',1f10.2,' at i = ',i5,' j= ',i5)") ze2max, iima2, ijma2
         WRITE(numout,"(14x,'e2t mini: ',1f10.2,' at i = ',i5,' j= ',i5)") ze2min, iimi2, ijmi2
      ENDIF
#endif
      !
      ! check that all processes are still there... If some process have an error,
      ! they will never enter in cfg_write
      IF( lk_mpp )   CALL mpp_max( 'nemogcm',nstop )
      IF (nstop /= 0) THEN
         WRITE(numout,*) ''
         WRITE(numout,*) '========================================================'
         WRITE(numout,*) 'E R R O R : ',nstop, ' error have been found'
         WRITE(numout,*) '========================================================'
         WRITE(numout,*) ''
         IF ( lk_mpp ) THEN
            CALL mppstop()
         ELSE
            STOP 123
         END IF
      END IF
      !
   END SUBROUTINE dom_ctl


   SUBROUTINE cfg_write
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE cfg_write  ***
      !!                   
      !! ** Purpose :   Create the "domain_cfg" file, a NetCDF file which 
      !!              contains all the ocean domain informations required to 
      !!              define an ocean configuration.
      !!
      !! ** Method  :   Write in a file all the arrays required to set up an
      !!              ocean configuration.
      !!
      !! ** output file :   domain_cfg.nc : domain size, characteristics,horizontal mesh,
      !!                              Coriolis parameter, and vertical scale factors
      !!                              NB: also contains ORCA family information (if cp_cfg = "ORCA")
      !!                              and depths (ln_e3_dep=F) 
      !!----------------------------------------------------------------------
      INTEGER           ::   ji, jj, jk   ! dummy loop indices
      INTEGER           ::   izco, izps, isco, icav
      INTEGER           ::   inum     ! temprary units for 'domain_cfg.nc' file
      CHARACTER(len=21) ::   clnam    ! filename (mesh and mask informations)
      REAL(wp), DIMENSION(jpi,jpj) ::   z2d   ! workspace
      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'cfg_write : create the "domain_cfg.nc" file containing all required configuration information'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~'
      !
      !                       ! ============================= !
      !                       !  create 'domain_cfg.nc' file  !
      !                       ! ============================= !
      !         
      clnam = 'domain_cfg'  ! filename (configuration information)
      CALL iom_open( TRIM(clnam), inum, ldwrt = .TRUE.)!, kiolib = jprstlib )
      
      !
      !                             !==  ORCA family specificities  ==!
      IF( cp_cfg == "ORCA" ) THEN
         CALL iom_rstput( 0, 0, inum, 'ORCA'      , 1._wp            , ktype = jp_i4 )
         CALL iom_rstput( 0, 0, inum, 'ORCA_index', REAL( jp_cfg, wp), ktype = jp_i4 )         
      ENDIF
      !                             !==  global domain size  ==!
      !
      CALL iom_rstput( 0, 0, inum, 'jpiglo', REAL( jpiglo, wp), ktype = jp_i4 )
      CALL iom_rstput( 0, 0, inum, 'jpjglo', REAL( jpjglo, wp), ktype = jp_i4 )
      CALL iom_rstput( 0, 0, inum, 'jpkglo', REAL( jpk   , wp), ktype = jp_i4 )
      !
      !                             !==  domain characteristics  ==!
      !
      !                                   ! lateral boundary of the global
      !                                   domain
      CALL iom_rstput( 0, 0, inum, 'jperio', REAL( jperio, wp), ktype = jp_i4 )
      !
      !                                   ! type of vertical coordinate
      IF( ln_zco    ) THEN   ;   izco = 1   ;   ELSE   ;   izco = 0   ;   ENDIF
      IF( ln_zps    ) THEN   ;   izps = 1   ;   ELSE   ;   izps = 0   ;   ENDIF
      IF( ln_sco    ) THEN   ;   isco = 1   ;   ELSE   ;   isco = 0   ;   ENDIF
      CALL iom_rstput( 0, 0, inum, 'ln_zco'   , REAL( izco, wp), ktype = jp_i4 )
      CALL iom_rstput( 0, 0, inum, 'ln_zps'   , REAL( izps, wp), ktype = jp_i4 )
      CALL iom_rstput( 0, 0, inum, 'ln_sco'   , REAL( isco, wp), ktype = jp_i4 )
      !
      !                                   ! ocean cavities under iceshelves
      IF( ln_isfcav ) THEN   ;   icav = 1   ;   ELSE   ;   icav = 0   ;   ENDIF
      CALL iom_rstput( 0, 0, inum, 'ln_isfcav', REAL( icav, wp), ktype = jp_i4 )
      !
      !                             !==  horizontal mesh  !
      !
      CALL iom_rstput( 0, 0, inum, 'glamt', glamt, ktype = jp_r8 )   ! latitude
      CALL iom_rstput( 0, 0, inum, 'glamu', glamu, ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'glamv', glamv, ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'glamf', glamf, ktype = jp_r8 )
      !                                
      CALL iom_rstput( 0, 0, inum, 'gphit', gphit, ktype = jp_r8 )   ! longitude
      CALL iom_rstput( 0, 0, inum, 'gphiu', gphiu, ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'gphiv', gphiv, ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'gphif', gphif, ktype = jp_r8 )
      !                                
      CALL iom_rstput( 0, 0, inum, 'e1t'  , e1t  , ktype = jp_r8 )   ! i-scale factors (e1.)
      CALL iom_rstput( 0, 0, inum, 'e1u'  , e1u  , ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'e1v'  , e1v  , ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'e1f'  , e1f  , ktype = jp_r8 )
      !
      CALL iom_rstput( 0, 0, inum, 'e2t'  , e2t  , ktype = jp_r8 )   ! j-scale factors (e2.)
      CALL iom_rstput( 0, 0, inum, 'e2u'  , e2u  , ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'e2v'  , e2v  , ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'e2f'  , e2f  , ktype = jp_r8 )
      !
      CALL iom_rstput( 0, 0, inum, 'ff_f' , ff_f , ktype = jp_r8 )   ! coriolis factor
      CALL iom_rstput( 0, 0, inum, 'ff_t' , ff_t , ktype = jp_r8 )
      !
      !                             !==  vertical mesh  ==!
      !                                                     
      CALL iom_rstput( 0, 0, inum, 'e3t_1d'  , e3t_1d  , ktype = jp_r8 )   !  reference 1D-coordinate
      CALL iom_rstput( 0, 0, inum, 'e3w_1d'  , e3w_1d  , ktype = jp_r8 )
      !
      CALL iom_rstput( 0, 0, inum, 'e3t_0'   , e3t_0   , ktype = jp_r8 )   !  vertical scale factors (e
      CALL iom_rstput( 0, 0, inum, 'e3u_0'   , e3u_0   , ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'e3v_0'   , e3v_0   , ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'e3f_0'   , e3f_0   , ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'e3w_0'   , e3w_0   , ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'e3uw_0'  , e3uw_0  , ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'e3vw_0'  , e3vw_0  , ktype = jp_r8 )
      !
      IF(.NOT.ln_e3_dep ) THEN                                             !  depth (t- & w-points)
         CALL iom_rstput( 0, 0, inum, 'gdept_1d', gdept_1d, ktype = jp_r8 )   ! required only with  
         CALL iom_rstput( 0, 0, inum, 'gdepw_1d', gdepw_1d, ktype = jp_r8 )   ! the old e3. definition
         CALL iom_rstput( 0, 0, inum, 'gdept_0' , gdept_0 , ktype = jp_r8 )
         CALL iom_rstput( 0, 0, inum, 'gdepw_0' , gdepw_0 , ktype = jp_r8 )
      ENDIF
      !                                         
      !                             !==  ocean top and bottom level  ==!
      !
      CALL iom_rstput( 0, 0, inum, 'bottom_level' , REAL( mbkt, wp )*ssmask , ktype = jp_i4 )   ! nb of ocean T-points
#if defined key_agrif
!!      IF ( Agrif_level() /= Agrif_maxlevel() ) THEN
         CALL iom_rstput( 0, 0, inum, 'mbku'         , REAL( mbku, wp )*ssumask, ktype = jp_i4 )   ! nb of ocean U-points
         CALL iom_rstput( 0, 0, inum, 'mbkv'         , REAL( mbkv, wp )*ssvmask, ktype = jp_i4 )   ! nb of ocean V-points
         CALL iom_rstput( 0, 0, inum, 'mbkf'         , REAL( mbkf, wp )*ssfmask, ktype = jp_i4 )   ! nb of ocean F-points
!!      ENDIF
#endif
      CALL iom_rstput( 0, 0, inum, 'top_level'    , REAL( mikt, wp )*ssmask , ktype = jp_i4 )   ! nb of ocean T-points (ISF)
      CALL iom_rstput( 0, 0, inum, 'isf_draft'    , risfdep , ktype = jp_r8 )
      DO jj = 1,jpj
         DO ji = 1,jpi
            z2d (ji,jj) = SUM ( e3t_0(ji,jj, 1:mbkt(ji,jj) ) ) * ssmask(ji,jj) 
         END DO
      END DO
      CALL iom_rstput( 0, 0, inum, 'bathy_metry'   , z2d , ktype = jp_r8 )
      !
      !                              !== closed sea ==!
      IF (ln_domclo) THEN
         ! mask for the open sea
         CALL iom_rstput( 0, 0, inum, 'mask_opensea' , msk_opnsea  , ktype = jp_i4 )
         ! mask for all the under closed sea
         CALL iom_rstput( 0, 0, inum, 'mask_csundef' , msk_csundef , ktype = jp_i4 )
         ! mask for global, local net precip, local net precip and evaporation correction
         CALL iom_rstput( 0, 0, inum, 'mask_csglo'   , msk_csglo   , ktype = jp_i4 )
         CALL iom_rstput( 0, 0, inum, 'mask_csemp'   , msk_csemp   , ktype = jp_i4 )
         CALL iom_rstput( 0, 0, inum, 'mask_csrnf'   , msk_csrnf   , ktype = jp_i4 )
         ! mask for the various river mouth (in case multiple lake in the same outlet)
         CALL iom_rstput( 0, 0, inum, 'mask_csgrpglo', msk_csgrpglo, ktype = jp_i4 )
         CALL iom_rstput( 0, 0, inum, 'mask_csgrpemp', msk_csgrpemp, ktype = jp_i4 )
         CALL iom_rstput( 0, 0, inum, 'mask_csgrprnf', msk_csgrprnf, ktype = jp_i4 )
      END IF
      !
      !                                ! ============================
      !                                !        close the files 
      !                                ! ============================
      CALL iom_close( inum )
      !
   END SUBROUTINE cfg_write

   !!======================================================================
END MODULE domain
