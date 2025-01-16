MODULE sedini
   !!======================================================================
   !!              ***  MODULE  sedini  ***
   !! Sediment : define sediment variables
   !!=====================================================================

   !!----------------------------------------------------------------------
   !!   sed_ini    : initialization, namelist read, and parameters control
   !!----------------------------------------------------------------------
   !! * Modules used
   USE sed     ! sediment global variable
   USE sed_oce
   USE sedarr
   USE sedadv
   USE trcdmp_sed
   USE trcdta
   USE iom
   USE lib_mpp         ! distribued memory computing library


   IMPLICIT NONE
   PRIVATE

   !! Module variables
   REAL(wp), PUBLIC :: sedmask 

   REAL(wp)    ::  &
      sedzmin = 0.3    ,  &  !: Minimum vertical spacing
      sedhmax = 10.0   ,  &  !: Maximum depth of the sediment
      sedkth  = 5.0    ,  &  !: Default parameters
      sedacr  = 3.0          !: Default parameters
      
   REAL(wp)    ::  &
      porsurf =  0.95  ,  &  !: Porosity at the surface
      porinf  =  0.75  ,  &  !: Porosity at infinite depth
      rhox    =  2.0         !: Vertical length scale of porosity variation 

   REAL(wp)    ::  &
      rcopal  =   40.        !: reactivity for si    [l.mol-1.an-1]

   REAL(wp), PUBLIC    ::  &
      redO2    =  138.  ,  &  !: Redfield coef for Oxygen
      redNo3   =   16.  ,  &  !: Redfield coef for Nitrate
      redPo4   =    1.  ,  &  !: Redfield coef for Phosphate
      redC     =  117.  ,  &  !: Redfield coef for Carbon
      redfep   =  0.175 ,  &  !: Ratio for iron bound phosphorus
      rcorgl   =   50.  ,  &  !: reactivity for POC/O2 [l.mol-1.an-1]    
      rcorgs   =   0.5  ,  &  !: reactivity of the semi-labile component
      rcorgr   =   1E-4 ,  &  !: reactivity of the refractory component
      rcnh4    =   10E6 ,  &  !: reactivity for O2/NH4 [l.mol-1.an-1]  
      rch2s    =   1.E5 ,  &  !: reactivity for O2/ODU [l.mol-1.an-1] 
      rcfe2    =   5.E8 ,  &  !: reactivity for O2/Fe2+ [l.mol-1.an-1]
      rcfeh2s  =   1.E4 ,  &  !: Reactivity for FEOH/H2S [l.mol-1.an-1]
      rcfeso   =   3.E5 ,  &  !: Reactivity for FES/O2 [l.mol-1.an-1]
      rcfesp   =   5E-6 ,  &  !: Precipitation of FeS [mol/l-1.an-1]
      rcfesd   =   1E-3 ,  &  !: Dissolution of FeS [an-1]
      xksedo2  =   5E-6 ,  &  !: half-sturation constant for oxic remin.
      xksedno3 =   5E-6 ,  &  !: half-saturation constant for denitrification
      xksedfeo =   0.6  ,  & !: half-saturation constant for iron remin
      xksedso4 =   2E-3       !: half-saturation constant for SO4 remin

   REAL(wp)    ::  &
      rccal   = 1000.,      & !: reactivity for calcite         [l.mol-1.an-1]
      rcligc  = 1.E-4         !: L/C ratio in POC

   REAL(wp), PUBLIC    ::  dbiot   = 15. , &  !: coefficient for bioturbation    [cm**2.(n-1)]
      dbtbzsc =  10.0  ,    &  !: Vertical scale of variation. If no variation, mixed layer in the sed [cm]
      xirrzsc = 2.0            !: Vertical scale of irrigation variation.
   REAL(wp)    ::  &
      ryear = 365. * 24. * 3600. !:  1 year converted in second

   REAL(wp), DIMENSION(jpwat), PUBLIC  :: diff1
   DATA diff1/ 1.104E-5, 9.78E-6, 3.58E-6, 9.8E-6, 9.73E-6, 5.0E-6, 3.31E-6, 4.81E-6, 4.81E-6, 4.81E-6, 4.59E-6 /


   REAL(wp), DIMENSION(jpwat), PUBLIC  :: diff2
   DATA diff2/ 4.47E-7, 3.89E-7, 1.77E-7, 3.89E-7, 3.06E-7, 2.5E-7, 1.5E-7, 2.51E-7, 2.51E-7, 2.51E-7, 1.74E-7 /

   !! *  Routine accessibility
   PUBLIC sed_ini          ! routine called by opa.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !! $Id: sedini.F90 15450 2021-10-27 14:32:08Z cetlod $
CONTAINS


   SUBROUTINE sed_ini
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_ini  ***
      !!
      !! ** Purpose :  Initialization of sediment module
      !!               - Reading namelist
      !!               - Read the deepest water layer thickness
      !!                 ( using as mask ) in Netcdf file
      !!               - Convert unity if necessary
      !!               - sets initial sediment composition
      !!                 ( only clay or reading restart file )
      !!               - sets sediment grid, porosity and others constants
      !!
      !!   History :
      !!        !  04-10  (N. Emprin, M. Gehlen )  Original code
      !!        !  06-07  (C. Ethe)  Re-organization
      !!----------------------------------------------------------------------
      INTEGER  :: ji, jj, js, jn, jk, ikt, ierr
      REAL(wp) :: ztmp1, ztmp2 
      !!----------------------------------------------------------------------

      ! Reading namelist.sed variables
      !---------------------------------------

      CALL ctl_opn( numsed, 'sediment.output', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )

      IF (lwp) THEN
         WRITE(numsed,*)
         WRITE(numsed,*) '                 PISCES framework'
         WRITE(numsed,*) '                 SEDIMENT model'
         WRITE(numsed,*) '                version 3.0  (2018) '
         WRITE(numsed,*)
         WRITE(numsed,*)
      ENDIF

      IF(lwp) WRITE(numsed,*) ' sed_ini : Initialization of sediment module  '
      IF(lwp) WRITE(numsed,*) ' '

      ! Read sediment Namelist
      !-------------------------
      CALL sed_ini_nam

      ! Allocate SEDIMENT arrays
      ierr =        sed_alloc()
      ierr = ierr + sed_oce_alloc()
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'sed_ini: unable to allocate sediment model arrays' )

      ! Determination of sediments number of points and allocate global variables
      epkbot(:,:) = 0.
      gdepbot(:,:) = 0.
      DO_2D( 0, 0, 0, 0 )
         ikt = mbkt(ji,jj) 
         IF( tmask(ji,jj,ikt) == 1 ) epkbot(ji,jj) = e3t_0(ji,jj,ikt)
         gdepbot(ji,jj) = gdepw_0(ji,jj,ikt+1)
      END_2D

      ! computation of total number of ocean points
      !--------------------------------------------
      sedmask = 0.
      IF ( COUNT( epkbot(:,:) > 0. ) == 0 ) THEN 
          sedmask = 0.
      ELSE
          sedmask = 1.
      ENDIF 
      jpoce  = MAX( COUNT( epkbot(:,:) > 0. ) , 1 )

      ! Allocate memory size of global variables
      ALLOCATE( pwcp (jpoce,jpksed,jpwat) )  ;  ALLOCATE( pwcp_dta  (jpoce,jpwat) )
      ALLOCATE( pwcpa(jpoce,jpksed,jpwat) )  ;  ALLOCATE( solcpa(jpoce,jpksed,jpsol) )
      ALLOCATE( solcp(jpoce,jpksed,jpsol) )  ;  ALLOCATE( rainrm_dta(jpoce,jpsol) )
      ALLOCATE( rainrg(jpoce,jpsol) )
      ALLOCATE( dzdep(jpoce) )               ;  ALLOCATE( iarroce(jpoce) )             ;  ALLOCATE( dzkbot(jpoce) )
      ALLOCATE( slatit(jpoce) )              ;  ALLOCATE( slongit(jpoce) )
      ALLOCATE( zkbot(jpoce) )               ;  ALLOCATE( db(jpoce,jpksed) )
      ALLOCATE( temp(jpoce) )                ;  ALLOCATE( salt(jpoce) )  
      ALLOCATE( diff(jpoce,jpksed,jpwat ) )  ;  ALLOCATE( irrig(jpoce, jpksed) )
      ALLOCATE( wacc(jpoce) )                ;  ALLOCATE( fecratio(jpoce) )
      ALLOCATE( densSW(jpoce) )   ;   ALLOCATE( saturco3(jpoce,jpksed) ) 
      ALLOCATE( hipor(jpoce,jpksed) )        ;  ALLOCATE( co3por(jpoce,jpksed) )
      ALLOCATE( dz3d(jpoce,jpksed) )         ;  ALLOCATE( volw3d(jpoce,jpksed) )       ;  ALLOCATE( vols3d(jpoce,jpksed) )
      ALLOCATE( rearatpom(jpoce, jpksed) )   ;  ALLOCATE( volc(jpoce,jpksed,jpsol) )
      ALLOCATE( radssol(jpksed, jpwat) )     ;  ALLOCATE( rads1sol(jpksed, jpwat) )
      ALLOCATE( apluss(jpoce, jpksed) )      ;  ALLOCATE( aminuss(jpoce,jpksed) )

      ! Initialization of global variables
      pwcp  (:,:,:) = 0.   ;  pwcp_dta  (:,:) = 0.  
      pwcpa (:,:,:) = 0.   ;  solcpa(:,:,:) = 0.
      solcp (:,:,:) = 0.   ;  rainrm_dta(:,:) = 0.
      rainrg(:,:  ) = 0.
      dzdep (:    ) = 0.   ;  iarroce(:   ) = 0   ; dzkbot    (:  ) = 0.
      temp  (:    ) = 0.   ;  salt   (:   ) = 0.  ; zkbot     (:  ) = 0.
      densSW (:   ) = 0.   ;  db     (:,:) = 0. 
      hipor (:,:  ) = 0.   ;  co3por (:,: ) = 0.  ; irrig     (:,:) = 0. 
      dz3d  (:,:  ) = 0.   ;  volw3d (:,: ) = 0.  ; vols3d    (:,:) = 0. 
      fecratio(:)   = 1E-5 ;  rearatpom(:,:) = 0. 
      radssol(:,:)  = 1.0  ;  rads1sol(:,:) = 0.
      apluss(:,:)   = 0.0  ;  aminuss(:,:)  = 0.0

      ! Chemical variables      
      ALLOCATE( akbs  (jpoce) )  ;  ALLOCATE( ak1s   (jpoce) )  ;  ALLOCATE( ak2s  (jpoce) ) ;  ALLOCATE( akws  (jpoce) )     
      ALLOCATE( ak1ps (jpoce) )  ;  ALLOCATE( ak2ps  (jpoce) )  ;  ALLOCATE( ak3ps (jpoce) ) ;  ALLOCATE( aksis (jpoce) )    
      ALLOCATE( aksps (jpoce) )  ;  ALLOCATE( ak12s  (jpoce) )  ;  ALLOCATE( ak12ps(jpoce) ) ;  ALLOCATE( ak123ps(jpoce) )    
      ALLOCATE( borats(jpoce) )  ;  ALLOCATE( calcon2(jpoce) )  ;  ALLOCATE( sieqs (jpoce) ) 
      ALLOCATE( aks3s(jpoce) )   ;  ALLOCATE( akf3s(jpoce) )    ;  ALLOCATE( sulfats(jpoce) )
      ALLOCATE( fluorids(jpoce) ) ; ALLOCATE( akh2s(jpoce) )    ;  ALLOCATE( aknh3(jpoce) )

      akbs  (:) = 0. ;   ak1s   (:) = 0. ;  ak2s  (:) = 0. ;   akws   (:) = 0.
      ak1ps (:) = 0. ;   ak2ps  (:) = 0. ;  ak3ps (:) = 0. ;   aksis  (:) = 0.
      aksps (:) = 0. ;   ak12s  (:) = 0. ;  ak12ps(:) = 0. ;   ak123ps(:) = 0.
      borats(:) = 0. ;   calcon2(:) = 0. ;  sieqs (:) = 0. ;   akh2s  (:) = 0.
      aks3s(:)  = 0. ;   akf3s(:)   = 0. ;  sulfats(:) = 0. ;  fluorids(:) = 0.
      aknh3(:)  = 0.

      ! Mass balance calculation  
      ALLOCATE( fromsed(jpoce, jpsol+jpads) ) ; ALLOCATE( tosed(jpoce, jpsol+jpads) )

      fromsed(:,:) = 0.    ;   tosed(:,:) = 0.

      ! Initialization of sediment geometry
      !------------------------------------
      CALL sed_ini_geom

      ! Offline specific mode
      ! ---------------------
      ln_sediment_offline = .FALSE.

      !---------------------------------------------
      ! Molecular weight [g/mol] for solid species
      !---------------------------------------------

      ! opal=sio2*0.4(h20)=28+2*16+0.4*(2+16)
      !---------------------------------------
      mol_wgt(jsopal) = 28. + 2. * 16. + 0.4 * ( 2. + 16. )

      !  clay
      !  some kind of Illit (according to Pape)
      !  K0.58(Al 1.38 Fe(III)0.37Fe(II)0.04Mg0.34)[(OH)2|(Si3.41Al0.59)O10]
      !--------------------------------------------------------------------
      mol_wgt(jsclay) = 0.58 * 39. + 1.38 * 27. + ( 0.37 + 0.04 ) * 56.+ &
         &              0.34 * 24. + 2. * ( 16. + 1. ) + 3.41 * 38. +    &
         &              0.59 * 27. + 10. * 16.

      mol_wgt(jsfeo)  = 55.0 + 3.0 * ( 16.0 + 1.0)


      mol_wgt(jsfes)  = 55.0 + 32.0

      ! for chemistry Poc : C(122)H(244)O(86)N(16)P(1)
      ! But den sity of Poc is an Hydrated material (= POC + 30H2O)
      ! So C(122)H(355)O(120)N(16)P(1)
      !------------------------------------------------------------
      mol_wgt(jspoc) = ( redC * 12. + 355. + 120. * 16. + redNo3 * 14. + 31. ) / redC
      mol_wgt(jspos) = mol_wgt(jspoc)
      mol_wgt(jspor) = mol_wgt(jspoc)

      ! CaCO3
      !---------
      mol_wgt(jscal) = 40. + 12. + 3. * 16.

      ! Density of solid material in sediment [g/cm**3]
      !------------------------------------------------
      ALLOCATE( dens_sol(jpsol) )
      dens_sol(jsclay) = 2.6
      dens_sol(jscal)  = 2.7
      dens_sol(jsopal) = 2.1 
      dens_sol(jsfeo)  = 3.4
      dens_sol(jsfes)  = 4.8
      dens_sol(jspoc)  = 1.0
      dens_sol(jspos)  = 1.0
      dens_sol(jspor)  = 1.0

      ! Accumulation rate from Burwicz et al. (2011). This is used to
      ! compute the flux of clays and minerals
      ! --------------------------------------------------------------
      DO ji = 1, jpoce
          ztmp1 = 0.0117 * 3.0 / ( 1.0 + ( zkbot(ji) / 200.)**3 )
          ztmp2 = 0.006 / ( 1.0 + ( zkbot(ji) / 5000.)**10 )
          wacc(ji) = ztmp2+ztmp1
      END DO

      ! Vertical profile of of the adsorption factor for adsorbed species
      ! -----------------------------------------------------------------
      radssol(:,jwfe2) = 1.0 / ( 1.0 + adsfe2 * por1(:) / por(:) )
      radssol(:,jwnh4) = 1.0 / ( 1.0 + adsnh4 * por1(:) / por(:) )
      rads1sol(:,jwnh4) = adsnh4 * por1(:) / ( por(:) + adsnh4 * por1(:) )
      rads1sol(:,jwfe2) = adsfe2 * por1(:) / ( por(:) + adsfe2 * por1(:) )

      ! Initialization of the array for non linear solving
      ! --------------------------------------------------

      ALLOCATE( jarr(jpksed*jpvode,2) )
      ALLOCATE( jsvode(jpvode), isvode(jptrased) )
      jsvode(1) = jwoxy ; jsvode(2) = jwno3 ; jsvode(3) = jwnh4
      jsvode(4) = jwh2s ; jsvode(5) = jwso4 ; jsvode(6) = jwfe2
      jsvode(7) = jpwat+jsfeo ; jsvode(8) = jpwat+jsfes
      isvode(jwoxy) = 1 ; isvode(jwno3) = 2 ; isvode(jwnh4) = 3
      isvode(jwh2s) = 4 ; isvode(jwso4) = 5 ; isvode(jwfe2) = 6
      isvode(jpwat+jsfeo) = 7 ; isvode(jpwat+jsfes) = 8
      DO js = 1, jpvode
         DO jk = 1, jpksed
            jn = (jk-1) * jpvode + js
            jarr(jn,1) = jk
            jarr(jn,2) = jsvode(js)
         END DO
      END DO

      ALLOCATE( rstepros(jpoce) )

#if defined key_sed_off
      ln_sediment_offline = .TRUE.
      IF (lwp) write(numsed,*) 'Sediment module is run in offline mode'
      IF (lwp) write(numsed,*) 'key_sed_off is activated at compilation time'
      IF (lwp) write(numsed,*) 'ln_sed_2way is forced to false'
      IF (lwp) write(numsed,*) '--------------------------------------------'
      ln_sed_2way = .FALSE.
#endif
      ! Initialisation of tracer damping
      ! --------------------------------
      IF (ln_sediment_offline) THEN
         CALL trc_dmp_sed_ini
      ENDIF

   END SUBROUTINE sed_ini

   SUBROUTINE sed_ini_geom
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_ini_geom  ***
      !!
      !! ** Purpose :  Initialization of sediment geometry
      !!               - Read the deepest water layer thickness
      !!                 ( using as mask ) in Netcdf file
      !!               - sets sediment grid, porosity and molecular weight
      !!                 and others constants
      !!
      !!   History :
      !!        !  06-07  (C. Ethe)  Original
      !!----------------------------------------------------------------------
      !! * Modules used
      !! * local declarations
      INTEGER  :: ji, jj, jk, jn
      REAL(wp) :: za0, za1, zt, zw, zsum, zsur, zprof, zprofw
      REAL(wp) :: ztmp1, ztmp2
      !---------------------------------------------------------- 

      IF(lwp) WRITE(numsed,*) ' sed_ini_geom : Initialization of sediment geometry '
      IF(lwp) WRITE(numsed,*) ' '

      ! Computation of 1D array of sediments points
      indoce = 0
      DO_2D( 0, 0, 0, 0 )
         IF (  epkbot(ji,jj) > 0. ) THEN
            indoce          = indoce + 1
            iarroce(indoce) = (jj - 1) * jpi + ji
         ENDIF
      END_2D

      IF ( indoce .EQ. 0 ) THEN
         indoce = 1
         iarroce(indoce) = 1
      ENDIF

      IF( indoce .NE. jpoce ) THEN
         CALL ctl_stop( 'STOP', 'sed_ini: number of ocean points indoce doesn''t match  number of point' )
      ELSE
         IF (lwp) WRITE(numsed,*) ' '
         IF (lwp) WRITE(numsed,*) ' total number of ocean points jpoce =  ',jpoce
         IF (lwp) WRITE(numsed,*) ' '
      ENDIF

      ! initialization of dzkbot in [cm]
      !------------------------------------------------    
      CALL pack_arr ( jpoce, dzkbot(1:jpoce), epkbot(1:jpi,1:jpj), iarroce(1:jpoce) )
#if defined key_sed_off
      dzkbot(1:jpoce) = 1.E8
#else
      dzkbot(1:jpoce) = dzkbot(1:jpoce) * 1.e+2 
#endif
      CALL pack_arr ( jpoce, zkbot(1:jpoce), gdepbot(1:jpi,1:jpj), iarroce(1:jpoce) )
      CALL pack_arr ( jpoce, slatit(1:jpoce), gphit(1:jpi,1:jpj), iarroce(1:jpoce) )
      CALL pack_arr ( jpoce, slongit(1:jpoce), glamt(1:jpi,1:jpj), iarroce(1:jpoce) )

      ! Geometry and  constants 
      ! sediment layer thickness [cm]
      ! (1st layer= diffusive layer = pur water) 
      !------------------------------------------
      za1  = (  sedzmin - sedhmax / FLOAT(jpksed-1)  )                                                      &
         & / ( TANH((1-sedkth)/sedacr) - sedacr/FLOAT(jpksed-1) * (  LOG( COSH( (jpksed - sedkth) / sedacr) )      &
         &                                                   - LOG( COSH( ( 1  - sedkth) / sedacr) )  )  )
      za0  = sedzmin - za1 * TANH( (1-sedkth) / sedacr )
      zsur = - za0 - za1 * sedacr * LOG( COSH( (1-sedkth) / sedacr )  )

      dz(1)       = 0.1
      profsedw(1) = 0.0
      profsed(1)  = -dz(1) / 2.
      DO jk = 2, jpksed
         zw = REAL( jk , wp )
         zt = REAL( jk , wp ) - 0.5_wp
         profsed(jk)  = ( zsur + za0 * zt + za1 * sedacr * LOG ( COSH( (zt-sedkth) / sedacr ) )  ) 
         profsedw(jk) = ( zsur + za0 * zw + za1 * sedacr * LOG ( COSH( (zw-sedkth) / sedacr ) )  )
         dz(jk) = profsedw(jk) - profsedw(jk-1)
      END DO

      DO ji = 1, jpoce
         dz3d(ji,:) = dz(:)
      END DO

      !  Porosity profile [0]
      !---------------------
      por(1) = 1.0
      DO jk = 2, jpksed
         por(jk) = porinf + ( porsurf-porinf) * exp(-rhox * profsed(jk) )
      END DO
 
      ! inverse of  Porosity profile
      !-----------------------------
      por1(:) = 1. - por(:)

      ! Volumes of pore water and solid fractions (vector and array)
      !     WARNING : volw(1) and vols(1) are sublayer volums
      volw(:) = dz(:) * por(:)
      vols(:) = dz(:) * por1(:)

      ! temporary new value for dz3d(:,1) 
      dz3d(1:jpoce,1) = dzkbot(1:jpoce)

      ! WARNING : volw3d(:,1) and vols3d(:,1) are deepest water column volums
      DO jk = 1, jpksed
         volw3d(1:jpoce,jk) = dz3d(1:jpoce,jk) * por (jk)
         vols3d(1:jpoce,jk) = dz3d(1:jpoce,jk) * por1(jk)
      ENDDO

      ! Back to the old sublayer vlaue for dz3d(:,1)
      dz3d(1:jpoce,1) = dz(1)

   END SUBROUTINE sed_ini_geom

   SUBROUTINE sed_ini_nam
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_ini_nam  ***
      !!
      !! ** Purpose :  Initialization of sediment geometry
      !!               - Reading namelist and defines constants variables
      !!
      !!   History :
      !!        !  06-07  (C. Ethe)  Original
      !!----------------------------------------------------------------------

      INTEGER ::   numnamsed_ref = -1           !! Logical units for namelist sediment
      INTEGER ::   numnamsed_cfg = -1           !! Logical units for namelist sediment
      INTEGER :: ios                 ! Local integer output status for namelist read
      CHARACTER(LEN=20)   ::   clname

      TYPE PSED
         CHARACTER(len = 20)  :: snamesed   !: short name
         CHARACTER(len = 80 ) :: lnamesed   !: long name
         CHARACTER(len = 20 ) :: unitsed    !: unit
      END TYPE PSED

      TYPE(PSED) , DIMENSION(jpsol     ) :: sedsol
      TYPE(PSED) , DIMENSION(jpwat     ) :: sedwat
      TYPE(PSED) , DIMENSION(jpdia3dsed) :: seddiag3d
      TYPE(PSED) , DIMENSION(jpdia2dsed) :: seddiag2d

      NAMELIST/nam_run/ln_sed_2way,nrosorder,rosatol,rosrtol
      NAMELIST/nam_geom/jpksed, sedzmin, sedhmax, sedkth, sedacr, porsurf, porinf, rhox
      NAMELIST/nam_trased/sedsol, sedwat
      NAMELIST/nam_diased/seddiag3d, seddiag2d
      NAMELIST/nam_inorg/rcopal, rccal, ratligc, rcligc
      NAMELIST/nam_poc/redO2, redNo3, redPo4, redC, redfep, rcorgl, rcorgs,  &
         &             rcorgr, rcnh4, rch2s, rcfe2, rcfeh2s, rcfeso, rcfesp, &
         &             rcfesd, xksedo2, xksedno3, xksedfeo, xksedso4
      NAMELIST/nam_btb/dbiot, ln_btbz, dbtbzsc, adsnh4, adsfe2, ln_irrig, xirrzsc
      NAMELIST/nam_rst/ln_rst_sed, cn_sedrst_indir, cn_sedrst_outdir, cn_sedrst_in, cn_sedrst_out

      INTEGER :: ji, jn, jn1
      !-------------------------------------------------------

      IF(lwp) WRITE(numsed,*) ' sed_ini_nam : Read namelists '
      IF(lwp) WRITE(numsed,*) ' '

      ! ryear = 1 year converted in second
      !------------------------------------
      IF (lwp) THEN
         WRITE(numsed,*) ' '
         WRITE(numsed,*) 'number of seconds in one year : ryear = ', ryear
         WRITE(numsed,*) ' '     
      ENDIF

      ! Reading namelist.sed variables
      !---------------------------------
      clname = 'namelist_sediment'
      IF(lwp) WRITE(numsed,*) ' sed_ini_nam : read SEDIMENT namelist'
      IF(lwp) WRITE(numsed,*) ' ~~~~~~~~~~~~~~'
      CALL ctl_opn( numnamsed_ref, TRIM( clname )//'_ref', 'OLD'    , 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )
      CALL ctl_opn( numnamsed_cfg, TRIM( clname )//'_cfg', 'OLD'    , 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )

      nitsed000 = nittrc000
      nitsedend = nitend
      ! Namelist nam_run
      REWIND( numnamsed_ref )              ! Namelist nam_run in reference namelist : Pisces variables
      READ  ( numnamsed_ref, nam_run, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'nam_run in reference namelist' )

      REWIND( numnamsed_cfg )              ! Namelist nam_run in reference namelist : Pisces variables
      READ  ( numnamsed_cfg, nam_run, IOSTAT = ios, ERR = 902)
902   IF( ios /= 0 ) CALL ctl_nam ( ios , 'nam_run in configuration namelist' )

      IF (lwp) THEN
         WRITE(numsed,*) ' namelist nam_run'
         WRITE(numsed,*) ' 2-way coupling between PISCES and Sed ln_sed_2way = ', ln_sed_2way
         WRITE(numsed,*) ' Order of the Rosenbrock method (2,3,4) = ', nrosorder
         WRITE(numsed,*) ' Tolerance for absolute error = ', rosatol
         WRITE(numsed,*) ' Tolerance for relative order = ', rosrtol
      ENDIF

      IF ( ln_p5z .AND. ln_sed_2way ) CALL ctl_stop( '2 ways coupling with sediment cannot be activated with PISCES-QUOTA' )

      REWIND( numnamsed_ref )              ! Namelist nam_geom in reference namelist : Pisces variables
      READ  ( numnamsed_ref, nam_geom, IOSTAT = ios, ERR = 903)
903   IF( ios /= 0 ) CALL ctl_nam ( ios , 'nam_geom in reference namelist' )

      REWIND( numnamsed_cfg )              ! Namelist nam_geom in reference namelist : Pisces variables
      READ  ( numnamsed_cfg, nam_geom, IOSTAT = ios, ERR = 904)
904   IF( ios /= 0 ) CALL ctl_nam ( ios , 'nam_geom in configuration namelist' )

      IF (lwp) THEN 
         WRITE(numsed,*) ' namelist nam_geom'
         WRITE(numsed,*) ' Number of vertical layers            jpksed  = ', jpksed
         WRITE(numsed,*) ' Minimum vertical spacing             sedzmin = ', sedzmin
         WRITE(numsed,*) ' Maximum depth of the sediment        sedhmax = ', sedhmax
         WRITE(numsed,*) ' Default parameter                    sedkth  = ', sedkth
         WRITE(numsed,*) ' Default parameter                    sedacr  = ', sedacr
         WRITE(numsed,*) ' Sediment porosity at the surface     porsurf = ', porsurf
         WRITE(numsed,*) ' Sediment porosity at infinite depth  porinf  = ', porinf
         WRITE(numsed,*) ' Length scale of porosity variation   rhox    = ', rhox
      ENDIF

      jpksedm1  = jpksed - 1
      dtsed = rDt_trc

      REWIND( numnamsed_ref )              ! Namelist nam_trased in reference namelist : Pisces variables
      READ  ( numnamsed_ref, nam_trased, IOSTAT = ios, ERR = 905)
905   IF( ios /= 0 ) CALL ctl_nam ( ios , 'nam_trased in reference namelist' )

      REWIND( numnamsed_cfg )              ! Namelist nam_trased in reference namelist : Pisces variables
      READ  ( numnamsed_cfg, nam_trased, IOSTAT = ios, ERR = 906)
906   IF( ios /= 0 ) CALL ctl_nam ( ios , 'nam_trased in configuration namelist' )

      DO jn = 1, jpsol
         sedtrcd(jn) = sedsol(jn)%snamesed
         sedtrcl(jn) = sedsol(jn)%lnamesed
         sedtrcu(jn) = sedsol(jn)%unitsed
      END DO

      DO jn = 1, jpwat
         jn1 = jn + jpsol
         sedtrcd(jn1) = sedwat(jn)%snamesed
         sedtrcl(jn1) = sedwat(jn)%lnamesed
         sedtrcu(jn1) = sedwat(jn)%unitsed
      END DO

      IF (lwp) THEN
         WRITE(numsed,*) ' namelist nam_trased'
         WRITE(numsed,*) ' '
         DO jn = 1, jptrased
            WRITE(numsed,*) 'name of 3d output sediment field number :',jn,' : ',TRIM(sedtrcd(jn))
            WRITE(numsed,*) 'long name ', TRIM(sedtrcl(jn))
            WRITE(numsed,*) ' in unit = ', TRIM(sedtrcu(jn))
            WRITE(numsed,*) ' '
         END DO
         WRITE(numsed,*) ' '
      ENDIF

      REWIND( numnamsed_ref )              ! Namelist nam_diased in reference namelist : Pisces variables
      READ  ( numnamsed_ref, nam_diased, IOSTAT = ios, ERR = 907)
907   IF( ios /= 0 ) CALL ctl_nam ( ios , 'nam_diased in reference namelist' )

      REWIND( numnamsed_cfg )              ! Namelist nam_diased in reference namelist : Pisces variables
      READ  ( numnamsed_cfg, nam_diased, IOSTAT = ios, ERR = 908)
908   IF( ios /= 0 ) CALL ctl_nam ( ios , 'nam_diased in configuration namelist' )
      
      DO jn = 1, jpdia3dsed
         seddia3d(jn) = seddiag3d(jn)%snamesed
         seddia3l(jn) = seddiag3d(jn)%lnamesed
         seddia3u(jn) = seddiag3d(jn)%unitsed
      END DO

      DO jn = 1, jpdia2dsed
         seddia2d(jn) = seddiag2d(jn)%snamesed
         seddia2l(jn) = seddiag2d(jn)%lnamesed
         seddia2u(jn) = seddiag2d(jn)%unitsed
      END DO

      IF (lwp) THEN
         WRITE(numsed,*) ' namelist nam_diased'
         WRITE(numsed,*) ' '
         DO jn = 1, jpdia3dsed
            WRITE(numsed,*) 'name of 3D output diag number :',jn, ' : ', TRIM(seddia3d(jn))
            WRITE(numsed,*) 'long name ', TRIM(seddia3l(jn))
            WRITE(numsed,*) ' in unit = ',TRIM(seddia3u(jn))
            WRITE(numsed,*) ' '
         END DO

         DO jn = 1, jpdia2dsed
            WRITE(numsed,*) 'name of 2D output diag number :',jn, ' : ', TRIM(seddia2d(jn))
            WRITE(numsed,*) 'long name ', TRIM(seddia2l(jn))
            WRITE(numsed,*) ' in unit = ',TRIM(seddia2u(jn))
            WRITE(numsed,*) ' '
         END DO

         WRITE(numsed,*) ' '
      ENDIF

      ! Inorganic chemistry parameters
      !----------------------------------
      REWIND( numnamsed_ref )              ! Namelist nam_inorg in reference namelist : Pisces variables
      READ  ( numnamsed_ref, nam_inorg, IOSTAT = ios, ERR = 909)
909   IF( ios /= 0 ) CALL ctl_nam ( ios , 'nam_inorg in reference namelist' )

      REWIND( numnamsed_cfg )              ! Namelist nam_inorg in reference namelist : Pisces variables
      READ  ( numnamsed_cfg, nam_inorg, IOSTAT = ios, ERR = 910)
910   IF( ios /= 0 ) CALL ctl_nam ( ios , 'nam_inorg in configuration namelist' )

      IF (lwp) THEN
         WRITE(numsed,*) ' namelist nam_inorg'
         WRITE(numsed,*) ' reactivity for Si      rcopal  = ', rcopal
         WRITE(numsed,*) ' reactivity for calcite rccal   = ', rccal
         WRITE(numsed,*) ' L/C ratio in POC       ratligc = ', ratligc
         WRITE(numsed,*) ' reactivity for ligands rcligc  = ', rcligc
         WRITE(numsed,*) ' '
      ENDIF

      ! Unity conversion to get saturation conc. psat in [mol.l-1]
      ! and reactivity rc in  [l.mol-1.s-1]
      !----------------------------------------------------------
      reac_sil   = rcopal / ryear     
      reac_ligc  = rcligc / ryear

      ! Additional parameter linked to POC/O2/No3/Po4
      !----------------------------------------------
      REWIND( numnamsed_ref )              ! Namelist nam_poc in reference namelist : Pisces variables
      READ  ( numnamsed_ref, nam_poc, IOSTAT = ios, ERR = 911)
911   IF( ios /= 0 ) CALL ctl_nam ( ios , 'nam_poc in reference namelist' )

      REWIND( numnamsed_cfg )              ! Namelist nam_poc in reference namelist : Pisces variables
      READ  ( numnamsed_cfg, nam_poc, IOSTAT = ios, ERR = 912)
912   IF( ios /= 0 ) CALL ctl_nam ( ios , 'nam_poc in configuration namelist' )

      IF (lwp) THEN
         WRITE(numsed,*) ' namelist nam_poc'
         WRITE(numsed,*) ' Redfield coef for oxy            redO2    = ', redO2
         WRITE(numsed,*) ' Redfield coef for no3            redNo3   = ', redNo3
         WRITE(numsed,*) ' Redfield coef for po4            redPo4   = ', redPo4
         WRITE(numsed,*) ' Redfield coef for carbon         redC     = ', redC
         WRITE(numsed,*) ' Ration for iron bound P          redfep   = ', redfep
         WRITE(numsed,*) ' reactivity for labile POC        rcorgl   = ', rcorgl
         WRITE(numsed,*) ' reactivity for semi-refract. POC rcorgs   = ', rcorgs
         WRITE(numsed,*) ' reactivity for refractory POC    rcorgr   = ', rcorgr
         WRITE(numsed,*) ' reactivity for NH4               rcnh4    = ', rcnh4
         WRITE(numsed,*) ' reactivity for H2S               rch2s    = ', rch2s
         WRITE(numsed,*) ' reactivity for Fe2+              rcfe2    = ', rcfe2
         WRITE(numsed,*) ' reactivity for FeOH/H2S          rcfeh2s  = ', rcfeh2s
         WRITE(numsed,*) ' reactivity for FeS/O2            rcfeso   = ', rcfeso
         WRITE(numsed,*) ' Precipitation of FeS             rcfesp   = ', rcfesp
         WRITE(numsed,*) ' Dissolution of FeS               rcfesd   = ', rcfesd
         WRITE(numsed,*) ' Half-sat. cste for oxic remin    xksedo2  = ', xksedo2
         WRITE(numsed,*) ' Half-sat. cste for denit.        xksedno3 = ', xksedno3
         WRITE(numsed,*) ' Half-sat. cste for iron remin    xksedfeo = ', xksedfeo
         WRITE(numsed,*) ' Half-sat. cste for SO4 remin     xksedso4 = ', xksedso4
         WRITE(numsed,*) ' '
      ENDIF


      so2ut  = redO2    / redC
      srno3  = redNo3   / redC
      spo4r  = redPo4   / redC
      srDnit = ( (redO2 + 32. ) * 0.8 - redNo3 - redNo3 * 0.6 ) / redC
      ! reactivity rc in  [l.mol-1.s-1]
      reac_pocl  = rcorgl / ryear
      reac_pocs  = rcorgs / ryear
      reac_pocr  = rcorgr / ryear
      reac_nh4   = rcnh4  / ryear
      reac_h2s   = rch2s  / ryear
      reac_fe2   = rcfe2  / ryear
      reac_feh2s = rcfeh2s/ ryear
      reac_feso  = rcfeso / ryear
      reac_fesp  = rcfesp / ryear
      reac_fesd  = rcfesd / ryear

      ! reactivity rc in  [l.mol-1.s-1]      
      reac_cal = rccal / ryear

      ! Bioturbation parameter
      !------------------------
      REWIND( numnamsed_ref )              ! Namelist nam_btb in reference namelist : Pisces variables
      READ  ( numnamsed_ref, nam_btb, IOSTAT = ios, ERR = 913)
913   IF( ios /= 0 ) CALL ctl_nam ( ios , 'nam_btb in reference namelist' )

      REWIND( numnamsed_cfg )              ! Namelist nam_btb in reference namelist : Pisces variables
      READ  ( numnamsed_cfg, nam_btb, IOSTAT = ios, ERR = 914)
914   IF( ios /= 0 ) CALL ctl_nam ( ios , 'nam_btb in configuration namelist' )

      IF (lwp) THEN
         WRITE(numsed,*) ' namelist nam_btb ' 
         WRITE(numsed,*) ' coefficient for bioturbation      dbiot    = ', dbiot
         WRITE(numsed,*) ' Depth varying bioturbation        ln_btbz  = ', ln_btbz
         WRITE(numsed,*) ' coefficient for btb attenuation   dbtbzsc  = ', dbtbzsc
         WRITE(numsed,*) ' Adsorption coefficient of NH4     adsnh4   = ', adsnh4
         WRITE(numsed,*) ' Adsorption coefficient of Fe2     adsfe2   = ', adsfe2
         WRITE(numsed,*) ' Bioirrigation in sediment         ln_irrig = ', ln_irrig
         WRITE(numsed,*) ' coefficient for irrig attenuation xirrzsc  = ', xirrzsc
         WRITE(numsed,*) ' '
      ENDIF

      ! Initial value (t=0) for sediment pore water and solid components
      !----------------------------------------------------------------
      REWIND( numnamsed_ref )              ! Namelist nam_rst in reference namelist : Pisces variables
      READ  ( numnamsed_ref, nam_rst, IOSTAT = ios, ERR = 915)
915   IF( ios /= 0 ) CALL ctl_nam ( ios , 'nam_rst in reference namelist' )

      REWIND( numnamsed_cfg )              ! Namelist nam_rst in reference namelist : Pisces variables
      READ  ( numnamsed_cfg, nam_rst, IOSTAT = ios, ERR = 916)
916   IF( ios /= 0 ) CALL ctl_nam ( ios , 'nam_rst in configuration namelist' )

      IF (lwp) THEN
         WRITE(numsed,*) ' namelist  nam_rst ' 
         WRITE(numsed,*) '  boolean term for restart (T or F) ln_rst_sed = ', ln_rst_sed 
         WRITE(numsed,*) ' '
      ENDIF

      CLOSE( numnamsed_cfg )
      CLOSE( numnamsed_ref )

   END SUBROUTINE sed_ini_nam

END MODULE sedini
