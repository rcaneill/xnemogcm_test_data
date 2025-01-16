MODULE p5zmeso
   !!======================================================================
   !!                         ***  MODULE p5zmeso  ***
   !! TOP :   PISCES-QUOTA Compute the sources/sinks for mesozooplankton
   !!======================================================================
   !! History :   1.0  !  2002     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-06  (O. Aumont, C. Ethe) Quota model for iron
   !!             3.6  !  2015-05  (O. Aumont) PISCES quota
   !!----------------------------------------------------------------------
   !!   p5z_meso       : Compute the sources/sinks for mesozooplankton
   !!   p5z_meso_init  : Initialization of the parameters for mesozooplankton
   !!   p5z_meso_alloc : Allocate variables for mesozooplankton 
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE prtctl          !  print control for debugging
   USE iom             !  I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p5z_meso              ! called in p5zbio.F90
   PUBLIC   p5z_meso_init         ! called in trcsms_pisces.F90
   PUBLIC   p5z_meso_alloc        ! called in trcini_pisces.F90

   !! * Shared module variables
   REAL(wp), PUBLIC ::  part2        !: part of calcite not dissolved in mesozoo guts
   REAL(wp), PUBLIC ::  xpref2c      !: mesozoo preference for POC 
   REAL(wp), PUBLIC ::  xpref2n      !: mesozoo preference for nanophyto
   REAL(wp), PUBLIC ::  xpref2z      !: mesozoo preference for zooplankton
   REAL(wp), PUBLIC ::  xpref2d      !: mesozoo preference for Diatoms 
   REAL(wp), PUBLIC ::  xpref2m      !: mesozoo preference for mesozoo
   REAL(wp), PUBLIC ::  xthresh2zoo  !: zoo feeding threshold for mesozooplankton 
   REAL(wp), PUBLIC ::  xthresh2dia  !: diatoms feeding threshold for mesozooplankton 
   REAL(wp), PUBLIC ::  xthresh2phy  !: nanophyto feeding threshold for mesozooplankton 
   REAL(wp), PUBLIC ::  xthresh2poc  !: poc feeding threshold for mesozooplankton 
   REAL(wp), PUBLIC ::  xthresh2mes  !: mesozoo feeding threshold for mesozooplankton 
   REAL(wp), PUBLIC ::  xthresh2     !: feeding threshold for mesozooplankton 
   REAL(wp), PUBLIC ::  resrat2      !: exsudation rate of mesozooplankton
   REAL(wp), PUBLIC ::  mzrat2       !: microzooplankton mortality rate 
   REAL(wp), PUBLIC ::  grazrat2     !: maximal mesozoo grazing rate
   REAL(wp), PUBLIC ::  xkgraz2      !: Half-saturation constant of assimilation
   REAL(wp), PUBLIC ::  unass2c      !: Non-assimilated fraction of food
   REAL(wp), PUBLIC ::  unass2n      !: Non-assimilated fraction of food
   REAL(wp), PUBLIC ::  unass2p      !: Non-assimilated fraction of food
   REAL(wp), PUBLIC ::  epsher2      !: Growth efficiency of mesozoo
   REAL(wp), PUBLIC ::  epsher2min   !: Minimum growth efficiency of mesozoo
   REAL(wp), PUBLIC ::  ssigma2      !: Fraction excreted as semi-labile DOM
   REAL(wp), PUBLIC ::  srespir2     !: Active respiration
   REAL(wp), PUBLIC ::  grazflux     !: mesozoo flux feeding rate
   REAL(wp), PUBLIC ::  xfracmig     !: Fractional biomass of meso that performs DVM
   REAL(wp), PUBLIC ::  xsigma2      !: Width of the predation window
   REAL(wp), PUBLIC ::  xsigma2del   !: Maximum width of the predation window at low food density
   LOGICAL,  PUBLIC ::  bmetexc2     !: Use of excess carbon for respiration
   LOGICAL , PUBLIC ::  ln_dvm_meso  !: Boolean to activate DVM of mesozooplankton
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) :: depmig  !: DVM of mesozooplankton : migration depth
   INTEGER , ALLOCATABLE, SAVE, DIMENSION(:,:) :: kmig    !: Vertical indice of the the migration depth

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p5zmeso.F90 15482 2021-11-08 20:02:22Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE p5z_meso( kt, knt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p5z_meso  ***
      !!
      !! ** Purpose :   Compute the sources/sinks for mesozooplankton
      !!                This includes ingestion and assimilation, flux feeding
      !!                and mortality. We use an active prey switching  
      !!                parameterization Morozov and Petrovskii (2013). 
      !!                All living compartments and mesozooplankton
      !!                are potential preys of mesozooplankton as well as small
      !!                sinking particles 
      !!
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, knt    ! ocean time step
      INTEGER, INTENT(in)  ::  Kbb, Kmm, Krhs  ! time level indices
      !
      INTEGER  :: ji, jj, jk, jkt
      REAL(wp) :: zcompadi, zcompaph, zcompapoc, zcompaz, zcompam, zcompames
      REAL(wp) :: zgraze2, zdenom, zfact, zfood, zfoodlim, zproport, zdep
      REAL(wp) :: zmortzgoc, zfracc, zfracn, zfracp, zfracfe, zratio, zratio2
      REAL(wp) :: zepsherf, zepshert, zepsherq, zepsherv, zrespirc, zrespirn, zrespirp, zbasresb, zbasresi
      REAL(wp) :: zgraztotc, zgraztotn, zgraztotp, zgraztotf, zbasresn, zbasresp, zbasresf
      REAL(wp) :: zgradoct, zgradont, zgrareft, zgradopt
      REAL(wp) :: zprcaca, zmortz, zexcess
      REAL(wp) :: zbeta, zrespz, ztortz, zgrasratp, zgrasratn, zgrasratf
      REAL(wp) :: ztmp1, ztmp2, ztmp3, ztmp4, ztmp5, ztmptot
      REAL(wp) :: zgrazdc, zgrazz, zgrazm, zgrazpof, zgrazcal, zfracal
      REAL(wp) :: zgraznc, zgrazpoc, zgrazpon, zgrazpop, zgraznf, zgrazdf
      REAL(wp) :: zgraznp, zgraznn, zgrazdn, zgrazdp
      REAL(wp) :: zgrazfffp, zgrazfffg, zgrazffep, zgrazffeg
      REAL(wp) :: zgrazffnp, zgrazffng, zgrazffpp, zgrazffpg
      REAL(wp) :: zmigreltime, zrum, zcodel, zargu, zval, zmigthick 
      CHARACTER (len=25) :: charout
      REAL(wp) :: zrfact2, zmetexcess, zsigma, zdiffdn
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zgrazing, zfezoo2
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zgrarem, zgraref, zgrapoc, zgrapof
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zgrarep, zgraren, zgrapon, zgrapop
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zgradoc, zgradon, zgradop
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)   ::   zgramigrem, zgramigref, zgramigpoc, zgramigpof
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)   ::   zgramigrep, zgramigren, zgramigpop, zgramigpon
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)   ::   zgramigdoc, zgramigdop, zgramigdon
      

      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p5z_meso')
      !
      ! Initialization of local arrays
      zgrazing(:,:,:) = 0._wp  ;  zfezoo2(:,:,:) = 0._wp
      zgrarem (:,:,:) = 0._wp  ;  zgraren(:,:,:) = 0._wp
      zgrarep (:,:,:) = 0._wp  ;  zgraref(:,:,:) = 0._wp
      zgrapoc (:,:,:) = 0._wp  ;  zgrapon(:,:,:) = 0._wp
      zgrapop (:,:,:) = 0._wp  ;  zgrapof(:,:,:) = 0._wp
      zgradoc (:,:,:) = 0._wp  ;  zgradon(:,:,:) = 0._wp
      zgradop (:,:,:) = 0._wp   
      !

      !
      ! Diurnal vertical migration of mesozooplankton
      ! Computation of the migration depth
      ! ---------------------------------------------
      IF( ln_dvm_meso ) CALL p5z_meso_depmig( Kbb, Kmm )

      ! Use of excess carbon for metabolism
      zmetexcess = 0.0
      IF ( bmetexc2 ) zmetexcess = 1.0

      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
         zcompam   = MAX( ( tr(ji,jj,jk,jpmes,Kbb) - 1.e-9 ), 0.e0 )
         zfact     = xstep * tgfunc2(ji,jj,jk) * zcompam

         !  linear mortality of mesozooplankton
         !  A michaelis menten modulation term is used to avoid extinction of 
         !  mesozooplankton at very low food concentrations
         !  -----------------------------------------------------------------
         zrespz   = resrat2 * zfact * ( tr(ji,jj,jk,jpmes,Kbb) / ( xkmort + tr(ji,jj,jk,jpmes,Kbb) )  &
         &          + 3. * nitrfac(ji,jj,jk) )

         !  Zooplankton quadratic mortality. A square function has been selected with
         !  to mimic predation and disease (density dependent mortality). It also tends
         !  to stabilise the model
         !  -------------------------------------------------------------------------
         ztortz   = mzrat2 * 1.e6 * zfact * tr(ji,jj,jk,jpmes,Kbb) * (1. - nitrfac(ji,jj,jk))

         !   Computation of the abundance of the preys
         !   A threshold can be specified in the namelist
         !   --------------------------------------------
         zcompadi  = MAX( ( tr(ji,jj,jk,jpdia,Kbb) - xthresh2dia ), 0.e0 )
         zcompaz   = MAX( ( tr(ji,jj,jk,jpzoo,Kbb) - xthresh2zoo ), 0.e0 )
         zcompaph  = MAX( ( tr(ji,jj,jk,jpphy,Kbb) - xthresh2phy ), 0.e0 )
         zcompapoc = MAX( ( tr(ji,jj,jk,jppoc,Kbb) - xthresh2poc ), 0.e0 )
         zcompames = MAX( ( tr(ji,jj,jk,jpmes,Kbb) - xthresh2mes ), 0.e0 )

         !  Mesozooplankton grazing
         ! The total amount of food is the sum of all preys accessible to mesozooplankton 
         ! multiplied by their food preference
         ! A threshold can be specified in the namelist (xthresh2). However, when food 
         ! concentration is close to this threshold, it is decreased to avoid the 
         ! accumulation of food in the mesozoopelagic domain
         ! -------------------------------------------------------------------------------
         zfood     = xpref2d * zcompadi + xpref2z * zcompaz + xpref2n * zcompaph + xpref2c * zcompapoc   &
         &           + xpref2m * zcompames 
         zfoodlim  = MAX( 0., zfood - MIN( 0.5 * zfood, xthresh2 ) )
         zdenom    = zfoodlim / ( xkgraz2 + zfoodlim )
         zgraze2   = grazrat2 * xstep * tgfunc2(ji,jj,jk) * tr(ji,jj,jk,jpmes,Kbb) * (1. - nitrfac(ji,jj,jk)) 

         ! An active switching parameterization is used here.
         ! We don't use the KTW parameterization proposed by 
         ! Vallina et al. because it tends to produce too steady biomass
         ! composition and the variance of Chl is too low as it grazes
         ! too strongly on winning organisms. We use a generalized
         ! switching parameterization proposed by Morozov and 
         ! Petrovskii (2013)
         ! ------------------------------------------------------------  
         ! The width of the selection window is increased when preys
         ! have low abundance, .i.e. zooplankton become less specific 
         ! to avoid starvation.
         ! ----------------------------------------------------------
         zsigma = 1.0 - zdenom**3/(0.1**3+zdenom**3)
         zsigma = xsigma2 + xsigma2del * zsigma
         ! Nanophytoplankton and diatoms are the only preys considered
         ! to be close enough to have potential interference
         ! -----------------------------------------------------------
         zdiffdn = exp( -ABS(log(3.0 * sizen(ji,jj,jk) / (5.0 * sized(ji,jj,jk) + rtrn )) )**2 / zsigma**2 )
         ztmp1 = xpref2n * zcompaph * ( zcompaph + zdiffdn * zcompadi ) / (1.0 + zdiffdn)
         ztmp2 = xpref2m * zcompames**2
         ztmp3 = xpref2c * zcompapoc**2
         ztmp4 = xpref2d * zcompadi * ( zdiffdn * zcompadi + zcompaph ) / (1.0 + zdiffdn)
         ztmp5 = xpref2z * zcompaz**2
         ztmptot = ztmp1 + ztmp2 + ztmp3 + ztmp4 + ztmp5 + rtrn
         ztmp1 = ztmp1 / ztmptot
         ztmp2 = ztmp2 / ztmptot
         ztmp3 = ztmp3 / ztmptot
         ztmp4 = ztmp4 / ztmptot
         ztmp5 = ztmp5 / ztmptot

         !   Mesozooplankton regular grazing on the different preys
         !   ------------------------------------------------------
         zgrazdc   = zgraze2 * ztmp4 * zdenom
         zgrazdn   = zgrazdc * tr(ji,jj,jk,jpndi,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn)
         zgrazdp   = zgrazdc * tr(ji,jj,jk,jppdi,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn)
         zgrazdf   = zgrazdc * tr(ji,jj,jk,jpdfe,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn)
         zgrazz    = zgraze2 * ztmp5 * zdenom
         zgrazm    = zgraze2 * ztmp2 * zdenom
         zgraznc   = zgraze2 * ztmp1 * zdenom
         zgraznn   = zgraznc * tr(ji,jj,jk,jpnph,Kbb) / ( tr(ji,jj,jk,jpphy,Kbb) + rtrn)
         zgraznp   = zgraznc * tr(ji,jj,jk,jppph,Kbb) / ( tr(ji,jj,jk,jpphy,Kbb) + rtrn)
         zgraznf   = zgraznc * tr(ji,jj,jk,jpnfe,Kbb) / ( tr(ji,jj,jk,jpphy,Kbb) + rtrn)
         zgrazpoc  = zgraze2 * ztmp3 * zdenom
         zgrazpon  = zgrazpoc * tr(ji,jj,jk,jppon,Kbb) / ( tr(ji,jj,jk,jppoc,Kbb) + rtrn)
         zgrazpop  = zgrazpoc * tr(ji,jj,jk,jppop,Kbb) / ( tr(ji,jj,jk,jppoc,Kbb) + rtrn)
         zgrazpof  = zgrazpoc * tr(ji,jj,jk,jpsfe,Kbb) / ( tr(ji,jj,jk,jppoc,Kbb) + rtrn)

         !  Mesozooplankton flux feeding on GOC and POC. The feeding pressure
         ! is proportional to the flux
         !  ------------------------------------------------------------------
         zgrazffeg = grazflux  * xstep * wsbio4(ji,jj,jk)      &
         &           * tgfunc2(ji,jj,jk) * tr(ji,jj,jk,jpgoc,Kbb) * tr(ji,jj,jk,jpmes,Kbb)  &
         &           * (1. - nitrfac(ji,jj,jk))
         zgrazfffg = zgrazffeg * tr(ji,jj,jk,jpbfe,Kbb) / (tr(ji,jj,jk,jpgoc,Kbb) + rtrn)
         zgrazffng = zgrazffeg * tr(ji,jj,jk,jpgon,Kbb) / (tr(ji,jj,jk,jpgoc,Kbb) + rtrn)
         zgrazffpg = zgrazffeg * tr(ji,jj,jk,jpgop,Kbb) / (tr(ji,jj,jk,jpgoc,Kbb) + rtrn)
         zgrazffep = grazflux  * xstep *  wsbio3(ji,jj,jk)     &
         &           * tgfunc2(ji,jj,jk) * tr(ji,jj,jk,jppoc,Kbb) * tr(ji,jj,jk,jpmes,Kbb)   &
         &           * (1. - nitrfac(ji,jj,jk))
         zgrazfffp = zgrazffep * tr(ji,jj,jk,jpsfe,Kbb) / (tr(ji,jj,jk,jppoc,Kbb) + rtrn)
         zgrazffnp = zgrazffep * tr(ji,jj,jk,jppon,Kbb) / (tr(ji,jj,jk,jppoc,Kbb) + rtrn)
         zgrazffpp = zgrazffep * tr(ji,jj,jk,jppop,Kbb) / (tr(ji,jj,jk,jppoc,Kbb) + rtrn)
         !
         zgraztotc  = zgrazdc + zgrazz + zgraznc + zgrazm + zgrazpoc + zgrazffep + zgrazffeg

         ! Compute the proportion of filter feeders. It is assumed steady state.
         ! ---------------------------------------------------------------------  
         zproport  = 0._wp
         IF( gdepw(ji,jj,jk+1,Kmm) > MAX(hmld(ji,jj), heup_01(ji,jj) ) ) THEN
            zproport  = (zgrazffep + zgrazffeg)/(rtrn + zgraztotc)
         ENDIF

         !   Compute fractionation of aggregates. It is assumed that 
         !   diatoms based aggregates are more prone to fractionation
         !   since they are more porous (marine snow instead of fecal pellets)
         !   ----------------------------------------------------------------
         zratio    = tr(ji,jj,jk,jpgsi,Kbb) / ( tr(ji,jj,jk,jpgoc,Kbb) + rtrn )
         zratio2   = zratio * zratio
         zfracc    = zproport * grazflux  * xstep * wsbio4(ji,jj,jk)      &
         &          * tr(ji,jj,jk,jpgoc,Kbb) * tr(ji,jj,jk,jpmes,Kbb)          &
         &          * ( 0.4 + 3.6 * zratio2 / ( 1.**2 + zratio2 ) )
         zfracfe   = zfracc * tr(ji,jj,jk,jpbfe,Kbb) / (tr(ji,jj,jk,jpgoc,Kbb) + rtrn)
         zfracn    = zfracc * tr(ji,jj,jk,jpgon,Kbb) / (tr(ji,jj,jk,jpgoc,Kbb) + rtrn)
         zfracp    = zfracc * tr(ji,jj,jk,jpgop,Kbb) / (tr(ji,jj,jk,jpgoc,Kbb) + rtrn)

         ! Flux feeding is multiplied by the fractional biomass of flux feeders
         zgrazffep = zproport * zgrazffep         ;   zgrazffeg = zproport * zgrazffeg
         zgrazfffp = zproport * zgrazfffp         ;   zgrazfffg = zproport * zgrazfffg
         zgrazffnp = zproport * zgrazffnp         ;   zgrazffng = zproport * zgrazffng
         zgrazffpp = zproport * zgrazffpp         ;   zgrazffpg = zproport * zgrazffpg
         zgrazdc   = (1.0 - zproport) * zgrazdc   ;   zgraznc   = (1.0 - zproport) * zgraznc
         zgrazz    = (1.0 - zproport) * zgrazz    ;   zgrazpoc  = (1.0 - zproport) * zgrazpoc
         zgrazm    = (1.0 - zproport) * zgrazm
         zgrazdn   = (1.0 - zproport) * zgrazdn   ;   zgraznn   = (1.0 - zproport) * zgraznn
         zgrazpon  = (1.0 - zproport) * zgrazpon
         zgrazdp   = (1.0 - zproport) * zgrazdp   ;   zgraznp   = (1.0 - zproport) * zgraznp
         zgrazpop  = (1.0 - zproport) * zgrazpop
         zgrazdf   = (1.0 - zproport) * zgrazdf   ;   zgraznf   = (1.0 - zproport) * zgraznf
         zgrazpof  = (1.0 - zproport) * zgrazpof

         zgraztotc  = zgrazdc + zgrazz + zgraznc + zgrazm + zgrazpoc + zgrazffep + zgrazffeg
         zgraztotf  = zgrazdf + zgraznf + zgrazz * feratz + zgrazm * feratm + zgrazpof &
         &            + zgrazfffp + zgrazfffg
         zgraztotn  = zgrazdn + (zgrazm + zgrazz) * no3rat3 + zgraznn + zgrazpon  &
         &            + zgrazffnp + zgrazffng
         zgraztotp  = zgrazdp + (zgrazz + zgrazm) * po4rat3 + zgraznp + zgrazpop  &
         &            + zgrazffpp + zgrazffpg

         ! Total grazing ( grazing by microzoo is already computed in p5zmicro )
         zgrazing(ji,jj,jk) = zgraztotc

         !   Stoichiometruc ratios of the food ingested by zooplanton 
         !   --------------------------------------------------------
         zgrasratf  =  (zgraztotf + rtrn) / ( zgraztotc + rtrn )
         zgrasratn  =  (zgraztotn + rtrn) / ( zgraztotc + rtrn )
         zgrasratp  =  (zgraztotp + rtrn) / ( zgraztotc + rtrn )

         ! Mesozooplankton efficiency. 
         ! We adopt a formulation proposed by Mitra et al. (2007)
         ! The gross growth efficiency is controled by the most limiting nutrient.
         ! Growth is also further decreased when the food quality is poor. This is currently
         ! hard coded : it can be decreased by up to 50% (zepsherq)
         ! GGE can also be decreased when food quantity is high, zepsherf (Montagnes and 
         ! Fulton, 2012)
         ! -----------------------------------------------------------------------------------
         zepshert  = MIN( 1., zgrasratn/ no3rat3, zgrasratp/ po4rat3, zgrasratf / feratm)
         zbeta     = MAX(0., (epsher2 - epsher2min) )
         zepsherf  = epsher2min + zbeta / ( 1.0 + 0.04E6 * 12. * zfood * zbeta )
         zepsherq  = 0.5 + (1.0 - 0.5) * zepshert * ( 1.0 + 1.0 ) / ( zepshert + 1.0 )
         zepsherv  = zepsherf * zepshert * zepsherq

         !   Respiration of mesozooplankton
         !   Excess carbon in the food is used preferentially
         !   when bmetexc2 is set to .true.
         !   -----------------------------------------------
         zexcess  = zgraztotc * zepsherf * (1.0 - zepshert) * zmetexcess 
         zbasresb = MAX(0., zrespz - zexcess)
         zbasresi = zexcess + MIN(0., zrespz - zexcess)
         zrespirc = srespir2 * zepsherv * zgraztotc + zbasresb

         !   When excess carbon is used, the other elements in excess
         !   are also used proportionally to their abundance
         !   --------------------------------------------------------
         zexcess  = ( zgrasratn/ no3rat3 - zepshert ) / ( 1.0 - zepshert + rtrn)
         zbasresn = zbasresi * zexcess * zgrasratn
         zexcess  = ( zgrasratp/ po4rat3 - zepshert ) / ( 1.0 - zepshert + rtrn)
         zbasresp = zbasresi * zexcess * zgrasratp
         zexcess  = ( zgrasratf/ feratm - zepshert ) / ( 1.0 - zepshert + rtrn)
         zbasresf = zbasresi * zexcess * zgrasratf

         !   Voiding of the excessive elements as organic matter
         !   --------------------------------------------------------
         zgradoct = (1. - unass2c - zepsherv) * zgraztotc - zbasresi
         zgradont = (1. - unass2n) * zgraztotn - zepsherv * no3rat3 * zgraztotc - zbasresn
         zgradopt = (1. - unass2p) * zgraztotp - zepsherv * po4rat3 * zgraztotc - zbasresp
         zgrareft = (1. - unass2c) * zgraztotf - zepsherv * feratm * zgraztotc - zbasresf
         ztmp1    = (1. - epsher2 - unass2c) /( 1. - epsher2 ) * ztortz

         zgradoc(ji,jj,jk) = (zgradoct + ztmp1) * ssigma2
         zgradon(ji,jj,jk) = (zgradont + no3rat3 * ztmp1) * ssigma2
         zgradop(ji,jj,jk) = (zgradopt + po4rat3 * ztmp1) * ssigma2


         !  Since only semilabile DOM is represented in PISCES
         !  part of DOM is in fact labile and is then released
         !  as dissolved inorganic compounds (ssigma2)
         !  --------------------------------------------------
         zgrarem(ji,jj,jk) = ( zgradoct + ztmp1 ) * (1.0 - ssigma2)
         zgraren(ji,jj,jk) = ( zgradont + no3rat3 * ztmp1 ) * (1.0 - ssigma2)
         zgrarep(ji,jj,jk) = ( zgradopt + po4rat3 * ztmp1 ) * (1.0 - ssigma2)
         zgraref(ji,jj,jk) = zgrareft + feratm * ztmp1

         !   Defecation as a result of non assimilated products
         !   --------------------------------------------------
         zgrapoc(ji,jj,jk)  = zgraztotc * unass2c + unass2c / ( 1. - epsher2 ) * ztortz
         zgrapon(ji,jj,jk)  = zgraztotn * unass2n + no3rat3 * unass2n / ( 1. - epsher2 ) * ztortz
         zgrapop(ji,jj,jk)  = zgraztotp * unass2p + po4rat3 * unass2p / ( 1. - epsher2 ) * ztortz
         zgrapof(ji,jj,jk)  = zgraztotf * unass2c + feratm  * unass2c / ( 1. - epsher2 ) * ztortz

         !  Addition of respiration to the release of inorganic nutrients
         !  -------------------------------------------------------------
         zgrarem(ji,jj,jk) = zgrarem(ji,jj,jk) + zbasresi + zrespirc
         zgraren(ji,jj,jk) = zgraren(ji,jj,jk) + zbasresn + zrespirc * no3rat3
         zgrarep(ji,jj,jk) = zgrarep(ji,jj,jk) + zbasresp + zrespirc * po4rat3
         zgraref(ji,jj,jk) = zgraref(ji,jj,jk) + zbasresf + zrespirc * feratm
         

         !   Update the arrays TRA which contain the biological sources and
         !   sinks
         !   --------------------------------------------------------------
         tr(ji,jj,jk,jpmes,Krhs) = tr(ji,jj,jk,jpmes,Krhs) + zepsherv * zgraztotc - zrespirc   &
         &                     - ztortz - zgrazm
         tr(ji,jj,jk,jpdia,Krhs) = tr(ji,jj,jk,jpdia,Krhs) - zgrazdc
         tr(ji,jj,jk,jpndi,Krhs) = tr(ji,jj,jk,jpndi,Krhs) - zgrazdn
         tr(ji,jj,jk,jppdi,Krhs) = tr(ji,jj,jk,jppdi,Krhs) - zgrazdp
         tr(ji,jj,jk,jpdfe,Krhs) = tr(ji,jj,jk,jpdfe,Krhs) - zgrazdf
         tr(ji,jj,jk,jpzoo,Krhs) = tr(ji,jj,jk,jpzoo,Krhs) - zgrazz
         tr(ji,jj,jk,jpphy,Krhs) = tr(ji,jj,jk,jpphy,Krhs) - zgraznc
         tr(ji,jj,jk,jpnph,Krhs) = tr(ji,jj,jk,jpnph,Krhs) - zgraznn
         tr(ji,jj,jk,jppph,Krhs) = tr(ji,jj,jk,jppph,Krhs) - zgraznp
         tr(ji,jj,jk,jpnfe,Krhs) = tr(ji,jj,jk,jpnfe,Krhs) - zgraznf
         tr(ji,jj,jk,jpnch,Krhs) = tr(ji,jj,jk,jpnch,Krhs) - zgraznc * tr(ji,jj,jk,jpnch,Kbb) / ( tr(ji,jj,jk,jpphy,Kbb) + rtrn )
         tr(ji,jj,jk,jpdch,Krhs) = tr(ji,jj,jk,jpdch,Krhs) - zgrazdc * tr(ji,jj,jk,jpdch,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn )
         tr(ji,jj,jk,jpdsi,Krhs) = tr(ji,jj,jk,jpdsi,Krhs) - zgrazdc * tr(ji,jj,jk,jpdsi,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn )
         tr(ji,jj,jk,jpgsi,Krhs) = tr(ji,jj,jk,jpgsi,Krhs) + zgrazdc * tr(ji,jj,jk,jpdsi,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn )

         tr(ji,jj,jk,jppoc,Krhs) = tr(ji,jj,jk,jppoc,Krhs) - zgrazpoc - zgrazffep + zfracc
         prodpoc(ji,jj,jk) = prodpoc(ji,jj,jk) + zfracc
         conspoc(ji,jj,jk) = conspoc(ji,jj,jk) - zgrazpoc - zgrazffep
         tr(ji,jj,jk,jppon,Krhs) = tr(ji,jj,jk,jppon,Krhs) - zgrazpon - zgrazffnp + zfracn
         tr(ji,jj,jk,jppop,Krhs) = tr(ji,jj,jk,jppop,Krhs) - zgrazpop - zgrazffpp + zfracp
         tr(ji,jj,jk,jpgoc,Krhs) = tr(ji,jj,jk,jpgoc,Krhs) - zgrazffeg - zfracc
         consgoc(ji,jj,jk) = consgoc(ji,jj,jk) - zgrazffeg - zfracc
         tr(ji,jj,jk,jpgon,Krhs) = tr(ji,jj,jk,jpgon,Krhs) - zgrazffng - zfracn
         tr(ji,jj,jk,jpgop,Krhs) = tr(ji,jj,jk,jpgop,Krhs) - zgrazffpg - zfracp
         tr(ji,jj,jk,jpsfe,Krhs) = tr(ji,jj,jk,jpsfe,Krhs) - zgrazpof - zgrazfffp + zfracfe
         tr(ji,jj,jk,jpbfe,Krhs) = tr(ji,jj,jk,jpbfe,Krhs) - zgrazfffg - zfracfe
         zfracal = tr(ji,jj,jk,jpcal,Kbb) / ( tr(ji,jj,jk,jpgoc,Kbb) + rtrn )
         zgrazcal = zgrazffeg * (1. - part2) * zfracal

         ! Calcite production
         ! Calcite remineralization due to zooplankton activity
         ! part2 of the ingested calcite is dissolving in the acidic gut
         ! -------------------------------------------------------------
         zprcaca = xfracal(ji,jj,jk) * zgraznc
         prodcal(ji,jj,jk) = prodcal(ji,jj,jk) + zprcaca  ! prodcal=prodcal(nanophy)+prodcal(microzoo)+prodcal(mesozoo)
         zprcaca = part2 * zprcaca
         tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) + zgrazcal - zprcaca
         tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) + 2. * ( zgrazcal - zprcaca )
         tr(ji,jj,jk,jpcal,Krhs) = tr(ji,jj,jk,jpcal,Krhs) - zgrazcal + zprcaca
      END_3D


      ! Computation of the effect of DVM by mesozooplankton
      ! This part is only activated if ln_dvm_meso is set to true
      ! The parameterization has been published in Gorgues et al. (2019).
      ! -----------------------------------------------------------------
      IF( ln_dvm_meso ) THEN
          ALLOCATE( zgramigrem(jpi,jpj), zgramigref(jpi,jpj), zgramigpoc(jpi,jpj), zgramigpof(jpi,jpj) )
          ALLOCATE( zgramigrep(jpi,jpj), zgramigren(jpi,jpj), zgramigpop(jpi,jpj), zgramigpon(jpi,jpj) )
          ALLOCATE( zgramigdoc(jpi,jpj), zgramigdon(jpi,jpj), zgramigdop(jpi,jpj) )
          zgramigrem(:,:)  = 0.0   ;   zgramigref(:,:) = 0.0
          zgramigrep(:,:)  = 0.0   ;   zgramigren(:,:) = 0.0
          zgramigpoc(:,:)  = 0.0   ;   zgramigpof(:,:) = 0.0
          zgramigpop(:,:)  = 0.0   ;   zgramigpon(:,:) = 0.0
          zgramigdoc(:,:)  = 0.0   ;   zgramigdon(:,:) = 0.0
          zgramigdop(:,:)  = 0.0   
                 
          ! Compute the amount of materials that will go into vertical migration
          ! This fraction is sumed over the euphotic zone and is removed from 
          ! the fluxes driven by mesozooplankton in the euphotic zone.
          ! --------------------------------------------------------------------
          DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
             zmigreltime = (1. - strn(ji,jj))
             IF( gdept(ji,jj,jk,Kmm) <= heup(ji,jj) ) THEN
                zmigthick  = e3t(ji,jj,jk,Kmm) * tmask(ji,jj,jk) * ( 1. - zmigreltime ) 
                zgramigrem(ji,jj) = zgramigrem(ji,jj) + xfracmig * zgrarem(ji,jj,jk) * zmigthick  
                zgramigrep(ji,jj) = zgramigrep(ji,jj) + xfracmig * zgrarep(ji,jj,jk) * zmigthick
                zgramigren(ji,jj) = zgramigren(ji,jj) + xfracmig * zgraren(ji,jj,jk) * zmigthick
                zgramigref(ji,jj) = zgramigref(ji,jj) + xfracmig * zgraref(ji,jj,jk) * zmigthick
                zgramigpoc(ji,jj) = zgramigpoc(ji,jj) + xfracmig * zgrapoc(ji,jj,jk) * zmigthick
                zgramigpop(ji,jj) = zgramigpop(ji,jj) + xfracmig * zgrapop(ji,jj,jk) * zmigthick
                zgramigpon(ji,jj) = zgramigpon(ji,jj) + xfracmig * zgrapon(ji,jj,jk) * zmigthick
                zgramigpof(ji,jj) = zgramigpof(ji,jj) + xfracmig * zgrapof(ji,jj,jk) * zmigthick
                zgramigdoc(ji,jj) = zgramigdoc(ji,jj) + xfracmig * zgradoc(ji,jj,jk) * zmigthick
                zgramigdop(ji,jj) = zgramigdop(ji,jj) + xfracmig * zgradop(ji,jj,jk) * zmigthick
                zgramigdon(ji,jj) = zgramigdon(ji,jj) + xfracmig * zgradon(ji,jj,jk) * zmigthick

                zgrarem(ji,jj,jk)  = zgrarem(ji,jj,jk) * ( (1.0 - xfracmig) + xfracmig * zmigreltime )
                zgrarep(ji,jj,jk)  = zgrarep(ji,jj,jk) * ( (1.0 - xfracmig) + xfracmig * zmigreltime )
                zgraren(ji,jj,jk)  = zgraren(ji,jj,jk) * ( (1.0 - xfracmig) + xfracmig * zmigreltime )
                zgraref(ji,jj,jk)  = zgraref(ji,jj,jk) * ( (1.0 - xfracmig) + xfracmig * zmigreltime )
                zgrapoc(ji,jj,jk)  = zgrapoc(ji,jj,jk) * ( (1.0 - xfracmig) + xfracmig * zmigreltime )
                zgrapop(ji,jj,jk)  = zgrapop(ji,jj,jk) * ( (1.0 - xfracmig) + xfracmig * zmigreltime )
                zgrapon(ji,jj,jk)  = zgrapon(ji,jj,jk) * ( (1.0 - xfracmig) + xfracmig * zmigreltime )
                zgrapof(ji,jj,jk)  = zgrapof(ji,jj,jk) * ( (1.0 - xfracmig) + xfracmig * zmigreltime )
                zgradoc(ji,jj,jk)  = zgradoc(ji,jj,jk) * ( (1.0 - xfracmig) + xfracmig * zmigreltime )
                zgradop(ji,jj,jk)  = zgradop(ji,jj,jk) * ( (1.0 - xfracmig) + xfracmig * zmigreltime )
                zgradon(ji,jj,jk)  = zgradon(ji,jj,jk) * ( (1.0 - xfracmig) + xfracmig * zmigreltime )
             ENDIF
          END_3D

          ! The inorganic and organic fluxes induced by migrating organisms are added at the 
          ! the migration depth (corresponding indice is set by kmig)
          ! --------------------------------------------------------------------------------
          DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
             IF( tmask(ji,jj,1) == 1. ) THEN
                 jkt = kmig(ji,jj)
                 zdep = 1. / e3t(ji,jj,jkt,Kmm)
                 zgrarem(ji,jj,jkt) = zgrarem(ji,jj,jkt) + zgramigrem(ji,jj) * zdep 
                 zgrarep(ji,jj,jkt) = zgrarep(ji,jj,jkt) + zgramigrep(ji,jj) * zdep 
                 zgraren(ji,jj,jkt) = zgraren(ji,jj,jkt) + zgramigren(ji,jj) * zdep 
                 zgraref(ji,jj,jkt) = zgraref(ji,jj,jkt) + zgramigref(ji,jj) * zdep 
                 zgrapoc(ji,jj,jkt) = zgrapoc(ji,jj,jkt) + zgramigpoc(ji,jj) * zdep 
                 zgrapop(ji,jj,jkt) = zgrapop(ji,jj,jkt) + zgramigpop(ji,jj) * zdep 
                 zgrapon(ji,jj,jkt) = zgrapon(ji,jj,jkt) + zgramigpon(ji,jj) * zdep 
                 zgrapof(ji,jj,jkt) = zgrapof(ji,jj,jkt) + zgramigpof(ji,jj) * zdep 
                 zgradoc(ji,jj,jkt) = zgradoc(ji,jj,jkt) + zgramigdoc(ji,jj) * zdep
                 zgradop(ji,jj,jkt) = zgradop(ji,jj,jkt) + zgramigdop(ji,jj) * zdep 
                 zgradon(ji,jj,jkt) = zgradon(ji,jj,jkt) + zgramigdon(ji,jj) * zdep 
              ENDIF
          END_2D
                   !
          ! Deallocate temporary variables
          ! ------------------------------
          DEALLOCATE( zgramigrem, zgramigref, zgramigpoc, zgramigpof )
          DEALLOCATE( zgramigrep, zgramigren, zgramigpop, zgramigpon )
          DEALLOCATE( zgramigdoc, zgramigdon, zgramigdop )
         ! End of the ln_dvm_meso part
      ENDIF

        !   Update the arrays TRA which contain the biological sources and sinks
        !   This only concerns the variables which are affected by DVM (inorganic 
        !   nutrients, DOC agands, and particulate organic carbon). 
        !   ---------------------------------------------------------------------
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
         tr(ji,jj,jk,jppo4,Krhs) = tr(ji,jj,jk,jppo4,Krhs) + zgrarep(ji,jj,jk) 
         tr(ji,jj,jk,jpnh4,Krhs) = tr(ji,jj,jk,jpnh4,Krhs) + zgraren(ji,jj,jk)
         tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) + zgradoc(ji,jj,jk)
         !
         IF( ln_ligand ) &
           &  tr(ji,jj,jk,jplgw,Krhs)  = tr(ji,jj,jk,jplgw,Krhs) + zgradoc(ji,jj,jk) * ldocz
         !
         tr(ji,jj,jk,jpdon,Krhs) = tr(ji,jj,jk,jpdon,Krhs) + zgradon(ji,jj,jk)
         tr(ji,jj,jk,jpdop,Krhs) = tr(ji,jj,jk,jpdop,Krhs) + zgradop(ji,jj,jk)
         tr(ji,jj,jk,jpoxy,Krhs) = tr(ji,jj,jk,jpoxy,Krhs) - o2ut * zgrarem(ji,jj,jk)
         tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) + zgraref(ji,jj,jk)
         zfezoo2(ji,jj,jk)   = zgraref(ji,jj,jk)
         tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) + zgrarem(ji,jj,jk)
         tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) + rno3 * zgraren(ji,jj,jk)                      
         tr(ji,jj,jk,jpgoc,Krhs) = tr(ji,jj,jk,jpgoc,Krhs) + zgrapoc(ji,jj,jk)
         prodgoc(ji,jj,jk)   = prodgoc(ji,jj,jk) + zgrapoc(ji,jj,jk)
         tr(ji,jj,jk,jpgon,Krhs) = tr(ji,jj,jk,jpgon,Krhs) + zgrapon(ji,jj,jk)
         tr(ji,jj,jk,jpgop,Krhs) = tr(ji,jj,jk,jpgop,Krhs) + zgrapop(ji,jj,jk)
         tr(ji,jj,jk,jpbfe,Krhs) = tr(ji,jj,jk,jpbfe,Krhs) + zgrapof(ji,jj,jk)
      END_3D
      !
      IF( lk_iomput .AND. knt == nrdttrc ) THEN
         CALL iom_put( "PCAL"  , prodcal(:,:,:) * 1.e+3  * rfact2r * tmask(:,:,:) )  !  Calcite production 
         CALL iom_put( "GRAZ2" , zgrazing(:,:,:) * 1.e+3  * rfact2r * tmask(:,:,:) ) ! Total grazing of phyto by zoo
         CALL iom_put( "FEZOO2", zfezoo2(:,:,:) * 1e9 * 1.e+3 * rfact2r * tmask(:,:,:) )
         IF( ln_ligand ) &
           & CALL iom_put( "LPRODZ2", zgradoc(:,:,:) * ldocz * 1e9 * 1.e+3 * rfact2r * tmask(:,:,:)  )
      ENDIF
      !
      IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
        WRITE(charout, FMT="('meso')")
        CALL prt_ctl_info( charout, cdcomp = 'top' )
        CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p5z_meso')
      !
   END SUBROUTINE p5z_meso


   SUBROUTINE p5z_meso_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p5z_meso_init  ***
      !!
      !! ** Purpose :   Initialization of mesozooplankton parameters
      !!
      !! ** Method  :   Read the namp5zmes namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist namp5zmes
      !!
      !!----------------------------------------------------------------------
      INTEGER :: ios    ! Local integer output status for namelist read
      !!
      NAMELIST/namp5zmes/part2, bmetexc2, grazrat2, resrat2, mzrat2, xpref2c, xpref2n, xpref2z, &
         &                xpref2m, xpref2d, xthresh2dia, xthresh2phy, xthresh2zoo, xthresh2poc, &
         &                xthresh2mes, xthresh2, xkgraz2, epsher2, epsher2min, ssigma2, unass2c, &
         &                unass2n, unass2p, srespir2, xsigma2, xsigma2del, grazflux, ln_dvm_meso, xfracmig
      !!----------------------------------------------------------------------
      !
      READ  ( numnatp_ref, namp5zmes, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namp5zmes in reference namelist' )
      !
      READ  ( numnatp_cfg, namp5zmes, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 ) CALL ctl_nam ( ios , 'namp5zmes in configuration namelist' )
      IF(lwm) WRITE ( numonp, namp5zmes )
      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' ' 
         WRITE(numout,*) ' Namelist parameters for mesozooplankton, namp5zmes'
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '    part of calcite not dissolved in mesozoo guts  part2       = ', part2
         WRITE(numout,*) '    mesozoo preference for nano.                   xpref2n     = ', xpref2n
         WRITE(numout,*) '    mesozoo preference for diatoms                 xpref2d     = ', xpref2d
         WRITE(numout,*) '    mesozoo preference for zoo                     xpref2z     = ', xpref2z
         WRITE(numout,*) '    mesozoo preference for mesozoo                 xpref2m     = ', xpref2m
         WRITE(numout,*) '    mesozoo preference for poc                     xpref2c     = ', xpref2c
         WRITE(numout,*) '    microzoo feeding threshold  for mesozoo        xthresh2zoo = ', xthresh2zoo
         WRITE(numout,*) '    diatoms feeding threshold  for mesozoo         xthresh2dia = ', xthresh2dia
         WRITE(numout,*) '    nanophyto feeding threshold for mesozoo        xthresh2phy = ', xthresh2phy
         WRITE(numout,*) '    poc feeding threshold for mesozoo              xthresh2poc = ', xthresh2poc
         WRITE(numout,*) '    mesozoo feeding threshold for mesozoo          xthresh2mes = ', xthresh2mes
         WRITE(numout,*) '    feeding threshold for mesozooplankton          xthresh2    = ', xthresh2
         WRITE(numout,*) '    exsudation rate of mesozooplankton             resrat2     = ', resrat2
         WRITE(numout,*) '    mesozooplankton mortality rate                 mzrat2      = ', mzrat2
         WRITE(numout,*) '    maximal mesozoo grazing rate                   grazrat2    = ', grazrat2
         WRITE(numout,*) '    mesozoo flux feeding rate                      grazflux    = ', grazflux
         WRITE(numout,*) '    C egested fraction of food by mesozoo          unass2c     = ', unass2c
         WRITE(numout,*) '    N egested fraction of food by mesozoo          unass2n     = ', unass2n
         WRITE(numout,*) '    P egested fraction of food by mesozoo          unass2p     = ', unass2p
         WRITE(numout,*) '    Efficicency of Mesozoo growth                  epsher2     = ', epsher2
         WRITE(numout,*) '    Minimum Efficiency of Mesozoo growth           epsher2min  =', epsher2min
         WRITE(numout,*) '    Fraction excreted as semi-labile DOM           ssigma2     = ', ssigma2
         WRITE(numout,*) '    Active respiration                             srespir2    = ', srespir2
         WRITE(numout,*) '    half sturation constant for grazing 2          xkgraz2     = ', xkgraz2
         WRITE(numout,*) '    Use excess carbon for respiration              bmetexc2    = ', bmetexc2
         WRITE(numout,*) '      Width of the grazing window                     xsigma2     =', xsigma2
         WRITE(numout,*) '      Maximum additional width of the grazing window  xsigma2del  =', xsigma2del
         WRITE(numout,*) '      Diurnal vertical migration of mesozoo.         ln_dvm_meso  =', ln_dvm_meso
         WRITE(numout,*) '      Fractional biomass of meso  that performs DVM  xfracmig     =', xfracmig
      ENDIF
      !
   END SUBROUTINE p5z_meso_init

   SUBROUTINE p5z_meso_depmig( Kbb, Kmm )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_meso_depmig  ***
      !!
      !! ** Purpose :   Computation the migration depth of mesozooplankton
      !!
      !! ** Method  :   Computes the DVM depth of mesozooplankton from oxygen
      !!      temperature and chlorophylle following the parameterization
      !!      proposed by Bianchi et al. (2013)
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in)  ::  Kbb, kmm ! time level indices
      !
      INTEGER  :: ji, jj, jk
      !
      REAL(wp) :: ztotchl, z1dep
      REAL(wp), DIMENSION(jpi,jpj) :: oxymoy, tempmoy, zdepmoy

      !!---------------------------------------------------------------------
      !
      IF( ln_timing )  CALL timing_start('p5z_meso_depmig')
      !
      oxymoy(:,:)  = 0.
      tempmoy(:,:) = 0.
      zdepmoy(:,:) = 0.
      depmig (:,:) = 5.
      kmig   (:,:) = 1
      !

      ! Compute the averaged values of oxygen, temperature over the domain 
      ! 150m to 500 m depth.
      ! ------------------------------------------------------------------
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpk )
         IF( tmask(ji,jj,jk) == 1.) THEN
            IF( gdept(ji,jj,jk,Kmm) >= 150. .AND. gdept(ji,jj,jk,kmm) <= 500.) THEN
               oxymoy(ji,jj)  = oxymoy(ji,jj)  + tr(ji,jj,jk,jpoxy,Kbb) * 1E6 * e3t(ji,jj,jk,Kmm)
               tempmoy(ji,jj) = tempmoy(ji,jj) + ts(ji,jj,jk,jp_tem,kmm)      * e3t(ji,jj,jk,kmm)
               zdepmoy(ji,jj) = zdepmoy(ji,jj) + e3t(ji,jj,jk,Kmm)
            ENDIF
         ENDIF
      END_3D

      ! Compute the difference between surface values and the mean values in the mesopelagic
      ! domain
      ! ------------------------------------------------------------------------------------
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         z1dep = 1. / ( zdepmoy(ji,jj) + rtrn )
         oxymoy(ji,jj)  = tr(ji,jj,1,jpoxy,Kbb) * 1E6 - oxymoy(ji,jj)  * z1dep
         tempmoy(ji,jj) = ts(ji,jj,1,jp_tem,Kmm)      - tempmoy(ji,jj) * z1dep
      END_2D
      !
      ! Computation of the migration depth based on the parameterization of 
      ! Bianchi et al. (2013)
      ! -------------------------------------------------------------------
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         IF( tmask(ji,jj,1) == 1. ) THEN
            ztotchl = ( tr(ji,jj,1,jppch,Kbb) + tr(ji,jj,1,jpnch,Kbb) + tr(ji,jj,1,jpdch,Kbb) ) * 1E6
            depmig(ji,jj) = 398. - 0.56 * oxymoy(ji,jj) -115. * log10(ztotchl) + 0.36 * hmld(ji,jj) -2.4 * tempmoy(ji,jj)
         ENDIF
      END_2D

            ! Computation of the corresponding jk indice 
      ! ------------------------------------------
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 )
         IF( depmig(ji,jj) >= gdepw(ji,jj,jk,Kmm) .AND. depmig(ji,jj) < gdepw(ji,jj,jk+1,Kmm) ) THEN
             kmig(ji,jj) = jk
          ENDIF
      END_3D
      !
      ! Correction of the migration depth and indice based on O2 levels
      ! If O2 is too low, imposing a migration depth at this low O2 levels
      ! would lead to negative O2 concentrations (respiration while O2 is close
      ! to 0. Thus, to avoid that problem, the migration depth is adjusted so
      ! that it falls above the OMZ
      ! -----------------------------------------------------------------------
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         IF( tr(ji,jj,kmig(ji,jj),jpoxy,Kbb) < 5E-6 ) THEN
            DO jk = kmig(ji,jj),1,-1
               IF( tr(ji,jj,jk,jpoxy,Kbb) >= 5E-6 .AND. tr(ji,jj,jk+1,jpoxy,Kbb)  < 5E-6) THEN
                  kmig(ji,jj) = jk
                  depmig(ji,jj) = gdept(ji,jj,jk,Kmm)
               ENDIF
            END DO
         ENDIF
      END_2D
      !
      IF( ln_timing )   CALL timing_stop('p5z_meso_depmig')
      !
   END SUBROUTINE p5z_meso_depmig

   INTEGER FUNCTION p5z_meso_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p5z_meso_alloc  ***
      !!----------------------------------------------------------------------
      !
      ALLOCATE( depmig(jpi,jpj), kmig(jpi,jpj), STAT= p5z_meso_alloc  )
      !
      IF( p5z_meso_alloc /= 0 ) CALL ctl_stop( 'STOP', 'p5z_meso_alloc : failed to allocate arrays.' )
      !
   END FUNCTION p5z_meso_alloc

   !!======================================================================
END MODULE p5zmeso
