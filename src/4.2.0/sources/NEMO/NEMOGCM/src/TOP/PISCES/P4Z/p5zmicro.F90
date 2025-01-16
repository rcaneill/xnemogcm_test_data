MODULE p5zmicro
   !!======================================================================
   !!                         ***  MODULE p5zmicro  ***
   !! TOP :   PISCES Compute the sources/sinks for microzooplankton
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-06  (O. Aumont, C. Ethe) Quota model for iron
   !!             3.6  !  2015-05  (O. Aumont) PISCES quota
   !!----------------------------------------------------------------------
   !!   p5z_micro       :   Compute the sources/sinks for microzooplankton
   !!   p5z_micro_init  :   Initialize and read the appropriate namelist
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE p4zlim          !  PISCES nutrient limitation term of PISCES std
   USE p5zlim          !  Phytoplankton limitation terms
   USE iom             !  I/O manager
   USE prtctl          !  print control for debugging

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p5z_micro         ! called in p5zbio.F90
   PUBLIC   p5z_micro_init    ! called in trcsms_pisces.F90

   !! * Shared module variables
   REAL(wp), PUBLIC ::  part        !: part of calcite not dissolved in microzoo guts
   REAL(wp), PUBLIC ::  xprefc     !: microzoo preference for POC 
   REAL(wp), PUBLIC ::  xprefn     !: microzoo preference for nanophyto
   REAL(wp), PUBLIC ::  xprefp     !: microzoo preference for picophyto
   REAL(wp), PUBLIC ::  xprefd     !: microzoo preference for diatoms
   REAL(wp), PUBLIC ::  xprefz     !: microzoo preference for microzoo
   REAL(wp), PUBLIC ::  xthreshdia  !: diatoms feeding threshold for microzooplankton 
   REAL(wp), PUBLIC ::  xthreshpic  !: picophyto feeding threshold for microzooplankton 
   REAL(wp), PUBLIC ::  xthreshphy  !: nanophyto threshold for microzooplankton 
   REAL(wp), PUBLIC ::  xthreshzoo  !: microzoo threshold for microzooplankton 
   REAL(wp), PUBLIC ::  xthreshpoc  !: poc threshold for microzooplankton 
   REAL(wp), PUBLIC ::  xthresh     !: feeding threshold for microzooplankton 
   REAL(wp), PUBLIC ::  resrat      !: exsudation rate of microzooplankton
   REAL(wp), PUBLIC ::  mzrat       !: microzooplankton mortality rate 
   REAL(wp), PUBLIC ::  grazrat     !: maximal microzoo grazing rate
   REAL(wp), PUBLIC ::  xkgraz      !: Half-saturation constant of assimilation
   REAL(wp), PUBLIC ::  unassc      !: Non-assimilated part of food
   REAL(wp), PUBLIC ::  unassn      !: Non-assimilated part of food
   REAL(wp), PUBLIC ::  unassp      !: Non-assimilated part of food
   REAL(wp), PUBLIC ::  epsher      !: Growth efficiency for microzoo
   REAL(wp), PUBLIC ::  epshermin   !: Minimum growth efficiency for microzoo
   REAL(wp), PUBLIC ::  srespir     !: half sturation constant for grazing 1 
   REAL(wp), PUBLIC ::  ssigma      !: Fraction excreted as semi-labile DOM
   REAL(wp), PUBLIC ::  xsigma      !: Width of the grazing window
   REAL(wp), PUBLIC ::  xsigmadel   !: Maximum additional width of the grazing window at low food density
   LOGICAL,  PUBLIC ::  bmetexc     !: Use of excess carbon for respiration

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p5zmicro.F90 15459 2021-10-29 08:19:18Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE p5z_micro( kt, knt, Kbb, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p5z_micro  ***
      !!
      !! ** Purpose :   Compute the sources/sinks for microzooplankton
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::  kt  ! ocean time step
      INTEGER, INTENT(in) ::  knt 
      INTEGER, INTENT(in) ::  Kbb, Krhs      ! time level indices
      !
      INTEGER  :: ji, jj, jk
      REAL(wp) :: zcompadi, zcompaz , zcompaph, zcompapoc, zcompapon, zcompapop
      REAL(wp) :: zcompapi, zgraze  , zdenom, zfact, zfood, zfoodlim
      REAL(wp) :: ztmp1, ztmp2, ztmp3, ztmp4, ztmp5, ztmptot
      REAL(wp) :: zepsherf, zepshert, zepsherq, zepsherv, zrespirc, zrespirn, zrespirp, zbasresb, zbasresi
      REAL(wp) :: zgraztotc, zgraztotn, zgraztotp, zgraztotf, zbasresn, zbasresp, zbasresf
      REAL(wp) :: zgradoc, zgradon, zgradop, zgraref, zgradoct, zgradont, zgradopt, zgrareft
      REAL(wp) :: zexcess, zgraren, zgrarep, zgrarem
      REAL(wp) :: zgrapoc, zgrapon, zgrapop, zgrapof, zprcaca, zmortz
      REAL(wp) :: zrespz, ztortz, zgrasratf, zgrasratn, zgrasratp
      REAL(wp) :: zgraznc, zgraznn, zgraznp, zgrazpoc, zgrazpon, zgrazpop, zgrazpof
      REAL(wp) :: zgrazdc, zgrazdn, zgrazdp, zgrazdf, zgraznf, zgrazz
      REAL(wp) :: zgrazpc, zgrazpn, zgrazpp, zgrazpf, zbeta, zrfact2, zmetexcess
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zgrazing, zfezoo, zzligprod
      REAL(wp) :: zsigma, zdiffdn, zdiffpn, zdiffdp, zproport, zproport2
      CHARACTER (len=25) :: charout
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p5z_micro')
      !
      ! Use of excess carbon for metabolism
      zmetexcess = 0.0
      IF ( bmetexc ) zmetexcess = 1.0
      !
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
         zcompaz = MAX( ( tr(ji,jj,jk,jpzoo,Kbb) - 1.e-9 ), 0.e0 )
         zfact   = xstep * tgfunc2(ji,jj,jk) * zcompaz
         ! Proportion of nano and diatoms that are within the size range
         ! accessible to microzooplankton. 
         zproport  = min(sized(ji,jj,jk),1.8)**(-0.48)*min(1.0, exp(-1.1 * MAX(0., ( sized(ji,jj,jk) - 1.8 ))**0.8 ))
         zproport2 = sizen(ji,jj,jk)**(-0.48)
         zproport2 = 1.0
         !  linear mortality of mesozooplankton
         !  A michaelis menten modulation term is used to avoid extinction of 
         !  microzooplankton at very low food concentrations. Mortality is 
         !  enhanced in low O2 waters
         !  -----------------------------------------------------------------

         !   Michaelis-Menten mortality rates of microzooplankton
         !   -----------------------------------------------------
         zrespz = resrat * zfact * ( tr(ji,jj,jk,jpzoo,Kbb) / ( xkmort + tr(ji,jj,jk,jpzoo,Kbb) )  &
         &        + 3. * nitrfac(ji,jj,jk) )

         !  Zooplankton quadratic mortality. A square function has been selected with
         !  to mimic predation and disease (density dependent mortality). It also tends
         !  to stabilise the model
         !  -------------------------------------------------------------------------
         ztortz = mzrat * 1.e6 * zfact * tr(ji,jj,jk,jpzoo,Kbb) * (1. - nitrfac(ji,jj,jk))

         !   Computation of the abundance of the preys
         !   A threshold can be specified in the namelist
         !   Nanophyto and diatoms have a specific treatment with 
         !   teir preference decreasing with size.
         !   --------------------------------------------------------
         zcompadi  = zproport  * MAX( ( tr(ji,jj,jk,jpdia,Kbb) - xthreshdia ), 0.e0 )
         zcompaph  = zproport2 * MAX( ( tr(ji,jj,jk,jpphy,Kbb) - xthreshphy ), 0.e0 )
         zcompaz   = MAX( ( tr(ji,jj,jk,jpzoo,Kbb) - xthreshzoo ), 0.e0 )
         zcompapi  = MAX( ( tr(ji,jj,jk,jppic,Kbb) - xthreshpic ), 0.e0 )
         zcompapoc = MAX( ( tr(ji,jj,jk,jppoc,Kbb) - xthreshpoc ), 0.e0 )
               
         ! Microzooplankton grazing
         ! The total amount of food is the sum of all preys accessible to mesozooplankton 
         ! multiplied by their food preference
         ! A threshold can be specified in the namelist (xthresh). However, when food 
         ! concentration is close to this threshold, it is decreased to avoid the 
         ! accumulation of food in the mesozoopelagic domain
         ! -------------------------------------------------------------------------------
         zfood     = xprefn * zcompaph + xprefc * zcompapoc + xprefd * zcompadi   &
         &           + xprefz * zcompaz + xprefp * zcompapi
         zfoodlim  = MAX( 0. , zfood - min(xthresh,0.5*zfood) )
         zdenom    = zfoodlim / ( xkgraz + zfoodlim )
         zgraze    = grazrat * xstep * tgfunc2(ji,jj,jk) * tr(ji,jj,jk,jpzoo,Kbb) * (1. - nitrfac(ji,jj,jk)) 

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
         zsigma = xsigma + xsigmadel * zsigma
         zdiffpn = exp( -ABS(log(0.7 * sizep(ji,jj,jk) / (3.0 * sizen(ji,jj,jk) + rtrn )) )**2 / zsigma**2 )
         zdiffdn = exp( -ABS(log(3.0 * sizen(ji,jj,jk) / (5.0 * sized(ji,jj,jk) + rtrn )) )**2 / zsigma**2)
         zdiffdp = exp( -ABS(log(0.7 * sizep(ji,jj,jk) / (5.0 * sized(ji,jj,jk) + rtrn )) )**2 / zsigma**2)
         ztmp1 = xprefn * zcompaph * ( zcompaph + zdiffdn * zcompadi + zdiffpn * zcompapi ) / ( 1.0 + zdiffdn + zdiffpn )
         ztmp2 = xprefp * zcompapi * ( zcompapi + zdiffpn * zcompaph + zdiffdp * zcompadi ) / ( 1.0 + zdiffpn + zdiffdp )
         ztmp3 = xprefc * zcompapoc**2
         ztmp4 = xprefd * zcompadi * ( zdiffdp * zcompapi + zdiffdn * zcompaph + zcompadi ) / ( 1.0 + zdiffdn + zdiffdp )
         ztmp5 = xprefz * zcompaz**2
         ztmptot = ztmp1 + ztmp2 + ztmp3 + ztmp4 + ztmp5 + rtrn
         ztmp1 = ztmp1 / ztmptot
         ztmp2 = ztmp2 / ztmptot
         ztmp3 = ztmp3 / ztmptot
         ztmp4 = ztmp4 / ztmptot
         ztmp5 = ztmp5 / ztmptot

         !   Microzooplankton regular grazing on the different preys
         !   -------------------------------------------------------
               !   Nanophytoplankton
         zgraznc   = zgraze  * ztmp1  * zdenom
         zgraznn   = zgraznc * tr(ji,jj,jk,jpnph,Kbb) / (tr(ji,jj,jk,jpphy,Kbb) + rtrn)
         zgraznp   = zgraznc * tr(ji,jj,jk,jppph,Kbb) / (tr(ji,jj,jk,jpphy,Kbb) + rtrn)
         zgraznf   = zgraznc * tr(ji,jj,jk,jpnfe,Kbb) / (tr(ji,jj,jk,jpphy,Kbb) + rtrn)

               ! Picophytoplankton
         zgrazpc   = zgraze  * ztmp2  * zdenom
         zgrazpn   = zgrazpc * tr(ji,jj,jk,jpnpi,Kbb) / (tr(ji,jj,jk,jppic,Kbb) + rtrn)
         zgrazpp   = zgrazpc * tr(ji,jj,jk,jpppi,Kbb) / (tr(ji,jj,jk,jppic,Kbb) + rtrn)
         zgrazpf   = zgrazpc * tr(ji,jj,jk,jppfe,Kbb) / (tr(ji,jj,jk,jppic,Kbb) + rtrn)

               ! Microzooplankton
         zgrazz    = zgraze  * ztmp5   * zdenom

               ! small POC
         zgrazpoc  = zgraze  * ztmp3   * zdenom
         zgrazpon  = zgrazpoc * tr(ji,jj,jk,jppon,Kbb) / ( tr(ji,jj,jk,jppoc,Kbb) + rtrn )
         zgrazpop  = zgrazpoc * tr(ji,jj,jk,jppop,Kbb) / ( tr(ji,jj,jk,jppoc,Kbb) + rtrn )
         zgrazpof  = zgrazpoc* tr(ji,jj,jk,jpsfe,Kbb) / (tr(ji,jj,jk,jppoc,Kbb) + rtrn)

               ! Diatoms
         zgrazdc   = zgraze  * ztmp4  * zdenom
         zgrazdn   = zgrazdc * tr(ji,jj,jk,jpndi,Kbb) / (tr(ji,jj,jk,jpdia,Kbb) + rtrn)
         zgrazdp   = zgrazdc * tr(ji,jj,jk,jppdi,Kbb) / (tr(ji,jj,jk,jpdia,Kbb) + rtrn)
         zgrazdf   = zgrazdc * tr(ji,jj,jk,jpdfe,Kbb) / (tr(ji,jj,jk,jpdia,Kbb) + rtrn)
         !
               ! Total ingestion rates in C, P, Fe, N
         zgraztotc = zgraznc + zgrazpoc + zgrazdc + zgrazz + zgrazpc
         zgraztotn = zgraznn + zgrazpn + zgrazpon + zgrazdn + zgrazz * no3rat3
         zgraztotp = zgraznp + zgrazpp + zgrazpop + zgrazdp + zgrazz * po4rat3
         zgraztotf = zgraznf + zgrazpf + zgrazpof + zgrazdf + zgrazz * feratz
         !
         ! Grazing by microzooplankton
         zgrazing(ji,jj,jk) = zgraztotc

         !   Stoichiometruc ratios of the food ingested by zooplanton 
         !   --------------------------------------------------------
         zgrasratf =  (zgraztotf + rtrn) / ( zgraztotc + rtrn )
         zgrasratn =  (zgraztotn + rtrn) / ( zgraztotc + rtrn )
         zgrasratp =  (zgraztotp + rtrn) / ( zgraztotc + rtrn )

         ! Mesozooplankton efficiency. 
         ! We adopt a formulation proposed by Mitra et al. (2007)
         ! The gross growth efficiency is controled by the most limiting nutrient.
         ! Growth is also further decreased when the food quality is poor. This is currently
         ! hard coded : it can be decreased by up to 50% (zepsherq)
         ! GGE can also be decreased when food quantity is high, zepsherf (Montagnes and 
         ! Fulton, 2012)
         ! -----------------------------------------------------------------------------------
         zepshert  = MIN( 1., zgrasratn/ no3rat3, zgrasratp/ po4rat3, zgrasratf / feratz)
         zbeta     = MAX( 0., (epsher - epshermin) )
         ! Food density deprivation of GGE
         zepsherf  = epshermin + zbeta / ( 1.0 + 0.04E6 * 12. * zfood * zbeta )
         ! Food quality deprivation of GGE
         zepsherq  = 0.5 + (1.0 - 0.5) * zepshert * ( 1.0 + 1.0 ) / ( zepshert + 1.0 )
         ! Actual GGE
         zepsherv  = zepsherf * zepshert * zepsherq

         ! Respiration of microzooplankton
         ! Excess carbon in the food is used preferentially
         ! when activated by zmetexcess
         ! ------------------------------------------------
         zexcess  = zgraztotc * zepsherf * (1.0 - zepshert) * zmetexcess
         zbasresb = MAX(0., zrespz - zexcess)
         zbasresi = zexcess + MIN(0., zrespz - zexcess)  
         zrespirc = srespir * zepsherv * zgraztotc + zbasresb
         
         ! When excess carbon is used, the other elements in excess
         ! are also used proportionally to their abundance
         ! --------------------------------------------------------
         zexcess  = ( zgrasratn/ no3rat3 - zepshert ) / ( 1.0 - zepshert + rtrn)
         zbasresn = zbasresi * zexcess * zgrasratn 
         zexcess  = ( zgrasratp/ po4rat3 - zepshert ) / ( 1.0 - zepshert + rtrn)
         zbasresp = zbasresi * zexcess * zgrasratp
         zexcess  = ( zgrasratf/ feratz - zepshert ) / ( 1.0 - zepshert + rtrn)
         zbasresf = zbasresi * zexcess * zgrasratf

         ! Voiding of the excessive elements as DOM
         ! ----------------------------------------
         zgradoct   = (1. - unassc - zepsherv) * zgraztotc - zbasresi  
         zgradont   = (1. - unassn) * zgraztotn - zepsherv * no3rat3 * zgraztotc - zbasresn
         zgradopt   = (1. - unassp) * zgraztotp - zepsherv * po4rat3 * zgraztotc - zbasresp
         zgrareft   = (1. - unassc) * zgraztotf - zepsherv * feratz * zgraztotc - zbasresf

         ! Since only semilabile DOM is represented in PISCES
         ! part of DOM is in fact labile and is then released
         ! as dissolved inorganic compounds (ssigma)
         ! --------------------------------------------------
         zgradoc =  zgradoct * ssigma
         zgradon =  zgradont * ssigma
         zgradop =  zgradopt * ssigma
         zgrarem = (1.0 - ssigma) * zgradoct
         zgraren = (1.0 - ssigma) * zgradont
         zgrarep = (1.0 - ssigma) * zgradopt
         zgraref = zgrareft

         ! Defecation as a result of non assimilated products
         ! --------------------------------------------------
         zgrapoc   = zgraztotc * unassc
         zgrapon   = zgraztotn * unassn
         zgrapop   = zgraztotp * unassp
         zgrapof   = zgraztotf * unassc

         ! Addition of respiration to the release of inorganic nutrients
         ! -------------------------------------------------------------
         zgrarem = zgrarem + zbasresi + zrespirc
         zgraren = zgraren + zbasresn + zrespirc * no3rat3
         zgrarep = zgrarep + zbasresp + zrespirc * po4rat3
         zgraref = zgraref + zbasresf + zrespirc * feratz

         !   Update of the TRA arrays
         !   ------------------------
         tr(ji,jj,jk,jppo4,Krhs) = tr(ji,jj,jk,jppo4,Krhs) + zgrarep
         tr(ji,jj,jk,jpnh4,Krhs) = tr(ji,jj,jk,jpnh4,Krhs) + zgraren
         tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) + zgradoc
         !
         IF( ln_ligand ) THEN 
            tr(ji,jj,jk,jplgw,Krhs) = tr(ji,jj,jk,jplgw,Krhs) + zgradoc * ldocz
            zzligprod(ji,jj,jk) = zgradoc * ldocz
         ENDIF
         !
         tr(ji,jj,jk,jpdon,Krhs) = tr(ji,jj,jk,jpdon,Krhs) + zgradon
         tr(ji,jj,jk,jpdop,Krhs) = tr(ji,jj,jk,jpdop,Krhs) + zgradop
         tr(ji,jj,jk,jpoxy,Krhs) = tr(ji,jj,jk,jpoxy,Krhs) - o2ut * zgrarem 
         tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) + zgraref
         zfezoo(ji,jj,jk)    = zgraref
         tr(ji,jj,jk,jpzoo,Krhs) = tr(ji,jj,jk,jpzoo,Krhs) + zepsherv * zgraztotc - zrespirc - ztortz - zgrazz
         tr(ji,jj,jk,jpphy,Krhs) = tr(ji,jj,jk,jpphy,Krhs) - zgraznc
         tr(ji,jj,jk,jpnph,Krhs) = tr(ji,jj,jk,jpnph,Krhs) - zgraznn
         tr(ji,jj,jk,jppph,Krhs) = tr(ji,jj,jk,jppph,Krhs) - zgraznp
         tr(ji,jj,jk,jppic,Krhs) = tr(ji,jj,jk,jppic,Krhs) - zgrazpc
         tr(ji,jj,jk,jpnpi,Krhs) = tr(ji,jj,jk,jpnpi,Krhs) - zgrazpn
         tr(ji,jj,jk,jpppi,Krhs) = tr(ji,jj,jk,jpppi,Krhs) - zgrazpp
         tr(ji,jj,jk,jpdia,Krhs) = tr(ji,jj,jk,jpdia,Krhs) - zgrazdc
         tr(ji,jj,jk,jpndi,Krhs) = tr(ji,jj,jk,jpndi,Krhs) - zgrazdn
         tr(ji,jj,jk,jppdi,Krhs) = tr(ji,jj,jk,jppdi,Krhs) - zgrazdp
         tr(ji,jj,jk,jpnch,Krhs) = tr(ji,jj,jk,jpnch,Krhs) - zgraznc * tr(ji,jj,jk,jpnch,Kbb)/(tr(ji,jj,jk,jpphy,Kbb)+rtrn)
         tr(ji,jj,jk,jppch,Krhs) = tr(ji,jj,jk,jppch,Krhs) - zgrazpc * tr(ji,jj,jk,jppch,Kbb)/(tr(ji,jj,jk,jppic,Kbb)+rtrn)
         tr(ji,jj,jk,jpdch,Krhs) = tr(ji,jj,jk,jpdch,Krhs) - zgrazdc * tr(ji,jj,jk,jpdch,Kbb)/(tr(ji,jj,jk,jpdia,Kbb)+rtrn)
         tr(ji,jj,jk,jpdsi,Krhs) = tr(ji,jj,jk,jpdsi,Krhs) - zgrazdc * tr(ji,jj,jk,jpdsi,Kbb)/(tr(ji,jj,jk,jpdia,Kbb)+rtrn)
         tr(ji,jj,jk,jpgsi,Krhs) = tr(ji,jj,jk,jpgsi,Krhs) + zgrazdc * tr(ji,jj,jk,jpdsi,Kbb)/(tr(ji,jj,jk,jpdia,Kbb)+rtrn)
         tr(ji,jj,jk,jpnfe,Krhs) = tr(ji,jj,jk,jpnfe,Krhs) - zgraznf
         tr(ji,jj,jk,jppfe,Krhs) = tr(ji,jj,jk,jppfe,Krhs) - zgrazpf
         tr(ji,jj,jk,jpdfe,Krhs) = tr(ji,jj,jk,jpdfe,Krhs) - zgrazdf
         tr(ji,jj,jk,jppoc,Krhs) = tr(ji,jj,jk,jppoc,Krhs) + ztortz + zgrapoc - zgrazpoc 
         prodpoc(ji,jj,jk) = prodpoc(ji,jj,jk) + ztortz + zgrapoc
         conspoc(ji,jj,jk) = conspoc(ji,jj,jk) - zgrazpoc
         tr(ji,jj,jk,jppon,Krhs) = tr(ji,jj,jk,jppon,Krhs) + no3rat3 * ztortz + zgrapon - zgrazpon
         tr(ji,jj,jk,jppop,Krhs) = tr(ji,jj,jk,jppop,Krhs) + po4rat3 * ztortz + zgrapop - zgrazpop
         tr(ji,jj,jk,jpsfe,Krhs) = tr(ji,jj,jk,jpsfe,Krhs) + feratz * ztortz  + zgrapof - zgrazpof
         !
         ! calcite production
         zprcaca = xfracal(ji,jj,jk) * zgraznc
         prodcal(ji,jj,jk) = prodcal(ji,jj,jk) + zprcaca  ! prodcal=prodcal(nanophy)+prodcal(microzoo)+prodcal(mesozoo)
         !
         zprcaca = part * zprcaca
         tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) + zgrarem - zprcaca
         tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) - 2. * zprcaca + rno3 * zgraren
         tr(ji,jj,jk,jpcal,Krhs) = tr(ji,jj,jk,jpcal,Krhs) + zprcaca
      END_3D
      !
      IF( lk_iomput .AND. knt == nrdttrc ) THEN
       IF( iom_use("GRAZ1") ) THEN  !   Total grazing of phyto by zooplankton
           zgrazing(:,:,jpk) = 0._wp   ; CALL iom_put( "GRAZ1" , zgrazing(:,:,:) * 1.e+3  * rfact2r * tmask(:,:,:) ) 
         ENDIF
         IF( iom_use("FEZOO") ) THEN  
           zfezoo (:,:,jpk) = 0._wp    ; CALL iom_put( "FEZOO" , zfezoo(:,:,:) * 1e9 * 1.e+3 * rfact2r * tmask(:,:,:) )
         ENDIF
         IF( ln_ligand ) THEN
            zzligprod(:,:,jpk) = 0._wp ; CALL iom_put( "LPRODZ", zzligprod(:,:,:) * 1e9 * 1.e+3 * rfact2r * tmask(:,:,:))
         ENDIF
      ENDIF
      !
      IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('micro')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p5z_micro')
      !
   END SUBROUTINE p5z_micro


   SUBROUTINE p5z_micro_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p5z_micro_init  ***
      !!
      !! ** Purpose :   Initialization of microzooplankton parameters
      !!
      !! ** Method  :   Read the namp5zzoo namelist and check the parameters
      !!                called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist namp5zzoo
      !!
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer
      !!
      NAMELIST/namp5zzoo/ part, grazrat, bmetexc, resrat, mzrat, xprefc, xprefn, &
         &                xprefp, xprefd, xprefz, xthreshdia, xthreshphy, &
         &                xthreshpic, xthreshpoc, xthreshzoo, xthresh, xkgraz, &
         &                epsher, epshermin, ssigma, srespir, unassc, unassn, unassp,   &
         &                xsigma, xsigmadel   
      !!----------------------------------------------------------------------
      !
      READ  ( numnatp_ref, namp5zzoo, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namp5zzoo in reference namelist' )
      !
      READ  ( numnatp_cfg, namp5zzoo, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 ) CALL ctl_nam ( ios , 'namp5zzoo in configuration namelist' )
      IF(lwm) WRITE ( numonp, namp5zzoo )
      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' '
         WRITE(numout,*) ' Namelist parameters for microzooplankton, nampiszooq'
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '    part of calcite not dissolved in microzoo guts  part        =', part
         WRITE(numout,*) '    microzoo preference for POC                     xprefc     =', xprefc
         WRITE(numout,*) '    microzoo preference for nano                    xprefn     =', xprefn
         WRITE(numout,*) '    microzoo preference for pico                    xprefp     =', xprefp
         WRITE(numout,*) '    microzoo preference for diatoms                 xprefd     =', xprefd
         WRITE(numout,*) '    microzoo preference for microzoo                xprefz     =', xprefz
         WRITE(numout,*) '    diatoms feeding threshold  for microzoo         xthreshdia  =', xthreshdia
         WRITE(numout,*) '    nanophyto feeding threshold for microzoo        xthreshphy  =', xthreshphy
         WRITE(numout,*) '    picophyto feeding threshold for microzoo        xthreshpic  =', xthreshpic
         WRITE(numout,*) '    poc feeding threshold for microzoo              xthreshpoc  =', xthreshpoc
         WRITE(numout,*) '    microzoo feeding threshold for microzoo         xthreshzoo  =', xthreshzoo
         WRITE(numout,*) '    feeding threshold for microzooplankton          xthresh     =', xthresh
         WRITE(numout,*) '    exsudation rate of microzooplankton             resrat      =', resrat
         WRITE(numout,*) '    microzooplankton mortality rate                 mzrat       =', mzrat
         WRITE(numout,*) '    maximal microzoo grazing rate                   grazrat     =', grazrat
         WRITE(numout,*) '    C egested fraction of fodd by microzoo          unassc      =', unassc
         WRITE(numout,*) '    N egested fraction of fodd by microzoo          unassn      =', unassn
         WRITE(numout,*) '    P egested fraction of fodd by microzoo          unassp      =', unassp
         WRITE(numout,*) '    Efficicency of microzoo growth                  epsher      =', epsher
         WRITE(numout,*) '    Minimum Efficiency of Microzoo growth           epshermin   =', epshermin
         WRITE(numout,*) '    Fraction excreted as semi-labile DOM            ssigma      =', ssigma
         WRITE(numout,*) '    Active respiration                              srespir     =', srespir
         WRITE(numout,*) '    half sturation constant for grazing 1           xkgraz      =', xkgraz
         WRITE(numout,*) '    Use of excess carbon for respiration            bmetexc     =', bmetexc
         WRITE(numout,*) '      Width of the grazing window                     xsigma      =', xsigma
         WRITE(numout,*) '      Maximum additional width of the grazing window  xsigmadel   =', xsigmadel
      ENDIF
      !
   END SUBROUTINE p5z_micro_init

   !!======================================================================
END MODULE p5zmicro
