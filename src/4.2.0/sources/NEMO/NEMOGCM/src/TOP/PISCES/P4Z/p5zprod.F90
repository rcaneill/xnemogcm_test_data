MODULE p5zprod
   !!======================================================================
   !!                         ***  MODULE p5zprod  ***
   !! TOP :  Growth Rate of the three phytoplanktons groups 
   !!        PISCES-QUOTA version of the module
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-05  (O. Aumont, C. Ethe) New parameterization of light limitation
   !!             3.6  !  2015-05  (O. Aumont) PISCES quota
   !!----------------------------------------------------------------------
   !!   p5z_prod       :   Compute the growth Rate of the two phytoplanktons groups
   !!   p5z_prod_init  :   Initialization of the parameters for growth
   !!   p5z_prod_alloc :   Allocate variables for growth
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE p4zlim
   USE p5zlim          !  Co-limitations of differents nutrients
   USE prtctl          !  print control for debugging
   USE iom             !  I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p5z_prod         ! called in p5zbio.F90
   PUBLIC   p5z_prod_init    ! called in trcsms_pisces.F90
   PUBLIC   p5z_prod_alloc

   !! * Shared module variables
   REAL(wp), PUBLIC ::  pislopen        !: P-I slope of nanophytoplankton
   REAL(wp), PUBLIC ::  pislopep        !: P-I slope of picophytoplankton
   REAL(wp), PUBLIC ::  pisloped        !: P-I slope of diatoms
   REAL(wp), PUBLIC ::  xadap           !: Adaptation factor to low light
   REAL(wp), PUBLIC ::  excretn         !: Excretion ratio of nanophyto
   REAL(wp), PUBLIC ::  excretp         !: Excretion ratio of picophyto
   REAL(wp), PUBLIC ::  excretd         !: Excretion ratio of diatoms
   REAL(wp), PUBLIC ::  bresp           !: Basal respiration rate
   REAL(wp), PUBLIC ::  thetanpm        !: Maximum Chl/N ratio of picophyto
   REAL(wp), PUBLIC ::  thetannm        !: Maximum Chl/N ratio of nanophyto
   REAL(wp), PUBLIC ::  thetandm        !: Maximum Chl/N ratio of diatoms
   REAL(wp), PUBLIC ::  chlcmin         !: Minimum Chl/C ratio of phytoplankton
   REAL(wp), PUBLIC ::  grosip          !: Mean Si/C ratio of diatoms

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   zdaylen ! day length
   
   REAL(wp) :: r1_rday                !: 1 / rday
   REAL(wp) :: texcretn               !: 1 - excretn 
   REAL(wp) :: texcretp               !: 1 - excretp 
   REAL(wp) :: texcretd               !: 1 - excretd        

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p5zprod.F90 15459 2021-10-29 08:19:18Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p5z_prod( kt , knt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p5z_prod  ***
      !!
      !! ** Purpose :   Compute the phytoplankton production depending on
      !!              light, temperature and nutrient availability
      !!              Computes also the uptake of nutrients. PISCES-quota
      !!              relies on a full quota formalism
      !!---------------------------------------------------------------------
      !
      INTEGER, INTENT(in) :: kt, knt
      INTEGER, INTENT(in) :: Kbb, Kmm, Krhs      ! time level indices
      !
      INTEGER  ::   ji, jj, jk
      REAL(wp) ::   zsilfac, znanotot, zpicotot, zdiattot, zconctemp, zconctemp2
      REAL(wp) ::   zration, zratiop, zratiof, zmax, ztn, zadap
      REAL(wp) ::   zpronmax, zpropmax, zprofmax, zratio
      REAL(wp) ::   zlim, zsilfac2, zsiborn, zprod, zprontot, zproptot, zprodtot
      REAL(wp) ::   zproddoc, zproddon, zproddop, zprodsil, zprodfer, zprodlig, zresptot     
      REAL(wp) ::   zprnutmax, zprochln, zprochld, zprochlp
      REAL(wp) ::   zpislopen, zpislopep, zpisloped
      REAL(wp) ::   zval, zpptot, zpnewtot, zpregtot
      REAL(wp) ::   zqfpmax, zqfnmax, zqfdmax
      REAL(wp) ::   zfact, zrfact2, zmaxsi, zratiosi, zsizetmp, zlimfac, zsilim
      CHARACTER (len=25) :: charout
      REAL(wp), DIMENSION(jpi,jpj    ) :: zmixnano, zmixpico, zmixdiat
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zpislopeadn, zpislopeadp, zpislopeadd
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zprnut, zprmaxp, zprmaxn, zprmaxd
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zprbio, zprpic, zprdia, zysopt
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zprchln, zprchlp, zprchld
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zprorcan, zprorcap, zprorcad 
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zprofed, zprofep, zprofen
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zpronewn, zpronewp, zpronewd
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zproregn, zproregp, zproregd
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zpropo4n, zpropo4p, zpropo4d
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zprodopn, zprodopp, zprodopd
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zrespn, zrespp, zrespd
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zmxl_fac, zmxl_chl
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) :: zpligprod
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p5z_prod')

      ! Initialize the local arrays
      zprorcan(:,:,:) = 0._wp ; zprorcap(:,:,:) = 0._wp ; zprorcad(:,:,:) = 0._wp
      zprofed (:,:,:) = 0._wp ; zprofep (:,:,:) = 0._wp ; zprofen (:,:,:) = 0._wp
      zpronewn(:,:,:) = 0._wp ; zpronewp(:,:,:) = 0._wp ; zpronewd(:,:,:) = 0._wp
      zproregn(:,:,:) = 0._wp ; zproregp(:,:,:) = 0._wp ; zproregd(:,:,:) = 0._wp 
      zpropo4n(:,:,:) = 0._wp ; zpropo4p(:,:,:) = 0._wp ; zpropo4d(:,:,:) = 0._wp
      zprdia  (:,:,:) = 0._wp ; zprpic  (:,:,:) = 0._wp ; zprbio  (:,:,:) = 0._wp
      zprodopn(:,:,:) = 0._wp ; zprodopp(:,:,:) = 0._wp ; zprodopd(:,:,:) = 0._wp
      zysopt  (:,:,:) = 0._wp 
      zrespn  (:,:,:) = 0._wp ; zrespp  (:,:,:) = 0._wp ; zrespd  (:,:,:) = 0._wp 
      zmxl_fac(:,:,:) = 0._wp ; zmxl_chl(:,:,:) = 0._wp

      ! Computation of the optimal production rates and nutrient uptake
      ! rates. Based on a Q10 description of the thermal dependency.
      zprnut (:,:,:) =  0.8_wp * r1_rday * tgfunc(:,:,:)
      zprmaxn(:,:,:) =  0.8_wp * (1. + xpsino3 * qnnmax ) * r1_rday * tgfunc(:,:,:)
      zprmaxd(:,:,:) =  0.8_wp * (1. + xpsino3 * qndmax ) * r1_rday * tgfunc(:,:,:)
      zprmaxp(:,:,:) =  0.6_wp * (1. + xpsino3 * qnpmax ) * r1_rday * tgfunc(:,:,:)

      ! Impact of the day duration and light intermittency on phytoplankton growth
      ! Intermittency is supposed to have a similar effect on production as 
      ! day length (Shatwell et al., 2012). The correcting factor is zmxl_fac. 
      ! zmxl_chl is the fractional day length and is used to compute the mean
      ! PAR during daytime. The effect of mixing is computed using the 
      ! absolute light level definition of the euphotic zone
      ! ------------------------------------------------------------------------- 

      IF ( ln_p4z_dcyc ) THEN    ! Diurnal cycle in PISCES

         DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
            IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
               IF( gdepw(ji,jj,jk+1,Kmm) <= hmld(ji,jj) ) THEN
                  zval = MIN(1., heup_01(ji,jj) / ( hmld(ji,jj) + rtrn ))
               ENDIF
               zmxl_chl(ji,jj,jk) = zval / 24.
               zmxl_fac(ji,jj,jk) = 1.0 - exp( -0.26 * zval )
            ENDIF
         END_3D

      ELSE ! No diurnal cycle in PISCES

         DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
            IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
               zval = MAX( 1., strn(ji,jj) )
               IF( gdepw(ji,jj,jk+1,Kmm) <= hmld(ji,jj) ) THEN
                  zval = zval * MIN(1., heup_01(ji,jj) / ( hmld(ji,jj) + rtrn ))
               ENDIF
               zmxl_chl(ji,jj,jk) = zval / 24.
               zmxl_fac(ji,jj,jk) = 1.0 - exp( -0.26 * zval )
            ENDIF
         END_3D

      ENDIF

      zprbio(:,:,:) = zprmaxn(:,:,:) * zmxl_fac(:,:,:)
      zprdia(:,:,:) = zprmaxd(:,:,:) * zmxl_fac(:,:,:)
      zprpic(:,:,:) = zprmaxp(:,:,:) * zmxl_fac(:,:,:)

      ! Maximum light intensity
      zdaylen(:,:) = MAX(1., strn(:,:)) / 24.

      ! Computation of the P-I slope for nanos, picos and diatoms
      ! The formulation proposed by Geider et al. (1997) has been used.
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
         IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
            ! Computation of the P-I slope for nanos and diatoms
            ztn         = MAX( 0., ts(ji,jj,jk,jp_tem,Kmm) - 15. )
            zadap       = xadap * ztn / ( 2.+ ztn )
            !
            ! Nanophytoplankton
            zpislopeadn(ji,jj,jk) = pislopen * tr(ji,jj,jk,jpnch,Kbb)    &
            &                       /( tr(ji,jj,jk,jpphy,Kbb) * 12. + rtrn)

            ! Picophytoplankton
            zpislopeadp(ji,jj,jk) = pislopep * ( 1. + zadap * EXP( -0.25 * epico(ji,jj,jk) ) )   &
            &                       * tr(ji,jj,jk,jppch,Kbb) /( tr(ji,jj,jk,jppic,Kbb) * 12. + rtrn)

            ! Diatoms
            zpislopeadd(ji,jj,jk) = pisloped * tr(ji,jj,jk,jpdch,Kbb)    &
               &                    /( tr(ji,jj,jk,jpdia,Kbb) * 12. + rtrn)
            !
            zpislopen = zpislopeadn(ji,jj,jk) / ( zprbio(ji,jj,jk) * rday * xlimphy(ji,jj,jk) + rtrn )
            zpislopep = zpislopeadp(ji,jj,jk) / ( zprpic(ji,jj,jk) * rday * xlimpic(ji,jj,jk) + rtrn )
            zpisloped = zpislopeadd(ji,jj,jk) / ( zprdia(ji,jj,jk) * rday * xlimdia(ji,jj,jk) + rtrn )

            ! Computation of production function for Carbon
            ! Actual light levels are used here 
            !  ---------------------------------------------
            zprbio(ji,jj,jk) = zprbio(ji,jj,jk) * ( 1.- EXP( -zpislopen * enano(ji,jj,jk) / zmxl_fac(ji,jj,jk) )  )
            zprpic(ji,jj,jk) = zprpic(ji,jj,jk) * ( 1.- EXP( -zpislopep * epico(ji,jj,jk) / zmxl_fac(ji,jj,jk) )  )
            zprdia(ji,jj,jk) = zprdia(ji,jj,jk) * ( 1.- EXP( -zpisloped * ediat(ji,jj,jk) / zmxl_fac(ji,jj,jk) )  )

            !  Computation of production function for Chlorophyll
            !  Mean light level in the mixed layer (when appropriate)
            !  is used here (acclimation is in general slower than 
            !  the characteristic time scales of vertical mixing)
            !  ------------------------------------------------------
            zpislopen = zpislopen * zmxl_fac(ji,jj,jk) / ( zmxl_chl(ji,jj,jk) + rtrn )
            zpisloped = zpisloped * zmxl_fac(ji,jj,jk) / ( zmxl_chl(ji,jj,jk) + rtrn )
            zpislopep = zpislopep * zmxl_fac(ji,jj,jk) / ( zmxl_chl(ji,jj,jk) + rtrn )
            zprchln(ji,jj,jk) = zprmaxn(ji,jj,jk) * ( 1.- EXP( -zpislopen * enanom(ji,jj,jk) )  )
            zprchlp(ji,jj,jk) = zprmaxp(ji,jj,jk) * ( 1.- EXP( -zpislopep * epicom(ji,jj,jk) )  )
            zprchld(ji,jj,jk) = zprmaxd(ji,jj,jk) * ( 1.- EXP( -zpisloped * ediatm(ji,jj,jk) )  )
         ENDIF
      END_3D

      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
          IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
            ! Si/C of diatoms
            ! ------------------------
            ! Si/C increases with iron stress and silicate availability (zsilfac)
            ! Si/C is arbitrariliy increased for very high Si concentrations
            ! to mimic the very high ratios observed in the Southern Ocean (zsilfac2)
            ! A parameterization derived from Flynn (2003) is used for the control
            ! when Si is not limiting which is similar to the parameterisation
            ! proposed by Gurney and Davidson (1999).
            ! -----------------------------------------------------------------------
            zlim  = tr(ji,jj,jk,jpsil,Kbb) / ( tr(ji,jj,jk,jpsil,Kbb) + xksi1 )
            zsilim = xlimdia(ji,jj,jk) * zprdia(ji,jj,jk) / ( zprmaxd(ji,jj,jk) + rtrn )
            zsiborn = tr(ji,jj,jk,jpsil,Kbb) * tr(ji,jj,jk,jpsil,Kbb) * tr(ji,jj,jk,jpsil,Kbb)
            IF (gphit(ji,jj) < -30 ) THEN
              zsilfac2 = 1. + 1. * zsiborn / ( zsiborn + xksi2**3 )
            ELSE
              zsilfac2 = 1.
            ENDIF
            zratiosi = 1.0 - tr(ji,jj,jk,jpdsi,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn ) / ( zsilfac2 * grosip * 3.0 + rtrn )
            zratiosi = MAX(0., MIN(1.0, zratiosi) )
            zmaxsi  = (1.0 + 0.1**4) * zratiosi**4 / ( zratiosi**4 + 0.1**4 )
            IF ( xlimsi(ji,jj,jk) /= xlimdia(ji,jj,jk) ) THEN
               zysopt(ji,jj,jk) = zlim * zsilfac2 * grosip * 1.0 * zmaxsi
            ELSE
               zysopt(ji,jj,jk) = zlim * zsilfac2 * grosip * 1.0 * zsilim**0.7 * zmaxsi
            ENDIF
         ENDIF
      END_3D

      !  Sea-ice effect on production
      ! No production is assumed below sea ice
      ! -------------------------------------- 
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
         zprbio(ji,jj,jk)  = zprbio(ji,jj,jk) * ( 1. - fr_i(ji,jj) )
         zprpic(ji,jj,jk)  = zprpic(ji,jj,jk) * ( 1. - fr_i(ji,jj) ) 
         zprdia(ji,jj,jk)  = zprdia(ji,jj,jk) * ( 1. - fr_i(ji,jj) ) 
         zprnut(ji,jj,jk)  = zprnut(ji,jj,jk) * ( 1. - fr_i(ji,jj) )
      END_3D

      ! Computation of the various production and uptake terms of nanophytoplankton 
      ! Interactions between N and P are modeled according to the Chain Model 
      ! of Pahlow et al. (2009). Iron uptake is modeled following traditional
      ! Droop kinetics. When the quota is approaching the maximum achievable
      ! quota, uptake is downregulated according to a sigmoidal function 
      ! (power 2), as proposed by Flynn (2003)
      ! ---------------------------------------------------------------------------
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
         IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
            !  production terms for nanophyto.
            zprorcan(ji,jj,jk) = zprbio(ji,jj,jk)  * xlimphy(ji,jj,jk) * tr(ji,jj,jk,jpphy,Kbb) * rfact2

            ! Size computation
            ! Size is made a function of the limitation of of phytoplankton growth
            ! Strongly limited cells are supposed to be smaller. sizena is the 
            ! size at time step t+1 and is thus updated at the end of the 
            ! current time step
            ! --------------------------------------------------------------------
            zlimfac = xlimphys(ji,jj,jk) * zprchln(ji,jj,jk) / ( zprmaxn(ji,jj,jk) + rtrn )
            zsizetmp = 1.0 + 1.3 * ( xsizern - 1.0 ) * zlimfac**3/(0.3 + zlimfac**3)
            sizena(ji,jj,jk) = MIN(xsizern, MAX( sizena(ji,jj,jk), zsizetmp ) )
            ! Maximum potential uptake rate
            zration = tr(ji,jj,jk,jpnph,Kbb) / ( tr(ji,jj,jk,jpphy,Kbb) + rtrn )
            zratiop = tr(ji,jj,jk,jppph,Kbb) / ( tr(ji,jj,jk,jpphy,Kbb) + rtrn )
            zratiof = tr(ji,jj,jk,jpnfe,Kbb) / ( tr(ji,jj,jk,jpphy,Kbb) + rtrn )
            zprnutmax = zprnut(ji,jj,jk) * fvnuptk(ji,jj,jk) / rno3 * tr(ji,jj,jk,jpphy,Kbb) * rfact2
            ! Uptake of nitrogen
            zratio = 1.0 - MIN( 1., zration / (xqnnmax(ji,jj,jk) + rtrn) )
            zmax = MAX(0., MIN(1., zratio**2 / (0.05**2 + zratio**2) ) )
            zpronmax = zprnutmax * zmax * MAX(0., MIN(1., ( zratiop - xqpnmin(ji,jj,jk) )   &
            &          / ( xqpnmax(ji,jj,jk) - xqpnmin(ji,jj,jk) + rtrn ), xlimnfe(ji,jj,jk) ) )
            zpronmax = zpronmax * xqnnmin(ji,jj,jk) / qnnmin
            zpronewn(ji,jj,jk) = zpronmax * xnanono3(ji,jj,jk)
            zproregn(ji,jj,jk) = zpronmax * xnanonh4(ji,jj,jk)
            ! Uptake of phosphorus and DOP
            zratio = 1.0 - MIN( 1., zratiop / (xqpnmax(ji,jj,jk) + rtrn) )
            zmax = MAX(0., MIN(1., zratio**2 / (0.05**2 + zratio**2) ) )
            zpropmax = zprnutmax * zmax * xlimnfe(ji,jj,jk)
            zpropo4n(ji,jj,jk) = zpropmax * xnanopo4(ji,jj,jk)
            zprodopn(ji,jj,jk) = zpropmax * xnanodop(ji,jj,jk)
            ! Uptake of iron
            zqfnmax = xqfuncfecn(ji,jj,jk) + ( qfnmax - xqfuncfecn(ji,jj,jk) ) * xlimnpn(ji,jj,jk)
            zratio = 1.0 - MIN( 1., zratiof / zqfnmax )
            zmax = MAX(0., MIN(1., zratio**2/ (0.05**2 + zratio**2) ) )
            zprofmax = zprnutmax * zqfnmax * zmax 
            zprofen(ji,jj,jk) = zprofmax * xnanofer(ji,jj,jk)    &
            &          * (1. + 0.8 * xnanono3(ji,jj,jk) / ( rtrn  &
            &          + xnanono3(ji,jj,jk) + xnanonh4(ji,jj,jk) ) * (1. - xnanofer(ji,jj,jk) ) )
         ENDIF
      END_3D

      ! Computation of the various production and uptake terms of picophytoplankton 
      ! Interactions between N and P are modeled according to the Chain Model 
      ! of Pahlow et al. (2009). Iron uptake is modeled following traditional
      ! Droop kinetics. When the quota is approaching the maximum achievable
      ! quota, uptake is downregulated according to a sigmoidal function 
      ! (power 2), as proposed by Flynn (2003)
      ! ---------------------------------------------------------------------------
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
         IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
            !  production terms for picophyto.
            zprorcap(ji,jj,jk) = zprpic(ji,jj,jk)  * xlimpic(ji,jj,jk) * tr(ji,jj,jk,jppic,Kbb) * rfact2
            ! Size computation
            ! Size is made a function of the limitation of of phytoplankton growth
            ! Strongly limited cells are supposed to be smaller. sizepa is
            ! size at time step t+1 and is thus updated at the end of the 
            ! current time step
            ! --------------------------------------------------------------------
            zlimfac = zprchlp(ji,jj,jk)  * xlimpics(ji,jj,jk) / ( zprmaxp(ji,jj,jk) + rtrn )
            zsizetmp = 1.0 + 1.3 * ( xsizerp - 1.0 ) * zlimfac**3/(0.3 + zlimfac**3)
            sizepa(ji,jj,jk) = min(xsizerp, max( sizepa(ji,jj,jk), zsizetmp ) )
            ! Maximum potential uptake rate of nutrients
            zration = tr(ji,jj,jk,jpnpi,Kbb) / ( tr(ji,jj,jk,jppic,Kbb) + rtrn )
            zratiop = tr(ji,jj,jk,jpppi,Kbb) / ( tr(ji,jj,jk,jppic,Kbb) + rtrn )
            zratiof = tr(ji,jj,jk,jppfe,Kbb) / ( tr(ji,jj,jk,jppic,Kbb) + rtrn )
            zprnutmax = zprnut(ji,jj,jk) * fvpuptk(ji,jj,jk) / rno3 * tr(ji,jj,jk,jppic,Kbb) * rfact2
            ! Uptake of nitrogen
            zratio = 1.0 - MIN( 1., zration / (xqnpmax(ji,jj,jk) + rtrn) )
            zmax = MAX(0., MIN(1., zratio**2/ (0.05**2 + zratio**2) ) )
            zpronmax = zprnutmax * zmax * MAX(0., MIN(1., ( zratiop - xqppmin(ji,jj,jk) )   &
            &          / ( xqppmax(ji,jj,jk) - xqppmin(ji,jj,jk) + rtrn ), xlimpfe(ji,jj,jk) ) )
            zpronmax = zpronmax * xqnpmin(ji,jj,jk) / qnnmin
            zpronewp(ji,jj,jk) = zpronmax * xpicono3(ji,jj,jk) 
            zproregp(ji,jj,jk) = zpronmax * xpiconh4(ji,jj,jk)
            ! Uptake of phosphorus
            zratio = 1.0 - MIN( 1., zratiop / (xqppmax(ji,jj,jk) + rtrn) )
            zmax = MAX(0., MIN(1., zratio**2 / (0.05**2 + zratio**2) ) )
            zpropmax = zprnutmax * zmax * xlimpfe(ji,jj,jk) 
            zpropo4p(ji,jj,jk) = zpropmax * xpicopo4(ji,jj,jk)
            zprodopp(ji,jj,jk) = zpropmax * xpicodop(ji,jj,jk)
            ! Uptake of iron
            zqfpmax = xqfuncfecp(ji,jj,jk) + ( qfpmax - xqfuncfecp(ji,jj,jk) ) * xlimnpp(ji,jj,jk)
            zratio = 1.0 - MIN( 1., zratiof / zqfpmax )
            zmax = MAX(0., MIN(1., zratio**2 / (0.05**2 + zratio**2) ) )
            zprofmax = zprnutmax * zqfpmax * zmax
            zprofep(ji,jj,jk) = zprofmax * xpicofer(ji,jj,jk)  &
            &          * (1. + 0.8 * xpicono3(ji,jj,jk) / ( rtrn   &
            &          + xpicono3(ji,jj,jk) + xpiconh4(ji,jj,jk) ) * (1. - xpicofer(ji,jj,jk) ) )
         ENDIF
      END_3D

      ! Computation of the various production and uptake terms of diatoms
      ! Interactions between N and P are modeled according to the Chain Model 
      ! of Pahlow et al. (2009). Iron uptake is modeled following traditional
      ! Droop kinetics. When the quota is approaching the maximum achievable
      ! quota, uptake is downregulated according to a sigmoidal function 
      ! (power 2), as proposed by Flynn (2003)
      ! ---------------------------------------------------------------------------
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
         IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
            !  production terms for diatomees
            zprorcad(ji,jj,jk) = zprdia(ji,jj,jk) * xlimdia(ji,jj,jk) * tr(ji,jj,jk,jpdia,Kbb) * rfact2
            ! Size computation
            ! Size is made a function of the limitation of of phytoplankton growth
            ! Strongly limited cells are supposed to be smaller. sizeda is
            ! size at time step t+1 and is thus updated at the end of the 
            ! current time step. 
            ! --------------------------------------------------------------------
            zlimfac = zprchld(ji,jj,jk) * xlimdias(ji,jj,jk) / ( zprmaxd(ji,jj,jk) + rtrn )
            zsizetmp = 1.0 + 1.3 * ( xsizerd - 1.0 ) * zlimfac**3/(0.3 + zlimfac**3)
            sizeda(ji,jj,jk) = min(xsizerd, max( sizeda(ji,jj,jk), zsizetmp ) )
            ! Maximum potential uptake rate of nutrients
            zration = tr(ji,jj,jk,jpndi,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn )
            zratiop = tr(ji,jj,jk,jppdi,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn )
            zratiof = tr(ji,jj,jk,jpdfe,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn )
            zprnutmax = zprnut(ji,jj,jk) * fvduptk(ji,jj,jk) / rno3 * tr(ji,jj,jk,jpdia,Kbb) * rfact2
            ! Uptake of nitrogen
            zratio = 1.0 - MIN( 1., zration / (xqndmax(ji,jj,jk) + rtrn) )
            zmax = MAX(0., MIN(1., zratio**2 / (0.05**2 + zratio**2) ) )
            zpronmax = zprnutmax * zmax * MAX(0., MIN(1., ( zratiop - xqpdmin(ji,jj,jk) )   &
            &          / ( xqpdmax(ji,jj,jk) - xqpdmin(ji,jj,jk) + rtrn ), xlimdfe(ji,jj,jk) ) )
            zpronmax = zpronmax * xqndmin(ji,jj,jk) / qnnmin
            zpronewd(ji,jj,jk) = zpronmax * xdiatno3(ji,jj,jk)
            zproregd(ji,jj,jk) = zpronmax * xdiatnh4(ji,jj,jk)
            ! Uptake of phosphorus
            zratio = 1.0 - MIN( 1., zratiop / (xqpdmax(ji,jj,jk) + rtrn) )
            zmax = MAX(0., MIN(1., zratio**2/ (0.05**2 + zratio**2) ) )
            zpropmax = zprnutmax * zmax * xlimdfe(ji,jj,jk)
            zpropo4d(ji,jj,jk) = zpropmax * xdiatpo4(ji,jj,jk)
            zprodopd(ji,jj,jk) = zpropmax * xdiatdop(ji,jj,jk)
            ! Uptake of iron
            zqfdmax = xqfuncfecd(ji,jj,jk) + ( qfdmax - xqfuncfecd(ji,jj,jk) ) * xlimnpd(ji,jj,jk)
            zratio = 1.0 - MIN( 1., zratiof / zqfdmax )
            zmax = MAX(0., MIN(1., zratio**2 / (0.05**2 + zratio**2) ) )
            zprofmax = zprnutmax * zqfdmax * zmax
            zprofed(ji,jj,jk) = zprofmax * xdiatfer(ji,jj,jk)    &
            &          * (1. + 0.8 * xdiatno3(ji,jj,jk) / ( rtrn   &
            &          + xdiatno3(ji,jj,jk) + xdiatnh4(ji,jj,jk) ) * (1. - xdiatfer(ji,jj,jk) ) )
         ENDIF
      END_3D

      ! Production of Chlorophyll. The formulation proposed by Geider et al.
      ! is adopted here.
      ! --------------------------------------------------------------------
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
         IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
               !  production terms for nanophyto. ( chlorophyll )
            znanotot = enanom(ji,jj,jk) / ( zmxl_chl(ji,jj,jk) + rtrn )
            zprod = rday * (zpronewn(ji,jj,jk) + zproregn(ji,jj,jk)) * zprchln(ji,jj,jk) * xlimphy(ji,jj,jk)
            zprochln = thetannm * zprod / ( zpislopeadn(ji,jj,jk) * znanotot + rtrn )
            zprochln = MAX(zprochln, chlcmin * 12. * zprorcan (ji,jj,jk) )
               !  production terms for picophyto. ( chlorophyll )
            zpicotot = epicom(ji,jj,jk) / ( zmxl_chl(ji,jj,jk) + rtrn )
            zprod = rday * (zpronewp(ji,jj,jk) + zproregp(ji,jj,jk)) * zprchlp(ji,jj,jk) * xlimpic(ji,jj,jk)
            zprochlp = thetanpm * zprod / ( zpislopeadp(ji,jj,jk) * zpicotot + rtrn )
            zprochlp = MAX(zprochlp, chlcmin * 12. * zprorcap(ji,jj,jk) )
            !  production terms for diatoms ( chlorophyll )
            zdiattot = ediatm(ji,jj,jk) / ( zmxl_chl(ji,jj,jk) + rtrn )
            zprod = rday * (zpronewd(ji,jj,jk) + zproregd(ji,jj,jk)) * zprchld(ji,jj,jk) * xlimdia(ji,jj,jk)
            zprochld = thetandm * zprod / ( zpislopeadd(ji,jj,jk) * zdiattot + rtrn )
            zprochld = MAX(zprochld, chlcmin * 12. * zprorcad(ji,jj,jk) )
            !   Update the arrays TRA which contain the Chla sources and sinks
            tr(ji,jj,jk,jpnch,Krhs) = tr(ji,jj,jk,jpnch,Krhs) + zprochln * texcretn
            tr(ji,jj,jk,jpdch,Krhs) = tr(ji,jj,jk,jpdch,Krhs) + zprochld * texcretd
            tr(ji,jj,jk,jppch,Krhs) = tr(ji,jj,jk,jppch,Krhs) + zprochlp * texcretp
         ENDIF
      END_3D

      !   Update the arrays TRA which contain the biological sources and sinks
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
        zpptot   = zpropo4n(ji,jj,jk) + zpropo4d(ji,jj,jk) + zpropo4p(ji,jj,jk)
        zpnewtot = zpronewn(ji,jj,jk) + zpronewd(ji,jj,jk) + zpronewp(ji,jj,jk)
        zpregtot = zproregn(ji,jj,jk) + zproregd(ji,jj,jk) + zproregp(ji,jj,jk)

        zprontot = zpronewn(ji,jj,jk) + zproregn(ji,jj,jk)
        zproptot = zpronewp(ji,jj,jk) + zproregp(ji,jj,jk)
        zprodtot = zpronewd(ji,jj,jk) + zproregd(ji,jj,jk)
        !
        zproddoc = excretd * zprorcad(ji,jj,jk) &
        &        + excretn * zprorcan(ji,jj,jk) &
        &        + excretp * zprorcap(ji,jj,jk)
        !
        zproddop = excretd * zpropo4d(ji,jj,jk) - texcretd * zprodopd(ji,jj,jk) &
        &        + excretn * zpropo4n(ji,jj,jk) - texcretn * zprodopn(ji,jj,jk) &
        &        + excretp * zpropo4p(ji,jj,jk) - texcretp * zprodopp(ji,jj,jk)

        zproddon =  excretd * zprodtot + excretn * zprontot + excretp * zproptot

        zprodfer = texcretn * zprofen(ji,jj,jk) + texcretd * zprofed(ji,jj,jk) + texcretp * zprofep(ji,jj,jk)
        zresptot = zrespn(ji,jj,jk) + zrespp(ji,jj,jk) + zrespd(ji,jj,jk) 
        !
        tr(ji,jj,jk,jppo4,Krhs) = tr(ji,jj,jk,jppo4,Krhs) - zpptot
        tr(ji,jj,jk,jpno3,Krhs) = tr(ji,jj,jk,jpno3,Krhs) - zpnewtot
        tr(ji,jj,jk,jpnh4,Krhs) = tr(ji,jj,jk,jpnh4,Krhs) - zpregtot  
        !
        tr(ji,jj,jk,jpphy,Krhs) = tr(ji,jj,jk,jpphy,Krhs)         &
           &                     + zprorcan(ji,jj,jk) * texcretn  &
           &                     - xpsino3 * zpronewn(ji,jj,jk)   &
           &                     - xpsinh4 * zproregn(ji,jj,jk)   &
           &                     - zrespn(ji,jj,jk) 

        tr(ji,jj,jk,jpnph,Krhs) = tr(ji,jj,jk,jpnph,Krhs) + zprontot * texcretn
        tr(ji,jj,jk,jppph,Krhs) = tr(ji,jj,jk,jppph,Krhs) + ( zpropo4n(ji,jj,jk) + zprodopn(ji,jj,jk) ) * texcretn
        tr(ji,jj,jk,jpnfe,Krhs) = tr(ji,jj,jk,jpnfe,Krhs) + zprofen(ji,jj,jk) * texcretn

        !
        tr(ji,jj,jk,jppic,Krhs) = tr(ji,jj,jk,jppic,Krhs)         &
           &                     + zprorcap(ji,jj,jk) * texcretp  &
           &                     - xpsino3 * zpronewp(ji,jj,jk)   &
           &                     - xpsinh4 * zproregp(ji,jj,jk)   &
           &                     - zrespp(ji,jj,jk) 

        tr(ji,jj,jk,jpnpi,Krhs) = tr(ji,jj,jk,jpnpi,Krhs) + zproptot * texcretp
        tr(ji,jj,jk,jpppi,Krhs) = tr(ji,jj,jk,jpppi,Krhs) + ( zpropo4p(ji,jj,jk) + zprodopp(ji,jj,jk) ) * texcretp
        tr(ji,jj,jk,jppfe,Krhs) = tr(ji,jj,jk,jppfe,Krhs) + zprofep(ji,jj,jk) * texcretp

        !
        tr(ji,jj,jk,jpdia,Krhs) = tr(ji,jj,jk,jpdia,Krhs)         &
           &                    + zprorcad(ji,jj,jk) * texcretd   &
           &                    - xpsino3 * zpronewd(ji,jj,jk)    &
           &                    - xpsinh4 * zproregd(ji,jj,jk)    &
           &                    - zrespd(ji,jj,jk) 

        tr(ji,jj,jk,jpndi,Krhs) = tr(ji,jj,jk,jpndi,Krhs) + zprodtot * texcretd
        tr(ji,jj,jk,jppdi,Krhs) = tr(ji,jj,jk,jppdi,Krhs) + ( zpropo4d(ji,jj,jk) + zprodopd(ji,jj,jk) ) * texcretd
        tr(ji,jj,jk,jpdfe,Krhs) = tr(ji,jj,jk,jpdfe,Krhs) + zprofed(ji,jj,jk) * texcretd
        tr(ji,jj,jk,jpdsi,Krhs) = tr(ji,jj,jk,jpdsi,Krhs) + zprmaxd(ji,jj,jk) * zysopt(ji,jj,jk) * rfact2 * tr(ji,jj,jk,jpdia,Kbb)
        tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) + zproddoc
        tr(ji,jj,jk,jpdon,Krhs) = tr(ji,jj,jk,jpdon,Krhs) + zproddon                                        
        tr(ji,jj,jk,jpdop,Krhs) = tr(ji,jj,jk,jpdop,Krhs) + zproddop
  
        tr(ji,jj,jk,jpoxy,Krhs) = tr(ji,jj,jk,jpoxy,Krhs) &
           &                     + o2ut * zpregtot + ( o2ut + o2nit ) * zpnewtot - o2ut * zresptot 

        tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) - zprodfer
        consfe3(ji,jj,jk)       = zprodfer * 75.0 / ( rtrn + ( plig(ji,jj,jk) + 75.0 * (1.0 - plig(ji,jj,jk) ) )   &
           &                   * tr(ji,jj,jk,jpfer,Kbb) ) / rfact2
        tr(ji,jj,jk,jpsil,Krhs) = tr(ji,jj,jk,jpsil,Krhs) - zprmaxd(ji,jj,jk) * zysopt(ji,jj,jk) * rfact2 * tr(ji,jj,jk,jpdia,Kbb) 

        tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) - zpptot  &
           &                     + xpsino3 * zpronewn(ji,jj,jk) + xpsinh4 * zproregn(ji,jj,jk)   &
           &                     + xpsino3 * zpronewp(ji,jj,jk) + xpsinh4 * zproregp(ji,jj,jk)   &
           &                     + xpsino3 * zpronewd(ji,jj,jk) + xpsinh4 * zproregd(ji,jj,jk)  

        tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) + rno3 * ( zpnewtot - zpregtot )
        !
      END_3D
     
     ! Production and uptake of ligands by phytoplankton. This part is activated 
     ! when ln_ligand is set to .true. in the namelist. Ligand uptake is small 
     ! and based on the FeL model by Morel et al. (2008) and on the study of
     ! Shaked and Lis (2012)
     ! -------------------------------------------------------------------------
     IF( ln_ligand ) THEN
         DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
           zproddoc = excretd * zprorcad(ji,jj,jk) + excretn * zprorcan(ji,jj,jk) + excretp * zprorcap(ji,jj,jk)
           zprodfer = texcretn * zprofen(ji,jj,jk) + texcretd * zprofed(ji,jj,jk) + texcretp * zprofep(ji,jj,jk)
           zprodlig = plig(ji,jj,jk) / ( rtrn + plig(ji,jj,jk) + 75.0 * (1.0 - plig(ji,jj,jk) ) ) * lthet 
           !
           tr(ji,jj,jk,jplgw,Krhs) = tr(ji,jj,jk,jplgw,Krhs) + zproddoc * ldocp - zprodfer * zprodlig
        END_3D
     ENDIF

    ! Total primary production per year
    IF( iom_use( "tintpp" ) .OR. ( ln_check_mass .AND. kt == nitend .AND. knt == nrdttrc )  )  &
      & tpp = glob_sum( 'p5zprod', ( zprorcan(:,:,:) + zprorcad(:,:,:) + zprorcap(:,:,:) ) * cvol(:,:,:) )

    IF( lk_iomput .AND.  knt == nrdttrc ) THEN
       zfact = 1.e+3 * rfact2r  !  conversion from mol/l/kt to  mol/m3/s
       !
       CALL iom_put( "PPPHYP"  , zprorcap(:,:,:) * zfact * tmask(:,:,:)   ) ! primary production by picophyto
       CALL iom_put( "PPPHYN"  , zprorcan(:,:,:) * zfact * tmask(:,:,:) )  ! primary production by nanophyto
       CALL iom_put( "PPPHYD"  , zprorcad(:,:,:) * zfact * tmask(:,:,:)   ) ! primary production by diatomes
       CALL iom_put( "PPNEWN"  , zpronewp(:,:,:) * zfact * tmask(:,:,:)    ) ! new primary production by picophyto
       CALL iom_put( "PPNEWN"  , zpronewn(:,:,:) * zfact * tmask(:,:,:)    ) ! new primary production by nanophyto
       CALL iom_put( "PPNEWD"  , zpronewd(:,:,:) * zfact * tmask(:,:,:)   ) ! new primary production by diatomes
       CALL iom_put( "PBSi"    , zprmaxd (:,:,:) * zfact * tmask(:,:,:) * zysopt(:,:,:)  ) ! biogenic silica production
       CALL iom_put( "PFeP"    , zprofep (:,:,:) * zfact * tmask(:,:,:)  ) ! biogenic iron production by picophyto
       CALL iom_put( "PFeN"    , zprofen(:,:,:) * zfact * tmask(:,:,:)  ) ! biogenic iron production by nanophyto
       CALL iom_put( "PFeD"    , zprofed(:,:,:) * zfact * tmask(:,:,:)  ) ! biogenic iron production by  diatomes
       IF( ln_ligand .AND. ( iom_use( "LPRODP" ) .OR. iom_use( "LDETP" ) ) ) THEN
           ALLOCATE(  zpligprod(jpi,jpj,jpk) )
           zpligprod(:,:,:) = excretd * zprorcad(:,:,:) + excretn * zprorcan(:,:,:) + excretp * zprorcap(:,:,:)
           CALL iom_put( "LPRODP"  , zpligprod(:,:,:) * ldocp * 1e9 * zfact * tmask(:,:,:) )
           !
           zpligprod(:,:,:) = ( texcretn * zprofen(:,:,:) + texcretd * zprofed(:,:,:) + texcretp * zprofep(:,:,:) ) & 
             &                  * plig(:,:,:) / ( rtrn + plig(:,:,:) + 75.0 * (1.0 - plig(:,:,:) ) )
           CALL iom_put( "LDETP"   , zpligprod(:,:,:)  * lthet * 1e9 * zfact * tmask(:,:,:) )
           DEALLOCATE(  zpligprod )
       ENDIF
       CALL iom_put( "Mumax"   , zprmaxn(:,:,:) * tmask(:,:,:)  ) ! Maximum growth rate
       CALL iom_put( "MuP"     , zprpic(:,:,:) * xlimpic(:,:,:) * tmask(:,:,:) ) ! Realized growth rate for picophyto
       CALL iom_put( "MuN"     , zprbio(:,:,:) * xlimphy(:,:,:) * tmask(:,:,:) ) ! Realized growth rate for nanophyto
       CALL iom_put( "MuD"     , zprdia(:,:,:) * xlimdia(:,:,:) * tmask(:,:,:) ) ! Realized growth rate for diatoms
       CALL iom_put( "LPlight" , zprpic(:,:,:) / (zprmaxp(:,:,:) + rtrn) * tmask(:,:,:)  )  ! light limitation term
       CALL iom_put( "LNlight" , zprbio(:,:,:) / (zprmaxn(:,:,:) + rtrn) * tmask(:,:,:)  )  ! light limitation term
       CALL iom_put( "LDlight" , zprdia(:,:,:) / (zprmaxd(:,:,:) + rtrn) * tmask(:,:,:)   )
       CALL iom_put( "MunetP"  , ( tr(:,:,:,jppic,Krhs)/rfact2/(tr(:,:,:,jppic,Kbb)+ rtrn ) * tmask(:,:,:)) ) ! Realized growth rate for picophyto
       CALL iom_put( "MunetN"  , ( tr(:,:,:,jpphy,Krhs)/rfact2/(tr(:,:,:,jpphy,Kbb)+ rtrn ) * tmask(:,:,:)) ) ! Realized growth rate for picophyto
       CALL iom_put( "MunetD"  , ( tr(:,:,:,jpdia,Krhs)/rfact2/(tr(:,:,:,jpdia,Kbb)+ rtrn ) * tmask(:,:,:)) ) ! Realized growth rate for picophyto
       CALL iom_put( "TPP"     , ( zprorcap(:,:,:) + zprorcan(:,:,:) + zprorcad(:,:,:) ) * zfact * tmask(:,:,:)  )  ! total primary production
       CALL iom_put( "TPNEW"   , ( zpronewp(:,:,:) + zpronewn(:,:,:) + zpronewd(:,:,:) ) * zfact * tmask(:,:,:)  ) ! total new production
       CALL iom_put( "TPBFE"   , ( zprofep (:,:,:) + zprofen (:,:,:) + zprofed (:,:,:) ) * zfact * tmask(:,:,:)  )  ! total biogenic iron production
       CALL iom_put( "tintpp"  , tpp * zfact )  !  global total integrated primary production molC/s
     ENDIF

      IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('prod')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p5z_prod')
      !
   END SUBROUTINE p5z_prod


   SUBROUTINE p5z_prod_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p5z_prod_init  ***
      !!
      !! ** Purpose :   Initialization of phytoplankton production parameters
      !!
      !! ** Method  :   Read the namp5zprod namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist namp5zprod
      !!----------------------------------------------------------------------
      INTEGER :: ios    ! Local integer output status for namelist read
      !!
      NAMELIST/namp5zprod/ pislopen, pislopep, pisloped, excretn, excretp, excretd,     &
         &                 thetannm, thetanpm, thetandm, chlcmin, grosip, bresp, xadap
      !!----------------------------------------------------------------------

      READ  ( numnatp_ref, namp5zprod, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namp5zprod in reference namelist' )

      READ  ( numnatp_cfg, namp5zprod, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 ) CALL ctl_nam ( ios , 'namp5zprod in configuration namelist' )
      IF(lwm) WRITE ( numonp, namp5zprod )

      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' '
         WRITE(numout,*) ' Namelist parameters for phytoplankton growth, namp5zprod'
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '    mean Si/C ratio                           grosip       =', grosip
         WRITE(numout,*) '    P-I slope                                 pislopen     =', pislopen
         WRITE(numout,*) '    P-I slope  for diatoms                    pisloped     =', pisloped
         WRITE(numout,*) '    P-I slope  for picophytoplankton          pislopep     =', pislopep
         WRITE(numout,*) '    Acclimation factor to low light           xadap        =', xadap
         WRITE(numout,*) '    excretion ratio of nanophytoplankton      excretn      =', excretn
         WRITE(numout,*) '    excretion ratio of picophytoplankton      excretp      =', excretp
         WRITE(numout,*) '    excretion ratio of diatoms                excretd      =', excretd
         WRITE(numout,*) '    basal respiration in phytoplankton        bresp        =', bresp
         WRITE(numout,*) '    Maximum Chl/C in phytoplankton            chlcmin      =', chlcmin
         WRITE(numout,*) '    Minimum Chl/N in nanophytoplankton        thetannm     =', thetannm
         WRITE(numout,*) '    Minimum Chl/N in picophytoplankton        thetanpm     =', thetanpm
         WRITE(numout,*) '    Minimum Chl/N in diatoms                  thetandm     =', thetandm
      ENDIF
      !
      r1_rday   = 1._wp / rday 
      texcretn  = 1._wp - excretn
      texcretp  = 1._wp - excretp
      texcretd  = 1._wp - excretd
      tpp       = 0._wp
      !
   END SUBROUTINE p5z_prod_init


   INTEGER FUNCTION p5z_prod_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p5z_prod_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( zdaylen(jpi,jpj), STAT = p5z_prod_alloc )
      !
      IF( p5z_prod_alloc /= 0 ) CALL ctl_stop( 'STOP', 'p5z_prod_alloc : failed to allocate arrays.' )
      !
   END FUNCTION p5z_prod_alloc
   !!======================================================================
END MODULE p5zprod
