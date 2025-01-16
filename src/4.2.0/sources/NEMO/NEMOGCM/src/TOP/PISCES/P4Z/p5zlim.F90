MODULE p5zlim
   !!======================================================================
   !!                         ***  MODULE p5zlim  ***
   !! TOP :   PISCES-QUOTA : Computes the various nutrient limitation terms
   !!                        of phytoplankton
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-04  (O. Aumont, C. Ethe) Limitation for iron modelled in quota 
   !!             3.6  !  2015-05  (O. Aumont) PISCES quota
   !!----------------------------------------------------------------------
   !!   p5z_lim        :   Compute the nutrients limitation terms 
   !!   p5z_lim_init   :   Read the namelist 
   !!----------------------------------------------------------------------
   USE oce_trc         ! Shared ocean-passive tracers variables
   USE trc             ! Tracers defined
   USE p4zlim          ! Nutrient limitation 
   USE sms_pisces      ! PISCES variables
   USE iom             !  I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC p5z_lim           ! called in p4zbio.F90  
   PUBLIC p5z_lim_init      ! called in trcsms_pisces.F90 
   PUBLIC p5z_lim_alloc     ! called in trcini_pisces.F90

   !! * Shared module variables
   REAL(wp), PUBLIC ::  concpno3    !:  NO3 half saturation for picophyto  
   REAL(wp), PUBLIC ::  concpnh4    !:  NH4 half saturation for picophyto
   REAL(wp), PUBLIC ::  concnpo4    !:  PO4 half saturation for nanophyto
   REAL(wp), PUBLIC ::  concppo4    !:  PO4 half saturation for picophyto
   REAL(wp), PUBLIC ::  concdpo4    !:  PO4 half saturation for diatoms
   REAL(wp), PUBLIC ::  concpfer    !:  Iron half saturation for picophyto
   REAL(wp), PUBLIC ::  concbpo4    !:  PO4 half saturation for bacteria
   REAL(wp), PUBLIC ::  xsizepic    !:  Minimum size criteria for picophyto
   REAL(wp), PUBLIC ::  xsizerp     !:  Size ratio for picophytoplankton
   REAL(wp), PUBLIC ::  qfnopt      !:  optimal Fe quota for nanophyto
   REAL(wp), PUBLIC ::  qfpopt      !:  optimal Fe quota for picophyto
   REAL(wp), PUBLIC ::  qfdopt      !:  optimal Fe quota for diatoms
   REAL(wp), PUBLIC ::  qnnmin      !:  minimum N  quota for nanophyto
   REAL(wp), PUBLIC ::  qnnmax      !:  maximum N quota for nanophyto
   REAL(wp), PUBLIC ::  qpnmin      !:  minimum P quota for nanophyto
   REAL(wp), PUBLIC ::  qpnmax      !:  maximum P quota for nanophyto
   REAL(wp), PUBLIC ::  qnpmin      !:  minimum N quota for nanophyto
   REAL(wp), PUBLIC ::  qnpmax      !:  maximum N quota for nanophyto
   REAL(wp), PUBLIC ::  qppmin      !:  minimum P quota for nanophyto
   REAL(wp), PUBLIC ::  qppmax      !:  maximum P quota for nanophyto
   REAL(wp), PUBLIC ::  qndmin      !:  minimum N quota for diatoms
   REAL(wp), PUBLIC ::  qndmax      !:  maximum N quota for diatoms
   REAL(wp), PUBLIC ::  qpdmin      !:  minimum P quota for diatoms
   REAL(wp), PUBLIC ::  qpdmax      !:  maximum P quota for diatoms
   REAL(wp), PUBLIC ::  qfnmax      !:  maximum Fe quota for nanophyto
   REAL(wp), PUBLIC ::  qfpmax      !:  maximum Fe quota for picophyto
   REAL(wp), PUBLIC ::  qfdmax      !:  maximum Fe quota for diatoms
   REAL(wp), PUBLIC ::  xpsinh4     !:  respiration cost of NH4 assimilation
   REAL(wp), PUBLIC ::  xpsino3     !:  respiration cost of NO3 assimilation
   REAL(wp), PUBLIC ::  xpsiuptk    !:  Mean respiration cost

   !!*  Allometric variations of the quotas
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqnnmin    !: Minimum N quota of nanophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqnnmax    !: Maximum N quota of nanophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqpnmin    !: Minimum P quota of nanophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqpnmax    !: Maximum P quota of picophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqnpmin    !: Minimum N quota of picophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqnpmax    !: Maximum N quota of picophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqppmin    !: Minimum P quota of picophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqppmax    !: Maximum P quota of picophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqndmin    !: Minimum N quota of diatoms
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqndmax    !: Maximum N quota of diatoms
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqpdmin    !: Minimum P quota of diatoms
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqpdmax    !: Maximum P quota of diatoms

   !!* Phytoplankton nutrient limitation terms
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xpicono3   !: Limitation of NO3 uptake by picophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xpiconh4   !: Limitation of NH4 uptake by picophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xpicopo4   !: Limitation of PO4 uptake by picophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xnanodop   !: Limitation of DOP uptake by nanophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xpicodop   !: Limitation of DOP uptake by picophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xdiatdop   !: Limitation of DOP uptake by diatoms
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xpicofer   !: Limitation of Fe uptake by picophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimpic    !: Limitation of picophyto PP by nutrients
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimpics   !: Limitation of picophyto PP by nutrients
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimphys   !: Limitation of nanophyto PP by nutrients
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimdias   !: Limitation of diatoms PP by nutrients
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimpfe    !: Limitation of picophyto PP by Fe
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   fvnuptk    !: Maximum potential uptake rate of nanophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   fvpuptk    !: Maximum potential uptake rate of picophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   fvduptk    !: Maximum potential uptake rate of diatoms
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xqfuncfecp !: 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimnpn, xlimnpp, xlimnpd

   ! Coefficient for iron limitation following Flynn and Hipkin (1999)
   REAL(wp) ::  xcoef1   = 0.00167  / 55.85
   REAL(wp) ::  xcoef2   = 1.21E-5 * 14. / 55.85 / 7.625 * 0.5 * 1.5
   REAL(wp) ::  xcoef3   = 1.15E-4 * 14. / 55.85 / 7.625 * 0.5 
   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p5zlim.F90 10070 2018-08-28 14:30:54Z nicolasmartin $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE p5z_lim( kt, knt, Kbb, Kmm )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p5z_lim  ***
      !!
      !! ** Purpose :   Compute the co-limitations by the various nutrients
      !!                for the various phytoplankton species. Quota based
      !!                approach. The quota model is derived from theoretical
      !!                models proposed by Pahlow and Oschlies (2009) and 
      !!                Flynn (2001). Various adaptations from several 
      !!                publications by these authors have been also adopted. 
      !!
      !! ** Method  : Quota based approach. The quota model is derived from 
      !!              theoretical models by Pahlow and Oschlies (2009) and 
      !!              Flynn (2001). Various adaptations from several publications
      !!              by these authors have been also adopted.
      !!---------------------------------------------------------------------
      !
      INTEGER, INTENT(in)  :: kt, knt
      INTEGER, INTENT(in)  :: Kbb, Kmm  ! time level indices
      !
      INTEGER  ::   ji, jj, jk
      REAL(wp) ::   zlim1, zlim2, zlim3, zlim4, zno3, zferlim
      REAL(wp) ::   z1_trndia, z1_trnpic, z1_trnphy, ztem1, ztem2, zetot1
      REAL(wp) ::   zratio, zration, zratiof, znutlim, zfalim, zxpsiuptk
      REAL(wp) ::   zconc1d, zconc1dnh4, zconc0n, zconc0nnh4, zconc0npo4, zconc0dpo4
      REAL(wp) ::   zconc0p, zconc0pnh4, zconc0ppo4, zconcpfe, zconcnfe, zconcdfe
      REAL(wp) ::   fanano, fananop, fananof, fadiat, fadiatp, fadiatf
      REAL(wp) ::   fapico, fapicop, fapicof, zlimpo4, zlimdop
      REAL(wp) ::   zrpho, zrass, zcoef, zfuptk, zratchl, ztrn, ztrp
      REAL(wp) ::   zfvn, zfvp, zfvf, zsizen, zsizep, zsized, znanochl, zpicochl, zdiatchl
      REAL(wp) ::   zqfemn, zqfemp, zqfemd, zbactno3, zbactnh4, zbiron
      REAL(wp) ::   znutlimtot, zlimno3, zlimnh4, zlim1f, zsizetmp
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zrassn, zrassp, zrassd
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p5z_lim')
      !
      zratchl = 6.0
      sizena(:,:,:) = 0.0  ;  sizepa(:,:,:) = 0.0  ;  sizeda(:,:,:) = 0.0
      !
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
         ! Computation of the Chl/C ratio of each phytoplankton group
         ! -------------------------------------------------------
         z1_trnphy   = 1. / ( tr(ji,jj,jk,jpphy,Kbb) + rtrn )
         z1_trnpic   = 1. / ( tr(ji,jj,jk,jppic,Kbb) + rtrn )
         z1_trndia   = 1. / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn )
         znanochl = tr(ji,jj,jk,jpnch,Kbb) * z1_trnphy
         zpicochl = tr(ji,jj,jk,jppch,Kbb) * z1_trnpic
         zdiatchl = tr(ji,jj,jk,jpdch,Kbb) * z1_trndia

         ! Computation of a variable Ks for the different phytoplankton
         ! group as a function of their relative size. Allometry
         ! from Edwards et al. (2012)
         !------------------------------------------------

         ! diatoms
         zsized            = sized(ji,jj,jk)**0.81
         zconcdfe          = concdfer * zsized
         zconc1d           = concdno3 * zsized
         zconc1dnh4        = concdnh4 * zsized
         zconc0dpo4        = concdpo4 * zsized

         ! picophytoplankton
         zsizep            = sizep(ji,jj,jk)**0.81
         zconcpfe          = concpfer * zsizep
         zconc0p           = concpno3 * zsizep
         zconc0pnh4        = concpnh4 * zsizep
         zconc0ppo4        = concppo4 * zsizep

         ! nanophytoplankton
         zsizen            = sizen(ji,jj,jk)**0.81
         zconcnfe          = concnfer * zsizen
         zconc0n           = concnno3 * zsizen
         zconc0nnh4        = concnnh4 * zsizen
         zconc0npo4        = concnpo4 * zsizen

         ! Allometric variations of the minimum and maximum quotas
         ! From Talmy et al. (2014) and Maranon et al. (2013)
         ! -------------------------------------------------------
         xqnnmin(ji,jj,jk) = qnnmin * sizen(ji,jj,jk)**(-0.36)
         xqnnmax(ji,jj,jk) = qnnmax
         xqndmin(ji,jj,jk) = qndmin * sized(ji,jj,jk)**(-0.36)
         xqndmax(ji,jj,jk) = qndmax
         xqnpmin(ji,jj,jk) = qnpmin * sizep(ji,jj,jk)**(-0.36)
         xqnpmax(ji,jj,jk) = qnpmax

         ! Computation of the optimal allocation parameters
         ! Based on the different papers by Pahlow et al., and Smith et al.
         ! -----------------------------------------------------------------
         zbiron = ( 75.0 * ( 1.0 - plig(ji,jj,jk) ) + plig(ji,jj,jk) ) * biron(ji,jj,jk)
               
         ! Nanophytoplankton
         znutlim = MAX( tr(ji,jj,jk,jpnh4,Kbb) / zconc0nnh4,    &
           &         tr(ji,jj,jk,jpno3,Kbb) / zconc0n)
         fanano = MAX(0.01, MIN(0.99, 1. / ( SQRT(znutlim) + 1.) ) )
         znutlim = tr(ji,jj,jk,jppo4,Kbb) / zconc0npo4
         fananop = MAX(0.01, MIN(0.99, 1. / ( SQRT(znutlim) + 1.) ) )
         znutlim = zbiron / zconcnfe
         fananof = MAX(0.01, MIN(0.99, 1. / ( SQRT(znutlim) + 1.) ) )

         ! Picophytoplankton
         znutlim = MAX( tr(ji,jj,jk,jpnh4,Kbb) / zconc0pnh4,    &
           &         tr(ji,jj,jk,jpno3,Kbb) / zconc0p)
         fapico = MAX(0.01, MIN(0.99, 1. / ( SQRT(znutlim) + 1.) ) )
         znutlim = tr(ji,jj,jk,jppo4,Kbb) / zconc0ppo4
         fapicop = MAX(0.01, MIN(0.99, 1. / ( SQRT(znutlim) + 1.) ) )
         znutlim = zbiron / zconcpfe
         fapicof = MAX(0.01, MIN(0.99, 1. / ( SQRT(znutlim) + 1.) ) )

         ! Diatoms
         znutlim = MAX( tr(ji,jj,jk,jpnh4,Kbb) / zconc1dnh4,    &
           &         tr(ji,jj,jk,jpno3,Kbb) / zconc1d )
         fadiat = MAX(0.01, MIN(0.99, 1. / ( SQRT(znutlim) + 1.) ) )
         znutlim = tr(ji,jj,jk,jppo4,Kbb) / zconc0dpo4
         fadiatp = MAX(0.01, MIN(0.99, 1. / ( SQRT(znutlim) + 1.) ) )
         znutlim = zbiron / zconcdfe
         fadiatf = MAX(0.01, MIN(0.99, 1. / ( SQRT(znutlim) + 1.) ) )

         !
         ! Michaelis-Menten Limitation term by nutrients of
         !  heterotrophic bacteria
         ! -------------------------------------------------------------
         zlimnh4 = tr(ji,jj,jk,jpnh4,Kbb) / ( concbnh4 + tr(ji,jj,jk,jpnh4,Kbb) )
         zlimno3 = tr(ji,jj,jk,jpno3,Kbb) / ( concbno3 + tr(ji,jj,jk,jpno3,Kbb) )
         znutlimtot = ( tr(ji,jj,jk,jpnh4,Kbb) + tr(ji,jj,jk,jpno3,Kbb) )  &
             &      / ( concbno3 + tr(ji,jj,jk,jpnh4,Kbb) + tr(ji,jj,jk,jpno3,Kbb) )
         zbactnh4 = znutlimtot * 5.0 * zlimnh4 / ( zlimno3 + 5.0 * zlimnh4 + rtrn )
         zbactno3 = znutlimtot * zlimno3 / ( zlimno3 + 5.0 * zlimnh4 + rtrn )         
         !
         zlim1    = zbactno3 + zbactnh4
         zlim2    = tr(ji,jj,jk,jppo4,Kbb) / ( tr(ji,jj,jk,jppo4,Kbb) + concbpo4)
         zlim3    = biron(ji,jj,jk) / ( concbfe + biron(ji,jj,jk) )
         zlim4    = tr(ji,jj,jk,jpdoc,Kbb) / ( xkdoc   + tr(ji,jj,jk,jpdoc,Kbb) )

         ! Xlimbac is used for DOC solubilization whereas xlimbacl
         ! is used for all the other bacterial-dependent term
         ! -------------------------------------------------------
         xlimbacl(ji,jj,jk) = MIN( zlim1, zlim2, zlim3 )
         xlimbac (ji,jj,jk) = xlimbacl(ji,jj,jk) * zlim4
         !
         ! Michaelis-Menten Limitation term for nutrients Small flagellates
         ! -----------------------------------------------
         ztrn    = tr(ji,jj,jk,jpnh4,Kbb) + tr(ji,jj,jk,jpno3,Kbb)
         ztrp    = tr(ji,jj,jk,jppo4,Kbb) + tr(ji,jj,jk,jpdop,Kbb) 
         !
         ! Limitation of N based nutrients uptake (NO3 and NH4)
         zfalim  = (1.-fanano) / fanano
         zlimnh4 = tr(ji,jj,jk,jpnh4,Kbb) / ( zconc0n + tr(ji,jj,jk,jpnh4,Kbb) )
         zlimno3 = tr(ji,jj,jk,jpno3,Kbb) / ( zconc0n + tr(ji,jj,jk,jpno3,Kbb) )
         znutlimtot = (1. - fanano) * ztrn  / ( zfalim * zconc0n + ztrn )
         xnanonh4(ji,jj,jk) = znutlimtot * 5.0 * zlimnh4 / ( zlimno3 + 5.0 * zlimnh4 + rtrn )
         xnanono3(ji,jj,jk) = znutlimtot * zlimno3 / ( zlimno3 + 5.0 * zlimnh4 + rtrn )
         !
         ! Limitation of P based nutrients (PO4 and DOP)
         zfalim  = (1.-fananop) / fananop
         zlimpo4 = tr(ji,jj,jk,jppo4,Kbb) / ( tr(ji,jj,jk,jppo4,Kbb) + zconc0npo4 )
         zlimdop = tr(ji,jj,jk,jpdop,Kbb) / ( tr(ji,jj,jk,jpdop,Kbb) + zconc0npo4 )
         znutlimtot = (1. - fananop) * ztrp / ( zfalim * zconc0npo4 + ztrp )
         xnanopo4(ji,jj,jk) = znutlimtot * 100.0 * zlimpo4 / ( zlimdop + 100.0 * zlimpo4 + rtrn )
         xnanodop(ji,jj,jk) = znutlimtot * zlimdop / ( zlimdop + 100.0 * zlimpo4 + rtrn )
         !
         ! Limitation of Fe uptake
         zfalim = (1.-fananof) / fananof
         xnanofer(ji,jj,jk) = (1. - fananof) * zbiron / ( zbiron + zfalim * zconcnfe )
         !
         ! The minimum iron quota depends on the size of PSU, respiration
         ! and the reduction of nitrate following the parameterization 
         ! proposed by Flynn and Hipkin (1999)
         zratiof   = tr(ji,jj,jk,jpnfe,Kbb) * z1_trnphy
         zqfemn = xcoef1 * znanochl + xcoef2 + xcoef3 * xnanono3(ji,jj,jk)
         xqfuncfecn(ji,jj,jk) = zqfemn + qfnopt
         !
         zration = tr(ji,jj,jk,jpnph,Kbb) * z1_trnphy
         zration = MIN(xqnnmax(ji,jj,jk), MAX( xqnnmin(ji,jj,jk), zration ))
         fvnuptk(ji,jj,jk) = 2.5 * xpsiuptk * xqnnmin(ji,jj,jk) / (zration + rtrn)  &
         &                   * MAX(0., (1. - zratchl * znanochl / 12. ) )
         !
         zlim1  = max(0., (zration - xqnnmin(ji,jj,jk) )  &
         &          / (xqnnmax(ji,jj,jk) - xqnnmin(ji,jj,jk) ) ) * xqnnmax(ji,jj,jk)  &
         &          / (zration + rtrn)
         ! The value of the optimal quota in the formulation below
         ! has been found by solving a non linear equation
         zlim1f = max(0., ( 1.13 - xqnnmin(ji,jj,jk) )  &
         &          / (xqnnmax(ji,jj,jk) - xqnnmin(ji,jj,jk) ) ) * xqnnmax(ji,jj,jk)
         zlim3  = MAX( 0.,( zratiof - zqfemn ) / qfnopt )
         ! computation of the various limitation terms of nanophyto
         ! growth and PP
         xlimnfe (ji,jj,jk) = MIN( 1., zlim3 )
         xlimphy (ji,jj,jk) = MIN( 1., zlim1, zlim3 )
         xlimphys(ji,jj,jk) = MIN( 1., zlim1/( zlim1f + rtrn ), zlim3 )
         xlimnpn (ji,jj,jk) = MIN( 1., zlim1)
         !
         ! Michaelis-Menten Limitation term for nutrients picophytoplankton
         ! ----------------------------------------------------------------
         ! Limitation of N based nutrients uptake (NO3 and NH4) 
         zfalim = (1.-fapico) / fapico 
         zlimnh4 = tr(ji,jj,jk,jpnh4,Kbb) / ( zconc0p + tr(ji,jj,jk,jpnh4,Kbb) )
         zlimno3 = tr(ji,jj,jk,jpno3,Kbb) / ( zconc0p + tr(ji,jj,jk,jpno3,Kbb) )
         znutlimtot = (1. - fapico) * ztrn / ( zfalim * zconc0p + ztrn )
         xpiconh4(ji,jj,jk) = znutlimtot * 5.0 * zlimnh4 / ( zlimno3 + 5.0 * zlimnh4 + rtrn )
         xpicono3(ji,jj,jk) = znutlimtot * zlimno3 / ( zlimno3 + 5.0 * zlimnh4 + rtrn )
         !
         ! Limitation of P based nutrients uptake (PO4 and DOP)
         zfalim = (1.-fapicop) / fapicop 
         zlimpo4 = tr(ji,jj,jk,jppo4,Kbb) / ( tr(ji,jj,jk,jppo4,Kbb) + zconc0ppo4 )
         zlimdop = tr(ji,jj,jk,jpdop,Kbb) / ( tr(ji,jj,jk,jpdop,Kbb) + zconc0ppo4 )
         znutlimtot = (1. - fapicop) * ztrp / ( zfalim * zconc0ppo4 + ztrp)
         xpicopo4(ji,jj,jk) = znutlimtot * 100.0 * zlimpo4 / ( zlimdop + 100.0 * zlimpo4 + rtrn )
         xpicodop(ji,jj,jk) = znutlimtot * zlimdop / ( zlimdop + 100.0 * zlimpo4 + rtrn )
         !
         zfalim = (1.-fapicof) / fapicof
         xpicofer(ji,jj,jk) = (1. - fapicof) * zbiron / ( zbiron + zfalim * zconcpfe )
         !
         ! The minimum iron quota depends on the size of PSU, respiration
         ! and the reduction of nitrate following the parameterization 
         ! proposed by Flynn and Hipkin (1999)
         zratiof = tr(ji,jj,jk,jppfe,Kbb) * z1_trnpic
         zqfemp = xcoef1 * zpicochl + xcoef2 + xcoef3 * xpicono3(ji,jj,jk)
         xqfuncfecp(ji,jj,jk) = zqfemp + qfpopt
         !
         zration   = tr(ji,jj,jk,jpnpi,Kbb) * z1_trnpic
         zration = MIN(xqnpmax(ji,jj,jk), MAX( xqnpmin(ji,jj,jk), zration ))
         fvpuptk(ji,jj,jk) = 2.5 * xpsiuptk * xqnpmin(ji,jj,jk) / (zration + rtrn)  &
         &                   * MAX(0., (1. - zratchl * zpicochl / 12. ) ) 
         !
         zlim1    = max(0., (zration - xqnpmin(ji,jj,jk) )  &
         &          / (xqnpmax(ji,jj,jk) - xqnpmin(ji,jj,jk) ) ) * xqnpmax(ji,jj,jk)  &
         &          / (zration + rtrn)
         ! The value of the optimal quota in the formulation below
         ! has been found by solving a non linear equation
         zlim1f   = max(0., (1.29 - xqnpmin(ji,jj,jk) )  &
         &          / (xqnpmax(ji,jj,jk) - xqnpmin(ji,jj,jk) ) ) * xqnpmax(ji,jj,jk)
         zlim3    = MAX( 0.,( zratiof - zqfemp ) / qfpopt )

         ! computation of the various limitation terms of picophyto
         ! growth and PP
         xlimpfe (ji,jj,jk) = MIN( 1., zlim3 )
         xlimpic (ji,jj,jk) = MIN( 1., zlim1, zlim3 )
         xlimnpp (ji,jj,jk) = MIN( 1., zlim1 )
         xlimpics(ji,jj,jk) = MIN( 1., zlim1/( zlim1f + rtrn ), zlim3 )


         !
         !   Michaelis-Menten Limitation term for nutrients Diatoms
         !   ------------------------------------------------------
         !
         ! Limitation of N based nutrients uptake (NO3 and NH4)
         zfalim = (1.-fadiat) / fadiat 
         zlimnh4 = tr(ji,jj,jk,jpnh4,Kbb) / ( zconc1d + tr(ji,jj,jk,jpnh4,Kbb) )
         zlimno3 = tr(ji,jj,jk,jpno3,Kbb) / ( zconc1d + tr(ji,jj,jk,jpno3,Kbb) )
         znutlimtot = (1.0 - fadiat) * ztrn / ( zfalim * zconc1d + ztrn )
         xdiatnh4(ji,jj,jk) = znutlimtot * 5.0 * zlimnh4 / ( zlimno3 + 5.0 * zlimnh4 + rtrn )
         xdiatno3(ji,jj,jk) = znutlimtot * zlimno3 / ( zlimno3 + 5.0 * zlimnh4 + rtrn )
         !
         ! Limitation of P based nutrients uptake (PO4 and DOP)
         zfalim = (1.-fadiatp) / fadiatp
         zlimpo4 = tr(ji,jj,jk,jppo4,Kbb) / ( tr(ji,jj,jk,jppo4,Kbb) + zconc0dpo4 )
         zlimdop = tr(ji,jj,jk,jpdop,Kbb) / ( tr(ji,jj,jk,jpdop,Kbb) + zconc0dpo4 )
         znutlimtot = (1. - fadiatp) * ztrp / ( zfalim * zconc0dpo4 + ztrp )
         xdiatpo4(ji,jj,jk) = znutlimtot * 100.0 * zlimpo4 / ( zlimdop + 100.0 * zlimpo4 + rtrn )
         xdiatdop(ji,jj,jk) = znutlimtot * zlimdop / ( zlimdop + 100.0 * zlimpo4 + rtrn )
         !
         ! Limitation of Fe uptake
         zfalim = (1.-fadiatf) / fadiatf
         xdiatfer(ji,jj,jk) = (1. - fadiatf) * zbiron / ( zbiron + zfalim * zconcdfe )
         !
         ! The minimum iron quota depends on the size of PSU, respiration
         ! and the reduction of nitrate following the parameterization 
         ! proposed by Flynn and Hipkin (1999)
         zratiof   = tr(ji,jj,jk,jpdfe,Kbb) * z1_trndia
         zqfemd = xcoef1 * zdiatchl + xcoef2 + xcoef3 * xdiatno3(ji,jj,jk)
         xqfuncfecd(ji,jj,jk) = zqfemd + qfdopt
         !
         zration   = tr(ji,jj,jk,jpndi,Kbb) * z1_trndia
         zration   = MIN(xqndmax(ji,jj,jk), MAX( xqndmin(ji,jj,jk), zration ))
         fvduptk(ji,jj,jk) = 2.5 * xpsiuptk * xqndmin(ji,jj,jk) / (zration + rtrn)   &
         &                   * MAX(0., (1. - zratchl * zdiatchl / 12. ) ) 
         !
         zlim1    = max(0., (zration - xqndmin(ji,jj,jk) )    &
         &          / (xqndmax(ji,jj,jk) - xqndmin(ji,jj,jk) ) )   &
         &          * xqndmax(ji,jj,jk) / (zration + rtrn)
         ! The value of the optimal quota in the formulation below
         ! has been found by solving a non linear equation
         zlim1f   = max(0., (1.13 - xqndmin(ji,jj,jk) )    &
         &          / (xqndmax(ji,jj,jk) - xqndmin(ji,jj,jk) ) )   &
         &          * xqndmax(ji,jj,jk)
         zlim3    = tr(ji,jj,jk,jpsil,Kbb) / ( tr(ji,jj,jk,jpsil,Kbb) + xksi(ji,jj) )
         zlim4    = MAX( 0., ( zratiof - zqfemd ) / qfdopt )
         ! computation of the various limitation terms of diatoms
         ! growth and PP
         xlimdfe(ji,jj,jk) = MIN( 1., zlim4 )
         xlimdia(ji,jj,jk) = MIN( 1., zlim1, zlim3, zlim4 )
         xlimdias(ji,jj,jk) = MIN (1.0, zlim1 / (zlim1f + rtrn ), zlim3, zlim4 )
         xlimsi(ji,jj,jk)  = MIN( zlim1, zlim4 )
         xlimnpd(ji,jj,jk) = MIN( 1., zlim1 )
      END_3D

      !
      ! Compute the phosphorus quota values. It is based on Litchmann et al., 2004 and Daines et al, 2013.
      ! The relative contribution of three fonctional pools are computed: light harvesting apparatus, 
      ! nutrient uptake pool and assembly machinery. DNA is assumed to represent 1% of the dry mass of 
      ! phytoplankton (see Daines et al., 2013). 
      ! --------------------------------------------------------------------------------------------------
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
         ! Size estimation of nanophytoplankton based on total biomass
         ! Assumes that larger biomass implies addition of larger cells
         ! ------------------------------------------------------------
         zcoef = tr(ji,jj,jk,jpphy,Kbb) - MIN(xsizephy, tr(ji,jj,jk,jpphy,Kbb) )
         sizena(ji,jj,jk) = 1. + ( xsizern -1.0 ) * zcoef / ( xsizephy + zcoef )
         ! N/P ratio of nanophytoplankton
         ! ------------------------------
         zfuptk = 0.2 + 0.12 / ( 3.0 * sizen(ji,jj,jk) + rtrn )
         ! Computed from Inomura et al. (2020) using Pavlova Lutheri
         zrpho  = 11.55 * tr(ji,jj,jk,jpnch,Kbb) / ( tr(ji,jj,jk,jpphy,Kbb) * 12. + rtrn )
         zrass = MAX(0.62/4., ( 1. - zrpho - zfuptk ) * xlimnpn(ji,jj,jk) )
         zrassn(ji,jj,jk) = zrass
         xqpnmin(ji,jj,jk) = ( 0.0 + 0.0078 + 0.62/4. * 0.0783 ) * 16.
         xqpnmax(ji,jj,jk) = ( zrpho * 0.0089 + zrass * 0.0783 ) * 16.
         xqpnmax(ji,jj,jk) = xqpnmax(ji,jj,jk) + (0.033 + 0.0078 ) * 16.
         xqpnmax(ji,jj,jk) = MIN( qpnmax, xqpnmax(ji,jj,jk) )

         ! Size estimation of picophytoplankton based on total biomass
         ! Assumes that larger biomass implies addition of larger cells
         ! ------------------------------------------------------------
         zcoef = tr(ji,jj,jk,jppic,Kbb) - MIN(xsizepic, tr(ji,jj,jk,jppic,Kbb) )
         sizepa(ji,jj,jk) = 1. + ( xsizerp -1.0 ) * zcoef / ( xsizepic + zcoef )

         ! N/P ratio of picophytoplankton
         ! ------------------------------
         zfuptk = 0.2 + 0.12 / ( 0.8 * sizep(ji,jj,jk) + rtrn )
         ! Computed from Inomura et al. (2020) using a synechococcus
         zrpho = 13.4 * tr(ji,jj,jk,jppch,Kbb) / ( tr(ji,jj,jk,jppic,Kbb) * 12. + rtrn )
         zrass = MAX(0.4/4., ( 1. - zrpho - zfuptk ) * xlimnpp(ji,jj,jk) )
         zrassp(ji,jj,jk) = zrass
         xqppmin(ji,jj,jk) = ( (0.0 + 0.0078 ) + 0.4/4. * 0.0517 ) * 16.
         xqppmax(ji,jj,jk) = ( zrpho * 0.0076 + zrass * 0.0517 ) * 16.
         xqppmax(ji,jj,jk) = xqppmax(ji,jj,jk) +  (0.033 + 0.0078 ) * 16
         xqppmax(ji,jj,jk) = MIN( qppmax, xqppmax(ji,jj,jk) )

         ! Size estimation of diatoms based on total biomass
         ! Assumes that larger biomass implies addition of larger cells
         ! ------------------------------------------------------------
         zcoef = tr(ji,jj,jk,jpdia,Kbb) - MIN(xsizedia, tr(ji,jj,jk,jpdia,Kbb) )
         sizeda(ji,jj,jk) = 1. + ( xsizerd - 1.0 ) * zcoef / ( xsizedia + zcoef )
         ! N/P ratio of diatoms
         ! --------------------
         zfuptk = 0.2 + 0.12 / ( 5.0 * sized(ji,jj,jk) + rtrn )
         ! Computed from Inomura et al. (2020) using a synechococcus
         zrpho = 8.08 * tr(ji,jj,jk,jpdch,Kbb) / ( tr(ji,jj,jk,jpndi,Kbb) * 12. + rtrn )
         zrass = MAX(0.66/4., ( 1. - zrpho - zfuptk ) * xlimnpd(ji,jj,jk) )
         zrassd(ji,jj,jk)=zrass
         xqpdmin(ji,jj,jk) = ( ( 0.0 + 0.0078 ) + 0.66/4. * 0.0783 ) * 16.
         xqpdmax(ji,jj,jk) = ( zrpho * 0.0135 + zrass * 0.0783 ) * 16.
         xqpdmax(ji,jj,jk) = xqpdmax(ji,jj,jk) + ( 0.0078 + 0.033 ) * 16.
         xqpdmax(ji,jj,jk) = MIN(qpdmax, xqpdmax(ji,jj,jk) )
      END_3D

      ! Compute the fraction of nanophytoplankton that is made of calcifiers
      ! This is a purely adhoc formulation described in Aumont et al. (2015)
      ! This fraction depends on nutrient limitation, light, temperature
      ! --------------------------------------------------------------------
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
         zlim1 =  tr(ji,jj,jk,jpnh4,Kbb) / ( tr(ji,jj,jk,jpnh4,Kbb) + concnnh4 ) + tr(ji,jj,jk,jpno3,Kbb)    &
         &        / ( tr(ji,jj,jk,jpno3,Kbb) + concnno3 ) * ( 1.0 - tr(ji,jj,jk,jpnh4,Kbb)   &
         &        / ( tr(ji,jj,jk,jpnh4,Kbb) + concnnh4 ) )
         zlim2  = tr(ji,jj,jk,jppo4,Kbb) / ( tr(ji,jj,jk,jppo4,Kbb) + concnpo4 )
         zlim3  = tr(ji,jj,jk,jpfer,Kbb) / ( tr(ji,jj,jk,jpfer,Kbb) +  6.E-11 ) 
         ztem1  = MAX( 0., ts(ji,jj,jk,jp_tem,Kmm) + 1.8 )
         ztem2  = ts(ji,jj,jk,jp_tem,Kmm) - 10.
         zetot1 = MAX( 0., etot_ndcy(ji,jj,jk) - 1.) / ( 4. + etot_ndcy(ji,jj,jk) ) * 30. / ( 30. + etot_ndcy(ji,jj,jk) ) 

         xfracal(ji,jj,jk) = caco3r * xlimphy(ji,jj,jk)     &
         &                   * ztem1 / ( 0.1 + ztem1 ) * MAX( 1., tr(ji,jj,jk,jpphy,Kbb)*1E6 )   &
            &                * ( 1. + EXP(-ztem2 * ztem2 / 25. ) )         &
            &                * zetot1 * MIN( 1., 50. / ( hmld(ji,jj) + rtrn ) )
         xfracal(ji,jj,jk) = MAX( 0.02, MIN( 0.8 , xfracal(ji,jj,jk) ) )
      END_3D
      !
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
         ! denitrification factor computed from O2 levels
         nitrfac(ji,jj,jk) = MAX(  0.e0, 0.4 * ( 6.e-6  - tr(ji,jj,jk,jpoxy,Kbb) )    &
            &                                / ( oxymin + tr(ji,jj,jk,jpoxy,Kbb) )  )
         nitrfac(ji,jj,jk) = MIN( 1., nitrfac(ji,jj,jk) )
      END_3D
      !
      IF( lk_iomput .AND. knt == nrdttrc ) THEN        ! save output diagnostics
        CALL iom_put( "xfracal", xfracal(:,:,:) * tmask(:,:,:) )  ! euphotic layer deptht
        CALL iom_put( "LNnut"  , xlimphy(:,:,:) * tmask(:,:,:) )  ! Nutrient limitation term
        CALL iom_put( "LPnut"  , xlimpic(:,:,:) * tmask(:,:,:) )  ! Nutrient limitation term
        CALL iom_put( "LDnut"  , xlimdia(:,:,:) * tmask(:,:,:) )  ! Nutrient limitation term
        CALL iom_put( "LNFe"   , xlimnfe(:,:,:) * tmask(:,:,:) )  ! Iron limitation term
        CALL iom_put( "LPFe"   , xlimpfe(:,:,:) * tmask(:,:,:) )  ! Iron limitation term
        CALL iom_put( "LDFe"   , xlimdfe(:,:,:) * tmask(:,:,:) )  ! Iron limitation term
        CALL iom_put( "SIZEN"  , sizen  (:,:,:) * tmask(:,:,:) )  ! Iron limitation term
        CALL iom_put( "SIZEP"  , sizep  (:,:,:) * tmask(:,:,:) )  ! Iron limitation term
        CALL iom_put( "SIZED"  , sized  (:,:,:) * tmask(:,:,:) )  ! Iron limitation term
        CALL iom_put( "RASSN"  , zrassn (:,:,:) * tmask(:,:,:) )  ! Iron limitation term
        CALL iom_put( "RASSP"  , zrassp (:,:,:) * tmask(:,:,:) )  ! Iron limitation term
        CALL iom_put( "RASSD"  , zrassd (:,:,:) * tmask(:,:,:) )  ! Iron limitation term
      ENDIF
      !
      IF( ln_timing )  CALL timing_stop('p5z_lim')
      !
   END SUBROUTINE p5z_lim


   SUBROUTINE p5z_lim_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p5z_lim_init  ***
      !!
      !! ** Purpose :   Initialization of nutrient limitation parameters
      !!
      !! ** Method  :   Read the namp5zlim and nampisquota namelists and check
      !!      the parameters called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist namp5zlim
      !!
      !!----------------------------------------------------------------------
      INTEGER :: ios                 ! Local integer output status for namelist read
      !!
      NAMELIST/namp5zlim/ concnno3, concpno3, concdno3, concnnh4, concpnh4, concdnh4,  &
         &                concnfer, concpfer, concdfer, concbfe, concnpo4, concppo4,   &
         &                concdpo4, concbno3, concbnh4, concbpo4, xsizedia, xsizepic,  &
         &                xsizephy, xsizern, xsizerp, xsizerd, xksi1, xksi2, xkdoc,    &
         &                caco3r, oxymin
         !
      NAMELIST/namp5zquota/ qnnmin, qnnmax, qpnmin, qpnmax, qnpmin, qnpmax, qppmin,      &
         &                  qppmax, qndmin, qndmax, qpdmin, qpdmax, qfnmax, qfpmax, qfdmax,  &
         &                  qfnopt, qfpopt, qfdopt
      !!----------------------------------------------------------------------
      !
      READ  ( numnatp_ref, namp5zlim, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namp5zlim in reference namelist' )
      !
      READ  ( numnatp_cfg, namp5zlim, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 ) CALL ctl_nam ( ios , 'namp5zlim in configuration namelist' )
      IF(lwm) WRITE ( numonp, namp5zlim )
      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' '
         WRITE(numout,*) ' Namelist parameters for nutrient limitations, namp5zlim'
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '    mean rainratio                           caco3r    = ', caco3r
         WRITE(numout,*) '    NO3 half saturation of nanophyto         concnno3  = ', concnno3
         WRITE(numout,*) '    NO3 half saturation of picophyto         concpno3  = ', concpno3
         WRITE(numout,*) '    NO3 half saturation of diatoms           concdno3  = ', concdno3
         WRITE(numout,*) '    NH4 half saturation for phyto            concnnh4  = ', concnnh4
         WRITE(numout,*) '    NH4 half saturation for pico             concpnh4  = ', concpnh4
         WRITE(numout,*) '    NH4 half saturation for diatoms          concdnh4  = ', concdnh4
         WRITE(numout,*) '    PO4 half saturation for phyto            concnpo4  = ', concnpo4
         WRITE(numout,*) '    PO4 half saturation for pico             concppo4  = ', concppo4
         WRITE(numout,*) '    PO4 half saturation for diatoms          concdpo4  = ', concdpo4
         WRITE(numout,*) '    half saturation constant for Si uptake   xksi1     = ', xksi1
         WRITE(numout,*) '    half saturation constant for Si/C        xksi2     = ', xksi2
         WRITE(numout,*) '    half-sat. of DOC remineralization        xkdoc     = ', xkdoc
         WRITE(numout,*) '    Iron half saturation for nanophyto       concnfer  = ', concnfer
         WRITE(numout,*) '    Iron half saturation for picophyto       concpfer  = ', concpfer
         WRITE(numout,*) '    Iron half saturation for diatoms         concdfer  = ', concdfer
         WRITE(numout,*) '    size ratio for nanophytoplankton         xsizern   = ', xsizern
         WRITE(numout,*) '    size ratio for picophytoplankton         xsizerp   = ', xsizerp
         WRITE(numout,*) '    size ratio for diatoms                   xsizerd   = ', xsizerd
         WRITE(numout,*) '    NO3 half saturation of bacteria          concbno3  = ', concbno3
         WRITE(numout,*) '    NH4 half saturation for bacteria         concbnh4  = ', concbnh4
         WRITE(numout,*) '    Minimum size criteria for diatoms        xsizedia  = ', xsizedia
         WRITE(numout,*) '    Minimum size criteria for picophyto      xsizepic  = ', xsizepic
         WRITE(numout,*) '    Minimum size criteria for nanophyto      xsizephy  = ', xsizephy
         WRITE(numout,*) '    Fe half saturation for bacteria          concbfe   = ', concbfe
         WRITE(numout,*) '    halk saturation constant for anoxia       oxymin   =' , oxymin
      ENDIF

      READ  ( numnatp_ref, namp5zquota, IOSTAT = ios, ERR = 903)
903   IF( ios /= 0 ) CALL ctl_nam ( ios , 'nampisquota in reference namelist' )
      !
      READ  ( numnatp_cfg, namp5zquota, IOSTAT = ios, ERR = 904 )
904   IF( ios >  0 ) CALL ctl_nam ( ios , 'nampisquota in configuration namelist' )
      IF(lwm) WRITE ( numonp, namp5zquota )
      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' '
         WRITE(numout,*) ' Namelist parameters for nutrient limitations, namp5zquota'
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '    optimal Fe quota for nano.               qfnopt    = ', qfnopt
         WRITE(numout,*) '    optimal Fe quota for pico.               qfpopt    = ', qfpopt
         WRITE(numout,*) '    Optimal Fe quota for diatoms             qfdopt    = ', qfdopt
         WRITE(numout,*) '    Minimal N quota for nano                 qnnmin    = ', qnnmin
         WRITE(numout,*) '    Maximal N quota for nano                 qnnmax    = ', qnnmax
         WRITE(numout,*) '    Minimal P quota for nano                 qpnmin    = ', qpnmin
         WRITE(numout,*) '    Maximal P quota for nano                 qpnmax    = ', qpnmax
         WRITE(numout,*) '    Minimal N quota for pico                 qnpmin    = ', qnpmin
         WRITE(numout,*) '    Maximal N quota for pico                 qnpmax    = ', qnpmax
         WRITE(numout,*) '    Minimal P quota for pico                 qppmin    = ', qppmin
         WRITE(numout,*) '    Maximal P quota for pico                 qppmax    = ', qppmax
         WRITE(numout,*) '    Minimal N quota for diatoms              qndmin    = ', qndmin
         WRITE(numout,*) '    Maximal N quota for diatoms              qndmax    = ', qndmax
         WRITE(numout,*) '    Minimal P quota for diatoms              qpdmin    = ', qpdmin
         WRITE(numout,*) '    Maximal P quota for diatoms              qpdmax    = ', qpdmax
         WRITE(numout,*) '    Maximal Fe quota for nanophyto.          qfnmax    = ', qfnmax
         WRITE(numout,*) '    Maximal Fe quota for picophyto.          qfpmax    = ', qfpmax
         WRITE(numout,*) '    Maximal Fe quota for diatoms             qfdmax    = ', qfdmax
      ENDIF
      !
      ! Metabolic cost of nitrate and ammonium utilisation
      xpsino3  = 2.3 * rno3
      xpsinh4  = 1.8 * rno3
      xpsiuptk = 1.0 / 6.625
      !
      nitrfac(:,:,jpk) = 0._wp
      xfracal(:,:,jpk) = 0._wp
      xlimphy(:,:,jpk) = 0._wp
      xlimpic(:,:,jpk) = 0._wp
      xlimdia(:,:,jpk) = 0._wp
      xlimnfe(:,:,jpk) = 0._wp
      xlimpfe(:,:,jpk) = 0._wp
      xlimdfe(:,:,jpk) = 0._wp
      sizen  (:,:,jpk) = 0._wp
      sizep  (:,:,jpk) = 0._wp
      sized  (:,:,jpk) = 0._wp
      !
   END SUBROUTINE p5z_lim_init


   INTEGER FUNCTION p5z_lim_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p5z_lim_alloc  ***
      !!----------------------------------------------------------------------
      USE lib_mpp , ONLY: ctl_stop
      INTEGER ::   ierr(2)        ! Local variables
      !!----------------------------------------------------------------------
      ierr(:) = 0
      !
      !*  Biological arrays for phytoplankton growth
      ALLOCATE( xpicono3(jpi,jpj,jpk), xpiconh4(jpi,jpj,jpk),       &
         &      xpicopo4(jpi,jpj,jpk), xpicodop(jpi,jpj,jpk),       &
         &      xnanodop(jpi,jpj,jpk), xdiatdop(jpi,jpj,jpk),       &
         &      xpicofer(jpi,jpj,jpk), xlimpfe (jpi,jpj,jpk),       &
         &      fvnuptk (jpi,jpj,jpk), fvduptk (jpi,jpj,jpk),       &
         &      xlimphys(jpi,jpj,jpk), xlimdias(jpi,jpj,jpk),       &
         &      xlimnpp (jpi,jpj,jpk), xlimnpn (jpi,jpj,jpk),       &
         &      xlimnpd (jpi,jpj,jpk),                              &
         &      xlimpics(jpi,jpj,jpk), xqfuncfecp(jpi,jpj,jpk),     &
         &      fvpuptk (jpi,jpj,jpk), xlimpic (jpi,jpj,jpk),    STAT=ierr(1) )
         !
      !*  Minimum/maximum quotas of phytoplankton
      ALLOCATE( xqnnmin (jpi,jpj,jpk), xqnnmax(jpi,jpj,jpk),       &
         &      xqpnmin (jpi,jpj,jpk), xqpnmax(jpi,jpj,jpk),       &
         &      xqnpmin (jpi,jpj,jpk), xqnpmax(jpi,jpj,jpk),       &
         &      xqppmin (jpi,jpj,jpk), xqppmax(jpi,jpj,jpk),       &
         &      xqndmin (jpi,jpj,jpk), xqndmax(jpi,jpj,jpk),       &
         &      xqpdmin (jpi,jpj,jpk), xqpdmax(jpi,jpj,jpk),     STAT=ierr(2) )
         !
      p5z_lim_alloc = MAXVAL( ierr )
      !
      IF( p5z_lim_alloc /= 0 ) CALL ctl_stop( 'STOP', 'p5z_lim_alloc : failed to allocate arrays.' )
      !
   END FUNCTION p5z_lim_alloc
   !!======================================================================
END MODULE p5zlim
