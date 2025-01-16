MODULE p4zlim
   !!======================================================================
   !!                         ***  MODULE p4zlim  ***
   !! TOP :   Computes the nutrient limitation terms of phytoplankton
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-04  (O. Aumont, C. Ethe) Limitation for iron modelled in quota 
   !!----------------------------------------------------------------------
   !!   p4z_lim        :   Compute the nutrients limitation terms 
   !!   p4z_lim_init   :   Read the namelist 
   !!----------------------------------------------------------------------
   USE oce_trc         ! Shared ocean-passive tracers variables
   USE trc             ! Tracers defined
   USE sms_pisces      ! PISCES variables
   USE iom             ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC p4z_lim           ! called in p4zbio.F90 
   PUBLIC p4z_lim_init      ! called in trcsms_pisces.F90 
   PUBLIC p4z_lim_alloc     ! called in trcini_pisces.F90

   !! * Shared module variables
   REAL(wp), PUBLIC ::  concnno3    !:  NO3, PO4 half saturation   
   REAL(wp), PUBLIC ::  concdno3    !:  Phosphate half saturation for diatoms  
   REAL(wp), PUBLIC ::  concnnh4    !:  NH4 half saturation for nanophyto  
   REAL(wp), PUBLIC ::  concdnh4    !:  NH4 half saturation for diatoms
   REAL(wp), PUBLIC ::  concnfer    !:  Iron half saturation for nanophyto 
   REAL(wp), PUBLIC ::  concdfer    !:  Iron half saturation for diatoms  
   REAL(wp), PUBLIC ::  concbno3    !:  NO3 half saturation  for bacteria 
   REAL(wp), PUBLIC ::  concbnh4    !:  NH4 half saturation for bacteria
   REAL(wp), PUBLIC ::  xsizedia    !:  Minimum size criteria for diatoms
   REAL(wp), PUBLIC ::  xsizephy    !:  Minimum size criteria for nanophyto
   REAL(wp), PUBLIC ::  xsizern     !:  Size ratio for nanophytoplankton
   REAL(wp), PUBLIC ::  xsizerd     !:  Size ratio for diatoms
   REAL(wp), PUBLIC ::  xksi1       !:  half saturation constant for Si uptake 
   REAL(wp), PUBLIC ::  xksi2       !:  half saturation constant for Si/C 
   REAL(wp), PUBLIC ::  xkdoc       !:  2nd half-sat. of DOC remineralization  
   REAL(wp), PUBLIC ::  concbfe     !:  Fe half saturation for bacteria 
   REAL(wp), PUBLIC ::  qnfelim     !:  optimal Fe quota for nanophyto
   REAL(wp), PUBLIC ::  qdfelim     !:  optimal Fe quota for diatoms
   REAL(wp), PUBLIC ::  caco3r      !:  mean rainratio 

   !!* Phytoplankton limitation terms
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xnanono3   !: Nanophyto limitation by NO3
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xdiatno3   !: Diatoms limitation by NO3
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xnanonh4   !: Nanophyto limitation by NH4
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xdiatnh4   !:  Diatoms limitation by NH4
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xnanopo4   !: Nanophyto limitation by PO4
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xdiatpo4   !: Diatoms limitation by PO4
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimphy    !: Nutrient limitation term of nanophytoplankton
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimdia    !: Nutrient limitation term of diatoms
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimnfe    !: Nanophyto limitation by Iron
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimdfe    !: Diatoms limitation by iron
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimsi     !: Diatoms limitation by Si
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimbac    !: Bacterial limitation term
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimbacl   !: Bacterial limitation term
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   concdfe    !: Limitation of diatoms uptake of Fe
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   concnfe    !: Limitation of Nano uptake of Fe
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xnanofer   !: Limitation of Fe uptake by nanophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xdiatfer   !: Limitation of Fe uptake by diatoms
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xqfuncfecd, xqfuncfecn

   ! Coefficient for iron limitation following Flynn and Hipkin (1999)
   REAL(wp) ::  xcoef1   = 0.0016  / 55.85  
   REAL(wp) ::  xcoef2   = 1.21E-5 * 14. / 55.85 / 7.3125 * 0.5 * 1.5
   REAL(wp) ::  xcoef3   = 1.15E-4 * 14. / 55.85 / 7.3125 * 0.5 

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p4zlim.F90 10069 2018-08-28 14:12:24Z nicolasmartin $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_lim( kt, knt, Kbb, Kmm )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_lim  ***
      !!
      !! ** Purpose :   Compute the co-limitations by the various nutrients
      !!                for the various phytoplankton species
      !!
      !! ** Method  : - Limitation follows the Liebieg law of the minimum
      !!              - Monod approach for N, P and Si. Quota approach 
      !!                for Iron
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in)  :: kt, knt
      INTEGER, INTENT(in)  :: Kbb, Kmm      ! time level indices
      !
      INTEGER  ::   ji, jj, jk
      REAL(wp) ::   zlim1, zlim2, zlim3, zlim4, zcoef
      REAL(wp) ::   z1_trbdia, z1_trbphy, ztem1, ztem2, zetot1, zetot2
      REAL(wp) ::   zdenom, zratio, zironmin, zbactno3, zbactnh4
      REAL(wp) ::   zconc1d, zconc1dnh4, zconc0n, zconc0nnh4   
      REAL(wp) ::   fananof, fadiatf, znutlim, zfalim
      REAL(wp) ::   znutlimtot, zlimno3, zlimnh4, zbiron
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p4z_lim')
      !
      sizena(:,:,:) = 1.0  ;  sizeda(:,:,:) = 1.0
      !
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
         
         ! Computation of a variable Ks for iron on diatoms taking into account
         ! that increasing biomass is made of generally bigger cells
         ! The allometric relationship is classical.
         !------------------------------------------------
         z1_trbphy   = 1. / ( tr(ji,jj,jk,jpphy,Kbb) + rtrn )
         z1_trbdia   = 1. / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn )

         concnfe(ji,jj,jk) = concnfer * sizen(ji,jj,jk)**0.81
         zconc0n           = concnno3 * sizen(ji,jj,jk)**0.81
         zconc0nnh4        = concnnh4 * sizen(ji,jj,jk)**0.81

         concdfe(ji,jj,jk) = concdfer * sized(ji,jj,jk)**0.81 
         zconc1d           = concdno3 * sized(ji,jj,jk)**0.81 
         zconc1dnh4        = concdnh4 * sized(ji,jj,jk)**0.81  

          ! Computation of the optimal allocation parameters
          ! Based on the different papers by Pahlow et al., and 
          ! Smith et al.
          ! ---------------------------------------------------

          ! Nanophytoplankton
          zbiron = ( 75.0 * ( 1.0 - plig(ji,jj,jk) ) + plig(ji,jj,jk) ) * biron(ji,jj,jk)
          znutlim = zbiron / concnfe(ji,jj,jk)
          fananof = MAX(0.01, MIN(0.99, 1. / ( SQRT(znutlim) + 1.) ) )

          ! Diatoms
          znutlim = zbiron / concdfe(ji,jj,jk)
          fadiatf = MAX(0.01, MIN(0.99, 1. / ( SQRT(znutlim) + 1.) ) )

          ! Michaelis-Menten Limitation term by nutrients of
          ! heterotrophic bacteria
          ! -------------------------------------------------
          zlimnh4 = tr(ji,jj,jk,jpnh4,Kbb) / ( concbno3 + tr(ji,jj,jk,jpnh4,Kbb) )
          zlimno3 = tr(ji,jj,jk,jpno3,Kbb) / ( concbno3 + tr(ji,jj,jk,jpno3,Kbb) )
          znutlimtot = ( tr(ji,jj,jk,jpnh4,Kbb) + tr(ji,jj,jk,jpno3,Kbb) ) / ( concbno3 + tr(ji,jj,jk,jpnh4,Kbb) + tr(ji,jj,jk,jpno3,Kbb) )
          zbactnh4 = znutlimtot * 5.0 * zlimnh4 / ( zlimno3 + 5.0 * zlimnh4 + rtrn )
          zbactno3 = znutlimtot * zlimno3 / ( zlimno3 + 5.0 * zlimnh4 + rtrn )
          !
          zlim1    = zbactno3 + zbactnh4
          zlim2    = tr(ji,jj,jk,jppo4,Kbb) / ( tr(ji,jj,jk,jppo4,Kbb) + concbnh4 )
          zlim3    = tr(ji,jj,jk,jpfer,Kbb) / ( concbfe + tr(ji,jj,jk,jpfer,Kbb) )
          zlim4    = tr(ji,jj,jk,jpdoc,Kbb) / ( xkdoc   + tr(ji,jj,jk,jpdoc,Kbb) )
          ! Xlimbac is used for DOC solubilization whereas xlimbacl
          ! is used for all the other bacterial-dependent terms
          ! -------------------------------------------------------
          xlimbacl(ji,jj,jk) = MIN( zlim1, zlim2, zlim3 )
          xlimbac (ji,jj,jk) = MIN( zlim1, zlim2, zlim3 ) * zlim4

          ! Michaelis-Menten Limitation term by nutrients: Nanophyto
          ! Optimal parameterization by Smith and Pahlow series of 
          ! papers is used. Optimal allocation is supposed independant
          ! for all nutrients. 
          ! --------------------------------------------------------

          ! Limitation of Fe uptake (Quota formalism)
          zfalim = (1.-fananof) / fananof
          xnanofer(ji,jj,jk) = (1. - fananof) * zbiron / ( zbiron + zfalim * concnfe(ji,jj,jk) )

          ! Limitation of nanophytoplankton growth
          zlimnh4 = tr(ji,jj,jk,jpnh4,Kbb) / ( zconc0n + tr(ji,jj,jk,jpnh4,Kbb) )
          zlimno3 = tr(ji,jj,jk,jpno3,Kbb) / ( zconc0n + tr(ji,jj,jk,jpno3,Kbb) )
          znutlimtot = ( tr(ji,jj,jk,jpnh4,Kbb) + tr(ji,jj,jk,jpno3,Kbb) ) / ( zconc0n + tr(ji,jj,jk,jpnh4,Kbb) + tr(ji,jj,jk,jpno3,Kbb) )
          xnanonh4(ji,jj,jk) = znutlimtot * 5.0 * zlimnh4 / ( zlimno3 + 5.0 * zlimnh4 + rtrn )
          xnanono3(ji,jj,jk) = znutlimtot * zlimno3 / ( zlimno3 + 5.0 * zlimnh4 + rtrn )
          !
          zlim1    = xnanono3(ji,jj,jk) + xnanonh4(ji,jj,jk)
          zlim2    = tr(ji,jj,jk,jppo4,Kbb) / ( tr(ji,jj,jk,jppo4,Kbb) + zconc0nnh4 )
          zratio   = tr(ji,jj,jk,jpnfe,Kbb) * z1_trbphy 

          ! The minimum iron quota depends on the size of PSU, respiration
          ! and the reduction of nitrate following the parameterization 
          ! proposed by Flynn and Hipkin (1999)
          zironmin = xcoef1 * tr(ji,jj,jk,jpnch,Kbb) * z1_trbphy + xcoef2 * zlim1 + xcoef3 * xnanono3(ji,jj,jk)
          xqfuncfecn(ji,jj,jk) = zironmin + qnfelim
          zlim3    = MAX( 0.,( zratio - zironmin ) / qnfelim )
          xnanopo4(ji,jj,jk) = zlim2
          xlimnfe (ji,jj,jk) = MIN( 1., zlim3 )
          xlimphy (ji,jj,jk) = MIN( zlim1, zlim2, zlim3 )
               
          !   Michaelis-Menten Limitation term by nutrients : Diatoms
          !   -------------------------------------------------------
          ! Limitation of Fe uptake (Quota formalism)
          zfalim = (1.-fadiatf) / fadiatf
          xdiatfer(ji,jj,jk) = (1. - fadiatf) * zbiron / ( zbiron + zfalim * concdfe(ji,jj,jk) )

          ! Limitation of diatoms growth
          zlimnh4 = tr(ji,jj,jk,jpnh4,Kbb) / ( zconc1d + tr(ji,jj,jk,jpnh4,Kbb) )
          zlimno3 = tr(ji,jj,jk,jpno3,Kbb) / ( zconc1d + tr(ji,jj,jk,jpno3,Kbb) )
          znutlimtot = ( tr(ji,jj,jk,jpnh4,Kbb) + tr(ji,jj,jk,jpno3,Kbb) ) / ( zconc1d + tr(ji,jj,jk,jpnh4,Kbb) + tr(ji,jj,jk,jpno3,Kbb) )
          xdiatnh4(ji,jj,jk) = znutlimtot * 5.0 * zlimnh4 / ( zlimno3 + 5.0 * zlimnh4 + rtrn ) 
          xdiatno3(ji,jj,jk) = znutlimtot * zlimno3 / ( zlimno3 + 5.0 * zlimnh4 + rtrn )
          !
          zlim1    = xdiatno3(ji,jj,jk) + xdiatnh4(ji,jj,jk)
          zlim2    = tr(ji,jj,jk,jppo4,Kbb) / ( tr(ji,jj,jk,jppo4,Kbb) + zconc1dnh4  )
          zlim3    = tr(ji,jj,jk,jpsil,Kbb) / ( tr(ji,jj,jk,jpsil,Kbb) + xksi(ji,jj) + rtrn )
          zratio   = tr(ji,jj,jk,jpdfe,Kbb) * z1_trbdia

          ! The minimum iron quota depends on the size of PSU, respiration
          ! and the reduction of nitrate following the parameterization 
          ! proposed by Flynn and Hipkin (1999)
          zironmin = xcoef1 * tr(ji,jj,jk,jpdch,Kbb) * z1_trbdia + xcoef2 * zlim1 + xcoef3 * xdiatno3(ji,jj,jk)
          xqfuncfecd(ji,jj,jk) = zironmin + qdfelim
          zlim4    = MAX( 0., ( zratio - zironmin ) / qdfelim )
          xdiatpo4(ji,jj,jk) = zlim2
          xlimdfe (ji,jj,jk) = MIN( 1., zlim4 )
          xlimdia (ji,jj,jk) = MIN( zlim1, zlim2, zlim3, zlim4 )
          xlimsi  (ji,jj,jk) = MIN( zlim1, zlim2, zlim4 )
      END_3D


      ! Size estimation of phytoplankton based on total biomass
      ! Assumes that larger biomass implies addition of larger cells
      ! ------------------------------------------------------------
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
         zcoef = tr(ji,jj,jk,jpphy,Kbb) - MIN(xsizephy, tr(ji,jj,jk,jpphy,Kbb) )
         sizena(ji,jj,jk) = 1. + ( xsizern -1.0 ) * zcoef / ( xsizephy + zcoef )
         zcoef = tr(ji,jj,jk,jpdia,Kbb) - MIN(xsizedia, tr(ji,jj,jk,jpdia,Kbb) )
         sizeda(ji,jj,jk) = 1. + ( xsizerd - 1.0 ) * zcoef / ( xsizedia + zcoef )
      END_3D


      ! Compute the fraction of nanophytoplankton that is made of calcifiers
      ! This is a purely adhoc formulation described in Aumont et al. (2015)
      ! This fraction depends on nutrient limitation, light, temperature
      ! --------------------------------------------------------------------
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
         zlim1  = xnanonh4(ji,jj,jk) + xnanono3(ji,jj,jk) 
         zlim2  = tr(ji,jj,jk,jppo4,Kbb) / ( tr(ji,jj,jk,jppo4,Kbb) + concnnh4 )
         zlim3  = tr(ji,jj,jk,jpfer,Kbb) / ( tr(ji,jj,jk,jpfer,Kbb) +  6.E-11   )
         ztem1  = MAX( 0., ts(ji,jj,jk,jp_tem,Kmm) + 1.8)
         ztem2  = ts(ji,jj,jk,jp_tem,Kmm) - 10.
         zetot1 = MAX( 0., etot_ndcy(ji,jj,jk) - 1.) / ( 4. + etot_ndcy(ji,jj,jk) ) 
         zetot2 = 30. / ( 30.0 + etot_ndcy(ji,jj,jk) )

         xfracal(ji,jj,jk) = caco3r * MIN( zlim1, zlim2, zlim3 )                  &
            &                       * ztem1 / ( 0.1 + ztem1 )                     &
            &                       * MAX( 1., tr(ji,jj,jk,jpphy,Kbb) * 1.e6 / 2. )  &
            &                       * zetot1 * zetot2               &
            &                       * ( 1. + EXP(-ztem2 * ztem2 / 25. ) )         &
            &                       * MIN( 1., 50. / ( hmld(ji,jj) + rtrn ) )
         xfracal(ji,jj,jk) = MIN( 0.8 , xfracal(ji,jj,jk) )
         xfracal(ji,jj,jk) = MAX( 0.02, xfracal(ji,jj,jk) )
      END_3D
      !
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
         ! denitrification factor computed from O2 levels
         nitrfac(ji,jj,jk) = MAX(  0.e0, 0.4 * ( 6.e-6  - tr(ji,jj,jk,jpoxy,Kbb) )    &
            &                                / ( oxymin + tr(ji,jj,jk,jpoxy,Kbb) )  )
         nitrfac(ji,jj,jk) = MIN( 1., nitrfac(ji,jj,jk) )
         !
         ! denitrification factor computed from NO3 levels
         nitrfac2(ji,jj,jk) = MAX( 0.e0,       ( 1.E-6 - tr(ji,jj,jk,jpno3,Kbb) )  &
            &                                / ( 1.E-6 + tr(ji,jj,jk,jpno3,Kbb) ) )
         nitrfac2(ji,jj,jk) = MIN( 1., nitrfac2(ji,jj,jk) )
      END_3D
      !
      IF( lk_iomput .AND. knt == nrdttrc ) THEN        ! save output diagnostics
        CALL iom_put( "xfracal", xfracal(:,:,:) * tmask(:,:,:) )  ! euphotic layer deptht
        CALL iom_put( "LNnut"  , xlimphy(:,:,:) * tmask(:,:,:) )  ! Nutrient limitation term
        CALL iom_put( "LDnut"  , xlimdia(:,:,:) * tmask(:,:,:) )  ! Nutrient limitation term
        CALL iom_put( "LNFe"   , xlimnfe(:,:,:) * tmask(:,:,:) )  ! Iron limitation term
        CALL iom_put( "LDFe"   , xlimdfe(:,:,:) * tmask(:,:,:) )  ! Iron limitation term
        CALL iom_put( "SIZEN"  , sizen  (:,:,:) * tmask(:,:,:) )  ! Iron limitation term
        CALL iom_put( "SIZED"  , sized  (:,:,:) * tmask(:,:,:) )  ! Iron limitation term
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p4z_lim')
      !
   END SUBROUTINE p4z_lim


   SUBROUTINE p4z_lim_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_lim_init  ***
      !!
      !! ** Purpose :   Initialization of the nutrient limitation parameters
      !!
      !! ** Method  :   Read the namp4zlim namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist namp4zlim
      !!
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer

      ! Namelist block
      NAMELIST/namp4zlim/ concnno3, concdno3, concnnh4, concdnh4, concnfer, concdfer, concbfe,   &
         &                concbno3, concbnh4, xsizedia, xsizephy, xsizern, xsizerd,          & 
         &                xksi1, xksi2, xkdoc, qnfelim, qdfelim, caco3r, oxymin
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'p4z_lim_init : initialization of nutrient limitations'
         WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF
      !
      READ  ( numnatp_ref, namp4zlim, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namp4zlim in reference namelist' )
      READ  ( numnatp_cfg, namp4zlim, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namp4zlim in configuration namelist' )
      IF(lwm) WRITE( numonp, namp4zlim )

      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*) '   Namelist : namp4zlim'
         WRITE(numout,*) '      mean rainratio                           caco3r    = ', caco3r
         WRITE(numout,*) '      NO3 half saturation of nanophyto         concnno3  = ', concnno3
         WRITE(numout,*) '      NO3 half saturation of diatoms           concdno3  = ', concdno3
         WRITE(numout,*) '      NH4 half saturation for phyto            concnnh4  = ', concnnh4
         WRITE(numout,*) '      NH4 half saturation for diatoms          concdnh4  = ', concdnh4
         WRITE(numout,*) '      half saturation constant for Si uptake   xksi1     = ', xksi1
         WRITE(numout,*) '      half saturation constant for Si/C        xksi2     = ', xksi2
         WRITE(numout,*) '      half-sat. of DOC remineralization        xkdoc     = ', xkdoc
         WRITE(numout,*) '      Iron half saturation for nanophyto       concnfer  = ', concnfer
         WRITE(numout,*) '      Iron half saturation for diatoms         concdfer  = ', concdfer
         WRITE(numout,*) '      size ratio for nanophytoplankton         xsizern   = ', xsizern
         WRITE(numout,*) '      size ratio for diatoms                   xsizerd   = ', xsizerd
         WRITE(numout,*) '      NO3 half saturation of bacteria          concbno3  = ', concbno3
         WRITE(numout,*) '      NH4 half saturation for bacteria         concbnh4  = ', concbnh4
         WRITE(numout,*) '      Minimum size criteria for diatoms        xsizedia  = ', xsizedia
         WRITE(numout,*) '      Minimum size criteria for nanophyto      xsizephy  = ', xsizephy
         WRITE(numout,*) '      Fe half saturation for bacteria          concbfe   = ', concbfe
         WRITE(numout,*) '      halk saturation constant for anoxia       oxymin   =' , oxymin
         WRITE(numout,*) '      optimal Fe quota for nano.               qnfelim   = ', qnfelim
         WRITE(numout,*) '      Optimal Fe quota for diatoms             qdfelim   = ', qdfelim
      ENDIF
      !
      nitrfac (:,:,jpk) = 0._wp
      nitrfac2(:,:,jpk) = 0._wp
      xfracal (:,:,jpk) = 0._wp
      xlimphy (:,:,jpk) = 0._wp
      xlimdia (:,:,jpk) = 0._wp
      xlimnfe (:,:,jpk) = 0._wp
      xlimdfe (:,:,jpk) = 0._wp
      !
   END SUBROUTINE p4z_lim_init


   INTEGER FUNCTION p4z_lim_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p5z_lim_alloc  ***
      !! 
      !            Allocation of the arrays used in this module
      !!----------------------------------------------------------------------
      USE lib_mpp , ONLY: ctl_stop
      !!----------------------------------------------------------------------

      !*  Biological arrays for phytoplankton growth
      ALLOCATE( xnanono3(jpi,jpj,jpk), xdiatno3(jpi,jpj,jpk),       &
         &      xnanonh4(jpi,jpj,jpk), xdiatnh4(jpi,jpj,jpk),       &
         &      xnanopo4(jpi,jpj,jpk), xdiatpo4(jpi,jpj,jpk),       &
         &      xnanofer(jpi,jpj,jpk), xdiatfer(jpi,jpj,jpk),       &
         &      xlimphy (jpi,jpj,jpk), xlimdia (jpi,jpj,jpk),       &
         &      xlimnfe (jpi,jpj,jpk), xlimdfe (jpi,jpj,jpk),       &
         &      xlimbac (jpi,jpj,jpk), xlimbacl(jpi,jpj,jpk),       &
         &      concnfe (jpi,jpj,jpk), concdfe (jpi,jpj,jpk),       &
         &      xqfuncfecn(jpi,jpj,jpk), xqfuncfecd(jpi,jpj,jpk),   &
         &      xlimsi  (jpi,jpj,jpk), STAT=p4z_lim_alloc )
      !
      IF( p4z_lim_alloc /= 0 ) CALL ctl_stop( 'STOP', 'p4z_lim_alloc : failed to allocate arrays.' )
      !
   END FUNCTION p4z_lim_alloc

   !!======================================================================
END MODULE p4zlim
