MODULE p4zfechem
   !!======================================================================
   !!                         ***  MODULE p4zfechem  ***
   !! TOP :   PISCES Compute iron chemistry and scavenging
   !!======================================================================
   !! History :   3.5  !  2012-07 (O. Aumont, A. Tagliabue, C. Ethe) Original code
   !!             3.6  !  2015-05  (O. Aumont) PISCES quota
   !!----------------------------------------------------------------------
   !!   p4z_fechem       : Compute remineralization/scavenging of iron
   !!   p4z_fechem_init  : Initialisation of parameters for remineralisation
   !!   p4z_fechem_alloc : Allocate remineralisation variables
   !!----------------------------------------------------------------------
   USE oce_trc         ! shared variables between ocean and passive tracers
   USE trc             ! passive tracers common variables 
   USE sms_pisces      ! PISCES Source Minus Sink variables
   USE p4zche          ! chemical model
   USE p4zbc           ! Boundary conditions from sediments
   USE prtctl          ! print control for debugging
   USE iom             ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_fechem        ! called in p4zbio.F90
   PUBLIC   p4z_fechem_init   ! called in trcsms_pisces.F90

   LOGICAL          ::   ln_ligvar    !: boolean for variable ligand concentration following Tagliabue and voelker
   REAL(wp), PUBLIC ::   xlam1        !: scavenging rate of Iron 
   REAL(wp), PUBLIC ::   xlamdust     !: scavenging rate of Iron by dust 
   REAL(wp), PUBLIC ::   ligand       !: ligand concentration in the ocean 
   REAL(wp), PUBLIC ::   kfep         !: rate constant for nanoparticle formation
   REAL(wp), PUBLIC ::   scaveff      !: Fraction of scavenged iron that is considered as being subject to solubilization

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p4zfechem.F90 15459 2021-10-29 08:19:18Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_fechem( kt, knt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_fechem  ***
      !!
      !! ** Purpose :   Compute remineralization/scavenging of iron
      !!
      !! ** Method  :   A simple chemistry model of iron from Aumont and Bopp (2006)
      !!                based on one ligand and one inorganic form
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, knt   ! ocean time step
      INTEGER, INTENT(in) ::   Kbb, Kmm, Krhs  ! time level indices
      !
      INTEGER  ::   ji, jj, jk, jic, jn
      REAL(wp) ::   zlam1a, zlam1b
      REAL(wp) ::   zkeq, zfesatur, fe3sol, zligco
      REAL(wp) ::   zscave, zaggdfea, zaggdfeb, ztrc, zdust, zklight
      REAL(wp) ::   ztfe, zhplus, zxlam, zaggliga, zaggligb
      REAL(wp) ::   zprecip, zprecipno3,  zconsfe, za1
      REAL(wp) ::   zrfact2
      CHARACTER (len=25) :: charout
      REAL(wp), DIMENSION(jpi,jpj,jpk) ::   zTL1, zFe3, ztotlig, zfeprecip, zFeL1, zfecoll
      REAL(wp), DIMENSION(jpi,jpj,jpk) ::   zcoll3d, zscav3d, zlcoll3d
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p4z_fechem')
      !
      zFe3     (:,:,jpk) = 0.
      zFeL1    (:,:,jpk) = 0.
      zTL1     (:,:,jpk) = 0.
      zfeprecip(:,:,jpk) = 0.
      zcoll3d  (:,:,jpk) = 0.
      zscav3d  (:,:,jpk) = 0.
      zlcoll3d (:,:,jpk) = 0.
      zfecoll  (:,:,jpk) = 0.
      xfecolagg(:,:,jpk) = 0.
      xcoagfe  (:,:,jpk) = 0.
      !
      ! Total ligand concentration : Ligands can be chosen to be constant or variable
      ! Parameterization from Pham and Ito (2018)
      ! -------------------------------------------------
      xfecolagg(:,:,:) = ligand * 1E9 + MAX(0., chemo2(:,:,:) - tr(:,:,:,jpoxy,Kbb) ) / 400.E-6
      IF( ln_ligvar ) THEN
         ztotlig(:,:,:) =  0.09 * 0.667 * tr(:,:,:,jpdoc,Kbb) * 1E6 + xfecolagg(:,:,:)
         ztotlig(:,:,:) =  MIN( ztotlig(:,:,:), 10. )
      ELSE
        IF( ln_ligand ) THEN  ;   ztotlig(:,:,:) = tr(:,:,:,jplgw,Kbb) * 1E9
        ELSE                  ;   ztotlig(:,:,:) = ligand * 1E9 
        ENDIF
      ENDIF

      ! ------------------------------------------------------------
      !  from Aumont and Bopp (2006)
      ! This model is based on one ligand, Fe2+ and Fe3+ 
      ! Chemistry is supposed to be fast enough to be at equilibrium
      ! ------------------------------------------------------------
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
          zTL1(ji,jj,jk)  = ztotlig(ji,jj,jk)
          zkeq            = fekeq(ji,jj,jk)
          zklight         = 4.77E-7 * etot(ji,jj,jk) * 0.5 / ( 10**(-6.3) )
          zconsfe         = consfe3(ji,jj,jk) / ( 10**(-6.3) )
          zfesatur        = zTL1(ji,jj,jk) * 1E-9
          ztfe            = (1.0 + zklight) * tr(ji,jj,jk,jpfer,Kbb) 
          ! Fe' is the root of a 2nd order polynom
          za1 =  1. + zfesatur * zkeq + zklight +  zconsfe - zkeq * tr(ji,jj,jk,jpfer,Kbb)
          zFe3 (ji,jj,jk) = ( -1 * za1 + SQRT( za1**2 + 4. * ztfe * zkeq) ) / ( 2. * zkeq + rtrn )
          zFeL1(ji,jj,jk) = MAX( 0., tr(ji,jj,jk,jpfer,Kbb) - zFe3(ji,jj,jk) )
      END_3D
      !
      plig(:,:,:) =  MAX( 0., ( zFeL1(:,:,:) / ( tr(:,:,:,jpfer,Kbb) + rtrn ) ) )
      !
      zdust = 0.         ! if no dust available

      ! Computation of the colloidal fraction that is subjecto to coagulation
      ! The assumption is that 50% of complexed iron is colloidal. Furthermore
      ! The refractory part is supposed to be non sticky. The refractory
      ! fraction is supposed to equal to the background concentration + 
      ! the fraction that accumulates in the deep ocean. AOU is taken as a 
      ! proxy of that accumulation following numerous studies showing 
      ! some relationship between weak ligands and AOU.
      ! An issue with that parameterization is that when ligands are not
      ! prognostic or non variable, all the colloidal fraction is supposed
      ! to coagulate
      ! ----------------------------------------------------------------------
      IF (ln_ligand) THEN
         zfecoll(:,:,:) = 0.5 * zFeL1(:,:,:) * MAX(0., tr(:,:,:,jplgw,Kbb) - xfecolagg(:,:,:) * 1.0E-9 ) / ( tr(:,:,:,jplgw,Kbb) + rtrn ) 
      ELSE
         IF (ln_ligvar) THEN
            zfecoll(:,:,:) = 0.5 * zFeL1(:,:,:) * MAX(0., tr(:,:,:,jplgw,Kbb) - xfecolagg(:,:,:) * 1.0E-9 ) / ( tr(:,:,:,jplgw,Kbb) + rtrn )   
         ELSE
            zfecoll(:,:,:) = 0.5 * zFeL1(:,:,:)
         ENDIF
      ENDIF

      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
         ! Scavenging rate of iron. This scavenging rate depends on the load of particles of sea water. 
         ! This parameterization assumes a simple second order kinetics (k[Particles][Fe]).
         ! Scavenging onto dust is also included as evidenced from the DUNE experiments.
         ! --------------------------------------------------------------------------------------
         zhplus  = max( rtrn, hi(ji,jj,jk) )
         fe3sol  = fesol(ji,jj,jk,1) * ( zhplus**3 + fesol(ji,jj,jk,2) * zhplus**2  &
         &         + fesol(ji,jj,jk,3) * zhplus + fesol(ji,jj,jk,4)     &
         &         + fesol(ji,jj,jk,5) / zhplus )
         !
         ! precipitation of Fe3+, creation of nanoparticles
         zprecip = MAX( 0., ( zFe3(ji,jj,jk) - fe3sol ) ) * kfep * xstep * ( 1.0 - nitrfac(ji,jj,jk) ) 
         ! Precipitation of Fe2+ due to oxidation by NO3 (Croot et al., 2019)
         ! This occurs in anoxic waters only
         zprecipno3 = 2.0 * 130.0 * tr(ji,jj,jk,jpno3,Kbb) * nitrfac(ji,jj,jk) * xstep * zFe3(ji,jj,jk)
         !
         zfeprecip(ji,jj,jk) = zprecip + zprecipno3
         !
         ztrc   = ( tr(ji,jj,jk,jppoc,Kbb) + tr(ji,jj,jk,jpgoc,Kbb) + tr(ji,jj,jk,jpcal,Kbb) + tr(ji,jj,jk,jpgsi,Kbb) ) * 1.e6 
         ztrc = MAX( rtrn, ztrc )
         IF( ll_dust )  zdust  = dust(ji,jj) / ( wdust / rday ) * tmask(ji,jj,jk)
         zxlam  = MAX( 1.E-3, (1. - EXP(-2 * tr(ji,jj,jk,jpoxy,Kbb) / 100.E-6 ) ))
         zlam1b = 3.e-5 + ( xlamdust * zdust + xlam1 * ztrc ) * zxlam
         zscave = zFe3(ji,jj,jk) * zlam1b * xstep

         !  Compute the coagulation of colloidal iron. This parameterization 
         !  could be thought as an equivalent of colloidal pumping.
         !  It requires certainly some more work as it is very poorly constrained.
         !  ----------------------------------------------------------------
         zlam1a   = ( 12.0  * 0.3 * tr(ji,jj,jk,jpdoc,Kbb) + 9.05  * tr(ji,jj,jk,jppoc,Kbb) ) * xdiss(ji,jj,jk)    &
             &    + ( 2.49  * tr(ji,jj,jk,jppoc,Kbb) )     &
             &    + ( 127.8 * 0.3 * tr(ji,jj,jk,jpdoc,Kbb) + 725.7 * tr(ji,jj,jk,jppoc,Kbb) )
         zaggdfea = zlam1a * xstep * zfecoll(ji,jj,jk)
               !
         zlam1b   = ( 1.94 * xdiss(ji,jj,jk) + 1.37 ) * tr(ji,jj,jk,jpgoc,Kbb)
         zaggdfeb = zlam1b * xstep * zfecoll(ji,jj,jk)
         xcoagfe(ji,jj,jk) = zlam1a + zlam1b
         !
         tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) - zscave - zaggdfea - zaggdfeb &
         &                       - zfeprecip(ji,jj,jk)

         tr(ji,jj,jk,jpsfe,Krhs) = tr(ji,jj,jk,jpsfe,Krhs) + zscave * scaveff * tr(ji,jj,jk,jppoc,Kbb) / ztrc
         tr(ji,jj,jk,jpbfe,Krhs) = tr(ji,jj,jk,jpbfe,Krhs) + zscave * scaveff * tr(ji,jj,jk,jppoc,Kbb) / ztrc


          ! Precipitated iron is supposed to be permanently lost.
          ! Scavenged iron is supposed to be released back to seawater
          ! when POM is solubilized. This is highly uncertain as probably
          ! a significant part of it may be rescavenged back onto 
          ! the particles. An efficiency factor is applied that is read
          ! in the namelist. 
          ! See for instance Tagliabue et al. (2019).
          ! Aggregated FeL is considered as biogenic Fe as it 
          ! probably remains  complexed when the particle is solubilized.
          ! -------------------------------------------------------------
          tr(ji,jj,jk,jpsfe,Krhs) = tr(ji,jj,jk,jpsfe,Krhs) + zaggdfea
          tr(ji,jj,jk,jpbfe,Krhs) = tr(ji,jj,jk,jpbfe,Krhs) + zaggdfeb
          !
          zscav3d(ji,jj,jk)   = zscave 
          zcoll3d(ji,jj,jk)   = zaggdfea + zaggdfeb
         !
      END_3D
      !
      !  Define the bioavailable fraction of iron
      !  ----------------------------------------
      biron(:,:,:) = tr(:,:,:,jpfer,Kbb) 
      !
      !  Output of some diagnostics variables
      !     ---------------------------------
      IF( lk_iomput .AND. knt == nrdttrc ) THEN
         zrfact2 = 1.e3 * rfact2r  ! conversion from mol/L/timestep into mol/m3/s
         IF( iom_use("Fe3")    )  CALL iom_put("Fe3"    , zFe3   (:,:,:)       * tmask(:,:,:) )   ! Fe3+
         IF( iom_use("FeL1")   )  CALL iom_put("FeL1"   , zFeL1  (:,:,:)       * tmask(:,:,:) )   ! FeL1
         IF( iom_use("TL1")    )  CALL iom_put("TL1"    , zTL1   (:,:,:)       * tmask(:,:,:) )   ! TL1
         IF( iom_use("Totlig") )  CALL iom_put("Totlig" , ztotlig(:,:,:)       * tmask(:,:,:) )   ! TL
         IF( iom_use("Biron")  )  CALL iom_put("Biron"  , biron  (:,:,:)  * 1e9 * tmask(:,:,:) )   ! biron
         IF( iom_use("FESCAV") )  CALL iom_put("FESCAV" , zscav3d(:,:,:)  * 1e9 * tmask(:,:,:) * zrfact2 )
         IF( iom_use("FECOLL") )  CALL iom_put("FECOLL" , zcoll3d(:,:,:)  * 1e9 * tmask(:,:,:) * zrfact2 )
         IF( iom_use("FEPREC") )  CALL iom_put("FEPREC" , zfeprecip(:,:,:) *1e9*tmask(:,:,:)*zrfact2 )
      ENDIF

      IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('fechem')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p4z_fechem')
      !
   END SUBROUTINE p4z_fechem


   SUBROUTINE p4z_fechem_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_fechem_init  ***
      !!
      !! ** Purpose :   Initialization of iron chemistry parameters
      !!
      !! ** Method  :   Read the nampisfer namelist and check the parameters
      !!      called at the first timestep
      !!
      !! ** input   :   Namelist nampisfer
      !!
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer 
      !!
      NAMELIST/nampisfer/ ln_ligvar, xlam1, xlamdust, ligand, kfep, scaveff 
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'p4z_rem_init : Initialization of iron chemistry parameters'
         WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF
      !
      READ  ( numnatp_ref, nampisfer, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'nampisfer in reference namelist' )
      READ  ( numnatp_cfg, nampisfer, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'nampisfer in configuration namelist' )
      IF(lwm) WRITE( numonp, nampisfer )

      IF(lwp) THEN                     ! control print
         WRITE(numout,*) '   Namelist : nampisfer'
         WRITE(numout,*) '      variable concentration of ligand          ln_ligvar    =', ln_ligvar
         WRITE(numout,*) '      scavenging rate of Iron                   xlam1        =', xlam1
         WRITE(numout,*) '      scavenging rate of Iron by dust           xlamdust     =', xlamdust
         WRITE(numout,*) '      ligand concentration in the ocean         ligand       =', ligand
         WRITE(numout,*) '      rate constant for nanoparticle formation  kfep         =', kfep
         WRITE(numout,*) '      Scavenged iron that is added to POFe      scaveff      =', scaveff
      ENDIF
      !
   END SUBROUTINE p4z_fechem_init
   
   !!======================================================================
END MODULE p4zfechem
