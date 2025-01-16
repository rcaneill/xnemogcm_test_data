MODULE trcini_pisces
   !!======================================================================
   !!                         ***  MODULE trcini_pisces  ***
   !! TOP :   initialisation of the PISCES biochemical model
   !!         This module is for LOBSTER, PISCES and PISCES-QUOTA
   !!======================================================================
   !! History :    -   !  1988-07  (E. Maier-Reiner) Original code
   !!              -   !  1999-10  (O. Aumont, C. Le Quere)
   !!              -   !  2002     (O. Aumont)  PISCES
   !!             1.0  !  2005-03  (O. Aumont, A. El Moussaoui) F90
   !!             2.0  !  2007-12  (C. Ethe, G. Madec) from trcini.pisces.h90
   !!             3.5  !  2012-05  (C. Ethe) Merge PISCES-LOBSTER
   !!----------------------------------------------------------------------
   !! trc_ini_pisces   : PISCES biochemical model initialisation
   !!----------------------------------------------------------------------
   USE par_trc         !  TOP parameters
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE trcnam_pisces   !  PISCES namelist
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE sedini          !  SEDIMENTS initialization routine

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_ini_pisces   ! called by trcini.F90 module

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcini_pisces.F90 15459 2021-10-29 08:19:18Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_ini_pisces( Kmm )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trc_ini_pisces ***
      !!
      !! ** Purpose :   Initialisation of the PISCES biochemical model
      !!                Allocation of the dynamic arrays
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in)  ::  Kmm      ! time level indices
      !
      ! Read the PISCES namelist
      CALL trc_nam_pisces
      !
      IF( ln_p4z .OR. ln_p5z ) THEN  ;   CALL p4z_ini( Kmm )   !  PISCES
      ELSE                           ;   CALL p2z_ini( Kmm )   !  LOBSTER
      ENDIF

   END SUBROUTINE trc_ini_pisces


   SUBROUTINE p4z_ini( Kmm )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE p4z_ini ***
      !!
      !! ** Purpose :   Initialisation of the PISCES biochemical model
      !!----------------------------------------------------------------------
      USE p4zsms          !  Main P4Z routine
      USE p4zche          !  Chemical model
      USE p4zsink         !  vertical flux of particulate matter due to sinking
      USE p4zopt          !  optical model
      USE p4zbc          !  Boundary conditions
      USE p4zfechem       !  Iron chemistry
      USE p4zrem          !  Remineralisation of organic matter
      USE p4zflx          !  Gas exchange
      USE p4zlim          !  Co-limitations of differents nutrients
      USE p4zprod         !  Growth rate of the 2 phyto groups
      USE p4zmicro        !  Sources and sinks of microzooplankton
      USE p4zmeso         !  Sources and sinks of mesozooplankton
      USE p4zmort         !  Mortality terms for phytoplankton
      USE p4zlys          !  Calcite saturation
      USE p4zsed          !  Sedimentation & burial
      USE p4zpoc          !  Remineralization of organic particles
      USE p4zligand       !  Remineralization of organic ligands
      USE p5zlim          !  Co-limitations of differents nutrients (QUOTA)
      USE p5zprod         !  Growth rate of the 3 phyto groups (QUOTA)
      USE p5zmicro        !  Sources and sinks of microzooplankton (QUOTA)
      USE p5zmeso         !  Sources and sinks of mesozooplankton (QUOTA)
      USE p5zmort         !  Mortality terms for phytoplankton (QUOTA)
      !
      INTEGER, INTENT(in)  ::  Kmm      ! time level indices
      !
      INTEGER  ::  ji, jj, jk, jn, ierr
      REAL(wp) ::  zcaralk, zbicarb, zco3
      REAL(wp) ::  ztmas, ztmas1
      REAL(wp) ::  sco2, alka0, oxyg0, po4, bioma0, silic1, no3   
      CHARACTER(len = 20)  ::  cltra
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         IF( ln_p4z ) THEN 
            WRITE(numout,*) 'p4z_ini :   PISCES biochemical model initialisation'
            WRITE(numout,*) '~~~~~~~'
         ELSE
            WRITE(numout,*) 'p5z_ini :   PISCES biochemical model initialisation'
            WRITE(numout,*) '~~~~~~~     With variable stoichiometry'
         ENDIF
      ENDIF
      !
      sco2   =  2.312e-3_wp
      alka0  =  2.426e-3_wp
      oxyg0  =  177.6e-6_wp 
      po4    =  2.165e-6_wp 
      bioma0 =  1.000e-8_wp  
      silic1 =  91.51e-6_wp  
      no3    =  30.9e-6_wp * 7.3125_wp
      !
      ! Allocate PISCES arrays
      ierr =         sms_pisces_alloc()          
      ierr = ierr +  p4z_che_alloc()
      ierr = ierr +  p4z_sink_alloc()
      ierr = ierr +  p4z_opt_alloc()
      ierr = ierr +  p4z_flx_alloc()
      ierr = ierr +  p4z_sed_alloc()
      ierr = ierr +  p4z_lim_alloc()
      IF( ln_p4z ) THEN
         ! PISCES part
         ierr = ierr +  p4z_prod_alloc()
         ierr = ierr +  p4z_meso_alloc()
      ELSE
         ! PISCES-QUOTA part
         ierr = ierr +  p5z_lim_alloc()
         ierr = ierr +  p5z_prod_alloc()
         ierr = ierr +  p5z_meso_alloc()
      ENDIF
      ierr = ierr +  p4z_rem_alloc()
      !
      CALL mpp_sum( 'trcini_pisces', ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'pisces_alloc: unable to allocate PISCES arrays' )
      !
      ryyss    = nyear_len(1) * rday    ! number of seconds per year
      r1_ryyss = 1. / ryyss
      !

      ! assign an index in trc arrays for each prognostic variables
      ! This is based on the information read in the namelist_top
      DO jn = 1, jptra
        cltra = ctrcnm(jn) 
        IF( cltra == 'DIC'      )   jpdic = jn      !: dissolved inoganic carbon concentration 
        IF( cltra == 'Alkalini' )   jptal = jn      !: total alkalinity 
        IF( cltra == 'O2'       )   jpoxy = jn      !: oxygen carbon concentration 
        IF( cltra == 'CaCO3'    )   jpcal = jn      !: calcite  concentration 
        IF( cltra == 'PO4'      )   jppo4 = jn      !: phosphate concentration 
        IF( cltra == 'POC'      )   jppoc = jn      !: small particulate organic phosphate concentration
        IF( cltra == 'Si'       )   jpsil = jn      !: silicate concentration
        IF( cltra == 'PHY'      )   jpphy = jn      !: phytoplancton concentration 
        IF( cltra == 'ZOO'      )   jpzoo = jn      !: zooplancton concentration
        IF( cltra == 'DOC'      )   jpdoc = jn      !: dissolved organic carbon concentration 
        IF( cltra == 'PHY2'     )   jpdia = jn      !: Diatoms Concentration
        IF( cltra == 'ZOO2'     )   jpmes = jn      !: Mesozooplankton Concentration
        IF( cltra == 'DSi'      )   jpdsi = jn      !: Diatoms Silicate Concentration
        IF( cltra == 'Fer'      )   jpfer = jn      !: Iron Concentration
        IF( cltra == 'BFe'      )   jpbfe = jn      !: Big iron particles Concentration
        IF( cltra == 'GOC'      )   jpgoc = jn      !: Big particulate organic phosphate concentration
        IF( cltra == 'SFe'      )   jpsfe = jn      !: Small iron particles Concentration
        IF( cltra == 'DFe'      )   jpdfe = jn      !: Diatoms iron Concentration
        IF( cltra == 'GSi'      )   jpgsi = jn      !: (big) Silicate Concentration
        IF( cltra == 'NFe'      )   jpnfe = jn      !: Nano iron Concentration
        IF( cltra == 'NCHL'     )   jpnch = jn      !: Nano Chlorophyll Concentration
        IF( cltra == 'DCHL'     )   jpdch = jn      !: Diatoms Chlorophyll Concentration
        IF( cltra == 'NO3'      )   jpno3 = jn      !: Nitrates Concentration
        IF( cltra == 'NH4'      )   jpnh4 = jn      !: Ammonium Concentration
        IF( cltra == 'DON'      )   jpdon = jn      !: Dissolved organic N Concentration
        IF( cltra == 'DOP'      )   jpdop = jn      !: Dissolved organic P Concentration
        IF( cltra == 'PON'      )   jppon = jn      !: Small Nitrogen particle Concentration
        IF( cltra == 'POP'      )   jppop = jn      !: Small Phosphorus particle Concentration
        IF( cltra == 'GON'      )   jpgon = jn      !: Big Nitrogen particles Concentration
        IF( cltra == 'GOP'      )   jpgop = jn      !: Big Phosphorus Concentration
        IF( cltra == 'PHYN'     )   jpnph = jn      !: Nanophytoplankton N biomass
        IF( cltra == 'PHYP'     )   jppph = jn      !: Nanophytoplankton P biomass
        IF( cltra == 'DIAN'     )   jpndi = jn      !: Diatoms N biomass
        IF( cltra == 'DIAP'     )   jppdi = jn      !: Diatoms P biomass
        IF( cltra == 'PIC'      )   jppic = jn      !: Picophytoplankton C biomass
        IF( cltra == 'PICN'     )   jpnpi = jn      !: Picophytoplankton N biomass
        IF( cltra == 'PICP'     )   jpppi = jn      !: Picophytoplankton P biomass
        IF( cltra == 'PCHL'     )   jppch = jn      !: Diatoms Chlorophyll Concentration
        IF( cltra == 'PFe'      )   jppfe = jn      !: Picophytoplankton Fe biomass
        IF( cltra == 'LGW'      )   jplgw = jn      !: Weak ligands
      END DO

      CALL p4z_sms_init       !  Maint routine
      !

      ! Set biological ratios
      ! ---------------------
      rno3    =  16._wp / 117._wp   ! C/N
      po4r    =   1._wp / 117._wp   ! C/P
      o2nit   =  32._wp / 117._wp   ! O2/C for nitrification
      o2ut    = 138._wp / 117._wp   ! O2/C for ammonification
      rdenit  =  ( ( o2ut + o2nit ) * 0.80 - rno3 - rno3 * 0.60 ) / rno3  ! Denitrification
      rdenita =   3._wp /  5._wp    ! Denitrification
      IF( ln_p5z ) THEN
         no3rat3 = no3rat3 / rno3   ! C/N ratio in zooplankton
         po4rat3 = po4rat3 / po4r   ! C/P ratio in zooplankton
      ENDIF

      ! Initialization of tracer concentration in case of  no restart 
      !--------------------------------------------------------------
      IF( .NOT.ln_rsttr ) THEN  
         tr(:,:,:,jpdic,Kmm) = sco2
         tr(:,:,:,jpdoc,Kmm) = bioma0
         tr(:,:,:,jptal,Kmm) = alka0
         tr(:,:,:,jpoxy,Kmm) = oxyg0
         tr(:,:,:,jpcal,Kmm) = bioma0
         tr(:,:,:,jppo4,Kmm) = po4 / po4r
         tr(:,:,:,jppoc,Kmm) = bioma0
         tr(:,:,:,jpgoc,Kmm) = bioma0
         tr(:,:,:,jpbfe,Kmm) = bioma0 * 5.e-6
         tr(:,:,:,jpsil,Kmm) = silic1
         tr(:,:,:,jpdsi,Kmm) = bioma0 * 0.15
         tr(:,:,:,jpgsi,Kmm) = bioma0 * 5.e-6
         tr(:,:,:,jpphy,Kmm) = bioma0
         tr(:,:,:,jpdia,Kmm) = bioma0
         tr(:,:,:,jpzoo,Kmm) = bioma0
         tr(:,:,:,jpmes,Kmm) = bioma0
         tr(:,:,:,jpfer,Kmm) = 0.6E-9
         tr(:,:,:,jpsfe,Kmm) = bioma0 * 5.e-6
         tr(:,:,:,jpdfe,Kmm) = bioma0 * 5.e-6
         tr(:,:,:,jpnfe,Kmm) = bioma0 * 5.e-6
         tr(:,:,:,jpnch,Kmm) = bioma0 * 12. / 55.
         tr(:,:,:,jpdch,Kmm) = bioma0 * 12. / 55.
         tr(:,:,:,jpno3,Kmm) = no3
         tr(:,:,:,jpnh4,Kmm) = bioma0
         IF( ln_ligand) THEN
            tr(:,:,:,jplgw,Kmm) = 0.6E-9
         ENDIF
         IF( ln_p5z ) THEN
            tr(:,:,:,jpdon,Kmm) = bioma0
            tr(:,:,:,jpdop,Kmm) = bioma0
            tr(:,:,:,jppon,Kmm) = bioma0
            tr(:,:,:,jppop,Kmm) = bioma0
            tr(:,:,:,jpgon,Kmm) = bioma0
            tr(:,:,:,jpgop,Kmm) = bioma0
            tr(:,:,:,jpnph,Kmm) = bioma0
            tr(:,:,:,jppph,Kmm) = bioma0
            tr(:,:,:,jppic,Kmm) = bioma0
            tr(:,:,:,jpnpi,Kmm) = bioma0
            tr(:,:,:,jpppi,Kmm) = bioma0
            tr(:,:,:,jpndi,Kmm) = bioma0
            tr(:,:,:,jppdi,Kmm) = bioma0
            tr(:,:,:,jppfe,Kmm) = bioma0 * 5.e-6
            tr(:,:,:,jppch,Kmm) = bioma0 * 12. / 55.
         ENDIF
         ! initialize the half saturation constant for silicate
         ! ----------------------------------------------------
         xksi(:,:)      = 2.e-6
         xksimax(:,:)   = xksi(:,:)
         consfe3(:,:,:) = 0._wp
         !
         sized(:,:,:) = 1.0
         sizen(:,:,:) = 1.0
         IF( ln_p5z )  sizep(:,:,:) = 1.0
      END IF


      ! Initialization of the different PISCES modules
      ! Mainly corresponds to the namelist use
      ! ----------------------------------------------
      CALL p4z_sink_init         !  vertical flux of particulate organic matter
      CALL p4z_opt_init          !  Optic: PAR in the water column
      IF( ln_p4z ) THEN
         ! PISCES part
         CALL p4z_lim_init       !  co-limitations by the various nutrients
         CALL p4z_prod_init      !  phytoplankton growth rate over the global ocean.
      ELSE
         ! PISCES-QUOTA part
         CALL p5z_lim_init       !  co-limitations by the various nutrients
         CALL p5z_prod_init      !  phytoplankton growth rate over the global ocean.
      ENDIF
      CALL p4z_bc_init( Kmm )    !  boundary conditions
      CALL p4z_fechem_init       !  Iron chemistry
      CALL p4z_rem_init          !  remineralisation
      CALL p4z_poc_init          !  remineralisation of organic particles
      IF( ln_ligand ) &
         & CALL p4z_ligand_init  !  remineralisation of organic ligands

      IF( ln_p4z ) THEN ! PISCES-std
         CALL p4z_mort_init      !  phytoplankton mortality 
         CALL p4z_micro_init     !  microzooplankton
         CALL p4z_meso_init      !  mesozooplankton
      ELSE ! PISCES-QUOTA
         CALL p5z_mort_init      !  phytoplankton mortality 
         CALL p5z_micro_init     !  microzooplankton
         CALL p5z_meso_init      !  mesozooplankton
      ENDIF
      CALL p4z_lys_init          !  calcite saturation
      IF( .NOT.l_co2cpl ) &
        & CALL p4z_flx_init      !  gas exchange 

      ! Initialization of the sediment model
      IF( ln_sediment)   &
        & CALL sed_ini           ! Initialization of the sediment model 

      CALL p4z_sed_init          ! loss of organic matter in the sediments 

      IF(lwp) WRITE(numout,*) 
      IF(lwp) WRITE(numout,*) '   ==>>>   Initialization of PISCES tracers done'
      IF(lwp) WRITE(numout,*) 
      !
   END SUBROUTINE p4z_ini


   SUBROUTINE p2z_ini( Kmm )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE p2z_ini ***
      !!
      !! ** Purpose :   Initialisation of the LOBSTER biochemical model
      !!----------------------------------------------------------------------
      !
      USE p2zopt
      USE p2zexp
      USE p2zbio
      USE p2zsed
      !
      INTEGER, INTENT(in)  ::  Kmm      ! time level indices
      INTEGER  ::  ji, jj, jk, jn, ierr
      CHARACTER(len = 10)  ::  cltra
      !!----------------------------------------------------------------------

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) ' p2z_ini :   LOBSTER biochemical model initialisation'
      IF(lwp) WRITE(numout,*) ' ~~~~~~~'

      ierr =        sms_pisces_alloc()          
      ierr = ierr + p2z_exp_alloc()
      !
      CALL mpp_sum( 'trcini_pisces', ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'p2z_ini: unable to allocate LOBSTER arrays' )

      DO jn = 1, jptra
        cltra = ctrcnm(jn) 
        IF( cltra == 'DET' )   jpdet = jn       !: detritus                    [mmoleN/m3]
        IF( cltra == 'ZOO' )   jpzoo = jn       !: zooplancton concentration   [mmoleN/m3]
        IF( cltra == 'PHY' )   jpphy = jn       !: phytoplancton concentration [mmoleN/m3]
        IF( cltra == 'NO3' )   jpno3 = jn       !: nitrate concentration       [mmoleN/m3]
        IF( cltra == 'NH4' )   jpnh4 = jn       !: ammonium concentration      [mmoleN/m3]
        IF( cltra == 'DOM' )   jpdom = jn       !: dissolved organic matter    [mmoleN/m3]
      ENDDO

      jpkb = 10        !  last level where depth less than 200 m
      DO jk = jpkm1, 1, -1
         IF( gdept_1d(jk) > 200. ) jpkb = jk 
      END DO
      IF (lwp) WRITE(numout,*)
      IF (lwp) WRITE(numout,*) ' first vertical layers where biology is active (200m depth ) ', jpkb
      IF (lwp) WRITE(numout,*)
      jpkbm1 = jpkb - 1
      !


      ! LOBSTER initialisation for GYRE : init NO3=f(density) by asklod AS Kremeur 2005-07
      ! ----------------------
      IF( .NOT. ln_rsttr ) THEN             ! in case of  no restart 
         tr(:,:,:,jpdet,Kmm) = 0.1 * tmask(:,:,:)
         tr(:,:,:,jpzoo,Kmm) = 0.1 * tmask(:,:,:)
         tr(:,:,:,jpnh4,Kmm) = 0.1 * tmask(:,:,:)
         tr(:,:,:,jpphy,Kmm) = 0.1 * tmask(:,:,:)
         tr(:,:,:,jpdom,Kmm) = 1.0 * tmask(:,:,:)
         WHERE( rhd(:,:,:) <= 24.5e-3 )  ;  tr(:,:,:,jpno3,Kmm) = 2._wp * tmask(:,:,:)
         ELSE WHERE                      ;  tr(:,:,:,jpno3,Kmm) = ( 15.55 * ( rhd(:,:,:) * 1000. ) - 380.11 ) * tmask(:,:,:)
         END WHERE                       
      ENDIF
      !                        !  Namelist read
      CALL p2z_opt_init        !  Optics parameters
      CALL p2z_sed_init        !  sedimentation
      CALL p2z_bio_init        !  biology
      CALL p2z_exp_init( Kmm ) !  export 
      !
      IF(lwp) WRITE(numout,*) 
      IF(lwp) WRITE(numout,*) '   ==>>>   Initialization of LOBSTER tracers done'
      IF(lwp) WRITE(numout,*) 
      !
   END SUBROUTINE p2z_ini

   !!======================================================================
END MODULE trcini_pisces
