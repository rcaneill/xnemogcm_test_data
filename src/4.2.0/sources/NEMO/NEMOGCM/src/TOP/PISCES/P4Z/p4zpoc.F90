MODULE p4zpoc
   !!======================================================================
   !!                         ***  MODULE p4zpoc  ***
   !! TOP :   PISCES Compute remineralization of organic particles
   !!         Same module for both PISCES and PISCES-QUOTA
   !!=========================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-06  (O. Aumont, C. Ethe) Quota model for iron
   !!             3.6  !  2016-03  (O. Aumont) Quota model and diverse
   !!             4.0  !  2018     (O. Aumont) Variable lability parameterization
   !!----------------------------------------------------------------------
   !!   p4z_poc       :  Compute remineralization/dissolution of organic compounds
   !!   p4z_poc_init  :  Initialisation of parameters for remineralisation
   !!   alngam and gamain : computation of the incomplete gamma function
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE prtctl          !  print control for debugging
   USE iom             !  I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_poc         ! called in p4zbio.F90
   PUBLIC   p4z_poc_init    ! called in trcini_pisces.F90
   PUBLIC   alngam          ! 
   PUBLIC   gamain          !

   REAL(wp), PUBLIC ::   xremip     !: remineralisation rate of DOC
   REAL(wp), PUBLIC ::   xremipc    !: remineralisation rate of DOC
   REAL(wp), PUBLIC ::   xremipn    !: remineralisation rate of DON
   REAL(wp), PUBLIC ::   xremipp    !: remineralisation rate of DOP
   INTEGER , PUBLIC ::   jcpoc      !: number of lability classes
   REAL(wp), PUBLIC ::   rshape     !: shape factor of the gamma distribution

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::   alphan, reminp   !: variable lability of POC and initial distribution
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   alphap           !: lability distribution of small particles


   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p4zpoc.F90 15459 2021-10-29 08:19:18Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_poc( kt, knt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_poc  ***
      !!
      !! ** Purpose :   Compute remineralization of organic particles
      !!                A reactivity-continuum parameterization is chosen
      !!                to describe the lability of the organic particles
      !!                As a consequence, the remineralisation rates of the 
      !!                the different pools change with time as a function of 
      !!                the lability distribution
      !!
      !! ** Method  : - Computation of the remineralisation rates is performed
      !!                according to reactivity continuum formalism described
      !!                in Aumont et al. (2017). 
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, knt         ! ocean time step and ???
      INTEGER, INTENT(in) ::   Kbb, Kmm, Krhs  ! time level indices
      !
      INTEGER  ::   ji, jj, jk, jn
      REAL(wp) ::   zremip, zremig, zdep, zorem, zorem2, zofer
      REAL(wp) ::   zopon, zopop, zopoc, zopoc2, zopon2, zopop2
      REAL(wp) ::   zsizek, zsizek1, alphat, remint, solgoc, zpoc
      REAL(wp) ::   zofer2, zofer3
      REAL(wp) ::   zrfact2
      CHARACTER (len=25) :: charout
      REAL(wp), DIMENSION(jpi,jpj  )   :: totprod, totthick, totcons 
      REAL(wp), DIMENSION(jpi,jpj,jpk)   :: zremipoc, zremigoc, zorem3, ztremint, zfolimi
      REAL(wp), DIMENSION(jpi,jpj,jpk,jcpoc) :: alphag
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )  CALL timing_start('p4z_poc')
      !
      ! Initialization of local variables
      ! ---------------------------------

      ! Here we compute the GOC -> POC rate due to the shrinking
      ! of the fecal pellets/aggregates as a result of bacterial
      ! solubilization
      ! This is based on a fractal dimension of 2.56 and a spectral
      ! slope of -3.6 (identical to what is used in p4zsink to compute
      ! aggregation
      solgoc = 0.04/ 2.56 * 1./ ( 1.-50**(-0.04) )

      ! Initialisation of temporary arrays
      IF( ln_p4z ) THEN
         zremipoc(:,:,:) = xremip
         zremigoc(:,:,:) = xremip
      ELSE    ! ln_p5z
         zremipoc(:,:,:) = xremipc
         zremigoc(:,:,:) = xremipc
      ENDIF
      zorem3(:,:,:)   = 0.
      orem  (:,:,:)   = 0.
      ztremint(:,:,:) = 0.
      zfolimi (:,:,:) = 0.

      ! Initialisation of the lability distributions that are set to 
      ! the distribution of newly produced organic particles
      DO jn = 1, jcpoc
        alphag(:,:,:,jn) = alphan(jn)
        alphap(:,:,:,jn) = alphan(jn)
      END DO

     ! Lability parameterization. This is the big particles part (GOC)
     ! This lability parameterization is always active. However, if only one
     ! lability class is specified in the namelist, this is equivalent to 
     ! a standard parameterisation with a constant lability
     ! -----------------------------------------------------------------------
     ztremint(:,:,:) = zremigoc(:,:,:)
     DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
        IF (tmask(ji,jj,jk) == 1.) THEN
          zdep = hmld(ji,jj)
          !
          ! In the case of GOC, lability is constant in the mixed layer 
          ! It is computed only below the mixed layer depth
          ! ------------------------------------------------------------
          !
          IF( gdept(ji,jj,jk,Kmm) > zdep ) THEN
            alphat = 0.
            remint = 0.
            !
            zsizek1  = e3t(ji,jj,jk-1,Kmm) / 2. / (wsbio4(ji,jj,jk-1) + rtrn) * tgfunc(ji,jj,jk-1)
            zsizek = e3t(ji,jj,jk,Kmm) / 2. / (wsbio4(ji,jj,jk) + rtrn) * tgfunc(ji,jj,jk)
            !
            IF ( gdept(ji,jj,jk-1,Kmm) <= zdep ) THEN
              ! 
              ! The first level just below the mixed layer needs a 
              ! specific treatment because lability is supposed constant
              ! everywhere within the mixed layer. This means that 
              ! change in lability in the bottom part of the previous cell
              ! should not be computed
              ! ----------------------------------------------------------
              !
              ! POC concentration is computed using the lagrangian 
              ! framework. It is only used for the lability param
              zpoc = tr(ji,jj,jk-1,jpgoc,Kbb) + consgoc(ji,jj,jk) * rday / rfact2               &
              &   * e3t(ji,jj,jk,Kmm) / 2. / (wsbio4(ji,jj,jk) + rtrn)
              zpoc = MAX(0., zpoc)
              !
              DO jn = 1, jcpoc
                 !
                 ! Lagrangian based algorithm. The fraction of each 
                 ! lability class is computed starting from the previous
                 ! level
                 ! -----------------------------------------------------
                 !
                 ! the concentration of each lability class is calculated
                 ! as the sum of the different sources and sinks
                 ! Please note that production of new GOC experiences
                 ! degradation 
                 alphag(ji,jj,jk,jn) = alphag(ji,jj,jk-1,jn) * exp( -reminp(jn) * zsizek ) * zpoc &
                 &   + prodgoc(ji,jj,jk) * alphan(jn) / tgfunc(ji,jj,jk) / reminp(jn)             &
                 &   * ( 1. - exp( -reminp(jn) * zsizek ) ) * rday / rfact2 
                 alphat = alphat + alphag(ji,jj,jk,jn)
                 remint = remint + alphag(ji,jj,jk,jn) * reminp(jn)
              END DO
            ELSE
              !
              ! standard algorithm in the rest of the water column
              ! See the comments in the previous block.
              ! ---------------------------------------------------
              !
              zpoc = tr(ji,jj,jk-1,jpgoc,Kbb) + consgoc(ji,jj,jk-1) * rday / rfact2               &
              &   * e3t(ji,jj,jk-1,Kmm) / 2. / (wsbio4(ji,jj,jk-1) + rtrn) + consgoc(ji,jj,jk)   &
              &   * rday / rfact2 * e3t(ji,jj,jk,Kmm) / 2. / (wsbio4(ji,jj,jk) + rtrn)
              zpoc = max(0., zpoc)
              !
              DO jn = 1, jcpoc
                 alphag(ji,jj,jk,jn) = alphag(ji,jj,jk-1,jn) * exp( -reminp(jn) * ( zsizek              &
                 &   + zsizek1 ) ) * zpoc + ( prodgoc(ji,jj,jk-1) / tgfunc(ji,jj,jk-1) * ( 1.           &
                 &   - exp( -reminp(jn) * zsizek1 ) ) * exp( -reminp(jn) * zsizek ) + prodgoc(ji,jj,jk) &
                 &   / tgfunc(ji,jj,jk) * ( 1. - exp( -reminp(jn) * zsizek ) ) ) * rday / rfact2 / reminp(jn) * alphan(jn) 
                 alphat = alphat + alphag(ji,jj,jk,jn)
                 remint = remint + alphag(ji,jj,jk,jn) * reminp(jn)
              END DO
            ENDIF
            !
            DO jn = 1, jcpoc
               ! The contribution of each lability class at the current
               ! level is computed
               alphag(ji,jj,jk,jn) = alphag(ji,jj,jk,jn) / ( alphat + rtrn)
            END DO
            ! Computation of the mean remineralisation rate
            ztremint(ji,jj,jk) =  MAX(0., remint / ( alphat + rtrn) )
            !
          ENDIF
        ENDIF
     END_3D

      IF( ln_p4z ) THEN   ;   zremigoc(:,:,:) = MIN( xremip , ztremint(:,:,:) )
      ELSE                ;   zremigoc(:,:,:) = MIN( xremipc, ztremint(:,:,:) )
      ENDIF

      IF( ln_p4z ) THEN
         ! The standard PISCES part
         DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
            ! POC degradation by bacterial activity. It is a function 
            ! of the mean lability and of temperature. This also includes
            ! shrinking of particles due to the bacterial activity
            ! -----------------------------------------------------------
            zremig = zremigoc(ji,jj,jk) * xstep * tgfunc(ji,jj,jk)
            zorem2  = zremig * tr(ji,jj,jk,jpgoc,Kbb)
            orem(ji,jj,jk)      = zorem2
            zorem3(ji,jj,jk) = zremig * solgoc * tr(ji,jj,jk,jpgoc,Kbb)
            zofer2 = zremig * tr(ji,jj,jk,jpbfe,Kbb)
            zofer3 = zremig * solgoc * tr(ji,jj,jk,jpbfe,Kbb)

            ! update of the TRA arrays
            tr(ji,jj,jk,jppoc,Krhs) = tr(ji,jj,jk,jppoc,Krhs) + zorem3(ji,jj,jk)
            tr(ji,jj,jk,jpgoc,Krhs) = tr(ji,jj,jk,jpgoc,Krhs) - zorem2 - zorem3(ji,jj,jk)
            tr(ji,jj,jk,jpsfe,Krhs) = tr(ji,jj,jk,jpsfe,Krhs) + zofer3
            tr(ji,jj,jk,jpbfe,Krhs) = tr(ji,jj,jk,jpbfe,Krhs) - zofer2 - zofer3
            tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) + zorem2
            tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) + zofer2
            zfolimi(ji,jj,jk)   = zofer2
         END_3D
      ELSE
         DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
            ! POC degradation by bacterial activity. It is a function 
            ! of the mean lability and of temperature. This also includes
            ! shrinking of particles due to the bacterial activity
            ! --------------------------------------------------------
            zremig = zremigoc(ji,jj,jk) * xstep * tgfunc(ji,jj,jk)
            zopoc2 = zremig  * tr(ji,jj,jk,jpgoc,Kbb)
            orem(ji,jj,jk) = zopoc2
            zorem3(ji,jj,jk) = zremig * solgoc * tr(ji,jj,jk,jpgoc,Kbb)
            zopon2 = xremipn / xremipc * zremig * tr(ji,jj,jk,jpgon,Kbb)
            zopop2 = xremipp / xremipc * zremig * tr(ji,jj,jk,jpgop,Kbb)
            zofer2 = xremipn / xremipc * zremig * tr(ji,jj,jk,jpbfe,Kbb)

            ! update of the TRA arrays
            tr(ji,jj,jk,jppoc,Krhs) = tr(ji,jj,jk,jppoc,Krhs) + zorem3(ji,jj,jk)
            tr(ji,jj,jk,jppon,Krhs) = tr(ji,jj,jk,jppon,Krhs) + solgoc * zopon2 
            tr(ji,jj,jk,jppop,Krhs) = tr(ji,jj,jk,jppop,Krhs) + solgoc * zopop2
            tr(ji,jj,jk,jpsfe,Krhs) = tr(ji,jj,jk,jpsfe,Krhs) + solgoc * zofer2
            tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) + zopoc2
            tr(ji,jj,jk,jpdon,Krhs) = tr(ji,jj,jk,jpdon,Krhs) + zopon2
            tr(ji,jj,jk,jpdop,Krhs) = tr(ji,jj,jk,jpdop,Krhs) + zopop2
            tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) + zofer2
            tr(ji,jj,jk,jpgoc,Krhs) = tr(ji,jj,jk,jpgoc,Krhs) - zopoc2 - zorem3(ji,jj,jk)
            tr(ji,jj,jk,jpgon,Krhs) = tr(ji,jj,jk,jpgon,Krhs) - zopon2 * (1. + solgoc)
            tr(ji,jj,jk,jpgop,Krhs) = tr(ji,jj,jk,jpgop,Krhs) - zopop2 * (1. + solgoc)
            tr(ji,jj,jk,jpbfe,Krhs) = tr(ji,jj,jk,jpbfe,Krhs) - zofer2 * (1. + solgoc)
            zfolimi(ji,jj,jk)   = zofer2
         END_3D
      ENDIF

     IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
        WRITE(charout, FMT="('poc1')")
        CALL prt_ctl_info( charout, cdcomp = 'top' )
        CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
     ENDIF

     ! Lability parameterization for the small OM particles. This param 
     ! is based on the same theoretical background as the big particles.
     ! However, because of its low sinking speed, lability is not supposed
     ! to be equal to its initial value (the value of the freshly produced
     ! organic matter) in the MLD. It is however uniform in the mixed layer.
     ! ---------------------------------------------------------------------
     totprod (:,:) = 0.
     totthick(:,:) = 0.
     totcons (:,:) = 0.

     ! intregrated production and consumption of POC in the mixed layer
     ! ----------------------------------------------------------------
     DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
        zdep = hmld(ji,jj)
        IF (tmask(ji,jj,jk) == 1. .AND. gdept(ji,jj,jk,Kmm) <= zdep ) THEN
          totprod(ji,jj) = totprod(ji,jj) + prodpoc(ji,jj,jk) * e3t(ji,jj,jk,Kmm) * rday/ rfact2
          ! The temperature effect is included here
          totthick(ji,jj) = totthick(ji,jj) + e3t(ji,jj,jk,Kmm)* tgfunc(ji,jj,jk)
          totcons(ji,jj) = totcons(ji,jj) - conspoc(ji,jj,jk) * e3t(ji,jj,jk,Kmm) * rday/ rfact2    &
          &                / ( tr(ji,jj,jk,jppoc,Kbb) + rtrn )
        ENDIF
     END_3D

     ! Computation of the lability spectrum in the mixed layer. In the mixed 
     ! layer, this spectrum is supposed to be uniform as a result of intense
     ! mixing.
     ! ---------------------------------------------------------------------
     ztremint(:,:,:) = zremipoc(:,:,:)
     DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
        IF (tmask(ji,jj,jk) == 1.) THEN
          zdep = hmld(ji,jj)
          alphat = 0.0
          remint = 0.0
          IF( gdept(ji,jj,jk,Kmm) <= zdep ) THEN
             DO jn = 1, jcpoc
                ! For each lability class, the system is supposed to be 
                ! at equilibrium: Prod - Sink - w alphap = 0.
                alphap(ji,jj,jk,jn) = totprod(ji,jj) * alphan(jn) / ( reminp(jn)    &
                &                     * totthick(ji,jj) + totcons(ji,jj) + wsbio + rtrn )
                alphat = alphat + alphap(ji,jj,jk,jn)
             END DO
             DO jn = 1, jcpoc
                alphap(ji,jj,jk,jn) = alphap(ji,jj,jk,jn) / ( alphat + rtrn)
                remint = remint + alphap(ji,jj,jk,jn) * reminp(jn)
             END DO
             ! Mean remineralization rate in the mixed layer
             ztremint(ji,jj,jk) =  MAX( 0., remint )
          ENDIF
        ENDIF
     END_3D
     !
     IF( ln_p4z ) THEN   ;  zremipoc(:,:,:) = MIN( xremip , ztremint(:,:,:) )
     ELSE                ;  zremipoc(:,:,:) = MIN( xremipc, ztremint(:,:,:) )
     ENDIF

     ! The lability parameterization is used here. The code is here 
     ! almost identical to what is done for big particles. The only difference
     ! is that an additional source from GOC to POC is included. This means 
     ! that since we need the lability spectrum of GOC, GOC spectrum 
     ! should be determined before.
     ! -----------------------------------------------------------------------
     DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 2, jpkm1)
        IF (tmask(ji,jj,jk) == 1.) THEN
          zdep = hmld(ji,jj)
          IF( gdept(ji,jj,jk,Kmm) > zdep ) THEN
            alphat = 0.
            remint = 0.
            !
            ! the scale factors are corrected with temperature
            zsizek1  = e3t(ji,jj,jk-1,Kmm) / 2. / (wsbio3(ji,jj,jk-1) + rtrn) * tgfunc(ji,jj,jk-1)
            zsizek = e3t(ji,jj,jk,Kmm) / 2. / (wsbio3(ji,jj,jk) + rtrn) * tgfunc(ji,jj,jk)
            !
            ! Special treatment of the level just below the MXL
            ! See the comments in the GOC section
            ! ---------------------------------------------------
            !
            IF ( gdept(ji,jj,jk-1,Kmm) <= zdep ) THEN
              !
              ! Computation of the POC concentration using the 
              ! lagrangian algorithm
              zpoc = tr(ji,jj,jk-1,jppoc,Kbb) + conspoc(ji,jj,jk) * rday / rfact2               &
              &   * e3t(ji,jj,jk,Kmm) / 2. / (wsbio3(ji,jj,jk) + rtrn)
              zpoc = max(0., zpoc)
              ! 
              DO jn = 1, jcpoc
                 ! computation of the lability spectrum applying the 
                 ! different sources and sinks
                 alphap(ji,jj,jk,jn) = alphap(ji,jj,jk-1,jn) * exp( -reminp(jn) * zsizek ) * zpoc  &
                 &   + ( prodpoc(ji,jj,jk) * alphan(jn) + zorem3(ji,jj,jk) * alphag(ji,jj,jk,jn) ) &
                 &   / tgfunc(ji,jj,jk) / reminp(jn) * rday / rfact2 * ( 1. - exp( -reminp(jn)     &
                 &   * zsizek ) )
                 alphap(ji,jj,jk,jn) = MAX( 0., alphap(ji,jj,jk,jn) )
                 alphat = alphat + alphap(ji,jj,jk,jn)
              END DO
            ELSE
              !
              ! Lability parameterization for the interior of the ocean
              ! This is very similar to what is done in the previous 
              ! block
              ! --------------------------------------------------------
              !
              zpoc = tr(ji,jj,jk-1,jppoc,Kbb) + conspoc(ji,jj,jk-1) * rday / rfact2               &
              &   * e3t(ji,jj,jk-1,Kmm) / 2. / (wsbio3(ji,jj,jk-1) + rtrn) + conspoc(ji,jj,jk)   &
              &   * rday / rfact2 * e3t(ji,jj,jk,Kmm) / 2. / (wsbio3(ji,jj,jk) + rtrn)
              zpoc = max(0., zpoc)
              !
              DO jn = 1, jcpoc
                 alphap(ji,jj,jk,jn) = alphap(ji,jj,jk-1,jn) * exp( -reminp(jn)                       &
                 &   * ( zsizek + zsizek1 ) ) * zpoc + ( prodpoc(ji,jj,jk-1) * alphan(jn)             & 
                 &   + zorem3(ji,jj,jk-1) * alphag(ji,jj,jk-1,jn) ) * rday / rfact2 / reminp(jn)      &
                 &   / tgfunc(ji,jj,jk-1) * ( 1. - exp( -reminp(jn) * zsizek1 ) ) * exp( -reminp(jn)  &
                 &   * zsizek ) + ( prodpoc(ji,jj,jk) * alphan(jn) + zorem3(ji,jj,jk)                 &
                 &   * alphag(ji,jj,jk,jn) ) * rday / rfact2 / reminp(jn) / tgfunc(ji,jj,jk) * ( 1.   &
                 &   - exp( -reminp(jn) * zsizek ) )
                 alphap(ji,jj,jk,jn) = max(0., alphap(ji,jj,jk,jn) )
                 alphat = alphat + alphap(ji,jj,jk,jn)
              END DO
            ENDIF
            ! Normalization of the lability spectrum so that the 
            ! integral is equal to 1
            DO jn = 1, jcpoc
               alphap(ji,jj,jk,jn) = alphap(ji,jj,jk,jn) / ( alphat + rtrn)
               remint = remint + alphap(ji,jj,jk,jn) * reminp(jn)
            END DO
            ! Mean remineralization rate in the water column
            ztremint(ji,jj,jk) =  MAX( 0., remint )
          ENDIF
        ENDIF
     END_3D

     IF( ln_p4z ) THEN   ;   zremipoc(:,:,:) = MIN( xremip , ztremint(:,:,:) )
     ELSE                ;   zremipoc(:,:,:) = MIN( xremipc, ztremint(:,:,:) )
     ENDIF

     IF( ln_p4z ) THEN
         DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
            IF (tmask(ji,jj,jk) == 1.) THEN
              ! POC disaggregation by turbulence and bacterial activity.It is a function
              ! of the mean lability and of temperature  
              ! --------------------------------------------------------
              zremip          = zremipoc(ji,jj,jk) * xstep * tgfunc(ji,jj,jk)
              zorem           = zremip * tr(ji,jj,jk,jppoc,Kbb)
              zofer           = zremip * tr(ji,jj,jk,jpsfe,Kbb)
              
              ! Update of the TRA arrays
              tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) + zorem
              orem(ji,jj,jk)      = orem(ji,jj,jk) + zorem
              tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) + zofer
              tr(ji,jj,jk,jppoc,Krhs) = tr(ji,jj,jk,jppoc,Krhs) - zorem
              tr(ji,jj,jk,jpsfe,Krhs) = tr(ji,jj,jk,jpsfe,Krhs) - zofer
              zfolimi(ji,jj,jk)   = zfolimi(ji,jj,jk) + zofer
            ENDIF
         END_3D
     ELSE
       DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
          ! POC disaggregation by turbulence and bacterial activity.It is a function
          ! of the mean lability and of temperature  
          !--------------------------------------------------------
          zremip = zremipoc(ji,jj,jk) * xstep * tgfunc(ji,jj,jk)
          zopoc  = zremip * tr(ji,jj,jk,jppoc,Kbb)
          orem(ji,jj,jk)  = orem(ji,jj,jk) + zopoc
          zopon  = xremipn / xremipc * zremip * tr(ji,jj,jk,jppon,Kbb)
          zopop  = xremipp / xremipc * zremip * tr(ji,jj,jk,jppop,Kbb)
          zofer  = xremipn / xremipc * zremip * tr(ji,jj,jk,jpsfe,Kbb)
              
          ! Update of the TRA arrays
          tr(ji,jj,jk,jppoc,Krhs) = tr(ji,jj,jk,jppoc,Krhs) - zopoc
          tr(ji,jj,jk,jppon,Krhs) = tr(ji,jj,jk,jppon,Krhs) - zopon
          tr(ji,jj,jk,jppop,Krhs) = tr(ji,jj,jk,jppop,Krhs) - zopop
          tr(ji,jj,jk,jpsfe,Krhs) = tr(ji,jj,jk,jpsfe,Krhs) - zofer
          tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) + zopoc
          tr(ji,jj,jk,jpdon,Krhs) = tr(ji,jj,jk,jpdon,Krhs) + zopon 
          tr(ji,jj,jk,jpdop,Krhs) = tr(ji,jj,jk,jpdop,Krhs) + zopop 
          tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) + zofer 
          zfolimi(ji,jj,jk)   = zfolimi(ji,jj,jk) + zofer
       END_3D
     ENDIF

     IF( lk_iomput ) THEN
        IF( knt == nrdttrc ) THEN
          zrfact2 = 1.e3 * rfact2r
          CALL iom_put( "REMINP" , zremipoc(:,:,:) * tmask(:,:,:) )  ! Remineralisation rate of small particles
          CALL iom_put( "REMING" , zremigoc(:,:,:) * tmask(:,:,:) )  ! Remineralisation rate of large particles
          CALL iom_put( "REMINF" , zfolimi(:,:,:)  * tmask(:,:,:)  * 1.e+9 * zrfact2 )  ! Remineralisation of biogenic particulate iron
        ENDIF
     ENDIF

      IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('poc2')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF
      !
      !
      IF( ln_timing )   CALL timing_stop('p4z_poc')
      !
   END SUBROUTINE p4z_poc


   SUBROUTINE p4z_poc_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_poc_init  ***
      !!
      !! ** Purpose :   Initialization of remineralization parameters
      !!
      !! ** Method  :   Read the nampispoc namelist and check the parameters
      !!              called at the first timestep
      !!
      !! ** input   :   Namelist nampispoc
      !!----------------------------------------------------------------------
      INTEGER ::   jn            ! dummy loop index
      INTEGER ::   ios, ifault   ! Local integer
      REAL(wp)::   remindelta, reminup, remindown
      !!
      NAMELIST/nampispoc/ xremip , jcpoc  , rshape,  &
         &                xremipc, xremipn, xremipp
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'p4z_poc_init : Initialization of remineralization parameters'
         WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF
      !
      READ  ( numnatp_ref, nampispoc, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'nampispoc in reference namelist' )
      READ  ( numnatp_cfg, nampispoc, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'nampispoc in configuration namelist' )
      IF(lwm) WRITE( numonp, nampispoc )

      IF(lwp) THEN                         ! control print
         WRITE(numout,*) '   Namelist : nampispoc'
         IF( ln_p4z ) THEN
            WRITE(numout,*) '      remineralisation rate of POC              xremip    =', xremip
         ELSE
            WRITE(numout,*) '      remineralisation rate of POC              xremipc   =', xremipc
            WRITE(numout,*) '      remineralisation rate of PON              xremipn   =', xremipn
            WRITE(numout,*) '      remineralisation rate of POP              xremipp   =', xremipp
         ENDIF
         WRITE(numout,*) '      Number of lability classes for POC        jcpoc     =', jcpoc
         WRITE(numout,*) '      Shape factor of the gamma distribution    rshape    =', rshape
      ENDIF
      !
      ! Discretization along the lability space
      ! ---------------------------------------
      !
      ALLOCATE( alphan(jcpoc) , reminp(jcpoc) , alphap(jpi,jpj,jpk,jcpoc) )
      !
      IF (jcpoc > 1) THEN  ! Case when more than one lability class is used
         !
         remindelta = LOG(4. * 1000. ) / REAL(jcpoc-1, wp)
         reminup = 1./ 400. * EXP(remindelta)
         !
         ! Discretization based on incomplete gamma functions
         ! As incomplete gamma functions are not available in standard 
         ! fortran 95, they have been coded as functions in this module (gamain)
         ! ---------------------------------------------------------------------
         !
         alphan(1) = gamain(reminup, rshape, ifault)
         reminp(1) = gamain(reminup, rshape+1.0, ifault) * xremip / alphan(1)
         DO jn = 2, jcpoc-1
            reminup = 1./ 400. * EXP( REAL(jn, wp) * remindelta)
            remindown = 1. / 400. * EXP( REAL(jn-1, wp) * remindelta)
            alphan(jn) = gamain(reminup, rshape, ifault) - gamain(remindown, rshape, ifault)
            reminp(jn) = gamain(reminup, rshape+1.0, ifault) - gamain(remindown, rshape+1.0, ifault)
            reminp(jn) = reminp(jn) * xremip / alphan(jn)
         END DO
         remindown = 1. / 400. * EXP( REAL(jcpoc-1, wp) * remindelta)
         alphan(jcpoc) = 1.0 - gamain(remindown, rshape, ifault)
         reminp(jcpoc) = 1.0 - gamain(remindown, rshape+1.0, ifault)
         reminp(jcpoc) = reminp(jcpoc) * xremip / alphan(jcpoc)

      ELSE  ! Only one lability class is used
         alphan(jcpoc) = 1.
         reminp(jcpoc) = xremip
      ENDIF

      DO jn = 1, jcpoc
         alphap(:,:,:,jn) = alphan(jn)
      END DO

   END SUBROUTINE p4z_poc_init


   REAL FUNCTION alngam( xvalue, ifault )
      !*****************************************************************************80
      !
      !! ALNGAM computes the logarithm of the gamma function.
      !
      !  Modified:    13 January 2008
      !
      !  Author  :    Allan Macleod
      !               FORTRAN90 version by John Burkardt
      !
      !  Reference:
      !    Allan Macleod, Algorithm AS 245,
      !    A Robust and Reliable Algorithm for the Logarithm of the Gamma Function,
      !    Applied Statistics,
      !    Volume 38, Number 2, 1989, pages 397-402.
      !
      !  Parameters:
      !
      !    Input, real ( kind = 8 ) XVALUE, the argument of the Gamma function.
      !
      !    Output, integer ( kind = 4 ) IFAULT, error flag.
      !    0, no error occurred.
      !    1, XVALUE is less than or equal to 0.
      !    2, XVALUE is too big.
      !
      !    Output, real ( kind = 8 ) ALNGAM, the logarithm of the gamma function of X.
      !*****************************************************************************80
  implicit none

  real(wp), parameter :: alr2pi = 0.918938533204673E+00
  integer:: ifault
  real(wp), dimension ( 9 ) :: r1 = (/ &
    -2.66685511495E+00, &
    -24.4387534237E+00, &
    -21.9698958928E+00, &
     11.1667541262E+00, &
     3.13060547623E+00, &
     0.607771387771E+00, &
     11.9400905721E+00, &
     31.4690115749E+00, &
     15.2346874070E+00 /)
  real(wp), dimension ( 9 ) :: r2 = (/ &
    -78.3359299449E+00, &
    -142.046296688E+00, &
     137.519416416E+00, &
     78.6994924154E+00, &
     4.16438922228E+00, &
     47.0668766060E+00, &
     313.399215894E+00, &
     263.505074721E+00, &
     43.3400022514E+00 /)
  real(wp), dimension ( 9 ) :: r3 = (/ &
    -2.12159572323E+05, &
     2.30661510616E+05, &
     2.74647644705E+04, &
    -4.02621119975E+04, &
    -2.29660729780E+03, &
    -1.16328495004E+05, &
    -1.46025937511E+05, &
    -2.42357409629E+04, &
    -5.70691009324E+02 /)
  real(wp), dimension ( 5 ) :: r4 = (/ &
     0.279195317918525E+00, &
     0.4917317610505968E+00, &
     0.0692910599291889E+00, &
     3.350343815022304E+00, &
     6.012459259764103E+00 /)
  real (wp) :: x
  real (wp) :: x1
  real (wp) :: x2
  real (wp), parameter :: xlge = 5.10E+05
  real (wp), parameter :: xlgst = 1.0E+30
  real (wp) :: xvalue
  real (wp) :: y

  x = xvalue
  alngam = 0.0E+00
!
!  Check the input.
!
  if ( xlgst <= x ) then
    ifault = 2
    return
  end if
  if ( x <= 0.0E+00 ) then
    ifault = 1
    return
  end if

  ifault = 0
!
!  Calculation for 0 < X < 0.5 and 0.5 <= X < 1.5 combined.
!
  if ( x < 1.5E+00 ) then

    if ( x < 0.5E+00 ) then
      alngam = - log ( x )
      y = x + 1.0E+00
!
!  Test whether X < machine epsilon.
!
      if ( y == 1.0E+00 ) then
        return
      end if

    else

      alngam = 0.0E+00
      y = x
      x = ( x - 0.5E+00 ) - 0.5E+00

    end if

    alngam = alngam + x * (((( &
        r1(5)   * y &
      + r1(4) ) * y &
      + r1(3) ) * y &
      + r1(2) ) * y &
      + r1(1) ) / (((( &
                  y &
      + r1(9) ) * y &
      + r1(8) ) * y &
      + r1(7) ) * y &
      + r1(6) )

    return

  end if
!
!  Calculation for 1.5 <= X < 4.0.
!
  if ( x < 4.0E+00 ) then

    y = ( x - 1.0E+00 ) - 1.0E+00

    alngam = y * (((( &
        r2(5)   * x &
      + r2(4) ) * x &
      + r2(3) ) * x &
      + r2(2) ) * x &
      + r2(1) ) / (((( &
                  x &
      + r2(9) ) * x &
      + r2(8) ) * x &
      + r2(7) ) * x &
      + r2(6) )
!
!  Calculation for 4.0 <= X < 12.0.
!
  else if ( x < 12.0E+00 ) then

    alngam = (((( &
        r3(5)   * x &
      + r3(4) ) * x &
      + r3(3) ) * x &
      + r3(2) ) * x &
      + r3(1) ) / (((( &
                  x &
      + r3(9) ) * x &
      + r3(8) ) * x &
      + r3(7) ) * x &
      + r3(6) )
!
!  Calculation for 12.0 <= X.
!
  else

    y = log ( x )
    alngam = x * ( y - 1.0E+00 ) - 0.5E+00 * y + alr2pi

    if ( x <= xlge ) then

      x1 = 1.0E+00 / x
      x2 = x1 * x1

      alngam = alngam + x1 * ( ( &
             r4(3)   * &
        x2 + r4(2) ) * &
        x2 + r4(1) ) / ( ( &
        x2 + r4(5) ) * &
        x2 + r4(4) )

    end if

   end if

   END FUNCTION alngam


   REAL FUNCTION gamain( x, p, ifault )
!*****************************************************************************80
!
!! GAMAIN computes the incomplete gamma ratio.
!
!  Discussion:
!
!    A series expansion is used if P > X or X <= 1.  Otherwise, a
!    continued fraction approximation is used.
!
!  Modified:
!
!    17 January 2008
!
!  Author:
!
!    G Bhattacharjee
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    G Bhattacharjee,
!    Algorithm AS 32:
!    The Incomplete Gamma Integral,
!    Applied Statistics,
!    Volume 19, Number 3, 1970, pages 285-287.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, P, the parameters of the incomplete 
!    gamma ratio.  0 <= X, and 0 < P.
!
!    Output, integer ( kind = 4 ) IFAULT, error flag.
!    0, no errors.
!    1, P <= 0.
!    2, X < 0.
!    3, underflow.
!    4, error return from the Log Gamma routine.
!
!    Output, real ( kind = 8 ) GAMAIN, the value of the incomplete
!    gamma ratio.
!
  implicit none

  real (wp) a
  real (wp), parameter :: acu = 1.0E-08
  real (wp) an
  real (wp) arg
  real (wp) b
  real (wp) dif
  real (wp) factor
  real (wp) g
  real (wp) gin
  integer i
  integer ifault
  real (wp), parameter :: oflo = 1.0E+37
  real (wp) p
  real (wp) pn(6)
  real (wp) rn
  real (wp) term
  real (wp), parameter :: uflo = 1.0E-37
  real (wp) x
!
!  Check the input.
!
  if ( p <= 0.0E+00 ) then
    ifault = 1
    gamain = 0.0E+00
    return
  end if

  if ( x < 0.0E+00 ) then
    ifault = 2
    gamain = 0.0E+00
    return
  end if

  if ( x == 0.0E+00 ) then
    ifault = 0
    gamain = 0.0E+00
    return
  end if

  g = alngam ( p, ifault )

  if ( ifault /= 0 ) then
    ifault = 4
    gamain = 0.0E+00
    return
  end if

  arg = p * log ( x ) - x - g

  if ( arg < log ( uflo ) ) then
    ifault = 3
    gamain = 0.0E+00
    return
  end if

  ifault = 0
  factor = exp ( arg )
!
!  Calculation by series expansion.
!
  if ( x <= 1.0E+00 .or. x < p ) then

    gin = 1.0E+00
    term = 1.0E+00
    rn = p

    do

      rn = rn + 1.0E+00
      term = term * x / rn
      gin = gin + term

      if ( term <= acu ) then
        exit
      end if

    end do

    gamain = gin * factor / p
    return

  end if
!
!  Calculation by continued fraction.
!
  a = 1.0E+00 - p
  b = a + x + 1.0E+00
  term = 0.0E+00

  pn(1) = 1.0E+00
  pn(2) = x
  pn(3) = x + 1.0E+00
  pn(4) = x * b

  gin = pn(3) / pn(4)

  do

    a = a + 1.0E+00
    b = b + 2.0E+00
    term = term + 1.0E+00
    an = a * term
    do i = 1, 2
      pn(i+4) = b * pn(i+2) - an * pn(i)
    end do

    if ( pn(6) /= 0.0E+00 ) then

      rn = pn(5) / pn(6)
      dif = abs ( gin - rn )
!
!  Absolute error tolerance satisfied?
!
      if ( dif <= acu ) then
!
!  Relative error tolerance satisfied?
!
        if ( dif <= acu * rn ) then
          gamain = 1.0E+00 - factor * gin
          exit
        end if

      end if

      gin = rn

    end if

    do i = 1, 4
      pn(i) = pn(i+2)
    end do
    if ( oflo <= abs ( pn(5) ) ) then

      do i = 1, 4
        pn(i) = pn(i) / oflo
      end do

    end if

  end do

  END FUNCTION gamain

   !!======================================================================
END MODULE p4zpoc
