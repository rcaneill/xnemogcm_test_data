MODULE trcopt
   !!======================================================================
   !!                         ***  MODULE trcopt  ***
   !! TOP :   Compute the light in the water column for RGB wavelengths
   !!======================================================================
   !! History :  1.0  !  2020     (T. Lovato) Initial code
   !!----------------------------------------------------------------------
   !!   trc_opt       : light availability in the water column
   !!----------------------------------------------------------------------
   USE trc            ! tracer variables
   USE oce_trc        ! tracer-ocean share variables
   USE iom            ! I/O manager
   USE fldread        ! time interpolation

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_opt       ! called in spefici BGC model routines
   PUBLIC   trc_opt_ini   ! called in trcini.F90
   PUBLIC   trc_opt_alloc

   !! * Shared module variables

   LOGICAL  ::   ln_varpar   ! boolean for variable PAR fraction
   REAL(wp), PUBLIC ::   parlux      ! Fraction of shortwave as PAR
   CHARACTER (len=25) :: light_loc ! Light location in the water cell ('center', 'integral')

   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf_par        ! structure of input par
   INTEGER                              ::   ntimes_par    ! number of time steps in par file
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   par_varsw      ! PAR fraction of shortwave
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   ekb, ekg, ekr  ! wavelength (Red-Green-Blue)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:), PUBLIC ::   zeps  ! weighted diffusion coefficient

   INTEGER  ::   nksrp   ! levels below which the light cannot penetrate ( depth larger than 391 m)

   ! TL: This array should come directly from traqsr module
   REAL(wp), DIMENSION(3,61) ::   xkrgb   ! tabulated attenuation coefficients for RGB absorption
   
   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2020)
   !! $Id: trcopt.F90 12377 2020-02-12 14:39:06Z acc $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_opt( kt, knt, Kbb, Kmm, zchl, ze1, ze2, ze3)
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_opt  ***
      !!
      !! ** Purpose : Compute the light availability in the water column
      !!              depending on depth and chlorophyll concentration
      !!
      !! ** Method  : Morel
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, knt   ! ocean time step
      INTEGER, INTENT(in) ::   Kbb, Kmm  ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(in) ::   zchl  ! chlorophyll field
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(out) :: ze1, ze2, ze3 ! PAR for individual wavelength
      !
      INTEGER  ::   ji, jj, jk, irgb
      REAL(wp) ::   ztmp
      REAL(wp), DIMENSION(jpi,jpj    ) :: parsw, zqsr100, zqsr_corr
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: ze0
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_opt')

      !     Initialisation of variables used to compute PAR
      !     -----------------------------------------------
      ze0(:,:,:) = 0._wp
      ze1(:,:,:) = 0._wp
      ze2(:,:,:) = 0._wp
      ze3(:,:,:) = 0._wp

      !     PAR conversion factor
      !     --------------------
      IF( knt == 1 .AND. ln_varpar )   CALL trc_opt_sbc( kt )
      !
      IF( ln_varpar ) THEN  ;  parsw(:,:) = par_varsw(:,:)
      ELSE                  ;  parsw(:,:) = parlux / 3.0
      ENDIF

      !     Attenuation coef. function of Chlorophyll and wavelength (RGB)
      !     --------------------------------------------------------------
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 )
         ztmp = ( zchl(ji,jj,jk) + rtrn ) * 1.e6
         ztmp = MIN(  10. , MAX( 0.05, ztmp )  )
         irgb = NINT( 41 + 20.* LOG10( ztmp ) + rtrn )
         !                                                         
         ekb(ji,jj,jk) = xkrgb(1,irgb) * e3t(ji,jj,jk,Kmm)
         ekg(ji,jj,jk) = xkrgb(2,irgb) * e3t(ji,jj,jk,Kmm)
         ekr(ji,jj,jk) = xkrgb(3,irgb) * e3t(ji,jj,jk,Kmm)
      END_3D

      !     Heat flux across w-level (used in the dynamics)
      !     -----------------------------------------------
      IF( ln_qsr_bio ) THEN
         !
         zqsr_corr(:,:) = parsw(:,:) * qsr(:,:)
         !
         ze0(:,:,1) = (1._wp - 3._wp * parsw(:,:)) * qsr(:,:)  !  ( 1 - 3 * alpha ) * q
         ze1(:,:,1) = zqsr_corr(:,:)
         ze2(:,:,1) = zqsr_corr(:,:)
         ze3(:,:,1) = zqsr_corr(:,:)
         !
         DO jk = 2, nksrp + 1
            DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
                  ze0(ji,jj,jk) = ze0(ji,jj,jk-1) * EXP( -e3t(ji,jj,jk-1,Kmm) * (1. / rn_si0) )
                  ze1(ji,jj,jk) = ze1(ji,jj,jk-1) * EXP( -ekb  (ji,jj,jk-1 )        )
                  ze2(ji,jj,jk) = ze2(ji,jj,jk-1) * EXP( -ekg  (ji,jj,jk-1 )        )
                  ze3(ji,jj,jk) = ze3(ji,jj,jk-1) * EXP( -ekr  (ji,jj,jk-1 )        )
            END_2D
         END DO
         !
         etot3(:,:,1) = qsr(:,:) * tmask(:,:,1)
         DO jk = 2, nksrp + 1
            etot3(:,:,jk) =  ( ze0(:,:,jk) + ze1(:,:,jk) + ze2(:,:,jk) + ze3(:,:,jk) ) * tmask(:,:,jk)
         END DO
         !                                     !  ------------------------
      ENDIF

      !     Photosynthetically Available Radiation (PAR)
      !     --------------------------------------------
      zqsr_corr(:,:) = parsw(:,:) * qsr(:,:) / ( 1.-fr_i(:,:) + rtrn )
      !
      CALL trc_opt_par( kt, zqsr_corr, ze1, ze2, ze3 )
      !
      DO jk = 1, nksrp
         etot (:,:,jk) = ze1(:,:,jk) + ze2(:,:,jk) + ze3(:,:,jk)
      ENDDO

      ! No Diurnal cycle PAR
      IF( l_trcdm2dc ) THEN
         zqsr_corr(:,:) = parsw(:,:) * qsr_mean(:,:) / ( 1.-fr_i(:,:) + rtrn )
         !
         CALL trc_opt_par( kt, zqsr_corr, ze1, ze2, ze3 )
         DO jk = 1, nksrp
            etot_ndcy(:,:,jk) = ze1(:,:,jk) + ze2(:,:,jk) + ze3(:,:,jk)
         END DO
      ELSE
         etot_ndcy(:,:,:) = etot(:,:,:)
      ENDIF

      !     Weighted broadband attenuation coefficient
      !     ------------------------------------------
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 )
         ztmp = ze1(ji,jj,jk)* ekb(ji,jj,jk) + ze2(ji,jj,jk) * ekg(ji,jj,jk) + ze3(ji,jj,jk) * ekr(ji,jj,jk)
         zeps(ji,jj,jk) = ztmp / e3t(ji,jj,jk,Kmm) / (etot(ji,jj,jk) + rtrn)
      END_3D
      !zeps = (ze1(:,:,:) * ekb(:,:,:) + ze2(:,:,:) * ekg(:,:,:) + ze3(:,:,:) * ekr(:,:,:)) / e3t(:,:,:,Kmm) / (etot(:,:,:) + rtrn)

      !     Light at the euphotic depth
      !     ---------------------------
      zqsr100 = 0.01 * 3. * zqsr_corr(:,:)

      !     Euphotic depth and level
      !     ------------------------
      neln   (:,:) = 1
      heup   (:,:) = gdepw(:,:,2,Kmm)
      heup_01(:,:) = gdepw(:,:,2,Kmm)
      !
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 2, nksrp )
        IF( etot_ndcy(ji,jj,jk) * tmask(ji,jj,jk) >=  zqsr100(ji,jj) )  THEN
           ! Euphotic level (1st T-level strictly below Euphotic layer)
           ! NOTE: ensure compatibility with nmld_trc definition in trdmxl_trc
           neln(ji,jj) = jk+1
           !
           ! Euphotic layer depth
           heup(ji,jj) = gdepw(ji,jj,jk+1,Kmm)
        ENDIF
        ! Euphotic layer depth (light level definition)
        IF( etot_ndcy(ji,jj,jk) * tmask(ji,jj,jk) >= 0.50 )  THEN
           heup_01(ji,jj) = gdepw(ji,jj,jk+1,Kmm)
        ENDIF
      END_3D
      !
      heup   (:,:) = MIN( 300., heup   (:,:) )
      heup_01(:,:) = MIN( 300., heup_01(:,:) )
      !
      IF( lk_iomput ) THEN
         CALL iom_put( "xbla" , zeps(:,:,:) * tmask(:,:,:) )
         CALL iom_put( "Heup" , heup(:,:  ) * tmask(:,:,1) )
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('trc_opt')
      !
   END SUBROUTINE trc_opt


   SUBROUTINE trc_opt_par( kt, zqsr, pe1, pe2, pe3) 
      !!----------------------------------------------------------------------
      !!                  ***  routine trc_opt_par  ***
      !!
      !! ** purpose :   compute PAR of each wavelength (Red-Green-Blue)
      !!                from given surface shortwave radiation
      !!
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in)      ::   kt                ! ocean time-step
      REAL(wp), DIMENSION(jpi,jpj)    , INTENT(in)      ::   zqsr              ! real shortwave
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(out)     ::   pe1 , pe2 , pe3   ! PAR (R-G-B)
      !
      INTEGER                       ::   ji, jj, jk        ! dummy loop indices
      REAL(wp), DIMENSION(jpi,jpj)  ::   we1, we2, we3     ! PAR (R-G-B) at w-level
      !!----------------------------------------------------------------------
      pe1(:,:,:) = 0. ; pe2(:,:,:) = 0. ; pe3(:,:,:) = 0.
      !
      IF ( TRIM(light_loc) == 'center' ) THEN
         ! cell-center (t-pivot)
         pe1(:,:,1) = zqsr(:,:) * EXP( -0.5 * ekb(:,:,1) )
         pe2(:,:,1) = zqsr(:,:) * EXP( -0.5 * ekg(:,:,1) )
         pe3(:,:,1) = zqsr(:,:) * EXP( -0.5 * ekr(:,:,1) )
         !
         DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 2, nksrp )
            pe1(ji,jj,jk) = pe1(ji,jj,jk-1) * EXP( -0.5 * ( ekb(ji,jj,jk-1) + ekb(ji,jj,jk) ) )
            pe2(ji,jj,jk) = pe2(ji,jj,jk-1) * EXP( -0.5 * ( ekg(ji,jj,jk-1) + ekg(ji,jj,jk) ) )
            pe3(ji,jj,jk) = pe3(ji,jj,jk-1) * EXP( -0.5 * ( ekr(ji,jj,jk-1) + ekr(ji,jj,jk) ) )
         END_3D
         !
      ELSE IF ( TRIM(light_loc) == 'integral' ) THEN
         ! integrate over cell thickness
         we1(:,:) = zqsr(:,:)
         we2(:,:) = zqsr(:,:)
         we3(:,:) = zqsr(:,:)
         !
         DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, nksrp )
            ! integrate PAR over current t-level
            pe1(ji,jj,jk) = we1(ji,jj) / (ekb(ji,jj,jk) + rtrn) * (1. - EXP( -ekb(ji,jj,jk) ))
            pe2(ji,jj,jk) = we2(ji,jj) / (ekg(ji,jj,jk) + rtrn) * (1. - EXP( -ekg(ji,jj,jk) ))
            pe3(ji,jj,jk) = we3(ji,jj) / (ekr(ji,jj,jk) + rtrn) * (1. - EXP( -ekr(ji,jj,jk) ))
            ! PAR at next w-level
            we1(ji,jj) = we1(ji,jj) * EXP( -ekb(ji,jj,jk) )
            we2(ji,jj) = we2(ji,jj) * EXP( -ekg(ji,jj,jk) )
            we3(ji,jj) = we3(ji,jj) * EXP( -ekr(ji,jj,jk) )
         END_3D
         !
      ENDIF 
      !
      ! 
   END SUBROUTINE trc_opt_par


   SUBROUTINE trc_opt_sbc( kt )
      !!----------------------------------------------------------------------
      !!                  ***  routine trc_opt_sbc  ***
      !!
      !! ** purpose :   read and interpolate the variable PAR fraction
      !!                of shortwave radiation
      !!
      !! ** method  :   read the files and interpolate the appropriate variables
      !!
      !! ** input   :   external netcdf files
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      !
      INTEGER  :: ji,jj
      REAL(wp) :: zcoef
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )  CALL timing_start('trc_opt_sbc')
      !
      ! Compute par_varsw at nittrc000 or only if there is more than 1 time record in par coefficient file
      IF( ln_varpar ) THEN
         IF( kt == nittrc000 .OR. ( kt /= nittrc000 .AND. ntimes_par > 1 ) ) THEN
            CALL fld_read( kt, 1, sf_par )
            par_varsw(:,:) = ( sf_par(1)%fnow(:,:,1) ) / 3.0
         ENDIF
      ENDIF
      !
      IF( ln_timing )  CALL timing_stop('trc_opt_sbc')
      !
   END SUBROUTINE trc_opt_sbc


   SUBROUTINE trc_opt_ini
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trc_opt_ini  ***
      !!
      !! ** Purpose :   Initialization of tabulated attenuation coefficients
      !!                and percentage of PAR in Shortwave
      !!
      !! ** Input   :   external ascii and netcdf files
      !!----------------------------------------------------------------------
      INTEGER :: numpar, ierr, ios   ! Local integer 
      !
      CHARACTER(len=100) ::  cn_dir          ! Root directory for location of ssr files
      TYPE(FLD_N) ::   sn_par                ! informations about the fields to be read
      !
      NAMELIST/namtrc_opt/cn_dir, sn_par, ln_varpar, parlux, light_loc
      !!----------------------------------------------------------------------
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'trc_opt_ini : Initialize light module'
         WRITE(numout,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
      ENDIF
      READ  ( numnat_ref, namtrc_opt, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namtrc_opt in reference namelist' )
      READ  ( numnat_cfg, namtrc_opt, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namtrc_opt in configuration namelist' )
      IF(lwm) WRITE ( numont, namtrc_opt )

      IF(lwp) THEN
         WRITE(numout,*) '   Namelist : namtrc_opt '
         WRITE(numout,*) '      PAR as a variable fraction of SW     ln_varpar      = ', ln_varpar
         WRITE(numout,*) '      Fraction of shortwave as PAR         parlux         = ', parlux
         WRITE(numout,*) '      Light location in the water cell     light_loc      = ', light_loc
      ENDIF
      !
      ! Variable PAR at the surface of the ocean
      ! ----------------------------------------
      IF( ln_varpar ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '   ==>>>   initialize variable par fraction (ln_varpar=T)'
         !
         ALLOCATE( par_varsw(jpi,jpj) )
         !
         ALLOCATE( sf_par(1), STAT=ierr )           !* allocate and fill sf_par (forcing structure) with sn_par
         IF( ierr > 0 )   CALL ctl_stop( 'STOP', 'trc_opt_ini: unable to allocate sf_par structure' )
         !
         CALL fld_fill( sf_par, (/ sn_par /), cn_dir, 'trc_opt_ini', 'Initialize prescribed PAR forcing ', 'namtrc_opt' )
                                   ALLOCATE( sf_par(1)%fnow(jpi,jpj,1)   )
         IF( sn_par%ln_tint )      ALLOCATE( sf_par(1)%fdta(jpi,jpj,1,2) )

         CALL iom_open (  TRIM( sn_par%clname ) , numpar )
         ntimes_par = iom_getszuld( numpar )   ! get number of record in file
      ENDIF
      !
      CALL trc_oce_rgb( xkrgb )                  ! tabulated attenuation coefficients
      nksrp = trc_oce_ext_lev( r_si2, 0.33e2 )   ! max level of light extinction (Blue Chl=0.01)
      !
      IF(lwp) WRITE(numout,*) '        level of light extinction = ', nksrp, ' ref depth = ', gdepw_1d(nksrp+1), ' m'
      !
                         ekr      (:,:,:) = 0._wp
                         ekb      (:,:,:) = 0._wp
                         ekg      (:,:,:) = 0._wp
                         etot     (:,:,:) = 0._wp
                         etot_ndcy(:,:,:) = 0._wp
      IF( ln_qsr_bio )   etot3    (:,:,:) = 0._wp
      ! 
   END SUBROUTINE trc_opt_ini


   INTEGER FUNCTION trc_opt_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE trc_opt_alloc  ***
      !!----------------------------------------------------------------------
      !
      ALLOCATE( ekb(jpi,jpj,jpk), ekr(jpi,jpj,jpk),  &
                ekg(jpi,jpj,jpk),zeps(jpi,jpj,jpk),  STAT= trc_opt_alloc  ) 
      !
      IF( trc_opt_alloc /= 0 ) CALL ctl_stop( 'STOP', 'trc_opt_alloc : failed to allocate arrays.' )
      !
   END FUNCTION trc_opt_alloc

   !!======================================================================
END MODULE trcopt
