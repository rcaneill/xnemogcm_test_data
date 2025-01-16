MODULE sbcabl
   !!======================================================================
   !!                       ***  MODULE  sbcabl  ***
   !! Ocean forcing:  momentum, heat and freshwater flux formulation
   !!                 derived from an ABL model
   !!=====================================================================
   !! History :  4.0  !  2019-03  (F. Lemarié & G. Samson)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   sbc_abl_init  : Initialization of ABL model based on namelist options
   !!   sbc_abl       : driver for the computation of momentum, heat and freshwater
   !!                   fluxes over ocean via the ABL model
   !!----------------------------------------------------------------------
   USE abl            ! ABL
   USE par_abl        ! abl parameters
   USE ablmod
   USE ablrst

   USE phycst         ! physical constants
   USE fldread        ! read input fields
   USE sbc_oce        ! Surface boundary condition: ocean fields
   USE sbcblk         ! Surface boundary condition: bulk formulae
   USE sbc_phy        ! Catalog of functions for physical/meteorological parameters in the marine boundary layer
   USE dom_oce, ONLY  : tmask
   !
   USE iom            ! I/O manager library
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! distribued memory computing library
   USE lib_fortran    ! to use key_nosignedzero
   USE timing         ! Timing
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE prtctl         ! Print control
#if defined key_si3
   USE ice    , ONLY : u_ice, v_ice, tm_su, ato_i      ! ato_i = total open water fractional area
   USE sbc_ice, ONLY : wndm_ice, utau_ice, vtau_ice
#endif
#if ! defined key_xios
   USE diawri    , ONLY : dia_wri_alloc_abl
#endif
   IMPLICIT NONE
   PRIVATE

   PUBLIC   sbc_abl_init       ! routine called in sbcmod module
   PUBLIC   sbc_abl            ! routine called in sbcmod module

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.7 , NEMO-consortium (2014)
   !! $Id: sbcabl.F90 6416 2016-04-01 12:22:17Z clem $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE sbc_abl_init
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE sbc_abl_init  ***
      !!
      !! ** Purposes :   - read namelist section namsbc_abl
      !!                 - initialize and check parameter values
      !!                 - initialize variables of ABL model
      !!
      !!----------------------------------------------------------------------
      INTEGER            ::   ji, jj, jk, jbak, jbak_dta       ! dummy loop indices
      INTEGER            ::   ios, ierror, ioptio              ! Local integer
      INTEGER            ::   inum, indims, idimsz(4), id
      CHARACTER(len=100) ::   cn_dir, cn_dom                   ! Atmospheric grid directory
      REAL(wp)           ::   zcff,zcff1
      LOGICAL            ::   lluldl
      NAMELIST/namsbc_abl/ cn_dir, cn_dom, cn_ablrst_in, cn_ablrst_out,           &
         &                 cn_ablrst_indir, cn_ablrst_outdir, ln_rstart_abl,      &
         &                 ln_hpgls_frc, ln_geos_winds, nn_dyn_restore,           &
         &                 rn_ldyn_min , rn_ldyn_max, rn_ltra_min, rn_ltra_max,   &
         &                 nn_amxl, rn_Cm, rn_Ct, rn_Ce, rn_Ceps, rn_Rod, rn_Ric, &
         &                 rn_vfac, ln_smth_pblh
      !!---------------------------------------------------------------------

                                        ! Namelist namsbc_abl in reference namelist : ABL parameters
      READ  ( numnam_ref, namsbc_abl, IOSTAT = ios, ERR = 901 )
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namsbc_abl in reference namelist' )
      !
                                        ! Namelist namsbc_abl in configuration namelist : ABL parameters
      READ  ( numnam_cfg, namsbc_abl, IOSTAT = ios, ERR = 902 )
902   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namsbc_abl in configuration namelist' )
      !
      IF(lwm) WRITE( numond, namsbc_abl )
      !
      ! Check ABL mixing length option
      IF( nn_amxl  < 0   .OR.  nn_amxl  > 2 )   &
         &                 CALL ctl_stop( 'abl_init : bad flag, nn_amxl must be  0, 1 or 2 ' )
      !
      ! Check ABL dyn restore option
      IF( nn_dyn_restore  < 0   .OR.  nn_dyn_restore  > 2 )   &
         &                 CALL ctl_stop( 'abl_init : bad flag, nn_dyn_restore must be  0, 1 or 2 ' )

      !!---------------------------------------------------------------------
      !! Control prints
      !!---------------------------------------------------------------------
      IF(lwp) THEN                              ! Control print (other namelist variable)
         WRITE(numout,*)
         WRITE(numout,*) '      ABL -- cn_dir         = ', cn_dir
         WRITE(numout,*) '      ABL -- cn_dom         = ', cn_dom
         IF( ln_hpgls_frc ) THEN
            WRITE(numout,*) '      ABL -- winds forced by large-scale pressure gradient'
            IF(ln_geos_winds) THEN
               ln_geos_winds = .FALSE.
               WRITE(numout,*) '      ABL -- geostrophic guide disabled (not compatible with ln_hpgls_frc = .T.)'
            END IF
         ELSE IF( ln_geos_winds ) THEN
            WRITE(numout,*) '      ABL -- winds forced by geostrophic winds'
         ELSE
            WRITE(numout,*) '      ABL -- Geostrophic winds and large-scale pressure gradient are ignored'
         END IF
         !
         SELECT CASE ( nn_dyn_restore )
         CASE ( 0 )
            WRITE(numout,*) '      ABL -- No restoring for ABL winds'
         CASE ( 1 )
            WRITE(numout,*) '      ABL -- Restoring of ABL winds only in the equatorial region '
         CASE ( 2 )
            WRITE(numout,*) '      ABL -- Restoring of ABL winds activated everywhere '
         END SELECT
         !
         IF( ln_smth_pblh )  WRITE(numout,*) '      ABL -- Smoothing of PBL height is activated'
         !
      ENDIF

      !!---------------------------------------------------------------------
      !! Convert nudging coefficient from hours to 1/sec
      !!---------------------------------------------------------------------
      zcff        = 1._wp / 3600._wp
      rn_ldyn_min = zcff / rn_ldyn_min
      rn_ldyn_max = zcff / rn_ldyn_max
      rn_ltra_min = zcff / rn_ltra_min
      rn_ltra_max = zcff / rn_ltra_max

      !!---------------------------------------------------------------------
      !! ABL grid initialization
      !!---------------------------------------------------------------------
      CALL iom_open( TRIM(cn_dir)//TRIM(cn_dom), inum )
      id     = iom_varid( inum, 'e3t_abl', kdimsz=idimsz, kndims=indims, lduld=lluldl )
      jpka   = idimsz(indims - COUNT( (/lluldl/) ) )
      jpkam1 = jpka - 1

      IF( abl_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'abl_init : unable to allocate arrays' )
      CALL iom_get( inum, jpdom_unknown, 'e3t_abl', e3t_abl(:) )
      CALL iom_get( inum, jpdom_unknown, 'e3w_abl', e3w_abl(:) )
      CALL iom_get( inum, jpdom_unknown, 'ght_abl', ght_abl(:) )
      CALL iom_get( inum, jpdom_unknown, 'ghw_abl', ghw_abl(:) )
      CALL iom_close( inum )

#if ! defined key_xios
      IF( dia_wri_alloc_abl()  /= 0 ) CALL ctl_stop( 'STOP', 'abl_init : unable to allocate arrays' )
#endif

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) '    sbc_abl_init   : ABL Reference vertical grid'
         WRITE(numout,*) '    ~~~~~~~'
         WRITE(numout, "(9x,'  level ght_abl   ghw_abl   e3t_abl   e3w_abl  ')" )
         WRITE(numout, "(10x, i4, 4f9.2)" ) ( jk, ght_abl(jk), ghw_abl(jk), e3t_abl(jk), e3w_abl(jk), jk = 1, jpka )
      END IF

      !!---------------------------------------------------------------------
      !! Check TKE closure parameters
      !!---------------------------------------------------------------------
      rn_Sch  = rn_ce / rn_cm
      mxl_min = (avm_bak / rn_cm) / sqrt( tke_min )
      rn_Esfc =  1._wp / SQRT(rn_cm*rn_ceps)
      rn_Lsfc = vkarmn * SQRT(SQRT(rn_cm*rn_ceps)) / rn_cm

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) '    abl_zdf_tke   : ABL TKE turbulent closure'
         WRITE(numout,*) '    ~~~~~~~~~~~'
         IF(nn_amxl==0) WRITE(numout,*) 'Deardorff 80 length-scale '
         IF(nn_amxl==1) WRITE(numout,*) 'Modified Deardorff 80 length-scale '
         IF(nn_amxl==2) WRITE(numout,*) 'Bougeault and Lacarrere length-scale '
         IF(nn_amxl==3) WRITE(numout,*) 'Rodier et al. length-scale '
         WRITE(numout,*) ' Minimum value of atmospheric TKE           = ',tke_min,' m^2 s^-2'
         WRITE(numout,*) ' Minimum value of atmospheric mixing length = ',mxl_min,' m'
         WRITE(numout,*) ' Constant for turbulent viscosity           = ',rn_Cm
         WRITE(numout,*) ' Constant for turbulent diffusivity         = ',rn_Ct
         WRITE(numout,*) ' Constant for Schmidt number                = ',rn_Sch
         WRITE(numout,*) ' Constant for TKE dissipation               = ',rn_Ceps
         WRITE(numout,*) ' Constant for TKE sfc boundary condition    = ',rn_Esfc
         WRITE(numout,*) ' Constant for mxl sfc boundary condition    = ',rn_Lsfc
      END IF

      !!-------------------------------------------------------------------------------------------
      !! Compute parameters to build the vertical profile for the nudging term (used in abl_stp())
      !!-------------------------------------------------------------------------------------------
      zcff1       = 1._wp / ( jp_bmax - jp_bmin )**3
      ! for active tracers
      jp_alp3_tra = -2._wp * zcff1                       * ( rn_ltra_max - rn_ltra_min )
      jp_alp2_tra =  3._wp * zcff1 * (jp_bmax + jp_bmin) * ( rn_ltra_max - rn_ltra_min )
      jp_alp1_tra = -6._wp * zcff1 *  jp_bmax * jp_bmin  * ( rn_ltra_max - rn_ltra_min )
      jp_alp0_tra = zcff1 * (  rn_ltra_max * jp_bmin*jp_bmin * (3._wp*jp_bmax - jp_bmin)      &
         &                   - rn_ltra_min * jp_bmax*jp_bmax * (3._wp*jp_bmin - jp_bmax) )
      ! for dynamics
      jp_alp3_dyn = -2._wp * zcff1                       * ( rn_ldyn_max - rn_ldyn_min )
      jp_alp2_dyn =  3._wp * zcff1 * (jp_bmax + jp_bmin) * ( rn_ldyn_max - rn_ldyn_min )
      jp_alp1_dyn = -6._wp * zcff1 *  jp_bmax * jp_bmin  * ( rn_ldyn_max - rn_ldyn_min )
      jp_alp0_dyn = zcff1 * (  rn_ldyn_max * jp_bmin*jp_bmin * (3._wp*jp_bmax - jp_bmin)      &
         &                   - rn_ldyn_min * jp_bmax*jp_bmax * (3._wp*jp_bmin - jp_bmax) )

      jp_pblh_min = ghw_abl(     4) / jp_bmin  !<-- at least 3 grid points at the bottom have value rn_ltra_min
      jp_pblh_max = ghw_abl(jpka-3) / jp_bmax  !<-- at least 3 grid points at the top    have value rn_ltra_max

      ! ABL timestep
      rDt_abl = nn_fsbc * rn_Dt
      IF(lwp) WRITE(numout,*) ' ABL timestep = ', rDt_abl,' s'

      ! Check parameters for dynamics
      zcff  = ( jp_alp3_dyn * jp_bmin**3 + jp_alp2_dyn * jp_bmin**2   &
         &    + jp_alp1_dyn * jp_bmin    + jp_alp0_dyn ) * rDt_abl
      zcff1 = ( jp_alp3_dyn * jp_bmax**3 + jp_alp2_dyn * jp_bmax**2   &
         &    + jp_alp1_dyn * jp_bmax    + jp_alp0_dyn ) * rDt_abl
      IF(lwp) THEN
         IF(nn_dyn_restore > 0) THEN
            WRITE(numout,*) ' Minimum value for ABL dynamics restoring = ',zcff
            WRITE(numout,*) ' Maximum value for ABL dynamics restoring = ',zcff1
            ! Check that restoring coefficients are between 0 and 1
            IF( zcff1 - nn_fsbc > 0.001_wp .OR. zcff1 < 0._wp )   &
               &                   CALL ctl_stop( 'abl_init : wrong value for rn_ldyn_max' )
            IF( zcff  - nn_fsbc > 0.001_wp .OR. zcff  < 0._wp )   &
               &                   CALL ctl_stop( 'abl_init : wrong value for rn_ldyn_min' )
            IF( zcff  > zcff1                    )   &
               &                   CALL ctl_stop( 'abl_init : rn_ldyn_max must be smaller than rn_ldyn_min' )
         END IF
      END IF

      ! Check parameters for active tracers
      zcff  = ( jp_alp3_tra * jp_bmin**3 + jp_alp2_tra * jp_bmin**2   &
         &    + jp_alp1_tra * jp_bmin    + jp_alp0_tra ) * rDt_abl
      zcff1 = ( jp_alp3_tra * jp_bmax**3 + jp_alp2_tra * jp_bmax**2   &
         &    + jp_alp1_tra * jp_bmax    + jp_alp0_tra ) * rDt_abl
      IF(lwp) THEN
         WRITE(numout,*) ' Minimum value for ABL tracers restoring = ',zcff
         WRITE(numout,*) ' Maximum value for ABL tracers restoring = ',zcff1
         ! Check that restoring coefficients are between 0 and 1
         IF( zcff1 - nn_fsbc > 0.001_wp .OR. zcff1 < 0._wp )   &
            &                   CALL ctl_stop( 'abl_init : wrong value for rn_ltra_max' )
         IF( zcff  - nn_fsbc > 0.001_wp .OR. zcff  < 0._wp )   &
            &                   CALL ctl_stop( 'abl_init : wrong value for rn_ltra_min' )
         IF( zcff  > zcff1                    )   &
            &                   CALL ctl_stop( 'abl_init : rn_ltra_max must be smaller than rn_ltra_min' )
      END IF

      !!-------------------------------------------------------------------------------------------
      !! Initialize Coriolis frequency, equatorial restoring and land/sea mask
      !!-------------------------------------------------------------------------------------------
      fft_abl(:,:) = 2._wp * omega * SIN( rad * gphit(:,:) )

      ! Equatorial restoring
      IF( nn_dyn_restore == 1 ) THEN
         zcff         = 2._wp * omega * SIN( rad * 90._wp )   !++ fmax
         rest_eq(:,:) = SIN( 0.5_wp*rpi*( (fft_abl(:,:) - zcff) / zcff ) )**8
      ELSE
         rest_eq(:,:) = 1._wp
      END IF
      ! T-mask
      msk_abl(:,:) = tmask(:,:,1)

      !!-------------------------------------------------------------------------------------------

      ! initialize 2D bulk fields AND 3D abl data
      CALL sbc_blk_init

      ! Initialize the time index for now time (nt_n) and after time (nt_a)
      nt_n = 1; nt_a = 2

      ! initialize ABL from data or restart
      IF( ln_rstart_abl ) THEN
         CALL abl_rst_read
      ELSE
         CALL fld_read( nit000, nn_fsbc, sf ) ! input fields provided at the first time-step

          u_abl(:,:,:,nt_n      ) = sf(jp_wndi)%fnow(:,:,:)
          v_abl(:,:,:,nt_n      ) = sf(jp_wndj)%fnow(:,:,:)
         tq_abl(:,:,:,nt_n,jp_ta) = sf(jp_tair)%fnow(:,:,:)
         tq_abl(:,:,:,nt_n,jp_qa) = sf(jp_humi)%fnow(:,:,:)

         tke_abl(:,:,:,nt_n     ) = tke_min
         avm_abl(:,:,:          ) = avm_bak
         avt_abl(:,:,:          ) = avt_bak
         pblh   (:,:            ) = ghw_abl( 3 )  !<-- assume that the pbl contains 3 grid points
         u_abl  (:,:,:,nt_a     ) = 0._wp
         v_abl  (:,:,:,nt_a     ) = 0._wp
         tq_abl (:,:,:,nt_a,:   ) = 0._wp
         tke_abl(:,:,:,nt_a     ) = 0._wp

         mxlm_abl(:,:,:         ) = mxl_min
         mxld_abl(:,:,:         ) = mxl_min
      ENDIF

   END SUBROUTINE sbc_abl_init


   SUBROUTINE sbc_abl( kt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE sbc_abl  ***
      !!
      !! ** Purpose :   provide the momentum, heat and freshwater fluxes at
      !!      the ocean surface from an ABL calculation at each oceanic time step
      !!
      !! ** Method  :
      !!              - Pre-compute part of turbulent fluxes in blk_oce_1
      !!              - Perform 1 time-step of the ABL model
      !!              - Finalize flux computation in blk_oce_2
      !!
      !! ** Outputs : - utau    : i-component of the stress at U-point  (N/m2)
      !!              - vtau    : j-component of the stress at V-point  (N/m2)
      !!              - taum    : Wind stress module at T-point         (N/m2)
      !!              - wndm    : Wind speed module at T-point          (m/s)
      !!              - qsr     : Solar heat flux over the ocean        (W/m2)
      !!              - qns     : Non Solar heat flux over the ocean    (W/m2)
      !!              - emp     : evaporation minus precipitation       (kg/m2/s)
      !!
      !!---------------------------------------------------------------------
      INTEGER ,         INTENT(in) ::   kt   ! ocean time step
      !!
      REAL(wp), DIMENSION(jpi,jpj) ::   zssq, zcd_du, zsen, zlat, zevp
#if defined key_si3
      REAL(wp), DIMENSION(jpi,jpj) ::   zssqi, zcd_dui, zseni, zevpi
#endif
      INTEGER                      ::   jbak, jbak_dta, ji, jj
      !!---------------------------------------------------------------------
      !
      !!-------------------------------------------------------------------------------------------
      !! 1 - Read Atmospheric 3D data for large-scale forcing
      !!-------------------------------------------------------------------------------------------

      CALL fld_read( kt, nn_fsbc, sf )             ! input fields provided at the current time-step

      IF( MOD( kt - 1, nn_fsbc ) == 0 ) THEN

         !!-------------------------------------------------------------------------------------------
         !! 2 - Compute Cd x ||U||, Ch x ||U||, Ce x ||U||, and SSQ using now fields
         !!-------------------------------------------------------------------------------------------

         CALL blk_oce_1( kt,  u_abl(:,:,2,nt_n      ),  v_abl(:,:,2,nt_n      ),   &   !   <<= in
            &                tq_abl(:,:,2,nt_n,jp_ta), tq_abl(:,:,2,nt_n,jp_qa),   &   !   <<= in
            &                sf(jp_slp )%fnow(:,:,1) , sst_m, ssu_m, ssv_m     ,   &   !   <<= in
            &                sf(jp_uoatm)%fnow(:,:,1), sf(jp_voatm)%fnow(:,:,1),   &   !   <<= in
            &                sf(jp_qsr )%fnow(:,:,1) , sf(jp_qlw )%fnow(:,:,1) ,   &   !   <<= in
            &                tsk_m, zssq, zcd_du, zsen, zlat, zevp                 )   !   =>> out

#if defined key_si3
         CALL blk_ice_1(  u_abl(:,:,2,nt_n      ),  v_abl(:,:,2,nt_n      ),    &   !   <<= in
            &            tq_abl(:,:,2,nt_n,jp_ta), tq_abl(:,:,2,nt_n,jp_qa),    &   !   <<= in
            &            sf(jp_slp)%fnow(:,:,1)  ,  u_ice, v_ice, tm_su    ,    &   !   <<= in
            &            pseni=zseni, pevpi=zevpi, pssqi=zssqi, pcd_dui=zcd_dui )   !   <<= out
#endif

         !!-------------------------------------------------------------------------------------------
         !! 3 - Advance ABL variables from now (n) to after (n+1)
         !!-------------------------------------------------------------------------------------------

         CALL abl_stp( kt, tsk_m, ssu_m, ssv_m, zssq,                          &   !   <<= in
            &              sf(jp_wndi)%fnow(:,:,:), sf(jp_wndj)%fnow(:,:,:),   &   !   <<= in
            &              sf(jp_tair)%fnow(:,:,:), sf(jp_humi)%fnow(:,:,:),   &   !   <<= in
            &              sf(jp_slp )%fnow(:,:,1),                            &   !   <<= in
            &              sf(jp_hpgi)%fnow(:,:,:), sf(jp_hpgj)%fnow(:,:,:),   &   !   <<= in
            &              zcd_du, zsen, zevp,                                 &   !   <=> in/out
            &              zlat, wndm, utau, vtau, taum                        &   !   =>> out
#if defined key_si3
            &            , tm_su, u_ice, v_ice, zssqi, zcd_dui                 &   !   <<= in
            &            , zseni, zevpi, wndm_ice, ato_i                       &   !   <<= in
            &            , utau_ice, vtau_ice                                  &   !   =>> out
#endif
            &                                                                  )

         !!-------------------------------------------------------------------------------------------
         !! 4 - Finalize flux computation using ABL variables at (n+1), nt_n corresponds to (n+1) since
         !!                                                                time swap is done in abl_stp
         !!-------------------------------------------------------------------------------------------

         CALL blk_oce_2( tq_abl(:,:,2,nt_n,jp_ta), sf(jp_qlw )%fnow(:,:,1),   &
            &            sf(jp_prec)%fnow(:,:,1) , sf(jp_snow)%fnow(:,:,1),   &
            &            tsk_m, zsen, zlat, zevp                              )

         CALL abl_rst_opn( kt )                       ! Open abl restart file (if necessary)
         IF( lrst_abl ) CALL abl_rst_write( kt )      ! -- abl restart file

#if defined key_si3
         ! Avoid a USE abl in icesbc module
         theta_air_zt = tq_abl(:,:,2,nt_n,jp_ta) ;   q_air_zt = tq_abl(:,:,2,nt_n,jp_qa)
#endif
      END IF

   END SUBROUTINE sbc_abl

   !!======================================================================
END MODULE sbcabl
