MODULE limcons
   !!======================================================================
   !!                   ***  MODULE  limcons  ***
   !! LIM-3 Sea Ice :   conservation check
   !!======================================================================
   !! History :   -   ! Original code from William H. Lipscomb, LANL
   !!            3.0  ! 2004-06  (M. Vancoppenolle)   Energy Conservation 
   !!            3.5  ! 2011-02  (G. Madec)  add mpp considerations
   !!             -   ! 2014-05  (C. Rousset) add lim_cons_hsm
   !!             -   ! 2015-03  (C. Rousset) add lim_cons_final
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3'                                      LIM-3 sea-ice model
   !!----------------------------------------------------------------------
   !!    lim_cons     :   checks whether energy, mass and salt are conserved 
   !!----------------------------------------------------------------------
   USE phycst         ! physical constants
   USE ice            ! LIM-3 variables
   USE dom_ice        ! LIM-3 domain
   USE dom_oce        ! ocean domain
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)  
   USE sbc_oce , ONLY : sfx  ! Surface boundary condition: ocean fields
   USE sbc_ice , ONLY : qevap_ice
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_column_sum
   PUBLIC   lim_column_sum_energy
   PUBLIC   lim_cons_check
   PUBLIC   lim_cons_hsm
   PUBLIC   lim_cons_final

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: limcons.F90 6963 2016-09-30 12:40:04Z clem $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_column_sum( ksum, pin, pout )
      !!-------------------------------------------------------------------
      !!               ***  ROUTINE lim_column_sum ***
      !!
      !! ** Purpose : Compute the sum of xin over nsum categories
      !!
      !! ** Method  : Arithmetics
      !!
      !! ** Action  : Gets xin(ji,jj,jl) and computes xout(ji,jj)
      !!---------------------------------------------------------------------
      INTEGER                   , INTENT(in   ) ::   ksum   ! number of categories/layers
      REAL(wp), DIMENSION(:,:,:), INTENT(in   ) ::   pin    ! input field
      REAL(wp), DIMENSION(:,:)  , INTENT(  out) ::   pout   ! output field
      !
      INTEGER ::   jl   ! dummy loop indices
      !!---------------------------------------------------------------------
      !
      pout(:,:) = pin(:,:,1)
      DO jl = 2, ksum
         pout(:,:) = pout(:,:) + pin(:,:,jl)
      END DO
      !
   END SUBROUTINE lim_column_sum


   SUBROUTINE lim_column_sum_energy( ksum, klay, pin, pout)
      !!-------------------------------------------------------------------
      !!               ***  ROUTINE lim_column_sum_energy ***
      !!
      !! ** Purpose : Compute the sum of xin over nsum categories
      !!              and nlay layers
      !!
      !! ** Method  : Arithmetics
      !!---------------------------------------------------------------------
      INTEGER                                , INTENT(in   ) ::   ksum   !: number of categories
      INTEGER                                , INTENT(in   ) ::   klay   !: number of vertical layers
      REAL(wp), DIMENSION(jpi,jpj,nlay_i,jpl), INTENT(in   ) ::   pin    !: input field
      REAL(wp), DIMENSION(jpi,jpj)           , INTENT(  out) ::   pout   !: output field
      !
      INTEGER ::   jk, jl   ! dummy loop indices
      !!---------------------------------------------------------------------
      !
      pout(:,:) = 0._wp
      DO jl = 1, ksum
         DO jk = 2, klay 
            pout(:,:) = pout(:,:) + pin(:,:,jk,jl)
         END DO
      END DO
      !
   END SUBROUTINE lim_column_sum_energy


   SUBROUTINE lim_cons_check( px1, px2, pmax_err, cd_fieldid )
      !!-------------------------------------------------------------------
      !!               ***  ROUTINE lim_cons_check ***
      !!
      !! ** Purpose : Test the conservation of a certain variable
      !!              For each physical grid cell, check that initial 
      !!              and final values
      !!              of a conserved field are equal to within a small value.
      !!
      !! ** Method  :
      !!---------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:), INTENT(in   ) ::   px1          !: initial field
      REAL(wp), DIMENSION(:,:), INTENT(in   ) ::   px2          !: final field
      REAL(wp)                , INTENT(in   ) ::   pmax_err     !: max allowed error
      CHARACTER(len=15)       , INTENT(in   ) ::   cd_fieldid   !: field identifyer
      !
      INTEGER  ::   ji, jj          ! dummy loop indices
      INTEGER  ::   inb_error       ! number of g.c where there is a cons. error
      LOGICAL  ::   llconserv_err   ! = .true. if conservation check failed
      REAL(wp) ::   zmean_error     ! mean error on error points
      !!---------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*) ' lim_cons_check '
      IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~~~~ '

      llconserv_err = .FALSE.
      inb_error     = 0
      zmean_error   = 0._wp
      IF( MAXVAL( px2(:,:) - px1(:,:) ) > pmax_err )   llconserv_err = .TRUE.

      IF( llconserv_err ) THEN
         DO jj = 1, jpj 
            DO ji = 1, jpi
               IF( ABS( px2(ji,jj) - px1(ji,jj) ) > pmax_err ) THEN
                  inb_error   = inb_error + 1
                  zmean_error = zmean_error + ABS( px2(ji,jj) - px1(ji,jj) )
                  !
                  IF(lwp) THEN
                     WRITE (numout,*) ' ALERTE 99 '
                     WRITE (numout,*) ' Conservation error: ', cd_fieldid
                     WRITE (numout,*) ' Point             : ', ji, jj 
                     WRITE (numout,*) ' lat, lon          : ', gphit(ji,jj), glamt(ji,jj)
                     WRITE (numout,*) ' Initial value     : ', px1(ji,jj)
                     WRITE (numout,*) ' Final value       : ', px2(ji,jj)
                     WRITE (numout,*) ' Difference        : ', px2(ji,jj) - px1(ji,jj)
                  ENDIF
               ENDIF
            END DO
         END DO
         !
      ENDIF
      IF(lk_mpp)   CALL mpp_sum( inb_error   )
      IF(lk_mpp)   CALL mpp_sum( zmean_error )
      !
      IF( inb_error > 0 .AND. lwp ) THEN
         zmean_error = zmean_error / REAL( inb_error, wp )
         WRITE(numout,*) ' Conservation check for : ', cd_fieldid
         WRITE(numout,*) ' Number of error points : ', inb_error
         WRITE(numout,*) ' Mean error on these pts: ', zmean_error
      ENDIF
      !
   END SUBROUTINE lim_cons_check


   SUBROUTINE lim_cons_hsm( icount, cd_routine, zvi_b, zsmv_b, zei_b, zfw_b, zfs_b, zft_b )
      !!--------------------------------------------------------------------------------------------------------
      !!                                        ***  ROUTINE lim_cons_hsm ***
      !!
      !! ** Purpose : Test the conservation of heat, salt and mass for each ice routine
      !!                     + test if ice concentration and volume are > 0
      !!
      !! ** Method  : This is an online diagnostics which can be activated with ln_limdiahsb=true
      !!              It prints in ocean.output if there is a violation of conservation at each time-step
      !!              The thresholds (zv_sill, zs_sill, zh_sill) which determine violations are set to
      !!              a minimum of 1 mm of ice (over the ice area) that is lost/gained spuriously during 100 years.
      !!              For salt and heat thresholds, ice is considered to have a salinity of 10 
      !!              and a heat content of 3e5 J/kg (=latent heat of fusion) 
      !!--------------------------------------------------------------------------------------------------------
      INTEGER         , INTENT(in)    :: icount        ! determine wether this is the beggining of the routine (0) or the end (1)
      CHARACTER(len=*), INTENT(in)    :: cd_routine    ! name of the routine
      REAL(wp)        , INTENT(inout) :: zvi_b, zsmv_b, zei_b, zfs_b, zfw_b, zft_b 
      REAL(wp)                        :: zvi,   zsmv,   zei,   zfs,   zfw,   zft
      REAL(wp)                        :: zvmin, zamin, zamax 
      REAL(wp)                        :: zvtrp, zetrp
      REAL(wp)                        :: zarea, zv_sill, zs_sill, zh_sill
      REAL(wp), PARAMETER             :: zconv = 1.e-9 ! convert W to GW and kg to Mt

      IF( icount == 0 ) THEN

         ! salt flux
         zfs_b  = glob_sum(  ( sfx_bri(:,:) + sfx_bog(:,:) + sfx_bom(:,:) + sfx_sum(:,:) + sfx_sni(:,:) +  &
            &                  sfx_opw(:,:) + sfx_res(:,:) + sfx_dyn(:,:) + sfx_sub(:,:)                   &
            &                ) *  e12t(:,:) * tmask(:,:,1) * zconv )

         ! water flux
         zfw_b  = glob_sum( -( wfx_bog(:,:) + wfx_bom(:,:) + wfx_sum(:,:) + wfx_sni(:,:) + wfx_opw(:,:) +  &
            &                  wfx_res(:,:) + wfx_dyn(:,:) + wfx_snw(:,:) + wfx_sub(:,:) + wfx_spr(:,:)    &
            &                ) *  e12t(:,:) * tmask(:,:,1) * zconv )

         ! heat flux
         zft_b  = glob_sum(  ( hfx_sum(:,:) + hfx_bom(:,:) + hfx_bog(:,:) + hfx_dif(:,:) + hfx_opw(:,:) + hfx_snw(:,:)  & 
            &                - hfx_thd(:,:) - hfx_dyn(:,:) - hfx_res(:,:) - hfx_sub(:,:) - hfx_spr(:,:)   &
            &                ) *  e12t(:,:) * tmask(:,:,1) * zconv )

         zvi_b  = glob_sum( SUM( v_i * rhoic + v_s * rhosn, dim=3 ) * e12t * tmask(:,:,1) * zconv )

         zsmv_b = glob_sum( SUM( smv_i * rhoic            , dim=3 ) * e12t * tmask(:,:,1) * zconv )

         zei_b  = glob_sum( ( SUM( SUM( e_i(:,:,1:nlay_i,:), dim=4 ), dim=3 ) +  &
            &                 SUM( SUM( e_s(:,:,1:nlay_s,:), dim=4 ), dim=3 )    &
                            ) * e12t * tmask(:,:,1) * zconv )

      ELSEIF( icount == 1 ) THEN

         ! salt flux
         zfs  = glob_sum(  ( sfx_bri(:,:) + sfx_bog(:,:) + sfx_bom(:,:) + sfx_sum(:,:) + sfx_sni(:,:) +  &
            &                sfx_opw(:,:) + sfx_res(:,:) + sfx_dyn(:,:) + sfx_sub(:,:)                   & 
            &              ) * e12t(:,:) * tmask(:,:,1) * zconv ) - zfs_b

         ! water flux
         zfw  = glob_sum( -( wfx_bog(:,:) + wfx_bom(:,:) + wfx_sum(:,:) + wfx_sni(:,:) + wfx_opw(:,:) +  &
            &                wfx_res(:,:) + wfx_dyn(:,:) + wfx_snw(:,:) + wfx_sub(:,:) + wfx_spr(:,:)    &
            &              ) * e12t(:,:) * tmask(:,:,1) * zconv ) - zfw_b

         ! heat flux
         zft  = glob_sum(  ( hfx_sum(:,:) + hfx_bom(:,:) + hfx_bog(:,:) + hfx_dif(:,:) + hfx_opw(:,:) + hfx_snw(:,:)  & 
            &              - hfx_thd(:,:) - hfx_dyn(:,:) - hfx_res(:,:) - hfx_sub(:,:) - hfx_spr(:,:)   &
            &              ) * e12t(:,:) * tmask(:,:,1) * zconv ) - zft_b
 
         ! outputs
         zvi  = ( ( glob_sum( SUM( v_i * rhoic + v_s * rhosn, dim=3 )  &
            &                    * e12t * tmask(:,:,1) * zconv ) - zvi_b ) * r1_rdtice - zfw ) * rday

         zsmv = ( ( glob_sum( SUM( smv_i * rhoic            , dim=3 )  &
            &                    * e12t * tmask(:,:,1) * zconv ) - zsmv_b ) * r1_rdtice + zfs ) * rday

         zei  =   glob_sum( ( SUM( SUM( e_i(:,:,1:nlay_i,:), dim=4 ), dim=3 ) +  &
            &                 SUM( SUM( e_s(:,:,1:nlay_s,:), dim=4 ), dim=3 )    &
            &                ) * e12t * tmask(:,:,1) * zconv ) * r1_rdtice - zei_b * r1_rdtice + zft

         ! zvtrp and zetrp must be close to 0 if the advection scheme is conservative
         zvtrp = glob_sum( ( diag_trp_vi * rhoic + diag_trp_vs * rhosn ) * e12t * tmask(:,:,1) * zconv ) * rday 
         zetrp = glob_sum( ( diag_trp_ei         + diag_trp_es         ) * e12t * tmask(:,:,1) * zconv )

         zvmin = glob_min( v_i )
         zamax = glob_max( SUM( a_i, dim=3 ) )
         zamin = glob_min( a_i )

         ! set threshold values and calculate the ice area (+epsi10 to set a threshold > 0 when there is no ice) 
         zarea   = glob_sum( SUM( a_i + epsi10, dim=3 ) * e12t * zconv ) ! in 1.e9 m2
         zv_sill = zarea * 2.5e-5
         zs_sill = zarea * 25.e-5
         zh_sill = zarea * 10.e-5

         IF(lwp) THEN
            IF ( ABS( zvi  ) > zv_sill ) WRITE(numout,*) 'violation volume [Mt/day]     (',cd_routine,') = ',zvi
            IF ( ABS( zsmv ) > zs_sill ) WRITE(numout,*) 'violation saline [psu*Mt/day] (',cd_routine,') = ',zsmv
            IF ( ABS( zei  ) > zh_sill ) WRITE(numout,*) 'violation enthalpy [GW]       (',cd_routine,') = ',zei
            IF ( ABS(zvtrp ) > zv_sill .AND. cd_routine == 'limtrp' ) THEN
                                         WRITE(numout,*) 'violation vtrp [Mt/day]       (',cd_routine,') = ',zvtrp
                                         WRITE(numout,*) 'violation etrp [GW]           (',cd_routine,') = ',zetrp
            ENDIF
            IF (     zvmin   < -epsi10 ) WRITE(numout,*) 'violation v_i<0  [m]          (',cd_routine,') = ',zvmin
            IF (     zamax   > MAX( rn_amax_n, rn_amax_s ) + epsi10 .AND. &
               &                         cd_routine /= 'limtrp' .AND. cd_routine /= 'limitd_me' ) THEN
                                         WRITE(numout,*) 'violation a_i>amax            (',cd_routine,') = ',zamax
            ENDIF
            IF (      zamin  < -epsi10 ) WRITE(numout,*) 'violation a_i<0               (',cd_routine,') = ',zamin
         ENDIF

      ENDIF

   END SUBROUTINE lim_cons_hsm

   SUBROUTINE lim_cons_final( cd_routine )
      !!---------------------------------------------------------------------------------------------------------
      !!                                   ***  ROUTINE lim_cons_final ***
      !!
      !! ** Purpose : Test the conservation of heat, salt and mass at the end of each ice time-step
      !!
      !! ** Method  : This is an online diagnostics which can be activated with ln_limdiahsb=true
      !!              It prints in ocean.output if there is a violation of conservation at each time-step
      !!              The thresholds (zv_sill, zs_sill, zh_sill) which determine the violation are set to
      !!              a minimum of 1 mm of ice (over the ice area) that is lost/gained spuriously during 100 years.
      !!              For salt and heat thresholds, ice is considered to have a salinity of 10 
      !!              and a heat content of 3e5 J/kg (=latent heat of fusion) 
      !!--------------------------------------------------------------------------------------------------------
      CHARACTER(len=*), INTENT(in)    :: cd_routine    ! name of the routine
      REAL(wp)                        :: zhfx, zsfx, zvfx
      REAL(wp)                        :: zarea, zv_sill, zs_sill, zh_sill
      REAL(wp), PARAMETER             :: zconv = 1.e-9 ! convert W to GW and kg to Mt

#if ! defined key_bdy
      ! heat flux
      zhfx  = glob_sum( ( hfx_in - hfx_out - diag_heat - diag_trp_ei - diag_trp_es  &
      !  &              - SUM( qevap_ice * a_i_b, dim=3 )                           & !!clem: I think this line must be commented (but need check)
         &              ) * e12t * tmask(:,:,1) * zconv ) 
      ! salt flux
      zsfx  = glob_sum( ( sfx + diag_smvi ) * e12t * tmask(:,:,1) * zconv ) * rday
      ! water flux
      zvfx  = glob_sum( ( wfx_ice + wfx_snw + wfx_spr + wfx_sub + diag_vice + diag_vsnw ) * e12t * tmask(:,:,1) * zconv ) * rday

      ! set threshold values and calculate the ice area (+epsi10 to set a threshold > 0 when there is no ice) 
      zarea   = glob_sum( SUM( a_i + epsi10, dim=3 ) * e12t * zconv ) ! in 1.e9 m2
      zv_sill = zarea * 2.5e-5
      zs_sill = zarea * 25.e-5
      zh_sill = zarea * 10.e-5

      IF( ABS( zvfx ) > zv_sill ) WRITE(numout,*) 'violation vfx    [Mt/day]       (',cd_routine,')  = ',(zvfx)
      IF( ABS( zsfx ) > zs_sill ) WRITE(numout,*) 'violation sfx    [psu*Mt/day]   (',cd_routine,')  = ',(zsfx)
      IF( ABS( zhfx ) > zh_sill ) WRITE(numout,*) 'violation hfx    [GW]           (',cd_routine,')  = ',(zhfx)
#endif

   END SUBROUTINE lim_cons_final

#else
   !!----------------------------------------------------------------------
   !!   Default option         Empty module            NO LIM sea-ice model
   !!----------------------------------------------------------------------
#endif
   !!======================================================================
END MODULE limcons
