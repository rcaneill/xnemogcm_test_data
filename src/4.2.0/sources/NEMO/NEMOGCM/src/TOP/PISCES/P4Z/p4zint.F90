MODULE p4zint
   !!=========================================================================
   !!                         ***  MODULE p4zint  ***
   !! TOP :   PISCES interpolation and computation of various accessory fields
   !!=========================================================================
   !! History :   1.0  !  2004-03 (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!----------------------------------------------------------------------
   !!   p4z_int        :  interpolation and computation of various accessory fields
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_int  
   REAL(wp) ::   xksilim = 16.5e-6_wp   ! Half-saturation constant for the Si half-saturation constant computation

#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p4zint.F90 15459 2021-10-29 08:19:18Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_int( kt, Kbb, Kmm )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_int  ***
      !!
      !! ** Purpose :   interpolation and computation of various accessory fields
      !!
      !!---------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt       ! ocean time-step index
      INTEGER, INTENT( in ) ::   Kbb, Kmm ! time level indices
      !
      INTEGER  :: ji, jj, jk              ! dummy loop indices
      REAL(wp) :: zrum, zcodel, zargu, zvar
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p4z_int')
      !
      ! Computation of phyto and zoo metabolic rate
      ! -------------------------------------------
      ! Generic temperature dependence (Eppley, 1972)
      tgfunc (:,:,:) = EXP( 0.0631 * ts(:,:,:,jp_tem,Kmm) )
      ! Temperature dependence of mesozooplankton (Buitenhuis et al. (2005))
      tgfunc2(:,:,:) = EXP( 0.0761 * ts(:,:,:,jp_tem,Kmm) )


      ! Computation of the silicon dependant half saturation  constant for silica uptake
      ! This is based on an old study by Pondaven et al. (1998)
      ! --------------------------------------------------------------------------------
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         zvar = tr(ji,jj,1,jpsil,Kbb) * tr(ji,jj,1,jpsil,Kbb)
         xksimax(ji,jj) = MAX( xksimax(ji,jj), ( 1.+ 7.* zvar / ( xksilim * xksilim + zvar ) ) * 1e-6 )
      END_2D
      !
      ! At the end of each year, the half saturation constant for silica is 
      ! updated as this is based on the highest concentration reached over 
      ! the year
      ! -------------------------------------------------------------------
      IF( nday_year == nyear_len(1) ) THEN
         xksi   (:,:) = xksimax(:,:)
         xksimax(:,:) = 0._wp
      ENDIF
      !
      ! compute the day length depending on latitude and the day
      ! Astronomical parameterization taken from HAMOCC3
      zrum = REAL( nday_year - 80, wp ) / REAL( nyear_len(1), wp )
      zcodel = ASIN(  SIN( zrum * rpi * 2._wp ) * SIN( rad * 23.5_wp )  )

      ! day length in hours
      strn(:,:) = 0.
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         zargu = TAN( zcodel ) * TAN( gphit(ji,jj) * rad )
         zargu = MAX( -1., MIN(  1., zargu ) )
         strn(ji,jj) = MAX( 0.0, 24. - 2. * ACOS( zargu ) / rad / 15. )
      END_2D
      !
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 )
        ! denitrification factor computed from O2 levels
         ! This factor diagnoses below which level of O2 denitrification
         ! is active
         nitrfac(ji,jj,jk) = MAX(  0.e0, 0.4 * ( 6.e-6  - tr(ji,jj,jk,jpoxy,Kbb) )    &
            &                                / ( oxymin + tr(ji,jj,jk,jpoxy,Kbb) )  )
         nitrfac(ji,jj,jk) = MIN( 1., nitrfac(ji,jj,jk) )
         !
         ! redox factor computed from NO3 levels
         ! This factor diagnoses below which level of NO3 additional redox
         ! reactions are taking place.
         nitrfac2(ji,jj,jk) = MAX( 0.e0,       ( 1.E-6 - tr(ji,jj,jk,jpno3,Kbb) )  &
            &                                / ( 1.E-6 + tr(ji,jj,jk,jpno3,Kbb) ) )
         nitrfac2(ji,jj,jk) = MIN( 1., nitrfac2(ji,jj,jk) )
      END_3D
      !
      IF( ln_timing )   CALL timing_stop('p4z_int')
      !
   END SUBROUTINE p4z_int

   !!======================================================================
END MODULE p4zint
