MODULE sbcdcy
   !!======================================================================
   !!                    ***  MODULE  sbcdcy  ***
   !! Ocean forcing:  compute the diurnal cycle
   !!======================================================================
   !! History : OPA  !  2005-02  (D. Bernie)  Original code
   !!   NEMO    2.0  !  2006-02  (S. Masson, G. Madec)  adaptation to NEMO
   !!           3.1  !  2009-07  (J.M. Molines)  adaptation to v3.1
   !!           4.*  !  2019-10  (L. Brodeau)  nothing really new, but the routine
   !!                ! "sbc_dcy_param" has been extracted from old function "sbc_dcy"
   !!                ! => this allows the warm-layer param of COARE3* to know the time
   !!                ! of dawn and dusk even if "ln_dm2dc=.false." (rdawn_dcy & rdusk_dcy
   !!                ! are now public)
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!  sbc_dcy : solar flux at kt from daily mean, taking diurnal cycle into account
   !!----------------------------------------------------------------------
   USE oce              ! ocean dynamics and tracers
   USE phycst           ! ocean physics
   USE dom_oce          ! ocean space and time domain
   USE sbc_oce          ! Surface boundary condition: ocean fields
   !
   USE in_out_manager   ! I/O manager
   USE lib_mpp          ! MPP library

   IMPLICIT NONE
   PRIVATE

   INTEGER, PUBLIC ::   nday_qsr   !: day when parameters were computed

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   raa , rbb  , rcc  , rab     ! diurnal cycle parameters
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   rtmd, rscal   !    -      -       -
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:), PUBLIC :: rdawn_dcy, rdusk_dcy   !    -      -       -

   PUBLIC   sbc_dcy        ! routine called by sbc
   PUBLIC   sbc_dcy_param  ! routine used here and called by warm-layer parameterization (sbcblk_skin_coare*)

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: sbcdcy.F90 13483 2020-09-17 08:24:00Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION sbc_dcy_alloc()
      !!----------------------------------------------------------------------
      !!                ***  FUNCTION sbc_dcy_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( raa (jpi,jpj) , rbb  (jpi,jpj) , rcc  (jpi,jpj) , rab  (jpi,jpj) ,     &
         &      rtmd(jpi,jpj) , rdawn_dcy(jpi,jpj) , rdusk_dcy(jpi,jpj) , rscal(jpi,jpj) , STAT=sbc_dcy_alloc )
      !
      CALL mpp_sum ( 'sbcdcy', sbc_dcy_alloc )
      IF( sbc_dcy_alloc /= 0 )   CALL ctl_stop( 'STOP', 'sbc_dcy_alloc: failed to allocate arrays' )
   END FUNCTION sbc_dcy_alloc


   FUNCTION sbc_dcy( pqsrin, l_mask ) RESULT( zqsrout )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE sbc_dcy  ***
      !!
      !! ** Purpose : introduce a diurnal cycle of qsr from daily values
      !!
      !! ** Method  : see Appendix A of Bernie et al. 2007.
      !!
      !! ** Action  : redistribute daily QSR on each time step following the diurnal cycle
      !!
      !! reference  : Bernie, DJ, E Guilyardi, G Madec, JM Slingo, and SJ Woolnough, 2007
      !!              Impact of resolving the diurnal cycle in an ocean--atmosphere GCM.
      !!              Part 1: a diurnally forced OGCM. Climate Dynamics 29:6, 575-590.
      !!----------------------------------------------------------------------
      LOGICAL , OPTIONAL          , INTENT(in) ::   l_mask    ! use the routine for night mask computation
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) ::   pqsrin    ! input daily QSR flux
      REAL(wp), DIMENSION(jpi,jpj)             ::   zqsrout   ! output QSR flux with diurnal cycle
      !!
      INTEGER  ::   ji, jj                                       ! dummy loop indices
      INTEGER, DIMENSION(jpi,jpj) :: imask_night ! night mask
      REAL(wp) ::   zlo, zup, zlousd, zupusd
      REAL(wp) ::   ztmp, ztmp1, ztmp2
      REAL(wp) ::   ztmpm, ztmpm1, ztmpm2
      !!---------------------------------------------------------------------
      !
      ! Initialization
      ! --------------
      ! When are we during the day (from 0 to 1)
      zlo = ( REAL(nsec_day, wp) - 0.5_wp * rn_Dt ) / rday
      zup = zlo + ( REAL(nn_fsbc, wp)     * rn_Dt ) / rday
      !
      IF( nday_qsr == -1 ) THEN       ! first time step only
         IF(lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) 'sbc_dcy : introduce diurnal cycle from daily mean qsr'
            WRITE(numout,*) '~~~~~~~'
            WRITE(numout,*)
         ENDIF
      ENDIF

      ! Setting parameters for each new day:
      CALL sbc_dcy_param()

      !CALL iom_put( "rdusk_dcy", rdusk_dcy(:,:)*tmask(:,:,1) ) !LB
      !CALL iom_put( "rdawn_dcy", rdawn_dcy(:,:)*tmask(:,:,1) ) !LB
      !CALL iom_put( "rscal_dcy", rscal(:,:)*tmask(:,:,1) ) !LB


      !     3. update qsr with the diurnal cycle
      !     ------------------------------------

      imask_night(:,:) = 0
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         ztmpm = 0._wp
         IF( ABS(rab(ji,jj)) < 1. ) THEN         ! day duration is less than 24h
            !
            IF( rdawn_dcy(ji,jj) < rdusk_dcy(ji,jj) ) THEN       ! day time in one part
               zlousd = MAX(zlo, rdawn_dcy(ji,jj))
               zlousd = MIN(zlousd, zup)
               zupusd = MIN(zup, rdusk_dcy(ji,jj))
               zupusd = MAX(zupusd, zlo)
               ztmp = fintegral(zlousd, zupusd, raa(ji,jj), rbb(ji,jj), rcc(ji,jj))
               zqsrout(ji,jj) = pqsrin(ji,jj) * ztmp * rscal(ji,jj)
               ztmpm = zupusd - zlousd
               IF( ztmpm .EQ. 0 ) imask_night(ji,jj) = 1
               !
            ELSE                                         ! day time in two parts
               zlousd = MIN(zlo, rdusk_dcy(ji,jj))
               zupusd = MIN(zup, rdusk_dcy(ji,jj))
               ztmp1 = fintegral(zlousd, zupusd, raa(ji,jj), rbb(ji,jj), rcc(ji,jj))
               ztmpm1=zupusd-zlousd
               zlousd = MAX(zlo, rdawn_dcy(ji,jj))
               zupusd = MAX(zup, rdawn_dcy(ji,jj))
               ztmp2 = fintegral(zlousd, zupusd, raa(ji,jj), rbb(ji,jj), rcc(ji,jj))
               ztmpm2 =zupusd-zlousd
               ztmp = ztmp1 + ztmp2
               ztmpm = ztmpm1 + ztmpm2
               zqsrout(ji,jj) = pqsrin(ji,jj) * ztmp * rscal(ji,jj)
               IF(ztmpm .EQ. 0.) imask_night(ji,jj) = 1
            ENDIF
         ELSE                                   ! 24h light or 24h night
            !
            IF( raa(ji,jj) > rbb(ji,jj) ) THEN           ! 24h day
               ztmp = fintegral(zlo, zup, raa(ji,jj), rbb(ji,jj), rcc(ji,jj))
               zqsrout(ji,jj) = pqsrin(ji,jj) * ztmp * rscal(ji,jj)
               imask_night(ji,jj) = 0
               !
            ELSE                                         ! No day
               zqsrout(ji,jj) = 0.0_wp
               imask_night(ji,jj) = 1
            ENDIF
         ENDIF
      END_2D
      !
      IF( PRESENT(l_mask) .AND. l_mask ) THEN
         zqsrout(:,:) = float(imask_night(:,:))
      ENDIF
      !
   END FUNCTION sbc_dcy


   SUBROUTINE sbc_dcy_param( )
      !!
      INTEGER  ::   ji, jj                                       ! dummy loop indices
      !INTEGER, DIMENSION(jpi,jpj) :: imask_night ! night mask
      REAL(wp) ::   zdsws, zdecrad, ztx, zsin, zcos
      REAL(wp) ::   ztmp, ztest
      !---------------------------statement functions------------------------
      !
      IF( nday_qsr == -1 ) THEN       ! first time step only
         ! allocate sbcdcy arrays
         IF( sbc_dcy_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'sbc_dcy_alloc : unable to allocate arrays' )
         ! Compute rcc needed to compute the time integral of the diurnal cycle
         rcc(:,:) = rad * glamt(:,:) - rpi
         ! time of midday
         rtmd(:,:) = 0.5_wp - glamt(:,:) / 360._wp
         rtmd(:,:) = MOD( (rtmd(:,:) + 1._wp) , 1._wp)
      ENDIF

      ! If this is a new day, we have to update the dawn, dusk and scaling function
      !----------------------

      !     2.1 dawn and dusk

      ! nday is the number of days since the beginning of the current month
      IF( nday_qsr /= nday ) THEN
         ! save the day of the year and the daily mean of qsr
         nday_qsr = nday
         ! number of days since the previous winter solstice (supposed to be always 21 December)
         zdsws = REAL(11 + nday_year, wp)
         ! declination of the earths orbit
         zdecrad = (-23.5_wp * rad) * COS( zdsws * 2._wp*rpi / REAL(nyear_len(1),wp) )
         ! Compute A and B needed to compute the time integral of the diurnal cycle

         zsin = SIN( zdecrad )   ;   zcos = COS( zdecrad )
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            ztmp = rad * gphit(ji,jj)
            raa(ji,jj) = SIN( ztmp ) * zsin
            rbb(ji,jj) = COS( ztmp ) * zcos
         END_2D
         ! Compute the time of dawn and dusk

         ! rab to test if the day time is equal to 0, less than 24h of full day
         rab(:,:) = -raa(:,:) / rbb(:,:)
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            IF( ABS(rab(ji,jj)) < 1._wp ) THEN         ! day duration is less than 24h
               ! When is it night?
               ztx = 1._wp/(2._wp*rpi) * (ACOS(rab(ji,jj)) - rcc(ji,jj))
               ztest = -rbb(ji,jj) * SIN( rcc(ji,jj) + 2._wp*rpi * ztx )
               ! is it dawn or dusk?
               IF( ztest > 0._wp ) THEN
                  rdawn_dcy(ji,jj) = ztx
                  rdusk_dcy(ji,jj) = rtmd(ji,jj) + ( rtmd(ji,jj) - rdawn_dcy(ji,jj) )
               ELSE
                  rdusk_dcy(ji,jj) = ztx
                  rdawn_dcy(ji,jj) = rtmd(ji,jj) - ( rdusk_dcy(ji,jj) - rtmd(ji,jj) )
               ENDIF
            ELSE
               rdawn_dcy(ji,jj) = rtmd(ji,jj) + 0.5_wp
               rdusk_dcy(ji,jj) = rdawn_dcy(ji,jj)
            ENDIF
         END_2D
         rdawn_dcy(:,:) = MOD( (rdawn_dcy(:,:) + 1._wp), 1._wp )
         rdusk_dcy(:,:) = MOD( (rdusk_dcy(:,:) + 1._wp), 1._wp )
         !     2.2 Compute the scaling function:
         !         S* = the inverse of the time integral of the diurnal cycle from dawn to dusk
         !         Avoid possible infinite scaling factor, associated with very short daylight
         !         periods, by ignoring periods less than 1/1000th of a day (ticket #1040)
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            IF( ABS(rab(ji,jj)) < 1._wp ) THEN         ! day duration is less than 24h
               rscal(ji,jj) = 0.0_wp
               IF( rdawn_dcy(ji,jj) < rdusk_dcy(ji,jj) ) THEN      ! day time in one part
                  IF( (rdusk_dcy(ji,jj) - rdawn_dcy(ji,jj) ) .ge. 0.001_wp ) THEN
                     rscal(ji,jj) = fintegral(rdawn_dcy(ji,jj), rdusk_dcy(ji,jj), raa(ji,jj), rbb(ji,jj), rcc(ji,jj))
                     rscal(ji,jj) = 1._wp / rscal(ji,jj)
                  ENDIF
               ELSE                                         ! day time in two parts
                  IF( (rdusk_dcy(ji,jj) + (1._wp - rdawn_dcy(ji,jj)) ) .ge. 0.001_wp ) THEN
                     rscal(ji,jj) = fintegral(0._wp, rdusk_dcy(ji,jj), raa(ji,jj), rbb(ji,jj), rcc(ji,jj))   &
                        &         + fintegral(rdawn_dcy(ji,jj), 1._wp, raa(ji,jj), rbb(ji,jj), rcc(ji,jj))
                     rscal(ji,jj) = 1. / rscal(ji,jj)
                  ENDIF
               ENDIF
            ELSE
               IF( raa(ji,jj) > rbb(ji,jj) ) THEN         ! 24h day
                  rscal(ji,jj) = fintegral(0._wp, 1._wp, raa(ji,jj), rbb(ji,jj), rcc(ji,jj))
                  rscal(ji,jj) = 1._wp / rscal(ji,jj)
               ELSE                                          ! No day
                  rscal(ji,jj) = 0.0_wp
               ENDIF
            ENDIF
         END_2D
         !
         ztmp = rday / ( rn_Dt * REAL(nn_fsbc, wp) )
         rscal(:,:) = rscal(:,:) * ztmp
         !
      ENDIF !IF( nday_qsr /= nday )
      !
   END SUBROUTINE sbc_dcy_param


   FUNCTION fintegral( pt1, pt2, paaa, pbbb, pccc )
      REAL(wp), INTENT(in) :: pt1, pt2, paaa, pbbb, pccc
      REAL(wp) :: fintegral
      fintegral =   paaa * pt2 + 1._wp/(2._wp*rpi) * pbbb * SIN(pccc + 2._wp*rpi*pt2)   &
         &        - paaa * pt1 - 1._wp/(2._wp*rpi) * pbbb * SIN(pccc + 2._wp*rpi*pt1)
   END FUNCTION fintegral

   !!======================================================================
END MODULE sbcdcy
