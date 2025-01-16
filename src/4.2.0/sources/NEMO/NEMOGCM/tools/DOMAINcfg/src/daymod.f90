MODULE daymod
   !!======================================================================
   !!                       ***  MODULE  daymod  ***
   !! Ocean :   management of the model calendar
   !!=====================================================================
   !! History :  OPA  ! 1994-09  (M. Pontaud M. Imbard)  Original code
   !!                 ! 1997-03  (O. Marti)
   !!                 ! 1997-05  (G. Madec)
   !!                 ! 1997-08  (M. Imbard)
   !!   NEMO     1.0  ! 2003-09  (G. Madec)  F90 + nyear, nmonth, nday
   !!                 ! 2004-01  (A.M. Treguier) new calculation based on adatrj
   !!                 ! 2006-08  (G. Madec)  surface module major update
   !!                 ! 2015-11  (D. Lea) Allow non-zero initial time of day
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   day        : calendar
   !!----------------------------------------------------------------------
   !!                    ----------- WARNING -----------
   !!                    -------------------------------
   !!   sbcmod assume that the time step is dividing the number of second of 
   !!   in a day, i.e. ===> MOD( rday, rdt ) == 0 
   !!   except when user defined forcing is used (see sbcmod.F90)
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space and time domain
   USE phycst         ! physical constants
   USE ioipsl  , ONLY :   ymds2ju      ! for calendar
   !
   USE in_out_manager ! I/O manager
   USE iom            !

   IMPLICIT NONE
   PRIVATE

   PUBLIC   day        ! called by step.F90
   PUBLIC   day_mth    ! Needed by TAM

   INTEGER, PUBLIC ::   nsecd, nsecd05, ndt, ndt05   !: (PUBLIC for TAM)

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: daymod.F90 10068 2018-08-28 14:09:04Z nicolasmartin $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE day_mth
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE day_init  ***
      !!
      !! ** Purpose :   calendar values related to the months
      !!
      !! ** Action  : - nmonth_len    : length in days of the months of the current year
      !!              - nyear_len     : length in days of the previous/current year
      !!              - nmonth_half   : second since the beginning of the year and the halft of the months
      !!              - nmonth_end    : second since the beginning of the year and the end of the months
      !!----------------------------------------------------------------------
      INTEGER  ::   jm               ! dummy loop indice
      !!----------------------------------------------------------------------

      ! length of the month of the current year (from nleapy, read in namelist)
      IF ( nleapy < 2 ) THEN
         nmonth_len(:) = (/ 31, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31 /)
         nyear_len(:) = 365
         IF ( nleapy == 1 ) THEN   ! we are using calandar with leap years
            IF ( MOD(nyear-1, 4) == 0 .AND. ( MOD(nyear-1, 400) == 0 .OR. MOD(nyear-1, 100) /= 0 ) ) THEN
               nyear_len(0)  = 366
            ENDIF
            IF ( MOD(nyear  , 4) == 0 .AND. ( MOD(nyear  , 400) == 0 .OR. MOD(nyear  , 100) /= 0 ) ) THEN
               nmonth_len(2) = 29
               nyear_len(1)  = 366
            ENDIF
            IF ( MOD(nyear+1, 4) == 0 .AND. ( MOD(nyear+1, 400) == 0 .OR. MOD(nyear+1, 100) /= 0 ) ) THEN
               nyear_len(2)  = 366
            ENDIF
         ENDIF
      ELSE
         nmonth_len(:) = nleapy   ! all months with nleapy days per year
         nyear_len(:) = 12 * nleapy
      ENDIF

      ! half month in second since the begining of the year:
      ! time since Jan 1st   0     1     2    ...    11    12    13
      !          ---------*--|--*--|--*--| ... |--*--|--*--|--*--|--------------------------------------
      !                 <---> <---> <--->  ...  <---> <---> <--->
      ! month number      0     1     2    ...    11    12    13
      !
      ! nmonth_half(jm) = rday * REAL( 0.5 * nmonth_len(jm) + SUM(nmonth_len(1:jm-1)) )
      nmonth_half(0) = - nsecd05 * nmonth_len(0)
      DO jm = 1, 13
         nmonth_half(jm) = nmonth_half(jm-1) + nsecd05 * ( nmonth_len(jm-1) + nmonth_len(jm) )
      END DO

      nmonth_end(0) = 0
      DO jm = 1, 13
         nmonth_end(jm) = nmonth_end(jm-1) + nsecd * nmonth_len(jm)
      END DO
      !
   END SUBROUTINE


   SUBROUTINE day( kt )
      !!----------------------------------------------------------------------
      !!                      ***  ROUTINE day  ***
      !!
      !! ** Purpose :   Compute the date with a day iteration IF necessary.
      !!
      !! ** Method  : - ???
      !!
      !! ** Action  : - nyear     : current year
      !!              - nmonth    : current month of the year nyear
      !!              - nday      : current day of the month nmonth
      !!              - nday_year : current day of the year nyear
      !!              - ndastp    : = nyear*10000 + nmonth*100 + nday
      !!              - adatrj    : date in days since the beginning of the run
      !!              - nsec_year : current time of the year (in second since 00h, jan 1st)
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt        ! ocean time-step indices
      !
      CHARACTER (len=25) ::   charout
      REAL(wp)           ::   zprec      ! fraction of day corresponding to 0.1 second
      !!----------------------------------------------------------------------
      !
      zprec = 0.1 / rday
      !                                                 ! New time-step
      nsec_year  = nsec_year  + ndt
      nsec_month = nsec_month + ndt
      nsec_week  = nsec_week  + ndt
      nsec_day   = nsec_day   + ndt
      adatrj  = adatrj  + rdt / rday
      fjulday = fjulday + rdt / rday
      IF( ABS(fjulday - REAL(NINT(fjulday),wp)) < zprec )   fjulday = REAL(NINT(fjulday),wp)   ! avoid truncation error
      IF( ABS(adatrj  - REAL(NINT(adatrj ),wp)) < zprec )   adatrj  = REAL(NINT(adatrj ),wp)   ! avoid truncation error

      IF( nsec_day > nsecd ) THEN                       ! New day
         !
         nday      = nday + 1
         nday_year = nday_year + 1
         nsec_day  = ndt05
         !
         IF( nday == nmonth_len(nmonth) + 1 ) THEN      ! New month
            nday   = 1
            nmonth = nmonth + 1
            nsec_month = ndt05
            IF( nmonth == 13 ) THEN                     ! New year
               nyear     = nyear + 1
               nmonth    = 1
               nday_year = 1
               nsec_year = ndt05
               nsec1jan000 = nsec1jan000 + nsecd * nyear_len(1)
               IF( nleapy == 1 )   CALL day_mth
            ENDIF
         ENDIF
         !
         ndastp = nyear * 10000 + nmonth * 100 + nday   ! New date
         !
         !compute first day of the year in julian days
         CALL ymds2ju( nyear, 01, 01, 0.0, fjulstartyear )
         !
         IF(lwp) WRITE(numout,'(a,i8,a,i4.4,a,i2.2,a,i2.2,a,i3.3)') '======>> time-step =', kt,   &
              &   '      New day, DATE Y/M/D = ', nyear, '/', nmonth, '/', nday, '      nday_year = ', nday_year
         IF(lwp) WRITE(numout,'(a,i8,a,i7,a,i5)') '         nsec_year = ', nsec_year,   &
              &   '   nsec_month = ', nsec_month, '   nsec_day = ', nsec_day, '   nsec_week = ', nsec_week
      ENDIF

      IF( nsec_week > 7*nsecd )   nsec_week = ndt05     ! New week
      !
   END SUBROUTINE day

   !!======================================================================
END MODULE daymod
