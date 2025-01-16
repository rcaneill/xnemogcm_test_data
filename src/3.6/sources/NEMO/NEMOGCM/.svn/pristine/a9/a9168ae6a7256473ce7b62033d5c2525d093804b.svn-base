!----------------------------------------------------------------------
! NEMO system team, System and Interface for oceanic RElocable Nesting
!----------------------------------------------------------------------
!
! MODULE: date
!
! DESCRIPTION:
!> @brief This module provide the calculation of Julian dates, and
!> do many manipulations with dates.
!>
!> @details
!> Actually we use Modified Julian Dates, with  
!> 17 Nov 1858 at 00:00:00 as origin.<br/>
!>
!>   define type TDATE:<br/>
!> @code
!>   TYPE(TDATE) :: tl_date1
!> @endcode
!>   default date is 17 Nov 1858 at 00:00:00<br/>
!>
!>   to intialise date : <br/>
!>   - from date of the day at 12:00:00 : 
!> @code
!> tl_date1=date_today()
!> @endcode
!>   - from date and time of the day    : 
!> @code
!> tl_date1=date_now()
!> @endcode
!>   - from julian day                  : 
!> @code
!> tl_date1=date_init(dd_jd)
!> @endcode
!>      - dd_jd julian day (double precision) 
!>   - from number of second since julian day origin   : 
!> @code
!> tl_date1=date_init(kd_nsec)
!> @endcode
!>      - kd_nsec number of second (integer 8) 
!>   - from year month day              : 
!> @code
!> tl_date1=date_init(2012,12,10)
!> @endcode
!>   - from string character formatted date  : 
!> @code
!> tl_date1=date_init(cd_fmtdate)
!> @endcode
!>      - cd_fmtdate date in format YYYY-MM-DD hh:mm:ss
!>
!>   to print date in format YYYY-MM-DD hh:mm:ss<br/>
!>   CHARACTER(LEN=lc) :: cl_date<br/>
!> @code
!>   cl_date=date_print(tl_date1)
!>   PRINT *, TRIM(cl_date)
!> @endcode
!>    
!>   to print date in another format (only year, month, day):
!> @code
!>   cl_date=date_print(tl_date1, cd_fmt)
!>   PRINT *, TRIM(cl_date)
!> @endcode
!>       - cd_fmt ouput format (ex: cd_fmt="('y',i0.4,'m',i0.2,'d',i0.2)" ) 
!>
!>   to print day of the week:<br/>
!> @code
!>   PRINT *,"dow ", tl_date1\%i_dow
!> @endcode
!>   to print last day of the month:<br/>
!> @code
!>   PRINT *,"last day ", tl_date1\%i_lday
!> @endcode
!>
!>   to know if year is a leap year:<br/>
!> @code
!>   ll_isleap=date_leapyear(tl_date1)
!> @endcode
!>    - ll_isleap is logical
!>
!>   to compute number of days between two dates:<br/>
!> @code
!>   tl_date2=date_init(2010,12,10)
!>   dl_diff=tl_date1-tl_date2
!> @endcode
!>    - dl_diff is the number of days between date1 and date2 (double precision)
!>
!>   to add or substract nday to a date:<br/>
!> @code
!>   tl_date2=tl_date1+2.
!>   tl_date2=tl_date1-2.6
!> @endcode
!>    - number of day (double precision)
!>
!>   to print julian day:<br/>
!> @code
!>   PRINT *," julian day",tl_date1\%r_jd
!> @endcode
!>
!>   to print CNES julian day (origin 1950-01-01 00:00:00)<br/>
!> @code
!>   PRINT *," CNES julian day",tl_date1\%r_jc
!> @endcode
!>
!>   to create pseudo julian day with origin date_now:<br/>
!> @code
!>   tl_date1=date_init(2012,12,10,td_dateo=date_now())
!> @endcode
!>   @note you erase CNES julian day when doing so<br/> 
!>
!>   to print julian day in seconds:<br/>
!> @code
!>   PRINT *, tl_date1\%k_jdsec
!> @endcode
!>   to print CNES or new julian day in seconds:<br/>
!> @code
!>   PRINT *, tl_date1\%k_jcsec
!> @endcode
!>
!> @author J.Paul
! REVISION HISTORY:
!> @date November, 2013 - Initial Version
!
!> @note This module is based on Perderabo's date calculator (ksh)
!> @note Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
!>
!> @todo
!> - see calendar.f90 and select Gregorian, NoLeap, or D360 calendar
!----------------------------------------------------------------------
MODULE date
   USE global                          ! global variable
   USE kind                            ! F90 kind parameter
   USE fct                             ! basic useful function
   USE logger                          ! log file manager
   IMPLICIT NONE
   ! NOTE_avoid_public_variables_if_possible

   ! type and variable
   PUBLIC :: TDATE              !< date structure

   PRIVATE :: cm_fmtdate        !< date and time format
   PRIVATE :: im_secbyday       !< number of second by day

   ! function and subroutine
   PUBLIC :: date_today         !< return the date of the day at 12:00:00
   PUBLIC :: date_now           !< return the date and time
   PUBLIC :: date_init          !< initialized date structure form julian day or year month day
   PUBLIC :: date_print         !< print the date with format YYYY-MM-DD hh:mm:ss
   PUBLIC :: date_leapyear      !< check if year is a leap year
   PUBLIC :: OPERATOR(-)        !< substract two dates or n days to a date
   PUBLIC :: OPERATOR(+)        !< add n days to a date

   PRIVATE :: date__init_fmtdate ! initialized date structure from character YYYY-MM-DD hh:mm:ss
   PRIVATE :: date__init_jd      ! initialized date structure from julian day
   PRIVATE :: date__init_nsec    ! initialized date structure from number of second since origin of julian day
   PRIVATE :: date__init_ymd     ! initialized date structure from year month day
   PRIVATE :: date__addnday      ! add nday to a date
   PRIVATE :: date__subnday      ! substract nday to a date
   PRIVATE :: date__diffdate     ! compute number of days between two dates 
   PRIVATE :: date__lastday      ! compute last day of the month
   PRIVATE :: date__ymd2jd       ! compute julian day from year month day
   PRIVATE :: date__jd2ymd       ! compute year month day from julian day 
   PRIVATE :: date__jc2jd        ! compute julian day from pseudo julian day
   PRIVATE :: date__jd2jc        ! compute pseudo julian day with new date origin
   PRIVATE :: date__jd2dow       ! compute the day of week from julian day
   PRIVATE :: date__hms2jd       ! compute fraction of a day from hour, minute, second
   PRIVATE :: date__jd2hms       ! compute hour, minute, second from julian fraction
   PRIVATE :: date__check        ! check date in date structure
   PRIVATE :: date__adjust       ! adjust date
   PRIVATE :: date__jd2sec       ! convert julian day in seconds since julian day origin 
   PRIVATE :: date__sec2jd       ! convert seconds since julian day origin in julian day

   TYPE TDATE !< date structure
      INTEGER(i4) :: i_year  = 1858   !< year
      INTEGER(i4) :: i_month = 11     !< month
      INTEGER(i4) :: i_day   = 17     !< day
      INTEGER(i4) :: i_hour  = 0      !< hour
      INTEGER(i4) :: i_min   = 0      !< min
      INTEGER(i4) :: i_sec   = 0      !< sec
      INTEGER(i4) :: i_dow   = 0      !< day of week
      INTEGER(i4) :: i_lday  = 0      !< last day of the month
      REAL(dp)    :: d_jd = 0         !< julian day (origin : 1858/11/17 00:00:00)
      REAL(dp)    :: d_jc = 0         !< CNES julian day or pseudo julian day with new date origin    
      INTEGER(i8) :: k_jdsec  = 0     !< number of seconds since julian day origin
      INTEGER(i8) :: k_jcsec  = 0     !< number of seconds since CNES or pseudo julian day origin
   END TYPE TDATE   

   !  module variable   
   CHARACTER(LEN=lc), PARAMETER :: cm_fmtdate = &  !< date and time format
   &  "(i0.4,'-',i0.2,'-',i0.2,1x,i0.2,':',i0.2,':',i0.2)"

   INTEGER(i4), PARAMETER :: im_secbyday = 86400    !< number of second by day   

   INTERFACE date_init
      MODULE PROCEDURE date__init_jd    ! initialized date structure from julian day
      MODULE PROCEDURE date__init_nsec  ! initialized date structure from number of second since origin of julian day
      MODULE PROCEDURE date__init_ymd   ! initialized date structure from year month day
      MODULE PROCEDURE date__init_fmtdate   ! initialized date structure from character YYYY-MM-DD hh:mm:ss
   END INTERFACE date_init

   INTERFACE OPERATOR(+)
      MODULE PROCEDURE date__addnday   ! add nday to a date
   END INTERFACE

   INTERFACE OPERATOR(-)
      MODULE PROCEDURE date__subnday   ! substract nday to a date
      MODULE PROCEDURE date__diffdate  ! compute number of day between two dates
   END INTERFACE

CONTAINS
   !-------------------------------------------------------------------
   !> @brief This function print the date and time with 
   !> format YYYY/MM/DD hh:mm:ss.
   !> @details
   !> Optionally, you could specify output format. However it will be only apply
   !> to year, month, day.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] td_date   date strutcutre
   !> @param[in] cd_fmt    ouput format (only for year,month,day)
   !> @return date in format YYYY-MM-DD hh:mm:ss
   !-------------------------------------------------------------------
   CHARACTER(LEN=lc) FUNCTION date_print(td_date, cd_fmt)
      IMPLICIT NONE
      ! Argument   
      TYPE(TDATE)     , INTENT(IN) :: td_date
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: cd_fmt
      !----------------------------------------------------------------

      IF( PRESENT(cd_fmt) )THEN
         WRITE(date_print,TRIM(cd_fmt)) &
         &    td_date%i_year,td_date%i_month,td_date%i_day
      ELSE
         WRITE(date_print,cm_fmtdate) &
         &    td_date%i_year,td_date%i_month,td_date%i_day, &
         &    td_date%i_hour,td_date%i_min,td_date%i_sec
      ENDIF

   END FUNCTION date_print
   !-------------------------------------------------------------------
   !> @brief This function check if year is a leap year.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] td_date   date strutcutre
   !> @return true if year is leap year
   !-------------------------------------------------------------------
   LOGICAL FUNCTION date_leapyear(td_date)
      IMPLICIT NONE
      ! Argument   
      TYPE(TDATE), INTENT(IN) :: td_date
      !----------------------------------------------------------------

      date_leapyear=.false.
      IF( (MOD(td_date%i_year,100_i4)==0) )THEN
         IF( (MOD(td_date%i_year,400_i4)==0) )THEN
            date_leapyear=.true.
         ENDIF
      ELSE
         IF( (MOD(td_date%i_year,4_i4)==0) )THEN
            date_leapyear=.true.
         ENDIF
      ENDIF      

   END FUNCTION date_leapyear
   !-------------------------------------------------------------------
   !> @brief This function return the current date and time.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @return current date and time in a date structure
   !-------------------------------------------------------------------
   TYPE(TDATE) FUNCTION date_now()
      IMPLICIT NONE
      ! local variable
      INTEGER(sp), DIMENSION(8) :: il_values
      !----------------------------------------------------------------

      CALL DATE_AND_TIME( values= il_values)

      date_now=date_init( il_values(1), il_values(2), il_values(3), &
      &                   il_values(5), il_values(6), il_values(7) )

   END FUNCTION date_now
   !-------------------------------------------------------------------
   !> @brief This function return the date of the day at 12:00:00.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @return date of the day at 12:00:00 in a date structure
   !-------------------------------------------------------------------
   TYPE(TDATE) FUNCTION date_today()
      IMPLICIT NONE
      ! local variable
      INTEGER(sp), DIMENSION(8) :: il_values
      !----------------------------------------------------------------

      CALL DATE_AND_TIME( values= il_values)

      date_today=date_init( il_values(1), il_values(2), il_values(3), 12_i4 )

   END FUNCTION date_today
   !-------------------------------------------------------------------
   !> @brief This function initialized date structure from a character 
   !> date with format YYYY-MM-DD hh:mm:ss.<br/>
   !> @details
   !> Optionaly create pseudo julian day with new origin.<br/>
   !> julian day origin is 17 Nov 1858 at 00:00:00
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] cd_date   date in format YYYY-MM-DD hh:mm:ss
   !> @param[in] td_dateo  new date origin for pseudo julian day
   !> @return date structure
   !-------------------------------------------------------------------
   TYPE(TDATE) FUNCTION date__init_fmtdate(cd_datetime, td_dateo)
      IMPLICIT NONE
      ! Argument   
      CHARACTER(LEN=*), INTENT(IN)  :: cd_datetime
      TYPE(TDATE),      INTENT(IN), OPTIONAL :: td_dateo

      ! local variable
      CHARACTER(LEN=lc) :: cl_datetime
      CHARACTER(LEN=lc) :: cl_date
      CHARACTER(LEN=lc) :: cl_time
      CHARACTER(LEN=lc) :: cl_year
      CHARACTER(LEN=lc) :: cl_month
      CHARACTER(LEN=lc) :: cl_day
      CHARACTER(LEN=lc) :: cl_hour
      CHARACTER(LEN=lc) :: cl_min
      CHARACTER(LEN=lc) :: cl_sec

      INTEGER(i4) :: il_year
      INTEGER(i4) :: il_month
      INTEGER(i4) :: il_day
      INTEGER(i4) :: il_hour
      INTEGER(i4) :: il_min
      INTEGER(i4) :: il_sec
      !----------------------------------------------------------------

      cl_datetime=TRIM(ADJUSTL(cd_datetime))

      cl_date=fct_split(cl_datetime,1,' ')
      cl_time=fct_split(cl_datetime,2,' ')

      cl_year = fct_split(cl_date,1,'-')
      READ(cl_year,*) il_year
      cl_month= fct_split(cl_date,2,'-')
      READ(cl_month, *) il_month
      cl_day  = fct_split(cl_date,3,'-')
      READ(cl_day, *) il_day
      cl_hour = fct_split(cl_time,1,':')
      READ(cl_hour, *) il_hour
      cl_min  = fct_split(cl_time,2,':')
      READ(cl_min, *) il_min
      cl_sec  = fct_split(cl_time,3,':')
      READ(cl_sec, *) il_sec

      date__init_fmtdate = date_init( il_year, il_month, il_day, il_hour, &
      &                               il_min, il_sec, td_dateo=td_dateo )

   END FUNCTION date__init_fmtdate
   !-------------------------------------------------------------------
   !> @brief This function initialized date structure from julian day.<br/>
   !> @details
   !> Optionaly create pseudo julian day with new origin.<br/>
   !> julian day origin is 17 Nov 1858 at 00:00:00
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] dd_jd     julian day
   !> @param[in] td_dateo  new date origin for pseudo julian day
   !
   !> @return date structure of julian day
   !-------------------------------------------------------------------
   TYPE(TDATE) FUNCTION date__init_jd(dd_jd, td_dateo)
      IMPLICIT NONE
      !Argument
      REAL(dp),    INTENT(IN)  :: dd_jd
      TYPE(TDATE), INTENT(IN), OPTIONAL :: td_dateo
      !----------------------------------------------------------------
      IF( PRESENT(td_dateo) )THEN
         CALL date__check(td_dateo)

         ! pseudo julian day with origin dateo
         date__init_jd%d_jc=dd_jd
         date__init_jd%k_jcsec=date__jd2sec(dd_jd)

         ! convert to truly julian day
         CALL date__jc2jd(date__init_jd, td_dateo)
      ELSE
         date__init_jd%d_jd=dd_jd
         date__init_jd%k_jdsec=date__jd2sec(dd_jd)

         ! compute CNES julian day
         CALL date__jd2jc(date__init_jd)
      ENDIF

      ! check input data
      CALL date__check(date__init_jd)

      ! compute year month day hour min sec 
      CALL date__jd2ymd(date__init_jd)

      ! compute day of the wekk
      CALL date__jd2dow(date__init_jd)

      !compute last day of the month
      date__init_jd%i_lday=date__lastday(date__init_jd)

   END FUNCTION date__init_jd
   !-------------------------------------------------------------------
   !> @brief This function initialized date structure from number of 
   !> second since julian day origin.<br/>
   !> @details
   !> Optionaly create pseudo julian day with new origin.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] kd_nsec   number of second since julian day origin
   !> @param[in] td_dateo  new date origin for pseudo julian day
   !
   !> @return date structure of julian day
   !-------------------------------------------------------------------
   TYPE(TDATE) FUNCTION date__init_nsec(kd_nsec, td_dateo)
      IMPLICIT NONE
      !Argument
      INTEGER(i8), INTENT(IN)  :: kd_nsec
      TYPE(TDATE), INTENT(IN), OPTIONAL :: td_dateo
      !----------------------------------------------------------------
      IF( PRESENT(td_dateo) )THEN
         date__init_nsec=date_init( date__sec2jd(kd_nsec), td_dateo )
      ELSE
         date__init_nsec=date_init( date__sec2jd(kd_nsec) )
      ENDIF

   END FUNCTION date__init_nsec
   !-------------------------------------------------------------------
   !> @brief This function initialized date structure form year month day
   !> and optionnaly hour min sec.<br/>
   !> @details
   !> Optionaly create pseudo julian day with new origin.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !>
   !> @param[in] id_year
   !> @param[in] id_month
   !> @param[in] id_day
   !> @param[in] id_hour
   !> @param[in] id_min
   !> @param[in] id_sec
   !> @param[in] td_dateo  new date origin for pseudo julian day
   !
   !> @return date structure of year month day
   !-------------------------------------------------------------------
   TYPE(TDATE) FUNCTION date__init_ymd(id_year, id_month, id_day,  &
   &                                   id_hour, id_min, id_sec, &
   &                                   td_dateo)
      IMPLICIT NONE
      !Argument
      INTEGER(i4), INTENT(IN) :: id_year
      INTEGER(i4), INTENT(IN) :: id_month
      INTEGER(i4), INTENT(IN) :: id_day
      INTEGER(i4), INTENT(IN), OPTIONAL :: id_hour
      INTEGER(i4), INTENT(IN), OPTIONAL :: id_min
      INTEGER(i4), INTENT(IN), OPTIONAL :: id_sec
      TYPE(TDATE), INTENT(IN), OPTIONAL :: td_dateo
      !----------------------------------------------------------------
      date__init_ymd%i_year=id_year
      date__init_ymd%i_month=id_month
      date__init_ymd%i_day=id_day
      IF( PRESENT(id_hour) )THEN
         date__init_ymd%i_hour=id_hour
      ENDIF
      IF( PRESENT(id_min) )THEN
         date__init_ymd%i_min=id_min
      ENDIF   
      IF( PRESENT(id_sec) )THEN   
         date__init_ymd%i_sec=id_sec
      ENDIF   
      ! check input data
      CALL date__check(date__init_ymd)

      ! compute julian day
      CALL date__ymd2jd(date__init_ymd)

      IF( PRESENT(td_dateo) )THEN
         CALL date__check(td_dateo)
         ! compute julian day with origin dateo
         CALL date__jd2jc(date__init_ymd, td_dateo)         
      ELSE
         ! compute CNES julian day
         CALL date__jd2jc(date__init_ymd)
      ENDIF      

      ! compute day of the week
      CALL date__jd2dow(date__init_ymd)

      !compute last day of the month
      date__init_ymd%i_lday=date__lastday(date__init_ymd)

   END FUNCTION date__init_ymd
   !-------------------------------------------------------------------
   !> @brief This function compute number of day between two dates: 
   !> nday= date1 - date2
   !
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] td_date1  first date strutcutre
   !> @param[in] td_date2  second date strutcutre
   !> @return nday
   !-------------------------------------------------------------------
   REAL(dp) FUNCTION date__diffdate(td_date1, td_date2)
      IMPLICIT NONE
       
      !Argument
      TYPE(TDATE), INTENT(IN) :: td_date1
      TYPE(TDATE), INTENT(IN) :: td_date2
      !----------------------------------------------------------------

      ! check year month day hour min sec
      CALL date__check(td_date1)   
      CALL date__check(td_date2)   

      date__diffdate = td_date1%d_jd - td_date2%d_jd

   END FUNCTION date__diffdate
   !-------------------------------------------------------------------
   !> @brief This function substract nday to a date:
   !> date2 = date1 - nday
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] td_date   date strutcutre
   !> @param[in] dd_nday   number of day
   !> @return date strutcutre of date - nday
   !-------------------------------------------------------------------
   TYPE(TDATE) FUNCTION date__subnday(td_date, dd_nday)
      IMPLICIT NONE
      !Argument
      TYPE(TDATE), INTENT(IN) :: td_date
      REAL(dp),    INTENT(IN) :: dd_nday
      !----------------------------------------------------------------

      ! check year month day hour min sec
      CALL date__check(td_date)   

      date__subnday=date__init_jd(td_date%d_jd-dd_nday)

   END FUNCTION date__subnday
   !-------------------------------------------------------------------
   !> @brief This function add nday to a date:
   !> date2 = date1 + nday
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] td_date   date strutcutre
   !> @param[in] dd_nday   number of day
   !> @return date strutcutre of date + nday
   !-------------------------------------------------------------------
   TYPE(TDATE) FUNCTION date__addnday(td_date, dd_nday)
      IMPLICIT NONE
      !Argument
      TYPE(TDATE), INTENT(IN) :: td_date
      REAL(dp),    INTENT(IN) :: dd_nday
      !----------------------------------------------------------------

      ! check year month day hour min sec
      CALL date__check(td_date)   

      date__addnday=date__init_jd(td_date%d_jd+dd_nday)

   END FUNCTION date__addnday
   !-------------------------------------------------------------------
   !> @brief This subroutine compute last day of the month
   !
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] td_date   date strutcutre
   !> @return last day of the month
   !-------------------------------------------------------------------
   INTEGER(i4) FUNCTION date__lastday(td_date)
      IMPLICIT NONE
      ! Argument   
      TYPE(TDATE), INTENT(IN) :: td_date

      ! local variable
      INTEGER, DIMENSION(12), PARAMETER :: il_lastdaytab = &
      &        (/31,28,31,30,31,30,31,31,30,31,30,31/)
      !----------------------------------------------------------------

      ! general case
      IF( td_date%i_month /= 2 )THEN
         date__lastday=il_lastdaytab(td_date%i_month)
      ELSE
         IF( date_leapyear(td_date) )THEN
            date__lastday=29
         ELSE
            date__lastday=il_lastdaytab(td_date%i_month)
         ENDIF
      ENDIF

   END FUNCTION date__lastday
   !-------------------------------------------------------------------
   !> @brief This subroutine compute julian day from year month day , and fill
   !> input date strutcutre.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[inout] td_date   date strutcutre
   !-------------------------------------------------------------------
   SUBROUTINE date__ymd2jd(td_date)
      IMPLICIT NONE
      ! Argument   
      TYPE(TDATE), INTENT(INOUT) :: td_date

      ! local variable
      REAL(dp) :: dl_standard_jd
      REAL(dp) :: dl_frac
      !----------------------------------------------------------------

      dl_standard_jd= td_date%i_day - 32075                               & 
          & + 1461 * (td_date%i_year + 4800 - (14 - td_date%i_month)/12)/4  &
          & + 367 * (td_date%i_month - 2 + (14 - td_date%i_month)/12*12)/12 &
          & - 3 * ((td_date%i_year + 4900 - (14 - td_date%i_month)/12)/100)/4
      
      td_date%d_jd = dl_standard_jd-2400001 ! origin : 17 nov 1858 h00m00s00

      ! compute fraction of day
      dl_frac=date__hms2jd(td_date)

      td_date%d_jd = td_date%d_jd + dl_frac

      td_date%k_jdsec = date__jd2sec( td_date%d_jd ) 

   END SUBROUTINE date__ymd2jd
   !-------------------------------------------------------------------
   !> @brief This subroutine compute year month day from julian day, and fill
   !> input date strutcutre.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[inout] td_date   date strutcutre
   !-------------------------------------------------------------------
   SUBROUTINE date__jd2ymd(td_date)
      IMPLICIT NONE
      ! Argument   
      TYPE(TDATE), INTENT(INOUT) :: td_date

      ! local variable
      INTEGER(i4) :: il_standard_jd
      INTEGER(i4) :: il_temp1
      INTEGER(i4) :: il_temp2
      !----------------------------------------------------------------

      ! check year month day hour min sec
      CALL date__check(td_date)   

      il_standard_jd=INT( td_date%d_jd+2400001, i4 )

      il_temp1=il_standard_jd + 68569
      il_temp2=4*il_temp1/146097
      il_temp1=il_temp1 - (146097 * il_temp2 + 3) / 4
      td_date%i_year  = 4000 * (il_temp1 + 1) / 1461001
      il_temp1 = il_temp1 - 1461 * td_date%i_year/4 + 31
      td_date%i_month = 80 * il_temp1 / 2447
      td_date%i_day   = il_temp1 - 2447 * td_date%i_month / 80
      il_temp1 = td_date%i_month / 11
      td_date%i_month = td_date%i_month + 2 - 12 * il_temp1
      td_date%i_year  = 100 * (il_temp2 - 49) + td_date%i_year + il_temp1

      ! compute hour, minute, second from julian fraction
      CALL date__jd2hms(td_date)

      ! adjust date
      CALL date__adjust(td_date)

   END SUBROUTINE date__jd2ymd
   !-------------------------------------------------------------------
   !> @brief This subroutine compute julian day from pseudo julian day 
   !> with new date origin, and fill input date strutcutre.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[inout] td_date   date
   !> @param[in]    td_dateo  new date origin for pseudo julian day
   !-------------------------------------------------------------------
   SUBROUTINE date__jc2jd(td_date, td_dateo)
      IMPLICIT NONE
      ! Argument   
      TYPE(TDATE), INTENT(INOUT) :: td_date
      TYPE(TDATE), INTENT(IN) :: td_dateo

      ! local variable
      TYPE(TDATE) :: tl_date
      REAL(dp)    :: dl_nday
      !----------------------------------------------------------------
      ! origin julian day
      tl_date=date_init(1858,11,17)

      dl_nday=td_dateo-tl_date

      ! compute julian day
      td_date%d_jd = td_date%d_jc + dl_nday
      ! compute number of second since julian day origin
      td_date%k_jdsec = date__jd2sec(td_date%d_jd)

   END SUBROUTINE date__jc2jd
   !-------------------------------------------------------------------
   !> @brief This subroutine compute pseudo julian day with new date origin, and
   !> fill input date structure.<br/>
   !>  default new origin is CNES julian day origin: 1950-01-01 00:00:00
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[inout] td_date   date
   !> @param[in] td_dateo     new origin date
   !-------------------------------------------------------------------
   SUBROUTINE date__jd2jc(td_date, td_dateo)
      IMPLICIT NONE
      ! Argument   
      TYPE(TDATE), INTENT(INOUT) :: td_date
      TYPE(TDATE), INTENT(IN),   OPTIONAL :: td_dateo

      ! local variable
      TYPE(TDATE) :: tl_dateo
      !----------------------------------------------------------------
      IF( PRESENT(td_dateo) )THEN
         td_date%d_jc=td_date%d_jd-td_dateo%d_jd
      ELSE
         ! CNES julian day origin
         tl_dateo%i_year = 1950
         tl_dateo%i_month = 1
         tl_dateo%i_day = 1

         CALL date__ymd2jd(tl_dateo)

         td_date%d_jc = td_date%d_jd-tl_dateo%d_jd
      ENDIF

      td_date%k_jcsec = date__jd2sec(td_date%d_jc)

   END SUBROUTINE date__jd2jc
   !-------------------------------------------------------------------
   !> @brief This subroutine compute the day of week from julian day, and fill
   !> input date structure.<br/>
   !>  days   : Sunday Monday Tuesday Wednesday Thursday Friday Saturday<br/>
   !>  numday : 0      1      2       3         4        5      6<br/>
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[inout] td_date   date strutcutre
   !-------------------------------------------------------------------
   SUBROUTINE date__jd2dow(td_date)
      IMPLICIT NONE
      ! Argument   
      TYPE(TDATE), INTENT(INOUT) :: td_date
      !----------------------------------------------------------------

      td_date%i_dow=MOD((INT(AINT(td_date%d_jd))+3),7)

   END SUBROUTINE date__jd2dow
   !-------------------------------------------------------------------
   !> @brief This function compute fraction of a day from 
   !> hour, minute, second. 
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] td_date   date strutcutre
   !> @return fraction of the day
   !-------------------------------------------------------------------
   REAL(dp) FUNCTION date__hms2jd(td_date)
      IMPLICIT NONE
      ! Argument   
      TYPE(TDATE), INTENT(IN) :: td_date
      !----------------------------------------------------------------

      !  compute real seconds
      date__hms2jd = REAL( td_date%i_sec, dp )   
      !  compute real minutes
      date__hms2jd = REAL( td_date%i_min, dp ) + date__hms2jd/60.0
      !  compute real hours
      date__hms2jd = REAL( td_date%i_hour, dp ) + date__hms2jd/60.0
      !  julian fraction of a day
      date__hms2jd = date__hms2jd/24.0

   END FUNCTION date__hms2jd
   !-------------------------------------------------------------------
   !> @brief This subroutine compute hour, minute, second from julian 
   !> fraction, and fill date structure.
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[inout] td_date   date strutcutre
   !-------------------------------------------------------------------
   SUBROUTINE date__jd2hms(td_date)
      IMPLICIT NONE
      ! Argument   
      TYPE(TDATE), INTENT(INOUT) :: td_date

      !local variable
      REAL(dp) :: dl_fract
      !----------------------------------------------------------------

      dl_fract=(td_date%d_jd)-AINT(td_date%d_jd)
      !  compute hour
      td_date%i_hour = INT( dl_fract * 24.0, i4 )
      dl_fract = ( dl_fract - REAL( td_date%i_hour, dp ) / 24.0) * 24.0
      !  compute minute
      td_date%i_min = INT( dl_fract * 60.0, i4 )
      dl_fract = ( dl_fract - REAL( td_date%i_min, dp ) / 60.0) * 60.0
      !  compute second
      td_date%i_sec = NINT( dl_fract * 60.0, i4 )

   END SUBROUTINE date__jd2hms
   !-------------------------------------------------------------------
   !> @brief This subroutine check date express in date structure
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] td_date   date strutcutre
   !-------------------------------------------------------------------
   SUBROUTINE date__check(td_date)
      IMPLICIT NONE
      ! Argument   
      TYPE(TDATE), INTENT(IN) :: td_date

      ! local variable
      INTEGER(i4)       :: il_lastday
      INTEGER(i4)       :: il_status
      CHARACTER(LEN=lc) :: cl_msg
      !----------------------------------------------------------------

      ! init
      il_status=0

      ! check year
      IF( td_date%i_year < 1858_i4 .OR. td_date%i_year > 39999_i4 )THEN
         il_status=il_status+1
         WRITE(cl_msg,*) "year ",td_date%i_year," out of range"
         CALL logger_error(cl_msg)
      ENDIF
      ! check month
      IF( td_date%i_month < 1_i4 .OR. td_date%i_month > 12_i4 )THEN
         il_status=il_status+1
         WRITE(cl_msg,*) "month ",td_date%i_month," out of range"
         CALL logger_error(cl_msg)
      ENDIF
      ! check day
      il_lastday=date__lastday(td_date)
      IF( td_date%i_day < 1_i4 .OR. td_date%i_day > il_lastday )THEN
         il_status=il_status+1
         WRITE(cl_msg,*) "day ",td_date%i_day," out of range"
         CALL logger_error(cl_msg)
      ENDIF
      ! check hour
      IF( td_date%i_hour < 0_i4 .OR. td_date%i_hour > 23_i4 )THEN
         il_status=il_status+1
         WRITE(cl_msg,*) "hour ",td_date%i_hour," out of range"
         CALL logger_error(cl_msg)
      ENDIF
      ! check minutes
      IF( td_date%i_min < 0_i4 .OR. td_date%i_min > 59_i4 )THEN
         il_status=il_status+1
         WRITE(cl_msg,*) "minutes ",td_date%i_min," out of range"
         CALL logger_error(cl_msg)
      ENDIF   
      ! check seconds
      IF( td_date%i_sec < 0_i4 .OR. td_date%i_sec > 59_i4 )THEN
         il_status=il_status+1
         WRITE(cl_msg,*) "seconds ",td_date%i_sec," out of range"
         CALL logger_error(cl_msg)
      ENDIF

      ! check julian day
      IF( td_date%d_jd < 0_sp .OR. td_date%d_jd > 782028_sp )THEN
         il_status=il_status+1
         WRITE(cl_msg,*) "julian day ",td_date%d_jd," out of range"
         CALL logger_error(cl_msg)
      ENDIF

      IF( il_status/= 0 )THEN
         WRITE(cl_msg,*) " date error"
         CALL logger_fatal(cl_msg)
      ENDIF

   END SUBROUTINE date__check
   !-------------------------------------------------------------------
   !> @brief This subroutine adjust date (correct hour, minutes, and seconds
   !> value if need be)
   !>
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[inout] td_date   date strutcutre
   !-------------------------------------------------------------------
   SUBROUTINE date__adjust(td_date)
      IMPLICIT NONE
      ! Argument   
      TYPE(TDATE), INTENT(INOUT) :: td_date
      !----------------------------------------------------------------

      IF( td_date%i_sec == 60 )THEN
         td_date%i_sec=0
         td_date%i_min=td_date%i_min+1
      ENDIF
      
      IF( td_date%i_min == 60 )THEN
         td_date%i_min=0
         td_date%i_hour=td_date%i_hour+1
      ENDIF

      IF( td_date%i_hour == 24 )THEN
         td_date%i_hour=0
         td_date=date__addnday(td_date,1._dp)
      ENDIF

   END SUBROUTINE date__adjust
   !-------------------------------------------------------------------
   !> @brief This function convert julian day in seconds
   !> since julian day origin.
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] td_date   date strutcutre
   !> @return number of seconds since julian day origin
   !-------------------------------------------------------------------
   INTEGER(i8) FUNCTION date__jd2sec(dd_jul)
      IMPLICIT NONE
      ! Argument   
      REAL(dp), INTENT(IN) :: dd_jul
      !----------------------------------------------------------------

      date__jd2sec = NINT( dd_jul * im_secbyday, i8 )

   END FUNCTION date__jd2sec
   !-------------------------------------------------------------------
   !> @brief This function convert seconds since julian day origin in 
   !> julian day.
   !> @author J.Paul
   !> @date November, 2013 - Initial Version
   !
   !> @param[in] kd_nsec   number of second since julian day origin
   !> @return julian day
   !-------------------------------------------------------------------
   REAL(dp) FUNCTION date__sec2jd(kd_nsec)
      IMPLICIT NONE
      ! Argument   
      INTEGER(i8), INTENT(IN) :: kd_nsec
      !----------------------------------------------------------------

      date__sec2jd = REAL( REAL( kd_nsec , dp ) / im_secbyday, dp )

   END FUNCTION date__sec2jd
END MODULE date

