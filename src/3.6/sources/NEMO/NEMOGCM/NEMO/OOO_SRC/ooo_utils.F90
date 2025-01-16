MODULE ooo_utils
   !! =================================================================
   !!                    *** MODULE ooo_utils ***
   !! =================================================================
   USE par_oce

   IMPLICIT NONE

   ! Define double precision obfillflt
   REAL(kind=dp), PARAMETER :: obfilldbl=99999.

   !! $Id: ooo_utils.F90 5215 2015-04-15 16:11:56Z nicolasmartin $
   CONTAINS

   SUBROUTINE yyyymmdd_to_ref_date(indate, intime, outstring)
      !----------------------------------------
      ! Routine to create reference date string
      !----------------------------------------
      ! Routine arguments
      CHARACTER(len=8),  INTENT(IN)  :: indate    ! yyyymmdd
      CHARACTER(len=6),  INTENT(IN)  :: intime    ! hhmmss
      CHARACTER(len=23), INTENT(OUT) :: outstring ! yyyy-mm-dd hh:mm:ss utc
      ! Local variables
      CHARACTER(len=4)  :: year      ! yyyy
      CHARACTER(len=2)  :: month     ! mm
      CHARACTER(len=2)  :: day       ! dd
      CHARACTER(len=2)  :: hour      ! hh
      CHARACTER(len=2)  :: minute    ! mm
      CHARACTER(len=2)  :: second    ! ss

      year   = indate(1:4)
      month  = indate(5:6)
      day    = indate(7:8)
      hour   = intime(1:2)
      minute = intime(3:4)
      second = intime(5:6)

      outstring = year//"-"//month//"-"//day//" "//hour//":"//minute//":"//second//" utc"
      
   END SUBROUTINE yyyymmdd_to_ref_date

   SUBROUTINE date_format(date_str)
      !---------------------------------------
      ! Routine to create creation date string
      !---------------------------------------

      ! Routine arguments
      CHARACTER(len=*), INTENT(OUT) :: date_str
      ! Local variables
      CHARACTER(8)  :: date
      CHARACTER(10) :: time

      CHARACTER(10)  :: date_part
      CHARACTER(8)  :: time_part

      CALL date_and_time(DATE=date)
      CALL date_and_time(TIME=time)
      date_part  = date(1:4)//'/'//date(5:6)//'/'//date(7:8)
      time_part  = time(1:2)//':'//time(3:4)//':'//time(5:6)

      date_str   = date_part//' '//time_part

   END SUBROUTINE date_format

   SUBROUTINE inst_converter(wmo_inst_list, nprofs, obs_names)
      !---------------------------------------
      ! Routine to convert WMO_INST_TYPE to
      ! 'ARGO', 'TESAC', 'XBT', 'BUOYS'
      !---------------------------------------

      !! Routine arguments
      INTEGER,                             INTENT(IN) :: nprofs
      CHARACTER(len=*),DIMENSION(nprofs),  INTENT(IN) :: wmo_inst_list      
      CHARACTER(len=128),DIMENSION(nprofs),INTENT(OUT):: obs_names

      !! Local variables
      INTEGER :: inam !: loop over name

      !! Initialise obs_names
      obs_names(:) = ''

      !! Convert number to text
      DO inam = 1,nprofs
         IF (trim(adjustl(wmo_inst_list(inam))) .EQ. '820') THEN
            obs_names(inam) = 'BUOYS'
         ELSEIF (trim(adjustl(wmo_inst_list(inam))) .EQ. '401') THEN
            obs_names(inam) = 'XBT'
         ELSEIF (trim(adjustl(wmo_inst_list(inam))) .EQ. '741') THEN
            obs_names(inam) = 'TESAC'
         ELSEIF (trim(adjustl(wmo_inst_list(inam))) .EQ. '831') THEN
            obs_names(inam) = 'ARGO'
         ELSEIF (trim(adjustl(wmo_inst_list(inam))) .EQ. '22') THEN ! Special case for AATSR
            obs_names(inam) = '22'
         ELSEIF (trim(adjustl(wmo_inst_list(inam))) .EQ. '') THEN
            obs_names(inam) = 'UNKNOWN'
         ELSE 
            obs_names(inam) = adjustl(wmo_inst_list(inam)) ! For all other cases
         ENDIF
      ENDDO

   END SUBROUTINE inst_converter

END MODULE ooo_utils
