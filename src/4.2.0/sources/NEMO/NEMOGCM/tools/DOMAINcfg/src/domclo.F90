MODULE domclo
   !!======================================================================
   !!                       ***  MODULE  domclo  ***
   !! domclo : definition of closed sea mask needed by NEMO
   !!======================================================================
   !! History : NEMO 4.0 ! 07-2019 (P. Mathiot) original version
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   dom_clo    : definition of closed sea mask needed by NEMO
   !!----------------------------------------------------------------------
   USE dom_oce         ! ocean space and time domain
   USE domngb          ! closest point algorithm
   USE phycst          ! rpi, rad, ra
   USE domutl          ! flood filling algorithm (fill_pool)
   USE in_out_manager  ! I/O manager
   USE lbclnk          ! lateral boundary condition - MPP exchanges
   USE lib_mpp
   USE lib_fortran

   IMPLICIT NONE
   PRIVATE

   PUBLIC dom_clo      ! routine called by domain module

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: closea.F90 7213 2016-11-09 09:07:42Z timgraham $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dom_clo
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE dom_clo  ***
      !!        
      !! ** Purpose :   Closed sea mask definition
      !!
      !! ** Method  :   A flood filling algorithm is used to detect lake and open ocean
      !!                Namelist provide seed for each lake and condition on how to
      !!                manage net evap/runoff.
      !!
      !! ** Action  :  - compute open ocean mask 
      !!               - loop over all the lake in namelist 
      !!               - define lake msk (msk_csglo/emp/rnf)
      !!               - define river mouth mask for each lake or group of lake with the same river mouth
      !!                 (msk_csgrpglo/emp/rnf) 
      !!               
      !!----------------------------------------------------------------------

      TYPE closea 
         CHARACTER(256) :: cname                       ! name
         REAL(wp)       :: rlonsrc                     ! seed src location (lon)
         REAL(wp)       :: rlatsrc                     ! seed src location (lat)
         REAL(wp)       :: rlontrg                     ! seed trg location (lon)
         REAL(wp)       :: rlattrg                     ! seed trg location (lat)
         CHARACTER(256) :: cloctrg                     ! where water is spread
         CHARACTER(256) :: cschtrg                     ! how   water is spread
         REAL(wp)       :: radtrg                      ! radius of closed sea river mouth (used if cschtrg is rnf or emp)
         INTEGER        :: idtrg                       ! target id in case multiple lakes for the same river mouth
      END TYPE
         
      INTEGER, PARAMETER :: jp_closea = 98 ! number maximal of closea
      INTEGER :: ji, jj, jcs, jsch  ! loop indexes
      INTEGER :: jglo, jemp, jrnf   ! closed sea indexes for global, emp, rnf spreading
      INTEGER :: jiseed, jjseed     ! seed indexes
      INTEGER :: jloc, jcoast       ! counter
      INTEGER :: ios
      INTEGER :: nn_closea          ! number of closed seas

      REAL(wp) :: zdistseed                         ! distance to seed
      REAL(wp) :: zarea                             ! river mouth area
      REAL(wp) :: rn_lon_opnsea, rn_lat_opnsea      ! open sea seed
      REAL(wp), DIMENSION(1)       :: zchk, zradtrg !
      REAL(wp), DIMENSION(jpi,jpj) :: zdist         ! distance to seed trg location
      REAL(wp), DIMENSION(jpi,jpj) :: zmsksrc, zmsktrg, zmsk_coastline ! various mask

      CHARACTER(256) :: csch, cloc                       ! scheme name for water spreading (glo, rnf, emp)
      TYPE(closea)  , DIMENSION(jp_closea)   :: sn_lake  ! lake properties

      LOGICAL :: lskip     ! flag in case lake seed on land or already filled (...)

      NAMELIST/namclo/ rn_lon_opnsea, rn_lat_opnsea, nn_closea, sn_lake

      !!----------------------------------------------------------------------
      !! 0 : Read namelist for closed sea definition
      !!----------------------------------------------------------------------
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*)'dom_clo : closed seas '
      IF(lwp) WRITE(numout,*)'~~~~~~~'

      !!---------------------------------------------------------------------
      
   !   REWIND( numnam_ref )              ! Namelist namlbc in reference namelist : Lateral momentum boundary condition
      READ  ( numnam_ref, namclo, IOSTAT = ios, ERR = 901 )
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namclo in reference namelist')
   !   REWIND( numnam_cfg )              ! Namelist namlbc in configuration namelist : Lateral momentum boundary condition
      READ  ( numnam_cfg, namclo, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namclo in configuration namelist')
      IF(lwm) WRITE ( numond, namclo )

      !!----------------------------------------------------------------------
      !! 1 : Define open ocean and closed sea masks
      !!----------------------------------------------------------------------

      !! 1.1 get closest point to the lat/lon seed
      msk_opnsea(:,:) = ssmask(:,:)
      CALL dom_ngb(rn_lon_opnsea, rn_lat_opnsea, jiseed, jjseed, zdistseed, 'T')

      !! 1.2 fill connected cell to -1
      CALL fill_pool( jiseed, jjseed, msk_opnsea, -1._wp )

      !! sanity check on the msk value (closea mask should be 99 otherwise, lake already processed)
      !! check if some lake are not connected
      zchk = 0._wp
      IF (mi0(jiseed) == mi1(jiseed) .AND. mj0(jjseed) == mj1(jjseed)) zchk = ssmask(mi0(jiseed),mj0(jjseed))
      CALL mpp_max('domclo',zchk)
      IF (zchk(1) == 0._wp) CALL ctl_stop( 'STOP', 'open sea seed is on land, please update namelist (rn_lon_opnsea,rn_lat_opnsea)' ) 

      !! print
      IF (lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*)'open ocean: '
         WRITE(numout,'(a,2f7.2)')'    lon/lat seed to detect main ocean is: ', rn_lon_opnsea, rn_lat_opnsea
         WRITE(numout,'(a,2i7)'  )'    i/j     seed to detect main ocean is: ', jiseed, jjseed
         WRITE(numout,'(a,f8.0)' )'    distance (m) between namelist seed location and model seed location: ', zdistseed
         WRITE(numout,*)
      END IF
      
      !! 1.3 set to 0 everything >0 and revert mask
      WHERE (msk_opnsea(:,:) > 0._wp) msk_opnsea(:,:) = 0._wp ! mask all closed seas
      WHERE (msk_opnsea(:,:) < 0._wp) msk_opnsea(:,:) = 1._wp ! restore mask value

      !! 1.4 Define closed sea mask (all of them, ie defined in the namelist or not)
      !! needed to remove the undefined closed seas at the end
      msk_csundef(:,:) = ( ssmask(:,:) - msk_opnsea(:,:) ) * 99._wp

      !!----------------------------------------------------------------------
      !! 2 : compute closed sea mask for global, rnf and emp cases
      !!----------------------------------------------------------------------

      !! 2.1 : initialisation of masks and lake indexes
      jglo = 1 ; jrnf = 1 ; jemp = 1
      !! mask used to group lake by net evap/precip distribution technics
      msk_csglo(:,:) = msk_csundef(:,:)
      msk_csrnf(:,:) = msk_csundef(:,:)
      msk_csemp(:,:) = msk_csundef(:,:)

      !! mask used to define group of lake sharing the same river mouth
      msk_csgrpglo(:,:) = 0.0_wp
      msk_csgrprnf(:,:) = 0.0_wp
      msk_csgrpemp(:,:) = 0.0_wp

      IF (lwp) WRITE(numout,*)'closed seas: '
 
      DO jcs = 1,nn_closea

         !! how the excess of the closed seas is spread out:
         cloc = sn_lake(jcs)%cloctrg
         csch = sn_lake(jcs)%cschtrg

         !! set up indexes and mask
         SELECT CASE (csch)
         CASE('glo')
            jsch = jglo
            zmsksrc(:,:) = msk_csglo(:,:)
            zmsktrg(:,:) = msk_csgrpglo(:,:)
         CASE('rnf') 
            jsch = jrnf
            zmsksrc(:,:) = msk_csrnf(:,:)
            zmsktrg(:,:) = msk_csgrprnf(:,:)
         CASE('emp')
            jsch = jemp 
            zmsksrc(:,:) = msk_csemp(:,:)
            zmsktrg(:,:) = msk_csgrpemp(:,:)
         CASE DEFAULT
            CALL ctl_stop( 'STOP', 'domclo: ',TRIM(csch),' is an unknown target type for lake (should be glo, emp, rnf)' )
         END SELECT

         !! 2.2 : print out and sanity check
         !! flag changed in sanity checks
         lskip = .FALSE.

         !! define seed to extract the closed sea jcs
         CALL dom_ngb(sn_lake(jcs)%rlonsrc, sn_lake(jcs)%rlatsrc, jiseed, jjseed, zdistseed, 'T')

         !! sanity check on the msk value (closea mask should be 99 otherwise, lake already processed)
         !! check if seed on land or lake already processed
         zchk = 0._wp
         IF (mi0(jiseed) == mi1(jiseed) .AND. mj0(jjseed) == mj1(jjseed)) zchk = msk_csundef(mi0(jiseed),mj0(jjseed))
         CALL mpp_max('domclo',zchk)
         
         IF (zchk(1) /= 99._wp) THEN
            IF (lwp) THEN
               WRITE(numout,*)
               WRITE(numout,*)'W A R N I N G: lake ',TRIM(sn_lake(jcs)%cname),' already processed or seed on land, we skip it.'
               WRITE(numout,*)'It is possible this lake is connected to another one already processed.'
               WRITE(numout,*)'It is possible the seed is on land because the lake is not present in the bathymetry file'
               WRITE(numout,*)'If concerned about it, check input file and namelist definition of this lake.'
            END IF
            lskip = .TRUE.
         END IF
 
         !! 2.3 : compute mask for global, rnf, emp case
         IF (.NOT. lskip) THEN

            IF (lwp) THEN
               WRITE(numout,*)
               WRITE(numout,'(a,a10,a,2f7.2,a,2i6,a,f7.2,a)')                               &
                    &         '    processing lake ',TRIM(sn_lake(jcs)%cname)               &
                    &        ,' ( lat/lon : ',sn_lake(jcs)%rlatsrc, sn_lake(jcs)%rlonsrc    &
                    &        ,' , i/j     : ',jiseed,jjseed                                 &
                    &        ,' , distance to seed : ',zdistseed,'m )'
            END IF

            !! fill close sea mask with counter value
            !! and update undefined closed sea mask
            CALL fill_pool( jiseed, jjseed, zmsksrc, REAL(jsch  ,8))
            WHERE (zmsksrc(:,:) == REAL(jsch,8))
               msk_csundef = 0._wp
               zmsktrg    = sn_lake(jcs)%idtrg
            END WHERE

            !! compute location of river mouth and distance to river mouth
            IF (cloc /= 'global') THEN

               !! set value for radius of the river influence
               zradtrg = 0._wp
               IF (mi0(jiseed) == mi1(jiseed) .AND. mj0(jjseed) == mj1(jjseed)) THEN
                  zradtrg(1) = MAX( sn_lake(jcs)%radtrg, e1t( mi0(jiseed), mj0(jjseed) ), e2t( mi0(jiseed), mj0(jjseed) ) )
               END IF
               CALL mpp_max('domclo',zradtrg)
              
               !! compute seed location for print 
               CALL dom_ngb(sn_lake(jcs)%rlontrg, sn_lake(jcs)%rlattrg, jiseed, jjseed, zdistseed, 'T')

               !! compute distance to river mouth
               zdist(:,:) = dist(sn_lake(jcs)%rlontrg, sn_lake(jcs)%rlattrg, glamt, gphit)

            END IF

            !! define estuary
            !! deal with global/local/coastal cases
            SELECT CASE (cloc)
            CASE ('global')
               WHERE (msk_opnsea(:,:) == 1._wp) zmsktrg = sn_lake(jcs)%idtrg

            CASE ('local')
               !! compute mask
               WHERE (zdist(:,:) < zradtrg(1) .AND. msk_opnsea(:,:) == 1 ) zmsktrg = sn_lake(jcs)%idtrg

               !! print
               IF (lwp) WRITE(numout,'(a,f7.0,a,2f7.2,a,2i7,a,f7.0,a)')                                            &
                             &         '         river mouth area is defined by         points within ',zradtrg(1) &
                             &        ,' m of lat/lon ', sn_lake(jcs)%rlontrg, sn_lake(jcs)%rlattrg                &
                             &        ,' (closest wet point is i/j ',jiseed, jjseed,' at ',zdistseed,' m )'
            CASE ('coast')
               !! define coastline mask
               zmsk_coastline = 0._wp
               DO jj=2,jpj-1
                  DO ji=2,jpi-1
                     IF ( ssmask(ji,jj) == 1._wp .AND. SUM(ssmask(ji-1:ji+1,jj-1:jj+1)) < 9 ) zmsk_coastline(ji,jj) = 1._wp
                  END DO
               END DO
               CALL lbc_lnk('domclo', zmsk_coastline,'T',1._wp)
   
               !! compute mask
               WHERE ( zdist(:,:) < zradtrg(1) .AND. zmsk_coastline(:,:) == 1 .AND. msk_opnsea(:,:) == 1 ) zmsktrg = sn_lake(jcs)%idtrg

               !! print
               IF (lwp) WRITE(numout,'(a,f7.0,a,2f7.2,a,2i7,a,f7.0,a)')                                            &
                             &         '         river mouth area is defined by coastal points within ',zradtrg(1) &
                             &        ,' m of lat/lon ', sn_lake(jcs)%rlontrg, sn_lake(jcs)%rlattrg                &
                             &        ,' (closest wet point is i/j ',jiseed, jjseed,' at ',zdistseed,' m )'

            CASE DEFAULT
               CALL ctl_stop( 'STOP', 'domclo: unknown target type for lake (should be global, coast or local)' )

            END SELECT
   
            !! sanity check
            zarea = glob_sum('domclo',zmsktrg * msk_opnsea)
            IF (zarea == 0._wp) CALL ctl_stop( 'STOP', 'river mouth area is 0, tune lon/lat trg or radtrg for this lake') 
            !
            zarea = glob_sum('domclo',zmsksrc * msk_opnsea)
            IF (zarea > 0._wp) CALL ctl_stop( 'STOP', 'closed seas and open ocean have common points, '                           &
               &                                    , 'tune lon/lat src or check if your lake is really closed on the model grid')

            !! set up indexes and mask 
            SELECT CASE (csch)
            CASE ('glo')
               jglo = jglo + 1
               msk_csglo(:,:)    = zmsksrc(:,:)
               msk_csgrpglo(:,:) = zmsktrg(:,:)
               IF (lwp) WRITE(numout,*)'        net evap/precip will be spread globally (glo)'
            CASE ('rnf')
               jrnf = jrnf + 1
               msk_csrnf(:,:)    = zmsksrc(:,:)
               msk_csgrprnf(:,:) = zmsktrg(:,:)
               IF (lwp) WRITE(numout,*)'        net precip will be spread locally and net evap globally (rnf)'
            CASE ('emp')
               jemp = jemp + 1
               msk_csemp(:,:)    = zmsksrc(:,:)
               msk_csgrpemp(:,:) = zmsktrg(:,:)
               IF (lwp) WRITE(numout,*)'        net evap/precip will be spread locally (emp)'
            END SELECT

         END IF ! lskip

      END DO ! nn_closea

      !!----------------------------------------------------------------------
      !! 3 : clean the masks of possible remaining undefined closed seas
      !!----------------------------------------------------------------------

      !! mask all the cells not defined as closed sea
      WHERE ( msk_csglo(:,:) == 99._wp )  msk_csglo = 0._wp
      WHERE ( msk_csrnf(:,:) == 99._wp )  msk_csrnf = 0._wp
      WHERE ( msk_csemp(:,:) == 99._wp )  msk_csemp = 0._wp

      !!  non defined closed sea
      WHERE ( msk_csundef(:,:) > 0._wp ) msk_csundef = 1._wp

   END SUBROUTINE dom_clo

   !!======================================================================

END MODULE domclo

