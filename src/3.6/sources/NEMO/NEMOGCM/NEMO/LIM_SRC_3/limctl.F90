MODULE limctl
   !!======================================================================
   !!                     ***  MODULE  limctl  ***
   !!   LIM-3 : control and printing
   !!======================================================================
   !! History :  3.5  !  2015-01  (M. Vancoppenolle) Original code
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3'                                      LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!    lim_ctl   : control prints in case of crash
   !!    lim_prt   : ice control print at a given grid point
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE ice             ! LIM-3: ice variables
   USE thd_ice         ! LIM-3: thermodynamical variables
   USE dom_ice         ! LIM-3: ice domain
   USE sbc_oce         ! Surface boundary condition: ocean fields
   USE sbc_ice         ! Surface boundary condition: ice   fields

   USE phycst          ! Define parameters for the routines

   USE lib_mpp         ! MPP library
   USE wrk_nemo        ! work arrays
   USE timing          ! Timing
   USE in_out_manager  ! I/O manager
   USE prtctl          ! Print control
   USE lib_fortran     ! 

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_ctl
   PUBLIC   lim_prt

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: limctl.F90 5043 2015-01-28 16:44:18Z clem $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE lim_ctl( kt )
      !!-----------------------------------------------------------------------
      !!                   ***  ROUTINE lim_ctl *** 
      !!                 
      !! ** Purpose :   Alerts in case of model crash
      !!-------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt      ! ocean time step
      INTEGER  ::   ji, jj, jk,  jl   ! dummy loop indices
      INTEGER  ::   inb_altests       ! number of alert tests (max 20)
      INTEGER  ::   ialert_id         ! number of the current alert
      REAL(wp) ::   ztmelts           ! ice layer melting point
      CHARACTER (len=30), DIMENSION(20)      ::   cl_alname   ! name of alert
      INTEGER           , DIMENSION(20)      ::   inb_alp     ! number of alerts positive
      !!-------------------------------------------------------------------

      inb_altests = 10
      inb_alp(:)  =  0

      ! Alert if incompatible volume and concentration
      ialert_id = 2 ! reference number of this alert
      cl_alname(ialert_id) = ' Incompat vol and con         '    ! name of the alert

      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF(  v_i(ji,jj,jl) /= 0._wp   .AND.   a_i(ji,jj,jl) == 0._wp   ) THEN
                  !WRITE(numout,*) ' ALERTE 2 :   Incompatible volume and concentration '
                  !WRITE(numout,*) ' at_i     ', at_i(ji,jj)
                  !WRITE(numout,*) ' Point - category', ji, jj, jl
                  !WRITE(numout,*) ' a_i *** a_i_b   ', a_i      (ji,jj,jl), a_i_b  (ji,jj,jl)
                  !WRITE(numout,*) ' v_i *** v_i_b   ', v_i      (ji,jj,jl), v_i_b  (ji,jj,jl)
                  inb_alp(ialert_id) = inb_alp(ialert_id) + 1
               ENDIF
            END DO
         END DO
      END DO

      ! Alerte if very thick ice
      ialert_id = 3 ! reference number of this alert
      cl_alname(ialert_id) = ' Very thick ice               ' ! name of the alert
      jl = jpl 
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF(   ht_i(ji,jj,jl)  >  50._wp   ) THEN
               !CALL lim_prt( kt, ji, jj, 2, ' ALERTE 3 :   Very thick ice ' )
               inb_alp(ialert_id) = inb_alp(ialert_id) + 1
            ENDIF
         END DO
      END DO

      ! Alert if very fast ice
      ialert_id = 4 ! reference number of this alert
      cl_alname(ialert_id) = ' Very fast ice               ' ! name of the alert
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF(   MAX( ABS( u_ice(ji,jj) ), ABS( v_ice(ji,jj) ) ) > 1.5  .AND.  &
               &  at_i(ji,jj) > 0._wp   ) THEN
               !CALL lim_prt( kt, ji, jj, 1, ' ALERTE 4 :   Very fast ice ' )
               !WRITE(numout,*) ' ice strength             : ', strength(ji,jj)
               !WRITE(numout,*) ' oceanic stress utau      : ', utau(ji,jj) 
               !WRITE(numout,*) ' oceanic stress vtau      : ', vtau(ji,jj)
               !WRITE(numout,*) ' sea-ice stress utau_ice  : ', utau_ice(ji,jj) 
               !WRITE(numout,*) ' sea-ice stress vtau_ice  : ', vtau_ice(ji,jj)
               !WRITE(numout,*) ' oceanic speed u          : ', u_oce(ji,jj)
               !WRITE(numout,*) ' oceanic speed v          : ', v_oce(ji,jj)
               !WRITE(numout,*) ' sst                      : ', sst_m(ji,jj)
               !WRITE(numout,*) ' sss                      : ', sss_m(ji,jj)
               !WRITE(numout,*) 
               inb_alp(ialert_id) = inb_alp(ialert_id) + 1
            ENDIF
         END DO
      END DO

      ! Alert if there is ice on continents
      ialert_id = 6 ! reference number of this alert
      cl_alname(ialert_id) = ' Ice on continents           ' ! name of the alert
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF(   tmask(ji,jj,1) <= 0._wp   .AND.   at_i(ji,jj) > 0._wp   ) THEN 
               !CALL lim_prt( kt, ji, jj, 1, ' ALERTE 6 :   Ice on continents ' )
               !WRITE(numout,*) ' masks s, u, v        : ', tmask(ji,jj,1), umask(ji,jj,1), vmask(ji,jj,1) 
               !WRITE(numout,*) ' sst                  : ', sst_m(ji,jj)
               !WRITE(numout,*) ' sss                  : ', sss_m(ji,jj)
               !WRITE(numout,*) ' at_i(ji,jj)          : ', at_i(ji,jj)
               !WRITE(numout,*) ' v_ice(ji,jj)         : ', v_ice(ji,jj)
               !WRITE(numout,*) ' v_ice(ji,jj-1)       : ', v_ice(ji,jj-1)
               !WRITE(numout,*) ' u_ice(ji-1,jj)       : ', u_ice(ji-1,jj)
               !WRITE(numout,*) ' u_ice(ji,jj)         : ', v_ice(ji,jj)
               !
               inb_alp(ialert_id) = inb_alp(ialert_id) + 1
            ENDIF
         END DO
      END DO

!
!     ! Alert if very fresh ice
      ialert_id = 7 ! reference number of this alert
      cl_alname(ialert_id) = ' Very fresh ice               ' ! name of the alert
      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF( sm_i(ji,jj,jl) < 0.1 .AND. a_i(ji,jj,jl) > 0._wp ) THEN
!                 CALL lim_prt(kt,ji,jj,1, ' ALERTE 7 :   Very fresh ice ' )
!                 WRITE(numout,*) ' sst                  : ', sst_m(ji,jj)
!                 WRITE(numout,*) ' sss                  : ', sss_m(ji,jj)
!                 WRITE(numout,*) 
                  inb_alp(ialert_id) = inb_alp(ialert_id) + 1
               ENDIF
            END DO
         END DO
      END DO
!

!     ! Alert if too old ice
      ialert_id = 9 ! reference number of this alert
      cl_alname(ialert_id) = ' Very old   ice               ' ! name of the alert
      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF ( ( ( ABS( o_i(ji,jj,jl) ) > rdt_ice ) .OR. &
                      ( ABS( o_i(ji,jj,jl) ) < 0._wp) ) .AND. &
                             ( a_i(ji,jj,jl) > 0._wp ) ) THEN
                  !CALL lim_prt( kt, ji, jj, 1, ' ALERTE 9 :   Wrong ice age ')
                  inb_alp(ialert_id) = inb_alp(ialert_id) + 1
               ENDIF
            END DO
         END DO
      END DO
 
      ! Alert on salt flux
      ialert_id = 5 ! reference number of this alert
      cl_alname(ialert_id) = ' High salt flux               ' ! name of the alert
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF( ABS( sfx (ji,jj) ) > 1.0e-2 ) THEN  ! = 1 psu/day for 1m ocean depth
               !CALL lim_prt( kt, ji, jj, 3, ' ALERTE 5 :   High salt flux ' )
               !DO jl = 1, jpl
                  !WRITE(numout,*) ' Category no: ', jl
                  !WRITE(numout,*) ' a_i        : ', a_i      (ji,jj,jl) , ' a_i_b      : ', a_i_b  (ji,jj,jl)   
                  !WRITE(numout,*) ' v_i        : ', v_i      (ji,jj,jl) , ' v_i_b      : ', v_i_b  (ji,jj,jl)   
                  !WRITE(numout,*) ' '
               !END DO
               inb_alp(ialert_id) = inb_alp(ialert_id) + 1
            ENDIF
         END DO
      END DO

      ! Alert if qns very big
      ialert_id = 8 ! reference number of this alert
      cl_alname(ialert_id) = ' fnsolar very big             ' ! name of the alert
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF( ABS( qns(ji,jj) ) > 1500._wp .AND. at_i(ji,jj) > 0._wp ) THEN
               !
               !WRITE(numout,*) ' ALERTE 8 :   Very high non-solar heat flux'
               !WRITE(numout,*) ' ji, jj    : ', ji, jj
               !WRITE(numout,*) ' qns       : ', qns(ji,jj)
               !WRITE(numout,*) ' sst       : ', sst_m(ji,jj)
               !WRITE(numout,*) ' sss       : ', sss_m(ji,jj)
               !
               !CALL lim_prt( kt, ji, jj, 2, '   ')
               inb_alp(ialert_id) = inb_alp(ialert_id) + 1
               !
            ENDIF
         END DO
      END DO
      !+++++
 
      ! Alert if very warm ice
      ialert_id = 10 ! reference number of this alert
      cl_alname(ialert_id) = ' Very warm ice                ' ! name of the alert
      inb_alp(ialert_id) = 0
      DO jl = 1, jpl
         DO jk = 1, nlay_i
            DO jj = 1, jpj
               DO ji = 1, jpi
                  ztmelts    =  -tmut * s_i(ji,jj,jk,jl) + rt0
                  IF( t_i(ji,jj,jk,jl) >= ztmelts  .AND.  v_i(ji,jj,jl) > 1.e-10   &
                     &                             .AND.  a_i(ji,jj,jl) > 0._wp   ) THEN
                     !WRITE(numout,*) ' ALERTE 10 :   Very warm ice'
                     !WRITE(numout,*) ' ji, jj, jk, jl : ', ji, jj, jk, jl
                     !WRITE(numout,*) ' t_i : ', t_i(ji,jj,jk,jl)
                     !WRITE(numout,*) ' e_i : ', e_i(ji,jj,jk,jl)
                     !WRITE(numout,*) ' s_i : ', s_i(ji,jj,jk,jl)
                     !WRITE(numout,*) ' ztmelts : ', ztmelts
                     inb_alp(ialert_id) = inb_alp(ialert_id) + 1
                  ENDIF
               END DO
            END DO
         END DO
      END DO

      ! sum of the alerts on all processors
      IF( lk_mpp ) THEN
         DO ialert_id = 1, inb_altests
            CALL mpp_sum(inb_alp(ialert_id))
         END DO
      ENDIF

      ! print alerts
      IF( lwp ) THEN
         ialert_id = 1                                 ! reference number of this alert
         cl_alname(ialert_id) = ' NO alerte 1      '   ! name of the alert
         WRITE(numout,*) ' time step ',kt
         WRITE(numout,*) ' All alerts at the end of ice model '
         DO ialert_id = 1, inb_altests
            WRITE(numout,*) ialert_id, cl_alname(ialert_id)//' : ', inb_alp(ialert_id), ' times ! '
         END DO
      ENDIF
     !
   END SUBROUTINE lim_ctl
 
   
   SUBROUTINE lim_prt( kt, ki, kj, kn, cd1 )
      !!-----------------------------------------------------------------------
      !!                   ***  ROUTINE lim_prt *** 
      !!                 
      !! ** Purpose :   Writes global ice state on the (i,j) point 
      !!                in ocean.ouput 
      !!                3 possibilities exist 
      !!                n = 1/-1 -> simple ice state (plus Mechanical Check if -1)
      !!                n = 2    -> exhaustive state
      !!                n = 3    -> ice/ocean salt fluxes
      !!
      !! ** input   :   point coordinates (i,j) 
      !!                n : number of the option
      !!-------------------------------------------------------------------
      INTEGER         , INTENT(in) ::   kt            ! ocean time step
      INTEGER         , INTENT(in) ::   ki, kj, kn    ! ocean gridpoint indices
      CHARACTER(len=*), INTENT(in) ::   cd1           !
      !!
      INTEGER :: jl, ji, jj
      !!-------------------------------------------------------------------

      DO ji = mi0(ki), mi1(ki)
         DO jj = mj0(kj), mj1(kj)

            WRITE(numout,*) ' time step ',kt,' ',cd1             ! print title

            !----------------
            !  Simple state
            !----------------
            
            IF ( kn == 1 .OR. kn == -1 ) THEN
               WRITE(numout,*) ' lim_prt - Point : ',ji,jj
               WRITE(numout,*) ' ~~~~~~~~~~~~~~ '
               WRITE(numout,*) ' Simple state '
               WRITE(numout,*) ' masks s,u,v   : ', tmask(ji,jj,1), umask(ji,jj,1), vmask(ji,jj,1)
               WRITE(numout,*) ' lat - long    : ', gphit(ji,jj), glamt(ji,jj)
               WRITE(numout,*) ' Time step     : ', numit
               WRITE(numout,*) ' - Ice drift   '
               WRITE(numout,*) '   ~~~~~~~~~~~ '
               WRITE(numout,*) ' u_ice(i-1,j)  : ', u_ice(ji-1,jj)
               WRITE(numout,*) ' u_ice(i  ,j)  : ', u_ice(ji,jj)
               WRITE(numout,*) ' v_ice(i  ,j-1): ', v_ice(ji,jj-1)
               WRITE(numout,*) ' v_ice(i  ,j)  : ', v_ice(ji,jj)
               WRITE(numout,*) ' strength      : ', strength(ji,jj)
               WRITE(numout,*)
               WRITE(numout,*) ' - Cell values '
               WRITE(numout,*) '   ~~~~~~~~~~~ '
               WRITE(numout,*) ' cell area     : ', e12t(ji,jj)
               WRITE(numout,*) ' at_i          : ', at_i(ji,jj)       
               WRITE(numout,*) ' vt_i          : ', vt_i(ji,jj)       
               WRITE(numout,*) ' vt_s          : ', vt_s(ji,jj)       
               DO jl = 1, jpl
                  WRITE(numout,*) ' - Category (', jl,')'
                  WRITE(numout,*) ' a_i           : ', a_i(ji,jj,jl)
                  WRITE(numout,*) ' ht_i          : ', ht_i(ji,jj,jl)
                  WRITE(numout,*) ' ht_s          : ', ht_s(ji,jj,jl)
                  WRITE(numout,*) ' v_i           : ', v_i(ji,jj,jl)
                  WRITE(numout,*) ' v_s           : ', v_s(ji,jj,jl)
                  WRITE(numout,*) ' e_s           : ', e_s(ji,jj,1,jl)
                  WRITE(numout,*) ' e_i           : ', e_i(ji,jj,1:nlay_i,jl)
                  WRITE(numout,*) ' t_su          : ', t_su(ji,jj,jl)
                  WRITE(numout,*) ' t_snow        : ', t_s(ji,jj,1,jl)
                  WRITE(numout,*) ' t_i           : ', t_i(ji,jj,1:nlay_i,jl)
                  WRITE(numout,*) ' sm_i          : ', sm_i(ji,jj,jl)
                  WRITE(numout,*) ' smv_i         : ', smv_i(ji,jj,jl)
                  WRITE(numout,*)
               END DO
            ENDIF
            IF( kn == -1 ) THEN
               WRITE(numout,*) ' Mechanical Check ************** '
               WRITE(numout,*) ' Check what means ice divergence '
               WRITE(numout,*) ' Total ice concentration ', at_i (ji,jj)
               WRITE(numout,*) ' Total lead fraction     ', ato_i(ji,jj)
               WRITE(numout,*) ' Sum of both             ', ato_i(ji,jj) + at_i(ji,jj)
               WRITE(numout,*) ' Sum of both minus 1     ', ato_i(ji,jj) + at_i(ji,jj) - 1.00
            ENDIF
            

            !--------------------
            !  Exhaustive state
            !--------------------
            
            IF ( kn .EQ. 2 ) THEN
               WRITE(numout,*) ' lim_prt - Point : ',ji,jj
               WRITE(numout,*) ' ~~~~~~~~~~~~~~ '
               WRITE(numout,*) ' Exhaustive state '
               WRITE(numout,*) ' lat - long ', gphit(ji,jj), glamt(ji,jj)
               WRITE(numout,*) ' Time step ', numit
               WRITE(numout,*) 
               WRITE(numout,*) ' - Cell values '
               WRITE(numout,*) '   ~~~~~~~~~~~ '
               WRITE(numout,*) ' cell area     : ', e12t(ji,jj)
               WRITE(numout,*) ' at_i          : ', at_i(ji,jj)       
               WRITE(numout,*) ' vt_i          : ', vt_i(ji,jj)       
               WRITE(numout,*) ' vt_s          : ', vt_s(ji,jj)       
               WRITE(numout,*) ' u_ice(i-1,j)  : ', u_ice(ji-1,jj)
               WRITE(numout,*) ' u_ice(i  ,j)  : ', u_ice(ji,jj)
               WRITE(numout,*) ' v_ice(i  ,j-1): ', v_ice(ji,jj-1)
               WRITE(numout,*) ' v_ice(i  ,j)  : ', v_ice(ji,jj)
               WRITE(numout,*) ' strength      : ', strength(ji,jj)
               WRITE(numout,*) ' u_ice_b       : ', u_ice_b(ji,jj)    , ' v_ice_b       : ', v_ice_b(ji,jj)  
               WRITE(numout,*)
               
               DO jl = 1, jpl
                  WRITE(numout,*) ' - Category (',jl,')'
                  WRITE(numout,*) '   ~~~~~~~~         ' 
                  WRITE(numout,*) ' ht_i       : ', ht_i(ji,jj,jl)             , ' ht_s       : ', ht_s(ji,jj,jl)
                  WRITE(numout,*) ' t_i        : ', t_i(ji,jj,1:nlay_i,jl)
                  WRITE(numout,*) ' t_su       : ', t_su(ji,jj,jl)             , ' t_s        : ', t_s(ji,jj,1,jl)
                  WRITE(numout,*) ' sm_i       : ', sm_i(ji,jj,jl)             , ' o_i        : ', o_i(ji,jj,jl)
                  WRITE(numout,*) ' a_i        : ', a_i(ji,jj,jl)              , ' a_i_b      : ', a_i_b(ji,jj,jl)   
                  WRITE(numout,*) ' v_i        : ', v_i(ji,jj,jl)              , ' v_i_b      : ', v_i_b(ji,jj,jl)   
                  WRITE(numout,*) ' v_s        : ', v_s(ji,jj,jl)              , ' v_s_b      : ', v_s_b(ji,jj,jl)  
                  WRITE(numout,*) ' e_i1       : ', e_i(ji,jj,1,jl)            , ' ei1        : ', e_i_b(ji,jj,1,jl) 
                  WRITE(numout,*) ' e_i2       : ', e_i(ji,jj,2,jl)            , ' ei2_b      : ', e_i_b(ji,jj,2,jl)  
                  WRITE(numout,*) ' e_snow     : ', e_s(ji,jj,1,jl)            , ' e_snow_b   : ', e_s_b(ji,jj,1,jl) 
                  WRITE(numout,*) ' smv_i      : ', smv_i(ji,jj,jl)            , ' smv_i_b    : ', smv_i_b(ji,jj,jl)   
                  WRITE(numout,*) ' oa_i       : ', oa_i(ji,jj,jl)             , ' oa_i_b     : ', oa_i_b(ji,jj,jl)
               END DO !jl
               
               WRITE(numout,*)
               WRITE(numout,*) ' - Heat / FW fluxes '
               WRITE(numout,*) '   ~~~~~~~~~~~~~~~~ '
               WRITE(numout,*) ' - Heat fluxes in and out the ice ***'
               WRITE(numout,*) ' qsr_ini       : ', pfrld(ji,jj) * qsr(ji,jj) + SUM( a_i_b(ji,jj,:) * qsr_ice(ji,jj,:) )
               WRITE(numout,*) ' qns_ini       : ', pfrld(ji,jj) * qns(ji,jj) + SUM( a_i_b(ji,jj,:) * qns_ice(ji,jj,:) )
               WRITE(numout,*)
               WRITE(numout,*) 
               WRITE(numout,*) ' sst        : ', sst_m(ji,jj)  
               WRITE(numout,*) ' sss        : ', sss_m(ji,jj)  
               WRITE(numout,*) 
               WRITE(numout,*) ' - Stresses '
               WRITE(numout,*) '   ~~~~~~~~ '
               WRITE(numout,*) ' utau_ice   : ', utau_ice(ji,jj) 
               WRITE(numout,*) ' vtau_ice   : ', vtau_ice(ji,jj)
               WRITE(numout,*) ' utau       : ', utau    (ji,jj) 
               WRITE(numout,*) ' vtau       : ', vtau    (ji,jj)
               WRITE(numout,*) ' oc. vel. u : ', u_oce   (ji,jj)
               WRITE(numout,*) ' oc. vel. v : ', v_oce   (ji,jj)
            ENDIF
            
            !---------------------
            ! Salt / heat fluxes
            !---------------------
            
            IF ( kn .EQ. 3 ) THEN
               WRITE(numout,*) ' lim_prt - Point : ',ji,jj
               WRITE(numout,*) ' ~~~~~~~~~~~~~~ '
               WRITE(numout,*) ' - Salt / Heat Fluxes '
               WRITE(numout,*) '   ~~~~~~~~~~~~~~~~ '
               WRITE(numout,*) ' lat - long ', gphit(ji,jj), glamt(ji,jj)
               WRITE(numout,*) ' Time step ', numit
               WRITE(numout,*)
               WRITE(numout,*) ' - Heat fluxes at bottom interface ***'
               WRITE(numout,*) ' qsr       : ', qsr(ji,jj)
               WRITE(numout,*) ' qns       : ', qns(ji,jj)
               WRITE(numout,*)
               WRITE(numout,*) ' hfx_mass     : ', hfx_thd(ji,jj) + hfx_dyn(ji,jj) + hfx_snw(ji,jj) + hfx_res(ji,jj)
               WRITE(numout,*) ' hfx_in       : ', hfx_in(ji,jj)
               WRITE(numout,*) ' hfx_out      : ', hfx_out(ji,jj)
               WRITE(numout,*) ' dhc          : ', diag_heat(ji,jj)              
               WRITE(numout,*)
               WRITE(numout,*) ' hfx_dyn      : ', hfx_dyn(ji,jj)
               WRITE(numout,*) ' hfx_thd      : ', hfx_thd(ji,jj)
               WRITE(numout,*) ' hfx_res      : ', hfx_res(ji,jj)
               WRITE(numout,*) ' fhtur        : ', fhtur(ji,jj) 
               WRITE(numout,*) ' qlead        : ', qlead(ji,jj) * r1_rdtice
               WRITE(numout,*)
               WRITE(numout,*) ' - Salt fluxes at bottom interface ***'
               WRITE(numout,*) ' emp       : ', emp    (ji,jj)
               WRITE(numout,*) ' sfx       : ', sfx    (ji,jj)
               WRITE(numout,*) ' sfx_res   : ', sfx_res(ji,jj)
               WRITE(numout,*) ' sfx_bri   : ', sfx_bri(ji,jj)
               WRITE(numout,*) ' sfx_dyn   : ', sfx_dyn(ji,jj)
               WRITE(numout,*)
               WRITE(numout,*) ' - Momentum fluxes '
               WRITE(numout,*) ' utau      : ', utau(ji,jj) 
               WRITE(numout,*) ' vtau      : ', vtau(ji,jj)
            ENDIF 
            WRITE(numout,*) ' '
            !
         END DO
      END DO
      !
   END SUBROUTINE lim_prt

#else
   !!--------------------------------------------------------------------------
   !!   Default option         Empty Module               No LIM3 sea-ice model
   !!--------------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_ctl     ! Empty routine
   END SUBROUTINE lim_ctl
   SUBROUTINE lim_prt     ! Empty routine
   END SUBROUTINE lim_prt
#endif
   !!======================================================================
END MODULE limctl
