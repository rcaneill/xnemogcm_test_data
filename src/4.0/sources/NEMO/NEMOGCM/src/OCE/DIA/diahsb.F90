MODULE diahsb
   !!======================================================================
   !!                       ***  MODULE  diahsb  ***
   !! Ocean diagnostics: Heat, salt and volume budgets
   !!======================================================================
   !! History :  3.3  ! 2010-09  (M. Leclair)  Original code 
   !!                 ! 2012-10  (C. Rousset)  add iom_put
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dia_hsb       : Diagnose the conservation of ocean heat and salt contents, and volume
   !!   dia_hsb_rst   : Read or write DIA file in restart file
   !!   dia_hsb_init  : Initialization of the conservation diagnostic
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
   USE dom_oce        ! ocean space and time domain
   USE phycst         ! physical constants
   USE sbc_oce        ! surface thermohaline fluxes
   USE sbcrnf         ! river runoff
   USE sbcisf         ! ice shelves
   USE domvvl         ! vertical scale factors
   USE traqsr         ! penetrative solar radiation
   USE trabbc         ! bottom boundary condition 
   USE trabbc         ! bottom boundary condition
   USE restart        ! ocean restart
   USE bdy_oce , ONLY : ln_bdy
   !
   USE iom            ! I/O manager
   USE in_out_manager ! I/O manager
   USE lib_fortran    ! glob_sum
   USE lib_mpp        ! distributed memory computing library
   USE timing         ! preformance summary

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dia_hsb        ! routine called by step.F90
   PUBLIC   dia_hsb_init   ! routine called by nemogcm.F90

   LOGICAL, PUBLIC ::   ln_diahsb   !: check the heat and salt budgets

   REAL(wp) ::   surf_tot              ! ocean surface
   REAL(wp) ::   frc_t, frc_s, frc_v   ! global forcing trends
   REAL(wp) ::   frc_wn_t, frc_wn_s    ! global forcing trends
   !
   REAL(wp), DIMENSION(:,:)  , ALLOCATABLE ::   surf 
   REAL(wp), DIMENSION(:,:)  , ALLOCATABLE ::   surf_ini      , ssh_ini          !
   REAL(wp), DIMENSION(:,:)  , ALLOCATABLE ::   ssh_hc_loc_ini, ssh_sc_loc_ini   !
   REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   hc_loc_ini, sc_loc_ini, e3t_ini  !

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: diahsb.F90 10425 2018-12-19 21:54:16Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dia_hsb( kt )
      !!---------------------------------------------------------------------------
      !!                  ***  ROUTINE dia_hsb  ***
      !!     
      !! ** Purpose: Compute the ocean global heat content, salt content and volume conservation
      !!	
      !! ** Method : - Compute the deviation of heat content, salt content and volume
      !!	            at the current time step from their values at nit000
      !!	            - Compute the contribution of forcing and remove it from these deviations
      !!
      !!---------------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time-step index
      !
      INTEGER    ::   ji, jj, jk                  ! dummy loop indice
      REAL(wp)   ::   zdiff_hc    , zdiff_sc      ! heat and salt content variations
      REAL(wp)   ::   zdiff_hc1   , zdiff_sc1     !  -         -     -        - 
      REAL(wp)   ::   zdiff_v1    , zdiff_v2      ! volume variation
      REAL(wp)   ::   zerr_hc1    , zerr_sc1      ! heat and salt content misfit
      REAL(wp)   ::   zvol_tot                    ! volume
      REAL(wp)   ::   z_frc_trd_t , z_frc_trd_s   !    -     -
      REAL(wp)   ::   z_frc_trd_v                 !    -     -
      REAL(wp)   ::   z_wn_trd_t , z_wn_trd_s     !    -     -
      REAL(wp)   ::   z_ssh_hc , z_ssh_sc         !    -     -
      REAL(wp), DIMENSION(jpi,jpj)       ::   z2d0, z2d1   ! 2D workspace
      REAL(wp), DIMENSION(jpi,jpj,jpkm1) ::   zwrk         ! 3D workspace
      !!---------------------------------------------------------------------------
      IF( ln_timing )   CALL timing_start('dia_hsb')      
      !
      tsn(:,:,:,1) = tsn(:,:,:,1) * tmask(:,:,:) ; tsb(:,:,:,1) = tsb(:,:,:,1) * tmask(:,:,:) ;
      tsn(:,:,:,2) = tsn(:,:,:,2) * tmask(:,:,:) ; tsb(:,:,:,2) = tsb(:,:,:,2) * tmask(:,:,:) ;
      ! ------------------------- !
      ! 1 - Trends due to forcing !
      ! ------------------------- !
      z_frc_trd_v = r1_rau0 * glob_sum( 'diahsb', - ( emp(:,:) - rnf(:,:) + fwfisf(:,:) ) * surf(:,:) )   ! volume fluxes
      z_frc_trd_t =           glob_sum( 'diahsb', sbc_tsc(:,:,jp_tem) * surf(:,:) )                       ! heat fluxes
      z_frc_trd_s =           glob_sum( 'diahsb', sbc_tsc(:,:,jp_sal) * surf(:,:) )                       ! salt fluxes
      !                    !  Add runoff    heat & salt input
      IF( ln_rnf    )   z_frc_trd_t = z_frc_trd_t + glob_sum( 'diahsb', rnf_tsc(:,:,jp_tem) * surf(:,:) )
      IF( ln_rnf_sal)   z_frc_trd_s = z_frc_trd_s + glob_sum( 'diahsb', rnf_tsc(:,:,jp_sal) * surf(:,:) )
      !                    ! Add ice shelf heat & salt input
      IF( ln_isf    )   z_frc_trd_t = z_frc_trd_t + glob_sum( 'diahsb', risf_tsc(:,:,jp_tem) * surf(:,:) )
      !                    ! Add penetrative solar radiation
      IF( ln_traqsr )   z_frc_trd_t = z_frc_trd_t + r1_rau0_rcp * glob_sum( 'diahsb', qsr     (:,:) * surf(:,:) )
      !                    ! Add geothermal heat flux
      IF( ln_trabbc )   z_frc_trd_t = z_frc_trd_t +               glob_sum( 'diahsb', qgh_trd0(:,:) * surf(:,:) )
      !
      IF( ln_linssh ) THEN
         IF( ln_isfcav ) THEN
            DO ji=1,jpi
               DO jj=1,jpj
                  z2d0(ji,jj) = surf(ji,jj) * wn(ji,jj,mikt(ji,jj)) * tsb(ji,jj,mikt(ji,jj),jp_tem)
                  z2d1(ji,jj) = surf(ji,jj) * wn(ji,jj,mikt(ji,jj)) * tsb(ji,jj,mikt(ji,jj),jp_sal)
               END DO
            END DO
         ELSE
            z2d0(:,:) = surf(:,:) * wn(:,:,1) * tsb(:,:,1,jp_tem)
            z2d1(:,:) = surf(:,:) * wn(:,:,1) * tsb(:,:,1,jp_sal)
         END IF
         z_wn_trd_t = - glob_sum( 'diahsb', z2d0 ) 
         z_wn_trd_s = - glob_sum( 'diahsb', z2d1 )
      ENDIF

      frc_v = frc_v + z_frc_trd_v * rdt
      frc_t = frc_t + z_frc_trd_t * rdt
      frc_s = frc_s + z_frc_trd_s * rdt
      !                                          ! Advection flux through fixed surface (z=0)
      IF( ln_linssh ) THEN
         frc_wn_t = frc_wn_t + z_wn_trd_t * rdt
         frc_wn_s = frc_wn_s + z_wn_trd_s * rdt
      ENDIF

      ! ------------------------ !
      ! 2 -  Content variations  !
      ! ------------------------ !
      ! glob_sum_full is needed because you keep the full interior domain to compute the sum (iscpl)

      !                    ! volume variation (calculated with ssh)
      zdiff_v1 = glob_sum_full( 'diahsb', surf(:,:)*sshn(:,:) - surf_ini(:,:)*ssh_ini(:,:) )

      !                    ! heat & salt content variation (associated with ssh)
      IF( ln_linssh ) THEN       ! linear free surface case
         IF( ln_isfcav ) THEN          ! ISF case
            DO ji = 1, jpi
               DO jj = 1, jpj
                  z2d0(ji,jj) = surf(ji,jj) * ( tsn(ji,jj,mikt(ji,jj),jp_tem) * sshn(ji,jj) - ssh_hc_loc_ini(ji,jj) ) 
                  z2d1(ji,jj) = surf(ji,jj) * ( tsn(ji,jj,mikt(ji,jj),jp_sal) * sshn(ji,jj) - ssh_sc_loc_ini(ji,jj) ) 
               END DO
            END DO
         ELSE                          ! no under ice-shelf seas
            z2d0(:,:) = surf(:,:) * ( tsn(:,:,1,jp_tem) * sshn(:,:) - ssh_hc_loc_ini(:,:) ) 
            z2d1(:,:) = surf(:,:) * ( tsn(:,:,1,jp_sal) * sshn(:,:) - ssh_sc_loc_ini(:,:) ) 
         END IF
         z_ssh_hc = glob_sum_full( 'diahsb', z2d0 ) 
         z_ssh_sc = glob_sum_full( 'diahsb', z2d1 ) 
      ENDIF
      !
      DO jk = 1, jpkm1           ! volume variation (calculated with scale factors)
         zwrk(:,:,jk) = ( surf(:,:)*e3t_n(:,:,jk) - surf_ini(:,:)*e3t_ini(:,:,jk) ) * tmask(:,:,jk)
      END DO
      zdiff_v2 = glob_sum_full( 'diahsb', zwrk(:,:,:) )
      DO jk = 1, jpkm1           ! heat content variation
         zwrk(:,:,jk) = ( surf(:,:)*e3t_n(:,:,jk)*tsn(:,:,jk,jp_tem) - surf_ini(:,:)*hc_loc_ini(:,:,jk) ) * tmask(:,:,jk)
      END DO
      zdiff_hc = glob_sum_full( 'diahsb', zwrk(:,:,:) )
      DO jk = 1, jpkm1           ! salt content variation
         zwrk(:,:,jk) = ( surf(:,:)*e3t_n(:,:,jk)*tsn(:,:,jk,jp_sal) - surf_ini(:,:)*sc_loc_ini(:,:,jk) ) * tmask(:,:,jk)
      END DO
      zdiff_sc = glob_sum_full( 'diahsb', zwrk(:,:,:) )

      ! ------------------------ !
      ! 3 -  Drifts              !
      ! ------------------------ !
      zdiff_v1 = zdiff_v1 - frc_v
      IF( .NOT.ln_linssh )   zdiff_v2 = zdiff_v2 - frc_v
      zdiff_hc = zdiff_hc - frc_t
      zdiff_sc = zdiff_sc - frc_s
      IF( ln_linssh ) THEN
         zdiff_hc1 = zdiff_hc + z_ssh_hc 
         zdiff_sc1 = zdiff_sc + z_ssh_sc
         zerr_hc1  = z_ssh_hc - frc_wn_t
         zerr_sc1  = z_ssh_sc - frc_wn_s
      ENDIF

      ! ----------------------- !
      ! 4 - Diagnostics writing !
      ! ----------------------- !
      DO jk = 1, jpkm1           ! total ocean volume (calculated with scale factors)
         zwrk(:,:,jk) = surf(:,:) * e3t_n(:,:,jk) * tmask(:,:,jk)
      END DO
      zvol_tot = glob_sum_full( 'diahsb', zwrk(:,:,:) )

!!gm to be added ?
!      IF( ln_linssh ) THEN            ! fixed volume, add the ssh contribution
!        zvol_tot = zvol_tot + glob_sum( 'diahsb', surf(:,:) * sshn(:,:) )
!      ENDIF
!!gm end

      CALL iom_put(   'bgfrcvol' , frc_v    * 1.e-9    )              ! vol - surface forcing (km3) 
      CALL iom_put(   'bgfrctem' , frc_t    * rau0 * rcp * 1.e-20 )   ! hc  - surface forcing (1.e20 J) 
      CALL iom_put(   'bgfrchfx' , frc_t    * rau0 * rcp /  &         ! hc  - surface forcing (W/m2) 
         &                       ( surf_tot * kt * rdt )        )
      CALL iom_put(   'bgfrcsal' , frc_s    * 1.e-9    )              ! sc  - surface forcing (psu*km3) 

      IF( .NOT. ln_linssh ) THEN
         CALL iom_put( 'bgtemper' , zdiff_hc / zvol_tot )              ! Temperature drift     (C) 
         CALL iom_put( 'bgsaline' , zdiff_sc / zvol_tot )              ! Salinity    drift     (PSU)
         CALL iom_put( 'bgheatco' , zdiff_hc * 1.e-20 * rau0 * rcp )   ! Heat content drift    (1.e20 J) 
         CALL iom_put( 'bgheatfx' , zdiff_hc * rau0 * rcp /  &         ! Heat flux drift       (W/m2) 
            &                       ( surf_tot * kt * rdt )        )
         CALL iom_put( 'bgsaltco' , zdiff_sc * 1.e-9    )              ! Salt content drift    (psu*km3)
         CALL iom_put( 'bgvolssh' , zdiff_v1 * 1.e-9    )              ! volume ssh drift      (km3)  
         CALL iom_put( 'bgvole3t' , zdiff_v2 * 1.e-9    )              ! volume e3t drift      (km3)  
         !
         IF( kt == nitend .AND. lwp ) THEN
            WRITE(numout,*)
            WRITE(numout,*) 'dia_hsb : last time step hsb diagnostics: at it= ', kt,' date= ', ndastp
            WRITE(numout,*) '~~~~~~~'
            WRITE(numout,*) '   Temperature drift = ', zdiff_hc / zvol_tot, ' C'
            WRITE(numout,*) '   Salinity    drift = ', zdiff_sc / zvol_tot, ' PSU'
            WRITE(numout,*) '   volume ssh  drift = ', zdiff_v1 * 1.e-9   , ' km^3'
            WRITE(numout,*) '   volume e3t  drift = ', zdiff_v2 * 1.e-9   , ' km^3'
         ENDIF
         !
      ELSE
         CALL iom_put( 'bgtemper' , zdiff_hc1 / zvol_tot)              ! Heat content drift    (C) 
         CALL iom_put( 'bgsaline' , zdiff_sc1 / zvol_tot)              ! Salt content drift    (PSU)
         CALL iom_put( 'bgheatco' , zdiff_hc1 * 1.e-20 * rau0 * rcp )  ! Heat content drift    (1.e20 J) 
         CALL iom_put( 'bgheatfx' , zdiff_hc1 * rau0 * rcp /  &        ! Heat flux drift       (W/m2) 
            &                       ( surf_tot * kt * rdt )         )
         CALL iom_put( 'bgsaltco' , zdiff_sc1 * 1.e-9    )             ! Salt content drift    (psu*km3)
         CALL iom_put( 'bgvolssh' , zdiff_v1 * 1.e-9    )              ! volume ssh drift      (km3)  
         CALL iom_put( 'bgmistem' , zerr_hc1 / zvol_tot )              ! hc  - error due to free surface (C)
         CALL iom_put( 'bgmissal' , zerr_sc1 / zvol_tot )              ! sc  - error due to free surface (psu)
      ENDIF
      !
      IF( lrst_oce )   CALL dia_hsb_rst( kt, 'WRITE' )
      !
      IF( ln_timing )   CALL timing_stop('dia_hsb')
      !
   END SUBROUTINE dia_hsb


   SUBROUTINE dia_hsb_rst( kt, cdrw )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE dia_hsb_rst  ***
      !!                     
      !! ** Purpose :   Read or write DIA file in restart file
      !!
      !! ** Method  :   use of IOM library
      !!----------------------------------------------------------------------
      INTEGER         , INTENT(in) ::   kt     ! ocean time-step
      CHARACTER(len=*), INTENT(in) ::   cdrw   ! "READ"/"WRITE" flag
      !
      INTEGER ::   ji, jj, jk   ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      IF( TRIM(cdrw) == 'READ' ) THEN        ! Read/initialise 
         IF( ln_rstart ) THEN                   !* Read the restart file
            !
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) '   dia_hsb_rst : read hsb restart at it= ', kt,' date= ', ndastp
            IF(lwp) WRITE(numout,*)
            CALL iom_get( numror, 'frc_v', frc_v, ldxios = lrxios )
            CALL iom_get( numror, 'frc_t', frc_t, ldxios = lrxios )
            CALL iom_get( numror, 'frc_s', frc_s, ldxios = lrxios )
            IF( ln_linssh ) THEN
               CALL iom_get( numror, 'frc_wn_t', frc_wn_t, ldxios = lrxios )
               CALL iom_get( numror, 'frc_wn_s', frc_wn_s, ldxios = lrxios )
            ENDIF
            CALL iom_get( numror, jpdom_autoglo, 'surf_ini'  , surf_ini  , ldxios = lrxios ) ! ice sheet coupling
            CALL iom_get( numror, jpdom_autoglo, 'ssh_ini'   , ssh_ini   , ldxios = lrxios )
            CALL iom_get( numror, jpdom_autoglo, 'e3t_ini'   , e3t_ini   , ldxios = lrxios )
            CALL iom_get( numror, jpdom_autoglo, 'hc_loc_ini', hc_loc_ini, ldxios = lrxios )
            CALL iom_get( numror, jpdom_autoglo, 'sc_loc_ini', sc_loc_ini, ldxios = lrxios )
            IF( ln_linssh ) THEN
               CALL iom_get( numror, jpdom_autoglo, 'ssh_hc_loc_ini', ssh_hc_loc_ini, ldxios = lrxios )
               CALL iom_get( numror, jpdom_autoglo, 'ssh_sc_loc_ini', ssh_sc_loc_ini, ldxios = lrxios )
            ENDIF
         ELSE
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) '   dia_hsb_rst : initialise hsb at initial state '
            IF(lwp) WRITE(numout,*)
            surf_ini(:,:) = e1e2t(:,:) * tmask_i(:,:)         ! initial ocean surface
            ssh_ini(:,:) = sshn(:,:)                          ! initial ssh
            DO jk = 1, jpk
              ! if ice sheet/oceqn coupling, need to mask ini variables here (mask could change at the next NEMO instance).
               e3t_ini   (:,:,jk) = e3t_n(:,:,jk)                      * tmask(:,:,jk)  ! initial vertical scale factors
               hc_loc_ini(:,:,jk) = tsn(:,:,jk,jp_tem) * e3t_n(:,:,jk) * tmask(:,:,jk)  ! initial heat content
               sc_loc_ini(:,:,jk) = tsn(:,:,jk,jp_sal) * e3t_n(:,:,jk) * tmask(:,:,jk)  ! initial salt content
            END DO
            frc_v = 0._wp                                           ! volume       trend due to forcing
            frc_t = 0._wp                                           ! heat content   -    -   -    -   
            frc_s = 0._wp                                           ! salt content   -    -   -    -        
            IF( ln_linssh ) THEN
               IF( ln_isfcav ) THEN
                  DO ji = 1, jpi
                     DO jj = 1, jpj
                        ssh_hc_loc_ini(ji,jj) = tsn(ji,jj,mikt(ji,jj),jp_tem) * sshn(ji,jj)   ! initial heat content in ssh
                        ssh_sc_loc_ini(ji,jj) = tsn(ji,jj,mikt(ji,jj),jp_sal) * sshn(ji,jj)   ! initial salt content in ssh
                     END DO
                   END DO
                ELSE
                  ssh_hc_loc_ini(:,:) = tsn(:,:,1,jp_tem) * sshn(:,:)   ! initial heat content in ssh
                  ssh_sc_loc_ini(:,:) = tsn(:,:,1,jp_sal) * sshn(:,:)   ! initial salt content in ssh
               END IF
               frc_wn_t = 0._wp                                       ! initial heat content misfit due to free surface
               frc_wn_s = 0._wp                                       ! initial salt content misfit due to free surface
            ENDIF
         ENDIF
         !
      ELSEIF( TRIM(cdrw) == 'WRITE' ) THEN   ! Create restart file
         !                                   ! -------------------
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '   dia_hsb_rst : write restart at it= ', kt,' date= ', ndastp
         IF(lwp) WRITE(numout,*)
         !
         IF( lwxios ) CALL iom_swap(      cwxios_context          )
         CALL iom_rstput( kt, nitrst, numrow, 'frc_v', frc_v, ldxios = lwxios )
         CALL iom_rstput( kt, nitrst, numrow, 'frc_t', frc_t, ldxios = lwxios )
         CALL iom_rstput( kt, nitrst, numrow, 'frc_s', frc_s, ldxios = lwxios )
         IF( ln_linssh ) THEN
            CALL iom_rstput( kt, nitrst, numrow, 'frc_wn_t', frc_wn_t, ldxios = lwxios )
            CALL iom_rstput( kt, nitrst, numrow, 'frc_wn_s', frc_wn_s, ldxios = lwxios )
         ENDIF
         CALL iom_rstput( kt, nitrst, numrow, 'surf_ini'  , surf_ini  , ldxios = lwxios )      ! ice sheet coupling
         CALL iom_rstput( kt, nitrst, numrow, 'ssh_ini'   , ssh_ini   , ldxios = lwxios )
         CALL iom_rstput( kt, nitrst, numrow, 'e3t_ini'   , e3t_ini   , ldxios = lwxios )
         CALL iom_rstput( kt, nitrst, numrow, 'hc_loc_ini', hc_loc_ini, ldxios = lwxios )
         CALL iom_rstput( kt, nitrst, numrow, 'sc_loc_ini', sc_loc_ini, ldxios = lwxios )
         IF( ln_linssh ) THEN
            CALL iom_rstput( kt, nitrst, numrow, 'ssh_hc_loc_ini', ssh_hc_loc_ini, ldxios = lwxios )
            CALL iom_rstput( kt, nitrst, numrow, 'ssh_sc_loc_ini', ssh_sc_loc_ini, ldxios = lwxios )
         ENDIF
         IF( lwxios ) CALL iom_swap(      cxios_context          )
         !
      ENDIF
      !
   END SUBROUTINE dia_hsb_rst


   SUBROUTINE dia_hsb_init
      !!---------------------------------------------------------------------------
      !!                  ***  ROUTINE dia_hsb  ***
      !!     
      !! ** Purpose: Initialization for the heat salt volume budgets
      !!	
      !! ** Method : Compute initial heat content, salt content and volume
      !!
      !! ** Action : - Compute initial heat content, salt content and volume
      !!             - Initialize forcing trends
      !!             - Compute coefficients for conversion
      !!---------------------------------------------------------------------------
      INTEGER ::   ierror, ios   ! local integer
      !!
      NAMELIST/namhsb/ ln_diahsb
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'dia_hsb_init : heat and salt budgets diagnostics'
         WRITE(numout,*) '~~~~~~~~~~~~ '
      ENDIF
      REWIND( numnam_ref )              ! Namelist namhsb in reference namelist
      READ  ( numnam_ref, namhsb, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namhsb in reference namelist', lwp )
      REWIND( numnam_cfg )              ! Namelist namhsb in configuration namelist
      READ  ( numnam_cfg, namhsb, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namhsb in configuration namelist', lwp )
      IF(lwm) WRITE( numond, namhsb )

      IF(lwp) THEN
         WRITE(numout,*) '   Namelist  namhsb :'
         WRITE(numout,*) '      check the heat and salt budgets (T) or not (F)       ln_diahsb = ', ln_diahsb
      ENDIF
      !
      IF( .NOT. ln_diahsb )   RETURN

      IF(lwxios) THEN
! define variables in restart file when writing with XIOS
        CALL iom_set_rstw_var_active('frc_v')
        CALL iom_set_rstw_var_active('frc_t')
        CALL iom_set_rstw_var_active('frc_s')
        CALL iom_set_rstw_var_active('surf_ini')
        CALL iom_set_rstw_var_active('ssh_ini')
        CALL iom_set_rstw_var_active('e3t_ini')
        CALL iom_set_rstw_var_active('hc_loc_ini')
        CALL iom_set_rstw_var_active('sc_loc_ini')
        IF( ln_linssh ) THEN
           CALL iom_set_rstw_var_active('ssh_hc_loc_ini')
           CALL iom_set_rstw_var_active('ssh_sc_loc_ini')
           CALL iom_set_rstw_var_active('frc_wn_t')
           CALL iom_set_rstw_var_active('frc_wn_s')
        ENDIF
      ENDIF
      ! ------------------- !
      ! 1 - Allocate memory !
      ! ------------------- !
      ALLOCATE( hc_loc_ini(jpi,jpj,jpk), sc_loc_ini(jpi,jpj,jpk), surf_ini(jpi,jpj), &
         &      e3t_ini(jpi,jpj,jpk), surf(jpi,jpj),  ssh_ini(jpi,jpj), STAT=ierror  )
      IF( ierror > 0 ) THEN
         CALL ctl_stop( 'dia_hsb_init: unable to allocate hc_loc_ini' )   ;   RETURN
      ENDIF

      IF( ln_linssh )   ALLOCATE( ssh_hc_loc_ini(jpi,jpj), ssh_sc_loc_ini(jpi,jpj),STAT=ierror )
      IF( ierror > 0 ) THEN
         CALL ctl_stop( 'dia_hsb: unable to allocate ssh_hc_loc_ini' )   ;   RETURN
      ENDIF

      ! ----------------------------------------------- !
      ! 2 - Time independant variables and file opening !
      ! ----------------------------------------------- !
      surf(:,:) = e1e2t(:,:) * tmask_i(:,:)               ! masked surface grid cell area
      surf_tot  = glob_sum( 'diahsb', surf(:,:) )         ! total ocean surface area

      IF( ln_bdy ) CALL ctl_warn( 'dia_hsb_init: heat/salt budget does not consider open boundary fluxes' )         
      !
      ! ---------------------------------- !
      ! 4 - initial conservation variables !
      ! ---------------------------------- !
      CALL dia_hsb_rst( nit000, 'READ' )  !* read or initialize all required files
      !
   END SUBROUTINE dia_hsb_init

   !!======================================================================
END MODULE diahsb
