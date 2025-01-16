MODULE traatf_qco
   !!======================================================================
   !!                       ***  MODULE  traatf_qco  ***
   !! Ocean active tracers:  Asselin time filtering for temperature and salinity
   !!======================================================================
   !! History :  OPA  !  1991-11  (G. Madec)  Original code
   !!            7.0  !  1993-03  (M. Guyon)  symetrical conditions
   !!            8.0  !  1996-02  (G. Madec & M. Imbard)  opa release 8.0
   !!             -   !  1996-04  (A. Weaver)  Euler forward step
   !!            8.2  !  1999-02  (G. Madec, N. Grima)  semi-implicit pressure grad.
   !!  NEMO      1.0  !  2002-08  (G. Madec)  F90: Free form and module
   !!             -   !  2002-11  (C. Talandier, A-M Treguier) Open boundaries
   !!             -   !  2005-04  (C. Deltel) Add Asselin trend in the ML budget
   !!            2.0  !  2006-02  (L. Debreu, C. Mazauric) Agrif implementation
   !!            3.0  !  2008-06  (G. Madec)  time stepping always done in trazdf
   !!            3.1  !  2009-02  (G. Madec, R. Benshila)  re-introduce the vvl option
   !!            3.3  !  2010-04  (M. Leclair, G. Madec)  semi-implicit hpg with asselin filter + modified LF-RA
   !!             -   !  2010-05  (C. Ethe, G. Madec)  merge TRC-TRA
   !!            4.1  !  2019-08  (A. Coward, D. Storkey) rename tranxt.F90 -> traatfLF.F90. Now only does time filtering.
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_atf       : time filtering on tracers
   !!   tra_atf_fix   : time filtering on tracers : fixed    volume case
   !!   tra_atf_vvl   : time filtering on tracers : variable volume case
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean space and time domain variables
   USE sbc_oce         ! surface boundary condition: ocean
   USE sbcrnf          ! river runoffs
   USE isf_oce         ! ice shelf melting
   USE zdf_oce         ! ocean vertical mixing
   USE domvvl          ! variable volume
   USE trd_oce         ! trends: ocean variables
   USE trdtra          ! trends manager: tracers
   USE traqsr          ! penetrative solar radiation (needed for nksr)
   USE phycst          ! physical constant
   USE ldftra          ! lateral physics : tracers
   USE ldfslp          ! lateral physics : slopes
   USE bdy_oce  , ONLY : ln_bdy
   USE bdytra          ! open boundary condition (bdy_tra routine)
   !
   USE in_out_manager  ! I/O manager
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE prtctl          ! Print control
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_atf_qco       ! routine called by step.F90
   PUBLIC   tra_atf_fix_lf    ! to be used in trcnxt !!st WARNING discrepancy here interpol is used by PISCES
   PUBLIC   tra_atf_qco_lf    ! to be used in trcnxt !!st WARNING discrepancy here interpol is used by PISCES

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: traatf_qco.F90 14433 2021-02-11 08:06:49Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tra_atf_qco( kt, Kbb, Kmm, Kaa, pts )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE traatfLF  ***
      !!
      !! ** Purpose :   Apply the boundary condition on the after temperature
      !!             and salinity fields and add the Asselin time filter on now fields.
      !!
      !! ** Method  :   At this stage of the computation, ta and sa are the
      !!             after temperature and salinity as the time stepping has
      !!             been performed in trazdf_imp or trazdf_exp module.
      !!
      !!              - Apply lateral boundary conditions on (ta,sa)
      !!             at the local domain   boundaries through lbc_lnk call,
      !!             at the one-way open boundaries (ln_bdy=T),
      !!             at the AGRIF zoom   boundaries (lk_agrif=T)
      !!
      !!              - Update lateral boundary conditions on AGRIF children
      !!             domains (lk_agrif=T)
      !!
      !! ** Action  : - ts(Kmm) time filtered
      !!----------------------------------------------------------------------
      INTEGER                                  , INTENT(in   ) :: kt             ! ocean time-step index
      INTEGER                                  , INTENT(in   ) :: Kbb, Kmm, Kaa  ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts,jpt), INTENT(inout) :: pts            ! active tracers
      !!
      INTEGER  ::   ji, jj, jk, jn   ! dummy loop indices
      REAL(wp) ::   zfact            ! local scalars
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   ztrdt, ztrds
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start( 'tra_atf_qco')
      !
      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'tra_atf_qco : apply Asselin time filter to "now" fields'
         IF(lwp) WRITE(numout,*) '~~~~~~~'
      ENDIF
!!st  Update after tracer on domain lateral boundaries as been removed outside

      ! trends computation initialisation
      IF( l_trdtra )   THEN
         ALLOCATE( ztrdt(jpi,jpj,jpk) , ztrds(jpi,jpj,jpk) )
         ztrdt(:,:,jpk) = 0._wp
         ztrds(:,:,jpk) = 0._wp
         IF( ln_traldf_iso ) THEN              ! diagnose the "pure" Kz diffusive trend
            CALL trd_tra( kt, Kmm, Kaa, 'TRA', jp_tem, jptra_zdfp, ztrdt )
            CALL trd_tra( kt, Kmm, Kaa, 'TRA', jp_sal, jptra_zdfp, ztrds )
         ENDIF
         ! total trend for the non-time-filtered variables.
         zfact = 1.0 / rn_Dt
         ! G Nurser 23 Mar 2017. Recalculate trend as Delta(e3t*T)/e3tn; e3tn cancel from pts(Kmm) terms
         DO jk = 1, jpkm1
            ztrdt(:,:,jk) = ( pts(:,:,jk,jp_tem,Kaa) * (1._wp + r3t(:,:,Kaa) * tmask(:,:,jk))/(1._wp + r3t(:,:,Kmm) * tmask(:,:,jk))  &
               &            - pts(:,:,jk,jp_tem,Kmm) ) * zfact
            ztrds(:,:,jk) = ( pts(:,:,jk,jp_sal,Kaa) * (1._wp + r3t(:,:,Kaa) * tmask(:,:,jk))/(1._wp + r3t(:,:,Kmm) * tmask(:,:,jk))  &
               &            - pts(:,:,jk,jp_sal,Kmm) ) * zfact
         END DO
         CALL trd_tra( kt, Kmm, Kaa, 'TRA', jp_tem, jptra_tot, ztrdt )
         CALL trd_tra( kt, Kmm, Kaa, 'TRA', jp_sal, jptra_tot, ztrds )
         IF( ln_linssh ) THEN       ! linear sea surface height only
            ! Store now fields before applying the Asselin filter
            ! in order to calculate Asselin filter trend later.
            ztrdt(:,:,:) = pts(:,:,:,jp_tem,Kmm)
            ztrds(:,:,:) = pts(:,:,:,jp_sal,Kmm)
         ENDIF
      ENDIF

      IF( l_1st_euler ) THEN       ! Euler time-stepping
         !
         IF (l_trdtra .AND. .NOT. ln_linssh ) THEN   ! Zero Asselin filter contribution must be explicitly written out since for vvl
            !                                        ! Asselin filter is output by tra_atf_vvl that is not called on this time step
            ztrdt(:,:,:) = 0._wp
            ztrds(:,:,:) = 0._wp
            CALL trd_tra( kt, Kmm, Kaa, 'TRA', jp_tem, jptra_atf, ztrdt )
            CALL trd_tra( kt, Kmm, Kaa, 'TRA', jp_sal, jptra_atf, ztrds )
         END IF
         !
      ELSE                                            ! Leap-Frog + Asselin filter time stepping
         !
         IF ( ln_linssh ) THEN   ;   CALL tra_atf_fix_lf( kt, Kbb, Kmm, Kaa, nit000,        'TRA', pts, jpts )  ! linear free surface
         ELSE                    ;   CALL tra_atf_qco_lf( kt, Kbb, Kmm, Kaa, nit000, rn_Dt, 'TRA', pts, sbc_tsc, sbc_tsc_b, jpts )  ! non-linear free surface
         ENDIF
         !
         CALL lbc_lnk( 'traatfqco', pts(:,:,:,jp_tem,Kmm) , 'T', 1._wp, pts(:,:,:,jp_sal,Kmm) , 'T', 1._wp )
         !
      ENDIF
      !
      IF( l_trdtra .AND. ln_linssh ) THEN      ! trend of the Asselin filter (tb filtered - tb)/dt
         DO jk = 1, jpkm1
            ztrdt(:,:,jk) = ( pts(:,:,jk,jp_tem,Kmm) - ztrdt(:,:,jk) ) * r1_Dt
            ztrds(:,:,jk) = ( pts(:,:,jk,jp_sal,Kmm) - ztrds(:,:,jk) ) * r1_Dt
         END DO
         CALL trd_tra( kt, Kmm, Kaa, 'TRA', jp_tem, jptra_atf, ztrdt )
         CALL trd_tra( kt, Kmm, Kaa, 'TRA', jp_sal, jptra_atf, ztrds )
      END IF
      IF( l_trdtra )   DEALLOCATE( ztrdt , ztrds )
      !
      !                        ! control print
      IF(sn_cfctl%l_prtctl)   CALL prt_ctl( tab3d_1=pts(:,:,:,jp_tem,Kmm), clinfo1=' nxt  - Tn: ', mask1=tmask,   &
         &                                  tab3d_2=pts(:,:,:,jp_sal,Kmm), clinfo2=       ' Sn: ', mask2=tmask )
      !
      IF( ln_timing )   CALL timing_stop('tra_atf_qco')
      !
   END SUBROUTINE tra_atf_qco


   SUBROUTINE tra_atf_fix_lf( kt, Kbb, Kmm, Kaa, kit000, cdtype, pt, kjpt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE tra_atf_fix  ***
      !!
      !! ** Purpose :   fixed volume: apply the Asselin time filter to the "now" field
      !!
      !! ** Method  : - Apply a Asselin time filter on now fields.
      !!
      !! ** Action  : - pt(Kmm) ready for the next time step
      !!----------------------------------------------------------------------
      INTEGER                                  , INTENT(in   ) ::  kt            ! ocean time-step index
      INTEGER                                  , INTENT(in   ) ::  Kbb, Kmm, Kaa ! time level indices
      INTEGER                                  , INTENT(in   ) ::  kit000        ! first time step index
      CHARACTER(len=3)                         , INTENT(in   ) ::  cdtype        ! =TRA or TRC (tracer indicator)
      INTEGER                                  , INTENT(in   ) ::  kjpt          ! number of tracers
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt,jpt), INTENT(inout) ::  pt            ! tracer fields
      !
      INTEGER  ::   ji, jj, jk, jn   ! dummy loop indices
      REAL(wp) ::   ztn, ztd         ! local scalars
      !!----------------------------------------------------------------------
      !
      IF( kt == kit000 )  THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'tra_atf_fix_lf : time filtering', cdtype
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~'
      ENDIF
      !
      DO jn = 1, kjpt
         !
         DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 )
            ztn = pt(ji,jj,jk,jn,Kmm)
            ztd = pt(ji,jj,jk,jn,Kaa) - 2._wp * ztn + pt(ji,jj,jk,jn,Kbb)  ! time laplacian on tracers
            !
            pt(ji,jj,jk,jn,Kmm) = ztn + rn_atfp * ztd                      ! pt <-- filtered pt
         END_3D
         !
      END DO
      !
   END SUBROUTINE tra_atf_fix_lf


   SUBROUTINE tra_atf_qco_lf( kt, Kbb, Kmm, Kaa, kit000, p2dt, cdtype, pt, psbc_tc, psbc_tc_b, kjpt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE tra_atf_vvl  ***
      !!
      !! ** Purpose :   Time varying volume: apply the Asselin time filter
      !!
      !! ** Method  : - Apply a thickness weighted Asselin time filter on now fields.
      !!             pt(Kmm)  = ( e3t_m*pt(Kmm) + rn_atfp*[ e3t_b*pt(Kbb) - 2 e3t_m*pt(Kmm) + e3t_a*pt(Kaa) ] )
      !!                       /( e3t_m         + rn_atfp*[ e3t_b         - 2 e3t_m         + e3t_a    ] )
      !!
      !! ** Action  : - pt(Kmm) ready for the next time step
      !!----------------------------------------------------------------------
      INTEGER                                  , INTENT(in   ) ::  kt        ! ocean time-step index
      INTEGER                                  , INTENT(in   ) ::  Kbb, Kmm, Kaa ! time level indices
      INTEGER                                  , INTENT(in   ) ::  kit000    ! first time step index
      REAL(wp)                                 , INTENT(in   ) ::  p2dt      ! time-step
      CHARACTER(len=3)                         , INTENT(in   ) ::  cdtype    ! =TRA or TRC (tracer indicator)
      INTEGER                                  , INTENT(in   ) ::  kjpt      ! number of tracers
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt,jpt), INTENT(inout) ::  pt        ! tracer fields
      REAL(wp), DIMENSION(jpi,jpj    ,kjpt)    , INTENT(in   ) ::  psbc_tc   ! surface tracer content
      REAL(wp), DIMENSION(jpi,jpj    ,kjpt)    , INTENT(in   ) ::  psbc_tc_b ! before surface tracer content
      !
      LOGICAL  ::   ll_traqsr, ll_rnf, ll_isf   ! local logical
      INTEGER  ::   ji, jj, jk, jn              ! dummy loop indices
      REAL(wp) ::   zfact, zfact1, ztc_a , ztc_n , ztc_b , ztc_f , ztc_d    ! local scalar
      REAL(wp) ::   zfact2, ze3t_b, ze3t_n, ze3t_a, ze3t_f                  !   -      -
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:) ::   ztrd_atf
      !!----------------------------------------------------------------------
      !
      IF( kt == kit000 )  THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'tra_atf_qco : time filtering', cdtype
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~'
      ENDIF
      !
      IF( cdtype == 'TRA' )  THEN
         ll_traqsr  = ln_traqsr        ! active  tracers case  and  solar penetration
         ll_rnf     = ln_rnf           ! active  tracers case  and  river runoffs
         ll_isf     = ln_isf           ! active  tracers case  and  ice shelf melting
      ELSE                          ! passive tracers case
         ll_traqsr  = .FALSE.          ! NO solar penetration
         ll_rnf     = .FALSE.          ! NO river runoffs ????          !!gm BUG ?
         ll_isf     = .FALSE.          ! NO ice shelf melting/freezing  !!gm BUG ??
      ENDIF
      !
      IF( ( l_trdtra .AND. cdtype == 'TRA' ) .OR. ( l_trdtrc .AND. cdtype == 'TRC' ) )   THEN
         ALLOCATE( ztrd_atf(jpi,jpj,jpk,kjpt) )
         ztrd_atf(:,:,:,:) = 0.0_wp
      ENDIF
      zfact = 1._wp / p2dt
      zfact1 = rn_atfp * p2dt
      zfact2 = zfact1 * r1_rho0
      DO jn = 1, kjpt
         DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 )
            ze3t_b = e3t(ji,jj,jk,Kbb)
            ze3t_n = e3t(ji,jj,jk,Kmm)
            ze3t_a = e3t(ji,jj,jk,Kaa)
            !                                         ! tracer content at Before, now and after
            ztc_b  = pt(ji,jj,jk,jn,Kbb) * ze3t_b
            ztc_n  = pt(ji,jj,jk,jn,Kmm) * ze3t_n
            ztc_a  = pt(ji,jj,jk,jn,Kaa) * ze3t_a
            !
            ztc_d  = ztc_a  - 2. * ztc_n  + ztc_b
            !
            ztc_f  = ztc_n  + rn_atfp * ztc_d
            !
            ! Asselin correction on scale factors is done via ssh in r3t_f
            ze3t_f = e3t_0(ji,jj,jk) * ( 1._wp + r3t_f(ji,jj) * tmask(ji,jj,jk) )

            !
            IF( jk == mikt(ji,jj) ) THEN           ! first level
               ztc_f  = ztc_f  - zfact1 * ( psbc_tc(ji,jj,jn) - psbc_tc_b(ji,jj,jn) )
            ENDIF
            !
            ! solar penetration (temperature only)
            IF( ll_traqsr .AND. jn == jp_tem .AND. jk <= nksr )                            &
               &     ztc_f  = ztc_f  - zfact1 * ( qsr_hc(ji,jj,jk) - qsr_hc_b(ji,jj,jk) )
               !
            !
            IF( ll_rnf .AND. jk <= nk_rnf(ji,jj) )                                          &
               &     ztc_f  = ztc_f  - zfact1 * ( rnf_tsc(ji,jj,jn) - rnf_tsc_b(ji,jj,jn) ) &
               &                              * e3t(ji,jj,jk,Kmm) / h_rnf(ji,jj)

            !
            ! ice shelf
            IF( ll_isf ) THEN
               !
               ! melt in the cavity
               IF ( ln_isfcav_mlt ) THEN
                  ! level fully include in the Losch_2008 ice shelf boundary layer
                  IF ( jk >= misfkt_cav(ji,jj) .AND. jk < misfkb_cav(ji,jj) ) THEN
                     ztc_f  = ztc_f  - zfact1 * ( risf_cav_tsc(ji,jj,jn) - risf_cav_tsc_b(ji,jj,jn) ) &
                        &                     * e3t(ji,jj,jk,Kmm) / rhisf_tbl_cav(ji,jj)
                  END IF
                  ! level partially include in Losch_2008 ice shelf boundary layer
                  IF ( jk == misfkb_cav(ji,jj) ) THEN
                     ztc_f  = ztc_f  - zfact1 * ( risf_cav_tsc(ji,jj,jn) - risf_cav_tsc_b(ji,jj,jn) )  &
                            &                 * e3t(ji,jj,jk,Kmm) / rhisf_tbl_cav(ji,jj)               &
                            &                 * rfrac_tbl_cav(ji,jj)
                  END IF
               END IF
               !
               ! parametrised melt (cavity closed)
               IF ( ln_isfpar_mlt ) THEN
                  ! level fully include in the Losch_2008 ice shelf boundary layer
                  IF ( jk >= misfkt_par(ji,jj) .AND. jk < misfkb_par(ji,jj) ) THEN
                     ztc_f  = ztc_f  - zfact1 * ( risf_par_tsc(ji,jj,jn) - risf_par_tsc_b(ji,jj,jn) )  &
                            &                 * e3t(ji,jj,jk,Kmm) / rhisf_tbl_par(ji,jj)
                  END IF
                  ! level partially include in Losch_2008 ice shelf boundary layer
                  IF ( jk == misfkb_par(ji,jj) ) THEN
                     ztc_f  = ztc_f  - zfact1 * ( risf_par_tsc(ji,jj,jn) - risf_par_tsc_b(ji,jj,jn) )  &
                            &                 * e3t(ji,jj,jk,Kmm) / rhisf_tbl_par(ji,jj)               &
                            &                 * rfrac_tbl_par(ji,jj)
                  END IF
               END IF
               !
               ! ice sheet coupling correction
               IF ( ln_isfcpl ) THEN
                  !
                  ! at kt = nit000,  risfcpl_vol_n = 0 and risfcpl_vol_b = risfcpl_vol so contribution nul
                  IF ( ln_rstart .AND. kt == nit000+1 ) THEN
                     ztc_f  = ztc_f  + zfact1 * risfcpl_tsc(ji,jj,jk,jn) * r1_e1e2t(ji,jj)
                     ! Shouldn't volume increment be spread according thanks to zscale  ?
                  END IF
                  !
               END IF
               !
            END IF
            !
            ze3t_f = 1.e0 / ze3t_f
            pt(ji,jj,jk,jn,Kmm) = ztc_f * ze3t_f    ! time filtered "now" field
            !
            IF( ( l_trdtra .and. cdtype == 'TRA' ) .OR. ( l_trdtrc .and. cdtype == 'TRC' ) ) THEN
               ztrd_atf(ji,jj,jk,jn) = (ztc_f - ztc_n) * zfact/ze3t_n
            ENDIF
            !
         END_3D
         !
      END DO
      !
      IF( ( l_trdtra .AND. cdtype == 'TRA' ) .OR. ( l_trdtrc .AND. cdtype == 'TRC' ) )   THEN
         IF( l_trdtra .AND. cdtype == 'TRA' ) THEN
            CALL trd_tra( kt, Kmm, Kaa, cdtype, jp_tem, jptra_atf, ztrd_atf(:,:,:,jp_tem) )
            CALL trd_tra( kt, Kmm, Kaa, cdtype, jp_sal, jptra_atf, ztrd_atf(:,:,:,jp_sal) )
         ENDIF
         IF( l_trdtrc .AND. cdtype == 'TRC' ) THEN
            DO jn = 1, kjpt
               CALL trd_tra( kt, Kmm, Kaa, cdtype, jn, jptra_atf, ztrd_atf(:,:,:,jn) )
            END DO
         ENDIF
         DEALLOCATE( ztrd_atf )
      ENDIF
      !
   END SUBROUTINE tra_atf_qco_lf

   !!======================================================================
END MODULE traatf_qco
