MODULE trctrp
   !!======================================================================
   !!                       ***  MODULE trctrp  ***
   !! Ocean Physics    : manage the passive tracer transport
   !!======================================================================
   !! History :   1.0  !  2004-03 (C. Ethe) Original code
   !!             3.3  !  2010-07 (C. Ethe) Merge TRA-TRC
   !!----------------------------------------------------------------------
#if defined key_top
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   !!   trc_trp        : passive tracer transport
   !!----------------------------------------------------------------------
   USE par_trc         ! need jptra, number of passive tracers
   USE oce_trc         ! ocean dynamics and active tracers variables
   USE trc             ! ocean passive tracers variables 
   USE trcbbl          ! bottom boundary layer               (trc_bbl routine)
   USE trcdmp          ! internal damping                    (trc_dmp routine)
   USE trcldf          ! lateral mixing                      (trc_ldf routine)
   USE trcadv          ! advection                           (trc_adv routine)
   USE trczdf          ! vertical diffusion                  (trc_zdf routine)
   USE trcatf          ! time filtering                      (trc_atf routine)
   USE trcrad          ! positivity                          (trc_rad routine)
   USE trcsbc          ! surface boundary condition          (trc_sbc routine)
   USE trcbc           ! Tracers boundary condtions          ( trc_bc routine)
   USE trcais          ! Antarctic Ice Sheet tracers         (trc_ais routine)
   USE zpshde          ! partial step: hor. derivative       (zps_hde routine)
   USE bdy_oce   , ONLY: ln_bdy
   USE trcbdy          ! BDY open boundaries
   USE in_out_manager

#if defined key_agrif
   USE agrif_top_sponge ! tracers sponges
#endif

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_trp    ! called by trc_stp

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trctrp.F90 15023 2021-06-18 14:35:25Z gsamson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE trc_trp( kt, Kbb, Kmm, Krhs, Kaa )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE trc_trp  ***
      !!                      
      !! ** Purpose :   Management of passive tracers transport
      !! 
      !! ** Method  : - Compute the passive tracers trends 
      !!              - Update the passive tracers
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) :: kt                  ! ocean time-step index
      INTEGER, INTENT( in ) :: Kbb, Kmm, Krhs, Kaa ! time level indices (not swapped in this routine)
      !! ---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_trp')
      !
      IF( .NOT. ln_c1d ) THEN
         !
         !                                                         ! Partial top/bottom cell: GRADh( trb )  
         IF( ln_zps ) THEN
            IF( ln_isfcav ) THEN ; CALL zps_hde_isf( kt, Kmm, jptra, tr(:,:,:,:,Kbb), pgtu=gtru, pgtv=gtrv, pgtui=gtrui, pgtvi=gtrvi )  ! both top & bottom
            ELSE                 ; CALL zps_hde    ( kt, Kmm, jptra, tr(:,:,:,:,Kbb), gtru, gtrv )                                      !  only bottom
            ENDIF
         ENDIF
         !
                                CALL trc_sbc    ( kt,      Kmm, tr, Krhs )      ! surface boundary condition
         IF( ln_trcbc .AND. lltrcbc .AND. kt /= nit000 )  &
                                CALL trc_bc     ( kt,      Kmm, tr, Krhs )      ! tracers: surface and lateral Boundary Conditions 
         IF( ln_trcais )        CALL trc_ais    ( kt,      Kmm, tr, Krhs )      ! tracers from Antarctic Ice Sheet (icb, isf)               
         IF( ln_trabbl )        CALL trc_bbl    ( kt, Kbb, Kmm, tr, Krhs )      ! advective (and/or diffusive) bottom boundary layer scheme
         IF( ln_trcdmp )        CALL trc_dmp    ( kt, Kbb, Kmm, tr, Krhs )      ! internal damping trends
         IF( ln_bdy )           CALL trc_bdy_dmp( kt, Kbb,      Krhs )      ! BDY damping trends
#if defined key_agrif
         IF(.NOT. Agrif_Root()) CALL Agrif_Sponge_trc       ! tracers sponge
#endif
                                CALL trc_adv    ( kt, Kbb, Kmm, tr, Krhs )      ! horizontal & vertical advection 
                                CALL trc_ldf    ( kt, Kbb, Kmm,       tr, Krhs )  ! lateral mixing
                                CALL trc_zdf    ( kt, Kbb, Kmm, Krhs, tr, Kaa  )  ! vert. mixing & after tracer	==> after
                                CALL trc_atf    ( kt, Kbb, Kmm, Kaa , tr )        ! time filtering of "now" tracer fields    
         !
         ! Subsequent calls use the filtered values: Kmm and Kaa 
         ! These are used explicitly here since time levels will not be swapped until after tra_atf/dyn_atf/ssh_atf in stp
         !
         IF( ln_trcrad )        CALL trc_rad    ( kt, Kmm, Kaa, tr       )    ! Correct artificial negative concentrations
         IF( ln_trcdmp_clo )    CALL trc_dmp_clo( kt, Kmm, Kaa )              ! internal damping trends on closed seas only

         !
      ELSE                                               ! 1D vertical configuration
                                CALL trc_sbc( kt,      Kmm,       tr, Krhs )  ! surface boundary condition
         IF( ln_trcdmp )        CALL trc_dmp( kt, Kbb, Kmm,       tr, Krhs )  ! internal damping trends
                                CALL trc_zdf( kt, Kbb, Kmm, Krhs, tr, Kaa  )  ! vert. mixing & after tracer	==> after
                                CALL trc_atf( kt, Kbb, Kmm, Kaa , tr )        ! time filtering of "now" tracer fields
         !
         ! Subsequent calls use the filtered values: Kmm and Kaa 
         ! These are used explicitly here since time levels will not be swapped until after tra_atf/dyn_atf/ssh_atf in stp
         !
         IF( ln_trcrad )       CALL trc_rad( kt, Kmm, Kaa, tr       )  ! Correct artificial negative concentrations
         !
      END IF
      !
      IF( ln_timing )   CALL timing_stop('trc_trp')
      !
   END SUBROUTINE trc_trp

#else
   !!----------------------------------------------------------------------
   !!   Dummy module :                                        No TOP models
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_trp( kt )              ! Empty routine
      INTEGER, INTENT(in) ::   kt
      WRITE(*,*) 'trc_trp: You should not have seen this print! error?', kt
   END SUBROUTINE trc_trp
#endif
   
   !!======================================================================
END MODULE trctrp
