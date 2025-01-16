MODULE trcldf
   !!======================================================================
   !!                       ***  MODULE  trcldf  ***
   !! Ocean Passive tracers : lateral diffusive trends
   !!=====================================================================
   !! History :  1.0  ! 2005-11  (G. Madec)  Original code
   !!            3.0  ! 2008-01  (C. Ethe, G. Madec)  merge TRC-TRA
   !!            3.7  ! 2014-03  (G. Madec)  LDF simplification
   !!----------------------------------------------------------------------
#if defined key_top
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   !!   trc_ldf       : update the tracer trend with the lateral diffusion
   !!   trc_ldf_ini   : initialization, namelist read, and parameters control
   !!----------------------------------------------------------------------
   USE par_trc        ! need jptra, number of passive tracers
   USE trc            ! ocean passive tracers variables
   USE oce_trc        ! ocean dynamics and active tracers
   USE ldftra         ! lateral diffusion: eddy diffusivity & EIV coeff.
   USE ldfslp         ! Lateral diffusion: slopes of neutral surfaces
   USE traldf_lap_blp ! lateral diffusion: lap/bilaplacian iso-level      operator  (tra_ldf_lap/_blp   routine)
   USE traldf_iso     ! lateral diffusion: laplacian iso-neutral standard operator  (tra_ldf_iso        routine)
   USE traldf_triad   ! lateral diffusion: laplacian iso-neutral triad    operator  (tra_ldf_     triad routine)
   USE trd_oce        ! trends: ocean variables
   USE trdtra         ! trends manager: tracers
   !
   USE prtctl         ! Print control

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_ldf    
   PUBLIC   trc_ldf_ini   
   !
   !                                      !!: ** lateral mixing namelist (nam_trcldf) **
   LOGICAL , PUBLIC ::   ln_trcldf_OFF     !: No operator (no explicit lateral diffusion)
   LOGICAL , PUBLIC ::   ln_trcldf_tra     !: use active tracer operator
   REAL(wp), PUBLIC ::      rn_ldf_multi      !: multiplier of T-S eddy diffusivity to obtain the passive tracer one
   REAL(wp), PUBLIC ::      rn_fact_lap       !: enhanced Equatorial zonal diffusivity coefficent
   !
   INTEGER  ::   nldf_trc = 0   ! type of lateral diffusion used defined from ln_traldf_... (namlist logicals)
   REAL(wp) ::   rldf           ! multiplier between active and passive tracers eddy diffusivity   [-]
   
   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcldf.F90 14834 2021-05-11 09:24:44Z hadcv $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_ldf( kt, Kbb, Kmm, ptr, Krhs )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_ldf  ***
      !!
      !! ** Purpose :   compute the lateral ocean tracer physics.
      !!
      !!----------------------------------------------------------------------
      INTEGER,                                    INTENT(in   ) :: kt              ! ocean time-step index
      INTEGER,                                    INTENT(in   ) :: Kbb, Kmm, Krhs  ! ocean time-level index
      REAL(wp), DIMENSION(jpi,jpj,jpk,jptra,jpt), INTENT(inout) :: ptr             ! passive tracers and RHS of tracer equation
      !
      INTEGER            :: ji, jj, jk, jn
      REAL(wp)           :: zdep
      CHARACTER (len=22) :: charout
      REAL(wp),          DIMENSION(jpi,jpj,jpk) ::   zahu, zahv
      REAL(wp), POINTER, DIMENSION(:,:,:,:)     ::   ztrtrd
      !!----------------------------------------------------------------------
      !
      IF( ln_trcldf_OFF )   RETURN        ! not lateral diffusion applied on passive tracers
      !
      IF( ln_timing )   CALL timing_start('trc_ldf')
      !
      IF( l_trdtrc )  THEN
         ALLOCATE( ztrtrd(jpi,jpj,jpk,jptra) )
         ztrtrd(:,:,:,:)  = ptr(:,:,:,:,Krhs)
      ENDIF
      !                                  !* set the lateral diffusivity coef. for passive tracer      
      zahu(:,:,:) = rldf * ahtu(:,:,:) 
      zahv(:,:,:) = rldf * ahtv(:,:,:)
      !                                  !* Enhanced zonal diffusivity coefficent in the equatorial domain
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpk )
         IF( gdept(ji,jj,jk,Kmm) > 200. .AND. gphit(ji,jj) < 5. .AND. gphit(ji,jj) > -5. ) THEN
            zdep = MAX( gdept(ji,jj,jk,Kmm) - 1000., 0. ) / 1000.
            zahu(ji,jj,jk) = zahu(ji,jj,jk) * MAX( 1., rn_fact_lap * EXP( -zdep ) )
         ENDIF
      END_3D
      !
      SELECT CASE ( nldf_trc )                 !* compute lateral mixing trend and add it to the general trend
      !
      CASE ( np_lap   )                                                                                    ! iso-level laplacian
         CALL tra_ldf_lap  ( kt, Kmm, nittrc000,'TRC', zahu, zahv, gtru, gtrv, gtrui, gtrvi,            &
           &                     ptr(:,:,:,:,Kbb), ptr(:,:,:,:,Krhs),                   jptra, 1 )
      CASE ( np_lap_i )                                                                                    ! laplacian : standard iso-neutral operator (Madec)
         CALL tra_ldf_iso  ( kt, Kmm, nittrc000,'TRC', zahu, zahv, gtru, gtrv, gtrui, gtrvi,            &
           &                     ptr(:,:,:,:,Kbb), ptr(:,:,:,:,Kbb), ptr(:,:,:,:,Krhs), jptra, 1 )
      CASE ( np_lap_it )                                                                                   ! laplacian : triad iso-neutral operator (griffies)
         CALL tra_ldf_triad( kt, Kmm, nittrc000,'TRC', zahu, zahv, gtru, gtrv, gtrui, gtrvi,            &
           &                     ptr(:,:,:,:,Kbb), ptr(:,:,:,:,Kbb), ptr(:,:,:,:,Krhs), jptra, 1 )
      CASE ( np_blp , np_blp_i , np_blp_it )                                                               ! bilaplacian: all operator (iso-level, -neutral)
         CALL tra_ldf_blp  ( kt, Kmm, nittrc000,'TRC', zahu, zahv, gtru, gtrv, gtrui, gtrvi,            &
           &                     ptr(:,:,:,:,Kbb) , ptr(:,:,:,:,Krhs),                 jptra, nldf_trc )
      END SELECT
      !
      IF( l_trdtrc )   THEN                    ! send the trends for further diagnostics
        DO jn = 1, jptra
           ztrtrd(:,:,:,jn) = ptr(:,:,:,jn,Krhs) - ztrtrd(:,:,:,jn)
           CALL trd_tra( kt, Kmm, Krhs, 'TRC', jn, jptra_ldf, ztrtrd(:,:,:,jn) )
        END DO
        DEALLOCATE( ztrtrd )
      ENDIF
      !                
      IF( sn_cfctl%l_prttrc ) THEN ! print mean trends (used for debugging)
         WRITE(charout, FMT="('ldf ')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl( tab4d_1=ptr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm, clinfo3='trd' )
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('trc_ldf')
      !
   END SUBROUTINE trc_ldf


   SUBROUTINE trc_ldf_ini
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE ldf_ctl  ***
      !!
      !! ** Purpose :   Define the operator for the lateral diffusion
      !!
      !! ** Method  : - ln_trcldf_tra=T : use nldf_tra set in ldftra module 
      !!              to defined the passive tracer lateral diffusive operator
      !!              - ln_trcldf_OFF=T : no explicit diffusion used
      !!----------------------------------------------------------------------
      INTEGER ::   ios, ioptio   ! local integers
      !!
      NAMELIST/namtrc_ldf/ ln_trcldf_OFF , ln_trcldf_tra,   &   ! operator & direction
         &                 rn_ldf_multi  , rn_fact_lap          ! coefficient
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'trc_ldf_ini : lateral passive tracer diffusive operator'
         WRITE(numout,*) '~~~~~~~~~~~'
      ENDIF
      !
      READ  ( numnat_ref, namtrc_ldf, IOSTAT = ios, ERR = 903)
903   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namtrc_ldf in reference namelist' )
      !
      READ  ( numnat_cfg, namtrc_ldf, IOSTAT = ios, ERR = 904 )
904   IF( ios >  0 )   CALL ctl_nam ( ios , 'namtrc_ldf in configuration namelist' )
      IF(lwm) WRITE ( numont, namtrc_ldf )
      !
      IF(lwp) THEN                     ! Namelist print
         WRITE(numout,*) '   Namelist namtrc_ldf : set lateral mixing parameters (type, direction, coefficients)'
         WRITE(numout,*) '      no explicit diffusion                 ln_trcldf_OFF   = ', ln_trcldf_OFF
         WRITE(numout,*) '      use active tracer operator            ln_trcldf_tra   = ', ln_trcldf_tra
         WRITE(numout,*) '      diffusivity coefficient :'
         WRITE(numout,*) '         multiplier of TRA coef. for TRC       rn_ldf_multi = ', rn_ldf_multi
         WRITE(numout,*) '         enhanced zonal Eq. laplacian coef.    rn_fact_lap  = ', rn_fact_lap

      ENDIF
      !      
      !                                ! control the namelist parameters
      nldf_trc = np_ERROR
      ioptio   = 0
      IF( ln_trcldf_OFF  ) THEN   ;   nldf_trc = np_no_ldf   ;   ioptio = ioptio + 1   ;   ENDIF
      IF( ln_trcldf_tra  ) THEN   ;   nldf_trc = nldf_tra    ;   ioptio = ioptio + 1   ;   ENDIF
      IF( ioptio /=  1   )   CALL ctl_stop( 'trc_ldf_ini: use ONE of the 2 operator options (OFF/tra)' )
      
      !                                ! multiplier : passive/active tracers ration
      IF( ln_traldf_lap ) THEN               ! laplacian operator
         rldf = rn_ldf_multi                       ! simple multiplier
      ELSEIF( ln_traldf_blp ) THEN           ! bilaplacian operator: 
         rldf = SQRT( ABS( rn_ldf_multi )  )       ! the coef. used is the SQRT of the bilaplacian coef.
      ENDIF
      !
      IF(lwp) THEN
         WRITE(numout,*)
         SELECT CASE( nldf_trc )
         CASE( np_no_ldf )   ;   WRITE(numout,*) '      ===>>   NO lateral diffusion'
         CASE( np_lap    )   ;   WRITE(numout,*) '      ===>>   laplacian iso-level operator'
         CASE( np_lap_i  )   ;   WRITE(numout,*) '      ===>>   Rotated laplacian operator (standard)'
         CASE( np_lap_it )   ;   WRITE(numout,*) '      ===>>   Rotated laplacian operator (triad)'
         CASE( np_blp    )   ;   WRITE(numout,*) '      ===>>   bilaplacian iso-level operator'
         CASE( np_blp_i  )   ;   WRITE(numout,*) '      ===>>   Rotated bilaplacian operator (standard)'
         CASE( np_blp_it )   ;   WRITE(numout,*) '      ===>>   Rotated bilaplacian operator (triad)'
         END SELECT
      ENDIF
      !
   END SUBROUTINE trc_ldf_ini

#endif
   !!======================================================================
END MODULE trcldf
