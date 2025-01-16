MODULE trcrad
   !!======================================================================
   !!                       ***  MODULE  trcrad  ***
   !! Ocean passive tracers:  correction of negative concentrations
   !!======================================================================
   !! History :   -   !  01-01  (O. Aumont & E. Kestenare)  Original code
   !!            1.0  !  04-03  (C. Ethe)  free form F90
   !!            4.1  !  08-19  (A. Coward, D. Storkey) tidy up using new time-level indices
   !!----------------------------------------------------------------------
#if defined key_top
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   !!   trc_rad    : correction of negative concentrations
   !!----------------------------------------------------------------------
   USE par_trc             ! need jptra, number of passive tracers 
   USE oce_trc             ! ocean dynamics and tracers variables
   USE trc                 ! ocean passive tracers variables
   USE trd_oce
   USE trdtra
   USE prtctl              ! Print control for debbuging
   USE lib_fortran

   IMPLICIT NONE
   PRIVATE

   PUBLIC trc_rad     
   PUBLIC trc_rad_ini  

   LOGICAL , PUBLIC ::   ln_trcrad           !: flag to artificially correct negative concentrations
   REAL(wp), DIMENSION(:,:), ALLOCATABLE::   gainmass

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcrad.F90 13324 2020-07-17 19:47:48Z acc $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_rad( kt, Kbb, Kmm, ptr )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trc_rad  ***
      !!
      !! ** Purpose :   "crappy" routine to correct artificial negative
      !!              concentrations due to isopycnal scheme
      !!
      !! ** Method  : - PISCES or LOBSTER: Set negative concentrations to zero
      !!                while computing the corresponding tracer content that
      !!                is added to the tracers. Then, adjust the tracer 
      !!                concentration using a multiplicative factor so that 
      !!                the total tracer concentration is preserved.
      !!              - CFC: simply set to zero the negative CFC concentration
      !!                (the total CFC content is not strictly preserved)
      !!----------------------------------------------------------------------
      INTEGER,                                    INTENT(in   ) :: kt         ! ocean time-step index
      INTEGER,                                    INTENT(in   ) :: Kbb, Kmm   ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jptra,jpt), INTENT(inout) :: ptr        ! passive tracers and RHS of tracer equation
      !
      CHARACTER (len=22) :: charout
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_rad')
      !
      IF( ln_age     )   CALL trc_rad_sms( kt, Kbb, Kmm, ptr, jp_age , jp_age                )  !  AGE
      IF( ll_cfc     )   CALL trc_rad_sms( kt, Kbb, Kmm, ptr, jp_cfc0, jp_cfc1               )  !  CFC model
      IF( ln_c14     )   CALL trc_rad_sms( kt, Kbb, Kmm, ptr, jp_c14 , jp_c14                )  !  C14
      IF( ln_pisces  )   CALL trc_rad_sms( kt, Kbb, Kmm, ptr, jp_pcs0, jp_pcs1, cpreserv='Y' )  !  PISCES model
      IF( ln_my_trc  )   CALL trc_rad_sms( kt, Kbb, Kmm, ptr, jp_myt0, jp_myt1               )  !  MY_TRC model
      !
      IF(sn_cfctl%l_prttrc) THEN      ! print mean trends (used for debugging)
         WRITE(charout, FMT="('rad')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl( tab4d_1=ptr(:,:,:,:,Kbb), mask1=tmask, clinfo=ctrcnm )
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('trc_rad')
      !
   END SUBROUTINE trc_rad


   SUBROUTINE trc_rad_ini
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE trc _rad_ini ***
      !!
      !! ** Purpose :   read  namelist options 
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer output status for namelist read
      !!
      NAMELIST/namtrc_rad/ ln_trcrad
      !!----------------------------------------------------------------------
      !
      READ  ( numnat_ref, namtrc_rad, IOSTAT = ios, ERR = 907)
907   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namtrc_rad in reference namelist' )
      READ  ( numnat_cfg, namtrc_rad, IOSTAT = ios, ERR = 908 )
908   IF( ios > 0 )   CALL ctl_nam ( ios , 'namtrc_rad in configuration namelist' )
      IF(lwm) WRITE( numont, namtrc_rad )

      IF(lwp) THEN                     !   ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'trc_rad : Correct artificial negative concentrations '
         WRITE(numout,*) '~~~~~~~ '
         WRITE(numout,*) '   Namelist namtrc_rad : treatment of negative concentrations'
         WRITE(numout,*) '      correct artificially negative concen. or not   ln_trcrad = ', ln_trcrad
         WRITE(numout,*)
         IF( ln_trcrad ) THEN   ;   WRITE(numout,*) '      ===>>   ensure the global tracer conservation'
         ELSE                   ;   WRITE(numout,*) '      ===>>   NO strict global tracer conservation'      
         ENDIF
      ENDIF
      !
      ALLOCATE( gainmass(jptra,2) )
      gainmass(:,:) = 0.
      !
   END SUBROUTINE trc_rad_ini


   SUBROUTINE trc_rad_sms( kt, Kbb, Kmm, ptr, jp_sms0, jp_sms1, cpreserv )
     !!-----------------------------------------------------------------------------
     !!                  ***  ROUTINE trc_rad_sms  ***
     !!
     !! ** Purpose :   "crappy" routine to correct artificial negative
     !!              concentrations due to isopycnal scheme
     !!
     !! ** Method  : 2 cases :
     !!                - Set negative concentrations to zero while computing
     !!                  the corresponding tracer content that is added to the
     !!                  tracers. Then, adjust the tracer concentration using
     !!                  a multiplicative factor so that the total tracer 
     !!                  concentration is preserved.
     !!                - simply set to zero the negative CFC concentration
     !!                  (the total content of concentration is not strictly preserved)
     !!--------------------------------------------------------------------------------
     INTEGER                                    , INTENT(in   ) ::   kt                 ! ocean time-step index
     INTEGER                                    , INTENT(in   ) ::   Kbb, Kmm           ! time level indices
     INTEGER                                    , INTENT(in   ) ::   jp_sms0, jp_sms1   ! First & last index of the passive tracer model
     REAL(wp), DIMENSION (jpi,jpj,jpk,jptra,jpt), INTENT(inout) ::   ptr                ! before and now traceur concentration
     CHARACTER( len = 1), OPTIONAL              , INTENT(in   ) ::   cpreserv           ! flag to preserve content or not
     !
     INTEGER ::   ji, ji2, jj, jj2, jk, jn, jt ! dummy loop indices
     INTEGER ::   icnt, itime
     LOGICAL ::   lldebug = .FALSE.            ! local logical
     REAL(wp)::   zcoef, zs2rdt, ztotmass
     REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   ztrneg, ztrpos
     REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   ztrtrd   ! workspace arrays
     !!----------------------------------------------------------------------
     !
     IF( l_trdtrc )   ALLOCATE( ztrtrd(jpi,jpj,jpk) )
     zs2rdt = 1. / ( 2. * rn_Dt )
     !
     DO jt = 1,2  ! Loop over time indices since exactly the same fix is applied to "now" and "after" fields
        IF( jt == 1 ) itime = Kbb
        IF( jt == 2 ) itime = Kmm

        IF( PRESENT( cpreserv )  ) THEN     !==  total tracer concentration is preserved  ==!
           !
           ALLOCATE( ztrneg(1:jpi,1:jpj,jp_sms0:jp_sms1), ztrpos(1:jpi,1:jpj,jp_sms0:jp_sms1) )

           DO jn = jp_sms0, jp_sms1
              ztrneg(:,:,jn) = SUM( MIN( 0., ptr(:,:,:,jn,itime) ) * cvol(:,:,:), dim = 3 )   ! sum of the negative values
              ztrpos(:,:,jn) = SUM( MAX( 0., ptr(:,:,:,jn,itime) ) * cvol(:,:,:), dim = 3 )   ! sum of the positive values
           END DO
           CALL sum3x3( ztrneg )
           CALL sum3x3( ztrpos )

           DO jn = jp_sms0, jp_sms1
              !
              IF( l_trdtrc )   ztrtrd(:,:,:) = ptr(:,:,:,jn,itime)                       ! save input tr(:,:,:,:,Kbb) for trend computation           
              !
              DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 )
                 IF( ztrneg(ji,jj,jn) /= 0. ) THEN                                 ! if negative values over the 3x3 box
                    !
                    ptr(ji,jj,jk,jn,itime) = ptr(ji,jj,jk,jn,itime) * tmask(ji,jj,jk)   ! really needed?
                    IF( ptr(ji,jj,jk,jn,itime) < 0. ) ptr(ji,jj,jk,jn,itime) = 0.       ! suppress negative values
                    IF( ptr(ji,jj,jk,jn,itime) > 0. ) THEN                    ! use positive values to compensate mass gain
                       zcoef = 1. + ztrneg(ji,jj,jn) / ztrpos(ji,jj,jn)       ! ztrpos > 0 as ptr > 0
                       ptr(ji,jj,jk,jn,itime) = ptr(ji,jj,jk,jn,itime) * zcoef
                       IF( zcoef < 0. ) THEN                                  ! if the compensation exceed the positive value
                          gainmass(jn,1) = gainmass(jn,1) - ptr(ji,jj,jk,jn,itime) * cvol(ji,jj,jk)   ! we are adding mass...
                          ptr(ji,jj,jk,jn,itime) = 0.                         ! limit the compensation to keep positive value
                       ENDIF
                    ENDIF
                    !
                 ENDIF
              END_3D
              !
              IF( l_trdtrc ) THEN
                 ztrtrd(:,:,:) = ( ptr(:,:,:,jn,itime) - ztrtrd(:,:,:) ) * zs2rdt
                 CALL trd_tra( kt, Kbb, Kmm, 'TRC', jn, jptra_radb, ztrtrd )       ! Asselin-like trend handling
              ENDIF
              !
           END DO

           IF( kt == nitend ) THEN
              CALL mpp_sum( 'trcrad', gainmass(:,1) )
              DO jn = jp_sms0, jp_sms1
                 IF( gainmass(jn,1) > 0. ) THEN
                    ztotmass = glob_sum( 'trcrad', ptr(:,:,:,jn,itime) * cvol(:,:,:) )
                    IF(lwp) WRITE(numout, '(a, i2, a, D23.16, a, D23.16)') 'trcrad ptrb, traceur ', jn  &
                         &        , ' total mass : ', ztotmass, ', mass gain : ',  gainmass(jn,1)
                 END IF
              END DO
           ENDIF

           DEALLOCATE( ztrneg, ztrpos )
           !
        ELSE                                !==  total CFC content is NOT strictly preserved  ==!
           !
           DO jn = jp_sms0, jp_sms1  
              !
              IF( l_trdtrc )   ztrtrd(:,:,:) = ptr(:,:,:,jn,itime)                 ! save input tr for trend computation
              !
              WHERE( ptr(:,:,:,jn,itime) < 0. )   ptr(:,:,:,jn,itime) = 0.
              !
              IF( l_trdtrc ) THEN
                 ztrtrd(:,:,:) = ( ptr(:,:,:,jn,itime) - ztrtrd(:,:,:) ) * zs2rdt
                 CALL trd_tra( kt, Kbb, Kmm, 'TRC', jn, jptra_radb, ztrtrd )       ! Asselin-like trend handling
              ENDIF
              !
           END DO
           !
        ENDIF
        !
      END DO
      !
      IF( l_trdtrc )  DEALLOCATE( ztrtrd )
      !
   END SUBROUTINE trc_rad_sms

#else
   !!----------------------------------------------------------------------
   !!   Dummy module :                                         NO TOP model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_rad( kt, Kbb, Kmm )              ! Empty routine
      INTEGER, INTENT(in) ::   kt
      INTEGER, INTENT(in) ::   Kbb, Kmm  ! time level indices
      WRITE(*,*) 'trc_rad: You should not have seen this print! error?', kt
   END SUBROUTINE trc_rad
#endif
   
   !!======================================================================
END MODULE trcrad
