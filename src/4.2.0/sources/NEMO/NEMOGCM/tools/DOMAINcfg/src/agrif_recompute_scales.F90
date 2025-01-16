MODULE agrif_recompute_scales

   USE dom_oce
   USE lbclnk            ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp

   IMPLICiT NONE

   PRIVATE

   PUBLIC agrif_recompute_scalefactors

CONTAINS

#if defined key_agrif
   SUBROUTINE agrif_recompute_scalefactors
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE nemo_gcm  ***
      !!----------------------------------------------------------------------
      !
      INTEGER :: ji, jj, jk, ikb, ikt
      REAL(wp), DIMENSION(jpi,jpj) ::   zk   ! workspace


      IF ( ln_sco ) RETURN

      ! Scale factors and depth at U-, V-, UW and VW-points
      DO jk = 1, jpk                        ! initialisation to z-scale factors
         e3u_0 (:,:,jk) = e3t_1d(jk)
         e3v_0 (:,:,jk) = e3t_1d(jk)
         e3uw_0(:,:,jk) = e3w_1d(jk)
         e3vw_0(:,:,jk) = e3w_1d(jk)
      END DO

      DO jk = 1,jpk                         ! Computed as the minimum of neighbooring scale factors
         DO jj = 1, jpjm1
            DO ji = 1, jpim1   ! vector opt.
               e3u_0 (ji,jj,jk) = MIN( e3t_0(ji,jj,jk), e3t_0(ji+1,jj,jk) )
               e3v_0 (ji,jj,jk) = MIN( e3t_0(ji,jj,jk), e3t_0(ji,jj+1,jk) )
               e3uw_0(ji,jj,jk) = MIN( e3w_0(ji,jj,jk), e3w_0(ji+1,jj,jk) )
               e3vw_0(ji,jj,jk) = MIN( e3w_0(ji,jj,jk), e3w_0(ji,jj+1,jk) )
            END DO
         END DO
      END DO
!      IF ( ln_isfcav ) THEN
!      ! (ISF) define e3uw (adapted for 2 cells in the water column)
!      print *,'NOT READY SINCE:'
!      print *,'MBATHY HAS NOT BEEN CORRECTED / UPDATED'
!      print *,'EVEN NOT COMPUTED IN THE CASE ln_read_cfg = .TRUE.'
!      STOP
!         DO jj = 2, jpjm1 
!            DO ji = 2, jpim1   ! vector opt. 
!               ikb = MAX(mbathy (ji,jj),mbathy (ji+1,jj))
!               ikt = MAX(misfdep(ji,jj),misfdep(ji+1,jj))
!               IF (ikb == ikt+1) e3uw_0(ji,jj,ikb) =  MIN( gdept_0(ji,jj,ikb  ), gdept_0(ji+1,jj  ,ikb  ) ) &
!                                       &            - MAX( gdept_0(ji,jj,ikb-1), gdept_0(ji+1,jj  ,ikb-1) )
!               ikb = MAX(mbathy (ji,jj),mbathy (ji,jj+1))
!               ikt = MAX(misfdep(ji,jj),misfdep(ji,jj+1))
!               IF (ikb == ikt+1) e3vw_0(ji,jj,ikb) =  MIN( gdept_0(ji,jj,ikb  ), gdept_0(ji  ,jj+1,ikb  ) ) &
!                                       &            - MAX( gdept_0(ji,jj,ikb-1), gdept_0(ji  ,jj+1,ikb-1) )
!            END DO
!         END DO
!      END IF

      CALL lbc_lnk('toto', e3u_0 , 'U', 1._wp )   ;   CALL lbc_lnk('toto', e3uw_0, 'U', 1._wp )   ! lateral boundary conditions
      CALL lbc_lnk( 'toto',e3v_0 , 'V', 1._wp )   ;   CALL lbc_lnk('toto', e3vw_0, 'V', 1._wp )
      !

      DO jk = 1, jpk                        ! set to z-scale factor if zero (i.e. along closed boundaries)
         WHERE( e3u_0 (:,:,jk) == 0._wp )   e3u_0 (:,:,jk) = e3t_1d(jk)
         WHERE( e3v_0 (:,:,jk) == 0._wp )   e3v_0 (:,:,jk) = e3t_1d(jk)
         WHERE( e3uw_0(:,:,jk) == 0._wp )   e3uw_0(:,:,jk) = e3w_1d(jk)
         WHERE( e3vw_0(:,:,jk) == 0._wp )   e3vw_0(:,:,jk) = e3w_1d(jk)
      END DO
      
      ! Scale factor at F-point
      DO jk = 1, jpk                        ! initialisation to z-scale factors
         e3f_0(:,:,jk) = e3t_1d(jk)
      END DO
      DO jk = 1, jpk                        ! Computed as the minimum of neighbooring V-scale factors
         DO jj = 1, jpjm1
            DO ji = 1, jpim1   ! vector opt.
               e3f_0(ji,jj,jk) = MIN( e3v_0(ji,jj,jk), e3v_0(ji+1,jj,jk) )
            END DO
         END DO
      END DO
      CALL lbc_lnk('toto', e3f_0, 'F', 1._wp )       ! Lateral boundary conditions
      !
      DO jk = 1, jpk                        ! set to z-scale factor if zero (i.e. along closed boundaries)
         WHERE( e3f_0(:,:,jk) == 0._wp )   e3f_0(:,:,jk) = e3t_1d(jk)
      END DO
!!gm  bug ? :  must be a do loop with mj0,mj1
      ! 
      e3t_0(:,mj0(1),:) = e3t_0(:,mj0(2),:)     ! we duplicate factor scales for jj = 1 and jj = 2
      e3w_0(:,mj0(1),:) = e3w_0(:,mj0(2),:) 
      e3u_0(:,mj0(1),:) = e3u_0(:,mj0(2),:) 
      e3v_0(:,mj0(1),:) = e3v_0(:,mj0(2),:) 
      e3f_0(:,mj0(1),:) = e3f_0(:,mj0(2),:) 

      ! Control of the sign
      IF( MINVAL( e3t_0  (:,:,:) ) <= 0._wp )   CALL ctl_stop( '    zgr_zps :   e r r o r   e3t_0 <= 0' )
      IF( MINVAL( e3w_0  (:,:,:) ) <= 0._wp )   CALL ctl_stop( '    zgr_zps :   e r r o r   e3w_0 <= 0' )
      IF( MINVAL( gdept_0(:,:,:) ) <  0._wp )   CALL ctl_stop( '    zgr_zps :   e r r o r   gdept_0 <  0' )
      IF( MINVAL( gdepw_0(:,:,:) ) <  0._wp )   CALL ctl_stop( '    zgr_zps :   e r r o r   gdepw_0 <  0' )
      ! 
      ! since these are read, re-compute mbku, mbkv, mbkf
      DO jj = 1, jpjm1
         DO ji = 1, jpim1
            mbku(ji,jj) = MIN(  mbkt(ji+1,jj  ) , mbkt(ji  ,jj  )  )
            mbkv(ji,jj) = MIN(  mbkt(ji  ,jj+1) , mbkt(ji  ,jj  )  )
            mbkf(ji,jj) = MIN(  mbkt(ji  ,jj+1) , mbkt(ji  ,jj  ), & 
                        &       mbkt(ji+1,jj  ) , mbkt(ji+1,jj+1) )
         END DO
      END DO
      !
      zk(:,:)   = REAL( mbku(:,:), wp )   ;   CALL lbc_lnk( 'domzgr', zk, 'U', 1.)
      mbku(:,:) = MAX( NINT( zk(:,:) ), 1 )
      zk(:,:)   = REAL( mbkv(:,:), wp )   ;   CALL lbc_lnk( 'domzgr', zk, 'V', 1.)
      mbkv(:,:) = MAX( NINT( zk(:,:) ), 1 )
      zk(:,:)   = REAL( mbkf(:,:), wp )   ;   CALL lbc_lnk( 'domzgr', zk, 'F', 1.)
      mbkf(:,:) = MAX( NINT( zk(:,:) ), 1 )
      !
   END SUBROUTINE agrif_recompute_scalefactors
#else
   SUBROUTINE agrif_recompute_scalefactors
   END SUBROUTINE agrif_recompute_scalefactors
#endif
END MODULE agrif_recompute_scales    
