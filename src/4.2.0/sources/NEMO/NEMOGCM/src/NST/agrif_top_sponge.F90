#define SPONGE_TOP

MODULE agrif_top_sponge
   !!======================================================================
   !!                ***  MODULE agrif_top_sponge  ***
   !! AGRIF :   sponge layer pakage for passive tracers (TOP)
   !!======================================================================
   !! History :  2.0  ! 2006-08  (R. Benshila, L. Debreu)  Original code
   !!----------------------------------------------------------------------
#if defined key_agrif && defined key_top
   !!----------------------------------------------------------------------
   !!   Agrif_Sponge_trc : 
   !!   interptrn_sponge :  
   !!----------------------------------------------------------------------
   USE par_oce
   USE par_trc
   USE oce
   USE trc
   USE dom_oce
   USE agrif_oce
   USE agrif_oce_sponge
   USE vremap
   !
   USE in_out_manager
   USE lib_mpp

   IMPLICIT NONE
   PRIVATE

   PUBLIC Agrif_Sponge_trc, interptrn_sponge

   !! * Substitutions
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/NST 4.0 , NEMO Consortium (2018)
   !! $Id: agrif_top_sponge.F90 15437 2021-10-22 12:21:20Z jchanut $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE Agrif_Sponge_trc
      !!----------------------------------------------------------------------
      !!                   *** ROUTINE Agrif_Sponge_Trc ***
      !!----------------------------------------------------------------------
      REAL(wp) ::   zcoef   ! local scalar
      !!----------------------------------------------------------------------
      !
#if defined SPONGE_TOP
      !! Assume persistence:
      zcoef = REAL(Agrif_rhot()-1,wp)/REAL(Agrif_rhot())

      Agrif_SpecialValue    = 0._wp
      Agrif_UseSpecialValue = l_spc_top
      l_vremap              = ln_vert_remap
      tabspongedone_trn     = .FALSE.
      !
      CALL Agrif_Bc_Variable( trn_sponge_id, calledweight=zcoef, procname=interptrn_sponge )
      !
      Agrif_UseSpecialValue = .FALSE.
      l_vremap              = .FALSE.
#endif
      !
   END SUBROUTINE Agrif_Sponge_Trc


   SUBROUTINE interptrn_sponge( tabres, i1, i2, j1, j2, k1, k2, n1, n2, before) 
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE interptrn_sponge ***
      !!----------------------------------------------------------------------
      INTEGER                                     , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2, n1, n2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2), INTENT(inout) ::   tabres
      LOGICAL                                     , INTENT(in   ) ::   before
      !
      INTEGER  ::   ji, jj, jk, jn   ! dummy loop indices
      INTEGER  ::   iku, ikv
      REAL(wp) :: ztra, zabe1, zabe2, zbtr, zhtot
      REAL(wp), DIMENSION(i1-1:i2,j1-1:j2,jpk) :: ztu, ztv
      REAL(wp), DIMENSION(i1:i2,j1:j2,jpk,n1:n2) ::trbdiff
      ! vertical interpolation:
      REAL(wp), DIMENSION(i1:i2,j1:j2,jpk,n1:n2) ::tabres_child
      REAL(wp), DIMENSION(k1:k2,n1:n2-1) :: tabin, tabin_i
      REAL(wp), DIMENSION(k1:k2) :: z_in, z_in_i, h_in_i
      REAL(wp), DIMENSION(1:jpk) :: h_out, z_out
      INTEGER :: N_in, N_out
      !!----------------------------------------------------------------------
      !
      IF( before ) THEN
         DO jn = 1, jptra
            DO jk=k1,k2-1
               DO jj=j1,j2
                  DO ji=i1,i2
                     tabres(ji,jj,jk,jn) = tr(ji,jj,jk,jn,Kbb_a) * tmask(ji,jj,jk) 
                  END DO
               END DO
            END DO
         END DO

         IF ( l_vremap.OR.ln_zps ) THEN

            ! Fill cell depths (i.e. gdept) to be interpolated
            ! Warning: these are masked, hence extrapolated prior interpolation.
            DO jj=j1,j2
               DO ji=i1,i2
                  tabres(ji,jj,k1,jptra+1) = 0.5_wp * tmask(ji,jj,k1) * e3w(ji,jj,k1,Kbb_a)
                  DO jk=k1+1,k2-1
                     tabres(ji,jj,jk,jptra+1) = tmask(ji,jj,jk) * &
                        & ( tabres(ji,jj,jk-1,jptra+1) + e3w(ji,jj,jk,Kbb_a) )
                  END DO
               END DO
            END DO

            ! Save ssh at last level:
            IF ( .NOT.ln_linssh ) THEN
               tabres(i1:i2,j1:j2,k2,jptra+1) = ssh(i1:i2,j1:j2,Kbb_a)*tmask(i1:i2,j1:j2,1) 
            END IF  
    
         END IF

      ELSE 
         !
         IF ( l_vremap ) THEN
            
            IF (ln_linssh) THEN
               tabres(i1:i2,j1:j2,k2,n2) = 0._wp

            ELSE ! Assuming parent volume follows child:
               tabres(i1:i2,j1:j2,k2,n2) = ssh(i1:i2,j1:j2,Kbb_a)
            ENDIF

            DO jj=j1,j2
               DO ji=i1,i2

                  tabres_child(ji,jj,:,:) = 0._wp 
                  ! Build vertical grids:
                  N_in = mbkt_parent(ji,jj)
                  ! Input grid (account for partial cells if any):
                  IF ( N_in > 0 ) THEN 
                     DO jk=1,N_in
                        z_in(jk) = tabres(ji,jj,jk,n2) - tabres(ji,jj,k2,n2)
                        tabin(jk,1:jptra) = tabres(ji,jj,jk,1:jptra)
                     END DO
                  
                     ! Intermediate grid:
                     DO jk = 1, N_in
                        h_in_i(jk) = e3t0_parent(ji,jj,jk) * & 
                          &       (1._wp + tabres(ji,jj,k2,n2)/(ht0_parent(ji,jj)*ssmask(ji,jj) + 1._wp - ssmask(ji,jj)))
                     END DO
                     z_in_i(1) = 0.5_wp * h_in_i(1)
                     DO jk=2,N_in
                        z_in_i(jk) = z_in_i(jk-1) + 0.5_wp * ( h_in_i(jk) + h_in_i(jk-1) )
                     END DO
                     z_in_i(1:N_in) = z_in_i(1:N_in)  - tabres(ji,jj,k2,n2)
                  END IF
                  ! Output (Child) grid:
                  N_out = mbkt(ji,jj)
                  DO jk=1,N_out
                     h_out(jk) = e3t(ji,jj,jk,Kbb_a)
                  END DO
                  z_out(1) = 0.5_wp * e3w(ji,jj,1,Kbb_a) 
                  DO jk=2,N_out
                     z_out(jk) = z_out(jk-1) + e3w(ji,jj,jk,Kbb_a) 
                  END DO
                  IF (.NOT.ln_linssh) z_out(1:N_out) = z_out(1:N_out)  - ssh(ji,jj,Kbb_a)

                  ! Account for small differences in the free-surface
                  IF ( sum(h_out(1:N_out)) > sum(h_in_i(1:N_in) )) THEN
                     h_out(1) = h_out(1)  - ( sum(h_out(1:N_out))-sum(h_in_i(1:N_in)) )
                  ELSE
                     h_in_i(1)= h_in_i(1) - ( sum(h_in_i(1:N_in))-sum(h_out(1:N_out)) )
                  END IF
                  IF (N_in*N_out > 0) THEN
! jc: disable "two steps" vertical remapping
!     since this would require e3w0_parent to be available
!                     CALL remap_linear(tabin(1:N_in,1:jptra),z_in(1:N_in),tabin_i(1:N_in,1:jptra),z_in_i(1:N_in),N_in,N_in,jptra)
                     CALL reconstructandremap(tabin(1:N_in,1:jptra),h_in_i(1:N_in),tabres_child(ji,jj,1:N_out,1:jptra),h_out(1:N_out),N_in,N_out,jptra)
!                     CALL remap_linear(tabin(1:N_in,1:jptra),z_in(1:N_in),tabres_child(ji,jj,1:N_out,1:jptra),z_out(1:N_in),N_in,N_out,jptra)  
                  ENDIF
               END DO
            END DO

            DO jj=j1,j2
               DO ji=i1,i2
                  DO jk=1,jpkm1
                     trbdiff(ji,jj,jk,1:jptra) = (tr(ji,jj,jk,1:jptra,Kbb_a) - tabres_child(ji,jj,jk,1:jptra)) * tmask(ji,jj,jk)
                  END DO
               END DO
            END DO

         ELSE

            IF ( Agrif_Parent(ln_zps) ) THEN ! Account for partial cells

               DO jj=j1,j2
                  DO ji=i1,i2
                     !
                     N_in  = mbkt(ji,jj) 
                     N_out = mbkt(ji,jj) 
                     z_in(1) = tabres(ji,jj,1,n2)
                     tabin(1,1:jptra) = tabres(ji,jj,1,1:jptra)
                     DO jk=2, N_in
                        z_in(jk) = tabres(ji,jj,jk,n2)
                        tabin(jk,1:jptra) = tabres(ji,jj,jk,1:jptra)
                     END DO 
                     IF (.NOT.ln_linssh) z_in(1:N_in) = z_in(1:N_in) - tabres(ji,jj,k2,n2)

                     z_out(1) = 0.5_wp * e3w(ji,jj,1,Kbb_a)
                     DO jk=2, N_out
                        z_out(jk) = z_out(jk-1) + e3w(ji,jj,jk,Kbb_a) 
                     END DO 
                     IF (.NOT.ln_linssh) z_out(1:N_out) = z_out(1:N_out) - ssh(ji,jj,Kbb_a)

                     CALL remap_linear(tabin(1:N_in,1:jptra), z_in(1:N_in), tabres(ji,jj,1:N_out,1:jptra), &
                                         &   z_out(1:N_out), N_in, N_out, jptra)
                  END DO
               END DO
            ENDIF

            DO jj=j1,j2
               DO ji=i1,i2
                  DO jk=1,jpkm1
                     trbdiff(ji,jj,jk,1:jptra) = (tr(ji,jj,jk,1:jptra,Kbb_a) - tabres(ji,jj,jk,1:jptra))*tmask(ji,jj,jk)
                  END DO
               END DO
            END DO

         END IF

         DO jn = 1, jptra            
            DO jk = 1, jpkm1
               ztu(i1-1:i2,j1-1:j2,jk) = 0._wp
               DO jj = j1,j2
                  DO ji = i1,i2-1
                     zabe1 = rn_sponge_tra * r1_Dt * umask(ji,jj,jk) * e1e2u(ji,jj) * e3u(ji,jj,jk,Kmm_a)
                     ztu(ji,jj,jk) = zabe1 * fspu(ji,jj) * ( trbdiff(ji+1,jj  ,jk,jn) - trbdiff(ji,jj,jk,jn) ) 
                  END DO
               END DO
               ztv(i1-1:i2,j1-1:j2,jk) = 0._wp
               DO ji = i1,i2
                  DO jj = j1,j2-1
                     zabe2 = rn_sponge_tra * r1_Dt * vmask(ji,jj,jk) * e1e2v(ji,jj) * e3v(ji,jj,jk,Kmm_a)
                     ztv(ji,jj,jk) = zabe2 * fspv(ji,jj) * ( trbdiff(ji  ,jj+1,jk,jn) - trbdiff(ji,jj,jk,jn) )
                  END DO
               END DO
               !
               IF( ln_zps ) THEN      ! set gradient at partial step level
                  DO jj = j1,j2
                     DO ji = i1,i2
                        ! last level
                        iku = mbku(ji,jj)
                        ikv = mbkv(ji,jj)
                        IF( iku == jk )   ztu(ji,jj,jk) = 0._wp
                        IF( ikv == jk )   ztv(ji,jj,jk) = 0._wp
                     END DO
                  END DO
               ENDIF
            END DO
            !
! JC: there is something wrong with the Laplacian in corners
            DO jk = 1, jpkm1
               DO jj = j1,j2
                  DO ji = i1,i2
                     IF (.NOT. tabspongedone_trn(ji,jj)) THEN 
                        zbtr = r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kmm_a)
                        ! horizontal diffusive trends
                        ztra = zbtr * ( ztu(ji,jj,jk) - ztu(ji-1,jj,jk)   & 
                          &           + ztv(ji,jj,jk) - ztv(ji,jj-1,jk) ) &
                          &   - rn_trelax_tra * r1_Dt * fspt(ji,jj) * trbdiff(ji,jj,jk,jn)
                        ! add it to the general tracer trends
                        tr(ji,jj,jk,jn,Krhs_a) = tr(ji,jj,jk,jn,Krhs_a) + ztra
                     ENDIF
                  END DO
               END DO

            END DO
            !
         END DO
         !
         tabspongedone_trn(i1:i2,j1:j2) = .TRUE.
         !
      ENDIF
      !
   END SUBROUTINE interptrn_sponge

#else
   !!----------------------------------------------------------------------
   !!   Empty module                                           no TOP AGRIF
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE agrif_top_sponge_empty
      WRITE(*,*)  'agrif_top_sponge : You should not have seen this print! error?'
   END SUBROUTINE agrif_top_sponge_empty
#endif

   !!======================================================================
END MODULE agrif_top_sponge
