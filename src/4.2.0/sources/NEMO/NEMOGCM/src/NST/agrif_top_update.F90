#undef DECAL_FEEDBACK

MODULE agrif_top_update
   !!======================================================================
   !!                ***  MODULE agrif_top_update  ***
   !! AGRIF :   update package for passive tracers (TOP) 
   !!======================================================================
   !! History :  
   !!----------------------------------------------------------------------
#if defined key_agrif && defined key_top
   !!----------------------------------------------------------------------
   !!   'key_agrif'                                              AGRIF zoom
   !!   'key_TOP'                                           on-line tracers
   !!----------------------------------------------------------------------
   USE par_oce
   USE oce
   USE dom_oce
   USE agrif_oce
   USE par_trc
   USE trc
   USE vremap

   IMPLICIT NONE
   PRIVATE

   PUBLIC Agrif_Update_Trc

   !! * Substitutions
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/NST 4.0 , NEMO Consortium (2018)
   !! $Id: agrif_top_update.F90 15265 2021-09-16 11:13:13Z jchanut $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE Agrif_Update_Trc( )
      !!----------------------------------------------------------------------
      !!                   *** ROUTINE Agrif_Update_Trc ***
      !!----------------------------------------------------------------------
      ! 
      IF (Agrif_Root()) RETURN 
      !
      l_vremap                      = ln_vert_remap
      Agrif_UseSpecialValueInUpdate = .FALSE.
      Agrif_SpecialValueFineGrid    = 0._wp

      ! 
# if ! defined DECAL_FEEDBACK
      CALL Agrif_Update_Variable(trn_id, procname=updateTRC )
!      CALL Agrif_Update_Variable( trn_id, locupdate=(/0,2/), procname=updateTRC )
# else
      CALL Agrif_Update_Variable(trn_id, locupdate=(/1,0/),procname=updateTRC )
!      CALL Agrif_Update_Variable( trn_id, locupdate=(/1,2/), procname=updateTRC )
# endif
      !
      Agrif_UseSpecialValueInUpdate = .FALSE.
      l_vremap                      = .FALSE.
      !
   END SUBROUTINE Agrif_Update_Trc

   SUBROUTINE updateTRC( tabres, i1, i2, j1, j2, k1, k2, n1, n2, before )

      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2,n1,n2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before
      !!
      INTEGER :: ji,jj,jk,jn
      REAL(wp) :: ztb, ztnu, ztno, ze3b
      REAL(wp) :: h_in(k1:k2)
      REAL(wp) :: h_out(1:jpk)
      INTEGER  :: N_in, N_out
      REAL(wp) :: h_diff
      REAL(wp) :: tabin(k1:k2,1:jptra)
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk,1:jptra) :: tabres_child

      IF (before) THEN
         DO jn = n1,n2-1
            DO jk=k1,k2
               DO jj=j1,j2
                  DO ji=i1,i2
                     tabres(ji,jj,jk,jn) = tr(ji,jj,jk,jn,Kmm_a) * e3t(ji,jj,jk,Kmm_a) &
                                         & * e1e2t_frac(ji,jj)
                  END DO
               END DO
            END DO
         END DO
         IF ( l_vremap ) THEN
            DO jk=k1,k2
               DO jj=j1,j2
                  DO ji=i1,i2
                     tabres(ji,jj,jk,n2) = tmask(ji,jj,jk) * e3t(ji,jj,jk,Kmm_a) &
                                         & * e1e2t_frac(ji,jj)
                  END DO
               END DO
            END DO
         ENDIF
      ELSE
         IF ( l_vremap ) THEN
            tabres_child(:,:,:,:) = 0._wp
            AGRIF_SpecialValue = 0._wp
            DO jj=j1,j2
               DO ji=i1,i2
                  N_in = 0
                  DO jk=k1,k2 !k2 = jpk of child grid
                     IF (tabres(ji,jj,jk,n2) <= 1.e-6_wp  ) EXIT
                     N_in = N_in + 1
                     DO jn=n1,n2-1
                        tabin(jk,jn) = tabres(ji,jj,jk,jn)/tabres(ji,jj,jk,n2)
                     END DO
                     h_in(N_in) = tabres(ji,jj,jk,n2)
                  ENDDO
                  N_out = 0
                  DO jk=1,jpk ! jpk of parent grid
                     IF (tmask(ji,jj,jk) == 0._wp ) EXIT ! TODO: Will not work with ISF
                     N_out = N_out + 1
                     h_out(N_out) = e3t(ji,jj,jk,Kmm_a)
                  ENDDO
                  IF (N_in*N_out > 0) THEN !Remove this?
                     CALL reconstructandremap(tabin(1:N_in,1:jptra),h_in(1:N_in),tabres_child(ji,jj,1:N_out,1:jptra),h_out(1:N_out),N_in,N_out,jptra)
                  ENDIF
               ENDDO
            ENDDO

            IF (.NOT.(lk_agrif_fstep.AND.(l_1st_euler))) THEN
               ! Add asselin part
               DO jn = 1,jptra
                  DO jk = 1, jpkm1
                     DO jj = j1, j2
                        DO ji = i1, i2
                           IF( tabres_child(ji,jj,jk,jn) /= 0._wp ) THEN
                              ze3b = e3t(ji,jj,jk,Kbb_a) & ! Recover e3tb before update
                                   & - rn_atfp * ( e3t(ji,jj,jk,Kmm_a) - e3t(ji,jj,jk,Krhs_a) )
                              ztb  = tr(ji,jj,jk,jn,Kbb_a) * ze3b
                              ztnu = tabres_child(ji,jj,jk,jn) * e3t(ji,jj,jk,Kmm_a)
                              ztno = tr(ji,jj,jk,jn,Kmm_a) * e3t(ji,jj,jk,Krhs_a)
                              tr(ji,jj,jk,jn,Kbb_a) = ( ztb + rn_atfp * ( ztnu - ztno) )  &
                                        &        * tmask(ji,jj,jk) / e3t(ji,jj,jk,Kbb_a)
                           ENDIF
                        END DO
                     END DO
                  END DO
               END DO
            ENDIF
            DO jn = 1,jptra
               DO jk = 1, jpkm1
                  DO jj = j1, j2
                     DO ji = i1, i2
                        IF( tabres_child(ji,jj,jk,jn) /= 0._wp ) THEN
                           tr(ji,jj,jk,jn,Kmm_a) = tabres_child(ji,jj,jk,jn)
                        END IF
                     END DO
                  END DO
               END DO
            END DO
         ELSE
            DO jn = 1,jptra
               DO jk = k1, k2
                  tabres(i1:i2,j1:j2,jk,jn) =  tabres(i1:i2,j1:j2,jk,jn) &
                                            & * tmask(i1:i2,j1:j2,jk)
               END DO
            ENDDO
            IF (.NOT.(lk_agrif_fstep.AND.(l_1st_euler))) THEN
               ! Add asselin part
               DO jn = 1,jptra
                  DO jk = k1, k2
                     DO jj = j1, j2
                        DO ji = i1, i2
                           IF( tabres(ji,jj,jk,jn) /= 0._wp ) THEN
                              ze3b = e3t(ji,jj,jk,Kbb_a) & ! Recover e3tb before update
                                   & - rn_atfp * ( e3t(ji,jj,jk,Kmm_a) - e3t(ji,jj,jk,Krhs_a) )
                              ztb  = tr(ji,jj,jk,jn,Kbb_a) * ze3b
                              ztnu = tabres(ji,jj,jk,jn)
                              ztno = tr(ji,jj,jk,jn,Kmm_a) * e3t(ji,jj,jk,Krhs_a)
                              tr(ji,jj,jk,jn,Kbb_a) = ( ztb + rn_atfp * ( ztnu - ztno) )  &
                                        &        * tmask(ji,jj,jk) / e3t(ji,jj,jk,Kbb_a)
                           ENDIF
                        END DO
                     END DO
                  END DO
               END DO
            ENDIF
            DO jn = 1,jptra
               DO jk=k1,k2
                  DO jj=j1,j2
                     DO ji=i1,i2
                        IF( tabres(ji,jj,jk,jn) /= 0._wp ) THEN
                           tr(ji,jj,jk,jn,Kmm_a) = tabres(ji,jj,jk,jn) / e3t(ji,jj,jk,Kmm_a)
                        END IF
                     END DO
                  END DO
               END DO
            END DO
            !
         ENDIF
         IF  ((l_1st_euler).AND.(Agrif_Nb_Step()==0) ) THEN
            tr(i1:i2,j1:j2,1:jpkm1,1:jptra,Kbb_a)  = tr(i1:i2,j1:j2,1:jpkm1,1:jptra,Kmm_a)
         ENDIF
      ENDIF
      !
   END SUBROUTINE updateTRC

#else
   !!----------------------------------------------------------------------
   !!   Empty module                                           no TOP AGRIF
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE agrif_top_update_empty
      WRITE(*,*)  'agrif_top_update : You should not have seen this print! error?'
   END SUBROUTINE agrif_top_update_empty
#endif

   !!======================================================================
END MODULE agrif_top_update
