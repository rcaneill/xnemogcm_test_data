MODULE agrif_top_interp
   !!======================================================================
   !!                   ***  MODULE  agrif_top_interp  ***
   !! AGRIF: interpolation package for TOP
   !!======================================================================
   !! History :  2.0  !  ??? 
   !!----------------------------------------------------------------------
#if defined key_agrif && defined key_top
   !!----------------------------------------------------------------------
   !!   'key_agrif'                                              AGRIF zoom
   !!   'key_top'                                           on-line tracers
   !!----------------------------------------------------------------------
   USE par_oce
   USE oce
   USE dom_oce      
   USE agrif_oce
   USE agrif_top_sponge
   USE par_trc
   USE trc
   USE vremap
   !
   USE lib_mpp     ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC Agrif_trc, interptrn

   !! * Substitutions
#  include "domzgr_substitute.h90"
  !!----------------------------------------------------------------------
   !! NEMO/NST 4.0 , NEMO Consortium (2018)
   !! $Id: agrif_top_interp.F90 14218 2020-12-18 16:44:52Z jchanut $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE Agrif_trc
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE Agrif_trc  ***
      !!----------------------------------------------------------------------
      !
      IF( Agrif_Root() )   RETURN
      !
      Agrif_SpecialValue    = 0._wp
      Agrif_UseSpecialValue = l_spc_top 
      l_vremap              = ln_vert_remap
      !
      CALL Agrif_Bc_variable( trn_id, procname=interptrn )
      !
      Agrif_UseSpecialValue = .FALSE.
      l_vremap              = .FALSE.
      !
   END SUBROUTINE Agrif_trc

   SUBROUTINE interptrn( ptab, i1, i2, j1, j2, k1, k2, n1, n2, before )
      !!----------------------------------------------------------------------
      !!                  *** ROUTINE interptrn ***
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2), INTENT(inout) ::   ptab
      INTEGER                                     , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2, n1, n2
      LOGICAL                                     , INTENT(in   ) ::   before
      !
      INTEGER  ::   ji, jj, jk, jn  ! dummy loop indices
      INTEGER  ::   N_in, N_out
      INTEGER  :: item
      ! vertical interpolation:
      REAL(wp) :: zhtot, zwgt
      REAL(wp), DIMENSION(k1:k2,1:jptra) :: tabin, tabin_i
      REAL(wp), DIMENSION(k1:k2) :: z_in, h_in
      REAL(wp), DIMENSION(1:jpk) :: h_out, z_out
      !!----------------------------------------------------------------------

      IF( before ) THEN

         item = Kmm_a
         IF( l_ini_child )   Kmm_a = Kbb_a  

         DO jn = 1,jptra
            DO jk=k1,k2-1
               DO jj=j1,j2
                 DO ji=i1,i2
                       ptab(ji,jj,jk,jn) = tr(ji,jj,jk,jn,Kmm_a)
                 END DO
              END DO
           END DO
         END DO

         IF( l_vremap .OR. l_ini_child .OR. ln_zps ) THEN
            ! Fill cell depths (i.e. gdept) to be interpolated
            ! Warning: these are masked, hence extrapolated prior interpolation.
            DO jj=j1,j2
               DO ji=i1,i2
                  ptab(ji,jj,k1,jptra+1) = 0.5_wp * tmask(ji,jj,k1) * e3w(ji,jj,k1,Kmm_a)
                  DO jk=k1+1,k2-1
                     ptab(ji,jj,jk,jptra+1) = tmask(ji,jj,jk) * &
                        & ( ptab(ji,jj,jk-1,jptra+1) + e3w(ji,jj,jk,Kmm_a) )
                  END DO
               END DO
            END DO

            ! Save ssh at last level:
            IF (.NOT.ln_linssh) THEN
               ptab(i1:i2,j1:j2,k2,jptra+1) = ssh(i1:i2,j1:j2,Kmm_a)*tmask(i1:i2,j1:j2,1) 
            END IF      
         ENDIF
         Kmm_a = item

      ELSE 
         item = Krhs_a
         IF( l_ini_child )   Krhs_a = Kbb_a  

         IF( l_vremap .OR. l_ini_child ) THEN
            IF (ln_linssh) THEN
               ptab(i1:i2,j1:j2,k2,n2) = 0._wp 

            ELSE ! Assuming parent volume follows child:
               ptab(i1:i2,j1:j2,k2,n2) = ssh(i1:i2,j1:j2,Krhs_a)          
            ENDIF
               
            DO jj=j1,j2
               DO ji=i1,i2
                  tr(ji,jj,:,:,Krhs_a) = 0.  
                  !
                  ! Build vertical grids:
                  ! N_in = mbkt_parent(ji,jj)
                  ! Input grid (account for partial cells if any):
                  N_in = k2-1
                  z_in(1) = ptab(ji,jj,1,n2) - ptab(ji,jj,k2,n2)
                  DO jk=2,k2
                     z_in(jk) = ptab(ji,jj,jk,n2) - ptab(ji,jj,k2,n2)
                     IF (( z_in(jk) <= z_in(jk-1) ).OR.(z_in(jk)>ht_0(ji,jj))) EXIT
                  END DO
                  N_in = jk-1
                  DO jk=1, N_in
                     tabin(jk,1:jptra) = ptab(ji,jj,jk,1:jptra)
                  END DO

                  IF (ssmask(ji,jj)==1._wp) THEN
                     N_out = mbkt(ji,jj)
                  ELSE
                     N_out = 0
                  ENDIF

                  IF (N_in*N_out > 0) THEN
                     IF ( l_vremap ) THEN
                        DO jk = 1, N_in
                           h_in(jk) = e3t0_parent(ji,jj,jk) * & 
                             &       (1._wp + ptab(ji,jj,k2,n2)/(ht0_parent(ji,jj)*ssmask(ji,jj) + 1._wp - ssmask(ji,jj)))
                        END DO
                        z_in(1) = 0.5_wp * h_in(1)
                        DO jk=2,N_in
                           z_in(jk) = z_in(jk-1) + 0.5_wp * ( h_in(jk) + h_in(jk-1) )
                        END DO
                        z_in(1:N_in) = z_in(1:N_in)  - ptab(ji,jj,k2,n2)
                     ENDIF                              

                     ! Output (Child) grid:
                     DO jk=1,N_out
                        h_out(jk) = e3t(ji,jj,jk,Krhs_a)
                     END DO
                     z_out(1) = 0.5_wp * e3w(ji,jj,1,Krhs_a) 
                     DO jk=2,N_out
                        z_out(jk) = z_out(jk-1) + e3w(ji,jj,jk,Krhs_a) 
                     END DO
                     IF (.NOT.ln_linssh) z_out(1:N_out) = z_out(1:N_out)  - ssh(ji,jj,Krhs_a)            

                     IF( l_ini_child ) THEN
                        CALL remap_linear(tabin(1:N_in,1:jptra),z_in(1:N_in),tr(ji,jj,1:N_out,1:jptra,Krhs_a),        &
                                      &   z_out(1:N_out),N_in,N_out,jptra)  
                     ELSE     
                        CALL reconstructandremap(tabin(1:N_in,1:jptra),h_in(1:N_in),tr(ji,jj,1:N_out,1:jptra,Krhs_a), &
                                      &   h_out(1:N_out),N_in,N_out,jptra)   
                     ENDIF
                  ENDIF
               END DO
            END DO
            Krhs_a = item
 
         ELSE

            IF ( Agrif_Parent(ln_zps) ) THEN ! Account for partial cells 
                                             ! linear vertical interpolation
               DO jj=j1,j2
                  DO ji=i1,i2
                     !
                     N_in  = mbkt(ji,jj)
                     N_out = mbkt(ji,jj)
                     z_in(1) = ptab(ji,jj,1,n2)
                     tabin(1,1:jptra) = ptab(ji,jj,1,1:jptra)
                     DO jk=2, N_in
                        z_in(jk) = ptab(ji,jj,jk,n2)
                        tabin(jk,1:jptra) = ptab(ji,jj,jk,1:jptra)
                     END DO
                     IF (.NOT.ln_linssh) z_in(1:N_in) = z_in(1:N_in) - ptab(ji,jj,k2,n2)
                     z_out(1) = 0.5_wp * e3w(ji,jj,1,Krhs_a)
                     DO jk=2, N_out
                        z_out(jk) = z_out(jk-1) + e3w(ji,jj,jk,Krhs_a)
                     END DO
                     IF (.NOT.ln_linssh) z_out(1:N_out) = z_out(1:N_out) - ssh(ji,jj,Krhs_a)
                     CALL remap_linear(tabin(1:N_in,1:jptra),z_in(1:N_in),ptab(ji,jj,1:N_out,1:jptra), &
                                   &   z_out(1:N_out),N_in,N_out,jptra)  
                  END DO
               END DO

            ENDIF

            DO jn=1, jptra
                tr(i1:i2,j1:j2,1:jpkm1,jn,Krhs_a)=ptab(i1:i2,j1:j2,1:jpkm1,jn)*tmask(i1:i2,j1:j2,1:jpkm1) 
            END DO
         ENDIF

      ENDIF
      !
   END SUBROUTINE interptrn

#else
   !!----------------------------------------------------------------------
   !!   Empty module                                           no TOP AGRIF
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE Agrif_TOP_Interp_empty
      !!---------------------------------------------
      !!   *** ROUTINE agrif_Top_Interp_empty ***
      !!---------------------------------------------
      WRITE(*,*)  'agrif_top_interp : You should not have seen this print! error?'
   END SUBROUTINE Agrif_TOP_Interp_empty
#endif

   !!======================================================================
END MODULE agrif_top_interp
