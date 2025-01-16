MODULE dynldf_lap_blp_lf
   !!======================================================================
   !!                   ***  MODULE  dynldf_lap_blp  ***
   !! Ocean dynamics:  lateral viscosity trend (laplacian and bilaplacian)
   !!======================================================================
   !! History : 3.7  ! 2014-01  (G. Madec, S. Masson)  Original code, re-entrant laplacian
   !!           4.0  ! 2020-04  (A. Nasser, G. Madec)  Add symmetric mixing tensor 
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dyn_ldf_lap   : update the momentum trend with the lateral viscosity using an iso-level   laplacian operator
   !!   dyn_ldf_blp   : update the momentum trend with the lateral viscosity using an iso-level bilaplacian operator
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
   USE dom_oce        ! ocean space and time domain
   USE domutl, ONLY : is_tile
   USE ldfdyn         ! lateral diffusion: eddy viscosity coef.
   USE ldfslp         ! iso-neutral slopes 
   USE zdf_oce        ! ocean vertical physics
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC dyn_ldf_lap_lf  ! called by dynldf.F90
   PUBLIC dyn_ldf_blp_lf  ! called by dynldf.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: dynldf_lap_blp.F90 14757 2021-04-27 15:33:44Z francesca $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dyn_ldf_lap_lf( kt, Kbb, Kmm, pu, pv, pu_rhs, pv_rhs, kpass )
      !!
      INTEGER                   , INTENT(in   ) ::   kt               ! ocean time-step index
      INTEGER                   , INTENT(in   ) ::   Kbb, Kmm         ! ocean time level indices
      INTEGER                   , INTENT(in   ) ::   kpass            ! =1/2 first or second passage
      REAL(wp), DIMENSION(:,:,:), INTENT(in   ) ::   pu, pv           ! before velocity  [m/s]
      REAL(wp), DIMENSION(:,:,:), INTENT(inout) ::   pu_rhs, pv_rhs   ! velocity trend   [m/s2]
      !!
      CALL dyn_ldf_lap_lf_t( kt, Kbb, Kmm, pu, pv, is_tile(pu), pu_rhs, pv_rhs, is_tile(pu_rhs), kpass )

   END SUBROUTINE dyn_ldf_lap_lf
  
   SUBROUTINE dyn_ldf_lap_lf_t( kt, Kbb, Kmm, pu, pv, ktuv, pu_rhs, pv_rhs, ktuv_rhs, kpass )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE dyn_ldf_lap  ***
      !!                       
      !! ** Purpose :   Compute the before horizontal momentum diffusive 
      !!      trend and add it to the general trend of momentum equation.
      !!
      !! ** Method  :   The Laplacian operator apply on horizontal velocity is 
      !!      writen as :   grad_h( ahmt div_h(U )) - curl_h( ahmf curl_z(U) ) 
      !!
      !! ** Action : - pu_rhs, pv_rhs increased by the harmonic operator applied on pu, pv.
      !!
      !! Reference : S.Griffies, R.Hallberg 2000 Mon.Wea.Rev., DOI:/ 
      !!----------------------------------------------------------------------
      INTEGER                                 , INTENT(in   ) ::   kt         ! ocean time-step index
      INTEGER                                 , INTENT(in   ) ::   Kbb, Kmm   ! ocean time level indices
      INTEGER                                 , INTENT(in   ) ::   kpass      ! =1/2 first or second passage
      INTEGER                                 , INTENT(in   ) ::   ktuv, ktuv_rhs
      REAL(wp), DIMENSION(A2D_T(ktuv)    ,JPK), INTENT(in   ) ::   pu, pv ! before velocity  [m/s]
      REAL(wp), DIMENSION(A2D_T(ktuv_rhs),JPK), INTENT(inout) ::   pu_rhs, pv_rhs   ! velocity trend   [m/s2]
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      INTEGER  ::   iij
      REAL(wp) ::   zsign        ! local scalars
      REAL(wp) ::   zcur, zcur_im1, zcur_jm1     ! local scalars
      REAL(wp) ::   zdiv, zdiv_ip1, zdiv_jp1     ! local scalars
      REAL(wp) ::   zten, zten_ip1, zten_jp1, zshe, zshe_im1, zshe_jm1  ! tension (diagonal) and shearing (anti-diagonal) terms
      !!----------------------------------------------------------------------
      !
      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                       ! Do only on the first tile
         IF( kt == nit000 .AND. lwp ) THEN
            WRITE(numout,*)
            WRITE(numout,*) 'dyn_ldf_lf : iso-level harmonic (laplacian) operator, pass=', kpass
            WRITE(numout,*) '~~~~~~~ '
         ENDIF
      ENDIF
      !
      ! Define pu_rhs/pv_rhs halo points for multi-point haloes in bilaplacian case
      IF( nldf_dyn == np_blp .AND. kpass == 1 ) THEN ; iij = nn_hls
      ELSE                                           ; iij = 1
      ENDIF
      !
      IF( kpass == 1 ) THEN   ;   zsign =  1._wp      ! bilaplacian operator require a minus sign
      ELSE                    ;   zsign = -1._wp      !  (eddy viscosity coef. >0)
      ENDIF
      !
      SELECT CASE( nn_dynldf_typ )  
      !              
      CASE ( np_typ_rot )       !==  Vorticity-Divergence operator  ==!
         !
         DO_3D( iij-1, iij-1, iij-1, iij-1, 1, jpkm1 )                           ! Horizontal slab
            !                                      ! ahm * e3 * curl  (warning: computed for ji-1,jj-1)
            zcur     = ahmf(ji  ,jj  ,jk) * e3f(ji  ,jj  ,jk) * r1_e1e2f(ji  ,jj  )               &   ! ahmf already * by fmask   
               &       * ( e2v(ji+1,jj  ) * pv(ji+1,jj  ,jk) - e2v(ji,jj) * pv(ji,jj,jk)  &
               &         - e1u(ji  ,jj+1) * pu(ji  ,jj+1,jk) + e1u(ji,jj) * pu(ji,jj,jk) )
            zcur_jm1 = ahmf(ji  ,jj-1,jk) * e3f(ji  ,jj-1,jk) * r1_e1e2f(ji  ,jj-1)               &   ! ahmf already * by fmask
               &       * ( e2v(ji+1,jj-1) * pv(ji+1,jj-1,jk) - e2v(ji,jj-1) * pv(ji,jj-1,jk)  &
               &         - e1u(ji  ,jj  ) * pu(ji  ,jj  ,jk) + e1u(ji,jj-1) * pu(ji,jj-1,jk) )
            zcur_im1 = ahmf(ji-1,jj  ,jk) * e3f(ji-1,jj  ,jk) * r1_e1e2f(ji-1,jj  )         &   ! ahmf already * by fmask
               &       * ( e2v(ji  ,jj  ) * pv(ji  ,jj  ,jk) - e2v(ji-1,jj) * pv(ji-1,jj,jk)  &
               &         - e1u(ji-1,jj+1) * pu(ji-1,jj+1,jk) + e1u(ji-1,jj) * pu(ji-1,jj,jk) )
            !                                      ! ahm * div        (warning: computed for ji,jj)
            zdiv     = ahmt(ji,jj,jk) * r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kbb)               &   ! ahmt already * by tmask
               &     * ( e2u(ji,jj)*e3u(ji,jj,jk,Kbb) * pu(ji,jj,jk) - e2u(ji-1,jj)*e3u(ji-1,jj,jk,Kbb) * pu(ji-1,jj,jk)  &
               &       + e1v(ji,jj)*e3v(ji,jj,jk,Kbb) * pv(ji,jj,jk) - e1v(ji,jj-1)*e3v(ji,jj-1,jk,Kbb) * pv(ji,jj-1,jk)  )
            zdiv_ip1 = ahmt(ji+1,jj,jk) * r1_e1e2t(ji+1,jj) / e3t(ji+1,jj,jk,Kbb)         &   ! ahmt already * by tmask
               &     * ( e2u(ji+1,jj)*e3u(ji+1,jj,jk,Kbb) * pu(ji+1,jj,jk) - e2u(ji,jj)*e3u(ji,jj,jk,Kbb) * pu(ji,jj,jk)  &
               &       + e1v(ji+1,jj)*e3v(ji+1,jj,jk,Kbb) * pv(ji+1,jj,jk) - e1v(ji+1,jj-1)*e3v(ji+1,jj-1,jk,Kbb) * pv(ji+1,jj-1,jk) )
            zdiv_jp1 = ahmt(ji,jj+1,jk) * r1_e1e2t(ji,jj+1) / e3t(ji,jj+1,jk,Kbb)         &   ! ahmt already * by tmask
               &     * ( e2u(ji,jj+1)*e3u(ji,jj+1,jk,Kbb) * pu(ji,jj+1,jk) - e2u(ji-1,jj+1)*e3u(ji-1,jj+1,jk,Kbb) * pu(ji-1,jj+1,jk)  &
               &       + e1v(ji,jj+1)*e3v(ji,jj+1,jk,Kbb) * pv(ji,jj+1,jk) - e1v(ji,jj)*e3v(ji,jj,jk,Kbb) * pv(ji,jj,jk) )
            !                                      ! - curl( curl) + grad( div )
            pu_rhs(ji,jj,jk) = pu_rhs(ji,jj,jk) + zsign * umask(ji,jj,jk) * (    &    ! * by umask is mandatory for dyn_ldf_blp use
               &              - ( zcur - zcur_jm1 ) * r1_e2u(ji,jj) / e3u(ji,jj,jk,Kmm)   &
               &              + ( zdiv_ip1 - zdiv ) * r1_e1u(ji,jj)                      )
            !
            pv_rhs(ji,jj,jk) = pv_rhs(ji,jj,jk) + zsign * vmask(ji,jj,jk) * (    &    ! * by vmask is mandatory for dyn_ldf_blp use
               &                ( zcur - zcur_im1 ) * r1_e1v(ji,jj) / e3v(ji,jj,jk,Kmm)   &
               &              + ( zdiv_jp1 - zdiv ) * r1_e2v(ji,jj)                      )
         END_3D                                                  !   End of slab
         !
      CASE ( np_typ_sym )       !==  Symmetric operator  ==!
         !
         DO_3D( iij-1, iij-1, iij-1, iij-1, 1, jpkm1 )                           ! Horizontal slab
            !                                      ! shearing stress component (F-point)   NB : ahmf has already been multiplied by fmask
            zshe = ahmf(ji,jj,jk)                                                            &
               &    * (    e1f(ji,jj)    * r1_e2f(ji,jj)                                     &
               &    * ( pu(ji,jj+1,jk) * r1_e1u(ji,jj+1)  - pu(ji,jj,jk) * r1_e1u(ji,jj) )   &
               &    +  e2f(ji,jj)    * r1_e1f(ji,jj)                                         &
               &    * ( pv(ji+1,jj,jk) * r1_e2v(ji+1,jj)  - pv(ji,jj,jk) * r1_e2v(ji,jj) )   ) 
            zshe_im1 = ahmf(ji-1,jj,jk)                                                                  &
               &        * (    e1f(ji-1,jj)    * r1_e2f(ji-1,jj)                                         &
               &        * ( pu(ji-1,jj+1,jk) * r1_e1u(ji-1,jj+1)  - pu(ji-1,jj,jk) * r1_e1u(ji-1,jj) )   &
               &        +  e2f(ji-1,jj)    * r1_e1f(ji-1,jj)                                             &
               &        * ( pv(ji  ,jj,jk) * r1_e2v(ji  ,jj)  - pv(ji-1,jj,jk) * r1_e2v(ji-1,jj) )   ) 
            zshe_jm1 = ahmf(ji,jj-1,jk)                                                                &
               &        * (    e1f(ji,jj-1)    * r1_e2f(ji,jj-1)                                       &
               &        * ( pu(ji,jj,jk) * r1_e1u(ji,jj)  - pu(ji,jj-1,jk) * r1_e1u(ji,jj-1) )         &
               &        +  e2f(ji,jj-1)    * r1_e1f(ji,jj-1)                                           &
               &        * ( pv(ji+1,jj-1,jk) * r1_e2v(ji+1,jj-1)  - pv(ji,jj-1,jk) * r1_e2v(ji,jj-1) )   ) 
            !                                      ! tension stress component (T-point)   NB : ahmt has already been multiplied by tmask
            zten        = ahmt(ji,jj,jk)                                                          &
               &     * (    e2t(ji,jj)    * r1_e1t(ji,jj)                                         &
               &         * ( pu(ji,jj,jk) * r1_e2u(ji,jj)  - pu(ji-1,jj,jk) * r1_e2u(ji-1,jj) )   &
               &         -  e1t(ji,jj)    * r1_e2t(ji,jj)                                         &
               &         * ( pv(ji,jj,jk) * r1_e1v(ji,jj)  - pv(ji,jj-1,jk) * r1_e1v(ji,jj-1) )   )   
            zten_ip1    = ahmt(ji+1,jj,jk)                                                              &
               &     * (    e2t(ji+1,jj)    * r1_e1t(ji+1,jj)                                           &
               &         * ( pu(ji+1,jj,jk) * r1_e2u(ji+1,jj)  - pu(ji,jj,jk) * r1_e2u(ji,jj) )         &
               &         -  e1t(ji+1,jj)    * r1_e2t(ji+1,jj)                                           &
               &         * ( pv(ji+1,jj,jk) * r1_e1v(ji+1,jj)  - pv(ji+1,jj-1,jk) * r1_e1v(ji+1,jj-1) ) )   
            zten_jp1    = ahmt(ji,jj+1,jk)                                                              &
               &     * (    e2t(ji,jj+1)    * r1_e1t(ji,jj+1)                                           &
               &         * ( pu(ji,jj+1,jk) * r1_e2u(ji,jj+1)  - pu(ji-1,jj+1,jk) * r1_e2u(ji-1,jj+1) ) &
               &         -  e1t(ji,jj+1)    * r1_e2t(ji,jj+1)                                           &
               &         * ( pv(ji,jj+1,jk) * r1_e1v(ji,jj+1)  - pv(ji,jj,jk) * r1_e1v(ji,jj) )   )   
            ! 
            pu_rhs(ji,jj,jk) = pu_rhs(ji,jj,jk) + zsign * r1_e1e2u(ji,jj) / e3u(ji,jj,jk,Kmm)                    &
               &    * (   (   zten_ip1 * e2t(ji+1,jj  )*e2t(ji+1,jj  ) * e3t(ji+1,jj  ,jk,Kmm)                   &
               &            - zten * e2t(ji  ,jj  )*e2t(ji  ,jj  ) * e3t(ji  ,jj  ,jk,Kmm) ) * r1_e2u(ji,jj)     & 
               &        + (   zshe * e1f(ji  ,jj  )*e1f(ji  ,jj  ) * e3f(ji  ,jj  ,jk)                           &
               &            - zshe_jm1 * e1f(ji  ,jj-1)*e1f(ji  ,jj-1) * e3f(ji  ,jj-1,jk)     ) * r1_e1u(ji,jj) )   
            !
            pv_rhs(ji,jj,jk) = pv_rhs(ji,jj,jk) + zsign * r1_e1e2v(ji,jj) / e3v(ji,jj,jk,Kmm)                    &
               &    * (   (   zshe * e2f(ji  ,jj  )*e2f(ji  ,jj  ) * e3f(ji  ,jj  ,jk)                           &
               &            - zshe_im1 * e2f(ji-1,jj  )*e2f(ji-1,jj  ) * e3f(ji-1,jj  ,jk)     ) * r1_e2v(ji,jj) &
               &        - (   zten_jp1 * e1t(ji  ,jj+1)*e1t(ji  ,jj+1) * e3t(ji  ,jj+1,jk,Kmm)                   &
               &            - zten * e1t(ji  ,jj  )*e1t(ji  ,jj  ) * e3t(ji  ,jj  ,jk,Kmm) ) * r1_e1v(ji,jj) )
            !
         END_3D
         !
      END SELECT
      !
   END SUBROUTINE dyn_ldf_lap_lf_t


   SUBROUTINE dyn_ldf_blp_lf( kt, Kbb, Kmm, pu, pv, pu_rhs, pv_rhs )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE dyn_ldf_blp  ***
      !!                    
      !! ** Purpose :   Compute the before lateral momentum viscous trend 
      !!              and add it to the general trend of momentum equation.
      !!
      !! ** Method  :   The lateral viscous trends is provided by a bilaplacian
      !!      operator applied to before field (forward in time).
      !!      It is computed by two successive calls to dyn_ldf_lap routine
      !!
      !! ** Action :   pt(:,:,:,:,Krhs)   updated with the before rotated bilaplacian diffusion
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   kt         ! ocean time-step index
      INTEGER                         , INTENT(in   ) ::   Kbb, Kmm   ! ocean time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(in   ) ::   pu, pv     ! before velocity fields
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) ::   pu_rhs, pv_rhs   ! momentum trend
      !
      REAL(wp), DIMENSION(A2D(nn_hls),jpk) ::   zulap, zvlap   ! laplacian at u- and v-point
      !!----------------------------------------------------------------------
      !
      IF( kt == nit000 )  THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'dyn_ldf_blp_lf : bilaplacian operator momentum '
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF
      !
      zulap(:,:,:) = 0._wp
      zvlap(:,:,:) = 0._wp
      !
      CALL dyn_ldf_lap_lf( kt, Kbb, Kmm, pu, pv, zulap, zvlap, 1 )   ! rotated laplacian applied to pt (output in zlap,Kbb)
      !
      CALL dyn_ldf_lap_lf( kt, Kbb, Kmm, zulap, zvlap, pu_rhs, pv_rhs, 2 )   ! rotated laplacian applied to zlap (output in pt(:,:,:,:,Krhs))
      !
   END SUBROUTINE dyn_ldf_blp_lf

   !!======================================================================
END MODULE dynldf_lap_blp_lf
