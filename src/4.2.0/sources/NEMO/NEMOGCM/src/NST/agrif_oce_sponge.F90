#define SPONGE && define SPONGE_TOP

MODULE agrif_oce_sponge
   !!======================================================================
   !!                   ***  MODULE  agrif_oce_interp  ***
   !! AGRIF: sponge package for the ocean dynamics (OCE)
   !!======================================================================
   !! History :  2.0  !  2002-06  (XXX)  Original cade
   !!             -   !  2005-11  (XXX) 
   !!            3.2  !  2009-04  (R. Benshila) 
   !!            3.6  !  2014-09  (R. Benshila) 
   !!----------------------------------------------------------------------
#if defined key_agrif
   !!----------------------------------------------------------------------
   !!   'key_agrif'                                              AGRIF zoom
   !!----------------------------------------------------------------------
   USE par_oce
   USE oce
   USE dom_oce
   !
   USE in_out_manager
   USE agrif_oce
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE iom
   USE vremap

   IMPLICIT NONE
   PRIVATE

   PUBLIC Agrif_Sponge, Agrif_Sponge_2d, Agrif_Sponge_Tra, Agrif_Sponge_Dyn
   PUBLIC interptsn_sponge, interpun_sponge, interpvn_sponge
   PUBLIC interpunb_sponge, interpvnb_sponge

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/NST 4.0 , NEMO Consortium (2018)
   !! $Id: agrif_oce_sponge.F90 15437 2021-10-22 12:21:20Z jchanut $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE Agrif_Sponge_Tra
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_Sponge_Tra ***
      !!----------------------------------------------------------------------
      REAL(wp) ::   zcoef   ! local scalar
      INTEGER  :: istart, iend, jstart, jend 
      !!----------------------------------------------------------------------
      !
#if defined SPONGE
      !! Assume persistence:
      zcoef = REAL(Agrif_rhot()-1,wp)/REAL(Agrif_rhot())

      Agrif_SpecialValue    = 0._wp
      Agrif_UseSpecialValue = l_spc_tra 
      l_vremap              = ln_vert_remap
      tabspongedone_tsn     = .FALSE.
      !
      CALL Agrif_Bc_Variable( ts_sponge_id, calledweight=zcoef, procname=interptsn_sponge )
      !
      Agrif_UseSpecialValue = .FALSE.
      l_vremap              = .FALSE.
#endif
      !
      CALL iom_put( 'agrif_spu', fspu(:,:))
      CALL iom_put( 'agrif_spv', fspv(:,:))
      !
   END SUBROUTINE Agrif_Sponge_Tra


   SUBROUTINE Agrif_Sponge_dyn
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_Sponge_dyn ***
      !!----------------------------------------------------------------------
      REAL(wp) ::   zcoef   ! local scalar
      !!----------------------------------------------------------------------
      !
#if defined SPONGE
      zcoef = REAL(Agrif_rhot()-1,wp)/REAL(Agrif_rhot())

      Agrif_SpecialValue    = 0._wp
      Agrif_UseSpecialValue = ln_spc_dyn
      l_vremap              = ln_vert_remap
      use_sign_north        = .TRUE.
      sign_north            = -1._wp
      !
      tabspongedone_u = .FALSE.
      tabspongedone_v = .FALSE.         
      CALL Agrif_Bc_Variable( un_sponge_id, calledweight=zcoef, procname=interpun_sponge )
      !
      tabspongedone_u = .FALSE.
      tabspongedone_v = .FALSE.
      CALL Agrif_Bc_Variable( vn_sponge_id, calledweight=zcoef, procname=interpvn_sponge )
      
      IF ( nn_shift_bar>0 ) THEN ! then split sponge between 2d and 3d
         zcoef = REAL(Agrif_NbStepint(),wp)/REAL(Agrif_rhot()) ! forward tsplit
         tabspongedone_u = .FALSE.
         tabspongedone_v = .FALSE.         
         CALL Agrif_Bc_Variable( unb_sponge_id, calledweight=zcoef, procname=interpunb_sponge )
         !
         tabspongedone_u = .FALSE.
         tabspongedone_v = .FALSE.
         CALL Agrif_Bc_Variable( vnb_sponge_id, calledweight=zcoef, procname=interpvnb_sponge )
      ENDIF
      !
      Agrif_UseSpecialValue = .FALSE.
      use_sign_north        = .FALSE.
      l_vremap              = .FALSE.
      !
#endif
      !
      CALL iom_put( 'agrif_spt', fspt(:,:))
      CALL iom_put( 'agrif_spf', fspf(:,:))
      !
   END SUBROUTINE Agrif_Sponge_dyn


   SUBROUTINE Agrif_Sponge
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE  Agrif_Sponge ***
      !!----------------------------------------------------------------------
      INTEGER  ::   ji, jj, ind1, ind2
      INTEGER  ::   ispongearea, jspongearea
      REAL(wp) ::   z1_ispongearea, z1_jspongearea
      REAL(wp), DIMENSION(jpi,jpj) :: ztabramp
      !!----------------------------------------------------------------------
      !
      ! Sponge 1d example with:
      !      iraf = 3 ; nbghost = 3 ; nn_sponge_len = 2
      !                        
      !coarse :     U     T     U     T     U     T     U
      !|            |           |           |           |
      !fine :     t u t u t u t u t u t u t u t u t u t u t
      !sponge val:0 1   1   1   1  5/6 4/6 3/6 2/6 1/6  0 
      !           |   ghost     | <-- sponge area  -- > |
      !           |   points    |                       |
      !                         |--> dynamical interface

#if defined SPONGE || defined SPONGE_TOP
      ! Define ramp from boundaries towards domain interior at F-points
      ! Store it in ztabramp

      ispongearea    = nn_sponge_len * Agrif_irhox()
      z1_ispongearea = 1._wp / REAL( MAX(ispongearea,1), wp )
      jspongearea    = nn_sponge_len * Agrif_irhoy()
      z1_jspongearea = 1._wp / REAL( MAX(jspongearea,1), wp )
         
      ztabramp(:,:) = 0._wp

      IF( lk_west ) THEN                            ! --- West --- !
         ind1 = nn_hls + nbghostcells               ! halo + nbghostcells
         ind2 = nn_hls + nbghostcells + ispongearea 
         DO ji = mi0(ind1), mi1(ind2)   
            DO jj = 1, jpj               
               ztabramp(ji,jj) =                       REAL(ind2 - mig(ji), wp) * z1_ispongearea
            END DO
         END DO
         ! ghost cells:
         ind1 = 1
         ind2 = nn_hls +  nbghostcells              ! halo + nbghostcells
         DO ji = mi0(ind1), mi1(ind2)   
            DO jj = 1, jpj               
               ztabramp(ji,jj) = 1._wp
            END DO
         END DO
      ENDIF
      IF( lk_east ) THEN                             ! --- East --- !
         ind1 = jpiglo - ( nn_hls + nbghostcells -1 ) - ispongearea - 1
         ind2 = jpiglo - ( nn_hls + nbghostcells -1 ) - 1    ! halo + land + nbghostcells - 1
         DO ji = mi0(ind1), mi1(ind2)
            DO jj = 1, jpj
               ztabramp(ji,jj) = MAX( ztabramp(ji,jj), REAL(mig(ji) - ind1, wp) * z1_ispongearea ) 
            END DO
         END DO
         ! ghost cells:
         ind1 = jpiglo - ( nn_hls + nbghostcells -1 ) - 1    ! halo + land + nbghostcells - 1
         ind2 = jpiglo - 1
         DO ji = mi0(ind1), mi1(ind2)
            DO jj = 1, jpj
               ztabramp(ji,jj) = 1._wp
            END DO
         END DO
      ENDIF      
      IF( lk_south ) THEN                            ! --- South --- !
         ind1 = nn_hls + nbghostcells                ! halo + nbghostcells
         ind2 = nn_hls + nbghostcells + jspongearea 
         DO jj = mj0(ind1), mj1(ind2) 
            DO ji = 1, jpi
               ztabramp(ji,jj) = MAX( ztabramp(ji,jj), REAL(ind2 - mjg(jj), wp) * z1_jspongearea )
            END DO
         END DO
         ! ghost cells:
         ind1 = 1
         ind2 = nn_hls + nbghostcells                ! halo + nbghostcells
         DO jj = mj0(ind1), mj1(ind2) 
            DO ji = 1, jpi
               ztabramp(ji,jj) = 1._wp
            END DO
         END DO
      ENDIF
      IF( lk_north ) THEN                            ! --- North --- !
         ind1 = jpjglo - ( nn_hls + nbghostcells -1 ) - jspongearea - 1
         ind2 = jpjglo - ( nn_hls + nbghostcells -1 ) - 1    ! halo + nbghostcells - 1
         DO jj = mj0(ind1), mj1(ind2)
            DO ji = 1, jpi
               ztabramp(ji,jj) = MAX( ztabramp(ji,jj), REAL(mjg(jj) - ind1, wp) * z1_jspongearea ) 
            END DO
         END DO
         ! ghost cells:
         ind1 = jpjglo - ( nn_hls + nbghostcells -1 )      ! halo + land + nbghostcells - 1
         ind2 = jpjglo
         DO jj = mj0(ind1), mj1(ind2)
            DO ji = 1, jpi
               ztabramp(ji,jj) = 1._wp
            END DO
         END DO
      ENDIF      
      !
      ! Tracers
      fspu(:,:) = 0._wp
      fspv(:,:) = 0._wp
      DO_2D( 0, 0, 0, 0 )
         fspu(ji,jj) = 0.5_wp * ( ztabramp(ji,jj) + ztabramp(ji,jj-1) ) * ssumask(ji,jj)
         fspv(ji,jj) = 0.5_wp * ( ztabramp(ji,jj) + ztabramp(ji-1,jj) ) * ssvmask(ji,jj)
      END_2D

      ! Dynamics
      fspt(:,:) = 0._wp
      fspf(:,:) = 0._wp
      DO_2D( 0, 0, 0, 0 )
         fspt(ji,jj) = 0.25_wp * ( ztabramp(ji  ,jj  ) + ztabramp(ji-1,jj  ) &
                               &  +ztabramp(ji  ,jj-1) + ztabramp(ji-1,jj-1) ) * ssmask(ji,jj)
         fspf(ji,jj) = ztabramp(ji,jj) * ssvmask(ji,jj) * ssvmask(ji,jj+1)
      END_2D
      
      CALL lbc_lnk( 'agrif_Sponge', fspu, 'U', 1._wp, fspv, 'V', 1._wp, fspt, 'T', 1._wp, fspf, 'F', 1._wp )
      !
      ! Remove vertical interpolation where not needed:
      ! (A null value in mbkx arrays does the job)
      WHERE (ssumask(:,:) == 0._wp) mbku_parent(:,:) = 0
      WHERE (ssvmask(:,:) == 0._wp) mbkv_parent(:,:) = 0
      WHERE (ssmask(:,:) == 0._wp) mbkt_parent(:,:) = 0
      !
#endif
      !
   END SUBROUTINE Agrif_Sponge


   SUBROUTINE Agrif_Sponge_2d
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE  Agrif_Sponge_2d ***
      !!----------------------------------------------------------------------
      INTEGER  ::   ji, jj, ind1, ind2, ishift, jshift
      INTEGER  ::   ispongearea, jspongearea
      REAL(wp) ::   z1_ispongearea, z1_jspongearea
      REAL(wp), DIMENSION(jpi,jpj) :: ztabramp
      !!----------------------------------------------------------------------
      !
      ! Sponge 1d example with:
      !      iraf = 3 ; nbghost = 3 ; nn_sponge_len = 2
      !                        
      !coarse :     U     T     U     T     U     T     U
      !|            |           |           |           |
      !fine :     t u t u t u t u t u t u t u t u t u t u t
      !sponge val:0 1   1   1   1  5/6 4/6 3/6 2/6 1/6  0 
      !           |   ghost     | <-- sponge area  -- > |
      !           |   points    |                       |
      !                         |--> dynamical interface

#if defined SPONGE || defined SPONGE_TOP
      ! Define ramp from boundaries towards domain interior at F-points
      ! Store it in ztabramp

      ispongearea    = nn_sponge_len * Agrif_irhox()
      z1_ispongearea = 1._wp / REAL( MAX(ispongearea,1), wp )
      jspongearea    = nn_sponge_len * Agrif_irhoy()
      z1_jspongearea = 1._wp / REAL( MAX(jspongearea,1), wp )
      ishift = nn_shift_bar * Agrif_irhox()
      jshift = nn_shift_bar * Agrif_irhoy()
        
      ztabramp(:,:) = 0._wp

      IF( lk_west ) THEN                             ! --- West --- !
         ind1 = nn_hls + nbghostcells + ishift
         ind2 = nn_hls + nbghostcells + ishift + ispongearea 
         DO ji = mi0(ind1), mi1(ind2)   
            DO jj = 1, jpj               
               ztabramp(ji,jj) =                       REAL(ind2 - mig(ji), wp) * z1_ispongearea
            END DO
         END DO
         ! ghost cells:
         ind1 = 1
         ind2 = nn_hls + nbghostcells + ishift               ! halo + nbghostcells
         DO ji = mi0(ind1), mi1(ind2)   
            DO jj = 1, jpj               
               ztabramp(ji,jj) = 1._wp
            END DO
         END DO
      ENDIF
      IF( lk_east ) THEN                             ! --- East --- !
         ind1 = jpiglo - ( nn_hls + nbghostcells -1  + ishift) - ispongearea - 1
         ind2 = jpiglo - ( nn_hls + nbghostcells -1  + ishift) - 1    ! halo + nbghostcells - 1
         DO ji = mi0(ind1), mi1(ind2)
            DO jj = 1, jpj
               ztabramp(ji,jj) = MAX( ztabramp(ji,jj), REAL(mig(ji) - ind1, wp) * z1_ispongearea ) 
            END DO
         END DO
         ! ghost cells:
         ind1 = jpiglo - ( nn_hls + nbghostcells -1 + ishift) - 1    ! halo + nbghostcells - 1
         ind2 = jpiglo - 1
         DO ji = mi0(ind1), mi1(ind2)
            DO jj = 1, jpj
               ztabramp(ji,jj) = 1._wp
            END DO
         END DO
      ENDIF      
      IF( lk_south ) THEN                            ! --- South --- !
         ind1 = nn_hls + nbghostcells + jshift                ! halo + nbghostcells
         ind2 = nn_hls + nbghostcells + jshift + jspongearea 
         DO jj = mj0(ind1), mj1(ind2) 
            DO ji = 1, jpi
               ztabramp(ji,jj) = MAX( ztabramp(ji,jj), REAL(ind2 - mjg(jj), wp) * z1_jspongearea )
            END DO
         END DO
         ! ghost cells:
         ind1 = 1
         ind2 = nn_hls + nbghostcells + jshift                ! halo + land + nbghostcells
         DO jj = mj0(ind1), mj1(ind2) 
            DO ji = 1, jpi
               ztabramp(ji,jj) = 1._wp
            END DO
         END DO
      ENDIF
      IF( lk_north ) THEN                            ! --- North --- !
         ind1 = jpjglo - ( nn_hls + nbghostcells -1 + jshift) - jspongearea - 1
         ind2 = jpjglo - ( nn_hls + nbghostcells -1 + jshift) - 1    ! halo + land + nbghostcells - 1
         DO jj = mj0(ind1), mj1(ind2)
            DO ji = 1, jpi
               ztabramp(ji,jj) = MAX( ztabramp(ji,jj), REAL(mjg(jj) - ind1, wp) * z1_jspongearea ) 
            END DO
         END DO
         ! ghost cells:
         ind1 = jpjglo - ( nn_hls + nbghostcells -1 + jshift)      ! halo + land + nbghostcells - 1
         ind2 = jpjglo
         DO jj = mj0(ind1), mj1(ind2)
            DO ji = 1, jpi
               ztabramp(ji,jj) = 1._wp
            END DO
         END DO
      ENDIF
      !
      ! Tracers
      fspu_2d(:,:) = 0._wp
      fspv_2d(:,:) = 0._wp
      DO_2D( 0, 0, 0, 0 )
         fspu_2d(ji,jj) = 0.5_wp * ( ztabramp(ji,jj) + ztabramp(ji,jj-1) ) * ssumask(ji,jj)
         fspv_2d(ji,jj) = 0.5_wp * ( ztabramp(ji,jj) + ztabramp(ji-1,jj) ) * ssvmask(ji,jj)
      END_2D

      ! Dynamics
      fspt_2d(:,:) = 0._wp
      fspf_2d(:,:) = 0._wp
      DO_2D( 0, 0, 0, 0 )
         fspt_2d(ji,jj) = 0.25_wp * ( ztabramp(ji  ,jj  ) + ztabramp(ji-1,jj  ) &
                               &  +ztabramp(ji  ,jj-1) + ztabramp(ji-1,jj-1) ) * ssmask(ji,jj)
         fspf_2d(ji,jj) = ztabramp(ji,jj) * ssvmask(ji,jj) * ssvmask(ji,jj+1)
         END_2D
      CALL lbc_lnk( 'agrif_Sponge_2d', fspu_2d, 'U', 1._wp, fspv_2d, 'V', 1._wp, fspt_2d, 'T', 1._wp, fspf_2d, 'F', 1._wp )
      !
#endif
      !
   END SUBROUTINE Agrif_Sponge_2d


   SUBROUTINE interptsn_sponge( tabres, i1, i2, j1, j2, k1, k2, n1, n2, before) 
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE interptsn_sponge ***
      !!----------------------------------------------------------------------
      INTEGER                                     , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2, n1, n2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2), INTENT(inout) ::   tabres
      LOGICAL                                     , INTENT(in   ) ::   before
      !
      INTEGER  ::   ji, jj, jk, jn   ! dummy loop indices
      INTEGER  ::   iku, ikv
      REAL(wp) :: ztsa, zabe1, zabe2, zbtr, zhtot
      REAl(wp) :: zflag, zdmod, zdtot
      REAL(wp), DIMENSION(i1-1:i2,j1-1:j2,jpk) :: ztu, ztv
      REAL(wp), DIMENSION(i1:i2,j1:j2,jpk,n1:n2) ::tsbdiff
      ! vertical interpolation:
      REAL(wp), DIMENSION(i1:i2,j1:j2,jpk,n1:n2) ::tabres_child
      REAL(wp), DIMENSION(k1:k2,n1:n2-1) :: tabin, tabin_i
      REAL(wp), DIMENSION(k1:k2) :: z_in, z_in_i, h_in_i
      REAL(wp), DIMENSION(1:jpk) :: h_out, z_out
      INTEGER :: N_in, N_out
      !!----------------------------------------------------------------------
      !
      IF( before ) THEN
         DO jn = 1, jpts
            DO jk=k1,k2-1
               DO jj=j1,j2
                  DO ji=i1,i2
                     ! JC: masking is mandatory here: before tracer field seems 
                     !     to hold non zero values where tmask=0
                     tabres(ji,jj,jk,jn) = ts(ji,jj,jk,jn,Kbb_a) * tmask(ji,jj,jk)
                  END DO
               END DO
            END DO
         END DO

         IF ( l_vremap.OR.ln_zps ) THEN

            ! Fill cell depths (i.e. gdept) to be interpolated
            ! Warning: these are masked, hence extrapolated prior interpolation.
            DO jj=j1,j2
               DO ji=i1,i2
                  tabres(ji,jj,k1,jpts+1) = 0.5_wp * tmask(ji,jj,k1) * e3w(ji,jj,k1,Kbb_a)
                  DO jk=k1+1,k2-1
                     tabres(ji,jj,jk,jpts+1) = tmask(ji,jj,jk) * &
                        & ( tabres(ji,jj,jk-1,jpts+1) + e3w(ji,jj,jk,Kbb_a) )
                  END DO
               END DO
            END DO

            ! Save ssh at last level:
            IF ( .NOT.ln_linssh ) THEN
               tabres(i1:i2,j1:j2,k2,jpts+1) = ssh(i1:i2,j1:j2,Kbb_a)*tmask(i1:i2,j1:j2,1) 
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
                        tabin(jk,1:jpts) = tabres(ji,jj,jk,1:jpts)
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
!                     CALL remap_linear(tabin(1:N_in,1:jpts),z_in(1:N_in),tabin_i(1:N_in,1:jpts),z_in_i(1:N_in),N_in,N_in,jpts)
!                     CALL reconstructandremap(tabin_i(1:N_in,1:jpts),h_in_i(1:N_in),tabres_child(ji,jj,1:N_out,1:jpts),h_out(1:N_out),N_in,N_out,jpts)
                     CALL reconstructandremap(tabin(1:N_in,1:jpts),h_in_i(1:N_in),tabres_child(ji,jj,1:N_out,1:jpts),h_out(1:N_out),N_in,N_out,jpts)
!                     CALL remap_linear(tabin(1:N_in,1:jpts),z_in(1:N_in),tabres_child(ji,jj,1:N_out,1:jpts),z_out(1:N_in),N_in,N_out,jpts)  
                  ENDIF
               END DO
            END DO

            DO jj=j1,j2
               DO ji=i1,i2
                  DO jk=1,jpkm1
                     tsbdiff(ji,jj,jk,1:jpts) = (ts(ji,jj,jk,1:jpts,Kbb_a) - tabres_child(ji,jj,jk,1:jpts)) * tmask(ji,jj,jk)
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
                     tabin(1,1:jpts) = tabres(ji,jj,1,1:jpts)
                     DO jk=2, N_in
                        z_in(jk) = tabres(ji,jj,jk,n2)
                        tabin(jk,1:jpts) = tabres(ji,jj,jk,1:jpts)
                     END DO 
                     IF (.NOT.ln_linssh) z_in(1:N_in) = z_in(1:N_in) - tabres(ji,jj,k2,n2)

                     z_out(1) = 0.5_wp * e3w(ji,jj,1,Kbb_a)
                     DO jk=2, N_out
                        z_out(jk) = z_out(jk-1) + e3w(ji,jj,jk,Kbb_a) 
                     END DO 
                     IF (.NOT.ln_linssh) z_out(1:N_out) = z_out(1:N_out) - ssh(ji,jj,Kbb_a)

                     CALL remap_linear(tabin(1:N_in,1:jpts), z_in(1:N_in), tabres(ji,jj,1:N_out,1:jpts), &
                                         &   z_out(1:N_out), N_in, N_out, jpts)
                  END DO
               END DO
            ENDIF

            DO jj=j1,j2
               DO ji=i1,i2
                  DO jk=1,jpkm1
                     tsbdiff(ji,jj,jk,1:jpts) = (ts(ji,jj,jk,1:jpts,Kbb_a) - tabres(ji,jj,jk,1:jpts))*tmask(ji,jj,jk)
                  END DO
               END DO
            END DO

         END IF

         DO jn = 1, jpts            
            DO jk = 1, jpkm1
               ztu(i1-1:i2,j1-1:j2,jk) = 0._wp
               DO jj = j1,j2
                  DO ji = i1,i2-1
                     zabe1 = rn_sponge_tra * r1_Dt * umask(ji,jj,jk) * e1e2u(ji,jj) * e3u(ji,jj,jk,Kmm_a)
                     zdtot =  tsbdiff(ji+1,jj,jk,jn) -  tsbdiff(ji,jj,jk,jn) 
                     zdmod =       ts(ji+1,jj,jk,jn,Kbb_a) - ts(ji,jj,jk,jn,Kbb_a)
                     zflag = 0.5_wp + SIGN(0.5_wp, zdtot*zdmod)
                     ztu(ji,jj,jk) = zabe1 * fspu(ji,jj) * ( zflag * zdtot + (1._wp - zflag) * zdmod ) 
                  END DO
               END DO
               ztv(i1-1:i2,j1-1:j2,jk) = 0._wp
               DO ji = i1,i2
                  DO jj = j1,j2-1
                     zabe2 = rn_sponge_tra * r1_Dt * vmask(ji,jj,jk) * e1e2v(ji,jj) * e3v(ji,jj,jk,Kmm_a)
                     ztv(ji,jj,jk) = zabe2 * fspv(ji,jj) * ( tsbdiff(ji  ,jj+1,jk,jn) - tsbdiff(ji,jj,jk,jn) )
                     zdtot =  tsbdiff(ji,jj+1,jk,jn) -  tsbdiff(ji,jj,jk,jn) 
                     zdmod =       ts(ji,jj+1,jk,jn,Kbb_a) - ts(ji,jj,jk,jn,Kbb_a)
                     zflag = 0.5_wp + SIGN(0.5_wp, zdtot*zdmod)
                     ztv(ji,jj,jk) = zabe2 * fspv(ji,jj) * ( zflag * zdtot + (1._wp - zflag) * zdmod ) 
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
                     IF (.NOT. tabspongedone_tsn(ji,jj)) THEN 
                        zbtr = r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kmm_a)
                        ! horizontal diffusive trends
                        ztsa = zbtr * ( ztu(ji,jj,jk) - ztu(ji-1,jj,jk)   & 
                          &           + ztv(ji,jj,jk) - ztv(ji,jj-1,jk) ) &
                          &   - rn_trelax_tra * r1_Dt * fspt(ji,jj) * tsbdiff(ji,jj,jk,jn)
                        ! add it to the general tracer trends
                        ts(ji,jj,jk,jn,Krhs_a) = ts(ji,jj,jk,jn,Krhs_a) + ztsa
                     ENDIF
                  END DO
               END DO

            END DO
            !
         END DO
         !
         tabspongedone_tsn(i1:i2,j1:j2) = .TRUE.
         !
      ENDIF
      !
   END SUBROUTINE interptsn_sponge

   
   SUBROUTINE interpun_sponge(tabres,i1,i2,j1,j2,k1,k2,m1,m2, before)
      !!---------------------------------------------
      !!   *** ROUTINE interpun_sponge ***
      !!---------------------------------------------    
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2,m1,m2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,m1:m2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before

      INTEGER  :: ji,jj,jk,jmax
      INTEGER  :: ind1
      ! sponge parameters 
      REAL(wp) :: ze2u, ze1v, zua, zva, zbtr, zrhoy
      REAL(wp), DIMENSION(i1:i2,j1:j2) :: zsshu
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk) :: ubdiff
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk) :: rotdiff, hdivdiff
      ! vertical interpolation:
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk) :: tabres_child
      REAL(wp), DIMENSION(k1:k2) :: tabin, h_in
      REAL(wp), DIMENSION(1:jpk) :: h_out
      INTEGER :: N_in, N_out
      !!---------------------------------------------    
      !
      IF( before ) THEN
         DO jk=k1,k2-1
            DO jj=j1,j2
               DO ji=i1,i2
                  tabres(ji,jj,jk,m1) = e2u(ji,jj) * e3u(ji,jj,jk,Kbb_a) * uu(ji,jj,jk,Kbb_a) * umask(ji,jj,jk)
               END DO
            END DO
         END DO

      ELSE
         zrhoy = Agrif_rhoy()

         IF ( l_vremap ) THEN

            IF ( ln_linssh ) THEN
               zsshu(i1:i2,j1:j2) = 0._wp  
            ELSE
               zsshu(i1:i2,j1:j2) = hu(i1:i2,j1:j2,Kbb_a) - hu_0(i1:i2,j1:j2)   
            ENDIF

            DO jj=j1,j2
               DO ji=i1,i2
                  tabres_child(ji,jj,:) = 0._wp
                  N_in  = mbku_parent(ji,jj)
                  N_out = mbku(ji,jj)
                  IF (N_in * N_out > 0) THEN
                     DO jk=1,N_in
                        h_in(jk)  = e3u0_parent(ji,jj,jk) * & 
                             &       (1._wp + zsshu(ji,jj)/(hu0_parent(ji,jj)*ssumask(ji,jj) + 1._wp - ssumask(ji,jj)))
                        tabin(jk) = tabres(ji,jj,jk,1) / (e2u(ji,jj)*zrhoy*h_in(jk))
                     END DO
                     !         
                     DO jk=1,N_out
                        h_out(jk) = e3u(ji,jj,jk,Kbb_a)
                     END DO
                  
                     CALL reconstructandremap(tabin(1:N_in),h_in(1:N_in),tabres_child(ji,jj,1:N_out),h_out(1:N_out),N_in,N_out,1)
                  ENDIF 
               END DO
            END DO
         ELSE
            DO jk=1,jpkm1
               tabres_child(i1:i2,j1:j2,jk) = tabres(i1:i2,j1:j2,jk,1)/(e2u(i1:i2,j1:j2)*zrhoy*e3u(i1:i2,j1:j2,jk,Kbb_a))
            END DO
         ENDIF
         !
         ubdiff(i1:i2,j1:j2,1:jpkm1) = (uu(i1:i2,j1:j2,1:jpkm1,Kbb_a) - tabres_child(i1:i2,j1:j2,1:jpkm1))*umask(i1:i2,j1:j2,1:jpkm1)
         !
         DO jk = 1, jpkm1                                 ! Horizontal slab
            !                                             ! ===============

            !                                             ! --------
            ! Horizontal divergence                       !   div
            !                                             ! --------
            DO jj = j1,j2
               DO ji = i1+1,i2   ! vector opt.
                  zbtr = rn_sponge_dyn * r1_Dt * fspt(ji,jj) / e3t(ji,jj,jk,Kbb_a)
                  hdivdiff(ji,jj,jk) = (  e2u(ji  ,jj)*e3u(ji  ,jj,jk,Kbb_a) * ubdiff(ji  ,jj,jk) &
                                     &   -e2u(ji-1,jj)*e3u(ji-1,jj,jk,Kbb_a) * ubdiff(ji-1,jj,jk) ) * zbtr
               END DO
            END DO

            DO jj = j1,j2-1
               DO ji = i1,i2   ! vector opt.
                  zbtr = rn_sponge_dyn * r1_Dt * fspf(ji,jj) * e3f(ji,jj,jk) 
                  rotdiff(ji,jj,jk) = ( -e1u(ji,jj+1) * ubdiff(ji,jj+1,jk)   &
                                    &   +e1u(ji,jj  ) * ubdiff(ji,jj  ,jk) ) * fmask(ji,jj,jk) * zbtr 
               END DO
            END DO
         END DO
         !
         DO jj = j1+1, j2-1
            DO ji = i1+1, i2-1   ! vector opt.

               IF (.NOT. tabspongedone_u(ji,jj)) THEN
                  DO jk = 1, jpkm1                                 ! Horizontal slab
                     ze2u = rotdiff (ji,jj,jk)
                     ze1v = hdivdiff(ji,jj,jk)
                     ! horizontal diffusive trends
                     zua = - ( ze2u - rotdiff (ji,jj-1,jk) ) / ( e2u(ji,jj) * e3u(ji,jj,jk,Kmm_a) )   &
                         & + ( hdivdiff(ji+1,jj,jk) - ze1v ) * r1_e1u(ji,jj) & 
                         & - rn_trelax_dyn * r1_Dt * fspu(ji,jj) * ubdiff(ji,jj,jk)

                     ! add it to the general momentum trends
                     uu(ji,jj,jk,Krhs_a) = uu(ji,jj,jk,Krhs_a) + zua                                 
                  END DO
               ENDIF

            END DO
         END DO

         tabspongedone_u(i1+1:i2-1,j1+1:j2-1) = .TRUE.

         jmax = j2-1
         ind1 = jpjglo - ( nn_hls + nbghostcells + 1 )   ! North
         DO jj = mj0(ind1), mj1(ind1)                 
            jmax = MIN(jmax,jj)
         END DO

         DO jj = j1+1, jmax
            DO ji = i1+1, i2   ! vector opt.

               IF (.NOT. tabspongedone_v(ji,jj)) THEN
                  DO jk = 1, jpkm1                                 ! Horizontal slab
                     ze2u = rotdiff (ji,jj,jk)
                     ze1v = hdivdiff(ji,jj,jk)

                     ! horizontal diffusive trends
                     zva = + ( ze2u - rotdiff (ji-1,jj,jk) ) / ( e1v(ji,jj) * e3v(ji,jj,jk,Kmm_a) )   &
                           + ( hdivdiff(ji,jj+1,jk) - ze1v ) * r1_e2v(ji,jj)

                     ! add it to the general momentum trends
                     vv(ji,jj,jk,Krhs_a) = vv(ji,jj,jk,Krhs_a) + zva
                  END DO
               ENDIF
               !
            END DO
         END DO
         !
         tabspongedone_v(i1+1:i2,j1+1:jmax) = .TRUE.
         !
      ENDIF
      !
   END SUBROUTINE interpun_sponge

   
   SUBROUTINE interpvn_sponge(tabres,i1,i2,j1,j2,k1,k2,m1,m2, before)
      !!---------------------------------------------
      !!   *** ROUTINE interpvn_sponge ***
      !!--------------------------------------------- 
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2,m1,m2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,m1:m2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before
      !
      INTEGER  ::   ji, jj, jk, imax
      INTEGER  :: ind1
      REAL(wp) ::   ze2u, ze1v, zua, zva, zbtr, zrhox
      REAL(wp), DIMENSION(i1:i2,j1:j2) :: zsshv
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk) :: vbdiff
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk) :: rotdiff, hdivdiff
      ! vertical interpolation:
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk) :: tabres_child
      REAL(wp), DIMENSION(k1:k2) :: tabin, h_in
      REAL(wp), DIMENSION(1:jpk) :: h_out
      INTEGER :: N_in, N_out
      !!--------------------------------------------- 
      
      IF( before ) THEN 
         DO jk=k1,k2-1
            DO jj=j1,j2
               DO ji=i1,i2
                  tabres(ji,jj,jk,m1) = e1v(ji,jj) * e3v(ji,jj,jk,Kbb_a) * vv(ji,jj,jk,Kbb_a) * vmask(ji,jj,jk)
               END DO
            END DO
         END DO

      ELSE
         zrhox = Agrif_rhox()

         IF ( l_vremap ) THEN
            
            IF ( ln_linssh ) THEN
               zsshv(i1:i2,j1:j2) = 0._wp  
            ELSE
               zsshv(i1:i2,j1:j2) = hv(i1:i2,j1:j2,Kbb_a) - hv_0(i1:i2,j1:j2)   
            ENDIF 

            DO jj=j1,j2
               DO ji=i1,i2
                  tabres_child(ji,jj,:) = 0._wp
                  N_in = mbkv_parent(ji,jj)
                  N_out = mbkv(ji,jj)
                  IF (N_in * N_out > 0) THEN
                     DO jk=1,N_in
                        h_in(jk)  = e3v0_parent(ji,jj,jk) * & 
                             &       (1._wp + zsshv(ji,jj)/(hv0_parent(ji,jj)*ssvmask(ji,jj) + 1._wp - ssvmask(ji,jj)))
                        tabin(jk) = tabres(ji,jj,jk,1) / (e1v(ji,jj)*zrhox*h_in(jk))
                     END DO
                     !         
                     DO jk=1,N_out
                        h_out(jk) = e3v(ji,jj,jk,Kbb_a)
                     END DO

                     CALL reconstructandremap(tabin(1:N_in),h_in(1:N_in),tabres_child(ji,jj,1:N_out),h_out(1:N_out),N_in,N_out,1)

                  ENDIF
               END DO
            END DO
         ELSE
            DO jk=1,jpkm1
               tabres_child(i1:i2,j1:j2,jk) = tabres(i1:i2,j1:j2,jk,1)/(e1v(i1:i2,j1:j2)*zrhox*e3v(i1:i2,j1:j2,jk,Kbb_a))
            END DO
         ENDIF
         !
         vbdiff(i1:i2,j1:j2,1:jpkm1) = (vv(i1:i2,j1:j2,1:jpkm1,Kbb_a) - tabres_child(i1:i2,j1:j2,1:jpkm1))*vmask(i1:i2,j1:j2,1:jpkm1)
         !
         DO jk = 1, jpkm1                                 ! Horizontal slab
            !                                             ! ===============

            !                                             ! --------
            ! Horizontal divergence                       !   div
            !                                             ! --------
            DO jj = j1+1,j2
               DO ji = i1,i2   ! vector opt.
                  zbtr = rn_sponge_dyn * r1_Dt * fspt(ji,jj) / e3t(ji,jj,jk,Kbb_a)
                  hdivdiff(ji,jj,jk) = ( e1v(ji,jj  ) * e3v(ji,jj  ,jk,Kbb_a) * vbdiff(ji,jj  ,jk)  &
                                     &  -e1v(ji,jj-1) * e3v(ji,jj-1,jk,Kbb_a) * vbdiff(ji,jj-1,jk)  ) * zbtr
               END DO
            END DO
            DO jj = j1,j2
               DO ji = i1,i2-1   ! vector opt.
                  zbtr = rn_sponge_dyn * r1_Dt * fspf(ji,jj) * e3f(ji,jj,jk) 
                  rotdiff(ji,jj,jk) = ( e2v(ji+1,jj) * vbdiff(ji+1,jj,jk) & 
                                    &  -e2v(ji  ,jj) * vbdiff(ji  ,jj,jk)  ) * fmask(ji,jj,jk) * zbtr
               END DO
            END DO
         END DO

         !                                                ! ===============
         !                                                

         imax = i2 - 1
         ind1 = jpiglo - ( nn_hls + nbghostcells + 1 )   ! East
         DO ji = mi0(ind1), mi1(ind1)                
            imax = MIN(imax,ji)
         END DO
         
         DO jj = j1+1, j2
            DO ji = i1+1, imax   ! vector opt.
               IF( .NOT. tabspongedone_u(ji,jj) ) THEN
                  DO jk = 1, jpkm1
                     uu(ji,jj,jk,Krhs_a) = uu(ji,jj,jk,Krhs_a)                                                     &
                        & - ( rotdiff (ji  ,jj,jk) - rotdiff (ji,jj-1,jk)) / ( e2u(ji,jj) * e3u(ji,jj,jk,Kmm_a) )  &
                        & + ( hdivdiff(ji+1,jj,jk) - hdivdiff(ji,jj  ,jk)) * r1_e1u(ji,jj)
                  END DO
               ENDIF
            END DO
         END DO
         !
         tabspongedone_u(i1+1:imax,j1+1:j2) = .TRUE.
         !
         DO jj = j1+1, j2-1
            DO ji = i1+1, i2-1   ! vector opt.
               IF( .NOT. tabspongedone_v(ji,jj) ) THEN
                  DO jk = 1, jpkm1
                     vv(ji,jj,jk,Krhs_a) = vv(ji,jj,jk,Krhs_a)                                                        &
                        &  + ( rotdiff (ji,jj  ,jk) - rotdiff (ji-1,jj,jk) ) / ( e1v(ji,jj) * e3v(ji,jj,jk,Kmm_a) )   &
                        &  + ( hdivdiff(ji,jj+1,jk) - hdivdiff(ji  ,jj,jk) ) * r1_e2v(ji,jj)                          &
                        &  - rn_trelax_dyn * r1_Dt * fspv(ji,jj) * vbdiff(ji,jj,jk)
                  END DO
               ENDIF
            END DO
         END DO
         tabspongedone_v(i1+1:i2-1,j1+1:j2-1) = .TRUE.
      ENDIF
      !
   END SUBROUTINE interpvn_sponge

   SUBROUTINE interpunb_sponge(tabres,i1,i2,j1,j2, before)
      !!---------------------------------------------
      !!   *** ROUTINE interpunb_sponge ***
      !!---------------------------------------------    
      INTEGER, INTENT(in) :: i1,i2,j1,j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before

      INTEGER  :: ji, jj, ind1, jmax
      ! sponge parameters 
      REAL(wp) :: ze2u, ze1v, zua, zva, zbtr
      REAL(wp), DIMENSION(i1:i2,j1:j2) :: ubdiff
      REAL(wp), DIMENSION(i1:i2,j1:j2) :: rotdiff, hdivdiff
      !!---------------------------------------------    
      !
      IF( before ) THEN
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj) = uu_b(ji,jj,Kmm_a)
            END DO
         END DO

      ELSE

         ubdiff(i1:i2,j1:j2) = (uu_b(i1:i2,j1:j2,Kmm_a) - tabres(i1:i2,j1:j2))*umask(i1:i2,j1:j2,1)
         !
         !                                             ! --------
         ! Horizontal divergence                       !   div
         !                                             ! --------
         DO jj = j1,j2
            DO ji = i1+1,i2   ! vector opt.
               zbtr = rn_sponge_dyn * r1_Dt * fspt_2d(ji,jj) * r1_ht_0(ji,jj)
               hdivdiff(ji,jj) = (  e2u(ji  ,jj)*hu(ji  ,jj,Kbb_a) * ubdiff(ji  ,jj) &
                                  &-e2u(ji-1,jj)*hu(ji-1,jj,Kbb_a) * ubdiff(ji-1,jj) ) * zbtr
            END DO
         END DO

         DO jj = j1,j2-1
            DO ji = i1,i2   ! vector opt.
               zbtr = rn_sponge_dyn * r1_Dt * fspf_2d(ji,jj) * hf_0(ji,jj)
               rotdiff(ji,jj) = ( -e1u(ji,jj+1) * ubdiff(ji,jj+1)   &
                              &   +e1u(ji,jj  ) * ubdiff(ji,jj  ) ) * fmask(ji,jj,1) * zbtr 
            END DO
         END DO
         !
         DO jj = j1+1, j2-1
            DO ji = i1+1, i2-1   ! vector opt.
               IF (.NOT. tabspongedone_u(ji,jj)) THEN
                  ze2u = rotdiff (ji,jj)
                  ze1v = hdivdiff(ji,jj)
                  ! horizontal diffusive trends
                  zua = - ( ze2u - rotdiff (ji,jj-1) ) * r1_e2u(ji,jj) * r1_hu(ji,jj,Kmm_a)  &
                      & + ( hdivdiff(ji+1,jj) - ze1v ) * r1_e1u(ji,jj)                       & 
                      & - rn_trelax_dyn * r1_Dt * fspu_2d(ji,jj) * ubdiff(ji,jj)

                  ! add it to the general momentum trends
                  uu(ji,jj,:,Krhs_a) = uu(ji,jj,:,Krhs_a) + zua                                 
               ENDIF
            END DO
         END DO

         tabspongedone_u(i1+1:i2-1,j1+1:j2-1) = .TRUE.

         jmax = j2-1
         ind1 = jpjglo - ( nn_hls + nbghostcells + 1 )   ! North
         DO jj = mj0(ind1), mj1(ind1)                 
            jmax = MIN(jmax,jj)
         END DO

         DO jj = j1+1, jmax
            DO ji = i1+1, i2   ! vector opt.
               IF (.NOT. tabspongedone_v(ji,jj)) THEN
                     ze2u = rotdiff (ji,jj)
                     ze1v = hdivdiff(ji,jj)
                     zva = + ( ze2u - rotdiff (ji-1,jj) ) * r1_e1v(ji,jj) * r1_hv(ji,jj,Kmm_a) &
                           + ( hdivdiff(ji,jj+1) - ze1v ) * r1_e2v(ji,jj)
                     vv(ji,jj,:,Krhs_a) = vv(ji,jj,:,Krhs_a) + zva
               ENDIF
            END DO
         END DO
         !
         tabspongedone_v(i1+1:i2,j1+1:jmax) = .TRUE.
         !
      ENDIF
      !
   END SUBROUTINE interpunb_sponge

   
   SUBROUTINE interpvnb_sponge(tabres,i1,i2,j1,j2, before)
      !!---------------------------------------------
      !!   *** ROUTINE interpvnb_sponge ***
      !!--------------------------------------------- 
      INTEGER, INTENT(in) :: i1,i2,j1,j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before
      !
      INTEGER  ::   ji, jj, ind1, imax
      REAL(wp) ::   ze2u, ze1v, zua, zva, zbtr
      REAL(wp), DIMENSION(i1:i2,j1:j2) :: vbdiff
      REAL(wp), DIMENSION(i1:i2,j1:j2) :: rotdiff, hdivdiff
      !!--------------------------------------------- 
      
      IF( before ) THEN 
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj) = vv_b(ji,jj,Kmm_a)
            END DO
         END DO
      ELSE
         vbdiff(i1:i2,j1:j2) = (vv_b(i1:i2,j1:j2,Kmm_a) - tabres(i1:i2,j1:j2))*vmask(i1:i2,j1:j2,1)
         !                                             ! --------
         ! Horizontal divergence                       !   div
         !                                             ! --------
         DO jj = j1+1,j2
            DO ji = i1,i2   ! vector opt.
               zbtr = rn_sponge_dyn * r1_Dt * fspt_2d(ji,jj) * r1_ht_0(ji,jj)
               hdivdiff(ji,jj) = ( e1v(ji,jj  ) * hv(ji,jj  ,Kbb_a) * vbdiff(ji,jj  )  &
                               &  -e1v(ji,jj-1) * hv(ji,jj-1,Kbb_a) * vbdiff(ji,jj-1)  ) * zbtr
            END DO
         END DO
         DO jj = j1,j2
            DO ji = i1,i2-1   ! vector opt.
               zbtr = rn_sponge_dyn * r1_Dt * fspf_2d(ji,jj) * hf_0(ji,jj) 
               rotdiff(ji,jj) = ( e2v(ji+1,jj) * vbdiff(ji+1,jj) & 
                              &  -e2v(ji  ,jj) * vbdiff(ji  ,jj)  ) * fmask(ji,jj,1) * zbtr
            END DO
         END DO
         !                                                ! ===============
         !                                                

         imax = i2 - 1
         ind1 = jpiglo - ( nn_hls + nbghostcells + 1 )   ! East
         DO ji = mi0(ind1), mi1(ind1)                
            imax = MIN(imax,ji)
         END DO
         
         DO jj = j1+1, j2
            DO ji = i1+1, imax   ! vector opt.
               IF( .NOT. tabspongedone_u(ji,jj) ) THEN                                                     
                  zua = - ( rotdiff (ji  ,jj) - rotdiff (ji,jj-1)) * r1_e2u(ji,jj) * r1_hu(ji,jj,Kmm_a)  &
                      & + ( hdivdiff(ji+1,jj) - hdivdiff(ji,jj  )) * r1_e1u(ji,jj)
                  uu(ji,jj,:,Krhs_a) = uu(ji,jj,:,Krhs_a) + zua
               ENDIF
            END DO
         END DO
         !
         tabspongedone_u(i1+1:imax,j1+1:j2) = .TRUE.
         !
         DO jj = j1+1, j2-1
            DO ji = i1+1, i2-1   ! vector opt.
               IF( .NOT. tabspongedone_v(ji,jj) ) THEN
                  zva  =  ( rotdiff (ji,jj  ) - rotdiff (ji-1,jj) ) * r1_e1v(ji,jj) *r1_hv(ji,jj,Kmm_a) &
                     &  + ( hdivdiff(ji,jj+1) - hdivdiff(ji  ,jj) ) * r1_e2v(ji,jj)                     &
                     &  - rn_trelax_dyn * r1_Dt * fspv_2d(ji,jj) * vbdiff(ji,jj)
                  vv(ji,jj,:,Krhs_a) = vv(ji,jj,:,Krhs_a) + zva
               ENDIF
            END DO
         END DO
         tabspongedone_v(i1+1:i2-1,j1+1:j2-1) = .TRUE.
      ENDIF
      !
   END SUBROUTINE interpvnb_sponge


#else
   !!----------------------------------------------------------------------
   !!   Empty module                                          no AGRIF zoom
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE agrif_oce_sponge_empty
      WRITE(*,*)  'agrif_oce_sponge : You should not have seen this print! error?'
   END SUBROUTINE agrif_oce_sponge_empty
#endif

   !!======================================================================
END MODULE agrif_oce_sponge
