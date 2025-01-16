MODULE agrif_dom_update

   USE dom_oce
   USE agrif_parameters
   USE agrif_profiles
   USE agrif_recompute_scales
   USE lbclnk
 
   IMPLICIT none
   PRIVATE

   REAL(wp), PARAMETER :: rminfrac = 0.98_wp ! Should be < 1
   LOGICAL             :: l_match_area=.FALSE.   

   PUBLIC agrif_update_all

CONTAINS 

#if defined key_agrif

   SUBROUTINE agrif_update_all
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE agrif_update_all  ***
      !!----------------------------------------------------------------------  
      !

      IF( Agrif_Root() ) return
      !
      ! Update e1t and e2t (set grid cell area as over child grid):
      CALL agrif_update_variable(e1e2t_upd_id, procname = update_e1e2t)
      
      ! Update e2u and e1v at cell faces:
      CALL agrif_update_variable(e2u_id,       procname = update_e2u)
      CALL agrif_update_variable(e1v_id,       procname = update_e1v)

      ! Then compute fractional area over child grid:
      ALLOCATE(e1e2t_frac(jpi,jpj), e2u_frac(jpi,jpj), e1v_frac(jpi,jpj))
      CALL Agrif_Init_variable(e1e2t_frac_id,  procname = interp_e1e2t_frac)
      ! And fractional size of cell faces:
      CALL Agrif_Init_variable(e2u_frac_id,    procname = interp_e2u_frac)
      CALL Agrif_Init_variable(e1v_frac_id,    procname = interp_e1v_frac)
      !
      ! Update scale factors:
      !
      Agrif_UseSpecialValueInUpdate = .FALSE.
      IF ( .NOT.ln_vert_remap ) THEN
         ! Get max bottom level over coarse grid (mbkt):
         CALL agrif_update_variable(mbkt_id,   procname = update_mbkt)
         ! Set e3t over parent as averages of child grid cells:        
         CALL agrif_update_variable(e3t_id,    procname = update_e3t_z) 
         ! Set e3w over parent as the center of child cells volumes: 
         CALL agrif_update_variable(e3w_id,    procname = update_e3w_z)
         !
         ! Update surface mask at U/V points in case it has been updated above:
         CALL Agrif_ChildGrid_To_ParentGrid()
         CALL update_surf_masks 
         CALL Agrif_ParentGrid_To_ChildGrid() 
         !
         ! Set mbku as maximum over U-faces:
         CALL agrif_update_variable(mbku_id,   procname = update_mbku)
         ! Set e3u/e3uw as U-faces averages:
         CALL agrif_update_variable(e3u_id,    procname = update_e3u_z)
         ! Set e3uw over parent as the center of child cells volumes: 
         CALL agrif_update_variable(e3uw_id,   procname = update_e3uw_z)
         ! Set mbkv as maximum over V-faces:
         CALL agrif_update_variable(mbkv_id,   procname = update_mbkv)
         ! Set e3v/e3vw as v-faces averages:
         CALL agrif_update_variable(e3v_id,    procname = update_e3v_z)
         ! Set e3vw over parent as the center of child cells volumes: 
         CALL agrif_update_variable(e3vw_id,   procname = update_e3vw_z)
         ! Copy mbkf value at F-points:
         CALL agrif_update_variable(mbkf_id,   procname = update_mbkf)
         ! Copy e3f at faces corners:
         CALL agrif_update_variable(e3f_id,    procname = update_e3f_z)
         !
      ELSE
         ! Reconstruct e3t/e3w over parent grid such that total volume is conserved:       
         CALL agrif_update_variable(e3t_id,    procname = update_e3tw_z_gen) 
         !
         ! Update vertical scale factors at U, V and F-points:
         CALL Agrif_ChildGrid_To_ParentGrid()
         CALL update_surf_masks
         CALL agrif_recompute_scalefactors
         CALL Agrif_ParentGrid_To_ChildGrid()
      ENDIF
      DEALLOCATE(e1e2t_frac, e2u_frac, e1v_frac)
      !    
   END SUBROUTINE agrif_update_all


   SUBROUTINE update_surf_masks
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE update_surf_masks  ***
      !!
      !! Update surface mask at U/V points from mask at T-points
      !!----------------------------------------------------------------------  
      INTEGER :: ji, jj
      !!----------------------------------------------------------------------
      DO jj = 1, jpjm1
         DO ji = 1, jpim1
            ssumask(ji,jj) = ssmask(ji  ,jj  ) * ssmask(ji+1,jj  )
            ssvmask(ji,jj) = ssmask(ji  ,jj  ) * ssmask(ji  ,jj+1)
            ssfmask(ji,jj) = ssmask(ji  ,jj  ) * ssmask(ji+1,jj  ) &
                           &*ssmask(ji+1,jj+1) * ssmask(ji  ,jj+1) 
         END DO
      END DO
      !
      CALL lbc_lnk_multi( 'update_surf_masks', ssumask, 'U', 1., ssvmask, 'V', 1., ssfmask,'F', 1.)
      !
   END SUBROUTINE update_surf_masks


   SUBROUTINE update_mbkt( ptab, i1, i2, j1, j2, before)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE update_mbkt  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL, DIMENSION(i1:i2,j1:j2)    , INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      !
      !!---------------------------------------------------------------------- 
      !
      IF( before) THEN
         ptab(i1:i2,j1:j2) = mbkt(i1:i2,j1:j2) * ssmask(i1:i2,j1:j2)
      ELSE
         mbkt(i1:i2,j1:j2) = nint(ptab(i1:i2,j1:j2))
         
         WHERE ( mbkt(i1:i2,j1:j2) .EQ. 0 )
            ssmask(i1:i2,j1:j2) = 0._wp
            mbkt(i1:i2,j1:j2)   = 1
         ELSEWHERE
            ssmask(i1:i2,j1:j2) = 1._wp
         END WHERE 
      ENDIF
      !
   END SUBROUTINE update_mbkt


   SUBROUTINE update_mbku( ptab, i1, i2, j1, j2, before)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE update_mbku  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL, DIMENSION(i1:i2,j1:j2)    , INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      !
      !!---------------------------------------------------------------------- 
      !
      IF( before) THEN
         ptab(i1:i2,j1:j2) = mbku(i1:i2,j1:j2) * ssumask(i1:i2,j1:j2)
      ELSE
         mbku(i1:i2,j1:j2) = nint(ptab(i1:i2,j1:j2))
         
         WHERE ( mbku(i1:i2,j1:j2) .EQ. 0 )
            mbku(i1:i2,j1:j2)    = 1
         END WHERE 
      ENDIF
      !
   END SUBROUTINE update_mbku


   SUBROUTINE update_mbkv( ptab, i1, i2, j1, j2, before)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE update_mbkv  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL, DIMENSION(i1:i2,j1:j2)    , INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      !
      !!---------------------------------------------------------------------- 
      !
      IF( before) THEN
         ptab(i1:i2,j1:j2) = mbkv(i1:i2,j1:j2) * ssvmask(i1:i2,j1:j2)
      ELSE
         mbkv(i1:i2,j1:j2) = nint(ptab(i1:i2,j1:j2))
         
         WHERE ( mbkv(i1:i2,j1:j2) .EQ. 0 )
            mbkv(i1:i2,j1:j2)    = 1
         END WHERE 
      ENDIF
      !
   END SUBROUTINE update_mbkv


   SUBROUTINE update_mbkf( ptab, i1, i2, j1, j2, before)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE update_mbkf  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL, DIMENSION(i1:i2,j1:j2)    , INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      !
      !!---------------------------------------------------------------------- 
      !
      IF( before) THEN
         ptab(i1:i2,j1:j2) = mbkf(i1:i2,j1:j2) * ssfmask(i1:i2,j1:j2)
      ELSE
         mbkf(i1:i2,j1:j2) = nint(ptab(i1:i2,j1:j2))
         
         WHERE ( mbkf(i1:i2,j1:j2) .EQ. 0 )
            mbkf(i1:i2,j1:j2)    = 1
         END WHERE 
      ENDIF
      !
   END SUBROUTINE update_mbkf


   SUBROUTINE update_e3t_z( tabres, i1, i2, j1, j2, k1, k2, before )
      !!---------------------------------------------
      !!           *** update_e3t_z ***
      !!---------------------------------------------
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before
      !!
      INTEGER :: ji, jj, jk, ik
      REAL(wp) :: zimin
      !!---------------------------------------------
      !
      IF (before) THEN
         DO jk=k1,k2
            DO jj=j1,j2
               DO ji=i1,i2
                  IF ( (ssmask(ji,jj) /=0._wp).AND.(mbkt(ji,jj).GE.jk) ) THEN
                     tabres(ji,jj,jk) = e1e2t_frac(ji,jj) * e3t_0(ji,jj,jk) 
                  ELSE
                     tabres(ji,jj,jk) = 0._wp
                  ENDIF 
               END DO
            END DO
         END DO
         tabres(i1:i2,j1:j2,k2) = e1e2t_frac(i1:i2,j1:j2) * ssmask(i1:i2,j1:j2)  ! To get fractional area
      ELSE
         !
         DO jk=1,jpkm1
            DO jj=j1,j2
               DO ji=i1,i2
                  IF ( (ssmask(ji,jj)==1._wp).AND.(mbkt(ji,jj).GE.jk) ) THEN
                     e3t_0(ji,jj,jk) = tabres(ji,jj,jk)
                  ELSE
                     e3t_0(ji,jj,jk) = e3t_1d(jk)
                  ENDIF
               END DO
            END DO
         END DO
         !
         ! Change surface mask below in case of too shallow
         ! depth on parent grid  
         !---------------------------------------------------
         ! Update bathymetry:
         DO jj=j1,j2
            DO ji=i1,i2
               bathy(ji,jj) = SUM(e3t_0(ji,jj,1:mbkt(ji,jj) ) ) 
            END DO
         END DO
         !
         ! Mask points:

         
         IF ( l_match_area) THEN ; zimin = 0._wp ; ELSE ; zimin = rminfrac ; ENDIF  

         WHERE ( ( mbkt(i1:i2,j1:j2) .EQ. 1 )      & 
           & .OR.(tabres(i1:i2,j1:j2,k2)<=zimin)  )
            bathy(i1:i2,j1:j2)  = 0._wp
            ssmask(i1:i2,j1:j2) = 0._wp
            mbkt(i1:i2,j1:j2)   = 1 
         END WHERE
         !
         ! Reset thicknesses to the one from the reference grid over land:
         DO jj=j1,j2
            DO ji=i1,i2
               IF (mbkt(ji,jj)==1) THEN
                  DO jk=1,jpk
                     e3t_0(ji,jj,jk) = e3t_1d(jk)
                  END DO
               ENDIF
            END DO
         END DO
      ENDIF
      ! 
   END SUBROUTINE update_e3t_z

   SUBROUTINE update_e3tw_z_gen( tabres, i1, i2, j1, j2, k1, k2, before )
      !!---------------------------------------------
      !!           *** update_e3tw_z_gen ***
      !!---------------------------------------------
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before
      !!
      INTEGER :: ji, jj, jk, ik
      REAL(wp) :: ze3min, zdepth, zdepwp, ze3tp, ze3wp, zimin
      !!---------------------------------------------
      !
      IF (before) THEN
         DO jk = k1, k2-1
            DO jj = j1, j2
               DO ji = i1, i2
                   IF ( (ssmask(ji,jj) /=0._wp).AND.( mbkt(ji,jj) .GE. jk ) ) THEN
                      tabres(ji,jj,jk) = e1e2t_frac(ji,jj) * e3t_0(ji,jj,jk)
                   ELSE
                      tabres(ji,jj,jk) = 0._wp
                   endif
               END DO
            END DO
         END DO
         tabres(i1:i2,j1:j2,k2) = e1e2t_frac(i1:i2,j1:j2) * ssmask(i1:i2,j1:j2)  ! To get fractional area
      ELSE
         !
         ! Compute child bathymetry:
         bathy(i1:i2,j1:j2) = 0._wp
         DO jk=k1,k2-1   
            bathy(i1:i2,j1:j2) = bathy(i1:i2,j1:j2) + tabres(i1:i2,j1:j2,jk)
         END DO
         WHERE( bathy(i1:i2,j1:j2) == 0._wp )   ;   mbathy(i1:i2,j1:j2) = 0       
         ELSE WHERE                             ;   mbathy(i1:i2,j1:j2) = jpkm1  
         END WHERE

         DO jk = jpkm1, 1, -1
            zdepth = gdepw_1d(jk)  + 1.e-6  ! + MIN( e3zps_min, e3t_1d(jk)*e3zps_rat )
            WHERE( 0._wp < bathy(i1:i2,j1:j2) .AND. bathy(i1:i2,j1:j2) <= zdepth )   mbathy(i1:i2,j1:j2) = jk-1
         END DO

         ! Scale factors and depth at T- and W-points
         DO jk = 1, jpk  
            gdept_0(i1:i2,j1:j2,jk) = gdept_1d(jk)
            gdepw_0(i1:i2,j1:j2,jk) = gdepw_1d(jk)
            e3t_0  (i1:i2,j1:j2,jk) = e3t_1d  (jk)
            e3w_0  (i1:i2,j1:j2,jk) = e3w_1d  (jk)
         END DO
         ! Scale factors and depth at T- and W-points
         DO jj = j1, j2
            DO ji = i1, i2 
               ik = mbathy(ji,jj)
               IF( ik > 0 ) THEN               ! ocean point only
                  ! max ocean level case
                  IF( ik == jpkm1 ) THEN
                     zdepwp = bathy(ji,jj)
                     ze3tp  = bathy(ji,jj) - gdepw_1d(ik)
                     ze3wp = 0.5_wp * e3w_1d(ik) * ( 1._wp + ( ze3tp/e3t_1d(ik) ) )
                     e3t_0(ji,jj,ik  ) = ze3tp
                     e3w_0(ji,jj,ik  ) = ze3wp
                     IF ( ln_e3_dep.AND.ln_dept_mid ) THEN
                        gdept_0(ji,jj,ik) = gdepw_1d(ik) + 0.5_wp * ze3tp
                        e3w_0(ji,jj,ik  ) = gdept_0(ji,jj,ik) - gdept_0(ji,jj,ik-1)
                     ELSE
                        gdept_0(ji,jj,ik) = gdept_1d(ik-1) + ze3wp
                        e3w_0(ji,jj,ik  ) = ze3wp
                     ENDIF
                     gdept_0(ji,jj,ik+1) = gdept_0(ji,jj,ik) + ze3tp
                     e3w_0(ji,jj,ik+1) = ze3tp
                     gdepw_0(ji,jj,ik+1) = zdepwp
                     !
                  ELSE                         ! standard case
                     IF( bathy(ji,jj) <= gdepw_1d(ik+1) ) THEN 
                        gdepw_0(ji,jj,ik+1) = bathy(ji,jj)
                     ELSE                                       
                        gdepw_0(ji,jj,ik+1) = gdepw_1d(ik+1)
                     ENDIF
                     e3t_0  (ji,jj,ik) = e3t_1d  (ik) * ( gdepw_0 (ji,jj,ik+1) - gdepw_1d(ik))           &
                        &                             / ( gdepw_1d(      ik+1) - gdepw_1d(ik)) 
                     IF ( ln_e3_dep.AND.ln_dept_mid ) THEN
                        gdept_0(ji,jj,ik) = gdepw_1d(ik) + 0.5_wp * e3t_0(ji,jj,ik)
                        e3w_0(ji,jj,ik) = gdept_0(ji,jj,ik) - gdept_0(ji,jj,ik-1)
                     ELSE 
                        gdept_0(ji,jj,ik) = gdepw_1d(ik) + ( gdepw_0(ji,jj,ik+1) - gdepw_1d(ik) )           &
                           &                             * ((gdept_1d(     ik  ) - gdepw_1d(ik) )           &
                           &                             / ( gdepw_1d(     ik+1) - gdepw_1d(ik) ))
                        e3w_0(ji,jj,ik) =                                                                   & 
                           &      0.5_wp * (gdepw_0(ji,jj,ik+1) + gdepw_1d(ik+1) - 2._wp * gdepw_1d(ik) )   &
                           &             * ( e3w_1d(ik) / ( gdepw_1d(ik+1) - gdepw_1d(ik) ) )
                     ENDIF
                     !       ... on ik+1
                     e3w_0  (ji,jj,ik+1) = e3t_0  (ji,jj,ik)
                     e3t_0  (ji,jj,ik+1) = e3t_0  (ji,jj,ik)
                     gdept_0(ji,jj,ik+1) = gdept_0(ji,jj,ik) + e3t_0(ji,jj,ik)
                  ENDIF
               ENDIF
            END DO
         END DO
         !
         ! Expand last level if too thin:
!         DO jj=j1,j2
!            DO ji=i1,i2
!              ik = mbathy(ji,jj)
!              IF ( ik > 2 ) THEN
!                 ze3min = MIN( e3zps_min, e3t_1d(ik)*e3zps_rat )
!                 IF ( e3t_0(ji,jj,ik) < ze3min ) THEN
!                    e3t_0(ji,jj,ik-1) = e3t_0(ji,jj,ik-1) - (ze3min - e3t_0(ji,jj,ik))
!                    e3t_0(ji,jj,ik  ) = ze3min 
!                    gdept_0(ji,jj,ik-1) = gdepw_1d(ik-1) + 0.5_wp * e3t_0(ji,jj,ik-1) 
!                    gdept_0(ji,jj,ik  ) = gdept_0(ji,jj,ik-1) + 0.5_wp * (e3t_0(ji,jj,ik) + e3t_0(ji,jj,ik-1)) 
!                    e3w_0(ji,jj,ik-1)   = gdept_0(ji,jj,ik-1)-gdept_0(ji,jj,ik-2) 
!                    e3w_0(ji,jj,ik  )   = gdept_0(ji,jj,ik  )-gdept_0(ji,jj,ik-1) 
!                    e3w_0  (ji,jj,ik+1) = e3t_0(ji,jj,ik)
!                    e3t_0  (ji,jj,ik+1) = e3t_0(ji,jj,ik)
!                 ENDIF 
!              ENDIF  
!            END DO
!         END DO
         !
         mbkt(i1:i2,j1:j2) = MAX( mbathy(i1:i2,j1:j2), 1 )
         DO jj=j1,j2
            DO ji=i1,i2
               bathy(ji,jj) = SUM(e3t_0(ji,jj,1:mbkt(ji,jj) ) ) 
            END DO
         END DO
         !
         IF ( l_match_area) THEN ; zimin = 0._wp ; ELSE ; zimin = rminfrac ; ENDIF  

         WHERE ( ( mbkt(i1:i2,j1:j2) .EQ. 1 )      & 
           & .OR.(tabres(i1:i2,j1:j2,k2)<zimin)  ) 
            bathy(i1:i2,j1:j2)  = 0._wp
            ssmask(i1:i2,j1:j2) = 0._wp
            mbkt(i1:i2,j1:j2)   = 1 
         ELSEWHERE
            ssmask(i1:i2,j1:j2) = 1._wp
         END WHERE

         DO jj=j1,j2
            DO ji=i1,i2
               IF (mbkt(ji,jj)==1) THEN
                  DO jk=1,jpk
                     e3t_0(ji,jj,jk) = e3t_1d(jk)
                     e3w_0(ji,jj,jk) = e3w_1d(jk)
                     gdept_0(ji,jj,jk) = gdept_1d(jk)
                     gdepw_0(ji,jj,jk) = gdepw_1d(jk)
                  END DO
               ENDIF
            END DO
         END DO
      ENDIF
      ! 
   END SUBROUTINE update_e3tw_z_gen


   SUBROUTINE update_e3w_z( tabres, i1, i2, j1, j2, k1, k2, before )
      !!---------------------------------------------
      !!           *** update_e3w_z ***
      !!---------------------------------------------
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before
      !!
      INTEGER :: ji, jj, jk
      !!---------------------------------------------
      !
      IF (before) THEN
         DO jk=k1,k2
            DO jj=j1,j2
               DO ji=i1,i2
                  IF ( (ssmask(ji,jj)/=0._wp).AND.(mbkt(ji,jj).GE.jk) ) THEN
                     tabres(ji,jj,jk) = e1e2t_frac(ji,jj) * e3w_0(ji,jj,jk) * e3t_0(ji,jj,jk)
                  ELSE
                     tabres(ji,jj,jk) = 0._wp
                  ENDIF 
               END DO
            END DO
         END DO
      ELSE
         DO jj=j1,j2
            DO ji=i1,i2
               IF ( ssmask(ji,jj)==1._wp ) THEN  
                  e3w_0(ji,jj,1) = tabres(ji,jj,1) / e3t_0(ji,jj,1)
                  gdept_0(ji,jj,1) = 0.5_wp * e3w_0(ji,jj,1) 
               ELSE 
                  e3w_0(ji,jj,1) = e3w_1d(1)
                  gdept_0(ji,jj,1) = gdept_1d(1)
               ENDIF
               ! 
               DO jk=2,jpkm1
                  IF ( (ssmask(ji,jj)==1._wp).AND.(mbkt(ji,jj).GE.jk) ) THEN
                     gdept_0(ji,jj,jk) = gdept_1d(jk-1) + tabres(ji,jj,jk) / e3t_0(ji,jj,jk)
                  ELSE
                     gdept_0(ji,jj,jk) = gdept_1d(jk)
                  ENDIF
               END DO
               DO jk=2,jpk
                  IF ( (ssmask(ji,jj)==1._wp).AND.(mbkt(ji,jj).GE.jk) ) THEN
                     e3w_0(ji,jj,jk) = gdept_0(ji,jj,jk) - gdept_0(ji,jj,jk-1)
                  ELSE
                     e3w_0(ji,jj,jk) = e3w_1d(jk)
                  ENDIF
               END DO
            END DO
         END DO
         !
      ENDIF
      ! 
   END SUBROUTINE update_e3w_z


   SUBROUTINE update_e3uw_z( tabres, i1, i2, j1, j2, k1, k2, before )
      !!---------------------------------------------
      !!           *** update_e3uw_z ***
      !!---------------------------------------------
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk) :: gdepu
      LOGICAL, INTENT(in) :: before
      !!
      INTEGER :: ji, jj, jk
      !!---------------------------------------------
      !
      IF (before) THEN
         DO jk=k1,k2
            DO jj=j1,j2
               DO ji=i1,i2
                  IF ( (ssumask(ji,jj)/=0._wp).AND.(mbku(ji,jj).GE.jk) ) THEN
                     tabres(ji,jj,jk) = e2u_frac(ji,jj) * e3uw_0(ji,jj,jk) * e3u_0(ji,jj,jk)
                  ELSE
                     tabres(ji,jj,jk) = 0._wp
                  ENDIF 
               END DO
            END DO
         END DO
      ELSE
         DO jj=j1,j2
            DO ji=i1,i2
               IF ( ssumask(ji,jj)==1._wp ) THEN  
                  e3uw_0(ji,jj,1) = tabres(ji,jj,1) / e3u_0(ji,jj,1)
                  gdepu(ji,jj,1) = 0.5_wp * e3uw_0(ji,jj,1) 
               ELSE 
                  e3uw_0(ji,jj,1) = e3w_1d(1)
                  gdepu(ji,jj,1) = gdept_1d(1)
               ENDIF
               ! 
               DO jk=2,jpkm1
                  IF ( (ssumask(ji,jj)==1._wp).AND.(mbku(ji,jj).GE.jk) ) THEN
                     gdepu(ji,jj,jk) = gdept_1d(jk-1) + tabres(ji,jj,jk) / e3u_0(ji,jj,jk)
                  ELSE
                     gdepu(ji,jj,jk) = gdept_1d(jk)
                  ENDIF
               END DO
               DO jk=2,jpk
                  IF ( (ssumask(ji,jj)==1._wp).AND.(mbku(ji,jj).GE.jk) ) THEN
                     e3uw_0(ji,jj,jk) = gdepu(ji,jj,jk) - gdepu(ji,jj,jk-1)
                  ELSE
                     e3uw_0(ji,jj,jk) = e3w_1d(jk)
                  ENDIF
               END DO
            END DO
         END DO
         !
      ENDIF
      ! 
   END SUBROUTINE update_e3uw_z


   SUBROUTINE update_e3vw_z( tabres, i1, i2, j1, j2, k1, k2, before )
      !!---------------------------------------------
      !!           *** update_e3vw_z ***
      !!---------------------------------------------
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres
      REAL(wp), DIMENSION(i1:i2,j1:j2,1:jpk) :: gdepv
      LOGICAL, INTENT(in) :: before
      !!
      INTEGER :: ji, jj, jk
      !!---------------------------------------------
      !
      IF (before) THEN
         DO jk=k1,k2
            DO jj=j1,j2
               DO ji=i1,i2
                  IF ( (ssvmask(ji,jj)/=0._wp).AND.(mbkv(ji,jj).GE.jk) ) THEN
                     tabres(ji,jj,jk) = e1v_frac(ji,jj) * e3vw_0(ji,jj,jk) * e3v_0(ji,jj,jk)
                  ELSE
                     tabres(ji,jj,jk) = 0._wp
                  ENDIF 
               END DO
            END DO
         END DO
      ELSE
         DO jj=j1,j2
            DO ji=i1,i2
               IF ( ssvmask(ji,jj)==1._wp ) THEN  
                  e3vw_0(ji,jj,1) = tabres(ji,jj,1) / e3v_0(ji,jj,1)
                  gdepv(ji,jj,1) = 0.5_wp * e3vw_0(ji,jj,1) 
               ELSE 
                  e3vw_0(ji,jj,1) = e3w_1d(1)
                  gdepv(ji,jj,1) = gdept_1d(1)
               ENDIF
               ! 
               DO jk=2,jpkm1
                  IF ( (ssvmask(ji,jj)==1._wp).AND.(mbkv(ji,jj).GE.jk) ) THEN
                     gdepv(ji,jj,jk) = gdept_1d(jk-1) + tabres(ji,jj,jk) / e3v_0(ji,jj,jk)
                  ELSE
                     gdepv(ji,jj,jk) = gdept_1d(jk)
                  ENDIF
               END DO
               DO jk=2,jpk
                  IF ( (ssvmask(ji,jj)==1._wp).AND.(mbkv(ji,jj).GE.jk) ) THEN
                     e3vw_0(ji,jj,jk) = gdepv(ji,jj,jk) - gdepv(ji,jj,jk-1)
                  ELSE
                     e3vw_0(ji,jj,jk) = e3w_1d(jk)
                  ENDIF
               END DO
            END DO
         END DO
         !
      ENDIF
      ! 
   END SUBROUTINE update_e3vw_z


   SUBROUTINE update_e3u_z( tabres, i1, i2, j1, j2, k1, k2, before )
      !!---------------------------------------------
      !!           *** update_e3u_z ***
      !!---------------------------------------------
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before
      !!
      INTEGER :: ji, jj, jk
      !!---------------------------------------------
      !
      IF (before) THEN
         DO jk=k1,k2
            DO jj=j1,j2
               DO ji=i1,i2
                  IF ( (ssumask(ji,jj) /=0._wp).AND.(mbku(ji,jj).GE.jk) ) THEN
                     tabres(ji,jj,jk) = e2u_frac(ji,jj) * e3u_0(ji,jj,jk) 
                  ELSE
                     tabres(ji,jj,jk) = 0._wp
                  ENDIF 
               END DO
            END DO
         END DO
      ELSE
         !
         DO jk=1,jpkm1
            DO jj=j1,j2
               DO ji=i1,i2
                  IF ( (ssumask(ji,jj)==1._wp).AND.(mbku(ji,jj).GE.jk) ) THEN
                     e3u_0(ji,jj,jk) = tabres(ji,jj,jk)
                  ELSE
                     e3u_0(ji,jj,jk) = e3t_1d(jk)
                  ENDIF
               END DO
            END DO
         END DO
         !
      ENDIF
      ! 
   END SUBROUTINE update_e3u_z


   SUBROUTINE update_e3v_z( tabres, i1, i2, j1, j2, k1, k2, before )
      !!---------------------------------------------
      !!           *** update_e3v_z ***
      !!---------------------------------------------
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before
      !!
      INTEGER :: ji, jj, jk
      !!---------------------------------------------
      !
      IF (before) THEN
         DO jk=k1,k2
            DO jj=j1,j2
               DO ji=i1,i2
                  IF ( (ssvmask(ji,jj) /=0._wp).AND.(mbkv(ji,jj).GE.jk) ) THEN
                     tabres(ji,jj,jk) = e1v_frac(ji,jj) * e3v_0(ji,jj,jk) 
                  ELSE
                     tabres(ji,jj,jk) = 0._wp
                  ENDIF 
               END DO
            END DO
         END DO
      ELSE
         !
         DO jk=1,jpkm1
            DO jj=j1,j2
               DO ji=i1,i2
                  IF ( (ssvmask(ji,jj)==1._wp).AND.(mbkv(ji,jj).GE.jk) ) THEN
                     e3v_0(ji,jj,jk) = tabres(ji,jj,jk)
                  ELSE
                     e3v_0(ji,jj,jk) = e3t_1d(jk)
                  ENDIF
               END DO
            END DO
         END DO
         !
      ENDIF
      ! 
   END SUBROUTINE update_e3v_z


   SUBROUTINE update_e3f_z( tabres, i1, i2, j1, j2, k1, k2, before )
      !!---------------------------------------------
      !!           *** update_e3f_z ***
      !!---------------------------------------------
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before
      !!
      INTEGER :: ji, jj, jk
      !!---------------------------------------------
      !
      IF (before) THEN
         DO jk=k1,k2
            DO jj=j1,j2
               DO ji=i1,i2
                  IF ( (ssfmask(ji,jj) /=0._wp).AND.(mbkf(ji,jj).GE.jk) ) THEN
                     tabres(ji,jj,jk) = e3f_0(ji,jj,jk) 
                  ELSE
                     tabres(ji,jj,jk) = 0._wp
                  ENDIF 
               END DO
            END DO
         END DO
      ELSE
         !
         DO jk=1,jpkm1
            DO jj=j1,j2
               DO ji=i1,i2
                  IF ( (ssfmask(ji,jj)==1._wp).AND.(mbkf(ji,jj).GE.jk) ) THEN
                     e3f_0(ji,jj,jk) = tabres(ji,jj,jk)
                  ELSE
                     e3f_0(ji,jj,jk) = e3t_1d(jk)
                  ENDIF
               END DO
            END DO
         END DO
         !
      ENDIF
      ! 
   END SUBROUTINE update_e3f_z


   SUBROUTINE update_e1e2t(tabres, i1, i2, j1, j2, n1, n2, before )
      !
      !!----------------------------------------------------------------------
      !!                      *** ROUTINE update_e1e2t ***
      !!----------------------------------------------------------------------
      INTEGER                              , INTENT(in   ) :: i1, i2, j1, j2, n1, n2
      REAL(wp),DIMENSION(i1:i2,j1:j2,n1:n2), INTENT(inout) :: tabres
      LOGICAL                              , INTENT(in   ) :: before
      !!
      INTEGER :: ji ,jj, jk
      REAL(wp) :: ztemp
      !!----------------------------------------------------------------------

      IF (before) THEN
         DO jj=j1,j2
            DO ji=i1,i2
               IF ( l_match_area ) THEN
                  tabres(ji,jj,1) = e1t(ji,jj)*e2t(ji,jj)*ssmask(ji,jj)
                  tabres(ji,jj,2) = e1t(ji,jj)*ssmask(ji,jj)
                  tabres(ji,jj,3) = e2t(ji,jj)*ssmask(ji,jj)
               ELSE
                  tabres(ji,jj,1) = e1t(ji,jj)*e2t(ji,jj)
                  tabres(ji,jj,2) = e1t(ji,jj)
                  tabres(ji,jj,3) = e2t(ji,jj)
               ENDIF
            END DO
         END DO
         tabres(i1:i2,j1:j2,1) = tabres(i1:i2,j1:j2,1)*Agrif_Rhox()*Agrif_Rhoy()
         tabres(i1:i2,j1:j2,2) = tabres(i1:i2,j1:j2,2)*Agrif_Rhox()
         tabres(i1:i2,j1:j2,3) = tabres(i1:i2,j1:j2,3)*Agrif_Rhoy()
      ELSE
         DO jj=j1,j2
            DO ji=i1,i2
               IF (tabres(ji,jj,1)/=0._wp) THEN
                  ztemp = SQRT(tabres(ji,jj,1) & 
                        & /(tabres(ji,jj,2)*tabres(ji,jj,3)))
                  e1t(ji,jj)      = tabres(ji,jj,2)*ztemp
                  e2t(ji,jj)      = tabres(ji,jj,3)*ztemp
                  e1e2t(ji,jj)    = tabres(ji,jj,1)
                  r1_e1e2t(ji,jj) = 1._wp / tabres(ji,jj,1) 
                  r1_e1t(ji,jj)   = 1._wp / (tabres(ji,jj,2)*ztemp) 
                  r1_e2t(ji,jj)   = 1._wp / (tabres(ji,jj,3)*ztemp) 
               ENDIF
            END DO
         END DO
      ENDIF
      !
   END SUBROUTINE update_e1e2t


   SUBROUTINE update_e2u(tabres, i1, i2, j1, j2, before )
      !
      !!----------------------------------------------------------------------
      !!                      *** ROUTINE update_e2u ***
      !!----------------------------------------------------------------------
      INTEGER                        , INTENT(in   ) :: i1, i2, j1, j2
      REAL(wp),DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres
      LOGICAL                        , INTENT(in   ) :: before
      !!
      INTEGER :: ji ,jj
      !!----------------------------------------------------------------------

      IF (before) THEN
         IF ( l_match_area ) THEN
            tabres(i1:i2,j1:j2) = e2u(i1:i2,j1:j2) * ssumask(i1:i2,j1:j2) * Agrif_Rhoy()
         ELSE
            tabres(i1:i2,j1:j2) = e2u(i1:i2,j1:j2) * Agrif_Rhoy()
         ENDIF
      ELSE  
         DO ji=i1,i2
            DO jj=j1,j2
               IF (tabres(ji,jj)/=0._wp) THEN
                  e2u(ji,jj)    = tabres(ji,jj)
                  r1_e2u(ji,jj) = 1._wp / tabres(ji,jj)
               END IF
            END DO
         END DO
      ENDIF
      !
   END SUBROUTINE update_e2u


   SUBROUTINE update_e1v(tabres, i1, i2, j1, j2, before )
      !
      !!----------------------------------------------------------------------
      !!                      *** ROUTINE update_e1v ***
      !!----------------------------------------------------------------------
      INTEGER                        , INTENT(in   ) :: i1, i2, j1, j2
      REAL(wp),DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres
      LOGICAL                        , INTENT(in   ) :: before
      !!
      INTEGER :: ji ,jj
      !!----------------------------------------------------------------------

      IF (before) THEN
         IF ( l_match_area ) THEN
            tabres(i1:i2,j1:j2) = e1v(i1:i2,j1:j2) * ssvmask(i1:i2,j1:j2) * Agrif_Rhox()
         ELSE
            tabres(i1:i2,j1:j2) = e1v(i1:i2,j1:j2) * Agrif_Rhox()
         ENDIF
      ELSE  
         DO ji=i1,i2
            DO jj=j1,j2
               IF (tabres(ji,jj)/=0._wp) THEN
                  e1v(ji,jj)    = tabres(ji,jj)
                  r1_e1v(ji,jj) = 1._wp / tabres(ji,jj)
               END IF
            END DO
         END DO
      ENDIF
      !
   END SUBROUTINE update_e1v


   SUBROUTINE interp_e1e2t_frac(tabres, i1, i2, j1, j2, before )
      !
      !!----------------------------------------------------------------------
      !!               *** ROUTINE interp_e1e2t_frac ***
      !!----------------------------------------------------------------------
      INTEGER                        , INTENT(in   ) :: i1, i2, j1, j2
      REAL(wp),DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres
      LOGICAL                        , INTENT(in   ) :: before
      !!
      !!----------------------------------------------------------------------

      IF (before) THEN
         tabres(i1:i2,j1:j2) = e1e2t(i1:i2,j1:j2)
      ELSE
         e1e2t_frac(i1:i2,j1:j2) = e1e2t(i1:i2,j1:j2) &
              & / tabres(i1:i2,j1:j2) * Agrif_Rhox() * Agrif_Rhoy()
      ENDIF
      !
   END SUBROUTINE interp_e1e2t_frac


   SUBROUTINE interp_e2u_frac(tabres, i1, i2, j1, j2, before )
      !
      !!----------------------------------------------------------------------
      !!               *** ROUTINE interp_e2u_frac ***
      !!----------------------------------------------------------------------
      INTEGER                        , INTENT(in   ) :: i1, i2, j1, j2
      REAL(wp),DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres
      LOGICAL                        , INTENT(in   ) :: before
      !!
      !!----------------------------------------------------------------------

      IF (before) THEN
         tabres(i1:i2,j1:j2) = e2u(i1:i2,j1:j2)
      ELSE
         e2u_frac(i1:i2,j1:j2) = e2u(i1:i2,j1:j2) &
              & / tabres(i1:i2,j1:j2) * Agrif_Rhoy()
      ENDIF
      !
   END SUBROUTINE interp_e2u_frac


   SUBROUTINE interp_e1v_frac(tabres, i1, i2, j1, j2, before )
      !
      !!----------------------------------------------------------------------
      !!               *** ROUTINE interp_e1v_frac ***
      !!----------------------------------------------------------------------
      INTEGER                        , INTENT(in   ) :: i1, i2, j1, j2
      REAL(wp),DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres
      LOGICAL                        , INTENT(in   ) :: before
      !!
      !!----------------------------------------------------------------------

      IF (before) THEN
         tabres(i1:i2,j1:j2) = e1v(i1:i2,j1:j2)
      ELSE
         e1v_frac(i1:i2,j1:j2) = e1v(i1:i2,j1:j2) &
              & / tabres(i1:i2,j1:j2) * Agrif_Rhox()
      ENDIF
      !
   END SUBROUTINE interp_e1v_frac

#else
   SUBROUTINE agrif_update_all
   END SUBROUTINE agrif_update_all
#endif

END MODULE agrif_dom_update
