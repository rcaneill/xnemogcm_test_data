MODULE agrif_connect

   USE dom_oce
   USE agrif_parameters
   USE agrif_profiles
   USE lbclnk
   USE domzgr, ONLY: rn_sbot_min

   IMPLICIT NONE
   PRIVATE

   REAL(wp), ALLOCATABLE, SAVE , DIMENSION(:,:) :: ht0_parent, &
                                                   hu0_parent, &
                                                   hv0_parent, &
                                                   hf0_parent

   PUBLIC agrif_boundary_connections, agrif_bathymetry_connect 

CONTAINS

#if defined key_agrif

   SUBROUTINE agrif_boundary_connections
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE agrif_boundary_connections  ***
      !!----------------------------------------------------------------------  
      INTEGER :: ji, jj

      IF( Agrif_Root() ) return

!      CALL agrif_connection()
      !
!      CALL Agrif_Bc_variable(mbkt_id, procname = connect_bottom_level)
      ! 
!      CALL Agrif_Bc_variable(e3t_copy_id, procname = connect_e3t_copy)

!      ALLOCATE(e3t_interp_done(jpi,jpj))
!!      e3t_interp_done(:,:) = .FALSE. 
!!      ! set extrapolation on for interpolation near the coastline:
!!      Agrif_UseSpecialValue = .TRUE.
!!      Agrif_SpecialValue = 0._wp
!!      CALL Agrif_Bc_variable(e3t_connect_id, procname = connect_e3t_connect)
      ! If child has zps, ensure parent bathymetry is used:
      ! No need to do this if vertical grids are the same
      IF ( ln_zps.AND.ln_vert_remap ) THEN  
         Agrif_UseSpecialValue = .FALSE.
         CALL Agrif_Bc_variable(e3t_id,    procname = connect_e3t_connect)
      ENDIF 
      IF ( ln_sco.AND.ln_vert_remap ) THEN  
! Build parent grid bathymetry over child grid
         ALLOCATE(ht0_parent(jpi,jpj), &
              &   hu0_parent(jpi,jpj), &
              &   hv0_parent(jpi,jpj), &
              &   hf0_parent(jpi,jpj)  ) 

         Agrif_UseSpecialValue = .FALSE.
         CALL Agrif_Init_Variable(ht0_id,  procname = interpht0 )
         !
         IF ( Agrif_Parent(ln_sco) ) THEN
            DO ji=1, jpim1
               DO jj=1, jpjm1
                  hu0_parent(ji,jj) = ssumask(ji,jj) * 0.5_wp  * ( ht0_parent(ji,jj)+ht0_parent(ji+1,jj) ) 
                  hv0_parent(ji,jj) = ssvmask(ji,jj) * 0.5_wp  * ( ht0_parent(ji,jj)+ht0_parent(ji,jj+1) ) 
                  hf0_parent(ji,jj) = ssfmask(ji,jj) * 0.25_wp * ( ht0_parent(ji,jj  )+ht0_parent(ji+1,jj  ) &
                                    &                             +ht0_parent(ji,jj+1)+ht0_parent(ji+1,jj+1) )
               END DO
            END DO
         ELSE
            DO ji=1, jpim1
               DO jj=1, jpjm1
                  hu0_parent(ji,jj) = MIN( ht0_parent(ji  ,jj  ), ht0_parent(ji+1,jj  ) ) 
                  hv0_parent(ji,jj) = MIN( ht0_parent(ji  ,jj  ), ht0_parent(ji  ,jj+1) ) 
                  hf0_parent(ji,jj) = MIN( ht0_parent(ji  ,jj  ), ht0_parent(ji+1,jj  ) , & 
                                    &      ht0_parent(ji  ,jj+1), ht0_parent(ji+1,jj+1) )
               END DO
            END DO
         ENDIF
         CALL lbc_lnk_multi( 'Agrif_boundary_condtions', hu0_parent, 'U', 1.0_wp, & 
                                         &               hv0_parent, 'V', 1.0_wp, &  
                                         &               hf0_parent, 'F', 1.0_wp  )

!         Agrif_UseSpecialValue = .TRUE.
!         Agrif_SpecialValue = 0._wp
         CALL Agrif_Bc_variable(e3u_id,    procname = connect_e3u_connect)
         CALL Agrif_Bc_variable(e3v_id,    procname = connect_e3v_connect)
         CALL Agrif_Bc_variable(e3f_id,    procname = connect_e3f_connect)
         DEALLOCATE(ht0_parent, hu0_parent, hv0_parent, hf0_parent)
      ENDIF  

!      DEALLOCATE(e3t_interp_done)
      !
   END SUBROUTINE agrif_boundary_connections

   SUBROUTINE agrif_bathymetry_connect
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE agrif_bathymetry_connect  ***
      !!----------------------------------------------------------------------  
      IF( Agrif_Root() ) return

      CALL agrif_connection()
      !
      ALLOCATE(e3t_interp_done(jpi,jpj))
      e3t_interp_done(:,:) = .FALSE. 
      ! set extrapolation on for interpolation near the coastline:
      Agrif_UseSpecialValue = .TRUE.
      Agrif_SpecialValue = 0._wp
      l_set_hmin = .TRUE.
      CALL Agrif_Bc_variable(e3t_connect_id, procname = connect_bathy_connect)
      ! Override in ghost zone by nearest value:
      Agrif_UseSpecialValue = .FALSE.
      e3t_interp_done(:,:) = .FALSE.
      l_set_hmin = .FALSE.
      CALL Agrif_Bc_variable(e3t_copy_id,    procname = connect_bathy_connect)
      Agrif_UseSpecialValue = .FALSE.
      DEALLOCATE(e3t_interp_done)
      !
   END SUBROUTINE agrif_bathymetry_connect

   SUBROUTINE connect_e3t_copy( ptab, i1, i2, j1, j2, k1, k2, before, nb,ndir)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE connect_e3t_copy  ***
      !!----------------------------------------------------------------------  
      INTEGER                               , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) ::   ptab
      LOGICAL                               , INTENT(in   ) ::   before
      INTEGER                               , INTENT(in   ) ::   nb , ndir
      !
      !!---------------------------------------------------------------------- 
      !
      IF( before) THEN
         ptab(i1:i2,j1:j2,k1:k2) = e3t_0(i1:i2,j1:j2,k1:k2)
      ELSE
         e3t_0(i1:i2,j1:j2,1:jpk) = ptab(i1:i2,j1:j2,1:jpk)
      ENDIF
      !
   END SUBROUTINE connect_e3t_copy
   
   SUBROUTINE connect_bottom_level( ptab, i1, i2, j1, j2, before, nb,ndir)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE connect_bottom_level  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      INTEGER                         , INTENT(in   ) ::   nb , ndir
      !
      !!---------------------------------------------------------------------- 
      !
      IF( before) THEN
         ptab(i1:i2,j1:j2) = mbkt(i1:i2,j1:j2)*ssmask(i1:i2,j1:j2)
      ELSE
         mbkt(i1:i2,j1:j2) = nint(ptab(i1:i2,j1:j2))
         WHERE (mbkt(i1:i2,j1:j2)==0)
           ssmask(i1:i2,j1:j2) = 0.
         ELSEWHERE
           ssmask(i1:i2,j1:j2) = 1.
         END WHERE  
      ENDIF
      !
   END SUBROUTINE connect_bottom_level
   
   SUBROUTINE connect_e3t_connect( ptab, i1, i2, j1, j2, k1, k2, before, nb,ndir)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE connect_e3t_connect  ***
      !!----------------------------------------------------------------------  
      INTEGER                               , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) ::   ptab
      LOGICAL                               , INTENT(in   ) ::   before
      INTEGER                               , INTENT(in   ) ::   nb , ndir
      !
      !!---------------------------------------------------------------------- 
      INTEGER :: ji, jj, jk, ik 
      REAL(wp) :: ze3min, zdepth, zdepwp, zmax, ze3tp, ze3wp, zhmin 
      !
      IF( before) THEN
         DO jk=k1, k2
            DO jj=j1,j2
               DO ji=i1,i2
                  IF( mbkt(ji,jj) .GE. jk ) THEN
                     ptab(ji,jj,jk) = e3t_0(ji,jj,jk)
                  ELSE
                     ptab(ji,jj,jk) = 0.
                  ENDIF
               END DO
            END DO
         END DO
         !
         DO jj=j1,j2
            DO ji=i1,i2
               ptab(ji,jj,k2) = SUM ( e3t_0(ji,jj, 1:mbkt(ji,jj) ) ) * ssmask(ji,jj)
            END DO
         END DO
      ELSE

         bathy(i1:i2, j1:j2) = ptab(i1:i2,j1:j2,k2)

         DO jk=jpk,1,-1
           zdepth = gdepw_1d(jk) + 1.e-6
           WHERE( 0._wp < bathy(i1:i2,j1:j2) .AND. bathy(i1:i2,j1:j2) <= zdepth ) mbathy(i1:i2,j1:j2) = jk-1
         ENDDO

         WHERE (mbathy(i1:i2,j1:j2) == 0); ssmask(i1:i2,j1:j2) = 0
         ELSE WHERE                      ; ssmask(i1:i2,j1:j2) = 1.
         END WHERE
         
         mbkt(i1:i2,j1:j2) = MAX( mbathy(i1:i2,j1:j2), 1 )
         !
         DO jj = j1, j2
            DO ji = i1, i2
               DO jk = 1, jpk    
                  gdept_0(ji,jj,jk) = gdept_1d(jk)
                  gdepw_0(ji,jj,jk) = gdepw_1d(jk)
                  e3t_0  (ji,jj,jk) = e3t_1d  (jk)
                  e3w_0  (ji,jj,jk) = e3w_1d  (jk)
               END DO 
               !
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
                     e3t_0  (ji,jj,ik) = e3t_1d  (ik) * ( gdepw_0 (ji,jj,ik+1) - gdepw_1d(ik)) &
                           &                / ( gdepw_1d(      ik+1) - gdepw_1d(ik))
                     IF ( ln_e3_dep.AND.ln_dept_mid ) THEN
                        gdept_0(ji,jj,ik) = gdepw_1d(ik) + 0.5_wp * e3t_0(ji,jj,ik)
                        e3w_0(ji,jj,ik) = gdept_0(ji,jj,ik) - gdept_0(ji,jj,ik-1)
                     ELSE
                        gdept_0(ji,jj,ik) = gdepw_1d(ik) + ( gdepw_0(ji,jj,ik+1) - gdepw_1d(ik) ) &
                              &                * ((gdept_1d(     ik  ) - gdepw_1d(ik) )           &
                              &                / ( gdepw_1d(     ik+1) - gdepw_1d(ik) ))
                        e3w_0(ji,jj,ik) = &
                              & 0.5_wp * (gdepw_0(ji,jj,ik+1) + gdepw_1d(ik+1) - 2._wp * gdepw_1d(ik) )   &
                              &        * ( e3w_1d(ik) / ( gdepw_1d(ik+1) - gdepw_1d(ik) ) )
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
!                    e3w_0(ji,jj,ik-1) = 0.5_wp * (e3t_0(ji,jj,ik-1) + e3t_0(ji,jj,ik-2))
!                    e3w_0(ji,jj,ik  ) = 0.5_wp * (e3t_0(ji,jj,ik  ) + e3t_0(ji,jj,ik-1))
!                    e3w_0  (ji,jj,ik+1) = e3t_0(ji,jj,ik)
!                    e3t_0  (ji,jj,ik+1) = e3t_0(ji,jj,ik)
!                 ENDIF
!              ENDIF
!            END DO
!         END DO
      ENDIF
      !
   END SUBROUTINE connect_e3t_connect

   SUBROUTINE connect_e3u_connect( ptab, i1, i2, j1, j2, k1, k2, before, nb,ndir)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE connect_e3u_connect  ***
      !!----------------------------------------------------------------------  
      INTEGER                               , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) ::   ptab
      LOGICAL                               , INTENT(in   ) ::   before
      INTEGER                               , INTENT(in   ) ::   nb , ndir
      !
      !!---------------------------------------------------------------------- 
      INTEGER :: ji, jj, jk
      REAL(wp) :: zup
      !
      IF(.NOT.before) THEN

         DO jj=j1,j2
            DO ji=i1,i2
               IF ( ssumask(ji,jj) > 0 ) THEN
                  zup = 0._wp
                  DO jk=1, mbku(ji,jj)
                     zup = zup + e3u_0(ji,jj,jk)
                  END DO
                  IF (ABS(zup-hu0_parent(ji,jj))>1.e-3) THEN
                     zup = 0._wp
                     DO jk=1, jpkm1   
                        IF ( (zup + 1.5_wp * e3u_0(ji,jj,jk) ) >= hu0_parent(ji,jj) ) THEN
                           e3u_0(ji,jj,jk) = hu0_parent(ji,jj) - zup
                           e3uw_0(ji,jj,jk) = 0.5_wp * (e3u_0(ji,jj,jk-1) + e3u_0(ji,jj,jk))
                           mbku(ji,jj) = jk
                           EXIT
                        ELSE
                           zup = zup + e3u_0(ji,jj,jk)
                        END IF
                     END DO
                  END IF   
               END IF   
            END DO      
         END DO

      ENDIF
      !
   END SUBROUTINE connect_e3u_connect

   SUBROUTINE connect_e3v_connect( ptab, i1, i2, j1, j2, k1, k2, before, nb,ndir)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE connect_e3v_connect  ***
      !!----------------------------------------------------------------------  
      INTEGER                               , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) ::   ptab
      LOGICAL                               , INTENT(in   ) ::   before
      INTEGER                               , INTENT(in   ) ::   nb , ndir
      !
      !!---------------------------------------------------------------------- 
      INTEGER :: ji, jj, jk
      REAL(wp) :: zup
      !
      IF(.NOT.before) THEN

         DO jj=j1,j2
            DO ji=i1,i2
               IF ( ssvmask(ji,jj) > 0 ) THEN
                  zup = 0._wp
                  DO jk=1, mbkv(ji,jj)
                     zup = zup + e3v_0(ji,jj,jk)
                  END DO
                  IF (ABS(zup-hv0_parent(ji,jj))>1.e-3) THEN
                     zup = 0._wp
                     DO jk=1, jpkm1   
                        IF ( (zup + 1.5_wp * e3v_0(ji,jj,jk) ) >= hv0_parent(ji,jj) ) THEN
                           e3v_0(ji,jj,jk) = hv0_parent(ji,jj) - zup
                           e3vw_0(ji,jj,jk) = 0.5_wp * (e3v_0(ji,jj,jk-1) + e3v_0(ji,jj,jk))
                           mbkv(ji,jj) = jk
                           EXIT
                        ELSE
                           zup = zup + e3v_0(ji,jj,jk)
                        END IF
                     END DO
                  END IF   
               END IF   
            END DO      
         END DO

      ENDIF
      !
   END SUBROUTINE connect_e3v_connect

   SUBROUTINE connect_e3f_connect( ptab, i1, i2, j1, j2, k1, k2, before, nb,ndir)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE connect_e3f_connect  ***
      !!----------------------------------------------------------------------  
      INTEGER                               , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) ::   ptab
      LOGICAL                               , INTENT(in   ) ::   before
      INTEGER                               , INTENT(in   ) ::   nb , ndir
      !
      !!---------------------------------------------------------------------- 
      INTEGER :: ji, jj, jk
      REAL(wp) :: zup
      !
      IF(.NOT.before) THEN

         DO jj=j1,j2
            DO ji=i1,i2
               IF ( ssfmask(ji,jj) > 0 ) THEN
                  zup = 0._wp
                  DO jk=1, mbkf(ji,jj)
                     zup = zup + e3f_0(ji,jj,jk)
                  END DO
                  IF (ABS(zup-hf0_parent(ji,jj))>1.e-3) THEN
                     zup = 0._wp
                     DO jk=1, jpkm1   
                        IF ( (zup + 1.5_wp * e3f_0(ji,jj,jk) ) >= hf0_parent(ji,jj) ) THEN
                           e3f_0(ji,jj,jk) = hf0_parent(ji,jj) - zup
                           mbkf(ji,jj) = jk
                           EXIT
                        ELSE
                           zup = zup + e3f_0(ji,jj,jk)
                        END IF
                     END DO
                  END IF   
               END IF   
            END DO      
         END DO

      ENDIF
      !
   END SUBROUTINE connect_e3f_connect

   SUBROUTINE connect_bathy_connect( ptab, i1, i2, j1, j2, k1, k2, before, nb,ndir)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE connect_e3t_connect  ***
      !!----------------------------------------------------------------------  
      INTEGER                               , INTENT(in   ) ::   i1, i2, j1, j2, k1, k2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) ::   ptab
      LOGICAL                               , INTENT(in   ) ::   before
      INTEGER                               , INTENT(in   ) ::   nb , ndir
      !
      !!---------------------------------------------------------------------- 
      INTEGER :: ji, jj, jk, ik
      REAL(wp) :: zhmin
      !
      IF( before) THEN
         DO jk=k1,k2
            DO jj=j1,j2
               DO ji=i1,i2
                  IF ((ssmask(ji,jj)/=0._wp).AND.( mbkt(ji,jj) .GE. jk )) THEN
                     ptab(ji,jj,jk) = e3t_0(ji,jj,jk)
                  ELSE
                     ptab(ji,jj,jk) = 0._wp
                  ENDIF
               END DO
            END DO
         END DO
         !
         DO jj=j1,j2
            DO ji=i1,i2
               ptab(ji,jj,k2) = SUM ( e3t_0(ji,jj, 1:mbkt(ji,jj) ) ) * ssmask(ji,jj)
            END DO
         END DO
      ELSE

         IF (l_set_hmin) THEN
            IF ( ln_sco ) THEN
               zhmin = rn_sbot_min
            ELSE
               IF( rn_hmin < 0._wp ) THEN
                  ik = - INT( rn_hmin )
               ELSE
                  ik = MINLOC( gdepw_1d, mask = gdepw_1d > rn_hmin, dim = 1 )
               ENDIF
               zhmin = gdepw_1d(ik+1)
            ENDIF
         ELSE
            zhmin = 0._wp
         ENDIF

         DO jj=j1,j2
            DO ji=i1,i2
               ! keep child masking in transition zone:
               IF ((ztabramp(ji,jj)/=1._wp).AND.(bathy(ji,jj)==0._wp)) ptab(ji,jj,k2) = 0._wp
               ! Connected bathymetry:
               IF( .NOT.e3t_interp_done(ji,jj) ) THEN
                  bathy(ji,jj)=(1._wp-ztabramp(ji,jj))*bathy(ji,jj)+ztabramp(ji,jj)*ptab(ji,jj,k2)
                  IF (bathy(ji,jj)/=0._wp) bathy(ji,jj) = MAX(bathy(ji,jj), zhmin)
                  e3t_interp_done(ji,jj) = .TRUE.
               ENDIF
 
            END DO
         END DO
      ENDIF
      !
   END SUBROUTINE connect_bathy_connect
   
   SUBROUTINE agrif_connection
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE  Agrif_connection ***
      !!----------------------------------------------------------------------
      INTEGER  ::   ji, jj, ind1, ind2
      INTEGER  ::   ispongearea, istart
      REAL(wp) ::   z1_spongearea
      !!----------------------------------------------------------------------
      !
      ! Define ramp from boundaries towards domain interior at T-points
      ! Store it in ztabramp

      IF (.NOT.ALLOCATED(ztabramp)) ALLOCATE(ztabramp(jpi,jpj))
      ispongearea = 1 + npt_connect * Agrif_iRhox()
      istart = npt_copy * Agrif_iRhox() + 1
      z1_spongearea = 1._wp / REAL( ispongearea, wp )
      
      ztabramp(:,:) = 0._wp

      ! --- West --- !
      IF( lk_west ) THEN
         ind1 = nn_hls + nbghostcells + istart
         ind2 = ind1 + ispongearea 
         DO ji = mi0(ind1), mi1(ind2)   
            DO jj = 1, jpj               
               ztabramp(ji,jj) = REAL(ind2 - mig(ji), wp) * z1_spongearea
            END DO
         ENDDO
            ! ghost cells:
            ind1 = 1
            ind2 = nn_hls + nbghostcells + istart   ! halo + land + nbghostcells
            DO ji = mi0(ind1), mi1(ind2)   
               DO jj = 1, jpj               
                  ztabramp(ji,jj) = 1._wp
               END DO
            END DO
      ENDIF

      ! --- East --- !
      IF( lk_east ) THEN
         ind2 = jpiglo -  (nn_hls + nbghostcells -1 ) - istart
         ind1 = ind2 -ispongearea       
         DO ji = mi0(ind1), mi1(ind2)
            DO jj = 1, jpj
               ztabramp(ji,jj) = MAX( ztabramp(ji,jj), REAL( mig(ji) - ind1 ) * z1_spongearea )
            ENDDO
         ENDDO
            ! ghost cells:
            ind1 = jpiglo -  (nn_hls + nbghostcells - 1 ) - istart   ! halo + land + nbghostcells - 1
            ind2 = jpiglo - 1
            DO ji = mi0(ind1), mi1(ind2)
               DO jj = 1, jpj
                  ztabramp(ji,jj) = 1._wp
               END DO
            END DO
      ENDIF

      ispongearea = 1 + npt_connect * Agrif_iRhoy()
      istart = npt_copy * Agrif_iRhoy() + 1
      z1_spongearea = 1._wp / REAL( ispongearea, wp )

      ! --- South --- !
      IF( lk_south ) THEN
         ind1 = nn_hls + nbghostcells + istart
         ind2 = ind1 + ispongearea 
         DO jj = mj0(ind1), mj1(ind2) 
            DO ji = 1, jpi
               ztabramp(ji,jj) = MAX( ztabramp(ji,jj), REAL( ind2 - mjg(jj) ) * z1_spongearea  )
            END DO
         ENDDO
            ! ghost cells:
            ind1 = 1
            ind2 = nn_hls + nbghostcells + istart                 ! halo + land + nbghostcells
            DO jj = mj0(ind1), mj1(ind2) 
               DO ji = 1, jpi
                  ztabramp(ji,jj) = 1._wp
               END DO
            END DO
      ENDIF

      ! --- North --- !
      IF( lk_north ) THEN
         ind2 = jpjglo - (nn_hls + nbghostcells - 1) - istart
         ind1 = ind2 -ispongearea
         DO jj = mj0(ind1), mj1(ind2)
            DO ji = 1, jpi
               ztabramp(ji,jj) = MAX( ztabramp(ji,jj), REAL( mjg(jj) - ind1 ) * z1_spongearea )
            END DO
         ENDDO
            ! ghost cells:
            ind1 = jpjglo - (nn_hls + nbghostcells - 1) - istart      ! halo + land + nbghostcells - 1
            ind2 = jpjglo
            DO jj = mj0(ind1), mj1(ind2)
               DO ji = 1, jpi
                  ztabramp(ji,jj) = 1._wp
               END DO
            END DO
      ENDIF
      !
   END SUBROUTINE agrif_connection


   SUBROUTINE interpht0( ptab, i1, i2, j1, j2, before )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpht0  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      INTEGER                                         ::   jk
      !
      !!----------------------------------------------------------------------  
      !
      IF( before) THEN
         ptab(i1:i2,j1:j2) = 0._wp
         DO jk=1,jpkm1 
            ptab(i1:i2,j1:j2) = ptab(i1:i2,j1:j2) + &
                              & e3t_0(i1:i2,j1:j2,jk) * tmask(i1:i2,j1:j2,jk) 
         END DO
      ELSE
         ht0_parent(i1:i2,j1:j2) = ptab(i1:i2,j1:j2)
      ENDIF
      !
   END SUBROUTINE interpht0


#else
   SUBROUTINE agrif_boundary_connections
   END SUBROUTINE agrif_boundary_connections
   SUBROUTINE agrif_bathymetry_connect 
   END SUBROUTINE agrif_bathymetry_connect 
#endif

END MODULE agrif_connect
