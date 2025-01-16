MODULE domisf
   !!==============================================================================
   !!                       ***  MODULE domisf   ***
   !! Ocean domain : 
   !!==============================================================================
   !! History :  4.1  ! 2019-07  (P. Mathiot) re-write of the geometry definition under ice shelf
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   zgr_isf_nam    : read ice shelf namelist; print and compatibility option check
   !!   zgr_isf_zspace : check compatibility between bathymetry and ice shelf draft and adjust ice shelf draft if needed (z space) 
   !!   zgr_isf_kspace : adjust ice shelf draft and compute top level to fit 2 cell in the water column (k space) 
   !!   zgr_isf        : compute top partial cell scale factor
   !!   zgr_isf_e3uv_w : correct e3uw and e3vw in case of 2 cell in water column under an ice shelf
   !!---------------------------------------------------------------------
   USE dom_oce
   USE domutl            ! flood filling algorithm
   USE domngb            ! find nearest neighbourg
   USE in_out_manager    ! I/O manager
   USE lbclnk            ! lbclnk and lbclnk_multi
   USE lib_mpp           ! 

   IMPLICIT NONE
   PRIVATE

   PUBLIC   zgr_isf_nam    !: read domisf namelist
   PUBLIC   zgr_isf_zspace !: check compatibility ice shelf draft/bathymetry and dig ice shelf draft if needed
   PUBLIC   zgr_isf_kspace !: compute misfdep and dig ice shelf draft if needed
   PUBLIC   zps_isf        !: compute zps scale factor
   PUBLIC   zps_isf_e3uv_w !: compute e3uw and e3vw in case of 2 cell in water column
   PUBLIC   zgr_isf_subgl

   INTEGER          :: nn_kisfmax    = 999              !: maximal number of level change allowed by ln_isfconnect option 
   REAL(wp), PUBLIC :: rn_isfdep_min = 10.0_wp          !: ice shelf minimal thickness 
   REAL(wp), PUBLIC :: rn_glhw_min   = 1.0e-3           !: threshold on hw to define grounding line water
   REAL(wp), PUBLIC :: rn_isfhw_min  = 1.0e-3           !: threshold on hw to define isf draft into the cavity
   REAL(wp), PUBLIC :: rn_isfshallow = 0.0_wp           !: threshold to define shallow ice shelf cavity
   REAL(wp), PUBLIC :: rn_zisfmax    = 6000.0_wp        !: maximun meter of ice we are allowed to dig to assure connectivity
   REAL(wp), PUBLIC :: rn_isfsubgllon, rn_isfsubgllat   !: seed for the detection of the 3d open ocean
   LOGICAL , PUBLIC :: ln_isfcheminey= .FALSE.          !: remove cheminey
   LOGICAL , PUBLIC :: ln_isfconnect = .FALSE.          !: assure connectivity 
   LOGICAL , PUBLIC :: ln_isfchannel = .FALSE.          !: remove channel in water column (on z space not level space)
   LOGICAL , PUBLIC :: ln_isfsubgl   = .FALSE.          !: remove subglacial lakes

CONTAINS

   SUBROUTINE zgr_isf_nam
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE zps_isf  ***
      !!   
      !! ** Purpose :   Read namzgr_isf namelist and 
      !!                check compatibility with other option
      !!   
      !!----------------------------------------------------------------------
      !!
      INTEGER  :: ios, ierr 
      !!---------------------------------------------------------------------
      NAMELIST/namzgr_isf/nn_kisfmax, rn_zisfmax, rn_isfdep_min, rn_glhw_min, rn_isfhw_min, &
         &                ln_isfcheminey, ln_isfconnect, ln_isfchannel, ln_isfsubgl, rn_isfsubgllon, rn_isfsubgllat
      !
      ! 0.0 read namelist
 !     REWIND( numnam_ref )              ! Namelist namzgr_isf in reference namelist : ice shelf geometry definition
      READ  ( numnam_ref, namzgr_isf, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namzgr_isf in reference namelist')

 !     REWIND( numnam_cfg )              ! Namelist namzgr_sco in configuration namelist : ice shelf geometry definition
      READ  ( numnam_cfg, namzgr_isf, IOSTAT = ios, ERR = 902 )
902   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namzgr_isf in configuration namelist')
      IF(lwm) WRITE ( numond, namzgr_isf )
      !
      IF (lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) ' dom_isf : '
         WRITE(numout,*) ' ~~~~~~~   '
         WRITE(numout,*) ' nn_kisfmax       = ',nn_kisfmax           !
         WRITE(numout,*) ' rn_zisfmax       = ',rn_zisfmax           !
         WRITE(numout,*) ' rn_isfdep_min    = ',rn_isfdep_min        !
         WRITE(numout,*) ' rn_isfhw_min     = ',rn_isfhw_min         !
         WRITE(numout,*) ' rn_glhw_min      = ',rn_glhw_min          !
         WRITE(numout,*) ' ln_isfcheminey   = ',ln_isfcheminey       !
         WRITE(numout,*) ' ln_isfconnect    = ',ln_isfconnect        !
         WRITE(numout,*) ' ln_isfchannel    = ',ln_isfchannel        !
      END IF
      !
      ! 0.1 compatibility option
      ierr = 0
      IF ( ln_zco .OR. ln_sco ) ierr = ierr + 1
      IF ( ierr > 0 ) CALL ctl_stop( ' Cavity not tested/compatible with full step (zco) and sigma (ln_sco) ' )

   END SUBROUTINE zgr_isf_nam

   SUBROUTINE zgr_isf_zspace
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dom_isf  ***
      !!   
      !! ** Purpose :   compute the grounding line position
      !!   
      !! ** Method  :   bathy set to 0 and isfdraft are modified (dig/close) to respect
      !!                these criterions.
      !!                digging and filling base on depth criterion only
      !!                          1.0 = set iceshelf to the minimum depth allowed
      !!                          1.1 = ground ice shelf if water column less than X m
      !!                          1.2 = ensure a minimum thickness for iceshelf cavity in shallow water
      !!                          1.3 = remove channels and single point 'bay'
      !!   
      !! ** Action  : - test compatibility between isfdraft and bathy 
      !!              - bathy and isfdraft are modified
      !!----------------------------------------------------------------------
      !!  
      INTEGER  :: ji, jj                             ! loop indexes
      INTEGER  :: imskjp1, imskjm1, imskip1, imskim1 ! local variable
      INTEGER, DIMENSION(jpi,jpj) :: imask           ! isf mask
      !
      REAL(wp) ::   zisfdep_min  ! minimal ice shelf draft allowed
      !!---------------------------------------------------------------------
      !
      ! 1.0 set iceshelf to the minimum depth allowed
      zisfdep_min = MAX(rn_isfdep_min,e3t_1d(1))
      WHERE(risfdep(:,:) > 0.0_wp .AND. risfdep(:,:) < zisfdep_min)
         risfdep(:,:) = zisfdep_min
      END WHERE
      !  
      ! 1.1 ground ice shelf if water column less than rn_glhw_min m 
      ! => set the grounding line position
      WHERE( bathy(:,:) - risfdep(:,:) < rn_glhw_min .AND. risfdep(:,:) > 0.0_wp ) 
         bathy  (:,:) = 0._wp ; risfdep(:,:) = 0._wp
      END WHERE
      !
      ! 1.2 ensure a minimum thickness for iceshelf cavity 
      ! => avoid to negative e3t if ssh + sum(e3t_0*tmask) < 0
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF ( bathy(ji,jj) - risfdep(ji,jj) < rn_isfhw_min .AND. risfdep(ji,jj) > 0.0_wp ) THEN
               risfdep(ji,jj) = bathy(ji,jj) - rn_isfhw_min
               ! sanity check on risfdep (if < zisfdep_min)
               ! => we ground it as it failed to respect condition 1.0 and 1.1
               IF ( risfdep(ji,jj) < zisfdep_min ) THEN 
                  bathy(ji,jj)=0._wp ; risfdep(ji,jj)=0._wp
               END IF
            END IF
         END DO
      END DO
      !
      ! 1.3 Remove channels and single point 'bay' using bathy mask.
      ! => channel could be created if connectivity is enforced later.
      IF (ln_isfchannel) THEN
         imask(:,:) = 0
         WHERE ( bathy(:,:) > 0._wp )
            imask(:,:) = 1
         END WHERE
         DO jj = 2, jpjm1
            DO ji = 2, jpim1
               IF( imask(ji,jj) == 1 .AND. risfdep(ji,jj) > 0._wp) THEN
                  ! define connexion
                  imskip1=imask(ji,jj)*imask(ji+1,jj  )  ! 1 = connexion
                  imskjp1=imask(ji,jj)*imask(ji  ,jj+1)  ! 1 = connexion
                  imskim1=imask(ji,jj)*imask(ji-1,jj  )  ! 1 = connexion
                  imskjm1=imask(ji,jj)*imask(ji  ,jj-1)  ! 1 = connexion
                  ! zonal channel and single bay
                  IF ((imskip1+imskim1>=1) .AND. (imskjp1+imskjm1==0)) THEN
                     bathy(ji,jj)=0._wp ; risfdep(ji,jj)=0._wp
                  END IF
                  ! meridionnal channel and single bay
                  IF ((imskjp1+imskjm1>=1) .AND. (imskip1+imskim1==0)) THEN
                     bathy(ji,jj)=0._wp ; risfdep(ji,jj)=0._wp
                  END IF
               END IF
            END DO
         END DO
         ! ensure halo correct 
         CALL lbc_lnk_multi( 'domisf', risfdep, 'T', 1._wp, bathy  , 'T', 1._wp )
      END IF
      !
   END SUBROUTINE zgr_isf_zspace

   SUBROUTINE zgr_isf_kspace
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dom_isf  ***
      !!   
      !! ** Purpose :   compute misfdep and update isf draft if needed
      !!   
      !! ** Method  :   The water column has to contain at least 2 wet cells in the water column under an ice shelf
      !!                isf draft is modified (dig/close) to respect this criterion.
      !!                compute level
      !!                          2.0 = compute level
      !!                          2.1 = ensure misfdep is not below bathymetry after step 2.0
      !!                digging and filling base on level criterion only                
      !!                          3.0 = dig to fit the 2 water level criterion (closed pool possible after this step)
      !!                          3.1 = dig enough to ensure connectivity of all the cell beneath an ice shelf 
      !!                                (most of the close pool should be remove after this step)
      !!                          3.2 = fill chimney
      !!   
      !! ** Action  : - compute misfdep
      !!              - isf draft is modified if needed
      !!----------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk, jn
      INTEGER  ::   icompt
      INTEGER  ::   ibtest,           ibtestim1, ibtestip1, ibtestjm1, ibtestjp1
      INTEGER  ::   ibathy, ibathyij, ibathyim1, ibathyip1, ibathyjm1, ibathyjp1
      INTEGER  ::   imskjp1  , imskjm1  , imskip1  , imskim1
      INTEGER  ::   imskip1_r, imskim1_r, imskjp1_r, imskjm1_r
      INTEGER , DIMENSION(jpi,jpj) :: imbathy, imisfdep, imask
      !
      REAL(wp) ::   zdepth
      REAL(wp), DIMENSION(jpi,jpj) :: zrisfdep, zdummy   ! 2D workspace (ISH)
      !!---------------------------------------------------------------------
      !
      ! 2 Compute misfdep for ocean points (i.e. first wet level) 
      ! find the first ocean level such that the first level thickness 
      ! is larger than the bot_level of e3zps_min and e3zps_rat * e3t_0 (where 
      ! e3t_0 is the reference level thickness 
      !
      ! 2.0 compute misfdep (taking into account the minimal cell thickness allowed)
      ! set misfdep to 1 on open water and initialisation beneath ice shelf
      WHERE( risfdep(:,:) == 0._wp ) ;   misfdep(:,:) = 1   ! open water or grounded : set misfdep to 1  
      ELSEWHERE                      ;   misfdep(:,:) = 2   ! iceshelf : initialize misfdep to second level 
      END WHERE  
      !
      DO jk = 2, jpkm1 
         zdepth = gdepw_1d(jk+1) - MIN( e3zps_min, e3t_1d(jk)*e3zps_rat ) 
         WHERE( risfdep(:,:) > 0._wp .AND. risfdep(:,:) >= zdepth )   misfdep(:,:) = jk+1 
      END DO 
      !
      ! 2.1 fill isolated grid point in the bathymetry
      ! will be done again later on in zgr_bat_ctl, but need to be done here to adjust misfdep respectively
      icompt = 0
      DO jj = 2, jpjm1
         DO ji = 2, jpim1
            ibtest = MAX(  mbathy(ji-1,jj), mbathy(ji+1,jj),   &
               &           mbathy(ji,jj-1), mbathy(ji,jj+1)  )
               IF( ibtest < mbathy(ji,jj) ) THEN
                  mbathy(ji,jj) = ibtest
                  icompt = icompt + 1
               END IF
         END DO
      END DO
      !
      ! ensure halo correct 
      zdummy(:,:) = FLOAT( mbathy(:,:) ) ; CALL lbc_lnk('domisf', zdummy, 'T', 1._wp ) ; mbathy(:,:) = INT( zdummy(:,:) )
      !
      IF( lk_mpp )   CALL mpp_sum('domisf', icompt )
      IF( icompt == 0 ) THEN
         IF(lwp) WRITE(numout,*)'     no isolated ocean grid points'
      ELSE
         IF(lwp) WRITE(numout,*)'    ',icompt,' ocean grid points suppressed'
      ENDIF
      !
      ! 2.2 be sure misfdep not below mbathy 
      ! warning because of condition 4 we could have 'wet cell with misfdep below mbathy
      ! risfdep of these cells will be fix later on (see 3)
      WHERE( misfdep > mbathy ) misfdep(:,:) = MAX( 1, mbathy(:,:) )
      !
      ! 3.0 Assure 2 wet cells in the water column at T point and along the edge.
      ! find the deepest isfdep level that fit the 2 wet cell on the water column
      ! on all the sides (still need 4 pass)
      ! It need 4 iterations: if un-luky, digging cell U-1 can trigger case for U+1, then V-1, then V+1
      DO jn = 1, 4
         imisfdep = misfdep
         DO jj = 2, jpjm1
            DO ji = 2, jpim1
               ! ISF cell only
               IF(  (misfdep(ji,jj) > 1) .AND. (mbathy(ji,jj) > 0) ) THEN
                  ibathyij  = mbathy(ji  ,jj)
                  !
                  ! set ground edge value to jpk to skip it later on
                  ibathyip1 = mbathy(ji+1,jj) ; IF ( ibathyip1 < misfdep(ji,jj) ) ibathyip1 = jpk ! no wet cell in common on this edge
                  ibathyim1 = mbathy(ji-1,jj) ; IF ( ibathyim1 < misfdep(ji,jj) ) ibathyim1 = jpk ! no wet cell in common on this edge
                  ibathyjp1 = mbathy(ji,jj+1) ; IF ( ibathyjp1 < misfdep(ji,jj) ) ibathyjp1 = jpk ! no wet cell in common on this edge
                  ibathyjm1 = mbathy(ji,jj-1) ; IF ( ibathyjm1 < misfdep(ji,jj) ) ibathyjm1 = jpk ! no wet cell in common on this edge
                  !
                  ! find shallowest bathy level among the current cell and the neigbourging cells
                  ibathy = MIN(ibathyij,ibathyip1,ibathyim1,ibathyjp1,ibathyjm1)
                  !
                  ! update misfdep and risfdep if needed
                  ! misfdep need to be <= zmbathyij-1 to fit 2 wet cell on the water column
                  jk = MIN(misfdep(ji,jj),ibathy-1)
                  IF ( jk < misfdep(ji,jj) ) THEN
                     imisfdep(ji,jj) = jk
                     risfdep(ji,jj)  = gdepw_1d(jk+1) - MIN( e3zps_min, e3t_1d(jk)*e3zps_rat )
                  END IF
               ENDIF
            END DO
         END DO
         misfdep=imisfdep
         !
         ! ensure halo correct before new pass 
         zdummy(:,:) = FLOAT( misfdep(:,:) ); CALL lbc_lnk('domisf', zdummy, 'T', 1. ); misfdep(:,:) = INT( zdummy(:,:) )
         CALL lbc_lnk('domisf', risfdep, 'T', 1. )
      END DO ! jn
      !
      ! 3.1 condition block to inssure connectivity everywhere beneath an ice shelf
      IF (ln_isfconnect) THEN
         imask(:,:) = 1
         imbathy  = mbathy
         imisfdep = misfdep
         zrisfdep = risfdep
         WHERE ( mbathy(:,:) == 0 )
            imask(:,:) = jpk 
            imbathy(:,:) = jpk
         END WHERE
         DO jj = 2, jpjm1
            DO ji = 2, jpim1
               IF(  (misfdep(ji,jj) > 1) .AND. (mbathy(ji,jj) > 0) ) THEN
                  !
                  ! what it should be (1 = should be connected; >= jpk = should not be connected)
                  imskip1 = imask(ji,jj) * imask(ji+1,jj  )  ! 1 = should be connected
                  imskim1 = imask(ji,jj) * imask(ji-1,jj  )  ! 1 = should be connected
                  imskjp1 = imask(ji,jj) * imask(ji  ,jj+1)  ! 1 = should be connected
                  imskjm1 = imask(ji,jj) * imask(ji  ,jj-1)  ! 1 = should be connected
                  !
                  ! what it is ? ( 1 = no effective connection; jpk = effective connection )
                  imskip1_r=jpk ; imskim1_r=jpk; imskjp1_r=jpk; imskjm1_r=jpk
                  IF (misfdep(ji,jj) > imbathy(ji+1,jj  )) imskip1_r=1.0 ! 1 = no effective connection
                  IF (misfdep(ji,jj) > imbathy(ji-1,jj  )) imskim1_r=1.0 ! 1 = no effective connection
                  IF (misfdep(ji,jj) > imbathy(ji  ,jj+1)) imskjp1_r=1.0 ! 1 = no effective connection
                  IF (misfdep(ji,jj) > imbathy(ji  ,jj-1)) imskjm1_r=1.0 ! 1 = no effective connection
                  !
                  ! defining level needed for connectivity
                  ! imskip1 * imskip1_r == 1 means connections need to be enforce
                  ! imskip1 * imskip1_r >= jpk means no connection need to be enforce          
                  jk=MIN(imbathy(ji+1,jj  ) * imskip1_r * imskip1, &
                     &   imbathy(ji-1,jj  ) * imskim1_r * imskim1, &
                     &   imbathy(ji  ,jj+1) * imskjp1_r * imskjp1, &
                     &   imbathy(ji  ,jj-1) * imskjm1_r * imskjm1, &
                     &   jpk ) ! add jpk in the MIN to avoid out of boundary later on
                  !
                  ! if connectivity is OK or no connection needed (grounding line) or grounded, zmisfdep=misfdep
                  imisfdep(ji,jj)=MIN(misfdep(ji,jj),jk-1)
                  !
                  ! update isf draft if needed (need to be done here because next test is in meter and level)
                  IF ( imisfdep(ji,jj) < misfdep(ji,jj) ) THEN
                     jk = imisfdep(ji,jj)
                     zrisfdep(ji,jj)  = gdepw_1d(jk+1) - MIN( e3zps_min, e3t_1d(jk)*e3zps_rat )
                  END IF
   
                  ! sanity check
                  ! check if we dig more than nn_kisfmax level or reach the surface
                  ! check if we dig more than rn_zisfmax meter
                  ! => if this is the case, undo what has been done before
                  IF (      (misfdep(ji,jj)-imisfdep(ji,jj) > MIN(nn_kisfmax,misfdep(ji,jj)-2)) &
                     & .OR. (risfdep(ji,jj)-zrisfdep(ji,jj) > MIN(rn_zisfmax,risfdep(ji,jj)  )) ) THEN
                     imisfdep(ji,jj)=misfdep(ji,jj) 
                     zrisfdep(ji,jj)=risfdep(ji,jj)
                  END IF
               END IF
            END DO
         END DO
         misfdep=imisfdep
         risfdep=zrisfdep
         !
         ! ensure halo correct 
         zdummy(:,:) = FLOAT( misfdep(:,:) ); CALL lbc_lnk('domisf', zdummy, 'T', 1. ); misfdep(:,:) = INT( zdummy(:,:) )
         CALL lbc_lnk('domisf', risfdep, 'T', 1. )
      END IF
      !
      ! 3.2 fill hole in ice shelf (ie cell with no velocity point)
      !      => misfdep = MIN(misfdep at U, U-1, V, V-1)
      !         risfdep = gdepw(misfdep) (ie full cell)
      IF (ln_isfcheminey) THEN
         imisfdep = misfdep
         WHERE (mbathy == 0) imisfdep=jpk   ! grounded
         DO jj = 2, jpjm1
            DO ji = 2, jpim1
               ibtest    = imisfdep(ji  ,jj  )
               ibtestim1 = imisfdep(ji-1,jj  ) ; ibtestip1 = imisfdep(ji+1,jj  )
               ibtestjm1 = imisfdep(ji  ,jj-1) ; ibtestjp1 = imisfdep(ji  ,jj+1)
               !
               ! correction in case isfdraft(ii,jj) deeper than bathy U-1/U+1/V-1/V+1
               IF( imisfdep(ji,jj) > mbathy(ji-1,jj  ) ) ibtestim1 = jpk ! mask at U-1
               IF( imisfdep(ji,jj) > mbathy(ji+1,jj  ) ) ibtestip1 = jpk ! mask at U+1
               IF( imisfdep(ji,jj) > mbathy(ji  ,jj-1) ) ibtestjm1 = jpk ! mask at V-1
               IF( imisfdep(ji,jj) > mbathy(ji  ,jj+1) ) ibtestjp1 = jpk ! mask at V+1
               !
               ! correction in case bathy(ii,jj) shallower than isfdraft U-1/U+1/V-1/V+1
               IF( mbathy(ji,jj) < imisfdep(ji-1,jj  ) ) ibtestim1 = jpk ! mask at U-1
               IF( mbathy(ji,jj) < imisfdep(ji+1,jj  ) ) ibtestip1 = jpk ! mask at U+1
               IF( mbathy(ji,jj) < imisfdep(ji  ,jj-1) ) ibtestjm1 = jpk ! mask at V-1
               IF( mbathy(ji,jj) < imisfdep(ji  ,jj+1) ) ibtestjp1 = jpk ! mask at V+1
               ! 
               ! if hole in the ice shelf, set to the min of surrounding (the MIN is doing the job) 
               ! if misfdep is not changed, nothing is done
               ibtest = MAX(ibtest,MIN(ibtestim1, ibtestip1, ibtestjm1,ibtestjp1))
               IF (misfdep(ji,jj) < ibtest) THEN
                  misfdep(ji,jj) = ibtest
                  risfdep(ji,jj) = gdepw_1d(ibtest)
               END IF
            ENDDO
         ENDDO
         !
         ! if surround is fully masked, we mask it
         WHERE( misfdep(:,:) == jpk)   ! ground case (1)
            mbathy(:,:) = 0 ; bathy(:,:) = 0.0_wp ; misfdep(:,:) = 1 ; risfdep(:,:) = 0.0_wp
         END WHERE
         !
         ! ensure halo correct
         zdummy(:,:) = FLOAT( misfdep(:,:) ); CALL lbc_lnk('domisf', zdummy, 'T', 1. ); misfdep(:,:) = INT( zdummy(:,:) )
         zdummy(:,:) = FLOAT( mbathy (:,:) ); CALL lbc_lnk('domisf', zdummy, 'T', 1. ); mbathy (:,:) = INT( zdummy(:,:) )
         CALL lbc_lnk('domisf', risfdep, 'T', 1. )
         CALL lbc_lnk('domisf', bathy  , 'T', 1. )
      END IF
      !
   END SUBROUTINE zgr_isf_kspace

   SUBROUTINE zgr_isf_subgl
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE zps_isf_subg  ***
      !!   
      !! ** Purpose :   remove subglacial lakes
      !!   
      !! ** Method  :   Use a flood filling algorithm to detect the open ocean
      !!                and reverse the mask
      !!   
      !! ** Action  : - ckeck if seed on land
      !!              - detect main ocean
      !!              - mask subglacial lake (closed sea under an ice shelf)
      !!              - mask properly mbathy, misfdep, bathy, risfdep
      !!----------------------------------------------------------------------
      !!
      INTEGER  :: jk                       ! loop index
      INTEGER  :: iiseed, ijseed           ! seed for flood filling algorithm
      INTEGER, DIMENSION(jpi,jpj) :: imask ! 2d mask
      !
      REAL(wp)               :: zdist      ! distance between the seed and the nearest neighbourg                
      REAL(wp), DIMENSION(1) :: zchk       ! sanity check variable
      !!----------------------------------------------------------------------
      !
      ! check closed wet pool
      CALL dom_ngb(rn_isfsubgllon, rn_isfsubgllat, iiseed, ijseed, zdist, 'T')
      !
      ! fill open ocean
      CALL fill_pool( iiseed, ijseed, 1, tmask, -1._wp )
      ! at this point tmask:,:,:) can have 3 different values :
      !              0 where there where already 0
      !             -1 where the ocean points are connected
      !              1 where ocean points in tmask are not connected
      IF (lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*)'dom_isf_subg : removal of subglacial lakes '
         WRITE(numout,*)'~~~~~~~~~~~~'
         WRITE(numout,*)'Number of disconected points : ', COUNT(  (tmask(:,:,:) == 1) )
         WRITE(numout,*)'lon/lat seed to detect main ocean is: ', rn_isfsubgllon, rn_isfsubgllat
         WRITE(numout,*)'i/j     seed to detect main ocean is: ', iiseed, ijseed
         WRITE(numout,*)
      END IF
      !
      DO jk = 1, jpk
         ! remove only subglacial lake (ie similar to close sea only below an ice shelf)
         WHERE (tmask(:,:,jk) > 0 .AND. misfdep(:,:) > 1) tmask(:,:,jk) = 0
         WHERE (tmask(:,:,jk) < 0) tmask(:,:,jk) = 1
         !
      END DO
      !
      ! surface mask
      imask(:,:)   = MAXVAL( tmask(:,:,:), DIM=3 )
      !
      ! mask mbathy, misfdep, bathy and risfdep 
      bathy(:,:)   = bathy(:,:)   * imask(:,:) 
      mbathy(:,:)  = mbathy(:,:)  * imask(:,:)
      risfdep(:,:) = risfdep(:,:) * imask(:,:) 
      misfdep(:,:) = misfdep(:,:) * imask(:,:)
      !
      ! sanity check on seed location (land or not)
      zchk = 0._wp
      IF (mi0(iiseed) == mi1(iiseed) .AND. mj0(ijseed) == mj1(ijseed)) zchk = tmask(mi0(iiseed),mj0(ijseed),1)
      CALL mpp_max('domisf',zchk)
      IF (zchk(1) == 0._wp) CALL ctl_stop( 'STOP', 'open sea seed is on land, please update namelist (rn_lon_opnsea,rn_lat_opnsea)' )
      !
   END SUBROUTINE zgr_isf_subgl


   SUBROUTINE zps_isf
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE zps_isf  ***
      !!   
      !! ** Purpose : compute top partial cell
      !!   
      !! ** Method  : same method as for the bottom adapted for the top
      !!   
      !!----------------------------------------------------------------------
      !!
      INTEGER :: jj, ji   ! loop indices
      INTEGER :: ik       ! local top level
      INTEGER :: it       ! counter index
      !
      REAL(wp) :: zdiff
      !!----------------------------------------------------------------------
      !
      ! compute top partial cell
      DO jj = 1, jpj
         DO ji = 1, jpi
            ik = misfdep(ji,jj)
            IF( ik > 1 ) THEN               ! ice shelf point only 
               IF( risfdep(ji,jj) < gdepw_1d(ik) )  risfdep(ji,jj)= gdepw_1d(ik)
               gdepw_0(ji,jj,ik) = risfdep(ji,jj)
!gm Bu check the gdepw_0 
            !       ... on ik 
               gdept_0(ji,jj,ik) = gdepw_1d(ik+1) - ( gdepw_1d(ik+1) - gdepw_0(ji,jj,ik) )   &
                  &                               * ( gdepw_1d(ik+1) - gdept_1d(ik)      )   &
                  &                               / ( gdepw_1d(ik+1) - gdepw_1d(ik)      )
               e3t_0  (ji,jj,ik  ) = gdepw_1d(ik+1) - gdepw_0(ji,jj,ik)
               e3w_0  (ji,jj,ik+1) = gdept_1d(ik+1) - gdept_0(ji,jj,ik)

               IF( ik + 1 == mbathy(ji,jj) ) THEN               ! ice shelf point only (2 cell water column) 
                  e3w_0  (ji,jj,ik+1) = gdept_0(ji,jj,ik+1) - gdept_0(ji,jj,ik)
               ENDIF
         !       ... on ik / ik-1 
               e3w_0  (ji,jj,ik  ) = e3t_0  (ji,jj,ik) !2._wp * (gdept_0(ji,jj,ik) - gdepw_0(ji,jj,ik)) 
               e3t_0  (ji,jj,ik-1) = gdepw_0(ji,jj,ik) - gdepw_1d(ik-1)
               gdept_0(ji,jj,ik-1) = gdept_0(ji,jj,ik) - e3w_0(ji,jj,ik)
               gdepw_0(ji,jj,ik-1) = gdepw_0(ji,jj,ik) - e3t_0(ji,jj,ik-1)
               IF ( ik > 2 ) THEN
                  e3w_0  (ji,jj,ik-1) = gdept_0(ji,jj,ik-1) - gdept_0(ji,jj,ik-2)
               ELSE
                  e3w_0  (ji,jj,ik-1) = 2.0 * gdept_0(ji,jj,ik-1)
               END IF
            ENDIF
         END DO
      END DO
      !
      it = 0
      DO jj = 1, jpj
         DO ji = 1, jpi
            ik = misfdep(ji,jj)
            IF( ik > 1 ) THEN               ! ice shelf point only 
               e3tp (ji,jj) = e3t_0(ji,jj,ik  )
               e3wp (ji,jj) = e3w_0(ji,jj,ik+1 )
            ! test 
               zdiff= gdept_0(ji,jj,ik) - gdepw_0(ji,jj,ik  )
               IF( zdiff <= 0. .AND. lwp ) THEN
                  it = it + 1
                  WRITE(numout,*) ' it      = ', it, ' ik      = ', ik, ' (i,j) = ', ji, jj
                  WRITE(numout,*) ' risfdep = ', risfdep(ji,jj)
                  WRITE(numout,*) ' gdept = ', gdept_0(ji,jj,ik), ' gdepw = ', gdepw_0(ji,jj,ik+1), ' zdiff = ', zdiff
                  WRITE(numout,*) ' e3tp  = ', e3tp(ji,jj), ' e3wp  = ', e3wp(ji,jj)
               ENDIF
            ENDIF
         END DO
      END DO
      !
   END SUBROUTINE zps_isf

   SUBROUTINE zps_isf_e3uv_w
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE zps_isf  ***
      !!   
      !! ** Purpose :   correct e3uw and e3vw for case with 2 wet cell in the water column under an ice shelf
      !!   
      !!----------------------------------------------------------------------
      !!
      INTEGER :: ji , jj   ! loop indices
      INTEGER :: ikt, ikb  ! local top/bottom level
      !!----------------------------------------------------------------------
      !
      ! define e3uw (adapted for 2 cells in the water column)
      DO jj = 2, jpjm1
         DO ji = 2, jpim1   ! vector opt. 
            ikb = MAX(mbathy (ji,jj),mbathy (ji+1,jj))
            ikt = MAX(misfdep(ji,jj),misfdep(ji+1,jj))
            IF (ikb == ikt+1) e3uw_0(ji,jj,ikb) =  MIN( gdept_0(ji,jj,ikb  ), gdept_0(ji+1,jj  ,ikb  ) ) &
                                    &            - MAX( gdept_0(ji,jj,ikb-1), gdept_0(ji+1,jj  ,ikb-1) )
            ikb = MAX(mbathy (ji,jj),mbathy (ji,jj+1))
            ikt = MAX(misfdep(ji,jj),misfdep(ji,jj+1))
            IF (ikb == ikt+1) e3vw_0(ji,jj,ikb) =  MIN( gdept_0(ji,jj,ikb  ), gdept_0(ji  ,jj+1,ikb  ) ) &
                                    &            - MAX( gdept_0(ji,jj,ikb-1), gdept_0(ji  ,jj+1,ikb-1) )
         END DO
      END DO
      !
   END SUBROUTINE zps_isf_e3uv_w

END MODULE domisf
