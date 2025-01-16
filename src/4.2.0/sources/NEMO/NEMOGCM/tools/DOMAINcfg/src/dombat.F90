MODULE dombat

   USE dom_oce           ! ocean domain
   !
   USE in_out_manager    ! I/O manager
   USE iom               ! I/O library
   USE lbclnk            ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp           ! distributed memory computing library
#if defined key_agrif
   USE agrif_modutil
   USE agrif_parameters
#endif   
   USE bilinear_interp
   USE dom_oce

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dom_bat        ! called by dom_zgr.F90
#if defined key_agrif
   PUBLIC   remove_closedseas        ! called by dom_zgr.F90

#endif   


CONTAINS

   SUBROUTINE dom_bat

      INTEGER :: inum, id, ji, jj,ji1,jj1
      INTEGER :: iimin,iimax,jjmin,jjmax
      INTEGER :: tabdim1, tabdim2, nxhr, nyhr, nxyhr
      INTEGER, DIMENSION(2) :: ddims
      INTEGER, DIMENSION(3) :: status
      INTEGER, DIMENSION(1) :: i_min,i_max
      INTEGER, DIMENSION(1) :: j_min,j_max
      INTEGER, DIMENSION(:)  ,ALLOCATABLE     ::   src_add,dst_add 
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: trouble_points , vardep,mask_oce
      REAL(wp) ,DIMENSION(jpi,jpj):: Cell_lonmin, Cell_lonmax, Cell_latmin, Cell_latmax
      REAL(wp) ::zdel
      REAL(wp), DIMENSION(:)  , ALLOCATABLE ::   lon_new1D , lat_new1D, vardep1d
      REAL(wp), DIMENSION(:,:), ALLOCATABLE ::   coarselon, coarselat, coarsebathy, bathy_test
      REAL(wp), DIMENSION(:,:),ALLOCATABLE     ::   matrix,interpdata 
      LOGICAL :: lonlat_2D, ln_pacifiq
      LOGICAL :: identical_grids
      LOGICAL, DIMENSION(:,:), ALLOCATABLE     ::   masksrc
      REAL(wp), DIMENSION(jpi,jpj) :: zglamt, zgphi, zglamu, zglamv, zglamf
      REAL(wp) :: zshift
 
      CHARACTER(32) :: bathyfile, bathyname, lonname,latname       

      bathyfile=TRIM(cn_topo)
      bathyname=TRIM(cn_bath)
      lonname=TRIM(cn_lon)
      latname=TRIM(cn_lat)
   
      CALL iom_open( bathyfile, inum, ldiof=.TRUE. )
      
      ! check if lon/lat are 2D arrays
      id = iom_varid( inum, lonname, ddims )
      IF (ddims(2)==0) THEN
         lonlat_2D = .FALSE.
      ELSE
         lonlat_2D = .TRUE.
      ENDIF   
      
      id = iom_varid( inum, bathyname, ddims )
      ln_pacifiq = .FALSE.
      zglamt(:,:) = glamt(:,:)
      zglamu(:,:) = glamu(:,:)
      zglamv(:,:) = glamv(:,:)
      zglamf(:,:) = glamf(:,:)

      IF( glamt(1,1) .GT. glamt(jpi,jpj) ) ln_pacifiq =.false. 

      zshift = 0.
      IF( ln_pacifiq  ) THEN
         zshift = 0.!Abs(minval(glamt)) +0.1 
         WHERE ( glamt < 0 )
            zglamt = zglamt + zshift + 360.
         END WHERE
         WHERE ( glamu < 0 )
            zglamu = zglamu + zshift +360.
         END WHERE
         WHERE ( glamv < 0 )
            zglamv = zglamv + zshift +360.
         END WHERE
         WHERE ( glamf < 0 )
            zglamf = zglamf + zshift +360.
         END WHERE
      ENDIF

      status=-1

      IF (lonlat_2D) THEN
         ! here implicitly it's old topo database (orca format)
         ALLOCATE(coarselon  (ddims(1),ddims(2)), STAT=status(1)) 
         ALLOCATE(coarselat  (ddims(1),ddims(2)), STAT=status(2)) 
         ALLOCATE(coarsebathy(ddims(1),ddims(2)), STAT=status(3)) 
         IF( sum(status) /= 0 )   CALL ctl_stop( 'STOP', 'dom_bat : unable to allocate arrays' )
         CALL iom_get  ( inum, jpdom_unknown, lonname, coarselon )
         CALL iom_get  ( inum, jpdom_unknown, latname, coarselat )
         CALL iom_get  ( inum, jpdom_unknown, bathyname, coarsebathy )
         CALL iom_close (inum)
         IF( ln_pacifiq ) THEN
       !     WHERE(coarselon < 0.00001) 
                coarselon = Coarselon + zshift
       !      END WHERE
         ENDIF     
         ! equivalent to new database
      ELSE
         ALLOCATE(lon_new1D(ddims(1)), lat_new1D(ddims(2)))
         CALL iom_get  ( inum, jpdom_unknown, lonname, lon_new1D )
         CALL iom_get  ( inum, jpdom_unknown, latname, lat_new1D )
         IF( ln_pacifiq ) THEN
            WHERE(lon_new1D < 0.00001) 
                lon_new1D = lon_new1D +360.!zshift
             END WHERE
         ENDIF                
         zdel =  0.00   
         IF( MAXVAL(zglamf) > 180 + zshift ) THEN  
            !          
         !   WHERE( lon_new1D < 0 )
         !       lon_new1D = lon_new1D + 360.
         !   END WHERE
            !     
            i_min = MAXLOC(lon_new1D,mask = lon_new1D < MINVAL(zglamf(1:jpi-1,1:jpj-1)) )
            i_max = MINLOC(lon_new1D,mask = lon_new1D > MAXVAL(zglamf(1:jpi-1,1:jpj-1)) )                   
            j_min = MAXLOC(lat_new1D,mask = lat_new1D < MINVAL( gphif(1:jpi-1,1:jpj-1)) )
            j_max = MINLOC(lat_new1D,mask = lat_new1D > MAXVAL( gphif(1:jpi-1,1:jpj-1)) )
            !
            tabdim1 = ( SIZE(lon_new1D) - i_min(1) + 1 ) + i_max(1)                   
            !          
            IF(j_min(1)-2 >= 1 .AND. j_max(1)+3 <= SIZE(lat_new1D,1) ) THEN
               j_min(1) = j_min(1) - 2
               j_max(1) = j_max(1)+ 3
            ENDIF
            tabdim2 = j_max(1) - j_min(1) + 1
            !
            ALLOCATE(coarselon  (tabdim1,tabdim2), STAT=status(1))
            ALLOCATE(coarselat  (tabdim1,tabdim2), STAT=status(2))
            ALLOCATE(Coarsebathy(tabdim1,tabdim2), STAT=status(3)) 

            IF( SUM(status) /= 0 ) CALL ctl_stop( 'STOP', 'dom_bat : unable to allocate arrays' )          
            !
            DO ji = 1,tabdim1
               coarselat(ji,:) = lat_new1D(j_min(1):j_max(1))
            END DO
           
            !
            DO jj = 1, tabdim2                                 
               coarselon(1:SIZE(lon_new1D)-i_min(1)+1      ,jj) = lon_new1D(i_min(1):SIZE(lon_new1D))
               coarselon(2+SIZE(lon_new1D)-i_min(1):tabdim1,jj) = lon_new1D(1:i_max(1))   
            END DO
            ! 
            CALL iom_get(inum, jpdom_unknown, bathyname,coarsebathy(1:SIZE(lon_new1D)-i_min(1)+1,:), &
            kstart=(/i_min(1),j_min(1)/), kcount=(/SIZE(lon_new1D)-i_min(1)+1,tabdim2/))   ! +1?
            !
            CALL iom_get(inum, jpdom_unknown, bathyname,coarsebathy(2+SIZE(lon_new1D)-i_min(1):tabdim1,:), &
            kstart=(/1,j_min(1)/),kcount=(/i_max(1),tabdim2/))                
            !
         ELSE
          !  WHERE( lon_new1D > (180. + zshift) )   lon_new1D = lon_new1D - 360.
            i_min = MAXLOC(lon_new1D,mask = lon_new1D < MINVAL(zglamf)-zdel)
            i_max = MINLOC(lon_new1D,mask = lon_new1D > MAXVAL(zglamf)+zdel)
            j_min = MAXLOC(lat_new1D,mask = lat_new1D < MINVAL(gphif)-zdel)
            j_max = MINLOC(lat_new1D,mask = lat_new1D > MAXVAL(gphif)+zdel)

            ! jc: to prevent from issues with grids that exactly pass through
            ! longitude= +-180: 
            IF (i_max(1)==0) i_max(1)=ddims(1)
         
            i_min(1)=max(i_min(1),1)
            !      
            IF(i_min(1)-2 >= 1 .AND. i_max(1)+3 <= SIZE(lon_new1D,1) ) THEN
               i_min(1) = i_min(1)-2
               i_max(1) = i_max(1)+3
            ENDIF
            tabdim1 = i_max(1) - i_min(1) + 1
            !
            IF(j_min(1)-2 >= 1 .AND. j_max(1)+3 <= SIZE(lat_new1D,1) ) THEN
               j_min(1) = j_min(1)-2
               j_max(1) = j_max(1)+3
            ENDIF
            tabdim2 = j_max(1) - j_min(1) + 1
            !
            ALLOCATE(coarselon  (tabdim1,tabdim2), STAT=status(1)) 
            ALLOCATE(coarselat  (tabdim1,tabdim2), STAT=status(2)) 
            ALLOCATE(coarsebathy(tabdim1,tabdim2), STAT=status(3)) 

            IF( SUM(status) /= 0 ) CALL ctl_stop( 'STOP', 'dom_bat : unable to allocate arrays' )  
            !
            DO jj = 1,tabdim2
               coarselon(:,jj) = lon_new1D(i_min(1):i_max(1))
            END DO
            !      
            DO ji = 1,tabdim1
              coarselat(ji,:) = lat_new1D(j_min(1):j_max(1)) 
            END DO
            !
            CALL iom_get(inum,jpdom_unknown,bathyname,coarsebathy, &
            &                  kstart=(/i_min(1),j_min(1)/),kcount=(/tabdim1,tabdim2/))
            !
         ENDIF   ! > 180
    
         DEALLOCATE(lon_new1D) ; DEALLOCATE(lat_new1D)
         CALL iom_close (inum)
         
         coarsebathy = coarsebathy *rn_scale
        ! reset land to 0 
         WHERE (coarsebathy < 0.)        
          coarsebathy=0.
         ENDWHERE 
 
      ENDIF   ! external

      IF(lwp) THEN
         WRITE(numout,*) 'Interpolation of high resolution bathymetry on child grid'
         IF( nn_interp == 0 ) THEN
            WRITE(numout,*) 'Arithmetic average ...'
         ELSE IF( nn_interp == 1 ) THEN
            WRITE(numout,*) 'Median average ...'
         ELSE IF( nn_interp == 2 ) THEN     
            WRITE(numout,*) 'Bilinear interpolation ...'
         ELSE     
            WRITE(*,*) 'bad value for nn_interp variable ( must be 0,1 or 2 )'
            STOP 
         ENDIF
      ENDIF  
      bathy(:,:) = 0.
      !
      !------------------------------------
      !MEDIAN AVERAGE or ARITHMETIC AVERAGE
      !------------------------------------
      !
      IF( nn_interp == 0 .OR. nn_interp == 1 ) THEN 
         !
         ALLOCATE(trouble_points(jpi,jpj))
         trouble_points = 0
         !
         !  POINT DETECTION
         !
         !                       
         DO jj = 1,jpj
            DO ji = 1,jpi
               !     
               ! FINE GRID CELLS DEFINITION  
               ji1=max(ji-1,1)
               jj1=max(jj-1,1)             
             
               !
               Cell_lonmin(ji,jj) = MIN(zglamf(ji1,jj1),zglamf(ji,jj1),zglamf(ji,jj),zglamf(ji1,jj))
               Cell_lonmax(ji,jj) = MAX(zglamf(ji1,jj1),zglamf(ji,jj1),zglamf(ji,jj),zglamf(ji1,jj))
               Cell_latmin(ji,jj) = MIN(gphif(ji1,jj1),gphif(ji,jj1),gphif(ji,jj),gphif(ji1,jj))
               Cell_latmax(ji,jj) = MAX(gphif(ji1,jj1),gphif(ji,jj1),gphif(ji,jj),gphif(ji1,jj))
               IF( ABS(Cell_lonmax(ji,jj) - Cell_lonmin(ji,jj) ) > 180 ) THEN
                    zdel = Cell_lonmax(ji,jj)
                    Cell_lonmax(ji,jj) = Cell_lonmin(ji,jj)
                    Cell_lonmin(ji,jj) = zdel-360
               ENDIF
               !               
               ! SEARCH FOR ALL POINTS CONTAINED IN THIS CELL
               !    
     !      ENDDO
     !    ENDDO   
      !   CALL lbc_lnk( 'dom_bat', Cell_lonmin, 'T', 1. )
      !   CALL lbc_lnk( 'dom_bat', Cell_lonmax, 'T', 1. )
      !   CALL lbc_lnk( 'dom_bat', Cell_latmin, 'T', 1. )
      !   CALL lbc_lnk( 'dom_bat', Cell_latmax, 'T', 1. )


      !   DO jj = 2,jpj
      !      DO ji = 2,jpi
               iimin = 1
               DO WHILE( coarselon(iimin,1) < Cell_lonmin(ji,jj) ) 
                  iimin = iimin + 1
             !     IF ( iimin .LE. 1 ) THEN
             !     iimin = 1
             !     EXIT
             !     ENDIF
               ENDDO
               !               
               jjmin = 1
               DO WHILE( coarselat(iimin,jjmin) < Cell_latmin(ji,jj) ) 
                  jjmin = jjmin + 1
             !     IF ( iimin .LE. 1 ) THEN
             !     iimin = 1
             !     EXIT
             !     ENDIF
               ENDDO
               jjmin=max(1,jjmin)
               !                
               iimax = iimin 
               DO WHILE( coarselon(iimax,1)<= Cell_lonmax(ji,jj) ) 
                  iimax = iimax + 1
                  IF ( iimax .GE. SIZE(coarsebathy,1) ) THEN
                  iimax = MIN( iimax,SIZE(coarsebathy,1))
                  EXIT
                ENDIF
               ENDDO
               !                               
               jjmax = jjmin 
               DO WHILE( coarselat(iimax,jjmax) <= Cell_latmax(ji,jj) ) 
                  jjmax = jjmax + 1
                  IF ( jjmax .GE. SIZE(coarsebathy,2) ) THEN
                  jjmax = MIN( jjmax,SIZE(coarsebathy,2))
                  EXIT
                ENDIF
               ENDDO
               !
            !   iimax = iimax-1
            !   jjmax = jjmax-1
               !               
               iimin = MAX( iimin,1 )
               jjmin = MAX( jjmin,1 )
               iimax = MIN( iimax,SIZE(coarsebathy,1))
               jjmax = MIN( jjmax,SIZE(coarsebathy,2))

               nxhr = iimax - iimin + 1
               nyhr = jjmax - jjmin + 1   


  
               IF( nxhr == 0 .OR. nyhr == 0 ) THEN
                  trouble_points(ji,jj) = 1
               ELSE
                  ALLOCATE( vardep(nxhr,nyhr) )
                  ALLOCATE( mask_oce(nxhr,nyhr) )
                  mask_oce = 0       
                  vardep(:,:) = coarsebathy(iimin:iimax,jjmin:jjmax)
                  WHERE( vardep(:,:) .GT. 0. ) 
                     mask_oce = 1
                  ENDWHERE
                  nxyhr = nxhr*nyhr
!                 IF( SUM(mask_oce) == 0 ) THEN
                   IF( SUM(mask_oce) < 0.5*(nxhr*nyhr) ) THEN
                     bathy(ji,jj) = 0.
                   ELSE
                     IF( nn_interp == 0 ) THEN
                        ! Arithmetic average                   
                        bathy(ji,jj) = SUM (vardep(:,:)*mask_oce(:,:))/SUM(mask_oce)
                     ELSE
                        ! Median average       
                        !
                        ALLOCATE(vardep1d(nxhr*nyhr))
                        vardep1d = RESHAPE(vardep,(/ nxhr*nyhr /) )
                       ! CALL ssort(vardep1d,nxhr*nyhr)
                        CALL quicksort(vardep1d,1,nxhr*nyhr)
                        !
                        ! Calculate median
                        !
                        IF (MOD(SUM(mask_oce),2) .NE. 0) THEN
                           bathy(ji,jj) = vardep1d( nxyhr/2 + 1 )
                        ELSE
                           bathy(ji,jj) =0.5 * ( vardep1d(nxyhr/2) + vardep1d(nxyhr/2+1) )
                        END IF
                        DEALLOCATE(vardep1d)   
                     ENDIF
                  ENDIF
                  DEALLOCATE (mask_oce,vardep)
               ENDIF
            ENDDO
         ENDDO

         IF( SUM( trouble_points ) > 0 ) THEN
            CALL ctl_warn ('too much empty cells, proceed to bilinear interpolation')
            nn_interp = 2
         ENDIF
      ENDIF

     !
     ! create logical array masksrc
     !
      IF( nn_interp == 2) THEN 
         !
         identical_grids = .FALSE.

# ifdef key_agrif
         IF( Agrif_Parent(jpi) == jpi  .AND.   &
             Agrif_Parent(jpj) == jpj  ) THEN
            IF( MAXVAL( ABS(coarselat(:,:)- gphit(:,:)) ) < 0.0001 .AND.   &
                 MAXVAL( ABS(coarselon(:,:)- gphit(:,:)) ) < 0.0001 ) THEN
               bathy(:,:)  = coarsebathy(:,:) 
                IF(lwp) WRITE(numout,*) 'same grid between ', bathyname,' and child domain'                   
               identical_grids = .TRUE.                          
            ENDIF
         ENDIF
# endif
         IF( .NOT. identical_grids ) THEN  
            ALLOCATE(masksrc(SIZE(coarsebathy,1),SIZE(coarsebathy,2)))
            ALLOCATE(bathy_test(jpi,jpj))
            !
            !                    Where(G0%bathy_meter.le.0.00001) 
            !                  masksrc = .false.
            !              ElseWhere
            !
            masksrc = .TRUE.
            !
            !              End where                       
            !            
            ! compute remapping matrix thanks to SCRIP package            
            !                                  
            CALL get_remap_matrix(coarselat,gphit,   &
                 coarselon,zglamt,   &
                 masksrc,matrix,src_add,dst_add)
            CALL make_remap(coarsebathy,bathy_test,jpi,jpj, &
                 matrix,src_add,dst_add)  
            !                                  
            bathy= bathy_test               
            !            
            DEALLOCATE(masksrc)
            DEALLOCATE(bathy_test) 
         ENDIF
        !            
      ENDIF
      CALL lbc_lnk( 'dom_bat', bathy, 'T', 1.,kfillmode = jpfillcopy)

!#if defined key_agrif
!      IF (ln_remove_closedseas.AND.(.NOT.Agrif_Root())) THEN
!         CALL remove_closedseas
!      ENDIF
!#endif


       ! Correct South and North
! #if defined key_agrif
!       IF( lk_south ) THEN  
!          IF( (nbondj == -1).OR.(nbondj == 2) ) THEN
!            bathy(:,1)=bathy(:,2)
!          ENDIF
!       ELSE
!             bathy(:,1) = 0.
!       ENDIF
! #else
!       IF ((nbondj == -1).OR.(nbondj == 2)) THEN
!             bathy(:,1)=bathy(:,2)
!       ENDIF
! #endif

!       IF ((nbondj == 1).OR.(nbondj == 2)) THEN
!          bathy(:,jpj)=bathy(:,jpj-1)
!       ENDIF

!       ! Correct West and East
!       IF (jperio /=1) THEN
!          IF ((nbondi == -1).OR.(nbondi == 2)) THEN
!             bathy(1,:)=bathy(2,:)
!          ENDIF
!          IF ((nbondi == 1).OR.(nbondi == 2)) THEN
!          bathy(jpi,:)=bathy(jpi-1,:)
!          ENDIF
!       ENDIF


   END SUBROUTINE dom_bat

#if defined key_agrif
   SUBROUTINE remove_closedseas

      INTEGER :: ji, jj
      INTEGER :: nbadd, istart, iend, jstart, jend
      REAL(wp), DIMENSION(:,:), ALLOCATABLE :: bathy_test


      ALLOCATE(bathy_test(jpi,jpj))
      bathy_test(:,:) = 0._wp 
      !
      ! --- West --- !
      IF(lk_west) THEN
         istart = nn_hls + 2
         iend   = nn_hls + nbghostcells 
         DO ji = mi0(istart), mi1(iend)
            DO jj = 1, jpj
               IF ( bathy (ji,jj)/=0._wp )   bathy_test(ji,jj) = 1._wp
            END DO
         END DO
      ENDIF
      !
      ! --- East --- !
      IF(lk_east) THEN
         istart = jpiglo - ( nn_hls + nbghostcells -1 ) 
         iend   = jpiglo - ( nn_hls + 1 )
         DO ji = mi0(istart), mi1(iend)
            DO jj = 1, jpj
               IF ( bathy (ji,jj)/=0._wp )   bathy_test(ji,jj) = 1._wp
            END DO
         END DO
      ENDIF
      !
      ! --- South --- !
      IF(lk_south) THEN
         jstart = nn_hls + 2
         jend   = nn_hls + nbghostcells
         DO jj = mj0(jstart), mj1(jend)
            DO ji = 1, jpi
               IF ( bathy (ji,jj)/=0._wp )   bathy_test(ji,jj) = 1._wp
            END DO
         END DO
      ENDIF
      !
      ! --- North --- !
      IF(lk_north) THEN
         jstart = jpjglo - ( nn_hls + nbghostcells -1 )
         jend   = jpjglo - ( nn_hls + 1 )
         DO jj = mj0(jstart), mj1(jend)
            DO ji = 1, jpi
               IF ( bathy (ji,jj)/=0._wp )  bathy_test(ji,jj) = 1._wp
            END DO
         END DO
      ENDIF
         
      nbadd = 1
      DO WHILE ( nbadd/=0 )
         nbadd = 0
         DO ji = 1+nn_hls, jpi-nn_hls
            DO jj = 1+nn_hls, jpj-nn_hls           
               IF (bathy(ji,jj) > 0._wp) THEN
                  IF (MAX(bathy_test(ji,jj+1),bathy_test(ji,jj-1), & 
                  &       bathy_test(ji-1,jj),bathy_test(ji+1,jj))==1._wp)  THEN
                     IF (bathy_test(ji,jj)/=1._wp) nbadd = nbadd + 1
                     bathy_test(ji,jj)=1._wp
                  ENDIF
               ENDIF
            END DO
         END DO
         IF( lk_mpp )   CALL mpp_sum('remove_closedseas', nbadd )
         CALL lbc_lnk( 'dom_bat', bathy_test, 'T', 1.,kfillmode = jpfillcopy)

      END DO

      WHERE(bathy_test==0._wp) bathy = 0._wp
      DEALLOCATE(bathy_test)

   END SUBROUTINE remove_closedseas
#endif

END MODULE dombat
