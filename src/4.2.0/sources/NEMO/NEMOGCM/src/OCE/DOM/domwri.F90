MODULE domwri
   !!======================================================================
   !!                       ***  MODULE domwri  ***
   !! Ocean initialization : write the ocean domain mesh file(s)
   !!======================================================================
   !! History :  OPA  ! 1997-02  (G. Madec)  Original code
   !!            8.1  ! 1999-11  (M. Imbard)  NetCDF FORMAT with IOIPSL
   !!   NEMO     1.0  ! 2002-08  (G. Madec)  F90 and several file
   !!            3.0  ! 2008-01  (S. Masson)  add dom_uniq 
   !!            4.0  ! 2016-01  (G. Madec)  simplified mesh_mask.nc file
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dom_wri        : create and write mesh and mask file(s)
   !!   dom_stiff      : diagnose maximum grid stiffness/hydrostatic consistency (s-coordinate)
   !!----------------------------------------------------------------------
   !
   USE dom_oce         ! ocean space and time domain
   USE domutl          ! 
   USE phycst ,   ONLY :   rsmall
   USE wet_dry,   ONLY :   ll_wd  ! Wetting and drying
   !
   USE in_out_manager  ! I/O manager
   USE iom             ! I/O library
   USE lbclnk          ! lateral boundary conditions - mpp exchanges
   USE lib_mpp         ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dom_wri              ! routine called by inidom.F90
   PUBLIC   dom_stiff            ! routine called by inidom.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: domwri.F90 15033 2021-06-21 10:24:45Z smasson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dom_wri
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dom_wri  ***
      !!                   
      !! ** Purpose :   Create the NetCDF file(s) which contain(s) all the
      !!      ocean domain informations (mesh and mask arrays). This (these)
      !!      file(s) is (are) used for visualisation (SAXO software) and
      !!      diagnostic computation.
      !!
      !! ** Method  :   create a file with all domain related arrays
      !!
      !! ** output file :   meshmask.nc  : domain size, horizontal grid-point position,
      !!                                   masks, depth and vertical scale factors
      !!----------------------------------------------------------------------
      INTEGER           ::   inum    ! temprary units for 'mesh_mask.nc' file
      CHARACTER(len=21) ::   clnam   ! filename (mesh and mask informations)
      INTEGER           ::   ji, jj, jk   ! dummy loop indices
      REAL(wp), DIMENSION(jpi,jpj)     ::   zprt, zprw     ! 2D workspace
      REAL(wp), DIMENSION(jpi,jpj,jpk) ::   zdepu, zdepv   ! 3D workspace
      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'dom_wri : create NetCDF mesh and mask information file(s)'
      IF(lwp) WRITE(numout,*) '~~~~~~~'
      
      clnam = 'mesh_mask'  ! filename (mesh and mask informations)
      
      !                                  ! ============================
      !                                  !  create 'mesh_mask.nc' file
      !                                  ! ============================
      CALL iom_open( TRIM(clnam), inum, ldwrt = .TRUE. )
      !                                                         ! Configuration specificities
      CALL iom_putatt( inum,  'CfgName', TRIM(cn_cfg) )
      CALL iom_putatt( inum, 'CfgIndex',      nn_cfg  )
      !                                                         ! lateral boundary of the global domain
      CALL iom_putatt( inum,   'Iperio', COUNT( (/l_Iperio/) ) )
      CALL iom_putatt( inum,   'Jperio', COUNT( (/l_Jperio/) ) )
      CALL iom_putatt( inum,    'NFold', COUNT( (/l_NFold /) ) )
      CALL iom_putatt( inum,   'NFtype',          c_NFtype     )
      !                                                         ! type of vertical coordinate
      IF(ln_zco)   CALL iom_putatt( inum, 'VertCoord', 'zco' )
      IF(ln_zps)   CALL iom_putatt( inum, 'VertCoord', 'zps' )
      IF(ln_sco)   CALL iom_putatt( inum, 'VertCoord', 'sco' )
      !                                                         ! ocean cavities under iceshelves
      CALL iom_putatt( inum,   'IsfCav', COUNT( (/ln_isfcav/) ) )  
      !                                                         ! masks
      CALL iom_rstput( 0, 0, inum, 'tmask', tmask, ktype = jp_i1 )     !    ! land-sea mask
      CALL iom_rstput( 0, 0, inum, 'umask', umask, ktype = jp_i1 )
      CALL iom_rstput( 0, 0, inum, 'vmask', vmask, ktype = jp_i1 )
      CALL iom_rstput( 0, 0, inum, 'fmask', fmask, ktype = jp_i1 )
      
      CALL dom_uniq( zprw, 'T' )
      DO_2D( 1, 1, 1, 1 )
         zprt(ji,jj) = ssmask(ji,jj) * zprw(ji,jj)                        !    ! unique point mask
      END_2D
      CALL iom_rstput( 0, 0, inum, 'tmaskutil', zprt, ktype = jp_i1 )  
      CALL dom_uniq( zprw, 'U' )
      DO_2D( 1, 1, 1, 1 )
         zprt(ji,jj) = ssumask(ji,jj) * zprw(ji,jj)                        !    ! unique point mask
      END_2D
      CALL iom_rstput( 0, 0, inum, 'umaskutil', zprt, ktype = jp_i1 )  
      CALL dom_uniq( zprw, 'V' )
      DO_2D( 1, 1, 1, 1 )
         zprt(ji,jj) = ssvmask(ji,jj) * zprw(ji,jj)                        !    ! unique point mask
      END_2D
      CALL iom_rstput( 0, 0, inum, 'vmaskutil', zprt, ktype = jp_i1 )  
!!gm  ssfmask has been removed  ==>> find another solution to defined fmaskutil
!!    Here we just remove the output of fmaskutil.
!      CALL dom_uniq( zprw, 'F' )
!      DO jj = 1, jpj
!         DO ji = 1, jpi
!            zprt(ji,jj) = ssfmask(ji,jj) * zprw(ji,jj)                        !    ! unique point mask
!         END DO
!      END DO
!      CALL iom_rstput( 0, 0, inum, 'fmaskutil', zprt, ktype = jp_i1 )  
!!gm

      !                                                         ! horizontal mesh (inum3)
      CALL iom_rstput( 0, 0, inum, 'glamt', glamt, ktype = jp_r8 )     !    ! latitude
      CALL iom_rstput( 0, 0, inum, 'glamu', glamu, ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'glamv', glamv, ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'glamf', glamf, ktype = jp_r8 )
      
      CALL iom_rstput( 0, 0, inum, 'gphit', gphit, ktype = jp_r8 )     !    ! longitude
      CALL iom_rstput( 0, 0, inum, 'gphiu', gphiu, ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'gphiv', gphiv, ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'gphif', gphif, ktype = jp_r8 )
      
      CALL iom_rstput( 0, 0, inum, 'e1t', e1t, ktype = jp_r8 )         !    ! e1 scale factors
      CALL iom_rstput( 0, 0, inum, 'e1u', e1u, ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'e1v', e1v, ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'e1f', e1f, ktype = jp_r8 )
      
      CALL iom_rstput( 0, 0, inum, 'e2t', e2t, ktype = jp_r8 )         !    ! e2 scale factors
      CALL iom_rstput( 0, 0, inum, 'e2u', e2u, ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'e2v', e2v, ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'e2f', e2f, ktype = jp_r8 )
      
      CALL iom_rstput( 0, 0, inum, 'ff_f', ff_f, ktype = jp_r8 )       !    ! coriolis factor
      CALL iom_rstput( 0, 0, inum, 'ff_t', ff_t, ktype = jp_r8 )
      
      ! note that mbkt is set to 1 over land ==> use surface tmask
      zprt(:,:) = REAL( mbkt(:,:) , wp )
      CALL iom_rstput( 0, 0, inum, 'mbathy', zprt, ktype = jp_i4 )     !    ! nb of ocean T-points
      zprt(:,:) = REAL( mikt(:,:) , wp )
      CALL iom_rstput( 0, 0, inum, 'misf', zprt, ktype = jp_i4 )       !    ! nb of ocean T-points
      !															             ! vertical mesh
      CALL iom_rstput( 0, 0, inum, 'e3t_1d', e3t_1d, ktype = jp_r8  )    !    ! scale factors
      CALL iom_rstput( 0, 0, inum, 'e3w_1d', e3w_1d, ktype = jp_r8  )
      
      CALL iom_rstput( 0, 0, inum, 'e3t_0' , e3t_0 , ktype = jp_r8  )
      CALL iom_rstput( 0, 0, inum, 'e3u_0' , e3u_0 , ktype = jp_r8  )
      CALL iom_rstput( 0, 0, inum, 'e3v_0' , e3v_0 , ktype = jp_r8  )
      CALL iom_rstput( 0, 0, inum, 'e3f_0' , e3f_0 , ktype = jp_r8  )
      CALL iom_rstput( 0, 0, inum, 'e3w_0' , e3w_0 , ktype = jp_r8  )
      CALL iom_rstput( 0, 0, inum, 'e3uw_0', e3uw_0, ktype = jp_r8  )
      CALL iom_rstput( 0, 0, inum, 'e3vw_0', e3vw_0, ktype = jp_r8  )
      !
      CALL iom_rstput( 0, 0, inum, 'gdept_1d' , gdept_1d , ktype = jp_r8 )  ! stretched system
      CALL iom_rstput( 0, 0, inum, 'gdepw_1d' , gdepw_1d , ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'gdept_0'  , gdept_0  , ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum, 'gdepw_0'  , gdepw_0  , ktype = jp_r8 )
      !
      IF( ln_sco ) THEN                                         ! s-coordinate stiffness
         CALL dom_stiff( zprt )
         CALL iom_rstput( 0, 0, inum, 'stiffness', zprt )       ! Max. grid stiffness ratio
      ENDIF
      !
      IF( ll_wd ) CALL iom_rstput( 0, 0, inum, 'ht_0'   , ht_0   , ktype = jp_r8 )

      !                                     ! ============================
      CALL iom_close( inum )                !        close the files 
      !                                     ! ============================
   END SUBROUTINE dom_wri


   SUBROUTINE dom_stiff( px1 )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dom_stiff  ***
      !!                     
      !! ** Purpose :   Diagnose maximum grid stiffness/hydrostatic consistency
      !!
      !! ** Method  :   Compute Haney (1991) hydrostatic condition ratio
      !!                Save the maximum in the vertical direction
      !!                (this number is only relevant in s-coordinates)
      !!
      !!                Haney, 1991, J. Phys. Oceanogr., 21, 610-619.
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:), INTENT(out), OPTIONAL ::   px1   ! stiffness
      !
      INTEGER  ::   ji, jj, jk 
      REAL(wp) ::   zrxmax
      REAL(wp), DIMENSION(4) ::   zr1
      REAL(wp), DIMENSION(jpi,jpj) ::   zx1
      !!----------------------------------------------------------------------
      zx1(:,:) = 0._wp
      zrxmax   = 0._wp
      zr1(:)   = 0._wp
      !
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         !!gm   remark: dk(gdepw) = e3t   ===>>>  possible simplification of the following calculation....
         !!             especially since it is gde3w which is used to compute the pressure gradient
         !!             furthermore, I think gdept_0 should be used below instead of w point in the numerator
         !!             so that the ratio is computed at the same point (i.e. uw and vw) ....
         zr1(1) = ABS(  ( gdepw_0(ji  ,jj,jk  )-gdepw_0(ji-1,jj,jk  )               & 
            &            +gdepw_0(ji  ,jj,jk+1)-gdepw_0(ji-1,jj,jk+1) )             &
            &         / ( gdepw_0(ji  ,jj,jk  )+gdepw_0(ji-1,jj,jk  )               &
            &            -gdepw_0(ji  ,jj,jk+1)-gdepw_0(ji-1,jj,jk+1) + rsmall )  ) * umask(ji-1,jj,jk)
         zr1(2) = ABS(  ( gdepw_0(ji+1,jj,jk  )-gdepw_0(ji  ,jj,jk  )               &
            &            +gdepw_0(ji+1,jj,jk+1)-gdepw_0(ji  ,jj,jk+1) )             &
            &         / ( gdepw_0(ji+1,jj,jk  )+gdepw_0(ji  ,jj,jk  )               &
            &            -gdepw_0(ji+1,jj,jk+1)-gdepw_0(ji  ,jj,jk+1) + rsmall )  ) * umask(ji  ,jj,jk)
         zr1(3) = ABS(  ( gdepw_0(ji,jj+1,jk  )-gdepw_0(ji,jj  ,jk  )               &
            &            +gdepw_0(ji,jj+1,jk+1)-gdepw_0(ji,jj  ,jk+1) )             &
            &         / ( gdepw_0(ji,jj+1,jk  )+gdepw_0(ji,jj  ,jk  )               &
            &            -gdepw_0(ji,jj+1,jk+1)-gdepw_0(ji,jj  ,jk+1) + rsmall )  ) * vmask(ji,jj  ,jk)
         zr1(4) = ABS(  ( gdepw_0(ji,jj  ,jk  )-gdepw_0(ji,jj-1,jk  )               &
            &            +gdepw_0(ji,jj  ,jk+1)-gdepw_0(ji,jj-1,jk+1) )             &
            &         / ( gdepw_0(ji,jj  ,jk  )+gdepw_0(ji,jj-1,jk  )               &
            &            -gdepw_0(ji,jj  ,jk+1)-gdepw_0(ji,jj-1,jk+1) + rsmall )  ) * vmask(ji,jj-1,jk)
         zrxmax = MAXVAL( zr1(1:4) )
         zx1(ji,jj) = MAX( zx1(ji,jj) , zrxmax )
      END_3D
      CALL lbc_lnk( 'domwri', zx1, 'T', 1.0_wp )
      !
      IF( PRESENT( px1 ) )    px1 = zx1
      !
      zrxmax = MAXVAL( zx1 )
      !
      CALL mpp_max( 'domwri', zrxmax ) ! max over the global domain
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'dom_stiff : maximum grid stiffness ratio: ', zrxmax
         WRITE(numout,*) '~~~~~~~~~'
      ENDIF
      !
   END SUBROUTINE dom_stiff

   !!======================================================================
END MODULE domwri
