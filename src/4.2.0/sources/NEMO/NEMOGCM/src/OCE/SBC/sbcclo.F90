MODULE sbcclo
   !!======================================================================
   !!                       ***  MODULE  sbcclo  ***
   !! Ocean forcing: redistribution of emp unbalance over closed sea into river mouth or open ocean
   !!=====================================================================
   !! History :  4.0 and earlier ! see closea.F90 history   
   !!   NEMO     4.1  ! 2019-09  (P. Mathiot) rewrite sbc_clo module to match new closed sea mask definition (original sbcclo.F90)
   !! 
   !!----------------------------------------------------------------------
   !
   !!----------------------------------------------------------------------
   !!   Public subroutines:
   !!   sbc_clo       : update emp and qns over target area and source area
   !!   sbc_clo_init  : initialise all variable needed for closed sea correction
   !!
   !!   Private subroutines:
   !!   alloc_csarr   : allocate closed sea array
   !!   get_cssrcsurf : compute source surface area
   !!   get_cstrgsurf : compute target surface area
   !!   prt_csctl     : closed sea control print
   !!   sbc_csupdate  : compute net fw from closed sea
   !!----------------------------------------------------------------------
   !
   USE closea                                  ! closed sea 
   USE in_out_manager                          ! I/O manager
   !
   USE dom_oce,     ONLY: e1e2t                ! ocean space and time domain
   USE phycst ,     ONLY: rcp                  ! physical constants
   USE sbc_oce,     ONLY: emp, qns, rnf, sst_m ! ocean surface boundary conditions
   USE iom    ,     ONLY: iom_put              ! I/O routines
   USE lib_fortran, ONLY: glob_sum             ! fortran library
   USE lib_mpp    , ONLY: mpp_min, ctl_stop    ! MPP library
   !
   IMPLICIT NONE
   !
   PRIVATE
   !
   PUBLIC sbc_clo
   PUBLIC sbc_clo_init
   !
   REAL(wp), SAVE, ALLOCATABLE, DIMENSION(:) :: rsurfsrcg, rsurftrgg      !: closed sea source/target glo surface areas 
   REAL(wp), SAVE, ALLOCATABLE, DIMENSION(:) :: rsurfsrcr, rsurftrgr      !: closed sea source/target rnf surface areas 
   REAL(wp), SAVE, ALLOCATABLE, DIMENSION(:) :: rsurfsrce, rsurftrge      !: closed sea source/target emp surface areas 
   !
   INTEGER, SAVE, ALLOCATABLE, DIMENSION(:)  :: mcsgrpg, mcsgrpr, mcsgrpe !: closed sea group for glo, rnf and emp
   !
   CONTAINS
   !
   !!----------------------------------------------------------------------
   !!  Public subroutines
   !!----------------------------------------------------------------------
   !
   SUBROUTINE sbc_clo_init
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE sbc_clo_init  ***
      !!                    
      !! ** Purpose :  Initialisation of the variable needed for the net fw closed sea correction
      !!
      !! ** Method  :  - compute source surface area for each closed sea
      !!               - defined the group of each closed sea 
      !!                 (needed to manage multiple closed sea and one target area like great lakes / St Laurent outlet)
      !!               - compute target surface area
      !!----------------------------------------------------------------------
      !
      ! 0. Allocate cs variables (surf)
      CALL alloc_csarr( ncsg, rsurfsrcg, rsurftrgg, mcsgrpg ) 
      CALL alloc_csarr( ncsr, rsurfsrcr, rsurftrgr, mcsgrpr )
      CALL alloc_csarr( ncse, rsurfsrce, rsurftrge, mcsgrpe )
      !
      ! 1. compute source surface area
      CALL get_cssrcsurf( ncsg, mask_csglo, rsurfsrcg )
      CALL get_cssrcsurf( ncsr, mask_csrnf, rsurfsrcr )
      CALL get_cssrcsurf( ncse, mask_csemp, rsurfsrce )
      !
      ! 2. compute target surface area and group number (mcsgrp) for all cs and cases 
      ! glo could be simpler but for lisibility, all treated the same way
      ! It is only done once, so not a big deal
      CALL get_cstrgsurf( ncsg, mask_csglo, mask_csgrpglo, rsurftrgg, mcsgrpg )
      CALL get_cstrgsurf( ncsr, mask_csrnf, mask_csgrprnf, rsurftrgr, mcsgrpr )
      CALL get_cstrgsurf( ncse, mask_csemp, mask_csgrpemp, rsurftrge, mcsgrpe )
      ! 
      ! 3. print out in ocean.ouput
      IF ( lwp ) WRITE(numout,*) 'sbc_clo_init : compute surface area for source (closed sea) and target (river mouth)'
      IF ( lwp ) WRITE(numout,*) '~~~~~~~~~~~~~~'
      CALL prt_csctl( ncsg, rsurfsrcg, rsurftrgg, mcsgrpg, 'glo' )
      CALL prt_csctl( ncsr, rsurfsrcr, rsurftrgr, mcsgrpr, 'rnf' )
      CALL prt_csctl( ncse, rsurfsrce, rsurftrge, mcsgrpe, 'emp' )

   END SUBROUTINE sbc_clo_init

   SUBROUTINE sbc_clo( kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE sbc_clo  ***
      !!                    
      !! ** Purpose :   Special handling of closed seas
      !!
      !! ** Method  :   Water flux is forced to zero over closed sea
      !!      Excess is shared between remaining ocean, or
      !!      put as run-off in open ocean.
      !!
      !! ** Action  : - compute surface freshwater fluxes and associated heat content flux at kt
      !!              - output closed sea contribution to fw and heat budget
      !!              - update emp and qns
      !!----------------------------------------------------------------------
      INTEGER         , INTENT(in   ) ::   kt       ! ocean model time step
      !
      REAL(wp), DIMENSION(jpi,jpj) :: zwcs, zqcs    ! water flux and heat flux correction due to closed seas
      !!----------------------------------------------------------------------
      !
      ! 0. initialisation
      zwcs(:,:) = 0._wp ; zqcs(:,:) = 0._wp
      !
      ! 1. update emp and qns
      CALL sbc_csupdate( ncsg, mcsgrpg, mask_csglo, mask_csgrpglo, rsurfsrcg, rsurftrgg, 'glo', mask_opnsea, rsurftrgg, zwcs, zqcs )
      CALL sbc_csupdate( ncsr, mcsgrpr, mask_csrnf, mask_csgrprnf, rsurfsrcr, rsurftrgr, 'rnf', mask_opnsea, rsurftrgg, zwcs, zqcs )
      CALL sbc_csupdate( ncse, mcsgrpe, mask_csemp, mask_csgrpemp, rsurfsrce, rsurftrge, 'emp', mask_opnsea, rsurftrgg, zwcs, zqcs )
      !
      ! 2. ouput closed sea contributions
      CALL iom_put('wclosea',zwcs)
      CALL iom_put('qclosea',zqcs)
      !
      ! 3. update emp and qns
      emp(:,:) = emp(:,:) + zwcs(:,:)
      qns(:,:) = qns(:,:) + zqcs(:,:)
      !
   END SUBROUTINE sbc_clo
   !
   !!----------------------------------------------------------------------
   !!  Private subroutines
   !!----------------------------------------------------------------------
   !
   SUBROUTINE get_cssrcsurf(kncs, kmaskcs, psurfsrc)
      !!-----------------------------------------------------------------------
      !!                  ***  routine get_cssrcsurf  ***
      !!
      !! ** Purpose : compute closed sea (source) surface area
      !!----------------------------------------------------------------------
      ! subroutine parameters
      INTEGER ,                 INTENT(in   ) :: kncs          ! closed sea number
      INTEGER , DIMENSION(:,:), INTENT(in   ) :: kmaskcs       ! closed sea mask
      REAL(wp), DIMENSION(:)  , INTENT(  out) :: psurfsrc      ! source surface area

      ! local variables
      INTEGER :: jcs                                           ! loop index
      INTEGER, DIMENSION(jpi,jpj) :: imsksrc                   ! source mask
      !!----------------------------------------------------------------------
      !
      DO jcs = 1,kncs  ! loop over closed seas
         !
         ! 0. build river mouth mask for this lake
         WHERE ( kmaskcs == jcs )
            imsksrc = 1
         ELSE WHERE
            imsksrc = 0
         END WHERE
         !
         ! 1. compute target area
         psurfsrc(jcs) = glob_sum('closea', e1e2t(:,:) * imsksrc(:,:) )
         !
      END DO  ! jcs

   END SUBROUTINE

   SUBROUTINE get_cstrgsurf(kncs, kmaskcs, kmaskcsgrp, psurftrg, kcsgrp )
      !!-----------------------------------------------------------------------
      !!                  ***  routine get_cstrgsurf  ***
      !!
      !! ** Purpose : compute closed sea (target) surface area
      !!----------------------------------------------------------------------
      ! subroutine parameters
      ! input
      INTEGER,                 INTENT(in   ) :: kncs                 ! closed sea number
      INTEGER, DIMENSION(:,:), INTENT(in   ) :: kmaskcs, kmaskcsgrp  ! closed sea and group mask

      ! output
      INTEGER , DIMENSION(:)  , INTENT(  out) :: kcsgrp              ! closed sea group number
      REAL(wp), DIMENSION(:)  , INTENT(  out) :: psurftrg            ! target surface area

      ! local variables
      INTEGER :: jcs, jtmp                                           ! tmp
      INTEGER, DIMENSION(jpi,jpj) :: imskgrp, imsksrc, imsktrg, imsk ! tmp group, source, target and tmp mask
      !!----------------------------------------------------------------------
      !
      DO jcs = 1,kncs  ! loop over closed seas
         !
         !! 0. find group number for cs number jcs
         imskgrp(:,:) = kmaskcsgrp(:,:)
         imsksrc(:,:) = kmaskcs(:,:)
         !
         ! set cs value where cs is defined
         ! imsk = HUGE outside the cs id jcs
         imsk(:,:) = HUGE(1)
         WHERE ( imsksrc(:,:) == jcs ) imsk(:,:) = jcs
         !
         ! jtmp = jcs - group id for this lake
         imsk(:,:) = imsk(:,:) - imskgrp(:,:)
         jtmp = MINVAL(imsk(:,:)) ; CALL mpp_min('closea',jtmp)
         ! kcsgrp = group id corresponding to the cs id jcs
         ! kcsgrp(jcs)=(jcs - (jcs - group id))=group id
         kcsgrp(jcs) = jcs - jtmp
         !
         !! 1. build the target river mouth mask for this lake
         WHERE ( imskgrp(:,:) * mask_opnsea(:,:) == kcsgrp(jcs) )
            imsktrg(:,:) = 1
         ELSE WHERE
            imsktrg(:,:) = 0
         END WHERE
         !
         !! 2. compute target area
         psurftrg(jcs) = glob_sum('closea', e1e2t(:,:) * imsktrg(:,:) )
         !
      END DO ! jcs

   END SUBROUTINE

   SUBROUTINE prt_csctl(kncs, psurfsrc, psurftrg, kcsgrp, cdcstype)
      !!-----------------------------------------------------------------------
      !!                  ***  routine prt_csctl  ***
      !!
      !! ** Purpose : output information about each closed sea (src id, trg id, src area and trg area)
      !!----------------------------------------------------------------------
      ! subroutine parameters
      INTEGER,                INTENT(in   ) :: kncs                ! closed sea number                
      INTEGER, DIMENSION(:) , INTENT(in   ) :: kcsgrp              ! closed sea group number
      !
      REAL(wp), DIMENSION(:), INTENT(in   ) :: psurfsrc, psurftrg  ! source and target surface area
      !
      CHARACTER(LEN=3)      , INTENT(in   ) :: cdcstype            ! closed sea scheme used for redistribution
      !!----------------------------------------------------------------------
      ! local variable
      INTEGER :: jcs
      !!----------------------------------------------------------------------
      !
      IF ( lwp .AND. kncs > 0 ) THEN
         WRITE(numout,*)''
         !
         WRITE(numout,*)'Closed sea target ',TRIM(cdcstype),' : '
         !
         DO jcs = 1,kncs
            WRITE(numout,FMT='(3a,i3,a,i3)') ' ',TRIM(cdcstype),' closed sea id is ',jcs,' and trg group id is : ', kcsgrp(jcs)
            WRITE(numout,FMT='(a,f12.2)'   ) ' src surface areas (km2) : ', psurfsrc(jcs) * 1.0e-6
            WRITE(numout,FMT='(a,f12.2)'   ) ' trg surface areas (km2) : ', psurftrg(jcs) * 1.0e-6
         END DO
         !
         WRITE(numout,*)''
      END IF

   END SUBROUTINE

   SUBROUTINE sbc_csupdate(kncs, kcsgrp, kmsk_src, kmsk_grp, psurfsrc, psurftrg, cdcstype, kmsk_opnsea, psurf_opnsea, pwcs, pqcs)
      !!-----------------------------------------------------------------------
      !!                  ***  routine sbc_csupdate  ***
      !!
      !! ** Purpose : - compute the net freshwater fluxes over each closed seas
      !!              - apply correction to closed sea source/target net fwf accordingly
      !!----------------------------------------------------------------------
      ! subroutine parameters
      CHARACTER(LEN=3)        , INTENT(in   ) :: cdcstype  ! closed sea scheme used for redistribution
      !
      INTEGER,                 INTENT(in)     :: kncs                                 ! closed sea id
      INTEGER, DIMENSION(:  ), INTENT(in)     :: kcsgrp                               ! closed sea group id
      INTEGER, DIMENSION(:,:), INTENT(in)     :: kmsk_src, kmsk_grp, kmsk_opnsea      ! source, target, open ocean mask
      
      REAL(wp), DIMENSION(:)  , INTENT(in   ) :: psurfsrc, psurftrg, psurf_opnsea ! source, target and open ocean surface area
      REAL(wp), DIMENSION(:,:), INTENT(inout) :: pwcs, pqcs                       ! water and heat flux correction due to closed seas


      ! local variables
      INTEGER :: jcs                                     ! loop index over closed sea 
      INTEGER, DIMENSION(jpi,jpj) :: imsk_src, imsk_trg  ! tmp array source and target closed sea masks
      
      REAL(wp) :: zcsfw, zcsh        ! total fresh water and associated heat over one closed sea
      REAL(wp) :: zcsfwf             ! mean fresh water flux over one closed sea
      REAL(wp) :: zsurftrg, zsurfsrc ! total target surface area
      !!----------------------------------------------------------------------
      !
      DO jcs = 1, kncs  ! loop over closed seas
         !
         !! 0. get mask and surface of the closed sea
         ! mask src
         WHERE ( kmsk_src(:,:) == jcs ) 
            imsk_src(:,:) = 1
         ELSEWHERE
            imsk_src(:,:) = 0
         END WHERE
         ! area src
         zsurfsrc = psurfsrc(jcs)
         !
         !! 1. Work out net freshwater over the closed sea from EMP - RNF.
         !!    Work out net heat associated with the correction (needed for conservation)
         !!    (PM: should we consider used delayed glob sum ?)
         zcsfw  = glob_sum( 'closea', e1e2t(:,:) * ( emp(:,:)-rnf(:,:) ) * imsk_src(:,:) )
         !
         !! 2. Deal with runoff special case (net evaporation spread globally)
         !!    and compute trg mask
         IF (cdcstype == 'rnf' .AND. zcsfw  > 0._wp) THEN
            zsurftrg = psurf_opnsea(1)           ! change the target area surface
            imsk_trg = kcsgrp(jcs) * kmsk_opnsea ! trg mask is now the open sea mask
         ELSE
            zsurftrg = psurftrg(jcs)
            imsk_trg = kmsk_grp * kmsk_opnsea
         END IF
         !
         IF( zsurftrg > 0._wp ) THEN  ! target area /=0
            !! 3. Subtract residuals from source points
            zcsfwf = zcsfw / zsurfsrc
            pwcs(:,:) = pwcs(:,:) -       zcsfwf              * imsk_src(:,:)
            pqcs(:,:) = pqcs(:,:) + rcp * zcsfwf * sst_m(:,:) * imsk_src(:,:)
            !
            !! 4. Add residuals to target points 
            !!    Do not use pqcs(:,:) = pqcs(:,:) - rcp * zcsfw  * sst_m(:,:) / zsurftrg 
            !!    as there is no reason heat will be conserved with this formulation
            zcsh   = glob_sum( 'closea', e1e2t(:,:) * rcp * zcsfwf * sst_m(:,:) * imsk_src(:,:) )
            WHERE( imsk_trg(:,:) == kcsgrp(jcs) )
               pwcs(:,:) = pwcs(:,:) + zcsfw / zsurftrg
               pqcs(:,:) = pqcs(:,:) - zcsh  / zsurftrg
            ENDWHERE
         ENDIF
         !
      END DO ! jcs

   END SUBROUTINE

   SUBROUTINE alloc_csarr( klen, pvarsrc, pvartrg, kvargrp )
      !!-----------------------------------------------------------------------
      !!                  ***  routine alloc_cssurf  ***
      !!
      !! ** Purpose : allocate closed sea surface array
      !!----------------------------------------------------------------------
      ! subroutine parameters
      INTEGER,  INTENT(in) :: klen
      INTEGER,  ALLOCATABLE, DIMENSION(:), INTENT(  out) :: kvargrp
      REAL(wp), ALLOCATABLE, DIMENSION(:), INTENT(  out) :: pvarsrc, pvartrg 
      !
      ! local variables
      INTEGER :: ierr
      !!----------------------------------------------------------------------
      !
      ! klen (number of lake) can be zero so use MAX(klen,1) to avoid 0 length array
      ALLOCATE( pvarsrc(MAX(klen,1)) , pvartrg(MAX(klen,1)) , STAT=ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'sbc_clo: failed to allocate surf array')
      !
      ALLOCATE( kvargrp(MAX(klen,1)) , STAT=ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'sbc_clo: failed to allocate group array')
      !
      ! initialise to 0
      pvarsrc(:) = 0.e0_wp
      pvartrg(:) = 0.e0_wp
      kvargrp(:) = 0
   END SUBROUTINE

END MODULE
