MODULE closea
   !!======================================================================
   !!                   ***  MODULE  closea  ***
   !!
   !! User define : specific treatments associated with closed seas
   !!======================================================================
   !! History :   8.2  !  2000-05  (O. Marti)  Original code
   !!   NEMO      1.0  !  2002-06  (E. Durand, G. Madec)  F90
   !!             3.0  !  2006-07  (G. Madec)  add clo_rnf, clo_ups, clo_bat
   !!             3.4  !  2014-12  (P.G. Fogli) sbc_clo bug fix & mpp reproducibility
   !!             4.0  !  2016-06  (G. Madec)  move to usrdef_closea, remove clo_ups
   !!             4.0  !  2017-12  (D. Storkey) new formulation based on masks read from file
   !!             4.1  !  2019-07  (P. Mathiot) update to the new domcfg.nc input file
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dom_clo    : read in masks which define closed seas and runoff areas
   !!   clo_rnf    : set close sea outflows as river mouths (see sbcrnf)
   !!   clo_msk    : set to zero a field over closed sea (see domzgr)
   !!----------------------------------------------------------------------
   USE in_out_manager  ! I/O manager
   !
   USE diu_bulk    , ONLY: ln_diurnal_only            ! used for sanity check
   USE iom         , ONLY: iom_open, iom_get, iom_close, jpdom_global ! I/O routines
   USE lib_fortran , ONLY: glob_sum                   ! fortran library
   USE lib_mpp     , ONLY: mpp_max, ctl_nam, ctl_stop ! MPP library

   IMPLICIT NONE

   PRIVATE

   PUBLIC dom_clo      ! called by domain module
   PUBLIC clo_rnf      ! called by sbcrnf module
   PUBLIC clo_msk      ! called in domzgr module

   LOGICAL, PUBLIC :: ln_maskcs        !: logical to mask all closed sea
   LOGICAL, PUBLIC :: ln_mask_csundef  !: logical to mask all undefined closed sea
   LOGICAL, PUBLIC :: ln_clo_rnf       !: closed sea treated as runoff (update rnf mask)

   ! WARNING: keep default definitions in the following lines as dom_clo is called only if ln_closea = .true.
   LOGICAL, PUBLIC :: l_sbc_clo = .FALSE.   !: T => net evap/precip over closed seas spread outover the globe/river mouth
   LOGICAL, PUBLIC :: l_clo_rnf = .FALSE.   !: T => Some closed seas output freshwater (RNF) to specified runoff points.

   INTEGER, PUBLIC :: ncsg = 0   !: number of closed seas global mappings (inferred from closea_mask_glo field)
   INTEGER, PUBLIC :: ncsr = 0   !: number of closed seas rnf    mappings (inferred from closea_mask_rnf field)
   INTEGER, PUBLIC :: ncse = 0   !: number of closed seas empmr  mappings (inferred from closea_mask_emp field)

   INTEGER, PUBLIC, SAVE, ALLOCATABLE, DIMENSION(:,:) :: mask_opnsea, mask_csundef  !: mask defining the open sea and the undefined closed sea
 
   INTEGER, PUBLIC, SAVE, ALLOCATABLE, DIMENSION(:,:) :: mask_csglo , mask_csgrpglo !: mask of integers defining closed seas
   INTEGER, PUBLIC, SAVE, ALLOCATABLE, DIMENSION(:,:) :: mask_csrnf , mask_csgrprnf !: mask of integers defining closed seas rnf mappings
   INTEGER, PUBLIC, SAVE, ALLOCATABLE, DIMENSION(:,:) :: mask_csemp , mask_csgrpemp !: mask of integers defining closed seas empmr mappings

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: closea.F90 13558 2020-10-02 15:30:22Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dom_clo()
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE dom_clo  ***
      !!        
      !! ** Purpose :   Closed sea domain initialization
      !!
      !! ** Action  :   Read mask_cs* fields (if needed) from domain_cfg file and infer
      !!                number of closed seas for each case (glo, rnf, emp) from mask_cs* field.
      !!
      !! ** Output  :   mask_csglo and mask_csgrpglo  : integer values defining mappings from closed seas and associated groups to the open ocean for net fluxes.
      !!                mask_csrnf and mask_csgrprnf  : integer values defining mappings from closed seas and associated groups to a runoff area for downwards flux only.
      !!                mask_csemp and mask_csgrpemp  : integer values defining mappings from closed seas and associated groups to a runoff area for net fluxes.
      !!----------------------------------------------------------------------
      INTEGER ::   ios     ! io status
      !!
      NAMELIST/namclo/ ln_maskcs, ln_mask_csundef, ln_clo_rnf
      !!---------------------------------------------------------------------
      !!
      READ  ( numnam_ref, namclo, IOSTAT = ios, ERR = 901 )
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namclo in reference namelist' )
      READ  ( numnam_cfg, namclo, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namclo in configuration namelist' )
      IF(lwm) WRITE ( numond, namclo )
      !!
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*)'dom_clo : read in masks to define closed seas '
      IF(lwp) WRITE(numout,*)'~~~~~~~'
      IF(lwp) WRITE(numout,*)
      !!
      !! check option compatibility
      IF( .NOT. ln_read_cfg ) THEN
         CALL ctl_stop('Suppression of closed seas does not work with ln_read_cfg = .true. . Set ln_closea = .false. .')
      ENDIF
      !!
      IF( (.NOT. ln_maskcs) .AND. ln_diurnal_only ) THEN
         CALL ctl_stop('Special handling of freshwater fluxes over closed seas not compatible with ln_diurnal_only.')
      END IF
      !
      ! read the closed seas masks (if they exist) from domain_cfg file (if it exists)
      ! ------------------------------------------------------------------------------
      !
      ! load mask of open sea
      CALL alloc_csmask( mask_opnsea )
      CALL read_csmask( cn_domcfg, 'mask_opensea' , mask_opnsea  )
      !
      IF ( ln_maskcs ) THEN
         ! closed sea are masked
         IF(lwp) WRITE(numout,*)'          ln_maskcs = T : all closed seas are masked'
         IF(lwp) WRITE(numout,*)
         ! no special treatment of closed sea
         ! no redistribution of emp unbalance over closed sea into river mouth/open ocean
         l_sbc_clo = .false. ; l_clo_rnf = .false.
      ELSE
         ! redistribution of emp unbalance over closed sea into river mouth/open ocean
         IF(lwp) WRITE(numout,*)'          ln_maskcs = F : net emp is corrected over defined closed seas'
         !
         l_sbc_clo = .true.
         !
         ! river mouth from lakes added to rnf mask for special treatment
         IF ( ln_clo_rnf) l_clo_rnf = .true.
         !
         IF ( ln_mask_csundef) THEN
            ! closed sea not defined (ie not in the domcfg namelist used to build the domcfg.nc file) are masked 
            IF(lwp) WRITE(numout,*)'          ln_mask_csundef = T : all undefined closed seas are masked'
            !
            CALL alloc_csmask( mask_csundef )
            CALL read_csmask( cn_domcfg, 'mask_csundef', mask_csundef )
            ! revert the mask for masking of undefined closed seas in domzgr 
            ! (0 over the undefined closed sea and 1 elsewhere)
            mask_csundef(:,:) = 1 - mask_csundef(:,:)
         END IF
         IF(lwp) WRITE(numout,*)
         !
         ! allocate source mask for each cases
         CALL alloc_csmask( mask_csglo )
         CALL alloc_csmask( mask_csrnf )
         CALL alloc_csmask( mask_csemp )
         !
         ! load source mask of cs for each cases
         CALL read_csmask( cn_domcfg, 'mask_csglo', mask_csglo )
         CALL read_csmask( cn_domcfg, 'mask_csrnf', mask_csrnf )
         CALL read_csmask( cn_domcfg, 'mask_csemp', mask_csemp )
         !
         ! compute number of cs for each cases
         ncsg = MAXVAL( mask_csglo(:,:) ) ; CALL mpp_max( 'closea', ncsg )
         ncsr = MAXVAL( mask_csrnf(:,:) ) ; CALL mpp_max( 'closea', ncsr )
         ncse = MAXVAL( mask_csemp(:,:) ) ; CALL mpp_max( 'closea', ncse )
         !
         ! allocate closed sea group masks 
         !(used to defined the target area in case multiple lakes have the same river mouth (great lakes for example))
         CALL alloc_csmask( mask_csgrpglo )
         CALL alloc_csmask( mask_csgrprnf )
         CALL alloc_csmask( mask_csgrpemp )

         ! load mask of cs group for each cases
         CALL read_csmask( cn_domcfg, 'mask_csgrpglo', mask_csgrpglo )
         CALL read_csmask( cn_domcfg, 'mask_csgrprnf', mask_csgrprnf )
         CALL read_csmask( cn_domcfg, 'mask_csgrpemp', mask_csgrpemp )
         !
      END IF
   END SUBROUTINE dom_clo

   SUBROUTINE clo_rnf( p_rnfmsk )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE clo_rnf  ***
      !!                    
      !! ** Purpose :   allow the treatment of closed sea outflow grid-points
      !!                to be the same as river mouth grid-points
      !!
      !! ** Method  :   set to 1 the runoff mask (mskrnf, see sbcrnf module)
      !!                at the closed sea outflow grid-point.
      !!
      !! ** Action  :   update (p_)mskrnf (set 1 at closed sea outflow)
      !!----------------------------------------------------------------------
      !! subroutine parameter
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) ::   p_rnfmsk   ! river runoff mask (rnfmsk array)
      !!
      !! local variables
      REAL(wp), DIMENSION(jpi,jpj) :: zmsk
      !!----------------------------------------------------------------------
      !
      ! zmsk > 0 where cs river mouth defined (case rnf and emp)
      zmsk(:,:) = ( mask_csgrprnf (:,:) + mask_csgrpemp(:,:) ) * mask_opnsea(:,:)
      WHERE( zmsk(:,:) > 0 )
         p_rnfmsk(:,:) = 1.0_wp
      END WHERE
      !
   END SUBROUTINE clo_rnf
      
   SUBROUTINE clo_msk( k_top, k_bot, k_mask, cd_prt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE clo_msk  ***
      !!                    
      !! ** Purpose :   Suppress closed sea from the domain
      !!
      !! ** Method  :   Where closea_mask > 0 set first and last ocean level to 0
      !!                (As currently coded you can't define a closea_mask field in 
      !!                usr_def_zgr).
      !!
      !! ** Action  :   set k_top=0 and k_bot=0 over closed seas
      !!----------------------------------------------------------------------
      !! subroutine parameter
      INTEGER, DIMENSION(:,:), INTENT(inout) ::   k_top, k_bot   ! ocean first and last level indices
      INTEGER, DIMENSION(:,:), INTENT(in   ) ::   k_mask         ! mask used to mask ktop and k_bot
      CHARACTER(LEN=*),        INTENT(in   ) ::   cd_prt         ! text for control print
      !!
      !! local variables
      !!----------------------------------------------------------------------
      !!
      IF ( lwp ) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'clo_msk : Suppression closed seas based on ',TRIM(cd_prt),' field.'
         WRITE(numout,*) '~~~~~~~'
         WRITE(numout,*)
      ENDIF
      !!
      k_top(:,:) = k_top(:,:) * k_mask(:,:)
      k_bot(:,:) = k_bot(:,:) * k_mask(:,:)
      !!
   END SUBROUTINE clo_msk

   SUBROUTINE read_csmask(cd_file, cd_var, k_mskout)
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE read_csmask  ***
      !!                    
      !! ** Purpose : read mask in cd_filec file
      !!----------------------------------------------------------------------
      ! subroutine parameter
      CHARACTER(LEN=256),          INTENT(in   ) :: cd_file    ! netcdf file     name
      CHARACTER(LEN= * ),          INTENT(in   ) :: cd_var     ! netcdf variable name
      INTEGER, DIMENSION(:,:), INTENT(  out) :: k_mskout            ! output mask variable
      !
      ! local variables
      INTEGER :: ics                       ! netcdf id
      REAL(wp), DIMENSION(jpi,jpj) :: zdta ! netcdf data
      !!----------------------------------------------------------------------
      !
      CALL iom_open ( cd_file, ics )
      CALL iom_get  ( ics, jpdom_global, TRIM(cd_var), zdta )
      CALL iom_close( ics )
      k_mskout(:,:) = NINT(zdta(:,:))
      !
   END SUBROUTINE read_csmask

   SUBROUTINE alloc_csmask( kmask )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE alloc_csmask  ***
      !!                    
      !! ** Purpose : allocated cs mask
      !!----------------------------------------------------------------------
      ! subroutine parameter
      INTEGER, ALLOCATABLE, DIMENSION(:,:), INTENT(inout) :: kmask
      !
      ! local variables
      INTEGER :: ierr
      !!----------------------------------------------------------------------
      !
      ALLOCATE( kmask(jpi,jpj) , STAT=ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'alloc_csmask: failed to allocate surf array')
      !
   END SUBROUTINE

END MODULE closea
