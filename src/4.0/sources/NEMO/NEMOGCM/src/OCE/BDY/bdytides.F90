MODULE bdytides
   !!======================================================================
   !!                       ***  MODULE  bdytides  ***
   !! Ocean dynamics:   Tidal forcing at open boundaries
   !!======================================================================
   !! History :  2.0  !  2007-01  (D.Storkey)  Original code
   !!            2.3  !  2008-01  (J.Holt)  Add date correction. Origins POLCOMS v6.3 2007
   !!            3.0  !  2008-04  (NEMO team)  add in the reference version
   !!            3.3  !  2010-09  (D.Storkey and E.O'Dea)  bug fixes
   !!            3.4  !  2012-09  (G. Reffray and J. Chanut) New inputs + mods
   !!            3.5  !  2013-07  (J. Chanut) Compliant with time splitting changes
   !!----------------------------------------------------------------------
   !!   bdytide_init  : read of namelist and initialisation of tidal harmonics data
   !!   tide_update   : calculation of tidal forcing at each timestep
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers 
   USE dom_oce        ! ocean space and time domain
   USE phycst         ! physical constants
   USE bdy_oce        ! ocean open boundary conditions
   USE tideini        ! 
   USE daymod         ! calendar
   !
   USE in_out_manager ! I/O units
   USE iom            ! xIO server
   USE fldread        !
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)

   IMPLICIT NONE
   PRIVATE

   PUBLIC   bdytide_init     ! routine called in bdy_init
   PUBLIC   bdytide_update   ! routine called in bdy_dta
   PUBLIC   bdy_dta_tides    ! routine called in dyn_spg_ts

   TYPE, PUBLIC ::   TIDES_DATA     !: Storage for external tidal harmonics data
      REAL(wp), POINTER, DIMENSION(:,:,:)    ::   ssh0     !: Tidal constituents : SSH0   (read in file)
      REAL(wp), POINTER, DIMENSION(:,:,:)    ::   u0, v0   !: Tidal constituents : U0, V0 (read in file)
      REAL(wp), POINTER, DIMENSION(:,:,:)    ::   ssh      !: Tidal constituents : SSH    (after nodal cor.)
      REAL(wp), POINTER, DIMENSION(:,:,:)    ::   u , v    !: Tidal constituents : U , V  (after nodal cor.)
   END TYPE TIDES_DATA

!$AGRIF_DO_NOT_TREAT
   TYPE(TIDES_DATA), PUBLIC, DIMENSION(jp_bdy), TARGET :: tides  !: External tidal harmonics data
!$AGRIF_END_DO_NOT_TREAT
   TYPE(OBC_DATA)  , PUBLIC, DIMENSION(jp_bdy) :: dta_bdy_s  !: bdy external data (slow component)

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: bdytides.F90 10068 2018-08-28 14:09:04Z nicolasmartin $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE bdytide_init
      !!----------------------------------------------------------------------
      !!                    ***  SUBROUTINE bdytide_init  ***
      !!                     
      !! ** Purpose : - Read in namelist for tides and initialise external
      !!                tidal harmonics data
      !!
      !!----------------------------------------------------------------------
      !! namelist variables
      !!-------------------
      CHARACTER(len=80)                         ::   filtide             !: Filename root for tidal input files
      LOGICAL                                   ::   ln_bdytide_2ddta    !: If true, read 2d harmonic data
      LOGICAL                                   ::   ln_bdytide_conj     !: If true, assume complex conjugate tidal data
      !!
      INTEGER                                   ::   ib_bdy, itide, ib   !: dummy loop indices
      INTEGER                                   ::   ii, ij              !: dummy loop indices
      INTEGER                                   ::   inum, igrd
      INTEGER, DIMENSION(3)                     ::   ilen0       !: length of boundary data (from OBC arrays)
      INTEGER, POINTER, DIMENSION(:)            ::   nblen, nblenrim     ! short cuts
      INTEGER                                   ::   ios                 ! Local integer output status for namelist read
      CHARACTER(len=80)                         ::   clfile              !: full file name for tidal input file 
      REAL(wp),ALLOCATABLE, DIMENSION(:,:,:)    ::   dta_read            !: work space to read in tidal harmonics data
      REAL(wp),ALLOCATABLE, DIMENSION(:,:)      ::   ztr, zti            !:  "     "    "   "   "   "        "      " 
      !!
      TYPE(TIDES_DATA),  POINTER                ::   td                  !: local short cut   
      TYPE(MAP_POINTER), DIMENSION(jpbgrd)      ::   ibmap_ptr           !: array of pointers to nbmap
      !!
      NAMELIST/nambdy_tide/filtide, ln_bdytide_2ddta, ln_bdytide_conj
      !!----------------------------------------------------------------------
      !
      IF (nb_bdy>0) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'bdytide_init : initialization of tidal harmonic forcing at open boundaries'
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF

      REWIND(numnam_cfg)

      DO ib_bdy = 1, nb_bdy
         IF( nn_dyn2d_dta(ib_bdy) >= 2 ) THEN
            !
            td => tides(ib_bdy)
            nblen => idx_bdy(ib_bdy)%nblen
            nblenrim => idx_bdy(ib_bdy)%nblenrim

            ! Namelist nambdy_tide : tidal harmonic forcing at open boundaries
            filtide(:) = ''

            ! Don't REWIND here - may need to read more than one of these namelists. 
            READ  ( numnam_ref, nambdy_tide, IOSTAT = ios, ERR = 901)
901         IF( ios /= 0 )   CALL ctl_nam ( ios , 'nambdy_tide in reference namelist', lwp )
            READ  ( numnam_cfg, nambdy_tide, IOSTAT = ios, ERR = 902 )
902         IF( ios >  0 )   CALL ctl_nam ( ios , 'nambdy_tide in configuration namelist', lwp )
            IF(lwm) WRITE ( numond, nambdy_tide )
            !                                               ! Parameter control and print
            IF(lwp) WRITE(numout,*) '  '
            IF(lwp) WRITE(numout,*) '          Namelist nambdy_tide : tidal harmonic forcing at open boundaries'
            IF(lwp) WRITE(numout,*) '             read tidal data in 2d files: ', ln_bdytide_2ddta
            IF(lwp) WRITE(numout,*) '             assume complex conjugate   : ', ln_bdytide_conj
            IF(lwp) WRITE(numout,*) '             Number of tidal components to read: ', nb_harmo
            IF(lwp) THEN 
                    WRITE(numout,*) '             Tidal components: ' 
               DO itide = 1, nb_harmo
                  WRITE(numout,*)  '                 ', Wave(ntide(itide))%cname_tide 
               END DO
            ENDIF 
            IF(lwp) WRITE(numout,*) ' '

            ! Allocate space for tidal harmonics data - get size from OBC data arrays
            ! -----------------------------------------------------------------------

            ! JC: If FRS scheme is used, we assume that tidal is needed over the whole
            ! relaxation area      
            IF( cn_dyn2d(ib_bdy) == 'frs' ) THEN   ;   ilen0(:) = nblen   (:)
            ELSE                                   ;   ilen0(:) = nblenrim(:)
            ENDIF

            ALLOCATE( td%ssh0( ilen0(1), nb_harmo, 2 ) )
            ALLOCATE( td%ssh ( ilen0(1), nb_harmo, 2 ) )

            ALLOCATE( td%u0( ilen0(2), nb_harmo, 2 ) )
            ALLOCATE( td%u ( ilen0(2), nb_harmo, 2 ) )

            ALLOCATE( td%v0( ilen0(3), nb_harmo, 2 ) )
            ALLOCATE( td%v ( ilen0(3), nb_harmo, 2 ) )

            td%ssh0(:,:,:) = 0._wp
            td%ssh (:,:,:) = 0._wp
            td%u0  (:,:,:) = 0._wp
            td%u   (:,:,:) = 0._wp
            td%v0  (:,:,:) = 0._wp
            td%v   (:,:,:) = 0._wp

            IF( ln_bdytide_2ddta ) THEN
               ! It is assumed that each data file contains all complex harmonic amplitudes
               ! given on the global domain (ie global, jpiglo x jpjglo)
               !
               ALLOCATE( zti(jpi,jpj), ztr(jpi,jpj) )
               !
               ! SSH fields
               clfile = TRIM(filtide)//'_grid_T.nc'
               CALL iom_open( clfile , inum ) 
               igrd = 1                       ! Everything is at T-points here
               DO itide = 1, nb_harmo
                  CALL iom_get( inum, jpdom_autoglo, TRIM(Wave(ntide(itide))%cname_tide)//'_z1', ztr(:,:) )
                  CALL iom_get( inum, jpdom_autoglo, TRIM(Wave(ntide(itide))%cname_tide)//'_z2', zti(:,:) ) 
                  DO ib = 1, ilen0(igrd)
                     ii = idx_bdy(ib_bdy)%nbi(ib,igrd)
                     ij = idx_bdy(ib_bdy)%nbj(ib,igrd)
                     td%ssh0(ib,itide,1) = ztr(ii,ij)
                     td%ssh0(ib,itide,2) = zti(ii,ij)
                  END DO
               END DO 
               CALL iom_close( inum )
               !
               ! U fields
               clfile = TRIM(filtide)//'_grid_U.nc'
               CALL iom_open( clfile , inum ) 
               igrd = 2                       ! Everything is at U-points here
               DO itide = 1, nb_harmo
                  CALL iom_get  ( inum, jpdom_autoglo, TRIM(Wave(ntide(itide))%cname_tide)//'_u1', ztr(:,:) )
                  CALL iom_get  ( inum, jpdom_autoglo, TRIM(Wave(ntide(itide))%cname_tide)//'_u2', zti(:,:) )
                  DO ib = 1, ilen0(igrd)
                     ii = idx_bdy(ib_bdy)%nbi(ib,igrd)
                     ij = idx_bdy(ib_bdy)%nbj(ib,igrd)
                     td%u0(ib,itide,1) = ztr(ii,ij)
                     td%u0(ib,itide,2) = zti(ii,ij)
                  END DO
               END DO
               CALL iom_close( inum )
               !
               ! V fields
               clfile = TRIM(filtide)//'_grid_V.nc'
               CALL iom_open( clfile , inum ) 
               igrd = 3                       ! Everything is at V-points here
               DO itide = 1, nb_harmo
                  CALL iom_get  ( inum, jpdom_autoglo, TRIM(Wave(ntide(itide))%cname_tide)//'_v1', ztr(:,:) )
                  CALL iom_get  ( inum, jpdom_autoglo, TRIM(Wave(ntide(itide))%cname_tide)//'_v2', zti(:,:) )
                  DO ib = 1, ilen0(igrd)
                     ii = idx_bdy(ib_bdy)%nbi(ib,igrd)
                     ij = idx_bdy(ib_bdy)%nbj(ib,igrd)
                     td%v0(ib,itide,1) = ztr(ii,ij)
                     td%v0(ib,itide,2) = zti(ii,ij)
                  END DO
               END DO  
               CALL iom_close( inum )
               !
               DEALLOCATE( ztr, zti ) 
               !
            ELSE            
               !
               ! Read tidal data only on bdy segments
               ! 
               ALLOCATE( dta_read( MAXVAL(ilen0(1:3)), 1, 1 ) )
               !
               ! Set map structure
               ibmap_ptr(1)%ptr => idx_bdy(ib_bdy)%nbmap(:,1)   ;   ibmap_ptr(1)%ll_unstruc = ln_coords_file(ib_bdy)
               ibmap_ptr(2)%ptr => idx_bdy(ib_bdy)%nbmap(:,2)   ;   ibmap_ptr(2)%ll_unstruc = ln_coords_file(ib_bdy)
               ibmap_ptr(3)%ptr => idx_bdy(ib_bdy)%nbmap(:,3)   ;   ibmap_ptr(3)%ll_unstruc = ln_coords_file(ib_bdy)

               ! Open files and read in tidal forcing data
               ! -----------------------------------------

               DO itide = 1, nb_harmo
                  !                                                              ! SSH fields
                  clfile = TRIM(filtide)//TRIM(Wave(ntide(itide))%cname_tide)//'_grid_T.nc'
                  CALL iom_open( clfile, inum )
                  CALL fld_map( inum, 'z1' , dta_read(1:ilen0(1),1:1,1:1) , 1,  ibmap_ptr(1) )
                  td%ssh0(:,itide,1) = dta_read(1:ilen0(1),1,1)
                  CALL fld_map( inum, 'z2' , dta_read(1:ilen0(1),1:1,1:1) , 1,  ibmap_ptr(1) )
                  td%ssh0(:,itide,2) = dta_read(1:ilen0(1),1,1)
                  CALL iom_close( inum )
                  !                                                              ! U fields
                  clfile = TRIM(filtide)//TRIM(Wave(ntide(itide))%cname_tide)//'_grid_U.nc'
                  CALL iom_open( clfile, inum )
                  CALL fld_map( inum, 'u1' , dta_read(1:ilen0(2),1:1,1:1) , 1, ibmap_ptr(2) )
                  td%u0(:,itide,1) = dta_read(1:ilen0(2),1,1)
                  CALL fld_map( inum, 'u2' , dta_read(1:ilen0(2),1:1,1:1) , 1, ibmap_ptr(2) )
                  td%u0(:,itide,2) = dta_read(1:ilen0(2),1,1)
                  CALL iom_close( inum )
                  !                                                              ! V fields
                  clfile = TRIM(filtide)//TRIM(Wave(ntide(itide))%cname_tide)//'_grid_V.nc'
                  CALL iom_open( clfile, inum )
                  CALL fld_map( inum, 'v1' , dta_read(1:ilen0(3),1:1,1:1) , 1, ibmap_ptr(3) )
                  td%v0(:,itide,1) = dta_read(1:ilen0(3),1,1)
                  CALL fld_map( inum, 'v2' , dta_read(1:ilen0(3),1:1,1:1) , 1, ibmap_ptr(3) )
                  td%v0(:,itide,2) = dta_read(1:ilen0(3),1,1)
                  CALL iom_close( inum )
                  !
               END DO ! end loop on tidal components
               !
               DEALLOCATE( dta_read )
               !
            ENDIF ! ln_bdytide_2ddta=.true.
            !
            IF( ln_bdytide_conj ) THEN    ! assume complex conjugate in data files
               td%ssh0(:,:,2) = - td%ssh0(:,:,2)
               td%u0  (:,:,2) = - td%u0  (:,:,2)
               td%v0  (:,:,2) = - td%v0  (:,:,2)
            ENDIF
            !
            ! Allocate slow varying data in the case of time splitting:
            ! Do it anyway because at this stage knowledge of free surface scheme is unknown
            ALLOCATE( dta_bdy_s(ib_bdy)%ssh ( ilen0(1) ) )
            ALLOCATE( dta_bdy_s(ib_bdy)%u2d ( ilen0(2) ) )
            ALLOCATE( dta_bdy_s(ib_bdy)%v2d ( ilen0(3) ) )
            dta_bdy_s(ib_bdy)%ssh(:) = 0._wp
            dta_bdy_s(ib_bdy)%u2d(:) = 0._wp
            dta_bdy_s(ib_bdy)%v2d(:) = 0._wp
            !
         ENDIF ! nn_dyn2d_dta(ib_bdy) >= 2
         !
      END DO ! loop on ib_bdy
      !
   END SUBROUTINE bdytide_init


   SUBROUTINE bdytide_update( kt, idx, dta, td, jit, time_offset )
      !!----------------------------------------------------------------------
      !!                 ***  SUBROUTINE bdytide_update  ***
      !!                
      !! ** Purpose : - Add tidal forcing to ssh, u2d and v2d OBC data arrays. 
      !!                
      !!----------------------------------------------------------------------
      INTEGER          , INTENT(in   ) ::   kt          ! Main timestep counter
      TYPE(OBC_INDEX)  , INTENT(in   ) ::   idx         ! OBC indices
      TYPE(OBC_DATA)   , INTENT(inout) ::   dta         ! OBC external data
      TYPE(TIDES_DATA) , INTENT(inout) ::   td          ! tidal harmonics data
      INTEGER, OPTIONAL, INTENT(in   ) ::   jit         ! Barotropic timestep counter (for timesplitting option)
      INTEGER, OPTIONAL, INTENT(in   ) ::   time_offset ! time offset in units of timesteps. NB. if jit
      !                                                 ! is present then units = subcycle timesteps.
      !                                                 ! time_offset = 0  => get data at "now"    time level
      !                                                 ! time_offset = -1 => get data at "before" time level
      !                                                 ! time_offset = +1 => get data at "after"  time level
      !                                                 ! etc.
      !
      INTEGER  ::   itide, igrd, ib       ! dummy loop indices
      INTEGER  ::   time_add              ! time offset in units of timesteps
      INTEGER, DIMENSION(3) ::   ilen0    ! length of boundary data (from OBC arrays)
      REAL(wp) ::   z_arg, z_sarg, zflag, zramp   ! local scalars    
      REAL(wp), DIMENSION(jpmax_harmo) :: z_sist, z_cost
      !!----------------------------------------------------------------------
      !
      ilen0(1) =  SIZE(td%ssh(:,1,1))
      ilen0(2) =  SIZE(td%u(:,1,1))
      ilen0(3) =  SIZE(td%v(:,1,1))

      zflag=1
      IF ( PRESENT(jit) ) THEN
        IF ( jit /= 1 ) zflag=0
      ENDIF

      IF ( (nsec_day == NINT(0.5_wp * rdt) .OR. kt==nit000) .AND. zflag==1 ) THEN
        !
        kt_tide = kt - (nsec_day - 0.5_wp * rdt)/rdt
        !
        IF(lwp) THEN
           WRITE(numout,*)
           WRITE(numout,*) 'bdytide_update : (re)Initialization of the tidal bdy forcing at kt=',kt
           WRITE(numout,*) '~~~~~~~~~~~~~~ '
        ENDIF
        !
        CALL tide_init_elevation ( idx, td )
        CALL tide_init_velocities( idx, td )
        !
      ENDIF 

      time_add = 0
      IF( PRESENT(time_offset) ) THEN
         time_add = time_offset
      ENDIF
         
      IF( PRESENT(jit) ) THEN  
         z_arg = ((kt-kt_tide) * rdt + (jit+0.5_wp*(time_add-1)) * rdt / REAL(nn_baro,wp) )
      ELSE                              
         z_arg = ((kt-kt_tide)+time_add) * rdt
      ENDIF

      ! Linear ramp on tidal component at open boundaries 
      zramp = 1._wp
      IF (ln_tide_ramp) zramp = MIN(MAX( (z_arg + (kt_tide-nit000)*rdt)/(rdttideramp*rday),0._wp),1._wp)

      DO itide = 1, nb_harmo
         z_sarg = z_arg * omega_tide(itide)
         z_cost(itide) = COS( z_sarg )
         z_sist(itide) = SIN( z_sarg )
      END DO

      DO itide = 1, nb_harmo
         igrd=1                              ! SSH on tracer grid
         DO ib = 1, ilen0(igrd)
            dta%ssh(ib) = dta%ssh(ib) + zramp*(td%ssh(ib,itide,1)*z_cost(itide) + td%ssh(ib,itide,2)*z_sist(itide))
         END DO
         igrd=2                              ! U grid
         DO ib = 1, ilen0(igrd)
            dta%u2d(ib) = dta%u2d(ib) + zramp*(td%u  (ib,itide,1)*z_cost(itide) + td%u  (ib,itide,2)*z_sist(itide))
         END DO
         igrd=3                              ! V grid
         DO ib = 1, ilen0(igrd) 
            dta%v2d(ib) = dta%v2d(ib) + zramp*(td%v  (ib,itide,1)*z_cost(itide) + td%v  (ib,itide,2)*z_sist(itide))
         END DO
      END DO
      !
   END SUBROUTINE bdytide_update


   SUBROUTINE bdy_dta_tides( kt, kit, time_offset )
      !!----------------------------------------------------------------------
      !!                 ***  SUBROUTINE bdy_dta_tides  ***
      !!                
      !! ** Purpose : - Add tidal forcing to ssh, u2d and v2d OBC data arrays. 
      !!                
      !!----------------------------------------------------------------------
      INTEGER,           INTENT(in) ::   kt          ! Main timestep counter
      INTEGER, OPTIONAL, INTENT(in) ::   kit         ! Barotropic timestep counter (for timesplitting option)
      INTEGER, OPTIONAL, INTENT(in) ::   time_offset ! time offset in units of timesteps. NB. if kit
      !                                              ! is present then units = subcycle timesteps.
      !                                              ! time_offset = 0  => get data at "now"    time level
      !                                              ! time_offset = -1 => get data at "before" time level
      !                                              ! time_offset = +1 => get data at "after"  time level
      !                                              ! etc.
      !
      LOGICAL  ::   lk_first_btstp            ! =.TRUE. if time splitting and first barotropic step
      INTEGER  ::   itide, ib_bdy, ib, igrd   ! loop indices
      INTEGER  ::   time_add                  ! time offset in units of timesteps
      INTEGER, DIMENSION(jpbgrd)   ::   ilen0 
      INTEGER, DIMENSION(1:jpbgrd) ::   nblen, nblenrim  ! short cuts
      REAL(wp) ::   z_arg, z_sarg, zramp, zoff, z_cost, z_sist      
      !!----------------------------------------------------------------------
      !
      lk_first_btstp=.TRUE.
      IF ( PRESENT(kit).AND.( kit /= 1 ) ) THEN ; lk_first_btstp=.FALSE. ; ENDIF

      time_add = 0
      IF( PRESENT(time_offset) ) THEN
         time_add = time_offset
      ENDIF
      
      ! Absolute time from model initialization:   
      IF( PRESENT(kit) ) THEN  
         z_arg = ( kt + (kit+time_add-1) / REAL(nn_baro,wp) ) * rdt
      ELSE                              
         z_arg = ( kt + time_add ) * rdt
      ENDIF

      ! Linear ramp on tidal component at open boundaries 
      zramp = 1.
      IF (ln_tide_ramp) zramp = MIN(MAX( (z_arg - nit000*rdt)/(rdttideramp*rday),0.),1.)

      DO ib_bdy = 1,nb_bdy
         !
         IF( nn_dyn2d_dta(ib_bdy) >= 2 ) THEN
            !
            nblen(1:jpbgrd) = idx_bdy(ib_bdy)%nblen(1:jpbgrd)
            nblenrim(1:jpbgrd) = idx_bdy(ib_bdy)%nblenrim(1:jpbgrd)
            !
            IF( cn_dyn2d(ib_bdy) == 'frs' ) THEN   ;   ilen0(:) = nblen   (:)
            ELSE                                   ;   ilen0(:) = nblenrim(:)
            ENDIF     
            !
            ! We refresh nodal factors every day below
            ! This should be done somewhere else
            IF ( ( nsec_day == NINT(0.5_wp * rdt) .OR. kt==nit000 ) .AND. lk_first_btstp ) THEN
               !
               kt_tide = kt - (nsec_day - 0.5_wp * rdt)/rdt
               !
               IF(lwp) THEN
               WRITE(numout,*)
               WRITE(numout,*) 'bdy_tide_dta : Refresh nodal factors for tidal open bdy data at kt=',kt
               WRITE(numout,*) '~~~~~~~~~~~~~~ '
               ENDIF
               !
               CALL tide_init_elevation ( idx=idx_bdy(ib_bdy), td=tides(ib_bdy) )
               CALL tide_init_velocities( idx=idx_bdy(ib_bdy), td=tides(ib_bdy) )
               !
            ENDIF
            zoff = -kt_tide * rdt ! time offset relative to nodal factor computation time
            !
            ! If time splitting, initialize arrays from slow varying open boundary data:
            IF ( PRESENT(kit) ) THEN           
               IF ( dta_bdy(ib_bdy)%ll_ssh ) dta_bdy(ib_bdy)%ssh(1:ilen0(1)) = dta_bdy_s(ib_bdy)%ssh(1:ilen0(1))
               IF ( dta_bdy(ib_bdy)%ll_u2d ) dta_bdy(ib_bdy)%u2d(1:ilen0(2)) = dta_bdy_s(ib_bdy)%u2d(1:ilen0(2))
               IF ( dta_bdy(ib_bdy)%ll_v2d ) dta_bdy(ib_bdy)%v2d(1:ilen0(3)) = dta_bdy_s(ib_bdy)%v2d(1:ilen0(3))
            ENDIF
            !
            ! Update open boundary data arrays:
            DO itide = 1, nb_harmo
               !
               z_sarg = (z_arg + zoff) * omega_tide(itide)
               z_cost = zramp * COS( z_sarg )
               z_sist = zramp * SIN( z_sarg )
               !
               IF ( dta_bdy(ib_bdy)%ll_ssh ) THEN
                  igrd=1                              ! SSH on tracer grid
                  DO ib = 1, ilen0(igrd)
                     dta_bdy(ib_bdy)%ssh(ib) = dta_bdy(ib_bdy)%ssh(ib) + &
                        &                      ( tides(ib_bdy)%ssh(ib,itide,1)*z_cost + &
                        &                        tides(ib_bdy)%ssh(ib,itide,2)*z_sist )
                  END DO
               ENDIF
               !
               IF ( dta_bdy(ib_bdy)%ll_u2d ) THEN
                  igrd=2                              ! U grid
                  DO ib = 1, ilen0(igrd)
                     dta_bdy(ib_bdy)%u2d(ib) = dta_bdy(ib_bdy)%u2d(ib) + &
                        &                      ( tides(ib_bdy)%u(ib,itide,1)*z_cost + &
                        &                        tides(ib_bdy)%u(ib,itide,2)*z_sist )
                  END DO
               ENDIF
               !
               IF ( dta_bdy(ib_bdy)%ll_v2d ) THEN
                  igrd=3                              ! V grid
                  DO ib = 1, ilen0(igrd) 
                     dta_bdy(ib_bdy)%v2d(ib) = dta_bdy(ib_bdy)%v2d(ib) + &
                        &                      ( tides(ib_bdy)%v(ib,itide,1)*z_cost + &
                        &                        tides(ib_bdy)%v(ib,itide,2)*z_sist )
                  END DO
               ENDIF
            END DO             
         END IF
      END DO
      !
   END SUBROUTINE bdy_dta_tides


   SUBROUTINE tide_init_elevation( idx, td )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE tide_init_elevation  ***
      !!----------------------------------------------------------------------
      TYPE(OBC_INDEX) , INTENT(in   ) ::   idx   ! OBC indices
      TYPE(TIDES_DATA), INTENT(inout) ::   td    ! tidal harmonics data
      !
      INTEGER ::   itide, igrd, ib       ! dummy loop indices
      INTEGER, DIMENSION(1) ::   ilen0   ! length of boundary data (from OBC arrays)
      REAL(wp),ALLOCATABLE, DIMENSION(:) ::   mod_tide, phi_tide
      !!----------------------------------------------------------------------
      !
      igrd=1   
                              ! SSH on tracer grid.
      ilen0(1) =  SIZE(td%ssh0(:,1,1))
      !
      ALLOCATE( mod_tide(ilen0(igrd)), phi_tide(ilen0(igrd)) )
      !
      DO itide = 1, nb_harmo
         DO ib = 1, ilen0(igrd)
            mod_tide(ib)=SQRT(td%ssh0(ib,itide,1)**2.+td%ssh0(ib,itide,2)**2.)
            phi_tide(ib)=ATAN2(-td%ssh0(ib,itide,2),td%ssh0(ib,itide,1))
         END DO
         DO ib = 1 , ilen0(igrd)
            mod_tide(ib)=mod_tide(ib)*ftide(itide)
            phi_tide(ib)=phi_tide(ib)+v0tide(itide)+utide(itide)
         ENDDO
         DO ib = 1 , ilen0(igrd)
            td%ssh(ib,itide,1)= mod_tide(ib)*COS(phi_tide(ib))
            td%ssh(ib,itide,2)=-mod_tide(ib)*SIN(phi_tide(ib))
         ENDDO
      END DO
      !
      DEALLOCATE( mod_tide, phi_tide )
      !
   END SUBROUTINE tide_init_elevation


   SUBROUTINE tide_init_velocities( idx, td )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE tide_init_elevation  ***
      !!----------------------------------------------------------------------
      TYPE(OBC_INDEX) , INTENT(in   ) ::   idx   ! OBC indices
      TYPE(TIDES_DATA), INTENT(inout) ::   td    ! tidal harmonics data
      !
      INTEGER ::   itide, igrd, ib       ! dummy loop indices
      INTEGER, DIMENSION(3) ::   ilen0   ! length of boundary data (from OBC arrays)
      REAL(wp),ALLOCATABLE, DIMENSION(:) ::   mod_tide, phi_tide
      !!----------------------------------------------------------------------
      !
      ilen0(2) =  SIZE(td%u0(:,1,1))
      ilen0(3) =  SIZE(td%v0(:,1,1))
      !
      igrd=2                                 ! U grid.
      !
      ALLOCATE( mod_tide(ilen0(igrd)) , phi_tide(ilen0(igrd)) )
      !
      DO itide = 1, nb_harmo
         DO ib = 1, ilen0(igrd)
            mod_tide(ib)=SQRT(td%u0(ib,itide,1)**2.+td%u0(ib,itide,2)**2.)
            phi_tide(ib)=ATAN2(-td%u0(ib,itide,2),td%u0(ib,itide,1))
         END DO
         DO ib = 1, ilen0(igrd)
            mod_tide(ib)=mod_tide(ib)*ftide(itide)
            phi_tide(ib)=phi_tide(ib)+v0tide(itide)+utide(itide)
         ENDDO
         DO ib = 1, ilen0(igrd)
            td%u(ib,itide,1)= mod_tide(ib)*COS(phi_tide(ib))
            td%u(ib,itide,2)=-mod_tide(ib)*SIN(phi_tide(ib))
         ENDDO
      END DO
      !
      DEALLOCATE( mod_tide , phi_tide )
      !
      igrd=3                                 ! V grid.
      !
      ALLOCATE( mod_tide(ilen0(igrd)) , phi_tide(ilen0(igrd)) )

      DO itide = 1, nb_harmo
         DO ib = 1, ilen0(igrd)
            mod_tide(ib)=SQRT(td%v0(ib,itide,1)**2.+td%v0(ib,itide,2)**2.)
            phi_tide(ib)=ATAN2(-td%v0(ib,itide,2),td%v0(ib,itide,1))
         END DO
         DO ib = 1, ilen0(igrd)
            mod_tide(ib)=mod_tide(ib)*ftide(itide)
            phi_tide(ib)=phi_tide(ib)+v0tide(itide)+utide(itide)
         ENDDO
         DO ib = 1, ilen0(igrd)
            td%v(ib,itide,1)= mod_tide(ib)*COS(phi_tide(ib))
            td%v(ib,itide,2)=-mod_tide(ib)*SIN(phi_tide(ib))
         ENDDO
      END DO
      !
      DEALLOCATE( mod_tide, phi_tide )
      !
  END SUBROUTINE tide_init_velocities

   !!======================================================================
END MODULE bdytides

