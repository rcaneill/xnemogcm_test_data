MODULE dommsk
   !!======================================================================
   !!                       ***  MODULE dommsk   ***
   !! Ocean initialization : domain land/sea mask 
   !!======================================================================
   !! History :  OPA  ! 1987-07  (G. Madec)  Original code
   !!            6.0  ! 1993-03  (M. Guyon)  symetrical conditions (M. Guyon)
   !!            7.0  ! 1996-01  (G. Madec)  suppression of common work arrays
   !!             -   ! 1996-05  (G. Madec)  mask computed from tmask
   !!            8.0  ! 1997-02  (G. Madec)  mesh information put in domhgr.F
   !!            8.1  ! 1997-07  (G. Madec)  modification of kbat and fmask
   !!             -   ! 1998-05  (G. Roullet)  free surface
   !!            8.2  ! 2000-03  (G. Madec)  no slip accurate
   !!             -   ! 2001-09  (J.-M. Molines)  Open boundaries
   !!   NEMO     1.0  ! 2002-08  (G. Madec)  F90: Free form and module
   !!             -   ! 2005-11  (V. Garnier) Surface pressure gradient organization
   !!            3.2  ! 2009-07  (R. Benshila) Suppression of rigid-lid option
   !!            3.6  ! 2015-05  (P. Mathiot) ISF: add wmask,wumask and wvmask
   !!            4.0  ! 2016-06  (G. Madec, S. Flavoni)  domain configuration / user defined interface
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dom_msk       : compute land/ocean mask
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space and time domain
   USE domisf         ! domain: ice shelf
   USE domwri         ! domain: write the meshmask file
   USE usrdef_fmask   ! user defined fmask
   !
   USE in_out_manager ! I/O manager
   USE iom            ! IOM library
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp        ! Massively Parallel Processing library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dom_msk    ! routine called by inidom.F90

   !                            !!* Namelist namlbc : lateral boundary condition *
   REAL(wp)        :: rn_shlat   ! type of lateral boundary condition on velocity
   LOGICAL, PUBLIC :: ln_vorlat  !  consistency of vorticity boundary condition 
   !                                            with analytical eqs.

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: dommsk.F90 10425 2018-12-19 21:54:16Z smasson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dom_msk( k_top, k_bot )
      !!---------------------------------------------------------------------
      !!                 ***  ROUTINE dom_msk  ***
      !!
      !! ** Purpose :   Compute land/ocean mask arrays at tracer points, hori-
      !!      zontal velocity points (u & v), vorticity points (f) points.
      !!
      !! ** Method  :   The ocean/land mask  at t-point is deduced from ko_top 
      !!      and ko_bot, the indices of the fist and last ocean t-levels which 
      !!      are either defined in usrdef_zgr or read in zgr_read.
      !!                The velocity masks (umask, vmask, wmask, wumask, wvmask) 
      !!      are deduced from a product of the two neighboring tmask.
      !!                The vorticity mask (fmask) is deduced from tmask taking
      !!      into account the choice of lateral boundary condition (rn_shlat) :
      !!         rn_shlat = 0, free slip  (no shear along the coast)
      !!         rn_shlat = 2, no slip  (specified zero velocity at the coast)
      !!         0 < rn_shlat < 2, partial slip   | non-linear velocity profile
      !!         2 < rn_shlat, strong slip        | in the lateral boundary layer
      !!
      !!      tmask_i : interior ocean mask at t-point, i.e. excluding duplicated
      !!                rows/lines due to cyclic or North Fold boundaries as well
      !!                as MPP halos.
      !!      tmask_h : halo mask at t-point, i.e. excluding duplicated rows/lines
      !!                due to cyclic or North Fold boundaries as well as MPP halos.
      !!
      !! ** Action :   tmask, umask, vmask, wmask, wumask, wvmask : land/ocean mask 
      !!                         at t-, u-, v- w, wu-, and wv-points (=0. or 1.)
      !!               fmask   : land/ocean mask at f-point (=0., or =1., or 
      !!                         =rn_shlat along lateral boundaries)
      !!               tmask_i : interior ocean mask 
      !!               tmask_h : halo mask
      !!               ssmask , ssumask, ssvmask, ssfmask : 2D ocean mask
      !!----------------------------------------------------------------------

      INTEGER, DIMENSION(:,:), INTENT(in) ::   k_top, k_bot   ! first and last ocean level
      !
      INTEGER  ::   ji, jj, jk     ! dummy loop indices
      INTEGER  ::   iif, iil       ! local integers
      INTEGER  ::   ijf, ijl       !   -       -
      INTEGER  ::   iktop, ikbot   !   -       -
      INTEGER  ::   ios, inum
      REAL(wp), ALLOCATABLE, DIMENSION(:,:) ::   zwf   ! 2D workspace
      !!
      NAMELIST/namlbc/ rn_shlat, ln_vorlat
      !!---------------------------------------------------------------------
      !
    !  REWIND( numnam_ref )              ! Namelist namlbc in reference namelist : Lateral momentum boundary condition
      READ  ( numnam_ref, namlbc, IOSTAT = ios, ERR = 901 )
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namlbc in reference namelist')
    !  REWIND( numnam_cfg )              ! Namelist namlbc in configuration namelist : Lateral momentum boundary condition
      READ  ( numnam_cfg, namlbc, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namlbc in configuration namelist')
      IF(lwm) WRITE ( numond, namlbc )
      
      IF(lwp) THEN                  ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'dommsk : ocean mask '
         WRITE(numout,*) '~~~~~~'
         WRITE(numout,*) '   Namelist namlbc'
         WRITE(numout,*) '      lateral momentum boundary cond.    rn_shlat  = ',rn_shlat
         WRITE(numout,*) '      consistency with analytical form   ln_vorlat = ',ln_vorlat 
      ENDIF
      !
      IF(lwp) WRITE(numout,*)
      IF     (      rn_shlat == 0.               ) THEN   ;   IF(lwp) WRITE(numout,*) '   ==>>>   ocean lateral  free-slip'
      ELSEIF (      rn_shlat == 2.               ) THEN   ;   IF(lwp) WRITE(numout,*) '   ==>>>   ocean lateral  no-slip'
      ELSEIF ( 0. < rn_shlat .AND. rn_shlat < 2. ) THEN   ;   IF(lwp) WRITE(numout,*) '   ==>>>   ocean lateral  partial-slip'
      ELSEIF ( 2. < rn_shlat                     ) THEN   ;   IF(lwp) WRITE(numout,*) '   ==>>>   ocean lateral  strong-slip'
      ELSE
         CALL ctl_stop( 'dom_msk: wrong value for rn_shlat (i.e. a negalive value). We stop.' )
      ENDIF

     ! 1. Ocean/land mask at t-point (computed from mbathy)
     ! -----------------------------
     ! N.B. tmask has already the right boundary conditions since mbathy is ok
     !
!      tmask(:,:,:) = 0._wp
!      DO jk = 1, jpk
!         DO jj = 1, jpj
!            DO ji = 1, jpi
!               IF(      ( REAL( mbathy (ji,jj) - jk, wp ) + 0.1_wp >= 0._wp )         &
!               &  .AND. ( REAL( misfdep(ji,jj) - jk, wp ) - 0.1_wp <= 0._wp ) ) THEN
!                  tmask(ji,jj,jk) = 1._wp
!               END IF  
!            END DO
!         END DO
!      END DO    
 
!      IF ( ln_isfsubgl ) CALL zgr_isf_subgl

      !  Ocean/land mask at t-point  (computed from ko_top and ko_bot)
      ! ----------------------------
      !
      tmask(:,:,:) = 0._wp
      IF( ln_read_cfg) THEN
         DO jj = 1, jpj
            DO ji = 1, jpi
               iktop = k_top(ji,jj)
               ikbot = k_bot(ji,jj)
               IF( iktop /= 0 ) THEN       ! water in the column
                  tmask(ji,jj,iktop:ikbot  ) = 1._wp
               ENDIF
            END DO  
         END DO  
      ELSE
         DO jk = 1, jpk
            DO jj = 1, jpj
               DO ji = 1, jpi
                  IF(      ( REAL( mbathy (ji,jj) - jk, wp ) + 0.1_wp >= 0._wp )         &
                  &  .AND. ( REAL( misfdep(ji,jj) - jk, wp ) - 0.1_wp <= 0._wp ) ) THEN
                     tmask(ji,jj,jk) = 1._wp
                  END IF
               END DO
            END DO
         END DO
         IF ( ln_isfsubgl ) CALL zgr_isf_subgl
      ENDIF


!SF  add here lbc_lnk: bug not still understood : cause now domain configuration is read !
!!gm I don't understand why...  
      CALL lbc_lnk( 'dommsk', tmask  , 'T', 1._wp )      ! Lateral boundary conditions

      !  Ocean/land mask at u-, v-, and f-points   (computed from tmask)
      ! ----------------------------------------
      ! NB: at this point, fmask is designed for free slip lateral boundary condition
      DO jk = 1, jpk
         DO jj = 1, jpjm1
            DO ji = 1, fs_jpim1   ! vector loop
               umask(ji,jj,jk) = tmask(ji,jj  ,jk) * tmask(ji+1,jj  ,jk)
               vmask(ji,jj,jk) = tmask(ji,jj  ,jk) * tmask(ji  ,jj+1,jk)
            END DO
            DO ji = 1, jpim1      ! NO vector opt.
               fmask(ji,jj,jk) = tmask(ji,jj  ,jk) * tmask(ji+1,jj  ,jk)   &
                  &            * tmask(ji,jj+1,jk) * tmask(ji+1,jj+1,jk)
            END DO
         END DO
      END DO
      CALL lbc_lnk_multi( 'dommsk', umask, 'U', 1., vmask, 'V', 1., fmask, 'F', 1. )      ! Lateral boundary conditions
 
      ! Ocean/land mask at wu-, wv- and w points    (computed from tmask)
      !-----------------------------------------
      wmask (:,:,1) = tmask(:,:,1)     ! surface
      DO jk = 2, jpk                   ! interior values
         wmask (:,:,jk) = tmask(:,:,jk) * tmask(:,:,jk-1)
      END DO


      ! Ocean/land column mask at t-, u-, and v-points   (i.e. at least 1 wet cell in the vertical)
      ! ----------------------------------------------
      ssmask (:,:) = MAXVAL( tmask(:,:,:), DIM=3 )
      ssumask(:,:) = MAXVAL( umask(:,:,:), DIM=3 )
      ssvmask(:,:) = MAXVAL( vmask(:,:,:), DIM=3 )
      ssfmask(:,:) = MAXVAL( fmask(:,:,:), DIM=3 )

      ! Interior domain mask  (used for global sum)
      ! --------------------
      !
      iif = nn_hls   ;   iil = jpi - nn_hls + 1
      ijf = nn_hls   ;   ijl = jpj - nn_hls + 1
      !
      !                          ! halo mask : 0 on the halo and 1 elsewhere
      tmask_h(:,:) = 1._wp                  
      tmask_h( 1 :iif,   :   ) = 0._wp      ! first columns
      tmask_h(iil:jpi,   :   ) = 0._wp      ! last  columns (including mpp extra columns)
      tmask_h(   :   , 1 :ijf) = 0._wp      ! first rows
      tmask_h(   :   ,ijl:jpj) = 0._wp      ! last  rows (including mpp extra rows)
      !
      !                          ! north fold mask
      tpol(1:jpiglo) = 1._wp 
      fpol(1:jpiglo) = 1._wp
      IF( jperio == 3 .OR. jperio == 4 ) THEN      ! T-point pivot
         tpol(jpiglo/2+1:jpiglo) = 0._wp
         fpol(     1    :jpiglo) = 0._wp
         IF( mjg(Nje0) == jpjglo ) THEN                  ! only half of the nlcj-1 row for tmask_h
            DO ji = iif+1, iil-1
               tmask_h(ji,Nje0-1) = tmask_h(ji,Nje0-1) * tpol(mig(ji))
            END DO
         ENDIF
      ENDIF
      !
      IF( jperio == 5 .OR. jperio == 6 ) THEN      ! F-point pivot
         tpol(     1    :jpiglo) = 0._wp
         fpol(jpiglo/2+1:jpiglo) = 0._wp
      ENDIF
      !
      !                          ! interior mask : 2D ocean mask x halo mask 
      tmask_i(:,:) = ssmask(:,:) * tmask_h(:,:)


      ! Lateral boundary conditions on velocity (modify fmask)
      ! ---------------------------------------  
      IF( rn_shlat /= 0 ) THEN      ! Not free-slip lateral boundary condition
         !
         ALLOCATE( zwf(jpi,jpj) )
         !
         DO jk = 1, jpk
            zwf(:,:) = fmask(:,:,jk)         
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  IF( fmask(ji,jj,jk) == 0._wp ) THEN
                     fmask(ji,jj,jk) = rn_shlat * MIN( 1._wp , MAX( zwf(ji+1,jj), zwf(ji,jj+1),   &
                        &                                           zwf(ji-1,jj), zwf(ji,jj-1)  )  )
                  ENDIF
               END DO
            END DO
            DO jj = 2, jpjm1
               IF( fmask(1,jj,jk) == 0._wp ) THEN
                  fmask(1  ,jj,jk) = rn_shlat * MIN( 1._wp , MAX( zwf(2,jj), zwf(1,jj+1), zwf(1,jj-1) ) )
               ENDIF
               IF( fmask(jpi,jj,jk) == 0._wp ) THEN
                  fmask(jpi,jj,jk) = rn_shlat * MIN( 1._wp , MAX( zwf(jpi,jj+1), zwf(jpim1,jj), zwf(jpi,jj-1) ) )
               ENDIF
            END DO         
            DO ji = 2, jpim1
               IF( fmask(ji,1,jk) == 0._wp ) THEN
                  fmask(ji, 1 ,jk) = rn_shlat * MIN( 1._wp , MAX( zwf(ji+1,1), zwf(ji,2), zwf(ji-1,1) ) )
               ENDIF
               IF( fmask(ji,jpj,jk) == 0._wp ) THEN
                  fmask(ji,jpj,jk) = rn_shlat * MIN( 1._wp , MAX( zwf(ji+1,jpj), zwf(ji-1,jpj), zwf(ji,jpjm1) ) )
               ENDIF
            END DO
#if defined key_agrif 
            IF( .NOT. AGRIF_Root() ) THEN 
               IF(lk_east)  fmask(jpi-1 , :     ,jk) = 0.e0      ! east 
               IF(lk_west)  fmask(1      , :     ,jk) = 0.e0      ! west 
               IF(lk_north) fmask(:      ,jpj-1 ,jk) = 0.e0      ! north 
               IF(lk_south) fmask(:      ,1      ,jk) = 0.e0      ! south 
            ENDIF 
#endif 
         END DO
         !
         DEALLOCATE( zwf )
         !
         CALL lbc_lnk( 'dommsk', fmask, 'F', 1._wp )      ! Lateral boundary conditions on fmask
         !
         ! CAUTION : The fmask may be further modified in dyn_vor_init ( dynvor.F90 ) depending on ln_vorlat
         !
      ENDIF
      
      ! User defined alteration of fmask (use to reduce ocean transport in specified straits)
      ! -------------------------------- 
      ! write mesh mask 
      IF ( nn_msh > 0 ) CALL dom_wri 
      !
      CALL usr_def_fmask( cp_cfg, jp_cfg, fmask )
      !
   END SUBROUTINE dom_msk
   
   !!======================================================================
END MODULE dommsk
