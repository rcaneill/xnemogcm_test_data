MODULE limdyn
   !!======================================================================
   !!                     ***  MODULE  limdyn  ***
   !!   Sea-Ice dynamics :  
   !!======================================================================
   !! history :  1.0  ! 2002-08  (C. Ethe, G. Madec)  original VP code 
   !!            3.0  ! 2007-03  (MA Morales Maqueda, S. Bouillon, M. Vancoppenolle)  LIM3: EVP-Cgrid
   !!            3.5  ! 2011-02  (G. Madec) dynamical allocation
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3' :                                 LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!    lim_dyn      : computes ice velocities
   !!    lim_dyn_init : initialization and namelist read
   !!----------------------------------------------------------------------
   USE phycst           ! physical constants
   USE dom_oce          ! ocean space and time domain
   USE sbc_oce          ! Surface boundary condition: ocean fields
   USE sbc_ice          ! Surface boundary condition: ice   fields
   USE ice              ! LIM-3 variables
   USE dom_ice          ! LIM-3 domain
   USE limrhg           ! LIM-3 rheology
   USE lbclnk           ! lateral boundary conditions - MPP exchanges
   USE lib_mpp          ! MPP library
   USE wrk_nemo         ! work arrays
   USE in_out_manager   ! I/O manager
   USE prtctl           ! Print control
   USE lib_fortran      ! glob_sum
   USE timing          ! Timing
   USE limcons        ! conservation tests
   USE limvar

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_dyn   ! routine called by ice_step

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: limdyn.F90 7607 2017-01-25 15:37:31Z cetlod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_dyn( kt )
      !!-------------------------------------------------------------------
      !!               ***  ROUTINE lim_dyn  ***
      !!               
      !! ** Purpose :   compute ice velocity and ocean-ice stress
      !!                
      !! ** Method  : 
      !!
      !! ** Action  : - Initialisation
      !!              - Call of the dynamic routine for each hemisphere
      !!              - computation of the stress at the ocean surface         
      !!              - treatment of the case if no ice dynamic
      !!------------------------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt     ! number of iteration
      !!
      INTEGER  ::   ji, jj, jl, ja    ! dummy loop indices
      INTEGER  ::   i_j1, i_jpj       ! Starting/ending j-indices for rheology
      REAL(wp) ::   zcoef             ! local scalar
      REAL(wp), POINTER, DIMENSION(:)   ::   zswitch        ! i-averaged indicator of sea-ice
      REAL(wp), POINTER, DIMENSION(:)   ::   zmsk           ! i-averaged of tmask
      REAL(wp), POINTER, DIMENSION(:,:) ::   zu_io, zv_io   ! ice-ocean velocity
      !
      REAL(wp) :: zvi_b, zsmv_b, zei_b, zfs_b, zfw_b, zft_b 
     !!---------------------------------------------------------------------

      IF( nn_timing == 1 )  CALL timing_start('limdyn')

      CALL wrk_alloc( jpi, jpj, zu_io, zv_io )
      CALL wrk_alloc( jpj, zswitch, zmsk )

      CALL lim_var_agg(1)             ! aggregate ice categories

      IF( kt == nit000 )   CALL lim_dyn_init   ! Initialization (first time-step only)

      IF( ln_limdyn ) THEN
         !
         ! conservation test
         IF( ln_limdiahsb ) CALL lim_cons_hsm(0, 'limdyn', zvi_b, zsmv_b, zei_b, zfw_b, zfs_b, zft_b)

         u_ice_b(:,:) = u_ice(:,:) * umask(:,:,1)
         v_ice_b(:,:) = v_ice(:,:) * vmask(:,:,1)

         ! Rheology (ice dynamics)
         ! ========

         !  Define the j-limits where ice rheology is computed
         ! ---------------------------------------------------

         IF( lk_mpp .OR. lk_mpp_rep ) THEN                    ! mpp: compute over the whole domain
            i_j1 = 1
            i_jpj = jpj
            IF(ln_ctl) CALL prt_ctl_info( 'lim_dyn  :    i_j1 = ', ivar1=i_j1, clinfo2=' ij_jpj = ', ivar2=i_jpj )
            CALL lim_rhg( i_j1, i_jpj )
         ELSE                                 ! optimization of the computational area
            !
            DO jj = 1, jpj
               zswitch(jj) = SUM( 1.0 - at_i(:,jj) )   ! = REAL(jpj) if ocean everywhere on a j-line
               zmsk   (jj) = SUM( tmask(:,jj,1)    )   ! = 0         if land  everywhere on a j-line
            END DO

            IF( l_jeq ) THEN                     ! local domain include both hemisphere
               !                                 ! Rheology is computed in each hemisphere
               !                                 ! only over the ice cover latitude strip
               ! Northern hemisphere
               i_j1  = njeq
               i_jpj = jpj
               DO WHILE ( i_j1 <= jpj .AND. zswitch(i_j1) == FLOAT(jpi) .AND. zmsk(i_j1) /=0 )
                  i_j1 = i_j1 + 1
               END DO
               i_j1 = MAX( 1, i_j1-2 )
               IF(ln_ctl) CALL prt_ctl_info( 'lim_dyn  : NH  i_j1 = ', ivar1=i_j1, clinfo2=' ij_jpj = ', ivar2=i_jpj )
               CALL lim_rhg( i_j1, i_jpj )
               !
               ! Southern hemisphere
               i_j1  =  1
               i_jpj = njeq
               DO WHILE ( i_jpj >= 1 .AND. zswitch(i_jpj) == FLOAT(jpi) .AND. zmsk(i_jpj) /=0 )
                  i_jpj = i_jpj - 1
               END DO
               i_jpj = MIN( jpj, i_jpj+1 )
               IF(ln_ctl) CALL prt_ctl_info( 'lim_dyn  : SH  i_j1 = ', ivar1=i_j1, clinfo2=' ij_jpj = ', ivar2=i_jpj )
               !
               CALL lim_rhg( i_j1, i_jpj )
               !
            ELSE                                 ! local domain extends over one hemisphere only
               !                                 ! Rheology is computed only over the ice cover
               !                                 ! latitude strip
               i_j1  = 1
               DO WHILE ( i_j1 <= jpj .AND. zswitch(i_j1) == FLOAT(jpi) .AND. zmsk(i_j1) /=0 )
                  i_j1 = i_j1 + 1
               END DO
               i_j1 = MAX( 1, i_j1-2 )

               i_jpj  = jpj
               DO WHILE ( i_jpj >= 1  .AND. zswitch(i_jpj) == FLOAT(jpi) .AND. zmsk(i_jpj) /=0 )
                  i_jpj = i_jpj - 1
               END DO
               i_jpj = MIN( jpj, i_jpj+1)
               !
               IF(ln_ctl) CALL prt_ctl_info( 'lim_dyn  : one hemisphere:  i_j1 = ', ivar1=i_j1, clinfo2=' ij_jpj = ', ivar2=i_jpj )
               !
               CALL lim_rhg( i_j1, i_jpj )
               !
            ENDIF
            !
         ENDIF

         ! computation of friction velocity
         ! --------------------------------
         ! ice-ocean velocity at U & V-points (u_ice v_ice at U- & V-points ; ssu_m, ssv_m at U- & V-points)
         zu_io(:,:) = u_ice(:,:) - ssu_m(:,:)
         zv_io(:,:) = v_ice(:,:) - ssv_m(:,:)
         ! frictional velocity at T-point
         zcoef = 0.5_wp * rn_cio
         DO jj = 2, jpjm1 
            DO ji = fs_2, fs_jpim1   ! vector opt.
               ust2s(ji,jj) = zcoef * (  zu_io(ji,jj) * zu_io(ji,jj) + zu_io(ji-1,jj) * zu_io(ji-1,jj)   &
                  &                    + zv_io(ji,jj) * zv_io(ji,jj) + zv_io(ji,jj-1) * zv_io(ji,jj-1) ) * tmask(ji,jj,1)
            END DO
         END DO
         !
         ! conservation test
         IF( ln_limdiahsb ) CALL lim_cons_hsm(1, 'limdyn', zvi_b, zsmv_b, zei_b, zfw_b, zfs_b, zft_b)
         !
      ELSE      ! no ice dynamics : transmit directly the atmospheric stress to the ocean
         !
         zcoef = SQRT( 0.5_wp ) * r1_rau0
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               ust2s(ji,jj) = zcoef * SQRT(  utau(ji,jj) * utau(ji,jj) + utau(ji-1,jj) * utau(ji-1,jj)   &
                  &                        + vtau(ji,jj) * vtau(ji,jj) + vtau(ji,jj-1) * vtau(ji,jj-1) ) * tmask(ji,jj,1)
            END DO
         END DO
         !
      ENDIF
      CALL lbc_lnk( ust2s, 'T',  1. )   ! T-point

      IF(ln_ctl) THEN   ! Control print
         CALL prt_ctl_info(' ')
         CALL prt_ctl_info(' - Cell values : ')
         CALL prt_ctl_info('   ~~~~~~~~~~~~~ ')
         CALL prt_ctl(tab2d_1=ust2s     , clinfo1=' lim_dyn  : ust2s     :')
         CALL prt_ctl(tab2d_1=divu_i    , clinfo1=' lim_dyn  : divu_i    :')
         CALL prt_ctl(tab2d_1=delta_i   , clinfo1=' lim_dyn  : delta_i   :')
         CALL prt_ctl(tab2d_1=strength  , clinfo1=' lim_dyn  : strength  :')
         CALL prt_ctl(tab2d_1=e12t      , clinfo1=' lim_dyn  : cell area :')
         CALL prt_ctl(tab2d_1=at_i      , clinfo1=' lim_dyn  : at_i      :')
         CALL prt_ctl(tab2d_1=vt_i      , clinfo1=' lim_dyn  : vt_i      :')
         CALL prt_ctl(tab2d_1=vt_s      , clinfo1=' lim_dyn  : vt_s      :')
         CALL prt_ctl(tab2d_1=stress1_i , clinfo1=' lim_dyn  : stress1_i :')
         CALL prt_ctl(tab2d_1=stress2_i , clinfo1=' lim_dyn  : stress2_i :')
         CALL prt_ctl(tab2d_1=stress12_i, clinfo1=' lim_dyn  : stress12_i:')
         DO jl = 1, jpl
            CALL prt_ctl_info(' ')
            CALL prt_ctl_info(' - Category : ', ivar1=jl)
            CALL prt_ctl_info('   ~~~~~~~~~~')
            CALL prt_ctl(tab2d_1=a_i   (:,:,jl)   , clinfo1= ' lim_dyn  : a_i      : ')
            CALL prt_ctl(tab2d_1=ht_i  (:,:,jl)   , clinfo1= ' lim_dyn  : ht_i     : ')
            CALL prt_ctl(tab2d_1=ht_s  (:,:,jl)   , clinfo1= ' lim_dyn  : ht_s     : ')
            CALL prt_ctl(tab2d_1=v_i   (:,:,jl)   , clinfo1= ' lim_dyn  : v_i      : ')
            CALL prt_ctl(tab2d_1=v_s   (:,:,jl)   , clinfo1= ' lim_dyn  : v_s      : ')
            CALL prt_ctl(tab2d_1=e_s   (:,:,1,jl) , clinfo1= ' lim_dyn  : e_s      : ')
            CALL prt_ctl(tab2d_1=t_su  (:,:,jl)   , clinfo1= ' lim_dyn  : t_su     : ')
            CALL prt_ctl(tab2d_1=t_s   (:,:,1,jl) , clinfo1= ' lim_dyn  : t_snow   : ')
            CALL prt_ctl(tab2d_1=sm_i  (:,:,jl)   , clinfo1= ' lim_dyn  : sm_i     : ')
            CALL prt_ctl(tab2d_1=smv_i (:,:,jl)   , clinfo1= ' lim_dyn  : smv_i    : ')
            DO ja = 1, nlay_i
               CALL prt_ctl_info(' ')
               CALL prt_ctl_info(' - Layer : ', ivar1=ja)
               CALL prt_ctl_info('   ~~~~~~~')
               CALL prt_ctl(tab2d_1=t_i(:,:,ja,jl) , clinfo1= ' lim_dyn  : t_i      : ')
               CALL prt_ctl(tab2d_1=e_i(:,:,ja,jl) , clinfo1= ' lim_dyn  : e_i      : ')
            END DO
         END DO
      ENDIF
      !
      CALL wrk_dealloc( jpi, jpj, zu_io, zv_io )
      CALL wrk_dealloc( jpj, zswitch, zmsk )
      !
      IF( nn_timing == 1 )  CALL timing_stop('limdyn')

   END SUBROUTINE lim_dyn


   SUBROUTINE lim_dyn_init
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE lim_dyn_init  ***
      !!
      !! ** Purpose : Physical constants and parameters linked to the ice
      !!      dynamics
      !!
      !! ** Method  :  Read the namicedyn namelist and check the ice-dynamic
      !!       parameter values called at the first timestep (nit000)
      !!
      !! ** input   :   Namelist namicedyn
      !!-------------------------------------------------------------------
      INTEGER  ::   ios                 ! Local integer output status for namelist read
      NAMELIST/namicedyn/ nn_icestr, ln_icestr_bvf, rn_pe_rdg, rn_pstar, rn_crhg, rn_cio,  rn_creepl, rn_ecc, &
         &                nn_nevp, rn_relast
      !!-------------------------------------------------------------------

      REWIND( numnam_ice_ref )              ! Namelist namicedyn in reference namelist : Ice dynamics
      READ  ( numnam_ice_ref, namicedyn, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namicedyn in reference namelist', lwp )

      REWIND( numnam_ice_cfg )              ! Namelist namicedyn in configuration namelist : Ice dynamics
      READ  ( numnam_ice_cfg, namicedyn, IOSTAT = ios, ERR = 902 )
902   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namicedyn in configuration namelist', lwp )
      IF(lwm) WRITE ( numoni, namicedyn )
      
      IF(lwp) THEN                        ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'lim_dyn_init : ice parameters for ice dynamics '
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*)'    ice strength parameterization (0=Hibler 1=Rothrock)  nn_icestr     = ', nn_icestr 
         WRITE(numout,*)'    Including brine volume in ice strength comp.         ln_icestr_bvf = ', ln_icestr_bvf
         WRITE(numout,*)'   Ratio of ridging work to PotEner change in ridging    rn_pe_rdg     = ', rn_pe_rdg 
         WRITE(numout,*) '   drag coefficient for oceanic stress                  rn_cio        = ', rn_cio
         WRITE(numout,*) '   first bulk-rheology parameter                        rn_pstar      = ', rn_pstar
         WRITE(numout,*) '   second bulk-rhelogy parameter                        rn_crhg       = ', rn_crhg
         WRITE(numout,*) '   creep limit                                          rn_creepl     = ', rn_creepl
         WRITE(numout,*) '   eccentricity of the elliptical yield curve           rn_ecc        = ', rn_ecc
         WRITE(numout,*) '   number of iterations for subcycling                  nn_nevp       = ', nn_nevp
         WRITE(numout,*) '   ratio of elastic timescale over ice time step        rn_relast     = ', rn_relast
      ENDIF
      !
      usecc2 = 1._wp / ( rn_ecc * rn_ecc )
      rhoco  = rau0  * rn_cio
      !
   END SUBROUTINE lim_dyn_init

#else
   !!----------------------------------------------------------------------
   !!   Default option          Empty module           NO LIM sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_dyn         ! Empty routine
   END SUBROUTINE lim_dyn
#endif 

   !!======================================================================
END MODULE limdyn
