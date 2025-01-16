MODULE limtrp
   !!======================================================================
   !!                       ***  MODULE limtrp   ***
   !! LIM transport ice model : sea-ice advection/diffusion
   !!======================================================================
   !! History : LIM-2 ! 2000-01 (M.A. Morales Maqueda, H. Goosse, and T. Fichefet)  Original code
   !!            3.0  ! 2005-11 (M. Vancoppenolle)   Multi-layer sea ice, salinity variations
   !!            4.0  ! 2011-02 (G. Madec) dynamical allocation
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3'                                      LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_trp      : advection/diffusion process of sea ice
   !!----------------------------------------------------------------------
   USE phycst         ! physical constant
   USE dom_oce        ! ocean domain
   USE sbc_oce        ! ocean surface boundary condition
   USE dom_ice        ! ice domain
   USE ice            ! ice variables
   USE limadv         ! ice advection
   USE limhdf         ! ice horizontal diffusion
   USE limvar         ! 
   !
   USE in_out_manager ! I/O manager
   USE lbclnk         ! lateral boundary conditions -- MPP exchanges
   USE lib_mpp        ! MPP library
   USE wrk_nemo       ! work arrays
   USE prtctl         ! Print control
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)  
   USE timing         ! Timing
   USE limcons        ! conservation tests
   USE limctl         ! control prints

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_trp    ! called by sbcice_lim

   INTEGER  ::   ncfl                 ! number of ice time step with CFL>1/2  

   !! * Substitution
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: limtrp.F90 9883 2018-07-05 15:57:31Z clem $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_trp( kt ) 
      !!-------------------------------------------------------------------
      !!                   ***  ROUTINE lim_trp ***
      !!                    
      !! ** purpose : advection/diffusion process of sea ice
      !!
      !! ** method  : variables included in the process are scalar,   
      !!     other values are considered as second order. 
      !!     For advection, a second order Prather scheme is used.  
      !!
      !! ** action :
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt           ! number of iteration
      !
      INTEGER  ::   ji, jj, jk, jm , jl, jt      ! dummy loop indices
      INTEGER  ::   initad                  ! number of sub-timestep for the advection
      REAL(wp) ::   zcfl , zusnit           !   -      -
      CHARACTER(len=80) ::   cltmp
      !
      REAL(wp), POINTER, DIMENSION(:,:)      ::   zsm
      REAL(wp), POINTER, DIMENSION(:,:,:)    ::   z0ice, z0snw, z0ai, z0es , z0smi , z0oi
      REAL(wp), POINTER, DIMENSION(:,:,:)    ::   z0opw
      REAL(wp), POINTER, DIMENSION(:,:,:,:)  ::   z0ei
      REAL(wp), POINTER, DIMENSION(:,:,:)    ::   zviold, zvsold, zsmvold  ! old ice volume...
      REAL(wp), POINTER, DIMENSION(:,:,:)    ::   zhimax                   ! old ice thickness
      REAL(wp), POINTER, DIMENSION(:,:)      ::   zatold, zeiold, zesold   ! old concentration, enthalpies
      REAL(wp), POINTER, DIMENSION(:,:,:)             ::   zhdfptab
      REAL(wp) ::    zdv, zvi, zvs, zsmv, zes, zei
      REAL(wp) ::    zvi_b, zsmv_b, zei_b, zfs_b, zfw_b, zft_b
      !!---------------------------------------------------------------------
      INTEGER                                ::  ihdf_vars  = 6  !!Number of variables in which we apply horizontal diffusion
                                                                   !!  inside limtrp for each ice category , not counting the 
                                                                   !!  variables corresponding to ice_layers 
      !!---------------------------------------------------------------------
      IF( nn_timing == 1 )  CALL timing_start('limtrp')

      CALL wrk_alloc( jpi,jpj,            zsm, zatold, zeiold, zesold )
      CALL wrk_alloc( jpi,jpj,jpl,        z0ice, z0snw, z0ai, z0es , z0smi , z0oi )
      CALL wrk_alloc( jpi,jpj,1,          z0opw )
      CALL wrk_alloc( jpi,jpj,nlay_i,jpl, z0ei )
      CALL wrk_alloc( jpi,jpj,jpl,        zhimax, zviold, zvsold, zsmvold )
      CALL wrk_alloc( jpi,jpj,jpl*(ihdf_vars + nlay_i)+1,zhdfptab)

      IF( numit == nstart .AND. lwp ) THEN
         WRITE(numout,*)
         IF( ln_limdyn ) THEN   ;   WRITE(numout,*) 'lim_trp : Ice transport '
         ELSE                   ;   WRITE(numout,*) 'lim_trp : No ice advection as ln_limdyn = ', ln_limdyn
         ENDIF
         WRITE(numout,*) '~~~~~~~~~~~~'
         ncfl = 0                ! nb of time step with CFL > 1/2
      ENDIF

      zsm(:,:) = e12t(:,:)
      
      !                             !-------------------------------------!
      IF( ln_limdyn ) THEN          !   Advection of sea ice properties   !
         !                          !-------------------------------------!

         ! conservation test
         IF( ln_limdiahsb )   CALL lim_cons_hsm(0, 'limtrp', zvi_b, zsmv_b, zei_b, zfw_b, zfs_b, zft_b)

         ! mass and salt flux init
         zviold(:,:,:)  = v_i(:,:,:)
         zvsold(:,:,:)  = v_s(:,:,:)
         zsmvold(:,:,:) = smv_i(:,:,:)
         zeiold(:,:)    = SUM( SUM( e_i(:,:,1:nlay_i,:), dim=4 ), dim=3 ) 
         zesold(:,:)    = SUM( SUM( e_s(:,:,1:nlay_s,:), dim=4 ), dim=3 ) 

         !--- Thickness correction init. -------------------------------
         zatold(:,:) = SUM( a_i(:,:,:), dim=3 )
         DO jl = 1, jpl
            DO jj = 1, jpj
               DO ji = 1, jpi
                  rswitch          = MAX( 0._wp , SIGN( 1._wp, a_i(ji,jj,jl) - epsi20 ) )
                  ht_i  (ji,jj,jl) = v_i (ji,jj,jl) / MAX( a_i(ji,jj,jl) , epsi20 ) * rswitch
                  ht_s  (ji,jj,jl) = v_s (ji,jj,jl) / MAX( a_i(ji,jj,jl) , epsi20 ) * rswitch
               END DO
            END DO
         END DO
         !---------------------------------------------------------------------
         ! Record max of the surrounding ice thicknesses for correction
         ! in case advection creates ice too thick.
         !---------------------------------------------------------------------
         zhimax(:,:,:) = ht_i(:,:,:) + ht_s(:,:,:)
         DO jl = 1, jpl
            DO jj = 2, jpjm1
               DO ji = 2, jpim1
                  zhimax(ji,jj,jl) = MAXVAL( ht_i(ji-1:ji+1,jj-1:jj+1,jl) + ht_s(ji-1:ji+1,jj-1:jj+1,jl) )
               END DO
            END DO
            CALL lbc_lnk(zhimax(:,:,jl),'T',1.)
         END DO
         
         !=============================!
         !==      Prather scheme     ==!
         !=============================!

         ! If ice drift field is too fast, use an appropriate time step for advection.         
         zcfl  =            MAXVAL( ABS( u_ice(:,:) ) * rdt_ice * r1_e1u(:,:) )         ! CFL test for stability
         zcfl  = MAX( zcfl, MAXVAL( ABS( v_ice(:,:) ) * rdt_ice * r1_e2v(:,:) ) )
         IF(lk_mpp )   CALL mpp_max( zcfl )

         IF( zcfl > 0.5 ) THEN   ;   initad = 2   ;   zusnit = 0.5_wp
         ELSE                    ;   initad = 1   ;   zusnit = 1.0_wp
         ENDIF

         IF( zcfl > 0.5_wp .AND. lwp )   ncfl = ncfl + 1
!!         IF( lwp ) THEN
!!            IF( ncfl > 0 ) THEN   
!!               WRITE(cltmp,'(i6.1)') ncfl
!!               CALL ctl_warn( 'lim_trp: ncfl= ', TRIM(cltmp), 'advective ice time-step using a split in sub-time-step ')
!!            ELSE
!!            !  WRITE(numout,*) 'lim_trp : CFL criterion for ice advection is always smaller than 1/2 '
!!            ENDIF
!!         ENDIF

         !-------------------------
         ! transported fields                                        
         !-------------------------
         z0opw(:,:,1) = ato_i(:,:) * e12t(:,:)             ! Open water area 
         DO jl = 1, jpl
            z0snw (:,:,jl)  = v_s  (:,:,jl) * e12t(:,:)    ! Snow volume
            z0ice(:,:,jl)   = v_i  (:,:,jl) * e12t(:,:)    ! Ice  volume
            z0ai  (:,:,jl)  = a_i  (:,:,jl) * e12t(:,:)    ! Ice area
            z0smi (:,:,jl)  = smv_i(:,:,jl) * e12t(:,:)    ! Salt content
            z0oi (:,:,jl)   = oa_i (:,:,jl) * e12t(:,:)    ! Age content
            z0es (:,:,jl)   = e_s  (:,:,1,jl) * e12t(:,:)  ! Snow heat content
           DO jk = 1, nlay_i
               z0ei  (:,:,jk,jl) = e_i  (:,:,jk,jl) * e12t(:,:) ! Ice  heat content
            END DO
         END DO


         IF( MOD( ( kt - 1) / nn_fsbc , 2 ) == 0 ) THEN       !==  odd ice time step:  adv_x then adv_y  ==!
            DO jt = 1, initad
               CALL lim_adv_x( zusnit, u_ice, 1._wp, zsm, z0opw (:,:,1), sxopw(:,:),   &             !--- ice open water area
                  &                                       sxxopw(:,:)  , syopw(:,:), syyopw(:,:), sxyopw(:,:)  )
               CALL lim_adv_y( zusnit, v_ice, 0._wp, zsm, z0opw (:,:,1), sxopw(:,:),   &
                  &                                       sxxopw(:,:)  , syopw(:,:), syyopw(:,:), sxyopw(:,:)  )
               DO jl = 1, jpl
                  CALL lim_adv_x( zusnit, u_ice, 1._wp, zsm, z0ice (:,:,jl), sxice(:,:,jl),   &    !--- ice volume  ---
                     &                                       sxxice(:,:,jl), syice(:,:,jl), syyice(:,:,jl), sxyice(:,:,jl)  )
                  CALL lim_adv_y( zusnit, v_ice, 0._wp, zsm, z0ice (:,:,jl), sxice(:,:,jl),   &
                     &                                       sxxice(:,:,jl), syice(:,:,jl), syyice(:,:,jl), sxyice(:,:,jl)  )
                  CALL lim_adv_x( zusnit, u_ice, 1._wp, zsm, z0snw (:,:,jl), sxsn (:,:,jl),   &    !--- snow volume  ---
                     &                                       sxxsn (:,:,jl), sysn (:,:,jl), syysn (:,:,jl), sxysn (:,:,jl)  )
                  CALL lim_adv_y( zusnit, v_ice, 0._wp, zsm, z0snw (:,:,jl), sxsn (:,:,jl),   &
                     &                                       sxxsn (:,:,jl), sysn (:,:,jl), syysn (:,:,jl), sxysn (:,:,jl)  )
                  CALL lim_adv_x( zusnit, u_ice, 1._wp, zsm, z0smi (:,:,jl), sxsal(:,:,jl),   &    !--- ice salinity ---
                     &                                       sxxsal(:,:,jl), sysal(:,:,jl), syysal(:,:,jl), sxysal(:,:,jl)  )
                  CALL lim_adv_y( zusnit, v_ice, 0._wp, zsm, z0smi (:,:,jl), sxsal(:,:,jl),   &
                     &                                       sxxsal(:,:,jl), sysal(:,:,jl), syysal(:,:,jl), sxysal(:,:,jl)  )
                  CALL lim_adv_x( zusnit, u_ice, 1._wp, zsm, z0oi  (:,:,jl), sxage(:,:,jl),   &    !--- ice age      ---     
                     &                                       sxxage(:,:,jl), syage(:,:,jl), syyage(:,:,jl), sxyage(:,:,jl)  )
                  CALL lim_adv_y( zusnit, v_ice, 0._wp, zsm, z0oi  (:,:,jl), sxage(:,:,jl),   &
                     &                                       sxxage(:,:,jl), syage(:,:,jl), syyage(:,:,jl), sxyage(:,:,jl)  )
                  CALL lim_adv_x( zusnit, u_ice, 1._wp, zsm, z0ai  (:,:,jl), sxa  (:,:,jl),   &    !--- ice concentrations ---
                     &                                       sxxa  (:,:,jl), sya  (:,:,jl), syya  (:,:,jl), sxya  (:,:,jl)  )
                  CALL lim_adv_y( zusnit, v_ice, 0._wp, zsm, z0ai  (:,:,jl), sxa  (:,:,jl),   & 
                     &                                       sxxa  (:,:,jl), sya  (:,:,jl), syya  (:,:,jl), sxya  (:,:,jl)  )
                  CALL lim_adv_x( zusnit, u_ice, 1._wp, zsm, z0es  (:,:,jl), sxc0 (:,:,jl),   &    !--- snow heat contents ---
                     &                                       sxxc0 (:,:,jl), syc0 (:,:,jl), syyc0 (:,:,jl), sxyc0 (:,:,jl)  )
                  CALL lim_adv_y( zusnit, v_ice, 0._wp, zsm, z0es  (:,:,jl), sxc0 (:,:,jl),   &
                     &                                       sxxc0 (:,:,jl), syc0 (:,:,jl), syyc0 (:,:,jl), sxyc0 (:,:,jl)  )
                  DO jk = 1, nlay_i                                                                !--- ice heat contents ---
                     CALL lim_adv_x( zusnit, u_ice, 1._wp, zsm, z0ei(:,:,jk,jl), sxe (:,:,jk,jl),   & 
                        &                                       sxxe(:,:,jk,jl), sye (:,:,jk,jl),   &
                        &                                       syye(:,:,jk,jl), sxye(:,:,jk,jl) )
                     CALL lim_adv_y( zusnit, v_ice, 0._wp, zsm, z0ei(:,:,jk,jl), sxe (:,:,jk,jl),   & 
                        &                                       sxxe(:,:,jk,jl), sye (:,:,jk,jl),   &
                        &                                       syye(:,:,jk,jl), sxye(:,:,jk,jl) )
                  END DO
               END DO
            END DO
         ELSE
            DO jt = 1, initad
               CALL lim_adv_y( zusnit, v_ice, 1._wp, zsm, z0opw (:,:,1), sxopw(:,:),   &             !--- ice open water area
                  &                                       sxxopw(:,:)  , syopw(:,:), syyopw(:,:), sxyopw(:,:)  )
               CALL lim_adv_x( zusnit, u_ice, 0._wp, zsm, z0opw (:,:,1), sxopw(:,:),   &
                  &                                       sxxopw(:,:)  , syopw(:,:), syyopw(:,:), sxyopw(:,:)  )
               DO jl = 1, jpl
                  CALL lim_adv_y( zusnit, v_ice, 1._wp, zsm, z0ice (:,:,jl), sxice(:,:,jl),   &    !--- ice volume  ---
                     &                                       sxxice(:,:,jl), syice(:,:,jl), syyice(:,:,jl), sxyice(:,:,jl)  )
                  CALL lim_adv_x( zusnit, u_ice, 0._wp, zsm, z0ice (:,:,jl), sxice(:,:,jl),   &
                     &                                       sxxice(:,:,jl), syice(:,:,jl), syyice(:,:,jl), sxyice(:,:,jl)  )
                  CALL lim_adv_y( zusnit, v_ice, 1._wp, zsm, z0snw (:,:,jl), sxsn (:,:,jl),   &    !--- snow volume  ---
                     &                                       sxxsn (:,:,jl), sysn (:,:,jl), syysn (:,:,jl), sxysn (:,:,jl)  )
                  CALL lim_adv_x( zusnit, u_ice, 0._wp, zsm, z0snw (:,:,jl), sxsn (:,:,jl),   &
                     &                                       sxxsn (:,:,jl), sysn (:,:,jl), syysn (:,:,jl), sxysn (:,:,jl)  )
                  CALL lim_adv_y( zusnit, v_ice, 1._wp, zsm, z0smi (:,:,jl), sxsal(:,:,jl),   &    !--- ice salinity ---
                     &                                       sxxsal(:,:,jl), sysal(:,:,jl), syysal(:,:,jl), sxysal(:,:,jl)  )
                  CALL lim_adv_x( zusnit, u_ice, 0._wp, zsm, z0smi (:,:,jl), sxsal(:,:,jl),   &
                     &                                       sxxsal(:,:,jl), sysal(:,:,jl), syysal(:,:,jl), sxysal(:,:,jl)  )
                  CALL lim_adv_y( zusnit, v_ice, 1._wp, zsm, z0oi  (:,:,jl), sxage(:,:,jl),   &   !--- ice age      ---
                     &                                       sxxage(:,:,jl), syage(:,:,jl), syyage(:,:,jl), sxyage(:,:,jl)  )
                  CALL lim_adv_x( zusnit, u_ice, 0._wp, zsm, z0oi  (:,:,jl), sxage(:,:,jl),   &
                     &                                       sxxage(:,:,jl), syage(:,:,jl), syyage(:,:,jl), sxyage(:,:,jl)  )
                  CALL lim_adv_y( zusnit, v_ice, 1._wp, zsm, z0ai  (:,:,jl), sxa  (:,:,jl),   &   !--- ice concentrations ---
                     &                                       sxxa  (:,:,jl), sya  (:,:,jl), syya  (:,:,jl), sxya  (:,:,jl)  )
                  CALL lim_adv_x( zusnit, u_ice, 0._wp, zsm, z0ai  (:,:,jl), sxa  (:,:,jl),   &
                     &                                       sxxa  (:,:,jl), sya  (:,:,jl), syya  (:,:,jl), sxya  (:,:,jl)  )
                  CALL lim_adv_y( zusnit, v_ice, 1._wp, zsm, z0es  (:,:,jl), sxc0 (:,:,jl),   &  !--- snow heat contents ---
                     &                                       sxxc0 (:,:,jl), syc0 (:,:,jl), syyc0 (:,:,jl), sxyc0 (:,:,jl)  )
                  CALL lim_adv_x( zusnit, u_ice, 0._wp, zsm, z0es  (:,:,jl), sxc0 (:,:,jl),   &
                     &                                       sxxc0 (:,:,jl), syc0 (:,:,jl), syyc0 (:,:,jl), sxyc0 (:,:,jl)  )
                  DO jk = 1, nlay_i                                                           !--- ice heat contents ---
                     CALL lim_adv_y( zusnit, v_ice, 1._wp, zsm, z0ei(:,:,jk,jl), sxe (:,:,jk,jl),   & 
                        &                                       sxxe(:,:,jk,jl), sye (:,:,jk,jl),   &
                        &                                       syye(:,:,jk,jl), sxye(:,:,jk,jl) )
                     CALL lim_adv_x( zusnit, u_ice, 0._wp, zsm, z0ei(:,:,jk,jl), sxe (:,:,jk,jl),   & 
                        &                                       sxxe(:,:,jk,jl), sye (:,:,jk,jl),   &
                        &                                       syye(:,:,jk,jl), sxye(:,:,jk,jl) )
                  END DO
               END DO
            END DO
         ENDIF

         !-------------------------------------------
         ! Recover the properties from their contents
         !-------------------------------------------
         ato_i(:,:) = z0opw(:,:,1) * r1_e12t(:,:) * tmask(:,:,1)
         DO jl = 1, jpl
            v_i  (:,:,jl)   = z0ice(:,:,jl) * r1_e12t(:,:) * tmask(:,:,1)
            v_s  (:,:,jl)   = z0snw(:,:,jl) * r1_e12t(:,:) * tmask(:,:,1)
            smv_i(:,:,jl)   = z0smi(:,:,jl) * r1_e12t(:,:) * tmask(:,:,1)
            oa_i (:,:,jl)   = z0oi (:,:,jl) * r1_e12t(:,:) * tmask(:,:,1)
            a_i  (:,:,jl)   = z0ai (:,:,jl) * r1_e12t(:,:) * tmask(:,:,1)
            e_s  (:,:,1,jl) = z0es (:,:,jl) * r1_e12t(:,:) * tmask(:,:,1)
            DO jk = 1, nlay_i
               e_i(:,:,jk,jl) = z0ei(:,:,jk,jl) * r1_e12t(:,:) * tmask(:,:,1)
            END DO
         END DO

         at_i(:,:) = a_i(:,:,1)      ! total ice fraction
         DO jl = 2, jpl
            at_i(:,:) = at_i(:,:) + a_i(:,:,jl)
         END DO

         !------------------------------------------------------------------------------!
         ! Diffusion of Ice fields                  
         !------------------------------------------------------------------------------!
         !------------------------------------
         !  Diffusion of other ice variables
         !------------------------------------
         jm=1
         DO jl = 1, jpl
         !                             ! Masked eddy diffusivity coefficient at ocean U- and V-points
         !   DO jj = 1, jpjm1                 ! NB: has not to be defined on jpj line and jpi row
         !      DO ji = 1 , fs_jpim1   ! vector opt.
         !         pahu(ji,jj) = ( 1._wp - MAX( 0._wp, SIGN( 1._wp, -a_i(ji  ,jj,jl) ) ) )   &
         !            &        * ( 1._wp - MAX( 0._wp, SIGN( 1._wp, -a_i(ji+1,jj,jl) ) ) ) * ahiu(ji,jj)
         !         pahv(ji,jj) = ( 1._wp - MAX( 0._wp, SIGN( 1._wp, -a_i(ji,jj  ,jl) ) ) )   &
         !            &        * ( 1._wp - MAX( 0._wp, SIGN( 1._wp,- a_i(ji,jj+1,jl) ) ) ) * ahiv(ji,jj)
         !      END DO
         !   END DO
            DO jj = 1, jpjm1                 ! NB: has not to be defined on jpj line and jpi row
               DO ji = 1 , fs_jpim1   ! vector opt.
                  pahu3D(ji,jj,jl) = ( 1._wp - MAX( 0._wp, SIGN( 1._wp, -a_i(ji  ,jj,  jl ) ) ) )   &
                  &                * ( 1._wp - MAX( 0._wp, SIGN( 1._wp, -a_i(ji+1,jj,  jl ) ) ) ) * ahiu(ji,jj)
                  pahv3D(ji,jj,jl) = ( 1._wp - MAX( 0._wp, SIGN( 1._wp, -a_i(ji,  jj,  jl ) ) ) )   &
                  &                * ( 1._wp - MAX( 0._wp, SIGN( 1._wp,- a_i(ji,  jj+1,jl ) ) ) ) * ahiv(ji,jj)
               END DO
            END DO

            zhdfptab(:,:,jm)= a_i  (:,:,  jl); jm = jm + 1    
            zhdfptab(:,:,jm)= v_i  (:,:,  jl); jm = jm + 1
            zhdfptab(:,:,jm)= v_s  (:,:,  jl); jm = jm + 1 
            zhdfptab(:,:,jm)= smv_i(:,:,  jl); jm = jm + 1
            zhdfptab(:,:,jm)= oa_i (:,:,  jl); jm = jm + 1
            zhdfptab(:,:,jm)= e_s  (:,:,1,jl); jm = jm + 1
         ! Sample of adding more variables to apply lim_hdf using lim_hdf optimization---
         !   zhdfptab(:,:,jm) = variable_1 (:,:,1,jl); jm = jm + 1  
         !   zhdfptab(:,:,jm) = variable_2 (:,:,1,jl); jm = jm + 1 
         !
         ! and in this example the parameter ihdf_vars musb be changed to 8 (necessary for allocation)
         !----------------------------------------------------------------------------------------
            DO jk = 1, nlay_i
              zhdfptab(:,:,jm)=e_i(:,:,jk,jl); jm= jm+1
            END DO
         END DO
         !
         !--------------------------------
         !  diffusion of open water area
         !--------------------------------
         !                             ! Masked eddy diffusivity coefficient at ocean U- and V-points
         !DO jj = 1, jpjm1                    ! NB: has not to be defined on jpj line and jpi row
         !   DO ji = 1 , fs_jpim1   ! vector opt.
         !      pahu(ji,jj) = ( 1._wp - MAX( 0._wp, SIGN( 1._wp, -at_i(ji  ,jj) ) ) )   &
         !         &        * ( 1._wp - MAX( 0._wp, SIGN( 1._wp, -at_i(ji+1,jj) ) ) ) * ahiu(ji,jj)
         !      pahv(ji,jj) = ( 1._wp - MAX( 0._wp, SIGN( 1._wp, -at_i(ji,jj  ) ) ) )   &
         !         &        * ( 1._wp - MAX( 0._wp, SIGN( 1._wp,- at_i(ji,jj+1) ) ) ) * ahiv(ji,jj)
         !   END DO
         !END DO
         
         DO jj = 1, jpjm1                    ! NB: has not to be defined on jpj line and jpi row
            DO ji = 1 , fs_jpim1   ! vector opt.
               pahu3D(ji,jj,jpl+1) = ( 1._wp - MAX( 0._wp, SIGN( 1._wp, -at_i(ji  ,jj) ) ) )   &
                  &                * ( 1._wp - MAX( 0._wp, SIGN( 1._wp, -at_i(ji+1,jj) ) ) ) * ahiu(ji,jj)
               pahv3D(ji,jj,jpl+1) = ( 1._wp - MAX( 0._wp, SIGN( 1._wp, -at_i(ji,jj  ) ) ) )   &
                  &                * ( 1._wp - MAX( 0._wp, SIGN( 1._wp,- at_i(ji,jj+1) ) ) ) * ahiv(ji,jj)
            END DO
         END DO
         !
         zhdfptab(:,:,jm)= ato_i  (:,:);
         CALL lim_hdf( zhdfptab, ihdf_vars, jpl, nlay_i) 

         jm=1
         DO jl = 1, jpl
            a_i  (:,:,  jl) = zhdfptab(:,:,jm); jm = jm + 1      
            v_i  (:,:,  jl) = zhdfptab(:,:,jm); jm = jm + 1 
            v_s  (:,:,  jl) = zhdfptab(:,:,jm); jm = jm + 1 
            smv_i(:,:,  jl) = zhdfptab(:,:,jm); jm = jm + 1 
            oa_i (:,:,  jl) = zhdfptab(:,:,jm); jm = jm + 1 
            e_s  (:,:,1,jl) = zhdfptab(:,:,jm); jm = jm + 1 
         ! Sample of adding more variables to apply lim_hdf---------
         !   variable_1  (:,:,1,jl) = zhdfptab(:,:, jm  ) ; jm + 1 
         !   variable_2  (:,:,1,jl) = zhdfptab(:,:, jm  ) ; jm + 1
         !-----------------------------------------------------------
            DO jk = 1, nlay_i
               e_i(:,:,jk,jl) = zhdfptab(:,:,jm);jm= jm + 1 
            END DO
         END DO

         ato_i  (:,:) = zhdfptab(:,:,jm)

         !------------------------------------------------------------------------------!
         ! limit ice properties after transport                           
         !------------------------------------------------------------------------------!
!!gm & cr   :  MAX should not be active if adv scheme is positive !
         DO jl = 1, jpl
            DO jj = 1, jpj
               DO ji = 1, jpi
                  v_s  (ji,jj,jl)   = MAX( 0._wp, v_s  (ji,jj,jl) )
                  v_i  (ji,jj,jl)   = MAX( 0._wp, v_i  (ji,jj,jl) )
                  smv_i(ji,jj,jl)   = MAX( 0._wp, smv_i(ji,jj,jl) )
                  oa_i (ji,jj,jl)   = MAX( 0._wp, oa_i (ji,jj,jl) )
                  a_i  (ji,jj,jl)   = MAX( 0._wp, a_i  (ji,jj,jl) )
                  e_s  (ji,jj,1,jl) = MAX( 0._wp, e_s  (ji,jj,1,jl) )
               END DO
            END DO

            DO jk = 1, nlay_i
               DO jj = 1, jpj
                  DO ji = 1, jpi
                     e_i(ji,jj,jk,jl) = MAX( 0._wp, e_i(ji,jj,jk,jl) )
                  END DO
               END DO
            END DO
         END DO
!!gm & cr 

         ! --- diags ---
         DO jj = 1, jpj
            DO ji = 1, jpi
               diag_trp_ei(ji,jj) = ( SUM( e_i(ji,jj,1:nlay_i,:) ) - zeiold(ji,jj) ) * r1_rdtice
               diag_trp_es(ji,jj) = ( SUM( e_s(ji,jj,1:nlay_s,:) ) - zesold(ji,jj) ) * r1_rdtice

               diag_trp_vi (ji,jj) = SUM(   v_i(ji,jj,:) -  zviold(ji,jj,:) ) * r1_rdtice
               diag_trp_vs (ji,jj) = SUM(   v_s(ji,jj,:) -  zvsold(ji,jj,:) ) * r1_rdtice
               diag_trp_smv(ji,jj) = SUM( smv_i(ji,jj,:) - zsmvold(ji,jj,:) ) * r1_rdtice
            END DO
         END DO

         ! zap small areas
         CALL lim_var_zapsmall

         !--- Thickness correction in case too high --------------------------------------------------------
         DO jl = 1, jpl
            DO jj = 1, jpj
               DO ji = 1, jpi

                  IF ( v_i(ji,jj,jl) > 0._wp ) THEN

                     rswitch          = MAX( 0._wp , SIGN( 1._wp, a_i(ji,jj,jl) - epsi20 ) )
                     ht_i  (ji,jj,jl) = v_i (ji,jj,jl) / MAX( a_i(ji,jj,jl) , epsi20 ) * rswitch
                     ht_s  (ji,jj,jl) = v_s (ji,jj,jl) / MAX( a_i(ji,jj,jl) , epsi20 ) * rswitch
                     
                     zvi  = v_i  (ji,jj,jl)
                     zvs  = v_s  (ji,jj,jl)
                     zsmv = smv_i(ji,jj,jl)
                     zes  = e_s  (ji,jj,1,jl)
                     zei  = SUM( e_i(ji,jj,1:nlay_i,jl) )

                     zdv  = v_i(ji,jj,jl) + v_s(ji,jj,jl) - zviold(ji,jj,jl) - zvsold(ji,jj,jl)  

                     IF ( ( zdv >  0.0 .AND. (ht_i(ji,jj,jl)+ht_s(ji,jj,jl)) > zhimax(ji,jj,jl) .AND. zatold(ji,jj) < 0.80 ) .OR. &
                        & ( zdv <= 0.0 .AND. (ht_i(ji,jj,jl)+ht_s(ji,jj,jl)) > zhimax(ji,jj,jl) ) ) THEN

                        rswitch        = MAX( 0._wp, SIGN( 1._wp, zhimax(ji,jj,jl) - epsi20 ) )
                        a_i(ji,jj,jl)  = rswitch * ( v_i(ji,jj,jl) + v_s(ji,jj,jl) ) / MAX( zhimax(ji,jj,jl), epsi20 )

                        ! small correction due to *rswitch for a_i
                        v_i  (ji,jj,jl)        = rswitch * v_i  (ji,jj,jl)
                        v_s  (ji,jj,jl)        = rswitch * v_s  (ji,jj,jl)
                        smv_i(ji,jj,jl)        = rswitch * smv_i(ji,jj,jl)
                        e_s(ji,jj,1,jl)        = rswitch * e_s(ji,jj,1,jl)
                        e_i(ji,jj,1:nlay_i,jl) = rswitch * e_i(ji,jj,1:nlay_i,jl)

                        ! Update mass fluxes
                        wfx_res(ji,jj) = wfx_res(ji,jj) - ( v_i(ji,jj,jl) - zvi ) * rhoic * r1_rdtice
                        wfx_snw(ji,jj) = wfx_snw(ji,jj) - ( v_s(ji,jj,jl) - zvs ) * rhosn * r1_rdtice
                        sfx_res(ji,jj) = sfx_res(ji,jj) - ( smv_i(ji,jj,jl) - zsmv ) * rhoic * r1_rdtice 
                        hfx_res(ji,jj) = hfx_res(ji,jj) + ( e_s(ji,jj,1,jl) - zes ) * r1_rdtice ! W.m-2 <0
                        hfx_res(ji,jj) = hfx_res(ji,jj) + ( SUM( e_i(ji,jj,1:nlay_i,jl) ) - zei ) * r1_rdtice ! W.m-2 <0

                     ENDIF

                  ENDIF

               END DO
            END DO
         END DO
         ! -------------------------------------------------
         
         !--------------------------------------
         ! Impose a_i < amax in mono-category
         !--------------------------------------
         !
         IF ( ( nn_monocat == 2 ) .AND. ( jpl == 1 ) ) THEN ! simple conservative piling, comparable with LIM2
            DO jj = 1, jpj
               DO ji = 1, jpi
                  a_i(ji,jj,1)  = MIN( a_i(ji,jj,1), rn_amax_2d(ji,jj) )
               END DO
            END DO
         ENDIF

         ! --- agglomerate variables -----------------
         vt_i (:,:) = 0._wp
         vt_s (:,:) = 0._wp
         at_i (:,:) = 0._wp
         DO jl = 1, jpl
            DO jj = 1, jpj
               DO ji = 1, jpi
                  vt_i(ji,jj) = vt_i(ji,jj) + v_i(ji,jj,jl)
                  vt_s(ji,jj) = vt_s(ji,jj) + v_s(ji,jj,jl)
                  at_i(ji,jj) = at_i(ji,jj) + a_i(ji,jj,jl)
               END DO
            END DO
         END DO

         ! --- open water = 1 if at_i=0 --------------------------------
         DO jj = 1, jpj
            DO ji = 1, jpi
               rswitch      = MAX( 0._wp , SIGN( 1._wp, - at_i(ji,jj) ) )
               ato_i(ji,jj) = rswitch + (1._wp - rswitch ) * ato_i(ji,jj)
            END DO
         END DO      

         ! conservation test
         IF( ln_limdiahsb ) CALL lim_cons_hsm(1, 'limtrp', zvi_b, zsmv_b, zei_b, zfw_b, zfs_b, zft_b)

      ENDIF

      ! -------------------------------------------------
      ! control prints
      ! -------------------------------------------------
      IF( ln_icectl )   CALL lim_prt( kt, iiceprt, jiceprt,-1, ' - ice dyn & trp - ' )
      !
      CALL wrk_dealloc( jpi,jpj,            zsm, zatold, zeiold, zesold )
      CALL wrk_dealloc( jpi,jpj,jpl,        z0ice, z0snw, z0ai, z0es , z0smi , z0oi )
      CALL wrk_dealloc( jpi,jpj,1,          z0opw )
      CALL wrk_dealloc( jpi,jpj,nlay_i,jpl, z0ei )
      CALL wrk_dealloc( jpi,jpj,jpl,        zviold, zvsold, zhimax, zsmvold )
      CALL wrk_dealloc( jpi,jpj,jpl*(ihdf_vars+nlay_i)+1,zhdfptab)
      !
      IF( nn_timing == 1 )  CALL timing_stop('limtrp')

   END SUBROUTINE lim_trp

#else
   !!----------------------------------------------------------------------
   !!   Default option         Empty Module                No sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_trp        ! Empty routine
   END SUBROUTINE lim_trp
#endif
   !!======================================================================
END MODULE limtrp

