MODULE limhdf
   !!======================================================================
   !!                    ***  MODULE limhdf   ***
   !! LIM ice model : horizontal diffusion of sea-ice quantities
   !!======================================================================
   !! History :  LIM  !  2000-01 (LIM) Original code
   !!             -   !  2001-05 (G. Madec, R. Hordoir) opa norm
   !!            1.0  !  2002-08 (C. Ethe)  F90, free form
   !!            3.0  !  2015-08 (O. Tintó and M. Castrillo)  added lim_hdf (multiple)
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3'                                      LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_hdf       : diffusion trend on sea-ice variable
   !!   lim_hdf_init  : initialisation of diffusion trend on sea-ice variable
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean domain
   USE ice            ! LIM-3: ice variables
   USE lbclnk         ! lateral boundary condition - MPP exchanges
   USE lib_mpp        ! MPP library
   USE wrk_nemo       ! work arrays
   USE prtctl         ! Print control
   USE in_out_manager ! I/O manager
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)  

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_hdf ! called by lim_trp
   PUBLIC   lim_hdf_init    ! called by sbc_lim_init

   LOGICAL  ::   linit = .TRUE.                             ! initialization flag (set to flase after the 1st call)
   INTEGER  ::   nn_convfrq                                 !:  convergence check frequency of the Crant-Nicholson scheme
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   efact   ! metric coefficient

   !! * Substitution 
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2010)
   !! $Id: limhdf.F90 7621 2017-01-31 08:43:47Z cetlod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_hdf( ptab , ihdf_vars , jpl , nlay_i )
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE lim_hdf  ***
      !!
      !! ** purpose :   Compute and add the diffusive trend on sea-ice variables
      !!
      !! ** method  :   Second order diffusive operator evaluated using a
      !!              Cranck-Nicholson time Scheme.
      !!
      !! ** Action  :    update ptab with the diffusive contribution
      !!-------------------------------------------------------------------
      INTEGER                           :: jpl, nlay_i, isize, ihdf_vars
      REAL(wp),  DIMENSION(:,:,:), INTENT( inout ),TARGET ::   ptab    ! Field on which the diffusion is applied
      !
      INTEGER                           ::  ji, jj, jk, jl , jm               ! dummy loop indices
      INTEGER                           ::  iter, ierr           ! local integers
      REAL(wp)                          ::  zrlxint     ! local scalars
      REAL(wp), POINTER , DIMENSION ( : )        :: zconv     ! local scalars
      REAL(wp), POINTER , DIMENSION(:,:,:) ::  zrlx,zdiv0, ztab0
      REAL(wp), POINTER , DIMENSION(:,:) ::  zflu, zflv, zdiv
      CHARACTER(lc)                     ::  charout                   ! local character
      REAL(wp), PARAMETER               ::  zrelax = 0.5_wp           ! relaxation constant for iterative procedure
      REAL(wp), PARAMETER               ::  zalfa  = 0.5_wp           ! =1.0/0.5/0.0 = implicit/Cranck-Nicholson/explicit
      INTEGER , PARAMETER               ::  its    = 100              ! Maximum number of iteration
      !!-------------------------------------------------------------------
      TYPE(arrayptr)   , ALLOCATABLE, DIMENSION(:) ::   pt2d_array, zrlx_array
      CHARACTER(len=1) , ALLOCATABLE, DIMENSION(:) ::   type_array ! define the nature of ptab array grid-points
      !                                                            ! = T , U , V , F , W and I points
      REAL(wp)        , ALLOCATABLE, DIMENSION(:)  ::   psgn_array    ! =-1 the sign change across the north fold boundary

     !!--------------------------------------------------------------------- 

      !                       !==  Initialisation  ==!
      ! +1 open water diffusion
      isize = jpl*(ihdf_vars+nlay_i)+1
      ALLOCATE( zconv (isize) )
      ALLOCATE( pt2d_array(isize) , zrlx_array(isize) )
      ALLOCATE( type_array(isize) )
      ALLOCATE( psgn_array(isize) )
      
      CALL wrk_alloc( jpi, jpj, isize, zrlx, zdiv0, ztab0 )
      CALL wrk_alloc( jpi, jpj, zflu, zflv, zdiv )

      DO jk= 1 , isize
         pt2d_array(jk)%pt2d=>ptab(:,:,jk)
         zrlx_array(jk)%pt2d=>zrlx(:,:,jk)
         type_array(jk)='T'
         psgn_array(jk)=1.
      END DO

      !
      IF( linit ) THEN              ! Metric coefficient (compute at the first call and saved in efact)
         ALLOCATE( efact(jpi,jpj) , STAT=ierr )
         IF( lk_mpp    )   CALL mpp_sum( ierr )
         IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'lim_hdf : unable to allocate arrays' )
         DO jj = 2, jpjm1
            DO ji = fs_2 , fs_jpim1   ! vector opt.
               efact(ji,jj) = ( e2u(ji,jj) + e2u(ji-1,jj) + e1v(ji,jj) + e1v(ji,jj-1) ) * r1_e12t(ji,jj)
            END DO
         END DO
         linit = .FALSE.
      ENDIF
      !                             ! Time integration parameters
      !
      zflu (jpi,: ) = 0._wp
      zflv (jpi,: ) = 0._wp

      DO jk=1 , isize
         ztab0(:, : , jk ) = ptab(:,:,jk)      ! Arrays initialization
         zdiv0(:, 1 , jk ) = 0._wp
         zdiv0(:,jpj, jk ) = 0._wp
         zdiv0(1,  :, jk ) = 0._wp
         zdiv0(jpi,:, jk ) = 0._wp
      END DO

      zconv = 1._wp           !==  horizontal diffusion using a Crant-Nicholson scheme  ==!
      iter  = 0
      !
      DO WHILE( MAXVAL(zconv(:)) > ( 2._wp * 1.e-04 ) .AND. iter <= its )   ! Sub-time step loop
         !
         iter = iter + 1                                 ! incrementation of the sub-time step number
         !
         DO jk = 1 , isize
            jl = (jk-1) /( ihdf_vars+nlay_i)+1
            IF (zconv(jk) > ( 2._wp * 1.e-04 )) THEN
               DO jj = 1, jpjm1                                ! diffusive fluxes in U- and V- direction
                  DO ji = 1 , fs_jpim1   ! vector opt.
                     zflu(ji,jj) = pahu3D(ji,jj,jl) * e2u(ji,jj) * r1_e1u(ji,jj) * ( ptab(ji+1,jj,jk) - ptab(ji,jj,jk) )
                     zflv(ji,jj) = pahv3D(ji,jj,jl) * e1v(ji,jj) * r1_e2v(ji,jj) * ( ptab(ji,jj+1,jk) - ptab(ji,jj,jk) )
                  END DO
               END DO
               !
               DO jj= 2, jpjm1                                 ! diffusive trend : divergence of the fluxes
                  DO ji = fs_2 , fs_jpim1   ! vector opt. 
                     zdiv(ji,jj) = ( zflu(ji,jj) - zflu(ji-1,jj) + zflv(ji,jj) - zflv(ji,jj-1) ) * r1_e12t(ji,jj)
                  END DO
               END DO
               !
               IF( iter == 1 )   zdiv0(:,:,jk) = zdiv(:,:)        ! save the 1st evaluation of the diffusive trend in zdiv0
               !
               DO jj = 2, jpjm1                                ! iterative evaluation
                  DO ji = fs_2 , fs_jpim1   ! vector opt.
                     zrlxint = (   ztab0(ji,jj,jk)    &
                        &       +  rdt_ice * (           zalfa   * ( zdiv(ji,jj) + efact(ji,jj) * ptab(ji,jj,jk) )   &
                        &                      + ( 1.0 - zalfa ) *   zdiv0(ji,jj,jk) )                               &
                        &      ) / ( 1.0 + zalfa * rdt_ice * efact(ji,jj) )
                     zrlx(ji,jj,jk) = ptab(ji,jj,jk) + zrelax * ( zrlxint - ptab(ji,jj,jk) )
                  END DO
               END DO
            END IF

         END DO

         CALL lbc_lnk_multi( zrlx_array, type_array , psgn_array , isize ) ! Multiple interchange of all the variables
         !
         
         IF ( MOD( iter-1 , nn_convfrq ) == 0 )  THEN   !Convergence test every nn_convfrq iterations (perf. optimization ) 
            DO jk=1,isize
               zconv(jk) = 0._wp                                   ! convergence test
               DO jj = 2, jpjm1
                  DO ji = fs_2, fs_jpim1
                     zconv(jk) = MAX( zconv(jk), ABS( zrlx(ji,jj,jk) - ptab(ji,jj,jk) )  )
                  END DO
               END DO
            END DO
            IF( lk_mpp ) CALL mpp_max_multiple( zconv , isize )            ! max over the global domain for all the variables
         ENDIF
         !
         DO jk=1,isize
            ptab(:,:,jk) = zrlx(:,:,jk)
         END DO
         !
      END DO                                       ! end of sub-time step loop

     ! -----------------------
      !!! final step (clem) !!!
      DO jk = 1, isize
         jl = (jk-1) /( ihdf_vars+nlay_i)+1
         DO jj = 1, jpjm1                                ! diffusive fluxes in U- and V- direction
            DO ji = 1 , fs_jpim1   ! vector opt.
               zflu(ji,jj) = pahu3D(ji,jj,jl) * e2u(ji,jj) * r1_e1u(ji,jj) * ( ptab(ji+1,jj,jk) - ptab(ji,jj,jk) )
               zflv(ji,jj) = pahv3D(ji,jj,jl) * e1v(ji,jj) * r1_e2v(ji,jj) * ( ptab(ji,jj+1,jk) - ptab(ji,jj,jk) )
            END DO
         END DO
         !
         DO jj= 2, jpjm1                                 ! diffusive trend : divergence of the fluxes
            DO ji = fs_2 , fs_jpim1   ! vector opt. 
               zdiv(ji,jj) = ( zflu(ji,jj) - zflu(ji-1,jj) + zflv(ji,jj) - zflv(ji,jj-1) ) * r1_e12t(ji,jj)
               ptab(ji,jj,jk) = ztab0(ji,jj,jk) + 0.5 * ( zdiv(ji,jj) + zdiv0(ji,jj,jk) )
            END DO
         END DO
      END DO

      CALL lbc_lnk_multi( pt2d_array, type_array , psgn_array , isize ) ! Multiple interchange of all the variables

      !!! final step (clem) !!!
      ! -----------------------

 !     IF(ln_ctl)   THEN
 !        DO jk = 1 , isize
 !           zrlx(:,:,jk) = ptab(:,:,jk) - ztab0(:,:,jk)
 !           WRITE(charout,FMT="('lim_hdf  : zconv =',D23.16, ' iter =',I4)") zconv, iter
 !           CALL prt_ctl( tab2d_1=zrlx(:,:,jk), clinfo1=charout )
 !        END DO
  !    ENDIF
      !
      CALL wrk_dealloc( jpi, jpj, isize, zrlx, zdiv0, ztab0 )
      CALL wrk_dealloc( jpi, jpj, zflu, zflv, zdiv )

      DEALLOCATE( zconv )
      DEALLOCATE( pt2d_array , zrlx_array )
      DEALLOCATE( type_array )
      DEALLOCATE( psgn_array )
      !
   END SUBROUTINE lim_hdf


   
   SUBROUTINE lim_hdf_init
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE lim_hdf_init  ***
      !!
      !! ** Purpose : Initialisation of horizontal diffusion of sea-ice 
      !!
      !! ** Method  : Read the namicehdf namelist
      !!
      !! ** input   : Namelist namicehdf
      !!-------------------------------------------------------------------
      INTEGER  ::   ios                 ! Local integer output status for namelist read
      NAMELIST/namicehdf/  nn_ahi0, rn_ahi0_ref, nn_convfrq 
      INTEGER  ::   ji, jj
      REAL(wp) ::   za00, zd_max
      !!-------------------------------------------------------------------
      !
      REWIND( numnam_ice_ref )              ! Namelist namicehdf in reference namelist : Ice horizontal diffusion
      READ  ( numnam_ice_ref, namicehdf, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namicehdf in reference namelist', lwp )

      REWIND( numnam_ice_cfg )              ! Namelist namicehdf in configuration namelist : Ice horizontal diffusion
      READ  ( numnam_ice_cfg, namicehdf, IOSTAT = ios, ERR = 902 )
902   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namicehdf in configuration namelist', lwp )
      IF(lwm) WRITE ( numoni, namicehdf )
      !
      IF(lwp) THEN                          ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'lim_hdf_init : Ice horizontal diffusion'
         WRITE(numout,*) '~~~~~~~~~~~'
         WRITE(numout,*) '   horizontal diffusivity calculation                          nn_ahi0      = ', nn_ahi0
         WRITE(numout,*) '   horizontal diffusivity coeff. (orca2 grid)                  rn_ahi0_ref  = ', rn_ahi0_ref
         WRITE(numout,*) '   convergence check frequency of the Crant-Nicholson scheme   nn_convfrq   = ', nn_convfrq
      ENDIF
      !
      !  Diffusion coefficients
      SELECT CASE( nn_ahi0 )

      CASE( -1 )
         ahiu(:,:) = 0._wp
         ahiv(:,:) = 0._wp

         IF(lwp) WRITE(numout,*) ''
         IF(lwp) WRITE(numout,*) '   No sea-ice diffusion applied'

      CASE( 0 )
         ahiu(:,:) = rn_ahi0_ref
         ahiv(:,:) = rn_ahi0_ref

         IF(lwp) WRITE(numout,*) ''
         IF(lwp) WRITE(numout,*) '   laplacian operator: ahim constant = rn_ahi0_ref'

      CASE( 1 ) 

         zd_max = MAX( MAXVAL( e1t(:,:) ), MAXVAL( e2t(:,:) ) )
         IF( lk_mpp )   CALL mpp_max( zd_max )          ! max over the global domain
         
         ahiu(:,:) = rn_ahi0_ref * zd_max * 1.e-05_wp   ! 1.e05 = 100km = max grid space at 60deg latitude in orca2
                                                        !                    (60deg = min latitude for ice cover)  
         ahiv(:,:) = rn_ahi0_ref * zd_max * 1.e-05_wp

         IF(lwp) WRITE(numout,*) ''
         IF(lwp) WRITE(numout,*) '   laplacian operator: ahim proportional to max of e1 e2 over the domain (', zd_max, ')'
         IF(lwp) WRITE(numout,*) '   value for ahim = ', rn_ahi0_ref * zd_max * 1.e-05_wp 
         
      CASE( 2 ) 

         zd_max = MAX( MAXVAL( e1t(:,:) ), MAXVAL( e2t(:,:) ) )
         IF( lk_mpp )   CALL mpp_max( zd_max )   ! max over the global domain
         
         za00 = rn_ahi0_ref * 1.e-05_wp          ! 1.e05 = 100km = max grid space at 60deg latitude in orca2
                                                 !                    (60deg = min latitude for ice cover)  
         DO jj = 1, jpj
            DO ji = 1, jpi
               ahiu(ji,jj) = za00 * MAX( e1t(ji,jj), e2t(ji,jj) ) * umask(ji,jj,1)
               ahiv(ji,jj) = za00 * MAX( e1f(ji,jj), e2f(ji,jj) ) * vmask(ji,jj,1)
            END DO
         END DO
         !
         IF(lwp) WRITE(numout,*) ''
         IF(lwp) WRITE(numout,*) '   laplacian operator: ahim proportional to e1'
         IF(lwp) WRITE(numout,*) '   maximum grid-spacing = ', zd_max, ' maximum value for ahim = ', za00*zd_max
         
      END SELECT
      !
   END SUBROUTINE lim_hdf_init
#else
   !!----------------------------------------------------------------------
   !!   Default option          Dummy module           NO LIM sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE limhdf

