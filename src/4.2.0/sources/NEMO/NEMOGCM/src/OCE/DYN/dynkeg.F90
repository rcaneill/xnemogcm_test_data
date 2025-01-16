MODULE dynkeg
   !!======================================================================
   !!                       ***  MODULE  dynkeg  ***
   !! Ocean dynamics:  kinetic energy gradient trend
   !!======================================================================
   !! History :  1.0  !  1987-09  (P. Andrich, M.-A. Foujols)  Original code
   !!            7.0  !  1997-05  (G. Madec)  Split dynber into dynkeg and dynhpg
   !!  NEMO      1.0  !  2002-07  (G. Madec)  F90: Free form and module
   !!            3.6  !  2015-05  (N. Ducousso, G. Madec)  add Hollingsworth scheme as an option 
   !!----------------------------------------------------------------------
   
   !!----------------------------------------------------------------------
   !!   dyn_keg      : update the momentum trend with the horizontal tke
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE trd_oce         ! trends: ocean variables
   USE trddyn          ! trend manager: dynamics
   !
   USE in_out_manager  ! I/O manager
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp         ! MPP library
   USE prtctl          ! Print control
   USE timing          ! Timing
   USE bdy_oce         ! ocean open boundary conditions

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dyn_keg    ! routine called by step module
   
   INTEGER, PARAMETER, PUBLIC  ::   nkeg_C2  = 0   !: 2nd order centered scheme (standard scheme)
   INTEGER, PARAMETER, PUBLIC  ::   nkeg_HW  = 1   !: Hollingsworth et al., QJRMS, 1983
   !
   REAL(wp) ::   r1_48 = 1._wp / 48._wp   !: =1/(4*2*6)
   
   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: dynkeg.F90 14834 2021-05-11 09:24:44Z hadcv $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dyn_keg( kt, kscheme, Kmm, puu, pvv, Krhs )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_keg  ***
      !!
      !! ** Purpose :   Compute the now momentum trend due to the horizontal
      !!      gradient of the horizontal kinetic energy and add it to the 
      !!      general momentum trend.
      !!
      !! ** Method  : * kscheme = nkeg_C2 : 2nd order centered scheme that 
      !!      conserve kinetic energy. Compute the now horizontal kinetic energy 
      !!         zhke = 1/2 [ mi-1( un^2 ) + mj-1( vn^2 ) ]
      !!              * kscheme = nkeg_HW : Hollingsworth correction following
      !!      Arakawa (2001). The now horizontal kinetic energy is given by:
      !!         zhke = 1/6 [ mi-1(  2 * un^2 + ((u(j+1)+u(j-1))/2)^2  )
      !!                    + mj-1(  2 * vn^2 + ((v(i+1)+v(i-1))/2)^2  ) ]
      !!      
      !!      Take its horizontal gradient and add it to the general momentum
      !!      trend.
      !!         u(rhs) = u(rhs) - 1/e1u di[ zhke ]
      !!         v(rhs) = v(rhs) - 1/e2v dj[ zhke ]
      !!
      !! ** Action : - Update the (puu(:,:,:,Krhs), pvv(:,:,:,Krhs)) with the hor. ke gradient trend
      !!             - send this trends to trd_dyn (l_trddyn=T) for post-processing
      !!
      !! ** References : Arakawa, A., International Geophysics 2001.
      !!                 Hollingsworth et al., Quart. J. Roy. Meteor. Soc., 1983.
      !!----------------------------------------------------------------------
      INTEGER                             , INTENT( in )  ::  kt               ! ocean time-step index
      INTEGER                             , INTENT( in )  ::  kscheme          ! =0/1   type of KEG scheme 
      INTEGER                             , INTENT( in )  ::  Kmm, Krhs        ! ocean time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpt), INTENT(inout) ::  puu, pvv         ! ocean velocities and RHS of momentum equation
      !
      INTEGER  ::   ji, jj, jk             ! dummy loop indices
      REAL(wp) ::   zu, zv                   ! local scalars
      REAL(wp), DIMENSION(A2D(nn_hls),jpk)    ::   zhke
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   ztrdu, ztrdv 
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('dyn_keg')
      !
      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                       ! Do only on the first tile
         IF( kt == nit000 ) THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'dyn_keg : kinetic energy gradient trend, scheme number=', kscheme
            IF(lwp) WRITE(numout,*) '~~~~~~~'
         ENDIF
      ENDIF

      IF( l_trddyn ) THEN           ! Save the input trends
         ALLOCATE( ztrdu(jpi,jpj,jpk) , ztrdv(jpi,jpj,jpk) )
         ztrdu(:,:,:) = puu(:,:,:,Krhs) 
         ztrdv(:,:,:) = pvv(:,:,:,Krhs) 
      ENDIF
      
      zhke(:,:,jpk) = 0._wp

      SELECT CASE ( kscheme )             !== Horizontal kinetic energy at T-point  ==!
      !
      CASE ( nkeg_C2 )                          !--  Standard scheme  --!
         DO_3D( 0, 1, 0, 1, 1, jpkm1 )
            zu =    puu(ji-1,jj  ,jk,Kmm) * puu(ji-1,jj  ,jk,Kmm)   &
               &  + puu(ji  ,jj  ,jk,Kmm) * puu(ji  ,jj  ,jk,Kmm)
            zv =    pvv(ji  ,jj-1,jk,Kmm) * pvv(ji  ,jj-1,jk,Kmm)   &
               &  + pvv(ji  ,jj  ,jk,Kmm) * pvv(ji  ,jj  ,jk,Kmm)
            zhke(ji,jj,jk) = 0.25_wp * ( zv + zu )
         END_3D
      CASE ( nkeg_HW )                          !--  Hollingsworth scheme  --!
         DO_3D( 0, nn_hls-1, 0, nn_hls-1, 1, jpkm1 )
            ! round brackets added to fix the order of floating point operations
            ! needed to ensure halo 1 - halo 2 compatibility
            zu = 8._wp * ( puu(ji-1,jj  ,jk,Kmm) * puu(ji-1,jj  ,jk,Kmm)    &
               &         + puu(ji  ,jj  ,jk,Kmm) * puu(ji  ,jj  ,jk,Kmm) )  &
               &   +     ( ( puu(ji-1,jj-1,jk,Kmm) + puu(ji-1,jj+1,jk,Kmm) ) * ( puu(ji-1,jj-1,jk,Kmm) + puu(ji-1,jj+1,jk,Kmm) )   &
               &   +       ( puu(ji  ,jj-1,jk,Kmm) + puu(ji  ,jj+1,jk,Kmm) ) * ( puu(ji  ,jj-1,jk,Kmm) + puu(ji  ,jj+1,jk,Kmm) )   &
               &         )                                                               ! bracket for halo 1 - halo 2 compatibility
               !
            zv = 8._wp * ( pvv(ji  ,jj-1,jk,Kmm) * pvv(ji  ,jj-1,jk,Kmm)    &
               &         + pvv(ji  ,jj  ,jk,Kmm) * pvv(ji  ,jj  ,jk,Kmm) )  &
               &  +      ( ( pvv(ji-1,jj-1,jk,Kmm) + pvv(ji+1,jj-1,jk,Kmm) ) * ( pvv(ji-1,jj-1,jk,Kmm) + pvv(ji+1,jj-1,jk,Kmm) )  &
               &  +        ( pvv(ji-1,jj  ,jk,Kmm) + pvv(ji+1,jj  ,jk,Kmm) ) * ( pvv(ji-1,jj  ,jk,Kmm) + pvv(ji+1,jj  ,jk,Kmm) )  &
               &         )                                                               ! bracket for halo 1 - halo 2 compatibility
            zhke(ji,jj,jk) = r1_48 * ( zv + zu )
         END_3D
         IF (nn_hls==1) CALL lbc_lnk( 'dynkeg', zhke, 'T', 1.0_wp )
         !
      END SELECT 
      !
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )       !==  grad( KE ) added to the general momentum trends  ==!
         puu(ji,jj,jk,Krhs) = puu(ji,jj,jk,Krhs) - ( zhke(ji+1,jj  ,jk) - zhke(ji,jj,jk) ) / e1u(ji,jj)
         pvv(ji,jj,jk,Krhs) = pvv(ji,jj,jk,Krhs) - ( zhke(ji  ,jj+1,jk) - zhke(ji,jj,jk) ) / e2v(ji,jj)
      END_3D
      !
      IF( l_trddyn ) THEN                 ! save the Kinetic Energy trends for diagnostic
         ztrdu(:,:,:) = puu(:,:,:,Krhs) - ztrdu(:,:,:)
         ztrdv(:,:,:) = pvv(:,:,:,Krhs) - ztrdv(:,:,:)
         CALL trd_dyn( ztrdu, ztrdv, jpdyn_keg, kt, Kmm )
         DEALLOCATE( ztrdu , ztrdv )
      ENDIF
      !
      IF(sn_cfctl%l_prtctl)   CALL prt_ctl( tab3d_1=puu(:,:,:,Krhs), clinfo1=' keg  - Ua: ', mask1=umask,   &
         &                                  tab3d_2=pvv(:,:,:,Krhs), clinfo2=       ' Va: ', mask2=vmask, clinfo3='dyn' )
      !
      IF( ln_timing )   CALL timing_stop('dyn_keg')
      !
   END SUBROUTINE dyn_keg

   !!======================================================================
END MODULE dynkeg
