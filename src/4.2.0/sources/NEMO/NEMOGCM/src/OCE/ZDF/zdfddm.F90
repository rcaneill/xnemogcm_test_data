MODULE zdfddm
   !!======================================================================
   !!                       ***  MODULE  zdfddm  ***
   !! Ocean physics : double diffusion mixing parameterization
   !!======================================================================
   !! History :  OPA  ! 2000-08  (G. Madec)  double diffusive mixing
   !!   NEMO     1.0  ! 2002-06  (G. Madec)  F90: Free form and module
   !!            3.3  ! 2010-10  (C. Ethe, G. Madec) reorganisation of initialisation phase
   !!            3.6  ! 2013-04  (G. Madec, F. Roquet) zrau compute locally using interpolation of alpha & beta
   !!            4.0  !  2017-04  (G. Madec)  remove CPP ddm key & avm at t-point only 
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   zdf_ddm       : compute the Kz for salinity
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers variables
   USE dom_oce        ! ocean space and time domain variables
   USE zdf_oce        ! ocean vertical physics variables
   USE eosbn2         ! equation of state
   !
   USE in_out_manager ! I/O manager
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE prtctl         ! Print control
   USE lib_mpp        ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   zdf_ddm       ! called by step.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: zdfddm.F90 14853 2021-05-12 13:07:30Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE zdf_ddm( kt, Kmm, p_avm, p_avt, p_avs )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zdf_ddm  ***
      !!                    
      !! ** Purpose :   Add to the vertical eddy diffusivity coefficient the 
      !!              effect of salt fingering and diffusive convection. 
      !!
      !! ** Method  :   Diapycnal mixing is increased in case of double
      !!      diffusive mixing (i.e. salt fingering and diffusive layering)
      !!      following Merryfield et al. (1999). The rate of double diffusive 
      !!      mixing depend on the buoyancy ratio (R=alpha/beta dk[T]/dk[S]):
      !!         * salt fingering (Schmitt 1981):
      !!      for R > 1 and rn2 > 0 : zavfs = rn_avts / ( 1 + (R/rn_hsbfr)^6 )
      !!      for R > 1 and rn2 > 0 : zavfs = O
      !!      otherwise                : zavft = 0.7 zavs / R
      !!         * diffusive layering (Federov 1988):
      !!      for 0< R < 1 and N^2 > 0 : zavdt = 1.3635e-6 * exp( 4.6 exp(-0.54 (1/R-1) ) )
      !!      otherwise                   : zavdt = 0 
      !!      for .5 < R < 1 and N^2 > 0 : zavds = zavdt (1.885 R -0.85)
      !!      for  0 < R <.5 and N^2 > 0 : zavds = zavdt 0.15 R      
      !!      otherwise                     : zavds = 0 
      !!         * update the eddy diffusivity:
      !!      avt = avt + zavft + zavdt
      !!      avs = avs + zavfs + zavds
      !!      avm is required to remain at least above avt and avs.
      !!      
      !! ** Action  :   avt, avs : updated vertical eddy diffusivity coef. for T & S
      !!
      !! References :   Merryfield et al., JPO, 29, 1124-1142, 1999.
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in   ) ::   kt       ! ocean time-step index
      INTEGER, INTENT(in   ) ::   Kmm      ! ocean time level index
      REAL(wp), DIMENSION(:,:,:), INTENT(inout) ::   p_avm   !  Kz on momentum    (w-points)
      REAL(wp), DIMENSION(:,:,:), INTENT(inout) ::   p_avt   !  Kz on temperature (w-points)
      REAL(wp), DIMENSION(:,:,:), INTENT(  out) ::   p_avs   !  Kz on salinity    (w-points)
      !
      INTEGER  ::   ji, jj , jk     ! dummy loop indices
      REAL(wp) ::   zaw, zbw, zrw   ! local scalars
      REAL(wp) ::   zdt, zds
      REAL(wp) ::   zinr            !   -      -
      REAL(dp) ::         zrr       !   -      -
      REAL(wp) ::   zavft           !   -      -
      REAL(dp) ::          zavfs    !   -      -
      REAL(wp) ::   zavdt, zavds    !   -      -
      REAL(wp), DIMENSION(A2D(nn_hls)) ::   zrau, zmsks, zmskf, zmskd1, zmskd2, zmskd3
      !!----------------------------------------------------------------------
      !
      !                                                ! ===============
      DO jk = 2, jpkm1                                 ! Horizontal slab
         !                                             ! ===============
         ! Define the mask 
         ! ---------------
!!gm  WORK to be done:   change the code from vector optimisation to scalar one.
!!gm                     ==>>>  test in the loop instead of use of mask arrays
!!gm                            and many acces in memory
         
         DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )           !==  R=zrau = (alpha / beta) (dk[t] / dk[s])  ==!
            zrw =   ( gdepw(ji,jj,jk  ,Kmm) - gdept(ji,jj,jk,Kmm) )   &
!!gm please, use e3w at Kmm below 
               &  / ( gdept(ji,jj,jk-1,Kmm) - gdept(ji,jj,jk,Kmm) ) 
            !
            zaw = (  rab_n(ji,jj,jk,jp_tem) * (1. - zrw) + rab_n(ji,jj,jk-1,jp_tem) * zrw  )  &
                &    * tmask(ji,jj,jk) * tmask(ji,jj,jk-1)
            zbw = (  rab_n(ji,jj,jk,jp_sal) * (1. - zrw) + rab_n(ji,jj,jk-1,jp_sal) * zrw  )  &
                &    * tmask(ji,jj,jk) * tmask(ji,jj,jk-1)
            !
            zdt = zaw * ( ts(ji,jj,jk-1,jp_tem,Kmm) - ts(ji,jj,jk,jp_tem,Kmm) )
            zds = zbw * ( ts(ji,jj,jk-1,jp_sal,Kmm) - ts(ji,jj,jk,jp_sal,Kmm) ) 
            IF( ABS( zds) <= 1.e-20_wp )   zds = 1.e-20_wp
            zrau(ji,jj) = MAX(  1.e-20, zdt / zds  )    ! only retains positive value of zrau
         END_2D

         DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )           !==  indicators  ==!
            ! stability indicator: msks=1 if rn2>0; 0 elsewhere
            IF( rn2(ji,jj,jk) + 1.e-12  <= 0. ) THEN   ;   zmsks(ji,jj) = 0._wp
            ELSE                                       ;   zmsks(ji,jj) = 1._wp * wmask(ji,jj,jk)   ! mask so avt and avs masked
            ENDIF
            ! salt fingering indicator: msksf=1 if R>1; 0 elsewhere            
            IF( zrau(ji,jj) <= 1.             ) THEN   ;   zmskf(ji,jj) = 0._wp
            ELSE                                       ;   zmskf(ji,jj) = 1._wp
            ENDIF
            ! diffusive layering indicators: 
            !     ! mskdl1=1 if 0< R <1; 0 elsewhere
            IF( zrau(ji,jj) >= 1.             ) THEN   ;   zmskd1(ji,jj) = 0._wp
            ELSE                                       ;   zmskd1(ji,jj) = 1._wp
            ENDIF
            !     ! mskdl2=1 if 0< R <0.5; 0 elsewhere
            IF( zrau(ji,jj) >= 0.5            ) THEN   ;   zmskd2(ji,jj) = 0._wp
            ELSE                                       ;   zmskd2(ji,jj) = 1._wp
            ENDIF
            !   mskdl3=1 if 0.5< R <1; 0 elsewhere
            IF( zrau(ji,jj) <= 0.5 .OR. zrau(ji,jj) >= 1. ) THEN   ;   zmskd3(ji,jj) = 0._wp
            ELSE                                                   ;   zmskd3(ji,jj) = 1._wp
            ENDIF
         END_2D

         ! Update avt and avs
         ! ------------------
         ! Constant eddy coefficient: reset to the background value
         DO_2D_OVR( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1 )
            zinr = 1._wp / zrau(ji,jj)
            ! salt fingering
            zrr = zrau(ji,jj) / rn_hsbfr
            zrr = zrr * zrr
            zavfs = rn_avts / ( 1 + zrr*zrr*zrr ) * zmsks(ji,jj) * zmskf(ji,jj)
            zavft = 0.7 * zavfs * zinr
            ! diffusive layering
            zavdt = 1.3635e-6 * EXP(  4.6 * EXP( -0.54*(zinr-1.) )  ) * zmsks(ji,jj) * zmskd1(ji,jj)
            zavds = zavdt * zmsks(ji,jj) * (  ( 1.85 * zrau(ji,jj) - 0.85 ) * zmskd3(ji,jj)   &
               &                             +  0.15 * zrau(ji,jj)          * zmskd2(ji,jj)  )
            ! add to the eddy viscosity coef. previously computed
            p_avs(ji,jj,jk) = p_avt(ji,jj,jk) + zavfs + zavds
            p_avt(ji,jj,jk) = p_avt(ji,jj,jk) + zavft + zavdt
            p_avm(ji,jj,jk) = p_avm(ji,jj,jk) + MAX( zavft + zavdt, zavfs + zavds )
         END_2D
         !                                                ! ===============
      END DO                                              !   End of slab
      !                                                   ! ===============
      !
      IF(sn_cfctl%l_prtctl) THEN
         CALL prt_ctl(tab3d_1=avt , clinfo1=' ddm  - t: ', tab3d_2=avs , clinfo2=' s: ')
      ENDIF
      !
   END SUBROUTINE zdf_ddm
   
   !!======================================================================
END MODULE zdfddm
