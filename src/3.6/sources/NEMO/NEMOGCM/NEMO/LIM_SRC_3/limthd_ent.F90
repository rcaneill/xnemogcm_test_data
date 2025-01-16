MODULE limthd_ent
   !!======================================================================
   !!                       ***  MODULE limthd_ent   ***
   !!                  Redistribution of Enthalpy in the ice
   !!                        on the new vertical grid
   !!                       after vertical growth/decay
   !!======================================================================
   !! History :  LIM  ! 2003-05 (M. Vancoppenolle) Original code in 1D
   !!                 ! 2005-07 (M. Vancoppenolle) 3D version 
   !!                 ! 2006-11 (X. Fettweis) Vectorized 
   !!            3.0  ! 2008-03 (M. Vancoppenolle) Energy conservation and clean code
   !!            3.4  ! 2011-02 (G. Madec) dynamical allocation
   !!             -   ! 2014-05 (C. Rousset) complete rewriting
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3'                                      LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_thd_ent   : ice redistribution of enthalpy
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean parameters
   USE dom_oce        ! domain variables
   USE domain         !
   USE phycst         ! physical constants
   USE sbc_oce        ! Surface boundary condition: ocean fields
   USE ice            ! LIM variables
   USE thd_ice        ! LIM thermodynamics
   USE limvar         ! LIM variables
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE wrk_nemo       ! work arrays
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)  

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_thd_ent         ! called by limthd and limthd_lac

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: limthd_ent.F90 9875 2018-07-04 17:40:26Z clem $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS
 
   SUBROUTINE lim_thd_ent( kideb, kiut, qnew )
      !!-------------------------------------------------------------------
      !!               ***   ROUTINE lim_thd_ent  ***
      !!
      !! ** Purpose :
      !!           This routine computes new vertical grids in the ice, 
      !!           and consistently redistributes temperatures. 
      !!           Redistribution is made so as to ensure to energy conservation
      !!
      !!
      !! ** Method  : linear conservative remapping
      !!           
      !! ** Steps : 1) cumulative integrals of old enthalpies/thicknesses
      !!            2) linear remapping on the new layers
      !!
      !! ------------ cum0(0)                        ------------- cum1(0)
      !!                                    NEW      -------------
      !! ------------ cum0(1)               ==>      -------------
      !!     ...                                     -------------
      !! ------------                                -------------
      !! ------------ cum0(nlay_i+2)                 ------------- cum1(nlay_i)
      !!
      !!
      !! References : Bitz & Lipscomb, JGR 99; Vancoppenolle et al., GRL, 2005
      !!-------------------------------------------------------------------
      INTEGER , INTENT(in) ::   kideb, kiut   ! Start/End point on which the  the computation is applied

      REAL(wp), INTENT(inout), DIMENSION(:,:) :: qnew          ! new enthlapies (J.m-3, remapped)

      INTEGER  :: ji         !  dummy loop indices
      INTEGER  :: jk0, jk1   !  old/new layer indices
      !
      REAL(wp), POINTER, DIMENSION(:,:) :: zqh_cum0, zh_cum0   ! old cumulative enthlapies and layers interfaces
      REAL(wp), POINTER, DIMENSION(:,:) :: zqh_cum1, zh_cum1   ! new cumulative enthlapies and layers interfaces
      REAL(wp), POINTER, DIMENSION(:)   :: zhnew               ! new layers thicknesses
      !!-------------------------------------------------------------------

      CALL wrk_alloc( jpij, nlay_i+3, zqh_cum0, zh_cum0, kjstart = 0 )
      CALL wrk_alloc( jpij, nlay_i+1, zqh_cum1, zh_cum1, kjstart = 0 )
      CALL wrk_alloc( jpij, zhnew )

      !--------------------------------------------------------------------------
      !  1) Cumulative integral of old enthalpy * thickness and layers interfaces
      !--------------------------------------------------------------------------
      zqh_cum0(:,0:nlay_i+2) = 0._wp 
      zh_cum0 (:,0:nlay_i+2) = 0._wp
      DO jk0 = 1, nlay_i+2
         DO ji = kideb, kiut
            zqh_cum0(ji,jk0) = zqh_cum0(ji,jk0-1) + qh_i_old(ji,jk0-1)
            zh_cum0 (ji,jk0) = zh_cum0 (ji,jk0-1) + h_i_old (ji,jk0-1)
         ENDDO
      ENDDO

      !------------------------------------
      !  2) Interpolation on the new layers
      !------------------------------------
      ! new layer thickesses
      DO ji = kideb, kiut
         zhnew(ji) = SUM( h_i_old(ji,0:nlay_i+1) ) * r1_nlay_i  
      ENDDO

      ! new layers interfaces
      zh_cum1(:,0:nlay_i) = 0._wp
      DO jk1 = 1, nlay_i
         DO ji = kideb, kiut
            zh_cum1(ji,jk1) = zh_cum1(ji,jk1-1) + zhnew(ji)
         ENDDO
      ENDDO

      zqh_cum1(:,0:nlay_i) = 0._wp 
      ! new cumulative q*h => linear interpolation
      DO jk0 = 1, nlay_i+2
         DO jk1 = 1, nlay_i-1
            DO ji = kideb, kiut
               IF( zh_cum1(ji,jk1) <= zh_cum0(ji,jk0) .AND. zh_cum1(ji,jk1) > zh_cum0(ji,jk0-1) ) THEN
                  zqh_cum1(ji,jk1) = ( zqh_cum0(ji,jk0-1) * ( zh_cum0(ji,jk0) - zh_cum1(ji,jk1  ) ) +  &
                     &                 zqh_cum0(ji,jk0  ) * ( zh_cum1(ji,jk1) - zh_cum0(ji,jk0-1) ) )  &
                     &             / ( zh_cum0(ji,jk0) - zh_cum0(ji,jk0-1) )
               ENDIF
            ENDDO
         ENDDO
      ENDDO
      ! to ensure that total heat content is strictly conserved, set:
      zqh_cum1(:,nlay_i) = zqh_cum0(:,nlay_i+2) 

      ! new enthalpies
      DO jk1 = 1, nlay_i
         DO ji = kideb, kiut
            rswitch      = MAX( 0._wp , SIGN( 1._wp , zhnew(ji) - epsi20 ) ) 
            qnew(ji,jk1) = rswitch * ( zqh_cum1(ji,jk1) - zqh_cum1(ji,jk1-1) ) / MAX( zhnew(ji), epsi20 )
         ENDDO
      ENDDO

      ! --- diag error on heat remapping --- !
      ! comment: if input h_i_old and qh_i_old are already multiplied by a_i (as in limthd_lac), 
      ! then we should not (* a_i) again but not important since this is just to check that remap error is ~0
      DO ji = kideb, kiut
         hfx_err_rem_1d(ji) = hfx_err_rem_1d(ji) + a_i_1d(ji) * r1_rdtice *  &
            &               ( SUM( qnew(ji,1:nlay_i) ) * zhnew(ji) - SUM( qh_i_old(ji,0:nlay_i+1) ) ) 
      END DO
      
      !
      CALL wrk_dealloc( jpij, nlay_i+3, zqh_cum0, zh_cum0, kjstart = 0 )
      CALL wrk_dealloc( jpij, nlay_i+1, zqh_cum1, zh_cum1, kjstart = 0 )
      CALL wrk_dealloc( jpij, zhnew )
      !
   END SUBROUTINE lim_thd_ent

#else
   !!----------------------------------------------------------------------
   !!   Default option                               NO  LIM3 sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_thd_ent          ! Empty routine
   END SUBROUTINE lim_thd_ent
#endif

   !!======================================================================
END MODULE limthd_ent
