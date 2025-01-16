MODULE trasbc
   !!==============================================================================
   !!                       ***  MODULE  trasbc  ***
   !! Ocean active tracers:  surface boundary condition
   !!==============================================================================
   !! History :  OPA  !  1998-10  (G. Madec, G. Roullet, M. Imbard)  Original code
   !!            8.2  !  2001-02  (D. Ludicone)  sea ice and free surface
   !!  NEMO      1.0  !  2002-06  (G. Madec)  F90: Free form and module
   !!            3.3  !  2010-04  (M. Leclair, G. Madec)  Forcing averaged over 2 time steps
   !!             -   !  2010-09  (C. Ethe, G. Madec) Merge TRA-TRC
   !!            3.6  !  2014-11  (P. Mathiot) isf melting forcing
   !!            4.1  !  2019-09  (P. Mathiot) isf moved in traisf
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_sbc       : update the tracer trend at ocean surface
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and active tracers
   USE sbc_oce        ! surface boundary condition: ocean
   USE dom_oce        ! ocean space domain variables
   USE phycst         ! physical constant
   USE eosbn2         ! Equation Of State
   USE sbcmod         ! ln_rnf
   USE sbcrnf         ! River runoff
   USE traqsr         ! solar radiation penetration
   USE trd_oce        ! trends: ocean variables
   USE trdtra         ! trends manager: tracers
#if defined key_asminc
   USE asminc         ! Assimilation increment
#endif
   !
   USE in_out_manager ! I/O manager
   USE prtctl         ! Print control
   USE iom            ! xIOS server
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_sbc   ! routine called by step.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: trasbc.F90 14834 2021-05-11 09:24:44Z hadcv $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tra_sbc ( kt, Kmm, pts, Krhs )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_sbc  ***
      !!
      !! ** Purpose :   Compute the tracer surface boundary condition trend of
      !!      (flux through the interface, concentration/dilution effect)
      !!      and add it to the general trend of tracer equations.
      !!
      !! ** Method :   The (air+ice)-sea flux has two components:
      !!      (1) Fext, external forcing (i.e. flux through the (air+ice)-sea interface);
      !!      (2) Fwe , tracer carried with the water that is exchanged with air+ice.
      !!               The input forcing fields (emp, rnf, sfx) contain Fext+Fwe,
      !!             they are simply added to the tracer trend (ts(Krhs)).
      !!               In linear free surface case (ln_linssh=T), the volume of the
      !!             ocean does not change with the water exchanges at the (air+ice)-sea
      !!             interface. Therefore another term has to be added, to mimic the
      !!             concentration/dilution effect associated with water exchanges.
      !!
      !! ** Action  : - Update ts(Krhs) with the surface boundary condition trend
      !!              - send trends to trdtra module for further diagnostics(l_trdtra=T)
      !!----------------------------------------------------------------------
      INTEGER,                                   INTENT(in   ) ::   kt         ! ocean time-step index
      INTEGER,                                   INTENT(in   ) ::   Kmm, Krhs  ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts,jpt), INTENT(inout) ::   pts        ! active tracers and RHS of tracer Eq.
      !
      INTEGER  ::   ji, jj, jk, jn               ! dummy loop indices
      INTEGER  ::   ikt, ikb                     ! local integers
      REAL(wp) ::   zfact, z1_e3t, zdep, ztim    ! local scalar
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::  ztrdt, ztrds
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('tra_sbc')
      !
      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                       ! Do only on the first tile
         IF( kt == nit000 ) THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'tra_sbc : TRAcer Surface Boundary Condition'
            IF(lwp) WRITE(numout,*) '~~~~~~~ '
         ENDIF
      ENDIF
      !
      IF( l_trdtra ) THEN                    !* Save ta and sa trends
         ALLOCATE( ztrdt(jpi,jpj,jpk), ztrds(jpi,jpj,jpk) )
         ztrdt(:,:,:) = pts(:,:,:,jp_tem,Krhs)
         ztrds(:,:,:) = pts(:,:,:,jp_sal,Krhs)
      ENDIF
      !
!!gm  This should be moved into sbcmod.F90 module ? (especially now that ln_traqsr is read in namsbc namelist)
      IF( .NOT.ln_traqsr ) THEN     ! no solar radiation penetration
         DO_2D_OVR( nn_hls, nn_hls, nn_hls, nn_hls )
            qns(ji,jj) = qns(ji,jj) + qsr(ji,jj)      ! total heat flux in qns
            qsr(ji,jj) = 0._wp                        ! qsr set to zero
         END_2D
      ENDIF

      !----------------------------------------
      !        EMP, SFX and QNS effects
      !----------------------------------------
      !                             !==  Set before sbc tracer content fields  ==!
      IF( kt == nit000 ) THEN             !* 1st time-step
         IF( ln_rstart .AND. .NOT.l_1st_euler ) THEN      ! Restart: read in restart file
            zfact = 0.5_wp
            IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                       ! Do only on the first tile
               IF(lwp) WRITE(numout,*) '          nit000-1 sbc tracer content field read in the restart file'
               sbc_tsc(:,:,:) = 0._wp
               CALL iom_get( numror, jpdom_auto, 'sbc_hc_b', sbc_tsc_b(:,:,jp_tem) )   ! before heat content sbc trend
               CALL iom_get( numror, jpdom_auto, 'sbc_sc_b', sbc_tsc_b(:,:,jp_sal) )   ! before salt content sbc trend
            ENDIF
         ELSE                                             ! No restart or restart not found: Euler forward time stepping
            zfact = 1._wp
            DO_2D_OVR( nn_hls, nn_hls, nn_hls, nn_hls )
               sbc_tsc(ji,jj,:) = 0._wp
               sbc_tsc_b(ji,jj,:) = 0._wp
            END_2D
         ENDIF
      ELSE                                !* other time-steps: swap of forcing fields
         zfact = 0.5_wp
         DO_2D_OVR( nn_hls, nn_hls, nn_hls, nn_hls )
            sbc_tsc_b(ji,jj,:) = sbc_tsc(ji,jj,:)
         END_2D
      ENDIF
      !                             !==  Now sbc tracer content fields  ==!
      DO_2D_OVR( nn_hls, nn_hls, nn_hls, nn_hls )
         sbc_tsc(ji,jj,jp_tem) = r1_rho0_rcp * qns(ji,jj)   ! non solar heat flux
         sbc_tsc(ji,jj,jp_sal) = r1_rho0     * sfx(ji,jj)   ! salt flux due to freezing/melting
      END_2D
      IF( ln_linssh ) THEN                !* linear free surface
         DO_2D_OVR( nn_hls, nn_hls, nn_hls, nn_hls )                    !==>> add concentration/dilution effect due to constant volume cell
            sbc_tsc(ji,jj,jp_tem) = sbc_tsc(ji,jj,jp_tem) + r1_rho0 * emp(ji,jj) * pts(ji,jj,1,jp_tem,Kmm)
            sbc_tsc(ji,jj,jp_sal) = sbc_tsc(ji,jj,jp_sal) + r1_rho0 * emp(ji,jj) * pts(ji,jj,1,jp_sal,Kmm)
         END_2D                                 !==>> output c./d. term
         IF( iom_use('emp_x_sst') )   CALL iom_put( "emp_x_sst", emp (:,:) * pts(:,:,1,jp_tem,Kmm) )
         IF( iom_use('emp_x_sss') )   CALL iom_put( "emp_x_sss", emp (:,:) * pts(:,:,1,jp_sal,Kmm) )
      ENDIF
      !
      DO jn = 1, jpts               !==  update tracer trend  ==!
         DO_2D( 0, 0, 0, 0 )
            pts(ji,jj,1,jn,Krhs) = pts(ji,jj,1,jn,Krhs) + zfact * ( sbc_tsc_b(ji,jj,jn) + sbc_tsc(ji,jj,jn) )    &
               &                                                / e3t(ji,jj,1,Kmm)
         END_2D
      END DO
      !
      IF( .NOT. l_istiled .OR. ntile == nijtile )  THEN                ! Do only on the last tile
         IF( lrst_oce ) THEN           !==  write sbc_tsc in the ocean restart file  ==!
            CALL iom_rstput( kt, nitrst, numrow, 'sbc_hc_b', sbc_tsc(:,:,jp_tem) )
            CALL iom_rstput( kt, nitrst, numrow, 'sbc_sc_b', sbc_tsc(:,:,jp_sal) )
         ENDIF
      ENDIF
      !
      !----------------------------------------
      !        River Runoff effects
      !----------------------------------------
      !
      IF( ln_rnf ) THEN         ! input of heat and salt due to river runoff
         zfact = 0.5_wp
         DO_2D( 0, 0, 0, 0 )
            IF( rnf(ji,jj) /= 0._wp ) THEN
               zdep = zfact / h_rnf(ji,jj)
               DO jk = 1, nk_rnf(ji,jj)
                                     pts(ji,jj,jk,jp_tem,Krhs) = pts(ji,jj,jk,jp_tem,Krhs)                                  &
                                        &                      +  ( rnf_tsc_b(ji,jj,jp_tem) + rnf_tsc(ji,jj,jp_tem) ) * zdep
                  IF( ln_rnf_sal )   pts(ji,jj,jk,jp_sal,Krhs) = pts(ji,jj,jk,jp_sal,Krhs)                                  &
                                        &                      +  ( rnf_tsc_b(ji,jj,jp_sal) + rnf_tsc(ji,jj,jp_sal) ) * zdep
               END DO
            ENDIF
         END_2D
      ENDIF

      IF( iom_use('rnf_x_sst') )   CALL iom_put( "rnf_x_sst", rnf*pts(:,:,1,jp_tem,Kmm) )   ! runoff term on sst
      IF( iom_use('rnf_x_sss') )   CALL iom_put( "rnf_x_sss", rnf*pts(:,:,1,jp_sal,Kmm) )   ! runoff term on sss

#if defined key_asminc
      !
      !----------------------------------------
      !        Assmilation effects
      !----------------------------------------
      !
      IF( ln_sshinc ) THEN         ! input of heat and salt due to assimilation
      	 !
         IF( ln_linssh ) THEN
            DO_2D( 0, 0, 0, 0 )
               ztim = ssh_iau(ji,jj) / e3t(ji,jj,1,Kmm)
               pts(ji,jj,1,jp_tem,Krhs) = pts(ji,jj,1,jp_tem,Krhs) + pts(ji,jj,1,jp_tem,Kmm) * ztim
               pts(ji,jj,1,jp_sal,Krhs) = pts(ji,jj,1,jp_sal,Krhs) + pts(ji,jj,1,jp_sal,Kmm) * ztim
            END_2D
         ELSE
            DO_2D( 0, 0, 0, 0 )
               ztim = ssh_iau(ji,jj) / ( ht(ji,jj) + 1. - ssmask(ji, jj) )
               pts(ji,jj,:,jp_tem,Krhs) = pts(ji,jj,:,jp_tem,Krhs) + pts(ji,jj,:,jp_tem,Kmm) * ztim
               pts(ji,jj,:,jp_sal,Krhs) = pts(ji,jj,:,jp_sal,Krhs) + pts(ji,jj,:,jp_sal,Kmm) * ztim
            END_2D
         ENDIF
         !
      ENDIF
      !
#endif
      !
      IF( l_trdtra )   THEN                      ! save the horizontal diffusive trends for further diagnostics
         ztrdt(:,:,:) = pts(:,:,:,jp_tem,Krhs) - ztrdt(:,:,:)
         ztrds(:,:,:) = pts(:,:,:,jp_sal,Krhs) - ztrds(:,:,:)
         CALL trd_tra( kt, Kmm, Krhs, 'TRA', jp_tem, jptra_nsr, ztrdt )
         CALL trd_tra( kt, Kmm, Krhs, 'TRA', jp_sal, jptra_nsr, ztrds )
         DEALLOCATE( ztrdt , ztrds )
      ENDIF
      !
      IF(sn_cfctl%l_prtctl)   CALL prt_ctl( tab3d_1=pts(:,:,:,jp_tem,Krhs), clinfo1=' sbc  - Ta: ', mask1=tmask,   &
         &                                  tab3d_2=pts(:,:,:,jp_sal,Krhs), clinfo2=       ' Sa: ', mask2=tmask, clinfo3='tra' )
      !
      IF( ln_timing )   CALL timing_stop('tra_sbc')
      !
   END SUBROUTINE tra_sbc

   !!======================================================================
END MODULE trasbc
