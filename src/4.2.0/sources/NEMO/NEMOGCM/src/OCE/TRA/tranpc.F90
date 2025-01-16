MODULE tranpc
   !!==============================================================================
   !!                       ***  MODULE  tranpc  ***
   !! Ocean active tracers:  non penetrative convective adjustment scheme
   !!==============================================================================
   !! History :  1.0  ! 1990-09  (G. Madec)  Original code
   !!                 ! 1996-01  (G. Madec)  statement function for e3
   !!   NEMO     1.0  ! 2002-06  (G. Madec)  free form F90
   !!            3.0  ! 2008-06  (G. Madec)  applied on ta, sa and called before tranxt in step.F90
   !!            3.3  ! 2010-05  (C. Ethe, G. Madec)  merge TRC-TRA
   !!            3.6  ! 2015-05  (L. Brodeau) new algorithm based on local Brunt-Vaisala freq.
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_npc       : apply the non penetrative convection scheme
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and active tracers
   USE dom_oce        ! ocean space and time domain
   USE phycst         ! physical constants
   USE zdf_oce        ! ocean vertical physics
   USE trd_oce        ! ocean active tracer trends
   USE trdtra         ! ocean active tracer trends
   USE eosbn2         ! equation of state (eos routine)
   !
   USE lbclnk         ! lateral boundary conditions (or mpp link)
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_npc    ! routine called by step.F90

   INTEGER  ::   nnpcc        ! number of statically instable water column

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: tranpc.F90 14834 2021-05-11 09:24:44Z hadcv $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tra_npc( kt, Kmm, Krhs, pts, Kaa )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tranpc  ***
      !!
      !! ** Purpose : Non-penetrative convective adjustment scheme. solve
      !!      the static instability of the water column on after fields
      !!      while conserving heat and salt contents.
      !!
      !! ** Method  : updated algorithm able to deal with non-linear equation of state
      !!              (i.e. static stability computed locally)
      !!
      !! ** Action  : - tsa: after tracers with the application of the npc scheme
      !!              - send the associated trends for on-line diagnostics (l_trdtra=T)
      !!
      !! References :     Madec, et al., 1991, JPO, 21, 9, 1349-1371.
      !!----------------------------------------------------------------------
      INTEGER,                                   INTENT(in   ) :: kt              ! ocean time-step index
      INTEGER,                                   INTENT(in   ) :: Kmm, Krhs, Kaa  ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts,jpt), INTENT(inout) :: pts             ! active tracers and RHS of tracer equation
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      INTEGER  ::   jiter, ikbot, ikp, ikup, ikdown, ilayer, ik_low   ! local integers
      LOGICAL  ::   l_bottom_reached, l_column_treated
      REAL(wp) ::   zta, zalfa, zsum_temp, zsum_alfa, zaw, zdz, zsum_z
      REAL(wp) ::   zsa, zbeta, zsum_sali, zsum_beta, zbw, zrw, z1_rDt
      REAL(wp), PARAMETER ::   zn2_zero = 1.e-14_wp             ! acceptance criteria for neutrality (N2==0)
      REAL(wp), DIMENSION(    jpk     )   ::   zvn2             ! vertical profile of N2 at 1 given point...
      REAL(wp), DIMENSION(    jpk,jpts)   ::   zvts, zvab       ! vertical profile of T & S , and  alpha & betaat 1 given point
      REAL(wp), DIMENSION(A2D(nn_hls),jpk     )   ::   zn2              ! N^2
      REAL(wp), DIMENSION(A2D(nn_hls),jpk,jpts)   ::   zab              ! alpha and beta
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   ztrdt, ztrds ! 3D workspace
      !
      LOGICAL, PARAMETER :: l_LB_debug = .FALSE. ! set to true if you want to follow what is
      INTEGER :: ilc1, jlc1, klc1, nncpu         ! actually happening in a water column at point "ilc1, jlc1"
      LOGICAL :: lp_monitor_point = .FALSE.      ! in CPU domain "nncpu"
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('tra_npc')
      !
      IF( MOD( kt, nn_npc ) == 0 ) THEN
         !
         IF( l_trdtra )   THEN                    !* Save initial after fields
            ALLOCATE( ztrdt(jpi,jpj,jpk) , ztrds(jpi,jpj,jpk) )
            ztrdt(:,:,:) = pts(:,:,:,jp_tem,Kaa)
            ztrds(:,:,:) = pts(:,:,:,jp_sal,Kaa)
         ENDIF
         !
         IF( l_LB_debug ) THEN
            ! Location of 1 known convection site to follow what's happening in the water column
            ilc1 = 45 ;  jlc1 = 3 ; !  ORCA2 4x4, Antarctic coast, more than 2 unstable portions in the  water column...
            nncpu = 1  ;            ! the CPU domain contains the convection spot
            klc1 =  mbkt(ilc1,jlc1) ! bottom of the ocean for debug point...
         ENDIF
         !
         CALL eos_rab( pts(:,:,:,:,Kaa), zab, Kmm )         ! after alpha and beta (given on T-points)
         CALL bn2    ( pts(:,:,:,:,Kaa), zab, zn2, Kmm )    ! after Brunt-Vaisala  (given on W-points)
         !
         IF( .NOT. l_istiled .OR. ntile == 1 ) nnpcc = 0         ! Do only on the first tile
         !
         DO_2D_OVR( 0, 0, 0, 0 )                        ! interior column only
            !
            IF( tmask(ji,jj,2) == 1 ) THEN      ! At least 2 ocean points
               !                                     ! consider one ocean column
               zvts(:,jp_tem) = pts(ji,jj,:,jp_tem,Kaa)      ! temperature
               zvts(:,jp_sal) = pts(ji,jj,:,jp_sal,Kaa)      ! salinity
               !
               zvab(:,jp_tem)  = zab(ji,jj,:,jp_tem)     ! Alpha
               zvab(:,jp_sal)  = zab(ji,jj,:,jp_sal)     ! Beta
               zvn2(:)         = zn2(ji,jj,:)            ! N^2
               !
               IF( l_LB_debug ) THEN                  !LB debug:
                  lp_monitor_point = .FALSE.
                  IF( ( ji == ilc1 ).AND.( jj == jlc1 ) ) lp_monitor_point = .TRUE.
                  ! writing only if on CPU domain where conv region is:
                  lp_monitor_point = (narea == nncpu).AND.lp_monitor_point
               ENDIF                                  !LB debug  end
               !
               ikbot = mbkt(ji,jj)   ! ikbot: ocean bottom T-level
               ikp = 1                  ! because N2 is irrelevant at the surface level (will start at ikp=2)
               ilayer = 0
               jiter  = 0
               l_column_treated = .FALSE.
               !
               DO WHILE ( .NOT. l_column_treated )
                  !
                  jiter = jiter + 1
                  !
                  IF( jiter >= 400 ) EXIT
                  !
                  l_bottom_reached = .FALSE.
                  !
                  DO WHILE ( .NOT. l_bottom_reached )
                     !
                     ikp = ikp + 1
                     !
                     !! Testing level ikp for instability
                     !! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     IF( zvn2(ikp) <  -zn2_zero ) THEN ! Instability found!
                        !
                        ilayer = ilayer + 1    ! yet another instable portion of the water column found....
                        !
                        IF( lp_monitor_point ) THEN
                           WRITE(numout,*)
                           IF( ilayer == 1 .AND. jiter == 1 ) THEN   ! first time a column is spoted with an instability
                              WRITE(numout,*)
                              WRITE(numout,*) 'Time step = ',kt,' !!!'
                           ENDIF
                           WRITE(numout,*)  ' * Iteration #',jiter,': found instable portion #',ilayer,   &
                              &                                    ' in column! Starting at ikp =', ikp
                           WRITE(numout,*)  ' *** N2 for point (i,j) = ',ji,' , ',jj
                           DO jk = 1, klc1
                              WRITE(numout,*) jk, zvn2(jk)
                           END DO
                           WRITE(numout,*)
                        ENDIF
                        !
                        IF( jiter == 1 )   nnpcc = nnpcc + 1
                        !
                        IF( lp_monitor_point )   WRITE(numout, *) 'Negative N2 at ikp =',ikp,' for layer #', ilayer
                        !
                        !! ikup is the uppermost point where mixing will start:
                        ikup = ikp - 1 ! ikup is always "at most at ikp-1", less if neutral levels overlying
                        !
                        !! If the points above ikp-1 have N2 == 0 they must also be mixed:
                        IF( ikp > 2 ) THEN
                           DO jk = ikp-1, 2, -1
                              IF( ABS(zvn2(jk)) < zn2_zero ) THEN
                                 ikup = ikup - 1  ! 1 more upper level has N2=0 and must be added for the mixing
                              ELSE
                                 EXIT
                              ENDIF
                           END DO
                        ENDIF
                        !
                        IF( ikup < 1 )   CALL ctl_stop( 'tra_npc :  PROBLEM #1')
                        !
                        zsum_temp = 0._wp
                        zsum_sali = 0._wp
                        zsum_alfa = 0._wp
                        zsum_beta = 0._wp
                        zsum_z    = 0._wp

                        DO jk = ikup, ikbot      ! Inside the instable (and overlying neutral) portion of the column
                           !
                           zdz       = e3t(ji,jj,jk,Kmm)
                           zsum_temp = zsum_temp + zvts(jk,jp_tem)*zdz
                           zsum_sali = zsum_sali + zvts(jk,jp_sal)*zdz
                           zsum_alfa = zsum_alfa + zvab(jk,jp_tem)*zdz
                           zsum_beta = zsum_beta + zvab(jk,jp_sal)*zdz
                           zsum_z    = zsum_z    + zdz
                           !
                           IF( jk == ikbot ) EXIT ! avoid array-index overshoot in case ikbot = jpk, cause we're calling jk+1 next line
                           !! EXIT when we have reached the last layer that is instable (N2<0) or neutral (N2=0):
                           IF( zvn2(jk+1) > zn2_zero ) EXIT
                        END DO

                        ikdown = jk ! for the current unstable layer, ikdown is the deepest point with a negative or neutral N2
                        IF( ikup == ikdown )   CALL ctl_stop( 'tra_npc :  PROBLEM #2')

                        ! Mixing Temperature, salinity, alpha and beta from ikup to ikdown included:
                        zta   = zsum_temp/zsum_z
                        zsa   = zsum_sali/zsum_z
                        zalfa = zsum_alfa/zsum_z
                        zbeta = zsum_beta/zsum_z

                        IF( lp_monitor_point ) THEN
                           WRITE(numout,*) 'MIXED T, S, alfa and beta between ikup =',ikup,   &
                              &            ' and ikdown =',ikdown,', in layer #',ilayer
                           WRITE(numout,*) '  => Mean temp. in that portion =', zta
                           WRITE(numout,*) '  => Mean sali. in that portion =', zsa
                           WRITE(numout,*) '  => Mean Alfa  in that portion =', zalfa
                           WRITE(numout,*) '  => Mean Beta  in that portion =', zbeta
                        ENDIF

                        !! Homogenaizing the temperature, salinity, alpha and beta in this portion of the column
                        DO jk = ikup, ikdown
                           zvts(jk,jp_tem) = zta
                           zvts(jk,jp_sal) = zsa
                           zvab(jk,jp_tem) = zalfa
                           zvab(jk,jp_sal) = zbeta
                        END DO


                        !! Updating N2 in the relvant portion of the water column
                        !! Temperature, Salinity, Alpha and Beta have been homogenized in the unstable portion
                        !! => Need to re-compute N2! will use Alpha and Beta!

                        ikup   = MAX(2,ikup)         ! ikup can never be 1 !
                        ik_low = MIN(ikdown+1,ikbot) ! we must go 1 point deeper than ikdown!

                        DO jk = ikup, ik_low              ! we must go 1 point deeper than ikdown!

                           !! Interpolating alfa and beta at W point:
                           zrw =  (gdepw(ji,jj,jk  ,Kmm) - gdept(ji,jj,jk,Kmm)) &
                              & / (gdept(ji,jj,jk-1,Kmm) - gdept(ji,jj,jk,Kmm))
                           zaw = zvab(jk,jp_tem) * (1._wp - zrw) + zvab(jk-1,jp_tem) * zrw
                           zbw = zvab(jk,jp_sal) * (1._wp - zrw) + zvab(jk-1,jp_sal) * zrw

                           !! N2 at W point, doing exactly as in eosbn2.F90:
                           zvn2(jk) = grav*( zaw * ( zvts(jk-1,jp_tem) - zvts(jk,jp_tem) )     &
                              &            - zbw * ( zvts(jk-1,jp_sal) - zvts(jk,jp_sal) )  )  &
                              &       / e3w(ji,jj,jk,Kmm) * tmask(ji,jj,jk)

                           !! OR, faster  => just considering the vertical gradient of density
                           !! as only the signa maters...
                           !zvn2(jk) = (  zaw * ( zvts(jk-1,jp_tem) - zvts(jk,jp_tem) )     &
                           !     &      - zbw * ( zvts(jk-1,jp_sal) - zvts(jk,jp_sal) )  )

                        END DO

                        ikp = MIN(ikdown+1,ikbot)


                     ENDIF  !IF( zvn2(ikp) < 0. )


                     IF( ikp == ikbot ) l_bottom_reached = .TRUE.
                     !
                  END DO ! DO WHILE ( .NOT. l_bottom_reached )

                  IF( ikp /= ikbot )   CALL ctl_stop( 'tra_npc :  PROBLEM #3')

                  ! ******* At this stage ikp == ikbot ! *******

                  IF( ilayer > 0 ) THEN      !! least an unstable layer has been found
                     !
                     IF( lp_monitor_point ) THEN
                        WRITE(numout,*)
                        WRITE(numout,*) 'After ',jiter,' iteration(s), we neutralized ',ilayer,' instable layer(s)'
                        WRITE(numout,*) '   ==> N2 at i,j=',ji,',',jj,' now looks like this:'
                        DO jk = 1, klc1
                           WRITE(numout,*) jk, zvn2(jk)
                        END DO
                        WRITE(numout,*)
                     ENDIF
                     !
                     ikp    = 1     ! starting again at the surface for the next iteration
                     ilayer = 0
                  ENDIF
                  !
                  IF( ikp >= ikbot )   l_column_treated = .TRUE.
                  !
               END DO ! DO WHILE ( .NOT. l_column_treated )

               !! Updating pts:
               pts(ji,jj,:,jp_tem,Kaa) = zvts(:,jp_tem)
               pts(ji,jj,:,jp_sal,Kaa) = zvts(:,jp_sal)

               !! LB:  Potentially some other global variable beside theta and S can be treated here
               !!      like BGC tracers.

               IF( lp_monitor_point )   WRITE(numout,*)

            ENDIF ! IF( tmask(ji,jj,3) == 1 ) THEN

         END_2D
         !
         IF( l_trdtra ) THEN         ! send the Non penetrative mixing trends for diagnostic
            z1_rDt = 1._wp / (2._wp * rn_Dt)
            ztrdt(:,:,:) = ( pts(:,:,:,jp_tem,Kaa) - ztrdt(:,:,:) ) * z1_rDt
            ztrds(:,:,:) = ( pts(:,:,:,jp_sal,Kaa) - ztrds(:,:,:) ) * z1_rDt
            CALL trd_tra( kt, Kmm, Krhs, 'TRA', jp_tem, jptra_npc, ztrdt )
            CALL trd_tra( kt, Kmm, Krhs, 'TRA', jp_sal, jptra_npc, ztrds )
            DEALLOCATE( ztrdt, ztrds )
         ENDIF
         !
         IF( .NOT. l_istiled .OR. ntile == nijtile )  THEN                ! Do only for the full domain
            IF( lwp .AND. l_LB_debug ) THEN
               WRITE(numout,*) 'Exiting tra_npc , kt = ',kt,', => numb. of statically instable water-columns: ', nnpcc
               WRITE(numout,*)
            ENDIF
         ENDIF
         !
      ENDIF   ! IF( MOD( kt, nn_npc ) == 0 ) THEN
      !
      IF( ln_timing )   CALL timing_stop('tra_npc')
      !
   END SUBROUTINE tra_npc

   !!======================================================================
END MODULE tranpc
