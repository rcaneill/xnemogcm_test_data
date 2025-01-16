MODULE domqco
   !!======================================================================
   !!                       ***  MODULE domqco   ***
   !! Ocean :
   !!======================================================================
   !! History :  2.0  !  2006-06  (B. Levier, L. Marie)  original code
   !!            3.1  !  2009-02  (G. Madec, M. Leclair, R. Benshila)  pure z* coordinate
   !!            3.3  !  2011-10  (M. Leclair) totally rewrote domvvl: vvl option includes z_star and z_tilde coordinates
   !!            3.6  !  2014-11  (P. Mathiot) add ice shelf capability
   !!            4.1  !  2019-08  (A. Coward, D. Storkey) add time level indices for prognostic variables
   !!             -   !  2020-02  (S. Techene, G. Madec) quasi-eulerian coordinate (z* or s*)
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dom_qco_init  : define initial vertical scale factors, depths and column thickness
   !!   dom_qco_zgr   : Set ssh/h_0 ratio at t
   !!   dom_qco_r3c   : Compute ssh/h_0 ratio at t-, u-, v-, and optionally f-points
   !!       qco_ctl   : Check the vvl options
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
   USE phycst         ! physical constant
   USE dom_oce        ! ocean space and time domain
   USE dynadv  , ONLY : ln_dynadv_vec
   USE isf_oce        ! iceshelf cavities
   USE sbc_oce        ! ocean surface boundary condition
   USE wet_dry        ! wetting and drying
   USE usrdef_istate  ! user defined initial state (wad only)
   USE restart        ! ocean restart
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O manager library
   USE lib_mpp        ! distributed memory computing library
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC  dom_qco_init       ! called by domain.F90
   PUBLIC  dom_qco_zgr        ! called by isfcpl.F90
   PUBLIC  dom_qco_r3c        ! called by steplf.F90

   !                                                      !!* Namelist nam_vvl
   LOGICAL , PUBLIC :: ln_vvl_zstar           = .FALSE.    ! zstar  vertical coordinate
   LOGICAL , PUBLIC :: ln_vvl_ztilde          = .FALSE.    ! ztilde vertical coordinate
   LOGICAL , PUBLIC :: ln_vvl_layer           = .FALSE.    ! level  vertical coordinate
   LOGICAL , PUBLIC :: ln_vvl_ztilde_as_zstar = .FALSE.    ! ztilde vertical coordinate
   LOGICAL , PUBLIC :: ln_vvl_zstar_at_eqtor  = .FALSE.    ! ztilde vertical coordinate
   LOGICAL , PUBLIC :: ln_vvl_kepe            = .FALSE.    ! kinetic/potential energy transfer
   !                                                       ! conservation: not used yet
   !
   INTEGER          :: nn_vvl_interp = 0                   ! scale factors anomaly interpolation method at U-V-F points
                                                           ! =0 linear with no bottom correction over steps (old)
                                                           ! =1 linear with bottom correction over steps
                                                           ! =2 "qco like", i.e. proportional to thicknesses at rest
   !
   REAL(wp)         :: rn_ahe3                             ! thickness diffusion coefficient
   REAL(wp)         :: rn_rst_e3t                          ! ztilde to zstar restoration timescale [days]
   REAL(wp)         :: rn_lf_cutoff                        ! cutoff frequency for low-pass filter  [days]
   REAL(wp)         :: rn_zdef_max                         ! maximum fractional e3t deformation
   LOGICAL , PUBLIC :: ln_vvl_dbg = .FALSE.                ! debug control prints

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: domvvl.F90 12377 2020-02-12 14:39:06Z acc $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dom_qco_init( Kbb, Kmm, Kaa )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE dom_qco_init  ***
      !!
      !! ** Purpose :  Initialization of all ssh. to h._0 ratio
      !!
      !! ** Method  :  - use restart file and/or initialize
      !!               - compute ssh. to h._0 ratio
      !!
      !! ** Action  : - r3(t/u/v)_b
      !!              - r3(t/u/v/f)_n
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   Kbb, Kmm, Kaa   ! time level indices
      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'dom_qco_init : Variable volume activated'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~'
      !
      CALL qco_ctl                            ! choose vertical coordinate (z_star, z_tilde or layer)
      !
      CALL dom_qco_zgr( Kbb, Kmm )            ! interpolation scale factor, depth and water column
      !
#if defined key_agrif
      ! We need to define r3[tuv](Kaa) for AGRIF initialisation (should not be a
      ! problem for the restartability...)
      r3t(:,:,Kaa) = r3t(:,:,Kmm)
      r3u(:,:,Kaa) = r3u(:,:,Kmm)
      r3v(:,:,Kaa) = r3v(:,:,Kmm)
#endif
      !
   END SUBROUTINE dom_qco_init


   SUBROUTINE dom_qco_zgr( Kbb, Kmm )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE dom_qco_init  ***
      !!
      !! ** Purpose :  Initialization of all r3. = ssh./h._0 ratios
      !!
      !! ** Method  :  Call domqco using Kbb and Kmm
      !!               NB: dom_qco_zgr is called by dom_qco_init it uses ssh from ssh_init 
      !!
      !! ** Action  : - r3(t/u/v)(Kbb)
      !!              - r3(t/u/v/f)(Kmm)
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   Kbb, Kmm   ! time level indices
      !!----------------------------------------------------------------------
      !
      !                    !== Set of all other vertical scale factors  ==!  (now and before)
      !                                ! Horizontal interpolation of e3t
#if defined key_RK3
      CALL dom_qco_r3c( ssh(:,:,Kbb), r3t(:,:,Kbb), r3u(:,:,Kbb), r3v(:,:,Kbb), r3f(:,:) )
      CALL dom_qco_r3c( ssh(:,:,Kmm), r3t(:,:,Kmm), r3u(:,:,Kmm), r3v(:,:,Kmm)           )
#else
      CALL dom_qco_r3c( ssh(:,:,Kbb), r3t(:,:,Kbb), r3u(:,:,Kbb), r3v(:,:,Kbb)           )
      CALL dom_qco_r3c( ssh(:,:,Kmm), r3t(:,:,Kmm), r3u(:,:,Kmm), r3v(:,:,Kmm), r3f(:,:) )
#endif
      ! dom_qco_r3c defines over [nn_hls, nn_hls-1, nn_hls, nn_hls-1]
      IF( nn_hls == 2 ) CALL lbc_lnk( 'dom_qco_zgr', r3u(:,:,Kbb), 'U', 1._wp, r3v(:,:,Kbb), 'V', 1._wp, &
         &                                           r3u(:,:,Kmm), 'U', 1._wp, r3v(:,:,Kmm), 'V', 1._wp, r3f(:,:), 'F', 1._wp )
      !                                                                                                ! r3f is needed for agrif
   END SUBROUTINE dom_qco_zgr


   SUBROUTINE dom_qco_r3c( pssh, pr3t, pr3u, pr3v, pr3f )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE r3c  ***
      !!
      !! ** Purpose :   compute the filtered ratio ssh/h_0 at t-,u-,v-,f-points
      !!
      !! ** Method  : - compute the ssh at u- and v-points (f-point optional)
      !!                   Vector Form : surface weighted averaging
      !!                   Flux   Form : simple           averaging
      !!              - compute the ratio ssh/h_0 at t-,u-,v-pts, (f-pt optional)
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:)          , INTENT(in   )  ::   pssh               ! sea surface height   [m]
      REAL(wp), DIMENSION(:,:)          , INTENT(  out)  ::   pr3t, pr3u, pr3v   ! ssh/h0 ratio at t-, u-, v-,points  [-]
      REAL(wp), DIMENSION(:,:), OPTIONAL, INTENT(  out)  ::   pr3f               ! ssh/h0 ratio at f-point   [-]
      !
      INTEGER ::   ji, jj   ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      !
      DO_2D_OVR( nn_hls, nn_hls, nn_hls, nn_hls )
         pr3t(ji,jj) = pssh(ji,jj) * r1_ht_0(ji,jj)   !==  ratio at t-point  ==!
      END_2D
      !
      !
      !                                      !==  ratio at u-,v-point  ==!
      !
!!st      IF( ln_dynadv_vec ) THEN                     !- Vector Form   (thickness weighted averaging)
#if ! defined key_qcoTest_FluxForm
      !                                ! no 'key_qcoTest_FluxForm' : surface weighted ssh average
      DO_2D_OVR( nn_hls, nn_hls-1, nn_hls, nn_hls-1 )
         pr3u(ji,jj) = 0.5_wp * (  e1e2t(ji  ,jj) * pssh(ji  ,jj)  &
            &                    + e1e2t(ji+1,jj) * pssh(ji+1,jj)  ) * r1_hu_0(ji,jj) * r1_e1e2u(ji,jj)
         pr3v(ji,jj) = 0.5_wp * (  e1e2t(ji,jj  ) * pssh(ji,jj  )  &
            &                    + e1e2t(ji,jj+1) * pssh(ji,jj+1)  ) * r1_hv_0(ji,jj) * r1_e1e2v(ji,jj)
      END_2D
!!st      ELSE                                         !- Flux Form   (simple averaging)
#else
      DO_2D_OVR( nn_hls, nn_hls-1, nn_hls, nn_hls-1 )
         pr3u(ji,jj) = 0.5_wp * (  pssh(ji,jj) + pssh(ji+1,jj  )  ) * r1_hu_0(ji,jj)
         pr3v(ji,jj) = 0.5_wp * (  pssh(ji,jj) + pssh(ji  ,jj+1)  ) * r1_hv_0(ji,jj)
      END_2D
!!st      ENDIF
#endif         
      !
      IF( .NOT.PRESENT( pr3f ) ) THEN              !- lbc on ratio at u-, v-points only
         IF (nn_hls==1) CALL lbc_lnk( 'dom_qco_r3c', pr3u, 'U', 1._wp, pr3v, 'V', 1._wp )
         !
         !
      ELSE                                   !==  ratio at f-point  ==!
         !
!!st         IF( ln_dynadv_vec )   THEN                !- Vector Form   (thickness weighted averaging)
#if ! defined key_qcoTest_FluxForm
         !                                ! no 'key_qcoTest_FluxForm' : surface weighted ssh average

      DO_2D_OVR( nn_hls, nn_hls-1, nn_hls, nn_hls-1 )
         ! round brackets added to fix the order of floating point operations
         ! needed to ensure halo 1 - halo 2 compatibility
         pr3f(ji,jj) = 0.25_wp * ( ( e1e2t(ji  ,jj  ) * pssh(ji  ,jj  )   &
            &                      + e1e2t(ji+1,jj  ) * pssh(ji+1,jj  )   &
            &                      )                                      & ! bracket for halo 1 - halo 2 compatibility
            &                     + ( e1e2t(ji  ,jj+1) * pssh(ji  ,jj+1)  &
            &                       + e1e2t(ji+1,jj+1) * pssh(ji+1,jj+1)  &
            &                       )                                     & ! bracket for halo 1 - halo 2 compatibility
            &                    ) * r1_hf_0(ji,jj) * r1_e1e2f(ji,jj)
      END_2D
!!st         ELSE                                      !- Flux Form   (simple averaging)
#else
      DO_2D_OVR( nn_hls, nn_hls-1, nn_hls, nn_hls-1 )
         ! round brackets added to fix the order of floating point operations
         ! needed to ensure halo 1 - halo 2 compatibility
         pr3f(ji,jj) = 0.25_wp * ( ( pssh(ji,jj  ) + pssh(ji+1,jj  ) ) &
            &                    + ( pssh(ji,jj+1) + pssh(ji+1,jj+1)   &
            &                       )                                  & ! bracket for halo 1 - halo 2 compatibility
            &                    ) * r1_hf_0(ji,jj)
      END_2D
!!st         ENDIF
#endif
         !                                                 ! lbc on ratio at u-,v-,f-points
         IF (nn_hls==1) CALL lbc_lnk( 'dom_qco_r3c', pr3u, 'U', 1._wp, pr3v, 'V', 1._wp, pr3f, 'F', 1._wp )
         !
      ENDIF
      !
   END SUBROUTINE dom_qco_r3c


   SUBROUTINE qco_ctl
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE qco_ctl  ***
      !!
      !! ** Purpose :   Control the consistency between namelist options
      !!                for vertical coordinate
      !!----------------------------------------------------------------------
      INTEGER ::   ioptio, ios
      !!
      NAMELIST/nam_vvl/ ln_vvl_zstar, ln_vvl_ztilde, ln_vvl_layer, ln_vvl_ztilde_as_zstar, &
         &              ln_vvl_zstar_at_eqtor      , rn_ahe3     , rn_rst_e3t            , &
         &              rn_lf_cutoff               , rn_zdef_max , ln_vvl_dbg            , &  ! not yet implemented: ln_vvl_kepe
         &              nn_vvl_interp
      !!----------------------------------------------------------------------
      !
      READ  ( numnam_ref, nam_vvl, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'nam_vvl in reference namelist' )
      READ  ( numnam_cfg, nam_vvl, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 ) CALL ctl_nam ( ios , 'nam_vvl in configuration namelist' )
      IF(lwm) WRITE ( numond, nam_vvl )
      !
      IF(lwp) THEN                    ! Namelist print
         WRITE(numout,*)
         WRITE(numout,*) 'qco_ctl : choice/control of the variable vertical coordinate'
         WRITE(numout,*) '~~~~~~~~'
         WRITE(numout,*) '   Namelist nam_vvl : chose a vertical coordinate'
         WRITE(numout,*) '      zstar                      ln_vvl_zstar   = ', ln_vvl_zstar
         WRITE(numout,*) '      ztilde                     ln_vvl_ztilde  = ', ln_vvl_ztilde
         WRITE(numout,*) '      layer                      ln_vvl_layer   = ', ln_vvl_layer
         WRITE(numout,*) '      ztilde as zstar   ln_vvl_ztilde_as_zstar  = ', ln_vvl_ztilde_as_zstar
         WRITE(numout,*) '      ztilde near the equator    ln_vvl_zstar_at_eqtor  = ', ln_vvl_zstar_at_eqtor
         WRITE(numout,*) '      !'
         WRITE(numout,*) '      thickness diffusion coefficient                      rn_ahe3      = ', rn_ahe3
         WRITE(numout,*) '      maximum e3t deformation fractional change            rn_zdef_max  = ', rn_zdef_max
         IF( ln_vvl_ztilde_as_zstar ) THEN
            WRITE(numout,*) '      ztilde running in zstar emulation mode (ln_vvl_ztilde_as_zstar=T) '
            WRITE(numout,*) '         ignoring namelist timescale parameters and using:'
            WRITE(numout,*) '            hard-wired : z-tilde to zstar restoration timescale (days)'
            WRITE(numout,*) '                         rn_rst_e3t     = 0.e0'
            WRITE(numout,*) '            hard-wired : z-tilde cutoff frequency of low-pass filter (days)'
            WRITE(numout,*) '                         rn_lf_cutoff   = 1.0/rn_Dt'
         ELSE
            WRITE(numout,*) '      z-tilde to zstar restoration timescale (days)        rn_rst_e3t   = ', rn_rst_e3t
            WRITE(numout,*) '      z-tilde cutoff frequency of low-pass filter (days)   rn_lf_cutoff = ', rn_lf_cutoff
         ENDIF
         WRITE(numout,*) '         debug prints flag                                 ln_vvl_dbg   = ', ln_vvl_dbg
      ENDIF
      !
      ioptio = 0                      ! Parameter control
      IF( ln_vvl_ztilde_as_zstar )   ln_vvl_ztilde = .true.
      IF( ln_vvl_zstar           )   ioptio = ioptio + 1
      IF( ln_vvl_ztilde          )   ioptio = ioptio + 1
      IF( ln_vvl_layer           )   ioptio = ioptio + 1
      !
      IF( ioptio /= 1 )   CALL ctl_stop( 'Choose ONE vertical coordinate in namelist nam_vvl' )
      !
      IF(lwp) THEN                   ! Print the choice
         WRITE(numout,*)
         IF( ln_vvl_zstar           ) WRITE(numout,*) '      ==>>>   zstar vertical coordinate is used'
         IF( ln_vvl_ztilde          ) WRITE(numout,*) '      ==>>>   ztilde vertical coordinate is used'
         IF( ln_vvl_layer           ) WRITE(numout,*) '      ==>>>   layer vertical coordinate is used'
         IF( ln_vvl_ztilde_as_zstar ) WRITE(numout,*) '      ==>>>   to emulate a zstar coordinate'
      ENDIF
      !
#if defined key_agrif
      IF( (.NOT.Agrif_Root()).AND.(.NOT.ln_vvl_zstar) )   CALL ctl_stop( 'AGRIF is implemented with zstar coordinate only' )
#endif
      !
   END SUBROUTINE qco_ctl

   !!======================================================================
END MODULE domqco
