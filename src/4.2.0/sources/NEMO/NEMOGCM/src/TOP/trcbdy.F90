MODULE trcbdy
   !!======================================================================
   !!                       ***  MODULE  bdytrc  ***
   !! Ocean tracers:   Apply boundary conditions for tracers in TOP component
   !!======================================================================
   !! History :  1.0  !  2005-01  (J. Chanut, A. Sellar)  Original code
   !!            3.0  !  2008-04  (NEMO team)  add in the reference version
   !!            3.4  !  2011     (D. Storkey) rewrite in preparation for OBC-BDY merge
   !!            3.5  !  2012     (S. Mocavero, I. Epicoco) Optimization of BDY communications
   !!            3.6  !  2015     (T. Lovato) Adapt BDY for tracers in TOP component
   !!            4.0  !  2016     (T. Lovato) Generalize OBC structure
   !!----------------------------------------------------------------------
#if defined key_top
   !!----------------------------------------------------------------------
   !!   trc_bdy       : Apply open boundary conditions & damping to tracers
   !!----------------------------------------------------------------------
   USE timing                       ! Timing
   USE oce_trc                      ! ocean dynamics and tracers variables
   USE par_trc
   USE trc                          ! ocean space and time domain variables 
   USE bdylib                       ! for orlanski library routines
   USE lbclnk                       ! ocean lateral boundary conditions (or mpp link)
   USE in_out_manager               ! I/O manager
   USE bdy_oce                      ! ocean open boundary conditions

   IMPLICIT NONE
   PRIVATE

   PUBLIC trc_bdy      ! routine called in trcnxt.F90 
   PUBLIC trc_bdy_dmp  ! routine called in trcstp.F90 

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcbdy.F90 15354 2021-10-12 13:44:46Z smasson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_bdy( kt, Kbb, Kmm, Krhs )
      !!----------------------------------------------------------------------
      !!                  ***  SUBROUTINE trc_bdy  ***
      !!
      !! ** Purpose : - Apply open boundary conditions for TOP tracers
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) :: kt              ! Main time step counter
      INTEGER, INTENT( in ) :: Kbb, Kmm, Krhs  ! time level indices
      !!
      INTEGER                           :: ib_bdy ,ir, jn ,igrd ! Loop indices
      REAL(wp), POINTER, DIMENSION(:,:) ::  ztrc
      LOGICAL                           :: llrim0               ! indicate if rim 0 is treated
      LOGICAL, DIMENSION(8)             :: llsend1, llrecv1     ! indicate how communications are to be carried out
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_bdy')
      !
      igrd = 1 
      llsend1(:) = .false.  ;   llrecv1(:) = .false.
      DO ir = 1, 0, -1   ! treat rim 1 before rim 0
         IF( ir == 0 ) THEN   ;   llrim0 = .TRUE.
         ELSE                 ;   llrim0 = .FALSE.
         ENDIF
         DO ib_bdy=1, nb_bdy
            !
            DO jn = 1, jptra
               !
               IF( ASSOCIATED(trcdta_bdy(jn,ib_bdy)%trc) .AND. trcdta_bdy(jn,ib_bdy)%cn_obc /= 'neumann' ) THEN
                  IF( .NOT. ASSOCIATED(ztrc) )   ALLOCATE( ztrc(idx_bdy(ib_bdy)%nblen(igrd),jpk) )
                  ztrc(:,:) = trcdta_bdy(jn,ib_bdy)%trc(:,:) * trcdta_bdy(jn,ib_bdy)%rn_fac
               ENDIF
               !
               SELECT CASE( trcdta_bdy(jn,ib_bdy)%cn_obc )
               CASE('none'        )   ;   CYCLE
               CASE('frs'         )   ! treat the whole boundary at once
                  IF( ir == 0 )           CALL bdy_frs( idx_bdy(ib_bdy),                   tr(:,:,:,jn,Krhs), ztrc )
               CASE('specified'   )   ! treat the whole rim      at once
                  IF( ir == 0 )           CALL bdy_spe( idx_bdy(ib_bdy),                   tr(:,:,:,jn,Krhs), ztrc )
               CASE('neumann'     )   ;   CALL bdy_nmn( idx_bdy(ib_bdy), igrd            , tr(:,:,:,jn,Krhs),       llrim0 )   ! tra masked
               CASE('orlanski'    )   ;   CALL bdy_orl( idx_bdy(ib_bdy), tr(:,:,:,jn,Kbb), tr(:,:,:,jn,Krhs), ztrc, llrim0,   &
                  &                                     ll_npo=.FALSE. )
               CASE('orlanski_npo')   ;   CALL bdy_orl( idx_bdy(ib_bdy), tr(:,:,:,jn,Kbb), tr(:,:,:,jn,Krhs), ztrc, llrim0,   &
                  &                                     ll_npo=.TRUE.  )
               CASE DEFAULT           ;   CALL ctl_stop( 'trc_bdy : unrecognised option for open boundaries for passive tracers' )
               END SELECT
               !
            END DO
            !
            IF( ASSOCIATED(ztrc) )   DEALLOCATE(ztrc)
            !
         END DO
         !
         IF( nn_hls > 1 .AND. ir == 1 ) CYCLE   ! at least 2 halos will be corrected -> no need to correct rim 1 before rim 0
         IF( nn_hls == 1 ) THEN   ;   llsend1(:) = .false.   ;   llrecv1(:) = .false.   ;   ENDIF
         DO ib_bdy=1, nb_bdy
            SELECT CASE( cn_tra(ib_bdy) )
            CASE('neumann')
               llsend1(:) = llsend1(:) .OR. lsend_bdyint(ib_bdy,1,:,ir)   ! possibly every direction, T points
               llrecv1(:) = llrecv1(:) .OR. lrecv_bdyint(ib_bdy,1,:,ir)   ! possibly every direction, T points
            CASE('orlanski','orlanski_npo')
               llsend1(:) = llsend1(:) .OR. lsend_bdyolr(ib_bdy,1,:,ir)   ! possibly every direction, T points
               llrecv1(:) = llrecv1(:) .OR. lrecv_bdyolr(ib_bdy,1,:,ir)   ! possibly every direction, T points
            END SELECT
         END DO
         IF( ANY(llsend1) .OR. ANY(llrecv1) ) THEN   ! if need to send/recv in at least one direction
            CALL lbc_lnk( 'trcbdy', tr(:,:,:,:,Krhs), 'T',  1.0_wp, kfillmode=jpfillnothing ,lsend=llsend1, lrecv=llrecv1 )
         ENDIF
         !
      END DO   ! ir
      !
      IF( ln_timing )   CALL timing_stop('trc_bdy')
      !
   END SUBROUTINE trc_bdy


   SUBROUTINE trc_bdy_dmp( kt, Kbb, Krhs )
      !!----------------------------------------------------------------------
      !!                 ***  SUBROUTINE trc_bdy_dmp  ***
      !!                    
      !! ** Purpose : Apply damping for tracers at open boundaries.
      !!             It currently applies the damping to all tracers!!!
      !! 
      !!----------------------------------------------------------------------
      INTEGER,         INTENT(in) ::   kt
      INTEGER,         INTENT(in) ::   Kbb, Krhs  ! time level indices
      !! 
      INTEGER  ::   jn             ! Tracer index
      REAL(wp) ::   zwgt           ! boundary weight
      REAL(wp) ::   zta, zsa, ztime
      INTEGER  ::   ib, ik, igrd   ! dummy loop indices
      INTEGER  ::   ii, ij         ! 2D addresses
      INTEGER  ::   ib_bdy         ! Loop index
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_bdy_dmp')
      !
      DO jn = 1, jptra
         DO ib_bdy=1, nb_bdy
            IF( trcdta_bdy(jn, ib_bdy)%dmp ) THEN
               igrd = 1                       ! Everything is at T-points here
               DO ib = 1, idx_bdy(ib_bdy)%nblen(igrd)
                  ii = idx_bdy(ib_bdy)%nbi(ib,igrd)
                  ij = idx_bdy(ib_bdy)%nbj(ib,igrd)
                  zwgt = idx_bdy(ib_bdy)%nbd(ib,igrd)
                  DO ik = 1, jpkm1
                     zta = zwgt * ( trcdta_bdy(jn, ib_bdy)%trc(ib,ik) - tr(ii,ij,ik,jn,Kbb) ) * tmask(ii,ij,ik)
                     tr(ii,ij,ik,jn,Krhs) = tr(ii,ij,ik,jn,Krhs) + zta
                  END DO
               END DO
            ENDIF
         END DO
      END DO
      !
      IF( ln_timing )   CALL timing_stop('trc_bdy_dmp')
      !
   END SUBROUTINE trc_bdy_dmp
 
#else
   !!----------------------------------------------------------------------
   !!   Dummy module                   NO Unstruct Open Boundary Conditions
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_bdy(kt)      ! Empty routine
      WRITE(*,*) 'trc_bdy: You should not have seen this print! error?', kt
   END SUBROUTINE trc_bdy

   SUBROUTINE trc_bdy_dmp(kt)      ! Empty routine
      WRITE(*,*) 'trc_bdy_dmp: You should not have seen this print! error?', kt
   END SUBROUTINE trc_bdy_dmp

#endif

   !!======================================================================
END MODULE trcbdy
