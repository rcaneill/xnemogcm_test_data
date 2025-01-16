MODULE bdytra
   !!======================================================================
   !!                       ***  MODULE  bdytra  ***
   !! Ocean tracers:   Apply boundary conditions for tracers
   !!======================================================================
   !! History :  1.0  !  2005-01  (J. Chanut, A. Sellar)  Original code
   !!            3.0  !  2008-04  (NEMO team)  add in the reference version
   !!            3.4  !  2011     (D. Storkey) rewrite in preparation for OBC-BDY merge
   !!            3.5  !  2012     (S. Mocavero, I. Epicoco) Optimization of BDY communications
   !!            4.0  !  2016     (T. Lovato) Generalize OBC structure
   !!----------------------------------------------------------------------
   !!   bdy_tra       : Apply open boundary conditions & damping to T and S
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers variables
   USE dom_oce        ! ocean space and time domain variables
   USE bdy_oce        ! ocean open boundary conditions
   USE bdylib         ! for orlanski library routines
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp, ONLY: jpfillnothing
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp, ONLY: ctl_stop
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   ! Local structure to rearrange tracers data
   TYPE, PUBLIC ::   ztrabdy
      REAL(wp), POINTER, DIMENSION(:,:) ::  tra
   END TYPE

   PUBLIC   bdy_tra      ! called in tranxt.F90
   PUBLIC   bdy_tra_dmp  ! called in step.F90

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: bdytra.F90 15354 2021-10-12 13:44:46Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE bdy_tra( kt, Kbb, pts, Kaa )
      !!----------------------------------------------------------------------
      !!                  ***  SUBROUTINE bdy_tra  ***
      !!
      !! ** Purpose : - Apply open boundary conditions for temperature and salinity
      !!
      !!----------------------------------------------------------------------
      INTEGER                                  , INTENT(in)    :: kt        ! Main time step counter
      INTEGER                                  , INTENT(in)    :: Kbb, Kaa  ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts,jpt), INTENT(inout) :: pts       ! tracer fields
      !
      INTEGER                        :: ib_bdy, jn, igrd, ir   ! Loop indeces
      TYPE(ztrabdy), DIMENSION(jpts) :: zdta                   ! Temporary data structure
      LOGICAL                        :: llrim0                 ! indicate if rim 0 is treated
      LOGICAL, DIMENSION(8)          :: llsend1, llrecv1       ! indicate how communications are to be carried out
      !!----------------------------------------------------------------------
      igrd = 1
      llsend1(:) = .false.  ;   llrecv1(:) = .false.
      DO ir = 1, 0, -1   ! treat rim 1 before rim 0
         IF( ir == 0 ) THEN   ;   llrim0 = .TRUE.
         ELSE                 ;   llrim0 = .FALSE.
         ENDIF
         DO ib_bdy=1, nb_bdy
            !
            zdta(1)%tra => dta_bdy(ib_bdy)%tem
            zdta(2)%tra => dta_bdy(ib_bdy)%sal
            !
            DO jn = 1, jpts
               !
               SELECT CASE( cn_tra(ib_bdy) )
               CASE('none'        )   ;   CYCLE
               CASE('frs'         )   ! treat the whole boundary at once
                  IF( ir == 0 )           CALL bdy_frs ( idx_bdy(ib_bdy),                    pts(:,:,:,jn,Kaa), zdta(jn)%tra )
               CASE('specified'   )   ! treat the whole rim      at once
                  IF( ir == 0 )           CALL bdy_spe ( idx_bdy(ib_bdy),                    pts(:,:,:,jn,Kaa), zdta(jn)%tra )
               CASE('neumann'     )   ;   CALL bdy_nmn ( idx_bdy(ib_bdy), igrd             , pts(:,:,:,jn,Kaa), llrim0 )   ! tsa masked
               CASE('orlanski'    )   ;   CALL bdy_orl ( idx_bdy(ib_bdy), pts(:,:,:,jn,Kbb), pts(:,:,:,jn,Kaa), zdta(jn)%tra,   &
                  &                                      llrim0, ll_npo=.FALSE. )
               CASE('orlanski_npo')   ;   CALL bdy_orl ( idx_bdy(ib_bdy), pts(:,:,:,jn,Kbb), pts(:,:,:,jn,Kaa), zdta(jn)%tra,   &
                  &                                      llrim0, ll_npo=.TRUE.  )
               CASE('runoff'      )   ;   CALL bdy_rnf ( idx_bdy(ib_bdy),                    pts(:,:,:,jn,Kaa), jn, llrim0 )
               CASE DEFAULT           ;   CALL ctl_stop( 'bdy_tra : unrecognised option for open boundaries for T and S' )
               END SELECT
               !
            END DO
         END DO
         !
         IF( nn_hls > 1 .AND. ir == 1 ) CYCLE   ! at least 2 halos will be corrected -> no need to correct rim 1 before rim 0
         IF( nn_hls == 1 ) THEN   ;   llsend1(:) = .false.   ;   llrecv1(:) = .false.   ;   ENDIF
         DO ib_bdy=1, nb_bdy
            SELECT CASE( cn_tra(ib_bdy) )
            CASE('neumann','runoff')
               llsend1(:) = llsend1(:) .OR. lsend_bdyint(ib_bdy,1,:,ir)   ! possibly every direction, T points
               llrecv1(:) = llrecv1(:) .OR. lrecv_bdyint(ib_bdy,1,:,ir)   ! possibly every direction, T points
            CASE('orlanski', 'orlanski_npo')
               llsend1(:) = llsend1(:) .OR. lsend_bdyolr(ib_bdy,1,:,ir)   ! possibly every direction, T points
               llrecv1(:) = llrecv1(:) .OR. lrecv_bdyolr(ib_bdy,1,:,ir)   ! possibly every direction, T points
            END SELECT
         END DO
         IF( ANY(llsend1) .OR. ANY(llrecv1) ) THEN   ! if need to send/recv in at least one direction
            CALL lbc_lnk( 'bdytra', pts(:,:,:,jn,Kaa), 'T',  1.0_wp, kfillmode=jpfillnothing ,lsend=llsend1, lrecv=llrecv1 )
         ENDIF
         !
      END DO   ! ir
      !
   END SUBROUTINE bdy_tra


   SUBROUTINE bdy_rnf( idx, pt, jpa, llrim0 )
      !!----------------------------------------------------------------------
      !!                 ***  SUBROUTINE bdy_rnf  ***
      !!
      !! ** Purpose : Specialized routine to apply TRA runoff values at OBs:
      !!                  - duplicate the neighbour value for the temperature
      !!                  - specified to 0.1 PSU for the salinity
      !!
      !!----------------------------------------------------------------------
      TYPE(OBC_INDEX),                     INTENT(in) ::   idx      ! OBC indices
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) ::   pt       ! tracer trend
      INTEGER,                             INTENT(in) ::   jpa      ! TRA index
      LOGICAL,                             INTENT(in) ::   llrim0   ! indicate if rim 0 is treated
      !
      INTEGER  ::   ib, ii, ij, igrd   ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      igrd = 1                       ! Everything is at T-points here
      IF(      jpa == jp_tem ) THEN
         CALL bdy_nmn( idx, igrd, pt, llrim0 )
      ELSE IF( jpa == jp_sal ) THEN
         IF( .NOT. llrim0 )   RETURN
         DO ib = 1, idx%nblenrim(igrd)   ! if llrim0 then treat the whole rim
            ii = idx%nbi(ib,igrd)
            ij = idx%nbj(ib,igrd)
            pt(ii,ij,1:jpkm1) = 0.1 * tmask(ii,ij,1:jpkm1)
         END DO
      ENDIF
      !
   END SUBROUTINE bdy_rnf


   SUBROUTINE bdy_tra_dmp( kt, Kbb, pts, Krhs )
      !!----------------------------------------------------------------------
      !!                 ***  SUBROUTINE bdy_tra_dmp  ***
      !!
      !! ** Purpose : Apply damping for tracers at open boundaries.
      !!
      !!----------------------------------------------------------------------
      INTEGER                                  , INTENT(in)    :: kt        ! time step
      INTEGER                                  , INTENT(in)    :: Kbb, Krhs ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts,jpt), INTENT(inout) :: pts       ! active tracers and RHS of tracer equation
      !
      REAL(wp) ::   zwgt           ! boundary weight
      REAL(wp) ::   zta, zsa, ztime
      INTEGER  ::   ib, ik, igrd   ! dummy loop indices
      INTEGER  ::   ii, ij         ! 2D addresses
      INTEGER  ::   ib_bdy         ! Loop index
      !!----------------------------------------------------------------------
      IF( l_istiled .AND. ntile /= 1 ) RETURN                        ! Do only for the full domain
      !
      IF( ln_timing )   CALL timing_start('bdy_tra_dmp')
      !
      DO ib_bdy = 1, nb_bdy
         IF( ln_tra_dmp(ib_bdy) ) THEN
            igrd = 1                       ! Everything is at T-points here
            DO ib = 1, idx_bdy(ib_bdy)%nblen(igrd)
               ii = idx_bdy(ib_bdy)%nbi(ib,igrd)
               ij = idx_bdy(ib_bdy)%nbj(ib,igrd)
               zwgt = idx_bdy(ib_bdy)%nbd(ib,igrd)
               DO ik = 1, jpkm1
                  zta = zwgt * ( dta_bdy(ib_bdy)%tem(ib,ik) - pts(ii,ij,ik,jp_tem,Kbb) ) * tmask(ii,ij,ik)
                  zsa = zwgt * ( dta_bdy(ib_bdy)%sal(ib,ik) - pts(ii,ij,ik,jp_sal,Kbb) ) * tmask(ii,ij,ik)
                  pts(ii,ij,ik,jp_tem,Krhs) = pts(ii,ij,ik,jp_tem,Krhs) + zta
                  pts(ii,ij,ik,jp_sal,Krhs) = pts(ii,ij,ik,jp_sal,Krhs) + zsa
               END DO
            END DO
         ENDIF
      END DO
      !
      IF( ln_timing )   CALL timing_stop('bdy_tra_dmp')
      !
   END SUBROUTINE bdy_tra_dmp

   !!======================================================================
END MODULE bdytra
