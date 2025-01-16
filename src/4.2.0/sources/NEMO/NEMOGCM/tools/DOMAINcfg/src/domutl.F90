MODULE domutl
   !!
   !!======================================================================
   !!                       ***  MODULE  closea  ***
   !! modutl : specific module to do
   !!              - 2d flood filling (closea)
   !!              - 3d flood filling (zgr_isf)
   !!
   !!======================================================================
   !! History :   4.0  !  03-18  (P. Mathiot) Original code
   !!----------------------------------------------------------------------
   !!
   !!----------------------------------------------------------------------
   !!   fillpool    : replace all point connected to the seed by a fillvalue 
   !!----------------------------------------------------------------------
   !!
   USE dom_oce         ! ocean space and time domain
   USE domngb
   USE in_out_manager    ! I/O manager
   USE lib_mpp
   USE lbclnk

   IMPLICIT NONE
   PRIVATE

   PUBLIC fill_pool      ! routine called by domain module

   INTERFACE fill_pool
      MODULE PROCEDURE FillPool2D, FillPool3D
   END INTERFACE

   TYPE idx
      INTEGER :: i,j,k
      TYPE(idx), POINTER :: next
   END TYPE idx

CONTAINS
   SUBROUTINE FillPool3D(kiseed, kjseed, kkseed, rdta, rfill)
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE FillPool3D  ***
      !!
      !! ** Purpose :  Replace all area surrounding by mask value by kifill value
      !!
      !! ** Method  :  flood fill algorithm
      !!
      !!----------------------------------------------------------------------
      INTEGER,                          INTENT(in)    :: kiseed, kjseed, kkseed ! seed
      REAL(wp),                         INTENT(in)    :: rfill                  ! filling value
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) :: rdta                   ! input data
      REAL(wp), DIMENSION(jpi,jpj,jpk)                :: rseedmap, rseedmap_b   ! map of seed (use for processor communication)
 
      INTEGER :: ii  , ij  , ik   , kii, kjj, jj, kk    ! working integer
      INTEGER :: iip1, ijp1, ikp1                       ! working integer
      INTEGER :: iim1, ijm1, ikm1                       ! working integer
      INTEGER :: nseed                                  ! size of the stack
      TYPE (idx), POINTER :: seed => NULL()
      !!---------------------------------------------------------------------- 
      !
      ! initialisation seed
  !    NULLIFY(seed)
      !
      ! create the first seed and update nseed for all processors
      nseed = 0
      rseedmap = 0.
      ii=kiseed; ij=kjseed ; ik=kkseed
      IF ((mi0(ii) == mi1(ii)) .AND. (mj0(ij) == mj1(ij))) THEN   ! T if seed on the local domain
         ii = mi0(ii) ; ij = mj0(ij);                             ! conversion to local index
         IF (rdta(ii,ij,ik) > 0 ) THEN                            ! create seed if not on land
            CALL add_head_idx(seed,ii,ij,ik)
            rdta    (ii,ij,ik) = rfill
            rseedmap(ii,ij,ik) = 1.
         END IF
      END IF
      nseed=SUM(rseedmap); IF( lk_mpp )   CALL mpp_sum('domutl', nseed )  ! nseed =0 means on land => WARNING later on
      !
      ! loop until the stack size is 0 or if the pool is larger than the critical size
      IF (nseed > 0) THEN
         ! seed on ocean continue
         DO WHILE ( nseed /= 0 )
            DO WHILE ( ASSOCIATED(seed) )
               ii=seed%i; ij=seed%j ; ik=seed%k ; rseedmap(ii,ij,ik)=1.
               ! 
               ! update bathy and update stack size
               CALL del_head_idx(seed) 
               !
               ! check neighbour cells
               iip1=ii+1 ; IF (iip1 == jpi+1 ) iip1=ii
               iim1=ii-1 ; IF (iim1 == 0     ) iim1=ii
               ijp1=ij+1 ; IF (ijp1 == jpj+1 ) ijp1=ij
               ijm1=ij-1 ; IF (ijm1 == 0     ) ijm1=ij
               ikp1=ik+1 ; IF (ikp1 == jpk+1 ) ikp1=ik
               ikm1=ik-1 ; IF (ikm1 == 0     ) ikm1=ik
               !
               ! ji direction
               IF (rdta(ii,ijp1,ik) > 0 .AND. rdta(ii,ijp1,ik) /= rfill ) THEN ; CALL add_head_idx(seed,ii,ijp1,ik) ; rdta(ii,ijp1,ik) = rfill ; rseedmap(ii,ijp1,ik)=1. ; END IF
               IF (rdta(ii,ijm1,ik) > 0 .AND. rdta(ii,ijm1,ik) /= rfill ) THEN ; CALL add_head_idx(seed,ii,ijm1,ik) ; rdta(ii,ijm1,ik) = rfill ; rseedmap(ii,ijm1,ik)=1. ; END IF
               !
               ! jj direction
               IF (rdta(iip1,ij,ik) > 0 .AND. rdta(iip1,ij,ik) /= rfill ) THEN ; CALL add_head_idx(seed,iip1,ij,ik) ; rdta(iip1,ij,ik) = rfill ; rseedmap(iip1,ij,ik)=1. ; END IF
               IF (rdta(iim1,ij,ik) > 0 .AND. rdta(iim1,ij,ik) /= rfill ) THEN ; CALL add_head_idx(seed,iim1,ij,ik) ; rdta(iim1,ij,ik) = rfill ; rseedmap(iim1,ij,ik)=1. ; END IF
               !
               ! jk direction
               IF (rdta(ii,ij,ikp1) > 0 .AND. rdta(ii,ij,ikp1) /= rfill ) THEN ; CALL add_head_idx(seed,ii,ij,ikp1) ; rdta(ii,ij,ikp1) = rfill ; rseedmap(ii,ij,ik)=1. ; END IF
               IF (rdta(ii,ij,ikm1) > 0 .AND. rdta(ii,ij,ikm1) /= rfill ) THEN ; CALL add_head_idx(seed,ii,ij,ikm1) ; rdta(ii,ij,ikm1) = rfill ; rseedmap(ii,ij,ik)=1. ; END IF
            END DO
            !
            ! exchange seed
            nseed=SUM(rseedmap); IF( lk_mpp )   CALL mpp_sum('domutl', nseed )  ! this is the sum of all the point check this iteration
            !
            rseedmap_b(:,:,:)=rseedmap(:,:,:)
            CALL lbc_lnk('domutl', rseedmap, 'T', 1.)
            !
            ! build new list of seed
            DO ii=1,jpi
               DO jj=1,jpj
                  DO kk=1,jpk
                     IF (rseedmap(ii,jj,kk) > 0.0 .AND. rseedmap(ii,jj,kk) /= rseedmap_b(ii,jj,kk) .AND. rdta(ii,jj,kk) > 0 .AND. rdta(ii,jj,kk) /= rfill) THEN
                        CALL add_head_idx(seed,ii,jj,kk)
                     END IF
                  END DO
               END DO
            END DO
            !
            ! reset map of seed
            rseedmap(:,:,:)=0.0
            !
         END DO
      ELSE
         WRITE(numout,*) 'W A R N I N G : SEED (',ii,ij,') is on land, nothing to do'
      END IF
 
   END SUBROUTINE FillPool3D

   SUBROUTINE FillPool2D(kiseed, kjseed, rdta, rfill)
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE FillPool3D  ***
      !!
      !! ** Purpose :  Replace all area surrounding by mask value by kifill value
      !!
      !! ** Method  :  flood fill algorithm
      !!
      !!----------------------------------------------------------------------
      INTEGER,                      INTENT(in)    :: kiseed, kjseed ! seed
      REAL(wp),                     INTENT(in)    :: rfill          ! filling value
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) :: rdta           ! input data
      REAL(wp), DIMENSION(jpi,jpj) :: rseedmap                      ! location of new seed (used for processor exchange)
 
      INTEGER :: ii  , ij  , jj, kii, kjj     ! working integer
      INTEGER :: iip1, ijp1                   ! working integer
      INTEGER :: iim1, ijm1                   ! working integer
      INTEGER :: nseed                        ! size of the stack
      TYPE (idx), POINTER :: seed => NULL()
      !!----------------------------------------------------------------------
      !
      ! initialisation seed
      !NULLIFY(seed)
      !
      ! create the first seed and update nseed for all processors
      nseed = 0
      rseedmap = 0.
      ii = kiseed ; ij = kjseed ;
      IF ((mi0(ii) == mi1(ii)) .AND. (mj0(ij) == mj1(ij))) THEN   ! T if seed on the local domain
         ii = mi0(ii) ; ij = mj0(ij)                              ! conversion to local index
         IF (rdta(ii,ij) > 0 ) THEN                               ! create seed if not on land
            CALL add_head_idx(seed,ii,ij,1)
            rdta    (ii,ij) = rfill
            rseedmap(ii,ij) = 1.
         END IF
      END IF
      nseed=SUM(rseedmap); IF( lk_mpp )   CALL mpp_sum('domutl', nseed )  ! nseed =0 means on land => WARNING later on
      !
      ! loop until the stack size is 0 or if the pool is larger than the critical size
      IF (nseed > 0) THEN
         ! seed on ocean continue
         DO WHILE ( nseed .NE. 0 )
            DO WHILE ( ASSOCIATED(seed) )
               ii=seed%i; ij=seed%j
               ! update stack size
               CALL del_head_idx(seed) 
               ! 
               ! check neighbour cells
               iip1=ii+1 ; IF (iip1 == jpi+1 ) iip1=ii
               iim1=ii-1 ; IF (iim1 == 0     ) iim1=ii
               ijp1=ij+1 ; IF (ijp1 == jpj+1 ) ijp1=ij
               ijm1=ij-1 ; IF (ijm1 == 0     ) ijm1=ij
               !
               ! ji direction
               IF (rdta(ii  ,ijp1) > 0 .AND. rdta(ii  ,ijp1) /= rfill ) THEN ; CALL add_head_idx(seed,ii  ,ijp1,1) ; rdta(ii  ,ijp1) = rfill ; rseedmap(ii  ,ijp1)=1. ; END IF
               IF (rdta(ii  ,ijm1) > 0 .AND. rdta(ii  ,ijm1) /= rfill ) THEN ; CALL add_head_idx(seed,ii  ,ijm1,1) ; rdta(ii  ,ijm1) = rfill ; rseedmap(ii  ,ijm1)=1. ; END IF
               !
               ! jj direction
               IF (rdta(iip1,ij  ) > 0 .AND. rdta(iip1,ij  ) /= rfill ) THEN ; CALL add_head_idx(seed,iip1,ij  ,1) ; rdta(iip1,ij  ) = rfill ; rseedmap(iip1,ij  )=1. ; END IF
               IF (rdta(iim1,ij  ) > 0 .AND. rdta(iim1,ij  ) /= rfill ) THEN ; CALL add_head_idx(seed,iim1,ij  ,1) ; rdta(iim1,ij  ) = rfill ; rseedmap(iim1,ij  )=1. ; END IF
            END DO
            !
            ! exchange seed
            nseed=SUM(rseedmap); IF( lk_mpp )   CALL mpp_sum('domutl', nseed )  ! this is the sum of all the point check this iteration
            !
            CALL lbc_lnk('domutl', rseedmap, 'T', 1.)
            !
            ! build new list of seed
            ! new seed only if data > 0 (ie not land), data not already filled and rseedmap > 0
            DO ii=1,jpi
               DO jj=1,jpj
                  IF (rseedmap(ii,jj) > 0.0 .AND. rdta(ii,jj) > 0 .AND. rdta(ii,jj) /= rfill) THEN
                     CALL add_head_idx(seed, ii, jj, 1)
                  END IF
               END DO
            END DO 
            !
            ! reset map of seed
            rseedmap(:,:)=0.0
            !
         END DO
      ELSE
         WRITE(numout,*) 'W A R N I N G : SEED (',ii,ij,') is on land, nothing to do', narea
      END IF
 
   END SUBROUTINE FillPool2D
   !
   !
   ! subroutine to deals with link list
   !
   SUBROUTINE add_head_idx(pt_idx, ki, kj, kk)
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE add_head_idx  ***
      !!
      !! ** Purpose : add one element in the linked list
      !!
      !! ** Method  :  allocate one element, then point %next to the linked list
      !!----------------------------------------------------------------------
      TYPE (idx), POINTER :: pt_idx
      TYPE (idx), POINTER :: zpt_new
      INTEGER, INTENT(in) :: ki, kj, kk
      !
      ! allocate new element
      ALLOCATE(zpt_new)
      zpt_new%i=ki ; zpt_new%j=kj ; zpt_new%k=kk ;
      zpt_new%next => NULL()
      !
      ! linked new element to main list
      zpt_new%next => pt_idx
      pt_idx => zpt_new
   END SUBROUTINE add_head_idx

   SUBROUTINE del_head_idx(pt_idx)
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE del_head_idx  ***
      !!
      !! ** Purpose : delete one element in the linked list
      !!
      !! ** Method  : move the pointer to the next node 
      !!----------------------------------------------------------------------
      TYPE (idx), POINTER :: pt_idx
      TYPE (idx), POINTER :: zpt_tmp
      !
      ! delete first element
      IF (ASSOCIATED(pt_idx%next)) THEN
         zpt_tmp => pt_idx%next
         DEALLOCATE(pt_idx)
         !NULLIFY(pt_idx)
         pt_idx => NULL()
         pt_idx => zpt_tmp
      ELSE
         !NULLIFY(pt_idx)
         pt_idx => NULL()
      ENDIF
   END SUBROUTINE del_head_idx
   !
END MODULE domutl
