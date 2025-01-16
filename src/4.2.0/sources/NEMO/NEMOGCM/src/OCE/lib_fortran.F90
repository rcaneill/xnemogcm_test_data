MODULE lib_fortran
   !!======================================================================
   !!                       ***  MODULE  lib_fortran  ***
   !! Fortran utilities:  includes some low levels fortran functionality
   !!======================================================================
   !! History :  3.2  !  2010-05  (M. Dunphy, R. Benshila)  Original code
   !!            3.4  !  2013-06  (C. Rousset)  add glob_min, glob_max 
   !!                                           + 3d dim. of input is fexible (jpk, jpl...) 
   !!            4.0  !  2016-06  (T. Lovato)  double precision global sum by default 
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   glob_sum    : generic interface for global masked summation over
   !!                 the interior domain for 1 or 2 2D or 3D arrays
   !!                 it works only for T points
   !!   SIGN        : generic interface for SIGN to overwrite f95 behaviour
   !!                 of intrinsinc sign function
   !!----------------------------------------------------------------------
   USE par_oce         ! Ocean parameter
   USE dom_oce         ! ocean domain
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! distributed memory computing
   USE lbclnk          ! ocean lateral boundary conditions

   IMPLICIT NONE
   PRIVATE

   PUBLIC   glob_sum      ! used in many places (masked with tmask_i = ssmask * (excludes halo+duplicated points (NP folding)) )
   PUBLIC   local_sum     ! used in trcrad, local operation before glob_sum_delay
   PUBLIC   sum3x3        ! used in trcrad, do a sum over 3x3 boxes
   PUBLIC   DDPDD         ! also used in closea module
   PUBLIC   glob_min, glob_max
   PUBLIC   glob_sum_vec
   PUBLIC   glob_min_vec, glob_max_vec
#if defined key_nosignedzero
   PUBLIC SIGN
#endif

   INTERFACE glob_sum
      MODULE PROCEDURE glob_sum_1d, glob_sum_2d, glob_sum_3d
   END INTERFACE
   INTERFACE local_sum
      MODULE PROCEDURE local_sum_2d, local_sum_3d
   END INTERFACE
   INTERFACE sum3x3
      MODULE PROCEDURE sum3x3_2d, sum3x3_3d
   END INTERFACE
   INTERFACE glob_min
      MODULE PROCEDURE glob_min_2d, glob_min_3d
   END INTERFACE
   INTERFACE glob_max
      MODULE PROCEDURE glob_max_2d, glob_max_3d
   END INTERFACE
   INTERFACE glob_sum_vec
      MODULE PROCEDURE glob_sum_vec_3d, glob_sum_vec_4d
   END INTERFACE
   INTERFACE glob_min_vec
      MODULE PROCEDURE glob_min_vec_3d, glob_min_vec_4d
   END INTERFACE
   INTERFACE glob_max_vec
      MODULE PROCEDURE glob_max_vec_3d, glob_max_vec_4d
   END INTERFACE

#if defined key_nosignedzero
   INTERFACE SIGN
      MODULE PROCEDURE SIGN_SCALAR, SIGN_ARRAY_1D, SIGN_ARRAY_2D, SIGN_ARRAY_3D,   &
         &             SIGN_ARRAY_1D_A, SIGN_ARRAY_2D_A, SIGN_ARRAY_3D_A,          &
         &             SIGN_ARRAY_1D_B, SIGN_ARRAY_2D_B, SIGN_ARRAY_3D_B
   END INTERFACE
#endif

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: lib_fortran.F90 15376 2021-10-14 20:41:23Z clem $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

#  define GLOBSUM_CODE
#     define DIM_1d
#        include "lib_fortran_generic.h90"
#     undef DIM_1d
#     define DIM_2d
#        include "lib_fortran_generic.h90"
#     undef DIM_2d
#     define DIM_3d
#        include "lib_fortran_generic.h90"
#     undef DIM_3d
#  undef GLOBSUM_CODE

#  define GLOBMINMAX_CODE
#     define DIM_2d
#        define OPERATION_GLOBMIN
#           include "lib_fortran_generic.h90"
#        undef OPERATION_GLOBMIN
#        define OPERATION_GLOBMAX
#           include "lib_fortran_generic.h90"
#        undef OPERATION_GLOBMAX
#     undef DIM_2d
#     define DIM_3d
#        define OPERATION_GLOBMIN
#           include "lib_fortran_generic.h90"
#        undef OPERATION_GLOBMIN
#        define OPERATION_GLOBMAX
#           include "lib_fortran_generic.h90"
#        undef OPERATION_GLOBMAX
#     undef DIM_3
#  undef GLOBMINMAX_CODE

!                          ! FUNCTION local_sum !

   FUNCTION local_sum_2d( ptab )
      !!----------------------------------------------------------------------
      REAL(wp),  INTENT(in   ) ::   ptab(:,:) ! array on which operation is applied
      COMPLEX(dp)              ::  local_sum_2d
      !
      !!-----------------------------------------------------------------------
      !
      COMPLEX(dp)::   ctmp
      REAL(wp)   ::   ztmp
      INTEGER    ::   ji, jj    ! dummy loop indices
      INTEGER    ::   ipi, ipj  ! dimensions
      !!-----------------------------------------------------------------------
      !
      ipi = SIZE(ptab,1)   ! 1st dimension
      ipj = SIZE(ptab,2)   ! 2nd dimension
      !
      ctmp = CMPLX( 0.e0, 0.e0, wp )   ! warning ctmp is cumulated

      DO jj = 1, ipj
         DO ji = 1, ipi
            ztmp =  ptab(ji,jj) * tmask_i(ji,jj)
            CALL DDPDD( CMPLX( ztmp, 0.e0, dp ), ctmp )
         END DO
      END DO
      !
      local_sum_2d = ctmp
       
   END FUNCTION local_sum_2d

   FUNCTION local_sum_3d( ptab )
      !!----------------------------------------------------------------------
      REAL(wp),  INTENT(in   ) ::   ptab(:,:,:) ! array on which operation is applied
      COMPLEX(dp)              ::  local_sum_3d
      !
      !!-----------------------------------------------------------------------
      !
      COMPLEX(dp)::   ctmp
      REAL(wp)   ::   ztmp
      INTEGER    ::   ji, jj, jk   ! dummy loop indices
      INTEGER    ::   ipi, ipj, ipk    ! dimensions
      !!-----------------------------------------------------------------------
      !
      ipi = SIZE(ptab,1)   ! 1st dimension
      ipj = SIZE(ptab,2)   ! 2nd dimension
      ipk = SIZE(ptab,3)   ! 3rd dimension
      !
      ctmp = CMPLX( 0.e0, 0.e0, wp )   ! warning ctmp is cumulated

      DO jk = 1, ipk
        DO jj = 1, ipj
          DO ji = 1, ipi
             ztmp =  ptab(ji,jj,jk) * tmask_i(ji,jj)
             CALL DDPDD( CMPLX( ztmp, 0.e0, dp ), ctmp )
          END DO
        END DO
      END DO
      !
      local_sum_3d = ctmp
       
   END FUNCTION local_sum_3d

!                          ! FUNCTION sum3x3 !

   SUBROUTINE sum3x3_2d( p2d )
      !!-----------------------------------------------------------------------
      !!                  ***  routine sum3x3_2d  ***
      !!
      !! ** Purpose : sum over 3x3 boxes
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION (:,:), INTENT(inout) ::   p2d
      !
      INTEGER ::   ji, ji2, jj, jj2     ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      IF( SIZE(p2d,1) /= jpi ) CALL ctl_stop( 'STOP', 'wrong call of sum3x3_2d, the first dimension is not equal to jpi' ) 
      IF( SIZE(p2d,2) /= jpj ) CALL ctl_stop( 'STOP', 'wrong call of sum3x3_2d, the second dimension is not equal to jpj' ) 
      !
      ! work over the whole domain (guarantees all internal cells are set when nn_hls=2)
      !
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         IF( MOD(mig(ji), 3) == MOD(nn_hls, 3) .AND.   &              ! 1st bottom left corner always at (Nis0-1, Njs0-1)
           & MOD(mjg(jj), 3) == MOD(nn_hls, 3)         ) THEN         ! bottom left corner of a 3x3 box
            ji2 = MIN(mig(ji)+2, jpiglo) - nimpp + 1                  ! right position of the box
            jj2 = MIN(mjg(jj)+2, jpjglo) - njmpp + 1                  ! upper position of the box
            IF( ji2 <= jpi .AND. jj2 <= jpj ) THEN                    ! the box is fully included in the local mpi domain
               p2d(ji:ji2,jj:jj2) = SUM(p2d(ji:ji2,jj:jj2))
            ENDIF
         ENDIF
      END_2D
      CALL lbc_lnk( 'lib_fortran', p2d, 'T', 1.0_wp )
      ! no need for 2nd exchange when nn_hls > 1
      IF( nn_hls == 1 ) THEN
         IF( mpiRnei(nn_hls,jpwe) > -1 ) THEN   ! 1st column was changed during the previous call to lbc_lnk
            IF( MOD(mig(    1), 3) == 1 )   &   ! 1st box start at i=1 -> column 1 to 3 correctly computed locally
               p2d(    1,:) = p2d(    2,:)      ! previous lbc_lnk corrupted column 1 -> put it back using column 2 
            IF( MOD(mig(    1), 3) == 2 )   &   ! 1st box start at i=3 -> column 1 and 2 correctly computed on west neighbourh
               p2d(    2,:) = p2d(    1,:)      !  previous lbc_lnk fix column 1 -> copy it to column 2 
         ENDIF
         IF( mpiRnei(nn_hls,jpea) > -1 ) THEN
            IF( MOD(mig(jpi-2), 3) == 1 )   p2d(  jpi,:) = p2d(jpi-1,:)
            IF( MOD(mig(jpi-2), 3) == 0 )   p2d(jpi-1,:) = p2d(  jpi,:)
         ENDIF
         IF( mpiRnei(nn_hls,jpso) > -1 ) THEN
            IF( MOD(mjg(    1), 3) == 1 )   p2d(:,    1) = p2d(:,    2)
            IF( MOD(mjg(    1), 3) == 2 )   p2d(:,    2) = p2d(:,    1)
         ENDIF
         IF( mpiRnei(nn_hls,jpno) > -1 ) THEN
            IF( MOD(mjg(jpj-2), 3) == 1 )   p2d(:,  jpj) = p2d(:,jpj-1)
            IF( MOD(mjg(jpj-2), 3) == 0 )   p2d(:,jpj-1) = p2d(:,  jpj)
         ENDIF
         CALL lbc_lnk( 'lib_fortran', p2d, 'T', 1.0_wp )
      ENDIF

   END SUBROUTINE sum3x3_2d

   SUBROUTINE sum3x3_3d( p3d )
      !!-----------------------------------------------------------------------
      !!                  ***  routine sum3x3_3d  ***
      !!
      !! ** Purpose : sum over 3x3 boxes
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION (:,:,:), INTENT(inout) ::   p3d
      !
      INTEGER ::   ji, ji2, jj, jj2, jn     ! dummy loop indices
      INTEGER ::   ipn                      ! Third dimension size
      !!----------------------------------------------------------------------
      !
      IF( SIZE(p3d,1) /= jpi ) CALL ctl_stop( 'STOP', 'wrong call of sum3x3_3d, the first dimension is not equal to jpi' ) 
      IF( SIZE(p3d,2) /= jpj ) CALL ctl_stop( 'STOP', 'wrong call of sum3x3_3d, the second dimension is not equal to jpj' ) 
      ipn = SIZE(p3d,3)
      !
      DO jn = 1, ipn
         !
         ! work over the whole domain (guarantees all internal cells are set when nn_hls=2)
         !
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            IF( MOD(mig(ji), 3) == MOD(nn_hls, 3) .AND.   &              ! 1st bottom left corner always at (Nis0-1, Njs0-1)
              & MOD(mjg(jj), 3) == MOD(nn_hls, 3)         ) THEN         ! bottom left corner of a 3x3 box
               ji2 = MIN(mig(ji)+2, jpiglo) - nimpp + 1                  ! right position of the box
               jj2 = MIN(mjg(jj)+2, jpjglo) - njmpp + 1                  ! upper position of the box
               IF( ji2 <= jpi .AND. jj2 <= jpj ) THEN                    ! the box is fully included in the local mpi domain
                  p3d(ji:ji2,jj:jj2,jn) = SUM(p3d(ji:ji2,jj:jj2,jn))
               ENDIF
            ENDIF
         END_2D
      END DO
      CALL lbc_lnk( 'lib_fortran', p3d, 'T', 1.0_wp )
      ! no need for 2nd exchange when nn_hls > 1
      IF( nn_hls == 1 ) THEN
         IF( mpiRnei(nn_hls,jpwe) > -1 ) THEN    ! 1st column was changed during the previous call to lbc_lnk
            IF( MOD(mig(    1), 3) == 1 )   &    ! 1st box start at i=1 -> column 1 to 3 correctly computed locally
               p3d(    1,:,:) = p3d(    2,:,:)   ! previous lbc_lnk corrupted column 1 -> put it back using column 2 
            IF( MOD(mig(    1), 3) == 2 )   &    ! 1st box start at i=3 -> column 1 and 2 correctly computed on west neighbourh
               p3d(    2,:,:) = p3d(    1,:,:)   !  previous lbc_lnk fix column 1 -> copy it to column 2 
         ENDIF
         IF( mpiRnei(nn_hls,jpea) > -1 ) THEN
            IF( MOD(mig(jpi-2), 3) == 1 )   p3d(  jpi,:,:) = p3d(jpi-1,:,:)
            IF( MOD(mig(jpi-2), 3) == 0 )   p3d(jpi-1,:,:) = p3d(  jpi,:,:)
         ENDIF
         IF( mpiRnei(nn_hls,jpso) > -1 ) THEN
            IF( MOD(mjg(    1), 3) == 1 )   p3d(:,    1,:) = p3d(:,    2,:)
            IF( MOD(mjg(    1), 3) == 2 )   p3d(:,    2,:) = p3d(:,    1,:)
         ENDIF
         IF( mpiRnei(nn_hls,jpno) > -1 ) THEN
            IF( MOD(mjg(jpj-2), 3) == 1 )   p3d(:,  jpj,:) = p3d(:,jpj-1,:)
            IF( MOD(mjg(jpj-2), 3) == 0 )   p3d(:,jpj-1,:) = p3d(:,  jpj,:)
         ENDIF
         CALL lbc_lnk( 'lib_fortran', p3d, 'T', 1.0_wp )
      ENDIF

   END SUBROUTINE sum3x3_3d


   FUNCTION glob_sum_vec_3d( cdname, ptab ) RESULT( ptmp )
      !!----------------------------------------------------------------------
      CHARACTER(len=*),  INTENT(in) ::   cdname      ! name of the calling subroutine
      REAL(wp),          INTENT(in) ::   ptab(:,:,:) ! array on which operation is applied
      REAL(wp), DIMENSION(SIZE(ptab,3)) ::   ptmp
      !
      COMPLEX(dp), DIMENSION(:), ALLOCATABLE ::   ctmp
      REAL(wp)    ::   ztmp
      INTEGER     ::   ji , jj , jk     ! dummy loop indices
      INTEGER     ::   ipi, ipj, ipk    ! dimensions
      INTEGER     ::   iis, iie, ijs, ije   ! loop start and end
      !!-----------------------------------------------------------------------
      !
      ipi = SIZE(ptab,1)   ! 1st dimension
      ipj = SIZE(ptab,2)   ! 2nd dimension
      ipk = SIZE(ptab,3)   ! 3rd dimension
      !
      IF( ipi == jpi .AND. ipj == jpj ) THEN   ! do 2D loop only over the inner domain (-> avoid to use undefined values)
         iis = Nis0   ;   iie = Nie0
         ijs = Njs0   ;   ije = Nje0
      ELSE                                     ! I think we are never in this case...
         iis = 1   ;   iie = jpi
         ijs = 1   ;   ije = jpj
      ENDIF
      !
      ALLOCATE( ctmp(ipk) )
      !
      DO jk = 1, ipk
         ctmp(jk) = CMPLX( 0.e0, 0.e0, dp )   ! warning ctmp is cumulated
         DO jj = ijs, ije
            DO ji = iis, iie
               ztmp =  ptab(ji,jj,jk) * tmask_i(ji,jj)
               CALL DDPDD( CMPLX( ztmp, 0.e0, dp ), ctmp(jk) )
            END DO
         END DO
      END DO
      CALL mpp_sum( cdname, ctmp(:) )   ! sum over the global domain
      !
      ptmp = REAL( ctmp(:), wp )
      !
      DEALLOCATE( ctmp )
      !
   END FUNCTION glob_sum_vec_3d

   FUNCTION glob_sum_vec_4d( cdname, ptab ) RESULT( ptmp )
      !!----------------------------------------------------------------------
      CHARACTER(len=*),  INTENT(in) ::   cdname        ! name of the calling subroutine
      REAL(wp),          INTENT(in) ::   ptab(:,:,:,:) ! array on which operation is applied
      REAL(wp), DIMENSION(SIZE(ptab,4)) ::   ptmp
      !
      COMPLEX(dp), DIMENSION(:), ALLOCATABLE ::   ctmp
      REAL(wp)    ::   ztmp
      INTEGER     ::   ji , jj , jk , jl     ! dummy loop indices
      INTEGER     ::   ipi, ipj, ipk, ipl    ! dimensions
      INTEGER     ::   iis, iie, ijs, ije    ! loop start and end
      !!-----------------------------------------------------------------------
      !
      ipi = SIZE(ptab,1)   ! 1st dimension
      ipj = SIZE(ptab,2)   ! 2nd dimension
      ipk = SIZE(ptab,3)   ! 3rd dimension
      ipl = SIZE(ptab,4)   ! 4th dimension
      !
      IF( ipi == jpi .AND. ipj == jpj ) THEN   ! do 2D loop only over the inner domain (-> avoid to use undefined values)
         iis = Nis0   ;   iie = Nie0
         ijs = Njs0   ;   ije = Nje0
      ELSE                                     ! I think we are never in this case...
         iis = 1   ;   iie = jpi
         ijs = 1   ;   ije = jpj
      ENDIF
      !
      ALLOCATE( ctmp(ipl) )
      !
      DO jl = 1, ipl
         ctmp(jl) = CMPLX( 0.e0, 0.e0, dp )   ! warning ctmp is cumulated
         DO jk = 1, ipk
            DO jj = ijs, ije
               DO ji = iis, iie
                  ztmp =  ptab(ji,jj,jk,jl) * tmask_i(ji,jj)
                  CALL DDPDD( CMPLX( ztmp, 0.e0, dp ), ctmp(jl) )
               END DO
            END DO
         END DO
      END DO
      CALL mpp_sum( cdname, ctmp(:) )   ! sum over the global domain
      !
      ptmp = REAL( ctmp(:), wp )
      !
      DEALLOCATE( ctmp )
      !
   END FUNCTION glob_sum_vec_4d

   FUNCTION glob_min_vec_3d( cdname, ptab ) RESULT( ptmp )
      !!----------------------------------------------------------------------
      CHARACTER(len=*),  INTENT(in) ::   cdname        ! name of the calling subroutine
      REAL(wp),          INTENT(in) ::   ptab(:,:,:)   ! array on which operation is applied
      REAL(wp), DIMENSION(SIZE(ptab,3)) ::   ptmp
      !
      INTEGER     ::   jk    ! dummy loop indice & dimension
      INTEGER     ::   ipk   ! dimension
      !!-----------------------------------------------------------------------
      !
      ipk = SIZE(ptab,3)
      DO jk = 1, ipk
         ptmp(jk) = MINVAL( ptab(:,:,jk) * tmask_i(:,:) )
      ENDDO
      !
      CALL mpp_min( cdname, ptmp (:) )
      !
   END FUNCTION glob_min_vec_3d

   FUNCTION glob_min_vec_4d( cdname, ptab ) RESULT( ptmp )
      !!----------------------------------------------------------------------
      CHARACTER(len=*),  INTENT(in) ::   cdname          ! name of the calling subroutine
      REAL(wp),          INTENT(in) ::   ptab(:,:,:,:)   ! array on which operation is applied
      REAL(wp), DIMENSION(SIZE(ptab,4)) ::   ptmp
      !
      INTEGER     ::   jk , jl    ! dummy loop indice & dimension
      INTEGER     ::   ipk, ipl   ! dimension
      !!-----------------------------------------------------------------------
      !
      ipk = SIZE(ptab,3)
      ipl = SIZE(ptab,4)
      DO jl = 1, ipl
            ptmp(jl) = MINVAL( ptab(:,:,1,jl) * tmask_i(:,:) )         
         DO jk = 2, ipk
            ptmp(jl) = MIN( ptmp(jl), MINVAL( ptab(:,:,jk,jl) * tmask_i(:,:) ) )
         ENDDO
      ENDDO
      !
      CALL mpp_min( cdname, ptmp (:) )
      !
   END FUNCTION glob_min_vec_4d
   
   FUNCTION glob_max_vec_3d( cdname, ptab ) RESULT( ptmp )
      !!----------------------------------------------------------------------
      CHARACTER(len=*),  INTENT(in) ::   cdname        ! name of the calling subroutine
      REAL(wp),          INTENT(in) ::   ptab(:,:,:)   ! array on which operation is applied
      REAL(wp), DIMENSION(SIZE(ptab,3)) ::   ptmp
      !
      INTEGER     ::   jk    ! dummy loop indice & dimension
      INTEGER     ::   ipk   ! dimension
      !!-----------------------------------------------------------------------
      !
      ipk = SIZE(ptab,3)
      DO jk = 1, ipk
         ptmp(jk) = MAXVAL( ptab(:,:,jk) * tmask_i(:,:) )
      ENDDO
      !
      CALL mpp_max( cdname, ptmp (:) )
      !
   END FUNCTION glob_max_vec_3d

   FUNCTION glob_max_vec_4d( cdname, ptab ) RESULT( ptmp )
      !!----------------------------------------------------------------------
      CHARACTER(len=*),  INTENT(in) ::   cdname          ! name of the calling subroutine
      REAL(wp),          INTENT(in) ::   ptab(:,:,:,:)   ! array on which operation is applied
      REAL(wp), DIMENSION(SIZE(ptab,4)) ::   ptmp
      !
      INTEGER     ::   jk , jl    ! dummy loop indice & dimension
      INTEGER     ::   ipk, ipl   ! dimension
      !!-----------------------------------------------------------------------
      !
      ipk = SIZE(ptab,3)
      ipl = SIZE(ptab,4)
      DO jl = 1, ipl
            ptmp(jl) = MAXVAL( ptab(:,:,1,jl) * tmask_i(:,:) )         
         DO jk = 2, ipk
            ptmp(jl) = MAX( ptmp(jl), MAXVAL( ptab(:,:,jk,jl) * tmask_i(:,:) ) )
         ENDDO
      ENDDO
      !
      CALL mpp_max( cdname, ptmp (:) )
      !
   END FUNCTION glob_max_vec_4d
   
   SUBROUTINE DDPDD( ydda, yddb )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE DDPDD ***
      !!
      !! ** Purpose : Add a scalar element to a sum
      !!
      !!
      !! ** Method  : The code uses the compensated summation with doublet
      !!              (sum,error) emulated useing complex numbers. ydda is the
      !!               scalar to add to the summ yddb
      !!
      !! ** Action  : This does only work for MPI.
      !!
      !! References : Using Acurate Arithmetics to Improve Numerical
      !!              Reproducibility and Sability in Parallel Applications
      !!              Yun HE and Chris H. Q. DING, Journal of Supercomputing 18, 259-277, 2001
      !!----------------------------------------------------------------------
      COMPLEX(dp), INTENT(in   ) ::   ydda
      COMPLEX(dp), INTENT(inout) ::   yddb
      !
      REAL(dp) :: zerr, zt1, zt2  ! local work variables
      !!-----------------------------------------------------------------------
      !
      ! Compute ydda + yddb using Knuth's trick.
      zt1  = REAL(ydda) + REAL(yddb)
      zerr = zt1 - REAL(ydda)
      zt2  = ( (REAL(yddb) - zerr) + (REAL(ydda) - (zt1 - zerr)) )   &
         &   + AIMAG(ydda)         + AIMAG(yddb)
      !
      ! The result is t1 + t2, after normalization.
      yddb = CMPLX( zt1 + zt2, zt2 - ((zt1 + zt2) - zt1), wp )
      !
   END SUBROUTINE DDPDD

#if defined key_nosignedzero
   !!----------------------------------------------------------------------
   !!   'key_nosignedzero'                                         F90 SIGN
   !!----------------------------------------------------------------------

   FUNCTION SIGN_SCALAR( pa, pb )
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION SIGN_SCALAR  ***
      !!
      !! ** Purpose : overwrite f95 behaviour of intrinsinc sign function
      !!-----------------------------------------------------------------------
      REAL(wp) :: pa,pb          ! input
      REAL(wp) :: SIGN_SCALAR    ! result
      !!-----------------------------------------------------------------------
      IF ( pb >= 0.e0) THEN   ;   SIGN_SCALAR = ABS(pa)
      ELSE                    ;   SIGN_SCALAR =-ABS(pa)
      ENDIF
   END FUNCTION SIGN_SCALAR


   FUNCTION SIGN_ARRAY_1D( pa, pb )
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION SIGN_ARRAY_1D  ***
      !!
      !! ** Purpose : overwrite f95 behaviour of intrinsinc sign function
      !!-----------------------------------------------------------------------
      REAL(wp) :: pa,pb(:)                   ! input
      REAL(wp) :: SIGN_ARRAY_1D(SIZE(pb,1))  ! result
      !!-----------------------------------------------------------------------
      WHERE ( pb >= 0.e0 )   ;   SIGN_ARRAY_1D = ABS(pa)
      ELSEWHERE              ;   SIGN_ARRAY_1D =-ABS(pa)
      END WHERE
   END FUNCTION SIGN_ARRAY_1D


   FUNCTION SIGN_ARRAY_2D(pa,pb)
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION SIGN_ARRAY_2D  ***
      !!
      !! ** Purpose : overwrite f95 behaviour of intrinsinc sign function
      !!-----------------------------------------------------------------------
      REAL(wp) :: pa,pb(:,:)      ! input
      REAL(wp) :: SIGN_ARRAY_2D(SIZE(pb,1),SIZE(pb,2))  ! result
      !!-----------------------------------------------------------------------
      WHERE ( pb >= 0.e0 )   ;   SIGN_ARRAY_2D = ABS(pa)
      ELSEWHERE              ;   SIGN_ARRAY_2D =-ABS(pa)
      END WHERE
   END FUNCTION SIGN_ARRAY_2D

   FUNCTION SIGN_ARRAY_3D(pa,pb)
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION SIGN_ARRAY_3D  ***
      !!
      !! ** Purpose : overwrite f95 behaviour of intrinsinc sign function
      !!-----------------------------------------------------------------------
      REAL(wp) :: pa,pb(:,:,:)      ! input
      REAL(wp) :: SIGN_ARRAY_3D(SIZE(pb,1),SIZE(pb,2),SIZE(pb,3))  ! result
      !!-----------------------------------------------------------------------
      WHERE ( pb >= 0.e0 )   ;   SIGN_ARRAY_3D = ABS(pa)
      ELSEWHERE              ;   SIGN_ARRAY_3D =-ABS(pa)
      END WHERE
   END FUNCTION SIGN_ARRAY_3D


   FUNCTION SIGN_ARRAY_1D_A(pa,pb)
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION SIGN_ARRAY_1D_A  ***
      !!
      !! ** Purpose : overwrite f95 behaviour of intrinsinc sign function
      !!-----------------------------------------------------------------------
      REAL(wp) :: pa(:),pb(:)      ! input
      REAL(wp) :: SIGN_ARRAY_1D_A(SIZE(pb,1))  ! result
      !!-----------------------------------------------------------------------
      WHERE ( pb >= 0.e0 )   ;   SIGN_ARRAY_1D_A = ABS(pa)
      ELSEWHERE              ;   SIGN_ARRAY_1D_A =-ABS(pa)
      END WHERE
   END FUNCTION SIGN_ARRAY_1D_A


   FUNCTION SIGN_ARRAY_2D_A(pa,pb)
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION SIGN_ARRAY_2D_A  ***
      !!
      !! ** Purpose : overwrite f95 behaviour of intrinsinc sign function
      !!-----------------------------------------------------------------------
      REAL(wp) :: pa(:,:),pb(:,:)      ! input
      REAL(wp) :: SIGN_ARRAY_2D_A(SIZE(pb,1),SIZE(pb,2))  ! result
      !!-----------------------------------------------------------------------
      WHERE ( pb >= 0.e0 )   ;   SIGN_ARRAY_2D_A = ABS(pa)
      ELSEWHERE              ;   SIGN_ARRAY_2D_A =-ABS(pa)
      END WHERE
   END FUNCTION SIGN_ARRAY_2D_A


   FUNCTION SIGN_ARRAY_3D_A(pa,pb)
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION SIGN_ARRAY_3D_A  ***
      !!
      !! ** Purpose : overwrite f95 behaviour of intrinsinc sign function
      !!-----------------------------------------------------------------------
      REAL(wp) :: pa(:,:,:),pb(:,:,:)  ! input
      REAL(wp) :: SIGN_ARRAY_3D_A(SIZE(pb,1),SIZE(pb,2),SIZE(pb,3)) ! result
      !!-----------------------------------------------------------------------
      WHERE ( pb >= 0.e0 )   ;   SIGN_ARRAY_3D_A = ABS(pa)
      ELSEWHERE              ;   SIGN_ARRAY_3D_A =-ABS(pa)
      END WHERE
   END FUNCTION SIGN_ARRAY_3D_A


   FUNCTION SIGN_ARRAY_1D_B(pa,pb)
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION SIGN_ARRAY_1D_B  ***
      !!
      !! ** Purpose : overwrite f95 behaviour of intrinsinc sign function
      !!-----------------------------------------------------------------------
      REAL(wp) :: pa(:),pb      ! input
      REAL(wp) :: SIGN_ARRAY_1D_B(SIZE(pa,1))  ! result
      !!-----------------------------------------------------------------------
      IF( pb >= 0.e0 ) THEN   ;   SIGN_ARRAY_1D_B = ABS(pa)
      ELSE                    ;   SIGN_ARRAY_1D_B =-ABS(pa)
      ENDIF
   END FUNCTION SIGN_ARRAY_1D_B


   FUNCTION SIGN_ARRAY_2D_B(pa,pb)
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION SIGN_ARRAY_2D_B  ***
      !!
      !! ** Purpose : overwrite f95 behaviour of intrinsinc sign function
      !!-----------------------------------------------------------------------
      REAL(wp) :: pa(:,:),pb      ! input
      REAL(wp) :: SIGN_ARRAY_2D_B(SIZE(pa,1),SIZE(pa,2))  ! result
      !!-----------------------------------------------------------------------
      IF( pb >= 0.e0 ) THEN   ;   SIGN_ARRAY_2D_B = ABS(pa)
      ELSE                    ;   SIGN_ARRAY_2D_B =-ABS(pa)
      ENDIF
   END FUNCTION SIGN_ARRAY_2D_B


   FUNCTION SIGN_ARRAY_3D_B(pa,pb)
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION SIGN_ARRAY_3D_B  ***
      !!
      !! ** Purpose : overwrite f95 behaviour of intrinsinc sign function
      !!-----------------------------------------------------------------------
      REAL(wp) :: pa(:,:,:),pb      ! input
      REAL(wp) :: SIGN_ARRAY_3D_B(SIZE(pa,1),SIZE(pa,2),SIZE(pa,3))  ! result
      !!-----------------------------------------------------------------------
      IF( pb >= 0.e0 ) THEN   ;   SIGN_ARRAY_3D_B = ABS(pa)
      ELSE                    ;   SIGN_ARRAY_3D_B =-ABS(pa)
      ENDIF
   END FUNCTION SIGN_ARRAY_3D_B
#endif

   !!======================================================================
END MODULE lib_fortran
