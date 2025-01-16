MODULE trosk
# if ! defined key_agrif
!****************************************************************
!* NUMERICAL SOLUTION OF A STIFF SYSTEM OF FIRST 0RDER ORDINARY *
!* DIFFERENTIAL EQUATIONS Y'=F(X,Y) BY ROSENBROCK METHOD.       *
!* ------------------------------------------------------------ *
!* ------------------------------------------------------------ *
!* Ref.: From Numath Library By Tuan Dang Trong in Fortran 77   *
!*       [BIBLI 18].                                            * 
!*                                                              *
!*                       F90 Release 1.0 By J-P Moreau, Paris   *
!*                                (www.jpmoreau.fr)             *
!****************************************************************
  USE timing
  USE in_out_manager, ONLY : ln_timing ! I/O manager
  USE sed
  USE sedfunc

  IMPLICIT NONE
  PRIVATE 

  PUBLIC rosk

  INTEGER, ALLOCATABLE, SAVE, DIMENSION(:) :: NFCN, NJAC, NSTEP, NACCPT, NREJCT, NDEC, NSOL


!define example #1
  INTERFACE
       SUBROUTINE JAC(NEQ,Y,DFY,LDFY,ACCMASK)
         INTEGER, PARAMETER :: WP = KIND(1.0D0)
         INTEGER, INTENT(IN) :: NEQ, LDFY
         REAL(WP)                  , INTENT(IN)  :: Y
         REAL(WP), DIMENSION(:,:,:), INTENT(OUT) :: DFY
         INTEGER , DIMENSION(:), INTENT(IN) :: ACCMASK
       END SUBROUTINE JAC
  END INTERFACE


  CONTAINS

!**********************************************************************
SUBROUTINE rosk(ROSM,N,X,Y,XEND,H, RTOL,ATOL,ITOL,                  &
           &    JAC, MLJAC, MUJAC, WORK,LWORK,IDID,ISTAT,RSTAT)
! ---------------------------------------------------------------------
!     NUMERICAL SOLUTION OF A STIFF (OR DIFFERENTIAL ALGEBRAIC)
!     SYSTEM OF FIRST 0RDER ORDINARY DIFFERENTIAL EQUATIONS  MY'=F(X,Y).
!     THIS IS AN EMBEDDED ROSENBROCK METHOD OF ORDER (3)4
!     (WITH STEP SIZE CONTROL).
!     C.F. SECTION IV.7
!
!     AUTHORS: E. HAIRER AND G. WANNER
!              UNIVERSITE DE GENEVE, DEPT. DE MATHEMATIQUES
!              CH-1211 GENEVE 24, SWITZERLAND
!              E-MAIL:  HAIRER@CGEUGE51.BITNET,  WANNER@CGEUGE51.BITNET
!
!     THIS CODE IS PART OF THE BOOK:
!         E. HAIRER AND G. WANNER, SOLVING ORDINARY DIFFERENTIAL
!         EQUATIONS II. STIFF AND DIFFERENTIAL-ALGEBRAIC PROBLEMS.
!         SPRINGER SERIES IN COMPUTATIONAL MATHEMATICS,
!         SPRINGER-VERLAG (1990)
!
!     VERSION OF OCTOBER 12, 1990
!
!     INPUT PARAMETERS
!     ----------------
!     N           DIMENSION OF THE SYSTEM
!
!     FCN         NAME (EXTERNAL) OF SUBROUTINE COMPUTING THE
!                 VALUE OF F(X,Y):
!                    SUBROUTINE FCN(N,X,Y,F)
!                    REAL*8 X,Y(N),F(N)
!                    F(1)=...   ETC.
!
!     X           INITIAL X-VALUE
!
!     Y(N)        INITIAL VALUES FOR Y
!
!     XEND        FINAL X-VALUE (XEND-X MAY BE POSITIVE OR NEGATIVE)
!
!     H           INITIAL STEP SIZE GUESS;
!                 FOR STIFF EQUATIONS WITH INITIAL TRANSIENT,
!                 H=1.D0/(NORM OF F'), USUALLY 1.D-2 OR 1.D-3, IS GOOD.
!                 THIS CHOICE IS NOT VERY IMPORTANT, THE CODE QUICKLY
!                 ADAPTS ITS STEP SIZE. STUDY THE CHOSEN VALUES FOR A FEW
!                 STEPS IN SUBROUTINE "SOLOUT", WHEN YOU ARE NOT SURE.
!                 (IF H=0.D0, THE CODE PUTS H=1.D-6).
!
!     RTOL,ATOL   RELATIVE AND ABSOLUTE ERROR TOLERANCES. THEY
!                 CAN BE BOTH SCALARS OR ELSE BOTH VECTORS OF LENGTH N.
!
!     ITOL        SWITCH FOR RTOL AND ATOL:
!                   ITOL=0: BOTH RTOL AND ATOL ARE SCALARS.
!                     THE CODE KEEPS, ROUGHLY, THE LOCAL ERROR OF
!                     Y(I) BELOW RTOL*ABS(Y(I))+ATOL
!                   ITOL=1: BOTH RTOL AND ATOL ARE VECTORS.
!                     THE CODE KEEPS THE LOCAL ERROR OF Y(I) BELOW
!                     RTOL(I)*ABS(Y(I))+ATOL(I).
!
!     JAC         NAME (EXTERNAL) OF THE SUBROUTINE WHICH COMPUTES
!                 THE PARTIAL DERIVATIVES OF F(X,Y) WITH RESPECT TO Y
!                 FOR IJAC=1, THIS SUBROUTINE MUST HAVE THE FORM:
!                    SUBROUTINE JAC(N,X,Y,DFY,LDFY)
!                    REAL*8 X,Y(N),DFY(LDFY,N)
!                    DFY(1,1)= ...
!                 LDFY, THE COLUMN-LENGTH OF THE ARRAY, IS
!                 FURNISHED BY THE CALLING PROGRAM.
!                 THE JACOBIAN IS TAKEN AS BANDED AND
!                    THE PARTIAL DERIVATIVES ARE STORED
!                    DIAGONAL-WISE AS
!                       DFY(I-J+MUJAC+1,J) = PARTIAL F(I) / PARTIAL Y(J).
!
!     MLJAC       SWITCH FOR THE BANDED STRUCTURE OF THE JACOBIAN:
!                    0<=MLJAC<N: MLJAC IS THE LOWER BANDWITH OF JACOBIAN
!                       MATRIX (>= NUMBER OF NON-ZERO DIAGONALS BELOW
!                       THE MAIN DIAGONAL).
!
!     MUJAC       UPPER BANDWITH OF JACOBIAN  MATRIX (>= NUMBER OF NON-
!                 ZERO DIAGONALS ABOVE THE MAIN DIAGONAL).
!                 NEED NOT BE DEFINED IF MLJAC=N.
!
!     WORK        ARRAY OF WORKING SPACE OF LENGTH "LWORK".
!                 SERVES AS WORKING SPACE FOR ALL VECTORS AND MATRICES.
!                 "LWORK" MUST BE AT LEAST
!                             N*(LJAC+LE1+8)+5
!                 WHERE
!                    LJAC=N              IF MLJAC=N (FULL JACOBIAN)
!                    LJAC=MLJAC+MUJAC+1  IF MLJAC<N (BANDED JAC.)
!                 AND
!                    LE1=N               IF MLJAC=N (FULL JACOBIAN)
!                    LE1=2*MLJAC+MUJAC+1 IF MLJAC<N (BANDED JAC.).
!
!                 IN THE USUAL CASE WHERE THE JACOBIAN IS FULL AND THE
!                 MASS-MATRIX IS THE INDENTITY (IMAS=0), THE MINIMUM
!                 STORAGE REQUIREMENT IS
!                             LWORK = 2*N*N+8*N+5.
!
!     LWORK       DECLARED LENGHT OF ARRAY "WORK".
!
! ----------------------------------------------------------------------
!
!     SOPHISTICATED SETTING OF PARAMETERS
!     -----------------------------------
!              SEVERAL PARAMETERS OF THE CODE ARE TUNED TO MAKE IT WORK
!              WELL. THEY MAY BE DEFINED BY SETTING WORK(1),..,WORK(5)
!              AS WELL AS IWORK(1),IWORK(2) DIFFERENT FROM ZERO.
!              FOR ZERO INPUT, THE CODE CHOOSES DEFAULT VALUES:
!
!    WORK(1)   UROUND, THE ROUNDING UNIT, DEFAULT 1.D-16.
!
!    WORK(2)   MAXIMAL STEP SIZE, DEFAULT XEND-X.
!
!    WORK(3), WORK(4)   PARAMETERS FOR STEP SIZE SELECTION
!              THE NEW STEP SIZE IS CHOSEN SUBJECT TO THE RESTRICTION
!                 WORK(3) <= HNEW/HOLD <= WORK(4)
!              DEFAULT VALUES: WORK(3)=0.2D0, WORK(4)=6.D0
!
!    WORK(5)   AVOID THE HUMP: AFTER TWO CONSECUTIVE STEP REJECTIONS
!              THE STEP SIZE IS MULTIPLIED BY WORK(5)
!              DEFAULT VALUES: WORK(5)=0.1D0
!
!-----------------------------------------------------------------------
!
!     OUTPUT PARAMETERS
!     -----------------
!     X           X-VALUE WHERE THE SOLUTION IS COMPUTED
!                 (AFTER SUCCESSFUL RETURN X=XEND)
!
!     Y(N)        SOLUTION AT X
!
!     H           PREDICTED STEP SIZE OF THE LAST ACCEPTED STEP
!
!     IDID        REPORTS ON SUCCESSFULNESS UPON RETURN:
!                   IDID=1  COMPUTATION SUCCESSFUL,
!                   IDID=-1 COMPUTATION UNSUCCESSFUL.
!
! ---------------------------------------------------------
! *** *** *** *** *** *** *** *** *** *** *** *** ***
!          DECLARATIONS
! *** *** *** *** *** *** *** *** *** *** *** *** ***
      INTEGER, INTENT(in) :: ROSM, N, ITOL, MLJAC, MUJAC, LWORK
      REAL(wp), DIMENSION(1), INTENT(in) :: ATOL, RTOL
      INTEGER, INTENT(inout) :: IDID
      INTEGER , DIMENSION(jpoce,3), INTENT(out) :: ISTAT
      REAL(wp), DIMENSION(jpoce,2), INTENT(out) :: RSTAT

      INTEGER :: NMAX, LDJAC, LDE, IEYNEW, IEDY1, IEDY, IEAK1
      INTEGER :: IEAK2, IEAK3, IEAK4, IEFX, IEJAC, IEE
      INTEGER :: ISTORE
      REAL(wp) :: UROUND, HMAX, FAC1, FAC2, FACREJ, XEND, X
      REAL(wp), DIMENSION(jpoce) :: H
      REAL(wp), DIMENSION(jpoce, N) :: Y
      REAL(wp), DIMENSION(LWORK) :: WORK
      LOGICAL ARRET
      EXTERNAL JAC
! --------------------------------------------------------------------
! --- COMMON STAT CAN BE USED FOR STATISTICS
! ---    NFCN      NUMBER OF FUNCTION EVALUATIONS (THOSE FOR NUMERICAL
!                  EVALUATION OF THE JACOBIAN ARE NOT COUNTED)
! ---    NJAC      NUMBER OF JACOBIAN EVALUATIONS (EITHER ANALYTICALLY
!                  OR NUMERICALLY)
! ---    NSTEP     NUMBER OF COMPUTED STEPS
! ---    NACCPT    NUMBER OF ACCEPTED STEPS
! ---    NREJCT    NUMBER OF REJECTED STEPS (AFTER AT LEAST ONE STEP
!                  HAS BEEN ACCEPTED)
! ---    NDEC      NUMBER OF LU-DECOMPOSITIONS (N-DIMENSIONAL MATRIX)
! ---    NSOL      NUMBER OF FORWARD-BACKWARD SUBSTITUTIONS
! --------------------------------------------------------------------
! *** *** *** *** *** *** ***
!    SETTING THE PARAMETERS
! *** *** *** *** *** *** ***

      IF ( ln_timing ) CALL timing_start('rosk')

      ALLOCATE (NFCN(jpoce), NJAC(jpoce), NSTEP(jpoce), NACCPT(jpoce), NREJCT(jpoce), NDEC(jpoce), NSOL(jpoce))

      NFCN=0
      NJAC=0
      NSTEP=0
      NACCPT=0
      NREJCT=0
      NDEC=0
      NSOL=0
      ARRET=.FALSE.
! -------- NMAX , THE MAXIMAL NUMBER OF STEPS -----
      NMAX = 100000
! -------- UROUND   SMALLEST NUMBER SATISFYING 1.D0+UROUND>1.D0
      IF(WORK(1) == 0.0)THEN
         UROUND = 1.E-16
      ELSE
         UROUND = WORK(1)
         IF(UROUND <= 1.E-14 .OR. UROUND >= 1.0)THEN
            WRITE(NUMSED,*)' COEFFICIENTS HAVE 16 DIGITS, UROUND=',WORK(1)
            ARRET=.TRUE.
         END IF
      END IF
! -------- MAXIMAL STEP SIZE
      IF(WORK(2) == 0.0)THEN
         HMAX = XEND-X
      ELSE
         HMAX = WORK(2)
      END IF
! -------  FAC1,FAC2     PARAMETERS FOR STEP SIZE SELECTION
      IF(WORK(3) == 0.0)THEN
         FAC1 = 5.0_wp
      ELSE
         FAC1 = 1.0/WORK(3)
      END IF
      IF(WORK(4) == 0.0)THEN
         FAC2 = 1.0_wp / 6.0_wp
      ELSE
         FAC2 = 1.0_wp / WORK(4)
      END IF
! -------  FACREJ    FOR THE HUMP
      IF(WORK(5) == 0.0)THEN
         FACREJ = 0.1_wp
      ELSE
         FACREJ = WORK(5)
      END IF
! *** *** *** *** *** *** *** *** *** *** *** *** ***
!         COMPUTATION OF ARRAY ENTRIES
! *** *** *** *** *** *** *** *** *** *** *** *** ***
! -------- COMPUTATION OF THE ROW-DIMENSIONS OF THE 2-ARRAYS ---
! -- JACOBIAN
      LDJAC=MLJAC+MUJAC+1
      LDE=2*MLJAC+MUJAC+1
! ------- PREPARE THE ENTRY-POINTS FOR THE ARRAYS IN WORK -----
      IEYNEW=6
      IEDY1=IEYNEW+N
      IEDY=IEDY1+N
      IEAK1=IEDY+N
      IEAK2=IEAK1+N
      IEAK3=IEAK2+N
      IEAK4=IEAK3+N
      IEFX =IEAK4+N
      IEJAC=IEFX +N
      IEE  =IEJAC+N*LDJAC
! ------ TOTAL STORAGE REQUIREMENT -----------
      ISTORE=IEE+N*LDE-1
      IF(ISTORE > LWORK)THEN
         WRITE(NUMSED,*)' INSUFFICIENT STORAGE FOR WORK, MIN. LWORK=',ISTORE
         ARRET=.TRUE.
      END IF
! ------ WHEN A FAIL HAS OCCURED, WE RETURN WITH IDID=-1
      IF (ARRET) THEN
         IDID=-1
         RETURN
      END IF

! -------- CALL TO CORE INTEGRATOR ------------
      IF (ROSM == 4) THEN
         CALL RO4COR(N,X,Y,XEND,HMAX,H,RTOL,ATOL,ITOL,JAC,        &
            MLJAC,MUJAC,IDID,                 &
            NMAX,UROUND,FAC1,FAC2,FACREJ,     &
            LDJAC,LDE,RSTAT )
      ELSE IF (ROSM == 3) THEN
         CALL RO3COR(N,X,Y,XEND,HMAX,H,RTOL,ATOL,ITOL,JAC,        &
            MLJAC,MUJAC,IDID,                 &
            NMAX,UROUND,FAC1,FAC2,FACREJ,     &
            LDJAC,LDE,RSTAT )
      ENDIF
! ----------- RETURN -----------

      ISTAT(:,1) = NFCN(:)
      ISTAT(:,2) = NJAC(:)
      ISTAT(:,3) = NSTEP(:)

      DEALLOCATE (NFCN, NJAC, NSTEP, NACCPT, NREJCT, NDEC, NSOL )

      IF ( ln_timing ) CALL timing_stop('rosk')

      RETURN

      END SUBROUTINE rosk

      SUBROUTINE RO3COR(N,X,Y,XEND,HMAX,H,RTOL,ATOL,ITOL,JAC,         &
        MLJAC,MUJAC,IDID, NMAX,UROUND,FAC1,FAC2,FACREJ,       &
        LFJAC,LE,RSTAT)
! ----------------------------------------------------------
!     CORE INTEGRATOR FOR ROS4
!     PARAMETERS SAME AS IN ROS4 WITH WORKSPACE ADDED
! ----------------------------------------------------------
! ----------------------------------------------------------
!         DECLARATIONS
! ----------------------------------------------------------
      IMPLICIT REAL(wp) (A-H,O-Z)
      IMPLICIT INTEGER (I-N)

      REAL(wp) :: ATOL(1),RTOL(1)
      REAL(wp), DIMENSION(jpoce,N) :: Y, YNEW, DY1, DY, AK1, AK2, AK3, AK4, FX
      REAL(wp), DIMENSION(jpoce,LFJAC,N) :: FJAC
      REAL(wp), DIMENSION(jpoce, LE, N)  :: E
      REAL(wp), DIMENSION(jpoce) :: H, HNEW, HMAXN, XI, FAC
      REAL(wp), DIMENSION(jpoce) :: HC21, HC31, HC32, HC41, HC42, HC43
      REAL(wp), DIMENSION(jpoce) :: XXOLD, HOPT, ERR
      INTEGER, DIMENSION(jpoce,N) :: IP
      LOGICAL, DIMENSION(jpoce) :: REJECT,RJECT2
      INTEGER, DIMENSION(jpoce) :: ACCMASK, ENDMASK, ERRMASK
      REAL(wp), DIMENSION(jpoce,2) :: RSTAT

      IF ( ln_timing ) CALL timing_start('ro3cor')

! ---- PREPARE BANDWIDTHS -----
       MLE=MLJAC
       MUE=MUJAC
       MBJAC=MLJAC+MUJAC+1
       MDIAG=MLE+MUE+1
! *** *** *** *** *** *** ***
!  INITIALISATIONS
! *** *** *** *** *** *** ***
      POSNEG=SIGN(1.0,XEND-X)
      CALL RODAS3 (A21,A31,A32,A41,A42,A43,C21,C31,C32,C41,C42,C43,  &
                B1,B2,B3,B4,E1,E2,E3,E4,GAMMA)

! --- INITIAL PREPARATIONS
      DO ji = 1, jpoce
         HMAXN(ji)=MIN(ABS(HMAX),ABS(XEND-X))
         H(ji)=MIN(MAX(1.E-10,ABS(H(ji))),HMAXN(ji))
         H(ji)=SIGN(H(ji),POSNEG)
         REJECT(ji)=.FALSE.
         XXOLD(ji)=X
         XI(ji) = X
      END DO
      IRTRN = 1
      ERRMASK(:) = 0
      ENDMASK(:) = 0
      ACCMASK(:) = ENDMASK(:)

      IF (IRTRN < 0) GOTO 79
! --- BASIC INTEGRATION STEP
   1  CONTINUE
      DO JI = 1, jpoce
         IF (NSTEP(ji) > NMAX .OR. XI(ji)+0.1*H(ji) == XI(ji) .OR. ABS(H(ji)) <= UROUND) ERRMASK(ji) = 1
         IF ((XI(ji)-XEND)*POSNEG+UROUND > 0.0) THEN
             H(ji)=HOPT(ji)
             ENDMASK(JI) = 1
         END IF
         IF ( ENDMASK(ji) == 0 ) THEN
            HOPT(JI)=H(JI)
            IF ((XI(ji)+H(ji)-XEND)*POSNEG > 0.0) H(ji)=XEND-XI(ji)
         ENDIF
      END DO

      ACCMASK(:) = ENDMASK(:)

      IF ( COUNT( ENDMASK(:) == 1 ) == jpoce ) THEN
         IF ( ln_timing ) CALL timing_stop('ro3cor')
         RETURN
      ENDIF
      IF ( COUNT( ERRMASK(:) == 1 ) > 0 ) GOTO 79

      CALL sed_func(N,Y,DY1,ACCMASK)

      NFCN(:)=NFCN(:)+1

! *** *** *** *** *** *** ***
!  COMPUTATION OF THE JACOBIAN
! *** *** *** *** *** *** ***
      NJAC(:)=NJAC(:)+1
      CALL JAC(N,Y,FJAC,LFJAC,ACCMASK)
   2  CONTINUE

! *** *** *** *** *** *** ***
!  COMPUTE THE STAGES
! *** *** *** *** *** *** ***
      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            NDEC(ji)=NDEC(ji)+1
            HC21(ji)=C21/H(ji)
            HC31(ji)=C31/H(ji)
            HC32(ji)=C32/H(ji)
            HC41(ji)=C41/H(ji)
            HC42(ji)=C42/H(ji)
            HC43(ji)=C43/H(ji)
            FAC(ji)=1.0/(H(ji)*GAMMA)
! --- THE MATRIX E (B=IDENTITY, JACOBIAN A BANDED MATRIX)
            DO 601 J=1,N
            I1=MAX(1,MUJAC+2-J)
            I2=MIN(MBJAC,N+MUJAC+1-J)
            DO 600 I=I1,I2
  600       E(ji,I+MLE,J)=-FJAC(ji,I,J)
  601       E(ji,MDIAG,J)=E(ji,MDIAG,J)+FAC(ji)
         ENDIF
      END DO
      CALL DECB(N,LE,E,MLE,MUE,IP,INFO,ACCMASK)

! --- THIS PART COMPUTES THE STAGES IN THE CASE WHERE
! ---   1) THE DIFFERENTIAL EQUATION IS IN EXPLICIT FORM
! ---   2) THE JACOBIAN OF THE PROBLEM IS A BANDED MATRIX
! ---   3) THE DIFFERENTIAL EQUATION IS AUTONOMOUS
      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            AK1(ji,1:N)=DY1(ji,1:N)
         ENDIF
      END DO
      CALL SOLB(N,LE,E,MLE,MUE,AK1,IP,ACCMASK)
      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            AK2(ji,1:N)=DY1(ji,1:N)+HC21(ji)*AK1(ji,1:N)
         ENDIF
      END DO
      CALL SOLB(N,LE,E,MLE,MUE,AK2,IP,ACCMASK)
      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            YNEW(ji,1:N)=Y(ji,1:N)+A31*AK1(ji,1:N)+A32*AK2(ji,1:N)
         ENDIF
      END DO
      CALL sed_func(N,YNEW,DY,ACCMASK)
      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            AK3(ji,1:N)=DY(ji,1:N)+HC31(ji)*AK1(ji,1:N)+HC32(ji)*AK2(ji,1:N)
         ENDIF
      END DO
      CALL SOLB(N,LE,E,MLE,MUE,AK3,IP,ACCMASK)
      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            YNEW(ji,1:N)=Y(ji,1:N)+A41*AK1(ji,1:N)+A42*AK2(ji,1:N)+A43*AK3(ji,1:N)
         ENDIF
      END DO
      CALL sed_func(N,YNEW,DY,ACCMASK)
      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            DO I = 1, N
               AK4(ji,I)=DY(ji,I)+HC41(ji)*AK1(ji,I)+HC42(ji)*AK2(ji,I)+HC43(ji)*AK3(ji,I)
            END DO
         ENDIF
      END DO
      CALL SOLB(N,LE,E,MLE,MUE,AK4,IP,ACCMASK)

      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            NSOL(ji) = NSOL(ji)+4
            NFCN(ji) = NFCN(ji)+2
! *** *** *** *** *** *** ***
!  ERROR ESTIMATION
! *** *** *** *** *** *** ***
            NSTEP(ji)=NSTEP(ji)+1
! ------------ NEW SOLUTION ---------------
            DO I = 1, N
               YNEW(ji,I)=Y(ji,I)+B1*AK1(ji,I)+B2*AK2(ji,I)+B3*AK3(ji,I)+B4*AK4(ji,I)
            END DO
! ------------ COMPUTE ERROR ESTIMATION ----------------
            ERR(JI) = 0.0_wp
            DO I = 1, N
               S = E1*AK1(ji,I)+E2*AK2(ji,I)+E3*AK3(ji,I)+E4*AK4(ji,I)
               IF (ITOL == 0) THEN
                  SK = ATOL(1)+RTOL(1)*MAX(ABS(Y(ji,I)),ABS(YNEW(ji,I)))
               ELSE
                  SK = ATOL(I)+RTOL(I)*MAX(ABS(Y(ji,I)),ABS(YNEW(ji,I)))
               END IF
               ERR(ji) = ERR(ji)+(S/SK)**2
            END DO
            ERR(ji) = SQRT(ERR(ji)/N)
! --- COMPUTATION OF HNEW
! --- WE REQUIRE .2<=HNEW/H<=6.
            FAC(ji)  = MAX(FAC2,MIN(FAC1,(ERR(ji))**.333D0/.9D0))
            HNEW(ji) = H(ji)/FAC(ji)
! *** *** *** *** *** *** ***
!  IS THE ERROR SMALL ENOUGH ?
! *** *** *** *** *** *** ***
            RJECT2(ji) = .TRUE.
            IF (ERR(ji) <= 1.0) THEN
! --- STEP IS ACCEPTED
               NACCPT(ji) = NACCPT(ji)+1
               Y(ji,1:N)  = YNEW(ji,1:N)
               XXOLD(ji)  = XI(ji)
               XI(ji) = XI(ji)+H(ji)
               RSTAT(ji,2) = H(ji)
               IF (IRTRN < 0) GOTO 79
               IF (ABS(HNEW(ji)) > HMAXN(ji)) HNEW(ji)=POSNEG*HMAXN(ji)
               IF (REJECT(ji)) HNEW(ji)=POSNEG*MIN(ABS(HNEW(ji)),ABS(H(ji)))
               REJECT(ji) = .FALSE.
               RJECT2(ji) = .FALSE.
               IF (NACCPT(ji) == 1) RSTAT(ji,1) = (H(ji)+HNEW(ji))/2.0
               H(ji) = HNEW(ji)
               ACCMASK(ji) = 1
            ELSE
! --- STEP IS REJECTED
               IF (RJECT2(ji)) HNEW(ji)   = H(ji)*FACREJ
               IF (REJECT(ji)) RJECT2(ji) = .TRUE.
               REJECT(ji) = .TRUE.
               H(ji)=HNEW(ji)
               IF (NACCPT(ji) >= 1) NREJCT(ji) = NREJCT(ji)+1
               ACCMASK(ji) = 0
            END IF
         ENDIF
      END DO
      IF (COUNT( ACCMASK(:) == 0 ) > 0 ) GOTO 2
      GOTO 1
! --- EXIT
 79   CONTINUE
 979  FORMAT(' EXIT OF ROS3 AT X=',D16.7,'   H=',D16.7)
      IDID=-1

      IF ( ln_timing ) CALL timing_stop('ro3cor')

      RETURN

      END SUBROUTINE RO3COR

      SUBROUTINE RO4COR(N,X,Y,XEND,HMAX,H,RTOL,ATOL,ITOL,JAC,         &
        MLJAC,MUJAC,IDID, NMAX,UROUND,FAC1,FAC2,FACREJ,       &
        LFJAC,LE,RSTAT)
! ----------------------------------------------------------
!     CORE INTEGRATOR FOR ROS4
!     PARAMETERS SAME AS IN ROS4 WITH WORKSPACE ADDED
! ----------------------------------------------------------
! ----------------------------------------------------------
!         DECLARATIONS
! ----------------------------------------------------------
      IMPLICIT REAL(wp) (A-H,O-Z)
      IMPLICIT INTEGER (I-N)

      REAL(wp) :: ATOL(1),RTOL(1)
      REAL(wp), DIMENSION(jpoce,N) :: Y, YNEW, DY1, DY, AK1, AK2, AK3, AK4, FX
      REAL(wp), DIMENSION(jpoce,N) :: AK5, AK6
      REAL(wp), DIMENSION(jpoce,LFJAC,N) :: FJAC
      REAL(wp), DIMENSION(jpoce, LE, N)  :: E
      REAL(wp), DIMENSION(jpoce) :: H, HNEW, HMAXN, XI, FAC
      REAL(wp), DIMENSION(jpoce) :: HC21, HC31, HC32, HC41, HC42, HC43
      REAL(wp), DIMENSION(jpoce) :: HC51, HC52, HC53, HC54, HC61, HC62
      REAL(wp), DIMENSION(jpoce) :: HC63, HC64, HC65
      REAL(wp), DIMENSION(jpoce) :: XXOLD, HOPT, ERR
      INTEGER, DIMENSION(jpoce,N) :: IP
      LOGICAL, DIMENSION(jpoce) :: REJECT,RJECT2
      INTEGER, DIMENSION(jpoce) :: ACCMASK, ENDMASK, ERRMASK
      REAL(wp), DIMENSION(jpoce,2) :: RSTAT
! ---- PREPARE BANDWIDTHS -----
       MLE   = MLJAC
       MUE   = MUJAC
       MBJAC = MLJAC+MUJAC+1
       MDIAG = MLE+MUE+1
! *** *** *** *** *** *** ***
!  INITIALISATIONS
! *** *** *** *** *** *** ***
      POSNEG = SIGN(1.0,XEND-X)
      CALL RODAS4(A21,A31,A32,A41,A42,A43,A51,A52,A53,A54,A61,A62,A63,  &
                A64,A65,C21,C31,C32,C41,C42,C43,C51,C52,C53,C54,C61,C62,C63,  &
                C64,C65,B1,B2,B3,B4,B5,B6,E1,E2,E3,E4,E5,E6,GAMMA)

! --- INITIAL PREPARATIONS
      DO ji = 1, jpoce
         HMAXN(ji) = MIN(ABS(HMAX),ABS(XEND-X))
         H(ji) = MIN(MAX(1.E-10,ABS(H(ji))),HMAXN(ji))
         H(ji) = SIGN(H(ji),POSNEG)
         REJECT(ji) = .FALSE.
         XXOLD(ji) = X
         XI(ji) = X
      END DO
      IRTRN = 1
      ERRMASK(:) = 0
      ENDMASK(:) = 0
      ACCMASK(:) = ENDMASK(:)

      IF (IRTRN < 0) GOTO 79
! --- BASIC INTEGRATION STEP
   1  CONTINUE
      DO JI = 1, jpoce
         IF (NSTEP(ji) > NMAX .OR. XI(ji)+0.1*H(ji) == XI(ji) .OR. ABS(H(ji)) <= UROUND) THEN
            ERRMASK(ji) = 1
         ENDIF
         IF ((XI(ji)-XEND)*POSNEG+UROUND > 0.0) THEN
             H(ji) = HOPT(ji)
             ENDMASK(JI) = 1
         END IF
         IF ( ENDMASK(ji) == 0 ) THEN
            HOPT(JI) = H(JI)
            IF ((XI(ji)+H(ji)-XEND)*POSNEG > 0.0) H(ji)=XEND-XI(ji)
         ENDIF
      END DO

      ACCMASK(:) = ENDMASK(:)

      IF ( COUNT( ENDMASK(:) == 1 ) == jpoce ) RETURN
      IF ( COUNT( ERRMASK(:) == 1 ) > 0 ) GOTO 79

      CALL sed_func(N,Y,DY1,ACCMASK)

      NFCN(:) = NFCN(:) + 1
! *** *** *** *** *** *** ***
!  COMPUTATION OF THE JACOBIAN
! *** *** *** *** *** *** ***
      NJAC(:) = NJAC(:)+1
      CALL JAC(N,Y,FJAC,LFJAC,jpoce,ACCMASK)
   2  CONTINUE
! *** *** *** *** *** *** ***
!  COMPUTE THE STAGES
! *** *** *** *** *** *** ***
      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            NDEC(ji) = NDEC(ji)+1
            HC21(ji) = C21/H(ji)
            HC31(ji) = C31/H(ji)
            HC32(ji) = C32/H(ji)
            HC41(ji) = C41/H(ji)
            HC42(ji) = C42/H(ji)
            HC43(ji) = C43/H(ji)
            HC51(ji) = C51/H(ji)
            HC52(ji) = C52/H(ji)
            HC53(ji) = C53/H(ji)
            HC54(ji) = C54/H(ji)
            HC61(ji) = C61/H(ji)
            HC62(ji) = C62/H(ji)
            HC63(ji) = C63/H(ji)
            HC64(ji) = C64/H(ji)
            HC65(ji) = C65/H(ji)

            FAC(ji) = 1.0/(H(ji)*GAMMA)
! --- THE MATRIX E (B=IDENTITY, JACOBIAN A BANDED MATRIX)
            DO 601 J=1,N
            I1=MAX0(1,MUJAC+2-J)
            I2=MIN0(MBJAC,N+MUJAC+1-J)
            DO 600 I=I1,I2
  600       E(ji,I+MLE,J)=-FJAC(ji,I,J)
  601       E(ji,MDIAG,J)=E(ji,MDIAG,J)+FAC(ji)
         ENDIF
      END DO
      CALL DECB(N,LE,E,MLE,MUE,IP,INFO,ACCMASK)

! --- THIS PART COMPUTES THE STAGES IN THE CASE WHERE
! ---   1) THE DIFFERENTIAL EQUATION IS IN EXPLICIT FORM
! ---   2) THE JACOBIAN OF THE PROBLEM IS A BANDED MATRIX
! ---   3) THE DIFFERENTIAL EQUATION IS AUTONOMOUS
      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            AK1(ji,1:N) = DY1(ji,1:N)
         ENDIF
      END DO
      CALL SOLB(N,LE,E,MLE,MUE,AK1,IP,ACCMASK)
      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            YNEW(ji,1:N)=Y(ji,1:N)+A21*AK1(ji,1:N)
         ENDIF
      END DO
      CALL sed_func(N,YNEW,DY,ACCMASK)
      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            AK2(ji,1:N)=DY(ji,1:N)+HC21(ji)*AK1(ji,1:N)
         ENDIF
      END DO
      CALL SOLB(N,LE,E,MLE,MUE,AK2,IP,ACCMASK)
      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            YNEW(ji,1:N)=Y(ji,1:N)+A31*AK1(ji,1:N)+A32*AK2(ji,1:N)
         ENDIF
      END DO
      CALL sed_func(N,YNEW,DY,ACCMASK)
      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            AK3(ji,1:N)=DY(ji,1:N)+HC31(ji)*AK1(ji,1:N)+HC32(ji)*AK2(ji,1:N)
         ENDIF
      END DO
      CALL SOLB(N,LE,E,MLE,MUE,AK3,IP,ACCMASK)
      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            DO I = 1, N
               YNEW(ji,I)=Y(ji,I)+A41*AK1(ji,I)+A42*AK2(ji,I)+A43*AK3(ji,I)
            END DO
         ENDIF
      END DO
      CALL sed_func(N,YNEW,DY,ACCMASK)
      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            DO I = 1, N
               AK4(ji,I)=DY(ji,I)+HC41(ji)*AK1(ji,I)+HC42(ji)*AK2(ji,I)+HC43(ji)*AK3(ji,I)
            END DO
         ENDIF
      END DO
      CALL SOLB(N,LE,E,MLE,MUE,AK4,IP,ACCMASK)
      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            DO I = 1, N
               YNEW(ji,I)=Y(ji,I)+A51*AK1(ji,I)+A52*AK2(ji,I)+A53*AK3(ji,I)+A54*AK4(ji,I)
            END DO
         ENDIF
      END DO
      CALL sed_func(N,YNEW,DY,ACCMASK)
      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            DO I = 1, N
               AK5(ji,I)=DY(ji,I)+HC51(ji)*AK1(ji,I)+HC52(ji)*AK2(ji,I)+HC53(ji)*AK3(ji,I)+HC54(ji)*AK4(ji,I)
            END DO
         ENDIF
      END DO
      CALL SOLB(N,LE,E,MLE,MUE,AK5,IP,ACCMASK)
      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            DO I = 1, N
               YNEW(ji,I)=Y(ji,I)+A61*AK1(ji,I)+A62*AK2(ji,I)+A63*AK3(ji,I)+A64*AK4(ji,I)+A65*AK5(ji,I)
            END DO
         ENDIF
      END DO
      CALL sed_func(N,YNEW,DY,ACCMASK)
      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            DO I = 1, N
               AK6(ji,I)=DY(ji,I)+HC61(ji)*AK1(ji,I)+HC62(ji)*AK2(ji,I)+HC63(ji)*AK3(ji,I)+HC64(ji)*AK4(ji,I)   &
               &         + HC65(ji)*AK5(ji,I)
            END DO
         ENDIF
      END DO
      CALL SOLB(N,LE,E,MLE,MUE,AK6,IP,ACCMASK)

      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            NSOL(ji) = NSOL(ji) + 6
            NFCN(ji) = NFCN(ji) + 5
! *** *** *** *** *** *** ***
!  ERROR ESTIMATION
! *** *** *** *** *** *** ***
            NSTEP(ji) = NSTEP(ji)+1
! ------------ NEW SOLUTION ---------------
            DO 240 I = 1, N
  240       YNEW(ji,I)=Y(ji,I)+B1*AK1(ji,I)+B2*AK2(ji,I)+B3*AK3(ji,I)+B4*AK4(ji,I)+B5*AK5(ji,I)+B6*AK6(ji,I)
! ------------ COMPUTE ERROR ESTIMATION ----------------
            ERR(JI) = 0.0_wp
            DO 300 I = 1, N
            S = E1*AK1(ji,I)+E2*AK2(ji,I)+E3*AK3(ji,I)+E4*AK4(ji,I)+E5*AK5(ji,I)+E6*AK6(ji,I)
            IF (ITOL == 0) THEN
               SK = ATOL(1)+RTOL(1)*MAX(ABS(Y(ji,I)),ABS(YNEW(ji,I)))
            ELSE
               SK = ATOL(I)+RTOL(I)*MAX(ABS(Y(ji,I)),ABS(YNEW(ji,I)))
            END IF
  300       ERR(ji) = ERR(ji)+(S/SK)**2
            ERR(ji) = SQRT(ERR(ji)/N)
! --- COMPUTATION OF HNEW
! --- WE REQUIRE .2<=HNEW/H<=6.
            FAC(ji)  = MAX(FAC2,MIN(FAC1,(ERR(ji))**0.25/0.9))
            HNEW(ji) = H(ji)/FAC(ji)
! *** *** *** *** *** *** ***
!  IS THE ERROR SMALL ENOUGH ?
! *** *** *** *** *** *** ***
            RJECT2(ji) = .TRUE.
            IF (ERR(ji) <= 1.0) THEN
! --- STEP IS ACCEPTED
               NACCPT(ji) = NACCPT(ji) + 1
               Y(ji,1:N)  = YNEW(ji,1:N)
               XXOLD(ji)  = XI(ji)
               XI(ji) = XI(ji)+H(ji)
               RSTAT(ji,2) = H(ji)
               IF (IRTRN < 0) GOTO 79
               IF (ABS(HNEW(ji)) > HMAXN(ji)) HNEW(ji)=POSNEG*HMAXN(ji)
               IF (REJECT(ji)) HNEW(ji)=POSNEG*MIN(ABS(HNEW(ji)),ABS(H(ji)))
               REJECT(ji) = .FALSE.
               RJECT2(ji) = .FALSE.
               IF (NACCPT(ji) == 1) RSTAT(ji,1) = (H(ji)+HNEW(ji))/2.0
               H(ji) = HNEW(ji)
               ACCMASK(ji) = 1
            ELSE
! --- STEP IS REJECTED
               IF (RJECT2(ji)) HNEW(ji)=H(ji)*FACREJ
               IF (REJECT(ji)) RJECT2(ji)=.TRUE.
               REJECT(ji) = .TRUE.
               H(ji) = HNEW(ji)
               IF (NACCPT(ji) >= 1) NREJCT(ji)=NREJCT(ji)+1
               ACCMASK(ji) = 0
            END IF
         ENDIF
      END DO
      IF (COUNT( ACCMASK(:) == 0 ) > 0 ) GOTO 2
      GOTO 1
! --- EXIT
 79   CONTINUE
 979  FORMAT(' EXIT OF ROS4 AT X=',D16.7,'   H=',D16.7)
      IDID=-1
      RETURN

      END SUBROUTINE RO4COR

      SUBROUTINE RODAS3 (A21,A31,A32,A41,A42,A43,C21,C31,C32,C41,C42,C43,  &
                B1,B2,B3,B4,E1,E2,E3,E4,GAMMA)

      REAL(wp), INTENT(out) :: A21, A31, A32, A41, A42, A43, C21, C31
      REAL(wp), INTENT(out) :: C32, C41, C42, C43, B1, B2, B3, B4, E1
      REAL(wp), INTENT(out) :: E2, E3, E4, GAMMA

         A21= 0.0
         A31= 2.0
         A32= 0.0
         A41= 2.0
         A42= 0.0
         A43= 1.0
         C21= 4.0
         C31= 1.0
         C32=-1.0
         C41= 1.0
         C42=-1.0
         C43=-8.0/3.0
         B1= 2.0
         B2= 0.0
         B3= 1.0
         B4= 1.0
         E1= 0.0
         E2= 0.0
         E3= 0.0
         E4= 1.0
         GAMMA= 0.5
      RETURN
      END SUBROUTINE RODAS3

      SUBROUTINE RODAS4(A21,A31,A32,A41,A42,A43,A51,A52,A53,A54,A61,A62,A63,  &
                A64,A65,C21,C31,C32,C41,C42,C43,C51,C52,C53,C54,C61,C62,C63,  &
                C64,C65,B1,B2,B3,B4,B5,B6,E1,E2,E3,E4,E5,E6,GAMMA)

      REAL(wp), INTENT(out) :: A21,A31,A32,A41,A42,A43,A51,A52,A53,A54,A61
      REAL(wp), INTENT(out) :: A62,A63,A64,A65,C21,C31,C32,C41,C42,C43,C51
      REAL(wp), INTENT(out) :: C52,C53,C54,C61,C62,C63,C64,C65,B1,B2,B3,B4,B5
      REAL(wp), INTENT(out) :: B6,E1,E2,E3,E4,E5,E6,GAMMA

      A21 = 0.1544000000000000E+01
      A31 = 0.9466785280815826
      A32 = 0.2557011698983284
      A41 = 0.3314825187068521E+01
      A42 = 0.2896124015972201E+01
      A43 = 0.9986419139977817
      A51 = 0.1221224509226641E+01
      A52 = 0.6019134481288629E+01
      A53 = 0.1253708332932087E+02
      A54 =-0.6878860361058950
      A61 = A51
      A62 = A52
      A63 = A53
      A64 = A54
      A65 = 1.0
      C21 =-0.5668800000000000E+01
      C31 =-0.2430093356833875E+01
      C32 =-0.2063599157091915
      C41 =-0.1073529058151375
      C42 =-0.9594562251023355E+01
      C43 =-0.2047028614809616E+02
      C51 = 0.7496443313967647E+01
      C52 =-0.1024680431464352E+02
      C53 =-0.3399990352819905E+02
      C54 = 0.1170890893206160E+02
      C61 = 0.8083246795921522E+01
      C62 =-0.7981132988064893E+01
      C63 =-0.3152159432874371E+02
      C64 = 0.1631930543123136E+02
      C65 =-0.6058818238834054E+01
      B1 = A51
      B2 = A52
      B3 = A53
      B4 = A54
      B5 = 1.0
      B6 = 1.0
      E1 = 0.0
      E2 = 0.0
      E3 = 0.0
      E4 = 0.0
      E5 = 0.0
      E6 = 1.0
      GAMMA= 0.25
      RETURN
      END SUBROUTINE RODAS4
!
      SUBROUTINE DECB (N, NDIM, A, ML, MU, IP, IER, ACCMASK)
      IMPLICIT INTEGER (I-N)
      REAL(wp), DIMENSION(jpoce,NDIM,N) :: A
      INTEGER, DIMENSION(jpoce, N) :: IP
      INTEGER, DIMENSION(jpoce) :: ACCMASK
      REAL(wp) :: T
      INTEGER :: JI
!-----------------------------------------------------------------------
!  MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION OF A BANDED
!  MATRIX WITH LOWER BANDWIDTH ML AND UPPER BANDWIDTH MU
!  INPUT..
!     N       ORDER OF THE ORIGINAL MATRIX A.
!     NDIM    DECLARED DIMENSION OF ARRAY  A.
!     A       CONTAINS THE MATRIX IN BAND STORAGE.   THE COLUMNS
!                OF THE MATRIX ARE STORED IN THE COLUMNS OF  A  AND
!                THE DIAGONALS OF THE MATRIX ARE STORED IN ROWS
!                ML+1 THROUGH 2*ML+MU+1 OF  A.
!     ML      LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
!     MU      UPPER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
!  OUTPUT..
!     A       AN UPPER TRIANGULAR MATRIX IN BAND STORAGE AND
!                THE MULTIPLIERS WHICH WERE USED TO OBTAIN IT.
!     IP      INDEX VECTOR OF PIVOT INDICES.
!     IP(N)   (-1)**(NUMBER OF INTERCHANGES) OR O .
!     IER     = 0 IF MATRIX A IS NONSINGULAR, OR  = K IF FOUND TO BE
!                SINGULAR AT STAGE K.
!  USE  SOLB  TO OBTAIN SOLUTION OF LINEAR SYSTEM.
!  DETERM(A) = IP(N)*A(MD,1)*A(MD,2)*...*A(MD,N)  WITH MD=ML+MU+1.
!  IF IP(N)=O, A IS SINGULAR, SOLB WILL DIVIDE BY ZERO.
!
!  REFERENCE..
!     THIS IS A MODIFICATION OF
!     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER,
!     C.A.C.M. 15 (1972), P. 274.
!-----------------------------------------------------------------------
    IF ( ln_timing ) CALL timing_start('decb')

    DO JI = 1, jpoce
      IF (ACCMASK(ji) == 0) THEN
      IER = 0
      IP(ji,N) = 1
      MD = ML + MU + 1
      MD1 = MD + 1
      JU = 0
      IF (ML == 0) GO TO 70
      IF (N == 1) GO TO 70
      IF (N < MU+2) GO TO 7
      DO 5 J = MU+2,N
      DO 5 I = 1,ML
  5   A(ji,I,J) = 0.0
  7   NM1 = N - 1
      DO 60 K = 1, NM1
        KP1 = K + 1
        M = MD
        MDL = MIN(ML,N-K) + MD
        DO 10 I = MD1, MDL
          IF (ABS(A(ji,I,K)) > ABS(A(ji,M,K))) M = I
 10     CONTINUE
        IP(ji,K) = M + K - MD
        T = A(ji,M,K)
        IF (M == MD) GO TO 20
        IP(ji,N) = -IP(ji,N)
        A(ji,M,K) = A(ji,MD,K)
        A(ji,MD,K) = T
 20     CONTINUE
        IF (T == 0.0) THEN
           IER = K
           IP(ji,N) = 0
        ENDIF
        T = 1.0/T
        DO 30 I = MD1, MDL
 30       A(ji,I,K) = -A(ji,I,K)*T
        JU = MIN(MAX(JU,MU+IP(ji,K)),N)
        MM = MD
        IF (JU < KP1) GO TO 55
        DO 50 J = KP1, JU
          M = M - 1
          MM = MM - 1
          T = A(ji,M,J)
          IF (M .EQ. MM) GO TO 35
          A(ji,M,J) = A(ji,MM,J)
          A(ji,MM,J) = T
 35       CONTINUE
          IF (T == 0.0) GO TO 45
          JK = J - K
          DO 40 I = MD1, MDL
            IJK = I - JK
 40         A(ji,IJK,J) = A(ji,IJK,J) + A(ji,I,K)*T
 45       CONTINUE
 50       CONTINUE
 55     CONTINUE
 60     CONTINUE
 70   K = N
      IF (A(ji,MD,N) == 0.0) THEN
         IER = K
         IP(ji,N) = 0
      ENDIF
      ENDIF
      END DO

      IF ( ln_timing ) CALL timing_stop('decb')

      RETURN
!----------------------- END OF SUBROUTINE DECB ------------------------
      END SUBROUTINE DECB

      SUBROUTINE SOLB (N, NDIM, A, ML, MU, B, IP, ACCMASK)
      IMPLICIT INTEGER (I-N)
      REAL(wp) :: T
      REAL(wp), DIMENSION(jpoce,NDIM,N) :: A
      REAL(wp), DIMENSION(jpoce,N) :: B
      INTEGER, DIMENSION(jpoce,N)  :: IP 
      INTEGER :: IER
      INTEGER, DIMENSION(jpoce) :: ACCMASK
!-----------------------------------------------------------------------
!  SOLUTION OF LINEAR SYSTEM, A*X = B .
!  INPUT..
!    N      ORDER OF MATRIX A.
!    NDIM   DECLARED DIMENSION OF ARRAY  A .
!    A      TRIANGULARIZED MATRIX OBTAINED FROM DECB.
!    ML     LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
!    MU     UPPER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
!    B      RIGHT HAND SIDE VECTOR.
!    IP     PIVOT VECTOR OBTAINED FROM DECB.
!  DO NOT USE IF DECB HAS SET IER  <> 0.
!  OUTPUT..
!    B      SOLUTION VECTOR, X .
!-----------------------------------------------------------------------

      IF ( ln_timing ) CALL timing_start('solb')

      DO JI = 1, jpoce
      IF (ACCMASK(ji) == 0) THEN
      MD = ML + MU + 1
      MD1 = MD + 1
      MDM = MD - 1
      NM1 = N - 1
      IF (ML == 0) GO TO 25
      IF (N == 1) GO TO 50
      DO 20 K = 1, NM1
        M = IP(ji,K)
        T = B(ji,M)
        B(ji,M) = B(ji,K)
        B(ji,K) = T
        MDL = MIN(ML,N-K) + MD
        DO 10 I = MD1, MDL
          IMD = I + K - MD
 10       B(ji,IMD) = B(ji,IMD) + A(ji,I,K)*T
 20     CONTINUE
 25   CONTINUE
      DO 40 KB = 1, NM1
        K = N + 1 - KB
        B(ji,K) = B(ji,K)/A(ji,MD,K)
        T = -B(ji,K)
        KMD = MD - K
        LM = MAX(1,KMD+1)
        DO 30 I = LM, MDM
          IMD = I - KMD
 30       B(ji,IMD) = B(ji,IMD) + A(ji,I,K)*T
 40     CONTINUE
 50   B(ji,1) = B(ji,1)/A(ji,MD,1)
      ENDIF
      END DO

      IF ( ln_timing ) CALL timing_stop('solb')

      RETURN
!----------------------- END OF SUBROUTINE SOLB ------------------------
      END SUBROUTINE SOLB
#endif
END MODULE trosk
