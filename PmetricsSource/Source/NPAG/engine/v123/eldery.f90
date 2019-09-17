MODULE ELDERY_MOD
    INTEGER, PARAMETER :: MAXNUMEQ = 7
CONTAINS

    FUNCTION IS_INPUT_VALID(REQMIN, N)
        IMPLICIT NONE
        REAL(kind = 8) :: REQMIN
        INTEGER :: N
        LOGICAL :: IS_INPUT_VALID

        IS_INPUT_VALID = (REQMIN > 0.0D0 .AND. N > 0 .AND. N <= 99)
        RETURN
    END FUNCTION IS_INPUT_VALID

    SUBROUTINE CONSTRUCT_INITIAL_SIMPLEX(START, STEP, XMIN, Y, P, N, NN, FUNC)
        IMPLICIT REAL(kind = 8)(A-H, O-Z)
        INTEGER :: I, N, NN
        REAL(kind = 8) :: P(30, 31), START(N), STEP(N), XMIN(N), Y(31)
        EXTERNAL FUNC

        DO I = 1, N
            P(I, NN) = START(I)
        END DO

        CALL FUNC(N, START, FN, NUMEQT, YO, C0, C1, C2, C3, &
                NDIM, MF, RTOL, ATOL, PCOPY, RSCOPY, BSCOPY, INTLIST, IPAR, ObsError, &
                RPAR)
        Y(NN) = FN
        ICOUNT = ICOUNT + 1

        IF(ITMAX==0) THEN
            DO I = 1, N
                XMIN(I) = START(I)
            END DO
            YNEWLO = FN
            RETURN
        END IF

        DO J = 1, N
            DCHK = START(J)
            START(J) = DCHK + STEP(J)
            DO I = 1, N
                P(I, J) = START(I)
            END DO
            CALL FUNC(N, START, FN, NUMEQT, YO, C0, C1, C2, C3, &
                    NDIM, MF, RTOL, ATOL, PCOPY, RSCOPY, BSCOPY, INTLIST, IPAR, ObsError, &
                    RPAR)
            Y(J) = FN
            ICOUNT = ICOUNT + 1
            START(J) = DCHK
        END DO
    END SUBROUTINE CONSTRUCT_INITIAL_SIMPLEX

    SUBROUTINE ELDERY(N, START, XMIN, YNEWLO, REQMIN, STEP, &
            ITMAX, FUNC, IPRINT, ICONV, NITER, ICOUNT, NUMEQT, YO, C0, C1, C2, C3, &
            NDIM, MF, RTOL, ATOL, PCOPY, RSCOPY, BSCOPY, INTLIST, IPAR, ObsError, &
            RPAR)

        !  ELDERY DIFFERS FROM ELDERX ONLY IN THE DIMENSION STATEMENT. ALL 5'S
        !  ARE CHANGED TO 25'S, AND ALL 6'S ARE CHANGED TO 26'S. THIS ALLOWS 25
        !  PARAMETERS INSTEAD OF JUST 5. As of itbig9x.f, we allow as many as
        !  30 parameters.

        !  ELDERX DIFFERS FROM ELDER (DESCRIBED BELOW) ONLY IN THAT N, THE
        !  DIMENSION OF START (THE NO. OF UNKNOWN PARAMETERS OVER WHICH THE
        !  MINIMIZATION IS DONE) IS PASSED TO THE SUBROUTINE FUNC IN THE CALLING
        !  STATEMENTS.
        !
        !  ELDER IS A PROGRAM TO MINIMIZE A FUNCTION USING THE NELDER-MEED
        !  ALGORITM.
        !    THE CODE WAS ADAPTED FROM A PROG. IN J. OF QUALITY TECHNOLOGY VOL.
        !    JAN. 1974. BY D.M. OLSSON.
        !  CALLING ARGUMENTS:
        !    N     -NUMBER OF UNKNOWN PARAMS. UP TO 99.
        !    START -A VECTOR WITH THE INITIAL QUESSES OF THE SOLUTION PARAMS.
        !    ITMAX -THE MAXIMUM NUMBER OF ITERATIONS.
        !             (KCOUNT IS THE MAX NUM OF FUNC CALLS.SET AT 1000000)
        !    STEP  -THE STEP SIZE VECTOR FOR DEFINING THE N ADDITIONAL
        !             VERTICIES.
        !    REQMIN-THE STOP TOLERANCE.

        !    XMIN   -THE SOLUTION VECTOR.
        !    YNEWLO-THE FUCTION VALUE AT XMIN.
        !    IPRINT-SWITCH WHICH DETERMINES IF INTERMEDIATE ITERATIONS
        !              ARE TO BE PRINTED. (0=NO,1=YES).
        !    ICONV -FLAG INDICATING WHETHER OR NOT CONVERGENCE HAS BEEN
        !             ACHEIVED. (0=NO,1=YES).
        !    NITER -THE NUMBER OF ITERATIONS PERFORMED.
        !    ICOUNT-THE NUMBER OF FUNCTION EVALUATIONS.
        !    FUNC  -THE NAME OF THE SUBROUTINE DEFINING THE FUNCTION.
        !             THIS SUBROUTINE MUST EVALUATE THE FUNCTION GIVEN A
        !             VALUE FOR THE PARAMETER VECTOR. THE ROUTINE IS OF
        !             THE FOLLOWING FORM:
        !               FUNC(P,FV), WHERE P IS THE PARAMETER VECTOR,
        !                             AND FV IS THE FUNCTION VALUE.
        !  A SUBROUTINE TO PRINT THE RESULTS OF ITERMEDIATE ITERATIONS
        !    MUST ALSO BE SUPPLIED. ITS NAME AND CALLING SEQUENCE ARE
        !    DEFINED AS FOLLOWS:
        !      PRNOUT(P,N,NITER,NFCALL,FV).
        !  OTHER PROGRAM VARIABLES OF INTEREST ARE;
        !    XSEC  -THE COORDINATES OF THE VETEX WITH THE 2ND SMALLEST FUNCTION
        !             VALUE.
        !    YSEC  - THE FUNCTION VALUE AT XSEC.
        !

        IMPLICIT REAL(kind = 8)(A-H, O-Z)

        REAL(kind = 8) START, STEP, XMIN, XSEC, P, PSTAR, P2STAR, PBAR, Y, YO
        INTEGER NUMEQT

        DIMENSION START(N), STEP(N), XMIN(N), XSEC(30), &
                P(30, 31), PSTAR(30), P2STAR(30), PBAR(30), Y(31), YO(594, NUMEQT), &
                C0(NUMEQT), C1(NUMEQT), C2(NUMEQT), C3(NUMEQT)

        ! wmy2017Sep27
        real(kind = 8), dimension(32) :: PCOPY
        real(kind = 8), dimension(5000, 34) :: RSCOPY
        real(kind = 8), dimension(5000, 7) :: BSCOPY
        ! wmy2017Sep30
        integer, dimension(128) :: INTLIST
        integer, dimension(257) :: IPAR
        double precision, dimension(594, MAXNUMEQ) :: ObsError
        double precision, dimension(257) :: RPAR
        ! wmy2017Oct09
        real(kind = 8), dimension(20) :: ATOL

        ! !$omp ThreadPrivate( IPAR, ObsError )

        EXTERNAL FUNC
        DATA RCOEFF/1.0D0/, ECOEFF/2.0D0/, CCOEFF/.5D0/
        KCOUNT = 1000000
        ICOUNT = 0
        NITER = 0
        ICONV = 0

        IF(.NOT. IS_INPUT_VALID(REQMIN, N)) RETURN
        !
        !  SET INITIAL CONSTANTS
        !
        DABIT = 2.04607D-35
        BIGNUM = 1.0D+38
        KONVGE = 5
        XN = FLOAT(N)
        DN = FLOAT(N)
        NN = N + 1

        CALL CONSTRUCT_INITIAL_SIMPLEX(START, STEP, XMIN, Y, P, N, NN, FUNC)

        !
        !    FIND THE HIGHEST AND LOWEST VALUES. YNEWLO (Y(IHI)) INDICATES THE
        !     VERTEX OF THE SIMPLEX TO BE REPLACED.
        !
        1000    YLO = Y(1)
        YNEWLO = YLO
        ILO = 1
        IHI = 1

        DO I = 2, NN
            IF(Y(I)<YLO) THEN
                YLO = Y(I)
                ILO = I
            END IF
            IF(Y(I)>YNEWLO) THEN
                YNEWLO = Y(I)
                IHI = I
            END IF
        end do

        IF(ICOUNT<=NN) YOLDLO = YLO

        IF(ICOUNT>NN) THEN
            IF(YLO<YOLDLO) THEN
                YOLDLO = YLO
                NITER = NITER + 1
                IF(NITER>=ITMAX) GO TO 900

                IF(IPRINT/=0) THEN
                    !       CALL PRNOUT(P(1,ILO),N,NITER,ICOUNT,YLO)
                END IF
            END IF
        END IF

        !
        !  PERFORM CONVERGENCE CHECKS ON FUNCTIONS.
        !
        DCHK = (YNEWLO + DABIT) / (YLO + DABIT) - 1.0D0
        IF(DABS(DCHK) <= REQMIN) THEN
            ICONV = 1
            GO TO 900
        END IF

        KONVGE = KONVGE - 1

        IF(KONVGE==0) THEN
            KONVGE = 5
            !
            !  CHECK CONVERGENCE OF COORDINATES ONLY EVERY 5 SIMPLEXES.
            !
            DO I = 1, N
                COORD1 = P(I, 1)
                COORD2 = COORD1
                DO J = 2, NN
                    IF(P(I, J) < COORD1) THEN
                        COORD1 = P(I, J)
                    END IF
                    IF(P(I, J) > COORD2) THEN
                        COORD2 = P(I, J)
                    END IF
                END DO
                DCHK = (COORD2 + DABIT) / (COORD1 + DABIT) - 1.0D0
                IF(DABS(DCHK)>REQMIN) THEN
                    IF(ICOUNT>=KCOUNT) GO TO 900
                END IF
            end do
            ICONV = 1
            GO TO 900
        END IF

        !
        !  CALCULATE PBAR, THE CENTRIOD OF THE SIMPLEX VERTICES EXCEPTING THAT
        !   WITH Y VALUE YNEWLO.
        !
        DO I = 1, N
            Z = 0.0D0
            DO J = 1, NN
                Z = Z + P(I, J)
            END DO
            Z = Z - P(I, IHI)
            PBAR(I) = Z / DN
        END DO

        !
        !  REFLECTION THROUGH THE CENTROID.
        !
        DO I = 1, N
            PSTAR(I) = (1.0D0 + RCOEFF) * PBAR(I) - RCOEFF * P(I, IHI)
        END DO

        CALL FUNC(N, PSTAR, FN, NUMEQT, YO, C0, C1, C2, C3, &
                NDIM, MF, RTOL, ATOL, PCOPY, RSCOPY, BSCOPY, INTLIST, IPAR, ObsError, &
                RPAR)

        YSTAR = FN
        ICOUNT = ICOUNT + 1
        IF(YSTAR<YLO) THEN
            IF(ICOUNT>=KCOUNT) GO TO 19
            !
            !  SUCESSFUL REFLECTION SO EXTENSION.
            !
            DO I = 1, N
                P2STAR(I) = ECOEFF * PSTAR(I) + (1.0D0 - ECOEFF) * PBAR(I)
            end do
            CALL FUNC(N, P2STAR, FN, NUMEQT, YO, C0, C1, C2, C3, &
                    NDIM, MF, RTOL, ATOL, PCOPY, RSCOPY, BSCOPY, INTLIST, IPAR, ObsError, &
                    RPAR)
            Y2STAR = FN
            ICOUNT = ICOUNT + 1
            !
            !  RETAIN EXTENSION OR CONTRACTION.
            !
            IF(Y2STAR>=YSTAR) GO TO 19
            10      DO I = 1, N
                P(I, IHI) = P2STAR(I)
            end do
            Y(IHI) = Y2STAR
            GO TO 1000
            !
            !  NO EXTENSION.
            !
        END IF

        L = 0

        DO I = 1, NN
            IF(Y(I)>YSTAR) L = L + 1
        END DO

        IF(L<=1) THEN
            IF(L/=0) THEN
                !
                !  CONTRACTION ON REFLECTION SIDE OF CENTROID.
                !
                DO I = 1, N
                    P(I, IHI) = PSTAR(I)
                end do
                Y(IHI) = YSTAR
                !
                !  CONTRACTION ON THE Y(IHI) SIDE OF THE CENTROID.
                !
            END IF

            IF(ICOUNT>=KCOUNT) GO TO 900
            DO I = 1, N
                P2STAR(I) = CCOEFF * P(I, IHI) + (1.0D0 - CCOEFF) * PBAR(I)
            end do
            CALL FUNC(N, P2STAR, FN, NUMEQT, YO, C0, C1, C2, C3, &
                    NDIM, MF, RTOL, ATOL, PCOPY, RSCOPY, BSCOPY, INTLIST, IPAR, ObsError, &
                    RPAR)
            Y2STAR = FN
            ICOUNT = ICOUNT + 1
            IF(Y2STAR<Y(IHI)) GO TO 10
            !
            !  CONTRACT THE WHOLE SIMPLEX
            !
            DO J = 1, NN
                DO I = 1, N
                    P(I, J) = (P(I, J) + P(I, ILO)) * 0.5D0
                    XMIN(I) = P(I, J)
                end do
                CALL FUNC(N, XMIN, FN, NUMEQT, YO, C0, C1, C2, C3, &
                        NDIM, MF, RTOL, ATOL, PCOPY, RSCOPY, BSCOPY, INTLIST, IPAR, ObsError, &
                        RPAR)
                Y(J) = FN
            end do
            ICOUNT = ICOUNT + NN
            IF(ICOUNT<KCOUNT) GO TO 1000
            GO TO 900
            !
            !  RETAIN REFLECTION.
            !

        END IF

        19      CONTINUE

        DO I = 1, N
            P(I, IHI) = PSTAR(I)
        end do
        Y(IHI) = YSTAR
        GO TO 1000
        !
        !  SELECT THE TWO BEST FUNCTION VALUES (YNEWLO AND YSEC) AND THEIR
        !    COORDINATES (XMIN AND XSEC)>
        !
        900     DO J = 1, NN
            DO I = 1, N
                XMIN(I) = P(I, J)
            end do
            CALL FUNC(N, XMIN, FN, NUMEQT, YO, C0, C1, C2, C3, &
                    NDIM, MF, RTOL, ATOL, PCOPY, RSCOPY, BSCOPY, INTLIST, IPAR, ObsError, &
                    RPAR)
            Y(J) = FN
        END DO

        ICOUNT = ICOUNT + NN
        YNEWLO = BIGNUM

        DO J = 1, NN
            IF(Y(J)<YNEWLO) THEN
                YNEWLO = Y(J)
                IBEST = J
            END IF
        END DO

        Y(IBEST) = BIGNUM
        YSEC = BIGNUM

        DO J = 1, NN
            IF(Y(J)>=YSEC) THEN
                YSEC = Y(J)
                ISEC = J
            END IF
        END DO

        DO I = 1, N
            XMIN(I) = P(I, IBEST)
            XSEC(I) = P(I, ISEC)
        END DO

        RETURN
    END

END MODULE ELDERY_MOD

PROGRAM MAIN
    WRITE (*, *) 'Hello World!!'
END PROGRAM MAIN
