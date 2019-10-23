C  This file contains source code for the BLAS routines that are used by BIGNPAG
C  These are separated out here since it may be more efficient to just compile
C  bignpag.f and link to an optimized math library containing the BLAS than to
C  compile bignpag.f and this file blasnpag.f together.
C  contents:
c       dgemm:    blas level 3
c       dgemv:    blas level 2
c       dsyrk:    blas level 3
c       dtrsm:    blas level 1
c       dcopy:    blas levle 1
c       dscal:    blas level 1
c       daxpy:    blas level 1
c       ddot:     blas level 1
c       idamax:   blas level 1
c       dswap:    blas level 1
c       dasum:    blas level 1
c       dnrm2:    blas level 1
      SUBROUTINE DGEMM ( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,
     $                   BETA, C, LDC )
*     .. Scalar Arguments ..
      CHARACTER*1        TRANSA, TRANSB
      INTEGER            M, N, K, LDA, LDB, LDC
      DOUBLE PRECISION   ALPHA, BETA
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * )
      CHARACTER*20       ERRFIL
*     ..
*
*  Purpose
*  =======
*
*  DGEMM  performs one of the matrix-matrix operations
*
*     C := alpha*op( A )*op( B ) + beta*C,
*
*  where  op( X ) is one of
*
*     op( X ) = X   or   op( X ) = X',
*
*  alpha and beta are scalars, and A, B and C are matrices, with op( A )
*  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n',  op( A ) = A.
*
*              TRANSA = 'T' or 't',  op( A ) = A'.
*
*              TRANSA = 'C' or 'c',  op( A ) = A'.
*
*           Unchanged on exit.
*
*  TRANSB - CHARACTER*1.
*           On entry, TRANSB specifies the form of op( B ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSB = 'N' or 'n',  op( B ) = B.
*
*              TRANSB = 'T' or 't',  op( B ) = B'.
*
*              TRANSB = 'C' or 'c',  op( B ) = B'.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry,  M  specifies  the number  of rows  of the  matrix
*           op( A )  and of the  matrix  C.  M  must  be at least  zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry,  N  specifies the number  of columns of the matrix
*           op( B ) and the number of columns of the matrix C. N must be
*           at least zero.
*           Unchanged on exit.
*
*  K      - INTEGER.
*           On entry,  K  specifies  the number of columns of the matrix
*           op( A ) and the number of rows of the matrix op( B ). K must
*           be at least  zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
*           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
*           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
*           part of the array  A  must contain the matrix  A,  otherwise
*           the leading  k by m  part of the array  A  must contain  the
*           matrix A.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
*           LDA must be at least  max( 1, m ), otherwise  LDA must be at
*           least  max( 1, k ).
*           Unchanged on exit.
*
*  B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
*           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
*           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
*           part of the array  B  must contain the matrix  B,  otherwise
*           the leading  n by k  part of the array  B  must contain  the
*           matrix B.
*           Unchanged on exit.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
*           LDB must be at least  max( 1, k ), otherwise  LDB must be at
*           least  max( 1, n ).
*           Unchanged on exit.
*
*  BETA   - DOUBLE PRECISION.
*           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
*           supplied as zero then C need not be set on input.
*           Unchanged on exit.
*
*  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
*           Before entry, the leading  m by n  part of the array  C must
*           contain the matrix  C,  except when  beta  is zero, in which
*           case C need not be set on entry.
*           On exit, the array  C  is overwritten by the  m by n  matrix
*           ( alpha*op( A )*op( B ) + beta*C ).
*
*  LDC    - INTEGER.
*           On entry, LDC specifies the first dimension of C as declared
*           in  the  calling  (sub)  program.   LDC  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     .. Local Scalars ..
      LOGICAL            NOTA, NOTB
      INTEGER            I, INFO, J, L, NCOLA, NROWA, NROWB
      DOUBLE PRECISION   TEMP
*     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Executable Statements ..
*
*     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
*     transposed and set  NROWA, NCOLA and  NROWB  as the number of rows
*     and  columns of  A  and the  number of  rows  of  B  respectively.
*
      NOTA  = LSAME( TRANSA, 'N' )
      NOTB  = LSAME( TRANSB, 'N' )
      IF( NOTA )THEN
         NROWA = M
         NCOLA = K
      ELSE
         NROWA = K
         NCOLA = M
      END IF
      IF( NOTB )THEN
         NROWB = K
      ELSE
         NROWB = N
      END IF
*
*     Test the input parameters.
*
      INFO = 0
      IF(      ( .NOT.NOTA                 ).AND.
     $         ( .NOT.LSAME( TRANSA, 'C' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'T' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.NOTB                 ).AND.
     $         ( .NOT.LSAME( TRANSB, 'C' ) ).AND.
     $         ( .NOT.LSAME( TRANSB, 'T' ) )      )THEN
         INFO = 2
      ELSE IF( M  .LT.0               )THEN
         INFO = 3
      ELSE IF( N  .LT.0               )THEN
         INFO = 4
      ELSE IF( K  .LT.0               )THEN
         INFO = 5
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 8
      ELSE IF( LDB.LT.MAX( 1, NROWB ) )THEN
         INFO = 10
      ELSE IF( LDC.LT.MAX( 1, M     ) )THEN
         INFO = 13
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DGEMM ', INFO, ERRFIL )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     And if  alpha.eq.zero.
*
      IF( ALPHA.EQ.ZERO )THEN
         IF( BETA.EQ.ZERO )THEN
            DO 20, J = 1, N
               DO 10, I = 1, M
                  C( I, J ) = ZERO
   10          CONTINUE
   20       CONTINUE
         ELSE
            DO 40, J = 1, N
               DO 30, I = 1, M
                  C( I, J ) = BETA*C( I, J )
   30          CONTINUE
   40       CONTINUE
         END IF
         RETURN
      END IF
*
*     Start the operations.
*
      IF( NOTB )THEN
         IF( NOTA )THEN
*
*           Form  C := alpha*A*B + beta*C.
*
            DO 90, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 50, I = 1, M
                     C( I, J ) = ZERO
   50             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 60, I = 1, M
                     C( I, J ) = BETA*C( I, J )
   60             CONTINUE
               END IF
               DO 80, L = 1, K
                  IF( B( L, J ).NE.ZERO )THEN
                     TEMP = ALPHA*B( L, J )
                     DO 70, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
   70                CONTINUE
                  END IF
   80          CONTINUE
   90       CONTINUE
         ELSE
*
*           Form  C := alpha*A'*B + beta*C
*
            DO 120, J = 1, N
               DO 110, I = 1, M
                  TEMP = ZERO
                  DO 100, L = 1, K
                     TEMP = TEMP + A( L, I )*B( L, J )
  100             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  110          CONTINUE
  120       CONTINUE
         END IF
      ELSE
         IF( NOTA )THEN
*
*           Form  C := alpha*A*B' + beta*C
*
            DO 170, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 130, I = 1, M
                     C( I, J ) = ZERO
  130             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 140, I = 1, M
                     C( I, J ) = BETA*C( I, J )
  140             CONTINUE
               END IF
               DO 160, L = 1, K
                  IF( B( J, L ).NE.ZERO )THEN
                     TEMP = ALPHA*B( J, L )
                     DO 150, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
  150                CONTINUE
                  END IF
  160          CONTINUE
  170       CONTINUE
         ELSE
*
*           Form  C := alpha*A'*B' + beta*C
*
            DO 200, J = 1, N
               DO 190, I = 1, M
                  TEMP = ZERO
                  DO 180, L = 1, K
                     TEMP = TEMP + A( L, I )*B( J, L )
  180             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  190          CONTINUE
  200       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of DGEMM .
*
      END
      SUBROUTINE DGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
*     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA, BETA
      INTEGER            INCX, INCY, LDA, M, N
      CHARACTER*1        TRANS
      CHARACTER*20       ERRFIL
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  DGEMV  performs one of the matrix-vector operations
*
*     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
*
*  where alpha and beta are scalars, x and y are vectors and A is an
*  m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANS  - CHARACTER*1.
*           On entry, TRANS specifies the operation to be performed as
*           follows:
*
*              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
*
*              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
*
*              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of the matrix A.
*           M must be at least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
*           Before entry, the leading m by n part of the array A must
*           contain the matrix of coefficients.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, m ).
*           Unchanged on exit.
*
*  X      - DOUBLE PRECISION array of DIMENSION at least
*           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
*           Before entry, the incremented array X must contain the
*           vector x.
*           Unchanged on exit.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  BETA   - DOUBLE PRECISION.
*           On entry, BETA specifies the scalar beta. When BETA is
*           supplied as zero then Y need not be set on input.
*           Unchanged on exit.
*
*  Y      - DOUBLE PRECISION array of DIMENSION at least
*           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
*           Before entry with BETA non-zero, the incremented array Y
*           must contain the vector y. On exit, Y is overwritten by the
*           updated vector y.
*
*  INCY   - INTEGER.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 1
      ELSE IF( M.LT.0 )THEN
         INFO = 2
      ELSE IF( N.LT.0 )THEN
         INFO = 3
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DGEMV ', INFO, ERRFIL )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
*     up the start points in  X  and  Y.
*
      IF( LSAME( TRANS, 'N' ) )THEN
         LENX = N
         LENY = M
      ELSE
         LENX = M
         LENY = N
      END IF
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( LENX - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( LENY - 1 )*INCY
      END IF
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through A.
*
*     First form  y := beta*y.
*
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, LENY
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, LENY
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, LENY
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, LENY
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )
     $   RETURN
      IF( LSAME( TRANS, 'N' ) )THEN
*
*        Form  y := alpha*A*x + y.
*
         JX = KX
         IF( INCY.EQ.1 )THEN
            DO 60, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  DO 50, I = 1, M
                     Y( I ) = Y( I ) + TEMP*A( I, J )
   50             CONTINUE
               END IF
               JX = JX + INCX
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  IY   = KY
                  DO 70, I = 1, M
                     Y( IY ) = Y( IY ) + TEMP*A( I, J )
                     IY      = IY      + INCY
   70             CONTINUE
               END IF
               JX = JX + INCX
   80       CONTINUE
         END IF
      ELSE
*
*        Form  y := alpha*A'*x + y.
*
         JY = KY
         IF( INCX.EQ.1 )THEN
            DO 100, J = 1, N
               TEMP = ZERO
               DO 90, I = 1, M
                  TEMP = TEMP + A( I, J )*X( I )
   90          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  100       CONTINUE
         ELSE
            DO 120, J = 1, N
               TEMP = ZERO
               IX   = KX
               DO 110, I = 1, M
                  TEMP = TEMP + A( I, J )*X( IX )
                  IX   = IX   + INCX
  110          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  120       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of DGEMV .
*
      END
      SUBROUTINE DSYRK ( UPLO, TRANS, N, K, ALPHA, A, LDA,
     $                   BETA, C, LDC )
*     .. Scalar Arguments ..
      CHARACTER*1        UPLO, TRANS
      CHARACTER*20       ERRFIL
      INTEGER            N, K, LDA, LDC
      DOUBLE PRECISION   ALPHA, BETA
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * )
*     ..
*
*  Purpose
*  =======
*
*  DSYRK  performs one of the symmetric rank k operations
*
*     C := alpha*A*A' + beta*C,
*
*  or
*
*     C := alpha*A'*A + beta*C,
*
*  where  alpha and beta  are scalars, C is an  n by n  symmetric matrix
*  and  A  is an  n by k  matrix in the first case and a  k by n  matrix
*  in the second case.
*
*  Parameters
*  ==========
*
*  UPLO   - CHARACTER*1.
*           On  entry,   UPLO  specifies  whether  the  upper  or  lower
*           triangular  part  of the  array  C  is to be  referenced  as
*           follows:
*
*              UPLO = 'U' or 'u'   Only the  upper triangular part of  C
*                                  is to be referenced.
*
*              UPLO = 'L' or 'l'   Only the  lower triangular part of  C
*                                  is to be referenced.
*
*           Unchanged on exit.
*
*  TRANS  - CHARACTER*1.
*           On entry,  TRANS  specifies the operation to be performed as
*           follows:
*
*              TRANS = 'N' or 'n'   C := alpha*A*A' + beta*C.
*
*              TRANS = 'T' or 't'   C := alpha*A'*A + beta*C.
*
*              TRANS = 'C' or 'c'   C := alpha*A'*A + beta*C.
*
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry,  N specifies the order of the matrix C.  N must be
*           at least zero.
*           Unchanged on exit.
*
*  K      - INTEGER.
*           On entry with  TRANS = 'N' or 'n',  K  specifies  the number
*           of  columns   of  the   matrix   A,   and  on   entry   with
*           TRANS = 'T' or 't' or 'C' or 'c',  K  specifies  the  number
*           of rows of the matrix  A.  K must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
*           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
*           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
*           part of the array  A  must contain the matrix  A,  otherwise
*           the leading  k by n  part of the array  A  must contain  the
*           matrix A.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
*           then  LDA must be at least  max( 1, n ), otherwise  LDA must
*           be at least  max( 1, k ).
*           Unchanged on exit.
*
*  BETA   - DOUBLE PRECISION.
*           On entry, BETA specifies the scalar beta.
*           Unchanged on exit.
*
*  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
*           Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
*           upper triangular part of the array C must contain the upper
*           triangular part  of the  symmetric matrix  and the strictly
*           lower triangular part of C is not referenced.  On exit, the
*           upper triangular part of the array  C is overwritten by the
*           upper triangular part of the updated matrix.
*           Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
*           lower triangular part of the array C must contain the lower
*           triangular part  of the  symmetric matrix  and the strictly
*           upper triangular part of C is not referenced.  On exit, the
*           lower triangular part of the array  C is overwritten by the
*           lower triangular part of the updated matrix.
*
*  LDC    - INTEGER.
*           On entry, LDC specifies the first dimension of C as declared
*           in  the  calling  (sub)  program.   LDC  must  be  at  least
*           max( 1, n ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            I, INFO, J, L, NROWA
      DOUBLE PRECISION   TEMP
*     .. Parameters ..
      DOUBLE PRECISION   ONE ,         ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      IF( LSAME( TRANS, 'N' ) )THEN
         NROWA = N
      ELSE
         NROWA = K
      END IF
      UPPER = LSAME( UPLO, 'U' )
*
      INFO = 0
      IF(      ( .NOT.UPPER               ).AND.
     $         ( .NOT.LSAME( UPLO , 'L' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.LSAME( TRANS, 'N' ) ).AND.
     $         ( .NOT.LSAME( TRANS, 'T' ) ).AND.
     $         ( .NOT.LSAME( TRANS, 'C' ) )      )THEN
         INFO = 2
      ELSE IF( N  .LT.0               )THEN
         INFO = 3
      ELSE IF( K  .LT.0               )THEN
         INFO = 4
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 7
      ELSE IF( LDC.LT.MAX( 1, N     ) )THEN
         INFO = 10
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DSYRK ', INFO, ERRFIL )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( N.EQ.0 ).OR.
     $    ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     And when  alpha.eq.zero.
*
      IF( ALPHA.EQ.ZERO )THEN
         IF( UPPER )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 20, J = 1, N
                  DO 10, I = 1, J
                     C( I, J ) = ZERO
   10             CONTINUE
   20          CONTINUE
            ELSE
               DO 40, J = 1, N
                  DO 30, I = 1, J
                     C( I, J ) = BETA*C( I, J )
   30             CONTINUE
   40          CONTINUE
            END IF
         ELSE
            IF( BETA.EQ.ZERO )THEN
               DO 60, J = 1, N
                  DO 50, I = J, N
                     C( I, J ) = ZERO
   50             CONTINUE
   60          CONTINUE
            ELSE
               DO 80, J = 1, N
                  DO 70, I = J, N
                     C( I, J ) = BETA*C( I, J )
   70             CONTINUE
   80          CONTINUE
            END IF
         END IF
         RETURN
      END IF
*
*     Start the operations.
*
      IF( LSAME( TRANS, 'N' ) )THEN
*
*        Form  C := alpha*A*A' + beta*C.
*
         IF( UPPER )THEN
            DO 130, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 90, I = 1, J
                     C( I, J ) = ZERO
   90             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 100, I = 1, J
                     C( I, J ) = BETA*C( I, J )
  100             CONTINUE
               END IF
               DO 120, L = 1, K
                  IF( A( J, L ).NE.ZERO )THEN
                     TEMP = ALPHA*A( J, L )
                     DO 110, I = 1, J
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
  110                CONTINUE
                  END IF
  120          CONTINUE
  130       CONTINUE
         ELSE
            DO 180, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 140, I = J, N
                     C( I, J ) = ZERO
  140             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 150, I = J, N
                     C( I, J ) = BETA*C( I, J )
  150             CONTINUE
               END IF
               DO 170, L = 1, K
                  IF( A( J, L ).NE.ZERO )THEN
                     TEMP      = ALPHA*A( J, L )
                     DO 160, I = J, N
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
  160                CONTINUE
                  END IF
  170          CONTINUE
  180       CONTINUE
         END IF
      ELSE
*
*        Form  C := alpha*A'*A + beta*C.
*
         IF( UPPER )THEN
            DO 210, J = 1, N
               DO 200, I = 1, J
                  TEMP = ZERO
                  DO 190, L = 1, K
                     TEMP = TEMP + A( L, I )*A( L, J )
  190             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  200          CONTINUE
  210       CONTINUE
         ELSE
            DO 240, J = 1, N
               DO 230, I = J, N
                  TEMP = ZERO
                  DO 220, L = 1, K
                     TEMP = TEMP + A( L, I )*A( L, J )
  220             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  230          CONTINUE
  240       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of DSYRK .
*
      END
      SUBROUTINE DTRSM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,
     $                   B, LDB )
*     .. Scalar Arguments ..
      CHARACTER*1        SIDE, UPLO, TRANSA, DIAG
      INTEGER            M, N, LDA, LDB
      DOUBLE PRECISION   ALPHA
      CHARACTER*20       ERRFIL
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  DTRSM  solves one of the matrix equations
*
*     op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
*
*  where alpha is a scalar, X and B are m by n matrices, A is a unit, or
*  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
*
*     op( A ) = A   or   op( A ) = A'.
*
*  The matrix X is overwritten on B.
*
*  Parameters
*  ==========
*
*  SIDE   - CHARACTER*1.
*           On entry, SIDE specifies whether op( A ) appears on the left
*           or right of X as follows:
*
*              SIDE = 'L' or 'l'   op( A )*X = alpha*B.
*
*              SIDE = 'R' or 'r'   X*op( A ) = alpha*B.
*
*           Unchanged on exit.
*
*  UPLO   - CHARACTER*1.
*           On entry, UPLO specifies whether the matrix A is an upper or
*           lower triangular matrix as follows:
*
*              UPLO = 'U' or 'u'   A is an upper triangular matrix.
*
*              UPLO = 'L' or 'l'   A is a lower triangular matrix.
*
*           Unchanged on exit.
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n'   op( A ) = A.
*
*              TRANSA = 'T' or 't'   op( A ) = A'.
*
*              TRANSA = 'C' or 'c'   op( A ) = A'.
*
*           Unchanged on exit.
*
*  DIAG   - CHARACTER*1.
*           On entry, DIAG specifies whether or not A is unit triangular
*           as follows:
*
*              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
*
*              DIAG = 'N' or 'n'   A is not assumed to be unit
*                                  triangular.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of B. M must be at
*           least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of B.  N must be
*           at least zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
*           zero then  A is not referenced and  B need not be set before
*           entry.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, k ), where k is m
*           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
*           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
*           upper triangular part of the array  A must contain the upper
*           triangular matrix  and the strictly lower triangular part of
*           A is not referenced.
*           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
*           lower triangular part of the array  A must contain the lower
*           triangular matrix  and the strictly upper triangular part of
*           A is not referenced.
*           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
*           A  are not referenced either,  but are assumed to be  unity.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
*           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
*           then LDA must be at least max( 1, n ).
*           Unchanged on exit.
*
*  B      - DOUBLE PRECISION array of DIMENSION ( LDB, n ).
*           Before entry,  the leading  m by n part of the array  B must
*           contain  the  right-hand  side  matrix  B,  and  on exit  is
*           overwritten by the solution matrix  X.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in  the  calling  (sub)  program.   LDB  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     .. Local Scalars ..
      LOGICAL            LSIDE, NOUNIT, UPPER
      INTEGER            I, INFO, J, K, NROWA
      DOUBLE PRECISION   TEMP
*     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      LSIDE  = LSAME( SIDE  , 'L' )
      IF( LSIDE )THEN
         NROWA = M
      ELSE
         NROWA = N
      END IF
      NOUNIT = LSAME( DIAG  , 'N' )
      UPPER  = LSAME( UPLO  , 'U' )
*
      INFO   = 0
      IF(      ( .NOT.LSIDE                ).AND.
     $         ( .NOT.LSAME( SIDE  , 'R' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.UPPER                ).AND.
     $         ( .NOT.LSAME( UPLO  , 'L' ) )      )THEN
         INFO = 2
      ELSE IF( ( .NOT.LSAME( TRANSA, 'N' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'T' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'C' ) )      )THEN
         INFO = 3
      ELSE IF( ( .NOT.LSAME( DIAG  , 'U' ) ).AND.
     $         ( .NOT.LSAME( DIAG  , 'N' ) )      )THEN
         INFO = 4
      ELSE IF( M  .LT.0               )THEN
         INFO = 5
      ELSE IF( N  .LT.0               )THEN
         INFO = 6
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 9
      ELSE IF( LDB.LT.MAX( 1, M     ) )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DTRSM ', INFO, ERRFIL )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( N.EQ.0 )
     $   RETURN
*
*     And when  alpha.eq.zero.
*
      IF( ALPHA.EQ.ZERO )THEN
         DO 20, J = 1, N
            DO 10, I = 1, M
               B( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
         RETURN
      END IF
*
*     Start the operations.
*
      IF( LSIDE )THEN
         IF( LSAME( TRANSA, 'N' ) )THEN
*
*           Form  B := alpha*inv( A )*B.
*
            IF( UPPER )THEN
               DO 60, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 30, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
   30                CONTINUE
                  END IF
                  DO 50, K = M, 1, -1
                     IF( B( K, J ).NE.ZERO )THEN
                        IF( NOUNIT )
     $                     B( K, J ) = B( K, J )/A( K, K )
                        DO 40, I = 1, K - 1
                           B( I, J ) = B( I, J ) - B( K, J )*A( I, K )
   40                   CONTINUE
                     END IF
   50             CONTINUE
   60          CONTINUE
            ELSE
               DO 100, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 70, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
   70                CONTINUE
                  END IF
                  DO 90 K = 1, M
                     IF( B( K, J ).NE.ZERO )THEN
                        IF( NOUNIT )
     $                     B( K, J ) = B( K, J )/A( K, K )
                        DO 80, I = K + 1, M
                           B( I, J ) = B( I, J ) - B( K, J )*A( I, K )
   80                   CONTINUE
                     END IF
   90             CONTINUE
  100          CONTINUE
            END IF
         ELSE
*
*           Form  B := alpha*inv( A' )*B.
*
            IF( UPPER )THEN
               DO 130, J = 1, N
                  DO 120, I = 1, M
                     TEMP = ALPHA*B( I, J )
                     DO 110, K = 1, I - 1
                        TEMP = TEMP - A( K, I )*B( K, J )
  110                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( I, I )
                     B( I, J ) = TEMP
  120             CONTINUE
  130          CONTINUE
            ELSE
               DO 160, J = 1, N
                  DO 150, I = M, 1, -1
                     TEMP = ALPHA*B( I, J )
                     DO 140, K = I + 1, M
                        TEMP = TEMP - A( K, I )*B( K, J )
  140                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( I, I )
                     B( I, J ) = TEMP
  150             CONTINUE
  160          CONTINUE
            END IF
         END IF
      ELSE
         IF( LSAME( TRANSA, 'N' ) )THEN
*
*           Form  B := alpha*B*inv( A ).
*
            IF( UPPER )THEN
               DO 210, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 170, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
  170                CONTINUE
                  END IF
                  DO 190, K = 1, J - 1
                     IF( A( K, J ).NE.ZERO )THEN
                        DO 180, I = 1, M
                           B( I, J ) = B( I, J ) - A( K, J )*B( I, K )
  180                   CONTINUE
                     END IF
  190             CONTINUE
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( J, J )
                     DO 200, I = 1, M
                        B( I, J ) = TEMP*B( I, J )
  200                CONTINUE
                  END IF
  210          CONTINUE
            ELSE
               DO 260, J = N, 1, -1
                  IF( ALPHA.NE.ONE )THEN
                     DO 220, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
  220                CONTINUE
                  END IF
                  DO 240, K = J + 1, N
                     IF( A( K, J ).NE.ZERO )THEN
                        DO 230, I = 1, M
                           B( I, J ) = B( I, J ) - A( K, J )*B( I, K )
  230                   CONTINUE
                     END IF
  240             CONTINUE
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( J, J )
                     DO 250, I = 1, M
                       B( I, J ) = TEMP*B( I, J )
  250                CONTINUE
                  END IF
  260          CONTINUE
            END IF
         ELSE
*
*           Form  B := alpha*B*inv( A' ).
*
            IF( UPPER )THEN
               DO 310, K = N, 1, -1
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( K, K )
                     DO 270, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  270                CONTINUE
                  END IF
                  DO 290, J = 1, K - 1
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = A( J, K )
                        DO 280, I = 1, M
                           B( I, J ) = B( I, J ) - TEMP*B( I, K )
  280                   CONTINUE
                     END IF
  290             CONTINUE
                  IF( ALPHA.NE.ONE )THEN
                     DO 300, I = 1, M
                        B( I, K ) = ALPHA*B( I, K )
  300                CONTINUE
                  END IF
  310          CONTINUE
            ELSE
               DO 360, K = 1, N
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( K, K )
                     DO 320, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  320                CONTINUE
                  END IF
                  DO 340, J = K + 1, N
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = A( J, K )
                        DO 330, I = 1, M
                           B( I, J ) = B( I, J ) - TEMP*B( I, K )
  330                   CONTINUE
                     END IF
  340             CONTINUE
                  IF( ALPHA.NE.ONE )THEN
                     DO 350, I = 1, M
                        B( I, K ) = ALPHA*B( I, K )
  350                CONTINUE
                  END IF
  360          CONTINUE
            END IF
         END IF
      END IF
*
      RETURN
*
*     End of DTRSM .
*
      END
      subroutine  dcopy(n,dx,incx,dy,incy)
c
c     copies a vector, x, to a vector, y.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dy(*)
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dy(iy) = dx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,7)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dy(i) = dx(i)
   30 continue
      if( n .lt. 7 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,7
        dy(i) = dx(i)
        dy(i + 1) = dx(i + 1)
        dy(i + 2) = dx(i + 2)
        dy(i + 3) = dx(i + 3)
        dy(i + 4) = dx(i + 4)
        dy(i + 5) = dx(i + 5)
        dy(i + 6) = dx(i + 6)
   50 continue
      return
      end
 
C-----------------------------------------------------------------------
 
      subroutine  dscal(n,da,dx,incx)
c
c     scales a vector by a constant.
c     uses unrolled loops for increment equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 3/93 to return if incx .le. 0.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision da,dx(*)
      integer i,incx,m,mp1,n,nincx
c
      if( n.le.0 .or. incx.le.0 )return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      nincx = n*incx
      do 10 i = 1,nincx,incx
        dx(i) = da*dx(i)
   10 continue
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dx(i) = da*dx(i)
   30 continue
      if( n .lt. 5 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dx(i) = da*dx(i)
        dx(i + 1) = da*dx(i + 1)
        dx(i + 2) = da*dx(i + 2)
        dx(i + 3) = da*dx(i + 3)
        dx(i + 4) = da*dx(i + 4)
   50 continue
      return
      end
 
C-----------------------------------------------------------------------
 
      subroutine daxpy(n,da,dx,incx,dy,incy)
c
c     constant times a vector plus a vector.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dy(*),da
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if (da .eq. 0.0d0) return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dy(iy) = dy(iy) + da*dx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,4)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dy(i) = dy(i) + da*dx(i)
   30 continue
      if( n .lt. 4 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,4
        dy(i) = dy(i) + da*dx(i)
        dy(i + 1) = dy(i + 1) + da*dx(i + 1)
        dy(i + 2) = dy(i + 2) + da*dx(i + 2)
        dy(i + 3) = dy(i + 3) + da*dx(i + 3)
   50 continue
      return
      end
 
C-----------------------------------------------------------------------
 
      double precision function ddot(n,dx,incx,dy,incy)
c
c     forms the dot product of two vectors.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dy(*),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
c
      ddot = 0.0d0
      dtemp = 0.0d0
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dtemp + dx(ix)*dy(iy)
        ix = ix + incx
        iy = iy + incy
   10 continue
      ddot = dtemp
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dtemp + dx(i)*dy(i)
   30 continue
      if( n .lt. 5 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dtemp = dtemp + dx(i)*dy(i) + dx(i + 1)*dy(i + 1) +
     *   dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + dx(i + 4)*dy(i + 4)
   50 continue
   60 ddot = dtemp
      return
      end
 
 
C-----------------------------------------------------------------------
 
      integer function idamax(n,dx,incx)
c
c     finds the index of element having max. absolute value.
c     jack dongarra, linpack, 3/11/78.
c     modified 3/93 to return if incx .le. 0.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dmax
      integer i,incx,ix,n
c
      idamax = 0
      if( n.lt.1 .or. incx.le.0 ) return
      idamax = 1
      if(n.eq.1)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      ix = 1
      dmax = dabs(dx(1))
      ix = ix + incx
      do 10 i = 2,n
         if(dabs(dx(ix)).le.dmax) go to 5
         idamax = i
         dmax = dabs(dx(ix))
    5    ix = ix + incx
   10 continue
      return
c
c        code for increment equal to 1
c
   20 dmax = dabs(dx(1))
      do 30 i = 2,n
         if(dabs(dx(i)).le.dmax) go to 30
         idamax = i
         dmax = dabs(dx(i))
   30 continue
      return
      end
 
C-----------------------------------------------------------------------
 
      subroutine  dswap (n,dx,incx,dy,incy)
c
c     interchanges two vectors.
c     uses unrolled loops for increments equal one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dy(*),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c       code for unequal increments or equal increments not equal
c         to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dx(ix)
        dx(ix) = dy(iy)
        dy(iy) = dtemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c       code for both increments equal to 1
c
c
c       clean-up loop
c
   20 m = mod(n,3)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
   30 continue
      if( n .lt. 3 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,3
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
        dtemp = dx(i + 1)
        dx(i + 1) = dy(i + 1)
        dy(i + 1) = dtemp
        dtemp = dx(i + 2)
        dx(i + 2) = dy(i + 2)
        dy(i + 2) = dtemp
   50 continue
      return
      end
      double precision function dasum(n,dx,incx)
c
c     takes the sum of the absolute values.
c     jack dongarra, linpack, 3/11/78.
c     modified 3/93 to return if incx .le. 0.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dtemp
      integer i,incx,m,mp1,n,nincx
c
      dasum = 0.0d0
      dtemp = 0.0d0
      if( n.le.0 .or. incx.le.0 )return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      nincx = n*incx
      do 10 i = 1,nincx,incx
        dtemp = dtemp + dabs(dx(i))
   10 continue
      dasum = dtemp
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,6)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dtemp + dabs(dx(i))
   30 continue
      if( n .lt. 6 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,6
        dtemp = dtemp + dabs(dx(i)) + dabs(dx(i + 1)) + dabs(dx(i + 2))
     *  + dabs(dx(i + 3)) + dabs(dx(i + 4)) + dabs(dx(i + 5))
   50 continue
   60 dasum = dtemp
      return
      end
      DOUBLE PRECISION FUNCTION DNRM2 ( N, X, INCX )
*     .. Scalar Arguments ..
      INTEGER                           INCX, N
*     .. Array Arguments ..
      DOUBLE PRECISION                  X( * )
*     ..
*
*  DNRM2 returns the euclidean norm of a vector via the function
*  name, so that
*
*     DNRM2 := sqrt( x'*x )
*
*
*
*  -- This version written on 25-October-1982.
*     Modified on 14-October-1993 to inline the call to DLASSQ.
*     Sven Hammarling, Nag Ltd.
*
*
*     .. Parameters ..
      DOUBLE PRECISION      ONE         , ZERO
      PARAMETER           ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     .. Local Scalars ..
      INTEGER               IX
      DOUBLE PRECISION      ABSXI, NORM, SCALE, SSQ
*     .. Intrinsic Functions ..
      INTRINSIC             ABS, SQRT
*     ..
*     .. Executable Statements ..
      IF( N.LT.1 .OR. INCX.LT.1 )THEN
         NORM  = ZERO
      ELSE IF( N.EQ.1 )THEN
         NORM  = ABS( X( 1 ) )
      ELSE
         SCALE = ZERO
         SSQ   = ONE
*        The following loop is equivalent to this call to the LAPACK
*        auxiliary routine:
*        CALL DLASSQ( N, X, INCX, SCALE, SSQ )
*
         DO 10, IX = 1, 1 + ( N - 1 )*INCX, INCX
            IF( X( IX ).NE.ZERO )THEN
               ABSXI = ABS( X( IX ) )
               IF( SCALE.LT.ABSXI )THEN
                  SSQ   = ONE   + SSQ*( SCALE/ABSXI )**2
                  SCALE = ABSXI
               ELSE
                  SSQ   = SSQ   +     ( ABSXI/SCALE )**2
               END IF
            END IF
   10    CONTINUE
         NORM  = SCALE * SQRT( SSQ )
      END IF
*
      DNRM2 = NORM
      RETURN
*
*     End of DNRM2.
*
      END
c  read23.f                                                10/11/15

c  read23 has the following change from read22:

c  This program reads through new quantities (NRANFIX, PARRANFIX(.),
c  and RANFIXEST(.)) and writes them to OUTFILER.

c  Note that because the output file will have new items, it will also
c  have a new code, which will be VERSION 1.8 - OCT 2015 in format 101.

c-----------------------------------------------------------------------

c  read22.f                                                3/11/15

c  read22 has the following change from read21:

c  All numbers written out in F or G format are now tested to see if
c  they are inside [-1.D-99, 1.D-99]. If so, they are changed to be
c  0. The reason is that otherwise they will be printed out without 
c  the accompanying D or E (e.g., as .934-106, rather than .934E-106).

c  Note that this module is linked with npageng30.f initially.

c-----------------------------------------------------------------------

c  read21.f                                                6/19/14

c  read21 has a bug fix to read20.

c  When  the changes in read20.f (from read19.f) in Subroutine
c  CONVERGE2 were made (i.e., the deleting of the reading of the AUC
c  values from file 25), the "GO TO 110" statement just before label
c  200 was accidentally deleted. This caused the program to move 
c  through the output file without reading the Bayesian log-liks, means,
c  and std. devs. And this caused these values to show up as 0's in
c  the NP_RFxxxx.TXT file.

c  This "GO TO 110" statement has now been put back in.

c  Ref: BAYESWRITEERROR.EXP.

c----------------------------------------------------------------------

c  read20.f                                                3/12/14

c  read20 has the following changes from read19:

c  1. AUC values are no longer read from file 25, and of course 
c  therefore no longer written to OUTFILER. So all the code in this
c  module related to AUCs is removed.

c  Because the AUCs are removed in the NP_RFxxxx.TXT file, the version
c  no. will change to VERSION 1.7 - MAR 2014 in format 101.

c  2. A new parameter statement is added in Subroutines READOUT,
C  FILREDT, and CONVERGE2 to establish MAXNUMEQ, so 
c  that the 3rd dimension in OBSBLOCK can be set as MAXNUMEQ+1, and
c  so all 6's which refer to the maximum no. of output equations, can
c  be changed to MAXNUMEQ.

c  3. This module is first linked with the main "engine" module,
c  npageng25.f.

c-----------------------------------------------------------------------

c  read19.f                                                4/14/13
 
c  read19 has the following changes to read18:

c  1. NDOSEV(.) is replaced in FORMAT 114 by NDORIG(.). NDOSEV(JSUB) 
c  gives the no. of dose events for subject JSUB after the steady state
c  doses have been considered, whereas NDORIG(JSUB) gives the no. of 
c  dose events where each steady state dose set is considered to be 
c  just one event.

c  Note that all references to NDOSEV(.) have been removed in this
c  module.

c  2. Subroutines GETMAXCYCNO and GETICYCSTART have been changed to 
c  allow a maximum of 999999 for their arguments, rather than 9999. This
c  is because, previously, if a run had gone say 9500 cycles, and then
c  that run was picked up and run another 9000 cycles to 18500, for
c  example, the 18500 wouldn't have been read properly since the max.
c  no. of characters was 4. Now, the max. no. of characters is 6, which
c  would be plenty even if a run had been run and picked up several
c  times with 9999 cycles each time.

c  3. In Subroutine GETMAXCYCNO, the following line caused a compiling
c  error when compiled with a version of Fortran which comes with R.
c  Somehow, the last character (the N in THEN) was not read. To prevent
c  this problem in the future, I manually retyped in the line. It now
c  starts in column 9 and ends in column 72.
c       IF(READLINE(I:I+30) .EQ. 'THE LAST CYCLE NO. WILL BE .LE.') THEN 

c  4. New information is now written to the NP_RFxxxx.TXT file. In 
c  addition to CORDEN(.,.), the final cycle joint density, now also
c  the Bayesian Posterior densities for each subject are written. These
c  are passed in COMMON/BAY/BAYPOS from Subroutine SUBRES in the main
c  module (currently npageng23.f). These values will be written


c  immediately after the # CORDEN section.

c  Because of the new information in the NP_RFxxxx.TXT file, the version
c  no. will change to VERSION 1.6 - APR 2013 in format 101.
 

c-----------------------------------------------------------------------

c  read18.f                                                9/27/12

c  read18 has the following changes to read17:

c  1. In Subroutine CONVERGE2, NOMAXTIM(.) is initialized to be all 0's
c  at the top of the routine. This avoids a potential garbage value
c  being set = a NOMAXTIM(I), for a subject I which has no AUC tables
c  (see CONVERGE2 code for details).

c  2. Subroutine SEPARATE will no longer be called since the
c  individual files which have the info in the combined output file will
c  now be left open from the npageng20.f run (updated from npageng19.f).
c  This also means that all READ(34, )'s in this module will be
c  changed to READ(23, )'s, and all READ(47, )'s in this module
c  will be changed to READ(29, )'s. 

c  Note that IVER = 42 will be hard-coded in the section of the code
c  where the combined output file used to be read. IVER is needed
c  later in the code.

c  Also, OUTFILE, the combined output file, is removed as a calling 
c  argument in this module since it is no longer needed (see above),
c  and all references to OUTFILE are removed.

c  3. In Subroutine GETIPATFF, the first calling argument is 
c  hardcoded to be 25, so the READ statement at label 4210 will now
c  be hardcoded to be from file 25.
  
c  3. New Format 911 tells the user that NP_RFxxxx.TXT is being
c  created by this routine.

c-----------------------------------------------------------------------

c  read17.f                                                7/01/12

c  read17.f has the following changes to read16.f:

c  1. Format 124 in Subroutine CONVERGE2 is changed (T44 is changed to
c  T45) since IOUTEQ is moved one column to the right in the output
c  file (in npageng18.f, "OUPUT" is corrected to be "OUTPUT" on the
c  line where IOUTEQ is written).

c  2. DOSEBLOCK(.,.,.) AND OBSBLOCK(.,.,.) are now passed to Subroutine
c  READOUT via COMMON/DOSEBLOCK from Subroutine NEWWORK1 in 
c  npageng18.f, rather than being read from file 27 by Subroutine 
c  CONVERGE2 of this module.

c  3. The version no. will now be VERSION 1.5 - JUL 2012 (format 101)
c  ... since the values in DOSEBLOCK will be different if there are
c  steady state doses (i.e., now the values will be those from the
c  original working copy file in npag102.inp, rather than the full 
c  converted working copy file (converted by Subroutine NEWWORK1 in
c  npageng18.f) - which means that there will be just one line of info
c  for each steady state set, rather than 100 lines.

c  4. The first dimension for DOSEBLOCK has been changed from 900 to
c  800 to match all the other arrays whose dimensions reflect the max.
c  no. of subjects.

c  5. There is a change in how the output file indicates why the
c  run stopped. Previously, there were only 3 possibilities:
c  ICONVERGE = 0 --> MAXCYC cycles were run and convergence was not
c                    achieved;
c              2 --> MAXCYC cycles were run and convergence was
c                    achieved on the final cycle.
c              1 --> the run converged prior to MAXCYC.

c  Now there will be an additional possibility: 
c              3 --> the run stopped because of a Hessian Matrix
c                    error (it is singular in Subroutine emint).


c  Note that read17.f is part of the npageng18.f "engine".

c-----------------------------------------------------------------------

c  read16.f                                                11/5/11

c  read16 differs from read15 only in that the dimensions related to the
c  no. of dose events are changed from 500 to 1000. This is needed as
c  read16 is part of the npageng16.f "engine", which accommodates 
c  steady state dose sets. 

c  Note that the other npageng16.f modules allow up to 5000 dose 
c  events, but this module only allows 1000 since the array, 
c  DOSEBLOCK is too big if the dimension is 5000.

c-----------------------------------------------------------------------

c  read15.f                                                7/29/11

c  read15 has the following changes from read14:

c  1. It is part of the new npageng15.f program. The version number for
c  the output file from the npageng15.f program is now required to be
c  42 (instead of 41), since NDRUG and AF(I),I=1,NDRUG will be written
c  to the output file, rather than just AF (see code below label 110).

c  Also, there is a change in how the output file indicates why the
c  run stopped. Previously, there were only two possibilities:
c  ICONVERGE = 0 if the run ran the max. no. of cycles (MAXCYC);
c              1 if the run converged before MAXCYC.

c  Now, there are 3 possibilities:
c  ICONVERGE = 0 --> MAXCYC cycles were run and convergence was not
c                    achieved;
c              2 --> MAXCYC cycles were run and convergence was
c                    achieved on the final cycle.
c              1 --> the run converged prior to MAXCYC.
c  i.e., note that the old ICONVERGE = 0 is divided into the
c  new ICONVERGE = 0 and ICONVERGE = 2 cases.

c  2. CALL PAUSE and STOP statements are added below format 5316; they
c  should have been there all along, though there should be no way
c  that that part of the code will be executed since an output file
c  should always exist before this module is called.

c  3. Format 101 now shows VERSION 1.4 - AUG 2011, updated from
c  the previous VERSION 1.3 - JUL 2011.

c  4. All reference to npemdriv.f is now changed to npagdriv.f.

c  5. Some unused code related to previous versions is now removed.



c-----------------------------------------------------------------------


c  read14.f                                                6/29/11

c  read14 has the following changes from read13:

c  1. It is part of the new npageng14.f program. The version number for
c  the output file from the npageng14.f program is now required to be
c  41 (instead of 40), because of the changed format (and some 
c  substance) of this file.

c  2. The following new values are read from the output file and written
c  to the NP_RFxxxx.TXT file: STARTING CYCLE NO., MAXIMUM ENDING CYCLE
c  NO., CONVERGENCE FLAG (1 --> CONVERGENCE ACHIEVED; 0 --> THE RUN
c  STOPPED AT THE MAXIMUM ENDING CYCLE (NOTE THAT CONVERGENCE IS NOT
c  TESTED FOR AT THE MAXIMUM ENDING CYCLE)), RTOL, APRIORI DENSITY 
c  FILENAME (OR 'UNIFORM' IF THE PRIOR FILE IS UNIFORM), ASSAY ERROR
c  MODEL (this was read previously but not passed back to Main from
c  Subroutine CONVERGE2), ACTIVE (SALT) FRACTION.

c  Note that new subroutine, GETMAXCYC, reads the maximum ending cycle
c  number.

c  3. The first block of formats (starting with Format 103) are changed
c  to have more leading blanks. This is done so that RTOL and the 
c  prior density info (see above), which are now written in this block,
c  will have plenty of space.

c  4. Some of the unused code is removed.

c-----------------------------------------------------------------------

c  read13.f                                                4/28/11

c  The only changes that read13 has from read12 are in the way the assay
c  coefficients are written to OUTFILER (see code regarding ASSAYC). Note
c  that format 6093 is no longer used since the values will be 
c  written in free format, and then new Subroutine CONDENSE3 will be used
c  since the lines can be larger than 80 columns.

c----------------------------------------------------------------------

c  read12.f                                                4/25/11

c  This revised read12.f has these changes from original:

c  1. FORMAT 101 now has VERSION 1.2 - APR 2011, rather than
c                        VERSION 1.1 - APR 2011.

c  2. FORMAT 123 in Subroutine FILREDT is changed to take out the 
c  reference to WT and CCr.

c  3. Code and comments in GETCOVR2 reflect that fact that 

c  WT and CCR are not automatically covariates.

c  4. After GETCOVR2 is called, the COVDESCR() are only written
c  to File 21 if NCOV .GE. 1. This is not a functional change
c  since if NCOV = 0, the Do Loop there would not be used, but
c  it shows the possibility that NCOV could now be 0. 

c  5. The I6 format has been changed to I10 in the 26 formats from
c  186 to 214. These formats establish the lines nos. for each new
c  section of data in OUTFILER.


c  6. The format for writing the assay coefficients into the last
c  section of the OUTFILER file is changed to make it more easily
c  readable by r.

c-----------------------------------------------------------------------

c  read12.f                                                4/12/11

c  read12 has the following changes from read11:

c  1. It will be linked with the new main module npageng13.f (updated 
c  from bigmlt12.f).

c  2. It will now write the assay coefficients into the rile. See the
c  new array ASSAYC, whose values are written next to the values in
c  OBSBLOCK which represent observations (not times). Also note that
c  NLPATOUT is replaced by NLPATOUTASSAY since this block will now have
c  assay c's along with output values. Similarly the phrase, PATIENT 
c  OUTPUT BLOCKS is replaced by PATIENT OUTPUT AND ASSAY COEFF. BLOCKS
c  several places in the program (in comments and in the rfile).

c  Note that FORMAT 6093 is used instead of * when writing the assay C's
c  to file 21 since, in gfortran, long lines don't get 'wrapped' --> 
c  they can get cut off when these lines are copied to File 22 using
c  the *80 Format of READLINE.

c  3. The name of the version is changed in FORMAT 101 to
c  VERSION 1.1 - APR 2011, since the rfile is changed to include 
c  assay coefficients now (see no. 2 above).

c-----------------------------------------------------------------------

c  read11.f - revised                                      1/28/11

c  The revised file is identical to the original, except format 101
c  is changed to write ' VERSION 1.1 - Jan 2011', rather than
c  the original ' VERSION 1.0 - OCT 2010'.

c-----------------------------------------------------------------------

c  read11.f                                                1/17/11

c  read11 has one change from read10. It also reads and stores AIC and
c  BIC from OUTFILE, and writes these values to OUTFILER.

c-----------------------------------------------------------------------

c  read10.f                                                   11/21/10

c  read10 is changed from read9 only in that PAUSE statements are
c  replaced by CALL PAUSE commands. Subroutine PAUSE is in the
c  bigmlt9.f module. This is because a PAUSE statement causes a warning
c  when the program is compiled and linked using gfortran (and it forces
c  the user to type "go" to continue).

c-----------------------------------------------------------------------

C  READ9.F - REVISED.                                         11/18/10

C  READ9.F REVISED HAS ONE SMALL CHANGE FROM THE ORIGINAL READ9.F. IN
C  THE CODE WHICH ESTABLISHES THE BAYESIAN MEANS (SUBMEAN()), THE KEY
C  PHRASE 'THE MEANS' CAN NOW BE IN ENTRIES 3:11 AS WELL AS 2:10. THE
C  REASON IS THAT THIS PHRASE WAS WRITTEN BY bigmlt8.f IN FREE FORMAT,
C  AND THE PROGRAM, WHEN COMPILED UNDER gfortran, ADVANCES THE PHRASE
C  ONE COLUMN ON ITS LINE.

C-----------------------------------------------------------------------

C  READ9.F                                                    11/10/10


C  READ9 IS THE SAME AS READ8, EXCEPT:

C  1. IT MINIMIZES THE SIZE OF RFILE.r (ACTUALLY THE NAME OF THE FILE
C  WILL NOW BE SUPPLIED IN THE ARGUMENT TO READOUT) BY USING SUBROUTINE
C  CONDENSE2 TO WRITE EACH LINE (THIS SUBROUTINE USES AS SMALL A FORMAT
C  FOR EACH LINE AS IS POSSIBLE).

C  2. PATHFILE, NOW CALLED OUTFILE, IS CHANGED FROM *73 TO *20 IN THIS
C  PROGRAM.

C  3. SUBROUTINE GETNUMSF IS RENAMED TO BE GETNUMSF2, AND SUBROUTINE
C  GETSUB IS RENAMED TO BE GETSUB2. THIS IS BECAUSE WHEN THIS MODULE IS
C  COMPILED WITH BIGMLT8.F, THERE WILL BE NO DUPLICATE SUBROUTINE NAMES.


C-----------------------------------------------------------------------

C  READ8.F                                                    11/8/10

C  READ8 IS AN UPGRADE FROM READ7. IT HAS THE FOLLOWING CHANGES FROM 
C  READ7:

C  1. IT CAN CORRECTLY READ OUTPUT FILES WHICH WERE MADE FROM A 
C  "CONTINUATION" RUN (I.E., A RUN WHICH STARTED WITH AN APRIOR 

C  DENSITY), WHEREAS READ7 CAN NOT HANDLE SUCH OUTPUT FILES. 

C  TO DO THIS, READ8 READS NOT JUST THE TOTAL NO. OF CYCLES IN THE 
C  DENSITY PART OF THE OUTPUT FILE, BUT ALSO THE STARTING CYCLE NO. 
C  NEAR THE TOP OF THE OUTPUT FILE. THEN IT ONLY READS CYCLE INFORMATION
C  FOR THOSE CYCLES WHICH ARE ACTUALLY IN THE OUTPUT FILE. NOTE THAT NEW
C  ROUTINE GETICYCSTART READS THE STARTING CYCLE NO. FROM THE OUTPUT 
C  FILE.

C  2. THE NAME, "BIGFILE" IS CHANGED TO "RFILE.r".

C  3. THE NAMES OF THE COVARIATES ARE ALSO NOW WRITTEN TO "RFILE.r". 
C  THIS REQUIRES, AMONG OTHER NEW CODE, SUBROUTINE GETCOVR2, WHICH IS
C  AN EDITED VERSION OF THE ROUTINE BY THE SAME NAME FROM NPBG15E1.FOR.

C-----------------------------------------------------------------------

C  READ7.F                                                   10/15/10

C  READ7 HAS THE FOLLOWING CHANGES TO READ6 IN TERMS OF THE INFORMATION
C  WRITTEN TO "BIGFILE":

C  1. THE FORMAT OF THE AUC INFO WRITTEN AT THE TOP OF BIGFILE IS 

C     CHANGED.
C  2. THE COMMENTS AND EXTRA BLANK LINES WRITTEN IN THE CYCLE AND 
C     BAYESIAN ADDITIONAL STATISTICS BLOCKS ARE REMOVED. NOW, AFTER
C     THE HEADER LINE, THE CYCLE ADDITIONAL STATISTICS BLOCK WILL 
C     HAVE AN UNINTERRUPTED SET OF NCYCLE x NVAR x 3 LINES. SIMILARLY,


C     THE BAYESIAN ADDITIONAL STATISTICS BLOCK WILL HAVE AN 
C     UNINTERRUPTED SET OF NSUB x NVAR x 3 LINES.

C-----------------------------------------------------------------------

C  READ6.F                                                   10/12/10

C  READ6 HAS THE SAME LOGIC AS READ5. THE DIFFERENCE IS THAT READ6 IS 
C  A SUBROUTINE AGAIN (AS WAS READ4).

C  THE CHANGES REQUIRED ARE:

C  1. SUBROUTINE READOUT(PATHFILE) IS PLACED BACK AT THE TOP OF THE 
C  CODE.

C  2. PATHFILE IS INPUT TO THIS ROUTINE AS A CALLING ARGUMENT, RATHER


C  THAN BE OBTAINED FROM THE USER BY THE MODULE. THERFORE, ALL CODE TO
C  GET THE FILENAME AND PATH FROM THE USER HAS BEEN COMMENTED OUT. 
C  ALSO, THE CALL TO FULLNAME BELOW HAS BEEN COMMENTED OUT. IN ADDITION,
C  FORMAT 5316 IS CHANGED, AS THE PROGRAM WILL SIMPLY STOP IF PATHFILE
C  DOES NOT EXIST.

C  3. THE STOP STATEMENT AT THE END OF THE MAIN MODULE IS CHANGED TO BE
C  A RETURN STATEMENT.

C  4. COMMON/TOBLK/NUMBER IS REMOVED (IT WASN'T USED), AS IS THE

C  DECLARATION OF NUMBER IN THE CHARACTER STATEMENT.

C  5. PATH, TMPFILE, AND OUTFIL ARE REMOVED FROM THE PROGRAM, AS THEY 
C  ARE NO LONGER NEEDED. SIMILARLY, SUBROUTINES GETPATH, FULLNAME,
C  AND SEEDIR ARE REMOVED.

C-----------------------------------------------------------------------

C  READ5.F                                                   10/5/10

C  READ5.F HAS THE FOLLOWING CHANGES FROM READ4.F:

C  INSTEAD OF CREATING TWO FILES, THIS PROGRAM WILL CREATE JUST ONE
C  FILE, BIGFILE. IN THIS FILE WILL BE ALMOST ALL THE INFO FROM THE
C  OUTPUT OF A BIG NPAG RUN, INCLUDING CYCLE INFO (FOR LOG-LIKS,
C  MEANS, ETC.). IN ADDITION, NEAR THE TOP OF THE FILE,
C  AFTER LOTS OF INTEGER VALUES HAVE BEEN WRITTEN, WILL BE A "TABLE OF
C  CONTENTS" SPECIFYING ON WHICH LINE NOS. OF THE FILE EACH ARRAY TO BE
C  WRITTEN WILL BEGIN. NOTE THAT TO KNOW THESE LINE NOS., THE INFO WILL


C  FIRST BE WRITTEN TO A TEMP FILE (SO THE LINES NOS. CAN BE RECORDED)
C  AND THEN THIS TEMP FILE WILL BE COPIED BACK INTO BIGFILE.

C  SUBROUTINE PRNLAST IS REMOVED FROM THIS PROGRAM. IT WAS NOT USED.
C  SUBROUTINE STACK IS REMOVED, ALONG WITH ALL CONDITIONAL CODE BASED
C  ON IVER .LE. 36, WHICH CANNOT OCCUR, SINCE IVER .GE. 40 IS REQUIRED.
C  FOR THE SAME REASON, SUBROUTINE GETNUMEQ IS REMOVED.


C  ALSO CODE ESTABLISHING IAF AND IRM IS REMOVED, AND THE CODE IS USED
C  AS IF THESE PARAMETERS WERE SET = 1. 


C  FILREDT HAS EXTRA ARGUMENTS (NDRUG,ND,NADD) WHICH MAIN NEEDS.

C  SUBROUTINE PATH IS NOW INCLUDED SO THE USER CAN ENTER AN OUTPUT
C  FILE FROM A DIRECTORY OTHER THAN THE WORKING ONE.


C  NOTE THAT READ5.F (AS WELL AS READ4.F) DO NOT REPACKAGE THE BIG 
C  4-DIMENSIONAL ARRAYS. THEY JUST WRITE THEM TO THE INDICATED FILES.


C  THIS PROGRAM WILL NOW BE A SELF-CONTAINED PROGRAM, RATHER THAN A 
C  CALLED ROUTINE BY A DRIVER PROGRAM. 

C  THE COV. MATRICES, BOTH CYCLE INFO AND BAYESIAN INFO, IS NOT WRITTEN
C  TO BIGFILE IN READ5.F.


C-----------------------------------------------------------------------

C  READ4.F                                                    7/31/10

C  READ4.F HAS THE FOLLOWING CHANGES FROM READ3.F:

C  SUBROUTINE READOUT NOW ONLY HAS ONE ARGUMENT, PATHFILE, WHICH 
C  CONTAINS THE OUTPUT FILE NAME. ALL THE OTHER VARIABLES/ARRAYS 
C  WHICH WERE IN THE ARGUMENT LIST ARE NOW WRITTEN TO TWO FILES,
C  SMALLFILE AND BIGFILE. 

C  SMALLFILE CONTAINS ALL THE INTEGERS FROM THE PREVIOUS ARGUMENT
C  LIST, ALONG WITH NOBS(.) AND NUMT(.), AND THE MAX VALUE OF EACH OF
C  THESE ARRAYS, WHICH ARE NEEDED TO DEFINE THE SIZES OF THE BIG 
C  ARRAYS. BIGFILE CONTAINS THE BIG ARRAYS.

C  IN THIS WAY, READ4 DOES THE "HEAVY LIFTING" OF WORKING WITH LARGE
C  ARRAYS, AND MICHAEL'S R PROGRAM (SIMULATED BY DRIVREAD5.FOR) WILL BE
C  ABLE TO READ THE INFO INTO RELATIVELY SMALL ARRAYS.

C-----------------------------------------------------------------------

C  READ3.F                                                  7/19/10

C  READ3.F IS A VARIATION TO READ2.F. THE CHANGES ARE NEEDED SINCE R
C  CANNOT HANDLE CHARACTER ARRAYS AND NUMERICAL ARRAYS LARGER THAN 
C  2 DIMENSIONS. THE CHANGES ARE AS FOLLOWS:

C  1. THE INFO IN PAR WILL BE STORED INTO PAR1 AND, IF NEEDED, PAR2
C  WHERE PAR1 AND PAR2 ARE CHARACTER VARAIBLES OF LENGTH 240. THE NAMES
C  IN PAR WILL BE CONCATENATED ONTO PAR1 (AND PAR2) WITH A SPACE BETWEEN
C  EACH NAME. I.E., THE FIRST 12 CHARACTERS OF PAR1 WILL BE PAR(1) 
C  FOLLOWED BY A SPACE; THE SECOND 12 CHARACTERS OF PAR1 WILL BE PAR(2)
C  FOLLOWED BY A SPACE, ETC. IF THERE ARE MORE THAN max_ODE_params RANDOM VARIABLES,
C  THE REST OF THE PAR INFO WILL BE STORED INTO PAR2 IN A SIMILAR
C  FASHION.

C  2. THE INFO IN PARFIX WILL BE STORED INTO PARFIX1 IN A MANNER SIMILAR
C  TO HOW PAR IS STORED TO PAR1 AND PAR2 (SEE 1. ABOVE). PARFIX1 WILL
C  ALSO BE A CHARACTER VARIABLE OF LENGTH 240.

C  3. THE INFO IN YPREDPOP, WHICH IS A 4-DIMENSIONAL ARRAY, WILL BE PUT


C  INTO YPREDPOP1, WHICH IS A 1-DIMENSIONAL VECTOR IN THE STANDARD WAY.
C  I.E., YPREDPOP(i,j,k,l) WILL BE STORED USING 4 DO LOOPS WHERE THE 
C  OUTERMOST WILL BE OVER i, AND THE INNERMOST WILL BE OVER l.

C  SIMILARLY, INFO IN YPREDPOPT, YPREDBAY, AND EXX WILL BE STORED INTO
C  1-DIMENSIONAL VECTORS, YPREDPOPT1, YPREDBAY1, AND EXX1, RESPECTIVELY.


C  NOTE THAT NOW, PAR, PARFIX, YPREDPOP, YPREDPOPT, YPREDBAY, AND EXX
C  WILL BE REPLACED IN THE SUBROUTINE ARGUMENT LIST BY, RESPECTIVELY,
C  PAR1 AND PAR2, PARFIX1, YPREDPOP1, YPREDPOP1, YPREDBAY1, AND EXX1.

C-----------------------------------------------------------------------


C  READ2.F                                                  7/16/10

C  READ2.F IS AN EXTENSION TO READOUT1.F. IN ADDITION TO OBTAINING THE 
C  VALUES THAT READOUT1.F DOES, IT ALSO OBTAINS AND STORES ALL THE OTHER
C  ARRAYS FROM THE DENSITY FILE, AS WELL AS NUMEQT, THE OBSERVED VALUE 
C  TIMES FOR ALL THE SUBJECTS (WHICH WILL BE STORED INTO NEW ARRAY 
C  TIMOBS(.,.)) AND THE NO. OF OBSERVED TIMES FOR EACH SUBJECT (STORED 
C  INTO NEW ARRAY NOBS(.)) - SEE BELOW.

C  IN ADDITION ERRCODE IS REPLACED BY IERRCODE, WHICH DOES NOT HAVE TO
C  BE DECLARED AN INTEGER.

C  NOTE THAT SUBROUTINE FILRED HAS BEEN RENAMED TO BE FILREDT SINCE IT
C  WILL NOW ALSO RETURN THE OBSERVED TIMES.

C-----------------------------------------------------------------------

C  READOUT1.F                                               7/14/10

C  READOUT1.F IS AN EXTENTSION TO READOUT.F. INSTEAD OF JUST OBTAINING 
C  NSUB, IT ALSO OBTAINS THE MATRIX, CORDEN, FROM THE OUTPUT FILE IN
C  PATHFILE.

C-----------------------------------------------------------------------

      SUBROUTINE READOUT( OUTFILER
     1 , DOSEBLOCK, OBSBLOCK, NDORIG
     2 , BAYPOS, NACTSUB )

        use npag_utils, only: verifyval, maxnumeq,
     1   max_pop_rand_varbs, max_pop_varbs, max_pop_params
     2   ,max_covs, max_input_dim

        IMPLICIT REAL*8(A-H,O-Z)

C      PARAMETER(MAXNUMEQ=7)

        DIMENSION YO(150,MAXNUMEQ),PYJGX(800,1500),
     1  VALFIX(max_pop_params),RANFIXEST(max_pop_varbs),
     2  YPREDPOP(800,MAXNUMEQ,150,3),
     3  YPREDBAY(800,MAXNUMEQ,150,3),
     4  CORDEN(1500,31),
     5  IPATVEC(800),YPREDPOPT(800,MAXNUMEQ,7201,3),
     6  TTPRED(800,7200),NUMT(800),TO(150),NOBS(800)

        DIMENSION XLOGLIK(9997),
     1   ACTPTS(9997),SCALNFO(9997),
     2   GAMLAM(9997),
     3   AGE(800),HEIGHT(800),SUBLOGLIK(800),NDD(800),
     4   AICBIC(9997,2),
     5   ASSAYC(800,MAXNUMEQ,4),AF(max_input_dim),
     6   XVERIFY(100)


        CHARACTER PAR(max_pop_rand_varbs)*11
        double precision, dimension(max_pop_rand_varbs,2) :: AB
        double precision, dimension(9997,max_pop_rand_varbs) :: XMEAN
        double precision, dimension(9997,max_pop_rand_varbs) :: STDEV
     1    ,PRCFVR
        double precision, dimension(800,max_pop_rand_varbs) :: SUBMEAN
     1    ,SUBSTD,SUBPERCOF
        double precision, dimension(800,3,max_pop_rand_varbs) :: EXX

        double precision, dimension(maxnumeq) :: C0,C1,C2,C3

C  SEE COMMENTS IN SUBROUTINE SUBRES OF npageng23.f, WHERE BAYPOS
C  (AND NACTSUB) ARE STORED. THE FIRST DIMENSION OF BAYPOS (AND THE
C  ONLY DIMENSION OF NACTSUB) IS SET = 100, RATHER THAN 800, BECAUSE
C  THE PROGRAM IS TOO BIG OTHERWISE.

C  NOTE THAT THE DIMENSIONS FOR MAXGRD, MAXDIM, MAXSUB, AND MAXOBDIM
C  ABOVE HAVE BEEN REPLACED BY, RESPECTIVELY, 1500, max_pop_rand_varbs, 800, AND
C  150. THIS COULD BE AVOIDED BY PROVIDING THESE VALUES IN THE ARGUMENT
C  LIST OF THIS ROUTINE. NOTE THAT MAXGRD IS HARDCODED TO BE 1500 BELOW 
C  AS IT IS USED IN THE PROGRAM.

        CHARACTER PARFIX(20)*11,READLINE*80,
     1   NAME(800)*53,CHARTNO(800)*53,SEX(800)*1,PARRANFIX(20)*11,
     2   COVDESCR(max_covs)*20,OUTFILER*20,READLINE2*1000,PRIFILE*20

        CHARACTER(LEN=20) :: OSName

C      COMMON/DOSEOBS/DOSEBLOCK,OBSBLOCK,NDORIG
      real*8, dimension(800,1000,35) :: DOSEBLOCK
      real*8, dimension(800,150,MAXNUMEQ+1) :: OBSBLOCK
      integer, dimension(800) :: NDORIG

C      COMMON/BAY/NACTSUB,BAYPOS
      real*8, dimension(100,1500,31) :: BAYPOS
      integer, dimension(100) :: NACTSUB

      integer ISUB,K,J

C  NOTE THAT BAYPOS STORES THE BAYESIAN POSTERIOR DENSITY FOR EACH
C  SUBJECT. IT IS CREATED IN SUBROUTINE SUBRES OF THE MAIN MODULE. AND
C  NACTSUB GIVES THE NO. OF GRID POINTS FOR THE BAYESIAN POSTERIOR OF
C  EACH SUBJECT.


      WRITE(*,911)
  911 FORMAT(//' NOW CREATING THE NP_RFxxxx.TXT FILE ...')


C  SINCE MAXGRD, MAXSUB, AND MAXOBDIM ARE USED BELOW, AND ARE NOT PASSED 
C  TO THIS ROUTINE (AS THEY ARE IN SUBROUTINE PREVRUN OF NPBG15E1.FOR), 
C  THEY WILL BE HARDCODED BELOW.


        MAXGRD = 1500
        MAXOBDIM = 150
        MAXSUB = 800

    1   FORMAT(A20)

C  FOR READ6.F, OUTFILE IS INPUT TO THIS ROUTINE AS A CALLING ARGUMENT.
C  THERFORE, ALL CODE TO GET THE FILENAME AND PATH FROM THE USER
C  HAS BEEN COMMENTED OUT. ALSO, THE CALL TO FULLNAME BELOW HAS BEEN
C  COMMENTED OUT. IN ADDITION, FORMAT 5316 IS CHANGED, AS THE PROGRAM
C  WILL SIMPLY STOP IF OUTFILE DOES NOT EXIST.

C  CALL GETPATH TO GET FROM THE USER THE PATH WHERE THE OUTPUT FILE IS
C  LOCATED. NOTE THAT PATH IS THE PATH WITH A TRAILING BACKSLASH, AND 
C  NOB IS THE NO. OF THE ENTRY WITH THE LAST NON-BLANK ENTRY.
 
C    	  CALL GETPATH(PATH,NOB)


C1170   WRITE(*,5321) 
C 5321   FORMAT(//' ENTER THE NAME OF THE OUTPUT FILE FROM A PREVIOUS'/
C     1' RUN.'// 
C     4' IT WILL BE OF THE FORM OUTxxxx, WHERE xxxx WAS THE JOB NUMBER'/
C     5' ASSIGNED TO THE RUN.'//)
C        WRITE(*,*)' ENTER -99 TO SEE ALL OR A PART OF YOUR DIRECTORY: ' 
C        READ(*,1) OUTFIL
C        IF(OUTFIL(1:3) .EQ. '-99') CALL SEEDIR(PATH,NOB,OUTFIL)

C  CALL FULLNAME WHICH CONVERTS THE FILENAME TO PATHFILE, THE COMPLETE 
C  NAME OF THE FILE, WHICH INCLUDES THE PATH (IF THE PATH IS NOT THE 
C  CURRENT DIRECTORY).


C    	   TMPFILE = ' '
C        TMPFILE = OUTFIL
C        CALL FULLNAME(PATH,TMPFILE,PATHFILE)

c  As of read18.f, all the individual files needed by this module are 
c  already open (they were left open by npageng20.f which calls this 
c  module), and so there is no need to call Subroutine SEPARATE to
c  parse the combined output file into these individual files. In fact,
c  all reference to the combined output file, OUTFILE, have now been
c  removed from this module.
  
c  Note that IVER is hardcoded to be 42 since that is what would have
c  been read in from the combined output file.

        IVER = 42


    2   FORMAT(A80)

        CALL GETNUM(NUMEQT)

   80   READ(25,2) READLINE

C  READ IN BOTH NSUBTOT AND NSUB AND THEN CALL ROUTINE GETIPATFF WHICH 
C  READS THIS PORTION OF FILE 25 TO OBTAIN IPATVEC. NOTE THE 1ST 
C  ARGUMENT TELLS GETIPATFF TO READ FILE 25.

C  NOTE THAT IF IERRR RETURNS AS -1, THERE IS A PROBLEM WITH THE
C  PATIENT NO. INFO ON THE FILE. IN THIS CASE, THE PROBLEM HAS ALREADY

C  BEEN WRITTEN TO THE SCREEN. SO STOP.

        CALL GETNSUB2(NSUBTOT)
        CALL GETNSUB(NSUB)
        CALL GETIPATFF(25,NSUBTOT,NSUB,MAXSUB,IPATVEC,IERRR)
        IF(IERRR .EQ. -1) STOP

        
 210    READ(25,2) READLINE
         IF(READLINE(2:17) .EQ. 'THE NO. OF DRUGS') THEN
          READ(25,*) NDRUG
          READ(25,*) 
          READ(25,*)
          READ(25,*)
          READ(25,*) (AF(I),I=1,NDRUG)
          GO TO 165
         ENDIF
        GO TO 210

  165	  CALL GETICYCSTART(ICYCSTART)

C  ABOVE CALL OBTAINS THE STARTING CYCLE NO. IN THE OUTPUT FILE.
C  THE CALL BELOW OBTAINS THE MAXIMUM ENDING CYCLE NO. IN THE OUTPUT
C  FILE.

        CALL GETMAXCYCNO(IMAXCYC)

 1650   READ(25,2) READLINE

        IF(READLINE(2:20) .EQ. 'THE TOLERANCE PARAM') THEN
         READ(25,*)
         READ(25,*) RTOL 
         GO TO 1660
        ENDIF

        GO TO 1650

1660   READ(25,2) READLINE

        IF(READLINE(14:34) .EQ. 'JOINT DENSITY IS FROM') THEN
         PRIFILE = READLINE(41:60)
         GO TO 1670
        ENDIF

        IF(READLINE(14:34) .EQ. 'JOINT DENSITY IS UNIF') THEN
         PRIFILE = 'UNIFORM'
         GO TO 1670
        ENDIF

        GO TO 1660

 1670   CONTINUE

C  NOW READ THROUGH THE OUTPUT FILE TO SEE WHY THE RUN STOPPED (I.E., IT
C  EITHER CONVERGED BEFORE MAXCYC CYCLES WERE RUN (ICONVERGE = 1);
C  CONVERGED AT MAXCYC (ICONVERGE = 2); RAN THE MAXCYC CYCLES WITHOUT
C  CONVERGING (ICONVERGE = 0); OR STOPPED BECAUSE OF A HESSIAN MATRIX
C  BEING SINGULAR (ICONVERGE = 3).

        READ(25,2) READLINE

        IF(READLINE(2:25) .EQ. 'THIS RUN STOPPED WITH IC') THEN
         READ(25,2) READLINE
         IF(READLINE(2:2) .EQ. '0') ICONVERGE = 0
         IF(READLINE(2:2) .EQ. '1') ICONVERGE = 1
         IF(READLINE(2:2) .EQ. '2') ICONVERGE = 2
         IF(READLINE(2:2) .EQ. '3') ICONVERGE = 3

         GO TO 1680
        ENDIF

        GO TO 1670

 1680   CONTINUE

C  READ THE DENSITY VALUES INTO CORDEN. ALSO READ OTHER VALUES.

C  THE 1ST LINE IN THE DENSITY PART OF THE FILE IS THE CODE WHICH
C  GIVES THE VERSION NO. OF THE DENSITY FILE. IF THE VERSION IS NOT 
C  CORRECT, TELL THE USER AND HAVE HIM TRY AGAIN.

        READ(23,*)

C  THE TEST FOR THE CODE BEING 'DENSITY APR_10' IS UNNECESSARY IN THIS
C  PROGRAM (AND HAS BEEN REMOVED) SINCE IVER .GE. 40.


	  READ(23,*) NDIM
        READ(23,*) INDPTS

C  ONLY USE INDPTS TO ESTABLISH NGRID IF IVER .LE. 36 SINCE
C  IF IVER .GE. 37, NGRID WAS READ IN FROM OUTPUT FILE ABOVE.
C  NGRID NOT USED IN THIS PROGRAM, SO COMMENT OUT FOLLOWING CODE.


C	IF(IVER .LE. 36) THEN
C	  IF(INDPTS .EQ. 1) NGRID=2129
C	  IF(INDPTS .EQ. 2) NGRID=5003
C	  IF(INDPTS .EQ. 3) NGRID=10007
C	  IF(INDPTS .EQ. 4) NGRID=20011
C	  IF(INDPTS .EQ. 5) NGRID=40009
C	  IF(INDPTS .EQ. 6) NGRID=80021
C	  IF(INDPTS .GT. 6) NGRID = 80021*(INDPTS - 100)
C	ENDIF

C  NOTE: STARTING WITH M2_11.FOR, INDPTS MAY BE .GT. 6. IN SUCH A CASE, 
C        THE BOUNDARY OF THE 1ST VARIABLE IS DIVIDED INTO (INDPTS-100)
C	 REGIONS, EACH OF WHICH GETS 80021 GRID POINTS.

	READ(23,*) NACTVE


C  NOTE THAT NACTVE MUST BE .LE. MAXGRD SINCE PYJGX IS DIMENSIONED 


C  (100,MAXGRD) AND MUST READ IN NACTVE GRID POINTS FOR EACH SUBJECT.
C  SEE BELOW.


	IF(NACTVE .GT. MAXGRD) THEN
	 WRITE(*,1718) NACTVE,MAXGRD,MAXGRD
 1718    FORMAT(//' THE NO. OF ACTIVE GRID POINTS IS ',I7,' WHICH IS'/
     1' MORE THAN THE MAXIMUM ALLOWED FOR THIS PROGRAM (',I7,'). RERUN'/
     2' THIS PROGRAM AFTER YOU HAVE REDUCED THE NO. OF GRID POINTS IN'/
     3' THE DENSITY FILE TO NO MORE THAN ',I7//)
	 CALL PAUSE
	 STOP
	ENDIF


	READ(23,*) NVAR
	READ(23,1717) (PAR(I),I=1,NVAR)
 1717 FORMAT(A11)
	READ(23,*) NOFIX
	READ(23,1717) (PARFIX(I),I=1,NOFIX)
      READ(23,*) NRANFIX
	READ(23,1717) (PARRANFIX(I),I=1,NRANFIX)


	DO I=1,NVAR
	READ(23,*) (AB(I,J),J=1,2)
	END DO

	READ(23,*) (VALFIX(I),I=1,NOFIX)
	READ(23,*) (RANFIXEST(I),I=1,NRANFIX)


	READ(23,*) 
	READ(23,*) ICYCTOT
	READ(23,*) 
C DORIG IS NOT NEEDED, AND SO IS NOT READ IN IN ABOVE LINE.


	DO I=1,NACTVE
	READ(23,*) (CORDEN(I,J),J=1,NVAR+1)
	END DO

C  ALSO READ IN:

C   PYJGX(J,I) = P(YJ|XI), I=1,NACTVE, J=1,NSUB.

C   NOBS(JSUB) = NO. OF OBSERVATIONS FOR SUBJECT JSUB, JSUB = 1,NSUB.

C   TIMOBS(JSUB,IOBS) = OBSERVATION TIME FOR SUBJECT JSUB, FOR 
C    OBSERVATION IOBS (FROM FILREDT), IOBS = 1,NOBS(JSUB).

C   NDRUG = THE NO. OF DRUGS FOR THE RUN (FROM FILREDT).

C   YPREDPOP(JSUB,IEQ,IOBS,ICEN) = THE PREDICTED VALUE FOR Y FOR SUBJECT
C     JSUB, FOR OUTPUT EQUATION IEQ, FOR OBSERVATION IOBS, FOR ICEN = 
C     1 (MEANS), 2 (MEDIANS), AND 3 (MODES), WHERE THE MEANS, MEDIANS, 
C     AND MODES ARE FROM THE FINAL CYCLE POPULATION DENSITY.

C   NUMT(J), J=1,NSUB, = THE NO. OF TOTAL OBSERVATION TIMES (OVER ALL
C    TIME RESETS) FOR SUBJECT J.

C   YPREDPOPT(JSUB,IEQ,J,ICEN) = THE PREDICTED VALUE FOR Y FOR SUBJECT
C     JSUB, FOR OUTPUT EQUATION IEQ, FOR THE JTH TIME IN TTPRED (SEE

C     BELOW, AND THE CODE IN THE BIG NPAG "engine"), FOR 
C     ICEN = 1 (MEANS), 2 (MEDIANS), AND 3 (MODES), WHERE THE MEANS, 
C     MEDIANS, AND MODES ARE FROM THE FINAL CYCLE POPULATION DENSITY.


C   TTPRED(JSUB,J) = THE JTH OBSERVATION TIME FOR SUBJECT JSUB, 

C    J=1,NUMT(JSUB).

C   YPREDBAY(JSUB,IEQ,IOBS,ICEN) = THE PREDICTED VALUE FOR Y FOR SUBJECT
C     JSUB, FOR OUTPUT EQUATION IEQ, FOR OBSERVATION IOBS, FOR ICEN = 
C     1 (MEANS), 2 (MEDIANS), AND 3 (MODES), WHERE THE MEANS, MEDIANS, 
C     AND MODES ARE FROM SUBJECT'S JSUB BAYESIAN POSTERIOR DENSITY 
C     (CALCULATED BY SUBROUTINE SUBRES - IN PROGRAM "BIG NPAG", 
C     TRANSFERRED FROM THIS PROGRAM).

C   EXX(JSUB,ICEN,J) = THE JTH MEAN (ICEN=1), MEDIAN (ICEN=2), AND 
C     MODE (ICEN=3), FOR SUBJECT JSUB, WHERE THE MEANS, MEDIANS, AND 
C     MODES ARE FROM SUBJECT'S JSUB BAYESIAN POSTERIOR DENSITY 
C     (CALCULATED BY SUBROUTINE SUBRES - IN PROGRAM "BIG NPAG",
C     TRANSFERRED FROM THIS PROGRAM).


	DO JSUB=1,NSUB 
	 DO I=1,NACTVE
	  READ(23,*) PYJGX(JSUB,I)
	 END DO
	END DO

	REWIND(27)
	DO JSUB=1,NSUB

	 CALL FILREDT(NOBSER,TO,YO,C0,C1,C2,C3,MAXOBDIM,NDRUG,ND,NADD)
       NOBS(JSUB) = NOBSER
C      NDOSEV(JSUB) = ND
C  AS OF read19.f, NDOSEV(.) IS NOT NEEDED. IT IS REPLACED IN FORMAT 114
C  BY NDORIG(.). SEE COMMENTS AT TOP OF read19.f.
    
C  TIMOBS NOT USED IN THIS PROGRAM, SO COMMENT OUT FOLLOWING LINES.

C       DO J = 1,NOBSER
C        TIMOBS(JSUB,J) = TO(J)
C       END DO

	 DO IEQ=1,NUMEQT
	  DO J=1,NOBSER
	   READ(23,*) (YPREDPOP(JSUB,IEQ,J,ICENTER),ICENTER=1,3)
	  END DO
	 END DO

	END DO


C  IF IVER .GE. 39, READ IN YPREDPOPT.

	IF(IVER .GE. 39) THEN

	 DO JSUB = 1,NSUB
	  READ(23,*) NUMT(JSUB)
	 END DO

	 DO JSUB=1,NSUB
	  DO IEQ=1,NUMEQT
	   DO J=1,NUMT(JSUB)
	    READ(23,*) (YPREDPOPT(JSUB,IEQ,J,ICENTER),ICENTER=1,3)
	   END DO

	  END DO
	 END DO



	 DO JSUB=1,NSUB
	  DO J=1,NUMT(JSUB)
	   READ(23,*) TTPRED(JSUB,J)
	  END DO
	 END DO


	ENDIF



	REWIND(27)
	DO JSUB=1,NSUB
	 CALL FILREDT(NOBSER,TO,YO,C0,C1,C2,C3,MAXOBDIM,NDRUG,ND,NADD)
	 DO IEQ=1,NUMEQT
	  DO J=1,NOBSER
	   READ(23,*) (YPREDBAY(JSUB,IEQ,J,ICENTER),ICENTER=1,3)
	  END DO
	 END DO
	END DO

	DO JSUB=1,NSUB
	 DO ICENTER=1,3
	  READ(23,*) (EXX(JSUB,ICENTER,J),J=1,NVAR)	 
       END DO
	END DO

	CLOSE(23)

C  READ IN AND STORE CYCLE INFO, BY CALLING SUBROUTINE
C  CONVERGE2, WHICH IS BASED ON SUBROUTINE CONVERGE OF NPBIG15E.FOR.

        CALL CONVERGE2(NCYCLE,XLOGLIK,XMEAN,STDEV,INDXSD,AICBIC, 
     1   PRCFVR,ACTPTS,SCALNFO,GAMLAM,AGE,HEIGHT,
     2   SUBMEAN,SUBLOGLIK,SUBSTD,SUBPERCOF,
     3   NAME,CHARTNO,SEX,NDD,NI,ASSAYC,IERRMOD)

C  AS OF READ5.F, NO REPACKAGING IS NEEDED, SO THE PAR1, PAR2, AND 
C  PARFIX1 LOGIC HAS BEEN REMOVED.


C  OPEN TEMP FILE, 21, AND WRITE ALL THE INFO INTO IT, RECORDING THE
C  STARTING LINE NOS. FOR EACH VARIABLE/ARRAY. THE COPY THE FILE BACK
C  INTO OUTFILER, AFTER WRITING TO THE TOP OF OUTFILER THE "TABLE OF
C  CONTENTS".

        OPEN(21)

C  NLINE IS THE RUNNING CURRENT LINE NO.

        NLINE = 0

        WRITE(21,101)
  101   FORMAT(' VERSION 1.8 - OCT 2015')
        NLINE = NLINE + 1
       
        WRITE(21,102)
  102   FORMAT(/' # Run information')
        NLINE = NLINE + 2

        WRITE(21,103) NSUB
  103   FORMAT(15X,I6,'   # NSUB')
        NLINE = NLINE + 1

        WRITE(21,104) NACTVE,NSUB
  104   FORMAT(15X,I6,'   # NACTVE FOR ALL ',I5,' SUBJECTS')
        NLINE = NLINE + 1

C  SEE COMMENTS IN SUBROUTINE SUBRES OF THE MAIN MODULE (npageng23.f
C  IS THE CURRENT VERSION). NACTSUB AND BAYPOS BELOW CAN ONLY STORE
C  UP TO 100 SUBJECTS WORTH OF DATA. 

        NNSUB = NSUB
        IF(NSUB .GT. 100) NNSUB = 100
        DO ISUB = 1,NNSUB
         WRITE(21,2011) NACTSUB(ISUB),ISUB
 2011    FORMAT(15X,I6,'   # NACTVE FOR BAYESIAN POSTERIOR OF SUBJECT ',
     1I5)
         NLINE = NLINE + 1
        END DO


        WRITE(21,106) NVAR
  106   FORMAT(15X,I6,'   # NVAR')
        NLINE = NLINE + 1

        WRITE(21,107) NOFIX
  107   FORMAT(15X,I6,'   # NOFIX')
        NLINE = NLINE + 1

        WRITE(21,307) NRANFIX
  307   FORMAT(15X,I6,'   # NRANFIX')
        NLINE = NLINE + 1

        WRITE(21,108) NDIM
  108   FORMAT(15X,I6,'   # NDIM')
        NLINE = NLINE + 1

        WRITE(21,109) INDPTS
  109   FORMAT(15X,I6,'   # INDPTS')
        NLINE = NLINE + 1

        WRITE(21,771) ICYCSTART
  771   FORMAT(15X,I6,'   # STARTING CYCLE NO.')
        NLINE = NLINE + 1

        WRITE(21,772) IMAXCYC
  772   FORMAT(15X,I6,'   # MAXIMUM ENDING CYCLE NO.')
        NLINE = NLINE + 1

        WRITE(21,111) ICYCTOT - ICYCSTART + 1
  111   FORMAT(15X,I6,'   # NO. OF CYCLES RUN')
        NLINE = NLINE + 1

        WRITE(21,773) ICONVERGE
  773   FORMAT(15X,I6,'   # CONVERGENCE FLAG ')
        NLINE = NLINE + 1

C  REPLACE WRITING OF RTOL WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL.

        XVERIFY(1) = RTOL 
        CALL VERIFYVAL(1,XVERIFY)

C       WRITE(21,774) RTOL
        WRITE(21,774) XVERIFY(1)

  774   FORMAT(2X,F19.17,'   # O.D.E. TOLERANCE ')
        NLINE = NLINE + 1

        WRITE(21,777) PRIFILE
  777   FORMAT(1X,A20,'   # PRIOR DENSITY ')
        NLINE = NLINE + 1

        WRITE(21,778) IERRMOD  
  778   FORMAT(15X,I6,'   # ASSAY ERROR MODEL ')
        NLINE = NLINE + 1

        WRITE(21,112) NUMEQT
  112   FORMAT(15X,I6,'   # NUMEQT')
        NLINE = NLINE + 1

        WRITE(21,113) NDRUG
  113   FORMAT(15X,I6,'   # NDRUG ')
        NLINE = NLINE + 1

C  REPLACE WRITING OF AF() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL.

        DO I = 1,NDRUG
         XVERIFY(1) = AF(I) 
         CALL VERIFYVAL(1,XVERIFY)
C        WRITE(21,1107) AF(I),I
         WRITE(21,1107) XVERIFY(I),I
         NLINE = NLINE + 1
 1107    FORMAT(2X,F19.17,'   # ACTIVE (SALT) FRACTION FOR DRUG ',I1)
        END DO


        DO JSUB = 1,NSUB
         WRITE(21,114) NDORIG(JSUB),JSUB
  114    FORMAT(15X,I6,'   # NO. OF DOSE EVENTS FOR SUBJ. ',I6)
         NLINE = NLINE + 1
        END DO

        WRITE(21,116) NADD
  116   FORMAT(15X,I6,'   # NO. OF ADDITIONAL COVARIATES')
        NLINE = NLINE + 1


        DO JSUB = 1,NSUB
         WRITE(21,117) NOBS(JSUB),JSUB
  117    FORMAT(15X,I6,'   # NO. OF OBS. VALUE TIMES FOR SUBJ. ',I6)
         NLINE = NLINE + 1
        END DO

        DO JSUB = 1,NSUB
         WRITE(21,118) NUMT(JSUB),JSUB
  118    FORMAT(15X,I6,'   # NO. OF PREDICTED TIMES FOR SUBJ. ',I6)
         NLINE = NLINE + 1
        END DO


C  PUT IN A LINE FOR EACH START LINE NO. TO BE WRITTEN TO OUTFILER,
C  JUST SO I CAN KEEP TRACK OF THE LINE NOS. I.E., FILE 21 IS JUST A 
C  TEMPFILE, NOT THE REAL OUTFILER.

        WRITE(21,121)
  121   FORMAT(/' #Block information with starting line numbers')
        NLINE = NLINE + 2

        WRITE(21,122) 
  122   FORMAT(8X,'   # START PAR')
        NLINE = NLINE + 1

        WRITE(21,123) 
  123   FORMAT(8X,'   # START PARFIX')
        NLINE = NLINE + 1

        WRITE(21,223) 
  223   FORMAT(8X,'   # START PARRANFIX')
        NLINE = NLINE + 1

        WRITE(21,124) 
  124   FORMAT(8X,'   # START AB')
        NLINE = NLINE + 1

        WRITE(21,126) 
  126   FORMAT(8X,'   # START VALFIX')
        NLINE = NLINE + 1

        WRITE(21,423) 
  423   FORMAT(8X,'   # START RANFIXEST')
        NLINE = NLINE + 1

        WRITE(21,226)
  226   FORMAT(8X,'   # START COVARIATE NAMES')
        NLINE = NLINE + 1

        WRITE(21,127) 
  127   FORMAT(8X,'   # START CORDEN')
        NLINE = NLINE + 1

        WRITE(21,2012)
 2012   FORMAT(8X,'   # START BAYESIAN POSTERIOR DENSITIES')
        NLINE = NLINE + 1

        WRITE(21,128) 
  128   FORMAT(8X,'   # START PYJGX')
        NLINE = NLINE + 1

        WRITE(21,129) 
  129   FORMAT(8X,'   # START YPREDPOP')
        NLINE = NLINE + 1

        WRITE(21,229) 
  229   FORMAT(8X,'   # START YPREDBAY')
        NLINE = NLINE + 1

        WRITE(21,131) 
  131   FORMAT(8X,'   # START TTPRED')
        NLINE = NLINE + 1

        WRITE(21,132) 
  132   FORMAT(8X,'   # START YPREDPOPT')
        NLINE = NLINE + 1

        WRITE(21,133) 
  133   FORMAT(8X,'   # START EXX')
        NLINE = NLINE + 1


        WRITE(21,134) 
  134   FORMAT(8X,'   # START CYCLE LOG-LIKELIHOODS')
        NLINE = NLINE + 1

        WRITE(21,1134) 
 1134   FORMAT(8X,'   # START CYCLE AIC AND BIC VALUES ')
        NLINE = NLINE + 1

        WRITE(21,136) 

  136   FORMAT(8X,'   # START CYCLE MEANS')
        NLINE = NLINE + 1

C??? DON'T STORE CYCLE COV. MATRICES YET. DECIDE IF WE NEED TO.

        WRITE(21,137) 
  137   FORMAT(8X,'   # START CYCLE STD. DEVS.')
        NLINE = NLINE + 1

        WRITE(21,177) 
  177   FORMAT(8X,'   # START CYCLE ADDITIONAL STATISTICS')
        NLINE = NLINE + 1

        WRITE(21,138) 
  138   FORMAT(8X,'   # START CYCLE GAMLAM VALUES')
        NLINE = NLINE + 1

        WRITE(21,139)
  139   FORMAT(8X,'   # START BAYESIAN LOG-LIKELIHOODS')
        NLINE = NLINE + 1

        WRITE(21,141) 
  141   FORMAT(8X,'   # START BAYESIAN MEANS')
        NLINE = NLINE + 1

C??? DON'T STORE BAYESIAN COV. MATRICES YET. DECIDE IF WE NEED TO.

        WRITE(21,143) 
  143   FORMAT(8X,'   # START BAYESIAN STD. DEVS.')
        NLINE = NLINE + 1

        WRITE(21,142) 
  142   FORMAT(8X,'   # START BAYESIAN ADDITIONAL STATISTICS')
        NLINE = NLINE + 1

        WRITE(21,146) 
  146   FORMAT(8X,'   # START PATIENT IDS')
        NLINE = NLINE + 1

        WRITE(21,147) 
  147   FORMAT(8X,'   # START PATIENT DOSE COV. BLOCKS')
        NLINE = NLINE + 1

        WRITE(21,148) 
  148   FORMAT(8X,'   # START PATIENT OUTPUT AND ASSAY COEFF. BLOCKS')
        NLINE = NLINE + 1

        WRITE(21,149)
  149   FORMAT(/' #BEGIN DATA HERE')
        NLINE = NLINE + 2


        WRITE(21,151) 
  151   FORMAT(/8X,'   # RANDOM PARAMETER NAMES')
        NLINE = NLINE + 2
        NLRANPAR = NLINE
C  NLRANPAR IS THE RANDOM PARAMETER NAME HEADER LINE NO.
     
        DO IVAR = 1,NVAR
         WRITE(21,1717) PAR(IVAR)
         NLINE = NLINE + 1
        END DO


        WRITE(21,153) 
  153   FORMAT(/8X,'   # FIXED PARAMETER NAMES')
        NLINE = NLINE + 2
        NLFIXPAR = NLINE
C  NLFIXPAR IS THE FIXED PARAMETER NAME HEADER LINE NO.
     
        IF(NOFIX .GT. 0) THEN
         DO IFIX = 1,NOFIX
          WRITE(21,1717) PARFIX(IFIX)
          NLINE = NLINE + 1
         END DO
        ENDIF

        WRITE(21,353) 
  353   FORMAT(/8X,'   # RANFIX PARAMETER NAMES')
        NLINE = NLINE + 2
        NLRANFIXPAR = NLINE
C  NLRANFIXPAR IS THE RANFIX PARAMETER NAME HEADER LINE NO.
     
        IF(NRANFIX .GT. 0) THEN
         DO IRANFIX = 1,NRANFIX
          WRITE(21,1717) PARRANFIX(IRANFIX)
          NLINE = NLINE + 1
         END DO
        ENDIF

        WRITE(21,154) 
  154   FORMAT(/8X,'   # PARAMETER BOUNDARIES')
        NLINE = NLINE + 2
        NLAB = NLINE
C  NLAB IS THE BOUNDARIES HEADER LINE NO.       

        DO I = 1,NVAR
         WRITE(21,*) AB(I,1),AB(I,2)
         NLINE = NLINE + 1
        END DO


        WRITE(21,156) 
  156   FORMAT(/8X,'   # FIXED PARAMETER VALUES')
        NLINE = NLINE + 2

        NLFIXVAL = NLINE
C  NLFIXVAL IS THE FIXED PARAMETER VALUES HEADER LINE NO.

        IF(NOFIX .GT. 0) THEN
         DO IFIX = 1,NOFIX
          WRITE(21,*) VALFIX(IFIX)
          NLINE = NLINE + 1
         END DO
        ENDIF



        WRITE(21,356) 
  356   FORMAT(/8X,'   # RANFIX PARAMETER ESTIMATES')
        NLINE = NLINE + 2

        NLRANFIXVAL = NLINE
C  NLRANFIXVAL IS THE RANFIX PARAMETER ESTIMATES HEADER LINE NO.

        IF(NRANFIX .GT. 0) THEN
         DO IRANFIX = 1,NRANFIX
          WRITE(21,*) RANFIXEST(IRANFIX)
          NLINE = NLINE + 1
         END DO
        ENDIF



        WRITE(21,336) 
  336   FORMAT(/8X,'   # COVARIATE NAMES')
        NLINE = NLINE + 2
        NLCOVNAM = NLINE
C  NLCOVNAM IS THE COVARIATE NAME HEADER LINE NO.

        CALL GETCOVR2(NCOV,COVDESCR)     

        IF(NCOV .GE. 1) THEN
         DO ICOV = 1,NCOV  
          WRITE(21,1717) COVDESCR(ICOV)
          NLINE = NLINE + 1
         END DO
        ENDIF

        WRITE(21,157)
  157   FORMAT(/8X,'   # CORDEN (FINAL DENSITY VALUES)')
        NLINE = NLINE + 2
        NLCORDEN = NLINE
C  NLCORDEN IS THE CORDEN ARRAY HEADER NO. LINE.

        DO I = 1,NACTVE
         DO J = 1,NVAR+1
          WRITE(21,*) CORDEN(I,J)
          NLINE = NLINE + 1
         END DO
        END DO

C        write(*,*) "At 2013"

        WRITE(21,2013)
 2013   FORMAT(/8X,'   # BAYESIAN POSTERIOR DENSITY VALUES, IN ORDER')
        NLINE = NLINE + 2
        NLBAYPOS = NLINE
C  NLBAYPOS IS THE BAYESIAN POSTERIOR DENSITY HEADER NO. LINE.

C  SEE ABOVE FOR WHY NNSUB IS USED INSTEAD OF NSUB HERE.
       ISUB = 1
       K = 1
       J = 1
       DO ISUB = 1,NNSUB
        DO K = 1,NACTSUB(ISUB)
         DO J = 1,NVAR+1

C          write (*,*) "ISUB,K,J",ISUB,K,J

          WRITE(21,*) BAYPOS(ISUB,K,J)
          NLINE = NLINE + 1
         END DO
        END DO
       END DO

        WRITE(21,158)
  158   FORMAT(/8X,'   # PYJGX (CONDITIONAL PROB. VALUES)')
        NLINE = NLINE + 2
        NLPYJGX = NLINE
C  NLPYJGX IS THE PYJGX ARRAY HEADER NO. LINE.

        DO JSUB = 1,NSUB
         DO K = 1,NACTVE
          WRITE(21,*) PYJGX(JSUB,K)
          NLINE = NLINE + 1
         END DO
        END DO


        WRITE(21,159)
  159   FORMAT(/8X,'   # YPREDPOP ARRAY')
        NLINE = NLINE + 2
        NLYPREDPOP = NLINE
C  NLYPREDPOP IS THE YPREDPOP ARRAY HEADER NO. LINE.

        DO JSUB = 1,NSUB
         DO IEQ = 1,NUMEQT
          DO IOBS = 1,NOBS(JSUB)
           DO ICEN = 1,3
            WRITE(21,*) YPREDPOP(JSUB,IEQ,IOBS,ICEN)
            NLINE = NLINE + 1
           END DO
          END DO
         END DO
        END DO


        WRITE(21,161)
  161   FORMAT(/8X,'   # YPREDBAY ARRAY')
        NLINE = NLINE + 2

        NLYPREDBAY = NLINE
C  NLYPREDBAY IS THE YPREDBAY ARRAY HEADER NO. LINE.

        DO JSUB = 1,NSUB
         DO IEQ = 1,NUMEQT
          DO IOBS = 1,NOBS(JSUB)
           DO ICEN = 1,3
            WRITE(21,*) YPREDBAY(JSUB,IEQ,IOBS,ICEN)
            NLINE = NLINE + 1
           END DO
          END DO
         END DO
        END DO


        WRITE(21,162)
  162   FORMAT(/8X,'   # TTPRED ARRAY')
        NLINE = NLINE + 2
        NLTTPRED = NLINE

C  NLTTPRED IS THE TTPRED ARRAY HEADER NO. LINE.


        DO JSUB=1,NSUB
         DO J=1,NUMT(JSUB)
          WRITE(21,*) TTPRED(JSUB,J)
          NLINE = NLINE + 1
         END DO
        END DO


        WRITE(21,163)
  163   FORMAT(/8X,'   # YPREDPOPT ARRAY')
        NLINE = NLINE + 2
        NLYPREDPOPT = NLINE
C  NLYPREDPOPT IS THE YPREDPOPT ARRAY HEADER NO. LINE.

        DO JSUB = 1,NSUB
         DO IEQ = 1,NUMEQT
          DO J = 1,NUMT(JSUB)
           DO ICEN = 1,3
            WRITE(21,*) YPREDPOPT(JSUB,IEQ,J,ICEN)
            NLINE = NLINE + 1
           END DO
          END DO
         END DO
        END DO


        WRITE(21,164)
  164   FORMAT(/8X,'   # EXX ARRAY')
        NLINE = NLINE + 2

        NLEXX = NLINE
C  NLEXX IS THE EXX ARRAY HEADER NO. LINE.

        DO JSUB = 1,NSUB
         DO ICEN = 1,3
          DO J = 1,NVAR
           WRITE(21,*) EXX(JSUB,ICEN,J)
           NLINE = NLINE + 1
          END DO
         END DO
        END DO


        WRITE(21,166)
  166   FORMAT(/8X,'   # CYCLE LOG-LIKLIHOODS')
        NLINE = NLINE + 2
        NLCYCLOGLIK = NLINE
C  NLCYCLOGLIK IS THE XLOGLIK HEADER NO. LINE.

        DO ICYCLE = 1,NCYCLE
         WRITE(21,*) XLOGLIK(ICYCLE)
         NLINE = NLINE + 1
        END DO


        WRITE(21,1166)
 1166   FORMAT(/8X,'   # CYCLE AICs AND BICs')
        NLINE = NLINE + 2
        NLCYCAICBIC = NLINE
C  NLCYCAICBIC IS THE AICBIC HEADER NO. LINE.

        DO ICYCLE = 1,NCYCLE

         WRITE(21,*) (AICBIC(ICYCLE,J),J=1,2)
         NLINE = NLINE + 1
        END DO


        WRITE(21,167)
  167   FORMAT(/8X,'   # CYCLE MEAN VECTORS')
        NLINE = NLINE + 2
        NLCYCMEAN = NLINE
C  NLCYCMEAN IS THE XMEAN ARRAY HEADER NO. LINE.

        DO ICYCLE = 1,NCYCLE
         DO J = 1,NVAR
          WRITE(21,*) XMEAN(ICYCLE,J)
          NLINE = NLINE + 1
         END DO
        END DO


        WRITE(21,168)
  168   FORMAT(/8X,'   # CYCLE STD. DEV. VECTORS')
        NLINE = NLINE + 2
        NLCYCSTDEV = NLINE
C  NLCYCSTDEV IS THE STDEV ARRAY HEADER NO. LINE.

C  NOTE BELOW THAT THERE ARE INDXSD CYCLES WITH STD. DEV. INFO. THIS
C  MAY BE LESS THAN NCYCLE SINCE ANY CYCLES WITH A SINGULAR COV. MATRIX
C  WILL HAVE SUPPRESSED STD. DEV. VALUES.

        DO ICYCLE = 1,INDXSD
         DO J = 1,NVAR  
          WRITE(21,*) STDEV(ICYCLE,J)
          NLINE = NLINE + 1
         END DO
        END DO


        WRITE(21,169)
  169   FORMAT(/8X,'   # CYCLE ADDITIONAL STATISTICS ')
        NLINE = NLINE + 2
        NLCYCADDSTAT = NLINE
C  NLCYCADDSTAT IS THE CYCLE ADDITIONAL STAT. BLOCK HEADER NO. LINE.

C  REWIND THE OUTPUT FILE, AND READ IN THESE ADDITIONAL STAT. BLOCKS
C  FOR EACH CYCLE, AND WRITE THEM TO FILE 21, LINE FOR LINE. THE
C  ADDITIONAL STATISTICS BLOCK STARTS AFTER THE LINE IN THE OUTPUT
C  FILE WHICH BEGINS WITH  "IN THE LINE IS THE THE".

        REWIND(25)
        ICYCLE = 0


  180   READ(25,2) READLINE
        IF(READLINE(2:23) .NE. 'IN THE LINE IS THE THE') GO TO 180



C  TO GET TO THIS POINT, A BLOCK OF ADDITIONAL STATISTICS FOLLOWS, UNTIL
C  ICYCLE GETS TO ICYCTOT - ICYCSTART + 1. AFTER THAT ALL SUCH BLOCKS 
C  ARE FOR THE BAYESIAN DENSITIES.

        ICYCLE = ICYCLE + 1


        IF(ICYCLE .GT. ICYCTOT - ICYCSTART + 1) GO TO 220

C  THERE ARE NOW 2 BLANK LINES WHICH WILL BE SKIPPED. THEN, EACH R.V. 
C  BLOCK OF 3 LINES BEGINS WITH A BLANK LINE AND A LINE WITH THE R.V.
C  NAME ON IT. BOTH OF THESE LINES WILL BE SKIPPED ALSO. I.E., WRITE
C  NVAR x 3 LINES CONTIGUOUSLY (I.E., WITHOUT THE VARIABLE NAMES, AND 
C  WITH NO INTERVEENING BLANK LINES).


        READ(25,*)
        READ(25,*)

        DO IVAR = 1,NVAR
         READ(25,*)
         READ(25,*)
          DO II = 1,3
           READ(25,2) READLINE
           WRITE(21,182) READLINE(1:80)
           NLINE = NLINE + 1
          END DO
        END DO


  182   FORMAT(A80)

        GO TO 180



  220   WRITE(21,171)
  171   FORMAT(/8X,'   # CYCLE GAMLAM VALUES')


        NLINE = NLINE + 2
        NLCYCGAM = NLINE
C  NLCYCGAM IS THE GAMLAMK HEADER NO. LINE.

        DO ICYCLE = 1,NCYCLE
         WRITE(21,*) GAMLAM(ICYCLE)
         NLINE = NLINE + 1
        END DO


        WRITE(21,172)
  172   FORMAT(/8X,'   # BAYESIAN LOG-LIKLIHOODS')
        NLINE = NLINE + 2
        NLBAYLOGLIK = NLINE
C  NLBAYLOGLIK IS THE SUBLOGLIK HEADER NO. LINE.


        DO JSUB = 1,NSUB
         WRITE(21,*) SUBLOGLIK(JSUB)
         NLINE = NLINE + 1
        END DO


        WRITE(21,173)
  173   FORMAT(/8X,'   # BAYESIAN MEANS')
        NLINE = NLINE + 2
        NLBAYMEAN = NLINE
C  NLBAYMEAN IS THE SUBMEAN ARRAY HEADER NO. LINE.

        DO JSUB = 1,NSUB
         DO J = 1,NVAR
          WRITE(21,*) SUBMEAN(JSUB,J)

          NLINE = NLINE + 1
         END DO
        END DO


        WRITE(21,174)
  174   FORMAT(/8X,'   # BAYESIAN STD. DEVS')
        NLINE = NLINE + 2
        NLBAYSTD = NLINE
C  NLBAYSTD IS THE SUBSTD ARRAY HEADER NO. LINE.

        DO JSUB = 1,NSUB
         DO J = 1,NVAR
          WRITE(21,*) SUBSTD(JSUB,J)
          NLINE = NLINE + 1
         END DO
        END DO

        WRITE(21,176)
  176   FORMAT(/8X,'   # BAYESIAN ADDITIONAL STATISTICS ')
        NLINE = NLINE + 2
        NLBAYADDSTAT = NLINE
C  NLBAYADDSTAT IS THE BAYESIAN ADDITIONAL STAT. BLOCK HEADER NO. LINE.

C  THE OUTPUT FILE WAS READ ABOVE (IN THE CODE WHICH READ IN THE
C  CYCLE ADDITIONAL STAT BLOCKS) DOWN TO START OF THE ADDITIONAL STAT

C  BLOCK FOR BAYESIAN RESULTS FOR THE 1ST SUBJECT. BACKSPACE THE FILE
C  A FEW LINES, AND THEN READ IN THESE ADDITIONAL STAT. BLOCKS IN THE
C  SAME MANNER AS THE CYCLE ADDITIONAL STATISTICS BLOCKS WERE READ IN
C  (I.E., CONTIGUOUSLY, WITH NO INTERVEENING BLANK LINES, OR LINES WITH
C  R.V. NAMES).


        BACKSPACE(25)
        BACKSPACE(25)
        BACKSPACE(25)
        JSUB = 0

  280   IF(JSUB .EQ. NSUB) GO TO 230
        READ(25,2,IOSTAT=IEND) READLINE
        IF(READLINE(2:23) .NE. 'IN THE LINE IS THE THE') GO TO 280


C  TO GET TO THIS POINT, A BLOCK OF ADDITIONAL STATISTICS FOLLOWS

        JSUB = JSUB + 1

C  THERE ARE NOW 2 BLANK LINES WHICH WILL BE SKIPPED. THEN, EACH R.V. 
C  BLOCK OF 3 LINES BEGINS WITH A BLANK LINE AND A LINE WITH THE R.V.
C  NAME ON IT. BOTH OF THESE LINES WILL BE SKIPPED ALSO. I.E., WRITE
C  NVAR x 3 LINES CONTIGUOUSLY (I.E., WITHOUT THE VARIABLE NAMES, AND 
C  WITH NO INTERVEENING BLANK LINES).

        READ(25,*)
        READ(25,*)

        DO IVAR = 1,NVAR
         READ(25,*)
         READ(25,*)
          DO II = 1,3
           READ(25,2) READLINE
           WRITE(21,182) READLINE(1:80)
           NLINE = NLINE + 1
          END DO
        END DO

        GO TO 280


  230   CONTINUE


        WRITE(21,179)
  179   FORMAT(/8X,'   # PATIENT ID DATA')
        NLINE = NLINE + 2
        NLPATID = NLINE
C  NLPATID IS THE PATIENT ID DATA HEADER NO. LINE.


        DO JSUB = 1,NSUB

         WRITE(21,181) NAME(JSUB)
  181    FORMAT(A53)
         WRITE(21,181) CHARTNO(JSUB)

C  REPLACE WRITING OF AGE() AND HEIGHT() WITH XVERIFY (SEE LOGIC IN 
C  SUBROUTINE VERIFYVAL.

         XVERIFY(1) = AGE(JSUB)
         XVERIFY(2) = HEIGHT(JSUB)
         CALL VERIFYVAL(2,XVERIFY)
C        WRITE(21,1182) AGE(JSUB),SEX(JSUB),HEIGHT(JSUB)
         WRITE(21,1182) XVERIFY(1),SEX(JSUB),XVERIFY(2)
 1182    FORMAT(2X,F10.3,2X,A1,2X,F10.3)
         NLINE = NLINE + 3

        END DO

        WRITE(21,183)
  183   FORMAT(/8X,'   # PATIENT DOSECOV BLOCKS')
        NLINE = NLINE + 2
        NLPATDOS = NLINE
C  NLPATDOS IS THE PATIENT DOSECOV BLOCK HEADER NO. LINE.

        DO JSUB =1,NSUB
         DO IDOSE = 1,NDORIG(JSUB)
          DO J = 1,1+NI
           WRITE(21,*) DOSEBLOCK(JSUB,IDOSE,J) 
           NLINE = NLINE + 1
          END DO
         END DO
        END DO

        WRITE(21,184)
  184   FORMAT(/8X,'   # PATIENT OUTPUT AND ASSAY COEFF. BLOCKS')
        NLINE = NLINE + 2
        NLPATOUTASSAY = NLINE
C  NLPATOUTASSAY IS THE PATIENT OUTPUT + ASSAY COEFF. BLOCK HEADER NO. 
C  LINE.
     
        DO JSUB =1,NSUB
         DO IOBS = 1,NOBS(JSUB)
          DO J = 2,NUMEQT+1
           WRITE(21,*) JSUB, OBSBLOCK(JSUB,IOBS,1), J-1,
     1      OBSBLOCK(JSUB,IOBS,J), (ASSAYC(JSUB,J-1,K),K=1,4)
           NLINE = NLINE + 1
          END DO
         END DO
        END DO


C --------------  REWIND FILE 21 AND COPY INFO TO OUTFILER ------------


C  CAN'T COPY FILE 21 EXACTLY TO OUTFILER SINCE THE LINE NUMBERS FOR THE
C  START OF THE ARRAYS, WHICH WERE CALCULATED ABOVE WILL HAVE TO BE
C  INSERTED AT THE APPROPRIATE SPOTS.

        REWIND(21)
        OPEN(22,FILE=OUTFILER)

  250   READ(21,2) READLINE

        IF(READLINE(12:22) .NE. '# START PAR') THEN
         CALL CONDENSE2(READLINE)
         GO TO 250
        ENDIF

C  WRITE IN THE # START PAR LINE, BUT INCLUDE THE LINE NO., NLRANPAR.

        WRITE(22,186) NLRANPAR
  186   FORMAT(2X,I10,'   # START PAR') 
         

  260   READ(21,2) READLINE

        IF(READLINE(12:25) .NE. '# START PARFIX') THEN
         CALL CONDENSE2(READLINE)
         GO TO 260
        ENDIF

C  WRITE IN THE # START PARFIX LINE, BUT INCLUDE THE LINE NO., NLFIXPAR.

        WRITE(22,187) NLFIXPAR
  187   FORMAT(2X,I10,'   # START PARFIX') 


  960   READ(21,2) READLINE

        IF(READLINE(12:28) .NE. '# START PARRANFIX') THEN
         CALL CONDENSE2(READLINE)
         GO TO 960
        ENDIF

C  WRITE IN THE # START PARRANFIX LINE, BUT INCLUDE THE LINE NO., 
C  NLRANFIXPAR.

        WRITE(22,1187) NLRANFIXPAR
 1187   FORMAT(2X,I10,'   # START PARRANFIX') 



  270   READ(21,2) READLINE

        IF(READLINE(12:21) .NE. '# START AB') THEN
         CALL CONDENSE2(READLINE)
         GO TO 270
        ENDIF

C  WRITE IN THE # START AB LINE, BUT INCLUDE THE LINE NO., NLAB.

        WRITE(22,188) NLAB
  188   FORMAT(2X,I10,'   # START AB') 



 1280   READ(21,2) READLINE

        IF(READLINE(12:25) .NE. '# START VALFIX') THEN
         CALL CONDENSE2(READLINE)
         GO TO 1280
        ENDIF

C  WRITE IN THE # START VALFIX LINE, BUT INCLUDE THE LINE NO., NLFIXVAL.

        WRITE(22,189) NLFIXVAL
  189   FORMAT(2X,I10,'   # START VALFIX')



1380   READ(21,2) READLINE

        IF(READLINE(12:28) .NE. '# START RANFIXEST') THEN
         CALL CONDENSE2(READLINE)
         GO TO 1380
        ENDIF

C  WRITE IN THE # START RANFIXEST LINE, BUT INCLUDE THE LINE NO., 
C  NLRANFIXVAL.

        WRITE(22,1189) NLRANFIXVAL
 1189   FORMAT(2X,I10,'   # START RANFIXEST')



  840   READ(21,2) READLINE

        IF(READLINE(12:25) .NE. '# START COVARI') THEN
         CALL CONDENSE2(READLINE)
         GO TO 840
        ENDIF

C  WRITE IN THE # START COVARIATE NAMES LINE, BUT INCLUDE THE LINE NO.,
C  NLCOVNAM.

        WRITE(22,841) NLCOVNAM
  841   FORMAT(2X,I10,'   # START COVARIATE NAMES')


 1290   READ(21,2) READLINE

        IF(READLINE(12:25) .NE. '# START CORDEN') THEN

         CALL CONDENSE2(READLINE)
         GO TO 1290
        ENDIF

C  WRITE IN THE # START CORDEN LINE, BUT INCLUDE THE LINE NO., NLCORDEN.

        WRITE(22,191) NLCORDEN
  191   FORMAT(2X,I10,'   # START CORDEN')



2020   READ(21,2) READLINE

        IF(READLINE(12:37) .NE. '# START BAYESIAN POSTERIOR') THEN
         CALL CONDENSE2(READLINE)
         GO TO 2020
        ENDIF

C  WRITE IN THE # START BAYESIAN POSTERIOR DENSITIES LINE , BUT INCLUDE
C  THE LINE NO., NLBAYPOS.

        WRITE(22,2014) NLBAYPOS
 2014   FORMAT(2X,I10,'   # START BAYESIAN POSTERIOR DENSITIES')



 1310   READ(21,2) READLINE

        IF(READLINE(12:24) .NE. '# START PYJGX') THEN
         CALL CONDENSE2(READLINE)
         GO TO 1310
        ENDIF

C  WRITE IN THE # START PYJGX LINE, BUT INCLUDE THE LINE NO., NLPYJGX.

        WRITE(22,192) NLPYJGX
  192   FORMAT(2X,I10,'   # START PYJGX')


  320   READ(21,2) READLINE

        IF(READLINE(12:27) .NE. '# START YPREDPOP') THEN
         CALL CONDENSE2(READLINE)
         GO TO 320
        ENDIF

C  WRITE IN THE # START YPREDPOP LINE, BUT INCLUDE THE LINE NO., 
C  YPREDPOP.

        WRITE(22,193) NLYPREDPOP
  193   FORMAT(2X,I10,'   # START YPREDPOP')


  330   READ(21,2) READLINE


        IF(READLINE(12:27) .NE. '# START YPREDBAY') THEN
         CALL CONDENSE2(READLINE)
         GO TO 330
        ENDIF

C  WRITE IN THE # START YPREDBAY LINE, BUT INCLUDE THE LINE NO., 
C  YPREDBAY.

        WRITE(22,194) NLYPREDBAY

  194   FORMAT(2X,I10,'   # START YPREDBAY')


  340   READ(21,2) READLINE

        IF(READLINE(12:25) .NE. '# START TTPRED') THEN
         CALL CONDENSE2(READLINE)
         GO TO 340
        ENDIF

C  WRITE IN THE # START TTPRED LINE, BUT INCLUDE THE LINE NO., NLTTPRED.

        WRITE(22,196) NLTTPRED
  196   FORMAT(2X,I10,'   # START TTPRED')



  350   READ(21,2) READLINE

        IF(READLINE(12:28) .NE. '# START YPREDPOPT') THEN
         CALL CONDENSE2(READLINE)
         GO TO 350


        ENDIF

C  WRITE IN THE # START YPREDPOPT LINE, BUT INCLUDE THE LINE NO., 
C  NLYPREDPOPT.


        WRITE(22,197) NLYPREDPOPT
  197   FORMAT(2X,I10,'   # START YPREDPOPT')


  360   READ(21,2) READLINE

        IF(READLINE(12:22) .NE. '# START EXX') THEN
         CALL CONDENSE2(READLINE)
         GO TO 360
        ENDIF

C  WRITE IN THE # START EXX LINE, BUT INCLUDE THE LINE NO., NLEXX.

        WRITE(22,198) NLEXX
  198   FORMAT(2X,I10,'   # START EXX')


  370   READ(21,2) READLINE

        IF(READLINE(12:28) .NE. '# START CYCLE LOG') THEN
         CALL CONDENSE2(READLINE)
         GO TO 370
        ENDIF

C  WRITE IN THE # START CYCLE LOG-LIKELIHOODS LINE, BUT INCLUDE THE
C  LINE NO., NLCYCLOGLIK

        WRITE(22,199) NLCYCLOGLIK
  199   FORMAT(2X,I10,'   # START CYCLE LOG-LIKELIHOODS')


 1370   READ(21,2) READLINE

        IF(READLINE(12:28) .NE. '# START CYCLE AIC') THEN
         CALL CONDENSE2(READLINE)
         GO TO 1370
        ENDIF

C  WRITE IN THE # START CYCLE AICBIC LINE, BUT INCLUDE THE
C  LINE NO., NLCYCAICBIC


        WRITE(22,1199) NLCYCAICBIC
 1199   FORMAT(2X,I10,'   # START CYCLE AIC AND BIC VALUES')


  380   READ(21,2) READLINE

        IF(READLINE(12:28) .NE. '# START CYCLE MEA') THEN
         CALL CONDENSE2(READLINE)
         GO TO 380
        ENDIF

C  WRITE IN THE # START CYCLE MEANS LINE, BUT INCLUDE THE
C  LINE NO., NLCYCMEAN.

        WRITE(22,201) NLCYCMEAN
  201   FORMAT(2X,I10,'   # START CYCLE MEANS')


  390   READ(21,2) READLINE

        IF(READLINE(12:28) .NE. '# START CYCLE STD') THEN
         CALL CONDENSE2(READLINE)
         GO TO 390
        ENDIF

C  WRITE IN THE # START CYCLE STD. DEVS. LINE, BUT INCLUDE THE
C  LINE NO., NLCYCSTDEV

        WRITE(22,202) NLCYCSTDEV
  202   FORMAT(2X,I10,'   # START CYCLE STD. DEVS.')


  410   READ(21,2) READLINE

        IF(READLINE(12:28) .NE. '# START CYCLE ADD') THEN
         CALL CONDENSE2(READLINE)
         GO TO 410
        ENDIF

C  WRITE IN THE # START CYCLE ADDITIONAL STATISTICS LINE, BUT INCLUDE
C  THE LINE NO., NLCYCADDSTAT

        WRITE(22,203) NLCYCADDSTAT
  203   FORMAT(2X,I10,'   # START CYCLE ADDITIONAL STATISTICS')


  420   READ(21,2) READLINE

        IF(READLINE(12:28) .NE. '# START CYCLE GAM') THEN
         CALL CONDENSE2(READLINE)
         GO TO 420
        ENDIF

C  WRITE IN THE # START CYCLE GAMLAM LINE, BUT INCLUDE
C  THE LINE NO., NLCYCGAM

        WRITE(22,204) NLCYCGAM
  204   FORMAT(2X,I10,'   # START CYCLE GAMLAM VALUES')


  430   READ(21,2) READLINE

        IF(READLINE(12:31) .NE. '# START BAYESIAN LOG') THEN
         CALL CONDENSE2(READLINE)
         GO TO 430
        ENDIF

C  WRITE IN THE # START BAYESIAN LOG-LIKELIHOODS LINE, BUT INCLUDE
C  THE LINE NO., NLBAYLOGLIK

        WRITE(22,206) NLBAYLOGLIK

  206   FORMAT(2X,I10,'   # START BAYESIAN LOG-LIKELIHOODS')


  440   READ(21,2) READLINE

        IF(READLINE(12:31) .NE. '# START BAYESIAN MEA') THEN
         CALL CONDENSE2(READLINE)
         GO TO 440
        ENDIF

C  WRITE IN THE # START BAYESIAN MEANS LINE, BUT INCLUDE
C  THE LINE NO., NLBAYMEAN

        WRITE(22,207) NLBAYMEAN
  207   FORMAT(2X,I10,'   # START BAYESIAN MEANS')


  450   READ(21,2) READLINE

        IF(READLINE(12:31) .NE. '# START BAYESIAN STD') THEN
         CALL CONDENSE2(READLINE)
         GO TO 450
        ENDIF

C  WRITE IN THE # START BAYESIAN STD. DEVS LINE BUT INCLUDE
C  THE LINE NO., NLBAYSTD.

        WRITE(22,208) NLBAYSTD
  208   FORMAT(2X,I10,'   # START BAYESIAN STD. DEVS.')


  460   READ(21,2) READLINE

        IF(READLINE(12:31) .NE. '# START BAYESIAN ADD') THEN
         CALL CONDENSE2(READLINE)
         GO TO 460
        ENDIF

C  WRITE IN THE # START BAYESIAN ADDITIONAL STATISTICS LINE BUT INCLUDE
C  THE LINE NO., NLBAYADDSTAT.
 
        WRITE(22,209) NLBAYADDSTAT
  209   FORMAT(2X,I10,'   # START BAYESIAN ADDITIONAL STATISTICS')


  480   READ(21,2) READLINE

        IF(READLINE(12:30) .NE. '# START PATIENT IDS') THEN
         CALL CONDENSE2(READLINE)
         GO TO 480
        ENDIF

C  WRITE IN THE # START PATIENT IDS LINE BUT INCLUDE
C  THE LINE NO., NLPATID.
 
        WRITE(22,212) NLPATID
  212   FORMAT(2X,I10,'   # START PATIENT IDS')




  490   READ(21,2) READLINE

        IF(READLINE(12:30) .NE. '# START PATIENT DOS') THEN
         CALL CONDENSE2(READLINE)
         GO TO 490
        ENDIF

C  WRITE IN THE # START PATIENT DOSE COV. BLOCKS LINE BUT INCLUDE
C  THE LINE NO., NLPATDOS.
 
        WRITE(22,213) NLPATDOS
  213   FORMAT(2X,I10,'   # START PATIENT DOSE COV. BLOCKS')


  510   READ(21,2) READLINE

        IF(READLINE(12:30) .NE. '# START PATIENT OUT') THEN
         CALL CONDENSE2(READLINE)
         GO TO 510

        ENDIF

C  WRITE IN THE # START PATIENT OUTPUT AND ASSAY COEFF. BLOCKS LINE
C  BUT INCLUDE THE LINE NO., NLPATOUTASSAY.
 
        WRITE(22,214) NLPATOUTASSAY
  214 FORMAT(2X,I10,'   # START PATIENT OUTPUT AND ASSAY COEFF. BLOCKS')

C  WRITE THE REST OF FILE 21 TO FILE 22.
C  NOTE THAT IEND .LT. 0 --> END OF FILE REACHED.	

  600   READ(21,222,IOSTAT=IEND) READLINE2
  222   FORMAT(A1000)
        IF(IEND .LT. 0) GO TO 700
        CALL CONDENSE3(READLINE2)
        GO TO 600

  700   CLOSE(21)
        CLOSE(22)



        RETURN

        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C	SUBROUTINE SEPARATE WAS REMOVED AS OF read18.f
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE GETNUM(NUMEQT)
	IMPLICIT REAL*8(A-H,O-Z)
	CHARACTER READLINE*1000


C  THIS SUBROUTINE, CALLED BY PREVRUN, READS DOWN IN FILE 27, WHICH IS 
C  ALREADY REWOUND TO OBTAIN THE NO. OF OUTPUT EQUATIONS (NUMEQT).


    2   FORMAT(A1000)

C  NOTE THAT NUMEQT IS ON THE LINE WITH "NO. OF TOTAL OUTPUT EQUATIONS"
C  IN COLUMNS 12:40. IF NO LINE HAS THESE WORDS, THIS PATIENT DATA
C  FILE IS NOT A NEW-STYLE WORKING COPY FILE FROM ANDREAS' NEW
C  BOXES PROGRAM.

   35	READ(27,2,IOSTAT=IEND) READLINE

	IF(IEND .LT. 0) THEN
	 WRITE(*,57) 
   57    FORMAT(//' THE COMBINATION OUTPUT FILE YOU HAVE ENTERED TO'/
     1' THIS PROGRAM WAS NOT MADE BY A RECENT BIG NPAG PROGRAM.'//
     2' SUCH A FILE MUST HAVE CONCATENATED PATIENT DATA FILES HAVING'/
     3' A LINE WITH "NO. OF TOTAL OUTPUT EQUATIONS" IN COLUMNS 12:40.'//
     3' THE PROGRAM STOPS. '//)
C	 CALL PAUSE
	 STOP
	ENDIF

	IF(READLINE(12:40) .NE. 'NO. OF TOTAL OUTPUT EQUATIONS')GO TO 35

	BACKSPACE(27)
   13   FORMAT(T2,I5)


        READ(27,13) NUMEQT

	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE GETNSUB2(NSUBTOT)
	CHARACTER READLINE*1000

C  THIS ROUTINE IS A VARIATION OF TEST38.FOR, WHICH READS AN INTEGER
C  ANYWHERE ON A GIVEN LINE WITH A PARTICULAR SET OF WORDS AT THE START 
C  OF THAT LINE.


    2   FORMAT(A1000)


C  READ UNTIL THE LINE WHICH HAS THE WORDS "CTS IN THE DATA SET IS" ON
C  IT SOMEWHERE.

   10   READ(25,2) READLINE
	ILINE=0

	 DO I=1,51
	  IF(READLINE(I:I+21) .EQ. 'CTS IN THE DATA SET IS') THEN
	   ILINE=1
	   GO TO 20
	  ENDIF
	 END DO
   20   IF(ILINE .EQ. 0) GO TO 10

C  SOMEWHERE AFTER THE CHARACTERS IN ENTRIES I:I+21 IS THE INTEGER.

C  READ THE CHARACTERS FOR THIS INTEGER, AND THEN CONVERT IT TO
C  AN INTEGER VALUE. AFTER THE FOLLOWING LOOP, THESE CHARACTERS WILL BE
C  IN READLINE(ISTART:IEND).

	 IEND = 0
	 ISTART = 0

	  DO J = I+22, 72
	   IF(ISTART .EQ. 0 .AND. READLINE(J:J) .NE. ' ') ISTART = J
	   IF(ISTART .NE. 0 .AND. READLINE(J:J) .EQ. ' ') THEN
	    IEND = J-1
	    GO TO 30
	   ENDIF
	  END DO

C  CHECK TO MAKE SURE THAT THE NO. OF CHARACTERS READ IN FOR THE INTEGER
C  IS NOT MORE THAN 4 (I.E., 4 CHARACTERS ALLOW A MAXIMUM SIZE OF 9999

C  WHICH IS MORE THAN THE LARGEST VALUE THIS INTEGER CAN BE).

   30	ISIZE = IEND-ISTART

	IF(ISIZE .GT. 3) THEN
	 WRITE(*,*)' NSUBTOT IS ',NSUBTOT,' WHICH IS TOO LARGE. '
	 WRITE(*,*)' THE PROGRAM STOPS. '
C	 CALL PAUSE
	 STOP
	ENDIF

C  CONVERT AS INDICATED ABOVE.

	NSUBTOT = 0
	 DO K=ISTART,IEND
	  IF(READLINE(K:K) .EQ. '0') IVAL = 0
	  IF(READLINE(K:K) .EQ. '1') IVAL = 1
	  IF(READLINE(K:K) .EQ. '2') IVAL = 2
	  IF(READLINE(K:K) .EQ. '3') IVAL = 3
	  IF(READLINE(K:K) .EQ. '4') IVAL = 4
	  IF(READLINE(K:K) .EQ. '5') IVAL = 5
	  IF(READLINE(K:K) .EQ. '6') IVAL = 6
	  IF(READLINE(K:K) .EQ. '7') IVAL = 7
	  IF(READLINE(K:K) .EQ. '8') IVAL = 8
	  IF(READLINE(K:K) .EQ. '9') IVAL = 9
	  NSUBTOT = NSUBTOT + IVAL*10**ISIZE
	  ISIZE = ISIZE-1
	 END DO


	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE GETNSUB(NSUB)

C  THIS ROUTINE IS A VARIATION OF TEST38.FOR, WHICH READS AN INTEGER
C  ANYWHERE ON A GIVEN LINE WITH A PARTICULAR SET OF WORDS AT THE START 
C  OF THAT LINE.


	CHARACTER READLINE*1000

    2   FORMAT(A1000)

C  READ UNTIL THE LINE WHICH HAS THE WORDS "THE NO. OF SUBJECTS IS" ON
C  IT SOMEWHERE.

   10   READ(25,2) READLINE
	ILINE=0
	 DO I=1,51
	  IF(READLINE(I:I+21) .EQ. 'THE NO. OF SUBJECTS IS') THEN
	   ILINE=1
	   GO TO 20
	  ENDIF
	 END DO
   20   IF(ILINE .EQ. 0) GO TO 10

C  SOMEWHERE AFTER THE CHARACTERS IN ENTRIES I:I+21 IS THE INTEGER.
C  READ THE CHARACTERS FOR THIS INTEGER, AND THEN CONVERT IT TO
C  AN INTEGER VALUE. AFTER THE FOLLOWING LOOP, THESE CHARACTERS WILL BE
C  IN READLINE(ISTART:IEND).

	 IEND = 0
	 ISTART = 0
	  DO J = I+22, 72
	   IF(ISTART .EQ. 0 .AND. READLINE(J:J) .NE. ' ') ISTART = J
	   IF(ISTART .NE. 0 .AND. READLINE(J:J) .EQ. ' ') THEN
	    IEND = J-1
	    GO TO 30
	   ENDIF
	  END DO

C  CHECK TO MAKE SURE THAT THE NO. OF CHARACTERS READ IN FOR THE INTEGER
C  IS NOT MORE THAN 4 (I.E., 4 CHARACTERS ALLOW A MAXIMUM SIZE OF 9999
C  WHICH IS MORE THAN THE LARGEST VALUE THIS INTEGER CAN BE).

   30	ISIZE = IEND-ISTART

	IF(ISIZE .GT. 3) THEN
	 WRITE(*,*)' NSUB IS ',NSUB,' WHICH IS TOO LARGE. '
	 WRITE(*,*)' THE PROGRAM STOPS. '
C	 CALL PAUSE
	 STOP
	ENDIF

C  CONVERT AS INDICATED ABOVE.

	NSUB = 0
	 DO K=ISTART,IEND
	  IF(READLINE(K:K) .EQ. '0') IVAL = 0
	  IF(READLINE(K:K) .EQ. '1') IVAL = 1
	  IF(READLINE(K:K) .EQ. '2') IVAL = 2
	  IF(READLINE(K:K) .EQ. '3') IVAL = 3
	  IF(READLINE(K:K) .EQ. '4') IVAL = 4
	  IF(READLINE(K:K) .EQ. '5') IVAL = 5
	  IF(READLINE(K:K) .EQ. '6') IVAL = 6
	  IF(READLINE(K:K) .EQ. '7') IVAL = 7
	  IF(READLINE(K:K) .EQ. '8') IVAL = 8
	  IF(READLINE(K:K) .EQ. '9') IVAL = 9

	  NSUB = NSUB + IVAL*10**ISIZE
	  ISIZE = ISIZE-1
	 END DO


	RETURN
	END

C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  SUBROUTINE SEEDIR(PATH,NOB,FILENAME) REMOVED IN READ6.F
C

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE GETIPATFF(IFILE,NSUBTOT,NSUB,MAXSUB,
     1   IPATVEC,IERRR)
	DIMENSION IPATVEC(MAXSUB)
	CHARACTER READLINE*1000


C  SUBROUTINE GETIPATFF IS THE SAME AS GETIPATF, EXCEPT THE ERROR
C  MESSAGES TO THE USER ARE DIFFERENT.

C  SUBROUTINE GETIPATFF IS CALLED BY PREVRUN TO OBTAIN THE INDICES OF
C  THE "ACTIVE" SUBJECTS, IPATVEC(I),I=1,NSUB, FOR THIS RUN. THESE
C  INDICES ARE OBTAINED FROM FILE IFILE.


    3   FORMAT(A1000)

	NSUBB = 0
	NUMCUR = 0


C  NSUBB WILL BE THE NO. OF SUBJECTS SELECTED BY THE USER. IT IS 
C  INITIALIZED AS 0, AND WILL GROW AS EACH LINE OF THE FILE IS READ.

C  NUMCUR IS THE INDEX OF THE HIGHEST PATIENT NO. ENTERED SO FAR.

C  NOTE THAT EACH LINE CONTAINS EITHER A SUBJECT NO. OR A RANGE OF 


C  SUBJECT NOS. TO BE INCLUDED IN THE ANALYSIS. MULTIPLE SUBJECTS
C  CAN BE ENTERED USING COMMAS, SPACES AND/OR HYPHENS. FOR EXAMPLE,
C  2, 17 - 28 INDICATES SUBJECTS 2 AND 17 THROUGH 28; 
C  17,28 INDICATES SUBJECTS 17 AND 28;
C  17 28 INDICATES SUBJECTS 17 AND 28. 

C  RESTRICTIONS: 1. ON EACH LINE, USE NO MORE THAN 70 CHARACTERS;
C                2. PATIENT NOS., OR RANGES OF PATIENT NOS.
C                   MUST BE LISTED IN ASCENDING ORDER.

C  A LINE WITH JUST A 0 --> END OF THE LIST OF SUBJECT NOS.

c  As of read18.f, SINCE IFILE IS HARDCODED IN THE CALLING STATEMENT TO
c  THIS ROUTINE TO BE 25, simplify the code below so that file 25 is
c  always read.

C 4210	IF(IFILE .EQ. 23) READ(23,3,ERR=4200) READLINE

 4210	READ(25,3,ERR=4200) READLINE

C  CALL SUBROUTINE GETNUMSF2 TO UPDATE NSUBB AND IPATVEC, WHERE IPATVEC
C  IS THE VECTOR WHICH CONTAINS THE PATIENT NOS. TO BE INCLUDED IN THE 
C  ANALYSIS.

C  NOTE THAT ISTOP RETURNS AS 0 TO INDICATE THE END OF THE LIST OF
C  PATIENT NOS; IT RETURNS AS -1 IF THERE IS A CONFLICT IN THE LIST
C  (SEE RESTRICTION ABOVE) OR A PATIENT NO. LARGER THAN NSUBTOT 
C  (THE MAXIMUM ALLOWABLE NO. OF SUBJECTS) HAS BEEN READ IN; AND IT 
C  RETURNS AS 1 IF THERE ARE MORE LINES TO BE READ IN.

	CALL GETNUMSF2(1,READLINE,NSUBB,NSUBTOT,NUMCUR,ISTOP,
     1                  MAXSUB,IPATVEC)
	IF(ISTOP .EQ. -1) GO TO 4200
	IF(ISTOP .EQ. 1) GO TO 4210


C  TO GET TO THIS POINT, ISTOP = 0 --> USER HAS STOPPED ENTERING
C  PATIENT NOS.

C  CHECK THAT NSUB, AS INPUT TO THIS ROUTINE (FROM MAIN) IS THE SAME
C  AS NSUBB, AS OBTAINED FROM THE LIST OF SUBJECTS TO BE INCLUDED IN
C  THE ANALYSIS. IF NOT, TELL USER AND RETURN IERRR = -1 SO THE USER
C  CAN MAKE A CORRECTION.

	IF(NSUB .EQ. NSUBB) THEN
	 IERRR = 0
	 RETURN
	ENDIF

	IF(NSUB .NE. NSUBB) THEN
         WRITE(*,2)
    2   FORMAT(//' THERE WAS AN ERROR IN THE READING OF PATIENT NOS.'/
     1' USED FOR THIS ANALYSIS. IN PARTICULAR, THE NO. OF '/
     2' SUBJECTS TO BE INCLUDED IN THE ANALYSIS, AS ENTERED IN THE'/
     3' OUTPUT FILE, DOES NOT MATCH THE LIST OF SUBJECT NOS.'/

     4' WHICH FOLLOW THAT NUMBER. IF YOU DID NOT MANUALLY EDIT THE'/
     5' OUTPUT FILE AFTER THE RUN, PLEASE CONTACT LAPK AND REPORT THIS'/
     6' ERROR.'//)

	IERRR = -1
	 RETURN
	ENDIF

 4200   WRITE(*,1)
    1   FORMAT(//' THERE WAS AN ERROR IN THE READING OF PATIENT NOS.'/
     1' USED FOR THIS ANALYSIS FROM THE OUTPUT FILE. IF YOU DID NOT '/
     2' MANUALLY EDIT THE OUTPUT FILE AFTER THE RUN, PLEASE CONTACT '/
     3' LAPK AND REPORT THIS ERROR.'//)

	
	IERRR = -1

	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  SUBROUTINE FULLNAME(PATH,FILE,FILE2) REMOVED IN READ6.F
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE FILREDT(NOBSER,TO,YO,C0,C1,C2,C3,MAXOBDIM,NDRUG,ND,
     1   NADD)

C  FILREDT IS CALLED BY SUBROUTINE PREVRUN TO READ THE PORTION OF 

C  SCRATCH FILE 27 WHICH APPLIES TO THE SUBJECT UNDER CONSIDERATION. THE
C  'POINTER' FOR FILE 27 IS IN THE PROPER POSITION TO BEGIN READING THE

C  INFO FOR THE DESIRED SUBJECT.

        use npag_utils, only: maxnumeq,max_RS_J,max_input_dim

        IMPLICIT REAL*8(A-H,O-Z)
C        PARAMETER(MAXNUMEQ=7)

        DIMENSION YO(MAXOBDIM,MAXNUMEQ),RJUNK(max_RS_J),C0(MAXNUMEQ),
     1  C1(MAXNUMEQ),C2(MAXNUMEQ),C3(MAXNUMEQ),TO(MAXOBDIM)

C SIG, RS, AND BS REMOVED IN DIMENSION STMT. - THEY AREN'T USED IN THIS 
C ROUTINE ... AS OF READ5.F.


       CHARACTER READLINE*1000
C  SEX REMOVED IN ABOVE CHARACTER STMT. IT IS NOT USED IN THIS ROUTINE
C  ... AS OF READ5.F.


C  AS OF read12.f, THE FORMAT FOR THE WORKING COPY FILES IS:
C     COL 1 = TIME
C     COL 2 = IV FOR DRUG 1; COL 3 = PO FOR DRUG 1;
C     COL 4 = IV FOR DRUG 2; COL 5 = PO FOR DRUG 2;
C     ... EACH SUCCEEDING DRUG HAS AN IV FOLLOWED BY A PO COLUMN.
C     NEXT NADD COLUMNS = ONE FOR EACH ADDITIONAL COVARIATE (ADDITIONAL
C      REFERS TO ANY EXTRA COVARIATES BEYOUND THE 4 PERMANENT ONES IN
C      COMMON COVDESCR (SEE BELOW).




C  INPUT IS: SCRATCH FILE 27, WHICH IS POSITIONED AT THE BEGINNING OF

C  THE INFO FOR THE SUBJECT DESIRED.

C  OUTPUT ARE:

C  NOBSER = THE NO. OF OBSERVATIONS FOR THIS SUBJECT.
C  YO(I,J),I=1,M; J=1,NUMEQT = NO. OF OUTPUT EQS; I=1,M, WHERE M = NO.
C	OF OBSERVATION TIMES.
C  [C0(J),C1(J),C2(J),C3(J)] = ASSAY NOISE COEFFICIENTS FOR OUTPUT EQ.
C	J; J=1,NUMEQT.


C  AGE, SEX, HEIGHT, AND ETHNICITY FLAG ARE ON LINES 8-11. BUT THEY
C  DON'T NEED TO BE READ IN THIS PROGRAM. ISEX ALSO NOT USED.


	DO I=1,7
	 READ(27,*)
	END DO
	 
	READ(27,*) 
	READ(27,*)
    2   FORMAT(A1)
C	ISEX=1
C	IF(SEX .EQ. 'F') ISEX=2
	
      READ(27,*) 
	READ(27,*) 

C  READ THE NO. OF DRUGS FROM THE LINE WITH 'NO. OF DRUGS' AS ENTRIES
C  12:23. THEN READ NO. OF ADDITIONAL COVARIATES, AND THE NO. OF DOSE 
C  EVENTS, ETC.

    1   FORMAT(A1000)
   10	READ(27,1) READLINE
	IF(READLINE(12:23) .NE. 'NO. OF DRUGS') GO TO 10
	BACKSPACE(27)


    3   FORMAT(T2,I5)
        READ(27,3) NDRUG

	IF(NDRUG .GT. max_input_dim) THEN
	 WRITE(*,124)
  124    FORMAT(' YOUR PATIENT DATA FILES CANNOT HAVE MORE THAN '/
     1' max_input_dim DRUGS. THE PROGRAM IS NOW STOPPING. '/)
	 CALL PAUSE
	 STOP
	ENDIF

        READ(27,3) NADD

C  NOTE THAT THE NO. OF "RATES" INCLUDES 2 FOR EACH DRUG (THE IV AND
C  THE PO COLUMNS) + NADD (1 COLUMN FOR EACH ADDITIONAL COVARIATE).




	NI = 2*NDRUG + NADD
	
	IF(NI .GT. max_RS_J) THEN
  	 WRITE(*,123)
123    FORMAT(/' YOUR PATIENT DATA FILES HAVE TOO MANY COLUMNS IN '/
     1' THE DOSAGE REGIMEN BLOCK. THE NO. OF ADDITIONAL COVARIATES '/
     2' PLUS TWICE THE NO. OF DRUGS CANNOT EXCEED max_RS_J. THE '/
     3' PROGRAM IS NOW STOPPING. '/)
	 CALL PAUSE
	 STOP
	ENDIF

        READ(27,3) ND

	IF(ND .GT. 1000) THEN
	 WRITE(*,125)
  125    FORMAT(' YOUR PATIENT DATA FILES CANNOT HAVE MORE THAN 1000'/
     1' DOSE EVENTS. THE PROGRAM IS NOW STOPPING. '/)
	 CALL PAUSE
	 STOP
	ENDIF

	READ(27,*)
	READ(27,*)

        IF(ND.EQ.0) GO TO 40

	DO I = 1,ND
         READ(27,*) XJUNK,(RJUNK(J),J=1,NI)
	END DO
C  SIG AND RS NOT USED IN THIS PROGRAM, SO THEY ARE NOT READ ABOVE.
	 


C  ASSIGN THE VALUES IN EACH DRUG'S PO COLUMN TO THE CORRESPONDING

C  COLUMN IN ARRAY BS. BS NOT USED, SO COMMENT OUT FOLLOWING CODE.

C        DO I=1,ND

C         DO J=1,NDRUG
C          BS(I,J)=RS(I,2*J)
C	 END DO
C	END DO

C  READ THE NO. OF OUTPUT EQUATIONS FROM THE LINE WITH 'NO. OF TOTAL'
C  AS ENTRIES 12:23. THEN READ NO. OF OBSERVED VALUE TIMES, ETC.

   40	READ(27,1) READLINE
	IF(READLINE(12:23) .NE. 'NO. OF TOTAL') GO TO 40

	BACKSPACE(27)

        READ(27,3) NUMEQT
        READ(27,3) M

	IF(M .GT. MAXOBDIM) THEN
  	 WRITE(*,126) MAXOBDIM


  126    FORMAT(/' AT LEAST ONE OF YOUR PATIENT DATA FILES HAS TOO'/
     1' MANY OBSERVED VALUE TIMES. THIS NO. CANNOT EXCEED ',I5,'.'/
     2' THE PROGRAM IS NOW STOPPING. '/)
	 CALL PAUSE
	 STOP
	ENDIF

	IF(NUMEQT .GT. MAXNUMEQ) THEN
  	 WRITE(*,127) MAXNUMEQ 
  127    FORMAT(/' AT LEAST ONE OF YOUR PATIENT DATA FILES HAS TOO'/
     1' MANY OUTPUT EQUATION COLUMNS. THIS NO. CANNOT EXCEED ',I2/
     2' THE PROGRAM IS NOW STOPPING. '/)
	 CALL PAUSE
	 STOP
	ENDIF

        
	DO I=1,M
         READ(27,*) TO(I),(YO(I,J),J=1,NUMEQT)
	END DO


	NOBSER = M

C  AT THIS POINT, MUST SKIP THE COVARIATE INFO IN THE FILE, AND PROCEED
C  TO READ THE ASSAY NOISE COEFFICIENTS BELOW THAT.

C  READ THE NUMEQT SETS OF ASSAY COEFFICIENTS JUST BELOW THE LINE
C  WHICH HAS "ASSAY COEFFICIENTS FOLLOW" IN ENTRIES 1:25.

   50	READ(27,1) READLINE
	IF(READLINE(1:25) .NE. 'ASSAY COEFFICIENTS FOLLOW') GO TO 50

	DO IEQ = 1,NUMEQT
	 READ(27,*) C0(IEQ),C1(IEQ),C2(IEQ),C3(IEQ)
	END DO

	RETURN
	END

C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE GETNUMSF2(IINCLUDE,READLINE,NSUBB,NSUBTOT,NUMCUR,

     1    ISTOP,MAXSUB,IPATVECC)
	DIMENSION IPATVECC(MAXSUB)
	CHARACTER READLINE*1000

C  SUBROUTINE GETNUMSF2 IS CALLED BY GETIPATF (OVER AND OVER) 
C  TO ESTABLISH NSUBB AND IPATVECC, WHERE NSUBB IS THE NO. OF SUBJECTS 
C  TO BE INCLUDED (IF IINCLUDE = 1) OR EXCLUDED (IF IINCLUDE = 2) IN THE 
C  ANALYSIS, AND IPATVECC IS THE VECTOR WHICH CONTAINS THE CORRESPONDING 
C  PATIENT NOS. EACH CALL TO GETNUMSF2 EITHER ADDS TO THE VALUES IN 
C  IPATVECC (AND INCREASES NSUBB) OR STOPS THE PROCESS.

C  NOTE THAT GETNUMSF2 IS THE SAME AS GETNUMS EXCEPT THAT THE COMMENTS
C  TO THE USER ARE DIFFERENT SINCE THIS ROUTINE IS CALLED BY GETIPATF

C  WHICH IS READING A FILE, RATHER THAN KEYBOARD ENTRIES.

C  NOTE THAT ISTOP RETURNS AS 0 IF THE USER IS FINISHED ENTERING 
C  PATIENT NOS; IT RETURNS AS -1 IF THE USER HAS ENTERED A CONFLICTING 
C  SET OF PATIENT NOS. (EACH PATIENT NO. OR RANGE OF NUMBERS MUST BE
C  GREATER THAN ANY PATIENT NO. ALREADY ENTERED ... THE LARGEST PATIENT 
C  NO. ENTERED SO FAR IS NUMCUR) OR A PATIENT NO. LARGER THAN NSUBTOT 
C  (THE MAXIMUM ALLOWABLE NO. OF SUBJECTS); AND IT RETURNS AS 1 IF THE 
C  USER HAS CORRECTLY ENTERED PATIENT DATA AND WANTS TO CONTINUE 
C  ENTERING OTHER NOS.



	ISTOP = 1

C  CHECK TO SEE IF THIS IS A BLANK LINE. IF SO, RETURN ISTOP = -1 


C  WITH A MESSAGE TO THE USER.

	DO J = 1,70
	 IF(READLINE(J:J) .NE. ' ') GO TO 10

	END DO

C  TO GET HERE --> READLINE IS COMPLETELY BLANK.

	IF(NSUBB .EQ. 0) WRITE(*,1)
    1   FORMAT(/' THE INSTRUCTION OR OUTPUT FILE HAS AN IMPROPER BLANK'/

     1' LINE IN THE PATIENT NUMBER SECTION. ')
	ISTOP = -1
	RETURN
	  

   10   CONTINUE

C  CHECK TO SEE IF THIS LINE HAS JUST A 0 ON IT. IF SO, RETURN ISTOP
C  = 0, UNLESS NSUBB = 0 AND IINCLUDE = 1, IN WHICH CASE THIS IS AN 
C  ERROR. SIMPLY CHECK THE FIRST NON-BLANK CHARACTER (THERE MUST BE ONE 
C  SINCE IF THE LINE IS COMPLETELY BLANK, THE CODE ABOVE ABOVE WOULD 
C  HAVE DETECTED IT AND RETURNED CONTROL TO THE CALLING ROUTINE) AND 
C  SEE IF a. IT IS A 0, AND b. EVERY OTHER CHARACTER IS A BLANK. IF
C  a. AND b. ARE TRUE, THE LINE JUST HAS A 0 IN IT. OTHERWISE NOT.

	DO J = 1,70
	 IF(READLINE(J:J) .NE. ' ') GO TO 20
	END DO

   20   ISTART = J
	IF(READLINE(ISTART:ISTART) .NE. '0') GO TO 30

	DO I = ISTART+1,70
	 IF(READLINE(I:I) .NE. ' ') GO TO 30
	END DO
	
C  TO GET HERE --> READLINE HAS JUST A 0 ON IT AND NOTHING ELSE.

C  IF THE USER HAS ENTERED A 0 WITH NSUBB = 0 (I.E., HE HAS NOT 
C  ENTERED PREVIOUS SUBJECT NOS.), THIS IS OK IF IINCLUDE = 2 (SINCE
C  IT IS OK TO EXCLUDE 0 PATIENTS), BUT IT IS NOT OK IF IINCLUDE = 1
C  (SINCE THIS WOULD MEAN THE ANALYSIS WOULD BE ON 0 SUBJECTS).

	IF(IINCLUDE .EQ. 1 .AND. NSUBB .EQ. 0) THEN
	 WRITE(*,3)
    3    FORMAT(/' THE INSTRUCTION OR OUTPUT FILE HAS AN IMPROPER '/

     1' LINE - WITH JUST A "0" ON IT - IN THE PATIENT NUMBER SECTION.')
	 ISTOP = -1
	 RETURN

	ENDIF

	IF(IINCLUDE .EQ. 2 .OR. NSUBB .GT. 0) THEN
	 ISTOP = 0
	 RETURN
	ENDIF



   30   CONTINUE


C  TO GET TO THIS POINT MEANS READLINE HAS A POTENTIAL NO. BEGINNING
C  AT ENTRY ISTART. IF THIS IS A LEGITIMATE NO., IT ENDS THE ENTRY
C  BEFORE THE NEXT SPACE, COMMA, DASH, WHICHEVER COMES FIRST.


     	DO I = ISTART+1,70
	 IF(READLINE(I:I) .EQ. ' ' .OR. READLINE(I:I) .EQ. ',' .OR.
     1      READLINE(I:I) .EQ. '-') GO TO 40
	END DO

   40   IEND = I-1

C  CALL ROUTINE GETSUB2 WHICH OBTAINS THE SUBJECT NO. FROM THE 
C  CHARACTERS IN READLINE(ISTART:IEND).

	CALL GETSUB2(READLINE,ISTART,IEND,ISUB,IERROR)


C  IF IERROR RETURNS AS -1, IT MEANS THE USER HAS ENTERED A NON-NUMERIC
C  CHARACTER. IN THIS CASE, PRINT AN ERROR MESSAGE AND RETURN.

	IF(IERROR .EQ. -1) THEN
	 WRITE(*,7) 
    7    FORMAT(/' THE INSTRUCTION OR OUTPUT FILE HAS AN IMPROPER '/
     1' LINE - WITH AN INVALID CHARACTER ON IT - IN THE PATIENT '/
     2' NUMBER SECTION.')
	 ISTOP = -1
	 RETURN
	ENDIF



C  IF ISUB IS .LE. NUMCUR, THE LAST (AND HIGHEST PATIENT NO. ENTERED
C  PREVIOUSLY), WRITE AN ERROR MESSAGE TO THE USER AND RETURN. 
C  SIMILARLY, IF ISUB .GE. NSUBTOT.
    

	IF(ISUB .LE. NUMCUR) THEN
	 WRITE(*,4) ISUB,NUMCUR
    4    FORMAT(/' THE INSTRUCTION OR OUTPUT FILE HAS AN IMPROPER '/
     1' LINE IN IT IN THE PATIENT NUMBER SECTION.'//
     2' IT HAS A SUBJECT NO. (',I4,' ) WHICH IS LESS THAN OR EQUAL TO '/
     3' A PREVIOUSLY ENTERED SUBJECT NO. (',I4,').')
	 ISTOP = -1
	 RETURN
	ENDIF

	IF(ISUB .GT. NSUBTOT) THEN
	 WRITE(*,6) ISUB,NSUBTOT
    6    FORMAT(/' THE INSTRUCTION OR OUTPUT FILE HAS AN IMPROPER '/
     1' LINE IN IT IN THE PATIENT NUMBER SECTION.'//
     2' IT HAS A SUBJECT NO. (',I4,' ) WHICH IS GREATER THAN THE NO.'/
     3' OF SUBJECTS IN YOUR DATA FILE (',I4,').')
	 ISTOP = -1
	 RETURN
	ENDIF

C  TO GET TO THIS POINT, THE PATIENT NO., ISUB, IS LEGITIMATE. I.E.,
C  IT IS .GT. NUMCUR AND .LE. NSUBTOT. THERE ARE 4 POSSIBILITIES:

C  1. IF THERE ARE NO OTHER ENTRIES IN READLINE PAST THIS POINT,
C  ISUB IS A SINGLE PATIENT NO.
C  2. IF THE NEXT NON-BLANK ENTRY IN READLINE IS A COMMA OR ANOTHER
C  NUMBER, ISUB IS A SINGLE PATIENT NO.
C  3. IF THE NEXT NON-BLANK ENTRY IN READLINE IS A DASH, ISUB IS THE
C  START OF A PATIENT RANGE.
C  4. IF THE NEXT NON-BLANK ENTRY IN READLINE IS ANOTHER CHARACTER
C  (I.E., NOT A NUMBER, DASH, OR COMMA), THE LINE HAS BEEN ENTERED
C  INCORRECTLY BY THE USER.


C  CHECK FOR NO. 1. ABOVE ...

	DO I = IEND+1,70
	 IF(READLINE(I:I) .NE. ' ') GO TO 50
	END DO

C  TO GET TO THIS POINT, ISUB IS AN INDIVIDUAL SUBJECT NO., AND THE
C  LAST NO. ON THE LINE. INCREASE NSUBB, NUMCUR, AND PUT THIS SUBJECT

C  NO. INTO IPATVECC BEFORE RETURNING.

	NUMCUR = ISUB
	NSUBB = NSUBB + 1
	IPATVECC(NSUBB) = ISUB
	RETURN


   50   CONTINUE

C  CHECK FOR NOS. 2,3, OR 4 (SEE ABOVE).

C  TO GET TO THIS POINT, THERE IS AT LEAST ONE CHARACTER ENTRY AFTER 
C  ISUB, AND THE FIRST OCCURS AT LOCATION I. IF THIS CHARACTER IS A
C  COMMA THEN ISUB IS AN INDIVIDUAL SUBJECT NO. 

	IF(READLINE(I:I) .EQ. ',') THEN


	 NUMCUR = ISUB
 	 NSUBB = NSUBB + 1
	 IPATVECC(NSUBB) = ISUB

C  CHECK TO SEE IF THERE IS ANOTHER ENTRY ON THIS LINE. IF SO,

C  RETURN CONTROL TO LABEL 30 AND CONTINUE CHECKING FOR OTHER NOS. IF 
C  NOT, IT MEANS THE USER HAS ENDED THE LINE WITH A COMMA ... WHICH 
C  IS ASSUMED TO BE SUPERFLOUS.

	 DO J = I+1,70
	  IF(READLINE(J:J) .NE. ' ') GO TO 60
	 END DO

	 RETURN


   60    ISTART = J
	 GO TO 30	

	ENDIF



C  TO GET TO THIS POINT, THERE IS AT LEAST ONE CHARACTER ENTRY AFTER 

C  ISUB, AND THE FIRST OCCURS AT LOCATION I ... AND THIS ENTRY IS NOT
C  A COMMA. CHECK TO SEE IF IT IS ANOTHER NUMBER. IN THIS CASE, ISUB
C  IS AN INDIVIDUAL SUBJECT NO. 

	INUM = 0
	IF(READLINE(I:I) .EQ. '0') INUM = 1
	IF(READLINE(I:I) .EQ. '1') INUM = 1
	IF(READLINE(I:I) .EQ. '2') INUM = 1
	IF(READLINE(I:I) .EQ. '3') INUM = 1
	IF(READLINE(I:I) .EQ. '4') INUM = 1
	IF(READLINE(I:I) .EQ. '5') INUM = 1
	IF(READLINE(I:I) .EQ. '6') INUM = 1

	IF(READLINE(I:I) .EQ. '7') INUM = 1
	IF(READLINE(I:I) .EQ. '8') INUM = 1
	IF(READLINE(I:I) .EQ. '9') INUM = 1

	IF(INUM .EQ. 1) THEN
	 NUMCUR = ISUB
 	 NSUBB = NSUBB + 1
	 IPATVECC(NSUBB) = ISUB

C  RETURN CONTROL TO LABEL 30 AND CONTINUE CHECKING FOR OTHER NOS.
C  STARTING WITH LOCATION I.

         ISTART = I
	 GO TO 30	

	ENDIF


C  TO GET TO THIS POINT, THERE IS AT LEAST ONE CHARACTER ENTRY AFTER 
C  ISUB, AND THE FIRST OCCURS AT LOCATION I ... AND THIS ENTRY IS NOT
C  A COMMA OR A NUMBER. CHECK TO SEE IF IT IS A DASH. IN THIS CASE,
C  ISUB IS THE FIRST NO. IN A RANGE OF PATIENT NUMBERS.

	IF(READLINE(I:I) .EQ. '-') THEN


C  STORE ISUB INTO NUMCUR1, BUT NOT NUMCUR. IN CASE THE USER
C  HAS NOT ENTERED A LEGITIMATE NO. AFTER THE DASH, KEEP THE PREVIOUS
C  VALUE OF NUMCUR (THE LAST PATIENT INDEX PUT INTO IPATVECC) INTACT.

	 NUMCUR1 = ISUB


C  READ UNTIL THE NEXT NON-BLANK CHARACTER, WHICH SHOULD BE THE 
C  BEGINNING OF THE NUMBER WHICH ENDS THE RANGE.


	 DO J = I+1,70
	  IF(READLINE(J:J) .NE. ' ') GO TO 70
	 END DO

C  TO GET TO THIS POINT MEANS THE USER ENDED A LINE WITH A DASH, WHICH
C  IS NOT ALLOWED. WRITE AN ERROR MESSAGE AND RETURN.

	 WRITE(*,8) 
    8    FORMAT(/' THE INSTRUCTION OR OUTPUT FILE HAS AN IMPROPER '/
     1' LINE IN IT IN THE PATIENT NUMBER SECTION.'//
     2' A LINE HAS BEEN ENDED WITH A DASH.')
	 ISTOP = -1
	 RETURN

   70   ISTART = J

C  TO GET TO THIS POINT, THERE IS AN ENTRY IN LOCATION ISTART, WHICH
C  SHOULD BE THE BEGINNING OF THE ENDING PATIENT NO. OF A RANGE OF
C  PATIENT NOS. THIS NUMBER ENDS THE ENTRY BEFORE A SPACE OR A 
C  COMMA.


     	DO K = ISTART+1,70
	 IF(READLINE(K:K) .EQ. ' ' .OR. READLINE(K:K) .EQ. ',') 
     1    GO TO 80

	END DO

   80   IEND = K-1


C  CALL ROUTINE GETSUB2 WHICH OBTAINS THE SUBJECT NO. FROM THE 
C  CHARACTERS IN READLINE(ISTART:IEND).

	CALL GETSUB2(READLINE,ISTART,IEND,ISUB,IERROR)

C  IF IERROR RETURNS AS -1, IT MEANS THE USER HAS ENTERED A NON-NUMERIC
C  CHARACTER. IN THIS CASE, PRINT AN ERROR MESSAGE AND RETURN.

	IF(IERROR .EQ. -1) THEN
	 WRITE(*,7) 
	 ISTOP = -1
	 RETURN
	ENDIF

C  IF ISUB IS .LE. NUMCUR1 (THE BEGINNING NO. IN THIS RANGE), WRITE AN 
C  ERROR MESSAGE TO THE USER AND RETURN. SIMILARLY, IF ISUB .GE. 
C  NSUBTOT
    
	IF(ISUB .LE. NUMCUR1) THEN

	 WRITE(*,9)

    9    FORMAT(/' THE INSTRUCTION OR OUTPUT FILE HAS AN IMPROPER '/
     1' LINE IN IT IN THE PATIENT NUMBER SECTION.'//
     2' IT HAS A RANGE OF SUBJECT NOS. WITH THE ENDING NO. LESS THAN '/
     3' OR EQUAL TO THE BEGINNING NO.')
	 ISTOP = -1
	 RETURN
	ENDIF

	IF(ISUB .GT. NSUBTOT) THEN
	 WRITE(*,6) ISUB,NSUBTOT
	 ISTOP = -1
	 RETURN
	ENDIF


C  TO GET TO THIS POINT MEANS THE USER HAS CORRECTLY ENTERED A RANGE
C  OF PATIENT NOS. FROM NUMCUR1 TO ISUB. UPDATE NSUBB, NUMCUR, AND
C  IPATVECC.

	 NUMCUR = ISUB
	 NN = NSUBB
 	 NSUBB = NSUBB + (NUMCUR - NUMCUR1) + 1

	 NONEW = 0
	 DO K = NN+1,NSUBB

	  NONEW = NONEW + 1
	  IPATVECC(K) = NUMCUR1 - 1 + NONEW

	 END DO

C  CHECK TO SEE IF THERE IS ANOTHER CHARACTER ON THE LINE AFTER
C  LOCATION IEND (IGNORE A COMMA AT THIS POINT, SINCE IT IS POSSIBLE
C  THAT THE USER HAS PUT IN A COMMA AT THE END OF HIS SUBJECT RANGE). 
C  IF SO, RETURN CONTROL TO LABEL 30 AND CONTINUE CHECKING FOR OTHER 
C  NOS. IF NOT, RETURN.

	 DO J = IEND+1,70
	  IF(READLINE(J:J) .NE. ' ' .AND. READLINE(J:J) .NE. ',' ) 
     1    GO TO 90
	 END DO

	 RETURN

   90    ISTART = J
	 GO TO 30	

	ENDIF


C  THE ABOVE ENDIF IS FOR THE  IF(READLINE(I:I) .EQ. '-')  CONDITION.


C  TO GET TO THIS POINT, THERE IS AT LEAST ONE CHARACTER ENTRY AFTER 
C  ISUB, AND THE FIRST OCCURS AT LOCATION I ... AND THIS ENTRY IS NOT

C  A COMMA, A NUMBER, OR A DASH, WHICH MEANS IT IS AN ERRONEOUS ENTRY.
C  WRITE AN ERROR MESSAGE TO THE USER AND RETURN.

	WRITE(*,7) 
	ISTOP = -1


	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE GETSUB2(READLINE,ISTART,IEND,ISUB,IERROR)
	CHARACTER READLINE*1000

C  THIS ROUTINE, CALLED BY GETNUMS, OBTAINS THE SUBJECT NO., ISUB, 
C  FROM THE CHARACTERS IN READLINE(ISTART:IEND).


    3   FORMAT(A1000)

	IERROR = 0

  	ISIZE = IEND-ISTART


	ISUB = 0
	 DO K=ISTART,IEND
	  IVAL = -9
	  IF(READLINE(K:K) .EQ. '0') IVAL = 0
	  IF(READLINE(K:K) .EQ. '1') IVAL = 1
	  IF(READLINE(K:K) .EQ. '2') IVAL = 2
	  IF(READLINE(K:K) .EQ. '3') IVAL = 3
	  IF(READLINE(K:K) .EQ. '4') IVAL = 4
	  IF(READLINE(K:K) .EQ. '5') IVAL = 5
	  IF(READLINE(K:K) .EQ. '6') IVAL = 6
	  IF(READLINE(K:K) .EQ. '7') IVAL = 7
	  IF(READLINE(K:K) .EQ. '8') IVAL = 8
	  IF(READLINE(K:K) .EQ. '9') IVAL = 9

	  IF(IVAL .EQ. -9) THEN
	   IERROR = -1
	   RETURN
	  ENDIF


	  ISUB = ISUB + IVAL*10**ISIZE
	  ISIZE = ISIZE-1
	 END DO

	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE CONVERGE2(NCYCLE,XLOGLIK,XMEAN,STDEV,INDXSD,AICBIC, 
     1   PRCFVR,ACTPTS,SCALNFO,GAMLAM,AGE,HEIGHT,
     2   SUBMEAN,SUBLOGLIK,SUBSTD,SUBPERCOF,
     3   NAME,CHARTNO,SEX,NDD,NI,ASSAYC,IERRMOD)

C  THIS IS AN EDITED VERSION OF SUBROUTINE CONVERGE IN NPBIG15E.FOR. 
C  THIS ROUTINE JUST INPUTS AND STORES VALUES FROM THE OUTPUT FILE OF A
C  BIG NPAG RUN. IT DOESN'T PLOT THOSE VALUES; IT RETURNS THEM TO MAIN.


C  THIS SUBROUTINE IS CALLED BY MAIN.

C  INPUT IS:

C  FILE 25, THE OUTPUT FILE, ALREADY OPENED.


C  OUTPUT IS:

C  THE VALUES IN THE ARGUMENTS TO THIS SUBROUTINE.

        use npag_utils, only: maxnumeq,max_pop_rand_varbs

        IMPLICIT REAL*8(A-H,O-Z)
C        PARAMETER(MAXNUMEQ=7)

        CHARACTER READLINE*1000,NAME(800)*53,CHARTNO(800)*53,SEX(800)*1
C  AS OF READ5.F, PAR(max_pop_rand_vrsbs)*11 REMOVED IN CHARACTER STMT. PAR IS NOT USED
C  IN THIS ROUTINE.

        DIMENSION XLOGLIK(9997),XMEAN(9997,max_pop_rand_varbs),
     1   AICBIC(9997,2),
     2   STDEV(9997,max_pop_rand_varbs),PRCFVR(9997,max_pop_rand_varbs),
     3   ACTPTS(9997),SCALNFO(9997),GAMLAM(9997), 
     4   AGE(800),HEIGHT(800),SUBMEAN(800,max_pop_rand_varbs),
     5   SUBLOGLIK(800),
     5   SUBSTD(800,max_pop_rand_varbs),
     6   SUBPERCOF(800,max_pop_rand_varbs),
     7   NDD(800),ASSAYC(800,MAXNUMEQ,4)

C  NOTE THAT THE DIMENSIONS FOR MAXSUB AND MAXOBDIM HAVE BEEN HARDCODED
C  TO BE, RESPECTIVELY, 800 AND 150. THIS COULD BE AVOIDED BY PROVIDING 
C  THESE VALUES IN THE ARGUMENT LIST OF THIS ROUTINE. NOTE  THAT THE
C  MAXIMUM ALLOWABLE VALUE FOR NI (2*NDRUG + NADD) IS max_RS_J --> LAST 
C  DIMENSION OF DOSEBLOCK IS (1 + max_RS_J). NOTE ALSO THAT THE MAX. NO.
C  OF DOSE EVENTS IS 1000. BUT, AS OF read17.f, DOSEBLOCK IS PASSED
C  IN COMMON/DOSEOBS (ALONG WITH OBSBLOCK) FROM SUBROUTINE NEWWORK1
C  OF npageng18.f, RATHER THAN READ FROM FILE 27 BELOW.

        REWIND(25)

    2   FORMAT(A1000)


C  THE 1ST LINE OF THE OUTPUT FILE HAS ALREADY BEEN CHECKED IN MAIN TO
C  VERIFY THAT IT IS VERSION 40 OR NEWER. SO PROCEED TO READING IN THE
C  OUTPUT VALUES.


C OBTAIN THE NAME AND NUMBER OF THE PATIENT DATA FILES, THE NO. OF 

C VARIABLES, AND THE VARIABLE NAMES. 


        ILOC=2

   50   READ(25,2) READLINE

        IF(ILOC .EQ. 2) GO TO 202
        IF(ILOC .EQ. 3) GO TO 203


C  THE FOLLOWING READS IN NVAR FROM MXEM2N42 AND LATER PROGRAMS.

  202   IF(READLINE(2:29) .EQ. 'STATISTICS FOR THE VARIABLES') THEN
	  BACKSPACE(25)

	  BACKSPACE(25)

	  READ(25,53) NVAR
   53     FORMAT(T6,I2)

	  ILOC=3
	  REWIND(25)
	ENDIF

C  THE FOLLOWING READS IN NVAR FROM MXEM2N41 AND EARLIER PROGRAMS.

        IF(READLINE(2:16) .EQ. 'THE FOLLOWING  ') THEN
	  BACKSPACE(25)
	  READ(25,531) NVAR
  531     FORMAT(T17,I1)
	  ILOC=3
	  REWIND(25)
	ENDIF

	GO TO 50



  203   IF(READLINE(2:21) .EQ. 'THE RANDOM VARIABLES' .OR.
     1  READLINE(3:22) .EQ. 'THE RANDOM VARIABLES') THEN
	  DO I=1,NVAR
	    READ(25,*)

C  PAR(I) NOT USED IN THIS ROUTINE, SO IT IS NOT READ IN BELOW.

	    READ(25,*)
	  END DO
C   54 FORMAT(T2,A11)  
	    GO TO 60 
	ENDIF
	GO TO 50


C  AS OF NPBIG6, EACH LINE READ WILL BE
C  CHECKED AGAINST ALL POSSIBLE VALUES TO BE INPUT ... TO AVOID
C  THE SITUATION WHERE A VARIANCE .LE. 0 --> NOTHING IS READ AGAIN 
C  UNTIL THE NEXT CYCLE WHERE ALL VARIANCES ARE > 0.
 
 
C  INDXLOG IS THE RUNNING INDEX OF THE NO. OF VALUES CURRENTLY IN 
C  XLOGLIK. SIMILARY INDAICBIC, INDXPTS, INDXNFO, INDXMEAN, INDXSD, 
C  AND INDXPRCF, ARE THE INDICES OF THE NO. OF VALUES CURRENTLY IN,
C  RESPECTIVELY, AICBIC, ACTPTS, SCALNFO, XMEAN, STDEV, PRCFVR. ALSO,
C  INDX IS THE RUNNING INDEX OF THE NO. OF VALUES IN CYCLE.



C  NOTE THAT CYCLE(INDX) IS THE REAL-VALUED CYCLE NO.
 
   60	INDXLOG = 0
      INDAICBIC = 0
	INDXPTS = 0
	INDXNFO = 0
	INDXMEAN = 0
	INDXSD = 0
	INDXPRCF = 0
      INDGAM = 0


   10	READ(25,2,IOSTAT=IEND) READLINE

C  NOTE THAT IEND .LT. 0 --> END OF FILE REACHED.
C  TO REACH THE END OF THE FILE AT THIS POINT --> SOMETHING IS WRONG 
C  WITH THE FILE SINCE THE BAYESIAN RESULTS HAVEN'T BEEN READ IN.

        IF(IEND .LT. 0) THEN
         WRITE(*,217)
  217    FORMAT(/' SOMETHING IS WRONG WITH THE OUTPUT FILE ENTERED;'/
     1' THERE ARE NO BAYESIAN RESULTS. THE PROGRAM STOPS.'//)
         CALL PAUSE
         STOP
        ENDIF


C  CHECK TO SEE IF THIS LINE BEGINS THE BAYESIAN RESULTS. IF SO, GO TO
C  LABEL 100 AND READ THE BAYESIAN INFO.

      IF(READLINE(2:31) .EQ. 'THE BAYESIAN POSTERIOR DENSITY') GO TO 100
 

C  AS OF NPLIT1.FOR, THERE ARE NOW 3 POSSIBLE FORMATS THAT THE
C  LOG-LIK CAN BE READ IN.
 
        IF(READLINE(2:12) .EQ. 'THE LOG-LIK') THEN
	 INDXLOG = INDXLOG+1
	 READ(25,*)
	 READ(25,*) XLOGLIK(INDXLOG)
	 GO TO 10
	ENDIF
 
	IF(READLINE(11:41) .EQ. '(NUMERICAL) LOG-LIKELIHOOD (USI') THEN
	 INDXLOG = INDXLOG+1
	 READ(25,*)
	 READ(25,*)
	 READ(25,*) XLOGLIK(INDXLOG)
	 GO TO 10
	ENDIF
 
	IF(READLINE(11:41) .EQ. '(NUMERICAL) LOG-LIKELIHOOD OF T') THEN	 
	 INDXLOG = INDXLOG+1
	 READ(25,*)
	 READ(25,*) XLOGLIK(INDXLOG)
	 GO TO 10
	ENDIF
 

	IF(READLINE(2:11) .EQ. 'THE AKAIKE') THEN	 

	 INDAICBIC = INDAICBIC+1
	 READ(25,*) (AICBIC(INDAICBIC,J),J=1,2)

	 GO TO 10
	ENDIF


C  AS OF NPBIG6.FOR, ALL REFERENCE TO "DIFF" IS REMOVED.


        IF(READLINE(3:19) .EQ. 'THE NO. OF ACTIVE' .OR.
     1   READLINE(2:18) .EQ. 'THE NO. OF ACTIVE') THEN
         INDXPTS = INDXPTS+1 
         BACKSPACE(25)
         READ(25,57) ACTPTS(INDXPTS)
   57    FORMAT(T39,G30.0)
         GO TO 10
        ENDIF
 
 
        IF(READLINE(2:11) .EQ. 'THE SCALED') THEN


         INDXNFO = INDXNFO+1 

C  IF A % SIGN IS IN COL. 43, THIS IS AN OLD OUTPUT FILE AND MUST BE
C  READ IN WITH A DIFFERENT FORMAT.
 
	 IOLDER = 0
	 IF(READLINE(43:43) .EQ. '%') IOLDER = 1
 
	 BACKSPACE(25)
	 IF(IOLDER .EQ. 0) READ(25,58) SCALNFO(INDXNFO)
	 IF(IOLDER .EQ. 1) READ(25,158) SCALNFO(INDXNFO)
 
   58    FORMAT(T36,F10.2)
  158    FORMAT(T36,F6.2)
	 GO TO 10
 
	ENDIF


        IF(READLINE(2:10) .EQ. 'THE MEANS' .OR.
     1     READLINE(3:11) .EQ. 'THE MEANS') THEN
           INDXMEAN = INDXMEAN+1 

 
C  IF THIS IS AN 'OLD' OUTPUT FILE (E.G., FROM MXEM2N10.EXE), THE VALUES
C  FOR THE MEANS ARE ON THE SAME LINE AS 'THE MEANS'. FOR NEWER OUTPUT
C  FILES, THE VALUES ARE TWO LINES DOWN. IDENTIFY WHICH TYPE OF OUTPUT
C  FILE THIS IS: SET IOLD = 1 FOR NEWER OUTPUT FILES; IOLD = -1 FOR
C  OLDER VERSIONS.
 
	IF(READLINE(20:30) .NE. '           ') THEN
 
C  THIS IS AN 'OLD' OUTPUT FILE (SEE COMMENTS ABOVE).
 
	  IOLD=-1
	  BACKSPACE(25)
	  READ(25,19) (XMEAN(INDXMEAN,J),J=1,NVAR)
   19   FORMAT(T17,7G13.6)
 
	ELSE
 
C  THIS IS A NEWER OUTPUT FILE.
 
	  IOLD=1

	  READ(25,*)
	  READ(25,*) (XMEAN(INDXMEAN,J),J=1,NVAR)

 
	ENDIF


	GO TO 10
 
	ENDIF
 
C  ABOVE ENDIF IS FOR THE IF/THEN REGARDING THE MEANS.

 
        IF(READLINE(2:28) .EQ. 'THE STANDARD DEVIATIONS ARE') THEN

C  NOTE THAT IT IS POSSIBLE FOR SOME CYCLES NOT TO HAVE STD. DEVS.
C  WRITTEN (IN THOSE SITUATIONS WHERE NUMERICAL ISSUES RESULTED IN
C  SINGULAR COV. MATRICES. SAME FOR PERCENT COEFF. OF VARIATIONS
C  BELOW.


         INDXSD = INDXSD+1 
	 
C  IF THIS IS A NEWER FILE, THE STD. DEV'S ARE TWO LINES DOWN; IF THIS
C  IS AN OLDER FILE, THE STD. DEV'S ARE ON THE NEXT LINE (SEE COMMENTS
C  BELOW LABEL 104).
 
	 IF(IOLD .EQ. 1) READ(25,*)
	 READ(25,*) (STDEV(INDXSD,J),J=1,NVAR)
 
	 GO TO 10
 
	ENDIF


        IF(READLINE(2:12) .EQ. 'THE PERCENT') THEN

	 INDXPRCF = INDXPRCF+1 
 
C  IF THIS IS A NEWER FILE, THE %-COEFF'S ARE TWO LINES DOWN; IF THIS
C  IS AN OLDER FILE, THE %-COEFF'S ARE ON THE NEXT LINE (SEE COMMENTS
C  BELOW LABEL 104).
 
	 IF(IOLD .EQ. 1) READ(25,*)
	 READ(25,*) (PRCFVR(INDXPRCF,J),J=1,NVAR)
	 GO TO 10
 
	ENDIF


        IF(READLINE(2:12) .EQ. 'IERRMOD AND') THEN
         INDGAM = INDGAM+1
         READ(25,*) IERRMOD,GAMLAM(INDGAM)
        ENDIF



	GO TO 10

  100   NCYCLE = INDXLOG

C  NCYCLE IS RETURNED TO THE CALLING PROGRAM AS THE NO. OF CYCLES IN 
C  THE OUTPUT FILE.

C  INITIALIZE INDSUB TO BE 0. IT IS THE RUNNING NO. OF SUBJECTS IN THE
C  OUTPUT FILE.



        INDSUB = 0



C  READ IN BAYESIAN INFO FROM HERE ON, FOR EACH OF THE SUBJECTS IN THE 
C  FILE.

  110	  READ(25,2,IOSTAT=IEND) READLINE

C  NOTE THAT IEND .LT. 0 --> END OF FILE REACHED. IN THIS CASE, GO TO
C  LABEL 200 TO READ IN PATIENT INFO FROM SCRATCH FILE 27.


        IF(IEND .LT. 0) GO TO 200


C  READ IN THE BAYESIAN LOG-LIK FOR THIS SUBJECT.

        IF(READLINE(11:41) .EQ. '(NUMERICAL) LOG-LIKELIHOOD OF T') THEN
         INDSUB = INDSUB + 1
         READ(25,*)

         READ(25,*)
         READ(25,*) SUBLOGLIK(INDSUB)


         GO TO 110
        ENDIF

C  READ IN THE BAYESIAN MEANS FOR THIS SUBJECT.

        IF(READLINE(2:10) .EQ. 'THE MEANS' .OR.
     1     READLINE(3:11) .EQ. 'THE MEANS') THEN
         READ(25,*)
         READ(25,*) (SUBMEAN(INDSUB,J),J=1,NVAR)
         GO TO 110
        ENDIF

C  READ IN THE BAYESIAN STD. DEVS. FOR THIS SUBJECT.
C  FIRST SET EACH STD. DEV. = -99. THAT WAY, IF THE STD. DEVs ARE NOT

C  IN THE FILE BECAUSE THE VARIANCE FOR ONE OF THE PARAMETERS IS
C  NUMERICALLY .LE. 0, SET THE VALUES IN SUBSTD = -99, THE MISSING VALUE
C  INDICATOR. SIMILARLY SET THE VALUES FOR SUBPERCOF = -99.


        IF(READLINE(2:27) .EQ. 'THE VARIANCE FOR PARAMETER') THEN
         DO J = 1,NVAR
          SUBSTD(INDSUB,J) = -99.D0
          SUBPERCOF(INDSUB,J) = -99.D0
         END DO
        ENDIF

        IF(READLINE(2:13) .EQ. 'THE STANDARD') THEN
         READ(25,*)
         READ(25,*) (SUBSTD(INDSUB,J),J=1,NVAR)
         GO TO 110
        ENDIF


C  READ IN THE BAYESIAN PERCENT COEF. OF VAR. FOR THIS SUBJECT

        IF(READLINE(2:13) .EQ. 'THE PERCENT ') THEN
         READ(25,*)
         READ(25,*) (SUBPERCOF(INDSUB,J),J=1,NVAR)
         GO TO 110
        ENDIF


        GO TO 110
C  THE ABOVE GO TO 110 STATEMENT WAS ACCIDENTALLY REMOVED IN THE
C  CODE CHANGES IN read20.f. IT HAS BEEN PUT BACK IN IN read21.f.


  200   REWIND(27)


        INDSUB = 0

C  READ IN PATIENT DATA INFO FROM HERE ON, FOR EACH OF THE SUBJECTS IN 
C  THE FILE.


  210	  READ(27,2,IOSTAT=IEND) READLINE



c  As of read18.f, test for reaching the end of file 27, rather
c  than reading 'END OF THE PATIENT' in entries 19:36. The reason is
c  that file 27 comes straight from the main npageng20.f program, and
c  has no 'END OF THE PATIENT ...' line.


C  NOTE THAT IEND .LT. 0 --> END OF FILE REACHED.

        IF(IEND .LT. 0) RETURN

        
        IF(READLINE(3:16) .EQ. 'LAST AND FIRST') THEN
         INDSUB = INDSUB + 1
         NAME(INDSUB) = READLINE(28:80)
         READ(27,2) READLINE
         CHARTNO(INDSUB) = READLINE(18:70)
          DO I = 1,5
           READ(27,*)
          END DO
         READ(27,*) AGE(INDSUB)
         READ(27,2) READLINE
         SEX(INDSUB) = READLINE(1:1)
         READ(27,*) HEIGHT(INDSUB)
         GO TO 210
        ENDIF

        IF(READLINE(12:23) .EQ. 'NO. OF DRUGS') THEN

C  READLINE NOW CONTAINS THE NO. OF DRUGS, NDRUG. BACKSPACE AND READ 
C  NDRUG; THEN READ THE NO. OF ADDITIONAL COVARIATES, THE NO. OF
C  DOSE EVENTS, AND THE DOSAGE BLOCK.
C  NO. AS OF read17.f, DOSEBLOCK AND OBSBLOCK ARE NO LONGER READ
C  FROM FILE 27 BELOW (THEY ARE PASSED IN COMMON/DOSEOBS VIA
C  SUBROUTINE NEWWORK1 IN npageng18.f). 

         BACKSPACE(27)
    3    FORMAT(T2,I5)
         READ(27,3) NDRUG
         READ(27,3) NADD

C  NOTE THAT THE NO. OF "RATES" INCLUDES 2 FOR EACH DRUG (THE IV AND
C  THE PO COLUMNS) + NADD (1 COLUMN FOR EACH ADDITIONAL COVARIATE).

         NI = 2*NDRUG + NADD
         READ(27,3) ND
         NDD(INDSUB) = ND
        
         GO TO 210

        ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(READLINE(12:23) .EQ. 'NO. OF DRUGS')
C  CONDITION.


        IF(READLINE(12:23) .EQ. 'NO. OF TOTAL') THEN


C  READLINE NOW CONTAINS THE NO. OUTPUT EQS. BACKSPACE AND READ NUMEQT;
C  THEN READ M (THE NO. OF OBSERVED VALUE TIMES) AND THEN THE OBSERVED
C  VALUES BLOCK. NOTE THAT THIS BLOCK HAS M ROWS AND 1 + NUMEQT COLUMNS 
C  (THE 1 IS FOR THE TIME COLUMN).
C  NO. AS OF read17.f, DOSEBLOCK AND OBSBLOCK ARE NO LONGER READ
C  FROM FILE 27 BELOW (THEY ARE PASSED IN COMMON/DOSEOBS VIA
C  SUBROUTINE NEWWORK1 IN npageng18.f). 

 
         BACKSPACE(27)

         READ(27,3) NUMEQT
         READ(27,3) M

         GO TO 210

        ENDIF


C  THE ABOVE ENDIF IS FOR THE IF(READLINE(12:23) .EQ. 'NO. OF TOTAL') 
C  CONDITION.


        IF(READLINE(1:25) .EQ. 'ASSAY COEFFICIENTS FOLLOW') THEN

C  READ THE ASSAY COEFFICIENTS ON THE NEXT NUMEQT ROWS, ONE FOR EACH
C  OF THE OUTPUT EQUATIONS.

         DO J = 1,NUMEQT
          READ(27,*) (ASSAYC(INDSUB,J,K),K=1,4)
         END DO

         GO TO 210

        ENDIF

C  THE ABOVE ENDIF IS FOR THE 
C  IF(READLINE(1:25) .EQ. 'ASSAY COEFFICIENTS FOLLOW') CONDITION.



        GO TO 210

c  As of read18.f, don't need the following stmt. RETURN occurs above
c  when EOF is reached for file 27.
c  400   RETURN
C  THE CODE TRANSFERS TO LABEL 400 WHEN ALL THE PATIENT DATA HAS BEEN 
C  READ IN.

        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  SUBROUTINE GETPATH(PATH,NOB) REMOVED FOR READ6.F.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE GETCOVR2(NCOV,COVDESCR)
      USE npag_utils, only: max_covs
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER READLINE*1000,COVDESCR(max_covs)*20

C  THIS ROUTINE IS CALLED BY MAIN TO OBTAIN, FROM FILE 27, THE NO. OF
C  COVARIATES (NCOV), AND THEIR NAMES(COVDESCR(I),I=1,NCOV). THIS ROUTINE
C  IS AN EDITED VERSION OF GETCOVR2 IN NPBG15E1.FOR.

    2   FORMAT(A20)
   33   FORMAT(A1000)

      REWIND(27)

C  THE NO. OF ADDITIONAL COVARIATES IS ON THE LINE HAVING 
C  "NO. OF ADDITIONAL COVARIATES" STARTING IN ENTRY 12.

   10   READ(27,33) READLINE
        IF(READLINE(12:28) .NE. 'NO. OF ADDITIONAL') GO TO 10
        BACKSPACE(27)
    3   FORMAT(T2,I5)
        READ(27,3) NADD

C  THE TOTAL NO. OF COVARIATES, NOT COUNTING AGE, SEX, HEIGHT, AND 
C  ETHNICITY FLAG AT THE TOP OF EACH SUBJECT'S PATIENT FILE, WILL BE
C  NADD.

	NCOV = NADD

C  READ THE NCOV COVARIATE NAMES FROM BELOW THE LINE HAVING 
C  "COVARIATE NAMES" STARTING IN COLUMN 2.

   20   READ(27,33) READLINE
        IF(READLINE(2:16) .NE. 'COVARIATE NAMES') GO TO 20


        IF(NCOV .GE. 1) THEN
  
         DO J = 1,NCOV

          READ(27,33) READLINE

C  FOR THIS COVARIATE, ESTABLISH COVDESCR(J) AS THE PORTION OF THE 
C  CURRENT READLINE UP TO THE  FIRST SPACE, WHICH WILL BE ASSUMED TO BE 
C  THE END OF THE COV. NAME. IN CASE THERE IS A SPACE OR TWO AT THE 
C  BEGINNING OF READLINE FOR SOME REASON, START CHECKING FOR SPACES AT 
C  ENTRY 3.

          DO I = 3,20
           IF(READLINE(I:I) .EQ. ' ') GO TO 30
          END DO

   30     COVDESCR(J) = READLINE(1:I-1)

         END DO

        ENDIF


	REWIND(27)


	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE GETICYCSTART(ICYCSTART)

 
	CHARACTER READLINE*1000

C  THIS ROUTINE IS A VARIATION OF TEST38.FOR, WHICH READS AN INTEGER
C  ANYWHERE ON A GIVEN LINE WITH A PARTICULAR SET OF WORDS AT THE START 
C  OF THAT LINE.

    2   FORMAT(A1000)


C  READ UNTIL THE LINE WHICH HAS THE WORDS 'LE NO. FOR THIS RUN IS' ON
C  IT SOMEWHERE.

   10 READ(25,2) READLINE
	ILINE=0
	 DO I=1,51
	  IF(READLINE(I:I+21) .EQ. 'LE NO. FOR THIS RUN IS') THEN
	   ILINE=1
	   GO TO 20
	  ENDIF
	 END DO
   20 IF(ILINE .EQ. 0) GO TO 10

C  SOMEWHERE AFTER THE CHARACTERS IN ENTRIES I:I+21 IS THE INTEGER.
C  READ THE CHARACTERS FOR THIS INTEGER, AND THEN CONVERT IT TO
C  AN INTEGER VALUE. AFTER THE FOLLOWING LOOP, THESE CHARACTERS WILL BE
C  IN READLINE(ISTART:IEND).

	 IEND = 0
	 ISTART = 0

	  DO J = I+22, 72
	   IF(ISTART .EQ. 0 .AND. READLINE(J:J) .NE. ' ') ISTART = J
	   IF(ISTART .NE. 0 .AND. READLINE(J:J) .EQ. ' ') THEN
	    IEND = J-1
	    GO TO 30
	   ENDIF
	  END DO

C  CHECK TO MAKE SURE THAT THE NO. OF CHARACTERS READ IN FOR THE INTEGER
C  IS NOT MORE THAN 6 (I.E., 6 CHARACTERS ALLOW A MAXIMUM SIZE OF 999999
C  WHICH IS MORE THAN THE LARGEST VALUE THIS INTEGER CAN BE).

   30	ISIZE = IEND-ISTART

        IF(ISIZE .GT. 5) THEN

         WRITE(*,31) 
   31    FORMAT(/' THE STARTING CYCLE NO IS LARGER THAN 999999, WHICH'/
     1' IS TOO LARGE.'//
     1' THE PROGRAM STOPS. ')
	   CALL PAUSE
         STOP
        ENDIF

C  CONVERT AS INDICATED ABOVE.

	ICYCSTART = 0
	 DO K=ISTART,IEND
	  IF(READLINE(K:K) .EQ. '0') IVAL = 0
	  IF(READLINE(K:K) .EQ. '1') IVAL = 1
	  IF(READLINE(K:K) .EQ. '2') IVAL = 2
	  IF(READLINE(K:K) .EQ. '3') IVAL = 3
	  IF(READLINE(K:K) .EQ. '4') IVAL = 4
	  IF(READLINE(K:K) .EQ. '5') IVAL = 5
	  IF(READLINE(K:K) .EQ. '6') IVAL = 6
	  IF(READLINE(K:K) .EQ. '7') IVAL = 7
	  IF(READLINE(K:K) .EQ. '8') IVAL = 8
	  IF(READLINE(K:K) .EQ. '9') IVAL = 9
	  ICYCSTART = ICYCSTART + IVAL*10**ISIZE
	  ISIZE = ISIZE-1
	 END DO


	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C
        SUBROUTINE CONDENSE2(READLINE)
        CHARACTER READLINE*80        

C  SUBROUTINE CONDENSE2 IS CALLED BY MAIN TO WRITE READLINE WITH AS 
C  SMALL A FORMAT AS POSSIBLE (WITHIN 25 CHARACTERS) TO FILE 22.

C  FOR THIS LINE, READLINE, FIND IEND, THE LAST CHARACTER WHICH IS NOT
C  BLANK. THEN ONLY CHARACTERS 1:IEND WILL BE WRITTEN TO FILE 22.

	DO IEND = 80,1,-1
	 IF(READLINE(IEND:IEND) .NE. ' ') GO TO 20
	END DO

   20   CONTINUE

C  CANNOT USE WRITE(22,_) READLINE(1:IEND) SINCE, FOR SOME REASON,
C  WRITING LIKE THIS "RIGHT JUSTIFIES" THE CHARACTERS AT THE END
C  OF THE A1000 FORMAT. INSTEAD MUST WRITE (22,__) READLINE, WHERE
C  THE FORMAT IS DETERMINED BY THE LAST NON-BLANK CHARACTER (IEND)
C  IN READLINE.

	IF(IEND .LE. 2) THEN
	 WRITE(22,26) READLINE
   26    FORMAT(A2)
	 RETURN
	ENDIF

	IF(IEND .LE. 4) THEN
	 WRITE(22,51) READLINE
   51    FORMAT(A4)
	 RETURN
	ENDIF

	IF(IEND .LE. 6) THEN
	 WRITE(22,76) READLINE
   76    FORMAT(A6)
	 RETURN
	ENDIF

	IF(IEND .LE. 8) THEN

	 WRITE(22,101) READLINE
  101    FORMAT(A8)
	 RETURN
	ENDIF

	IF(IEND .LE. 10) THEN
	 WRITE(22,126) READLINE
  126    FORMAT(A10)
	 RETURN
	ENDIF

	IF(IEND .LE. 12) THEN
	 WRITE(22,151) READLINE
  151    FORMAT(A12)
	 RETURN
	ENDIF

	IF(IEND .LE. 14) THEN
	 WRITE(22,176) READLINE
  176    FORMAT(A14)
	 RETURN
	ENDIF

	IF(IEND .LE. 16) THEN
	 WRITE(22,201) READLINE
  201    FORMAT(A16)
	 RETURN
	ENDIF

	IF(IEND .LE. 18) THEN
	 WRITE(22,226) READLINE
  226    FORMAT(A18)
	 RETURN
	ENDIF

	IF(IEND .LE. 20) THEN
	 WRITE(22,251) READLINE
  251    FORMAT(A20)
	 RETURN
	ENDIF

	IF(IEND .LE. 22) THEN
	 WRITE(22,276) READLINE
  276    FORMAT(A22)
	 RETURN
	ENDIF

	IF(IEND .LE. 24) THEN
	 WRITE(22,301) READLINE
  301    FORMAT(A24)
	 RETURN
	ENDIF

	IF(IEND .LE. 26) THEN
	 WRITE(22,326) READLINE
  326    FORMAT(A26)
	 RETURN
	ENDIF

	IF(IEND .LE. 28) THEN
	 WRITE(22,351) READLINE
  351    FORMAT(A28)
	 RETURN
	ENDIF

	IF(IEND .LE. 30) THEN
	 WRITE(22,376) READLINE
  376    FORMAT(A30)
	 RETURN
	ENDIF

	IF(IEND .LE. 32) THEN
	 WRITE(22,401) READLINE
  401    FORMAT(A32)
	 RETURN
	ENDIF

	IF(IEND .LE. 34) THEN
	 WRITE(22,426) READLINE
  426    FORMAT(A34)
	 RETURN
	ENDIF


	IF(IEND .LE. 36) THEN
	 WRITE(22,451) READLINE
  451    FORMAT(A36)
	 RETURN
	ENDIF

	IF(IEND .LE. 38) THEN
	 WRITE(22,476) READLINE
  476    FORMAT(A38)
	 RETURN
	ENDIF

	IF(IEND .LE. 40) THEN
	 WRITE(22,501) READLINE
  501    FORMAT(A40)
	 RETURN
	ENDIF

	IF(IEND .LE. 42) THEN
	 WRITE(22,526) READLINE
  526    FORMAT(A42)
	 RETURN
	ENDIF

	IF(IEND .LE. 44) THEN
	 WRITE(22,551) READLINE
  551    FORMAT(A44)
	 RETURN
	ENDIF

	IF(IEND .LE. 46) THEN
	 WRITE(22,576) READLINE

  576    FORMAT(A46)
	 RETURN
	ENDIF

	IF(IEND .LE. 48) THEN
	 WRITE(22,601) READLINE
  601    FORMAT(A48)
	 RETURN
	ENDIF

	IF(IEND .LE. 50) THEN
	 WRITE(22,626) READLINE
  626    FORMAT(A50)
	 RETURN
	ENDIF

	IF(IEND .LE. 52) THEN
	 WRITE(22,651) READLINE
  651    FORMAT(A52)
	 RETURN
	ENDIF

	IF(IEND .LE. 54) THEN
	 WRITE(22,676) READLINE
  676    FORMAT(A54)
	 RETURN
	ENDIF

	IF(IEND .LE. 56) THEN
	 WRITE(22,701) READLINE
  701    FORMAT(A56)
	 RETURN
	ENDIF

	IF(IEND .LE. 58) THEN
	 WRITE(22,726) READLINE
  726    FORMAT(A58)
	 RETURN
	ENDIF

	IF(IEND .LE. 60) THEN
	 WRITE(22,751) READLINE
  751    FORMAT(A60)
	 RETURN
	ENDIF

	IF(IEND .LE. 62) THEN
	 WRITE(22,776) READLINE
  776    FORMAT(A62)
	 RETURN

	ENDIF

	IF(IEND .LE. 64) THEN
	 WRITE(22,801) READLINE
  801    FORMAT(A64)
	 RETURN
	ENDIF

	IF(IEND .LE. 66) THEN
	 WRITE(22,826) READLINE
  826    FORMAT(A66)
	 RETURN
	ENDIF

	IF(IEND .LE. 68) THEN
	 WRITE(22,851) READLINE
  851    FORMAT(A68)
	 RETURN
	ENDIF

	IF(IEND .LE. 70) THEN
	 WRITE(22,876) READLINE
  876    FORMAT(A70)
	 RETURN
	ENDIF

	IF(IEND .LE. 72) THEN
	 WRITE(22,901) READLINE
  901    FORMAT(A72)
	 RETURN
	ENDIF

	IF(IEND .LE. 74) THEN
	 WRITE(22,926) READLINE
  926    FORMAT(A74)
	 RETURN
	ENDIF

	IF(IEND .LE. 76) THEN
	 WRITE(22,951) READLINE
  951    FORMAT(A76)
	 RETURN
	ENDIF

	IF(IEND .LE. 78) THEN
	 WRITE(22,976) READLINE
  976    FORMAT(A78)
	 RETURN
	ENDIF

	WRITE(22,4) READLINE
    4    FORMAT(A80)
        RETURN

        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE CONDENSE3(READLINE)
        CHARACTER READLINE*1000        

C  SUBROUTINE CONDENSE3 IS CALLED BY MAIN TO WRITE READLINE WITH AS 
C  SMALL A FORMAT AS POSSIBLE (WITHIN 25 CHARACTERS) TO FILE 22.

C  FOR THIS LINE, READLINE, FIND IEND, THE LAST CHARACTER WHICH IS NOT
C  BLANK. THEN ONLY CHARACTERS 1:IEND WILL BE WRITTEN TO FILE 22.


	DO IEND = 1000,1,-1
	 IF(READLINE(IEND:IEND) .NE. ' ') GO TO 20
	END DO

   20   CONTINUE

C  CANNOT USE WRITE(22,_) READLINE(1:IEND) SINCE, FOR SOME REASON,
C  WRITING LIKE THIS "RIGHT JUSTIFIES" THE CHARACTERS AT THE END
C  OF THE A1000 FORMAT. INSTEAD MUST WRITE (22,__) READLINE, WHERE
C  THE FORMAT IS DETERMINED BY THE LAST NON-BLANK CHARACTER (IEND)
C  IN READLINE.

	IF(IEND .LE. 26) THEN
	 WRITE(22,26) READLINE
   26    FORMAT(A26)
	 RETURN
	ENDIF

	IF(IEND .LE. 51) THEN
	 WRITE(22,51) READLINE
   51    FORMAT(A51)
	 RETURN
	ENDIF

	IF(IEND .LE. 76) THEN
	 WRITE(22,76) READLINE
   76    FORMAT(A76)
	 RETURN
	ENDIF

	IF(IEND .LE. 101) THEN
	 WRITE(22,101) READLINE
  101    FORMAT(A101)
	 RETURN
	ENDIF

	IF(IEND .LE. 126) THEN
	 WRITE(22,126) READLINE
  126    FORMAT(A126)
	 RETURN
	ENDIF

	IF(IEND .LE. 151) THEN
	 WRITE(22,151) READLINE
  151    FORMAT(A151)
	 RETURN

	ENDIF

	IF(IEND .LE. 176) THEN
	 WRITE(22,176) READLINE
  176    FORMAT(A176)
	 RETURN
	ENDIF

	IF(IEND .LE. 201) THEN
	 WRITE(22,201) READLINE
  201    FORMAT(A201)
	 RETURN
	ENDIF

	IF(IEND .LE. 226) THEN
	 WRITE(22,226) READLINE
  226    FORMAT(A226)
	 RETURN
	ENDIF

	IF(IEND .LE. 251) THEN
	 WRITE(22,251) READLINE
  251    FORMAT(A251)

	 RETURN
	ENDIF

	IF(IEND .LE. 276) THEN
	 WRITE(22,276) READLINE
  276    FORMAT(A276)
	 RETURN
	ENDIF

	IF(IEND .LE. 301) THEN
	 WRITE(22,301) READLINE
  301    FORMAT(A301)
	 RETURN
	ENDIF

	IF(IEND .LE. 326) THEN
	 WRITE(22,326) READLINE
  326    FORMAT(A326)
	 RETURN
	ENDIF


	IF(IEND .LE. 351) THEN
	 WRITE(22,351) READLINE
  351    FORMAT(A351)
	 RETURN
	ENDIF

	IF(IEND .LE. 376) THEN
	 WRITE(22,376) READLINE
  376    FORMAT(A376)
	 RETURN
	ENDIF


	IF(IEND .LE. 401) THEN
	 WRITE(22,401) READLINE
  401    FORMAT(A401)
	 RETURN
	ENDIF

	IF(IEND .LE. 426) THEN
	 WRITE(22,426) READLINE
  426    FORMAT(A426)
	 RETURN
	ENDIF

	IF(IEND .LE. 451) THEN
	 WRITE(22,451) READLINE
  451    FORMAT(A451)
	 RETURN
	ENDIF

	IF(IEND .LE. 476) THEN
	 WRITE(22,476) READLINE
  476    FORMAT(A476)
	 RETURN
	ENDIF

	IF(IEND .LE. 501) THEN
	 WRITE(22,501) READLINE
  501    FORMAT(A501)
	 RETURN
	ENDIF

	IF(IEND .LE. 526) THEN
	 WRITE(22,526) READLINE
  526    FORMAT(A526)
	 RETURN
	ENDIF

	IF(IEND .LE. 551) THEN
	 WRITE(22,551) READLINE
  551    FORMAT(A551)
	 RETURN
	ENDIF

	IF(IEND .LE. 576) THEN
	 WRITE(22,576) READLINE
  576    FORMAT(A576)
	 RETURN
	ENDIF

	IF(IEND .LE. 601) THEN
	 WRITE(22,601) READLINE
  601    FORMAT(A601)
	 RETURN
	ENDIF

	IF(IEND .LE. 626) THEN
	 WRITE(22,626) READLINE
  626    FORMAT(A626)
	 RETURN
	ENDIF

	IF(IEND .LE. 651) THEN
	 WRITE(22,651) READLINE
  651    FORMAT(A651)
	 RETURN

	ENDIF

	IF(IEND .LE. 676) THEN
	 WRITE(22,676) READLINE
  676    FORMAT(A676)
	 RETURN
	ENDIF

	IF(IEND .LE. 701) THEN
	 WRITE(22,701) READLINE
  701    FORMAT(A701)
	 RETURN
	ENDIF

	IF(IEND .LE. 726) THEN
	 WRITE(22,726) READLINE
  726    FORMAT(A726)
	 RETURN
	ENDIF

	IF(IEND .LE. 751) THEN
	 WRITE(22,751) READLINE
  751    FORMAT(A751)
	 RETURN
	ENDIF

	IF(IEND .LE. 776) THEN
	 WRITE(22,776) READLINE
  776    FORMAT(A776)
	 RETURN
	ENDIF

	IF(IEND .LE. 801) THEN
	 WRITE(22,801) READLINE
  801    FORMAT(A801)
	 RETURN
	ENDIF

	IF(IEND .LE. 826) THEN
	 WRITE(22,826) READLINE
  826    FORMAT(A826)
	 RETURN
	ENDIF


	IF(IEND .LE. 851) THEN
	 WRITE(22,851) READLINE
  851    FORMAT(A851)
	 RETURN
	ENDIF

	IF(IEND .LE. 876) THEN
	 WRITE(22,876) READLINE
  876    FORMAT(A876)
	 RETURN
	ENDIF

	IF(IEND .LE. 901) THEN
	 WRITE(22,901) READLINE
  901    FORMAT(A901)
	 RETURN
	ENDIF

	IF(IEND .LE. 926) THEN
	 WRITE(22,926) READLINE

  926    FORMAT(A926)
	 RETURN
	ENDIF

	IF(IEND .LE. 951) THEN
	 WRITE(22,951) READLINE
  951    FORMAT(A951)
	 RETURN
	ENDIF

	IF(IEND .LE. 976) THEN
	 WRITE(22,976) READLINE
  976    FORMAT(A976)
	 RETURN
	ENDIF

	WRITE(22,4) READLINE
    4    FORMAT(A1000)
        RETURN

        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE GETMAXCYCNO(IMAXCYC)
        
        CHARACTER READLINE*1000

C  THIS ROUTINE IS A VARIATION OF SUBROUTINE GETICYCSTART, WHICH READS
C  AN INTEGER ANYWHERE ON A GIVEN LINE WITH A PARTICULAR SET OF WORDS 
C  AT THE START OF THAT LINE.

    2   FORMAT(A1000)

C  READ UNTIL THE LINE WHICH HAS THE WORDS 
C  'THE LAST CYCLE NO. WILL BE .LE.' ON IT SOMEWHERE.

   10 READ(25,2) READLINE
	ILINE=0
	 DO I=1,42
        IF(READLINE(I:I+30) .EQ. 'THE LAST CYCLE NO. WILL BE .LE.') THEN     
	   ILINE=1
	   GO TO 20
	  ENDIF
	 END DO
   20 IF(ILINE .EQ. 0) GO TO 10

C  SOMEWHERE AFTER THE CHARACTERS IN ENTRIES I:I+30 IS THE INTEGER.
C  READ THE CHARACTERS FOR THIS INTEGER, AND THEN CONVERT IT TO
C  AN INTEGER VALUE. AFTER THE FOLLOWING LOOP, THESE CHARACTERS WILL BE
C  IN READLINE(ISTART:IEND).

	 IEND = 0
	 ISTART = 0

	  DO J = I+31,72
	   IF(ISTART .EQ. 0 .AND. READLINE(J:J) .NE. ' ') ISTART = J
	   IF(ISTART .NE. 0 .AND. READLINE(J:J) .EQ. ' ') THEN
	    IEND = J-1
	    GO TO 30
	   ENDIF
	  END DO

C  CHECK TO MAKE SURE THAT THE NO. OF CHARACTERS READ IN FOR THE INTEGER
C  IS NOT MORE THAN 6 (I.E., 6 CHARACTERS ALLOW A MAXIMUM SIZE OF 999999
C  WHICH IS MORE THAN THE LARGEST VALUE THIS INTEGER CAN BE).

   30	ISIZE = IEND-ISTART

        IF(ISIZE .GT. 5) THEN
         WRITE(*,31) 
   31    FORMAT(/' THE MAXIMUM ENDING CYCLE NO IS LARGER THAN 999999,'/
     1' WHICH IS TOO LARGE.'//
     1' THE PROGRAM STOPS. ')
	   CALL PAUSE
         STOP
        ENDIF

C  CONVERT AS INDICATED ABOVE.

	IMAXCYC = 0
	 DO K=ISTART,IEND
	  IF(READLINE(K:K) .EQ. '0') IVAL = 0
	  IF(READLINE(K:K) .EQ. '1') IVAL = 1
	  IF(READLINE(K:K) .EQ. '2') IVAL = 2
	  IF(READLINE(K:K) .EQ. '3') IVAL = 3
	  IF(READLINE(K:K) .EQ. '4') IVAL = 4
	  IF(READLINE(K:K) .EQ. '5') IVAL = 5
	  IF(READLINE(K:K) .EQ. '6') IVAL = 6
	  IF(READLINE(K:K) .EQ. '7') IVAL = 7
	  IF(READLINE(K:K) .EQ. '8') IVAL = 8
	  IF(READLINE(K:K) .EQ. '9') IVAL = 9
	  IMAXCYC = IMAXCYC + IVAL*10**ISIZE
	  ISIZE = ISIZE-1
	 END DO


	RETURN
	END



c-----------------------------------------------------------------------
c  idm1x18.f                                               9/09/19

C wmy210190909

C Fixed OpenMP/gcc compatibility issues. This is primarily through
C   adding interfaces and moving some subroutines into the npag_utils.f90
C   module. And rewriting the dvode package to f90 standard. Note that
C   NPprep is also edited to support these changes.

C Moved calculation of standard deviation to the end of SUBROUTINE FUNC()
C   to facilitate using multiple probability distributions. The model
C   now supports measurements drawn from Poisson and/or Normal
C   distributions. The error coefficient array is now of length 6.

C npag_utils.mod contains almost all parameters.

C Removed older comments unless they might be useful for resolving
C   potential bugs.

c-----------------------------------------------------------------------
c  idm1x18.f                                               3/11/15

C  Code that this file was edited from.

c-----------------------------------------------------------------------

c  idm1x17.f                                               12/05/14

c  First OpenMP compatible release.

c-----------------------------------------------------------------------

c  idm1x16.f                                               7/21/14

c  idm1x16 has the following changes from idm1x15:

c  1. In subroutine USERANAL, the dimensions of RWORK and IWORK are
c  now made consistent with the maximum allowable no. of compartments
c  (i.e., differential equations). In Subroutine DVODE, comments
c  indicate that RWORK must be dimensioned (for MF = 21 OR 22, which
c  is the case in our programs) at least 22 +  9*NEQ + 2*NEQ**2,
c  where NEQ = the no. of differential eqs.). Since NEQ = NDIM can be 
c  as high as 20, RWORK is now dimensioned 1002. And correspondingly,
c  LRW must be also set = 1002.

c  Similarly, the dimension of IWORK, and the value of LIW must be
c  30 + NEQ = 50.

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c  idm1x11.f                                               5/25/12

c  idm1x11 has the following changes from idm1x10:

C  IT HAS CODE CHANGES IN SUBROUTINE PREDLAST3 TO HANDLE THE CASE WHERE
C  PRED1 + PRED3 - 2*PRED2 = 0 --> PREDNEG SHOULD NOT BE CALCULATED. 
C  USUALLY THIS WILL HAPPEN WHEN THE MODEL/DOSAGE REGIMEN IS SO "EASY"
C  TO PREDICT THAT THE 3 PREDICTED VALUES ARE VERY CLOSE TO EACH OTHER,
C  AND BY "BAD LUCK" COULD BE IN A LINEAR PROGRESSION. I.E., IF
C  PRED1 + DEL = PRED2, AND PRED2 + DEL = PRED3, THEN 
C  PRED1 + PRED3 - 2*PRED2 = 0.

C  IN THIS CASE, OF COURSE, PREDNEG SHOULD NOT BE CALCULATED SINCE THAT
C  WILL RESULT IN A DIVIDE BY 0, OR A NaN IF THE PROGRAM DOES NOT STOP.

C  WHEN THIS HAPPENS (SEE CODE IN PREDLAST3), WHETHER OR NOT CONVERGENCE
C  IS ACHIEVED WILL DEPEND SOLELY ON THE TOL1 CRITERION (I.E., THE TOL2
C  CRITERION CANNOT BE USED, AND IS UNNEEDED).

C-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

C-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c----------------------------------------------------------------------C
      SUBROUTINE IDPC(JSUB,IG,NPX,PX,NBCOMP,SUMSQJ,NOBSER,NUMEQT,
     1   NDIM,MF,RTOL,ATOL,TIM,SIG,YO,RS,BS,
     2   INTLIST,IPAR,ObsError,RPAR,ERRFIL) 

C  INPUT ARE: 

C  PX(I) = Ith COORDINATE OF THE (IGth) GRID POINT OF LENGTH NPX, 
C   parsed by SUBROUTINE MAKEVEC(), and

C  Information about the model, and how to solve it: IF NDIM.eq.-1 then
C   ANAL3, else DIFFEQ, and IF DIFFEQ then MF, RTOL, ATOL are DVODE
C   parameters. Note that a model may also require COVariate information
C   describing the particular subject, which is stored in RS (see I/O).
C   And,

C  Information about the I/O. Size of observations. NOBSER are the
C   total no. of measurements, while NUMEQT is the number of types of
C   measurements, i.e. output equations in SUBROUTINE OUTPUT. Inputs
C   are at time TIM, observations are at times SIG, and actual values
C   of both are in RS, BS, and YO. NBCOMP(.) are the Bolus compartment
C   numbers. And,

C  INFORMATION ABOUT THE JSUBth SUBJECT, WHICH HAS BEEN READ IN 
C  PREVIOUSLY, by SUBROUTINE FILRED(), some of this information
C  is in RS:
C
C  INTLIST contains:
C  (1) = AGE; (2) = ISEX; (3) = HEIGHT; (4) = IETHFLAG;
C  (5) = NDRUG ! types of input; (6) = NADD (#Additional COVariates);
C  (7) = NI = 2*NDRUG+NADD; (8) = ND;
C  (9) = NUMEQT ! types of output;
C  (10) = M = NOBSER ! total measurements for subject JSUB

C  OUTPUT IS:

C  SUMSQJ = SUM(F^2), FOR THIS SUBJECT, OVER I=1,M x NOS (ACTUALLY THE (I,J)
C   CONTRIBUTION IS IGNORED IF YO(I,J) = -99 --> MISSING VALUE), OF 
C   ((YO(I,J)-H(I,J))/ObsError(I,J))**2, WHERE H(I,J) = PREDICTED VALUE
C   OF THE JTH OUTPUT EQ AT THE ITH OBSERVATION TIME, ASSUMING THE IGTH 
C   GRID POINT, X.

C  ObsError(I,J)

C  And, some values in RPAR that must be passed back up to MAIN to compute
C   the Pr(JSUB|IG), as of 20190909 (Sep 9, 2019), in main:
C      PYJGX(JSUB,IG) = 10**RPAR(k_prod_pr)
C     1        * DEXP(-.5D0*RPAR(k_sum_z_sq))/RPAR(k_sfac)/RPAR(k_ofac)

C Finally, in called routines, IPAR(i_skip_ig) can be set to immediately
C  return to MAIN and PYJGX(JSU,IG) <- 0 and cnotinue to next support point.

C----------- USE
       USE npag_utils, only: maxnumeq,max_m_per_obs
     1   ,max_ODE_params,max_doses,max_ODE_comps,max_RS_J
     2   ,max_input_dim

        IMPLICIT none 

C----------- ARGS
        integer JSUB, IG, NPX
        double precision, dimension(max_ODE_params) :: PX
        integer, dimension(max_input_dim) :: NBCOMP
        double precision SUMSQJ
        integer NOBSER,NUMEQT,NDIM,MF
        double precision RTOL
        double precision, dimension(max_ODE_comps) :: ATOL
        double precision, dimension(max_m_per_obs) :: TIM
        double precision, dimension(max_doses) :: SIG
        double precision, dimension(max_m_per_obs,MAXNUMEQ) :: YO
        double precision, dimension(max_doses,max_RS_J) :: RS
        double precision, dimension(max_doses,max_input_dim) :: BS
        integer, dimension(128), intent(INOUT) :: INTLIST
        integer, dimension(257) :: IPAR
        double precision, dimension(max_m_per_obs,MAXNUMEQ) :: ObsError
        double precision, dimension(257), intent(INOUT) :: RPAR
        character*20 ERRFIL

C  FIND THE SUM OF SQUARES OF DIFFERENCES BETWEEN THE OBSERVED
C  VALUES AND THE PREDICTED VALUES (NORMALIZED BY THE ASSAY
C  VARIANCE OF EACH OBSERVATION) FOR THIS POINT, i.e. \sum z^2.
C  Note: This is calculated w.o.re: for distribution type.

C  If MAIN requires other information to continue calculation
C   (of PYJ) Then these values should be passed up to MAIN via
C    RPAR and IPAR.  The information held in these arrays is
C    noted in  npag_utils.f90

          CALL SUMSQ(JSUB,IG,NPX,PX,NBCOMP,SUMSQJ,NOBSER,NUMEQT,
     1   NDIM,MF,RTOL,ATOL,TIM,SIG,YO,RS,BS,
     2   INTLIST,IPAR,ObsError,RPAR,ERRFIL)

        RETURN
	END
c----------------------------------------------------------------------C
        SUBROUTINE SUMSQ(JSUB,IG,NPX,PX,NBCOMP,SSQ,NOBSER,NUMEQT,
     1   NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     2   INTLIST,IPAR,ObsError,RPAR,ERRFIL) 

C  SUBROUTINE TO EVALUATE THE SUM OF SQUARES OF THE RESIDUAL VECTOR.

       USE npag_utils, only: maxnumeq, max_m_per_obs, max_ODE_params
     1   , max_doses,max_ODE_comps, max_RS_J, max_input_dim
     2   , k_prod_pr, k_sum_z_sq, i_skip_ig, i_do 

C  NOTE THAT F HAS DIMENSION 3564 = max_m_per_obs*6 SINCE IT HAS NOS*M ENTRIES,
C  THE MAX VALUE OF NOS = 6, AND THE MAX VALUE FOR M = 99*6 = max_m_per_obs.

C-------- argument list ----------

C wmy2017Sep11
C NOBSER = M; NUMEQT = NOS; PX = PCOPY
        integer JSUB,IG,NPX,NOBSER,NUMEQT,NDIM,MF
        real*8, dimension(max_ODE_params) :: PX
        integer, dimension(max_input_dim) :: NBCOMP
        double precision SSQ, RTOL
        real*8, dimension(max_ODE_comps) :: ATOL
        real*8, dimension(max_m_per_obs) :: TIMCOPY
        real*8, dimension(max_doses) :: SIGCOPY
        real*8, dimension(max_m_per_obs,MAXNUMEQ) :: YO
        real*8, dimension(max_doses,max_RS_J) :: RSCOPY
        real*8, dimension(max_doses,max_input_dim) :: BSCOPY
        integer, dimension(128), intent(INOUT) :: INTLIST
        integer, dimension(257) :: IPAR
        double precision, dimension(max_m_per_obs,MAXNUMEQ) :: ObsError
        double precision, dimension(257) :: RPAR
        character*20 ERRFIL

C-------- local variables ----------

c wmy2017Jan05 Variables that will be passed to subroutines need
c   the SAVE and ThreadPrivate attributes; local varbs w/extent
c   limited to this subroutine do not.


C        DIMENSION F(3564)
        double precision, save,
     1    dimension(max_m_per_obs*MAXNUMEQ*MAXNUMEQ) :: F
        integer NUMRES, I, IinF

!$omp Threadprivate(F)

C-------- calculation ----------

        CALL FUNC(JSUB,IG,NOBSER,F,NPX,PX,NBCOMP,
     1   NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     2   INTLIST,IPAR,ObsError,RPAR,ERRFIL)

C--   If IPAR(i_skip_ig).eq.0 then the point (JSUB,IG) is "bad" according to a
c   user defined criterion. Instead of risking a NaN or some other 
c   error, which will lead to bogus results, program exit, or crash,
c   we instead pass the "bad" state as quickly as possible to main
c   and continue calculations as though the P(JSUB|IG)->0. Program
c   will automatically IPAR(i_skip_ig)<-0 if DVODE returns an error.

        if (IPAR(i_skip_ig).eq.0) then
          SSQ = 10.73d99
          return
        endif

C wmy2017Spr18 -- Nelder Mead uses the sum of squared deviations
C  as the simplex vertices
C        NUMRES=NUMEQT*NOBSER
        NUMRES=INTLIST(9)*INTLIST(10)
        SSQ=0.0D0
        DO 10 I=1,NUMRES
 10       SSQ=SSQ+F(I)*F(I)

        RETURN
        END

C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE FUNC(JSUB,IG,M,F,NPX,PX,NBCOMP,
     1   NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     2   INTLIST,IPAR,ObsError,RPAR,ERRFIL)

C  FUNCTION TO DETERMINE THE ENTRIES IN F, GIVEN PX.
C  F(:) returns as (Yobser - Ypredicted)/ObsError

C INTLIST(1) = int(AGE)
C INTLIST(2) = ISEX
C INTLIST(3) = int(HEIGHT)
C INTLIST(4) = IETHFLAG
C INTLIST(5) = /CNST2/NDRUG
C INTLIST(6) = /CNST2/NADD = No. Additional Covariates
C INTLIST(7) = /CNST/NI = 2*NDRUG+NADD
C INTLIST(8) = /CNST/ND ; ND =  NDO
C INTLIST(9) = /CNST2/NOS (Called /CNST2/NUMEQTT in SUBROUTINE FILRED)
C INTLIST(10) = /CNST2/M = NOBSER

C USE can not follow implicit or include statements
       USE npag_utils, only: verifyval, shift, thesame, do140
     1   , predlast3, maxnumeq, max_m_per_obs, max_SS_doses
     2   , max_ODE_params,max_doses,max_ODE_comps,max_RS_J
     3   , max_input_dim, k_gamma, k_flat, k_sfac, k_ofac
     4   , k_sum_z_sq, k_prod_pr, k_dvode_reserved, k_p_end
     5   , k_ig, k_jsub, i_ig, i_jsub, k_c0_base, k_c1_base
     6   , k_c2_base, k_c3_base, k_c4_base, k_c5_base
     7   , k_resolve, i_errmod
     8   , i_misval, i_skip_ig, i_is_log10, i_Nnormalobs
     9   , i_is_poisson_obs, i_Npoissonobs, i_dvode_reserved

      IMPLICIT none
C REAL*8(A-H,O-Z)

C---------- USE LIST ---------------------------------------------------
C wmy2017NOv29 -- OpenMP hates COMMON blocks; instead, pass arguments
C  in and/or make use of interfaces.  If you can't, then use
C  modules. If all of this is impossible, try to SAVE + !$omp Threadprivate
C  to put a private copy for each thread on the heap.

C M = NOBSER
C N = NDIM
C ND = No. Dose Events
C NI = 2*NDRUG + NADD
C NUP and NUIC are not used anywhere in program
C NP = NVAR+NOFIX ; ND read in from 27 by filred

C note: Verified that /CNST/N == NDIM by making bogus
C  models w/XP(N); N = 7,6,11,etc. ... N == NDIM always

C wmy2019.03.12 -- SR SHIFT moved into npag_utils.f90
C       include "interface_0SHIFT.txt"

! NEW PARALLEL CODE BELOW AS OF npageng28.f.
C !$omp Threadprivate(/PARAMD/,/INPUT/,/STATE/,RSO,BSO,SIGO,Y)   
C !$omp Threadprivate(/CNST2/,/STATE/,/CNST/,/OBSER/,/BOLUSCOMP/)
C !$omp Threadprivate(/BOLUSCOMP/,/OBSER/,/CNST/,/CNST2/)
C !$omp Threadprivate(/BOLUSCOMP/)

C      DIMENSION NBCOMP(7)
C      ,TIM(max_m_per_obs),SIG(max_doses),RS(max_doses,max_RS_J),
C     1 YO(max_m_per_obs,MAXNUMEQ), BS(max_doses,max_input_dim)
C      , STDEV(max_m_per_obs,MAXNUMEQ)

C     integer N,ND,NI,NUP,NUIC,NP
C     integer NPL,NOS,NDRUG,NADD

      CHARACTER ERRFIL*20

C---------- ARGUMENT LIST ---------------------------------------------
       integer JSUB,IG,M,NPX,NDIM, MF
       doubleprecision, dimension(max_m_per_obs*MAXNUMEQ*MAXNUMEQ) :: F
       doubleprecision RTOL
       real*8, dimension(max_ODE_comps) :: ATOL
       real*8, dimension(max_ODE_params) :: PX
       integer, dimension(max_input_dim) :: NBCOMP
       real*8, dimension(max_m_per_obs) :: TIMCOPY
       real*8, dimension(max_doses) :: SIGCOPY
       real*8, dimension(max_m_per_obs,MAXNUMEQ) :: YO
       real*8, dimension(max_doses,max_RS_J) :: RSCOPY
       real*8, dimension(max_doses,max_input_dim) :: BSCOPY
       integer, dimension(128), intent(INOUT) :: INTLIST
       integer, dimension(257) :: IPAR
       double precision, dimension(max_m_per_obs,MAXNUMEQ) :: ObsError
       double precision, dimension(257) :: RPAR

C !$omp ThreadPrivate( IPAR, ObsError )

C ----- Local Variables -----------------------------------------------

       double precision UseNormal, P_thresh
       double precision YOBSERVED, YMEAN, T, TOUT, DOSEINT
       integer I, J, ICONV, ID, III, ISAME, ISTEADY
       integer KNTM1, N, NN, NSET, NTL

C ----- ------ SAVEd

       double precision, save, dimension(max_m_per_obs,MAXNUMEQ) :: Y
       double precision, save, dimension(max_ODE_comps) :: X

! ADDED BSO(.,.) AS OF idm1x17.f.
       real*8, save, dimension(max_m_per_obs) :: TIMO
       real*8, save, dimension(max_doses) :: SIGO
       real*8, save, dimension(max_doses,max_RS_J) :: RSO
       real*8, save, dimension(max_doses,max_input_dim) :: BSO

! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp Threadprivate(RSO,BSO,SIGO,TIMO,Y,X)

       integer NDO 
       integer, save :: IKNS,KNS,KNT,KNSOLD
!$omp Threadprivate(IKNS,KNS,KNT,KNSOLD)

C ----- ------ Implicit Private

       double precision, dimension(maxnumeq) :: YT
       double precision, dimension(max_input_dim) :: TLAG
C DIMENSION YT(MAXNUMEQ),TLAG(max_input_dim)

       double precision, dimension(100) :: XVERIFY
       double precision, dimension(max_SS_doses,max_ODE_comps) :: XSTORE
       double precision, dimension(max_ODE_comps) :: XPRED, B, BCOPY
       double precision, dimension(max_RS_J) :: R, RCOPY

       integer JUMPTO45

       real*8, dimension(max_input_dim) :: FA
       integer ISKIPBOL

       integer NNORMALOBS, NPOISSONOBS, MISVAL
       real*8 SIGFAC,OFAC,ZSCORE

       double precision poissonprob

       integer UseInlineDO140
       UseInlineDO140 = 0

C  NOTE THAT AS OF idm1x15.f, THE DIMENSIONS OF 6 IN XSTORE AND XPRED
C  HAVE BEEN CHANGED TO max_ODE_comps, WHICH IS WHAT THEY SHOULD HAVE BEEN ALL
C  ALONG (I.E., THE SAME AS FOR X).

C  NOTE THAT THE 2ND DIMENSION OF STDEV AND YO IS MAXNUMEQ, WHICH
C  IS SET IN THE NEW PARAMETER STATEMENT ABOVE.
C  NOTE THAT THE DIMENSIONS RELATED TO THE NO. OF OUTPUT EQS. IN
C  YO, YT, STDEV, AND Y ARE CHANGED TO MAXNUMEQ (FROM 6). NUMEQT COULD
C  NOT BE USED BECAUSE THESE ARRAYS WERE NOT PASSED TO THIS ROUTINE AS
C  DUMMY ARGUMENTS.

C-----------------------------------------------------------------------

C /CNST/N reassigned to argument NDIM -- They should be equal anyways
       N = NDIM

C  NOTE THAT "7" IN THE ABOVE ARRAYS INDICATE THE NO. OF DRUGS ALLOWED.

C  NOTE THAT F HAS DIMENSION 3564 = max_m_per_obs*6 SINCE IT HAS NOS*M ENTRIES,
C  THE MAX VALUE OF NOS = 6, AND THE MAX VALUE FOR M = 99*6 = max_m_per_obs.
C
C  wmy20190820 -- each equation (maxnumeq) can be sampled max_m_per_obs
C    times, and each of those times can be unique, so that an observation
C    might be quite sparse, w/only one value and maxnumeq-1 zeroes or -99s.
C    on each row.

C  R(7) CHANGED TO R(max_ODE_comps) <-- No. of 'rate inputs'
C  B(3) CHANGED TO B(max_ODE_comps) <-- No. of different bolus inputs
C  CHANGED X(3) TO X(max_ODE_comps) <-- No. of compartments
C  IC(10) CHANGED TO IC(max_ODE_comps) <-- Initial conditions in compartments;
C 		should have been changed to 20 previously (like X,B).
C  NBCOMP(10) CHANGED TO NBCOMP(max_ODE_comps) <-- Same remarks as for IC.
C  P(10) CHANGED TO P(max_ODE_params) <-- No. of parameters

C*****ODE CONSTANTS AND INITIALIZATION*****

C wmy2017Sep25 - Ensure X is not random.
      do III=1,max_ODE_comps
         X(iii) = 0.0D0
      end do
c wmy2017Nov24 - Copy COMMON/OBSER/TIM(:) to SAVEd ThreadPrivate local TIMO(:)
      do III=1,max_m_per_obs
         TIMO(III)=TIMCOPY(III)
      end do

c      if (T.eq.0) then
c        write (*,*) JSUB,IG,"KNS,SIGO(1...)",
c     1    KNS,SIG(1),SIG(2),SIG(3),SIG(4),
c     2    KNT,TIMO(1),TIMO(2),TIMO(3),TIMO(4)
c      endif

      KNS=1
      KNT=1

C      write (*,*) "KNS and KNT init",KNS,KNT

C  NOTE THAT KNT IS THE RUNNING INDEX OF THE NEXT OBSERVATION TIME,
C  AND       KNS IS THE RUNNING INDEX OF THE NEXT DOSAGE TIME.

      T=0.0D0

C  INITIALIZE ISKIPBOL = 0. SEE CODE BELOW. IT IS ONLY NEEDED FOR A
C  STEADY STATE DOSE SET WHICH HAS BOLUS DOSES.

      ISKIPBOL = 0

c wmy2017Oct01
C      DO I = 1,NDRUG
      DO I = 1,INTLIST(5)
       R(2*I-1) = 0.D0
      END DO


!  AS OF idm1x17.f, ESTABLISH BSO(.,.), AND THEN USE BSO,RSO,SIGO,
!  AND NDO RATHER THAN BS,RS,SIG, AND ND FOR ALL CODE BELOW.

c wmy2017Oct01
C      DO I=1,ND
C       DO J=1,NDRUG
      DO I=1,INTLIST(8)
       DO J = 1,INTLIST(5)
        BSO(I,J)=RSCOPY(I,2*J)
C wmy2017Sep26
C        if (RSCOPY(I,2*J) .ne. RS(I,2*J)) then
C           write (*,*) "BSO init :: ERROR: RS.ne.RSCOPY for",
C     1     "(JSUB,IG)(KNT,KNS;T)",Jsub,IG,KNT,KNS,T
C        endif
       END DO
      END DO

c  AS OF idm1x7.f, instead of R(1) = 0, the code has been changed to 
c  set R(2*I-1) = 0, for I = 1,NDRUG. I.E., All IV rates for all NDRUG
c  drugs are initialized to be 0 ... in case the 1st obs. time is 0,
c  which means that OUTPUT is called before the R(I) are set below.


C  CALL SUBROUTINE GETFA IN npemdriv.f (THE FIRST TEMPLATE FILE TO 
C  INCLUDE GETFA IS TSTMULTG.FOR) TO OBTAIN THE VALUE OF FA FOR EACH
C  OF THE NDRUG DRUGS.

C  AS OF idm1x13.f, BEFORE CALLING GETFA, MUST SET
C  THE R(.) IN CASE ANY OF THE FA(.) ARE FUNCTIONS OF THE 
C  COVARIATES WHICH ARE ESTABLISHED FROM THE R(.) VALUES IN
C  GETFA.

c 2017Oct01 
C      DO I=1,7  ! NI should be equal to 7
c      DO I=1,NI
      DO I=1,INTLIST(7)
C wmy2017Sep30 -- replace /INPUT/R
       R(I)=RSCOPY(KNS,I)
       RCOPY(I)=RSCOPY(KNS,I)
C wmy2017Sep26
C        if (RSCOPY(KNS,I) .ne. RS(KNS,I)) then
C           write (*,*) JSUB,IG, "R init :: RS.ne.RSCOPY for KNS=", KNS
C        end if
      END DO
C
C wmy2017Sep29 -- Fixed this bug
C   (1) RSCOPY is fine at previous write, but here,
C       R(I) = 0.0
C   (2) For last thread, NI = 7, for the others NI = 0
C

C        write (*,*) "DO 800", JSUB,IG,"In FUNC RCOPY :: ",
C     1    NI, RCOPY(1), RCOPY(2), RCOPY(3), RCOPY(4),
C     2    RCOPY(5), RCOPY(6), RCOPY(7),
C     2    "Calling GETFA"
C        write (*,*) "DO 800", JSUB,IG,"In FUNC R :: ",
C     1    NI, R(1), R(2), R(3), R(4),
C     2    R(5), R(6), R(7),
C     2    "Calling GETFA"

c wmy2017Nov03 -- debug DEBUG
c         write (*,*) "Calling GETFA()"

c      CALL GETFA(FA,X)
C	 CALL GETFA(FA,X,P,R,B,INTLIST)
	 CALL GETFA(FA,X,PX,R,B,INTLIST)

c wmy2017Nov03 -- debug DEBUG
c         write (*,*) "Returned from GETFA()"

C  NOTE THAT NBCOMP(I),I=1,NDRUG WAS SET IN SUBROUTINE SYMBOL AND
C  PASSED TO THIS ROUTINE VIA COMMON/BOLUSCOMP.

C  As of idm1x12.f, the code to save ND0, SIGO, RSO, is moved to before
c  the IF(N .EQ. 0) GO TO 75  statement. The reason is that before this
c  routine returns, ND, SIG, and RS are reset back to these values,
c  even if N = 0, and so they must be established at this time.

C  AS OF idm1x9.f, SAVE ND, SIG, AND RS WHETHER OR NOT NTL = 1, SINCE
C  IF THERE ARE STEADY STATE DOSE SETS, THE FIRST SIG(.) VALUE IN EACH
C  SET WILL BE CHANGED TO BE 0 BELOW.

C wmy2017.07.11 debug
c         write (*,*) "Enterred FUNC: RSO and SIGO init", JSUB, IG,
c     1    NDO, NI, INTLIST(8),intlist(7)
C
C wmy2017Dec01 -- This was the bug at the center of the last 3 months
C  of troubles! ND is 0 for all spawned threads.
C
C      if (ND .ne. intlist(8)) then
C	 write (*,*) JSUB,IG,"ERROR: ND.NE.intlist(8)",ND,intlist(8)
C      end if

         NDO = intlist(8)

C         DO I=1,NDO
C          DO J=1,NI
         DO I=1,INTLIST(8)
          DO J=1,INTLIST(7)
           RSO(I,J) = RSCOPY(I,J)
          END DO
          SIGO(I) = SIGCOPY(I)
         END DO

C  IF N = 0, THE OUTPUT EQUATION(S) FOR Y ARE CODED EXPLICITLY INTO
C  SUBROUTINE OUTPUT, AND NO D.E. SOLUTIONS (VIA USERANAL/DIFFEQ) ARE
C  TO BE USED. IN THIS CASE, SKIP THE CODE REGARDING INITIAL CONDITIONS
C  OF THE COMPARTMENTS, SINCE THEY ARE IRRELEVANT (I.E., THE COMPARTMENT
C  AMOUNTS DON'T NEED TO BE INITIALIZED SINCE THEY WON'T BE UPDATED BY
C  INTEGRATING D.E.'S). IN FACT, COULD PROBABLY SKIP TIMELAGS TOO, 
C  SINCE THEY CHANGE THE TIME THAT BOLUS DOSES ARE GIVEN, AND THIS
C  THEORETICALLY ONLY AFFECTS COMPARTMENT AMOUNTS (WHICH ARE NOT USED
C  IF N = 0), BUT JUST SKIP INITIAL CONDITIONS FOR NOW.

C------------------- This Block replaced w/single line that follows
C C        IF(N.EQ. 0) GO TO 75 ! Note that /CNST/N is undefined
C        IF(NDIM .EQ. 0) GO TO 75

C  CALL SUBROUTINE GETIX IN npemdriv.f (THE FIRST TEMPLATE FILE TO 
C  INCLUDE GETIX IS TSTMULTG.FOR) TO OBTAIN THE VALUE OF X (THE INITIAL
C  COMPARTMENT AMOUNT) FOR EACH OF THE N COMPARTMENTS.


c wmy2017Oct02
C	 CALL GETIX(N,X)
C          CALL GETIX(NDIM,X,P,R,B,INTLIST)

C  CALL SUBROUTINE GETTLAG IN npemdriv.f (THE FIRST TEMPLATE FILE TO 
C  INCLUDE GETTLAG IS TSTMULTG.FOR) TO OBTAIN THE VALUE OF THE TIMELAG
C  FOR EACH OF THE NDRUG DRUGS.

C---------------------- End of block replaced by single line below

C        IF(NDIM .NE. 0) CALL GETIX(NDIM,X,P,R,B,INTLIST)
        IF(NDIM .NE. 0) CALL GETIX(NDIM,X,PX,R,B,INTLIST)

C debug
C        if (NDIM.eq.0) write (*,*) "DEBUG 2018.06.15: NDIM.eq.0"

C--------------------------------------------------------------------

C   75	 CALL GETTLAG(TLAG,X)
C   75	 CALL GETTLAG(TLAG,X,P,R,B,INTLIST)
   75	 CALL GETTLAG(TLAG,X,PX,R,B,INTLIST)

C  IF ANY TLAG(.) VALUES RETURN AS .NE. 0, THEN, CALL SUBROUTINE SHIFT
C  TO ADJUST THE DOSAGE REGIMEN APPROPRIATELY.

      NTL = 0
C      DO ID = 1,NDRUG
      DO ID = 1,INTLIST(5)
       IF(TLAG(ID) .NE. 0) NTL = 1
      END DO

        IF(NTL .EQ. 1) THEN

C  STORE INCOMING VALUES IN ND, SIG, AND RS (WHICH CONTAINS BS VALUES)
C  SINCE THEY WILL BE CHANGED IN THE CALL TO SUBROUTINE SHIFT, WHICH 
C  "SHIFTS" THE DOSAGE REGIMEN MATRIX TO ACCOUNT FOR THE TIMELAG 
C  PARAMETER(S), TLAG(I). AT THE END OF THIS ROUTINE, THE VALUES IN ND, 
C  SIG, AND RS WILL BE RESET TO THEIR INCOMING VALUES - TO BE READY FOR 
C  THE NEXT CALL TO THIS ROUTINE WITH POSSIBLY DIFFERENT VALUES FOR 
C  TLAG(I). NO! AS OF idm1x17.f, THE VALUES OF ND,SIG,RS,BS ARE 
C  NOT CHANGED; INSTEAD NDO,SIGO,RSO,BSO ARE USED.


C         CALL SHIFT(TLAG,NDO,SIGO,NDRUG,NADD,RSO)
          CALL SHIFT(TLAG,NDO,SIGO,INTLIST(5),INTLIST(6),
     1      RSO,INTLIST)

C
C         CALL SHIFT(TLAG,INTLIST(8),SIGO,INTLIST(5),INTLIST(6),RSO)
C
C          if (INTLIST(8) .ne. NDO) then
C              write (*,*) "IL(8) vs NDO", INTLIST(8), NDO
C              INTLIST(8) = NDO
C          endif 
C
C NDO is incremented by 1 after shift() -- this is to accomodate the
C   NULL stimulus at T=0.
C
C wmy2018Jul17 BUG: Any write to INTLIST(8) inside FUNC() causes the
C   value in INLIST(8) to be incremented to absurdly high values, as soon
C   as the value reaches above 5000 the program exits. NDO is also
C   incremented b/c it is set to INTLIST(8) near top of FUNC().
C   Therefore, can't pass INTLIST(8) into SHIFT(), and can't increment
C   INTLIST(8) to new NDO value after call to SHIFT().
C   Tried:
C     Attributing intent(INOUT) to INTLIST in FUNC(), SUMSQ(), NPAG(), etc.
C     Removing SAVE attribute from NDO -- Note that NDO still retains 
C       value between calls to FUNC()
C
C debug
C          write (*,*) "DEBUG 2018.06.15 Called SHIFT(): TLAG,NDO,NDRUG,N
C     1ADD,NI,ND,NOS,M",TLAG(1),NDO,INTLIST(5),INTLIST(6),INTLIST(7)
C     2,INTLIST(8),INTLIST(9),INTLIST(10)
C          write (*,*) "SIGO", sigo(1),sigo(2),sigo(3),sigo(4),sigo(5),
C     1       sigo(6), sigo(7)
c
c wmy2017Jul13 -- shift() seems to return correct values in SIGO
c
c

C  ESTABLISH THE VALUES IN EACH DRUG'S PO COLUMN TO THE CORRESPONDING
C  COLUMN IN ARRAY BSO.

C          if (NDO.ne.INTLIST(8)) write (*,*) "shift: NDO.ne.I(8)",
C     1         NDO, INTLIST(8)

          DO I=1,NDO
C           DO J=1,NDRUG
C          DO I=1,INTLIST(8)
            DO J=1,INTLIST(5)
              BSO(I,J)=RSO(I,2*J)
            END DO
          END DO
C      write (*,*) "BSO(NDO,IL(5))",BSO(NDO,INTLIST(5)),NDO,INTLIST(5)


        ENDIF
C  THE ABOVE ENDIF IS FOR THE  IF(NTL .EQ. 1)  CONDITION.


C------- Prepare to loop through times in TIMO(:) and SIGO(:)
C
C wmy2017Nov27 -- at this point, TNS and TNT are at their initial
C   values: TNS=TNT=1
C
C-----------------------------------------------------------------------

        JUMPTO45 = 0
C
C wmy2017Nov27 -- This block checks for 3 conditions, if any are true,
C  then the calculation will jump to label 45.
C
C  [1] If (next obs time < next stimulus time) then JUMPTO45; also,
C     if (next obs time == 0.0) then calculate the output before
C     making the jump.
C  Else
C  [2] If (next obs time == next stimulus time) then JUMPTO45;
C     also, if (next obs time == 0.0) then calculate the output
C     before making the jump.  And,
C  [3] If (next stimulus time > 0.0) JUMPTO45.
C
C Note: Conditions [1] and [2] can be combined to <=, rather than two
C  conditions.
C Note: The code between the end of this block and label 45 checks if a
C  SS calculation is desired at TOUT=0. If we know that the next stimulus
C  time is > 0, then we can skip that logic, i.e. jump to label 45.
C

C        IF(TIMO(KNT).GE.SIGO(KNS)) GO TO 12
        IF(TIMO(KNT).LT.SIGO(KNS)) then

C          IF(TIMO(KNT).NE.0.0D0) GO TO 45
          IF(TIMO(KNT).eq.0.0D0) then

C  THE ONLY WAY THE FOLLOWING CALL TO OUTPUT CAN OCCUR IS IF TIM(KNT)
C  = 0 --> OBTAIN YT = OUTPUT VALUE(S) AT TIME 0.0.

            CALL OUTPUT(0.D0,YT,X,RPAR,IPAR)
C            DO 2000 I=1,NOS
            DO 2000 I=1,INTLIST(9)
2000          Y(KNT,I)=YT(I)
            KNT=KNT+1
C           GO TO 45
          ENDIF

          JUMPTO45 = 1

        ELSE

C 12        IF(TIMO(KNT).GT.SIGO(KNS)) GO TO 13
C 12        IF(TIMO(KNT).LE.SIGO(KNS)) then   ! Can't be <, so must be .GE.
12        IF(TIMO(KNT).EQ.SIGO(KNS)) then

C            IF(TIMO(KNT).NE.0.0D0) GO TO 45
            IF(TIMO(KNT).EQ.0.0D0) then 

C  THE ONLY WAY THE FOLLOWING CALL TO OUTPUT CAN OCCUR IS IF TIM(KNT)
C  = 0 --> OBTAIN YT = OUTPUT VALUE(S) AT TIME 0.0.

              CALL OUTPUT(0.D0,YT,X,RPAR,IPAR)
C              DO 2005 I=1,NOS
              DO 2005 I=1,INTLIST(9)
2005            Y(KNT,I)=YT(I)
              KNT=KNT+1
            else 
              JUMPTO45 = 1
            end if
          END IF

C 13       IF(SIGO(KNS) .GT. 0.0D0) GO TO 45
13        IF(SIGO(KNS) .GT. 0.0D0) JUMPTO45 = 1

        END IF

      ISTEADY = 0

C wmy2018.06.21 ANAL3() seems to cause KNS to jumpt from 1 to 3 and miss
C the last stimulus. Setting JUMPTO45=1 should make ALL observations
C will jump to 45, skipping next block
C      JUMPTO45 = 1
C setting JUMPTO45=1, above, forces KNS=1, always

      if (JUMPTO45.eq.0) then
C-----------------------------------------------------------------------

C  CHECK TO SEE IF SIGO(KNS) < 0. IF SO, IT MEANS THAT 100 STEADY STATE
C  DOSES SHOULD NOW BE APPLIED WITH AN INTERDOSE INTERVAL EQUAL TO
C  -SIGO(KNS).

C-----------------------------------------------------------------------
      IF(SIGO(KNS) .LT. 0.D0) THEN


        ISTEADY = 1
        NSET = 1

C  NOTE THAT ISTEADY = 1 TELLS THE PROGRAM BELOW TO PROCEED AS IF THE
C  DOSE TIME IS 0, AND START INTEGRATING THROUGH THE SET OF 100 
C  DOSE SETS, ALL OF WHICH OCCUR BEFORE THE NEXT OBSERVATION TIME ...
C  BUT PAUSE AFTER THE END OF THE 5TH DOSE SET (NSET IS THE RUNNING NO.
C  OF THE CURRENT DOSE SETS THAT HAVE BEEN RUN) AND CALL SUBROUTINE
C  PREDLAST3 TO PREDICT THE STEADY STATE COMPARTMENT AMOUNTS AFTER THE
C  100 DOSE SETS (NOTE THAT THE COMPARTMENT AMOUNTS WILL HAVE TO BE
C  STORED AT THE END OF EACH OF THE STEADY STATE DOSE SETS AS THE LOGIC
C  OF PREDLAST3 REQUIRES). 


C  IF "CONVERGENCE" IS ACHIEVED AT THAT POINT, ASSIGN THE COMPARTMENT 
C  AMOUNTS TO BE THE PREDICTED AMOUNTS, AND ASSIGN KNS TO BE WHAT IT IS
C  WHEN THESE STEADY STATE DOSE SETS HAVE FINISHED. NOTE THAT THE END OF
C  THE 100TH DOSE SET WILL BE AT TIME 100*(-SIGO(KNS)), SO KNS WILL BE 
C  THE INDEX OF THE FIRST DOSE EVENT WHICH OCCURS AFTER THIS TIME.

C  IF "CONVERGENCE" IS NOT ACHIEVED, CONTINUE APPLYING THE LOGIC OF
C  PREDLAST3 UNTIL IT IS ACHIEVED, OR UNTIL THE 100 DOSE SETS ARE ALL
C  INTEGRATED THROUGH, WHICHEVER COMES FIRST.

        DOSEINT = -SIGO(KNS)

C  RESET SIGO(KNS) TO BE 0 SINCE THIS DOSE EVENT REPRESENTS THE START
C  OF 100 DOSE SETS THAT BEGIN AT TIME 0.


        SIGO(KNS) = 0

      ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(SIGO(KNS) .LT. 0.D0)  CONDITION.
C-----------------------------------------------------------------------

C      DO I=1,NI
      DO I=1,INTLIST(7)
       R(I)=RSO(KNS,I)
      END DO

C      write (*,*) "Initializing R(:) to RSO(KNS,:)", KNS

C----- GO TO are replaced by standard logic in this block --------------
C
C wmy2017Nov27 If (NDRUG.eq.0) skip this block.
C
C      IF(NDRUG .EQ. 0) GO TO 81
C      IF(NDRUG .NE. 0) then

C      write (*,*) "INTLIST(5)", intlist(5)

      IF(INTLIST(5) .NE. 0) then

C  AS OF idm1x13.f: MUST CALL GETFA BEFORE EVERY TIME THAT
C  FA(.) ARE USED IN CASE THE EQUATION(S) FOR THE FA(.) ARE BASED
C  ON THE COVARIATES, WHICH CAN CHANGE DOSE TO DOSE.

C         CALL GETFA(FA,X)
C        CALL GETFA(FA,X,P,R,B,INTLIST)
        CALL GETFA(FA,X,PX,R,B,INTLIST)

C        IF(N .EQ. 0) GO TO 120
C        IF(NDIM .EQ. 0) GO TO 120
        IF(NDIM .NE. 0) then

C          DO I=1,NDRUG
          DO I=1,INTLIST(5)
            X(NBCOMP(I))=X(NBCOMP(I))+BSO(KNS,I)*FA(I)

C            write (*,*) JSUB,IG,T,"Bolus to",NBCOMP(I),X(NBCOMP(I)),KNS

          END DO

C  NOTE THAT FA(I) IS THE FRACTION OF DRUG AVAILABLE FROM A BOLUS INPUT
C  FOR DRUG I INTO ITS ABSORPTIVE COMPARTMENT.

C        GO TO 81 ! All branches merge into statement 81

        else

C120       DO I=1,NDRUG
120       DO I=1,INTLIST(5)
            B(I)=BSO(KNS,I)*FA(I)
          END DO
        end if
      end if
C-----------------------------------------------------------------------

81    KNS = KNS+1

C      write (*,*) "AT 81", JSUB,IG,T,KNS

C Commenting out the update at 81 seems to cause ALL ++KNS to be skipped!?


C-----------------------------------------------------------------------

      ENDIF
C
C wmy2017Nov28 -- IF (JUMPTO45.eq.1), we skipped the above blocks and
C   came to the above ENDIF (JUMPTO45.eq.0).
C

C  DETERMINE IF, OBSER(ID=0), OR DOSE(ID=1), OR BOTH(ID=2).

! NEW PARALLEL CODE BELOW AS OF npageng28.f

!
!45    IF(KNS .GT. NDO) GO TO 15
! Replaced by below

 45   Continue

C
C
C --- ----- ------- INTEGRATION LOOP -----------------------------------
C
C TODO Convert GO TO logic to DO {from label 46 to label 40} WHILE {KNT .LE. M}
C   note: INTLIST(10)=M

C 46   IF(KNS .GT. NDO) GO TO 15
C 46   IF(KNS .GT. INTLIST(8)) GO TO 15

C 46   IF(KNS .GT. INTLIST(8)) then
 46   IF(KNS .GT. NDO) then
C   IF(KNS .GT. INTLIST(8)) then ! wmy2018Jul13 -- This seems to have
C   fixed ./PmetricsExamples/Run 1/ -- I believe this is because shift()
C   receives NDO and increments by 1; but for some reason INTLIST was 
C   not threadsafe (?) or some other issue -- working on this now! 7/16/18

C
C Label 46 begins four conditional computations that each end with a 
C forced decision to GO TO 31 or 30. The only significant entanglement of
C GO TO computations turned out to be Block 15, which we untangled by
C moving a copy here, in a previous hardening session.  So we are now
C in a position encompass all of the calculation from Label 46 to Label
C 30 inside a single IF/THEN logical structure:
C
C 46   IF(KNS .GT. INTLIST(8)) then
C        { 15 }
C      else
C        IF () THEN
C          {unlabeled block}
C        ELSE
C          {IF () THEN 15 ELSE 25 ENDIF}
C        endif
C      ENDIF 
C      Continue to 30 or Skip to 31
C------------------------------------------------------ Copy of Block 15
C
C wmy2017Dec06 See notes in Block 15, below
C
        ID=0
        TOUT=TIMO(KNT)
        KNT=KNT+1

C        write (*,*) "KNT++ at first 15 block", JSUB,IG,T,TOUT,KNT

C        IF(NDIM .EQ. 0) GO TO 31
C        GO TO 30
C----------------------------------------------- End of Copy of Block 15
C      ENDIF
      ELSE

C CODE CHANGE BELOW FOR idm1x8.f.
      IF(TIMO(KNT) .EQ. 0.D0 .AND. KNT .GT. 1) THEN

C  AS OF idm1x7.f, A TIME RESET NO LONGER REQUIRES ALL INITIAL
C  COMPARTMENT AMOUNTS TO BE RESET TO 0. THIS IS BECAUSE A TIME RESET
C  NO LONGER HAS TO MEAN THAT AN "INFINITE" AMOUNT OF TIME HAS OCCURRED
C  WITH NO DOSING; IT CAN ALSO NOW MEAN THAT AN "INFINITE" AMOUNT OF 
C  TIME HAS OCCURRED WITH UNKNOWN DOSING (IN THIS CASE, SUBROUTINE
C  GETIX WILL BE CALLED BELOW TO ESTABLISH INITIAL CONDITIONS FOR THIS
C  TIME PERIOD). 

C  ADVANCE KNS TO THE NEXT VALUE THAT HAS SIGO(KNS) .LE. 0. I.E., ONCE
C  TIMN(KNT) = 0, IT MEANS THAT WE ARE DONE WITH THE OUTPUT OBS.
C  TIMES IN THE PREVIOUS SECTION --> THERE IS NO POINT IN CONTINUING
C  TO INTEGRATE TILL THE END OF THE DOSES IN THE PREVIOUS SECTION
C  (IF THERE ARE ANY).

C----------------------------------------------------- AAAAA
C        DO IKNS = KNS,INTLIST(8)
C          IF(SIGO(IKNS) .LE. 0.D0) GO TO 110
C        END DO
C        stop program
C  110   KNS = IKNS
C
C --- wmy2017Dec05 --- Above logic is replaced with
C
C        DO IKNS = KNS,INTLIST(8)
C          IF(SIGO(IKNS) .LE. 0.D0) then ! GO TO 110
C            KNS = IKNS
C            exit
C          endif
C        END DO
C        if (SIGO(IKNS).gt.0.D0) stop program
C  110   Continue ! KNS = IKNS
C-----------------------------------------------------

C       write (*,*) "*** wmy2017Dec05 MvG code validation required."

C        DO IKNS = KNS,INTLIST(8)
C ! see note wmy2018Jul13
        DO IKNS = KNS,NDO
C          IF(SIGO(IKNS) .LE. 0.D0) GO TO 110
          IF(SIGO(IKNS) .LE. 0.D0) then
            KNS = IKNS

C            write (*,*) "110 Somehow got to KNS=IKNS",T,TOUT,IKNS

            exit
          endif
        END DO

C  TO GET HERE MEANS THAT NO VALUE IN SIGO(.) FROM KNS TO NDO HAS A 
C  VALUE .LE. 0, AND THIS IS AN ERROR. IT MEANS THAT THE PATIENT DATA
C  FILE HAS AN OBSERVATION TIME RESET ROW WITHOUT AN ACCOMPANYING
C  DOSE RESET ROW. TELL THE USER AND STOP.

C  REPLACE WRITING OF SIGO() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL.

C wmy added this line as per comment above in AAAAA
        if (SIGO(IKNS) .gt. 0.D0) then

          XVERIFY(1) = SIGO(KNS)
          CALL VERIFYVAL(1,XVERIFY)

C        WRITE(*,111) NDO,KNS,SIGO(KNS)
C        WRITE(25,111) KNS,SIGO(KNS)
          WRITE(*,111) NDO,INTLIST(8),KNS,XVERIFY(1)
          WRITE(25,111) INTLIST(8),KNS,XVERIFY(1)

 111  FORMAT(//' THE CURRENT SUBJECT HAS AN OBSERVATION TIME RESET'/
     1' ROW WITHOUT AN ACCOMPANYING DOSE RESET ROW. THE PROGRAM NOW'/
     2' STOPS. '//
     3' REVIEW YOUR PATIENT FILES AND CORRECT THE ERROR.'//
     4' NOTE THAT THE ',I4,' DOSE TIMES (POSSIBLY ALTERED BY TIMELAGS'/
     5' ARE THE FOLLOWING (AND THERE IS NO TIME .LE. 0 AFTER TIME'/
     6' NO. ',I4,' WHICH HAS CORRESPONDING TIME ',F15.4,'):')

          OPEN(42,FILE=ERRFIL)

C          WRITE(42,111) NDO,KNS,SIGO(KNS) 
          WRITE(42,111) INTLIST(8),KNS,XVERIFY(1) 

C          DO I = 1,INTLIST(8)
C ! see note wmy2018Jul13
          DO I = 1,NDO
            WRITE(*,*) SIGO(I)
            WRITE(25,*) SIGO(I)
            WRITE(42,*) SIGO(I)
          END DO

          CLOSE(42)

          CALL PAUSE
          STOP

        endif
C wmy Above endif as per note AAAAA; KNS is already updated, so
C  comment out label 110, which is no longer needed
C  110   KNS = IKNS

C  THERE ARE TWO POSSIBILITES AT THIS POINT, EITHER SIGO(KNS) = 0
C  OR SIGO(KNS) < 0. 

C  IF SIGO(KNS) = 0, THIS REPRESENTS A TIME RESET (T WILL BE SET = 0
C  BELOW) WITH A SINGLE DOSE LINE TO START. IN THIS CASE, CALL GETIX
C  AGAIN (JUST AS WAS DONE NEAR THE TOP OF THIS ROUTINE) TO OBTAIN
C  INITIAL COMPARTMEN AMOUNTS. NOTE THAT BY DEFAULT, IN GETIX, ALL
C  COMPARTMENT AMOUNTS ARE SET = 0 (WHICH WOULD BE THE CASE IF IN THE 
C  LONG TIME PERIOD BETWEEN THE LAST SET OF DOSES AND THIS NEW
C  BEGINNING, NO DOSES HAVE BEEN GIVEN). BUT THE USER MAY ALSO HAVE
C  CODED INTO GETIX EQUATIONS THAT SET ONE OR MORE OF THE X(I) TO
C  FUNCTIONS OF COVARIATE AND PARAMETER VALUES (WHICH WOULD BE THE
C  SITUATION IF AN UNKNOWN DOSING REGIMEN HAS TAKEN PLACE BUT IT
C  DOESN'T MATTER WHAT IT WAS BECAUSE THE PATIENT COMES TO A LAB AND
C  SIMPLY HAS HIS COMPARTMENT VALUES ESTABLISHED BEFORE CONTINUING 
C  WITH THE OTHER VALUES IN HIS PATIENT FILE). 

C  IF SIGO(KNS) < 0, THIS REPRESENTS A TIME RESET WITH A STEADY STATE
C  SET OF 100 DOSES ABOUT TO BEGIN. IN THIS CASE, WE ASSUME THAT THE
C  PATIENT IS ABOUT TO GET 100 SETS OF DOSES SO THAT HIS COMPARTMENT
C  AMOUNTS WILL ACHIEVE STEADY STATE VALUES. THESE STEADY STATE VALUES
C  WILL BE ESTIMATED IN THE BLOCK OF CODE BELOW THAT STARTS WITH 
C  IF(ISTEADY .EQ. 1). IN THIS CASE, WE WILL STILL CALL GETIX TO 
C  MAKE SURE THAT ANY RESIDUAL COMPARTMENT AMOUNTS FROM A PREVIOUS
C  SET OF DOSES IS ZEROED OUT (OR SET = VALUES AS DETERMINED BY
C  SUBROUTINE GETIX).

C  AS OF idm1x14.f, BEFORE CALLING GETIX, MUST SET
C  THE R(.) IN CASE ANY OF THE INITIAL CONDITIONS FOR THE X(.)
C  ARE FUNCTIONS OF THE COVARIATES WHICH ARE ESTABLISHED FROM THE 
C  R(.) VALUES IN GETFA.
 
C        DO I=1,NI
        DO I=1,INTLIST(7)
          R(I)=RSO(KNS,I)
        END DO



C        CALL GETIX(N,X)
C        CALL GETIX(NDIM,X,P,R,B,INTLIST)
        CALL GETIX(NDIM,X,PX,R,B,INTLIST)
		
C  MUST ALSO RESET T = 0 SINCE THE INTEGRATION WILL AGAIN START FROM 
C  TIME 0.

        T = 0.D0

C  IF SIGO(KNS) .LT. 0, THIS IS NOT ONLY A TIME RESET, IT IS THE
C  BEGINNING OF A STEADY STATE DOSE SET. IN THIS CASE, APPLY 100 
C  STEADY STATE DOSES WITH AN INTERDOSE INTERVAL EQUAL TO -SIGO(KNS).

        ISTEADY = 0

        IF(SIGO(KNS) .LT. 0.D0) THEN

          ISTEADY = 1
          NSET = 1

C  NOTE THAT ISTEADY = 1 TELLS THE PROGRAM BELOW TO PROCEED AS IF THE
C  DOSE TIME IS 0, AND START INTEGRATING THROUGH THE SET OF 100 
C  DOSE SETS, ALL OF WHICH OCCUR BEFORE THE NEXT OBSERVATION TIME ...
C  BUT PAUSE AFTER THE END OF THE 5TH DOSE SET (NSET IS THE RUNNING NO.
C  OF THE CURRENT DOSE SETS THAT HAVE BEEN RUN) AND CALL SUBROUTINE
C  PREDLAST3 TO PREDICT THE STEADY STATE COMPARTMENT AMOUNTS AFTER THE

C  100 DOSE SETS (NOTE THAT THE COMPARTMENT AMOUNTS WILL HAVE TO BE
C  STORED AT THE END OF EACH OF THE STEADY STATE DOSE SETS AS THE LOGIC
C  OF PREDLAST3 REQUIRES). 


C  IF "CONVERGENCE" IS ACHIEVED AT THAT POINT, ASSIGN THE COMPARTMENT 
C  AMOUNTS TO BE THE PREDICTED AMOUNTS, AND ASSIGN KNS TO BE WHAT IT IS
C  WHEN THESE STEADY STATE DOSE SETS HAVE FINISHED. NOTE THAT THE END OF
C  THE 100TH DOSE SET WILL BE AT TIME 100*(-SIGO(KNS)), SO KNS WILL BE 
C  THE INDEX OF THE FIRST DOSE EVENT WHICH OCCURS AFTER THIS TIME.

C  IF "CONVERGENCE" IS NOT ACHIEVED, CONTINUE APPLYING THE LOGIC OF
C  PREDLAST3 UNTIL IT IS ACHIEVED, OR UNTIL THE 100 DOSE SETS ARE ALL
C  INTEGRATED THROUGH, WHICHEVER COMES FIRST.

          DOSEINT = -SIGO(KNS)

C  RESET SIGO(KNS) TO BE 0 SINCE THIS DOSE EVENT REPRESENTS THE START
C  OF 100 DOSE SETS THAT BEGIN AT TIME 0.

          SIGO(KNS) = 0

        ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(SIGO(KNS) .LT. 0.D0)  CONDITION.

      ENDIF

C  THE ABOVE ENDIF IS FOR THE 
C   IF(TIM(KNT) .EQ. 0.D0 .AND. KNT .GT. 1)  CONDITION.
C
C-----------------------------------------------------------------------
C
C wmy2017Dec07 Replacing GO TO 20 logic w/IF THEN {...} ELSE {...}
C   This block ends w/a forced decision to skip to Label 30 or 31.
C Blocks 15 and 25 after this block also end in the same forced
C decision. Blocks 15 and 25 are now conditionally calculated, and the
C decision to go to label 30 or 31 is calculated at the end of the
C IF THEN 15 ELSE 25 logic. The intention now is to enclose this logic
C w/in this block:
C    IF () THEN
C      {this block}
C    ELSE
C      {IF () THEN 15 ELSE 25}
C    ENDIF 
C    Skip to 30 or 31
C
C      IF(TIMO(KNT) .NE. SIGO(KNS)) GO TO 20
      IF(TIMO(KNT) .EQ. SIGO(KNS)) then
        ID=2
        TOUT=TIMO(KNT)
        KNT=KNT+1
        KNS=KNS+1

C        write (*,*) "KNS and KNT update at #1450", JSUB,IG,
C     1    T,TOUT,KNS, KNT
C
C wmy2017Dec08 -- This is done at the end of the IF/THEN logic
C      IF(NDIM .EQ. 0) GO TO 31
C      GO TO 30
C
      else

C 20    IF(TIMO(KNT) .GT. SIGO(KNS) .AND. SIGO(KNS) .GT. 0) GO TO 25
C
C wmy2017Dec06
C    A copy of block 15 is moved to Label 46; thus, there is now only
C one way to execute block 15, from this statemnt, label 20, which
C immediatley precedes it, so we can change Label 20 to an IF (.) THEN
C block 15 ELSE block 25, logic. Also, since both block 15 and block 25
C end with the same switch to labels 30 or 31, we can put switch at the
C end of the new Label 20 IF THEN 15 ELSE 25.
C
20    IF(TIMO(KNT) .LE. SIGO(KNS) .OR. SIGO(KNS) .LE. 0) then
C-------------------------------------------------------------------- 15
C Block 15 is entered in only two ways
C 1) via Label 46   IF(KNS .GT. INTLIST(8)) GO TO 15
C 2) if the conditional switch in Label 20, immediateley above, does
C    not force computation to skip Block 15.
C Block 15 has a forced exit to either Label 30 or 31.
C
15      ID=0
        TOUT=TIMO(KNT)
        KNT=KNT+1

C        write (*,*) "KNT update at second Block 15", KNT,JSUB,IG,T

C        IF(NDIM .EQ. 0) GO TO 31
C        GO TO 30
C-------------------------------------------------------------------- 15
      else
C-------------------------------------------------------------------- 25
25      ID=1
        TOUT=SIGO(KNS)
        KNS=KNS+1

C        write (*,*) "KNS update at Block 25", JSUB,IG,T, KNS

C-------------------------------------------------------------------- 25
      endif
      endif
      endif

      IF(NDIM .EQ. 0) GO TO 31
30    CONTINUE
C--------------------------------------------------------------- 30 / 32
C --- wmy2018.06.15 These lines are lifted from USERANAL; they have to
C --- be here to make ANAL3() work.
C --- When you get a chance, go back to useranal and erase these lines
C --- there as those lines are now redundant.  Also, remove INTLIST
C --- from the USERANAL() arglist
        do III=1,max_ODE_params
          RPAR(k_dvode_reserved + III) = PX(III)
C          RPAR(k_dvode_reserved + III) = P(III)
C          RPAR(23 + III) = P(III)
        end do
        do III = 1,max_RS_J
          RPAR(k_p_end + III) = R(III)
C          RPAR(55 + III) = R(III)
C          write (*,*) "what I is the bolus?", iii, RPAR(55+iii), r(iii) ! seems to be in R(2)
        end do

C DEBUG debug
C        if (IPAR(i_do).eq.7000) then
C        write (*,*) "func(...RPAR...):", RPAR(k_p_end+1),
C     1    RPAR(k_p_end+2), RPAR(k_p_end+3), RPAR(k_p_end+4),
C     2    RPAR(k_p_end+5), RPAR(k_p_end+6), RPAR(k_p_end+7),
C     3    RPAR(k_p_end+8), RPAR(k_p_end+9), RPAR(k_p_end+10)
C        end if

        RPAR(k_jsub) = dble(JSUB)
C        RPAR(93) = dble(JSUB)
        RPAR(k_ig) = dble(IG)
C        RPAR(94) = dble(IG)
        do III = 1,10
          IPAR(i_dvode_reserved + III) = INTLIST(III)
        end do
        IPAR(i_jsub) = JSUB
        IPAR(i_ig) = IG
C        write (*,*) "DEBUG 2018.06.15: RPAR",RPAR(24),RPAR(25),RPAR(26)
C     1     ,RPAR(27),RPAR(28) 
C--------------------------------------------------------------
32      IF(NDIM .NE. -1) then
          CALL USERANAL(JSUB,IG,X,T,TOUT,
     1      NDIM,MF,RTOL,ATOL,PX,R,INTLIST,IPAR,RPAR)
C          CALL USERANAL(JSUB,IG,X,T,TOUT,
C     1      NDIM,MF,RTOL,ATOL,P,R,INTLIST,IPAR,RPAR)

C-- Cycle past point
          if (IPAR(i_skip_ig).eq.0) return
        endif

C-- 6/12/18 -- Remove COMMON blocks from ANAL routines
C        write (*,*) "CALL ANAL3():",JSUB,IG,T,TOUT,ISTEADY,KNS,KNT
C     1    X(1),X(2),X(3)

        IF(NDIM .EQ. -1) CALL ANAL3(X,T,TOUT,RPAR,IPAR)

C-------------------------------------------------------------------- 32

C  IF ISTEADY = 1, THIS (Block 32) IS INSIDE A STEADY STATE DOSE SET. CHECK TO SEE
C  IF TOUT IS A MULTIPLE OF DOSEINT. IF SO, RECORD THE COMPARTMENT
C  AMOUNTS. THEN, AFTER COMPARTMENT AMOUNTS HAVE BEEN STORED FOR AT 
C  LEAST THE 1ST 5 MULTIPLES OF DOSEINT, STOP AND CALL SUBROUTINE
C  PREDLAST3 WHICH PREDICTS THE FINAL (STEADY STATE) COMPARTMENT AMOUNTS
C  AFTER THE LAST (100TH) DOSE SET. 

C  IF PREDLAST3 HAS PREDICTED VALUES WHICH "CONVERGE", ASSIGN THE
C  PREDICTED VALUES TO X, INCREASE KNS TO BE THE INDEX OF THE FIRST DOSE
C  EVENT WHICH OCCURS AFTER THE STEADY STATE DOSE SET ENDS AND CONTINUE.

C  IF PREDLAST3 VALUES DON'T CONVERGE, CONTINUE THE PROCESS WITH 
C  COMPARTMENT AMOUNTS FOR MULTIPLES 2 - 6 OF DOSEINT, TEST FOR
C  "CONVERGENCE", ETC. THIS PROCESS CONTINUES UNTIL "CONVERGENCE" IS
C  ACHIEVED FOR A SET OF 5 COMPARTMENT AMOUNTS (OR SETS OF AMOUNTS IF
C  NDRUG IS > 1), OR UNTIL ALL 100 DOSE SETS IN THE STEADY STATE REGIMEN
C  HAVE FINISHED. 

      IF(ISTEADY .EQ. 1) THEN


C  THE NEXT DOSE SET END TIME IS DOSEINT*NSET. IF TOUT = DOSEINT*NSET,
C  STORE THE COMPARTMENT AMOUNTS. IF NSET .GE. 5, CALL PREDLAST3 AND
C  PROCEED AS INDICATED ABOVE.

       CALL THESAME(TOUT,DOSEINT*NSET,ISAME)

       IF(ISAME .EQ. 1) THEN

C        NN = N
C        IF(N .EQ. -1) NN = 3
        NN = NDIM
        IF(NDIM .EQ. -1) NN = 3

        DO J = 1,NN
         XSTORE(NSET,J) = X(J)
        END DO


        IF(NSET .GE. 5) THEN

         CALL PREDLAST3(NN,NSET,XSTORE,XPRED,ICONV)

  
         IF(ICONV .EQ. 1) THEN

C  SINCE THE PREDICTED VALUES ARE CONSIDERED ACCURATE (I.E., 
C  "CONVERGENCE WAS ACHIEVED IN PREDLAST), RESET ISTEADY TO 0,
C  WHICH MEANS THAT THE STEADY STATE DOSES ARE FINISHED; ASSIGN THE
C  COMPARTMENT AMOUNTS TO BE THE PREDICTED VALUES; AND SET KNS TO THE
C  FIRST DOSE EVENT AFTER THE END OF THE STEADY STATE DOSE SET. ALSO,
C  SET T = THE ENDING TIME OF THE STEADY STATE DOSE SET = 100*DOSEINT,
C  SINCE THAT IS WHAT IT WOULD HAVE BEEN HAD ALL 100 DOSE SETS BEEN
C  RUN.

          ISTEADY = 0

          DO J = 1,NN
           X(J) = XPRED(J)
          END DO

          T = 100.D0*DOSEINT


C  ADVANCE KNS TO BE THE FIRST DOSE PAST THE 100 DOSE SETS IN THIS
C  STEADY STATE SET. NOTE THAT THIS SET ENDS BEFORE 100*DOSEINT, SO
C  FIND THE FIRST SIGO(.) THAT IS .GE. 100*DOSEINT, OR THAT IS = 0
C  (WHICH SIGNIFIES A TIME RESET) OR THAT IS < 0 (WHICH SIGNIFIES 
C  ANOTHER STEADY STATE SET).

C-----------------------------  wmy2017Dec05 --- Replaced Logic
C          DO I = KNS,INTLIST(8)
C           IF(SIGO(I) .GE. 100.D0*DOSEINT .OR. SIGO(I) .LE. 0.D0) THEN
C            KNSNEW = I
C            GO TO 100
C           ENDIF
C          END DO
C
C C  TO GET HERE MEANS THAT THERE ARE NO DOSE TIMES PAST THE END OF THIS
C C  STEADY STATE DOSE SET. IN THIS CASE, SET KNS TO NDO+1.
C
C          KNS = INTLIST(8) + 1
C          GO TO 200
C
C  100     KNS = KNSNEW
C  200     CONTINUE
C---------------------------- Above is replaced w/logic below
          KNSOLD=KNS
          KNS = INTLIST(8) + 1
          DO I = KNSOLD,INTLIST(8)
           IF(SIGO(I) .GE. 100.D0*DOSEINT .OR. SIGO(I) .LE. 0.D0) THEN
            KNS = I
            EXIT
           ENDIF
          END DO

C           write (*,*) "KNS update at 200", JSUB,IG,T,KNS

C       write (*,*) "*** wmy2017Dec05 MvG code validation required."

  200     CONTINUE
C-----------------------------

C  SET ISKIPBOL = 1 WHENEVER CONVERGENCE OCCURS IN
C  THE STEADY STATE DOSES SINCE IN THIS CASE, WE DON'T WANT TO
C  REAPPLY THE LAST BOLUS FROM THE STEADY STATE SET BELOW LABEL 83.

          ISKIPBOL = 1

         ENDIF


C  THE ABOVE ENDIF IS FOR THE  IF(ICONV .EQ. 1)  CONDITION.

C  IF ICONV = 0, ISTEADY IS STILL = 1, 
C  WHICH MEANS THAT THE ATTEMPT TO PREDICT THE FINAL (STEADY STATE)
C  COMPARTMENT AMOUNTS CONTINUES.
          
        ENDIF
      
C  THE ABOVE ENDIF IS FOR THE  IF(NSET .GE. 5)  CONDITION.

C  SINCE ISAME = 1, THE END OF THE SET NO. NSET HAS OCCURRED -->
C  INCREASE NSET BY 1.


        NSET = NSET + 1

       ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(ISAME .EQ. 1)  CONDITION.

      ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(ISTEADY .EQ. 1)  CONDITION.


C-------------------------------------------------------------------- 31
31      CONTINUE

C
C wmy2017Dec07 (Pearl Harbor Day)
C   Harden code and prepare for DO/WHILE logic to control
C code from Label 46 to Label 40.
C
C  GO TO logic:
C     IF (ID==1) GO TO 35
C       {...}
C     IF (ID==0) GO TO 40
C  35 CONTINUE
C     IF (intlist(7)==0) GO TO 83
C       {...}
C  83 IF (intlist(5)==0 OR NDIM==0) GO TO 82
C       {...}
C  82 CONTINUE
C  40 IF (KNT <= M) GO TO 46
C
C  is Replaced with IF/THEN logic:
C     IF (ID .ne. 1) then
C       {...}
C     endif
C     IF (ID .NE. 0) then
C       {
C         IF (intlist(7).NE.0) then
C           {...}
C         endif
C         IF (intlist(5).NE.0 .AND. NDIM.ne.0) then
C           {...}
C         endif
C       }
C     endif
C  40 IF (KNT <= M) GO TO 46
C     

C  RECORD OBSERVATION AND SUPPLY NEW DOSE

C        IF(ID .EQ. 1) GO TO 35
        IF(ID .NE. 1) then
          KNTM1=KNT-1

C  NOTE THAT THE TIME AT WHICH THE OUTPUT IS DESIRED IS TIM(KNTM1); THIS
C  IS CLEAR SINCE THE RETURNING VALUE(S) IN YT ARE PUT INTO ROW NO.
C  KNTM1 OF Y.

          CALL OUTPUT(TIMO(KNTM1),YT,X,RPAR,IPAR)

C          DO 2010 I=1,NOS
          DO 2010 I=1,INTLIST(9)
2010        Y(KNTM1,I)=YT(I)

        endif

C 55      IF(ID.EQ.0) GO TO 40
55      IF (ID.NE.0) then

C-------------------------------------------------------------------- 35
  35    CONTINUE

C            IF(NI .EQ. 0) GO TO 83 
C            IF(INTLIST(7) .EQ. 0) GO TO 83 
            IF(INTLIST(7) .NE. 0) then
     
C              DO I=1,NI
              DO I=1,INTLIST(7)
                R(I)=RSO(KNS-1,I)
              END DO

C  AS OF idm1x13.f: MUST CALL GETFA BEFORE EVERY TIME THAT
C  FA(.) ARE USED IN CASE THE EQUATION(S) FOR THE FA(.) ARE BASED
C  ON THE COVARIATES, WHICH CAN CHANGE DOSE TO DOSE.

c              CALL GETFA(FA,X)
              CALL GETFA(FA,X,PX,R,B,INTLIST)
            endif

C-------------------------------------------------------------------- 83
C83      IF(NDRUG .EQ. 0 .OR. N .EQ. 0) GO TO 82
C83      IF(NDRUG .EQ. 0 .OR. NDIM .EQ. 0) GO TO 82
C83      IF(INTLIST(5) .EQ. 0 .OR. NDIM .EQ. 0) GO TO 82
83          IF(INTLIST(5) .NE. 0 .AND. NDIM .NE. 0) then

C  ADDING N .EQ. 0 TO ABOVE IF STATEMENT SHOWS CLEARLY THAT IF
C  N = 0 (IN WHICH CASE ANALYTIC SOLUTIONS ARE CODED DIRECTLY INTO
C  SUBROUTINE OUTPUT, WHICH MAKES THE COMPARTMENT AMOUNTS IRRELEVANT)
C  SETTING VALUES FOR THE COMPARTMENTS, X, IS UNNECESSARY.


C  IF ISKIPBOL = 1, DO NOT APPLY BOLUSES FROM DOSE KNS-1, SINCE THESE
C  BOLUSES WERE PART OF THE STEADY STATE DOSE SET WHICH ALREADY HAD
C  BOLUSES (EFFECTIVELY) APPLIED ABOVE WHERE "CONVERGENCE" OF THE
C  STEADY STATE DOSE SET WAS OBTAINED.

              IF(ISKIPBOL .EQ. 0) THEN
C                DO I=1,NDRUG
                DO I=1,INTLIST(5)
                  X(NBCOMP(I))=X(NBCOMP(I))+BSO(KNS-1,I)*FA(I)
                END DO
              ENDIF

C  RESET ISKIPBOL = 0 HERE. IF IT IS NOW = 1, IT MEANS THAT
C  THE ABOVE APPLICATION OF BOLUSES WAS SKIPPED SINCE THERE HAS JUST
C  BEEN A STEADY STATE SET OF DOSES WHICH CONVERGED (AND WE DON'T
C  WANT THE LAST BOLUS DOSE REAPPLIED). BUT, GOING FORWARD, ISKIPBOL
C  SHOULD BE SET AGAIN TO 0 SO THE ABOVE APPLICATION OF BOLUSES WILL
C  OCCUR WHENEVER THERE IS A NEW BOLUS TO BE APPLIED.

              ISKIPBOL = 0

            ENDIF
C-------------------------------------------------------------------- 83

C 82      CONTINUE

        endif
C Above endif is for IF (ID .ne. 0) THEN; i.e. we skipped here b/c ID==0 

C  CHECK STOPPING TIME.


C       write (*,*) "40 INTLIST(8,10)", JSUB,IG,intlist(8), intlist(10)
C     1    ,KNS,KNT,T,TOUT

C40    IF(KNT .LE. M) GO TO 46
40    IF(KNT .LE. INTLIST(10)) GO TO 46
C  AS OF npageng28.f, GO TO 45 IS REPLACE BY GO TO 46 ABOVE.

C-------------------------------------------------- UPDATE F(JSUB,IG)
      if (UseInlineDO140.eq.1) then

C  NOTE THAT IF YO(I,J) = -99 --> THIS OBSERVED LEVEL IS MISSING.
C  IN THIS CASE, SET THE CORRESPONDING VALUE OF F = 0.

C wmy2017Dec29 Replaced COMMON STDEV with ObsError; note COMMON STDEV is
c  commented out above; uncomment if you need to use it!
c          IF(YO(I,J) .NE. -99) F((J-1)*M+I) =(Y(I,J)-YO(I,J))/STDEV(I,J)

C C       DO J=1,NOS
C C         DO I=1,M
C C           IF(YO(I,J) .EQ. -99) F((J-1)*M+I) = 0.D0
C C           IF(YO(I,J) .NE. -99) F((J-1)*M+I) =(Y(I,J)-YO(I,J))
C C    1                                         /ObsError(I,J)
C        DO J=1,INTLIST(9)
C         DO I=1,INTLIST(10)
C         IF(YO(I,J) .EQ. -99) F((J-1)*INTLIST(10)+I) = 0.D0
C         IF(YO(I,J) .NE. -99) F((J-1)*INTLIST(10)+I) =(Y(I,J)-YO(I,J))
C     1                                         /ObsError(I,J)
C        END DO
C        END DO

C ------ Above is old code, that assumed ObsError filled in main and ---
C ------ all measures are Normally distributed r.v.s -------------------
C ------ Below assumes ObsError is filled here, in the DO 140 loop and -
C ------ some measures are Poisson. ------------------------------------

C Poisson or Normally distributed measurements
C
C Strategy: In model.txt, for each of the 7 output equation set IPAR to
C indicate: Log10 or absolute measurement is reported, Poisson or Normal
C as flagged. Also, uses RPAR(>94,257) if real values, such as assay
C error coefficients are reqd.
C
C Computation:

        NNORMALOBS = 0
        NPOISSONOBS = 0
        MISVAL = 0
        SIGFAC = 1.D0
        OFAC = 0.D0

C Initialize or reset return variables
        RPAR(k_sum_z_sq) = 0.D0
        RPAR(k_prod_pr) = 0.D0
        RPAR(k_sfac) = SIGFAC
        RPAR(k_ofac) = OFAC
        IPAR(i_misval) = MISVAL
        IPAR(i_Nnormalobs) = NNORMALOBS
        IPAR(i_Npoissonobs) = NPOISSONOBS

C-----------------------------------------------------------------------
C DO 140 loop was done in main. Moved DO 140 here because some YOBSERVED
C are not Normally distributed.  Note that SIGFAC and OFAC were
C calculated prior to doing anything (by assuming it's OK to use
C YOBSERVED as a surrogate to the true mean: YMEAN.  Note: Useful to
C calculate DO 140 prior to any other calculation b/c (1) if a problem
C arises you can immediatley exit code and so not waste a lot of user
C time, and (2) saves a lot of computation! The first point is still
C valid -- so we should calculate DO 140 prior to the DO 800 loop for
C (at least) the first few cycles, using YOBSERVED as surrogate for
C YMEAN. This also allows us to have a more stable estimate of ObsError
C in the event that YMEAN is absurd. Or to have a "relaxation" toward
C YMEAN from YOBSERVED. For example, use the res parameter to control
C use of YMEAN or YOBSERVED or even use the average of these two values.
C wmy will just go ahead and program the average

C M Is a verbatim copy of code from Main that is NOT merged w/idm1*
C   Is a comment 

C  ******* DO 140 loop from main *******
C
C  FIND THE ASSAY STANDARD DEVIATIONS FOR THIS SUBJECT. FOR EACH
C  OF THE NOBSER*NUMEQT OBSERVED VALUES (EXCEPT THAT YO(I,J) = -99 -->
C  OUTPUT EQ. J HAS NO OBSERVED LEVEL FOR OBSERVATION TIME I),
C  Y, SIG = C0 + C1*Y + C2*Y**2 + C3*Y**3.
C  NOTE THAT, THEORETICALLY, SIG SHOULD BE A CUBIC FNT. OF
C  THE 'TRUE' OBSERVED VALUES, NOT THE 'NOISY' OBSERVED VALUES (BUT THE
C  'TRUE' VALUES ARE UNKNOWN).
 
C  ALSO, CALCULATE SIGFAC, THE PRODUCT OF THE NON-MISSING STD. DEV.'S

C  (A NON-MISSING S.D. IS ONE FOR WHICH THE CORRESPONDING YO(I,J) IS
C  .NE. -99, THE MISSING VALUE CODE).
C  INITIALIZE SIGFAC=1, AND THEN UPDATE IT FOR EACH NON-MISSING
C  OBSERVATION.
 
C  MISVAL WILL BE THE RUNNING TOTAL OF MISSING VALUES AMONG ALL THE
C  NUMEQT x NOBSER POTENTIAL OBSERVED LEVELS.

C M      DO 140 I=1,NOBSER
C M       DO 140 J=1,NUMEQT
C
        DO J=1,INTLIST(9)     ! NUMEQT or number of output equations
         DO I=1,INTLIST(10)   ! Number of measurements

C           if (NINT(RPAR(J+k_c4_base)).eq.10) then    ! wmy: worked, 1/16/2019
C            write (*,*) "Obs is log10",I,J
C           endif
C           if (NINT(RPAR(J+k_c5_base)).eq.229) then   ! wmy: worked, 1/16/2019
C            write (*,*) "Obs is Poisson",I,J
C           endif
C           write (*,*) "Checking C4 and C5",
C     1            RPAR(J+k_c4_base), RPAR(J+k_c5_base)   ! wmy: worked, 1/16/2019

C  IF Y = -99, IT MEANS THAT OUTPUT EQ. J HAD NO VALUE AT OBSERVATION
C  TIME I. IN THIS CASE, IGNORE THIS Y AND INCREASE MISVAL BY 1.

C M        Y = YO(I,J) ! replaced by YMEAN, below
C M        IF(Y .EQ. -99) THEN    ! This is moved above
C M         MISVAL = MISVAL+1
C M         GO TO 140          ! Not necessary anymore, logic changed
C M        ENDIF
C M        IF(YO(I,J) .NE. -99) then { Assume obs ~ Normal } 

C 2018Sep01 :: MN wants -99 to indicate BLQ, and -88 to indicate missing.
C    RPAR(130) is available, I think, to use for the Pr(obs<LQ). But we
C    also need a space for the LOQ -- use RPAR(131).
C
C -----
          if(YO(I,J) .EQ. -99) then
C BLQ
C            A = <prob this obs is BLQ>
C            A = 1/2 * ERF((LQ - 0)/sigma)
C            RPAR(130) = RPAR(130) * A

C          else if (Y.eq.-88) then
C Desired Prediction
C
           F((J-1)*INTLIST(10)+I) = 0.D0
           MISVAL = MISVAL+1
           YOBSERVED = -99
           YMEAN = Y(I,J)
           ObsError(I,J) = 0.D0
           ZSCORE = 0.D0

c           write (*,*) JSUB,IG,I,J,"is missing."

C -----
          else

C ASSIGN YOBSERVED and YMEAN (predicted) for this measurement

C Y is log10(pred) and YO(I,J) are log10(obs). log10(obs) was read from
C  data.csv or working copy; while, log10(pred) was calculated in call
C  to OUTPUT, above. Use C4 = 10 in the model.txt file. C4 is not a
C  recognized variable in the data.csv file.
           IF (IPAR(i_is_log10 + J) == 10) THEN
            YOBSERVED = 10**(YO(I,J))
            YMEAN = 10**Y(I,J)
           ELSE
            YOBSERVED = YO(I,J)
            YMEAN = Y(I,J)
           ENDIF

C YOBSERVED ~ POISSON

C  229 == extended ASCII code for lower case lambda; USE Poisson
C  set C5 = 229 in the model.txt file. C5 is not a recognized variable
C  in the data.csv file.

C But, if YOBSERVED .gt. UseNormal, treat the count data observation
C  as a Normal variable, including using ierrmod and C0,1,2,3
           UseNormal = 128.0

           IF ( (IPAR(i_is_poisson_obs + J) == 229)
     1       .and. (YOBSERVED .lt. UseNormal) )  THEN

C wmy2019Feb04 -- for mxacd dataset, subject 1, only a single trough measure will
C   come into this loop; for UseNormal = 105; obs = 2.02; 10^2.02 = 104.7129
C   So, this one measurement ~ Normal(YMEAN,sqrt(YMEAN))

C            Poisson: p( YO(i,J) | Y(I,J) ) =  exp( -1 * Y(I,J) )
C                              * ( Y(I,J)^YO(I,J) / fact(YO(I,J)) ))

C Use Normal approx. to Poisson if YMEAN > P_thresh
           P_thresh = 32.0

C If resolve is still high, then use the greater of YMEAN or YOBSERVED
C   as surrogate for mean.  The goal is to allow the path to seek out models
C   that are biased toward the peak, initially, and then to attempt to
C   move the support point mass in a direction that will predict the
C   troughs, too. Recall: For Poisson, the var = mean.
C              if (RPAR(k_resolve) .gt. 0.05 ) then
C                  if (YOBSERVED .gt. YMEAN) then
C                     meanobsswap = YOBSERVED
C                     YOBSERVED = YMEAN
C                     YMEAN = YOBSERVED
C                  endif
C              endif
C wmy2018.01.28 -- Above still results in P(Y=1|G)=0 for moxi dataset

! fact reqs nint(); but fact(N) = Gamma(N + 1), and we use Gamma(N+1),
!  Gamma takes a real. So there is not reason to round YOBSERVED to
!  the nearest integer.
!              YOBSERVED = nint(YOBSERVED)
! Poisson w/mean Y; OK to be a REAL value
              ObsError(I,J) = sqrt( YMEAN )

!  Normal approx.
              IF (YMEAN > P_thresh) THEN
                 SIGFAC = SIGFAC * ObsError(I,J)
                 NNORMALOBS = NNORMALOBS+1
                 RPAR(k_sum_z_sq) = RPAR(k_sum_z_sq)
     1                 + ((YOBSERVED - YMEAN)/ObsError(I,J))**2

               if (ig.eq.1) then
                 write (*,*) JSUB,IG,I,J,"is P_N",YMEAN,YOBSERVED,
     1             ObsError(I,J),RPAR(k_sum_z_sq)
               endif

              ELSE
                 NPOISSONOBS = NPOISSONOBS + 1

C wmy2019.01.29 -- if Pr -> 0, then don't count probability in product.
C  
                 poissonprob = exp( -1.D0 * YMEAN )
     1              * ( YMEAN**YOBSERVED ) / gamma( YOBSERVED + 1)

                 if (poissonprob .gt. 0.0000001 ) then

C wmy2019.02.05 
C   To test code, set RPAR(k_prod_pr) to 0.D0; i.e. 10**0 = 1 in product in main
C   so that \prod pr(obs ~ Poisson) * \prod pr(obs ~ Normal) = \prod pr(obs ~ Normal)

                    RPAR(k_prod_pr) = RPAR(k_prod_pr)
     1                 + log10(PoissonProb)
                    write (*,*) JSUB,IG,I,J,"CAUGHT PoissonProb="
     1                 ,PoissonProb
                 else
                    write (*,*) JSUB,IG,I,J,"PoissonProb -> 0.D0"
     1                ,PoissonProb
                 endif

C wmy2019.01.29 :: Checked for "1          15          23           2 is P"
C   and all output numbers are correct; note that 10**RPAR(k_prod_pre) = 1.0
C   which just means: 10^0 = 1. YMEAN ~ 1.32337, YOBS = =105 = nint(104.7...)

C               if (ig.eq.1) then
C                 write (*,*) JSUB,IG,I,J,"is P  ",YMEAN,YOBSERVED,
C     1             ObsError(I,J), poissonprob, 10**RPAR(k_prod_pr)
C               endif
               if (ig.eq.1) then
                 write (*,*) "JSUB,IG,M,EQN",JSUB,IG,I,J
     1             , "is P w/mu,y,err,z^2,Pr="
     2             , YMEAN,YOBSERVED,ObsError(I,J)
     3             , RPAR(k_sum_z_sq),poissonprob
               endif
              ENDIF

C wmy2018Apr18 -- F returns to IDPC ast he concatenated z-scores

C              ZSCORE=(YOBSERVED-YMEAN)/ObsError(I,J)
C              F((J-1)*INTLIST(10)+I)=ZSCORE

C YOBSERVED ~ NORMAL

           ELSE
C              // CODE for Normal is unchanged from previous program
C 
C M NOTE: FOR EACH SUBJECT, MUST ENSURE THAT ALL THE STD DEV'S ARE NON-
C        ZERO. OTHERWISE, THE PROGRAM WILL BLOW UP! THIS IS BECAUSE
C        P(YJ|X) INVOLVES SQUARED DIFFERNCES BETWEEN OBSERVED Y'S AND
C        EXPECTED Y'S (FOR EACH X GRID POINT)...EACH DIFFERENCE
C        NORMALIZED (I.E., DIVIDED) BY THE VARIANCE OF THE RESPECTED
C        OBSERSATION.
 
C M  SEE M2_17CAL.F CODE FOR COMMENTS ON HOW A STD. DEV. COULD = 0.
 
C M  ALSO TEST TO MAKE SURE NO STD. DEV. < 0, SINCE SIGFAC BEING NEGATIVE
C  WOULD RESULT IN A NEGATIVE PROBABILITY (SEE PYJGX CALCULATION BELOW).

C note: in main:
C ierrmod -> IPAR(38)
C gamma -> RPAR(k_gamma)
C flat -> RPAR(k_flat)
C C0 -> RPAR(98,99,100,101,102,103,104)
C C1 -> RPAR(105:111)
C C2 -> RPAR(112:118)
C C3 -> RPAR(119:125)

C wmy2019.01.24 adjusting sigma calculation to use ymean after
C   current F is assumed to be w/in +/- 5% of the search space
C   range; uses observation as surrogate of mean if F is not
C   yet assumed to be w/in 5% of search space range.

C wmy2019.02.08 -- Switch u=OBS to u=PRED as soon as resolve
C  falls below initial value = 0.2; at this point, the simula-
C  tions should be stable.
C              if (RPAR(k_resolve) .lt. 0.15) then

C M            SIG(I,J) = C0(J)+C1(J)*Y+C2(J)*Y*Y+C3(J)*Y**3
C M            ObsError(I,J) = SIG(I,J) ! at end of DO 140

C              ObsError(I,J) = RPAR(J+k_c0_base)
C     1          + RPAR(J+k_c1_base)*YMEAN
C     2          + RPAR(J+k_c2_base)*YMEAN*YMEAN
C     3          + RPAR(J+k_c3_base)*YMEAN*YMEAN*YMEAN

C              else

              ObsError(I,J) = RPAR(J+k_c0_base)
     1          + RPAR(J+k_c1_base)*YOBSERVED
     2          + RPAR(J+k_c2_base)*YOBSERVED*YOBSERVED
     3          + RPAR(J+k_c3_base)*YOBSERVED*YOBSERVED*YOBSERVED

C              endif

C M cgam4
C M       if(ierrmod.eq.2) sig(i,j) = sig(i,j)*gamma
C M       if(ierrmod.eq.3) sig(i,j) = dsqrt(sig(i,j)**2 + gamma**2)
C M       if(ierrmod.eq.4) sig(i,j) = gamma*flat

              if(IPAR(i_errmod).eq.2) ObsError(i,j)
     1           = ObsError(i,j)*RPAR(k_gamma)
              if(IPAR(i_errmod).eq.4) ObsError(i,j)
     1           = RPAR(k_gamma)*RPAR(k_flat)
              if(IPAR(i_errmod).eq.3) ObsError(i,j)
     1           = dsqrt(ObsError(i,j)**2 + RPAR(k_gamma)**2)

C-------------- 
C M            IF(SIG(I,J) .EQ. 0) THEN  {} ! replaced with:
              IF(ObsError(I,J) .EQ. 0) THEN
                WRITE(*,2345) JSUB
                WRITE(25,2345) JSUB
2345            FORMAT(//' A S.D. IS 0 FOR JSUB = ',I5,'. RERUN THE '/
     1' PROGRAM WITH C0 NOT = 0  FOR THIS SUBJECT, OR WITH THIS'/
     2' SUBJECT ELIMINATED.')
                CLOSE(27)
                CLOSE(25)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

                OPEN(42,FILE=ERRFIL)
                WRITE(42,2345) JSUB
                CLOSE(42)

                CALL PAUSE
                STOP
              ENDIF
 
              IF(ObsError(I,J) .LT. 0) THEN

C wmy2019.02.10
C               write (*,*) "idm1 SD Err01:", JSUB,IG,I,J,YMEAN,YOBSERVED
C     1           ,RPAR(J+k_c0_base), RPAR(J+k_c1_base)
C     2           ,RPAR(J+k_c2_base), RPAR(J+k_c3_base)
C     3           ,ObsError(I,J)
C              ObsError(I,J) = RPAR(J+k_c0_base)
C     1            + RPAR(J+k_c1_base)*YOBSERVED
C     2            + RPAR(J+k_c2_base)*YOBSERVED*YOBSERVED
C     3            + RPAR(J+k_c3_base)*YOBSERVED**3
C               write (*,*) "idm1 SD Err02:", JSUB,IG,I,J,YMEAN,YOBSERVED
C     1           ,RPAR(J+k_c0_base), RPAR(J+k_c1_base)
C     2           ,RPAR(J+k_c2_base), RPAR(J+k_c3_base)
C     3           ,ObsError(I,J)

                WRITE(*,2346) JSUB,IG
                WRITE(25,2346) JSUB,IG
2346            FORMAT(//' A S.D. < 0 FOR JSUB,IG = ',I5,I6,'.     '/
     1'RERUN THE PROGRAM WITH A BETTER CHOICE FOR THE ASSAY ERROR  '/
     2'POLYNOMIAL COEFFICIENTS.')
                CLOSE(27)
                CLOSE(25)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

                OPEN(42,FILE=ERRFIL)
                WRITE(42,2346) JSUB
                CLOSE(42)

                CALL PAUSE
                STOP
              ENDIF

C--------------  SIGFAC and NNORMALOBS

C In main, COMMON SIG is used, and values are copied from SIG into
C ObsError. ObsError is used inside of the parallel region. Here, we
C only have access to ObsError, not SIG. So fill ObsError, and copy
C ObsError to SIG when you return to main. Note that the analytic
C routines use COMMON SIG (the primary reason we have to still
C calculate SIG in main).  ObsError may be calculated differently
C here, than in main.

C M      ObsError(I,J) = SIG(I,J)

              SIGFAC=SIGFAC*ObsError(I,J)
              NNORMALOBS=NNORMALOBS+1

              RPAR(k_sum_z_sq) = RPAR(k_sum_z_sq)
     1           + ((YOBSERVED-YMEAN)/ObsError(I,J))**2

               if (ig.eq.1) then
                 write (*,*) "JSUB,IG,M,EQN",JSUB,IG,I,J
     1             , "is N w/mu,obs,err,z^2="
     2             , YMEAN,YOBSERVED,ObsError(I,J)
     3             , RPAR(k_sum_z_sq)
C     4             , IPAR(i_errmod), RPAR(k_gamma)
               endif
           endif ! if(Poisson){...} else {Normal}

C wmy2018Apr18 -- if (YO.NE.-99) then {F = z-score}

           ZSCORE=(YOBSERVED-YMEAN)/ObsError(I,J)
           F((J-1)*INTLIST(10)+I)=ZSCORE

C debug
c                 write (*,*) JSUB,IG,I,J,YOBSERVED,YMEAN,zscore

          ENDIF ! if(YO(I,J) .EQ. -99){...} else {YO ~ {Normal,Poisson}}

C wmy DEBUG
C          write (*,*) "idm01",I,J,ZSCORE,RPAR(k_prod_pr)
C     1       , YMEAN, YOBSERVED, ObsError(I,J), YO(I,J), Y(I,J)

         END DO ! number of measurements
        END DO ! number of output equations
C M  140 CONTINUE ! is replaced by the two END DO above

C M NOTE THAT SIGFAC WAS CALCULATED IN LOOP 140 ABOVE, AND THAT OFAC IS
C M NOW THE RESULT OF (NOBSER*NUMEQT - MISVAL) VALUES.
C M      OFAC=2.506628274631**(NOBSER*NUMEQT - MISVAL)
C   The above comment and code form main are no longer valid because
C   Some observations are ~Poisson and some of those are going to be
C   calculated using a Normal approx. to the Poisson.

C M      NOBTOT = NOBTOT + NOBSER*NUMEQT - MISVAL
C   Above NOBTOT counts the total observations for all OBSERVATIONS.
C   We are only interested in the number of observations for JSUB
        OFAC=2.506628274631**NNORMALOBS

C Final checks on NNORMALOBS, NPOISSONOBS, MISVAL, SIGFAC, and OFAC
C Copy values into appropriate IPAR and RPAR bins for communication 
C back up to main.
C
C RPAR(k_prod_pr) = \prod Poisson Probs
C RPAR(k_sum_z_sq) = summsq Normalized deviations for Y_obs ~ Normal
        RPAR(k_sfac) = SIGFAC
        RPAR(k_ofac) = OFAC
        IPAR(i_misval) = MISVAL
        IPAR(i_Nnormalobs) = NNORMALOBS
        IPAR(i_Npoissonobs) = NPOISSONOBS


C-----------------------------------------------------------------------
      else 

C STDEV - Above is replaced with the MODULE subroutine:

C        CALL DO140(ijob,JSUB,IG,INTLIST,IPAR,RPAR,F,ObsError,YO,Y,ERRFIL)
        CALL DO140(1,JSUB,IG,INTLIST,IPAR,RPAR,F,ObsError,YO,Y,ERRFIL)

      endif

C Data required to calculate sigma:
C
C  1. Integer array including
C     a. JSUB and IG
C     b. Flag for type of noise: Logistic, exponential, Poisson, Normal, ...
C     c. Flags for computational choices
C     d. Calculation constants that are integer valued (IPAR)
C  2. History of Yobs and Ypred for interoccassion noise
C  3. Real array of fixed constants 
C  4. Real scalar RETURN variable
C 
C------------------------------------------------------------------------------------------------------------------

C  AS OF idm1x9.f, RESTORE THE VALUES FOR ND, SIG, AND RS, IN CASE
C  THIS MODEL HAS TIME LAGS OR STEADY STATE DOSES - TO BE READY FOR THE
C  NEXT CALL TO THIS ROUTINE.
C  NO! AS OF idm1x17.f, THESE VALUES WERE NEVER CHANGED BECAUSE
C      NDO, SIGO, RSO, BSO WERE USED ABOVE INSTEAD.

! NEW PARALLEL CODE BELOW AS OF npageng28.f


!	 ND = NDO
!	 DO I=1,INTLIST(8)
!	  SIGCOPY(I) = SIGO(I)
!	  DO J=1,NI
!	   RSCOPY(I,J) = RSO(I,J)
!	  END DO
!	 END DO

C  ESTABLISH THE VALUES IN EACH DRUG'S PO COLUMN TO THE CORRESPONDING
C  COLUMN IN ARRAY BS.

!         DO I=1,INTLIST(8)
!          DO J=1,INTLIST(5)
!           BSCOPY(I,J)=RSCOPY(I,2*J)
!	  END DO
!	 END DO


      RETURN
      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE USERANAL(JSUB,IG,X,TIN,TOUT,
     1      NDIM,MF,RTOL,ATOL,P,R,INTLIST,IPAR,RPAR)

C  PURPOSE:
C	GIVEN X(TIN) THE PROGRAM CALCULATES X(TOUT), WHERE X IS THE 
C	STATE VECTOR FOR THE MODEL UNDER CONSIDERATION (AS DEFINED
C	BY THE D.E'S IN SUBROUTINE DIFFEQ). THESE D.E'S ARE SOLVED
C	USING THE LINPACK ROUTINE, VODE.FOR (AND ASSOCIATED ROUTINES).

C  	THIS ROUTINE CALLS SUBROUTINE DVODE (VODE.FOR) ONCE FOR EACH
C	POINT AT WHICH ANSWERS ARE DESIRED. NOTE THAT DVODE WILL CALL
C	SUBROUTINE DIFFEQ (SUPPLIED BY THE USER -- IT GIVES THE 
C	DIFF. EQS. OF THE MODEL, XP(I)) AND, IF THE USER DESIRES,
C	SUBROUTINE JACOB (SUPPLIED BY THE USER -- IT GIVES THE 
C	JACOBIAN OF PARTIAL DERIVATIVES, dXP(I)/dX(J)). SUBROUTINES
C	DIFFEQ AND JACOB ARE IN THIS MODULE.

C  ARGUMENTS ON INPUT:
C           X - AN ARRAY OF DIMENSION max_ODE_comp. IN THE STANDARD 3-COMPARTMENT
C		MODEL,  X(1), X(2), X(3) SHOULD
C               BE SET TO THE AMOUNT OF DRUG IN THE ABSORBTION,
C               CENTRAL, AND PERIPHERAL COMPARTMENTS, RESPECTIVELY,
C               AT TIME T=TIN.
C         TIN - CURRENT VALUE OF TIME.
C        TOUT - TIME AT WHICH SOLUTION IS DESIRED.

C	VALUES FROM COMMON/TOUSER (FROM MXEM2__/MAIN) WHICH WERE INPUT
C 	REAL-TIME BY THE USER (SEE DETAILS BELOW).
C	 NDIM = NO. OF STATES IN MODEL (.LE. 3 FOR NOW).
C	 MF = METHOD FLAG.
C	 RTOL = SCALAR RELATIVE TOLERANCE PARAMETER.
C	 ATOL(I), I=1,NDIM = ABSOLUTE TOLERANCE PARAMETERS.

C  ARGUMENTS ON OUTPUT:
C           X - THE COMPARTMENT AMOUNTS AT T=TOUT.
C         TIN - SET AT TOUT

        use npag_utils, only: max_RS_J,max_ODE_params,max_ODE_comps
     1   , k_dvode_reserved, k_p_end, k_ig, k_jsub, i_ig, i_ig
     2   , i_jsub, i_skip_ig, k_dvode_reserved, i_dvode_reserved

        IMPLICIT none
C REAL*8(A-H,O-Z)

c INPUT parameters ::
        integer :: JSUB, IG
        double precision, dimension(max_ODE_comps), intent(INOUT) :: X
        double precision :: TIN, TOUT
        integer :: NDIM, MF
        double precision :: RTOL
        double precision,dimension(max_ODE_comps),intent(INOUT) :: ATOL
        real*8, dimension(max_ODE_params) :: P
        real*8, dimension(max_RS_J) :: R
        integer, dimension(128) :: INTLIST
        integer, dimension(257) :: IPAR
        double precision, dimension(257) :: RPAR

c wmy2017Oct23 -- These variables will be passed to DVODE
C
C  AS OF idm1x16.f, THE DIMENSION OF RWORK IS CHANGED FROM 300 TO
C  1002 TO ACCOMADATE AN NDIM (NEQ IN SUBROUTINE DVODE) OF UP TO max_ODE_comp. SO
C  CHANGE LRW BELOW TO 1002. SIMILARLY THE DIMENSION OF IWORK AND
C  LIW BELOW ARE CHANGED FROM 40 TO 50.
c
        EXTERNAL DIFFEQ,JACOB
        real*8, dimension(1002) ::  RWORK
        integer, dimension(50) :: IWORK
        integer :: ITOL, ITASK, ISTATE, IOPT, LRW, LIW, III
C        real*8, dimension(257) :: RPAR

C wmy2017Oct23
C The following ThreadPrivates got the program "working" again i.e. the
c 3.5 week long search for the bug causing serial program to work
c perfectly, BUT parallel program throwing an Illegal Instruction : 4 error
C Is likely due to an inappropriate memory map imposed by OpenMP not
c 
c  !$omp ThreadPrivate (RWORK, IWORK, RPAR)
c        save RWORK, IWORK, RPAR
!$omp ThreadPrivate (RWORK, IWORK)
        save RWORK, IWORK
C wmy2017Dec28: removed IPAR from save list above; save-ing in main
c
c On 10/23/2017 at end of day; all flags passed OK (as before)
c ATOL(1) OK, but ATOL(max_ODE_comps) = 0, not 10^-4 (actually, serial run has ATOL(max_ODE_comps) = 0, too)
c RPAR = 0.0, except for last thread, is this master? JSUB, IG = 1,1, RPAR has
c correct values (compared to serial run)
c

      INTERFACE
        SUBROUTINE DVODE (F, NEQ, Y, T, TOUT, ITOL, RtolIn,
     &    ATOL, ITASK, ISTATE, IOPT, RWORK, LRW, IWORK, LIW,
     &    JAC, MF, RPAR, IPAR)

          EXTERNAL F
          integer, intent(IN) :: NEQ
          double precision, dimension(:), intent(INOUT) :: Y
          double precision, intent(INOUT) :: T, TOUT
          integer, intent(IN) :: ITOL
          double precision, intent(IN) :: RtolIn
          double precision, dimension(:), intent(IN) :: ATOL 
          integer, intent(IN) :: ITASK
          integer, intent(INOUT) :: ISTATE
          integer, intent(IN) :: IOPT
          double precision, dimension(:), intent(INOUT) :: RWORK
          integer, intent(IN) :: LRW
          integer, dimension(:), intent(INOUT) :: IWORK
          integer, intent(IN) :: LIW
          EXTERNAL JAC
          integer, intent(IN) :: MF
          double precision, dimension(:), intent(INOUT) :: RPAR
          integer, dimension(:), intent(INOUT) :: IPAR

        end SUBROUTINE
      END INTERFACE

C wmy2017Sep12 /TOUSER/ varbs are passed in
C	COMMON/TOUSER/NDIM,MF,RTOL,ATOL
C
C !$omp Threadprivate(/TOUSER/)

! -------- START OF DVODE DECLARATIONS SPECIFIC ------------

c wmy2017Oct06 -- See DVODE docs "Part iii." -- not sure if the
c   declaration is supposed to be here, in SUBROUTINE FUNC, or 
c   maybe even as far back as SUBROUTINE NPAG, outside the 
c   parallelized region.
      COMMON /DVOD01/ ACNRM, CCMXJ, CONP, CRATE, DRC, EL(13),
     1                ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2                RC, RL1, TAU(13), TQ(5), TN, UROUND,
     3                ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     4                L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     5                LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     6                N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     7                NSLP, NYH
      COMMON /DVOD02/ HU, NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
!
! Type declarations for labeled COMMON block DVOD01 --------------------
!
      DOUBLEPRECISION ACNRM, CCMXJ, CONP, CRATE, DRC, EL, ETA, ETAMAX,
     1  H, HMIN, HMXI, HNEW, HSCAL, PRL1, RC, RL1, TAU, TQ, TN, UROUND

      INTEGER ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH, L, LMAX,
     1 LYH, LEWT, LACOR, LSAVF, LWM, LIWM, LOCJS, MAXORD, METH, MITER,
     2 MSBJ, MXHNIL, MXSTEP, N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT,
     3 NSLJ, NSLP, NYH
!
! Type declarations for labeled COMMON block DVOD02 --------------------
!
      DOUBLEPRECISION HU

      INTEGER NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST

c wmy2017Oct -- May also req. calls to DVSRCO -- see DVODE doc
C  "Interrupting and Restarting" -- which will store the above COMMON
c  variables in the following arrays
c      COMMON /DVOD01/ RVOD(48), IVOD(33)
c      COMMON /DVOD02/ HU, NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST

c wmy2017Oct06 -- I _assume_ this is required
!$omp Threadprivate(/DVOD01/,/DVOD02/)

c wmy2017Nov07 -- I _assume_ this is also required
      save /DVOD01/,/DVOD02/

! -------- END OF DVODE DECLARATIONS SPECIFIC -------------

C  THE LOGIC OF THIS CODE IS TAKEN FROM PROGRAM DESOLV3.FOR (4/28/96).

C  FOLLOWING VALUES ARE SUPPLIED TO SUBROUTINE DVODE:

C  DIFFEQ  = NAME OF SUBROUTINE COMPLETED BY USER WHICH GIVES THE D.E.'S 
C	     OF THE MODEL. IT MUST BE DECLARED EXTERNAL.
C TIN      = The initial value of the independent variable.

C TOUT   = First point where output is desired (.ne. TIN). 
C ITOL   = 2 SINCE ATOL IS AN ARRAY.
C RTOL   = Relative tolerance parameter (scalar).
C ATOL   = Absolute tolerance parameter.
C          The estimated local error in X(i) will be controlled so as
C          to be roughly less (in magnitude) than
C             EWT(i) = RTOL*abs(X(i)) + ATOL(i)  SINCE ITOL = 2.
C          Thus the local error test passes if, in each component,
C          either the absolute error is less than ATOL (or ATOL(i)),
C          or the relative error is less than RTOL.
C          Use RTOL = 0.0 for pure absolute error control, and
C          use ATOL = 0.0 (or ATOL(i) = 0.0) for pure relative error
C          control.  Caution.. Actual (global) errors may exceed these
C          local tolerances, so choose them conservatively.
C ITASK  = 1 for normal computation of output values of X at t = TOUT.
C ISTATE = Integer flag (input and output).  Set ISTATE = 1.
C IOPT   = 0 to indicate no optional input used.
C RWORK  = Real work array of length at least..
C             20 + 16*NDIM                      for MF = 10,
C             22 +  9*NDIM + 2*NDIM**2           for MF = 21 or 22,
C             22 + 11*NDIM + (3*ML + 2*MU)*NDIM  for MF = 24 or 25.
C	... I'LL USE AN ARRAY OF 300 (PLENTY FOR NDIM .LE. 8).
C LRW    = Declared length of RWORK (in user's DIMENSION statement).
C IWORK  = Integer work array of length at least..
C             30        for MF = 10,
C             30 + NDIM  for MF = 21, 22, 24, or 25.
C          If MF = 24 or 25, input in IWORK(1),IWORK(2) the lower
C          and upper half-bandwidths ML,MU.
C	... I'LL USE AN ARRAY OF 40 (PLENTY FOR NDIM .LE. 8).
C LIW    = Declared length of IWORK (in user's DIMENSION).
C JACOB    = Name of subroutine COMPLETED BY USER for Jacobian matrix 
C	     (MF = 21 or 24). If used, this name must be declared 
C	     external.  If not used, pass a dummy name.
C MF     = Method flag.  Standard values are..
C          10 for nonstiff (Adams) method, no Jacobian used.
C          21 for stiff (BDF) method, user-supplied full Jacobian.
C          22 for stiff method, internally generated full Jacobian.

C          24 for stiff method, user-supplied banded Jacobian.
C          25 for stiff method, internally generated banded Jacobian.
C RPAR,IPAR = user-defined real and integer SCALARS OR arrays passed to 
C	      DIFFEQ AND JACOB.

C Note that the main program must declare arrays X, RWORK, IWORK,
C and possibly ATOL, RPAR, and IPAR.


C  THE FOLLOWING VALUES RETURN FROM CALLS TO SUBROUTINE DVODE.

C      X = Array of computed values of X vector (AT TIME TOUT).
C      T = Corresponding value of independent variable (normally TOUT).
C ISTATE = 2  if DVODE was successful, negative otherwise.
C          -1 means excess work done on this call. (Perhaps wrong MF.)
C          -2 means excess accuracy requested. (Tolerances too small.)
C          -3 means illegal input detected. (See printed message.)
C          -4 means repeated error test failures. (Check all input.)
C          -5 means repeated convergence failures. (Perhaps bad
C             Jacobian supplied or wrong choice of MF or tolerances.)
C          -6 means error weight became zero during problem. (Solution
C             component i vanished, and ATOL or ATOL(i) = 0.)

        ITOL=2
        ITASK=1
        ISTATE=1
        IOPT=0
        LRW=1002
        LIW=50

C Code in DIFFEQ:
C
C wmy2017Sep29 -- ALL of the varbs in teh following 4 COMMON blocks need
C   to be passed explicitely to DIFFEQ via DVODE subroutines. This will
C   be accomplished by copying them into appropriate spots in RPAR and
C   IPAR, and then read back into their named variables in DIFFEQ.
C
C   As of 9/29/2017 Only P and R are passed correctly.
C
C      COMMON /PARAMD/ P
C      COMMON /INPUT/ R,B
C      COMMON /DESCR/ AGE,HEIGHT,ISEX,IETHFLG
C      COMMON /CNST2/ NPL,NUMEQT,NDRUG,NADD
C
C      DIMENSION X(NDIM),XP(NDIM),P(max_ODE_params),R(max_RS_J),B(20),CV(26),RATEIV(max_input_dim) 
C
C         DO I = 1,NDRUG
C           RATEIV(I) = R(2*I - 1)
C         END DO
C         DO I = 1, NADD
C           CV(I) = R(2*NDRUG + I)
C         END DO  
C End Code in DIFFEQ
C
C Based on above:
C   rpar(1:23) = reserved 
C   rpar(24:55) = P(1:max_ODE_params) -- CORDEN(IG,1:NVAR)
C   rpar(56:92) = R(1:max_RS_J) -- rateiv + covariates
C
C wmy2017Sep29 :: NOTE ::  What about B, and /DESCR/ and /CNST/
C
C wmy2017Sep25 --
C      write (*,*) "USER->DVODE",JSUB,IG,TIN,TOUT,NDIM,MF
C      write (*,*) "PX",JSUB,IG,P(1),P(2),P(3),P(4),P(5),P(6)
C      write (*,*) "R ",JSUB,IG,R(1),R(3),R(4),R(5),R(6),R(7)

c wmy2017Sep27 -- Edited PMetrics::makeModel() to
C   read P(III) from RPAR(23 + III, III = 1:max_ODE_params), and
C   read R(III) from RPAR(55 + III, III = 1:max_RS_J)
        do III=1,max_ODE_params
           RPAR(k_dvode_reserved + III) = P(III)
        end do
        do III = 1,max_RS_J
           RPAR(k_p_end + III) = R(III)
        end do
           RPAR(k_jsub) = dble(JSUB)
           RPAR(k_ig) = dble(IG)
C wmy2017Oct02 -- 
        do III = 1,10
           IPAR(i_dvode_reserved + III) = INTLIST(III)
        end do
           IPAR(i_jsub) = JSUB
           IPAR(i_ig) = IG

C      write (*,*) "RPAR",JSUB,IG,RPAR(24),RPAR(25),
C     1   RPAR(26),RPAR(27),RPAR(28),RPAR(29)
C      write (*,*) "RPAR",JSUB,IG,RPAR(56),RPAR(58),
C     1   RPAR(59),RPAR(60),RPAR(61),RPAR(62)

c wmy2017Oct03 Note that Parallel.eq.true->(3,0,3)
c        write (*,*) "NDIM =",NDIM, N, INTLIST(9)
C
C wmy2017)ct05 -- Program gets to here fine; but note
C

c       write (*,*) "In USERANAL"

c      write (*,*) JSUB,IG, "FUNC->DVODE",
c     1  TIN,X(1),X(2),X(3),TOUT,RTOL,ATOL(1)

        if (TIN.eq.TOUT) write (*,*) "WARNING: TIN=TOUT; JSUB=",JSUB

        CALL DVODE(DIFFEQ,NDIM,X,TIN,TOUT,ITOL,RTOL,ATOL,ITASK,ISTATE,
     1            IOPT,RWORK,LRW,IWORK,LIW,JACOB,MF,RPAR,IPAR)

c wmy2018August29
C        if (JSUB.eq.1) then
C          if ((IG.eq.40012).or.(IG.eq.120034).or.(IG.eq.160045)
C     1        .or.(IG.eq.280075)) then
C            write (*,*) IG,"IG",TOUT,(X(iii),iii=1,3)
C          end if
C        end if

        IF (ISTATE .LT. 0) THEN
C         WRITE(*,16) ISTATE
C 16      FORMAT(///' On return from DVODE, ISTATE =',I3)
         IPAR(i_skip_ig) = 0
        ENDIF



        TIN=TOUT

        RETURN
        END
C

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE JACOB(NDIM, T, X, ML, MU, PD, NRPD, RPAR, IPAR)

        use npag_utils, only: max_RS_J,max_ODE_params,max_ODE_comps

	IMPLICIT none
C REAL*8(A-H,O-Z)

! NEW PARALLEL CODE BELOW AS OF npageng28.f
C !$omp Threadprivate(/PARAMD/,/INPUT/)


C  THIS ROUTINE IS CALLED BY LINPACK ROUTINE DVODE (WHICH IS CALLED
C  BY ROUTINE USERANAL). THE USER CODES THE JACOBIAN MATRIX CALCULATIONS
C  OF THE MODEL (I.E., THE PARTIAL DERIVATIVES OF XP(I) W.R.T. X(I),
C  WHERE XP(I) WERE CODED INTO ROUTINE DIFFEQ).


C  SINCE THIS ROUTINE CAN'T BE MADE BY THE 'BOXES' PROGRAM AT THIS TIME,
C  IT WILL NOT BE USED. IT IS JUST A DUMMY ROUTINE, NEEDED BECAUSE
C  DVODE EXPECTS TO 'SEE' IT.

C  INPUT ARE:

C  NDIM = NO. OF STATES (DIMENSION OF PROBLEM).

C  T = CURRENT TIME.
C  X(I) = VALUE OF STATE I AT T, I=1,NDIM.
C  [ML,MU] = HALF BANDWIDTH PARAMETERS ... UNNEEDED IF MF = 21 OR 22
C            --> FULL JACOBIAN IS PROVIDED BY USER BELOW (SEE 
C	     DESOLV3.FOR CODE FOR DETAILS).
C	     NOTE THAT SINCE MF = 21 OR 22 IN THIS CASE, NRPD = NDIM.
C  R AND B VIA COMMON/INPUT.

        integer NDIM, NRPD
        double precision T,X,ML,MU
        double precision, dimension(:,:) :: PD
        double precision, dimension(257) :: RPAR
        integer, dimension(257) :: IPAR


C  OUTPUT ARE:


C  PD(I,J) = PARTIAL DERIVATIVE OF XP(I) W.R.T. X(J), WHERE XP(I)
C	     ARE CALCULATED IN ROUTINE DIFFEQ ABOVE.

        RETURN
        END							  
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
c  idm2x18.f                                               10/9/15

c  idm2x18 has the following change from idm2x17:

c  Comments regarding NPP now indicate that it = NVAR+NOFIX+NRANFIX,
c  rather than just NVAR+NOFIX. There are no functional changes to
c  this new module.

c-----------------------------------------------------------------------

c  idm2x17.f                                               3/11/15

c  idm2x17 has the following change from idm2x16:

c  All numbers written out in F or G format are now tested to see if
c  they are inside [-1.D-99, 1.D-99]. If so, they are changed to be
c  0. The reason is that otherwise they will be printed out without 
c  the accompanying D or E (e.g., as .934-106, rather than .934E-106).

c  Note that this module is linked with npageng30.f initially.

c-----------------------------------------------------------------------

c  idm2x16.f                                               11/21/14

c  idm2x16 has the following changes from idm1x15:

c  It has the Threadprivate statements to make it compatible
c  with the new npageng28.f program. These statements allow the 
c  program to be run in parallel. 

c-----------------------------------------------------------------------

c  idm2x15.f                                               7/21/14

c  idm2x15 has the following change to idm2x14:

c  If the program stops unexpectedly with the writing of format 111
c  in Subroutine FUNC2, this same comment will now be written to
c  the file, ERRFIL, which is passed to FUNC2 in COMMON/ERR.

c-----------------------------------------------------------------------

c  idm2x14.f                                               3/6/14

c  idm2x14 has the following changes from idm2x13:

c  1. In Subroutine FUNC2, the dimensions related to the no. of output
c  equations have been changed from 6 to NUMEQT OR MAXNUMEQ (see 
c  comments in that routine).

c  2. In Subroutine FUNC2, the dimensions of 6 in XSTORE and XPRED have
c  been changed to max_ODE_comps, as they should have been all along (i.e., this
c  represents the maximum no. of compartments allowed).

c  3. For clarity, the argument in EVAL2 has been changed from Y to
c  YPRED. For the same reason, the 2nd argument in FUNC2 has been
c  changed from F to YPRED.

c  4. The argument list to IDCALCY has the additional argument,
c  NUMEQT, so that YPRED can now be variably dimensioned. For the
c  same reason, NUMEQT has been added to the argument list of 
c  Subroutines EVAL2 and FUNC2.

c-----------------------------------------------------------------------

c  idm2x13.f                                               10/11/12

c  idm2x13 has one correction from idm2x12:

c  THE R(.) ARE SET = RS(.,.) BEFORE GETIX IS CALLED IN THE TIME RESET
c  SECTION OF SUBROUTINE FUNC2. NOT DOING THIS WOULD MEAN THAT IF THE 
C  INITIAL CONDITIONS FOR THE X(.) ARE FUNCTIONS OF THE COVARIATES
C  (ESTABLISHED IN GETIX FROM THE R(.) VALUES), THEY WOULD BE ASSIGNED
C  VALUES BASED ON COVARIATES FROM A PREVIOUS DOSAGE LINE IN THE
C  PATIENT'S DATA FILE, RATHER THAN THE LINE WHICH IS THE DOSE RESET
C  LINE.

c-----------------------------------------------------------------------

c  idm2x12.f                                               9/27/12

c  idm2x12 has the following bug correction to idm2x11:

C  IN SUBROUTINE FUNC2, BEFORE
C  THE FIRST CALL TO GETFA, THE R(.) ARE SET = RS(.,.) IN CASE ANY OF
C  THE FA(.) ARE FUNCTIONS OF THE COVARIATES WHICH ARE ESTABLISHED FROM
C  THE R(.) VALUES IN  GETFA. IN ADDITION, PRIOR TO THE 2 SECTIONS WHERE
C  THE FA(.) ARE USED, GETFA IS CALLED SO THAT THE FA(.) ARE UPDATED TO
C  CURRENT VALUES, BASED ON THE MOST RECENT COVARIATE VALUES IN THE
C  PATIENT'S DATA FILE. IN PREVIOUS PROGRAMS, IT WAS SIMPLY ASSUMED
C  THAT THE FA(.) WERE FUNCTIONS OF THE PARAMETERS, BUT NOT THE

C  COVARIATES, AND SO THIS WASN'T NECESSARY. BUT THE CODE IN 
C  TSTMULTI.FOR IMPLIES THAT THE FA(.) COULD BE FUNCTIONS OF THE
C  COVARIATES, AND SO THIS CHANGE IS NECESSARY.

C  NOTE THAT SETTING THE R(.) TO RS(.,.) BEFORE THE FIRST CALL TO
C  GETFA ALSO MEANS THE R(.) WILL BE SET BEFORE GETIX AND GETTLAG ARE
C  FIRST CALLED, WHICH AGAIN IS REQUIRED IN CASE THEY ESTABLISH VALUES
C  AS FUNCTIONS OF THE COVARIATES IN THE PATIENT DATA FILE.

c-----------------------------------------------------------------------

c  idm2x11.f                                               7/25/12

c  idm2x11 has the following change to idm2x10:

c  In SUBROUTINE FUNC2, the code to save ND0, SIGO, RSO, is moved to
c  before the IF(N .EQ. 0) GO TO 75  statement. The reason is that 
c  before this  routine returns, ND, SIG, and RS are reset back to these
c  values, even if N = 0, and so they must be established at this time.

c-----------------------------------------------------------------------

c  idm2x10.f                                               4/14/12

c  idm2x10 has the following changes to idm2x9.f:

c  It is to be used with npageng17.f, which allows steady state doses
c  to be boluses as well as IVs. As a result, an additional parameter,

c  ISKIPBOL, is used so, in Subroutine FUNC, when convergence occurs in
c  a steady state dose set, the last bolus from that set will not be
c  reapplied below label 83.

c-----------------------------------------------------------------------

c  idm2x9.f                                               3/2/12

c  idm2x9 has the following bug fix to idm2x8.f. In Subroutine FUNC2, 
c  the code to save ND, SIG, and RS before altering them if there are 
c  time lag parameters (in the call to GETTLAG) is now executed whether
c  or not there are time lag parameters. The reason is that, with steady
c  state doses, the first SIG(.) time in a steady state dose set is
c  reset to be 0 after the steady state dose is identified. And this
c  time must be reset back to be its original negative value at the end
c  of the routine so that the next time the routine is called, the 
c  program will again know when a steady state dose is coming. 

c-----------------------------------------------------------------------

c  idm2x8.f                                                1/15/12

c  Corrects bug in Subroutine FUNC2 - now time resets are identified
c  by just the observation time = 0 (i.e., the dose time = 0 is
c  no longer required). This is because it is possible for a dose
c  time (especially if there are timelags) to be after the last
c  observation time in a section of the patient file (before a time
c  reset), and if this happens, the program will not be able to
c  identify the observation time of 0 as a time reset.

c-----------------------------------------------------------------------


c  idm2x7.f                                                11/11/11

c  idm2x7 has the same changes to idm2x6 that idm1x7 has from idm1x6
c  (see all the comments in idm1x7.f for explanations). In particular:

c  1. It can accommodate steady state dose regimens.

c  2. All arrays related to doses (SIG,SIGO,RS,RSO, and BS) in
c  Subroutine FUNC have their 500's changed to 5000's.

c  3. Near the top of Subroutine FUNC, R(1)=0.0D0 is replaced by setting
c  R(2*I-1) = 0.D0, for I = 1,NDRUG. This should have been done when
c  the program became a multi-drug program (see comment in FUNC).

c  4. A time reset no longer requires all initial compartment amounts
c  to be reset to 0. This is because a time reset no longer has to mean
c  an "infinite" amount of time has occurred with no dosing; it can also
c  now mean an "infinite" amount of time has occurred with unknown 
c  dosing. So Subroutine GETIX will be called to establish initial
c  conditions for the new time period (these initial values can of
c  course be 0's as was always assumed in previous programs). This is
c  the situation where a patient, who previously had doses and
c  observations which were recorded while he was in a lab, goes home and
c  gets unknown doses over a long time period, and then returns to the
c  lab to get a new set of doses and observations, starting with 
c  observations which establish his initial conditions for this new
c  time period.

c-----------------------------------------------------------------------

c  idm2x6.f                                                12/20/10

c  idm2x6 has the following change to idm2x5:

c  In Subroutine FUNC2, it has code that calls Subroutine ANAL3, rather
c  than USERANAL if N .EQ. -1. Also, the code to reset X(I),I=1,N to 0
c  where there is a time reset now includes extra code to set 
c  X(I),I=1,3 to 0 if N .EQ. -1.

c  Note that ANAL3, and the routines it calls are from the Little NPAG
c  program module, IDPC9A.FOR. 

c  Note that this module is linked first with bigmlt11.f, and the 
c  template model file is TSTMULTH.FOR (in which in Subroutine SYMBOL,
c  the user is told to code N=-1 if he wants to assume the standard
c  3-compartment linear model with analytic solutions, and in this 
c  case also establish the 5 parameters, {KE,KA,KCP,KPC,V} of this
c  model).

c-----------------------------------------------------------------------



c  idm2x5.f							4/03/10

c  idm2x5 has a bug correction to idm2x4. In Subroutine FUNC2, in the
c  IF(TIM(KNT) .EQ. 0.D0 .AND. SIG(KNS) .EQ. 0.D0) block, the time,
c  T, is also reset = 0 since the integration will again start from
c  time 0. When this wasn't done (in idm2x4.f), the results were
c  unpredictable (depending on how the DVODE integration routines
c  treated a (T,TOUT) pair which decreased rather than increased.

c-----------------------------------------------------------------------




c  idm2x4.f							11/23/09

c  idm2x4 fixes a bug in the idm2x3 code. Label 75 is moved to in
c  front of the  CALL GETTLAG(TLAG)  statement (see the reason in
c  that part of the code).

c-----------------------------------------------------------------------

c  idm2x3.f							9/18/09

c  idm2x3 has the following changes from idm2x2:

c  1. The TLAG and FA vectors, and the initial values for the X array 
c  will be set by calling new routines (GETTLAG, GETFA, and GETIX, 
c  respectively) that are part of the model file (the new template is 
c  TSTMULT.FOR). This means the user can now code explicit formulas
c  for these values. As a result, all reference to NTLAG, IC, IFA, and
c  IVOL have been removed.

c  2. The shift subroutine will now be from the module, shift5.f, 
c  rather than shift4.f.

c  Note that this module, along with idm1x3.f, id3x3.f, and shift5.f
c  are part of the new "engine", whose main module is bigmlt4.f.

c-----------------------------------------------------------------------

c  idm2x2.f							8/14/09

c  idm2x2 has the following changes from idm2x1:

c  1. The code for setting initial compartment amounts from initial
c  compartment concentrations is changed to reflect the fact that
c  now IC(2) refers to the index of the covariates, not the
c  column no. of RS (see comment in code).

c  2. The code to establish the timelag parameters has changed to
c  reflect that NTLAG(I) can now be negative --> in Subroutine
c  SHIFT, the associated timelag parameter will now be the 
c  exponent of the indicated parameter (rather than the parameter 
c  itself).

c  3. The code to establish the FA parameters has changed to
c  reflect that IFA(I) can now be negative --> the associated FA
c  parameter will now be the exponent of the indicated parameter 
c  (rather than the parameter itself).


c  idm2x2.f (along with other new modules idm1x2.f and idm3x2.f) are
c  still called by bigmlt2.f, but are part of the "engine" for the
c  new NPBIG15B.FOR program.

c-----------------------------------------------------------------------

c  idm2x1.f							5/27/09

c  idm2x1.f has the following changes from idcy_53f.f:

c  1. It allows the extra option of setting initial compartment 
c  amounts from their initial concentrations - see code in Subroutine 
c  FUNC2.

c  2. It is part of the new Big NPAG "engine", bigmlt2.f, which allows 
c  patient data files to have "reset" values of 0 in the dosage and 
c  sampling blocks. Whenever, in Subroutine FUNC2, the program sees a 
c  SIG(.) = 0 and a TIM(.) = 0, it knows that a large enough time has 
c  passed since the last dose that all compartment amounts are to be 
c  reset = 0. Subsequent dose and observed value times are then values 
c  from this point.

c  3. The first argument to Subroutine OUTPUT is changed from 0.0 to 
c  0.D0 in two places.

c  This module, along with idm1x1.f and idm3x1.f are first used in the 
c  bigmlt2.f program.

c-----------------------------------------------------------------------

c  idcy_53g.f							5-28-02

c  idcy_53g has the following changes from idcy_53f:

c  It allows multiple drug inputs (rather than just one drug input).
c  The changes required for this are:

c  1. BS has dimension change from (500,3) to (500,7)
c  2. COMMON/CNST2 is changed to include NDRUG (no. of drugs) and
c     NADD (no. of additional covariates), rather than NBI and NRI.
c  3. NTLAG is now a vector instead of a scalar. In particular, 
C     NTLAG(I) = 0 IF DRUG I'S BOLUS COL. HAS NO TIMELAG PARAMETER;
C                K IF DRUG I'S BOLUS COL. HAS A TIMELAG WHOSE VALUE IS
C		   GIVEN BY PARAMETER NO K.
C  4. IFA, PASSED IN COMMON/FRABS FROM SUBROUTINE SYMBOL IS NOW A VECTOR
C     INSTEAD OF A SCALAR.
C     IFA(I) = 0 IF DRUG I WILL HAVE FA = 1.0.
C              K IF DRUG I WILL HAVE AN FA WHOSE VALUE IS TO BE GIVEN
C                BY PARAMETER K.
C  5. THE BOLUS COMPARTMENT NOS., NBCOMP(I), NOW COME VIA 
C     COMMON/BOLUSCOMP FROM SUBROUTINE SYMBOL, AND THE DIMENSION OF
C     NBCOMP HAS BEEN CHANGED TO 7 (MAXIMUM OF 1 PER DRUG) FROM 20.
C  6. ALL OF THE CODE IN SUBROUTINE FUNC2 RELATED TO NRI AND NBI HAS 
C     BEEN CHANGED TO BE IN TERMS OF NI AND NDRUG. 
C  7. THE CODE RELATED TO CALLING SUBROUTINE SHIFT, INCLUDING THE 
C     CALLING ARGUMENTS, HAS BEEN CHANGED TO REFLECT THE ABOVE CHANGES
C     IN NTLAG (I.E., IT IS NOW A VECTOR RATHER THAN A SCALAR). A NEW
C     MODULE, shift3.f (WHICH REPLACES shift2.f) WILL BE LINKED WITH 
C     THIS MODULE.

C-----------------------------------------------------------------------

c  idcy_53f.f							4-23-02

c  idcy_53f has the following changes to idcy_53e:

c  1. To enable FA to be a parameter value (either fixed or random), 
c  rather than always be hardcoded = 1.0, the following changes are
c  implemented ...

c  The hardcoding of FA = 1.0 and the code for NBCOMP are removed
c  from main. In addition, COMMON/BCOMP is removed from the entire 
c  module. Instead, in SUBROUTINE FUNC2, a new COMMON/FRABS/IFA provides 
c  the value IFA which is the parameter index of the FA value (passed
c  from SUBROUTINE SYMBOL) unless it = 0, in which case FA is
c  set = 1.0. Also the NBCOMP compartment nos. are now set in 
c  SUBROUTINE FUNC2.

c  2. COMMONS /OBSER AND /SUM2 (and the arrays in them) are deleted from 
c  main. They were not needed. Also, COMMON CNST2 is deleted from main
c  since NBI is no longer needed here (since NBCOMP code is removed -
c  see no. 1. above).

c-----------------------------------------------------------------------

c  idcy_53e.f							1-22-00

c  idcy_53e has the following changes to idcy_53d:

c  It allows the initial conditions of the amounts in the compartments
c  to be paramater values, rather than fixed at 0.0. These parameter
c  values may be either fixed or random.

c  To affect this enhancement, the primary change is the code in 
c  subroutine FUNC2 which sets the initial conditions based on the 
c  values in IC which are provided by COMMON/INITCOND from 
c  SUBROUTINE SYMBOL of the Fortran model file.

c  There are many other changes to simply the code (i.e., a lot of
c  code was leftover code which was unused and/or confusing), namely:

c  - Commons ADAPT1, ADAPT2, LPARAM, PRED, TRANS, and PARAM are 
c    deleted. Variables ISW, IP, and C are deleted.
c  - COMMON/PARAMD/P is now in MAIN, FUNC, and JACOB of idfix5e.f; 
c    MAIN and FUNCx of idcy_53e.f and idcy_63e.f; and DIFFEQ and OUTPUT 
c    of the Fortran model file.
c  - P is redimensioned max_ODE_params. It will hold only the parameters of the
c    model (although some of those parameters may be initial conditions)
c    and there are max_pop_rand_varbs allowable random paramaters and max_pop_params allowable
c    fixed paramaters now.
c  - All the code to reverse the paramater order (using PD) and to do
c    and undo square root transformations in MAIN and FUNC2 is removed
c    (it was unneeded, and therefore confusing). In particular, all
c    references to NPT, NUMYES, NUIC, NUP, NPNL, and NBOT are removed.
c  - COMMON ANALYT/IDIFF is removed. IDIFF is unneeded since IDIFF = 0
c    is equivalent to N = 0, and so IDIFF code in FUNC2 is replaced by
c    the equivalent code for N. NEQN is replaced by N.
c  - In SUBROUTINE EVAL2, COMMON/PARAM is removed, along with PP and P.
c    Setting PP(I) = P(I), I=1,NPNL made no sense since PP wasn't used
c    and NPNL was always = 0 anyway.
c  - In FUNC2, the If statment at label 83 is changed to include 
c    N .EQ. 0 since if N = 0, setting compartment values is unnecessary.

c  idcy_53e is part of the big npem program, npbig4.f.

        SUBROUTINE IDCALCY(JSUB,IG,NPP,NDIM,ESTML,YPRED,NUMEQT,NOBSER,
     1    MF,NBCOMP,RTOL,ATOL,TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     2    INTLIST,IPAR,ObsError,RPAR,ERRFIL)

c wmy2017Sep29
C   note: IDPC updates SUMSQJ; IDCALCY updates YPRED 
C

C  INPUT ARE:

C  NPP = NO. OF PARAMETERS (RANDOM, FIXED, AND RANFIX) IN THE PARAMETER
C        VECTOR, ESTML.
C  NDIM = NO. OF COMPARTMENTS IN THE MODEL.
C  ESTML = VECTOR OF PARAMETER ESTIMATES (RANDOM AND FIXED). PX in main

C  INFORMATION FROM A SUBJECT DATA FILE WHOSE INFO IS PASSED TO THE 
C  ROUTINES IN THIS MODULE VIA COMMONS /OBSER/, /CNST/, /CNST2/, AND 
C  /SUM2/.


C  OUTPUT IS:

C  YPRED(I,J), I=1,M; J=1,NOS = THE PREDICTED VALUE FOR THE ITH 
C	OBSERVATION OF THE JTH OUTPUT EQUATION, GIVEN THE INPUT VECTOR
C	ESTML. M AND NOS ARE INPUT TO THIS MODULE VIA COMMONS SUM2 AND
C	CNST2, RESPECTIVELY.

c-----------------------------------------------------------------------

       USE npag_utils, only: maxnumeq, max_m_per_obs, max_ODE_params
     1    , max_doses, max_ODE_comps, max_RS_J, max_input_dim
     2    , k_ig, k_jsub, k_dvode_reserved, k_p_end, i_ig, i_jsub

C        IMPLICIT REAL*8(A-H,O-Z)

C        parameter( MAXNUMEQ=7 )

        integer JSUB, IG, NPP, NDIM
        double precision, dimension(max_ODE_params) :: ESTML
        double precision, dimension(max_m_per_obs,NUMEQT) :: YPRED
        integer NUMEQT, NOBSER, MF
        integer, dimension(max_input_dim) :: NBCOMP
        double precision  RTOL
        double precision, dimension(max_ODE_comps) :: ATOL
        double precision, dimension(max_m_per_obs) :: TIMCOPY
        double precision, dimension(max_doses) :: SIGCOPY
        double precision, dimension(max_doses,max_RS_J) :: RSCOPY
        double precision, dimension(max_doses,max_input_dim) :: BSCOPY
        integer, dimension(128) :: INTLIST
        integer, dimension(257) :: IPAR
        double precision, dimension(max_m_per_obs,MAXNUMEQ) :: ObsError
        double precision, dimension(257) :: RPAR
        character ERRFIL*20

C --- These blocks were initialized here, before. /PARAMD/ is
C   now  initialized in main prior to CALL IDCALCY, and /CNST/N,NP
C   are not initialized. But the values of N and NP are carried 
C   by the arguments NDIM and NPP, which are passed directly to FUNC2()
C        COMMON/PARAMD/ P
C        COMMON/CNST/ N,ND,NI,NUP,NUIC,NP
C        double precision, dimension(max_ODE_params) :: P

! NEW PARALLEL CODE BELOW AS OF npageng28.f.
C !$omp Threadprivate(/PARAMD/)
c !$omp Threadprivate(/PARAMD/,/CNST/)

C !$omp ThreadPrivate( IPAR, ObsError )

C*****INITIALIZE PROGRAM*****

c        write (*,*) "CALL SYMBOL; NPP, NOBSER =",NPP,NOBSER
C wmy2018.10.16 moved CALL SYMBOL to main
C      CALL SYMBOL(NBCOMP)

C  THE ABOVE CALL OBTAINS INFO FROM COMMONS.

C  NOTE THAT THIS PROGRAM NOW GETS N = NDIM AND NPP = NVAR+NOFIX+NRANFIX
C  AS CALLING ARGUMENTS. wmy2018Jan10 Which is to say that the inisialization
C  of N and NP in symbol are overwritten here. SYMBOL initializes, N, NP,
C  /BOLUSCOMP/, and the parameter names to their respective values.

c      N = NDIM
c      NP = NPP

C  CALCULATE THE OUTPUT CONCENTRATION VECTOR, Y, FOR THE PARAMETER
C  VECTOR, ESTML.

C  PUT MODEL PARAMETER VALUES INTO P.

C wmy2018Jan11 -- COMMON blocks are being removed; but note that
c   P() must be replaced w/ESTML in the called subroutines!
c        DO I=1,NP
c        DO I=1,NPP
c          P(I) = ESTML(I)
C           write (*,*) ESTML(I)
c        END DO

C  CALL SUBROUTINE EVAL2 TO GET Y, EVALUATED
C  AT ESTML(I) AS DEFINED ABOVE.

c        write (*,*) "/PARAMD/P initialized; Calling EVAL2"
c        write (*,*) "(NPP,NOBSER,NDIM)",NPP,NOBSER,NDIM
c        write (*,*) "BUG 9/18/2017 NOBSER=0, not 8 (Ashley's Greco)"

C--- wmy2018Jan11 Included RSCOPY and BSCOPY in arguments to EVAL2; but
c  then realized that EVAL2 does nothing more than call FUNC2; so just 
c  call FUNC2 from here!
c	CALL EVAL2(JSUB,IG,NPP,ESTML,YPRED,NUMEQT,NOBSER,NDIM,MF,RTOL,
c     1      ATOL,RSCOPY,BSCOPY,INTLIST,IPAR,ObsError)
c
        CALL FUNC2(JSUB,IG,NOBSER,YPRED,NUMEQT,NBCOMP,
     1      NPP,ESTML,NDIM,MF,RTOL,ATOL,INTLIST,
     2      TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     3      ObsError,IPAR,RPAR,ERRFIL)

C        write (*,*) "Returning to NPAG"

        RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE FUNC2(JSUB,IG,M,YPRED,NUMEQT,NBCOMP,
     1      NPP,ESTML,NDIM,MF,RTOL,ATOL,INTLIST,
     2      TIM,SIG,RS,BS,
     3      ObsError,IPAR,RPAR,ERRFIL)

C wmy2017Sep13 In above, Local variable M is NOBSER, which is
C   passed through from main. NPP and ESTML are NPX and PX in
C   other parts of program (number of params, and param array
C   for the current support point), remaining params are from
C   COMMON /TOUSER/, and are now being passed through from main.

C  THIS SUBROUTINE, CALLED BY EVAL2, FINDS YPRED(I) = OUTPUT CONC. AT
C  TIME I, I=1,M, GIVEN PARAMETER VALUES IN P.

       USE npag_utils, only: verifyval, shift, thesame, predlast3
     1   , maxnumeq, max_m_per_obs, max_SS_doses
     2   , max_ODE_params, max_doses, max_ODE_comps, max_RS_J
     3   , max_input_dim, k_dvode_reserved, k_p_end, k_jsub, k_ig
     4   , i_ig, i_jsub, i_dvode_reserved

      IMPLICIT REAL*8(A-H,O-Z)

C wmy2019.03.12 SR SHIFT moved into npag_utils.f90
C      include "interface_0SHIFT.txt"

C TIM,SIG,RS,BS are used; but YO is not.
C arguments <TIM,SIG,RS,BS>COPY replace <TIM,SIG,RS,BS>
C      COMMON/OBSER/ TIM,SIG,RS,YO,BS
C      COMMON/BOLUSCOMP/NBCOMP
C      COMMON/CNST/ N,ND,NI,NUP,NUIC,NP
C      COMMON/CNST2/ NPL,NOS,NDRUG,NADD
C      COMMON/INPUT/ R,B
C      COMMON/PARAMD/ P
C      COMMON/ERR/ERRFIL
C      COMMON/STATE/ X  ! unix> grep COMMON * | grep STATE suggests /STATE/ is not used
C
C INTLIST(:), RPAR(:), and IPAR(:) are used to pass in all necessary arguments and params
C
C INTLIST(1) = int(AGE)
C INTLIST(2) = ISEX
C INTLIST(3) = int(HEIGHT)
C INTLIST(4) = IETHFLAG
C INTLIST(5) = /CNST2/NDRUG
C INTLIST(6) = /CNST2/NADD = No. Additional Covariates
C INTLIST(7) = /CNST/NI = 2*NDRUG+NADD
C INTLIST(8) = /CNST/ND ; ND =  NDO
C INTLIST(9) = /CNST2/NOS (Called /CNST2/NUMEQTT in SUBROUTINE FILRED)
C INTLIST(10) = /CNST2/M = NOBSER


C      PARAMETER(MAXNUMEQ=7)

! NEW PARALLEL CODE BELOW AS OF npageng28.f.
C !$omp Threadprivate(/BOLUSCOMP/)
C !$omp Threadprivate(/PARAMD/,/INPUT/,/STATE/)
C !$omp Threadprivate(/OBSER/,/CNST2/)
C !$omp Threadprivate(/CNST/)

C      double precision, dimension(max_m_per_obs) :: TIM
C      double precision, dimension(max_doses) :: SIG
C      double precision, dimension(max_doses,max_RS_J) :: RS
C      double precision, dimension(max_m_per_obs,MAXNUMEQ) :: YO
C      double precision, dimension(max_doses,max_input_dim) :: BS
      integer N,ND,NI,NUP,NUIC,NP
      integer NOS,NDRUG,NADD

C----- Below are converted to local variables
      double precision, dimension(max_RS_J) :: R
      double precision, dimension(max_ODE_comps) :: B
C      double precision, dimension(max_ODE_params) :: P
      CHARACTER ERRFIL*20

C Above replaces declaration below
C  Note that above are all COMMON parameters; while local parameter
C  declarations follow.
C      DIMENSION X(max_ODE_comps),P(max_ODE_params),TIM(max_m_per_obs),SIG(max_doses),SIGO(max_doses),R(max_RS_J),
C     1 RS(max_doses,max_RS_J),RSO(max_doses,max)RS_J),YT(MAXNUMEQ),YO(max_m_per_obs,MAXNUMEQ),
C     2 YPRED(max_m_per_obs,NUMEQT), BS(max_doses,max_input_dim),
C     3 Y(max_m_per_obs,MAXNUMEQ),B(max_ODE_comps),NBCOMP(max_input_dim),
C     3 TLAG(max_input_dim),FA(max_input_dim),XSTORE(100,max_ODE_comps),XPRED(max_ODE_comps),XVERIFY(100)

C------ End of COMMON declarations


C------ Argument List

      integer JSUB,IG,M
      double precision, dimension(max_m_per_obs,NUMEQT) :: YPRED
      integer NUMEQT,NPP
      real*8, dimension(max_ODE_params) :: ESTML
      integer NDIM,MF
      integer, dimension(max_input_dim) :: NBCOMP
      double precision RTOL
      real*8, dimension(max_ODE_comps) :: ATOL
      integer, dimension(128) :: INTLIST
      double precision, dimension(max_m_per_obs) :: TIM
      double precision, dimension(max_doses) :: SIG
      double precision, dimension(max_doses,max_RS_J) :: RS
      double precision, dimension(max_doses,max_input_dim) :: BS
      double precision, dimension(max_m_per_obs,MAXNUMEQ) :: ObsError
      integer, dimension(257) :: IPAR
      double precision, dimension(257) :: RPAR

C------ Local variables
c  (apply SAVE attribute to params that are passed to subroutines)

      double precision, save, dimension(max_ODE_comps) :: X, XPRED
      double precision, save, dimension(100) :: XVERIFY
      double precision, save, dimension(max_SS_doses,max_ODE_comps)
     1  :: XSTORE
      double precision, save, dimension(max_doses) :: SIGO
      double precision, save, dimension(max_doses,max_RS_J) :: RSO
      double precision, save, dimension(max_input_dim) :: TLAG, FA
      double precision, save, dimension(MAXNUMEQ) :: YT
      double precision, save, dimension(max_m_per_obs,MAXNUMEQ) :: Y 

C ----- Now use INTLIST to initialize counters, if necessary

C INTLIST(1) = int(AGE)
C INTLIST(2) = ISEX
C INTLIST(3) = int(HEIGHT)
C INTLIST(4) = IETHFLAG
C INTLIST(5) = /CNST2/NDRUG
C INTLIST(6) = /CNST2/NADD = No. Additional Covariates
C INTLIST(7) = /CNST/NI = 2*NDRUG+NADD
C INTLIST(8) = /CNST/ND ; ND =  NDO
C INTLIST(9) = /CNST2/NOS (Called /CNST2/NUMEQTT in SUBROUTINE FILRED)
C INTLIST(10) = /CNST2/M = NOBSER

C      COMMON/CNST/ N,ND,NI,NUP,NUIC,NP
       N = NDIM
       ND = intlist(8)
       NI = intlist(7)
C       NUP =
C       NUIC =
       NP = NPP

C      COMMON/CNST2/ NPL,NOS,NDRUG,NADD
       NDRUG = intlist(5)
       NADD = intlist(6)
       NOS = intlist(9)

C  NOTE THAT AS OF idm2x14.f, THE DIMENSIONS OF 6 IN XSTORE AND XPRED
C  HAVE BEEN CHANGED TO max_ODE_comps, WHICH IS WHAT THEY SHOULD HAVE BEEN ALL
C  ALONG (I.E., THE SAME AS FOR X).

C  NOTE THAT THE DIMENSIONS RELATED TO THE NO. OF OUTPUT EQS. IN
C  YO, YT AND Y ARE CHANGED TO MAXNUMEQ (FROM 6). NUMEQT COULD NOT
C  BE USED BECAUSE THESE ARRAYS WERE NOT PASSED TO THIS ROUTINE AS
C  DUMMY ARGUMENTS.

C  THE 2ND DIMENSION OF YPRED IS CHANGED TO NUMEQT, SINCE IT IS PASSED
C  IN THE ARGUMENT LIST, AND CAN THEREFORE BE VARIABLY DIMENSIONED BY
C  NUMEQT.


C  NOTE THAT "7" IN THE ABOVE ARRAYS INDICATE THE NO. OF DRUGS ALLOWED.


C       write (*,*) "in FUNC2"

C*****ODE CONSTANTS AND INITIALIZATION*****

      do III=1,max_ODE_comps
        X(III)=0.0
      end do

      KNS=1
      KNT=1

C  NOTE THAT KNT IS THE RUNNING INDEX OF THE NEXT OBSERVATION TIME,
C  AND       KNS IS THE RUNNING INDEX OF THE NEXT DOSAGE TIME.

        T=0.0D0

C  INITIALIZE ISKIPBOL = 0. SEE CODE BELOW. IT IS ONLY NEEDED FOR A
C  STEADY STATE DOSE SET WHICH HAS BOLUS DOSES.

      ISKIPBOL = 0


      DO I = 1,NDRUG
       R(2*I-1) = 0.D0
      END DO

c  AS OF idm2x7.f, instead of R(1) = 0, the code has been changed to 
c  set R(2*I-1) = 0, for I = 1,NDRUG. I.E., All IV rates for all NDRUG
c  drugs are initialized to be 0 ... in case the 1st obs. time is 0,
c  which means that OUTPUT is called before the R(I) are set below.

C  CALL SUBROUTINE GETFA IN npemdriv.f (THE FIRST TEMPLATE FILE TO 
C  INCLUDE GETFA IS TSTMULTG.FOR) TO OBTAIN THE VALUE OF FA FOR EACH
C  OF THE NDRUG DRUGS.

C  AS OF idm2x12.f, BEFORE CALLING GETFA, MUST SET
C  THE R(.) IN CASE ANY OF THE FA(.) ARE FUNCTIONS OF THE 
C  COVARIATES WHICH ARE ESTABLISHED FROM THE R(.) VALUES IN
C  GETFA.
 
      DO I=1,NI
       R(I)=RS(KNS,I)
      END DO


c         CALL GETFA(FA,X)
         CALL GETFA(FA,X,ESTML,R,B,INTLIST)


C  NOTE THAT NBCOMP(I),I=1,NDRUG WAS SET IN SUBROUTINE SYMBOL AND
C  PASSED TO THIS ROUTINE VIA COMMON/BOLUSCOMP.


C  As of idm2x11.f, the code to save ND0, SIGO, RSO, is moved to before
c  the IF(N .EQ. 0) GO TO 75  statement. The reason is that before this
c  routine returns, ND, SIG, and RS are reset back to these values,
c  even if N = 0, and so they must be established at this time.

C  AS OF idm2x9.f, SAVE ND, SIG, AND RS WHETHER OR NOT NTL = 1, SINCE
C  IF THERE ARE STEADY STATE DOSE SETS, THE FIRST SIG(.) VALUE IN EACH
C  SET WILL BE CHANGED TO BE 0 BELOW.

         NDO = ND
         DO I=1,ND
           SIGO(I) = SIG(I)
           DO J=1,NI
             RSO(I,J) = RS(I,J)
           END DO
         END DO


C  IF N = 0, THE OUTPUT EQUATION(S) FOR Y ARE CODED EXPLICITLY INTO
C  SUBROUTINE OUTPUT, AND NO D.E. SOLUTIONS (VIA USERANAL/DIFFEQ) ARE
C  TO BE USED. IN THIS CASE, SKIP THE CODE REGARDING INITIAL CONDITIONS
C  OF THE COMPARTMENTS, SINCE THEY ARE IRRELEVANT (I.E., THE COMPARTMENT
C  AMOUNTS DON'T NEED TO BE INITIALIZED SINCE THEY WON'T BE UPDATED BY
C  INTEGRATING D.E.'S). IN FACT, COULD PROBABLY SKIP TIMELAGS TOO, 
C  SINCE THEY CHANGE THE TIME THAT BOLUS DOSES ARE GIVEN, AND THIS
C  THEORETICALLY ONLY AFFECTS COMPARTMENT AMOUNTS (WHICH ARE NOT USED
C  IF N = 0), BUT JUST SKIP INITIAL CONDITIONS FOR NOW.

        IF(N .EQ. 0) GO TO 75


C  CALL SUBROUTINE GETIX IN npemdriv.f (THE FIRST TEMPLATE FILE TO 
C  INCLUDE GETIX IS TSTMULTG.FOR) TO OBTAIN THE VALUE OF X (THE INITIAL
C  COMPARTMENT AMOUNT) FOR EACH OF THE N COMPARTMENTS.

	 CALL GETIX(N,X,ESTML,R,B,INTLIST)



C  CALL SUBROUTINE GETTLAG IN npemdriv.f (THE FIRST TEMPLATE FILE TO 
C  INCLUDE GETTLAG IS TSTMULTG.FOR) TO OBTAIN THE VALUE OF THE TIMELAG
C  FOR EACH OF THE NDRUG DRUGS.

C   75	 CALL GETTLAG(TLAG,X)
   75    CALL GETTLAG(TLAG,X,ESTML,R,B,INTLIST)

C  IF ANY TLAG(.) VALUES RETURN AS .NE. 0, THEN, CALL SUBROUTINE SHIFT
C  TO ADJUST THE DOSAGE REGIMEN APPROPRIATELY.

      NTL = 0
	DO ID = 1,NDRUG
	 IF(TLAG(ID) .NE. 0) NTL = 1
	END DO

	IF(NTL .EQ. 1) THEN

C  STORE INCOMING VALUES IN ND, SIG, AND RS (WHICH CONTAINS BS VALUES)
C  SINCE THEY WILL BE CHANGED IN THE CALL TO SUBROUTINE SHIFT, WHICH 
C  "SHIFTS" THE DOSAGE REGIMEN MATRIX TO ACCOUNT FOR THE TIMELAG 
C  PARAMETER(S), TLAG(I). AT THE END OF THIS ROUTINE, THE VALUES IN ND, 
C  SIG, AND RS WILL BE RESET TO THEIR INCOMING VALUES - TO BE READY FOR 
C  THE NEXT CALL TO THIS ROUTINE WITH POSSIBLY DIFFERENT VALUES FOR 
C  TLAG(I).

	 CALL SHIFT(TLAG,ND,SIG,NDRUG,NADD,RS,INTLIST)


C  ESTABLISH THE VALUES IN EACH DRUG'S PO COLUMN TO THE CORRESPONDING
C  COLUMN IN ARRAY BS.

      DO I=1,ND
       DO J=1,NDRUG
        BS(I,J)=RS(I,2*J)
       END DO
      END DO


	ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(NTL .EQ. 1)  CONDITION.


C       write (*,*) "T,KNS,KNT", T, KNS, KNT

      IF(TIM(KNT).GE.SIG(KNS)) GO TO 12
      IF(TIM(KNT).NE.0.0D0) GO TO 45

C  THE ONLY WAY THE FOLLOWING CALL TO OUTPUT CAN OCCUR IS IF TIM(KNT)
C  = 0 --> OBTAIN YT = OUTPUT VALUE(S) AT TIME 0.0.

      CALL OUTPUT(0.D0,YT,X,RPAR,IPAR)
	DO 2000 I=1,NOS
2000    Y(KNT,I)=YT(I)
        KNT=KNT+1
        GO TO 45

12      IF(TIM(KNT).GT.SIG(KNS)) GO TO 13
        IF(TIM(KNT).NE.0.0D0) GO TO 45

C  THE ONLY WAY THE FOLLOWING CALL TO OUTPUT CAN OCCUR IS IF TIM(KNT)
C  = 0 --> OBTAIN YT = OUTPUT VALUE(S) AT TIME 0.0.

        CALL OUTPUT(0.D0,YT,X,RPAR,IPAR)
	  DO 2005 I=1,NOS
2005    Y(KNT,I)=YT(I)
        KNT=KNT+1

13      IF(SIG(KNS) .GT. 0.0D0) GO TO 45

C  CHECK TO SEE IF SIG(KNS) < 0. IF SO, IT MEANS THAT 100 STEADY STATE
C  DOSES SHOULD NOW BE APPLIED WITH AN INTERDOSE INTERVAL EQUAL TO
C  -SIG(KNS).

      ISTEADY = 0

      IF(SIG(KNS) .LT. 0.D0) THEN

       ISTEADY = 1
       NSET = 1

C  NOTE THAT ISTEADY = 1 TELLS THE PROGRAM BELOW TO PROCEED AS IF THE
C  DOSE TIME IS 0, AND START INTEGRATING THROUGH THE SET OF 100 
C  DOSE SETS, ALL OF WHICH OCCUR BEFORE THE NEXT OBSERVATION TIME ...
C  BUT PAUSE AFTER THE END OF THE 5TH DOSE SET (NSET IS THE RUNNING NO.
C  OF THE CURRENT DOSE SETS THAT HAVE BEEN RUN) AND CALL SUBROUTINE
C  PREDLAST3 TO PREDICT THE STEADY STATE COMPARTMENT AMOUNTS AFTER THE
C  100 DOSE SETS (NOTE THAT THE COMPARTMENT AMOUNTS WILL HAVE TO BE
C  STORED AT THE END OF EACH OF THE STEADY STATE DOSE SETS AS THE LOGIC
C  OF PREDLAST3 REQUIRES). 

C  IF "CONVERGENCE" IS ACHIEVED AT THAT POINT, ASSIGN THE COMPARTMENT 
C  AMOUNTS TO BE THE PREDICTED AMOUNTS, AND ASSIGN KNS TO BE WHAT IT IS
C  WHEN THESE STEADY STATE DOSE SETS HAVE FINISHED. NOTE THAT THE END OF
C  THE 100TH DOSE SET WILL BE AT TIME 100*(-SIG(KNS)), SO KNS WILL BE 
C  THE INDEX OF THE FIRST DOSE EVENT WHICH OCCURS AFTER THIS TIME.

C  IF "CONVERGENCE" IS NOT ACHIEVED, CONTINUE APPLYING THE LOGIC OF
C  PREDLAST3 UNTIL IT IS ACHIEVED, OR UNTIL THE 100 DOSE SETS ARE ALL
C  INTEGRATED THROUGH, WHICHEVER COMES FIRST.

       DOSEINT = -SIG(KNS)

C  RESET SIG(KNS) TO BE 0 SINCE THIS DOSE EVENT REPRESENTS THE START
C  OF 100 DOSE SETS THAT BEGIN AT TIME 0.


       SIG(KNS) = 0

      ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(SIG(KNS) .LT. 0.D0)  CONDITION.


      DO I=1,NI
       R(I)=RS(KNS,I)
      END DO

	IF(NDRUG .EQ. 0) GO TO 81

C  AS OF idm2x11.f: MUST CALL GETFA BEFORE EVERY TIME THAT
C  FA(.) ARE USED IN CASE THE EQUATION(S) FOR THE FA(.) ARE BASED
C  ON THE COVARIATES, WHICH CAN CHANGE DOSE TO DOSE.

c         CALL GETFA(FA,X)
         CALL GETFA(FA,X,ESTML,R,B,INTLIST)


        IF(N .EQ. 0) GO TO 120

        DO I=1,NDRUG
	 X(NBCOMP(I))=X(NBCOMP(I))+BS(KNS,I)*FA(I)
	END DO

C  NOTE THAT FA(I) IS THE FRACTION OF DRUG AVAILABLE FROM A BOLUS INPUT
C  FOR DRUG I INTO ITS ABSORPTIVE COMPARTMENT.

        GO TO 81

120   DO I=1,NDRUG
       B(I)=BS(KNS,I)*FA(I)
      END DO

81      KNS=KNS+1

C*****INTEGRATION OF EQUATIONS*****


C  DETERMINE IF, OBSER(ID=0), OR DOSE(ID=1), OR BOTH(ID=2).

45    IF(KNS.GT.ND) GO TO 15


C CODE CHANGE BELOW FOR idm2x8.f.

      IF(TIM(KNT) .EQ. 0.D0 .AND. KNT .GT. 1) THEN


C  AS OF idm2x7.f, A TIME RESET NO LONGER REQUIRES ALL INITIAL
C  COMPARTMENT AMOUNTS TO BE RESET TO 0. THIS IS BECAUSE A TIME RESET
C  NO LONGER HAS TO MEAN THAT AN "INFINITE" AMOUNT OF TIME HAS OCCURRED
C  WITH NO DOSING; IT CAN ALSO NOW MEAN THAT AN "INFINITE" AMOUNT OF 
C  TIME HAS OCCURRED WITH UNKNOWN DOSING (IN THIS CASE, SUBROUTINE
C  GETIX WILL BE CALLED BELOW TO ESTABLISH INITIAL CONDITIONS FOR THIS
C  TIME PERIOD). 

C  ADVANCE KNS TO THE NEXT VALUE THAT HAS SIG(KNS) .LE. 0. I.E., ONCE
C  TIMN(KNT) = 0, IT MEANS THAT WE ARE DONE WITH THE OUTPUT OBS.
C  TIMES IN THE PREVIOUS SECTION --> THERE IS NO POINT IN CONTINUING
C  TO INTEGRATE TILL THE END OF THE DOSES IN THE PREVIOUS SECTION
C  (IF THERE ARE ANY).

      DO IKNS = KNS,ND
       IF(SIG(IKNS) .LE. 0.D0) GO TO 110
      END DO

C  TO GET HERE MEANS THAT NO VALUE IN SIG(.) FROM KNS TO ND HAS A 
C  VALUE .LE. 0, AND THIS IS AN ERROR. IT MEANS THAT THE PATIENT DATA
C  FILE HAS AN OBSERVATION TIME RESET ROW WITHOUT AN ACCOMPANYING
C  DOSE RESET ROW. TELL THE USER AND STOP.

C  REPLACE WRITING OF SIG() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL.

       XVERIFY(1) = SIG(KNS)
       CALL VERIFYVAL(1,XVERIFY)


C      WRITE(*,111) ND,KNS,SIG(KNS)
       WRITE(*,111) ND,KNS,XVERIFY(1)

C      WRITE(25,111) ND,KNS,SIG(KNS)
       WRITE(25,111) ND,KNS,XVERIFY(1)

 111  FORMAT(//' THE CURRENT SUBJECT HAS AN OBSERVATION TIME RESET'/
     1' ROW WITHOUT AN ACCOMPANYING DOSE RESET ROW. THE PROGRAM NOW'/
     2' STOPS. '//
     3' REVIEW YOUR PATIENT FILES AND CORRECT THE ERROR.'//
     4' NOTE THAT THE ',I4,' DOSE TIMES (POSSIBLY ALTERED BY TIMELAGS'/
     5' ARE THE FOLLOWING (AND THERE IS NO TIME .LE. 0 AFTER TIME'/
     6' NO. ',I4,' WHICH HAS CORRESPONDING TIME ',F15.4,'):')

       OPEN(42,FILE=ERRFIL)
C       WRITE(42,111) ND,KNS,SIG(KNS) 
        WRITE(42,111) ND,KNS,XVERIFY(1)
 

      DO I = 1,ND
       WRITE(*,*) SIG(I)
       WRITE(25,*) SIG(I)
       WRITE(42,*) SIG(I)
      END DO

       CLOSE(42)

      CALL PAUSE
      STOP


  110 KNS = IKNS


C  THERE ARE TWO POSSIBILITES AT THIS POINT, EITHER SIG(KNS) = 0
C  OR SIG(KNS) < 0. 

C  IF SIG(KNS) = 0, THIS REPRESENTS A TIME RESET (T WILL BE SET = 0
C  BELOW) WITH A SINGLE DOSE LINE TO START. IN THIS CASE, CALL GETIX
C  AGAIN (JUST AS WAS DONE NEAR THE TOP OF THIS ROUTINE) TO OBTAIN
C  INITIAL COMPARTMENT AMOUNTS. NOTE THAT BY DEFAULT, IN GETIX, ALL
C  COMPARTMENT AMOUNTS ARE SET = 0 (WHICH WOULD BE THE CASE IF IN THE 
C  LONG TIME PERIOD BETWEEN THE LAST SET OF DOSES AND THIS NEW
C  BEGINNING, NO DOSES HAVE BEEN GIVEN). BUT THE USER MAY ALSO HAVE
C  CODED INTO GETIX EQUATIONS THAT SET ONE OR MORE OF THE X(I) TO
C  FUNCTIONS OF COVARIATE AND PARAMETER VALUES (WHICH WOULD BE THE
C  SITUATION IF AN UNKNOWN DOSING REGIMEN HAS TAKEN PLACE BUT IT
C  DOESN'T MATTER WHAT IT WAS BECAUSE THE PATIENT COMES TO A LAB AND
C  SIMPLY HAS HIS COMPARTMENT VALUES ESTABLISHED BEFORE CONTINUING 
C  WITH THE OTHER VALUES IN HIS PATIENT FILE). 

C  IF SIG(KNS) < 0, THIS REPRESENTS A TIME RESET WITH A STEADY STATE
C  SET OF 100 DOSES ABOUT TO BEGIN. IN THIS CASE, WE ASSUME THAT THE
C  PATIENT IS ABOUT TO GET 100 SETS OF DOSES SO THAT HIS COMPARTMENT
C  AMOUNTS WILL ACHIEVE STEADY STATE VALUES. THESE STEADY STATE VALUES
C  WILL BE ESTIMATED IN THE BLOCK OF CODE BELOW THAT STARTS WITH 
C  IF(ISTEADY .EQ. 1). IN THIS CASE, WE WILL STILL CALL GETIX TO 
C  MAKE SURE THAT ANY RESIDUAL COMPARTMENT AMOUNTS FROM A PREVIOUS
C  SET OF DOSES IS ZEROED OUT (OR SET = VALUES AS DETERMINED BY
C  SUBROUTINE GETIX).

C  AS OF idm2x13.f, BEFORE CALLING GETIX, MUST SET
C  THE R(.) IN CASE ANY OF THE INITIAL CONDITIONS FOR THE X(.)
C  ARE FUNCTIONS OF THE COVARIATES WHICH ARE ESTABLISHED FROM THE 
C  R(.) VALUES IN GETFA.
 
      DO I=1,NI
       R(I)=RS(KNS,I)
      END DO



       CALL GETIX(N,X,ESTML,R,B,INTLIST)
		
C  MUST ALSO RESET T = 0 SINCE THE INTEGRATION WILL AGAIN START FROM 
C  TIME 0.

       T = 0.D0


C  IF SIG(KNS) .LT. 0, THIS IS NOT ONLY A TIME RESET, IT IS THE
C  BEGINNING OF A STEADY STATE DOSE SET. IN THIS CASE, APPLY 100 
C  STEADY STATE DOSES WITH AN INTERDOSE INTERVAL EQUAL TO -SIG(KNS).

      ISTEADY = 0

      IF(SIG(KNS) .LT. 0.D0) THEN

       ISTEADY = 1
       NSET = 1

C  NOTE THAT ISTEADY = 1 TELLS THE PROGRAM BELOW TO PROCEED AS IF THE
C  DOSE TIME IS 0, AND START INTEGRATING THROUGH THE SET OF 100 
C  DOSE SETS, ALL OF WHICH OCCUR BEFORE THE NEXT OBSERVATION TIME ...
C  BUT PAUSE AFTER THE END OF THE 5TH DOSE SET (NSET IS THE RUNNING NO.
C  OF THE CURRENT DOSE SETS THAT HAVE BEEN RUN) AND CALL SUBROUTINE
C  PREDLAST3 TO PREDICT THE STEADY STATE COMPARTMENT AMOUNTS AFTER THE
C  100 DOSE SETS (NOTE THAT THE COMPARTMENT AMOUNTS WILL HAVE TO BE
C  STORED AT THE END OF EACH OF THE STEADY STATE DOSE SETS AS THE LOGIC
C  OF PREDLAST3 REQUIRES). 

C  IF "CONVERGENCE" IS ACHIEVED AT THAT POINT, ASSIGN THE COMPARTMENT 
C  AMOUNTS TO BE THE PREDICTED AMOUNTS, AND ASSIGN KNS TO BE WHAT IT IS
C  WHEN THESE STEADY STATE DOSE SETS HAVE FINISHED. NOTE THAT THE END OF
C  THE 100TH DOSE SET WILL BE AT TIME 100*(-SIG(KNS)), SO KNS WILL BE 
C  THE INDEX OF THE FIRST DOSE EVENT WHICH OCCURS AFTER THIS TIME.

C  IF "CONVERGENCE" IS NOT ACHIEVED, CONTINUE APPLYING THE LOGIC OF
C  PREDLAST3 UNTIL IT IS ACHIEVED, OR UNTIL THE max_SS_doses DOSE SETS ARE ALL
C  INTEGRATED THROUGH, WHICHEVER COMES FIRST.

       DOSEINT = -SIG(KNS)

C  RESET SIG(KNS) TO BE 0 SINCE THIS DOSE EVENT REPRESENTS THE START
C  OF max_SS_doses DOSE SETS THAT BEGIN AT TIME 0.

       SIG(KNS) = 0

      ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(SIG(KNS) .LT. 0.D0)  CONDITION.



	ENDIF

C  THE ABOVE ENDIF IS FOR THE 
C   IF(TIM(KNT) .EQ. 0.D0 .AND. KNT .GT. 1)  CONDITION.




      IF(TIM(KNT) .NE .SIG(KNS)) GO TO 20
      ID=2
      TOUT=TIM(KNT)
      KNT=KNT+1
      KNS=KNS+1

      IF(N .EQ. 0) GO TO 31
      GO TO 30

20    IF(TIM(KNT) .GT. SIG(KNS) .AND. SIG(KNS) .GT. 0) GO TO 25

15    ID=0
      TOUT=TIM(KNT)
      KNT=KNT+1
      IF(N .EQ. 0) GO TO 31
      GO TO 30

25    ID=1
      TOUT=SIG(KNS)
      KNS=KNS+1
      IF(N .EQ. 0) GO TO 31

30      CONTINUE
C--------------------------------------------------------------- 30 / 32
C --- wmy2018.06.15 These lines are lifted from USERANAL; they have to
C --- be here to make ANAL3() work.
C --- When you get a chance, go back to useranal and erase these lines
C --- there as those lines are now redundant.  Also, remove INTLIST
C --- from the USERANAL() arglist
        do III=1,max_ODE_params
C          RPAR(23 + III) = ESTML(III)
          RPAR(k_dvode_reserved + III) = ESTML(III)
        end do
        do III = 1,max_RS_J
C          RPAR(55 + III) = R(III)
          RPAR(k_p_end + III) = R(III)
        end do
        RPAR(k_jsub) = dble(JSUB)
        RPAR(k_ig) = dble(IG)
        do III = 1,10
C          IPAR(23 + III) = INTLIST(III)
          IPAR(i_dvode_reserved + III) = INTLIST(III)
        end do
        IPAR(i_jsub) = JSUB
        IPAR(i_ig) = IG
C        write (*,*) "DEBUG 2018.06.15: RPAR",RPAR(24),RPAR(25),RPAR(26)
C     1     ,RPAR(27),RPAR(28)
C---------------------------------------------------------------

32      IF(N .NE. -1) then
          CALL USERANAL(JSUB,IG,X,T,TOUT,
     1      NDIM,MF,RTOL,ATOL,ESTML,R,INTLIST,IPAR,RPAR)
        endif
        IF(N .EQ. -1) CALL ANAL3(X,T,TOUT,RPAR,IPAR)

C  IF ISTEADY = 1, THIS IS INSIDE A STEADY STATE DOSE SET. CHECK TO SEE
C  IF TOUT IS A MULTIPLE OF DOSEINT. IF SO, RECORD THE COMPARTMENT
C  AMOUNTS. THEN, AFTER COMPARTMENT AMOUNTS HAVE BEEN STORED FOR AT 
C  LEAST THE 1ST 5 MULTIPLES OF DOSEINT, STOP AND CALL SUBROUTINE
C  PREDLAST3 WHICH PREDICTS THE FINAL (STEADY STATE) COMPARTMENT AMOUNTS
C  AFTER THE LAST (100TH) DOSE SET. 

C  IF PREDLAST3 HAS PREDICTED VALUES WHICH "CONVERGE", ASSIGN THE
C  PREDICTED VALUES TO X, INCREASE KNS TO BE THE INDEX OF THE FIRST DOSE
C  EVENT WHICH OCCURS AFTER THE STEADY STATE DOSE SET ENDS AND CONTINUE.

C  IF PREDLAST3 VALUES DON'T CONVERGE, CONTINUE THE PROCESS WITH 
C  COMPARTMENT AMOUNTS FOR MULTIPLES 2 - 6 OF DOSEINT, TEST FOR
C  "CONVERGENCE", ETC. THIS PROCESS CONTINUES UNTIL "CONVERGENCE" IS
C  ACHIEVED FOR A SET OF 5 COMPARTMENT AMOUNTS (OR SETS OF AMOUNTS IF
C  NDRUG IS > 1), OR UNTIL ALL 100 DOSE SETS IN THE STEADY STATE REGIMEN
C  HAVE FINISHED. 

      IF(ISTEADY .EQ. 1) THEN


C  THE NEXT DOSE SET END TIME IS DOSEINT*NSET. IF TOUT = DOSEINT*NSET,
C  STORE THE COMPARTMENT AMOUNTS. IF NSET .GE. 5, CALL PREDLAST3 AND
C  PROCEED AS INDICATED ABOVE.

       CALL THESAME(TOUT,DOSEINT*NSET,ISAME)

       IF(ISAME .EQ. 1) THEN

        NN = N
        IF(N .EQ. -1) NN = 3

        DO J = 1,NN
         XSTORE(NSET,J) = X(J)
        END DO

        IF(NSET .GE. 5) THEN

         CALL PREDLAST3(NN,NSET,XSTORE,XPRED,ICONV)
  
         IF(ICONV .EQ. 1) THEN

C  SINCE THE PREDICTED VALUES ARE CONSIDERED ACCURATE (I.E., 
C  "CONVERGENCE WAS ACHIEVED IN PREDLAST), RESET ISTEADY TO 0,
C  WHICH MEANS THAT THE STEADY STATE DOSES ARE FINISHED; ASSIGN THE
C  COMPARTMENT AMOUNTS TO BE THE PREDICTED VALUES; AND SET KNS TO THE
C  FIRST DOSE EVENT AFTER THE END OF THE STEADY STATE DOSE SET. ALSO,
C  SET T = THE ENDING TIME OF THE STEADY STATE DOSE SET = 100*DOSEINT,
C  SINCE THAT IS WHAT IT WOULD HAVE BEEN HAD ALL 100 DOSE SETS BEEN
C  RUN.


          ISTEADY = 0

          DO J = 1,NN
           X(J) = XPRED(J)
          END DO

          T = 100.D0*DOSEINT

C  ADVANCE KNS TO BE THE FIRST DOSE PAST THE 100 DOSE SETS IN THIS
C  STEADY STATE SET. NOTE THAT THIS SET ENDS BEFORE 100*DOSEINT, SO
C  FIND THE FIRST SIG(.) THAT IS .GE. 100*DOSEINT, OR THAT IS = 0
C  (WHICH SIGNIFIES A TIME RESET) OR THAT IS < 0 (WHICH SIGNIFIES 
C  ANOTHER STEADY STATE SET).

          DO I = KNS,ND
           IF(SIG(I) .GE. 100.D0*DOSEINT .OR. SIG(I) .LE. 0.D0) THEN
            KNSNEW = I
            GO TO 100
           ENDIF
          END DO

C  TO GET HERE MEANS THAT THERE ARE NO DOSE TIMES PAST THE END OF THIS
C  STEADY STATE DOSE SET. IN THIS CASE, SET KNS TO ND+1.


          KNS = ND+1
          GO TO 200

  100     KNS = KNSNEW
  200     CONTINUE


C  SET ISKIPBOL = 1 WHENEVER CONVERGENCE OCCURS IN
C  THE STEADY STATE DOSES SINCE IN THIS CASE, WE DON'T WANT TO
C  REAPPLY THE LAST BOLUS FROM THE STEADY STATE SET BELOW LABEL 83.

          ISKIPBOL = 1


         ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(ICONV .EQ. 1)  CONDITION.

C  IF ICONV = 0, ISTEADY IS STILL = 1, 
C  WHICH MEANS THAT THE ATTEMPT TO PREDICT THE FINAL (STEADY STATE)
C  COMPARTMENT AMOUNTS CONTINUES.
          
        ENDIF
      
C  THE ABOVE ENDIF IS FOR THE  IF(NSET .GE. 5)  CONDITION.

C  SINCE ISAME = 1, THE END OF THE SET NO. NSET HAS OCCURRED -->
C  INCREASE NSET BY 1.

        NSET = NSET + 1

       ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(ISAME .EQ. 1)  CONDITION.

      ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(ISTEADY .EQ. 1)  CONDITION.



31      CONTINUE

C  RECORD OBSERVATION AND SUPPLY NEW DOSE

      IF(ID.EQ.1) GO TO 35
	KNTM1=KNT-1

C  NOTE THAT THE TIME AT WHICH THE OUTPUT IS DESIRED IS TIM(KNTM1); THIS
C  IS CLEAR SINCE THE RETURNING VALUE(S) IN YT ARE PUT INTO ROW NO.
C  KNTM1 OF Y.

        CALL OUTPUT(TIM(KNTM1),YT,X,RPAR,IPAR)

        DO 2010 I=1,NOS
2010    Y(KNTM1,I)=YT(I)

55      IF(ID.EQ.0) GO TO 40

  35    CONTINUE

        IF(NI .EQ. 0) GO TO 83

        DO I=1,NI
         R(I)=RS(KNS-1,I)
        END DO

C  AS OF idm2x12.f: MUST CALL GETFA BEFORE EVERY TIME THAT
C  FA(.) ARE USED IN CASE THE EQUATION(S) FOR THE FA(.) ARE BASED
C  ON THE COVARIATES, WHICH CAN CHANGE DOSE TO DOSE.

c         CALL GETFA(FA,X)
         CALL GETFA(FA,X,ESTML,R,B,INTLIST)


83      IF(NDRUG .EQ. 0 .OR. N .EQ. 0) GO TO 82

C  ADDING N .EQ. 0 TO ABOVE IF STATEMENT SHOWS CLEARLY THAT IF
C  N = 0 (IN WHICH CASE ANALYTIC SOLUTIONS ARE CODED DIRECTLY INTO
C  SUBROUTINE OUTPUT, WHICH MAKES THE COMPARTMENT AMOUNTS IRRELEVANT)
C  SETTING VALUES FOR THE COMPARTMENTS, X, IS UNNECESSARY.


C  IF ISKIPBOL = 1, DO NOT APPLY BOLUSES FROM DOSE KNS-1, SINCE THESE
C  BOLUSES WERE PART OF THE STEADY STATE DOSE SET WHICH ALREADY HAD
C  BOLUSES (EFFECTIVELY) APPLIED ABOVE WHERE "CONVERGENCE" OF THE
C  STEADY STATE DOSE SET WAS OBTAINED.

        IF(ISKIPBOL .EQ. 0) THEN
         DO I=1,NDRUG
C          X(NBCOMP(I))=X(NBCOMP(I))+BS(KNS-1,I)*FA(I)
          X(NBCOMP(I))=X(NBCOMP(I))+BS(KNS-1,I)*FA(I)
         END DO
        ENDIF

C  RESET ISKIPBOL = 0 HERE. IF IT IS NOW = 1, IT MEANS THAT
C  THE ABOVE APPLICATION OF BOLUSES WAS SKIPPED SINCE THERE HAS JUST
C  BEEN A STEADY STATE SET OF DOSES WHICH CONVERGED (AND WE DON'T
C  WANT THE LAST BOLUS DOSE REAPPLIED). BUT, GOING FORWARD, ISKIPBOL
C  SHOULD BE SET AGAIN TO 0 SO THE ABOVE APPLICATION OF BOLUSES WILL
C  OCCUR WHENEVER THERE IS A NEW BOLUS TO BE APPLIED.

      ISKIPBOL = 0


82      CONTINUE

C  CHECK STOPPING TIME.

40      IF(KNT .LE. M) GO TO 45

C*****DETERMINE YPRED(I)*****

C        write (*,*) "Writing YPRED(1:#meas,1:#out_eq)",M,NOS

	DO J=1,NOS
         DO I=1,M
	  YPRED(I,J) = Y(I,J)
	 END DO
	END DO


C  AS OF idm2x9.f, RESTORE THE VALUES FOR ND, SIG, AND RS, IN CASE
C  THIS MODEL HAS TIME LAGS OR STEADY STATE DOSES - TO BE READY FOR THE
C  NEXT CALL TO THIS ROUTINE.


	 ND = NDO
	 DO I=1,ND
	  SIG(I) = SIGO(I)
	  DO J=1,NI
C	   RS(I,J) = RSO(I,J)
	   RS(I,J) = RSO(I,J)
	  END DO
	 END DO

C  ESTABLISH THE VALUES IN EACH DRUG'S PO COLUMN TO THE CORRESPONDING
C  COLUMN IN ARRAY BS.

         DO I=1,ND
          DO J=1,NDRUG
           BS(I,J)=RS(I,2*J)
	  END DO
	 END DO


C        write (*,*) "Returning from FUNC2"

      RETURN
      END
c  idm3x19.f                                               10/9/15

c  idm3x19 has the following change from idm3x18:

c  Comments regarding NPP now indicate that it = NVAR+NOFIX+NRANFIX,
c  rather than just NVAR+NOFIX. There are no functional changes to
c  this new module.

c-----------------------------------------------------------------------

c  idm3x18.f                                               3/11/15

c  idm3x18 has the following change from idm3x17:

c  All numbers written out in F or G format are now tested to see if
c  they are inside [-1.D-99, 1.D-99]. If so, they are changed to be
c  0. The reason is that otherwise they will be printed out without 
c  the accompanying D or E (e.g., as .934-106, rather than .934E-106).

c  Note that this module is linked with npageng30.f initially.

c-----------------------------------------------------------------------

c  idm3x17.f                                               11/21/14

c  idm3x17 has the following changes from idm3x16:

c  It has the Threadprivate statements to make it compatible
c  with the new npageng28.f program. These statements allow the 
c  program to be run in parallel. 

c-----------------------------------------------------------------------

c  idm3x16.f                                               7/21/14

c  idm3x16 has the following change to idm3x15:

c  If the program stops unexpectedly with the writing of format 111
c  in Subroutine FUNC3, this same comment will now be written to
c  the file, ERRFIL, which is passed to FUNC3 in COMMON/ERR.

c-----------------------------------------------------------------------

c  idm3x15.f                                               3/6/14

c  idm3x15 has the following changes from idm3x14:

c  1. In Subroutine FUNC3, the dimensions related to the no. of output
c  equations have been changed from 6 to NUMEQT OR MAXNUMEQ (see 
c  comments in that routine).

c  2. In Subroutine FUNC3, the dimensions of 6 in XSTORE and XPRED have
c  been changed to max_ODE_comps, as they should have been all along (i.e., this
c  represents the maximum no. of compartments allowed).

c. 3. YPRED has been renamed to be YYPRED (to be consistent with 
c  the calling argument in the calling module, npageng25.f). Also,
c  this will avoid confusion with the YPRED used in the module
c  idm2x14.f.

c  4. The argument list to IDCALCYY has the additional argument,
c  NUMEQT, so that YYPRED can now be variably dimensioned. For the
c  same reason, NUMEQT has been added to the argument list of 
c  Subroutines EVAL3 and FUNC3.

c-----------------------------------------------------------------------

c  idm3x14.f                                               10/11/12

c  idm3x14 has one correction from idm3x13:

c  THE R(.) ARE SET = RS(.,.) BEFORE GETIX IS CALLED IN THE TIME RESET
c  SECTION OF SUBROUTINE FUNC3. NOT DOING THIS WOULD MEAN THAT IF THE 
C  INITIAL CONDITIONS FOR THE X(.) ARE FUNCTIONS OF THE COVARIATES
C  (ESTABLISHED IN GETIX FROM THE R(.) VALUES), THEY WOULD BE ASSIGNED
C  VALUES BASED ON COVARIATES FROM A PREVIOUS DOSAGE LINE IN THE
C  PATIENT'S DATA FILE, RATHER THAN THE LINE WHICH IS THE DOSE RESET
C  LINE.

c-----------------------------------------------------------------------

c  idm3x13.f                                               9/27/12

c  idm3x13 has the following bug correction to idm3x12:

C  IN SUBROUTINE FUNC3, BEFORE
C  THE FIRST CALL TO GETFA, THE R(.) ARE SET = RS(.,.) IN CASE ANY OF
C  THE FA(.) ARE FUNCTIONS OF THE COVARIATES WHICH ARE ESTABLISHED FROM
C  THE R(.) VALUES IN  GETFA. IN ADDITION, PRIOR TO THE 2 SECTIONS WHERE
C  THE FA(.) ARE USED, GETFA IS CALLED SO THAT THE FA(.) ARE UPDATED TO
C  CURRENT VALUES, BASED ON THE MOST RECENT COVARIATE VALUES IN THE
C  PATIENT'S DATA FILE. IN PREVIOUS PROGRAMS, IT WAS SIMPLY ASSUMED
C  THAT THE FA(.) WERE FUNCTIONS OF THE PARAMETERS, BUT NOT THE

C  COVARIATES, AND SO THIS WASN'T NECESSARY. BUT THE CODE IN 
C  TSTMULTI.FOR IMPLIES THAT THE FA(.) COULD BE FUNCTIONS OF THE
C  COVARIATES, AND SO THIS CHANGE IS NECESSARY.

C  NOTE THAT SETTING THE R(.) TO RS(.,.) BEFORE THE FIRST CALL TO
C  GETFA ALSO MEANS THE R(.) WILL BE SET BEFORE GETIX AND GETTLAG ARE
C  FIRST CALLED, WHICH AGAIN IS REQUIRED IN CASE THEY ESTABLISH VALUES
C  AS FUNCTIONS OF THE COVARIATES IN THE PATIENT DATA FILE.

c-----------------------------------------------------------------------

c  idm3x12.f                                               7/25/12

c  idm3x12 has the following change to idm1x11:

c  In SUBROUTINE FUNC3, the code to save ND0, SIGO, RSO, is moved to
c  before the IF(N .EQ. 0) GO TO 75  statement. The reason is that 
c  before this  routine returns, ND, SIG, and RS are reset back to these
c  values, even if N = 0, and so they must be established at this time.

c-----------------------------------------------------------------------

c  idm3x11.f                                               4/14/12

c  idm3x11 has the following changes to idm2x10.f:

c  It is to be used with npageng17.f, which allows steady state doses
c  to be boluses as well as IVs. As a result, an additional parameter,
c  ISKIPBOL, is used so, in Subroutine FUNC, when convergence occurs in
c  a steady state dose set, the last bolus from that set will not be
c  reapplied below label 83.

c-----------------------------------------------------------------------

c  idm3x10.f                                               4/10/12

c  idm3x10 has one small 'bug' fix to idm3x9:

c  In Subroutine FUNC3, at label 40, and just below it in the do loop,
c  NUMT+1 is replaced by NUMT. Also, all comment references to NUMT+1 
c  are replaced by NUMT. The reason is that the no. of times at which
c  predicted values are required is NUMT, not NUMT+1. This can, in
c  rare situations, mean that TPRED(NUMT+1) = 0 can cause the program
c  to stop with an error message (see code around format 111). 

c-----------------------------------------------------------------------

c  idm3x9.f                                               3/2/12

c  idm3x9 has the following bug fix to idm3x8.f. In Subroutine FUNC3, 
c  the code to save ND, SIG, and RS before altering them if there are 
c  time lag parameters (in the call to GETTLAG) is now executed whether
c  or not there are time lag parameters. The reason is that, with steady
c  state doses, the first SIG(.) time in a steady state dose set is
c  reset to be 0 after the steady state dose is identified. And this
c  time must be reset back to be its original negative value at the end
c  of the routine so that the next time the routine is called, the 
c  program will again know when a steady state dose is coming. 

c-----------------------------------------------------------------------

c  idm3x8.f                                                1/15/12

c  Corrects bug in Subroutine FUNC3 - now time resets are identified
c  by just the observation time = 0 (i.e., the dose time = 0 is
c  no longer required). This is because it is possible for a dose
c  time (especially if there are timelags) to be after the last
c  observation time in a section of the patient file (before a time
c  reset), and if this happens, the program will not be able to
c  identify the observation time of 0 as a time reset.

c-----------------------------------------------------------------------

c  idm3x7.f                                                11/11/11

c  idm3x7 has the same changes to idm3x6 that idm1x7 has from idm1x6
c  (see all the comments in idm1x7.f for explanations). In particular:

c  1. It can accommodate steady state dose regimens.

c  2. All arrays related to doses (SIG,SIGO,RS,RSO, and BS) in
c  Subroutine FUNC have their 500's changed to 5000's.

c  3. Near the top of Subroutine FUNC, R(1)=0.0D0 is replaced by setting
c  R(2*I-1) = 0.D0, for I = 1,NDRUG. This should have been done when
c  the program became a multi-drug program (see comment in FUNC).

c  4. A time reset no longer requires all initial compartment amounts
c  to be reset to 0. This is because a time reset no longer has to mean
c  an "infinite" amount of time has occurred with no dosing; it can also
c  now mean an "infinite" amount of time has occurred with unknown 
c  dosing. So Subroutine GETIX will be called to establish initial
c  conditions for the new time period (these initial values can of
c  course be 0's as was always assumed in previous programs). This is
c  the situation where a patient, who previously had doses and
c  observations which were recorded while he was in a lab, goes home and
c  gets unknown doses over a long time period, and then returns to the
c  lab to get a new set of doses and observations, starting with 
c  observations which establish his initial conditions for this new
c  time period.

c-----------------------------------------------------------------------

c  idm3x6.f                                                12/20/10

c  idm3x6 has the following change to idm3x5:

c  In Subroutine FUNC3, it has code that calls Subroutine ANAL3, rather
c  than USERANAL if N .EQ. -1. Also, the code to reset X(I),I=1,N to 0
c  where there is a time reset now includes extra code to set 
c  X(I),I=1,3 to 0 if N .EQ. -1.

c  Note that ANAL3, and the routines it calls are from the Little NPAG
c  program module, IDPC9A.FOR. 

c  Note that this module is linked first with bigmlt11.f, and the 
c  template model file is TSTMULTH.FOR (in which in Subroutine SYMBOL,
c  the user is told to code N=-1 if he wants to assume the standard
c  3-compartment linear model with analytic solutions, and in this 
c  case also establish the 5 parameters, {KE,KA,KCP,KPC,V} of this
c  model).

c-----------------------------------------------------------------------

c  idm3x5.f							4/03/10

c  idm3x5 has a bug correction to idm3x4. In Subroutine FUNC3, in the
c  IF(TPRED(KNT) .EQ. 0.D0 .AND. SIG(KNS) .EQ. 0.D0) block, the time,
c  T, is also reset = 0 since the integration will again start from
c  time 0. When this wasn't done (in idm3x4.f), the results were
c  unpredictable (depending on how the DVODE integration routines
c  treated a (T,TOUT) pair which decreased rather than increased.

c-----------------------------------------------------------------------

c  idm3x4.f							11/23/09

c  idm3x4 fixes a bug in the idm3x3 code. Label 75 is moved to in
c  front of the  CALL GETTLAG(TLAG)  statement (see the reason in
c  that part of the code).

c-----------------------------------------------------------------------

c  idm3x3.f							9/18/09

c  idm3x3 has the following changes from idm3x2:

c  1. The TLAG and FA vectors, and the initial values for the X array 
c  will be set by calling new routines (GETTLAG, GETFA, and GETIX, 
c  respectively) that are part of the model file (the new template is 
c  TSTMULT.FOR). This means the user can now code explicit formulas
c  for these values. As a result, all reference to NTLAG, IC, IFA, and
c  IVOL have been removed.

c  2. The shift subroutine will now be from the module, shift5.f, 
c  rather than shift4.f.

c  Note that this module, along with idm1x3.f, id2x3.f, and shift5.f
c  are part of the new "engine", whose main module is bigmlt4.f.

c-----------------------------------------------------------------------

c  idm3x2.f							8/14/09

c  idm3x2 has the following changes from idm3x1:

c  1. The code for setting initial compartment amounts from initial
c  compartment concentrations is changed to reflect the fact that
c  now IC(2) refers to the index of the covariates, not the
c  column no. of RS (see comment in code).

c  2. The code to establish the timelag parameters has changed to
c  reflect that NTLAG(I) can now be negative --> in Subroutine
c  SHIFT, the associated timelag parameter will now be the 
c  exponent of the indicated parameter (rather than the parameter 
c  itself).

c  3. The code to establish the FA parameters has changed to
c  reflect that IFA(I) can now be negative --> the associated FA
c  parameter will now be the exponent of the indicated parameter 
c  (rather than the parameter itself).


c  idm3x2.f (along with other new modules idm1x2.f and idm2x2.f) are
c  still called by bigmlt2.f, but are part of the "engine" for the
c  new NPBIG15B.FOR program.

c-----------------------------------------------------------------------


c  idm3x1.f							5/27/09

c  idm3x1.f has the following changes from idcy_63f.f:

c  1. It allows the extra option of setting initial compartment 
c  amounts from their initial concentrations - see code in Subroutine 
c  FUNC3.

c  2. It is part of the new Big NPAG "engine", bigmlt2.f, which allows 
c  patient data files to have "reset" values of 0 in the dosage and 
c  sampling blocks. Whenever, in Subroutine FUNC2, the program sees a 
c  SIG(.) = 0 and a TIM(.) = 0, it knows that a large enough time has 
c  passed since the last dose that all compartment amounts are to be 
c  reset = 0. Subsequent dose and observed value times are then values 
c  from this point.

c  3. The first argument to Subroutine OUTPUT is changed from 0.0 to 
c  0.D0 in two places.


c  This module, along with idm1x1.f and idm2x1.f are first used in the 
c  bigmlt2.f program.

c-----------------------------------------------------------------------



c  idcy_63g.f							5-28-02

c  idcy_63g has the following changes from idcy_63f:

c  It allows multiple drug inputs (rather than just one drug input).
c  The changes required for this are:

c  1. BS has dimension change from (500,3) to (500,7)
c  2. COMMON/CNST2 is changed to include NDRUG (no. of drugs) and
c     NADD (no. of additional covariates), rather than NBI and NRI.
c  3. NTLAG is now a vector instead of a scalar. In particular, 
C     NTLAG(I) = 0 IF DRUG I'S BOLUS COL. HAS NO TIMELAG PARAMETER;
C                K IF DRUG I'S BOLUS COL. HAS A TIMELAG WHOSE VALUE IS
C		   GIVEN BY PARAMETER NO K.
C  4. IFA, PASSED IN COMMON/FRABS FROM SUBROUTINE SYMBOL IS NOW A VECTOR
C     INSTEAD OF A SCALAR.
C     IFA(I) = 0 IF DRUG I WILL HAVE FA = 1.0.
C              K IF DRUG I WILL HAVE AN FA WHOSE VALUE IS TO BE GIVEN
C                BY PARAMETER K.
C  5. THE BOLUS COMPARTMENT NOS., NBCOMP(I), NOW COME VIA 
C     COMMON/BOLUSCOMP FROM SUBROUTINE SYMBOL, AND THE DIMENSION OF
C     NBCOMP HAS BEEN CHANGED TO 7 (MAXIMUM OF 1 PER DRUG) FROM 20.
C  6. ALL OF THE CODE IN SUBROUTINE FUNC3 RELATED TO NRI AND NBI HAS 
C     BEEN CHANGED TO BE IN TERMS OF NI AND NDRUG. 
C  7. THE CODE RELATED TO CALLING SUBROUTINE SHIFT, INCLUDING THE 
C     CALLING ARGUMENTS, HAS BEEN CHANGED TO REFLECT THE ABOVE CHANGES
C     IN NTLAG (I.E., IT IS NOW A VECTOR RATHER THAN A SCALAR). A NEW
C     MODULE, shift3.f (WHICH REPLACES shift2.f) WILL BE LINKED WITH 
C     THIS MODULE.

C-----------------------------------------------------------------------

c  idcy_63f.f							4-23-02


c  idcy_63f has the following changes to idcy_63e:

c  1. To enable FA to be a parameter value (either fixed or random), 
c  rather than always be hardcoded = 1.0, the following changes are
c  implemented ...

c  The hardcoding of FA = 1.0 and the code for NBCOMP are removed
c  from main. In addition, COMMON/BCOMP is removed from the entire 
c  module. Instead, in SUBROUTINE FUNC3, a new COMMON/FRABS/IFA provides 
c  the value IFA which is the parameter index of the FA value (passed
c  from SUBROUTINE SYMBOL) unless it = 0, in which case FA is
c  set = 1.0. Also the NBCOMP compartment nos. are now set in 
c  SUBROUTINE FUNC3.

c  2. COMMONS /OBSER AND /SUM2 (and the arrays in them) are deleted from 
c  main. They were not needed. Also, COMMON CNST2 is deleted from main
c  since NBI is no longer needed here (since NBCOMP code is removed -
c  see no. 1. above).

c-----------------------------------------------------------------------

c  idcy_63e.f							1-22-00

c  idcy_63e has the following changes to idcy_63d:

c  It allows the initial conditions of the amounts in the compartments
c  to be paramater values, rather than fixed at 0.0. These parameter
c  values may be either fixed or random.

c  To affect this enhancement, the primary change is the code in 
c  subroutine FUNC3 which sets the initial conditions based on the 
c  values in IC which are provided by COMMON/INITCOND from 
c  SUBROUTINE SYMBOL of the Fortran model file.

c  There are many other changes to simply the code (i.e., a lot of
c  code was leftover code which was unused and/or confusing), namely:

c  - Commons ADAPT1, ADAPT2, LPARAM, PRED, TRANS, and PARAM are 
c    deleted. Variables ISW, IP, and C are deleted.
c  - COMMON/PARAMD/P is now in MAIN, FUNC, and JACOB of idfix5e.f; 
c    MAIN and FUNCx of idcy_53e.f and idcy_63e.f; and DIFFEQ and OUTPUT 
c    of the Fortran model file.
c  - P is redimensioned max_ODE_params. It will hold only the parameters of the
c    model (although some of those parameters may be initial conditions)
c    and there are max_pop_rand_varbs allowable random paramaters and max_pop_params allowable
c    fixed paramaters now.
c  - All the code to reverse the paramater order (using PD) and to do
c    and undo square root transformations in MAIN and FUNC3 is removed
c    (it was unneeded, and therefore confusing). In particular, all
c    references to NPT, NUMYES, NUIC, NUP, NPNL, and NBOT are removed.
c  - COMMON ANALYT/IDIFF is removed. IDIFF is unneeded since IDIFF = 0
c    is equivalent to N = 0, and so IDIFF code in FUNC3 is replaced by
c    the equivalent code for N. NEQN is replaced by N.
c  - In SUBROUTINE EVAL3, COMMON/PARAM is removed, along with PP and P.
c    Setting PP(I) = P(I), I=1,NPNL made no sense since PP wasn't used
c    and NPNL was always = 0 anyway.
c  - In FUNC3, the If statment at label 83 is changed to include 
c    N .EQ. 0 since if N = 0, setting compartment values is unnecessary.

c  idcy_63e is part of the big npem program, npbig4.f.

C 5785         CALL IDCALCYY(JSUB,IG,NVAR+NOFIX+NRANFIX,NDIM,PX,TPRED,
C 5786      1      NUMT(JSUB), YYPRED,NUMEQT,
C 5787      2      NOBSER,MF,RTOL,ATOL,RSCOPY,BSCOPY,INTLIST,IPAR,ObsError)

	SUBROUTINE IDCALCYY(JSUB,IG,NPP,NDIM,ESTML,TPRED,NUMT,YYPRED,
     1      NUMEQT,NOBSER,MF,NBCOMP,RTOL,ATOL,
     2      TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     3      INTLIST,IPAR,ObsError,RPAR,ERRFIL)

C  INPUT ARE:

C  NPP = NO. OF PARAMETERS (RANDOM, FIXED, AND RANFIX) IN THE PARAMETER
C        VECTOR, ESTML.
C  NDIM = NO. OF STATES FOR THE O.D.E.
C  ESTML = VECTOR OF PARAMETER ESTIMATES.
C  TPRED = VECTOR OF TIMES AT WHICH PREDICTED CONCENTRATIONS WILL
C	   BE FOUND.
C  NUMT = OF TIMES IN TPRED.


C  INFORMATION FROM A SUBJECT DATA FILE WHOSE INFO IS PASSED TO THE 
C  ROUTINES IN THIS MODULE VIA COMMONS /OBSER/, /CNST/, /CNST2/, AND 
C  /SUM2/.


C  OUTPUT IS:


C  YYPRED(I,J), I=1,NUMT; J=1,NOS = THE PREDICTED VALUE AT TIME 
C  	TPRED(I) OF THE JTH OUTPUT EQUATION, GIVEN THE INPUT VECTOR
C	ESTML. M AND NOS ARE INPUT TO THIS MODULE VIA COMMONS SUM2 AND
C	CNST2, RESPECTIVELY.

c-----------------------------------------------------------------------

c  See other comments at the top of idcy_63d.f code.

C-----------------------------------------------------------------------

         use npag_utils, only : maxnumeq,max_m_per_obs,max_RS_J
     1     ,max_ODE_comps, max_input_dim, max_doses, max_ODE_params

        IMPLICIT REAL*8(A-H,O-Z)
        DIMENSION ESTML(max_ODE_params),YYPRED(71281,NUMEQT),
     1    TPRED(71281)


C        parameter(MAXNUMEQ=7)

C        COMMON/CNST/ N,ND,NI,NUP,NUIC,NP
C        COMMON/PARAMD/ P

! NEW PARALLEL CODE BELOW AS OF npageng28.f.
C !$omp Threadprivate(/PARAMD/,/CNST/)

C wmy2017Sep14 ... temporarily move these here
C  note that NOBSER is replaced by NUMT
        integer NUMT,NDIM,MF
        integer, dimension(max_input_dim) :: NBCOMP
        real*8  RTOL
        real*8, dimension(max_ODE_comps) :: ATOL
        real*8, dimension(max_RS_J) :: RCOPY
        real*8, dimension(max_ODE_comps) :: BCOPY
        real*8, dimension(max_m_per_obs) :: TIMCOPY
        real*8, dimension(max_doses) :: SIGCOPY
        real*8, dimension(max_doses,max_RS_J) :: RSCOPY
        real*8, dimension(max_doses,max_input_dim) :: BSCOPY
        integer, dimension(128) :: INTLIST
        integer, dimension(257) :: IPAR
        double precision, dimension(max_m_per_obs,MAXNUMEQ) :: ObsError
        double precision, dimension(257) :: RPAR
        CHARACTER ERRFIL*20

C  !$omp Threadprivate( IPAR, ObsError )

C*****INITIALIZE PROGRAM*****

C wmy2018.10.16 moved CALL SYMBOL to main
C      CALL SYMBOL(NBCOMP)

C  THE ABOVE CALL OBTAINS INFO FROM COMMONS.

C  NOTE THAT THIS PROGRAM NOW GETS N = NDIM AND NPP = NVAR+NOFIX+NRANFIX
C  AS CALLING ARGUMENTS.

C	N = NDIM
C	NP = NPP

C  CALCULATE THE OUTPUT CONCENTRATION VECTOR, Y, FOR THE PARAMETER
C  VECTOR, ESTML.


C  THIS SUBROUTINE, CALLED BY MAIN, FINDS THE OUTPUT CONC. 
C  ARRAY, YYPRED, EVALUATED AT PARAMETER VALUES IN VECTOR P, PASSED
C  DIRECTLY TO SUBROUTINE FUNC3 VIA COMMON/PARAMD ... AT THE NUMT 
C  TIMES IN TPRED.

        CALL FUNC3(JSUB,IG,NUMT,YYPRED,TPRED,NUMEQT,NBCOMP,
     1      NPP,ESTML,NDIM,MF,RTOL,ATOL,
     2      TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     3      INTLIST,IPAR,ObsError,RPAR,ERRFIL)


        RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE FUNC3(JSUB,IG,NUMT,YYPRED,TPRED,NUMEQT,NBCOMP,
     1      NPP,ESTML,NDIM,MF,RTOL,ATOL,
     2      TIM,SIG,RS,BS,INTLIST,IPAR,ObsError,RPAR,ERRFIL)

C  THIS SUBROUTINE, CALLED BY IDCALCYY, FINDS YYPRED(I) = OUTPUT CONC. AT
C  THE NUMT TIMES IN TPRED, GIVEN PARAMETER VALUES IN ESTML.
C  NOTE THAT YYPRED IS FOUND AT THE NUMT TIMES IN TPRED BELOW.

         use npag_utils, only : shift, thesame, predlast3
     1    ,maxnumeq,max_m_per_obs,max_RS_J
     2    ,verifyval, max_SS_doses
     3    ,max_ODE_params, max_doses, max_ODE_comps, max_input_dim
     4    ,k_dvode_reserved, k_ig, k_jsub, k_p_end, i_jsub, i_ig
     5    ,i_dvode_reserved

C------- DECLARATIONS -------C


C      IMPLICIT REAL*8(A-H,O-Z)
      implicit none

C wmy2019.03.12 SR SHIFT moved into npag_utils.f90
C      include "interface_0SHIFT.txt"

C      integer MAXNUMEQ
C      PARAMETER(MAXNUMEQ=7) 

C Arguments

      integer JSUB, IG, NUMT
      double precision, dimension(71281,NUMEQT) :: YYPRED
      real*8, dimension(71281) :: TPRED
      integer NUMEQT

C      COMMON/BOLUSCOMP/NBCOMP
      integer, dimension(max_input_dim) :: NBCOMP

      integer NPP

C      COMMON/PARAMD/ P   ! Not used -- replaced by ESTML
      real*8, dimension(max_ODE_params) :: ESTML

      integer NDIM,MF
      real*8 RTOL
      real*8, dimension(max_ODE_comps) :: ATOL

C      COMMON/OBSER/ TIM,SIG,RS,YO,BS
      real*8, dimension(max_m_per_obs) :: TIM
      real*8, dimension(max_doses) :: SIG
C      real*8,dimension(max_m_per_obs,MAXNUMEQ) :: YO ! Not used anymore
      real*8, dimension(max_doses,max_RS_J) :: RS
      real*8, dimension(max_doses,max_input_dim) :: BS
      integer, dimension(128) :: INTLIST
      integer, dimension(257) :: IPAR
      double precision, dimension(max_m_per_obs,MAXNUMEQ) :: ObsError
      double precision, dimension(257) :: RPAR


C      COMMON/ERR/ERRFIL
      CHARACTER ERRFIL*20

C Local variables

C      COMMON/CNST/ N,ND,NI,NUP,NUIC,NP
       integer N,ND,NI,NP

C      COMMON/CNST2/ NPL,NOS,NDRUG,NADD
       integer NOS,NDRUG,NADD

C      COMMON/INPUT/ R,B
      double precision, dimension(max_RS_J) :: R
      double precision, dimension(max_ODE_comps) :: B

C      COMMON/STATE/ X
      double precision, dimension(max_ODE_comps) :: X

      integer KNT,KNS,ID,NTL,ISKIPBOL,ISTEADY,IKNS,ICONV
      integer I,J,III,KNSNEW,KNTM1,NDO,ISAME
      double precision T,TOUT,DOSEINT

      double precision, dimension(100) :: XVERIFY

C  NOTE THAT AS OF idm3x15.f, THE DIMENSIONS OF 6 IN XSTORE AND XPRED
C  HAVE BEEN CHANGED TO max_ODE_comps, WHICH IS WHAT THEY SHOULD HAVE BEEN ALL
C  ALONG (I.E., THE SAME AS FOR X).

      integer NN, NSET
      double precision, dimension(max_SS_doses,max_ODE_comps) :: XSTORE
      double precision, dimension(max_ODE_comps) :: XPRED

C  NOTE THAT THE DIMENSIONS RELATED TO THE NO. OF OUTPUT EQS. IN
C  YO, YT AND Y ARE CHANGED TO MAXNUMEQ (FROM 6). NUMEQT COULD NOT
C  BE USED BECAUSE THESE ARRAYS WERE NOT PASSED TO THIS ROUTINE AS
C  DUMMY ARGUMENTS.

      double precision, dimension(MAXNUMEQ) :: YT
      double precision, dimension(71281,MAXNUMEQ) :: Y

C  THE 2ND DIMENSION OF YYPRED IS CHANGED TO NUMEQT, SINCE IT IS PASSED
C  IN THE ARGUMENT LIST, AND CAN THEREFORE BE VARIABLY DIMENSIONED.

C  NOTE THAT "7" IN THE ABOVE ARRAYS INDICATE THE NO. OF DRUGS ALLOWED.

      double precision, dimension(7) :: TLAG,FA

C  INTERMEDIATE and BUFFER variables

      double precision, dimension(max_doses) :: SIGO
      double precision, dimension(max_doses,max_RS_J) :: RSO

      double precision constant001

C------- INITIALIZATIONS -------

C      COMMON/CNST/ N,ND,NI,NUP,NUIC,NP
      N = NDIM
      ND = intlist(8)
      NI = intlist(7)
C       NUP = not used
C       NUIC = not used
      NP = NPP

C      COMMON/CNST2/ NPL,NOS,NDRUG,NADD ! Note NOS=NUMEQT
      NOS = intlist(9)
      NDRUG = intlist(5)
      NADD = intlist(6)

C*****ODE CONSTANTS AND (more) INITIALIZATION*****

        KNS=1
        KNT=1

C  NOTE THAT KNT IS THE RUNNING INDEX OF THE NEXT OBSERVATION TIME,
C  AND       KNS IS THE RUNNING INDEX OF THE NEXT DOSAGE TIME.

        T=0.0D0

C  INITIALIZE ISKIPBOL = 0. SEE CODE BELOW. IT IS ONLY NEEDED FOR A
C  STEADY STATE DOSE SET WHICH HAS BOLUS DOSES.

      ISKIPBOL = 0


      DO I = 1,NDRUG
       R(2*I-1) = 0.D0
      END DO

c  AS OF idm3x7.f, instead of R(1) = 0, the code has been changed to 
c  set R(2*I-1) = 0, for I = 1,NDRUG. I.E., All IV rates for all NDRUG
c  drugs are initialized to be 0 ... in case the 1st obs. time is 0,
c  which means that OUTPUT is called before the R(I) are set below.

C  CALL SUBROUTINE GETFA IN npemdriv.f (THE FIRST TEMPLATE FILE TO 
C  INCLUDE GETFA IS TSTMULTG.FOR) TO OBTAIN THE VALUE OF FA FOR EACH
C  OF THE NDRUG DRUGS.

C  AS OF idm3x13.f, BEFORE CALLING GETFA, MUST SET
C  THE R(.) IN CASE ANY OF THE FA(.) ARE FUNCTIONS OF THE 
C  COVARIATES WHICH ARE ESTABLISHED FROM THE R(.) VALUES IN
C  GETFA.
 
      DO I=1,NI
       R(I)=RS(KNS,I)
      END DO


c         CALL GETFA(FA,X)
         CALL GETFA(FA,X,ESTML,R,B,INTLIST)


C  NOTE THAT NBCOMP(I),I=1,NDRUG WAS SET IN SUBROUTINE SYMBOL AND
C  PASSED TO THIS ROUTINE VIA COMMON/BOLUSCOMP.


C  As of idm3x12.f, the code to save ND0, SIGO, RSO, is moved to before
c  the IF(N .EQ. 0) GO TO 75  statement. The reason is that before this
c  routine returns, ND, SIG, and RS are reset back to these values,
c  even if N = 0, and so they must be established at this time.

C  AS OF idm3x9.f, SAVE ND, SIG, AND RS WHETHER OR NOT NTL = 1, SINCE
C  IF THERE ARE STEADY STATE DOSE SETS, THE FIRST SIG(.) VALUE IN EACH
C  SET WILL BE CHANGED TO BE 0 BELOW.

	 NDO = ND
	 DO I=1,ND
	  SIGO(I) = SIG(I)
	  DO J=1,NI
	   RSO(I,J) = RS(I,J)
	  END DO
	 END DO



C  IF N = 0, THE OUTPUT EQUATION(S) FOR Y ARE CODED EXPLICITLY INTO
C  SUBROUTINE OUTPUT, AND NO D.E. SOLUTIONS (VIA USERANAL/DIFFEQ) ARE
C  TO BE USED. IN THIS CASE, SKIP THE CODE REGARDING INITIAL CONDITIONS
C  OF THE COMPARTMENTS, SINCE THEY ARE IRRELEVANT (I.E., THE COMPARTMENT
C  AMOUNTS DON'T NEED TO BE INITIALIZED SINCE THEY WON'T BE UPDATED BY
C  INTEGRATING D.E.'S). IN FACT, COULD PROBABLY SKIP TIMELAGS TOO, 
C  SINCE THEY CHANGE THE TIME THAT BOLUS DOSES ARE GIVEN, AND THIS
C  THEORETICALLY ONLY AFFECTS COMPARTMENT AMOUNTS (WHICH ARE NOT USED
C  IF N = 0), BUT JUST SKIP INITIAL CONDITIONS FOR NOW.

        IF(N .EQ. 0) GO TO 75


C  CALL SUBROUTINE GETIX IN npemdriv.f (THE FIRST TEMPLATE FILE TO 
C  INCLUDE GETIX IS TSTMULTG.FOR) TO OBTAIN THE VALUE OF X (THE INITIAL
C  COMPARTMENT AMOUNT) FOR EACH OF THE N COMPARTMENTS.

	 CALL GETIX(N,X,ESTML,R,B,INTLIST)



C  CALL SUBROUTINE GETTLAG IN npemdriv.f (THE FIRST TEMPLATE FILE TO 
C  INCLUDE GETTLAG IS TSTMULTG.FOR) TO OBTAIN THE VALUE OF THE TIMELAG
C  FOR EACH OF THE NDRUG DRUGS.

C   75	 CALL GETTLAG(TLAG,X)
   75	 CALL GETTLAG(TLAG,X,ESTML,R,B,INTLIST)

C  IF ANY TLAG(.) VALUES RETURN AS .NE. 0, THEN, CALL SUBROUTINE SHIFT
C  TO ADJUST THE DOSAGE REGIMEN APPROPRIATELY.

      NTL = 0
	DO ID = 1,NDRUG
	 IF(TLAG(ID) .NE. 0) NTL = 1
	END DO

	IF(NTL .EQ. 1) THEN

C  STORE INCOMING VALUES IN ND, SIG, AND RS (WHICH CONTAINS BS VALUES)
C  SINCE THEY WILL BE CHANGED IN THE CALL TO SUBROUTINE SHIFT, WHICH 
C  "SHIFTS" THE DOSAGE REGIMEN MATRIX TO ACCOUNT FOR THE TIMELAG 
C  PARAMETER(S), TLAG(I). AT THE END OF THIS ROUTINE, THE VALUES IN ND, 
C  SIG, AND RS WILL BE RESET TO THEIR INCOMING VALUES - TO BE READY FOR 
C  THE NEXT CALL TO THIS ROUTINE WITH POSSIBLY DIFFERENT VALUES FOR 
C  TLAG(I).

	 CALL SHIFT(TLAG,ND,SIG,NDRUG,NADD,RS,INTLIST)


C  ESTABLISH THE VALUES IN EACH DRUG'S PO COLUMN TO THE CORRESPONDING
C  COLUMN IN ARRAY BS.

         DO I=1,ND
          DO J=1,NDRUG
           BS(I,J)=RS(I,2*J)
	  END DO
	 END DO


	ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(NTL .EQ. 1)  CONDITION.


      IF(TPRED(KNT).GE.SIG(KNS)) GO TO 12
      IF(TPRED(KNT).NE.0.0D0) GO TO 45

C  THE ONLY WAY THE FOLLOWING CALL TO OUTPUT CAN OCCUR IS IF TPRED(KNT)
C  = 0 --> OBTAIN YT = OUTPUT VALUE(S) AT TIME 0.0.

        CALL OUTPUT(0.D0,YT,X,RPAR,IPAR)
	DO 2000 I=1,NOS
2000    Y(KNT,I)=YT(I)
        KNT=KNT+1
        GO TO 45

12    IF(TPRED(KNT) .GT. SIG(KNS)) GO TO 13
      IF(TPRED(KNT) .NE. 0.0D0) GO TO 45

C  THE ONLY WAY THE FOLLOWING CALL TO OUTPUT CAN OCCUR IS IF TPRED(KNT)
C  = 0 --> OBTAIN YT = OUTPUT VALUE(S) AT TIME 0.0.

      CALL OUTPUT(0.D0,YT,X,RPAR,IPAR)
	DO 2005 I=1,NOS
2005  Y(KNT,I)=YT(I)
	KNT=KNT+1

13    IF(SIG(KNS) .GT. 0.0D0) GO TO 45

C  CHECK TO SEE IF SIG(KNS) < 0. IF SO, IT MEANS THAT 100 STEADY STATE
C  DOSES SHOULD NOW BE APPLIED WITH AN INTERDOSE INTERVAL EQUAL TO
C  -SIG(KNS).

      ISTEADY = 0

      IF(SIG(KNS) .LT. 0.D0) THEN

       ISTEADY = 1
       NSET = 1

C  NOTE THAT ISTEADY = 1 TELLS THE PROGRAM BELOW TO PROCEED AS IF THE
C  DOSE TIME IS 0, AND START INTEGRATING THROUGH THE SET OF 100 
C  DOSE SETS, ALL OF WHICH OCCUR BEFORE THE NEXT OBSERVATION TIME ...
C  BUT PAUSE AFTER THE END OF THE 5TH DOSE SET (NSET IS THE RUNNING NO.
C  OF THE CURRENT DOSE SETS THAT HAVE BEEN RUN) AND CALL SUBROUTINE
C  PREDLAST3 TO PREDICT THE STEADY STATE COMPARTMENT AMOUNTS AFTER THE
C  100 DOSE SETS (NOTE THAT THE COMPARTMENT AMOUNTS WILL HAVE TO BE
C  STORED AT THE END OF EACH OF THE STEADY STATE DOSE SETS AS THE LOGIC
C  OF PREDLAST3 REQUIRES). 

C  IF "CONVERGENCE" IS ACHIEVED AT THAT POINT, ASSIGN THE COMPARTMENT 
C  AMOUNTS TO BE THE PREDICTED AMOUNTS, AND ASSIGN KNS TO BE WHAT IT IS
C  WHEN THESE STEADY STATE DOSE SETS HAVE FINISHED. NOTE THAT THE END OF
C  THE 100TH DOSE SET WILL BE AT TIME 100*(-SIG(KNS)), SO KNS WILL BE 
C  THE INDEX OF THE FIRST DOSE EVENT WHICH OCCURS AFTER THIS TIME.

C  IF "CONVERGENCE" IS NOT ACHIEVED, CONTINUE APPLYING THE LOGIC OF
C  PREDLAST3 UNTIL IT IS ACHIEVED, OR UNTIL THE 100 DOSE SETS ARE ALL
C  INTEGRATED THROUGH, WHICHEVER COMES FIRST.

       DOSEINT = -SIG(KNS)
	


C  RESET SIG(KNS) TO BE 0 SINCE THIS DOSE EVENT REPRESENTS THE START
C  OF 100 DOSE SETS THAT BEGIN AT TIME 0.


       SIG(KNS) = 0



      ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(SIG(KNS) .LT. 0.D0)  CONDITION.


      DO I=1,NI
       R(I)=RS(KNS,I)
      END DO

	IF(NDRUG .EQ. 0) GO TO 81

C  AS OF idm3x13.f: MUST CALL GETFA BEFORE EVERY TIME THAT
C  FA(.) ARE USED IN CASE THE EQUATION(S) FOR THE FA(.) ARE BASED
C  ON THE COVARIATES, WHICH CAN CHANGE DOSE TO DOSE.

c         CALL GETFA(FA,X)
         CALL GETFA(FA,X,ESTML,R,B,INTLIST)


        IF(N .EQ. 0) GO TO 120
        DO I=1,NDRUG
	 X(NBCOMP(I))=X(NBCOMP(I))+BS(KNS,I)*FA(I)
	END DO

C  NOTE THAT FA(I) IS THE FRACTION OF DRUG AVAILABLE FROM A BOLUS INPUT
C  FOR DRUG I INTO ITS ABSORPTIVE COMPARTMENT.

        GO TO 81

120   DO I=1,NDRUG
       B(I)=BS(KNS,I)*FA(I)
      END DO

81      KNS=KNS+1

C*****INTEGRATION OF EQUATIONS*****


C  DETERMINE IF, OBSER(ID=0), OR DOSE(ID=1), OR BOTH(ID=2).

45    IF(KNS.GT.ND) GO TO 15


C CODE CHANGE BELOW FOR idm3x8.f.

	IF(TPRED(KNT) .EQ. 0.D0 .AND. KNT .GT. 1) THEN


C  AS OF idm3x7.f, A TIME RESET NO LONGER REQUIRES ALL INITIAL
C  COMPARTMENT AMOUNTS TO BE RESET TO 0. THIS IS BECAUSE A TIME RESET
C  NO LONGER HAS TO MEAN THAT AN "INFINITE" AMOUNT OF TIME HAS OCCURRED
C  WITH NO DOSING; IT CAN ALSO NOW MEAN THAT AN "INFINITE" AMOUNT OF 
C  TIME HAS OCCURRED WITH UNKNOWN DOSING (IN THIS CASE, SUBROUTINE
C  GETIX WILL BE CALLED BELOW TO ESTABLISH INITIAL CONDITIONS FOR THIS
C  TIME PERIOD).

C  ADVANCE KNS TO THE NEXT VALUE THAT HAS SIG(KNS) .LE. 0. I.E., ONCE
C  TIMN(KNT) = 0, IT MEANS THAT WE ARE DONE WITH THE OUTPUT OBS.
C  TIMES IN THE PREVIOUS SECTION --> THERE IS NO POINT IN CONTINUING
C  TO INTEGRATE TILL THE END OF THE DOSES IN THE PREVIOUS SECTION
C  (IF THERE ARE ANY).

      DO IKNS = KNS,ND
       IF(SIG(IKNS) .LE. 0.D0) GO TO 110
      END DO

C  TO GET HERE MEANS THAT NO VALUE IN SIG(.) FROM KNS TO ND HAS A 
C  VALUE .LE. 0, AND THIS IS AN ERROR. IT MEANS THAT THE PATIENT DATA
C  FILE HAS AN OBSERVATION TIME RESET ROW WITHOUT AN ACCOMPANYING
C  DOSE RESET ROW. TELL THE USER AND STOP.

C  REPLACE WRITING OF SIG() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL.

       XVERIFY(1) = SIG(KNS)
       CALL VERIFYVAL(1,XVERIFY)

C     WRITE(*,111) ND,KNS,SIG(KNS)
      WRITE(*,111) ND,KNS,XVERIFY(1)

C     WRITE(25,111) ND,KNS,SIG(KNS)
      WRITE(25,111) ND,KNS,XVERIFY(1)


 111  FORMAT(//' THE CURRENT SUBJECT HAS AN OBSERVATION TIME RESET'/
     1' ROW WITHOUT AN ACCOMPANYING DOSE RESET ROW. THE PROGRAM NOW'/
     2' STOPS. '//
     3' REVIEW YOUR PATIENT FILES AND CORRECT THE ERROR.'//
     4' NOTE THAT THE ',I4,' DOSE TIMES (POSSIBLY ALTERED BY TIMELAGS'/
     5' ARE THE FOLLOWING (AND THERE IS NO TIME .LE. 0 AFTER TIME'/
     6' NO. ',I4,' WHICH HAS CORRESPONDING TIME ',F15.4,'):')

       OPEN(42,FILE=ERRFIL)
C       WRITE(42,111) ND,KNS,SIG(KNS)
        WRITE(42,111) ND,KNS,XVERIFY(1) 


      DO I = 1,ND
       WRITE(*,*) SIG(I)
       WRITE(25,*) SIG(I)
       WRITE(42,*) SIG(I)
      END DO

        CLOSE(42)

      CALL PAUSE
      STOP


  110 KNS = IKNS

 

C  THERE ARE TWO POSSIBILITES AT THIS POINT, EITHER SIG(KNS) = 0
C  OR SIG(KNS) < 0. 

C  IF SIG(KNS) = 0, THIS REPRESENTS A TIME RESET (T WILL BE SET = 0
C  BELOW) WITH A SINGLE DOSE LINE TO START. IN THIS CASE, CALL GETIX
C  AGAIN (JUST AS WAS DONE NEAR THE TOP OF THIS ROUTINE) TO OBTAIN
C  INITIAL COMPARTMENT AMOUNTS. NOTE THAT BY DEFAULT, IN GETIX, ALL
C  COMPARTMENT AMOUNTS ARE SET = 0 (WHICH WOULD BE THE CASE IF IN THE 
C  LONG TIME PERIOD BETWEEN THE LAST SET OF DOSES AND THIS NEW
C  BEGINNING, NO DOSES HAVE BEEN GIVEN). BUT THE USER MAY ALSO HAVE
C  CODED INTO GETIX EQUATIONS THAT SET ONE OR MORE OF THE X(I) TO
C  FUNCTIONS OF COVARIATE AND PARAMETER VALUES (WHICH WOULD BE THE
C  SITUATION IF AN UNKNOWN DOSING REGIMEN HAS TAKEN PLACE BUT IT
C  DOESN'T MATTER WHAT IT WAS BECAUSE THE PATIENT COMES TO A LAB AND
C  SIMPLY HAS HIS COMPARTMENT VALUES ESTABLISHED BEFORE CONTINUING 
C  WITH THE OTHER VALUES IN HIS PATIENT FILE). 

C  IF SIG(KNS) < 0, THIS REPRESENTS A TIME RESET WITH A STEADY STATE
C  SET OF 100 DOSES ABOUT TO BEGIN. IN THIS CASE, WE ASSUME THAT THE
C  PATIENT IS ABOUT TO GET 100 SETS OF DOSES SO THAT HIS COMPARTMENT
C  AMOUNTS WILL ACHIEVE STEADY STATE VALUES. THESE STEADY STATE VALUES
C  WILL BE ESTIMATED IN THE BLOCK OF CODE BELOW THAT STARTS WITH 
C  IF(ISTEADY .EQ. 1). IN THIS CASE, WE WILL STILL CALL GETIX TO 
C  MAKE SURE THAT ANY RESIDUAL COMPARTMENT AMOUNTS FROM A PREVIOUS
C  SET OF DOSES IS ZEROED OUT (OR SET = VALUES AS DETERMINED BY
C  SUBROUTINE GETIX).

C  AS OF idm3x14.f, BEFORE CALLING GETIX, MUST SET
C  THE R(.) IN CASE ANY OF THE INITIAL CONDITIONS FOR THE X(.)
C  ARE FUNCTIONS OF THE COVARIATES WHICH ARE ESTABLISHED FROM THE 
C  R(.) VALUES IN GETFA.
 
      DO I=1,NI
       R(I)=RS(KNS,I)
      END DO


       CALL GETIX(N,X,ESTML,R,B,INTLIST)
		
C  MUST ALSO RESET T = 0 SINCE THE INTEGRATION WILL AGAIN START FROM 
C  TIME 0.

       T = 0.D0


C  IF SIG(KNS) .LT. 0, THIS IS NOT ONLY A TIME RESET, IT IS THE
C  BEGINNING OF A STEADY STATE DOSE SET. IN THIS CASE, APPLY 100 
C  STEADY STATE DOSES WITH AN INTERDOSE INTERVAL EQUAL TO -SIG(KNS).

      ISTEADY = 0

      IF(SIG(KNS) .LT. 0.D0) THEN

       ISTEADY = 1
       NSET = 1

C  NOTE THAT ISTEADY = 1 TELLS THE PROGRAM BELOW TO PROCEED AS IF THE
C  DOSE TIME IS 0, AND START INTEGRATING THROUGH THE SET OF 100 
C  DOSE SETS, ALL OF WHICH OCCUR BEFORE THE NEXT OBSERVATION TIME ...
C  BUT PAUSE AFTER THE END OF THE 5TH DOSE SET (NSET IS THE RUNNING NO.
C  OF THE CURRENT DOSE SETS THAT HAVE BEEN RUN) AND CALL SUBROUTINE
C  PREDLAST3 TO PREDICT THE STEADY STATE COMPARTMENT AMOUNTS AFTER THE
C  100 DOSE SETS (NOTE THAT THE COMPARTMENT AMOUNTS WILL HAVE TO BE
C  STORED AT THE END OF EACH OF THE STEADY STATE DOSE SETS AS THE LOGIC
C  OF PREDLAST3 REQUIRES). 

C  IF "CONVERGENCE" IS ACHIEVED AT THAT POINT, ASSIGN THE COMPARTMENT 
C  AMOUNTS TO BE THE PREDICTED AMOUNTS, AND ASSIGN KNS TO BE WHAT IT IS
C  WHEN THESE STEADY STATE DOSE SETS HAVE FINISHED. NOTE THAT THE END OF
C  THE 100TH DOSE SET WILL BE AT TIME 100*(-SIG(KNS)), SO KNS WILL BE 
C  THE INDEX OF THE FIRST DOSE EVENT WHICH OCCURS AFTER THIS TIME.

C  IF "CONVERGENCE" IS NOT ACHIEVED, CONTINUE APPLYING THE LOGIC OF
C  PREDLAST3 UNTIL IT IS ACHIEVED, OR UNTIL THE 100 DOSE SETS ARE ALL
C  INTEGRATED THROUGH, WHICHEVER COMES FIRST.

       DOSEINT = -SIG(KNS)

C  RESET SIG(KNS) TO BE 0 SINCE THIS DOSE EVENT REPRESENTS THE START
C  OF 100 DOSE SETS THAT BEGIN AT TIME 0.



       SIG(KNS) = 0

      ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(SIG(KNS) .LT. 0.D0)  CONDITION.



	ENDIF

C  THE ABOVE ENDIF IS FOR THE
C   IF(TPRED(KNT) .EQ. 0.D0 .AND. KNT .GT. 1)  CONDITION.


      IF(TPRED(KNT) .NE. SIG(KNS)) GO TO 20
      ID=2
      TOUT=TPRED(KNT)

      KNT=KNT+1
      KNS=KNS+1

      IF(N .EQ. 0) GO TO 31
      GO TO 30

20    IF(TPRED(KNT) .GT. SIG(KNS) .AND. SIG(KNS) .GT. 0) GO TO 25

15    ID=0
      TOUT=TPRED(KNT)

      KNT=KNT+1

      IF(N .EQ. 0) GO TO 31
      GO TO 30

25    ID=1
      TOUT=SIG(KNS)

      KNS=KNS+1
      IF(N .EQ. 0) GO TO 31

30      CONTINUE
C--------------------------------------------------------------- 30 / 32

C --- wmy2018.06.15 These lines are lifted from USERANAL; they have to
C --- be here to make ANAL3() work.
C --- When you get a chance, go back to useranal and erase these lines
C --- there as those lines are now redundant.  Also, remove INTLIST
C --- from the USERANAL() arglist
c        constant001 = 0.d0
        do III=1,max_ODE_params
          RPAR(k_dvode_reserved + III) = ESTML(III)
c          RPAR(23 + III) = P(III)
c          constant001 = constant001 + P(III) - ESTML(III) + 1.d0
        end do
c        write (*,*) "Calling PK update routine w/",constant001
        do III = 1,max_RS_J
          RPAR(k_p_end + III) = R(III)
C          RPAR(55 + III) = R(III)
        end do
        RPAR(k_jsub) = dble(JSUB)
        RPAR(k_ig) = dble(IG)
        do III = 1,10
          IPAR(i_dvode_reserved + III) = INTLIST(III)
        end do
C        IPAR(23 + 11) = JSUB
        IPAR(i_jsub) = JSUB
C        IPAR(23 + 12) = IG
        IPAR(i_ig) = IG
C        write (*,*) "DEBUG 2018.06.15: RPAR",RPAR(24),RPAR(25),RPAR(26)
C     1     ,RPAR(27),RPAR(28)
C---------------------------------------------------------------
32      IF(N .NE. -1) CALL USERANAL(JSUB,IG,X,T,TOUT,
     1      NDIM,MF,RTOL,ATOL,ESTML,R,INTLIST,IPAR,RPAR)
        IF(N .EQ. -1) CALL ANAL3(X,T,TOUT,RPAR,IPAR)

C  IF ISTEADY = 1, THIS IS INSIDE A STEADY STATE DOSE SET. CHECK TO SEE
C  IF TOUT IS A MULTIPLE OF DOSEINT. IF SO, RECORD THE COMPARTMENT
C  AMOUNTS. THEN, AFTER COMPARTMENT AMOUNTS HAVE BEEN STORED FOR AT 
C  LEAST THE 1ST 5 MULTIPLES OF DOSEINT, STOP AND CALL SUBROUTINE
C  PREDLAST3 WHICH PREDICTS THE FINAL (STEADY STATE) COMPARTMENT AMOUNTS
C  AFTER THE LAST (100TH) DOSE SET. 

C  IF PREDLAST3 HAS PREDICTED VALUES WHICH "CONVERGE", ASSIGN THE
C  PREDICTED VALUES TO X, INCREASE KNS TO BE THE INDEX OF THE FIRST DOSE
C  EVENT WHICH OCCURS AFTER THE STEADY STATE DOSE SET ENDS AND CONTINUE.

C  IF PREDLAST3 VALUES DON'T CONVERGE, CONTINUE THE PROCESS WITH 
C  COMPARTMENT AMOUNTS FOR MULTIPLES 2 - 6 OF DOSEINT, TEST FOR
C  "CONVERGENCE", ETC. THIS PROCESS CONTINUES UNTIL "CONVERGENCE" IS
C  ACHIEVED FOR A SET OF 5 COMPARTMENT AMOUNTS (OR SETS OF AMOUNTS IF
C  NDRUG IS > 1), OR UNTIL ALL 100 DOSE SETS IN THE STEADY STATE REGIMEN
C  HAVE FINISHED. 

      IF(ISTEADY .EQ. 1) THEN


C  THE NEXT DOSE SET END TIME IS DOSEINT*NSET. IF TOUT = DOSEINT*NSET,
C  STORE THE COMPARTMENT AMOUNTS. IF NSET .GE. 5, CALL PREDLAST3 AND
C  PROCEED AS INDICATED ABOVE.

       CALL THESAME(TOUT,DOSEINT*NSET,ISAME)

       IF(ISAME .EQ. 1) THEN

        NN = N
        IF(N .EQ. -1) NN = 3

        DO J = 1,NN
         XSTORE(NSET,J) = X(J)
        END DO

        IF(NSET .GE. 5) THEN

         CALL PREDLAST3(NN,NSET,XSTORE,XPRED,ICONV)

  
         IF(ICONV .EQ. 1) THEN

C  SINCE THE PREDICTED VALUES ARE CONSIDERED ACCURATE (I.E., 
C  "CONVERGENCE WAS ACHIEVED IN PREDLAST), RESET ISTEADY TO 0,
C  WHICH MEANS THAT THE STEADY STATE DOSES ARE FINISHED; ASSIGN THE
C  COMPARTMENT AMOUNTS TO BE THE PREDICTED VALUES; AND SET KNS TO THE
C  FIRST DOSE EVENT AFTER THE END OF THE STEADY STATE DOSE SET. ALSO,
C  SET T = THE ENDING TIME OF THE STEADY STATE DOSE SET = 100*DOSEINT,
C  SINCE THAT IS WHAT IT WOULD HAVE BEEN HAD ALL 100 DOSE SETS BEEN
C  RUN.

          ISTEADY = 0

          DO J = 1,NN
           X(J) = XPRED(J)
          END DO

          T = 100.D0*DOSEINT

C  ADVANCE KNS TO BE THE FIRST DOSE PAST THE 100 DOSE SETS IN THIS
C  STEADY STATE SET. NOTE THAT THIS SET ENDS BEFORE 100*DOSEINT, SO
C  FIND THE FIRST SIG(.) THAT IS .GE. 100*DOSEINT, OR THAT IS = 0
C  (WHICH SIGNIFIES A TIME RESET) OR THAT IS < 0 (WHICH SIGNIFIES 
C  ANOTHER STEADY STATE SET).

          DO I = KNS,ND
           IF(SIG(I) .GE. 100.D0*DOSEINT .OR. SIG(I) .LE. 0.D0) THEN
            KNSNEW = I
            GO TO 100
           ENDIF
          END DO

C  TO GET HERE MEANS THAT THERE ARE NO DOSE TIMES PAST THE END OF THIS
C  STEADY STATE DOSE SET. IN THIS CASE, SET KNS TO ND+1

          KNS = ND+1
          GO TO 200

  100     KNS = KNSNEW
  200     CONTINUE

C  SET ISKIPBOL = 1 WHENEVER CONVERGENCE OCCURS IN
C  THE STEADY STATE DOSES SINCE IN THIS CASE, WE DON'T WANT TO
C  REAPPLY THE LAST BOLUS FROM THE STEADY STATE SET BELOW LABEL 83.

          ISKIPBOL = 1

         ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(ICONV .EQ. 1)  CONDITION.

C  IF ICONV = 0, ISTEADY IS STILL = 1, 
C  WHICH MEANS THAT THE ATTEMPT TO PREDICT THE FINAL (STEADY STATE)
C  COMPARTMENT AMOUNTS CONTINUES.
          
        ENDIF
      
C  THE ABOVE ENDIF IS FOR THE  IF(NSET .GE. 5)  CONDITION.

C  SINCE ISAME = 1, THE END OF THE SET NO. NSET HAS OCCURRED -->
C  INCREASE NSET BY 1.

        NSET = NSET + 1

       ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(ISAME .EQ. 1)  CONDITION.

      ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(ISTEADY .EQ. 1)  CONDITION.



31      CONTINUE


C  RECORD OBSERVATION AND SUPPLY NEW DOSE

      IF(ID.EQ.1) GO TO 35
	KNTM1=KNT-1

C  NOTE THAT THE TIME AT WHICH THE OUTPUT IS DESIRED IS TPRED(KNTM1); 
C  THIS IS CLEAR SINCE THE RETURNING VALUE(S) IN YT ARE PUT INTO ROW NO.
C  KNTM1 OF Y.

        CALL OUTPUT(TPRED(KNTM1),YT,X,RPAR,IPAR)

        DO 2010 I=1,NOS
2010    Y(KNTM1,I)=YT(I)

55      IF(ID.EQ.0) GO TO 40

  35    CONTINUE

        IF(NI .EQ. 0) GO TO 83

        DO I=1,NI
         R(I)=RS(KNS-1,I)
        END DO

C  AS OF idm3x13.f: MUST CALL GETFA BEFORE EVERY TIME THAT
C  FA(.) ARE USED IN CASE THE EQUATION(S) FOR THE FA(.) ARE BASED
C  ON THE COVARIATES, WHICH CAN CHANGE DOSE TO DOSE.

c         CALL GETFA(FA,X)
         CALL GETFA(FA,X,ESTML,R,B,INTLIST)


83      IF(NDRUG .EQ. 0 .OR. N .EQ. 0) GO TO 82

C  ADDING N .EQ. 0 TO ABOVE IF STATEMENT SHOWS CLEARLY THAT IF
C  N = 0 (IN WHICH CASE ANALYTIC SOLUTIONS ARE CODED DIRECTLY INTO
C  SUBROUTINE OUTPUT, WHICH MAKES THE COMPARTMENT AMOUNTS IRRELEVANT)
C  SETTING VALUES FOR THE COMPARTMENTS, X, IS UNNECESSARY.


C  IF ISKIPBOL = 1, DO NOT APPLY BOLUSES FROM DOSE KNS-1, SINCE THESE
C  BOLUSES WERE PART OF THE STEADY STATE DOSE SET WHICH ALREADY HAD
C  BOLUSES (EFFECTIVELY) APPLIED ABOVE WHERE "CONVERGENCE" OF THE
C  STEADY STATE DOSE SET WAS OBTAINED.

        IF(ISKIPBOL .EQ. 0) THEN
         DO I=1,NDRUG
          X(NBCOMP(I))=X(NBCOMP(I))+BS(KNS-1,I)*FA(I)
         END DO
        ENDIF

C  RESET ISKIPBOL = 0 HERE. IF IT IS NOW = 1, IT MEANS THAT
C  THE ABOVE APPLICATION OF BOLUSES WAS SKIPPED SINCE THERE HAS JUST
C  BEEN A STEADY STATE SET OF DOSES WHICH CONVERGED (AND WE DON'T
C  WANT THE LAST BOLUS DOSE REAPPLIED). BUT, GOING FORWARD, ISKIPBOL
C  SHOULD BE SET AGAIN TO 0 SO THE ABOVE APPLICATION OF BOLUSES WILL
C  OCCUR WHENEVER THERE IS A NEW BOLUS TO BE APPLIED.

      ISKIPBOL = 0


82      CONTINUE

C  CHECK STOPPING TIME.

40    IF(KNT  .LE. NUMT) GO TO 45

C*****DETERMINE YYPRED(I)*****

	DO J=1,NOS
         DO I=1,NUMT
	  YYPRED(I,J)=Y(I,J)
	 END DO
	END DO


C  AS OF idm3x9.f, RESTORE THE VALUES FOR ND, SIG, AND RS, IN CASE
C  THIS MODEL HAS TIME LAGS OR STEADY STATE DOSES - TO BE READY FOR THE
C  NEXT CALL TO THIS ROUTINE.


	 ND = NDO
	 DO I=1,ND
	  SIG(I) = SIGO(I)
	  DO J=1,NI
	   RS(I,J) = RSO(I,J)
	  END DO
	 END DO

C  ESTABLISH THE VALUES IN EACH DRUG'S PO COLUMN TO THE CORRESPONDING
C  COLUMN IN ARRAY BS.

         DO I=1,ND
          DO J=1,NDRUG
           BS(I,J)=RS(I,2*J)
	  END DO
	 END DO


      RETURN
      END
c  npagranfix6.f                                           3/9/16

c  npagranfix6 has the following changes from npagranfix5:

c  1. In Subroutine SUBRES, after loop 800, and the call from NOTINT, 
c  PYJ is tested to verify it is not 0. This test has always been in 
c  main at the same point, but not in SUBRES since it didn't seem to be
c  needed there. But it is possible, if the user is doing a MAXCYC = 0
c  run, that the apriori density may not include any points which give
c  predicted values close enough to those of the new subject being 
c  analyzed (especially if this subject's assay coefficients are 
c  relatively small). So the message to the user in this case will
c  be to try the run again after increasing the size of the assay 
c  coefficients.

c  2. Formats 1657 and 7124 are changed to show that the output file
c  is made by npagranfix6 rather than npagranfix5.

c-----------------------------------------------------------------------

c  npagranfix5.f                                           10/24/15

c  npagranfix5 has the following changes from npagranfix4:

c  1. RANFIXEST and NRANFIX are added to a !$omp statement just above 
c  the DO 800 loop in main. This will now enable this program to be
c  run in parallel (npagranfix4.f could only be run in serial mode).

c  2. Formats 1657 and 7124 are changed to show that the output file
c  is made by npagranfix5 rather than npagranfix4.

c-----------------------------------------------------------------------

c  npagranfix4.f                                           10/11/15

c  npagranfix4 has the following changes to npagranfix3:

c  1. NRANFIX, PARRANFIX(.), and RANFIXEST(.) are now added to DENFIL, 
c  both files 23 and 33. This means that the code at the top of these
c  files will be changed from DENSITY APR_10 to DENSITY OCT_15. 
c  Similarly for the output file, file 25, whose code at the top will 
c  change from VERSION 42 on line 1 to VERSION 43, and whose code on 
c  line 2 will change from VER_BAK JUL_11 to VER_BAK OCT_15.

c  2. Similarly, NRANFIX, PARRANFIX(.), and RANFIXEST(.) are added to 
c  the writing of the NP_RFxxxx.TXT file by Subroutine READOUT, which
c  means the code for that file on the top line will change to 
c  VERSION 1.8 - OCT 2015. The module which has Subroutine READOUT will 
c  change from read22.f to read23.f.

c  3. Two of the modules linked with the main "engine" module will be
c  updated. idm2x17.f will be replaced by idm2x18.f, and idm3x18.f will
c  be replaced by idm3x19.f. The changes will not be functional, just
c  comments that NPP now also includes the no. of RANFIX parameters
c  (in addition to the no. of random and fixed parameters).


c  4. This "engine" program will be part of the NPAG119.FOR program.
c  Note that NPAG119.FOR will have many changes related to items 1. and
c  2. above, and also because npag102.inp will be updated to be  
c  npag103.inp, which will include NRANFIX, PARRANFIX(.), and 
c  RANFIXEST(.).

c  5. Formats 1657 and 7124 are changed to show that the output file
c  is made by npagranfix4 rather than npagranfix3.

c-----------------------------------------------------------------------

c  npagranfix3.f                                           10/06/15

c  npagranfix3 has the following changes to npagranfix2:

c  1. CALCRF is changed so that it rejects any candidate vectors which
c  have random variable values outside their input ranges. In such
c  case, FNTVAL is returned as an unattractive value, 1.D30.
 
c  2. Formats 1657 and 7124 are changed to show that the output file
c  is made by npagranfix3 rather than npagranfix2.

c-----------------------------------------------------------------------

c  npagranfix2.f                                          10/05/15

c  npagranfix2 has the following change to npagranfix:

c  1. The ELDERY code now optimizes over not just the NRANFIX parameters
c  which are unknown but the same for all subjects, but also the 
c  NVAR random parameters (using the current cycle means as the 
c  initial estimates for these NVAR parameters). This will take more
c  time, but is intended to bring all parameters up to their best
c  current estimates simultaneously. At the end of this block of code,
c  the grid values in CORDEN(.,.) are adjusted so the means of each
c  of the NVAR random parameters are equal to the just found new
c  estimates from ELDERY. But this code is only executed after cycle
c  no. 1.

c  2. The SIG(I,J) code in Subroutine CALCRF now has the ierrmod
c  code following it, as it should always have had.

c  3. Formats 1657 and 7124 are changed to show that the output file
c  is made by npagranfix2 rather than npagranfix.


c-----------------------------------------------------------------------

c  npagranfix.f                                            9/20/15

c  npagranfix is an extension to npageng30. It has the added capability
c  to estimate the value of parameters that are the same for all 
c  subjects.

c  This means that now the user can designate parameters as fixed
c  (IRAN(.) = 0), random (IRAN(.) = 1), or ranfix (IRAN(.) = 2).

c  Formats 1657 and 7124 are changed to show that the output file
c  is made by npagranfix rather than npageng29.

c-----------------------------------------------------------------------

c  npageng30.f                                             3/11/15

c  npageng30 has the following change from npageng29:

c  All numbers written out in F or G format are now tested to see if
c  they are inside [-1.D-99, 1.D-99]. If so, they are changed to be
c  0. The reason is that otherwise they will be printed out without 
c  the accompanying D or E (e.g., as .934-106, rather than .934E-106).

c  Note that a new Subroutine, VERIFYVAL is added to the code to 
c  do the indicated testing above.

c-----------------------------------------------------------------------

c  npageng29.f                                             12/16/14

c  npageng29 has the following changes to npageng28:

c  1. The statement preceding the DO 800 loop in MAIN is changed from 
c  !$omp Do Schedule(guided)  to  !$omp Do 

c  This change removes the slight randomness (typically in about the
c  12th significant digit in the log-liks) that sometimes appears 
c  when the program is run repeatedly on the same problem.

c  2. Formats 1657 and 7124 are changed to show that the output file
c  is made by npageng29 rather than npageng28.

c-----------------------------------------------------------------------

c  npageng28.f                                             11/25/14

c  npageng28 has the following changes to npageng27:


c  1. It has the parallel code (i.e., the Threadprivate and
c  other statements) to make this program run in parallel, using the
c  compilation command, 
c  gfortran -O3 -fopenmp -fmax-stack-var-size=32768 -o npageng28p.exe 
c  npageng28.f ... npagdriv.f.

c  Note that all the accompanying modules (except blasnpag.f and
c  read21.f) will be updated to have the necessary parallel code
c  (the Threadprivate, Save, etc. commands).

c  Note that !$omp lines are not comments, but statements read by the
c  compiler to tell it how to parallelize. But lines beginning with
c  just ! are simple comment lines.

c  So, shift9.f will be updated to shift10.f; idm1x16.f will be 
c  updated to idm1x17.f; idm2x15.f will be updated to idm2x16.f;
c  idm3x16.f will be updated to idm3x17.f. And, the new template 
c  model file will be TSTMULTN.FOR (updated from TSTMULTM.FOR).

c  Note that CALL SYMBOL has been removed from idm1x17.f/Subroutine
c  IDPC, and instead inserted into MAIN just above label 1001.


c  2. The code for AICC is changed. If the denominator,
c  NOBTOT-KP-1 is .LE. 0, then AICC will be set = the AIC
c  (i.e., in this case, the old version of AIC will be used).

c  3. Formats 1657 and 7124 are changed to show that the output file
c  is made by npageng28 rather than npageng27.

c-----------------------------------------------------------------------

c  npageng27.f                                             8/20/14

c  npageng27 has the following change to npageng26:

c  1. Subroutine NEWWORK1 is modified so that a steady state regimen 
c  now translates to 101 dose sets, rather than 100. The effect is
c  that if, for example, a patient has an observation at time t = 2 hrs.
c  after a steady state regimen with an inter-dose interval (II) of
c  24 hours, this observation will now be 2 hours after the beginning of
c  the last (the 101st) dose set, as opposed to 26 hours (after the
c  100th dose set in the previous program).


c  Also note that an extra check is put into Subroutine NEWWORK1 to
c  make sure that no two dose times are the same. This could happen,
c  for example, if the 101st dose set of a steady state dose regimen
c  had an ending IV time of 3 hours, and the first non-steady-state
c  IV dose following started at t = 3 hours. Having two dose times which
c  are the same can confuse the logic in Subroutine Shift, and cause
c  it to get into an infinite loop (see NPAG115.EXP, TESTCASE 5).

c  2. Formats 1657 and 7124 are changed to show that the output file

c  is made by npageng27 rather than npageng26.

c  3. The formula for AIC is changed.

c-----------------------------------------------------------------------

c  npageng26.f                                             8/05/14

c  npageng26 has the following changes to npageng25:

c  1. Formats 1657 and 7124 are changed to show that the output file
c  is made by npageng26 rather than npageng25.

c  2. The 3 ID modules are updated: idm1x15.f to idm1x16.f; idm2x14.f
c  to idm2x15.f; and idm3x15.f to idm3x16.f.

c  3. The formulas for AIC and BIC are changed.

c-----------------------------------------------------------------------

c  npageng25.f                                             3/10/14

c  npageng25 has the following changes from npageng24:

C  1. ICENT WILL NOW BE IRRELEVANT. PREVIOUSLY, THE USER ENTERED ICENT
C  AS 1, 2, OR, 3 TO SELECT WHETHER HE WANTED THE PREDICTED VALUES 
C  WRITTEN TO PRTBxxxx TO BE BASED ON THE BAYESIAN MEANS, MEDIANS, OR
C  MODES, RESPECTIVELY. NOW, ICENT WILL BE UNUSED SINCE THE PRTBxxxx 
C  FILE WILL INCLUDE THE PREDICTED VALUES FOR ALL 3 OF THESE
C  MEASURES. SIMILARLY THE OUTPUT FILE WILL NOW HAVE AUC TABLES FOR ALL
C  3 OF THESE MEASURES (PREVIOUSLY IT JUST HAD THE AUC TABLE BASED ON
C  THE MEASURE SPECIFIED BY ICENT). NOTE THAT THE NEW PC PREP PROGRAM
C  IS NPAG113.FOR.

C  2. THE MAXIMUM NO. OF OUTPUT EQUATIONS WILL BE CHANGED FROM 6 TO 
C  NUMEQT, WHICH IS SUPPLIED IN THE ARGUMENT LIST TO SUBROUTINE NPAG.
C  THIS MEANS THAT NUMEQT WILL NOW BE PASSED TO ALL THE SUBROUTINES
C  THAT NEED IT; AND IN THOSE SUBROUTINES, ANY 6 REFERRING TO THE MAX.
C  NO. OF OUTPUT EQUATIONS WILL BE CHANGED TO NUMEQT.

C  NOTE THAT THIS ALSO INCLUDES PASSING NUMEQT TO IDCALCY AND IDCALCYY
C  SO THAT YPRED AND YYPRED, RESPECTIVELY, AND BE VARIABLY DIMENSIONED
C  IN THOSE MODULES.

C  ALSO, IN THOSE ROUTINES WHERE ARRAYS ARE PASSED IN COMMON STATEMENTS,
C  OR EXIST ONLY IN THOSE ROUTINES, DIMENSIONS RELATED TO THE 
C  MAX. NO. OF OUTPUT EQS. WILL BE DIMENSIONED BY A PARAMTER STMT.
C  SETTING MAXNUMEQ = 7, THE CURRENT LIMIT (SINCE THESE ARRAYS CANNOT
C  BE VARIABLY DIMENSIONED BY A CALLING ARGUMENT). THIS INCLUDES IN 
C  MAIN, FILRED, AND OTHER ROUTINES IN THE ID MODULES WHERE YOO IS
C  PASSED IN COMMON/OBSER. IT ALSO INCLUDES OBSBLOCK IN SUBROUTINES
C  NEWWORK1 AND READOUT. AND IT INCLUDES SUBROUTINE OUTPUT IN THE NEW
C  TEMPLATE MODEL FILE, TSTMULTM.FOR (SEE CHANGE 3.).

C  THE NEW ID MODULES TO BE LINKED WITH THIS MAIN MODULE ARE
C  idm1x15.f, idm2x14.f, AND idm3x15.f.

C  3. NOTE THAT THE TEMPLATE MODEL FILE FOR THIS PROGRAM HAS BEEN
C  CHANGED FROM TSTMULTL.FOR TO TSTMULTM.FOR (SEE COMMENT 2. ABOVE).

C  4. read19.f IS CHANGED TO read20.f. THIS IS BECAUSE THAT MODULE
C  NO LONGER WILL READ AUC VALUES FROM THE OUTxxxx FILE. I.E., AUC
C  VALUES WILL NO LONGER BE INCLUDED IN THE NP_RFxxxx.TXT FILE.

c  5. Formats 1657 and 7124 are changed to show that the output file
c  is made by npageng25 rather than npageng24.


c-----------------------------------------------------------------------

c  npageng24.f                                             6/2/13

C  npageng24 has the following changes to npageng23:

C  1. THE CODE TO READ extnum TO GET THE 4-DIGIT JOB NUMBER IS MOVED TO

C  THE TOP OF THE CODE, SO ERRORxxxx CAN BE OPENED AND THEN FILLED 
C  AT EARLIER LOCATIONS IF THE PROGRAM STOPS ABNORMALLY. IN PARTICULAR,
C  ERRFIL IS ADDED TO THE ARG. LIST OF SUBROUTINE GETIPATF SO IT CAN BE
C  WRITTEN TO IF THERE IS AN ERROR IN THAT ROUTINE. ALSO, FORMATS
C  4706 (SEE BELOW) AND 1721 ARE NOW WRITTEN TO ERRFIL IF THERE IS AN
C  ERROR IN THOSE LOCATIONS.

C  NOTE THAT A NEW FORMAT, 4706, IS USED INSTEAD OF FREE FORMAT TO TELL
C  THE USER THAT npag102.inp IS NOT AVAILABLE, AND THE PROGRAM IS
C  THEREFORE STOPPING.

C  2. FORMAT 26 IS CHANGED TO INCLUDE JSUB, SO THE USER WILL KNOW WHICH
C  IS THE FIRST SUBJECT TO CAUSE THE ALL P(YJ|X) = 0 ERROR.

C  3. npageng24 is the main module for NPAG112.FOR.

c  4. Formats 1657 and 7124 are changed to show that the output file
c  is made by npageng24 rather than npageng23.

c-----------------------------------------------------------------------

c  npageng23.f                                             4/14/13

c  npageng23 has the following changes to npageng22:

c  1. TOL is no longer read in from npag102.inp. The read-in value 
c  wasn't used anyway since TOL was subsequently hardcoded to be 1.D-4.

c  Instead, the tolerance parameter read in is now TOLC, and this value
c  will be the one against which checkbig is compared from now on 
c  (rather than a hardcoded value of .01 as was used previously).

c  Note that the following is a quick summary of how the tolerance 
c  parameters are used to establish convergence of the algorithm:

c  a. The program keeps track of the log-lik improvement between 2 
c  consecutive regular cycles. If this improvement is not at least
c  TOL (1.D-4), then some perturbed grid points are added to the grid,
c  at a resolution of 20% of the range of the parameters. This process
c  continues with the resolution halved each time a regular cycle 
c  doesn't have a log-lik improvement of at least TOL, and it continues
c  until the resolution gets down to .0001. That marks the end of a
c  "major cycle".
 
c  b. The log-lik difference between 2 consecutive "major cycles" must
c  be .LE. TOLC (which is now read in from npag102.inp, rather than 
c  hardcoded to be .01).

c  Note that Format 1223 is changed so that TOLC is now included in the
c  output file.

c  2. If the program bombs, the message that is written to the screen 
c  will now also be written to the file ERRFIL = ERRORxxxx, where xxxx
c  is the 4-digit run no. In this way, if the program is being run using
c  Pmetrics, the Pmetrics program can respond appropriately. Note that
c  ERRFIL must be passed to all the routines which could write to it
c  using COMMON/ERR/ERRFIL.

c  3. Formats 1657 and 7124 are changed to show that the output file
c  is made by npageng23 rather than npageng22.

c  4. Subroutine SUBRES now also stores each subject's Bayesian 
c  posterior density into arrays BAYPOS and NACTSUB, which are passed
c  via COMMON/BAY to Subroutine READOUT. They will be written into
c  the NP_RFxxxx.TXT file. But note that, due to size restrictions
c  (see NPAG111.EXP), only the first 100 subjects worth of values can
c  be stored at present.

c  Also note that only the grid points that are active for each subject
c  are used to calculate Bayesian subject values now, rather than all the
c  NACTVE grid points from the population final density.

c  5. This program is now compiled with read19.f, which replaces
c  read18.f. The other modules remain unchanged.

c  6. Change 4. in npageng20.f is reversed. Now the previous cycle's 
c  density will again be saved each cycle, except for cycle 1. But this
c  can be manually changed by changing the value of ISAVEDEN (below 
c  label 1243) to something other than 1.

c  7. Several C???DEBUG comments are removed in MAIN ...
c  from write(*,*)' About to create density file ...'  to
c  NOW CLOSE THE FILES USED BY READOUT. These statements are no longer
c  debug statements.


c-----------------------------------------------------------------------

c  npageng22.f                                             11/8/12

c  npageng22 has the following change from npageng21:

c  1. It comments out the PAUSE statement following Format 164 in 
c  Subroutine emint. Reason: the program will not complete properly if 
c  it is run under  Pmetrics (which cannot supply a keyboard response
c  during a run). 

c  2. Formats 1657 and 7124 are changed to show that the output file
c  is made by npageng22 rather than npageng21. 

c-----------------------------------------------------------------------

c  npageng21.f                                             10/15/12

c  npageng21 has the following changes from npageng20:


c  1. The 3 id modules linked to this program have been updated; they
c  are now idm1x14.f, idm2x13.f, and idm3x14.f. These 3 id modules
c  correct a bug. Now R(.) are set = RS(.,.) before GETIX is called
c  in the time reset section (see details in those modules).

c  2. A bug which was introduced in npageng18.f (see change 1. there)

c  has been corrected. Instead of writing TPREDREL(.) to File 31 below
c  Format 2131, TTPREDREL(JSUB,.) is written. Note that TPREDREL(.) 
c  holds the values for the last subject only (no. NSUB), but Format
c  2131,which is in loop 7000 over all NSUB subjects, requires that
c  TTPREDREL(JSUB,.) be used so that each subject has its own predicted
c  times  written.

c  3. Another bug is fixed in the AUC part of the code ... by using a
c  new parameter, IELAST, and initializing it to be 0 just below where
c  ILAST is initialized = 0 for each new subject. IELAST must be used in
c  addition to ILAST for those subjects whose AUC tables start with a 
c  partial period, rather than a whole period. In this case, for 
c  JSUB > 2, without IELAST, the index of which YYPRED(.,.) value starts
c  the current AUC would not be set correctly. See the details in the
c  code.

c  4. Formats 1657 and 7124 are changed to show that the output file
c  is made by npageng21 rather than npageng20. 

c  Note that npageng21.f is the main "engine" module for the new PC
c  prep program, NPAG110.FOR.

c-----------------------------------------------------------------------

c  npageng20.f                                             10/04/12

c  npageng20 has the following changes from npageng19:

c  1. It will now be linked with read18.f, rather than read17.f. The 
c  reason is that NOMAXTIM(.) in Subroutine CONVERGE2 must be 
c  initialized to be all 0's to avoid a possible bug if the program
c  is compiled and linked with gfortran (see details in read18.f). Also,
c  note that since the individual files needed by read18.f will now be
c  left open when that module is called, the combined output file,
c  OUTFILE, will no longer be needed as a calling argument to
c  read18.f (i.e., Subroutine READOUT). 

c  Note that not having Subroutine READOUT separate the combined output
c  file into the 4 needed individual files can save a lot of execution
c  time if this program is compiled and linked with gfortran.

c  2. It will now be linked with shift9.f, rather than shift7.f. The
c  reason is to fix a bug which occurred if a steady state dose had
c  bolus inputs (see details in shift9.f).

c  3. It will be linked with idm1x13.f (updated from idm1x12.f),
c  idm2x12.f (updated from idm2x11.f), and idm3x13.f (updated from
c  idm3x12.f). Each of the new id modules have new code to make sure
c  that Subroutine GETFA has the initial values for R(.) when it is
c  called the first time, and to make sure that GETFA is called just
c  before each time FA(.) are used, so the FA(.) are updated to current
c  values (see details in the id modules). 

c  4. See the code below Format 1243. Starting with this program, the
c  joint density will be saved only after cycle no. 2, not after every
c  cycle. See code below 1243 for the reasons.

c  5. Formats 1657 and 7124 are changed to show that the output file
c  is made by npageng20 rather than npageng19. 

c  Note that npageng20.f is the main "engine" module for the new PC
c  prep program, NPAG109.FOR.

c-----------------------------------------------------------------------

c  npageng19.f                                             8/19/12

c  npageng19 has the following changes to npageng18:

c  1. Preset NACTLAST TO BE NACTVE just below label 30. This avoids a
c  very unlikely and subtle error that would occur if the first
c  cycle of a run has a Hessian error. See comments below label 30

c  in main.

c  2. When a Hessian error occurs in Subroutine emint, now the value
c  of info will be reported to the user. If it is positive, it is 
c  the subject no. which  caused the Hessian error (see


c  \ALAN3\NEELY\VORI2\VORI2.EXP for example).

C  3. NEW CODE IN SUBROUTINE NEWWORK1 IS USED TO ESTABLISH THE VALUES
C  FOR DOSEBLOCK, WITHOUT USING A BACKSPACE COMMAND. THE REASON IS THAT
C  DEPENDING ON WHICH COMPILER IS USED TO MAKE THE PR PREP PROGRAM
C  (CURRENT ONE IS NPAG108.FOR), IT IS POSSIBLE FOR A DOSE EVENT
C  TO LOOK LIKE SEVERAL LINES RATHER THAN ONE LONG WORD-WRAPPED LINE.
C  IN THE FORMER CASE, BACKSPACING ONE LINE WILL NOT BACKSPACE TO THE
C  BEGINNING OF THE DOSE EVENT AS SHOULD BE DONE. SO TO BE SAFE, THE
C  LOGIC TO USE BACKSPACE(23) WILL BE COMMENTED OUT, AND 
C  DOSEBLOCK(.,.,.) WILL BE ESTABLISHED DIRECTLY.

C  4. npageng19.f WILL BE COMPILED AND LINKED WITH NEW id MODULES:
C  idm1x11.f REPLACED BY idm1x12.f;
C  idm2x10.f REPLACED BY idm2x11.f;
C  idm3x11.f REPLACED BY idm3x12.f;

C  THE REASON IS TO CORRECT A BUG WHEN N = 0 (ANALYTIC SOLUTIONS CODED
C  INTO SUBROUTINE OUTPUT). IN THIS CASE, THE NDO = ND, 
C  SIGO(.) = SIG(.), ETC. SECTION IS SKIPPED; AND THIS MEANS THAT AT THE
C  END OF idm1x11.f, ND = ND0 BECOMES 0 --> THE NEXT TIME idpc IS 
C  CALLED, THE PROGRAM IS SCREWED UP.

C  THIS BUG ONLY HAPPENS IF N = 0 AND NO TESTCASES WITH N = 0, WHICH
C  WOULD HAVE DISCOVERED THE PROBLEM, WERE RUN SINCE idm1x9.f WAS 
C  USED.



C  THE SOLUTION, IN THE NEW id ROUTINES, IS TO MOVE THE NDO = ND, 
C  SIG0(.) = SIG(.), ETC. ... CODE TO JUST AFTER CALL GETFA.

c  See \ALAN3\NEELY\EMAX\EMAX.EXP for details.


c  5. Formats 1657 and 7124 are changed to show that the output file

c  is made by npageng19 rather than npageng18. 

c  Note that npageng19.f is the main "engine" module for the new PC
c  prep program, NPAG108.FOR.


c-----------------------------------------------------------------------

c  npageng18.f                                             7/01/12

c  npageng18 has the following changes from npageng17:

c  1. "Relative" rather than "real" times are written to the output 
c  files. This means that if a steady state dose set occurs, the output
c  times in the AUC tables, the DENxxxx file, the PTRTxxxx file, etc.
c  will now be based on the end of the steady state dose set rather than
c  the beginning. As an example, if the interdose interval of a steady
c  state dose set is 4 hours, and the last observation for that region

c  is 20 hours, the previous program would have written into PTRBxxxx
c  and the DENxxxx files times of 400.00, 400.0333,..., 444.00 (i.e.,
c  24 hours after the last observation time). Those are the "real" times
c  that are used by the id modules in this program. But now, the times
c  written to these files will be 0.00, 0.033,..., 44.00, which are the
c  "relative" times (i.e., those starting at the end of the 400 hours
c  of steady state dosing).

c  This change is made by having Subroutine CALCTPRED calculate
c  TPREDREL(.) in addition to TPRED(.). TPRED(.) is still used in the
c  call to Subroutine IDCALCYY (i.e., that routine must see the real
c  times), but the times in TPREDREL(.) are the ones written to the
c  output files).

c  Also, when writing the AUC tables, the "relative" times are written
c  rather than the "real" times ... by not adding TBEGGSUB(.,.) to the
c  calculation of IHRST (in the above example, TBEGGSUM(.,.) = 400 is
c  the length of the 100 steady state dose sets).

c  In addition, now Subroutine NEWWORK1 establishes TIMOBREL(JSUB,J)
c  which give the original ("relative") observation times passed via
c  npag102.inp, and these values, rather than the "real" ones stored
c  to File 27 by NEWWORK1, will be written to the end of the PRTBxxxx
c  file.

c  2. The module read16.f linked to this program will be updated to be
c  read17.f. read17.f will no longer read DOSEBLOCK(.,.,.) and
c  OBSBLOCK(.,.,.) from file 27; instead this info will be passed via
c  COMMON/DOSEOBS from Subroutine NEWWORK1. These values will be the
c  values from the patient files in the npag102.inp (i.e., before these
c  working copy format files are converted by SUBROUTINE NEWWORK1 to
c  have a full 100 dose lines for each steady state set, and to have
c  "real" rather than "relative" times in the dose/cov and observation
c  blocks).

c  All other modules linked with the main module, are unchanged.

c  3. Format 2053 has a typo correction (which required a corresponding
c  change in read17.f - see comment 1. in that file).

c  4. If a Hessian Matrix is singular in Subroutine emint, instead of
c  stopping immediately without writing the output files, now the
c  program sets IHESS = -1, and returns to MAIN, where control is
c  transferred to label 900, so the output files can be created, based
c  on the previous cycle's values. Then the program stops.

c  Note that in this case (IHESS = -1), the program will write into the
c  output file that it stopped because of a Hessian error, and it will
c  re-establish CORDEN to be = CORDLAST, which was the joint density
c  array from the previous cycle. This way, when Subroutine SUBRES is
c  called in loop 7000 at the end of the run, the values will all be
c  based on the values from the last fully completed cycle. Otherwise,
c  CORDEN would have been some hybrid combination of the CORDEN from
c  the previous cycle and the partially updated CORDEN from the
c  incomplete current cycle). And because of this new array, CORDLAST,
c  SUBROUTINE MAKEDRIV in the new NPAG107.FOR must be changed to 

c  include this array with variable dimensions as a calling argument
c  to Subroutine NPAG in this module (and CORDLAST must be dimensioned
c  below also).

c  5. Formats 1657 and 7124 are changed to show that the output file
c  is made by npageng18 rather than npageng17. 

c-----------------------------------------------------------------------

c  npageng17.f                                             4/10/12

c  npageng17.f has the following changes from npageng16:

c  It is the main engine module for NPAG106.FOR. The change is that this
c  program allows steady state dose sets to have bolus doses as well as
c  IVs (rather than being limited to just IVs). 

c  But each drug in a steady state dose set can have either
c  an IV or a bolus, not both. The reason is that the dose column in 
c  the .csv file is used for the total amount of drug given. If the
c  corresponding duration is 0, this represents a bolus; if the 
c  corresponding duration > 0, this represents the total amount of the
c  IV (with the indicated duration). If one wanted both an IV
c  and a bolus, there would be no way to impart this information in 
c  the two entries (DUR and DOSE).

c  To affect this change, several code changes are made to Subroutine
c  NEWWORK1.

c  In addition, the 3 id modules require code changes are updated to be
c  idm1x10.f, idm2x10.f, and idm3x11.f.

c  Also, formats 1657 and 7124 are changed to show that the output file
c  is made by npageng17 rather than npageng16. 

c-----------------------------------------------------------------------

c  npageng16.f                                             11/23/11

c  npageng16 has the following changes from npageng15:

c  1. It is the main engine module for the new PC prep program, 
c  NPAG104.FOR (which will be completed later). For now, it is 
c  compatible with NPAG103.FOR, as long as any subjects with steady
c  state dose sets are input in working copy format. The new
c  NPAG104.FOR/npageng16.f program now allows patient files to have 
c  steady state dose sets. In particular, this program now calls new
c  subroutine NEWWORK1 (based on the stand-a-lone program of the same
c  name) to read each working copy file in npag102.inp, which may have
c  steady state dose indicator lines, and convert it to the typical form
c  that that the id routines require, except that the steady steady 

c  state dose indicators themselves (negative dose times) remain in the
c  file.

c  2. blasnpag.f is not changed, but the other 5 permanent engine .f
c  files linked to this program, read15.f, shift6.f, idm1x6.f, 
c  idm2x6.f, and idm3x6.f, will be updated to be read16.f, shift7.f,
c  idm1x7.f, idm2x7.f, and idm3x7.f. The changes in the id routines will
c  allow them to read steady state dose indicators and know how to 
c  integrate through each set, testing for convergence of the steady
c  state compartment amounts to see if the program can fast forward to
c  the end of that set. The changes for read16.f are simply in the
c  dimensions related to no. of doses, which will change from 500 to 
c  5000 (see comment 4. below). The changes in shift7.f are the 
c  dimensions changes of read16.f, and some edited code required since

c  now a dose (time) reset occurs when a dose time is .LE. 0, rather 
c  than .EQ. 0.

c  3. All 500's related to dose times dimensions (which are passed in

c  COMMON/OBSER) are changed to 5000's to be compatible with the new
c  id routines. This is because in those routines, because of steady
c  state dose sets, the no. of dose times can expand to be much 
c  bigger than 500. All 500's will also be changed to 5000 in 
c  Subroutine FILRED.

c  4. Many changes are made in Subroutine CALCTPRED and Main (where

c  the AUC tables are calculated) to take into account that the 


c  predicted values to be calculated by IDCALCYY only start at t=0
c  if there is no active steady state dose set; if there is an active
c  steady state dose set, then the predicted values start at the end
c  of that dose set.

c  5. A bug in loop 2050 (calculating AUCs) is corrected (see code
c  involving new parameter ILAST).

c  6. When the variance of a parameter in a cycle is numerically .LE. 0,

c  rather than suppress all covariance-related output for that cycle,
c  as was done in previous programs, now the covariances, std. devs.,
c  and coefficients of variations will all be written as 0's, and the
c  correlations as -99's. This change is because suppressing the output
c  means that the NP_RFxxxx.TXT file (made by Subroutine READOUT) has 
c  fewer than expected std. devs., which screws up the R program which
c  reads that file. This same change is made in Subroutine SUBRES
c  for the Bayesian statistics for each subject.

c  7. Variables PRCN, tbeg, PRFIX2, EXT2, IFORMT, NSTORESV, IPRED, JCOL,
c  T1000A, saveres are now removed, as they are no longer used (and 

c  haven't been for some time).

c  8. Formats 1657 and 7124 are changed to show that the output file is
c  made by npageng16 rather than npageng15. 

c-----------------------------------------------------------------------

c  npageng15.f                                             7/29/11

c  npageng15 has the following changes from npageng14:

c  1. This module is part of the NPAG102.FOR program. That program was
c  changed to provide an npagdriv.f file (rather than an npemdriv.f 
c  file), and one of the changes in the new npagdriv.f file is the 
c  CALL NPAG statement, rather than a CALL BIGNPAG statement. As a 

c  result, the SUBROUTINE BIGNPAG statement below is replaced by 
c  SUBROUTINE NPAG.


c  2. The instruction file made by the PC prep program (NPAG102.FOR is
c  the first in the series) is changed from npembg34.inp to 
c  npag102.inp. This was done since now NDRUG and AF(I),I=1,NDRUG will
c  be in the file, as opposed to just AF (i.e., it is no longer
c  assumed that all drugs have the same active salt fraction).

c  3. Formats 1657 and 7124 are changed to show that the output file is
c  made by npageng15 rather than npageng14. Also in Format 1657, the
c  version no. is changed from 41 to 42 (since NDRUG and 
c  AF(I),I=1,NDRUG will be written to the output file, rather than
c  just AF).

c  4. This program will now be linked with read15.f, updated from
c  read14.f (which must be updated to write NDRUG and AF(I),I=1,NDRUG,
c  rather than just AF). Also, it reads info regarding ICONVERGE
c  differently now (see 5. below). The other permanent modules, 
c  blasnpag.f, shift6.f, idm1x6.f, idm2x6.f, and idm3x6.f are unchanged.

c  5. New code involving IMAXCYC enables the program to test whether
c  the analysis converged at MAXCYC. Previously, if ICYCLE = MAXCYC,
c  the program would stop without checking to see if convergence was
c  also achieved. Now convergence will still be tested even if ICYCLE
c  = MAXCYC. Note that new parameter, ICONVERG, along with IMAXCYC
c  will enable the program to write below label 900 the reason the

c  program stopped (because of MAXCYC being run; because the analysis
c  converged; or because of both of the above) - see formats 5197 
c  through 6001.

c-----------------------------------------------------------------------

c  npageng14.f                                             6/29/11

c  npageng14 has the following changes to npageng13:

c  1. The comment in the output file regarding the stopping criterion is
c  changed (JSTOP has been ignored since Bob Leary's adaptive grid
c  algorithm was inserted in 2000). TOL was still used in a complicated
c  way in the stopping criterion, but was not allowed to be < 1.D-4. To
c  simplify the code, TOL will now not be used as read in from 
c  npembg34.inp. Instead it will always be hardcoded to be 1.D-4.

c  Note that though JSTOP and TOL are not used, they are still read in
c  from npembg34.inp, so the format of this file does not have to be
c  changed.

c  Formats 1221 and 1222 are changed to report the starting cycle no.,
c  and the maximum cycle no., and new format 1223 reports that the 

c  program will stop prior to the max. cycle no. if convergence is
c  achieved.

c  Note also that all code involving PRESLP, which was related to JSTOP,
c  has been removed.


c  New formats 5197 and 5198 write to the output file why the analysis

c  stopped (i.e., either MAXCYC reached or convergence was obtained).

c  2. New formats 9771 and 9772 write to output file 25 whether the
c  apriori density is uniform or is a prior run's density file; they
c  replace the previously unformatted write statements.

c  3. Since all ATOL(I) are set = RTOL, the comment in the output file
c  is simplified to report just RTOL as the value of the tolerance used
c  in the differential equation solver (VODE). See FORMAT 9769.

c  4. The comment in the output file that the no. of intervals used
c  in calculating the marginal density approximations is 100 is removed
c  since this value (NINT) is always 100.

c  5. Formats 1657 and 7124 are changed to show that the output file is
c  made by npageng14 rather than npageng13. Also in Format 1657, the
c  version no. is changed from 40 to 41 (since new/changed info will be
c  written to the output file).


c  6. Parameter ilast is removed from MAIN. It was set but never used.

c  7. This program will now be linked with read14.f, updated from
c  read13.f. The other permanent modules, blasnpag.f, shift6.f,
c  idm1x6.f, idm2x6.f, and idm3x6.f are unchanged.

c-----------------------------------------------------------------------

c  npageng13.f                                             4/28/11

c  npageng13.f has the following changes from bigmlt12.f:

c  1. This is the main module in the program whose PC Prep program is
c  the new NPAG100.FOR. As in that .FOR module, the formula for NI in 
c  Subroutine FILRED is changed from  NI = 2*NDRUG + 2 + NADD  to  
c  NI = 2*NDRUG + NADD, because from now on, WT and CCR will not be 
c  considered special covariates. If they are included in the working 
c  copy file, they will be part of the NADD 'additional' covariates 
c  (beyond the 4 permanent ones in Common DESCR).

c  Because of the above change, the shift5.f module linked to this
c  program will be updated to shift6.f.

c  2. This program will be linked with read13.f, updated from read11.f
c  and then two versions of read12.f.
 

c  The difference is that read13.f will include the assay coefficients
c  for each observation in the rfile, which will be renamed to be
c  NP_RFxxxx.TXT from RFILExxxx.TXT.

c  3. Formats 1657 and 7124 are changed to show that the output 
c  files are made by npageng13 rather than bigmlt12.

c-----------------------------------------------------------------------

c  bigmlt12.f                                              01/11/11



c  bigmlt12 has the following changes from bigmlt11:

c  1. Messages to the user are written after the end of the first cycle
c  (see format 1243 in MAIN) and in Subroutine emint (see format 123) 
c  so that he will know the program has not 'hung' if the adaptive grid
c  optimization takes several minutes (as it can with a large no. of 
c  subjects and grid points).

c  2. The Akaike Information Criterion (AIC), and the Schwartz 
c  (Bayesian) Information Criterion (BIC) are written to the output 
c  file. They are calculated similarly to how they are in the Big 
c  IT2B program. 


c  Note that this main "engine" module is now linked with read11.f, 
c  rather than read10.f. The difference is that read11.f stores the new
c  AIC and BIC values.

c  Also, of course, formats 1657 and 7124 are changed to show bigmlt12,
c  rather than bigmlt11.f.

c  Note that the first PC Prep program to use the bigmlt11.f "engine"
c  is NPBG15E6.FOR.

c-----------------------------------------------------------------------

c  bigmlt11.f                                              12/20/10

c  bigmlt11 is the same, functionally, as bigmlt10.f. The difference 
c  is that it is linked with idm1x6.f, idm2x6.f, and idm3x6.f, all
c  updated from the _____5.f versions. The other permanent .f files, 
c  blasnpag.f, shift5.f, and read10.f are unchanged.

c  Also, of course, formats 1657 and 7124 are changed to show bigmlt11,
c  rather than bigmlt10.f.


c  Note that the first PC Prep program to use the bigmlt11.f "engine"
c  is NPBG15E5.FOR.

c-----------------------------------------------------------------------

c  bigmlt10.f                                              12/13/10

c  bigmlt10 has the following changes to bigmlt9:


c  1. In Subroutine SUBRES, the NACTVE grid points from the final cycle
c  are checked and the no. which are "active" for each subject (i.e.,
c  within 1.D-10 of the maximum density for that subject) is printed to

c  the screen and File 25 (previously just the no. of active grid pts.
c  from the joint density of the final cycle was printed).

c  2. When Andreas' Intel compiler compiles the program, it objects to
c  RPAR and IPAR being dimensioned (as (*)) in Subroutine DVODE (and
c  routines called by DVODE) when they are not dimensioned in 
c  Subroutine USERANAL (in idm1x5.f). The comments in DVODE state that
c  if these values are not being used, they do not need to be 
c  dimensioned in routines that call DVODE. Nevertheless, to remove the

c  Intel objection, RPAR(*) and IPAR(*) are removed from the 5 routines
c  in this module which declare them arrays.

c  Similarly, RTOL supposedly does not need to be dimensioned in 
c  USERANAL since it is a scalar, but the Intel compiler objects to
c  having it dimensioned (*) in DVODE, etc. when it is a scalar in
c  USERANAL. So, all RTOL(*) occurrences are removed in this module,
c  and all references to RTOL(1), RTOL(I), etc. are changed to RTOL.

c  3. Formats 1657 and 7124 are changed to show bigmlt10, rather than 
c  bigmlt9.

c  Note that the first PC Prep program to use the bigmlt10.f "engine"
c  is NPBG15E4.FOR.

c-----------------------------------------------------------------------

c  bigmlt9.f                                               11/21/10

c  bigmlt9 has one major change to bigmlt8:

C  A CALL TO NEW SUBROUTINE PAUSE REPLACES EACH PAUSE STATEMENT. 
C  THIS IS BECAUSE A PAUSE STATEMENT CAUSES A WARNING WHEN THE PROGRAM
C  IS COMPILED AND LINKED USING gfortran (AND IT FORCES THE USER TO 
C  TYPE "go" INSTEAD OF SIMPLY HITTING THE ENTER KEY). ALSO, SEVERAL
C  PAUSE STATEMENTS THAT WERE PREVIOUSLY COMMENTED OUT ARE NOW 
C  "REINSTATED" WITH CALL PAUSE COMMANDS.

c  Also, formats 1657 and 7124 are changed to show bigmlt9, rather than 
c  bigmlt8.

C  Note that one of the modules linked to the bigmlt9.f "engine" is
c  read10.f (updated from read9.f). The only change in read10.f is the
c  same one as above in this module.

c  Note that the first PC Prep program to use the bigmlt9.f "engine"
c  is NPBG15E3.FOR.

C-----------------------------------------------------------------------

c  bigmlt8.f                                               11/10/10



c  bigmlt8 has the following changes to bigmlt7:

c  1. In Subroutine CALCTPRED, a code change is made so that an initial 
c  observation time of 0 does not trigger a "time reset".

c  2. Formats 1657 and 7124 are changed to show bigmlt8, rather than 
c  bigmlt7.

c  3. At the end of the run, the logic of read9.f (SEE
c  \ALAN3\NEELY\READOUT\READOUT.EXP) will create RFILExxxx.txt, an 
c  output file which is easy to use with the program, R. This will
c  be done by calling READOUT, the main routine in the module read9.f,
c  which is now compiled with this program. 

c-----------------------------------------------------------------------

c  bigmlt7.f - revised                                        11/05/10

c  The revised version of bigmlt7.f is identical to the original, except
c  that formats 1657 and 7124 are changed to show that the output 
c  files are made by bigmlt7.f, rather than bigmlt6.f

c-----------------------------------------------------------------------

c  bigmlt7.f                                                  10/26/10

c  bigmlt7 is the same as bigmlt6 except at the end of the run, new 
c  Subroutine CONDENSE (based on the free standing program, 
c  CONDENSE.FOR) is used when writing lines to the combined output 
c  file, OUTxxxx. This makes OUTxxxx a much smaller file - by only 
c  using line sizes which are required for each line, rather than 
c  always using A1000 as the the format.

c-----------------------------------------------------------------------

c  bigmlt6.f						   4/3/10


c  bigmlt6 has the following changes to bigmlt5:


c  1. In bigmlt5.f, the PRTB file was not completely written in the case
c  where there were time resets in the patient data files. This was
c  because the following condition in idm3x4.f just below label 45
c  was never engaged ...
c	IF(TPRED(KNT) .EQ. 0.D0 .AND. SIG(KNS) .EQ. 0.D0) THEN
c  ... since the predicted times went monotonically from 0 to 24 hours
c      past the last observed value time over all the subjects (this
c      value, T_END, was calculated by NPBIG15D.FOR and passed to 
c      bigmlt5.f in npembg34.inp). Instead the observed value times 
c      should have gone from 0 to 24 hours past the largest observation 
c      time before the next time reset value of 0, for each subject 
c      individually. This logic will be done in bigmlt6; these 
c      predicted value times will be calculated by routine CALCTPRED 
c      for each subject in turn. i.e., there will now be as many 
c      predicted sets of concentrations as there are time resets.

c  Also note that the AUCs in bigmlt5.f were not calculated correctly
c  when the patients had time resets, for the same reason as indicated
c  in the above paragraph. But now there will be one AUC table for 
c  each time reset for each output equation for each subject.

c  See new Subroutine CALCTPRED which calculates the NUMT(JSUB) values
c  in TPRED, based on each subject's observed value times. Note that
c  these observed value times will be IDELTA minutes apart, and will
c  consist of times up to 24 hours after each max. observed value time 
c  (and there will be as many sets of times for each subject as there
c  are time resets).

c  2. The code for the density file will now be changed since NUMT will
c  now be an array (since it will be different for each subject).
c  i.e., NUMT(JSUB) and TTPRED(JSUB,.) will have to be written for each 
c  subject (see code). Also, since the combined output file includes the 
c  density file, its code will be changed also. i.e., formats 1657 and 
c  7124 now show bigmlt6 as the main "engine" module. Note that TPREDD
c  is now changed to TTPRED to be consistent with the names in 
c  NPBIG15E.FOR (the PC Prep program) associated with this program.


c  Note that, though T_END will not be used anymore, it will still be
c  read in from npembg34.inp so the structure of this file will not
c  have to change. Note that IDELTA will still be used.

c  3. bigmlt6.f will now be linked with 3 new id modules (idm1x5.f,

c  idm2x5.f, and idm3x5.f). The previous id modules had a bug related
c  to the time resets (see code in the new id modules).

c-----------------------------------------------------------------------

c  bigmlt5.f							12/12/09

c  bigmlt5.f is exactly the same as bigmlt4.f, except ...
c  formats 1657 and 7124 now show bigmlt5 as the main "engine"
c  module.

c  The new name (bigmlt5 vs. bigmlt4) is used since the 3 id files
c  this program calls (idm1x4.f, idm2x4.f, and idm3x4.f) are updated

c  from the previous set (idm1x3.f, idm2x3.f, and idm3x3.f).

c-----------------------------------------------------------------------

c  bigmlt4.f							9/18/09

c  bigmlt4 has the following changes from bigmlt3:

c  1. The modules with which it is linked are changed:
c  idm1x2.f, idm2x2.f, and idm3x2.f are changed, respectively to
c  idm1x3.f, idm2x3.f, and idm3x3.f. Also, shift4.f is changed to
c  shift5.f.

c  2. Subroutine XERRWD is changed so that it writes no warning


c  messages to the screen. Instead, the no. of calls to XERRWD is
c  passed back to main and written to the screen in loop 800 (see
c  code related to NXE in several places). The reason is that if 
c  there are a lot of warnings written to the screen, it can slow the 
c  overall program down a lot (in one example, it slowed the program 

c  by a factor of almost 2.5).

c  3. All references to NTLAG have been removed. The reason is that
c  the user now codes explicitly his/her formulas for TLAG (and FA and
c  IC) into his model file (the new template is TSTMULTG.FOR).

c  4. bigmlt4.f is the new main "engine" module for NPBIG15C.FOR 
c  (updated from NPBIG15B.FOR).

c  5. Formats 1657 and 7124 now show bigmlt4 as the main "engine"

c  module.


c-----------------------------------------------------------------------

c  bigmlt3.f							9/4/09


c  bigmlt3 is the same as bigmlt2 except that formats 2048, 2049, 2051,
c  and 2052 have been changed to allow for larger numbers.


c  Also, the modules it calls are changed:
c  idm1x1.f, idm2x1.f, and idm3x1.f are changed, respectively to
c  idm1x2.f, idm2x2.f, and idm3x2.f.

c-----------------------------------------------------------------------

c  bigmlt2.f							7/7/09

c  Slight correction from original bigmlt2.f (dated 6/1/09):


c  BS(500,3) is changed to BS(500,7) in the dimension statement of
c  the main module (this change should have been made at the same time
c  the chnage was made in Subroutine FILRED - with the program
c  bignpaglap2.f.

c-----------------------------------------------------------------------

c  bigmlt2.f							6/1/09

c  bigmlt2 is the main module for the "engine" which corresponds to 
c  the most recent multiple drug PC PREP PROGRAM, NPBIG15A.FOR. The

c  other modules of the "engine" are idm1x1.f, idm2x1.f, idm3x1.f,
c  shift3.f, and blasnpag.f (the latter two of which are unchanged from 
c  the previous version). The changes in this modules from bigmlt1.f are
c  essentially the changes that the single drug program, big28.f, had
c  from big24.f. They are as follows:

c  1. The time interval for AUCs, instead of being hardcoded to be 24
c  hours, will be read in from the instruction file, npembg34.inp
c  (changed from npembig33.inp). This value is called AUCINT.

c  The first Big NPAG PC Prep program which is compatible with this
c  program (i.e., makes npembg34.inp) is NPBIG15A.FOR.

c  Also, formats 1657 and 7124 are changed so the name of this "engine"
c  program, bigmlt2.f, is written.

c  2. THE DIMENSIONS OF AB, PAR, PARFIX, VALFIX, AND IRAN ARE 

C  MADE CONSISTENT WITH THE MAXIMUM ALLOWED VALUES (A MAXIMUM OF max_pop_rand_varbs 
C  RANDOM PARAMETERS AND max_pop_params FIXED PARAMETERS). IN PARTICULAR:
C  ALL ARRAYS RELATED TO THE NO. OF RANDOM VARIABLES ARE NOW DIMENSIONED 
C  max_pop_rand_varbs; ALL ARRAYS RELATED TO THE NO. OF FIXED PARAMETERS ARE NOW 
C  DIMENSIONED max_pop_params; AND IRAN IS STIL DIMENSIONED max_ODE_params.

C  ALSO, ALL FORMATS RELATED TO THE NO. OF RANDOM VARIABLES ARE CHANGED
C  TO max_pop_rand_varbs FROM 25; SIMILARLY THOSE RELATED TO THE NO. OF FIXED VARIABLES

C  ARE CHANGED FROM 12 TO max_pop_params.

C  NOTE THAT THIS PROGRAM WILL BE COMPILED AND LINKED WITH A NEW MODEL
C  FILE TEMPLATE, TSTMULTE.FOR, IN WHICH PSYM IS DIMENSIONED max_ODE_params
C  (INCREASED FROM 25) IN SUBROUTINE SYMBOL. ALSO SEE BELOW FOR ANOTHER
C  CHANGE TO TSTMULTE.FOR.


c  3. READLARG has been changed from *300 to *1000, and FORMAT
c  2717 has been changed from A300 to A1000. The reason is that in a
c  run using George's 24 parameters (see \ALAN3\GEORGE\DRUSANO7.EXP),
c  NPBIG10H bombed when doing option 5 on \ALAN3\BIGNPAG\OUT0436 - the 
c  reason is that big25.f cut off the output file lines at entry 300
c  and this cut off the means, covs, etc. of parameter no. 24. Then,
c  NPBIG10H couldn't read the 24th no. and bombed with an 
c  "invalid numeric input" error.

c  4. READLINE has been changed from *78 to *300 in several routines,
c  and the corresponding format for READLINE has been changed to
c  A300 from A78 in each of those routines. The reason is that
c  in one of the patient data files that George sent, his observed

c  values went past column 78 (which --> the values were cut off
c  when read by big25).

c  5.In addition to storing YPREDPOP, this program now stores 
c  YPREDPOPT(JSUB,IEQ,J,ICENTER) = the predicted value for Y for
c  subject JSUB, for output equation IEQ, for time J, for 
c  ICEN = 1 (means), 2 (medians), AND 3 (modes), where the means, 
c  medians, and modes are from the final cycle population density.



c  These values differ from YPREDPOP in that the observed values
c  do not occur at the J=1,NOBSER observation times in a patient's
c  data file. Instead, they occur at the values of t inside TTPRED

c  (see the logic for forming the PRTBxxxx file = file 31), which are
c  all the times from t = 0 till t = 24 hours past the last obs. time
c  among all the patient data files ... but the no. of values is
c  capped at 7201 (see logic below for doing this).

c  Because the density file, and therefore the combined output file,
c  have extra info (YPREDPOPT) in them, they will have a new code.
c  In particular, format 1657 will be changed to specify VERSION 39
c  (and MADE BY bigmlt2.f), and format 7124 will be changed to specify
c  DENSITY JUN_09 (and MADE BY bigmlt2.f).

c  6. 3 modules which are linked into this program are changed. In
c  particular, idfix5g.f, idcy_53g.f, and idcy_63g.f are changed, 
c  respectively, to idm1x1.f, idm2x1.f, and idm3x1.f. These new modules 
c  allow the extra option of setting initial compartment amounts from 
c  their initial concentrations - see code in Subroutines FUNC, FUNC2, 
c  and FUNC3. And they allow patient data files to have "reset" values 
c  of 0 in the dosage and sampling blocks. Whenever, in Subroutine FUNC 
c  (or FUNC2 or FUNC3) the program sees a SIG(.) = 0 and a TIM(.) = 0, 
c  it knows that a large enough time has passed since the last dose
c  that all compartment amounts are to be reset = 0. Subsequent dose
c  and observed value times are values from this point.

c  Note that the modules blasnpag.f and shift3.f are unchanged.
c  Note that the new model file template, TSTMULTE.FOR, has code in
c  Subroutine SYMBOL that allows the user to set up the option indicated
c  above of setting initial compartment amounts from their initial
c  concentrations.


c-----------------------------------------------------------------------

c  bigmlt1.f							1/6/08

c  bigmlt1.f is the main module for the "engine" which corresponds to 
c  the most recent multiple drug PC PREP PROGRAM, NPBIG15.FOR. The

c  other modules of the "engine" (idfix5g.f, idcy_53g.f, idcy_63g.f,
c  shift3.f, and blasnpag.f) are unchanged. The changes in this module


c  from bignpaglap4.f are as follows:

c  1. It reads in a different instruction file, npembig33.inp (updated

c  from npembig3.inp). npembig33.inp has extra info. Instead of just

c  NSUB, the no. of subjects, it also has NSUBTOT and IPATVEC(I),
c  I=1,NSUB, where NSUBTOT is the total no. of subjects in the


c  patient population (all of these subject files are concatenated
c  on npembig33.inp), NSUB is the no. of these subjects which are to
c  be analyzed in this run, and IPATVEC(I),I=1,NSUB are the indices
c  of these "active" subjects for this run. Note that the first
c  PC PREP program which creates npembig33.inp is NPBIG15.FOR.


c  The code will be changed to write just the "active" subject data
c  files (IPATVEC(I),I=1,NSUB) onto the scratch file (27) to be analyzed

c  during this run (see code below label 1717).

c  2. This program will write just the "active" NSUB patient data files
c  into the output file, along with IPATVEC(I),I=1,NSUB, so the PC PREP 
c  program will know which subjects were analyzed. Because of the extra 
c  info to be put into the output file (NSUBTOT, IPATVEC), the code for 
c  the output file will be changed from VERSION 37 to VERSION 38 in 
c  FORMAT 1657.

c  3. New subroutines, WRITEPT2, GETIPATF, GETNUMSF, GETSUB are
c  added.

c  4. Note that formats 1657 and 7124 are changed to write out this
c  program "bigmlt1. And VER_BAK AUG_02 is changed to VER_BAK NOV_07
c  just below format 1657.

c  5. In subroutine EMINT,  rmax = -1.e100  is changed to  rmax = -1.e38
c  to be compatible with the Fortran F77L3 compiler (i.e., e100 is too
c  big for that compiler). In addition, all etime(dummy) references are
c  changed to 0 (since the PC compiler doesn't recognize etime).

c  6. In SUBROUTINE DPOTRF, both references to  ILAENV are removed. It
c  is never used, and being declared EXTERNAL causes an error when
c  the program is linked on the PC using the F77L3 linker.

c  7. The screen dump of all output info from the main module has 
c  been replaced by four lines having just the cycle no., convergence 
c  criterion, and the medians. The exception is that until
c  NACTVE .LE. NSTORE, the program will print to the screen the update 
c  on what % of grid points have been calculated since otherwise the 
c  user might think his computer has locked up (once NACTVE .LE. NSTORE, 
c  all the P(YJ|X)'s will already be stored into PYJGX --> the DO 800 
c  loop will go very fast.


c  Note that ISUPRES will be hardcoded = 1 (this will tell the user
c  the the above minimal info is to be printed to the screen). If
c  for some reason the full info is needed again, I can just change
c  ISUPRES = 0 in the code.

c  8. The user will be told before the run begins that he can 
c  execute the batch file, CHMAXCYC.BAT (by typing CHMAXCYC at 
c  a DOS prompt), in the working directory of a PC and that will
c  cause the program to stop safely at the end of whatever cycle it 
c  is on, as if the maximum no. of cycles has been reached.


c  This batch file will simply copy to the file CHMAXCYC.OLD the file

c  CHMAXCYC.NEW. Note that CHMAXCYC.BAT, CHMAXCYC.OLD and CHMAXCYC.NEW 
c  are made by this program before cycle calculations begin. 
c  CHMAXCYC.OLD has a 1 on the first line and CHMAXCYC.NEW has a 0 on 
c  the first line. The program will open and read CHMAXCYC.OLD at the 
c  beginning of each new cycle. If it reads a 1, it will continue the 
c  calculations as before. If it reads a 0, it will change the value of 
c  MAXCYC to whatever ICYCLE is currently, which will cause the program 

c  to halt at the end of that cycle just as if MAXCYC had been set
c  to the changed value originally. If this happens, the program will
c  write a comment to the console and file 25 of why the program

c  has stopped prematurely.

c  Note that the user may want to exercise this option if he wants to
c  see the results from a slowly converging run, and then, because
c  the density file from the last cycle will have been correctly 
c  created, still be able to start another run using the final cycle 
c  joint density from the halted run (with different parameters if 
c  desired).

c  Note that because of the messages to the user, accompanied by
c  PAUSE commands, this program must be run interactively at least
c  at the start. I.e., if this program is to be run without user
c  oversight, I will have to take out the PAUSE commands, and of 
c  course then the option described here will be moot (i.e., without
c  user interaction, the user would not know that the program is
c  converging slowly).


c  9. ADDITIONAL INFO WILL BE WRITTEN INTO FILE 31 (PRTBxxxx). 
C  PREVIOUSLY, FOR EACH SUBJECT THE PREDICTED VALUES (BASED ON EITHER 
C  THE MEANS, MEDIANS, OR MODES FROM THAT SUBJECT'S BAYESIAN POSTERIOR 
C  DISTRIBUTION) WERE CALCULATED AND WRITTEN FOR THE TIMES IN TPRED. 
C  NOW, IN ADDITION, FOR EACH SUBJECT AND EACH OUTPUT EQUATION, THE 
C  OBSERVATION TIMES AND OBSERVED VALUES (FROM EACH SUBJECT'S PATIENT 
C  FILE), AND PREDICTED VALUES (BASED ON THE MEANS, MEDIANS, OR MODES 
C  FROM THAT SUBJECT'S BAYESIAN POSTERIOR DISTRIBUTION) WILL BE WRITTEN.

c  10. New format 5456 has been added to clarify the results in the
c  output file.

c  11. Every STOP statement, except the one that terminates the program 
c  after a complete run, is now preceeded by a PAUSE statement. This is
c  done so that when this program is run unders windows, the window 

c  will not disappear immediately with no explanation. i.e., with the
c  PAUSE statements, the explanation for the stopping of the program

c  will remain on the screen until the user presses the enter key.

c  12. The limitation of 250000 for maxactem in Subroutine emint is 
c  changed to be 10000000.

C  13. THE CODE AT THE END OF MAIN (TO REMOVE THE fort.27 FILE, AND
C  TO WRITE THE TIME OF THE RUN INTO FILE 91) ARE COMMENTED OUT
C  SINCE THEY ARE NO LONGER APPLICABLE.

C  14. The combined output file is made using READLARG (and format 2717) 
c  instead of using READLINE (and format 1717). READLARG is a 
c  character 300 variable, where READLINE is a character 78 variable. 
c  The extra characters ensures that no lines will be cut off.

C  15. At the end of each cycle, the program now
c  writes the 1st part of the density file into DENFIL (i.e., it 
c  overwrites this info from the previous cycle). This will be 
c  useful if the program crashes after a long run ... since then the 
c  user can simply run the PC prep program and restart the run using 
c  the latest density file as the aprior density (i.e., the engine will
c  pick up where it crashed). big1 only made the density file (and

c  the combined output file) at the end of the run.

c  Note that the density file created at the end of each cycle is
c  not the full density file created at the end of the run. It only
c  includes the info down to CORDEN, which is all that the PC prep
c  program needs for its apriori density.

c  16. It checks MAXCYC to see if it is 0. If so, it means that the user
c  wants to bypass the usual NPAG analysis, and instead calculate the
c  output files based on the input density (which becomes the "final
c  cycle joint density") and the patient data files. This option is
c  used to get the Bayesian Posterior Joint Densities (and predicted
c  values, etc.) for a set of subjects, based on a joint density from
c  a previous Big NPAG run.


c  Note that Subroutine SUBRES (and its argument list) is changed to
c  accomadate the calculation of PYJGX in case MAXCYC = 0 (see 
c  explanation before the call to SUBRES).


c  17. The no. of random variables is now .LE. 25 rather than .LE. 20. 
c  Also, the no. of fixed parameters is now .LE. 7 rather than .LE. 12.

c  18. In the DO 800 loop, the % increment reported during the cycle 
c  calculations for each subject is changed to every 1% rather than

c  every .1%.

C  19. NOTE THAT THE PATIENT DATA FILES WILL BE CONCATENATED AS USUAL
C  AT THE END OF npembig33.inp, REGARDLESS OF WHETHER IFORMT = 1 OR 3.
C  I.E., AFTER READING IN IFORMT, THE PROGRAM WILL READ IN PREFIX AND 
C  EXT. I.E., THERE WILL BE NO PRFIX2/EXT2, ETC.

c-----------------------------------------------------------------------

c  bignpaglap4.f						8-31-03


c  bignpaglap4.f has the following changes from bignpaglap3.f:

c  1. In the output file, NGRID is written using format 9869 so 
c  SUBROUTINE PREVRUN in the PC PREP PROGRAM will easily be able to 
c  read in NGRID. Previously, NGRID was established from the reading 
c  in of INDPTS in the density file, but this could cause a problem 
c  if NGRID was reset because it was larger than MAXACT, since INDPTS 
c  is not reset (see comments in NPBIG14.FOR).

c  2. Note that formats 1657 and 7124 are changed to write out this
c  program "bignpaglap4.

c  3. VERSION 36 in FORMAT 1657 is changed to VERSION 37, because

c  SUBROUTINE PREVRUN in the PC PREP PROGRAM will only read NGRID
c  from the output file (see format 9869) from this version on.

c  4. Subroutine STAZ has a bug fix. Previously, grid points which 
c  landed exactly on the upper boundary value didn't get counted.
c  Now, they will be (see code).

c  5. A bug is fixed which could occur when the program concatenates 
c  the density file onto the combined output file. Line are truncated to 

c  72 characters, and so when the the following line is copied:
c  198.532739532312       0.314723681391700       0.405442300013591D-001
c  it will be written as 
c  198.532739532312       0.314723681391700       0.405442300013591D-
c  and then when the PC preparation program (currently NPBIG14.EXE)
c  reads this line, it will bomb since it expects 3 nos. in this line,
c  but the last no. is not read as a number (because of the "D-").

c  The fix is to read and write 78 characters rather than just 72 
c  characters. So, format 1717 will be changed to A78, and READLINE
C  will be changed to CHARACTER*78. Also, in Subroutine Filred, 
c  the same change will be made since it doesn't hurt, and this
c  keeps the code consistent throughout.

c-----------------------------------------------------------------------


c  bignpaglap3.f						8-29-02

c  bignpaglap3 has the following changes from bignpaglap2:


c  1. Instead of hardcoding ierrmod = 1, ierrmod (and gamlam0) will
c  be read in from npembig3.inp (changed from npembig2.inp). There is
c  corresponding new code to assign the inital value of gamma or flat 
c  from gamlam0.

c  2. For each cycle, ierrmod and gamlam are now written into the
c  output file. The code "bignpaglap2" is changed to bignpaglap3 in
c  both the output and density file. Also, the version code in the
c  output file is changed from VER_BAK DEC_01 to VER_BAK AUG_02 
c  (because of the additional information, ierrmod and gamlam, which
c  is written to the file).


c  3. New output formats 2112, ..., 2117 are put into the output file
c  to remind the user what the assay std. dev. model is in the run.

c  4. New code involving LASTCYC prevents the cycle no. from being
c  written 3 times for each cycle if ierrmod .GE. 2 (since the code

c  to estimate gamma/lambda if ierrmod .GE. 2 requires the loop which 
c  includes writing the cycle no. to be executed 3 times for each
c  cycle).

c-----------------------------------------------------------------------

c  bignpaglap2.f						7-24-02

c  bignpaglap2 is the multiple drug version of bignpaglap1 (which only

c  allows one drug). It essentially makes the same changes to 

c  bignpaglap1 (in this regard) that npbig8adapt.f made to 
c  npbig7adapt.f. 

c  Note that just as bignpaglap1.f is at the same "level" as 
c  npbig7aadapt.f [except that the former has Bob Leary's updated 
c  efficiency improvements, and multiplicative GAMMA and additive LAMBDA 
c  options (which are turned off for now)], bignpaglap2.f will be at the
c  same "level" as npbig8adapt.f [with same exceptions as indicated
c  above].


c  The changes are as follows:



c  1. bignpaglap2 allows multiple drugs. Coding changes are required
c  in Subroutine FILRED to read in multiple drug info. Note that each 
c  drug will have one column for IV values and one column for bolus 
c  values. In addition, there are other changes to the formatting 
c  (see 2DRUG001 for an example of a typical new working copy patient 
c  data file). Note that the only dimension changes in Subroutine FILRED
c  are: BS(500,3) is changed to BS(500,7), and NTLAG is now a vector 
c  instead of a scalar (it has dimension 7).

c  This file is now compiled with blasnpag.f, idfix5g.f (updated from 
c  idfix5f.f), idcy_53g.f (updated from idcy_53f.f), idcy_63g.f (updated 
c  from idcy_63f.f), and shift3.f (updated from shift2.f).

c  2. In the regular OUTPUT file and the density file, 
c  "MADE BY bignpaglap1" is replaced by "MADE BY bignpaglap2".

c  3. SUBROUTINE BIGNPEM is replaced by SUBROUTINE BIGNPAG to emphasize 


c  that the new code for multiple inputs is only applicable for the
c  BIG NPAG program (note that the first PC preparation program used 
c  with this program was NPBIG11.FOR).

c-----------------------------------------------------------------------


c  bignpaglap1.f						7-19-02

c  Note that bignpaglap1.f is at the same "level" as npbig7aadapt.f,
c  but it has Bob Leary's updated efficiency improvements, and 
c  multiplicative GAMMA and additive LAMBDA options (which are 
c  turned off for now).

c  bignpaglap1.f has the following changes from bignpaglap.f
c  (bignpag.f):

c  1. All information needed by the Big PC Prep Program (currently 
c  NPBIG10B.FOR) will now be put into one combined output file. That is,
c  this program will essentially concatenate 4 files which were 
c  previously kept separate, OUTFIL, DENFIL, npembig2.inp (actually 
c  just the patient data portion of npembig2.inp), and npemdriv.f. This 
c  will enable the user to run the PC Prep Program with just this one
c  combined output file (i.e., even the working copy patient data files
c  will no longer be needed).


c  THE name for the OUTPUT FILE is now changed to 'OUTT'//NAME SINCE 
c  'OUT//NAME will be reserved for the combined OUTPUT FILE formed at 
c  the end of the run.

c  2. In the regular OUTPUT file, on line 1, the version no. is changed 
c  from 35 to 36, and "MADE BY bignpaglap1" is added to the 1st lines 
c  in the output and density files. See formats 1657 (changed) and
c  7124 (new). Also, the 2nd line in the output file is changed to 
c  VER_BAK DEC_01.

c  3. The convergence index (HOWCLOSE), and how close the current 
c  density is to the M.L.E. of the density (MAX(DXI) - NSUB), are
c  removed. DXI is left in only as an argument to BIGNPEM, and DORIG is 
c  left in only to be read and written to the density file so that the 
c  format of that file does not have to be changed.

c  4. This module will no longer contain the modules idfix5*.f, 


c  idcy_53*.f, idcy_63*.f, and shift2.f. These modules will be
c  compiled and linked separately.


c  5. The I/O to input ierrmod will be suppressed since there can be
c  no user interaction with this program. For now, ierrmod will be
c  hardcoded = 1.

c  6. Note that the PARAMETER statement in Subroutine emint has the
c  following line to set the values for MAXACTem and MAXSUBem:
c      parameter (MAXSUBem=999,MAXACTem=250000)
c  These values can be reset as needed.


c-----------------------------------------------------------------------

c bignpag.f                                                     07-05-2002
C BIGNPAG with multiplicative GAMMA and additive LAMBDA options
c this is the bignpag version with time lags and initial conditions
c This version is consistent with BIGNPEM version 5 (see  npbig5.f below)
c Changes in July 2002 involve replacing the LINPACK-based linear equation solver
c in subnroutine emint with the Cholesky sovler from LAPACK.
c changes in July 01 include new, more efficient Hessian construcution routine
c This version is dimensioned for a MAXSUBem (maximum number of subjects)
C of 400 - this is easily changed by changing the MAXSUBem PARAMETER
C statement in subroutine EMINT.  Also, the max number of grid points is
c 250,000 - this is also adjustable by the PARAMETER statement for
C MAXACTem in emint.


C July 05 2002 changes
c repalced calls to Linpacksymmetric indefinite linear equation solver
c dsifa and dsisl with lapack cholesky solver DPOTRF and DPOTRS
c begin 01/02/02 changes

c saved density results in denstor(*,4)  for best of base, plus,

c and minus case, so proper statistics would be generated based
c on the best case.
c begin 12/31/01 changes
c renamed iteration logfile itlog to ILOGxxxx, where xxxx is current run
c number from 'extnum' file.
c end 12/31/01 changes
c begin 12/16/01 changes
c added error model selection capability - currently four choices:
c 1 - use error polynomial as given in input file,
c 2 - optimally scale error polynomial by multiplicative factor gamma

c 3 - optimally find an additive error lambda, such that error^2 =

c     lambda^2 + polynomial^2
c 4 - find optimal flat weighting - this is equivalent to using a
c     polynomial with only a constatn term, and finding the optimal
c     value of that constant with option 1)
c end 12/16/01 changes
c begin 7/04/01 changes
c ROUTINE STAT change to STAZ to avoid conflict with system routine
c Also, first '0.0' agrument changed in CALL OUTPUT(0.0,...' to 0.0D0
c to agree with argument typing
c end 7/04/01 changes
c  NPBIGADAPT
c  4/06/2000 - April 6, 2000 - New adaptive grid version
 
c  This version is built on top of the previous fixed
c  grid version of BIGNPEM and represents a major algorithmic
c  change. NPGIGADAPT is the adaptive grid version of BIGNPEM -
c  It is designed to work with exactly the same driver file
c  npemdriv.f and input file npembig2.inp as previous versions
c  of BIGNPEM.  Results in terms of the log likelihood of the

c  final density produced are typically much better than
c  previous versions, which used a fixed grid, even when the

c  adaptive version is started from a small grid and the previous
c  fixed grid version is started from a very large grid.

c  Thus the NPBIGADAPT version can be run on a workstation or PC
c  in moderate amounts of time and with relatively low
c  memory requirements and still produce results equivalent to or
c  better than a large fixed grid version run for many processor
c  hours on a parallel supercomputer.
 
 
c  This version works on the following logic
 
c  Step 0 (intialization) ;
c  The data file npembig2.inp is read and the initial grid of
c  size NGRID (as specified by the INDPTS variable in the
c  iput file) is generated using a low-discrepancy (or
c  'quasirandom') Faure generator.
c  Note that we require that all points fit in memory, so if
c  NGRID is larger than the memory limit MAXACT in the driver
c  program npemdriv.f, we reset NGRID to
c  MAXACT.  Also, the old termination criteria JSTOP and
c  TOL in npembig2.inp are now still read but ignored - they
c  are replaced with hardwired criteria (see below).  However,
c  the MAXCYCLE criterion is retained but changes its meaning.
c  It now represents a limit on the number of grid condensation -

c  expansion cycles, not the limit on the number of cycles in
c  the EM algorithm (the EM algorithm is no longer used, having
c  been replaced by the more efficient interior point algorithm
c  developed by Jim Burke at University of Washington).

 
c  Step 1; (solve ODEs on current grid)

c  solve all the ordinary differential equations defining the
c  PK model on the current set of grid points and compute
c  the corresponding likeihoods for each combination of

c  subject and grid point to produce the likelihood matrix
c  PYJGX(J,IG)  (J=subject index, IG = grid point index)
 
c  Step 2 (grid condensation)
 
c  Solve the maximum likelihood EM problem  defined by the likelihood
c  matrix PYJGX corresponding to the current grid
c  via the interior point method implemented in subroutine emint.
c  On the first cycle this condenses the number of active gridpoints
c  NACTVE from the  starting value of NGRID to approximately
c  NSUB, the number of subjects.  On subsequent cycles, this step

c  will condense the current set of NACTVE (usually now much
c  smaller than NGRID but still several times larger than NSUB)
c  points to NSUB points.

 
c  Step 3 (grid expansion).
c  Each of the NSUB active grid points is perturbed in each
c  parameter with a +EPS and -EPS percentage perturbation while
c  holding the other parameters fixed.  Thus if there
c  are NPARAM parameters, there are 2*NPARAM perturbed points.
c  These checked to see if they lie within the original parameter
c  bounds specified in npembig2.inp.  Those perturbed grid points
c  that lie within the bound are added to the NSUB active grid points
c  to form a new set of approximately NACTVE = NSUB*(1+2*NPARAM).

c  Note that in general this is much smaller than the original set of
c  NGRID points.  EPS is initially set to 20% of the range of
c  the corresponding parameter.  As the algorithm proceeds, the
c  grid resolution will be lowered in stages to 0.01% of the
c  range.
 
c  Step 4 (termination check)
c  If the LOG-LIKELIHOOD of the current grid is at least a
c  tolerance TOL (hardwired in the current verison to 0.001), we
c  maintain EPS at the current value and
c  continue.  If the LOG-LIKELIHOOD does not improve by at least TOL,
c  we cut EPS in half, generate a new grid, and continue.

c  The algorithm terminates when either
c  a) the designated maximum number of cycles MAXCYC in npembig2.inp
c     is reached, or
c  b) EPS reaches the (currently hardwired) limit of 0.01% AND the
c     improvement of the current cycle over the previous cycle

c     in log-likelihood is less that the (currently hardwired)
c     tolerance of 0.001
c

c
c-----------comments below refer to previous fixed grid versions ----
c  npbig5.f							2-17-00
 
c  npbig5 is the same as npbig4, except:
 
c  FORMAT 88 IN MAIN AND PRNTOP IS CHANGED TO INDICATE THAT A
C  CONVERGENCE INDEX OF .LE. 1 = CONVERGENCE (NOT JUST AN INDEX = 1).
 
C-----------------------------------------------------------------------
 
c  npbig4.f							1-23-00
 
c  npbig4 is exactly the same as npbig3. The only change is that it is
c  linked with new modules on the supercomputer. idfix5d.f is replaced
c  by idfix5e.f; idcy_53d.f is replaced by idcy_53e.f; and idcy_63d.f is
c  replaced by idcy_63e.f. These 3 new modules have updated code to
c  allow initial conditions of the amounts in the compartments to be set
c  = paramater values, rather than always fixed = 0.0.
 
c  SUBROUTINE SYMBOL in the Fortran model file (see, e.g.,

c  INITCOND.FOR) now contains an additional COMMON/INITCOND/IC, and the
c  user sets IC(I) = J for each compartment, I, which will have its
c  initial amount set = value of parameter J. This info is passed to
c  the above 3 id modules. Also, the dimension of P in OUTPUT and
c  DIFFEQ is changed to max_ODE_params.
 
c  Note that the 3 new id modules above have a lot of code
c  simplification (see notes in their code). In particular, the
c  square root transformations are no longer done --> the results
c  for this program will differ possibly a little from previous
c  results.
 
c-----------------------------------------------------------------------
 
c  npbig3.f							1-19-00
 
c  npbig3 is exactly the same as npbig2. The only change is that it is
c  linked with new modules on the supercomputer. idfix5c.f is replaced


c  by idfix5d.f; idcy_53c.f is replaced by idcy_53d.f; and idcy_63c.f is
c  replaced by idcy_63d.f. These 3 new modules have updated code to


c  correctly allow the IDIFF = 0 option, which bypasses the calling
c  of USERANAL (and its calls to DIFFEQ). Instead the value(s) for
c  the output(s) will be coded explicitly into SUBROUTINE OUTPUT. Also,

c  SUBROUTINE OUTPUT has an additional argument, the time at which the
c  output value(s) is(are) desired.
 
c-----------------------------------------------------------------------

 
c  npbig2.f							11-8-99
 
c  npbig2 is the same as npbig1 except for a change in COMMON/CNST
c  in subroutine FILRED. NTLAG is now added.

 
c  This change, and others, are required for all modules in this
c  program, which allow time lags.
 
c-----------------------------------------------------------------------
 
c  npbig1.f							10-12-99
 
c  npbig1 has the following changes from m2_19aca.f:
 
C  1. THE GRID POINT SELECTION PROCEDURE IS CHANGED. SUBROUTINE GETCOF
C  IS REMOVED, AND SUBROUTINE CALGRD IS REPLACED BY A NEW CALGRD, ALONG
C  WITH ROUTINES INFAUR AND GOFAUR. NOTE THAT ALL REFERENCES TO THE
C  NUMBER THEORETIC INTEGRATION SCHEME ARE REMOVED.
 
C  2. THE CONVERGENCE CRITERION IS CHANGED TO HAVE AN ABSOLUTE VALUE
C  SIGN AROUND (SLPYJ-PRESLP). THIS SHOULDN'T BE NECESSARY SINCE THE
C  LOG-LIK IS MONOTONICALLY INCREASING, IN THEORY. THE ABS. VALUE
C  MEANS THAT IF A STRANGE NUMERICAL "GLITCH" OCCURS WHICH RESULTS
C  IN THE LOG-LIK DECREASING FROM 1 CYCLE TO THE NEXT, THE PROGRAM WON'T
C  AUTOMATICALLY HAVE SATISFIED THE CONVERGEGENCE CRITERION. OF COURSE,
C  IT'S POSSIBLE THAT IN SUCH A CASE, WE WOULD WANT THE PROGRAM TO STOP,
C  REGARDLESS.
 
C  3. SUBROUTINE STAT NO LONGER ALWAYS USES NINT = 100. INSTEAD, NINT
C  WILL BE THE MAX(100,2*NSUB). THE REASON IS THAT THE MARGINAL SCALED
C  INFO HAS A DENOMINATOR OF LN(NINT/NSUB) IN THE CALCULATION, WHICH
C  RESULTS IN A DIVIDE BY 0 ERROR IF NSUB = 100 (AND RESULTS IN
C  NEGATIVE SCALED INFO (WHICH IS SUPPOSED TO BE BETWEEN 0 AND 100),

C  WHEN NSUB > 100. BY MAKING NINT .GE. 2*NSUB, THIS PROBLEM WILL
C  DISAPPEAR.
 
C  4. AN ADDITIONAL STATISTIC IS OUTPUT EACH CYCLE, HOWCLOSE. IT
C  IS A MEASURE OF HOW CLOSE TO CONVERGENCE THE PROGRAM IS. THIS WILL BE
C  IN FORMAT 88, JUST BELOW THE FORMAT 8'S IN MAIN AND PRNTOP. HOWCLOSE
C  WILL BE ADDED AS AN ARGUMENT TO PRNTOP.
 

C  5. A CHANGE IS MADE TO FORMAT 1652 (IN MAIN AND SUBRES), AND THE
C  VALUES SK AND KU ARE SET TO WHEN ICOVL0 = 1 (WHICH OCCURS WHEN AT
C  LEAST ONE PARAMETER HAS VARIANCE NUMERICALLY .LE. 0). NOW KU AND SK

C  WILL BE WRITTEN OUT AS -99999999, RATHER THAN 1.D30. IT USED TO BE
C  THAT 1.D30 WOULD PRINT OUT AS *'S, BUT NOW WITH THE G FORMAT IT
C  PRINTS OUT AS A LEGITIMATE NUMBER, AND -99999999 IS A BETTER WAY TO
C  EXPRESS THAT THESE VALUES DO NOT EXIST.
 
c-----------------------------------------------------------------------
 
c  m2_19aca.f							5-23-99
 
c  m2_19aca has all the changes that m2_20cal thru m2_20bca made to
c  m2_19cal, except that the assay s.d.'s are still functions of

c  observed values, rather than predicted values.

 
c  The changes are:
 
 
c  1. RS(500,14) DIMENSIONS ARE CHANGED TO RS(500,34), TO ALLOW UP TO
c  30 USER-SUPPLIED COVARIATES.
 
C  2. A BUG IN THE CALCULATION OF ENTROPY IS CORRECTED. THE ENTROPY
C  ENTROPY CALCULATION IS MOVED TO BE AFTER SUM=SUM/DL2 STATEMENT.

 
C  3. FORMAT 6543 IN PRNTOP IS CHANGED TO BE SAME AS IN MAIN.
 
c-----------------------------------------------------------------------
 
 
c  m2_19cal.f							7-28-98
 
c  m2_19cal.f has the following changes from m2_18cal.f:
 

c  It corrects a "bug" in m2_18cal.f. m2_18cal.f never allowed for
c  an observed value to be missing. With multiple outputs, not all
c  output equations will necessarily have observed levels at all
c  observation times. An observed level which is "missing" has the
c  value -99 in its entry. m2_18cal.f just treated the -99 as a
c  regular value.
 
c  The only functional changes involve MISVAL in and after loop 140.
 
c  The only other module which needs to be changed is idfixed4.f -->
c  idfixed5.f (idcy_53.f and idcy_63.f just calculate predicted values
c  at all the observation times. This can be done for all output eqs.
c  for all the observation times, regardless of which observed values
c  are missing).
 
c-----------------------------------------------------------------------
 
c  m2_18cal.f							6-28-98
 
c  m2_18cal.f has the following changes from m2_17cal.f:
 
C  1. IT ALLOWS MULTIPLE OUTPUTS. THERE WILL BE NUMEQT OUTPUT EQUATIONS.

C  NUMEQT IS PASSED TO THIS ROUTINE (BIGNPEM) BY npemdriv.f (MADE BY
C  M2_18.FOR) IN THE ARGUMENT LIST. SEVERAL ARRAYS HAVE AN EXTRA
C  DIMENSION TO IDENTIFY THE OUTPUT EQUATION, INCLUDING YPREDPOP AND
C  YPREDBAY WHICH ARE ARGUMENTS FROM npemdriv.f.
C  YPREDBAY WHICH ARE ARGUMENTS FROM npemdriv.f.

 
C  THE INPUT FILE HAS BEEN RENAME npembig2.inp SINCE NEW INFORMATION
C  IS INCLUDED.

 
C  SUBROUTINE FILRED HAS CHANGES TO ACCOMODATE MULTIPLE OUTPUTS.
 
C  THIS MODULE WILL BE LINKED WITH OTHER CHANGED MODULES, idfixed4.f,
C  idcy_53.f, AND idcy_63.f, ALONG WITH vodtot.f (WHICH IS UNCHANGED).
 

C  2. SOME COMMENTS REGARDING WHAT'S IN THE INPUT FILE, npembig2.inp,
C  HAVE BEEN IMPROVED OR CORRECTED.
 
C  3. POPULATION VALUES FOR THE NUMEQT SETS OF C'S NOW WRITTEN TO FILE
C  25 USING FORMAT 162, INSTEAD OF 161.
 
C  4. INFIL NO LONGER READ FROM INPUT FILE npembig2.inp (IT WAS NEVER
C  USED).
 
C  5. CYCLE NO. IS ALSO WRITTEN OUT IN FORMAT 8888.
 
C-----------------------------------------------------------------------

 
c  m2_17cal.f							5-1-98
 
c  m2_17cal.f has the following changes from m2_16cal.f:

 

C  1. Input file fil01.inp IS renamed npembig1.inp. There are no
c  changes to this file, but now the name is more suggestive of what
c  it is.
 
C  2. FORMAT 161 REPLACES * FORMAT WHEN WRITING OUT THE C'S TO A FILE,
C  TO AVOID POSSIBILITY THAT PART OF LINE WRITTEN WILL BE TRUNCATED IF
C  IT EXTENDS PAST COLUMN 72.

 
c-----------------------------------------------------------------------
 
c  m2_16cal.f						2-12-98

 
c  m2_16cal.f has the following changes from m2_15cal.f:
 
c  1. m2_16cal.f has the same format changes from m2_15cal.f M2_16.FOR
c  has from M2_15.FOR, namely:
 
C  ALL F AND E FORMATS WILL BE CHANGED TO G FORMATS. SINCE THE G

C  FORMAT REQUIRES AT LEAST 6 SPACES IN ADDITION TO THE DECIMAL SPACES +
C  THE SPACE FOR THE DECIMAL POINT ITSELF (TO REPRESENT ALL POSSIBLE
C  NUMBERS, INCLUDING THE NEGATIVE SIGN IF THERE IS A NEGATIVE NO.),
C  THE FORMAT WILL BE Gw.d, WHERE w MUST BE AT LEAST 6 BIGGER THAN d.
 
C  THEREFORE, THE FOLLOWING LOGIC WILL BE USED TO CONVERT THE FORMATS.
C  GIVEN AN F OR E FORMAT w.d, THE REPLACING G FORMAT WILL BE ww.d,
C  WHERE ww = MAX(w,d+6). IF ANY ACCOMPANYING FORMATS ARE DEPENDENT ON
C  THE REPLACED FORMAT, IT WILL BE CHANGED ACCORDINGLY. FOR EXAMPLE,
C  FORMAT 5104 IS USED TO PLACE THE PARAMETER NAMES ABOVE MEANS,
C  MEDIANS, ETC. USING FORMAT 5103. PREVIOUSLY, THE FORMATS WERE:
 
C  5104   FORMAT(5X,20(A11,5X))
C  5103   FORMAT(1X,20(F11.6,5X))
 
C  IN THIS PROGRAM, SINCE 11.6 MUST BE CHANGED TO 12.6 FOR THE G FORMAT
C  (I.E., ww = MAX(11,6+6) = 12), AND SINCE 5X HAS BEEN CHANGED TO 1X,
C  FORMAT 5104 MUST BE CHANGED TO BE COMPATIBLE.
 
C  5104   FORMAT(5X,20(A11,2X))
C  5103   FORMAT(1X,20(G12.6,1X))
 
C  A COUPLE OF OTHER EXCEPTIONS FROM THE ABOVE RULE HAVE BEEN MADE FOR
C  COSMETIC REASONS, INCLUDING PRINTING OUT % VALUES (AND FORMATS
C  2049, 2051, 2052) IN MAIN.
 
 
c  2. The order of the arguments in COMMON/DESCR has been changed. For
c  some reason, the fortran compiler likes to see Real*8 variables
c  preceed integer variables. Otherwise, warning messages appear.
 
c-----------------------------------------------------------------------
 
c  m2_15cal.f							1-22-98


 
c  m2_15cal has the following changes from m2_14cal:

 
c  1. AGE, ISEX, HEIGHT, and IETHFLG are input in subroutine FILRED,

c  for each subject, and these values are now passed to subroutines
c  DIFFEQ and OUTPUT, part of the fortran file created by the
c  boxes-type program (or made manually). This fortran file is part
c  of npemdriv.f, uploaded by the user after running M2_15.EXE. These
c  values are passed via COMMON/DESCR.
c  The new boxes program is BOXNEW2.PAS, changed from BOXESNEW.PAS.
 
c  2. In subroutine FILRED, since RS must store all the psuedo "rates"
c  (i.e., all the covariate info input in the dosage regimen), RS

c  dimensions have been increased from (500,8) to (500,14). Since
c  RS is passed via COMMON/OBSER, modules idfixed3.f (changed from
c  idfixed2.f), idcy_52.f (changed from idcy_51.f), and idcy_62.f
c  (changed from idcy_61.f) must be changed correspondingly. Also,
c  these modules have changes related to a "bug" correction regarding
c  IDIFF.
 
c-----------------------------------------------------------------------
 
c  m2_14cal.f						   	11-6-97
 
c  m2_14cal has the following changes from m2_13cal.
 
c  1. This module is now SUBROUTINE BIGNPEM, called by npemdriv.f, which
c  is created by the PC preparation program, M2_14.FOR. This change was
c  made so a dynamic allocation of dimensions can be made, based on
c  how many subjects, random parameters, and grid points the user
c  has selected.
 
c  npemdriv.f, the "MAIN" module, HAS A PARAMETER STATEMENT WHICH

c  DEFINES THE PARAMETERS WHICH ESTABLISH THE DIMENSIONS IN THE VARIABLY
c  DIMENSIONED ARRAYS. IT THEN HAS THE STATEMENT:
C  CALL BIGNPEM( ...) , WHERE ALL VARIABLY DIMENSIONED ARRAYS (AND THE
C  VARIABLE PARAMETER DIMENSIONS) ARE PASSED IN THE ARGUMENT LIST.
 
C  NOTE THAT ARRAYS PYJGX, EXX, YPREDPOP, AND YPREDBAY NOW HAVE MAXSUB
C  AS A VARIABLE DIMENSION. ALSO, PYJGX'S 2ND DIMENSION, AND PYJGXX'S
C  SINGLE DIMENSION IN SUBROUTINE SUBRES HAVE BEEN CHANGED FROM MAXGRD
C  TO MAXACT (WHICH THEY WERE SUPPOSED TO HAVE BEEN ANYWAY).

 
c  2. Some unneeded format statements are eliminated.
 
c  3. ATOL(3) TO ATOL(max_ODE_comps), WHICH IT SHOULD HAVE BEEN ALL ALONG.
 
c  4. THE VALUES FOR RTOL AND ATOL() ARE PRINTED TO THE OUTPUT FILE.
 
c  5. THE ORIGINAL NO. OF GRID POINTS, IN ADDITION TO THE CURRENTLY
C  ACTIVE NO. OF GRID POINTS, IS PRINTED OUT EACH CYCLE (INCLUDING
C  PARTIAL CYCLES PRINTED BY SUBROUTINE PRNTOP, AND THE MAP-BAYESIAN
C  CYCLE FOR EACH SUBJECT IN SUBROUTINE SUBRES).
 
c  6. Format 2314, if applicable, is written to the output file, in
c  addition to the screen.
 
c-----------------------------------------------------------------------
 
c  m2_13cal.f						   	8-31-97
 

c  m2_13cal is the same as m2_12cal except that the maximum no. of grid
c  points, MAXGRD, is increased from 16*80021 to 100*80021, the maximum

c  no. of subjects is increased  to 200, from 100, and the maximum no.
c  of dimensions is increased to 10 from 8.
 
c  Note that there is a limit to how big the program can be in more
c  than one way. In particular, the maximum number of double precision
c  entries in a matrix = 268,435,455. Since CORDEN and CORHOLD have
c  MAXGRD*(MAXDIM+1) entries, if MAXDIM = 20, since MAXGRD = MAX*80021
c  this --> MAX*80021*21 .LE. 268,435,455 --> MAX .LE. 159.74... But,
c  even though MAXGRD = 159*80021 will compile OK, the program is
c  killed or terminated ('segmentation fault') by the sun system when
c  execution is attempted. For now, the combination of parameter values
c  which is executable is MAXGRD = 100*80021, MAXACT = 100000 (see
c  below), MAXDIM = 10, and PYJGX dimensions set = (200,MAXGRD) in MAIN
c  and subroutine SUBRES.
 
c  This can be done by using a creating a new parameter, MAXACT = the
c  maximum no. of grid points which can be stored into PYJGX. MAXACT
c  will be set arbitrarily to 100000, so PYJGX has dimensions of
c  just (200,10000).
c  Then, in loop 800, when PYJGX is filled with values, at most 100000
c  grid points can be put into PYJGX. The rest, if there others, will
c  simply have to have their P(YJ|X)'s recalculated each cycle until
c  the no. of active grid points .LE. MAXACT, at which time all the
c  P(YJ|X)'s can be stored into PYJGX.
 
c  Note that, with the exception of PYJGXX the other matrices which had


c  MAXGRD in their dimensions (i.e., WORK, SPXGYJ, DXI, CORDEN, CORHOLD,
c  and DENSTOR) will still have dimensions based on MAXGRD. Also, a new
c  matrix, WORKK(MAXGRD) will be added.

 
c  The most significant changes in the code are in loop 800.
 


c-----------------------------------------------------------------------
 

c  m2_12cal.f						    8-27-97
 
c  m2_12cal is the same as m2_11cal, except that the no. of subjects
c  allowed is reduced from 999 to 100, the no. of dimensions allowed
c  is reduced from 20 to 8, and the no. of grid points allowed is
c  increased from 2*80021 to 16*80021.

 
c  In particular, PYJGX(999,MAXGRD) IS CHANGED TO PYJGX(100,MAXGRD) in
c  main and subroutine subres. Also, some other 999's in dimensions are
c  changed to 100. Also, MAXDIM is changed from 20 to 8 in the
c  PARAMETER statement, but no dimensions are changed from 20 to 8.
c  Finally, MAXGRD is changed from 2*80021 to 16*80021 in the
c  PARAMETER statement.

 
c-----------------------------------------------------------------------

 
c  m2_11cal.f						    8-18-97
 
C  m2_11cal.f HAS THE FOLLOWING CHANGES FROM m2_10cal.f
 
C  1. THE NO. OF GRID POINTS MAY NOW BE > 80021. IN FACT, THE USER
C  MAY SELECT UP TO 3 (FOR NOW) MULTIPLES OF 80021 POINTS. EACH MULTIPLE
C  OF 80021 GRID POINTS WILL BE PUT INTO A 'SLICE' OF THE GRID SPACE,
C  DEFINED BY THE 1ST PARAMETER'S BOUNDARIES.
 
C  A RELATED CHANGE IS THAT PREVIOUSLY INDPTS HAD TO
C  BE INSIDE [1,6], WITH NGRID DEFINED AS BEFORE, BUT NOW IT CAN ALSO
C  BE [101,...], WHERE NGRID = 80021*(INDPTS-100).
 
C  2. PYJGX, EXX, YPREDPOP,
c  AND YPREDBAY all have a their dimension of 19 changed to 999. 999
c  was the desired dimension, but space limitations previously limited
c  it to be 19. Now, the almaak machine allows more subjects. In fact,
c  the current limitation is that the no. of entries in any matrix be
c  less than or equal to 2**28 - 1 = 268,435,455. So, PYJGX can be
c  dimensioned as high as (999,268704). FOR NOW, THEREFORE, WE WILL

C  SET MAXGRD = 3*80021 = 240063 < 268704. NO. IT WAS TOO BIG (WHEN I
C  TRIED TO RUN m2_11cal.exe, almaak responded with 'killed'), BUT
C  MAXGRD = 2*80021 WAS O.K.
 
C  3. DEFAULT CONVERGENCE TOLERANCE (IF JSTOP=1) IS CHANGED FROM .00001
C  TO .000001.
 
C  4. THE MAXIMUM NO. OF PARAMETERS IS INCREASED FROM 7 TO 20. THIS
C  AFFECTS MOSTLY THE DIMENSION STATEMENTS OF THE AFFECTED ROUTINES.
C  BUT ALSO SOME FORMATS ARE CHANGED.
 
C  NOTE THAT THIS CHANGE REQUIRES CORRESPONDING DIMENSION CHANGES IN
C  idfixed.f --> idfixed2.f, idcy_5.f --> idcy_51.f, and
c  idcy_6.f --> idcy_61.f.
 
C  5. AT THE END OF THE ANALYSIS, ONE LAST CONDENSING OF POINTS IS
C  DONE. THIS IS PARTICULARLY IMPORTANT IF THE PROGRAM 'ACCELERATED'
C  MANY TIMES WITH OUT CONDENSING POINTS NEAR THE END OF THE RUN
C  (I.E., THE PROGRAM CANNOT THROW OUT POINTS IF IT IS CONSTANTLY
C  'ACCELERATING' -- SEE REASON IN CODE).
 
c-----------------------------------------------------------------------
 
c  m2_10calc.f = m2_9calc.f					6-11-97
 
c  m2_9calc.f has the following change from m2_8calc.f.
 
c  The assay noise coefficients, written to fil01.inp by M2_9.FOR,
c  have been written on two lines, instead of one. This prevents the
c  possibility that C3, if written with a "D-xxx" format at the end
c  (e.g. D-003) will be too long for the line, and therefore not
c  properly read by this program.
 
c-----------------------------------------------------------------------

 
c  m2_8calc.f						    6-5-97
 
c  m2_8calc.f has the following changes from m2_7calc.f.
 
C 1.  NAME CHANGES:
 
C   user_4.f IS CHANGED TO mod01.f (this is for info only. user_4.f
C   is never used explicitly in this code).
 
C   m2__calc.inp IS CHANGED TO fil01.inp.
 

 
C  2. NEW INFO IS INPUT BY USER (AND IS INCLUDED IN fil01.inp).
 

C   IDELTA, T_END, MIC, AND MEAN/MEDIAN/MODE SELECTION. THESE VALUES
C   WILL BE USED TO CALCULATE PREDICTED VALUES FOR EACH
C   SUBJECT AT TIMES 0, IDELTA, 2*IDELTA, ..., T-END, ALONG WITH AUC'S
C   (AREAS UNDER CURVES) AND AUC/MIC'S. NOTE THAT IDELTA = 2, 4, 6, OR
C   12 MINUTES, AND T_END IS IN HOURS.
 
C   NOTE THAT THE NSUB TABLES OF PREDICTED VALUES WILL BE PUT INTO
C   THE FILE PRTBxxxx, WHERE xxxx IS THE SAME AS xxxx FOR THE OUTxxxx
C   AND DENxxxx OUTPUT FILES. THE AUC'S AND AUC/MIC'S WILL ACCOMPANY


C   EACH SUBJECT'S BAYESIAN POSTERIOR INFO (AT THE END OF THE RUN).
 
c       PRTBxxxx will be used as input to an effects model. i.e.,
c       knowing the concentration of a drug at certain times (as
c       predicted by a previous run's analysis) will be compared to
c       corresponding info on some 'effect', whose values will be known
c       at a given set of times ... so that the relationship between
c       concentration and the 'effect' can be established.
 

 
C  Details:
 
c 1. using subject 1's bayesian posterior density, use either the means,
c    medians, or modes (as selected by user);
 
c    calculate:
 
c    time    ypred
c ----------------------

c     0       ___
c    delta    ---
c  2*delta    ___
c	.

c	.
c	.
c   t-end     ___
 
c Notes: this ypred column of values would be exactly the same as
c       YPREDBAY for the means, medians or modes (see below), except
c       that the 'observed' times are as shown (and therefore don't
c	necessarily match the observed times in the patient's data
c	file).
c
c  Calculate total AUC = area under the observed value curve (horizontal
c	axis is time) = delta * (y1/2 + sum (ypred) = ylast/2),
c       approximately, where y1 = y(0), ylast = y(t_end), and sum is
c 	the sum of the y's for indices between 1 and last.
 
c   Also, calculate AUC for each 24 hour period (up to t-end).
 
c   Also, calculate AUC/MIC, total
c         and AUC/MIC for each 24 hour period.
 
 
c  2., ..., nsub = same as 1. above, except for subjects 2, ..., nsub.
 
c  Note that comp.sh (in the working directory of the supercomputer)
c  must be changed (m2_7calc. f-->m2_8calc.f and user_4 --> mod01). And
c  new module, idcy_6.f must be included (in addition to idcy_5.f). It
c  it contains IDCALCYY/FUNC3/EVAL3, which are similar to
c  idcy_5's IDCALCY/FUNC2/EVAL2, except concentrations are calculated
c  at above specified times, rather than at observation times for each
c  subject.
c  Note that m2_7calc.sh must be changed to m2_8calc.sh.
 
c-----------------------------------------------------------------------
 
c  m2_7calc.f							2-11-97
 
c  m2_7calc.f has the following changes from m2_6calc.f.
 
c  1. The density file now contains, at the end, PYJGX(JSUB,IG),
c     IG=1,NACTVE, JSUB=1,NSUB. It also includes YPREDPOP, YPREDBAY,
c     and EXX (see no. 4 below).
 
c  2. m2__calc.inp includes extra info, which previously had been
c     obtained via COMMON/TOMAIN (note that subroutine SYMBOL has
c     changed since now it's a part of user_4.f (created by a 'BOXES'-
c     type program), instead of iduser_4.f. m2__calc.inp also includes
c     IRAN, THE vector which tells which of the parameters are random
c     and which are fixed.
 

c  3. The call to IDPC no longer includes NVAR as an argument. The no.
c     of parameter values passed is NP = NVAR+NOFIX, and NP is obtained
c     by IDPC via COMMON/CNST from SUBROUTINE SYMBOL.
 
c  4. The write statements to files 24 and 37 have been removed from
c     subroutine PRNTOP (they should have been removed before, since
c     no files 24 and 37 are needed on the mainframe).
 

c  5. Extra info will be included in the density file, which will be
c     read by the PC program, M2_7.FOR. The extra info includes
c     matrices PYJGX, YPREDPOP, YPREDBAY, AND EXX.
c     Since SUBROUTINE IDCALCY (of module IDCY_5.FOR) can no longer be
c     linked with M2_7.FOR (since it calls SUBROUTINE OUTPUT - of
c     module user_4.f, which changes analysis - to - analysis, and
c     therefore cannot be linked to the user's PC program), all the
c     code which uses IDCALCY must be included in this program (and so
c     IDCY_5.FOR = idcy_5.f will be linked with this program). Then,

c     in M2_7.FOR (SUBROUTINE PREVRUN), YPREDPOP, YPREDBAY, and EXX will
c     be read in, and options 6 and 7 will be available.
 
c     Note that YPREDPOP(JSUB,IOBS,ICEN) = the predicted value for Y for
c     subject JSUB, for observation IOBS, for ICEN = 1 (means),
c     2 (medians), AND 3 (modes), where the means, medians, and modes
c     are from the final cycle population density.
 
c     Also, YPREDBAY(JSUB,IOBS,ICEN) = the predicted value for Y for
c     subject JSUB, for observation IOBS, for ICEN = 1 (means),
c     2 (medians), AND 3 (modes), where the means, medians, and modes
c     are from subject's JSUB Bayesian posterior density (calculated by
c     SUBROUTINE SUBRES - now included in this program, in addition to
c     M2_7.FOR.

 
c     Also note that the output density file is now created only at the
c     end of the run, not after each cycle (the only reason it was
c     written at the end of each cycle previously was to provide
c     'protection' in case the program bombed before finishing -- the
c     program could then pick up where it left off with an apriori
c     density from the last completed cycle, rather than starting over;
c     but downloading this density from the mainframe, incorporating it
c     into another input file, and restarting this program is probably
c     more trouble than it's worth, given the low chance that the
c     program will bomb).

 
 
c-----------------------------------------------------------------------
 
c  m2_6calc.f							1-15-97
 
C  M2_6.FOR (PC PROGRAM) + m2_6calc.f (sun/mtha program) MAKE THE
C  SAME CHANGES TO M2_5.FOR + m2_5calc.f THAT MXEM2N60.FOR MADE TO
C  MXEM2N59.FOR. THE DETAILS ARE:
 
C  THE PROGRAM NOW ALLOWS EACH PATIENT FILE TO INCLUDE ITS OWN
C  ASSSAY NOISE COEFFICIENTS (BUT PATIENT FILES WITHOUT THEIR OWN
C  INDIVIDUAL COEFFICIENTS CAN STILL BE ANALYZED).
 
C  FOR EACH PATIENT, THE ASSAY COEFFICIENTS TO BE USED WILL BE EITHER

C  THE ONES ALREADY IN ITS FILE (IF ANY), THE DEFAULT (POPULATION)
C  VALUES, OR A SPECIFIC SET FOR THAT PATIENT.
 
C  AT THE BEGINNING OF THE RUN, THE C'S TO BE USED FOR EACH PATIENT WILL
C  BE WRITTEN AT THE END OF HIS/HER ADAPT-LIKE DATA FILE (IF THEY'RE NOT
C  ALREADY THERE).
 

C  MODULE CONVRTG.FOR IS CHANGED TO CONVRTH.FOR (THIS MODULE CONTAINS
C  SUBROUTINE CONVRT, WHICH CONVERTS USC*PACK FILES TO ADAPT-LIKE
C  FILES. CONVRT IS CHANGED TO READ THE C'S IN A USC*PACK FILE, IF
C  IF THEY'RE THERE, AND WRITE THEM INTO AN ADAPT-FILE.
 
C  NOTE THAT THE 'POPULATION' ASSAY NOISE COEFFICIENTS WILL BE CALLED
C  [C0P,C1P,C2P,C3P]. [C0,C1,C2,C3] WILL BE USED FOR THE INDIVIDUAL
C  VALUES FOR EACH PATIENT, IN TURN.
 
C  NOTE THAT SUBROUTINE SCATPLOT MUST BE CHANGED SO THAT IT CAN

C  INPUT THE ASSAY COEFFICIENTS, [C0,C1,C2,C3], FOR EACH SUBJECT
C  IF JSUB=0. NEW MATRIX COEFF STORES THESE VALUES AND IS PASSED VIA
C  COMMON/TOSCAT TO SCATPLT9.FOR (CHANGED FROM SCATPLT8.FOR).
 
C-----------------------------------------------------------------------
 
C  m2_5calc.f							11-14-96
 
C  M2_5.FOR (PC PROGRAM) + m2_5calc.f (sun/mtha program) ARE EQUIVALENT

C  TO MXEM2_5.FOR. M2_5.FOR DOES THE PREPARATION PART OF THE PROGRAM,
C  AS WELL AS THE EXAMINATION OF THE OUTPUT FILES FROM THE ANALYSIS. THE
C  ANALYSIS IS DONE ON THE SUN BY m2_5calc.f.

 
C  IN THE PREPARATION PHASE, M2_5.FOR DOES ALL THE USER I/O AND
C  CONCATENATING OF PATIENT FILES. IT OUTPUTS ONE FILE, m2__calc.inp,
C  WHICH CONTAINS THE INPUT INFO FOR THIS RUN, AS WELL AS THE
C  CONCATENATED PATIENT DATA FILES FOR THIS RUN.
 
C  m2_5calc.f CAN THEN BE RUN ON A SUPERCOMPUTER AFTER UPLOADING
C  m2__calc.inp TO IT.
 
C  THE SEPARATION OF MX3M2_5.FOR INTO THE M2_5.FOR AND m2_5calc.f IS AS
C  FOLLOWS:
 

C  M2_5.FOR <-- I/O PART OF MAIN + SUBROUTINES FILRED, STACK, VERIF1,
C		CHANGE, CALMAR, THREED1, FORMLC, CONVERGE, PLTCON,
C		MINMAX, PLOTS, SUBRES, GETOUT, PREVRUN, PRNLAST, SEEDIR,

C		CALCSER, NOTINT, STAT, DELAY, AND EXTREME, ... PLUS
C               MODULES DATABLK1 IDUSER_4 CONVRTG GHCSEL IDCY_4 SCATPLT8
C               AND VODTOT.
 
C  m2_5calc.f <-- CALCULATION PART OF MAIN + SUBROUTINES FILRED, GETCOF,
C		CALGRD, NOTINT, STAT, PREDCYC, AND PRNTOP, ALONG WITH
C		SUBROUTINE EQUIV FROM m246calc.f ... PLUS

C	        MODULES iduser_4.f, idcy_4.f, AND vodtot.f (= IDUSER.FOR
C		IDCY_4.FOR, AND VODTOT.FOR, RESPECTIVELY).
 
C  NOTE THAT SUBROUTINE DELAY IS NOT USED.
 
C  NOTE THAT SEVERAL OF THE OUTPUT FORMATS ARE CHANGED FROM
C  (25,*)' ...' TO (25,xxxx) WHERE xxxx IS A FORMAT LABEL. THIS
C  PREVENTS THE sun COMPUTER FROM INSERTING AN EXTRA SPACE IN THE
C  OUTPUT FILE, WHICH SCREWS UP SUBROUTINE PREVRUN'S READING IN OF

C  THE OUTPUT FILE.
 
C-----------------------------------------------------------------------
 
C  REFER TO THE CODE OF MXEM2_5.FOR FOR THE COMMENTS REGARDING THIS
C  SERIES OF PROGRAMS.
 
C-----------------------------------------------------------------------
 
C***********************************************************************

      SUBROUTINE NPAG(MAXSUB,MAXGRD,MAXDIM,MAXACT,
     1  NUMEQT,MAXOBS,WORK,WORKK,SPXGYJ,DXI,PYJGX,PYJGXX,
     2  DENSTOR,EXX,CORDEN,CORHOLD,YPREDPOP,YPREDPOPT,YPREDBAY,CORDLAST)
 
C wmy2017Sep08 ... to get thead num in DO 800
        USE OMP_LIB
        USE npag_utils, only: makevec,check_input_array_size,expand_grid
     1   ,verifyval,cp_lrcs_to_rpar,max_input_dim,maxnumeq,max_m_per_obs
     2   ,max_ODE_params,max_pop_rand_varbs,max_doses,max_ODE_comps
     3   ,max_pop_varbs,max_pop_params,max_RS_J,k_gamma,k_flat
     4   ,k_sfac,k_ofac,k_sum_z_sq,k_prod_pr,i_skip_ig,i_do,i_cycle
     5   ,i_errmod,i_is_poisson_obs,i_is_log10,i_Npoissonobs,i_Jsub,i_IG
     6   ,k_resolve

      IMPLICIT REAL*8(A-H,O-Z)

	REAL*8 KU

C      PARAMETER(MAXNUMEQ=7)
 
        DIMENSION WORK(MAXGRD),WORKK(MAXGRD),
     1  SPXGYJ(MAXGRD),DXI(MAXGRD),PYJGX(MAXSUB,MAXACT),
     2  PYJGXX(MAXACT),DENSTOR(MAXGRD,4),
     3  CORDEN(MAXGRD,MAXDIM+1),CORHOLD(MAXGRD,MAXDIM+1),
     4  YPREDPOP(MAXSUB,NUMEQT,MAXOBS,3),
     5  YPREDPOPT(MAXSUB,NUMEQT,7201,3),
     6  YPREDBAY(MAXSUB,NUMEQT,MAXOBS,3),IPATVEC(9999),
     7  AF(max_input_dim),
     8  CORDLAST(MAXGRD,MAXDIM+1),XVERIFY(100),
     1  CORSUBRES(MAXGRD,MAXDIM+1)


C  NOTE THAT ALL THE DIMENSIONS = 25 BELOW 'SHOULD' BE CHANGED TO
C  MAXDIM, BUT SINCE THESE ARRAYS ARE SO SMALL, CHANGING THEM TO
C  VARIABLY DIMENSIONED ARRAYS (WHICH REQUIRE PASSING THE ARRAYS AND
C  MAXDIM THROUGH ALL RELATED CALLING STATEMENTS) IS NOT WORTH IT.
C  SIMILARLY FOR PX(max_ODE_params), SINCE max_ODE_params = 25 (MAXDIM) + 7 (MAX. NO. OF
C  FIXED PARAMETERS).
 
C  NOTE THAT ALL DIMENSIONS = 150 HAVE BEEN CHANGED TO max_m_per_obs, SINCE THIS
C  NO. REPRESENTS THE TOTAL NO. OF OBSERVATIONS (AND THE MAX. NO IS
C  6 OUTPUT EQUATIONS x 99 OBSERVATIONS/EQ). THIS COULD BE CHANGED
C  TO NUMEQT*MAXOBS, BUT IT WOULD BE MORE TROUBLE THAN IT'S WORTH TO
C  MAKE THESE DIMENSIONS VARIABLE.
C  ACTUALLY, IN THE PC PREP PROGRAM, MAXOBDIM IS SET = 150. SO THAT
C  REMAINS THE MAX. NO. OF OBSERVATION TIMES (AS OF NPAG113.FOR).

      DIMENSION YO(max_m_per_obs,NUMEQT),SIG(max_m_per_obs,MAXNUMEQ),
     1 TPRED(71281),YYPRED(71281,NUMEQT),C5P(NUMEQT),C5(NUMEQT),
     2 C0P(NUMEQT),C1P(NUMEQT),C2P(NUMEQT),C3P(NUMEQT),C4P(NUMEQT),
     3 C0(NUMEQT),C1(NUMEQT),C2(NUMEQT),C3(NUMEQT),C4(NUMEQT),
     4 TIMOB(max_m_per_obs),DOSTIM(max_doses),
     5 RS(max_doses,max_RS_J),YOO(max_m_per_obs,MAXNUMEQ),
     6 BS(max_doses,max_input_dim),NUMT(MAXSUB),
     6 TTPRED(MAXSUB,7200),TEND(99),NOMAXTIM(MAXSUB),
     7 TENDSUB(MAXSUB,99),
     7 TBEGG(99),TBEGGSUB(MAXSUB,99),TPREDREL(71281),
     8 TTPREDREL(MAXSUB,7200),TIMOBREL(MAXSUB,max_m_per_obs),
     9 YYYPRED(3,71281,NUMEQT)

C
C ------ PK model parameter declarations
C
C      DIMENSION AB(30,2),    ! population random parameter ranges
C     1 EXX(MAXSUB,3,30),CENTER(3,30),EXXX(30)   ! central tendencies, mean, med, and mode 
C     2 E(30,30),EX(30),CORR(30,30),COV(30,30),STD(30),    ! COV = E - EX(I)*EX(J); CORR = COV/STD
C     3 COFVR(30),    ! = STD/EX
C     9 X(30),    ! I think these are means
C     9 EXO(30),START(30),STEP(30),XMED(30),OPTVAR(max_ODE_params),    ! eldery variables
C     3 RANFIXEST(max_pop_varbs)
C     2 VALFIX(max_pop_params), ! Population parameters: fixed, user defined constant parameters
C     9 PX(max_ODE_params) ! Support point
C
      double precision, dimension(max_pop_rand_varbs,2) :: AB
      double precision, dimension(max_pop_rand_varbs) :: X,EX,EXXX,
     1 STD, COFVR, XMED, EXO,START, STEP, OPTVAR
      double precision, dimension(max_pop_rand_varbs,
     1 max_pop_rand_varbs) :: COV, CORR, E
      double precision, dimension(MAXSUB,3,max_pop_rand_varbs) :: EXX
      double precision, dimension(3,max_pop_rand_varbs) :: CENTER
      double precision, dimension(max_pop_varbs) :: RANFIXEST
      double precision, dimension(max_pop_params) :: VALFIX
      double precision, dimension(max_ODE_params) :: PX

       CHARACTER PAR(max_pop_rand_varbs)*11
       CHARACTER PARRANFIX(max_pop_varbs)*11
       CHARACTER PARFIX(max_pop_params)*11
C
C --- end PK model parameter declarations
C


C  NOTE THAT THE 2ND DIMENSION OF SIG AND YOO IS MAXNUMEQ, RATHER THAN
C  NUMEQT. THE REASON IS THAT THESE ARRAYS ARE PASSED IN COMMONS AND
C  THEREFORE CAN ONLY BE VARIABLY DIMENSIONED BY A VALUE SET IN
C  A PARAMETER STATEMENT.

      CHARACTER PREFIX*5,READLINE*300,EXT*3,NAME*4,
     1PRIFIL2*20,DENFIL*20,OUTFIL*20,PREDFIL*20,
     2OUTCOM*20,READLARG*1000,OUTFILER*20,ERRFIL*20

      character*20 ERRFILNAME
      character*20 ITFIL

C       COMMON SIG
C       COMMON/SUPRES/ISUPRES
C        COMMON/OBSER/TIMOB,DOSTIM,RS,YOO,BS 
C       COMMON/NXER/NXE
C NXE FROM ABOVE COMMON IS NO. OF TIMES XERRWD IS CALLED.

C wmy2017Sep26 -- WMY retained the common blocks, and all
c  common parameters are still initialized.  This is so that
c  the analytic routes have access to them. But the parallelized
c  code (calling dvode via USERANAL) does not use the common
c  blocks. And eventually ANAL3() will have to receive 
c  required arguments, rather than read values in from a common
c  block.
C        COMMON/PARAMD/P
C        COMMON/INPUT/ R,B
!       COMMON/TOCALC/IRAN,PX,NOFIX,NSUB,gamma,flat,AB
!       COMMON/TOCALC/gamma,flat,AB,PX,IRAN,NOFIX,NSUB
C   	COMMON/TOUSER/NDIM,MF,RTOL,ATOL

C !$omp ThreadPrivate(/OBSER/)
C !$omp ThreadPrivate(/TOCALC/,/PARAMD/,/INPUT/)

C  COMMON/TOCALC IS PROVIDED TO SUBROUTINE CALCRF, WHICH IS CALLED
C  BY SUBROUTINE ELDERY. wmy20190722-variables passed to CALCRF
C  via ELDERY in argument list. No more need for /TOCALC/

C      COMMON/ERR/ERRFIL 
      logical input_arrays_within_bounds

       integer nxe

c wmy2017Sep22 Variables passing through DO 800
       integer NVAR,NRANFIX
       integer, dimension(max_ODE_params) :: IRAN

C "interface" to emint()
       integer ijob

c wmy2017Dec01 DO 1000 variables
       integer NInDO1000
       real PInDO1000
       double precision, dimension(max_m_per_obs,NUMEQT) :: YPRED

C wmy2018Apr17 (Tax Day) DO 140 Variables
       integer UseInlineDO140
       integer MISVAL, NNORMALOBS,NPOISSONOBS
       real*8  SIGFAC, OFAC

c wmy2017Sep08 DO 800 variables
       integer IterFirst, IterLast, NInDO800, NNInDO800,NBadInDO800
       integer NPX, NOFIXCOPY,ThreadNo
       integer, dimension(max_ODE_params) :: IRANCOPY
       real*8 W, XXIG(max_pop_rand_varbs)
       real    PInDO800
       double precision, save, dimension(max_m_per_obs,MAXNUMEQ)
     1   :: ObsError
C wmy2018.10.16 /BOLUSCOMP/
       integer, save, dimension(max_input_dim) :: NBCOMP
C wmy2017Sep22 /TOUSER/
       integer NDIM, MF
       doubleprecision RTOL
       real*8, dimension(max_ODE_comps) :: ATOL
C wmy2017Sep26 Copies of /INPUT/, /PARAMD/
       real*8, dimension(max_RS_J) :: R, RCOPY
       real*8, dimension(max_ODE_params) :: P
C       real*8, dimension(max_ODE_params) :: PCOPY
       real*8, dimension(max_ODE_comps) :: B, BCOPY
       real*8, save, dimension(max_m_per_obs) :: TIMCOPY
       real*8, save, dimension(max_doses) :: SIGCOPY
       real*8, save, dimension(max_doses,max_RS_J) :: RSCOPY
       real*8, save, dimension(max_doses,max_input_dim) :: BSCOPY
c wmy2017Sep30 -- Keep a copy of all integers
C   required to calculate P(JSUB|IG) here.
       integer, save, dimension(128) :: INTLIST
       integer, save, dimension(257) :: IPAR
       double precision, save, dimension(257) :: RPAR

       integer alg_type
       integer isupres

C COMMON/BAY
        integer, dimension(100) :: NACTSUB
        real*8, dimension(100,1500,31) :: BAYPOS
C COMMON/DOSEOBS
        real*8, dimension(800,150,MAXNUMEQ+1) :: OBSBLOCK
        real*8, dimension(800,1000,35) :: DOSEBLOCK
        integer, dimension(800) :: NDORIG

C       save ObsError, IPAR  ! SAVE attribute in declaration
!$omp ThreadPrivate(ObsError,IPAR,RPAR,INTLIST)

C wmy2017Nov14 -- Trying to figure out why non-master threads are
c   slightly off; Removed RSCOPY and BSCOPY from Firstprivate
c   list in DO 800 and made them ThreadPrivate here; as far as I
c   can tell, these are the last two variables that are SAVEd but
c   were not threadprivate -- I can't figure out a good reason
c   to suspect these variables are an issue, except that they are
c   large and so might need to be on the heap. An example of the
c   error thrown is:
c 
c DVODE--  Warning: internal T (=R1) and H (=R2) are                              
c       such that in the machine, T + H = T on the next step                      
c       (H = step size). solver will continue anyway                              
c      In above,  R1 =  0.7000000000000D+01   R2 =  0.1897079122660D-26 
c
c !$omp ThreadPrivate(RSCOPY,BSCOPY)
c
c Above is incorrect for an unknown reason. The code generates 
c   following errors (examples below):
c "R init :: RS.ne.RSCOPY for          15         203           1 "
c
c


C  THE BLANK COMMON ABOVE IS SUPPLIED TO SUBROUTINE IDPC.
C  COMMON/TOUSER IS SUPPLIED TO SUBROUTINE USERANAL IN idfixed.f.
C  COMMON/OBSER/ IS SUPPLIED FROM SUBROUTINE FILRED.
C  COMMON/SUPRES/ IS SUPPLIED TO SUBROUTINE EMINT.
C  COMMON/ERR/ IS SUPPLIED TO ALL THE ROUTINES WHICH COULD WRITE TO
C   ERRFIL.

      EXTERNAL CALCRF

C***********************************************************************
 
C-----------------------------------------------------------------------
 
    2 FORMAT(A20)
  222 FORMAT(A3)
 2222 FORMAT(A5)
 
C-----------------------------------------------------------------------
 
C  INPUT FILE npag103.inp FROM THE PREPARATION PROGRAM. npag103.inp 
C  CONTAINS THE USER DESIRED PARAMETER VALUES, ALONG WITH THE 
C  CONCATENATED PATIENT DATA FILES (IN ADAPT FORMAT).

CCCCCCCCCCCCCCCCCCCCCC  INPUT INFO  (BELOW) CCCCCCCCCCCCCCCCCCCCCCCCC
 
C  INPUT THE FOLLOWING DATA FROM FILE npag103.inp.

C  READ IN VALUES FOR MF, RTOL, AND ATOL, WHICH ARE NEEDED FOR THE

C  O.D.E. SOLVER USED BY ROUTINE USERANAL (IN MODULE IDUSER__.FOR).
 
C MF     = Method flag.  Standard values are..
C          10 for nonstiff (Adams) method, no Jacobian used.
C          21 for stiff (BDF) method, user-supplied full Jacobian.
C          22 for stiff method, internally generated full Jacobian.
C          24 for stiff method, user-supplied banded Jacobian.
C          25 for stiff method, internally generated banded Jacobian.
C RTOL   = Relative tolerance parameter (scalar).
C ATOL   = Absolute tolerance parameter.
C          The estimated local error in X(i) will be controlled so as
C          to be roughly less (in magnitude) than
C             EWT(i) = RTOL*abs(X(i)) + ATOL(i)  SINCE ITOL = 2.
C          Thus the local error test passes if, in each component,
C          either the absolute error is less than ATOL (or ATOL(i)),
C          or the relative error is less than RTOL.
C          Use RTOL = 0.0 for pure absolute error control, and
C          use ATOL = 0.0 (or ATOL(i) = 0.0) for pure relative error
C          control.  Caution.. Actual (global) errors may exceed these
C          local tolerances, so choose them conservatively.
 
C  IFORMT = 1 OR 3 IF PATIENT DATA FILES ARE IN ADAPT FORMAT. AND THEY 
C           ALWAYS ARE (ALTHOUGH THIS IS THE NEW MULTI-DRUG FORMAT).
C           NOTE THAT IFORMT IS STILL READ IN, BUT CALLED JUNK.
 
C       PREFIX = 5-CHARACTER PREFIX FOR ALL SUBJECT FILENAMES. THEN
C           001, 002,... UP TO 999  WILL COMPLETE THE 1ST NAMES OF THE
C           INDIVIDUAL FILENAMES WHICH ARE READ IN.
 
C       EXT = AN OPTIONAL 3-CHARACTER EXTENSION (AFTER DECIMAL POINT)
C             FOR THE INPUT FILENAMES.
 
C  	NVAR = NO. OF RANDOM VARIABLES FOR THE RUN.
C  	PAR(I) = NAME OF R.V. I; I=1,NVAR.
C 	NOFIX = NO. OF FIXED PARAMTER VALUES FOR THE RUN.
C 	PARFIX(I) = NAME OF FIXED PARAMETER I; I=1,NOFIX.
C     NRANFIX = NO. OF UNKNOWN PARAMETERS WHICH ARE THE SAME FOR 
C               ALL SUBJECTS FOR THE RUN.
C     PARRANFIX(I) = NAME OF UNKNOWN PARAMETER I; I=1,NRANFIX.

C  	IRAN(I) = 1 IF PARAMATER I IS RANDOM;
C	          0 IF PARAMETER I IS FIXED; 
C               2 IF PARAMETER I IS UNKNOWN BUT THE SAME FOR ALL
C                 SUBJECTS; I = 1,NVAR+NOFIX+NRANFIX.


C  NSUBTOT = TOTAL NO. OF SUBJECTS IN THE PATIENT POPULATION. 
C  NSUB = NO. OF SUBJECTS WHOSE DATA ARE TO USED FOR THE ANALYSIS (MAX
C         NO. = 999).
C  IPATVEC(I),I=1,NSUB = INDICES OF THE SUBJECTS WHICH ARE TO BE USED

C                        IN THIS ANALYSIS.
 
C  IF NOFIX > 0, VALFIX(I) = VALUE OF FIXED PARAMETER I, I=1,NOFIX.

C  IF NRANFIX > 0, RANFIXEST(I) = INITIAL ESTIMATE FOR RANFIX PARAMETER
C   I, I = 1,NRANFIX.

C   	AB(I,1) = LOWEST VALUE FOR VARIABLE I ON ITS GRID, I=1,NVAR.
C     AB(I,2) = HIGHEST VALUE FOR VARIABLE I ON ITS GRID,I=1,NVAR.

 
C	NUMEQT = NO. OF OUTPUT EQUATIONS.
C	C0P(I),C1P(I),C2P(I),C3P(I) = COEFFICIENTS FOR THE ASSAY STD.
C		DEV. (OF THE OBSERVED VALUES) FOR OUTPUT EQ. I;
C		I=1,NUMEQT. THEY'RE INCLUDED ONLY SO THEY CAN BE PUT
C		INTO THE OUTPUT FILE.

C  AS OF m2_6calc.f, EACH SUBJECT'S INDIVIDUAL C'S ARE INCLUDED IN THE
c  CONCATENATED PATIENT DATA FILES PORTION OF npag102.inp.
 
C  IERRMOD, GAMLAM0 = INFO ON THE ERROR PATTERN (SEE CODE).


C  NDRUG = NO. OF DRUGS IN THE PATIENT DATA FILES.
C  AF(I),I=1,NDRUG = ACTIVE (SALT) FRACTION FOR DRUG I.

C  INDPTS = THE INDEX OF THE NO. OF GRID POINTS TO BE USED.
C	    INDPTS = 1,2,3,4,5,6 FOR,
C           RESPECTIVELY, 2129, 5003, 10007, 20011, 40009, OR 80021 PTS.
C           IF INDPTS > 6, THE NO. OF GRID PTS = 80021*(INDPTS-100).
 
C    AS OF npbig1.f, THE GRID POINTS ARE FOUND DIRECTLY FROM

C    SUBROUTINE CALGRD (I.E., SUBROUTINE GETCOF IS NO LONGER USED).
 
 
C  MAXCYC = MAXIMUM NO. OF CYCLES; THE PROGRAM WILL STOP BEFORE THIS 
C           NO. OF CYCLES IF IT CONVERGES--SEE FORMATS 5197 AND 5198
C           IN THE CODE.


C  NOTE THAT JSTOP AND TOL ARE STILL READ IN, BUT ARE NOT USED. INSTEAD,
C  TOL IS HARDCODED TO BE 1.D-4 IN THIS PROGRAM.

C  IDELTA, T_END, XMIC, ICENT, AUCINT: THESE VALUES WILL BE USED TO 
C   CALCULATE PREDICTED VALUES FOR EACH SUBJECT AT TIMES 0, IDELTA,


C   2*IDELTA, ..., T-END, ALONG WITH AUC'S (AREAS UNDER CURVES) AND 
C   AUC/MIC'S. NOTE THAT IDELTA = 2, 4, 6, OR 12 MINUTES, AND T_END IS 
C   IN HOURS.

C   NO. AS OF bigmlt6.f, T_END WILL STILL BE READ, BUT NO LONGER USED,
C    SINCE SUBJECTS CAN HAVE TIME RESETS --> EACH SUBJECT WILL HAVE TO
C    HAVE ITS MAXIMUM TIMES (ONE FOR EACH SET OF TIMES BEFORE A TIME
C    RESET OR THE ENDING TIME) CALCULATED BY NEW SUBROUTINE CALCTPRED.
 
C  ICYCLE = NO. OF CYCLES RUN BY A PREVIOUS RUNNING OF THIS PROGRAM.
 

C  IF ICYCLE .GT. 1 --> THIS RUN IS PICKING UP WHERE A PREVIOUS RUN
C			LEFT OFF. IN THIS CASE, READ IN ALSO:
C	DORIG = THE UPPER BOUND FOR THE DIFFERENCE BETWEEN THE DENSITY
C		OF CYCLE 1 AND THE MAXIMUM LIKELIHOOD ESTIMATE OF THE
C		DENSITY.
 
C	NACTVE = THE NO. OF ACTIVE GRID POINTS IN THE PRIOR DENSITY.
C	CORDEN(I,.) = ITH ACTIVE GRID COORDINATES (1ST NVAR ENTRIES) +
C		      THE ASSOCIATED DENSITY (NVAR+1 ST ENTRY),
C		      I=1,NACTVE.

C	PRIFIL2 = NAME OF FILE GIVING THE PRIOR INFO (DORIG, CORDEN,
C		  ETC). NOTE THAT THIS PRIOR INFO HAS BEEN STORED
C		  INTO npembig3.inp. PRIFIL2 ITSELF IS NOT USED BY THE
C		  PROGRAM, BUT THE NAME IS INCLUDED SO THE USER WILL
C		  KNOW WHERE THE PRIOR DATA ORIGINATED.
 
CCCCCCCCCCCCCCCCCCCCCCCC  INPUT INFO (ABOVE) CCCCCCCCCCCCCCCCCCCC

      input_arrays_within_bounds = check_input_array_size(
     1  MAXSUB,MAXGRD,MAXDIM,MAXACT,NUMEQT,MAXOBS,WORK,WORKK,
     2  SPXGYJ,DXI,PYJGX,PYJGXX,DENSTOR,EXX,CORDEN,CORHOLD,
     3  YPREDPOP,YPREDPOPT,YPREDBAY,CORDLAST)
      if (input_arrays_within_bounds .eqv. .false.) then
         write (*,*) "Input array error; exiting"
         return
      end if

2227        FORMAT(A11)

c  Read 'extnum' to get the 4 digit job number which will be used for
c  each of the files produced by this program. Read this integer now,
c  and replace the value by 1 greater (unless it is 9999, in which case
c  replace it by 1) and then close extnum.
 
C  OPEN FILE extnum AND READ THE NO. THERE.
 
	OPEN(25,FILE='extnum',STATUS='OLD')
	READ(25,*) INUM
 
C  OBTAIN THE CHARACTER*4 EQUIVALENT TO INUM.
 
	CALL EQUIV(INUM,NAME)
 

C  REPLACE THE NO. IN 'extnum' BY INUM+1 (EXCEPT INUM=9999 IS
C  TO BE REPLACED BY 1).
 
	JNUM=INUM+1
	IF(JNUM .EQ. 10000) JNUM = 1
	BACKSPACE(25)
	WRITE(25,*) JNUM
	CLOSE(25)

 
C  CREATE OUTPUT FILE WHICH HAS 'OUT' AS ITS 1ST 3 CHARACTERS AND

C  NAME AS ITS LAST 4. SIMILARLY, CREATE DENFIL, PREDFIL, ITFIL, AND
C  ERRFIL.

C  AS OF bignpaglap1.f, THE NAME FOR THE OUTPUT FILE IS CHANGED TO
C  'OUTT'//NAME SINCE 'OUT//NAME WILL BE RESERVED FOR THE COMBINED
C  OUTPUT FILE FORMED AT THE END OF THE RUN.
 
      OUTFIL = 'OUTT'//NAME
      DENFIL = 'DEN'//NAME
      PREDFIL = 'PRTB'//NAME
      ITFIL = 'ILOG'//NAME
      ERRFIL = 'ERROR'//NAME
      ERRFILNAME = ERRFIL

      OPEN(23,FILE='npag103.inp',ERR=4705,STATUS='OLD')
      GO TO 4710
 4705	WRITE(*,4706)
 4706 FORMAT(/' INPUT FILE npag103.inp IS NOT AVAILABLE. THE'/
     1' PROGRAM STOPS. TRY AGAIN AFTER RUNNING THE PREPARATION PROGRAM'/
     2' TO CREATE npag103.inp, AND THEN PUTTING THAT FILE IN THE '/
     3' WORKING DIRECTORY.'/)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,4706) 
        CLOSE(42)

            CALL PAUSE
            STOP

 4710       READ(23,*) NDIM
            READ(23,*) MF
            READ(23,*) RTOL
            READ(23,*) (ATOL(I),I=1,NDIM)
            READ(23,*) JUNK
            READ(23,2222) PREFIX
            READ(23,222) EXT
            READ(23,*) NVAR
            READ(23,2227) (PAR(I),I=1,NVAR)
            READ(23,*) NOFIX
            READ(23,2227) (PARFIX(I),I=1,NOFIX)
            READ(23,*) NRANFIX
            READ(23,2227) (PARRANFIX(I),I=1,NRANFIX)
            READ(23,*) (IRAN(I),I=1,NVAR+NOFIX+NRANFIX)


C  READ IN BOTH NSUBTOT AND NSUB AND THEN CALL ROUTINE GETIPATF WHICH 
C  READS THIS PORTION OF FILE 23 TO OBTAIN IPATVEC. NOTE THE 1ST 
C  ARGUMENT TELLS GETIPATF TO READ FILE 23.

C  NOTE THAT IF IERRR RETURNS AS -1, THERE IS A PROBLEM WITH THE
C  PATIENT NO. INFO ON THE FILE. IN THIS CASE, THE PROBLEM HAS ALREADY
C  BEEN WRITTEN TO THE SCREEN. SO STOP.

       READ(23,*) NSUBTOT
       READ(23,*) NSUB
       CALL GETIPATF(23,NSUBTOT,NSUB,IPATVEC,IERRR,ERRFIL)

       IF(IERRR .EQ. -1) THEN
        CALL PAUSE
        STOP
       ENDIF

       IF(NOFIX .GT. 0) READ(23,*) (VALFIX(I),I=1,NOFIX)
       IF(NRANFIX .GT. 0) READ(23,*) (RANFIXEST(I),I=1,NRANFIX)
 
       DO I=1,NVAR
        READ(23,*) (AB(I,J),J=1,2)
       END DO

 
C  NOTE THAT NUMEQT WAS PASSED IN ARGUMENT LIST FROM npagdriv.f
C  TO THIS MODULE, SUBROUTINE BIGNPAG, SO JUST READ(23,*) ON NEXT LINE.
 
		READ(23,*)

		DO IEQ=1,NUMEQT
		 READ(23,*) C0P(IEQ),C1P(IEQ),C2P(IEQ),C3P(IEQ)
     1                      ,C4P(IEQ),C5P(IEQ) 
		END DO
 
		READ(23,*) ierrmod,gamlam0

                IPAR(i_errmod) = ierrmod

C  NOTE THAT IN npag102.inp FROM NPBIG15.FOR, ierrmod AND gamlam0
C  (INITIAL EST. FOR gamma or lambda) WILL BE READ IN. AND NOTE THAT FOR
C  ierrmod = 4, THIS VALUE SHOULD BE READ IN AS flat, RATHER THAN AS


C  gamma, TO BE COMPATIBLE WITH THE CODE BELOW.


	gamma = 1.d0
	flat = 1.d0
	if(ierrmod .eq. 2) gamma = gamlam0
        if(ierrmod .eq. 3) gamma = gamlam0
        if(ierrmod .eq. 4) flat = gamlam0

          RPAR(k_gamma) = gamma
          RPAR(k_flat) = flat

	igamma = 0
        gamdel=0.1
        if(ierrmod.eq.1) gamdel=0.d0

        READ(23,*) NDRUG

        READ(23,*) (AF(I),I=1,NDRUG)

		READ(23,*) INDPTS
 
C  ESTABLISH THE NO. OF GRID POINTS TO BE USED.
 
	IF(INDPTS .EQ. 1) NGRID=2129
	IF(INDPTS .EQ. 2) NGRID=5003

	IF(INDPTS .EQ. 3) NGRID=10007
	IF(INDPTS .EQ. 4) NGRID=20011
	IF(INDPTS .EQ. 5) NGRID=40009
	IF(INDPTS .EQ. 6) NGRID=80021
	IF(INDPTS .GT. 6) NGRID = (INDPTS - 100)*80021
cadapt change 1 - reset ngrid to MAXACT if NGRID>MAXACT
 
        if(ngrid.gt.maxact) then
 
          write(6,*)
 
          write(6,*) 'requested NGRD = ',NGRID, ' gridpoints'
          write(6,*) 'maximum allowable is MAXACT=',MAXACT
          write(6,*) 'resetting NGRID = ',MAXACT
 

          write(6,*) 'to fit in available storage'
 
          write(6,*)
 
          ngrid = maxact
 
         endif
      ngridn=ngrid
 
 
      READ(23,*) MAXCYC
      READ(23,*) JSTOP
      IF(JSTOP .NE. 1) READ(23,*) TOLC

c  As of npageng23.f, TOL is no longer read in, as it is going to be

c  reset = 1.D-4 below anyway. Instead, TOLC (the value against which
c  checkbig is compared) will now be read in.

      TOL = 1.D-4

		NINT=100
 
		READ(23,*) IDELTA
		READ(23,*) T_END
		READ(23,*) XMIC
		READ(23,*) ICENT
		READ(23,*) AUCINT
		READ(23,*) ICYCLE
 
		IF(ICYCLE .GE. 1) THEN
		  READ(23,*) DORIG
 

		  READ(23,*) NACTVE

		  DO I=1,NACTVE
		   READ(23,*) (CORDEN(I,J),J=1,NVAR+1)
		  END DO
 
		  READ(23,2) PRIFIL2
		ENDIF
 
 
c  The patient data info is read in from npag103.inp, and is put onto 
c  scratch file 27 (because it will need to be reread each cycle --> 
c  file 27 will be rewound each cycle).

c  Note that there are NSUBTOT subjects, but only NSUB of them,
c  with indices IPATVEC(I),I=1,NSUB, will be put onto file 27.
 
        OPEN(27)

 1717  FORMAT(A300)
       NLAFIR = 0

       DO JSUB = 1,NSUB


C  THE NEXT SUBJECT WHOSE DATA SET IS TO BE PUT TO FILE 27 IS SUBJECT 
C  IPATVEC(JSUB). SO FAR, NLAFIR IS THE NO. OF LINES WHICH HAVE BEEN
C  READ WHICH HAVE 'LAST AND FIRST' AS CHARACTERS 3:16 (THIS LINE IS
C  THE FIRST LINE OF EACH SUBJECT'S DATA SET). READ UNTIL THE NEXT
C  SUCH LINE.

 1720    READ(23,1717,IOSTAT=IEND) READLINE

	 IF(IEND .LT. 0) THEN

C  NOTE THAT IEND .LT. 0 --> END OF FILE REACHED, BUT IF IT'S REACHED
C  AT THIS POINT, NOT ALL "ACTIVE" NSUB SUBJECT DATA SETS WERE READ

C  AND WRITTEN CORRECTLY TO FILE 27. IN THIS CASE, WRITE A MESSAGE TO
C  THE USER AND STOP.

        WRITE(*,1721)
 1721   FORMAT(/' PATIENT DATA INFORMATION WAS NOT READ CORRECTLY'/
     1' FROM THE INSTRUCTION FILE, npag103.inp. IF YOU EDITED THIS'/
     2' FILE MANUALLY, PLEASE RERUN THE PC PREP PROGRAM TO HAVE IT'/
     3' PREPARE npag103.inp AGAIN AND THEN RERUN THIS PROGRAM.'//


     4' IF YOU DID NOT MANUALLY EDIT npag103.inp, PLEASE SEND THE'/
     5' DETAILS OF THIS RUN (STARTING WITH THE PC PREP EXECUTION) TO'/
     5' THE LAPK. '//
     6' THANK YOU.'/)
  
C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,1721) 
        CLOSE(42)

	  CALL PAUSE

	  STOP

	 ENDIF


	 IF(READLINE(3:16) .NE. 'LAST AND FIRST') GO TO 1720

	  NLAFIR = NLAFIR+1

C  THIS READLINE IS THE BEGINNING OF A SUBJECT'S DATA. IF NLAFIR
C  = IPATVEC(JSUB), THIS SUBJECT'S DATA IS TO BE USED, SO PUT IT ON
C  FILE 27. IF NLAFIR .LT. IPATVEC(JSUB), KEEP READING UNTIL 
C  NLAFIR = IPATVEC(JSUB).

	   IF(IPATVEC(JSUB) .GT. NLAFIR) GO TO 1720

C  TO GET TO THIS POINT, IPATVEC(JSUB) = NLAFIR, SO PUT THIS SUBJECT'S
C  DATA ONTO FILE 27 BY CALLING SUBROUTINE NEWWORK1 (BASED ON THE 
C  STAND-A-LONE PROGRAM OF THE SAME NAME). NOTE THAT IF THE FILE HAS
C  NO STEADY STATE DOSE INDICATORS, IT WILL NOT BE CHANGED; IF IT DOES,
C  IT WILL BE ALTERED TO INCLUDE AN EXTRA 101 DOSES SETS FOR EACH STEADY
C  STATE DOSE INDICATOR. NOTE THAT, UNLIKE IN THE STAND-A-LONE PROGRAM,
C  SUBROUTINE NEWWORK1 WILL LEAVE IN THE NEGATIVE DOSE TIME (WHICH IS
C  THE STEADY STATE DOSE INDICATOR) BECAUSE THE ID ROUTINES NEED TO SEE
C  THIS INDICATOR TO KNOW THAT A STEADY STATE DOSE SET IS COMING.

       CALL NEWWORK1(MAXSUB,JSUB,TIMOBREL,
     1 DOSEBLOCK, OBSBLOCK, NDORIG , errfilname)

C  AS OF npageng18.f, NEWWORK1 ALSO ESTABLISHES TIMOBREL(JSUB,J).
C  THESE ARE THE "RELATIVE" OBSERVATION TIMES FOR THIS SUBJECT. 
C  IF THE SUBJECT CONTAINS STEADY STATE DOSE SETS, THESE TIMES WILL
C  DIFFER FROM THE TIMES WRITTEN TO FILE 27 SINCE THOSE TIMES WILL BE 
C  THE "REAL" TIMES STARTING FROM THE BEGINNING OF EACH STEADY STATE
C  DOSE SET, AND NEEDED BY THE ID MODULES, RATHER THAN THE "RELATIVE"
C  TIMES STARTING AT THE END OF EACH STEADY STATE DOSE SET).

  
	END DO

C  THE ABOVE END DO CLOSES THE  DO JSUB = 1,NSUB  LOOP.

 
 1730 REWIND(27)
	CLOSE(23)

C  NOTE THAT IF LABEL 1730 WAS REACHED VIA THE
C  IF(IEND .LT. 0) GO TO 1730    STATEMENT ABOVE, IT MUST BE BECAUSE
C  THE END OF THE FILE WAS REACHED AND THE LAST SUBJECT ON THE FILE
C  23 WAS ALSO THE LAST ONE (NO. IPATVEC(NSUB)) TO BE ANALYZED. THIS
C  MEANS JSUB SHOULD BE NSUB. IF, HOWEVER, JSUB .LT. NSUB, IT MEANS
C  THAT, SOMEHOW, NOT ALL NSUB SUBJECTS TO BE ANALYZED WERE ON THE
C  npag103.inp FILE. IN THIS CASE, WRITE A MESSAGE TO THE USER AND
C  STOP.

	 IF(JSUB .LT. NSUB) THEN
	  WRITE(*,1721)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,1721)
        CLOSE(42)

	  CALL PAUSE
	  STOP
	 ENDIF


 
C  IF ISUPRES = 1 --> the output to the screen will be minimal ...
c  one line having just the cycle no., convergence criterion, and the 
c  medians. The exception is that until NACTVE .LE. NSTORE, the
c  program will print to the screen the update on what % of grid
c  points have been calculated since otherwise the user might think

c  his computer has locked up (once NACTVE .LE. NSTORE, all the
c  P(YJ|X)'s will already be stored into PYJGX --> the DO 800 loop
c  will go very fast.

c  IF ISUPRES = 0 --> the output to the screen will be the full set
c  of information that has been historically printed.

	ISUPRES = 1



C  OPEN THE OUTPUT FILE -- ALL OUTPUT FROM THE PROGRAM WILL BE PUT
C			   INTO THIS FILE.
 
	OPEN(25,FILE=OUTFIL)
 
C  SET IFIRST = THE 1ST CYCLE FOR THIS RUN. IFIRST MUST BE SAVED SO
C  LOOP 800 WILL ONLY NEED TO CALL IDPC IF ICYCLE=IFIRST.
 
	IFIRST=ICYCLE+1
 
c open the iteration log file
      open(91,file=ITFIL)
      if(ierrmod.eq.1.or.ierrmod.eq.2) write(91,9190)
      if(ierrmod.eq.4) write(91,9290)
      if(ierrmod.eq.3) write(91,9390)
cgam2_9190 format('  icycle',5x,'fobj1',10x,'fobj0',10x,'res',5x,
 9290 format('  icycle',5x,'fobj1',10x,'flat wt',8x,'res',5x,
     &      'grid points (start and end)')
 9190 format('  icycle',5x,'fobj1',10x,'gamma',10x,'res',5x,
     &      'grid points (start and end)')

 9390 format('  icycle',5x,'fobj1',10x,'lambda',9x,'res',5x,
     &      'grid points (start and end)')

c  Create the files CHMAXCYC.BAT, CHMAXCYC.OLD, and CHMAXCYC.NEW:
C  CHMAXCYC.OLD has one line with a 1 in it. CHMAXCYC.NEW has one
c  line with a 0 in it. CHMAXCYC.BAT is a DOS batch file which has
c  the system command to copy CHMAXCYC.NEW to CHMAXCYC.OLD.

C  Note that the program will open and read CHMAXCYC.OLD at the 
c  beginning of each new cycle. If it reads a 1, it will continue the 

c  calculations as before. If it reads a 0, it will change the value of 
c  MAXCYC to whatever ICYCLE is currently, which will cause the program 
c  to halt at the end of that cycle just as if MAXCYC had been set
c  to the changed value originally. If this happens, the program will
c  write a comment to the console and file 25 of why the program
c  has stopped prematurely.

c  Note that the user may want to exercise this option if he wants to
c  see the results from a slowly converging run, and then, because
c  the density file from the last cycle will have been correctly 
c  created, still be able to start another run using the final cycle 
c  joint density from the halted run (with different parameters if 
c  desired).

	OPEN(37,FILE='CHMAXCYC.OLD')
	 WRITE(37,*)'   1'
	CLOSE(37)

	OPEN(37,FILE='CHMAXCYC.NEW')
	 WRITE(37,*)'   0'
	CLOSE(37)

	OPEN(37,FILE='CHMAXCYC.BAT')
	 WRITE(37,*)'  COPY CHMAXCYC.NEW CHMAXCYC.OLD'
	CLOSE(37)

c  Tell the user that he can execute the batch file, CHMAXCYC.BAT 
c  (by typing CHMAXCYC at a DOS prompt), in the working directory of 
c  his PC and that will cause the program to stop safely at the end of 
c  whatever cycle it is on, as if the maximum no. of cycles has been 
c  reached.



	WRITE(*,1231)
 1231   FORMAT(/' IF YOU WOULD LIKE TO STOP THE PROGRAM BEFORE THE'/
     1' MAXIMUM NO. OF CYCLES HAVE BEEN RUN, ALT-TAB TO A DOS'/
     2' WINDOW IN THE WORKING DIRECTORY OF THE RUN AND TYPE: '//
     3' >CHMAXCYC   '//
     4' THIS WILL CAUSE THE PROGRAM TO STOP SAFELY AT THE END OF THE '/
     5' NEXT CYCLE, AFTER CREATING THE OUTPUT FILES.')
!	CALL PAUSE
C  COMMENTED OUT THE ABOVE ALL PAUSE AS OF npageng28.f

C  CALCULATE VOLSPA, THE 'VOLUME' OF THE INTEGRATION SPACE (NEEDED IN
C  CALLS TO NOTINT).
 
      VOLSPA=1.D0
      DO 170 I=1,NVAR
  170 VOLSPA = VOLSPA*(AB(I,2)-AB(I,1))
 
 
C  IF THE NO. OF PREVIOUS CYCLES, ICYCLE = 0, IT MEANS THAT THIS IS
C  A NEW RUN, USING A UNIFORM PRIOR JOINT DENSITY. IF ICYCLE .GE. 1,
C  THIS IS A CONTINUATION RUN, AND THE PRIOR JOINT DENSITY HAS BEEN
C  READ IN ABOVE.
 
	IF(ICYCLE .EQ. 0) THEN
 
C  FIND THE VALUE OF THE UNIFORM DENSITY OVER THE 'RECTANGULAR' NVAR-
C  DIM VECTOR SPACE [AB(1,1),AB(1,2)] x ... x [AB(NVAR,1),AB(NVAR,2)].
 
      CONST=1.D0/VOLSPA
 
C  CONST IS THE INITIAL VALUE OF THE JOINT DENSITY AT ALL THE GRID
C  POINTS TO BE DETERMINED BELOW. PUT IT INTO ALL ENTRIES OF COLUMN
C  NVAR+1 OF CORDEN.
 
C  NOTE: SINCE THE APRIORI DENSITY IS UNIFORM, ITS VALUES ARE
C        THE SAME REGARDLESS OF THE ACTUAL GRID POINTS USED (I.E., ONLY
C        THE BOUNDARIES OF THE RECTANGULAR GRID SPACE, [AB(I,1),AB(I,2)]
C        FOR I=1,NVAR ARE IMPORTANT).
 
C  ESTABLISH MATRIX CORDEN TO HAVE ALL THE GRID POINTS AND ASSOCIATED
C  DENSITY VALUES (FOR ROW K, THE 1ST NVAR VALUES = COORDINATES OF GRID
C  POINT K; LAST VALUE = ASSOCIATED DENSITY OF GRID POINT K).

C  AFTER THE DO 30 LOOP, SUBROUTINE CALGRD (WHICH GIVES THE COORDINATES
C  OF A DESIRED POINT) WILL NOT HAVE TO BE CALLED AGAIN, SINCE ALL THE
C  INFORMATION WILL BE STORED INTO CORDEN, WHICH WILL BE CONDENSED
C  AFTER EACH CYCLE TO INCLUDE ONLY THE 'HIGH PROBABILITY' POINTS.
 
	DO 30 IG = 1,NGRID
 


	  CORDEN(IG,NVAR+1)=CONST
 
C  GET THE COORDINATES OF THE IGTH GRID POINT.
 

C  AS OF npbig1.f, THE GRID POINTS ARE FOUND DIRECTLY FROM
C  SUBROUTINE CALGRD (I.E., ROUTINE GETCOF IS NO LONGER USED).
 
C  CALL CALGRD AND GET THE IGTH COORDINATES; THEN PUT THEM INTO THE
C  1ST NVAR ENTRIES OF ROW IG OF CORDEN. NOTE THAT THE 1ST TIME CALGRD
C  IS CALLED, A VARIABLE (FIRST) IS INITIALIZED, AND THEN EACH
C  SUCCEEDING TIME IT IS CALLED, CALGRD 'KNOWS' TO CONTINUE FINDING
C  THE SEQUENCE OF GRID POINTS.
 
	CALL CALGRD(NVAR,NGRID,AB,X,ERRFIL)
C     1     S,QS,COEF,RQS,NEXTN,TESTN,HISUM)
	DO J=1,NVAR
	  CORDEN(IG,J) = X(J)
	END DO
	
   30   CONTINUE
 
 
C  INITIALIZE NACTVE (THE NO. OF ACTIVE GRID POINTS) TO = NGRID (WHEN

C  A PRIOR DENSITY IS USED, NACTVE IS READ IN).
 
	NACTVE=NGRID
 
 
	ENDIF
 
C  THE ABOVE ENDIF IS FOR THE  IF(ICYCLE .EQ. 0)  CONDITION.
 

C  AS OF npageng19.f, PRESET NACTLAST TO BE NACTVE. THIS WAY, IN THE
C  UNLIKELY EVENT THAT THE FIRST CYCLE OF A RUN HAS A HESSIAN ERROR
C  (WHICH MEANS THAT WHEN CONTROL COMES BACK TO MAIN FROM SUBROUTINE
C  emint, IT IS TRANSFERRED TO LABEL 900 AND THEREFORE SKIPS THE
C  cbegin statistics  SECTION WHERE NACTLAST = NACTVE IS SET), THERE
C  WON'T BE A PROBLEM WHEN NACTVE IS SET = NACTLAST JUST BELOW LABEL 
C  900. IN PREVIOUS PROGRAMS, IN THE ABOVE SITUATION, BELOW LABEL 900,
C  NACTVE = NACTLAST WOULD RESULT IN NACTVE BEING SET = 0 SINCE 
C  NACTLAST WAS UNITIALIZED.

      NACTLAST = NACTVE 


        prefobj=-1.d30
        prebig=-1.d30

 
C  THE 2ND LINE GIVES THE VERSION NO. OF THE PROGRAM ITSELF.

        WRITE(25,1657)
 1657   FORMAT(1X,'VERSION 43  ... Made by npagranfix6')

        WRITE(25,7123) 'VER_BAK OCT_15'
 7123   FORMAT(A14)


        WRITE(25,1212)
 1212   FORMAT(//' THE NEXT FEW LINES GIVE INPUT INFO FOR THIS RUN: '/)

	WRITE(25,9761) PREFIX,EXT
 9761   FORMAT(/' THE SUBJ. FILENAMES (IN "Adapt-Like" FORMAT) HAVE'/
     1' PREFIX ',A5,' AND EXT. ',A3)


	WRITE(25,9767) NSUBTOT
 9767   FORMAT(/' THE TOTAL NO. OF SUBJECTS IN THE DATA SET IS ',I4)
	WRITE(25,9762) NSUB
 9762   FORMAT(/' THE NO. OF SUBJECTS IS          ',I3)

C  CALL SUBROUTINE WRITEPT2 TO WRITE THE PATIENT NOS. TO BE USED IN 
C  THE ANALYSIS TO FILE 25 ... IN AN "EFFICIENT" WAY, AS OPPOSED TO
C  ONE INDEX PER LINE. NOTE THAT THE FIRST ARGUMENT TELLS WRITEPT2
C  TO WRITE TO FILE 25. NOTE THAT THE '    0' AFTER THE CALL TO
C  WRITEPT2 TELLS THE PROGRAM READING THIS FILE THAT THE PATIENT
C  NOS. HAVE ENDED.

	CALL WRITEPT2(25,NSUB,IPATVEC)
	WRITE(25,*) '    0'
 
	WRITE(25,9763)

 9763   FORMAT(/' THE RANDOM VARIABLES AND THEIR RANGES ARE: ')
	
C  REPLACE WRITING OF AB() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL. SIMIARLY FOR ALL CALLS TO VERIFYVAL.

      DO I=1,NVAR
       XVERIFY(1) = AB(I,1)
       XVERIFY(2) = AB(I,2)
       CALL VERIFYVAL(2,XVERIFY)

C      WRITE(25,1217) PAR(I),AB(I,1),AB(I,2)
       WRITE(25,1217) PAR(I),XVERIFY(1),XVERIFY(2)
	END DO
 1217   FORMAT(/' ',A11,': ',G17.10,'   TO   ',G17.10)
 
  	IF(NOFIX .EQ. 0) WRITE(25,9764)
 9764   FORMAT(/' NO FIXED PARAMETER VALUES.')

 
  	IF(NOFIX .GT. 0) THEN
 
       WRITE(25,9766)
 9766  FORMAT(/' THE USER-ENTERED FIXED PARAMETER VALUE(S) IS (ARE):')
 
C  REPLACE WRITING OF VALFIX() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL.

      DO I=1,NOFIX
       XVERIFY(1) = VALFIX(I)
       CALL VERIFYVAL(1,XVERIFY)
C      WRITE(25,1219) PARFIX(I),VALFIX(I)
       WRITE(25,1219) PARFIX(I),XVERIFY(1)
	END DO

 1219   FORMAT(/' ',A11,' =  ',G17.10)
	WRITE(25,*)
 
  	ENDIF

      IF(NRANFIX .EQ. 0) WRITE(25,9789)
 9789 FORMAT(/' NO "RANFIX" PARAMETER ESTIMATES.')


  	IF(NRANFIX.GT. 0) THEN
 
       WRITE(25,9768)
 9768  FORMAT(/' THE USER-ENTERED INITIAL ESTIMATES FOR THE PARAMETERS'/
     1' WHICH ARE UNKNOWN, BUT THE SAME FOR ALL SUBJECTS, IS (ARE):')

C  REPLACE WRITING OF RANFIXEST() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL.
 
      DO I=1,NRANFIX
       XVERIFY(1) = RANFIXEST(I)
       CALL VERIFYVAL(1,XVERIFY)
C      WRITE(25,1219) PARRANFIX(I),RANFIXEST(I)
       WRITE(25,1219) PARRANFIX(I),XVERIFY(1)
	END DO
	WRITE(25,*)
 
  	ENDIF


C  AS OF bignpaglap4.f, NGRID is written on the line following the
c  text.

	WRITE(25,9869) NGRID
 9869   FORMAT(/' THE NO. OF GRID POINTS IS '/,I10)

	WRITE(25,*)' THE NO. OF CURRENTLY ACTIVE GRID POINTS IS ',NACTVE
 


C  REPLACE WRITING OF C0P(),...,C3P() WITH XVERIFY (SEE LOGIC IN 
C  SUBROUTINE VERIFYVAL.

	WRITE(25,2217) NUMEQT
 2217   FORMAT(/' THE POPULATION VALUES FOR [C0,C1,C2,C3] FOR EACH OF '/
     1' THE ',I2,' OUTPUT EQUATION(S), ARE SHOWN BELOW: '/)
      DO IEQ = 1,NUMEQT
       XVERIFY(1) = C0P(IEQ)
       XVERIFY(2) = C1P(IEQ)
       XVERIFY(3) = C2P(IEQ)
       XVERIFY(4) = C3P(IEQ)
       XVERIFY(5) = C4P(IEQ)
       XVERIFY(6) = C5P(IEQ)
       CALL VERIFYVAL(6,XVERIFY)
       WRITE(25,162) IEQ,(XVERIFY(IXV),IXV=1,6)
       C4(IEQ) = C4P(IEQ)
       C5(IEQ) = C5P(IEQ)
       write (*,*) IEQ,"C4,C5=",c4(ieq),c5(ieq)
      END DO

  162   FORMAT(' EQ. ',I2,': ',6(G16.10,1X))
C
C wmy2019.01.16 In above DO IEQ = 1,NUMEQT, C4P and C5P are added to
C    OUT<N> file.  Also, C4 = C4P and C5 = C5P.  C4 and C5
C    tell the program that these YO are log10(measure) and ~Poisson,
C    respectively.  Thus, C4 and C5, apply to all YO. 

	WRITE(25,2112)
 2112   FORMAT(/' YOU HAVE CHOSEN TO MODEL THE ASSAY ERROR FUNCTION,'/
     1' S.D. AS FOLLOWS (ASSUMING SD1 = C0+C1*Y+C2*Y**2+C3*Y**3):')

C  REPLACE WRITING OF GAMLAM0 WITH XVERIFY (SEE LOGIC IN 
C  SUBROUTINE VERIFYVAL.



     	 IF(IERRMOD .EQ. 1) WRITE(25,2113)
       XVERIFY(1) = GAMLAM0
       CALL VERIFYVAL(1,XVERIFY)


C      IF(IERRMOD .EQ. 2) WRITE(25,2114) GAMLAM0
C      IF(IERRMOD .EQ. 3) WRITE(25,2116) GAMLAM0
C      IF(IERRMOD .EQ. 4) WRITE(25,2117) GAMLAM0
       IF(IERRMOD .EQ. 2) WRITE(25,2114) XVERIFY(1)
       IF(IERRMOD .EQ. 3) WRITE(25,2116) XVERIFY(1)
       IF(IERRMOD .EQ. 4) WRITE(25,2117) XVERIFY(1)


 2113   FORMAT(/' S.D. = SD1')
 2114   FORMAT(/' S.D. = GAMMA*SD1, WITH GAMMA TO BE ESTIMATED, AND'/ 
     1'                  THE INITIAL GAMMA ESTIMATE TO BE ',G16.10)
 2116   FORMAT(/' S.D. = SQRT(SD1**2 + LAMBDA**2), WITH LAMBDA TO BE EST
     1IMATED'/
     2'                  AND THE INITIAL LAMBDA ESTIMATE TO BE ',G16.10)
 2117   FORMAT(/' S.D. = GAMMA, WITH GAMMA TO BE ESTIMATED, AND'/
     1'                   THE INITIAL GAMMA ESTIMATE TO BE ',G16.10)


        WRITE(25,1331) NDRUG
 1331   FORMAT(/' THE NO. OF DRUGS IS '/
     1' ',I2)
        WRITE(25,1329) NDRUG
 1329   FORMAT(/' THE ACTIVE (SALT) FRACTION(S) FOR THE ',I2,' DRUG(S)'/
     1' FOLLOW, IN ORDER: ')
        WRITE(25,*) (AF(I),I=1,NDRUG)

        WRITE(25,1221) ICYCLE+1
 1221   FORMAT(/' THE STARTING CYCLE NO. FOR THIS RUN IS ',I6)
        WRITE(25,1222) MAXCYC
 1222   FORMAT(/' THE LAST CYCLE NO. WILL BE .LE. ',I6)
        WRITE(25,1223) TOLC
 1223   FORMAT(//' BUT THE ANALYSIS WILL STOP BEFORE THE MAX. NO. OF'/
     1' CYCLES HAVE BEEN RUN IF CONVERGENCE IS ACHIEVED, AND'/
     2' CONVERGENCE IS ACHIEVED BASED ON LOGIC AND PRE-SET TOLERANCE'/
     3' PARAMETERS WHICH ARE APPROPRIATE FOR THE "INTERIOR POINT '/
     4' ALGORITHM" USED BY THIS PROGRAM. THIS ALGORITM WAS DEVELOPED'/
     5' BY JIM BURKE AT THE UNIVERSITY OF WASHINGTON, AND ADAPTED FOR'/
     6' THIS PROGRAM BY BOB LEARY.'//
     7' THE CYCLE TOL. PARAMETER SET BY THE USER IS ',F20.17/

     8' FOR CONVERGENCE, THE LOG-LIK BETWEEN 2 CONSECUTIVE "MAJOR CYCLES
     9"'/
     1' MUST BE .LE. THIS VALUE.') 

        WRITE(25,9769) 
 9769   FORMAT(/' THE TOLERANCE PARAMETER USED BY THE DIFFERENTIAL'/
     1' EQUATION SOLVER (VODE), IS: ')
        WRITE(25,*) RTOL
 
        IF(ICYCLE .GE. 1) WRITE(25,9771) PRIFIL2
 9771   FORMAT(/' THE APRIORI JOINT DENSITY IS FROM FILE ',A20)

        IF(ICYCLE .EQ. 0) WRITE(25,9772)
 9772   FORMAT(/' THE APRIORI JOINT DENSITY IS UNIFORM.')
 
	WRITE(25,*)
	WRITE(25,*)
	WRITE(25,*)' **************************************************'
	WRITE(25,*)' **************************************************'
 
	WRITE(25,1213)
 1213   FORMAT(///' THE FOLLOWING IS THE OUTPUT FROM THE PROGRAM.'///)

C  SET LASTCYC = LAST CYCLE NO. WHICH WAS PRINTED TO THE OUTPUT FILE
C  AND TO THE SCREEN. THIS IS NEEDED SINCE OTHERWISE ICYCLE IS PRINTED
C  OUT 3 TIMES (SEE FORMATS 1237 AND 1239) IF ierrmod .GE. 2. SEE THE 
C  NEW CODE WHICH RETURNS CONTROL TO 10001 TO RUN DIFFERENT 
C  "gammaplus/minus eps tries". THEN ONLY PRINT THE CYCLE NO. IF IT
C  HASN'T BEEN PRINTED BEFORE.

	LASTCYC = ICYCLE
 
C  CORDEN HOLDS, IN ITS FIRST NACTVE ROWS, THE STARTING JOINT DENSITY

C  AND COORDINATE VALUES. FOR K=1,NACTVE, CORDEN(K,J) = JTH COORDINATE
C  OF THE KTH ACTIVE POINT, J=1,NVAR; AND CORDEN(K,NVAR+1) IS THE
C  ASSOCIATED DENSITY FOR THE KTH ACTIVE POINT.
 
C  IF ICYCLE .GT. 0, CORDEN WAS READ IN.
C  IF ICYCLE = 0, NACTVE=NGRID, AND CORDEN WAS FILLED AT LABEL 30 ABOVE.
C		 IN THIS CASE, THE DENSITY IS UNIFORM, SO ALL
C		 CORDEN(K,NVAR+1) VALUES = 1/VOLSPA, K=1,NACTVE.
 
 
C	IPRED=11 + ICYCLE
C	JCOL=0
	ITEST=0
 
C  IPRED IS THE CYCLE NO. WHERE THE NEXT 2-CYCLE PREDICTION
C  ALGORITHM STARTS (IT IS NO LONGER USED). JCOL = COLUMN NO. OF DENSTOR
C  IN WHICH IS STORED THE DENSITY OF ONE OF THE 2-CYCLES USED IN THE
C  PREDICTION (IT IS NO LONGER USED). IT IS SET = 0 ABOVE, SINCE NO 

C  STORAGE IS REQUIRED UNTIL CYCLE NO. 11


C  (SEE BELOW). ITEST=0 --> THE NEXT CYCLE IS NOT (INITIALIZED) TO BE
C  A TEST CYCLE (SEE CODE BELOW WHEN ITEST=1,2, OR 3).
 
C    NEW FOR m2_13cal.f: NSTORE SET = 0. NSTORE IS THE NO. OF GRID
C    POINTS, WHOSE P(YJ|X) VALUES HAVE BEEN STORED IN PYJGX IN LOOP 800.
C    THIS NO. CAN BE CHANGED BY THE 'CONDENSING' CODE BELOW, SINCE
C    INACTIVE POINTS ARE THROWN OUT.
 
	NSTORE=0
cadapt    initialize grid resoution to 20%
          resolve=0.20000000298023224
          rpar(k_resolve) = resolve

c  As of bigmlt1.f, the program checks to see if MAXCYC = 0. If so, no
c  cycle calculations are done. Instead control transfers to the end 
c  of the cycle calculations to pick up the means, medians, and modes

c  of the read-in density (which are needed to calculate YPREDPOP AND
c  YPREDPOPT), and then to label 900 to calculate the output files.

	IF(MAXCYC .EQ. 0) THEN
	 WRITE(*,2123) NSUB
	 WRITE(25,2123) NSUB
 2123    FORMAT(/' SINCE MAXCYC = 0, THIS RUN DOES NOT CONDUCT ANY'/
     1' NPAG ITERATIONS. INSTEAD, THE END-OF-THE RUN CALCULATIONS WILL'/
     2' TAKE PLACE WITH THE "FINAL CYCLE JOINT DENSITY" = THE PRIOR'/
     3' JOINT DENSITY READ IN, AND WITH THE ',I5,' PATIENT DATA FILES'/
     3' INPUT TO THIS PROGRAM.'//
     4' NOTE THAT THE FOLLOWING VALUES, DOWN TO THE BAYESIAN '/
     5' POSTERIOR DENSITY SECTION, ARE FOR THE APRIORI DENSITY, AND'/
     6' ARE ONLY INCLUDED BECAUSE THE PC PREP PROGRAM WHICH READS'/
     7' IN THIS OUTPUT FILE, EXPECTS TO SEE THEM.'//)
	 GO TO 2100
	ENDIF

! NEW PARALLEL CODE BELOW AS OF npageng28.f.
      CALL SYMBOL(NBCOMP)  
   
 
 1001 ICYCLE=ICYCLE+1

        IPAR(i_cycle) = ICYCLE

C  Note that the program will open and read CHMAXCYC.OLD at the 
c  beginning of each new cycle. If it reads a 1, it will continue the 
c  calculations as before. If it reads a 0, it will change the value of 
c  MAXCYC to whatever ICYCLE is currently, which will cause the program 
c  to halt at the end of that cycle just as if MAXCYC had been set
c  to the changed value originally. If this happens, the program will
c  write a comment to the console and file 25 of why the program
c  has stopped prematurely.

	OPEN(37,FILE='CHMAXCYC.OLD')
	 READ(37,*) ICONTIN
	CLOSE(37)

	IF(ICONTIN .EQ. 0) THEN
	 MAXCYC0 = MAXCYC
	 MAXCYC = ICYCLE
	ENDIF


cgam3
10001 continue

c above is new entry point for gammaplus/minus eps tries
      itest = 0
 
 
cadapt  reset number of stored points to that before expansion
c     nstore=nstoresv
 
C+++++++++++++++ CODE ABOVE TO USE ONLY HIGH PROB POINTS ++++++++++++++
C  PRINT THE CYCLE NO. ONLY IF THIS CYCLE IS CERTAIN TO BE RUN (I.E.,
C  IF ITEST = 0). SUBROUTINE PRNTOP PRINTS THE CYCLE NO., ETC. FOR
C  ACCELERATED CYCLES.
 
	IF(ITEST .EQ. 0 .AND. ICYCLE .NE. LASTCYC) THEN
	 WRITE(*,1237) ICYCLE
	 WRITE(25,1239) ICYCLE
	 LASTCYC = ICYCLE
	ENDIF
 
 1237   FORMAT(///' CYCLE NO.',I5,'. SUBJECT NOS FOLLOW: ')
 1239   FORMAT(///' CYCLE NO.',I5,/)
	
 

C  ICYCLE IS THE NUMBER OF THE NEXT CYCLE TO BE RUN.
C
C  THIS IS WHERE EACH NEW CYCLE STARTS (FOR EACH CYCLE, THE DENSITY OF
C  X IS UPDATED FROM THE PREVIOUS DENSITY ESTIMATE, USING THE
C  OBSERVED SUBJECT DATA FROM THE INPUT DATA FILES WHICH ARE PASSED TO
C  SUBROUTINE IDPC BELOW.
C
C  ZERO OUT SPXGYJ. AFTER THE SUBJECT LOOP
C  (DO 1000), IT WILL BE THE VECTOR OF LENGTH NACTVE WHOSE ITH ELEMENT
C  IS THE SUM(P(X=X(I)|Y(J))) OVER J=1,NSUB, WHERE X = NVAR-
C  DIMENSIONAL. ALSO, ZERO OUT SLPYJ. AFTER THE SUBJECT LOOP, IT
C  WILL BE THE SCALAR SUM(LN(P(Y(J)))) OVER J=1,NSUB.

 
 

      DO 55 I=1,NACTVE
   55 SPXGYJ(I)=0.D0
!      SLPYJ=0.D0
C  COMMENTED OUT SLPYJ = 0 AS OF npageng28.f

 
C  START THE SUBJECT LOOP.
 
 

C  REWIND SCRATCH FILE 27 WHICH HAS ALL THE SUBJECT DATA FILES
C  CONCATENATED ON IT, IN ORDER.
 
	REWIND(27)
 



C  NOBTOT WILL BE THE RUNNING TOTAL OF ALL NON-MISSING OBSERVED VALUES
C  OVER ALL THE NSUB SUBJECTS. THIS IS NEEDED TO CALCULATE BIC BELOW.

        NOBTOT = 0

      NInDO1000 = 0
      IPAR(i_do) = 1000
      DO 1000 JSUB=1,NSUB

        write (*,*)
        write(*,*) "*********** DO 1000 *********** SUBJECT", JSUB
     1    , "of", NSUB, "in cycle", ICYCLE
        write (*,*)

        NInDO1000 = NInDO1000 + 1
        PInDO1000 = 100.0 * real(NInDO1000) / real(NSUB)

C  CALL SUBROUTINE FILRED TO READ, FOR THIS SUBJECT, FROM SCRATCH FILE
C  27, THE NO. OF OBSERVATION TIMES (NOBSER) AS WELL AS THE
C  OBSERVED VALUES THEMSELVES: YO(I,J) = THE 'NOISY' OBSERVED VALUES
C  FOR THIS SUBJECT; I=1,NOBSER, J=1,NUMEQT. THESE OBSERVED VALUES ARE
C  USED ONLY TO CALCULATE THE ASSAY STANDARD DEVIATIONS (USING THE
C  VECTORS, C0,C1,C2,C3, WHICH ARE ALSO READ IN). THE REST OF THE INFO
C  IN THE SUBJECT DATA FILE IS PASSED IN COMMONS TO THE IDPC MODULE
C  SUBROUTINES.
 
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,
     1    TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     2    INTLIST,ERRFILNAME)

C        write (*,*) "Ret. from FILRED() nr. #3675"

        call cp_lrcs_to_rpar(gamma,flat,numeqt,c0,c1,c2,c3,c4,c5,rpar)
!         RPAR(k_gamma) = gamma
!         do III=1,numeq
!           RPAR(k_c0_base + III) = C0(III)
!           RPAR(k_c1_base + III) = C1(III)
!           RPAR(k_c2_base + III) = C2(III)
!           RPAR(k_c3_base + III) = C3(III) wmy2018.01.16 added C4 and C5 to list
!        end do

C Verify that NUMEQT <= 7
        if (NUMEQT.gt.7) then
           write (*,*) "WARNING :: NUMEQT.gt.7",
     1        NUMEQT
           CALL PAUSE
           STOP
        end if

C wmy2017Sep29
C        do III=1,2
C        write (*,*) "JSUB->FILRED",
C     1    RSCOPY(III,1), RSCOPY(III,2), RSCOPY(III,3),
C     2    RSCOPY(III,4), RSCOPY(III,5), RSCOPY(III,6),
C     3    RSCOPY(III,7), "Continue to DO 800"
C        end do
C in above debug statement: the greco model has 1 drug and
C 5 additional covariates, COMMON/CONST2/ND,NADD  -- thus
C NI = 2*ND+NADD = 7. I don't have access to these variables 
C here, in NPAG. The 2 is because there are only 2 input
C lines.


C  FIND THE ASSAY STANDARD DEVIATIONS FOR THIS SUBJECT. FOR EACH
C  OF THE NOBSER*NUMEQT OBSERVED VALUES (EXCEPT THAT YO(I,J) = -99 -->
C  OUTPUT EQ. J HAS NO OBSERVED LEVEL FOR OBSERVATION TIME I),
C  Y, SIG = C0 + C1*Y + C2*Y**2 + C3*Y**3.
C  NOTE THAT, THEORETICALLY, SIG SHOULD BE A CUBIC FNT. OF
C  THE 'TRUE' OBSERVED VALUES, NOT THE 'NOISY' OBSERVED VALUES (BUT THE

C  'TRUE' VALUES ARE UNKNOWN).
 
C  ALSO, CALCULATE SIGFAC, THE PRODUCT OF THE NON-MISSING STD. DEV.'S

C  (A NON-MISSING S.D. IS ONE FOR WHICH THE CORRESPONDING YO(I,J) IS
C  .NE. -99, THE MISSING VALUE CODE).
C  INITIALIZE SIGFAC=1, AND THEN UPDATE IT FOR EACH NON-MISSING
C  OBSERVATION.
 
C  MISVAL WILL BE THE RUNNING TOTAL OF MISSING VALUES AMONG ALL THE
C  NUMEQT x NOBSER POTENTIAL OBSERVED LEVELS.

        UseInlineDO140 = 1
        if (UseInlineDO140.eq.1) then

	MISVAL = 0
        NNORMALOBS = 0
        NPOISSONOBS = 0   
        SIGFAC=1.D0
        OFAC=0.D0
 
 	DO 140 I=1,NOBSER
 	 DO 140 J=1,NUMEQT
 
	  Y = YO(I,J)
 
C  IF Y = -99, IT MEANS THAT OUTPUT EQ. J HAD NO VALUE AT OBSERVATION
C  TIME I. IN THIS CASE, IGNORE THIS Y AND INCREASE MISVAL BY 1.
 

	  IF(Y .EQ. -99) THEN
	   MISVAL = MISVAL+1
	   GO TO 140
	  ENDIF

C wmy2019.01.18 -- observation transformation is independent of distribution type
C Are observations recorded as log10(obs) _AND_ in subroutine
C output, are X converted to log10(X)? If so, do you want the
C sd to be calculated on 10^Y(obs or est)? then = 10.
C             if (C1(J).eq.-10) IPAR(i_is_log10+J) = -10
             if (C4(J).eq.10) IPAR(i_is_log10+J) = 10

C wmy2019.01.17 Added distribution flag
C          if (C0(J).eq.-229.and.C2(J).eq.-229
C     1   .and.C3(J).eq.-229) then 
           if (C5(J).eq.229) then
C--------------------------------- Start Poisson

             if (ICYCLE.lt.2) then
               write (*,*) "Poisson analysis req. for OUTEQ",J
             endif

             NPOISSONOBS=NPOISSONOBS+1
             ObsError(I,J)=1.D0
             SIG(I,J)=1.D0
             IPAR(i_is_poisson_obs+J)=229
C--------------------------------- End Poisson
          else
C--------------------------------- Start NORMAL

C  NOTE: FOR EACH SUBJECT, MUST ENSURE THAT ALL THE STD DEV'S ARE NON-
C        ZERO. OTHERWISE, THE PROGRAM WILL BLOW UP! THIS IS BECAUSE
C        P(YJ|X) INVOLVES SQUARED DIFFERNCES BETWEEN OBSERVED Y'S AND
C        EXPECTED Y'S (FOR EACH X GRID POINT)...EACH DIFFERENCE
C        NORMALIZED (I.E., DIVIDED) BY THE VARIANCE OF THE RESPECTED
C        OBSERSATION.
 
C 	 SEE M2_17CAL.F CODE FOR COMMENTS ON HOW A STD. DEV. COULD = 0.
 
C  ALSO TEST TO MAKE SURE NO STD. DEV. < 0, SINCE SIGFAC BEING NEGATIVE
C  WOULD RESULT IN A NEGATIVE PROBABILITY (SEE PYJGX CALCULATION BELOW).

      SIG(I,J) = C0(J)+C1(J)*Y+C2(J)*Y*Y+C3(J)*Y**3
cgam4
      if(ierrmod.eq.2) sig(i,j) = sig(i,j)*gamma
      if(ierrmod.eq.3) sig(i,j)=dsqrt(sig(i,j)**2 + gamma**2)
      if(ierrmod.eq.4) sig(i,j) = gamma*flat


      IF(SIG(I,J) .EQ. 0) THEN
                write (*,*) "SUB(I,J) w/SD<0",JSUB,I,J
		WRITE(*,2345) JSUB, I, J
		WRITE(25,2345) JSUB, I, J
2345            FORMAT(//' A S.D. IS 0 FOR JSUB = ',I5,'. RERUN THE '/
     1' PROGRAM WITH C0 NOT = 0  FOR THIS SUBJECT, OR WITH THIS'/
     2' SUBJECT ELIMINATED.', I5, I5)
	   	CLOSE(27)
	   	CLOSE(25)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,2345) JSUB
        CLOSE(42)

		CALL PAUSE
       	STOP
      ENDIF

C wmy2018.06.26 Not sure why this is here; the test inside of the
C  Normal initialization above should be all that is necessary.
C 
C      IF(SIG(I,J) .LT. 0) THEN
C                WRITE(*,2346) JSUB
C                WRITE(25,2346) JSUB
 2346            FORMAT(//' A S.D. < 0 FOR JSUB = ',I5,'. RERUN THE '/
     1' PROGRAM WITH A BETTER CHOICE FOR THE ASSAY ERROR POLYNOMIAL'/
     2' COEFFICIENTS.')
C               CLOSE(27)
C               CLOSE(25)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

C        OPEN(42,FILE=ERRFIL)
C         WRITE(42,2346) JSUB
C        CLOSE(42)
C
C               CALL PAUSE
C               STOP
C      ENDIF

 
C wmy2017Dec28 SIG is in the un-named COMMON; This can cause issues
c  inside the DO 800 loop; so I'm copying the stdev to ObsError
c
      ObsError(I,J) = SIG(I,J)

      SIGFAC=SIGFAC*SIG(I,J)

C--------------------------------- END NORMAL
       endif
 
  140 CONTINUE


C        write (*,*) "Passed DO 140 For", JSUB

C  NOTE THAT SIGFAC WAS CALCULATED IN LOOP 140 ABOVE, AND THAT OFAC IS
C  NOW THE RESULT OF (NOBSER*NUMEQT - MISVAL - NPOISSONOBS) VALUES.

C wmy -- eqns updated to include Poisson  measures; and note that
C  there is no error checking on Poisson measures! But I think we
C  can use C1(J) for that, so I left it out. Maybe this can be
C  the allowable %difference between predicted and observed? else
C  set IPAR(i_skip_ig) to 0.

        OFAC=2.506628274631**(NOBSER*NUMEQT - MISVAL - NPOISSONOBS)

        NOBTOT = NOBTOT + NOBSER*NUMEQT - MISVAL

        RPAR(k_sfac) = SIGFAC
        RPAR(k_ofac) = OFAC
 
C  NOTE THAT 2.5066... = SQRT(2*PI).

C       else CALL DO140(ijob = 0,...); contained in npag_utils.mod; 
C          but I think this is now unnecessary as ObsError() should
C          be used throughout the code, and ObsError will be
C          calculated later, but prior to calculating PYJGX.

        endif
C above "if (UseInlineDO140)" can be removed if no problems
C   arise from it's not being run above. Otherwise, call DO140()
C   from npag_utils.
C   wmy20190731 -- benchRun001c DEN and ILOG match 
 
C  FOR EACH SUBJECT, AND EACH GRID POINT, CALL IDPC, A SUBROUTINIZED
C  VERSION OF THE ADAPT PROGRAM ID3 TO CALCULATE THE SUM OF SQUARES OF
C  DIFFERENCES BETWEEN THE OBSERVED VALUES AND PREDICTED (BY THE MODEL)
C  VALUES (NORMALIZED BY THE ASSAY VARIANCE OF EACH OBSERVATION) ...
C  BUT THIS NEEDS TO BE DONE JUST ONCE (ICYCLE=IFIRST), BECAUSE THE
C  VALUES P(JSUB|IG), FOR SUBJECT JSUB AND GRID PT. IG, DON'T CHANGE
C  CYCLE - TO - CYCLE, AND CAN THEREFORE BE STORED INTO PYJGX(JSUB,IG).
 
 8888   FORMAT(' ',' CYCLE ',I5,',  SUBJECT ',I5,' ...  % COMPLETED = ',

     1F8.2)
	XNEXT = 1.D0


C wmy2017Sep21 -- Note that ALL COMMON blocks are going to have 
C   unexpected behavior inside of the !$omp parallel region --
C   So make sure all COMMON blocks inside of parallel regions
C   are declared Threadprivate (if that works, great!); but
C   it's even better if we just don't use them at all: 
C   pass variables down to the SRs and funcs that need them.
C   Alternatively, some people say to make them into modules.
C  
C	COMMON/TOCALC/IRAN,PX,NOFIX,NSUB,gamma,flat,AB
C
C wmy 2017Sep08 Copy the stored values of PYJGX to work, here.
C   this is a separate DO loop preceding DO 800 because it
C   can't be a parallelized due to all of the shared memory
C   Original code looked this this
C      DO 800 IG=1,NACTVE
C	IF(IG .LE. NSTORE) GO TO 700
C    700 (This code) 
C    800 Continue
C
      if (ICYCLE .ge. 2) then

        if (NSTORE .le. NACTVE) then

          DO 750  IG=1,NSTORE

            WORK(IG)=PYJGX(JSUB,IG)*CORDEN(IG,NVAR+1)

	    WORKK(IG) = PYJGX(JSUB,IG)

  750     Continue

        endif

      endif
C
C wmy2017Sep08 ... I moved the above block out of the DO 800 loop
C  Thus, the Iterations for the !$omp do are changed as follows.
C
      if (ICYCLE .eq. 1) then
        IterFirst = 1
      else
        IterFirst = NSTORE + 1
      endif
      IterLast = NACTVE
      if ( IterLast > MAXACT ) IterLast = MAXACT + 1
c
c wmy2017Nov02
      if (IterLast < 1) write (*,*) "IterLast < 1",
     1   "check INDPTS in (1...6;101,102,...)"
C
C Also, inside the DO 800 loop, there is no reason now to check
C if IG goes above MAXACT+1 to exit loop, just require the
C last iteration to be the lesser of MAXACT+1 or NACTVE
C

c wmy2017Oct25 -- COMMON blocks have "random" (unexpected) behavior
c inside the parallel region for all threads other than master. So,
c for "shared" read only variables, as opposed to working variables
c (see below), make a FirstPrivate variable, e.g. IRANCOPY, that is
c passed to each subroutine.  Initialize these "shared" variables
c immediately before entering the parallel region.
        do iparam=1,max_ODE_params
           IRANCOPY(iparam) = IRAN(iparam)
        end do
        NOFIXCOPY = NOFIX
c-----------------------------------------------------------------------
C      write (*,*) "START DO 800 :: First and Last Support are",
C     1 IterFirst, IterLast, "Gamma = ", gamma
       NInDO800=0
       NBadInDO800=0
       NNInDO800=99
       PInDO800=0.0
       IPAR(i_skip_ig)=1
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp Parallel Default(PRIVATE)
!$omp&Shared(PYJGX,WORKK,WORK,NInDO800,NNInDO800,CORDEN,NBadInDO800)
!$omp&CopyIn(ObsError,IPAR,RPAR,INTLIST)
!$omp&FirstPrivate(JSUB,SIGFAC,OFAC,NOBSER,NUMEQT,PInDO1000,NDIM,MF)
!$omp&FirstPrivate(IterFirst,IterLast,NSTORE,NACTVE,MAXACT,RTOL,ATOL)
!$omp&FirstPrivate(VALFIX,RANFIXEST,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY)
!$omp&FirstPrivate(NVAR,NRANFIX,IRANCOPY,NOFIXCOPY,NBCOMP)
c !$omp&num_threads(2)
c !$omp&Shared(MAXTHREAD)
c -----------
c  !$omp&private(ThreadNo) ! By Default
c  !$omp&CopyIn(/OBSER/,/PARAMD/,/INPUT/) ! removed these COMMON blocks 9/2019

c These COMMON blocks are still defined AND used by the analytic
c routines. So IDPC must receive copies of them (see above, and below
c comments).
c     COMMON/PARAMD/P -> PCOPY
c     COMMON/INPUT/ R,B -> RCOPY, BCOPY
c     COMMON/TOCALC/IRAN,PX,NOFIX,NSUB,gamma,flat,AB -> IRANCOPY, etc.

c COMMON blocks will have unexpected behavior inside the parallel region.
c "shared" read only variables are initialized before the parallel region
c and values are assigned to temporary FirstPrivate copies of those variables
c to be used inside the parallel region (see above) -- but for local
c "working" variables, copies must still be made, e.g. PCOPY, but the
c variable is a PRIVATE variable -- and assignment is first to 0.0, near
c the top of the parallel region and then identical to serial program:
c INSIDE the DO 800 loop by calls to functions or subroutines or by
c calculation, as is appropriate for the variable.

!$omp Do
c
c !$omp ThreadPrivate(/TOCALC/,/OBSER/)
C wmy2017Sep10 /TOCALC/ is common block containing PX; I added
C   an explicit declaration of  REAL*8 W to SUBROUTINE NPAG. W should be
C   created w/random value, but is initialized prior to use.
c
C wmy2017Sep08
C   DO is over less IG now
C       DO 800 IG=1,NACTVE
        DO 800 IG=IterFirst,IterLast


C            write (*,*) "Literally, just entered DO 800"


!$omp critical
           IPAR(i_skip_ig) = 1
           NInDO800 = NInDO800 + 1
           NNInDO800 = NNInDO800 + 1
           if (NNInDO800.eq.100) then
             PInDO800 = 100.0*REAL(NInDO800)
     1        /(REAL(IterLast) - REAL(IterFirst))

C  PRINT TO THE SCREEN THE UPDATE ON WHAT % OF GRID POINTS HAVE BEEN
C  CALCULATED IF NACTVE > NSTORE (I.E., IF NACTVE .LE. NSTORE -->
C  ALL P(YJ|X)'s ARE ALREADY STORED INTO PYJGX AND SO THIS 8OO LOOP
C  WILL GO VERY FAST.

C wmy2017Sep29 -- correct to here ... check if it gets to IDPC
c        do III=1,2
C        write (*,*)
C        write(*,*) PInDO1000, "%     --- DO 800 ------- Obs:", JSUB,
C     1    "Support:", IG, "of", IterFirst, "to", IterLast, PInDO800,"%"
c
c     ,"RSCOPY :: ",
c     1    RSCOPY(III,1), RSCOPY(III,2), RSCOPY(III,3),
c     2    RSCOPY(III,4), RSCOPY(III,5), RSCOPY(III,6),
c     3    RSCOPY(III,7), "Initializing IG"
c        end do
C          write (*,*)

          NNInDO800 = 0

        end if

!$omp end critical

C            write (*,*) "... and passed the first critical section"
C
C wmy2017Sep08 no need for this IF anymore 
C       IF(NACTVE .GT. NSTORE) THEN
C
 
C  PRINT GRID PT. AND % COMPLETED TO SCREEN.
        XPER=IG*100.D0/NACTVE

        IF(XPER .GE. XNEXT) THEN

          IF(ICYCLE.eq.1) THEN

! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$          GOTO 676        

            WRITE(*,8888) ICYCLE,JSUB,XPER
  676       IF(NXE .GT. 0) WRITE(*,1254) NXE
 1254       FORMAT('  TOTAL NO. OF NUM. INTEG. WARNINGS IS ',I20)
          ENDIF

          XNEXT=XNEXT+1.D0

        ENDIF 
C 
C       ENDIF
C

C  ESTABLISH THE IGTH GRID POINT. IT IS STORED IN ROW IG OF
C  CORDEN.

c !$omp critical
c !$omp flush(CORDEN) 
        DO J = 1,max_pop_rand_varbs
          XXIG(J) = 0.D0
        END DO
        DO J=1,NVAR
          XXIG(J)=CORDEN(IG,J)
        END DO
c !$omp end critical
 
C  ESTABLISH THE COMBINED RANDOM AND FIXED PARAMETER VALUES INTO
C  PX -- IN THE CORRECT ORDER AS INDICATED BY VECTOR IRAN. CALL
C  MAKEVEC TO DO THIS. AS OF npagranfix.f, ADD NRANFIX AND RANFIXEST
C  TO THE ARGUMENT LIST TOO (I.E., THERE IS NOW A 3RD TYPE OF 
C  PARAMETER (THOSE WHICH ARE UNKNOWN BUT THE SAME FOR ALL SUBJECTS).

C wmy2017Sep21 -- PCOPY enters DO 800 w/no initialization;
C   It isn't used until after it returns from being initialized
C   in MAKEVEC, but it can't hurt to init here.
C        do iparam=1,max_ODE_params
C          PCOPY(iparam) = 0.0
C        end do


C        write (*,*) "1st call to MAKEVEC() NVAR = ", NVAR

        CALL MAKEVEC(NVAR,NOFIXCOPY,NRANFIX,IRANCOPY,XXIG,VALFIX,
     1    RANFIXEST, PX)
        NPX = NVAR+NOFIXCOPY+NRANFIX
C       if (NPX < max_ODE_params) then exit progam w/error
C
C !$omp critical
C        TID = OMP_GET_THREAD_NUM()
C        write (*,*) "IG", IG, "calling IDPC" 
C

C wmy2018Jan12 I removed the COMMON/PARAMD/P from SUBROUTINE IDCALCY and
c  IDCALCYY, so P must be initialized in main after calling MAKEVEC, but
c  before calling them. Note: IDCALCY and -YY now call SYMBOL and then 
c  immediately call FUNC2() or FUNC3(). Here, the ANAL3 routine still
c  requires that /PARAMD/P be set, and I'm not sure where, exactly, it is
c  used, but we should also set /TOCALC/PX
C        DO I=1,NVAR+NOFIX+NRANFIX
C        DO I=1,NPX
C          PX(I) = PCOPY(I)
C          P(I) = PCOPY(I)
C        END DO

c wmy2017Sep12 Added /TOUSER/ varbs to CALL IDPC()
c NPX,NOBSER,NUMEQT,NDIM, MF

C      write (*,*) JSUB, IG, "DO 800 CALL IDPC", PCOPY(1), PCOPY(2),
C     &  PCOPY(3), PCOPY(4), PCOPY(5)

          W = 0.0

C
C wmy2018Aug29 -- cross platform validation (is failing)
C
c        if (JSUB.eq.1) then 
c          if ((IG.eq.40012).or.(IG.eq.120034).or.(IG.eq.160045)
c     1     .or.(IG.eq.280075)) then
c            write (*,*) IG,"IG",(PCOPY(rrow),rrow=1,NPX)
c          end if
c        end if
c
        CALL IDPC(JSUB,IG,NPX,PX,NBCOMP,W,NOBSER,NUMEQT,
     1    NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     2    INTLIST,IPAR,ObsError,RPAR,ERRFILNAME)
C
C        w = rand() # wmy used this to prevent a crash and get an idea of code
C        w = w**2 # wmy wanted to guarantee the rand() was positive
C        write (*,*) IG, "returns", w, "from IDPC." 
C !$omp end critical

C  These are written at end of DO 800
C       write (*,*) "IG, W", IG, W
 
C  W RETURNS AS THE SUM OF:
C  ((YO(I,J)-H(I,J))/SIG(I,J))**2, WHERE H(I,J) = PREDICTED VALUE OF THE
C  JTH OUTPUT EQ AT THE ITH OBSERVATION TIME, ASSUMING THE IGTH GRID
C  POINT, X, ... OVER THE NOBSER x NUMEQT QUANTITIES ABOVE WHICH DON'T
C  HAVE YO(I,J) = -99 (WHICH MEANS THAT OUTPUT EQ. J HAS NO OBSERVED
C  LEVEL FOR TIME I).
 
C  CALCULATE P(YJ|X) FOR X-GRID POINT NO. IG.


 
C  THIS NEXT TEST IS FOR THE PC. AS AN EXAMPLE, THE COMPAC COMPUTER
C  CANNOT HANDLE ARGUMENTS TO DEXP WHICH ARE SMALLER THAN -11354. SINCE
C  THE ARGUMENT TO DEXP BELOW IS -.5*W, SET PYJGX = 0 IF W IS .GT.
C  22708.
 
C  SEE CODE AFTER CALCULATION OF P(YJ) TO SEE WHAT HAPPENS IF ALL THE
C  P(YJ|X) ARE  SET = 0.
 
C  NOTE THAT WORKK WILL ALWAYS BE SET = P(YJ|X=IG GRID PT), WHICH IS
C  NEEDED IN THE CALCULATION OF DXI (NOTE DXI NOT USED AS OF 
C  bignpaglap1.f) SINCE PYJGX WILL NOT BE COMPLETE IF NACTVE > MAXACT.

C wmy2017Sep07 note that as of OpenMPv2.5 the following flush and
c   critical should not matter, compiler knows to do this for read/
c   writing to the shared arrays.
c !$omp flush (WORK,WORKK,PYJGX,CORDEN)
c !$omp critical


C
C Poisson (or other distibution)
C    ObsError(I,J) at top of DO 1000 loop assumes Normal.  But for 
C other distribtions, such as Poisson, we must recalculate
C ObsError(I,J) _AND_ for any JSUB that has at least 1 non-normal
C ObsError(I,J), we must recalculate SIGFAC an OFAC.
C    Since recalculating seems ponderous -- just move entire
C Calculation inside of the DO 800 loop.
C

        if (IPAR(i_skip_ig).eq.1) then


          IF(IG .LE. MAXACT) PYJGX(JSUB,IG)=0.D0

          WORKK(IG) = 0.D0
C
C Old Code, Assumes all observations are ~ Normal
C 
C          IF(W .LE. 22708.D0) THEN
C           IF(IG .LE. MAXACT) PYJGX(JSUB,IG) = DEXP(-.5D0*W)/SIGFAC/OFAC
C           WORKK(IG) = DEXP(-.5D0*W)/SIGFAC/OFAC
C          ENDIF
C
C New Code, Assumes Mix of ~ Poisson and ~ Normal

C wmy20190906 -- Below should be caught in DO140(), and printed out there.
C          if ((IPAR(i_Npoissonobs).gt.0)
C     1        .and.(RPAR(k_prod_pr).eq.0)) then
C              write (*,*) JSUB,IG,"Err: pr=0 for"
C     1           , IPAR(i_Npoissonobs), "count measurements."
C          endif

          IF(RPAR(k_sum_z_sq) .LE. 22708.D0) THEN
           IF(IG .LE. MAXACT) THEN
            PYJGX(JSUB,IG) = 10**RPAR(k_prod_pr)
     1        * DEXP(-.5D0*RPAR(k_sum_z_sq))/RPAR(k_sfac)/RPAR(k_ofac)
           ENDIF
           WORKK(IG) = 10**RPAR(k_prod_pr)
     1        * DEXP(-.5D0*RPAR(k_sum_z_sq))/RPAR(k_sfac)/RPAR(k_ofac)
          ENDIF

C wmy2019.02.08 -- debugging
C          if (PYJGX(JSUB,IG) .LT. 0.0000000001) then
C              write (*,*) "PYJGX",JSUB,IG,10**RPAR(k_prod_pr),WORKK(IG)
C          endif

C End New Code

C  CALCULATE P(X,YJ) FOR X-GRID POINT NO. IG. PUT IT INTO WORK(IG).
 
          IF(IG .GT. MAXACT) THEN

           WORK(IG) = WORKK(IG)*CORDEN(IG,NVAR+1)

          ELSE

  700      WORK(IG)=PYJGX(JSUB,IG)*CORDEN(IG,NVAR+1)

           WORKK(IG) = PYJGX(JSUB,IG)

          ENDIF

C Remember that the following command ONLY
C works if the program is running in parallel!
C          ThreadNo = OMP_GET_THREAD_NUM()
C          write (*,*) "TID", TID, "returns", work(IG), WORKK(IG) 
C
C IPAR(i_cycle) = ICYCLE // at label 1001
c           if (NNInDO800.eq.0) then
C              write (*,*)
C     1 " --- END DO 800 :: (CYCLE,TID,JSUB,IG,s,w/w',PYJGX,px) ="
C     1    ,IPAR(i_cycle),ThreadNo,JSUB,IG,RPAR(k_sfac)
C     1    ,W,RPAR(k_sum_z_sq),WORKK(IG),CORDEN(IG,NVAR+1)
c           endif

c wmy201722Dec -- Need to restore option to skip bad points
c wmy2017Jul26 -- I think go to is safe here ...
C if (IgIsGoodPoint.eq.0) then ; IgIsGoodPoint replaced by IPAR(i_skip_ig)
C

        else

          NBadInDO800 = NBadInDO800 + 1
          PYJGX(JSUB,IG)=0.D0
          WORKK(IG) = 0.D0
          WORK(IG) = 0.D0

C          write (*,*) "Cycling past (JSUB,IG) ==", JSUB, IG,
C     1       NBadInDO800, "of", NInDO800, "of", IterLast

C          GO TO 800 ! we'll get there next anyways
        endif

c !$omp end critical


!        write (*,*) "At 800 ::",JSUB,IG,PYJGX(JSUB,IG)

  800   CONTINUE

! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp   End Do
!$omp   End Parallel

          write (*,*) "For JSUB:",JSUB,"Skipped",
     1       NBadInDO800, "of", NInDO800, "of", IterLast
 
C  CALCULATE P(YJ), A SCALAR WHICH IS THE INTEGRAL OF P(X,YJ) OVER

C  X-SPACE.
 
C  CALL NOTINT, AN INTEGRATION ROUTINE. THE

C  FOLLOWING IS SUPPLIED TO THIS ROUTINE:
C  VOLSPA = VOLUMNE OF THE INTEGRATION SPACE.
C  NGRID = NO. OF ORIGINAL GRID POINTS.
C  NACTVE = NO. OF ACTIVE GRID POINTS.
C  WORK(I), I=1,NACTVE = VALUE OF THE FUNCTION TO BE INTEGRATED, AT
C                       THE ITH GRID POINT.
C  MAXGRD  = THE DIMENSION OF WORK.
  
C wmy2017Sep08
C      write (*,*) "Calling NOTINT immediately after DO 800"

      CALL NOTINT(VOLSPA,NGRID,NACTVE,WORK,MAXGRD,PYJ)

C      write (*,*) "Returning", PYJ
 
C  IF PYJ RETURNS AS 0, IT IS BECAUSE P(X,YJ)=WORK IS 0 IN ALL ITS
C  NACTVE ENTRIES. THIS OCCURS WHEN EACH OF NACTVE VALUES OF W (WHICH
C  RETURNS FROM THE CALLS TO IDPC) IS LARGER THAN 1416 (SINCE P(YJ|X)
C  INVOLVES e RAISED TO THE POWER -.5*W, AND e RAISED TO A POWER
C  SMALLER THAN -708 IS SET TO 0 BY, FOR EXAMPLE, THE COMPAC COMPUTER).
C

C  IN CASE THIS HAPPENS, PRINT A MESSAGE TO THE USER AND STOP.
C
       IF (PYJ .EQ. 0.D0) THEN
       WRITE(*,26) JSUB
       WRITE(25,26) JSUB
   26  FORMAT(//' FOR SUBJECT, ',I6,' THE PROB. OF THE OBSERVED'/
     1' CONCENTRATIONS (FOR THE INDICATED DOSAGE REGIMEN), GIVEN EACH '/

     2' AND EVERY GRID POINT IN THE ESTABLISHED GRID, IS 0. THE '/

     3' PROGRAM STOPS. THE USER SHOULD CONSIDER INCREASING THE'/
     4' NO. OF GRID POINTS ALLOWED (HARDCODED INTO MAIN), AND/OR '/
     5' NARROWING THE GRID BOUNDARIES OF THE VARIABLES, AND/OR  '/
     6' INCREASING THE SIZES OF (C0,C1,C2,C3), THE ASSAY NOISE '/
     7' COEFFICIENTS. ALL OF THESE CHANGES WILL HAVE THE EFFECT OF'/
     8' MAKING SOME OF THE ABOVE CONDITIONAL PROBABILITES LARGER.')

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,26) JSUB 
        CLOSE(42)


        CALL PAUSE
        STOP
        ENDIF
 
 
C  ALSO, FROM LOOP 800 ABOVE, WORK(I) = P(X(I),YJ) --> WORK(I)/PYJ
C  = P(X(I)|Y=YJ). ADD THIS TO SPXGYJ(I) = SUM OF P(X(I)|Y=YJ) OVER ALL
C  NSUB SUBJECTS. DO THIS FOR ALL I=1,NACTVE.
 
! NEW PARALLEL CODE BELOW AS OF npageng28.f. 

!       DO I=1,NACTVE
!         SPXGYJ(I)=SPXGYJ(I)+WORK(I)/PYJ
!       END DO
 
C  ADD THIS SUBJECT'S CONTRIBUTION TO SLPYJ.
 
!       SLPYJ=SLPYJ+DLOG(PYJ)
 

 
 1000   CONTINUE


C  WRITE A MESSAGE TO THE USER IN CASE THIS RUN IS ANALYZING A LARGE

C  NO. OF SUBJECTS WITH A LARGE NO. OF INITIAL GRID POINTS ... SO HE
C  WILL KNOW THE PROGRAM HAS NOT 'HUNG'. ALSO SEE FORMAT 123 IN 
C  Subroutine emint.

      IF(ICYCLE .EQ. 1) WRITE(*,1243)
 1243 FORMAT(/' The Adaptive Grid optimization process could take '/
     1' several minutes if you are analyzing a large no. of subjects'/
     2' with a large no. of grid points. '//
     3' An approximate measure of how close the process is to being'/
     4' completed will be printed below: '/) 


C  AS OF npageng20.f, ONLY SAVE THE JOINT DENSITY AFTER CYCLE NO. 2.
C  REASON: CYCLE NO. 1 TYPICALLY HAS A LARGE NO. OF GRID POINTS AND
C   SO SAVING THE JOINT DENSITY AT THAT POINT COULD TAKE A LOT OF 
C   TIME. THAT, ALONG WITH THE FACT THAT THE PROBABILITY THE PROGRAM
C   WILL BOMB AFTER CYCLE 1 AND BEFORE CYCLE 2 IS VERY LOW, ARGUES
C   FOR SAVING THE DENSITY AFTER CYCLE NO. 2., WHEN THE TIME TO SAVE
C   WILL TYPICALLY BE MUCH LESS (SINCE THE NO. OF GRID POINTS WILL
C   PROBABLY HAVE DECREASED A LOT).

C   BY NOT SAVING THE DENSITY EVERY CYCLE, THE RUN WILL BE SPEEDED
C   UP, AND THERE WILL BE VERY LITTLE CHANCE THAT THE RUN, IF COMPILED
C   AND LINKED WITH gfortran, WILL STOP WITH A "Cannot write to file
C   opened for READ" error (referring to DENFIL).

C   ALSO, SINCE THE LION'S SHARE OF TIME FOR AN ANALYSIS IS USUALLY
C   GETTING THROUGH CYCLE 1, SAVING THE DENSITY AFTER CYCLE 2 WILL
C   TYPICALLY BE ALMOST AS GOOD AS SAVING IT AFTER, SAY, 100 CYCLES.

C  SO, IF ICYCLE = 2, STORE THIS CYCLE'S JOINT DENSITY (DOWN TO CORDEN
C  ONLY) INTO THE FILE DENFIL. THEN, IF THE RUN CRASHES BEFORE
C  COMPLETION, THE USER WILL BE ABLE TO PICK THE RUN UP AGAIN AFTER
C  THE END OF CYCLE 2, BY USING THE FOLLOWING DENSITY FILE AS AN 
C  APRIORI FILE.

C  NO! AS OF npageng23.f, THE SAVING OF EACH CYCLE'S DENSITY, EXCEPT
C  AFTER CYCLE 1, IS RESUMED. BUT THIS CAN BE MANUALLY CHANGED BY
C  CHANGING ISAVEDEN BELOW (IF ISAVEDEN IS SET TO SOMETHING OTHER THAN
C  1, EACH CYCLE'S DENSITY WILL NOT BE SAVED).

 
C???DEBUG. CHANGE ISAVEDEN TO BE 0 SO THE DENSITY IS NOT SAVED. THIS
C  IS BECAUSE I GOT SOME 'Cannot write to file opened for READ' 
C  execution errors while testing npagranfix2n.f.


      ISAVEDEN = 0

      IF(ISAVEDEN .EQ. 1 .AND. ICYCLE .NE. 1) THEN 

        OPEN(33,FILE=DENFIL)
 
        WRITE(33,7124)
 
        WRITE(33,*) NDIM
        WRITE(33,*) INDPTS
 
C  NEW CODE ABOVE FOR m2_11cal.f
 
        WRITE(33,*) NACTVE
        WRITE(33,*) NVAR
        WRITE(33,2227) (PAR(I),I=1,NVAR)
        WRITE(33,*) NOFIX
        WRITE(33,2227) (PARFIX(I),I=1,NOFIX)
        WRITE(33,*) NRANFIX
        WRITE(33,2227) (PARRANFIX(I),I=1,NOFIX)

 
        DO I=1,NVAR
          WRITE(33,*) (AB(I,J),J=1,2)
        END DO
 
        WRITE(33,*) (VALFIX(I),I=1,NOFIX)
        WRITE(33,*) (RANFIXEST(I),I=1,NOFIX)


C  STARTING WITH MXEM2N36.FOR, NINT WILL ALWAYS BE 100. BUT, IN ORDER
C  NOT TO CHANGE THE STRUTURE OF PRIOR DENSITY FILES (SO THAT PRIOR
C  DENSITIES CAN STILL BE RUN WITH MXEM2N36.FOR), NINT WILL STILL
C  BE WRITTEN TO, AND READ FROM, THIS FILE.
 
        WRITE(33,*) NINT
 
        WRITE(33,*) ICYCLE
        WRITE(33,*) DORIG
 
        DO I=1,NACTVE
          WRITE(33,*) (CORDEN(I,J),J=1,NVAR+1)
        END DO

        CLOSE(33)

      ENDIF

C  ABOVE ENDIF IS FOR THE IF(ISAVEDEN .EQ. 1 .AND. ICYCLE .NE. 1)
C   CONDITION.


c begin optimization
cgam5
cgam5 - from here (immediately after 1000   CONTINUE to
cgam5 - immediately before c end optimization was lifted
cgam5 - from gamadapt1.f, replacing old material beteen these limits
      igamma = igamma + 1
      if(ierrmod.eq.1) igamma=1
csdsc - added April 2, 2000
c con first iteration, call hte interior point method

      if(mod(igamma,3).eq.1) then
      IF(ISUPRES .EQ. 0) write(6,*)
      IF(ISUPRES .EQ. 0) write(6,*) 'icycle,igamma =',icycle,igamma
      IF(ISUPRES .EQ. 0) 
     1 write(6,*) 'condensing current ', nactve, ' point grid'
      IF(ISUPRES .EQ. 0) write(6,*) 'base gamma =',gamma

      ijob = 1
      gammab = gamma
      gammap = gamma * (1.d0+gamdel)
      gammam = gamma / (1.d0+gamdel)
      call emint(pyjgx,maxsub,corden,maxgrd,nactve,nsub,ijob,
     &corden(1,nvar+1),denstor(1,1),denstor(1,2),denstor(1,3),
     &fobj,gap,nvar,keep,IHESS,errfil,ISUPRES)

C wmy2017Jan05
c      subroutine emint(psi,ldpsi,theta,ldtheta,npoint,nsub,ijob,
c     &                 x,dx,y,dy,fobj,gap,nvar,keep,IHESS)



C  AS OF npageng18.f, IHESS IS ADDED TO ARGUMENT LIST OF emint.
C  IF IHESS RETURNS AS -1, IT MEANS THE HESSIAN MATRIX IN THE INTERIOR
C  POINTS ALGORITHM WAS SINGULAR. IN THIS CASE, GO TO LABEL 900 TO
C  CREATE THE OUTPUT FILES BASED ON THE VALUES FROM THE PREVIOUS CYCLE.
C  NOTE THAT BY GOING TO LABEL 900, NOTHING FROM THIS CYCLE WILL BE
C  WRITTEN TO FILE 25, AND THE CORDEN(.,.) MATRIX WILL REMAIN WHAT IT
C  WAS FROM THE PREVIOUS CYCLE.

      IF(IHESS .EQ. -1) GO TO 900


      fobj1 = fobj
      nactve1 = nactve

      IF(ISUPRES .EQ. 0) 


     1 write(6,*) 'base job 1, fobj,keep,icycle=',fobj,keep,icycle
C      CALL PAUSE


      ijob = 0
      nactve = keep
      call emint(pyjgx,maxsub,corden,maxgrd,nactve,nsub,ijob,
     &corden(1,nvar+1),denstor(1,1),denstor(1,2),denstor(1,3),
     &fobj,gap,nvar,keep,IHESS,errfil,ISUPRES)

C  AS OF npageng18.f, IHESS IS ADDED TO ARGUMENT LIST OF emint.

C  IF IHESS RETURNS AS -1, IT MEANS THE HESSIAN MATRIX IN THE INTERIOR
C  POINTS ALGORITHM WAS SINGULAR. IN THIS CASE, GO TO LABEL 900 TO
C  CREATE THE OUTPUT FILES BASED ON THE VALUES FROM THE PREVIOUS CYCLE.
C  NOTE THAT BY GOING TO LABEL 900, NOTHING FROM THIS CYCLE WILL BE
C  WRITTEN TO FILE 25, AND THE CORDEN(.,.) MATRIX WILL REMAIN WHAT IT
C  WAS FROM THE PREVIOUS CYCLE.


      IF(IHESS .EQ. -1) GO TO 900


      fobjbase = fobj


      IF(ISUPRES .EQ. 0)
     1 write(6,*) 'base job 0, fobj,keep,icycle=',fobj,keep,icycle
      IF(ISUPRES .EQ. 0) 
     1 write(6,*) 'base job 0 nactve,gamma=',nactve,gamma

      nactve0 = nactve
c new on Jan 2, 2002 - save otpimal solution in denstor(1,4)
c so that stat program can work on best of base, up, and down
c solutions
      do i=1,nactve
      denstor(i,4)=corden(i,nvar+1)
      enddo
      nstore = 0
      fobjbest = fobjbase

      IF(ISUPRES .EQ. 0) write(6,*) 'finished base case'

      if(ierrmod.eq.1) go to 14001
      gamma = gammap
      RPAR(k_gamma) = gamma
      go to 10001

      endif
cgamma above endif is for mod(igamma,3).eq.1 case
      if(mod(igamma,3).eq.2) then


      IF(ISUPRES .EQ. 0) write(6,*) 'gamma plus =',gamma

      ijob = 0
      call emint(pyjgx,maxsub,corden,maxgrd,nactve,nsub,ijob,
     &corden(1,nvar+1),denstor(1,1),denstor(1,2),denstor(1,3),
     &fobj,gap,nvar,keep,IHESS,errfil,ISUPRES)

C  AS OF npageng18.f, IHESS IS ADDED TO ARGUMENT LIST OF emint.
C  IF IHESS RETURNS AS -1, IT MEANS THE HESSIAN MATRIX IN THE INTERIOR
C  POINTS ALGORITHM WAS SINGULAR. IN THIS CASE, GO TO LABEL 900 TO
C  CREATE THE OUTPUT FILES BASED ON THE VALUES FROM THE PREVIOUS CYCLE.
C  NOTE THAT BY GOING TO LABEL 900, NOTHING FROM THIS CYCLE WILL BE
C  WRITTEN TO FILE 25, AND THE CORDEN(.,.) MATRIX WILL REMAIN WHAT IT
C  WAS FROM THE PREVIOUS CYCLE.

      IF(IHESS .EQ. -1) GO TO 900


      fobjplus = fobj



      IF(ISUPRES .EQ. 0) 

     1 write(6,*) 'fobjplus,gamma,icycle=',fobjplus,gamma,icycle

c new  Jan 2, 2002 - save solution if fobjplus is better than fobjbase
      if(fobjplus.gt.fobjbest) then
      fobjbest = fobjplus
      do i=1,nactve

      denstor(i,4) = corden(i,nvar+1)
      enddo
      endif
      gamma = gammam
      RPAR(k_gamma) = gamma


      IF(ISUPRES .EQ. 0) write(6,*) 'finished plus case'


         go to 10001
      endif
      if(mod(igamma,3).eq.0) then

      IF(ISUPRES .EQ. 0) write(6,*) 'gamma minus=',gamma

      ijob = 0
      call emint(pyjgx,maxsub,corden,maxgrd,nactve,nsub,ijob,
     &corden(1,nvar+1),denstor(1,1),denstor(1,2),denstor(1,3),
     &fobj,gap,nvar,keep,IHESS,errfil,ISUPRES)


C  AS OF npageng18.f, IHESS IS ADDED TO ARGUMENT LIST OF emint.

C  IF IHESS RETURNS AS -1, IT MEANS THE HESSIAN MATRIX IN THE INTERIOR
C  POINTS ALGORITHM WAS SINGULAR. IN THIS CASE, GO TO LABEL 900 TO
C  CREATE THE OUTPUT FILES BASED ON THE VALUES FROM THE PREVIOUS CYCLE.
C  NOTE THAT BY GOING TO LABEL 900, NOTHING FROM THIS CYCLE WILL BE
C  WRITTEN TO FILE 25, AND THE CORDEN(.,.) MATRIX WILL REMAIN WHAT IT
C  WAS FROM THE PREVIOUS CYCLE.

      IF(IHESS .EQ. -1) GO TO 900


      fobjminu = fobj

      IF(ISUPRES .EQ. 0) 
     1 write(6,*) 'fobjminu,gamma,icycle=',fobjminu,gamma,icycle

      if(fobjminu.gt.fobjbest) then
      fobjbest = fobjminu
      do i=1,nactve
      denstor(i,4) = corden(i,nvar+1)
      enddo
      endif

      IF(ISUPRES .EQ. 0) write(6,*) 'finished gamma minus case'

      endif
cgamma - above statement changed from "nstore = nactve" to force
c reevaluation of all points.
      
c now temporairily reset to gamma
      gamma = gammab
      RPAR(k_gamma) = gammab
      fobj = fobjbase
      if(fobjplus.gt.fobjbase) then
      gamma = gammap
      RPAR(k_gamma) = gammap

      fobj = fobjplus
      gamdel = 4.*gamdel
      endif
      if(fobjminu.gt.fobjbase) then
      gamma = gammam
      RPAR(k_gamma) = gammam
      fobj = fobjminu
      gamdel = 4.*gamdel
      endif

      gamdel = gamdel*0.5
      if(gamdel.lt.0.01) gamdel=0.01
14001 continue
cgam5 above label is entry point for ierrmod = 1 (no  gamma) case

      IF(ISUPRES .EQ. 0) write(6,*) 'fobjbest=',fobj

c corden(*,nvar+1) sums to 1 when it comes out of emint
c Now reset forden(i,nvar+1) to best of three solutions
c and normalize to funny BIGNPEM factor
      fact=ngrid/volspa

      do i=1,nactve
      corden(i,nvar+1)=fact*denstor(i,4)
      enddo


cend optimization

C      write (*,*) "Made it past optimization"

cbegin statistics


c now we compute all hte statistical stuff using this distribution
c and the full nactve (before condensation) points.
c Later, in the condensation performed just before the grid refienment
c and subsequent expansion, we will condense by just using the
c 'keep' flags in DENSTOR(i,1) that emint left there.  The density will

c not be updated to refelct this cahnge (there is no need)
c until the next call to emint


c  As of npageng18.f, save CORDEN to CORDLAST AND NACTVE TO NACTLAST.
c  The reason is that if, somewhere during the next cycle's calculations
c  (during one of the calls to Subroutine emint), a Hessian Matrix is
c  singular, then IHESS will be set = -1, and the program will stop.
c  And in this case, the program must be able to write out all of the
c  information from this cycle (the last completed cycle). And that
c  means that the CORDEN from this cycle (which will be stored into 
c  CORDLAST), and NACTVE (store into NACTLAST) should be used in the
c  call to Subroutine SUBRES in loop 7000. Otherwise, the CORDEN and
c  NACTVE used in that call would have already partly updated in the
c  next cycle before the Hessian error occurred.

      DO I = 1,NACTVE
       DO J = 1,NVAR+1
       CORDLAST(I,J) = CORDEN(I,J)
       END DO
      END DO

      NACTLAST = NACTVE


 
 
C  SLPYJ IS NOW THE LOG-LIKELIHOOD OF ALL NSUB SUBJECT VECTORS, GIVEN
C  THE PRIOR DENSITY IN COL. NVAR+1 OF CORDEN.
 
C  PRINT OUT SLPYJ FOR THE PRIOR DENSITY:


        IF(ISUPRES .EQ. 0) WRITE(*,8) NSUB 

        WRITE(25,8) NSUB
    8 FORMAT(/' THE TRUE (NUMERICAL) LOG-LIKELIHOOD OF THE ',I3/

     1' SUBJECT VECTORS, GIVEN THE PRIOR DENSITY, IS: ')

        IF(ISUPRES .EQ. 0) WRITE(*,*) fobj

        WRITE(25,*) fobj


C  AS OF bigmlt12.f, CALCULATE AIC AND BIC, AS IN itbig13.f. THESE 
C  VALUES WILL PRINT EVERY CYCLE IN THE OUTPUT FILE.

C  AS OF npageng26.f, THE FORMULAS FOR AIC AND BIC ARE CHANGED.

C  CALCULATE TWO MEASURES OF INFORMATION: THE AKAIKE INFORMATION 
C  CRITERION (AIC), AND THE SCHWARTZ (BAYESIAN) INFORMATION CRITERION 
C  (BIC). IN GENERAL, THE MODEL WITH THE MINIMUM AIC AND/OR
C  BIC IS THE PREFERRED MODEL.

C  AIC = 2*(-TRULOG + (D + Q)), WHERE TRULOG = fobj (SEE ABOVE),
C    D = NVAR, THE DIMENSION OF THE RANDOM PARAMETER VECTOR.
C    Q = NO. OF NOISE PARAMETERS = 
C      = 0 IF IERRMOD = 1;
C          = 1 IF IERRMOD = 2,3, OR 4.
C  BUT AS OF npageng27.f, WE USE THE NEW FORMULA, AICC, WHERE
C  AICC = AIC + 2*KP(KP+1)/(NOBTOT-KP-1), WHERE KP = D + Q.

C  BIC = 2*(-TRULOG + .5*(D + Q)*LOG(NOBTOT)), WHERE TRULOG, D, AND Q
C      ARE AS ABOVE, AND NOBTOT IS THE TOTAL NO. OF NON-MISSING OBSERVED
C      VALUES OVER ALL SUBJECTS (INCLUDING ALL NUMEQT OUTPUT EQUATIONS).

        QVAL = 1
        IF(IERRMOD .EQ. 1) QVAL = 0
        KP = NVAR + int(QVAL)

        AIC = 2.D0*(-FOBJ + KP)

C  AS OF npageng28.f, verify that NOBTOT-KP-1 is not .LE. 0. If it
C  is, set AICC = AIC.

        AICC = AIC
        IF(NOBTOT-KP-1 .GT. 0) AICC = AIC + 2.D0*KP*(KP+1)/(NOBTOT-KP-1)
        BIC = 2.D0*(-FOBJ + .5D0*(NVAR + QVAL)*DLOG(1.D0*NOBTOT))

        IF(ISUPRES .EQ. 0) THEN
         WRITE(*,3412)
 3412   FORMAT(/' THE AKAIKE AND BAYESIAN INFO CRITERIONS (AIC AND BIC) 
     1ARE: ') 
         WRITE(*,*) AICC,BIC
        ENDIF

         WRITE(25,3412)
         WRITE(25,*) AICC,BIC

C+++++++++++++++ CODE BELOW TO USE ONLY HIGH PROB POINTS ++++++++++++++
 

C  PRINT NACTVE ONLY IF THIS CYCLE IS CERTAIN TO RUN (I.E.,
C  ONLY IF ITEST = 0). SUBROUTINE PRNTOP PRINTS THIS INFO FOR
C  ACCELERATED CYCLES.
 
        IF(ITEST .EQ. 0) THEN
 
C  NOTE THAT SPXGYJ(I) FOR ALL INACTIVE GRID POINTS = 0 --- SEE CODE
C  NEAR LABEL 800. SO ALL INACTIVE GRID POINTS WILL REMAIN FOREVER
C  INACTIVE.
 
          IF(ISUPRES .EQ. 0) THEN
            WRITE(*,*)
            WRITE(*,*)' THE NO. OF ACTIVE GRID POINTS IS NOW ',NACTVE
            WRITE(*,*)
            WRITE(*,*)' THE INITIAL NO. OF GRID POINTS WAS ',ngridn
            WRITE(*,*)
          ENDIF
 
          WRITE(25,*)
          WRITE(25,*)' THE NO. OF ACTIVE GRID POINTS IS NOW ',NACTVE
          WRITE(25,*)
          WRITE(25,*)' THE INITIAL NO. OF GRID POINTS WAS ',ngridn
          WRITE(25,*)
 
 

        ENDIF
 
C+++++++++++++++ CODE ABOVE TO USE ONLY HIGH PROB POINTS ++++++++++++++

        IF(ISUPRES .EQ. 0) WRITE(*,11)

        WRITE(25,11)
   11 FORMAT(//' THE FOLLOWING VALUES ARE FOR THE UPDATED DENSITY: ')
 
 
C  CALCULATE THE 'SCALED INFORMATION' FOR THIS CYCLE (STARTING WITH

C  PROGRAM m234calc.f, 11-6-94; BUT THE SCALED 'INFO' IS CHANGED
C  IN PROGRAM MXEM2N35.FOR/m235calc.f, 11-19-94):
 
C  INFO = SUM(PI*LOG(PI)), WHERE THE SUM IS OVER I=1,NACTVE, THE LOG
C         IS TO BASE 2, PI*LOG(PI) --> 0 IF PI = 0, AND
C    PI = D(I)*VOLSPA/NGRID (SINCE SUM(D(I)) = NGRID/VOLSPA),
C         WHERE D(I) = CORDEN(I,NVAR+1), I=1,NACTVE.
 
 
C  THEN INFO IS SCALED: y = 100*ln(2)/ln(n/N)*(x + ln(n)/ln(2)), WHERE
C  y = SCALED INFO, x = ABOVE INFO, n = NGRID, N = NO. OF SUBJECTS. SO
C  y = 0 (%) IF DENSITY IS UNIFORM, and y = 100 (%) if DENSITY IS
C  CONCENTRATED AT N EQUALLY LIKELY POINTS, ... I.E., SCALED INFO = 0 %
C  FOR NO INFO, AND SCALED INFO = 100 % FOR PERFECT INFO.
 
        FACT=VOLSPA/NGRID
 
        SUM=0.D0

 
        DO I=1,NACTVE
 
          PI=CORDEN(I,NVAR+1)*FACT
          IF (PI .GT. 1.D-200) THEN

            SUM=SUM+PI*DLOG(PI)
          ENDIF
 
        END DO
 
        DL2=DLOG(2.D0)
        SUM = SUM/DL2
        ENT = -SUM
 
C  BUG CORRECTION DONE FOR m2_19aca.f. ENTROPY CALCULATION MOVED TO
C  AFTER SUM=SUM/DL2 STATEMENT.
 

C  ENTROPY = -INFO = -SUM(PI*LOG(PI)) IS ALSO PRINTED OUT STARTING WITH
C  MXEM2N36.FOR.
 
 
C  SUM IS NOW THE INFO FOR THE JOINT DENSITY. SCALE IT AS SHOWN ABOVE,
C  AND PRINT IT OUT.
 
        DGRID=NGRID

        FACT=100.D0*DL2/DLOG(DGRID/NSUB)
        SUM = FACT*(SUM + DLOG(DGRID)/DL2)

C  REPLACE WRITING OF SUM AND ENT WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL.

      XVERIFY(1) = SUM
      XVERIFY(2) = ENT
      CALL VERIFYVAL(2,XVERIFY)


C     IF(ISUPRES .EQ. 0) WRITE(*,31) SUM
C     IF(ISUPRES .EQ. 0) WRITE(*,131) ENT
 
      IF(ISUPRES .EQ. 0) THEN
        WRITE(*,31) XVERIFY(1)
        WRITE(*,131) XVERIFY(2)
      ENDIF  
   
C     WRITE(25,31) SUM
C     WRITE(25,131) ENT

        WRITE(25,31) XVERIFY(1)
        WRITE(25,131) XVERIFY(2)


   31 FORMAT(/' THE SCALED INFO FOR THIS CYCLE IS ',F10.2,' %'/)
  131 FORMAT(/' THE ENTROPY FOR THIS CYCLE IS ',G11.4/)


 2100   CONTINUE


C  Starting with bigmlt1.f, this is an entry point to pick up needed
c  values.

C        write (*,*) "DO 1100 :: Calculate expected values"

C  CALCULATE EXPECTED VALUES FOR THIS CYCLE'S UPDATED DENSITY.
 
        DO 1100 I=1,NVAR
 
C  FORM THE VECTOR WORK TO BE THE PRODUCT OF THE COORDINATES OF
C  VARIABLE I (IN COL. I OF CORDEN) AND ITS ASSOCIATED DENSITY (IN
C  COLUMN NVAR+1 OF CORDEN).
 
        DO IG=1,NACTVE
          WORK(IG)=CORDEN(IG,I)*CORDEN(IG,NVAR+1)
        END DO
 
        II=I
        CALL NOTINT(VOLSPA,NGRID,NACTVE,WORK,MAXGRD,EX(II))
 
        DO 1100 J=1,I
 
C  FORM THE VECTOR WORK TO BE THE PRODUCT OF THE COORDINATES OF
C  VARIABLES I AND J (IN COLS. I AND J OF CORDEN) AND THEIR ASSOCIATED
C  DENSITY (IN COLUMN NVAR+1 OF CORDEN).
 
        DO IG=1,NACTVE
          WORK(IG)=CORDEN(IG,I)*CORDEN(IG,J)*CORDEN(IG,NVAR+1)
        END DO
 
        JJ=J
 1100   CALL NOTINT(VOLSPA,NGRID,NACTVE,WORK,MAXGRD,E(II,JJ))
 
 
C  STORE THE MEANS INTO CENTER(1,.). CENTER(2,.) AND CENTER(3,.)
C  THE MEDIANS AND MODES WILL BE STORED JUST BELOW THE CALL TO STAT.
C  THE VALUES IN CENTER WILL BE NEEDED BELOW IN THE CALLS TO IDCALCY
C  IF THIS TURNS OUT TO BE THE LAST CYCLE.
 
          DO I=1,NVAR
            CENTER(1,I)=EX(I)
          END DO
 
 
C  CALCULATE THE COVARIANCES; THEN WRITE RESULTS.
 
      DO 190 I=1,NVAR
      DO 190 J=1,I
  190 COV(I,J)=E(I,J)-EX(I)*EX(J)
 
      IF(ISUPRES .EQ. 0)  WRITE(*,*)' THE MEANS ARE: ' 


        WRITE(25,*)' THE MEANS ARE: '
 
        IF(ISUPRES .EQ. 0) WRITE(*,5104) (PAR(I),I=1,NVAR)

        WRITE(25,5104) (PAR(I),I=1,NVAR)
 5104   FORMAT(5X,30(A11,2X))

      DO I = 1,NVAR
       XVERIFY(I) = EX(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)      

C     IF(ISUPRES .EQ. 0) WRITE(*,5103) (EX(I),I=1,NVAR)
      IF(ISUPRES .EQ. 0) WRITE(*,5103) (XVERIFY(I),I=1,NVAR)


C     WRITE(25,5103) (EX(I),I=1,NVAR)
      WRITE(25,5103) (XVERIFY(I),I=1,NVAR)

 5103   FORMAT(1X,30(G12.6,1X))

C  IN CASE OF VERY SMALL VARIANCES, COV(I,I) COULD TURN OUT TO BE,
C  NUMERICALLY, A SMALL NEGATIVE NO. IN SUCH CASES, SET ALL COV'S
C  = 0 (WHICH MEANS THAT THE STD'S AND COFVR'S WILL ALSO BE SET = 0
C  BELOW), AND SET CORR'S = -99 BELOW. ALSO, SKEWNESS AND KURTOSIS WILL
C  HAVE -99999999 WRITTEN IN THEIR LOCATIONS.

      ICOVL0=0
 
      DO I=1,NVAR
       IF(COV(I,I) .LE. 0.D0) ICOVL0=1
      END DO

      IF(ICOVL0 .EQ. 1) THEN
       DO I = 1,NVAR
        DO J = 1,NVAR
         COV(I,J) = 0.D0
        END DO
       END DO

      ENDIF


        IF(ISUPRES .EQ. 0) WRITE(*,13) 

        WRITE(25,13)
   13 FORMAT(/' THE COV MATRIX IS, IN LOWER TRI FORM: ')
 
      IF(ISUPRES .EQ. 0) WRITE(*,5104) (PAR(I),I=1,NVAR)
      WRITE(25,5104) (PAR(I),I=1,NVAR)
 

      DO 200 I = 1,NVAR

       DO J = 1,I
        XVERIFY(J) = COV(I,J)

       END DO
       CALL VERIFYVAL(I,XVERIFY)



C       WRITE(25,5103) (COV(I,J),J=1,I)
        WRITE(25,5103) (XVERIFY(J),J=1,I)


C 200 IF(ISUPRES .EQ. 0) WRITE(*,5103) (COV(I,J),J=1,I)
  200 IF(ISUPRES .EQ. 0) WRITE(*,5103) (XVERIFY(J),J=1,I)



C  CALCULATE AND PRINT OUT STANDARD DEVIATIONS, COEFFICIENTS OF
C  VARIATION, AND CORRELATION COEFFICIENTS.
 

      DO I = 1,NVAR


       STD(I)=DSQRT(COV(I,I))
       COFVR(I)=STD(I)*1.D2/EX(I)

        DO J = 1,I
         IF(ICOVL0 .EQ. 0) CORR(I,J) = COV(I,J)/STD(I)/STD(J)
         IF(ICOVL0 .EQ. 1) CORR(I,J) = -99.D0
        END DO


      END DO


      DO I = 1,NVAR
       XVERIFY(I) = STD(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)      


        IF(ISUPRES .EQ. 0) WRITE(*,6071)
        WRITE(25,6071)
 6071 FORMAT(/' THE STANDARD DEVIATIONS ARE, RESPECTIVELY: ')
 
        IF(ISUPRES .EQ. 0) WRITE(*,5104) (PAR(I),I=1,NVAR)
        WRITE(25,5104) (PAR(I),I=1,NVAR)
 
C     IF(ISUPRES .EQ. 0) WRITE(*,5103) (STD(I),I=1,NVAR)


      IF(ISUPRES .EQ. 0) WRITE(*,5103) (XVERIFY(I),I=1,NVAR)
C     WRITE(25,5103) (STD(I),I=1,NVAR)
      WRITE(25,5103) (XVERIFY(I),I=1,NVAR)
 

      DO I = 1,NVAR
       XVERIFY(I) = COFVR(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)
      


        IF(ISUPRES .EQ. 0) WRITE(*,6072)
        WRITE(25,6072)
 6072 FORMAT(/' THE PERCENT COEFFICIENTS OF VARIATION ARE, RESP.: ')
 
        IF(ISUPRES .EQ. 0) WRITE(*,5104) (PAR(I),I=1,NVAR)
        WRITE(25,5104) (PAR(I),I=1,NVAR)
 


C     IF(ISUPRES .EQ. 0) WRITE(*,5103) (COFVR(I),I=1,NVAR)
      IF(ISUPRES .EQ. 0) WRITE(*,5103) (XVERIFY(I),I=1,NVAR)


C     WRITE(25,5103) (COFVR(I),I=1,NVAR)
      WRITE(25,5103) (XVERIFY(I),I=1,NVAR)
 
      IF(ISUPRES .EQ. 0) WRITE(*,6073)
      WRITE(25,6073)
 6073 FORMAT(/' THE CORR. MATRIX IS, IN LOWER TRIANGULAR FORM: ')
 
      IF(ISUPRES .EQ. 0) WRITE(*,5104) (PAR(I),I=1,NVAR)
	WRITE(25,5104) (PAR(I),I=1,NVAR)


      DO 6080 I=1,NVAR

       DO J = 1,I
        XVERIFY(J) = CORR(I,J)
       END DO
       CALL VERIFYVAL(I,XVERIFY)



C      WRITE(25,5103) (CORR(I,J),J=1,I)
       WRITE(25,5103) (XVERIFY(J),J=1,I)


C6080   IF(ISUPRES .EQ. 0) WRITE(*,5103) (CORR(I,J),J=1,I)
 6080   IF(ISUPRES .EQ. 0) WRITE(*,5103) (XVERIFY(J),J=1,I)
 

        IF(ISUPRES .EQ. 0) WRITE(*,6091) NVAR
        WRITE(25,6091) NVAR
 6091   FORMAT(//' THE ',I2,' SETS OF LINES BELOW WILL GIVE ADDITIONAL'/
     1' STATISTICS FOR THE VARIABLES. FOR EACH SET:'//
     2' THE 1ST LINE WILL GIVE THE MODE, THE SKEWNESS, THE KURTOSIS,'/
     3' AND THE 2.5 %-TILE VALUE OF THE DISTRIBUTION. '//
     4' THE 2ND LINE WILL GIVE THE 25, 50, 75, AND 97.5 %-TILE VALUES'/
     5' OF THE DISTRIBUTION. '//
     6' THE 3RD LINE WILL GIVE THREE ADDITIONAL AD-HOC ESTIMATES OF'/
     7' THE STANDARD DEVIATION FOR THAT MARGINAL DENSITY. THE 1ST S.D.'/
     7' ESTIMATE IS THE STANDARD DEVIATION OF A NORMAL DISTRIBUTION '/
     8' HAVING THE SAME [25, 75] %-TILE RANGE AS THAT VARIABLE. THE'/
     9' 2ND ESTIMATE IS THE STANDARD DEVIATION OF A NORMAL DIST.'/
     1' HAVING THE SAME [2.5, 97.5] %-TILE RANGE AS THAT VARIABLE. THE'/
     2' 3RD ESTIMATE IS THE AVERAGE OF THE FIRST TWO. THE 4TH VALUE'/
     3' IN THE LINE IS THE THE % SCALED INFO FOR THAT MARGINAL DENS.'//)
 
C  CALC. THE MODE (XMODE) AND 5 %-TILE VALUES (X025,X25,X50,X75,X975)
C  FOR EACH VARIABLE (NOTE THAT X50 IS THE MEDIAN). ALSO, CALCULATE
C  SCALINFO, THE SCALED INFORMATION FOR EACH MARGINAL DENSITY.

 
       DO 6090 I=1,NVAR

 
         IND=I
 
C  NOTE: IN THE CALL TO STAT, THE ARGUMENT WORK IS A DUMMY ARRAY; IT
C        MUST BE AN AN ARGUMENT SINCE IT IS VARIABLY DIMENSIONED IN

C        STAT.
 
        CALL STAZ(VOLSPA,NGRID,NACTVE,NVAR,IND,CORDEN,WORK,MAXGRD,NINT,
     1  AB(IND,1),AB(IND,2),XMODE,X025,X25,X50,X75,X975,SCALINFO,NSUB,
     2  MAXDIM)
 
C  STORE THE MEDIANS AND MODES IN CASE THIS IS THE LAST CYCLE (IN WHICH
C  CASE THEY WILL BE NEEDED BELOW IN CALLS TO IDCALCY.
 
         CENTER(2,IND)=X50
         CENTER(3,IND)=XMODE

C  THE 1ST 'AD-HOC' ESTIMATE OF THE STD. DEV. IS THAT OF A NORMAL DIST.
C  HAVING THE SAME [25, 75] %-TILE RANGE AS THIS VARIABLE. SINCE 50 % OF
C  A STANDARD NORMAL DIST. IS BETWEEN MU +/- .6745*SD, THE INNER
C  50 %-TILE RANGE IS APPROX. 1.349*SD WIDE. I.E., SD IS APPROX.
C  (X75-X25)/1.349.
 
C  SIMILARLY, THE 2ND ESTIMATE OF THE SD IS THAT OF A NORMAL DIST.
C  HAVING THE SAME [2.5, 97.5] %-TILE RANGE AS THIS VARIABLE. IN THIS
C  CASE THE SD APPROX = (X975-X025)/3.92.
 
         SDEST1 = (X75-X25)/1.349
         SDEST2 = (X975-X025)/3.92
         SDEST3 = .5D0*(SDEST1+SDEST2)
 
C  CALCULATE KURTOSIS AND SKEWNESS FOR EACH VARIABLE.
C
C  THE COEFF OF SKEWNESS = EXP[(X-EXP(X))**3]/STDX**3, WHERE STDX IS
C                             THE STD DEV OF X.

C
C  SKEWNESS IS > 0 IF THE DISTRIBUTION SKEWS (EXTENDS OUT) TO THE RIGHT.
C  SKEWNESS IS < 0 IF THE DISTRIBUTION SKEWS (EXTENDS OUT) TO THE LEFT.
C  SKEWNESS IS = 0 IF THE DISTRIBUTION IS SYMMETRIC.
C
C  THE COEFF OF KURTOSIS = EXP[(X-EXP(X))**4]/STDX**4.

C
C  KURTOSIS = 3 IF THE DISTRIBUTION IS NORMAL.
C  KURTOSIS > 3 IF THE DISTRIBUTION IS MORE PEAKED THAN THE NORMAL DIST.
C  KURTOSIS < 3 IF THE DISTRIBUTION IS LESS PEAKED THAN THE NORMAL DIST.
C
C  FORM THE VECTOR WORK TO BE (X(I)-EXI)**3*CORDEN(I,NVAR+1), WHERE
C  EXI IS THE EXPECTED VALUE FOR VARIABLE I. THEN INTEGRATE IT TO
C  GET THE NUMERATOR OF SKEWNESS. THEN DO THE SAME FOR THE NUMERATOR
C  OF KURTOSIS, EXCEPT THE EXPONENT IS 4 INSTEAD OF 3.
 
C  NOTE: IF ICOVL0 = 1, STD'S HAVE NOT BEEN CALCULATED ABOVE (SINCE AT
C        LEAST 1 COV(I,I) WAS NUMERICALLY .LE. 0). IN SUCH A CASE,
C        ARBITRARILY SET SK AND KU = -99999999.
 
        IF(ICOVL0 .EQ. 1) THEN
          SK = -99999999
          KU = -99999999
        ENDIF
 
        IF(ICOVL0 .EQ. 0) THEN
 
      DO IG=1,NACTVE
       XX = CORDEN(IG,IND)
       WORK(IG) = CORDEN(IG,NVAR+1)*(XX-EX(IND))**3
      END DO
 
      CALL NOTINT(VOLSPA,NGRID,NACTVE,WORK,MAXGRD,SK)
      SK=SK/STD(I)**3
 
       DO IG=1,NACTVE
         XX = CORDEN(IG,IND)
        WORK(IG) = CORDEN(IG,NVAR+1)*(XX-EX(IND))**4
       END DO

 
      CALL NOTINT(VOLSPA,NGRID,NACTVE,WORK,MAXGRD,KU)
      KU=KU/STD(I)**4
 
      ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(ICOVL0 .EQ. 1)  CONDITION.

 
      IF(ISUPRES .EQ. 0) WRITE(*,6092) PAR(IND)
        WRITE(25,6092) PAR(IND)
 6092 FORMAT(/' ',A11,':')
 

       XVERIFY(1) = XMODE
       XVERIFY(2) = SK
       XVERIFY(3) = KU   
       XVERIFY(4) = X025
       CALL VERIFYVAL(4,XVERIFY)



C      IF(ISUPRES .EQ. 0) WRITE(*,6093) XMODE,SK,KU,X025
       IF(ISUPRES .EQ. 0) WRITE(*,6093) (XVERIFY(IXV),IXV=1,4) 

C      WRITE(25,6093) XMODE,SK,KU,X025
       WRITE(25,6093) (XVERIFY(IXV),IXV=1,4) 



       XVERIFY(1) = X25
       XVERIFY(2) = X50
       XVERIFY(3) = X75  
       XVERIFY(4) = X975
       CALL VERIFYVAL(4,XVERIFY)

C      IF(ISUPRES .EQ. 0) WRITE(*,6093) X25,X50,X75,X975
       IF(ISUPRES .EQ. 0) WRITE(*,6093) (XVERIFY(IXV),IXV=1,4) 

C      WRITE(25,6093) X25,X50,X75,X975
       WRITE(25,6093) (XVERIFY(IXV),IXV=1,4) 

       XVERIFY(1) = SDEST1
       XVERIFY(2) = SDEST2
       XVERIFY(3) = SDEST3

       XVERIFY(4) = SCALINFO
       CALL VERIFYVAL(4,XVERIFY)



C      IF(ISUPRES .EQ. 0) WRITE(*,6093) SDEST1,SDEST2,SDEST3,SCALINFO
       IF(ISUPRES .EQ. 0) WRITE(*,6093) (XVERIFY(IXV),IXV=1,4) 

C      WRITE(25,6093) SDEST1,SDEST2,SDEST3,SCALINFO
       WRITE(25,6093) (XVERIFY(IXV),IXV=1,4)
 6093 FORMAT(1X,4(G15.8,2X))

       XMED(IND) = X50


 6090  CONTINUE

C        write (*,*) "6090 Calculated Means, Medians, and Modes"

        IF(ISUPRES .EQ. 0) WRITE(*,*)

        WRITE(25,*)

C  WRITE IERRMOD AND GAMLAM TO FILE 25. NOTE THAT FOR IERRMOD = 1,
C  GAMMA WAS NOT ESTIMATED, SO USE A VALUE OF -99 FOR GAMMA.

        IF(IERRMOD .EQ. 1) GAMLAM = -99.D0
        IF(IERRMOD .EQ. 2) GAMLAM = GAMMA
        IF(IERRMOD .EQ. 3) GAMLAM = GAMMA
        IF(IERRMOD .EQ. 4) GAMLAM = FLAT*GAMMA
        WRITE(25,5454) 
 5454   FORMAT(/' IERRMOD AND THE ESTIMATE FOR GAMLAM ARE: ')
        WRITE(25,*) IERRMOD, GAMLAM
        IF(IERRMOD .EQ. 1) WRITE(25,5456)
 5456    FORMAT(/' WHICH MEANS THAT GAMMA WAS NOT ESTIMATED IN THIS RUN.
     1 ')

      write (*,*) "IF(NRANFIX .GT. 0 .AND. ...",NRANFIX,ICYCLE

      IF(NRANFIX .GT. 0 .AND. ICYCLE .EQ. 1) THEN


C  IF NRANFIX .GT. 0, CALL ELDERY TO GET UPDATED ESTIMATES FOR THESE
C  NRANFIX PARAMETERS WHICH ARE UNKNOWN BUT THE SAME FOR ALL SUBJECTS.
C  ALSO AS OF npagranfix2.f., INCLUDE THE NVAR RANDOM VARIABLES AS 
C  PARAMETERS WHOSE NEW ESTIMATES WILL BE FOUND BY ELDERY. THE
C  INITIAL ESTIMATES FOR THESE PARAMETERS WILL BE THEIR CURRENT CYCLE
C  MEANS. NOTE THAT THIS WILL ONLY BE DONE AT END OF CYCLE NO. 1.

C  PREPARE TO CALL ELDERY. 

C  SET EACH RANDOM PARAMETER = ITS MEAN VALUE AND EACH FIXED PARAMETER
C  = ITS VALUE IN VALFIX.

      DO I = 1,NVAR
       X(I) = EX(I)
      END DO

C      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,RANFIXEST,PX)
      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,RANFIXEST,PX)
        NPX = NVAR+NOFIX+NRANFIX
C       if (NPX < max_ODE_params) then exit progam w/error
      IPAR(i_skip_ig)=1
C wmy2018.03.07 -- Send local variable to MAKEVEC, then update COMMON
C   block variables P and PX
C        DO I=1,max_ODE_params
C          PX(I) = PCOPY(I)
C          P(I) = PCOPY(I)
C        END DO

C  NOTE THAT PX NOW INCLUDES THE VALUES OF THE RANDOM PARAMETERS (SET
C  = TO THEIR MEANS FROM THE JUST COMPLETED CYCLE) AND THE FIXED 
C  PARAMETER VALUES. AND IT ALSO HAS THE VALUES IN RANFIXEST(.) IN THE
C  APPROPRIATE NRANFIX ENTRIES (I.E., FOR THOSE PARAMETERS WITH 
C  IRAN(.) = 2), BUT THESE LAST VALUES OF COURSE WILL BE RESET IN 
C  SUBROUTINE CALCRF EACH TIME IT IS CALLED BY ELDERY WITH ANOTHER SET 
C  OF VALUES SUPPLIED IN THE CANDIDATE VECTOR, VEC(.). AND, AS OF
C  npagranfix2.f., THE NVAR PARAMETER VALUES FOR THE RANDOM VARIABLES
C  WILL ALSO BE RESET IN SUBROUTINE CALCRF TO THEIR CANDIDATE VALUES
C  FROM VEC(.).

C  TO START THE PROCESS TO FIND THE BEST ESTIMATES FOR THE NRANFIX
C  PARAMETERS WITH IRAN(.) = 2, SINCE THE CURRENT ESTIMATES FOR THESE 
C  PARAMETERS ARE IN RANFIXEST(.), I=1,NRANFIX, THESE WILL BE THE 
C  STARTING ESTIMATES FOR THIS CALL TO ELDERY.

C  ALSO, AS OF npagranfix2.f., THE INITIAL ESTIMATES FOR THE NVAR
C  RANDOM VARIABLES WILL BE THE MEANS FROM THE JUST COMPLETED CYCLE.

       DO I = 1,NRANFIX
        START(I) = RANFIXEST(I)
        STEP(I) = -.2D0*START(I)
       END DO

       DO I = NRANFIX+1,NRANFIX+NVAR
        START(I) = EX(I-NRANFIX)
        STEP(I) = -.2D0*START(I)
       END DO

C       write (*,*) "CALL ELDERY(...",START

C wmy20190722 -- /TOCALC/ should be passed to CALCRF via ELDERY
C  COMMON/TOCALC/gamma,flat,AB,PX,IRAN,NOFIX,NSUB

       CALL ELDERY(NRANFIX+NVAR,START,OPTVAR,VALMIN,1.D-10,STEP,1000,
     1  CALCRF,0,ICONV,NITER,ICNT,NUMEQT,YO,C0,C1,C2,C3,C4,C5,NBCOMP,
     2  NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,INTLIST,
     3  IPAR,ObsError,RPAR,gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     4  errfilname)


C  OPTVAR(I),I=1,NRANFIX+NVAR = THE UPDATED SET OF ESTIMATES FOR THE
C    NRANFIX PARAMETERS WITH IRAN(.) = 2, AND THE NVAR PARAMETERS WITH

C       write (*,*) "Ret. from ELDERY()"
C       write (*,*) "IRAN", IRAN
C       write (*,*) OPTVAR

C    IRAN(.) = 1 (SEE ABOVE). 

C  VALMIN = MIN. VALUE OF FUNCTION ACHIEVED.

C  ICONV = 1 IF THE ESTIMATE CONVERGED; 0 OTHERWISE.


        IF(ICONV .EQ. 0 .AND. ISUPRES .EQ. 0) WRITE(*,9011) 
        IF(ICONV .EQ. 0) WRITE(25,9011) 
 9011 FORMAT(' ',' NO CONVERGENCE THIS CYCLE ON ESTIMATES FOR THE'/
     1' RANFIX AND RANDOM PARAMETERS. '/)
 
 
C  ESTABLISH THE NEW SET OF RANFIX AND RANDOM VARIABLES.

       DO I = 1,NRANFIX
        RANFIXEST(I) = OPTVAR(I)
       END DO

       DO I = NRANFIX+1,NRANFIX+NVAR
        EXO(I-NRANFIX) = OPTVAR(I)
       END DO


C  RESET THE VALUES IN CORDEN TO REFLECT THE FACT THAT THE MEANS OF
C  THE NVAR RANDOM VARIABLES SHOULD NOW BE EXO(.) RATHER THAN EX(.).
C  SO 

       DO J = 1,NVAR
        DO IG = 1,NACTVE
         CORDEN(IG,J) = CORDEN(IG,J)*EXO(J)/EX(J)
        END DO
       END DO


C  WRITE THE ESTIMATES FOR THESE NRANFIX RANFIX PARAMETERS FOR THIS 
C  CYCLE, AS WELL AS FOR THE REVISED NVAR RANDOM PARAMETERS.


        IF(ISUPRES .EQ. 0)  WRITE(*,9012) NRANFIX
        WRITE(25,9012) NRANFIX

 9012 FORMAT(//' FOR THIS CYCLE, THE ESTIMATES FOR THE ',I2,' PARAMETERS
     1'/
     2' WHICH ARE UNKNOWN BUT THE SAME FOR ALL SUBJECTS ARE: ')

        IF(ISUPRES .EQ. 0) WRITE(*,5104) (PARRANFIX(I),I=1,NRANFIX)

        WRITE(25,5104) (PARRANFIX(I),I=1,NRANFIX)
 
      DO I = 1,NRANFIX
       XVERIFY(I) = RANFIXEST(I)
      END DO
      CALL VERIFYVAL(NRANFIX,XVERIFY)      

      IF(ISUPRES .EQ. 0) WRITE(*,5103) (XVERIFY(I),I=1,NRANFIX)
      WRITE(25,5103) (XVERIFY(I),I=1,NRANFIX)


      IF(ISUPRES .EQ. 0)  WRITE(*,9013) NVAR
      WRITE(25,9013) NVAR

 9013 FORMAT(//' FOR THIS CYCLE, THE REVISED MEANS FOR THE ',I2,/
     1' RANDOM PARAMETERS ARE: ')

       IF(ISUPRES .EQ. 0) WRITE(*,5104) (PAR(I),I=1,NVAR)
       WRITE(25,5104) (PAR(I),I=1,NVAR)
 
      DO I = 1,NVAR
       XVERIFY(I) = EXO(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)      

      IF(ISUPRES .EQ. 0) WRITE(*,5103) (XVERIFY(I),I=1,NVAR)
      WRITE(25,5103) (XVERIFY(I),I=1,NVAR)


      ENDIF
C  ABOVE ENDIF IS FOR THE  IF(NRANFIX .GT. 0 .AND. NRANFIX .EQ. 1)
C    CONDITION.




        IF(MAXCYC .EQ. 0) GO TO 900

C  Starting with bigmlt1.f, this is a jump point.


cend statistics

        write (*,*) "Begin control"

cbegin control
c we are now done wtih statistics - this is the best place to
c check for whether we can exit - if so , last printed statistic
c will agree with current density corden, and corden is still
c correct (e.g. after condensation-expansion, it is no longer
c correct until we call emint again)
cint.9 control section to check for exit criteria, resolution 
c  refinement, and end of major cycles

cint9.a  first, we exit if we have reached maxcyc on cycle counter

C  SET IMAXCYC = 0; IF IT CHANGES TO 1, IT MEANS THAT MAXCYC CYCLES
C  HAVE BEEN RUN, AND THE PROGRAM WILL STOP.

        IMAXCYC = 0

      if(icycle .ge. maxcyc) then


C  IF ICONTIN = 0, IT MEANS THE USER EXECUTED THE BATCH FILE,
C  CHMAXCYC.BAT TO STOP THE PROGRAM PREMATURELY (BY RESETTING MAXCYC
C  TO BE = THE CURRENT VALUE OF ICYCLE). IN THIS CASE, WRITE A
C  COMMENT TO FILE 25 AND TO THE SCREEN.

        IF(ICONTIN .EQ. 0) THEN
         WRITE(25,1261) MAXCYC,MAXCYC0

 1261    FORMAT(/' THE USER CHOSE TO STOP THE PROGRAM AT CYCLE NO. '/
     1' ',I7,' ... THE ORIGINAL NO. OF MAXIMUM CYCLES WAS ',I7//)
         WRITE(*,1261) MAXCYC,MAXCYC0
C        CALL PAUSE
        ENDIF

C  SET IMAXCYC = 1 --> MAXCYC WAS REACHED.

        IMAXCYC = 1

C  COMMENT OUT THE GO TO 900 STATEMENT BELOW SINCE EVEN IF ICYCLE
C  = MAXCYC, THE PROGRAM STILL NEEDS TO TEST TO SEE IF CONVERGENCE
C  WAS ACHIEVED IN THE FINAL CYCLE.
C      go to 900

      endif

c  The above endif is for the  if(icycle .ge. maxcyc)  condition.


C  SET ICONVERG = 0; IF IT CHANGES TO 1, IT MEANS THAT CONVERGENCE HAS
C  BEEN ACHIEVED, AND THE PROGRAM WILL STOP.

        ICONVERG = 0

cint9.b  second, we check improvement from last cycle




      ximprove=fobj-prefobj

       XVERIFY(1) = fobj1
       XVERIFY(2) = gamma*flat
       XVERIFY(3) = resolve
       CALL VERIFYVAL(3,XVERIFY)



C      write(91,9191) icycle,fobj1,gamma*flat,resolve,
       write(91,9191) icycle,(XVERIFY(IXV),IXV=1,3),
     &nactve1,nactve0
 9191 format(i5,2f15.6,5x,f8.5,5x,2i5)

      IF(ISUPRES .EQ. 0) 
     1 write(6,*) 'icycle=',icycle,' fobj=',fobj,' resolution=',resolve
      IF(ISUPRES .EQ. 0) 

     1 write(6,*) 'improvement from last cycle =',ximprove
      IF(ISUPRES .EQ. 0 .AND. ierrmod.eq.2) 
     1 write(6,*) 'current gamma=',gamma
      IF(ISUPRES .EQ. 0 .AND. ierrmod.eq.3) 
     1 write(6,*) 'current additive lambda=',gamma
      IF(ISUPRES .EQ. 0 .AND. ierrmod.eq.4) 
     1 write(6,*) 'current flat weight = ',flat*gamma

      prefobj = fobj

cint9.c if ximprove is too low, refine the resolve criterion

      if(dabs(ximprove) .le. tol .and. resolve .gt. 0.0001) then
        resolve=resolve*0.5D0
        rpar(k_resolve) = resolve
      endif

cint9.d check to see if resolve bottoms out - if so, start a new
c major cycle by resetting it to its highest allowable value, or


c exit if the improvment from the last major cycle is too small ...

C  AND EXIT IF IMAXCYC = 1 (SEE ABOVE; THIS MEANS THAT THE MAX. NO.
C  OF CYCLES HAS ALREADY BEEN RUN AND THE ONLY REASON THIS PART OF THE
C  CODE IS BEING RUN IS TO SEE IF CONVERGENCE WAS ACHIEVED IN THE FINAL
C  CYCLE.


C      write (*,*) "Checking resolve"

      if(resolve.le.0.0001) then


c      saveres = resolve
        resolve=0.20000000298023224
        rpar(k_resolve) = 0.20000000298023224
        checkbig = fobj - prebig
      write(91,*) 'res set to .2 ',' checkbig=',checkbig
        prebig =fobj

C  PRINT OUT THE ONE LINE OF INFO IF ISUPRES = 1. NOTE THAT THE 
C  CONVERGENCE CRITERION IS THAT DABS(CHECKBIG) .LE. TOLC. ALSO NOTE
C  THAT XMED(I) ARE THE MEDIANS FROM THE JUST COMPLETED CYCLE.

         IF(ISUPRES .EQ. 1) THEN



         WRITE(*,1023) ICYCLE
 1023    FORMAT(/' FOR CYCLE NO, ',I6,' THE CONVERGENCE CRITERION AND ME
     1DIANS ARE: ')

       XVERIFY(1) = checkbig
       XVERIFY(2) = TOLC
       CALL VERIFYVAL(2,XVERIFY)



C      WRITE(*,1024) DABS(checkbig),TOLC
       WRITE(*,1024) DABS(XVERIFY(1)),XVERIFY(2)
 1024    FORMAT(1X,G14.4,' <-- CONVERGENCE OCCURS WHEN THIS NO. < ',F20.
     117)
       WRITE(*,5104) (PAR(I),I=1,NVAR)

       DO I = 1,NVAR
        XVERIFY(I) = XMED(I)
       END DO
       CALL VERIFYVAL(NVAR,XVERIFY) 
     



C      WRITE(*,5103) (XMED(I),I=1,NVAR)
       WRITE(*,5103) (XVERIFY(IXV),IXV=1,NVAR)



        ENDIF

c  As of npageng23.f, the tolerance value against which checkbig is
c  compared is now longer hardcoded to be .01. Instead it is input
c  from npag102.inp (now npag103.inp) as TOLC.

        if(dabs(checkbig) .le. TOLC) then

C  SET ICONVERG = 1 --> CONVERGENCE WAS ACHIEVED.

        ICONVERG = 1

          go to 900

        endif

      endif

c  above endif is for the  if(resolve .le. .0001)  condition.


C  IF IMAXCYC = 1, THE MAX. NO. OF CYCLES HAVE ALREADY BEEN RUN -->
C  GO TO 900. THE ONLY REASON THIS PART OF THE CODE WAS BEING RUN IS TO


C  SEE IF CONVERGENCE WAS ACHIEVED IN THIS FINAL CYCLE, AND THAT WAS
C  JUST TESTED ABOVE (SEE ICONVERG CODE).
 
        IF(IMAXCYC .EQ. 1) GO TO 900


cend control



cbegin expansion ! Replaced w/expand_grid()

      alg_type = 0
      isupres = 0
      call expand_grid(alg_type,isupres,nvar,nactve,ngridn,
     1    resolve, corden, ab)
      isupres = 0

C------ replaced code ---
c
c          write (*,*) "Begin expansion"
c
c      IF(ISUPRES .EQ. 0) write(6,*) 'Number of active points =', nactve
c now add more points near the current solution
c
c      IF(ISUPRES .EQ. 0) write(6,*)
c      IF(ISUPRES .EQ. 0) 
c     1 write(6,*) 'expanding current grid with new points'
c      IF(ISUPRES .EQ. 0) write(6,5200) 100.*resolve
c
c 5200 format(' current grid resolution = ',f8.3, '%')
c         new=2*nvar+1
c
c         nactveold=nactve
c
c         do ipoint=1,nactveold
c first, divide current probability into 2*nvar+1 pieces
c
c           pcur=corden(ipoint,nvar+1)/(2*nvar+1)
c update original point
c           corden(ipoint,nvar+1)=pcur
c
c             do ivar=1,nvar
c               del=(ab(ivar,2)-ab(ivar,1))*resolve
c create first new trial point at -eps in coordinate ivar
c               do i=1,nvar
c                  corden(nactve+1,i)=corden(ipoint,i)
c               enddo
c               corden(nactve+1,ivar)=corden(nactve+1,ivar)-del
c               corden(nactve+1,nvar+1)=pcur
c               ntry=nactve+1
c icheck that new point is at least minimally distant from old points
c
c               call checkd(corden,ntry,nactve,ab,maxgrd,nvar,iclose)
c only keep trial lower point if it lies above lower bound and satisfies
c minimal distance requirement
c             if(corden(nactve+1,ivar).ge.ab(ivar,1)) then
c
c                if(iclose.eq.0) nactve=nactve+1
c              endif
c now create second trail point at +eps in coordinate ivar
c               do i=1,nvar
c                 corden(nactve+1,i)=corden(ipoint,i)
c              enddo
c              corden(nactve+1,ivar)=corden(nactve+1,ivar)+del
c               corden(nactve+1,nvar+1)=pcur
c only keep upper point if it lies below upper bound and
c satisfies distance requirement
c               ntry=nactve+1
c               call checkd(corden,ntry,nactve,ab,maxgrd,nvar,iclose)
c           if(corden(nactve+1,ivar).le.ab(ivar,2)) then
c
c             if(iclose.eq.0) nactve=nactve+1
c
c               endif
c             enddo
c    above enddo for loop over ivar=1,nvar
c
c           enddo
c    above enddo for loop over ipoint=1,nactveold
c
c      IF(ISUPRES .EQ. 0) 
c     1 write(6,*) 'Number of actve grid points after expansion =',nactve
c      ngridn=nactve
c      IF(ISUPRES .EQ. 0) write(6,*)
c
cend expansion
c
C------ end replaced code ---

c go to begin new cycle

        prefobj=fobj


        GO TO 1001
 
  900 continue

C       write (*,*) "Passed 900"

c  As of npagranfix2.f, write the estimates for the parameters which are
c  unknown but the same for all subjects, if applicable. Note that 
c  these estimates were obtained only after cycle no. 1 and are now
c  written out again at the end of the run.

      IF(NRANFIX .GT. 0) THEN

       DO I = 1,NRANFIX
        XVERIFY(I) = RANFIXEST(I)
       END DO
       CALL VERIFYVAL(NRANFIX,XVERIFY) 

       WRITE(25,9014) NRANFIX

 9014 FORMAT(//' THE ESTIMATES FOR THE ',I2,' PARAMETERS WHICH ARE '/
     1' UNKNOWN BUT THE SAME FOR ALL SUBJECTS, AND WERE FOUND AT THE'/
     2' END OF CYCLE NO. 1, ARE: ')

       WRITE(25,5104) (PARRANFIX(I),I=1,NRANFIX)
       WRITE(25,5103) (XVERIFY(I),I=1,NRANFIX)


       IF(ISUPRES .EQ. 0) THEN
        WRITE(*,9014) NRANFIX
        WRITE(*,5104) (PARRANFIX(I),I=1,NRANFIX)
        WRITE(*,5103) (XVERIFY(I),I=1,NRANFIX)
       ENDIF

      ENDIF
C  THE ABOVE ENDIF IS FOR THE  IF(NRANFIX .GT. 0)  CONDITION.



C  AS OF npageng18.f, CONTROL CAN BE TRANSFERRED TO LABEL 900 DIRECTLY
C  AFTER RETURNING FROM A CALL TO SUBROUTINE emint. THIS HAPPENS WHEN
C  IHESS = -1, WHICH MEANS THAT THE HESSIAN MATRIX IN THE INTERIOR
C  POINT EM ALGORITHM WAS SINGULAR. RATHER THAN SIMPLY STOPPING AS IT
C  DID PREVIOUSLY, NOW THE PROGRAM WILL CREATE THE OUTPUT FILES BEFORE
C  STOPPING ... BASED ON THE VALUES FROM THE PREVIOUS CYCLE.
C  FIRST, WRITE THE REASON FOR STOPPING AS ICONVERGE = 3 BELOW. THEN
C  RESET CORDEN BACK TO CORDLAST (SEE ABOVE), WHICH WAS THE CORDEN
C  AT THE END OF THE PREVIOUS CYCLE.


C  WRITE WHY THE PROGRAM STOPPED.

        WRITE(25,5197)
 5197   FORMAT(//' THIS RUN STOPPED WITH ICONVERGE = ')

        IF(IHESS .EQ. -1) THEN

         WRITE(25,6002)
 6002    FORMAT(' 3 <-- THE PROGRAM STOPPED DUE TO HESSIAN ERROR.')


         NACTVE = NACTLAST

         DO I = 1,NACTVE
          DO J = 1,NVAR+1
           CORDEN(I,J) = CORDLAST(I,J)

          END DO
         END DO

         GO TO 910

        ENDIF



        IF(ICONVERG .EQ. 1 .AND. IMAXCYC .EQ. 1) WRITE(25,5198)
 5198   FORMAT(' 2 <-- THE PROGRAM CONVERGED AT MAXCYC CYCLES.')
        IF(ICONVERG .EQ. 1 .AND. IMAXCYC .EQ. 0) WRITE(25,5199)
 5199   FORMAT(' 1 <-- THE PROGRAM CONVERGED PRIOR TO MAXCYC CYCLES.')
        IF(ICONVERG .EQ. 0 .AND. IMAXCYC .EQ. 1) WRITE(25,6001)
 6001   FORMAT(' 0 <-- THE PROGRAM RAN MAXCYC CYCLES WITHOUT CONVERGING.
     1')


C  Starting with bigmlt1.f, this is an entry point to continue 
c  calculations


  910 CONTINUE

C        write (*,*) "Passed 910"

cbegin endgame
c we can only arrive here from the control section, which menas
c that we ahve completed optimizaiton but not done the subsequent
c expansion.  This means that the density is correct, and we can safely
c just write it out and exit


C  SINCE THE ANALYSIS IS OVER:
C  ESTABLISH THREE VECTORS, YPREDPOP, YPREDPOPT, AND YPREDBAY, WHICH 
C  WILL BE STORED INTO THE DENSITY FILE FOR USE BY THE PC PROGRAM.
 
C  ESTABLISH YPREDPOP(MAXSUB,NUMEQT,MAXOBS,3), WHERE

C  YPREDPOP(JSUB,J,IOBS,ICEN) = THE PREDICTED VALUE FOR SUBJECT
C  JSUB, OUTPUT EQ. J, OBSERVATION IOBS, AND ICEN (ICEN = 1 (MEANS),
C  2 (MEDIANS), AND 3 (MODES), WHERE THE MEANS, MEDIANS, AND MODES ARE
C  FROM THE FINAL CYCLE POPULATION DENSITY).

C  ESTABLISH YPREDPOPT(MAXSUB,NUMEQT,7201,3), WHERE

C  YPREDPOPT(JSUB,J,T,ICEN) = THE PREDICTED VALUE FOR SUBJECT
C  JSUB, OUTPUT EQ. J, AT TIME T, AND ICEN (ICEN = 1 (MEANS),
C  2 (MEDIANS), AND 3 (MODES), WHERE THE MEANS, MEDIANS, AND MODES ARE
C  FROM THE FINAL CYCLE POPULATION DENSITY), AND T IS A VALUE IN
C  TPRED, ESTABLISHED IN THE CALL TO CALCTPRED - SEE BELOW.

C  ESTABLISH YPREDBAY(MAXSUB,NUMEQT,MAXOBS,3), WHERE
C  YPREDBAY(JSUB,J,IOBS,ICEN) = THE PREDICTED VALUE FOR SUBJECT
C  JSUB, OUTPUT EQ. J, OBSERVATION IOBS, AND ICEN (ICEN = 1 (MEANS),
C  2 (MEDIANS), AND 3 (MODES), WHERE THE MEANS, MEDIANS AND MODES ARE
C  FROM SUBJECT'S JSUB BAYSESIAN POSTERIOR DENSITY (CALCULATED BY
C  SUBROUTINE SUBRES)).
 

C  ALSO, ESTABLISH PREDICTED VALUES SIMILAR TO YPREDBAY, BUT WHICH WILL
C  BE AT TIMES SPECIFIED BY IDELTA (SEE SUBROUTINE CALCTPRED) AND ONLY 
C  AT ONE SET OF PARAMETER VALUES (MEANS, MEDIANS, OR MODES OF THE 
C  BAYESIAN POSTERIOR DIST.) SPECIFIED BY ICENT (SEE DETAILS IN TOP OF 
C  CODE FOR m2_8calc.f). THESE VALUES WILL BE PUT INTO FILE PREDFIL. IN 
C  ADDITION, CALCULATE TOTAL AND 24 HOUR AUC'S AND AUC/MIC'S, WHICH WILL 

C  BE PUT INTO THE OUTPUT FILE, ACCOMPANYING EACH SUBJECT'S BAYESIAN 
C  POSTERIOR INFO.
C!! NO. AS OF npageng25.f, BOTH THE PREDICTED VALUES PUT INTO PREDFIL,
C  AND THE AUCs WILL BE CALCULATED FOR ALL OF THE MEANS, MEDIANS, AND
C  MODES (I.E., ICENT IS NO LONGER USED).

 
C 1ST CALCULATE YPREDPOP AND YPREDPOPT VALUES.
 
         REWIND(27)

 
C  FOR EACH SUBJECT, FIND THE PREDICTED VALUES (VIA SUBROUTINE IDCALCY
C  FOR YPREDPOP, AND VIA IDCALCYY FOR YPREDPOPT).
 
C  NOTE THAT THE LAST CYCLE'S MEANS ARE IN   CENTER(1,J), J = 1,NVAR;
C                             MEDIANS ARE IN CENTER(2,J), J = 1,NVAR;
C                             MODES ARE IN   CENTER(3,J), J = 1,NVAR.
 

C wmy2018.10.16 Moved CALL SYMBOL out of IDCALCY and IDCALCYY (similar
C  to moving CALL SYMBOL out of IDPC -- I'm not really convinced that
C  this routine does anything. I believe it's original purpose was to
C  "relate" the parameter names in the model file to parameters in a
C  support.  But I can't find reference to the PSYM(:) array, that 
C  these names are written. SYMBOL also is supposed to initialize the
C  NBCOMP array, which tells NPAG where bolus of a drug goes; but can't
C  figure out how this happens.
C        CALL SYMBOL(NBCOMP) ... moved to CALL IDCALCY and CALL IDCALCYY, below

        IPAR(i_do) = 6000
        DO 6000 JSUB=1,NSUB

        write (*,*) "DO 6000", JSUB 
 
        CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,
     1    TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     2    INTLIST,ERRFILNAME)

        call cp_lrcs_to_rpar(gamma,flat,numeqt,c0,c1,c2,c3,c4,c5,rpar)

C        write (*,*) "Ret. from FILRED() nr. #5700"

         DO ICENTER = 1,3

         DO J=1,NVAR
           EXXX(J) = CENTER(ICENTER,J)
         END DO

C  FIND YPREDPOP FIRST:  
C  CALL SUBROUTINE IDCALCY, A VERSION OF THE ID PROGRAM WHICH SIMPLY
C  CALCULATES THE PREDICTED VALUES OF YO(I,J) = OUTPUT CONCENTRATION OF
C  THE JTH OUTPUT EQUATION (J=1,NUMEQT) AT THE ITH OBSERVATION TIME
C  (I=1,NOBSER), ASSUMING THE PARAMETER VECTOR EXXX ...
 

C  BEFORE ALL CALLS TO IDCALCY, MUST INTEGRATE FIXED AND RANDOM
C  VALUES INTO PX, USING IRAN(I),I=1,NVAR+NOFIX. CALL MAKEVEC TO DO
C  THIS. AS OF npagranfix.f, ADD NRANFIX AND RANFIXEST
C  TO THE ARGUMENT LIST TOO (I.E., THERE IS NOW A 3RD TYPE OF 
C  PARAMETER (THOSE WHICH ARE UNKNOWN BUT THE SAME FOR ALL SUBJECTS).

C      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,EXXX,VALFIX,RANFIXEST,PX)
      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,EXXX,VALFIX,RANFIXEST,PX)
        NPX = NVAR+NOFIX+NRANFIX
C       if (NPX < max_ODE_params) then exit progam w/error
      IPAR(i_skip_ig)=1

C wmy2018Jan12 I removed the COMMON/PARAMD/P from SUBROUTINE IDCALCY and
c  IDCALCYY, so P must be initialized in main after calling MAKEVEC, but
c  before calling them. Note: IDCALCY and -YY now call SYMBOL and then 
c  immediately call FUNC2() or FUNC3(). 

C wmy2018.03.07 -- Send local variable to MAKEVEC, then update COMMON
C   block variables P and PX
C        DO I=1,NPX
C          PX(I) = PCOPY(I)
C           P(I) = PCOPY(I)
C        END DO

c        DO I=1,NPX
c          P(I) = PX(I)
c        END DO
C In above DO, NVAR+NOFIX+NRANFIX is replaced by NPX
       
C         write (*,*) JSUB,"DO 6000 Calling IDCALCY", NVAR+NOFIX+NRANFIX
c     1     ,NDIM
c     2      ,PX(21),YPRED(1,2),NUMEQT,NOBSER,NDIM,MF,RTOL,ATOL,MF
c         write (*,*) "Where did all the 0.0s come from?"

C wmy2017Sep13 -- printed values of the above look OK; but are followed
C   by a list of sixteen 0.00s 

C        CALL IDCALCY(NVAR+NOFIX+NRANFIX,NDIM,PX,YPRED,NUMEQT)
C       real*8, save, dimension(max_doses,max_RS_J) :: RSCOPY
C        rrr = 0.0
C        do III=1,max_doses
C          do JJJ =1,max_RS_J
C            rrr = rrr + rscopy(III,JJJ) - rs(III,JJJ) 
C            rscopy(iii,jjj) = rs(iii,jjj)
C            if (jjj.le.max_input_dim) bscopy(iii,jjj) = bs(iii,jjj)
C          end do
C        end do
C        write (*,*) "calling IDCALCY with rrr=",rrr

        CALL SYMBOL(NBCOMP)

        CALL IDCALCY(JSUB,IG,NPX,NDIM,PX,YPRED,NUMEQT,
     1      NOBSER,MF,NBCOMP,RTOL,ATOL,
     2      TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     3      INTLIST,IPAR,ObsError,RPAR,ERRFILNAME)

C wmy2017Jan12 IDCALC receives params as ...
c        SUBROUTINE IDCALCY(JSUB,IG,NPP,NDIM,ESTML,YPRED,NUMEQT
c     1    ,NOBSER,MF,RTOL,ATOL,RSCOPY,BSCOPY,INTLIST,IPAR,ObsError)


C         write (*,*) "DO 6000 Return from IDCALCY", JSUB, ICENTER

C  NOTE: PREDICTED VALUES WERE FOUND EVEN FOR OBSERVED LEVELS WHICH ARE
C  MISSING (I.E., OBSERVED LEVEL = -99) SINCE IT IS EASIER TO CALCULATE
C  ALL PREDICTED VALUES THAN TO KEEP TRACK OF WHICH DO AND WHICH DON'T
C  NEED TO BE CALCULATED.
 
C  STORE YPRED INTO YPREDPOP(JSUB,.,.,ICENTER)
 
         DO IOBS=1,NOBSER
         DO IEQ=1,NUMEQT
           YPREDPOP(JSUB,IEQ,IOBS,ICENTER) = YPRED(IOBS,IEQ)
         END DO
         END DO



C  FIND YPREDPOPT: 
C  CALL SUBROUTINE IDCALCYY, A VERSION OF THE ID PROGRAM WHICH SIMPLY
C  CALCULATES THE PREDICTED VALUES OF Y(I,J) (OUTPUT CONCENTRATION
C  OF THE JTH OUTPUT EQ. AT TIME TPRED(I),I=1,NUMT(JSUB)), ASSUMING THE
C  PARAMETER VECTOR EXXX. NOTE THAT IDCALCYY DIFFERS FROM IDCALCY IN
C  THAT THE TIMES FOR THE PREDICTED VALUES ARE SUPPLIED IN TPRED, 
C  RATHER THAN INPUT VIA COMMON/OBSER FROM THE PATIENT'S DATA FILE. 
C  ALSO, THE NO. OF OBSERVED TIMES IS NUMT(JSUB), RATHER THAN M WHICH IS 
C  SUPPLIED VIA COMMON/SUM2. AND NOTE THAT NUMT(JSUB) AND TPRED(.) ARE 
C  FOUND FROM THE CALL TO CALCTPRED BELOW.

C  CALL CALCTPRED TO CALCULATE THE NUMT(JSUB) TIMES TO BE IN TPRED FOR 
C  THIS SUBJECT. NOTE THAT, AFTER THE CALL TO FILRED ABOVE FOR THIS 
C  SUBJECT, THE NO. OF OBSERVED VALUE TIMES = NOBSER, AND THESE VALUES 
C  ARE IN ARRAY, TIMOB.

C  AS OF npageng16.f, TIMOBB IS NO LONGER NEEDED AS AN ARGUMENT TO
C  CALCTPRED, SINCE NOW CALCTPRED HAS COMMON/OBSER (WHICH PROVIDES
C  TIMOB) IN IT.

C  AS OF npageng18.f, CALCTPRED ALSO RETURNS TPREDREL, WHICH GIVES THE
C  "RELATIVE" RATHER THAN "REAL" TIMES AFTER A STEADY STATE DOSE SET.
C  THESE VALUES ARE THE ONES WHICH WILL BE WRITTEN TO THE OUTPUT FILES
C  FROM NOW ON.


      CALL CALCTPRED(JSUB,IDELTA,NOBSER,NUMT(JSUB),TPRED,TPREDREL,
     1   NOMAXTIM(JSUB),TEND,TBEGG,TIMCOPY,SIGCOPY,INTLIST)

C         write (*,*) "Return from CALCTPRED", JSUB, ICENTER

C  STORE THE TIMES FOR THE PREDICTED CONCENTRATIONS INTO TTPRED. THEY
C  WILL BE NEEDED BELOW. ALSO STORE THE MAXIMUM ENDING TIME + 24 HOURS 
C  (TEND), AND BEGINNING TIME (TBEGG) FOR THIS SUBJECT ... FOR EACH
C  TIME RESET AND THE OVERALL MAXIMUM TIME. THESE VALUES WILL BE NEEDED
C  WHEN SUBJECT AUCs ARE CALCULATED BELOW.

C  ALSO, AS OF npageng18.f, STORE THE "RELATIVE" TIMES INTO TTPREDREL
C  (TTPRED STORES THE "REAL" TIMES). THESE ARE THE VALUES WHICH WILL BE
C  WRITTEN TO THE OUTPUT FILES.

        DO J = 1,NUMT(JSUB)
          TTPRED(JSUB,J) = TPRED(J)
          TTPREDREL(JSUB,J) = TPREDREL(J)
        END DO

        DO J = 1,NOMAXTIM(JSUB)
          TENDSUB(JSUB,J) = TEND(J)
          TBEGGSUB(JSUB,J) = TBEGG(J)
        END DO


C  BEFORE ALL CALLS TO IDCALCYY, MUST INTEGRATE FIXED AND RANDOM
C  VALUES INTO PX, USING IRAN(I),I=1,NVAR+NOFIX. CALL MAKEVEC TO DO
C  THIS. AS OF npagranfix.f, ADD NRANFIX AND RANFIXEST
C  TO THE ARGUMENT LIST TOO (I.E., THERE IS NOW A 3RD TYPE OF 
C  PARAMETER (THOSE WHICH ARE UNKNOWN BUT THE SAME FOR ALL SUBJECTS).

      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,EXXX,VALFIX,RANFIXEST,PX)
        NPX = NVAR+NOFIX+NRANFIX
C       if (NPX < max_ODE_params) then exit progam w/error
      IPAR(i_skip_ig) = 1

C wmy2018Jan12 I removed the COMMON/PARAMD/P from SUBROUTINE IDCALCY and
c  IDCALCYY, so P must be initialized in main after calling MAKEVEC, but
c  before calling them. Note: IDCALCY and -YY now call SYMBOL and then
c  immediately call FUNC2() or FUNC3(); bypassing SUBROUTINES EVAL2 and -3
C        DO I=1,NVAR+NOFIX+NRANFIX
C        DO I=1,NPX
C          PX(I) = PCOPY(I)
C           P(I) = PCOPY(I)
C        END DO

c      write (*,*) "CALL IDCALCYY", NVAR+NOFIX+NRANFIX,NDIM,PX(8),
c     1  TPRED(5),NUMT(JSUB),NUMEQT,NOBSER,MF,RTOL,ATOL(1)

C wmy2017Sep20 -- call from the original npagranfix.f
C        CALL IDCALCYY(NVAR+NOFIX+NRANFIX,NDIM,PX,TPRED,NUMT(JSUB),
C     1  YYPRED,NUMEQT)

C       CALL IDCALCYY(JSUB,IG,NVAR+NOFIX+NRANFIX,NDIM,PX,TPRED,
C        rrr = 0.0
C        do III=1,max_doses
C          do JJJ =1,max_RS_J
C            rrr = rrr + rscopy(III,JJJ) - rs(III,JJJ) 
C            rscopy(iii,jjj) = rs(iii,jjj)
C            if (jjj.le.max_input_dim) bscopy(iii,jjj) = bs(iii,jjj)
C          end do
C        end do
C        write (*,*) "calling IDCALCYY with rrr=",rrr

        CALL SYMBOL(NBCOMP)

        CALL IDCALCYY(JSUB,IG,NPX,NDIM,PX,TPRED,
     1      NUMT(JSUB), YYPRED,NUMEQT,NOBSER,MF,NBCOMP,RTOL,ATOL,
     2      TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     3      INTLIST,IPAR,ObsError,RPAR,ERRFILNAME)

C        write (*,*) "#5789 Return from IDCALCYY: JSUB, ICENTER",
C     1      JSUB, ICENTER

C  STORE YYPRED INTO YPREDPOPT(JSUB,.,.,ICENTER)
 
         DO J=1,NUMT(JSUB)
         DO IEQ=1,NUMEQT
           YPREDPOPT(JSUB,IEQ,J,ICENTER) = YYPRED(J,IEQ)
         END DO
         END DO

        END DO
 
C  THE ABOVE END DO IS FOR THE   DO ICENTER = 1,3  LOOP.

C         write (*,*) "DO 6000 done for ", JSUB 
 
 6000   CONTINUE
 
        write (*,*) "Passed 6000" 
 
C NEXT CALCULATE YPREDBAY AND PREDICTED VALUES AT SPECIFIED TIMES.
 
        REWIND(27)
 
C  FOR EACH SUBJECT IN TURN, FIND THE BAYESIAN POSTERIOR P.D.F., GIVEN
C  THE FINAL JOINT P.D.F. OF THE ENTIRE POPULATION (IN CORDEN) AS THE
C  PRIOR. THEN FIND THE PREDICTED VALUES FOR THE MEANS, MEDIANS, AND
C  MODES OF THIS BAYESIAN P.D.F. (YPREDBAY), AS WELL AS THE PREDICTED
C  VALUES AT THE TIMES SPECIFIED IN TPRED FOR THE MEANS, MEDIANS, OR
C  MODES (SEE ICENT) OF THIS BAYESIAN P.D.F.
C!! NO. AS OF npageng25.f, ICENT IS NO LONGER USED. THE PREDICTED
C  VALUES WILL BE FOUND FOR ALL OF THE MEANS, MEDIANS, AND MODES.

  
C  STORE NACTVE AND CORDEN INTO NNACTVE AND CORHOLD, RESPECTIVELY.
C  THEY MUST BE RESET BEFORE EACH CALL TO SUBRES (WHICH ALTERS THEM).
 
        NNACTVE=NACTVE

 
        DO I=1,NACTVE
        DO J=1,NVAR+1
          CORHOLD(I,J) = CORDEN(I,J)
        END DO
        END DO
 
C  OPEN THE PREDICTED CONCENTRATION FILE, PREDFIL (ESTABLISHED ABOVE
C  AS PRTBxxxx, WHERE xxxx IS THE RUN NO. WRITE EACH SUBJECT NO, EACH
C  FOLLOWED BY NUMT(JSUB) ROWS OF TPRED AND PREDICTED VALUES FOR EACH
C  OF THE MEANS, MEDIANS, AND MODES OF THE BAYESIAN POSTERIOR DENSITY.

        OPEN(31,FILE=PREDFIL)
 
C------------------------------------------------------- 7000 
       write (*,*) "Starting 7000"

C wmy2018.10.16 Moved CALL SYMBOL out of IDCALCY and IDCALCYY (see
C  long comment at DO 6000)
        CALL SYMBOL(NBCOMP)

      IPAR(i_do) = 7000
      DO 7000 JSUB=1,NSUB

        NACTVE=NNACTVE
        DO I=1,NACTVE
          DO J=1,NVAR+1
            CORDEN(I,J) = CORHOLD(I,J)
          END DO
        END DO
 
 8506   FORMAT(////' THE FOLLOWING RESULTS ARE FOR SUBJECT ',I4)
        WRITE(*,8506) JSUB

        CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,
     1     TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     2     INTLIST,ERRFILNAME)

        call cp_lrcs_to_rpar(gamma,flat,numeqt,c0,c1,c2,c3,c4,c5,rpar)

C        write (*,*) "Ret. from FILRED() nr. #5950"

C  CALCULATE SIGFAC AND OFAC FOR THIS SUBJECT. SEE COMMENTS IN LOOP 140
C  ABOVE.

        MISVAL = 0
 
        SIGFAC=1.D0
 
        DO 240 I=1,NOBSER
          DO 240 J=1,NUMEQT
 
            Y = YO(I,J)
 
            IF(Y .EQ. -99) THEN
              MISVAL = MISVAL+1
              GO TO 240
            ENDIF

C Are observations recorded as log10(obs) _AND_ in subroutine
C output, are X converted to log10(X)? If so, do you want the
C sd to be calculated on 10^Y(obs or est)?
C             if (C1(J).eq.-10) IPAR(i_is_log10+J) = -10 
             if (C4(J).eq.10) IPAR(i_is_log10+J) = 10 

C wmy2018.06.26 -- wmy added the Poisson initialization to the end-game
C          if (C0(J).eq.-229.and.C2(J).eq.-229
C     1   .and.C3(J).eq.-229) then
           if (C5(J).eq.229) then
C--------------------------------- Start Poisson

             write (*,*) "Poisson analysis req. for OUTEQ",J
C             NPOISSONOBS=NPOISSONOBS+1
             ObsError(I,J)=1.D0
             SIG(I,J)=1.D0
             IPAR(i_is_poisson_obs+J)=229

C--------------------------------- End Poisson
          else  
C--------------------------------- Start NORMAL
 
            SIG(I,J) = C0(J)+C1(J)*Y+C2(J)*Y*Y+C3(J)*Y**3
cgam4
            if(ierrmod.eq.2) sig(i,j) = sig(i,j)*gamma
            if(ierrmod.eq.3) sig(i,j)=dsqrt(sig(i,j)**2 + gamma**2)
            if(ierrmod.eq.4) sig(i,j) = gamma*flat

            ObsError(I,J) = sig(I,J)

C--------------------------------- End NORMAL

C wmy2018.06.26 -- Following test should only be relevant if (I,J) is a 
C   Normal draw.
            IF(SIG(I,J) .LT. 0) THEN
              WRITE(*,2346) JSUB
              WRITE(25,2346) JSUB

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

              OPEN(42,FILE=ERRFIL)
              WRITE(42,2346) JSUB
              CLOSE(42)

              CALL PAUSE
              STOP
            ENDIF

         endif

            IF(SIG(I,J) .EQ. 0) THEN
              WRITE(*,2345) JSUB
              WRITE(25,2345) JSUB

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

              OPEN(42,FILE=ERRFIL)
              WRITE(42,2345) JSUB
              CLOSE(42)

              CALL PAUSE
              STOP
            ENDIF
 
            ObsError(I,J) = SIG(I,J)
 
            SIGFAC=SIGFAC*SIG(I,J)

  240   CONTINUE
 
        OFAC=2.506628274631**(NOBSER*NUMEQT - MISVAL)

        RPAR(k_sfac) = SIGFAC
        RPAR(k_ofac) = OFAC

C        write (*,*) "DO 7000 (",IPAR(i_do),") Jsub, IG, sigma, ofac",
C     1    JSUB, IG, SIGFAC, OFAC, k_sfac, k_ofac


C  CALL SUBROUTINE SUBRES WHICH DOES THE ACTUAL CALCULATIONS FOR
C  THIS SUBJECT. SUBRES CALCULATES THE BAYESIAN POSTERIOR P.D.F. FOR
C  THIS SUBJECT. IT CALCULATES P(XI|Y), WHERE Y IS THE VECTOR OF
C  OBSERVED VALUES (ACTUALLY, Y IS A 2-DIM ARRAY IF THERE ARE MULTIPLE
C  OUTPUTS) FOR THIS SUBJECT, AND XI, I=1,NACTVE, ARE THE
C  REMAINING GRID POINTS FROM THE FINAL CYCLE OF THE POPULATION
C  ANALYSIS. IT ALSO RETURNS CENTER(I,J),J=1,NVAR, WHERE I = 1 -->
C  MEANS OF THE BAYESIAN POSTERIOR; I = 2 --> MEDIANS OF THE BAYESIAN
C  POSTERIOR; AND I = 3 --> MODES OF THE BAYESIAN POSTERIOR.

C  ALSO NOTE THAT SUBRES RETURNS PYJGXX IN CASE THIS RUN WAS A MAXCYC=0
C  RUN (I.E., A RUN USING A PRIOR DENSITY WITH A DIFFERENT SET OF
C  PATIENTS WHICH MEANS THAT PYJGX WAS NOT CALCULATED BECAUSE NO
C  CYCLES WERE RUN ABOVE - IN THIS CASE, PYJGX MUST BE CALCULATED 
C  IN ORDER TO BE WRITTEN INTO THE DENSITY FILE BELOW). SO PYJGX FOR
C  THIS SUBJECT WILL BE SET = PYJGXX AFTER THE CALL TO SUBRES.

C  ALSO, AS OF npageng23.f, SUBRES PASSES IN COMMON/BAY THE BAYESIAN
C  POSTERIOR DENSITY FOR EACH SUBJECT TO SUBROUTINE READOUT.

C wmy2017Sep12
      CALL SUBRES(MAXSUB,MAXACT,JSUB,CORDEN,WORK,MAXGRD,MAXDIM,NVAR,
     1  NOFIX,VALFIX,SIGFAC,OFAC,AB,PAR,NACTVE,NGRID,VOLSPA,IRAN,CENTER,
     2  PYJGXX,NRANFIX,RANFIXEST,NOBSER,NUMEQT,NBCOMP,
     3  NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     4  INTLIST,IPAR,ObsError,RPAR,NACTSUB,BAYPOS,ERRFILNAME)
C     4  INTLIST,IPAR,ObsError,RPAR,ERRFILNAME)

        DO IG = 1,NACTVE
          PYJGX(JSUB,IG) = PYJGXX(IG)
        END DO
 
C  FOR THIS SUBJECT, FIND THE PREDICTED VALUES (VIA SUBROUTINE IDCALCY),
C  FOR EACH OF THE MEANS, MEDIANS, AND MODES (ICENTER = 1,2,3,
C  RESPECTIVELY).
 
C  ALSO FIND THE PREDICTED VALUES AT THE TIMES SPECIFIED IN TPRED
C  FOR THE MEANS, MEDIANS, AND MODES.

        DO ICENTER = 1,3
 
          DO J=1,NVAR
            EXXX(J) = CENTER(ICENTER,J)
            EXX(JSUB,ICENTER,J) = CENTER(ICENTER,J)
          END DO

C          write (*,*) "EXX filled for ICENTER", ICENTER
 
C  NOTE THAT THE MEANS, MEDIANS, AND MODES FOR EACH SUBJECT ARE
C  STORED INTO EXX, FOR SUBSEQUENT STORAGE INTO THE DENSITY FILE.
 
C  CALL SUBROUTINE IDCALCY, A VERSION OF THE ID PROGRAM WHICH SIMPLY
C  CALCULATES THE PREDICTED VALUES OF YO(I,J) = OUTPUT CONCENTRATION OF
C  THE JTH OUTPUT EQUATION (J=1,NUMEQT) AT THE ITH OBSERVATION TIME
C  (I=1,NOBSER), ASSUMING THE PARAMETER VECTOR EXXX ...
 
C  BEFORE ALL CALLS TO IDCALCY, MUST INTEGRATE FIXED AND RANDOM
C  VALUES INTO PX, USING IRAN(I),I=1,NVAR+NOFIX. CALL MAKEVEC TO DO
C  THIS. AS OF npagranfix.f, ADD NRANFIX AND RANFIXEST
C  TO THE ARGUMENT LIST TOO (I.E., THERE IS NOW A 3RD TYPE OF 
C  PARAMETER (THOSE WHICH ARE UNKNOWN BUT THE SAME FOR ALL SUBJECTS).

      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,EXXX,VALFIX,RANFIXEST,PX)
        NPX = NVAR+NOFIX+NRANFIX
C       if (NPX < max_ODE_params) then exit progam w/error
      IPAR(i_skip_ig) = 1

C wmy2018Jan12 I removed the COMMON/PARAMD/P from SUBROUTINE IDCALCY and
c  IDCALCYY, so P must be initialized in main after calling MAKEVEC, but
c  before calling them. Note: IDCALCY and -YY now call SYMBOL and then 
c  immediately call FUNC2() or FUNC3(). 
C        DO I=1,NVAR+NOFIX+NRANFIX
C        DO I=1,NPX
C          PX(I) = PCOPY(I)
C           P(I) = PCOPY(I)
C        END DO

C      write (*,*) "Param VECs filled; CALLing IDCALCY"

C        rrr = 0.0
C        do III=1,max_doses
C          do JJJ =1,max_RS_J
C            rrr = rrr + rscopy(III,JJJ) - rs(III,JJJ) 
C            rscopy(iii,jjj) = rs(iii,jjj)
C            if (jjj.le.max_input_dim) bscopy(iii,jjj) = bs(iii,jjj)
C          end do
C        end do
C        write (*,*) "calling IDCALCY with rrr=",rrr

        CALL SYMBOL(NBCOMP)

        CALL IDCALCY(JSUB,IG,NPX,NDIM,PX,YPRED,NUMEQT,
     1      NOBSER,MF,NBCOMP,RTOL,ATOL,
     2      TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     3      INTLIST,IPAR,ObsError,RPAR,ERRFILNAME)

C        write (*,*) "#5733 Returned from IDCALCY()"
 
C  NOTE: PREDICTED VALUES WERE FOUND EVEN FOR OBSERVED LEVELS WHICH ARE
C  MISSING (I.E., OBSERVED LEVEL = -99) SINCE IT IS EASIER TO CALCULATE
C  ALL PREDICTED VALUES THAN TO KEEP TRACK OF WHICH DO AND WHICH DON'T
C  NEED TO BE CALCULATED.
 
C  STORE YPRED INTO YPREDBAY(JSUB,.,.,ICENTER)

C        write (*,*) "YPRED", NOBSER, "of", MAXOBS, "updates"
 
        DO IOBS=1,NOBSER
         DO IEQ=1,NUMEQT

C         write (*,*) "YPREDBAY(,,,) update",JSUB
C     1    ,IEQ,"of",NUMEQT
C     2    ,IOBS,"of",MAXOBS
C     3    ,ICENTER

C        YPREDBAY(MAXSUB,NUMEQT,MAXOBS,3)

         YPREDBAY(JSUB,IEQ,IOBS,ICENTER) = YPRED(IOBS,IEQ)
         END DO
        END DO

        END DO
C  THE ABOVE END DO IS FOR THE   DO ICENTER = 1,3  LOOP.

        write (*,*) "YPREDBAY(,,,) update done"
 
C  NOW CALCULATE THE PREDICTED VALUES TO BE STORED INTO FILE 31.
C  NOTE THAT AS OF npageng25.f, THESE VALUES ARE CALCULATED FOR ALL
C  3 MEASURES OF CENTRAL TENDENCY (MEANS, MEDIANS, AND MODES) FROM
C  THE BAYESIAN POSTERIOR DENSITY (IN PREVIOUS PROGRAMS, THE PREDICTED
C  VALUES WERE ONLY CALCULATED FOR THE MEASURE SPECIFIED BY ICENT, BUT
C  ICENT IS NOW IRRELEVANT, EVEN THOUGH IT IS STILL PASSED TO THIS
C  MODULE FROM THE PC PREP PROGRAM).
 
C  AND NOTE AS OF npageng18.f, INSTEAD OF WRITING TPRED(.) TO FILE 31,
C  TPREREL(.) IS WRITTEN (THE LATTER GIVE THE "RELATIVE" TIMES RATHER
C  THAN THE "REAL" TIMES). ESTABLISH THESE VALUES OUTSIDE THE 
C  ICENTER = 1,3 LOOP, SINCE THEY ARE THE SAME REGARDLESS OF THE VALUE
C  OF ICENTER.

        DO I=1,NUMT(JSUB)
         TPRED(I) = TTPRED(JSUB,I)
        END DO

          write (*,*) "TPRED(I) update done"

      DO ICENTER = 1,3



       DO J=1,NVAR
        EXXX(J) = CENTER(ICENTER,J)
       END DO
 
C  CALL SUBROUTINE IDCALCYY, A VERSION OF THE ID PROGRAM WHICH SIMPLY
C  CALCULATES THE PREDICTED VALUES OF Y(I,J) (OUTPUT CONCENTRATION
C  OF THE JTH OUTPUT EQ. AT TIME TTPRED(JSUB,I),I=1,NUMT(JSUB)),ASSUMING 
C  THE PARAMETER VECTOR EXXX. NOTE THAT IDCALCYY DIFFERS FROM IDCALCY IN
C  THAT THE TIMES FOR THE PREDICTED VALUES ARE SUPPLIED IN TPRED,
C  RATHER THAN INPUT VIA COMMON/OBSER FROM THE PATIENT'S DATA FILE. 
C  ALSO, THE NO. OF OBSERVED TIMES IS NUMT(JSUB), RATHER THAN M WHICH IS 
C  SUPPLIED VIA COMMON/SUM2. AND NOTE THAT NUMT(JSUB) AND TTPRED(.,.)
C  WERE FOUND IN CALLS TO CALCTPRED ABOVE.

C  BEFORE ALL CALLS TO IDCALCYY, MUST INTEGRATE FIXED AND RANDOM
C  VALUES INTO PX, USING IRAN(I),I=1,NVAR+NOFIX. CALL MAKEVEC TO DO
C  THIS. AS OF npagranfix.f, ADD NRANFIX AND RANFIXEST
C  TO THE ARGUMENT LIST TOO (I.E., THERE IS NOW A 3RD TYPE OF 
C  PARAMETER (THOSE WHICH ARE UNKNOWN BUT THE SAME FOR ALL SUBJECTS).

      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,EXXX,VALFIX,RANFIXEST,PX)
        NPX = NVAR+NOFIX+NRANFIX
C       if (NPX < max_ODE_params) then exit progam w/error
      IPAR(i_skip_ig) = 1

C      write (*,*) "DO 7000 CALL IDCALCYY", NVAR+NOFIX+NRANFIX,NDIM,
C     1  PX(8),TPRED(5),NUMT(JSUB),NUMEQT,NOBSER,MF,RTOL,ATOL(1)

C wmy2018Jan12 I removed the COMMON/PARAMD/P from SUBROUTINE IDCALCY and
c  IDCALCYY, so P must be initialized in main after calling MAKEVEC, but
c  before calling them. Note: IDCALCY and -YY now call SYMBOL and then 
c  immediately call FUNC2() or FUNC3(). 
C        DO I=1,NVAR+NOFIX+NRANFIX
C        DO I=1,NPX
C          PX(I) = PCOPY(I)
C           P(I) = PCOPY(I)
C        END DO

C      CALL IDCALCYY(JSUB,IG,NVAR+NOFIX+NRANFIX,NDIM,PX,TPRED,
C        rrr = 0.0
C        do III=1,max_doses
C          do JJJ =1,max_RS_J
C            rrr = rrr + rscopy(III,JJJ) - rs(III,JJJ) 
C            rscopy(iii,jjj) = rs(iii,jjj)
C            if (jjj.le.max_input_dim) bscopy(iii,jjj) = bs(iii,jjj)
C          end do
C        end do
C        write (*,*) "calling IDCALCYY with rrr=",rrr

        CALL SYMBOL(NBCOMP)

C      CALL IDCALCYY(JSUB,IG,NPX,NDIM,PCOPY,TPRED,
      CALL IDCALCYY(JSUB,IG,NPX,NDIM,PX,TPRED,
     1  NUMT(JSUB), YYPRED,NUMEQT,NOBSER,MF,NBCOMP,
     2  RTOL,ATOL,
     3  TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     4  INTLIST,IPAR,ObsError,RPAR,ERRFILNAME)

C       write (*,*) "#5800 Returned from IDCALCYY"

C  STORE YYPRED INTO YYYPRED(ICENTER,J,IEQ).

       DO J = 1,NUMT(JSUB)
        DO IEQ = 1,NUMEQT
         YYYPRED(ICENTER,J,IEQ) = YYPRED(J,IEQ)
        END DO
       END DO

      END DO
C  THE ABOVE END DO IS FOR THE  DO ICENTER = 1,3  LOOP.


 
C  STORE YYYPRED(.,.,.) INTO FILE 31.
 
      WRITE(31,2131) JSUB
 2131 FORMAT('  SUBJECT NO. ',I5//
     1' COL. 1. = PREDICTION TIMES'/
     2' COL. 2. = PRED. VALUES FOR OUTPUT EQ. 1, BASED ON POSTERIOR MEAN
     3S'/
     4' COL. 3. = PRED. VALUES FOR OUTPUT EQ. 1, BASED ON POSTERIOR MEDI

     5ANS'/
     6' COL. 4. = PRED. VALUES FOR OUTPUT EQ. 1, BASED ON POSTERIOR MODE
     7S'/
     8' EACH ADDITIONAL OUTPUT EQ. HAS 3 COLUMNS OF PREDICTED VALUES'/
     9' BASED, IN ORDER, ON THE POSTERIOR MEANS, MEDIANS, AND MODES: '/
     1'-------------------------------------------------------------')




      DO J = 1,NUMT(JSUB)

       XVERIFY(1) = TTPREDREL(JSUB,J)
       IXV = 1

       DO IEQ = 1,NUMEQT
        IXV = IXV + 1
        XVERIFY(IXV) = YYYPRED(1,J,IEQ)
        IXV = IXV + 1     
        XVERIFY(IXV) = YYYPRED(2,J,IEQ)
        IXV = IXV + 1     
        XVERIFY(IXV) = YYYPRED(3,J,IEQ)
       END DO

       CALL VERIFYVAL(1+3*NUMEQT,XVERIFY)



C      WRITE(31,2167) TTPREDREL(JSUB,J),
C     1 (YYYPRED(1,J,IEQ),YYYPRED(2,J,IEQ),YYYPRED(3,J,IEQ),
C     2 IEQ=1,NUMEQT)

       WRITE(31,2167) (XVERIFY(IXV),IXV=1,1+3*NUMEQT)


 2167  FORMAT(90(G16.5,2X))



C  NOTE THAT THE MOST NOS. IN THE ABOVE FORMAT IS 1 + 3*NUMEQT. THIS 
C  WILL SURELY BE < 90, SO 90 IS SUFFICIENT.
      END DO

c  AS OF npageng21.f, TTPREDREL(JSUB,J) is written to file 31 above, 
C  rather than TPREDREL(J). Writing TPREDREL(J) was a mistake since it
C  holds the values for the last subject, not the values for each 
C  subject.


       DO IEQ = 1,NUMEQT

        WRITE(31,2132) JSUB,IEQ
 2132    FORMAT(//'  SUBJECT NO. ',I5,' ...  OUTPUT EQUATION NO. ',I2//
     1'     TIMES                   OBSERVED VALUES          PREDICTED V
     2ALUES, BASED ON POSTERIOR MEANS; THEN MEDIANS; THEN MODES'/
     2'-----------------------------------------------------------------
     3---------------------------------------------------------')
        DO IOBS=1,NOBSER
         WRITE(31,2167) TIMOBREL(JSUB,IOBS),YO(IOBS,IEQ),
     1   (YPREDBAY(JSUB,IEQ,IOBS,ICENTER),ICENTER=1,3)
C  SEE ABOVE COMMENT BELOW FORMAT 2167.
        END DO

       END DO

c  Note that as of npageng18.f, instead of writing TIMOBS(.) to File 31,
c  the program writes TIMOBREL(.,.) to the file. TIMOB(IOBS) are the
c  "real" times; and TIMOBREL(JSUB,IOBS) are the "relative" times. See
c  comments regarding change 1 at top of npageng18.f code.


        WRITE(31,*)
        WRITE(31,*) 

 
C  CALCULATE AUC'S AND AUC/MIC'S AND PUT THEM TO SCREEN AND TO THE
C  OUTPUT FILE ... ONE TABLE FOR EACH OUTPUT EQUATION. ACTUALLY, THERE
C  WILL NOW BE ONE TABLE FOR EACH OUTPUT EQUATION FOR EACH TIME RESET
C  FOR THE SUBJECT. 

C  AS OF npageng25.f, THERE WILL BE 1 SET OF AUC TABLES FOR EACH OF
C  THE MEAN, MEDIAN, AND MODE.



C  THE NO. OF MAXIMUM TIMES FOR THIS SUBJECT IS NOMAXTIM(JSUB), WHICH
C  IS 1 MORE THAN THE NO. OF TIME RESETS; THE ENDING TIMES (24 HOURS 
C  PAST THE LAST OBSERVATION TIME) ARE IN 
C  TENDSUB(JSUB,J), J=1,NOMAXTIM(JSUB); AND THE BEGINNING TIMES ARE IN
C  TBEGGSUB(JSUB,J), J=1,NOMAXTIM(JSUB). MAKE A TABLE FOR EACH OF THESE
C  NOMAXTIM(JSUB) PAIRS OF TIMES.

      ILAST = 0

C  ILAST IS THE RUNNING NO. OF YYPRED VALUES ALREADY USED IN CALCULATING
C  AUCs FROM THE PREVIOUS LOOP ON IMAXTIM (SEE LOGIC BELOW).

C  FOR npageng21.f, A BUG IS FIXED BY INITIALIZING IELAST = 0 HERE:
C  FOR EACH NEW SUBJECT, INITIALIZE IELAST = 0. REASON: IT IS SUPPOSED
C  TO BE SET IN LOOP 2050, BUT IF NWHOLE = 0 FOR THE 1ST TIME PERIOD 
C  (IMAXTIM = 1) FOR A NEW SUBJECT, LOOP 2050 IS SKIPPED AND THEN
C  IS = IELAST OCCURS IN THE   IF(NPAR .GT. 0)   SECTION, AND IF THIS
C  OCCURS FOR JSUB > 1, THEN IS WILL BE SET TO WHATEVER IELAST WAS FROM 
C  THE PREVIOUS SUBJECT (WHICH MEANS SUM WILL NOT START WITH YYPRED(1,.)
C  FOR THE NEW SUBJECT AS IT SHOULD). TO PREVENT THIS, INITIALIZE 
C  IELAST = 0 HERE, FOR EACH NEW SUBJECT.

      IELAST = 0


      DO IMAXTIM = 1,NOMAXTIM(JSUB)

       NUMTT = int((TENDSUB(JSUB,IMAXTIM)-TBEGGSUB(JSUB,IMAXTIM)
     1   )*60/IDELTA)

C  NDELPER = NO. OF INTERVALS IN EACH AUCINT HOURS (1 PERIOD =
C            AUCINT*60 MINUTES).
C  NWHOLE = NO. OF WHOLE PERIODS.
C  NPAR = NO. OF INTERVALS ON THE LAST PARTIAL PERIOD, IF ANY.
C  NOTE: IF NUMTT/NDELPER IS AN INTEGER, THEN NPAR WILL = 0 --> THERE
C	   WILL BE NO LAST PARTIAL PERIOD.

C  RECALL THAT NUMTT IS THE TOTAL NO. OF INTERVALS FOR THE PREDICTED
C  CONCS. (NUMTT+1 IS THE NO. OF PREDICTED CONCS., SINCE THE 1ST
C  PREDICTED CONC. IS AT TIME = TBEGGSUB(JSUB,IMAXTIM)).

       NDELPER = int(AUCINT*60/IDELTA)
       NWHOLE = int(NUMTT/NDELPER)
       NPAR = NUMTT - NWHOLE*NDELPER


	 DO IEQ = 1,NUMEQT


        DO ICENTER = 1,3

 
	  WRITE(*,2053) IEQ
	  WRITE(25,2053) IEQ
        IF(ICENTER .EQ. 1) WRITE(*,3051)
        IF(ICENTER .EQ. 2) WRITE(*,3052)
        IF(ICENTER .EQ. 3) WRITE(*,3053)
        IF(ICENTER .EQ. 1) WRITE(25,3051)
        IF(ICENTER .EQ. 2) WRITE(25,3052)
        IF(ICENTER .EQ. 3) WRITE(25,3053)
 

 2053  FORMAT(//' THE FOLLOWING TABLE IS FOR OUTPUT EQUATION ',I2)
 3051  FORMAT(' BASED ON THE POSTERIOR MEANS: ')
 3052  FORMAT(' BASED ON THE POSTERIOR MEDIANS: ')
 3053  FORMAT(' BASED ON THE POSTERIOR MODES: ')


	  IF(NOMAXTIM(JSUB) .GT. 1) THEN
	   WRITE(*,2054) IMAXTIM
	   WRITE(25,2054) IMAXTIM

 2054      FORMAT(/' FOR MAXIMUM TIME NO. ',I3)
	  ENDIF

C  BEFORE CALCULATING THE AUC'S AND AUC/MIC'S EVERY AUCINT HOURS,
C  ESTABLISH THE HEADER INFO. FOR THE TABLE SHOWING THE AUC'S AND
C  THE AUC/MIC'S FOR THIS SUBJECT.
 
	WRITE(*,2048)

	WRITE(25,2048)
 2048   FORMAT(/'      PERIOD',10X,'TIME (HOURS)',8X,'  AUC',8X,'   AUC/
     1MIC'/
     1' ----------------------------------------------------------------
     1----')
 
C  OUTPUT ONE LINE IN THE AUC TABLE FOR EACH AUCINT HOURS, AND THEN ONE 
C  LINE FOR THE TOTALS.
 
C  AUCRUN = RUNNING TOTAL OF AUC'S THRU EACH AUCINT HOURS.

	AUCRUN = 0.D0

	DO 2050 IPERIOD = 1,NWHOLE


C  FOR WHOLE PERIOD, IPERIOD, CALCULATE THE STARTING AND ENDING INDICES
C  OF PREDICTED CONCENTRATIONS (IS AND IE, RESPECTIVELY).

C  NOTE THAT A BUG IS CORRECTED IN npageng16.f. PREVIOUSLY, THE VALUE
C  FOR IS WAS WRONG AFTER THE IMAXTIM = 1, SINCE THE YYPRED VALUES 
C  USED KEPT STARTING OVER WITH THE 1ST VALUE IN THE ARRAY, RATHER THAN
C  CONTINUE WITH THE NEXT SET OF YYPRED VALUES (WHICH ARE STACKED ONE
C  AFTER THE OTHER FOR ALL TTPRED VALUES FROM SUBROUTINE CALCTPRED).
C  I.E., IS ALWAYS STARTED WITH 1, REGARDLESS OF THE VALUE OF IMAXTIM.

C  NOW IS WILL BE ADJUSTED AHEAD ILAST VALUES, WHERE ILAST IS THE NO.
C  OF YYPRED VALUES ALREADY USED FOR THE PREVIOUS IMAXTIM VALUES.

	IS = ILAST + (IPERIOD-1)*NDELPER + 1
	IE = IS + NDELPER
      IELAST = IE


C  THE AREA UNDER THE CURVE WILL BE CALCULATED USING THE
C  TRAPEZOIDAL RULE. SINCE EACH OF THE CONCS. ARE IDELTA APART,
C  AUC = IDELTA*(YYPRED(IS,IEQ)/2 + SUM + YYPRED(IE,IEQ)/2), WHERE SUM
C  IS THE SUM OF YYPRED(.,IEQ), FOR INDICES BETWEEN IS+1 AND IE-1.

 
	SUM=0.D0
	DO I=IS+1, IE-1
	 SUM=SUM+YYYPRED(ICENTER,I,IEQ)
	END DO
 
	AUC = IDELTA*((YYYPRED(ICENTER,IS,IEQ) +
     1      YYYPRED(ICENTER,IE,IEQ))/2.D0 + SUM)/60.D0

 
C  NOTE THAT AUC HAS UNITS IN TERMS OF HOURS BECAUSE OF THE DIVISION BY
C  60 ABOVE (WHICH CHANGES IDELTA FROM MINUTES TO HOURS).
 
	AUCMIC = AUC/XMIC

	AUCRUN = AUCRUN+AUC
 
C  THIS PERIOD STARTS WITH HOUR, IHRST, AND ENDS WITH HOUR, IHREN.

C  NOTE THAT AS OF npageng18.f, IHRST GIVES THE "RELATIVE" INSTEAD OF
C  THE "REAL" TIME FOR THIS PERIOD (SINCE TBEGGSUB(.,.) IS NO LONGER
C  ADDED TO THE EQUATION BELOW.


C	IHRST = (IPERIOD-1)*AUCINT + TBEGGSUB(JSUB,IMAXTIM)
	IHRST = (IPERIOD-1)*int(AUCINT)
	IHREN = IHRST + int(AUCINT)

C  PRINT TO OUTPUT FILE AND THE SCREEN THE INFO FOR THIS PERIOD.
 

      XVERIFY(1) = AUC
      XVERIFY(2) = AUCMIC
      CALL VERIFYVAL(2,XVERIFY)

C     WRITE(*,2049) IPERIOD, IHRST, IHREN, AUC, AUCMIC
      WRITE(*,2049) IPERIOD, IHRST, IHREN, XVERIFY(1),XVERIFY(2)
C     WRITE(25,2049) IPERIOD, IHRST, IHREN, AUC, AUCMIC
      WRITE(25,2049) IPERIOD, IHRST, IHREN, XVERIFY(1),XVERIFY(2)


 2049 FORMAT(' ',I8,2X,I10,'    -',I10,5X,G12.6,3X,G12.6)
 
 2050 CONTINUE
 
C  NOW, PUT IN A LINE FOR THE LAST PARTIAL PERIOD, IF ANY.
 
	IF(NPAR .GT. 0) THEN
 
C  THE STARTING INDEX FOR THE PREDICTED CONCS. IS SIMPLY THE ENDING
C  INDEX FROM LOOP 2050 ABOVE. THE ENDING INDEX IS NPAR MORE THAN THE
C  STARTING INDEX. THEN PROCEED AS IN LOOP 2050 ABOVE.

 
	IS = IELAST
	IE = IS + NPAR
 
	SUM=0.D0
	DO I=IS+1, IE-1
	 SUM=SUM+YYYPRED(ICENTER,I,IEQ)
	END DO

 
	AUC = IDELTA*((YYYPRED(ICENTER,IS,IEQ) +
     1      YYYPRED(ICENTER,IE,IEQ))/2.D0 + SUM)/60.D0
	AUCMIC = AUC/XMIC
	AUCRUN = AUCRUN+AUC
 
C  ESTABLISH THE PERIOD NO., AND THE STARTING AND ENDING HOUR NOS.
 
	IPERIOD = NWHOLE + 1

C  NOTE THAT AS OF npageng18.f, IHRST GIVES THE "RELATIVE" INSTEAD OF
C  THE "REAL" TIME FOR THIS PERIOD (SINCE TBEGGSUB(.,.) IS NO LONGER
C  ADDED TO THE EQUATION BELOW.

C	IHRST = (IPERIOD-1)*AUCINT + TBEGGSUB(JSUB,IMAXTIM)

	IHRST = (IPERIOD-1)*int(AUCINT)
	IHREN = IHRST + int(NPAR*IDELTA/60)

 
C  PRINT TO OUTPUT FILE AND THE SCREEN THE INFO FOR THIS PERIOD.
 

      XVERIFY(1) = AUC
      XVERIFY(2) = AUCMIC
      CALL VERIFYVAL(2,XVERIFY)



C     WRITE(*,2051) IPERIOD, IHRST, IHREN, AUC, AUCMIC
      WRITE(*,2051) IPERIOD, IHRST, IHREN, XVERIFY(1),XVERIFY(2)


C     WRITE(25,2051) IPERIOD, IHRST, IHREN, AUC, AUCMIC
      WRITE(25,2051) IPERIOD, IHRST, IHREN, XVERIFY(1),XVERIFY(2)



 2051   FORMAT(' ',I2,' (PARTIAL)',I8,'    -',I10,5X,G12.6,3X,G12.6)
 

	ENDIF
 
C  THE ABOVE ENDIF IS FOR THE  IF(NPAR .GT. 0)  CONDITION.
 
C  NOW WRITE THE LAST LINE, GIVING TOTAL VALUES FOR AUC AND AUC/MIC.
C  NOTE THAT TOTAL AUC IS NOW AUCRUN. NOTE THAT IHREN IS THE SAME AS
C  THE LAST VALUE CALCULATED ABOVE (I.E., IT'S THE ENDING HOUR).
 
	AUCMIC = AUCRUN/XMIC

C  AS OF npageng18.f, THE BEGINNING TIME FOR EACH AUC TABLE, SINCE THE
C  TIMES ARE "RELATIVE", INSTEAD OF "REAL", WILL ALWAYS BE 0.

      XVERIFY(1) = AUCRUN
      XVERIFY(2) = AUCMIC
      CALL VERIFYVAL(2,XVERIFY)



C     WRITE(*,2052) IHREN, AUCRUN, AUCMIC
      WRITE(*,2052) IHREN, XVERIFY(1),XVERIFY(2)
C     WRITE(25,2052) IHREN, AUCRUN, AUCMIC
      WRITE(25,2052) IHREN, XVERIFY(1),XVERIFY(2)


 2052   FORMAT(' ','----------------------------------------------------
     1---------------'/
     2'    TOTAL',2X,'         0    -',I10,5X,G12.6,3X,G12.6//)
 

       END DO
C  THE ABOVE END DO IS FOR THE  DO ICENTER = 1,3  LOOP.


 
	END DO 
C  THE ABOVE END DO IS FOR THE  DO IEQ = 1,NUMEQT  LOOP.

C  INCREASE ILAST TO BE THE NO. OF YYPRED VALUES ALREADY USED.
C  SIMILARLY FOR IELAST.

       ILAST = IE
       IELAST = IE

	END DO
C  THE ABOVE END DO IS FOR THE  DO IMAXTIM = 1,NOMAXTIM(JSUB)  LOOP.
 
 

 7000   CONTINUE

C       write (*,*) "Passed 7000; writing DENSITY" 
	
C  RESTORE THE CORRECT CORDEN AND NACTVE (THEY WERE CHANGED IN
C  THE CALLS TO SUBRES). THEY MUST BE RESTORED BEFORE BEING WRITTEN
C  TO THE DENSITY FILE.
 
	NACTVE=NNACTVE
	DO I=1,NACTVE
	DO J=1,NVAR+1
	 CORDEN(I,J) = CORHOLD(I,J)
	END DO
	END DO
 
 
C  STORE THIS CYCLE'S JOINT DENSITY (AND ASSOCIATED VALUES) INTO THE
C  FILE DENFIL.
 
      write(*,*)' About to create density file ...'

	OPEN(23,FILE=DENFIL)
 
	WRITE(23,7124)
 7124   FORMAT('DENSITY OCT_15 ... Made by npagranfix6')

	WRITE(23,*) NDIM
	WRITE(23,*) INDPTS
 
C  NEW CODE ABOVE FOR m2_11cal.f
 
	WRITE(23,*) NACTVE
 
	WRITE(23,*) NVAR
	WRITE(23,2227) (PAR(I),I=1,NVAR)
	WRITE(23,*) NOFIX
	WRITE(23,2227) (PARFIX(I),I=1,NOFIX)
      WRITE(23,*) NRANFIX
	WRITE(23,2227) (PARRANFIX(I),I=1,NRANFIX)

 
	DO I=1,NVAR
	 WRITE(23,*) (AB(I,J),J=1,2)
	END DO
 
	WRITE(23,*) (VALFIX(I),I=1,NOFIX)
	WRITE(23,*) (RANFIXEST(I),I=1,NRANFIX)

	
C  STARTING WITH MXEM2N36.FOR, NINT WILL ALWAYS BE 100. BUT, IN ORDER
C  NOT TO CHANGE THE STRUTURE OF PRIOR DENSITY FILES (SO THAT PRIOR
C  DENSITIES CAN STILL BE RUN WITH MXEM2N36.FOR), NINT WILL STILL
C  BE WRITTEN TO, AND READ FROM, THIS FILE.

 
	WRITE(23,*) NINT

c  As of npageng18.f, reduce ICYCLE BY 1 if IHESS = -1, because this 
c  means a Hessian Matrix was singular in a call to Subroutine emint,
c  which --> the current ICYCLE never had its values written to the
c  output file since control was transferred immediately to label 900
c  after returning from emint.

      IF(IHESS .EQ. -1) ICYCLE = ICYCLE - 1


	WRITE(23,*) ICYCLE
	WRITE(23,*) DORIG
 

	write(*,*)' Writing CORDEN ... '

	 DO I=1,NACTVE
	  WRITE(23,*) (CORDEN(I,J),J=1,NVAR+1)
	 END DO
 
C	write(23,*)' Writing PYJGX ... '
	write(*,*)' Writing PYJGX ... '

	DO JSUB=1,NSUB

	 DO I=1,NACTVE
	  WRITE(23,*) PYJGX(JSUB,I)
	 END DO
	END DO
 

 
	REWIND(27)

C        write(23,*)' Writing YPREDPOP ...'
        write(*,*)' Writing YPREDPOP ...'

        DO JSUB=1,NSUB
          CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,
     1     TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     2     INTLIST,ERRFILNAME)

        call cp_lrcs_to_rpar(gamma,flat,numeqt,c0,c1,c2,c3,c4,c5,rpar)

C        write (*,*) "Ret. from FILRED() nr. #5685"

          DO IEQ=1,NUMEQT
            DO J=1,NOBSER
              WRITE(23,*) (YPREDPOP(JSUB,IEQ,J,ICENTER),ICENTER=1,3)
            END DO
          END DO
        END DO

	DO JSUB = 1,NSUB
	 WRITE(23,*) NUMT(JSUB)
	END DO


C	write(23,*)' Writing YPREDOPT ... '
	write(*,*)' Writing YPREDOPT ... '
C
C Differences in DEN traced to this block on 10/12/2018 wmy
C
C

	DO JSUB=1,NSUB
	 DO IEQ=1,NUMEQT

	  DO J=1,NUMT(JSUB)
	   WRITE(23,*) (YPREDPOPT(JSUB,IEQ,J,ICENTER),ICENTER=1,3)
	  END DO
	 END DO
	END DO

C	write(23,*)' Writing TTPREDREL ... '
	write(*,*)' Writing TTPREDREL ... '


	DO JSUB=1,NSUB
	 DO J=1,NUMT(JSUB)

C  AS OF npageng18.f, WRITE THE "RELATIVE" INSTEAD OF THE "REAL"
C  TIMES TO FILE 23.

C	  WRITE(23,*) TTPRED(JSUB,J)
	  WRITE(23,*) TTPREDREL(JSUB,J)

	 END DO
	END DO
 
	REWIND(27)

C	write(23,*)' Writing YPREDBAY ... '
	write(*,*)' Writing YPREDBAY ... '

 
	DO JSUB=1,NSUB

	 CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,
     1     TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     2     INTLIST,ERRFILNAME)

        call cp_lrcs_to_rpar(gamma,flat,numeqt,c0,c1,c2,c3,c4,c5,rpar)

C        write (*,*) "Ret. from FILRED() nr. #6700"

	 DO IEQ=1,NUMEQT
	  DO J=1,NOBSER
	   WRITE(23,*) (YPREDBAY(JSUB,IEQ,J,ICENTER),ICENTER=1,3)
	  END DO
	 END DO
	END DO

	write(*,*)' Writing EXX ... '
C	write(23,*)' Writing EXX ... '

 
	DO JSUB=1,NSUB
	 DO ICENTER=1,3
	  WRITE(23,*) (EXX(JSUB,ICENTER,J),J=1,NVAR)
	 END DO
	END DO
 
      write(25,*) 'Optimal value of gamma=',gamma


C  CONSTRUCT THE COMBINED OUTPUT FILE, TO BE CALLED 'OUT'//NAME, WHERE
C  NAME WAS OBTAINED ABOVE, AND IS THE CHARACTER*4 EQUIVALENT TO INUM

C  AS OF npbig7aadapt.f, CREATE A COMBINED OUTPUT FILE.
C  NOTE THAT FILE 25 IS THE REGULAR OUTPUT FILE AND FILE 23 IS THE 
C  DENSITY FILE. ALSO SCRATCH FILE 27 HAS THE PATIENT DATA PART OF
C  npembig3.inp. SO ONLY NEED TO OPEN npagdriv.f, AND THEN OPEN
C  THE COMBINED OUTPUT FILE, OUTCOM, AND PUT ALL THE REQUIRED INFO
C  INTO OUTCOM, IN THE FORMAT REQUIRED BY SUBROUTINE PREVRUN OF THE
C  PC PREP/ANALYSIS PROGRAM (CURRENTLY, NPBIG10A.FOR).

	OPEN(29,FILE='npagdriv.f')
	REWIND(27)
	REWIND(25)
	REWIND(23)

	write(*,*)' About to create the combined output file ... '


	OUTCOM = 'OUT'//NAME
	OPEN(26,FILE=OUTCOM)

C  PUT THE REGULAR OUTPUT FILE INTO THE COMBINED OUTPUT FILE. 

 1110   READ(25,2717,IOSTAT=IEND) READLARG
 2717   FORMAT(A1000)
        IF(IEND .LT. 0) GO TO 1120

        CALL CONDENSE(READLARG)

C  USE SUBROUTINE CONDENSE TO WRITE THIS LINE WITH AS SMALL A FORMAT
C  AS POSSIBLE (WITHIN 25 CHARACTERS) TO FILE 26. SIMILARLY FOR THE
C  OTHER CALLS TO CONDENSE BELOW.

        GO TO 1110

 1120   WRITE(26,1121)
 1121   FORMAT(/'***************** END OF THE OUTPUT FILE **************
     1***'//
     2'***************** START OF THE DENSITY FILE *****************'/)


	write(*,*)' Writing density to combined output file ...'


C  PUT THE DENSITY FILE INTO THE COMBINED OUTPUT FILE. 

 1130   READ(23,2717,IOSTAT=IEND) READLARG
        IF(IEND .LT. 0) GO TO 1140
        CALL CONDENSE(READLARG)
        GO TO 1130



 1140   WRITE(26,1141)
 1141   FORMAT(/'***************** END OF THE DENSITY FILE *************
     1****'//
     2'***************** START OF THE PATIENT DATA INFO FILE ***********
     3******'/)


	write(*,*)' Writing patient data to combined output file ...'

C  PUT THE PATIENT DATA INFO INTO THE COMBINED OUTPUT FILE. 

 1150   READ(27,2717,IOSTAT=IEND) READLARG
        IF(IEND .LT. 0) GO TO 1160
        CALL CONDENSE(READLARG)

        GO TO 1150

 1160   WRITE(26,1161)
 1161   FORMAT(/'***************** END OF THE PATIENT DATA INFO FILE ***
     1**************'//
     2'***************** START OF THE npagdriv.f FILE *****************'
     3)



	write(*,*)' Writing model file to combined output file ...'

C  PUT npagdriv.f INTO THE COMBINED OUTPUT FILE. 

 1170   READ(29,2717,IOSTAT=IEND) READLARG
        IF(IEND .LT. 0) GO TO 1180
        CALL CONDENSE(READLARG)

        GO TO 1170

 1180   WRITE(26,1181)
 1181   FORMAT(/'***************** END OF THE npagdriv.f FILE **********
     1*******'/)
     


C  DON'T CLOSE FILES 25,23,27,29. THEY WILL BE REUSED
C  BY read__.f. Just rewind them.

      REWIND(23)
      REWIND(27)
      REWIND(25)
      REWIND(29)
      CLOSE(26)
      CLOSE(31)



C  FROM THE COMBINED OUTPUT FILE, OUTCOM, CALL READOUT TO FORM THE
C  SUMMARIZED OUTPUT FILE, NP_RFxxxx.TXT, WHICH IS COMPATIBLE WITH THE
C  R GRAPHICS PACKAGE.



        OUTFILER = 'NP_RF'//NAME//'.TXT'
        CALL READOUT( OUTFILER
     1 , DOSEBLOCK, OBSBLOCK, NDORIG
     2 , BAYPOS, NACTSUB )
 
c  For some reason, the unix system keeps file 27 in the directory.
c  To eliminate it, use the following command.

c  As of bigmlt1.f, the call system line below is commented out since
c  this program is usually run now on PCs. Also tbeg and tend are not
c  used now, so their write statements are commented out.
 
C	call system("rm -f fort.27")

C      tend = 0
C      write(91,*) 'total run time in seconds=',tend-tbeg
C       write(6,*) 'total run time in seconds=',tend-tbeg
 	  close(91)


C  NOW CLOSE THE FILES USED BY READOUT.

      CLOSE(23)
      CLOSE(27)
      CLOSE(25)
      CLOSE(29)
 
	STOP
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       SUBROUTINE FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,
C     1    TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,INTLIST,ERRFIL)
        SUBROUTINE FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,
     1    TIM,SIG,RS,BS,INTLIST,ERRFIL)

C  FILRED IS CALLED BY MAIN TO READ THE PORTION OF
C  SCRATCH FILE 27 WHICH APPLIES TO THE SUBJECT UNDER CONSIDERATION. THE
C  'POINTER' FOR FILE 27 IS IN THE PROPER POSITION TO BEGIN READING THE
C  INFO FOR THE DESIRED SUBJECT.

         use npag_utils, only: maxnumeq,max_m_per_obs
     1    ,max_obs_dim,max_input_dim,max_doses,max_RS_J
 
        IMPLICIT REAL*8(A-H,O-Z)
C        PARAMETER(MAXNUMEQ=7)

c ARGS passed in from main
        integer NOBSER
        DIMENSION YO(max_m_per_obs,NUMEQT)
        DIMENSION C4(NUMEQT),C5(NUMEQT)
     1    ,C0(NUMEQT),C1(NUMEQT),C2(NUMEQT),C3(NUMEQT)
        integer NUMEQT
C wmy2017Sep26 Copies of /INPUT/
       real*8, dimension(max_m_per_obs) :: TIMCOPY
       real*8, dimension(max_doses) :: SIGCOPY
       real*8, dimension(max_doses,max_RS_J) :: RSCOPY
       real*8, dimension(max_doses,max_input_dim) :: BSCOPY
c wmy2017Sep30
       integer, dimension(128) :: INTLIST

! wmy20190628 -- 
C        COMMON /CNST/ N,ND,NI,NUP,NUIC,NP
C        COMMON /CNST2/ NPL,NUMEQTT,NDRUG,NADD
C        COMMON/DESCR/AGE,HEIGHT,ISEX,IETHFLAG
C        COMMON /SUM2/ M,NPNL
        integer N,ND,NI,NUP,NUIC,NP
        integer NUMEQTT,NDRUG,NADD,ISEX,IETHFLAG,M
        double precision AGE,HEIGHT

c COMMON /OBSER/ wmy20190904 now passed in as args
        DIMENSION TIM(max_m_per_obs),SIG(max_doses),
     1    RS(max_doses,max_RS_J),
     2    YOO(max_m_per_obs,MAXNUMEQ),BS(max_doses,max_input_dim)
C     2    YO(max_m_per_obs,NUMEQT),BS(max_doses,max_input_dim)

C  AS OF npageng13.f, THE FORMAT FOR THE WORKING COPY FILES IS:

C     COL 1 = TIME
C     COL 2 = IV FOR DRUG 1; COL 3 = PO FOR DRUG 1;
C     COL 4 = IV FOR DRUG 2; COL 5 = PO FOR DRUG 2;
C     ... EACH SUCCEEDING DRUG HAS AN IV FOLLOWED BY A PO COLUMN.
C     NEXT NADD COLUMNS = ONE FOR EACH ADDITIONAL COVARIATE (ADDITIONAL
C      REFERS TO ANY EXTRA COVARIATES BEYOUND THE 4 PERMANENT ONES IN
C      COMMON DESCR (SEE BELOW).
 
C        COMMON /OBSER/ TIM,SIG,RS,YOO,BS
C        COMMON /CNST/ N,ND,NI,NUP,NUIC,NP
C        COMMON/ERR/ERRFIL 

        character ERRFIL*20

C !$omp Threadprivate(/OBSER/)
C !$omp Threadprivate(/CNST/,/SUM2/,/CNST2/,/DESCR/)

c LOCAL variables
        CHARACTER SEX*1,READLINE*300
C wmy2017Sep26 -- This is a temporary copy, req. to fill BCOPY
        real*8, dimension(max_input_dim) :: FA

  
C  INPUT IS: SCRATCH FILE 27, WHICH IS POSITIONED AT THE BEGINNING OF
C  THE INFO FOR THE SUBJECT DESIRED.
 
C  OUTPUT ARE:

 
C  NOBSER = THE NO. OF OBSERVATIONS FOR THIS SUBJECT.
C  YO(I,J),I=1,M; J=1,NUMEQT = NO. OF OUTPUT EQS; I=1,M, WHERE M = NO.
C	OF OBSERVATION TIMES.
C  [C0(J),C1(J),C2(J),C3(J),C4(j),C5(J)] = ASSAY NOISE COEFFICIENTS FOR
C       OUTPUT EQ. J; J=1,NUMEQT.
C  THE 4 DESCRIPTOR VALUES FOR THIS SUBJECT (AGE, SEX, HEIGHT,
C    ETHNICITY FLAG) VIA COMMON/DESCR TO SUBROUTINES DIFFEQ/OUTPUT.
C  VARIABLES/ARRAYS IN ABOVE COMMON STATEMENTS.
 
 
C  AGE, SEX, HEIGHT, AND ETHNICITY FLAG ARE ON LINES 8-11. So skip first 7
 
	DO I=1,7
	 READ(27,*)
	END DO

	
	READ(27,*) AGE
	READ(27,2) SEX
    2   FORMAT(A1)
	ISEX=1
	IF(SEX .EQ. 'F') ISEX=2
	READ(27,*) HEIGHT
	READ(27,*) IETHFLAG

C wmy2017Sep30 Note loss of precision for AGE and HEIGHT
        INTLIST(1) = int(AGE)
        INTLIST(2) = ISEX
        INTLIST(3) = int(HEIGHT)
        INTLIST(4) = IETHFLAG
 
C  READ THE NO. OF DRUGS FROM THE LINE WITH 'NO. OF DRUGS' AS ENTRIES
C  12:23. THEN READ NO. OF ADDITIONAL COVARIATES, AND THE NO. OF DOSE 
C  EVENTS, ETC.

    1   FORMAT(A300)
   10	READ(27,1) READLINE
	IF(READLINE(12:23) .NE. 'NO. OF DRUGS') GO TO 10
	BACKSPACE(27)

    3   FORMAT(T2,I5)
        READ(27,3) NDRUG

C wmy2017Sep30
        INTLIST(5) = NDRUG
c        write (*,*) "INTLIST(5) = NDRUG", INTLIST(5), NDRUG

	IF(NDRUG .GT. max_input_dim) THEN
	 WRITE(*,124)
  124    FORMAT(' YOUR PATIENT DATA FILES CANNOT HAVE MORE THAN 7'/
     1' DRUGS. THE PROGRAM IS NOW STOPPING. '/)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,124) 
        CLOSE(42)

       CALL PAUSE
	 STOP
	ENDIF

        READ(27,3) NADD

C  NOTE THAT THE NO. OF "RATES" INCLUDES 2 FOR EACH DRUG (THE IV AND
C  THE PO COLUMNS) + NADD (1 COLUMN FOR EACH ADDITIONAL COVARIATE).

	NI = 2*NDRUG + NADD

C wmy2017Sep30
        INTLIST(6) = NADD
        INTLIST(7) = NI
c        write (*,*) "INTLIST(6,7) = NADD, NI", INTLIST(6), INTLIST(7),
c     1     NADD, NI 

	IF(NI .GT. max_RS_J) THEN
  	 WRITE(*,123)
  123    FORMAT(/' YOUR PATIENT DATA FILES HAVE TOO MANY COLUMNS IN '/
     1' THE DOSAGE REGIMEN BLOCK. THE NO. OF ADDITIONAL COVARIATES '/
     2' PLUS TWICE THE NO. OF DRUGS CANNOT EXCEED max_RS_J. THE'/
     3' PROGRAM IS NOW STOPPING. '/)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,123) 
        CLOSE(42)

       CALL PAUSE
	 STOP
	ENDIF

        READ(27,3) ND

C wmy2017Sep30
        INTLIST(8) = ND
c        write (*,*) "INTLIST(8) = ND", INTLIST(8), ND

	IF(ND .GT. max_doses) THEN
	 WRITE(*,125)
  125    FORMAT(' YOUR PATIENT DATA FILES CANNOT HAVE MORE THAN '/
     1' max_doses DOSE EVENTS. THE PROGRAM IS NOW STOPPING. '/)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,125) 
        CLOSE(42)

       CALL PAUSE
	 STOP
	ENDIF

	READ(27,*)

	READ(27,*)

        IF(ND.EQ.0) GO TO 40

	DO I = 1,ND
         READ(27,*) SIG(I),(RS(I,J),J=1,NI)
C         SIGCOPY(I)=SIG(I)
C         do JJJ = 1,NI
C            rscopy(I,JJJ) = rs(I,JJJ)
C         end do
	END DO	 

C  ASSIGN THE VALUES IN EACH DRUG'S PO COLUMN TO THE CORRESPONDING
C  COLUMN IN ARRAY BS.

        DO I=1,ND
         DO J=1,NDRUG
          BS(I,J)=RS(I,2*J)
C          BSCOPY(I,J)=RS(I,2*J)
         END DO
        END DO

C  READ THE NO. OF OUTPUT EQUATIONS FROM THE LINE WITH 'NO. OF TOTAL'
C  AS ENTRIES 12:23. THEN READ NO. OF OBSERVED VALUE TIMES, ETC.

   40	READ(27,1) READLINE
        IF(READLINE(12:23) .NE. 'NO. OF TOTAL') GO TO 40
        BACKSPACE(27)

        READ(27,*) NUMEQTT

C wmy2017Sep30
        INTLIST(9) = NUMEQTT
c        write (*,*) "INTLIST(9) = NUMEQT", INTLIST(9), NUMEQTT

C  NOTE THAT NUMEQT IS PROVIDED TO THIS ROUTINE IN THE ARGUMENT LIST SO
C  THAT IT CAN BE USED TO VARIABLY DIMENSION SEVERAL ARRAYS. SO, INSTEAD
C  OF READING IN NUMEQT ABOVE, READ IN NUMEQTT. BUT NOW CHECK THAT
C  THESE TWO VALUES ARE EQUAL.

      IF(NUMEQTT .NE. NUMEQT) THEN

       WRITE(*,127) NUMEQT,NUMEQTT
  127  FORMAT(/' THERE IS A CONFLICT IN SUBROUTINE FILRED.'/
     1' NUMEQT = ',I2,', BUT NUMEQTT = ',I2/
     2' THESE TWO VALUES SHOULD BE THE SAME. SOMETHING IS AMISS WITH'/
     3' AT LEAST ONE OF YOUR PATIENT DATA FILES. THE PROGRAM IS NOW'/
     4' STOPPING. '/)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,127) NUMEQT,NUMEQTT 
        CLOSE(42)

       CALL PAUSE
       STOP

      ENDIF


        READ(27,3) M
C wmy2017Sep30
        INTLIST(10) = M
c        write (*,*) "INTLIST(10) = M", INTLIST(10), M

	MAXOBDIM = max_obs_dim
	IF(M .GT. MAXOBDIM) THEN
  	 WRITE(*,126) MAXOBDIM
  126    FORMAT(/' AT LEAST ONE OF YOUR PATIENT DATA FILES HAS TOO'/
     1' MANY OBSERVED VALUE TIMES. THIS NO. CANNOT EXCEED ',I5,'.'/
     2' THE PROGRAM IS NOW STOPPING. '/)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,126) MAXOBDIM
        CLOSE(42)

       CALL PAUSE
	 STOP
	ENDIF


         DO I=1,M
          READ(27,*) TIM(I),(YO(I,J),J=1,NUMEQT)
C          TIMCOPY(I)=TIM(I)
         END DO
 
C  PUT YO VALUES INTO YOO BECAUSE A DUMMY ARGUMENT CANNOT BE IN A
C  COMMON STATEMENT.
 
C	DO I=1,M
C	 DO J=1,NUMEQT
C	  YOO(I,J) = YO(I,J)
C	 END DO
C	END DO
 
	NOBSER=M
 
C  AT THIS POINT, MUST SKIP THE COVARIATE INFO IN THE FILE, AND PROCEED
C  TO READ THE ASSAY NOISE COEFFICIENTS BELOW THAT.
 
C  READ THE NUMEQT SETS OF ASSAY COEFFICIENTS JUST BELOW THE LINE
C  WHICH HAS "ASSAY COEFFICIENTS FOLLOW" IN ENTRIES 1:25.

   50	READ(27,1) READLINE
	IF(READLINE(1:25) .NE. 'ASSAY COEFFICIENTS FOLLOW') GO TO 50

	DO IEQ = 1,NUMEQT
	 READ(27,*) C0(IEQ),C1(IEQ),C2(IEQ),C3(IEQ),C4(IEQ),C5(IEQ)
	END DO
 
	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE CALGRD(NVAR,NGRID,AB,X,errfil)
C     1     S,QS,COEF,RQS,NEXTN,TESTN,HISUM)

        USE npag_utils, only: max_pop_rand_varbs

	IMPLICIT REAL*8(A-H,O-Z)

        double precision, dimension(max_pop_rand_varbs) :: X, QUASI
        double precision, dimension(max_pop_rand_varbs,2) :: AB

      CHARACTER ERRFIL*20

        INTEGER S,QS,COEF(0:19,0:19),NEXTN,TESTN,HISUM
        double precision RQS
      save S,QS,COEF,NEXTN,TESTN,HISUM,RQS


        save first,flag
	logical flag(2),first
        data first/.TRUE./
C      COMMON/ERR/ERRFIL 

c wmy2017Oct09
c  save flag required for run in parallel, but not serial
c  since CALGRD is called _outside_ of the parallel region
C  I have no idea what the issue really is!
 
C  THIS SUBROUTINE, CALLED BY MAIN, DETERMINES THE COORDINATES OF
C  THE IGTH GRID POINT, AND STORES THEM INTO X.
 
C  INPUT ARE:
 
C  NVAR = NO. OF COORDINATES (RANDOM VARIABLES).
C  NGRID = TOTAL NO. OF GRID POINTS
C  [AB(I,1),AB(I,2)] = GRID BOUNDARIES FOR COORDINATE (R.V.) I,
C                      I=1,NVAR.
 
C  OUTPUT IS:
 
C  X(I), I=1,NVAR = AS DESCRIBED ABOVE.
 
 
C  THE ICTH COORDINATE IS:
C       (AB(IC,2)-AB(IC,1))*QUASI(IC) + AB(IC,1),
C       WHERE QUASI(IC) is a quasi-random variable from the low discrepancy
C       FAURE sequence on the NVAR dimensional hyypercube defined by
C       ACM TOMS algorithm 647
C       (routines INFAUR and GOFAUR, available from www.netlib.org in
C       the TOMS directory are included below and are
C       properly set up for both SUNs, IBM workstations, and the CRAY
C       T3E.

 
c       first, we initialize the Faure sequence with a call to INFAUR
c       if this is the first time that we want a number.
 
c        write (*,*) first,"Calling INFAUR with",
c     1    nvar,ngrid,FLAG(1),FLAG(2)

        if(first) then
         CALL INFAUR(flag,nvar,ngrid,
     1     S,QS,COEF,RQS,NEXTN,TESTN,HISUM)
         first = .FALSE.
        endif
 

c here nvar = dimensionality of grid, ngrid = number of grid
c points to be generated, and flag(1) and flag(2) = logical variables
c set to 'T" if call is successful.
 
        IF(.NOT. FLAG(1)) THEN
	 WRITE(*,11) NVAR
   11    FORMAT(/' THE NUMBER OF RANDOM VARIABLES, ',I3,', IS NOT '/
     1' ACCEPTABLE IN SUBROUTINE INFAUR.')

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.


        OPEN(42,FILE=ERRFIL)
         WRITE(42,11) NVAR
        CLOSE(42)

	 CALL PAUSE
	 STOP
        ENDIF
 
	IF(.NOT. FLAG(2)) THEN
	 WRITE(*,12) NGRID
   12    FORMAT(/' THE NUMBER OF REQUESTED GRID POINTS, ',I8,', IS NOT'/
     1' ACCEPTABLE IN SUBROUTINE INFAUR.')

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,12) NGRID
        CLOSE(42)

	 CALL PAUSE
         STOP
        ENDIF
 
c now we call gofaur to generate  the nvar coordinaters inside
c the unit hypercube that will later be scaled to the grid point.
 
        CALL GOFAUR(quasi,
     1     S,QS,COEF,RQS,NEXTN,TESTN,HISUM)

 
c now we scale each of the i-coordinates to lie
c between AB(I,1) and AB(I,2).
 
	DO IC = 1,NVAR
        X(IC) = (AB(IC,2)-AB(IC,1))*quasi(IC) + AB(IC,1)
	END DO
 
	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE INFAUR(FLAG,DIMEN,ATMOST,
     1     S,QS,COEF,RQS,NEXTN,TESTN,HISUM)

        implicit double precision (a-h,o-z)
        LOGICAL FLAG(2)
 
      INTEGER S,ATMOST,QS,COEF(0:19,0:19),NEXTN,
     +        TESTN,HISUM,I,J,PRIMES(40),DIMEN
 

C      COMMON /FAURE/ S,QS,COEF,RQS,NEXTN,TESTN,HISUM
C      SAVE /FAURE/
 
      DATA (PRIMES(I),I=1,40)/1,2,3,5,5,7,7,11,11,11,11,
     +                        13,13,17,17,17,17,19,19,
     +                        23,23,23,23,29,29,29,29,
     +                        29,29,31,31,37,37,37,37,
     +                        37,37,41,41,41/
 
 
C      ALGORITHM 659, COLLECTED ALGORITHMS FROM ACM.

C      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
C      VOL. 14, NO. 1, P.88.
 
C       THIS SUBROUTINE FIRST CHECKS WHETHER
C       THE USER-SUPPLIED DIMENSION "DIMEN" OF THE
C       QUASIRANDOM VECTORS IS ACCEPTABLE
C       (STRICTLY BETWEEN 1 AND 41) : IF SO,
C       FLAG(1)=.TRUE.
 
C       THEN IT CALCULATES AN UPPER SUMMATION
C       LIMIT "HISUM" BASED ON "DIMEN" AND THE
C       USER-SUPPLIED NUMBER "ATMOST" OF QUASIRANDOM
C       VECTORS REQUIRED. FLAG(2)=.TRUE. IF
C       ATMOST IS OK.
 
C       IF FLAG(1) AND FLAG(2) ARE TRUE,
C       "INFAUR" NEXT PRODUCES THE OTHER
C       OUTPUTS LISTED BELOW PASSED TO
C       SUBROUTINE GOFAUR VIA LABELLED
C       COMMON "FAURE". THESE OUTPUTS ARE
C       IRRELEVANT TO THE USER.
 
C       FIRST CALL INFAUR. IF FLAG(1) AND
C       FLAG(2) ARE TRUE, EACH (SUBSEQUENT)
C       CALL TO GOFAUR GENERATES A NEW
C       QUASIRANDOM VECTOR.
 
C       INPUTS : DIMEN, ATMOST
 
C       OUTPUTS
C          TO USERS CALLING PROGRAM:
C             FLAG
C             QSS   : SAME AS QS - SEE BELOW
 
C          TO GOFAUR:
C             S      :DIMENSION
C             QS     :SMALLEST PRIME >=S
C             COEF   :TABLE OF BINOMIAL
C                     COEFFICIENTS NEEDED
C                     BY GOFAUR.
C             NEXTN  :THE NUMBER OF THE
C                     NEXT QUASIRANDOM
C                     VECTOR,INITIALIZED
C                     TO TESTN-1 HERE.
C             TESTN  :INITIALIZED TO QS**4
C             HISUM  :AFTER BEING USED TO
C                     PRODUCE COEF, INITIALIZED
C                     TO 3 FOR GOFAUR.
C             RQS    :1.0/QS.
 
C       CHECK S
 
      S=DIMEN
      FLAG(1) = S.GT.1 .AND. S.LT.41
c
c wmy2017Oct09 -- debug -- flag was not SAVEd 
c   between calls in parallel - but in serial
c   code worked fine. The decision immediately
c   above this write statement was always good.
c
c      write (*,*) "In INFAUR test S in (1,41)",
c     1  S,FLAG(1),FLAG(2)

      IF (.NOT.FLAG(1)) RETURN
 

      QS=PRIMES(S)
      TESTN=QS**4
 
C         COMPUTE LOG(ATMOST+TESTN) IN BASE QS
C         USING A RATIO OF NATURAL LOGS TO GET
C         AN UPPER BOUND ON (THE NUMBER OF
C         DIGITS IN THE BASE QS REPRESENTATION
C         OF ATMOST+TESTN) MINUS ONE.
 
      HISUM=NINT(LOG(REAL(ATMOST+TESTN))/LOG(REAL(QS)))
      FLAG(2)=HISUM.LT.20
      IF(.NOT. FLAG(2)) RETURN
 
C        NOW FIND BINOMIAL COEFFICIENTS MOD QS
C        IN A LOWER-TRIANGULAR MATRIX "COEF"
C        USING RECURSION BINOM(I,J)=BINOM(I-1,J)
C        +BINOM(I-1,J-1) AND A=B+C IMPLIES MOD(A,D)=
C        MOD(MOD(B,D)+MOD(C,D),D)
 
      COEF(0,0)=1
      DO 50 J=1,HISUM
        COEF(J,0)=1
        COEF(J,J)=1
   50 CONTINUE
      DO 200 J=1,HISUM
        DO 100 I=J+1,HISUM
          COEF(I,J)=MOD(COEF(I-1,J)+COEF(I-1,J-1),QS)
  100   CONTINUE
  200 CONTINUE
 
C        CALCULATING THESE COEFFICIENTS
C        MOD QS AVOIDS POSSIBLE OVERFLOW
C        PROBLEMS WITH RAW BINOMIAL COEFFICIENTS
 
C        NOW COMPLETE INITIALIZATION
C        AS DESCRIBED IN SECTION 2.
C        NEXTN HAS 4 DIGITS IN BASE
C        QS, SO HISUM EQUALS 3.
 

      NEXTN=TESTN-1
      HISUM=3
      RQS=1.0/REAL(QS)
 
      RETURN
      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE GOFAUR(QUASI,
     1    S,QS,COEF,RQS,NEXTN,TESTN,HISUM)

        USE npag_utils, only: max_pop_rand_varbs

        implicit double precision (a-h,o-z)
        INTEGER S,QS,COEF(0:19,0:19),NEXTN,TESTN,
     +        HISUM,I,J,K,YTEMP(0:19),ZTEMP,
     +        KTEMP,LTEMP,MTEMP
 
C QUASI DIMENSION CHANGED FROM 40 TO 25 TO MATCH DIMENSION IN CALGRD.


      dimension QUASI(max_pop_rand_varbs)
C      COMMON /FAURE/ S,QS,COEF,RQS,NEXTN,TESTN,HISUM
C      SAVE /FAURE/
 
C       THIS SUBROUTINE GENERATES A NEW
C       QUASIRANDOM VECTOR WITH EACH CALL
 
C       IT IMPLEMENTS A METHOD OF H.FAURE,
C       "ACTA ARITHMETICA XLI(1982),337-351".

C       (SEE ESPECIALLY PAGE 342).
 
C       THE USER MUST CALL "INFAUR" BEFORE
C       CALLING "GOFAUR".
C       AFTER CALLING "INFAUR", TEST FLAG(1)
C       AND FLAG(2); IF EITHER IS FALSE, DO
C       NOT CALL GOFAUR. READ THE COMMENTS AT
C       THE BEGINNING OF INFAUR AND THEN
C       THOSE BELOW.
 
C       ALL INPUTS COME FROM "INFAUR" VIA
C       LABELLED COMMON "FAURE"; FOR THEIR
C       DEFINITIONS, SEE "INFAUR".
 

C       INPUTS:
C         S,QS,COEF,NEXTN,TESTN,HISUM,RQS
 
C       OUTPUTS:
C         TO USER'S CALLING PROGRAM:
C         QUASI - A NEW QUASIRANDOM VECTOR

 
 
C       FIND QUASI(1) USING FAURE (SECTION 3.3)

 
C       NEXTN HAS A REPRESENTATION IN BASE
C       QS OF THE FORM: SUM OVER J FROM ZERO
C       TO HISUM OF YTEMP(J)*(QS**J)
 
C       WE NOW COMPUTE THE YTEMP(J)'S.
 
      KTEMP=TESTN
      LTEMP=NEXTN
      DO 100 I=HISUM,0,-1
          KTEMP=KTEMP/QS
          MTEMP=MOD(LTEMP,KTEMP)
          YTEMP(I)=(LTEMP-MTEMP)/KTEMP
          LTEMP=MTEMP

  100   CONTINUE
 

C       QUASI(K) HAS THE FORM SUM OVER J
C       FROM ZERO TO HISUM OF
C       YTEMP(J)*(QS**(-(J+1)))
 
C       READY TO COMPUTE QUASI(1)
C       USING NESTED MULTIPLICATION
 
      R=YTEMP(HISUM)

      DO 200 I=HISUM-1,0,-1

          R=YTEMP(I)+RQS*R
  200   CONTINUE
      QUASI(1)=R*RQS
 
C       FIND THE OTHER S-1 COMPONENTS
C       OF QUASI USING "FAURE" (SECTIONS
C       3.2 AND 3.3)
 
      DO 500 K=2,S
          QUASI(K)=0.0
          R=RQS
          DO 400 J=0,HISUM
              ZTEMP=0
              DO 300 I=J,HISUM
                  ZTEMP=ZTEMP+COEF(I,J)*YTEMP(I)
 
C       NO APPARENT ALTERNATIVE
C       ONE-DIMENSIONAL COEFFICIENT ARRAY
C       EXCEPT VIA SUBSCRIPT ADDRESS
C       COMPUTATIONS AND EQUIVALENCING
 
  300           CONTINUE
 
C       NEW YTEMP(J) IS THE SUM
C       OVER I FROM J TO HISUM

C       OF (OLD YTEMP(I)*BINOM(I,J))
C       MOD QS
 
              YTEMP(J)=MOD(ZTEMP,QS)
              QUASI(K)=QUASI(K)+YTEMP(J)*R
              R=R*RQS
  400       CONTINUE

  500   CONTINUE
 
C       UPDATE NEXTN AND, IF NEEDED, TESTN AND
C       HISUM
 
      NEXTN=NEXTN+1
      IF(NEXTN.EQ.TESTN) THEN
        TESTN=TESTN*QS
        HISUM=HISUM+1
 
C       SINCE FLAG(2) IS TRUE,
C       HISUM STAYS UNDER 20
 
      ENDIF
 
      RETURN
      END

C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE NOTINT(VOLSPA,NGRID,NACTVE,FUNC,MAXGRD,ESTINT)
	IMPLICIT REAL*8(A-H,O-Z)

	DIMENSION FUNC(MAXGRD)
C
C  THIS SUBROUTINE, CALLED BY MAIN, IS A MULTI-DIMENSIONAL INTEGRATOR.

C
C  INPUT ARE:


C
C  VOLSPA = 'VOLUME' OF THE INTEGRATION SPACE.
C  NGRID = NO. OF GRID POINTS OVER WHICH THE INTEGRATION IS DONE.
C  NACTVE = NO. OF CURRENTLY ACTIVE GRID POINTS.
C  FUNC(I), I=1,NACTVE = VALUE OF THE FUNCTION TO BE INTEGRATED AT

C                       THE ITH GRID POINT.
C  MAXGRD = DIMENSION OF FUNC -- SEE EXPLATION IN MAIN.
C
C  OUTPUT IS:
C
C  ESTINT = THE ESTIMATE OF THE NVAR-DIM INTEGRAL OF THE FUNCTION WHOSE
C           VALUES ARE GIVEN IN FUNC.
C
	SUM=0.D0
	DO 100 IG=1,NACTVE
  100   SUM=SUM+FUNC(IG)
	ESTINT=VOLSPA*SUM/NGRID
	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
       SUBROUTINE STAZ(VOLSPA,NGRID,NACTVE,NVAR,IND,CORDEN,PROD,MAXGRD,
     1  NINT,X1,X2,XMODE,X025,X25,X50,X75,X975,SCALINFO,NSUB,MAXDIM)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION CORDEN(MAXGRD,MAXDIM+1),PROD(MAXGRD),X(1999),

     1  PROB(1998),CUMPRO(0:1998)
 
C  THIS SUBROUTINE, CALLED BY MAIN, CALCULATES THE MODE AND 5 %-TILE
C  VALUES OF THE APPROXIMATE MARGINAL DENSITY OF R.V. IND, WHOSE

C  BOUNDARIES [X1,X2] ARE INPUT. ALSO, THE
C  SCALED 'INFO' FOR THIS MARGINAL DENSITY IS CALCULATED.
 
 
C  INPUT ARE:
C
C  VOLSPA = 'VOLUME' OF THE INTEGRATION SPACE.
C  NGRID = THE NO. OF GRID POINTS.
C  NACTVE = THE NO. OF CURRENTLY ACTIVE GRID POINTS.
C  NVAR = NO. OF R.V.'S.
C  IND = INDEX OF THE R.V. WHOSE STATISTICS ARE BEING FOUND.
C  CORDEN(I,J) = JTH COORDINATE FOR THE ITH GRID POINT, J=1,NVAR;
C		 I=1,NACTVE;
C		 DENSITY FOR THE ITH GRID POINT, J=NVAR+1;I=1,NACTVE.
C  PROD(I), I=1,NACTVE = DUMMY ARRAY; IN ARGUMENT LIST SO IT CAN BE

C                       VARIABLY DIMENSIONED.
C  MAXGRD = DIMENSION OF CORDEN AND PROD -- SEE EXPLANATION IN MAIN.
C  NINT = THE NO. OF INTERVALS TO BE USED IN CALCULATING THE APPROXIMATE
C         MARGINAL DENSITY OF R.V. INDICATED ABOVE (CHANGED -SEE BELOW).
C  [X1,X2] = BOUNDS ON THE R.V. INDICATED ABOVE.
C  NSUB = NO. OF SUBJECTS.

C  MAXDIM = DIMENSION OF CORDEN.

 
C  OUTPUT IS:
C
C  XMODE = MODE OF THE APPROXIMATE DISTRIBUTION OF R.V. INDICATED ABOVE.
C  X025 =  2.5 %-TILE VALUE OF ABOVE APPROXIMATE DISTRIBUTION.
C  X25 =    25 %-TILE VALUE OF ABOVE APPROXIMATE DISTRIBUTION.

C  X50 =    50 %-TILE VALUE OF ABOVE APPROXIMATE DISTRIBUTION (MEDIAN).
C  X75 =    75 %-TILE VALUE OF ABOVE APPROXIMATE DISTRIBUTION.
C  X975 = 97.5 %-TILE VALUE OF ABOVE APPROXIMATE DISTRIBUTION.
C  SCALINFO = SCALED 'INFO' FOR THIS MARGINAL DENSITY.
 
C  AS OF npbig1.f, THE NO. OF INTERVALS USED IN THIS ROUTINE IS
C  NO LONGER HARDCODED TO BE NINT = 100. THE REASON IS THAT THE
C  SCALED INFO HAS A DENOMINATOR OF LOG(NINT/NSUB) WHICH = 0 IF
C  NSUB = NINT = 100 (--> DIVIDE BY 0 ERROR), AND IF NSUB > 100, THIS
C  PRODUCES A NEGATIVE SCALED INFO (WHICH IS SUPPOSED TO VARY FROM 0
C  TO 100). SO MAKE THE NO. OF INTERVALS = MAX(100,2*NSUB).
 
C  NOTE THAT NINT IS PASSED TO THIS ROUTINE AS ALWAYS 100; DON'T
C  CHANGE NINT; USE NEWINT BELOW.
 
	NEWINT = NINT
	IF(2*NSUB .GT. NEWINT) NEWINT = 2*NSUB
 
 
C  FIND THE NEWINT INTERVALS OF INTEGRATION -- EQUALLY SPACED -- BETWEEN
C  X1 AND X2.
C
	XINT=(X2-X1)/NEWINT
	X(1)=X1

	X(NEWINT+1) = X2 

	DO 5 I=1,NEWINT-1
    5   X(I+1)=X(I)+XINT

 
C  FIND THE APPROXIMATE PROBABILITY THAT THE R.V. IS IN EACH OF THE
C  NEWINT INTERVALS.
 
	DO 1000 INTR=1,NEWINT
 
C  FIND PROB{ R.V. IN [X(INTR),X(INTR+1)] } = PROB(INTR).
C

C  INTEGRATE (CORDEN(I,NVAR+1)*W(X(INTR),X(INTR+1))), WHERE W(A,B) = 1
C  IF R.V. IS IN [X(INTR),X(INTR+1)], AND = 0 IF NOT.
C
C  FIND W EVALUATED AT EACH GRID POINT AND MULTIPLY IT BY
C  CORDEN(I,NVAR+1)
 
	DO IG=1,NACTVE
	  W = 0.D0
	  XX = CORDEN(IG,IND)
	  IF(XX .GE. X(INTR) .AND. XX .LT. X(INTR+1)) W = 1.D0
	  IF(INTR .EQ. NEWINT .AND. XX .GE. X(INTR)) W = 1.D0
C    BUG FIX IN bignpaglap4.f IS THE LINE ABOVE, WHICH GUARANTEES THAT
C    ANY DENSITY VALUE AT THE UPPER BOUNDARY IS COUNTED.


          PROD(IG) = CORDEN(IG,NVAR+1)*W
	END DO
 
 1000  CALL NOTINT(VOLSPA,NGRID,NACTVE,PROD,MAXGRD,PROB(INTR))
 
 
C  WITH PROB(.), CALCULATE THE MODE.
 
	INTMAX=1
	DO INTR=2,NEWINT
	  IF(PROB(INTR) .GT. PROB(INTMAX)) INTMAX=INTR
	END DO
 

	XMODE=(X(INTMAX)+X(INTMAX+1))/2.D0
 
 
C  CALCULATE THE 'SCALED INFORMATION' FOR THIS CYCLE (STARTING WITH
C  PROGRAM m234calc.f, 11-6-94; BUT THE SCALED 'INFO' IS CHANGED
C  IN PROGRAM MXEM2N35.FOR/m235calc.f, 11-19-94):
 
C  INFO = SUM(PI*LOG(PI)), WHERE THE SUM IS OVER I=1,NEWINT, THE LOG
C    	  IS TO BASE 2, PI*LOG(PI) --> 0 IF PI = 0, AND
C	  PI = PROB(I).
 
C  THEN INFO IS SCALED: y = 100*ln(2)/ln(n/N)*(x + ln(n)/ln(2)), WHERE
C  y = SCALED INFO, x = ABOVE INFO, n = NEWINT, N = NO. OF SUBJECTS. SO
C  y = 0 (%) IF DENSITY IS UNIFORM, and y = 100 (%) if DENSITY IS
C  CONCENTRATED AT N EQUALLY LIKELY POINTS, ... I.E., SCALED INFO = 0 %
C  FOR NO INFO, AND SCALED INFO = 100 % FOR PERFECT INFO.
 
	SUM=0.D0
 
	DO I=1,NEWINT
 
	IF (PROB(I) .GT. 0.D0) THEN
	  PI=PROB(I)
	  SUM=SUM+PI*DLOG(PI)
	ENDIF
 
	END DO
 
	DL2=DLOG(2.D0)
	SUM = SUM/DL2
 
C  SUM IS NOW THE INFO FOR THIS MARGINAL DENSITY. SCALE IT AS SHOWN
C  ABOVE.
 
        DINT=NEWINT
	FACT=100.D0*DL2/DLOG(DINT/NSUB)
	SCALINFO = FACT*(SUM + DLOG(DINT)/DL2)
 
 
C  FIND THE 5 %-TILE VALUES.
 
C  X025 IS THE X-VALUE BELOW WHICH IS 2.5 % OF THE DISTRIBUTION.
C  X25  IS THE X-VALUE BELOW WHICH IS 25 % OF THE DISTRIBUTION.
C  X50  IS THE X-VALUE BELOW WHICH IS 50 % OF THE DISTRIBUTION.
C  X75  IS THE X-VALUE BELOW WHICH IS 75 % OF THE DISTRIBUTION.

C  X975 IS THE X-VALUE BELOW WHICH IS 97.5 % OF THE DISTRIBUTION.
 
 
C  ACTUALLY, X025 WILL BE THE WEIGHTED X-VALUE IN THE INTERVAL WHERE
C  THE CUMULATIVE PROBABILITY EXCEEDS 2.5%. SIMILARLY FOR THE OTHER
C  %-TILE VALUES.
 
C  FIND THE INTERVALS WHERE THE CUMULATIVE PROBABILITES EXCEED 2.5%,
C  25%, 50%, 75%, AND 97.5%.
C
C  CUMPRO(I) BELOW IS THE CUMULATIVE PROBABILITY OF THE DISTRIBUTION
C  THROUGH INTERVAL I.
 
	CUMPRO(0)=0.D0
	DO 1200 INTR=1,NEWINT
	CUMPRO(INTR)=CUMPRO(INTR-1)+PROB(INTR)
 
	IF(CUMPRO(INTR-1) .LT. .025D0 .AND. CUMPRO(INTR) .GE. .025D0)
     1  IND025=INTR
	IF(CUMPRO(INTR-1) .LT. .25D0 .AND. CUMPRO(INTR) .GE. .25D0)
     1  IND25=INTR
	IF(CUMPRO(INTR-1) .LT. .50D0 .AND. CUMPRO(INTR) .GE. .50D0)
     1  IND50=INTR

	IF(CUMPRO(INTR-1) .LT. .75D0 .AND. CUMPRO(INTR) .GE. .75D0)
     1  IND75=INTR
	IF(CUMPRO(INTR-1) .LT. .975D0 .AND. CUMPRO(INTR) .GE. .975D0)
     1  IND975=INTR
 
 1200 CONTINUE
C

C  NOW IND025 IS THE INTERVAL WHERE THE CUMULATIVE PROBABILITY EXCEEDS
C  .025 (SIMILARLY FOR THE OTHER IND'S).
C
	X025 = X(IND025)+(X(IND025+1)-X(IND025))*
     1  (.025D0-CUMPRO(IND025-1))/(CUMPRO(IND025)-CUMPRO(IND025-1))

 
	X25 = X(IND25)+(X(IND25+1)-X(IND25))*
     1  (.25D0-CUMPRO(IND25-1))/(CUMPRO(IND25)-CUMPRO(IND25-1))
 

	X50 = X(IND50)+(X(IND50+1)-X(IND50))*
     1  (.50D0-CUMPRO(IND50-1))/(CUMPRO(IND50)-CUMPRO(IND50-1))
 

	X75 = X(IND75)+(X(IND75+1)-X(IND75))*

     1  (.75D0-CUMPRO(IND75-1))/(CUMPRO(IND75)-CUMPRO(IND75-1))
 

	X975 = X(IND975)+(X(IND975+1)-X(IND975))*

     1  (.975D0-CUMPRO(IND975-1))/(CUMPRO(IND975)-CUMPRO(IND975-1))
 
 
	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE EQUIV(INUM,NAME)

 
	CHARACTER*1 A,B,C,D
	CHARACTER NAME*4
 
C  THIS SUBROUTINE, CALLED BY MAIN, INPUTS INTEGER INUM, AND RETURNS THE
C  4-CHARACTER EQUIVALENT IN NAME.
 
	I4 = INUM/1000
	ILEFT = INUM - I4*1000
	I3 = ILEFT/100
	ILEFT = ILEFT - I3*100
	I2 = ILEFT/10
 	ILEFT = ILEFT - I2*10
	I1 = ILEFT
 
C  CONVERT THIS TO THE CHARACTER EQUIVALENT.
 
	IF(I4 .EQ. 1) A='1'
	IF(I4 .EQ. 2) A='2'
	IF(I4 .EQ. 3) A='3'
	IF(I4 .EQ. 4) A='4'
	IF(I4 .EQ. 5) A='5'
	IF(I4 .EQ. 6) A='6'
	IF(I4 .EQ. 7) A='7'
	IF(I4 .EQ. 8) A='8'
	IF(I4 .EQ. 9) A='9'
	IF(I4 .EQ. 0) A='0'
 
	IF(I3 .EQ. 1) B='1'
	IF(I3 .EQ. 2) B='2'
	IF(I3 .EQ. 3) B='3'
	IF(I3 .EQ. 4) B='4'
	IF(I3 .EQ. 5) B='5'
	IF(I3 .EQ. 6) B='6'
	IF(I3 .EQ. 7) B='7'
	IF(I3 .EQ. 8) B='8'
	IF(I3 .EQ. 9) B='9'
	IF(I3 .EQ. 0) B='0'
 
	IF(I2 .EQ. 1) C='1'
	IF(I2 .EQ. 2) C='2'
	IF(I2 .EQ. 3) C='3'
	IF(I2 .EQ. 4) C='4'
	IF(I2 .EQ. 5) C='5'
	IF(I2 .EQ. 6) C='6'
	IF(I2 .EQ. 7) C='7'
	IF(I2 .EQ. 8) C='8'
	IF(I2 .EQ. 9) C='9'
	IF(I2 .EQ. 0) C='0'
 
	IF(I1 .EQ. 1) D='1'
	IF(I1 .EQ. 2) D='2'
	IF(I1 .EQ. 3) D='3'
	IF(I1 .EQ. 4) D='4'
	IF(I1 .EQ. 5) D='5'
	IF(I1 .EQ. 6) D='6'
	IF(I1 .EQ. 7) D='7'
	IF(I1 .EQ. 8) D='8'
	IF(I1 .EQ. 9) D='9'
	IF(I1 .EQ. 0) D='0'
 
	NAME = A//B//C//D
 
	RETURN
	END

C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      SUBROUTINE MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,RANFIXEST,PX)

C
C
C                    THIS ROUTINE MOVED INTO npag_utils.mod
C

C	IMPLICIT REAL*8(A-H,O-Z)
C	DIMENSION IRAN(max_ODE_params),X(max_pop_rand_varbs),
C     1   VALFIX(20),PX(max_ODE_params),RANFIXEST(20)

C  THIS ROUTINE, CALLED BY MAIN, INPUTS NVAR, NOFIX, NRANFIX, IRAN, X,
C  VALFIX, AND RANFIXEST, AND RETURNS PX(I) = A COMBINATION OF THE 
C  VALUES IN X, VALFIX, AND RANFIXEST, IN THE PROPER ORDER (AS 
C  DETERMINED BY IRAN).
 
C      NNNVAR = 0
C      NNNFIX = 0
C      NNNRANFIX = 0

C      DO I = 1,NVAR+NOFIX+NRANFIX
 
C       IF(IRAN(I) .EQ. 1) THEN
C        NNNVAR = NNNVAR+1
C        PX(I) = X(NNNVAR)
C       ENDIF
 
C       IF(IRAN(I) .EQ. 0) THEN
C        NNNFIX = NNNFIX+1
C        PX(I) = VALFIX(NNNFIX)
C       ENDIF

C       IF(IRAN(I) .EQ. 2) THEN
C        NNNRANFIX = NNNRANFIX+1
C        PX(I) = RANFIXEST(NNNRANFIX)
C       ENDIF
 
C      END DO
 
c      write (*,*) "Initialized IG",NNNVAR,NNNFIX,NNNRANFIX

C      RETURN
C      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C	SUBROUTINE SUBRES(MAXSUB,MAXACT,JSUB,CORDEN,WORK,MAXGRD,MAXDIM,
C     1  NVAR,NOFIX,VALFIX,SIGFAC,OFAC,AB,PAR,NACTVE,NGRID,VOLSPA,IRAN,
C     2  CENTER,PYJGXX,NRANFIX,RANFIXEST,NOBSER,NUMEQT)
C wmy2017Sep12
      SUBROUTINE SUBRES(MAXSUB,MAXACT,JSUB,CORDEN,WORK,MAXGRD,MAXDIM,
     1  NVAR,NOFIX,VALFIX,SIGFAC,OFAC,AB,PAR,NACTVE,NGRID,VOLSPA,IRAN,
     2  CENTER,PYJGXX,NRANFIX,RANFIXEST,NOBSER,NUMEQT,NBCOMP,
     3  NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     4  INTLIST,IPAR,ObsError,RPAR,NACTSUB,BAYPOS,ERRFIL)
C     4  INTLIST,IPAR,ObsError,RPAR,ERRFIL)

      USE npag_utils, only: verifyval,makevec,maxnumeq,max_m_per_obs
     1  ,max_ODE_params,max_pop_rand_varbs,max_doses,max_pop_params
     2  ,max_ODE_comps,max_pop_varbs,max_RS_J,max_input_dim
     3  ,k_sfac,k_ofac,k_sum_z_sq,k_prod_pr,i_skip_ig,i_do

      IMPLICIT REAL*8(A-H,O-Z)

C      PARAMETER(MAXNUMEQ=7)

C--- ARG LIST
        integer MAXSUB,MAXACT,JSUB
        integer MAXDIM,MAXGRD,NVAR,NOFIX
        REAL*8, dimension(MAXGRD,MAXDIM+1) :: CORDEN
        REAL*8, dimension(MAXGRD) :: WORK
        REAL*8, dimension(max_pop_params) :: VALFIX
        REAL*8 SIGFAC,OFAC
        REAL*8, dimension(max_pop_rand_varbs,2) :: AB
        CHARACTER PAR(max_pop_rand_varbs)*11
        integer NACTVE,NGRID
        REAL*8 VOLSPA
        integer, dimension(max_ODE_params) :: IRAN
        REAL*8, dimension(3,max_pop_rand_varbs) :: CENTER
        REAL*8, dimension(MAXACT) :: PYJGXX
        integer NRANFIX
        REAL*8, dimension(max_pop_varbs) ::  RANFIXEST
        integer NOBSER,NUMEQT,NDIM,MF
        integer, dimension(max_input_dim) :: NBCOMP
        REAL*8 RTOL
        REAL*8, dimension(max_ODE_comps) ::  ATOL
        REAL*8, dimension(max_m_per_obs) :: TIMCOPY
        REAL*8, dimension(max_doses) :: SIGCOPY
        REAL*8, dimension(max_doses,MAXNUMEQ) :: YO
        REAL*8, dimension(max_doses,max_RS_J) :: RSCOPY
        REAL*8, dimension(max_doses,max_input_dim) :: BSCOPY
        integer, dimension(128) :: INTLIST
        integer, dimension(257) :: IPAR
        double precision, dimension(max_m_per_obs,MAXNUMEQ) :: ObsError
        double precision, dimension(257) :: RPAR

C??? SEE COMMENTS BELOW WHERE BAYPOS (AND NACTSUB) ARE STORED. THE 
C  FIRST DIMENSION OF BAYPOS (AND THE ONLY DIMENSION OF NACTSUB) IS
C  SET = 100, RATHER THAN 800, BECAUSE THE PROGRAM IS TOO BIG 
C  OTHERWISE.

C  NOTE THAT BAYPOS STORES THE BAYESIAN POSTERIOR DENSITY FOR EACH
C  SUBJECT, AND NACTSUB STORES THE NO. OF GRID POINTS IN EACH 
C  SUBJECT'S BAYESIAN POSTERIOR.

C      COMMON/BAY/NACTSUB,BAYPOS
        integer, dimension(100) :: NACTSUB
        real*8, dimension(100,1500,31) :: BAYPOS
C      COMMON/ERR/ERRFIL ! passed in from main, ERRFILNAME
        CHARACTER ERRFIL*20

C wmy2017Sep12 Added to the input
C      COMMON/TOUSER/NDIM,MF,RTOL,ATOL

C      COMMON/PARAMD/P
C        real*8, dimension(max_ODE_params) :: P

!  $omp ThreadPrivate(/PARAMD/)

C--- Local varbs

C wmy2017Sep19
        integer IG,NPX,NEWIND
        REAL*8 W,KU,DENMAX,D,SUMD,FACT
        REAL*8, dimension(max_ODE_params) :: PX
        REAL*8, dimension(max_ODE_comps) :: RCOPY,BCOPY
        REAL*8, dimension(max_pop_rand_varbs) :: EX,STD,COFVR,X
        REAL*8, dimension(max_pop_rand_varbs,max_pop_rand_varbs) ::
     1    COV,E,CORR
        REAL*8, dimension(100) :: XVERIFY

C  AS OF MXEM2N54, SUBRES IS CHANGED. RATHER THAN CALCULATE THE
C  'FINAL FITTED JOINT P.D.F.' FOR EACH SUBJECT, IT CALCULATES
C  THE BAYESIAN POSTERIOR DENSITY FOR EACH SUBJECT. IN PARTICULAR, FOR
C  A SUBJECT WITH OBSERVATIONS Y, THE  BAYESIAN POSTERIOR DENSITY IS
C  P(XI|Y) FOR EACH REMAINING GRID POINT (XI) IN THE POPULATION FINAL
C  CYCLE JOINT DENSITY, WHERE P(XI|Y) = PF(XI) * P(Y|XI) / P(Y),
C  P(Y) = INTEGRAL OF P(Y|XI)*PF(XI), AND PF(XI) IS THE POPULATION
C  FINAL CYCLE JOINT DENSITY VALUE AT XI.
 
C  THIS ROUTINE, CALLED BY MAIN,

C  INPUTS THE FINAL JOINT DENSITY (CORDEN)
C  FROM THE PROGRAM (FOR THE ENTIRE POPULATION) AS THE APRIORI DENSITY
C  FOR A SINGLE SUBJECT WHOSE CONCENTRATION LEVELS, ETC. HAVE ALREADY
C  BEEN READ INTO COMMON STATEMENTS FOR MODULE IDPC, AND CALCULATES THAT
C  SUBJECT'S BAYESIAN POSTERIOR JOINT DENSITY. IT OUTPUTS LOG-LIKS,
C  EXPECTED VALUES, ETC. FOR THIS DENSITY. THE ONLY VALUES RETURNED TO
C  MAIN ARE THE FINAL CYCLE EXPECTED VALUES, MEDIANS, AND MODES
C  (IN CENTER).

C  NOTE THAT PYJGXX(IG) = IS CALCULATED IN LOOP 800 AND RETURNED
C  TO MAIN.

C  NOTE: THE RESULTS ARE OUTPUT TO THE SCREEN AND FILE 25.
 
C  FOR DETAILED COMMENTS ON THE FOLLOWING CODE, SEE THE SIMILAR CODE
C  IN MAIN.
 
        NSUB=1
        NINT=100
 
        WRITE(*,5432)
 5432   FORMAT('1')

        DENMAX=CORDEN(1,NVAR+1)
        DO I=1,NACTVE
          D=CORDEN(I,NVAR+1)
          IF(D .GT. DENMAX) DENMAX=D
        END DO
 
	SUMD=0.
	NEWIND=0

        DO I=1,NACTVE
         D=CORDEN(I,NVAR+1)
          IF(D .GT. 1.D-10*DENMAX) THEN
          SUMD=SUMD+D
          NEWIND=NEWIND+1
           DO J=1,NVAR
            CORDEN(NEWIND,J)=CORDEN(I,J)
           END DO
          CORDEN(NEWIND,NVAR+1)=D
         ENDIF
        END DO

        NACTVE=NEWIND
 
        FACT = NGRID/VOLSPA/SUMD
 
	DO I=1,NACTVE
	  CORDEN(I,NVAR+1)=CORDEN(I,NVAR+1)*FACT
	END DO

 
     	WRITE(*,1241) JSUB
	WRITE(25,1241) JSUB
 1241   FORMAT(//' THE BAYESIAN POSTERIOR DENSITY RESULTS FOLLOW FOR '/
     1' SUBJECT NO. ',I4/)
 
          DO 800 IG=1,NACTVE

C  SEE COMMENTS IN LOOP 800 IN MAIN.
 
            DO J=1,NVAR
              X(J)=CORDEN(IG,J)
            END DO

C wmy2018Mar07 PX is a local variable in this subroutine, unlike in main
c   where PX is a variable of /TOCALC/
            CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,RANFIXEST,PX)
            NPX = NVAR+NOFIX+NRANFIX
C       if (NPX < max_ODE_params) then exit progam w/error
            IPAR(i_skip_ig) = 1

C wmy2018Jan12 I removed COMMON/PARAMD/P from SUBROUTINE IDCALCY and
c  IDCALCYY, so P must be initialized in main after calling MAKEVEC, but
c  before calling IDCALCY/YY. Note: IDCALCY/YY now call SYMBOL and then 
c  immediately call FUNC2() or FUNC3(). 

C            DO I=1,NVAR+NOFIX+NRANFIX
c wmy 10/5/2018 removed /PARAMD/         ! 20190903 removed /PARAMD/
c            DO I=1,NPX
c              P(I) = PX(I)
c            END DO

C wmy2017Sep12 Added /TOUSER/ varbs to CALL IDPC()
C        CALL IDPC(NPX,PX,W,NOBSER,NUMEQT)

C            write (*,*) PX(1),PX(2), PX(3), PX(4)
C     1        , PX(5), PX(6), PX(7), PX(8), PX(9), PX(10)

            CALL IDPC(JSUB,IG,NPX,PX,NBCOMP,W,NOBSER,NUMEQT,NDIM,MF,
     1        RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     2        INTLIST,IPAR,ObsError,RPAR,ERRFIL)

C            write (*,*) "DO 7000 IDPC(JSUB,IG,W)",JSUB,IG,W

            PYJGX=0.D0

C
C Old Code, Assumes all observations are ~ Normal
C 
C            IF (W .LE. 22708.D0) THEN
C              PYJGX = DEXP(-.5D0*W)/SIGFAC/OFAC
C            ENDIF
C
C
C New Code, Assumes Mix of ~ Poisson and ~ Normal
C
          IF(RPAR(k_sum_z_sq) .LE. 22708.D0) THEN
            PYJGX = 10**RPAR(k_prod_pr)
     1        * DEXP(-.5D0*RPAR(k_sum_z_sq))/RPAR(k_sfac)/RPAR(k_ofac)
          ENDIF
C
C End New Code

            PYJGXX(IG) = PYJGX 
            WORK(IG)=PYJGX*CORDEN(IG,NVAR+1)

  800     CONTINUE

C  WORK(IG) = P(YJ,XIG), FOR IG=1,NACTVE.
C  PYJ = P(YJ) = INTEGRAL OF WORK.

C wmy2017Sep7
C      write (*,*) "First call to NOTINT()"
 
      CALL NOTINT(VOLSPA,NGRID,NACTVE,WORK,MAXGRD,PYJ)

C      write (*,*) "Returns ", PYJ

C  AS OF npagranfix6.f, VERIFY THAT PYJ IS NOT 0 (SIMILARLY TO THE 
C  CODE IN MAIN.

C      write (*,*) "Second call to NOTINT()"

      CALL NOTINT(VOLSPA,NGRID,NACTVE,WORK,MAXGRD,PYJ)
 
C      write (*,*) "Returns ", PYJ

C  IF PYJ RETURNS AS 0, IT IS BECAUSE P(X,YJ)=WORK IS 0 IN ALL ITS
C  NACTVE ENTRIES. THIS OCCURS WHEN EACH OF NACTVE VALUES OF W (WHICH
C  RETURNS FROM THE CALLS TO IDPC) IS LARGER THAN 1416 (SINCE P(YJ|X)
C  INVOLVES e RAISED TO THE POWER -.5*W, AND e RAISED TO A POWER
C  SMALLER THAN -708 IS SET TO 0 BY, FOR EXAMPLE, THE COMPAC COMPUTER).


C  IN CASE THIS HAPPENS, PRINT A MESSAGE TO THE USER AND STOP.

      IF (PYJ .EQ. 0.D0) THEN

       WRITE(*,26) JSUB
       WRITE(25,26) JSUB
   26  FORMAT(//' FOR SUBJECT, ',I6,' THE PROB. OF THE OBSERVED'/
     1' CONCENTRATIONS (FOR THE INDICATED DOSAGE REGIMEN), GIVEN EACH '/
     2' AND EVERY GRID POINT IN THE ESTABLISHED GRID, IS 0. THE '/
     3' PROGRAM STOPS. THE USER SHOULD CONSIDER INCREASING THE SIZES'/
     4' OF (C0,C1,C2,C3), THE ASSAY NOISE COEFFICIENTS, WHICH WILL'/
     5' HAVE THE EFFECT OF MAKING THE ABOVE CONDITIONAL PROBABILITES'/
     6' LARGER.')

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,26) JSUB 
        CLOSE(42)

        CALL PAUSE
        STOP

      ENDIF

 


C  THE BAYESIAN POSTERIOR DENSITY OF THIS SUBJECT IS, FOR GRID PT. IG,

C  P(XIG|YJ) = P(YJ,XIG)/P(YJ). PUT THESE VALUES INTO CORDEN(IG,NVAR+1).

	DO IG=1,NACTVE
	  CORDEN(IG,NVAR+1) = WORK(IG)/PYJ
	END DO

C  NEW CODE FOR bigmlt10.f - BELOW


C  CALCULATE HOW MANY OF THE NACTVE GRID POINTS FROM THE FINAL CYCLE
C  ARE "ACTIVE" (WITHIN 1.D-10 OF THE MAXIMUM DENSITY FOR THIS SUBJECT).
C  ... AND, AS OF npageng23.f, ELIMINATE NON-SIGNIFICANT GRID PTS. IN
C  CORDEN (PREVIOUSLY ALL THE POINT FROM THE FINAL CYCLE DENSITY
C  SHOWED UP IN CORDEN, EVEN THOSE WITH INSIGNIFICANT PROBABILITIES).
C  AND NOTE THAT THE BAYESIAN POSTERIOR DENSITY FOR THIS SUBJECT
C  WILL BE STORED INTO BAYPOS(JSUB,.,.), AND PASSED IN COMMON/BAY
C  TO SUBROUTINE READOUT. AND NACTSUB(JSUB) WILL CONTAIN THE NO. OF

C  ACTIVE GRID POINTS FOR THIS SUBJECT'S BAYESIAN POSTERIOR DENSITY.

C  AND NOTE THAT THE NOMINAL DIMENSIONS OF BAYPOS, (800,1500,31),
C  CANNOT BE EXCEEDED BECAUSE THESE ARE THE VALUES FOR MAXSUB, MAXGRD,
C  AND MAXDIM+1, AS SPECIFIED IN THE PARAMETER STATEMENT IN THE PC PREP
C  MAIN MODULE (CURRENTLY NPAG111.FOR). BUT NOTE THAT npageng23.f WILL
C  NOT EXECTUTE WITH THESE DIMENSIONS BECAUSE IT IS TOO BIG FOR A 
C  WIN32 APPLICATION (SEE NPAG111.EXP). SO, IN THIS ROUTINE, AND IN
C  SUBROUTINE READOUT (IN read19.f), THE FIRST DIMENSION HAS BEEN 
C  REDUCED TO 100 (AND SIMILARLY FOR THE DIMENSION OF NACTSUB). IF
C  JSUB > 100, THE BAYESIAN POSTERIOR VALUES BELOW WILL NOT BE STORED.

        DENMAX=CORDEN(1,NVAR+1)

        DO I=1,NACTVE
         D=CORDEN(I,NVAR+1)
         IF(D .GT. DENMAX) DENMAX=D
        END DO
 
        SUMD = 0.D0
        NEWIND = 0

        DO I=1,NACTVE
         D=CORDEN(I,NVAR+1)
          IF(D .GT. 1.D-10*DENMAX) THEN
          SUMD=SUMD+D
          NEWIND=NEWIND+1
           DO J=1,NVAR
            CORDEN(NEWIND,J) = CORDEN(I,J)
            IF(JSUB .LE. 100) BAYPOS(JSUB,NEWIND,J) = CORDEN(I,J)
           END DO
          CORDEN(NEWIND,NVAR+1)=D
         ENDIF
        END DO


C  STORE NACTVE INTO NACTVEFULL BECAUSE THIS VALUE, THE NO. OF ACTIVE
C  GRID POINTS FOR THE ENTIRE POPULATION, IS NEEDED BELOW IN 
C  FORMAT 3258. 


        NACTVEFULL = NACTVE

        NACTVE = NEWIND

C  STORE NACTVE INTO NACTSUB(.) SO IT CAN BE PASSED IN COMMON/BAY TO
C  SUBROUTINE READOUT, ALONG WITH ARRAY BAYPOS. BUT, SEE COMMENTS 
C  ABOVE, ONLY STORE NACTVE IF JSUB .LE. 100.

        IF(JSUB .LE. 100) THEN
          NACTSUB(JSUB) = NACTVE
          IF (NACTSUB(JSUB).GE.1501) THEN
             write (*,*) "Warning :: NACTSUB > 1500 for JSUB", JSUB
             write (*,*) "Warning :: Setting NACTSUB(JSUB) = 1500 for",
     1         JSUB
             NACTSUB(JSUB) = 1500
          ENDIF
        ENDIF
 
        FACT = NGRID/VOLSPA/SUMD
 
	DO I=1,NACTVE
	  CORDEN(I,NVAR+1) = CORDEN(I,NVAR+1)*FACT
        IF(JSUB .LE. 100) BAYPOS(JSUB,I,NVAR+1) = CORDEN(I,NVAR+1) 
	END DO


        SLPYJ=DLOG(PYJ)
 
 3010	WRITE(*,8)
    8   FORMAT(/' THE TRUE (NUMERICAL) LOG-LIKELIHOOD OF THE BAYESIAN'/
     1' POSTERIOR DENSITY FOR THIS SUBJECT, ASSUMING THE PRIOR DENSITY'/
     2' IS THE FINAL CYCLE DENSITY FROM THE POPULATION ANALYSIS, IS: ')
	WRITE(*,*) SLPYJ
 
      WRITE(*,3258) NACTVEFULL
3258  FORMAT(/' OF THE ',I7,' ACTIVE GRID POINTS IN THE FINAL CYCLE OF')
      IF(NEWIND .EQ. 1) WRITE(*,3259) NEWIND
3259  FORMAT(' THE RUN, ',I7,' IS ACTIVE FOR THIS SUBJECT.'/)
      IF(NEWIND .GT. 1) WRITE(*,3261) NEWIND 
3261  FORMAT(' THE RUN, ',I7,' ARE ACTIVE FOR THIS SUBJECT.'/)



	 WRITE(25,8)
	 WRITE(25,*) SLPYJ
	 WRITE(25,*)


      WRITE(25,3258) NACTVEFULL
      IF(NEWIND .EQ. 1) WRITE(25,3259) NEWIND
      IF(NEWIND .GT. 1) WRITE(25,3261) NEWIND 

C   NEW CODE FOR bigmlt10.f - ABOVE
 
	
	FACT=VOLSPA/NGRID
	SUM=0.D0
 
	DO I=1,NACTVE
 
	PI=CORDEN(I,NVAR+1)*FACT

	IF (PI .GT. 1.D-200) THEN
	  SUM=SUM+PI*DLOG(PI)
	ENDIF
 
	END DO
 
	DL2=DLOG(2.D0)
	SUM = SUM/DL2
	ENT = -SUM
 
        DGRID=NGRID
	FACT=100.D0*DL2/DLOG(DGRID/NSUB)
	SUM = FACT*(SUM + DLOG(DGRID)/DL2)
 

C  REPLACE WRITING OF SUM AND ENT WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL.

      XVERIFY(1) = SUM
      XVERIFY(2) = ENT
      CALL VERIFYVAL(2,XVERIFY)



C     WRITE(*,31) SUM
	WRITE(*,31) XVERIFY(1)


C	WRITE(*,131) ENT
	WRITE(*,131) XVERIFY(2)


C	WRITE(25,31) SUM
	WRITE(25,31) XVERIFY(1)


C	WRITE(25,131) ENT
	WRITE(25,131) XVERIFY(2)


   31 FORMAT(/' THE SCALED INFO FOR THIS DENSITY IS ',F10.2,' %'/)
  131 FORMAT(/' THE ENTROPY FOR THIS DENSITY IS ',G11.4/)



	DO 1100 I=1,NVAR
 
	DO IG=1,NACTVE
	  WORK(IG)=CORDEN(IG,I)*CORDEN(IG,NVAR+1)
	END DO
 
	II=I
        CALL NOTINT(VOLSPA,NGRID,NACTVE,WORK,MAXGRD,EX(II))
 
	DO 1100 J=1,I
 
	DO IG=1,NACTVE
	  WORK(IG)=CORDEN(IG,I)*CORDEN(IG,J)*CORDEN(IG,NVAR+1)
	END DO
 
	JJ=J
 1100   CALL NOTINT(VOLSPA,NGRID,NACTVE,WORK,MAXGRD,E(II,JJ))
 
 
      DO 190 I=1,NVAR
      DO 190 J=1,I
  190 COV(I,J)=E(I,J)-EX(I)*EX(J)

 
 3030	WRITE(*,*)' THE MEANS ARE: '
	WRITE(25,*)' THE MEANS ARE: '
	WRITE(*,5104) (PAR(I),I=1,NVAR)
        WRITE(25,5104) (PAR(I),I=1,NVAR)
 5104   FORMAT(5X,30(A11,2X))
 

      DO I = 1,NVAR
       XVERIFY(I) = EX(I)
      END DO

      CALL VERIFYVAL(NVAR,XVERIFY)      



C     WRITE(*,5103) (EX(I),I=1,NVAR)
      WRITE(*,5103) (XVERIFY(I),I=1,NVAR)
C     WRITE(25,5103) (EX(I),I=1,NVAR)
      WRITE(25,5103) (XVERIFY(I),I=1,NVAR)


 5103   FORMAT(1X,30(G12.6,1X))


C  SEE COMMENTS IN MAIN ABOUT ICOVL0.
 
      ICOVL0=0

      DO I=1,NVAR
       IF(COV(I,I) .LE. 0.D0) ICOVL0=1
      END DO

      IF(ICOVL0 .EQ. 1) THEN
       DO I = 1,NVAR
        DO J = 1,NVAR
         COV(I,J) = 0.D0
        END DO
       END DO
      ENDIF
 

      WRITE(*,13)
	WRITE(25,13)

   13 FORMAT(/' THE COV MATRIX IS, IN LOWER TRI FORM:')
 
	  WRITE(*,5104) (PAR(I),I=1,NVAR)
        WRITE(25,5104) (PAR(I),I=1,NVAR)
 

      DO 200 I = 1,NVAR

       DO J = 1,I
        XVERIFY(J) = COV(I,J)
       END DO
       CALL VERIFYVAL(I,XVERIFY)

C       WRITE(25,5103) (COV(I,J),J=1,I)
        WRITE(25,5103) (XVERIFY(J),J=1,I)

C 200   WRITE(*,5103) (COV(I,J),J=1,I)
  200   WRITE(*,5103) (XVERIFY(J),J=1,I)	


      DO I = 1,NVAR
       STD(I)=DSQRT(COV(I,I))
       COFVR(I)=STD(I)*1.D2/EX(I)
        DO J = 1,I
         IF(ICOVL0 .EQ. 0) CORR(I,J) = COV(I,J)/STD(I)/STD(J)
         IF(ICOVL0 .EQ. 1) CORR(I,J) = -99.D0
        END DO
      END DO

 
 3040	WRITE(*,6071)
	WRITE(25,6071)
 6071 FORMAT(/' THE STANDARD DEVIATIONS ARE, RESPECTIVELY: ')

 
      WRITE(*,5104) (PAR(I),I=1,NVAR)
      WRITE(25,5104) (PAR(I),I=1,NVAR)
 
      DO I = 1,NVAR
       XVERIFY(I) = STD(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)      



C     WRITE(*,5103) (STD(I),I=1,NVAR)
      WRITE(*,5103) (XVERIFY(I),I=1,NVAR)
C     WRITE(25,5103) (STD(I),I=1,NVAR)
      WRITE(25,5103) (XVERIFY(I),I=1,NVAR)
 
 
	WRITE(*,6072)
	WRITE(25,6072)


 6072 FORMAT(/' THE PERCENT COEFFICIENTS OF VARIATION ARE, RESP.: ')
 
      WRITE(*,5104) (PAR(I),I=1,NVAR)
      WRITE(25,5104) (PAR(I),I=1,NVAR)
 
      DO I = 1,NVAR
       XVERIFY(I) = COFVR(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)      



C     WRITE(*,5103) (COFVR(I),I=1,NVAR)
      WRITE(*,5103) (XVERIFY(I),I=1,NVAR)


C     WRITE(25,5103) (COFVR(I),I=1,NVAR)
      WRITE(25,5103) (XVERIFY(I),I=1,NVAR)
 
	WRITE(*,6073)
	WRITE(25,6073)
 6073   FORMAT(/' THE CORR. MATRIX IS, IN LOWER TRIANGULAR FORM: ')
 
      WRITE(*,5104) (PAR(I),I=1,NVAR)
      WRITE(25,5104) (PAR(I),I=1,NVAR)
 

      DO 6080 I=1,NVAR

       DO J = 1,I
        XVERIFY(J) = CORR(I,J)
       END DO
       CALL VERIFYVAL(I,XVERIFY)

C      WRITE(25,5103) (CORR(I,J),J=1,I)
       WRITE(25,5103) (XVERIFY(J),J=1,I)

C6080  WRITE(*,5103) (CORR(I,J),J=1,I)
 6080  WRITE(*,5103) (XVERIFY(J),J=1,I)


 
	WRITE(*,6091) NVAR
	WRITE(25,6091) NVAR
 6091   FORMAT(//' THE FOLLOWING ',I2,' SETS OF LINES GIVE ADDITIONAL'/
     1' STATISTICS FOR THE VARIABLES. FOR EACH SET:'//
     2' THE 1ST LINE GIVES THE MODE, THE SKEWNESS, THE KURTOSIS, AND '/
     3' THE 2.5 %-TILE VALUE OF THE DISTRIBUTION. '//
     4' THE 2ND LINE GIVES THE 25, 50, 75, AND 97.5 %-TILE VALUES OF '/
     5' THE DISTRIBUTION. '//
     6' THE 3RD LINE GIVES THREE ADDITIONAL AD-HOC ESTIMATES OF THE '/
     6' STANDARD DEVIATION FOR THAT MARGINAL DENSITY. THE 1ST S.D. '/
     7' ESTIMATE IS THE STANDARD DEVIATION OF A NORMAL DISTRIBUTION '/
     8' HAVING THE SAME [25, 75] %-TILE RANGE AS THAT VARIABLE. THE'/
     9' 2ND ESTIMATE IS THE STANDARD DEVIATION OF A NORMAL DIST.'/
     1' HAVING THE SAME [2.5, 97.5] %-TILE RANGE AS THAT VARIABLE. THE'/
     2' 3RD ESTIMATE IS THE AVERAGE OF THE FIRST TWO. THE 4TH VALUE'/
     3' IN THE LINE IS THE THE % SCALED INFO FOR THAT MARGINAL DENS.'//)
 
 

	DO 6090 I=1,NVAR



	IND=I
 
	CALL STAZ(VOLSPA,NGRID,NACTVE,NVAR,IND,CORDEN,WORK,MAXGRD,NINT,
     1  AB(IND,1),AB(IND,2),XMODE,X025,X25,X50,X75,X975,SCALINFO,NSUB,

     2  MAXDIM)
 
C  STORE THE MEDIANS AND MODES.
 
	CENTER(2,IND)=X50
	CENTER(3,IND)=XMODE
 
	SDEST1 = (X75-X25)/1.349
	SDEST2 = (X975-X025)/3.92
	SDEST3 = .5D0*(SDEST1+SDEST2)
 
	IF(ICOVL0 .EQ. 1) THEN
	  SK = -99999999
	  KU = -99999999
	ENDIF
 
	IF(ICOVL0 .EQ. 0) THEN
 
	DO IG=1,NACTVE
	  XX = CORDEN(IG,IND)
          WORK(IG) = CORDEN(IG,NVAR+1)*(XX-EX(IND))**3
	END DO
 
        CALL NOTINT(VOLSPA,NGRID,NACTVE,WORK,MAXGRD,SK)
	SK=SK/STD(I)**3
 

	DO IG=1,NACTVE
	  XX = CORDEN(IG,IND)
          WORK(IG) = CORDEN(IG,NVAR+1)*(XX-EX(IND))**4
	END DO
 
        CALL NOTINT(VOLSPA,NGRID,NACTVE,WORK,MAXGRD,KU)
	KU=KU/STD(I)**4
 
	ENDIF
 
 
	WRITE(*,6092) PAR(IND)
	WRITE(25,6092) PAR(IND)

 6092   FORMAT(/' ',A11,':')
 
       XVERIFY(1) = XMODE
       XVERIFY(2) = SK
       XVERIFY(3) = KU   
       XVERIFY(4) = X025
       CALL VERIFYVAL(4,XVERIFY)

C      WRITE(*,6093) XMODE,SK,KU,X025
       WRITE(*,6093) (XVERIFY(IXV),IXV=1,4) 


C      WRITE(25,6093) XMODE,SK,KU,X025
       WRITE(25,6093) (XVERIFY(IXV),IXV=1,4) 

       XVERIFY(1) = X25
       XVERIFY(2) = X50
       XVERIFY(3) = X75  
       XVERIFY(4) = X975
       CALL VERIFYVAL(4,XVERIFY)

C      WRITE(*,6093) X25,X50,X75,X975
       WRITE(*,6093) (XVERIFY(IXV),IXV=1,4) 

C      WRITE(25,6093) X25,X50,X75,X975
       WRITE(25,6093) (XVERIFY(IXV),IXV=1,4) 
 
       XVERIFY(1) = SDEST1
       XVERIFY(2) = SDEST2
       XVERIFY(3) = SDEST3
       XVERIFY(4) = SCALINFO
       CALL VERIFYVAL(4,XVERIFY)

C      WRITE(*,6093) SDEST1,SDEST2,SDEST3,SCALINFO
       WRITE(*,6093) (XVERIFY(IXV),IXV=1,4) 

C      WRITE(25,6093) SDEST1,SDEST2,SDEST3,SCALINFO
       WRITE(25,6093) (XVERIFY(IXV),IXV=1,4) 

 6093  FORMAT(1X,4(G15.8,2X))
 
 6090   CONTINUE



      WRITE(*,*)
      WRITE(25,*)
 
C  STORE THE MEANS INTO CENTER(1,.). CENTER(2,.) AND CENTER(3,.)
C  THE MEDIANS AND MODES WERE STORED JUST BELOW THE CALL TO STAT.
 

	 DO I=1,NVAR
	  CENTER(1,I)=EX(I)
	 END DO
 
	RETURN

	END


      subroutine checkd_mv_to_exp_dot_f90(corden,new,nactveold,
     1  ab,maxgrd,nvar,iclose)

      use npag_utils,only:max_pop_rand_varbs


      implicit real*8 (a-h,o-z)
      real*8 ab(max_pop_rand_varbs,2), corden(maxgrd,1)
      iclose=0
      do ibas=1,nactveold
       sum=0.
       do i=1,nvar
       sum=sum+abs(corden(new,i)-corden(ibas,i))/(ab(i,2)-ab(i,1))
       enddo

      if(sum.le.1.d-4) then
      iclose=1
      return
      endif
      enddo

      return
      end

C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE GETIPATF(IFILE,NSUBTOT,NSUB,IPATVEC,IERRR,ERRFIL)

	DIMENSION IPATVEC(9999)
	CHARACTER READLINE*300,ERRFIL*20

C  SUBROUTINE GETIPATF IS CALLED BY MAIN TO OBTAIN THE INDICES OF
C  THE "ACTIVE" SUBJECTS, IPATVEC(I),I=1,NSUB, FOR THIS RUN. THESE
C  INDICES ARE OBTAINED FROM FILE IFILE.

    3   FORMAT(A300)

	NSUBB = 0
	NUMCUR = 0


C  NSUBB WILL BE THE NO. OF SUBJECTS SELECTED BY THE USER. IT IS 
C  INITIALIZED AS 0, AND WILL GROW AS EACH LINE OF THE FILE IS READ.

C  NUMCUR IS THE INDEX OF THE HIGHEST PATIENT NO. ENTERED SO FAR.

C  NOTE THAT EACH LINE CONTAINS EITHER A SUBJECT NO. OR A RANGE OF 
C  SUBJECT NOS. TO BE INCLUDED IN THE ANALYSIS. MULTIPLE SUBJECTS
C  CAN BE ENTERED USING COMMAS, SPACES AND/OR HYPHENS. FOR EXAMPLE,
C  2, 17 - 28 INDICATES SUBJECTS 2 AND 17 THROUGH 28; 
C  17,28 INDICATES SUBJECTS 17 AND 28;
C  17 28 INDICATES SUBJECTS 17 AND 28. 


C  RESTRICTIONS: 1. ON EACH LINE, USE NO MORE THAN 70 CHARACTERS;
C                2. PATIENT NOS., OR RANGES OF PATIENT NOS.

C                   MUST BE LISTED IN ASCENDING ORDER.

C  A LINE WITH JUST A 0 --> END OF THE LIST OF SUBJECT NOS.


 4210	IF(IFILE .EQ. 23) READ(23,3,ERR=4200) READLINE
	IF(IFILE .EQ. 25) READ(25,3,ERR=4200) READLINE

C  CALL SUBROUTINE GETNUMSF TO UPDATE NSUBB AND IPATVEC, WHERE IPATVEC
C  IS THE VECTOR WHICH CONTAINS THE PATIENT NOS. TO BE INCLUDED IN THE 
C  ANALYSIS.

C  NOTE THAT ISTOP RETURNS AS 0 TO INDICATE THE END OF THE LIST OF
C  PATIENT NOS; IT RETURNS AS -1 IF THERE IS A CONFLICT IN THE LIST
C  (SEE RESTRICTION ABOVE) OR A PATIENT NO. LARGER THAN NSUBTOT 
C  (THE MAXIMUM ALLOWABLE NO. OF SUBJECTS) HAS BEEN READ IN; AND IT 

C  RETURNS AS 1 IF THERE ARE MORE LINES TO BE READ IN.

	CALL GETNUMSF(1,READLINE,NSUBB,NSUBTOT,NUMCUR,ISTOP,IPATVEC)
	IF(ISTOP .EQ. -1) GO TO 4200
	IF(ISTOP .EQ. 1) GO TO 4210

C  TO GET TO THIS POINT, ISTOP = 0 --> USER HAS STOPPED ENTERING
C  PATIENT NOS.

C  CHECK THAT NSUB, AS INPUT TO THIS ROUTINE (FROM MAIN) IS THE SAME
C  AS NSUBB, AS OBTAINED FROM THE LIST OF SUBJECTS TO BE INCLUDED IN
C  THE ANALYSIS. IF NOT, TELL USER AND RETURN IERRR = -1 SO THE USER
C  CAN MAKE A CORRECTION.

	IF(NSUB .EQ. NSUBB) THEN
	 IERRR = 0
	 RETURN
	ENDIF

	IF(NSUB .NE. NSUBB) THEN

         WRITE(*,2)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,2) 
        CLOSE(42)

    2   FORMAT(//' THERE WAS AN ERROR IN THE READING OF PATIENT NOS.'/
     1' TO BE USED FOR THIS ANALYSIS. IN PARTICULAR, THE NO. OF '/
     2' SUBJECTS TO BE INCLUDED IN THE ANALYSIS, AS ENTERED IN THE'/
     3' INSTRUCTION FILE, DOES NOT MATCH THE LIST OF SUBJECT NOS.'/
     4' WHICH FOLLOW THAT NUMBER. PLEASE RERUN THE PC PREP PROGRAM'/
     2' TO MAKE SURE THESE PATIENT NOS. ARE WRITTEN CORRECTLY INTO'/
     3' THE INSTRUCTION FILE. ')
	IERRR = -1
	 RETURN
	ENDIF

 4200   WRITE(*,1)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,1)
        CLOSE(42)

    1   FORMAT(//' THERE WAS AN ERROR IN THE READING OF PATIENT NOS.'/
     1' TO BE USED FOR THIS ANALYSIS. PLEASE RERUN THE PC PREP PROGRAM'/
     2' TO MAKE SURE THESE PATIENT NOS. ARE WRITTEN CORRECTLY INTO'/
     3' THE INSTRUCTION FILE. ')
	IERRR = -1


	RETURN
	END
C

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C
	SUBROUTINE GETNUMSF(IINCLUDE,READLINE,NSUBB,NSUBTOT,NUMCUR,
     1    ISTOP,IPATVECC)

	DIMENSION IPATVECC(9999)
	CHARACTER READLINE*300

C  SUBROUTINE GETNUMSF IS CALLED BY GETIPATF (OVER AND OVER) 
C  TO ESTABLISH NSUBB AND IPATVECC, WHERE NSUBB IS THE NO. OF SUBJECTS 
C  TO BE INCLUDED (IF IINCLUDE = 1) OR EXCLUDED (IF IINCLUDE = 2) IN THE 

C  ANALYSIS, AND IPATVECC IS THE VECTOR WHICH CONTAINS THE CORRESPONDING 
C  PATIENT NOS. EACH CALL TO GETNUMSF EITHER ADDS TO THE VALUES IN 
C  IPATVECC (AND INCREASES NSUBB) OR STOPS THE PROCESS.

C  NOTE THAT GETNUMSF IS THE SAME AS GETNUMS EXCEPT THAT THE COMMENTS
C  TO THE USER ARE DIFFERENT SINCE THIS ROUTINE IS CALLED BY GETIPATF
C  WHICH IS READING A FILE, RATHER THAN KEYBOARD ENTRIES.

C  NOTE THAT ISTOP RETURNS AS 0 IF THE USER IS FINISHED ENTERING 
C  PATIENT NOS; IT RETURNS AS -1 IF THE USER HAS ENTERED A CONFLICTING 
C  SET OF PATIENT NOS. (EACH PATIENT NO. OR RANGE OF NUMBERS MUST BE
C  GREATER THAN ANY PATIENT NO. ALREADY ENTERED ... THE LARGEST PATIENT 
C  NO. ENTERED SO FAR IS NUMCUR) OR A PATIENT NO. LARGER THAN NSUBTOT 
C  (THE MAXIMUM ALLOWABLE NO. OF SUBJECTS); AND IT RETURNS AS 1 IF THE 
C  USER HAS CORRECTLY ENTERED PATIENT DATA AND WANTS TO CONTINUE 
C  ENTERING OTHER NOS.

	ISTOP = 1

C  CHECK TO SEE IF THIS IS A BLANK LINE. IF SO, RETURN ISTOP = -1 
C  WITH A MESSAGE TO THE USER.

	DO J = 1,70
	 IF(READLINE(J:J) .NE. ' ') GO TO 10
	END DO

C  TO GET HERE --> READLINE IS COMPLETELY BLANK.

	IF(NSUBB .EQ. 0) WRITE(*,1)
    1   FORMAT(/' THE INSTRUCTION FILE HAS AN IMPROPER BLANK LINE IN '/


     1' THE PATIENT NUMBER SECTION. ')

	ISTOP = -1
	RETURN
	  


   10   CONTINUE

C  CHECK TO SEE IF THIS LINE HAS JUST A 0 ON IT. IF SO, RETURN ISTOP
C  = 0, UNLESS NSUBB = 0 AND IINCLUDE = 1, IN WHICH CASE THIS IS AN 
C  ERROR. SIMPLY CHECK THE FIRST NON-BLANK CHARACTER (THERE MUST BE ONE 
C  SINCE IF THE LINE IS COMPLETELY BLANK, THE CODE ABOVE ABOVE WOULD 
C  HAVE DETECTED IT AND RETURNED CONTROL TO THE CALLING ROUTINE) AND 
C  SEE IF a. IT IS A 0, AND b. EVERY OTHER CHARACTER IS A BLANK. IF
C  a. AND b. ARE TRUE, THE LINE JUST HAS A 0 IN IT. OTHERWISE NOT.

	DO J = 1,70
	 IF(READLINE(J:J) .NE. ' ') GO TO 20
	END DO

   20   ISTART = J
	IF(READLINE(ISTART:ISTART) .NE. '0') GO TO 30

	DO I = ISTART+1,70
	 IF(READLINE(I:I) .NE. ' ') GO TO 30
	END DO
	
C  TO GET HERE --> READLINE HAS JUST A 0 ON IT AND NOTHING ELSE.

C  IF THE USER HAS ENTERED A 0 WITH NSUBB = 0 (I.E., HE HAS NOT 
C  ENTERED PREVIOUS SUBJECT NOS.), THIS IS OK IF IINCLUDE = 2 (SINCE
C  IT IS OK TO EXCLUDE 0 PATIENTS), BUT IT IS NOT OK IF IINCLUDE = 1
C  (SINCE THIS WOULD MEAN THE ANALYSIS WOULD BE ON 0 SUBJECTS).

	IF(IINCLUDE .EQ. 1 .AND. NSUBB .EQ. 0) THEN
	 WRITE(*,3)
    3    FORMAT(/' THE INSTRUCTION FILE HAS AN IMPROPER LINE - WITH'/
     1' JUST A "0" ON IT - IN THE PATIENT NUMBER SECTION.')
	 ISTOP = -1
	 RETURN
	ENDIF


	IF(IINCLUDE .EQ. 2 .OR. NSUBB .GT. 0) THEN
	 ISTOP = 0
	 RETURN
	ENDIF


   30   CONTINUE


C  TO GET TO THIS POINT MEANS READLINE HAS A POTENTIAL NO. BEGINNING
C  AT ENTRY ISTART. IF THIS IS A LEGITIMATE NO., IT ENDS THE ENTRY
C  BEFORE THE NEXT SPACE, COMMA, DASH, WHICHEVER COMES FIRST.

     	DO I = ISTART+1,70
	 IF(READLINE(I:I) .EQ. ' ' .OR. READLINE(I:I) .EQ. ',' .OR.
     1      READLINE(I:I) .EQ. '-') GO TO 40
	END DO


   40   IEND = I-1

C  CALL ROUTINE GETSUB WHICH OBTAINS THE SUBJECT NO. FROM THE 
C  CHARACTERS IN READLINE(ISTART:IEND).

	CALL GETSUB(READLINE,ISTART,IEND,ISUB,IERROR)

C  IF IERROR RETURNS AS -1, IT MEANS THE USER HAS ENTERED A NON-NUMERIC
C  CHARACTER. IN THIS CASE, PRINT AN ERROR MESSAGE AND RETURN.


	IF(IERROR .EQ. -1) THEN
	 WRITE(*,7) 
    7    FORMAT(/' THE INSTRUCTION FILE HAS AN IMPROPER LINE - WITH'/

     1' AN INVALID CHARACTER ON IT - IN THE PATIENT NUMBER SECTION.')
	 ISTOP = -1
	 RETURN
	ENDIF

C  IF ISUB IS .LE. NUMCUR, THE LAST (AND HIGHEST PATIENT NO. ENTERED
C  PREVIOUSLY), WRITE AN ERROR MESSAGE TO THE USER AND RETURN. 
C  SIMILARLY, IF ISUB .GE. NSUBTOT.
    
	IF(ISUB .LE. NUMCUR) THEN
	 WRITE(*,4) ISUB,NUMCUR
    4    FORMAT(/' THE INSTRUCTION FILE HAS AN IMPROPER LINE IN IT'/
     1' IN THE PATIENT NUMBER SECTION.'//
     2' IT HAS A SUBJECT NO. (',I4,' ) WHICH IS LESS THAN OR EQUAL TO '/
     3' A PREVIOUSLY ENTERED SUBJECT NO. (',I4,').')
	 ISTOP = -1

	 RETURN
	ENDIF

	IF(ISUB .GT. NSUBTOT) THEN
	 WRITE(*,6) ISUB,NSUBTOT
    6    FORMAT(/' THE INSTRUCTION FILE HAS AN IMPROPER LINE IN IT'/
     1' IN THE PATIENT NUMBER SECTION.'//
     2' IT HAS A SUBJECT NO. (',I4,' ) WHICH IS GREATER THAN THE NO.'/
     3' OF SUBJECTS IN YOUR DATA FILE (',I4,').')
	 ISTOP = -1
	 RETURN
	ENDIF

C  TO GET TO THIS POINT, THE PATIENT NO., ISUB, IS LEGITIMATE. I.E.,
C  IT IS .GT. NUMCUR AND .LE. NSUBTOT. THERE ARE 4 POSSIBILITIES:

C  1. IF THERE ARE NO OTHER ENTRIES IN READLINE PAST THIS POINT,
C  ISUB IS A SINGLE PATIENT NO.
C  2. IF THE NEXT NON-BLANK ENTRY IN READLINE IS A COMMA OR ANOTHER
C  NUMBER, ISUB IS A SINGLE PATIENT NO.
C  3. IF THE NEXT NON-BLANK ENTRY IN READLINE IS A DASH, ISUB IS THE
C  START OF A PATIENT RANGE.
C  4. IF THE NEXT NON-BLANK ENTRY IN READLINE IS ANOTHER CHARACTER
C  (I.E., NOT A NUMBER, DASH, OR COMMA), THE LINE HAS BEEN ENTERED
C  INCORRECTLY BY THE USER.



C  CHECK FOR NO. 1. ABOVE ...

	DO I = IEND+1,70

	 IF(READLINE(I:I) .NE. ' ') GO TO 50
	END DO

C  TO GET TO THIS POINT, ISUB IS AN INDIVIDUAL SUBJECT NO., AND THE
C  LAST NO. ON THE LINE. INCREASE NSUBB, NUMCUR, AND PUT THIS SUBJECT
C  NO. INTO IPATVECC BEFORE RETURNING.

	NUMCUR = ISUB
	NSUBB = NSUBB + 1
	IPATVECC(NSUBB) = ISUB
	RETURN


   50   CONTINUE

C  CHECK FOR NOS. 2,3, OR 4 (SEE ABOVE).

C  TO GET TO THIS POINT, THERE IS AT LEAST ONE CHARACTER ENTRY AFTER 

C  ISUB, AND THE FIRST OCCURS AT LOCATION I. IF THIS CHARACTER IS A

C  COMMA THEN ISUB IS AN INDIVIDUAL SUBJECT NO. 

	IF(READLINE(I:I) .EQ. ',') THEN

	 NUMCUR = ISUB
 	 NSUBB = NSUBB + 1

	 IPATVECC(NSUBB) = ISUB

C  CHECK TO SEE IF THERE IS ANOTHER ENTRY ON THIS LINE. IF SO,
C  RETURN CONTROL TO LABEL 30 AND CONTINUE CHECKING FOR OTHER NOS. IF 
C  NOT, IT MEANS THE USER HAS ENDED THE LINE WITH A COMMA ... WHICH 
C  IS ASSUMED TO BE SUPERFLOUS.

	 DO J = I+1,70
	  IF(READLINE(J:J) .NE. ' ') GO TO 60
	 END DO



	 RETURN

   60    ISTART = J
	 GO TO 30	

	ENDIF



C  TO GET TO THIS POINT, THERE IS AT LEAST ONE CHARACTER ENTRY AFTER 
C  ISUB, AND THE FIRST OCCURS AT LOCATION I ... AND THIS ENTRY IS NOT
C  A COMMA. CHECK TO SEE IF IT IS ANOTHER NUMBER. IN THIS CASE, ISUB
C  IS AN INDIVIDUAL SUBJECT NO. 

	INUM = 0
	IF(READLINE(I:I) .EQ. '0') INUM = 1
	IF(READLINE(I:I) .EQ. '1') INUM = 1
	IF(READLINE(I:I) .EQ. '2') INUM = 1
	IF(READLINE(I:I) .EQ. '3') INUM = 1
	IF(READLINE(I:I) .EQ. '4') INUM = 1
	IF(READLINE(I:I) .EQ. '5') INUM = 1
	IF(READLINE(I:I) .EQ. '6') INUM = 1
	IF(READLINE(I:I) .EQ. '7') INUM = 1
	IF(READLINE(I:I) .EQ. '8') INUM = 1
	IF(READLINE(I:I) .EQ. '9') INUM = 1

	IF(INUM .EQ. 1) THEN
	 NUMCUR = ISUB
 	 NSUBB = NSUBB + 1
	 IPATVECC(NSUBB) = ISUB

C  RETURN CONTROL TO LABEL 30 AND CONTINUE CHECKING FOR OTHER NOS.
C  STARTING WITH LOCATION I.

         ISTART = I
	 GO TO 30	

	ENDIF


C  TO GET TO THIS POINT, THERE IS AT LEAST ONE CHARACTER ENTRY AFTER 
C  ISUB, AND THE FIRST OCCURS AT LOCATION I ... AND THIS ENTRY IS NOT
C  A COMMA OR A NUMBER. CHECK TO SEE IF IT IS A DASH. IN THIS CASE,
C  ISUB IS THE FIRST NO. IN A RANGE OF PATIENT NUMBERS.


	IF(READLINE(I:I) .EQ. '-') THEN

C  STORE ISUB INTO NUMCUR1, BUT NOT NUMCUR. IN CASE THE USER

C  HAS NOT ENTERED A LEGITIMATE NO. AFTER THE DASH, KEEP THE PREVIOUS
C  VALUE OF NUMCUR (THE LAST PATIENT INDEX PUT INTO IPATVECC) INTACT.

	 NUMCUR1 = ISUB

C  READ UNTIL THE NEXT NON-BLANK CHARACTER, WHICH SHOULD BE THE 
C  BEGINNING OF THE NUMBER WHICH ENDS THE RANGE.


	 DO J = I+1,70
	  IF(READLINE(J:J) .NE. ' ') GO TO 70

	 END DO


C  TO GET TO THIS POINT MEANS THE USER ENDED A LINE WITH A DASH, WHICH
C  IS NOT ALLOWED. WRITE AN ERROR MESSAGE AND RETURN.

	 WRITE(*,8) 

    8    FORMAT(/' THE INSTRUCTION FILE HAS AN IMPROPER LINE IN IT'/
     1' IN THE PATIENT NUMBER SECTION.'//
     2' A LINE HAS BEEN ENDED WITH A DASH.')
	 ISTOP = -1
	 RETURN

   70   ISTART = J

C  TO GET TO THIS POINT, THERE IS AN ENTRY IN LOCATION ISTART, WHICH
C  SHOULD BE THE BEGINNING OF THE ENDING PATIENT NO. OF A RANGE OF
C  PATIENT NOS. THIS NUMBER ENDS THE ENTRY BEFORE A SPACE OR A 
C  COMMA.


     	DO K = ISTART+1,70
	 IF(READLINE(K:K) .EQ. ' ' .OR. READLINE(K:K) .EQ. ',') 
     1    GO TO 80
	END DO

   80   IEND = K-1

C  CALL ROUTINE GETSUB WHICH OBTAINS THE SUBJECT NO. FROM THE 
C  CHARACTERS IN READLINE(ISTART:IEND).


	CALL GETSUB(READLINE,ISTART,IEND,ISUB,IERROR)



C  IF IERROR RETURNS AS -1, IT MEANS THE USER HAS ENTERED A NON-NUMERIC

C  CHARACTER. IN THIS CASE, PRINT AN ERROR MESSAGE AND RETURN.

	IF(IERROR .EQ. -1) THEN
	 WRITE(*,7) 
	 ISTOP = -1
	 RETURN
	ENDIF

C  IF ISUB IS .LE. NUMCUR1 (THE BEGINNING NO. IN THIS RANGE), WRITE AN 
C  ERROR MESSAGE TO THE USER AND RETURN. SIMILARLY, IF ISUB .GE. 
C  NSUBTOT
    
	IF(ISUB .LE. NUMCUR1) THEN
	 WRITE(*,9)
    9    FORMAT(/' THE INSTRUCTION FILE HAS AN IMPROPER LINE IN IT'/
     1' IN THE PATIENT NUMBER SECTION.'//
     2' IT HAS A RANGE OF SUBJECT NOS. WITH THE ENDING NO. LESS THAN '/
     3' OR EQUAL TO THE BEGINNING NO.')
	 ISTOP = -1
	 RETURN
	ENDIF


	IF(ISUB .GT. NSUBTOT) THEN
	 WRITE(*,6) ISUB,NSUBTOT
	 ISTOP = -1

	 RETURN
	ENDIF



C  TO GET TO THIS POINT MEANS THE USER HAS CORRECTLY ENTERED A RANGE
C  OF PATIENT NOS. FROM NUMCUR1 TO ISUB. UPDATE NSUBB, NUMCUR, AND
C  IPATVECC.

	 NUMCUR = ISUB
	 NN = NSUBB
 	 NSUBB = NSUBB + (NUMCUR - NUMCUR1) + 1

	 NONEW = 0
	 DO K = NN+1,NSUBB
	  NONEW = NONEW + 1
	  IPATVECC(K) = NUMCUR1 - 1 + NONEW
	 END DO

C  CHECK TO SEE IF THERE IS ANOTHER CHARACTER ON THE LINE AFTER
C  LOCATION IEND (IGNORE A COMMA AT THIS POINT, SINCE IT IS POSSIBLE
C  THAT THE USER HAS PUT IN A COMMA AT THE END OF HIS SUBJECT RANGE). 
C  IF SO, RETURN CONTROL TO LABEL 30 AND CONTINUE CHECKING FOR OTHER 
C  NOS. IF NOT, RETURN.

	 DO J = IEND+1,70
	  IF(READLINE(J:J) .NE. ' ' .AND. READLINE(J:J) .NE. ',' ) 
     1    GO TO 90
	 END DO

	 RETURN

   90    ISTART = J
	 GO TO 30	

	ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(READLINE(I:I) .EQ. '-')  CONDITION.


C  TO GET TO THIS POINT, THERE IS AT LEAST ONE CHARACTER ENTRY AFTER 
C  ISUB, AND THE FIRST OCCURS AT LOCATION I ... AND THIS ENTRY IS NOT
C  A COMMA, A NUMBER, OR A DASH, WHICH MEANS IT IS AN ERRONEOUS ENTRY.
C  WRITE AN ERROR MESSAGE TO THE USER AND RETURN.

	WRITE(*,7) 
	ISTOP = -1



	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE WRITEPT2(IFILE,NSUB,IPATVEC)
	DIMENSION IPATVEC(9999)

C  THIS ROUTINE WRITES THE PATIENT NOS. TO FILE IFILE.

C  INSTEAD OF WRITING OUT THE PATIENT NOS. ONE TO A LINE, WRITE OUT
C  CONTINUOUS SETS ON EACH LINE. E.G., IF SUBJECTS 1,4,5,6,9,10,12 ARE
C  TO BE WRITTEN OUT, WRITE 1 ON THE 1ST LINE, 4 - 6 ON THE 2ND LINE,

C  9 - 10 OR THE 3RD LINE, AND 12 ON THE 4TH.

C  NEXTIND IS THE INDEX OF THE NEXT SUBJECT NO. TO BE WRITTEN.

	NEXTIND = 0

   50   NEXTIND = NEXTIND + 1

C  IF NEXTIND .GT. NSUB, THE NOS. HAVE ALL BEEN WRITTEN OUT, SO STOP 
C  THE WRITING.

	IF(NEXTIND .GT. NSUB) GO TO 100

C  ESTABLISH THE NOS. TO BE WRITTEN ON THE NEXT LINE. THE FIRST NO.
C  WILL BE IPATVEC(NEXTIND)

	IFIRST = IPATVEC(NEXTIND)

C  IF NEXTIND = NSUB, THIS IS THE LAST PATIENT NO. TO BE WRITTEN OUT.

	IF(NEXTIND .EQ. NSUB) THEN
	 IF(IFILE .EQ. 25) WRITE(25,222) IFIRST
	 IF(IFILE .EQ. 29) WRITE(29,222) IFIRST

  222    FORMAT(1X,I5)
	 GO TO 100	
	ENDIF


C  IF THE NEXT PATIENT NO. IN IPATVEC = IFIRST + 1, THEN IFIRST IS THE
C  FIRST OF A STRING OF CONSECUTIVE NUMBERS (FIND THE LAST NO. IN THIS 
C  STRING AND WRITE THE STRING OUT). OTHERWISE, IFIRST WILL BE WRITTEN 
C  OUT BY ITSELF.

	IF(IPATVEC(NEXTIND+1) .NE. IFIRST + 1) THEN
	 IF(IFILE .EQ. 25) WRITE(25,222) IFIRST
	 IF(IFILE .EQ. 29) WRITE(29,222) IFIRST
	 GO TO 50
	ENDIF

C  SET ILAST = THE LAST NO. IN THE STRING USING THE FOLLOWING DO LOOP.

	ILAST = IPATVEC(NEXTIND+1)
	NEXT = NEXTIND+1


	DO I = NEXTIND+2,NSUB
	 IF(IPATVEC(I) .NE. ILAST + 1) GO TO 80
	 ILAST = IPATVEC(I)
	 NEXT = I
	END DO



   80	IF(IFILE .EQ. 25) WRITE(25,221) IFIRST,ILAST
   	IF(IFILE .EQ. 29) WRITE(29,221) IFIRST,ILAST
  221   FORMAT(1X,I5,'   - ',I5)


C  THE INDEX OF THE LAST NO. WRITTEN OUT IS NEXT.
	

	NEXTIND = NEXT

	GO TO 50

  100   RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE GETSUB(READLINE,ISTART,IEND,ISUB,IERROR)

	CHARACTER READLINE*300

C  THIS ROUTINE, CALLED BY GETNUMSF, OBTAINS THE SUBJECT NO., ISUB, 
C  FROM THE CHARACTERS IN READLINE(ISTART:IEND).

    3   FORMAT(A300)

	IERROR = 0
  	ISIZE = IEND-ISTART

	ISUB = 0
	 DO K=ISTART,IEND
	  IVAL = -9

	  IF(READLINE(K:K) .EQ. '0') IVAL = 0
	  IF(READLINE(K:K) .EQ. '1') IVAL = 1
	  IF(READLINE(K:K) .EQ. '2') IVAL = 2
	  IF(READLINE(K:K) .EQ. '3') IVAL = 3
	  IF(READLINE(K:K) .EQ. '4') IVAL = 4
	  IF(READLINE(K:K) .EQ. '5') IVAL = 5
	  IF(READLINE(K:K) .EQ. '6') IVAL = 6
	  IF(READLINE(K:K) .EQ. '7') IVAL = 7
	  IF(READLINE(K:K) .EQ. '8') IVAL = 8
	  IF(READLINE(K:K) .EQ. '9') IVAL = 9

	  IF(IVAL .EQ. -9) THEN
	   IERROR = -1
	   RETURN
	  ENDIF

	  ISUB = ISUB + IVAL*10**ISIZE
	  ISIZE = ISIZE-1

	 END DO

	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE CALCTPRED(JSUB,IDELTA,NOBSER,NUMTSUB,TPRED,TPREDREL,
     1   NOMAXTIMS,TEND,TBEGG,TIM,SIG,INTLIST)

       use npag_utils, only: maxnumeq,max_m_per_obs,max_doses,
     1   max_RS_J,max_input_dim

      IMPLICIT REAL*8(A-H,O-Z)

C      PARAMETER(MAXNUMEQ=7)

C  NOTE THAT AS OF CALCTPRED, MAXNUMEQ IS SET IN A PARAMETER STMT.
C  HERE SO THAT YOO CAN BE VARIABLY DIMENSIONED BELOW.

      DIMENSION TPRED(71281),TEND(99),TIM(max_m_per_obs),
     1  SIG(max_doses), TBEGG(99),TPREDREL(71281)

C     2  RS(max_doses,max_RS_J),BS(max_doses,max_input_dim),
C     3  YOO(max_m_per_obs,MAXNUMEQ)

      integer, dimension(128) :: intlist

C        COMMON/OBSER/ TIM,SIG,RS,YOO,BS ! RS, BS, YOO not used
C        COMMON/CNST/ N,ND,NI,NUP,NUIC,NP

C !$omp Threadprivate(/OBSER/)
C ,/CNST/)

C  AS OF npageng16.f, COMMON/OBSER AND COMMON/CNST ARE INCLUDED IN
C  THIS ROUTINE SINCE ND AND SIG ARE NEEDED. NOTE THAT TIM IS NOW
C  ALSO PROVIDED VIA COMMON/OBSER, RATHER THAN AS THE ARGUMENT,
C  TIMOBB.


C  THIS ROUTINE IS CALLED BY MAIN TO CALCULATE THE NUMTSUB TIMES TO BE 
C  PUT INTO TPRED, FROM THE NOBSER TIMES IN TIM. THESE NOBSER TIMES
C  IN TIM ARE THE OBSERVED VALUE TIMES FROM A SUBJECT. THE TIMES
C  IN TPRED ARE TO START AT 0, AND CONTINUE UNTIL 24 HOURS AFTER THE
C  LAST TIME BEFORE EACH TIME RESET (EACH TIME OF 0 AFTER THE FIRST 
C  TIME IS A TIME RESET).

C  AS OF npageng16.f, FOR EACH STEADY STATE DOSE SET, TIMES IN TPRED
C  WILL START AT THE END OF IT, RATHER THAN AT 0 ... EXCEPT IF THE
C  STEADY STATE DOSE OCCURS AFTER THE BEGINNING OF THE SUBJECT FILE,
C  ONE TPRED TIME WILL BE SET = 0 SO THE PROGRAM WILL RECOGNIZE THAT
C  THIS IS ALSO THE BEGINNING OF A TIME RESET.

C  FOR EXAMPLE, IF THERE IS A STEADY STATE DOSE SET WITH INTERDOSE
C  INTERVAL = DOSEINT, THEN THE TIMES WILL START AT 100*DOSEINT
C  (SINCE EACH STEADY STATE DOSE SET IS ASSUMED TO HAVE 100 DOSES PLUS
C  ONE ADDITIONAL STARTING DOSE).
C  AS A SPECIFIC EXAMPLE, IF A STEADY STATE DOSE SET WITH INTERDOSE
C  INTERVAL OF 2 HOURS STARTS AT TIME 0 AND THE 1ST OBS. TIME IS AT
C  205, IT REALLY MEANS THAT THE OBSERVATIONS START 5 HOURS AFTER THE
C  END OF THE STEADY STATE DOSE SET. IN THIS CASE, THE TIMES IN TPRED
C  STILL START AT 200. 

C  BUT, AS OF npageng18.f, ALSO ESTABLISH TPREDREL(.) WHICH IS SIMILAR
C  TO TPRED, BUT HAS "RELATIVE" INSTEAD OF "REAL" TIMES AFTER STEADY
C  STATE DOSES. IN THE EXAMPLE ABOVE WITH AN INTERDOSE INTERVAL OF 2 
C  HOURS, THE TPRED(.) VALUES START AT 200 AND THE TPREDREL(.) VALUES
C  START AT 0. AND EACH TPREDREL(I) = TPRED(I) - 200.


C  SET IDOSE = 1; IT WILL BE THE RUNNING DOSE NUMBER. IT MUST BE
C  CHECKED TO SEE WHERE, IF AT ALL, THERE ARE STEADY STATE DOSE SETS.

      IDOSE = 1


C  NOTE THAT THE TIMES ARE TO BE IDELTA MINUTES APART, SUBJECT TO
C  THE CONSTRAINT THAT THE MAXIMUM NO. OF TIMES BE 7200.
C  BUT NOTE BELOW THAT THE TIMES IN TPRED ARE IN HOURS, NOT MINUTES.

C  NOMAXTIMS = NO. OF MAXIMUM TIMES FOR THIS SUBJECT (WHICH EQUALS 
C  1 MORE THAN THE NO. OF TIME RESETS).
C  NUMTSUB WILL BE THE RUNNING NO. OF TIMES ALREADY PUT INTO TPRED.
C  INDEX IS THE RUNNING INDEX OF TIMES ALREADY CONSIDERED IN TIM.
C  TIMMAX IS THE CURRENT MAXIMUM TIME THROUGH TIM(INDEX) SINCE
C  THE LAST TIME RESET VALUE OF 0.

      NOMAXTIMS = 0
      NUMTSUB = 0
      INDEX = 0

   50 TIMMAX = -1.D30

   10 INDEX = INDEX + 1

        IF(TIM(INDEX) .GT. TIMMAX) TIMMAX = TIM(INDEX)
        IF(TIM(INDEX) .LE. 0.D0 .AND. INDEX .GT. 1) GO TO 20

C  ADDED ADDITIONAL REQUIREMENT ABOVE, 'INDEX .GT. 1' IN bigmlt8.f.
C  REASON IS THAT, OTHERWISE, AN INITIAL OBS. TIME OF 0 WILL LOOK
C  LIKE A TIME RESET TO THE PROGRAM AND CAUSE AN EXTRA AUC TABLE IN
C  THE OUTPUT FILE.


        IF(INDEX .EQ. NOBSER) GO TO 20

        GO TO 10

   20   CONTINUE 

C  TO GET HERE MEANS TIMMAX IS THE MAXIMUM OBSERVATION TIME BEFORE A
C  TIME RESET, OR IT IS SIMPLY THE MAXIMUM OBSERVATION TIME IF THERE
C  ARE NO TIME RESETS. ESTABLISH THE APPROPRIATE TIMES IN TPRED FROM 0 
C  TO T_END = TIMMAX + 24. SIMILARLY ESTABLISH THE TIMES IN TPREDREL.


C  !!! AS OF npageng16.f, TPRED VALUES WILL START AT 0 UNLESS THE
C  CORRESPONDING DOSES FOR THIS TIME BLOCK START WITH A STEADY STATE
C  SET. IN THAT CASE, THE TPRED VALUES WILL START FROM THE END OF THE
C  STEADY STATE SET. THE STARTING DOSE TIME IS IN SIG(IDOSE). IF

C  THIS VALUE IS < 0, IT REPRESENTS THE START OF A STEADY STATE DOSE
C  SET, WITH INTERDOSE INTERVAL = -SIG(IDOSE). IF THIS VALUE IS .GE. 0,
C  IT DOES NOT REPRESENT THE BEGINNING OF A STEADY STATE DOSE SET.

C  NOTE THAT, AS INDICATED ABOVE, THE TPREDREL VALUES WILL ALWAYS START
C  AT 0.
  
      TBEG = 0.D0
      IF(SIG(IDOSE) .LT. 0.D0) TBEG = 100.D0*(-SIG(IDOSE))


C  NOTE THAT NUMTSUB TIMES HAVE ALREADY BEEN STORED IN TPRED. PUT IN
C  THE NEXT SET.


	T_END = TIMMAX + 24.D0

c wmy2017Oct10 - included explicit conversion with int()
	NUMT2 = int((T_END - TBEG)*60/IDELTA)

	NUMTSUB = NUMTSUB + 1

C  IF TBEG > 0, IT MEANS THERE IS A STEADY STATE DOSE SET OCCURRING.
C  IF THIS IS NOT AT THE BEGINNING OF THE SUBJECT FILE (I.E., IF 
C  NUMTSUB > 1), THEN ONE PREDICTED VALUE MUST BE SET = 0 (SO
C  SUBROUTINE FUNC3 IN idm3x_.7 WILL KNOW THIS IS ALSO THE BEGINNING OF
C  A TIME RESET), BUT THE REST WILL START AT TBEG.

      IF(TBEG .GT. 0.D0 .AND. NUMTSUB .GT. 1) THEN
       TPRED(NUMTSUB) = 0.D0
       TPREDREL(NUMTSUB) = 0.D0

       NUMTSUB = NUMTSUB + 1
      ENDIF

	TPRED(NUMTSUB) = TBEG
      TPREDREL(NUMTSUB) = 0.D0

c  As of npageng18.f, the new code with TPREDREL --> if there is a 
c  steady state dose set, which is not at the beginning of the patient
c  file, TPREDREL(.) will have two consecutive values set to 0.0 in
c  the above code. This is required since otherwise, the correspondence
c  between TPREDREL(.) and NUMTSUB would be lost.


	DO I=1,NUMT2
	 NUMTSUB = NUMTSUB + 1
	 IF(NUMTSUB .GT. 7200) GO TO 40
	 TPRED(NUMTSUB) = TPRED(NUMTSUB-1) + IDELTA/60.D0
       TPREDREL(NUMTSUB) = TPRED(NUMTSUB) - TBEG
	END DO

C  SAVE THIS MAXIMUM TIME + 24 HOURS INTO TEND. IT WILL BE NEEDED

C  IN MAIN WHERE SUBJECT AUCs ARE CALCULATED. ALSO, AS OF npageng16.f,
C  SAVE THE BEGINNING TIME INTO TBEGG.

	NOMAXTIMS = NOMAXTIMS + 1

C  NOTE THAT TEND IS DIMENSIONED 99 ABOVE, BUT IT IS NOT NECESSARY
C  TO TEST THAT NOMAXTIMS .LE. 99 SINCE IF IT WAS EVEN CLOSE TO THIS
C  VALUE, THE NUMTSUB > 7200 TEST BELOW WOULD BE TRUE AND THIS 
C  SUBROUTINE WOULD BE EXITED.

	TEND(NOMAXTIMS) = T_END
      TBEGG(NOMAXTIMS) = TBEG

C  ESTABLISH THE NEXT DOSE TIME THAT STARTS A TIME RESET (WHICH MAY OR

C  MAY NOT BE THE BEGINNING OF A STEADY STATE DOSE SET).

C      IF(IDOSE .LT. ND) THEN
C       DO ID = IDOSE + 1,ND
      IF(IDOSE .LT. intlist(8)) THEN
       DO ID = IDOSE + 1,intlist(8)
        IF(SIG(ID) .LE. 0.D0) THEN
         IDOSE = ID
         GO TO 35
        ENDIF
       END DO
      ENDIF

   35 CONTINUE


	IF(INDEX .EQ. NOBSER) RETURN

C  THE ABOVE RETURN IS THE NORMAL EXIT FROM THIS ROUTINE. THE ONLY
C  OTHER WAY OUT IS THROUGH LABEL 40 BELOW, WHICH WILL ONLY HAPPEN IF
C  NUMTSUB EXCEEDS THE MAX. NO. OF ALLOWABLE TIMES TO BE IN TPRED.

	GO TO 50


   40   CONTINUE


C   TO GET HERE --> NUMTSUB > 7200. 


	WRITE(*,2031) JSUB,IDELTA
	WRITE(25,2031) JSUB,IDELTA
 2031    FORMAT(///' FOR SUBJECT NO. ',I4,' THE MAXIMUM NO. OF '/
     1' PREDICTED VALUES (7200) HAS BEEN REACHED. THIS MEANS THAT IN'/
     2' THE DENSITY PART OF THE OUTPUT FILE, AND IN THE PRTB FILE'/
     3' (WHERE THE PREDICTED VALUES ARE WRITTEN ',I3,' MINUTES APART),'/

     4' THIS SUBJECT WILL NOT HAVE A COMPLETE SET OF PREDICTED VALUES.')

	NUMTSUB = 7200	

C	CALL PAUSE

C  NOTE THAT THE REASON NUMTSUB IS LIMITED TO 7200 IS THAT THE 

C  PREDICTED VALUES FOR THESE TIMES MUST BE STORED INTO
C  YPREDPOPT(MAXSUB,NUMEQT,7201,3), AND IF MAXSUB = 999, AND 
C  NUMEQT = 6, THIS WOULD RESULT IN ALMOST 130 MILLION VALUES IN
C  THIS MATRIX. THIS IS PROBABLY TOO MUCH, BUT FOR NOW (WITH MOST
C  MAXSUB VALUES << 999, AND NUMEQT USUALLY .LE. 3), IT IS PROBABLY
C  OK. BUT MAY HAVE TO ADJUST THIS 7200 DOWN FOR RUNS WITH A LARGE
C  NO. OF PATIENT FILES.



	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE CONDENSE(READLINE)
        CHARACTER READLINE*1000        

C  SUBROUTINE CONDENSE IS CALLED BY MAIN TO WRITE READLINE WITH AS 
C  SMALL A FORMAT AS POSSIBLE (WITHIN 25 CHARACTERS) TO FILE 26

C  FOR THIS LINE, READLINE, FIND IEND, THE LAST CHARACTER WHICH IS NOT
C  BLANK. THEN ONLY CHARACTERS 1:IEND WILL BE WRITTEN TO FILE 26.


	DO IEND = 1000,1,-1
	 IF(READLINE(IEND:IEND) .NE. ' ') GO TO 20
	END DO

   20   CONTINUE

C  CANNOT USE WRITE(26,_) READLINE(1:IEND) SINCE, FOR SOME REASON,
C  WRITING LIKE THIS "RIGHT JUSTIFIES" THE CHARACTERS AT THE END
C  OF THE A1000 FORMAT. INSTEAD MUST WRITE (26,__) READLINE, WHERE
C  THE FORMAT IS DETERMINED BY THE LAST NON-BLANK CHARACTER (IEND)
C  IN READLINE.


	IF(IEND .LE. 26) THEN
	 WRITE(26,26) READLINE
   26    FORMAT(A26)
	 RETURN

	ENDIF

	IF(IEND .LE. 51) THEN
	 WRITE(26,51) READLINE
   51    FORMAT(A51)
	 RETURN
	ENDIF

	IF(IEND .LE. 76) THEN
	 WRITE(26,76) READLINE
   76    FORMAT(A76)
	 RETURN
	ENDIF


	IF(IEND .LE. 101) THEN



	 WRITE(26,101) READLINE

  101    FORMAT(A101)
	 RETURN
	ENDIF

	IF(IEND .LE. 126) THEN
	 WRITE(26,126) READLINE
  126    FORMAT(A126)
	 RETURN
	ENDIF

	IF(IEND .LE. 151) THEN
	 WRITE(26,151) READLINE
  151    FORMAT(A151)
	 RETURN
	ENDIF

	IF(IEND .LE. 176) THEN

	 WRITE(26,176) READLINE
  176    FORMAT(A176)
	 RETURN
	ENDIF

	IF(IEND .LE. 201) THEN
	 WRITE(26,201) READLINE
  201    FORMAT(A201)
	 RETURN
	ENDIF

	IF(IEND .LE. 226) THEN
	 WRITE(26,226) READLINE

  226    FORMAT(A226)
	 RETURN
	ENDIF


	IF(IEND .LE. 251) THEN
	 WRITE(26,251) READLINE
  251    FORMAT(A251)

	 RETURN
	ENDIF

	IF(IEND .LE. 276) THEN
	 WRITE(26,276) READLINE
  276    FORMAT(A276)
	 RETURN
	ENDIF

	IF(IEND .LE. 301) THEN
	 WRITE(26,301) READLINE
  301    FORMAT(A301)
	 RETURN
	ENDIF

	IF(IEND .LE. 326) THEN
	 WRITE(26,326) READLINE
  326    FORMAT(A326)
	 RETURN
	ENDIF

	IF(IEND .LE. 351) THEN
	 WRITE(26,351) READLINE
  351    FORMAT(A351)
	 RETURN
	ENDIF

	IF(IEND .LE. 376) THEN
	 WRITE(26,376) READLINE
  376    FORMAT(A376)
	 RETURN
	ENDIF

	IF(IEND .LE. 401) THEN
	 WRITE(26,401) READLINE

  401    FORMAT(A401)
	 RETURN
	ENDIF

	IF(IEND .LE. 426) THEN
	 WRITE(26,426) READLINE
  426    FORMAT(A426)
	 RETURN
	ENDIF

	IF(IEND .LE. 451) THEN
	 WRITE(26,451) READLINE
  451    FORMAT(A451)
	 RETURN
	ENDIF

	IF(IEND .LE. 476) THEN

	 WRITE(26,476) READLINE
  476    FORMAT(A476)
	 RETURN
	ENDIF

	IF(IEND .LE. 501) THEN
	 WRITE(26,501) READLINE
  501    FORMAT(A501)
	 RETURN
	ENDIF

	IF(IEND .LE. 526) THEN
	 WRITE(26,526) READLINE
  526    FORMAT(A526)
	 RETURN
	ENDIF

	IF(IEND .LE. 551) THEN
	 WRITE(26,551) READLINE
  551    FORMAT(A551)
	 RETURN
	ENDIF

	IF(IEND .LE. 576) THEN
	 WRITE(26,576) READLINE
  576    FORMAT(A576)
	 RETURN
	ENDIF


	IF(IEND .LE. 601) THEN
	 WRITE(26,601) READLINE
  601    FORMAT(A601)
	 RETURN
	ENDIF

	IF(IEND .LE. 626) THEN
	 WRITE(26,626) READLINE
  626    FORMAT(A626)
	 RETURN
	ENDIF

	IF(IEND .LE. 651) THEN
	 WRITE(26,651) READLINE
  651    FORMAT(A651)
	 RETURN
	ENDIF

	IF(IEND .LE. 676) THEN
	 WRITE(26,676) READLINE
  676    FORMAT(A676)
	 RETURN
	ENDIF


	IF(IEND .LE. 701) THEN
	 WRITE(26,701) READLINE
  701    FORMAT(A701)
	 RETURN
	ENDIF


	IF(IEND .LE. 726) THEN
	 WRITE(26,726) READLINE
  726    FORMAT(A726)
	 RETURN
	ENDIF

	IF(IEND .LE. 751) THEN
	 WRITE(26,751) READLINE

  751    FORMAT(A751)
	 RETURN
	ENDIF


	IF(IEND .LE. 776) THEN

	 WRITE(26,776) READLINE
  776    FORMAT(A776)
	 RETURN
	ENDIF

	IF(IEND .LE. 801) THEN
	 WRITE(26,801) READLINE
  801    FORMAT(A801)
	 RETURN
	ENDIF

	IF(IEND .LE. 826) THEN

	 WRITE(26,826) READLINE
  826    FORMAT(A826)
	 RETURN
	ENDIF


	IF(IEND .LE. 851) THEN
	 WRITE(26,851) READLINE
  851    FORMAT(A851)
	 RETURN
	ENDIF

	IF(IEND .LE. 876) THEN
	 WRITE(26,876) READLINE
  876    FORMAT(A876)
	 RETURN
	ENDIF

	IF(IEND .LE. 901) THEN
	 WRITE(26,901) READLINE
  901    FORMAT(A901)
	 RETURN
	ENDIF

	IF(IEND .LE. 926) THEN
	 WRITE(26,926) READLINE
  926    FORMAT(A926)
	 RETURN
	ENDIF

	IF(IEND .LE. 951) THEN
	 WRITE(26,951) READLINE
  951    FORMAT(A951)
	 RETURN
	ENDIF

	IF(IEND .LE. 976) THEN
	 WRITE(26,976) READLINE
  976    FORMAT(A976)
	 RETURN
	ENDIF

	WRITE(26,4) READLINE
    4    FORMAT(A1000)
        RETURN

        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE PAUSE

C  THIS ROUTINE IS USED TO REPLACE A PAUSE STATEMENT, WHICH CAUSES 
C  WARNINGS WHEN THIS PROGRAM IS COMPILED AND LINKED USING gfortran
C  (AND FORCES THE USER TO TYPE "go" INSTEAD OF SIMPLY HITTING THE
C  ENTER KEY).

        WRITE(*,1)
    1   FORMAT(' HIT ANY KEY TO CONTINUE: ')
        READ(*,*,ERR=10) IKEY
        IF(IKEY .EQ. 1) RETURN
   10   RETURN
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE NEWWORK1(MAXSUB,JSUB,TIMOBREL,
     1 DOSEBLOCK, OBSBLOCK, NDORIG , errfil)

       use npag_utils,only: verifyval,orderdelta,thesame,
     1   maxnumeq,max_m_per_obs,max_doses,
     2   max_RS_J

      IMPLICIT REAL*8(A-H,O-Z)

C      PARAMETER(MAXNUMEQ=7)

      DIMENSION SIG(max_doses),RS(max_doses,max_RS_J),
     1 DELTAIV(7),ORDELT(7),
     1 RSS(max_doses,max_RS_J),SIGG(max_doses),
     2 TIM(max_m_per_obs),TIMM(max_m_per_obs),
     3 YO(max_m_per_obs,MAXNUMEQ),
     4 TIMDELAY(99),TIMOBREL(MAXSUB,max_m_per_obs),
     5 OBSBLOCK(800,150,MAXNUMEQ+1),
     6 DOSEBLOCK(800,1000,35),NDORIG(800),XVERIFY(100)

      CHARACTER READLINE*300,ERRFIL*20

C   	COMMON/DOSEOBS/DOSEBLOCK,OBSBLOCK,NDORIG
C      COMMON/ERR/ERRFIL 


C  SUBROUTINE NEWWORK1 (BASED ON THE STAND-A-LONE VERSION OF THE SAME
C  NAME) READS IN A WORKING COPY PATIENT DATA FILE, AND OUTPUTS ANOTHER
C  FILE WHICH IS:

C  a. EXACTLY THE SAME IF THE ORIGINAL FILE HAS NO STEADY STATE DOSE
C     INDICATORS;

C  OR

C  b. ALTERED TO HAVE THE SAME INFO AS IN THE ORIGINAL FILE, BUT ALSO
C     CONTAINING AN EXTRA 101 DOSES FOR EACH STEADY STATE DOSE 
C     INDICATOR.

C  NOTES:

C  1. A STEADY STATE DOSE INDICATOR IS A NEGATIVE VALUE IN THE TIME
C  ENTRY FOR A DOSE. THIS IS ACCOMPANIED BY INFORMATION ON THE SET OF
C  DOSES IN THE IV AND BOLUS ENTRIES, AS THE FOLLOWING EXAMPLE SHOWS:

C   Time    IV     Bolus
C   -2.0   100.0   150.0 ...

C  THE ABOVE LINE WOULD TELL THE PROGRAM:

C  a. THAT THIS WAS INFO ON 101 STEADY STATE DOSES BECAUSE OF THE 
C     NEGATIVE TIME VALUE;
C  b. THE TIME BETWEEN CONSECUTIVE IV START TIMES = 2 HOURS, BECAUSE 
C     THIS IS THE ABS. VALUE OF THE TIME;
C  C. THE IV RATE = 100MG/HOUR;
C  D. THE TOTAL DRUG AMT. FOR EACH IV DOSE IS 150MG.

C  SO THE PROGRAM WOULD THEN ADD 101 DOSES TO THE PATIENT DATA FILE,
C  STARTING AT T = 0, EACH WITH AN IV RATE = 100, AND CONTINUING FOR
C  1.5 HOURS.

C  2. IT WILL BE ASSUMED THAT EACH STEADY STATE DOSE INDICATOR ALWAYS
C  WILL BE REPLACED BY 101 IV DOSES (NOT BOLUS DOSES).
C  AS OF npageng17.f, STEADY STATE DOSES MAY BE BOLUS DOSES. IN THIS
C  CASE, THE IV RATE WILL BE 0.0 OF COURSE.

C  3. ALL OTHER TIMES IN THE PATIENT DATA FILE (UP TO THE NEXT TIME 
C  RESET IF THERE IS ONE) WILL BE ASSUMED TO BE TIMES FROM THE END OF 
C  THE 100TH DOSE INTERVAL (NOT THE 101ST). IN THE ABOVE EXAMPLE, THE
C  100TH DOSE INTERVAL WOULD END AT T = 200 (THE 101ST IV ITSELF WOULD
C  END AT T = 201.5, BUT THE 100TH DOSE INTERVAL WOULD END AT T = 200).
C  SO ALL OTHER TIMES IN THE DOSAGE AND OBSERVATION BLOCKS WOULD HAVE
C  200 ADDED TO THEIR VALUES.

C  4. THE ABOVE EXAMPLE IS FOR ONE DRUG ONLY, BUT ANY OR ALL OF THE
C  NDRUGS IN A PATIENT'S FILE CAN HAVE STEADY STATE DOSES. ANY DRUG
C  WHICH HAS A NON-0 VALUE IN THE BOLUS COLUMN OF A STEADY STATE DOSE
C  LINE (I.E., ONE WITH TIME < 0) WILL PARTICIPATE IN A STEADY STATE
C  DOSE SET, GETTING THAT AMOUNT OF DRUG IN EACH OF THE 101 DOSES. IF
C  THE IV COLUMN IS > 0, THEN THE DRUG WILL BE GIVEN AT THE RATE 
C  SHOWN IN THE IV COLUMN. IF THE IV COLUMN IS 0, THEN THE DRUG WILL
C  BE GIVEN AS A BOLUS.

C  5. THE 101 STEADY STATE DOSES CAN BE GIVEN AS THE FIRST SET OF DOSES
C  IN A PATIENT'S FILE, AS INDICATED ABOVE, OR AT ANY TIME RESET. IF
C  THEY ARE AT A TIME RESET, ALL THE SUBSEQUENT TIMES AFTER THAT TIME
C  RESET (UP TO THE NEXT TIME RESET IF THERE IS ONE) ARE ADJUSTED AS 
C  INDICATED ABOVE TO BE TIMES AFTER THE END OF THE 100TH DOSE SET.

C-----------------------------------------------------------------------


C  FILE 23 WAS OPENED IN MAIN AND THE POINTER IS AT THE TOP OF A PATIENT
C  WHOSE INFO IS TO BE PUT ONTO FILE 27. BUT IT WILL NOT BE PUT ON TO
C  FILE 27 UNTIL THE DOSE BLOCK OF FILE 23 HAS BEEN READ ... AND 
C  EXAMINED TO SEE IF IT HAS A STEADY STATE DOSE INDICATOR. IF IT DOES,
C  IT MEANS THAT THIS PART OF FILE 27 WILL HAVE AN EXTRA SET OF 101
C  DOSES FOR EACH DRUG.

C  NOTE ALSO THAT, AS OF npagen18.f, TIMOBREL(JSUB,J), J=1,M, WILL BE
C  STORED AND RETURNED TO MAIN.

 1717 FORMAT(A300)

   10 READ(23,1717) READLINE
      IF(READLINE(12:23) .NE. 'NO. OF DRUGS') GO TO 10

C  READLINE NOW CONTAINS THE NO. OF DRUGS, NDRUG. BACKSPACE AND READ
C  NDRUG; THEN READ THE NO. OF ADDITIONAL COVARIATES, AND THE NO. OF

C  DOSE EVENTS. 

    3 FORMAT(T2,I5)

      BACKSPACE(23)
      READ(23,3) NDRUG
      READ(23,3) NADD
      READ(23,3) ND
	NI = 2*NDRUG + NADD

C  IF THERE ARE NO DOSE EVENTS (ND = 0), THE INFO ON FILE 27 WILL BE THE
C  SAME AS ON FILE 23 (SINCE THERE CAN BE NO STEADY STATE DOSE EVENTS IF
C  THERE ARE NO DOSES). IN THIS CASE, SET ICOPY = 1 (SEE BELOW).

      IF(ND .EQ. 0) ICOPY = 1

C  IF ANY SIG(.) IS NEGATIVE, SET ICOPY = 0 SINCE A SIG(.) < 0 IS THE
C  INDICATOR FOR A STEADY STATE SET OF DOSES.


      IF(ND .GE. 1) THEN

       READ(23,*)
       READ(23,*)

       ICOPY = 1

C  As of npageng18.f, STORE ND INTO NDORIG(JSUB); IT WILL BE PASSED
C  TO SUBROUTINE READOUT VIA COMMON/DOSEOBS.

       NDORIG(JSUB) = ND


       DO I = 1,ND

        READ(23,*) SIG(I),(RS(I,J),J=1,NI)

C  AS OF npageng18.f, STORE THE VALUES IN THE DOSE BLOCK FOR PASSAGE
C  TO SUBROUTINE READOUT VIA COMMON/DOSEOBS.

C  AS OF npageng19.f, RATHER THAN USING BACKSPACE(23), ESTABLISH
C  DOSEBLOCK BY STRAIGHTFORWARD ASSIGNMENTS. THE REASON IS THAT,
C  DEPENDING ON WHICH COMPILER IS USED TO MAKE THE PR PREP PROGRAM
C  (CURRENT ONE IS NPAG108.FOR), IT IS POSSIBLE FOR A DOSE EVENT
C  TO LOOK LIKE SEVERAL LINES RATHER THAN ONE LONG WORD-WRAPPED LINE.
C  IN THE FORMER CASE, BACKSPACING ONE LINE WILL NOT BACKSPACE TO THE
C  BEGINNING OF THE DOSE EVENT AS SHOULD BE DONE. SO TO BE SAFE, THE
C  LOGIC TO USE BACKSPACE(23) WILL BE COMMENTED OUT, AND 
C  DOSEBLOCK(.,.,.) WILL BE ESTABLISHED DIRECTLY.

        DOSEBLOCK(JSUB,I,1) = SIG(I)
        DO J = 2,1+NI
         DOSEBLOCK(JSUB,I,J) = RS(I,J-1)
        END DO
        
C        BACKSPACE(23)
C        READ(23,*) (DOSEBLOCK(JSUB,I,J),J=1,1+NI)

        IF(SIG(I) .LT. 0.D0) ICOPY = 0

       END DO

      ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(ND .GE. 1)  CONDITION.




C  AS OF npageng18.f, STORE IN THE VALUES IN THE OBSERVATION BLOCK FOR
C  PASSAGE TO SUBROUTINE READOUT VIA COMMON DOSEOBS.

  140	 READ(23,1717) READLINE

       IF(READLINE(12:23) .NE. 'NO. OF TOTAL') GO TO 140

       BACKSPACE(23)

       READ(23,*) NUMEQT
       READ(23,3) M

       DO I = 1,M
        READ(23,*) (OBSBLOCK(JSUB,I,J),J=1,1+NUMEQT)
       END DO

  
C  IF ICOPY = 1, IT MEANS THAT THIS PATIENT DATA FILE DOES NOT HAVE
C  A STEADY STATE DOSE SET, WHICH MEANS THAT THIS PART OF FILE 23 WILL 
C  BE COPIED LINE FOR LINE TO FILE 27 BELOW.

      IF(ICOPY .EQ. 1) THEN


C  COPY FILE 23 TO FILE 27,LINE FOR LINE.

C  BACKSPACE FILE 23 TO THE FIRST LINE FOR THIS PATIENT.


 1720  BACKSPACE(23)
       BACKSPACE(23)
       READ(23,1717,IOSTAT=IEND) READLINE

	 IF(IEND .LT. 0) THEN

C  NOTE THAT IEND .LT. 0 --> END OF FILE REACHED, BUT IF IT'S REACHED
C  AT THIS POINT, NOT ALL "ACTIVE" NSUB SUBJECT DATA SETS WERE READ
C  AND WRITTEN CORRECTLY TO FILE 27. IN THIS CASE, WRITE A MESSAGE TO
C  THE USER AND STOP.

        WRITE(*,1721)
 1721   FORMAT(/' PATIENT DATA INFORMATION WAS NOT READ CORRECTLY'/
     1' FROM THE INSTRUCTION FILE, npag103.inp. IF YOU EDITED THIS'/
     2' FILE MANUALLY, PLEASE RERUN THE PC PREP PROGRAM TO HAVE IT'/
     3' PREPARE npag103.inp AGAIN AND THEN RERUN THIS PROGRAM.'//
     4' IF YOU DID NOT MANUALLY EDIT npag103.inp, PLEASE SEND THE'/
     5' DETAILS OF THIS RUN (STARTING WITH THE PC PREP EXECUTION) TO'/
     5' THE LAPK. '//
     6' THANK YOU.'/)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,1721)
        CLOSE(42)

	  CALL PAUSE
	  STOP

	 ENDIF

       IF(READLINE(3:16) .NE. 'LAST AND FIRST') GO TO 1720


       WRITE(27,1717) READLINE
     
   30  READ(23,1717) READLINE
       WRITE(27,1717) READLINE
   
       IF(READLINE(12:23) .NE. 'NO. OF DOSE ') GO TO 30

C  THE LINE JUST WRITTEN TO FILE 27 IS THE NO. OF DOSE EVENTS LINE.
C  WRITE THE NEXT TWO LINES ALSO.

       DO I = 1,2
        READ(23,1717) READLINE
        WRITE(27,1717) READLINE
       END DO


C  IF ND = 0, SKIP TO THE OUTPUT SECTION. OTHERWISE, WRITE THE DOSAGE
C  REGIMEN TO FILE 27.

       IF(ND.EQ.0) GO TO 40

       DO I = 1,ND
        READ(23,*) SIG(I),(RS(I,J),J=1,NI)
        WRITE(27,*) SIG(I),(RS(I,J),J=1,NI)
       END DO

C  READ THE NO. OF OUTPUT EQUATIONS FROM THE LINE WITH 'NO. OF TOTAL'
C  AS ENTRIES 12:23. THEN READ NO. OF OBSERVED VALUE TIMES, ETC., AND

C  WRITE THE REST OF THE FILE 23 TO FILE 27.

   40	 READ(23,1717) READLINE
       WRITE(27,1717) READLINE
       IF(READLINE(12:23) .NE. 'NO. OF TOTAL') GO TO 40

       BACKSPACE(23)

C  SINCE NUMEQT IS PROVIDED TO THIS ROUTINE IN THE ARGUMENT LIST,
C  JUST READ(23,*) ON NEXT LINE.

       READ(23,*)
       READ(23,3) M

C  BACKSPACE JUST ONCE TO THE LINE WITH M ON IT, SINCE THE LINE WITH

C  NUMEQT ON IT WAS ALREADY PUT INTO FILE 27. 

       BACKSPACE(23)
       READ(23,1717) READLINE
       WRITE(27,1717) READLINE

       DO I = 1,M
        READ(23,*) TIM(I),(YO(I,J),J=1,NUMEQT)
        WRITE(27,*) TIM(I),(YO(I,J),J=1,NUMEQT)
        TIMOBREL(JSUB,I) = TIM(I)
       END DO

C  NOW COPY LINE FOR LINE THE REST OF THIS PATIENT'S INFO TO FILE 27.
C  THIS PATIENT'S INFO WILL END WHEN THE END OF THE FILE IS REACHED
C  (IF THIS IS THE LAST PATIENT), OR WHEN THE START OF THE NEXT

C  PATIENT OCCURS.


   50	 READ(23,1717,IOSTAT=IEND) READLINE
       IF(IEND .LT. 0) GO TO 100

   	 IF(READLINE(3:16) .EQ. 'LAST AND FIRST') GO TO 100

       WRITE(27,1717) READLINE
       GO TO 50


  100	 BACKSPACE(23)

C  FILE 23 WAS BACKSPACED ONE LINE SO THE NEXT LINE TO BE READ IN IN
C  MAIN WILL BE THE FIRST LINE OF THE NEXT SUBJECT (UNLESS THIS SUBJECT
C  IS THE LAST SUBJECT, IN WHICH CASE THE BACKSPACE WON'T MATTER).

      ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(ICOPY .EQ. 1)  CONDITION.


      IF(ICOPY .EQ. 0) THEN


C  SINCE ICOPY = 0, IT MEANS THAT THERE IS AT LEAST ONE SET OF STEADY
C  STATE DOSES IN THE DOSAGE BLOCK. THE LOGIC FOR TRANSLATING THESE
C  STEADY STATE DOSES TO A REGULAR DOSAGE BLOCK (EXCEPT FOR THE NEGATIVE
C  DOSE TIME AT THE START OF EACH STEADY STATE DOSE SET) IS AS FOLLOWS:

C  EACH DOSAGE LINE WILL BE COPIED UNALTERED UNLESS IT IS PART OF A
C  STEADY STATE SET.

C  EACH STEADY STATE SET STARTS WITH A SIG(I) < 0. IN THIS CASE, 101
C  DOSES WILL BE APPLIED AT THIS POINT WITH THE STEADY STATE DOSE FOR
C  DRUG IDRUG = RS(I,2*IDRUG), WHICH WILL BE APPLIED AS A BOLUS IF
C  RS(I,2*IDRUG-1) = 0, AND AS AN IV WITH DURATION
C  RS(I,2*IDRUG)/RS(I,2*IDRUG-1) IF RS(I,2*IDRUG-1) > 0.
C  THE REST OF THE DOSE TIMES IN THIS BLOCK OF DOSES (I.E., UNTIL THE
C  NEXT TIME RESET OR STEADY STATE DOSE INDICATOR) WILL BE INCREASED
C  BY 100*DELDOSE, WHERE DELDOSE = -SIG(I) = INTERDOSE INTERVAL FOR
C  THIS SET.


C  ILINE WILL BE THE RUNNING INDEX OF THE NEXT DOSAGE LINE TO BE PUT

C  INTO THE ALTERED DOSAGE REGIMEN. SIGG(ILINE) AND RSS(ILINE,.) ARE
C  THE VALUES THAT GO INTO THIS LINE. DELDOSE IS THE CURRENT INTERDOSE
C  TIME INTERVAL FOR THE LAST STEADY STATE SET OF DOSES ALREADY PUT
C  INTO THE ALTERED DOSAGE REGIMEN (IT IS INITIALIZED TO BE 0 OF 
C  COURSE). 

C  AND NSECTION IS INITIALIZED TO BE 0. IT WILL BE THE RUNNING NO. OF
C  DOSAGE SECTIONS. EACH SECTION BEGINS WITH EITHER A 0.0 (BEGINNING
C  LINE OR TIME RESET) OR A NEGATIVE NO. (STEADY STATE DOSE SET 
C  INDICATOR). THE TIME DELAY ASSOCIATED WITH EACH DOSE SECTION (WHICH
C  WILL BE 0 IF THAT SECTION IS NOT A STEADY STATE DOSE SET), MUST BE 
C  STORED TO BE APPLIED TO THE CORRESPONDING SET OF OBSERVED VALUES
C  BELOW.


      ILINE = 0
      DELDOSE = 0.D0
      NSECTION = 0

      DO ID = 1,ND


       IF(SIG(ID) .GE. 0.D0) THEN

        CALL THESAME(SIG(ID),0.D0,ISAME)

        IF(ISAME .EQ. 1) THEN
         DELDOSE = 0.D0
         NSECTION = NSECTION + 1

         TIMDELAY(NSECTION) = 0.0
        ENDIF   

C  NOTE THAT IF SIG(ID) = 0, THIS LINE IS A TIME RESET LINE, OR THE
C  FIRST LINE IN THE DOSAGE REGIMEN. IF IT'S THE FIRST LINE IN THE
C  DOSAGE REGIMEN, THERE ARE OBVIOUSLY NO PREVIOUS STEADY STATE DOSE
C  SETS. IF ITS A TIME RESET LINE, A PREVIOUS SET OF 101 STEADY STATE
C  DOSES HAS NO EFFECT ON IT. THAT'S WHY DELDOSE IS SET = 0, WHICH 
C  MEANS, BELOW, THAT SIGG(ILINE) WILL = SIG(ID) = 0. ALSO, THE TIME
C  DELAY STORED IN TIMDELAY ABOVE IS 0 SINCE SIG(ID) .GE. 0 --> THIS
C  IS NOT A STEADY STATE DOSE SET.
        
        ILINE = ILINE + 1
        SIGG(ILINE) = SIG(ID) + 100.D0*DELDOSE

        DO J = 1,NI
         RSS(ILINE,J) = RS(ID,J)
        END DO
       
       ENDIF


       IF(SIG(ID) .LT. 0.D0) THEN

C  THIS LINE GIVES INFO ON A STEADY STATE SET OF 101 DOSES WHICH IS
C  TO APPLIED AT THIS POINT.

        DO IDRUG = 1,NDRUG

C  FOR DRUG, IDRUG, THE AMOUNT OF DRUG FOR DRUG NO. IDRUG IN EACH OF THE
C  101 DOSES WILL BE RS(ID,2*IDRUG). IF RS(ID,2*IDRUG) > 0, DRUG, IDRUG,
C  PARTICIPATES IN THE STEADY STATE DOSING. IF THIS VALUE = 0, DRUG,
C  IDRUG, DOES NOT PARTICIPATE. NOTE THAT IF A DRUG PARTICIPATES, THE
C  ROUTE WILL BE AS AN IV, WITH RATE RS(ID,2*IDRUG-1), IF 
C  RS(ID,2*IDRUG-1) > 0. BUT IF THIS VALUE IS 0, THE DRUG WILL BE GIVEN
C  AS A BOLUS. NOTE THAT THE INTERVAL BETWEEN DOSES IS -SIG(ID).

C  IF DRUG, IDRUG, PARTICIPATES IN THE 101 STEADY STATE DOSE SET, PUT 
C  THE DURATION OF IV INTO DELTAIV(IDRUG) IF RS(ID,2*IDRUG-1) > 0;
C  OTHERWISE PUT 0 INTO DELTAIV(IDRUG) SINCE IN THIS CASE, THE DRUG IS
C  GIVEN AS A BOLUS.

         DELTAIV(IDRUG) = 0.D0 
         IF(RS(ID,2*IDRUG) .GT. 0.D0 .AND. RS(ID,2*IDRUG-1) .GT. 0.D0) 
     1    DELTAIV(IDRUG) = RS(ID,2*IDRUG)/RS(ID,2*IDRUG-1) 

C  IT SHOULD NOT BE POSSIBLE FOR THE IV OF THIS DRUG TO BE > 0 AT THE
C  SAME TIME THAT THE BOLUS ENTRY = 0. THIS WOULD MEAN THAT AN IV
C  WAS TO BE GIVEN AT A SPECIFIED RATE, BUT WITH A TOTAL DOSE OF 0,
C  AND THIS MAKES NO SENSE. IF, SOMEHOW, THIS HAS OCCURRED, REPORT IT
C  TO THE USER AS AN ERROR, AND STOP.

       XVERIFY(1) = SIG(ID)
       XVERIFY(2) = RS(ID,2*IDRUG-1) 
       XVERIFY(3) = RS(ID,2*IDRUG)
       CALL VERIFYVAL(3,XVERIFY)  
      


      IF(RS(ID,2*IDRUG) .LE. 0.D0 .AND. RS(ID,2*IDRUG-1) .GT. 0) THEN
C      WRITE(*,101) ID,SIG(ID),IDRUG,RS(ID,2*IDRUG-1),RS(ID,2*IDRUG)
       WRITE(*,101) ID,XVERIFY(1),IDRUG,XVERIFY(2),XVERIFY(3)

  101     FORMAT(//' THERE IS AN ERROR IN YOUR INSTRUCTION FILE, AS'/
     1' DETERMINED BY SUBROUTINE NEWWORK1.'//
     2' ONE OF THE SUBJECTS HAS A STEADY STATE DOSE SET WITH A '/
     3' POSITIVE IV RATE, BUT WITH A TOTAL DOSE AMOUNT .LE. 0.'//
     4' IN PARTICULAR, FOR DOSE EVENT ',I4,' AND TIME ',G19.9,/
     5' FOR DRUG ',I2,', THE IV VALUE IS ',G19.9,' WHILE THE TOTAL'/
     6' DOSE AMOUNT IS ',G19.9//
     7' THE PROGRAM STOPS. PLEASE CORRECT THE ERROR BEFORE RERUNNING.'/)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,101) ID,SIG(ID),IDRUG,RS(ID,2*IDRUG-1),RS(ID,2*IDRUG)
        CLOSE(42)

          CALL PAUSE
          STOP
         ENDIF

 
        END DO

        
C  CALL SUBROUTINE ORDERDELTA TO OBTAIN NDELTA, THE NO. OF UNIQUE
C  NON-0 VALUES IN THE DELTAIV(.) ARRAY JUST ESTABLISHED ABOVE, AND TO
C  PUT THE ORDERED SET OF THESE NDELTA VALUES INTO ORDELT(.).

C  NOTE THAT IF DELTAIV(IDRUG) = 0, IT MEANS THAT DRUG, IDRUG, DOES NOT
C  PARTICIPATE IN THE STEADY STATE DOSE SET, OR IF IT DOES, IT IS GIVEN
C  AS A BOLUS RATHER THAN AN IV.

        CALL ORDERDELTA(NDRUG,DELTAIV,NDELTA,ORDELT)

C  NOW ESTABLISH THE LINES WITH SIGG(.) AND RSS(.,.) AS FOLLOWS:

C  1. THE NEXT 101*(NDELTA + 1) ROWS WILL BE FOR THE STEADY STATE
C  DOSE SET (I.E., EACH OF THE 101 REPEATED DOSES HAS A START TIME,
C  AND THEN NDELTA ENDING TIMES AMONG ALL NDRUG DRUGS). NOTE THAT
C  NDELTA WILL BE 0 IF ALL THE PARTICIPATING DRUGS ARE BOLUSES SINCE
C  THEY WOULDN'T NEED AN ENDING TIME THEN.

C  2. EVERY ROW OF THE ORIGINAL DOSAGE REGIMEN AFTER LINE ID
C  WILL HAVE THE SAME VALUES IN RSS(.,.) AS IN RS(.,.), BUT THE

C  TIMES IN SIGG(.) WILL ALL BE INCREASED BY 100*DELDOSE OVER THOSE
C  IN SIG(.) ... UP TO BUT NOT INCLUDING THE NEXT TIME RESET EVENT
C  OR NEXT STEADY STATE DOSE INDICATOR LINE, WHERE DELDOSE IS THE TIME
C  INCREMENT BETWEEN CONSECUTIVE DOSES IN THE 101 STEADY STATE DOSE SET.
C  NOTE THAT DELDOSE IS THE NEGATIVE OF SIG(ID).

        DELDOSE = -SIG(ID)        
        NSECTION = NSECTION + 1
        TIMDELAY(NSECTION) = 100.D0*DELDOSE

C  NOTE THAT THE TIME DELAY ASSOCIATED WITH THIS STEADY STATE SET IS
C  STORED INTO TIMDELAY ABOVE SO THAT IT CAN BE APPLIED TO THE 
C  CORRESPONDING SET OF OBSERVED VALUES BELOW.


        DO ISET = 1,101


C  FOR EACH SET, ESTABLISH NDELTA + 1 ROWS (DOSE EVENT LINES).

C  THE FIRST ROW IN THIS SET HAS EACH DRUG IV SET = RS(ID,2*IDRUG-1),
C  AND, FOR EACH DRUG IV WHICH IS 0, THE BOLUS VALUE WILL BE SET =
C  RS(ID,2*IDRUG). NOTE THAT IF A DRUG IV > 0, THE BOLUS VALUE WILL BE
C  SET = 0 SINCE IN THIS CASE, THE VALUE IN THE BOLUS COLUMN IS THE
C  TOTAL AMOUNT OF IV (NOT A BOLUS AMOUNT).

         ILINE = ILINE + 1

         DO IDRUG = 1,NDRUG
          RSS(ILINE,2*IDRUG-1) = RS(ID,2*IDRUG-1)
          RSS(ILINE,2*IDRUG) = RS(ID,2*IDRUG)
          IF(RS(ID,2*IDRUG-1) .GT. 0.D0) RSS(ILINE,2*IDRUG) = 0.D0
         END DO

C  SET ALL THE COVARIATE VALUES = TO THOSE IN LINE ID OF RS OF COURSE.

         DO IADD = 1,NADD
          RSS(ILINE,2*NDRUG+IADD) = RS(ID,2*NDRUG+IADD)
         END DO

C  THE TIME FOR THIS ROW IS (ISET-1)*DELDOSE, EXCEPT FOR THE FIRST
C  LINE, WHICH MUST HAVE THE SAME NEGATIVE VALUE AS IN SIG, SINCE
C  THE ID ROUTINES MUST READ THE NEGATIVE SIG VALUE TO KNOW THAT A
C  STEADY STATE DOSE SET IS STARTING.

         IF(ISET .EQ. 1) THEN
          SIGG(ILINE) = SIG(ID)
          DOSESTART = 0.D0
         ENDIF

         IF(ISET .GT. 1) THEN
          SIGG(ILINE) = (ISET-1)*DELDOSE
          DOSESTART = SIGG(ILINE)

         ENDIF

C  THE NEXT NDELTA ROWS ARE THE IV TURN OFF ROWS FOR THE VARIOUS DRUGS,
C  IF NDELTA > 0. NOTE THAT NDELTA COULD = 0 IF ALL PARTICIPATING DRUGS
C  ARE GIVEN VIA A BOLUS, SINCE THEN NONE WOULD NEED A TURN OFF ROW.

        IF(NDELTA .GT. 0) THEN

         DO INDEL = 1,NDELTA

          ILINE = ILINE + 1

C  THE NEXT TURN OFF TIME IS DOSESTART + ORDELT(INDEL). EACH IV WILL BE
C  OFF UNLESS ITS DELTAIV(.) IS .GT ORDELT(INDEL). AND EACH BOLUS VALUE
C  WILL BE 0 OF COURSE (I.E., EACH BOLUS IS GIVEN JUST ONE TIME AT THE
C  START OF EACH SET).

          DO IDRUG = 1,NDRUG
           RSS(ILINE,2*IDRUG-1) = 0.D0
           IF(DELTAIV(IDRUG) .GT. ORDELT(INDEL))
     1      RSS(ILINE,2*IDRUG-1) = RS(ID,2*IDRUG-1)        
           RSS(ILINE,2*IDRUG) = 0.D0
          END DO

C  SET ALL THE COVARIATE VALUES = TO THOSE IN LINE ID OF RS AGAIN.

          DO IADD = 1,NADD
           RSS(ILINE,2*NDRUG+IADD) = RS(ID,2*NDRUG+IADD)
          END DO


C  THE TIME FOR THIS ROW IS DOSESTART + ORDELT(INDEL)

          SIGG(ILINE) = DOSESTART + ORDELT(INDEL)

         END DO   

C  THE ABOVE END DO IS FOR THE  DO INDEL = 1,NDELTA  LOOP.  

        ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(NDELTA .GT. 0)  CONDITION.


 
        END DO

C  THE ABOVE END DO IS FOR  DO ISET = 1,101  LOOP.


       ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(SIG(ID) .LT. 0.D0)  CONDITION.


      END DO

C  THE ABOVE END DO IS FOR THE  DO ID = 1,ND  LOOP.


C  THIS COMPLETES THE ESTABLISHMENT OF RSS(.,.) AND SIGG(.) ABOVE.


C  NOW ALTER THE OBSERVED VALUE TIMES BY ADDING THE APPROPRIATE VALUE
C  IN TIMDELAY(.) TO EACH OBSERVED VALUE TIME BELOW. NOTE THAT
C  TIMDELAY(1) APPLIES TO ALL TIMES BEFORE THE FIRST TIME RESET,
C  TIMDELAY(2) APPLIES TO THE NEXT SET OF TIMES AFTER THE FIRST 
C  TIME RESET BUT BEFORE THE 2ND, ETC. IF THERE ARE NO TIME RESETS,
C  ALL TIMES WILL HAVE TIMDELAY(1) ADDED TO THEM, AND THIS VALUE WILL
C  BE 0.0 (SEE DOSAGE BLOCK CODE ABOVE - IF THERE ARE NO TIME RESETS
C  OR STEADY STATE DOSE SETS, TIMDELAY(1) IS SET = 0).

C  SINCE THE OBSERVATION BLOCK WAS READ THROUGH ABOVE, BACKSPACE TO
C  THE BEGINNING OF THE OBS. BLOCK, SO THIS PART OF THE PATIENT'S
C  DATA CAN BE ACCESSED AGAIN.

1920   BACKSPACE(23)
       BACKSPACE(23)
       READ(23,1717,IOSTAT=IEND) READLINE

	 IF(IEND .LT. 0) THEN

C  NOTE THAT IEND .LT. 0 --> END OF FILE REACHED, BUT IF IT'S REACHED
C  AT THIS POINT, NOT ALL "ACTIVE" NSUB SUBJECT DATA SETS WERE READ
C  AND WRITTEN CORRECTLY TO FILE 27. IN THIS CASE, WRITE A MESSAGE TO
C  THE USER AND STOP.

        WRITE(*,1721)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,1721) 
        CLOSE(42)

	  CALL PAUSE
	  STOP

	 ENDIF

       IF(READLINE(12:23) .NE. 'NO. OF TOTAL') GO TO 1920

       BACKSPACE(23)

C  SINCE NUMEQT IS PROVIDED TO THIS ROUTINE IN THE ARGUMENT LIST,
C  JUST READ(23,*) ON NEXT LINE.

       READ(23,*)

       READ(23,3) M

       NSECTION = 1

       DO I = 1,M
        READ(23,*) TIM(I),(YO(I,J),J=1,NUMEQT)
        TIMOBREL(JSUB,I) = TIM(I)
        CALL THESAME(TIM(I),0.D0,ISAME)
        IF(ISAME .EQ. 1 .AND. I .GT. 1) NSECTION = NSECTION + 1
        IF(ISAME .EQ. 1) TIMM(I) = 0.D0   
        IF(ISAME .EQ. 0) TIMM(I) = TIM(I) + TIMDELAY(NSECTION)
       END DO



C  NOW COPY THIS PART OF FILE 23 TO FILE 27 WITH THE FOLLOWING
C  EXCEPTIONS:
C   1. ND WILL BE REPLACED BY ILINE (THE TOTAL NO. OF DOSAGE LINES IN
C      THE ALTERED DOSAGE REGIMEN).
C   2. SIG(.) WILL BE REPLACED BY SIGG(.).
C   3. RS(.,.) WILL BE REPLACED BY RSS(.,.)
C   4. TIM(.) WILL BE REPLACED BY TIMM(.)
C   NOTE THAT YO(.,.) WILL BE UNCHANGED.

C  BACKSPACE FILE 23 TO THE FIRST LINE FOR THIS PATIENT.


 1820  BACKSPACE(23)

       BACKSPACE(23)
       READ(23,1717,IOSTAT=IEND) READLINE

	 IF(IEND .LT. 0) THEN

C  NOTE THAT IEND .LT. 0 --> END OF FILE REACHED, BUT IF IT'S REACHED
C  AT THIS POINT, NOT ALL "ACTIVE" NSUB SUBJECT DATA SETS WERE READ
C  AND WRITTEN CORRECTLY TO FILE 27. IN THIS CASE, WRITE A MESSAGE TO
C  THE USER AND STOP.

        WRITE(*,1721)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,1721)
        CLOSE(42)

	  CALL PAUSE
	  STOP

	 ENDIF

       IF(READLINE(3:16) .NE. 'LAST AND FIRST') GO TO 1820

       WRITE(27,1717) READLINE

   60  READ(23,1717) READLINE
       WRITE(27,1717) READLINE
   
       IF(READLINE(12:23) .NE. 'NO. OF ADDIT') GO TO 60

C  THE LINE JUST WRITTEN TO FILE 27 IS THE NO. OF ADDITIONAL COVARIATES
C  LINE. WRITE THE NEXT LINE BUT CHANGE FROM ND TO ILINE AS THE NO.
C  OF DOSE EVENTS.

       READ(23,1717) READLINE
       WRITE(27,133) ILINE
  133  FORMAT(I6,' ... NO. OF DOSE EVENTS')

C  WRITE THE NEXT TWO LINES TO FILE 27 (INCLUDING THE HEADER LINE FOR
C  THE DOSAGE BLOCK).

       DO I = 1,2
        READ(23,1717) READLINE
        WRITE(27,1717) READLINE
       END DO

C  WRITE THE NEW DOSAGE BLOCK.

       SIGLAST = -999999.D0

       DO I = 1,ILINE

        WRITE(27,*) SIGG(I),(RSS(I,J),J=1,NI)


C  AS OF npageng27.f, MAKE SURE THAT NO TWO TIMES ARE THE SAME SINCE
C  IF THEY ARE, IT CAN CONFUSE SUBROUTINE SHIFT (CAUSING IT TO GO INTO
C  AN INFINITE LOOP - SEE NPAG115.EXP, TESTCASE 5).

        CALL THESAME(SIGLAST,SIGG(I),ISAME)

        IF(ISAME .EQ. 1) THEN

         XVERIFY(1) = SIGLAST
         CALL VERIFYVAL(1,XVERIFY)



C        WRITE(*,4031) SIGLAST


         WRITE(*,4031) XVERIFY(1)
 4031    FORMAT(/' IN SUBROUTINE NEWWORK1, TWO CONSECUTIVE DOSE TIMES'/
     1' HAVE THE SAME VALUE IN WORKING COPY FORMAT, ',F20.8//
     2' THIS COULD CAUSE UNEXPECTED RESULTS IF THE PROGRAM WERE TO '/
     3' CONTINUE. SO THE PROGRAM NOW STOPS. PLEASE CHECK YOUR PATIENT '/
     4' INFORMATION AND CORRECT (NOTE THAT THIS CAN HAPPEN IF THE '/
     5' FIRST DOSE FOLLOWING A STEADY STATE DOSE SET HAS THE SAME'/
     6' STARTING TIME AS THE ENDING TIME OF THE LAST STEADY STATE '/
     7' DOSE SET.)'//)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,4031) SIGLAST
        CLOSE(42)

	  CALL PAUSE
	  STOP

	 ENDIF

       SIGLAST = SIGG(I)


       END DO


C  READ THROUGH FILE 23 DOWN TO THE END OF THE DOSAGE BLOCK

       DO I = 1,ND
        READ(23,*) SIG(I),(RS(I,J),J=1,NI)
       END DO
 
C  PUT THE BLANK LINE BETWEEN THE DOSAGE BLOCK AND THE OBSERVATION
C  BLOCK TO FILE 27, ALONG WITH THE TWO LINES WHICH GIVE THE NO. OF
C  OUTPUT EQS. AND THE NO. OF OBSERVED VALUE TIMES.

       DO I = 1,3
        READ(23,1717) READLINE
        WRITE(27,1717) READLINE
       END DO

C  WRITE THE OBSERVED BLOCK TO FILE 27, AND READ THROUGH IT IN FILE 23.

      DO I = 1,M
       WRITE(27,*) TIMM(I),(YO(I,J),J=1,NUMEQT)
       READ(23,*) TIM(I),(YO(I,J),J=1,NUMEQT)
      END DO
 
C  NOW COPY LINE FOR LINE THE REST OF THIS SUBJECT'S INFO TO FILE 27.
C  THIS PATIENT'S INFO WILL END WHEN THE END OF THE FILE IS REACHED
C  (IF THIS IS THE LAST PATIENT), OR WHEN THE START OF THE NEXT
C  PATIENT OCCURS.

   70	 READ(23,1717,IOSTAT=IEND) READLINE
       IF(IEND .LT. 0) GO TO 200
   	 IF(READLINE(3:16) .EQ. 'LAST AND FIRST') GO TO 200
       WRITE(27,1717) READLINE
       GO TO 70
  200	 BACKSPACE(23)

C  FILE 23 WAS BACKSPACED ONE LINE SO THE NEXT LINE TO BE READ IN IN
C  MAIN WILL BE THE FIRST LINE OF THE NEXT SUBJECT (UNLESS THIS SUBJECT

C  IS THE SUBJECT, IN WHICH CASE THE BACKSPACE WON'T MATTER).

      ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(ICOPY .EQ. 0)  CONDITION.


      RETURN
      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C wmy20190317 Moved to npag_utils.f90
C
      SUBROUTINE Old_ORDERDELTA(NDRUG,DELTAIV,NDELTA,ORDELT)

      use npag_utils, only: thesame

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION DELTAIV(7),ORDELT(7),X(7)

C  SUBROUTINE ORDERDELTA IS CALLED BY NEWWORK1 TO OBTAIN NDELTA, THE NO.
C  OF UNIQUE NON-0 VALUES IN THE DELTAIV(.) ARRAY. THEN THE ORDERED SET
C  OF THESE NDELTA VALUES IS PUT INTO ORDELT(.). NOTE THAT
C  NDELTA WILL BE 0 IF ALL THE PARTICIPATING DRUGS ARE BOLUSES SINCE
C  THEY WOULDN'T NEED AN ENDING TIME THEN.



C  FIRST STORE ALL THE VALUES IN DELTAIV INTO X SO THAT DELTAIV WILL
C  NOT BE CHANGED.

      DO IDRUG = 1,NDRUG
       X(IDRUG) = DELTAIV(IDRUG)
      END DO


C  THE LOGIC OF THIS ROUTINE IS BASED ON \PERSONAL\FINANCE\ORDER.FOR.
C  TO DO THIS, EACH VALUE IN X(.) WILL BE COMPARED TO THE
C  PREVIOUS ONE. IF IT IS < THE PREVIOUS ONE, THE VALUE WILL EXCHANGE
C  PLACES WITH THE PREVIOUS ONE, AND THE TESTING WILL CONTINUE. THE
C  TESTING WILL STOP FOR A VALUE WHEN IT IS COMPARED TO A PREVIOUS
C  VALUE WHICH IS .LE. ITS VALUE.

      DO IDRUG = 2, NDRUG


C  COMPARE VALUE FOR IDRUG WITH EACH PREVIOUS VALUE, AND HAVE IT 
C  EXCHANGE PLACES WITH THAT VALUE, UNTIL IT REACHES ONE WHICH HAS A 
C  SMALLER VALUE. FIRST SET IDRUGNEW = IDRUG; AFTER THE FOLLOWING
C  CODE, IDRUGNEW WILL BE THE INDEX NO. FOR VALUE AT THE OLD IDRUG
C  POSITION.

       IDRUGNEW = IDRUG

       ICOMP = IDRUG 

  110  ICOMP = ICOMP - 1

C  NOW COMPARE VALUE IN LOCATION ICOMP WITH THE VALUE IN LOCATION
C  IDRUGNEW. IF THE LATTER IS .LT. THE FORMER, INTERCHANGE THE RECORDS.

       IF(X(IDRUGNEW) .LT. X(ICOMP)) THEN

        VALUE = X(IDRUGNEW)
        X(IDRUGNEW) = X(ICOMP)         
        X(ICOMP) = VALUE
        IDRUGNEW = ICOMP


C  IF IDRUGNEW = 1, IT HAS BEEN CHECKED AGAINST ALL RECORDS (AND IS
C  THE SMALLEST VALUE); IF IS IS > 1, CONTINUE THE PROCESS.

        IF(IDRUGNEW .EQ. 1) GO TO 150
        IF(IDRUGNEW .GT. 1) GO TO 110



       ENDIF

C  THE ABOVE ENDIF IS FOR THE 
C   IF(X(IDRUGNEW) .LT. X(ICOMP))  CONDITION.


  150 END DO

C  THE ABOVE END DO IS FOR THE  DO IDRUG = 2, NDRUG LOOP.


C  NOW THE NDRUG VALUES ARE ORDERED, FROM SMALL TO LARGE IN X.
C  REWRITE THEM INTO ORDELT, BUT PUT ONLY THE NON-0 AND
C  UNIQUE VALUES INTO ORDELT, AND KEEP TRACK OF NOW MANY OF THESE
C  UNIQUE NON O VALUES THERE ARE - IT WILL BE NDELTA AT THE END OF
C  THE FOLLOWING LOOP.

      NDELTA = 0 

      DO IDRUG = 1,NDRUG

C  FOR THIS VALUE TO BE COUNTED, IT CANNOT = THE PREVIOUS VALUE, AND
C  IT CANNOT = 0.



       IF(IDRUG .EQ. 1 .AND. X(IDRUG) .GT. 0) THEN
        NDELTA = NDELTA + 1
        ORDELT(NDELTA) = X(IDRUG)
       ENDIF



       IF(IDRUG .GE. 2) THEN

        CALL THESAME(X(IDRUG),X(IDRUG-1),ISAME)

        IF(ISAME .EQ. 0) THEN
         NDELTA = NDELTA + 1
         ORDELT(NDELTA) = X(IDRUG)
        ENDIF

       ENDIF

      END DO 

C  THE ABOVE END DO IS FOR THE  DO IDRUG = 1,NDRUG  LOOP.


      RETURN
      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C wmy20190317 -- Moved to npag_utils.f90
	SUBROUTINE Old_THESAME(X1,X2,ISAME)
	IMPLICIT REAL*8(A-H,O-Z)

C  THIS ROUTINE CHECKS TO SEE IF X1 AND X2 ARE VIRTUALLY THE SAME
C  VALUES (I.E., IF THEY ARE WITHIN 1.D-10 OF EACH OTHER). IF SO,
C  ISAME RETURNS AS 1; IF NOT ISAME RETURNS AS 0.

	ISAME = 0
	XDEL = DABS(X1-X2)
	IF(XDEL .LE. 1.D-10) ISAME = 1

	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C wmy20190315 Moved to npag_utils.f90
C
      SUBROUTINE Old_VERIFYVAL(N,X)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(100)

C  THIS ROUTINE INPUTS X(I),I=1,N.

C  ON OUTPUT, EACH X(.) WHICH IS INSIDE [-1.D-99, 1.D-99] IS REPLACED
C  BY 0. THIS PREVENTS THIS VALUE FROM BEING WRITTEN OUT IMPROPERLY,
C  E.G., AS .934-106, RATHER THAN .934E-106.
C  ANY X(.) VALUE NOT INSIDE THE ABOVE RANGE WILL BE UNCHANGED ON
C  OUTPUT.

      DO I = 1,N
       IF(X(I) .GE. -1.D-99 .AND. X(I) .LE. 1.D-99) X(I) = 0.D0
      END DO

      RETURN
      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE CALCRF(NTOTPAR,VEC,FNTVAL,NUMEQT,YO,C0,C1,C2,C3,C4,C5
     1    ,NBCOMP,NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     2    INTLIST,IPAR,ObsError,RPAR,
     3    gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     4    errfilname)

        use npag_utils, only: maxnumeq,max_m_per_obs,max_ODE_params
     1    ,cp_lrcs_to_rpar
     2    ,max_pop_rand_varbs,max_doses,max_ODE_comps,max_RS_J
     3    ,max_input_dim,k_gamma,k_flat,i_is_poisson_obs,i_is_log10

        IMPLICIT REAL*8(A-H,O-Z)

C  THIS SUBROUTINE IS CALLED BY ELDERY TO FIND THE FUNCTIONAL VALUE,
C  FNTVAL, FOR THE SUPPLIED CANDIDATE VECTOR, VEC. ELDERY CALLS THIS
C  SUBROUTINE OVER AND OVER UNTIL IT FINDS THE VECTOR, VEC, WHICH
C  MINIMIZES FNTVAL.

C  FNTVAL IS THE NORMALIZED SUM OF SQ. DIFFERENCES BETWEEN ALL OBSERVED
C  AND PREDICTED VALUES OVER ALL NSUB SUBJECTS, GIVEN THE NTOTPAR 
C  VALUES SUPPLIED IN THE CANDIDATE VECTOR VEC. NOTE THAT THESE VALUES
C  WILL BE ASSIGNED TO THE PARAMETER ENTRIES IN PX(.) WHICH HAVE 
C  IRAN(.) = 2, AND THEN IRAN(.) = 1. THE OTHER PARAMETER VALUES (I.E.,
C  THOSE WHICH HAVE IRAN(.) = 0) WERE ALREADY ASSIGNED TO THE 
C  APPROPRIATE ENTRIES IN PX BEFORE ELDERY WAS CALLED IN MAIN.

C      PARAMETER(MAXNUMEQ=7)

        DIMENSION VEC(NTOTPAR),
     1 YO(max_m_per_obs,NUMEQT),
     2 C0(NUMEQT),C1(NUMEQT),C2(NUMEQT),C3(NUMEQT),C4(NUMEQT),C5(NUMEQT)
C     1 SIG(max_m_per_obs,MAXNUMEQ),

C      COMMON SIG

C      COMMON/TOCALC/gamma,flat,AB,PX,IRAN,NOFIX,NSUB
      double precision gamma, flat
      double precision, dimension(max_pop_rand_varbs,2) :: AB
      double precision, dimension(max_ODE_params) :: PX
      integer, dimension(max_ODE_params) :: IRAN
      integer NOFIX,NSUB

       character*20 errfilname

C wmy2017Sep12 Added
C      COMMON/TOUSER/NDIM,MF,RTOL,ATOL
       integer  NDIM,MF
       real*8 RTOL
       real*8, dimension(max_ODE_comps) :: ATOL
       real*8, dimension(max_m_per_obs) :: TIMCOPY
       real*8, dimension(max_doses) :: SIGCOPY
       real*8, dimension(max_doses,max_RS_J) :: RSCOPY
       real*8, dimension(max_doses,max_input_dim) :: BSCOPY

c wmy2017Sep30
       integer, dimension(128) :: INTLIST
       integer, dimension(257) :: IPAR
       double precision, dimension(max_m_per_obs,MAXNUMEQ) :: ObsError
       double precision, dimension(257) :: RPAR

       integer NNORMALOBS, NPOISSONOBS
C       integer MISVAL

       integer, dimension(max_input_dim) :: NBCOMP

C !$omp ThreadPrivate(/TOCALC/)
C !$omp ThreadPrivate( IPAR, ObsError )

C  COMMON SIG IS USED TO PASS THE VALUES ESTABLISHED IN SIG(.,.)
C  BELOW TO SUBROUTINE FUNC. wmy20190731 Removed COMMON SIG, and
C  replaced with ObsError

C  COMMON/TOCALC VALUES ARE PASSED TO THIS ROUTINE FROM MAIN.


C  AS INDICATED ABOVE, PX(.) HAS THE CORRECT VALUES ALREADY IN PLACE
C  FOR THE PARAMETERS WITH IRAN(.) = 0. NOW INSERT THE CANDIDATE
C  VALUES IN VEC(.) INTO THE ENTRIES IN PX(.) WITH IRAN(.) = 2 AND 1.
C  AND, AS OF npagranfix3.f, ENSURE THE CANDIDATE VALUES FOR THE
C  RANDOM VARIABLES (I.E., THOSE WITH IRAN(.) = 1) ARE INSIDE THEIR
C  RESPECTIVE BOUNDARIES.

      NVEC = 0
 
      DO I = 1,NTOTPAR+NOFIX
       IF(IRAN(I) .EQ. 2) THEN
        NVEC = NVEC + 1
        PX(I) = VEC(NVEC)
       ENDIF
      END DO

      IRANO = 0
      DO I = 1,NTOTPAR+NOFIX
       IF(IRAN(I) .EQ. 1) THEN
        NVEC = NVEC + 1
        IRANO = IRANO + 1
C  THIS IS RANDOM VARIABLE NO. IRANO. VERIFY THAT ITS CANDIDATE
C  VALUE IS INSIDE ITS BOUNDARIES, [AB(IRANO,1), AB(IRANO,2)]. IF NOT,
C  RETURN A LARGE POSITIVE VALUE, WHICH IS UNATTRACTIVE, FOR FNTVAL.
        VN = VEC(NVEC)
        IF(VN .GT. AB(IRANO,2) .OR. VN .LT. AB(IRANO,1)) THEN
         FNTVAL = 1.D30
         RETURN
        ENDIF
C  TO GET HERE --> THE CANDIDATE VECTOR IS WITHIN THE REQUIRED RANGE.        
        PX(I) = VEC(NVEC)
       ENDIF
      END DO



C  INITIALIZE SUMTOT = 0.D0. IT WILL BE THE RUNNING SUM OVER ALL NSUB
C  SUBJECTS OF THE NORMALIZED SUM OF SQ. DIFFERENCES BETWEEN ALL
C  OBSERVED AND PREDICTED VALUES.

      SUMTOT = 0.D0

      REWIND(27)

      DO JSUB = 1,NSUB

C  ADD TO SUMTOT THIS SUBJECT'S NORMALIZED SUM OF SQUARED DIFFERENCES
C  BETWEEN ITS OBSERVED AND PREDICTED VALUES (THESE PREDICTED VALUES
C  ARE BASED ON THE VALUES ESTABLISHED IN PX(.) (SEE ABOVE).

C  CALL SUBROUTINE FILRED TO STORE THE DATA FOR THIS SUBJECT INTO
C  COMMONS USED BY SUBROUTINE IDPC AND ITS SUBROUTINES. ALSO, THE
C  VALUES IN SIG(.) HAVE TO BE ESTABLISHED SINCE THESE VALUES ARE
C  PASSED IN THE BLANK COMMON TO SUBROUTINE FUNC. 

       CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,
     1    TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     2    INTLIST,errfilname)

        call cp_lrcs_to_rpar(gamma,flat,numeqt,c0,c1,c2,c3,c4,c5,rpar)

C  SEE COMMENTS IN THE 140 LOOP IN MAIN.

       NPOISSONOBS=0
       NNORMALOBS=0
C       MISVAL=0

C  Initialize IPAR w/type of observation declaration
       DO 140 I=1,NOBSER
        DO 140 J=1,NUMEQT

         Y = YO(I,J)

C  IF YO(I,J) = -99, IT MEANS THAT OUTPUT EQ. J HAD NO VALUE AT 
C  OBSERVATION TIME I. IN THIS CASE, SIG(I,J) WILL NOT BE SET, AND IT
C  OF COURSE WILL NOT BE NEEDED IN SUBROUTINE FUNC.
 
         IF(Y .EQ. -99) GO TO 140

C Are observations recorded as log10(obs) _AND_ in subroutine
C output, are X converted to log10(X)? If so, do you want the
C sd to be calculated on 10^Y(obs or est)?
C             if (C1(J).eq.-10) IPAR(i_is_log10+J) = -10
             if (C4(J).eq.10) IPAR(i_is_log10+J) = 10

C         if (C0(J).eq.-229.and.C2(J).eq.-229
C     1     .and.C3(J).eq.-229) then 
          if (C5(J).eq.229) then
C--------------------------------- Start Poisson

             write (*,*) "Poisson analysis req. for OUTEQ",J
             NPOISSONOBS=NPOISSONOBS+1
C             ObsError(I,J)=1.D0
             ObsError(I,J)=Y
C             SIG(I,J)=1.D0
             IPAR(i_is_poisson_obs+J)=229

C--------------------------------- End Poisson
         else
C--------------------------------- Start NORMAL

C         SIG(I,J) = C0(J)+C1(J)*Y+C2(J)*Y*Y+C3(J)*Y**3
C         if(ierrmod.eq.2) sig(i,j) = sig(i,j)*gamma
C         if(ierrmod.eq.3) sig(i,j)=dsqrt(sig(i,j)**2 + gamma**2)
C         if(ierrmod.eq.4) sig(i,j) = gamma*flat
         ObsError(I,J) = C0(J)+C1(J)*Y+C2(J)*Y*Y+C3(J)*Y**3
         if(ierrmod.eq.2) ObsError(i,j) = ObsError(i,j)*gamma
         if(ierrmod.eq.3) ObsError(i,j)=dsqrt(ObsError(i,j)**2
     &     + gamma**2)
         if(ierrmod.eq.4) ObsError(i,j) = gamma*flat

C         ObsError(I,J) = sig(I,J)

C--------------------------------- End NORMAL
         endif

  140    CONTINUE



C  CALL IDPC, A SUBROUTINIZED VERSION OF THE ADAPT PROGRAM ID3, TO
C  CALCULATE THE SUM OF SQUARES OF DIFFERENCES BETWEEN THE OBSERVED 
C  VALUES AND THE PREDICTED (BY THE MODEL) VALUES, FOR EACH OUTPUT
C  EQUATION, FOR THIS VARIABLE VECTOR, VEC. THESE SUM OF SQUARES ARE
C  EACH NORMALIZED BY THE ASSAY VARIANCE OF EACH OBSERVATION.

        NPX = NVAR+NOFIX+NRANFIX
C       if (NPX < max_ODE_params) then exit progam w/error
C wmy2017Sep12 Added /TOUSER/ varbs to CALL IDPC()
C       CALL IDPC(NPX,PX,W,NOBSER,NUMEQT)

C       write (*,*) "CALL IDPC",NPX,NOBSER,NUMEQT,NDIM,MF

        CALL IDPC(JSUB,IG,NPX,PX,NBCOMP,W,NOBSER,NUMEQT,
     1    NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     2    INTLIST,IPAR,ObsError,RPAR,ERRFILNAME)

C  W RETURNS AS THE SUM OF:
C  ((YO(I,J)-H(I,J))/SIG(I,J))**2, WHERE H(I,J) = PREDICTED VALUE OF THE
C  JTH OUTPUT EQ AT THE ITH OBSERVATION TIME, ASSUMING THE IGTH GRID
C  POINT, X, ... OVER THE NOBSER x NUMEQT QUANTITIES ABOVE WHICH DON'T
C  HAVE YO(I,J) = -99 (WHICH MEANS THAT OUTPUT EQ. J HAS NO OBSERVED
C  LEVEL FOR TIME I).

C  ADD THIS W TO SUMTOT.

       SUMTOT = SUMTOT + W
C
C Note that SUMTOT = sum(z-score^2) over all observations, including
C  those that do not arise from a Normal distribution
C

      END DO
C  THE ABOVE END DO IS FOR THE  DO JSUB = 1,NSUB  LOOP.

      FNTVAL = SUMTOT


	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE ELDERY(N,START,XMIN,YNEWLO,REQMIN,STEP,ITMAX,
     1    FUNC,IPRINT,ICONV,NITER,ICOUNT,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     2    NBCOMP,NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     3    INTLIST,IPAR,ObsError,RPAR,gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     4    errfilname)

C  ELDERY DIFFERS FROM ELDERX ONLY IN THE DIMENSION STATEMENT. ALL 5'S
C  ARE CHANGED TO 25'S, AND ALL 6'S ARE CHANGED TO 26'S. THIS ALLOWS 25
C  PARAMETERS INSTEAD OF JUST 5. As of itbig9x.f, we allow as many as
C  max_pop_rand_varbs parameters.

C  ELDERX DIFFERS FROM ELDER (DESCRIBED BELOW) ONLY IN THAT N, THE
C  DIMENSION OF START (THE NO. OF UNKNOWN PARAMETERS OVER WHICH THE
C  MINIMIZATION IS DONE) IS PASSED TO THE SUBROUTINE FUNC IN THE CALLING
C  STATEMENTS.
C
C  ELDER IS A PROGRAM TO MINIMIZE A FUNCTION USING THE NELDER-MEED
C  ALGORITM.
C    THE CODE WAS ADAPTED FROM A PROG. IN J. OF QUALITY TECHNOLOGY VOL. 
C    JAN. 1974. BY D.M. OLSSON.
C  CALLING ARGUMENTS:
C    N     -NUMBER OF UNKNOWN PARAMS. UP TO 99.
C    START -A VECTOR WITH THE INITIAL QUESSES OF THE SOLUTION PARAMS.
C    ITMAX -THE MAXIMUM NUMBER OF ITERATIONS.
C             (KCOUNT IS THE MAX NUM OF FUNC CALLS.SET AT 1000000)
C    STEP  -THE STEP SIZE VECTOR FOR DEFINING THE N ADDITIONAL 
C             VERTICIES.
C    REQMIN-THE STOP TOLERANCE.

C    XMIN   -THE SOLUTION VECTOR.
C    YNEWLO-THE FUCTION VALUE AT XMIN.
C    IPRINT-SWITCH WHICH DETERMINES IF INTERMEDIATE ITERATIONS
C              ARE TO BE PRINTED. (0=NO,1=YES).
C    ICONV -FLAG INDICATING WHETHER OR NOT CONVERGENCE HAS BEEN
C             ACHEIVED. (0=NO,1=YES).
C    NITER -THE NUMBER OF ITERATIONS PERFORMED.
C    ICOUNT-THE NUMBER OF FUNCTION EVALUATIONS.
C    FUNC  -THE NAME OF THE SUBROUTINE DEFINING THE FUNCTION.
C             THIS SUBROUTINE MUST EVALUATE THE FUNCTION GIVEN A
C             VALUE FOR THE PARAMETER VECTOR. THE ROUTINE IS OF
C             THE FOLLOWING FORM:
C               FUNC(P,FV), WHERE P IS THE PARAMETER VECTOR,
C                             AND FV IS THE FUNCTION VALUE.
C  A SUBROUTINE TO PRINT THE RESULTS OF ITERMEDIATE ITERATIONS
C    MUST ALSO BE SUPPLIED. ITS NAME AND CALLING SEQUENCE ARE 
C    DEFINED AS FOLLOWS:
C      PRNOUT(P,N,NITER,NFCALL,FV).
C  OTHER PROGRAM VARIABLES OF INTEREST ARE;
C    XSEC  -THE COORDINATES OF THE VERTEX WITH THE 2ND SMALLEST FUNCTION
C             VALUE.
C    YSEC  - THE FUNCTION VALUE AT XSEC.
C
      use npag_utils, only: maxnumeq,max_m_per_obs,max_ODE_params
     1  ,max_pop_rand_varbs,max_doses,max_ODE_comps,max_RS_J
     2  ,max_input_dim

C      IMPLICIT REAL*8(A-H,O-Z)
      implicit none

C ARGUMENT LIST
C
      integer N
      double precision, dimension(N) :: START, XMIN
      double precision YNEWLO, REQMIN
      double precision, dimension(N) :: STEP
      integer ITMAX
      EXTERNAL FUNC
      integer IPRINT, ICONV, NITER, ICOUNT, NUMEQT 
      double precision, dimension(max_m_per_obs,NUMEQT) :: YO
      double precision, dimension(NUMEQT) :: C0,C1,C2,C3,C4,C5
      integer, dimension(max_input_dim) :: NBCOMP
      integer NDIM, MF
      double precision RTOL
      double precision, dimension(max_ODE_comps) :: ATOL
      double precision, dimension(max_m_per_obs) :: TIMCOPY
      double precision, dimension(max_doses) :: SIGCOPY
      double precision, dimension(max_doses,max_RS_J) :: RSCOPY
      double precision, dimension(max_doses,max_input_dim) :: BSCOPY
      integer, dimension(128) :: INTLIST
      integer, dimension(257) :: IPAR
      double precision, dimension(max_m_per_obs,MAXNUMEQ) :: ObsError
      double precision, dimension(257) :: RPAR
      double precision gamma, flat
      double precision, dimension(max_pop_rand_varbs,2) :: AB
      double precision, dimension(max_ODE_params) :: PX
      integer, dimension(max_ODE_params) :: IRAN
      integer NOFIX,NSUB
      character*20 errfilname

C Local Variables

        integer I,ILO,IHI,IBEST,ISEC
        integer J,KCOUNT,KONVGE,L,NN
        double precision, dimension(max_pop_rand_varbs) :: XSEC,
     1   PSTAR,P2STAR,PBAR
        double precision, dimension(max_pop_rand_varbs,
     1   max_pop_rand_varbs+1) :: P
        double precision, dimension(max_pop_rand_varbs+1) :: Y
        double precision DABIT,BIGNUM,YLO,YOLDLO,YSEC,YSTAR,Y2STAR
        double precision XN,DN,FN,DCHK,Z,RCOEFF,ECOEFF,CCOEFF
        double precision COORD1, COORD2

C !$omp ThreadPrivate( IPAR, ObsError )

        DATA RCOEFF/1.0D0/,ECOEFF/2.0D0/,CCOEFF/.5D0/

        KCOUNT=1000000
        ICOUNT=0
        NITER=0
        ICONV=0
C
C  CHECK INPUT DATA AND RETURN IF AN ERROR IS FOUND.
C
        IF(REQMIN.LE.0.0D0) ICOUNT=ICOUNT-1
        IF(N.LE.0) ICOUNT=ICOUNT-10
        IF(N.GT.99) ICOUNT=ICOUNT-10

        IF(ICOUNT.LT.0) RETURN
C
C  SET INITIAL CONSTANTS
C
        DABIT=2.04607D-35
        BIGNUM=1.0D+38
        KONVGE=5
C        XN=FLOAT(N)
C        DN=FLOAT(N)
        XN = 1.d0 * N
        DN = 1.d0* N
        FN = 0.d0
        NN=N+1
C
C  CONSTRUCTION OF INITIAL SIMPLEX.
C
1001    DO 1 I=1,N
1       P(I,NN)=START(I)
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     2    NBCOMP,NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,
     3    RSCOPY,BSCOPY,INTLIST,IPAR,ObsError,RPAR,
     4    gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     5    errfilname)
        Y(NN)=FN
        ICOUNT=ICOUNT+1
C       CALL PRNOUT(START,N,NITER,ICOUNT,FN)
        IF(ITMAX.NE.0) GO TO 40
        DO 45 I=1,N
45      XMIN(I)=START(I)
        YNEWLO=FN
        RETURN
40      DO 2 J=1,N
        DCHK=START(J)
        START(J)=DCHK+STEP(J)
        DO 3 I=1,N
3       P(I,J)=START(I)
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     2    NBCOMP,NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,
     3    RSCOPY,BSCOPY,INTLIST,IPAR,ObsError,RPAR,
     4    gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     5    errfilname)
        Y(J)=FN
        ICOUNT=ICOUNT+1
2       START(J)=DCHK

C  SIMPLEX CONSTRUCTION COMPLETE.

C
C    FIND THE HIGHEST AND LOWEST VALUES. YNEWLO (Y(IHI)) INDICATES THE
C     VERTEX OF THE SIMPLEX TO BE REPLACED.
C
1000    YLO=Y(1)
        YNEWLO=YLO
        ILO=1
        IHI=1
        DO 5 I=2,NN
        IF(Y(I).GE.YLO) GO TO 4
        YLO=Y(I)
        ILO=I
4       IF(Y(I).LE.YNEWLO) GO TO 5
        YNEWLO=Y(I)
        IHI=I
5       CONTINUE
C
        IF(ICOUNT.LE.NN) YOLDLO=YLO
        IF(ICOUNT.LE.NN) GO TO 2002
        IF(YLO.GE.YOLDLO) GO TO 2002
        YOLDLO=YLO
        NITER=NITER+1
        IF(NITER.GE.ITMAX) GO TO 900
        IF(IPRINT.EQ.0) GO TO 2002
C       CALL PRNOUT(P(1,ILO),N,NITER,ICOUNT,YLO)
C
C  PERFORM CONVERGENCE CHECKS ON FUNCTIONS.
C
2002    DCHK=(YNEWLO+DABIT)/(YLO+DABIT)-1.0D0
        IF(DABS(DCHK).GT. REQMIN) GO TO 2001
        ICONV=1
        GO TO 900
C
2001    KONVGE=KONVGE-1
        IF(KONVGE.NE.0) GO TO 2020
        KONVGE=5
C
C  CHECK CONVERGENCE OF COORDINATES ONLY EVERY 5 SIMPLEXES.
C
        DO 2015 I=1,N
        COORD1=P(I,1)
        COORD2=COORD1
        DO 2010 J=2,NN
        IF(P(I,J).GE.COORD1) GO TO 2005
        COORD1=P(I,J)
2005    IF(P(I,J).LE.COORD2) GO TO 2010
        COORD2=P(I,J)
2010    CONTINUE
        DCHK=(COORD2+DABIT)/(COORD1+DABIT)-1.0D0
        IF(DABS(DCHK).GT.REQMIN) GO TO 2020
2015    CONTINUE
        ICONV=1
        GO TO 900
2020    IF(ICOUNT.GE.KCOUNT) GO TO 900
C
C  CALCULATE PBAR, THE CENTRIOD OF THE SIMPLEX VERTICES EXCEPTING THAT
C   WITH Y VALUE YNEWLO.
C

        DO 7 I=1,N
        Z=0.0D0
        DO 6 J=1,NN
6       Z=Z+P(I,J)
        Z=Z-P(I,IHI)
7       PBAR(I)=Z/DN
C
C  REFLECTION THROUGH THE CENTROID.
C
        DO 8 I=1,N
8       PSTAR(I)=(1.0D0+RCOEFF)*PBAR(I)-RCOEFF*P(I,IHI)
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     2    NBCOMP,NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,
     3    RSCOPY,BSCOPY,INTLIST,IPAR,ObsError,RPAR,
     4    gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     5    errfilname)
        YSTAR=FN
        ICOUNT=ICOUNT+1
        IF(YSTAR.GE.YLO) GO TO 12
        IF(ICOUNT.GE.KCOUNT) GO TO 19
C
C  SUCESSFUL REFLECTION SO EXTENSION.
C
        DO 9 I=1,N
9       P2STAR(I)=ECOEFF*PSTAR(I)+(1.0D0-ECOEFF)*PBAR(I)
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     2    NBCOMP,NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,
     3    RSCOPY,BSCOPY,INTLIST,IPAR,ObsError,RPAR,
     4    gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     5    errfilname)
        Y2STAR=FN
        ICOUNT=ICOUNT+1
C
C  RETAIN EXTENSION OR CONTRACTION.
C
        IF(Y2STAR.GE.YSTAR) GO TO 19
10      DO 11 I=1,N
11      P(I,IHI)=P2STAR(I)
        Y(IHI)=Y2STAR
        GO TO 1000
C
C  NO EXTENSION.
C
12      L=0
        DO 13 I=1,NN

        IF(Y(I).GT.YSTAR) L=L+1
13      CONTINUE
        IF(L.GT.1) GO TO 19
        IF(L.EQ.0) GO TO 15
C
C  CONTRACTION ON REFLECTION SIDE OF CENTROID.
C
        DO 14 I=1,N
14      P(I,IHI)=PSTAR(I)
        Y(IHI)=YSTAR
C
C  CONTRACTION ON THE Y(IHI) SIDE OF THE CENTROID.
C
15      IF(ICOUNT.GE.KCOUNT) GO TO 900
        DO 16 I=1,N
16      P2STAR(I)=CCOEFF*P(I,IHI)+(1.0D0-CCOEFF)*PBAR(I)
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     2    NBCOMP,NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,
     3    RSCOPY,BSCOPY,INTLIST,IPAR,ObsError,RPAR,
     4    gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     5    errfilname)
        Y2STAR=FN
        ICOUNT=ICOUNT+1
        IF(Y2STAR.LT.Y(IHI)) GO TO 10
C
C  CONTRACT THE WHOLE SIMPLEX
C
        DO 18 J=1,NN
        DO 17 I=1,N
        P(I,J)=(P(I,J)+P(I,ILO))*0.5D0
17      XMIN(I)=P(I,J)
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     2    NBCOMP,NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,
     3    RSCOPY,BSCOPY,INTLIST,IPAR,ObsError,RPAR,
     4    gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     5    errfilname)
        Y(J)=FN
18      CONTINUE
        ICOUNT=ICOUNT+NN
        IF(ICOUNT.LT.KCOUNT) GO TO 1000
        GO TO 900
C
C  RETAIN REFLECTION.

C
19      CONTINUE
        DO 20 I=1,N
20      P(I,IHI)=PSTAR(I)
        Y(IHI)=YSTAR
        GO TO 1000
C
C  SELECT THE TWO BEST FUNCTION VALUES (YNEWLO AND YSEC) AND THEIR
C    COORDINATES (XMIN AND XSEC)>
C
900     DO 23 J=1,NN
        DO 22 I=1,N
22      XMIN(I)=P(I,J)
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     2    NBCOMP,NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,
     3    RSCOPY,BSCOPY,INTLIST,IPAR,ObsError,RPAR,
     4    gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     5    errfilname)
        Y(J)=FN
23      CONTINUE
        ICOUNT=ICOUNT+NN
        YNEWLO=BIGNUM
        DO 24 J=1,NN
        IF(Y(J).GE.YNEWLO) GO TO 24
        YNEWLO=Y(J)
        IBEST=J
24      CONTINUE
        Y(IBEST)=BIGNUM
        YSEC=BIGNUM
        DO 25 J=1,NN

        IF(Y(J).GE.YSEC) GO TO 25
        YSEC=Y(J)
        ISEC=J
25      CONTINUE
        DO 26 I=1,N
        XMIN(I)=P(I,IBEST)
        XSEC(I)=P(I,ISEC)
26      CONTINUE

        RETURN
        END













CCC
C
C wmy2017Oct12
C
C Contains emint and supporting LAPACK and BLAS routines that make
C     sense to put here
C
C

      subroutine emint(psi,ldpsi,theta,ldtheta,npoint,nsub,ijob,
     &                 x,dx,y,dy,fobj,gap,nvar,keep,IHESS,ERRFIL,
     &                 isupres)

      use npag_utils, only: verifyval

      implicit real*8 (a-h,o-z)
      integer ldpsi,ldtheta,npoint,nsub,ijob,nvar,IHESS
      dimension psi(ldpsi,*),theta(ldtheta,*),x(*),dx(*),y(*),dy(*)
      CHARACTER ERRFIL*20
      integer isupres

C     COMMON/SUPRES/ISUPRES
C  COMMON/SUPRES IS SUPPLIED FROM SUBROUTINE BIGNPAG.

C      COMMON/ERR/ERRFIL 

      real*8 mu

c This subroutine solves the 'EM' problem of maximizing the function
 
c   fobj(x) = sum_i (log[sum_j ( psi(i,j) * x(j)) ]  ),
c             j=1,..,npoint and i=1,...,nsub
c   subject to: x(j) >= 0, sum_j x(j) = 1 (i.e. x is a probability
c             vector of length npoint)
c   where psi(i,j) is a fixed non-negative data array representing the
c   likelihood of point j for subject i
 
c inputs: psi,ldpsi,npoint,nsub,nvar
c psi contains the likelihood vectors for each subject - the i-th
c row of psi is likelikhood vector for subject i.  Thus psi(i,j) is
c likelihood of the j-th point for c the i-th subject.  The input value
c ldpsi is the 'leading dimension of psi' - i.e. the first dimension of the
c array psi as dimensioned in the calling program.
c
c input work arrays: dx(*), y(*), dy(*) - should be at least large enough to
c contain npoint points, as should the probabiltiy array x(*)

c
c outputs: x(*), fobj
c x(i) is  final probability for point i
c fobj - optimal value of the objective function
 
c note - usually npoint is much larger than nsub; here we dimension

c some internal work arrays with the maximum expected number of subjects
c MAXSUBem and the maximum number of points MAXACTem
c are be set in the parameter statement


      parameter (MAXSUBem=999,MAXACTem=10000000)
      dimension w(MAXSUBem),dw(MAXSUBem),Ptx(MAXSUBem),
     &          hess(MAXSUBem,2*MAXSUBem)
      dimension psisum(MAXSUBem),XVERIFY(100)


      integer kpvt(MAXSUBem), ipivot(MAXACTem), list(MAXACTem)

C       DATA ILOOP/0/
C  ILOOP IS USED BELOW TO KNOW WHEN TO WRITE MESSAGE TO USER IN CASE

C  THE OPTIMIZATION TAKES A 'LONG' TIME.


c here w(*) is a vector if `dual variables'
c dw(*) is a calculated change (as a Newton step) in w(*)
c Ptx(*) (Psi times x) is the vector Ptx(j) = sum_i Psi(j,i)*x(i)
c first , perform some dimension checks to make sure no internal dimensions
c are exceeded

c 2018Aug20 -- This is a bug -- or is the last part of code that got
c   accidently erased! NACTVE is not even passed into emint() Set keep=0;
c   although this is done later; so no need to do it here.
c      keep = nactve
       keep = 0

      write (*,*) "Enterred emint(); ijob =",ijob
C      if (ldpsi.gt.21) write (*,*) "On entry, Psi[3,11; 9,18] =",
C     & psi(3,9),psi(3,18),psi(11,9),psi(11,18)
      

      if(nsub.gt.MAXSUBem) then

      write(6,*) 'nsub =',nsub, ' is greater than MAXSUBem=',MAXSUBem
      write(6,*) 'MAXSUBem needs to be reset as large as nsub'
      write(6,*) 'in PARAMETER statement in subroutine emint'

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
      write(42,*) 'nsub =',nsub, ' is greater than MAXSUBem=',MAXSUBem
      write(42,*) 'MAXSUBem needs to be reset as large as nsub'
      write(42,*) 'in PARAMETER statement in subroutine emint'
        CLOSE(42)

      CALL PAUSE
      stop

      endif


      if(npoint.gt.MAXACTem) then

      write(6,*) 'npoint=',npoint,' is larger than MAXACTem=',MAXACTem
      write(6,*) 'MAXACTem needs to be reset as large as npoint'
      write(6,*) 'in PARAMETER statement in subroutine emint'

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
      write(42,*) 'npoint=',npoint,' is larger than MAXACTem=',MAXACTem
      write(42,*) 'MAXACTem needs to be reset as large as npoint'
      write(42,*) 'in PARAMETER statement in subroutine emint'
        CLOSE(42)

      CALL PAUSE
      stop

      endif


c     Second, check that psi is non-negative
      psimin=0.
      do j=1,nsub
      do i=1,npoint
      if(psi(j,i).le.psimin) psimin=psi(j,i)
      enddo
      enddo

      if(psimin.lt.0) then

        write(6,*) 'Psi matrix not non-negative -stop'

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
        write(42,*) 'Psi matrix not non-negative -stop'
        CLOSE(42)

        CALL PAUSE
        stop

      endif

c     Third,check that the row sums of psi are positive - no zero rows
c     also initialize x and w
      colsummin=1.e10
      do j=1,nsub
        s=0.
        do i=1,npoint
           x(i)=1.d0
           s=s+psi(j,i)
        enddo
      psisum(j) = s
        Ptx(j)=s
        if(s.le.colsummin) colsummin=s

        if(s.le.0) then

           write(6,*) 'psi has a zero row -stop'

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)

        write(42,*) 'psi has a zero row -stop'
        CLOSE(42)

        CALL PAUSE
        stop

        endif

        w(j)=1./s
      enddo
c     calc ptw = w'*psi
      shrink=0.
      do i=1,npoint
        sum=0.d0
        do j=1,nsub
           sum=sum+psi(j,i)*w(j)
        enddo
        y(i)=sum
        if(sum.gt.shrink) shrink=sum
      enddo
      shrink=2.d0*shrink

      if(s.le.0) then

        write(6,*) 'Psi has a zero column -stop'

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
        write(42,*) 'Psi has a zero column -stop'
        CLOSE(42)

        CALL PAUSE
        stop

      endif

c     stopping tolerance
      eps=1.d-10
      sig=0.d0
      mu=0.d0
      do i=1,npoint
c       x = x*shrink;
        x(i)=1.d0*shrink
c       Ptw  = Ptw/shrink;
        y(i)=y(i)/shrink
c       y  = ecol-Ptw;
        y(i)=1.d0-y(i)
c       mu  =  (x'*y)/npoint;
        mu=mu+x(i)*y(i)
      enddo
      mu=mu/npoint
      rmax = -1.e38
      do j=1,nsub
c       w                       =  w/shrink;
        w(j)=w(j)/shrink
c       Plam            =  Plam*shrink;


        Ptx(j)=Ptx(j)*shrink
c
c       R =  erow-w.*Plam;
        if(dabs(1.d0-w(j)*Ptx(j)).ge.rmax) rmax =
     &  dabs(1.d0-w(j)*Ptx(j))

      enddo
      gap=1.d0
c start of iterations
      iter=0
100   continue
c     following is iteration termination condition


      conval = mu
      if(conval .lt. rmax) conval = rmax

      if(conval .lt. gap) conval = gap
      convcrit = eps/conval 

      XVERIFY(1) = convcrit
      CALL VERIFYVAL(1,XVERIFY)
    
C      IF(ILOOP .GT. 0) WRITE(*,123) iter,convcrit
C       IF(ILOOP .GT. 0) WRITE(*,123) iter,XVERIFY(1)
C wmy2017Sep09 ... seems like we need this from step 0
       WRITE(*,123) iter,XVERIFY(1)

  123 FORMAT(' Iteration ',I9,' CONV. CRIT = ',G15.2,' (1 OR HIGHER FOR 
     1CONVERGENCE)')

C  ABOVE WRITE STATEMENT ADDED IN bigmlt12.f SO THE USER WILL KNOW
C  THE PROGRAM HAS NOT 'HUNG' IF THE OPTIMIZATION TAKES A 'LONG' TIME.


      if(mu.le.eps.and.rmax.le.eps.and.gap.le.eps) go to 9000
      iter=iter+1

        ILOOP = ILOOP + 1

      tbuilda=0
      smu=sig*mu
c     zero out hessian
      do j=1,nsub
        do k=1,nsub
           hess(j,k)=0.
        enddo
      enddo
c do outer product portion of Hessian
      do i=1,npoint
        scale=x(i)/y(i)
       do j=1,nsub
         fact=scale*psi(j,i)
         do k=j,nsub
           hess(k,j)=hess(k,j)+fact*psi(k,i)
         enddo
       enddo
      enddo
      do j=1,nsub-1
      do k=j+1,nsub

      hess(j,k)=hess(k,j)
      enddo
      enddo
c do diagonal portion of hessian
      do j=1,nsub
        hess(j,j)=hess(j,j)+Ptx(j)/w(j)
      enddo
      tbuildb=0
      tbuild=tbuildb-tbuilda

      IF(ISUPRES .EQ. 0) write(6,*) 'tbuild=',tbuild

c now do cholesky decomposition-for time bing, use simple dpofa
c from LINPACK
c     call dpofa(hess,MAXSUBem,nsub,info)

c     call dsifa(hess,MAXSUBem,nsub,kpvt,info)
c note dpofa is cholesky factorization routine from LINPACK
c      dsifa is symmetric indefintie factorization routine from LINAPCK
c      DPOTRF is Cholesky factorization routine from LAPACK
c DPOTRF is fastest of the three, but DSIFA may be more reliable for
c nearly singular cases
c Regardless of which of the three routines is used, it must be matched
c with the proper solve routine (dposl for dpofa, dsisl for dsifa, 
c DPOTRS for DPOTRF below


      CALL DPOTRF( 'L', nsub, hess, MAXSUBem, INFO, ERRFIL )
      tbuildc=0
      tfactor=tbuildc-tbuildb

      IF(ISUPRES .EQ. 0) write(6,*) 'tfactor=',tfactor

cdebug
      write(6,*) 'gap,info,tfactor=',gap,info,tfactor


c  As of npageng18.f, set IHESS = 0. If info .ne. 0, reset it = -1 and,
c  after writing the indicated message to the screen (and also now to 
c  the output file), return to MAIN, where IHESS = -1 tells the program
c  to create the output files before stopping (previously, if 
c  info .ne. 0, the program would simply stop after writing the 
c  indicated message to the screen).

      IHESS = 0


      if(info .ne. 0) then

       IHESS = -1


       WRITE(25,163)
       WRITE(*,163)
  163  FORMAT(//' Hessian matrix in interior point EM algorithm'/
     1' is singular.  Possibly number of grid points is too small,'/
     2' or assay coefficients are too large. '//
     3' Try again with a new assay polynomial or larger grid.'//
     4' Suggested quick fix: rerun and select error model 2)'/
     5' in response to the initial question; then enter a'/
     6' initial value gamma = 10.0 in response to the prompt for'/
     7' that value.'//
     8' THE PROGRAM WILL CREATE OUTPUT FILES BEFORE STOPPING. '//)

      WRITE(25,164) info
      WRITE(*,164) info
  164 FORMAT(//' NOTE THAT IN SUBROUTINE emint, THE VALUE OF INFO'/
     1' IS ',i6,//
     2' IF THIS VALUE IS POSTIVE, IT IS LIKELY THE NO. OF THE SUBJECT'/
     3' (OR AT LEAST THE FIRST SUBJECT) WHICH CAUSED THE HESSIAN '/
     4' ERROR. SO IN THIS CASE, YOU MIGHT ALSO WANT TO EXAMINE THE'/
     5' DATA IN THIS SUBJECT TO VERIFY THEY ARE CORRECT.'//)
      
c  As of npageng22.f, the following PAUSE is commented out ... since
c  it --> the program will not complete properly if it is run under
c  Pmetrics (which cannot supply a keyboard response during a run).
c     CALL PAUSE

c  As of npageng18.f, the program does not stop here; it returns to 
c  MAIN to write out the output files and then stops.

C  AS of npageng23.f, SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE
C  THE ERROR MESSAGE TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,163) 
         WRITE(42,164) info
        CLOSE(42)


      return

      endif



c construct rhs for linear equation system
      do j=1,nsub
        sum=0.d0
        do i=1,npoint

          sum=sum+psi(j,i)*smu/y(i)
        enddo
        dw(j)=1.d0/w(j)-sum
      enddo
c now solve linear system with LINPACK routine dposl
c and put answer in dw
c note - these routines match the factor routines dpofa, dsifa, and DPOTRF, respectively
c see note about 15 lines back where the factor routine is called
c     call dposl(hess,MAXSUBem,nsub,dw)

c     call dsisl(hess,MAXSUBem,nsub,kpvt,dw)
      call DPOTRS( 'L', nsub, 1, hess, MAXSUBem, dw, nsub,INFO,ERRFIL)
c now compute dy and dx from dw

      do i=1,npoint
        sum=0.
        do j=1,nsub
          sum=sum+psi(j,i)*dw(j)
        enddo
        dy(i)=-sum
        dx(i)=smu/y(i)-x(i)-dy(i)*x(i)/y(i)
      enddo
c damp the Newton step
      alfpri=-.5
      do i=1,npoint
        if(dx(i)/x(i).le.alfpri) alfpri=dx(i)/x(i)
      enddo
      alfpri=-1.d0/alfpri
      alfpri=min(1.d0,0.99995*alfpri)
      alfdual=-0.5d0
      do i=1,npoint
        if(dy(i)/y(i).le.alfdual) alfdual=dy(i)/y(i)
      enddo
      alfdual=-1.d0/alfdual

      alfdual=min(1.d0,0.99995*alfdual)
      mu=0.d0
      do i=1,npoint
        x(i)=x(i)+alfpri*dx(i)
        y(i)=y(i)+alfdual*dy(i)
        mu=mu+x(i)*y(i)
      enddo
      mu=mu/npoint
      do j=1,nsub
        sum=0.d0
        do i=1,npoint
          sum=sum+psi(j,i)*x(i)
        enddo


        Ptx(j)=sum
      enddo
      do j=1,nsub
        w(j)=w(j)+alfdual*dw(j)

      enddo
c compute rmax (norm(r,inf)-note we don't really need to compute r
      rmax=0.
      do j=1,nsub
        rtest=1.d0-w(j)*Ptx(j)
        if(dabs(rtest).gt.rmax) rmax=dabs(rtest)
      enddo
      sumlogw=0.d0
      sumlgPtx=0.d0
      do j=1,nsub
        sumlogw=sumlogw+dlog(w(j))
        sumlgPtx=sumlgPtx+dlog(Ptx(j))
      enddo
      gap = dabs(sumlogw+sumlgPtx)/(1.d0+dabs(sumlgPtx))
      if(mu.lt.eps.and.rmax.gt.eps) then
        sig=1.d0
      else
        c2=1.d2
        term1=(1.d0-alfpri)**2
        term2=(1.d0-alfdual)**2
        term3=(rmax-mu)/(rmax+c2*mu)
        term=max(term1,term2)
        term=max(term,term3)
        sig=min(0.3d0,term)
      endif
      sumx=0.d0
      do i=1,npoint
        sumx=sumx+x(i)
      enddo
      fobj=0.
      do j=1,nsub
        fobj=fobj+dlog(Ptx(j)/sumx)
      enddo
      go to 100
c following is exit point
9000  continue
c finish by normalizing x to sum to 1.
c fobj has already been computed
      sumx=0.
      do i=1,npoint
      sumx=sumx+x(i)
      enddo
      do i=1,npoint
      x(i)=x(i)/sumx
      enddo
c finished if ijob=0
      if(ijob.eq.0) return
      isum=0
      xlim=0.
      do i=1,npoint
      if(x(i).gt.xlim) xlim=x(i)
      enddo
      xlim=xlim*1.d-3
      isum = 0
      do i=1,npoint
      if(x(i).gt.xlim) then
        isum = isum + 1
        list(isum) = i
        do j=1,nsub
        psi(j,isum) = psi(j,i)
        enddo
cpull
c now condense the original density grid
      do j=1,nvar
      theta(isum,j)=theta(i,j)
      enddo
      x(isum)=x(i)
      endif
      enddo
      job=1
      do k=1,npoint
      ipivot(k)=0
      enddo
c save a copy of psi after current end of psi
      do i=1,isum

      do j=1,nsub
      psi(j,i+isum)=psi(j,i)
      enddo
      enddo
      do i=1,isum
      do j=1,nsub
      psi(j,i) = psi(j,i)/psisum(j)
      enddo
      enddo
      call dqrdc(psi,ldpsi,nsub,isum,y,ipivot,dy,job)

      keep = 0
      limloop = nsub
      if(isum.lt.nsub) limloop = isum
      do i=1,limloop
        test=dnrm2(i,psi(1,i),1)
cdebug  write(6,*) i,psi(i,i),test,psi(i,i)/test
      if(dabs(psi(i,i)/test).ge.1.d-8) keep=keep+1
      enddo


C wmy2017Sep15 debug
C      write (*,*) "R :: "
C      do i=1,limloop
C         do j=i,limloop
C            write(*, '(D12.6,X)', advance='no') psi(j,i)
C         end do
C         write(*, *) ipivot(i)
C      end do

c sort ipivot to avoid collisions during condensing

      if(isum.gt.1) then
      do i=1,keep-1
      do j=i,keep
      if(ipivot(i)*ipivot(j).ne.0.and.ipivot(i).gt.ipivot(j)) then
         itemp=ipivot(i)
         ipivot(i)=ipivot(j)

         ipivot(j)=itemp
      endif
      enddo
      enddo
      endif

c restore psi
      do i=1,isum
      do j=1,nsub
      psi(j,i)=psi(j,i+isum)
      enddo
      enddo
 
C      CALL PAUSE
      do k=1,npoint
      dx(k)=0
      enddo
      sumkeep = 0.
      do k=1,keep
      j=ipivot(k)

      if(j.ne.0) then
         do jj=1,nsub
         psi(jj,k)=psi(jj,j)
         enddo
      do jvar=1,nvar
      theta(k,jvar) = theta(j,jvar)
      enddo
      endif
      if(j.gt.0) dx(list(j))=1.
      if(j.gt.0) sumkeep = sumkeep + x(list(j))
      if(j.gt.0) w(k)=x(list(j))
      enddo

C wmy2017Sep14
        write (*,*) 
        write (*,*) "isum,keep", isum, keep
        write (*,*) 

      return
      end

      subroutine dpoco(a,lda,n,rcond,z,info)
      integer lda,n,info
      double precision a(lda,1),z(1)
      double precision rcond
c
c     dpoco factors a double precision symmetric positive definite
c     matrix and estimates the condition of the matrix.
c
c     if  rcond  is not needed, dpofa is slightly faster.
c     to solve  a*x = b , follow dpoco by dposl.
c     to compute  inverse(a)*c , follow dpoco by dposl.

c     to compute  determinant(a) , follow dpoco by dpodi.
c     to compute  inverse(a) , follow dpoco by dpodi.
c

c     on entry
c
c        a       double precision(lda, n)
c                the symmetric matrix to be factored.  only the
c                diagonal and upper triangle are used.
c

c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c     on return
c
c        a       an upper triangular matrix  r  so that  a = trans(r)*r

c                where  trans(r)  is the transpose.
c                the strict lower triangle is unaltered.
c                if  info .ne. 0 , the factorization is not complete.
c
c        rcond   double precision
c                an estimate of the reciprocal condition of  a .
c                for the system  a*x = b , relative perturbations
c                in  a  and  b  of size  epsilon  may cause
c                relative perturbations in  x  of size  epsilon/rcond .
c                if  rcond  is so small that the logical expression
c                           1.0 + rcond .eq. 1.0
c                is true, then  a  may be singular to working
c                precision.  in particular,  rcond  is zero  if
c                exact singularity is detected or the estimate
c                underflows.  if info .ne. 0 , rcond is unchanged.
c
c        z       double precision(n)
c                a work vector whose contents are usually unimportant.
c                if  a  is close to a singular matrix, then  z  is
c                an approximate null vector in the sense that
c                norm(a*z) = rcond*norm(a)*norm(z) .
c                if  info .ne. 0 , z  is unchanged.
c
c        info    integer
c                = 0  for normal return.
c                = k  signals an error condition.  the leading minor
c                     of order  k  is not positive definite.
c
c     linpack.  this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     linpack dpofa


c     blas daxpy,ddot,dscal,dasum
c     fortran dabs,dmax1,dreal,dsign
c
c     internal variables
c
      double precision ddot,ek,t,wk,wkm
      double precision anorm,s,dasum,sm,ynorm
      integer i,j,jm1,k,kb,kp1
c
c
c     find norm of a using only upper half
c

      do 30 j = 1, n
         z(j) = dasum(j,a(1,j),1)
         jm1 = j - 1
         if (jm1 .lt. 1) go to 20
         do 10 i = 1, jm1
            z(i) = z(i) + dabs(a(i,j))
   10    continue

   20    continue
   30 continue
      anorm = 0.0d0
      do 40 j = 1, n
         anorm = dmax1(anorm,z(j))
   40 continue
c
c     factor
c
      call dpofa(a,lda,n,info)
      if (info .ne. 0) go to 180
c
c        rcond = 1/(norm(a)*(estimate of norm(inverse(a)))) .
c        estimate = norm(z)/norm(y) where  a*z = y  and  a*y = e .
c        the components of  e  are chosen to cause maximum local
c        growth in the elements of w  where  trans(r)*w = e .
c        the vectors are frequently rescaled to avoid overflow.

c
c        solve trans(r)*w = e
c
         ek = 1.0d0
         do 50 j = 1, n
            z(j) = 0.0d0
   50    continue
         do 110 k = 1, n
            if (z(k) .ne. 0.0d0) ek = dsign(ek,-z(k))
            if (dabs(ek-z(k)) .le. a(k,k)) go to 60
               s = a(k,k)/dabs(ek-z(k))
               call dscal(n,s,z,1)
               ek = s*ek
   60       continue
            wk = ek - z(k)
            wkm = -ek - z(k)
            s = dabs(wk)
            sm = dabs(wkm)
            wk = wk/a(k,k)
            wkm = wkm/a(k,k)
            kp1 = k + 1
            if (kp1 .gt. n) go to 100
               do 70 j = kp1, n
                  sm = sm + dabs(z(j)+wkm*a(k,j))
                  z(j) = z(j) + wk*a(k,j)
                  s = s + dabs(z(j))
   70          continue
               if (s .ge. sm) go to 90
                  t = wkm - wk
                  wk = wkm
                  do 80 j = kp1, n
                     z(j) = z(j) + t*a(k,j)
   80             continue
   90          continue
  100       continue
            z(k) = wk
  110    continue
         s = 1.0d0/dasum(n,z,1)
         call dscal(n,s,z,1)
c
c        solve r*y = w
c
         do 130 kb = 1, n

            k = n + 1 - kb
            if (dabs(z(k)) .le. a(k,k)) go to 120

               s = a(k,k)/dabs(z(k))
               call dscal(n,s,z,1)
  120       continue
            z(k) = z(k)/a(k,k)
            t = -z(k)
            call daxpy(k-1,t,a(1,k),1,z(1),1)
  130    continue
         s = 1.0d0/dasum(n,z,1)
         call dscal(n,s,z,1)
c
         ynorm = 1.0d0
c
c        solve trans(r)*v = y
c
         do 150 k = 1, n
            z(k) = z(k) - ddot(k-1,a(1,k),1,z(1),1)
            if (dabs(z(k)) .le. a(k,k)) go to 140
               s = a(k,k)/dabs(z(k))

               call dscal(n,s,z,1)
               ynorm = s*ynorm
  140       continue
            z(k) = z(k)/a(k,k)
  150    continue
         s = 1.0d0/dasum(n,z,1)
         call dscal(n,s,z,1)
         ynorm = s*ynorm


c
c        solve r*z = v
c
         do 170 kb = 1, n
            k = n + 1 - kb
            if (dabs(z(k)) .le. a(k,k)) go to 160
               s = a(k,k)/dabs(z(k))
               call dscal(n,s,z,1)
               ynorm = s*ynorm
  160       continue
            z(k) = z(k)/a(k,k)
            t = -z(k)
            call daxpy(k-1,t,a(1,k),1,z(1),1)
  170    continue
c        make znorm = 1.0

         s = 1.0d0/dasum(n,z,1)
         call dscal(n,s,z,1)
         ynorm = s*ynorm
c
         if (anorm .ne. 0.0d0) rcond = ynorm/anorm
         if (anorm .eq. 0.0d0) rcond = 0.0d0
  180 continue
      return
      end
      subroutine dpofa(a,lda,n,info)
      integer lda,n,info
      double precision a(lda,1)
c
c     dpofa factors a double precision symmetric positive definite
c     matrix.
c

c     dpofa is usually called by dpoco, but it can be called
c     directly with a saving in time if  rcond  is not needed.
c     (time for dpoco) = (1 + 18/n)*(time for dpofa) .
c
c     on entry
c
c        a       double precision(lda, n)
c                the symmetric matrix to be factored.  only the
c                diagonal and upper triangle are used.
c
c        lda     integer
c                the leading dimension of the array  a .

c
c        n       integer
c                the order of the matrix  a .
c
c     on return
c
c        a       an upper triangular matrix  r  so that  a = trans(r)*r
c                where  trans(r)  is the transpose.
c                the strict lower triangle is unaltered.

c                if  info .ne. 0 , the factorization is not complete.
c
c        info    integer
c                = 0  for normal return.
c                = k  signals an error condition.  the leading minor
c                     of order  k  is not positive definite.
c
c     linpack.  this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas ddot
c     fortran dsqrt
c
c     internal variables
c

      double precision ddot,t
      double precision s
      integer j,jm1,k
c     begin block with ...exits to 40

c
c
         do 30 j = 1, n
            info = j
            s = 0.0d0
            jm1 = j - 1
            if (jm1 .lt. 1) go to 20
            do 10 k = 1, jm1
               t = a(k,j) - ddot(k-1,a(1,k),1,a(1,j),1)
               t = t/a(k,k)
               a(k,j) = t
               s = s + t*t
   10       continue
   20       continue
            s = a(j,j) - s
c     ......exit

            if (s .le. 0.0d0) go to 40
            a(j,j) = dsqrt(s)
   30    continue
         info = 0
   40 continue
      return
      end
      subroutine dposl(a,lda,n,b)

      integer lda,n
      double precision a(lda,1),b(1)
c

c     dposl solves the double precision symmetric positive definite

c     system a * x = b
c     using the factors computed by dpoco or dpofa.
c
c     on entry
c
c        a       double precision(lda, n)
c                the output from dpoco or dpofa.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer

c                the order of the matrix  a .
c
c        b       double precision(n)
c                the right hand side vector.
c

c     on return
c
c        b       the solution vector  x .

c
c     error condition
c
c        a division by zero will occur if the input factor contains
c        a zero on the diagonal.  technically this indicates
c        singularity but it is usually caused by improper subroutine
c        arguments.  it will not occur if the subroutines are called
c        correctly and  info .eq. 0 .
c
c     to compute  inverse(a) * c  where  c  is a matrix

c     with  p  columns
c           call dpoco(a,lda,n,rcond,z,info)
c           if (rcond is too small .or. info .ne. 0) go to ...
c           do 10 j = 1, p
c              call dposl(a,lda,n,c(1,j))
c        10 continue
c
c     linpack.  this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c

c     subroutines and functions
c
c     blas daxpy,ddot
c
c     internal variables
c
      double precision ddot,t
      integer k,kb
c
c     solve trans(r)*y = b
c
      do 10 k = 1, n

         t = ddot(k-1,a(1,k),1,b(1),1)
         b(k) = (b(k) - t)/a(k,k)
   10 continue
c
c     solve r*x = y
c
      do 20 kb = 1, n
         k = n + 1 - kb
         b(k) = b(k)/a(k,k)
         t = -b(k)
         call daxpy(k-1,t,a(1,k),1,b(1),1)
   20 continue
      return
      end
      subroutine dsifa(a,lda,n,kpvt,info)
      integer lda,n,kpvt(1),info
      double precision a(lda,1)
c
c     dsifa factors a double precision symmetric matrix by elimination
c     with symmetric pivoting.

c
c     to solve  a*x = b , follow dsifa by dsisl.
c     to compute  inverse(a)*c , follow dsifa by dsisl.
c     to compute  determinant(a) , follow dsifa by dsidi.
c     to compute  inertia(a) , follow dsifa by dsidi.
c     to compute  inverse(a) , follow dsifa by dsidi.
c
c     on entry
c
c        a       double precision(lda,n)
c                the symmetric matrix to be factored.
c                only the diagonal and upper triangle are used.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c     on return
c
c        a       a block diagonal matrix and the multipliers which
c                were used to obtain it.

c                the factorization can be written  a = u*d*trans(u)
c                where  u  is a product of permutation and unit
c                upper triangular matrices , trans(u) is the
c                transpose of  u , and  d  is block diagonal

c                with 1 by 1 and 2 by 2 blocks.
c
c        kpvt    integer(n)
c                an integer vector of pivot indices.
c
c        info    integer
c                = 0  normal value.
c                = k  if the k-th pivot block is singular. this is
c                     not an error condition for this subroutine,
c                     but it does indicate that dsisl or dsidi may
c                     divide by zero if called.
c

c     linpack. this version dated 08/14/78 .
c     james bunch, univ. calif. san diego, argonne nat. lab.

c
c     subroutines and functions
c
c     blas daxpy,dswap,idamax
c     fortran dabs,dmax1,dsqrt
c
c     internal variables
c
      double precision ak,akm1,bk,bkm1,denom,mulk,mulkm1,t
      double precision absakk,alpha,colmax,rowmax
      integer imax,imaxp1,j,jj,jmax,k,km1,km2,kstep,idamax
      logical swap
c
c
c     initialize
c
c     alpha is used in choosing pivot block size.
      alpha = (1.0d0 + dsqrt(17.0d0))/8.0d0
c
      info = 0
c
c     main loop on k, which goes from n to 1.
c
      k = n
   10 continue
c
c        leave the loop if k=0 or k=1.
c
c     ...exit
         if (k .eq. 0) go to 200
         if (k .gt. 1) go to 20
            kpvt(1) = 1
            if (a(1,1) .eq. 0.0d0) info = 1
c     ......exit
            go to 200
   20    continue
c
c        this section of code determines the kind of
c        elimination to be performed.  when it is completed,
c        kstep will be set to the size of the pivot block, and
c        swap will be set to .true. if an interchange is
c        required.
c
         km1 = k - 1
         absakk = dabs(a(k,k))
c
c        determine the largest off-diagonal element in
c        column k.

c

         imax = idamax(k-1,a(1,k),1)
         colmax = dabs(a(imax,k))
         if (absakk .lt. alpha*colmax) go to 30
            kstep = 1
            swap = .false.
         go to 90
   30    continue
c
c           determine the largest off-diagonal element in
c           row imax.
c
            rowmax = 0.0d0

            imaxp1 = imax + 1
            do 40 j = imaxp1, k

               rowmax = dmax1(rowmax,dabs(a(imax,j)))
   40       continue
            if (imax .eq. 1) go to 50
               jmax = idamax(imax-1,a(1,imax),1)
               rowmax = dmax1(rowmax,dabs(a(jmax,imax)))
   50       continue
            if (dabs(a(imax,imax)) .lt. alpha*rowmax) go to 60
               kstep = 1
               swap = .true.
            go to 80
   60       continue
            if (absakk .lt. alpha*colmax*(colmax/rowmax)) go to 70
               kstep = 1
               swap = .false.
            go to 80

   70       continue
               kstep = 2
               swap = imax .ne. km1
   80       continue
   90    continue
         if (dmax1(absakk,colmax) .ne. 0.0d0) go to 100
c
c           column k is zero.  set info and iterate the loop.
c
            kpvt(k) = k
            info = k
         go to 190

  100    continue


         if (kstep .eq. 2) go to 140
c
c           1 x 1 pivot block.
c
            if (.not.swap) go to 120
c
c              perform an interchange.
c
               call dswap(imax,a(1,imax),1,a(1,k),1)
               do 110 jj = imax, k
                  j = k + imax - jj
                  t = a(j,k)

                  a(j,k) = a(imax,j)
                  a(imax,j) = t
  110          continue
  120       continue
c
c           perform the elimination.
c
            do 130 jj = 1, km1
               j = k - jj
               mulk = -a(j,k)/a(k,k)
               t = mulk
               call daxpy(j,t,a(1,k),1,a(1,j),1)
               a(j,k) = mulk
  130       continue
c
c           set the pivot array.
c
            kpvt(k) = k

            if (swap) kpvt(k) = imax
         go to 190
  140    continue
c
c           2 x 2 pivot block.
c
            if (.not.swap) go to 160
c
c              perform an interchange.
c
               call dswap(imax,a(1,imax),1,a(1,k-1),1)
               do 150 jj = imax, km1
                  j = km1 + imax - jj
                  t = a(j,k-1)
                  a(j,k-1) = a(imax,j)
                  a(imax,j) = t
  150          continue
               t = a(k-1,k)
               a(k-1,k) = a(imax,k)
               a(imax,k) = t

  160       continue

c
c           perform the elimination.
c
            km2 = k - 2
            if (km2 .eq. 0) go to 180
               ak = a(k,k)/a(k-1,k)
               akm1 = a(k-1,k-1)/a(k-1,k)

               denom = 1.0d0 - ak*akm1
               do 170 jj = 1, km2
                  j = km1 - jj
                  bk = a(j,k)/a(k-1,k)
                  bkm1 = a(j,k-1)/a(k-1,k)
                  mulk = (akm1*bk - bkm1)/denom

                  mulkm1 = (ak*bkm1 - bk)/denom
                  t = mulk
                  call daxpy(j,t,a(1,k),1,a(1,j),1)
                  t = mulkm1
                  call daxpy(j,t,a(1,k-1),1,a(1,j),1)
                  a(j,k) = mulk
                  a(j,k-1) = mulkm1



  170          continue

  180       continue
c
c           set the pivot array.
c
            kpvt(k) = 1 - k
            if (swap) kpvt(k) = -imax
            kpvt(k-1) = kpvt(k)

  190    continue
         k = k - kstep
      go to 10
  200 continue
      return
      end
      subroutine dsisl(a,lda,n,kpvt,b)
      integer lda,n,kpvt(1)
      double precision a(lda,1),b(1)
c
c     dsisl solves the double precision symmetric system
c     a * x = b

c     using the factors computed by dsifa.
c
c     on entry
c
c        a       double precision(lda,n)
c                the output from dsifa.
c
c        lda     integer
c                the leading dimension of the array  a .
c

c        n       integer
c                the order of the matrix  a .
c
c        kpvt    integer(n)
c                the pivot vector from dsifa.
c
c        b       double precision(n)
c                the right hand side vector.
c
c     on return
c
c        b       the solution vector  x .
c
c     error condition
c
c        a division by zero may occur if  dsico  has set rcond .eq. 0.0
c        or  dsifa  has set info .ne. 0  .
c
c     to compute  inverse(a) * c  where  c  is a matrix
c     with  p  columns
c           call dsifa(a,lda,n,kpvt,info)
c           if (info .ne. 0) go to ...
c           do 10 j = 1, p
c              call dsisl(a,lda,n,kpvt,c(1,j))
c        10 continue
c
c     linpack. this version dated 08/14/78 .
c     james bunch, univ. calif. san diego, argonne nat. lab.
c
c     subroutines and functions

c
c     blas daxpy,ddot
c     fortran iabs
c
c     internal variables.
c
      double precision ak,akm1,bk,bkm1,ddot,denom,temp
      integer k,kp

c
c     loop backward applying the transformations and
c     d inverse to b.
c
      k = n
   10 if (k .eq. 0) go to 80
         if (kpvt(k) .lt. 0) go to 40
c
c           1 x 1 pivot block.
c
            if (k .eq. 1) go to 30
               kp = kpvt(k)
               if (kp .eq. k) go to 20
c
c                 interchange.
c

                  temp = b(k)
                  b(k) = b(kp)
                  b(kp) = temp

   20          continue
c
c              apply the transformation.
c
               call daxpy(k-1,b(k),a(1,k),1,b(1),1)
   30       continue
c
c           apply d inverse.
c
            b(k) = b(k)/a(k,k)
            k = k - 1
         go to 70

   40    continue

c
c           2 x 2 pivot block.
c
            if (k .eq. 2) go to 60
               kp = iabs(kpvt(k))
               if (kp .eq. k - 1) go to 50
c
c                 interchange.
c
                  temp = b(k-1)
                  b(k-1) = b(kp)
                  b(kp) = temp
   50          continue
c
c              apply the transformation.
c
               call daxpy(k-2,b(k),a(1,k),1,b(1),1)
               call daxpy(k-2,b(k-1),a(1,k-1),1,b(1),1)
   60       continue
c
c           apply d inverse.
c
            ak = a(k,k)/a(k-1,k)
            akm1 = a(k-1,k-1)/a(k-1,k)
            bk = b(k)/a(k-1,k)
            bkm1 = b(k-1)/a(k-1,k)
            denom = ak*akm1 - 1.0d0
            b(k) = (akm1*bk - bkm1)/denom
            b(k-1) = (ak*bkm1 - bk)/denom
            k = k - 2
   70    continue
      go to 10
   80 continue
c
c     loop forward applying the transformations.
c
      k = 1
   90 if (k .gt. n) go to 160
         if (kpvt(k) .lt. 0) go to 120
c
c           1 x 1 pivot block.
c
            if (k .eq. 1) go to 110

c
c              apply the transformation.
c
               b(k) = b(k) + ddot(k-1,a(1,k),1,b(1),1)
               kp = kpvt(k)
               if (kp .eq. k) go to 100


c
c                 interchange.
c
                  temp = b(k)
                  b(k) = b(kp)
                  b(kp) = temp


  100          continue

  110       continue
            k = k + 1
         go to 150
  120    continue
c
c           2 x 2 pivot block.
c
            if (k .eq. 1) go to 140
c
c              apply the transformation.
c
               b(k) = b(k) + ddot(k-1,a(1,k),1,b(1),1)
               b(k+1) = b(k+1) + ddot(k-1,a(1,k+1),1,b(1),1)
               kp = iabs(kpvt(k))
               if (kp .eq. k) go to 130
c
c                 interchange.


c
                  temp = b(k)
                  b(k) = b(kp)
                  b(kp) = temp
  130          continue
  140       continue
            k = k + 2

  150    continue

      go to 90
  160 continue
      return
      end
      subroutine dqrdc(x,ldx,n,p,qraux,jpvt,work,job)
      integer ldx,n,p,job

      integer jpvt(1)
      double precision x(ldx,1),qraux(1),work(1)
c
c     dqrdc uses householder transformations to compute the qr
c     factorization of an n by p matrix x.  column pivoting
c     based on the 2-norms of the reduced columns may be
c     performed at the users option.
c
c     on entry
c
c        x       double precision(ldx,p), where ldx .ge. n.
c                x contains the matrix whose decomposition is to be

c                computed.
c
c        ldx     integer.
c                ldx is the leading dimension of the array x.
c
c        n       integer.
c                n is the number of rows of the matrix x.
c
c        p       integer.
c                p is the number of columns of the matrix x.
c
c        jpvt    integer(p).
c                jpvt contains integers that control the selection
c                of the pivot columns.  the k-th column x(k) of x

c                is placed in one of three classes according to the
c                value of jpvt(k).
c
c                   if jpvt(k) .gt. 0, then x(k) is an initial
c                                      column.

c
c                   if jpvt(k) .eq. 0, then x(k) is a free column.

c
c                   if jpvt(k) .lt. 0, then x(k) is a final column.
c
c                before the decomposition is computed, initial columns
c                are moved to the beginning of the array x and final

c                columns to the end.  both initial and final columns
c                are frozen in place during the computation and only
c                free columns are moved.  at the k-th stage of the
c                reduction, if x(k) is occupied by a free column
c                it is interchanged with the free column of largest
c                reduced norm.  jpvt is not referenced if
c                job .eq. 0.
c
c        work    double precision(p).
c                work is a work array.  work is not referenced if
c                job .eq. 0.
c
c        job     integer.
c                job is an integer that initiates column pivoting.
c                if job .eq. 0, no pivoting is done.
c                if job .ne. 0, pivoting is done.
c

c     on return
c
c        x       x contains in its upper triangle the upper

c                triangular matrix r of the qr factorization.
c                below its diagonal x contains information from
c                which the orthogonal part of the decomposition
c                can be recovered.  note that if pivoting has

c                been requested, the decomposition is not that
c                of the original matrix x but that of x
c                with its columns permuted as described by jpvt.
c
c        qraux   double precision(p).
c                qraux contains further information required to recover
c                the orthogonal part of the decomposition.
c
c        jpvt    jpvt(k) contains the index of the column of the
c                original matrix that has been interchanged into
c                the k-th column, if pivoting was requested.
c
c     linpack. this version dated 08/14/78 .
c     g.w. stewart, university of maryland, argonne national lab.
c

c     dqrdc uses the following functions and subprograms.
c
c     blas daxpy,ddot,dscal,dswap,dnrm2
c     fortran dabs,dmax1,min0,dsqrt
c
c     internal variables
c
      integer j,jp,l,lp1,lup,maxj,pl,pu
      double precision maxnrm,dnrm2,tt
      double precision ddot,nrmxl,t
      logical negj,swapj
c
c
      pl = 1
      pu = 0
      if (job .eq. 0) go to 60
c
c        pivoting has been requested.  rearrange the columns
c        according to jpvt.
c
         do 20 j = 1, p


            swapj = jpvt(j) .gt. 0
            negj = jpvt(j) .lt. 0
            jpvt(j) = j
            if (negj) jpvt(j) = -j
            if (.not.swapj) go to 10
               if (j .ne. pl) call dswap(n,x(1,pl),1,x(1,j),1)
               jpvt(j) = jpvt(pl)
               jpvt(pl) = j
               pl = pl + 1
   10       continue

   20    continue
         pu = p
         do 50 jj = 1, p
            j = p - jj + 1
            if (jpvt(j) .ge. 0) go to 40

               jpvt(j) = -jpvt(j)

               if (j .eq. pu) go to 30
                  call dswap(n,x(1,pu),1,x(1,j),1)
                  jp = jpvt(pu)
                  jpvt(pu) = jpvt(j)
                  jpvt(j) = jp
   30          continue
               pu = pu - 1
   40       continue
   50    continue
   60 continue

c
c     compute the norms of the free columns.
c
      if (pu .lt. pl) go to 80
      do 70 j = pl, pu
         qraux(j) = dnrm2(n,x(1,j),1)
         work(j) = qraux(j)
   70 continue
   80 continue
c
c     perform the householder reduction of x.
c
      lup = min0(n,p)
      do 200 l = 1, lup
         if (l .lt. pl .or. l .ge. pu) go to 120
c
c           locate the column of largest norm and bring it
c           into the pivot position.
c
            maxnrm = 0.0d0

            maxj = l
            do 100 j = l, pu
               if (qraux(j) .le. maxnrm) go to 90
                  maxnrm = qraux(j)
                  maxj = j
   90          continue

  100       continue


            if (maxj .eq. l) go to 110
               call dswap(n,x(1,l),1,x(1,maxj),1)
               qraux(maxj) = qraux(l)
               work(maxj) = work(l)
               jp = jpvt(maxj)
               jpvt(maxj) = jpvt(l)
               jpvt(l) = jp
  110       continue
  120    continue
         qraux(l) = 0.0d0
         if (l .eq. n) go to 190
c
c           compute the householder transformation for column l.
c
            nrmxl = dnrm2(n-l+1,x(l,l),1)
            if (nrmxl .eq. 0.0d0) go to 180
               if (x(l,l) .ne. 0.0d0) nrmxl = dsign(nrmxl,x(l,l))
               call dscal(n-l+1,1.0d0/nrmxl,x(l,l),1)
               x(l,l) = 1.0d0 + x(l,l)
c
c              apply the transformation to the remaining columns,
c              updating the norms.
c
               lp1 = l + 1

               if (p .lt. lp1) go to 170
               do 160 j = lp1, p

                  t = -ddot(n-l+1,x(l,l),1,x(l,j),1)/x(l,l)
                  call daxpy(n-l+1,t,x(l,l),1,x(l,j),1)
                  if (j .lt. pl .or. j .gt. pu) go to 150
                  if (qraux(j) .eq. 0.0d0) go to 150
                     tt = 1.0d0 - (dabs(x(l,j))/qraux(j))**2
                     tt = dmax1(tt,0.0d0)
                     t = tt
                     tt = 1.0d0 + 0.05d0*tt*(qraux(j)/work(j))**2
                     if (tt .eq. 1.0d0) go to 130
                        qraux(j) = qraux(j)*dsqrt(t)
                     go to 140

  130                continue
                        qraux(j) = dnrm2(n-l,x(l+1,j),1)
                        work(j) = qraux(j)
  140                continue
  150             continue
  160          continue
  170          continue
c
c              save the transformation.

c
               qraux(l) = x(l,l)
               x(l,l) = -nrmxl
  180       continue

  190    continue
  200 continue
      return
      end
C LAPACK routines follow
C note that thte call to the LAPACK auxialliary routine
C that defines NB has been rpelaced by a hardwired
C NB=16 in dpotrf.f
C This is probably OK for PCs, but workstations may be a bit faster with
C NB = 32
      SUBROUTINE DPOTRF( UPLO, N, A, LDA, INFO, ERRFIL )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993

*

*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDA, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
      CHARACTER*20       ERRFIL

*     ..
*
*  Purpose
*  =======
*
*  DPOTRF computes the Cholesky factorization of a real symmetric
*  positive definite matrix A.
*
*  The factorization has the form
*     A = U**T * U,  if UPLO = 'U', or
*     A = L  * L**T,  if UPLO = 'L',
*  where U is an upper triangular matrix and L is lower triangular.

*
*  This is the block version of the algorithm, calling Level 3 BLAS.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER

*          The order of the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the symmetric matrix A.  If UPLO = 'U', the leading
*          N-by-N upper triangular part of A contains the upper
*          triangular part of the matrix A, and the strictly lower
*          triangular part of A is not referenced.  If UPLO = 'L', the
*          leading N-by-N lower triangular part of A contains the lower
*          triangular part of the matrix A, and the strictly upper
*          triangular part of A is not referenced.
*
*          On exit, if INFO = 0, the factor U or L from the Cholesky
*          factorization A = U**T*U or A = L*L**T.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value

*          > 0:  if INFO = i, the leading minor of order i is not
*                positive definite, and the factorization could not be
*                completed.
*
*  =====================================================================
*
*     .. Parameters ..

      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            J, JB, NB
*     ..

*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMM, DPOTF2, DSYRK, DTRSM, XERBLA
*     ..
*     .. Intrinsic Functions ..

      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..

*
*     Test the input parameters.
*
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      END IF

      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DPOTRF', -INFO, ERRFIL )
         RETURN
      END IF
*
*     Quick return if possible


*

      IF( N.EQ.0 )
     $   RETURN
*
*     Determine the block size for this environment.
*
      nb = 16
      IF( NB.LE.1 .OR. NB.GE.N ) THEN
*
*        Use unblocked code.

*
         CALL DPOTF2( UPLO, N, A, LDA, INFO, ERRFIL )
      ELSE
*
*        Use blocked code.
*
         IF( UPPER ) THEN
*
*           Compute the Cholesky factorization A = U'*U.
*
            DO 10 J = 1, N, NB
*
*              Update and factorize the current diagonal block and test
*              for non-positive-definiteness.
*
               JB = MIN( NB, N-J+1 )
               CALL DSYRK( 'Upper', 'Transpose', JB, J-1, -ONE,
     $                     A( 1, J ), LDA, ONE, A( J, J ), LDA )
               CALL DPOTF2( 'Upper', JB, A( J, J ), LDA, INFO, ERRFIL )
               IF( INFO.NE.0 )
     $            GO TO 30
               IF( J+JB.LE.N ) THEN
*
*                 Compute the current block row.
*
                  CALL DGEMM( 'Transpose', 'No transpose', JB, N-J-JB+1,
     $                        J-1, -ONE, A( 1, J ), LDA, A( 1, J+JB ),
     $                        LDA, ONE, A( J, J+JB ), LDA )
                  CALL DTRSM( 'Left', 'Upper', 'Transpose', 'Non-unit',
     $                        JB, N-J-JB+1, ONE, A( J, J ), LDA,
     $                        A( J, J+JB ), LDA )
               END IF
   10       CONTINUE

*
         ELSE
*
*           Compute the Cholesky factorization A = L*L'.
*
            DO 20 J = 1, N, NB
*
*              Update and factorize the current diagonal block and test
*              for non-positive-definiteness.
*
               JB = MIN( NB, N-J+1 )
               CALL DSYRK( 'Lower', 'No transpose', JB, J-1, -ONE,
     $                     A( J, 1 ), LDA, ONE, A( J, J ), LDA )
               CALL DPOTF2( 'Lower', JB, A( J, J ), LDA, INFO, ERRFIL )
               IF( INFO.NE.0 )
     $            GO TO 30
               IF( J+JB.LE.N ) THEN
*
*                 Compute the current block column.
*
                  CALL DGEMM( 'No transpose', 'Transpose', N-J-JB+1, JB,
     $                        J-1, -ONE, A( J+JB, 1 ), LDA, A( J, 1 ),
     $                        LDA, ONE, A( J+JB, J ), LDA )
                  CALL DTRSM( 'Right', 'Lower', 'Transpose', 'Non-unit',
     $                        N-J-JB+1, JB, ONE, A( J, J ), LDA,
     $                        A( J+JB, J ), LDA )
               END IF

   20       CONTINUE
         END IF
      END IF
      GO TO 40
*

   30 CONTINUE
      INFO = INFO + J - 1
*
   40 CONTINUE
      RETURN
*
*     End of DPOTRF
*
      END
      SUBROUTINE DPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO, ERRFIL )
*

*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..

      CHARACTER          UPLO

      INTEGER            INFO, LDA, LDB, N, NRHS
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
      CHARACTER*20       ERRFIL

*     ..
*
*  Purpose
*  =======
*
*  DPOTRS solves a system of linear equations A*X = B with a symmetric
*  positive definite matrix A using the Cholesky factorization
*  A = U**T*U or A = L*L**T computed by DPOTRF.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*

*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  NRHS    (input) INTEGER

*          The number of right hand sides, i.e., the number of columns
*          of the matrix B.  NRHS >= 0.
*

*  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
*          The triangular factor U or L from the Cholesky factorization
*          A = U**T*U or A = L*L**T, as computed by DPOTRF.
*
*  LDA     (input) INTEGER

*          The leading dimension of the array A.  LDA >= max(1,N).

*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the right hand side matrix B.
*          On exit, the solution matrix X.
*

*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            UPPER
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DTRSM, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..

*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3

      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DPOTRS', -INFO, ERRFIL )
         RETURN
      END IF

*
*     Quick return if possible
*
      IF( N.EQ.0 .OR. NRHS.EQ.0 )
     $   RETURN
*
      IF( UPPER ) THEN
*
*        Solve A*X = B where A = U'*U.
*

*        Solve U'*X = B, overwriting B with X.
*
         CALL DTRSM( 'Left', 'Upper', 'Transpose', 'Non-unit', N, NRHS,
     $               ONE, A, LDA, B, LDB )
*
*        Solve U*X = B, overwriting B with X.
*
         CALL DTRSM( 'Left', 'Upper', 'No transpose', 'Non-unit', N,
     $               NRHS, ONE, A, LDA, B, LDB )
      ELSE
*
*        Solve A*X = B where A = L*L'.
*
*        Solve L*X = B, overwriting B with X.
*
         CALL DTRSM( 'Left', 'Lower', 'No transpose', 'Non-unit', N,
     $               NRHS, ONE, A, LDA, B, LDB )
*
*        Solve L'*X = B, overwriting B with X.
*
         CALL DTRSM( 'Left', 'Lower', 'Transpose', 'Non-unit', N, NRHS,
     $               ONE, A, LDA, B, LDB )
      END IF
*
      RETURN
*
*     End of DPOTRS
*
      END
      SUBROUTINE DPOTF2( UPLO, N, A, LDA, INFO, ERRFIL )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..

      CHARACTER          UPLO
      INTEGER            INFO, LDA, N
*     ..

*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
      CHARACTER*20       ERRFIL
*     ..
*
*  Purpose
*  =======

*
*  DPOTF2 computes the Cholesky factorization of a real symmetric
*  positive definite matrix A.

*
*  The factorization has the form
*     A = U' * U ,  if UPLO = 'U', or
*     A = L  * L',  if UPLO = 'L',
*  where U is an upper triangular matrix and L is lower triangular.
*
*  This is the unblocked version of the algorithm, calling Level 2 BLAS.
*
*  Arguments


*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the upper or lower triangular part of the
*          symmetric matrix A is stored.
*          = 'U':  Upper triangular

*          = 'L':  Lower triangular
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)


*          On entry, the symmetric matrix A.  If UPLO = 'U', the leading
*          n by n upper triangular part of A contains the upper
*          triangular part of the matrix A, and the strictly lower
*          triangular part of A is not referenced.  If UPLO = 'L', the
*          leading n by n lower triangular part of A contains the lower
*          triangular part of the matrix A, and the strictly upper
*          triangular part of A is not referenced.
*
*          On exit, if INFO = 0, the factor U or L from the Cholesky
*          factorization A = U'*U  or A = L*L'.
*
*  LDA     (input) INTEGER

*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -k, the k-th argument had an illegal value
*          > 0: if INFO = k, the leading minor of order k is not
*               positive definite, and the factorization could not be
*               completed.
*

*  =====================================================================

*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            J
      DOUBLE PRECISION   AJJ
*     ..
*     .. External Functions ..
      LOGICAL            LSAME

      DOUBLE PRECISION   DDOT
      EXTERNAL           LSAME, DDOT
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMV, DSCAL, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, SQRT

*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )

      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DPOTF2', -INFO, ERRFIL )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
      IF( UPPER ) THEN
*
*        Compute the Cholesky factorization A = U'*U.
*
         DO 10 J = 1, N
*
*           Compute U(J,J) and test for non-positive-definiteness.
*
            AJJ = A( J, J ) - DDOT( J-1, A( 1, J ), 1, A( 1, J ), 1 )
            IF( AJJ.LE.ZERO ) THEN
               A( J, J ) = AJJ
               GO TO 30
            END IF
            AJJ = SQRT( AJJ )
            A( J, J ) = AJJ
*
*           Compute elements J+1:N of row J.
*
            IF( J.LT.N ) THEN
               CALL DGEMV( 'Transpose', J-1, N-J, -ONE, A( 1, J+1 ),
     $                     LDA, A( 1, J ), 1, ONE, A( J, J+1 ), LDA )
               CALL DSCAL( N-J, ONE / AJJ, A( J, J+1 ), LDA )
            END IF
   10    CONTINUE
      ELSE
*
*        Compute the Cholesky factorization A = L*L'.
*
         DO 20 J = 1, N
*
*           Compute L(J,J) and test for non-positive-definiteness.
*
            AJJ = A( J, J ) - DDOT( J-1, A( J, 1 ), LDA, A( J, 1 ),
     $            LDA )
            IF( AJJ.LE.ZERO ) THEN
               A( J, J ) = AJJ
               GO TO 30
            END IF
            AJJ = SQRT( AJJ )
            A( J, J ) = AJJ
*
*           Compute elements J+1:N of column J.
*
            IF( J.LT.N ) THEN
               CALL DGEMV( 'No transpose', N-J, J-1, -ONE, A( J+1, 1 ),
     $                     LDA, A( J, 1 ), LDA, ONE, A( J+1, J ), 1 )
               CALL DSCAL( N-J, ONE / AJJ, A( J+1, J ), 1 )
            END IF
   20    CONTINUE
      END IF
      GO TO 40
*
   30 CONTINUE
      INFO = J
*
   40 CONTINUE
      RETURN
*
*     End of DPOTF2
*
      END
      LOGICAL          FUNCTION LSAME( CA, CB )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994

*
*     .. Scalar Arguments ..
      CHARACTER          CA, CB
*     ..
*
*  Purpose
*  =======
*
*  LSAME returns .TRUE. if CA is the same letter as CB regardless of
*  case.
*
*  Arguments
*  =========
*
*  CA      (input) CHARACTER*1
*  CB      (input) CHARACTER*1
*          CA and CB specify the single characters to be compared.
*
* =====================================================================
*
*     .. Intrinsic Functions ..
      INTRINSIC          ICHAR
*     ..
*     .. Local Scalars ..
      INTEGER            INTA, INTB, ZCODE
*     ..
*     .. Executable Statements ..

*

*     Test if the characters are equal

*
      LSAME = CA.EQ.CB

      IF( LSAME )
     $   RETURN
*
*     Now test for equivalence if both characters are alphabetic.
*

      ZCODE = ICHAR( 'Z' )

*
*     Use 'Z' rather than 'A' so that ASCII can be detected on Prime
*     machines, on which ICHAR returns a value with bit 8 set.
*     ICHAR('A') on Prime machines returns 193 which is the same as
*     ICHAR('A') on an EBCDIC machine.
*
      INTA = ICHAR( CA )
      INTB = ICHAR( CB )
*
      IF( ZCODE.EQ.90 .OR. ZCODE.EQ.122 ) THEN
*
*        ASCII is assumed - ZCODE is the ASCII code of either lower or

*        upper case 'Z'.
*
         IF( INTA.GE.97 .AND. INTA.LE.122 ) INTA = INTA - 32
         IF( INTB.GE.97 .AND. INTB.LE.122 ) INTB = INTB - 32
*
      ELSE IF( ZCODE.EQ.233 .OR. ZCODE.EQ.169 ) THEN
*
*        EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
*        upper case 'Z'.

*
         IF( INTA.GE.129 .AND. INTA.LE.137 .OR.
     $       INTA.GE.145 .AND. INTA.LE.153 .OR.
     $       INTA.GE.162 .AND. INTA.LE.169 ) INTA = INTA + 64
         IF( INTB.GE.129 .AND. INTB.LE.137 .OR.
     $       INTB.GE.145 .AND. INTB.LE.153 .OR.
     $       INTB.GE.162 .AND. INTB.LE.169 ) INTB = INTB + 64

*
      ELSE IF( ZCODE.EQ.218 .OR. ZCODE.EQ.250 ) THEN
*
*        ASCII is assumed, on Prime machines - ZCODE is the ASCII code
*        plus 128 of either lower or upper case 'Z'.
*
         IF( INTA.GE.225 .AND. INTA.LE.250 ) INTA = INTA - 32
         IF( INTB.GE.225 .AND. INTB.LE.250 ) INTB = INTB - 32
      END IF
      LSAME = INTA.EQ.INTB
*
*     RETURN
*

*     End of LSAME
*
      END
      SUBROUTINE XERBLA( SRNAME, INFO, ERRFIL )
*
*  -- LAPACK auxiliary routine (preliminary version) --


*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER*6        SRNAME
      CHARACTER*20       ERRFIL
      INTEGER            INFO
*      COMMON/ERR/ERRFIL 

*     ..


*
*  Purpose
*  =======
*
*  XERBLA  is an error handler for the LAPACK routines.
*  It is called by an LAPACK routine if an input parameter has an
*  invalid value.  A message is printed and execution stops.
*
*  Installers may consider modifying the STOP statement in order to
*  call system-specific exception-handling facilities.
*
*  Arguments
*  =========
*
*  SRNAME  (input) CHARACTER*6
*          The name of the routine which called XERBLA.

*
*  INFO    (input) INTEGER
*          The position of the invalid parameter in the parameter list
*          of the calling routine.
*
*
      WRITE( *, FMT = 9999 )SRNAME, INFO

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
      WRITE( 42, FMT = 9999 )SRNAME, INFO
        CLOSE(42)

      CALL PAUSE
      STOP

 9999 FORMAT( ' ** On entry to ', A6, ' parameter number ', I2, ' had ',
     $      'an illegal value' )
*
*     End of XERBLA
*
      END
