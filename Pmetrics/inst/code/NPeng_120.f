      SUBROUTINE DGEMM ( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,
     $                   BETA, C, LDC )
      CHARACTER*1        TRANSA, TRANSB
      INTEGER            M, N, K, LDA, LDB, LDC
      DOUBLE PRECISION   ALPHA, BETA
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * )
      CHARACTER*20       ERRFIL
      LOGICAL            LSAME
      EXTERNAL           LSAME
      EXTERNAL           XERBLA
      INTRINSIC          MAX
      LOGICAL            NOTA, NOTB
      INTEGER            I, INFO, J, L, NCOLA, NROWA, NROWB
      DOUBLE PRECISION   TEMP
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
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
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
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
      IF( NOTB )THEN
         IF( NOTA )THEN
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
      RETURN
      END
      SUBROUTINE DGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
      DOUBLE PRECISION   ALPHA, BETA
      INTEGER            INCX, INCY, LDA, M, N
      CHARACTER*1        TRANS
      CHARACTER*20       ERRFIL
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY
      LOGICAL            LSAME
      EXTERNAL           LSAME
      EXTERNAL           XERBLA
      INTRINSIC          MAX
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
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
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
      RETURN
      END
      SUBROUTINE DSYRK ( UPLO, TRANS, N, K, ALPHA, A, LDA,
     $                   BETA, C, LDC )
      CHARACTER*1        UPLO, TRANS
      CHARACTER*20       ERRFIL
      INTEGER            N, K, LDA, LDC
      DOUBLE PRECISION   ALPHA, BETA
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * )
      LOGICAL            LSAME
      EXTERNAL           LSAME
      EXTERNAL           XERBLA
      INTRINSIC          MAX
      LOGICAL            UPPER
      INTEGER            I, INFO, J, L, NROWA
      DOUBLE PRECISION   TEMP
      DOUBLE PRECISION   ONE ,         ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      IF( LSAME( TRANS, 'N' ) )THEN
         NROWA = N
      ELSE
         NROWA = K
      END IF
      UPPER = LSAME( UPLO, 'U' )
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
      IF( ( N.EQ.0 ).OR.
     $    ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
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
      IF( LSAME( TRANS, 'N' ) )THEN
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
      RETURN
      END
      SUBROUTINE DTRSM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,
     $                   B, LDB )
      CHARACTER*1        SIDE, UPLO, TRANSA, DIAG
      INTEGER            M, N, LDA, LDB
      DOUBLE PRECISION   ALPHA
      CHARACTER*20       ERRFIL
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
      LOGICAL            LSAME
      EXTERNAL           LSAME
      EXTERNAL           XERBLA
      INTRINSIC          MAX
      LOGICAL            LSIDE, NOUNIT, UPPER
      INTEGER            I, INFO, J, K, NROWA
      DOUBLE PRECISION   TEMP
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      LSIDE  = LSAME( SIDE  , 'L' )
      IF( LSIDE )THEN
         NROWA = M
      ELSE
         NROWA = N
      END IF
      NOUNIT = LSAME( DIAG  , 'N' )
      UPPER  = LSAME( UPLO  , 'U' )
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
      IF( N.EQ.0 )
     $   RETURN
      IF( ALPHA.EQ.ZERO )THEN
         DO 20, J = 1, N
            DO 10, I = 1, M
               B( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
         RETURN
      END IF
      IF( LSIDE )THEN
         IF( LSAME( TRANSA, 'N' ) )THEN
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
      RETURN
      END
      subroutine  dcopy(n,dx,incx,dy,incy)
      double precision dx(*),dy(*)
      integer i,incx,incy,ix,iy,m,mp1,n
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
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
      subroutine  dscal(n,da,dx,incx)
      double precision da,dx(*)
      integer i,incx,m,mp1,n,nincx
      if( n.le.0 .or. incx.le.0 )return
      if(incx.eq.1)go to 20
      nincx = n*incx
      do 10 i = 1,nincx,incx
        dx(i) = da*dx(i)
   10 continue
      return
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
      subroutine daxpy(n,da,dx,incx,dy,incy)
      double precision dx(*),dy(*),da
      integer i,incx,incy,ix,iy,m,mp1,n
      if(n.le.0)return
      if (da .eq. 0.0d0) return
      if(incx.eq.1.and.incy.eq.1)go to 20
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
      double precision function ddot(n,dx,incx,dy,incy)
      double precision dx(*),dy(*),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
      ddot = 0.0d0
      dtemp = 0.0d0
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
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
      integer function idamax(n,dx,incx)
      double precision dx(*),dmax
      integer i,incx,ix,n
      idamax = 0
      if( n.lt.1 .or. incx.le.0 ) return
      idamax = 1
      if(n.eq.1)return
      if(incx.eq.1)go to 20
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
   20 dmax = dabs(dx(1))
      do 30 i = 2,n
         if(dabs(dx(i)).le.dmax) go to 30
         idamax = i
         dmax = dabs(dx(i))
   30 continue
      return
      end
      subroutine  dswap (n,dx,incx,dy,incy)
      double precision dx(*),dy(*),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
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
      double precision dx(*),dtemp
      integer i,incx,m,mp1,n,nincx
      dasum = 0.0d0
      dtemp = 0.0d0
      if( n.le.0 .or. incx.le.0 )return
      if(incx.eq.1)go to 20
      nincx = n*incx
      do 10 i = 1,nincx,incx
        dtemp = dtemp + dabs(dx(i))
   10 continue
      dasum = dtemp
      return
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
      INTEGER                           INCX, N
      DOUBLE PRECISION                  X( * )
      DOUBLE PRECISION      ONE         , ZERO
      PARAMETER           ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      INTEGER               IX
      DOUBLE PRECISION      ABSXI, NORM, SCALE, SSQ
      INTRINSIC             ABS, SQRT
      IF( N.LT.1 .OR. INCX.LT.1 )THEN
         NORM  = ZERO
      ELSE IF( N.EQ.1 )THEN
         NORM  = ABS( X( 1 ) )
      ELSE
         SCALE = ZERO
         SSQ   = ONE
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
      DNRM2 = NORM
      RETURN
      END
      SUBROUTINE READOUT( OUTFILER
     1 , DOSEBLOCK, OBSBLOCK, NDORIG
     2 , BAYPOS, NACTSUB )
        use npag_utils, only: verifyval, maxnumeq,
     1   max_pop_rand_varbs, max_pop_varbs, max_pop_params
     2   ,max_covs, max_input_dim
        IMPLICIT REAL*8(A-H,O-Z)
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
        CHARACTER PARFIX(20)*11,READLINE*80,
     1   NAME(800)*53,CHARTNO(800)*53,SEX(800)*1,PARRANFIX(20)*11,
     2   COVDESCR(max_covs)*20,OUTFILER*20,READLINE2*1000,PRIFILE*20
        CHARACTER(LEN=20) :: OSName
      real*8, dimension(800,1000,35) :: DOSEBLOCK
      real*8, dimension(800,150,MAXNUMEQ+1) :: OBSBLOCK
      integer, dimension(800) :: NDORIG
      real*8, dimension(100,1500,31) :: BAYPOS
      integer, dimension(100) :: NACTSUB
      integer ISUB,K,J
      WRITE(*,911)
  911 FORMAT(//' NOW CREATING THE NP_RFxxxx.TXT FILE ...')
        MAXGRD = 1500
        MAXOBDIM = 150
        MAXSUB = 800
    1   FORMAT(A20)
        IVER = 42
    2   FORMAT(A80)
        CALL GETNUM(NUMEQT)
   80   READ(25,2) READLINE
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
        READ(23,*)
	  READ(23,*) NDIM
        READ(23,*) INDPTS
	READ(23,*) NACTVE
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
	DO I=1,NACTVE
	READ(23,*) (CORDEN(I,J),J=1,NVAR+1)
	END DO
	DO JSUB=1,NSUB
	 DO I=1,NACTVE
	  READ(23,*) PYJGX(JSUB,I)
	 END DO
	END DO
	REWIND(27)
	DO JSUB=1,NSUB
	 CALL FILREDT(NOBSER,TO,YO,C0,C1,C2,C3,MAXOBDIM,NDRUG,ND,NADD)
       NOBS(JSUB) = NOBSER
	 DO IEQ=1,NUMEQT
	  DO J=1,NOBSER
	   READ(23,*) (YPREDPOP(JSUB,IEQ,J,ICENTER),ICENTER=1,3)
	  END DO
	 END DO
	END DO
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
        CALL CONVERGE2(NCYCLE,XLOGLIK,XMEAN,STDEV,INDXSD,AICBIC,
     1   PRCFVR,ACTPTS,SCALNFO,GAMLAM,AGE,HEIGHT,
     2   SUBMEAN,SUBLOGLIK,SUBSTD,SUBPERCOF,
     3   NAME,CHARTNO,SEX,NDD,NI,ASSAYC,IERRMOD)
        OPEN(21)
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
        XVERIFY(1) = RTOL
        CALL VERIFYVAL(1,XVERIFY)
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
        DO I = 1,NDRUG
         XVERIFY(1) = AF(I)
         CALL VERIFYVAL(1,XVERIFY)
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
        DO IVAR = 1,NVAR
         WRITE(21,1717) PAR(IVAR)
         NLINE = NLINE + 1
        END DO
        WRITE(21,153)
  153   FORMAT(/8X,'   # FIXED PARAMETER NAMES')
        NLINE = NLINE + 2
        NLFIXPAR = NLINE
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
        DO I = 1,NVAR
         WRITE(21,*) AB(I,1),AB(I,2)
         NLINE = NLINE + 1
        END DO
        WRITE(21,156)
  156   FORMAT(/8X,'   # FIXED PARAMETER VALUES')
        NLINE = NLINE + 2
        NLFIXVAL = NLINE
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
        DO I = 1,NACTVE
         DO J = 1,NVAR+1
          WRITE(21,*) CORDEN(I,J)
          NLINE = NLINE + 1
         END DO
        END DO
        WRITE(21,2013)
 2013   FORMAT(/8X,'   # BAYESIAN POSTERIOR DENSITY VALUES, IN ORDER')
        NLINE = NLINE + 2
        NLBAYPOS = NLINE
       ISUB = 1
       K = 1
       J = 1
       DO ISUB = 1,NNSUB
        DO K = 1,NACTSUB(ISUB)
         DO J = 1,NVAR+1
          WRITE(21,*) BAYPOS(ISUB,K,J)
          NLINE = NLINE + 1
         END DO
        END DO
       END DO
        WRITE(21,158)
  158   FORMAT(/8X,'   # PYJGX (CONDITIONAL PROB. VALUES)')
        NLINE = NLINE + 2
        NLPYJGX = NLINE
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
        DO ICYCLE = 1,NCYCLE
         WRITE(21,*) XLOGLIK(ICYCLE)
         NLINE = NLINE + 1
        END DO
        WRITE(21,1166)
 1166   FORMAT(/8X,'   # CYCLE AICs AND BICs')
        NLINE = NLINE + 2
        NLCYCAICBIC = NLINE
        DO ICYCLE = 1,NCYCLE
         WRITE(21,*) (AICBIC(ICYCLE,J),J=1,2)
         NLINE = NLINE + 1
        END DO
        WRITE(21,167)
  167   FORMAT(/8X,'   # CYCLE MEAN VECTORS')
        NLINE = NLINE + 2
        NLCYCMEAN = NLINE
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
        REWIND(25)
        ICYCLE = 0
  180   READ(25,2) READLINE
        IF(READLINE(2:23) .NE. 'IN THE LINE IS THE THE') GO TO 180
        ICYCLE = ICYCLE + 1
        IF(ICYCLE .GT. ICYCTOT - ICYCSTART + 1) GO TO 220
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
        DO ICYCLE = 1,NCYCLE
         WRITE(21,*) GAMLAM(ICYCLE)
         NLINE = NLINE + 1
        END DO
        WRITE(21,172)
  172   FORMAT(/8X,'   # BAYESIAN LOG-LIKLIHOODS')
        NLINE = NLINE + 2
        NLBAYLOGLIK = NLINE
        DO JSUB = 1,NSUB
         WRITE(21,*) SUBLOGLIK(JSUB)
         NLINE = NLINE + 1
        END DO
        WRITE(21,173)
  173   FORMAT(/8X,'   # BAYESIAN MEANS')
        NLINE = NLINE + 2
        NLBAYMEAN = NLINE
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
        BACKSPACE(25)
        BACKSPACE(25)
        BACKSPACE(25)
        JSUB = 0
  280   IF(JSUB .EQ. NSUB) GO TO 230
        READ(25,2,IOSTAT=IEND) READLINE
        IF(READLINE(2:23) .NE. 'IN THE LINE IS THE THE') GO TO 280
        JSUB = JSUB + 1
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
        DO JSUB = 1,NSUB
         WRITE(21,181) NAME(JSUB)
  181    FORMAT(A53)
         WRITE(21,181) CHARTNO(JSUB)
         XVERIFY(1) = AGE(JSUB)
         XVERIFY(2) = HEIGHT(JSUB)
         CALL VERIFYVAL(2,XVERIFY)
         WRITE(21,1182) XVERIFY(1),SEX(JSUB),XVERIFY(2)
 1182    FORMAT(2X,F10.3,2X,A1,2X,F10.3)
         NLINE = NLINE + 3
        END DO
        WRITE(21,183)
  183   FORMAT(/8X,'   # PATIENT DOSECOV BLOCKS')
        NLINE = NLINE + 2
        NLPATDOS = NLINE
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
        DO JSUB =1,NSUB
         DO IOBS = 1,NOBS(JSUB)
          DO J = 2,NUMEQT+1
           WRITE(21,*) JSUB, OBSBLOCK(JSUB,IOBS,1), J-1,
     1      OBSBLOCK(JSUB,IOBS,J), (ASSAYC(JSUB,J-1,K),K=1,4)
           NLINE = NLINE + 1
          END DO
         END DO
        END DO
        REWIND(21)
        OPEN(22,FILE=OUTFILER)
  250   READ(21,2) READLINE
        IF(READLINE(12:22) .NE. '# START PAR') THEN
         CALL CONDENSE2(READLINE)
         GO TO 250
        ENDIF
        WRITE(22,186) NLRANPAR
  186   FORMAT(2X,I10,'   # START PAR')
  260   READ(21,2) READLINE
        IF(READLINE(12:25) .NE. '# START PARFIX') THEN
         CALL CONDENSE2(READLINE)
         GO TO 260
        ENDIF
        WRITE(22,187) NLFIXPAR
  187   FORMAT(2X,I10,'   # START PARFIX')
  960   READ(21,2) READLINE
        IF(READLINE(12:28) .NE. '# START PARRANFIX') THEN
         CALL CONDENSE2(READLINE)
         GO TO 960
        ENDIF
        WRITE(22,1187) NLRANFIXPAR
 1187   FORMAT(2X,I10,'   # START PARRANFIX')
  270   READ(21,2) READLINE
        IF(READLINE(12:21) .NE. '# START AB') THEN
         CALL CONDENSE2(READLINE)
         GO TO 270
        ENDIF
        WRITE(22,188) NLAB
  188   FORMAT(2X,I10,'   # START AB')
 1280   READ(21,2) READLINE
        IF(READLINE(12:25) .NE. '# START VALFIX') THEN
         CALL CONDENSE2(READLINE)
         GO TO 1280
        ENDIF
        WRITE(22,189) NLFIXVAL
  189   FORMAT(2X,I10,'   # START VALFIX')
1380   READ(21,2) READLINE
        IF(READLINE(12:28) .NE. '# START RANFIXEST') THEN
         CALL CONDENSE2(READLINE)
         GO TO 1380
        ENDIF
        WRITE(22,1189) NLRANFIXVAL
 1189   FORMAT(2X,I10,'   # START RANFIXEST')
  840   READ(21,2) READLINE
        IF(READLINE(12:25) .NE. '# START COVARI') THEN
         CALL CONDENSE2(READLINE)
         GO TO 840
        ENDIF
        WRITE(22,841) NLCOVNAM
  841   FORMAT(2X,I10,'   # START COVARIATE NAMES')
 1290   READ(21,2) READLINE
        IF(READLINE(12:25) .NE. '# START CORDEN') THEN
         CALL CONDENSE2(READLINE)
         GO TO 1290
        ENDIF
        WRITE(22,191) NLCORDEN
  191   FORMAT(2X,I10,'   # START CORDEN')
2020   READ(21,2) READLINE
        IF(READLINE(12:37) .NE. '# START BAYESIAN POSTERIOR') THEN
         CALL CONDENSE2(READLINE)
         GO TO 2020
        ENDIF
        WRITE(22,2014) NLBAYPOS
 2014   FORMAT(2X,I10,'   # START BAYESIAN POSTERIOR DENSITIES')
 1310   READ(21,2) READLINE
        IF(READLINE(12:24) .NE. '# START PYJGX') THEN
         CALL CONDENSE2(READLINE)
         GO TO 1310
        ENDIF
        WRITE(22,192) NLPYJGX
  192   FORMAT(2X,I10,'   # START PYJGX')
  320   READ(21,2) READLINE
        IF(READLINE(12:27) .NE. '# START YPREDPOP') THEN
         CALL CONDENSE2(READLINE)
         GO TO 320
        ENDIF
        WRITE(22,193) NLYPREDPOP
  193   FORMAT(2X,I10,'   # START YPREDPOP')
  330   READ(21,2) READLINE
        IF(READLINE(12:27) .NE. '# START YPREDBAY') THEN
         CALL CONDENSE2(READLINE)
         GO TO 330
        ENDIF
        WRITE(22,194) NLYPREDBAY
  194   FORMAT(2X,I10,'   # START YPREDBAY')
  340   READ(21,2) READLINE
        IF(READLINE(12:25) .NE. '# START TTPRED') THEN
         CALL CONDENSE2(READLINE)
         GO TO 340
        ENDIF
        WRITE(22,196) NLTTPRED
  196   FORMAT(2X,I10,'   # START TTPRED')
  350   READ(21,2) READLINE
        IF(READLINE(12:28) .NE. '# START YPREDPOPT') THEN
         CALL CONDENSE2(READLINE)
         GO TO 350
        ENDIF
        WRITE(22,197) NLYPREDPOPT
  197   FORMAT(2X,I10,'   # START YPREDPOPT')
  360   READ(21,2) READLINE
        IF(READLINE(12:22) .NE. '# START EXX') THEN
         CALL CONDENSE2(READLINE)
         GO TO 360
        ENDIF
        WRITE(22,198) NLEXX
  198   FORMAT(2X,I10,'   # START EXX')
  370   READ(21,2) READLINE
        IF(READLINE(12:28) .NE. '# START CYCLE LOG') THEN
         CALL CONDENSE2(READLINE)
         GO TO 370
        ENDIF
        WRITE(22,199) NLCYCLOGLIK
  199   FORMAT(2X,I10,'   # START CYCLE LOG-LIKELIHOODS')
 1370   READ(21,2) READLINE
        IF(READLINE(12:28) .NE. '# START CYCLE AIC') THEN
         CALL CONDENSE2(READLINE)
         GO TO 1370
        ENDIF
        WRITE(22,1199) NLCYCAICBIC
 1199   FORMAT(2X,I10,'   # START CYCLE AIC AND BIC VALUES')
  380   READ(21,2) READLINE
        IF(READLINE(12:28) .NE. '# START CYCLE MEA') THEN
         CALL CONDENSE2(READLINE)
         GO TO 380
        ENDIF
        WRITE(22,201) NLCYCMEAN
  201   FORMAT(2X,I10,'   # START CYCLE MEANS')
  390   READ(21,2) READLINE
        IF(READLINE(12:28) .NE. '# START CYCLE STD') THEN
         CALL CONDENSE2(READLINE)
         GO TO 390
        ENDIF
        WRITE(22,202) NLCYCSTDEV
  202   FORMAT(2X,I10,'   # START CYCLE STD. DEVS.')
  410   READ(21,2) READLINE
        IF(READLINE(12:28) .NE. '# START CYCLE ADD') THEN
         CALL CONDENSE2(READLINE)
         GO TO 410
        ENDIF
        WRITE(22,203) NLCYCADDSTAT
  203   FORMAT(2X,I10,'   # START CYCLE ADDITIONAL STATISTICS')
  420   READ(21,2) READLINE
        IF(READLINE(12:28) .NE. '# START CYCLE GAM') THEN
         CALL CONDENSE2(READLINE)
         GO TO 420
        ENDIF
        WRITE(22,204) NLCYCGAM
  204   FORMAT(2X,I10,'   # START CYCLE GAMLAM VALUES')
  430   READ(21,2) READLINE
        IF(READLINE(12:31) .NE. '# START BAYESIAN LOG') THEN
         CALL CONDENSE2(READLINE)
         GO TO 430
        ENDIF
        WRITE(22,206) NLBAYLOGLIK
  206   FORMAT(2X,I10,'   # START BAYESIAN LOG-LIKELIHOODS')
  440   READ(21,2) READLINE
        IF(READLINE(12:31) .NE. '# START BAYESIAN MEA') THEN
         CALL CONDENSE2(READLINE)
         GO TO 440
        ENDIF
        WRITE(22,207) NLBAYMEAN
  207   FORMAT(2X,I10,'   # START BAYESIAN MEANS')
  450   READ(21,2) READLINE
        IF(READLINE(12:31) .NE. '# START BAYESIAN STD') THEN
         CALL CONDENSE2(READLINE)
         GO TO 450
        ENDIF
        WRITE(22,208) NLBAYSTD
  208   FORMAT(2X,I10,'   # START BAYESIAN STD. DEVS.')
  460   READ(21,2) READLINE
        IF(READLINE(12:31) .NE. '# START BAYESIAN ADD') THEN
         CALL CONDENSE2(READLINE)
         GO TO 460
        ENDIF
        WRITE(22,209) NLBAYADDSTAT
  209   FORMAT(2X,I10,'   # START BAYESIAN ADDITIONAL STATISTICS')
  480   READ(21,2) READLINE
        IF(READLINE(12:30) .NE. '# START PATIENT IDS') THEN
         CALL CONDENSE2(READLINE)
         GO TO 480
        ENDIF
        WRITE(22,212) NLPATID
  212   FORMAT(2X,I10,'   # START PATIENT IDS')
  490   READ(21,2) READLINE
        IF(READLINE(12:30) .NE. '# START PATIENT DOS') THEN
         CALL CONDENSE2(READLINE)
         GO TO 490
        ENDIF
        WRITE(22,213) NLPATDOS
  213   FORMAT(2X,I10,'   # START PATIENT DOSE COV. BLOCKS')
  510   READ(21,2) READLINE
        IF(READLINE(12:30) .NE. '# START PATIENT OUT') THEN
         CALL CONDENSE2(READLINE)
         GO TO 510
        ENDIF
        WRITE(22,214) NLPATOUTASSAY
  214 FORMAT(2X,I10,'   # START PATIENT OUTPUT AND ASSAY COEFF. BLOCKS')
  600   READ(21,222,IOSTAT=IEND) READLINE2
  222   FORMAT(A1000)
        IF(IEND .LT. 0) GO TO 700
        CALL CONDENSE3(READLINE2)
        GO TO 600
  700   CLOSE(21)
        CLOSE(22)
        RETURN
        END
	SUBROUTINE GETNUM(NUMEQT)
	IMPLICIT REAL*8(A-H,O-Z)
	CHARACTER READLINE*1000
    2   FORMAT(A1000)
   35	READ(27,2,IOSTAT=IEND) READLINE
	IF(IEND .LT. 0) THEN
	 WRITE(*,57)
   57    FORMAT(//' THE COMBINATION OUTPUT FILE YOU HAVE ENTERED TO'/
     1' THIS PROGRAM WAS NOT MADE BY A RECENT BIG NPAG PROGRAM.'//
     2' SUCH A FILE MUST HAVE CONCATENATED PATIENT DATA FILES HAVING'/
     3' A LINE WITH "NO. OF TOTAL OUTPUT EQUATIONS" IN COLUMNS 12:40.'//
     3' THE PROGRAM STOPS. '//)
	 STOP
	ENDIF
	IF(READLINE(12:40) .NE. 'NO. OF TOTAL OUTPUT EQUATIONS')GO TO 35
	BACKSPACE(27)
   13   FORMAT(T2,I5)
        READ(27,13) NUMEQT
	RETURN
	END
	SUBROUTINE GETNSUB2(NSUBTOT)
	CHARACTER READLINE*1000
    2   FORMAT(A1000)
   10   READ(25,2) READLINE
	ILINE=0
	 DO I=1,51
	  IF(READLINE(I:I+21) .EQ. 'CTS IN THE DATA SET IS') THEN
	   ILINE=1
	   GO TO 20
	  ENDIF
	 END DO
   20   IF(ILINE .EQ. 0) GO TO 10
	 IEND = 0
	 ISTART = 0
	  DO J = I+22, 72
	   IF(ISTART .EQ. 0 .AND. READLINE(J:J) .NE. ' ') ISTART = J
	   IF(ISTART .NE. 0 .AND. READLINE(J:J) .EQ. ' ') THEN
	    IEND = J-1
	    GO TO 30
	   ENDIF
	  END DO
   30	ISIZE = IEND-ISTART
	IF(ISIZE .GT. 3) THEN
	 WRITE(*,*)' NSUBTOT IS ',NSUBTOT,' WHICH IS TOO LARGE. '
	 WRITE(*,*)' THE PROGRAM STOPS. '
	 STOP
	ENDIF
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
	SUBROUTINE GETNSUB(NSUB)
	CHARACTER READLINE*1000
    2   FORMAT(A1000)
   10   READ(25,2) READLINE
	ILINE=0
	 DO I=1,51
	  IF(READLINE(I:I+21) .EQ. 'THE NO. OF SUBJECTS IS') THEN
	   ILINE=1
	   GO TO 20
	  ENDIF
	 END DO
   20   IF(ILINE .EQ. 0) GO TO 10
	 IEND = 0
	 ISTART = 0
	  DO J = I+22, 72
	   IF(ISTART .EQ. 0 .AND. READLINE(J:J) .NE. ' ') ISTART = J
	   IF(ISTART .NE. 0 .AND. READLINE(J:J) .EQ. ' ') THEN
	    IEND = J-1
	    GO TO 30
	   ENDIF
	  END DO
   30	ISIZE = IEND-ISTART
	IF(ISIZE .GT. 3) THEN
	 WRITE(*,*)' NSUB IS ',NSUB,' WHICH IS TOO LARGE. '
	 WRITE(*,*)' THE PROGRAM STOPS. '
	 STOP
	ENDIF
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
	SUBROUTINE GETIPATFF(IFILE,NSUBTOT,NSUB,MAXSUB,
     1   IPATVEC,IERRR)
	DIMENSION IPATVEC(MAXSUB)
	CHARACTER READLINE*1000
    3   FORMAT(A1000)
	NSUBB = 0
	NUMCUR = 0
 4210	READ(25,3,ERR=4200) READLINE
	CALL GETNUMSF2(1,READLINE,NSUBB,NSUBTOT,NUMCUR,ISTOP,
     1                  MAXSUB,IPATVEC)
	IF(ISTOP .EQ. -1) GO TO 4200
	IF(ISTOP .EQ. 1) GO TO 4210
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
        SUBROUTINE FILREDT(NOBSER,TO,YO,C0,C1,C2,C3,MAXOBDIM,NDRUG,ND,
     1   NADD)
        use npag_utils, only: maxnumeq,max_RS_J,max_input_dim
        IMPLICIT REAL*8(A-H,O-Z)
        DIMENSION YO(MAXOBDIM,MAXNUMEQ),RJUNK(max_RS_J),C0(MAXNUMEQ),
     1  C1(MAXNUMEQ),C2(MAXNUMEQ),C3(MAXNUMEQ),TO(MAXOBDIM)
       CHARACTER READLINE*1000
	DO I=1,7
	 READ(27,*)
	END DO
	READ(27,*)
	READ(27,*)
    2   FORMAT(A1)
      READ(27,*)
	READ(27,*)
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
   50	READ(27,1) READLINE
	IF(READLINE(1:25) .NE. 'ASSAY COEFFICIENTS FOLLOW') GO TO 50
	DO IEQ = 1,NUMEQT
	 READ(27,*) C0(IEQ),C1(IEQ),C2(IEQ),C3(IEQ)
	END DO
	RETURN
	END
	SUBROUTINE GETNUMSF2(IINCLUDE,READLINE,NSUBB,NSUBTOT,NUMCUR,
     1    ISTOP,MAXSUB,IPATVECC)
	DIMENSION IPATVECC(MAXSUB)
	CHARACTER READLINE*1000
	ISTOP = 1
	DO J = 1,70
	 IF(READLINE(J:J) .NE. ' ') GO TO 10
	END DO
	IF(NSUBB .EQ. 0) WRITE(*,1)
    1   FORMAT(/' THE INSTRUCTION OR OUTPUT FILE HAS AN IMPROPER BLANK'/
     1' LINE IN THE PATIENT NUMBER SECTION. ')
	ISTOP = -1
	RETURN
   10   CONTINUE
	DO J = 1,70
	 IF(READLINE(J:J) .NE. ' ') GO TO 20
	END DO
   20   ISTART = J
	IF(READLINE(ISTART:ISTART) .NE. '0') GO TO 30
	DO I = ISTART+1,70
	 IF(READLINE(I:I) .NE. ' ') GO TO 30
	END DO
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
     	DO I = ISTART+1,70
	 IF(READLINE(I:I) .EQ. ' ' .OR. READLINE(I:I) .EQ. ',' .OR.
     1      READLINE(I:I) .EQ. '-') GO TO 40
	END DO
   40   IEND = I-1
	CALL GETSUB2(READLINE,ISTART,IEND,ISUB,IERROR)
	IF(IERROR .EQ. -1) THEN
	 WRITE(*,7)
    7    FORMAT(/' THE INSTRUCTION OR OUTPUT FILE HAS AN IMPROPER '/
     1' LINE - WITH AN INVALID CHARACTER ON IT - IN THE PATIENT '/
     2' NUMBER SECTION.')
	 ISTOP = -1
	 RETURN
	ENDIF
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
	DO I = IEND+1,70
	 IF(READLINE(I:I) .NE. ' ') GO TO 50
	END DO
	NUMCUR = ISUB
	NSUBB = NSUBB + 1
	IPATVECC(NSUBB) = ISUB
	RETURN
   50   CONTINUE
	IF(READLINE(I:I) .EQ. ',') THEN
	 NUMCUR = ISUB
 	 NSUBB = NSUBB + 1
	 IPATVECC(NSUBB) = ISUB
	 DO J = I+1,70
	  IF(READLINE(J:J) .NE. ' ') GO TO 60
	 END DO
	 RETURN
   60    ISTART = J
	 GO TO 30
	ENDIF
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
         ISTART = I
	 GO TO 30
	ENDIF
	IF(READLINE(I:I) .EQ. '-') THEN
	 NUMCUR1 = ISUB
	 DO J = I+1,70
	  IF(READLINE(J:J) .NE. ' ') GO TO 70
	 END DO
	 WRITE(*,8)
    8    FORMAT(/' THE INSTRUCTION OR OUTPUT FILE HAS AN IMPROPER '/
     1' LINE IN IT IN THE PATIENT NUMBER SECTION.'//
     2' A LINE HAS BEEN ENDED WITH A DASH.')
	 ISTOP = -1
	 RETURN
   70   ISTART = J
     	DO K = ISTART+1,70
	 IF(READLINE(K:K) .EQ. ' ' .OR. READLINE(K:K) .EQ. ',')
     1    GO TO 80
	END DO
   80   IEND = K-1
	CALL GETSUB2(READLINE,ISTART,IEND,ISUB,IERROR)
	IF(IERROR .EQ. -1) THEN
	 WRITE(*,7)
	 ISTOP = -1
	 RETURN
	ENDIF
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
	 NUMCUR = ISUB
	 NN = NSUBB
 	 NSUBB = NSUBB + (NUMCUR - NUMCUR1) + 1
	 NONEW = 0
	 DO K = NN+1,NSUBB
	  NONEW = NONEW + 1
	  IPATVECC(K) = NUMCUR1 - 1 + NONEW
	 END DO
	 DO J = IEND+1,70
	  IF(READLINE(J:J) .NE. ' ' .AND. READLINE(J:J) .NE. ',' )
     1    GO TO 90
	 END DO
	 RETURN
   90    ISTART = J
	 GO TO 30
	ENDIF
	WRITE(*,7)
	ISTOP = -1
	RETURN
	END
	SUBROUTINE GETSUB2(READLINE,ISTART,IEND,ISUB,IERROR)
	CHARACTER READLINE*1000
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
        SUBROUTINE CONVERGE2(NCYCLE,XLOGLIK,XMEAN,STDEV,INDXSD,AICBIC,
     1   PRCFVR,ACTPTS,SCALNFO,GAMLAM,AGE,HEIGHT,
     2   SUBMEAN,SUBLOGLIK,SUBSTD,SUBPERCOF,
     3   NAME,CHARTNO,SEX,NDD,NI,ASSAYC,IERRMOD)
        use npag_utils, only: maxnumeq,max_pop_rand_varbs
        IMPLICIT REAL*8(A-H,O-Z)
        CHARACTER READLINE*1000,NAME(800)*53,CHARTNO(800)*53,SEX(800)*1
        DIMENSION XLOGLIK(9997),XMEAN(9997,max_pop_rand_varbs),
     1   AICBIC(9997,2),
     2   STDEV(9997,max_pop_rand_varbs),PRCFVR(9997,max_pop_rand_varbs),
     3   ACTPTS(9997),SCALNFO(9997),GAMLAM(9997),
     4   AGE(800),HEIGHT(800),SUBMEAN(800,max_pop_rand_varbs),
     5   SUBLOGLIK(800),
     5   SUBSTD(800,max_pop_rand_varbs),
     6   SUBPERCOF(800,max_pop_rand_varbs),
     7   NDD(800),ASSAYC(800,MAXNUMEQ,4)
        REWIND(25)
    2   FORMAT(A1000)
        ILOC=2
   50   READ(25,2) READLINE
        IF(ILOC .EQ. 2) GO TO 202
        IF(ILOC .EQ. 3) GO TO 203
  202   IF(READLINE(2:29) .EQ. 'STATISTICS FOR THE VARIABLES') THEN
	  BACKSPACE(25)
	  BACKSPACE(25)
	  READ(25,53) NVAR
   53     FORMAT(T6,I2)
	  ILOC=3
	  REWIND(25)
	ENDIF
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
	    READ(25,*)
	  END DO
	    GO TO 60
	ENDIF
	GO TO 50
   60	INDXLOG = 0
      INDAICBIC = 0
	INDXPTS = 0
	INDXNFO = 0
	INDXMEAN = 0
	INDXSD = 0
	INDXPRCF = 0
      INDGAM = 0
   10	READ(25,2,IOSTAT=IEND) READLINE
        IF(IEND .LT. 0) THEN
         WRITE(*,217)
  217    FORMAT(/' SOMETHING IS WRONG WITH THE OUTPUT FILE ENTERED;'/
     1' THERE ARE NO BAYESIAN RESULTS. THE PROGRAM STOPS.'//)
         CALL PAUSE
         STOP
        ENDIF
      IF(READLINE(2:31) .EQ. 'THE BAYESIAN POSTERIOR DENSITY') GO TO 100
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
	IF(READLINE(20:30) .NE. '           ') THEN
	  IOLD=-1
	  BACKSPACE(25)
	  READ(25,19) (XMEAN(INDXMEAN,J),J=1,NVAR)
   19   FORMAT(T17,7G13.6)
	ELSE
	  IOLD=1
	  READ(25,*)
	  READ(25,*) (XMEAN(INDXMEAN,J),J=1,NVAR)
	ENDIF
	GO TO 10
	ENDIF
        IF(READLINE(2:28) .EQ. 'THE STANDARD DEVIATIONS ARE') THEN
         INDXSD = INDXSD+1
	 IF(IOLD .EQ. 1) READ(25,*)
	 READ(25,*) (STDEV(INDXSD,J),J=1,NVAR)
	 GO TO 10
	ENDIF
        IF(READLINE(2:12) .EQ. 'THE PERCENT') THEN
	 INDXPRCF = INDXPRCF+1
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
        INDSUB = 0
  110	  READ(25,2,IOSTAT=IEND) READLINE
        IF(IEND .LT. 0) GO TO 200
        IF(READLINE(11:41) .EQ. '(NUMERICAL) LOG-LIKELIHOOD OF T') THEN
         INDSUB = INDSUB + 1
         READ(25,*)
         READ(25,*)
         READ(25,*) SUBLOGLIK(INDSUB)
         GO TO 110
        ENDIF
        IF(READLINE(2:10) .EQ. 'THE MEANS' .OR.
     1     READLINE(3:11) .EQ. 'THE MEANS') THEN
         READ(25,*)
         READ(25,*) (SUBMEAN(INDSUB,J),J=1,NVAR)
         GO TO 110
        ENDIF
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
        IF(READLINE(2:13) .EQ. 'THE PERCENT ') THEN
         READ(25,*)
         READ(25,*) (SUBPERCOF(INDSUB,J),J=1,NVAR)
         GO TO 110
        ENDIF
        GO TO 110
  200   REWIND(27)
        INDSUB = 0
  210	  READ(27,2,IOSTAT=IEND) READLINE
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
         BACKSPACE(27)
    3    FORMAT(T2,I5)
         READ(27,3) NDRUG
         READ(27,3) NADD
         NI = 2*NDRUG + NADD
         READ(27,3) ND
         NDD(INDSUB) = ND
         GO TO 210
        ENDIF
        IF(READLINE(12:23) .EQ. 'NO. OF TOTAL') THEN
         BACKSPACE(27)
         READ(27,3) NUMEQT
         READ(27,3) M
         GO TO 210
        ENDIF
        IF(READLINE(1:25) .EQ. 'ASSAY COEFFICIENTS FOLLOW') THEN
         DO J = 1,NUMEQT
          READ(27,*) (ASSAYC(INDSUB,J,K),K=1,4)
         END DO
         GO TO 210
        ENDIF
        GO TO 210
        END
      SUBROUTINE GETCOVR2(NCOV,COVDESCR)
      USE npag_utils, only: max_covs
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER READLINE*1000,COVDESCR(max_covs)*20
    2   FORMAT(A20)
   33   FORMAT(A1000)
      REWIND(27)
   10   READ(27,33) READLINE
        IF(READLINE(12:28) .NE. 'NO. OF ADDITIONAL') GO TO 10
        BACKSPACE(27)
    3   FORMAT(T2,I5)
        READ(27,3) NADD
	NCOV = NADD
   20   READ(27,33) READLINE
        IF(READLINE(2:16) .NE. 'COVARIATE NAMES') GO TO 20
        IF(NCOV .GE. 1) THEN
         DO J = 1,NCOV
          READ(27,33) READLINE
          DO I = 3,20
           IF(READLINE(I:I) .EQ. ' ') GO TO 30
          END DO
   30     COVDESCR(J) = READLINE(1:I-1)
         END DO
        ENDIF
	REWIND(27)
	RETURN
	END
	SUBROUTINE GETICYCSTART(ICYCSTART)
	CHARACTER READLINE*1000
    2   FORMAT(A1000)
   10 READ(25,2) READLINE
	ILINE=0
	 DO I=1,51
	  IF(READLINE(I:I+21) .EQ. 'LE NO. FOR THIS RUN IS') THEN
	   ILINE=1
	   GO TO 20
	  ENDIF
	 END DO
   20 IF(ILINE .EQ. 0) GO TO 10
	 IEND = 0
	 ISTART = 0
	  DO J = I+22, 72
	   IF(ISTART .EQ. 0 .AND. READLINE(J:J) .NE. ' ') ISTART = J
	   IF(ISTART .NE. 0 .AND. READLINE(J:J) .EQ. ' ') THEN
	    IEND = J-1
	    GO TO 30
	   ENDIF
	  END DO
   30	ISIZE = IEND-ISTART
        IF(ISIZE .GT. 5) THEN
         WRITE(*,31)
   31    FORMAT(/' THE STARTING CYCLE NO IS LARGER THAN 999999, WHICH'/
     1' IS TOO LARGE.'//
     1' THE PROGRAM STOPS. ')
	   CALL PAUSE
         STOP
        ENDIF
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
        SUBROUTINE CONDENSE2(READLINE)
        CHARACTER READLINE*80
	DO IEND = 80,1,-1
	 IF(READLINE(IEND:IEND) .NE. ' ') GO TO 20
	END DO
   20   CONTINUE
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
        SUBROUTINE CONDENSE3(READLINE)
        CHARACTER READLINE*1000
	DO IEND = 1000,1,-1
	 IF(READLINE(IEND:IEND) .NE. ' ') GO TO 20
	END DO
   20   CONTINUE
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
        SUBROUTINE GETMAXCYCNO(IMAXCYC)
        CHARACTER READLINE*1000
    2   FORMAT(A1000)
   10 READ(25,2) READLINE
	ILINE=0
	 DO I=1,42
        IF(READLINE(I:I+30) .EQ. 'THE LAST CYCLE NO. WILL BE .LE.') THEN
	   ILINE=1
	   GO TO 20
	  ENDIF
	 END DO
   20 IF(ILINE .EQ. 0) GO TO 10
	 IEND = 0
	 ISTART = 0
	  DO J = I+31,72
	   IF(ISTART .EQ. 0 .AND. READLINE(J:J) .NE. ' ') ISTART = J
	   IF(ISTART .NE. 0 .AND. READLINE(J:J) .EQ. ' ') THEN
	    IEND = J-1
	    GO TO 30
	   ENDIF
	  END DO
   30	ISIZE = IEND-ISTART
        IF(ISIZE .GT. 5) THEN
         WRITE(*,31)
   31    FORMAT(/' THE MAXIMUM ENDING CYCLE NO IS LARGER THAN 999999,'/
     1' WHICH IS TOO LARGE.'//
     1' THE PROGRAM STOPS. ')
	   CALL PAUSE
         STOP
        ENDIF
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
      SUBROUTINE IDPC(JSUB,IG,NPX,PX,NBCOMP,SUMSQJ,NOBSER,NUMEQT,
     1   NDIM,MF,RTOL,ATOL,TIM,SIG,YO,RS,BS,
     2   INTLIST,IPAR,ObsError,RPAR,ERRFIL)
       USE npag_utils, only: maxnumeq,max_m_per_obs
     1   ,max_ODE_params,max_doses,max_ODE_comps,max_RS_J
     2   ,max_input_dim
        IMPLICIT none
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
          CALL SUMSQ(JSUB,IG,NPX,PX,NBCOMP,SUMSQJ,NOBSER,NUMEQT,
     1   NDIM,MF,RTOL,ATOL,TIM,SIG,YO,RS,BS,
     2   INTLIST,IPAR,ObsError,RPAR,ERRFIL)
        RETURN
	END
        SUBROUTINE SUMSQ(JSUB,IG,NPX,PX,NBCOMP,SSQ,NOBSER,NUMEQT,
     1   NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     2   INTLIST,IPAR,ObsError,RPAR,ERRFIL)
       USE npag_utils, only: maxnumeq, max_m_per_obs, max_ODE_params
     1   , max_doses,max_ODE_comps, max_RS_J, max_input_dim
     2   , k_prod_pr, k_sum_z_sq, i_skip_ig, i_do
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
        double precision, save,
     1    dimension(max_m_per_obs*MAXNUMEQ*MAXNUMEQ) :: F
        integer NUMRES, I, IinF
!$omp Threadprivate(F)
        CALL FUNC(JSUB,IG,NOBSER,F,NPX,PX,NBCOMP,
     1   NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     2   INTLIST,IPAR,ObsError,RPAR,ERRFIL)
        if (IPAR(i_skip_ig).eq.0) then
          SSQ = 10.73d99
          return
        endif
        NUMRES=INTLIST(9)*INTLIST(10)
        SSQ=0.0D0
        DO 10 I=1,NUMRES
 10       SSQ=SSQ+F(I)*F(I)
        RETURN
        END
      SUBROUTINE FUNC(JSUB,IG,M,F,NPX,PX,NBCOMP,
     1   NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     2   INTLIST,IPAR,ObsError,RPAR,ERRFIL)
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
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
      CHARACTER ERRFIL*20
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
       double precision UseNormal, P_thresh
       double precision YOBSERVED, YMEAN, T, TOUT, DOSEINT
       integer I, J, ICONV, ID, III, ISAME, ISTEADY
       integer KNTM1, N, NN, NSET, NTL
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
       double precision, dimension(maxnumeq) :: YT
       double precision, dimension(max_input_dim) :: TLAG
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
       N = NDIM
      do III=1,max_ODE_comps
         X(iii) = 0.0D0
      end do
      do III=1,max_m_per_obs
         TIMO(III)=TIMCOPY(III)
      end do
      KNS=1
      KNT=1
      T=0.0D0
      ISKIPBOL = 0
      DO I = 1,INTLIST(5)
       R(2*I-1) = 0.D0
      END DO
!  AS OF idm1x17.f, ESTABLISH BSO(.,.), AND THEN USE BSO,RSO,SIGO,
!  AND NDO RATHER THAN BS,RS,SIG, AND ND FOR ALL CODE BELOW.
      DO I=1,INTLIST(8)
       DO J = 1,INTLIST(5)
        BSO(I,J)=RSCOPY(I,2*J)
       END DO
      END DO
      DO I=1,INTLIST(7)
       R(I)=RSCOPY(KNS,I)
       RCOPY(I)=RSCOPY(KNS,I)
      END DO
	 CALL GETFA(FA,X,PX,R,B,INTLIST)
         NDO = intlist(8)
         DO I=1,INTLIST(8)
          DO J=1,INTLIST(7)
           RSO(I,J) = RSCOPY(I,J)
          END DO
          SIGO(I) = SIGCOPY(I)
         END DO
        IF(NDIM .NE. 0) CALL GETIX(NDIM,X,PX,R,B,INTLIST)
   75	 CALL GETTLAG(TLAG,X,PX,R,B,INTLIST)
      NTL = 0
      DO ID = 1,INTLIST(5)
       IF(TLAG(ID) .NE. 0) NTL = 1
      END DO
        IF(NTL .EQ. 1) THEN
          CALL SHIFT(TLAG,NDO,SIGO,INTLIST(5),INTLIST(6),
     1      RSO,INTLIST)
          DO I=1,NDO
            DO J=1,INTLIST(5)
              BSO(I,J)=RSO(I,2*J)
            END DO
          END DO
        ENDIF
        JUMPTO45 = 0
        IF(TIMO(KNT).LT.SIGO(KNS)) then
          IF(TIMO(KNT).eq.0.0D0) then
            CALL OUTPUT(0.D0,YT,X,RPAR,IPAR)
            DO 2000 I=1,INTLIST(9)
2000          Y(KNT,I)=YT(I)
            KNT=KNT+1
          ENDIF
          JUMPTO45 = 1
        ELSE
12        IF(TIMO(KNT).EQ.SIGO(KNS)) then
            IF(TIMO(KNT).EQ.0.0D0) then
              CALL OUTPUT(0.D0,YT,X,RPAR,IPAR)
              DO 2005 I=1,INTLIST(9)
2005            Y(KNT,I)=YT(I)
              KNT=KNT+1
            else
              JUMPTO45 = 1
            end if
          END IF
13        IF(SIGO(KNS) .GT. 0.0D0) JUMPTO45 = 1
        END IF
      ISTEADY = 0
      if (JUMPTO45.eq.0) then
      IF(SIGO(KNS) .LT. 0.D0) THEN
        ISTEADY = 1
        NSET = 1
        DOSEINT = -SIGO(KNS)
        SIGO(KNS) = 0
      ENDIF
      DO I=1,INTLIST(7)
       R(I)=RSO(KNS,I)
      END DO
      IF(INTLIST(5) .NE. 0) then
        CALL GETFA(FA,X,PX,R,B,INTLIST)
        IF(NDIM .NE. 0) then
          DO I=1,INTLIST(5)
            X(NBCOMP(I))=X(NBCOMP(I))+BSO(KNS,I)*FA(I)
          END DO
        else
120       DO I=1,INTLIST(5)
            B(I)=BSO(KNS,I)*FA(I)
          END DO
        end if
      end if
81    KNS = KNS+1
      ENDIF
! NEW PARALLEL CODE BELOW AS OF npageng28.f
!
!45    IF(KNS .GT. NDO) GO TO 15
! Replaced by below
 45   Continue
 46   IF(KNS .GT. NDO) then
        ID=0
        TOUT=TIMO(KNT)
        KNT=KNT+1
      ELSE
      IF(TIMO(KNT) .EQ. 0.D0 .AND. KNT .GT. 1) THEN
        DO IKNS = KNS,NDO
          IF(SIGO(IKNS) .LE. 0.D0) then
            KNS = IKNS
            exit
          endif
        END DO
        if (SIGO(IKNS) .gt. 0.D0) then
          XVERIFY(1) = SIGO(KNS)
          CALL VERIFYVAL(1,XVERIFY)
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
          WRITE(42,111) INTLIST(8),KNS,XVERIFY(1)
          DO I = 1,NDO
            WRITE(*,*) SIGO(I)
            WRITE(25,*) SIGO(I)
            WRITE(42,*) SIGO(I)
          END DO
          CLOSE(42)
          CALL PAUSE
          STOP
        endif
        DO I=1,INTLIST(7)
          R(I)=RSO(KNS,I)
        END DO
        CALL GETIX(NDIM,X,PX,R,B,INTLIST)
        T = 0.D0
        ISTEADY = 0
        IF(SIGO(KNS) .LT. 0.D0) THEN
          ISTEADY = 1
          NSET = 1
          DOSEINT = -SIGO(KNS)
          SIGO(KNS) = 0
        ENDIF
      ENDIF
      IF(TIMO(KNT) .EQ. SIGO(KNS)) then
        ID=2
        TOUT=TIMO(KNT)
        KNT=KNT+1
        KNS=KNS+1
      else
20    IF(TIMO(KNT) .LE. SIGO(KNS) .OR. SIGO(KNS) .LE. 0) then
15      ID=0
        TOUT=TIMO(KNT)
        KNT=KNT+1
      else
25      ID=1
        TOUT=SIGO(KNS)
        KNS=KNS+1
      endif
      endif
      endif
      IF(NDIM .EQ. 0) GO TO 31
30    CONTINUE
        do III=1,max_ODE_params
          RPAR(k_dvode_reserved + III) = PX(III)
        end do
        do III = 1,max_RS_J
          RPAR(k_p_end + III) = R(III)
        end do
        RPAR(k_jsub) = dble(JSUB)
        RPAR(k_ig) = dble(IG)
        do III = 1,10
          IPAR(i_dvode_reserved + III) = INTLIST(III)
        end do
        IPAR(i_jsub) = JSUB
        IPAR(i_ig) = IG
32      IF(NDIM .NE. -1) then
          CALL USERANAL(JSUB,IG,X,T,TOUT,
     1      NDIM,MF,RTOL,ATOL,PX,R,INTLIST,IPAR,RPAR)
          do III = 1,max_ODE_comps
            if (X(III)<0.D0) X(III) = 0.D0
          end do
          if (IPAR(i_skip_ig).eq.0) return
        endif
        IF(NDIM .EQ. -1) CALL ANAL3(X,T,TOUT,RPAR,IPAR)
      IF(ISTEADY .EQ. 1) THEN
       CALL THESAME(TOUT,DOSEINT*NSET,ISAME)
       IF(ISAME .EQ. 1) THEN
        NN = NDIM
        IF(NDIM .EQ. -1) NN = 3
        DO J = 1,NN
         XSTORE(NSET,J) = X(J)
        END DO
        IF(NSET .GE. 5) THEN
         CALL PREDLAST3(NN,NSET,XSTORE,XPRED,ICONV)
         IF(ICONV .EQ. 1) THEN
          ISTEADY = 0
          DO J = 1,NN
           X(J) = XPRED(J)
          END DO
          T = 100.D0*DOSEINT
          KNSOLD=KNS
          KNS = INTLIST(8) + 1
          DO I = KNSOLD,INTLIST(8)
           IF(SIGO(I) .GE. 100.D0*DOSEINT .OR. SIGO(I) .LE. 0.D0) THEN
            KNS = I
            EXIT
           ENDIF
          END DO
  200     CONTINUE
          ISKIPBOL = 1
         ENDIF
        ENDIF
        NSET = NSET + 1
       ENDIF
      ENDIF
31      CONTINUE
        IF(ID .NE. 1) then
          KNTM1=KNT-1
          CALL OUTPUT(TIMO(KNTM1),YT,X,RPAR,IPAR)
          DO 2010 I=1,INTLIST(9)
2010        Y(KNTM1,I)=YT(I)
        endif
55      IF (ID.NE.0) then
  35    CONTINUE
            IF(INTLIST(7) .NE. 0) then
              DO I=1,INTLIST(7)
                R(I)=RSO(KNS-1,I)
              END DO
              CALL GETFA(FA,X,PX,R,B,INTLIST)
            endif
83          IF(INTLIST(5) .NE. 0 .AND. NDIM .NE. 0) then
              IF(ISKIPBOL .EQ. 0) THEN
                DO I=1,INTLIST(5)
                  X(NBCOMP(I))=X(NBCOMP(I))+BSO(KNS-1,I)*FA(I)
                END DO
              ENDIF
              ISKIPBOL = 0
            ENDIF
        endif
40    IF(KNT .LE. INTLIST(10)) GO TO 46
      if (UseInlineDO140.eq.1) then
        NNORMALOBS = 0
        NPOISSONOBS = 0
        MISVAL = 0
        SIGFAC = 1.D0
        OFAC = 0.D0
        RPAR(k_sum_z_sq) = 0.D0
        RPAR(k_prod_pr) = 0.D0
        RPAR(k_sfac) = SIGFAC
        RPAR(k_ofac) = OFAC
        IPAR(i_misval) = MISVAL
        IPAR(i_Nnormalobs) = NNORMALOBS
        IPAR(i_Npoissonobs) = NPOISSONOBS
        DO J=1,INTLIST(9)     ! NUMEQT or number of output equations
         DO I=1,INTLIST(10)   ! Number of measurements
          if(YO(I,J) .EQ. -99) then
           F((J-1)*INTLIST(10)+I) = 0.D0
           MISVAL = MISVAL+1
           YOBSERVED = -99
           YMEAN = Y(I,J)
           ObsError(I,J) = 0.D0
           ZSCORE = 0.D0
          else
           IF (IPAR(i_is_log10 + J) == 10) THEN
            YOBSERVED = 10**(YO(I,J))
            YMEAN = 10**Y(I,J)
           ELSE
            YOBSERVED = YO(I,J)
            YMEAN = Y(I,J)
           ENDIF
           UseNormal = 128.0
           IF ( (IPAR(i_is_poisson_obs + J) == 229)
     1       .and. (YOBSERVED .lt. UseNormal) )  THEN
           P_thresh = 32.0
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
                 poissonprob = exp( -1.D0 * YMEAN )
     1              * ( YMEAN**YOBSERVED ) / gamma( YOBSERVED + 1)
                 if (poissonprob .gt. 0.0000001 ) then
                    RPAR(k_prod_pr) = RPAR(k_prod_pr)
     1                 + log10(PoissonProb)
                    write (*,*) JSUB,IG,I,J,"CAUGHT PoissonProb="
     1                 ,PoissonProb
                 else
                    write (*,*) JSUB,IG,I,J,"PoissonProb -> 0.D0"
     1                ,PoissonProb
                 endif
              ENDIF
           ELSE
              ObsError(I,J) = RPAR(J+k_c0_base)
     1          + RPAR(J+k_c1_base)*YOBSERVED
     2          + RPAR(J+k_c2_base)*YOBSERVED*YOBSERVED
     3          + RPAR(J+k_c3_base)*YOBSERVED*YOBSERVED*YOBSERVED
              if(IPAR(i_errmod).eq.2) ObsError(i,j)
     1           = ObsError(i,j)*RPAR(k_gamma)
              if(IPAR(i_errmod).eq.4) ObsError(i,j)
     1           = RPAR(k_gamma)*RPAR(k_flat)
              if(IPAR(i_errmod).eq.3) ObsError(i,j)
     1           = dsqrt(ObsError(i,j)**2 + RPAR(k_gamma)**2)
              IF(ObsError(I,J) .EQ. 0) THEN
                WRITE(*,2345) JSUB
                WRITE(25,2345) JSUB
2345            FORMAT(//' A S.D. IS 0 FOR JSUB = ',I5,'. RERUN THE '/
     1' PROGRAM WITH C0 NOT = 0  FOR THIS SUBJECT, OR WITH THIS'/
     2' SUBJECT ELIMINATED.')
                CLOSE(27)
                CLOSE(25)
                OPEN(42,FILE=ERRFIL)
                WRITE(42,2345) JSUB
                CLOSE(42)
                CALL PAUSE
                STOP
              ENDIF
              IF(ObsError(I,J) .LT. 0) THEN
                WRITE(*,2346) JSUB,IG
                WRITE(25,2346) JSUB,IG
2346            FORMAT(//' A S.D. < 0 FOR JSUB,IG = ',I5,I6,'.     '/
     1'RERUN THE PROGRAM WITH A BETTER CHOICE FOR THE ASSAY ERROR  '/
     2'POLYNOMIAL COEFFICIENTS.')
                CLOSE(27)
                CLOSE(25)
                OPEN(42,FILE=ERRFIL)
                WRITE(42,2346) JSUB
                CLOSE(42)
                CALL PAUSE
                STOP
              ENDIF
              SIGFAC=SIGFAC*ObsError(I,J)
              NNORMALOBS=NNORMALOBS+1
              RPAR(k_sum_z_sq) = RPAR(k_sum_z_sq)
     1           + ((YOBSERVED-YMEAN)/ObsError(I,J))**2
           endif ! if(Poisson){...} else {Normal}
           ZSCORE=(YOBSERVED-YMEAN)/ObsError(I,J)
           F((J-1)*INTLIST(10)+I)=ZSCORE
          ENDIF ! if(YO(I,J) .EQ. -99){...} else {YO ~ {Normal,Poisson}}
         END DO ! number of measurements
        END DO ! number of output equations
        OFAC=2.506628274631**NNORMALOBS
        RPAR(k_sfac) = SIGFAC
        RPAR(k_ofac) = OFAC
        IPAR(i_misval) = MISVAL
        IPAR(i_Nnormalobs) = NNORMALOBS
        IPAR(i_Npoissonobs) = NPOISSONOBS
      else
        CALL DO140(1,JSUB,IG,INTLIST,IPAR,RPAR,F,ObsError,YO,Y,ERRFIL)
      endif
! NEW PARALLEL CODE BELOW AS OF npageng28.f
!	 ND = NDO
!	 DO I=1,INTLIST(8)
!	  SIGCOPY(I) = SIGO(I)
!	  DO J=1,NI
!	   RSCOPY(I,J) = RSO(I,J)
!	  END DO
!	 END DO
!         DO I=1,INTLIST(8)
!          DO J=1,INTLIST(5)
!           BSCOPY(I,J)=RSCOPY(I,2*J)
!	  END DO
!	 END DO
      RETURN
      END
        SUBROUTINE USERANAL(JSUB,IG,X,TIN,TOUT,
     1      NDIM,MF,RTOL,ATOL,P,R,INTLIST,IPAR,RPAR)
        use npag_utils, only: max_RS_J,max_ODE_params,max_ODE_comps
     1   , k_dvode_reserved, k_p_end, k_ig, k_jsub, i_ig, i_ig
     2   , i_jsub, i_skip_ig, k_dvode_reserved, i_dvode_reserved
        IMPLICIT none
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
        EXTERNAL DIFFEQ,JACOB
        real*8, dimension(1002) ::  RWORK
        integer, dimension(50) :: IWORK
        integer :: ITOL, ITASK, ISTATE, IOPT, LRW, LIW, III
!$omp ThreadPrivate (RWORK, IWORK)
        save RWORK, IWORK
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
! -------- START OF DVODE DECLARATIONS SPECIFIC ------------
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
!$omp Threadprivate(/DVOD01/,/DVOD02/)
      save /DVOD01/,/DVOD02/
! -------- END OF DVODE DECLARATIONS SPECIFIC -------------
        ITOL=2
        ITASK=1
        ISTATE=1
        IOPT=0
        LRW=1002
        LIW=50
        do III=1,max_ODE_params
           RPAR(k_dvode_reserved + III) = P(III)
        end do
        do III = 1,max_RS_J
           RPAR(k_p_end + III) = R(III)
        end do
           RPAR(k_jsub) = dble(JSUB)
           RPAR(k_ig) = dble(IG)
        do III = 1,10
           IPAR(i_dvode_reserved + III) = INTLIST(III)
        end do
           IPAR(i_jsub) = JSUB
           IPAR(i_ig) = IG
        if (TIN.eq.TOUT) write (*,*) "WARNING: TIN=TOUT; JSUB=",JSUB
        CALL DVODE(DIFFEQ,NDIM,X,TIN,TOUT,ITOL,RTOL,ATOL,ITASK,ISTATE,
     1            IOPT,RWORK,LRW,IWORK,LIW,JACOB,MF,RPAR,IPAR)
        IF (ISTATE .LT. 0) THEN
         IPAR(i_skip_ig) = 0
        ENDIF
        TIN=TOUT
        RETURN
        END
	SUBROUTINE JACOB(NDIM, T, X, ML, MU, PD, NRPD, RPAR, IPAR)
        use npag_utils, only: max_RS_J,max_ODE_params,max_ODE_comps
	IMPLICIT none
! NEW PARALLEL CODE BELOW AS OF npageng28.f
        integer NDIM, NRPD
        double precision T,X,ML,MU
        double precision, dimension(:,:) :: PD
        double precision, dimension(257) :: RPAR
        integer, dimension(257) :: IPAR
        RETURN
        END
        SUBROUTINE IDCALCY(JSUB,IG,NPP,NDIM,ESTML,YPRED,NUMEQT,NOBSER,
     1    MF,NBCOMP,RTOL,ATOL,TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     2    INTLIST,IPAR,ObsError,RPAR,ERRFIL)
       USE npag_utils, only: maxnumeq, max_m_per_obs, max_ODE_params
     1    , max_doses, max_ODE_comps, max_RS_J, max_input_dim
     2    , k_ig, k_jsub, k_dvode_reserved, k_p_end, i_ig, i_jsub
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
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
        CALL FUNC2(JSUB,IG,NOBSER,YPRED,NUMEQT,NBCOMP,
     1      NPP,ESTML,NDIM,MF,RTOL,ATOL,INTLIST,
     2      TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     3      ObsError,IPAR,RPAR,ERRFIL)
        RETURN
	END
	SUBROUTINE FUNC2(JSUB,IG,M,YPRED,NUMEQT,NBCOMP,
     1      NPP,ESTML,NDIM,MF,RTOL,ATOL,INTLIST,
     2      TIM,SIG,RS,BS,
     3      ObsError,IPAR,RPAR,ERRFIL)
       USE npag_utils, only: verifyval, shift, thesame, predlast3
     1   , maxnumeq, max_m_per_obs, max_SS_doses
     2   , max_ODE_params, max_doses, max_ODE_comps, max_RS_J
     3   , max_input_dim, k_dvode_reserved, k_p_end, k_jsub, k_ig
     4   , i_ig, i_jsub, i_dvode_reserved
      IMPLICIT REAL*8(A-H,O-Z)
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
      integer N,ND,NI,NUP,NUIC,NP
      integer NOS,NDRUG,NADD
      double precision, dimension(max_RS_J) :: R
      double precision, dimension(max_ODE_comps) :: B
      CHARACTER ERRFIL*20
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
      double precision, save, dimension(max_ODE_comps) :: X, XPRED
      double precision, save, dimension(100) :: XVERIFY
      double precision, save, dimension(max_SS_doses,max_ODE_comps)
     1  :: XSTORE
      double precision, save, dimension(max_doses) :: SIGO
      double precision, save, dimension(max_doses,max_RS_J) :: RSO
      double precision, save, dimension(max_input_dim) :: TLAG, FA
      double precision, save, dimension(MAXNUMEQ) :: YT
      double precision, save, dimension(max_m_per_obs,MAXNUMEQ) :: Y
       N = NDIM
       ND = intlist(8)
       NI = intlist(7)
       NP = NPP
       NDRUG = intlist(5)
       NADD = intlist(6)
       NOS = intlist(9)
      do III=1,max_ODE_comps
        X(III)=0.0
      end do
      KNS=1
      KNT=1
        T=0.0D0
      ISKIPBOL = 0
      DO I = 1,NDRUG
       R(2*I-1) = 0.D0
      END DO
      DO I=1,NI
       R(I)=RS(KNS,I)
      END DO
         CALL GETFA(FA,X,ESTML,R,B,INTLIST)
         NDO = ND
         DO I=1,ND
           SIGO(I) = SIG(I)
           DO J=1,NI
             RSO(I,J) = RS(I,J)
           END DO
         END DO
        IF(N .EQ. 0) GO TO 75
	 CALL GETIX(N,X,ESTML,R,B,INTLIST)
   75    CALL GETTLAG(TLAG,X,ESTML,R,B,INTLIST)
      NTL = 0
	DO ID = 1,NDRUG
	 IF(TLAG(ID) .NE. 0) NTL = 1
	END DO
	IF(NTL .EQ. 1) THEN
	 CALL SHIFT(TLAG,ND,SIG,NDRUG,NADD,RS,INTLIST)
      DO I=1,ND
       DO J=1,NDRUG
        BS(I,J)=RS(I,2*J)
       END DO
      END DO
	ENDIF
      IF(TIM(KNT).GE.SIG(KNS)) GO TO 12
      IF(TIM(KNT).NE.0.0D0) GO TO 45
      CALL OUTPUT(0.D0,YT,X,RPAR,IPAR)
	DO 2000 I=1,NOS
2000    Y(KNT,I)=YT(I)
        KNT=KNT+1
        GO TO 45
12      IF(TIM(KNT).GT.SIG(KNS)) GO TO 13
        IF(TIM(KNT).NE.0.0D0) GO TO 45
        CALL OUTPUT(0.D0,YT,X,RPAR,IPAR)
	  DO 2005 I=1,NOS
2005    Y(KNT,I)=YT(I)
        KNT=KNT+1
13      IF(SIG(KNS) .GT. 0.0D0) GO TO 45
      ISTEADY = 0
      IF(SIG(KNS) .LT. 0.D0) THEN
       ISTEADY = 1
       NSET = 1
       DOSEINT = -SIG(KNS)
       SIG(KNS) = 0
      ENDIF
      DO I=1,NI
       R(I)=RS(KNS,I)
      END DO
	IF(NDRUG .EQ. 0) GO TO 81
         CALL GETFA(FA,X,ESTML,R,B,INTLIST)
        IF(N .EQ. 0) GO TO 120
        DO I=1,NDRUG
	 X(NBCOMP(I))=X(NBCOMP(I))+BS(KNS,I)*FA(I)
	END DO
        GO TO 81
120   DO I=1,NDRUG
       B(I)=BS(KNS,I)*FA(I)
      END DO
81      KNS=KNS+1
45    IF(KNS.GT.ND) GO TO 15
      IF(TIM(KNT) .EQ. 0.D0 .AND. KNT .GT. 1) THEN
      DO IKNS = KNS,ND
       IF(SIG(IKNS) .LE. 0.D0) GO TO 110
      END DO
       XVERIFY(1) = SIG(KNS)
       CALL VERIFYVAL(1,XVERIFY)
       WRITE(*,111) ND,KNS,XVERIFY(1)
       WRITE(25,111) ND,KNS,XVERIFY(1)
 111  FORMAT(//' THE CURRENT SUBJECT HAS AN OBSERVATION TIME RESET'/
     1' ROW WITHOUT AN ACCOMPANYING DOSE RESET ROW. THE PROGRAM NOW'/
     2' STOPS. '//
     3' REVIEW YOUR PATIENT FILES AND CORRECT THE ERROR.'//
     4' NOTE THAT THE ',I4,' DOSE TIMES (POSSIBLY ALTERED BY TIMELAGS'/
     5' ARE THE FOLLOWING (AND THERE IS NO TIME .LE. 0 AFTER TIME'/
     6' NO. ',I4,' WHICH HAS CORRESPONDING TIME ',F15.4,'):')
       OPEN(42,FILE=ERRFIL)
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
      DO I=1,NI
       R(I)=RS(KNS,I)
      END DO
       CALL GETIX(N,X,ESTML,R,B,INTLIST)
       T = 0.D0
      ISTEADY = 0
      IF(SIG(KNS) .LT. 0.D0) THEN
       ISTEADY = 1
       NSET = 1
       DOSEINT = -SIG(KNS)
       SIG(KNS) = 0
      ENDIF
	ENDIF
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
        do III=1,max_ODE_params
          RPAR(k_dvode_reserved + III) = ESTML(III)
        end do
        do III = 1,max_RS_J
          RPAR(k_p_end + III) = R(III)
        end do
        RPAR(k_jsub) = dble(JSUB)
        RPAR(k_ig) = dble(IG)
        do III = 1,10
          IPAR(i_dvode_reserved + III) = INTLIST(III)
        end do
        IPAR(i_jsub) = JSUB
        IPAR(i_ig) = IG
32      IF(N .NE. -1) then
          CALL USERANAL(JSUB,IG,X,T,TOUT,
     1      NDIM,MF,RTOL,ATOL,ESTML,R,INTLIST,IPAR,RPAR)
        endif
        IF(N .EQ. -1) CALL ANAL3(X,T,TOUT,RPAR,IPAR)
        DO III = 1,max_ODE_comps
          if (X(III)<0.D0) X(III)=0.D0
        END DO
      IF(ISTEADY .EQ. 1) THEN
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
          ISTEADY = 0
          DO J = 1,NN
           X(J) = XPRED(J)
          END DO
          T = 100.D0*DOSEINT
          DO I = KNS,ND
           IF(SIG(I) .GE. 100.D0*DOSEINT .OR. SIG(I) .LE. 0.D0) THEN
            KNSNEW = I
            GO TO 100
           ENDIF
          END DO
          KNS = ND+1
          GO TO 200
  100     KNS = KNSNEW
  200     CONTINUE
          ISKIPBOL = 1
         ENDIF
        ENDIF
        NSET = NSET + 1
       ENDIF
      ENDIF
31      CONTINUE
      IF(ID.EQ.1) GO TO 35
	KNTM1=KNT-1
        CALL OUTPUT(TIM(KNTM1),YT,X,RPAR,IPAR)
        DO 2010 I=1,NOS
2010    Y(KNTM1,I)=YT(I)
55      IF(ID.EQ.0) GO TO 40
  35    CONTINUE
        IF(NI .EQ. 0) GO TO 83
        DO I=1,NI
         R(I)=RS(KNS-1,I)
        END DO
         CALL GETFA(FA,X,ESTML,R,B,INTLIST)
83      IF(NDRUG .EQ. 0 .OR. N .EQ. 0) GO TO 82
        IF(ISKIPBOL .EQ. 0) THEN
         DO I=1,NDRUG
          X(NBCOMP(I))=X(NBCOMP(I))+BS(KNS-1,I)*FA(I)
         END DO
        ENDIF
      ISKIPBOL = 0
82      CONTINUE
40      IF(KNT .LE. M) GO TO 45
	DO J=1,NOS
         DO I=1,M
	  YPRED(I,J) = Y(I,J)
	 END DO
	END DO
	 ND = NDO
	 DO I=1,ND
	  SIG(I) = SIGO(I)
	  DO J=1,NI
	   RS(I,J) = RSO(I,J)
	  END DO
	 END DO
         DO I=1,ND
          DO J=1,NDRUG
           BS(I,J)=RS(I,2*J)
	  END DO
	 END DO
      RETURN
      END
	SUBROUTINE IDCALCYY(JSUB,IG,NPP,NDIM,ESTML,TPRED,NUMT,YYPRED,
     1      NUMEQT,NOBSER,MF,NBCOMP,RTOL,ATOL,
     2      TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     3      INTLIST,IPAR,ObsError,RPAR,ERRFIL)
         use npag_utils, only : maxnumeq,max_m_per_obs,max_RS_J
     1     ,max_ODE_comps, max_input_dim, max_doses, max_ODE_params
        IMPLICIT REAL*8(A-H,O-Z)
        DIMENSION ESTML(max_ODE_params),YYPRED(71281,NUMEQT),
     1    TPRED(71281)
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
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
        CALL FUNC3(JSUB,IG,NUMT,YYPRED,TPRED,NUMEQT,NBCOMP,
     1      NPP,ESTML,NDIM,MF,RTOL,ATOL,
     2      TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     3      INTLIST,IPAR,ObsError,RPAR,ERRFIL)
        RETURN
	END
      SUBROUTINE FUNC3(JSUB,IG,NUMT,YYPRED,TPRED,NUMEQT,NBCOMP,
     1      NPP,ESTML,NDIM,MF,RTOL,ATOL,
     2      TIM,SIG,RS,BS,INTLIST,IPAR,ObsError,RPAR,ERRFIL)
         use npag_utils, only : shift, thesame, predlast3
     1    ,maxnumeq,max_m_per_obs,max_RS_J
     2    ,verifyval, max_SS_doses
     3    ,max_ODE_params, max_doses, max_ODE_comps, max_input_dim
     4    ,k_dvode_reserved, k_ig, k_jsub, k_p_end, i_jsub, i_ig
     5    ,i_dvode_reserved
      implicit none
      integer JSUB, IG, NUMT
      double precision, dimension(71281,NUMEQT) :: YYPRED
      real*8, dimension(71281) :: TPRED
      integer NUMEQT
      integer, dimension(max_input_dim) :: NBCOMP
      integer NPP
      real*8, dimension(max_ODE_params) :: ESTML
      integer NDIM,MF
      real*8 RTOL
      real*8, dimension(max_ODE_comps) :: ATOL
      real*8, dimension(max_m_per_obs) :: TIM
      real*8, dimension(max_doses) :: SIG
      real*8, dimension(max_doses,max_RS_J) :: RS
      real*8, dimension(max_doses,max_input_dim) :: BS
      integer, dimension(128) :: INTLIST
      integer, dimension(257) :: IPAR
      double precision, dimension(max_m_per_obs,MAXNUMEQ) :: ObsError
      double precision, dimension(257) :: RPAR
      CHARACTER ERRFIL*20
       integer N,ND,NI,NP
       integer NOS,NDRUG,NADD
      double precision, dimension(max_RS_J) :: R
      double precision, dimension(max_ODE_comps) :: B
      double precision, dimension(max_ODE_comps) :: X
      integer KNT,KNS,ID,NTL,ISKIPBOL,ISTEADY,IKNS,ICONV
      integer I,J,III,KNSNEW,KNTM1,NDO,ISAME
      double precision T,TOUT,DOSEINT
      double precision, dimension(100) :: XVERIFY
      integer NN, NSET
      double precision, dimension(max_SS_doses,max_ODE_comps) :: XSTORE
      double precision, dimension(max_ODE_comps) :: XPRED
      double precision, dimension(MAXNUMEQ) :: YT
      double precision, dimension(71281,MAXNUMEQ) :: Y
      double precision, dimension(7) :: TLAG,FA
      double precision, dimension(max_doses) :: SIGO
      double precision, dimension(max_doses,max_RS_J) :: RSO
      double precision constant001
      N = NDIM
      ND = intlist(8)
      NI = intlist(7)
      NP = NPP
      NOS = intlist(9)
      NDRUG = intlist(5)
      NADD = intlist(6)
        KNS=1
        KNT=1
        T=0.0D0
      ISKIPBOL = 0
      DO I = 1,NDRUG
       R(2*I-1) = 0.D0
      END DO
      DO I=1,NI
       R(I)=RS(KNS,I)
      END DO
         CALL GETFA(FA,X,ESTML,R,B,INTLIST)
	 NDO = ND
	 DO I=1,ND
	  SIGO(I) = SIG(I)
	  DO J=1,NI
	   RSO(I,J) = RS(I,J)
	  END DO
	 END DO
        IF(N .EQ. 0) GO TO 75
	 CALL GETIX(N,X,ESTML,R,B,INTLIST)
   75	 CALL GETTLAG(TLAG,X,ESTML,R,B,INTLIST)
      NTL = 0
	DO ID = 1,NDRUG
	 IF(TLAG(ID) .NE. 0) NTL = 1
	END DO
	IF(NTL .EQ. 1) THEN
	 CALL SHIFT(TLAG,ND,SIG,NDRUG,NADD,RS,INTLIST)
         DO I=1,ND
          DO J=1,NDRUG
           BS(I,J)=RS(I,2*J)
	  END DO
	 END DO
	ENDIF
      IF(TPRED(KNT).GE.SIG(KNS)) GO TO 12
      IF(TPRED(KNT).NE.0.0D0) GO TO 45
        CALL OUTPUT(0.D0,YT,X,RPAR,IPAR)
	DO 2000 I=1,NOS
2000    Y(KNT,I)=YT(I)
        KNT=KNT+1
        GO TO 45
12    IF(TPRED(KNT) .GT. SIG(KNS)) GO TO 13
      IF(TPRED(KNT) .NE. 0.0D0) GO TO 45
      CALL OUTPUT(0.D0,YT,X,RPAR,IPAR)
	DO 2005 I=1,NOS
2005  Y(KNT,I)=YT(I)
	KNT=KNT+1
13    IF(SIG(KNS) .GT. 0.0D0) GO TO 45
      ISTEADY = 0
      IF(SIG(KNS) .LT. 0.D0) THEN
       ISTEADY = 1
       NSET = 1
       DOSEINT = -SIG(KNS)
       SIG(KNS) = 0
      ENDIF
      DO I=1,NI
       R(I)=RS(KNS,I)
      END DO
	IF(NDRUG .EQ. 0) GO TO 81
         CALL GETFA(FA,X,ESTML,R,B,INTLIST)
        IF(N .EQ. 0) GO TO 120
        DO I=1,NDRUG
	 X(NBCOMP(I))=X(NBCOMP(I))+BS(KNS,I)*FA(I)
	END DO
        GO TO 81
120   DO I=1,NDRUG
       B(I)=BS(KNS,I)*FA(I)
      END DO
81      KNS=KNS+1
45    IF(KNS.GT.ND) GO TO 15
	IF(TPRED(KNT) .EQ. 0.D0 .AND. KNT .GT. 1) THEN
      DO IKNS = KNS,ND
       IF(SIG(IKNS) .LE. 0.D0) GO TO 110
      END DO
       XVERIFY(1) = SIG(KNS)
       CALL VERIFYVAL(1,XVERIFY)
      WRITE(*,111) ND,KNS,XVERIFY(1)
      WRITE(25,111) ND,KNS,XVERIFY(1)
 111  FORMAT(//' THE CURRENT SUBJECT HAS AN OBSERVATION TIME RESET'/
     1' ROW WITHOUT AN ACCOMPANYING DOSE RESET ROW. THE PROGRAM NOW'/
     2' STOPS. '//
     3' REVIEW YOUR PATIENT FILES AND CORRECT THE ERROR.'//
     4' NOTE THAT THE ',I4,' DOSE TIMES (POSSIBLY ALTERED BY TIMELAGS'/
     5' ARE THE FOLLOWING (AND THERE IS NO TIME .LE. 0 AFTER TIME'/
     6' NO. ',I4,' WHICH HAS CORRESPONDING TIME ',F15.4,'):')
       OPEN(42,FILE=ERRFIL)
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
      DO I=1,NI
       R(I)=RS(KNS,I)
      END DO
       CALL GETIX(N,X,ESTML,R,B,INTLIST)
       T = 0.D0
      ISTEADY = 0
      IF(SIG(KNS) .LT. 0.D0) THEN
       ISTEADY = 1
       NSET = 1
       DOSEINT = -SIG(KNS)
       SIG(KNS) = 0
      ENDIF
	ENDIF
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
        do III=1,max_ODE_params
          RPAR(k_dvode_reserved + III) = ESTML(III)
        end do
        do III = 1,max_RS_J
          RPAR(k_p_end + III) = R(III)
        end do
        RPAR(k_jsub) = dble(JSUB)
        RPAR(k_ig) = dble(IG)
        do III = 1,10
          IPAR(i_dvode_reserved + III) = INTLIST(III)
        end do
        IPAR(i_jsub) = JSUB
        IPAR(i_ig) = IG
32      IF(N .NE. -1) CALL USERANAL(JSUB,IG,X,T,TOUT,
     1      NDIM,MF,RTOL,ATOL,ESTML,R,INTLIST,IPAR,RPAR)
        IF(N .EQ. -1) CALL ANAL3(X,T,TOUT,RPAR,IPAR)
        DO III=1,max_ODE_comps
          if (X(III)<0.D0) X(III)=0.D0
        END DO
      IF(ISTEADY .EQ. 1) THEN
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
          ISTEADY = 0
          DO J = 1,NN
           X(J) = XPRED(J)
          END DO
          T = 100.D0*DOSEINT
          DO I = KNS,ND
           IF(SIG(I) .GE. 100.D0*DOSEINT .OR. SIG(I) .LE. 0.D0) THEN
            KNSNEW = I
            GO TO 100
           ENDIF
          END DO
          KNS = ND+1
          GO TO 200
  100     KNS = KNSNEW
  200     CONTINUE
          ISKIPBOL = 1
         ENDIF
        ENDIF
        NSET = NSET + 1
       ENDIF
      ENDIF
31      CONTINUE
      IF(ID.EQ.1) GO TO 35
	KNTM1=KNT-1
        CALL OUTPUT(TPRED(KNTM1),YT,X,RPAR,IPAR)
        DO 2010 I=1,NOS
2010    Y(KNTM1,I)=YT(I)
55      IF(ID.EQ.0) GO TO 40
  35    CONTINUE
        IF(NI .EQ. 0) GO TO 83
        DO I=1,NI
         R(I)=RS(KNS-1,I)
        END DO
         CALL GETFA(FA,X,ESTML,R,B,INTLIST)
83      IF(NDRUG .EQ. 0 .OR. N .EQ. 0) GO TO 82
        IF(ISKIPBOL .EQ. 0) THEN
         DO I=1,NDRUG
          X(NBCOMP(I))=X(NBCOMP(I))+BS(KNS-1,I)*FA(I)
         END DO
        ENDIF
      ISKIPBOL = 0
82      CONTINUE
40    IF(KNT  .LE. NUMT) GO TO 45
	DO J=1,NOS
         DO I=1,NUMT
	  YYPRED(I,J)=Y(I,J)
	 END DO
	END DO
	 ND = NDO
	 DO I=1,ND
	  SIG(I) = SIGO(I)
	  DO J=1,NI
	   RS(I,J) = RSO(I,J)
	  END DO
	 END DO
         DO I=1,ND
          DO J=1,NDRUG
           BS(I,J)=RS(I,2*J)
	  END DO
	 END DO
      RETURN
      END
      SUBROUTINE NPAG(MAXSUB,MAXGRD,MAXDIM,MAXACT,
     1  NUMEQT,MAXOBS,WORK,WORKK,SPXGYJ,DXI,PYJGX,PYJGXX,
     2  DENSTOR,EXX,CORDEN,CORHOLD,YPREDPOP,YPREDPOPT,YPREDBAY,CORDLAST)
        USE OMP_LIB
        USE npag_utils, only: makevec,check_input_array_size,expand_grid
     1   ,verifyval,cp_lrcs_to_rpar,max_input_dim,maxnumeq,max_m_per_obs
     2   ,max_ODE_params,max_pop_rand_varbs,max_doses,max_ODE_comps
     3   ,max_pop_varbs,max_pop_params,max_RS_J,k_gamma,k_flat
     4   ,k_sfac,k_ofac,k_sum_z_sq,k_prod_pr,i_skip_ig,i_do,i_cycle
     5   ,i_errmod,i_is_poisson_obs,i_is_log10,i_Npoissonobs,i_Jsub,i_IG
     6   ,k_resolve, print_matrix
      IMPLICIT REAL*8(A-H,O-Z)
	REAL*8 KU
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
      CHARACTER PREFIX*5,READLINE*300,EXT*3,NAME*4,
     1PRIFIL2*20,DENFIL*20,OUTFIL*20,PREDFIL*20,
     2OUTCOM*20,READLARG*1000,OUTFILER*20,ERRFIL*20
      character*20 ERRFILNAME
      character*20 ITFIL
!       COMMON/TOCALC/IRAN,PX,NOFIX,NSUB,gamma,flat,AB
!       COMMON/TOCALC/gamma,flat,AB,PX,IRAN,NOFIX,NSUB
      logical input_arrays_within_bounds
       integer nxe
       integer NVAR,NRANFIX
       integer, dimension(max_ODE_params) :: IRAN
       integer ijob
       integer NInDO1000
       real PInDO1000
       double precision, dimension(max_m_per_obs,NUMEQT) :: YPRED
       integer UseInlineDO140
       integer MISVAL, NNORMALOBS,NPOISSONOBS
       real*8  SIGFAC, OFAC
       integer IterFirst, IterLast, NInDO800, NNInDO800,NBadInDO800
       integer NPX, NOFIXCOPY,ThreadNo
       integer, dimension(max_ODE_params) :: IRANCOPY
       real*8 W, XXIG(max_pop_rand_varbs)
       real    PInDO800
       double precision, save, dimension(max_m_per_obs,MAXNUMEQ)
     1   :: ObsError
       integer, save, dimension(max_input_dim) :: NBCOMP
       integer NDIM, MF
       doubleprecision RTOL
       real*8, dimension(max_ODE_comps) :: ATOL
       real*8, dimension(max_RS_J) :: R, RCOPY
       real*8, dimension(max_ODE_params) :: P
       real*8, dimension(max_ODE_comps) :: B, BCOPY
       real*8, save, dimension(max_m_per_obs) :: TIMCOPY
       real*8, save, dimension(max_doses) :: SIGCOPY
       real*8, save, dimension(max_doses,max_RS_J) :: RSCOPY
       real*8, save, dimension(max_doses,max_input_dim) :: BSCOPY
       integer, save, dimension(128) :: INTLIST
       integer, save, dimension(257) :: IPAR
       double precision, save, dimension(257) :: RPAR
       integer alg_type
       integer isupres
        integer, dimension(100) :: NACTSUB
        real*8, dimension(100,1500,31) :: BAYPOS
        real*8, dimension(800,150,MAXNUMEQ+1) :: OBSBLOCK
        real*8, dimension(800,1000,35) :: DOSEBLOCK
        integer, dimension(800) :: NDORIG
!$omp ThreadPrivate(ObsError,IPAR,RPAR,INTLIST)
      EXTERNAL CALCRF
    2 FORMAT(A20)
  222 FORMAT(A3)
 2222 FORMAT(A5)
      input_arrays_within_bounds = check_input_array_size(
     1  MAXSUB,MAXGRD,MAXDIM,MAXACT,NUMEQT,MAXOBS,WORK,WORKK,
     2  SPXGYJ,DXI,PYJGX,PYJGXX,DENSTOR,EXX,CORDEN,CORHOLD,
     3  YPREDPOP,YPREDPOPT,YPREDBAY,CORDLAST)
      if (input_arrays_within_bounds .eqv. .false.) then
         write (*,*) "Input array error; exiting"
         return
      end if
2227        FORMAT(A11)
	OPEN(25,FILE='extnum',STATUS='OLD')
	READ(25,*) INUM
	CALL EQUIV(INUM,NAME)
	JNUM=INUM+1
	IF(JNUM .EQ. 10000) JNUM = 1
	BACKSPACE(25)
	WRITE(25,*) JNUM
	CLOSE(25)
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
		READ(23,*)
		DO IEQ=1,NUMEQT
		 READ(23,*) C0P(IEQ),C1P(IEQ),C2P(IEQ),C3P(IEQ)
     1                      ,C4P(IEQ),C5P(IEQ)
		END DO
		READ(23,*) ierrmod,gamlam0
                IPAR(i_errmod) = ierrmod
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
	IF(INDPTS .EQ. 1) NGRID=2129
	IF(INDPTS .EQ. 2) NGRID=5003
	IF(INDPTS .EQ. 3) NGRID=10007
	IF(INDPTS .EQ. 4) NGRID=20011
	IF(INDPTS .EQ. 5) NGRID=40009
	IF(INDPTS .EQ. 6) NGRID=80021
	IF(INDPTS .GT. 6) NGRID = (INDPTS - 100)*80021
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
        OPEN(27)
 1717  FORMAT(A300)
       NLAFIR = 0
       DO JSUB = 1,NSUB
 1720    READ(23,1717,IOSTAT=IEND) READLINE
	 IF(IEND .LT. 0) THEN
        WRITE(*,1721)
 1721   FORMAT(/' PATIENT DATA INFORMATION WAS NOT READ CORRECTLY'/
     1' FROM THE INSTRUCTION FILE, npag103.inp. IF YOU EDITED THIS'/
     2' FILE MANUALLY, PLEASE RERUN THE PC PREP PROGRAM TO HAVE IT'/
     3' PREPARE npag103.inp AGAIN AND THEN RERUN THIS PROGRAM.'//
     4' IF YOU DID NOT MANUALLY EDIT npag103.inp, PLEASE SEND THE'/
     5' DETAILS OF THIS RUN (STARTING WITH THE PC PREP EXECUTION) TO'/
     5' THE LAPK. '//
     6' THANK YOU.'/)
        OPEN(42,FILE=ERRFIL)
         WRITE(42,1721)
        CLOSE(42)
	  CALL PAUSE
	  STOP
	 ENDIF
	 IF(READLINE(3:16) .NE. 'LAST AND FIRST') GO TO 1720
	  NLAFIR = NLAFIR+1
	   IF(IPATVEC(JSUB) .GT. NLAFIR) GO TO 1720
       CALL NEWWORK1(MAXSUB,JSUB,TIMOBREL,
     1 DOSEBLOCK, OBSBLOCK, NDORIG , errfilname)
	END DO
 1730 REWIND(27)
	CLOSE(23)
	 IF(JSUB .LT. NSUB) THEN
	  WRITE(*,1721)
        OPEN(42,FILE=ERRFIL)
         WRITE(42,1721)
        CLOSE(42)
	  CALL PAUSE
	  STOP
	 ENDIF
	ISUPRES = 1
	OPEN(25,FILE=OUTFIL)
	IFIRST=ICYCLE+1
      open(91,file=ITFIL)
      if(ierrmod.eq.1.or.ierrmod.eq.2) write(91,9190)
      if(ierrmod.eq.4) write(91,9290)
      if(ierrmod.eq.3) write(91,9390)
 9290 format('  icycle',5x,'fobj1',10x,'flat wt',8x,'res',5x,
     &      'grid points (start and end)')
 9190 format('  icycle',5x,'fobj1',10x,'gamma',10x,'res',5x,
     &      'grid points (start and end)')
 9390 format('  icycle',5x,'fobj1',10x,'lambda',9x,'res',5x,
     &      'grid points (start and end)')
	OPEN(37,FILE='CHMAXCYC.OLD')
	 WRITE(37,*)'   1'
	CLOSE(37)
	OPEN(37,FILE='CHMAXCYC.NEW')
	 WRITE(37,*)'   0'
	CLOSE(37)
	OPEN(37,FILE='CHMAXCYC.BAT')
	 WRITE(37,*)'  COPY CHMAXCYC.NEW CHMAXCYC.OLD'
	CLOSE(37)
	WRITE(*,1231)
 1231   FORMAT(/' IF YOU WOULD LIKE TO STOP THE PROGRAM BEFORE THE'/
     1' MAXIMUM NO. OF CYCLES HAVE BEEN RUN, ALT-TAB TO A DOS'/
     2' WINDOW IN THE WORKING DIRECTORY OF THE RUN AND TYPE: '//
     3' >CHMAXCYC   '//
     4' THIS WILL CAUSE THE PROGRAM TO STOP SAFELY AT THE END OF THE '/
     5' NEXT CYCLE, AFTER CREATING THE OUTPUT FILES.')
!	CALL PAUSE
      VOLSPA=1.D0
      DO 170 I=1,NVAR
  170 VOLSPA = VOLSPA*(AB(I,2)-AB(I,1))
	IF(ICYCLE .EQ. 0) THEN
      CONST=1.D0/VOLSPA
	DO 30 IG = 1,NGRID
	  CORDEN(IG,NVAR+1)=CONST
	CALL CALGRD(NVAR,NGRID,AB,X,ERRFIL)
	DO J=1,NVAR
	  CORDEN(IG,J) = X(J)
	END DO
   30   CONTINUE
	NACTVE=NGRID
	ENDIF
      NACTLAST = NACTVE
        prefobj=-1.d30
        prebig=-1.d30
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
	CALL WRITEPT2(25,NSUB,IPATVEC)
	WRITE(25,*) '    0'
	WRITE(25,9763)
 9763   FORMAT(/' THE RANDOM VARIABLES AND THEIR RANGES ARE: ')
      DO I=1,NVAR
       XVERIFY(1) = AB(I,1)
       XVERIFY(2) = AB(I,2)
       CALL VERIFYVAL(2,XVERIFY)
       WRITE(25,1217) PAR(I),XVERIFY(1),XVERIFY(2)
	END DO
 1217   FORMAT(/' ',A11,': ',G17.10,'   TO   ',G17.10)
  	IF(NOFIX .EQ. 0) WRITE(25,9764)
 9764   FORMAT(/' NO FIXED PARAMETER VALUES.')
  	IF(NOFIX .GT. 0) THEN
       WRITE(25,9766)
 9766  FORMAT(/' THE USER-ENTERED FIXED PARAMETER VALUE(S) IS (ARE):')
      DO I=1,NOFIX
       XVERIFY(1) = VALFIX(I)
       CALL VERIFYVAL(1,XVERIFY)
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
      DO I=1,NRANFIX
       XVERIFY(1) = RANFIXEST(I)
       CALL VERIFYVAL(1,XVERIFY)
       WRITE(25,1219) PARRANFIX(I),XVERIFY(1)
	END DO
	WRITE(25,*)
  	ENDIF
	WRITE(25,9869) NGRID
 9869   FORMAT(/' THE NO. OF GRID POINTS IS '/,I10)
	WRITE(25,*)' THE NO. OF CURRENTLY ACTIVE GRID POINTS IS ',NACTVE
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
	WRITE(25,2112)
 2112   FORMAT(/' YOU HAVE CHOSEN TO MODEL THE ASSAY ERROR FUNCTION,'/
     1' S.D. AS FOLLOWS (ASSUMING SD1 = C0+C1*Y+C2*Y**2+C3*Y**3):')
     	 IF(IERRMOD .EQ. 1) WRITE(25,2113)
       XVERIFY(1) = GAMLAM0
       CALL VERIFYVAL(1,XVERIFY)
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
	LASTCYC = ICYCLE
	ITEST=0
	NSTORE=0
          resolve=0.20000000298023224
          rpar(k_resolve) = resolve
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
	OPEN(37,FILE='CHMAXCYC.OLD')
	 READ(37,*) ICONTIN
	CLOSE(37)
	IF(ICONTIN .EQ. 0) THEN
	 MAXCYC0 = MAXCYC
	 MAXCYC = ICYCLE
	ENDIF
10001 continue
      itest = 0
	IF(ITEST .EQ. 0 .AND. ICYCLE .NE. LASTCYC) THEN
	 WRITE(*,1237) ICYCLE
	 WRITE(25,1239) ICYCLE
	 LASTCYC = ICYCLE
	ENDIF
 1237   FORMAT(///' CYCLE NO.',I5,'. SUBJECT NOS FOLLOW: ')
 1239   FORMAT(///' CYCLE NO.',I5,/)
      DO 55 I=1,NACTVE
   55 SPXGYJ(I)=0.D0
!      SLPYJ=0.D0
	REWIND(27)
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
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,
     1    TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     2    INTLIST,ERRFILNAME)
        call cp_lrcs_to_rpar(gamma,flat,numeqt,c0,c1,c2,c3,c4,c5,rpar)
!         RPAR(k_gamma) = gamma
!         do III=1,numeq
!           RPAR(k_c0_base + III) = C0(III)
!           RPAR(k_c1_base + III) = C1(III)
!           RPAR(k_c2_base + III) = C2(III)
!           RPAR(k_c3_base + III) = C3(III) wmy2018.01.16 added C4 and C5 to list
!        end do
        if (NUMEQT.gt.7) then
           write (*,*) "WARNING :: NUMEQT.gt.7",
     1        NUMEQT
           CALL PAUSE
           STOP
        end if
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
	  IF(Y .EQ. -99) THEN
	   MISVAL = MISVAL+1
	   GO TO 140
	  ENDIF
             if (C4(J).eq.10) IPAR(i_is_log10+J) = 10
           if (C5(J).eq.229) then
             if (ICYCLE.lt.2) then
               write (*,*) "Poisson analysis req. for OUTEQ",J
             endif
             NPOISSONOBS=NPOISSONOBS+1
             ObsError(I,J)=1.D0
             SIG(I,J)=1.D0
             IPAR(i_is_poisson_obs+J)=229
          else
      SIG(I,J) = C0(J)+C1(J)*Y+C2(J)*Y*Y+C3(J)*Y**3
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
        OPEN(42,FILE=ERRFIL)
         WRITE(42,2345) JSUB
        CLOSE(42)
		CALL PAUSE
       	STOP
      ENDIF
 2346            FORMAT(//' A S.D. < 0 FOR JSUB = ',I5,'. RERUN THE '/
     1' PROGRAM WITH A BETTER CHOICE FOR THE ASSAY ERROR POLYNOMIAL'/
     2' COEFFICIENTS.')
      ObsError(I,J) = SIG(I,J)
      SIGFAC=SIGFAC*SIG(I,J)
       endif
  140 CONTINUE
        OFAC=2.506628274631**(NOBSER*NUMEQT - MISVAL - NPOISSONOBS)
        NOBTOT = NOBTOT + NOBSER*NUMEQT - MISVAL
        RPAR(k_sfac) = SIGFAC
        RPAR(k_ofac) = OFAC
        endif
 8888   FORMAT(' ',' CYCLE ',I5,',  SUBJECT ',I5,' ...  % COMPLETED = ',
     1F8.2)
	XNEXT = 1.D0
      if (ICYCLE .ge. 2) then
        if (NSTORE .le. NACTVE) then
          DO 750  IG=1,NSTORE
            WORK(IG)=PYJGX(JSUB,IG)*CORDEN(IG,NVAR+1)
	    WORKK(IG) = PYJGX(JSUB,IG)
  750     Continue
        endif
      endif
      if (ICYCLE .eq. 1) then
        IterFirst = 1
      else
        IterFirst = NSTORE + 1
      endif
      IterLast = NACTVE
      if ( IterLast > MAXACT ) IterLast = MAXACT + 1
      if (IterLast < 1) write (*,*) "IterLast < 1",
     1   "check INDPTS in (1...6;101,102,...)"
        do iparam=1,max_ODE_params
           IRANCOPY(iparam) = IRAN(iparam)
        end do
        NOFIXCOPY = NOFIX
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
!$omp Do
        DO 800 IG=IterFirst,IterLast
!$omp critical
           IPAR(i_skip_ig) = 1
           NInDO800 = NInDO800 + 1
           NNInDO800 = NNInDO800 + 1
           if (NNInDO800.eq.100) then
             PInDO800 = 100.0*REAL(NInDO800)
     1        /(REAL(IterLast) - REAL(IterFirst))
          NNInDO800 = 0
        end if
!$omp end critical
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
        DO J = 1,max_pop_rand_varbs
          XXIG(J) = 0.D0
        END DO
        DO J=1,NVAR
          XXIG(J)=CORDEN(IG,J)
        END DO
        CALL MAKEVEC(NVAR,NOFIXCOPY,NRANFIX,IRANCOPY,XXIG,VALFIX,
     1    RANFIXEST, PX)
        NPX = NVAR+NOFIXCOPY+NRANFIX
          W = 0.0
        CALL IDPC(JSUB,IG,NPX,PX,NBCOMP,W,NOBSER,NUMEQT,
     1    NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     2    INTLIST,IPAR,ObsError,RPAR,ERRFILNAME)
        if (IPAR(i_skip_ig).eq.1) then
          IF(IG .LE. MAXACT) PYJGX(JSUB,IG)=0.D0
          WORKK(IG) = 0.D0
          IF(RPAR(k_sum_z_sq) .LE. 22708.D0) THEN
           IF(IG .LE. MAXACT) THEN
            PYJGX(JSUB,IG) = 10**RPAR(k_prod_pr)
     1        * DEXP(-.5D0*RPAR(k_sum_z_sq))/RPAR(k_sfac)/RPAR(k_ofac)
           ENDIF
           WORKK(IG) = 10**RPAR(k_prod_pr)
     1        * DEXP(-.5D0*RPAR(k_sum_z_sq))/RPAR(k_sfac)/RPAR(k_ofac)
          ENDIF
          IF(IG .GT. MAXACT) THEN
           WORK(IG) = WORKK(IG)*CORDEN(IG,NVAR+1)
          ELSE
  700      WORK(IG)=PYJGX(JSUB,IG)*CORDEN(IG,NVAR+1)
           WORKK(IG) = PYJGX(JSUB,IG)
          ENDIF
        else
          NBadInDO800 = NBadInDO800 + 1
          PYJGX(JSUB,IG)=0.D0
          WORKK(IG) = 0.D0
          WORK(IG) = 0.D0
        endif
  800   CONTINUE
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp   End Do
!$omp   End Parallel
          write (*,*) "For JSUB:",JSUB,"Skipped",
     1       NBadInDO800, "of", NInDO800, "of", IterLast
      CALL NOTINT(VOLSPA,NGRID,NACTVE,WORK,MAXGRD,PYJ)
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
        OPEN(42,FILE=ERRFIL)
         WRITE(42,26) JSUB
        CLOSE(42)
        CALL PAUSE
        STOP
        ENDIF
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!       DO I=1,NACTVE
!         SPXGYJ(I)=SPXGYJ(I)+WORK(I)/PYJ
!       END DO
!       SLPYJ=SLPYJ+DLOG(PYJ)
 1000   CONTINUE
      IF(ICYCLE .EQ. 1) WRITE(*,1243)
 1243 FORMAT(/' The Adaptive Grid optimization process could take '/
     1' several minutes if you are analyzing a large no. of subjects'/
     2' with a large no. of grid points. '//
     3' An approximate measure of how close the process is to being'/
     4' completed will be printed below: '/)
      ISAVEDEN = 0
      IF(ISAVEDEN .EQ. 1 .AND. ICYCLE .NE. 1) THEN
        OPEN(33,FILE=DENFIL)
        WRITE(33,7124)
        WRITE(33,*) NDIM
        WRITE(33,*) INDPTS
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
        WRITE(33,*) NINT
        WRITE(33,*) ICYCLE
        WRITE(33,*) DORIG
        DO I=1,NACTVE
          WRITE(33,*) (CORDEN(I,J),J=1,NVAR+1)
        END DO
        CLOSE(33)
      ENDIF
      igamma = igamma + 1
      if(ierrmod.eq.1) igamma=1
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
      IF(IHESS .EQ. -1) GO TO 900
      fobj1 = fobj
      nactve1 = nactve
      IF(ISUPRES .EQ. 0)
     1 write(6,*) 'base job 1, fobj,keep,icycle=',fobj,keep,icycle
      ijob = 0
      nactve = keep
      call emint(pyjgx,maxsub,corden,maxgrd,nactve,nsub,ijob,
     &corden(1,nvar+1),denstor(1,1),denstor(1,2),denstor(1,3),
     &fobj,gap,nvar,keep,IHESS,errfil,ISUPRES)
      IF(IHESS .EQ. -1) GO TO 900
      fobjbase = fobj
      IF(ISUPRES .EQ. 0)
     1 write(6,*) 'base job 0, fobj,keep,icycle=',fobj,keep,icycle
      IF(ISUPRES .EQ. 0)
     1 write(6,*) 'base job 0 nactve,gamma=',nactve,gamma
      nactve0 = nactve
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
      if(mod(igamma,3).eq.2) then
      IF(ISUPRES .EQ. 0) write(6,*) 'gamma plus =',gamma
      ijob = 0
      call emint(pyjgx,maxsub,corden,maxgrd,nactve,nsub,ijob,
     &corden(1,nvar+1),denstor(1,1),denstor(1,2),denstor(1,3),
     &fobj,gap,nvar,keep,IHESS,errfil,ISUPRES)
      IF(IHESS .EQ. -1) GO TO 900
      fobjplus = fobj
      IF(ISUPRES .EQ. 0)
     1 write(6,*) 'fobjplus,gamma,icycle=',fobjplus,gamma,icycle
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
      IF(ISUPRES .EQ. 0) write(6,*) 'fobjbest=',fobj
      fact=ngrid/volspa
      do i=1,nactve
      corden(i,nvar+1)=fact*denstor(i,4)
      enddo
      DO I = 1,NACTVE
       DO J = 1,NVAR+1
       CORDLAST(I,J) = CORDEN(I,J)
       END DO
      END DO
      NACTLAST = NACTVE
        IF(ISUPRES .EQ. 0) WRITE(*,8) NSUB
        WRITE(25,8) NSUB
    8 FORMAT(/' THE TRUE (NUMERICAL) LOG-LIKELIHOOD OF THE ',I3/
     1' SUBJECT VECTORS, GIVEN THE PRIOR DENSITY, IS: ')
        IF(ISUPRES .EQ. 0) WRITE(*,*) fobj
        WRITE(25,*) fobj
        QVAL = 1
        IF(IERRMOD .EQ. 1) QVAL = 0
        KP = NVAR + int(QVAL)
        AIC = 2.D0*(-FOBJ + KP)
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
        IF(ITEST .EQ. 0) THEN
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
        IF(ISUPRES .EQ. 0) WRITE(*,11)
        WRITE(25,11)
   11 FORMAT(//' THE FOLLOWING VALUES ARE FOR THE UPDATED DENSITY: ')
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
      XVERIFY(1) = SUM
      XVERIFY(2) = ENT
      CALL VERIFYVAL(2,XVERIFY)
      IF(ISUPRES .EQ. 0) THEN
        WRITE(*,31) XVERIFY(1)
        WRITE(*,131) XVERIFY(2)
      ENDIF
        WRITE(25,31) XVERIFY(1)
        WRITE(25,131) XVERIFY(2)
   31 FORMAT(/' THE SCALED INFO FOR THIS CYCLE IS ',F10.2,' %'/)
  131 FORMAT(/' THE ENTROPY FOR THIS CYCLE IS ',G11.4/)
 2100   CONTINUE
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
        write (*,*) "Passed 1100"
          DO I=1,NVAR
            CENTER(1,I)=EX(I)
          END DO
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
      IF(ISUPRES .EQ. 0) WRITE(*,5103) (XVERIFY(I),I=1,NVAR)
      WRITE(25,5103) (XVERIFY(I),I=1,NVAR)
 5103   FORMAT(1X,30(G12.6,1X))
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
        WRITE(25,5103) (XVERIFY(J),J=1,I)
  200 IF(ISUPRES .EQ. 0) WRITE(*,5103) (XVERIFY(J),J=1,I)
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
      IF(ISUPRES .EQ. 0) WRITE(*,5103) (XVERIFY(I),I=1,NVAR)
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
      IF(ISUPRES .EQ. 0) WRITE(*,5103) (XVERIFY(I),I=1,NVAR)
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
       WRITE(25,5103) (XVERIFY(J),J=1,I)
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
       DO 6090 I=1,NVAR
         IND=I
        CALL STAZ(VOLSPA,NGRID,NACTVE,NVAR,IND,CORDEN,WORK,MAXGRD,NINT,
     1  AB(IND,1),AB(IND,2),XMODE,X025,X25,X50,X75,X975,SCALINFO,NSUB,
     2  MAXDIM)
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
      IF(ISUPRES .EQ. 0) WRITE(*,6092) PAR(IND)
        WRITE(25,6092) PAR(IND)
 6092 FORMAT(/' ',A11,':')
       XVERIFY(1) = XMODE
       XVERIFY(2) = SK
       XVERIFY(3) = KU
       XVERIFY(4) = X025
       CALL VERIFYVAL(4,XVERIFY)
       IF(ISUPRES .EQ. 0) WRITE(*,6093) (XVERIFY(IXV),IXV=1,4)
       WRITE(25,6093) (XVERIFY(IXV),IXV=1,4)
       XVERIFY(1) = X25
       XVERIFY(2) = X50
       XVERIFY(3) = X75
       XVERIFY(4) = X975
       CALL VERIFYVAL(4,XVERIFY)
       IF(ISUPRES .EQ. 0) WRITE(*,6093) (XVERIFY(IXV),IXV=1,4)
       WRITE(25,6093) (XVERIFY(IXV),IXV=1,4)
       XVERIFY(1) = SDEST1
       XVERIFY(2) = SDEST2
       XVERIFY(3) = SDEST3
       XVERIFY(4) = SCALINFO
       CALL VERIFYVAL(4,XVERIFY)
       IF(ISUPRES .EQ. 0) WRITE(*,6093) (XVERIFY(IXV),IXV=1,4)
       WRITE(25,6093) (XVERIFY(IXV),IXV=1,4)
 6093 FORMAT(1X,4(G15.8,2X))
       XMED(IND) = X50
 6090  CONTINUE
        IF(ISUPRES .EQ. 0) WRITE(*,*)
        WRITE(25,*)
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
      DO I = 1,NVAR
       X(I) = EX(I)
      END DO
      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,RANFIXEST,PX)
        NPX = NVAR+NOFIX+NRANFIX
      IPAR(i_skip_ig)=1
       DO I = 1,NRANFIX
        START(I) = RANFIXEST(I)
        STEP(I) = -.2D0*START(I)
       END DO
       DO I = NRANFIX+1,NRANFIX+NVAR
        START(I) = EX(I-NRANFIX)
        STEP(I) = -.2D0*START(I)
       END DO
       CALL ELDERY(NRANFIX+NVAR,START,OPTVAR,VALMIN,1.D-10,STEP,1000,
     1  CALCRF,0,ICONV,NITER,ICNT,NUMEQT,YO,C0,C1,C2,C3,C4,C5,NBCOMP,
     2  NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,INTLIST,
     3  IPAR,ObsError,RPAR,gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     4  errfilname)
        IF(ICONV .EQ. 0 .AND. ISUPRES .EQ. 0) WRITE(*,9011)
        IF(ICONV .EQ. 0) WRITE(25,9011)
 9011 FORMAT(' ',' NO CONVERGENCE THIS CYCLE ON ESTIMATES FOR THE'/
     1' RANFIX AND RANDOM PARAMETERS. '/)
       DO I = 1,NRANFIX
        RANFIXEST(I) = OPTVAR(I)
       END DO
       DO I = NRANFIX+1,NRANFIX+NVAR
        EXO(I-NRANFIX) = OPTVAR(I)
       END DO
       DO J = 1,NVAR
        DO IG = 1,NACTVE
         CORDEN(IG,J) = CORDEN(IG,J)*EXO(J)/EX(J)
        END DO
       END DO
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
        IF(MAXCYC .EQ. 0) GO TO 900
        write (*,*) "Begin control"
        IMAXCYC = 0
      if(icycle .ge. maxcyc) then
        IF(ICONTIN .EQ. 0) THEN
         WRITE(25,1261) MAXCYC,MAXCYC0
 1261    FORMAT(/' THE USER CHOSE TO STOP THE PROGRAM AT CYCLE NO. '/
     1' ',I7,' ... THE ORIGINAL NO. OF MAXIMUM CYCLES WAS ',I7//)
         WRITE(*,1261) MAXCYC,MAXCYC0
        ENDIF
        IMAXCYC = 1
      endif
        ICONVERG = 0
      ximprove=fobj-prefobj
       XVERIFY(1) = fobj1
       XVERIFY(2) = gamma*flat
       XVERIFY(3) = resolve
       CALL VERIFYVAL(3,XVERIFY)
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
      if(dabs(ximprove) .le. tol .and. resolve .gt. 0.0001) then
        resolve=resolve*0.5D0
        rpar(k_resolve) = resolve
      endif
      if(resolve.le.0.0001) then
        resolve=0.20000000298023224
        rpar(k_resolve) = 0.20000000298023224
        checkbig = fobj - prebig
      write(91,*) 'res set to .2 ',' checkbig=',checkbig
        prebig =fobj
         IF(ISUPRES .EQ. 1) THEN
         WRITE(*,1023) ICYCLE
 1023    FORMAT(/' FOR CYCLE NO, ',I6,' THE CONVERGENCE CRITERION AND ME
     1DIANS ARE: ')
       XVERIFY(1) = checkbig
       XVERIFY(2) = TOLC
       CALL VERIFYVAL(2,XVERIFY)
       WRITE(*,1024) DABS(XVERIFY(1)),XVERIFY(2)
 1024    FORMAT(1X,G14.4,' <-- CONVERGENCE OCCURS WHEN THIS NO. < ',F20.
     117)
       WRITE(*,5104) (PAR(I),I=1,NVAR)
       DO I = 1,NVAR
        XVERIFY(I) = XMED(I)
       END DO
       CALL VERIFYVAL(NVAR,XVERIFY)
       WRITE(*,5103) (XVERIFY(IXV),IXV=1,NVAR)
        ENDIF
        if(dabs(checkbig) .le. TOLC) then
        ICONVERG = 1
          go to 900
        endif
      endif
        IF(IMAXCYC .EQ. 1) GO TO 900
      alg_type = 0
      isupres = 0
      call expand_grid(alg_type,isupres,nvar,nactve,ngridn,
     1    resolve, corden, ab)
      isupres = 0
        prefobj=fobj
        GO TO 1001
  900 continue
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
  910 CONTINUE
         REWIND(27)
        IPAR(i_do) = 6000
        DO 6000 JSUB=1,NSUB
        write (*,*) "DO 6000", JSUB
        CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,
     1    TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     2    INTLIST,ERRFILNAME)
        call cp_lrcs_to_rpar(gamma,flat,numeqt,c0,c1,c2,c3,c4,c5,rpar)
         DO ICENTER = 1,3
         DO J=1,NVAR
           EXXX(J) = CENTER(ICENTER,J)
         END DO
      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,EXXX,VALFIX,RANFIXEST,PX)
        NPX = NVAR+NOFIX+NRANFIX
      IPAR(i_skip_ig)=1
        CALL SYMBOL(NBCOMP)
        CALL IDCALCY(JSUB,IG,NPX,NDIM,PX,YPRED,NUMEQT,
     1      NOBSER,MF,NBCOMP,RTOL,ATOL,
     2      TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     3      INTLIST,IPAR,ObsError,RPAR,ERRFILNAME)
         DO IOBS=1,NOBSER
         DO IEQ=1,NUMEQT
           YPREDPOP(JSUB,IEQ,IOBS,ICENTER) = YPRED(IOBS,IEQ)
         END DO
         END DO
      CALL CALCTPRED(JSUB,IDELTA,NOBSER,NUMT(JSUB),TPRED,TPREDREL,
     1   NOMAXTIM(JSUB),TEND,TBEGG,TIMCOPY,SIGCOPY,INTLIST)
        DO J = 1,NUMT(JSUB)
          TTPRED(JSUB,J) = TPRED(J)
          TTPREDREL(JSUB,J) = TPREDREL(J)
        END DO
        DO J = 1,NOMAXTIM(JSUB)
          TENDSUB(JSUB,J) = TEND(J)
          TBEGGSUB(JSUB,J) = TBEGG(J)
        END DO
      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,EXXX,VALFIX,RANFIXEST,PX)
        NPX = NVAR+NOFIX+NRANFIX
      IPAR(i_skip_ig) = 1
        CALL SYMBOL(NBCOMP)
        CALL IDCALCYY(JSUB,IG,NPX,NDIM,PX,TPRED,
     1      NUMT(JSUB), YYPRED,NUMEQT,NOBSER,MF,NBCOMP,RTOL,ATOL,
     2      TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     3      INTLIST,IPAR,ObsError,RPAR,ERRFILNAME)
         DO J=1,NUMT(JSUB)
         DO IEQ=1,NUMEQT
           YPREDPOPT(JSUB,IEQ,J,ICENTER) = YYPRED(J,IEQ)
         END DO
         END DO
        END DO
 6000   CONTINUE
        write (*,*) "Passed 6000"
        REWIND(27)
        NNACTVE=NACTVE
        DO I=1,NACTVE
        DO J=1,NVAR+1
          CORHOLD(I,J) = CORDEN(I,J)
        END DO
        END DO
        OPEN(31,FILE=PREDFIL)
       write (*,*) "Starting 7000"
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
        MISVAL = 0
        SIGFAC=1.D0
        DO 240 I=1,NOBSER
          DO 240 J=1,NUMEQT
            Y = YO(I,J)
            IF(Y .EQ. -99) THEN
              MISVAL = MISVAL+1
              GO TO 240
            ENDIF
             if (C4(J).eq.10) IPAR(i_is_log10+J) = 10
           if (C5(J).eq.229) then
             write (*,*) "Poisson analysis req. for OUTEQ",J
             ObsError(I,J)=1.D0
             SIG(I,J)=1.D0
             IPAR(i_is_poisson_obs+J)=229
          else
            SIG(I,J) = C0(J)+C1(J)*Y+C2(J)*Y*Y+C3(J)*Y**3
            if(ierrmod.eq.2) sig(i,j) = sig(i,j)*gamma
            if(ierrmod.eq.3) sig(i,j)=dsqrt(sig(i,j)**2 + gamma**2)
            if(ierrmod.eq.4) sig(i,j) = gamma*flat
            ObsError(I,J) = sig(I,J)
            IF(SIG(I,J) .LT. 0) THEN
              WRITE(*,2346) JSUB
              WRITE(25,2346) JSUB
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
      CALL SUBRES(MAXSUB,MAXACT,JSUB,CORDEN,WORK,MAXGRD,MAXDIM,NVAR,
     1  NOFIX,VALFIX,SIGFAC,OFAC,AB,PAR,NACTVE,NGRID,VOLSPA,IRAN,CENTER,
     2  PYJGXX,NRANFIX,RANFIXEST,NOBSER,NUMEQT,NBCOMP,
     3  NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     4  INTLIST,IPAR,ObsError,RPAR,NACTSUB,BAYPOS,ERRFILNAME)
        DO IG = 1,NACTVE
          PYJGX(JSUB,IG) = PYJGXX(IG)
        END DO
        DO ICENTER = 1,3
          DO J=1,NVAR
            EXXX(J) = CENTER(ICENTER,J)
            EXX(JSUB,ICENTER,J) = CENTER(ICENTER,J)
          END DO
      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,EXXX,VALFIX,RANFIXEST,PX)
        NPX = NVAR+NOFIX+NRANFIX
      IPAR(i_skip_ig) = 1
        CALL SYMBOL(NBCOMP)
        CALL IDCALCY(JSUB,IG,NPX,NDIM,PX,YPRED,NUMEQT,
     1      NOBSER,MF,NBCOMP,RTOL,ATOL,
     2      TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     3      INTLIST,IPAR,ObsError,RPAR,ERRFILNAME)
        DO IOBS=1,NOBSER
         DO IEQ=1,NUMEQT
         YPREDBAY(JSUB,IEQ,IOBS,ICENTER) = YPRED(IOBS,IEQ)
         END DO
        END DO
        END DO
        write (*,*) "YPREDBAY(,,,) update done"
        DO I=1,NUMT(JSUB)
         TPRED(I) = TTPRED(JSUB,I)
        END DO
          write (*,*) "TPRED(I) update done"
      DO ICENTER = 1,3
       DO J=1,NVAR
        EXXX(J) = CENTER(ICENTER,J)
       END DO
      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,EXXX,VALFIX,RANFIXEST,PX)
        NPX = NVAR+NOFIX+NRANFIX
      IPAR(i_skip_ig) = 1
        CALL SYMBOL(NBCOMP)
      CALL IDCALCYY(JSUB,IG,NPX,NDIM,PX,TPRED,
     1  NUMT(JSUB), YYPRED,NUMEQT,NOBSER,MF,NBCOMP,
     2  RTOL,ATOL,
     3  TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     4  INTLIST,IPAR,ObsError,RPAR,ERRFILNAME)
       DO J = 1,NUMT(JSUB)
        DO IEQ = 1,NUMEQT
         YYYPRED(ICENTER,J,IEQ) = YYPRED(J,IEQ)
        END DO
       END DO
      END DO
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
       WRITE(31,2167) (XVERIFY(IXV),IXV=1,1+3*NUMEQT)
 2167  FORMAT(90(G16.5,2X))
      END DO
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
        END DO
       END DO
        WRITE(31,*)
        WRITE(31,*)
      ILAST = 0
      IELAST = 0
      DO IMAXTIM = 1,NOMAXTIM(JSUB)
       NUMTT = int((TENDSUB(JSUB,IMAXTIM)-TBEGGSUB(JSUB,IMAXTIM)
     1   )*60/IDELTA)
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
	WRITE(*,2048)
	WRITE(25,2048)
 2048   FORMAT(/'      PERIOD',10X,'TIME (HOURS)',8X,'  AUC',8X,'   AUC/
     1MIC'/
     1' ----------------------------------------------------------------
     1----')
	AUCRUN = 0.D0
	DO 2050 IPERIOD = 1,NWHOLE
	IS = ILAST + (IPERIOD-1)*NDELPER + 1
	IE = IS + NDELPER
      IELAST = IE
	SUM=0.D0
	DO I=IS+1, IE-1
	 SUM=SUM+YYYPRED(ICENTER,I,IEQ)
	END DO
	AUC = IDELTA*((YYYPRED(ICENTER,IS,IEQ) +
     1      YYYPRED(ICENTER,IE,IEQ))/2.D0 + SUM)/60.D0
	AUCMIC = AUC/XMIC
	AUCRUN = AUCRUN+AUC
	IHRST = (IPERIOD-1)*int(AUCINT)
	IHREN = IHRST + int(AUCINT)
      XVERIFY(1) = AUC
      XVERIFY(2) = AUCMIC
      CALL VERIFYVAL(2,XVERIFY)
      WRITE(*,2049) IPERIOD, IHRST, IHREN, XVERIFY(1),XVERIFY(2)
      WRITE(25,2049) IPERIOD, IHRST, IHREN, XVERIFY(1),XVERIFY(2)
 2049 FORMAT(' ',I8,2X,I10,'    -',I10,5X,G12.6,3X,G12.6)
 2050 CONTINUE
	IF(NPAR .GT. 0) THEN
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
	IPERIOD = NWHOLE + 1
	IHRST = (IPERIOD-1)*int(AUCINT)
	IHREN = IHRST + int(NPAR*IDELTA/60)
      XVERIFY(1) = AUC
      XVERIFY(2) = AUCMIC
      CALL VERIFYVAL(2,XVERIFY)
      WRITE(*,2051) IPERIOD, IHRST, IHREN, XVERIFY(1),XVERIFY(2)
      WRITE(25,2051) IPERIOD, IHRST, IHREN, XVERIFY(1),XVERIFY(2)
 2051   FORMAT(' ',I2,' (PARTIAL)',I8,'    -',I10,5X,G12.6,3X,G12.6)
	ENDIF
	AUCMIC = AUCRUN/XMIC
      XVERIFY(1) = AUCRUN
      XVERIFY(2) = AUCMIC
      CALL VERIFYVAL(2,XVERIFY)
      WRITE(*,2052) IHREN, XVERIFY(1),XVERIFY(2)
      WRITE(25,2052) IHREN, XVERIFY(1),XVERIFY(2)
 2052   FORMAT(' ','----------------------------------------------------
     1---------------'/
     2'    TOTAL',2X,'         0    -',I10,5X,G12.6,3X,G12.6//)
       END DO
	END DO
       ILAST = IE
       IELAST = IE
	END DO
 7000   CONTINUE
	NACTVE=NNACTVE
	DO I=1,NACTVE
	DO J=1,NVAR+1
	 CORDEN(I,J) = CORHOLD(I,J)
	END DO
	END DO
      write(*,*)' About to create density file ...'
	OPEN(23,FILE=DENFIL)
	WRITE(23,7124)
 7124   FORMAT('DENSITY OCT_15 ... Made by npagranfix6')
	WRITE(23,*) NDIM
	WRITE(23,*) INDPTS
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
	WRITE(23,*) NINT
      IF(IHESS .EQ. -1) ICYCLE = ICYCLE - 1
	WRITE(23,*) ICYCLE
	WRITE(23,*) DORIG
	write(*,*)' Writing CORDEN ... '
	 DO I=1,NACTVE
	  WRITE(23,*) (CORDEN(I,J),J=1,NVAR+1)
	 END DO
	write(*,*)' Writing PYJGX ... '
	DO JSUB=1,NSUB
	 DO I=1,NACTVE
	  WRITE(23,*) PYJGX(JSUB,I)
	 END DO
	END DO
	REWIND(27)
        write(*,*)' Writing YPREDPOP ...'
        DO JSUB=1,NSUB
          CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,
     1     TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     2     INTLIST,ERRFILNAME)
        call cp_lrcs_to_rpar(gamma,flat,numeqt,c0,c1,c2,c3,c4,c5,rpar)
          DO IEQ=1,NUMEQT
            DO J=1,NOBSER
              WRITE(23,*) (YPREDPOP(JSUB,IEQ,J,ICENTER),ICENTER=1,3)
            END DO
          END DO
        END DO
	DO JSUB = 1,NSUB
	 WRITE(23,*) NUMT(JSUB)
	END DO
	write(*,*)' Writing YPREDOPT ... '
	DO JSUB=1,NSUB
	 DO IEQ=1,NUMEQT
	  DO J=1,NUMT(JSUB)
	   WRITE(23,*) (YPREDPOPT(JSUB,IEQ,J,ICENTER),ICENTER=1,3)
	  END DO
	 END DO
	END DO
	write(*,*)' Writing TTPREDREL ... '
	DO JSUB=1,NSUB
	 DO J=1,NUMT(JSUB)
	  WRITE(23,*) TTPREDREL(JSUB,J)
	 END DO
	END DO
	REWIND(27)
	write(*,*)' Writing YPREDBAY ... '
	DO JSUB=1,NSUB
	 CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,
     1     TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     2     INTLIST,ERRFILNAME)
        call cp_lrcs_to_rpar(gamma,flat,numeqt,c0,c1,c2,c3,c4,c5,rpar)
	 DO IEQ=1,NUMEQT
	  DO J=1,NOBSER
	   WRITE(23,*) (YPREDBAY(JSUB,IEQ,J,ICENTER),ICENTER=1,3)
	  END DO
	 END DO
	END DO
	write(*,*)' Writing EXX ... '
	DO JSUB=1,NSUB
	 DO ICENTER=1,3
	  WRITE(23,*) (EXX(JSUB,ICENTER,J),J=1,NVAR)
	 END DO
	END DO
      write(25,*) 'Optimal value of gamma=',gamma
	OPEN(29,FILE='npagdriv.f')
	REWIND(27)
	REWIND(25)
	REWIND(23)
	write(*,*)' About to create the combined output file ... '
	OUTCOM = 'OUT'//NAME
	OPEN(26,FILE=OUTCOM)
 1110   READ(25,2717,IOSTAT=IEND) READLARG
 2717   FORMAT(A1000)
        IF(IEND .LT. 0) GO TO 1120
        CALL CONDENSE(READLARG)
        GO TO 1110
 1120   WRITE(26,1121)
 1121   FORMAT(/'***************** END OF THE OUTPUT FILE **************
     1***'//
     2'***************** START OF THE DENSITY FILE *****************'/)
	write(*,*)' Writing density to combined output file ...'
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
 1170   READ(29,2717,IOSTAT=IEND) READLARG
        IF(IEND .LT. 0) GO TO 1180
        CALL CONDENSE(READLARG)
        GO TO 1170
 1180   WRITE(26,1181)
 1181   FORMAT(/'***************** END OF THE npagdriv.f FILE **********
     1*******'/)
      REWIND(23)
      REWIND(27)
      REWIND(25)
      REWIND(29)
      CLOSE(26)
      CLOSE(31)
        OUTFILER = 'NP_RF'//NAME//'.TXT'
        CALL READOUT( OUTFILER
     1 , DOSEBLOCK, OBSBLOCK, NDORIG
     2 , BAYPOS, NACTSUB )
 	  close(91)
      CLOSE(23)
      CLOSE(27)
      CLOSE(25)
      CLOSE(29)
	STOP
	END
        SUBROUTINE FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,
     1    TIM,SIG,RS,BS,INTLIST,ERRFIL)
         use npag_utils, only: maxnumeq,max_m_per_obs
     1    ,max_obs_dim,max_input_dim,max_doses,max_RS_J
        IMPLICIT REAL*8(A-H,O-Z)
        integer NOBSER
        DIMENSION YO(max_m_per_obs,NUMEQT)
        DIMENSION C4(NUMEQT),C5(NUMEQT)
     1    ,C0(NUMEQT),C1(NUMEQT),C2(NUMEQT),C3(NUMEQT)
        integer NUMEQT
       real*8, dimension(max_m_per_obs) :: TIMCOPY
       real*8, dimension(max_doses) :: SIGCOPY
       real*8, dimension(max_doses,max_RS_J) :: RSCOPY
       real*8, dimension(max_doses,max_input_dim) :: BSCOPY
       integer, dimension(128) :: INTLIST
! wmy20190628 --
        integer N,ND,NI,NUP,NUIC,NP
        integer NUMEQTT,NDRUG,NADD,ISEX,IETHFLAG,M
        double precision AGE,HEIGHT
        DIMENSION TIM(max_m_per_obs),SIG(max_doses),
     1    RS(max_doses,max_RS_J),
     2    YOO(max_m_per_obs,MAXNUMEQ),BS(max_doses,max_input_dim)
        character ERRFIL*20
        CHARACTER SEX*1,READLINE*300
        real*8, dimension(max_input_dim) :: FA
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
        INTLIST(1) = int(AGE)
        INTLIST(2) = ISEX
        INTLIST(3) = int(HEIGHT)
        INTLIST(4) = IETHFLAG
    1   FORMAT(A300)
   10	READ(27,1) READLINE
	IF(READLINE(12:23) .NE. 'NO. OF DRUGS') GO TO 10
	BACKSPACE(27)
    3   FORMAT(T2,I5)
        READ(27,3) NDRUG
        INTLIST(5) = NDRUG
	IF(NDRUG .GT. max_input_dim) THEN
	 WRITE(*,124)
  124    FORMAT(' YOUR PATIENT DATA FILES CANNOT HAVE MORE THAN 7'/
     1' DRUGS. THE PROGRAM IS NOW STOPPING. '/)
        OPEN(42,FILE=ERRFIL)
         WRITE(42,124)
        CLOSE(42)
       CALL PAUSE
	 STOP
	ENDIF
        READ(27,3) NADD
	NI = 2*NDRUG + NADD
        INTLIST(6) = NADD
        INTLIST(7) = NI
	IF(NI .GT. max_RS_J) THEN
  	 WRITE(*,123)
  123    FORMAT(/' YOUR PATIENT DATA FILES HAVE TOO MANY COLUMNS IN '/
     1' THE DOSAGE REGIMEN BLOCK. THE NO. OF ADDITIONAL COVARIATES '/
     2' PLUS TWICE THE NO. OF DRUGS CANNOT EXCEED max_RS_J. THE'/
     3' PROGRAM IS NOW STOPPING. '/)
        OPEN(42,FILE=ERRFIL)
         WRITE(42,123)
        CLOSE(42)
       CALL PAUSE
	 STOP
	ENDIF
        READ(27,3) ND
        INTLIST(8) = ND
	IF(ND .GT. max_doses) THEN
	 WRITE(*,125)
  125    FORMAT(' YOUR PATIENT DATA FILES CANNOT HAVE MORE THAN '/
     1' max_doses DOSE EVENTS. THE PROGRAM IS NOW STOPPING. '/)
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
	END DO
        DO I=1,ND
         DO J=1,NDRUG
          BS(I,J)=RS(I,2*J)
         END DO
        END DO
   40	READ(27,1) READLINE
        IF(READLINE(12:23) .NE. 'NO. OF TOTAL') GO TO 40
        BACKSPACE(27)
        READ(27,*) NUMEQTT
        INTLIST(9) = NUMEQTT
      IF(NUMEQTT .NE. NUMEQT) THEN
       WRITE(*,127) NUMEQT,NUMEQTT
  127  FORMAT(/' THERE IS A CONFLICT IN SUBROUTINE FILRED.'/
     1' NUMEQT = ',I2,', BUT NUMEQTT = ',I2/
     2' THESE TWO VALUES SHOULD BE THE SAME. SOMETHING IS AMISS WITH'/
     3' AT LEAST ONE OF YOUR PATIENT DATA FILES. THE PROGRAM IS NOW'/
     4' STOPPING. '/)
        OPEN(42,FILE=ERRFIL)
         WRITE(42,127) NUMEQT,NUMEQTT
        CLOSE(42)
       CALL PAUSE
       STOP
      ENDIF
        READ(27,3) M
        INTLIST(10) = M
	MAXOBDIM = max_obs_dim
	IF(M .GT. MAXOBDIM) THEN
  	 WRITE(*,126) MAXOBDIM
  126    FORMAT(/' AT LEAST ONE OF YOUR PATIENT DATA FILES HAS TOO'/
     1' MANY OBSERVED VALUE TIMES. THIS NO. CANNOT EXCEED ',I5,'.'/
     2' THE PROGRAM IS NOW STOPPING. '/)
        OPEN(42,FILE=ERRFIL)
         WRITE(42,126) MAXOBDIM
        CLOSE(42)
       CALL PAUSE
	 STOP
	ENDIF
         DO I=1,M
          READ(27,*) TIM(I),(YO(I,J),J=1,NUMEQT)
         END DO
	NOBSER=M
   50	READ(27,1) READLINE
	IF(READLINE(1:25) .NE. 'ASSAY COEFFICIENTS FOLLOW') GO TO 50
	DO IEQ = 1,NUMEQT
	 READ(27,*) C0(IEQ),C1(IEQ),C2(IEQ),C3(IEQ),C4(IEQ),C5(IEQ)
	END DO
	RETURN
	END
	SUBROUTINE CALGRD(NVAR,NGRID,AB,X,errfil)
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
        if(first) then
         CALL INFAUR(flag,nvar,ngrid,
     1     S,QS,COEF,RQS,NEXTN,TESTN,HISUM)
         first = .FALSE.
        endif
        IF(.NOT. FLAG(1)) THEN
	 WRITE(*,11) NVAR
   11    FORMAT(/' THE NUMBER OF RANDOM VARIABLES, ',I3,', IS NOT '/
     1' ACCEPTABLE IN SUBROUTINE INFAUR.')
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
        OPEN(42,FILE=ERRFIL)
         WRITE(42,12) NGRID
        CLOSE(42)
	 CALL PAUSE
         STOP
        ENDIF
        CALL GOFAUR(quasi,
     1     S,QS,COEF,RQS,NEXTN,TESTN,HISUM)
	DO IC = 1,NVAR
        X(IC) = (AB(IC,2)-AB(IC,1))*quasi(IC) + AB(IC,1)
	END DO
	RETURN
	END
	SUBROUTINE INFAUR(FLAG,DIMEN,ATMOST,
     1     S,QS,COEF,RQS,NEXTN,TESTN,HISUM)
        implicit double precision (a-h,o-z)
        LOGICAL FLAG(2)
      INTEGER S,ATMOST,QS,COEF(0:19,0:19),NEXTN,
     +        TESTN,HISUM,I,J,PRIMES(40),DIMEN
      DATA (PRIMES(I),I=1,40)/1,2,3,5,5,7,7,11,11,11,11,
     +                        13,13,17,17,17,17,19,19,
     +                        23,23,23,23,29,29,29,29,
     +                        29,29,31,31,37,37,37,37,
     +                        37,37,41,41,41/
      S=DIMEN
      FLAG(1) = S.GT.1 .AND. S.LT.41
      IF (.NOT.FLAG(1)) RETURN
      QS=PRIMES(S)
      TESTN=QS**4
      HISUM=NINT(LOG(REAL(ATMOST+TESTN))/LOG(REAL(QS)))
      FLAG(2)=HISUM.LT.20
      IF(.NOT. FLAG(2)) RETURN
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
      NEXTN=TESTN-1
      HISUM=3
      RQS=1.0/REAL(QS)
      RETURN
      END
        SUBROUTINE GOFAUR(QUASI,
     1    S,QS,COEF,RQS,NEXTN,TESTN,HISUM)
        USE npag_utils, only: max_pop_rand_varbs
        implicit double precision (a-h,o-z)
        INTEGER S,QS,COEF(0:19,0:19),NEXTN,TESTN,
     +        HISUM,I,J,K,YTEMP(0:19),ZTEMP,
     +        KTEMP,LTEMP,MTEMP
      dimension QUASI(max_pop_rand_varbs)
      KTEMP=TESTN
      LTEMP=NEXTN
      DO 100 I=HISUM,0,-1
          KTEMP=KTEMP/QS
          MTEMP=MOD(LTEMP,KTEMP)
          YTEMP(I)=(LTEMP-MTEMP)/KTEMP
          LTEMP=MTEMP
  100   CONTINUE
      R=YTEMP(HISUM)
      DO 200 I=HISUM-1,0,-1
          R=YTEMP(I)+RQS*R
  200   CONTINUE
      QUASI(1)=R*RQS
      DO 500 K=2,S
          QUASI(K)=0.0
          R=RQS
          DO 400 J=0,HISUM
              ZTEMP=0
              DO 300 I=J,HISUM
                  ZTEMP=ZTEMP+COEF(I,J)*YTEMP(I)
  300           CONTINUE
              YTEMP(J)=MOD(ZTEMP,QS)
              QUASI(K)=QUASI(K)+YTEMP(J)*R
              R=R*RQS
  400       CONTINUE
  500   CONTINUE
      NEXTN=NEXTN+1
      IF(NEXTN.EQ.TESTN) THEN
        TESTN=TESTN*QS
        HISUM=HISUM+1
      ENDIF
      RETURN
      END
	SUBROUTINE NOTINT(VOLSPA,NGRID,NACTVE,FUNC,MAXGRD,ESTINT)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION FUNC(MAXGRD)
	SUM=0.D0
	DO 100 IG=1,NACTVE
  100   SUM=SUM+FUNC(IG)
	ESTINT=VOLSPA*SUM/NGRID
	RETURN
	END
       SUBROUTINE STAZ(VOLSPA,NGRID,NACTVE,NVAR,IND,CORDEN,PROD,MAXGRD,
     1  NINT,X1,X2,XMODE,X025,X25,X50,X75,X975,SCALINFO,NSUB,MAXDIM)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION CORDEN(MAXGRD,MAXDIM+1),PROD(MAXGRD),X(1999),
     1  PROB(1998),CUMPRO(0:1998)
	NEWINT = NINT
	IF(2*NSUB .GT. NEWINT) NEWINT = 2*NSUB
	XINT=(X2-X1)/NEWINT
	X(1)=X1
	X(NEWINT+1) = X2
	DO 5 I=1,NEWINT-1
    5   X(I+1)=X(I)+XINT
	DO 1000 INTR=1,NEWINT
	DO IG=1,NACTVE
	  W = 0.D0
	  XX = CORDEN(IG,IND)
	  IF(XX .GE. X(INTR) .AND. XX .LT. X(INTR+1)) W = 1.D0
	  IF(INTR .EQ. NEWINT .AND. XX .GE. X(INTR)) W = 1.D0
          PROD(IG) = CORDEN(IG,NVAR+1)*W
	END DO
 1000  CALL NOTINT(VOLSPA,NGRID,NACTVE,PROD,MAXGRD,PROB(INTR))
	INTMAX=1
	DO INTR=2,NEWINT
	  IF(PROB(INTR) .GT. PROB(INTMAX)) INTMAX=INTR
	END DO
	XMODE=(X(INTMAX)+X(INTMAX+1))/2.D0
	SUM=0.D0
	DO I=1,NEWINT
	IF (PROB(I) .GT. 0.D0) THEN
	  PI=PROB(I)
	  SUM=SUM+PI*DLOG(PI)
	ENDIF
	END DO
	DL2=DLOG(2.D0)
	SUM = SUM/DL2
        DINT=NEWINT
	FACT=100.D0*DL2/DLOG(DINT/NSUB)
	SCALINFO = FACT*(SUM + DLOG(DINT)/DL2)
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
	SUBROUTINE EQUIV(INUM,NAME)
	CHARACTER*1 A,B,C,D
	CHARACTER NAME*4
	I4 = INUM/1000
	ILEFT = INUM - I4*1000
	I3 = ILEFT/100
	ILEFT = ILEFT - I3*100
	I2 = ILEFT/10
 	ILEFT = ILEFT - I2*10
	I1 = ILEFT
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
      SUBROUTINE SUBRES(MAXSUB,MAXACT,JSUB,CORDEN,WORK,MAXGRD,MAXDIM,
     1  NVAR,NOFIX,VALFIX,SIGFAC,OFAC,AB,PAR,NACTVE,NGRID,VOLSPA,IRAN,
     2  CENTER,PYJGXX,NRANFIX,RANFIXEST,NOBSER,NUMEQT,NBCOMP,
     3  NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     4  INTLIST,IPAR,ObsError,RPAR,NACTSUB,BAYPOS,ERRFIL)
      USE npag_utils, only: verifyval,makevec,maxnumeq,max_m_per_obs
     1  ,max_ODE_params,max_pop_rand_varbs,max_doses,max_pop_params
     2  ,max_ODE_comps,max_pop_varbs,max_RS_J,max_input_dim
     3  ,k_sfac,k_ofac,k_sum_z_sq,k_prod_pr,i_skip_ig,i_do
      IMPLICIT REAL*8(A-H,O-Z)
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
        integer, dimension(100) :: NACTSUB
        real*8, dimension(100,1500,31) :: BAYPOS
        CHARACTER ERRFIL*20
!  $omp ThreadPrivate(/PARAMD/)
        integer IG,NPX,NEWIND
        REAL*8 W,KU,DENMAX,D,SUMD,FACT
        REAL*8, dimension(max_ODE_params) :: PX
        REAL*8, dimension(max_ODE_comps) :: RCOPY,BCOPY
        REAL*8, dimension(max_pop_rand_varbs) :: EX,STD,COFVR,X
        REAL*8, dimension(max_pop_rand_varbs,max_pop_rand_varbs) ::
     1    COV,E,CORR
        REAL*8, dimension(100) :: XVERIFY
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
            DO J=1,NVAR
              X(J)=CORDEN(IG,J)
            END DO
            CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,RANFIXEST,PX)
            NPX = NVAR+NOFIX+NRANFIX
            IPAR(i_skip_ig) = 1
            CALL IDPC(JSUB,IG,NPX,PX,NBCOMP,W,NOBSER,NUMEQT,NDIM,MF,
     1        RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     2        INTLIST,IPAR,ObsError,RPAR,ERRFIL)
            PYJGX=0.D0
          IF(RPAR(k_sum_z_sq) .LE. 22708.D0) THEN
            PYJGX = 10**RPAR(k_prod_pr)
     1        * DEXP(-.5D0*RPAR(k_sum_z_sq))/RPAR(k_sfac)/RPAR(k_ofac)
          ENDIF
            PYJGXX(IG) = PYJGX
            WORK(IG)=PYJGX*CORDEN(IG,NVAR+1)
  800     CONTINUE
      CALL NOTINT(VOLSPA,NGRID,NACTVE,WORK,MAXGRD,PYJ)
      CALL NOTINT(VOLSPA,NGRID,NACTVE,WORK,MAXGRD,PYJ)
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
        OPEN(42,FILE=ERRFIL)
         WRITE(42,26) JSUB
        CLOSE(42)
        CALL PAUSE
        STOP
      ENDIF
	DO IG=1,NACTVE
	  CORDEN(IG,NVAR+1) = WORK(IG)/PYJ
	END DO
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
        NACTVEFULL = NACTVE
        NACTVE = NEWIND
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
      XVERIFY(1) = SUM
      XVERIFY(2) = ENT
      CALL VERIFYVAL(2,XVERIFY)
	WRITE(*,31) XVERIFY(1)
	WRITE(*,131) XVERIFY(2)
	WRITE(25,31) XVERIFY(1)
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
      WRITE(*,5103) (XVERIFY(I),I=1,NVAR)
      WRITE(25,5103) (XVERIFY(I),I=1,NVAR)
 5103   FORMAT(1X,30(G12.6,1X))
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
        WRITE(25,5103) (XVERIFY(J),J=1,I)
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
      WRITE(*,5103) (XVERIFY(I),I=1,NVAR)
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
      WRITE(*,5103) (XVERIFY(I),I=1,NVAR)
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
       WRITE(25,5103) (XVERIFY(J),J=1,I)
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
       WRITE(*,6093) (XVERIFY(IXV),IXV=1,4)
       WRITE(25,6093) (XVERIFY(IXV),IXV=1,4)
       XVERIFY(1) = X25
       XVERIFY(2) = X50
       XVERIFY(3) = X75
       XVERIFY(4) = X975
       CALL VERIFYVAL(4,XVERIFY)
       WRITE(*,6093) (XVERIFY(IXV),IXV=1,4)
       WRITE(25,6093) (XVERIFY(IXV),IXV=1,4)
       XVERIFY(1) = SDEST1
       XVERIFY(2) = SDEST2
       XVERIFY(3) = SDEST3
       XVERIFY(4) = SCALINFO
       CALL VERIFYVAL(4,XVERIFY)
       WRITE(*,6093) (XVERIFY(IXV),IXV=1,4)
       WRITE(25,6093) (XVERIFY(IXV),IXV=1,4)
 6093  FORMAT(1X,4(G15.8,2X))
 6090   CONTINUE
      WRITE(*,*)
      WRITE(25,*)
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
	SUBROUTINE GETIPATF(IFILE,NSUBTOT,NSUB,IPATVEC,IERRR,ERRFIL)
	DIMENSION IPATVEC(9999)
	CHARACTER READLINE*300,ERRFIL*20
    3   FORMAT(A300)
	NSUBB = 0
	NUMCUR = 0
 4210	IF(IFILE .EQ. 23) READ(23,3,ERR=4200) READLINE
	IF(IFILE .EQ. 25) READ(25,3,ERR=4200) READLINE
	CALL GETNUMSF(1,READLINE,NSUBB,NSUBTOT,NUMCUR,ISTOP,IPATVEC)
	IF(ISTOP .EQ. -1) GO TO 4200
	IF(ISTOP .EQ. 1) GO TO 4210
	IF(NSUB .EQ. NSUBB) THEN
	 IERRR = 0
	 RETURN
	ENDIF
	IF(NSUB .NE. NSUBB) THEN
         WRITE(*,2)
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
	SUBROUTINE GETNUMSF(IINCLUDE,READLINE,NSUBB,NSUBTOT,NUMCUR,
     1    ISTOP,IPATVECC)
	DIMENSION IPATVECC(9999)
	CHARACTER READLINE*300
	ISTOP = 1
	DO J = 1,70
	 IF(READLINE(J:J) .NE. ' ') GO TO 10
	END DO
	IF(NSUBB .EQ. 0) WRITE(*,1)
    1   FORMAT(/' THE INSTRUCTION FILE HAS AN IMPROPER BLANK LINE IN '/
     1' THE PATIENT NUMBER SECTION. ')
	ISTOP = -1
	RETURN
   10   CONTINUE
	DO J = 1,70
	 IF(READLINE(J:J) .NE. ' ') GO TO 20
	END DO
   20   ISTART = J
	IF(READLINE(ISTART:ISTART) .NE. '0') GO TO 30
	DO I = ISTART+1,70
	 IF(READLINE(I:I) .NE. ' ') GO TO 30
	END DO
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
     	DO I = ISTART+1,70
	 IF(READLINE(I:I) .EQ. ' ' .OR. READLINE(I:I) .EQ. ',' .OR.
     1      READLINE(I:I) .EQ. '-') GO TO 40
	END DO
   40   IEND = I-1
	CALL GETSUB(READLINE,ISTART,IEND,ISUB,IERROR)
	IF(IERROR .EQ. -1) THEN
	 WRITE(*,7)
    7    FORMAT(/' THE INSTRUCTION FILE HAS AN IMPROPER LINE - WITH'/
     1' AN INVALID CHARACTER ON IT - IN THE PATIENT NUMBER SECTION.')
	 ISTOP = -1
	 RETURN
	ENDIF
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
	DO I = IEND+1,70
	 IF(READLINE(I:I) .NE. ' ') GO TO 50
	END DO
	NUMCUR = ISUB
	NSUBB = NSUBB + 1
	IPATVECC(NSUBB) = ISUB
	RETURN
   50   CONTINUE
	IF(READLINE(I:I) .EQ. ',') THEN
	 NUMCUR = ISUB
 	 NSUBB = NSUBB + 1
	 IPATVECC(NSUBB) = ISUB
	 DO J = I+1,70
	  IF(READLINE(J:J) .NE. ' ') GO TO 60
	 END DO
	 RETURN
   60    ISTART = J
	 GO TO 30
	ENDIF
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
         ISTART = I
	 GO TO 30
	ENDIF
	IF(READLINE(I:I) .EQ. '-') THEN
	 NUMCUR1 = ISUB
	 DO J = I+1,70
	  IF(READLINE(J:J) .NE. ' ') GO TO 70
	 END DO
	 WRITE(*,8)
    8    FORMAT(/' THE INSTRUCTION FILE HAS AN IMPROPER LINE IN IT'/
     1' IN THE PATIENT NUMBER SECTION.'//
     2' A LINE HAS BEEN ENDED WITH A DASH.')
	 ISTOP = -1
	 RETURN
   70   ISTART = J
     	DO K = ISTART+1,70
	 IF(READLINE(K:K) .EQ. ' ' .OR. READLINE(K:K) .EQ. ',')
     1    GO TO 80
	END DO
   80   IEND = K-1
	CALL GETSUB(READLINE,ISTART,IEND,ISUB,IERROR)
	IF(IERROR .EQ. -1) THEN
	 WRITE(*,7)
	 ISTOP = -1
	 RETURN
	ENDIF
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
	 NUMCUR = ISUB
	 NN = NSUBB
 	 NSUBB = NSUBB + (NUMCUR - NUMCUR1) + 1
	 NONEW = 0
	 DO K = NN+1,NSUBB
	  NONEW = NONEW + 1
	  IPATVECC(K) = NUMCUR1 - 1 + NONEW
	 END DO
	 DO J = IEND+1,70
	  IF(READLINE(J:J) .NE. ' ' .AND. READLINE(J:J) .NE. ',' )
     1    GO TO 90
	 END DO
	 RETURN
   90    ISTART = J
	 GO TO 30
	ENDIF
	WRITE(*,7)
	ISTOP = -1
	RETURN
	END
	SUBROUTINE WRITEPT2(IFILE,NSUB,IPATVEC)
	DIMENSION IPATVEC(9999)
	NEXTIND = 0
   50   NEXTIND = NEXTIND + 1
	IF(NEXTIND .GT. NSUB) GO TO 100
	IFIRST = IPATVEC(NEXTIND)
	IF(NEXTIND .EQ. NSUB) THEN
	 IF(IFILE .EQ. 25) WRITE(25,222) IFIRST
	 IF(IFILE .EQ. 29) WRITE(29,222) IFIRST
  222    FORMAT(1X,I5)
	 GO TO 100
	ENDIF
	IF(IPATVEC(NEXTIND+1) .NE. IFIRST + 1) THEN
	 IF(IFILE .EQ. 25) WRITE(25,222) IFIRST
	 IF(IFILE .EQ. 29) WRITE(29,222) IFIRST
	 GO TO 50
	ENDIF
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
	NEXTIND = NEXT
	GO TO 50
  100   RETURN
	END
	SUBROUTINE GETSUB(READLINE,ISTART,IEND,ISUB,IERROR)
	CHARACTER READLINE*300
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
      SUBROUTINE CALCTPRED(JSUB,IDELTA,NOBSER,NUMTSUB,TPRED,TPREDREL,
     1   NOMAXTIMS,TEND,TBEGG,TIM,SIG,INTLIST)
       use npag_utils, only: maxnumeq,max_m_per_obs,max_doses,
     1   max_RS_J,max_input_dim
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION TPRED(71281),TEND(99),TIM(max_m_per_obs),
     1  SIG(max_doses), TBEGG(99),TPREDREL(71281)
      integer, dimension(128) :: intlist
      IDOSE = 1
      NOMAXTIMS = 0
      NUMTSUB = 0
      INDEX = 0
   50 TIMMAX = -1.D30
   10 INDEX = INDEX + 1
        IF(TIM(INDEX) .GT. TIMMAX) TIMMAX = TIM(INDEX)
        IF(TIM(INDEX) .LE. 0.D0 .AND. INDEX .GT. 1) GO TO 20
        IF(INDEX .EQ. NOBSER) GO TO 20
        GO TO 10
   20   CONTINUE
      TBEG = 0.D0
      IF(SIG(IDOSE) .LT. 0.D0) TBEG = 100.D0*(-SIG(IDOSE))
	T_END = TIMMAX + 24.D0
	NUMT2 = int((T_END - TBEG)*60/IDELTA)
	NUMTSUB = NUMTSUB + 1
      IF(TBEG .GT. 0.D0 .AND. NUMTSUB .GT. 1) THEN
       TPRED(NUMTSUB) = 0.D0
       TPREDREL(NUMTSUB) = 0.D0
       NUMTSUB = NUMTSUB + 1
      ENDIF
	TPRED(NUMTSUB) = TBEG
      TPREDREL(NUMTSUB) = 0.D0
	DO I=1,NUMT2
	 NUMTSUB = NUMTSUB + 1
	 IF(NUMTSUB .GT. 7200) GO TO 40
	 TPRED(NUMTSUB) = TPRED(NUMTSUB-1) + IDELTA/60.D0
       TPREDREL(NUMTSUB) = TPRED(NUMTSUB) - TBEG
	END DO
	NOMAXTIMS = NOMAXTIMS + 1
	TEND(NOMAXTIMS) = T_END
      TBEGG(NOMAXTIMS) = TBEG
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
	GO TO 50
   40   CONTINUE
	WRITE(*,2031) JSUB,IDELTA
	WRITE(25,2031) JSUB,IDELTA
 2031    FORMAT(///' FOR SUBJECT NO. ',I4,' THE MAXIMUM NO. OF '/
     1' PREDICTED VALUES (7200) HAS BEEN REACHED. THIS MEANS THAT IN'/
     2' THE DENSITY PART OF THE OUTPUT FILE, AND IN THE PRTB FILE'/
     3' (WHERE THE PREDICTED VALUES ARE WRITTEN ',I3,' MINUTES APART),'/
     4' THIS SUBJECT WILL NOT HAVE A COMPLETE SET OF PREDICTED VALUES.')
	NUMTSUB = 7200
	RETURN
	END
        SUBROUTINE CONDENSE(READLINE)
        CHARACTER READLINE*1000
	DO IEND = 1000,1,-1
	 IF(READLINE(IEND:IEND) .NE. ' ') GO TO 20
	END DO
   20   CONTINUE
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
        SUBROUTINE PAUSE
        WRITE(*,1)
    1   FORMAT(' HIT ANY KEY TO CONTINUE: ')
        READ(*,*,ERR=10) IKEY
        IF(IKEY .EQ. 1) RETURN
   10   RETURN
        END
      SUBROUTINE NEWWORK1(MAXSUB,JSUB,TIMOBREL,
     1 DOSEBLOCK, OBSBLOCK, NDORIG , errfil)
       use npag_utils,only: verifyval,orderdelta,thesame,
     1   maxnumeq,max_m_per_obs,max_doses,
     2   max_RS_J
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SIG(max_doses),RS(max_doses,max_RS_J),
     1 DELTAIV(7),ORDELT(7),
     1 RSS(max_doses,max_RS_J),SIGG(max_doses),
     2 TIM(max_m_per_obs),TIMM(max_m_per_obs),
     3 YO(max_m_per_obs,MAXNUMEQ),
     4 TIMDELAY(99),TIMOBREL(MAXSUB,max_m_per_obs),
     5 OBSBLOCK(800,150,MAXNUMEQ+1),
     6 DOSEBLOCK(800,1000,35),NDORIG(800),XVERIFY(100)
      CHARACTER READLINE*300,ERRFIL*20
 1717 FORMAT(A300)
   10 READ(23,1717) READLINE
      IF(READLINE(12:23) .NE. 'NO. OF DRUGS') GO TO 10
    3 FORMAT(T2,I5)
      BACKSPACE(23)
      READ(23,3) NDRUG
      READ(23,3) NADD
      READ(23,3) ND
	NI = 2*NDRUG + NADD
      IF(ND .EQ. 0) ICOPY = 1
      IF(ND .GE. 1) THEN
       READ(23,*)
       READ(23,*)
       ICOPY = 1
       NDORIG(JSUB) = ND
       DO I = 1,ND
        READ(23,*) SIG(I),(RS(I,J),J=1,NI)
        DOSEBLOCK(JSUB,I,1) = SIG(I)
        DO J = 2,1+NI
         DOSEBLOCK(JSUB,I,J) = RS(I,J-1)
        END DO
        IF(SIG(I) .LT. 0.D0) ICOPY = 0
       END DO
      ENDIF
  140	 READ(23,1717) READLINE
       IF(READLINE(12:23) .NE. 'NO. OF TOTAL') GO TO 140
       BACKSPACE(23)
       READ(23,*) NUMEQT
       READ(23,3) M
       DO I = 1,M
        READ(23,*) (OBSBLOCK(JSUB,I,J),J=1,1+NUMEQT)
       END DO
      IF(ICOPY .EQ. 1) THEN
 1720  BACKSPACE(23)
       BACKSPACE(23)
       READ(23,1717,IOSTAT=IEND) READLINE
	 IF(IEND .LT. 0) THEN
        WRITE(*,1721)
 1721   FORMAT(/' PATIENT DATA INFORMATION WAS NOT READ CORRECTLY'/
     1' FROM THE INSTRUCTION FILE, npag103.inp. IF YOU EDITED THIS'/
     2' FILE MANUALLY, PLEASE RERUN THE PC PREP PROGRAM TO HAVE IT'/
     3' PREPARE npag103.inp AGAIN AND THEN RERUN THIS PROGRAM.'//
     4' IF YOU DID NOT MANUALLY EDIT npag103.inp, PLEASE SEND THE'/
     5' DETAILS OF THIS RUN (STARTING WITH THE PC PREP EXECUTION) TO'/
     5' THE LAPK. '//
     6' THANK YOU.'/)
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
       DO I = 1,2
        READ(23,1717) READLINE
        WRITE(27,1717) READLINE
       END DO
       IF(ND.EQ.0) GO TO 40
       DO I = 1,ND
        READ(23,*) SIG(I),(RS(I,J),J=1,NI)
        WRITE(27,*) SIG(I),(RS(I,J),J=1,NI)
       END DO
   40	 READ(23,1717) READLINE
       WRITE(27,1717) READLINE
       IF(READLINE(12:23) .NE. 'NO. OF TOTAL') GO TO 40
       BACKSPACE(23)
       READ(23,*)
       READ(23,3) M
       BACKSPACE(23)
       READ(23,1717) READLINE
       WRITE(27,1717) READLINE
       DO I = 1,M
        READ(23,*) TIM(I),(YO(I,J),J=1,NUMEQT)
        WRITE(27,*) TIM(I),(YO(I,J),J=1,NUMEQT)
        TIMOBREL(JSUB,I) = TIM(I)
       END DO
   50	 READ(23,1717,IOSTAT=IEND) READLINE
       IF(IEND .LT. 0) GO TO 100
   	 IF(READLINE(3:16) .EQ. 'LAST AND FIRST') GO TO 100
       WRITE(27,1717) READLINE
       GO TO 50
  100	 BACKSPACE(23)
      ENDIF
      IF(ICOPY .EQ. 0) THEN
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
        ILINE = ILINE + 1
        SIGG(ILINE) = SIG(ID) + 100.D0*DELDOSE
        DO J = 1,NI
         RSS(ILINE,J) = RS(ID,J)
        END DO
       ENDIF
       IF(SIG(ID) .LT. 0.D0) THEN
        DO IDRUG = 1,NDRUG
         DELTAIV(IDRUG) = 0.D0
         IF(RS(ID,2*IDRUG) .GT. 0.D0 .AND. RS(ID,2*IDRUG-1) .GT. 0.D0)
     1    DELTAIV(IDRUG) = RS(ID,2*IDRUG)/RS(ID,2*IDRUG-1)
       XVERIFY(1) = SIG(ID)
       XVERIFY(2) = RS(ID,2*IDRUG-1)
       XVERIFY(3) = RS(ID,2*IDRUG)
       CALL VERIFYVAL(3,XVERIFY)
      IF(RS(ID,2*IDRUG) .LE. 0.D0 .AND. RS(ID,2*IDRUG-1) .GT. 0) THEN
       WRITE(*,101) ID,XVERIFY(1),IDRUG,XVERIFY(2),XVERIFY(3)
  101     FORMAT(//' THERE IS AN ERROR IN YOUR INSTRUCTION FILE, AS'/
     1' DETERMINED BY SUBROUTINE NEWWORK1.'//
     2' ONE OF THE SUBJECTS HAS A STEADY STATE DOSE SET WITH A '/
     3' POSITIVE IV RATE, BUT WITH A TOTAL DOSE AMOUNT .LE. 0.'//
     4' IN PARTICULAR, FOR DOSE EVENT ',I4,' AND TIME ',G19.9,/
     5' FOR DRUG ',I2,', THE IV VALUE IS ',G19.9,' WHILE THE TOTAL'/
     6' DOSE AMOUNT IS ',G19.9//
     7' THE PROGRAM STOPS. PLEASE CORRECT THE ERROR BEFORE RERUNNING.'/)
        OPEN(42,FILE=ERRFIL)
         WRITE(42,101) ID,SIG(ID),IDRUG,RS(ID,2*IDRUG-1),RS(ID,2*IDRUG)
        CLOSE(42)
          CALL PAUSE
          STOP
         ENDIF
        END DO
        CALL ORDERDELTA(NDRUG,DELTAIV,NDELTA,ORDELT)
        DELDOSE = -SIG(ID)
        NSECTION = NSECTION + 1
        TIMDELAY(NSECTION) = 100.D0*DELDOSE
        DO ISET = 1,101
         ILINE = ILINE + 1
         DO IDRUG = 1,NDRUG
          RSS(ILINE,2*IDRUG-1) = RS(ID,2*IDRUG-1)
          RSS(ILINE,2*IDRUG) = RS(ID,2*IDRUG)
          IF(RS(ID,2*IDRUG-1) .GT. 0.D0) RSS(ILINE,2*IDRUG) = 0.D0
         END DO
         DO IADD = 1,NADD
          RSS(ILINE,2*NDRUG+IADD) = RS(ID,2*NDRUG+IADD)
         END DO
         IF(ISET .EQ. 1) THEN
          SIGG(ILINE) = SIG(ID)
          DOSESTART = 0.D0
         ENDIF
         IF(ISET .GT. 1) THEN
          SIGG(ILINE) = (ISET-1)*DELDOSE
          DOSESTART = SIGG(ILINE)
         ENDIF
        IF(NDELTA .GT. 0) THEN
         DO INDEL = 1,NDELTA
          ILINE = ILINE + 1
          DO IDRUG = 1,NDRUG
           RSS(ILINE,2*IDRUG-1) = 0.D0
           IF(DELTAIV(IDRUG) .GT. ORDELT(INDEL))
     1      RSS(ILINE,2*IDRUG-1) = RS(ID,2*IDRUG-1)
           RSS(ILINE,2*IDRUG) = 0.D0
          END DO
          DO IADD = 1,NADD
           RSS(ILINE,2*NDRUG+IADD) = RS(ID,2*NDRUG+IADD)
          END DO
          SIGG(ILINE) = DOSESTART + ORDELT(INDEL)
         END DO
        ENDIF
        END DO
       ENDIF
      END DO
1920   BACKSPACE(23)
       BACKSPACE(23)
       READ(23,1717,IOSTAT=IEND) READLINE
	 IF(IEND .LT. 0) THEN
        WRITE(*,1721)
        OPEN(42,FILE=ERRFIL)
         WRITE(42,1721)
        CLOSE(42)
	  CALL PAUSE
	  STOP
	 ENDIF
       IF(READLINE(12:23) .NE. 'NO. OF TOTAL') GO TO 1920
       BACKSPACE(23)
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
 1820  BACKSPACE(23)
       BACKSPACE(23)
       READ(23,1717,IOSTAT=IEND) READLINE
	 IF(IEND .LT. 0) THEN
        WRITE(*,1721)
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
       READ(23,1717) READLINE
       WRITE(27,133) ILINE
  133  FORMAT(I6,' ... NO. OF DOSE EVENTS')
       DO I = 1,2
        READ(23,1717) READLINE
        WRITE(27,1717) READLINE
       END DO
       SIGLAST = -999999.D0
       DO I = 1,ILINE
        WRITE(27,*) SIGG(I),(RSS(I,J),J=1,NI)
        CALL THESAME(SIGLAST,SIGG(I),ISAME)
        IF(ISAME .EQ. 1) THEN
         XVERIFY(1) = SIGLAST
         CALL VERIFYVAL(1,XVERIFY)
         WRITE(*,4031) XVERIFY(1)
 4031    FORMAT(/' IN SUBROUTINE NEWWORK1, TWO CONSECUTIVE DOSE TIMES'/
     1' HAVE THE SAME VALUE IN WORKING COPY FORMAT, ',F20.8//
     2' THIS COULD CAUSE UNEXPECTED RESULTS IF THE PROGRAM WERE TO '/
     3' CONTINUE. SO THE PROGRAM NOW STOPS. PLEASE CHECK YOUR PATIENT '/
     4' INFORMATION AND CORRECT (NOTE THAT THIS CAN HAPPEN IF THE '/
     5' FIRST DOSE FOLLOWING A STEADY STATE DOSE SET HAS THE SAME'/
     6' STARTING TIME AS THE ENDING TIME OF THE LAST STEADY STATE '/
     7' DOSE SET.)'//)
        OPEN(42,FILE=ERRFIL)
         WRITE(42,4031) SIGLAST
        CLOSE(42)
	  CALL PAUSE
	  STOP
	 ENDIF
       SIGLAST = SIGG(I)
       END DO
       DO I = 1,ND
        READ(23,*) SIG(I),(RS(I,J),J=1,NI)
       END DO
       DO I = 1,3
        READ(23,1717) READLINE
        WRITE(27,1717) READLINE
       END DO
      DO I = 1,M
       WRITE(27,*) TIMM(I),(YO(I,J),J=1,NUMEQT)
       READ(23,*) TIM(I),(YO(I,J),J=1,NUMEQT)
      END DO
   70	 READ(23,1717,IOSTAT=IEND) READLINE
       IF(IEND .LT. 0) GO TO 200
   	 IF(READLINE(3:16) .EQ. 'LAST AND FIRST') GO TO 200
       WRITE(27,1717) READLINE
       GO TO 70
  200	 BACKSPACE(23)
      ENDIF
      RETURN
      END
      SUBROUTINE Old_ORDERDELTA(NDRUG,DELTAIV,NDELTA,ORDELT)
      use npag_utils, only: thesame
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION DELTAIV(7),ORDELT(7),X(7)
      DO IDRUG = 1,NDRUG
       X(IDRUG) = DELTAIV(IDRUG)
      END DO
      DO IDRUG = 2, NDRUG
       IDRUGNEW = IDRUG
       ICOMP = IDRUG
  110  ICOMP = ICOMP - 1
       IF(X(IDRUGNEW) .LT. X(ICOMP)) THEN
        VALUE = X(IDRUGNEW)
        X(IDRUGNEW) = X(ICOMP)
        X(ICOMP) = VALUE
        IDRUGNEW = ICOMP
        IF(IDRUGNEW .EQ. 1) GO TO 150
        IF(IDRUGNEW .GT. 1) GO TO 110
       ENDIF
  150 END DO
      NDELTA = 0
      DO IDRUG = 1,NDRUG
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
      RETURN
      END
	SUBROUTINE Old_THESAME(X1,X2,ISAME)
	IMPLICIT REAL*8(A-H,O-Z)
	ISAME = 0
	XDEL = DABS(X1-X2)
	IF(XDEL .LE. 1.D-10) ISAME = 1
	RETURN
	END
      SUBROUTINE Old_VERIFYVAL(N,X)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(100)
      DO I = 1,N
       IF(X(I) .GE. -1.D-99 .AND. X(I) .LE. 1.D-99) X(I) = 0.D0
      END DO
      RETURN
      END
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
        DIMENSION VEC(NTOTPAR),
     1 YO(max_m_per_obs,NUMEQT),
     2 C0(NUMEQT),C1(NUMEQT),C2(NUMEQT),C3(NUMEQT),C4(NUMEQT),C5(NUMEQT)
      double precision gamma, flat
      double precision, dimension(max_pop_rand_varbs,2) :: AB
      double precision, dimension(max_ODE_params) :: PX
      integer, dimension(max_ODE_params) :: IRAN
      integer NOFIX,NSUB
       character*20 errfilname
       integer  NDIM,MF
       real*8 RTOL
       real*8, dimension(max_ODE_comps) :: ATOL
       real*8, dimension(max_m_per_obs) :: TIMCOPY
       real*8, dimension(max_doses) :: SIGCOPY
       real*8, dimension(max_doses,max_RS_J) :: RSCOPY
       real*8, dimension(max_doses,max_input_dim) :: BSCOPY
       integer, dimension(128) :: INTLIST
       integer, dimension(257) :: IPAR
       double precision, dimension(max_m_per_obs,MAXNUMEQ) :: ObsError
       double precision, dimension(257) :: RPAR
       integer NNORMALOBS, NPOISSONOBS
       integer, dimension(max_input_dim) :: NBCOMP
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
        VN = VEC(NVEC)
        IF(VN .GT. AB(IRANO,2) .OR. VN .LT. AB(IRANO,1)) THEN
         FNTVAL = 1.D30
         RETURN
        ENDIF
        PX(I) = VEC(NVEC)
       ENDIF
      END DO
      SUMTOT = 0.D0
      REWIND(27)
      DO JSUB = 1,NSUB
       CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,
     1    TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     2    INTLIST,errfilname)
        call cp_lrcs_to_rpar(gamma,flat,numeqt,c0,c1,c2,c3,c4,c5,rpar)
       NPOISSONOBS=0
       NNORMALOBS=0
       DO 140 I=1,NOBSER
        DO 140 J=1,NUMEQT
         Y = YO(I,J)
         IF(Y .EQ. -99) GO TO 140
             if (C4(J).eq.10) IPAR(i_is_log10+J) = 10
          if (C5(J).eq.229) then
             write (*,*) "Poisson analysis req. for OUTEQ",J
             NPOISSONOBS=NPOISSONOBS+1
             ObsError(I,J)=Y
             IPAR(i_is_poisson_obs+J)=229
         else
         ObsError(I,J) = C0(J)+C1(J)*Y+C2(J)*Y*Y+C3(J)*Y**3
         if(ierrmod.eq.2) ObsError(i,j) = ObsError(i,j)*gamma
         if(ierrmod.eq.3) ObsError(i,j)=dsqrt(ObsError(i,j)**2
     &     + gamma**2)
         if(ierrmod.eq.4) ObsError(i,j) = gamma*flat
         endif
  140    CONTINUE
        NPX = NVAR+NOFIX+NRANFIX
        CALL IDPC(JSUB,IG,NPX,PX,NBCOMP,W,NOBSER,NUMEQT,
     1    NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     2    INTLIST,IPAR,ObsError,RPAR,ERRFILNAME)
       SUMTOT = SUMTOT + W
      END DO
      FNTVAL = SUMTOT
	RETURN
	END
        SUBROUTINE ELDERY(N,START,XMIN,YNEWLO,REQMIN,STEP,ITMAX,
     1    FUNC,IPRINT,ICONV,NITER,ICOUNT,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     2    NBCOMP,NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     3    INTLIST,IPAR,ObsError,RPAR,gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     4    errfilname)
      use npag_utils, only: maxnumeq,max_m_per_obs,max_ODE_params
     1  ,max_pop_rand_varbs,max_doses,max_ODE_comps,max_RS_J
     2  ,max_input_dim
      implicit none
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
        DATA RCOEFF/1.0D0/,ECOEFF/2.0D0/,CCOEFF/.5D0/
        KCOUNT=1000000
        ICOUNT=0
        NITER=0
        ICONV=0
        IF(REQMIN.LE.0.0D0) ICOUNT=ICOUNT-1
        IF(N.LE.0) ICOUNT=ICOUNT-10
        IF(N.GT.99) ICOUNT=ICOUNT-10
        IF(ICOUNT.LT.0) RETURN
        DABIT=2.04607D-35
        BIGNUM=1.0D+38
        KONVGE=5
        XN = 1.d0 * N
        DN = 1.d0* N
        FN = 0.d0
        NN=N+1
1001    DO 1 I=1,N
1       P(I,NN)=START(I)
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     2    NBCOMP,NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,
     3    RSCOPY,BSCOPY,INTLIST,IPAR,ObsError,RPAR,
     4    gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     5    errfilname)
        Y(NN)=FN
        ICOUNT=ICOUNT+1
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
        IF(ICOUNT.LE.NN) YOLDLO=YLO
        IF(ICOUNT.LE.NN) GO TO 2002
        IF(YLO.GE.YOLDLO) GO TO 2002
        YOLDLO=YLO
        NITER=NITER+1
        IF(NITER.GE.ITMAX) GO TO 900
        IF(IPRINT.EQ.0) GO TO 2002
2002    DCHK=(YNEWLO+DABIT)/(YLO+DABIT)-1.0D0
        IF(DABS(DCHK).GT. REQMIN) GO TO 2001
        ICONV=1
        GO TO 900
2001    KONVGE=KONVGE-1
        IF(KONVGE.NE.0) GO TO 2020
        KONVGE=5
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
        DO 7 I=1,N
        Z=0.0D0
        DO 6 J=1,NN
6       Z=Z+P(I,J)
        Z=Z-P(I,IHI)
7       PBAR(I)=Z/DN
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
        DO 9 I=1,N
9       P2STAR(I)=ECOEFF*PSTAR(I)+(1.0D0-ECOEFF)*PBAR(I)
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     2    NBCOMP,NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,
     3    RSCOPY,BSCOPY,INTLIST,IPAR,ObsError,RPAR,
     4    gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     5    errfilname)
        Y2STAR=FN
        ICOUNT=ICOUNT+1
        IF(Y2STAR.GE.YSTAR) GO TO 19
10      DO 11 I=1,N
11      P(I,IHI)=P2STAR(I)
        Y(IHI)=Y2STAR
        GO TO 1000
12      L=0
        DO 13 I=1,NN
        IF(Y(I).GT.YSTAR) L=L+1
13      CONTINUE
        IF(L.GT.1) GO TO 19
        IF(L.EQ.0) GO TO 15
        DO 14 I=1,N
14      P(I,IHI)=PSTAR(I)
        Y(IHI)=YSTAR
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
19      CONTINUE
        DO 20 I=1,N
20      P(I,IHI)=PSTAR(I)
        Y(IHI)=YSTAR
        GO TO 1000
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
      subroutine emint(psi,ldpsi,theta,ldtheta,npoint,nsub,ijob,
     &                 x,dx,y,dy,fobj,gap,nvar,keep,IHESS,ERRFIL,
     &                 isupres)
      use npag_utils, only: verifyval
      implicit real*8 (a-h,o-z)
      integer ldpsi,ldtheta,npoint,nsub,ijob,nvar,IHESS
      dimension psi(ldpsi,*),theta(ldtheta,*),x(*),dx(*),y(*),dy(*)
      CHARACTER ERRFIL*20
      integer isupres
      real*8 mu
      parameter (MAXSUBem=999,MAXACTem=10000000)
      dimension w(MAXSUBem),dw(MAXSUBem),Ptx(MAXSUBem),
     &          hess(MAXSUBem,2*MAXSUBem)
      dimension psisum(MAXSUBem),XVERIFY(100)
      integer kpvt(MAXSUBem), ipivot(MAXACTem), list(MAXACTem)
       keep = 0
      write (*,*) "Enterred emint(); ijob =",ijob
      if(nsub.gt.MAXSUBem) then
      write(6,*) 'nsub =',nsub, ' is greater than MAXSUBem=',MAXSUBem
      write(6,*) 'MAXSUBem needs to be reset as large as nsub'
      write(6,*) 'in PARAMETER statement in subroutine emint'
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
        OPEN(42,FILE=ERRFIL)
      write(42,*) 'npoint=',npoint,' is larger than MAXACTem=',MAXACTem
      write(42,*) 'MAXACTem needs to be reset as large as npoint'
      write(42,*) 'in PARAMETER statement in subroutine emint'
        CLOSE(42)
      CALL PAUSE
      stop
      endif
      psimin=0.
      do j=1,nsub
      do i=1,npoint
      if(psi(j,i).le.psimin) psimin=psi(j,i)
      enddo
      enddo
      if(psimin.lt.0) then
        write(6,*) 'Psi matrix not non-negative -stop'
        OPEN(42,FILE=ERRFIL)
        write(42,*) 'Psi matrix not non-negative -stop'
        CLOSE(42)
        CALL PAUSE
        stop
      endif
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
        OPEN(42,FILE=ERRFIL)
        write(42,*) 'psi has a zero row -stop'
        CLOSE(42)
        CALL PAUSE
        stop
        endif
        w(j)=1./s
      enddo
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
        OPEN(42,FILE=ERRFIL)
        write(42,*) 'Psi has a zero column -stop'
        CLOSE(42)
        CALL PAUSE
        stop
      endif
      eps=1.d-10
      sig=0.d0
      mu=0.d0
      do i=1,npoint
        x(i)=1.d0*shrink
        y(i)=y(i)/shrink
        y(i)=1.d0-y(i)
        mu=mu+x(i)*y(i)
      enddo
      mu=mu/npoint
      rmax = -1.e38
      do j=1,nsub
        w(j)=w(j)/shrink
        Ptx(j)=Ptx(j)*shrink
        if(dabs(1.d0-w(j)*Ptx(j)).ge.rmax) rmax =
     &  dabs(1.d0-w(j)*Ptx(j))
      enddo
      gap=1.d0
      iter=0
100   continue
      conval = mu
      if(conval .lt. rmax) conval = rmax
      if(conval .lt. gap) conval = gap
      convcrit = eps/conval
      XVERIFY(1) = convcrit
      CALL VERIFYVAL(1,XVERIFY)
       WRITE(*,123) iter,XVERIFY(1)
  123 FORMAT(' Iteration ',I9,' CONV. CRIT = ',G15.2,' (1 OR HIGHER FOR
     1CONVERGENCE)')
      if(mu.le.eps.and.rmax.le.eps.and.gap.le.eps) go to 9000
      iter=iter+1
        ILOOP = ILOOP + 1
      tbuilda=0
      smu=sig*mu
      do j=1,nsub
        do k=1,nsub
           hess(j,k)=0.
        enddo
      enddo
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
      do j=1,nsub
        hess(j,j)=hess(j,j)+Ptx(j)/w(j)
      enddo
      tbuildb=0
      tbuild=tbuildb-tbuilda
      IF(ISUPRES .EQ. 0) write(6,*) 'tbuild=',tbuild
      CALL DPOTRF( 'L', nsub, hess, MAXSUBem, INFO, ERRFIL )
      tbuildc=0
      tfactor=tbuildc-tbuildb
      IF(ISUPRES .EQ. 0) write(6,*) 'tfactor=',tfactor
      write(6,*) 'gap,info,tfactor=',gap,info,tfactor
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
        OPEN(42,FILE=ERRFIL)
         WRITE(42,163)
         WRITE(42,164) info
        CLOSE(42)
      return
      endif
      do j=1,nsub
        sum=0.d0
        do i=1,npoint
          sum=sum+psi(j,i)*smu/y(i)
        enddo
        dw(j)=1.d0/w(j)-sum
      enddo
      call DPOTRS( 'L', nsub, 1, hess, MAXSUBem, dw, nsub,INFO,ERRFIL)
      do i=1,npoint
        sum=0.
        do j=1,nsub
          sum=sum+psi(j,i)*dw(j)
        enddo
        dy(i)=-sum
        dx(i)=smu/y(i)-x(i)-dy(i)*x(i)/y(i)
      enddo
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
9000  continue
      sumx=0.
      do i=1,npoint
      sumx=sumx+x(i)
      enddo
      do i=1,npoint
      x(i)=x(i)/sumx
      enddo
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
      if(dabs(psi(i,i)/test).ge.1.d-8) keep=keep+1
      enddo
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
      do i=1,isum
      do j=1,nsub
      psi(j,i)=psi(j,i+isum)
      enddo
      enddo
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
        write (*,*)
        write (*,*) "isum,keep", isum, keep
        write (*,*)
      return
      end
      subroutine dpoco(a,lda,n,rcond,z,info)
      integer lda,n,info
      double precision a(lda,1),z(1)
      double precision rcond
      double precision ddot,ek,t,wk,wkm
      double precision anorm,s,dasum,sm,ynorm
      integer i,j,jm1,k,kb,kp1
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
      call dpofa(a,lda,n,info)
      if (info .ne. 0) go to 180
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
         ynorm = 1.0d0
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
         s = 1.0d0/dasum(n,z,1)
         call dscal(n,s,z,1)
         ynorm = s*ynorm
         if (anorm .ne. 0.0d0) rcond = ynorm/anorm
         if (anorm .eq. 0.0d0) rcond = 0.0d0
  180 continue
      return
      end
      subroutine dpofa(a,lda,n,info)
      integer lda,n,info
      double precision a(lda,1)
      double precision ddot,t
      double precision s
      integer j,jm1,k
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
      double precision ddot,t
      integer k,kb
      do 10 k = 1, n
         t = ddot(k-1,a(1,k),1,b(1),1)
         b(k) = (b(k) - t)/a(k,k)
   10 continue
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
      double precision ak,akm1,bk,bkm1,denom,mulk,mulkm1,t
      double precision absakk,alpha,colmax,rowmax
      integer imax,imaxp1,j,jj,jmax,k,km1,km2,kstep,idamax
      logical swap
      alpha = (1.0d0 + dsqrt(17.0d0))/8.0d0
      info = 0
      k = n
   10 continue
         if (k .eq. 0) go to 200
         if (k .gt. 1) go to 20
            kpvt(1) = 1
            if (a(1,1) .eq. 0.0d0) info = 1
            go to 200
   20    continue
         km1 = k - 1
         absakk = dabs(a(k,k))
         imax = idamax(k-1,a(1,k),1)
         colmax = dabs(a(imax,k))
         if (absakk .lt. alpha*colmax) go to 30
            kstep = 1
            swap = .false.
         go to 90
   30    continue
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
            kpvt(k) = k
            info = k
         go to 190
  100    continue
         if (kstep .eq. 2) go to 140
            if (.not.swap) go to 120
               call dswap(imax,a(1,imax),1,a(1,k),1)
               do 110 jj = imax, k
                  j = k + imax - jj
                  t = a(j,k)
                  a(j,k) = a(imax,j)
                  a(imax,j) = t
  110          continue
  120       continue
            do 130 jj = 1, km1
               j = k - jj
               mulk = -a(j,k)/a(k,k)
               t = mulk
               call daxpy(j,t,a(1,k),1,a(1,j),1)
               a(j,k) = mulk
  130       continue
            kpvt(k) = k
            if (swap) kpvt(k) = imax
         go to 190
  140    continue
            if (.not.swap) go to 160
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
      double precision ak,akm1,bk,bkm1,ddot,denom,temp
      integer k,kp
      k = n
   10 if (k .eq. 0) go to 80
         if (kpvt(k) .lt. 0) go to 40
            if (k .eq. 1) go to 30
               kp = kpvt(k)
               if (kp .eq. k) go to 20
                  temp = b(k)
                  b(k) = b(kp)
                  b(kp) = temp
   20          continue
               call daxpy(k-1,b(k),a(1,k),1,b(1),1)
   30       continue
            b(k) = b(k)/a(k,k)
            k = k - 1
         go to 70
   40    continue
            if (k .eq. 2) go to 60
               kp = iabs(kpvt(k))
               if (kp .eq. k - 1) go to 50
                  temp = b(k-1)
                  b(k-1) = b(kp)
                  b(kp) = temp
   50          continue
               call daxpy(k-2,b(k),a(1,k),1,b(1),1)
               call daxpy(k-2,b(k-1),a(1,k-1),1,b(1),1)
   60       continue
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
      k = 1
   90 if (k .gt. n) go to 160
         if (kpvt(k) .lt. 0) go to 120
            if (k .eq. 1) go to 110
               b(k) = b(k) + ddot(k-1,a(1,k),1,b(1),1)
               kp = kpvt(k)
               if (kp .eq. k) go to 100
                  temp = b(k)
                  b(k) = b(kp)
                  b(kp) = temp
  100          continue
  110       continue
            k = k + 1
         go to 150
  120    continue
            if (k .eq. 1) go to 140
               b(k) = b(k) + ddot(k-1,a(1,k),1,b(1),1)
               b(k+1) = b(k+1) + ddot(k-1,a(1,k+1),1,b(1),1)
               kp = iabs(kpvt(k))
               if (kp .eq. k) go to 130
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
      integer j,jp,l,lp1,lup,maxj,pl,pu
      double precision maxnrm,dnrm2,tt
      double precision ddot,nrmxl,t
      logical negj,swapj
      pl = 1
      pu = 0
      if (job .eq. 0) go to 60
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
      if (pu .lt. pl) go to 80
      do 70 j = pl, pu
         qraux(j) = dnrm2(n,x(1,j),1)
         work(j) = qraux(j)
   70 continue
   80 continue
      lup = min0(n,p)
      do 200 l = 1, lup
         if (l .lt. pl .or. l .ge. pu) go to 120
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
            nrmxl = dnrm2(n-l+1,x(l,l),1)
            if (nrmxl .eq. 0.0d0) go to 180
               if (x(l,l) .ne. 0.0d0) nrmxl = dsign(nrmxl,x(l,l))
               call dscal(n-l+1,1.0d0/nrmxl,x(l,l),1)
               x(l,l) = 1.0d0 + x(l,l)
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
               qraux(l) = x(l,l)
               x(l,l) = -nrmxl
  180       continue
  190    continue
  200 continue
      return
      end
      SUBROUTINE DPOTRF( UPLO, N, A, LDA, INFO, ERRFIL )
      CHARACTER          UPLO
      INTEGER            INFO, LDA, N
      DOUBLE PRECISION   A( LDA, * )
      CHARACTER*20       ERRFIL
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
      LOGICAL            UPPER
      INTEGER            J, JB, NB
      LOGICAL            LSAME
      EXTERNAL           LSAME
      EXTERNAL           DGEMM, DPOTF2, DSYRK, DTRSM, XERBLA
      INTRINSIC          MAX, MIN
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
      IF( N.EQ.0 )
     $   RETURN
      nb = 16
      IF( NB.LE.1 .OR. NB.GE.N ) THEN
         CALL DPOTF2( UPLO, N, A, LDA, INFO, ERRFIL )
      ELSE
         IF( UPPER ) THEN
            DO 10 J = 1, N, NB
               JB = MIN( NB, N-J+1 )
               CALL DSYRK( 'Upper', 'Transpose', JB, J-1, -ONE,
     $                     A( 1, J ), LDA, ONE, A( J, J ), LDA )
               CALL DPOTF2( 'Upper', JB, A( J, J ), LDA, INFO, ERRFIL )
               IF( INFO.NE.0 )
     $            GO TO 30
               IF( J+JB.LE.N ) THEN
                  CALL DGEMM( 'Transpose', 'No transpose', JB, N-J-JB+1,
     $                        J-1, -ONE, A( 1, J ), LDA, A( 1, J+JB ),
     $                        LDA, ONE, A( J, J+JB ), LDA )
                  CALL DTRSM( 'Left', 'Upper', 'Transpose', 'Non-unit',
     $                        JB, N-J-JB+1, ONE, A( J, J ), LDA,
     $                        A( J, J+JB ), LDA )
               END IF
   10       CONTINUE
         ELSE
            DO 20 J = 1, N, NB
               JB = MIN( NB, N-J+1 )
               CALL DSYRK( 'Lower', 'No transpose', JB, J-1, -ONE,
     $                     A( J, 1 ), LDA, ONE, A( J, J ), LDA )
               CALL DPOTF2( 'Lower', JB, A( J, J ), LDA, INFO, ERRFIL )
               IF( INFO.NE.0 )
     $            GO TO 30
               IF( J+JB.LE.N ) THEN
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
   30 CONTINUE
      INFO = INFO + J - 1
   40 CONTINUE
      RETURN
      END
      SUBROUTINE DPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO, ERRFIL )
      CHARACTER          UPLO
      INTEGER            INFO, LDA, LDB, N, NRHS
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
      CHARACTER*20       ERRFIL
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
      LOGICAL            UPPER
      LOGICAL            LSAME
      EXTERNAL           LSAME
      EXTERNAL           DTRSM, XERBLA
      INTRINSIC          MAX
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
      IF( N.EQ.0 .OR. NRHS.EQ.0 )
     $   RETURN
      IF( UPPER ) THEN
         CALL DTRSM( 'Left', 'Upper', 'Transpose', 'Non-unit', N, NRHS,
     $               ONE, A, LDA, B, LDB )
         CALL DTRSM( 'Left', 'Upper', 'No transpose', 'Non-unit', N,
     $               NRHS, ONE, A, LDA, B, LDB )
      ELSE
         CALL DTRSM( 'Left', 'Lower', 'No transpose', 'Non-unit', N,
     $               NRHS, ONE, A, LDA, B, LDB )
         CALL DTRSM( 'Left', 'Lower', 'Transpose', 'Non-unit', N, NRHS,
     $               ONE, A, LDA, B, LDB )
      END IF
      RETURN
      END
      SUBROUTINE DPOTF2( UPLO, N, A, LDA, INFO, ERRFIL )
      CHARACTER          UPLO
      INTEGER            INFO, LDA, N
      DOUBLE PRECISION   A( LDA, * )
      CHARACTER*20       ERRFIL
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      LOGICAL            UPPER
      INTEGER            J
      DOUBLE PRECISION   AJJ
      LOGICAL            LSAME
      DOUBLE PRECISION   DDOT
      EXTERNAL           LSAME, DDOT
      EXTERNAL           DGEMV, DSCAL, XERBLA
      INTRINSIC          MAX, SQRT
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
      IF( N.EQ.0 )
     $   RETURN
      IF( UPPER ) THEN
         DO 10 J = 1, N
            AJJ = A( J, J ) - DDOT( J-1, A( 1, J ), 1, A( 1, J ), 1 )
            IF( AJJ.LE.ZERO ) THEN
               A( J, J ) = AJJ
               GO TO 30
            END IF
            AJJ = SQRT( AJJ )
            A( J, J ) = AJJ
            IF( J.LT.N ) THEN
               CALL DGEMV( 'Transpose', J-1, N-J, -ONE, A( 1, J+1 ),
     $                     LDA, A( 1, J ), 1, ONE, A( J, J+1 ), LDA )
               CALL DSCAL( N-J, ONE / AJJ, A( J, J+1 ), LDA )
            END IF
   10    CONTINUE
      ELSE
         DO 20 J = 1, N
            AJJ = A( J, J ) - DDOT( J-1, A( J, 1 ), LDA, A( J, 1 ),
     $            LDA )
            IF( AJJ.LE.ZERO ) THEN
               A( J, J ) = AJJ
               GO TO 30
            END IF
            AJJ = SQRT( AJJ )
            A( J, J ) = AJJ
            IF( J.LT.N ) THEN
               CALL DGEMV( 'No transpose', N-J, J-1, -ONE, A( J+1, 1 ),
     $                     LDA, A( J, 1 ), LDA, ONE, A( J+1, J ), 1 )
               CALL DSCAL( N-J, ONE / AJJ, A( J+1, J ), 1 )
            END IF
   20    CONTINUE
      END IF
      GO TO 40
   30 CONTINUE
      INFO = J
   40 CONTINUE
      RETURN
      END
      LOGICAL          FUNCTION LSAME( CA, CB )
      CHARACTER          CA, CB
      INTRINSIC          ICHAR
      INTEGER            INTA, INTB, ZCODE
      LSAME = CA.EQ.CB
      IF( LSAME )
     $   RETURN
      ZCODE = ICHAR( 'Z' )
      INTA = ICHAR( CA )
      INTB = ICHAR( CB )
      IF( ZCODE.EQ.90 .OR. ZCODE.EQ.122 ) THEN
         IF( INTA.GE.97 .AND. INTA.LE.122 ) INTA = INTA - 32
         IF( INTB.GE.97 .AND. INTB.LE.122 ) INTB = INTB - 32
      ELSE IF( ZCODE.EQ.233 .OR. ZCODE.EQ.169 ) THEN
         IF( INTA.GE.129 .AND. INTA.LE.137 .OR.
     $       INTA.GE.145 .AND. INTA.LE.153 .OR.
     $       INTA.GE.162 .AND. INTA.LE.169 ) INTA = INTA + 64
         IF( INTB.GE.129 .AND. INTB.LE.137 .OR.
     $       INTB.GE.145 .AND. INTB.LE.153 .OR.
     $       INTB.GE.162 .AND. INTB.LE.169 ) INTB = INTB + 64
      ELSE IF( ZCODE.EQ.218 .OR. ZCODE.EQ.250 ) THEN
         IF( INTA.GE.225 .AND. INTA.LE.250 ) INTA = INTA - 32
         IF( INTB.GE.225 .AND. INTB.LE.250 ) INTB = INTB - 32
      END IF
      LSAME = INTA.EQ.INTB
      END
      SUBROUTINE XERBLA( SRNAME, INFO, ERRFIL )
      CHARACTER*6        SRNAME
      CHARACTER*20       ERRFIL
      INTEGER            INFO
      WRITE( *, FMT = 9999 )SRNAME, INFO
        OPEN(42,FILE=ERRFIL)
      WRITE( 42, FMT = 9999 )SRNAME, INFO
        CLOSE(42)
      CALL PAUSE
      STOP
 9999 FORMAT( ' ** On entry to ', A6, ' parameter number ', I2, ' had ',
     $      'an illegal value' )
      END
