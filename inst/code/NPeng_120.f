      !subroutine pause(message_in)
      !use, intrinsic:: iso_fortran_env, only: stdin=>input_unit
      !character(*), optional :: message_in
      !character(len=:), allocatable :: message
      !message = 'paused. please hit any key to continue: '
      !if (present(message_in)) message = message_in
      !write(6,'(a)') message; read(stdin,*)
      !return
      !end subroutine pause
      subroutine pause
      use, intrinsic:: iso_fortran_env, only: stdin=>input_unit
      character(len=:), allocatable :: message
      message = 'PAUSED. PLEASE HIT ANY KEY TO CONTINUE: '
      write(6,'(a)') message; read(stdin,*)
      return
      end subroutine pause
      SUBROUTINE DGEMM ( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,
     $                   BETA, C, LDC )
      CHARACTER*1        TRANSA, TRANSB
      INTEGER            M, N, K, LDA, LDB, LDC
      DOUBLE PRECISION   ALPHA, BETA
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * )
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
         CALL XERBLA( 'DGEMM ', INFO )
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
         CALL XERBLA( 'DGEMV ', INFO )
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
         CALL XERBLA( 'DSYRK ', INFO )
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
         CALL XERBLA( 'DTRSM ', INFO )
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
      SUBROUTINE DVODE (F, NEQ, Y, T, TOUT, ITOL, RTOL, ATOL, ITASK,
     1            ISTATE, IOPT, RWORK, LRW, IWORK, LIW, JAC, MF,
     2            RPAR, IPAR)
      EXTERNAL F, JAC
      DOUBLE PRECISION Y, T, TOUT, RTOL, ATOL, RWORK, RPAR
      INTEGER NEQ, ITOL, ITASK, ISTATE, IOPT, LRW, IWORK, LIW,
     1        MF, IPAR
      DIMENSION Y(*), ATOL(*), RWORK(LRW), IWORK(LIW)
      DOUBLE PRECISION ACNRM, CCMXJ, CONP, CRATE, DRC, EL,
     1     ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2     RC, RL1, TAU, TQ, TN, UROUND
      INTEGER ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     1        L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     2        LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     3        N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     4        NSLP, NYH
      DOUBLE PRECISION HU
      INTEGER NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
      EXTERNAL DVNLSD
      LOGICAL IHIT
      DOUBLE PRECISION ATOLI, BIG, EWTI, FOUR, H0, HMAX, HMX, HUN, ONE,
     1   PT2, RH, RTOLI, SIZE, TCRIT, TNEXT, TOLSF, TP, TWO, ZERO
      INTEGER I, IER, IFLAG, IMXER, JCO, KGO, LENIW, LENJ, LENP, LENRW,
     1   LENWM, LF0, MBAND, ML, MORD, MU, MXHNL0, MXSTP0, NITER, NSLAST
      CHARACTER*80 MSG
      DOUBLE PRECISION D1MACH, DVNORM
      DIMENSION MORD(2)
      SAVE MORD, MXHNL0, MXSTP0
      SAVE ZERO, ONE, TWO, FOUR, PT2, HUN
      COMMON /DVOD01/ ACNRM, CCMXJ, CONP, CRATE, DRC, EL(13),
     1                ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2                RC, RL1, TAU(13), TQ(5), TN, UROUND,
     3                ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     4                L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     5                LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     6                N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     7                NSLP, NYH
      COMMON /DVOD02/ HU, NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp Threadprivate(/DVOD01/,/DVOD02/,MORD,MXHNL0,MXSTP0,ZERO,ONE,TWO,
!$omp&FOUR,PT2,HUN)
      DATA  MORD(1) /12/, MORD(2) /5/, MXSTP0 /500/, MXHNL0 /10/
      DATA ZERO /0.0D0/, ONE /1.0D0/, TWO /2.0D0/, FOUR /4.0D0/,
     1     PT2 /0.2D0/, HUN /100.0D0/
      IF (ISTATE .LT. 1 .OR. ISTATE .GT. 3) GO TO 601
      IF (ITASK .LT. 1 .OR. ITASK .GT. 5) GO TO 602
      IF (ISTATE .EQ. 1) GO TO 10
      IF (INIT .NE. 1) GO TO 603
      IF (ISTATE .EQ. 2) GO TO 200
      GO TO 20
 10   INIT = 0
      IF (TOUT .EQ. T) RETURN
 20   IF (NEQ .LE. 0) GO TO 604
      IF (ISTATE .EQ. 1) GO TO 25
      IF (NEQ .GT. N) GO TO 605
 25   N = NEQ
      IF (ITOL .LT. 1 .OR. ITOL .GT. 4) GO TO 606
      IF (IOPT .LT. 0 .OR. IOPT .GT. 1) GO TO 607
      JSV = SIGN(1,MF)
      MF = ABS(MF)
      METH = MF/10
      MITER = MF - 10*METH
      IF (METH .LT. 1 .OR. METH .GT. 2) GO TO 608
      IF (MITER .LT. 0 .OR. MITER .GT. 5) GO TO 608
      IF (MITER .LE. 3) GO TO 30
      ML = IWORK(1)
      MU = IWORK(2)
      IF (ML .LT. 0 .OR. ML .GE. N) GO TO 609
      IF (MU .LT. 0 .OR. MU .GE. N) GO TO 610
 30   CONTINUE
      IF (IOPT .EQ. 1) GO TO 40
      MAXORD = MORD(METH)
      MXSTEP = MXSTP0
      MXHNIL = MXHNL0
      IF (ISTATE .EQ. 1) H0 = ZERO
      HMXI = ZERO
      HMIN = ZERO
      GO TO 60
 40   MAXORD = IWORK(5)
      IF (MAXORD .LT. 0) GO TO 611
      IF (MAXORD .EQ. 0) MAXORD = 100
      MAXORD = MIN(MAXORD,MORD(METH))
      MXSTEP = IWORK(6)
      IF (MXSTEP .LT. 0) GO TO 612
      IF (MXSTEP .EQ. 0) MXSTEP = MXSTP0
      MXHNIL = IWORK(7)
      IF (MXHNIL .LT. 0) GO TO 613
      IF (MXHNIL .EQ. 0) MXHNIL = MXHNL0
      IF (ISTATE .NE. 1) GO TO 50
      H0 = RWORK(5)
      IF ((TOUT - T)*H0 .LT. ZERO) GO TO 614
 50   HMAX = RWORK(6)
      IF (HMAX .LT. ZERO) GO TO 615
      HMXI = ZERO
      IF (HMAX .GT. ZERO) HMXI = ONE/HMAX
      HMIN = RWORK(7)
      IF (HMIN .LT. ZERO) GO TO 616
 60   LYH = 21
      IF (ISTATE .EQ. 1) NYH = N
      LWM = LYH + (MAXORD + 1)*NYH
      JCO = MAX(0,JSV)
      IF (MITER .EQ. 0) LENWM = 0
      IF (MITER .EQ. 1 .OR. MITER .EQ. 2) THEN
        LENWM = 2 + (1 + JCO)*N*N
        LOCJS = N*N + 3
      ENDIF
      IF (MITER .EQ. 3) LENWM = 2 + N
      IF (MITER .EQ. 4 .OR. MITER .EQ. 5) THEN
        MBAND = ML + MU + 1
        LENP = (MBAND + ML)*N
        LENJ = MBAND*N
        LENWM = 2 + LENP + JCO*LENJ
        LOCJS = LENP + 3
        ENDIF
      LEWT = LWM + LENWM
      LSAVF = LEWT + N
      LACOR = LSAVF + N
      LENRW = LACOR + N - 1
      IWORK(17) = LENRW
      LIWM = 1
      LENIW = 30 + N
      IF (MITER .EQ. 0 .OR. MITER .EQ. 3) LENIW = 30
      IWORK(18) = LENIW
      IF (LENRW .GT. LRW) GO TO 617
      IF (LENIW .GT. LIW) GO TO 618
      RTOLI = RTOL
      ATOLI = ATOL(1)
      DO 70 I = 1,N
        IF (ITOL .GE. 3) RTOLI = RTOL
        IF (ITOL .EQ. 2 .OR. ITOL .EQ. 4) ATOLI = ATOL(I)
        IF (RTOLI .LT. ZERO) GO TO 619
        IF (ATOLI .LT. ZERO) GO TO 620
 70     CONTINUE
      IF (ISTATE .EQ. 1) GO TO 100
      JSTART = -1
      IF (NQ .LE. MAXORD) GO TO 90
      CALL DCOPY (N, RWORK(LWM), 1, RWORK(LSAVF), 1)
 90   IF (MITER .GT. 0) RWORK(LWM) = SQRT(UROUND)
 100  UROUND = D1MACH(4)
      TN = T
      IF (ITASK .NE. 4 .AND. ITASK .NE. 5) GO TO 110
      TCRIT = RWORK(1)
      IF ((TCRIT - TOUT)*(TOUT - T) .LT. ZERO) GO TO 625
      IF (H0 .NE. ZERO .AND. (T + H0 - TCRIT)*H0 .GT. ZERO)
     1   H0 = TCRIT - T
 110  JSTART = 0
      IF (MITER .GT. 0) RWORK(LWM) = SQRT(UROUND)
      CCMXJ = PT2
      MSBJ = 50
      NHNIL = 0
      NST = 0
      NJE = 0
      NNI = 0
      NCFN = 0
      NETF = 0
      NLU = 0
      NSLJ = 0
      NSLAST = 0
      HU = ZERO
      NQU = 0
      LF0 = LYH + NYH
      CALL F (N, T, Y, RWORK(LF0), RPAR, IPAR)
      NFE = 1
      CALL DCOPY (N, Y, 1, RWORK(LYH), 1)
      NQ = 1
      H = ONE
      CALL DEWSET (N, ITOL, RTOL, ATOL, RWORK(LYH), RWORK(LEWT))
      DO 120 I = 1,N
        IF (RWORK(I+LEWT-1) .LE. ZERO) GO TO 621
 120    RWORK(I+LEWT-1) = ONE/RWORK(I+LEWT-1)
      IF (H0 .NE. ZERO) GO TO 180
      CALL DVHIN (N, T, RWORK(LYH), RWORK(LF0), F, RPAR, IPAR, TOUT,
     1   UROUND, RWORK(LEWT), ITOL, ATOL, Y, RWORK(LACOR), H0,
     2   NITER, IER)
      NFE = NFE + NITER
      IF (IER .NE. 0) GO TO 622
 180  RH = ABS(H0)*HMXI
      IF (RH .GT. ONE) H0 = H0/RH
      H = H0
      CALL DSCAL (N, H0, RWORK(LF0), 1)
      GO TO 270
 200  NSLAST = NST
      KUTH = 0
      GO TO (210, 250, 220, 230, 240), ITASK
 210  IF ((TN - TOUT)*H .LT. ZERO) GO TO 250
      CALL DVINDY (TOUT, 0, RWORK(LYH), NYH, Y, IFLAG)
      IF (IFLAG .NE. 0) GO TO 627
      T = TOUT
      GO TO 420
 220  TP = TN - HU*(ONE + HUN*UROUND)
      IF ((TP - TOUT)*H .GT. ZERO) GO TO 623
      IF ((TN - TOUT)*H .LT. ZERO) GO TO 250
      GO TO 400
 230  TCRIT = RWORK(1)
      IF ((TN - TCRIT)*H .GT. ZERO) GO TO 624
      IF ((TCRIT - TOUT)*H .LT. ZERO) GO TO 625
      IF ((TN - TOUT)*H .LT. ZERO) GO TO 245
      CALL DVINDY (TOUT, 0, RWORK(LYH), NYH, Y, IFLAG)
      IF (IFLAG .NE. 0) GO TO 627
      T = TOUT
      GO TO 420
 240  TCRIT = RWORK(1)
      IF ((TN - TCRIT)*H .GT. ZERO) GO TO 624
 245  HMX = ABS(TN) + ABS(H)
      IHIT = ABS(TN - TCRIT) .LE. HUN*UROUND*HMX
      IF (IHIT) GO TO 400
      TNEXT = TN + HNEW*(ONE + FOUR*UROUND)
      IF ((TNEXT - TCRIT)*H .LE. ZERO) GO TO 250
      H = (TCRIT - TN)*(ONE - FOUR*UROUND)
      KUTH = 1
 250  CONTINUE
      IF ((NST-NSLAST) .GE. MXSTEP) GO TO 500
      CALL DEWSET (N, ITOL, RTOL, ATOL, RWORK(LYH), RWORK(LEWT))
      DO 260 I = 1,N
        IF (RWORK(I+LEWT-1) .LE. ZERO) GO TO 510
 260    RWORK(I+LEWT-1) = ONE/RWORK(I+LEWT-1)
 270  TOLSF = UROUND*DVNORM (N, RWORK(LYH), RWORK(LEWT))
      IF (TOLSF .LE. ONE) GO TO 280
      TOLSF = TOLSF*TWO
      IF (NST .EQ. 0) GO TO 626
      GO TO 520
 280  IF ((TN + H) .NE. TN) GO TO 290
      NHNIL = NHNIL + 1
      IF (NHNIL .GT. MXHNIL) GO TO 290
      MSG = 'DVODE--  Warning..internal T (=R1) and H (=R2) are'
      CALL XERRWD (MSG, 50, 101, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG='      such that in the machine, T + H = T on the next step  '
      CALL XERRWD (MSG, 60, 101, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG = '      (H = step size). solver will continue anyway'
      CALL XERRWD (MSG, 50, 101, 1, 0, 0, 0, 2, TN, H)
      IF (NHNIL .LT. MXHNIL) GO TO 290
      MSG = 'DVODE--  Above warning has been issued I1 times.  '
      CALL XERRWD (MSG, 50, 102, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG = '      it will not be issued again for this problem'
      CALL XERRWD (MSG, 50, 102, 1, 1, MXHNIL, 0, 0, ZERO, ZERO)
 290  CONTINUE
      CALL DVSTEP (Y, RWORK(LYH), NYH, RWORK(LYH), RWORK(LEWT),
     1   RWORK(LSAVF), Y, RWORK(LACOR), RWORK(LWM), IWORK(LIWM),
     2   F, JAC, F, DVNLSD, RPAR, IPAR)
      KGO = 1 - KFLAG
      GO TO (300, 530, 540), KGO
 300  INIT = 1
      KUTH = 0
      GO TO (310, 400, 330, 340, 350), ITASK
 310  IF ((TN - TOUT)*H .LT. ZERO) GO TO 250
      CALL DVINDY (TOUT, 0, RWORK(LYH), NYH, Y, IFLAG)
      T = TOUT
      GO TO 420
 330  IF ((TN - TOUT)*H .GE. ZERO) GO TO 400
      GO TO 250
 340  IF ((TN - TOUT)*H .LT. ZERO) GO TO 345
      CALL DVINDY (TOUT, 0, RWORK(LYH), NYH, Y, IFLAG)
      T = TOUT
      GO TO 420
 345  HMX = ABS(TN) + ABS(H)
      IHIT = ABS(TN - TCRIT) .LE. HUN*UROUND*HMX
      IF (IHIT) GO TO 400
      TNEXT = TN + HNEW*(ONE + FOUR*UROUND)
      IF ((TNEXT - TCRIT)*H .LE. ZERO) GO TO 250
      H = (TCRIT - TN)*(ONE - FOUR*UROUND)
      KUTH = 1
      GO TO 250
 350  HMX = ABS(TN) + ABS(H)
      IHIT = ABS(TN - TCRIT) .LE. HUN*UROUND*HMX
 400  CONTINUE
      CALL DCOPY (N, RWORK(LYH), 1, Y, 1)
      T = TN
      IF (ITASK .NE. 4 .AND. ITASK .NE. 5) GO TO 420
      IF (IHIT) T = TCRIT
 420  ISTATE = 2
      RWORK(11) = HU
      RWORK(12) = HNEW
      RWORK(13) = TN
      IWORK(11) = NST
      IWORK(12) = NFE
      IWORK(13) = NJE
      IWORK(14) = NQU
      IWORK(15) = NEWQ
      IWORK(19) = NLU
      IWORK(20) = NNI
      IWORK(21) = NCFN
      IWORK(22) = NETF
      RETURN
 500  MSG = 'DVODE--  At current T (=R1), MXSTEP (=I1) steps   '
      CALL XERRWD (MSG, 50, 201, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG = '      taken on this call before reaching TOUT     '
      CALL XERRWD (MSG, 50, 201, 1, 1, MXSTEP, 0, 1, TN, ZERO)
      ISTATE = -1
      GO TO 580
 510  EWTI = RWORK(LEWT+I-1)
      MSG = 'DVODE--  At T (=R1), EWT(I1) has become R2 .le. 0.'
      CALL XERRWD (MSG, 50, 202, 1, 1, I, 0, 2, TN, EWTI)
      ISTATE = -6
      GO TO 580
 520  MSG = 'DVODE--  At T (=R1), too much accuracy requested  '
      CALL XERRWD (MSG, 50, 203, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG = '      for precision of machine..  see TOLSF (=R2) '
      CALL XERRWD (MSG, 50, 203, 1, 0, 0, 0, 2, TN, TOLSF)
      RWORK(14) = TOLSF
      ISTATE = -2
      GO TO 580
 530  MSG = 'DVODE--  At T(=R1) and step size H(=R2), the error'
      CALL XERRWD (MSG, 50, 204, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG = '      test failed repeatedly or with abs(H) = HMIN'
      CALL XERRWD (MSG, 50, 204, 1, 0, 0, 0, 2, TN, H)
      ISTATE = -4
      GO TO 560
 540  MSG = 'DVODE--  At T (=R1) and step size H (=R2), the    '
      CALL XERRWD (MSG, 50, 205, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG = '      corrector convergence failed repeatedly     '
      CALL XERRWD (MSG, 50, 205, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG = '      or with abs(H) = HMIN   '
      CALL XERRWD (MSG, 30, 205, 1, 0, 0, 0, 2, TN, H)
      ISTATE = -5
 560  BIG = ZERO
      IMXER = 1
      DO 570 I = 1,N
        SIZE = ABS(RWORK(I+LACOR-1)*RWORK(I+LEWT-1))
        IF (BIG .GE. SIZE) GO TO 570
        BIG = SIZE
        IMXER = I
 570    CONTINUE
      IWORK(16) = IMXER
 580  CONTINUE
      CALL DCOPY (N, RWORK(LYH), 1, Y, 1)
      T = TN
      RWORK(11) = HU
      RWORK(12) = H
      RWORK(13) = TN
      IWORK(11) = NST
      IWORK(12) = NFE
      IWORK(13) = NJE
      IWORK(14) = NQU
      IWORK(15) = NQ
      IWORK(19) = NLU
      IWORK(20) = NNI
      IWORK(21) = NCFN
      IWORK(22) = NETF
      RETURN
 601  MSG = 'DVODE--  ISTATE (=I1) illegal '
      CALL XERRWD (MSG, 30, 1, 1, 1, ISTATE, 0, 0, ZERO, ZERO)
      IF (ISTATE .LT. 0) GO TO 800
      GO TO 700
 602  MSG = 'DVODE--  ITASK (=I1) illegal  '
      CALL XERRWD (MSG, 30, 2, 1, 1, ITASK, 0, 0, ZERO, ZERO)
      GO TO 700
 603  MSG='DVODE--  ISTATE (=I1) .gt. 1 but DVODE not initialized      '
      CALL XERRWD (MSG, 60, 3, 1, 1, ISTATE, 0, 0, ZERO, ZERO)
      GO TO 700
 604  MSG = 'DVODE--  NEQ (=I1) .lt. 1     '
      CALL XERRWD (MSG, 30, 4, 1, 1, NEQ, 0, 0, ZERO, ZERO)
      GO TO 700
 605  MSG = 'DVODE--  ISTATE = 3 and NEQ increased (I1 to I2)  '
      CALL XERRWD (MSG, 50, 5, 1, 2, N, NEQ, 0, ZERO, ZERO)
      GO TO 700
 606  MSG = 'DVODE--  ITOL (=I1) illegal   '
      CALL XERRWD (MSG, 30, 6, 1, 1, ITOL, 0, 0, ZERO, ZERO)
      GO TO 700
 607  MSG = 'DVODE--  IOPT (=I1) illegal   '
      CALL XERRWD (MSG, 30, 7, 1, 1, IOPT, 0, 0, ZERO, ZERO)
      GO TO 700
 608  MSG = 'DVODE--  MF (=I1) illegal     '
      CALL XERRWD (MSG, 30, 8, 1, 1, MF, 0, 0, ZERO, ZERO)
      GO TO 700
 609  MSG = 'DVODE--  ML (=I1) illegal.. .lt.0 or .ge.NEQ (=I2)'
      CALL XERRWD (MSG, 50, 9, 1, 2, ML, NEQ, 0, ZERO, ZERO)
      GO TO 700
 610  MSG = 'DVODE--  MU (=I1) illegal.. .lt.0 or .ge.NEQ (=I2)'
      CALL XERRWD (MSG, 50, 10, 1, 2, MU, NEQ, 0, ZERO, ZERO)
      GO TO 700
 611  MSG = 'DVODE--  MAXORD (=I1) .lt. 0  '
      CALL XERRWD (MSG, 30, 11, 1, 1, MAXORD, 0, 0, ZERO, ZERO)
      GO TO 700
 612  MSG = 'DVODE--  MXSTEP (=I1) .lt. 0  '
      CALL XERRWD (MSG, 30, 12, 1, 1, MXSTEP, 0, 0, ZERO, ZERO)
      GO TO 700
 613  MSG = 'DVODE--  MXHNIL (=I1) .lt. 0  '
      CALL XERRWD (MSG, 30, 13, 1, 1, MXHNIL, 0, 0, ZERO, ZERO)
      GO TO 700
 614  MSG = 'DVODE--  TOUT (=R1) behind T (=R2)      '
      CALL XERRWD (MSG, 40, 14, 1, 0, 0, 0, 2, TOUT, T)
      MSG = '      integration direction is given by H0 (=R1)  '
      CALL XERRWD (MSG, 50, 14, 1, 0, 0, 0, 1, H0, ZERO)
      GO TO 700
 615  MSG = 'DVODE--  HMAX (=R1) .lt. 0.0  '
      CALL XERRWD (MSG, 30, 15, 1, 0, 0, 0, 1, HMAX, ZERO)
      GO TO 700
 616  MSG = 'DVODE--  HMIN (=R1) .lt. 0.0  '
      CALL XERRWD (MSG, 30, 16, 1, 0, 0, 0, 1, HMIN, ZERO)
      GO TO 700
 617  CONTINUE
      MSG='DVODE--  RWORK length needed, LENRW (=I1), exceeds LRW (=I2)'
      CALL XERRWD (MSG, 60, 17, 1, 2, LENRW, LRW, 0, ZERO, ZERO)
      GO TO 700
 618  CONTINUE
      MSG='DVODE--  IWORK length needed, LENIW (=I1), exceeds LIW (=I2)'
      CALL XERRWD (MSG, 60, 18, 1, 2, LENIW, LIW, 0, ZERO, ZERO)
      GO TO 700
 619  MSG = 'DVODE--  RTOL is R1 .lt. 0.0        '
      CALL XERRWD (MSG, 40, 19, 1, 1, I, 0, 1, RTOLI, ZERO)
      GO TO 700
 620  MSG = 'DVODE--  ATOL(I1) is R1 .lt. 0.0        '
      CALL XERRWD (MSG, 40, 20, 1, 1, I, 0, 1, ATOLI, ZERO)
      GO TO 700
 621  EWTI = RWORK(LEWT+I-1)
      MSG = 'DVODE--  EWT(I1) is R1 .le. 0.0         '
      CALL XERRWD (MSG, 40, 21, 1, 1, I, 0, 1, EWTI, ZERO)
      GO TO 700
 622  CONTINUE
      MSG='DVODE--  TOUT (=R1) too close to T(=R2) to start integration'
      CALL XERRWD (MSG, 60, 22, 1, 0, 0, 0, 2, TOUT, T)
      GO TO 700
 623  CONTINUE
      MSG='DVODE--  ITASK = I1 and TOUT (=R1) behind TCUR - HU (= R2)  '
      CALL XERRWD (MSG, 60, 23, 1, 1, ITASK, 0, 2, TOUT, TP)
      GO TO 700
 624  CONTINUE
      MSG='DVODE--  ITASK = 4 or 5 and TCRIT (=R1) behind TCUR (=R2)   '
      CALL XERRWD (MSG, 60, 24, 1, 0, 0, 0, 2, TCRIT, TN)
      GO TO 700
 625  CONTINUE
      MSG='DVODE--  ITASK = 4 or 5 and TCRIT (=R1) behind TOUT (=R2)   '
      CALL XERRWD (MSG, 60, 25, 1, 0, 0, 0, 2, TCRIT, TOUT)
      GO TO 700
 626  MSG = 'DVODE--  At start of problem, too much accuracy   '
      CALL XERRWD (MSG, 50, 26, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG='      requested for precision of machine..  see TOLSF (=R1) '
      CALL XERRWD (MSG, 60, 26, 1, 0, 0, 0, 1, TOLSF, ZERO)
      RWORK(14) = TOLSF
      GO TO 700
 627  MSG='DVODE--  Trouble from DVINDY.  ITASK = I1, TOUT = R1.       '
      CALL XERRWD (MSG, 60, 27, 1, 1, ITASK, 0, 1, TOUT, ZERO)
 700  CONTINUE
      ISTATE = -3
      RETURN
 800  MSG = 'DVODE--  Run aborted.. apparent infinite loop     '
      CALL XERRWD (MSG, 50, 303, 2, 0, 0, 0, 0, ZERO, ZERO)
      RETURN
      END
      SUBROUTINE DVHIN (N, T0, Y0, YDOT, F, RPAR, IPAR, TOUT, UROUND,
     1   EWT, ITOL, ATOL, Y, TEMP, H0, NITER, IER)
      EXTERNAL F
      DOUBLE PRECISION T0, Y0, YDOT, RPAR, TOUT, UROUND, EWT, ATOL, Y,
     1   TEMP, H0
      INTEGER N, IPAR, ITOL, NITER, IER
      DIMENSION Y0(*), YDOT(*), EWT(*), ATOL(*), Y(*),
     1   TEMP(*)
      DOUBLE PRECISION AFI, ATOLI, DELYI, HALF, HG, HLB, HNEW, HRAT,
     1     HUB, HUN, PT1, T1, TDIST, TROUND, TWO, YDDNRM
      INTEGER I, ITER
      DOUBLE PRECISION DVNORM
      SAVE HALF, HUN, PT1, TWO
      DATA HALF /0.5D0/, HUN /100.0D0/, PT1 /0.1D0/, TWO /2.0D0/
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp Threadprivate(HALF, HUN, PT1, TWO)
      NITER = 0
      TDIST = ABS(TOUT - T0)
      TROUND = UROUND*MAX(ABS(T0),ABS(TOUT))
      IF (TDIST .LT. TWO*TROUND) GO TO 100
      HLB = HUN*TROUND
      HUB = PT1*TDIST
      ATOLI = ATOL(1)
      DO 10 I = 1, N
        IF (ITOL .EQ. 2 .OR. ITOL .EQ. 4) ATOLI = ATOL(I)
        DELYI = PT1*ABS(Y0(I)) + ATOLI
        AFI = ABS(YDOT(I))
        IF (AFI*HUB .GT. DELYI) HUB = DELYI/AFI
 10     CONTINUE
      ITER = 0
      HG = SQRT(HLB*HUB)
      IF (HUB .LT. HLB) THEN
        H0 = HG
        GO TO 90
      ENDIF
 50   CONTINUE
      T1 = T0 + HG
      DO 60 I = 1, N
 60     Y(I) = Y0(I) + HG*YDOT(I)
      CALL F (N, T1, Y, TEMP, RPAR, IPAR)
      DO 70 I = 1, N
 70     TEMP(I) = (TEMP(I) - YDOT(I))/HG
      YDDNRM = DVNORM (N, TEMP, EWT)
      IF (YDDNRM*HUB*HUB .GT. TWO) THEN
        HNEW = SQRT(TWO/YDDNRM)
      ELSE
        HNEW = SQRT(HG*HUB)
      ENDIF
      ITER = ITER + 1
      IF (ITER .GE. 4) GO TO 80
      HRAT = HNEW/HG
      IF ( (HRAT .GT. HALF) .AND. (HRAT .LT. TWO) ) GO TO 80
      IF ( (ITER .GE. 2) .AND. (HNEW .GT. TWO*HG) ) THEN
        HNEW = HG
        GO TO 80
      ENDIF
      HG = HNEW
      GO TO 50
 80   H0 = HNEW*HALF
      IF (H0 .LT. HLB) H0 = HLB
      IF (H0 .GT. HUB) H0 = HUB
 90   H0 = SIGN(H0, TOUT - T0)
      NITER = ITER
      IER = 0
      RETURN
 100  IER = -1
      RETURN
      END
      SUBROUTINE DVINDY (T, K, YH, LDYH, DKY, IFLAG)
      DOUBLE PRECISION T, YH, DKY
      INTEGER K, LDYH, IFLAG
      DIMENSION YH(LDYH,*), DKY(*)
      DOUBLE PRECISION ACNRM, CCMXJ, CONP, CRATE, DRC, EL,
     1     ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2     RC, RL1, TAU, TQ, TN, UROUND
      INTEGER ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     1        L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     2        LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     3        N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     4        NSLP, NYH
      DOUBLE PRECISION HU
      INTEGER NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
      DOUBLE PRECISION C, HUN, R, S, TFUZZ, TN1, TP, ZERO
      INTEGER I, IC, J, JB, JB2, JJ, JJ1, JP1
      CHARACTER*80 MSG
      SAVE HUN, ZERO
      COMMON /DVOD01/ ACNRM, CCMXJ, CONP, CRATE, DRC, EL(13),
     1                ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2                RC, RL1, TAU(13), TQ(5), TN, UROUND,
     3                ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     4                L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     5                LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     6                N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     7                NSLP, NYH
      COMMON /DVOD02/ HU, NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp Threadprivate(/DVOD01/,/DVOD02/,HUN,ZERO)
      DATA HUN /100.0D0/, ZERO /0.0D0/
      IFLAG = 0
      IF (K .LT. 0 .OR. K .GT. NQ) GO TO 80
      TFUZZ = HUN*UROUND*(TN + HU)
      TP = TN - HU - TFUZZ
      TN1 = TN + TFUZZ
      IF ((T-TP)*(T-TN1) .GT. ZERO) GO TO 90
      S = (T - TN)/H
      IC = 1
      IF (K .EQ. 0) GO TO 15
      JJ1 = L - K
      DO 10 JJ = JJ1, NQ
 10     IC = IC*JJ
 15   C = REAL(IC)
      DO 20 I = 1, N
 20     DKY(I) = C*YH(I,L)
      IF (K .EQ. NQ) GO TO 55
      JB2 = NQ - K
      DO 50 JB = 1, JB2
        J = NQ - JB
        JP1 = J + 1
        IC = 1
        IF (K .EQ. 0) GO TO 35
        JJ1 = JP1 - K
        DO 30 JJ = JJ1, J
 30       IC = IC*JJ
 35     C = REAL(IC)
        DO 40 I = 1, N
 40       DKY(I) = C*YH(I,JP1) + S*DKY(I)
 50     CONTINUE
      IF (K .EQ. 0) RETURN
 55   R = H**(-K)
      CALL DSCAL (N, R, DKY, 1)
      RETURN
 80   MSG = 'DVINDY-- K (=I1) illegal      '
      CALL XERRWD (MSG, 30, 51, 1, 1, K, 0, 0, ZERO, ZERO)
      IFLAG = -1
      RETURN
 90   MSG = 'DVINDY-- T (=R1) illegal      '
      CALL XERRWD (MSG, 30, 52, 1, 0, 0, 0, 1, T, ZERO)
      MSG='      T not in interval TCUR - HU (= R1) to TCUR (=R2)      '
      CALL XERRWD (MSG, 60, 52, 1, 0, 0, 0, 2, TP, TN)
      IFLAG = -2
      RETURN
      END
      SUBROUTINE DVSTEP (Y, YH, LDYH, YH1, EWT, SAVF, VSAV, ACOR,
     1                  WM, IWM, F, JAC, PSOL, VNLS, RPAR, IPAR)
      EXTERNAL F, JAC, PSOL, VNLS
      DOUBLE PRECISION Y, YH, YH1, EWT, SAVF, VSAV, ACOR, WM, RPAR
      INTEGER LDYH, IWM, IPAR
      DIMENSION Y(*), YH(LDYH,*), YH1(*), EWT(*), SAVF(*), VSAV(*),
     1   ACOR(*), WM(*), IWM(*)
      DOUBLE PRECISION ACNRM, CCMXJ, CONP, CRATE, DRC, EL,
     1     ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2     RC, RL1, TAU, TQ, TN, UROUND
      INTEGER ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     1        L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     2        LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     3        N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     4        NSLP, NYH
      DOUBLE PRECISION HU
      INTEGER NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
      DOUBLE PRECISION ADDON, BIAS1,BIAS2,BIAS3, CNQUOT, DDN, DSM, DUP,
     1     ETACF, ETAMIN, ETAMX1, ETAMX2, ETAMX3, ETAMXF,
     2     ETAQ, ETAQM1, ETAQP1, FLOTL, ONE, ONEPSM,
     3     R, THRESH, TOLD, ZERO
      INTEGER I, I1, I2, IBACK, J, JB, KFC, KFH, MXNCF, NCF, NFLAG
      DOUBLE PRECISION DVNORM
      SAVE ADDON, BIAS1, BIAS2, BIAS3,
     1     ETACF, ETAMIN, ETAMX1, ETAMX2, ETAMX3, ETAMXF,
     2     KFC, KFH, MXNCF, ONEPSM, THRESH, ONE, ZERO
      COMMON /DVOD01/ ACNRM, CCMXJ, CONP, CRATE, DRC, EL(13),
     1                ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2                RC, RL1, TAU(13), TQ(5), TN, UROUND,
     3                ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     4                L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     5                LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     6                N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     7                NSLP, NYH
      COMMON /DVOD02/ HU, NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp Threadprivate(/DVOD01/,/DVOD02/,ADDON,BIAS1,BIAS2,BIAS3,ETACF,
!$omp&ETAMIN,ETAMX1,ETAMX2,ETAMX3,ETAMXF,KFC,KFH,MXNCF,ONEPSM,THRESH,
!$omp&ONE,ZERO)
      DATA KFC/-3/, KFH/-7/, MXNCF/10/
      DATA ADDON  /1.0D-6/,    BIAS1  /6.0D0/,     BIAS2  /6.0D0/,
     1     BIAS3  /10.0D0/,    ETACF  /0.25D0/,    ETAMIN /0.1D0/,
     2     ETAMXF /0.2D0/,     ETAMX1 /1.0D4/,     ETAMX2 /10.0D0/,
     3     ETAMX3 /10.0D0/,    ONEPSM /1.00001D0/, THRESH /1.5D0/
      DATA ONE/1.0D0/, ZERO/0.0D0/
      KFLAG = 0
      TOLD = TN
      NCF = 0
      JCUR = 0
      NFLAG = 0
      IF (JSTART .GT. 0) GO TO 20
      IF (JSTART .EQ. -1) GO TO 100
      LMAX = MAXORD + 1
      NQ = 1
      L = 2
      NQNYH = NQ*LDYH
      TAU(1) = H
      PRL1 = ONE
      RC = ZERO
      ETAMAX = ETAMX1
      NQWAIT = 2
      HSCAL = H
      GO TO 200
 20   CONTINUE
      IF (KUTH .EQ. 1) THEN
        ETA = MIN(ETA,H/HSCAL)
        NEWH = 1
        ENDIF
 50   IF (NEWH .EQ. 0) GO TO 200
      IF (NEWQ .EQ. NQ) GO TO 150
      IF (NEWQ .LT. NQ) THEN
        CALL DVJUST (YH, LDYH, -1)
        NQ = NEWQ
        L = NQ + 1
        NQWAIT = L
        GO TO 150
        ENDIF
      IF (NEWQ .GT. NQ) THEN
        CALL DVJUST (YH, LDYH, 1)
        NQ = NEWQ
        L = NQ + 1
        NQWAIT = L
        GO TO 150
      ENDIF
 100  CONTINUE
      LMAX = MAXORD + 1
      IF (N .EQ. LDYH) GO TO 120
      I1 = 1 + (NEWQ + 1)*LDYH
      I2 = (MAXORD + 1)*LDYH
      IF (I1 .GT. I2) GO TO 120
      DO 110 I = I1, I2
 110    YH1(I) = ZERO
 120  IF (NEWQ .LE. MAXORD) GO TO 140
      FLOTL = REAL(LMAX)
      IF (MAXORD .LT. NQ-1) THEN
        DDN = DVNORM (N, SAVF, EWT)/TQ(1)
        ETA = ONE/((BIAS1*DDN)**(ONE/FLOTL) + ADDON)
        ENDIF
      IF (MAXORD .EQ. NQ .AND. NEWQ .EQ. NQ+1) ETA = ETAQ
      IF (MAXORD .EQ. NQ-1 .AND. NEWQ .EQ. NQ+1) THEN
        ETA = ETAQM1
        CALL DVJUST (YH, LDYH, -1)
        ENDIF
      IF (MAXORD .EQ. NQ-1 .AND. NEWQ .EQ. NQ) THEN
        DDN = DVNORM (N, SAVF, EWT)/TQ(1)
        ETA = ONE/((BIAS1*DDN)**(ONE/FLOTL) + ADDON)
        CALL DVJUST (YH, LDYH, -1)
        ENDIF
      ETA = MIN(ETA,ONE)
      NQ = MAXORD
      L = LMAX
 140  IF (KUTH .EQ. 1) ETA = MIN(ETA,ABS(H/HSCAL))
      IF (KUTH .EQ. 0) ETA = MAX(ETA,HMIN/ABS(HSCAL))
      ETA = ETA/MAX(ONE,ABS(HSCAL)*HMXI*ETA)
      NEWH = 1
      NQWAIT = L
      IF (NEWQ .LE. MAXORD) GO TO 50
 150  R = ONE
      DO 180 J = 2, L
        R = R*ETA
        CALL DSCAL (N, R, YH(1,J), 1 )
 180    CONTINUE
      H = HSCAL*ETA
      HSCAL = H
      RC = RC*ETA
      NQNYH = NQ*LDYH
 200  TN = TN + H
      I1 = NQNYH + 1
      DO 220 JB = 1, NQ
        I1 = I1 - LDYH
        DO 210 I = I1, NQNYH
 210      YH1(I) = YH1(I) + YH1(I+LDYH)
 220  CONTINUE
      CALL DVSET
      RL1 = ONE/EL(2)
      RC = RC*(RL1/PRL1)
      PRL1 = RL1
      CALL VNLS (Y, YH, LDYH, VSAV, SAVF, EWT, ACOR, IWM, WM,
     1           F, JAC, PSOL, NFLAG, RPAR, IPAR)
      IF (NFLAG .EQ. 0) GO TO 450
        NCF = NCF + 1
        NCFN = NCFN + 1
        ETAMAX = ONE
        TN = TOLD
        I1 = NQNYH + 1
        DO 430 JB = 1, NQ
          I1 = I1 - LDYH
          DO 420 I = I1, NQNYH
 420        YH1(I) = YH1(I) - YH1(I+LDYH)
 430      CONTINUE
        IF (NFLAG .LT. -1) GO TO 680
        IF (ABS(H) .LE. HMIN*ONEPSM) GO TO 670
        IF (NCF .EQ. MXNCF) GO TO 670
        ETA = ETACF
        ETA = MAX(ETA,HMIN/ABS(H))
        NFLAG = -1
        GO TO 150
 450  CONTINUE
      DSM = ACNRM/TQ(2)
      IF (DSM .GT. ONE) GO TO 500
      KFLAG = 0
      NST = NST + 1
      HU = H
      NQU = NQ
      DO 470 IBACK = 1, NQ
        I = L - IBACK
 470    TAU(I+1) = TAU(I)
      TAU(1) = H
      DO 480 J = 1, L
        CALL DAXPY (N, EL(J), ACOR, 1, YH(1,J), 1 )
 480    CONTINUE
      NQWAIT = NQWAIT - 1
      IF ((L .EQ. LMAX) .OR. (NQWAIT .NE. 1)) GO TO 490
      CALL DCOPY (N, ACOR, 1, YH(1,LMAX), 1 )
      CONP = TQ(5)
 490  IF (ETAMAX .NE. ONE) GO TO 560
      IF (NQWAIT .LT. 2) NQWAIT = 2
      NEWQ = NQ
      NEWH = 0
      ETA = ONE
      HNEW = H
      GO TO 690
 500  KFLAG = KFLAG - 1
      NETF = NETF + 1
      NFLAG = -2
      TN = TOLD
      I1 = NQNYH + 1
      DO 520 JB = 1, NQ
        I1 = I1 - LDYH
        DO 510 I = I1, NQNYH
 510      YH1(I) = YH1(I) - YH1(I+LDYH)
 520  CONTINUE
      IF (ABS(H) .LE. HMIN*ONEPSM) GO TO 660
      ETAMAX = ONE
      IF (KFLAG .LE. KFC) GO TO 530
      FLOTL = REAL(L)
      ETA = ONE/((BIAS2*DSM)**(ONE/FLOTL) + ADDON)
      ETA = MAX(ETA,HMIN/ABS(H),ETAMIN)
      IF ((KFLAG .LE. -2) .AND. (ETA .GT. ETAMXF)) ETA = ETAMXF
      GO TO 150
 530  IF (KFLAG .EQ. KFH) GO TO 660
      IF (NQ .EQ. 1) GO TO 540
      ETA = MAX(ETAMIN,HMIN/ABS(H))
      CALL DVJUST (YH, LDYH, -1)
      L = NQ
      NQ = NQ - 1
      NQWAIT = L
      GO TO 150
 540  ETA = MAX(ETAMIN,HMIN/ABS(H))
      H = H*ETA
      HSCAL = H
      TAU(1) = H
      CALL F (N, TN, Y, SAVF, RPAR, IPAR)
      NFE = NFE + 1
      DO 550 I = 1, N
 550    YH(I,2) = H*SAVF(I)
      NQWAIT = 10
      GO TO 200
 560  FLOTL = REAL(L)
      ETAQ = ONE/((BIAS2*DSM)**(ONE/FLOTL) + ADDON)
      IF (NQWAIT .NE. 0) GO TO 600
      NQWAIT = 2
      ETAQM1 = ZERO
      IF (NQ .EQ. 1) GO TO 570
      DDN = DVNORM (N, YH(1,L), EWT)/TQ(1)
      ETAQM1 = ONE/((BIAS1*DDN)**(ONE/(FLOTL - ONE)) + ADDON)
 570  ETAQP1 = ZERO
      IF (L .EQ. LMAX) GO TO 580
      CNQUOT = (TQ(5)/CONP)*(H/TAU(2))**L
      DO 575 I = 1, N
 575    SAVF(I) = ACOR(I) - CNQUOT*YH(I,LMAX)
      DUP = DVNORM (N, SAVF, EWT)/TQ(3)
      ETAQP1 = ONE/((BIAS3*DUP)**(ONE/(FLOTL + ONE)) + ADDON)
 580  IF (ETAQ .GE. ETAQP1) GO TO 590
      IF (ETAQP1 .GT. ETAQM1) GO TO 620
      GO TO 610
 590  IF (ETAQ .LT. ETAQM1) GO TO 610
 600  ETA = ETAQ
      NEWQ = NQ
      GO TO 630
 610  ETA = ETAQM1
      NEWQ = NQ - 1
      GO TO 630
 620  ETA = ETAQP1
      NEWQ = NQ + 1
      CALL DCOPY (N, ACOR, 1, YH(1,LMAX), 1)
 630  IF (ETA .LT. THRESH .OR. ETAMAX .EQ. ONE) GO TO 640
      ETA = MIN(ETA,ETAMAX)
      ETA = ETA/MAX(ONE,ABS(H)*HMXI*ETA)
      NEWH = 1
      HNEW = H*ETA
      GO TO 690
 640  NEWQ = NQ
      NEWH = 0
      ETA = ONE
      HNEW = H
      GO TO 690
 660  KFLAG = -1
      GO TO 720
 670  KFLAG = -2
      GO TO 720
 680  IF (NFLAG .EQ. -2) KFLAG = -3
      IF (NFLAG .EQ. -3) KFLAG = -4
      GO TO 720
 690  ETAMAX = ETAMX3
      IF (NST .LE. 10) ETAMAX = ETAMX2
 700  R = ONE/TQ(2)
      CALL DSCAL (N, R, ACOR, 1)
 720  JSTART = 1
      RETURN
      END
      SUBROUTINE DVSET
      DOUBLE PRECISION ACNRM, CCMXJ, CONP, CRATE, DRC, EL,
     1     ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2     RC, RL1, TAU, TQ, TN, UROUND
      INTEGER ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     1        L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     2        LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     3        N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     4        NSLP, NYH
      DOUBLE PRECISION AHATN0, ALPH0, CNQM1, CORTES, CSUM, ELP, EM,
     1     EM0, FLOTI, FLOTL, FLOTNQ, HSUM, ONE, RXI, RXIS, S, SIX,
     2     T1, T2, T3, T4, T5, T6, TWO, XI, ZERO
      INTEGER I, IBACK, J, JP1, NQM1, NQM2
      DIMENSION EM(13)
      SAVE CORTES, ONE, SIX, TWO, ZERO
      COMMON /DVOD01/ ACNRM, CCMXJ, CONP, CRATE, DRC, EL(13),
     1                ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2                RC, RL1, TAU(13), TQ(5), TN, UROUND,
     3                ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     4                L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     5                LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     6                N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     7                NSLP, NYH
      DATA CORTES /0.1D0/
      DATA ONE  /1.0D0/, SIX /6.0D0/, TWO /2.0D0/, ZERO /0.0D0/
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp Threadprivate(/DVOD01/,CORTES, ONE, SIX, TWO, ZERO)
      FLOTL = REAL(L)
      NQM1 = NQ - 1
      NQM2 = NQ - 2
      GO TO (100, 200), METH
 100  IF (NQ .NE. 1) GO TO 110
      EL(1) = ONE
      EL(2) = ONE
      TQ(1) = ONE
      TQ(2) = TWO
      TQ(3) = SIX*TQ(2)
      TQ(5) = ONE
      GO TO 300
 110  HSUM = H
      EM(1) = ONE
      FLOTNQ = FLOTL - ONE
      DO 115 I = 2, L
 115    EM(I) = ZERO
      DO 150 J = 1, NQM1
        IF ((J .NE. NQM1) .OR. (NQWAIT .NE. 1)) GO TO 130
        S = ONE
        CSUM = ZERO
        DO 120 I = 1, NQM1
          CSUM = CSUM + S*EM(I)/REAL(I+1)
 120      S = -S
        TQ(1) = EM(NQM1)/(FLOTNQ*CSUM)
 130    RXI = H/HSUM
        DO 140 IBACK = 1, J
          I = (J + 2) - IBACK
 140      EM(I) = EM(I) + EM(I-1)*RXI
        HSUM = HSUM + TAU(J)
 150    CONTINUE
      S = ONE
      EM0 = ZERO
      CSUM = ZERO
      DO 160 I = 1, NQ
        FLOTI = REAL(I)
        EM0 = EM0 + S*EM(I)/FLOTI
        CSUM = CSUM + S*EM(I)/(FLOTI+ONE)
 160    S = -S
      S = ONE/EM0
      EL(1) = ONE
      DO 170 I = 1, NQ
 170    EL(I+1) = S*EM(I)/REAL(I)
      XI = HSUM/H
      TQ(2) = XI*EM0/CSUM
      TQ(5) = XI/EL(L)
      IF (NQWAIT .NE. 1) GO TO 300
      RXI = ONE/XI
      DO 180 IBACK = 1, NQ
        I = (L + 1) - IBACK
 180    EM(I) = EM(I) + EM(I-1)*RXI
      S = ONE
      CSUM = ZERO
      DO 190 I = 1, L
        CSUM = CSUM + S*EM(I)/REAL(I+1)
 190    S = -S
      TQ(3) = FLOTL*EM0/CSUM
      GO TO 300
 200  DO 210 I = 3, L
 210    EL(I) = ZERO
      EL(1) = ONE
      EL(2) = ONE
      ALPH0 = -ONE
      AHATN0 = -ONE
      HSUM = H
      RXI = ONE
      RXIS = ONE
      IF (NQ .EQ. 1) GO TO 240
      DO 230 J = 1, NQM2
        HSUM = HSUM + TAU(J)
        RXI = H/HSUM
        JP1 = J + 1
        ALPH0 = ALPH0 - ONE/REAL(JP1)
        DO 220 IBACK = 1, JP1
          I = (J + 3) - IBACK
 220      EL(I) = EL(I) + EL(I-1)*RXI
 230    CONTINUE
      ALPH0 = ALPH0 - ONE/REAL(NQ)
      RXIS = -EL(2) - ALPH0
      HSUM = HSUM + TAU(NQM1)
      RXI = H/HSUM
      AHATN0 = -EL(2) - RXI
      DO 235 IBACK = 1, NQ
        I = (NQ + 2) - IBACK
 235    EL(I) = EL(I) + EL(I-1)*RXIS
 240  T1 = ONE - AHATN0 + ALPH0
      T2 = ONE + REAL(NQ)*T1
      TQ(2) = ABS(ALPH0*T2/T1)
      TQ(5) = ABS(T2/(EL(L)*RXI/RXIS))
      IF (NQWAIT .NE. 1) GO TO 300
      CNQM1 = RXIS/EL(L)
      T3 = ALPH0 + ONE/REAL(NQ)
      T4 = AHATN0 + RXI
      ELP = T3/(ONE - T4 + T3)
      TQ(1) = ABS(ELP/CNQM1)
      HSUM = HSUM + TAU(NQ)
      RXI = H/HSUM
      T5 = ALPH0 - ONE/REAL(NQ+1)
      T6 = AHATN0 - RXI
      ELP = T2/(ONE - T6 + T5)
      TQ(3) = ABS(ELP*RXI*(FLOTL + ONE)*T5)
 300  TQ(4) = CORTES*TQ(2)
      RETURN
      END
      SUBROUTINE DVJUST (YH, LDYH, IORD)
      DOUBLE PRECISION YH
      INTEGER LDYH, IORD
      DIMENSION YH(LDYH,*)
      DOUBLE PRECISION ACNRM, CCMXJ, CONP, CRATE, DRC, EL,
     1     ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2     RC, RL1, TAU, TQ, TN, UROUND
      INTEGER ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     1        L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     2        LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     3        N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     4        NSLP, NYH
      DOUBLE PRECISION ALPH0, ALPH1, HSUM, ONE, PROD, T1, XI,XIOLD, ZERO
      INTEGER I, IBACK, J, JP1, LP1, NQM1, NQM2, NQP1
      SAVE ONE, ZERO
      COMMON /DVOD01/ ACNRM, CCMXJ, CONP, CRATE, DRC, EL(13),
     1                ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2                RC, RL1, TAU(13), TQ(5), TN, UROUND,
     3                ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     4                L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     5                LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     6                N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     7                NSLP, NYH
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp Threadprivate(/DVOD01/,One,Zero)
      DATA ONE /1.0D0/, ZERO /0.0D0/
      IF ((NQ .EQ. 2) .AND. (IORD .NE. 1)) RETURN
      NQM1 = NQ - 1
      NQM2 = NQ - 2
      GO TO (100, 200), METH
 100  CONTINUE
      IF (IORD .EQ. 1) GO TO 180
      DO 110 J = 1, LMAX
 110    EL(J) = ZERO
      EL(2) = ONE
      HSUM = ZERO
      DO 130 J = 1, NQM2
        HSUM = HSUM + TAU(J)
        XI = HSUM/HSCAL
        JP1 = J + 1
        DO 120 IBACK = 1, JP1
          I = (J + 3) - IBACK
 120      EL(I) = EL(I)*XI + EL(I-1)
 130    CONTINUE
      DO 140 J = 2, NQM1
 140    EL(J+1) = REAL(NQ)*EL(J)/REAL(J)
      DO 170 J = 3, NQ
        DO 160 I = 1, N
 160      YH(I,J) = YH(I,J) - YH(I,L)*EL(J)
 170    CONTINUE
      RETURN
 180  CONTINUE
      LP1 = L + 1
      DO 190 I = 1, N
 190    YH(I,LP1) = ZERO
      RETURN
 200  CONTINUE
      IF (IORD .EQ. 1) GO TO 300
      DO 210 J = 1, LMAX
 210    EL(J) = ZERO
      EL(3) = ONE
      HSUM = ZERO
      DO 230 J = 1,NQM2
        HSUM = HSUM + TAU(J)
        XI = HSUM/HSCAL
        JP1 = J + 1
        DO 220 IBACK = 1, JP1
          I = (J + 4) - IBACK
 220      EL(I) = EL(I)*XI + EL(I-1)
 230    CONTINUE
      DO 250 J = 3,NQ
        DO 240 I = 1, N
 240      YH(I,J) = YH(I,J) - YH(I,L)*EL(J)
 250    CONTINUE
      RETURN
 300  DO 310 J = 1, LMAX
 310    EL(J) = ZERO
      EL(3) = ONE
      ALPH0 = -ONE
      ALPH1 = ONE
      PROD = ONE
      XIOLD = ONE
      HSUM = HSCAL
      IF (NQ .EQ. 1) GO TO 340
      DO 330 J = 1, NQM1
        JP1 = J + 1
        HSUM = HSUM + TAU(JP1)
        XI = HSUM/HSCAL
        PROD = PROD*XI
        ALPH0 = ALPH0 - ONE/REAL(JP1)
        ALPH1 = ALPH1 + ONE/XI
        DO 320 IBACK = 1, JP1
          I = (J + 4) - IBACK
 320      EL(I) = EL(I)*XIOLD + EL(I-1)
        XIOLD = XI
 330    CONTINUE
 340  CONTINUE
      T1 = (-ALPH0 - ALPH1)/PROD
      LP1 = L + 1
      DO 350 I = 1, N
 350    YH(I,LP1) = T1*YH(I,LMAX)
      NQP1 = NQ + 1
      DO 370 J = 3, NQP1
        CALL DAXPY (N, EL(J), YH(1,LP1), 1, YH(1,J), 1 )
 370  CONTINUE
      RETURN
      END
      SUBROUTINE DVNLSD (Y, YH, LDYH, VSAV, SAVF, EWT, ACOR, IWM, WM,
     1                 F, JAC, PDUM, NFLAG, RPAR, IPAR)
      EXTERNAL F, JAC, PDUM
      DOUBLE PRECISION Y, YH, VSAV, SAVF, EWT, ACOR, WM, RPAR
      INTEGER LDYH, IWM, NFLAG, IPAR
      DIMENSION Y(*), YH(LDYH,*), VSAV(*), SAVF(*), EWT(*), ACOR(*),
     1          IWM(*), WM(*)
      DOUBLE PRECISION ACNRM, CCMXJ, CONP, CRATE, DRC, EL,
     1     ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2     RC, RL1, TAU, TQ, TN, UROUND
      INTEGER ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     1        L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     2        LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     3        N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     4        NSLP, NYH
      DOUBLE PRECISION HU
      INTEGER NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
      DOUBLE PRECISION CCMAX, CRDOWN, CSCALE, DCON, DEL, DELP, ONE,
     1     RDIV, TWO, ZERO
      INTEGER I, IERPJ, IERSL, M, MAXCOR, MSBP
      DOUBLE PRECISION DVNORM
      SAVE CCMAX, CRDOWN, MAXCOR, MSBP, RDIV, ONE, TWO, ZERO
      COMMON /DVOD01/ ACNRM, CCMXJ, CONP, CRATE, DRC, EL(13),
     1                ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2                RC, RL1, TAU(13), TQ(5), TN, UROUND,
     3                ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     4                L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     5                LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     6                N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     7                NSLP, NYH
      COMMON /DVOD02/ HU, NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp Threadprivate(/DVOD01/,/DVOD02/,CCMAX,CRDOWN,MAXCOR,MSBP,RDIV,
!$omp&ONE,TWO,ZERO)
      DATA CCMAX /0.3D0/, CRDOWN /0.3D0/, MAXCOR /3/, MSBP /20/,
     1     RDIV  /2.0D0/
      DATA ONE /1.0D0/, TWO /2.0D0/, ZERO /0.0D0/
      IF (JSTART .EQ. 0) NSLP = 0
      IF (NFLAG .EQ. 0) ICF = 0
      IF (NFLAG .EQ. -2) IPUP = MITER
      IF ( (JSTART .EQ. 0) .OR. (JSTART .EQ. -1) ) IPUP = MITER
      IF (MITER .EQ. 0) THEN
        CRATE = ONE
        GO TO 220
      ENDIF
      DRC = ABS(RC-ONE)
      IF (DRC .GT. CCMAX .OR. NST .GE. NSLP+MSBP) IPUP = MITER
 220  M = 0
      DELP = ZERO
      CALL DCOPY (N, YH(1,1), 1, Y, 1 )
      CALL F (N, TN, Y, SAVF, RPAR, IPAR)
      NFE = NFE + 1
      IF (IPUP .LE. 0) GO TO 250
      CALL DVJAC (Y, YH, LDYH, EWT, ACOR, SAVF, WM, IWM, F, JAC, IERPJ,
     1           RPAR, IPAR)
      IPUP = 0
      RC = ONE
      DRC = ZERO
      CRATE = ONE
      NSLP = NST
      IF (IERPJ .NE. 0) GO TO 430
 250  DO 260 I = 1,N
 260    ACOR(I) = ZERO
 270  IF (MITER .NE. 0) GO TO 350
      DO 280 I = 1,N
 280    SAVF(I) = RL1*(H*SAVF(I) - YH(I,2))
      DO 290 I = 1,N
 290    Y(I) = SAVF(I) - ACOR(I)
      DEL = DVNORM (N, Y, EWT)
      DO 300 I = 1,N
 300    Y(I) = YH(I,1) + SAVF(I)
      CALL DCOPY (N, SAVF, 1, ACOR, 1)
      GO TO 400
 350  DO 360 I = 1,N
 360    Y(I) = (RL1*H)*SAVF(I) - (RL1*YH(I,2) + ACOR(I))
      CALL DVSOL (WM, IWM, Y, IERSL)
      NNI = NNI + 1
      IF (IERSL .GT. 0) GO TO 410
      IF (METH .EQ. 2 .AND. RC .NE. ONE) THEN
        CSCALE = TWO/(ONE + RC)
        CALL DSCAL (N, CSCALE, Y, 1)
      ENDIF
      DEL = DVNORM (N, Y, EWT)
      CALL DAXPY (N, ONE, Y, 1, ACOR, 1)
      DO 380 I = 1,N
 380    Y(I) = YH(I,1) + ACOR(I)
 400  IF (M .NE. 0) CRATE = MAX(CRDOWN*CRATE,DEL/DELP)
      DCON = DEL*MIN(ONE,CRATE)/TQ(4)
      IF (DCON .LE. ONE) GO TO 450
      M = M + 1
      IF (M .EQ. MAXCOR) GO TO 410
      IF (M .GE. 2 .AND. DEL .GT. RDIV*DELP) GO TO 410
      DELP = DEL
      CALL F (N, TN, Y, SAVF, RPAR, IPAR)
      NFE = NFE + 1
      GO TO 270
 410  IF (MITER .EQ. 0 .OR. JCUR .EQ. 1) GO TO 430
      ICF = 1
      IPUP = MITER
      GO TO 220
 430  CONTINUE
      NFLAG = -1
      ICF = 2
      IPUP = MITER
      RETURN
 450  NFLAG = 0
      JCUR = 0
      ICF = 0
      IF (M .EQ. 0) ACNRM = DEL
      IF (M .GT. 0) ACNRM = DVNORM (N, ACOR, EWT)
      RETURN
      END
      SUBROUTINE DVJAC (Y, YH, LDYH, EWT, FTEM, SAVF, WM, IWM, F, JAC,
     1                 IERPJ, RPAR, IPAR)
      EXTERNAL F, JAC
      DOUBLE PRECISION Y, YH, EWT, FTEM, SAVF, WM, RPAR
      INTEGER LDYH, IWM, IERPJ, IPAR
      DIMENSION Y(*), YH(LDYH,*), EWT(*), FTEM(*), SAVF(*),
     1   WM(*), IWM(*)
      DOUBLE PRECISION ACNRM, CCMXJ, CONP, CRATE, DRC, EL,
     1     ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2     RC, RL1, TAU, TQ, TN, UROUND
      INTEGER ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     1        L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     2        LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     3        N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     4        NSLP, NYH
      DOUBLE PRECISION HU
      INTEGER NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
      DOUBLE PRECISION CON, DI, FAC, HRL1, ONE, PT1, R, R0, SRUR, THOU,
     1     YI, YJ, YJJ, ZERO
      INTEGER I, I1, I2, IER, II, J, J1, JJ, JOK, LENP, MBA, MBAND,
     1        MEB1, MEBAND, ML, ML3, MU, NP1
      DOUBLE PRECISION DVNORM
      SAVE ONE, PT1, THOU, ZERO
      COMMON /DVOD01/ ACNRM, CCMXJ, CONP, CRATE, DRC, EL(13),
     1                ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2                RC, RL1, TAU(13), TQ(5), TN, UROUND,
     3                ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     4                L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     5                LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     6                N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     7                NSLP, NYH
      COMMON /DVOD02/ HU, NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
      DATA ONE /1.0D0/, THOU /1000.0D0/, ZERO /0.0D0/, PT1 /0.1D0/
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp Threadprivate(/DVOD01/,/DVOD02/,One,PT1,THOU,Zero)
      IERPJ = 0
      HRL1 = H*RL1
      JOK = JSV
      IF (JSV .EQ. 1) THEN
        IF (NST .EQ. 0 .OR. NST .GT. NSLJ+MSBJ) JOK = -1
        IF (ICF .EQ. 1 .AND. DRC .LT. CCMXJ) JOK = -1
        IF (ICF .EQ. 2) JOK = -1
      ENDIF
      IF (JOK .EQ. -1 .AND. MITER .EQ. 1) THEN
      NJE = NJE + 1
      NSLJ = NST
      JCUR = 1
      LENP = N*N
      DO 110 I = 1,LENP
 110    WM(I+2) = ZERO
      CALL JAC (N, TN, Y, 0, 0, WM(3), N, RPAR, IPAR)
      IF (JSV .EQ. 1) CALL DCOPY (LENP, WM(3), 1, WM(LOCJS), 1)
      ENDIF
      IF (JOK .EQ. -1 .AND. MITER .EQ. 2) THEN
      NJE = NJE + 1
      NSLJ = NST
      JCUR = 1
      FAC = DVNORM (N, SAVF, EWT)
      R0 = THOU*ABS(H)*UROUND*REAL(N)*FAC
      IF (R0 .EQ. ZERO) R0 = ONE
      SRUR = WM(1)
      J1 = 2
      DO 230 J = 1,N
        YJ = Y(J)
        R = MAX(SRUR*ABS(YJ),R0/EWT(J))
        Y(J) = Y(J) + R
        FAC = ONE/R
        CALL F (N, TN, Y, FTEM, RPAR, IPAR)
        DO 220 I = 1,N
 220      WM(I+J1) = (FTEM(I) - SAVF(I))*FAC
        Y(J) = YJ
        J1 = J1 + N
 230    CONTINUE
      NFE = NFE + N
      LENP = N*N
      IF (JSV .EQ. 1) CALL DCOPY (LENP, WM(3), 1, WM(LOCJS), 1)
      ENDIF
      IF (JOK .EQ. 1 .AND. (MITER .EQ. 1 .OR. MITER .EQ. 2)) THEN
      JCUR = 0
      LENP = N*N
      CALL DCOPY (LENP, WM(LOCJS), 1, WM(3), 1)
      ENDIF
      IF (MITER .EQ. 1 .OR. MITER .EQ. 2) THEN
      CON = -HRL1
      CALL DSCAL (LENP, CON, WM(3), 1)
      J = 3
      NP1 = N + 1
      DO 250 I = 1,N
        WM(J) = WM(J) + ONE
 250    J = J + NP1
      NLU = NLU + 1
      !CALL DGEFA (WM(3), N, N, IWM(31), IER)
      CALL DGETRF(N,N,WM(3),N,IWM(31),IER)
      IF (IER .NE. 0) IERPJ = 1
      RETURN
      ENDIF
      IF (MITER .EQ. 3) THEN
      NJE = NJE + 1
      JCUR = 1
      WM(2) = HRL1
      R = RL1*PT1
      DO 310 I = 1,N
 310    Y(I) = Y(I) + R*(H*SAVF(I) - YH(I,2))
      CALL F (N, TN, Y, WM(3), RPAR, IPAR)
      NFE = NFE + 1
      DO 320 I = 1,N
        R0 = H*SAVF(I) - YH(I,2)
        DI = PT1*R0 - H*(WM(I+2) - SAVF(I))
        WM(I+2) = ONE
        IF (ABS(R0) .LT. UROUND/EWT(I)) GO TO 320
        IF (ABS(DI) .EQ. ZERO) GO TO 330
        WM(I+2) = PT1*R0/DI
 320    CONTINUE
      RETURN
 330  IERPJ = 1
      RETURN
      ENDIF
      ML = IWM(1)
      MU = IWM(2)
      ML3 = ML + 3
      MBAND = ML + MU + 1
      MEBAND = MBAND + ML
      LENP = MEBAND*N
      IF (JOK .EQ. -1 .AND. MITER .EQ. 4) THEN
      NJE = NJE + 1
      NSLJ = NST
      JCUR = 1
      DO 410 I = 1,LENP
 410    WM(I+2) = ZERO
      CALL JAC (N, TN, Y, ML, MU, WM(ML3), MEBAND, RPAR, IPAR)
      IF (JSV .EQ. 1)
     1   CALL DACOPY (MBAND, N, WM(ML3), MEBAND, WM(LOCJS), MBAND)
      ENDIF
      IF (JOK .EQ. -1 .AND. MITER .EQ. 5) THEN
      NJE = NJE + 1
      NSLJ = NST
      JCUR = 1
      MBA = MIN(MBAND,N)
      MEB1 = MEBAND - 1
      SRUR = WM(1)
      FAC = DVNORM (N, SAVF, EWT)
      R0 = THOU*ABS(H)*UROUND*REAL(N)*FAC
      IF (R0 .EQ. ZERO) R0 = ONE
      DO 560 J = 1,MBA
        DO 530 I = J,N,MBAND
          YI = Y(I)
          R = MAX(SRUR*ABS(YI),R0/EWT(I))
 530      Y(I) = Y(I) + R
        CALL F (N, TN, Y, FTEM, RPAR, IPAR)
        DO 550 JJ = J,N,MBAND
          Y(JJ) = YH(JJ,1)
          YJJ = Y(JJ)
          R = MAX(SRUR*ABS(YJJ),R0/EWT(JJ))
          FAC = ONE/R
          I1 = MAX(JJ-MU,1)
          I2 = MIN(JJ+ML,N)
          II = JJ*MEB1 - ML + 2
          DO 540 I = I1,I2
 540        WM(II+I) = (FTEM(I) - SAVF(I))*FAC
 550      CONTINUE
 560    CONTINUE
      NFE = NFE + MBA
      IF (JSV .EQ. 1)
     1   CALL DACOPY (MBAND, N, WM(ML3), MEBAND, WM(LOCJS), MBAND)
      ENDIF
      IF (JOK .EQ. 1) THEN
      JCUR = 0
      CALL DACOPY (MBAND, N, WM(LOCJS), MBAND, WM(ML3), MEBAND)
      ENDIF
      CON = -HRL1
      CALL DSCAL (LENP, CON, WM(3), 1 )
      II = MBAND + 2
      DO 580 I = 1,N
        WM(II) = WM(II) + ONE
 580    II = II + MEBAND
      NLU = NLU + 1
      !CALL DGBFA (WM(3), MEBAND, N, ML, MU, IWM(31), IER)
      CALL DGBTRF(N,N,ML,MU,WM(3),MEBAND,IWM(31),IER)
      IF (IER .NE. 0) IERPJ = 1
      RETURN
      END
      SUBROUTINE DACOPY (NROW, NCOL, A, NROWA, B, NROWB)
      DOUBLE PRECISION A, B
      INTEGER NROW, NCOL, NROWA, NROWB
      DIMENSION A(NROWA,NCOL), B(NROWB,NCOL)
      INTEGER IC
      DO 20 IC = 1,NCOL
        CALL DCOPY (NROW, A(1,IC), 1, B(1,IC), 1)
 20     CONTINUE
      RETURN
      END
      SUBROUTINE DVSOL (WM, IWM, X, IERSL)
      DOUBLE PRECISION WM, X
      INTEGER IWM, IERSL
      DIMENSION WM(*), IWM(*), X(*)
      DOUBLE PRECISION ACNRM, CCMXJ, CONP, CRATE, DRC, EL,
     1     ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2     RC, RL1, TAU, TQ, TN, UROUND
      INTEGER ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     1        L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     2        LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     3        N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     4        NSLP, NYH
      INTEGER I, MEBAND, ML, MU
      DOUBLE PRECISION DI, HRL1, ONE, PHRL1, R, ZERO
      SAVE ONE, ZERO
      COMMON /DVOD01/ ACNRM, CCMXJ, CONP, CRATE, DRC, EL(13),
     1                ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2                RC, RL1, TAU(13), TQ(5), TN, UROUND,
     3                ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     4                L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     5                LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     6                N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     7                NSLP, NYH
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp Threadprivate(/DVOD01/,One,Zero)
      DATA ONE /1.0D0/, ZERO /0.0D0/
      IERSL = 0
      GO TO (100, 100, 300, 400, 400), MITER
 !100  !CALL DGESL (WM(3), N, N, IWM(31), X, 0)
 100  CALL DGETRS('N',N,1,WM(3),N,IWM(31),X,N,INFO)
      RETURN
 300  PHRL1 = WM(2)
      HRL1 = H*RL1
      WM(2) = HRL1
      IF (HRL1 .EQ. PHRL1) GO TO 330
      R = HRL1/PHRL1
      DO 320 I = 1,N
        DI = ONE - R*(ONE - ONE/WM(I+2))
        IF (ABS(DI) .EQ. ZERO) GO TO 390
 320    WM(I+2) = ONE/DI
 330  DO 340 I = 1,N
 340    X(I) = WM(I+2)*X(I)
      RETURN
 390  IERSL = 1
      RETURN
 400  ML = IWM(1)
      MU = IWM(2)
      MEBAND = 2*ML + MU + 1
      !CALL DGBSL (WM(3), MEBAND, N, ML, MU, IWM(31), X, 0)
      CALL DGBTRS('N',N,ML,MU,1,WM(3),MEBAND,IWM(31),X,N,INFO)
      RETURN
      END
      SUBROUTINE DVSRCO (RSAV, ISAV, JOB)
      DOUBLE PRECISION RSAV
      INTEGER ISAV, JOB
      DIMENSION RSAV(*), ISAV(*)
      DOUBLE PRECISION RVOD1, RVOD2
      INTEGER IVOD1, IVOD2
      INTEGER I, LENIV1, LENIV2, LENRV1, LENRV2
      SAVE LENRV1, LENIV1, LENRV2, LENIV2
      COMMON /DVOD01/ RVOD1(48), IVOD1(33)
      COMMON /DVOD02/ RVOD2(1), IVOD2(8)
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp THreadprivate(/DVOD01/,/DVOD02/,LENRV1,LENIV1,LENRV2,LENIV2)
      DATA LENRV1/48/, LENIV1/33/, LENRV2/1/, LENIV2/8/
      IF (JOB .EQ. 2) GO TO 100
      DO 10 I = 1,LENRV1
 10     RSAV(I) = RVOD1(I)
      DO 15 I = 1,LENRV2
 15     RSAV(LENRV1+I) = RVOD2(I)
      DO 20 I = 1,LENIV1
 20     ISAV(I) = IVOD1(I)
      DO 25 I = 1,LENIV2
 25     ISAV(LENIV1+I) = IVOD2(I)
      RETURN
 100  CONTINUE
      DO 110 I = 1,LENRV1
 110     RVOD1(I) = RSAV(I)
      DO 115 I = 1,LENRV2
 115     RVOD2(I) = RSAV(LENRV1+I)
      DO 120 I = 1,LENIV1
 120     IVOD1(I) = ISAV(I)
      DO 125 I = 1,LENIV2
 125     IVOD2(I) = ISAV(LENIV1+I)
      RETURN
      END
      SUBROUTINE DEWSET (N, ITOL, RTOL, ATOL, YCUR, EWT)
      DOUBLE PRECISION RTOL, ATOL, YCUR, EWT
      INTEGER N, ITOL
      DIMENSION ATOL(*), YCUR(N), EWT(N)
      INTEGER I
      GO TO (10, 20, 30, 40), ITOL
 10   CONTINUE
      DO 15 I = 1, N
 15     EWT(I) = RTOL*ABS(YCUR(I)) + ATOL(1)
      RETURN
 20   CONTINUE
      DO 25 I = 1, N
 25     EWT(I) = RTOL*ABS(YCUR(I)) + ATOL(I)
      RETURN
 30   CONTINUE
      DO 35 I = 1, N
 35     EWT(I) = RTOL*ABS(YCUR(I)) + ATOL(1)
      RETURN
 40   CONTINUE
      DO 45 I = 1, N
 45     EWT(I) = RTOL*ABS(YCUR(I)) + ATOL(I)
      RETURN
      END
      DOUBLE PRECISION FUNCTION DVNORM (N, V, W)
      DOUBLE PRECISION V, W
      INTEGER N
      DIMENSION V(N), W(N)
      DOUBLE PRECISION SUM
      INTEGER I
      SUM = 0.0D0
      DO 10 I = 1, N
 10     SUM = SUM + (V(I)*W(I))**2
      DVNORM = SQRT(SUM/REAL(N))
      RETURN
      END
      DOUBLE PRECISION FUNCTION D1MACH (IDUM)
      INTEGER IDUM
      DOUBLE PRECISION U, COMP
      U = 1.0D0
 10   U = U*0.5D0
      COMP = 1.0D0 + U
      IF (COMP .NE. 1.0D0) GO TO 10
      D1MACH = U*2.0D0
      RETURN
      END
      SUBROUTINE XERRWD (MSG, NMES, NERR, LEVEL, NI, I1, I2, NR, R1, R2)
      DOUBLE PRECISION R1, R2
      INTEGER NMES, NERR, LEVEL, NI, I1, I2, NR
      CHARACTER*1 MSG(NMES),ERRFIL*20
      COMMON/ERR/ERRFIL
	COMMON/NXER/NXE
      INTEGER I, LUNIT, LUNSAV, MESFLG, MFLGSV
	NXE = NXE + 1
	GO TO 100
      MESFLG = MFLGSV (0,.FALSE.)
      LUNIT = LUNSAV (0,.FALSE.)
      IF (MESFLG .EQ. 0) GO TO 100
      WRITE (LUNIT,10) (MSG(I),I=1,NMES)
 10   FORMAT(1X,80A1)
      IF (NI .EQ. 1) WRITE (LUNIT, 20) I1
 20   FORMAT(6X,'In above message,  I1 =',I10)
      IF (NI .EQ. 2) WRITE (LUNIT, 30) I1,I2
 30   FORMAT(6X,'In above message,  I1 =',I10,3X,'I2 =',I10)
      IF (NR .EQ. 1) WRITE (LUNIT, 40) R1
 40   FORMAT(6X,'In above message,  R1 =',D21.13)
      IF (NR .EQ. 2) WRITE (LUNIT, 50) R1,R2
 50   FORMAT(6X,'In above,  R1 =',D21.13,3X,'R2 =',D21.13)
 100  IF (LEVEL .NE. 2) RETURN
        OPEN(42,FILE=ERRFIL)
         WRITE(42,51)
   51    FORMAT(/' LEVEL 2 (FATAL) IN SUBROUTINE XERRWD. '/)
        CLOSE(42)
      CALL PAUSE
      STOP
      END
      SUBROUTINE XSETF (MFLAG)
      INTEGER MFLAG, JUNK, MFLGSV
      IF (MFLAG .EQ. 0 .OR. MFLAG .EQ. 1) JUNK = MFLGSV (MFLAG,.TRUE.)
      RETURN
      END
      SUBROUTINE XSETUN (LUN)
      INTEGER LUN, JUNK, LUNSAV
      IF (LUN .GT. 0) JUNK = LUNSAV (LUN,.TRUE.)
      RETURN
      END
      INTEGER FUNCTION MFLGSV (IVALUE, ISET)
      LOGICAL ISET
      INTEGER IVALUE
      INTEGER MESFLG
      SAVE MESFLG
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp Threadprivate(Mesflg)
      DATA MESFLG/1/
      MFLGSV = MESFLG
      IF (ISET) MESFLG = IVALUE
      RETURN
      END
      INTEGER FUNCTION LUNSAV (IVALUE, ISET)
      LOGICAL ISET
      INTEGER IVALUE
      INTEGER LUNIT
      SAVE LUNIT
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp THreadprivate(LUNIT)
      DATA LUNIT/6/
      LUNSAV = LUNIT
      IF (ISET) LUNIT = IVALUE
      RETURN
      END
      subroutine dgefa(a,lda,n,ipvt,info)
      integer lda,n,ipvt(1),info
      double precision a(lda,1)
      double precision t
      integer idamax,j,k,kp1,l,nm1
      info = 0
      nm1 = n - 1
      if (nm1 .lt. 1) go to 70
      do 60 k = 1, nm1
         kp1 = k + 1
         l = idamax(n-k+1,a(k,k),1) + k - 1
         ipvt(k) = l
         if (a(l,k) .eq. 0.0d0) go to 40
            if (l .eq. k) go to 10
               t = a(l,k)
               a(l,k) = a(k,k)
               a(k,k) = t
   10       continue
            t = -1.0d0/a(k,k)
            call dscal(n-k,t,a(k+1,k),1)
            do 30 j = kp1, n
               t = a(l,j)
               if (l .eq. k) go to 20
                  a(l,j) = a(k,j)
                  a(k,j) = t
   20          continue
               call daxpy(n-k,t,a(k+1,k),1,a(k+1,j),1)
   30       continue
         go to 50
   40    continue
            info = k
   50    continue
   60 continue
   70 continue
      ipvt(n) = n
      if (a(n,n) .eq. 0.0d0) info = n
      return
      end
      subroutine dgesl(a,lda,n,ipvt,b,job)
      integer lda,n,ipvt(1),job
      double precision a(lda,1),b(1)
      double precision ddot,t
      integer k,kb,l,nm1
      nm1 = n - 1
      if (job .ne. 0) go to 50
         if (nm1 .lt. 1) go to 30
         do 20 k = 1, nm1
            l = ipvt(k)
            t = b(l)
            if (l .eq. k) go to 10
               b(l) = b(k)
               b(k) = t
   10       continue
            call daxpy(n-k,t,a(k+1,k),1,b(k+1),1)
   20    continue
   30    continue
         do 40 kb = 1, n
            k = n + 1 - kb
            b(k) = b(k)/a(k,k)
            t = -b(k)
            call daxpy(k-1,t,a(1,k),1,b(1),1)
   40    continue
      go to 100
   50 continue
         do 60 k = 1, n
            t = ddot(k-1,a(1,k),1,b(1),1)
            b(k) = (b(k) - t)/a(k,k)
   60    continue
         if (nm1 .lt. 1) go to 90
         do 80 kb = 1, nm1
            k = n - kb
            b(k) = b(k) + ddot(n-k,a(k+1,k),1,b(k+1),1)
            l = ipvt(k)
            if (l .eq. k) go to 70
               t = b(l)
               b(l) = b(k)
               b(k) = t
   70       continue
   80    continue
   90    continue
  100 continue
      return
      end
      subroutine dgbfa(abd,lda,n,ml,mu,ipvt,info)
      integer lda,n,ml,mu,ipvt(1),info
      double precision abd(lda,1)
      double precision t
      integer i,idamax,i0,j,ju,jz,j0,j1,k,kp1,l,lm,m,mm,nm1
      m = ml + mu + 1
      info = 0
      j0 = mu + 2
      j1 = min0(n,m) - 1
      if (j1 .lt. j0) go to 30
      do 20 jz = j0, j1
         i0 = m + 1 - jz
         do 10 i = i0, ml
            abd(i,jz) = 0.0d0
   10    continue
   20 continue
   30 continue
      jz = j1
      ju = 0
      nm1 = n - 1
      if (nm1 .lt. 1) go to 130
      do 120 k = 1, nm1
         kp1 = k + 1
         jz = jz + 1
         if (jz .gt. n) go to 50
         if (ml .lt. 1) go to 50
            do 40 i = 1, ml
               abd(i,jz) = 0.0d0
   40       continue
   50    continue
         lm = min0(ml,n-k)
         l = idamax(lm+1,abd(m,k),1) + m - 1
         ipvt(k) = l + k - m
         if (abd(l,k) .eq. 0.0d0) go to 100
            if (l .eq. m) go to 60
               t = abd(l,k)
               abd(l,k) = abd(m,k)
               abd(m,k) = t
   60       continue
            t = -1.0d0/abd(m,k)
            call dscal(lm,t,abd(m+1,k),1)
            ju = min0(max0(ju,mu+ipvt(k)),n)
            mm = m
            if (ju .lt. kp1) go to 90
            do 80 j = kp1, ju
               l = l - 1
               mm = mm - 1
               t = abd(l,j)
               if (l .eq. mm) go to 70
                  abd(l,j) = abd(mm,j)
                  abd(mm,j) = t
   70          continue
               call daxpy(lm,t,abd(m+1,k),1,abd(mm+1,j),1)
   80       continue
   90       continue
         go to 110
  100    continue
            info = k
  110    continue
  120 continue
  130 continue
      ipvt(n) = n
      if (abd(m,n) .eq. 0.0d0) info = n
      return
      end
      subroutine dgbsl(abd,lda,n,ml,mu,ipvt,b,job)
      integer lda,n,ml,mu,ipvt(1),job
      double precision abd(lda,1),b(1)
      double precision ddot,t
      integer k,kb,l,la,lb,lm,m,nm1
      m = mu + ml + 1
      nm1 = n - 1
      if (job .ne. 0) go to 50
         if (ml .eq. 0) go to 30
         if (nm1 .lt. 1) go to 30
            do 20 k = 1, nm1
               lm = min0(ml,n-k)
               l = ipvt(k)
               t = b(l)
               if (l .eq. k) go to 10
                  b(l) = b(k)
                  b(k) = t
   10          continue
               call daxpy(lm,t,abd(m+1,k),1,b(k+1),1)
   20       continue
   30    continue
         do 40 kb = 1, n
            k = n + 1 - kb
            b(k) = b(k)/abd(m,k)
            lm = min0(k,m) - 1
            la = m - lm
            lb = k - lm
            t = -b(k)
            call daxpy(lm,t,abd(la,k),1,b(lb),1)
   40    continue
      go to 100
   50 continue
         do 60 k = 1, n
            lm = min0(k,m) - 1
            la = m - lm
            lb = k - lm
            t = ddot(lm,abd(la,k),1,b(lb),1)
            b(k) = (b(k) - t)/abd(m,k)
   60    continue
         if (ml .eq. 0) go to 90
         if (nm1 .lt. 1) go to 90
            do 80 kb = 1, nm1
               k = n - kb
               lm = min0(ml,n-k)
               b(k) = b(k) + ddot(lm,abd(m+1,k),1,b(k+1),1)
               l = ipvt(k)
               if (l .eq. k) go to 70
                  t = b(l)
                  b(l) = b(k)
                  b(k) = t
   70          continue
   80       continue
   90    continue
  100 continue
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
      SUBROUTINE DPOTRF( UPLO, N, A, LDA, INFO )
      CHARACTER          UPLO
      INTEGER            INFO, LDA, N
      DOUBLE PRECISION   A( LDA, * )
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
         CALL XERBLA( 'DPOTRF', -INFO )
         RETURN
      END IF
      IF( N.EQ.0 )
     $   RETURN
      nb = 16
      IF( NB.LE.1 .OR. NB.GE.N ) THEN
         CALL DPOTF2( UPLO, N, A, LDA, INFO )
      ELSE
         IF( UPPER ) THEN
            DO 10 J = 1, N, NB
               JB = MIN( NB, N-J+1 )
               CALL DSYRK( 'Upper', 'Transpose', JB, J-1, -ONE,
     $                     A( 1, J ), LDA, ONE, A( J, J ), LDA )
               CALL DPOTF2( 'Upper', JB, A( J, J ), LDA, INFO )
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
               CALL DPOTF2( 'Lower', JB, A( J, J ), LDA, INFO )
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
      SUBROUTINE DPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
      CHARACTER          UPLO
      INTEGER            INFO, LDA, LDB, N, NRHS
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
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
         CALL XERBLA( 'DPOTRS', -INFO )
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
      SUBROUTINE DPOTF2( UPLO, N, A, LDA, INFO )
      CHARACTER          UPLO
      INTEGER            INFO, LDA, N
      DOUBLE PRECISION   A( LDA, * )
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
         CALL XERBLA( 'DPOTF2', -INFO )
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
      SUBROUTINE XERBLA( SRNAME, INFO )
      CHARACTER*6        SRNAME,ERRFIL*20
      INTEGER            INFO
      COMMON/ERR/ERRFIL
      WRITE( *, FMT = 9999 )SRNAME, INFO
        OPEN(42,FILE=ERRFIL)
      WRITE( 42, FMT = 9999 )SRNAME, INFO
        CLOSE(42)
      CALL PAUSE
      STOP
 9999 FORMAT( ' ** On entry to ', A6, ' parameter number ', I2, ' had ',
     $      'an illegal value' )
      END
	SUBROUTINE JACOB(NDIM, T, X, ML, MU, PD, NRPD, RPAR, IPAR)
	IMPLICIT REAL*8(A-H,O-Z)
        COMMON/PARAMD/ P
        COMMON/INPUT/ R,B
        DIMENSION X(NDIM), PD(NRPD,NDIM), P(32),R(37),B(20)
! NEW PARALLEL CODE BELOW AS OF npageng28.f
!$omp Threadprivate(/PARAMD/,/INPUT/)
        RETURN
      END
      SUBROUTINE DGETRS( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
      CHARACTER          TRANS
      INTEGER            INFO, LDA, LDB, N, NRHS
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
      LOGICAL            NOTRAN
      LOGICAL            LSAME
      EXTERNAL           LSAME
      EXTERNAL           DLASWP, DTRSM, XERBLA
      INTRINSIC          MAX
      INFO = 0
      NOTRAN = LSAME( TRANS, 'N' )
      IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) .AND. .NOT.
     $    LSAME( TRANS, 'C' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGETRS', -INFO )
         RETURN
      END IF
      IF( N.EQ.0 .OR. NRHS.EQ.0 )
     $   RETURN
      IF( NOTRAN ) THEN
         CALL DLASWP( NRHS, B, LDB, 1, N, IPIV, 1 )
         CALL DTRSM( 'Left', 'Lower', 'No transpose', 'Unit', N, NRHS,
     $               ONE, A, LDA, B, LDB )
         CALL DTRSM( 'Left', 'Upper', 'No transpose', 'Non-unit', N,
     $               NRHS, ONE, A, LDA, B, LDB )
      ELSE
         CALL DTRSM( 'Left', 'Upper', 'Transpose', 'Non-unit', N, NRHS,
     $               ONE, A, LDA, B, LDB )
         CALL DTRSM( 'Left', 'Lower', 'Transpose', 'Unit', N, NRHS, ONE,
     $               A, LDA, B, LDB )
         CALL DLASWP( NRHS, B, LDB, 1, N, IPIV, -1 )
      END IF
      RETURN
      END
      INTEGER          FUNCTION IEEECK( ISPEC, ZERO, ONE )
      INTEGER            ISPEC
      REAL               ONE, ZERO
      REAL               NAN1, NAN2, NAN3, NAN4, NAN5, NAN6, NEGINF,
     $                   NEGZRO, NEWZRO, POSINF
      IEEECK = 1
      POSINF = ONE / ZERO
      IF( POSINF.LE.ONE ) THEN
         IEEECK = 0
         RETURN
      END IF
      NEGINF = -ONE / ZERO
      IF( NEGINF.GE.ZERO ) THEN
         IEEECK = 0
         RETURN
      END IF
      NEGZRO = ONE / ( NEGINF+ONE )
      IF( NEGZRO.NE.ZERO ) THEN
         IEEECK = 0
         RETURN
      END IF
      NEGINF = ONE / NEGZRO
      IF( NEGINF.GE.ZERO ) THEN
         IEEECK = 0
         RETURN
      END IF
      NEWZRO = NEGZRO + ZERO
      IF( NEWZRO.NE.ZERO ) THEN
         IEEECK = 0
         RETURN
      END IF
      POSINF = ONE / NEWZRO
      IF( POSINF.LE.ONE ) THEN
         IEEECK = 0
         RETURN
      END IF
      NEGINF = NEGINF*POSINF
      IF( NEGINF.GE.ZERO ) THEN
         IEEECK = 0
         RETURN
      END IF
      POSINF = POSINF*POSINF
      IF( POSINF.LE.ONE ) THEN
         IEEECK = 0
         RETURN
      END IF
      IF( ISPEC.EQ.0 )
     $   RETURN
      NAN1 = POSINF + NEGINF
      NAN2 = POSINF / NEGINF
      NAN3 = POSINF / POSINF
      NAN4 = POSINF*ZERO
      NAN5 = NEGINF*NEGZRO
      NAN6 = NAN5*ZERO
      IF( NAN1.EQ.NAN1 ) THEN
         IEEECK = 0
         RETURN
      END IF
      IF( NAN2.EQ.NAN2 ) THEN
         IEEECK = 0
         RETURN
      END IF
      IF( NAN3.EQ.NAN3 ) THEN
         IEEECK = 0
         RETURN
      END IF
      IF( NAN4.EQ.NAN4 ) THEN
         IEEECK = 0
         RETURN
      END IF
      IF( NAN5.EQ.NAN5 ) THEN
         IEEECK = 0
         RETURN
      END IF
      IF( NAN6.EQ.NAN6 ) THEN
         IEEECK = 0
         RETURN
      END IF
      RETURN
      END
      INTEGER FUNCTION IPARMQ( ISPEC, NAME, OPTS, N, ILO, IHI, LWORK )
      INTEGER            IHI, ILO, ISPEC, LWORK, N
      CHARACTER          NAME*( * ), OPTS*( * )
      INTEGER            INMIN, INWIN, INIBL, ISHFTS, IACC22, ICOST
      PARAMETER          ( INMIN = 12, INWIN = 13, INIBL = 14,
     $                   ISHFTS = 15, IACC22 = 16, ICOST = 17 )
      INTEGER            NMIN, K22MIN, KACMIN, NIBBLE, KNWSWP, RCOST
      PARAMETER          ( NMIN = 75, K22MIN = 14, KACMIN = 14,
     $                   NIBBLE = 14, KNWSWP = 500, RCOST = 10 )
      REAL               TWO
      PARAMETER          ( TWO = 2.0 )
      INTEGER            NH, NS
      INTEGER            I, IC, IZ
      CHARACTER          SUBNAM*6
      INTRINSIC          LOG, MAX, MOD, NINT, REAL
      IF( ( ISPEC.EQ.ISHFTS ) .OR. ( ISPEC.EQ.INWIN ) .OR.
     $    ( ISPEC.EQ.IACC22 ) ) THEN
         NH = IHI - ILO + 1
         NS = 2
         IF( NH.GE.30 )
     $      NS = 4
         IF( NH.GE.60 )
     $      NS = 10
         IF( NH.GE.150 )
     $      NS = MAX( 10, NH / NINT( LOG( REAL( NH ) ) / LOG( TWO ) ) )
         IF( NH.GE.590 )
     $      NS = 64
         IF( NH.GE.3000 )
     $      NS = 128
         IF( NH.GE.6000 )
     $      NS = 256
         NS = MAX( 2, NS-MOD( NS, 2 ) )
      END IF
      IF( ISPEC.EQ.INMIN ) THEN
         IPARMQ = NMIN
      ELSE IF( ISPEC.EQ.INIBL ) THEN
         IPARMQ = NIBBLE
      ELSE IF( ISPEC.EQ.ISHFTS ) THEN
         IPARMQ = NS
      ELSE IF( ISPEC.EQ.INWIN ) THEN
         IF( NH.LE.KNWSWP ) THEN
            IPARMQ = NS
         ELSE
            IPARMQ = 3*NS / 2
         END IF
      ELSE IF( ISPEC.EQ.IACC22 ) THEN
         IPARMQ = 0
         SUBNAM = NAME
         IC = ICHAR( SUBNAM( 1: 1 ) )
         IZ = ICHAR( 'Z' )
         IF( IZ.EQ.90 .OR. IZ.EQ.122 ) THEN
            IF( IC.GE.97 .AND. IC.LE.122 ) THEN
               SUBNAM( 1: 1 ) = CHAR( IC-32 )
               DO I = 2, 6
                  IC = ICHAR( SUBNAM( I: I ) )
                  IF( IC.GE.97 .AND. IC.LE.122 )
     $               SUBNAM( I: I ) = CHAR( IC-32 )
               END DO
            END IF
         ELSE IF( IZ.EQ.233 .OR. IZ.EQ.169 ) THEN
            IF( ( IC.GE.129 .AND. IC.LE.137 ) .OR.
     $          ( IC.GE.145 .AND. IC.LE.153 ) .OR.
     $          ( IC.GE.162 .AND. IC.LE.169 ) ) THEN
               SUBNAM( 1: 1 ) = CHAR( IC+64 )
               DO I = 2, 6
                  IC = ICHAR( SUBNAM( I: I ) )
                  IF( ( IC.GE.129 .AND. IC.LE.137 ) .OR.
     $                ( IC.GE.145 .AND. IC.LE.153 ) .OR.
     $                ( IC.GE.162 .AND. IC.LE.169 ) )SUBNAM( I:
     $                I ) = CHAR( IC+64 )
               END DO
            END IF
         ELSE IF( IZ.EQ.218 .OR. IZ.EQ.250 ) THEN
            IF( IC.GE.225 .AND. IC.LE.250 ) THEN
               SUBNAM( 1: 1 ) = CHAR( IC-32 )
               DO I = 2, 6
                  IC = ICHAR( SUBNAM( I: I ) )
                  IF( IC.GE.225 .AND. IC.LE.250 )
     $               SUBNAM( I: I ) = CHAR( IC-32 )
               END DO
            END IF
         END IF
         IF( SUBNAM( 2:6 ).EQ.'GGHRD' .OR.
     $       SUBNAM( 2:6 ).EQ.'GGHD3' ) THEN
            IPARMQ = 1
            IF( NH.GE.K22MIN )
     $         IPARMQ = 2
         ELSE IF ( SUBNAM( 4:6 ).EQ.'EXC' ) THEN
            IF( NH.GE.KACMIN )
     $         IPARMQ = 1
            IF( NH.GE.K22MIN )
     $         IPARMQ = 2
         ELSE IF ( SUBNAM( 2:6 ).EQ.'HSEQR' .OR.
     $             SUBNAM( 2:5 ).EQ.'LAQR' ) THEN
            IF( NS.GE.KACMIN )
     $         IPARMQ = 1
            IF( NS.GE.K22MIN )
     $         IPARMQ = 2
         END IF
      ELSE IF( ISPEC.EQ.ICOST ) THEN
         IPARMQ = RCOST
      ELSE
         IPARMQ = -1
      END IF
      END
      DOUBLE PRECISION FUNCTION DLAMCH( CMACH )
      CHARACTER          CMACH
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      DOUBLE PRECISION   RND, EPS, SFMIN, SMALL, RMACH
      LOGICAL            LSAME
      EXTERNAL           LSAME
      INTRINSIC          DIGITS, EPSILON, HUGE, MAXEXPONENT,
     $                   MINEXPONENT, RADIX, TINY
      RND = ONE
      IF( ONE.EQ.RND ) THEN
         EPS = EPSILON(ZERO) * 0.5
      ELSE
         EPS = EPSILON(ZERO)
      END IF
      IF( LSAME( CMACH, 'E' ) ) THEN
         RMACH = EPS
      ELSE IF( LSAME( CMACH, 'S' ) ) THEN
         SFMIN = TINY(ZERO)
         SMALL = ONE / HUGE(ZERO)
         IF( SMALL.GE.SFMIN ) THEN
            SFMIN = SMALL*( ONE+EPS )
         END IF
         RMACH = SFMIN
      ELSE IF( LSAME( CMACH, 'B' ) ) THEN
         RMACH = RADIX(ZERO)
      ELSE IF( LSAME( CMACH, 'P' ) ) THEN
         RMACH = EPS * RADIX(ZERO)
      ELSE IF( LSAME( CMACH, 'N' ) ) THEN
         RMACH = DIGITS(ZERO)
      ELSE IF( LSAME( CMACH, 'R' ) ) THEN
         RMACH = RND
      ELSE IF( LSAME( CMACH, 'M' ) ) THEN
         RMACH = MINEXPONENT(ZERO)
      ELSE IF( LSAME( CMACH, 'U' ) ) THEN
         RMACH = tiny(zero)
      ELSE IF( LSAME( CMACH, 'L' ) ) THEN
         RMACH = MAXEXPONENT(ZERO)
      ELSE IF( LSAME( CMACH, 'O' ) ) THEN
         RMACH = HUGE(ZERO)
      ELSE
         RMACH = ZERO
      END IF
      DLAMCH = RMACH
      RETURN
      END
      DOUBLE PRECISION FUNCTION DLAMC3( A, B )
      DOUBLE PRECISION   A, B
      DLAMC3 = A + B
      RETURN
      END
      RECURSIVE SUBROUTINE DGETRF2( M, N, A, LDA, IPIV, INFO )
      INTEGER            INFO, LDA, M, N
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * )
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      DOUBLE PRECISION   SFMIN, TEMP
      INTEGER            I, IINFO, N1, N2
      DOUBLE PRECISION   DLAMCH
      INTEGER            IDAMAX
      EXTERNAL           DLAMCH, IDAMAX
      EXTERNAL           DGEMM, DSCAL, DLASWP, DTRSM, XERBLA
      INTRINSIC          MAX, MIN
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGETRF2', -INFO )
         RETURN
      END IF
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
      IF ( M.EQ.1 ) THEN
         IPIV( 1 ) = 1
         IF ( A(1,1).EQ.ZERO )
     $      INFO = 1
      ELSE IF( N.EQ.1 ) THEN
         SFMIN = DLAMCH('S')
         I = IDAMAX( M, A( 1, 1 ), 1 )
         IPIV( 1 ) = I
         IF( A( I, 1 ).NE.ZERO ) THEN
            IF( I.NE.1 ) THEN
               TEMP = A( 1, 1 )
               A( 1, 1 ) = A( I, 1 )
               A( I, 1 ) = TEMP
            END IF
            IF( ABS(A( 1, 1 )) .GE. SFMIN ) THEN
               CALL DSCAL( M-1, ONE / A( 1, 1 ), A( 2, 1 ), 1 )
            ELSE
               DO 10 I = 1, M-1
                  A( 1+I, 1 ) = A( 1+I, 1 ) / A( 1, 1 )
   10          CONTINUE
            END IF
         ELSE
            INFO = 1
         END IF
      ELSE
         N1 = MIN( M, N ) / 2
         N2 = N-N1
         CALL DGETRF2( M, N1, A, LDA, IPIV, IINFO )
         IF ( INFO.EQ.0 .AND. IINFO.GT.0 )
     $      INFO = IINFO
         CALL DLASWP( N2, A( 1, N1+1 ), LDA, 1, N1, IPIV, 1 )
         CALL DTRSM( 'L', 'L', 'N', 'U', N1, N2, ONE, A, LDA,
     $               A( 1, N1+1 ), LDA )
         CALL DGEMM( 'N', 'N', M-N1, N2, N1, -ONE, A( N1+1, 1 ), LDA,
     $               A( 1, N1+1 ), LDA, ONE, A( N1+1, N1+1 ), LDA )
         CALL DGETRF2( M-N1, N2, A( N1+1, N1+1 ), LDA, IPIV( N1+1 ),
     $                 IINFO )
         IF ( INFO.EQ.0 .AND. IINFO.GT.0 )
     $      INFO = IINFO + N1
         DO 20 I = N1+1, MIN( M, N )
            IPIV( I ) = IPIV( I ) + N1
   20    CONTINUE
         CALL DLASWP( N1, A( 1, 1 ), LDA, N1+1, MIN( M, N), IPIV, 1 )
      END IF
      RETURN
      END
      SUBROUTINE DGBTRF( M, N, KL, KU, AB, LDAB, IPIV, INFO )
      INTEGER            INFO, KL, KU, LDAB, M, N
      INTEGER            IPIV( * )
      DOUBLE PRECISION   AB( LDAB, * )
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      INTEGER            NBMAX, LDWORK
      PARAMETER          ( NBMAX = 64, LDWORK = NBMAX+1 )
      INTEGER            I, I2, I3, II, IP, J, J2, J3, JB, JJ, JM, JP,
     $                   JU, K2, KM, KV, NB, NW
      DOUBLE PRECISION   TEMP
      DOUBLE PRECISION   WORK13( LDWORK, NBMAX ),
     $                   WORK31( LDWORK, NBMAX )
      INTEGER            IDAMAX, ILAENV
      EXTERNAL           IDAMAX, ILAENV
      EXTERNAL           DCOPY, DGBTF2, DGEMM, DGER, DLASWP, DSCAL,
     $                   DSWAP, DTRSM, XERBLA
      INTRINSIC          MAX, MIN
      KV = KU + KL
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( KL.LT.0 ) THEN
         INFO = -3
      ELSE IF( KU.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDAB.LT.KL+KV+1 ) THEN
         INFO = -6
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGBTRF', -INFO )
         RETURN
      END IF
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
      NB = ILAENV( 1, 'DGBTRF', ' ', M, N, KL, KU )
      NB = MIN( NB, NBMAX )
      IF( NB.LE.1 .OR. NB.GT.KL ) THEN
         CALL DGBTF2( M, N, KL, KU, AB, LDAB, IPIV, INFO )
      ELSE
         DO 20 J = 1, NB
            DO 10 I = 1, J - 1
               WORK13( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
         DO 40 J = 1, NB
            DO 30 I = J + 1, NB
               WORK31( I, J ) = ZERO
   30       CONTINUE
   40    CONTINUE
         DO 60 J = KU + 2, MIN( KV, N )
            DO 50 I = KV - J + 2, KL
               AB( I, J ) = ZERO
   50       CONTINUE
   60    CONTINUE
         JU = 1
         DO 180 J = 1, MIN( M, N ), NB
            JB = MIN( NB, MIN( M, N )-J+1 )
            I2 = MIN( KL-JB, M-J-JB+1 )
            I3 = MIN( JB, M-J-KL+1 )
            DO 80 JJ = J, J + JB - 1
               IF( JJ+KV.LE.N ) THEN
                  DO 70 I = 1, KL
                     AB( I, JJ+KV ) = ZERO
   70             CONTINUE
               END IF
               KM = MIN( KL, M-JJ )
               JP = IDAMAX( KM+1, AB( KV+1, JJ ), 1 )
               IPIV( JJ ) = JP + JJ - J
               IF( AB( KV+JP, JJ ).NE.ZERO ) THEN
                  JU = MAX( JU, MIN( JJ+KU+JP-1, N ) )
                  IF( JP.NE.1 ) THEN
                     IF( JP+JJ-1.LT.J+KL ) THEN
                        CALL DSWAP( JB, AB( KV+1+JJ-J, J ), LDAB-1,
     $                              AB( KV+JP+JJ-J, J ), LDAB-1 )
                     ELSE
                        CALL DSWAP( JJ-J, AB( KV+1+JJ-J, J ), LDAB-1,
     $                              WORK31( JP+JJ-J-KL, 1 ), LDWORK )
                        CALL DSWAP( J+JB-JJ, AB( KV+1, JJ ), LDAB-1,
     $                              AB( KV+JP, JJ ), LDAB-1 )
                     END IF
                  END IF
                  CALL DSCAL( KM, ONE / AB( KV+1, JJ ), AB( KV+2, JJ ),
     $                        1 )
                  JM = MIN( JU, J+JB-1 )
                  IF( JM.GT.JJ )
     $               CALL DGER( KM, JM-JJ, -ONE, AB( KV+2, JJ ), 1,
     $                          AB( KV, JJ+1 ), LDAB-1,
     $                          AB( KV+1, JJ+1 ), LDAB-1 )
               ELSE
                  IF( INFO.EQ.0 )
     $               INFO = JJ
               END IF
               NW = MIN( JJ-J+1, I3 )
               IF( NW.GT.0 )
     $            CALL DCOPY( NW, AB( KV+KL+1-JJ+J, JJ ), 1,
     $                        WORK31( 1, JJ-J+1 ), 1 )
   80       CONTINUE
            IF( J+JB.LE.N ) THEN
               J2 = MIN( JU-J+1, KV ) - JB
               J3 = MAX( 0, JU-J-KV+1 )
               CALL DLASWP( J2, AB( KV+1-JB, J+JB ), LDAB-1, 1, JB,
     $                      IPIV( J ), 1 )
               DO 90 I = J, J + JB - 1
                  IPIV( I ) = IPIV( I ) + J - 1
   90          CONTINUE
               K2 = J - 1 + JB + J2
               DO 110 I = 1, J3
                  JJ = K2 + I
                  DO 100 II = J + I - 1, J + JB - 1
                     IP = IPIV( II )
                     IF( IP.NE.II ) THEN
                        TEMP = AB( KV+1+II-JJ, JJ )
                        AB( KV+1+II-JJ, JJ ) = AB( KV+1+IP-JJ, JJ )
                        AB( KV+1+IP-JJ, JJ ) = TEMP
                     END IF
  100             CONTINUE
  110          CONTINUE
               IF( J2.GT.0 ) THEN
                  CALL DTRSM( 'Left', 'Lower', 'No transpose', 'Unit',
     $                        JB, J2, ONE, AB( KV+1, J ), LDAB-1,
     $                        AB( KV+1-JB, J+JB ), LDAB-1 )
                  IF( I2.GT.0 ) THEN
                     CALL DGEMM( 'No transpose', 'No transpose', I2, J2,
     $                           JB, -ONE, AB( KV+1+JB, J ), LDAB-1,
     $                           AB( KV+1-JB, J+JB ), LDAB-1, ONE,
     $                           AB( KV+1, J+JB ), LDAB-1 )
                  END IF
                  IF( I3.GT.0 ) THEN
                     CALL DGEMM( 'No transpose', 'No transpose', I3, J2,
     $                           JB, -ONE, WORK31, LDWORK,
     $                           AB( KV+1-JB, J+JB ), LDAB-1, ONE,
     $                           AB( KV+KL+1-JB, J+JB ), LDAB-1 )
                  END IF
               END IF
               IF( J3.GT.0 ) THEN
                  DO 130 JJ = 1, J3
                     DO 120 II = JJ, JB
                        WORK13( II, JJ ) = AB( II-JJ+1, JJ+J+KV-1 )
  120                CONTINUE
  130             CONTINUE
                  CALL DTRSM( 'Left', 'Lower', 'No transpose', 'Unit',
     $                        JB, J3, ONE, AB( KV+1, J ), LDAB-1,
     $                        WORK13, LDWORK )
                  IF( I2.GT.0 ) THEN
                     CALL DGEMM( 'No transpose', 'No transpose', I2, J3,
     $                           JB, -ONE, AB( KV+1+JB, J ), LDAB-1,
     $                           WORK13, LDWORK, ONE, AB( 1+JB, J+KV ),
     $                           LDAB-1 )
                  END IF
                  IF( I3.GT.0 ) THEN
                     CALL DGEMM( 'No transpose', 'No transpose', I3, J3,
     $                           JB, -ONE, WORK31, LDWORK, WORK13,
     $                           LDWORK, ONE, AB( 1+KL, J+KV ), LDAB-1 )
                  END IF
                  DO 150 JJ = 1, J3
                     DO 140 II = JJ, JB
                        AB( II-JJ+1, JJ+J+KV-1 ) = WORK13( II, JJ )
  140                CONTINUE
  150             CONTINUE
               END IF
            ELSE
               DO 160 I = J, J + JB - 1
                  IPIV( I ) = IPIV( I ) + J - 1
  160          CONTINUE
            END IF
            DO 170 JJ = J + JB - 1, J, -1
               JP = IPIV( JJ ) - JJ + 1
               IF( JP.NE.1 ) THEN
                  IF( JP+JJ-1.LT.J+KL ) THEN
                     CALL DSWAP( JJ-J, AB( KV+1+JJ-J, J ), LDAB-1,
     $                           AB( KV+JP+JJ-J, J ), LDAB-1 )
                  ELSE
                     CALL DSWAP( JJ-J, AB( KV+1+JJ-J, J ), LDAB-1,
     $                           WORK31( JP+JJ-J-KL, 1 ), LDWORK )
                  END IF
               END IF
               NW = MIN( I3, JJ-J+1 )
               IF( NW.GT.0 )
     $            CALL DCOPY( NW, WORK31( 1, JJ-J+1 ), 1,
     $                        AB( KV+KL+1-JJ+J, JJ ), 1 )
  170       CONTINUE
  180    CONTINUE
      END IF
      RETURN
      END
      SUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )
      INTEGER            INFO, LDA, M, N
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
      INTEGER            I, IINFO, J, JB, NB
      EXTERNAL           DGEMM, DGETRF2, DLASWP, DTRSM, XERBLA
      INTEGER            ILAENV
      EXTERNAL           ILAENV
      INTRINSIC          MAX, MIN
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGETRF', -INFO )
         RETURN
      END IF
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
      NB = ILAENV( 1, 'DGETRF', ' ', M, N, -1, -1 )
      IF( NB.LE.1 .OR. NB.GE.MIN( M, N ) ) THEN
         CALL DGETRF2( M, N, A, LDA, IPIV, INFO )
      ELSE
         DO 20 J = 1, MIN( M, N ), NB
            JB = MIN( MIN( M, N )-J+1, NB )
            CALL DGETRF2( M-J+1, JB, A( J, J ), LDA, IPIV( J ), IINFO )
            IF( INFO.EQ.0 .AND. IINFO.GT.0 )
     $         INFO = IINFO + J - 1
            DO 10 I = J, MIN( M, J+JB-1 )
               IPIV( I ) = J - 1 + IPIV( I )
   10       CONTINUE
            CALL DLASWP( J-1, A, LDA, J, J+JB-1, IPIV, 1 )
            IF( J+JB.LE.N ) THEN
               CALL DLASWP( N-J-JB+1, A( 1, J+JB ), LDA, J, J+JB-1,
     $                      IPIV, 1 )
               CALL DTRSM( 'Left', 'Lower', 'No transpose', 'Unit', JB,
     $                     N-J-JB+1, ONE, A( J, J ), LDA, A( J, J+JB ),
     $                     LDA )
               IF( J+JB.LE.M ) THEN
                  CALL DGEMM( 'No transpose', 'No transpose', M-J-JB+1,
     $                        N-J-JB+1, JB, -ONE, A( J+JB, J ), LDA,
     $                        A( J, J+JB ), LDA, ONE, A( J+JB, J+JB ),
     $                        LDA )
               END IF
            END IF
   20    CONTINUE
      END IF
      RETURN
      END
      SUBROUTINE DGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB,
     $                   INFO )
      CHARACTER          TRANS
      INTEGER            INFO, KL, KU, LDAB, LDB, N, NRHS
      INTEGER            IPIV( * )
      DOUBLE PRECISION   AB( LDAB, * ), B( LDB, * )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
      LOGICAL            LNOTI, NOTRAN
      INTEGER            I, J, KD, L, LM
      LOGICAL            LSAME
      EXTERNAL           LSAME
      EXTERNAL           DGEMV, DGER, DSWAP, DTBSV, XERBLA
      INTRINSIC          MAX, MIN
      INFO = 0
      NOTRAN = LSAME( TRANS, 'N' )
      IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) .AND. .NOT.
     $    LSAME( TRANS, 'C' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( KL.LT.0 ) THEN
         INFO = -3
      ELSE IF( KU.LT.0 ) THEN
         INFO = -4
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDAB.LT.( 2*KL+KU+1 ) ) THEN
         INFO = -7
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -10
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGBTRS', -INFO )
         RETURN
      END IF
      IF( N.EQ.0 .OR. NRHS.EQ.0 )
     $   RETURN
      KD = KU + KL + 1
      LNOTI = KL.GT.0
      IF( NOTRAN ) THEN
         IF( LNOTI ) THEN
            DO 10 J = 1, N - 1
               LM = MIN( KL, N-J )
               L = IPIV( J )
               IF( L.NE.J )
     $            CALL DSWAP( NRHS, B( L, 1 ), LDB, B( J, 1 ), LDB )
               CALL DGER( LM, NRHS, -ONE, AB( KD+1, J ), 1, B( J, 1 ),
     $                    LDB, B( J+1, 1 ), LDB )
   10       CONTINUE
         END IF
         DO 20 I = 1, NRHS
            CALL DTBSV( 'Upper', 'No transpose', 'Non-unit', N, KL+KU,
     $                  AB, LDAB, B( 1, I ), 1 )
   20    CONTINUE
      ELSE
         DO 30 I = 1, NRHS
            CALL DTBSV( 'Upper', 'Transpose', 'Non-unit', N, KL+KU, AB,
     $                  LDAB, B( 1, I ), 1 )
   30    CONTINUE
         IF( LNOTI ) THEN
            DO 40 J = N - 1, 1, -1
               LM = MIN( KL, N-J )
               CALL DGEMV( 'Transpose', LM, NRHS, -ONE, B( J+1, 1 ),
     $                     LDB, AB( KD+1, J ), 1, ONE, B( J, 1 ), LDB )
               L = IPIV( J )
               IF( L.NE.J )
     $            CALL DSWAP( NRHS, B( L, 1 ), LDB, B( J, 1 ), LDB )
   40       CONTINUE
         END IF
      END IF
      RETURN
      END
      INTEGER FUNCTION ILAENV( ISPEC, NAME, OPTS, N1, N2, N3, N4 )
      CHARACTER*( * )    NAME, OPTS
      INTEGER            ISPEC, N1, N2, N3, N4
      INTEGER            I, IC, IZ, NB, NBMIN, NX
      LOGICAL            CNAME, SNAME, TWOSTAGE
      CHARACTER          C1*1, C2*2, C4*2, C3*3, SUBNAM*16
      INTRINSIC          CHAR, ICHAR, INT, MIN, REAL
      INTEGER            IEEECK, IPARMQ, IPARAM2STAGE
      EXTERNAL           IEEECK, IPARMQ, IPARAM2STAGE
      GO TO ( 10, 10, 10, 80, 90, 100, 110, 120,
     $        130, 140, 150, 160, 160, 160, 160, 160, 160)ISPEC
      ILAENV = -1
      RETURN
   10 CONTINUE
      ILAENV = 1
      SUBNAM = NAME
      IC = ICHAR( SUBNAM( 1: 1 ) )
      IZ = ICHAR( 'Z' )
      IF( IZ.EQ.90 .OR. IZ.EQ.122 ) THEN
         IF( IC.GE.97 .AND. IC.LE.122 ) THEN
            SUBNAM( 1: 1 ) = CHAR( IC-32 )
            DO 20 I = 2, 6
               IC = ICHAR( SUBNAM( I: I ) )
               IF( IC.GE.97 .AND. IC.LE.122 )
     $            SUBNAM( I: I ) = CHAR( IC-32 )
   20       CONTINUE
         END IF
      ELSE IF( IZ.EQ.233 .OR. IZ.EQ.169 ) THEN
         IF( ( IC.GE.129 .AND. IC.LE.137 ) .OR.
     $       ( IC.GE.145 .AND. IC.LE.153 ) .OR.
     $       ( IC.GE.162 .AND. IC.LE.169 ) ) THEN
            SUBNAM( 1: 1 ) = CHAR( IC+64 )
            DO 30 I = 2, 6
               IC = ICHAR( SUBNAM( I: I ) )
               IF( ( IC.GE.129 .AND. IC.LE.137 ) .OR.
     $             ( IC.GE.145 .AND. IC.LE.153 ) .OR.
     $             ( IC.GE.162 .AND. IC.LE.169 ) )SUBNAM( I:
     $             I ) = CHAR( IC+64 )
   30       CONTINUE
         END IF
      ELSE IF( IZ.EQ.218 .OR. IZ.EQ.250 ) THEN
         IF( IC.GE.225 .AND. IC.LE.250 ) THEN
            SUBNAM( 1: 1 ) = CHAR( IC-32 )
            DO 40 I = 2, 6
               IC = ICHAR( SUBNAM( I: I ) )
               IF( IC.GE.225 .AND. IC.LE.250 )
     $            SUBNAM( I: I ) = CHAR( IC-32 )
   40       CONTINUE
         END IF
      END IF
      C1 = SUBNAM( 1: 1 )
      SNAME = C1.EQ.'S' .OR. C1.EQ.'D'
      CNAME = C1.EQ.'C' .OR. C1.EQ.'Z'
      IF( .NOT.( CNAME .OR. SNAME ) )
     $   RETURN
      C2 = SUBNAM( 2: 3 )
      C3 = SUBNAM( 4: 6 )
      C4 = C3( 2: 3 )
      TWOSTAGE = LEN( SUBNAM ).GE.11
     $           .AND. SUBNAM( 11: 11 ).EQ.'2'
      GO TO ( 50, 60, 70 )ISPEC
   50 CONTINUE
      NB = 1
      IF( SUBNAM(2:6).EQ.'LAORH' ) THEN
         IF( SNAME ) THEN
             NB = 32
         ELSE
             NB = 32
         END IF
      ELSE IF( C2.EQ.'GE' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         ELSE IF( C3.EQ.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR.
     $            C3.EQ.'QLF' ) THEN
            IF( SNAME ) THEN
               NB = 32
            ELSE
               NB = 32
            END IF
         ELSE IF( C3.EQ.'QR ') THEN
            IF( N3 .EQ. 1) THEN
               IF( SNAME ) THEN
                  IF ((N1*N2.LE.131072).OR.(N1.LE.8192)) THEN
                     NB = N1
                  ELSE
                     NB = 32768/N2
                  END IF
               ELSE
                  IF ((N1*N2.LE.131072).OR.(N1.LE.8192)) THEN
                     NB = N1
                  ELSE
                     NB = 32768/N2
                  END IF
               END IF
            ELSE
               IF( SNAME ) THEN
                  NB = 1
               ELSE
                  NB = 1
               END IF
            END IF
         ELSE IF( C3.EQ.'LQ ') THEN
            IF( N3 .EQ. 2) THEN
               IF( SNAME ) THEN
                  IF ((N1*N2.LE.131072).OR.(N1.LE.8192)) THEN
                     NB = N1
                  ELSE
                     NB = 32768/N2
                  END IF
               ELSE
                  IF ((N1*N2.LE.131072).OR.(N1.LE.8192)) THEN
                     NB = N1
                  ELSE
                     NB = 32768/N2
                  END IF
               END IF
            ELSE
               IF( SNAME ) THEN
                  NB = 1
               ELSE
                  NB = 1
               END IF
            END IF
         ELSE IF( C3.EQ.'HRD' ) THEN
            IF( SNAME ) THEN
               NB = 32
            ELSE
               NB = 32
            END IF
         ELSE IF( C3.EQ.'BRD' ) THEN
            IF( SNAME ) THEN
               NB = 32
            ELSE
               NB = 32
            END IF
         ELSE IF( C3.EQ.'TRI' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( C2.EQ.'PO' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( C2.EQ.'SY' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               IF( TWOSTAGE ) THEN
                  NB = 192
               ELSE
                  NB = 64
               END IF
            ELSE
               IF( TWOSTAGE ) THEN
                  NB = 192
               ELSE
                  NB = 64
               END IF
            END IF
         ELSE IF( SNAME .AND. C3.EQ.'TRD' ) THEN
            NB = 32
         ELSE IF( SNAME .AND. C3.EQ.'GST' ) THEN
            NB = 64
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( TWOSTAGE ) THEN
               NB = 192
            ELSE
               NB = 64
            END IF
         ELSE IF( C3.EQ.'TRD' ) THEN
            NB = 32
         ELSE IF( C3.EQ.'GST' ) THEN
            NB = 64
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN
         IF( C3( 1: 1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NB = 32
            END IF
         ELSE IF( C3( 1: 1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NB = 32
            END IF
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN
         IF( C3( 1: 1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NB = 32
            END IF
         ELSE IF( C3( 1: 1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NB = 32
            END IF
         END IF
      ELSE IF( C2.EQ.'GB' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               IF( N4.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            ELSE
               IF( N4.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            END IF
         END IF
      ELSE IF( C2.EQ.'PB' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               IF( N2.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            ELSE
               IF( N2.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            END IF
         END IF
      ELSE IF( C2.EQ.'TR' ) THEN
         IF( C3.EQ.'TRI' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         ELSE IF ( C3.EQ.'EVC' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( C2.EQ.'LA' ) THEN
         IF( C3.EQ.'UUM' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'ST' ) THEN
         IF( C3.EQ.'EBZ' ) THEN
            NB = 1
         END IF
      ELSE IF( C2.EQ.'GG' ) THEN
         NB = 32
         IF( C3.EQ.'HD3' ) THEN
            IF( SNAME ) THEN
               NB = 32
            ELSE
               NB = 32
            END IF
         END IF
      END IF
      ILAENV = NB
      RETURN
   60 CONTINUE
      NBMIN = 2
      IF( C2.EQ.'GE' ) THEN
         IF( C3.EQ.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR. C3.EQ.
     $       'QLF' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( C3.EQ.'HRD' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( C3.EQ.'BRD' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( C3.EQ.'TRI' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         END IF
      ELSE IF( C2.EQ.'SY' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NBMIN = 8
            ELSE
               NBMIN = 8
            END IF
         ELSE IF( SNAME .AND. C3.EQ.'TRD' ) THEN
            NBMIN = 2
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN
         IF( C3.EQ.'TRD' ) THEN
            NBMIN = 2
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN
         IF( C3( 1: 1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NBMIN = 2
            END IF
         ELSE IF( C3( 1: 1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NBMIN = 2
            END IF
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN
         IF( C3( 1: 1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NBMIN = 2
            END IF
         ELSE IF( C3( 1: 1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NBMIN = 2
            END IF
         END IF
      ELSE IF( C2.EQ.'GG' ) THEN
         NBMIN = 2
         IF( C3.EQ.'HD3' ) THEN
            NBMIN = 2
         END IF
      END IF
      ILAENV = NBMIN
      RETURN
   70 CONTINUE
      NX = 0
      IF( C2.EQ.'GE' ) THEN
         IF( C3.EQ.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR. C3.EQ.
     $       'QLF' ) THEN
            IF( SNAME ) THEN
               NX = 128
            ELSE
               NX = 128
            END IF
         ELSE IF( C3.EQ.'HRD' ) THEN
            IF( SNAME ) THEN
               NX = 128
            ELSE
               NX = 128
            END IF
         ELSE IF( C3.EQ.'BRD' ) THEN
            IF( SNAME ) THEN
               NX = 128
            ELSE
               NX = 128
            END IF
         END IF
      ELSE IF( C2.EQ.'SY' ) THEN
         IF( SNAME .AND. C3.EQ.'TRD' ) THEN
            NX = 32
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN
         IF( C3.EQ.'TRD' ) THEN
            NX = 32
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN
         IF( C3( 1: 1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NX = 128
            END IF
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN
         IF( C3( 1: 1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NX = 128
            END IF
         END IF
      ELSE IF( C2.EQ.'GG' ) THEN
         NX = 128
         IF( C3.EQ.'HD3' ) THEN
            NX = 128
         END IF
      END IF
      ILAENV = NX
      RETURN
   80 CONTINUE
      ILAENV = 6
      RETURN
   90 CONTINUE
      ILAENV = 2
      RETURN
  100 CONTINUE
      ILAENV = INT( REAL( MIN( N1, N2 ) )*1.6E0 )
      RETURN
  110 CONTINUE
      ILAENV = 1
      RETURN
  120 CONTINUE
      ILAENV = 50
      RETURN
  130 CONTINUE
      ILAENV = 25
      RETURN
  140 CONTINUE
      ILAENV = 1
      IF( ILAENV.EQ.1 ) THEN
         ILAENV = IEEECK( 1, 0.0, 1.0 )
      END IF
      RETURN
  150 CONTINUE
      ILAENV = 1
      IF( ILAENV.EQ.1 ) THEN
         ILAENV = IEEECK( 0, 0.0, 1.0 )
      END IF
      RETURN
  160 CONTINUE
      ILAENV = IPARMQ( ISPEC, NAME, OPTS, N1, N2, N3, N4 )
      RETURN
      END
      SUBROUTINE DGBTF2( M, N, KL, KU, AB, LDAB, IPIV, INFO )
      INTEGER            INFO, KL, KU, LDAB, M, N
      INTEGER            IPIV( * )
      DOUBLE PRECISION   AB( LDAB, * )
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      INTEGER            I, J, JP, JU, KM, KV
      INTEGER            IDAMAX
      EXTERNAL           IDAMAX
      EXTERNAL           DGER, DSCAL, DSWAP, XERBLA
      INTRINSIC          MAX, MIN
      KV = KU + KL
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( KL.LT.0 ) THEN
         INFO = -3
      ELSE IF( KU.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDAB.LT.KL+KV+1 ) THEN
         INFO = -6
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGBTF2', -INFO )
         RETURN
      END IF
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
      DO 20 J = KU + 2, MIN( KV, N )
         DO 10 I = KV - J + 2, KL
            AB( I, J ) = ZERO
   10    CONTINUE
   20 CONTINUE
      JU = 1
      DO 40 J = 1, MIN( M, N )
         IF( J+KV.LE.N ) THEN
            DO 30 I = 1, KL
               AB( I, J+KV ) = ZERO
   30       CONTINUE
         END IF
         KM = MIN( KL, M-J )
         JP = IDAMAX( KM+1, AB( KV+1, J ), 1 )
         IPIV( J ) = JP + J - 1
         IF( AB( KV+JP, J ).NE.ZERO ) THEN
            JU = MAX( JU, MIN( J+KU+JP-1, N ) )
            IF( JP.NE.1 )
     $         CALL DSWAP( JU-J+1, AB( KV+JP, J ), LDAB-1,
     $                     AB( KV+1, J ), LDAB-1 )
            IF( KM.GT.0 ) THEN
               CALL DSCAL( KM, ONE / AB( KV+1, J ), AB( KV+2, J ), 1 )
               IF( JU.GT.J )
     $            CALL DGER( KM, JU-J, -ONE, AB( KV+2, J ), 1,
     $                       AB( KV, J+1 ), LDAB-1, AB( KV+1, J+1 ),
     $                       LDAB-1 )
            END IF
         ELSE
            IF( INFO.EQ.0 )
     $         INFO = J
         END IF
   40 CONTINUE
      RETURN
      END
      SUBROUTINE DGER(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)
      DOUBLE PRECISION ALPHA
      INTEGER INCX,INCY,LDA,M,N
      DOUBLE PRECISION A(LDA,*),X(*),Y(*)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D+0)
      DOUBLE PRECISION TEMP
      INTEGER I,INFO,IX,J,JY,KX
      EXTERNAL XERBLA
      INTRINSIC MAX
      INFO = 0
      IF (M.LT.0) THEN
          INFO = 1
      ELSE IF (N.LT.0) THEN
          INFO = 2
      ELSE IF (INCX.EQ.0) THEN
          INFO = 5
      ELSE IF (INCY.EQ.0) THEN
          INFO = 7
      ELSE IF (LDA.LT.MAX(1,M)) THEN
          INFO = 9
      END IF
      IF (INFO.NE.0) THEN
          CALL XERBLA('DGER  ',INFO)
          RETURN
      END IF
      IF ((M.EQ.0) .OR. (N.EQ.0) .OR. (ALPHA.EQ.ZERO)) RETURN
      IF (INCY.GT.0) THEN
          JY = 1
      ELSE
          JY = 1 - (N-1)*INCY
      END IF
      IF (INCX.EQ.1) THEN
          DO 20 J = 1,N
              IF (Y(JY).NE.ZERO) THEN
                  TEMP = ALPHA*Y(JY)
                  DO 10 I = 1,M
                      A(I,J) = A(I,J) + X(I)*TEMP
   10             CONTINUE
              END IF
              JY = JY + INCY
   20     CONTINUE
      ELSE
          IF (INCX.GT.0) THEN
              KX = 1
          ELSE
              KX = 1 - (M-1)*INCX
          END IF
          DO 40 J = 1,N
              IF (Y(JY).NE.ZERO) THEN
                  TEMP = ALPHA*Y(JY)
                  IX = KX
                  DO 30 I = 1,M
                      A(I,J) = A(I,J) + X(IX)*TEMP
                      IX = IX + INCX
   30             CONTINUE
              END IF
              JY = JY + INCY
   40     CONTINUE
      END IF
      RETURN
      END
      SUBROUTINE DLASWP( N, A, LDA, K1, K2, IPIV, INCX )
      INTEGER            INCX, K1, K2, LDA, N
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * )
      INTEGER            I, I1, I2, INC, IP, IX, IX0, J, K, N32
      DOUBLE PRECISION   TEMP
      IF( INCX.GT.0 ) THEN
         IX0 = K1
         I1 = K1
         I2 = K2
         INC = 1
      ELSE IF( INCX.LT.0 ) THEN
         IX0 = K1 + ( K1-K2 )*INCX
         I1 = K2
         I2 = K1
         INC = -1
      ELSE
         RETURN
      END IF
      N32 = ( N / 32 )*32
      IF( N32.NE.0 ) THEN
         DO 30 J = 1, N32, 32
            IX = IX0
            DO 20 I = I1, I2, INC
               IP = IPIV( IX )
               IF( IP.NE.I ) THEN
                  DO 10 K = J, J + 31
                     TEMP = A( I, K )
                     A( I, K ) = A( IP, K )
                     A( IP, K ) = TEMP
   10             CONTINUE
               END IF
               IX = IX + INCX
   20       CONTINUE
   30    CONTINUE
      END IF
      IF( N32.NE.N ) THEN
         N32 = N32 + 1
         IX = IX0
         DO 50 I = I1, I2, INC
            IP = IPIV( IX )
            IF( IP.NE.I ) THEN
               DO 40 K = N32, N
                  TEMP = A( I, K )
                  A( I, K ) = A( IP, K )
                  A( IP, K ) = TEMP
   40          CONTINUE
            END IF
            IX = IX + INCX
   50    CONTINUE
      END IF
      RETURN
      END
      SUBROUTINE DGETF2( M, N, A, LDA, IPIV, INFO )
      INTEGER            INFO, LDA, M, N
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * )
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      DOUBLE PRECISION   SFMIN
      INTEGER            I, J, JP
      DOUBLE PRECISION   DLAMCH
      INTEGER            IDAMAX
      EXTERNAL           DLAMCH, IDAMAX
      EXTERNAL           DGER, DSCAL, DSWAP, XERBLA
      INTRINSIC          MAX, MIN
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGETF2', -INFO )
         RETURN
      END IF
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
      SFMIN = DLAMCH('S')
      DO 10 J = 1, MIN( M, N )
         JP = J - 1 + IDAMAX( M-J+1, A( J, J ), 1 )
         IPIV( J ) = JP
         IF( A( JP, J ).NE.ZERO ) THEN
            IF( JP.NE.J )
     $         CALL DSWAP( N, A( J, 1 ), LDA, A( JP, 1 ), LDA )
            IF( J.LT.M ) THEN
               IF( ABS(A( J, J )) .GE. SFMIN ) THEN
                  CALL DSCAL( M-J, ONE / A( J, J ), A( J+1, J ), 1 )
               ELSE
                 DO 20 I = 1, M-J
                    A( J+I, J ) = A( J+I, J ) / A( J, J )
   20            CONTINUE
               END IF
            END IF
         ELSE IF( INFO.EQ.0 ) THEN
            INFO = J
         END IF
         IF( J.LT.MIN( M, N ) ) THEN
            CALL DGER( M-J, N-J, -ONE, A( J+1, J ), 1, A( J, J+1 ), LDA,
     $                 A( J+1, J+1 ), LDA )
         END IF
   10 CONTINUE
      RETURN
      END
      SUBROUTINE DTBSV(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX)
      INTEGER INCX,K,LDA,N
      CHARACTER DIAG,TRANS,UPLO
      DOUBLE PRECISION A(LDA,*),X(*)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D+0)
      DOUBLE PRECISION TEMP
      INTEGER I,INFO,IX,J,JX,KPLUS1,KX,L
      LOGICAL NOUNIT
      LOGICAL LSAME
      EXTERNAL LSAME
      EXTERNAL XERBLA
      INTRINSIC MAX,MIN
      INFO = 0
      IF (.NOT.LSAME(UPLO,'U') .AND. .NOT.LSAME(UPLO,'L')) THEN
          INFO = 1
      ELSE IF (.NOT.LSAME(TRANS,'N') .AND. .NOT.LSAME(TRANS,'T') .AND.
     +         .NOT.LSAME(TRANS,'C')) THEN
          INFO = 2
      ELSE IF (.NOT.LSAME(DIAG,'U') .AND. .NOT.LSAME(DIAG,'N')) THEN
          INFO = 3
      ELSE IF (N.LT.0) THEN
          INFO = 4
      ELSE IF (K.LT.0) THEN
          INFO = 5
      ELSE IF (LDA.LT. (K+1)) THEN
          INFO = 7
      ELSE IF (INCX.EQ.0) THEN
          INFO = 9
      END IF
      IF (INFO.NE.0) THEN
          CALL XERBLA('DTBSV ',INFO)
          RETURN
      END IF
      IF (N.EQ.0) RETURN
      NOUNIT = LSAME(DIAG,'N')
      IF (INCX.LE.0) THEN
          KX = 1 - (N-1)*INCX
      ELSE IF (INCX.NE.1) THEN
          KX = 1
      END IF
      IF (LSAME(TRANS,'N')) THEN
          IF (LSAME(UPLO,'U')) THEN
              KPLUS1 = K + 1
              IF (INCX.EQ.1) THEN
                  DO 20 J = N,1,-1
                      IF (X(J).NE.ZERO) THEN
                          L = KPLUS1 - J
                          IF (NOUNIT) X(J) = X(J)/A(KPLUS1,J)
                          TEMP = X(J)
                          DO 10 I = J - 1,MAX(1,J-K),-1
                              X(I) = X(I) - TEMP*A(L+I,J)
   10                     CONTINUE
                      END IF
   20             CONTINUE
              ELSE
                  KX = KX + (N-1)*INCX
                  JX = KX
                  DO 40 J = N,1,-1
                      KX = KX - INCX
                      IF (X(JX).NE.ZERO) THEN
                          IX = KX
                          L = KPLUS1 - J
                          IF (NOUNIT) X(JX) = X(JX)/A(KPLUS1,J)
                          TEMP = X(JX)
                          DO 30 I = J - 1,MAX(1,J-K),-1
                              X(IX) = X(IX) - TEMP*A(L+I,J)
                              IX = IX - INCX
   30                     CONTINUE
                      END IF
                      JX = JX - INCX
   40             CONTINUE
              END IF
          ELSE
              IF (INCX.EQ.1) THEN
                  DO 60 J = 1,N
                      IF (X(J).NE.ZERO) THEN
                          L = 1 - J
                          IF (NOUNIT) X(J) = X(J)/A(1,J)
                          TEMP = X(J)
                          DO 50 I = J + 1,MIN(N,J+K)
                              X(I) = X(I) - TEMP*A(L+I,J)
   50                     CONTINUE
                      END IF
   60             CONTINUE
              ELSE
                  JX = KX
                  DO 80 J = 1,N
                      KX = KX + INCX
                      IF (X(JX).NE.ZERO) THEN
                          IX = KX
                          L = 1 - J
                          IF (NOUNIT) X(JX) = X(JX)/A(1,J)
                          TEMP = X(JX)
                          DO 70 I = J + 1,MIN(N,J+K)
                              X(IX) = X(IX) - TEMP*A(L+I,J)
                              IX = IX + INCX
   70                     CONTINUE
                      END IF
                      JX = JX + INCX
   80             CONTINUE
              END IF
          END IF
      ELSE
          IF (LSAME(UPLO,'U')) THEN
              KPLUS1 = K + 1
              IF (INCX.EQ.1) THEN
                  DO 100 J = 1,N
                      TEMP = X(J)
                      L = KPLUS1 - J
                      DO 90 I = MAX(1,J-K),J - 1
                          TEMP = TEMP - A(L+I,J)*X(I)
   90                 CONTINUE
                      IF (NOUNIT) TEMP = TEMP/A(KPLUS1,J)
                      X(J) = TEMP
  100             CONTINUE
              ELSE
                  JX = KX
                  DO 120 J = 1,N
                      TEMP = X(JX)
                      IX = KX
                      L = KPLUS1 - J
                      DO 110 I = MAX(1,J-K),J - 1
                          TEMP = TEMP - A(L+I,J)*X(IX)
                          IX = IX + INCX
  110                 CONTINUE
                      IF (NOUNIT) TEMP = TEMP/A(KPLUS1,J)
                      X(JX) = TEMP
                      JX = JX + INCX
                      IF (J.GT.K) KX = KX + INCX
  120             CONTINUE
              END IF
          ELSE
              IF (INCX.EQ.1) THEN
                  DO 140 J = N,1,-1
                      TEMP = X(J)
                      L = 1 - J
                      DO 130 I = MIN(N,J+K),J + 1,-1
                          TEMP = TEMP - A(L+I,J)*X(I)
  130                 CONTINUE
                      IF (NOUNIT) TEMP = TEMP/A(1,J)
                      X(J) = TEMP
  140             CONTINUE
              ELSE
                  KX = KX + (N-1)*INCX
                  JX = KX
                  DO 160 J = N,1,-1
                      TEMP = X(JX)
                      IX = KX
                      L = 1 - J
                      DO 150 I = MIN(N,J+K),J + 1,-1
                          TEMP = TEMP - A(L+I,J)*X(IX)
                          IX = IX - INCX
  150                 CONTINUE
                      IF (NOUNIT) TEMP = TEMP/A(1,J)
                      X(JX) = TEMP
                      JX = JX - INCX
                      IF ((N-J).GE.K) KX = KX - INCX
  160             CONTINUE
              END IF
          END IF
      END IF
      RETURN
      END
      RECURSIVE SUBROUTINE DPOTRF2( UPLO, N, A, LDA, INFO )
      CHARACTER          UPLO
      INTEGER            INFO, LDA, N
      DOUBLE PRECISION   A( LDA, * )
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      LOGICAL            UPPER
      INTEGER            N1, N2, IINFO
      LOGICAL            LSAME, DISNAN
      EXTERNAL           LSAME, DISNAN
      EXTERNAL           DSYRK, DTRSM, XERBLA
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
         CALL XERBLA( 'DPOTRF2', -INFO )
         RETURN
      END IF
      IF( N.EQ.0 )
     $   RETURN
      IF( N.EQ.1 ) THEN
         IF( A( 1, 1 ).LE.ZERO.OR.DISNAN( A( 1, 1 ) ) ) THEN
            INFO = 1
            RETURN
         END IF
         A( 1, 1 ) = SQRT( A( 1, 1 ) )
      ELSE
         N1 = N/2
         N2 = N-N1
         CALL DPOTRF2( UPLO, N1, A( 1, 1 ), LDA, IINFO )
         IF ( IINFO.NE.0 ) THEN
            INFO = IINFO
            RETURN
         END IF
         IF( UPPER ) THEN
            CALL DTRSM( 'L', 'U', 'T', 'N', N1, N2, ONE,
     $                  A( 1, 1 ), LDA, A( 1, N1+1 ), LDA )
            CALL DSYRK( UPLO, 'T', N2, N1, -ONE, A( 1, N1+1 ), LDA,
     $                  ONE, A( N1+1, N1+1 ), LDA )
            CALL DPOTRF2( UPLO, N2, A( N1+1, N1+1 ), LDA, IINFO )
            IF ( IINFO.NE.0 ) THEN
               INFO = IINFO + N1
               RETURN
            END IF
         ELSE
            CALL DTRSM( 'R', 'L', 'T', 'N', N2, N1, ONE,
     $                  A( 1, 1 ), LDA, A( N1+1, 1 ), LDA )
            CALL DSYRK( UPLO, 'N', N2, N1, -ONE, A( N1+1, 1 ), LDA,
     $                  ONE, A( N1+1, N1+1 ), LDA )
            CALL DPOTRF2( UPLO, N2, A( N1+1, N1+1 ), LDA, IINFO )
            IF ( IINFO.NE.0 ) THEN
               INFO = IINFO + N1
               RETURN
            END IF
         END IF
      END IF
      RETURN
      END
      LOGICAL FUNCTION DISNAN( DIN )
      DOUBLE PRECISION, INTENT(IN) :: DIN
      LOGICAL DLAISNAN
      EXTERNAL DLAISNAN
      DISNAN = DLAISNAN(DIN,DIN)
      RETURN
      END
      LOGICAL FUNCTION DLAISNAN( DIN1, DIN2 )
      DOUBLE PRECISION, INTENT(IN) :: DIN1, DIN2
      DLAISNAN = (DIN1.NE.DIN2)
      RETURN
      END
! cholesky patch
      SUBROUTINE DSYEVD( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, IWORK,
     $                   LIWORK, INFO )
      CHARACTER          JOBZ, UPLO
      INTEGER            INFO, LDA, LIWORK, LWORK, N
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), W( * ), WORK( * )
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      LOGICAL            LOWER, LQUERY, WANTZ
      INTEGER            IINFO, INDE, INDTAU, INDWK2, INDWRK, ISCALE,
     $                   LIOPT, LIWMIN, LLWORK, LLWRK2, LOPT, LWMIN
      DOUBLE PRECISION   ANRM, BIGNUM, EPS, RMAX, RMIN, SAFMIN, SIGMA,
     $                   SMLNUM
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANSY
      EXTERNAL           LSAME, DLAMCH, DLANSY, ILAENV
      EXTERNAL           DLACPY, DLASCL, DORMTR, DSCAL, DSTEDC, DSTERF,
     $                   DSYTRD, XERBLA
      INTRINSIC          MAX, SQRT
      WANTZ = LSAME( JOBZ, 'V' )
      LOWER = LSAME( UPLO, 'L' )
      LQUERY = ( LWORK.EQ.-1 .OR. LIWORK.EQ.-1 )
      INFO = 0
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( LOWER .OR. LSAME( UPLO, 'U' ) ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      END IF
      IF( INFO.EQ.0 ) THEN
         IF( N.LE.1 ) THEN
            LIWMIN = 1
            LWMIN = 1
            LOPT = LWMIN
            LIOPT = LIWMIN
         ELSE
            IF( WANTZ ) THEN
               LIWMIN = 3 + 5*N
               LWMIN = 1 + 6*N + 2*N**2
            ELSE
               LIWMIN = 1
               LWMIN = 2*N + 1
            END IF
            LOPT = MAX( LWMIN, 2*N +
     $                  ILAENV( 1, 'DSYTRD', UPLO, N, -1, -1, -1 ) )
            LIOPT = LIWMIN
         END IF
         WORK( 1 ) = LOPT
         IWORK( 1 ) = LIOPT
         IF( LWORK.LT.LWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -8
         ELSE IF( LIWORK.LT.LIWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -10
         END IF
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSYEVD', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
      IF( N.EQ.0 )
     $   RETURN
      IF( N.EQ.1 ) THEN
         W( 1 ) = A( 1, 1 )
         IF( WANTZ )
     $      A( 1, 1 ) = ONE
         RETURN
      END IF
      SAFMIN = DLAMCH( 'Safe minimum' )
      EPS = DLAMCH( 'Precision' )
      SMLNUM = SAFMIN / EPS
      BIGNUM = ONE / SMLNUM
      RMIN = SQRT( SMLNUM )
      RMAX = SQRT( BIGNUM )
      ANRM = DLANSY( 'M', UPLO, N, A, LDA, WORK )
      ISCALE = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.RMIN ) THEN
         ISCALE = 1
         SIGMA = RMIN / ANRM
      ELSE IF( ANRM.GT.RMAX ) THEN
         ISCALE = 1
         SIGMA = RMAX / ANRM
      END IF
      IF( ISCALE.EQ.1 )
     $   CALL DLASCL( UPLO, 0, 0, ONE, SIGMA, N, N, A, LDA, INFO )
      INDE = 1
      INDTAU = INDE + N
      INDWRK = INDTAU + N
      LLWORK = LWORK - INDWRK + 1
      INDWK2 = INDWRK + N*N
      LLWRK2 = LWORK - INDWK2 + 1
      CALL DSYTRD( UPLO, N, A, LDA, W, WORK( INDE ), WORK( INDTAU ),
     $             WORK( INDWRK ), LLWORK, IINFO )
      IF( .NOT.WANTZ ) THEN
         CALL DSTERF( N, W, WORK( INDE ), INFO )
      ELSE
         CALL DSTEDC( 'I', N, W, WORK( INDE ), WORK( INDWRK ), N,
     $                WORK( INDWK2 ), LLWRK2, IWORK, LIWORK, INFO )
         CALL DORMTR( 'L', UPLO, 'N', N, N, A, LDA, WORK( INDTAU ),
     $                WORK( INDWRK ), N, WORK( INDWK2 ), LLWRK2, IINFO )
         CALL DLACPY( 'A', N, N, WORK( INDWRK ), N, A, LDA )
      END IF
      IF( ISCALE.EQ.1 )
     $   CALL DSCAL( N, ONE / SIGMA, W, 1 )
      WORK( 1 ) = LOPT
      IWORK( 1 ) = LIOPT
      RETURN
      END
      DOUBLE PRECISION FUNCTION DLANSY( NORM, UPLO, N, A, LDA, WORK )
      IMPLICIT NONE
      CHARACTER          NORM, UPLO
      INTEGER            LDA, N
      DOUBLE PRECISION   A( LDA, * ), WORK( * )
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      INTEGER            I, J
      DOUBLE PRECISION   ABSA, SUM, VALUE
      DOUBLE PRECISION   SSQ( 2 ), COLSSQ( 2 )
      LOGICAL            LSAME, DISNAN
      EXTERNAL           LSAME, DISNAN
      EXTERNAL           DLASSQ, DCOMBSSQ
      INTRINSIC          ABS, SQRT
      IF( N.EQ.0 ) THEN
         VALUE = ZERO
      ELSE IF( LSAME( NORM, 'M' ) ) THEN
         VALUE = ZERO
         IF( LSAME( UPLO, 'U' ) ) THEN
            DO 20 J = 1, N
               DO 10 I = 1, J
                  SUM = ABS( A( I, J ) )
                  IF( VALUE .LT. SUM .OR. DISNAN( SUM ) ) VALUE = SUM
   10          CONTINUE
   20       CONTINUE
         ELSE
            DO 40 J = 1, N
               DO 30 I = J, N
                  SUM = ABS( A( I, J ) )
                  IF( VALUE .LT. SUM .OR. DISNAN( SUM ) ) VALUE = SUM
   30          CONTINUE
   40       CONTINUE
         END IF
      ELSE IF( ( LSAME( NORM, 'I' ) ) .OR. ( LSAME( NORM, 'O' ) ) .OR.
     $         ( NORM.EQ.'1' ) ) THEN
         VALUE = ZERO
         IF( LSAME( UPLO, 'U' ) ) THEN
            DO 60 J = 1, N
               SUM = ZERO
               DO 50 I = 1, J - 1
                  ABSA = ABS( A( I, J ) )
                  SUM = SUM + ABSA
                  WORK( I ) = WORK( I ) + ABSA
   50          CONTINUE
               WORK( J ) = SUM + ABS( A( J, J ) )
   60       CONTINUE
            DO 70 I = 1, N
               SUM = WORK( I )
               IF( VALUE .LT. SUM .OR. DISNAN( SUM ) ) VALUE = SUM
   70       CONTINUE
         ELSE
            DO 80 I = 1, N
               WORK( I ) = ZERO
   80       CONTINUE
            DO 100 J = 1, N
               SUM = WORK( J ) + ABS( A( J, J ) )
               DO 90 I = J + 1, N
                  ABSA = ABS( A( I, J ) )
                  SUM = SUM + ABSA
                  WORK( I ) = WORK( I ) + ABSA
   90          CONTINUE
               IF( VALUE .LT. SUM .OR. DISNAN( SUM ) ) VALUE = SUM
  100       CONTINUE
         END IF
      ELSE IF( ( LSAME( NORM, 'F' ) ) .OR. ( LSAME( NORM, 'E' ) ) ) THEN
         SSQ( 1 ) = ZERO
         SSQ( 2 ) = ONE
         IF( LSAME( UPLO, 'U' ) ) THEN
            DO 110 J = 2, N
               COLSSQ( 1 ) = ZERO
               COLSSQ( 2 ) = ONE
               CALL DLASSQ( J-1, A( 1, J ), 1, COLSSQ(1), COLSSQ(2) )
               CALL DCOMBSSQ( SSQ, COLSSQ )
  110       CONTINUE
         ELSE
            DO 120 J = 1, N - 1
               COLSSQ( 1 ) = ZERO
               COLSSQ( 2 ) = ONE
               CALL DLASSQ( N-J, A( J+1, J ), 1, COLSSQ(1), COLSSQ(2) )
               CALL DCOMBSSQ( SSQ, COLSSQ )
  120       CONTINUE
         END IF
         SSQ( 2 ) = 2*SSQ( 2 )
         COLSSQ( 1 ) = ZERO
         COLSSQ( 2 ) = ONE
         CALL DLASSQ( N, A, LDA+1, COLSSQ( 1 ), COLSSQ( 2 ) )
         CALL DCOMBSSQ( SSQ, COLSSQ )
         VALUE = SSQ( 1 )*SQRT( SSQ( 2 ) )
      END IF
      DLANSY = VALUE
      RETURN
      END
      SUBROUTINE DLASSQ( N, X, INCX, SCALE, SUMSQ )
      INTEGER            INCX, N
      DOUBLE PRECISION   SCALE, SUMSQ
      DOUBLE PRECISION   X( * )
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
      INTEGER            IX
      DOUBLE PRECISION   ABSXI
      LOGICAL            DISNAN
      EXTERNAL           DISNAN
      INTRINSIC          ABS
      IF( N.GT.0 ) THEN
         DO 10 IX = 1, 1 + ( N-1 )*INCX, INCX
            ABSXI = ABS( X( IX ) )
            IF( ABSXI.GT.ZERO.OR.DISNAN( ABSXI ) ) THEN
               IF( SCALE.LT.ABSXI ) THEN
                  SUMSQ = 1 + SUMSQ*( SCALE / ABSXI )**2
                  SCALE = ABSXI
               ELSE
                  SUMSQ = SUMSQ + ( ABSXI / SCALE )**2
               END IF
            END IF
   10    CONTINUE
      END IF
      RETURN
      END
      SUBROUTINE DCOMBSSQ( V1, V2 )
      DOUBLE PRECISION   V1( 2 ), V2( 2 )
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
      IF( V2( 2 ).EQ.ZERO ) RETURN
      IF( V1( 1 ).GE.V2( 1 ) ) THEN
         IF( V1( 1 ).NE.ZERO ) THEN
            V1( 2 ) = V1( 2 ) + ( V2( 1 ) / V1( 1 ) )**2 * V2( 2 )
         ELSE
            V1( 2 ) = V1( 2 ) + V2( 2 )
         END IF
      ELSE
         V1( 2 ) = V2( 2 ) + ( V1( 1 ) / V2( 1 ) )**2 * V1( 2 )
         V1( 1 ) = V2( 1 )
      END IF
      RETURN
      END
      SUBROUTINE DLASCL( TYPE, KL, KU, CFROM, CTO, M, N, A, LDA, INFO )
      CHARACTER          TYPE
      INTEGER            INFO, KL, KU, LDA, M, N
      DOUBLE PRECISION   CFROM, CTO
      DOUBLE PRECISION   A( LDA, * )
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
      LOGICAL            DONE
      INTEGER            I, ITYPE, J, K1, K2, K3, K4
      DOUBLE PRECISION   BIGNUM, CFROM1, CFROMC, CTO1, CTOC, MUL, SMLNUM
      LOGICAL            LSAME, DISNAN
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, DLAMCH, DISNAN
      INTRINSIC          ABS, MAX, MIN
      EXTERNAL           XERBLA
      INFO = 0
      IF( LSAME( TYPE, 'G' ) ) THEN
         ITYPE = 0
      ELSE IF( LSAME( TYPE, 'L' ) ) THEN
         ITYPE = 1
      ELSE IF( LSAME( TYPE, 'U' ) ) THEN
         ITYPE = 2
      ELSE IF( LSAME( TYPE, 'H' ) ) THEN
         ITYPE = 3
      ELSE IF( LSAME( TYPE, 'B' ) ) THEN
         ITYPE = 4
      ELSE IF( LSAME( TYPE, 'Q' ) ) THEN
         ITYPE = 5
      ELSE IF( LSAME( TYPE, 'Z' ) ) THEN
         ITYPE = 6
      ELSE
         ITYPE = -1
      END IF
      IF( ITYPE.EQ.-1 ) THEN
         INFO = -1
      ELSE IF( CFROM.EQ.ZERO .OR. DISNAN(CFROM) ) THEN
         INFO = -4
      ELSE IF( DISNAN(CTO) ) THEN
         INFO = -5
      ELSE IF( M.LT.0 ) THEN
         INFO = -6
      ELSE IF( N.LT.0 .OR. ( ITYPE.EQ.4 .AND. N.NE.M ) .OR.
     $         ( ITYPE.EQ.5 .AND. N.NE.M ) ) THEN
         INFO = -7
      ELSE IF( ITYPE.LE.3 .AND. LDA.LT.MAX( 1, M ) ) THEN
         INFO = -9
      ELSE IF( ITYPE.GE.4 ) THEN
         IF( KL.LT.0 .OR. KL.GT.MAX( M-1, 0 ) ) THEN
            INFO = -2
         ELSE IF( KU.LT.0 .OR. KU.GT.MAX( N-1, 0 ) .OR.
     $            ( ( ITYPE.EQ.4 .OR. ITYPE.EQ.5 ) .AND. KL.NE.KU ) )
     $             THEN
            INFO = -3
         ELSE IF( ( ITYPE.EQ.4 .AND. LDA.LT.KL+1 ) .OR.
     $            ( ITYPE.EQ.5 .AND. LDA.LT.KU+1 ) .OR.
     $            ( ITYPE.EQ.6 .AND. LDA.LT.2*KL+KU+1 ) ) THEN
            INFO = -9
         END IF
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASCL', -INFO )
         RETURN
      END IF
      IF( N.EQ.0 .OR. M.EQ.0 )
     $   RETURN
      SMLNUM = DLAMCH( 'S' )
      BIGNUM = ONE / SMLNUM
      CFROMC = CFROM
      CTOC = CTO
   10 CONTINUE
      CFROM1 = CFROMC*SMLNUM
      IF( CFROM1.EQ.CFROMC ) THEN
!        CFROMC is an inf.  Multiply by a correctly signed zero for
!        finite CTOC, or a NaN if CTOC is infinite.
         MUL = CTOC / CFROMC
         DONE = .TRUE.
         CTO1 = CTOC
      ELSE
         CTO1 = CTOC / BIGNUM
         IF( CTO1.EQ.CTOC ) THEN
!           CTOC is either 0 or an inf.  In both cases, CTOC itself
!           serves as the correct multiplication factor.
            MUL = CTOC
            DONE = .TRUE.
            CFROMC = ONE
         ELSE IF( ABS( CFROM1 ).GT.ABS( CTOC ) .AND. CTOC.NE.ZERO ) THEN
            MUL = SMLNUM
            DONE = .FALSE.
            CFROMC = CFROM1
         ELSE IF( ABS( CTO1 ).GT.ABS( CFROMC ) ) THEN
            MUL = BIGNUM
            DONE = .FALSE.
            CTOC = CTO1
         ELSE
            MUL = CTOC / CFROMC
            DONE = .TRUE.
         END IF
      END IF
      IF( ITYPE.EQ.0 ) THEN
         DO 30 J = 1, N
            DO 20 I = 1, M
               A( I, J ) = A( I, J )*MUL
   20       CONTINUE
   30    CONTINUE
      ELSE IF( ITYPE.EQ.1 ) THEN
         DO 50 J = 1, N
            DO 40 I = J, M
               A( I, J ) = A( I, J )*MUL
   40       CONTINUE
   50    CONTINUE
      ELSE IF( ITYPE.EQ.2 ) THEN
         DO 70 J = 1, N
            DO 60 I = 1, MIN( J, M )
               A( I, J ) = A( I, J )*MUL
   60       CONTINUE
   70    CONTINUE
      ELSE IF( ITYPE.EQ.3 ) THEN
         DO 90 J = 1, N
            DO 80 I = 1, MIN( J+1, M )
               A( I, J ) = A( I, J )*MUL
   80       CONTINUE
   90    CONTINUE
      ELSE IF( ITYPE.EQ.4 ) THEN
         K3 = KL + 1
         K4 = N + 1
         DO 110 J = 1, N
            DO 100 I = 1, MIN( K3, K4-J )
               A( I, J ) = A( I, J )*MUL
  100       CONTINUE
  110    CONTINUE
      ELSE IF( ITYPE.EQ.5 ) THEN
         K1 = KU + 2
         K3 = KU + 1
         DO 130 J = 1, N
            DO 120 I = MAX( K1-J, 1 ), K3
               A( I, J ) = A( I, J )*MUL
  120       CONTINUE
  130    CONTINUE
      ELSE IF( ITYPE.EQ.6 ) THEN
         K1 = KL + KU + 2
         K2 = KL + 1
         K3 = 2*KL + KU + 1
         K4 = KL + KU + 1 + M
         DO 150 J = 1, N
            DO 140 I = MAX( K1-J, K2 ), MIN( K3, K4-J )
               A( I, J ) = A( I, J )*MUL
  140       CONTINUE
  150    CONTINUE
      END IF
      IF( .NOT.DONE )
     $   GO TO 10
      RETURN
      END
      SUBROUTINE DSYTRD( UPLO, N, A, LDA, D, E, TAU, WORK, LWORK, INFO )
      CHARACTER          UPLO
      INTEGER            INFO, LDA, LWORK, N
      DOUBLE PRECISION   A( LDA, * ), D( * ), E( * ), TAU( * ),
     $                   WORK( * )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
      LOGICAL            LQUERY, UPPER
      INTEGER            I, IINFO, IWS, J, KK, LDWORK, LWKOPT, NB,
     $                   NBMIN, NX
      EXTERNAL           DLATRD, DSYR2K, DSYTD2, XERBLA
      INTRINSIC          MAX
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      LQUERY = ( LWORK.EQ.-1 )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      ELSE IF( LWORK.LT.1 .AND. .NOT.LQUERY ) THEN
         INFO = -9
      END IF
      IF( INFO.EQ.0 ) THEN
         NB = ILAENV( 1, 'DSYTRD', UPLO, N, -1, -1, -1 )
         LWKOPT = N*NB
         WORK( 1 ) = LWKOPT
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSYTRD', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
      IF( N.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
      NX = N
      IWS = 1
      IF( NB.GT.1 .AND. NB.LT.N ) THEN
         NX = MAX( NB, ILAENV( 3, 'DSYTRD', UPLO, N, -1, -1, -1 ) )
         IF( NX.LT.N ) THEN
            LDWORK = N
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
               NB = MAX( LWORK / LDWORK, 1 )
               NBMIN = ILAENV( 2, 'DSYTRD', UPLO, N, -1, -1, -1 )
               IF( NB.LT.NBMIN )
     $            NX = N
            END IF
         ELSE
            NX = N
         END IF
      ELSE
         NB = 1
      END IF
      IF( UPPER ) THEN
         KK = N - ( ( N-NX+NB-1 ) / NB )*NB
         DO 20 I = N - NB + 1, KK + 1, -NB
            CALL DLATRD( UPLO, I+NB-1, NB, A, LDA, E, TAU, WORK,
     $                   LDWORK )
            CALL DSYR2K( UPLO, 'No transpose', I-1, NB, -ONE, A( 1, I ),
     $                   LDA, WORK, LDWORK, ONE, A, LDA )
            DO 10 J = I, I + NB - 1
               A( J-1, J ) = E( J-1 )
               D( J ) = A( J, J )
   10       CONTINUE
   20    CONTINUE
         CALL DSYTD2( UPLO, KK, A, LDA, D, E, TAU, IINFO )
      ELSE
         DO 40 I = 1, N - NX, NB
            CALL DLATRD( UPLO, N-I+1, NB, A( I, I ), LDA, E( I ),
     $                   TAU( I ), WORK, LDWORK )
            CALL DSYR2K( UPLO, 'No transpose', N-I-NB+1, NB, -ONE,
     $                   A( I+NB, I ), LDA, WORK( NB+1 ), LDWORK, ONE,
     $                   A( I+NB, I+NB ), LDA )
            DO 30 J = I, I + NB - 1
               A( J+1, J ) = E( J )
               D( J ) = A( J, J )
   30       CONTINUE
   40    CONTINUE
         CALL DSYTD2( UPLO, N-I+1, A( I, I ), LDA, D( I ), E( I ),
     $                TAU( I ), IINFO )
      END IF
      WORK( 1 ) = LWKOPT
      RETURN
      END
      SUBROUTINE DSTERF( N, D, E, INFO )
      INTEGER            INFO, N
      DOUBLE PRECISION   D( * ), E( * )
      DOUBLE PRECISION   ZERO, ONE, TWO, THREE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                   THREE = 3.0D0 )
      INTEGER            MAXIT
      PARAMETER          ( MAXIT = 30 )
      INTEGER            I, ISCALE, JTOT, L, L1, LEND, LENDSV, LSV, M,
     $                   NMAXIT
      DOUBLE PRECISION   ALPHA, ANORM, BB, C, EPS, EPS2, GAMMA, OLDC,
     $                   OLDGAM, P, R, RT1, RT2, RTE, S, SAFMAX, SAFMIN,
     $                   SIGMA, SSFMAX, SSFMIN, RMAX
      DOUBLE PRECISION   DLAMCH, DLANST, DLAPY2
      EXTERNAL           DLAMCH, DLANST, DLAPY2
      EXTERNAL           DLAE2, DLASCL, DLASRT, XERBLA
      INTRINSIC          ABS, SIGN, SQRT
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -1
         CALL XERBLA( 'DSTERF', -INFO )
         RETURN
      END IF
      IF( N.LE.1 )
     $   RETURN
      EPS = DLAMCH( 'E' )
      EPS2 = EPS**2
      SAFMIN = DLAMCH( 'S' )
      SAFMAX = ONE / SAFMIN
      SSFMAX = SQRT( SAFMAX ) / THREE
      SSFMIN = SQRT( SAFMIN ) / EPS2
      RMAX = DLAMCH( 'O' )
      NMAXIT = N*MAXIT
      SIGMA = ZERO
      JTOT = 0
      L1 = 1
   10 CONTINUE
      IF( L1.GT.N )
     $   GO TO 170
      IF( L1.GT.1 )
     $   E( L1-1 ) = ZERO
      DO 20 M = L1, N - 1
         IF( ABS( E( M ) ).LE.( SQRT( ABS( D( M ) ) )*SQRT( ABS( D( M+
     $       1 ) ) ) )*EPS ) THEN
            E( M ) = ZERO
            GO TO 30
         END IF
   20 CONTINUE
      M = N
   30 CONTINUE
      L = L1
      LSV = L
      LEND = M
      LENDSV = LEND
      L1 = M + 1
      IF( LEND.EQ.L )
     $   GO TO 10
      ANORM = DLANST( 'M', LEND-L+1, D( L ), E( L ) )
      ISCALE = 0
      IF( ANORM.EQ.ZERO )
     $   GO TO 10
      IF( (ANORM.GT.SSFMAX) ) THEN
         ISCALE = 1
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMAX, LEND-L+1, 1, D( L ), N,
     $                INFO )
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMAX, LEND-L, 1, E( L ), N,
     $                INFO )
      ELSE IF( ANORM.LT.SSFMIN ) THEN
         ISCALE = 2
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMIN, LEND-L+1, 1, D( L ), N,
     $                INFO )
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMIN, LEND-L, 1, E( L ), N,
     $                INFO )
      END IF
      DO 40 I = L, LEND - 1
         E( I ) = E( I )**2
   40 CONTINUE
      IF( ABS( D( LEND ) ).LT.ABS( D( L ) ) ) THEN
         LEND = LSV
         L = LENDSV
      END IF
      IF( LEND.GE.L ) THEN
   50    CONTINUE
         IF( L.NE.LEND ) THEN
            DO 60 M = L, LEND - 1
               IF( ABS( E( M ) ).LE.EPS2*ABS( D( M )*D( M+1 ) ) )
     $            GO TO 70
   60       CONTINUE
         END IF
         M = LEND
   70    CONTINUE
         IF( M.LT.LEND )
     $      E( M ) = ZERO
         P = D( L )
         IF( M.EQ.L )
     $      GO TO 90
         IF( M.EQ.L+1 ) THEN
            RTE = SQRT( E( L ) )
            CALL DLAE2( D( L ), RTE, D( L+1 ), RT1, RT2 )
            D( L ) = RT1
            D( L+1 ) = RT2
            E( L ) = ZERO
            L = L + 2
            IF( L.LE.LEND )
     $         GO TO 50
            GO TO 150
         END IF
         IF( JTOT.EQ.NMAXIT )
     $      GO TO 150
         JTOT = JTOT + 1
         RTE = SQRT( E( L ) )
         SIGMA = ( D( L+1 )-P ) / ( TWO*RTE )
         R = DLAPY2( SIGMA, ONE )
         SIGMA = P - ( RTE / ( SIGMA+SIGN( R, SIGMA ) ) )
         C = ONE
         S = ZERO
         GAMMA = D( M ) - SIGMA
         P = GAMMA*GAMMA
         DO 80 I = M - 1, L, -1
            BB = E( I )
            R = P + BB
            IF( I.NE.M-1 )
     $         E( I+1 ) = S*R
            OLDC = C
            C = P / R
            S = BB / R
            OLDGAM = GAMMA
            ALPHA = D( I )
            GAMMA = C*( ALPHA-SIGMA ) - S*OLDGAM
            D( I+1 ) = OLDGAM + ( ALPHA-GAMMA )
            IF( C.NE.ZERO ) THEN
               P = ( GAMMA*GAMMA ) / C
            ELSE
               P = OLDC*BB
            END IF
   80    CONTINUE
         E( L ) = S*P
         D( L ) = SIGMA + GAMMA
         GO TO 50
   90    CONTINUE
         D( L ) = P
         L = L + 1
         IF( L.LE.LEND )
     $      GO TO 50
         GO TO 150
      ELSE
  100    CONTINUE
         DO 110 M = L, LEND + 1, -1
            IF( ABS( E( M-1 ) ).LE.EPS2*ABS( D( M )*D( M-1 ) ) )
     $         GO TO 120
  110    CONTINUE
         M = LEND
  120    CONTINUE
         IF( M.GT.LEND )
     $      E( M-1 ) = ZERO
         P = D( L )
         IF( M.EQ.L )
     $      GO TO 140
         IF( M.EQ.L-1 ) THEN
            RTE = SQRT( E( L-1 ) )
            CALL DLAE2( D( L ), RTE, D( L-1 ), RT1, RT2 )
            D( L ) = RT1
            D( L-1 ) = RT2
            E( L-1 ) = ZERO
            L = L - 2
            IF( L.GE.LEND )
     $         GO TO 100
            GO TO 150
         END IF
         IF( JTOT.EQ.NMAXIT )
     $      GO TO 150
         JTOT = JTOT + 1
         RTE = SQRT( E( L-1 ) )
         SIGMA = ( D( L-1 )-P ) / ( TWO*RTE )
         R = DLAPY2( SIGMA, ONE )
         SIGMA = P - ( RTE / ( SIGMA+SIGN( R, SIGMA ) ) )
         C = ONE
         S = ZERO
         GAMMA = D( M ) - SIGMA
         P = GAMMA*GAMMA
         DO 130 I = M, L - 1
            BB = E( I )
            R = P + BB
            IF( I.NE.M )
     $         E( I-1 ) = S*R
            OLDC = C
            C = P / R
            S = BB / R
            OLDGAM = GAMMA
            ALPHA = D( I+1 )
            GAMMA = C*( ALPHA-SIGMA ) - S*OLDGAM
            D( I ) = OLDGAM + ( ALPHA-GAMMA )
            IF( C.NE.ZERO ) THEN
               P = ( GAMMA*GAMMA ) / C
            ELSE
               P = OLDC*BB
            END IF
  130    CONTINUE
         E( L-1 ) = S*P
         D( L ) = SIGMA + GAMMA
         GO TO 100
  140    CONTINUE
         D( L ) = P
         L = L - 1
         IF( L.GE.LEND )
     $      GO TO 100
         GO TO 150
      END IF
  150 CONTINUE
      IF( ISCALE.EQ.1 )
     $   CALL DLASCL( 'G', 0, 0, SSFMAX, ANORM, LENDSV-LSV+1, 1,
     $                D( LSV ), N, INFO )
      IF( ISCALE.EQ.2 )
     $   CALL DLASCL( 'G', 0, 0, SSFMIN, ANORM, LENDSV-LSV+1, 1,
     $                D( LSV ), N, INFO )
      IF( JTOT.LT.NMAXIT )
     $   GO TO 10
      DO 160 I = 1, N - 1
         IF( E( I ).NE.ZERO )
     $      INFO = INFO + 1
  160 CONTINUE
      GO TO 180
  170 CONTINUE
      CALL DLASRT( 'I', N, D, INFO )
  180 CONTINUE
      RETURN
      END
      SUBROUTINE DSTEDC( COMPZ, N, D, E, Z, LDZ, WORK, LWORK, IWORK,
     $                   LIWORK, INFO )
      CHARACTER          COMPZ
      INTEGER            INFO, LDZ, LIWORK, LWORK, N
      INTEGER            IWORK( * )
      DOUBLE PRECISION   D( * ), E( * ), WORK( * ), Z( LDZ, * )
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0 )
      LOGICAL            LQUERY
      INTEGER            FINISH, I, ICOMPZ, II, J, K, LGN, LIWMIN,
     $                   LWMIN, M, SMLSIZ, START, STOREZ, STRTRW
      DOUBLE PRECISION   EPS, ORGNRM, P, TINY
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANST
      EXTERNAL           LSAME, ILAENV, DLAMCH, DLANST
      EXTERNAL           DGEMM, DLACPY, DLAED0, DLASCL, DLASET, DLASRT,
     $                   DSTEQR, DSTERF, DSWAP, XERBLA
      INTRINSIC          ABS, DBLE, INT, LOG, MAX, MOD, SQRT
      INFO = 0
      LQUERY = ( LWORK.EQ.-1 .OR. LIWORK.EQ.-1 )
      IF( LSAME( COMPZ, 'N' ) ) THEN
         ICOMPZ = 0
      ELSE IF( LSAME( COMPZ, 'V' ) ) THEN
         ICOMPZ = 1
      ELSE IF( LSAME( COMPZ, 'I' ) ) THEN
         ICOMPZ = 2
      ELSE
         ICOMPZ = -1
      END IF
      IF( ICOMPZ.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( ( LDZ.LT.1 ) .OR.
     $         ( ICOMPZ.GT.0 .AND. LDZ.LT.MAX( 1, N ) ) ) THEN
         INFO = -6
      END IF
      IF( INFO.EQ.0 ) THEN
         SMLSIZ = ILAENV( 9, 'DSTEDC', ' ', 0, 0, 0, 0 )
         IF( N.LE.1 .OR. ICOMPZ.EQ.0 ) THEN
            LIWMIN = 1
            LWMIN = 1
         ELSE IF( N.LE.SMLSIZ ) THEN
            LIWMIN = 1
            LWMIN = 2*( N - 1 )
         ELSE
            LGN = INT( LOG( DBLE( N ) )/LOG( TWO ) )
            IF( 2**LGN.LT.N )
     $         LGN = LGN + 1
            IF( 2**LGN.LT.N )
     $         LGN = LGN + 1
            IF( ICOMPZ.EQ.1 ) THEN
               LWMIN = 1 + 3*N + 2*N*LGN + 4*N**2
               LIWMIN = 6 + 6*N + 5*N*LGN
            ELSE IF( ICOMPZ.EQ.2 ) THEN
               LWMIN = 1 + 4*N + N**2
               LIWMIN = 3 + 5*N
            END IF
         END IF
         WORK( 1 ) = LWMIN
         IWORK( 1 ) = LIWMIN
         IF( LWORK.LT.LWMIN .AND. .NOT. LQUERY ) THEN
            INFO = -8
         ELSE IF( LIWORK.LT.LIWMIN .AND. .NOT. LQUERY ) THEN
            INFO = -10
         END IF
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSTEDC', -INFO )
         RETURN
      ELSE IF (LQUERY) THEN
         RETURN
      END IF
      IF( N.EQ.0 )
     $   RETURN
      IF( N.EQ.1 ) THEN
         IF( ICOMPZ.NE.0 )
     $      Z( 1, 1 ) = ONE
         RETURN
      END IF
      IF( ICOMPZ.EQ.0 ) THEN
         CALL DSTERF( N, D, E, INFO )
         GO TO 50
      END IF
      IF( N.LE.SMLSIZ ) THEN
         CALL DSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
      ELSE
         IF( ICOMPZ.EQ.1 ) THEN
            STOREZ = 1 + N*N
         ELSE
            STOREZ = 1
         END IF
         IF( ICOMPZ.EQ.2 ) THEN
            CALL DLASET( 'Full', N, N, ZERO, ONE, Z, LDZ )
         END IF
         ORGNRM = DLANST( 'M', N, D, E )
         IF( ORGNRM.EQ.ZERO )
     $      GO TO 50
         EPS = DLAMCH( 'Epsilon' )
         START = 1
   10    CONTINUE
         IF( START.LE.N ) THEN
            FINISH = START
   20       CONTINUE
            IF( FINISH.LT.N ) THEN
               TINY = EPS*SQRT( ABS( D( FINISH ) ) )*
     $                    SQRT( ABS( D( FINISH+1 ) ) )
               IF( ABS( E( FINISH ) ).GT.TINY ) THEN
                  FINISH = FINISH + 1
                  GO TO 20
               END IF
            END IF
            M = FINISH - START + 1
            IF( M.EQ.1 ) THEN
               START = FINISH + 1
               GO TO 10
            END IF
            IF( M.GT.SMLSIZ ) THEN
               ORGNRM = DLANST( 'M', M, D( START ), E( START ) )
               CALL DLASCL( 'G', 0, 0, ORGNRM, ONE, M, 1, D( START ), M,
     $                      INFO )
               CALL DLASCL( 'G', 0, 0, ORGNRM, ONE, M-1, 1, E( START ),
     $                      M-1, INFO )
               IF( ICOMPZ.EQ.1 ) THEN
                  STRTRW = 1
               ELSE
                  STRTRW = START
               END IF
               CALL DLAED0( ICOMPZ, N, M, D( START ), E( START ),
     $                      Z( STRTRW, START ), LDZ, WORK( 1 ), N,
     $                      WORK( STOREZ ), IWORK, INFO )
               IF( INFO.NE.0 ) THEN
                  INFO = ( INFO / ( M+1 )+START-1 )*( N+1 ) +
     $                   MOD( INFO, ( M+1 ) ) + START - 1
                  GO TO 50
               END IF
               CALL DLASCL( 'G', 0, 0, ONE, ORGNRM, M, 1, D( START ), M,
     $                      INFO )
            ELSE
               IF( ICOMPZ.EQ.1 ) THEN
                  CALL DSTEQR( 'I', M, D( START ), E( START ), WORK, M,
     $                         WORK( M*M+1 ), INFO )
                  CALL DLACPY( 'A', N, M, Z( 1, START ), LDZ,
     $                         WORK( STOREZ ), N )
                  CALL DGEMM( 'N', 'N', N, M, M, ONE,
     $                        WORK( STOREZ ), N, WORK, M, ZERO,
     $                        Z( 1, START ), LDZ )
               ELSE IF( ICOMPZ.EQ.2 ) THEN
                  CALL DSTEQR( 'I', M, D( START ), E( START ),
     $                         Z( START, START ), LDZ, WORK, INFO )
               ELSE
                  CALL DSTERF( M, D( START ), E( START ), INFO )
               END IF
               IF( INFO.NE.0 ) THEN
                  INFO = START*( N+1 ) + FINISH
                  GO TO 50
               END IF
            END IF
            START = FINISH + 1
            GO TO 10
         END IF
         IF( ICOMPZ.EQ.0 ) THEN
           CALL DLASRT( 'I', N, D, INFO )
         ELSE
           DO 40 II = 2, N
              I = II - 1
              K = I
              P = D( I )
              DO 30 J = II, N
                 IF( D( J ).LT.P ) THEN
                    K = J
                    P = D( J )
                 END IF
   30         CONTINUE
              IF( K.NE.I ) THEN
                 D( K ) = D( I )
                 D( I ) = P
                 CALL DSWAP( N, Z( 1, I ), 1, Z( 1, K ), 1 )
              END IF
   40      CONTINUE
         END IF
      END IF
   50 CONTINUE
      WORK( 1 ) = LWMIN
      IWORK( 1 ) = LIWMIN
      RETURN
      END
      SUBROUTINE DORMTR( SIDE, UPLO, TRANS, M, N, A, LDA, TAU, C, LDC,
     $                   WORK, LWORK, INFO )
      CHARACTER          SIDE, TRANS, UPLO
      INTEGER            INFO, LDA, LDC, LWORK, M, N
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
      LOGICAL            LEFT, LQUERY, UPPER
      INTEGER            I1, I2, IINFO, LWKOPT, MI, NB, NI, NQ, NW
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
      EXTERNAL           DORMQL, DORMQR, XERBLA
      INTRINSIC          MAX
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      UPPER = LSAME( UPLO, 'U' )
      LQUERY = ( LWORK.EQ.-1 )
      IF( LEFT ) THEN
         NQ = M
         NW = MAX( 1, N )
      ELSE
         NQ = N
         NW = MAX( 1, M )
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -2
      ELSE IF( .NOT.LSAME( TRANS, 'N' ) .AND. .NOT.LSAME( TRANS, 'T' ) )
     $          THEN
         INFO = -3
      ELSE IF( M.LT.0 ) THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ELSE IF( LWORK.LT.NW .AND. .NOT.LQUERY ) THEN
         INFO = -12
      END IF
      IF( INFO.EQ.0 ) THEN
         IF( UPPER ) THEN
            IF( LEFT ) THEN
               NB = ILAENV( 1, 'DORMQL', SIDE // TRANS, M-1, N, M-1,
     $              -1 )
            ELSE
               NB = ILAENV( 1, 'DORMQL', SIDE // TRANS, M, N-1, N-1,
     $              -1 )
            END IF
         ELSE
            IF( LEFT ) THEN
               NB = ILAENV( 1, 'DORMQR', SIDE // TRANS, M-1, N, M-1,
     $              -1 )
            ELSE
               NB = ILAENV( 1, 'DORMQR', SIDE // TRANS, M, N-1, N-1,
     $              -1 )
            END IF
         END IF
         LWKOPT = NW*NB
         WORK( 1 ) = LWKOPT
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORMTR', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
      IF( M.EQ.0 .OR. N.EQ.0 .OR. NQ.EQ.1 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
      IF( LEFT ) THEN
         MI = M - 1
         NI = N
      ELSE
         MI = M
         NI = N - 1
      END IF
      IF( UPPER ) THEN
         CALL DORMQL( SIDE, TRANS, MI, NI, NQ-1, A( 1, 2 ), LDA, TAU, C,
     $                LDC, WORK, LWORK, IINFO )
      ELSE
         IF( LEFT ) THEN
            I1 = 2
            I2 = 1
         ELSE
            I1 = 1
            I2 = 2
         END IF
         CALL DORMQR( SIDE, TRANS, MI, NI, NQ-1, A( 2, 1 ), LDA, TAU,
     $                C( I1, I2 ), LDC, WORK, LWORK, IINFO )
      END IF
      WORK( 1 ) = LWKOPT
      RETURN
      END
      SUBROUTINE DLACPY( UPLO, M, N, A, LDA, B, LDB )
      CHARACTER          UPLO
      INTEGER            LDA, LDB, M, N
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
      INTEGER            I, J
      LOGICAL            LSAME
      EXTERNAL           LSAME
      INTRINSIC          MIN
      IF( LSAME( UPLO, 'U' ) ) THEN
         DO 20 J = 1, N
            DO 10 I = 1, MIN( J, M )
               B( I, J ) = A( I, J )
   10       CONTINUE
   20    CONTINUE
      ELSE IF( LSAME( UPLO, 'L' ) ) THEN
         DO 40 J = 1, N
            DO 30 I = J, M
               B( I, J ) = A( I, J )
   30       CONTINUE
   40    CONTINUE
      ELSE
         DO 60 J = 1, N
            DO 50 I = 1, M
               B( I, J ) = A( I, J )
   50       CONTINUE
   60    CONTINUE
      END IF
      RETURN
      END
      SUBROUTINE DLATRD( UPLO, N, NB, A, LDA, E, TAU, W, LDW )
      CHARACTER          UPLO
      INTEGER            LDA, LDW, N, NB
      DOUBLE PRECISION   A( LDA, * ), E( * ), TAU( * ), W( LDW, * )
      DOUBLE PRECISION   ZERO, ONE, HALF
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, HALF = 0.5D+0 )
      INTEGER            I, IW
      DOUBLE PRECISION   ALPHA
      EXTERNAL           DAXPY, DGEMV, DLARFG, DSCAL, DSYMV
      LOGICAL            LSAME
      DOUBLE PRECISION   DDOT
      EXTERNAL           LSAME, DDOT
      INTRINSIC          MIN
      IF( N.LE.0 )
     $   RETURN
      IF( LSAME( UPLO, 'U' ) ) THEN
         DO 10 I = N, N - NB + 1, -1
            IW = I - N + NB
            IF( I.LT.N ) THEN
               CALL DGEMV( 'No transpose', I, N-I, -ONE, A( 1, I+1 ),
     $                     LDA, W( I, IW+1 ), LDW, ONE, A( 1, I ), 1 )
               CALL DGEMV( 'No transpose', I, N-I, -ONE, W( 1, IW+1 ),
     $                     LDW, A( I, I+1 ), LDA, ONE, A( 1, I ), 1 )
            END IF
            IF( I.GT.1 ) THEN
               CALL DLARFG( I-1, A( I-1, I ), A( 1, I ), 1, TAU( I-1 ) )
               E( I-1 ) = A( I-1, I )
               A( I-1, I ) = ONE
               CALL DSYMV( 'Upper', I-1, ONE, A, LDA, A( 1, I ), 1,
     $                     ZERO, W( 1, IW ), 1 )
               IF( I.LT.N ) THEN
                  CALL DGEMV( 'Transpose', I-1, N-I, ONE, W( 1, IW+1 ),
     $                        LDW, A( 1, I ), 1, ZERO, W( I+1, IW ), 1 )
                  CALL DGEMV( 'No transpose', I-1, N-I, -ONE,
     $                        A( 1, I+1 ), LDA, W( I+1, IW ), 1, ONE,
     $                        W( 1, IW ), 1 )
                  CALL DGEMV( 'Transpose', I-1, N-I, ONE, A( 1, I+1 ),
     $                        LDA, A( 1, I ), 1, ZERO, W( I+1, IW ), 1 )
                  CALL DGEMV( 'No transpose', I-1, N-I, -ONE,
     $                        W( 1, IW+1 ), LDW, W( I+1, IW ), 1, ONE,
     $                        W( 1, IW ), 1 )
               END IF
               CALL DSCAL( I-1, TAU( I-1 ), W( 1, IW ), 1 )
               ALPHA = -HALF*TAU( I-1 )*DDOT( I-1, W( 1, IW ), 1,
     $                 A( 1, I ), 1 )
               CALL DAXPY( I-1, ALPHA, A( 1, I ), 1, W( 1, IW ), 1 )
            END IF
   10    CONTINUE
      ELSE
         DO 20 I = 1, NB
            CALL DGEMV( 'No transpose', N-I+1, I-1, -ONE, A( I, 1 ),
     $                  LDA, W( I, 1 ), LDW, ONE, A( I, I ), 1 )
            CALL DGEMV( 'No transpose', N-I+1, I-1, -ONE, W( I, 1 ),
     $                  LDW, A( I, 1 ), LDA, ONE, A( I, I ), 1 )
            IF( I.LT.N ) THEN
               CALL DLARFG( N-I, A( I+1, I ), A( MIN( I+2, N ), I ), 1,
     $                      TAU( I ) )
               E( I ) = A( I+1, I )
               A( I+1, I ) = ONE
               CALL DSYMV( 'Lower', N-I, ONE, A( I+1, I+1 ), LDA,
     $                     A( I+1, I ), 1, ZERO, W( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', N-I, I-1, ONE, W( I+1, 1 ), LDW,
     $                     A( I+1, I ), 1, ZERO, W( 1, I ), 1 )
               CALL DGEMV( 'No transpose', N-I, I-1, -ONE, A( I+1, 1 ),
     $                     LDA, W( 1, I ), 1, ONE, W( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', N-I, I-1, ONE, A( I+1, 1 ), LDA,
     $                     A( I+1, I ), 1, ZERO, W( 1, I ), 1 )
               CALL DGEMV( 'No transpose', N-I, I-1, -ONE, W( I+1, 1 ),
     $                     LDW, W( 1, I ), 1, ONE, W( I+1, I ), 1 )
               CALL DSCAL( N-I, TAU( I ), W( I+1, I ), 1 )
               ALPHA = -HALF*TAU( I )*DDOT( N-I, W( I+1, I ), 1,
     $                 A( I+1, I ), 1 )
               CALL DAXPY( N-I, ALPHA, A( I+1, I ), 1, W( I+1, I ), 1 )
            END IF
   20    CONTINUE
      END IF
      RETURN
      END
      SUBROUTINE DSYR2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
      DOUBLE PRECISION ALPHA,BETA
      INTEGER K,LDA,LDB,LDC,N
      CHARACTER TRANS,UPLO
      DOUBLE PRECISION A(LDA,*),B(LDB,*),C(LDC,*)
      LOGICAL LSAME
      EXTERNAL LSAME
      EXTERNAL XERBLA
      INTRINSIC MAX
      DOUBLE PRECISION TEMP1,TEMP2
      INTEGER I,INFO,J,L,NROWA
      LOGICAL UPPER
      DOUBLE PRECISION ONE,ZERO
      PARAMETER (ONE=1.0D+0,ZERO=0.0D+0)
      IF (LSAME(TRANS,'N')) THEN
          NROWA = N
      ELSE
          NROWA = K
      END IF
      UPPER = LSAME(UPLO,'U')
      INFO = 0
      IF ((.NOT.UPPER) .AND. (.NOT.LSAME(UPLO,'L'))) THEN
          INFO = 1
      ELSE IF ((.NOT.LSAME(TRANS,'N')) .AND.
     +         (.NOT.LSAME(TRANS,'T')) .AND.
     +         (.NOT.LSAME(TRANS,'C'))) THEN
          INFO = 2
      ELSE IF (N.LT.0) THEN
          INFO = 3
      ELSE IF (K.LT.0) THEN
          INFO = 4
      ELSE IF (LDA.LT.MAX(1,NROWA)) THEN
          INFO = 7
      ELSE IF (LDB.LT.MAX(1,NROWA)) THEN
          INFO = 9
      ELSE IF (LDC.LT.MAX(1,N)) THEN
          INFO = 12
      END IF
      IF (INFO.NE.0) THEN
          CALL XERBLA('DSYR2K',INFO)
          RETURN
      END IF
      IF ((N.EQ.0) .OR. (((ALPHA.EQ.ZERO).OR.
     +    (K.EQ.0)).AND. (BETA.EQ.ONE))) RETURN
      IF (ALPHA.EQ.ZERO) THEN
          IF (UPPER) THEN
              IF (BETA.EQ.ZERO) THEN
                  DO 20 J = 1,N
                      DO 10 I = 1,J
                          C(I,J) = ZERO
   10                 CONTINUE
   20             CONTINUE
              ELSE
                  DO 40 J = 1,N
                      DO 30 I = 1,J
                          C(I,J) = BETA*C(I,J)
   30                 CONTINUE
   40             CONTINUE
              END IF
          ELSE
              IF (BETA.EQ.ZERO) THEN
                  DO 60 J = 1,N
                      DO 50 I = J,N
                          C(I,J) = ZERO
   50                 CONTINUE
   60             CONTINUE
              ELSE
                  DO 80 J = 1,N
                      DO 70 I = J,N
                          C(I,J) = BETA*C(I,J)
   70                 CONTINUE
   80             CONTINUE
              END IF
          END IF
          RETURN
      END IF
      IF (LSAME(TRANS,'N')) THEN
          IF (UPPER) THEN
              DO 130 J = 1,N
                  IF (BETA.EQ.ZERO) THEN
                      DO 90 I = 1,J
                          C(I,J) = ZERO
   90                 CONTINUE
                  ELSE IF (BETA.NE.ONE) THEN
                      DO 100 I = 1,J
                          C(I,J) = BETA*C(I,J)
  100                 CONTINUE
                  END IF
                  DO 120 L = 1,K
                      IF ((A(J,L).NE.ZERO) .OR. (B(J,L).NE.ZERO)) THEN
                          TEMP1 = ALPHA*B(J,L)
                          TEMP2 = ALPHA*A(J,L)
                          DO 110 I = 1,J
                              C(I,J) = C(I,J) + A(I,L)*TEMP1 +
     +                                 B(I,L)*TEMP2
  110                     CONTINUE
                      END IF
  120             CONTINUE
  130         CONTINUE
          ELSE
              DO 180 J = 1,N
                  IF (BETA.EQ.ZERO) THEN
                      DO 140 I = J,N
                          C(I,J) = ZERO
  140                 CONTINUE
                  ELSE IF (BETA.NE.ONE) THEN
                      DO 150 I = J,N
                          C(I,J) = BETA*C(I,J)
  150                 CONTINUE
                  END IF
                  DO 170 L = 1,K
                      IF ((A(J,L).NE.ZERO) .OR. (B(J,L).NE.ZERO)) THEN
                          TEMP1 = ALPHA*B(J,L)
                          TEMP2 = ALPHA*A(J,L)
                          DO 160 I = J,N
                              C(I,J) = C(I,J) + A(I,L)*TEMP1 +
     +                                 B(I,L)*TEMP2
  160                     CONTINUE
                      END IF
  170             CONTINUE
  180         CONTINUE
          END IF
      ELSE
          IF (UPPER) THEN
              DO 210 J = 1,N
                  DO 200 I = 1,J
                      TEMP1 = ZERO
                      TEMP2 = ZERO
                      DO 190 L = 1,K
                          TEMP1 = TEMP1 + A(L,I)*B(L,J)
                          TEMP2 = TEMP2 + B(L,I)*A(L,J)
  190                 CONTINUE
                      IF (BETA.EQ.ZERO) THEN
                          C(I,J) = ALPHA*TEMP1 + ALPHA*TEMP2
                      ELSE
                          C(I,J) = BETA*C(I,J) + ALPHA*TEMP1 +
     +                             ALPHA*TEMP2
                      END IF
  200             CONTINUE
  210         CONTINUE
          ELSE
              DO 240 J = 1,N
                  DO 230 I = J,N
                      TEMP1 = ZERO
                      TEMP2 = ZERO
                      DO 220 L = 1,K
                          TEMP1 = TEMP1 + A(L,I)*B(L,J)
                          TEMP2 = TEMP2 + B(L,I)*A(L,J)
  220                 CONTINUE
                      IF (BETA.EQ.ZERO) THEN
                          C(I,J) = ALPHA*TEMP1 + ALPHA*TEMP2
                      ELSE
                          C(I,J) = BETA*C(I,J) + ALPHA*TEMP1 +
     +                             ALPHA*TEMP2
                      END IF
  230             CONTINUE
  240         CONTINUE
          END IF
      END IF
      RETURN
      END
      SUBROUTINE DSYTD2( UPLO, N, A, LDA, D, E, TAU, INFO )
      CHARACTER          UPLO
      INTEGER            INFO, LDA, N
      DOUBLE PRECISION   A( LDA, * ), D( * ), E( * ), TAU( * )
      DOUBLE PRECISION   ONE, ZERO, HALF
      PARAMETER          ( ONE = 1.0D0, ZERO = 0.0D0,
     $                   HALF = 1.0D0 / 2.0D0 )
      LOGICAL            UPPER
      INTEGER            I
      DOUBLE PRECISION   ALPHA, TAUI
      EXTERNAL           DAXPY, DLARFG, DSYMV, DSYR2, XERBLA
      LOGICAL            LSAME
      DOUBLE PRECISION   DDOT
      EXTERNAL           LSAME, DDOT
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
         CALL XERBLA( 'DSYTD2', -INFO )
         RETURN
      END IF
      IF( N.LE.0 )
     $   RETURN
      IF( UPPER ) THEN
         DO 10 I = N - 1, 1, -1
            CALL DLARFG( I, A( I, I+1 ), A( 1, I+1 ), 1, TAUI )
            E( I ) = A( I, I+1 )
            IF( TAUI.NE.ZERO ) THEN
               A( I, I+1 ) = ONE
               CALL DSYMV( UPLO, I, TAUI, A, LDA, A( 1, I+1 ), 1, ZERO,
     $                     TAU, 1 )
               ALPHA = -HALF*TAUI*DDOT( I, TAU, 1, A( 1, I+1 ), 1 )
               CALL DAXPY( I, ALPHA, A( 1, I+1 ), 1, TAU, 1 )
               CALL DSYR2( UPLO, I, -ONE, A( 1, I+1 ), 1, TAU, 1, A,
     $                     LDA )
               A( I, I+1 ) = E( I )
            END IF
            D( I+1 ) = A( I+1, I+1 )
            TAU( I ) = TAUI
   10    CONTINUE
         D( 1 ) = A( 1, 1 )
      ELSE
         DO 20 I = 1, N - 1
            CALL DLARFG( N-I, A( I+1, I ), A( MIN( I+2, N ), I ), 1,
     $                   TAUI )
            E( I ) = A( I+1, I )
            IF( TAUI.NE.ZERO ) THEN
               A( I+1, I ) = ONE
               CALL DSYMV( UPLO, N-I, TAUI, A( I+1, I+1 ), LDA,
     $                     A( I+1, I ), 1, ZERO, TAU( I ), 1 )
               ALPHA = -HALF*TAUI*DDOT( N-I, TAU( I ), 1, A( I+1, I ),
     $                 1 )
               CALL DAXPY( N-I, ALPHA, A( I+1, I ), 1, TAU( I ), 1 )
               CALL DSYR2( UPLO, N-I, -ONE, A( I+1, I ), 1, TAU( I ), 1,
     $                     A( I+1, I+1 ), LDA )
               A( I+1, I ) = E( I )
            END IF
            D( I ) = A( I, I )
            TAU( I ) = TAUI
   20    CONTINUE
         D( N ) = A( N, N )
      END IF
      RETURN
      END
      SUBROUTINE DLASRT( ID, N, D, INFO )
      CHARACTER          ID
      INTEGER            INFO, N
      DOUBLE PRECISION   D( * )
      INTEGER            SELECT
      PARAMETER          ( SELECT = 20 )
      INTEGER            DIR, ENDD, I, J, START, STKPNT
      DOUBLE PRECISION   D1, D2, D3, DMNMX, TMP
      INTEGER            STACK( 2, 32 )
      LOGICAL            LSAME
      EXTERNAL           LSAME
      EXTERNAL           XERBLA
      INFO = 0
      DIR = -1
      IF( LSAME( ID, 'D' ) ) THEN
         DIR = 0
      ELSE IF( LSAME( ID, 'I' ) ) THEN
         DIR = 1
      END IF
      IF( DIR.EQ.-1 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASRT', -INFO )
         RETURN
      END IF
      IF( N.LE.1 )
     $   RETURN
      STKPNT = 1
      STACK( 1, 1 ) = 1
      STACK( 2, 1 ) = N
   10 CONTINUE
      START = STACK( 1, STKPNT )
      ENDD = STACK( 2, STKPNT )
      STKPNT = STKPNT - 1
      IF( ENDD-START.LE.SELECT .AND. ENDD-START.GT.0 ) THEN
         IF( DIR.EQ.0 ) THEN
            DO 30 I = START + 1, ENDD
               DO 20 J = I, START + 1, -1
                  IF( D( J ).GT.D( J-1 ) ) THEN
                     DMNMX = D( J )
                     D( J ) = D( J-1 )
                     D( J-1 ) = DMNMX
                  ELSE
                     GO TO 30
                  END IF
   20          CONTINUE
   30       CONTINUE
         ELSE
            DO 50 I = START + 1, ENDD
               DO 40 J = I, START + 1, -1
                  IF( D( J ).LT.D( J-1 ) ) THEN
                     DMNMX = D( J )
                     D( J ) = D( J-1 )
                     D( J-1 ) = DMNMX
                  ELSE
                     GO TO 50
                  END IF
   40          CONTINUE
   50       CONTINUE
         END IF
      ELSE IF( ENDD-START.GT.SELECT ) THEN
         D1 = D( START )
         D2 = D( ENDD )
         I = ( START+ENDD ) / 2
         D3 = D( I )
         IF( D1.LT.D2 ) THEN
            IF( D3.LT.D1 ) THEN
               DMNMX = D1
            ELSE IF( D3.LT.D2 ) THEN
               DMNMX = D3
            ELSE
               DMNMX = D2
            END IF
         ELSE
            IF( D3.LT.D2 ) THEN
               DMNMX = D2
            ELSE IF( D3.LT.D1 ) THEN
               DMNMX = D3
            ELSE
               DMNMX = D1
            END IF
         END IF
         IF( DIR.EQ.0 ) THEN
            I = START - 1
            J = ENDD + 1
   60       CONTINUE
   70       CONTINUE
            J = J - 1
            IF( D( J ).LT.DMNMX )
     $         GO TO 70
   80       CONTINUE
            I = I + 1
            IF( D( I ).GT.DMNMX )
     $         GO TO 80
            IF( I.LT.J ) THEN
               TMP = D( I )
               D( I ) = D( J )
               D( J ) = TMP
               GO TO 60
            END IF
            IF( J-START.GT.ENDD-J-1 ) THEN
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = START
               STACK( 2, STKPNT ) = J
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = J + 1
               STACK( 2, STKPNT ) = ENDD
            ELSE
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = J + 1
               STACK( 2, STKPNT ) = ENDD
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = START
               STACK( 2, STKPNT ) = J
            END IF
         ELSE
            I = START - 1
            J = ENDD + 1
   90       CONTINUE
  100       CONTINUE
            J = J - 1
            IF( D( J ).GT.DMNMX )
     $         GO TO 100
  110       CONTINUE
            I = I + 1
            IF( D( I ).LT.DMNMX )
     $         GO TO 110
            IF( I.LT.J ) THEN
               TMP = D( I )
               D( I ) = D( J )
               D( J ) = TMP
               GO TO 90
            END IF
            IF( J-START.GT.ENDD-J-1 ) THEN
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = START
               STACK( 2, STKPNT ) = J
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = J + 1
               STACK( 2, STKPNT ) = ENDD
            ELSE
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = J + 1
               STACK( 2, STKPNT ) = ENDD
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = START
               STACK( 2, STKPNT ) = J
            END IF
         END IF
      END IF
      IF( STKPNT.GT.0 )
     $   GO TO 10
      RETURN
      END
      DOUBLE PRECISION FUNCTION DLANST( NORM, N, D, E )
      CHARACTER          NORM
      INTEGER            N
      DOUBLE PRECISION   D( * ), E( * )
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      INTEGER            I
      DOUBLE PRECISION   ANORM, SCALE, SUM
      LOGICAL            LSAME, DISNAN
      EXTERNAL           LSAME, DISNAN
      EXTERNAL           DLASSQ
      INTRINSIC          ABS, SQRT
      IF( N.LE.0 ) THEN
         ANORM = ZERO
      ELSE IF( LSAME( NORM, 'M' ) ) THEN
         ANORM = ABS( D( N ) )
         DO 10 I = 1, N - 1
            SUM = ABS( D( I ) )
            IF( ANORM .LT. SUM .OR. DISNAN( SUM ) ) ANORM = SUM
            SUM = ABS( E( I ) )
            IF( ANORM .LT. SUM .OR. DISNAN( SUM ) ) ANORM = SUM
   10    CONTINUE
      ELSE IF( LSAME( NORM, 'O' ) .OR. NORM.EQ.'1' .OR.
     $         LSAME( NORM, 'I' ) ) THEN
         IF( N.EQ.1 ) THEN
            ANORM = ABS( D( 1 ) )
         ELSE
            ANORM = ABS( D( 1 ) )+ABS( E( 1 ) )
            SUM = ABS( E( N-1 ) )+ABS( D( N ) )
            IF( ANORM .LT. SUM .OR. DISNAN( SUM ) ) ANORM = SUM
            DO 20 I = 2, N - 1
               SUM = ABS( D( I ) )+ABS( E( I ) )+ABS( E( I-1 ) )
               IF( ANORM .LT. SUM .OR. DISNAN( SUM ) ) ANORM = SUM
   20       CONTINUE
         END IF
      ELSE IF( ( LSAME( NORM, 'F' ) ) .OR. ( LSAME( NORM, 'E' ) ) ) THEN
         SCALE = ZERO
         SUM = ONE
         IF( N.GT.1 ) THEN
            CALL DLASSQ( N-1, E, 1, SCALE, SUM )
            SUM = 2*SUM
         END IF
         CALL DLASSQ( N, D, 1, SCALE, SUM )
         ANORM = SCALE*SQRT( SUM )
      END IF
      DLANST = ANORM
      RETURN
      END
      SUBROUTINE DLAE2( A, B, C, RT1, RT2 )
      DOUBLE PRECISION   A, B, C, RT1, RT2
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   TWO
      PARAMETER          ( TWO = 2.0D0 )
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   HALF
      PARAMETER          ( HALF = 0.5D0 )
      DOUBLE PRECISION   AB, ACMN, ACMX, ADF, DF, RT, SM, TB
      INTRINSIC          ABS, SQRT
      SM = A + C
      DF = A - C
      ADF = ABS( DF )
      TB = B + B
      AB = ABS( TB )
      IF( ABS( A ).GT.ABS( C ) ) THEN
         ACMX = A
         ACMN = C
      ELSE
         ACMX = C
         ACMN = A
      END IF
      IF( ADF.GT.AB ) THEN
         RT = ADF*SQRT( ONE+( AB / ADF )**2 )
      ELSE IF( ADF.LT.AB ) THEN
         RT = AB*SQRT( ONE+( ADF / AB )**2 )
      ELSE
         RT = AB*SQRT( TWO )
      END IF
      IF( SM.LT.ZERO ) THEN
         RT1 = HALF*( SM-RT )
         RT2 = ( ACMX / RT1 )*ACMN - ( B / RT1 )*B
      ELSE IF( SM.GT.ZERO ) THEN
         RT1 = HALF*( SM+RT )
         RT2 = ( ACMX / RT1 )*ACMN - ( B / RT1 )*B
      ELSE
         RT1 = HALF*RT
         RT2 = -HALF*RT
      END IF
      RETURN
      END
      DOUBLE PRECISION FUNCTION DLAPY2( X, Y )
      DOUBLE PRECISION   X, Y
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   W, XABS, YABS, Z
      LOGICAL            X_IS_NAN, Y_IS_NAN
      LOGICAL            DISNAN
      EXTERNAL           DISNAN
      INTRINSIC          ABS, MAX, MIN, SQRT
      X_IS_NAN = DISNAN( X )
      Y_IS_NAN = DISNAN( Y )
      IF ( X_IS_NAN ) DLAPY2 = X
      IF ( Y_IS_NAN ) DLAPY2 = Y
      IF ( .NOT.( X_IS_NAN.OR.Y_IS_NAN ) ) THEN
         XABS = ABS( X )
         YABS = ABS( Y )
         W = MAX( XABS, YABS )
         Z = MIN( XABS, YABS )
         IF( Z.EQ.ZERO ) THEN
            DLAPY2 = W
         ELSE
            DLAPY2 = W*SQRT( ONE+( Z / W )**2 )
         END IF
      END IF
      RETURN
      END
      SUBROUTINE DORMQL( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, LWORK, INFO )
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, LWORK, M, N
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
      INTEGER            NBMAX, LDT, TSIZE
      PARAMETER          ( NBMAX = 64, LDT = NBMAX+1,
     $                     TSIZE = LDT*NBMAX )
      LOGICAL            LEFT, LQUERY, NOTRAN
      INTEGER            I, I1, I2, I3, IB, IINFO, IWT, LDWORK, LWKOPT,
     $                   MI, NB, NBMIN, NI, NQ, NW
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
      EXTERNAL           DLARFB, DLARFT, DORM2L, XERBLA
      INTRINSIC          MAX, MIN
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
      LQUERY = ( LWORK.EQ.-1 )
      IF( LEFT ) THEN
         NQ = M
         NW = MAX( 1, N )
      ELSE
         NQ = N
         NW = MAX( 1, M )
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ELSE IF( LWORK.LT.NW .AND. .NOT.LQUERY ) THEN
         INFO = -12
      END IF
      IF( INFO.EQ.0 ) THEN
         IF( M.EQ.0 .OR. N.EQ.0 ) THEN
            LWKOPT = 1
         ELSE
            NB = MIN( NBMAX, ILAENV( 1, 'DORMQL', SIDE // TRANS, M, N,
     $                               K, -1 ) )
            LWKOPT = NW*NB + TSIZE
         END IF
         WORK( 1 ) = LWKOPT
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORMQL', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
      IF( M.EQ.0 .OR. N.EQ.0 ) THEN
         RETURN
      END IF
      NBMIN = 2
      LDWORK = NW
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
         IF( LWORK.LT.LWKOPT ) THEN
            NB = (LWORK-TSIZE) / LDWORK
            NBMIN = MAX( 2, ILAENV( 2, 'DORMQL', SIDE // TRANS, M, N, K,
     $              -1 ) )
         END IF
      END IF
      IF( NB.LT.NBMIN .OR. NB.GE.K ) THEN
         CALL DORM2L( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, WORK,
     $                IINFO )
      ELSE
         IWT = 1 + NW*NB
         IF( ( LEFT .AND. NOTRAN ) .OR.
     $       ( .NOT.LEFT .AND. .NOT.NOTRAN ) ) THEN
            I1 = 1
            I2 = K
            I3 = NB
         ELSE
            I1 = ( ( K-1 ) / NB )*NB + 1
            I2 = 1
            I3 = -NB
         END IF
         IF( LEFT ) THEN
            NI = N
         ELSE
            MI = M
         END IF
         DO 10 I = I1, I2, I3
            IB = MIN( NB, K-I+1 )
            CALL DLARFT( 'Backward', 'Columnwise', NQ-K+I+IB-1, IB,
     $                   A( 1, I ), LDA, TAU( I ), WORK( IWT ), LDT )
            IF( LEFT ) THEN
               MI = M - K + I + IB - 1
            ELSE
               NI = N - K + I + IB - 1
            END IF
            CALL DLARFB( SIDE, TRANS, 'Backward', 'Columnwise', MI, NI,
     $                   IB, A( 1, I ), LDA, WORK( IWT ), LDT, C, LDC,
     $                   WORK, LDWORK )
   10    CONTINUE
      END IF
      WORK( 1 ) = LWKOPT
      RETURN
      END
      SUBROUTINE DORMQR( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, LWORK, INFO )
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, LWORK, M, N
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
      INTEGER            NBMAX, LDT, TSIZE
      PARAMETER          ( NBMAX = 64, LDT = NBMAX+1,
     $                     TSIZE = LDT*NBMAX )
      LOGICAL            LEFT, LQUERY, NOTRAN
      INTEGER            I, I1, I2, I3, IB, IC, IINFO, IWT, JC, LDWORK,
     $                   LWKOPT, MI, NB, NBMIN, NI, NQ, NW
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
      EXTERNAL           DLARFB, DLARFT, DORM2R, XERBLA
      INTRINSIC          MAX, MIN
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
      LQUERY = ( LWORK.EQ.-1 )
      IF( LEFT ) THEN
         NQ = M
         NW = MAX( 1, N )
      ELSE
         NQ = N
         NW = MAX( 1, M )
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ELSE IF( LWORK.LT.NW .AND. .NOT.LQUERY ) THEN
         INFO = -12
      END IF
      IF( INFO.EQ.0 ) THEN
         NB = MIN( NBMAX, ILAENV( 1, 'DORMQR', SIDE // TRANS, M, N, K,
     $        -1 ) )
         LWKOPT = NW*NB + TSIZE
         WORK( 1 ) = LWKOPT
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORMQR', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
      NBMIN = 2
      LDWORK = NW
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
         IF( LWORK.LT.LWKOPT ) THEN
            NB = (LWORK-TSIZE) / LDWORK
            NBMIN = MAX( 2, ILAENV( 2, 'DORMQR', SIDE // TRANS, M, N, K,
     $              -1 ) )
         END IF
      END IF
      IF( NB.LT.NBMIN .OR. NB.GE.K ) THEN
         CALL DORM2R( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, WORK,
     $                IINFO )
      ELSE
         IWT = 1 + NW*NB
         IF( ( LEFT .AND. .NOT.NOTRAN ) .OR.
     $       ( .NOT.LEFT .AND. NOTRAN ) ) THEN
            I1 = 1
            I2 = K
            I3 = NB
         ELSE
            I1 = ( ( K-1 ) / NB )*NB + 1
            I2 = 1
            I3 = -NB
         END IF
         IF( LEFT ) THEN
            NI = N
            JC = 1
         ELSE
            MI = M
            IC = 1
         END IF
         DO 10 I = I1, I2, I3
            IB = MIN( NB, K-I+1 )
            CALL DLARFT( 'Forward', 'Columnwise', NQ-I+1, IB, A( I, I ),
     $                   LDA, TAU( I ), WORK( IWT ), LDT )
            IF( LEFT ) THEN
               MI = M - I + 1
               IC = I
            ELSE
               NI = N - I + 1
               JC = I
            END IF
            CALL DLARFB( SIDE, TRANS, 'Forward', 'Columnwise', MI, NI,
     $                   IB, A( I, I ), LDA, WORK( IWT ), LDT,
     $                   C( IC, JC ), LDC, WORK, LDWORK )
   10    CONTINUE
      END IF
      WORK( 1 ) = LWKOPT
      RETURN
      END
      SUBROUTINE DSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
      CHARACTER          COMPZ
      INTEGER            INFO, LDZ, N
      DOUBLE PRECISION   D( * ), E( * ), WORK( * ), Z( LDZ, * )
      DOUBLE PRECISION   ZERO, ONE, TWO, THREE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                   THREE = 3.0D0 )
      INTEGER            MAXIT
      PARAMETER          ( MAXIT = 30 )
      INTEGER            I, ICOMPZ, II, ISCALE, J, JTOT, K, L, L1, LEND,
     $                   LENDM1, LENDP1, LENDSV, LM1, LSV, M, MM, MM1,
     $                   NM1, NMAXIT
      DOUBLE PRECISION   ANORM, B, C, EPS, EPS2, F, G, P, R, RT1, RT2,
     $                   S, SAFMAX, SAFMIN, SSFMAX, SSFMIN, TST
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANST, DLAPY2
      EXTERNAL           LSAME, DLAMCH, DLANST, DLAPY2
      EXTERNAL           DLAE2, DLAEV2, DLARTG, DLASCL, DLASET, DLASR,
     $                   DLASRT, DSWAP, XERBLA
      INTRINSIC          ABS, MAX, SIGN, SQRT
      INFO = 0
      IF( LSAME( COMPZ, 'N' ) ) THEN
         ICOMPZ = 0
      ELSE IF( LSAME( COMPZ, 'V' ) ) THEN
         ICOMPZ = 1
      ELSE IF( LSAME( COMPZ, 'I' ) ) THEN
         ICOMPZ = 2
      ELSE
         ICOMPZ = -1
      END IF
      IF( ICOMPZ.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( ( LDZ.LT.1 ) .OR. ( ICOMPZ.GT.0 .AND. LDZ.LT.MAX( 1,
     $         N ) ) ) THEN
         INFO = -6
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSTEQR', -INFO )
         RETURN
      END IF
      IF( N.EQ.0 )
     $   RETURN
      IF( N.EQ.1 ) THEN
         IF( ICOMPZ.EQ.2 )
     $      Z( 1, 1 ) = ONE
         RETURN
      END IF
      EPS = DLAMCH( 'E' )
      EPS2 = EPS**2
      SAFMIN = DLAMCH( 'S' )
      SAFMAX = ONE / SAFMIN
      SSFMAX = SQRT( SAFMAX ) / THREE
      SSFMIN = SQRT( SAFMIN ) / EPS2
      IF( ICOMPZ.EQ.2 )
     $   CALL DLASET( 'Full', N, N, ZERO, ONE, Z, LDZ )
      NMAXIT = N*MAXIT
      JTOT = 0
      L1 = 1
      NM1 = N - 1
   10 CONTINUE
      IF( L1.GT.N )
     $   GO TO 160
      IF( L1.GT.1 )
     $   E( L1-1 ) = ZERO
      IF( L1.LE.NM1 ) THEN
         DO 20 M = L1, NM1
            TST = ABS( E( M ) )
            IF( TST.EQ.ZERO )
     $         GO TO 30
            IF( TST.LE.( SQRT( ABS( D( M ) ) )*SQRT( ABS( D( M+
     $          1 ) ) ) )*EPS ) THEN
               E( M ) = ZERO
               GO TO 30
            END IF
   20    CONTINUE
      END IF
      M = N
   30 CONTINUE
      L = L1
      LSV = L
      LEND = M
      LENDSV = LEND
      L1 = M + 1
      IF( LEND.EQ.L )
     $   GO TO 10
      ANORM = DLANST( 'M', LEND-L+1, D( L ), E( L ) )
      ISCALE = 0
      IF( ANORM.EQ.ZERO )
     $   GO TO 10
      IF( ANORM.GT.SSFMAX ) THEN
         ISCALE = 1
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMAX, LEND-L+1, 1, D( L ), N,
     $                INFO )
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMAX, LEND-L, 1, E( L ), N,
     $                INFO )
      ELSE IF( ANORM.LT.SSFMIN ) THEN
         ISCALE = 2
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMIN, LEND-L+1, 1, D( L ), N,
     $                INFO )
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMIN, LEND-L, 1, E( L ), N,
     $                INFO )
      END IF
      IF( ABS( D( LEND ) ).LT.ABS( D( L ) ) ) THEN
         LEND = LSV
         L = LENDSV
      END IF
      IF( LEND.GT.L ) THEN
   40    CONTINUE
         IF( L.NE.LEND ) THEN
            LENDM1 = LEND - 1
            DO 50 M = L, LENDM1
               TST = ABS( E( M ) )**2
               IF( TST.LE.( EPS2*ABS( D( M ) ) )*ABS( D( M+1 ) )+
     $             SAFMIN )GO TO 60
   50       CONTINUE
         END IF
         M = LEND
   60    CONTINUE
         IF( M.LT.LEND )
     $      E( M ) = ZERO
         P = D( L )
         IF( M.EQ.L )
     $      GO TO 80
         IF( M.EQ.L+1 ) THEN
            IF( ICOMPZ.GT.0 ) THEN
               CALL DLAEV2( D( L ), E( L ), D( L+1 ), RT1, RT2, C, S )
               WORK( L ) = C
               WORK( N-1+L ) = S
               CALL DLASR( 'R', 'V', 'B', N, 2, WORK( L ),
     $                     WORK( N-1+L ), Z( 1, L ), LDZ )
            ELSE
               CALL DLAE2( D( L ), E( L ), D( L+1 ), RT1, RT2 )
            END IF
            D( L ) = RT1
            D( L+1 ) = RT2
            E( L ) = ZERO
            L = L + 2
            IF( L.LE.LEND )
     $         GO TO 40
            GO TO 140
         END IF
         IF( JTOT.EQ.NMAXIT )
     $      GO TO 140
         JTOT = JTOT + 1
         G = ( D( L+1 )-P ) / ( TWO*E( L ) )
         R = DLAPY2( G, ONE )
         G = D( M ) - P + ( E( L ) / ( G+SIGN( R, G ) ) )
         S = ONE
         C = ONE
         P = ZERO
         MM1 = M - 1
         DO 70 I = MM1, L, -1
            F = S*E( I )
            B = C*E( I )
            CALL DLARTG( G, F, C, S, R )
            IF( I.NE.M-1 )
     $         E( I+1 ) = R
            G = D( I+1 ) - P
            R = ( D( I )-G )*S + TWO*C*B
            P = S*R
            D( I+1 ) = G + P
            G = C*R - B
            IF( ICOMPZ.GT.0 ) THEN
               WORK( I ) = C
               WORK( N-1+I ) = -S
            END IF
   70    CONTINUE
         IF( ICOMPZ.GT.0 ) THEN
            MM = M - L + 1
            CALL DLASR( 'R', 'V', 'B', N, MM, WORK( L ), WORK( N-1+L ),
     $                  Z( 1, L ), LDZ )
         END IF
         D( L ) = D( L ) - P
         E( L ) = G
         GO TO 40
   80    CONTINUE
         D( L ) = P
         L = L + 1
         IF( L.LE.LEND )
     $      GO TO 40
         GO TO 140
      ELSE
   90    CONTINUE
         IF( L.NE.LEND ) THEN
            LENDP1 = LEND + 1
            DO 100 M = L, LENDP1, -1
               TST = ABS( E( M-1 ) )**2
               IF( TST.LE.( EPS2*ABS( D( M ) ) )*ABS( D( M-1 ) )+
     $             SAFMIN )GO TO 110
  100       CONTINUE
         END IF
         M = LEND
  110    CONTINUE
         IF( M.GT.LEND )
     $      E( M-1 ) = ZERO
         P = D( L )
         IF( M.EQ.L )
     $      GO TO 130
         IF( M.EQ.L-1 ) THEN
            IF( ICOMPZ.GT.0 ) THEN
               CALL DLAEV2( D( L-1 ), E( L-1 ), D( L ), RT1, RT2, C, S )
               WORK( M ) = C
               WORK( N-1+M ) = S
               CALL DLASR( 'R', 'V', 'F', N, 2, WORK( M ),
     $                     WORK( N-1+M ), Z( 1, L-1 ), LDZ )
            ELSE
               CALL DLAE2( D( L-1 ), E( L-1 ), D( L ), RT1, RT2 )
            END IF
            D( L-1 ) = RT1
            D( L ) = RT2
            E( L-1 ) = ZERO
            L = L - 2
            IF( L.GE.LEND )
     $         GO TO 90
            GO TO 140
         END IF
         IF( JTOT.EQ.NMAXIT )
     $      GO TO 140
         JTOT = JTOT + 1
         G = ( D( L-1 )-P ) / ( TWO*E( L-1 ) )
         R = DLAPY2( G, ONE )
         G = D( M ) - P + ( E( L-1 ) / ( G+SIGN( R, G ) ) )
         S = ONE
         C = ONE
         P = ZERO
         LM1 = L - 1
         DO 120 I = M, LM1
            F = S*E( I )
            B = C*E( I )
            CALL DLARTG( G, F, C, S, R )
            IF( I.NE.M )
     $         E( I-1 ) = R
            G = D( I ) - P
            R = ( D( I+1 )-G )*S + TWO*C*B
            P = S*R
            D( I ) = G + P
            G = C*R - B
            IF( ICOMPZ.GT.0 ) THEN
               WORK( I ) = C
               WORK( N-1+I ) = S
            END IF
  120    CONTINUE
         IF( ICOMPZ.GT.0 ) THEN
            MM = L - M + 1
            CALL DLASR( 'R', 'V', 'F', N, MM, WORK( M ), WORK( N-1+M ),
     $                  Z( 1, M ), LDZ )
         END IF
         D( L ) = D( L ) - P
         E( LM1 ) = G
         GO TO 90
  130    CONTINUE
         D( L ) = P
         L = L - 1
         IF( L.GE.LEND )
     $      GO TO 90
         GO TO 140
      END IF
  140 CONTINUE
      IF( ISCALE.EQ.1 ) THEN
         CALL DLASCL( 'G', 0, 0, SSFMAX, ANORM, LENDSV-LSV+1, 1,
     $                D( LSV ), N, INFO )
         CALL DLASCL( 'G', 0, 0, SSFMAX, ANORM, LENDSV-LSV, 1, E( LSV ),
     $                N, INFO )
      ELSE IF( ISCALE.EQ.2 ) THEN
         CALL DLASCL( 'G', 0, 0, SSFMIN, ANORM, LENDSV-LSV+1, 1,
     $                D( LSV ), N, INFO )
         CALL DLASCL( 'G', 0, 0, SSFMIN, ANORM, LENDSV-LSV, 1, E( LSV ),
     $                N, INFO )
      END IF
      IF( JTOT.LT.NMAXIT )
     $   GO TO 10
      DO 150 I = 1, N - 1
         IF( E( I ).NE.ZERO )
     $      INFO = INFO + 1
  150 CONTINUE
      GO TO 190
  160 CONTINUE
      IF( ICOMPZ.EQ.0 ) THEN
         CALL DLASRT( 'I', N, D, INFO )
      ELSE
         DO 180 II = 2, N
            I = II - 1
            K = I
            P = D( I )
            DO 170 J = II, N
               IF( D( J ).LT.P ) THEN
                  K = J
                  P = D( J )
               END IF
  170       CONTINUE
            IF( K.NE.I ) THEN
               D( K ) = D( I )
               D( I ) = P
               CALL DSWAP( N, Z( 1, I ), 1, Z( 1, K ), 1 )
            END IF
  180    CONTINUE
      END IF
  190 CONTINUE
      RETURN
      END
      SUBROUTINE DLASET( UPLO, M, N, ALPHA, BETA, A, LDA )
      CHARACTER          UPLO
      INTEGER            LDA, M, N
      DOUBLE PRECISION   ALPHA, BETA
      DOUBLE PRECISION   A( LDA, * )
      INTEGER            I, J
      LOGICAL            LSAME
      EXTERNAL           LSAME
      INTRINSIC          MIN
      IF( LSAME( UPLO, 'U' ) ) THEN
         DO 20 J = 2, N
            DO 10 I = 1, MIN( J-1, M )
               A( I, J ) = ALPHA
   10       CONTINUE
   20    CONTINUE
      ELSE IF( LSAME( UPLO, 'L' ) ) THEN
         DO 40 J = 1, MIN( M, N )
            DO 30 I = J + 1, M
               A( I, J ) = ALPHA
   30       CONTINUE
   40    CONTINUE
      ELSE
         DO 60 J = 1, N
            DO 50 I = 1, M
               A( I, J ) = ALPHA
   50       CONTINUE
   60    CONTINUE
      END IF
      DO 70 I = 1, MIN( M, N )
         A( I, I ) = BETA
   70 CONTINUE
      RETURN
      END
      SUBROUTINE DLAED0( ICOMPQ, QSIZ, N, D, E, Q, LDQ, QSTORE, LDQS,
     $                   WORK, IWORK, INFO )
      INTEGER            ICOMPQ, INFO, LDQ, LDQS, N, QSIZ
      INTEGER            IWORK( * )
      DOUBLE PRECISION   D( * ), E( * ), Q( LDQ, * ), QSTORE( LDQS, * ),
     $                   WORK( * )
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.D0, ONE = 1.D0, TWO = 2.D0 )
      INTEGER            CURLVL, CURPRB, CURR, I, IGIVCL, IGIVNM,
     $                   IGIVPT, INDXQ, IPERM, IPRMPT, IQ, IQPTR, IWREM,
     $                   J, K, LGN, MATSIZ, MSD2, SMLSIZ, SMM1, SPM1,
     $                   SPM2, SUBMAT, SUBPBS, TLVLS
      DOUBLE PRECISION   TEMP
      EXTERNAL           DCOPY, DGEMM, DLACPY, DLAED1, DLAED7, DSTEQR,
     $                   XERBLA
      INTEGER            ILAENV
      EXTERNAL           ILAENV
      INTRINSIC          ABS, DBLE, INT, LOG, MAX
      INFO = 0
      IF( ICOMPQ.LT.0 .OR. ICOMPQ.GT.2 ) THEN
         INFO = -1
      ELSE IF( ( ICOMPQ.EQ.1 ) .AND. ( QSIZ.LT.MAX( 0, N ) ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDQ.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDQS.LT.MAX( 1, N ) ) THEN
         INFO = -9
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLAED0', -INFO )
         RETURN
      END IF
      IF( N.EQ.0 )
     $   RETURN
      SMLSIZ = ILAENV( 9, 'DLAED0', ' ', 0, 0, 0, 0 )
      IWORK( 1 ) = N
      SUBPBS = 1
      TLVLS = 0
   10 CONTINUE
      IF( IWORK( SUBPBS ).GT.SMLSIZ ) THEN
         DO 20 J = SUBPBS, 1, -1
            IWORK( 2*J ) = ( IWORK( J )+1 ) / 2
            IWORK( 2*J-1 ) = IWORK( J ) / 2
   20    CONTINUE
         TLVLS = TLVLS + 1
         SUBPBS = 2*SUBPBS
         GO TO 10
      END IF
      DO 30 J = 2, SUBPBS
         IWORK( J ) = IWORK( J ) + IWORK( J-1 )
   30 CONTINUE
      SPM1 = SUBPBS - 1
      DO 40 I = 1, SPM1
         SUBMAT = IWORK( I ) + 1
         SMM1 = SUBMAT - 1
         D( SMM1 ) = D( SMM1 ) - ABS( E( SMM1 ) )
         D( SUBMAT ) = D( SUBMAT ) - ABS( E( SMM1 ) )
   40 CONTINUE
      INDXQ = 4*N + 3
      IF( ICOMPQ.NE.2 ) THEN
         TEMP = LOG( DBLE( N ) ) / LOG( TWO )
         LGN = INT( TEMP )
         IF( 2**LGN.LT.N )
     $      LGN = LGN + 1
         IF( 2**LGN.LT.N )
     $      LGN = LGN + 1
         IPRMPT = INDXQ + N + 1
         IPERM = IPRMPT + N*LGN
         IQPTR = IPERM + N*LGN
         IGIVPT = IQPTR + N + 2
         IGIVCL = IGIVPT + N*LGN
         IGIVNM = 1
         IQ = IGIVNM + 2*N*LGN
         IWREM = IQ + N**2 + 1
         DO 50 I = 0, SUBPBS
            IWORK( IPRMPT+I ) = 1
            IWORK( IGIVPT+I ) = 1
   50    CONTINUE
         IWORK( IQPTR ) = 1
      END IF
      CURR = 0
      DO 70 I = 0, SPM1
         IF( I.EQ.0 ) THEN
            SUBMAT = 1
            MATSIZ = IWORK( 1 )
         ELSE
            SUBMAT = IWORK( I ) + 1
            MATSIZ = IWORK( I+1 ) - IWORK( I )
         END IF
         IF( ICOMPQ.EQ.2 ) THEN
            CALL DSTEQR( 'I', MATSIZ, D( SUBMAT ), E( SUBMAT ),
     $                   Q( SUBMAT, SUBMAT ), LDQ, WORK, INFO )
            IF( INFO.NE.0 )
     $         GO TO 130
         ELSE
            CALL DSTEQR( 'I', MATSIZ, D( SUBMAT ), E( SUBMAT ),
     $                   WORK( IQ-1+IWORK( IQPTR+CURR ) ), MATSIZ, WORK,
     $                   INFO )
            IF( INFO.NE.0 )
     $         GO TO 130
            IF( ICOMPQ.EQ.1 ) THEN
               CALL DGEMM( 'N', 'N', QSIZ, MATSIZ, MATSIZ, ONE,
     $                     Q( 1, SUBMAT ), LDQ, WORK( IQ-1+IWORK( IQPTR+
     $                     CURR ) ), MATSIZ, ZERO, QSTORE( 1, SUBMAT ),
     $                     LDQS )
            END IF
            IWORK( IQPTR+CURR+1 ) = IWORK( IQPTR+CURR ) + MATSIZ**2
            CURR = CURR + 1
         END IF
         K = 1
         DO 60 J = SUBMAT, IWORK( I+1 )
            IWORK( INDXQ+J ) = K
            K = K + 1
   60    CONTINUE
   70 CONTINUE
      CURLVL = 1
   80 CONTINUE
      IF( SUBPBS.GT.1 ) THEN
         SPM2 = SUBPBS - 2
         DO 90 I = 0, SPM2, 2
            IF( I.EQ.0 ) THEN
               SUBMAT = 1
               MATSIZ = IWORK( 2 )
               MSD2 = IWORK( 1 )
               CURPRB = 0
            ELSE
               SUBMAT = IWORK( I ) + 1
               MATSIZ = IWORK( I+2 ) - IWORK( I )
               MSD2 = MATSIZ / 2
               CURPRB = CURPRB + 1
            END IF
            IF( ICOMPQ.EQ.2 ) THEN
               CALL DLAED1( MATSIZ, D( SUBMAT ), Q( SUBMAT, SUBMAT ),
     $                      LDQ, IWORK( INDXQ+SUBMAT ),
     $                      E( SUBMAT+MSD2-1 ), MSD2, WORK,
     $                      IWORK( SUBPBS+1 ), INFO )
            ELSE
               CALL DLAED7( ICOMPQ, MATSIZ, QSIZ, TLVLS, CURLVL, CURPRB,
     $                      D( SUBMAT ), QSTORE( 1, SUBMAT ), LDQS,
     $                      IWORK( INDXQ+SUBMAT ), E( SUBMAT+MSD2-1 ),
     $                      MSD2, WORK( IQ ), IWORK( IQPTR ),
     $                      IWORK( IPRMPT ), IWORK( IPERM ),
     $                      IWORK( IGIVPT ), IWORK( IGIVCL ),
     $                      WORK( IGIVNM ), WORK( IWREM ),
     $                      IWORK( SUBPBS+1 ), INFO )
            END IF
            IF( INFO.NE.0 )
     $         GO TO 130
            IWORK( I / 2+1 ) = IWORK( I+2 )
   90    CONTINUE
         SUBPBS = SUBPBS / 2
         CURLVL = CURLVL + 1
         GO TO 80
      END IF
      IF( ICOMPQ.EQ.1 ) THEN
         DO 100 I = 1, N
            J = IWORK( INDXQ+I )
            WORK( I ) = D( J )
            CALL DCOPY( QSIZ, QSTORE( 1, J ), 1, Q( 1, I ), 1 )
  100    CONTINUE
         CALL DCOPY( N, WORK, 1, D, 1 )
      ELSE IF( ICOMPQ.EQ.2 ) THEN
         DO 110 I = 1, N
            J = IWORK( INDXQ+I )
            WORK( I ) = D( J )
            CALL DCOPY( N, Q( 1, J ), 1, WORK( N*I+1 ), 1 )
  110    CONTINUE
         CALL DCOPY( N, WORK, 1, D, 1 )
         CALL DLACPY( 'A', N, N, WORK( N+1 ), N, Q, LDQ )
      ELSE
         DO 120 I = 1, N
            J = IWORK( INDXQ+I )
            WORK( I ) = D( J )
  120    CONTINUE
         CALL DCOPY( N, WORK, 1, D, 1 )
      END IF
      GO TO 140
  130 CONTINUE
      INFO = SUBMAT*( N+1 ) + SUBMAT + MATSIZ - 1
  140 CONTINUE
      RETURN
      END
      SUBROUTINE DLARFG( N, ALPHA, X, INCX, TAU )
      INTEGER            INCX, N
      DOUBLE PRECISION   ALPHA, TAU
      DOUBLE PRECISION   X( * )
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      INTEGER            J, KNT
      DOUBLE PRECISION   BETA, RSAFMN, SAFMIN, XNORM
      DOUBLE PRECISION   DLAMCH, DLAPY2, DNRM2
      EXTERNAL           DLAMCH, DLAPY2, DNRM2
      INTRINSIC          ABS, SIGN
      EXTERNAL           DSCAL
      IF( N.LE.1 ) THEN
         TAU = ZERO
         RETURN
      END IF
      XNORM = DNRM2( N-1, X, INCX )
      IF( XNORM.EQ.ZERO ) THEN
         TAU = ZERO
      ELSE
         BETA = -SIGN( DLAPY2( ALPHA, XNORM ), ALPHA )
         SAFMIN = DLAMCH( 'S' ) / DLAMCH( 'E' )
         KNT = 0
         IF( ABS( BETA ).LT.SAFMIN ) THEN
            RSAFMN = ONE / SAFMIN
   10       CONTINUE
            KNT = KNT + 1
            CALL DSCAL( N-1, RSAFMN, X, INCX )
            BETA = BETA*RSAFMN
            ALPHA = ALPHA*RSAFMN
            IF( (ABS( BETA ).LT.SAFMIN) .AND. (KNT .LT. 20) )
     $         GO TO 10
            XNORM = DNRM2( N-1, X, INCX )
            BETA = -SIGN( DLAPY2( ALPHA, XNORM ), ALPHA )
         END IF
         TAU = ( BETA-ALPHA ) / BETA
         CALL DSCAL( N-1, ONE / ( ALPHA-BETA ), X, INCX )
         DO 20 J = 1, KNT
            BETA = BETA*SAFMIN
 20      CONTINUE
         ALPHA = BETA
      END IF
      RETURN
      END
      SUBROUTINE DSYMV(UPLO,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
      DOUBLE PRECISION ALPHA,BETA
      INTEGER INCX,INCY,LDA,N
      CHARACTER UPLO
      DOUBLE PRECISION A(LDA,*),X(*),Y(*)
      DOUBLE PRECISION ONE,ZERO
      PARAMETER (ONE=1.0D+0,ZERO=0.0D+0)
      DOUBLE PRECISION TEMP1,TEMP2
      INTEGER I,INFO,IX,IY,J,JX,JY,KX,KY
      LOGICAL LSAME
      EXTERNAL LSAME
      EXTERNAL XERBLA
      INTRINSIC MAX
      INFO = 0
      IF (.NOT.LSAME(UPLO,'U') .AND. .NOT.LSAME(UPLO,'L')) THEN
          INFO = 1
      ELSE IF (N.LT.0) THEN
          INFO = 2
      ELSE IF (LDA.LT.MAX(1,N)) THEN
          INFO = 5
      ELSE IF (INCX.EQ.0) THEN
          INFO = 7
      ELSE IF (INCY.EQ.0) THEN
          INFO = 10
      END IF
      IF (INFO.NE.0) THEN
          CALL XERBLA('DSYMV ',INFO)
          RETURN
      END IF
      IF ((N.EQ.0) .OR. ((ALPHA.EQ.ZERO).AND. (BETA.EQ.ONE))) RETURN
      IF (INCX.GT.0) THEN
          KX = 1
      ELSE
          KX = 1 - (N-1)*INCX
      END IF
      IF (INCY.GT.0) THEN
          KY = 1
      ELSE
          KY = 1 - (N-1)*INCY
      END IF
      IF (BETA.NE.ONE) THEN
          IF (INCY.EQ.1) THEN
              IF (BETA.EQ.ZERO) THEN
                  DO 10 I = 1,N
                      Y(I) = ZERO
   10             CONTINUE
              ELSE
                  DO 20 I = 1,N
                      Y(I) = BETA*Y(I)
   20             CONTINUE
              END IF
          ELSE
              IY = KY
              IF (BETA.EQ.ZERO) THEN
                  DO 30 I = 1,N
                      Y(IY) = ZERO
                      IY = IY + INCY
   30             CONTINUE
              ELSE
                  DO 40 I = 1,N
                      Y(IY) = BETA*Y(IY)
                      IY = IY + INCY
   40             CONTINUE
              END IF
          END IF
      END IF
      IF (ALPHA.EQ.ZERO) RETURN
      IF (LSAME(UPLO,'U')) THEN
          IF ((INCX.EQ.1) .AND. (INCY.EQ.1)) THEN
              DO 60 J = 1,N
                  TEMP1 = ALPHA*X(J)
                  TEMP2 = ZERO
                  DO 50 I = 1,J - 1
                      Y(I) = Y(I) + TEMP1*A(I,J)
                      TEMP2 = TEMP2 + A(I,J)*X(I)
   50             CONTINUE
                  Y(J) = Y(J) + TEMP1*A(J,J) + ALPHA*TEMP2
   60         CONTINUE
          ELSE
              JX = KX
              JY = KY
              DO 80 J = 1,N
                  TEMP1 = ALPHA*X(JX)
                  TEMP2 = ZERO
                  IX = KX
                  IY = KY
                  DO 70 I = 1,J - 1
                      Y(IY) = Y(IY) + TEMP1*A(I,J)
                      TEMP2 = TEMP2 + A(I,J)*X(IX)
                      IX = IX + INCX
                      IY = IY + INCY
   70             CONTINUE
                  Y(JY) = Y(JY) + TEMP1*A(J,J) + ALPHA*TEMP2
                  JX = JX + INCX
                  JY = JY + INCY
   80         CONTINUE
          END IF
      ELSE
          IF ((INCX.EQ.1) .AND. (INCY.EQ.1)) THEN
              DO 100 J = 1,N
                  TEMP1 = ALPHA*X(J)
                  TEMP2 = ZERO
                  Y(J) = Y(J) + TEMP1*A(J,J)
                  DO 90 I = J + 1,N
                      Y(I) = Y(I) + TEMP1*A(I,J)
                      TEMP2 = TEMP2 + A(I,J)*X(I)
   90             CONTINUE
                  Y(J) = Y(J) + ALPHA*TEMP2
  100         CONTINUE
          ELSE
              JX = KX
              JY = KY
              DO 120 J = 1,N
                  TEMP1 = ALPHA*X(JX)
                  TEMP2 = ZERO
                  Y(JY) = Y(JY) + TEMP1*A(J,J)
                  IX = JX
                  IY = JY
                  DO 110 I = J + 1,N
                      IX = IX + INCX
                      IY = IY + INCY
                      Y(IY) = Y(IY) + TEMP1*A(I,J)
                      TEMP2 = TEMP2 + A(I,J)*X(IX)
  110             CONTINUE
                  Y(JY) = Y(JY) + ALPHA*TEMP2
                  JX = JX + INCX
                  JY = JY + INCY
  120         CONTINUE
          END IF
      END IF
      RETURN
      END
      SUBROUTINE DSYR2(UPLO,N,ALPHA,X,INCX,Y,INCY,A,LDA)
      DOUBLE PRECISION ALPHA
      INTEGER INCX,INCY,LDA,N
      CHARACTER UPLO
      DOUBLE PRECISION A(LDA,*),X(*),Y(*)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D+0)
      DOUBLE PRECISION TEMP1,TEMP2
      INTEGER I,INFO,IX,IY,J,JX,JY,KX,KY
      LOGICAL LSAME
      EXTERNAL LSAME
      EXTERNAL XERBLA
      INTRINSIC MAX
      INFO = 0
      IF (.NOT.LSAME(UPLO,'U') .AND. .NOT.LSAME(UPLO,'L')) THEN
          INFO = 1
      ELSE IF (N.LT.0) THEN
          INFO = 2
      ELSE IF (INCX.EQ.0) THEN
          INFO = 5
      ELSE IF (INCY.EQ.0) THEN
          INFO = 7
      ELSE IF (LDA.LT.MAX(1,N)) THEN
          INFO = 9
      END IF
      IF (INFO.NE.0) THEN
          CALL XERBLA('DSYR2 ',INFO)
          RETURN
      END IF
      IF ((N.EQ.0) .OR. (ALPHA.EQ.ZERO)) RETURN
      IF ((INCX.NE.1) .OR. (INCY.NE.1)) THEN
          IF (INCX.GT.0) THEN
              KX = 1
          ELSE
              KX = 1 - (N-1)*INCX
          END IF
          IF (INCY.GT.0) THEN
              KY = 1
          ELSE
              KY = 1 - (N-1)*INCY
          END IF
          JX = KX
          JY = KY
      END IF
      IF (LSAME(UPLO,'U')) THEN
          IF ((INCX.EQ.1) .AND. (INCY.EQ.1)) THEN
              DO 20 J = 1,N
                  IF ((X(J).NE.ZERO) .OR. (Y(J).NE.ZERO)) THEN
                      TEMP1 = ALPHA*Y(J)
                      TEMP2 = ALPHA*X(J)
                      DO 10 I = 1,J
                          A(I,J) = A(I,J) + X(I)*TEMP1 + Y(I)*TEMP2
   10                 CONTINUE
                  END IF
   20         CONTINUE
          ELSE
              DO 40 J = 1,N
                  IF ((X(JX).NE.ZERO) .OR. (Y(JY).NE.ZERO)) THEN
                      TEMP1 = ALPHA*Y(JY)
                      TEMP2 = ALPHA*X(JX)
                      IX = KX
                      IY = KY
                      DO 30 I = 1,J
                          A(I,J) = A(I,J) + X(IX)*TEMP1 + Y(IY)*TEMP2
                          IX = IX + INCX
                          IY = IY + INCY
   30                 CONTINUE
                  END IF
                  JX = JX + INCX
                  JY = JY + INCY
   40         CONTINUE
          END IF
      ELSE
          IF ((INCX.EQ.1) .AND. (INCY.EQ.1)) THEN
              DO 60 J = 1,N
                  IF ((X(J).NE.ZERO) .OR. (Y(J).NE.ZERO)) THEN
                      TEMP1 = ALPHA*Y(J)
                      TEMP2 = ALPHA*X(J)
                      DO 50 I = J,N
                          A(I,J) = A(I,J) + X(I)*TEMP1 + Y(I)*TEMP2
   50                 CONTINUE
                  END IF
   60         CONTINUE
          ELSE
              DO 80 J = 1,N
                  IF ((X(JX).NE.ZERO) .OR. (Y(JY).NE.ZERO)) THEN
                      TEMP1 = ALPHA*Y(JY)
                      TEMP2 = ALPHA*X(JX)
                      IX = JX
                      IY = JY
                      DO 70 I = J,N
                          A(I,J) = A(I,J) + X(IX)*TEMP1 + Y(IY)*TEMP2
                          IX = IX + INCX
                          IY = IY + INCY
   70                 CONTINUE
                  END IF
                  JX = JX + INCX
                  JY = JY + INCY
   80         CONTINUE
          END IF
      END IF
      RETURN
      END
      SUBROUTINE DORM2L( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, INFO )
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, M, N
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
      LOGICAL            LEFT, NOTRAN
      INTEGER            I, I1, I2, I3, MI, NI, NQ
      DOUBLE PRECISION   AII
      LOGICAL            LSAME
      EXTERNAL           LSAME
      EXTERNAL           DLARF, XERBLA
      INTRINSIC          MAX
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
      IF( LEFT ) THEN
         NQ = M
      ELSE
         NQ = N
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORM2L', -INFO )
         RETURN
      END IF
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 )
     $   RETURN
      IF( ( LEFT .AND. NOTRAN ) .OR. ( .NOT.LEFT .AND. .NOT.NOTRAN ) )
     $     THEN
         I1 = 1
         I2 = K
         I3 = 1
      ELSE
         I1 = K
         I2 = 1
         I3 = -1
      END IF
      IF( LEFT ) THEN
         NI = N
      ELSE
         MI = M
      END IF
      DO 10 I = I1, I2, I3
         IF( LEFT ) THEN
            MI = M - K + I
         ELSE
            NI = N - K + I
         END IF
         AII = A( NQ-K+I, I )
         A( NQ-K+I, I ) = ONE
         CALL DLARF( SIDE, MI, NI, A( 1, I ), 1, TAU( I ), C, LDC,
     $               WORK )
         A( NQ-K+I, I ) = AII
   10 CONTINUE
      RETURN
      END
      SUBROUTINE DLARFT( DIRECT, STOREV, N, K, V, LDV, TAU, T, LDT )
      CHARACTER          DIRECT, STOREV
      INTEGER            K, LDT, LDV, N
      DOUBLE PRECISION   T( LDT, * ), TAU( * ), V( LDV, * )
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      INTEGER            I, J, PREVLASTV, LASTV
      EXTERNAL           DGEMV, DTRMV
      LOGICAL            LSAME
      EXTERNAL           LSAME
      IF( N.EQ.0 )
     $   RETURN
      IF( LSAME( DIRECT, 'F' ) ) THEN
         PREVLASTV = N
         DO I = 1, K
            PREVLASTV = MAX( I, PREVLASTV )
            IF( TAU( I ).EQ.ZERO ) THEN
               DO J = 1, I
                  T( J, I ) = ZERO
               END DO
            ELSE
               IF( LSAME( STOREV, 'C' ) ) THEN
                  DO LASTV = N, I+1, -1
                     IF( V( LASTV, I ).NE.ZERO ) EXIT
                  END DO
                  DO J = 1, I-1
                     T( J, I ) = -TAU( I ) * V( I , J )
                  END DO
                  J = MIN( LASTV, PREVLASTV )
                  CALL DGEMV( 'Transpose', J-I, I-1, -TAU( I ),
     $                        V( I+1, 1 ), LDV, V( I+1, I ), 1, ONE,
     $                        T( 1, I ), 1 )
               ELSE
                  DO LASTV = N, I+1, -1
                     IF( V( I, LASTV ).NE.ZERO ) EXIT
                  END DO
                  DO J = 1, I-1
                     T( J, I ) = -TAU( I ) * V( J , I )
                  END DO
                  J = MIN( LASTV, PREVLASTV )
                  CALL DGEMV( 'No transpose', I-1, J-I, -TAU( I ),
     $                        V( 1, I+1 ), LDV, V( I, I+1 ), LDV, ONE,
     $                        T( 1, I ), 1 )
               END IF
               CALL DTRMV( 'Upper', 'No transpose', 'Non-unit', I-1, T,
     $                     LDT, T( 1, I ), 1 )
               T( I, I ) = TAU( I )
               IF( I.GT.1 ) THEN
                  PREVLASTV = MAX( PREVLASTV, LASTV )
               ELSE
                  PREVLASTV = LASTV
               END IF
            END IF
         END DO
      ELSE
         PREVLASTV = 1
         DO I = K, 1, -1
            IF( TAU( I ).EQ.ZERO ) THEN
               DO J = I, K
                  T( J, I ) = ZERO
               END DO
            ELSE
               IF( I.LT.K ) THEN
                  IF( LSAME( STOREV, 'C' ) ) THEN
                     DO LASTV = 1, I-1
                        IF( V( LASTV, I ).NE.ZERO ) EXIT
                     END DO
                     DO J = I+1, K
                        T( J, I ) = -TAU( I ) * V( N-K+I , J )
                     END DO
                     J = MAX( LASTV, PREVLASTV )
                     CALL DGEMV( 'Transpose', N-K+I-J, K-I, -TAU( I ),
     $                           V( J, I+1 ), LDV, V( J, I ), 1, ONE,
     $                           T( I+1, I ), 1 )
                  ELSE
                     DO LASTV = 1, I-1
                        IF( V( I, LASTV ).NE.ZERO ) EXIT
                     END DO
                     DO J = I+1, K
                        T( J, I ) = -TAU( I ) * V( J, N-K+I )
                     END DO
                     J = MAX( LASTV, PREVLASTV )
                     CALL DGEMV( 'No transpose', K-I, N-K+I-J,
     $                    -TAU( I ), V( I+1, J ), LDV, V( I, J ), LDV,
     $                    ONE, T( I+1, I ), 1 )
                  END IF
                  CALL DTRMV( 'Lower', 'No transpose', 'Non-unit', K-I,
     $                        T( I+1, I+1 ), LDT, T( I+1, I ), 1 )
                  IF( I.GT.1 ) THEN
                     PREVLASTV = MIN( PREVLASTV, LASTV )
                  ELSE
                     PREVLASTV = LASTV
                  END IF
               END IF
               T( I, I ) = TAU( I )
            END IF
         END DO
      END IF
      RETURN
      END
      SUBROUTINE DLARFB( SIDE, TRANS, DIRECT, STOREV, M, N, K, V, LDV,
     $                   T, LDT, C, LDC, WORK, LDWORK )
      CHARACTER          DIRECT, SIDE, STOREV, TRANS
      INTEGER            K, LDC, LDT, LDV, LDWORK, M, N
      DOUBLE PRECISION   C( LDC, * ), T( LDT, * ), V( LDV, * ),
     $                   WORK( LDWORK, * )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
      CHARACTER          TRANST
      INTEGER            I, J
      LOGICAL            LSAME
      EXTERNAL           LSAME
      EXTERNAL           DCOPY, DGEMM, DTRMM
      IF( M.LE.0 .OR. N.LE.0 )
     $   RETURN
      IF( LSAME( TRANS, 'N' ) ) THEN
         TRANST = 'T'
      ELSE
         TRANST = 'N'
      END IF
      IF( LSAME( STOREV, 'C' ) ) THEN
         IF( LSAME( DIRECT, 'F' ) ) THEN
            IF( LSAME( SIDE, 'L' ) ) THEN
               DO 10 J = 1, K
                  CALL DCOPY( N, C( J, 1 ), LDC, WORK( 1, J ), 1 )
   10          CONTINUE
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', N,
     $                     K, ONE, V, LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
                  CALL DGEMM( 'Transpose', 'No transpose', N, K, M-K,
     $                        ONE, C( K+1, 1 ), LDC, V( K+1, 1 ), LDV,
     $                        ONE, WORK, LDWORK )
               END IF
               CALL DTRMM( 'Right', 'Upper', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
               IF( M.GT.K ) THEN
                  CALL DGEMM( 'No transpose', 'Transpose', M-K, N, K,
     $                        -ONE, V( K+1, 1 ), LDV, WORK, LDWORK, ONE,
     $                        C( K+1, 1 ), LDC )
               END IF
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', N, K,
     $                     ONE, V, LDV, WORK, LDWORK )
               DO 30 J = 1, K
                  DO 20 I = 1, N
                     C( J, I ) = C( J, I ) - WORK( I, J )
   20             CONTINUE
   30          CONTINUE
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
               DO 40 J = 1, K
                  CALL DCOPY( M, C( 1, J ), 1, WORK( 1, J ), 1 )
   40          CONTINUE
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', M,
     $                     K, ONE, V, LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
                  CALL DGEMM( 'No transpose', 'No transpose', M, K, N-K,
     $                        ONE, C( 1, K+1 ), LDC, V( K+1, 1 ), LDV,
     $                        ONE, WORK, LDWORK )
               END IF
               CALL DTRMM( 'Right', 'Upper', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
               IF( N.GT.K ) THEN
                  CALL DGEMM( 'No transpose', 'Transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V( K+1, 1 ), LDV, ONE,
     $                        C( 1, K+1 ), LDC )
               END IF
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', M, K,
     $                     ONE, V, LDV, WORK, LDWORK )
               DO 60 J = 1, K
                  DO 50 I = 1, M
                     C( I, J ) = C( I, J ) - WORK( I, J )
   50             CONTINUE
   60          CONTINUE
            END IF
         ELSE
            IF( LSAME( SIDE, 'L' ) ) THEN
               DO 70 J = 1, K
                  CALL DCOPY( N, C( M-K+J, 1 ), LDC, WORK( 1, J ), 1 )
   70          CONTINUE
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', N,
     $                     K, ONE, V( M-K+1, 1 ), LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
                  CALL DGEMM( 'Transpose', 'No transpose', N, K, M-K,
     $                        ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
               CALL DTRMM( 'Right', 'Lower', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
               IF( M.GT.K ) THEN
                  CALL DGEMM( 'No transpose', 'Transpose', M-K, N, K,
     $                        -ONE, V, LDV, WORK, LDWORK, ONE, C, LDC )
               END IF
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', N, K,
     $                     ONE, V( M-K+1, 1 ), LDV, WORK, LDWORK )
               DO 90 J = 1, K
                  DO 80 I = 1, N
                     C( M-K+J, I ) = C( M-K+J, I ) - WORK( I, J )
   80             CONTINUE
   90          CONTINUE
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
               DO 100 J = 1, K
                  CALL DCOPY( M, C( 1, N-K+J ), 1, WORK( 1, J ), 1 )
  100          CONTINUE
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', M,
     $                     K, ONE, V( N-K+1, 1 ), LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
                  CALL DGEMM( 'No transpose', 'No transpose', M, K, N-K,
     $                        ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
               CALL DTRMM( 'Right', 'Lower', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
               IF( N.GT.K ) THEN
                  CALL DGEMM( 'No transpose', 'Transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V, LDV, ONE, C, LDC )
               END IF
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', M, K,
     $                     ONE, V( N-K+1, 1 ), LDV, WORK, LDWORK )
               DO 120 J = 1, K
                  DO 110 I = 1, M
                     C( I, N-K+J ) = C( I, N-K+J ) - WORK( I, J )
  110             CONTINUE
  120          CONTINUE
            END IF
         END IF
      ELSE IF( LSAME( STOREV, 'R' ) ) THEN
         IF( LSAME( DIRECT, 'F' ) ) THEN
            IF( LSAME( SIDE, 'L' ) ) THEN
               DO 130 J = 1, K
                  CALL DCOPY( N, C( J, 1 ), LDC, WORK( 1, J ), 1 )
  130          CONTINUE
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', N, K,
     $                     ONE, V, LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
                  CALL DGEMM( 'Transpose', 'Transpose', N, K, M-K, ONE,
     $                        C( K+1, 1 ), LDC, V( 1, K+1 ), LDV, ONE,
     $                        WORK, LDWORK )
               END IF
               CALL DTRMM( 'Right', 'Upper', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
               IF( M.GT.K ) THEN
                  CALL DGEMM( 'Transpose', 'Transpose', M-K, N, K, -ONE,
     $                        V( 1, K+1 ), LDV, WORK, LDWORK, ONE,
     $                        C( K+1, 1 ), LDC )
               END IF
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', N,
     $                     K, ONE, V, LDV, WORK, LDWORK )
               DO 150 J = 1, K
                  DO 140 I = 1, N
                     C( J, I ) = C( J, I ) - WORK( I, J )
  140             CONTINUE
  150          CONTINUE
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
               DO 160 J = 1, K
                  CALL DCOPY( M, C( 1, J ), 1, WORK( 1, J ), 1 )
  160          CONTINUE
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', M, K,
     $                     ONE, V, LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
                  CALL DGEMM( 'No transpose', 'Transpose', M, K, N-K,
     $                        ONE, C( 1, K+1 ), LDC, V( 1, K+1 ), LDV,
     $                        ONE, WORK, LDWORK )
               END IF
               CALL DTRMM( 'Right', 'Upper', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
               IF( N.GT.K ) THEN
                  CALL DGEMM( 'No transpose', 'No transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V( 1, K+1 ), LDV, ONE,
     $                        C( 1, K+1 ), LDC )
               END IF
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', M,
     $                     K, ONE, V, LDV, WORK, LDWORK )
               DO 180 J = 1, K
                  DO 170 I = 1, M
                     C( I, J ) = C( I, J ) - WORK( I, J )
  170             CONTINUE
  180          CONTINUE
            END IF
         ELSE
            IF( LSAME( SIDE, 'L' ) ) THEN
               DO 190 J = 1, K
                  CALL DCOPY( N, C( M-K+J, 1 ), LDC, WORK( 1, J ), 1 )
  190          CONTINUE
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', N, K,
     $                     ONE, V( 1, M-K+1 ), LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
                  CALL DGEMM( 'Transpose', 'Transpose', N, K, M-K, ONE,
     $                        C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
               CALL DTRMM( 'Right', 'Lower', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
               IF( M.GT.K ) THEN
                  CALL DGEMM( 'Transpose', 'Transpose', M-K, N, K, -ONE,
     $                        V, LDV, WORK, LDWORK, ONE, C, LDC )
               END IF
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', N,
     $                     K, ONE, V( 1, M-K+1 ), LDV, WORK, LDWORK )
               DO 210 J = 1, K
                  DO 200 I = 1, N
                     C( M-K+J, I ) = C( M-K+J, I ) - WORK( I, J )
  200             CONTINUE
  210          CONTINUE
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
               DO 220 J = 1, K
                  CALL DCOPY( M, C( 1, N-K+J ), 1, WORK( 1, J ), 1 )
  220          CONTINUE
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', M, K,
     $                     ONE, V( 1, N-K+1 ), LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
                  CALL DGEMM( 'No transpose', 'Transpose', M, K, N-K,
     $                        ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
               CALL DTRMM( 'Right', 'Lower', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
               IF( N.GT.K ) THEN
                  CALL DGEMM( 'No transpose', 'No transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V, LDV, ONE, C, LDC )
               END IF
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', M,
     $                     K, ONE, V( 1, N-K+1 ), LDV, WORK, LDWORK )
               DO 240 J = 1, K
                  DO 230 I = 1, M
                     C( I, N-K+J ) = C( I, N-K+J ) - WORK( I, J )
  230             CONTINUE
  240          CONTINUE
            END IF
         END IF
      END IF
      RETURN
      END
      SUBROUTINE DORM2R( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, INFO )
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, M, N
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
      LOGICAL            LEFT, NOTRAN
      INTEGER            I, I1, I2, I3, IC, JC, MI, NI, NQ
      DOUBLE PRECISION   AII
      LOGICAL            LSAME
      EXTERNAL           LSAME
      EXTERNAL           DLARF, XERBLA
      INTRINSIC          MAX
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
      IF( LEFT ) THEN
         NQ = M
      ELSE
         NQ = N
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORM2R', -INFO )
         RETURN
      END IF
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 )
     $   RETURN
      IF( ( LEFT .AND. .NOT.NOTRAN ) .OR. ( .NOT.LEFT .AND. NOTRAN ) )
     $     THEN
         I1 = 1
         I2 = K
         I3 = 1
      ELSE
         I1 = K
         I2 = 1
         I3 = -1
      END IF
      IF( LEFT ) THEN
         NI = N
         JC = 1
      ELSE
         MI = M
         IC = 1
      END IF
      DO 10 I = I1, I2, I3
         IF( LEFT ) THEN
            MI = M - I + 1
            IC = I
         ELSE
            NI = N - I + 1
            JC = I
         END IF
         AII = A( I, I )
         A( I, I ) = ONE
         CALL DLARF( SIDE, MI, NI, A( I, I ), 1, TAU( I ), C( IC, JC ),
     $               LDC, WORK )
         A( I, I ) = AII
   10 CONTINUE
      RETURN
      END
      SUBROUTINE DLAEV2( A, B, C, RT1, RT2, CS1, SN1 )
      DOUBLE PRECISION   A, B, C, CS1, RT1, RT2, SN1
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   TWO
      PARAMETER          ( TWO = 2.0D0 )
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   HALF
      PARAMETER          ( HALF = 0.5D0 )
      INTEGER            SGN1, SGN2
      DOUBLE PRECISION   AB, ACMN, ACMX, ACS, ADF, CS, CT, DF, RT, SM,
     $                   TB, TN
      INTRINSIC          ABS, SQRT
      SM = A + C
      DF = A - C
      ADF = ABS( DF )
      TB = B + B
      AB = ABS( TB )
      IF( ABS( A ).GT.ABS( C ) ) THEN
         ACMX = A
         ACMN = C
      ELSE
         ACMX = C
         ACMN = A
      END IF
      IF( ADF.GT.AB ) THEN
         RT = ADF*SQRT( ONE+( AB / ADF )**2 )
      ELSE IF( ADF.LT.AB ) THEN
         RT = AB*SQRT( ONE+( ADF / AB )**2 )
      ELSE
         RT = AB*SQRT( TWO )
      END IF
      IF( SM.LT.ZERO ) THEN
         RT1 = HALF*( SM-RT )
         SGN1 = -1
         RT2 = ( ACMX / RT1 )*ACMN - ( B / RT1 )*B
      ELSE IF( SM.GT.ZERO ) THEN
         RT1 = HALF*( SM+RT )
         SGN1 = 1
         RT2 = ( ACMX / RT1 )*ACMN - ( B / RT1 )*B
      ELSE
         RT1 = HALF*RT
         RT2 = -HALF*RT
         SGN1 = 1
      END IF
      IF( DF.GE.ZERO ) THEN
         CS = DF + RT
         SGN2 = 1
      ELSE
         CS = DF - RT
         SGN2 = -1
      END IF
      ACS = ABS( CS )
      IF( ACS.GT.AB ) THEN
         CT = -TB / CS
         SN1 = ONE / SQRT( ONE+CT*CT )
         CS1 = CT*SN1
      ELSE
         IF( AB.EQ.ZERO ) THEN
            CS1 = ONE
            SN1 = ZERO
         ELSE
            TN = -CS / TB
            CS1 = ONE / SQRT( ONE+TN*TN )
            SN1 = TN*CS1
         END IF
      END IF
      IF( SGN1.EQ.SGN2 ) THEN
         TN = CS1
         CS1 = -SN1
         SN1 = TN
      END IF
      RETURN
      END
      SUBROUTINE DLASR( SIDE, PIVOT, DIRECT, M, N, C, S, A, LDA )
      CHARACTER          DIRECT, PIVOT, SIDE
      INTEGER            LDA, M, N
      DOUBLE PRECISION   A( LDA, * ), C( * ), S( * )
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      INTEGER            I, INFO, J
      DOUBLE PRECISION   CTEMP, STEMP, TEMP
      LOGICAL            LSAME
      EXTERNAL           LSAME
      EXTERNAL           XERBLA
      INTRINSIC          MAX
      INFO = 0
      IF( .NOT.( LSAME( SIDE, 'L' ) .OR. LSAME( SIDE, 'R' ) ) ) THEN
         INFO = 1
      ELSE IF( .NOT.( LSAME( PIVOT, 'V' ) .OR. LSAME( PIVOT,
     $         'T' ) .OR. LSAME( PIVOT, 'B' ) ) ) THEN
         INFO = 2
      ELSE IF( .NOT.( LSAME( DIRECT, 'F' ) .OR. LSAME( DIRECT, 'B' ) ) )
     $          THEN
         INFO = 3
      ELSE IF( M.LT.0 ) THEN
         INFO = 4
      ELSE IF( N.LT.0 ) THEN
         INFO = 5
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASR ', INFO )
         RETURN
      END IF
      IF( ( M.EQ.0 ) .OR. ( N.EQ.0 ) )
     $   RETURN
      IF( LSAME( SIDE, 'L' ) ) THEN
         IF( LSAME( PIVOT, 'V' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 20 J = 1, M - 1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 10 I = 1, N
                        TEMP = A( J+1, I )
                        A( J+1, I ) = CTEMP*TEMP - STEMP*A( J, I )
                        A( J, I ) = STEMP*TEMP + CTEMP*A( J, I )
   10                CONTINUE
                  END IF
   20          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 40 J = M - 1, 1, -1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 30 I = 1, N
                        TEMP = A( J+1, I )
                        A( J+1, I ) = CTEMP*TEMP - STEMP*A( J, I )
                        A( J, I ) = STEMP*TEMP + CTEMP*A( J, I )
   30                CONTINUE
                  END IF
   40          CONTINUE
            END IF
         ELSE IF( LSAME( PIVOT, 'T' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 60 J = 2, M
                  CTEMP = C( J-1 )
                  STEMP = S( J-1 )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 50 I = 1, N
                        TEMP = A( J, I )
                        A( J, I ) = CTEMP*TEMP - STEMP*A( 1, I )
                        A( 1, I ) = STEMP*TEMP + CTEMP*A( 1, I )
   50                CONTINUE
                  END IF
   60          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 80 J = M, 2, -1
                  CTEMP = C( J-1 )
                  STEMP = S( J-1 )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 70 I = 1, N
                        TEMP = A( J, I )
                        A( J, I ) = CTEMP*TEMP - STEMP*A( 1, I )
                        A( 1, I ) = STEMP*TEMP + CTEMP*A( 1, I )
   70                CONTINUE
                  END IF
   80          CONTINUE
            END IF
         ELSE IF( LSAME( PIVOT, 'B' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 100 J = 1, M - 1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 90 I = 1, N
                        TEMP = A( J, I )
                        A( J, I ) = STEMP*A( M, I ) + CTEMP*TEMP
                        A( M, I ) = CTEMP*A( M, I ) - STEMP*TEMP
   90                CONTINUE
                  END IF
  100          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 120 J = M - 1, 1, -1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 110 I = 1, N
                        TEMP = A( J, I )
                        A( J, I ) = STEMP*A( M, I ) + CTEMP*TEMP
                        A( M, I ) = CTEMP*A( M, I ) - STEMP*TEMP
  110                CONTINUE
                  END IF
  120          CONTINUE
            END IF
         END IF
      ELSE IF( LSAME( SIDE, 'R' ) ) THEN
         IF( LSAME( PIVOT, 'V' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 140 J = 1, N - 1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 130 I = 1, M
                        TEMP = A( I, J+1 )
                        A( I, J+1 ) = CTEMP*TEMP - STEMP*A( I, J )
                        A( I, J ) = STEMP*TEMP + CTEMP*A( I, J )
  130                CONTINUE
                  END IF
  140          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 160 J = N - 1, 1, -1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 150 I = 1, M
                        TEMP = A( I, J+1 )
                        A( I, J+1 ) = CTEMP*TEMP - STEMP*A( I, J )
                        A( I, J ) = STEMP*TEMP + CTEMP*A( I, J )
  150                CONTINUE
                  END IF
  160          CONTINUE
            END IF
         ELSE IF( LSAME( PIVOT, 'T' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 180 J = 2, N
                  CTEMP = C( J-1 )
                  STEMP = S( J-1 )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 170 I = 1, M
                        TEMP = A( I, J )
                        A( I, J ) = CTEMP*TEMP - STEMP*A( I, 1 )
                        A( I, 1 ) = STEMP*TEMP + CTEMP*A( I, 1 )
  170                CONTINUE
                  END IF
  180          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 200 J = N, 2, -1
                  CTEMP = C( J-1 )
                  STEMP = S( J-1 )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 190 I = 1, M
                        TEMP = A( I, J )
                        A( I, J ) = CTEMP*TEMP - STEMP*A( I, 1 )
                        A( I, 1 ) = STEMP*TEMP + CTEMP*A( I, 1 )
  190                CONTINUE
                  END IF
  200          CONTINUE
            END IF
         ELSE IF( LSAME( PIVOT, 'B' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 220 J = 1, N - 1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 210 I = 1, M
                        TEMP = A( I, J )
                        A( I, J ) = STEMP*A( I, N ) + CTEMP*TEMP
                        A( I, N ) = CTEMP*A( I, N ) - STEMP*TEMP
  210                CONTINUE
                  END IF
  220          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 240 J = N - 1, 1, -1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 230 I = 1, M
                        TEMP = A( I, J )
                        A( I, J ) = STEMP*A( I, N ) + CTEMP*TEMP
                        A( I, N ) = CTEMP*A( I, N ) - STEMP*TEMP
  230                CONTINUE
                  END IF
  240          CONTINUE
            END IF
         END IF
      END IF
      RETURN
      END
      SUBROUTINE DLARTG( F, G, CS, SN, R )
      DOUBLE PRECISION   CS, F, G, R, SN
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   TWO
      PARAMETER          ( TWO = 2.0D0 )
      INTEGER            COUNT, I
      DOUBLE PRECISION   EPS, F1, G1, SAFMIN, SAFMN2, SAFMX2, SCALE
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
      INTRINSIC          ABS, INT, LOG, MAX, SQRT
         SAFMIN = DLAMCH( 'S' )
         EPS = DLAMCH( 'E' )
         SAFMN2 = DLAMCH( 'B' )**INT( LOG( SAFMIN / EPS ) /
     $            LOG( DLAMCH( 'B' ) ) / TWO )
         SAFMX2 = ONE / SAFMN2
      IF( G.EQ.ZERO ) THEN
         CS = ONE
         SN = ZERO
         R = F
      ELSE IF( F.EQ.ZERO ) THEN
         CS = ZERO
         SN = ONE
         R = G
      ELSE
         F1 = F
         G1 = G
         SCALE = MAX( ABS( F1 ), ABS( G1 ) )
         IF( SCALE.GE.SAFMX2 ) THEN
            COUNT = 0
   10       CONTINUE
            COUNT = COUNT + 1
            F1 = F1*SAFMN2
            G1 = G1*SAFMN2
            SCALE = MAX( ABS( F1 ), ABS( G1 ) )
            IF( SCALE.GE.SAFMX2 .AND. COUNT .LT. 20)
     $         GO TO 10
            R = SQRT( F1**2+G1**2 )
            CS = F1 / R
            SN = G1 / R
            DO 20 I = 1, COUNT
               R = R*SAFMX2
   20       CONTINUE
         ELSE IF( SCALE.LE.SAFMN2 ) THEN
            COUNT = 0
   30       CONTINUE
            COUNT = COUNT + 1
            F1 = F1*SAFMX2
            G1 = G1*SAFMX2
            SCALE = MAX( ABS( F1 ), ABS( G1 ) )
            IF( SCALE.LE.SAFMN2 )
     $         GO TO 30
            R = SQRT( F1**2+G1**2 )
            CS = F1 / R
            SN = G1 / R
            DO 40 I = 1, COUNT
               R = R*SAFMN2
   40       CONTINUE
         ELSE
            R = SQRT( F1**2+G1**2 )
            CS = F1 / R
            SN = G1 / R
         END IF
         IF( ABS( F ).GT.ABS( G ) .AND. CS.LT.ZERO ) THEN
            CS = -CS
            SN = -SN
            R = -R
         END IF
      END IF
      RETURN
      END
      SUBROUTINE DLAED1( N, D, Q, LDQ, INDXQ, RHO, CUTPNT, WORK, IWORK,
     $                   INFO )
      INTEGER            CUTPNT, INFO, LDQ, N
      DOUBLE PRECISION   RHO
      INTEGER            INDXQ( * ), IWORK( * )
      DOUBLE PRECISION   D( * ), Q( LDQ, * ), WORK( * )
      INTEGER            COLTYP, I, IDLMDA, INDX, INDXC, INDXP, IQ2, IS,
     $                   IW, IZ, K, N1, N2, ZPP1
      EXTERNAL           DCOPY, DLAED2, DLAED3, DLAMRG, XERBLA
      INTRINSIC          MAX, MIN
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( LDQ.LT.MAX( 1, N ) ) THEN
         INFO = -4
      ELSE IF( MIN( 1, N / 2 ).GT.CUTPNT .OR. ( N / 2 ).LT.CUTPNT ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLAED1', -INFO )
         RETURN
      END IF
      IF( N.EQ.0 )
     $   RETURN
      IZ = 1
      IDLMDA = IZ + N
      IW = IDLMDA + N
      IQ2 = IW + N
      INDX = 1
      INDXC = INDX + N
      COLTYP = INDXC + N
      INDXP = COLTYP + N
      CALL DCOPY( CUTPNT, Q( CUTPNT, 1 ), LDQ, WORK( IZ ), 1 )
      ZPP1 = CUTPNT + 1
      CALL DCOPY( N-CUTPNT, Q( ZPP1, ZPP1 ), LDQ, WORK( IZ+CUTPNT ), 1 )
      CALL DLAED2( K, N, CUTPNT, D, Q, LDQ, INDXQ, RHO, WORK( IZ ),
     $             WORK( IDLMDA ), WORK( IW ), WORK( IQ2 ),
     $             IWORK( INDX ), IWORK( INDXC ), IWORK( INDXP ),
     $             IWORK( COLTYP ), INFO )
      IF( INFO.NE.0 )
     $   GO TO 20
      IF( K.NE.0 ) THEN
         IS = ( IWORK( COLTYP )+IWORK( COLTYP+1 ) )*CUTPNT +
     $        ( IWORK( COLTYP+1 )+IWORK( COLTYP+2 ) )*( N-CUTPNT ) + IQ2
         CALL DLAED3( K, N, CUTPNT, D, Q, LDQ, RHO, WORK( IDLMDA ),
     $                WORK( IQ2 ), IWORK( INDXC ), IWORK( COLTYP ),
     $                WORK( IW ), WORK( IS ), INFO )
         IF( INFO.NE.0 )
     $      GO TO 20
         N1 = K
         N2 = N - K
         CALL DLAMRG( N1, N2, D, 1, -1, INDXQ )
      ELSE
         DO 10 I = 1, N
            INDXQ( I ) = I
   10    CONTINUE
      END IF
   20 CONTINUE
      RETURN
      END
      SUBROUTINE DLAED7( ICOMPQ, N, QSIZ, TLVLS, CURLVL, CURPBM, D, Q,
     $                   LDQ, INDXQ, RHO, CUTPNT, QSTORE, QPTR, PRMPTR,
     $                   PERM, GIVPTR, GIVCOL, GIVNUM, WORK, IWORK,
     $                   INFO )
      INTEGER            CURLVL, CURPBM, CUTPNT, ICOMPQ, INFO, LDQ, N,
     $                   QSIZ, TLVLS
      DOUBLE PRECISION   RHO
      INTEGER            GIVCOL( 2, * ), GIVPTR( * ), INDXQ( * ),
     $                   IWORK( * ), PERM( * ), PRMPTR( * ), QPTR( * )
      DOUBLE PRECISION   D( * ), GIVNUM( 2, * ), Q( LDQ, * ),
     $                   QSTORE( * ), WORK( * )
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D0, ZERO = 0.0D0 )
      INTEGER            COLTYP, CURR, I, IDLMDA, INDX, INDXC, INDXP,
     $                   IQ2, IS, IW, IZ, K, LDQ2, N1, N2, PTR
      EXTERNAL           DGEMM, DLAED8, DLAED9, DLAEDA, DLAMRG, XERBLA
      INTRINSIC          MAX, MIN
      INFO = 0
      IF( ICOMPQ.LT.0 .OR. ICOMPQ.GT.1 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( ICOMPQ.EQ.1 .AND. QSIZ.LT.N ) THEN
         INFO = -3
      ELSE IF( LDQ.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF( MIN( 1, N ).GT.CUTPNT .OR. N.LT.CUTPNT ) THEN
         INFO = -12
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLAED7', -INFO )
         RETURN
      END IF
      IF( N.EQ.0 )
     $   RETURN
      IF( ICOMPQ.EQ.1 ) THEN
         LDQ2 = QSIZ
      ELSE
         LDQ2 = N
      END IF
      IZ = 1
      IDLMDA = IZ + N
      IW = IDLMDA + N
      IQ2 = IW + N
      IS = IQ2 + N*LDQ2
      INDX = 1
      INDXC = INDX + N
      COLTYP = INDXC + N
      INDXP = COLTYP + N
      PTR = 1 + 2**TLVLS
      DO 10 I = 1, CURLVL - 1
         PTR = PTR + 2**( TLVLS-I )
   10 CONTINUE
      CURR = PTR + CURPBM
      CALL DLAEDA( N, TLVLS, CURLVL, CURPBM, PRMPTR, PERM, GIVPTR,
     $             GIVCOL, GIVNUM, QSTORE, QPTR, WORK( IZ ),
     $             WORK( IZ+N ), INFO )
      IF( CURLVL.EQ.TLVLS ) THEN
         QPTR( CURR ) = 1
         PRMPTR( CURR ) = 1
         GIVPTR( CURR ) = 1
      END IF
      CALL DLAED8( ICOMPQ, K, N, QSIZ, D, Q, LDQ, INDXQ, RHO, CUTPNT,
     $             WORK( IZ ), WORK( IDLMDA ), WORK( IQ2 ), LDQ2,
     $             WORK( IW ), PERM( PRMPTR( CURR ) ), GIVPTR( CURR+1 ),
     $             GIVCOL( 1, GIVPTR( CURR ) ),
     $             GIVNUM( 1, GIVPTR( CURR ) ), IWORK( INDXP ),
     $             IWORK( INDX ), INFO )
      PRMPTR( CURR+1 ) = PRMPTR( CURR ) + N
      GIVPTR( CURR+1 ) = GIVPTR( CURR+1 ) + GIVPTR( CURR )
      IF( K.NE.0 ) THEN
         CALL DLAED9( K, 1, K, N, D, WORK( IS ), K, RHO, WORK( IDLMDA ),
     $                WORK( IW ), QSTORE( QPTR( CURR ) ), K, INFO )
         IF( INFO.NE.0 )
     $      GO TO 30
         IF( ICOMPQ.EQ.1 ) THEN
            CALL DGEMM( 'N', 'N', QSIZ, K, K, ONE, WORK( IQ2 ), LDQ2,
     $                  QSTORE( QPTR( CURR ) ), K, ZERO, Q, LDQ )
         END IF
         QPTR( CURR+1 ) = QPTR( CURR ) + K**2
         N1 = K
         N2 = N - K
         CALL DLAMRG( N1, N2, D, 1, -1, INDXQ )
      ELSE
         QPTR( CURR+1 ) = QPTR( CURR )
         DO 20 I = 1, N
            INDXQ( I ) = I
   20    CONTINUE
      END IF
   30 CONTINUE
      RETURN
      END
      SUBROUTINE DLARF( SIDE, M, N, V, INCV, TAU, C, LDC, WORK )
      CHARACTER          SIDE
      INTEGER            INCV, LDC, M, N
      DOUBLE PRECISION   TAU
      DOUBLE PRECISION   C( LDC, * ), V( * ), WORK( * )
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      LOGICAL            APPLYLEFT
      INTEGER            I, LASTV, LASTC
      EXTERNAL           DGEMV, DGER
      LOGICAL            LSAME
      INTEGER            ILADLR, ILADLC
      EXTERNAL           LSAME, ILADLR, ILADLC
      APPLYLEFT = LSAME( SIDE, 'L' )
      LASTV = 0
      LASTC = 0
      IF( TAU.NE.ZERO ) THEN
!     Set up variables for scanning V.  LASTV begins pointing to the end
!     of V.
         IF( APPLYLEFT ) THEN
            LASTV = M
         ELSE
            LASTV = N
         END IF
         IF( INCV.GT.0 ) THEN
            I = 1 + (LASTV-1) * INCV
         ELSE
            I = 1
         END IF
!     Look for the last non-zero row in V.
         DO WHILE( LASTV.GT.0 .AND. V( I ).EQ.ZERO )
            LASTV = LASTV - 1
            I = I - INCV
         END DO
         IF( APPLYLEFT ) THEN
!     Scan for the last non-zero column in C(1:lastv,:).
            LASTC = ILADLC(LASTV, N, C, LDC)
         ELSE
!     Scan for the last non-zero row in C(:,1:lastv).
            LASTC = ILADLR(M, LASTV, C, LDC)
         END IF
      END IF
!     Note that lastc.eq.0 renders the BLAS operations null; no special
!     case is needed at this level.
      IF( APPLYLEFT ) THEN
         IF( LASTV.GT.0 ) THEN
            CALL DGEMV( 'Transpose', LASTV, LASTC, ONE, C, LDC, V, INCV,
     $           ZERO, WORK, 1 )
            CALL DGER( LASTV, LASTC, -TAU, V, INCV, WORK, 1, C, LDC )
         END IF
      ELSE
         IF( LASTV.GT.0 ) THEN
            CALL DGEMV( 'No transpose', LASTC, LASTV, ONE, C, LDC,
     $           V, INCV, ZERO, WORK, 1 )
            CALL DGER( LASTC, LASTV, -TAU, WORK, 1, V, INCV, C, LDC )
         END IF
      END IF
      RETURN
      END
      SUBROUTINE DLAED2( K, N, N1, D, Q, LDQ, INDXQ, RHO, Z, DLAMDA, W,
     $                   Q2, INDX, INDXC, INDXP, COLTYP, INFO )
      INTEGER            INFO, K, LDQ, N, N1
      DOUBLE PRECISION   RHO
      INTEGER            COLTYP( * ), INDX( * ), INDXC( * ), INDXP( * ),
     $                   INDXQ( * )
      DOUBLE PRECISION   D( * ), DLAMDA( * ), Q( LDQ, * ), Q2( * ),
     $                   W( * ), Z( * )
      DOUBLE PRECISION   MONE, ZERO, ONE, TWO, EIGHT
      PARAMETER          ( MONE = -1.0D0, ZERO = 0.0D0, ONE = 1.0D0,
     $                   TWO = 2.0D0, EIGHT = 8.0D0 )
      INTEGER            CTOT( 4 ), PSM( 4 )
      INTEGER            CT, I, IMAX, IQ1, IQ2, J, JMAX, JS, K2, N1P1,
     $                   N2, NJ, PJ
      DOUBLE PRECISION   C, EPS, S, T, TAU, TOL
      INTEGER            IDAMAX
      DOUBLE PRECISION   DLAMCH, DLAPY2
      EXTERNAL           IDAMAX, DLAMCH, DLAPY2
      EXTERNAL           DCOPY, DLACPY, DLAMRG, DROT, DSCAL, XERBLA
      INTRINSIC          ABS, MAX, MIN, SQRT
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDQ.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( MIN( 1, ( N / 2 ) ).GT.N1 .OR. ( N / 2 ).LT.N1 ) THEN
         INFO = -3
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLAED2', -INFO )
         RETURN
      END IF
      IF( N.EQ.0 )
     $   RETURN
      N2 = N - N1
      N1P1 = N1 + 1
      IF( RHO.LT.ZERO ) THEN
         CALL DSCAL( N2, MONE, Z( N1P1 ), 1 )
      END IF
      T = ONE / SQRT( TWO )
      CALL DSCAL( N, T, Z, 1 )
      RHO = ABS( TWO*RHO )
      DO 10 I = N1P1, N
         INDXQ( I ) = INDXQ( I ) + N1
   10 CONTINUE
      DO 20 I = 1, N
         DLAMDA( I ) = D( INDXQ( I ) )
   20 CONTINUE
      CALL DLAMRG( N1, N2, DLAMDA, 1, 1, INDXC )
      DO 30 I = 1, N
         INDX( I ) = INDXQ( INDXC( I ) )
   30 CONTINUE
      IMAX = IDAMAX( N, Z, 1 )
      JMAX = IDAMAX( N, D, 1 )
      EPS = DLAMCH( 'Epsilon' )
      TOL = EIGHT*EPS*MAX( ABS( D( JMAX ) ), ABS( Z( IMAX ) ) )
      IF( RHO*ABS( Z( IMAX ) ).LE.TOL ) THEN
         K = 0
         IQ2 = 1
         DO 40 J = 1, N
            I = INDX( J )
            CALL DCOPY( N, Q( 1, I ), 1, Q2( IQ2 ), 1 )
            DLAMDA( J ) = D( I )
            IQ2 = IQ2 + N
   40    CONTINUE
         CALL DLACPY( 'A', N, N, Q2, N, Q, LDQ )
         CALL DCOPY( N, DLAMDA, 1, D, 1 )
         GO TO 190
      END IF
      DO 50 I = 1, N1
         COLTYP( I ) = 1
   50 CONTINUE
      DO 60 I = N1P1, N
         COLTYP( I ) = 3
   60 CONTINUE
      K = 0
      K2 = N + 1
      DO 70 J = 1, N
         NJ = INDX( J )
         IF( RHO*ABS( Z( NJ ) ).LE.TOL ) THEN
            K2 = K2 - 1
            COLTYP( NJ ) = 4
            INDXP( K2 ) = NJ
            IF( J.EQ.N )
     $         GO TO 100
         ELSE
            PJ = NJ
            GO TO 80
         END IF
   70 CONTINUE
   80 CONTINUE
      J = J + 1
      NJ = INDX( J )
      IF( J.GT.N )
     $   GO TO 100
      IF( RHO*ABS( Z( NJ ) ).LE.TOL ) THEN
         K2 = K2 - 1
         COLTYP( NJ ) = 4
         INDXP( K2 ) = NJ
      ELSE
         S = Z( PJ )
         C = Z( NJ )
         TAU = DLAPY2( C, S )
         T = D( NJ ) - D( PJ )
         C = C / TAU
         S = -S / TAU
         IF( ABS( T*C*S ).LE.TOL ) THEN
            Z( NJ ) = TAU
            Z( PJ ) = ZERO
            IF( COLTYP( NJ ).NE.COLTYP( PJ ) )
     $         COLTYP( NJ ) = 2
            COLTYP( PJ ) = 4
            CALL DROT( N, Q( 1, PJ ), 1, Q( 1, NJ ), 1, C, S )
            T = D( PJ )*C**2 + D( NJ )*S**2
            D( NJ ) = D( PJ )*S**2 + D( NJ )*C**2
            D( PJ ) = T
            K2 = K2 - 1
            I = 1
   90       CONTINUE
            IF( K2+I.LE.N ) THEN
               IF( D( PJ ).LT.D( INDXP( K2+I ) ) ) THEN
                  INDXP( K2+I-1 ) = INDXP( K2+I )
                  INDXP( K2+I ) = PJ
                  I = I + 1
                  GO TO 90
               ELSE
                  INDXP( K2+I-1 ) = PJ
               END IF
            ELSE
               INDXP( K2+I-1 ) = PJ
            END IF
            PJ = NJ
         ELSE
            K = K + 1
            DLAMDA( K ) = D( PJ )
            W( K ) = Z( PJ )
            INDXP( K ) = PJ
            PJ = NJ
         END IF
      END IF
      GO TO 80
  100 CONTINUE
      K = K + 1
      DLAMDA( K ) = D( PJ )
      W( K ) = Z( PJ )
      INDXP( K ) = PJ
      DO 110 J = 1, 4
         CTOT( J ) = 0
  110 CONTINUE
      DO 120 J = 1, N
         CT = COLTYP( J )
         CTOT( CT ) = CTOT( CT ) + 1
  120 CONTINUE
      PSM( 1 ) = 1
      PSM( 2 ) = 1 + CTOT( 1 )
      PSM( 3 ) = PSM( 2 ) + CTOT( 2 )
      PSM( 4 ) = PSM( 3 ) + CTOT( 3 )
      K = N - CTOT( 4 )
      DO 130 J = 1, N
         JS = INDXP( J )
         CT = COLTYP( JS )
         INDX( PSM( CT ) ) = JS
         INDXC( PSM( CT ) ) = J
         PSM( CT ) = PSM( CT ) + 1
  130 CONTINUE
      I = 1
      IQ1 = 1
      IQ2 = 1 + ( CTOT( 1 )+CTOT( 2 ) )*N1
      DO 140 J = 1, CTOT( 1 )
         JS = INDX( I )
         CALL DCOPY( N1, Q( 1, JS ), 1, Q2( IQ1 ), 1 )
         Z( I ) = D( JS )
         I = I + 1
         IQ1 = IQ1 + N1
  140 CONTINUE
      DO 150 J = 1, CTOT( 2 )
         JS = INDX( I )
         CALL DCOPY( N1, Q( 1, JS ), 1, Q2( IQ1 ), 1 )
         CALL DCOPY( N2, Q( N1+1, JS ), 1, Q2( IQ2 ), 1 )
         Z( I ) = D( JS )
         I = I + 1
         IQ1 = IQ1 + N1
         IQ2 = IQ2 + N2
  150 CONTINUE
      DO 160 J = 1, CTOT( 3 )
         JS = INDX( I )
         CALL DCOPY( N2, Q( N1+1, JS ), 1, Q2( IQ2 ), 1 )
         Z( I ) = D( JS )
         I = I + 1
         IQ2 = IQ2 + N2
  160 CONTINUE
      IQ1 = IQ2
      DO 170 J = 1, CTOT( 4 )
         JS = INDX( I )
         CALL DCOPY( N, Q( 1, JS ), 1, Q2( IQ2 ), 1 )
         IQ2 = IQ2 + N
         Z( I ) = D( JS )
         I = I + 1
  170 CONTINUE
      IF( K.LT.N ) THEN
         CALL DLACPY( 'A', N, CTOT( 4 ), Q2( IQ1 ), N,
     $                Q( 1, K+1 ), LDQ )
         CALL DCOPY( N-K, Z( K+1 ), 1, D( K+1 ), 1 )
      END IF
      DO 180 J = 1, 4
         COLTYP( J ) = CTOT( J )
  180 CONTINUE
  190 CONTINUE
      RETURN
      END
      SUBROUTINE DLAED3( K, N, N1, D, Q, LDQ, RHO, DLAMDA, Q2, INDX,
     $                   CTOT, W, S, INFO )
      INTEGER            INFO, K, LDQ, N, N1
      DOUBLE PRECISION   RHO
      INTEGER            CTOT( * ), INDX( * )
      DOUBLE PRECISION   D( * ), DLAMDA( * ), Q( LDQ, * ), Q2( * ),
     $                   S( * ), W( * )
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D0, ZERO = 0.0D0 )
      INTEGER            I, II, IQ2, J, N12, N2, N23
      DOUBLE PRECISION   TEMP
      DOUBLE PRECISION   DLAMC3, DNRM2
      EXTERNAL           DLAMC3, DNRM2
      EXTERNAL           DCOPY, DGEMM, DLACPY, DLAED4, DLASET, XERBLA
      INTRINSIC          MAX, SIGN, SQRT
      INFO = 0
      IF( K.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.K ) THEN
         INFO = -2
      ELSE IF( LDQ.LT.MAX( 1, N ) ) THEN
         INFO = -6
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLAED3', -INFO )
         RETURN
      END IF
      IF( K.EQ.0 )
     $   RETURN
      DO 10 I = 1, K
         DLAMDA( I ) = DLAMC3( DLAMDA( I ), DLAMDA( I ) ) - DLAMDA( I )
   10 CONTINUE
      DO 20 J = 1, K
         CALL DLAED4( K, J, DLAMDA, W, Q( 1, J ), RHO, D( J ), INFO )
         IF( INFO.NE.0 )
     $      GO TO 120
   20 CONTINUE
      IF( K.EQ.1 )
     $   GO TO 110
      IF( K.EQ.2 ) THEN
         DO 30 J = 1, K
            W( 1 ) = Q( 1, J )
            W( 2 ) = Q( 2, J )
            II = INDX( 1 )
            Q( 1, J ) = W( II )
            II = INDX( 2 )
            Q( 2, J ) = W( II )
   30    CONTINUE
         GO TO 110
      END IF
      CALL DCOPY( K, W, 1, S, 1 )
      CALL DCOPY( K, Q, LDQ+1, W, 1 )
      DO 60 J = 1, K
         DO 40 I = 1, J - 1
            W( I ) = W( I )*( Q( I, J ) / ( DLAMDA( I )-DLAMDA( J ) ) )
   40    CONTINUE
         DO 50 I = J + 1, K
            W( I ) = W( I )*( Q( I, J ) / ( DLAMDA( I )-DLAMDA( J ) ) )
   50    CONTINUE
   60 CONTINUE
      DO 70 I = 1, K
         W( I ) = SIGN( SQRT( -W( I ) ), S( I ) )
   70 CONTINUE
      DO 100 J = 1, K
         DO 80 I = 1, K
            S( I ) = W( I ) / Q( I, J )
   80    CONTINUE
         TEMP = DNRM2( K, S, 1 )
         DO 90 I = 1, K
            II = INDX( I )
            Q( I, J ) = S( II ) / TEMP
   90    CONTINUE
  100 CONTINUE
  110 CONTINUE
      N2 = N - N1
      N12 = CTOT( 1 ) + CTOT( 2 )
      N23 = CTOT( 2 ) + CTOT( 3 )
      CALL DLACPY( 'A', N23, K, Q( CTOT( 1 )+1, 1 ), LDQ, S, N23 )
      IQ2 = N1*N12 + 1
      IF( N23.NE.0 ) THEN
         CALL DGEMM( 'N', 'N', N2, K, N23, ONE, Q2( IQ2 ), N2, S, N23,
     $               ZERO, Q( N1+1, 1 ), LDQ )
      ELSE
         CALL DLASET( 'A', N2, K, ZERO, ZERO, Q( N1+1, 1 ), LDQ )
      END IF
      CALL DLACPY( 'A', N12, K, Q, LDQ, S, N12 )
      IF( N12.NE.0 ) THEN
         CALL DGEMM( 'N', 'N', N1, K, N12, ONE, Q2, N1, S, N12, ZERO, Q,
     $               LDQ )
      ELSE
         CALL DLASET( 'A', N1, K, ZERO, ZERO, Q( 1, 1 ), LDQ )
      END IF
  120 CONTINUE
      RETURN
      END
      SUBROUTINE DLAMRG( N1, N2, A, DTRD1, DTRD2, INDEX )
      INTEGER            DTRD1, DTRD2, N1, N2
      INTEGER            INDEX( * )
      DOUBLE PRECISION   A( * )
      INTEGER            I, IND1, IND2, N1SV, N2SV
      N1SV = N1
      N2SV = N2
      IF( DTRD1.GT.0 ) THEN
         IND1 = 1
      ELSE
         IND1 = N1
      END IF
      IF( DTRD2.GT.0 ) THEN
         IND2 = 1 + N1
      ELSE
         IND2 = N1 + N2
      END IF
      I = 1
   10 CONTINUE
      IF( N1SV.GT.0 .AND. N2SV.GT.0 ) THEN
         IF( A( IND1 ).LE.A( IND2 ) ) THEN
            INDEX( I ) = IND1
            I = I + 1
            IND1 = IND1 + DTRD1
            N1SV = N1SV - 1
         ELSE
            INDEX( I ) = IND2
            I = I + 1
            IND2 = IND2 + DTRD2
            N2SV = N2SV - 1
         END IF
         GO TO 10
      END IF
      IF( N1SV.EQ.0 ) THEN
         DO 20 N1SV = 1, N2SV
            INDEX( I ) = IND2
            I = I + 1
            IND2 = IND2 + DTRD2
   20    CONTINUE
      ELSE
         DO 30 N2SV = 1, N1SV
            INDEX( I ) = IND1
            I = I + 1
            IND1 = IND1 + DTRD1
   30    CONTINUE
      END IF
      RETURN
      END
      SUBROUTINE DTRMV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
      INTEGER INCX,LDA,N
      CHARACTER DIAG,TRANS,UPLO
      DOUBLE PRECISION A(LDA,*),X(*)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D+0)
      DOUBLE PRECISION TEMP
      INTEGER I,INFO,IX,J,JX,KX
      LOGICAL NOUNIT
      LOGICAL LSAME
      EXTERNAL LSAME
      EXTERNAL XERBLA
      INTRINSIC MAX
      INFO = 0
      IF (.NOT.LSAME(UPLO,'U') .AND. .NOT.LSAME(UPLO,'L')) THEN
          INFO = 1
      ELSE IF (.NOT.LSAME(TRANS,'N') .AND. .NOT.LSAME(TRANS,'T') .AND.
     +         .NOT.LSAME(TRANS,'C')) THEN
          INFO = 2
      ELSE IF (.NOT.LSAME(DIAG,'U') .AND. .NOT.LSAME(DIAG,'N')) THEN
          INFO = 3
      ELSE IF (N.LT.0) THEN
          INFO = 4
      ELSE IF (LDA.LT.MAX(1,N)) THEN
          INFO = 6
      ELSE IF (INCX.EQ.0) THEN
          INFO = 8
      END IF
      IF (INFO.NE.0) THEN
          CALL XERBLA('DTRMV ',INFO)
          RETURN
      END IF
      IF (N.EQ.0) RETURN
      NOUNIT = LSAME(DIAG,'N')
      IF (INCX.LE.0) THEN
          KX = 1 - (N-1)*INCX
      ELSE IF (INCX.NE.1) THEN
          KX = 1
      END IF
      IF (LSAME(TRANS,'N')) THEN
          IF (LSAME(UPLO,'U')) THEN
              IF (INCX.EQ.1) THEN
                  DO 20 J = 1,N
                      IF (X(J).NE.ZERO) THEN
                          TEMP = X(J)
                          DO 10 I = 1,J - 1
                              X(I) = X(I) + TEMP*A(I,J)
   10                     CONTINUE
                          IF (NOUNIT) X(J) = X(J)*A(J,J)
                      END IF
   20             CONTINUE
              ELSE
                  JX = KX
                  DO 40 J = 1,N
                      IF (X(JX).NE.ZERO) THEN
                          TEMP = X(JX)
                          IX = KX
                          DO 30 I = 1,J - 1
                              X(IX) = X(IX) + TEMP*A(I,J)
                              IX = IX + INCX
   30                     CONTINUE
                          IF (NOUNIT) X(JX) = X(JX)*A(J,J)
                      END IF
                      JX = JX + INCX
   40             CONTINUE
              END IF
          ELSE
              IF (INCX.EQ.1) THEN
                  DO 60 J = N,1,-1
                      IF (X(J).NE.ZERO) THEN
                          TEMP = X(J)
                          DO 50 I = N,J + 1,-1
                              X(I) = X(I) + TEMP*A(I,J)
   50                     CONTINUE
                          IF (NOUNIT) X(J) = X(J)*A(J,J)
                      END IF
   60             CONTINUE
              ELSE
                  KX = KX + (N-1)*INCX
                  JX = KX
                  DO 80 J = N,1,-1
                      IF (X(JX).NE.ZERO) THEN
                          TEMP = X(JX)
                          IX = KX
                          DO 70 I = N,J + 1,-1
                              X(IX) = X(IX) + TEMP*A(I,J)
                              IX = IX - INCX
   70                     CONTINUE
                          IF (NOUNIT) X(JX) = X(JX)*A(J,J)
                      END IF
                      JX = JX - INCX
   80             CONTINUE
              END IF
          END IF
      ELSE
          IF (LSAME(UPLO,'U')) THEN
              IF (INCX.EQ.1) THEN
                  DO 100 J = N,1,-1
                      TEMP = X(J)
                      IF (NOUNIT) TEMP = TEMP*A(J,J)
                      DO 90 I = J - 1,1,-1
                          TEMP = TEMP + A(I,J)*X(I)
   90                 CONTINUE
                      X(J) = TEMP
  100             CONTINUE
              ELSE
                  JX = KX + (N-1)*INCX
                  DO 120 J = N,1,-1
                      TEMP = X(JX)
                      IX = JX
                      IF (NOUNIT) TEMP = TEMP*A(J,J)
                      DO 110 I = J - 1,1,-1
                          IX = IX - INCX
                          TEMP = TEMP + A(I,J)*X(IX)
  110                 CONTINUE
                      X(JX) = TEMP
                      JX = JX - INCX
  120             CONTINUE
              END IF
          ELSE
              IF (INCX.EQ.1) THEN
                  DO 140 J = 1,N
                      TEMP = X(J)
                      IF (NOUNIT) TEMP = TEMP*A(J,J)
                      DO 130 I = J + 1,N
                          TEMP = TEMP + A(I,J)*X(I)
  130                 CONTINUE
                      X(J) = TEMP
  140             CONTINUE
              ELSE
                  JX = KX
                  DO 160 J = 1,N
                      TEMP = X(JX)
                      IX = JX
                      IF (NOUNIT) TEMP = TEMP*A(J,J)
                      DO 150 I = J + 1,N
                          IX = IX + INCX
                          TEMP = TEMP + A(I,J)*X(IX)
  150                 CONTINUE
                      X(JX) = TEMP
                      JX = JX + INCX
  160             CONTINUE
              END IF
          END IF
      END IF
      RETURN
      END
      SUBROUTINE DTRMM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
      DOUBLE PRECISION ALPHA
      INTEGER LDA,LDB,M,N
      CHARACTER DIAG,SIDE,TRANSA,UPLO
      DOUBLE PRECISION A(LDA,*),B(LDB,*)
      LOGICAL LSAME
      EXTERNAL LSAME
      EXTERNAL XERBLA
      INTRINSIC MAX
      DOUBLE PRECISION TEMP
      INTEGER I,INFO,J,K,NROWA
      LOGICAL LSIDE,NOUNIT,UPPER
      DOUBLE PRECISION ONE,ZERO
      PARAMETER (ONE=1.0D+0,ZERO=0.0D+0)
      LSIDE = LSAME(SIDE,'L')
      IF (LSIDE) THEN
          NROWA = M
      ELSE
          NROWA = N
      END IF
      NOUNIT = LSAME(DIAG,'N')
      UPPER = LSAME(UPLO,'U')
      INFO = 0
      IF ((.NOT.LSIDE) .AND. (.NOT.LSAME(SIDE,'R'))) THEN
          INFO = 1
      ELSE IF ((.NOT.UPPER) .AND. (.NOT.LSAME(UPLO,'L'))) THEN
          INFO = 2
      ELSE IF ((.NOT.LSAME(TRANSA,'N')) .AND.
     +         (.NOT.LSAME(TRANSA,'T')) .AND.
     +         (.NOT.LSAME(TRANSA,'C'))) THEN
          INFO = 3
      ELSE IF ((.NOT.LSAME(DIAG,'U')) .AND. (.NOT.LSAME(DIAG,'N'))) THEN
          INFO = 4
      ELSE IF (M.LT.0) THEN
          INFO = 5
      ELSE IF (N.LT.0) THEN
          INFO = 6
      ELSE IF (LDA.LT.MAX(1,NROWA)) THEN
          INFO = 9
      ELSE IF (LDB.LT.MAX(1,M)) THEN
          INFO = 11
      END IF
      IF (INFO.NE.0) THEN
          CALL XERBLA('DTRMM ',INFO)
          RETURN
      END IF
      IF (M.EQ.0 .OR. N.EQ.0) RETURN
      IF (ALPHA.EQ.ZERO) THEN
          DO 20 J = 1,N
              DO 10 I = 1,M
                  B(I,J) = ZERO
   10         CONTINUE
   20     CONTINUE
          RETURN
      END IF
      IF (LSIDE) THEN
          IF (LSAME(TRANSA,'N')) THEN
              IF (UPPER) THEN
                  DO 50 J = 1,N
                      DO 40 K = 1,M
                          IF (B(K,J).NE.ZERO) THEN
                              TEMP = ALPHA*B(K,J)
                              DO 30 I = 1,K - 1
                                  B(I,J) = B(I,J) + TEMP*A(I,K)
   30                         CONTINUE
                              IF (NOUNIT) TEMP = TEMP*A(K,K)
                              B(K,J) = TEMP
                          END IF
   40                 CONTINUE
   50             CONTINUE
              ELSE
                  DO 80 J = 1,N
                      DO 70 K = M,1,-1
                          IF (B(K,J).NE.ZERO) THEN
                              TEMP = ALPHA*B(K,J)
                              B(K,J) = TEMP
                              IF (NOUNIT) B(K,J) = B(K,J)*A(K,K)
                              DO 60 I = K + 1,M
                                  B(I,J) = B(I,J) + TEMP*A(I,K)
   60                         CONTINUE
                          END IF
   70                 CONTINUE
   80             CONTINUE
              END IF
          ELSE
              IF (UPPER) THEN
                  DO 110 J = 1,N
                      DO 100 I = M,1,-1
                          TEMP = B(I,J)
                          IF (NOUNIT) TEMP = TEMP*A(I,I)
                          DO 90 K = 1,I - 1
                              TEMP = TEMP + A(K,I)*B(K,J)
   90                     CONTINUE
                          B(I,J) = ALPHA*TEMP
  100                 CONTINUE
  110             CONTINUE
              ELSE
                  DO 140 J = 1,N
                      DO 130 I = 1,M
                          TEMP = B(I,J)
                          IF (NOUNIT) TEMP = TEMP*A(I,I)
                          DO 120 K = I + 1,M
                              TEMP = TEMP + A(K,I)*B(K,J)
  120                     CONTINUE
                          B(I,J) = ALPHA*TEMP
  130                 CONTINUE
  140             CONTINUE
              END IF
          END IF
      ELSE
          IF (LSAME(TRANSA,'N')) THEN
              IF (UPPER) THEN
                  DO 180 J = N,1,-1
                      TEMP = ALPHA
                      IF (NOUNIT) TEMP = TEMP*A(J,J)
                      DO 150 I = 1,M
                          B(I,J) = TEMP*B(I,J)
  150                 CONTINUE
                      DO 170 K = 1,J - 1
                          IF (A(K,J).NE.ZERO) THEN
                              TEMP = ALPHA*A(K,J)
                              DO 160 I = 1,M
                                  B(I,J) = B(I,J) + TEMP*B(I,K)
  160                         CONTINUE
                          END IF
  170                 CONTINUE
  180             CONTINUE
              ELSE
                  DO 220 J = 1,N
                      TEMP = ALPHA
                      IF (NOUNIT) TEMP = TEMP*A(J,J)
                      DO 190 I = 1,M
                          B(I,J) = TEMP*B(I,J)
  190                 CONTINUE
                      DO 210 K = J + 1,N
                          IF (A(K,J).NE.ZERO) THEN
                              TEMP = ALPHA*A(K,J)
                              DO 200 I = 1,M
                                  B(I,J) = B(I,J) + TEMP*B(I,K)
  200                         CONTINUE
                          END IF
  210                 CONTINUE
  220             CONTINUE
              END IF
          ELSE
              IF (UPPER) THEN
                  DO 260 K = 1,N
                      DO 240 J = 1,K - 1
                          IF (A(J,K).NE.ZERO) THEN
                              TEMP = ALPHA*A(J,K)
                              DO 230 I = 1,M
                                  B(I,J) = B(I,J) + TEMP*B(I,K)
  230                         CONTINUE
                          END IF
  240                 CONTINUE
                      TEMP = ALPHA
                      IF (NOUNIT) TEMP = TEMP*A(K,K)
                      IF (TEMP.NE.ONE) THEN
                          DO 250 I = 1,M
                              B(I,K) = TEMP*B(I,K)
  250                     CONTINUE
                      END IF
  260             CONTINUE
              ELSE
                  DO 300 K = N,1,-1
                      DO 280 J = K + 1,N
                          IF (A(J,K).NE.ZERO) THEN
                              TEMP = ALPHA*A(J,K)
                              DO 270 I = 1,M
                                  B(I,J) = B(I,J) + TEMP*B(I,K)
  270                         CONTINUE
                          END IF
  280                 CONTINUE
                      TEMP = ALPHA
                      IF (NOUNIT) TEMP = TEMP*A(K,K)
                      IF (TEMP.NE.ONE) THEN
                          DO 290 I = 1,M
                              B(I,K) = TEMP*B(I,K)
  290                     CONTINUE
                      END IF
  300             CONTINUE
              END IF
          END IF
      END IF
      RETURN
      END
      SUBROUTINE DLAEDA( N, TLVLS, CURLVL, CURPBM, PRMPTR, PERM, GIVPTR,
     $                   GIVCOL, GIVNUM, Q, QPTR, Z, ZTEMP, INFO )
      INTEGER            CURLVL, CURPBM, INFO, N, TLVLS
      INTEGER            GIVCOL( 2, * ), GIVPTR( * ), PERM( * ),
     $                   PRMPTR( * ), QPTR( * )
      DOUBLE PRECISION   GIVNUM( 2, * ), Q( * ), Z( * ), ZTEMP( * )
      DOUBLE PRECISION   ZERO, HALF, ONE
      PARAMETER          ( ZERO = 0.0D0, HALF = 0.5D0, ONE = 1.0D0 )
      INTEGER            BSIZ1, BSIZ2, CURR, I, K, MID, PSIZ1, PSIZ2,
     $                   PTR, ZPTR1
      EXTERNAL           DCOPY, DGEMV, DROT, XERBLA
      INTRINSIC          DBLE, INT, SQRT
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -1
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLAEDA', -INFO )
         RETURN
      END IF
      IF( N.EQ.0 )
     $   RETURN
      MID = N / 2 + 1
      PTR = 1
      CURR = PTR + CURPBM*2**CURLVL + 2**( CURLVL-1 ) - 1
      BSIZ1 = INT( HALF+SQRT( DBLE( QPTR( CURR+1 )-QPTR( CURR ) ) ) )
      BSIZ2 = INT( HALF+SQRT( DBLE( QPTR( CURR+2 )-QPTR( CURR+1 ) ) ) )
      DO 10 K = 1, MID - BSIZ1 - 1
         Z( K ) = ZERO
   10 CONTINUE
      CALL DCOPY( BSIZ1, Q( QPTR( CURR )+BSIZ1-1 ), BSIZ1,
     $            Z( MID-BSIZ1 ), 1 )
      CALL DCOPY( BSIZ2, Q( QPTR( CURR+1 ) ), BSIZ2, Z( MID ), 1 )
      DO 20 K = MID + BSIZ2, N
         Z( K ) = ZERO
   20 CONTINUE
      PTR = 2**TLVLS + 1
      DO 70 K = 1, CURLVL - 1
         CURR = PTR + CURPBM*2**( CURLVL-K ) + 2**( CURLVL-K-1 ) - 1
         PSIZ1 = PRMPTR( CURR+1 ) - PRMPTR( CURR )
         PSIZ2 = PRMPTR( CURR+2 ) - PRMPTR( CURR+1 )
         ZPTR1 = MID - PSIZ1
         DO 30 I = GIVPTR( CURR ), GIVPTR( CURR+1 ) - 1
            CALL DROT( 1, Z( ZPTR1+GIVCOL( 1, I )-1 ), 1,
     $                 Z( ZPTR1+GIVCOL( 2, I )-1 ), 1, GIVNUM( 1, I ),
     $                 GIVNUM( 2, I ) )
   30    CONTINUE
         DO 40 I = GIVPTR( CURR+1 ), GIVPTR( CURR+2 ) - 1
            CALL DROT( 1, Z( MID-1+GIVCOL( 1, I ) ), 1,
     $                 Z( MID-1+GIVCOL( 2, I ) ), 1, GIVNUM( 1, I ),
     $                 GIVNUM( 2, I ) )
   40    CONTINUE
         PSIZ1 = PRMPTR( CURR+1 ) - PRMPTR( CURR )
         PSIZ2 = PRMPTR( CURR+2 ) - PRMPTR( CURR+1 )
         DO 50 I = 0, PSIZ1 - 1
            ZTEMP( I+1 ) = Z( ZPTR1+PERM( PRMPTR( CURR )+I )-1 )
   50    CONTINUE
         DO 60 I = 0, PSIZ2 - 1
            ZTEMP( PSIZ1+I+1 ) = Z( MID+PERM( PRMPTR( CURR+1 )+I )-1 )
   60    CONTINUE
         BSIZ1 = INT( HALF+SQRT( DBLE( QPTR( CURR+1 )-QPTR( CURR ) ) ) )
         BSIZ2 = INT( HALF+SQRT( DBLE( QPTR( CURR+2 )-QPTR( CURR+
     $           1 ) ) ) )
         IF( BSIZ1.GT.0 ) THEN
            CALL DGEMV( 'T', BSIZ1, BSIZ1, ONE, Q( QPTR( CURR ) ),
     $                  BSIZ1, ZTEMP( 1 ), 1, ZERO, Z( ZPTR1 ), 1 )
         END IF
         CALL DCOPY( PSIZ1-BSIZ1, ZTEMP( BSIZ1+1 ), 1, Z( ZPTR1+BSIZ1 ),
     $               1 )
         IF( BSIZ2.GT.0 ) THEN
            CALL DGEMV( 'T', BSIZ2, BSIZ2, ONE, Q( QPTR( CURR+1 ) ),
     $                  BSIZ2, ZTEMP( PSIZ1+1 ), 1, ZERO, Z( MID ), 1 )
         END IF
         CALL DCOPY( PSIZ2-BSIZ2, ZTEMP( PSIZ1+BSIZ2+1 ), 1,
     $               Z( MID+BSIZ2 ), 1 )
         PTR = PTR + 2**( TLVLS-K )
   70 CONTINUE
      RETURN
      END
      SUBROUTINE DLAED8( ICOMPQ, K, N, QSIZ, D, Q, LDQ, INDXQ, RHO,
     $                   CUTPNT, Z, DLAMDA, Q2, LDQ2, W, PERM, GIVPTR,
     $                   GIVCOL, GIVNUM, INDXP, INDX, INFO )
      INTEGER            CUTPNT, GIVPTR, ICOMPQ, INFO, K, LDQ, LDQ2, N,
     $                   QSIZ
      DOUBLE PRECISION   RHO
      INTEGER            GIVCOL( 2, * ), INDX( * ), INDXP( * ),
     $                   INDXQ( * ), PERM( * )
      DOUBLE PRECISION   D( * ), DLAMDA( * ), GIVNUM( 2, * ),
     $                   Q( LDQ, * ), Q2( LDQ2, * ), W( * ), Z( * )
      DOUBLE PRECISION   MONE, ZERO, ONE, TWO, EIGHT
      PARAMETER          ( MONE = -1.0D0, ZERO = 0.0D0, ONE = 1.0D0,
     $                   TWO = 2.0D0, EIGHT = 8.0D0 )
      INTEGER            I, IMAX, J, JLAM, JMAX, JP, K2, N1, N1P1, N2
      DOUBLE PRECISION   C, EPS, S, T, TAU, TOL
      INTEGER            IDAMAX
      DOUBLE PRECISION   DLAMCH, DLAPY2
      EXTERNAL           IDAMAX, DLAMCH, DLAPY2
      EXTERNAL           DCOPY, DLACPY, DLAMRG, DROT, DSCAL, XERBLA
      INTRINSIC          ABS, MAX, MIN, SQRT
      INFO = 0
      IF( ICOMPQ.LT.0 .OR. ICOMPQ.GT.1 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( ICOMPQ.EQ.1 .AND. QSIZ.LT.N ) THEN
         INFO = -4
      ELSE IF( LDQ.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( CUTPNT.LT.MIN( 1, N ) .OR. CUTPNT.GT.N ) THEN
         INFO = -10
      ELSE IF( LDQ2.LT.MAX( 1, N ) ) THEN
         INFO = -14
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLAED8', -INFO )
         RETURN
      END IF
      GIVPTR = 0
      IF( N.EQ.0 )
     $   RETURN
      N1 = CUTPNT
      N2 = N - N1
      N1P1 = N1 + 1
      IF( RHO.LT.ZERO ) THEN
         CALL DSCAL( N2, MONE, Z( N1P1 ), 1 )
      END IF
      T = ONE / SQRT( TWO )
      DO 10 J = 1, N
         INDX( J ) = J
   10 CONTINUE
      CALL DSCAL( N, T, Z, 1 )
      RHO = ABS( TWO*RHO )
      DO 20 I = CUTPNT + 1, N
         INDXQ( I ) = INDXQ( I ) + CUTPNT
   20 CONTINUE
      DO 30 I = 1, N
         DLAMDA( I ) = D( INDXQ( I ) )
         W( I ) = Z( INDXQ( I ) )
   30 CONTINUE
      I = 1
      J = CUTPNT + 1
      CALL DLAMRG( N1, N2, DLAMDA, 1, 1, INDX )
      DO 40 I = 1, N
         D( I ) = DLAMDA( INDX( I ) )
         Z( I ) = W( INDX( I ) )
   40 CONTINUE
      IMAX = IDAMAX( N, Z, 1 )
      JMAX = IDAMAX( N, D, 1 )
      EPS = DLAMCH( 'Epsilon' )
      TOL = EIGHT*EPS*ABS( D( JMAX ) )
      IF( RHO*ABS( Z( IMAX ) ).LE.TOL ) THEN
         K = 0
         IF( ICOMPQ.EQ.0 ) THEN
            DO 50 J = 1, N
               PERM( J ) = INDXQ( INDX( J ) )
   50       CONTINUE
         ELSE
            DO 60 J = 1, N
               PERM( J ) = INDXQ( INDX( J ) )
               CALL DCOPY( QSIZ, Q( 1, PERM( J ) ), 1, Q2( 1, J ), 1 )
   60       CONTINUE
            CALL DLACPY( 'A', QSIZ, N, Q2( 1, 1 ), LDQ2, Q( 1, 1 ),
     $                   LDQ )
         END IF
         RETURN
      END IF
      K = 0
      K2 = N + 1
      DO 70 J = 1, N
         IF( RHO*ABS( Z( J ) ).LE.TOL ) THEN
            K2 = K2 - 1
            INDXP( K2 ) = J
            IF( J.EQ.N )
     $         GO TO 110
         ELSE
            JLAM = J
            GO TO 80
         END IF
   70 CONTINUE
   80 CONTINUE
      J = J + 1
      IF( J.GT.N )
     $   GO TO 100
      IF( RHO*ABS( Z( J ) ).LE.TOL ) THEN
         K2 = K2 - 1
         INDXP( K2 ) = J
      ELSE
         S = Z( JLAM )
         C = Z( J )
         TAU = DLAPY2( C, S )
         T = D( J ) - D( JLAM )
         C = C / TAU
         S = -S / TAU
         IF( ABS( T*C*S ).LE.TOL ) THEN
            Z( J ) = TAU
            Z( JLAM ) = ZERO
            GIVPTR = GIVPTR + 1
            GIVCOL( 1, GIVPTR ) = INDXQ( INDX( JLAM ) )
            GIVCOL( 2, GIVPTR ) = INDXQ( INDX( J ) )
            GIVNUM( 1, GIVPTR ) = C
            GIVNUM( 2, GIVPTR ) = S
            IF( ICOMPQ.EQ.1 ) THEN
               CALL DROT( QSIZ, Q( 1, INDXQ( INDX( JLAM ) ) ), 1,
     $                    Q( 1, INDXQ( INDX( J ) ) ), 1, C, S )
            END IF
            T = D( JLAM )*C*C + D( J )*S*S
            D( J ) = D( JLAM )*S*S + D( J )*C*C
            D( JLAM ) = T
            K2 = K2 - 1
            I = 1
   90       CONTINUE
            IF( K2+I.LE.N ) THEN
               IF( D( JLAM ).LT.D( INDXP( K2+I ) ) ) THEN
                  INDXP( K2+I-1 ) = INDXP( K2+I )
                  INDXP( K2+I ) = JLAM
                  I = I + 1
                  GO TO 90
               ELSE
                  INDXP( K2+I-1 ) = JLAM
               END IF
            ELSE
               INDXP( K2+I-1 ) = JLAM
            END IF
            JLAM = J
         ELSE
            K = K + 1
            W( K ) = Z( JLAM )
            DLAMDA( K ) = D( JLAM )
            INDXP( K ) = JLAM
            JLAM = J
         END IF
      END IF
      GO TO 80
  100 CONTINUE
      K = K + 1
      W( K ) = Z( JLAM )
      DLAMDA( K ) = D( JLAM )
      INDXP( K ) = JLAM
  110 CONTINUE
      IF( ICOMPQ.EQ.0 ) THEN
         DO 120 J = 1, N
            JP = INDXP( J )
            DLAMDA( J ) = D( JP )
            PERM( J ) = INDXQ( INDX( JP ) )
  120    CONTINUE
      ELSE
         DO 130 J = 1, N
            JP = INDXP( J )
            DLAMDA( J ) = D( JP )
            PERM( J ) = INDXQ( INDX( JP ) )
            CALL DCOPY( QSIZ, Q( 1, PERM( J ) ), 1, Q2( 1, J ), 1 )
  130    CONTINUE
      END IF
      IF( K.LT.N ) THEN
         IF( ICOMPQ.EQ.0 ) THEN
            CALL DCOPY( N-K, DLAMDA( K+1 ), 1, D( K+1 ), 1 )
         ELSE
            CALL DCOPY( N-K, DLAMDA( K+1 ), 1, D( K+1 ), 1 )
            CALL DLACPY( 'A', QSIZ, N-K, Q2( 1, K+1 ), LDQ2,
     $                   Q( 1, K+1 ), LDQ )
         END IF
      END IF
      RETURN
      END
      SUBROUTINE DLAED9( K, KSTART, KSTOP, N, D, Q, LDQ, RHO, DLAMDA, W,
     $                   S, LDS, INFO )
      INTEGER            INFO, K, KSTART, KSTOP, LDQ, LDS, N
      DOUBLE PRECISION   RHO
      DOUBLE PRECISION   D( * ), DLAMDA( * ), Q( LDQ, * ), S( LDS, * ),
     $                   W( * )
      INTEGER            I, J
      DOUBLE PRECISION   TEMP
      DOUBLE PRECISION   DLAMC3, DNRM2
      EXTERNAL           DLAMC3, DNRM2
      EXTERNAL           DCOPY, DLAED4, XERBLA
      INTRINSIC          MAX, SIGN, SQRT
      INFO = 0
      IF( K.LT.0 ) THEN
         INFO = -1
      ELSE IF( KSTART.LT.1 .OR. KSTART.GT.MAX( 1, K ) ) THEN
         INFO = -2
      ELSE IF( MAX( 1, KSTOP ).LT.KSTART .OR. KSTOP.GT.MAX( 1, K ) )
     $          THEN
         INFO = -3
      ELSE IF( N.LT.K ) THEN
         INFO = -4
      ELSE IF( LDQ.LT.MAX( 1, K ) ) THEN
         INFO = -7
      ELSE IF( LDS.LT.MAX( 1, K ) ) THEN
         INFO = -12
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLAED9', -INFO )
         RETURN
      END IF
      IF( K.EQ.0 )
     $   RETURN
      DO 10 I = 1, N
         DLAMDA( I ) = DLAMC3( DLAMDA( I ), DLAMDA( I ) ) - DLAMDA( I )
   10 CONTINUE
      DO 20 J = KSTART, KSTOP
         CALL DLAED4( K, J, DLAMDA, W, Q( 1, J ), RHO, D( J ), INFO )
         IF( INFO.NE.0 )
     $      GO TO 120
   20 CONTINUE
      IF( K.EQ.1 .OR. K.EQ.2 ) THEN
         DO 40 I = 1, K
            DO 30 J = 1, K
               S( J, I ) = Q( J, I )
   30       CONTINUE
   40    CONTINUE
         GO TO 120
      END IF
      CALL DCOPY( K, W, 1, S, 1 )
      CALL DCOPY( K, Q, LDQ+1, W, 1 )
      DO 70 J = 1, K
         DO 50 I = 1, J - 1
            W( I ) = W( I )*( Q( I, J ) / ( DLAMDA( I )-DLAMDA( J ) ) )
   50    CONTINUE
         DO 60 I = J + 1, K
            W( I ) = W( I )*( Q( I, J ) / ( DLAMDA( I )-DLAMDA( J ) ) )
   60    CONTINUE
   70 CONTINUE
      DO 80 I = 1, K
         W( I ) = SIGN( SQRT( -W( I ) ), S( I, 1 ) )
   80 CONTINUE
      DO 110 J = 1, K
         DO 90 I = 1, K
            Q( I, J ) = W( I ) / Q( I, J )
   90    CONTINUE
         TEMP = DNRM2( K, Q( 1, J ), 1 )
         DO 100 I = 1, K
            S( I, J ) = Q( I, J ) / TEMP
  100    CONTINUE
  110 CONTINUE
  120 CONTINUE
      RETURN
      END
      SUBROUTINE DROT(N,DX,INCX,DY,INCY,C,S)
      DOUBLE PRECISION C,S
      INTEGER INCX,INCY,N
      DOUBLE PRECISION DX(*),DY(*)
      DOUBLE PRECISION DTEMP
      INTEGER I,IX,IY
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
         DO I = 1,N
            DTEMP = C*DX(I) + S*DY(I)
            DY(I) = C*DY(I) - S*DX(I)
            DX(I) = DTEMP
         END DO
      ELSE
         IX = 1
         IY = 1
         IF (INCX.LT.0) IX = (-N+1)*INCX + 1
         IF (INCY.LT.0) IY = (-N+1)*INCY + 1
         DO I = 1,N
            DTEMP = C*DX(IX) + S*DY(IY)
            DY(IY) = C*DY(IY) - S*DX(IX)
            DX(IX) = DTEMP
            IX = IX + INCX
            IY = IY + INCY
         END DO
      END IF
      RETURN
      END
      SUBROUTINE DLAED4( N, I, D, Z, DELTA, RHO, DLAM, INFO )
      INTEGER            I, INFO, N
      DOUBLE PRECISION   DLAM, RHO
      DOUBLE PRECISION   D( * ), DELTA( * ), Z( * )
      INTEGER            MAXIT
      PARAMETER          ( MAXIT = 30 )
      DOUBLE PRECISION   ZERO, ONE, TWO, THREE, FOUR, EIGHT, TEN
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                   THREE = 3.0D0, FOUR = 4.0D0, EIGHT = 8.0D0,
     $                   TEN = 10.0D0 )
      LOGICAL            ORGATI, SWTCH, SWTCH3
      INTEGER            II, IIM1, IIP1, IP1, ITER, J, NITER
      DOUBLE PRECISION   A, B, C, DEL, DLTLB, DLTUB, DPHI, DPSI, DW,
     $                   EPS, ERRETM, ETA, MIDPT, PHI, PREW, PSI,
     $                   RHOINV, TAU, TEMP, TEMP1, W
      DOUBLE PRECISION   ZZ( 3 )
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
      EXTERNAL           DLAED5, DLAED6
      INTRINSIC          ABS, MAX, MIN, SQRT
      INFO = 0
      IF( N.EQ.1 ) THEN
         DLAM = D( 1 ) + RHO*Z( 1 )*Z( 1 )
         DELTA( 1 ) = ONE
         RETURN
      END IF
      IF( N.EQ.2 ) THEN
         CALL DLAED5( I, D, Z, DELTA, RHO, DLAM )
         RETURN
      END IF
      EPS = DLAMCH( 'Epsilon' )
      RHOINV = ONE / RHO
      IF( I.EQ.N ) THEN
         II = N - 1
         NITER = 1
         MIDPT = RHO / TWO
         DO 10 J = 1, N
            DELTA( J ) = ( D( J )-D( I ) ) - MIDPT
   10    CONTINUE
         PSI = ZERO
         DO 20 J = 1, N - 2
            PSI = PSI + Z( J )*Z( J ) / DELTA( J )
   20    CONTINUE
         C = RHOINV + PSI
         W = C + Z( II )*Z( II ) / DELTA( II ) +
     $       Z( N )*Z( N ) / DELTA( N )
         IF( W.LE.ZERO ) THEN
            TEMP = Z( N-1 )*Z( N-1 ) / ( D( N )-D( N-1 )+RHO ) +
     $             Z( N )*Z( N ) / RHO
            IF( C.LE.TEMP ) THEN
               TAU = RHO
            ELSE
               DEL = D( N ) - D( N-1 )
               A = -C*DEL + Z( N-1 )*Z( N-1 ) + Z( N )*Z( N )
               B = Z( N )*Z( N )*DEL
               IF( A.LT.ZERO ) THEN
                  TAU = TWO*B / ( SQRT( A*A+FOUR*B*C )-A )
               ELSE
                  TAU = ( A+SQRT( A*A+FOUR*B*C ) ) / ( TWO*C )
               END IF
            END IF
            DLTLB = MIDPT
            DLTUB = RHO
         ELSE
            DEL = D( N ) - D( N-1 )
            A = -C*DEL + Z( N-1 )*Z( N-1 ) + Z( N )*Z( N )
            B = Z( N )*Z( N )*DEL
            IF( A.LT.ZERO ) THEN
               TAU = TWO*B / ( SQRT( A*A+FOUR*B*C )-A )
            ELSE
               TAU = ( A+SQRT( A*A+FOUR*B*C ) ) / ( TWO*C )
            END IF
            DLTLB = ZERO
            DLTUB = MIDPT
         END IF
         DO 30 J = 1, N
            DELTA( J ) = ( D( J )-D( I ) ) - TAU
   30    CONTINUE
         DPSI = ZERO
         PSI = ZERO
         ERRETM = ZERO
         DO 40 J = 1, II
            TEMP = Z( J ) / DELTA( J )
            PSI = PSI + Z( J )*TEMP
            DPSI = DPSI + TEMP*TEMP
            ERRETM = ERRETM + PSI
   40    CONTINUE
         ERRETM = ABS( ERRETM )
         TEMP = Z( N ) / DELTA( N )
         PHI = Z( N )*TEMP
         DPHI = TEMP*TEMP
         ERRETM = EIGHT*( -PHI-PSI ) + ERRETM - PHI + RHOINV +
     $            ABS( TAU )*( DPSI+DPHI )
         W = RHOINV + PHI + PSI
         IF( ABS( W ).LE.EPS*ERRETM ) THEN
            DLAM = D( I ) + TAU
            GO TO 250
         END IF
         IF( W.LE.ZERO ) THEN
            DLTLB = MAX( DLTLB, TAU )
         ELSE
            DLTUB = MIN( DLTUB, TAU )
         END IF
         NITER = NITER + 1
         C = W - DELTA( N-1 )*DPSI - DELTA( N )*DPHI
         A = ( DELTA( N-1 )+DELTA( N ) )*W -
     $       DELTA( N-1 )*DELTA( N )*( DPSI+DPHI )
         B = DELTA( N-1 )*DELTA( N )*W
         IF( C.LT.ZERO )
     $      C = ABS( C )
         IF( C.EQ.ZERO ) THEN
            ETA = DLTUB - TAU
         ELSE IF( A.GE.ZERO ) THEN
            ETA = ( A+SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
         ELSE
            ETA = TWO*B / ( A-SQRT( ABS( A*A-FOUR*B*C ) ) )
         END IF
         IF( W*ETA.GT.ZERO )
     $      ETA = -W / ( DPSI+DPHI )
         TEMP = TAU + ETA
         IF( TEMP.GT.DLTUB .OR. TEMP.LT.DLTLB ) THEN
            IF( W.LT.ZERO ) THEN
               ETA = ( DLTUB-TAU ) / TWO
            ELSE
               ETA = ( DLTLB-TAU ) / TWO
            END IF
         END IF
         DO 50 J = 1, N
            DELTA( J ) = DELTA( J ) - ETA
   50    CONTINUE
         TAU = TAU + ETA
         DPSI = ZERO
         PSI = ZERO
         ERRETM = ZERO
         DO 60 J = 1, II
            TEMP = Z( J ) / DELTA( J )
            PSI = PSI + Z( J )*TEMP
            DPSI = DPSI + TEMP*TEMP
            ERRETM = ERRETM + PSI
   60    CONTINUE
         ERRETM = ABS( ERRETM )
         TEMP = Z( N ) / DELTA( N )
         PHI = Z( N )*TEMP
         DPHI = TEMP*TEMP
         ERRETM = EIGHT*( -PHI-PSI ) + ERRETM - PHI + RHOINV +
     $            ABS( TAU )*( DPSI+DPHI )
         W = RHOINV + PHI + PSI
         ITER = NITER + 1
         DO 90 NITER = ITER, MAXIT
            IF( ABS( W ).LE.EPS*ERRETM ) THEN
               DLAM = D( I ) + TAU
               GO TO 250
            END IF
            IF( W.LE.ZERO ) THEN
               DLTLB = MAX( DLTLB, TAU )
            ELSE
               DLTUB = MIN( DLTUB, TAU )
            END IF
            C = W - DELTA( N-1 )*DPSI - DELTA( N )*DPHI
            A = ( DELTA( N-1 )+DELTA( N ) )*W -
     $          DELTA( N-1 )*DELTA( N )*( DPSI+DPHI )
            B = DELTA( N-1 )*DELTA( N )*W
            IF( A.GE.ZERO ) THEN
               ETA = ( A+SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
            ELSE
               ETA = TWO*B / ( A-SQRT( ABS( A*A-FOUR*B*C ) ) )
            END IF
            IF( W*ETA.GT.ZERO )
     $         ETA = -W / ( DPSI+DPHI )
            TEMP = TAU + ETA
            IF( TEMP.GT.DLTUB .OR. TEMP.LT.DLTLB ) THEN
               IF( W.LT.ZERO ) THEN
                  ETA = ( DLTUB-TAU ) / TWO
               ELSE
                  ETA = ( DLTLB-TAU ) / TWO
               END IF
            END IF
            DO 70 J = 1, N
               DELTA( J ) = DELTA( J ) - ETA
   70       CONTINUE
            TAU = TAU + ETA
            DPSI = ZERO
            PSI = ZERO
            ERRETM = ZERO
            DO 80 J = 1, II
               TEMP = Z( J ) / DELTA( J )
               PSI = PSI + Z( J )*TEMP
               DPSI = DPSI + TEMP*TEMP
               ERRETM = ERRETM + PSI
   80       CONTINUE
            ERRETM = ABS( ERRETM )
            TEMP = Z( N ) / DELTA( N )
            PHI = Z( N )*TEMP
            DPHI = TEMP*TEMP
            ERRETM = EIGHT*( -PHI-PSI ) + ERRETM - PHI + RHOINV +
     $               ABS( TAU )*( DPSI+DPHI )
            W = RHOINV + PHI + PSI
   90    CONTINUE
         INFO = 1
         DLAM = D( I ) + TAU
         GO TO 250
      ELSE
         NITER = 1
         IP1 = I + 1
         DEL = D( IP1 ) - D( I )
         MIDPT = DEL / TWO
         DO 100 J = 1, N
            DELTA( J ) = ( D( J )-D( I ) ) - MIDPT
  100    CONTINUE
         PSI = ZERO
         DO 110 J = 1, I - 1
            PSI = PSI + Z( J )*Z( J ) / DELTA( J )
  110    CONTINUE
         PHI = ZERO
         DO 120 J = N, I + 2, -1
            PHI = PHI + Z( J )*Z( J ) / DELTA( J )
  120    CONTINUE
         C = RHOINV + PSI + PHI
         W = C + Z( I )*Z( I ) / DELTA( I ) +
     $       Z( IP1 )*Z( IP1 ) / DELTA( IP1 )
         IF( W.GT.ZERO ) THEN
            ORGATI = .TRUE.
            A = C*DEL + Z( I )*Z( I ) + Z( IP1 )*Z( IP1 )
            B = Z( I )*Z( I )*DEL
            IF( A.GT.ZERO ) THEN
               TAU = TWO*B / ( A+SQRT( ABS( A*A-FOUR*B*C ) ) )
            ELSE
               TAU = ( A-SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
            END IF
            DLTLB = ZERO
            DLTUB = MIDPT
         ELSE
            ORGATI = .FALSE.
            A = C*DEL - Z( I )*Z( I ) - Z( IP1 )*Z( IP1 )
            B = Z( IP1 )*Z( IP1 )*DEL
            IF( A.LT.ZERO ) THEN
               TAU = TWO*B / ( A-SQRT( ABS( A*A+FOUR*B*C ) ) )
            ELSE
               TAU = -( A+SQRT( ABS( A*A+FOUR*B*C ) ) ) / ( TWO*C )
            END IF
            DLTLB = -MIDPT
            DLTUB = ZERO
         END IF
         IF( ORGATI ) THEN
            DO 130 J = 1, N
               DELTA( J ) = ( D( J )-D( I ) ) - TAU
  130       CONTINUE
         ELSE
            DO 140 J = 1, N
               DELTA( J ) = ( D( J )-D( IP1 ) ) - TAU
  140       CONTINUE
         END IF
         IF( ORGATI ) THEN
            II = I
         ELSE
            II = I + 1
         END IF
         IIM1 = II - 1
         IIP1 = II + 1
         DPSI = ZERO
         PSI = ZERO
         ERRETM = ZERO
         DO 150 J = 1, IIM1
            TEMP = Z( J ) / DELTA( J )
            PSI = PSI + Z( J )*TEMP
            DPSI = DPSI + TEMP*TEMP
            ERRETM = ERRETM + PSI
  150    CONTINUE
         ERRETM = ABS( ERRETM )
         DPHI = ZERO
         PHI = ZERO
         DO 160 J = N, IIP1, -1
            TEMP = Z( J ) / DELTA( J )
            PHI = PHI + Z( J )*TEMP
            DPHI = DPHI + TEMP*TEMP
            ERRETM = ERRETM + PHI
  160    CONTINUE
         W = RHOINV + PHI + PSI
         SWTCH3 = .FALSE.
         IF( ORGATI ) THEN
            IF( W.LT.ZERO )
     $         SWTCH3 = .TRUE.
         ELSE
            IF( W.GT.ZERO )
     $         SWTCH3 = .TRUE.
         END IF
         IF( II.EQ.1 .OR. II.EQ.N )
     $      SWTCH3 = .FALSE.
         TEMP = Z( II ) / DELTA( II )
         DW = DPSI + DPHI + TEMP*TEMP
         TEMP = Z( II )*TEMP
         W = W + TEMP
         ERRETM = EIGHT*( PHI-PSI ) + ERRETM + TWO*RHOINV +
     $            THREE*ABS( TEMP ) + ABS( TAU )*DW
         IF( ABS( W ).LE.EPS*ERRETM ) THEN
            IF( ORGATI ) THEN
               DLAM = D( I ) + TAU
            ELSE
               DLAM = D( IP1 ) + TAU
            END IF
            GO TO 250
         END IF
         IF( W.LE.ZERO ) THEN
            DLTLB = MAX( DLTLB, TAU )
         ELSE
            DLTUB = MIN( DLTUB, TAU )
         END IF
         NITER = NITER + 1
         IF( .NOT.SWTCH3 ) THEN
            IF( ORGATI ) THEN
               C = W - DELTA( IP1 )*DW - ( D( I )-D( IP1 ) )*
     $             ( Z( I ) / DELTA( I ) )**2
            ELSE
               C = W - DELTA( I )*DW - ( D( IP1 )-D( I ) )*
     $             ( Z( IP1 ) / DELTA( IP1 ) )**2
            END IF
            A = ( DELTA( I )+DELTA( IP1 ) )*W -
     $          DELTA( I )*DELTA( IP1 )*DW
            B = DELTA( I )*DELTA( IP1 )*W
            IF( C.EQ.ZERO ) THEN
               IF( A.EQ.ZERO ) THEN
                  IF( ORGATI ) THEN
                     A = Z( I )*Z( I ) + DELTA( IP1 )*DELTA( IP1 )*
     $                   ( DPSI+DPHI )
                  ELSE
                     A = Z( IP1 )*Z( IP1 ) + DELTA( I )*DELTA( I )*
     $                   ( DPSI+DPHI )
                  END IF
               END IF
               ETA = B / A
            ELSE IF( A.LE.ZERO ) THEN
               ETA = ( A-SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
            ELSE
               ETA = TWO*B / ( A+SQRT( ABS( A*A-FOUR*B*C ) ) )
            END IF
         ELSE
            TEMP = RHOINV + PSI + PHI
            IF( ORGATI ) THEN
               TEMP1 = Z( IIM1 ) / DELTA( IIM1 )
               TEMP1 = TEMP1*TEMP1
               C = TEMP - DELTA( IIP1 )*( DPSI+DPHI ) -
     $             ( D( IIM1 )-D( IIP1 ) )*TEMP1
               ZZ( 1 ) = Z( IIM1 )*Z( IIM1 )
               ZZ( 3 ) = DELTA( IIP1 )*DELTA( IIP1 )*
     $                   ( ( DPSI-TEMP1 )+DPHI )
            ELSE
               TEMP1 = Z( IIP1 ) / DELTA( IIP1 )
               TEMP1 = TEMP1*TEMP1
               C = TEMP - DELTA( IIM1 )*( DPSI+DPHI ) -
     $             ( D( IIP1 )-D( IIM1 ) )*TEMP1
               ZZ( 1 ) = DELTA( IIM1 )*DELTA( IIM1 )*
     $                   ( DPSI+( DPHI-TEMP1 ) )
               ZZ( 3 ) = Z( IIP1 )*Z( IIP1 )
            END IF
            ZZ( 2 ) = Z( II )*Z( II )
            CALL DLAED6( NITER, ORGATI, C, DELTA( IIM1 ), ZZ, W, ETA,
     $                   INFO )
            IF( INFO.NE.0 )
     $         GO TO 250
         END IF
         IF( W*ETA.GE.ZERO )
     $      ETA = -W / DW
         TEMP = TAU + ETA
         IF( TEMP.GT.DLTUB .OR. TEMP.LT.DLTLB ) THEN
            IF( W.LT.ZERO ) THEN
               ETA = ( DLTUB-TAU ) / TWO
            ELSE
               ETA = ( DLTLB-TAU ) / TWO
            END IF
         END IF
         PREW = W
         DO 180 J = 1, N
            DELTA( J ) = DELTA( J ) - ETA
  180    CONTINUE
         DPSI = ZERO
         PSI = ZERO
         ERRETM = ZERO
         DO 190 J = 1, IIM1
            TEMP = Z( J ) / DELTA( J )
            PSI = PSI + Z( J )*TEMP
            DPSI = DPSI + TEMP*TEMP
            ERRETM = ERRETM + PSI
  190    CONTINUE
         ERRETM = ABS( ERRETM )
         DPHI = ZERO
         PHI = ZERO
         DO 200 J = N, IIP1, -1
            TEMP = Z( J ) / DELTA( J )
            PHI = PHI + Z( J )*TEMP
            DPHI = DPHI + TEMP*TEMP
            ERRETM = ERRETM + PHI
  200    CONTINUE
         TEMP = Z( II ) / DELTA( II )
         DW = DPSI + DPHI + TEMP*TEMP
         TEMP = Z( II )*TEMP
         W = RHOINV + PHI + PSI + TEMP
         ERRETM = EIGHT*( PHI-PSI ) + ERRETM + TWO*RHOINV +
     $            THREE*ABS( TEMP ) + ABS( TAU+ETA )*DW
         SWTCH = .FALSE.
         IF( ORGATI ) THEN
            IF( -W.GT.ABS( PREW ) / TEN )
     $         SWTCH = .TRUE.
         ELSE
            IF( W.GT.ABS( PREW ) / TEN )
     $         SWTCH = .TRUE.
         END IF
         TAU = TAU + ETA
         ITER = NITER + 1
         DO 240 NITER = ITER, MAXIT
            IF( ABS( W ).LE.EPS*ERRETM ) THEN
               IF( ORGATI ) THEN
                  DLAM = D( I ) + TAU
               ELSE
                  DLAM = D( IP1 ) + TAU
               END IF
               GO TO 250
            END IF
            IF( W.LE.ZERO ) THEN
               DLTLB = MAX( DLTLB, TAU )
            ELSE
               DLTUB = MIN( DLTUB, TAU )
            END IF
            IF( .NOT.SWTCH3 ) THEN
               IF( .NOT.SWTCH ) THEN
                  IF( ORGATI ) THEN
                     C = W - DELTA( IP1 )*DW -
     $                   ( D( I )-D( IP1 ) )*( Z( I ) / DELTA( I ) )**2
                  ELSE
                     C = W - DELTA( I )*DW - ( D( IP1 )-D( I ) )*
     $                   ( Z( IP1 ) / DELTA( IP1 ) )**2
                  END IF
               ELSE
                  TEMP = Z( II ) / DELTA( II )
                  IF( ORGATI ) THEN
                     DPSI = DPSI + TEMP*TEMP
                  ELSE
                     DPHI = DPHI + TEMP*TEMP
                  END IF
                  C = W - DELTA( I )*DPSI - DELTA( IP1 )*DPHI
               END IF
               A = ( DELTA( I )+DELTA( IP1 ) )*W -
     $             DELTA( I )*DELTA( IP1 )*DW
               B = DELTA( I )*DELTA( IP1 )*W
               IF( C.EQ.ZERO ) THEN
                  IF( A.EQ.ZERO ) THEN
                     IF( .NOT.SWTCH ) THEN
                        IF( ORGATI ) THEN
                           A = Z( I )*Z( I ) + DELTA( IP1 )*
     $                         DELTA( IP1 )*( DPSI+DPHI )
                        ELSE
                           A = Z( IP1 )*Z( IP1 ) +
     $                         DELTA( I )*DELTA( I )*( DPSI+DPHI )
                        END IF
                     ELSE
                        A = DELTA( I )*DELTA( I )*DPSI +
     $                      DELTA( IP1 )*DELTA( IP1 )*DPHI
                     END IF
                  END IF
                  ETA = B / A
               ELSE IF( A.LE.ZERO ) THEN
                  ETA = ( A-SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
               ELSE
                  ETA = TWO*B / ( A+SQRT( ABS( A*A-FOUR*B*C ) ) )
               END IF
            ELSE
               TEMP = RHOINV + PSI + PHI
               IF( SWTCH ) THEN
                  C = TEMP - DELTA( IIM1 )*DPSI - DELTA( IIP1 )*DPHI
                  ZZ( 1 ) = DELTA( IIM1 )*DELTA( IIM1 )*DPSI
                  ZZ( 3 ) = DELTA( IIP1 )*DELTA( IIP1 )*DPHI
               ELSE
                  IF( ORGATI ) THEN
                     TEMP1 = Z( IIM1 ) / DELTA( IIM1 )
                     TEMP1 = TEMP1*TEMP1
                     C = TEMP - DELTA( IIP1 )*( DPSI+DPHI ) -
     $                   ( D( IIM1 )-D( IIP1 ) )*TEMP1
                     ZZ( 1 ) = Z( IIM1 )*Z( IIM1 )
                     ZZ( 3 ) = DELTA( IIP1 )*DELTA( IIP1 )*
     $                         ( ( DPSI-TEMP1 )+DPHI )
                  ELSE
                     TEMP1 = Z( IIP1 ) / DELTA( IIP1 )
                     TEMP1 = TEMP1*TEMP1
                     C = TEMP - DELTA( IIM1 )*( DPSI+DPHI ) -
     $                   ( D( IIP1 )-D( IIM1 ) )*TEMP1
                     ZZ( 1 ) = DELTA( IIM1 )*DELTA( IIM1 )*
     $                         ( DPSI+( DPHI-TEMP1 ) )
                     ZZ( 3 ) = Z( IIP1 )*Z( IIP1 )
                  END IF
               END IF
               CALL DLAED6( NITER, ORGATI, C, DELTA( IIM1 ), ZZ, W, ETA,
     $                      INFO )
               IF( INFO.NE.0 )
     $            GO TO 250
            END IF
            IF( W*ETA.GE.ZERO )
     $         ETA = -W / DW
            TEMP = TAU + ETA
            IF( TEMP.GT.DLTUB .OR. TEMP.LT.DLTLB ) THEN
               IF( W.LT.ZERO ) THEN
                  ETA = ( DLTUB-TAU ) / TWO
               ELSE
                  ETA = ( DLTLB-TAU ) / TWO
               END IF
            END IF
            DO 210 J = 1, N
               DELTA( J ) = DELTA( J ) - ETA
  210       CONTINUE
            TAU = TAU + ETA
            PREW = W
            DPSI = ZERO
            PSI = ZERO
            ERRETM = ZERO
            DO 220 J = 1, IIM1
               TEMP = Z( J ) / DELTA( J )
               PSI = PSI + Z( J )*TEMP
               DPSI = DPSI + TEMP*TEMP
               ERRETM = ERRETM + PSI
  220       CONTINUE
            ERRETM = ABS( ERRETM )
            DPHI = ZERO
            PHI = ZERO
            DO 230 J = N, IIP1, -1
               TEMP = Z( J ) / DELTA( J )
               PHI = PHI + Z( J )*TEMP
               DPHI = DPHI + TEMP*TEMP
               ERRETM = ERRETM + PHI
  230       CONTINUE
            TEMP = Z( II ) / DELTA( II )
            DW = DPSI + DPHI + TEMP*TEMP
            TEMP = Z( II )*TEMP
            W = RHOINV + PHI + PSI + TEMP
            ERRETM = EIGHT*( PHI-PSI ) + ERRETM + TWO*RHOINV +
     $               THREE*ABS( TEMP ) + ABS( TAU )*DW
            IF( W*PREW.GT.ZERO .AND. ABS( W ).GT.ABS( PREW ) / TEN )
     $         SWTCH = .NOT.SWTCH
  240    CONTINUE
         INFO = 1
         IF( ORGATI ) THEN
            DLAM = D( I ) + TAU
         ELSE
            DLAM = D( IP1 ) + TAU
         END IF
      END IF
  250 CONTINUE
      RETURN
      END
      INTEGER FUNCTION ILADLC( M, N, A, LDA )
      INTEGER            M, N, LDA
      DOUBLE PRECISION   A( LDA, * )
      DOUBLE PRECISION ZERO
      PARAMETER ( ZERO = 0.0D+0 )
      INTEGER I
      IF( N.EQ.0 ) THEN
         ILADLC = N
      ELSE IF( A(1, N).NE.ZERO .OR. A(M, N).NE.ZERO ) THEN
         ILADLC = N
      ELSE
         DO ILADLC = N, 1, -1
            DO I = 1, M
               IF( A(I, ILADLC).NE.ZERO ) RETURN
            END DO
         END DO
      END IF
      RETURN
      END
      INTEGER FUNCTION ILADLR( M, N, A, LDA )
      INTEGER            M, N, LDA
      DOUBLE PRECISION   A( LDA, * )
      DOUBLE PRECISION ZERO
      PARAMETER ( ZERO = 0.0D+0 )
      INTEGER I, J
      IF( M.EQ.0 ) THEN
         ILADLR = M
      ELSE IF( A(M, 1).NE.ZERO .OR. A(M, N).NE.ZERO ) THEN
         ILADLR = M
      ELSE
         ILADLR = 0
         DO J = 1, N
            I=M
            DO WHILE((A(MAX(I,1),J).EQ.ZERO).AND.(I.GE.1))
               I=I-1
            ENDDO
            ILADLR = MAX( ILADLR, I )
         END DO
      END IF
      RETURN
      END
      SUBROUTINE DLAED5( I, D, Z, DELTA, RHO, DLAM )
      INTEGER            I
      DOUBLE PRECISION   DLAM, RHO
      DOUBLE PRECISION   D( 2 ), DELTA( 2 ), Z( 2 )
      DOUBLE PRECISION   ZERO, ONE, TWO, FOUR
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                   FOUR = 4.0D0 )
      DOUBLE PRECISION   B, C, DEL, TAU, TEMP, W
      INTRINSIC          ABS, SQRT
      DEL = D( 2 ) - D( 1 )
      IF( I.EQ.1 ) THEN
         W = ONE + TWO*RHO*( Z( 2 )*Z( 2 )-Z( 1 )*Z( 1 ) ) / DEL
         IF( W.GT.ZERO ) THEN
            B = DEL + RHO*( Z( 1 )*Z( 1 )+Z( 2 )*Z( 2 ) )
            C = RHO*Z( 1 )*Z( 1 )*DEL
            TAU = TWO*C / ( B+SQRT( ABS( B*B-FOUR*C ) ) )
            DLAM = D( 1 ) + TAU
            DELTA( 1 ) = -Z( 1 ) / TAU
            DELTA( 2 ) = Z( 2 ) / ( DEL-TAU )
         ELSE
            B = -DEL + RHO*( Z( 1 )*Z( 1 )+Z( 2 )*Z( 2 ) )
            C = RHO*Z( 2 )*Z( 2 )*DEL
            IF( B.GT.ZERO ) THEN
               TAU = -TWO*C / ( B+SQRT( B*B+FOUR*C ) )
            ELSE
               TAU = ( B-SQRT( B*B+FOUR*C ) ) / TWO
            END IF
            DLAM = D( 2 ) + TAU
            DELTA( 1 ) = -Z( 1 ) / ( DEL+TAU )
            DELTA( 2 ) = -Z( 2 ) / TAU
         END IF
         TEMP = SQRT( DELTA( 1 )*DELTA( 1 )+DELTA( 2 )*DELTA( 2 ) )
         DELTA( 1 ) = DELTA( 1 ) / TEMP
         DELTA( 2 ) = DELTA( 2 ) / TEMP
      ELSE
         B = -DEL + RHO*( Z( 1 )*Z( 1 )+Z( 2 )*Z( 2 ) )
         C = RHO*Z( 2 )*Z( 2 )*DEL
         IF( B.GT.ZERO ) THEN
            TAU = ( B+SQRT( B*B+FOUR*C ) ) / TWO
         ELSE
            TAU = TWO*C / ( -B+SQRT( B*B+FOUR*C ) )
         END IF
         DLAM = D( 2 ) + TAU
         DELTA( 1 ) = -Z( 1 ) / ( DEL+TAU )
         DELTA( 2 ) = -Z( 2 ) / TAU
         TEMP = SQRT( DELTA( 1 )*DELTA( 1 )+DELTA( 2 )*DELTA( 2 ) )
         DELTA( 1 ) = DELTA( 1 ) / TEMP
         DELTA( 2 ) = DELTA( 2 ) / TEMP
      END IF
      RETURN
      END
      SUBROUTINE DLAED6( KNITER, ORGATI, RHO, D, Z, FINIT, TAU, INFO )
      LOGICAL            ORGATI
      INTEGER            INFO, KNITER
      DOUBLE PRECISION   FINIT, RHO, TAU
      DOUBLE PRECISION   D( 3 ), Z( 3 )
      INTEGER            MAXIT
      PARAMETER          ( MAXIT = 40 )
      DOUBLE PRECISION   ZERO, ONE, TWO, THREE, FOUR, EIGHT
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                   THREE = 3.0D0, FOUR = 4.0D0, EIGHT = 8.0D0 )
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
      DOUBLE PRECISION   DSCALE( 3 ), ZSCALE( 3 )
      LOGICAL            SCALE
      INTEGER            I, ITER, NITER
      DOUBLE PRECISION   A, B, BASE, C, DDF, DF, EPS, ERRETM, ETA, F,
     $                   FC, SCLFAC, SCLINV, SMALL1, SMALL2, SMINV1,
     $                   SMINV2, TEMP, TEMP1, TEMP2, TEMP3, TEMP4,
     $                   LBD, UBD
      INTRINSIC          ABS, INT, LOG, MAX, MIN, SQRT
      INFO = 0
      IF( ORGATI ) THEN
         LBD = D(2)
         UBD = D(3)
      ELSE
         LBD = D(1)
         UBD = D(2)
      END IF
      IF( FINIT .LT. ZERO )THEN
         LBD = ZERO
      ELSE
         UBD = ZERO
      END IF
      NITER = 1
      TAU = ZERO
      IF( KNITER.EQ.2 ) THEN
         IF( ORGATI ) THEN
            TEMP = ( D( 3 )-D( 2 ) ) / TWO
            C = RHO + Z( 1 ) / ( ( D( 1 )-D( 2 ) )-TEMP )
            A = C*( D( 2 )+D( 3 ) ) + Z( 2 ) + Z( 3 )
            B = C*D( 2 )*D( 3 ) + Z( 2 )*D( 3 ) + Z( 3 )*D( 2 )
         ELSE
            TEMP = ( D( 1 )-D( 2 ) ) / TWO
            C = RHO + Z( 3 ) / ( ( D( 3 )-D( 2 ) )-TEMP )
            A = C*( D( 1 )+D( 2 ) ) + Z( 1 ) + Z( 2 )
            B = C*D( 1 )*D( 2 ) + Z( 1 )*D( 2 ) + Z( 2 )*D( 1 )
         END IF
         TEMP = MAX( ABS( A ), ABS( B ), ABS( C ) )
         A = A / TEMP
         B = B / TEMP
         C = C / TEMP
         IF( C.EQ.ZERO ) THEN
            TAU = B / A
         ELSE IF( A.LE.ZERO ) THEN
            TAU = ( A-SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
         ELSE
            TAU = TWO*B / ( A+SQRT( ABS( A*A-FOUR*B*C ) ) )
         END IF
         IF( TAU .LT. LBD .OR. TAU .GT. UBD )
     $      TAU = ( LBD+UBD )/TWO
         IF( D(1).EQ.TAU .OR. D(2).EQ.TAU .OR. D(3).EQ.TAU ) THEN
            TAU = ZERO
         ELSE
            TEMP = FINIT + TAU*Z(1)/( D(1)*( D( 1 )-TAU ) ) +
     $                     TAU*Z(2)/( D(2)*( D( 2 )-TAU ) ) +
     $                     TAU*Z(3)/( D(3)*( D( 3 )-TAU ) )
            IF( TEMP .LE. ZERO )THEN
               LBD = TAU
            ELSE
               UBD = TAU
            END IF
            IF( ABS( FINIT ).LE.ABS( TEMP ) )
     $         TAU = ZERO
         END IF
      END IF
      EPS = DLAMCH( 'Epsilon' )
      BASE = DLAMCH( 'Base' )
      SMALL1 = BASE**( INT( LOG( DLAMCH( 'SafMin' ) ) / LOG( BASE ) /
     $         THREE ) )
      SMINV1 = ONE / SMALL1
      SMALL2 = SMALL1*SMALL1
      SMINV2 = SMINV1*SMINV1
      IF( ORGATI ) THEN
         TEMP = MIN( ABS( D( 2 )-TAU ), ABS( D( 3 )-TAU ) )
      ELSE
         TEMP = MIN( ABS( D( 1 )-TAU ), ABS( D( 2 )-TAU ) )
      END IF
      SCALE = .FALSE.
      IF( TEMP.LE.SMALL1 ) THEN
         SCALE = .TRUE.
         IF( TEMP.LE.SMALL2 ) THEN
            SCLFAC = SMINV2
            SCLINV = SMALL2
         ELSE
            SCLFAC = SMINV1
            SCLINV = SMALL1
         END IF
         DO 10 I = 1, 3
            DSCALE( I ) = D( I )*SCLFAC
            ZSCALE( I ) = Z( I )*SCLFAC
   10    CONTINUE
         TAU = TAU*SCLFAC
         LBD = LBD*SCLFAC
         UBD = UBD*SCLFAC
      ELSE
         DO 20 I = 1, 3
            DSCALE( I ) = D( I )
            ZSCALE( I ) = Z( I )
   20    CONTINUE
      END IF
      FC = ZERO
      DF = ZERO
      DDF = ZERO
      DO 30 I = 1, 3
         TEMP = ONE / ( DSCALE( I )-TAU )
         TEMP1 = ZSCALE( I )*TEMP
         TEMP2 = TEMP1*TEMP
         TEMP3 = TEMP2*TEMP
         FC = FC + TEMP1 / DSCALE( I )
         DF = DF + TEMP2
         DDF = DDF + TEMP3
   30 CONTINUE
      F = FINIT + TAU*FC
      IF( ABS( F ).LE.ZERO )
     $   GO TO 60
      IF( F .LE. ZERO )THEN
         LBD = TAU
      ELSE
         UBD = TAU
      END IF
      ITER = NITER + 1
      DO 50 NITER = ITER, MAXIT
         IF( ORGATI ) THEN
            TEMP1 = DSCALE( 2 ) - TAU
            TEMP2 = DSCALE( 3 ) - TAU
         ELSE
            TEMP1 = DSCALE( 1 ) - TAU
            TEMP2 = DSCALE( 2 ) - TAU
         END IF
         A = ( TEMP1+TEMP2 )*F - TEMP1*TEMP2*DF
         B = TEMP1*TEMP2*F
         C = F - ( TEMP1+TEMP2 )*DF + TEMP1*TEMP2*DDF
         TEMP = MAX( ABS( A ), ABS( B ), ABS( C ) )
         A = A / TEMP
         B = B / TEMP
         C = C / TEMP
         IF( C.EQ.ZERO ) THEN
            ETA = B / A
         ELSE IF( A.LE.ZERO ) THEN
            ETA = ( A-SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
         ELSE
            ETA = TWO*B / ( A+SQRT( ABS( A*A-FOUR*B*C ) ) )
         END IF
         IF( F*ETA.GE.ZERO ) THEN
            ETA = -F / DF
         END IF
         TAU = TAU + ETA
         IF( TAU .LT. LBD .OR. TAU .GT. UBD )
     $      TAU = ( LBD + UBD )/TWO
         FC = ZERO
         ERRETM = ZERO
         DF = ZERO
         DDF = ZERO
         DO 40 I = 1, 3
            IF ( ( DSCALE( I )-TAU ).NE.ZERO ) THEN
               TEMP = ONE / ( DSCALE( I )-TAU )
               TEMP1 = ZSCALE( I )*TEMP
               TEMP2 = TEMP1*TEMP
               TEMP3 = TEMP2*TEMP
               TEMP4 = TEMP1 / DSCALE( I )
               FC = FC + TEMP4
               ERRETM = ERRETM + ABS( TEMP4 )
               DF = DF + TEMP2
               DDF = DDF + TEMP3
            ELSE
               GO TO 60
            END IF
   40    CONTINUE
         F = FINIT + TAU*FC
         ERRETM = EIGHT*( ABS( FINIT )+ABS( TAU )*ERRETM ) +
     $            ABS( TAU )*DF
         IF( ( ABS( F ).LE.FOUR*EPS*ERRETM ) .OR.
     $      ( (UBD-LBD).LE.FOUR*EPS*ABS(TAU) )  )
     $      GO TO 60
         IF( F .LE. ZERO )THEN
            LBD = TAU
         ELSE
            UBD = TAU
         END IF
   50 CONTINUE
      INFO = 1
   60 CONTINUE
      IF( SCALE )
     $   TAU = TAU*SCLINV
      RETURN
      END
      SUBROUTINE DGEQRF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
      INTEGER            INFO, LDA, LWORK, M, N
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
      LOGICAL            LQUERY
      INTEGER            I, IB, IINFO, IWS, K, LDWORK, LWKOPT, NB,
     $                   NBMIN, NX
      EXTERNAL           DGEQR2, DLARFB, DLARFT, XERBLA
      INTRINSIC          MAX, MIN
      INTEGER            ILAENV
      EXTERNAL           ILAENV
      INFO = 0
      NB = ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
      LWKOPT = N*NB
      WORK( 1 ) = LWKOPT
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      ELSE IF( LWORK.LT.MAX( 1, N ) .AND. .NOT.LQUERY ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGEQRF', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
      K = MIN( M, N )
      IF( K.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
      NBMIN = 2
      NX = 0
      IWS = N
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
         NX = MAX( 0, ILAENV( 3, 'DGEQRF', ' ', M, N, -1, -1 ) )
         IF( NX.LT.K ) THEN
            LDWORK = N
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
               NB = LWORK / LDWORK
               NBMIN = MAX( 2, ILAENV( 2, 'DGEQRF', ' ', M, N, -1,
     $                 -1 ) )
            END IF
         END IF
      END IF
      IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN
         DO 10 I = 1, K - NX, NB
            IB = MIN( K-I+1, NB )
            CALL DGEQR2( M-I+1, IB, A( I, I ), LDA, TAU( I ), WORK,
     $                   IINFO )
            IF( I+IB.LE.N ) THEN
               CALL DLARFT( 'Forward', 'Columnwise', M-I+1, IB,
     $                      A( I, I ), LDA, TAU( I ), WORK, LDWORK )
               CALL DLARFB( 'Left', 'Transpose', 'Forward',
     $                      'Columnwise', M-I+1, N-I-IB+1, IB,
     $                      A( I, I ), LDA, WORK, LDWORK, A( I, I+IB ),
     $                      LDA, WORK( IB+1 ), LDWORK )
            END IF
   10    CONTINUE
      ELSE
         I = 1
      END IF
      IF( I.LE.K )
     $   CALL DGEQR2( M-I+1, N-I+1, A( I, I ), LDA, TAU( I ), WORK,
     $                IINFO )
      WORK( 1 ) = IWS
      RETURN
      END
      SUBROUTINE DGEQR2( M, N, A, LDA, TAU, WORK, INFO )
      INTEGER            INFO, LDA, M, N
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
      INTEGER            I, K
      DOUBLE PRECISION   AII
      EXTERNAL           DLARF, DLARFG, XERBLA
      INTRINSIC          MAX, MIN
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGEQR2', -INFO )
         RETURN
      END IF
      K = MIN( M, N )
      DO 10 I = 1, K
         CALL DLARFG( M-I+1, A( I, I ), A( MIN( I+1, M ), I ), 1,
     $                TAU( I ) )
         IF( I.LT.N ) THEN
            AII = A( I, I )
            A( I, I ) = ONE
            CALL DLARF( 'Left', M-I+1, N-I, A( I, I ), 1, TAU( I ),
     $                  A( I, I+1 ), LDA, WORK )
            A( I, I ) = AII
         END IF
   10 CONTINUE
      RETURN
      END
      SUBROUTINE DGESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
      INTEGER            INFO, LDA, LDB, N, NRHS
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
      EXTERNAL           DGETRF, DGETRS, XERBLA
      INTRINSIC          MAX
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGESV ', -INFO )
         RETURN
      END IF
      CALL DGETRF( N, N, A, LDA, IPIV, INFO )
      IF( INFO.EQ.0 ) THEN
         CALL DGETRS( 'No transpose', N, NRHS, A, LDA, IPIV, B, LDB,
     $                INFO )
      END IF
      RETURN
      END
      SUBROUTINE DSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,
     $                  LWORK, INFO )
      CHARACTER          UPLO
      INTEGER            INFO, LDA, LDB, LWORK, N, NRHS
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), WORK( * )
      LOGICAL            LQUERY
      INTEGER            LWKOPT
      LOGICAL            LSAME
      EXTERNAL           LSAME
      EXTERNAL           XERBLA, DSYTRF, DSYTRS, DSYTRS2
      INTRINSIC          MAX
      INFO = 0
      LQUERY = ( LWORK.EQ.-1 )
      IF( .NOT.LSAME( UPLO, 'U' ) .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -8
      ELSE IF( LWORK.LT.1 .AND. .NOT.LQUERY ) THEN
         INFO = -10
      END IF
      IF( INFO.EQ.0 ) THEN
         IF( N.EQ.0 ) THEN
            LWKOPT = 1
         ELSE
            CALL DSYTRF( UPLO, N, A, LDA, IPIV, WORK, -1, INFO )
            LWKOPT = WORK(1)
         END IF
         WORK( 1 ) = LWKOPT
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSYSV ', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
      CALL DSYTRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )
      IF( INFO.EQ.0 ) THEN
         IF ( LWORK.LT.N ) THEN
            CALL DSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
         ELSE
            CALL DSYTRS2( UPLO,N,NRHS,A,LDA,IPIV,B,LDB,WORK,INFO )
         END IF
      END IF
      WORK( 1 ) = LWKOPT
      RETURN
      END
      SUBROUTINE DSYTRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )
      CHARACTER          UPLO
      INTEGER            INFO, LDA, LWORK, N
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * ), WORK( * )
      LOGICAL            LQUERY, UPPER
      INTEGER            IINFO, IWS, J, K, KB, LDWORK, LWKOPT, NB, NBMIN
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
      EXTERNAL           DLASYF, DSYTF2, XERBLA
      INTRINSIC          MAX
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      LQUERY = ( LWORK.EQ.-1 )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      ELSE IF( LWORK.LT.1 .AND. .NOT.LQUERY ) THEN
         INFO = -7
      END IF
      IF( INFO.EQ.0 ) THEN
         NB = ILAENV( 1, 'DSYTRF', UPLO, N, -1, -1, -1 )
         LWKOPT = N*NB
         WORK( 1 ) = LWKOPT
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSYTRF', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
      NBMIN = 2
      LDWORK = N
      IF( NB.GT.1 .AND. NB.LT.N ) THEN
         IWS = LDWORK*NB
         IF( LWORK.LT.IWS ) THEN
            NB = MAX( LWORK / LDWORK, 1 )
            NBMIN = MAX( 2, ILAENV( 2, 'DSYTRF', UPLO, N, -1, -1, -1 ) )
         END IF
      ELSE
         IWS = 1
      END IF
      IF( NB.LT.NBMIN )
     $   NB = N
      IF( UPPER ) THEN
         K = N
   10    CONTINUE
         IF( K.LT.1 )
     $      GO TO 40
         IF( K.GT.NB ) THEN
            CALL DLASYF( UPLO, K, NB, KB, A, LDA, IPIV, WORK, LDWORK,
     $                   IINFO )
         ELSE
            CALL DSYTF2( UPLO, K, A, LDA, IPIV, IINFO )
            KB = K
         END IF
         IF( INFO.EQ.0 .AND. IINFO.GT.0 )
     $      INFO = IINFO
         K = K - KB
         GO TO 10
      ELSE
         K = 1
   20    CONTINUE
         IF( K.GT.N )
     $      GO TO 40
         IF( K.LE.N-NB ) THEN
            CALL DLASYF( UPLO, N-K+1, NB, KB, A( K, K ), LDA, IPIV( K ),
     $                   WORK, LDWORK, IINFO )
         ELSE
            CALL DSYTF2( UPLO, N-K+1, A( K, K ), LDA, IPIV( K ), IINFO )
            KB = N - K + 1
         END IF
         IF( INFO.EQ.0 .AND. IINFO.GT.0 )
     $      INFO = IINFO + K - 1
         DO 30 J = K, K + KB - 1
            IF( IPIV( J ).GT.0 ) THEN
               IPIV( J ) = IPIV( J ) + K - 1
            ELSE
               IPIV( J ) = IPIV( J ) - K + 1
            END IF
   30    CONTINUE
         K = K + KB
         GO TO 20
      END IF
   40 CONTINUE
      WORK( 1 ) = LWKOPT
      RETURN
      END
      SUBROUTINE DSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
      CHARACTER          UPLO
      INTEGER            INFO, LDA, LDB, N, NRHS
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
      LOGICAL            UPPER
      INTEGER            J, K, KP
      DOUBLE PRECISION   AK, AKM1, AKM1K, BK, BKM1, DENOM
      LOGICAL            LSAME
      EXTERNAL           LSAME
      EXTERNAL           DGEMV, DGER, DSCAL, DSWAP, XERBLA
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
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSYTRS', -INFO )
         RETURN
      END IF
      IF( N.EQ.0 .OR. NRHS.EQ.0 )
     $   RETURN
      IF( UPPER ) THEN
         K = N
   10    CONTINUE
         IF( K.LT.1 )
     $      GO TO 30
         IF( IPIV( K ).GT.0 ) THEN
            KP = IPIV( K )
            IF( KP.NE.K )
     $         CALL DSWAP( NRHS, B( K, 1 ), LDB, B( KP, 1 ), LDB )
            CALL DGER( K-1, NRHS, -ONE, A( 1, K ), 1, B( K, 1 ), LDB,
     $                 B( 1, 1 ), LDB )
            CALL DSCAL( NRHS, ONE / A( K, K ), B( K, 1 ), LDB )
            K = K - 1
         ELSE
            KP = -IPIV( K )
            IF( KP.NE.K-1 )
     $         CALL DSWAP( NRHS, B( K-1, 1 ), LDB, B( KP, 1 ), LDB )
            CALL DGER( K-2, NRHS, -ONE, A( 1, K ), 1, B( K, 1 ), LDB,
     $                 B( 1, 1 ), LDB )
            CALL DGER( K-2, NRHS, -ONE, A( 1, K-1 ), 1, B( K-1, 1 ),
     $                 LDB, B( 1, 1 ), LDB )
            AKM1K = A( K-1, K )
            AKM1 = A( K-1, K-1 ) / AKM1K
            AK = A( K, K ) / AKM1K
            DENOM = AKM1*AK - ONE
            DO 20 J = 1, NRHS
               BKM1 = B( K-1, J ) / AKM1K
               BK = B( K, J ) / AKM1K
               B( K-1, J ) = ( AK*BKM1-BK ) / DENOM
               B( K, J ) = ( AKM1*BK-BKM1 ) / DENOM
   20       CONTINUE
            K = K - 2
         END IF
         GO TO 10
   30    CONTINUE
         K = 1
   40    CONTINUE
         IF( K.GT.N )
     $      GO TO 50
         IF( IPIV( K ).GT.0 ) THEN
            CALL DGEMV( 'Transpose', K-1, NRHS, -ONE, B, LDB, A( 1, K ),
     $                  1, ONE, B( K, 1 ), LDB )
            KP = IPIV( K )
            IF( KP.NE.K )
     $         CALL DSWAP( NRHS, B( K, 1 ), LDB, B( KP, 1 ), LDB )
            K = K + 1
         ELSE
            CALL DGEMV( 'Transpose', K-1, NRHS, -ONE, B, LDB, A( 1, K ),
     $                  1, ONE, B( K, 1 ), LDB )
            CALL DGEMV( 'Transpose', K-1, NRHS, -ONE, B, LDB,
     $                  A( 1, K+1 ), 1, ONE, B( K+1, 1 ), LDB )
            KP = -IPIV( K )
            IF( KP.NE.K )
     $         CALL DSWAP( NRHS, B( K, 1 ), LDB, B( KP, 1 ), LDB )
            K = K + 2
         END IF
         GO TO 40
   50    CONTINUE
      ELSE
         K = 1
   60    CONTINUE
         IF( K.GT.N )
     $      GO TO 80
         IF( IPIV( K ).GT.0 ) THEN
            KP = IPIV( K )
            IF( KP.NE.K )
     $         CALL DSWAP( NRHS, B( K, 1 ), LDB, B( KP, 1 ), LDB )
            IF( K.LT.N )
     $         CALL DGER( N-K, NRHS, -ONE, A( K+1, K ), 1, B( K, 1 ),
     $                    LDB, B( K+1, 1 ), LDB )
            CALL DSCAL( NRHS, ONE / A( K, K ), B( K, 1 ), LDB )
            K = K + 1
         ELSE
            KP = -IPIV( K )
            IF( KP.NE.K+1 )
     $         CALL DSWAP( NRHS, B( K+1, 1 ), LDB, B( KP, 1 ), LDB )
            IF( K.LT.N-1 ) THEN
               CALL DGER( N-K-1, NRHS, -ONE, A( K+2, K ), 1, B( K, 1 ),
     $                    LDB, B( K+2, 1 ), LDB )
               CALL DGER( N-K-1, NRHS, -ONE, A( K+2, K+1 ), 1,
     $                    B( K+1, 1 ), LDB, B( K+2, 1 ), LDB )
            END IF
            AKM1K = A( K+1, K )
            AKM1 = A( K, K ) / AKM1K
            AK = A( K+1, K+1 ) / AKM1K
            DENOM = AKM1*AK - ONE
            DO 70 J = 1, NRHS
               BKM1 = B( K, J ) / AKM1K
               BK = B( K+1, J ) / AKM1K
               B( K, J ) = ( AK*BKM1-BK ) / DENOM
               B( K+1, J ) = ( AKM1*BK-BKM1 ) / DENOM
   70       CONTINUE
            K = K + 2
         END IF
         GO TO 60
   80    CONTINUE
         K = N
   90    CONTINUE
         IF( K.LT.1 )
     $      GO TO 100
         IF( IPIV( K ).GT.0 ) THEN
            IF( K.LT.N )
     $         CALL DGEMV( 'Transpose', N-K, NRHS, -ONE, B( K+1, 1 ),
     $                     LDB, A( K+1, K ), 1, ONE, B( K, 1 ), LDB )
            KP = IPIV( K )
            IF( KP.NE.K )
     $         CALL DSWAP( NRHS, B( K, 1 ), LDB, B( KP, 1 ), LDB )
            K = K - 1
         ELSE
            IF( K.LT.N ) THEN
               CALL DGEMV( 'Transpose', N-K, NRHS, -ONE, B( K+1, 1 ),
     $                     LDB, A( K+1, K ), 1, ONE, B( K, 1 ), LDB )
               CALL DGEMV( 'Transpose', N-K, NRHS, -ONE, B( K+1, 1 ),
     $                     LDB, A( K+1, K-1 ), 1, ONE, B( K-1, 1 ),
     $                     LDB )
            END IF
            KP = -IPIV( K )
            IF( KP.NE.K )
     $         CALL DSWAP( NRHS, B( K, 1 ), LDB, B( KP, 1 ), LDB )
            K = K - 2
         END IF
         GO TO 90
  100    CONTINUE
      END IF
      RETURN
      END
      SUBROUTINE DSYTRS2( UPLO, N, NRHS, A, LDA, IPIV, B, LDB,
     $                    WORK, INFO )
      CHARACTER          UPLO
      INTEGER            INFO, LDA, LDB, N, NRHS
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), WORK( * )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
      LOGICAL            UPPER
      INTEGER            I, IINFO, J, K, KP
      DOUBLE PRECISION   AK, AKM1, AKM1K, BK, BKM1, DENOM
      LOGICAL            LSAME
      EXTERNAL           LSAME
      EXTERNAL           DSCAL, DSYCONV, DSWAP, DTRSM, XERBLA
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
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSYTRS2', -INFO )
         RETURN
      END IF
      IF( N.EQ.0 .OR. NRHS.EQ.0 )
     $   RETURN
      CALL DSYCONV( UPLO, 'C', N, A, LDA, IPIV, WORK, IINFO )
      IF( UPPER ) THEN
        K=N
        DO WHILE ( K .GE. 1 )
         IF( IPIV( K ).GT.0 ) THEN
            KP = IPIV( K )
            IF( KP.NE.K )
     $         CALL DSWAP( NRHS, B( K, 1 ), LDB, B( KP, 1 ), LDB )
            K=K-1
         ELSE
            KP = -IPIV( K )
            IF( KP.EQ.-IPIV( K-1 ) )
     $         CALL DSWAP( NRHS, B( K-1, 1 ), LDB, B( KP, 1 ), LDB )
            K=K-2
         END IF
        END DO
        CALL DTRSM('L','U','N','U',N,NRHS,ONE,A,LDA,B,LDB)
         I=N
         DO WHILE ( I .GE. 1 )
            IF( IPIV(I) .GT. 0 ) THEN
              CALL DSCAL( NRHS, ONE / A( I, I ), B( I, 1 ), LDB )
            ELSEIF ( I .GT. 1) THEN
               IF ( IPIV(I-1) .EQ. IPIV(I) ) THEN
                  AKM1K = WORK(I)
                  AKM1 = A( I-1, I-1 ) / AKM1K
                  AK = A( I, I ) / AKM1K
                  DENOM = AKM1*AK - ONE
                  DO 15 J = 1, NRHS
                     BKM1 = B( I-1, J ) / AKM1K
                     BK = B( I, J ) / AKM1K
                     B( I-1, J ) = ( AK*BKM1-BK ) / DENOM
                     B( I, J ) = ( AKM1*BK-BKM1 ) / DENOM
 15              CONTINUE
               I = I - 1
               ENDIF
            ENDIF
            I = I - 1
         END DO
         CALL DTRSM('L','U','T','U',N,NRHS,ONE,A,LDA,B,LDB)
        K=1
        DO WHILE ( K .LE. N )
         IF( IPIV( K ).GT.0 ) THEN
            KP = IPIV( K )
            IF( KP.NE.K )
     $         CALL DSWAP( NRHS, B( K, 1 ), LDB, B( KP, 1 ), LDB )
            K=K+1
         ELSE
            KP = -IPIV( K )
            IF( K .LT. N .AND. KP.EQ.-IPIV( K+1 ) )
     $         CALL DSWAP( NRHS, B( K, 1 ), LDB, B( KP, 1 ), LDB )
            K=K+2
         ENDIF
        END DO
      ELSE
        K=1
        DO WHILE ( K .LE. N )
         IF( IPIV( K ).GT.0 ) THEN
            KP = IPIV( K )
            IF( KP.NE.K )
     $         CALL DSWAP( NRHS, B( K, 1 ), LDB, B( KP, 1 ), LDB )
            K=K+1
         ELSE
            KP = -IPIV( K+1 )
            IF( KP.EQ.-IPIV( K ) )
     $         CALL DSWAP( NRHS, B( K+1, 1 ), LDB, B( KP, 1 ), LDB )
            K=K+2
         ENDIF
        END DO
        CALL DTRSM('L','L','N','U',N,NRHS,ONE,A,LDA,B,LDB)
         I=1
         DO WHILE ( I .LE. N )
            IF( IPIV(I) .GT. 0 ) THEN
              CALL DSCAL( NRHS, ONE / A( I, I ), B( I, 1 ), LDB )
            ELSE
                  AKM1K = WORK(I)
                  AKM1 = A( I, I ) / AKM1K
                  AK = A( I+1, I+1 ) / AKM1K
                  DENOM = AKM1*AK - ONE
                  DO 25 J = 1, NRHS
                     BKM1 = B( I, J ) / AKM1K
                     BK = B( I+1, J ) / AKM1K
                     B( I, J ) = ( AK*BKM1-BK ) / DENOM
                     B( I+1, J ) = ( AKM1*BK-BKM1 ) / DENOM
 25              CONTINUE
                  I = I + 1
            ENDIF
            I = I + 1
         END DO
        CALL DTRSM('L','L','T','U',N,NRHS,ONE,A,LDA,B,LDB)
        K=N
        DO WHILE ( K .GE. 1 )
         IF( IPIV( K ).GT.0 ) THEN
            KP = IPIV( K )
            IF( KP.NE.K )
     $         CALL DSWAP( NRHS, B( K, 1 ), LDB, B( KP, 1 ), LDB )
            K=K-1
         ELSE
            KP = -IPIV( K )
            IF( K.GT.1 .AND. KP.EQ.-IPIV( K-1 ) )
     $         CALL DSWAP( NRHS, B( K, 1 ), LDB, B( KP, 1 ), LDB )
            K=K-2
         ENDIF
        END DO
      END IF
      CALL DSYCONV( UPLO, 'R', N, A, LDA, IPIV, WORK, IINFO )
      RETURN
      END
      SUBROUTINE DLASYF( UPLO, N, NB, KB, A, LDA, IPIV, W, LDW, INFO )
      CHARACTER          UPLO
      INTEGER            INFO, KB, LDA, LDW, N, NB
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * ), W( LDW, * )
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      DOUBLE PRECISION   EIGHT, SEVTEN
      PARAMETER          ( EIGHT = 8.0D+0, SEVTEN = 17.0D+0 )
      INTEGER            IMAX, J, JB, JJ, JMAX, JP, K, KK, KKW, KP,
     $                   KSTEP, KW
      DOUBLE PRECISION   ABSAKK, ALPHA, COLMAX, D11, D21, D22, R1,
     $                   ROWMAX, T
      LOGICAL            LSAME
      INTEGER            IDAMAX
      EXTERNAL           LSAME, IDAMAX
      EXTERNAL           DCOPY, DGEMM, DGEMV, DSCAL, DSWAP
      INTRINSIC          ABS, MAX, MIN, SQRT
      INFO = 0
      ALPHA = ( ONE+SQRT( SEVTEN ) ) / EIGHT
      IF( LSAME( UPLO, 'U' ) ) THEN
         K = N
   10    CONTINUE
         KW = NB + K - N
         IF( ( K.LE.N-NB+1 .AND. NB.LT.N ) .OR. K.LT.1 )
     $      GO TO 30
         CALL DCOPY( K, A( 1, K ), 1, W( 1, KW ), 1 )
         IF( K.LT.N )
     $      CALL DGEMV( 'No transpose', K, N-K, -ONE, A( 1, K+1 ), LDA,
     $                  W( K, KW+1 ), LDW, ONE, W( 1, KW ), 1 )
         KSTEP = 1
         ABSAKK = ABS( W( K, KW ) )
         IF( K.GT.1 ) THEN
            IMAX = IDAMAX( K-1, W( 1, KW ), 1 )
            COLMAX = ABS( W( IMAX, KW ) )
         ELSE
            COLMAX = ZERO
         END IF
         IF( MAX( ABSAKK, COLMAX ).EQ.ZERO ) THEN
            IF( INFO.EQ.0 )
     $         INFO = K
            KP = K
         ELSE
            IF( ABSAKK.GE.ALPHA*COLMAX ) THEN
               KP = K
            ELSE
               CALL DCOPY( IMAX, A( 1, IMAX ), 1, W( 1, KW-1 ), 1 )
               CALL DCOPY( K-IMAX, A( IMAX, IMAX+1 ), LDA,
     $                     W( IMAX+1, KW-1 ), 1 )
               IF( K.LT.N )
     $            CALL DGEMV( 'No transpose', K, N-K, -ONE, A( 1, K+1 ),
     $                        LDA, W( IMAX, KW+1 ), LDW, ONE,
     $                        W( 1, KW-1 ), 1 )
               JMAX = IMAX + IDAMAX( K-IMAX, W( IMAX+1, KW-1 ), 1 )
               ROWMAX = ABS( W( JMAX, KW-1 ) )
               IF( IMAX.GT.1 ) THEN
                  JMAX = IDAMAX( IMAX-1, W( 1, KW-1 ), 1 )
                  ROWMAX = MAX( ROWMAX, ABS( W( JMAX, KW-1 ) ) )
               END IF
               IF( ABSAKK.GE.ALPHA*COLMAX*( COLMAX / ROWMAX ) ) THEN
                  KP = K
               ELSE IF( ABS( W( IMAX, KW-1 ) ).GE.ALPHA*ROWMAX ) THEN
                  KP = IMAX
                  CALL DCOPY( K, W( 1, KW-1 ), 1, W( 1, KW ), 1 )
               ELSE
                  KP = IMAX
                  KSTEP = 2
               END IF
            END IF
            KK = K - KSTEP + 1
            KKW = NB + KK - N
            IF( KP.NE.KK ) THEN
               A( KP, KP ) = A( KK, KK )
               CALL DCOPY( KK-1-KP, A( KP+1, KK ), 1, A( KP, KP+1 ),
     $                     LDA )
               IF( KP.GT.1 )
     $            CALL DCOPY( KP-1, A( 1, KK ), 1, A( 1, KP ), 1 )
               IF( K.LT.N )
     $            CALL DSWAP( N-K, A( KK, K+1 ), LDA, A( KP, K+1 ),
     $                        LDA )
               CALL DSWAP( N-KK+1, W( KK, KKW ), LDW, W( KP, KKW ),
     $                     LDW )
            END IF
            IF( KSTEP.EQ.1 ) THEN
               CALL DCOPY( K, W( 1, KW ), 1, A( 1, K ), 1 )
               R1 = ONE / A( K, K )
               CALL DSCAL( K-1, R1, A( 1, K ), 1 )
            ELSE
               IF( K.GT.2 ) THEN
                  D21 = W( K-1, KW )
                  D11 = W( K, KW ) / D21
                  D22 = W( K-1, KW-1 ) / D21
                  T = ONE / ( D11*D22-ONE )
                  D21 = T / D21
                  DO 20 J = 1, K - 2
                     A( J, K-1 ) = D21*( D11*W( J, KW-1 )-W( J, KW ) )
                     A( J, K ) = D21*( D22*W( J, KW )-W( J, KW-1 ) )
   20             CONTINUE
               END IF
               A( K-1, K-1 ) = W( K-1, KW-1 )
               A( K-1, K ) = W( K-1, KW )
               A( K, K ) = W( K, KW )
            END IF
         END IF
         IF( KSTEP.EQ.1 ) THEN
            IPIV( K ) = KP
         ELSE
            IPIV( K ) = -KP
            IPIV( K-1 ) = -KP
         END IF
         K = K - KSTEP
         GO TO 10
   30    CONTINUE
         DO 50 J = ( ( K-1 ) / NB )*NB + 1, 1, -NB
            JB = MIN( NB, K-J+1 )
            DO 40 JJ = J, J + JB - 1
               CALL DGEMV( 'No transpose', JJ-J+1, N-K, -ONE,
     $                     A( J, K+1 ), LDA, W( JJ, KW+1 ), LDW, ONE,
     $                     A( J, JJ ), 1 )
   40       CONTINUE
            CALL DGEMM( 'No transpose', 'Transpose', J-1, JB, N-K, -ONE,
     $                  A( 1, K+1 ), LDA, W( J, KW+1 ), LDW, ONE,
     $                  A( 1, J ), LDA )
   50    CONTINUE
         J = K + 1
   60    CONTINUE
            JJ = J
            JP = IPIV( J )
            IF( JP.LT.0 ) THEN
               JP = -JP
               J = J + 1
            END IF
            J = J + 1
            IF( JP.NE.JJ .AND. J.LE.N )
     $         CALL DSWAP( N-J+1, A( JP, J ), LDA, A( JJ, J ), LDA )
         IF( J.LT.N )
     $      GO TO 60
         KB = N - K
      ELSE
         K = 1
   70    CONTINUE
         IF( ( K.GE.NB .AND. NB.LT.N ) .OR. K.GT.N )
     $      GO TO 90
         CALL DCOPY( N-K+1, A( K, K ), 1, W( K, K ), 1 )
         CALL DGEMV( 'No transpose', N-K+1, K-1, -ONE, A( K, 1 ), LDA,
     $               W( K, 1 ), LDW, ONE, W( K, K ), 1 )
         KSTEP = 1
         ABSAKK = ABS( W( K, K ) )
         IF( K.LT.N ) THEN
            IMAX = K + IDAMAX( N-K, W( K+1, K ), 1 )
            COLMAX = ABS( W( IMAX, K ) )
         ELSE
            COLMAX = ZERO
         END IF
         IF( MAX( ABSAKK, COLMAX ).EQ.ZERO ) THEN
            IF( INFO.EQ.0 )
     $         INFO = K
            KP = K
         ELSE
            IF( ABSAKK.GE.ALPHA*COLMAX ) THEN
               KP = K
            ELSE
               CALL DCOPY( IMAX-K, A( IMAX, K ), LDA, W( K, K+1 ), 1 )
               CALL DCOPY( N-IMAX+1, A( IMAX, IMAX ), 1, W( IMAX, K+1 ),
     $                     1 )
               CALL DGEMV( 'No transpose', N-K+1, K-1, -ONE, A( K, 1 ),
     $                     LDA, W( IMAX, 1 ), LDW, ONE, W( K, K+1 ), 1 )
               JMAX = K - 1 + IDAMAX( IMAX-K, W( K, K+1 ), 1 )
               ROWMAX = ABS( W( JMAX, K+1 ) )
               IF( IMAX.LT.N ) THEN
                  JMAX = IMAX + IDAMAX( N-IMAX, W( IMAX+1, K+1 ), 1 )
                  ROWMAX = MAX( ROWMAX, ABS( W( JMAX, K+1 ) ) )
               END IF
               IF( ABSAKK.GE.ALPHA*COLMAX*( COLMAX / ROWMAX ) ) THEN
                  KP = K
               ELSE IF( ABS( W( IMAX, K+1 ) ).GE.ALPHA*ROWMAX ) THEN
                  KP = IMAX
                  CALL DCOPY( N-K+1, W( K, K+1 ), 1, W( K, K ), 1 )
               ELSE
                  KP = IMAX
                  KSTEP = 2
               END IF
            END IF
            KK = K + KSTEP - 1
            IF( KP.NE.KK ) THEN
               A( KP, KP ) = A( KK, KK )
               CALL DCOPY( KP-KK-1, A( KK+1, KK ), 1, A( KP, KK+1 ),
     $                     LDA )
               IF( KP.LT.N )
     $            CALL DCOPY( N-KP, A( KP+1, KK ), 1, A( KP+1, KP ), 1 )
               IF( K.GT.1 )
     $            CALL DSWAP( K-1, A( KK, 1 ), LDA, A( KP, 1 ), LDA )
               CALL DSWAP( KK, W( KK, 1 ), LDW, W( KP, 1 ), LDW )
            END IF
            IF( KSTEP.EQ.1 ) THEN
               CALL DCOPY( N-K+1, W( K, K ), 1, A( K, K ), 1 )
               IF( K.LT.N ) THEN
                  R1 = ONE / A( K, K )
                  CALL DSCAL( N-K, R1, A( K+1, K ), 1 )
               END IF
            ELSE
               IF( K.LT.N-1 ) THEN
                  D21 = W( K+1, K )
                  D11 = W( K+1, K+1 ) / D21
                  D22 = W( K, K ) / D21
                  T = ONE / ( D11*D22-ONE )
                  D21 = T / D21
                  DO 80 J = K + 2, N
                     A( J, K ) = D21*( D11*W( J, K )-W( J, K+1 ) )
                     A( J, K+1 ) = D21*( D22*W( J, K+1 )-W( J, K ) )
   80             CONTINUE
               END IF
               A( K, K ) = W( K, K )
               A( K+1, K ) = W( K+1, K )
               A( K+1, K+1 ) = W( K+1, K+1 )
            END IF
         END IF
         IF( KSTEP.EQ.1 ) THEN
            IPIV( K ) = KP
         ELSE
            IPIV( K ) = -KP
            IPIV( K+1 ) = -KP
         END IF
         K = K + KSTEP
         GO TO 70
   90    CONTINUE
         DO 110 J = K, N, NB
            JB = MIN( NB, N-J+1 )
            DO 100 JJ = J, J + JB - 1
               CALL DGEMV( 'No transpose', J+JB-JJ, K-1, -ONE,
     $                     A( JJ, 1 ), LDA, W( JJ, 1 ), LDW, ONE,
     $                     A( JJ, JJ ), 1 )
  100       CONTINUE
            IF( J+JB.LE.N )
     $         CALL DGEMM( 'No transpose', 'Transpose', N-J-JB+1, JB,
     $                     K-1, -ONE, A( J+JB, 1 ), LDA, W( J, 1 ), LDW,
     $                     ONE, A( J+JB, J ), LDA )
  110    CONTINUE
         J = K - 1
  120    CONTINUE
            JJ = J
            JP = IPIV( J )
            IF( JP.LT.0 ) THEN
               JP = -JP
               J = J - 1
            END IF
            J = J - 1
            IF( JP.NE.JJ .AND. J.GE.1 )
     $         CALL DSWAP( J, A( JP, 1 ), LDA, A( JJ, 1 ), LDA )
         IF( J.GT.1 )
     $      GO TO 120
         KB = K - 1
      END IF
      RETURN
      END
      SUBROUTINE DSYTF2( UPLO, N, A, LDA, IPIV, INFO )
      CHARACTER          UPLO
      INTEGER            INFO, LDA, N
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * )
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      DOUBLE PRECISION   EIGHT, SEVTEN
      PARAMETER          ( EIGHT = 8.0D+0, SEVTEN = 17.0D+0 )
      LOGICAL            UPPER
      INTEGER            I, IMAX, J, JMAX, K, KK, KP, KSTEP
      DOUBLE PRECISION   ABSAKK, ALPHA, COLMAX, D11, D12, D21, D22, R1,
     $                   ROWMAX, T, WK, WKM1, WKP1
      LOGICAL            LSAME, DISNAN
      INTEGER            IDAMAX
      EXTERNAL           LSAME, IDAMAX, DISNAN
      EXTERNAL           DSCAL, DSWAP, DSYR, XERBLA
      INTRINSIC          ABS, MAX, SQRT
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
         CALL XERBLA( 'DSYTF2', -INFO )
         RETURN
      END IF
      ALPHA = ( ONE+SQRT( SEVTEN ) ) / EIGHT
      IF( UPPER ) THEN
         K = N
   10    CONTINUE
         IF( K.LT.1 )
     $      GO TO 70
         KSTEP = 1
         ABSAKK = ABS( A( K, K ) )
         IF( K.GT.1 ) THEN
            IMAX = IDAMAX( K-1, A( 1, K ), 1 )
            COLMAX = ABS( A( IMAX, K ) )
         ELSE
            COLMAX = ZERO
         END IF
         IF( (MAX( ABSAKK, COLMAX ).EQ.ZERO) .OR. DISNAN(ABSAKK) ) THEN
            IF( INFO.EQ.0 )
     $         INFO = K
            KP = K
         ELSE
            IF( ABSAKK.GE.ALPHA*COLMAX ) THEN
               KP = K
            ELSE
               JMAX = IMAX + IDAMAX( K-IMAX, A( IMAX, IMAX+1 ), LDA )
               ROWMAX = ABS( A( IMAX, JMAX ) )
               IF( IMAX.GT.1 ) THEN
                  JMAX = IDAMAX( IMAX-1, A( 1, IMAX ), 1 )
                  ROWMAX = MAX( ROWMAX, ABS( A( JMAX, IMAX ) ) )
               END IF
               IF( ABSAKK.GE.ALPHA*COLMAX*( COLMAX / ROWMAX ) ) THEN
                  KP = K
               ELSE IF( ABS( A( IMAX, IMAX ) ).GE.ALPHA*ROWMAX ) THEN
                  KP = IMAX
               ELSE
                  KP = IMAX
                  KSTEP = 2
               END IF
            END IF
            KK = K - KSTEP + 1
            IF( KP.NE.KK ) THEN
               CALL DSWAP( KP-1, A( 1, KK ), 1, A( 1, KP ), 1 )
               CALL DSWAP( KK-KP-1, A( KP+1, KK ), 1, A( KP, KP+1 ),
     $                     LDA )
               T = A( KK, KK )
               A( KK, KK ) = A( KP, KP )
               A( KP, KP ) = T
               IF( KSTEP.EQ.2 ) THEN
                  T = A( K-1, K )
                  A( K-1, K ) = A( KP, K )
                  A( KP, K ) = T
               END IF
            END IF
            IF( KSTEP.EQ.1 ) THEN
               R1 = ONE / A( K, K )
               CALL DSYR( UPLO, K-1, -R1, A( 1, K ), 1, A, LDA )
               CALL DSCAL( K-1, R1, A( 1, K ), 1 )
            ELSE
               IF( K.GT.2 ) THEN
                  D12 = A( K-1, K )
                  D22 = A( K-1, K-1 ) / D12
                  D11 = A( K, K ) / D12
                  T = ONE / ( D11*D22-ONE )
                  D12 = T / D12
                  DO 30 J = K - 2, 1, -1
                     WKM1 = D12*( D11*A( J, K-1 )-A( J, K ) )
                     WK = D12*( D22*A( J, K )-A( J, K-1 ) )
                     DO 20 I = J, 1, -1
                        A( I, J ) = A( I, J ) - A( I, K )*WK -
     $                              A( I, K-1 )*WKM1
   20                CONTINUE
                     A( J, K ) = WK
                     A( J, K-1 ) = WKM1
   30             CONTINUE
               END IF
            END IF
         END IF
         IF( KSTEP.EQ.1 ) THEN
            IPIV( K ) = KP
         ELSE
            IPIV( K ) = -KP
            IPIV( K-1 ) = -KP
         END IF
         K = K - KSTEP
         GO TO 10
      ELSE
         K = 1
   40    CONTINUE
         IF( K.GT.N )
     $      GO TO 70
         KSTEP = 1
         ABSAKK = ABS( A( K, K ) )
         IF( K.LT.N ) THEN
            IMAX = K + IDAMAX( N-K, A( K+1, K ), 1 )
            COLMAX = ABS( A( IMAX, K ) )
         ELSE
            COLMAX = ZERO
         END IF
         IF( (MAX( ABSAKK, COLMAX ).EQ.ZERO) .OR. DISNAN(ABSAKK) ) THEN
            IF( INFO.EQ.0 )
     $         INFO = K
            KP = K
         ELSE
            IF( ABSAKK.GE.ALPHA*COLMAX ) THEN
               KP = K
            ELSE
               JMAX = K - 1 + IDAMAX( IMAX-K, A( IMAX, K ), LDA )
               ROWMAX = ABS( A( IMAX, JMAX ) )
               IF( IMAX.LT.N ) THEN
                  JMAX = IMAX + IDAMAX( N-IMAX, A( IMAX+1, IMAX ), 1 )
                  ROWMAX = MAX( ROWMAX, ABS( A( JMAX, IMAX ) ) )
               END IF
               IF( ABSAKK.GE.ALPHA*COLMAX*( COLMAX / ROWMAX ) ) THEN
                  KP = K
               ELSE IF( ABS( A( IMAX, IMAX ) ).GE.ALPHA*ROWMAX ) THEN
                  KP = IMAX
               ELSE
                  KP = IMAX
                  KSTEP = 2
               END IF
            END IF
            KK = K + KSTEP - 1
            IF( KP.NE.KK ) THEN
               IF( KP.LT.N )
     $            CALL DSWAP( N-KP, A( KP+1, KK ), 1, A( KP+1, KP ), 1 )
               CALL DSWAP( KP-KK-1, A( KK+1, KK ), 1, A( KP, KK+1 ),
     $                     LDA )
               T = A( KK, KK )
               A( KK, KK ) = A( KP, KP )
               A( KP, KP ) = T
               IF( KSTEP.EQ.2 ) THEN
                  T = A( K+1, K )
                  A( K+1, K ) = A( KP, K )
                  A( KP, K ) = T
               END IF
            END IF
            IF( KSTEP.EQ.1 ) THEN
               IF( K.LT.N ) THEN
                  D11 = ONE / A( K, K )
                  CALL DSYR( UPLO, N-K, -D11, A( K+1, K ), 1,
     $                       A( K+1, K+1 ), LDA )
                  CALL DSCAL( N-K, D11, A( K+1, K ), 1 )
               END IF
            ELSE
               IF( K.LT.N-1 ) THEN
                  D21 = A( K+1, K )
                  D11 = A( K+1, K+1 ) / D21
                  D22 = A( K, K ) / D21
                  T = ONE / ( D11*D22-ONE )
                  D21 = T / D21
                  DO 60 J = K + 2, N
                     WK = D21*( D11*A( J, K )-A( J, K+1 ) )
                     WKP1 = D21*( D22*A( J, K+1 )-A( J, K ) )
                     DO 50 I = J, N
                        A( I, J ) = A( I, J ) - A( I, K )*WK -
     $                              A( I, K+1 )*WKP1
   50                CONTINUE
                     A( J, K ) = WK
                     A( J, K+1 ) = WKP1
   60             CONTINUE
               END IF
            END IF
         END IF
         IF( KSTEP.EQ.1 ) THEN
            IPIV( K ) = KP
         ELSE
            IPIV( K ) = -KP
            IPIV( K+1 ) = -KP
         END IF
         K = K + KSTEP
         GO TO 40
      END IF
   70 CONTINUE
      RETURN
      END
      SUBROUTINE DSYCONV( UPLO, WAY, N, A, LDA, IPIV, E, INFO )
      CHARACTER          UPLO, WAY
      INTEGER            INFO, LDA, N
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * ), E( * )
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
      LOGICAL            LSAME
      EXTERNAL           LSAME
      EXTERNAL           XERBLA
      LOGICAL            UPPER, CONVERT
      INTEGER            I, IP, J
      DOUBLE PRECISION   TEMP
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      CONVERT = LSAME( WAY, 'C' )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.CONVERT .AND. .NOT.LSAME( WAY, 'R' ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSYCONV', -INFO )
         RETURN
      END IF
      IF( N.EQ.0 )
     $   RETURN
      IF( UPPER ) THEN
         IF ( CONVERT ) THEN
            I=N
            E(1)=ZERO
            DO WHILE ( I .GT. 1 )
               IF( IPIV(I) .LT. 0 ) THEN
                  E(I)=A(I-1,I)
                  E(I-1)=ZERO
                  A(I-1,I)=ZERO
                  I=I-1
               ELSE
                  E(I)=ZERO
               ENDIF
               I=I-1
            END DO
         I=N
         DO WHILE ( I .GE. 1 )
            IF( IPIV(I) .GT. 0) THEN
               IP=IPIV(I)
               IF( I .LT. N) THEN
                  DO 12 J= I+1,N
                    TEMP=A(IP,J)
                    A(IP,J)=A(I,J)
                    A(I,J)=TEMP
 12            CONTINUE
               ENDIF
            ELSE
              IP=-IPIV(I)
               IF( I .LT. N) THEN
             DO 13 J= I+1,N
                 TEMP=A(IP,J)
                 A(IP,J)=A(I-1,J)
                 A(I-1,J)=TEMP
 13            CONTINUE
                ENDIF
                I=I-1
           ENDIF
           I=I-1
        END DO
         ELSE
            I=1
            DO WHILE ( I .LE. N )
               IF( IPIV(I) .GT. 0 ) THEN
                  IP=IPIV(I)
                  IF( I .LT. N) THEN
                  DO J= I+1,N
                    TEMP=A(IP,J)
                    A(IP,J)=A(I,J)
                    A(I,J)=TEMP
                  END DO
                  ENDIF
               ELSE
                 IP=-IPIV(I)
                 I=I+1
                 IF( I .LT. N) THEN
                    DO J= I+1,N
                       TEMP=A(IP,J)
                       A(IP,J)=A(I-1,J)
                       A(I-1,J)=TEMP
                    END DO
                 ENDIF
               ENDIF
               I=I+1
            END DO
            I=N
            DO WHILE ( I .GT. 1 )
               IF( IPIV(I) .LT. 0 ) THEN
                  A(I-1,I)=E(I)
                  I=I-1
               ENDIF
               I=I-1
            END DO
         END IF
      ELSE
         IF ( CONVERT ) THEN
            I=1
            E(N)=ZERO
            DO WHILE ( I .LE. N )
               IF( I.LT.N .AND. IPIV(I) .LT. 0 ) THEN
                  E(I)=A(I+1,I)
                  E(I+1)=ZERO
                  A(I+1,I)=ZERO
                  I=I+1
               ELSE
                  E(I)=ZERO
               ENDIF
               I=I+1
            END DO
         I=1
         DO WHILE ( I .LE. N )
            IF( IPIV(I) .GT. 0 ) THEN
               IP=IPIV(I)
               IF (I .GT. 1) THEN
               DO 22 J= 1,I-1
                 TEMP=A(IP,J)
                 A(IP,J)=A(I,J)
                 A(I,J)=TEMP
 22            CONTINUE
               ENDIF
            ELSE
              IP=-IPIV(I)
              IF (I .GT. 1) THEN
              DO 23 J= 1,I-1
                 TEMP=A(IP,J)
                 A(IP,J)=A(I+1,J)
                 A(I+1,J)=TEMP
 23           CONTINUE
              ENDIF
              I=I+1
           ENDIF
           I=I+1
        END DO
         ELSE
            I=N
            DO WHILE ( I .GE. 1 )
               IF( IPIV(I) .GT. 0 ) THEN
                  IP=IPIV(I)
                  IF (I .GT. 1) THEN
                     DO J= 1,I-1
                        TEMP=A(I,J)
                        A(I,J)=A(IP,J)
                        A(IP,J)=TEMP
                     END DO
                  ENDIF
               ELSE
                  IP=-IPIV(I)
                  I=I-1
                  IF (I .GT. 1) THEN
                     DO J= 1,I-1
                        TEMP=A(I+1,J)
                        A(I+1,J)=A(IP,J)
                        A(IP,J)=TEMP
                     END DO
                  ENDIF
               ENDIF
               I=I-1
            END DO
            I=1
            DO WHILE ( I .LE. N-1 )
               IF( IPIV(I) .LT. 0 ) THEN
                  A(I+1,I)=E(I)
                  I=I+1
               ENDIF
               I=I+1
            END DO
         END IF
      END IF
      RETURN
      END
      SUBROUTINE DSYR(UPLO,N,ALPHA,X,INCX,A,LDA)
      DOUBLE PRECISION ALPHA
      INTEGER INCX,LDA,N
      CHARACTER UPLO
      DOUBLE PRECISION A(LDA,*),X(*)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D+0)
      DOUBLE PRECISION TEMP
      INTEGER I,INFO,IX,J,JX,KX
      LOGICAL LSAME
      EXTERNAL LSAME
      EXTERNAL XERBLA
      INTRINSIC MAX
      INFO = 0
      IF (.NOT.LSAME(UPLO,'U') .AND. .NOT.LSAME(UPLO,'L')) THEN
          INFO = 1
      ELSE IF (N.LT.0) THEN
          INFO = 2
      ELSE IF (INCX.EQ.0) THEN
          INFO = 5
      ELSE IF (LDA.LT.MAX(1,N)) THEN
          INFO = 7
      END IF
      IF (INFO.NE.0) THEN
          CALL XERBLA('DSYR  ',INFO)
          RETURN
      END IF
      IF ((N.EQ.0) .OR. (ALPHA.EQ.ZERO)) RETURN
      IF (INCX.LE.0) THEN
          KX = 1 - (N-1)*INCX
      ELSE IF (INCX.NE.1) THEN
          KX = 1
      END IF
      IF (LSAME(UPLO,'U')) THEN
          IF (INCX.EQ.1) THEN
              DO 20 J = 1,N
                  IF (X(J).NE.ZERO) THEN
                      TEMP = ALPHA*X(J)
                      DO 10 I = 1,J
                          A(I,J) = A(I,J) + X(I)*TEMP
   10                 CONTINUE
                  END IF
   20         CONTINUE
          ELSE
              JX = KX
              DO 40 J = 1,N
                  IF (X(JX).NE.ZERO) THEN
                      TEMP = ALPHA*X(JX)
                      IX = KX
                      DO 30 I = 1,J
                          A(I,J) = A(I,J) + X(IX)*TEMP
                          IX = IX + INCX
   30                 CONTINUE
                  END IF
                  JX = JX + INCX
   40         CONTINUE
          END IF
      ELSE
          IF (INCX.EQ.1) THEN
              DO 60 J = 1,N
                  IF (X(J).NE.ZERO) THEN
                      TEMP = ALPHA*X(J)
                      DO 50 I = J,N
                          A(I,J) = A(I,J) + X(I)*TEMP
   50                 CONTINUE
                  END IF
   60         CONTINUE
          ELSE
              JX = KX
              DO 80 J = 1,N
                  IF (X(JX).NE.ZERO) THEN
                      TEMP = ALPHA*X(JX)
                      IX = JX
                      DO 70 I = J,N
                          A(I,J) = A(I,J) + X(IX)*TEMP
                          IX = IX + INCX
   70                 CONTINUE
                  END IF
                  JX = JX + INCX
   80         CONTINUE
          END IF
      END IF
      RETURN
      END
	SUBROUTINE IDPC(X,SUMSQJ)
        IMPLICIT REAL*8(A-H,O-Z)
        DIMENSION X(32),P(32)
        COMMON/CNST/ N,ND,NI,NUP,NUIC,NP
        COMMON/PARAMD/ P
! NEW PARALLEL CODE BELOW AS OF idm1x17.f.
!$omp   Threadprivate(/PARAMD/)
!  AS OF npageng28.f/idm1x17.f, COMMENT OUT CALL TO SYMBOL HERE.
!	CALL SYMBOL
        DO I=1,NP
	  P(I)=X(I)
	END DO
	CALL SUMSQ(SUMLM)
        SUMSQJ=SUMLM
        RETURN
	END SUBROUTINE IDPC
      SUBROUTINE FUNC(M,F)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/BOLUSCOMP/NBCOMP
      COMMON/OBSER/ TIM,SIG,RS,YO,BS
      COMMON/CNST/ N,ND,NI,NUP,NUIC,NP
      COMMON/INPUT/ R,B
      COMMON/PARAMD/ P
      COMMON/CNST2/ NPL,NOS,NDRUG,NADD
      COMMON/STATE/ X
      COMMON STDEV
      COMMON/ERR/ERRFIL
      PARAMETER(MAXNUMEQ=7)
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp Threadprivate(/PARAMD/,/INPUT/,/STATE/,RSO,BSO,SIGO,Y)
      DIMENSION X(20),P(32),TIM(594),SIG(5000),SIGO(5000),R(37),
     1 RS(5000,34),RSO(5000,34),YT(MAXNUMEQ),YO(594,MAXNUMEQ),F(3564),
     2 BS(5000,7),Y(594,MAXNUMEQ),B(20),NBCOMP(7),STDEV(594,MAXNUMEQ),
     3 FA(7),TLAG(7),XSTORE(100,20),XPRED(20),BSO(5000,7),
     4 XVERIFY(100)
! ADDED BSO(.,.) AS OF idm1x17.f.
      CHARACTER ERRFIL*20
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
      Save RSO,BSO,SIGO,Y
      KNS=1
      KNT=1
      T=0.0D0
      ISKIPBOL = 0
      DO I = 1,NDRUG
       R(2*I-1) = 0.D0
      END DO
!  AS OF idm1x17.f, ESTABLISH BSO(.,.), AND THEN USE BSO,RSO,SIGO,
!  AND NDO RATHER THAN BS,RS,SIG, AND ND FOR ALL CODE BELOW.
      DO I=1,ND
       DO J=1,NDRUG
        BSO(I,J)=RS(I,2*J)
       END DO
      END DO
      DO I=1,NI
       R(I)=RS(KNS,I)
      END DO
	 CALL GETFA(FA)
         NDO = ND
        SIGO(1:NDO) = SIG(1:NDO) !CR
        DO J=1,NI !CR
          DO I=1,NDO
            RSO(I,J) = RS(I,J)
          enddo
        enddo
        IF(N .EQ. 0) GO TO 75
	 CALL GETIX(N,X)
   75	 CALL GETTLAG(TLAG)
      NTL = 0
      DO ID = 1,NDRUG
       IF(TLAG(ID) .NE. 0) NTL = 1
      END DO
	IF(NTL .EQ. 1) THEN
	 CALL SHIFT(TLAG,NDO,SIGO,NDRUG,NADD,RSO)
        DO J=1,NDRUG !CR re-order the do loop make is column major.
          j2=2*j
          DO I=1,NDO
            BSO(I,J)=RSO(I,j2)
           END DO
        END DO
	ENDIF
        IF(TIM(KNT).GE.SIGO(KNS)) GO TO 12
        IF(TIM(KNT).NE.0.0D0) GO TO 45
        CALL OUTPUT(0.D0,YT)
        DO 2000 I=1,NOS
2000    Y(KNT,I)=YT(I)
        KNT=KNT+1
        GO TO 45
12      IF(TIM(KNT).GT.SIGO(KNS)) GO TO 13
        IF(TIM(KNT).NE.0.0D0) GO TO 45
        CALL OUTPUT(0.D0,YT)
        DO 2005 I=1,NOS
2005    Y(KNT,I)=YT(I)
        KNT=KNT+1
13      IF(SIGO(KNS) .GT. 0.0D0) GO TO 45
      ISTEADY = 0
      IF(SIGO(KNS) .LT. 0.D0) THEN
       ISTEADY = 1
       NSET = 1
       DOSEINT = -SIGO(KNS)
       SIGO(KNS) = 0
      ENDIF
      DO I=1,NI
       R(I)=RSO(KNS,I)
      END DO
      IF(NDRUG .EQ. 0) GO TO 81
	 CALL GETFA(FA)
      IF(N .EQ. 0) GO TO 120
       DO I=1,NDRUG
       X(NBCOMP(I))=X(NBCOMP(I))+BSO(KNS,I)*FA(I)
      END DO
      GO TO 81
120   DO I=1,NDRUG
       B(I)=BSO(KNS,I)*FA(I)
      END DO
81    KNS = KNS+1
! NEW PARALLEL CODE BELOW AS OF npageng28.f
!
!45    IF(KNS .GT. NDO) GO TO 15
! Replaced by below
 45   Continue
 46   IF(KNS .GT. NDO) GO TO 15
      IF(TIM(KNT) .EQ. 0.D0 .AND. KNT .GT. 1) THEN
      DO IKNS = KNS,NDO
       IF(SIGO(IKNS) .LE. 0.D0) GO TO 110
      END DO
       XVERIFY(1) = SIGO(KNS)
       CALL VERIFYVAL(1,XVERIFY)
       WRITE(*,111) NDO,KNS,XVERIFY(1)
       WRITE(25,111) NDO,KNS,XVERIFY(1)
 111  FORMAT(//' THE CURRENT SUBJECT HAS AN OBSERVATION TIME RESET'/
     1' ROW WITHOUT AN ACCOMPANYING DOSE RESET ROW. THE PROGRAM NOW'/
     2' STOPS. '//
     3' REVIEW YOUR PATIENT FILES AND CORRECT THE ERROR.'//
     4' NOTE THAT THE ',I4,' DOSE TIMES (POSSIBLY ALTERED BY TIMELAGS'/
     5' ARE THE FOLLOWING (AND THERE IS NO TIME .LE. 0 AFTER TIME'/
     6' NO. ',I4,' WHICH HAS CORRESPONDING TIME ',F15.4,'):')
       OPEN(42,FILE=ERRFIL)
        WRITE(42,111) NDO,KNS,XVERIFY(1)
      DO I = 1,NDO
       WRITE(*,*) SIGO(I)
       WRITE(25,*) SIGO(I)
       WRITE(42,*) SIGO(I)
      END DO
       CLOSE(42)
      CALL PAUSE
      STOP
  110 KNS = IKNS
      DO I=1,NI
       R(I)=RSO(KNS,I)
      END DO
        CALL GETIX(N,X)
       T = 0.D0
      ISTEADY = 0
      IF(SIGO(KNS) .LT. 0.D0) THEN
       ISTEADY = 1
       NSET = 1
       DOSEINT = -SIGO(KNS)
       SIGO(KNS) = 0
      ENDIF
	ENDIF
      IF(TIM(KNT) .NE. SIGO(KNS)) GO TO 20
      ID=2
      TOUT=TIM(KNT)
      KNT=KNT+1
      KNS=KNS+1
      IF(N .EQ. 0) GO TO 31
      GO TO 30
20    IF(TIM(KNT) .GT. SIGO(KNS) .AND. SIGO(KNS) .GT. 0) GO TO 25
15    ID=0
      TOUT=TIM(KNT)
      KNT=KNT+1
      IF(N .EQ. 0) GO TO 31
      GO TO 30
25    ID=1
      TOUT=SIGO(KNS)
      KNS=KNS+1
      IF(N .EQ. 0) GO TO 31
30      CONTINUE
32      IF(N .NE. -1) CALL USERANAL(X,T,TOUT)
        IF(N .EQ. -1) CALL ANAL3(X,T,TOUT)
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
          DO I = KNS,NDO
           IF(SIGO(I) .GE. 100.D0*DOSEINT .OR. SIGO(I) .LE. 0.D0) THEN
            KNSNEW = I
            GO TO 100
           ENDIF
          END DO
          KNS = NDO+1
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
        IF(ID .EQ. 1) GO TO 35
        KNTM1=KNT-1
        CALL OUTPUT(TIM(KNTM1),YT)
        DO 2010 I=1,NOS
2010    Y(KNTM1,I)=YT(I)
55      IF(ID.EQ.0) GO TO 40
  35    CONTINUE
        IF(NI .EQ. 0) GO TO 83
        DO I=1,NI
         R(I)=RSO(KNS-1,I)
        END DO
	 CALL GETFA(FA)
83      IF(NDRUG .EQ. 0 .OR. N .EQ. 0) GO TO 82
        IF(ISKIPBOL .EQ. 0) THEN
         DO I=1,NDRUG
          X(NBCOMP(I))=X(NBCOMP(I))+BSO(KNS-1,I)*FA(I)
         END DO
        ENDIF
      ISKIPBOL = 0
82      CONTINUE
40      IF(KNT .LE. M) GO TO 46
        DO J=1,NOS
         jnow=(J-1)*M
         DO I=1,M
	  IF(YO(I,J) .EQ. -99) then
            F(jnow+I) = 0.D0
          else
            F(jnow+I) =(Y(I,J)-YO(I,J))/STDEV(I,J)
          endif
	 END DO
	END DO
! NEW PARALLEL CODE BELOW AS OF npageng28.f
!	 ND = NDO
!	 DO I=1,ND
!	  SIG(I) = SIGO(I)
!	  DO J=1,NI
!	   RS(I,J) = RSO(I,J)
!	  END DO
!	 END DO
!         DO I=1,ND
!          DO J=1,NDRUG
!           BS(I,J)=RS(I,2*J)
!	  END DO
!	 END DO
      RETURN
      END
        SUBROUTINE SUMSQ(SSQ)
        IMPLICIT REAL*8(A-H,O-Z)
        COMMON/SUM2/ M,NPNL
        COMMON/CNST2/ NPL,NOS,NDRUG,NADD
        DIMENSION F(3564)
        CALL FUNC(M,F)
        NUMRES=M*NOS
        ssq=sum(F(1:NUMRES)**2)
        RETURN
        END SUBROUTINE SUMSQ
        SUBROUTINE USERANAL(X,TIN,TOUT)
        IMPLICIT REAL*8(A-H,O-Z)
        DIMENSION X(20),ATOL(20),RWORK(1002),IWORK(50)
	EXTERNAL DIFFEQ,JACOB
	COMMON/TOUSER/NDIM,MF,RTOL,ATOL
	ITOL=2
	ITASK=1
	ISTATE=1
	IOPT=0
	LRW=1002
	LIW=50
        CALL DVODE(DIFFEQ,NDIM,X,TIN,TOUT,ITOL,RTOL,ATOL,ITASK,ISTATE,
     1            IOPT,RWORK,LRW,IWORK,LIW,JACOB,MF,RPAR,IPAR)
	TIN=TOUT
        RETURN
        END
      SUBROUTINE PREDLAST3(NN,NSET,XSTORE,XPRED,ICONV)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XSTORE(100,20),XPRED(20),COMP(5,20)
        TOL1 = .0005D0
        TOL2 = .0005D0
      II = 0
      DO I = NSET-4,NSET
       II = II+1
       DO J = 1,NN
        COMP(II,J) = XSTORE(I,J)
       END DO
      END DO
      DO IN = 1,NN
       A1 = COMP(1,IN)
       A2 = COMP(2,IN)
       A3 = COMP(3,IN)
       DEL1 = A2 - A1
       DEL2 = A3 - A2
       CALL THESAME(DEL1,0.D0,ISAME1)
       IF(ISAME1 .EQ. 0) THEN
        F = DEL2/DEL1
        CALL THESAME(F,1.D0,ISAMEF1)
        IF(ISAMEF1 .EQ. 0) PRED1 = A1 + DEL1/(1.D0 - F)
       ENDIF
       A1 = COMP(2,IN)
       A2 = COMP(3,IN)
       A3 = COMP(4,IN)
       DEL1 = A2 - A1
       DEL2 = A3 - A2
       CALL THESAME(DEL1,0.D0,ISAME2)
       IF(ISAME2 .EQ. 0) THEN
        F = DEL2/DEL1
        CALL THESAME(F,1.D0,ISAMEF2)
        IF(ISAMEF2 .EQ. 0) PRED2 = A1 + DEL1/(1.D0 - F)
       ENDIF
       A1 = COMP(3,IN)
       A2 = COMP(4,IN)
       A3 = COMP(5,IN)
       DEL1 = A2 - A1
       DEL2 = A3 - A2
       CALL THESAME(DEL1,0.D0,ISAME3)
       IF(ISAME3 .EQ. 0) THEN
        F = DEL2/DEL1
        CALL THESAME(F,1.D0,ISAMEF3)
        IF(ISAMEF3 .EQ. 0) PRED3 = A1 + DEL1/(1.D0 - F)
       ENDIF
       ISAMETOT = ISAME1 + ISAME2 + ISAME3
       ISAMEFTOT = ISAMEF1 + ISAMEF2 + ISAMEF3
       IF(ISAMETOT .EQ. 0 .AND. ISAMEFTOT .EQ. 0) THEN
        DEN = PRED1+PRED3-2.D0*PRED2
        CALL THESAME(DEN,0.D0,ISAMEDEN)
        IF(ISAMEDEN .EQ. 0) PREDNEG = (PRED1*PRED3 - PRED2*PRED2)/DEN
        ICONV = 1
        IF(DABS(PRED3/PRED2 - 1.D0) .GE. TOL1) ICONV = 0
        IF(ISAMEDEN .EQ. 0 .AND. DABS(PREDNEG/PRED3 - 1.D0) .GE. TOL2)
     1   ICONV = 0
        IF(ICONV .EQ. 1 .AND. ISAMEDEN .EQ. 1) XPRED(IN) = PRED3
        IF(ICONV .EQ. 1 .AND. ISAMEDEN .EQ. 0) XPRED(IN) = PREDNEG
       ENDIF
       IF(ISAMETOT .EQ. 3) THEN
        CALL THESAME(COMP(5,IN),COMP(1,IN),ISAME)
        IF(ISAME .EQ. 1) THEN
         ICONV = 1
         XPRED(IN) = COMP(1,IN)
        ENDIF
        IF(ISAME .EQ. 0) ICONV = 0
       ENDIF
       IF(ISAMETOT .EQ. 1 .OR. ISAMETOT .EQ. 2) ICONV = 0
       IF(ICONV .EQ. 0) RETURN
      END DO
      RETURN
      END
	SUBROUTINE IDCALCY(NPP,NDIM,ESTML,YPRED,NUMEQT)
        IMPLICIT REAL*8(A-H,O-Z)
        DIMENSION ESTML(32),YPRED(594,NUMEQT),P(32)
        COMMON/CNST/ N,ND,NI,NUP,NUIC,NP
        COMMON/PARAMD/ P
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp Threadprivate(/PARAMD/)
	CALL SYMBOL
	N = NDIM
	NP = NPP
        DO I=1,NP
	  P(I) = ESTML(I)
	END DO
	CALL EVAL2(YPRED,NUMEQT)
        RETURN
	END
	SUBROUTINE FUNC2(M,YPRED,NUMEQT)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/BOLUSCOMP/NBCOMP
      COMMON/OBSER/ TIM,SIG,RS,YO,BS
      COMMON/CNST/ N,ND,NI,NUP,NUIC,NP
      COMMON/INPUT/ R,B
      COMMON/PARAMD/ P
      COMMON/CNST2/ NPL,NOS,NDRUG,NADD
      COMMON/STATE/ X
      COMMON/ERR/ERRFIL
      PARAMETER(MAXNUMEQ=7)
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp Threadprivate(/PARAMD/,/INPUT/,/STATE/)
      DIMENSION X(20),P(32),TIM(594),SIG(5000),SIGO(5000),R(37),
     1 RS(5000,34),RSO(5000,34),YT(MAXNUMEQ),YO(594,MAXNUMEQ),
     2 YPRED(594,NUMEQT), BS(5000,7),Y(594,MAXNUMEQ),B(20),NBCOMP(7),
     3 TLAG(7),FA(7),XSTORE(100,20),XPRED(20),XVERIFY(100)
      CHARACTER ERRFIL*20
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
	 CALL GETFA(FA)
	 NDO = ND
	 DO I=1,ND
	  SIGO(I) = SIG(I)
	  DO J=1,NI
	   RSO(I,J) = RS(I,J)
	  END DO
	 END DO
        IF(N .EQ. 0) GO TO 75
	 CALL GETIX(N,X)
   75	 CALL GETTLAG(TLAG)
      NTL = 0
	DO ID = 1,NDRUG
	 IF(TLAG(ID) .NE. 0) NTL = 1
	END DO
	IF(NTL .EQ. 1) THEN
	 CALL SHIFT(TLAG,ND,SIG,NDRUG,NADD,RS)
      DO I=1,ND
       DO J=1,NDRUG
        BS(I,J)=RS(I,2*J)
       END DO
      END DO
	ENDIF
      IF(TIM(KNT).GE.SIG(KNS)) GO TO 12
      IF(TIM(KNT).NE.0.0D0) GO TO 45
      CALL OUTPUT(0.D0,YT)
	DO 2000 I=1,NOS
2000    Y(KNT,I)=YT(I)
        KNT=KNT+1
        GO TO 45
12      IF(TIM(KNT).GT.SIG(KNS)) GO TO 13
        IF(TIM(KNT).NE.0.0D0) GO TO 45
        CALL OUTPUT(0.D0,YT)
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
	 CALL GETFA(FA)
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
       CALL GETIX(N,X)
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
32      IF(N .NE. -1) CALL USERANAL(X,T,TOUT)
        IF(N .EQ. -1) CALL ANAL3(X,T,TOUT)
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
        CALL OUTPUT(TIM(KNTM1),YT)
        DO 2010 I=1,NOS
2010    Y(KNTM1,I)=YT(I)
55      IF(ID.EQ.0) GO TO 40
  35    CONTINUE
        IF(NI .EQ. 0) GO TO 83
        DO I=1,NI
         R(I)=RS(KNS-1,I)
        END DO
	 CALL GETFA(FA)
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
	SUBROUTINE EVAL2(YPRED,NUMEQT)
	IMPLICIT REAL*8(A-H,O-Z)
	COMMON/SUM2/ M,NPNL
        COMMON/CNST2/ NPL,NOS,NDRUG,NADD
	DIMENSION YPRED(594,NUMEQT)
	CALL FUNC2(M,YPRED,NUMEQT)
	RETURN
	END
	SUBROUTINE IDCALCYY(NPP,NDIM,ESTML,TPRED,NUMT,YYPRED,NUMEQT)
        IMPLICIT REAL*8(A-H,O-Z)
        DIMENSION ESTML(32),YYPRED(71281,NUMEQT),TPRED(71281),P(32)
        COMMON/CNST/ N,ND,NI,NUP,NUIC,NP
        COMMON/PARAMD/ P
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp Threadprivate(/PARAMD/)
	CALL SYMBOL
	N = NDIM
	NP = NPP
        DO I=1,NP
	  P(I) = ESTML(I)
	END DO
	CALL EVAL3(NUMT,YYPRED,TPRED,NUMEQT)
        RETURN
	END
      SUBROUTINE FUNC3(NUMT,YYPRED,TPRED,NUMEQT)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/BOLUSCOMP/NBCOMP
      COMMON/OBSER/ TIM,SIG,RS,YO,BS
      COMMON/CNST/ N,ND,NI,NUP,NUIC,NP
      COMMON/INPUT/ R,B
      COMMON/PARAMD/ P
      COMMON/CNST2/ NPL,NOS,NDRUG,NADD
      COMMON/STATE/ X
      COMMON/ERR/ERRFIL
      PARAMETER(MAXNUMEQ=7)
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp Threadprivate(/PARAMD/,/INPUT/,/STATE/)
      DIMENSION X(20),P(32),TIM(594),SIG(5000),SIGO(5000),R(37),
     1 RS(5000,34),RSO(5000,34),YT(MAXNUMEQ),YO(594,MAXNUMEQ),
     2 YYPRED(71281,NUMEQT),BS(5000,7),Y(71281,MAXNUMEQ),B(20),
     3 NBCOMP(7),TPRED(71281),TLAG(7),FA(7),XSTORE(100,20),XPRED(20),
     4 XVERIFY(100)
      CHARACTER ERRFIL*20
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
	 CALL GETFA(FA)
	 NDO = ND
	 DO I=1,ND
	  SIGO(I) = SIG(I)
	  DO J=1,NI
	   RSO(I,J) = RS(I,J)
	  END DO
	 END DO
        IF(N .EQ. 0) GO TO 75
	 CALL GETIX(N,X)
   75	 CALL GETTLAG(TLAG)
      NTL = 0
	DO ID = 1,NDRUG
	 IF(TLAG(ID) .NE. 0) NTL = 1
	END DO
	IF(NTL .EQ. 1) THEN
	 CALL SHIFT(TLAG,ND,SIG,NDRUG,NADD,RS)
         DO I=1,ND
          DO J=1,NDRUG
           BS(I,J)=RS(I,2*J)
	  END DO
	 END DO
	ENDIF
      IF(TPRED(KNT).GE.SIG(KNS)) GO TO 12
      IF(TPRED(KNT).NE.0.0D0) GO TO 45
        CALL OUTPUT(0.D0,YT)
	DO 2000 I=1,NOS
2000    Y(KNT,I)=YT(I)
        KNT=KNT+1
        GO TO 45
12    IF(TPRED(KNT) .GT. SIG(KNS)) GO TO 13
      IF(TPRED(KNT) .NE. 0.0D0) GO TO 45
      CALL OUTPUT(0.D0,YT)
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
	 CALL GETFA(FA)
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
       CALL GETIX(N,X)
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
32      IF(N .NE. -1) CALL USERANAL(X,T,TOUT)
        IF(N .EQ. -1) CALL ANAL3(X,T,TOUT)
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
        CALL OUTPUT(TPRED(KNTM1),YT)
        DO 2010 I=1,NOS
2010    Y(KNTM1,I)=YT(I)
55      IF(ID.EQ.0) GO TO 40
  35    CONTINUE
        IF(NI .EQ. 0) GO TO 83
        DO I=1,NI
         R(I)=RS(KNS-1,I)
        END DO
	 CALL GETFA(FA)
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
	SUBROUTINE EVAL3(NUMT,YYPRED,TPRED,NUMEQT)
	IMPLICIT REAL*8(A-H,O-Z)
	COMMON /SUM2/ M,NPNL
        COMMON/CNST2/ NPL,NOS,NDRUG,NADD
	DIMENSION YYPRED(71281,NUMEQT),TPRED(71281)
	CALL FUNC3(NUMT,YYPRED,TPRED,NUMEQT)
	RETURN
	END
      SUBROUTINE NPAG(MAXSUB,MAXGRD,MAXDIM,MAXACT,
     1  NUMEQT,MAXOBS,WORK,WORKK,SPXGYJ,DXI,PYJGX,PYJGXX,
     2  DENSTOR,EXX,CORDEN,CORHOLD,YPREDPOP,YPREDPOPT,YPREDBAY,CORDLAST)
      IMPLICIT REAL*8(A-H,O-Z)
	REAL*8 KU
      PARAMETER(MAXNUMEQ=7)
        DIMENSION WORK(MAXGRD),WORKK(MAXGRD),
     1  SPXGYJ(MAXGRD),DXI(MAXGRD),PYJGX(MAXSUB,MAXACT),
     2  PYJGXX(MAXACT),DENSTOR(MAXGRD,4),EXX(MAXSUB,3,30),
     3  CORDEN(MAXGRD,MAXDIM+1),CORHOLD(MAXGRD,MAXDIM+1),
     4  YPREDPOP(MAXSUB,NUMEQT,MAXOBS,3),
     5  YPREDPOPT(MAXSUB,NUMEQT,7201,3),
     6  YPREDBAY(MAXSUB,NUMEQT,MAXOBS,3),IPATVEC(9999),AF(7),
     7  CORDLAST(MAXGRD,MAXDIM+1),XVERIFY(100),
     1  CORSUBRES(MAXGRD,MAXDIM+1)
      DIMENSION YO(594,NUMEQT),SIG(594,MAXNUMEQ),AB(30,2),EX(30),
     1 COV(30,30),E(30,30),STD(30),CORR(30,30),COFVR(30),X(30),
     2 VALFIX(20),CENTER(3,30),EXXX(30),IRAN(32),PX(32),ATOL(20),
     3 TPRED(71281),YYPRED(71281,NUMEQT),YPRED(594,NUMEQT),C0P(NUMEQT),
     4 C1P(NUMEQT),C2P(NUMEQT),C3P(NUMEQT),C0(NUMEQT),C1(NUMEQT),
     5 C2(NUMEQT),C3(NUMEQT),XMED(30),TIMOB(594),DOSTIM(5000),
     5 RS(5000,34),YOO(594,MAXNUMEQ),BS(5000,7),NUMT(MAXSUB),
     6 TTPRED(MAXSUB,7200),TEND(99),NOMAXTIM(MAXSUB),TENDSUB(MAXSUB,99),
     7 TBEGG(99),TBEGGSUB(MAXSUB,99),TPREDREL(71281),
     8 TTPREDREL(MAXSUB,7200),TIMOBREL(MAXSUB,594),OPTVAR(32),EXO(30),
     9 YYYPRED(3,71281,NUMEQT),RANFIXEST(20),START(30),STEP(30)
      CHARACTER PREFIX*5,PAR(30)*11,READLINE*300,EXT*3,
     1PRIFIL2*20,DENFIL*20,PARFIX(20)*11,OUTFIL*20,NAME*4,PREDFIL*20,
     2OUTCOM*20,READLARG*1000,OUTFILER*20,ERRFIL*20,PARRANFIX(20)*11
      character*20 ITFIL
	COMMON SIG
   	COMMON/TOUSER/NDIM,MF,RTOL,ATOL
      COMMON/OBSER/TIMOB,DOSTIM,RS,YOO,BS
	COMMON/SUPRES/ISUPRES
	COMMON/NXER/NXE
	COMMON/TOCALC/IRAN,PX,NOFIX,NSUB,gamma,flat,AB
      COMMON/ERR/ERRFIL
      EXTERNAL CALCRF
    2 FORMAT(A20)
  222 FORMAT(A3)
 2222 FORMAT(A5)
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
 4710		READ(23,*) NDIM
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
		END DO
		READ(23,*) ierrmod,gamlam0
	gamma = 1.d0
	flat = 1.d0
	if(ierrmod .eq. 2) gamma = gamlam0
        if(ierrmod .eq. 3) gamma = gamlam0
        if(ierrmod .eq. 4) flat = gamlam0
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
       CALL NEWWORK1(MAXSUB,JSUB,TIMOBREL)
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
      VOLSPA = product(AB(1:NVAR,2)-AB(1:NVAR,1)) !CR
	IF(ICYCLE .EQ. 0) THEN
      CONST=1.D0/VOLSPA
	DO 30 IG = 1,NGRID
	  CORDEN(IG,NVAR+1)=CONST
	CALL CALGRD(NVAR,NGRID,AB,X)
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
       CALL VERIFYVAL(4,XVERIFY)
	 WRITE(25,162) IEQ,(XVERIFY(IXV),IXV=1,4)
	END DO
  162   FORMAT(' EQ. ',I2,': ',4(G16.10,1X))
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
          resolve=0.20
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
      CALL SYMBOL
 1001 ICYCLE=ICYCLE+1
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
      DO 1000 JSUB=1,NSUB
	WRITE(*,*) JSUB
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT)
	MISVAL = 0
        SIGFAC=1.D0
 	DO 140 I=1,NOBSER
 	 DO 140 J=1,NUMEQT
	  Y = YO(I,J)
	  IF(Y .EQ. -99) THEN
	   MISVAL = MISVAL+1
	   GO TO 140
	  ENDIF
      SIG(I,J) = C0(J)+C1(J)*Y+C2(J)*Y*Y+C3(J)*Y**3
      if(ierrmod.eq.2) sig(i,j) = sig(i,j)*gamma
      if(ierrmod.eq.3) sig(i,j) = dsqrt(sig(i,j)**2 + gamma**2)
      if(ierrmod.eq.4) sig(i,j) = gamma*flat
      IF(SIG(I,J) .EQ. 0) THEN
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
      IF(SIG(I,J) .LT. 0) THEN
		WRITE(*,2346) JSUB
		WRITE(25,2346) JSUB
2346            FORMAT(//' A S.D. < 0 FOR JSUB = ',I5,'. RERUN THE '/
     1' PROGRAM WITH A BETTER CHOICE FOR THE ASSAY ERROR POLYNOMIAL'/
     2' COEFFICIENTS.')
	   	CLOSE(27)
	   	CLOSE(25)
        OPEN(42,FILE=ERRFIL)
         WRITE(42,2346) JSUB
        CLOSE(42)
		CALL PAUSE
	   	STOP
      ENDIF
      SIGFAC=SIGFAC*SIG(I,J)
  140 CONTINUE
        OFAC=2.506628274631**(NOBSER*NUMEQT - MISVAL)
        NOBTOT = NOBTOT + NOBSER*NUMEQT - MISVAL
 8888   FORMAT(' ',' CYCLE ',I5,',  SUBJECT ',I5,' ... COMPLETED = ',
     1F8.2, ' %')
	XNEXT = 1.D0
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp Parallel Default(PRIVATE) Shared(NACTVE,NSTORE,NVAR,CORDEN)
!$omp&Shared(IRAN,VALFIX,MAXACT,PYJGX,WORKK,JSUB,SIGFAC,OFAC,WORK)
!$omp&Shared(MAXTHREAD,NOFIX,RANFIXEST,NRANFIX)
!$omp Do
	DO 800 IG=1,NACTVE
	IF(NACTVE .GT. NSTORE) THEN
	XPER=IG*100.D0/NACTVE
	IF(XPER .GE. XNEXT) THEN
        IF(ICYCLE.eq.1) THEN
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$       GOTO 676
	 WRITE(*,8888) ICYCLE,JSUB,XPER
  676	 IF(NXE .GT. 0) WRITE(*,1254) NXE
 1254    FORMAT('  TOTAL NO. OF NUM. INTEG. WARNINGS IS ',I20)
	ENDIF
	 XNEXT=XNEXT+1.D0
	ENDIF
	ENDIF
	IF(IG .LE. NSTORE) GO TO 700
        X(1:NVAR)=CORDEN(IG,1:NVAR) !CR
      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,RANFIXEST,PX)
	CALL IDPC(PX,W)
	IF(IG .LE. MAXACT) PYJGX(JSUB,IG)=0.D0
	WORKK(IG) = 0.D0
	IF(W .LE. 22708.D0) THEN
	 IF(IG .LE. MAXACT) PYJGX(JSUB,IG) = DEXP(-.5D0*W)/SIGFAC/OFAC
	 WORKK(IG) = DEXP(-.5D0*W)/SIGFAC/OFAC
	ENDIF
	IF(IG .GT. MAXACT) THEN
	 WORK(IG) = WORKK(IG)*CORDEN(IG,NVAR+1)
	 GO TO 800
	ENDIF
  700   WORK(IG)=PYJGX(JSUB,IG)*CORDEN(IG,NVAR+1)
	WORKK(IG) = PYJGX(JSUB,IG)
  800   CONTINUE
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp   End Do
!$omp   End Parallel
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
!	DO I=1,NACTVE
!         SPXGYJ(I)=SPXGYJ(I)+WORK(I)/PYJ
!	END DO
!	SLPYJ=SLPYJ+DLOG(PYJ)
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
      gammab = gamma
      gammap = gamma * (1.d0+gamdel)
      gammam = gamma / (1.d0+gamdel)
      call emint(pyjgx,maxsub,corden,maxgrd,nactve,nsub,1,
     &corden(1,nvar+1),denstor(1,1),denstor(1,2),denstor(1,3),
     &fobj,gap,nvar,keep,IHESS)
      IF(IHESS .EQ. -1) GO TO 900
      fobj1 = fobj
      nactve1 = nactve
      IF(ISUPRES .EQ. 0)
     1 write(6,*) 'base job 1, fobj,keep,icycle=',fobj,keep,icycle
      nactve = keep
      call emint(pyjgx,maxsub,corden,maxgrd,nactve,nsub,0,
     &corden(1,nvar+1),denstor(1,1),denstor(1,2),denstor(1,3),
     &fobj,gap,nvar,keep,IHESS)
      IF(IHESS .EQ. -1) GO TO 900
      fobjbase = fobj
      IF(ISUPRES .EQ. 0)
     1 write(6,*) 'base job 0, fobj,keep,icycle=',fobj,keep,icycle
      IF(ISUPRES .EQ. 0)
     1 write(6,*) 'base job 0 nactve,gamma=',nactve,gamma
      nactve0 = nactve
      denstor(1:nactve,4)=corden(1:nactve,nvar+1) !CR
      nstore = 0
      fobjbest = fobjbase
      IF(ISUPRES .EQ. 0) write(6,*) 'finished base case'
      if(ierrmod.eq.1) go to 14001
      gamma = gammap
      go to 10001
      endif
      if(mod(igamma,3).eq.2) then
      IF(ISUPRES .EQ. 0) write(6,*) 'gamma plus =',gamma
      call emint(pyjgx,maxsub,corden,maxgrd,nactve,nsub,0,
     &corden(1,nvar+1),denstor(1,1),denstor(1,2),denstor(1,3),
     &fobj,gap,nvar,keep,IHESS)
      IF(IHESS .EQ. -1) GO TO 900
      fobjplus = fobj
      IF(ISUPRES .EQ. 0)
     1 write(6,*) 'fobjplus,gamma,icycle=',fobjplus,gamma,icycle
      if(fobjplus.gt.fobjbest) then
        fobjbest = fobjplus
        denstor(1:nactve,4) = corden(1:nactve,nvar+1) !CR
      endif
      gamma = gammam
      IF(ISUPRES .EQ. 0) write(6,*) 'finished plus case'
         go to 10001
      endif
      if(mod(igamma,3).eq.0) then
      IF(ISUPRES .EQ. 0) write(6,*) 'gamma minus=',gamma
      call emint(pyjgx,maxsub,corden,maxgrd,nactve,nsub,0,
     &corden(1,nvar+1),denstor(1,1),denstor(1,2),denstor(1,3),
     &fobj,gap,nvar,keep,IHESS)
      IF(IHESS .EQ. -1) GO TO 900
      fobjminu = fobj
      IF(ISUPRES .EQ. 0)
     1 write(6,*) 'fobjminu,gamma,icycle=',fobjminu,gamma,icycle
      if(fobjminu.gt.fobjbest) then
       fobjbest = fobjminu
       denstor(1:nactve,4) = corden(1:nactve,nvar+1) !CR
      endif
      IF(ISUPRES .EQ. 0) write(6,*) 'finished gamma minus case'
      endif
      gamma = gammab
      fobj = fobjbase
      if(fobjplus.gt.fobjbase) then
      gamma = gammap
      fobj = fobjplus
      gamdel = 4.*gamdel
      endif
      if(fobjminu.gt.fobjbase) then
      gamma = gammam
      fobj = fobjminu
      gamdel = 4.*gamdel
      endif
      gamdel = gamdel*0.5
      if(gamdel.lt.0.01) gamdel=0.01
14001 continue
      IF(ISUPRES .EQ. 0) write(6,*) 'fobjbest=',fobj
      fact=ngrid/volspa
      corden(1:nactve,nvar+1)=fact*denstor(1:nactve,4) !CR
      DO J = 1,NVAR+1  !CR switch I, J order.
       DO I = 1,NACTVE
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
        KP = NVAR + QVAL
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
      IF(NRANFIX .GT. 0 .AND. ICYCLE .EQ. 1) THEN
      DO I = 1,NVAR
       X(I) = EX(I)
      END DO
      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,RANFIXEST,PX)
       DO I = 1,NRANFIX
        START(I) = RANFIXEST(I)
        STEP(I) = -.2D0*START(I)
       END DO
       DO I = NRANFIX+1,NRANFIX+NVAR
        START(I) = EX(I-NRANFIX)
        STEP(I) = -.2D0*START(I)
       END DO
       CALL ELDERY(NRANFIX+NVAR,START,OPTVAR,VALMIN,1.D-10,STEP,1000,
     1  CALCRF,0,ICONV,NITER,ICNT,NUMEQT,YO,C0,C1,C2,C3)
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
 9191 format(i0,t10,g24.16e3,t40,g15.7,t60,g15.7,t80,i0,t95,i0) !CR make it i8 instead of i5
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
        resolve=resolve*0.5
      endif
      if(resolve.le.0.0001) then
        resolve=0.2
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
      IF(ISUPRES .EQ. 0) write(6,*) 'Number of active points =', nactve
      IF(ISUPRES .EQ. 0) write(6,*)
      IF(ISUPRES .EQ. 0)
     1 write(6,*) 'expanding current grid with new points'
      IF(ISUPRES .EQ. 0) write(6,5200) 100.*resolve
 5200 format(' current grid resolution = ',f8.3, '%')
         new=2*nvar+1
         nactveold=nactve
         do ipoint=1,nactveold
           pcur=corden(ipoint,nvar+1)/(2*nvar+1)
           corden(ipoint,nvar+1)=pcur
             do ivar=1,nvar
	       del=(ab(ivar,2)-ab(ivar,1))*resolve
               do i=1,nvar
                  corden(nactve+1,i)=corden(ipoint,i)
	       enddo
	       corden(nactve+1,ivar)=corden(nactve+1,ivar)-del
               corden(nactve+1,nvar+1)=pcur
               ntry=nactve+1
               call checkd(corden,ntry,nactve,ab,maxgrd,nvar,iclose)
	       if(corden(nactve+1,ivar).ge.ab(ivar,1)) then
                if(iclose.eq.0) nactve=nactve+1
	       endif
               do i=1,nvar
                 corden(nactve+1,i)=corden(ipoint,i)
	       enddo
	       corden(nactve+1,ivar)=corden(nactve+1,ivar)+del
               corden(nactve+1,nvar+1)=pcur
               ntry=nactve+1
               call checkd(corden,ntry,nactve,ab,maxgrd,nvar,iclose)
	       if(corden(nactve+1,ivar).le.ab(ivar,2)) then
	         if(iclose.eq.0) nactve=nactve+1
               endif
             enddo
           enddo
      IF(ISUPRES .EQ. 0)
     1 write(6,*) 'Number of actve grid points after expansion =',nactve
      ngridn=nactve
      IF(ISUPRES .EQ. 0) write(6,*)
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
	DO 6000 JSUB=1,NSUB
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT)
	DO ICENTER = 1,3
	 DO J=1,NVAR
	  EXXX(J) = CENTER(ICENTER,J)
	 END DO
      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,EXXX,VALFIX,RANFIXEST,PX)
	CALL IDCALCY(NVAR+NOFIX+NRANFIX,NDIM,PX,YPRED,NUMEQT)
	DO IOBS=1,NOBSER
	 DO IEQ=1,NUMEQT
	 YPREDPOP(JSUB,IEQ,IOBS,ICENTER) = YPRED(IOBS,IEQ)
	 END DO
	END DO
      CALL CALCTPRED(JSUB,IDELTA,NOBSER,NUMT(JSUB),TPRED,TPREDREL,
     1   NOMAXTIM(JSUB),TEND,TBEGG)
	DO J = 1,NUMT(JSUB)
	 TTPRED(JSUB,J) = TPRED(J)
	 TTPREDREL(JSUB,J) = TPREDREL(J)
	END DO
	 DO J = 1,NOMAXTIM(JSUB)
	  TENDSUB(JSUB,J) = TEND(J)
        TBEGGSUB(JSUB,J) = TBEGG(J)
	 END DO
      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,EXXX,VALFIX,RANFIXEST,PX)
	CALL IDCALCYY(NVAR+NOFIX+NRANFIX,NDIM,PX,TPRED,NUMT(JSUB),
     1  YYPRED,NUMEQT)
	DO J=1,NUMT(JSUB)
	 DO IEQ=1,NUMEQT
	 YPREDPOPT(JSUB,IEQ,J,ICENTER) = YYPRED(J,IEQ)
	 END DO
	END DO
	END DO
 6000   CONTINUE
     	REWIND(27)
	NNACTVE=NACTVE
      CORHOLD(1:NACTVE,1:NVAR+1) = CORDEN(1:NACTVE,1:NVAR+1) !CR
	OPEN(31,FILE=PREDFIL)
	DO 7000 JSUB=1,NSUB
      NACTVE=NNACTVE
        CORDEN(1:NACTVE,1:NVAR+1) = CORHOLD(1:NACTVE,1:NVAR+1) !CR
 8506   FORMAT(////' THE FOLLOWING RESULTS ARE FOR SUBJECT ',I4)
	WRITE(*,8506) JSUB
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT)
	MISVAL = 0
        SIGFAC=1.D0
 	DO 240 I=1,NOBSER
 	 DO 240 J=1,NUMEQT
	  Y = YO(I,J)
	  IF(Y .EQ. -99) THEN
	   MISVAL = MISVAL+1
	   GO TO 240
	  ENDIF
          SIG(I,J) = C0(J)+C1(J)*Y+C2(J)*Y*Y+C3(J)*Y**3
      if(ierrmod.eq.2) sig(i,j) = sig(i,j)*gamma
      if(ierrmod.eq.3) sig(i,j)=dsqrt(sig(i,j)**2 + gamma**2)
      if(ierrmod.eq.4) sig(i,j) = gamma*flat
      IF(SIG(I,J) .EQ. 0) THEN
		WRITE(*,2345) JSUB
		WRITE(25,2345) JSUB
        OPEN(42,FILE=ERRFIL)
         WRITE(42,2345) JSUB
        CLOSE(42)
	  	CALL PAUSE
	   	STOP
      ENDIF
      IF(SIG(I,J) .LT. 0) THEN
		WRITE(*,2346) JSUB
		WRITE(25,2346) JSUB
        OPEN(42,FILE=ERRFIL)
         WRITE(42,2346) JSUB
        CLOSE(42)
		CALL PAUSE
	   	STOP
      ENDIF
      SIGFAC=SIGFAC*SIG(I,J)
  240 CONTINUE
      OFAC=2.506628274631**(NOBSER*NUMEQT - MISVAL)
      CALL SUBRES(MAXSUB,MAXACT,JSUB,CORDEN,WORK,MAXGRD,MAXDIM,NVAR,
     1  NOFIX,VALFIX,SIGFAC,OFAC,AB,PAR,NACTVE,NGRID,VOLSPA,IRAN,CENTER,
     2  PYJGXX,NRANFIX,RANFIXEST)
	DO IG = 1,NACTVE
	 PYJGX(JSUB,IG) = PYJGXX(IG)
	END DO
	DO ICENTER = 1,3
	 DO J=1,NVAR
	  EXXX(J) = CENTER(ICENTER,J)
	  EXX(JSUB,ICENTER,J) = CENTER(ICENTER,J)
	 END DO
      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,EXXX,VALFIX,RANFIXEST,PX)
	CALL IDCALCY(NVAR+NOFIX+NRANFIX,NDIM,PX,YPRED,NUMEQT)
	DO IOBS=1,NOBSER
	 DO IEQ=1,NUMEQT
	 YPREDBAY(JSUB,IEQ,IOBS,ICENTER) = YPRED(IOBS,IEQ)
	 END DO
	END DO
	END DO
	DO I=1,NUMT(JSUB)
	 TPRED(I) = TTPRED(JSUB,I)
	END DO
      DO ICENTER = 1,3
       DO J=1,NVAR
        EXXX(J) = CENTER(ICENTER,J)
       END DO
      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,EXXX,VALFIX,RANFIXEST,PX)
      CALL IDCALCYY(NVAR+NOFIX+NRANFIX,NDIM,PX,TPRED,NUMT(JSUB),
     1  YYPRED,NUMEQT)
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
       NUMTT = (TENDSUB(JSUB,IMAXTIM)-TBEGGSUB(JSUB,IMAXTIM))*60/IDELTA
       NDELPER = AUCINT*60/IDELTA
       NWHOLE = NUMTT/NDELPER
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
	IHRST = (IPERIOD-1)*AUCINT
	IHREN = IHRST + AUCINT
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
	IHRST = (IPERIOD-1)*AUCINT
	IHREN = IHRST + NPAR*IDELTA/60
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
	 CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT)
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
	 CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT)
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
        CALL READOUT(OUTFILER)
 	  close(91)
      CLOSE(23)
      CLOSE(27)
      CLOSE(25)
      CLOSE(29)
	STOP
	END
	SUBROUTINE FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT)
        IMPLICIT REAL*8(A-H,O-Z)
        PARAMETER(MAXNUMEQ=7)
        DIMENSION TIM(594),SIG(5000),RS(5000,34),YO(594,NUMEQT),
     1  BS(5000,7),C0(NUMEQT),C1(NUMEQT),C2(NUMEQT),C3(NUMEQT),
     2  YOO(594,MAXNUMEQ)
        COMMON /OBSER/ TIM,SIG,RS,YOO,BS
        COMMON /CNST/ N,ND,NI,NUP,NUIC,NP
        COMMON /CNST2/ NPL,NUMEQTT,NDRUG,NADD
        COMMON /SUM2/ M,NPNL
        COMMON/DESCR/AGE,HEIGHT,ISEX,IETHFLG
        COMMON/ERR/ERRFIL
        CHARACTER SEX*1,READLINE*300,ERRFIL*20
	DO I=1,7
	 READ(27,*)
	END DO
	READ(27,*) AGE
	READ(27,2) SEX
    2   FORMAT(A1)
	ISEX=1
	IF(SEX .EQ. 'F') ISEX=2
	READ(27,*) HEIGHT
	READ(27,*) IETHFLG
    1   FORMAT(A300)
   10	READ(27,1) READLINE
	IF(READLINE(12:23) .NE. 'NO. OF DRUGS') GO TO 10
	BACKSPACE(27)
    3   FORMAT(T2,I5)
        READ(27,3) NDRUG
	IF(NDRUG .GT. 7) THEN
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
	IF(NI .GT. 34) THEN
  	 WRITE(*,123)
  123    FORMAT(/' YOUR PATIENT DATA FILES HAVE TOO MANY COLUMNS IN '/
     1' THE DOSAGE REGIMEN BLOCK. THE NO. OF ADDITIONAL COVARIATES '/
     2' PLUS TWICE THE NO. OF DRUGS CANNOT EXCEED 34. THE PROGRAM IS'/
     3' NOW STOPPING. '/)
        OPEN(42,FILE=ERRFIL)
         WRITE(42,123)
        CLOSE(42)
       CALL PAUSE
	 STOP
	ENDIF
        READ(27,3) ND
	IF(ND .GT. 5000) THEN
	 WRITE(*,125)
  125    FORMAT(' YOUR PATIENT DATA FILES CANNOT HAVE MORE THAN 5000'/
     1' DOSE EVENTS. THE PROGRAM IS NOW STOPPING. '/)
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
	MAXOBDIM = 150
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
	DO I=1,M
	 DO J=1,NUMEQT
	  YOO(I,J) = YO(I,J)
	 END DO
	END DO
	NOBSER=M
   50	READ(27,1) READLINE
	IF(READLINE(1:25) .NE. 'ASSAY COEFFICIENTS FOLLOW') GO TO 50
	DO IEQ = 1,NUMEQT
	 READ(27,*) C0(IEQ),C1(IEQ),C2(IEQ),C3(IEQ)
	END DO
	RETURN
	END
	SUBROUTINE CALGRD(NVAR,NGRID,AB,X)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION X(30),AB(30,2),QUASI(30)
      CHARACTER ERRFIL*20
        save first
	logical flag(2),first
        data first/.TRUE./
      COMMON/ERR/ERRFIL
        if(first) then
         CALL INFAUR(flag,nvar,ngrid)
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
        CALL GOFAUR(quasi)
	DO IC = 1,NVAR
        X(IC) = (AB(IC,2)-AB(IC,1))*quasi(IC) + AB(IC,1)
	END DO
	RETURN
	END
	SUBROUTINE INFAUR(FLAG,DIMEN,ATMOST)
        implicit double precision (a-h,o-z)
        LOGICAL FLAG(2)
      INTEGER S,ATMOST,QS,COEF(0:19,0:19),NEXTN,
     +        TESTN,HISUM,I,J,PRIMES(40),DIMEN
      COMMON /FAURE/ S,QS,COEF,RQS,NEXTN,TESTN,HISUM
      SAVE /FAURE/
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
        SUBROUTINE GOFAUR(QUASI)
        implicit double precision (a-h,o-z)
        INTEGER S,QS,COEF(0:19,0:19),NEXTN,TESTN,
     +        HISUM,I,J,K,YTEMP(0:19),ZTEMP,
     +        KTEMP,LTEMP,MTEMP
      dimension QUASI(30)
      COMMON /FAURE/ S,QS,COEF,RQS,NEXTN,TESTN,HISUM
      SAVE /FAURE/
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
      ESTINT=VOLSPA*sum(func(1:NACTVE))/NGRID !CR
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
      SUBROUTINE MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,RANFIXEST,PX)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION IRAN(32),X(30),VALFIX(20),PX(32),RANFIXEST(20)
      NNNVAR = 0
      NNNFIX = 0
      NNNRANFIX = 0
      DO I = 1,NVAR+NOFIX+NRANFIX
       IF(IRAN(I) .EQ. 1) THEN
        NNNVAR = NNNVAR+1
        PX(I) = X(NNNVAR)
       ENDIF
       IF(IRAN(I) .EQ. 0) THEN
        NNNFIX = NNNFIX+1
        PX(I) = VALFIX(NNNFIX)
       ENDIF
       IF(IRAN(I) .EQ. 2) THEN
        NNNRANFIX = NNNRANFIX+1
        PX(I) = RANFIXEST(NNNRANFIX)
       ENDIF
      END DO
      RETURN
      END
	SUBROUTINE SUBRES(MAXSUB,MAXACT,JSUB,CORDEN,WORK,MAXGRD,MAXDIM,
     1  NVAR,NOFIX,VALFIX,SIGFAC,OFAC,AB,PAR,NACTVE,NGRID,VOLSPA,IRAN,
     2  CENTER,PYJGXX,NRANFIX,RANFIXEST)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AB(30,2),EX(30),COV(30,30),E(30,30),STD(30),
     1   CORR(30,30),COFVR(30),WORK(MAXGRD),CORDEN(MAXGRD,MAXDIM+1),
     2   CENTER(3,30),IRAN(32),PX(32),X(30),VALFIX(20),
     3   PYJGXX(MAXACT),BAYPOS(100,1500,31),NACTSUB(100),XVERIFY(100),
     4   RANFIXEST(20)
      COMMON/BAY/NACTSUB,BAYPOS
      COMMON/ERR/ERRFIL
	REAL*8 KU
	CHARACTER PAR(30)*11,ERRFIL*20
   	NSUB=1
	NINT=100
	WRITE(*,5432)
 5432   FORMAT('1')
      DENMAX = maxval(CORDEN(1:NACTVE,NVAR+1)) !CR
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
	CALL IDPC(PX,W)
	PYJGX=0.D0
	IF(W .LE. 22708.D0) PYJGX = DEXP(-.5D0*W)/SIGFAC/OFAC
        PYJGXX(IG) = PYJGX
       WORK(IG)=PYJGX*CORDEN(IG,NVAR+1)
  800 CONTINUE
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
        IF(JSUB .LE. 100) NACTSUB(JSUB) = NACTVE
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
      subroutine checkd(corden,new,nactveold,ab,maxgrd,nvar,iclose)
      implicit real*8 (a-h,o-z)
      real*8 ab(30,2), corden(maxgrd,1)
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
      subroutine emint(psi,ldpsi,theta,ldtheta,npoint,nsub,ijob,
     &                 x,dx,y,dy,fobj,gap,nvar,keep,IHESS)
      use ieee_arithmetic !CR
      use NPpatch_120 !CR; wmy renamed to accommodate Pmetrics file naming convention
      implicit real*8 (a-h,o-z)
      real*8 mu
      dimension psi(ldpsi,*),theta(ldtheta,*),x(*),dx(*),y(*),dy(*)
      CHARACTER ERRFIL*20
	COMMON/SUPRES/ISUPRES
      COMMON/ERR/ERRFIL
      parameter (MAXSUBem=999,MAXACTem=10000000)
      dimension w(MAXSUBem),dw(MAXSUBem),Ptx(MAXSUBem),
     &          hess(MAXSUBem,2*MAXSUBem),
     &          hess_copy(MAXSUBem,2*MAXSUBem)
      dimension psisum(MAXSUBem),XVERIFY(100)
      integer kpvt(MAXSUBem), ipivot(MAXACTem), list(MAXACTem)
      integer*8, save :: iloop = 0 !CR
      logical :: cf_repaired !CR
      keep = npoint !CR originally it is nactve which is wrong.
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
       IF(ILOOP .GT. 0) WRITE(*,123) iter,XVERIFY(1)
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
           hess_copy(j,k) = hess(j,k)
        enddo
      enddo
      do i=1,npoint
        scale=x(i)/y(i)
       do j=1,nsub
         fact=scale*psi(j,i)
         do k=j,nsub
           hess(k,j)=hess(k,j)+fact*psi(k,i)
           hess_copy(k,j) = hess(k,j)
         enddo
       enddo
      enddo
      do j=1,nsub-1
        do k=j+1,nsub
          hess(j,k)=hess(k,j)
          hess_copy(j,k) = hess(j,k)
        enddo
      enddo
      do j=1,nsub
        hess(j,j)=hess(j,j)+Ptx(j)/w(j)
        hess_copy(j,j) = hess(j,j)
      enddo
      tbuildb=0
      tbuild=tbuildb-tbuilda
      IF(ISUPRES .EQ. 0) write(6,*) 'tbuild=',tbuild
      CALL DPOTRF( 'L', nsub, hess, MAXSUBem, INFO )
      tbuildc=0
      tfactor=tbuildc-tbuildb
      IF(ISUPRES .EQ. 0) write(6,*) 'tfactor=',tfactor
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
        write(*,*) 'dw repair begin'
        call dw_repair('L',nsub,hess,dw,42,'dw_repair.txt',info)
        if (info /= 0) then
          write(*,*) 'dw repair failed!'
          write(42,*) 'dw repair failed!'
          CLOSE(42)
          return
        else
          write(*,*) 'dw repair is successful!'
          write(42,*) 'dw repair is successful!'
          CLOSE(42)
        endif
      else
        do j=1,nsub
          sum=0.d0
          do i=1,npoint
            sum=sum+psi(j,i)*smu/y(i)
          enddo
          dw(j)=1.d0/w(j)-sum
        enddo
        call DPOTRS( 'L', nsub, 1, hess, MAXSUBem, dw, nsub, INFO )
      endif
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
     1   NOMAXTIMS,TEND,TBEGG)
	IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(MAXNUMEQ=7)
      DIMENSION TPRED(71281),TEND(99),TIM(594),SIG(5000),RS(5000,34),
     1  BS(5000,7),YOO(594,MAXNUMEQ),TBEGG(99),TPREDREL(71281)
        COMMON/OBSER/ TIM,SIG,RS,YOO,BS
        COMMON/CNST/ N,ND,NI,NUP,NUIC,NP
      IDOSE = 1
	NOMAXTIMS = 0
	NUMTSUB = 0
	INDEX = 0
   50	TIMMAX = -1.D30
   10 INDEX = INDEX + 1
	IF(TIM(INDEX) .GT. TIMMAX) TIMMAX = TIM(INDEX)
	IF(TIM(INDEX) .LE. 0.D0 .AND. INDEX .GT. 1) GO TO 20
	IF(INDEX .EQ. NOBSER) GO TO 20
	GO TO 10
   20   CONTINUE
      TBEG = 0.D0
      IF(SIG(IDOSE) .LT. 0.D0) TBEG = 100.D0*(-SIG(IDOSE))
	T_END = TIMMAX + 24.D0
	NUMT2 = (T_END - TBEG)*60/IDELTA
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
      IF(IDOSE .LT. ND) THEN
       DO ID = IDOSE + 1,ND
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
      SUBROUTINE NEWWORK1(MAXSUB,JSUB,TIMOBREL)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(MAXNUMEQ=7)
      DIMENSION SIG(5000),RS(5000,34),DELTAIV(7),ORDELT(7),
     1 RSS(5000,34),SIGG(5000),TIM(594),TIMM(594),YO(594,MAXNUMEQ),
     2 TIMDELAY(99),TIMOBREL(MAXSUB,594),OBSBLOCK(800,150,MAXNUMEQ+1),
     3 DOSEBLOCK(800,1000,35),NDORIG(800),XVERIFY(100)
      CHARACTER READLINE*300,ERRFIL*20
   	COMMON/DOSEOBS/DOSEBLOCK,OBSBLOCK,NDORIG
      COMMON/ERR/ERRFIL
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
      SUBROUTINE ORDERDELTA(NDRUG,DELTAIV,NDELTA,ORDELT)
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
	SUBROUTINE THESAME(X1,X2,ISAME)
	IMPLICIT REAL*8(A-H,O-Z)
	ISAME = 0
	XDEL = DABS(X1-X2)
	IF(XDEL .LE. 1.D-10) ISAME = 1
	RETURN
	END
      SUBROUTINE VERIFYVAL(N,X)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(100)
      DO I = 1,N
       IF(X(I) .GE. -1.D-99 .AND. X(I) .LE. 1.D-99) X(I) = 0.D0
      END DO
      RETURN
      END
	SUBROUTINE CALCRF(NTOTPAR,VEC,FNTVAL,NUMEQT,YO,C0,C1,C2,C3)
	IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(MAXNUMEQ=7)
	DIMENSION VEC(NTOTPAR),IRAN(32),PX(32),SIG(594,MAXNUMEQ),
     1 YO(594,NUMEQT),C0(NUMEQT),C1(NUMEQT),C2(NUMEQT),C3(NUMEQT),
     2 AB(30,2)
      COMMON SIG
	COMMON/TOCALC/IRAN,PX,NOFIX,NSUB,gamma,flat,AB
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
       CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT)
       DO 140 I=1,NOBSER
        DO 140 J=1,NUMEQT
         Y = YO(I,J)
         IF(Y .EQ. -99) GO TO 140
         SIG(I,J) = C0(J)+C1(J)*Y+C2(J)*Y*Y+C3(J)*Y**3
         if(ierrmod.eq.2) sig(i,j) = sig(i,j)*gamma
         if(ierrmod.eq.3) sig(i,j)=dsqrt(sig(i,j)**2 + gamma**2)
         if(ierrmod.eq.4) sig(i,j) = gamma*flat
  140    CONTINUE
       CALL IDPC(PX,W)
       SUMTOT = SUMTOT + W
      END DO
      FNTVAL = SUMTOT
	RETURN
	END
        SUBROUTINE ELDERY(N,START,XMIN,YNEWLO,REQMIN,STEP,
     X  ITMAX,FUNC,IPRINT,ICONV,NITER,ICOUNT,NUMEQT,YO,C0,C1,C2,C3)
      IMPLICIT REAL*8(A-H,O-Z)
        DIMENSION START(N),STEP(N),XMIN(N),XSEC(30),
     X  P(30,31),PSTAR(30),P2STAR(30),PBAR(30),Y(31),YO(594,NUMEQT),
     1  C0(NUMEQT),C1(NUMEQT),C2(NUMEQT),C3(NUMEQT)
        EXTERNAL FUNC
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
        XN=FLOAT(N)
        DN=FLOAT(N)
        NN=N+1
1001    DO 1 I=1,N
1       P(I,NN)=START(I)
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3)
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
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3)
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
        CALL FUNC(N,PSTAR,FN,NUMEQT,YO,C0,C1,C2,C3)
        YSTAR=FN
        ICOUNT=ICOUNT+1
        IF(YSTAR.GE.YLO) GO TO 12
        IF(ICOUNT.GE.KCOUNT) GO TO 19
        DO 9 I=1,N
9       P2STAR(I)=ECOEFF*PSTAR(I)+(1.0D0-ECOEFF)*PBAR(I)
        CALL FUNC(N,P2STAR,FN,NUMEQT,YO,C0,C1,C2,C3)
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
        CALL FUNC(N,P2STAR,FN,NUMEQT,YO,C0,C1,C2,C3)
        Y2STAR=FN
        ICOUNT=ICOUNT+1
        IF(Y2STAR.LT.Y(IHI)) GO TO 10
        DO 18 J=1,NN
        DO 17 I=1,N
        P(I,J)=(P(I,J)+P(I,ILO))*0.5D0
17      XMIN(I)=P(I,J)
        CALL FUNC(N,XMIN,FN,NUMEQT,YO,C0,C1,C2,C3)
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
        CALL FUNC(N,XMIN,FN,NUMEQT,YO,C0,C1,C2,C3)
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
      SUBROUTINE READOUT(OUTFILER)
        IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(MAXNUMEQ=7)
        DIMENSION YO(150,MAXNUMEQ),PYJGX(800,1500),AB(30,2),VALFIX(20),
     1  EXX(800,3,30),YPREDPOP(800,MAXNUMEQ,150,3),
     2  YPREDBAY(800,MAXNUMEQ,150,3),
     2  CORDEN(1500,31),C0(MAXNUMEQ),C1(MAXNUMEQ),C2(MAXNUMEQ),
     3  C3(MAXNUMEQ),IPATVEC(800),YPREDPOPT(800,MAXNUMEQ,7201,3),
     4  TTPRED(800,7200),NUMT(800),TO(150),NOBS(800),RANFIXEST(20)
        DIMENSION XLOGLIK(9997),XMEAN(9997,30),
     1   STDEV(9997,30),PRCFVR(9997,30),ACTPTS(9997),SCALNFO(9997),
     2   GAMLAM(9997),OBSBLOCK(800,150,MAXNUMEQ+1),
     3   DOSEBLOCK(800,1000,35),
     3   AGE(800),HEIGHT(800),SUBMEAN(800,30),SUBLOGLIK(800),
     4   SUBSTD(800,30),SUBPERCOF(800,30),
     6   NDD(800),AICBIC(9997,2),
     7   ASSAYC(800,MAXNUMEQ,4),AF(7),NDORIG(800),
     8   BAYPOS(100,1500,31),NACTSUB(100),XVERIFY(100)
        CHARACTER PAR(30)*11,PARFIX(20)*11,READLINE*80,
     1   NAME(800)*53,CHARTNO(800)*53,SEX(800)*1,PARRANFIX(20)*11,
     2   DESCR(26)*20,OUTFILER*20,READLINE2*1000,PRIFILE*20
        CHARACTER(LEN=20) :: OSName
   	COMMON/DOSEOBS/DOSEBLOCK,OBSBLOCK,NDORIG
      COMMON/BAY/NACTSUB,BAYPOS
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
        CALL GETCOVR2(NCOV,DESCR)
        IF(NCOV .GE. 1) THEN
         DO ICOV = 1,NCOV
          WRITE(21,1717) DESCR(ICOV)
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
        IMPLICIT REAL*8(A-H,O-Z)
        PARAMETER(MAXNUMEQ=7)
        DIMENSION YO(MAXOBDIM,MAXNUMEQ),RJUNK(34),C0(MAXNUMEQ),
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
	IF(NDRUG .GT. 7) THEN
	 WRITE(*,124)
  124    FORMAT(' YOUR PATIENT DATA FILES CANNOT HAVE MORE THAN 7'/
     1' DRUGS. THE PROGRAM IS NOW STOPPING. '/)
	 CALL PAUSE
	 STOP
	ENDIF
        READ(27,3) NADD
	NI = 2*NDRUG + NADD
	IF(NI .GT. 34) THEN
  	 WRITE(*,123)
123    FORMAT(/' YOUR PATIENT DATA FILES HAVE TOO MANY COLUMNS IN '/
     1' THE DOSAGE REGIMEN BLOCK. THE NO. OF ADDITIONAL COVARIATES '/
     2' PLUS TWICE THE NO. OF DRUGS CANNOT EXCEED 34. THE PROGRAM IS'/
     3' NOW STOPPING. '/)
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
        IMPLICIT REAL*8(A-H,O-Z)
        PARAMETER(MAXNUMEQ=7)
        CHARACTER READLINE*1000,NAME(800)*53,CHARTNO(800)*53,SEX(800)*1
        DIMENSION XLOGLIK(9997),XMEAN(9997,30),AICBIC(9997,2),
     1   STDEV(9997,30),PRCFVR(9997,30),ACTPTS(9997),SCALNFO(9997),
     2   GAMLAM(9997),
     3   AGE(800),HEIGHT(800),SUBMEAN(800,30),SUBLOGLIK(800),
     4   SUBSTD(800,30),SUBPERCOF(800,30),
     6   NDD(800),ASSAYC(800,MAXNUMEQ,4)
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
	SUBROUTINE GETCOVR2(NCOV,DESCR)
	IMPLICIT REAL*8(A-H,O-Z)
	CHARACTER READLINE*1000,DESCR(26)*20
    2   FORMAT(A20)
   33   FORMAT(A1000)
	REWIND(27)
   10	READ(27,33) READLINE
	IF(READLINE(12:28) .NE. 'NO. OF ADDITIONAL') GO TO 10
	BACKSPACE(27)
    3   FORMAT(T2,I5)
        READ(27,3) NADD
	NCOV = NADD
   20	READ(27,33) READLINE
	IF(READLINE(2:16) .NE. 'COVARIATE NAMES') GO TO 20
        IF(NCOV .GE. 1) THEN
         DO J = 1,NCOV
          READ(27,33) READLINE
          DO I = 3,20
           IF(READLINE(I:I) .EQ. ' ') GO TO 30
          END DO
   30     DESCR(J) = READLINE(1:I-1)
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
	SUBROUTINE SHIFT(TAU,ND,SIG,NDRUG,NADD,RS)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION SIG(5000),RS(5000,34),TAU(7),XIV(7,5000,2),
     1  BOL(7,5000,2),COV(20,5000,2),INDIV(7),INDBOL(7),INDCOV(20),
     2  TIMCAN(34)
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
        Save XIV,BOL,COV
!$omp   Threadprivate(XIV,BOL,COV)
	DO I = 1,NDRUG
	 XIV(I,1,1) = 1.D29
	 IND = 0
	 VALAST = -99.D0
	DO IDOSE = 1,ND
	  RR = RS(IDOSE,2*I-1)
	  IF(SIG(IDOSE) .LE. 0 .AND. IDOSE .GT. 1) THEN
	    IND = IND + 1
	    XIV(I,IND,1) = 1.D19
	    XIV(I,IND,2) = XIV(I,IND-1,2)
	    IND = IND + 1
	    XIV(I,IND,1) = SIG(IDOSE)
	    XIV(I,IND,2) = RR
	    XIV(I,IND+1,1) = 1.D29
	    VALAST = RR
	    GO TO 200
	  ENDIF
	  IF(RR .NE. VALAST) THEN
         IND = IND + 1
	   XIV(I,IND,1) = SIG(IDOSE)
	   XIV(I,IND,2) = RR
	   XIV(I,IND+1,1) = 1.D29
	   VALAST = RR
	  ENDIF
  200     CONTINUE
	 END DO
	END DO
        IF(NADD .GT. 0) THEN
	DO I = 1, NADD
	 COV(I,1,1) = 1.D29
	 IND = 0
	 VALAST = -99.D0
	 DO IDOSE = 1,ND
	  RR = RS(IDOSE,2*NDRUG+I)
	  IF(SIG(IDOSE) .LE. 0 .AND. IDOSE .GT. 1) THEN
	    IND = IND + 1
	    COV(I,IND,1) = 1.D19
	    COV(I,IND,2) = COV(I,IND-1,2)
	    IND = IND + 1
	    COV(I,IND,1) = SIG(IDOSE)
	    COV(I,IND,2) = RR
	    COV(I,IND+1,1) = 1.D29
	    VALAST = RR
	    GO TO 300
	  ENDIF
	  IF(RR .NE. VALAST) THEN
           IND = IND + 1
	   COV(I,IND,1) = SIG(IDOSE)
	   COV(I,IND,2) = RR
	   COV(I,IND+1,1) = 1.D29
	   VALAST = RR
	  ENDIF
  300     CONTINUE
	 END DO
	END DO
        ENDIF
	DO I = 1,NDRUG
	 BOL(I,1,1) = 1.D29
	 IND = 0
	 DO IDOSE = 1,ND
	  RR = RS(IDOSE,2*I)
	  IF(SIG(IDOSE) .LE. 0 .AND. IDOSE .GT. 1) THEN
	    IND = IND + 1
	    BOL(I,IND,1) = 1.D19
	    BOL(I,IND,2) = 0.D0
	    IND = IND + 1
      CALL THESAME(SIG(IDOSE),0.D0,ISAME1)
      CALL THESAME(TAU(I),0.D0,ISAME2)
      CALL THESAME(RR,0.D0,ISAME3)
      IF(ISAME1 .EQ. 1) BOL(I,IND,1) = TAU(I)
      IF(ISAME1 .EQ. 0) THEN
       BOL(I,IND,1) = SIG(IDOSE)
       IF(ISAME2 .EQ. 0 .AND. ISAME3 .EQ. 0) BOL(I,IND,1) = TAU(I)
      ENDIF
	    BOL(I,IND,2) = RR
	    BOL(I,IND+1,1) = 1.D29
	    VALAST = RR
	    GO TO 400
	  ENDIF
	  IF(RR .NE. 0.D0) THEN
           IND = IND + 1
         IF(SIG(IDOSE) .GE. 0.D0) BOL(I,IND,1) = SIG(IDOSE) + TAU(I)
         IF(SIG(IDOSE) .LT. 0.D0) BOL(I,IND,1) = TAU(I)
	   BOL(I,IND,2) = RR
	   BOL(I,IND+1,1) = 1.D29
	  ENDIF
  400     CONTINUE
	 END DO
	END DO
	NI = 2*NDRUG + NADD
	ND = 0
	DO I = 1,NDRUG
	 INDIV(I) = 1
	 INDBOL(I) = 1
	END DO
        IF(NADD .GT. 0) THEN
         DO I = 1,NADD
          INDCOV(I) = 1
         END DO
        ENDIF
	TIMNXT = -9999999.D0
  100   CONTINUE
        DO I = 1,NDRUG
	 IF(XIV(I,INDIV(I),1) .GT. TIMNXT) TIMCAN(I)=XIV(I,INDIV(I),1)
	 IF(XIV(I,INDIV(I),1) .EQ. TIMNXT) TIMCAN(I)=XIV(I,INDIV(I)+1,1)
	END DO
        DO I = 1,NDRUG
	 IF(BOL(I,INDBOL(I),1) .GT. TIMNXT) TIMCAN(NDRUG+I) =
     1    BOL(I,INDBOL(I),1)
	 IF(BOL(I,INDBOL(I),1) .EQ. TIMNXT) TIMCAN(NDRUG+I) =
     1    BOL(I,INDBOL(I)+1,1)
	END DO
        IF(NADD .GT. 0) THEN
         DO I = 1,NADD
          IF(COV(I,INDCOV(I),1) .GT. TIMNXT) TIMCAN(2*NDRUG+I) =
     1     COV(I,INDCOV(I),1)
          IF(COV(I,INDCOV(I),1) .EQ. TIMNXT) TIMCAN(2*NDRUG+I) =
     1     COV(I,INDCOV(I)+1,1)
         END DO
        ENDIF
	TIMNXT = TIMCAN(1)
	DO I = 2,NI
	 IF(TIMCAN(I) .LT. TIMNXT) TIMNXT = TIMCAN(I)
	END DO
	IF(TIMNXT .EQ. 1.D29) RETURN
	IF(TIMNXT .EQ. 1.D19) THEN
       DO I = 1,NDRUG
	  INDIV(I) = INDIV(I) + 1
	  INDBOL(I) = INDBOL(I) + 1
	 END DO
        IF(NADD .GT. 0) THEN
         DO I = 1,NADD
          INDCOV(I) = INDCOV(I) + 1
         END DO
        ENDIF
	 TIMNXT = -9999999.D0
	 GO TO 100
	ENDIF
	ND = ND+1
	IF(ND .GT. 5000) THEN
   10	 WRITE(*,1) ND
    1    FORMAT(/' THE NUMBER OF DOSE EVENTS, AFTER TAKING INTO'/
     1' ACCOUNT DIFFERING TIMES DUE TO TIMELAGS IS ',I6,', MORE THAN'/
     2' THE ALLOWABLE MAXIMUM OF 5000. THE PROGRAM IS STOPPING. PLEASE'/
     3' RERUN WITH PATIENTS HAVING FEWER DOSE EVENTS, OR WITH FEWER'/
     4' TIMELAG VALUES SELECTED AS FIXED OR RANDOM PARAMETERS.'//)
	 STOP
	ENDIF
	SIG(ND) = TIMNXT
        DO I = 1,NDRUG
	 IF(TIMNXT .LT. XIV(I,INDIV(I),1)) THEN
	  RS(ND,2*I-1) = RS(ND-1,2*I-1)
	 ENDIF
	 IF(TIMNXT .EQ. XIV(I,INDIV(I),1)) THEN
	  RS(ND,2*I-1) = XIV(I,INDIV(I),2)
	  INDIV(I) = INDIV(I) + 1
	 ENDIF
	 IF(TIMNXT .LT. BOL(I,INDBOL(I),1)) THEN
	  RS(ND,2*I) = 0.D0
	 ENDIF
	 IF(TIMNXT .EQ. BOL(I,INDBOL(I),1)) THEN
	  RS(ND,2*I) = BOL(I,INDBOL(I),2)
	  INDBOL(I) = INDBOL(I) + 1
	 ENDIF
	END DO
        IF(NADD .GT. 0) THEN
         DO I = 1,NADD
          IF(TIMNXT .LT. COV(I,INDCOV(I),1))
     1     RS(ND,2*NDRUG+I) = RS(ND-1,2*NDRUG+I)
          IF(TIMNXT .EQ. COV(I,INDCOV(I),1)) THEN
           RS(ND,2*NDRUG+I) = COV(I,INDCOV(I),2)
           INDCOV(I) = INDCOV(I) + 1
          ENDIF
         END DO
        ENDIF
	GO TO 100
	END
