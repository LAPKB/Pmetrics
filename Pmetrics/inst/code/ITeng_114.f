	SUBROUTINE IDPC(JSUB,IG,X,SUMSQJ,INTLIST,RPAR,IPAR)
        IMPLICIT REAL*8(A-H,O-Z)
        PARAMETER(MAXNUMEQ=7)
        DIMENSION X(32),P(32),SUMSQJ(MAXNUMEQ)
        COMMON/CNST/ N,ND,NI,NUP,NUIC,NP
        COMMON/PARAMD/ P
        COMMON/BOLUSCOMP/NBCOMP
        integer, dimension(7) :: NBCOMP
        integer JSUB,IG,JSUBmain
        integer, dimension(128) :: INTLIST
        double precision, dimension(257) :: RPAR
        integer, dimension(257) :: IPAR
        JSUBmain = JSUB
	CALL SYMBOL(NBCOMP)
        DO I=1,NP
         P(I)=X(I)
        END DO
        CALL SUMSQ(JSUB,IG,SUMSQJ,INTLIST,RPAR,IPAR)
        JSUB = JSUBmain
        RETURN
        END
      SUBROUTINE FUNC(JSUB,IG,M,SUMSQJ,INTLIST,RPAR,IPAR)
       use npag_utils, only: thesame, shift,
     1  max_ODE_params, k_dvode_reserved,
     2  max_RS_J,k_p_end,k_jsub,k_ig,i_dvode_reserved,
     3  i_jsub,i_ig
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(MAXNUMEQ=7)
      COMMON/BOLUSCOMP/NBCOMP
      COMMON/OBSER/ TIM,SIG,RS,YO,BS
      COMMON/CNST/ N,ND,NI,NUP,NUIC,NP
      COMMON/INPUT/ R,B
      COMMON/PARAMD/ P
      COMMON/CNST2/ NPL,NOS,NDRUG,NADD
      COMMON/STATE/ X
      COMMON STDEV
      COMMON/ERR/ERRFIL
      DIMENSION X(20),P(32),TIM(594),SIG(5000),SIGO(5000),R(37),
     1 RS(5000,34),RSO(5000,34),YT(MAXNUMEQ),YO(594,MAXNUMEQ),
     2 BS(5000,7),Y(594,MAXNUMEQ),B(20),NBCOMP(7),STDEV(594,MAXNUMEQ),
     3 FA(7),TLAG(7),SUMSQJ(MAXNUMEQ),XSTORE(100,20),XPRED(20),
     4 XVERIFY(100)
      CHARACTER ERRFIL*20
       double precision, dimension(257) :: RPAR
       integer, dimension(257) :: IPAR
       integer, dimension(128) :: INTLIST
       integer JSUB,IG,III,JSUBmain
        JSUBmain = JSUB
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
	 CALL GETFA(FA,X,P,R,B,INTLIST)
	 NDO = ND
	 DO I=1,ND
	  SIGO(I) = SIG(I)
	  DO J=1,NI
	   RSO(I,J) = RS(I,J)
	  END DO
	 END DO
        IF(N .EQ. 0) GO TO 75
	 CALL GETIX(N,X,P,R,B,INTLIST)
   75	 CALL GETTLAG(TLAG,X,P,R,B,INTLIST)
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
	 CALL GETFA(FA,X,P,R,B,INTLIST)
      IF(N .EQ. 0) GO TO 120
       DO I=1,NDRUG
       X(NBCOMP(I))=X(NBCOMP(I))+BS(KNS,I)*FA(I)
      END DO
      GO TO 81
120   DO I=1,NDRUG
       B(I)=BS(KNS,I)*FA(I)
      END DO
81    KNS=KNS+1
45    IF(KNS .GT. ND) GO TO 15
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
        CALL GETIX(N,X,P,R,B,INTLIST)
       T = 0.D0
      ISTEADY = 0
      IF(SIG(KNS) .LT. 0.D0) THEN
       ISTEADY = 1
       NSET = 1
       DOSEINT = -SIG(KNS)
       SIG(KNS) = 0
      ENDIF
	ENDIF
      IF(TIM(KNT).NE.SIG(KNS)) GO TO 20
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
32      IF(N .NE. -1) CALL USERANAL(X,T,TOUT,RPAR,IPAR)
        IF(N .EQ. -1) CALL ANAL3(X,T,TOUT,RPAR,IPAR)
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
        IF(ID .EQ. 1) GO TO 35
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
	 CALL GETFA(FA,X,P,R,B,INTLIST)
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
         SUMSQJ(J) = 0.D0
         DO I=1,M
          IF(YO(I,J) .NE. -99) SUMSQJ(J) = SUMSQJ(J) +
     1                         ((Y(I,J)-YO(I,J))/STDEV(I,J))**2.D0
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
        JSUB = JSUBmain
      RETURN
      END
        SUBROUTINE SUMSQ(JSUB,IG,SUMSQJ,INTLIST,RPAR,IPAR)
        IMPLICIT REAL*8(A-H,O-Z)
        PARAMETER(MAXNUMEQ=7)
        COMMON/SUM2/ M,NPNL
        COMMON/CNST2/ NPL,NOS,NBI,NRI
        DIMENSION SUMSQJ(MAXNUMEQ)
        integer JSUB,IG, JSUBmain
        integer, dimension(128)::INTLIST
        double precision, dimension(257) :: RPAR
        integer, dimension(257) :: IPAR
        JSUBmain = JSUB
        CALL FUNC(JSUB,IG,M,SUMSQJ,INTLIST,RPAR,IPAR)
        JSUB = JSUBmain
        RETURN
        END
        SUBROUTINE USERANAL(X,TIN,TOUT,RPAR,IPAR)
        IMPLICIT REAL*8(A-H,O-Z)
        DIMENSION X(20),ATOL(20),RWORK(1002),IWORK(50)
        DIMENSION RPAR(257)
        DIMENSION IPAR(257)
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
	SUBROUTINE JACOB(NDIM, T, X, ML, MU, PD, NRPD, RPAR, IPAR)
	IMPLICIT REAL*8(A-H,O-Z)
        COMMON/PARAMD/ P
        COMMON/INPUT/ R,B
        DIMENSION X(NDIM), PD(NRPD,NDIM), P(32),R(37),B(20)
        RETURN
        END
      SUBROUTINE PREDLAST3(NN,NSET,XSTORE,XPRED,ICONV)
      use npag_utils, only : thesame
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XSTORE(100,20),XPRED(20),COMP(5,20)
      REAL*8 F,DEL1,DEL2,TOL1,TOL2,A1,A2,A3
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
	SUBROUTINE IDCALCP(JSUB,IG,NVAR,NOFIX,NRANFIX,IRAN,NDIM,
     1    ESTML,PMAT,INTLIST,RPAR,IPAR)
        IMPLICIT REAL*8(A-H,O-Z)
        PARAMETER(MAXNUMEQ=7)
        DIMENSION ESTML(32),PMAT(594,30),YEST(594),Y(3564),
     1  IRAN(32),P(32),TIM(594),SIG(5000),RS(5000,34),YO(594,MAXNUMEQ),
     2  BS(5000,7)
        COMMON/CNST/ N,ND,NI,NUP,NUIC,NP
        COMMON/PARAMD/ P
        COMMON/OBSER/ TIM,SIG,RS,YO,BS
        COMMON/CNST2/ NPL,NOS,NDRUG,NAD
        COMMON/SUM2/ M,NPNL
        COMMON/BOLUSCOMP/NBCOMP
        integer, dimension(7) :: NBCOMP
        integer JSUB,IG,JSUBmain,IGmain
        integer,dimension(128)::INTLIST
        double precision,dimension(257)::RPAR
        integer,dimension(257)::IPAR
        JSUBmain = JSUB
        IGmain = IG
	CALL SYMBOL(NBCOMP)
	N = NDIM
	NP = NVAR+NOFIX+NRANFIX
	JCOL = 0
	DO 5000 JSIM = 0,NP
       DO I = 1,NP
        P(I)=ESTML(I)
       END DO
	IF(JSIM .GT. 0) THEN
	 IF(IRAN(JSIM) .EQ. 0 .OR. IRAN(JSIM) .EQ. 2) GO TO 5000
     	 DELJ=ESTML(JSIM)*1.D-4
	 P(JSIM)=ESTML(JSIM) + DELJ
	ENDIF
	CALL EVAL(JSUB,IG,Y,INTLIST,RPAR,IPAR)
	NDEX = 0
	IF(JSIM .EQ. 0) THEN
	 DO J=1,NOS
	  DO I=1,M
	   IF(YO(I,J) .NE. -99) THEN
	    NDEX = NDEX + 1
	    YEST(NDEX) = Y((J-1)*M+I)
	   ENDIF
	  END DO
	 END DO
	ENDIF
	IF(JSIM .GT. 0) THEN
	 JCOL = JCOL+1
	 DO J=1,NOS
	  DO I=1,M
	   IF(YO(I,J) .NE. -99) THEN
	    NDEX = NDEX + 1
	    PMAT(NDEX,JCOL) = (Y((J-1)*M+I) - YEST(NDEX))/DELJ
	   ENDIF
	  END DO
	 END DO
	ENDIF
 5000   CONTINUE
        JSUB = JSUBmain
        IG = IGmain
        RETURN
        END
      SUBROUTINE FUNC1(JSUB,IG,M,F,INTLIST,RPAR,IPAR)
       use npag_utils, only: thesame, shift,
     1  max_ODE_params, k_dvode_reserved,
     2  max_RS_J,k_p_end,k_jsub,k_ig,i_dvode_reserved,
     3  i_jsub,i_ig
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
      DIMENSION X(20),P(32),TIM(594),SIG(5000),SIGO(5000),R(37),
     1 RS(5000,34),RSO(5000,34),YT(MAXNUMEQ),YO(594,MAXNUMEQ),F(3564),
     2 BS(5000,7),Y(594,MAXNUMEQ),B(20),NBCOMP(7),TLAG(7),FA(7),
     3 XSTORE(100,20),XPRED(20),XVERIFY(100)
      CHARACTER ERRFIL*20
       double precision, dimension(257) :: RPAR
       integer, dimension(257) :: IPAR
       integer, dimension(128) :: INTLIST
       integer III,JSUB,IG,JSUBmain,IGmain
        JSUBmain = JSUB
        IGmain = IG
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
	 CALL GETFA(FA,X,P,R,B,INTLIST)
	 NDO = ND
	 DO I=1,ND
	  SIGO(I) = SIG(I)
	  DO J=1,NI
	   RSO(I,J) = RS(I,J)
	  END DO
	 END DO
        IF(N .EQ. 0) GO TO 75
	 CALL GETIX(N,X,P,R,B,INTLIST)
   75	 CALL GETTLAG(TLAG,X,P,R,B,INTLIST)
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
2000  Y(KNT,I)=YT(I)
      KNT=KNT+1
      GO TO 45
12    IF(TIM(KNT).GT.SIG(KNS)) GO TO 13
	IF(TIM(KNT).NE.0.0D0) GO TO 45
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
	 CALL GETFA(FA,X,P,R,B,INTLIST)
      IF(N .EQ. 0) GO TO 120
       DO I=1,NDRUG
       X(NBCOMP(I))=X(NBCOMP(I))+BS(KNS,I)*FA(I)
      END DO
      GO TO 81
120   DO I=1,NDRUG
       B(I)=BS(KNS,I)*FA(I)
      END DO
81    KNS=KNS+1
45    IF(KNS.GT.ND) GO TO 15
      IF(TIM(KNT) .EQ. 0.D0 .AND. KNT .GT. 1) THEN
      DO IKNS = KNS,ND
       IF(SIG(IKNS) .LE. 0.D0) GO TO 110
      END DO
       XVERIFY(1) = SIG(KNS)
       CALL VERIFYVAL(1,XVERIFY)
       WRITE(*,111) ND,KNS,XVERIFY(1)
       WRITE(25,111) ND,KNS,XVERIFY(1)
111   FORMAT(//' THE CURRENT SUBJECT HAS AN OBSERVATION TIME RESET'/
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
        CALL GETIX(N,X,P,R,B,INTLIST)
       T = 0.D0
      ISTEADY = 0
      IF(SIG(KNS) .LT. 0.D0) THEN
       ISTEADY = 1
       NSET = 1
       DOSEINT = -SIG(KNS)
       SIG(KNS) = 0
      ENDIF
	ENDIF
      IF(TIM(KNT).NE.SIG(KNS)) GO TO 20
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
          RPAR(k_dvode_reserved + III) = P(III)
        end do
        do III = 1,max_RS_J
          RPAR(k_p_end + III) = R(III)
        end do
        RPAR(k_jsub) = dble(JSUBmain)
        RPAR(k_ig) = dble(IG)
        do III = 1,10
          IPAR(i_dvode_reserved + III) = INTLIST(III)
        end do
        IPAR(i_jsub) = JSUBmain
        IPAR(i_ig) = IG
32      IF(N .NE. -1) CALL USERANAL(X,T,TOUT)
        IF(N .EQ. -1) CALL ANAL3(X,T,TOUT,RPAR,IPAR)
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
	 CALL GETFA(FA,X,P,R,B,INTLIST)
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
	  F((J-1)*M+I)=Y(I,J)
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
         JSUB = JSUBmain
         IG = IGmain
      RETURN
      END
	SUBROUTINE EVAL(JSUB,IG,F,INTLIST,RPAR,IPAR)
	IMPLICIT REAL*8(A-H,O-Z)
	COMMON/SUM2/ M,NPNL
        COMMON/CNST2/ NPL,NOS,NDRUG,NADD
	DIMENSION F(3564)
        integer JSUB,IG,JSUBmain,IGmain
        integer,dimension(128)::INTLIST
        double precision,dimension(257)::RPAR
        integer,dimension(257)::IPAR
        JSUBmain = JSUB
        IGmain = IG
	CALL FUNC1(JSUB,IG,M,F,INTLIST,RPAR,IPAR)
        JSUB = JSUBmain
        IG = IGmain
	RETURN
	END
	SUBROUTINE IDCALCY(JSUB,IG,NPP,NDIM,ESTML,YPRED,NUMEQT,
     1    INTLIST,RPAR,IPAR)
        IMPLICIT REAL*8(A-H,O-Z)
        DIMENSION ESTML(32),YPRED(594,NUMEQT),P(32)
        COMMON/CNST/ N,ND,NI,NUP,NUIC,NP
        COMMON/PARAMD/ P
        COMMON/BOLUSCOMP/NBCOMP
        integer, dimension(7) :: NBCOMP
        integer JSUB,IG,JSUBmain,IGmain
        integer, dimension(128)::INTLIST
        double precision, dimension(257)::RPAR
        integer, dimension(257)::IPAR
        JSUBmain = JSUB
        IGmain = IG
	CALL SYMBOL(NBCOMP)
	N = NDIM
	NP = NPP
        DO I=1,NP
	  P(I) = ESTML(I)
	END DO
	CALL EVAL2(JSUB,IG,YPRED,NUMEQT,INTLIST,RPAR,IPAR)
        JSUB = JSUBmain
        IG = IGmain
        RETURN
	END
	SUBROUTINE FUNC2(JSUB,IG,M,YPRED,NUMEQT,INTLIST,RPAR,IPAR)
       use npag_utils, only: thesame, shift,
     1  max_ODE_params, k_dvode_reserved,
     2  max_RS_J,k_p_end,k_jsub,k_ig,i_dvode_reserved,
     3  i_jsub,i_ig
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
      DIMENSION X(20),P(32),TIM(594),SIG(5000),SIGO(5000),R(37),
     1 RS(5000,34),RSO(5000,34),YT(MAXNUMEQ),YO(594,MAXNUMEQ),
     2 YPRED(594,NUMEQT), BS(5000,7),Y(594,MAXNUMEQ),B(20),NBCOMP(7),
     3 TLAG(7),FA(7),XSTORE(100,20),XPRED(20),XVERIFY(100)
      CHARACTER ERRFIL*20
       double precision, dimension(257) :: RPAR
       integer, dimension(257) :: IPAR
       integer, dimension(128) :: INTLIST
       integer III,JSUB,IG,JSUBmain,IGmain
      JSUBmain = JSUB
      IGmain = IG
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
	 CALL GETFA(FA,X,P,R,B,INTLIST)
	 NDO = ND
	 DO I=1,ND
	  SIGO(I) = SIG(I)
	  DO J=1,NI
	   RSO(I,J) = RS(I,J)
	  END DO
	 END DO
        IF(N .EQ. 0) GO TO 75
	 CALL GETIX(N,X,P,R,B,INTLIST)
   75	 CALL GETTLAG(TLAG,X,P,R,B,INTLIST)
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
	 CALL GETFA(FA,X,P,R,B,INTLIST)
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
       CALL GETIX(N,X,P,R,B,INTLIST)
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
32      IF(N .NE. -1) CALL USERANAL(X,T,TOUT)
        IF(N .EQ. -1) CALL ANAL3(X,T,TOUT,RPAR,IPAR)
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
	 CALL GETFA(FA,X,P,B,INTLIST)
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
      JSUB = JSUBmain
      IG = IGmain
      RETURN
      END
	SUBROUTINE EVAL2(JSUB,IG,YPRED,NUMEQT,INTLIST,RPAR,IPAR)
	IMPLICIT REAL*8(A-H,O-Z)
	COMMON/SUM2/ M,NPNL
        COMMON/CNST2/ NPL,NOS,NDRUG,NADD
	DIMENSION YPRED(594,NUMEQT)
        integer jsub,ig,JSUBmain,IGmain
        integer, dimension(128)::INTLIST
        double precision, dimension(257)::RPAR
        integer, dimension(257)::IPAR
        JSUBmain = JSUB
        IGmain = IG
	CALL FUNC2(JSUB,IG,M,YPRED,NUMEQT,INTLIST,RPAR,IPAR)
        JSUB = JSUBmain
        IG = IGmain
	RETURN
	END
	SUBROUTINE IT2B(MAXSUB,MAXDIM,MAXGRD,NUMEQT,MAXOBDIM,PAREST,
     1  IESTIJ,IREPRT,WORK,CORDEN,PXGEE,SPXGYJ,YPREDPOP,YPREDBAY,PARBAY,
     2  VEC)
        use npag_utils, only : makevec
        IMPLICIT REAL*8(A-H,O-Z)
        PARAMETER(MAXNUMEQ=7)
    	DIMENSION PAREST(MAXSUB,MAXDIM),IESTIJ(MAXSUB,MAXDIM),
     1  IREPRT(MAXSUB),WORK(MAXGRD),CORDEN(MAXGRD,MAXDIM+1),
     2  PXGEE(MAXGRD),SPXGYJ(MAXGRD),YPREDPOP(MAXSUB,NUMEQT,MAXOBDIM,2),
     3  YPREDBAY(MAXSUB,NUMEQT,MAXOBDIM,2),PARBAY(MAXSUB,2,MAXDIM),
     4  VEC(MAXSUB),XVERIFY(100)
      DIMENSION YO(594,NUMEQT),SIG(594,MAXNUMEQ),AB(30,2),VALFIX(20),
     1 START(30),STEP(30),PMAT(594,30),ESTCOV(30,30),THETA(30),
     2 ESTMEN(30),PIK(30,30),COVINV(30,30),ESTINV(30,30),IRAN(32),
     3 PX(32),B(30,1),SUM(30),SUMCOV(30,30),DIFF(30),DIFPRD(30,30),
     4 CORR(30,30),STDEV(30),ESTMED(30),SUMCOL(30),X(30),ATOL(20),
     5 YPRED(594,NUMEQT),C0P(NUMEQT),C1P(NUMEQT),C2P(NUMEQT),
     6 C3P(NUMEQT),C0(NUMEQT),C1(NUMEQT),C2(NUMEQT),C3(NUMEQT),
     7 GAMMA(NUMEQT),IGAMMA(NUMEQT),SUMDIF(NUMEQT),NOBTOT(NUMEQT),
     8 MISVAL(NUMEQT),RINV(594,594),SSND(MAXNUMEQ),WEQ(NUMEQT),
     9 IPATVEC(9999),AF(7),RANFIXEST(20),IIRAN(32),OPTVAR(32),
     1 ESTMENO(32),C4P(NUMEQT),C5P(NUMEQT),C4(NUMEQT),C5(NUMEQT)
      CHARACTER PREFIX*5,PAR(30)*11,EXT*3,CSVFILE*20,
     2 PARFIX(20)*11,OUTFIL*20,PARFIL*20,FROMFIL*20,NAME*4,READLINE*300,
     3 DENFIL*20,OUTCOM*20,READLARG*1000,OUTFILER*20,ERRFIL*20,ANS*3,
     4 PARRANFIX(20)*11
        COMMON SIG
        COMMON/TOUSER/NDIM,MF,RTOL,ATOL
        COMMON/TOMAP/IRAN,VALFIX,SSND,SIGFAC,OFAC,ESTMEN,ESTINV,
     1   DET,NOFIX,NUMEQTT,RANFIXEST,NRANFIX
        COMMON/ERR/ERRFIL
      COMMON/TOCALC/IIRAN,PX,NNOFIX,NSUB
        COMMON/CNST/N,ND,NI,NUP,NUIC,NP
        integer JSUB, IG
        double precision AVGLOG,SUMLOG,VALMIN
        integer, dimension(128) :: INTLIST
        double precision, dimension(257) :: RPAR
        integer, dimension(257) :: IPAR
        EXTERNAL MAPBAYS
        EXTERNAL CALCRF
        NUMEQTT = NUMEQT
    2 FORMAT(A20)
  222 FORMAT(A3)
 2222 FORMAT(A5)
 2227 FORMAT(A11)
	OPEN(25,FILE='extnum',STATUS='OLD')
	READ(25,*) INUM
	CALL EQUIV(INUM,NAME)
	JNUM=INUM+1
	IF(JNUM .EQ. 10000) JNUM=1
	BACKSPACE(25)
	WRITE(25,*) JNUM
	CLOSE(25)
	OUTFIL = 'OUFF'//NAME
	PARFIL = 'LAST'//NAME
	FROMFIL = 'FROM'//NAME
	DENFIL = 'DENF'//NAME
      ERRFIL = 'ERROR'//NAME
        OPEN(23,FILE='it2b102.inp',ERR=4705,STATUS='OLD')
        GO TO 4710
 4705	  WRITE(*,4706)
 4706   FORMAT(/' INPUT FILE it2b102.inp IS NOT AVAILABLE. THE'/
     1' PROGRAM STOPS. TRY AGAIN AFTER RUNNING THE PREPARATION PROGRAM'/
     2' TO CREATE it2b102.inp, AND THEN PUTTING THAT FILE IN THE '/
     3' WORKING DIRECTORY.'/)
        OPEN(42,FILE=ERRFIL)
         WRITE(42,4706)
        CLOSE(42)
        CALL PAUSE
        STOP
 4710	  READ(23,*) NDIM
        N = NDIM
        READ(23,*) MF
        READ(23,*) RTOL
        READ(23,*) (ATOL(I),I=1,NDIM)
        READ(23,2222) PREFIX
        READ(23,222) EXT
        READ(23,2) CSVFILE
        READ(23,*) NVAR
        READ(23,2227) (PAR(I),I=1,NVAR)
        READ(23,*) NOFIX
        READ(23,2227) (PARFIX(I),I=1,NOFIX)
       NNOFIX = NOFIX
        READ(23,*) NRANFIX
        READ(23,2227) (PARRANFIX(I),I=1,NRANFIX)
        READ(23,*) (IRAN(I),I=1,NVAR+NOFIX+NRANFIX)
        DO I = 1,NVAR+NOFIX+NRANFIX
         IIRAN(I) = IRAN(I)
        END DO
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
        DO I=1,NUMEQT
         READ(23,*) IGAMMA(I),C0P(I),C1P(I),C2P(I),C3P(I),C4P(I),C5P(I)
        END DO
        READ(23,*) XSIG
        READ(23,*) QVAL
        READ(23,*) NDRUG
        READ(23,*) (AF(I),I=1,NDRUG)
        READ(23,*) TOL
        READ(23,*) MAXIT
        READ(23,*) XDEV
        READ(23,*)
        READ(23,*) INDPTS
        READ(23,*) ILOG
        OPEN(27)
 1717   FORMAT(A300)
        NLAFIR = 0
        DO JSUB=1,NSUB
          IG=JSUB
 1720   READ(23,1717,IOSTAT=IEND) READLINE
        IF(IEND .LT. 0) THEN
	  WRITE(*,1721)
 1721   FORMAT(/' PATIENT DATA INFORMATION WAS NOT READ CORRECTLY'/
     1' FROM THE INSTRUCTION FILE, it2b102.inp. IF YOU EDITED THIS'/
     2' FILE MANUALLY, PLEASE RERUN THE PC PREP PROGRAM TO HAVE IT'/
     3' PREPARE it2b102.inp AGAIN AND THEN RERUN THIS PROGRAM.'//
     4' IF YOU DID NOT MANUALLY EDIT it2b102.inp, PLEASE SEND THE'/
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
       CALL NEWWORK1(JSUB)
        END DO
 1730   REWIND(27)
        CLOSE(23)
	 IF(JSUB .LT. NSUB) THEN
	  WRITE(*,1721)
        OPEN(42,FILE=ERRFIL)
         WRITE(42,1721)
        CLOSE(42)
	  CALL PAUSE
	  STOP
	 ENDIF
	OPEN(25,FILE=OUTFIL)
	  OPEN(24,FILE=FROMFIL)
	    NEGONE = -1
	    WRITE(24,*) NEGONE
	  CLOSE(24)
        WRITE(*,9801)
 9801   FORMAT(//' THIS FRONT-END PROGRAM PERFORMS AN'//
     1'                    ITERATIVE  BAYESIAN  ANALYSIS'//
     2' ON SUBJECT DATA FILES TO OBTAIN THE INDIVIDUAL PARAMETER '/
     3' ESTIMATES FOR EACH SUBJECT, AS WELL AS THE INITIAL RANGES FOR'/
     4' THESE PARAMETERS--WHICH WILL BE USED BY THE 2ND PART OF THIS '/
     5' PROGRAM, THE NPAG POPULATION ANALYZER.'//)
        WRITE(25,7124)
 7124   FORMAT('  REM_FRN MAR_16 ... made by it2branfix1.f')
        WRITE(25,1212)
 1212   FORMAT(/' THE NEXT FEW LINES GIVE INPUT INFO FOR THIS RUN: '/)
        WRITE(25,9761) PREFIX,EXT
9761    FORMAT(/' THE SUBJ. FILENAMES (IN "Adapt-Like" FORMAT) HAVE'/
     1' PREFIX ',A5,' AND EXT. ',A3)
        WRITE(25,9861)
 9861   FORMAT(/' THE SUBJECT INFORMATION IS STORED IN THE .CSV FILE,'/)
        WRITE(25,2) CSVFILE
        WRITE(25,9767) NSUBTOT
 9767   FORMAT(/' THE TOTAL NO. OF SUBJECTS IN THE DATA SET IS ',I4)
        WRITE(25,9762) NSUB
 9762   FORMAT(/' THE NO. OF SUBJECTS IS          ',I3)
        CALL WRITEPT2(25,NSUB,IPATVEC)
        WRITE(25,*) '    0'
        WRITE(25,1012) NVAR
 1012   FORMAT(/' THE NO. OF RANDOM VARIABLES IS ',I2)
      WRITE(25,2239)
 2239 FORMAT(' THE RANDOM VARIABLES AND THEIR BOUNDARIES, AND IF THE EST
     1. MUST BE .GE. 0')
      NP = NVAR + NOFIX + NRANFIX
      NORAN = 0
      DO I = 1,NP
       IF(IRAN(I) .EQ. 0 .OR. IRAN(I) .EQ. 2) GO TO 2220
       NORAN = NORAN + 1
       IF(IRAN(I) .EQ. 1) ANS = 'YES'
       IF(IRAN(I) .EQ. -1) ANS = 'NO '
       XVERIFY(1) = AB(NORAN,1)
       XVERIFY(2) = AB(NORAN,2)
       CALL VERIFYVAL(2,XVERIFY)
       WRITE(25,2217) PAR(NORAN),XVERIFY(1),XVERIFY(2),ANS
 2220  CONTINUE
      END DO
2217   FORMAT(/' ',A11,': ',G17.10,'   TO   ',G17.10,5X,3A)
        WRITE(25,*)
        IF(NOFIX .EQ. 0) WRITE(25,9764)
 9764   FORMAT(/' NO FIXED PARAMETER VALUES.')
	IF(NOFIX .GE. 1) THEN
       WRITE(25,9766)
 9766  FORMAT(/' THE USER-ENTERED FIXED PARAMETER VALUE(S) IS (ARE):')
	DO I=1,NOFIX
       XVERIFY(1) = VALFIX(I)
       CALL VERIFYVAL(1,XVERIFY)
       WRITE(25,2219) PARFIX(I),XVERIFY(1)
	END DO
 2219   FORMAT(/' ',A11,' =  ',G17.10)
	WRITE(25,*)
	ENDIF
      IF(NRANFIX .EQ. 0) WRITE(25,9789)
 9789 FORMAT(/' NO "RANFIX" PARAMETER ESTIMATES.')
  	IF(NRANFIX.GT. 0) THEN
       WRITE(25,9678)
 9678  FORMAT(/' THE USER-ENTERED INITIAL ESTIMATES FOR THE PARAMETERS'/
     1' WHICH ARE UNKNOWN, BUT THE SAME FOR ALL SUBJECTS, IS (ARE):')
      DO I=1,NRANFIX
       XVERIFY(1) = RANFIXEST(I)
       CALL VERIFYVAL(1,XVERIFY)
       WRITE(25,2219) PARRANFIX(I),XVERIFY(1)
      END DO
	WRITE(25,*)
  	ENDIF
       XVERIFY(1) = XSIG
       CALL VERIFYVAL(1,XVERIFY)
       WRITE(25,1223) XVERIFY(1)
 1223   FORMAT(/' THE INITIAL STD. DEVS. ARE ',G12.6,' x  THE RANGES.')
	DO IEQ=1,NUMEQT
	 IF(IGAMMA(IEQ) .EQ. 1) WRITE(25,1222) IEQ
	 IF(IGAMMA(IEQ) .EQ. 0) WRITE(25,1221) IEQ
	END DO
 1222   FORMAT(/' GAMMA FOR OUTPUT EQ. ',I2,' WILL BE FIXED AT 1.0')
 1221   FORMAT(/' GAMMA FOR OUTPUT EQ. ',I2,' WILL BE ESTIMATED.')
	WRITE(25,2218) NUMEQT
 2218   FORMAT(/' THE POPULATION VALUES FOR [C0,C1,C2,C3] FOR EACH OF '/
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
	END DO
  162   FORMAT(' EQ. ',I2,': ',4(G16.10,1X))
        WRITE(25,1231) NDRUG
 1231   FORMAT(/' THE NO. OF DRUGS IS '/
     1' ',I2)
        WRITE(25,1229) NDRUG
 1229   FORMAT(/' THE ACTIVE (SALT) FRACTION(S) FOR THE ',I2,' DRUG(S)'/
     1' FOLLOW, IN ORDER: ')
        WRITE(25,*) (AF(I),I=1,NDRUG)
        WRITE(25,9768)
9768   FORMAT(/' THE STOPPING TOLERANCE IS ')
        WRITE(25,*) TOL
        WRITE(25,9772)
 9772   FORMAT(/' THE MAXIMUM NO. OF ITERATIONS IS ')
        WRITE(25,*) MAXIT
        WRITE(25,9773)
 9773   FORMAT(/' EACH PARAMETER RANGE FOR NPAG WILL BE, NOMINALLY, '/
     1' THE ASSOCIATED MEAN +/- XDEV x THE ASSOCIATED STD. DEVIATION,'/
     2' WHERE XDEV IS ')
        WRITE(25,*) XDEV
        WRITE(25,9769)
 9769   FORMAT(/' THE TOLERANCE PARAMETER USED BY THE DIFFERENTIAL'/
     1' EQUATION SOLVER (VODE), IS: ')
        WRITE(25,*) RTOL
       XVERIFY(1) = QVAL
       CALL VERIFYVAL(1,XVERIFY)
       WRITE(25,9771) XVERIFY(1)
 9771   FORMAT(/' THE VALUE OF QVAL IS ',F5.1)
	WRITE(25,1213)
 1213   FORMAT(////' THE FOLLOWING IS THE OUTPUT FROM THE RUNNING OF'/
     1' THE PROGRAM.'///)
	DO I=1,NVAR
	 ESTMEN(I) = (AB(I,1)+AB(I,2))/2.D0
	END DO
	DO I=1,NVAR
	DO J=1,NVAR
	ESTCOV(I,J) = 0.D0
	END DO
	SIGI = XSIG*(AB(I,2)-AB(I,1))
	ESTCOV(I,I) = SIGI*SIGI
	END DO
	WRITE(*,5101) NVAR
	WRITE(25,5101) NVAR
 5101   FORMAT(//' THE INITIAL POPULATION ESTIMATES ARE SHOWN BELOW'/
     1' (THE 1ST ROW GIVES THE MEANS; THE NEXT ',I1,' ROWS GIVE THE '/
     2' COVARIANCE MATRIX):'/)
	WRITE(*,5102) (PAR(I),I=1,NVAR)
	WRITE(25,5102) (PAR(I),I=1,NVAR)
5102   FORMAT(5X,30(A11,2X))
      DO I = 1,NVAR
       XVERIFY(I) = ESTMEN(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)
      WRITE(*,5103) (XVERIFY(I),I=1,NVAR)
      WRITE(25,5103) (XVERIFY(I),I=1,NVAR)
5103   FORMAT(1X,30(G12.6,1X))
      WRITE(*,*)
      WRITE(25,*)
      WRITE(*,5102) (PAR(I),I=1,NVAR)
      WRITE(25,5102) (PAR(I),I=1,NVAR)
      DO I=1,NVAR
       DO J = 1,I
        XVERIFY(J) = ESTCOV(I,J)
       END DO
       CALL VERIFYVAL(I,XVERIFY)
       WRITE(*,5103) (XVERIFY(J),J=1,I)
       WRITE(25,5103) (XVERIFY(J),J=1,I)
      END DO
	DO IEQ=1,NUMEQT
	 GAMMA(IEQ) = 1.D0
	END DO
	REWIND(27)
	WRITE(*,701)
  701   FORMAT(//' CHECKING EACH SUBJECT FOR ADEQUATE INFORMATION TO'/
     1' ESTIMATE THE PARAMETERS.'//)
	IRPRT = 1
	DO I=1,NSUB
	 DO J=1,NVAR
	  IESTIJ(I,J)=1
	 END DO
	 IREPRT(I)=1
	END DO
	DO IEQ=1,NUMEQT
	 NOBTOT(IEQ) = 0
	END DO
	DO 700 JSUB = 1, NSUB, 1
            IG = JSUB
	WRITE(*,*) "DO 700:", JSUB
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)
	MAXTOT = NUMEQT*NOBSER
	DO I =1,MAXTOT
	 DO J=1,MAXTOT
	  RINV(I,J) = 0.D0
	 END DO
	END DO
        NDEX = 0
        DO IEQ=1,NUMEQT
          MISVAL(IEQ) = 0
        END DO
        SIGFAC=1.D0
      DO 140 IEQ=1,NUMEQT
       DO 140 I=1,NOBSER
        Y=YO(I,IEQ)
	  IF(Y .EQ. -99) THEN
	   MISVAL(IEQ) = MISVAL(IEQ) + 1
	   GO TO 140
	  ENDIF
      SIG(I,IEQ)=GAMMA(IEQ)*(C0(IEQ)+C1(IEQ)*Y+C2(IEQ)*Y*Y+C3(IEQ)*Y**3)
      IF(SIG(I,IEQ) .EQ. 0) THEN
       WRITE(*,2345) JSUB
       WRITE(25,2345) JSUB
2345   FORMAT(//' A S.D. IS 0 FOR JSUB = ',I5,'. RERUN THE '/
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
      IF(SIG(I,IEQ) .LT. 0) THEN
       WRITE(*,2346) JSUB
       WRITE(25,2346) JSUB
2346   FORMAT(//' A S.D. < 0 FOR JSUB = ',I5,'. RERUN THE '/
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
      SIGFAC=SIGFAC*SIG(I,IEQ)
      NDEX = NDEX+1
      RINV(NDEX,NDEX) = 1.D0/SIG(I,IEQ)/SIG(I,IEQ)
  140 CONTINUE
        MISTOT = 0
        DO IEQ=1,NUMEQT
          MISTOT = MISTOT + MISVAL(IEQ)
          NOBTOT(IEQ) = NOBTOT(IEQ) + NOBSER - MISVAL(IEQ)
        END DO
        NOBACT = NOBSER*NUMEQT - MISTOT
        OFAC=2.506628274631**NOBACT
        DO J=1,NVAR
          THETA(J) = ESTMEN(J)
        END DO
        CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,THETA,VALFIX,RANFIXEST,PX)
        CALL IDCALCP(JSUB,IG,NVAR,NOFIX,NRANFIX,IRAN,NDIM,
     1    PX,PMAT,INTLIST,RPAR,IPAR)
        CALL CALCOV(NVAR,NOBACT,PMAT,RINV,COVINV)
        DO I=1,NVAR
          IF(COVINV(I,I) .LE. 0) THEN
            IRPRT=0
            IREPRT(JSUB)=0
            IESTIJ(JSUB,I)=0
          ENDIF
        END DO
  700 CONTINUE
	IF(IRPRT .EQ. 1) WRITE(*,5106) NSUB,NVAR
	IF(IRPRT .EQ. 1) WRITE(25,5106) NSUB,NVAR
 5106   FORMAT(//' ALL ',I3,' SUBJECTS GIVE ADEQUATE INFORMATION ON '/
     1' ALL ',I2,' PARAMETERS.'/)
	IF(IRPRT .EQ. 0) THEN
	WRITE(*,5107)
	WRITE(25,5107)
 5107   FORMAT(//' THE FOLLOWING SUBJECTS GIVE NO INFORMATION ON THE'/
     1' INDICATED PARAMETERS. EACH OF THE RELATED PARAMETER ESTIMATES'/
     2' FOR THESE SUBJECTS (FOR EACH CYCLE) WILL BE SET EQUAL TO THE '/
     3' POPULATION MEAN FROM THE SUBJECTS WHICH HAVE ADEQUATE'/
     4' INFORMATION ON THE INDICATED PARAMETER: '//
     5' SUBJECT   PARAMETER(S)'/)
	DO ISUB=1,NSUB
	IF(IREPRT(ISUB) .EQ. 0) THEN
	  WRITE(*,5108) ISUB
	  WRITE(25,5108) ISUB
 5108     FORMAT('  ',I3,'     ')
	  DO J=1,NVAR
	    IF(IESTIJ(ISUB,J) .EQ. 0) WRITE(*,5109) PAR(J)
	    IF(IESTIJ(ISUB,J) .EQ. 0) WRITE(25,5109) PAR(J)
 5109       FORMAT(' ',A11,2X)
	  END DO
	ENDIF
	END DO
	ENDIF
	OLDAVG=-1.D30
	OPEN(28,FILE=PARFIL)
	DO 2000 ITER = 1,MAXIT
	REWIND(27)
	WRITE(*,1217) ITER
	WRITE(25,1217) ITER
 1217 FORMAT(//' ITERATION NUMBER ',I5,'.'//)
	CLOSE(28)
	PARFIL = 'LAST'//NAME
	OPEN(28,FILE=PARFIL)
        WRITE(28,7126)
 7126   FORMAT(' REM_FRN MAR_16 ... made by it2branfix1.f')
        WRITE(28,1013)
 1013   FORMAT(' ',' THIS FILE CONTAINS, FOR THE LAST ITERATION ONLY,'/
     1' FOR EACH SUBJECT, THE PARAMETER ESTIMATES, THE ESTIMATE OF THE'/
     2' COVARIANCE MATRIX OF THESE ESTIMATES, AND THE CORRESPONDING '/
     3' CORRELATION MATRIX, STANDARD DEVIATIONS, AND % COEFFICIENTS OF'/
     4' OF VARIATIONS. ALSO INCLUDED ARE GAMMA UPDATE(S)'/
     5' (IF APPLICABLE) AND PREDICTED VALUES BASED FIRST ON FINAL'/
     6' CYCLE POPULATION VALUES, AND THEN ON MAP BAYESIAN PARAMETER '/
     7' ESTIMATES (THE MAP BAYESIANS ESTIMATES THEMSELVES ARE ALSO'/
     8' WRITTEN TO THIS FILE.'//
     6' ALL MATRICES ARE WRITTEN IN LOWER TRIANGULAR FORM.'//)
	WRITE(28,1217) ITER
	DO I=1,NVAR
	 DO J=1,NVAR
	  ESTINV(I,J) = ESTCOV(I,J)
	 END DO
	END DO
 	CALL MATNV2(ESTINV,NVAR,B,1,DET)
	IF(DET .LE. 0) THEN
	 WRITE(*,1216)
	 WRITE(25,1216)
 1216   FORMAT(/' THE CURRENT POPULATION COV. ESTIMATE IS SINGULAR.'/
     1' THIS INDICATES AN ILL-CONDITIONED PROBLEM, OR POSSIBLY AN'/
     2' OVER-PARAMETERIZED PROBLEM (I.E., FEWER OBSERVED VALUES THAN'/
     3' PARAMETERS TO BE ESTIMATED). PLEASE RE-EXAMINE YOUR PATIENT'/
     4' DATA FILES, AND YOUR INPUT INSTRUCTIONS, CORRECT ANY '/
     5' INCONSISTENCIES, AND THEN RERUN THE PROGRAM.'//)
        OPEN(42,FILE=ERRFIL)
         WRITE(42,1216)
        CLOSE(42)
       CLOSE(27)
       CLOSE(25)
       CALL PAUSE
       STOP
      ENDIF
	SUMLOG=0.D0
	WRITE(*,1215)
 1215   FORMAT(/' SUBJECT NUMBERS FOLLOW AS PARAMETER ESTIMATES ARE'/
     1' BEING FOUND: '/)
 9999   FORMAT(' ',I3)
	DO IEQ=1,NUMEQT
	 SUMDIF(IEQ) = 0.D0
	END DO
      DO 1000 JSUB=1,NSUB
        IG = JSUB
	WRITE(*,9999) JSUB
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)
	MISTOT = 0
	SIGFAC=1.D0
	DO I=1,NOBSER
       DO IEQ=1,NUMEQT
          Y=YO(I,IEQ)
	  IF(Y .EQ. -99) THEN
	   MISTOT = MISTOT + 1
	  ENDIF
	  IF(Y .NE. -99) THEN
      SIG(I,IEQ)=GAMMA(IEQ)*(C0(IEQ)+C1(IEQ)*Y+C2(IEQ)*Y*Y+C3(IEQ)*Y**3)
      SIGFAC=SIGFAC*SIG(I,IEQ)
	  ENDIF
	 END DO
        END DO
        OFAC=2.506628274631**(NOBSER*NUMEQT - MISTOT)
        DO I=1,NVAR
          START(I)=ESTMEN(I)
          STEP(I)= -.2D0*START(I)
        END DO
	CALL ELDERY(NVAR,START,THETA,VALMIN,1.D-10,STEP,1000,MAPBAYS,
     1  0,ICONV,NITER,ICNT,JSUB,IG,INTLIST,RPAR,IPAR)
	IF(ICONV .EQ. 0) WRITE(*,9011) JSUB
	IF(ICONV .EQ. 0) WRITE(25,9011) JSUB
 9011 FORMAT(' ',' NO PARAMETER ESTIMATE CONV. FOR SUBJECT NO ',I3)
	DO IEQ=1,NUMEQT
	 SUMDIF(IEQ) = SUMDIF(IEQ) + SSND(IEQ)
	END DO
	DO J=1,NVAR
	IF(IESTIJ(JSUB,J) .EQ. 1) PAREST(JSUB,J) = THETA(J)
	IF(IESTIJ(JSUB,J) .EQ. 0) PAREST(JSUB,J) = 0.D0
	END DO
         SUMLOG = SUMLOG - VALMIN
 1000   CONTINUE
	DO IEQ=1,NUMEQT
	 IF(IGAMMA(IEQ) .EQ. 0)
     1   GAMMA(IEQ) = GAMMA(IEQ)*DSQRT(SUMDIF(IEQ)/NOBTOT(IEQ))
	END DO
	AVGLOG = SUMLOG/dble(NSUB)
	DO J=1,NVAR
	 SUM(J)=0.D0
	 SUMCOL(J)=0.D0
	END DO
	DO JSUB=1,NSUB
	 DO J=1,NVAR
	  SUM(J) = SUM(J) + PAREST(JSUB,J)
	  SUMCOL(J) = SUMCOL(J) + IESTIJ(JSUB,J)
	 END DO
	END DO
	DO J=1,NVAR
	IF(SUMCOL(J) .GT. 0.D0)	ESTMEN(J) = SUM(J)/SUMCOL(J)
	IF(SUMCOL(J) .EQ. 0.D0) THEN
	 WRITE(*,7131) PAR(J)
	 WRITE(25,7131) PAR(J)
        OPEN(42,FILE=ERRFIL)
         WRITE(42,7131) PAR(J)
        CLOSE(42)
	 CLOSE(27)
	 CLOSE(25)
       CALL PAUSE
	 STOP
	ENDIF
 7131 FORMAT(///' NO SUBJECT CAN ESTIMATE PARAMETER ',A11,'. THE '/
     1' PROGRAM STOPS. YOU MAY WISH TO RERUN THE ANALYSIS WITH A '/
     2' DIFFERENT PARAMETERIZATION AND/OR CHECK YOUR SUBJECT DATA '/
     3' FILES FOR ERRORS.'//)
	END DO
	DO JSUB=1,NSUB
	 DO J=1,NVAR
	  IF(IESTIJ(JSUB,J) .EQ. 0) PAREST(JSUB,J) = ESTMEN(J)
	 END DO
	END DO
	CALL GETMED(NVAR,NSUB,MAXSUB,MAXDIM,IESTIJ,PAREST,VEC,ESTMED)
	DO I=1,NVAR
	 DO J=1,NVAR
	  SUMCOV(I,J) = 0.D0
	 END DO
	END DO
	WRITE(*,1219)
 1219   FORMAT(/' SUBJECT NUMBERS FOLLOW AS COVARIANCE MATRIX'/
     1' ESTIMATES (OF PARAMETER ESTIMATES) ARE BEING FOUND: '/)
	REWIND(27)
	DO 1500 JSUB=1,NSUB
          IG=JSUB
	WRITE(*,9999) JSUB
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)
	MISTOT = 0
	MAXTOT = NUMEQT*NOBSER
	DO I =1,MAXTOT
	 DO J=1,MAXTOT
	  RINV(I,J) = 0.D0
	 END DO
	END DO
	NDEX = 0
	SIGFAC=1.D0
      	DO IEQ=1,NUMEQT
	 DO I=1,NOBSER
          Y=YO(I,IEQ)
	  IF(Y .EQ. -99) THEN
	   MISTOT = MISTOT + 1
	  ENDIF
	  IF(Y .NE. -99) THEN
      SIG(I,IEQ)=GAMMA(IEQ)*(C0(IEQ)+C1(IEQ)*Y+C2(IEQ)*Y*Y+C3(IEQ)*Y**3)
      SIGFAC=SIGFAC*SIG(I,IEQ)
      NDEX = NDEX+1
      RINV(NDEX,NDEX) = 1.D0/SIG(I,IEQ)/SIG(I,IEQ)
	  ENDIF
	 END DO
        END DO
        NOBACT = NOBSER*NUMEQT - MISTOT
        OFAC=2.506628274631**NOBACT
	DO J=1,NVAR
	 THETA(J) = PAREST(JSUB,J)
	END DO
	CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,THETA,VALFIX,RANFIXEST,PX)
	CALL IDCALCP(JSUB,IG,NVAR,NOFIX,NRANFIX,IRAN,NDIM,PX,PMAT,
     1     INTLIST,RPAR,IPAR)
	CALL CALCOV(NVAR,NOBACT,PMAT,RINV,COVINV)
	DO I=1,NVAR
	IF(COVINV(I,I) .LE. 0) THEN
	  IF(IESTIJ(JSUB,I) .EQ. 1) THEN
	    IESTIJ(JSUB,I) = 0
	    WRITE(*,5987) JSUB, PAR(I)
	    WRITE(25,5987) JSUB, PAR(I)
 5987       FORMAT(//' SUBJECT ',I3,' NO LONGER CAN ESTIMATE '/
     1' PARAMETER ',A11,'. STARTING WITH THE NEXT CYCLE, THIS '/
     2' PARAMETER ESTIMATE FOR THIS SUBJECT WILL BE SET EQUAL TO THE '/
     3' POPULATION MEAN FROM THE SUBJECTS WHICH HAVE INFORMATION ON '/
     4' IT.'/)
	  ENDIF
	ENDIF
	END DO
	CALL CALCPIK(NVAR,COVINV,ESTINV,PIK)
	DO J=1,NVAR
	 DIFF(J) = THETA(J) - ESTMEN(J)
	END DO
	DO I=1,NVAR
	 DO J=1,NVAR
	  DIFPRD(I,J) = DIFF(I)*DIFF(J)
	 END DO
	END DO
	DO I=1,NVAR
	 DO J=1,NVAR
	  SUMCOV(I,J) = SUMCOV(I,J) + PIK(I,J) + DIFPRD(I,J)
	 END DO
	END DO
	WRITE(28,5321) JSUB
 5321   FORMAT(///' SUBJECT NO. ',I4//
     1' THE PARAMETER ESTIMATES ARE: '/)
    	WRITE(28,5102) (PAR(I),I=1,NVAR)
	WRITE(28,5103) (THETA(I),I=1,NVAR)
	WRITE(28,*)
	WRITE(28,*)' THE COVARIANCE MATRIX EST. OF THESE ESTIMATES ARE:'
	WRITE(28,*)
	WRITE(28,5102) (PAR(I),I=1,NVAR)
	DO I=1,NVAR
	 WRITE(28,5103) (PIK(I,J),J=1,I)
	END DO
	DO I=1,NVAR
	IF(PIK(I,I) .LE. 0.D0) THEN
	 WRITE(*,2228) JSUB
	 WRITE(28,2228) JSUB
	 WRITE(25,2228) JSUB
 2228   FORMAT(/' THE CURRENT ESTIMATE OF THE COVARIANCE MATRIX OF THE'/
     1' PARAMETER ESTIMATES FOR SUBJECT ',I4,' IS "SINGULAR". THIS IS'/
     2' DUE TO AN ILL-CONDITIONED PROBLEM, OR POSSIBLY AN'/
     2' OVER-PARAMETERIZED PROBLEM (I.E., FEWER OBSERVED VALUES THAN'/
     3' PARAMETERS TO BE ESTIMATED). PLEASE RE-EXAMINE YOUR PATIENT'/
     4' DATA FILES, ALONG WITH YOUR INPUT INSTRUCTIONS TO CHECK FOR'/
     5' ANY INCONSISTENCIES.'//)
        OPEN(42,FILE=ERRFIL)
         WRITE(42,2228) JSUB
        CLOSE(42)
       CALL PAUSE
	 STOP
	ENDIF
	 STDEV(I) = DSQRT(PIK(I,I))
	 DO J=1,I
	  CORR(I,J) = PIK(I,J)/STDEV(I)/STDEV(J)
	 END DO
	END DO
      WRITE(28,1521)
 1521 FORMAT(/' THE CORRESPONDING STANDARD DEVIATIONS ARE: '/)
      WRITE(28,5102) (PAR(I),I=1,NVAR)
      WRITE(28,5103) (STDEV(I),I=1,NVAR)
      WRITE(28,1522)
 1522 FORMAT(/' THE CORRESPONDING CORRELATION MATRIX IS: '/)
      WRITE(28,5102) (PAR(I),I=1,NVAR)
      DO I=1,NVAR
       WRITE(28,5103) (CORR(I,J),J=1,I)
      END DO
      WRITE(28,1523)
 1523 FORMAT(/' THE CORRESPONDING % COEFF. OF VARIATIONS ARE:'/)
      WRITE(28,5102) (PAR(I),I=1,NVAR)
      WRITE(28,5103) ((100.*STDEV(I)/THETA(I)),I=1,NVAR)
 1500   CONTINUE
	 WRITE(28,5117) NUMEQT,NUMEQT
	 DO I=1,NUMEQT
	  WRITE(28,*) GAMMA(I)
	 END DO
	DO I=1,NVAR
	 DO J=1,NVAR
	  ESTCOV(I,J) = SUMCOV(I,J)/NSUB
	 END DO
	END DO
	ITRULOG=2
	ISTOP=0
	IF(DABS(AVGLOG - OLDAVG) .LT. TOL) ISTOP=1
	IF(ITER .EQ. MAXIT .OR. ISTOP .EQ. 1) THEN
	 ITRULOG=1
      NNNVAR=0
      DO I=1,NVAR+NOFIX+NRANFIX
       IF(IRAN(I) .EQ. 1 .OR. IRAN(I) .EQ. -1) THEN
        NNNVAR=NNNVAR+1
        STD = DSQRT(ESTCOV(NNNVAR,NNNVAR))
        AB(NNNVAR,1) = ESTMEN(NNNVAR) - XDEV*STD
        IF(IRAN(I) .EQ. 1 .AND. AB(NNNVAR,1) .LE. 0.D0)
     1   AB(NNNVAR,1) = 1.D-8
        AB(NNNVAR,2) = ESTMEN(NNNVAR) + XDEV*STD
       ENDIF
      END DO
	IF(ILOG .EQ. 1) GO TO 5120
	WRITE(*,*)
	WRITE(*,*)
	WRITE(*,*)' CALCULATING THE TRUE (NUMERICAL) LOG-LIKELIHOOD'
	WRITE(*,*)' OF THE PATIENT DATA FILES ... '
	WRITE(*,*)
	WRITE(*,*)
	NGRID = NSUB
	DO JSUB=1,NGRID
	 DO J=1,NVAR
	  CORDEN(JSUB,J) = PAREST(JSUB,J)
	 END DO
	END DO
	REWIND(27)
      TRULOG=0.D0
	DO 3500 JSUB=1,NSUB
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)
	MISTOT = 0
	SIGFAC=1.D0
	DO I=1,NOBSER
      	 DO IEQ=1,NUMEQT
          Y=YO(I,IEQ)
	  IF(Y .EQ. -99) THEN
	   MISTOT = MISTOT + 1
	  ENDIF
	  IF(Y .NE. -99) THEN
      SIG(I,IEQ)=GAMMA(IEQ)*(C0(IEQ)+C1(IEQ)*Y+C2(IEQ)*Y*Y+C3(IEQ)*Y**3)
      SIGFAC=SIGFAC*SIG(I,IEQ)
	  ENDIF
	 END DO
        END DO
        OFAC=2.506628274631**(NOBSER*NUMEQT - MISTOT)
 8888   FORMAT(' ',' SUBJECT ',I5,' ...  % COMPLETED = ',F8.2)
	XNEXT=.1D0
	DO 3800 IG=1,NGRID
	XPER=IG*100.D0/NGRID
	IF(XPER .GE. XNEXT) THEN
	 WRITE(*,8888) JSUB,XPER
	 XNEXT=XNEXT+.1D0
	ENDIF
	DO J=1,NVAR
	 X(J)=CORDEN(IG,J)
	END DO
      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,RANFIXEST,PX)
      CALL IDPC(JSUB,IG,PX,WEQ,INTLIST,RPAR,IPAR)
	WTOTAL = 0.D0
	DO IEQ=1,NUMEQT
	 WTOTAL = WTOTAL + WEQ(IEQ)
	END DO
        PYJGX=0.D0
        IF(WTOTAL .LE. 22708.D0) PYJGX=DEXP(-.5D0*WTOTAL)/SIGFAC/OFAC
 3800   WORK(IG)=PYJGX/NSUB
	SUMM=0.D0
	DO IG=1,NGRID
       SUMM=SUMM+WORK(IG)
	END DO
	PYJ=SUMM
	IF (PYJ .LE. 0.D0) THEN
	 WRITE(*,126) JSUB
	 WRITE(25,126) JSUB
  126    FORMAT(//' FOR SUBJECT ',I4,' THE PROB. OF THE OBSERVED'/
     1' CONCENTRATIONS, IS NUMERICALLY .LE. 0. THIS CAN OCCUR WHEN THE'/
     2' PROBABILITY OF THE CONCENTRATIONS GIVEN EACH AND EVERY GRID '/
     3' POINT IN THE ESTABLISHED GRID = 0.'//
     4' YOU MAY WISH TO CONSIDER INCREASING THE MAGNITUDE OF THE ASSAY'/
     5' NOISE COEFFICIENTS (WHICH WILL INCREASE THE ABOVE CONDITIONAL'/
     6' PROBABILITIES) AND/OR DOUBLE CHECKING THE DATA FILE FOR THIS'/
     7' SUBJECT.'//
     8' THIS SUBJECT WILL MAKE NO CONTRIBUTION TO THE TRUE'/
     9' LOG-LIKELIHOOD OF THE PATIENT DATA FILES. '/)
	 GO TO 3500
	ENDIF
	TRULOG = TRULOG + DLOG(PYJ)
 3500   CONTINUE
	NOBTOTAL = 0
	DO IEQ=1,NUMEQT
	 NOBTOTAL = NOBTOTAL + NOBTOT(IEQ)
	END DO
	PVAL = (NVAR*NVAR + 3*NVAR)/2.D0
	AIC = 2.D0*(-TRULOG + (PVAL + QVAL))
	BIC = 2.D0*(-TRULOG + .5D0*(PVAL + QVAL)*DLOG(1.D0*NOBTOTAL))
	OPEN(30,FILE=DENFIL)
        VOLSPA=1.D0
        DO I=1,NVAR
         VOLSPA = VOLSPA*(AB(I,2)-AB(I,1))
      	END DO
         PROBASS = 2129.D0/VOLSPA/NSUB
        WRITE(30,8124)
 8124   FORMAT('DENSITY MAR_16 ... MADE BY BIG IT2B FOR NPAG')
	WRITE(30,*) NDIM
	WRITE(30,*)' 1'
	WRITE(30,*) NSUB
	WRITE(30,*) NVAR
	WRITE(30,2227) (PAR(I),I=1,NVAR)
	WRITE(30,*) NOFIX
	WRITE(30,2227) (PARFIX(I),I=1,NOFIX)
      WRITE(30,*) NRANFIX
      WRITE(30,2227) (PARRANFIX(I),I=1,NRANFIX)
	DO I=1,NVAR
	 WRITE(30,*) (AB(I,J),J=1,2)
	END DO
	WRITE(30,*) (VALFIX(I),I=1,NOFIX)
	WRITE(30,*) (RANFIXEST(I),I=1,NRANFIX)
	WRITE(30,*) ' 100'
	WRITE(30,*)' 1'
	WRITE(30,*)' 1.0'
	 DO I=1,NSUB
	  WRITE(30,*) (CORDEN(I,J),J=1,NVAR),PROBASS
	 END DO
	CLOSE(30)
	ENDIF
 5120   CONTINUE
      HOWCLOSE = (DABS(AVGLOG - OLDAVG))/TOL
      XVERIFY(1) = AVGLOG
      XVERIFY(2) = HOWCLOSE
      CALL VERIFYVAL(2,XVERIFY)
      WRITE(*,4324) XVERIFY(1),XVERIFY(2)
4324   FORMAT(/' THE AVERAGE LOG-LIKELIHOOD OF THE PATIENT DATA FILES'/
     1' GIVEN THE PARAMETER ESTIMATES FOR THIS CYCLE, AND THE PRIOR '/
     2' POPULATION ESTIMATES (MEAN AND COVARIANCE), IS ',G15.6//
     3' THE CONVERGENCE INDEX IS   ',G15.6,'   (1.0 = CONVERGENCE).'//)
      XVERIFY(1) = TRULOG
      XVERIFY(2) = AIC
      XVERIFY(3) = BIC
      CALL VERIFYVAL(3,XVERIFY)
      IF(ITRULOG .EQ. 1 .AND. ILOG .EQ. 0) THEN
        WRITE(*,4327) NSUB,XVERIFY(1)
        WRITE(*,4331) XVERIFY(2),XVERIFY(3)
      ENDIF
 4327   FORMAT(//' THE TRUE (NUMERICAL) LOG-LIKELIHOOD OF THE ',I3/
     1' SUBJECT VECTORS IS ',G15.6/)
 4331   FORMAT(/' THE AKAIKE INFO. CRITERION (AIC) IS ',G15.6//
     1' THE SCHWARTZ (BAYESIAN) INFO. CRITERION (BIC) IS ',G15.6/)
	WRITE(*,5104) NVAR
 5104   FORMAT(//' THE NEW BAYESIAN POSTERIOR POPULATION ESTIMATES (AT'/
     1' THE END OF THIS ITERATION) ARE SHOWN BELOW (THE 1ST ROW GIVES '/
     2' THE MEANS; THE 2ND ROW GIVES THE MEDIANS; THE NEXT ',I1,' ROWS'/
     3' GIVE THE COVARIANCE MATRIX):'/)
	WRITE(*,5102) (PAR(I),I=1,NVAR)
      DO I = 1,NVAR
       XVERIFY(I) = ESTMEN(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)
      WRITE(*,5103) (XVERIFY(I),I=1,NVAR)
      DO I = 1,NVAR
       XVERIFY(I) = ESTMED(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)
      WRITE(*,5103) (XVERIFY(I),I=1,NVAR)
	WRITE(*,*)
	WRITE(*,5102) (PAR(I),I=1,NVAR)
      DO I=1,NVAR
       DO J = 1,I
        XVERIFY(J) = ESTCOV(I,J)
       END DO
       CALL VERIFYVAL(I,XVERIFY)
       WRITE(*,5103) (XVERIFY(J),J=1,I)
      END DO
	DO I=1,NVAR
	 IF(ESTCOV(I,I) .LE. 0.D0) THEN
	  WRITE(*,5111) PAR(I)
	  WRITE(25,5111) PAR(I)
 5111     FORMAT(///' PARAMETER ',A11,' HAS A POPULATION VARIANCE '/
     1' ESTIMATE WHICH IS LESS THAN OR EQUAL TO 0. THE PROGRAM STOPS.'/
     2' PLEASE CHECK YOUR PATIENT DATA FILES, AND/OR RERUN WITH A '/
     3' DIFFERENT PARAMETER SELECTION.'//)
        OPEN(42,FILE=ERRFIL)
         WRITE(42,5111) PAR(I)
        CLOSE(42)
	  CLOSE(27)
	  CLOSE(25)
        CALL PAUSE
	  STOP
	 ENDIF
	END DO
	DO I=1,NVAR
	 STDEV(I)=DSQRT(ESTCOV(I,I))
	 DO J=1,I
	  CORR(I,J)=ESTCOV(I,J)/STDEV(I)/STDEV(J)
	 END DO
	END DO
	WRITE(*,9042)
 9042   FORMAT(/' THE STANDARD DEVIATION ESTIMATES ARE: '/)
	WRITE(*,5102) (PAR(I),I=1,NVAR)
      DO I = 1,NVAR
       XVERIFY(I) = STDEV(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)
      WRITE(*,5103) (XVERIFY(I),I=1,NVAR)
	WRITE(*,9041)
 9041   FORMAT(/' THE ESTIMATE OF THE CORRELATION COEFFICIENT MATRIX'/
     1' IS, IN LOWER-TRI FORM: '/)
	WRITE(*,5102) (PAR(I),I=1,NVAR)
      DO I=1,NVAR
       DO J = 1,I
        XVERIFY(J) = CORR(I,J)
       END DO
       CALL VERIFYVAL(I,XVERIFY)
       WRITE(*,5103) (XVERIFY(J),J=1,I)
      END DO
      WRITE(*,5128)
 5128   FORMAT(/' THE ESTIMATE OF THE % COEFFICIENTS OF VARIATIONS '/
     1' FOR THE VARIABLES ARE:'/)
      WRITE(*,5102) (PAR(I),I=1,NVAR)
      DO I = 1,NVAR
       XVERIFY(I) = 100*DSQRT(ESTCOV(I,I))/ESTMEN(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)
      WRITE(*,5103) (XVERIFY(I),I=1,NVAR)
	 WRITE(*,5117) NUMEQT,NUMEQT
 5117  FORMAT(//' EACH OF THE ',I1,' OUTPUT EQUATION(S) HAS ASSOCIATED'/
     1' A UNIQUE GAMMA, WHERE THE ASSAY ERROR S.D. FOR THAT OUTPUT IS '/
     2' S.D. = GAMMA*(C0+C1*Y+C2*Y^2+C3*Y^3). THE UPDATED'/
     2' ESTIMATE(S) FOR THE ',I1,' GAMMA(S) FOLLOW(S): ')
	 DO I=1,NUMEQT
	  WRITE(*,*) GAMMA(I)
	 END DO
        write (*,*) "looking at XVERIFY"
      XVERIFY(1) = AVGLOG
      XVERIFY(2) = HOWCLOSE
      CALL VERIFYVAL(2,XVERIFY)
      WRITE(25,4324) XVERIFY(1),XVERIFY(2)
      XVERIFY(1) = TRULOG
      XVERIFY(2) = AIC
      XVERIFY(3) = BIC
      CALL VERIFYVAL(3,XVERIFY)
      IF(ITRULOG .EQ. 1 .AND. ILOG .EQ. 0) THEN
        WRITE(25,4327) NSUB,XVERIFY(1)
        WRITE(25,4331) XVERIFY(2),XVERIFY(3)
      ENDIF
	WRITE(25,5104) NVAR
	WRITE(25,5102) (PAR(I),I=1,NVAR)
      DO I = 1,NVAR
       XVERIFY(I) = ESTMEN(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)
      WRITE(25,5103) (XVERIFY(I),I=1,NVAR)
      DO I = 1,NVAR
       XVERIFY(I) = ESTMED(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)
      WRITE(25,5103) (XVERIFY(I),I=1,NVAR)
	WRITE(25,*)
	WRITE(25,5102) (PAR(I),I=1,NVAR)
      DO I=1,NVAR
       DO J = 1,I
        XVERIFY(J) = ESTCOV(I,J)
       END DO
       CALL VERIFYVAL(I,XVERIFY)
       WRITE(25,5103) (XVERIFY(J),J=1,I)
      END DO
	WRITE(25,9042)
	WRITE(25,5102) (PAR(I),I=1,NVAR)
      DO I = 1,NVAR
       XVERIFY(I) = STDEV(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)
      WRITE(25,5103) (XVERIFY(I),I=1,NVAR)
	WRITE(25,9041)
	WRITE(25,5102) (PAR(I),I=1,NVAR)
      DO I=1,NVAR
       DO J = 1,I
        XVERIFY(J) = CORR(I,J)
       END DO
       CALL VERIFYVAL(I,XVERIFY)
       WRITE(25,5103) (XVERIFY(J),J=1,I)
      END DO
	WRITE(25,5128)
	WRITE(25,5102) (PAR(I),I=1,NVAR)
      DO I = 1,NVAR
       XVERIFY(I) = 100*DSQRT(ESTCOV(I,I))/ESTMEN(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)
      WRITE(25,5103) (XVERIFY(I),I=1,NVAR)
	 WRITE(25,5117) NUMEQT,NUMEQT
	 DO I=1,NUMEQT
	  WRITE(25,*) GAMMA(I)
	 END DO
      IF(NRANFIX .GT. 0 .AND. ITER .EQ. 1) THEN
      DO I = 1,NVAR
       X(I) = ESTMEN(I)
      END DO
      write (*,*) "Calling MAKEVEC near 3440"
      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,RANFIXEST,PX)
       DO I = 1,NRANFIX
        START(I) = RANFIXEST(I)
        STEP(I) = -.2D0*START(I)
       END DO
       DO I = NRANFIX+1,NRANFIX+NVAR
        START(I) = ESTMEN(I-NRANFIX)
        STEP(I) = -.2D0*START(I)
       END DO
       CALL ELDERY2(NRANFIX+NVAR,START,OPTVAR,VALMIN,1.D-10,STEP,1000,
     1  CALCRF,0,ICONV,NITER,ICNT,NUMEQT,YO,C0,C1,C2,C3,C4,C5,GAMMA
     2  ,JSUB,IG,INTLIST)
	IF(ICONV .EQ. 0) WRITE(*,9021)
	IF(ICONV .EQ. 0) WRITE(25,9021)
 9021 FORMAT(' ',' NO CONVERGENCE THIS CYCLE ON ESTIMATES FOR THE'/
     1' RANFIX AND RANDOM PARAMETERS. '/)
      DO I = 1,NRANFIX
       RANFIXEST(I) = OPTVAR(I)
      END DO
      DO I = NRANFIX+1,NRANFIX+NVAR
       ESTMENO(I-NRANFIX) = OPTVAR(I)
      END DO
      DO JSUB=1,NSUB
       DO J=1,NVAR
        PAREST(JSUB,J) = PAREST(JSUB,J) * ESTMENO(J)/ESTMEN(J)
       END DO
      END DO
      DO I=1,NVAR
       DO J=1,NVAR
        SUMCOV(I,J) = 0.D0
       END DO
      END DO
	WRITE(*,3219)
 3219   FORMAT(/' THE ESTIMATION OF THE RANFIX PARAMETERS HAS JUST'/
     1' BEEN COMPLETED. THIS PROCESS ALSO INCLUDED UPDATING THE'/
     2' RANDOM VARIABLE ESTIMATES. NOW UPDATING THE COVARIANCE MATRIX'/
     3' ESTIMATES BASED ON THESE NEW VALUES.'//
     4' SUBJECT NUMBERS FOLLOW AS THESE ESTIMATES ARE BEING FOUND: ')
      REWIND(27)
      DO 4500 JSUB=1,NSUB
        IG=JSUB
       WRITE(*,9999) JSUB
       CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)
       MISTOT = 0
       MAXTOT = NUMEQT*NOBSER
       DO I =1,MAXTOT
        DO J=1,MAXTOT
         RINV(I,J) = 0.D0
        END DO
       END DO
       NDEX = 0
       SIGFAC=1.D0
       DO IEQ=1,NUMEQT
        DO I=1,NOBSER
         Y = YO(I,IEQ)
         IF(Y .EQ. -99) MISTOT = MISTOT + 1
         IF(Y .NE. -99) THEN
          SIG(I,IEQ) = GAMMA(IEQ)*(C0(IEQ) + C1(IEQ)*Y
     1                 + C2(IEQ)*Y*Y + C3(IEQ)*Y**3)
          SIGFAC=SIGFAC*SIG(I,IEQ)
          NDEX = NDEX+1
          RINV(NDEX,NDEX) = 1.D0/SIG(I,IEQ)/SIG(I,IEQ)
         ENDIF
        END DO
       END DO
       NOBACT = NOBSER*NUMEQT - MISTOT
       OFAC=2.506628274631**NOBACT
       DO J=1,NVAR
        THETA(J) = PAREST(JSUB,J)
       END DO
       CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,THETA,VALFIX,RANFIXEST,PX)
       CALL IDCALCP(JSUB,IG,NVAR,NOFIX,NRANFIX,IRAN,NDIM,
     1   PX,PMAT,INTLIST,RPAR,IPAR)
       CALL CALCOV(NVAR,NOBACT,PMAT,RINV,COVINV)
       DO I=1,NVAR
        IF(COVINV(I,I) .LE. 0) THEN
         IF(IESTIJ(JSUB,I) .EQ. 1) THEN
          IESTIJ(JSUB,I) = 0
          WRITE(*,5987) JSUB, PAR(I)
          WRITE(25,5987) JSUB, PAR(I)
         ENDIF
        ENDIF
       END DO
       CALL CALCPIK(NVAR,COVINV,ESTINV,PIK)
       DO J=1,NVAR
        DIFF(J) = THETA(J) - ESTMENO(J)
       END DO
       DO I=1,NVAR
        DO J=1,NVAR
         DIFPRD(I,J) = DIFF(I)*DIFF(J)
        END DO
       END DO
       DO I=1,NVAR
        DO J=1,NVAR
         SUMCOV(I,J) = SUMCOV(I,J) + PIK(I,J) + DIFPRD(I,J)
        END DO
       END DO
       DO I = 1,NVAR
        IF(PIK(I,I) .LE. 0.D0) THEN
         WRITE(*,3228) JSUB
         WRITE(28,3228) JSUB
         WRITE(25,3228) JSUB
 3228   FORMAT(/' AFTER THE CALCULATION OF THE RANFIX PARAMETERS IN'/
     1' ITER. 1, THE CURRENT ESTIMATE OF THE COVARIANCE MATRIX OF THE'/
     1' PARAMETER ESTIMATES FOR SUBJECT ',I4,' IS "SINGULAR". THIS IS'/
     2' DUE TO AN ILL-CONDTIONED PROBLEM, OR POSSIBLY AN'/
     2' OVER-PARAMETERIZED PROBLEM (I.E., FEWER OBSERVED VALUES THAN'/
     3' PARAMETERS TO BE ESTIMATED). PLEASE RE-EXAMINE YOUR PATIENT'/
     4' DATA FILES, ALONG WITH YOUR INPUT INSTRUCTIONS TO CHECK FOR '/
     5' ANY INCONSISTENCIES.'//)
         OPEN(42,FILE=ERRFIL)
          WRITE(42,3228) JSUB
         CLOSE(42)
         CALL PAUSE
         STOP
        ENDIF
       END DO
 4500 CONTINUE
      DO I=1,NVAR
       DO J=1,NVAR
        ESTCOV(I,J) = SUMCOV(I,J)/NSUB
       END DO
      END DO
      DO J = 1,NVAR
       ESTMEN(J) = ESTMENO(J)
      END DO
      WRITE(*,9012) NRANFIX
      WRITE(25,9012) NRANFIX
 9012 FORMAT(//' FOR THIS ITER., THE ESTIMATES FOR THE ',I2,' PARAMETERS
     1'/
     2' WHICH ARE UNKNOWN BUT THE SAME FOR ALL SUBJECTS ARE: ')
      WRITE(*,5102) (PARRANFIX(I),I=1,NRANFIX)
      WRITE(25,5102) (PARRANFIX(I),I=1,NRANFIX)
       DO I = 1,NRANFIX
        XVERIFY(I) = RANFIXEST(I)
       END DO
       CALL VERIFYVAL(NRANFIX,XVERIFY)
       WRITE(*,5103) (XVERIFY(I),I=1,NRANFIX)
       WRITE(25,5103) (XVERIFY(I),I=1,NRANFIX)
       WRITE(*,9013) NVAR
       WRITE(25,9013) NVAR
 9013 FORMAT(//' AFTER THE ESTIMATION OF THE PARAMETERS WHICH ARE '/
     1' UNKNOWN BUT THE SAME FOR ALL SUBJECTS, THE REVISED MEANS FOR'/
     2' THE ',I2,' RANDOM PARAMETERS ARE: ')
       WRITE(*,5102) (PAR(I),I=1,NVAR)
       WRITE(25,5102) (PAR(I),I=1,NVAR)
       DO I = 1,NVAR
        XVERIFY(I) = ESTMEN(I)
       END DO
       CALL VERIFYVAL(NVAR,XVERIFY)
       WRITE(*,5103) (XVERIFY(I),I=1,NVAR)
       WRITE(25,5103) (XVERIFY(I),I=1,NVAR)
       WRITE(*,9016) NVAR
       WRITE(25,9016) NVAR
 9016 FORMAT(//' AND THE REVISED COV. MATRIX FOR THE ',I2,' RANDOM'/
     1' PARAMETERS IS: ')
       WRITE(*,5102) (PAR(I),I=1,NVAR)
       WRITE(25,5102) (PAR(I),I=1,NVAR)
       DO I=1,NVAR
        DO J = 1,I
         XVERIFY(J) = ESTCOV(I,J)
        END DO
        CALL VERIFYVAL(I,XVERIFY)
        WRITE(*,5103) (XVERIFY(J),J=1,I)
        WRITE(25,5103) (XVERIFY(J),J=1,I)
       END DO
      ENDIF
	IF(ITER .EQ. MAXIT .OR. ISTOP .EQ. 1) THEN
	REWIND(27)
	DO 6000 JSUB=1,NSUB
          IG=JSUB
	 CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)
	 DO ICENTER = 1,2
	  IF(ICENTER .EQ. 1) THEN
	   DO J=1,NVAR
	    THETA(J) = ESTMEN(J)
	   END DO
	  ENDIF
	  IF(ICENTER .EQ. 2) THEN
	   DO J=1,NVAR
	    THETA(J) = ESTMED(J)
	   END DO
	  ENDIF
	CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,THETA,VALFIX,RANFIXEST,PX)
	CALL IDCALCY(JSUB,IG,NVAR+NOFIX+NRANFIX,NDIM,PX,YPRED,
     1      NUMEQT,INTLIST,RPAR,IPAR)
	DO IOBS=1,NOBSER
	 DO IEQ=1,NUMEQT
	 YPREDPOP(JSUB,IEQ,IOBS,ICENTER) = YPRED(IOBS,IEQ)
	 END DO
	END DO
	END DO
 6000   CONTINUE
	WRITE(*,2215)
 2215   FORMAT(/' SUBJECT NUMBERS FOLLOW AS THE MAP BAYESIAN PARAMETER'/
     1' ESTIMATES ARE BEING FOUND -- ASSUMING A PRIOR FIRST OF THE'/
     2' FINAL CYCLE POPULATION MEANS, AND THEN OF THE FINAL CYCLE'/
     3' POPULATION MEDIANS: '/)
	DO 9000 ICENTER = 1,2
	  IF(ICENTER .EQ. 2) THEN
	   DO J=1,NVAR
	    ESTMEN(J) = ESTMED(J)
	   END DO
	  ENDIF
	REWIND(27)
	DO I=1,NVAR
	 DO J=1,NVAR
	  ESTINV(I,J) = ESTCOV(I,J)
	 END DO
	END DO
 	CALL MATNV2(ESTINV,NVAR,B,1,DET)
	IF(DET .LE. 0) THEN
	 WRITE(*,2216)
	 WRITE(25,2216)
 2216  FORMAT(/' THE FINAL CYCLE POPULATION COV. ESTIMATE IS SINGULAR.'/
     1' THIS INDICATES AN ILL-CONDTIONED PROBLEM, OR POSSIBLY AN'/
     2' OVER-PARAMETERIZED PROBLEM (I.E., FEWER OBSERVED VALUES THAN'/
     3' PARAMETERS TO BE ESTIMATED). PLEASE RE-EXAMINE YOUR PATIENT'/
     4' DATA FILES, AND YOUR INPUT INSTRUCTIONS, CORRECT ANY'/
     5' INCONSISTENCIES, AND THEN RERUN THE PROGRAM.'//)
        OPEN(42,FILE=ERRFIL)
         WRITE(42,2216)
        CLOSE(42)
       CLOSE(27)
       CLOSE(25)
       CALL PAUSE
       STOP
      ENDIF
	DO 7000 JSUB=1,NSUB
          IG=JSUB
	WRITE(*,9999) JSUB
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)
	MISTOT = 0
	SIGFAC=1.D0
	DO I=1,NOBSER
      	 DO IEQ=1,NUMEQT
          Y=YO(I,IEQ)
	  IF(Y .EQ. -99) THEN
	   MISTOT = MISTOT + 1
	  ENDIF
	  IF(Y .NE. -99) THEN
      SIG(I,IEQ)=GAMMA(IEQ)*(C0(IEQ)+C1(IEQ)*Y+C2(IEQ)*Y*Y+C3(IEQ)*Y**3)
      SIGFAC=SIGFAC*SIG(I,IEQ)
	  ENDIF
	 END DO
        END DO
        OFAC=2.506628274631**(NOBSER*NUMEQT - MISTOT)
	DO I=1,NVAR
	 START(I)=ESTMEN(I)
         STEP(I)= -.2D0*START(I)
	END DO
	CALL ELDERY(NVAR,START,THETA,VALMIN,1.D-10,STEP,1000,MAPBAYS,
     1  0,ICONV,NITER,ICNT,JSUB,IG,INTLIST,RPAR,IPAR)
	IF(ICONV .EQ. 0) WRITE(*,9011) JSUB
	DO J=1,NVAR
	 IF(IESTIJ(JSUB,J) .EQ. 1) PAREST(JSUB,J) = THETA(J)
	 IF(IESTIJ(JSUB,J) .EQ. 0) PAREST(JSUB,J) = 0.D0
	END DO
 7000   CONTINUE
	DO J=1,NVAR
	 SUM(J)=0.D0
	 SUMCOL(J)=0.D0
	END DO
	DO JSUB=1,NSUB
	 DO J=1,NVAR
	  SUM(J) = SUM(J) + PAREST(JSUB,J)
	  SUMCOL(J) = SUMCOL(J) + IESTIJ(JSUB,J)
	 END DO
	END DO
	DO J=1,NVAR
	 IF(SUMCOL(J) .GT. 0.D0) ESTMEN(J) = SUM(J)/SUMCOL(J)
	END DO
	DO JSUB=1,NSUB
	 DO J=1,NVAR
	  IF(IESTIJ(JSUB,J) .EQ. 0) PAREST(JSUB,J) = ESTMEN(J)
	 END DO
	END DO
	DO JSUB=1,NSUB
	 DO J=1,NVAR
	  PARBAY(JSUB,ICENTER,J) = PAREST(JSUB,J)
	 END DO
	END DO
	REWIND(27)
	DO 8000 JSUB=1,NSUB
          IG=JSUB
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)
	DO J=1,NVAR
	 THETA(J) = PAREST(JSUB,J)
	END DO
	CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,THETA,VALFIX,RANFIXEST,PX)
	CALL IDCALCY(JSUB,IG,NVAR+NOFIX+NRANFIX,NDIM,PX,YPRED,
     1    NUMEQT,INTLIST,RPAR,IPAR)
	DO IOBS=1,NOBSER
	 DO IEQ=1,NUMEQT
	 YPREDBAY(JSUB,IEQ,IOBS,ICENTER) = YPRED(IOBS,IEQ)
	 END DO
	END DO
 8000   CONTINUE
 9000   CONTINUE
	WRITE(28,7203)
 7203   FORMAT(//' SUMMARY INFORMATION FOR THE RUN FOLLOWS: ')
	WRITE(28,5301) NSUB
 5301   FORMAT(//' THE NUMBER OF SUBJECTS IS ',I3)
	WRITE(28,5302) NVAR
 5302   FORMAT(/' THE NUMBER OF PARAMETERS IS ',I2)
	WRITE(28,7204)
 7204   FORMAT(/' THE PARAMETERS ARE: ')
	WRITE(28,5303) (PAR(I),I=1,NVAR)
 5303 FORMAT(30(A11,1X))
	WRITE(28,7217)
 7217 FORMAT(/' THE CORRESPONDING BOUNDARIES ARE: ')
      DO I=1,NVAR
       WRITE(28,*) (AB(I,J),J=1,2)
      END DO
	WRITE(28,2218) NUMEQT
	DO IEQ = 1,NUMEQT
       XVERIFY(1) = C0P(IEQ)
       XVERIFY(2) = C1P(IEQ)
       XVERIFY(3) = C2P(IEQ)
       XVERIFY(4) = C3P(IEQ)
       XVERIFY(5) = C4P(IEQ)
       XVERIFY(6) = C5P(IEQ)
       CALL VERIFYVAL(4,XVERIFY)
	 WRITE(28,162) IEQ,(XVERIFY(IXV),IXV=1,6)
	END DO
        WRITE(28,1229) NDRUG
        WRITE(28,*) (AF(I),I=1,NDRUG)
      WRITE(28,5306) NOFIX
 5306 FORMAT(/' THE NUMBER OF FIXED VARIABLE VALUES IS ',I2)
      IF(NOFIX .GT. 0) THEN
       WRITE(28,5307)
 5307  FORMAT(/' THESE FIXED VALUES ARE: ')
       DO I=1,NOFIX
        XVERIFY(1) = VALFIX(I)
        CALL VERIFYVAL(1,XVERIFY)
        WRITE(28,5309) PARFIX(I),XVERIFY(1)
       END DO
      ENDIF
 5309   FORMAT(' ',A11,':  ',G17.10)
      WRITE(28,6306) NRANFIX
 6306 FORMAT(/' THE NUMBER OF RANFIX PARAMETERS IS ',I2)
      IF(NRANFIX .GT. 0) THEN
       WRITE(28,6307)
 6307  FORMAT(/' THESE RANFIX ESTIMATES, WHICH ARE THE SAME FOR ALL'/
     1' SUBJECTS, AND WERE FOUND AT THE END OF ITER NO. 1 ARE: ')
       DO I=1,NRANFIX
        XVERIFY(1) = RANFIXEST(I)
        CALL VERIFYVAL(1,XVERIFY)
        WRITE(28,5309) PARRANFIX(I),XVERIFY(1)
       END DO
      ENDIF
	WRITE(28,7206)
 7206   FORMAT(//' THE PREDICTED VALUES FOR EACH SUBJECT, BASED FIRST'/
     1' ON THE FINAL CYCLE POPULATION MEANS, AND THEN ON THE FINAL '/
     2' CYCLE POPULATION MEDIANS, FOLLOW: '/)
	REWIND(27)
	DO JSUB=1,NSUB
	 CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)
	 DO IEQ=1,NUMEQT
	  DO IOBS=1,NOBSER
	   WRITE(28,*) (YPREDPOP(JSUB,IEQ,IOBS,ICEN),ICEN=1,2)
	  END DO
	 END DO
	END DO
	WRITE(28,7207)
 7207   FORMAT(//' THE PREDICTED VALUES FOR EACH SUBJECT, BASED ON '/
     1' THE MAP BAYESIAN POSTERIOR PARAMETER ESTIMATES OF THAT SUBJECT'/
     2' (WHICH ARE THEMSELVES BASED FIRST ON THE FINAL CYCLE'/
     3' POPULATION MEANS, AND THEN ON THE FINAL CYCLE POPULATION'/
     4' MEDIANS), FOLLOW: '/)
	REWIND(27)
	DO JSUB=1,NSUB
	 CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)
	 DO IEQ=1,NUMEQT
	  DO IOBS=1,NOBSER
	   WRITE(28,*) (YPREDBAY(JSUB,IEQ,IOBS,ICEN),ICEN=1,2)
	  END DO
	 END DO
	END DO
	WRITE(28,7208)
 7208   FORMAT(//' THE MAP BAYESIAN PARAMATER ESTIMATES FOR EACH '/
     1' SUBJECT, BASED FIRST ON THE FINAL CYCLE POPULATION MEANS, '/
     2' AND THEN ON THE FINAL CYCLE POPULATION MEDIANS, FOLLOW: '/)
	DO JSUB=1,NSUB
	 DO J=1,NVAR
	  WRITE(28,*) (PARBAY(JSUB,ICEN,J),ICEN=1,2)
	 END DO
	END DO
	CLOSE(28)
	ENDIF
        OPEN(24,FILE=FROMFIL)
         IONE = 1
         WRITE(24,*) IONE
         WRITE(24,7123)
 7123	   FORMAT('REM_FRN MAR_16 ... made by it2branfix1.f')
         WRITE(24,2) CSVFILE
         WRITE(24,*) NSUBTOT
         WRITE(24,*) NSUB
         CALL WRITEPT2(24,NSUB,IPATVEC)
         WRITE(24,*) '    0'
         IF(NOFIX .GT. 0) WRITE(24,*) (VALFIX(I),I=1,NOFIX)
         IF(NRANFIX .GT. 0) WRITE(24,*) (RANFIXEST(I),I=1,NRANFIX)
         DO I=1,NVAR
          WRITE(24,*) (AB(I,J),J=1,2)
         END DO
         WRITE(24,*) NUMEQT
         DO IEQ = 1,NUMEQT
          WRITE(24,*) GAMMA(IEQ)
          WRITE(24,*) C0P(IEQ),C1P(IEQ),C2P(IEQ),C3P(IEQ)
     1      ,C4P(IEQ),C5P(IEQ)
         END DO
         WRITE(24,*) NDRUG
         WRITE(24,*) (AF(I),I=1,NDRUG)
         CLOSE(24)
	IF(DABS(AVGLOG - OLDAVG) .LT. TOL) GO TO 2500
	OLDAVG=AVGLOG
 2000   CONTINUE
 2500   CONTINUE
      IF(NRANFIX .GT. 0) THEN
       DO I = 1,NRANFIX
        XVERIFY(I) = RANFIXEST(I)
       END DO
       CALL VERIFYVAL(NRANFIX,XVERIFY)
       WRITE(25,9014) NRANFIX
 9014 FORMAT(//' THE ESTIMATES FOR THE ',I2,' PARAMETERS WHICH ARE '/
     1' UNKNOWN BUT THE SAME FOR ALL SUBJECTS, AND WERE FOUND AT THE'/
     2' END OF ITER. NO. 1, ARE: ')
       WRITE(25,5102) (PARRANFIX(I),I=1,NRANFIX)
       WRITE(25,5103) (XVERIFY(I),I=1,NRANFIX)
        WRITE(*,9014) NRANFIX
        WRITE(*,5102) (PARRANFIX(I),I=1,NRANFIX)
        WRITE(*,5103) (XVERIFY(I),I=1,NRANFIX)
      ENDIF
	WRITE(*,*)
	WRITE(*,*)
	WRITE(*,*)'THE RANGES TO BE CONSIDERED FOR THE NPAG POPULATION'
	WRITE(*,*)'PROGRAM ARE AS FOLLOWS:'
	WRITE(*,*)
      DO I=1,NVAR
       XVERIFY(1) = AB(I,1)
       XVERIFY(2) = AB(I,2)
       CALL VERIFYVAL(2,XVERIFY)
       WRITE(*,341) PAR(I),XVERIFY(1),XVERIFY(2)
      END DO
  341 FORMAT(/' ',A11,': ',G17.10,'  TO  ',G17.10)
	WRITE(*,*)
      WRITE(25,*)
      WRITE(25,*)
      WRITE(25,*)'THE RANGES TO BE CONSIDERED FOR THE NPAG'
      WRITE(25,*)'POPULATION PROGRAM ARE AS FOLLOWS:'
      WRITE(25,*)
      DO I=1,NVAR
       XVERIFY(1) = AB(I,1)
       XVERIFY(2) = AB(I,2)
       CALL VERIFYVAL(2,XVERIFY)
       WRITE(25,341) PAR(I),XVERIFY(1),XVERIFY(2)
      END DO
        IF(ISTOP .EQ. 0) WRITE(25,5197) MAXIT
 5197   FORMAT(//' THIS RUN STOPPED  BECAUSE ...'/
     1' THE MAXIMUM NO. OF CYCLES WAS RUN. THIS MAXIMUM NO. WAS '/
     2' ',I6)
        IF(ISTOP .EQ. 1 .AND. ITER .LT. MAXIT) WRITE(25,5198) ITER
 5198   FORMAT(//' THIS RUN STOPPED BECAUSE ...'/
     1' THE CONVERGENCE CRITERION WAS MET AT ITERATION NO.'/
     2' ',I6)
        IF(ISTOP .EQ. 1 .AND. ITER .EQ. MAXIT) WRITE(25,5199) ITER
 5199   FORMAT(//' THIS RUN STOPPED BECAUSE ...'/
     2' BOTH THE CONVERGENCE CRITERION WAS MET, AND THE MAXIMUM NO.'/
     3' OF CYCLES WAS RUN. THIS MAXIMUM NO. WAS '/
     4' ',I6)
	PARFIL = 'LAST'//NAME
	OPEN(28,FILE=PARFIL)
	OPEN(29,FILE='it2bdriv.f')
	REWIND(27)
	REWIND(25)
	write(*,*)' About to create the combined output file ... '
	OUTCOM = 'OUTF'//NAME
	OPEN(26,FILE=OUTCOM)
	WRITE(26,1119)
 1119   FORMAT(' THIS IS A COMBINED OUTPUT FILE FROM BIG IT2B ENGINE.'/)
 1110   READ(25,2717,IOSTAT=IEND) READLARG
 2717   FORMAT(A1000)
        IF(IEND .LT. 0) GO TO 1120
        CALL CONDENSE(READLARG)
	GO TO 1110
 1120   WRITE(26,1121)
 1121   FORMAT(/'***************** END OF THE OUTPUT FILE **************
     1***'//
     2'************ LAST CYCLE PARAMETER INFORMATION FILE **********'/)
	write(*,*)' Writing parameter info to combined output file ...'
 1130   READ(28,2717,IOSTAT=IEND) READLARG
        IF(IEND .LT. 0) GO TO 1140
        CALL CONDENSE(READLARG)
        GO TO 1130
 1140   WRITE(26,1141)
 1141   FORMAT(/'********* END OF LAST CYCLE PARAMETER INFO FILE *******
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
     2'***************** START OF THE it2bdriv.f FILE *****************'
     3)
	write(*,*)' Writing model file to combined output file ...'
 1170   READ(29,2717,IOSTAT=IEND) READLARG
        IF(IEND .LT. 0) GO TO 1180
        CALL CONDENSE(READLARG)
        GO TO 1170
 1180   WRITE(26,1181)
 1181   FORMAT(/'***************** END OF THE it2bdriv.f FILE **********
     1*******'/)
      REWIND(25)
      REWIND(27)
      REWIND(29)
        OUTFILER = 'IT_RF'//NAME//'.TXT'
        CALL READOUT(OUTFILER,IRAN)
        STOP
        END
        SUBROUTINE ELDERY(N,START,XMIN,YNEWLO,REQMIN,STEP,
     X  ITMAX,FUNC,IPRINT,ICONV,NITER,ICOUNT,JSUB,IG,INTLIST,RPAR,IPAR)
      IMPLICIT REAL*8(A-H,O-Z)
        DIMENSION START(N),STEP(N),XMIN(N),XSEC(30),
     X  P(30,31),PSTAR(30),P2STAR(30),PBAR(30),Y(31)
        integer JSUB,IG
        integer, dimension(128) :: INTLIST
        double precision, dimension(257) :: RPAR
        integer, dimension(257) :: IPAR
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
        CALL FUNC(N,START,FN,JSUB,IG,INTLIST,RPAR,IPAR)
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
        CALL FUNC(N,START,FN,JSUB,IG,INTLIST,RPAR,IPAR)
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
        CALL FUNC(N,PSTAR,FN,JSUB,IG,INTLIST,RPAR,IPAR)
        YSTAR=FN
        ICOUNT=ICOUNT+1
        IF(YSTAR.GE.YLO) GO TO 12
        IF(ICOUNT.GE.KCOUNT) GO TO 19
        DO 9 I=1,N
9       P2STAR(I)=ECOEFF*PSTAR(I)+(1.0D0-ECOEFF)*PBAR(I)
        CALL FUNC(N,P2STAR,FN,JSUB,IG,INTLIST,RPAR,IPAR)
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
        CALL FUNC(N,P2STAR,FN,JSUB,IG,INTLIST,RPAR,IPAR)
        Y2STAR=FN
        ICOUNT=ICOUNT+1
        IF(Y2STAR.LT.Y(IHI)) GO TO 10
        DO 18 J=1,NN
        DO 17 I=1,N
        P(I,J)=(P(I,J)+P(I,ILO))*0.5D0
17      XMIN(I)=P(I,J)
        CALL FUNC(N,XMIN,FN,JSUB,IG,INTLIST,RPAR,IPAR)
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
        CALL FUNC(N,XMIN,FN,JSUB,IG,INTLIST,RPAR,IPAR)
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
	SUBROUTINE MAPBAYS(NVAR,VEC,FNTVAL,JSUB,IG,INTLIST,RPAR,IPAR)
        use npag_utils, only : makevec
	IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(MAXNUMEQ=7)
	DIMENSION VEC(NVAR),X(30),ESTMEN(30),ESTINV(30,30),DIFF(30),
     1  IRAN(32),VALFIX(20),PX(32),SSND(MAXNUMEQ),W(MAXNUMEQ),
     2  RANFIXEST(20)
	COMMON/TOMAP/IRAN,VALFIX,SSND,SIGFAC,OFAC,ESTMEN,ESTINV,
     1  DET,NOFIX,NUMEQT,RANFIXEST,NRANFIX
        integer JSUB,IG
        integer, dimension(128)::INTLIST
        double precision, dimension(257)::RPAR
        integer, dimension(257)::IPAR
      NNNVAR=0
      DO I=1,NVAR+NOFIX+NRANFIX
       IF(IRAN(I) .EQ. 1 .OR. IRAN(I) .EQ. -1) THEN
        NNNVAR=NNNVAR+1
        X(NNNVAR) = VEC(NNNVAR)
         IF(IRAN(I) .EQ. 1 .AND. X(NNNVAR) .LT. 0.D0) THEN
          FNTVAL = 1.D30
          RETURN
         ENDIF
       ENDIF
      END DO
	CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,RANFIXEST,PX)
	CALL IDPC(JSUB,IG,PX,W,INTLIST,RPAR,IPAR)
	WTOTAL = 0.D0
	DO IEQ=1,NUMEQT
	 SSND(IEQ) = W(IEQ)
	 WTOTAL = WTOTAL + W(IEQ)
	END DO
	PYXLOG = .5D0*WTOTAL + DLOG(SIGFAC) + DLOG(OFAC)
	DO J=1,NVAR
	 DIFF(J) = VEC(J) - ESTMEN(J)
	END DO
	SUM=0.D0
	 DO I=1,NVAR
	  DO J=1,NVAR
	   SUM = SUM + DIFF(I)*ESTINV(I,J)*DIFF(J)
	  END DO
	 END DO
	PVECLOG = .5D0*NVAR*DLOG(2.D0*3.1415926)+.5D0*DLOG(DET)+.5D0*SUM
	FNTVAL = PYXLOG + PVECLOG
	RETURN
	END
	SUBROUTINE FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)
        IMPLICIT REAL*8(A-H,O-Z)
        PARAMETER(MAXNUMEQ=7)
        DIMENSION TIM(594),SIG(5000),RS(5000,34),YO(594,NUMEQT),
     1  BS(5000,7),C0(NUMEQT),C1(NUMEQT),C2(NUMEQT),C3(NUMEQT),
     2  C4(NUMEQT),C5(NUMEQT),YOO(594,MAXNUMEQ)
        COMMON /OBSER/ TIM,SIG,RS,YOO,BS
        COMMON /CNST/ N,ND,NI,NUP,NUIC,NP
        COMMON /CNST2/ NPL,NUMEQTT,NDRUG,NADD
        COMMON /SUM2/ M,NPNL
        COMMON/DESCR/AGE,HEIGHT,ISEX,IETHFLG
        COMMON/ERR/ERRFIL
        CHARACTER SEX*1,READLINE*300,ERRFIL*20
        integer, dimension(128) :: INTLIST
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
        INTLIST(1) = int(AGE)
        INTLIST(2) = ISEX
        INTLIST(3) = int(HEIGHT)
        INTLIST(4) = IETHFLAG
    1   FORMAT(A300)
   10	  READ(27,1) READLINE
        IF(READLINE(12:23) .NE. 'NO. OF DRUGS') GO TO 10
        BACKSPACE(27)
    3   FORMAT(T2,I5)
        READ(27,3) NDRUG
        INTLIST(5)=NDRUG
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
        INTLIST(6) = NADD
        INTLIST(7) = NI
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
        INTLIST(8)=ND
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
   40	  READ(27,1) READLINE
        IF(READLINE(12:23) .NE. 'NO. OF TOTAL') GO TO 40
        BACKSPACE(27)
        READ(27,3) NUMEQTT
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
        INTLIST(10)=M
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
   50	  READ(27,1) READLINE
        IF(READLINE(1:25) .NE. 'ASSAY COEFFICIENTS FOLLOW') GO TO 50
        DO IEQ = 1,NUMEQT
         READ(27,*) C0(IEQ),C1(IEQ),C2(IEQ),C3(IEQ)
         C4(IEQ) = 0
         C5(IEQ) = 0
        END DO
        RETURN
        END
	SUBROUTINE CALCOV(NVAR,NOBACT,PMAT,RINV,COVINV)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION PMAT(594,30),COVINV(30,30),RINV(594,594),
     1  PMATT(30,594),WORK(30,594)
	DO I=1,NOBACT
	 DO J=1,NVAR
	  PMATT(J,I)=PMAT(I,J)
	 END DO
	END DO
	CALL MULT1(NVAR,NOBACT,NOBACT,PMATT,RINV,WORK)
	CALL MULT2(NVAR,NOBACT,NVAR,WORK,PMAT,COVINV)
	RETURN
	END
	SUBROUTINE MULT1(N1,N2,N3,A,B,C)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION A(30,594),B(594,594),C(30,594)
	DO I=1,N1
	 DO J=1,N3
	  SUM=0.D0
	   DO K=1,N2
            SUM=SUM+A(I,K)*B(K,J)
	   END DO
	  C(I,J)=SUM
	 END DO
	END DO
	RETURN
	END
	SUBROUTINE MULT2(N1,N2,N3,A,B,C)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION A(30,594),B(594,30),C(30,30)
	DO I=1,N1
	 DO J=1,N3
	  SUM=0.D0
	   DO K=1,N2
            SUM=SUM+A(I,K)*B(K,J)
	   END DO
	  C(I,J)=SUM
	 END DO
	END DO
	RETURN
	END
        SUBROUTINE MATNV2(A,N,B,M,DETERM)
	IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION PIVOT(100),INDEX(100)
      DIMENSION A(30,30),B(30,1)
      DATA DETMAX,DETMIN/1.0D+30,1.0D-30/
      DETERM=1.D0
      IDET=0
      DO 20 I=1,N
      PIVOT(I)=0.D0
   20 INDEX(I)=0.0
      DO 550 I=1,N
      AMAX=0.D0
      DO 105 J=1,N
      IF (PIVOT(J).NE.0.D0) GO TO 105
      DO 100 K=1,N
      IF (PIVOT(K).NE.0.D0) GO TO 100
      TEMP=ABS(A(J,K))
      IF (TEMP.LT.AMAX) GO TO 100
      IROW=J
      ICOLUM=K
      AMAX=TEMP
  100 CONTINUE
  105 CONTINUE
      INDEX(I)=4096*IROW+ICOLUM
      J=IROW
      AMAX=A(J,ICOLUM)
      DETERM=AMAX*DETERM
      IF (ABS(DETERM).LT.DETMAX) GO TO 130
      DETERM=DETERM*DETMIN
      IDET=IDET+1
      GO TO 140
  130 IF (ABS(DETERM).GT.DETMIN) GO TO 140
      DETERM=DETERM*DETMAX
      IDET=IDET-1
  140 CONTINUE
      IF (DETERM.EQ.0.D0) GO TO 600
      PIVOT(ICOLUM)=AMAX
      IF (IROW.EQ.ICOLUM) GO TO 260
      DETERM=-DETERM
      DO 200 K=1,N
      SWAP=A(J,K)
      A(J,K)=A(ICOLUM,K)
      A(ICOLUM,K)=SWAP
  200 CONTINUE
      IF (M.LE.0) GO TO 260
      DO 250 K=1,M
      SWAP=B(J,K)
      B(J,K)=B(ICOLUM,K)
      B(ICOLUM,K)=SWAP
  250 CONTINUE
  260 K=ICOLUM
      A(ICOLUM,K)=1.D0
      DO 350 K=1,N
      A(ICOLUM,K)=A(ICOLUM,K)/AMAX
  350 CONTINUE
      IF (M.LE.0) GO TO 380
      DO 370 K=1,M
      B(ICOLUM,K)=B(ICOLUM,K)/AMAX
  370 CONTINUE
  380 DO 550 J=1,N
      IF (J.EQ.ICOLUM) GO TO 550
      T=A( J,ICOLUM)
      A( J,ICOLUM)=0.D0
      DO 450 K=1,N
      A( J,K)=A( J,K)-A(ICOLUM,K)*T
  450 CONTINUE
      IF (M.LE.0) GO TO 550
      DO 500 K=1,M
      B( J,K)=B( J,K)-B(ICOLUM,K)*T
  500 CONTINUE
  550 CONTINUE
  600 DO 710 I=1,N
      I1=N+1-I
      K=INDEX(I1)/4096
      ICOLUM=INDEX(I1)-4096*K
      IF (K.EQ.ICOLUM) GO TO 710
      DO 705 J=1,N
      SWAP=A(J,K)
      A(J,K)=A(J,ICOLUM)
      A(J,ICOLUM)=SWAP
  705 CONTINUE
  710 CONTINUE
      PIVOT(1)=IDET
      RETURN
      END
	SUBROUTINE CALCPIK(NVAR,COVINV,ESTINV,PIK)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION B(30,1),COVINV(30,30),ESTINV(30,30),PIK(30,30)
	DO I=1,NVAR
	 DO J=1,NVAR
	  PIK(I,J) = COVINV(I,J) + ESTINV(I,J)
	 END DO
	END DO
	CALL MATNV2(PIK,NVAR,B,1,DET)
	IF(DET .LE. 0) WRITE(*,*)' PIK IS SINGULAR IN CALCPIK.'
	IF(DET .LE. 0) WRITE(25,*)' PIK IS SINGULAR IN CALCPIK.'
	RETURN
	END
	SUBROUTINE GETMED(NVAR,NSUB,MAXSUB,MAXDIM,IESTIJ,PAREST,VEC,
     1  ESTMED)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION PAREST(MAXSUB,MAXDIM),IESTIJ(MAXSUB,MAXDIM),
     1  VEC(MAXSUB),ESTMED(30)
	DO 100 J=1,NVAR
	IND=0
	DO I=1,NSUB
	  IF(IESTIJ(I,J) .EQ. 1) THEN
	    IND=IND+1
	    VEC(IND)=PAREST(I,J)
	  ENDIF
	END DO
	IHALF=IND/2
	NN=2*IHALF
	IF(NN .EQ. IND) IEVEN=1
	IF(NN .NE. IND) IEVEN=0
	DO 50 NOW=1,IND-1
	DO 50 I=NOW+1,IND
	IF(VEC(I) .LT. VEC(NOW)) THEN
	  V1=VEC(I)
	  V2=VEC(NOW)
	  VEC(I)=V2
	  VEC(NOW)=V1
	ENDIF
   50   CONTINUE
	IF(IEVEN .EQ. 0) ESTMED(J) = VEC((IND+1)/2)
	IF(IEVEN .EQ. 1) ESTMED(J) = (VEC(IND/2) + VEC(IND/2 + 1))/2.D0
  100   CONTINUE
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
      SUBROUTINE Old_MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,
     2 RANFIXEST,PX)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION IRAN(32),X(30),VALFIX(20),PX(32),RANFIXEST(20)
      NNNVAR = 0
      NNNFIX = 0
      NNNRANFIX = 0
	DO I=1,NVAR+NOFIX+NRANFIX
	IF(IRAN(I) .EQ. 1 .OR. IRAN(I) .EQ. -1) THEN
	 NNNVAR=NNNVAR+1
	 PX(I) = X(NNNVAR)
	ENDIF
	IF(IRAN(I) .EQ. 0) THEN
	 NNNFIX=NNNFIX+1
	 PX(I) = VALFIX(NNNFIX)
	ENDIF
       IF(IRAN(I) .EQ. 2) THEN
        NNNRANFIX = NNNRANFIX+1
        PX(I) = RANFIXEST(NNNRANFIX)
       ENDIF
	END DO
	RETURN
	END
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
	 IF(IFILE .EQ. 24) WRITE(24,222) IFIRST
	 IF(IFILE .EQ. 25) WRITE(25,222) IFIRST
  222    FORMAT(1X,I5)
	 GO TO 100
	ENDIF
	IF(IPATVEC(NEXTIND+1) .NE. IFIRST + 1) THEN
	 IF(IFILE .EQ. 24) WRITE(24,222) IFIRST
	 IF(IFILE .EQ. 25) WRITE(25,222) IFIRST
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
   	IF(IFILE .EQ. 24) WRITE(24,221) IFIRST,ILAST
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
      SUBROUTINE NEWWORK1(JSUB)
      use npag_utils, only : thesame
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(MAXNUMEQ=7)
      DIMENSION SIG(5000),RS(5000,34),DELTAIV(7),ORDELT(7),
     1 RSS(5000,34),SIGG(5000),TIM(594),TIMM(594),YO(594,MAXNUMEQ),
     2 TIMDELAY(99),OBSBLOCK(800,150,MAXNUMEQ+1),
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
       READ(23,3) NUMEQT
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
     1' FROM THE INSTRUCTION FILE, it2b102.inp. IF YOU EDITED THIS'/
     2' FILE MANUALLY, PLEASE RERUN THE PC PREP PROGRAM TO HAVE IT'/
     3' PREPARE it2b102.inp AGAIN AND THEN RERUN THIS PROGRAM.'//
     4' IF YOU DID NOT MANUALLY EDIT it2b102.inp, PLEASE SEND THE'/
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
         WRITE(42,101) ID,XVERIFY(1),IDRUG,XVERIFY(2),XVERIFY(3)
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
         WRITE(42,4031) XVERIFY(1)
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
      use npag_utils, only : thesame
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
      SUBROUTINE VERIFYVAL(N,X)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(100)
      DO I = 1,N
       IF(X(I) .GE. -1.D-99 .AND. X(I) .LE. 1.D-99) X(I) = 0.D0
      END DO
      RETURN
      END
	SUBROUTINE CALCRF(NTOTPAR,VEC,FNTVAL,NUMEQT,YO,C0,C1,C2,C3,
     1  C4,C5,GAMMA,JSUB,IG,INTLIST)
	IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(MAXNUMEQ=7)
	DIMENSION VEC(NTOTPAR),IRAN(32),PX(32),SIG(594,MAXNUMEQ),
     1 YO(594,NUMEQT),C0(NUMEQT),C1(NUMEQT),C2(NUMEQT),C3(NUMEQT),
     2 C4(NUMEQT),C5(NUMEQT),W(MAXNUMEQ),GAMMA(NUMEQT)
      COMMON SIG
	COMMON/TOCALC/IRAN,PX,NOFIX,NSUB
      integer JSUB,IG
      integer, dimension(128)::INTLIST
      NVEC = 0
      DO I = 1,NTOTPAR+NOFIX
       IF(IRAN(I) .EQ. 2) THEN
        NVEC = NVEC + 1
        PX(I) = VEC(NVEC)
       ENDIF
      END DO
      DO I = 1,NTOTPAR+NOFIX
       IF(IRAN(I) .EQ. 1 .OR. IRAN(I) .EQ. -1) THEN
        NVEC = NVEC + 1
        IF(VEC(NVEC) .LT. 0.D0 .AND. IRAN(I) .EQ. 1) THEN
         FNTVAL = 1.D30
         RETURN
        ENDIF
        PX(I) = VEC(NVEC)
       ENDIF
      END DO
      SUMTOT = 0.D0
      REWIND(27)
      DO JSUB = 1,NSUB
       CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)
       DO 140 I=1,NOBSER
        DO 140 J=1,NUMEQT
         Y = YO(I,J)
         IF(Y .EQ. -99) GO TO 140
         SIG(I,J) = GAMMA(J)*(C0(J)+C1(J)*Y+C2(J)*Y*Y+C3(J)*Y**3)
  140    CONTINUE
       CALL IDPC(JSUB,IG,PX,W,INTLIST,RPAR,IPAR)
       WTOTAL = 0.D0
       DO IEQ = 1,NUMEQT
        WTOTAL = WTOTAL + W(IEQ)
       END DO
       SUMTOT = SUMTOT + WTOTAL
      END DO
      FNTVAL = SUMTOT
	RETURN
	END
        SUBROUTINE ELDERY2(N,START,XMIN,YNEWLO,REQMIN,STEP,
     X  ITMAX,FUNC,IPRINT,ICONV,NITER,ICOUNT,NUMEQT,YO,C0,C1,C2,C3,
     1  C4,C5,GAMMA,JSUB,IG,INTLIST)
      IMPLICIT REAL*8(A-H,O-Z)
        DIMENSION START(N),STEP(N),XMIN(N),XSEC(30),
     X  P(30,31),PSTAR(30),P2STAR(30),PBAR(30),Y(31),YO(594,NUMEQT),
     1  C0(NUMEQT),C1(NUMEQT),C2(NUMEQT),C3(NUMEQT),C4(NUMEQT),
     2  C5(NUMEQT),GAMMA(NUMEQT)
        integer JSUB,IG
        integer, dimension(128)::INTLIST
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
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,GAMMA,
     1    JSUB,IG,INTLIST)
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
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,GAMMA,
     1    JSUB,IG,INTLIST)
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
        CALL FUNC(N,PSTAR,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     1    GAMMA,JSUB,IG,INTLIST)
        YSTAR=FN
        ICOUNT=ICOUNT+1
        IF(YSTAR.GE.YLO) GO TO 12
        IF(ICOUNT.GE.KCOUNT) GO TO 19
        DO 9 I=1,N
9       P2STAR(I)=ECOEFF*PSTAR(I)+(1.0D0-ECOEFF)*PBAR(I)
        CALL FUNC(N,P2STAR,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     1    GAMMA,JSUB,IG,INTLIST)
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
        CALL FUNC(N,P2STAR,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     1    GAMMA,JSUB,IG,INTLIST)
        Y2STAR=FN
        ICOUNT=ICOUNT+1
        IF(Y2STAR.LT.Y(IHI)) GO TO 10
        DO 18 J=1,NN
        DO 17 I=1,N
        P(I,J)=(P(I,J)+P(I,ILO))*0.5D0
17      XMIN(I)=P(I,J)
        CALL FUNC(N,XMIN,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     1   GAMMA,JSUB,IG,INTLIST)
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
        CALL FUNC(N,XMIN,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,GAMMA,
     1   JSUB,IG,INTLIST)
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
        SUBROUTINE READOUT(OUTFILER,IRAN)
        IMPLICIT REAL*8(A-H,O-Z)
        PARAMETER(MAXNUMEQ=7)
        DIMENSION YO(150,MAXNUMEQ),AB(30,2),VALFIX(20),
     1 YPREDPOP(800,MAXNUMEQ,150,3),YPREDBAY(800,MAXNUMEQ,150,3),
     2 C0(MAXNUMEQ),C1(MAXNUMEQ),C2(MAXNUMEQ),C3(MAXNUMEQ),IPATVEC(800),
     3 NOBS(800),NDOSEV(800),PARBAY(800,2,30),IRAN(32),RANFIXEST(20)
        DIMENSION ALOGLIK(40999),XMEAN(40999,30),XMED(40999,30),
     1  GAMMA(40999,MAXNUMEQ),STDEV(40999,30),PRCFVR(40999,30),
     2  OBSBLOCK(800,150,MAXNUMEQ+1),DOSEBLOCK(800,1000,35),
     3  AGE(800),HEIGHT(800),SUBMEAN(800,30),SUBSTD(800,30),
     4  SUBPERCOF(800,30),NDD(800),ASSAYC(800,MAXNUMEQ,4),
     5  IGAMMA(MAXNUMEQ),AF(7),NDORIG(800),XVERIFY(100)
        CHARACTER PAR(30)*11,PARFIX(20)*11,READLINE*80,
     1   NAME(800)*53,CHARTNO(800)*53,SEX(800)*1,PARRANFIX(20)*11,
     2   DESCR(26)*20,OUTFILER*20,READLINE2*1000,ANS*5
        CHARACTER(LEN=20) :: OSName
   	COMMON/DOSEOBS/DOSEBLOCK,OBSBLOCK,NDORIG
      WRITE(*,911)
  911 FORMAT(//' NOW CREATING THE IT_RFxxxx.TXT FILE ...')
        MAXOBDIM = 150
        MAXSUB = 800
    1   FORMAT(A20)
    2   FORMAT(A80)
        CALL GETNUM(NUMEQT)
        CALL GETNSUB2(NSUBTOT)
        CALL GETNSUB(NSUB)
        CALL GETIPATFF(25,NSUBTOT,NSUB,MAXSUB,IPATVEC,IERRR)
        IF(IERRR .EQ. -1) STOP
   90	BACKSPACE(28)
	BACKSPACE(28)
	READ(28,2) READLINE
	IF(READLINE(2:13) .NE. 'SUMMARY INFO') GO TO 90
	READ(28,*)
	READ(28,*)
	READ(28,5301) NSUB
 5301 FORMAT(T28,I3)
	READ(28,*)
	READ(28,5302) NVAR
 5302 FORMAT(T30,I2)
	READ(28,*)
	READ(28,*)
	READ(28,5303) (PAR(I),I=1,NVAR)
 5303 FORMAT(30(A11,1X))
  550   READ(28,2) READLINE
        IF(READLINE(6:24) .NE. 'CORRESPONDING BOUND') GO TO 550
        DO I = 1,NVAR
         READ(28,*) (AB(I,J),J=1,2)
        END DO
  190	READ(28,2) READLINE
	IF(READLINE(2:22) .NE. 'THE NUMBER OF FIXED V') GO TO 190
	BACKSPACE(28)
	READ(28,191) NOFIX
  191 FORMAT(T41,I2)
	IF(NOFIX .GT. 0) THEN
	 READ(28,*)
	 READ(28,*)
	 DO I=1,NOFIX
	  READ(28,192) PARFIX(I),VALFIX(I)
	 END DO
  192   FORMAT(T2,A11,3X,G17.10)
	ENDIF
  290	READ(28,2) READLINE
	IF(READLINE(2:22) .NE. 'THE NUMBER OF RANFIX ') GO TO 290
	BACKSPACE(28)
	READ(28,291) NRANFIX
  291 FORMAT(T37,I2)
      IF(NRANFIX .GT. 0) THEN
       READ(28,*)
       READ(28,*)
       READ(28,*)
       DO I=1,NRANFIX
        READ(28,192) PARRANFIX(I),RANFIXEST(I)
       END DO
      ENDIF
	REWIND(28)
	REWIND(27)
  510  READ(28,2) READLINE
	 IF(READLINE(2:25) .NE. 'CYCLE POPULATION MEDIANS') GO TO 510
	 READ(28,*)
        DO JSUB=1,NSUB
	   CALL FILREDT(NOBSER,YO,C0,C1,C2,C3,MAXOBDIM,NDRUG,ND,NADD)
         NOBS(JSUB) = NOBSER
         NDOSEV(JSUB) = ND
          DO IEQ=1,NUMEQT
           DO IOBS=1,NOBSER
            READ(28,*) (YPREDPOP(JSUB,IEQ,IOBS,ICEN),ICEN=1,2)
           END DO
          END DO
         END DO
	REWIND(27)
  515    READ(28,2) READLINE
	 IF(READLINE(2:17) .NE. 'MEDIANS), FOLLOW') GO TO 515
	 READ(28,*)
        DO JSUB=1,NSUB
         CALL FILREDT(NOBSER,YO,C0,C1,C2,C3,MAXOBDIM,NDRUG,ND,NADD)
          DO IEQ=1,NUMEQT
           DO IOBS=1,NOBSER
            READ(28,*) (YPREDBAY(JSUB,IEQ,IOBS,ICEN),ICEN=1,2)
           END DO
          END DO
         END DO
  520    READ(28,2) READLINE
	 IF(READLINE(2:28) .NE. 'AND THEN ON THE FINAL CYCLE') GO TO 520
	 READ(28,*)
	 DO JSUB=1,NSUB
	  DO J=1,NVAR
	   READ(28,*) (PARBAY(JSUB,ICEN,J),ICEN=1,2)
	  END DO
	 END DO
	CLOSE(28)
        REWIND(25)
  210   READ(25,2) READLINE
        IF(READLINE(2:17) .NE. 'GAMMA FOR OUTPUT') GO TO 210
        IF(READLINE(34:38) .EQ. 'ESTIM') IGAMMA(1) = 0
        IF(READLINE(34:38) .EQ. 'FIXED') IGAMMA(1) = 1
        IF(NUMEQT .GT. 1) THEN
         DO IEQ = 2,NUMEQT
          READ(25,*)
          READ(25,2) READLINE
          IF(READLINE(34:38) .EQ. 'ESTIM') IGAMMA(IEQ) = 0
          IF(READLINE(34:38) .EQ. 'FIXED') IGAMMA(IEQ) = 1
         END DO
        ENDIF
  220   READ(25,2) READLINE
        IF(READLINE(2:17) .NE. 'THE ACTIVE (SALT') GO TO 220
        READ(25,*)
        READ(25,*) (AF(I),I=1,NDRUG)
  230   READ(25,2) READLINE
        IF(READLINE(2:17) .NE. 'THE STOPPING TOL') GO TO 230
        READ(25,*) TOL
  240   READ(25,2) READLINE
        IF(READLINE(2:17) .NE. 'THE MAXIMUM NO. ') GO TO 240
        READ(25,*) MAXIT
  245   READ(25,2) READLINE
        IF(READLINE(2:17) .NE. 'THE TOLERANCE PA') GO TO 245
        READ(25,*)
        READ(25,*) RTOL
        CALL CONVERGE2(NCYCLE,ALOGLIK,XMEAN,XMED,GAMMA,STDEV,
     1   PRCFVR,SUBMEAN,SUBSTD,SUBPERCOF,NAME,CHARTNO,SEX,NDD,NI,
     2   AGE,HEIGHT,NUMEQT,ASSAYC,AIC,BIC,ICONVERGE)
        OPEN(21)
        NLINE = 0
        WRITE(21,101)
  101   FORMAT(' VERSION 1.5 - MAR 2016')
        NLINE = NLINE + 1
        WRITE(21,102)
  102   FORMAT(/' # Run information')
        NLINE = NLINE + 2
        WRITE(21,103) NSUB
  103   FORMAT(15X,I6,'   # NSUB')
        NLINE = NLINE + 1
        WRITE(21,106) NVAR
  106   FORMAT(15X,I6,'   # NVAR')
        NLINE = NLINE + 1
        WRITE(21,107) NOFIX
  107   FORMAT(15X,I6,'   # NOFIX')
        NLINE = NLINE + 1
       WRITE(21,307) NRANFIX
  307   FORMAT(15X,I6,'   # NRANFIX')
        NLINE = NLINE + 1
        WRITE(21,1101) MAXIT
 1101   FORMAT(15X,I6,'   # MAXIMUM NO. OF ITERATIONS')
        NLINE = NLINE + 1
        WRITE(21,111) NCYCLE
  111   FORMAT(15X,I6,'   # NO. OF ITERATIONS RUN')
        NLINE = NLINE + 1
        XVERIFY(1) = TOL
        CALL VERIFYVAL(1,XVERIFY)
        WRITE(21,1102) XVERIFY(1)
 1102   FORMAT(2X,F19.17,'   # STOPPING TOLERANCE')
        NLINE = NLINE + 1
        WRITE(21,1103) ICONVERGE
 1103   FORMAT(15X,I6,'   # CONVERGENCE FLAG ')
        NLINE = NLINE + 1
        XVERIFY(1) = RTOL
        CALL VERIFYVAL(1,XVERIFY)
        WRITE(21,1104) XVERIFY(1)
1104   FORMAT(2X,F19.17,'   # O.D.E. TOLERANCE ')
        NLINE = NLINE + 1
        WRITE(21,112) NUMEQT
  112   FORMAT(15X,I6,'   # NUMEQT')
        NLINE = NLINE + 1
        DO I = 1,NUMEQT
         WRITE(21,1106) IGAMMA(I),I
         NLINE = NLINE + 1
 1106    FORMAT(15X,I6,'   # IGAMMA: 0(EST) OR 1(FIXED) FOR EQ. ',I1)
        END DO
        WRITE(21,113) NDRUG
  113   FORMAT(15X,I6,'   # NDRUG ')
        NLINE = NLINE + 1
        DO I = 1,NDRUG
         XVERIFY(1) = AF(I)
         CALL VERIFYVAL(1,XVERIFY)
         WRITE(21,1107) XVERIFY(1),I
         NLINE = NLINE + 1
 1107    FORMAT(2X,F19.17,'   # ACTIVE (SALT) FRACTION FOR DRUG ',I1)
        END DO
        DO JSUB = 1,NSUB
         WRITE(21,114) NDOSEV(JSUB),JSUB
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
  124   FORMAT(8X,'   # START RANGES FOR NPAG, AND IF EST MUST BE > 0')
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
        WRITE(21,326)
  326   FORMAT(8X,'   # START AIC AND BIC VALUES')
        NLINE = NLINE + 1
        WRITE(21,129)
  129   FORMAT(8X,'   # START YPREDPOP')
        NLINE = NLINE + 1
        WRITE(21,229)
  229   FORMAT(8X,'   # START YPREDBAY')
        NLINE = NLINE + 1
        WRITE(21,131)
  131   FORMAT(8X,'   # START PARBAY')
        NLINE = NLINE + 1
        WRITE(21,134)
  134   FORMAT(8X,'   # START CYCLE AVERAGE LOG-LIKS')
        NLINE = NLINE + 1
        WRITE(21,136)
  136   FORMAT(8X,'   # START CYCLE MEANS')
        NLINE = NLINE + 1
        WRITE(21,177)
  177   FORMAT(8X,'   # START CYCLE MEDIANS')
        NLINE = NLINE + 1
        WRITE(21,137)
  137   FORMAT(8X,'   # START CYCLE STD. DEVS.')
        NLINE = NLINE + 1
        WRITE(21,139)
  139   FORMAT(8X,'   # START CYCLE % COEFF. OF VARS.')
        NLINE = NLINE + 1
        WRITE(21,138)
  138   FORMAT(8X,'   # START CYCLE GAMMA EST. VALUES')
        NLINE = NLINE + 1
        WRITE(21,141)
  141   FORMAT(8X,'   # START LAST CYCLE SUBJ. PAR. ESTIMATES')
        NLINE = NLINE + 1
        WRITE(21,143)
  143   FORMAT(8X,'   # START LAST CYCLE SUBJ. STD. DEVS.')
        NLINE = NLINE + 1
        WRITE(21,142)
  142   FORMAT(8X,'   # START LAST CYCLE SUBJ. % COEFF. OF VARS.')
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
1717     FORMAT(A11)
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
  154   FORMAT(/8X,'   # PARAMETER BOUNDARIES',17X,'MUST EST. BE .GE. 0'
     1)
        NLINE = NLINE + 2
        NLAB = NLINE
      NP = NVAR + NOFIX + NRANFIX
      NORAN = 0
      DO I = 1,NP
       IF(IRAN(I) .EQ. 0 .OR. IRAN(I) .EQ. 2) GO TO 2220
       NORAN = NORAN + 1
       IF(IRAN(I) .EQ. 1) ANS = '  YES'
       IF(IRAN(I) .EQ. -1) ANS = '  NO '
       WRITE(21,*) AB(NORAN,1),AB(NORAN,2),ANS
       NLINE = NLINE + 1
 2220  CONTINUE
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
        WRITE(21,259)
  259   FORMAT(/8X,'   # AIC AND BIC VALUES')
        NLINE = NLINE + 2
        NLAICBIC = NLINE
          WRITE(21,*) AIC,BIC
          NLINE = NLINE + 1
        WRITE(21,159)
  159   FORMAT(/8X,'   # YPREDPOP ARRAY')
        NLINE = NLINE + 2
        NLYPREDPOP = NLINE
        DO JSUB = 1,NSUB
         DO IEQ = 1,NUMEQT
          DO IOBS = 1,NOBS(JSUB)
           DO ICEN = 1,2
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
           DO ICEN = 1,2
            WRITE(21,*) YPREDBAY(JSUB,IEQ,IOBS,ICEN)
            NLINE = NLINE + 1
           END DO
          END DO
         END DO
        END DO
        WRITE(21,162)
  162   FORMAT(/8X,'   # PARBAY ARRAY')
        NLINE = NLINE + 2
        NLPARBAY = NLINE
        DO JSUB=1,NSUB
         DO J=1,NVAR
          DO ICEN = 1,2
           WRITE(21,*) PARBAY(JSUB,ICEN,J)
           NLINE = NLINE + 1
          END DO
         END DO
        END DO
        WRITE(21,166)
  166   FORMAT(/8X,'   # CYCLE AVERAGE LOG-LIKS')
        NLINE = NLINE + 2
        NLCYCAVGLOGLIK = NLINE
        DO ICYCLE = 1,NCYCLE
         WRITE(21,*) ALOGLIK(ICYCLE)
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
        WRITE(21,169)
  169   FORMAT(/8X,'   # CYCLE MEDIAN VECTORS')
        NLINE = NLINE + 2
        NLCYCMEDIAN = NLINE
        DO ICYCLE = 1,NCYCLE
         DO J = 1,NVAR
          WRITE(21,*) XMED(ICYCLE,J)
          NLINE = NLINE + 1
         END DO
        END DO
        WRITE(21,168)
  168   FORMAT(/8X,'   # CYCLE STD. DEV. VECTORS')
        NLINE = NLINE + 2
        NLCYCSTDEV = NLINE
        DO ICYCLE = 1,NCYCLE
         DO J = 1,NVAR
          WRITE(21,*) STDEV(ICYCLE,J)
          NLINE = NLINE + 1
         END DO
        END DO
        WRITE(21,178)
  178   FORMAT(/8X,'   # CYCLE % COEFF. OF VARS. VECTORS')
        NLINE = NLINE + 2
        NLCYCPRCFVR = NLINE
        DO ICYCLE = 1,NCYCLE
         DO J = 1,NVAR
          WRITE(21,*) PRCFVR(ICYCLE,J)
          NLINE = NLINE + 1
         END DO
        END DO
        WRITE(21,171)
  171   FORMAT(/8X,'   # CYCLE GAMMA EST. VALUES')
        NLINE = NLINE + 2
        NLCYCGAM = NLINE
        DO ICYCLE = 1,NCYCLE
         DO J = 1,NUMEQT
          WRITE(21,*) GAMMA(ICYCLE,J)
          NLINE = NLINE + 1
         END DO
        END DO
        WRITE(21,173)
  173   FORMAT(/8X,'   # LAST CYCLE SUBJ. PAR. ESTIMATES')
        NLINE = NLINE + 2
        NLLASTCYCPAREST = NLINE
        DO JSUB = 1,NSUB
         DO J = 1,NVAR
          WRITE(21,*) SUBMEAN(JSUB,J)
          NLINE = NLINE + 1
         END DO
        END DO
        WRITE(21,174)
  174   FORMAT(/8X,'   # LAST CYCLE SUBJ. STD. DEVS')
        NLINE = NLINE + 2
        NLLASTCYCSTD = NLINE
        DO JSUB = 1,NSUB
         DO J = 1,NVAR
          WRITE(21,*) SUBSTD(JSUB,J)
          NLINE = NLINE + 1
         END DO
        END DO
        WRITE(21,176)
  176   FORMAT(/8X,'   # LAST CYCLE SUBJ. % COEFF. OF VARS')
        NLINE = NLINE + 2
        NLLASTCYCPER = NLINE
        DO JSUB = 1,NSUB
         DO J = 1,NVAR
          WRITE(21,*) SUBPERCOF(JSUB,J)
          NLINE = NLINE + 1
         END DO
        END DO
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
1182     FORMAT(15X,F10.3,15X,A1,15X,F10.3)
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
  186   FORMAT(15X,I10,'   # START PAR')
  260   READ(21,2) READLINE
        IF(READLINE(12:25) .NE. '# START PARFIX') THEN
         CALL CONDENSE2(READLINE)
         GO TO 260
        ENDIF
        WRITE(22,187) NLFIXPAR
  187   FORMAT(15X,I10,'   # START PARFIX')
  960   READ(21,2) READLINE
        IF(READLINE(12:28) .NE. '# START PARRANFIX') THEN
         CALL CONDENSE2(READLINE)
         GO TO 960
        ENDIF
        WRITE(22,1187) NLRANFIXPAR
 1187   FORMAT(15X,I10,'   # START PARRANFIX')
  270   READ(21,2) READLINE
        IF(READLINE(12:25) .NE. '# START RANGES') THEN
         CALL CONDENSE2(READLINE)
         GO TO 270
        ENDIF
        WRITE(22,188) NLAB
  188   FORMAT(15X,I10,'   # START RANGES FOR NPAG, AND IF EST MUST BE >
     1 0')
 1280   READ(21,2) READLINE
        IF(READLINE(12:25) .NE. '# START VALFIX') THEN
         CALL CONDENSE2(READLINE)
         GO TO 1280
        ENDIF
        WRITE(22,189) NLFIXVAL
  189   FORMAT(15X,I10,'   # START VALFIX')
1380   READ(21,2) READLINE
        IF(READLINE(12:28) .NE. '# START RANFIXEST') THEN
         CALL CONDENSE2(READLINE)
         GO TO 1380
        ENDIF
        WRITE(22,1189) NLRANFIXVAL
 1189   FORMAT(15X,I10,'   # START RANFIXEST')
  840   READ(21,2) READLINE
        IF(READLINE(12:25) .NE. '# START COVARI') THEN
         CALL CONDENSE2(READLINE)
         GO TO 840
        ENDIF
        WRITE(22,841) NLCOVNAM
  841   FORMAT(15X,I10,'   # START COVARIATE NAMES')
  940   READ(21,2) READLINE
        IF(READLINE(12:25) .NE. '# START AIC AN') THEN
         CALL CONDENSE2(READLINE)
         GO TO 940
        ENDIF
        WRITE(22,941) NLAICBIC
  941   FORMAT(15X,I10,'   # START AIC AND BIC VALUES')
  320   READ(21,2) READLINE
        IF(READLINE(12:27) .NE. '# START YPREDPOP') THEN
         CALL CONDENSE2(READLINE)
         GO TO 320
        ENDIF
        WRITE(22,193) NLYPREDPOP
  193   FORMAT(15X,I10,'   # START YPREDPOP')
  330   READ(21,2) READLINE
        IF(READLINE(12:27) .NE. '# START YPREDBAY') THEN
         CALL CONDENSE2(READLINE)
         GO TO 330
        ENDIF
        WRITE(22,194) NLYPREDBAY
  194   FORMAT(15X,I10,'   # START YPREDBAY')
  350   READ(21,2) READLINE
        IF(READLINE(12:25) .NE. '# START PARBAY') THEN
         CALL CONDENSE2(READLINE)
         GO TO 350
        ENDIF
        WRITE(22,197) NLPARBAY
  197   FORMAT(15X,I10,'   # START PARBAY')
  370   READ(21,2) READLINE
        IF(READLINE(12:28) .NE. '# START CYCLE AVE') THEN
         CALL CONDENSE2(READLINE)
         GO TO 370
        ENDIF
        WRITE(22,199) NLCYCAVGLOGLIK
  199   FORMAT(15X,I10,'   # START CYCLE AVERAGE LOG-LIKS')
  380   READ(21,2) READLINE
        IF(READLINE(12:28) .NE. '# START CYCLE MEA') THEN
         CALL CONDENSE2(READLINE)
         GO TO 380
        ENDIF
        WRITE(22,201) NLCYCMEAN
  201   FORMAT(15X,I10,'   # START CYCLE MEANS')
1370   READ(21,2) READLINE
        IF(READLINE(12:28) .NE. '# START CYCLE MED') THEN
         CALL CONDENSE2(READLINE)
         GO TO 1370
        ENDIF
        WRITE(22,1199) NLCYCMEDIAN
 1199   FORMAT(15X,I10,'   # START CYCLE MEDIANS')
  390   READ(21,2) READLINE
        IF(READLINE(12:28) .NE. '# START CYCLE STD') THEN
         CALL CONDENSE2(READLINE)
         GO TO 390
        ENDIF
        WRITE(22,202) NLCYCSTDEV
  202   FORMAT(15X,I10,'   # START CYCLE STD. DEVS.')
  410   READ(21,2) READLINE
        IF(READLINE(12:28) .NE. '# START CYCLE % C') THEN
         CALL CONDENSE2(READLINE)
         GO TO 410
        ENDIF
        WRITE(22,203) NLCYCPRCFVR
  203   FORMAT(15X,I10,'   # START CYCLE % COEFF. OF VARS.')
  420   READ(21,2) READLINE
        IF(READLINE(12:28) .NE. '# START CYCLE GAM') THEN
         CALL CONDENSE2(READLINE)
         GO TO 420
        ENDIF
        WRITE(22,204) NLCYCGAM
  204   FORMAT(15X,I10,'   # START CYCLE GAMMA EST. VALUES')
  430   READ(21,2) READLINE
        IF(READLINE(12:39) .NE. '# START LAST CYCLE SUBJ. PAR') THEN
         CALL CONDENSE2(READLINE)
         GO TO 430
        ENDIF
        WRITE(22,206) NLLASTCYCPAREST
  206   FORMAT(15X,I10,'   # START LAST CYCLE SUBJ. PAR. ESTIMATES')
  450   READ(21,2) READLINE
        IF(READLINE(12:39) .NE. '# START LAST CYCLE SUBJ. STD') THEN
         CALL CONDENSE2(READLINE)
         GO TO 450
        ENDIF
        WRITE(22,208) NLLASTCYCSTD
  208   FORMAT(15X,I10,'   # START LAST CYCLE SUBJ. STD. DEVS.')
  440   READ(21,2) READLINE
        IF(READLINE(12:37) .NE. '# START LAST CYCLE SUBJ. %') THEN
         CALL CONDENSE2(READLINE)
         GO TO 440
        ENDIF
        WRITE(22,207) NLLASTCYCPER
  207   FORMAT(15X,I10,'   # START LAST CYCLE SUBJ. % COEFF. OF VARS.')
  480   READ(21,2) READLINE
        IF(READLINE(12:30) .NE. '# START PATIENT IDS') THEN
         CALL CONDENSE2(READLINE)
         GO TO 480
        ENDIF
        WRITE(22,212) NLPATID
  212   FORMAT(15X,I10,'   # START PATIENT IDS')
  490   READ(21,2) READLINE
        IF(READLINE(12:30) .NE. '# START PATIENT DOS') THEN
         CALL CONDENSE2(READLINE)
         GO TO 490
        ENDIF
        WRITE(22,213) NLPATDOS
  213   FORMAT(15X,I10,'   # START PATIENT DOSE COV. BLOCKS')
  530   READ(21,2) READLINE
        IF(READLINE(12:30) .NE. '# START PATIENT OUT') THEN
         CALL CONDENSE2(READLINE)
         GO TO 530
        ENDIF
        WRITE(22,214) NLPATOUTASSAY
  214   FORMAT(15X,I10,'   # START PATIENT OUTPUT AND ASSAY COEFF. BLOCK
     1S')
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
 4210 READ(25,3,ERR=4200) READLINE
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
        SUBROUTINE FILREDT(NOBSER,YO,C0,C1,C2,C3,MAXOBDIM,NDRUG,ND,
     1   NADD)
        IMPLICIT REAL*8(A-H,O-Z)
        PARAMETER(MAXNUMEQ=7)
        DIMENSION YO(MAXOBDIM,MAXNUMEQ),RJUNK(34),C0(MAXNUMEQ),
     1  C1(MAXNUMEQ),C2(MAXNUMEQ),C3(MAXNUMEQ)
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
         READ(27,*) XJUNK,(YO(I,J),J=1,NUMEQT)
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
        SUBROUTINE CONVERGE2(NCYCLE,ALOGLIK,XMEAN,XMED,GAMMA,STDEV,
     1   PRCFVR,SUBMEAN,SUBSTD,SUBPERCOF,NAME,CHARTNO,SEX,NDD,NI,
     2   AGE,HEIGHT,NUMEQT,ASSAYC,AIC,BIC,ICONVERGE)
        IMPLICIT REAL*8(A-H,O-Z)
        PARAMETER(MAXNUMEQ=7)
        CHARACTER READLINE*1000,NAME(800)*53,CHARTNO(800)*53,SEX(800)*1
        DIMENSION ALOGLIK(40999),XMEAN(40999,30),XMED(40999,30),
     1  GAMMA(40999,MAXNUMEQ),STDEV(40999,30),PRCFVR(40999,30),
     2  AGE(800),HEIGHT(800),
     3  SUBMEAN(800,30),SUBSTD(800,30),SUBPERCOF(800,30),NDD(800),
     4  ASSAYC(800,MAXNUMEQ,4)
        REWIND(26)
    2   FORMAT(A1000)
   50   READ(26,2) READLINE
     	  IF(READLINE(10:28) .NE. 'OF RANDOM VARIABLES') GO TO 50
        BACKSPACE(26)
        READ(26,53) NVAR
   53   FORMAT(T33,I2)
     	ICYC=1
	IPLACE=1
   10	READ(26,2,IOSTAT=IEND) READLINE
        IF(IEND .LT. 0) THEN
         WRITE(*,217)
  217    FORMAT(/' SOMETHING IS WRONG WITH THE OUTPUT FILE ENTERED;'/
     1' THERE ARE NO LAST CYCLE PARAMETER INFO RESULTS. THE PROGRAM'/
     2' STOPS.'//)
         CALL PAUSE
         STOP
        ENDIF
      IF(READLINE(14:33) .EQ. 'LAST CYCLE PARAMETER') THEN
  420  BACKSPACE(26)
       BACKSPACE(26)
       READ(26,2) READLINE
       IF(READLINE(2:11) .EQ. 'THE AKAIKE') THEN
        BACKSPACE(26)
        READ(26,416) AIC
  416   FORMAT(T37,G15.6)
        GO TO 410
       ENDIF
       GO TO 420
  410  READ(26,2) READLINE
       IF(READLINE(2:11) .NE. 'THE SCHWAR') GO TO 410
       BACKSPACE(26)
       READ(26,418) BIC
  418  FORMAT(T50,G15.6)
  255  READ(26,2) READLINE
       IF(READLINE(2:17) .NE. 'THIS RUN STOPPED') GO TO 255
       READ(26,2) READLINE
       IF(READLINE(2:17) .EQ. 'THE MAXIMUM NO. ') ICONVERGE = 0
       IF(READLINE(2:17) .EQ. 'THE CONVERGENCE ') ICONVERGE = 1
       IF(READLINE(2:17) .EQ. 'BOTH THE CONVERG') ICONVERGE = 2
       GO TO 100
      ENDIF
	GO TO (101,102,103,104,105,106) IPLACE
  101   IF(READLINE(2:17) .EQ. 'ITERATION NUMBER') THEN
	  IPLACE=2
	ENDIF
	GO TO 10
  102   IF(READLINE(6:20) .EQ. 'AVERAGE LOG-LIK') THEN
	  READ(26,*)
	  READ(26,56) ALOGLIK(ICYC)
   56	  FORMAT(T49,G15.6)
	  IPLACE=3
	ENDIF
	GO TO 10
  103   IF(READLINE(2:13) .EQ. 'GIVE THE COV') THEN
	  READ(26,*)
	  READ(26,*)
	  READ(26,*) (XMEAN(ICYC,I),I=1,NVAR)
	  READ(26,*) (XMED(ICYC,I),I=1,NVAR)
	  IPLACE=4
	ENDIF
	GO TO 10
  104   IF(READLINE(2:13) .EQ. 'THE STANDARD') THEN
	  READ(26,*)
	  READ(26,*)
	  READ(26,*) (STDEV(ICYC,I),I=1,NVAR)
	  IPLACE=5
	ENDIF
	GO TO 10
  105   IF(READLINE(22:35) .EQ. '% COEFFICIENTS') THEN
	  READ(26,*)
	  READ(26,*)
	  READ(26,*)
	  READ(26,*) (PRCFVR(ICYC,I),I=1,NVAR)
	  IPLACE=6
	ENDIF
	GO TO 10
  106   IF(READLINE(2:15) .EQ. 'A UNIQUE GAMMA') THEN
	  READ(26,*)
	  READ(26,*)
	   DO I=1,NUMEQT
	    READ(26,*) GAMMA(ICYC,I)
	   END DO
	  ICYC = ICYC + 1
        IPLACE=1
	ENDIF
	GO TO 10
  100   ICYC=ICYC-1
        NCYCLE = ICYC
        INDSUB = 0
  110	  READ(26,2,IOSTAT=IEND) READLINE
        IF(IEND .LT. 0) THEN
         WRITE(*,417)
  417    FORMAT(/' SOMETHING IS WRONG WITH THE OUTPUT FILE ENTERED;'/
     1' THE END OF THE FILE OCCURRED BEFORE ALL THE PATIENT DATA WAS'/
     2' READ IN. THE PROGRAM STOPS: '//)
         CALL PAUSE
         STOP
        ENDIF
      IF(READLINE(28:43) .EQ. 'THE PATIENT DATA') GO TO 200
        IF(READLINE(6:24) .EQ. 'PARAMETER ESTIMATES') THEN
         INDSUB = INDSUB + 1
         READ(26,*)
         READ(26,*)
         READ(26,*) (SUBMEAN(INDSUB,J),J=1,NVAR)
         GO TO 110
        ENDIF
        IF(READLINE(6:24) .EQ. 'CORRESPONDING STAND') THEN
         READ(26,*)
         READ(26,*)
         READ(26,*) (SUBSTD(INDSUB,J),J=1,NVAR)
         GO TO 110
        ENDIF
        IF(READLINE(6:24) .EQ. 'CORRESPONDING % COE') THEN
         READ(26,*)
         READ(26,*)
         READ(26,*) (SUBPERCOF(INDSUB,J),J=1,NVAR)
         GO TO 110
        ENDIF
        GO TO 110
  200   INDSUB = 0
  210	  READ(26,2,IOSTAT=IEND) READLINE
        IF(IEND .LT. 0) THEN
         WRITE(*,417)
         CALL PAUSE
         STOP
        ENDIF
        IF(READLINE(19:36) .EQ. 'END OF THE PATIENT') GO TO 400
        IF(READLINE(3:16) .EQ. 'LAST AND FIRST') THEN
         INDSUB = INDSUB + 1
         NAME(INDSUB) = READLINE(28:80)
         READ(26,2) READLINE
         CHARTNO(INDSUB) = READLINE(18:70)
          DO I = 1,5
           READ(26,*)
          END DO
         READ(26,*) AGE(INDSUB)
         READ(26,2) READLINE
         SEX(INDSUB) = READLINE(1:1)
         READ(26,*) HEIGHT(INDSUB)
         GO TO 210
        ENDIF
        IF(READLINE(12:23) .EQ. 'NO. OF DRUGS') THEN
         BACKSPACE(26)
    3    FORMAT(T2,I5)
         READ(26,3) NDRUG
         READ(26,3) NADD
         NI = 2*NDRUG + NADD
         READ(26,3) ND
         NDD(INDSUB) = ND
          GO TO 210
        ENDIF
        IF(READLINE(12:23) .EQ. 'NO. OF TOTAL') THEN
         BACKSPACE(26)
         READ(26,3) NUMEQT
         READ(26,3) M
         GO TO 210
        ENDIF
        IF(READLINE(1:25) .EQ. 'ASSAY COEFFICIENTS FOLLOW') THEN
         DO J = 1,NUMEQT
          READ(26,*) (ASSAYC(INDSUB,J,K),K=1,4)
         END DO
         GO TO 210
        ENDIF
        GO TO 210
  400   RETURN
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
	SUBROUTINE Old_SHIFT(TAU,ND,SIG,NDRUG,NADD,RS)
        use npag_utils, only : thesame
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION SIG(5000),RS(5000,34),TAU(7),XIV(7,5000,2),
     1  BOL(7,5000,2),COV(20,5000,2),INDIV(7),INDBOL(7),INDCOV(20),
     2  TIMCAN(34)
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
