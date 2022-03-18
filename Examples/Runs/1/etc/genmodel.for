C  TSTMULTN.FOR                          NOV, 2014


      SUBROUTINE DIFFEQ(NDIM,T,X,XP,RPAR,IPAR)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 Ka,Ke,V,Tlag1,KCP,KPC,WT,AFRICA,AGE,GENDER,HEIGHT
      COMMON /PARAMD/ P
      COMMON /INPUT/ R,B
      COMMON /DESCR/ AGE,HEIGHT,ISEX,IETHFLG
      COMMON /CNST2/ NPL,NUMEQT,NDRUG,NADD
      DIMENSION X(NDIM),XP(NDIM),P(32),R(37),B(20),CV(26),RATEIV(7)
!$omp Threadprivate(/PARAMD/,/INPUT/)  

      

         DO I = 1,NDRUG
           RATEIV(I) = R(2*I - 1)
         END DO

         DO I = 1, NADD
           CV(I) = R(2*NDRUG + I)
         END DO

         Ka=P(1)
         Ke=P(2)
         V=P(3)
         Tlag1=P(4)
         WT=CV(1)
         AFRICA=CV(2)
         AGE=CV(3)
         GENDER=CV(4)
         HEIGHT=CV(5)
         KCP=0
         KPC=0

         

      RETURN
      END



      SUBROUTINE OUTPUT(T,Y)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 Ka,Ke,V,Tlag1,KCP,KPC,WT,AFRICA,AGE,GENDER,HEIGHT
      COMMON /PARAMD/ P
      COMMON /STATE/ X
      COMMON /INPUT/ R,B
      COMMON /DESCR/ AGE,HEIGHT,ISEX,IETHFLG
      COMMON /CNST2/ NPL,NUMEQT,NDRUG,NADD
      PARAMETER(MAXNUMEQ=7)
      DIMENSION X(20),P(32),Y(MAXNUMEQ),R(37),B(20),CV(26)
!$omp Threadprivate(/PARAMD/,/INPUT/,/STATE/) 


         DO I = 1, NADD
           CV(I) = R(2*NDRUG + I)
         END DO

         Ka=P(1)
         Ke=P(2)
         V=P(3)
         Tlag1=P(4)
         WT=CV(1)
         AFRICA=CV(2)
         AGE=CV(3)
         GENDER=CV(4)
         HEIGHT=CV(5)
         KCP=0
         KPC=0

         Y(1)=X(2)/V

      RETURN
      END



      SUBROUTINE SYMBOL
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 Ka,Ke,V,Tlag1,KCP,KPC,WT,AFRICA,AGE,GENDER,HEIGHT
      CHARACTER PSYM(32)*11
      COMMON /CNST/ N,ND,NI,NUP,NUIC,NP
      COMMON/BOLUSCOMP/NBCOMP
      DIMENSION NBCOMP(7)


         DO I = 1,7
           NBCOMP(I) = I
         END DO

         
       N=-1
       NP=4
       PSYM(1)='Ka'
       PSYM(2)='Ke'
       PSYM(3)='V'
       PSYM(4)='Tlag1'

      RETURN
      END



      SUBROUTINE GETFA(FA)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 Ka,Ke,V,Tlag1,KCP,KPC,WT,AFRICA,AGE,GENDER,HEIGHT
      COMMON /PARAMD/ P
      COMMON /INPUT/ R,B
      COMMON /DESCR/ AGE,HEIGHT,ISEX,IETHFLG
      COMMON /CNST2/ NPL,NUMEQT,NDRUG,NADD
      COMMON /STATE/ X
      DIMENSION P(32),R(37),B(20),CV(26),FA(7),X(20)
!$omp Threadprivate(/PARAMD/,/INPUT/)  


         DO I = 1, NADD
           CV(I) = R(2*NDRUG + I)
         END DO

         DO I = 1,NDRUG
           FA(I) = 1.D0
         END DO

         Ka=P(1)
         Ke=P(2)
         V=P(3)
         Tlag1=P(4)
         WT=CV(1)
         AFRICA=CV(2)
         AGE=CV(3)
         GENDER=CV(4)
         HEIGHT=CV(5)
         KCP=0
         KPC=0

         

      RETURN
      END



      SUBROUTINE GETIX(N,X)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 Ka,Ke,V,Tlag1,KCP,KPC,WT,AFRICA,AGE,GENDER,HEIGHT
      COMMON /PARAMD/ P
      COMMON /INPUT/ R,B
      COMMON /DESCR/ AGE,HEIGHT,ISEX,IETHFLG
      COMMON /CNST2/ NPL,NUMEQT,NDRUG,NADD
      DIMENSION P(32),R(37),B(20),CV(26),X(20)
!$omp Threadprivate(/PARAMD/,/INPUT/)  


         DO I = 1, NADD
           CV(I) = R(2*NDRUG + I)
         END DO

         IF(N .GT. 0) THEN
           DO I = 1,N
             X(I) = 0.D0 
           END DO
         ENDIF

         IF(N .EQ. -1) THEN
           DO I = 1,3
             X(I) = 0.D0 
           END DO
         ENDIF

         Ka=P(1)
         Ke=P(2)
         V=P(3)
         Tlag1=P(4)
         WT=CV(1)
         AFRICA=CV(2)
         AGE=CV(3)
         GENDER=CV(4)
         HEIGHT=CV(5)
         KCP=0
         KPC=0

         

      RETURN
      END



      SUBROUTINE GETTLAG(TLAG)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 Ka,Ke,V,Tlag1,KCP,KPC,WT,AFRICA,AGE,GENDER,HEIGHT
      COMMON /PARAMD/ P
      COMMON /INPUT/ R,B
      COMMON /DESCR/ AGE,HEIGHT,ISEX,IETHFLG
      COMMON /CNST2/ NPL,NUMEQT,NDRUG,NADD
      COMMON /STATE/ X
      DIMENSION P(32),R(37),B(20),CV(26),TLAG(7),X(20)
!$omp Threadprivate(/PARAMD/,/INPUT/)  


         DO I = 1, NADD
           CV(I) = R(2*NDRUG + I)
         END DO

         DO I = 1,NDRUG
           TLAG(I) = 0.D0
         END DO

         Ka=P(1)
         Ke=P(2)
         V=P(3)
         Tlag1=P(4)
         WT=CV(1)
         AFRICA=CV(2)
         AGE=CV(3)
         GENDER=CV(4)
         HEIGHT=CV(5)
         KCP=0
         KPC=0

         TLAG(1) = Tlag1

      RETURN
      END



      SUBROUTINE ANAL3(X,TIN,TOUT)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 Ka,Ke,V,Tlag1,KCP,KPC,WT,AFRICA,AGE,GENDER,HEIGHT

      COMMON /PARAMD/ P
      COMMON /INPUT/ R,B
      COMMON /RATESV/ KE,KA,KCP,KPC,V
      COMMON /CNST2/ NPL,NUMEQT,NDRUG,NADD
      DIMENSION X(20),P(32),R(37),B(20),CV(26)
!$omp Threadprivate(/PARAMD/,/INPUT/,/RATESV/)   


         DO I = 1, NADD
           CV(I) = R(2*NDRUG + I)
         END DO

         Ka=P(1)
         Ke=P(2)
         V=P(3)
         Tlag1=P(4)
         WT=CV(1)
         AFRICA=CV(2)
         AGE=CV(3)
         GENDER=CV(4)
         HEIGHT=CV(5)
         KCP=0
         KPC=0

         T=TOUT-TIN
         IF(KCP.EQ.0.0D0.AND.KPC.EQ.0.0D0) THEN
           IF(KA.EQ.0.0D0) ICASE=1
           IF(KA.NE.0.0D0) ICASE=2
         ELSE
           IF(KA.EQ.0.0D0) ICASE=3
           IF(KA.NE.0.0D0) ICASE=4
         ENDIF
         GO TO (100,200,300,400), ICASE
C  CASE 1 SOLUTION - 1 COMP. NO 1ST ORDER INPUT
100     CALL CASE1(X(1),T)
         TIN=TOUT
         RETURN
C  CASE 2 SOLUTION - 1 COMP. + 1ST ORDER INPUT
200     CALL CASE2(X(1),X(2),T)
         TIN=TOUT
         RETURN
C  CASE 3 SOLUTION - 2 COMPARTMENT NO 1ST ORDER INPUT
300     CALL CASE3(X(1),X(2),T)
         TIN=TOUT
         RETURN
C CASE 4 SOLUTION - 2 COMP. + 1ST ORDER INPUT
400     CALL CASE4(X,T)
         TIN=TOUT
         RETURN
         END

         SUBROUTINE CASE1(X2,T)
         IMPLICIT REAL*8(A-H,O-Z)
         REAL*8 KE,KA,KCP,KPC
         DIMENSION R(37),B(20)
         COMMON /RATESV/ KE,KA,KCP,KPC,V
         COMMON /INPUT/ R,B
!$omp Threadprivate(/RATESV/,/INPUT/)  
         IF(KE.NE.0.0D0) GO TO 10
         X2=T*R(1)+X2
         RETURN
10      EKET=DEXP(-KE*T)
         X2=R(1)*(1.0D0-EKET)/KE+X2*EKET
         RETURN
         END

         SUBROUTINE CASE2(X1,X2,T)
         IMPLICIT REAL*8(A-H,O-Z)
         REAL*8 KE,KA,KCP,KPC
         COMMON /RATESV/ KE,KA,KCP,KPC,V
         COMMON /INPUT/ R,B
         DIMENSION R(37),B(20)
!$omp Threadprivate(/RATESV/,/INPUT/)  
         IF(KA.NE.KE) GO TO 30
         EKT=DEXP(-KE*T)
         X2=(X2-R(1)/KE)*EKT+R(1)/KE+KE*X1*T*EKT
         X1=X1*DEXP(-KA*T)
         RETURN
30      IF(KE.NE.0.0D0) GO TO 50
         EKAT=DEXP(-KA*T)
         X2=X2+T*R(1)+X1*(1.0D0-EKAT)
         X1=X1*EKAT
         RETURN
50      EKET=DEXP(-KE*T)
         EKAT=DEXP(-KA*T)
         X2=X2*EKET+R(1)*(1.0D0-EKET)/KE+
     X    KA*X1*(EKET-EKAT)/(KA-KE)
         X1=X1*EKAT
         RETURN
         END

         SUBROUTINE CASE3(X2,X3,T)
         IMPLICIT REAL*8(A-H,O-Z)
         REAL*8 KE,KA,KCP,KPC,L1,L2
         COMMON /RATESV/ KE,KA,KCP,KPC,V
         COMMON /INPUT/ R,B
         DIMENSION EA(2,2),R(37),B(20)
!$omp Threadprivate(/RATESV/,/INPUT/)  
         T1=KE+KCP+KPC
         T2=DSQRT(T1*T1-4.0D0*KE*KPC)
         L1=0.5D0*(T1+T2)
         L2=0.5D0*(T1-T2)
         IF(L2.NE.0.0D0) GO TO 200
         EL1T=DEXP(-L1*T)
         OEL1T=1.0D0-EL1T
         EA(1,1)=(L1-KPC)*EL1T+KPC
         EA(1,2)=KPC*OEL1T
         EA(2,1)=KCP*OEL1T
         EA(2,2)=(L1-KE-KCP)*EL1T+KE+KCP
         P1=R(1)*((1.0D0-KPC/L1)*OEL1T+KPC*T)
         P2=R(1)*((-KCP/L1)*OEL1T+KCP*T)
         C1=EA(1,1)*X2+EA(1,2)*X3+P1
         C2=EA(2,1)*X2+EA(2,2)*X3+P2
         X2=C1/L1
         X3=C2/L1
         RETURN
200     CONTINUE
         EL1T=DEXP(-L1*T) 
         EL2T=DEXP(-L2*T)
         OEL1T=1.0D0-EL1T
         OEL2T=1.0D0-EL2T
         DEL2L1=EL2T-EL1T
         EA(1,1)=(L1-KPC)*EL1T+(KPC-L2)*EL2T
         EA(1,2)=KPC*DEL2L1
         EA(2,1)=KCP*DEL2L1
         EA(2,2)=(L1-KE-KCP)*EL1T+(KE+KCP-L2)*EL2T
         P1=R(1)*((1.0D0-KPC/L1)*OEL1T+(KPC/L2-1.0D0)*OEL2T)
         P2=R(1)*((-KCP/L1)*OEL1T+(KCP/L2)*OEL2T)
         D=L1-L2
         C1=EA(1,1)*X2+EA(1,2)*X3+P1
         C2=EA(2,1)*X2+EA(2,2)*X3+P2
         X2=C1/D
         X3=C2/D
         RETURN
         END

         SUBROUTINE CASE4(X,T)
         IMPLICIT REAL*8(A-H,O-Z)
         REAL*8 KE,KA,KCP,KPC,L1,L2
         COMMON /RATESV/ KE,KA,KCP,KPC,V
         COMMON /INPUT/ R,B
         DIMENSION EA(2,2),R(37),B(20),X(20)
!$omp Threadprivate(/RATESV/,/INPUT/)  
         T1=KE+KCP+KPC
         T2=DSQRT(T1*T1-4.0D0*KE*KPC)
         L1=0.5D0*(T1+T2)
         L2=0.5D0*(T1-T2)
         IF(L2.NE.0.0D0) GO TO 200
         EL1T=DEXP(-L1*T)
         OEL1T=1.0D0-EL1T
         EA(1,1)=(L1-KPC)*EL1T+KPC
         EA(1,2)=KPC*OEL1T
         EA(2,1)=KCP*OEL1T
         EA(2,2)=(L1-KE-KCP)*EL1T+KE+KCP
         P1A=R(1)*((1.0D0-KPC/L1)*OEL1T+KPC*T)
         P2A=R(1)*((-KCP/L1)*OEL1T+KCP*T)
         EKAT=DEXP(-KA*T)
         RL1=(EL1T-EKAT)/(KA-L1)
         RKA=(1.0D0-EKAT)/KA
         P1B=KA*X(1)*((L1-KPC)*RL1+KPC*RKA)
         P2B=KA*X(1)*(-KCP*RL1+KCP*RKA)
         C1=EA(1,1)*X(2)+EA(1,2)*X(3)+P1A+P1B
         C2=EA(2,1)*X(2)+EA(2,2)*X(3)+P2A+P2B
         X(1)=X(1)*EKAT
         X(2)=C1/L1
         X(3)=C2/L1
         RETURN
200     CONTINUE
         EL1T=DEXP(-L1*T)
         EL2T=DEXP(-L2*T)
         EKAT=DEXP(-KA*T)
         OEL1T=1.0D0-EL1T
         OEL2T=1.0D0-EL2T
         DEL2L1=EL2T-EL1T
         EA(1,1)=(L1-KPC)*EL1T+(KPC-L2)*EL2T
         EA(1,2)=KPC*DEL2L1
         EA(2,1)=KCP*DEL2L1
         EA(2,2)=(L1-KE-KCP)*EL1T+(KE+KCP-L2)*EL2T
         P1A=R(1)*((1.0D0-KPC/L1)*OEL1T+(KPC/L2-1.0D0)*OEL2T)
         P2A=R(1)*((-KCP/L1)*OEL1T+(KCP/L2)*OEL2T)
         IF(KA.NE.L1) GO TO 240
         RL=DEL2L1/(L1-L2)
         P1B=L1*X(1)*(T*(L1-KPC)*EL1T+(KPC-L2)*RL)
         P2B=L1*X(1)*(-T*KCP*EL1T+KCP*RL)
         GO TO 300
240     IF(KA.NE.L2) GO TO 280
         RL=DEL2L1/(L1-L2)
         P1B=L2*X(1)*((L1-KPC)*RL+T*(KPC-L2)*EL2T)
         P2B=L2*X(1)*(-KCP*RL+T*KCP*EL2T)
         GO TO 300 
280     RL1KA=(EL1T-EKAT)/(KA-L1)
         RL2KA=(EL2T-EKAT)/(KA-L2)
         P1B=KA*X(1)*((L1-KPC)*RL1KA+(KPC-L2)*RL2KA)
         P2B=KA*X(1)*(-KCP*RL1KA+KCP*RL2KA)
300     D=L1-L2
         C1=EA(1,1)*X(2)+EA(1,2)*X(3)+P1A+P1B
         C2=EA(2,1)*X(2)+EA(2,2)*X(3)+P2A+P2B
         X(1)=X(1)*EKAT
         X(2)=C1/D
         X(3)=C2/D
         RETURN 
         END


       
