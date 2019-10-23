C  TSTMULTI.FOR  [TSTMULTI-TEMPLATE]                         APR, 2011     
                          
C-----------------------------------------------------------------------    
                      
                      
C-Inputs----------------------------------------------------------------
C 
C   Example drug
C-END-------------------------------------------------------------------


C-Covariates----------------------------------------------------------------
C 
C   No covariates included in model.
C-END-------------------------------------------------------------------


C-Notes----------------------------------------------------------------

C-END-------------------------------------------------------------------
C-END-------------------------------------------------------------------                          
                          
                          
      SUBROUTINE DIFFEQ(NDIM,T,X,XP,RPAR,IPAR)     
      IMPLICIT REAL*8(A-H,O-Z)                     
      COMMON /PARAMD/ P   
      COMMON /INPUT/ R,B  
      COMMON /DESCR/ AGE,HEIGHT,ISEX,IETHFLG       
      COMMON /CNST2/ NPL,NUMEQT,NDRUG,NADD         
      DIMENSION X(NDIM),XP(NDIM),P(32),R(37),B(20),CV(26),RATEIV(7)         
                                                
                          
	DO I = 1,NDRUG           
	 RATEIV(I) = R(2*I - 1)  
	END DO                   
                          
	DO I = 1, NADD       
	 CV(I) = R(2*NDRUG + I)  
	END DO                            
                          
                          
C*********** USER DEFINED DIFFERENTIAL EQUATIONS BELOW: ******************    
                          
C-DiffEQ--------------------------------------------------------------

C-END-----------------------------------------------------------------

C*********** USER DEFINED DIFFERENTIAL EQUATIONS ABOVE: ******************    

                          
      RETURN              
      END
      
                          
C-----------------------------------------------------------------------    
                          
      SUBROUTINE OUTPUT(T,Y)                       
      IMPLICIT REAL*8(A-H,O-Z)                     
      DOUBLE PRECISION KE,KA,KCP,KPC               
      COMMON /PARAMD/ P   
      COMMON /STATE/ X    
      COMMON /INPUT/ R,B  
      COMMON /DESCR/ AGE,HEIGHT,ISEX,IETHFLG       
      COMMON /CNST2/ NPL,NUMEQT,NDRUG,NADD         
      COMMON /RATESV/ KE,KA,KCP,KPC,V              
      DIMENSION X(20),P(32),Y(6),R(37),B(20),CV(26)
                                                                             
                          
	DO I = 1, NADD       
	 CV(I) = R(2*NDRUG + I)  
	END DO                   
                          

                          
C*********** USER DEFINED OUTPUT EQUATIONS BELOW: ******************    
C-Outputs-------------------------------------------------------------
        Y(1)=X(2)/V
C-END-----------------------------------------------------------------
        
C*********** USER DEFINED OUTPUT EQUATIONS ABOVE: ******************    

                          
       RETURN             
       END                
                          
C-----------------------------------------------------------------------    
                          
      SUBROUTINE SYMBOL   
      IMPLICIT REAL*8(A-H,O-Z)                     
      CHARACTER PSYM(32)*11                        
      COMMON /CNST/ N,ND,NI,NUP,NUIC,NP            
      COMMON/BOLUSCOMP/NBCOMP                      
      DIMENSION NBCOMP(7) 
                                                   
          
                          
	DO I = 1,7               
	 NBCOMP(I) = I           
	END DO
        
                          
C*********** USER DEFINED BOLUS EQUATIONS BELOW: ******************   
C-Bolus--------------------------------------------------------------

C-END----------------------------------------------------------------
        
C*********** USER DEFINED BOLUS EQUATIONS ABOVE: ******************    

                        
                          
                          
C************************* SET N BELOW: ********************************    
                                  
C-NumberOfCompartments--------------------------------------------------                          
       N=-1
C-END-------------------------------------------------------------------
                          
C************************* SET N ABOVE: ********************************    
                          
                          
                          
C**** SET THE NUMBER OF AND THE NAMES OF THE PARAMETERS BELOW *****               
C-Parameters-----------------------------------------------------------
       NP=4
       PSYM(1)='Ka'
       PSYM(2)='Kel'
       PSYM(3)='Vol'
       PSYM(4)='Tlag'
C-END------------------------------------------------------------------
                          
C**** SET THE NUMBER OF AND THE NAMES OF THE PARAMETERS ABOVE *****               
                         
                          
       RETURN             
       END                

C-----------------------------------------------------------------------    

        SUBROUTINE GETFA(FA)                       
        IMPLICIT REAL*8(A-H,O-Z)                   
        COMMON /PARAMD/ P 
        COMMON /INPUT/ R,B
        COMMON /DESCR/ AGE,HEIGHT,ISEX,IETHFLG     
        COMMON /CNST2/ NPL,NUMEQT,NDRUG,NADD       
        DIMENSION P(32),R(37),B(20),CV(26),FA(7)   
                          
                          
	DO I = 1, NADD       
	 CV(I) = R(2*NDRUG + I)  
	END DO                   
                                 
                          
	DO I = 1,NDRUG           
	 FA(I) = 1.D0            
	END DO                   
                          
                          
C********************** USER DEFINED FA BELOW ************************    
                                                    
C-Bioavailability------------------------------------------------------                          

C-END------------------------------------------------------------------
       
                          
C********************** USER DEFINED FA ABOVE ************************                        
                          
	RETURN                   
	END                      
	                         
C-----------------------------------------------------------------------    
                      
        SUBROUTINE GETIX(N,X)                      
        IMPLICIT REAL*8(A-H,O-Z)                   
        COMMON /PARAMD/ P 
        COMMON /INPUT/ R,B
        COMMON /DESCR/ AGE,HEIGHT,ISEX,IETHFLG     
        COMMON /CNST2/ NPL,NUMEQT,NDRUG,NADD       
        DIMENSION P(32),R(37),B(20),CV(26),X(20)   
                                                    
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

                                                  
C********************** USER DEFINED IC BELOW ************************    
                                                          
C-InitialConditions----------------------------------------------------                          

C-END------------------------------------------------------------------                                                                                                     
                          
C********************** USER DEFINED IC ABOVE ************************    
                           
	RETURN                   
	END                      

C-----------------------------------------------------------------------    
                         
        SUBROUTINE GETTLAG(TLAG)                   
        IMPLICIT REAL*8(A-H,O-Z)                   
        COMMON /PARAMD/ P 
        COMMON /INPUT/ R,B
        COMMON /DESCR/ AGE,HEIGHT,ISEX,IETHFLG     
        COMMON /CNST2/ NPL,NUMEQT,NDRUG,NADD       
        DIMENSION P(32),R(37),B(20),CV(26),TLAG(7) 
                          
                                                    
	DO I = 1, NADD       
	 CV(I) = R(2*NDRUG + I)  
	END DO                   
                          
	DO I = 1,NDRUG           
	 TLAG(I) = 0.D0          
	END DO                   
                          
                          
C********************** USER DEFINED TLAG BELOW ************************    
                                                                         
C-Tlag-----------------------------------------------------------------                          
      TLAG(1) = P(4)
C-END------------------------------------------------------------------

                          
C********************** USER DEFINED TLAG ABOVE ************************    
                      
                          
	RETURN                   
	END                      
                          
C-----------------------------------------------------------------------    
                        
        SUBROUTINE ANAL3(X,TIN,TOUT)               
                       
        IMPLICIT REAL*8(A-H,O-Z)                   
        DOUBLE PRECISION KE,KA,KCP,KPC             
        DIMENSION X(20),P(32),R(37),B(20),CV(26) 
        COMMON /CNST2/ NPL,NUMEQT,NDRUG,NADD                  
        COMMON /RATESV/ KE,KA,KCP,KPC,V            
        COMMON /INPUT/ R,B
        COMMON /PARAMD/ P   

                        
	DO I = 1, NADD       
	 CV(I) = R(2*NDRUG + I)  
	END DO                   
                          
C********************** USER DEFINED CODE BELOW ************************    
             
                          
C  THE FOLLOWING IS AN EXAMPLE:                    
                          
C  KE = DEXP(P(1))        
C  KA = DEXP(P(2))        
C  KCP = DEXP(P(3))       
C  KPC = DEXP(P(4))       
C  V = DEXP(P(5))

C-AnalyticEQ ----------------------------------------------------------
      KE = P(2)
      KA = P(1)
      KCP = 0
      KPC = 0
      V = P(3)
C-END------------------------------------------------------------------
                          
                           
C********************** USER DEFINED CODE ABOVE ************************    
                          
                          
        T=TOUT-TIN        
C                         
C  SELECT WHICH CASE THE MODEL CONFORMS TO.        
C                         
        IF(KCP.EQ.0.0D0.AND.KPC.EQ.0.0D0) THEN     
          IF(KA.EQ.0.0D0) ICASE=1                  
          IF(KA.NE.0.0D0) ICASE=2	                 
                          
	ELSE                     
                          
	  IF(KA.EQ.0.0D0) ICASE=3
          IF(KA.NE.0.0D0) ICASE=4                  
                          
	ENDIF                    
                          
        GO TO (100,200,300,400), ICASE             
C                         
C  CASE 1 SOLUTION - 1 COMP. NO 1ST ORDER INPUT    
C                         
100     CALL CASE1(X(2),T)
	TIN=TOUT                 
        RETURN            
C                         
C  CASE 2 SOLUTION - 1 COMP. + 1ST ORDER INPUT.    
C                         
200     CALL CASE2(X(1),X(2),T)                    
        TIN=TOUT          
        RETURN            
C                         
C  CASE 3 SOLUTION - 2 COMPARTMENT NO 1ST ORDER INPUT.                      
C                         
300     CALL CASE3(X(2),X(3),T)                    
        TIN=TOUT          
        RETURN            
C                         
C CASE 4 SOLUTION - 2 COMP. + 1ST ORDER INPUT.     
C                         
C400    CONTINUE          
400     CALL CASE4(X,T)   
        TIN=TOUT          
        RETURN            
        END               
C                         
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC              
C                         
        SUBROUTINE CASE1(X2,T)                     
C                         
C  SOLUTION FOR 1 COMP. MODEL WITH NO 1ST ORDER INPUT.                      
C                         
        IMPLICIT REAL*8(A-H,O-Z)                   
        DOUBLE PRECISION KE,KA,KCP,KPC             
                          
        DIMENSION R(37),B(20)                      
        COMMON /RATESV/ KE,KA,KCP,KPC,V            
        COMMON /INPUT/ R,B
                          
C                         
C*****SUBCASE 1A - KE=0.0*****                     
C                         
        IF(KE.NE.0.0D0) GO TO 10                   
        X2=T*R(1)+X2      
        RETURN            
C                         
C*****COMPLETE SOLUTION*****                       
C                         
10      EKET=DEXP(-KE*T)  
        X2=R(1)*(1.0D0-EKET)/KE+X2*EKET            
C                         
        RETURN            
        END               
C                         
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC              
C                         
        SUBROUTINE CASE2(X1,X2,T)                  
C                         
C  SOLUTION FOR 1 COMP. MODEL WITH 1ST ORDER INPUT.
        IMPLICIT REAL*8(A-H,O-Z)                   
        DOUBLE PRECISION KE,KA,KCP,KPC             
        COMMON /RATESV/ KE,KA,KCP,KPC,V            
        COMMON /INPUT/ R,B
        DIMENSION R(37),B(20)                      
                          
C                         
C*****SUBCASE 2A - KA=KE*****                      
C                         
        IF(KA.NE.KE) GO TO 30                      
        EKT=DEXP(-KE*T)   
        X2=(X2-R(1)/KE)*EKT+R(1)/KE+KE*X1*T*EKT    
        X1=X1*DEXP(-KA*T) 
        RETURN            
C                         
C*****SUBCASE 2B - KE=0.0*****                     
C                         
30      IF(KE.NE.0.0D0) GO TO 50                   
        EKAT=DEXP(-KA*T)  
        X2=X2+T*R(1)+X1*(1.0D0-EKAT)               
        X1=X1*EKAT        
        RETURN            
C                         
C*****COMPLETE SOLUTION*****                       
C                         
50      EKET=DEXP(-KE*T)  
        EKAT=DEXP(-KA*T)  
        X2=X2*EKET+R(1)*(1.0D0-EKET)/KE+           
     X    KA*X1*(EKET-EKAT)/(KA-KE)                
        X1=X1*EKAT        
C                         
        RETURN            
        END               
C                         
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC              
C                         
        SUBROUTINE CASE3(X2,X3,T)                  
C                         
C  SOLUTION FOR 2 COMP. MODEL WITH NO 1ST ORDER INPUT.                      
C                         
        IMPLICIT REAL*8(A-H,O-Z)                   
        DOUBLE PRECISION KE,KA,KCP,KPC,L1,L2       
        COMMON /RATESV/ KE,KA,KCP,KPC,V            
        COMMON /INPUT/ R,B
        DIMENSION EA(2,2),R(37),B(20)              
                          
C                         
C  CALCULATE EIGEN VALUES L1 AND L2.               
C                         
        T1=KE+KCP+KPC     
        T2=DSQRT(T1*T1-4.0D0*KE*KPC)               
        L1=0.5D0*(T1+T2)  
        L2=0.5D0*(T1-T2)  
C                         
C*****SUBCASE 3A - L2=0.0*****                     
C                         
        IF(L2.NE.0.0D0) GO TO 200                  
        EL1T=DEXP(-L1*T)  
C                         
C  FORM EXP(AT)*L1        
        OEL1T=1.0D0-EL1T  
        EA(1,1)=(L1-KPC)*EL1T+KPC                  
        EA(1,2)=KPC*OEL1T 
        EA(2,1)=KCP*OEL1T 
        EA(2,2)=(L1-KE-KCP)*EL1T+KE+KCP            
C                         
C  FORM THE PARTICULAR SOLUTION*L1                 
        P1=R(1)*((1.0D0-KPC/L1)*OEL1T+KPC*T)       
        P2=R(1)*((-KCP/L1)*OEL1T+KCP*T)            
C                         
C  FORM COMPLETE SOLUTION 
        C1=EA(1,1)*X2+EA(1,2)*X3+P1                
        C2=EA(2,1)*X2+EA(2,2)*X3+P2                
        X2=C1/L1          
        X3=C2/L1          
        RETURN            
C                         
C*****COMPLETE SOLUTION*****                       
C                         
200     CONTINUE          
        EL1T=DEXP(-L1*T)  
        EL2T=DEXP(-L2*T)  
        OEL1T=1.0D0-EL1T  
        OEL2T=1.0D0-EL2T  
        DEL2L1=EL2T-EL1T  
C                         
C  FORM EXP(AT)*(L1-L2)   
        EA(1,1)=(L1-KPC)*EL1T+(KPC-L2)*EL2T        
        EA(1,2)=KPC*DEL2L1
        EA(2,1)=KCP*DEL2L1
        EA(2,2)=(L1-KE-KCP)*EL1T+(KE+KCP-L2)*EL2T  
C                         
C  FORM THE PARTICULAR SOLUTION*(L1-L2).           
        P1=R(1)*((1.0D0-KPC/L1)*OEL1T+(KPC/L2-1.0D0)*OEL2T)                 
        P2=R(1)*((-KCP/L1)*OEL1T+(KCP/L2)*OEL2T)   
C                         
C  FORM THE COMPLETE SOLUTION                      
        D=L1-L2           
        C1=EA(1,1)*X2+EA(1,2)*X3+P1                
        C2=EA(2,1)*X2+EA(2,2)*X3+P2                
        X2=C1/D           
        X3=C2/D           
        RETURN            
        END               
C                         
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        SUBROUTINE CASE4(X,T)                      
C                         
C  SOLUTION FOR 2 COMP. MODEL WITH 1ST ORDER INPUT.
C                         
        IMPLICIT REAL*8(A-H,O-Z)                   
        DOUBLE PRECISION KE,KA,KCP,KPC,L1,L2       
        COMMON /RATESV/ KE,KA,KCP,KPC,V            
        COMMON /INPUT/ R,B
        DIMENSION EA(2,2),R(37),B(20),X(20)        
                          
                          
C  CALCULATE EIGEN VALUES L1 AND L2.               
                          
        T1=KE+KCP+KPC     
        T2=DSQRT(T1*T1-4.0D0*KE*KPC)               
        L1=0.5D0*(T1+T2)  
        L2=0.5D0*(T1-T2)  
C                         
C*****SUBCASE 4A - L2=0.0*****                     
C                         
        IF(L2.NE.0.0D0) GO TO 200                  
        EL1T=DEXP(-L1*T)  
C                         
C  FORM EXP(AT)*L1        
        OEL1T=1.0D0-EL1T  
        EA(1,1)=(L1-KPC)*EL1T+KPC                  
        EA(1,2)=KPC*OEL1T 
        EA(2,1)=KCP*OEL1T 
        EA(2,2)=(L1-KE-KCP)*EL1T+KE+KCP            
C                         
C  FORM THE PARTICULAR SOLUTION*L1 FROM IV         
        P1A=R(1)*((1.0D0-KPC/L1)*OEL1T+KPC*T)      
        P2A=R(1)*((-KCP/L1)*OEL1T+KCP*T)           
C                         
C  FORM THE PARTICULAR SOLUTION*L1 FROM 1ST ORDER ABS. INPUT.               
        EKAT=DEXP(-KA*T)  
        RL1=(EL1T-EKAT)/(KA-L1)                    
        RKA=(1.0D0-EKAT)/KA                        
        P1B=KA*X(1)*((L1-KPC)*RL1+KPC*RKA)         
        P2B=KA*X(1)*(-KCP*RL1+KCP*RKA)             
C                         
C  FORM COMPLETE SOLUTION 
        C1=EA(1,1)*X(2)+EA(1,2)*X(3)+P1A+P1B       
        C2=EA(2,1)*X(2)+EA(2,2)*X(3)+P2A+P2B       
        X(1)=X(1)*EKAT    
        X(2)=C1/L1        
        X(3)=C2/L1        
        RETURN            
C                         
C*****SOLUTIONS FOR THE FOLLOWING CASES:*****      
C           1. SUB CASE 4B - KA=L1                 
C           2. SUB CASE 4C - KA=L2                   
C           3. COMPLETE SOLUTION.                    
200     CONTINUE          
C                         
C---FORM PART OF SOLUTION COMMON TO ALL 3 CASES.---
        EL1T=DEXP(-L1*T)  
        EL2T=DEXP(-L2*T)  
        EKAT=DEXP(-KA*T)  
        OEL1T=1.0D0-EL1T  
        OEL2T=1.0D0-EL2T  
        DEL2L1=EL2T-EL1T  
C                         
C  FORM EXP(AT)*(L1-L2)   
        EA(1,1)=(L1-KPC)*EL1T+(KPC-L2)*EL2T        
        EA(1,2)=KPC*DEL2L1
        EA(2,1)=KCP*DEL2L1
        EA(2,2)=(L1-KE-KCP)*EL1T+(KE+KCP-L2)*EL2T  
C                         
C  FORM THE PARTICULAR SOLUTION*(L1-L2) FROM THE INFUSION(A).               
        P1A=R(1)*((1.0D0-KPC/L1)*OEL1T+(KPC/L2-1.0D0)*OEL2T)                
        P2A=R(1)*((-KCP/L1)*OEL1T+(KCP/L2)*OEL2T)  
C                         
C---FORM THE PARTICULAR SOLUTION*(L1-L2) FROM 1ST ORDER INPUT,              
C   FOR THE APPROPRIATE  CASE(B).---        
C  SUBCASE 4B             
        IF(KA.NE.L1) GO TO 240                     
        RL=DEL2L1/(L1-L2) 
        P1B=L1*X(1)*(T*(L1-KPC)*EL1T+(KPC-L2)*RL)  
        P2B=L1*X(1)*(-T*KCP*EL1T+KCP*RL)           
        GO TO 300         
C                         
C  SUBCASE 4C             
240     IF(KA.NE.L2) GO TO 280                     
        RL=DEL2L1/(L1-L2) 
        P1B=L2*X(1)*((L1-KPC)*RL+T*(KPC-L2)*EL2T)  
        P2B=L2*X(1)*(-KCP*RL+T*KCP*EL2T)           
        GO TO 300         
C                         
C  COMPLETE SOL SUBCASE   
280     RL1KA=(EL1T-EKAT)/(KA-L1)                  
        RL2KA=(EL2T-EKAT)/(KA-L2)                  
        P1B=KA*X(1)*((L1-KPC)*RL1KA+(KPC-L2)*RL2KA)
        P2B=KA*X(1)*(-KCP*RL1KA+KCP*RL2KA)         
C                         
C---FORM THE COMPOSITE SOLUTION---                 
300     D=L1-L2           
        C1=EA(1,1)*X(2)+EA(1,2)*X(3)+P1A+P1B       
        C2=EA(2,1)*X(2)+EA(2,2)*X(3)+P2A+P2B       
        X(1)=X(1)*EKAT    
        X(2)=C1/D         
        X(3)=C2/D         
        RETURN            
        END               

