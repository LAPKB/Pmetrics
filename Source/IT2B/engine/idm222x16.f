c  idm222x16.f                                             3/20/16

c  idm222x16 has the following changes from idm222x15:

c  NRANFIX is added to the argument list, and IRAN(.) is now allowed to
c  be 2, which indicates a RANFIX parameter. The code is adjusted 
c  because the partial derivatives, which are of course only calculated
c  for the random parameters, will therefore now be calculated only for 
c  those parameter with IRAN(.) values which are neither 0 or 2.

c-----------------------------------------------------------------------

c  idm222x15.f                                             3/26/15

c  idm222x15 has the following change from idm222x14:

c  All numbers written out in F or G format are now tested to see if
c  they are inside [-1.D-99, 1.D-99]. If so, they are changed to be
c  0. The reason is that otherwise they will be printed out without 
c  the accompanying D or E (e.g., as .934-106, rather than .934E-106).

c  Note that this module is linked with npageng30.f initially.

c-----------------------------------------------------------------------

c  idm222x14.f                                             7/21/14

c  idm222x14 has the following change to idm222x13:

c  If the program stops unexpectedly with the writing of format 111
c  in Subroutine FUNC1, this same comment will now be written to
c  the file, ERRFIL, which is passed to FUNC in COMMON/ERR.

c-----------------------------------------------------------------------

c  idm222x13.f                                             4/8/14

c  idm222x13 has the following changes to idm222x12:

c  1. In the main module and FUNC1, a new PARAMETER(MAXNUMEQ=7) stmt.
c  allows all dimensions of 6 (related to the max. no. of output eqs.
c  to be changed to MAXNUMEQ).

c  2. In Subroutine FUNC1, the dimensions of 6 in XSTORE and XPRED have
c  been changed to 20, as they should have been all along (i.e., this
c  represents the maximum no. of compartments allowed).

c-----------------------------------------------------------------------

c  idm222x12.f                                             10/26/12

c  idm222x12 has the following bug corrections to idm222x11:

C  1.  IN SUBROUTINE FUNC1, BEFORE
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

c  2. THE R(.) ARE SET = RS(.,.) BEFORE GETIX IS CALLED IN THE TIME 
C  RESET SECTION OF SUBROUTINE FUNC. NOT DOING THIS WOULD MEAN THAT IF
C  THE INITIAL CONDITIONS FOR THE X(.) ARE FUNCTIONS OF THE COVARIATES
C  (ESTABLISHED IN GETIX FROM THE R(.) VALUES), THEY WOULD BE ASSIGNED
C  VALUES BASED ON COVARIATES FROM A PREVIOUS DOSAGE LINE IN THE
C  PATIENT'S DATA FILE, RATHER THAN THE LINE WHICH IS THE DOSE RESET
C  LINE.

c-----------------------------------------------------------------------

c  idm222x11.f                                               8/28/12

c  idm222x11 has the following change to idm222x10:

c  In SUBROUTINE FUNC1, the code to save ND0, SIGO, RSO, is moved to
c  before the IF(N .EQ. 0) GO TO 75  statement. The reason is that 
c  before this  routine returns, ND, SIG, and RS are reset back to these
c  values, even if N = 0, and so they must be established at this time.

c-----------------------------------------------------------------------

c  idm222x10.f                                             4/18/12

c  idm222x10 has the following changes to idm222x9.f:

c  It is to be used with it2beng19.f, which allows steady state doses
c  to be boluses as well as IVs. As a result, an additional parameter,
c  ISKIPBOL, is used so, in Subroutine FUNC1, when convergence occurs in
c  a steady state dose set, the last bolus from that set will not be
c  reapplied below label 83.

c-----------------------------------------------------------------------

c  idm222x9.f                                               3/4/12

c  idm222x9 has the following bug fix to idm222x9.f. In Subroutine 
c  FUNC1, the code to save ND, SIG, and RS before altering them if 
c  there are time lag parameters (in the call to GETTLAG) is now 
c  executed whether or not there are time lag parameters. The reason is
c  that, with steady state doses, the first SIG(.) time in a steady 
c  state dose set is reset to be 0 after the steady state dose is
c  identified. And this time must be reset back to be its original
c  negative value at the end of the routine so that the next time the
c  routine is called, the program will again know when a steady state
c  dose is coming. 

c-----------------------------------------------------------------------

c  idm222x8.f                                              1/29/12

c  idm222x8.f has the same changes from idm222x6.f that idm11x8.f has
c  from idm11x6.f, namely:

c  1. A bug is corrected in Subroutine FUNC1 - now time resets are
c  identified by just the observation time = 0 (i.e., the dose time = 0
c  is no longer required). This is because it is possible for a dose
c  time (especially if there are timelags) to be after the last
c  observation time in a section of the patient file (before a time
c  reset), and if this happens, the program will not be able to
c  identify the observation time of 0 as a time reset.

c  2. It can accommodate steady state dose regimens as created by 
c  new subroutine NEWWORK1.FOR in it2beng17.f. And it uses new
c  Subroutine PREDLAST3 in idm11x8.f (both of these 2 new subroutines
c  are based on stand-a-lone versions of the same name) which is called
c  by Subroutine FUNC1 to predict the final (steady state) compartment
c  amounts. If these predicted values are determined to have
c  converged, the rest of the steady state dose set will be skipped
c  to save time. Note that predictions start after the end of the
c  5th dose set (out of 100 in each steady state regimen), and
c  continue until convergence is reached, or the entire steady state
c  dose set has been integrated through.

C  SO THE MAIN CHANGES TO THE CODE ARE:

C  CHECK TO SEE IF A DOSE TIME IS NEGATIVE. IF NOT, PROCEED AS USUAL. IF 
C  SO, PROCEED AS IF THAT TIME WAS 0, BUT AFTER THE END OF THAT DOSE AND 
C  THE NEXT 4, CALL SUBROUTINE PREDLAST3 TO PREDICT THE STEADY STATE
C  COMPARTMENT AMOUNTS AFTER THE 100 DOSES (NOTE THAT THE COMPARTMENT
C  AMOUNTS WILL HAVE TO BE FOUND AT THE END OF EACH OF THE STEADY STATE
C  DOSES OF COURSE AS THE LOGIC OF PREDLAST3 REQUIRES). IF CONVERGENCE
C  IS ACHIEVED, ASSIGN THE COMPARTMENT AMOUNTS TO BE THE PREDICTED
C  AMOUNTS AND SET KNS TO BE WHAT IT IS WHEN THESE STEADY STATE DOSE 
C  SETS HAVE FINISHED. ALSO, SET T = END OF THE STEADY STATE DOSE SET
C  SINCE THAT'S WHAT IT WOULD HAVE BEEN HAD ALL THE DOSES BEEN 
C  INTEGRATED THROUGH.

c  3. All arrays related to doses (SIG,SIGO,RS,RSO, and BS) in
c  Subroutine FUNC have their 500's changed to 5000's. This is because
c  each set of 100 steady state doses, with each of up to 7 drugs having
c  its own stopping time, could require an extra 100 x 8 dose events, 
c  and there could be multiple steady state sets (they can occur at the
c  start of the dose regimen, or at any time reset point).

c  4. Near the top of Subroutine FUNC, R(1)=0.0D0 is replaced by setting
c  R(2*I-1) = 0.D0, for I = 1,NDRUG. This should have been done when
c  the program became a multi-drug program (see comment in FUNC).
c  5. A time reset no longer requires all initial compartment amounts
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

c  idm222x6.f                                              12/30/10

c  idm222x6 has the following change to idm222x5:

c  In Subroutine FUNC1, it has code that calls Subroutine ANAL3, rather
c  than USERANAL if N .EQ. -1. Also, the code to reset X(I),I=1,N to 0
c  where there is a time reset now includes extra code to set 
c  X(I),I=1,3 to 0 if N .EQ. -1.

c  Note that ANAL3, and the routines it calls are from the Little NPAG
c  program module, IDPC9A.FOR. 

c  Note that this module is linked first with itbig13.f, and the 
c  template model file is TSTMULTH.FOR (in which in Subroutine SYMBOL,
c  the user is told to code N=-1 if he wants to assume the standard
c  3-compartment linear model with analytic solutions, and in this 
c  case also establish the 5 parameters, {KE,KA,KCP,KPC,V} of this
c  model).

c-----------------------------------------------------------------------

c  idm222x5.f                                              11/16/10

c  idm222x5.f is the same as idm22x5.f except the dimension for PMAT

c  is changed to be (594,30) from (594,20). This makes it consistent
c  with the calling module, itbig9y.f.

c-----------------------------------------------------------------------

c  idm22x5.f                                    7/1/10

c  idm22x5 has the same differences from idm2x5.f that idcp_3f.f had 
c  from idcy_53f.f.

c  These include:

c  0. CALL IDCALCY(...) is replaced by CALL IDCALCP(...)
c  1. The declaration statements in IDCALCY are the same as those
c     in idcp_3f.f, except for the changes that COMMON/CNST no longer
c     has NTLAG; BS is now (500,7); COMMON CNST2 is changed to be the
c     same as in idm2x5.f.
c  1. The code in IDCALCP is the same as that in IDCALCP of idcp_3f.f.
c  2. SUBROUTINE FUNC2(M,F) is replaced by SUBROUTINE FUNC1(M,F)
c  3. In FUNC1, F(594,6) is now declared to be F(3564)
c  4. In FUNC1, the double do loop to calculate F(I,J) now calculates
c     F((J-1)*M+I)
c  5. SUBROUTINE EVAL2(Y) is replaced by SUBROUTINE EVAL(F)
c  6. In EVAL, Y(594,6) is now declared F(3564)
c  7. In EVAL, CALL FUNC2(M,Y) is replaced by CALL FUNC1(M,F)

c-----------------------------------------------------------------------

c  idm2x5.f							4/03/10

c  idm2x5 has a bug correction to idm2x4. In Subroutine FUNC2, in the
c  IF(TIM(KNT) .EQ. 0.D0 .AND. SIG(KNS) .EQ. 0.D0) block, the time,
c  T, is also reset = 0 since the integration will again start from
c  time 0. When this wasn't done (in idm2x4.f), the results were
c  unpredictable (depending on how the DVODE integration routines
c  treated a (T,TOUT) pair which decreased rather than increased.

c-----------------------------------------------------------------------

c  For the comments of previous program, see idm2x5.f.

c-----------------------------------------------------------------------

	SUBROUTINE IDCALCP(JSUB,IG,NVAR,NOFIX,NRANFIX,IRAN,NDIM,
     1    ESTML,PMAT,INTLIST,RPAR,IPAR)

C  IDCALCP CALCULATES MATRIX PMAT WHICH HAS, IN COLUMN J, THE PARTIAL 
C  DERIVATIVES OF OBSERVATIONS IN ARRAY YO W.R.T. THE JTH PARAMETER 
C  ESTIMATE. THE PARTIAL DERIVATIVES ARE CALCULATED USING 
C  'FORWARD DIFFERENCES' -- SEE BELOW. SEE MORE DETAILS IN THE
C  MODULE WHICH CALL THIS MODULE.

C  INPUT ARE:

C  NVAR = NO. OF RANDOM VARIABLES STORED IN VECTOR ESTML.
C  NOFIX = NO. OF FIXED VALUES STORED IN VECTOR ESTML.
C  NRANFIX = NO. OF RANFIX PARAMETERS STORED IN VECTOR ESTML.
C  IRAN(I) = 1 IF PARAMATER I IN ESTML IS RANDOM AND REQUIRED TO BE 
C                 POSITIVE;
C           -1 IF PARAMETER I IN ESTML IS RANDOM AND MAY BE NEGATIVE;
C	       0 IF PARAMETER I IN ESTML IS FIXED; 
C            2 IF PARAMETER I IN ESTM  IS UNKNOWN BUT THE SAME FOR ALL
C                 SUBJECTS; I = 1,NVAR+NOFIX+NRANFIX.
C  NDIM = NO. OF COMPARTMENTS IN THE MODEL.
C  ESTML = VECTOR OF PARAMETER VALUES, I=1,NVAR+NOFIX.

C  INFORMATION FROM A SUBJECT DATA FILE WHOSE INFO IS PASSED TO THE 
C  ROUTINES IN THIS MODULE VIA COMMONS /OBSER/, /CNST/, /CNST2/, AND 
C  /SUM2/.


C  OUTPUT IS:

C  PMAT = NN x NVAR MATRIX DESCRIBED ABOVE, WHERE NN = M*NOS IF THERE 
C 	  ARE NO MISSING VALUES (-99'S) IN ARRAY YO. FOR EVERY MISSING
C	  VALUE, NN IS REDUCED BY 1. NOTE THAT M AND NOS ARE INPUT TO 
C	  THE PROGRAM VIA COMMON STATEMENTS.

c-----------------------------------------------------------------------

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

C*****INITIALIZE PROGRAM*****

	CALL SYMBOL(NBCOMP)

C  THE ABOVE CALL TO SYMBOL GUARANTEES THAT NTLAG IS PUT INTO 
C  COMMON/CNST.

C  NOTE THAT THIS PROGRAM NOW GETS N = NDIM AND NP = NVAR+NOFIX+NRANFIX
C  AS CALLING ARGUMENTS.

	N = NDIM
	NP = NVAR+NOFIX+NRANFIX

C  MUST CALCULATE THE OUTPUT CONCENTRATION ARRAY, Y, FOR THE PARAMETER
C  VECTOR, ESTML, AS WELL AS FOR THE NVAR VARIATIONS OF ESTML (EACH
C  RANDOM COORDINATE INCREASED BY A DELTA-VALUE, WHILE THE OTHER 
C  COORDINATES ARE HELD FIXED TO THEIR VALUES IN ESTML) TO OBTAIN THE 
C  PIECES TO CALCULATE PMAT.

C  NOTE THAT ESTML INCLUDES RANDOM, FIXED, AND RANFIX PARAMTER VALUES.

C  IN PARTICULAR, THE (NDEX,J) ELEMENT OF 

C  PMAT = (Y(NDEX;J) - Y(NDEX))/DELJ, WHERE DELJ IS AN INCREMENT IN THE 
C  JTH RANDOM PARAMETER DIRECTION, Y(NDEX) IS THE OUTPUT CONCENTRATION 
C  CORRESPONDING TO NDEX_TH NON-MISSING ENTRY IN YO (USING "COLUMN-WISE"
C  STACKING), ASSUMING THE PARAMETER VALUES ARE THE ONES IN ESTML, AND 
C  Y(NDEX;J) IS THE SAME AS Y(NDEX), EXCEPT THAT THE JTH RANDOM 
C  PARAMETER VALUE = ESTML(J)+DELJ.

C  NOTE, THEREFORE, THAT PMAT WILL BE NOBACT x NVAR, WHERE NOBACT = 
C  M*NOS - MISTOT, WHERE MISTOT IS THE TOTAL NO. OF MISSING VALUES
C  (-99'S) IN ARRAY YO.


C  INITIALIZE JCOL = 0. IT IS THE RUNNING INDEX OF THE LAST COLUMN OF
C  PMAT WHICH HAS BEEN FILLED (COLUMN JCOL OF PMAT REFERS TO THE
C  JCOLth RANDOM VARIABLE).

	JCOL = 0

	DO 5000 JSIM = 0,NP

C        write (*,*) "DO 5000 Calculating PMAT at",JSIM,"of",NP

C  IF JSIM = 0, SIMULATE SYSTEM TO OBTAIN THE Y-ARRAY EVALUATED AT THE
C  PARAMETER VALUES IN ESTML. IF JSIM > 0, AND IRAN(JSIM) = 1 OR -1, 
C  WHICH MEANS THAN THE JSIMth ENTRY IN ESTML IS RANDOM -- SIMULATE THE 
C  SYSTEM TO OBTAIN THE Y-ARRAY AS ABOVE, EXCEPT WITH THE JSIMth ENTRY 
C  IN ESTML INCREASED BY DELJ = ESTML(JSIM)*1.D-4.

C  1ST RESET ALL PARAMETER VALUES TO THEIR ORIGINAL ESTML VALUES. THEN
C  ADJUST IF JSIM > 0 AND IRAN(JSIM) = 1.

       DO I = 1,NP
        P(I)=ESTML(I)
       END DO


	IF(JSIM .GT. 0) THEN

C  IF IRAN(JSIM) = 0 OR 2, THE JSIMth ENTRY IN ESTML IS A FIXED VALUE
C  OR A RANFIX VALUE --> NO SIMULATION NEEDED --> GO TO 5000. 

	 IF(IRAN(JSIM) .EQ. 0 .OR. IRAN(JSIM) .EQ. 2) GO TO 5000

     	 DELJ=ESTML(JSIM)*1.D-4
	 P(JSIM)=ESTML(JSIM) + DELJ

	ENDIF 


C  CALL SUBROUTINE EVAL (REPLACES SUBR. SUMSQ) TO GET Y, EVALUATED
C  AT P(I) AS DEFINED ABOVE.

C        write (*,*) "Calling EVAL() in IDCALCP"

	CALL EVAL(JSUB,IG,Y,INTLIST,RPAR,IPAR)

C        write (*,*) "Ret. fr. EVAL() in IDCALCP"

C  IF JSIM = 0, STORE Y-VALUES INTO YEST; IF JSIM > 0, CALCULATE COLUMN
C  JCOL OF PMAT (NOTE, IF JSIM > 0, IRAN(JSIM) MUST = 1 OR -1; OTHERWISE
C  CONTROL WOULD HAVE PASSED FROM ABOVE DIRECTLY TO LABEL 5000).

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
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE FUNC1(JSUB,IG,M,F,INTLIST,RPAR,IPAR)

C  THIS SUBROUTINE, CALLED BY EVAL2, FINDS F(I) = OUTPUT CONC. AT
C  TIME I, I=1,M, GIVEN PARAMETER VALUES IN P.

C wmy20190513
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

C wmy20190513
       double precision, dimension(257) :: RPAR
       integer, dimension(257) :: IPAR
       integer, dimension(128) :: INTLIST
       integer III,JSUB,IG,JSUBmain,IGmain

C  NOTE THAT AS OF idm222x13.f, THE DIMENSIONS OF 6 IN XSTORE AND XPRED
C  HAVE BEEN CHANGED TO 20, WHICH IS WHAT THEY SHOULD HAVE BEEN ALL
C  ALONG (I.E., THE SAME AS FOR X).

C  NOTE THAT THE DIMENSIONS RELATED TO THE NO. OF OUTPUT EQS. IN
C  YO, YT AND Y ARE CHANGED TO MAXNUMEQ (FROM 6). 


C  NOTE THAT "7" IN THE ABOVE ARRAYS INDICATE THE NO. OF DRUGS ALLOWED.


        JSUBmain = JSUB
        IGmain = IG

C*****ODE CONSTANTS AND INITIALIZATION*****

C       write (*,*) "SR FUNC1()"

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

c  AS OF idm11x8.f, instead of R(1) = 0, the code has been changed to 
c  set R(2*I-1) = 0, for I = 1,NDRUG. I.E., All IV rates for all NDRUG
c  drugs are initialized to be 0 ... in case the 1st obs. time is 0,
c  which means that OUTPUT is called before the R(I) are set below.


C  CALL SUBROUTINE GETFA IN it2bdriv.f (THE FIRST TEMPLATE FILE TO 
C  INCLUDE GETFA IS TSTMULTG.FOR) TO OBTAIN THE VALUE OF FA FOR EACH
C  OF THE NDRUG DRUGS.

C  AS OF idm222x12.f, BEFORE CALLING GETFA, MUST SET
C  THE R(.) IN CASE ANY OF THE FA(.) ARE FUNCTIONS OF THE 
C  COVARIATES WHICH ARE ESTABLISHED FROM THE R(.) VALUES IN
C  GETFA.
 
      DO I=1,NI
       R(I)=RS(KNS,I)
      END DO


C        write (*,*) "SR FUNC1(... CALL GETFA() ...)"

	 CALL GETFA(FA,X,P,R,B,INTLIST)


C  NOTE THAT NBCOMP(I),I=1,NDRUG WAS SET IN SUBROUTINE SYMBOL AND
C  PASSED TO THIS ROUTINE VIA COMMON/BOLUSCOMP.


C  As of idm222x11.f, the code to save ND0, SIGO, RSO, is moved to 
c  before the IF(N .EQ. 0) GO TO 75  statement. The reason is that
c  before this routine returns, ND, SIG, and RS are reset back to these
c  values, even if N = 0, and so they must be established at this time.

C  AS OF idm222x9.f, SAVE ND, SIG, AND RS WHETHER OR NOT NTL = 1, SINCE
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

C        write (*,*) "SR FUNC1(if N==0 CALL GETIX() ...) N=", N

        IF(N .EQ. 0) GO TO 75


C  CALL SUBROUTINE GETIX IN it2bdriv.f (THE FIRST TEMPLATE FILE TO 
C  INCLUDE GETIX IS TSTMULTG.FOR) TO OBTAIN THE VALUE OF X (THE INITIAL
C  COMPARTMENT AMOUNT) FOR EACH OF THE N COMPARTMENTS.

	 CALL GETIX(N,X,P,R,B,INTLIST)



C  CALL SUBROUTINE GETTLAG IN it2bdriv.f (THE FIRST TEMPLATE FILE TO 
C  INCLUDE GETTLAG IS TSTMULTG.FOR) TO OBTAIN THE VALUE OF THE TIMELAG
C  FOR EACH OF THE NDRUG DRUGS.

   75	 CALL GETTLAG(TLAG,X,P,R,B,INTLIST)

C          write (*,*) "Ret. Fr. GETTLAG()", TLAG

C  IF ANY TLAG(.) VALUES RETURN AS .NE. 0, THEN, CALL SUBROUTINE SHIFT
C  TO ADJUST THE DOSAGE REGIMEN APPROPRIATELY.

      NTL = 0
      DO ID = 1,NDRUG
       IF(TLAG(ID) .NE. 0) NTL = 1
      END DO

C        write (*,*) "FUNC1( IF NTL==1 CALL SHIFT) NTL=",NTL

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

C      write (*,*) "FUNC1(... go to 12 or 45 ...); KNT,KNS=",
C     1   KNT,TIM(KNT),KNS,SIG(KNS)

      IF(TIM(KNT).GE.SIG(KNS)) GO TO 12
	IF(TIM(KNT).NE.0.0D0) GO TO 45

C  THE ONLY WAY THE FOLLOWING CALL TO OUTPUT CAN OCCUR IS IF TIM(KNT)
C  = 0 --> OBTAIN YT = OUTPUT VALUE(S) AT TIME 0.0.

      CALL OUTPUT(0.D0,YT,X,RPAR,IPAR)
	DO 2000 I=1,NOS
2000  Y(KNT,I)=YT(I)
      KNT=KNT+1
      GO TO 45

12    IF(TIM(KNT).GT.SIG(KNS)) GO TO 13
	IF(TIM(KNT).NE.0.0D0) GO TO 45

C  THE ONLY WAY THE FOLLOWING CALL TO OUTPUT CAN OCCUR IS IF TIM(KNT)
C  = 0 --> OBTAIN YT = OUTPUT VALUE(S) AT TIME 0.0.

      CALL OUTPUT(0.D0,YT,X,RPAR,IPAR)
	DO 2005 I=1,NOS
2005  Y(KNT,I)=YT(I)
      KNT=KNT+1

C      write (*,*) "KNT++ near 606 CALL OUTPUT(...); KNT=",
C     1  KNT,KNS,SIG(KNS)

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

C       write (*,*) "Reset SIG(KNS) to 0; KNS=", KNS

      ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(SIG(KNS) .LT. 0.D0)  CONDITION.


      DO I=1,NI
       R(I)=RS(KNS,I)
      END DO

	IF(NDRUG .EQ. 0) GO TO 81

C  AS OF idm222x12.f: MUST CALL GETFA BEFORE EVERY TIME THAT
C  FA(.) ARE USED IN CASE THE EQUATION(S) FOR THE FA(.) ARE BASED
C  ON THE COVARIATES, WHICH CAN CHANGE DOSE TO DOSE.

	 CALL GETFA(FA,X,P,R,B,INTLIST)


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

81    KNS=KNS+1

C      write (*,*) "KNS++ near 81, KNT,KNS,ND=",KNT,KNS,ND

C*****INTEGRATION OF EQUATIONS*****
C
C  DETERMINE IF, OBSER(ID=0), OR DOSE(ID=1), OR BOTH(ID=2).

45    IF(KNS.GT.ND) GO TO 15

C CODE CHANGE BELOW FOR idm222x8.f.

      IF(TIM(KNT) .EQ. 0.D0 .AND. KNT .GT. 1) THEN

C  AS OF idm222x8.f, A TIME RESET NO LONGER REQUIRES ALL INITIAL
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

111   FORMAT(//' THE CURRENT SUBJECT HAS AN OBSERVATION TIME RESET'/
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

C      write (*,*) "KNS = IKNS near 110; KNT,KNS=",KNT,KNS

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

C  AS OF idm222x12.f, BEFORE CALLING GETIX, MUST SET
C  THE R(.) IN CASE ANY OF THE INITIAL CONDITIONS FOR THE X(.)
C  ARE FUNCTIONS OF THE COVARIATES WHICH ARE ESTABLISHED FROM THE 
C  R(.) VALUES IN GETFA.
 
      DO I=1,NI
       R(I)=RS(KNS,I)
      END DO


C       write (*,*) "Reset T=0 near #802 CALL GETIX(); T=",T

        CALL GETIX(N,X,P,R,B,INTLIST)
		
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
C   IF(TIM(KNT) .EQ. 0.D0 .AND. KNT .GT. 1)  CONDITION.


C      write (*,*) "IF(TIM(KNT).NE.SIG(KNS)) GO TO 20",TIM(KNT),SIG(KNS)

      IF(TIM(KNT).NE.SIG(KNS)) GO TO 20
      ID=2
      TOUT=TIM(KNT)
      KNT=KNT+1
      KNS=KNS+1

C      write (*,*) "KNT++, KNS++ near 860",KNT,TIM(KNT),KNS,SIG(KNS),N

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

C       write (*,*) "At 30 KNT,KNS=",KNT,KNS

C wmy20190513 Start Copied from NPAG -----------------------------------
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
C 32      IF(NDIM .NE. -1) then {CALL USERANAL(...) ... }
C wmy20190513 End Copied from NPAG -------------------------------------

C       write (*,*) "CALL USERANAL or ANAL3, N=",N,JSUBmain,IG,T,TOUT

32      IF(N .NE. -1) CALL USERANAL(X,T,TOUT)
        IF(N .EQ. -1) CALL ANAL3(X,T,TOUT,RPAR,IPAR)

C       write (*,*) "Ret. fr. XP update with",
C     1  T,X(1),X(2),X(3),X(4)

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

C  AS OF idm222x12.f: MUST CALL GETFA BEFORE EVERY TIME THAT
C  FA(.) ARE USED IN CASE THE EQUATION(S) FOR THE FA(.) ARE BASED
C  ON THE COVARIATES, WHICH CAN CHANGE DOSE TO DOSE.

	 CALL GETFA(FA,X,P,R,B,INTLIST)


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

40      IF(KNT .LE. M) GO TO 45

C*****DETERMINE F(I)*****

	DO J=1,NOS
         DO I=1,M
	  F((J-1)*M+I)=Y(I,J)
	 END DO
	END DO


C  AS OF idm222x9.f, RESTORE THE VALUES FOR ND, SIG, AND RS, IN CASE
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

         JSUB = JSUBmain
         IG = IGmain
	
      RETURN
      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE EVAL(JSUB,IG,F,INTLIST,RPAR,IPAR)

C  THIS SUBROUTINE, CALLED BY MAIN, FINDS THE OUTPUT CONC. 
C  VECTOR, Y, EVALUATED AT PARAMETER VALUES IN VECTOR P, PASSED 
C  DIRECTLY TO SUBROUTINE FUNC2 VIA COMMON/PARAMD.

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

C        write (*,*) "EVAL(): CALL FUNC1"
	CALL FUNC1(JSUB,IG,M,F,INTLIST,RPAR,IPAR)
C        write (*,*) "EVAL(): Ret. fr. FUNC1"

        JSUB = JSUBmain
        IG = IGmain

	RETURN
	END


