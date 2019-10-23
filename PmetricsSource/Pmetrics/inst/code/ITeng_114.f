c  idm11x16.f                                              3/26/15

c  idm11x16. has the following change from idm11x15:

c  All numbers written out in F or G format are now tested to see if
c  they are inside [-1.D-99, 1.D-99]. If so, they are changed to be
c  0. The reason is that otherwise they will be printed out without 
c  the accompanying D or E (e.g., as .934-106, rather than .934E-106).

c  Note that this module is linked with npageng30.f initially.

c-----------------------------------------------------------------------

c  idm11x15.f                                               7/23/14

c  idm11x15 has the following changes from idm11x14:

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

c  2. If the program stops unexpectedly with the writing of format 111
c  in Subroutine FUNC, this same comment will now be written to
c  the file, ERRFIL, which is passed to FUNC in COMMON/ERR.

c-----------------------------------------------------------------------

c  idm11x14.f                                              4/8/14

c  idm11x14 has the following changes from idm11x13:

c  1. SUMSQJ is now dimensioned MAXNUMEQ, rather than 6 (MAXNUMEQ
c  is set in a parameter stmt. at the top of each routine that needs 
c  it).

c  2. In Subroutine FUNC, the dimensions related to the no. of output
c  equations have been changed from 6 to MAXNUMEQ (see comments in that
c  routine).

c  2. In Subroutines FUNC and PREDLAST3, the dimensions of 6 in XSTORE,
c  XPRED, and COMP have been changed to 20, as they should have been 
c  all along (i.e., this represents the maximum no. of compartments
c  allowed).

c-----------------------------------------------------------------------

c  idm11x13.f                                              10/26/12

c  idm11x13 has the following bug corrections to idm11x11:

C  1.  IN SUBROUTINE FUNC, BEFORE
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

c  idm11x12.f                                               8/28/12

c  idm11x12 has the following change to idm11x11:

c  In SUBROUTINE FUNC, the code to save ND0, SIGO, RSO, is moved to
c  before the IF(N .EQ. 0) GO TO 75  statement. The reason is that 
c  before this  routine returns, ND, SIG, and RS are reset back to these
c  values, even if N = 0, and so they must be established at this time.

c-----------------------------------------------------------------------

c  idm11x11.f                                              5/25/12

c  idm11x11 has the following changes from idm11x10:

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

c-----------------------------------------------------------------------

c  idm11x10.f                                              4/18/12

c  idm11x10 has the following changes to idm11x9.f:

c  It is to be used with it2beng19.f, which allows steady state doses
c  to be boluses as well as IVs. As a result, an additional parameter,
c  ISKIPBOL, is used so, in Subroutine FUNC, when convergence occurs in
c  a steady state dose set, the last bolus from that set will not be
c  reapplied below label 83.

c-----------------------------------------------------------------------

c  idm11x9.f                                               3/4/12

c  idm11x9 has the following bug fix to idm11x8.f. In Subroutine FUNC,
c  the code to save ND, SIG, and RS before altering them if there are 
c  time lag parameters (in the call to GETTLAG) is now executed whether
c  or not there are time lag parameters. The reason is that, with steady
c  state doses, the first SIG(.) time in a steady state dose set is
c  reset to be 0 after the steady state dose is identified. And this
c  time must be reset back to be its original negative value at the end
c  of the routine so that the next time the routine is called, the 
c  program will again know when a steady state dose is coming. 

c-----------------------------------------------------------------------

c  idm11x8.f                                               1/29/12

c  idm11x8.f has the same changes from idm11x6.f that idm1x8.f has from
c  idm1x6.f, namely:

c  1. A bug is corrected in Subroutine FUNC - now time resets are
c  identified by just the observation time = 0 (i.e., the dose time = 0
c  is no longer required). This is because it is possible for a dose
c  time (especially if there are timelags) to be after the last
c  observation time in a section of the patient file (before a time
c  reset), and if this happens, the program will not be able to
c  identify the observation time of 0 as a time reset.

c  2. It can accommodate steady state dose regimens as created by 
c  new subroutine NEWWORK1.FOR in it2beng17.f. And it has new
c  Subroutine PREDLAST3 (both of these 2 new subroutines are based on
c  stand-a-lone versions of the same name) which is called by
c  Subroutine FUNC to predict the final (steady state) compartment
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

c  idm11x6.f                                               12/30/10

c  idm11x6 has the following change to idm11x5:

c  In Subroutine FUNC, it has code that calls Subroutine ANAL3, rather
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

c  idm11x5.f                                                 6/30/10

c  idm11x5 is the same as idm1x5, except it has the changes that are 
c  needed by the Big IT2B "engine" (first in series is itbig9x.f) 
c  compared to the Big NPAG "engine" (currently bigmlt6.f).

c  In particular, rather than returning the total sum of squares over 
c  all the M x NOS observed values (SUMSQ), this routine will return
c  SUMSQJ(J), J=1,NOS, where each SUMSQ(J) = is the sum of squares 
c  just for output equation J. This requires changes at the bottom of 
c  this main routine, in FUNC where SUMSQJ(J) are calculated, rather
c  than F(.), and in routine SUMSQ.

c-----------------------------------------------------------------------

c  idm1x5.f							4/03/10

c  idm1x5 has a bug correction to idm1x4. In Subroutine FUNC, in the
c  IF(TIM(KNT) .EQ. 0.D0 .AND. SIG(KNS) .EQ. 0.D0) block, the time,
c  T, is also reset = 0 since the integration will again start from
c  time 0. When this wasn't done (in idm1x4.f), the results were
c  unpredictable (depending on how the DVODE integration routines
c  treated a (T,TOUT) pair which decreased rather than increased.

c-----------------------------------------------------------------------

c  see the code in idm1x5.f for the comments for previous programs.

c-----------------------------------------------------------------------

	SUBROUTINE IDPC(JSUB,IG,X,SUMSQJ,INTLIST,RPAR,IPAR)

C  INPUT ARE: 

C  INFORMATION FROM A SUBJECT DATA FILE WHICH HAS BEEN READ IN 
C  PREVIOUSLY. THIS INFO IS PASSED TO THE OTHER ROUTINES IN THIS 
C  MODULE BY COMMONS /OBSER/, /CNST/, /CNST2/, AND  /SUM2/.

C  X(I) = ITH COORDINATE OF THE GRID POINT OF INTEREST (INCLUDING FIXED
C	  PARAMETER VALUES).
C  STDEV(I,J) = STD DEV FOR THE ITH OBSERVATION OF THE JTH OUTPUT EQ.
C               (INPUT IN BLANK COMMON TO SUBROUTINE FUNC).

C  OUTPUT IS:

C  SUMSQJ(J), J=1,NOS, WHERE SUMSQ(J) = SUM, FOR THIS SUBJECT, OVER 
C  I=1,M (ACTUALLY THE (I,J) CONTRIBUTION IS IGNORED IF YO(I,J) = -99 
C  --> MISSING VALUE), OF ((YO(I,J)-H(I,J))/STDEV(I,J))**2, WHERE 
C  H(I,J) = PREDICTED VALUE OF THE JTH OUTPUT EQ AT THE ITH OBSERVATION 
C  TIME, ASSUMING THE IGTH GRID POINT, X. NOTE THAT M AND NOS ARE INPUT 
C  IN COMMONS SUM2 AND CNST2, RESPECTIVELY.

C-----------------------------------------------------------------------

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


C*****INITIALIZE PROGRAM*****

        JSUBmain = JSUB
	CALL SYMBOL(NBCOMP)

C  THE ABOVE CALL OBTAINS INFO FROM COMMONS.

C  FIND THE SUM OF SQUARES OF DIFFERENCES BETWEEN THE OBSERVED
C  VALUES AND THE PREDICTED VALUES (NORMALIZED BY THE ASSAY
C  VARIANCE OF EACH OBSERVATION) FOR THIS POINT.

C  PUT MODEL PARAMETER VALUES INTO P.

        DO I=1,NP
         P(I)=X(I)
        END DO

C   SUMSQJ(J) RETURNS FROM SUBROUTINE SUMSQ AS THE SUM OF SQUARES
C   FOR THIS SET OF (X1,X2,X3,X4,X5) VALUES, FOR OUTPUT EQUATION J,
C   J=1,NOS.
   
        CALL SUMSQ(JSUB,IG,SUMSQJ,INTLIST,RPAR,IPAR)

        JSUB = JSUBmain

C        write (*,*) JSUB,IG,"Ret. to main w/SUMSQ=",SUMSQJ

        RETURN
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE FUNC(JSUB,IG,M,SUMSQJ,INTLIST,RPAR,IPAR)

C  FUNCTION TO DETERMINE THE ENTRIES IN F, GIVEN P.

C wmy20190513
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

C wmy20190513
       double precision, dimension(257) :: RPAR
       integer, dimension(257) :: IPAR
       integer, dimension(128) :: INTLIST
       integer JSUB,IG,III,JSUBmain

C  NOTE THAT "7" IN THE ABOVE ARRAYS INDICATE THE NO. OF DRUGS ALLOWED.

C  NOTE THAT F HAS DIMENSION 3564 = 594*6 SINCE IT HAS NOS*M ENTRIES,
C  THE MAX VALUE OF NOS = 6, AND THE MAX VALUE FOR M = 99*6 = 594.

C  R(7) CHANGED TO R(20) <-- No. of 'rate inputs'
C  B(3) CHANGED TO B(20) <-- No. of different bolus inputs
C  CHANGED X(3) TO X(20) <-- No. of compartments
C  IC(10) CHANGED TO IC(20) <-- Initial conditions in compartments;
C 		should have been changed to 20 previously (like X,B).
C  NBCOMP(10) CHANGED TO NBCOMP(20) <-- Same remarks as for IC.
C  P(10) CHANGED TO P(32) <-- No. of parameters


C      Write out COMMON/CNST/ N,ND,NI,NUP,NUIC,NP
C        write (*,*) "In FUNC: N,ND,NI,NUP,NUIC,NP=",N,ND,NI,NUP,NUIC,NP



C*****ODE CONSTANTS AND INITIALIZATION*****

        JSUBmain = JSUB
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

C  AS OF idm11x13.f, BEFORE CALLING GETFA, MUST SET
C  THE R(.) IN CASE ANY OF THE FA(.) ARE FUNCTIONS OF THE 
C  COVARIATES WHICH ARE ESTABLISHED FROM THE R(.) VALUES IN
C  GETFA.
 
      DO I=1,NI
       R(I)=RS(KNS,I)
      END DO


	 CALL GETFA(FA,X,P,R,B,INTLIST)

C         write (*,*) "IDPC::FUNC: Ret. fr. GETFA"

C  NOTE THAT NBCOMP(I),I=1,NDRUG WAS SET IN SUBROUTINE SYMBOL AND
C  PASSED TO THIS ROUTINE VIA COMMON/BOLUSCOMP.

C  As of idm11x12.f, the code to save ND0, SIGO, RSO, is moved to before
c  the IF(N .EQ. 0) GO TO 75  statement. The reason is that before this
c  routine returns, ND, SIG, and RS are reset back to these values,
c  even if N = 0, and so they must be established at this time.

C  AS OF idm11x9.f, SAVE ND, SIG, AND RS WHETHER OR NOT NTL = 1, SINCE
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


C  CALL SUBROUTINE GETIX IN it2bdriv.f (THE FIRST TEMPLATE FILE TO 
C  INCLUDE GETIX IS TSTMULTG.FOR) TO OBTAIN THE VALUE OF X (THE INITIAL
C  COMPARTMENT AMOUNT) FOR EACH OF THE N COMPARTMENTS.

	 CALL GETIX(N,X,P,R,B,INTLIST)

C         write (*,*) "IDPC::FUNC: Ret. fr. GETIX"

C  CALL SUBROUTINE GETTLAG IN it2bdriv.f (THE FIRST TEMPLATE FILE TO 
C  INCLUDE GETTLAG IS TSTMULTG.FOR) TO OBTAIN THE VALUE OF THE TIMELAG
C  FOR EACH OF THE NDRUG DRUGS.

   75	 CALL GETTLAG(TLAG,X,P,R,B,INTLIST)

C         write (*,*) "IDPC::FUNC: Ret. fr. 75 GETTLAG"

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

C         write (*,*) "IDPC::FUNC: Ret. fr. SHIFT"

C  ESTABLISH THE VALUES IN EACH DRUG'S PO COLUMN TO THE CORRESPONDING
C  COLUMN IN ARRAY BS.

      DO I=1,ND
       DO J=1,NDRUG
        BS(I,J)=RS(I,2*J)
       END DO
      END DO


	ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(NTL .EQ. 1)  CONDITION.


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

C  AS OF idm11x13.f: MUST CALL GETFA BEFORE EVERY TIME THAT
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

C*****INTEGRATION OF EQUATIONS*****
  
C  DETERMINE IF, OBSER(ID=0), OR DOSE(ID=1), OR BOTH(ID=2).

45    IF(KNS .GT. ND) GO TO 15

C CODE CHANGE BELOW FOR idm11x8.f.

      IF(TIM(KNT) .EQ. 0.D0 .AND. KNT .GT. 1) THEN

C  AS OF idm11x8.f, A TIME RESET NO LONGER REQUIRES ALL INITIAL
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

C     WRITE(42,111) ND,KNS,SIG(KNS)
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

C  AS OF idm11x13.f, BEFORE CALLING GETIX, MUST SET
C  THE R(.) IN CASE ANY OF THE INITIAL CONDITIONS FOR THE X(.)
C  ARE FUNCTIONS OF THE COVARIATES WHICH ARE ESTABLISHED FROM THE 
C  R(.) VALUES IN GETFA.
 
      DO I=1,NI
       R(I)=RS(KNS,I)
      END DO


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

C wmy20190513 Start Copied from NPAG -----------------------------------
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
C 32      IF(NDIM .NE. -1) then
C wmy20190513 End Copied from NPAG -------------------------------------


C        write (*,*) "IDPC::FUNC: Prediction at KNT,KNS,N",KNT,KNS,N

32      IF(N .NE. -1) CALL USERANAL(X,T,TOUT,RPAR,IPAR)
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

        IF(ID .EQ. 1) GO TO 35
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

C  AS OF idm11x13.f: MUST CALL GETFA BEFORE EVERY TIME THAT
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

C***** DETERMINE SUMSQJ(J),J=1,NOS *****

C  SUMSQJ(J), AFTER THE FOLLOWING LOOPS, WILL BE THE SUM OF SQUARES OF
C  NORMALIZED DIFFERENCES BETWEEN ACTUAL AND PREDICTED OBSERVED VALUES,
C  FOR OUTPUT EQ. J; J=1,NOS (MISSING VALUES ARE IGNORED OF COURSE --
C  YO(I,J) = -99 --> THIS OBSERVED LEVEL IS MISSING).

        DO J=1,NOS
         SUMSQJ(J) = 0.D0
         DO I=1,M
          IF(YO(I,J) .NE. -99) SUMSQJ(J) = SUMSQJ(J) + 
     1                         ((Y(I,J)-YO(I,J))/STDEV(I,J))**2.D0
         END DO
        END DO


C  AS OF idm11x9.f, RESTORE THE VALUES FOR ND, SIG, AND RS, IN CASE
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
      RETURN
      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE SUMSQ(JSUB,IG,SUMSQJ,INTLIST,RPAR,IPAR)

C  SUBROUTINE TO EVALUATE THE SUM OF SQUARES OF THE RESIDUAL VECTOR.

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
CŠCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE USERANAL(X,TIN,TOUT,RPAR,IPAR)

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
C           X - AN ARRAY OF DIMENSION 20. IN THE STANDARD 3-COMPARTMENT
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

        IMPLICIT REAL*8(A-H,O-Z)
        DIMENSION X(20),ATOL(20),RWORK(1002),IWORK(50)

        DIMENSION RPAR(257)
        DIMENSION IPAR(257)

C  AS OF idm11x15.f, THE DIMENSION OF RWORK IS CHANGED FROM 300 TO
C  1002 TO ACCOMADATE AN NDIM (NEQ IN SUBROUTINE DVODE) OF UP TO 20. SO
C  CHANGE LRW BELOW TO 1002. SIMILARLY THE DIMENSION OF IWORK AND
C  LIW BELOW ARE CHANGED FROM 40 TO 50.

	EXTERNAL DIFFEQ,JACOB
	COMMON/TOUSER/NDIM,MF,RTOL,ATOL

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

        CALL DVODE(DIFFEQ,NDIM,X,TIN,TOUT,ITOL,RTOL,ATOL,ITASK,ISTATE,
     1            IOPT,RWORK,LRW,IWORK,LIW,JACOB,MF,RPAR,IPAR)

c        IF (ISTATE .LT. 0) THEN
c         WRITE(*,16) ISTATE
c 16      FORMAT(///' On return from DVODE, ISTATE =',I3)
c	ENDIF


	TIN=TOUT

        RETURN
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE JACOB(NDIM, T, X, ML, MU, PD, NRPD, RPAR, IPAR)

	IMPLICIT REAL*8(A-H,O-Z)
        COMMON/PARAMD/ P
        COMMON/INPUT/ R,B


        DIMENSION X(NDIM), PD(NRPD,NDIM), P(32),R(37),B(20)

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


C  OUTPUT ARE:

C  PD(I,J) = PARTIAL DERIVATIVE OF XP(I) W.R.T. X(J), WHERE XP(I)
C	     ARE CALCULATED IN ROUTINE DIFFEQ ABOVE.

        RETURN
        END	
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C						  
      SUBROUTINE PREDLAST3(NN,NSET,XSTORE,XPRED,ICONV)

      use npag_utils, only : thesame

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XSTORE(100,20),XPRED(20),COMP(5,20)

      REAL*8 F,DEL1,DEL2,TOL1,TOL2,A1,A2,A3

C  THIS SUBROUTINE IS CALLED BY SUBROUTINE FUNC WITH NSET SETS OF NN
C  COMPARTMENT  VALUES IN XSTORE. USE THE LAST 5 SETS OF VALUES TO
C  PREDICT THE FINAL (STEADY STATE) COMPARTMENT AMOUNTS AFTER THE LAST
C  (100TH) DOSE SET. 

C  IF THESE VALUES "CONVERGE", SET ICONV = 1, AND WRITE THE PREDICTED
C  VALUES INTO XPRED. IF THEY DON'T CONVERGE, SET ICONV = 0.

C  TOL1 AND TOL2 ARE, FOR NOW, HARDCODED TO BE .0005.

        TOL1 = .0005D0
        TOL2 = .0005D0


C  THE LAST 5 SETS OF VALUES ARE IN XSTORE(NSET-4:NSET,.). PUT THESE
C  VALUES INTO COMP(.,.).

      II = 0
    
      DO I = NSET-4,NSET
       II = II+1
       DO J = 1,NN
        COMP(II,J) = XSTORE(I,J)
       END DO
      END DO


C  FOR EACH COMPARTMENT AMOUNT, SEE IF THE FINAL STEADY STATE COMP.
C  AMOUNT CAN BE PREDICTED ACCURATELY.

      DO IN = 1,NN

       A1 = COMP(1,IN)
       A2 = COMP(2,IN)
       A3 = COMP(3,IN)
       DEL1 = A2 - A1
       DEL2 = A3 - A2

C  TEST FOR DEL1 = 0. IF SO, SEE ISAMETOT BELOW.

       CALL THESAME(DEL1,0.D0,ISAME1)

       IF(ISAME1 .EQ. 0) THEN

        F = DEL2/DEL1

C  THE UNDERLYING ASSUMPTION IS THAT THE RATIO F = DEL2/DEL1
C  IS CONTANT BETWEEN CONSECUTIVE OUTPUT DIFFERENCES. IF SO, THEN
C  THE STEADY STATE VALUE WILL BE A1 + DEL1/(1 - F) (SEE SS.EXP
C  IN \ALAN3\STEADYSTATE). CALCULATE THIS VALUE AND CALL IT PRED1.

C  BUT, IF DEL2 = DEL1, THEN F = 1. IN THIS CASE, CAN'T DO THE FOLLOWING 
C  CALCULATION FOR PRED1, AND WE WOULDN'T WANT TO DO IT SINCE 
C  DEL2 = DEL1 --> A2 - A1 = A3 - A2 --> A1, A2, AND A3 ARE IN AN 
C  ARITHMETIC PROGRESSION --> THERE OBVIOUSLY CAN BE NO CONVERGENCE
C  SINCE, AFTER 100 DOSES, THE VALUE WOULD JUST A1 + 99*DEL1 ... 
C  UNLESS DEL1 = 0, IN WHICH CASE THE VALUE WOULD CONVERGE TO A1.
C  IN THIS CASE SET ISAMEF1 = 1, AND SKIP CALC. OF PRED1. AND THEN
C  SEE THE LOGIC RELATED TO ISAMEF1 BELOW.



        CALL THESAME(F,1.D0,ISAMEF1)
        IF(ISAMEF1 .EQ. 0) PRED1 = A1 + DEL1/(1.D0 - F)

       ENDIF


C  SIMILARLY, CALCULATE PRED2 (BASED ON (A2,A3,A4)) AND PRED3 (BASED
C  ON (A3,A4,A5).

       A1 = COMP(2,IN)
       A2 = COMP(3,IN)
       A3 = COMP(4,IN)
       DEL1 = A2 - A1
       DEL2 = A3 - A2

C  TEST FOR DEL1 = 0. IF SO, SEE ISAMETOT BELOW.

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

C  TEST FOR DEL1 = 0. IF SO, SEE ISAMETOT BELOW.

       CALL THESAME(DEL1,0.D0,ISAME3)

       IF(ISAME3 .EQ. 0) THEN
        F = DEL2/DEL1


        CALL THESAME(F,1.D0,ISAMEF3)
        IF(ISAMEF3 .EQ. 0) PRED3 = A1 + DEL1/(1.D0 - F)
       ENDIF


C  ASSUMING A NEGATIVE EXPONENTIAL PATTERN FIT (SEE SS.EXP IN 
C  \ALAN3\STEADYSTATE OR HOME NOTES, PG.2 ON 9/11/11 FOR DETAILS) ON
C (PRED1,PRED2,PRED3), CALCULATE PREDNEG.

C  BUT ONLY DO THIS CALCULATION, AND THE SUBSEQUENT 
C  CONVERGENCE DETERMINATION IF ISAME1 = ISAME2 = ISAME3 = 0, AND
C  ISAMEF1 = ISAMEF2 = ISAMEF3 = 0. OTHERWISE, AT LEAST ONE OF THE
C  PREDICTED VALUES ABOVE WAS NOT CALCULATED.

       ISAMETOT = ISAME1 + ISAME2 + ISAME3
       ISAMEFTOT = ISAMEF1 + ISAMEF2 + ISAMEF3


       IF(ISAMETOT .EQ. 0 .AND. ISAMEFTOT .EQ. 0) THEN

C EDITED CODE BELOW FOR idm11x11.f.

C  IF PRED1 + PRED3 - 2*PRED2 = 0, PREDNEG (SEE BELOW) CANNOT BE 
C  CALCULATED. IN THIS CASE, PRED2 - PRED1 = PRED3 - PRED2 --> 
C  THE SEQUENCE (PRED1, PRED2, PRED3) IS LINEAR, WHICH CANNOT BE 
C  MODELED WITH AN EXPONENTIAL FIT (SEE COMMENTS ABOVE). SO, IF THIS
C  HAPPENS, CONVERGENCE WILL BE SATISFIED IF THESE 3 VALUES ARE 
C  VIRTUALLY THE SAME - I.E., ONLY THE REQUIREMENT INVOLVING TOL1
C  WILL BE NEEDED FOR CONVERGENCE (RECALL THE ONLY REASON FOR THE
C  EXTRA NEGATIVE EXPONENTIAL FIT, AND THE CALCULATION OF PREDNEG IS FOR
C  THOSE CASES WHERE PRED1, PRED2, AND PRED3 ARE NOT ALL VIRTUALLY THE
C  SAME VALUE).

        DEN = PRED1+PRED3-2.D0*PRED2
        CALL THESAME(DEN,0.D0,ISAMEDEN)
      
        IF(ISAMEDEN .EQ. 0) PREDNEG = (PRED1*PRED3 - PRED2*PRED2)/DEN

C  NOW CHECK FOR CONVERGENCE, WHICH HAS BEEN OBTAINED IF 
C  |PRED3/PRED2 - 1| < TOL1 AND |PREDNEG/PRED3 - 1| < TOL2. 

        ICONV = 1
        IF(DABS(PRED3/PRED2 - 1.D0) .GE. TOL1) ICONV = 0
        IF(ISAMEDEN .EQ. 0 .AND. DABS(PREDNEG/PRED3 - 1.D0) .GE. TOL2)
     1   ICONV = 0

C  IF ICONV = 1 FOR THIS COMPARTMENT, IN, STORE THE PREDICTED AMOUNT,
C  AND CONTINUE TO THE NEXT COMPARTMENT. NOTE BELOW THAT 
C  NON-CONVERGENCE IN ANY COMPARTMENT ENDS THE PROCESS SINCE TO
C  CONVERGE, ALL COMPARTMENT PREDICTIONS MUST CONVERGE.

        IF(ICONV .EQ. 1 .AND. ISAMEDEN .EQ. 1) XPRED(IN) = PRED3 
        IF(ICONV .EQ. 1 .AND. ISAMEDEN .EQ. 0) XPRED(IN) = PREDNEG

C EDITED CODE ABOVE FOR idm11x11.f.

       ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(ISAMETOT .EQ. 0 .AND. ISAMEFTOT .EQ.0)
C  CONDITION.


C  IF ISAMETOT .GT. 0, THERE ARE TWO POSSIBILITIES (AND NOTE THAT IT
C  DOSEN'T MATTER WHAT ISAMEFTOT IS IN THIS CASE):

C   ISAMETOT = 3, IN WHICH CASE COMP(1:4,IN) ARE ALL THE SAME.
C   ISAMETOT = 1 OR 2, IN WHICH CASE SOME OF THE COMP(1:4,IN) VALUES
C     ARE THE SAME, AND SOME ARE NOT.

C  IN THE FORMER CASE, VERIFY THAT COMP(5,IN) IS THE SAME VALUE AS
C  THE COMP(1:4,IN). IF SO, SET THE PREDICTED VALUE = THIS VALUE
C  (I.E., THE PREDICTED VALUE FOR A CONSTANT FUNCTION IS THE
C  CONSTANT VALUE), AND SET ICONV = 1. OTHERWISE, SET ICONV = 0
C  SINCE THERE IS NO WAY TO FIT 4 VALUES WHICH ARE THE SAME AND ONE
C  WHICH IS NOT USING A NEGATIVE EXPONENTIAL FUNCTION.

C  IN THE LATTER CASE, SINCE SOME OF THE COMP(1:4,IN) VALUES ARE THE
C  SAME, AND SOME ARE NOT, SET ICONV = 0 FOR THE SAME REASON AS 
C  STATED IN THE PREVIOUS PARAGRAPH.

       IF(ISAMETOT .EQ. 3) THEN

        CALL THESAME(COMP(5,IN),COMP(1,IN),ISAME)

        IF(ISAME .EQ. 1) THEN
         ICONV = 1
         XPRED(IN) = COMP(1,IN)
        ENDIF

        IF(ISAME .EQ. 0) ICONV = 0
  
       ENDIF

       IF(ISAMETOT .EQ. 1 .OR. ISAMETOT .EQ. 2) ICONV = 0


C  IF ICONV = 0, CONVERGENCE WAS NOT ACHIEVED.

       IF(ICONV .EQ. 0) RETURN


      END DO

C  THE ABOVE END DO IS FOR THE  DO IN = 1,NN  LOOP.

C  TO GET TO THIS POINT, ALL COMPARTMENT AMOUNTS HAVE CONVERGED, AND
C  THEIR PREDICTED AMOUNTS HAVE BEEN STORED INTO XPRED(IN),IN=1,NN.


      RETURN
      END


		

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


c  idm2x16a.f                                               3/20/16

c  idm2x16a has the following change from idm2x15a:

c  Comments regarding NPP now indicate that it = NVAR+NOFIX+NRANFIX,
c  rather than just NVAR+NOFIX. There are no functional changes to
c  this new module.

c-----------------------------------------------------------------------

c  idm2x15a.f                                              3/26/15

c  idm2x15a has the following change from idm2x15:

c  All numbers written out in F or G format are now tested to see if
c  they are inside [-1.D-99, 1.D-99]. If so, they are changed to be
c  0. The reason is that otherwise they will be printed out without 
c  the accompanying D or E (e.g., as .934-106, rather than .934E-106).

c  Note that this module is linked with npageng30.f initially.

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
c  been changed to 20, as they should have been all along (i.e., this
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
c  - P is redimensioned 32. It will hold only the parameters of the
c    model (although some of those parameters may be initial conditions)
c    and there are 20 allowable random paramaters and 12 allowable
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


	SUBROUTINE IDCALCY(JSUB,IG,NPP,NDIM,ESTML,YPRED,NUMEQT,
     1    INTLIST,RPAR,IPAR)


C  INPUT ARE:

C  NPP = NO. OF PARAMETERS (RANDOM AND FIXED) IN THE PARAMATER 
C       VECTOR, ESTML.
C  NDIM = NO. OF COMPARTMENTS IN THE MODEL.
C  ESTML = VECTOR OF PARAMETER ESTIMATES (RANDOM AND FIXED).

C  INFORMATION FROM A SUBJECT DATA FILE WHOSE INFO IS PASSED TO THE 
C  ROUTINES IN THIS MODULE VIA COMMONS /OBSER/, /CNST/, /CNST2/, AND 
C  /SUM2/.


C  OUTPUT IS:

C  YPRED(I,J), I=1,M; J=1,NOS = THE PREDICTED VALUE FOR THE ITH 
C	OBSERVATION OF THE JTH OUTPUT EQUATION, GIVEN THE INPUT VECTOR
C	ESTML. M AND NOS ARE INPUT TO THIS MODULE VIA COMMONS SUM2 AND
C	CNST2, RESPECTIVELY.

c-----------------------------------------------------------------------

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


C*****INITIALIZE PROGRAM*****

        JSUBmain = JSUB
        IGmain = IG
	CALL SYMBOL(NBCOMP)

C  THE ABOVE CALL OBTAINS INFO FROM COMMONS.

C  NOTE THAT THIS PROGRAM NOW GETS N = NDIM AND NPP = NVAR+NOFIX+NRANFIX
C  AS CALLING ARGUMENTS.

	N = NDIM
	NP = NPP

C  CALCULATE THE OUTPUT CONCENTRATION VECTOR, Y, FOR THE PARAMETER
C  VECTOR, ESTML.

C  PUT MODEL PARAMETER VALUES INTO P.

        DO I=1,NP
	  P(I) = ESTML(I)
	END DO


C  CALL SUBROUTINE EVAL2 TO GET Y, EVALUATED
C  AT ESTML(I) AS DEFINED ABOVE.

	CALL EVAL2(JSUB,IG,YPRED,NUMEQT,INTLIST,RPAR,IPAR)

        JSUB = JSUBmain
        IG = IGmain

        RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE FUNC2(JSUB,IG,M,YPRED,NUMEQT,INTLIST,RPAR,IPAR)

C  THIS SUBROUTINE, CALLED BY EVAL2, FINDS YPRED(I) = OUTPUT CONC. AT
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
     1 RS(5000,34),RSO(5000,34),YT(MAXNUMEQ),YO(594,MAXNUMEQ),
     2 YPRED(594,NUMEQT), BS(5000,7),Y(594,MAXNUMEQ),B(20),NBCOMP(7),
     3 TLAG(7),FA(7),XSTORE(100,20),XPRED(20),XVERIFY(100)

      CHARACTER ERRFIL*20

C wmy20190513
       double precision, dimension(257) :: RPAR
       integer, dimension(257) :: IPAR
       integer, dimension(128) :: INTLIST
       integer III,JSUB,IG,JSUBmain,IGmain

C  NOTE THAT AS OF idm2x14.f, THE DIMENSIONS OF 6 IN XSTORE AND XPRED
C  HAVE BEEN CHANGED TO 20, WHICH IS WHAT THEY SHOULD HAVE BEEN ALL
C  ALONG (I.E., THE SAME AS FOR X).

C  NOTE THAT THE DIMENSIONS RELATED TO THE NO. OF OUTPUT EQS. IN
C  YO, YT AND Y ARE CHANGED TO MAXNUMEQ (FROM 6). NUMEQT COULD NOT
C  BE USED BECAUSE THESE ARRAYS WERE NOT PASSED TO THIS ROUTINE AS
C  DUMMY ARGUMENTS.

C  THE 2ND DIMENSION OF YPRED IS CHANGED TO NUMEQT, SINCE IT IS PASSED
C  IN THE ARGUMENT LIST, AND CAN THEREFORE BE VARIABLY DIMENSIONED BY
C  NUMEQT.


C  NOTE THAT "7" IN THE ABOVE ARRAYS INDICATE THE NO. OF DRUGS ALLOWED.

C*****ODE CONSTANTS AND INITIALIZATION*****

      JSUBmain = JSUB
      IGmain = IG

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


	 CALL GETFA(FA,X,P,R,B,INTLIST)


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

	 CALL GETIX(N,X,P,R,B,INTLIST)



C  CALL SUBROUTINE GETTLAG IN npemdriv.f (THE FIRST TEMPLATE FILE TO 
C  INCLUDE GETTLAG IS TSTMULTG.FOR) TO OBTAIN THE VALUE OF THE TIMELAG
C  FOR EACH OF THE NDRUG DRUGS.

   75	 CALL GETTLAG(TLAG,X,P,R,B,INTLIST)

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

C wmy20190513 Start Copied from NPAG -----------------------------------
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
C 32      IF(NDIM .NE. -1) then
C wmy20190513 End Copied from NPAG -------------------------------------

32      IF(N .NE. -1) CALL USERANAL(X,T,TOUT)
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

	 CALL GETFA(FA,X,P,B,INTLIST)


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

C*****DETERMINE YPRED(I)*****

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
	SUBROUTINE EVAL2(JSUB,IG,YPRED,NUMEQT,INTLIST,RPAR,IPAR)

C  THIS SUBROUTINE, CALLED BY MAIN, FINDS THE OUTPUT CONC. 
C  VECTOR, YPRED, EVALUATED AT PARAMETER VALUES IN VECTOR P, PASSED 
C  DIRECTLY TO SUBROUTINE FUNC2 VIA COMMON/PARAMD.

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


c  it2branfix1.f                                           4/11/16

c  it2branfix1 has the following changes from it2beng27:

c  1. it2branfix1 is an extension to it2beng27. It has the added 
c  capability to estimate the values of parameters that are the same for
c  all subjects. Note that new Subroutines ELDERY2 and CALCRF are added
c  to calculate these updated estimates.

c  This means that now the user can designate parameters as fixed
c  (IRAN(.) = 0), random (IRAN(.) = 1 or -1), or ranfix (IRAN(.) = 2).

c  2. Formats 7126, 7124, and 7123 are changed to show that the 
c  output files are made by it2branfix1 rather than it2beng27, and
c  they also now show MAR_16 as the date, rather than JUL_11.

c  3. Note that modules idm2x15a.f and idm222x15.f have been updated
c  to be idm2x16a.f and idm222x16.f, respectively

c  4. Note that the instruction file made by the new PC Prep program,
c  IT2B114.FOR is now it2b102.inp, updated from it2b101.inp. The
c  difference of course is that it2b102.inp includes RANFIX parameter
c  info.

c  5. All messages to the user about there being an "ill-conditioned"
c  problem, now have an added comment about the possibility that the
c  problem is over-parameterized. See \ALAN3\NEELY\IT2B\IT2BPROB.EXP 
c  to see that if over-parameterization occurs, AND gamma is being 
c  estimated, then the est. of gamma --> 0, which causes numerical 
c  instability eventually. Note though that the write statements to the
c  user include the over-parameterization comment even if gamma is not
c  being estimated ... because it can't hurt to check for this in 
c  general.

c-----------------------------------------------------------------------

c  it2beng27.f                                             3/28/15

c  it2beng27 has the following changes from it2beng26:

c  1. All numbers written out in F or G format are now tested to see if
c  they are inside [-1.D-99, 1.D-99]. If so, they are changed to be
c  0. The reason is that otherwise they will be printed out without 
c  the accompanying D or E (e.g., as .934-106, rather than .934E-106).

c  Note that a new Subroutine, VERIFYVAL is added to the code to 
c  do the indicated testing above.

c  Note that the modules linked to this main module are changed as
c  follows:
c  idm11x15.f to idm11x16.f;
c  idm2x15.f to idm2x15a.f;
c  idm222x14.f to idm222x15.f;
c  readi09.f to readi10.f;
c  shift9.f remains unchanged;

c  vodtot remains unchanged.

c  2. Formats 7126, 7124, and 7123 are changed to show that the 
c  output files are made by it2beng27 rather than it2beng26.

c-----------------------------------------------------------------------

c  it2beng26.f                                             9/5/14

c  it2beng26 has the following change to it2beng25:

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

c  2. Formats 7126, 7124, and 7123 are changed to show that the 
c  output files are made by it2beng26 rather than it2beng25.

c  3. The formula for BIC is changed to match the new formula put in
c  for AIC in it2beng25.f (i.e., it has an extra factor of 2.0).


c-----------------------------------------------------------------------

c  it2beng25.f                                             7/24/14

c  it2beng25 has the following changes to it2beng24:

C  1. THE ID MODULES LINKED WITH it2beng25.f WILL BE UPDATED:
C  idm11x14.f TO idm11x15.f; idm2x14.f TO idm2x15.f; and idm222x13.f
C  TO idm222x14.f.

c  2. THE AIC calculation is changed. Now, instead of 
c  AIC = -TRULOG + (P + Q), it will be 
c  AIC = 2*(-TRULOG + (P + Q)). 
c  So all AIC values will now be double their previous values.

c  3. Formats 7126, 7124, and 7123 are changed to show that the 
c  output files are made by it2beng25 rather than it2beng24. 

c-----------------------------------------------------------------------

c  it2beng24.f                                             4/9/14

c  it2beng24 has the following changes to it2beng23

C  1. THE MAXIMUM NO. OF OUTPUT EQUATIONS WILL BE CHANGED FROM 6 TO 
C  NUMEQT, WHICH IS SUPPLIED IN THE ARGUMENT LIST TO SUBROUTINE IT2B.
C  THIS MEANS THAT NUMEQT WILL NOW BE PASSED TO ALL THE SUBROUTINES
C  THAT NEED IT; AND IN THOSE SUBROUTINES, ANY 6 REFERRING TO THE MAX.
C  NO. OF OUTPUT EQUATIONS WILL BE CHANGED TO NUMEQT (OR MAXNUMEQ WHICH
C  WILL BE ESTABLISHED AS 7 IN A PARAMETER STATEMENT).

C  NOTE THAT THIS ALSO INCLUDES PASSING NUMEQT TO IDCALCY SO THAT YPRED
C  CAN BE VARIABLY DIMENSIONED IN THAT MODULE.

C  ALSO, IN THOSE ROUTINES WHERE ARRAYS ARE PASSED IN COMMON STATEMENTS,
C  OR EXIST ONLY IN THOSE ROUTINES, DIMENSIONS RELATED TO THE 
C  MAX. NO. OF OUTPUT EQS. WILL BE DIMENSIONED BY A PARAMTER STMT.
C  SETTING MAXNUMEQ = 7, THE CURRENT LIMIT (SINCE THESE ARRAYS CANNOT
C  BE VARIABLY DIMENSIONED BY A CALLING ARGUMENT). THIS INCLUDES IN 
C  MAIN, FILRED, AND OTHER ROUTINES IN THE ID MODULES WHERE YOO IS
C  PASSED IN COMMON/OBSER. IT ALSO INCLUDES OBSBLOCK IN SUBROUTINES
C  NEWWORK1 AND READOUT. AND IT INCLUDES SUBROUTINE OUTPUT IN THE NEW
C  TEMPLATE MODEL FILE, TSTMULTM.FOR (SEE CHANGE 2.).

C  THE NEW ID MODULES TO BE LINKED WITH THIS MAIN MODULE ARE
C  idm11x14.f, idm2x14.f, AND idm222x13.f.

C  ALSO THE MODULE, readio8.f, IS UPDATED TO BE readio9.f.

C  2. NOTE THAT THE TEMPLATE MODEL FILE FOR THIS PROGRAM HAS BEEN
C  CHANGED FROM TSTMULTL.FOR TO TSTMULTM.FOR (SEE COMMENT 1. ABOVE).

c  3. Formats 7126, 7124, and 7123 are changed to show that the 
c  output files are made by it2beng24 rather than it2beng23. 

c  Note that it2beng24.f is the main "engine" module for the new PC
c  prep program, IT2B110.FOR.

c-----------------------------------------------------------------------

c  it2beng23.f                                             7/21/13 

c  it2beng23 has the following changes to it2beng22:

c  1. If the program bombs, the message that is written to the screen 
c  will now also be written to the file ERRFIL = ERRORxxxx, where xxxx
c  is the 4-digit run no. In this way, if the program is being run using
c  Pmetrics, the Pmetrics program can respond appropriately. Note that
c  ERRFIL must be passed to all the routines which could write to it
c  using COMMON/ERR/ERRFIL ... except to routine GETIPATF, where it is
c  included as a calling argument (to be consistent with how the code
c  is written in npageng24.f).

C  2. THE CODE TO READ extnum TO GET THE 4-DIGIT JOB NUMBER IS MOVED TO
C  THE TOP OF THE CODE, SO ERRORxxxx CAN BE OPENED AND THEN FILLED 
C  AT EARLIER LOCATIONS IF THE PROGRAM STOPS ABNORMALLY. IN PARTICULAR,
C  ERRFIL IS ADDED TO THE ARG. LIST OF SUBROUTINE GETIPATF SO IT CAN BE
C  WRITTEN TO IF THERE IS AN ERROR IN THAT ROUTINE. ALSO, FORMATS
C  4706 (SEE BELOW) AND 1721 ARE NOW WRITTEN TO ERRFIL IF THERE IS AN
C  ERROR IN THOSE LOCATIONS.

C  NOTE THAT A NEW FORMAT, 4706, IS USED INSTEAD OF FREE FORMAT TO TELL
C  THE USER THAT it2b101.inp IS NOT AVAILABLE, AND THE PROGRAM IS
C  THEREFORE STOPPING.

C  3. THE OUTPUT FILE NOW INCLUDES AN EXTRA SPECIFICATION (YES OR NO)
C  AT THE END OF EACH RANDOM VARIABLE'S RANGE ... TO TELL WHETHER OR
C  NOT THAT VARIABLE MUST BE ESTIMATED TO BE .GE. 0 (I.E., IRAN(.)
C  INFO). WITHOUT THIS INFORMATION, TWO OUTPUT FILES COULD BE IDENTICAL
C  AT THE TOP, BUT HAVE VERY DIFFERENT RESULTS.

C  SIMILARLY, THE IT_RFxxxx.TXT FILE CREATED BY SUBROUTINE READOUT WILL
C  HAVE THIS SAME IRAN(.)-RELATED INFO, AND SO IRAN IS NOW ADDED TO THE
C  ARGUMENT LIST FOR THAT ROUTINE. AND NOTE THAT SUBROUTINE READOUT IS
C  NOW PART OF THE UPDATED readi08.f (UPDATED FROM readi07.f).

c  4. Formats 7126, 7124, and 7123 are changed to show that the 
c  output files are made by it2beng23 rather than it2beng22. 

c  Note that it2beng23.f is the main "engine" module for the new PC
c  prep program, IT2B109.FOR.

c-----------------------------------------------------------------------

c  it2beng22.f                                             10/30/12

c  it2beng22 has the following changes from it2beng21:

c  1. It will now be linked with readi07.f, rather than readi06.f. The 
c  reason is that the individual files needed by readi07.f will now be
c  left open when that module is called, and so the combined output
c  file, OUTFILE, will no longer be needed as a calling argument to
c  readi07.f (i.e., Subroutine READOUT). 

c  Note that not having Subroutine READOUT separate the combined output
c  file into the 4 needed individual files can save a lot of execution
c  time if this program is compiled and linked with gfortran.

c  2. It will now be linked with shift9.f, rather than shift7.f. The
c  reason is to fix bugs which occurred if a steady state dose had
c  bolus inputs (see details in shift8.f and shift9.f).

c  3. It will be linked with idm11x13.f (updated from idm11x12.f),
c  idm2x13.f (updated from idm2x11.f), and idm222x12.f (updated from
c  idm222x11.f). The new id modules fix bugs related to the R(.) array
c  being updated before each call to a routine that uses it, and also
c  to GETFA being called before the FA(.) array is used (see details
c  in the id modules).

c  4. Formats 7126, 7124, and 7123 are changed to show that the 
c  output files are made by it2beng22 rather than it2beng21. 

c  Note that it2beng22.f is the main "engine" module for the new PC
c  prep program, IT2B108.FOR.

c-----------------------------------------------------------------------

c  it2beng21.f                                             8/29/12

c  it2beng21 has the following changes from it2beng20:

C  1. NEW CODE IN SUBROUTINE NEWWORK1 IS USED TO ESTABLISH THE VALUES
C  FOR DOSEBLOCK, WITHOUT USING A BACKSPACE COMMAND. THE REASON IS THAT
C  DEPENDING ON WHICH COMPILER IS USED TO MAKE THE PR PREP PROGRAM
C  (CURRENT ONE IS IT2B107.FOR), IT IS POSSIBLE FOR A DOSE EVENT
C  TO LOOK LIKE SEVERAL LINES RATHER THAN ONE LONG WORD-WRAPPED LINE.
C  IN THE FORMER CASE, BACKSPACING ONE LINE WILL NOT BACKSPACE TO THE
C  BEGINNING OF THE DOSE EVENT AS SHOULD BE DONE. SO TO BE SAFE, THE
C  LOGIC TO USE BACKSPACE(23) WILL BE COMMENTED OUT, AND 
C  DOSEBLOCK(.,.,.) WILL BE ESTABLISHED DIRECTLY.

C  2. it2beng21.f WILL BE COMPILED AND LINKED WITH NEW id MODULES:
C  idm11x11.f REPLACED BY idm11x12.f;
C  idm2x10.f REPLACED BY idm2x11.f;
C  idm222x10.f REPLACED BY idm222x11.f;

C  THE REASON IS TO CORRECT A BUG WHEN N = 0 (ANALYTIC SOLUTIONS CODED
C  INTO SUBROUTINE OUTPUT). IN THIS CASE, THE NDO = ND, 
C  SIGO(.) = SIG(.), ETC. SECTION IS SKIPPED; AND THIS MEANS THAT AT THE
C  END OF idm11x11.f, ND = ND0 BECOMES 0 --> THE NEXT TIME idpc IS 
C  CALLED, THE PROGRAM IS SCREWED UP.

C  THIS BUG ONLY HAPPENS IF N = 0 AND NO TESTCASES WITH N = 0, WHICH
C  WOULD HAVE DISCOVERED THE PROBLEM, WERE RUN SINCE idm11x9.f WAS 
C  USED.

C  THE SOLUTION, IN THE NEW id ROUTINES, IS TO MOVE THE NDO = ND, 
C  SIG0(.) = SIG(.), ETC. ... CODE TO JUST AFTER CALL GETFA.

c  See \ALAN3\NEELY\EMAX\EMAX.EXP for details.

c  3. it2beng21.f will be compiled and linked with readi06.f, updated
c  from readi05.f (see comments in that module).

c  4. Formats 7126, 7124, and 7123 are changed to show that the 
c  output files are made by it2beng21 rather than it2beng20. 

c  Note that it2beng21.f is the main "engine" module for the new PC
c  prep program, IT2B107.FOR.

c-----------------------------------------------------------------------

c  it2beng20.f                                             7/8/12

c  it2beng20 has the following changes from it2beng19.f:


c  1. "Relative" rather than "real" times are written to IT_RFxxxx.TXT. 
c  This means that if a steady state dose set occurs, the dose and
c  output times in IT_RFxxxx.TXT files will now be based
c  on the end of the steady state dose set rather than the beginning. As
c  an example, if the interdose interval of a steady state dose set is
c  4 hours, and the last observation for that region is 20 hours, the 
c  previous program would have written into this file, the times 
c  400.00, 400.0333,..., 444.00 (i.e., 24 hours after the last 
c  observation time). Those are the "real" times that are used by the
c  id modules in this program. But now, the times written to these files
c  will be 0.00, 0.033,..., 44.00, which are the "relative" times (i.e.,
c  those starting at the end of the 400 hours of steady state dosing).
c  Note that the patient data files written to the end of
c  the OUTFxxxx file will still be the full versions which are seen by
c  the id modules, including the "real" times.

c  The module readi04.f linked to this program will be updated to be
c  readi05.f. readi05.f will no longer read DOSEBLOCK(.,.,.) and
c  OBSBLOCK(.,.,.) from file 27; instead this info will be passed via
c  COMMON/DOSEOBS from Subroutine NEWWORK1. These values will be the
c  values from the patient files in the it2b101.inp (i.e., before these
c  working copy format files are converted by SUBROUTINE NEWWORK1 to
c  have a full 100 dose lines for each steady state set, and to have
c  "real" rather than "relative" times in the dose/cov and observation
c  blocks).

c  All other modules linked with the main module, are unchanged.

c  2. Formats 7126, 7124, and 7123 are changed to show that the 
c  output files are made by it2beng20 rather than it2beng19. 

c-----------------------------------------------------------------------

c  it2beng19.f                                             4/18/12

c  it2beng19.f has the following changes from it2beng18:

c  1. It is the main engine module for IT2B105.FOR. The change is that 
c  this program allows steady state dose sets to have bolus doses as 
c  well as IVs (rather than being limited to just IVs). 

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

c  In addition, the 3 id modules require code changes, and are updated
c  to be idm11x10.f, idm2x10.f, and idm222x10.f.

c  2. Formats 7126, 7124, and 7123 are changed to show that the 
c  output files are made by it2beng19 rather than it2beng18. 

c-----------------------------------------------------------------------

c  it2beng18.f                                             2/10/12

c  it2beng18 has the following changes from it2beng17:

c  1. It is part of the IT2B104.FOR program, which now restricts 
c  variable values in Subroutine MAPBAYS to be positive if their
c  IRAN(.) value is 1. This means that the parameter estimates for
c  variables whose IRAN(.) value is 1 will be non-negative. In 
c  addition these variables will have their lower boundaries, which
c  are stored in the FROMxxxx file, be no lower than 1.D-8.

c  See changes in Subroutine MAPABYS and MAKEVEC, and where the 
c  boundaries AB(.,.) are calculated in Main.

c  In particular, the it2b101.inp instruction file format will not 
c  change, but now, IRAN(I) = 0 if parameter I is fixed; = -1 if
c  parameter I is a variable and may be negative; = 1 if parameter I is
c  a variable and may not be negative.

c  2. Formats 7126, 7124, and 7123 are changed to show that the 
c  output files are made by it2beng18 rather than it2beng17. 

c-----------------------------------------------------------------------

c  it2beng17.f                                             1/29/12

c  it2beng17 has the following changes from it2beng16:

c  1. It is the main engine module for the new PC prep program, 
c  IT2B103.FOR. The new IT2B103.FOR/it2beng17.f program now allows
c  patient files to have steady state dose sets. In particular, this
c  program now calls new subroutine NEWWORK1 (based on the stand-a-lone
c  program of the same name) to read each working copy file in 
c  it2b101.inp, which may have steady state dose indicator lines, and
c  convert it to the typical form that that the id routines require, 
c  except that the steady steady state dose indicators themselves
c  (negative dose times) remain in the file.

c  2. vodtot.f is not changed, but the other 5 permanent engine .f
c  files linked to this program, readi03.f, shift6.f, idm11x6.f, 
c  idm2x6.f, and idm222x6.f, will be updated to be readi04.f, shift7.f,
c  idm11x8.f, idm2x8.f, and idm222x8.f. The changes in the id routines
c  will allow them to read steady state dose indicators and know how to 
c  integrate through each set, testing for convergence of the steady
c  state compartment amounts to see if the program can fast forward to
c  the end of that set. The changes for readi04.f are simply in the
c  dimensions related to no. of doses, which will change from 500 to 
c  1000. The changes in shift7.f are the dimensions changes of 

c  readi04.f, and some edited code required since now a dose (time)
c  reset occurs when a dose time is .LE. 0, rather than .EQ. 0.


c  3. All 500's related to dose times dimensions (which are passed in
c  COMMON/OBSER) are changed to 5000's to be compatible with the new
c  id routines. This is because in those routines, because of steady
c  state dose sets, the no. of dose times can expand to be much 
c  bigger than 500. All 500's will also be changed to 5000 in 
c  Subroutine FILRED.

c  4. Formats 7126, 7124, and 7123 are changed to show that the 
c  output files are made by it2beng17 rather than it2beng16. 

c  5. This module is part of the IT2B103.FOR program. That program was
c  changed so that in the it2bdriv.f file the CALL BIGIT2B statement
c  is now replaced by a CALL IT2B statement. As a result, the SUBROUTINE 
c  BIGIT2B statement below is replaced by SUBROUTINE IT2B.

c-----------------------------------------------------------------------

c  it2beng16.f                                             7/15/11

c  it2beng16 has the following changes from it2beng15:

c  1. Extra information is now included in the output file, so more 
c  information can be written to the IT_RFxxxx.TXT by the new readi03.f
c  module, which is updated from readi02.f. Note that the other modules
c  linked to this program (idm11x6.f idm2x6.f idm222x6.f shift6.f and
c  vodtot.f) are unchanged.

c  The new information includes the ending cycle no., whether or not the
c  analysis converged (see formats 5197 - 5199), and whether GAMMA for 
c  each output eq. was fixed at 1.0 or estimated.

c  Also, since all ATOL(I) are set = RTOL, the comment in the output 
c  file is simplified to report just RTOL as the value of the tolerance
c  used in the differential equation solver (VODE). See FORMAT 9769.

c  2. "made by it2beng15.f" is replaced by "made by it2beng16.f" in
c  three format statements.

c  3. The file, it2bbig5.inp, prepared by the PC prep program (the
c  first one used by this program is IT2B101.FOR) will be changed to
c  be it2b101.inp. The reason is that now the active salt fraction,

c  AF, is changed to be AF(I),I=1,NDRUG (i.e., it is no longer assumed
c  that all drugs have the same salt fraction). Also, in addition to
c  PREFIX and EXT, the patient data info will also be provided in .csv 
c  format., so the name of this .csv file (CSVFILE) can be written into
c  FROMFIL.

c  4. Since AF(.) will now written to the output file (see Format 1229)
c  rather than just the scalar, AF, and the .csv file in addition to
c  PREFIX and EXT (see no. 3. above) below format 9861, the output file 
c  will have a new code. It will change from REM_FRN JUL_10 to 
c  REM_FRN JUL_11.

c-----------------------------------------------------------------------

c  it2beng15.f                                             4/26/11

c  it2beng15 has the following changes from itbig14:

c  1. This is the main module in the program whose PC Prep program is
c  the new IT2B100.FOR. As in that .FOR module, the formula for NI in 
c  Subroutine FILRED is changed from  NI = 2*NDRUG + 2 + NADD  to  
c  NI = 2*NDRUG + NADD, because from now on, WT and CCR will not be 
c  considered special covariates. If they are included in the working 
c  copy file, they will be part of the NADD 'additional' covariates 
c  (beyond the 4 permanent ones in Common DESCR).

c  Because of the above change, the shift5.f module linked to this
c  program will be updated to shift6.f.

c  2. This program will be linked with readi02.f, updated from readi01.f. 
c  The difference is that readi02.f will include the assay coefficients
c  for each observation in the rfile, and AIC and BIC will be put into
c  this file (for the final cycle). Also the rfile will be renamed to be
c  IT_RFxxxx.TXT from RFILExxxx.IT.

c  3. "made by itbig14.f" is replaced by "made by it2beng15.f" in
c  three format statements.

c  4. A subtle bug is corrected just before loop 7000. Before itbig7.f,
c  the calculation of TRULOG required a final cycle value of ESTINV to 
c  be calculated. But from itbig7.f, TRULOG was calculated differently
c  and the final cycle ESTINV was not computed. Therefore, the calls by
c  ELDERY to MAPBAYS in loop 7000 used the ESTINV from the next to last
c  cycle. This bug has now been corrected with new code just before
c  loop 7000 which establishes the final cycle ESTINV.

c----------------------------------------------------------------------

c  itbig14.f                                               3/2/11

c  itbig14 has the following changes to itbig13:

c  1. At the end of MAIN, new subroutine READOUT, which is the main

c  routine in new module readi01.f will create RFILExxxx.IT, an output
c  file which is easy to use with the program, r. readi01.f is now 
c  compiled  with this program. 

c  2. "made by itbig13.f" is replaced by "made by itbig14.f" in
c  three format statements.

c-----------------------------------------------------------------------

c  itbig13.f                                               12/30/10

c  itbig13 is the same, functionally, as itbig9z. The difference 

c  is that it is linked with idm11x6.f (updated from idm11x5.f),
c  idm2x6.f (updated from idm2x5.f), and idm222x6.f (updated from
c  idm222x5.f). The other permanent .f files, shift5.f, and vodtot.f
c  are unchanged.

c  2. "made by itbig9z.f" will be replaced by "made by itbig13.f" in
c  three format statements.

c  Note that the first PC Prep program to use the itbig13.f "engine"

c  is ITBIG11.FOR

c-----------------------------------------------------------------------

c  itbig9z.f                                               11/23/10

c  itbig9z has the following changes to itbig9y:

C  1. A CALL TO NEW SUBROUTINE PAUSE REPLACES EACH PAUSE STATEMENT. 
C  THIS IS BECAUSE A PAUSE STATEMENT CAUSES A WARNING WHEN THE PROGRAM
C  IS COMPILED AND LINKED USING gfortran (AND IT FORCES THE USER TO 
C  TYPE "go" INSTEAD OF SIMPLY HITTING THE ENTER KEY.

c  2. "made by itbig9y.f" will be replaced by "made by itbig9z.f" in
c  three format statements.

c  Note that the first PC Prep program to use the itbig9z.f "engine"
c  is ITBIG10.FOR.

c-----------------------------------------------------------------------

c  itbig9y.f - revised                                        11/17/10

c  itbig9y.f has another slight change from the original itbig9y.f:

c  The ranges found by this program, to be used as boundaries for a
c  subsequent NPAG run, are no longer constrained to have positive
c  values (since parameters which can be negative will have boundaries 
c  which can be negative).


c-----------------------------------------------------------------------

c  itbig9y.f                                                  11/16/10

c  itbig9y has the following changes to itbig9x:

c  1. The warnings about small parameter values is removed since
c  parameters which are logs can be negative (i.e., the standard
c  parameters from menu 1 and menu 2 in the Little Population programs
c  can never be less than 0, and values close to 0 could cause numerical
c  issues, but there is no such restriction for a parameter such as a 
c  log of another parameter).

c  2. In Subroutine MAPBAYS, the code which returns a large 

c  (unattractive) functional value if an entry in VEC is .LE. 0 is 
c  removed for the same reason as in 1. above.


c  3. One of the modules of the Big IT2B "engine" (itbig9y.f is the 
c  main module), idm22x5.f, is updated to idm222x5.f. The difference is
c  that PMAT is now dimensioned (594,30) in this new module, which is
c  consistent with this main module, rather than (594,20). This small
c  bug apparently never caused a problem, but it could.

c  4. "made by itbig9x.f" will be replaced by "made by itbig9y.f" in
c  three format statements.

c-----------------------------------------------------------------------

c  itbig9x.f                                                    8/9/10

c  itbig9x has the following changes from itbig991.f:

c  1. It is the main "engine" module for the ITBIG9.FOR program. ITBIG9
c  is at the same level as the Big NPAG program, NPBG15E1.FOR. i.e., it 
c  can now be used for multiple drugs.

c  2. This program will be linked with a new set of modules as follows:

c  idfxd51f.f is changed to idm11x5.f, which is the same as idm1x5.f of
c  the bigmlt6.f "engine" (for Big NPAG), except that rather than 
c  returning the total sum of squares over all the M x NOS observed 
c  values (SUMSQ), it returns SUMSQJ(J), J=1,NOS, where
c  each SUMSQ(J) = is the sum of squares just for output equation J.
c  The individual SUMSQJ(J) are needed in this program - see logic
c  regarding SSND(.).

c  idcy_53f.f is changed to idm2x5.f, the same module used in the
c  bigmlt6.f "engine" (for Big NPAG).

c  idcp_3f.f is changed to idm22x5.f, which is the same as idm2x5.f of
c  the bigmlt6.f "engine" (for Big NPAG), except it has the same
c  differences from idm2x5.f that idcp_3f.f had from idcy_53f.f (see 
c  code at the top of idm22x5.f for the details).

c  shift2.f is changed to shift5.f, the same modules used in the 
c  bigmlt6.f "engine" (or Big NPAG).

c  vodtot.f is unchanged.

c  3. The template model file will now be TSTMULTG.FOR, just as it is
c  for the Big NPAG program. The id modules above are compatible with
c  TSTMULTG.FOR.

c  4. A new instruction file will be needed. The new name will be
c  it2bbig5.inp, which will be different from it2bbig3.inp in that
c  new info (NSUBTOT, IPATVEC) will be included, and old info 
c  (PRFIX2,EXT2) will be eliminated.

c  5. Changes are made to FROMFIL which is written to be read by the Big
c  NPAG program in the format that program expects.

c  6. Many changes are made to write onto scratch file 27 the active
c  NSUB out of the total of NSUBTOT patients available - this code is 
c  the same as that used in the Big NPAG "engine", bigmlt6.f.

c  7. Subroutine FILRED is redone - it's the same version as in the 
c  Big NPAG "engine", bigmlt6.f (so it can read the multiple drug
c  working copy patient files).

c  8. READLARG, which is now *400 is changed to be *1000 as it is in
c  bigmlt6.f. This will ensure that no line is cutoff as it is being
c  copied into the combined output file. This also means that Format
c  2717 is changed from A400 to A1000. Similarly, READLINE is changed
c  from *72 to *300, and the corresponding formats in all routines
c  are changed from A72 to A300.

c  9. At the end of the run, new Subroutine CONDENSE (based on the
c  free standing program, CONDENSE.FOR) is used when writing lines
c  to the combined output file, OUTFxxxx. This makes OUTFxxxx a much
c  smaller file - by only using line sizes which are required for 
c  each line, rather than always using A1000 as the the format 
c  (see comment no. 8 above regarding READLARG).

c 10. ALL 25's in DIMENSION statements which are related to NVAR are
c  changed to 30'S (in all routines). Similarly, all 25(A11... formats 
c  are changed to 30(A11 formats. In addition all 12's related to the 
c  max. no. of fixed variables are changed to 20's.

c  11. The new output code will be REM_FRN JUL_10; it will be put into
C  FORMATS 7124 and 7126. And, in these formats, as well as in 7123,  
C  "made by itbig991.f" will be replaced by "made by itbig9x.f".


c  12. The terms, "NPEM" and "NPEM2" will be replaced by "NPAG"
c  throughout the program. This should have been done when Big NPEM
C  turned into BiG NPAG some time ago.

c  13. PAUSE commands are put below every STOP command so if this program
c  is called by a Windows GUI, the user will be able to see why the
c  program stopped before the Window closes.

c  14. Format 9013 is changed to indicate that the program will continue
c  even though a parameter estimate is very small. The format had
c  erroneously indicated that the program would stop in that case. A
c  WARNING to the user is still written to the output file and the 
c  screen.

c  15. Format 4324 will be changed so that HOWCLOSE will be output in
c  G format rather than F format.

c  16. New subroutines, GETIPATF, GETNUMSF, GETSUB, and WRITEPT2 are 
c  added. They are all the same as those in bigmlt6.f, the Big NPAG 
c  "engine", except that in WRITEPT2, writing to file 29 is replaced
c  by writing to file 24. 

c-----------------------------------------------------------------------

c  itbig991.f							8/28/08

c  itbig991.f is the same as itbig99.f except in the making of the 
c  combined output file. In itbig991, the combined output file is made 
c  using READLARG (and format 2717) instead of using REALINE (and 
c  format 1717). READLARG is a character 400 variable, where READLINE
c  is a character 72 variable. The extra characters ensures that 
c  no lines will be cut off.

c  Also, itbig9.f was replaced by itbig991.f in the 3 formats,
c  7124, 7126, and 7123 (this was overlooked in the itbig99.f
c  program).

c-----------------------------------------------------------------------


c  itbig99.f					              7/29/07

c  itbig99 is the same as itbig9 except that the no. of random variables
c  is now .LE. 25 rather than .LE. 20. Also, the no. of fixed parameters
c  is now .LE. 7 rather than .LE. 12.

c  The PC prep programs for this "engine" is ITBIG7Y.FOR.


c-----------------------------------------------------------------------



c  itbig9.f							3-10-05

c  itbig9 has the following changes to itbig8x:


c  1. All information needed by the Big PC Prep Program (currently 
c  ITBIG8.FOR) will now be put into one combined output file. That is,
c  this program will essentially concatenate 4 files which were 

c  previously kept separate, OUTFIL, PARFIL, it2bbig3.inp (actually 
c  just the patient data portion of it2bbig3.inp), and it2bdriv.f. This 
c  will enable the user to run the PC Prep Program with just this one
c  combined output file (i.e., even the working copy patient data files
c  will no longer be needed).

c  THE name for the OUTPUT FILE is now changed to 'OUFF'//NAME SINCE 
c  'OUTF'//NAME will be reserved for the combined OUTPUT FILE formed at 
c  the end of the run.

c  2. "made by itbig8x" is changed to "made by itbig9" in the 1st lines
c  in OUTFIL, PARFIL, and FROMFIL. See formats 7124, 7126, and 7123.

c-----------------------------------------------------------------------

C  For comments on itbig8x.f (10-10-02) - m1_1calc.f (10-3-97),
C  see itbig991.f program.

C-----------------------------------------------------------------------

C************************************

C  THE ITS ALGORITHM IS AS FOLLOWS (See its1comp.m notes for details):

C General Model:

C Y(i) = hi(theta_i) + e(i); e(i) -- iid -- N(0,Ri).

C Y(i), i=1,nsub, are observed --> obtain estimates theta_i_hat
C theta_i_hat -- iid -- N(theta_i, Vi) <-- cond dist., given theta_i.
C theta_i -- iid -- N(mu,cov)
C theta_i_hat -- iid -- N(mu, cov+Vi)  <-- unconditional dist.
C -- SEE DERIVATION IN HOME NOTES, 8-19-98.

C Goal is to estimate mu and cov.


C Its Technique:
 
C Do iter = 1, niter 

C Given the current estimates of (mu,cov) = (muk,covk):

C Do i = 1, nsub 

C let yi = data vector;

C Find theta_i, called xik, to minimize (using ELDERY):

C -ln P(yi|xik) - ln P(xik|muk,covk), where 
C  ln P(yi|xik) = C1 - .5*ln det(Ri) - 
C  .5*(yi - hi(xik))*inv(Ri)*(yi - hi(xik))', 
C  hi(xik) = E(yi|xik), and
C     	Ri = nobs x nobs diagonal matrix with sig(l)^2 in element
C     (l,l), where sig(l) = cubic function of yi(l)  (see assay 
C     coefficients, C0,...,C3 below) <--- see model above.
C     AS OF MXEM1S40.FOR, THE ASSAY NOISE POLYNOMIAL IS MULTIPLIED BY
C     THE FACTOR GAMMA (WHICH IS CONSTANT OVER THE ENTIRE POPULATION).
C     THE UPDATED ESTIMATE OF GAMMA IS FOUND AT THE END OF EACH CYCLE

C     (IF REQUESTED BY THE USER; OTHERWISE, IT REMAINS FIXED AT 1.0).

C  ln P(xik|muk,covk) = C2 - .5*ln det(covk) -
C	   .5*(xik - muk)*inv(covk)*(xik - muk)',
C     muk is the current estimate of the population mean, and

C     covk is the current estimate of the population covariance
C     matrix (see above).

C Note that xik is the 'MAP BAYESIAN' estimate of theta_i.


C For each xik, Find vi = inv(Pi'*inv(Ri)*Pi), where

C Ri is defined above, and
C Pi is the nobs x nvar matrix with Pi(l,m) = partial derivative of
C yi(tim(l)) w.r.t. parameter m, assuming the parameters = their
C values in xik. 

C Calculate Pik = inv(inv(vi) + inv(covk))

C End of Do i = 1, nsub loop

C Update: 
C  muk = sum(xik)/nsub 
C  covk = sum[Pik + (xik-muk)*(xik-muk)']/nsub <--- use updated muk.
C         ... both of the above sums over all nsub subjects.

C End of Do iter = 1, niter loop.


C************************************

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


C  NOTE:
C  EVEN THOUGH MAXGRD IS 100*80021 IN "BIG" NPEM2 (m2_17cal.f IS THE
C  CURRENT PROGRAM), MAXGRD PASSED ABOVE TO THIS PROGRAM (FROM

C  SUBROUTINE MAKEDRIV M1_4.FOR (CURRENT PREPARATION PROGRAM) HAS A 
C  MAXIMUM VALUE OF 10*80021. THIS SEEMS BIG ENOUGH FOR NOW. NOTE THAT
C  WITH NVAR = 10 --> COULD STILL GET ALMOST 4 GRID POINTS TO A 
C  DIMENSION (I.E., 4**10 = 1,048,576 WHICH IS

C  JUST A BIT MORE THAN 10*80021), AND THE INTEGRATION APPROACH OF THIS 
C  PROGRAM ASSIGNS POINTS IN A MORE EFFICIENT WAY THAN JUST IN A 
C  RECTANGULAR GRID.

C  ALSO, THE VALUE OF TRULOG BELOW, WHICH DEPENDS ON NGRID SELECTED, IS
C  JUST AN ESTIMATE OF THE VALUE WHICH WILL BE OBTAINED BY 'BIG NPEM' 
C  AND SO IT IS NOT NECESSARY TO USE AS MANY GRID POINTS AS DOES NPEM.

C  NOTE: AS OF itbig7.f, THE ABOVE NOTE IS MOOT SINCE THE TRULOG 
C        CALCULATION WILL NO LONGER BE BASED ON A "GRID" AS NPEM/NPAG
C        DOES IT; INSTEAD THE TRULOG WILL BE CALCULATED AS IF THE
C        "GRID" WAS JUST COMPOSED OF THE NSUB FINAL CYCLE PARAMETER
C        ESTIMATES (ONE FOR EACH SUBJECT); SEE CODE BELOW).

C  NOTE THAT ALL THE DIMENSIONS = 25 BELOW 'SHOULD' BE CHANGED TO 
C  MAXDIM, BUT SINCE THESE ARRAYS ARE SO SMALL, CHANGING THEM TO 
C  VARIABLY DIMENSIONED ARRAYS (WHICH REQUIRE PASSING THE ARRAYS AND
C  MAXDIM THROUGH ALL RELATED CALLING STATEMENTS) IS NOT WORTH IT.
C  SIMILARLY FOR PX(32), SINCE 32 = 25 (MAXDIM) + 7 (MAX. NO. OF
C  FIXED PARAMETERS).

C  AS OF m1_4calc.f:
C  NOTE THAT ALL DIMENSIONS = 150 HAVE BEEN CHANGED TO 594, SINCE THIS
C  NO. REPRESENTS THE TOTAL NO. OF OBSERVATIONS (AND THE MAX. NO IS
C  6 OUTPUT EQUATIONS x 99 OBSERVATIONS/EQ). THIS COULD BE CHANGED
C  TO NUMEQT*MAXOBS, BUT IT WOULD BE MORE TROUBLE THAN IT'S WORTH TO
C  MAKE THESE DIMENSIONS VARIABLE.


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
     1 ESTMENO(32)

C  NOTE THAT THE 2ND DIMENSION OF SIG IS MAXNUMEQ, RATHER THAN
C  NUMEQT. THE REASON IS THAT THIS ARRAY IS PASSED IN COMMON AND
C  THEREFORE CAN ONLY BE VARIABLY DIMENSIONED BY A VALUE SET IN
C  A PARAMETER STATEMENT. SIMILARLY FOR SSND.

      CHARACTER PREFIX*5,PAR(30)*11,EXT*3,CSVFILE*20,
     2 PARFIX(20)*11,OUTFIL*20,PARFIL*20,FROMFIL*20,NAME*4,READLINE*300,
     3 DENFIL*20,OUTCOM*20,READLARG*1000,OUTFILER*20,ERRFIL*20,ANS*3,
     4 PARRANFIX(20)*11


        COMMON SIG 
C  THE BLANK COMMON ABOVE IS SUPPLIED TO SUBROUTINE IDPC.
   
        COMMON/TOUSER/NDIM,MF,RTOL,ATOL
C  COMMON/TOUSER IS SUPPLIED TO SUBROUTINE USERANAL.


        COMMON/TOMAP/IRAN,VALFIX,SSND,SIGFAC,OFAC,ESTMEN,ESTINV,
     1   DET,NOFIX,NUMEQTT,RANFIXEST,NRANFIX
C  COMMON/TOMAP/ IS SUPPLIED TO SUBROUTINE MAPBAYS, WHICH IS CALLED
C  BY ELDERY.



        COMMON/ERR/ERRFIL 
C  COMMON/ERR/ IS SUPPLIED TO ALL THE ROUTINES WHICH COULD WRITE TO
C   ERRFIL.


      COMMON/TOCALC/IIRAN,PX,NNOFIX,NSUB
C  COMMON/TOCALC IS PROVIDED TO SUBROUTINE CALCRF, WHICH IS CALLED
C  BY SUBROUTINE ELDERY2.

C  NOTE THAT IIRAN(.) IS SET = IRAN(.), AND NNOFIX IS SET = NOFIX BELOW.
C  IRAN(.) AND NOFIX ARE USED IN COMMON/MAPBAYS ABOVE.

C wmy20190514 ----------------------------------------------------------

        COMMON/CNST/N,ND,NI,NUP,NUIC,NP

        integer JSUB, IG
        double precision AVGLOG,SUMLOG,VALMIN
        integer, dimension(128) :: INTLIST
        double precision, dimension(257) :: RPAR
        integer, dimension(257) :: IPAR
C ----------------------------------------------------------------------

        EXTERNAL MAPBAYS
        EXTERNAL CALCRF


        NUMEQTT = NUMEQT

C  NOTE ABOVE THAT NUMEQTT MUST BE DIFFERENT THAN NUMEQT BECAUSE ONE IS
C  A DUMMY ARGUMENT PASSED TO THIS ROUTINE, AND THE OTHER IS PASSED
C  IN A COMMON STATEMENT.


    2 FORMAT(A20)
  222 FORMAT(A3)
 2222 FORMAT(A5)


C-----------------------------------------------------------------------

C  INPUT FILE it2b102.inp FROM THE PREPARATION PHASE OF THIS PROGRAM.
C  THIS FILE CONTAINS THE USER DESIRED PARAMETER VALUES, ALONG WITH THE
C  CONCATENATED PATIENT DATA FILES (IN ADAPT FORMAT).



CCCCCCCCCCCCCCCCCCCCCC  INPUT INFO  (BELOW) CCCCCCCCCCCCCCCCCCCCCCCCC

C  INPUT THE FOLLOWING DATA FROM FILE it2b102.inp. 

C  NDIM = NO. OF COMPARTMENTS IN THE PHARMACOKINETIC MODEL.

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

C  PREFIX AND EXT FOR THE WORKING COPY FILES FOR THIS RUN.
C  CSVFILE = NAME OF THE .CSV FILE WHICH HAS THE PATIENT INFO IN IT.

C  NVAR = NO. OF RANDOM VARIABLES FOR THE ANALYSIS.
C  PAR(I),I=1,NVAR = NAMES OF R.V.'S.

C  NOFIX = NO. OF FIXED VARIABLES.
C  PARFIX(I),I=1,NOFIX = NAMES OF FIXED VARIABLES.

C  NRANFIX = NO. OF UNKNOWN PARAMETERS WHICH ARE THE SAME FOR 
C            ALL SUBJECTS FOR THE RUN.
C  PARRANFIX(I) = NAME OF UNKNOWN PARAMETER I; I=1,NRANFIX.


C  IRAN(I) = 1 IF PARAMATER I IS RANDOM AND REQUIRED TO BE POSITIVE;
C           -1 IF PARAMETER I IS RANDOM AND MAY BE NEGATIVE;
C	       0 IF PARAMETER I IS FIXED; 
C            2 IF PARAMETER I IS UNKNOWN BUT THE SAME FOR ALL
C                 SUBJECTS; I = 1,NVAR+NOFIX+NRANFIX.


C  NSUBTOT = TOTAL NO. OF SUBJECTS IN THE PATIENT POPULATION. 
C  NSUB = NO. OF SUBJECTS WHOSE DATA ARE TO USED FOR THE ANALYSIS (MAX
C         NO. = 999).
C  IPATVEC(I),I=1,NSUB = INDICES OF THE SUBJECTS WHICH ARE TO BE USED
C                        IN THIS ANALYSIS.

C  IF NOFIX > 0, VALFIX(I) = VALUE OF FIXED PARAMETER I, I=1,NOFIX.
C  IF NRANFIX > 0, RANFIXEST(I) = INITIAL ESTIMATE FOR RANFIX PARAMETER
C   I, I = 1,NRANFIX.

C  AB(I,1) = LOWEST VALUE FOR VARIABLE I ON ITS INPUT RANGE, I=1,NVAR.
C  AB(I,2) = HIGHEST VALUE FOR VARIABLE I ON ITS INPUT RANGE, I=1,NVAR.

C  NOTE THAT NUMEQT IS INPUT TO THIS ROUTINE, IT2B, BY THE CALLING
C  PROGRAM, it2bdriv.f.

C  NUMEQT = NO. OF OUTPUT EQS. IN THE MODEL FILE.

C  FOR IEQ = 1,NUMEQT:

C  IGAMMA(IEQ) = 1 IF GAMMA(IEQ) IS TO BE FIXED = 1.0 FOR THIS ANALYSIS.
C              = 0 IF GAMMA(IEQ) IS TO BE ESTIMATED IN THIS ANALYSIS.
C		 NOTE THAT NO IGAMMA(IEQ) CAN BE = 2 IN THIS PROGRAM
C		 (I.E., IF ANY IGAMMA(IEQ) = 2 --> PROGRAM assdriv.exe
C		 WOULD HAVE BEEN CALLED).

C  C0P(IEQ),C1P(IEQ),C2P(IEQ),C3P(IEQ) = THE COEFFICIENTS FOR THE ASSAY
C     STANDARD DEVIATION OF THE OBSERVED VALUES (DEFAULT POPULATION 
C     VALUES) -- INCLUDED ONLY SO THEY CAN BE OUTPUT IN OUTPUT FILE).
C     EACH SUBJECT'S INDIVIDUAL C'S ARE INCLUDED IN THE CONCATENATED 
C     PATIENT DATA FILES PORTION OF it2bbig3.inp.

C  XSIG, WHERE THE INITIAL POPULATION STD. DEV. ESTIMATE FOR EACH 
C        PARAMETER WILL BE XSIG*(AB(I,2) - AB(I,1)). 

C  QVAL = THE NO. OF NOISE PARAMETERS WHICH HAVE BEEN OR ARE TO BE
C	  ESTIMATED FOR THIS ANALYSIS, OVER ALL NUMEQT OUTPUT EQUATIONS
C	  (SEE COMMENTS BELOW)


C  NDRUG = NO. OF DRUGS.
C  AF(I), I=1,NDRUG = ACTIVE (SALT) FRACTION OF DRUG I.

C  TOL = TOLERANCE WHICH DETERMINES WHEN THE "ITS" ALGORITHM STOPS.
C	 IN PARTICULAR, IF THE AVERAGE LOG-LIKIHOOD OF THE PARAMETER
C	 VECTOR ESTIMATES, GIVEN THE DATA AND THE CURRENT ESTIMATES FOR
C	 (MU, COV), INCREASES BY LESS THAN TOL FROM THE PREVIOUS 
C	 ITERATION, THE PROGRAM STOPS.

C  MAXIT = MAXIMUM NO. OF ITERATIONS (STOPS THE PROGRAM IF TOL ABOVE
C	   IS NEVER REACHED). 

C  XDEV, WHERE THE PARAMETER BOUNDARIES WHICH WILL BE SUPPLIED TO THE
C  2ND PART (MXEM2N__) OF THIS TWO-PART BATCH PROGRAM ARE, FOR EACH 
C  VARIABLE, [a, M+XDEV*S], WHERE M AND S ARE THE FINAL POPULATION MEAN 
C  AND STD DEV FOR THAT VARIABLE, RESPECTIVELY, AND 

C  a = max(aa, M - XDEV*S), WHERE aa = 0, 1.D-8, 1.D-3, OR 1.D-1 
C  DEPENDING ON THE PARAMETER INVOLVED. THE ONE EXCEPTION TO THE ABOVE 
C  IS THAT, IF FA IS A VARIABLE, ITS UPPER BOUNDARY IS .LE. 1.0.


C  TOLCS ... USED FOR THE MAINFRAME ASSAY CALCULATION PROGRAM; NOT 
C	     NEEDED FOR THIS PROGRAM.

C  INDPTS = INDEX OF THE NO. OF GRID POINTS TO BE USED AT THE END OF THE
C           RUN TO CALCULATE THE TRUE (NUMERICAL) LOG-LIK. NO LONGER 
C           USED.

C  ILOG = 0 IF THE TRUE (NUMERICAL) LOG-LIK CALCULATION IS TO BE DONE
C	      AT THE END OF THE RUN;
C       = 1 IF THIS CALCULATION IS NOT TO BE DONE. 
C  ILOG IS NOW HARDCODED = 0 IN THE PC PREP PROGRAM NOW.

CCCCCCCCCCCCCCCCCCCCCCCC  INPUT INFO (ABOVE) CCCCCCCCCCCCCCCCCCCC

 2227 FORMAT(A11)


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
	IF(JNUM .EQ. 10000) JNUM=1
	BACKSPACE(25)
	WRITE(25,*) JNUM
	CLOSE(25)


C  CREATE OUTPUT FILE WHICH HAS 'OUTF' AS ITS 1ST 4 CHARACTERS AND
C  NAME AS ITS LAST 4. SIMILARLY, CREATE PARFIL, FROMFIL, DENFIL, AND
C  ERRFIL.


C  AS OF itbig9.f, THE NAME FOR THE OUTPUT FILE IS CHANGED TO
C  'OUFF'//NAME SINCE 'OUTF//NAME WILL BE RESERVED FOR THE COMBINED
C  OUTPUT FILE FORMED AT THE END OF THE RUN.

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

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,4706) 
        CLOSE(42)

        CALL PAUSE
        STOP

 4710	  READ(23,*) NDIM

C wmy20190521 -- /CNST/ N has to be initialized prior to CALL IDPC
        N = NDIM

C        write (*,*) "NDIM=",N,NDIM

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

C  SET NNOFIX = NOFIX. ONE IS USED IN COMMON/MAPBAYS, AND ONE IN
C  COMMON/TOCALC.

       NNOFIX = NOFIX

        READ(23,*) NRANFIX
        READ(23,2227) (PARRANFIX(I),I=1,NRANFIX)

        READ(23,*) (IRAN(I),I=1,NVAR+NOFIX+NRANFIX)

C  SET IIRAN(.) = IRAN(.). ONE IS USED IN COMMON/MAPBAYS, AND ONE IN
C  COMMON/TOCALC.

        DO I = 1,NVAR+NOFIX+NRANFIX
         IIRAN(I) = IRAN(I)
        END DO


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

C  NOTE THAT THE NEXT LINE HAS NUMEQT ON IT, BUT IT IS NOT NEEDED SINCE
C  NUMEQT IS SUPPLIED IN ARGUMENT LIST TO THIS ROUTINE, IT2B FROM 
C  it2bdriv.f. SO READ IN NOTHING FROM NEXT LINE. SIMILARLY, TOLCS IS
C  NOT READ IN BETWEEN XDEV AND INDPTS.

        READ(23,*) 

        DO I=1,NUMEQT
         READ(23,*) IGAMMA(I),C0P(I),C1P(I),C2P(I),C3P(I)
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

c  The patient data info is read in from it2b102.inp, and is put onto 
c  scratch file 27 (because it will need to be reread each cycle --> 
c  file 27 will be rewound each cycle).

c  Note that there are NSUBTOT subjects, but only NSUB of them,
c  with indices IPATVEC(I),I=1,NSUB, will be put onto file 27.
        OPEN(27)


 1717   FORMAT(A300)

        NLAFIR = 0

        DO JSUB=1,NSUB
          IG=JSUB

C  THE NEXT SUBJECT WHOSE DATA SET IS TO BE PUT TO FILE 27 IS SUBJECT 
C  IPATVEC(JSUB). SO FAR, NLAFIR IS THE NO. OF LINES WHICH HAVE BEEN
C  READ WHICH HAVE 'LAST AND FIRST' AS CHARACTERS 3:16 (THIS LINE IS
C  THE FIRST LINE OF EACH SUBJECT'S DATA SET). READ UNTIL THE NEXT
C  SUCH LINE.

 1720   READ(23,1717,IOSTAT=IEND) READLINE

        IF(IEND .LT. 0) THEN

C  NOTE THAT IEND .LT. 0 --> END OF FILE REACHED, BUT IF IT'S REACHED
C  AT THIS POINT, NOT ALL "ACTIVE" NSUB SUBJECT DATA SETS WERE READ
C  AND WRITTEN CORRECTLY TO FILE 27. IN THIS CASE, WRITE A MESSAGE TO
C  THE USER AND STOP.

	  WRITE(*,1721)
 1721   FORMAT(/' PATIENT DATA INFORMATION WAS NOT READ CORRECTLY'/
     1' FROM THE INSTRUCTION FILE, it2b102.inp. IF YOU EDITED THIS'/
     2' FILE MANUALLY, PLEASE RERUN THE PC PREP PROGRAM TO HAVE IT'/
     3' PREPARE it2b102.inp AGAIN AND THEN RERUN THIS PROGRAM.'//
     4' IF YOU DID NOT MANUALLY EDIT it2b102.inp, PLEASE SEND THE'/
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

       CALL NEWWORK1(JSUB)

        END DO

C  THE ABOVE END DO CLOSES THE  DO JSUB = 1,NSUB  LOOP.

 
 1730   REWIND(27)
        CLOSE(23)

C  NOTE THAT IF LABEL 1730 WAS REACHED VIA THE
C  IF(IEND .LT. 0) GO TO 1730    STATEMENT ABOVE, IT MUST BE BECAUSE
C  THE END OF THE FILE WAS REACHED AND THE LAST SUBJECT ON THE FILE
C  23 WAS ALSO THE LAST ONE (NO. IPATVEC(NSUB)) TO BE ANALYZED. THIS
C  MEANS JSUB SHOULD BE NSUB. IF, HOWEVER, JSUB .LT. NSUB, IT MEANS
C  THAT, SOMEHOW, NOT ALL NSUB SUBJECTS TO BE ANALYZED WERE ON THE
C  it2b102.inp FILE. IN THIS CASE, WRITE A MESSAGE TO THE USER AND
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



C  OPEN THE OUTPUT FILE -- ALL OUTPUT FROM THE PROGRAM WILL BE PUT
C			   INTO THIS FILE.

	OPEN(25,FILE=OUTFIL)



C  OPEN FROMFIL AND PUT '-1' ON THE TOP LINE (1ST ENTRY). IF
C  THIS PROGRAM TERMINATES BEFORE THE END OF THE 1ST ITERATION, THIS -1 
C  WILL TELL THE 'BIG NPAG' PREPARATION PROGRAM THAT THIS RUN BOMBED. 
C  IF THIS PROGRAM TERMINATES AFTER RUNNING AT LEAST 1 ITERATION, 
C  THE '-1' WILL BE REPLACED WITH A '1', AND THE 'BIG NPAG' PREPARATION
C  PROGRAM WILL BE ABLE TO USE THIS PROGRAM'S RESULTS.

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


C  BOTH THE LAST CYCLE PARAMETER FILE AND THE OUTPUT FILE WILL HAVE THE
C  SAME VERSION CODE WRITTEN TO THEIR RESPECTIVE LINE 1'S. BUT THE
C  OUTPUT FILE WILL HAVE THE CODE STARTING AT COLUMN 3, WHEREAS THE
C  LAST CYCLE PARAMETER FILE WILL HAVE THE CODE STARTING AT COLUMN 2.
C  THESE CODES WILL BE READ BY THE PC PREPARATION/EXAMINATION PROGRAM
C  (THE 1ST IN THIS SERIES IS M1_1.FOR) TO ENSURE THE USER ENTERS THE
C  RIGHT FILENAMES.

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

C  CALL SUBROUTINE WRITEPT2 TO WRITE THE PATIENT NOS. TO BE USED IN 
C  THE ANALYSIS TO FILE 25 ... IN AN "EFFICIENT" WAY, AS OPPOSED TO
C  ONE INDEX PER LINE. NOTE THAT THE FIRST ARGUMENT TELLS WRITEPT2
C  TO WRITE TO FILE 25. NOTE THAT THE '    0' AFTER THE CALL TO
C  WRITEPT2 TELLS THE PROGRAM READING THIS FILE THAT THE PATIENT
C  NOS. HAVE ENDED.

        CALL WRITEPT2(25,NSUB,IPATVEC)
        WRITE(25,*) '    0'

        WRITE(25,1012) NVAR
 1012   FORMAT(/' THE NO. OF RANDOM VARIABLES IS ',I2)

      WRITE(25,2239)
 2239 FORMAT(' THE RANDOM VARIABLES AND THEIR BOUNDARIES, AND IF THE EST
     1. MUST BE .GE. 0')

C  REPLACE WRITING OF AB() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL. SIMIARLY FOR ALL CALLS TO VERIFYVAL.
	
C  NORAN IN THE LOOP BELOW IS THE RUNNING INDEX (OUT OF THE NP TOTAL
C  PARAMETERS) OF THE NEXT PARAMETER WHICH IS RANDOM.


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
C      WRITE(25,2217) PAR(NORAN),AB(NORAN,1),AB(NORAN,2),ANS       
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

C  REPLACE WRITING OF VALFIX() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL. 

	DO I=1,NOFIX
       XVERIFY(1) = VALFIX(I)
       CALL VERIFYVAL(1,XVERIFY)
C      WRITE(25,2219) PARFIX(I),VALFIX(I)
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

C  REPLACE WRITING OF RANFIXEST() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL.
 
      DO I=1,NRANFIX
       XVERIFY(1) = RANFIXEST(I)
       CALL VERIFYVAL(1,XVERIFY)
C      WRITE(25,2219) PARRANFIX(I),RANFIXEST(I)
       WRITE(25,2219) PARRANFIX(I),XVERIFY(1)
      END DO
	WRITE(25,*)
 
  	ENDIF



C  REPLACE WRITING OF XSIG WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL. 

       XVERIFY(1) = XSIG
       CALL VERIFYVAL(1,XVERIFY)
C	 WRITE(25,1223) XSIG
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

C  REPLACE WRITING OF C0P(),...,C3P() WITH XVERIFY (SEE LOGIC IN 
C  SUBROUTINE VERIFYVAL.

	DO IEQ = 1,NUMEQT
       XVERIFY(1) = C0P(IEQ)
       XVERIFY(2) = C1P(IEQ)
       XVERIFY(3) = C2P(IEQ)
       XVERIFY(4) = C3P(IEQ)
       CALL VERIFYVAL(4,XVERIFY)
C	 WRITE(25,162) IEQ,C0P(IEQ),C1P(IEQ),C2P(IEQ),C3P(IEQ)
	 WRITE(25,162) IEQ,(XVERIFY(IXV),IXV=1,4)
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

C  REPLACE WRITING OF QVAL WITH XVERIFY (SEE LOGIC IN 
C  SUBROUTINE VERIFYVAL.

       XVERIFY(1) = QVAL
       CALL VERIFYVAL(1,XVERIFY)
C	 WRITE(25,9771) QVAL
       WRITE(25,9771) XVERIFY(1)
 9771   FORMAT(/' THE VALUE OF QVAL IS ',F5.1)

	WRITE(25,1213)
 1213   FORMAT(////' THE FOLLOWING IS THE OUTPUT FROM THE RUNNING OF'/
     1' THE PROGRAM.'///)


C  ESTABLISH THE INITIAL POPULATION MEAN ESTIMATES FOR THE 1ST
C  ITERATION. THESE WILL BE THE MIDRANGE VALUES FROM THE BOUNDARIES 
C  INPUT BY THE USER ABOVE.

	DO I=1,NVAR
	 ESTMEN(I) = (AB(I,1)+AB(I,2))/2.D0
	END DO


C  ESTABLISH THE INITIAL ESTIMATE OF THE POPULATION COVARIANCE MATRIX,
C  COVEST. IT WILL BE DIAGONAL WITH EACH VARIANCE = SIGI*SIGI, WHERE
C  SIGI = XSIG*(AB(I,2) - AB(I,1)).


C  NOTE THAT SIGI = (AB(I,2) - AB(I,1))/6 IS REASONABLE (SINCE IN A 
C  NORMAL DIST, THE PRACTICAL RANGE (99 % OF THE DISTRIBUTION) IS 
C  APPROX. 6 STANDARD DEVIATIONS WIDE). IF THE VALUE OF XSIG IS > 1/6,
C  IT WILL PUT RELATIVELY LESS EMPHASIS ON THE USER'S INITIAL BOUNDARY
C  ESTIMATES (SINCE THE INITIAL VARIANCES WILL BE LARGER); IF THE VALUE 
C  IS < 1/6, IT  WILL PUT RELATIVELY MORE EMPHASIS ON THE USER'S 
C  INITIAL BOUNDARY ESTIMATES. 

	DO I=1,NVAR
	DO J=1,NVAR  
	ESTCOV(I,J) = 0.D0 
	END DO
	SIGI = XSIG*(AB(I,2)-AB(I,1))

	ESTCOV(I,I) = SIGI*SIGI
	END DO


C  PRINT THE INITIAL ESTIMATES TO THE SCREEN AND TO THE OUTPUT FILE.

	WRITE(*,5101) NVAR
	WRITE(25,5101) NVAR
 5101   FORMAT(//' THE INITIAL POPULATION ESTIMATES ARE SHOWN BELOW'/
     1' (THE 1ST ROW GIVES THE MEANS; THE NEXT ',I1,' ROWS GIVE THE '/
     2' COVARIANCE MATRIX):'/)
	WRITE(*,5102) (PAR(I),I=1,NVAR)
	WRITE(25,5102) (PAR(I),I=1,NVAR)
5102   FORMAT(5X,30(A11,2X))

C  REPLACE WRITING OF ESTMEN() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL. SIMIARLY FOR ALL CALLS TO VERIFYVAL.

      DO I = 1,NVAR
       XVERIFY(I) = ESTMEN(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)  
    
C	WRITE(*,5103) (ESTMEN(I),I=1,NVAR)
      WRITE(*,5103) (XVERIFY(I),I=1,NVAR)
C	WRITE(25,5103) (ESTMEN(I),I=1,NVAR)
      WRITE(25,5103) (XVERIFY(I),I=1,NVAR)

5103   FORMAT(1X,30(G12.6,1X))

      WRITE(*,*)
      WRITE(25,*)
      WRITE(*,5102) (PAR(I),I=1,NVAR)
      WRITE(25,5102) (PAR(I),I=1,NVAR)


C  REPLACE WRITING OF ESTCOV() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL. SIMIARLY FOR ALL CALLS TO VERIFYVAL.

      DO I=1,NVAR

       DO J = 1,I

        XVERIFY(J) = ESTCOV(I,J)
       END DO
       CALL VERIFYVAL(I,XVERIFY)

C      WRITE(*,5103) (ESTCOV(I,J),J=1,I)
       WRITE(*,5103) (XVERIFY(J),J=1,I)
C      WRITE(25,5103) (ESTCOV(I,J),J=1,I)
       WRITE(25,5103) (XVERIFY(J),J=1,I)

      END DO


C  INITIALIZE GAMMA(IEQ) = 1. IF IGAMMA(IEQ)=1, GAMMA(IEQ) REMAINS 1 
C  THE ENTIRE RUN; IF IGAMMA(IEQ) .EQ. 0, GAMMA(IEQ) IS UPDATED EACH 
C  ITERATION.

	DO IEQ=1,NUMEQT
	 GAMMA(IEQ) = 1.D0
	END DO

C  GO THROUGH ALL NSUB SUBJECTS AND IDENTIFY WHICH IF ANY GIVE NO INFO
C  ON 1 OR MORE PARAMETERS. THIS IS DONE BY CALCULATING MATRIX COVINV 
C  = INV(COVEST) WHERE COVEST IS THE ESTIMATE OF THE COV. MATRIX OF
C  THETA (THE PARAMETER VECTOR) AT THE INITIAL POPULATION ESTIMATES,
C  ESTMEN(I),I=1,NVAR. IF, FOR A GIVEN SUBJECT, COVINV(I,I) = 0.D0 
C  (OR < 0.0), IT MEANS THAT SUBJECT CANNOT GIVE A REASONABLE ESTIMATE
C  FOR PARAMETER I. IN SUCH A CASE, INFORM THE USER. THIS 
C  'IDENTIFICATION' HAS NO EFFECT ON THE PROGRAM CALCULATIONS. EVERY
C  SUBJECT MUST CONTINUE TO GIVE AN ESTIMATE FOR EVERY PARAMETER 
C  SO THE CALCULATION OF THE UPDATED ESTIMATE FOR ESTCOV CAN BE DONE 
C  (THOUGH IN THE CASE OF A SUBJECT WHICH HAS NO INFO ON A PARAMETER, 
C  THIS ESTIMATE IS 'RANDOM' -- BUT LIKELY TO BE IN THE NEIGHBORHOOD OF 
C  THE INITIAL ESTIMATE = MEAN FROM LAST CYCLE). 

C  REWIND FILE 27, WHICH HAS THE PATIENT DATA FILES CONCATENATED
C  ON IT.


	REWIND(27)

	WRITE(*,701)
  701   FORMAT(//' CHECKING EACH SUBJECT FOR ADEQUATE INFORMATION TO'/
     1' ESTIMATE THE PARAMETERS.'//)


C  IF IRPRT STAYS 1 THROUGHOUT LOOP 700 ---> ALL SUBJECTS WILL BE ABLE
C  TO ESTIMATE ALL PARAMETERS. IF IT CHANGES TO 0 ---> AT LEAST 1
C  SUBJECT WILL BE UNABLE TO ESTIMATE AT LEAST 1 PARAMETER.

	IRPRT = 1

C  IESTIJ(I,J) = 1 ---> SUBJECT I CAN ESTIMATE PARAMETER J.
C	       = 0 ---> SUBJECT I CANNOT ESTIMATE PARAMETER J.

C  SET ALL ENTRIES = 1 INITIALLY AND CHANGE TO 0 IF APPROPRIATE (DURING
C  LOOP 700).

C  IREPRT(I) = 0 ---> SUBJECT I ESTIMATES ALL PARAMETERS.
C            = 1 ---> SUBJECT I DOES NOT ESTIMATE AT LEAST 1 PARAMETER.


	DO I=1,NSUB
	 DO J=1,NVAR
	  IESTIJ(I,J)=1
	 END DO
	 IREPRT(I)=1
	END DO

C  AFTER THE DO 700 LOOP BELOW, NOBTOT(IEQ) WILL = TOTAL NO. OF 
C  NON-MISSING OBSERVATIONS OVER ALL NSUB SUBJECTS, FOR OUTPUT EQUATION 
C  IEQ, IEQ = 1,NUMEQT.
C  NOTE THAT NOBTOT(1) MAY NOT EQUAL NOBTOT(2), ETC, IF THERE 
C  ARE ANY MISSING VALUES.

	DO IEQ=1,NUMEQT
	 NOBTOT(IEQ) = 0
	END DO


	DO 700 JSUB = 1, NSUB, 1

C wmy20190514 -- "IG=" and "IG =" is only found in DO 3500 JSUB=1,NSUB
C   so set IG=JSUB everywhere else in SUBROUTINE IT2B. This is because
C   NPAG requires both JSUB and IG be initialized throughout entire
C   calculation.  Merging NPAG with IT2B requires then that IG, like
C   JSUB, always be declared and initialized.
            IG = JSUB

	WRITE(*,*) "DO 700:", JSUB

C  CALL SUBROUTINE FILRED TO READ, FOR THIS SUBJECT, FROM SCRATCH FILE
C  27, THE NO. OF OBSERVATION TIMES (NOBSER) AS WELL AS THE 
C  OBSERVED VALUES THEMSELVES: YO(I,IEQ) = THE 'NOISY' OBSERVED VALUES 
C  FOR THIS SUBJECT; I=1,NOBSER, IEQ=1,NUMEQT. THESE OBSERVED VALUES ARE 
C  USED ONLY TO CALCULATE THE ASSAY STANDARD DEVIATIONS (USING THE 

C  VECTORS, C0,C1,C2,C3, WHICH ARE ALSO READ IN). THE REST OF THE INFO 
C  IN THE SUBJECT DATA FILE IS PASSED IN COMMONS TO THE IDPC MODULE 
C  SUBROUTINES.


C        write (*,*) "CALL FILRED at 1726; JSUB of NSUB", JSUB,NSUB
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT,INTLIST)

C  FIND THE ASSAY STANDARD DEVIATIONS FOR THIS SUBJECT. FOR EACH 
C  OF THE NOBSER*NUMEQT OBSERVED VALUES (EXCEPT THAT YO(I,IEQ) = -99 -->
C  OUTPUT EQ. IEQ HAS NO OBSERVED LEVEL FOR OBSERVATION TIME I),
C  Y, SIG = C0 + C1*Y + C2*Y**2 + C3*Y**3.
C  NOTE THAT, THEORETICALLY, SIG SHOULD BE A CUBIC FNT. OF
C  THE 'TRUE' OBSERVED VALUES, NOT THE 'NOISY' OBSERVED VALUES (BUT THE
C  'TRUE' VALUES ARE UNKNOWN).

C  ALSO, CALCULATE SIGFAC, THE PRODUCT OF THE NON-MISSING STD. DEV.'S
C  (A NON-MISSING S.D. IS ONE FOR WHICH THE CORRESPONDING YO(I,IEQ) IS
C  .NE. -99, THE MISSING VALUE CODE).
C  INITIALIZE SIGFAC=1, AND THEN UPDATE IT FOR EACH NON-MISSING
C  OBSERVATION.

C  ALSO CALCULATE RINV WHICH IS THE DIAGONAL MATRIX CONTAINING THE 
C  RECIPROCALS OF THE VARIANCES FOR ALL THE NON-MISSING OBSERVATIONS
C  IN THIS PATIENT DATA FILE. THE TOTAL NO. OF NON-MISSING OBSERVATIONS
C  = NOBACT (SEE BELOW), SO RINV IS NOBACT x NOBACT AFTER LOOP 140. THE
C  ORDER OF ENTRIES FOR RINV CORRESPONDS TO THE STANDARD COLUMN-WISE 
C  ORDERING FROM MATRIX YO (WHICH IS NOBSER x NUMEQT). NOTE BELOW THAT
C  NOBACT = NOBSER*NUMEQT - MISTOT, WHERE MISTOT = TOTAL NO. OF MISSING
C  VALUES AMONG THE NOBSER*NUMEQT OBSERVATIONS.

C  FIRST, ZERO OUT ALL THE OBSERVATIONS IN RINV. THE DIAGONAL ELEMENTS
C  WILL BE ESTABLISHED IN LOOP 140 BELOW. NDEX WILL BE THE RUNNING
C  INDEX OF THE LAST NON-MISSING VARIANCE PUT INTO RINV.

	MAXTOT = NUMEQT*NOBSER
	DO I =1,MAXTOT
	 DO J=1,MAXTOT
	  RINV(I,J) = 0.D0
	 END DO

	END DO

        NDEX = 0

C  MISVAL(IEQ) WILL BE THE RUNNING TOTAL OF MISSING VALUES AMONG THE
C  NOBSER POTENTIAL OBSERVED LEVELS FOR OUTPUT EQ. IEQ.

        DO IEQ=1,NUMEQT
          MISVAL(IEQ) = 0

        END DO

        SIGFAC=1.D0

      DO 140 IEQ=1,NUMEQT
       DO 140 I=1,NOBSER

        Y=YO(I,IEQ)

C  IF Y = -99, IT MEANS THAT OUTPUT EQ. J HAD NO VALUE AT OBSERVATION
C  TIME I. IN THIS CASE, IGNORE THIS Y AND INCREASE MISVAL(J) BY 1.

	  IF(Y .EQ. -99) THEN
	   MISVAL(IEQ) = MISVAL(IEQ) + 1
	   GO TO 140
	  ENDIF

C  NOTE: FOR EACH SUBJECT, MUST ENSURE THAT ALL THE STD DEV'S ARE NON-
C	 ZERO. OTHERWISE, THE PROGRAM WILL BLOW UP! THIS IS BECAUSE 
C 	 P(YJ|X) INVOLVES SQUARED DIFFERNCES BETWEEN OBSERVED Y'S AND
C	 EXPECTED Y'S (FOR A GIVEN PARAMETER VECTOR X)...EACH DIFFERENCE 
C	 NORMALIZED (I.E., DIVIDED) BY THE VARIANCE OF THE RESPECTED
C	 OBSERVATION.

C  	 SEE m1_3calc.f CODE FOR COMMENTS ON HOW A S.D. COULD = 0.

C  ALSO TEST TO MAKE SURE NO SIG(I) < 0, SINCE LOG(SIGFAC) IS USED IN 

C  SUBROUTINE MAPBAYS.

      SIG(I,IEQ)=GAMMA(IEQ)*(C0(IEQ)+C1(IEQ)*Y+C2(IEQ)*Y*Y+C3(IEQ)*Y**3)

      IF(SIG(I,IEQ) .EQ. 0) THEN


       WRITE(*,2345) JSUB
       WRITE(25,2345) JSUB
2345   FORMAT(//' A S.D. IS 0 FOR JSUB = ',I5,'. RERUN THE '/
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

      IF(SIG(I,IEQ) .LT. 0) THEN


       WRITE(*,2346) JSUB
       WRITE(25,2346) JSUB
2346   FORMAT(//' A S.D. < 0 FOR JSUB = ',I5,'. RERUN THE '/
     1' PROGRAM WITH A BETTER CHOICE FOR THE ASSAY ERROR POLYNOMIAL'/
     2' COEFFICIENTS.')
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

      SIGFAC=SIGFAC*SIG(I,IEQ)

      NDEX = NDEX+1
      RINV(NDEX,NDEX) = 1.D0/SIG(I,IEQ)/SIG(I,IEQ)

  140 CONTINUE

C  CALCULATE MISTOT = TOTAL NO. OF MISSING VALUES AMONG ALL THE 
C  NOBSER*NUMEQT OBSERVED VALUES. ALSO, UPDATE NOBTOT(IEQ) = RUNNING
C  TOTAL NO. OF OBSERVED VALUES FOR OUTPUT EQ. IEQ, OVER ALL NSUB
C  SUBJECTS.

        MISTOT = 0
        DO IEQ=1,NUMEQT
          MISTOT = MISTOT + MISVAL(IEQ)
          NOBTOT(IEQ) = NOBTOT(IEQ) + NOBSER - MISVAL(IEQ)
        END DO

C  NOTE THAT SIGFAC WAS CALCULATED IN LOOP 140 ABOVE, AND THAT OFAC IS 
C  NOW THE RESULT OF NOBACT = NOBSER*NUMEQT - MISTOT VALUES. 

        NOBACT = NOBSER*NUMEQT - MISTOT
        OFAC=2.506628274631**NOBACT

C  NOTE THAT 2.5066... = SQRT(2*PI).


C  ESTABLISH THE PARAMETER VECTOR STORED INTO ESTMEN ABOVE (INITIAL
C  POPULATION MEAN VECTOR).

        DO J=1,NVAR
          THETA(J) = ESTMEN(J) 	
        END DO


C  FIND THE ESTIMATE OF THE INVERSE OF THE COVARIANCE MATRIX OF THETA.
C  IT WILL BE CALLED COVINV.

C  I.E., COVEST IS THE ESTIMATE OF THE COVARIANCE MATRIX OF THETA, SO
C  COVINV = INV(COVEST) =   PMAT'*RINV*PMAT,   WHERE:

C	' INDICATES TRANSPOSE;

C	PMAT = NOBACT x NVAR MATRIX, WITH THE (I,J) ELEMENT = PARTIAL 
C	DERIVATIVE OF THE ITH NON-MISSING VALUE IN THE YO MATRIX  W.R.T. 
C	THE JTH COORDINATE OF THE PARAMETER VECTOR, AT THE TIME OF THAT

C	OBSERVATION IN YO, ASSUMING ALL THE PARAMETERS = THEIR 
C 	ESTIMATED VALUES IN THETA. NOTE THAT THE ITH NON-MISSING VALUE

C	IN YO IS COUNTED "COLUMN-WISE". I.E., THE ORDERING OF THE NOBACT
C	VALUES IN YO IS (EXCLUDING MISSING VALUES, -99'S):
C	(1,1),(2,1),...,(NOBSER,1), (1,2),(2,2),...,(NOBSER,2), ...
C	(1,NUMEQT),(2,NUMEQT),...,(NOBSER,NUMEQT);

C 	RINV = NOBACT x NOBACT DIAGONAL MATRIX WITH 1/S(I)**2 AS THE 
C	(I,I) ELEMENT, WHERE S(I) IS THE SIG(.,.) ELEMENT WHICH 
C	CORRESPONDS TO THE ITH NON-MISSING VALUE IN YO (SEE ABOVE DEFN. 
C	OF PMAT). NOTE THAT RINV WAS ESTABLISHED IN LOOP 140.

C  OBTAIN PMAT FROM SUBROUTINE IDCALCP, A VERSION OF THE ID PROGRAM,
C  MODIFIED TO CALCULATE THE DESIRED PARTIAL DERIVATIVES USING 'FORWARD'
C  DIFFERENCES.


C  BEFORE ALL CALLS TO IDCALCP, MUST INTEGRATE FIXED, RANDOM, AND
C  RANFIX VALUES INTO PX, USING IRAN(I),I=1,NVAR+NOFIX+NRANFIX. CALL 
C  MAKEVEC TO DO THIS.

        CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,THETA,VALFIX,RANFIXEST,PX)

C wmy20190517 -- Bug search. NNNNNN increments, albeit incorrectly!
C   and JSUB does not increment past 2. This leads to an EOF error in
C   read fort.27.  If I comment out the following call to IDCALCP,
C   incrementing of NNNNNN mirrors JSUB increment, and JSUB is
C   incremented correctly.
C
C   Above bug was fixed by adding N to the argument list of SYMBOL
C
        CALL IDCALCP(JSUB,IG,NVAR,NOFIX,NRANFIX,IRAN,NDIM,
     1    PX,PMAT,INTLIST,RPAR,IPAR)

C        write (*,*) "Ret. Fr. IDCALCP" 

C  CALL CALCOV TO CALCULATE COVINV FROM THE ABOVE FORMULA.

        CALL CALCOV(NVAR,NOBACT,PMAT,RINV,COVINV)

C        write (*,*) "Ret. fr. CALCOV="

        DO I=1,NVAR

          IF(COVINV(I,I) .LE. 0) THEN


C  INDICATE IN MATRIX IESTIJ THAT PARAMETER I FOR SUBJECT JSUB CANNOT
C  BE ESTIMATED. ALSO, CHANGE IREPRT(JSUB) AND IRPRT TO 0.

            IRPRT=0
            IREPRT(JSUB)=0
            IESTIJ(JSUB,I)=0

          ENDIF

        END DO

C        write (*,*) "At 700, JSUB=",JSUB,NNNNNN
  700 CONTINUE


C  REPORT TO THE USER THE ABOVE RESULTS (TO THE SCREEN AND OUTPUT FILE).

	IF(IRPRT .EQ. 1) WRITE(*,5106) NSUB,NVAR
	IF(IRPRT .EQ. 1) WRITE(25,5106) NSUB,NVAR
 5106   FORMAT(//' ALL ',I3,' SUBJECTS GIVE ADEQUATE INFORMATION ON '/
     1' ALL ',I2,' PARAMETERS.'/)

	IF(IRPRT .EQ. 0) THEN

C  REPORT THE SUBJECTS WHICH CANNOT ESTIMATE CERTAIN PARAMETERS.

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

C  THE ABOVE ENDIF IS FOR THE  IF(IRPRT .EQ. 0) CONDITION.



C  START THE ANALYSIS. PRESET OLDAVG (THE PREVIOUS ITERATION'S AVERAGE
C  OF THE NEGATIVE FUNCTIONAL VALUES RETURNED FROM ELDER, OVER ALL NSUB 
C  SUBJECTS) TO A LARGE NEGATIVE NUMBER. THIS WILL ENSURE THAT THE 
C  CONVERGENCE CRITERION CANNOT BE MET AFTER JUST ONE ITERATION 
C  (SEE LOGIC AT END OF LOOP).

	OLDAVG=-1.D30


C  FILE 28 MUST BE REOPENED BEFORE EACH NEW ITERATION. OPEN IT NOW,
C  SO IT CAN BE CLOSED AND THEN REOPENED BEFORE THE START OF THE
C  1ST ITERATION.

C	write (*,*) "OPEN(28,FILE=PARFIL)", PARFIL
	OPEN(28,FILE=PARFIL)



	DO 2000 ITER = 1,MAXIT



C  REWIND FILE 27, WHICH HAS THE PATIENT DATA FILES CONCATENATED

C  ON IT.

	REWIND(27)

	WRITE(*,1217) ITER 
	WRITE(25,1217) ITER 
 1217 FORMAT(//' ITERATION NUMBER ',I5,'.'//)



C  CLOSE AND THEN OPEN FILE 28, THE FILE WHICH CONTAINS PARAMETER INFO 
C  FROM THE LAST ITERATION ONLY.

	CLOSE(28)

C wmy20190522 -- BUG -- PARFIL is set to NULL after first loop, so
C OPEN(28,FILE=PARFIL) below crashes program.
	PARFIL = 'LAST'//NAME

C	write (*,*) "OPEN(28,FILE=PARFIL)", PARFIL

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


C  FIND ESTINV = INV(ESTCOV).

C  STORE ESTCOV INTO ESTINV. ON RETURN FROM SUBROUTINE MATNV2, ESTINV
C  WILL BE INV(ESTCOV).

	DO I=1,NVAR
	 DO J=1,NVAR
	  ESTINV(I,J) = ESTCOV(I,J)
	 END DO
	END DO

 	CALL MATNV2(ESTINV,NVAR,B,1,DET)

C  NOTE THAT B IS A WORK VECTOR, AND DET IS THE DETERMINANT OF ESTCOV.


	IF(DET .LE. 0) THEN

C  NOTE THAT IF DET .LE. 0 --> THE PROGRAM WILL 
C  BOMB WHEN MAPBAYS IS CALLED BY ELDER BELOW (SINCE MAPBAYS HAS A 
C  LOG(DET) IN IT). TO PREVENT THIS, PRINT MESSAGE TO USER AND STOP.
 
	 WRITE(*,1216)
	 WRITE(25,1216) 
 1216   FORMAT(/' THE CURRENT POPULATION COV. ESTIMATE IS SINGULAR.'/
     1' THIS INDICATES AN ILL-CONDITIONED PROBLEM, OR POSSIBLY AN'/
     2' OVER-PARAMETERIZED PROBLEM (I.E., FEWER OBSERVED VALUES THAN'/
     3' PARAMETERS TO BE ESTIMATED). PLEASE RE-EXAMINE YOUR PATIENT'/
     4' DATA FILES, AND YOUR INPUT INSTRUCTIONS, CORRECT ANY '/
     5' INCONSISTENCIES, AND THEN RERUN THE PROGRAM.'//)



C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.


        OPEN(42,FILE=ERRFIL)


         WRITE(42,1216) 
        CLOSE(42)


       CLOSE(27)
       CLOSE(25)
       CALL PAUSE
       STOP


      ENDIF


	SUMLOG=0.D0

C  SUMLOG IS THE RUNNING SUM (OVER ALL THE SUBJECTS) OF THE 
C  LOG-LIKLIHOODS OF THE PARAMETER ESTIMATES, GIVEN THE DATA AND THE
C  CURRENT ESTIMATES OF THE POPULATION MEAN VECTOR AND COVARIANCE
C  MATRIX (SUMLOG = SUM(-VALMIN), VALMIN RETURNS FROM ELDERY).

	WRITE(*,1215)
 1215   FORMAT(/' SUBJECT NUMBERS FOLLOW AS PARAMETER ESTIMATES ARE'/
     1' BEING FOUND: '/) 


 9999   FORMAT(' ',I3)  

C  SUMDIF(IEQ) WILL BE THE RUNNING SUM (OVER ALL THE SUBJECTS) OF THE
C  NORMALIZED SUM OF SQUARES OF DIFFERENCES BETWEEN THE OBSERVED
C  CONCENTRATIONS AND THE EXPECTED VALUES OF THESW OBSERVATIONS FOR 
C  OUTPUT EQUATION IEQ (IEQ=1,NUMEQT), GIVEN THE UPDATED PARAMETER 
C  ESTIMATES. SUMDIF(IEQ) WILL BE USED AT THE END OF LOOP 1000 TO 
C  CALCULATE THE UPDATED ESTIMATE OF GAMMA(IEQ) -- IF IGAMMA(IEQ) = 0.

	DO IEQ=1,NUMEQT
	 SUMDIF(IEQ) = 0.D0
	END DO


      DO 1000 JSUB=1,NSUB
        IG = JSUB

	WRITE(*,9999) JSUB

C  CALL SUBROUTINE FILRED TO READ, FOR THIS SUBJECT, FROM SCRATCH FILE
C  27, THE NO. OF OBSERVATIONS (NOBSER) AS WELL AS THE 
C  OBSERVED VALUES THEMSELVES: YO(I,IEQ) = THE 'NOISY' OBSERVED VALUES 
C  FOR THIS SUBJECT; I=1,NOBSER, IEQ=1,NUMEQT. THESE OBSERVED VALUES ARE 
C  USED ONLY TO CALCULATE THE ASSAY STANDARD DEVIATIONS (USING THE 
C  VECTORS, C0,C1,C2,C3, WHICH ARE ALSO READ IN). THE REST OF THE INFO 
C  IN THE SUBJECT DATA FILE IS PASSED IN COMMONS TO THE IDPC MODULE 
C  SUBROUTINES.

C        write (*,*) "DO 1000: CALL FILRED near 2164"
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT,INTLIST)

C  FIND THE ASSAY STANDARD DEVIATIONS FOR THIS SUBJECT, ETC. SEE 
C  ADDITIONAL COMMENTS IN LOOP 700.

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

C        write (*,*) "DO 1000: OFAC, MISTOT, and SIGFAC, calculated"

C  FIND THE UPDATED ESTIMATES OF THE PARAMETER VECTOR, THETA(I), I=1,
C  NVAR, FOR THIS SUBJECT.

C  PREPARE TO CALL ELDERY.

C  THE INITIAL ESTIMATES FOR EACH THETA(I), I=1,NVAR, IS ESTMEN(I).
C  IF ITER > 1, ESTMEN(I) IS THE POPULATION MEAN ESTIMATE FROM THE 
C  PREVIOUS ITERATION. IF ITER = 1, ESTMEN(I) IS THE MIDPOINT OF THE
C  BOUNDARY RANGE INPUT BY THE USER.

        DO I=1,NVAR
          START(I)=ESTMEN(I)
          STEP(I)= -.2D0*START(I)
        END DO

C        write (*,*) "DO 1000: Calling ELDERY"

	CALL ELDERY(NVAR,START,THETA,VALMIN,1.D-10,STEP,1000,MAPBAYS,
     1  0,ICONV,NITER,ICNT,JSUB,IG,INTLIST,RPAR,IPAR)

C        write (*,*) "DO 1000: Ret. fr. ELDERY; func=MAPBAYS: VALMIN="
C     1    ,VALMIN,JSUB

C  THETA = THE PARAMETER VECTOR, FOR THIS SUBJECT, WHICH GIVES THE 

C	   LARGEST  P(THETA|YJ AND ESTMEN,ESTCOV), WHERE ESTMEN AND 
C	   ESTCOV ARE THIS ITERATION'S ESTIMATES OF THE POPULATION MEAN
C	   VECTOR AND COVARIANCE MATRIX, RESPECTIVELY, AND YJ IS THIS
C	   SUBJECT'S DATA ARRAY.

C	   THETA IS THE 'MAP BAYESIAN' ESTIMATE OF THE PARAMETER VECTOR
C	   FOR THIS SUBJECT.

C  VALMIN = MIN. VALUE OF FUNCTION ACHIEVED, SO -VALMIN = MAXIMUM 
C	    LOG OF  P(YJ,THETA|ESTMEN,ESTCOV) ACHIEVED.

C  ICONV = 1 IF MAX-LIK ESTIMATE CONVERGED; 0 OTHERWISE.

	IF(ICONV .EQ. 0) WRITE(*,9011) JSUB
	IF(ICONV .EQ. 0) WRITE(25,9011) JSUB
 9011 FORMAT(' ',' NO PARAMETER ESTIMATE CONV. FOR SUBJECT NO ',I3)


C  NEW FOR MXEM1S55.FOR: 


C  WARN USER IF ANY ENTRY IN THETA IS 'SMALL', BECAUSE WHEN IDCALCP IS 
C  CALLED (IN LOOP 1500) WITH A SMALL VALUE FOR AN ENTRY IN THETA, THE 
C  RESULTANT VALUES IN PMAT CAN VARY WILDLY AFTER INCREMENTING THETA TO 
C  CALCULATE FORWARD DIFFERENCES. AS A RESULT, TWO DIFFERENT RUNS WITH 
C  VERY SMALL DIFFERENCES IN THETA CAN HAVE BIG DIFFERENCES IN PMAT 
C  VALUES. SEE MXEM1S54.BUG WHICH ANALYZED BIG DIFFERENCES WHICH OCCURED
C  WHEN THE SAME RUN WAS DONE WITH SUBJECTS ENTERED IN DIFFERENT ORDERS
C  (THE DIFFERENT ORDERS RESULTED IN VERY SMALL DIFFERENCES (DUE TO
C  ROUNDOFF) IN THETA AFTER A FEW ITERATIONS, AND ONE ENTRY IN THETA
C  WAS CLOSE TO 0.

C  NO. ALLOW THE RUN TO CONTINUE IN THIS CASE, WITH A MESSAGE TO THE
C  USER.

C  AS OF itbig9y.f, THE WARNINGS ABOUT SMALL PARAMETER VALUES IS REMOVED
C  SINCE PARAMETERS WHICH ARE LOGS CAN BE NEGATIVE.


C  NOTE THAT, RETURNING FROM SUBROUTINE MAPBAYS, VIA COMMON/TOMAP,
C  IS SSND(IEQ), IEQ=1,NUMEQT. SSND(IEQ) = THE SUM OF SQUARES OF 
C  NORMALIZED DIFFERENCES BETWEEN THIS SUBJECT'S OBSERVATIONS FOR OUTPUT
C  EQUATION IEQ, AND THE EXPECTED VALUE OF THESE OBSERVATIONS (GIVEN THE 
C  PARAMETER VECTOR THETA RETURNED ABOVE IN THE CALL TO ELDER). ADD THIS 
C  SUM TO SUMDIF(IEQ), IEQ=1,NUMEQT. NOTE THAT, BELOW LOOP 1000, THESE 
C  SUMS WILL BE USED TO CALCULATE UPDATED ESTIMATES OF GAMMA(IEQ), FOR
C  EACH IEQ WITH IGAMMA(IEQ) = 0.



	DO IEQ=1,NUMEQT
	 SUMDIF(IEQ) = SUMDIF(IEQ) + SSND(IEQ)
	END DO

	DO J=1,NVAR
	IF(IESTIJ(JSUB,J) .EQ. 1) PAREST(JSUB,J) = THETA(J)
	IF(IESTIJ(JSUB,J) .EQ. 0) PAREST(JSUB,J) = 0.D0 
	END DO

C  NOTE THAT IF THIS SUBJECT GIVES NO INFO ON PARAMETER J, THETA(J) IS
C  SET = 0 FOR NOW. BELOW THE (1000) LOOP, THIS THETA(J) WILL BE RESET
C  EQUAL TO THE MEAN OF PARAMETER J ESTIMATES FROM ALL THE SUBJECTS
C  WHICH HAVE INFO ON IT.

C        SUMLOG = SUMLOG + (-VALMIN)
         SUMLOG = SUMLOG - VALMIN

C        write (*,*) "At 1000: SUMLOG=",SUMLOG

 1000   CONTINUE


C  CALCULATE THE UPDATED ESTIMATES OF GAMMA(IEQ) HERE (SEE PG. 1A OF 
C  1/26/96 NOTES IN MXEM1S40.FOR) -- FOR EACH GAMMA WITH 
C  IGAMMA(IEQ) = 0).

C  NOTE: CALCULATING GAMMA(IEQ)'S HERE MEANS THAT THE UPDATED 
C        GAMMA(IEQ)'S WILL BE USED IN CALCULATING COVINV (WHICH IS A 
C        FUNCTION OF RINV, WHICH IS A FUNCTION OF THE SIG(I,J)'S,
C	 WHICH HAVE GAMMA(IEQ)'S IN THEM), IN LOOP 1500.

	DO IEQ=1,NUMEQT
	 IF(IGAMMA(IEQ) .EQ. 0) 
     1   GAMMA(IEQ) = GAMMA(IEQ)*DSQRT(SUMDIF(IEQ)/NOBTOT(IEQ))
	END DO

	AVGLOG = SUMLOG/dble(NSUB)

C        write (*,*) "AVGLOG=SUMLOG/NSUB",AVGLOG,SUMLOG,NSUB

C  AVGLOG IS NOW THE AVERAGE LOG-LIKELIHOOD OF  
C  P(YJ,THETA|ESTMEN, ESTCOV), WHERE ESTMEN AND ESTCOV ARE THE
C  ESTIMATES (PRIOR TO THIS ITERATION) OF THE POPULATION MEAN VECTOR 
C  AND COVARIANCE MATRIX, RESPECTIVELY, OVER ALL SUBJECTS, AND THETA
C  IS A GIVEN SUBJECT'S MAP-BAYESIAN ESTIMATE.

C  CALCULATE THE UPDATED POPULATION MEAN VECTOR, ESTMEN, FOR THIS 
C  ITERATION.

C  NOTE THAT THE NEW ESTMEN(J) = SUM OF ALL PARAMETER ESTIMATES IN 
C  PAREST(JSUB,J) FOR THE SUBJECTS WHICH GIVE AN ESTIMATE FOR THAT 
C  PARAMETER. THE NO. OF SUBJECTS WHICH GIVE AN ESTIMATE FOR PARAMETER

C  J = SUM OF COLUMN J OF IESTIJ = SUMCOL(J).

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

C  NOTE: IF SUMCOL(J) = 0, ESTMEN(J) IS LEFT UNCHANGED FROM PREVIOUS

C        VALUE.

	IF(SUMCOL(J) .EQ. 0.D0) THEN
	 WRITE(*,7131) PAR(J)
	 WRITE(25,7131) PAR(J)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

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

C  SEE FORMAT LABEL 5111. THERE THE PROGRAM STOPS IF THE CURRENT 
C  ESTIMATE OF THE POPULATION COV. MATRIX (ESTCOV) HAS ANY DIAGONAL
C  ELEMENTS .LE. 0.


	END DO

C  NOW RESET THE PARAMETER ESTIMATE FOR EACH SUBJECT WHICH HAS NO
C  INFORMATION ON A PARAMETER TO THE JUST CALCULATED POPULATION MEAN
C  ESTIMATE (FROM ALL THE SUBJECTS WHICH DO HAVE INFO ON THAT
C  PARAMETER).

	DO JSUB=1,NSUB	
	 DO J=1,NVAR
	  IF(IESTIJ(JSUB,J) .EQ. 0) PAREST(JSUB,J) = ESTMEN(J) 
	 END DO
	END DO


C  CALCULATE THE UPDATED POPULATION MEDIAN VECTOR, ESTMED, FOR THIS 
C  ITERATION. CALL GETMED. NOTE THAT THE MEDIAN, LIKE THE MEAN,
C  IS FOR ONLY THOSE SUBJECTS WHICH HAVE INFO ON A GIVEN PARAMETER.

	CALL GETMED(NVAR,NSUB,MAXSUB,MAXDIM,IESTIJ,PAREST,VEC,ESTMED)


C  CALCULATE UPDATED ESTCOV = AVERAGE, OVER ALL SUBJECTS, OF
C  PIK(.,.) + DIFF(.)*DIFF(.)'], WHERE PIK IS DESCRIBED IN MODEL AT THE
C  TOP OF THIS CODE, DIFF(.) = PAREST(ISUB,.) - ESTMEN(.), AND 
C  ESTMEN(.) IS THE JUST UPDATED ESTMEN ABOVE.

C  SUMCOV BELOW IS THE RUNNING SUM OF THE NUMERATOR OF ESTCOV.



	DO I=1,NVAR
	 DO J=1,NVAR
	  SUMCOV(I,J) = 0.D0
	 END DO
	END DO


C  NOW GO THROUGH ALL SUBJECTS AGAINS, WITH THE PARAMETER ESTIMATE
C  VALUES IN PAREST, TO FIND THE MATRICES PIK, AND ADD THE
C  APPROPRIATE PIECES TO SUMCOV FOR EACH SUBJECT.

	WRITE(*,1219)
 1219   FORMAT(/' SUBJECT NUMBERS FOLLOW AS COVARIANCE MATRIX'/
     1' ESTIMATES (OF PARAMETER ESTIMATES) ARE BEING FOUND: '/) 


C  SEE COMMENTS IN DO LOOPS 700 AND 1000 FOR EXPLANATION OF THE 
C  ACTIVITIES IN LOOP 1500 BELOW.


	REWIND(27)


	DO 1500 JSUB=1,NSUB
          IG=JSUB


	WRITE(*,9999) JSUB

C        write (*,*) "Pretty sure this is it! CALL FILRED at 2437"

	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT,INTLIST)
	
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

C  FOR THIS SUBJECT, NO., JSUB, ESTABLISH THE THETA VECTOR
C  STORED INTO PAREST ABOVE.

	DO J=1,NVAR
	 THETA(J) = PAREST(JSUB,J)
	END DO

C  FIND THE ESTIMATE OF THE INVERSE OF THE COVARIANCE MATRIX OF THETA.
C  IT WILL BE CALLED COVINV. SEE DETAILS BELOW LOOP 140 ABOUT THE
C  CALCULATION OF PMAT AND COVINV = INV(COVEST).

	CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,THETA,VALFIX,RANFIXEST,PX)
	CALL IDCALCP(JSUB,IG,NVAR,NOFIX,NRANFIX,IRAN,NDIM,PX,PMAT,
     1     INTLIST,RPAR,IPAR)

	CALL CALCOV(NVAR,NOBACT,PMAT,RINV,COVINV)

	DO I=1,NVAR

	IF(COVINV(I,I) .LE. 0) THEN

	  IF(IESTIJ(JSUB,I) .EQ. 1) THEN

C  SUBJECT JSUB WAS SUPPOSED TO HAVE BEEN ABLE TO ESTIMATE PARAMETER I
C  (AS DETERMINED IN LOOP 700 ABOVE). NOW IT CANNOT. REPORT THIS TO THE
C  USER, AND CHANGE THE ENTRY IN IESTIJ TO 0.


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


C  THE ABOVE ENDIF IS FOR THE IF(COVINV(I,I) .LE. 0) CONDITION.
C  THE END DO BELOW IS FOR THE DO I=1,NVAR STATEMENT.



	END DO	   


C  CALL CALCPIK TO CALCULATE PIK (SEE MODEL DETAILS AT THE TOP OF 
C  PROGRAM). 

C  PIK = INV[COVINV + ESTINV], WHERE ESTINV = INV(ESTCOV) WAS
C  	 CALCULATED OUTSIDE THE DO 1500, SUBJECT LOOP, SINCE IT
C	 IS SAME FOR ALL SUBJECTS.



	CALL CALCPIK(NVAR,COVINV,ESTINV,PIK)


C  CALCULATE DIFF(.) FOR THIS SUBJECT.

	DO J=1,NVAR
	 DIFF(J) = THETA(J) - ESTMEN(J)
	END DO

C  CALCULATE DIFF(.)*DIFF(.)' = DIFPRD, FOR THIS SUBJECT.

	DO I=1,NVAR
	 DO J=1,NVAR
	  DIFPRD(I,J) = DIFF(I)*DIFF(J)
	 END DO
	END DO

C  ADD THIS SUBJECT'S PIK(.,.) + DIFPRD(.,.) TO SUMCOV(.,.).

	DO I=1,NVAR
	 DO J=1,NVAR
	  SUMCOV(I,J) = SUMCOV(I,J) + PIK(I,J) + DIFPRD(I,J)
	 END DO
	END DO


C  ADD THIS SUBJECT'S VALUES TO FILE 28.

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

C  CHECK FOR AN "ILL-CONDITIONED" PROBLEM ... ONE IN WHICH ANY PIK(I,I)
C  IS .LE. 0. SINCE PIK IS A COVARIANCE MATRIX, IT SHOULD BE POSITIVE
C  DEFINITE; SO IF ANY PIK(I,I) .LE. 0, STOP THE PROGRAM WITH A MESSAGE.


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

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

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


C  NOW DIVIDE SUMCOV BY NSUB TO GET ESTCOV. 

	DO I=1,NVAR
	 DO J=1,NVAR
	  ESTCOV(I,J) = SUMCOV(I,J)/NSUB
	 END DO
	END DO

c  AS OF itbig1.f, CALCULATION OF TRULOG IS ONLY DONE IF ILOG=0.

C  AS OF itbig7.f, ILOG IS HARDCODED = 0.


C ------ CALCULATION OF TRULOG AND RELATED STUFF BELOW ------C ----- C

C  ALSO, A DENSITY FILE FOR NPAG WILL BE CONSTRUCTED BELOW. IT WILL
C  CONSIST OF THE FINAL CYCLE PARAMETER ESTIMATES FOR EACH OF THE
C  NSUB SUBJECTS, EACH ASSOCIATED WITH A PROBABILITY OF 1/NSUB.


C  IF THIS IS THE LAST CYCLE:

C  CALCULATE TRULOG, THE TRUE (NUMERICAL) LOG-LIK OF THE PATIENT DATA 
C  FILES.

C  THIS IS THE LAST ITERATION IF ITER = MAXIT, OR IF 
C  DABS(AVGLOG - OLDAVG) .LT. TOL.

C  INITIALIZE ITRULOG = 2 (WHICH MEANS THAT THIS IS NOT THE END OF 
C  THE LAST CYCLE. THEN CHANGE IT TO 1 (IF THIS IS THE END OF THE 
C  LAST CYCLE). 

	ITRULOG=2

	ISTOP=0
	IF(DABS(AVGLOG - OLDAVG) .LT. TOL) ISTOP=1


	IF(ITER .EQ. MAXIT .OR. ISTOP .EQ. 1) THEN



	 ITRULOG=1

C  AS OF itbig7.f, TRULOG IS NOT A FUNCTION OF ESTCOV AND SO THE TEST
C  TO ABANDON THE TRULOG CALCULATION IF ANY ESTCOV(I,I) IS .LE. 0
C  HAS BEEN REMOVED.


C  STEPS TO CALCULATE TRULOG:


C  1. CALCULATE BOUNDARIES FOR EACH PARAMETER - THIS INFO IS NO LONGER
C     NEEDED FOR THE CALCULATION OF TRULOG (SEE BELOW), BUT IS TO BE
C     PRINTED TO FILES 28, AND 25.

C     AS OF itbig7.f, TRULOG WILL BE CALCULATED BASED ON THE
C     "GRID POINTS" WHICH ARE SIMPLY THE FINAL CYCLE PARAMETER 
C     ESTIMATES FOR EACH OF THE NSUB SUBJECTS, AND WILL NO LONGER
C     USE AN NPAG-LIKE GRID. 

C  2. INTEGRATE TO FIND P(YI) FOR EACH SUBJECT... SIMILAR 
C     TO FINDING P(YJ) IN NPAG AT LABEL 800.

C  3. CALC. SUM (LOG P(YI)) = TRULOG


C     NOTE THAT TRULOG IS PRINTED OUT BELOW PRINT OUT OF AVGLOG.

C  AS OF it2beng18.f, ONLY R.V.'S WITH IRAN(.) = -1 MAY HAVE NEGATIVE
C  BOUNDARIES. SO ...

C  FIND THE BOUNDARIES FOR THE PARAMETERS (SO THEY CAN BE STORED INTO
C  FILE FROMFIL. FOR PARAMETER I, THE BOUNDARIES WILL BE  
C  ESTMEN(I) +/- XDEV*(STD DEV(I)), WITH XDEV(I) = SQRT(ESTCOV(I,I)),
C  EXCEPT THAT A R.V. WITH IRAN(.) = 1 (I.E., IT MUST BE NON-NEGATIVE)
C  WILL HAVE ITS LOWER BOUNDARY NO LOWER THAN 1.D-8.
 
C  A R.V. WITH IRAN(.) = -1 IS ALLOWED TO HAVE NEGATIVE VALUES, AND 
C  NEGATIVE BOUNDARIES (E.G. LOGS OF OTHER PARAMETERS WILL HAVE NEGATIVE
C  VALUES).

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


c  AS OF itbig1.f, CALCULATION OF TRULOG IS ONLY DONE IF ILOG=0.

	IF(ILOG .EQ. 1) GO TO 5120

	WRITE(*,*)
	WRITE(*,*)
	WRITE(*,*)' CALCULATING THE TRUE (NUMERICAL) LOG-LIKELIHOOD'
	WRITE(*,*)' OF THE PATIENT DATA FILES ... '
	WRITE(*,*)
	WRITE(*,*)


C  AS OF itbig7.f, THE NO. OF "GRID" POINTS WILL JUST BE THE NO.
C  OF SUBJECTS, NSUB, AND EACH "GRID" POINT WILL BE THE FINAL
C  CYCLE PARAMETER ESTIMATES FROM THE CORRESPONDING SUBJECT, WHICH
C  WERE STORED INTO PAREST ABOVE.

	NGRID = NSUB
	DO JSUB=1,NGRID	
	 DO J=1,NVAR
	  CORDEN(JSUB,J) = PAREST(JSUB,J)
	 END DO
	END DO


C  AS OF itbig7.f, PXGEE(IG) = P(X|ESTMEN,ESTCOV) HAS BEEN REPLACED BY
C  THE CONSTANT VALUE OF 1/NSUB. SO ALL THE CODE LEADING TO THE
C  CALLING OF GETPXGEE HAS BEEN REMOVED.

C  SEE COMMENTS IN DO 1000 LOOP FOR EXPLANATION OF THE ACTIVITIES IN
C  LOOP 3500 BELOW.

	REWIND(27)


C  ZERO OUT TRULOG. AFTER THE SUBJECT LOOP, IT WILL BE THE SCALAR 
C  SUM(LN(P(Y(J)))) OVER J=1,NSUB,

      TRULOG=0.D0


	DO 3500 JSUB=1,NSUB

C        write (*,*) "CALL FILRED at 2786"
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT,INTLIST)
	
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


C  FOR EACH SUBJECT, AND EACH GRID POINT, CALL IDPC, A SUBROUTINIZED 

C  VERSION OF THE ADAPT PROGRAM ID3 TO CALCULATE THE SUM OF SQUARES OF 
C  DIFFERENCES BETWEEN THE OBSERVED VALUES AND PREDICTED (BY THE MODEL) 
C  VALUES (NORMALIZED BY THE ASSAY VARIANCE OF EACH OBSERVATION).

 8888   FORMAT(' ',' SUBJECT ',I5,' ...  % COMPLETED = ',F8.2)  
	XNEXT=.1D0

	DO 3800 IG=1,NGRID


C  PRINT GRID PT. AND % COMPLETED TO SCREEN EVERY .1 %.

	XPER=IG*100.D0/NGRID
	IF(XPER .GE. XNEXT) THEN
	 WRITE(*,8888) JSUB,XPER
	 XNEXT=XNEXT+.1D0
	ENDIF

C  ESTABLISH THE IGTH GRID POINT. IT IS STORED IN ROW IG OF
C  CORDEN.

	DO J=1,NVAR
	 X(J)=CORDEN(IG,J)
	END DO


C  BEFORE ALL CALLS TO IDPC, MUST INTEGRATE FIXED, RANDOM, AND
C  RANFIX VALUES INTO PX, USING IRAN(I),I=1,NVAR+NOFIX+NRANFIX. CALL 
C  MAKEVEC TO DO THIS.

      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,RANFIXEST,PX)

C      write (*,*) "Call IDPC(): N=", N

      CALL IDPC(JSUB,IG,PX,WEQ,INTLIST,RPAR,IPAR)

C wmy20190522 -- below is CALL IDPC from npag
C        CALL IDPC(JSUB,IG,NPX,PCOPY,NBCOMP,W,NOBSER,NUMEQT,
C     1    NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
C     2    INTLIST,IPAR,ObsError,RPAR,ERRFILNAME)

C  WEQ(J), J=1,NUMEQT RETURNS, WHERE W(J) IS THE SUM OVER I=1,NOBSER, OF
C  ((YO(I,J)-H(I,J))/SIG(I,J))**2, WHERE H(I,J) = PREDICTED VALUE OF THE
C  JTH OUTPUT EQ AT THE ITH OBSERVATION TIME, ASSUMING THE IGTH GRID   
C  POINT, X, ... I.E., WEQ(J) IS THE NORMALIZED SUM OF SQUARES FOR 
C  THE OBSERVATIONS OF THE JTH OUTPUT EQUATION. NOTE THAT MISSING VALUES
C  (I.E., YO(I,J) = -99) DO NOT CONTRIBUTE TO THE ABOVE SUMS OF SQUARES.

C  ADD WEQ VALUES TO GET WTOTAL = SUM OF ABOVE NORMALIZED SUMS OF 
C  SQUARES, ACROSS ALL OUTPUT EQUATIONS.

	WTOTAL = 0.D0
	DO IEQ=1,NUMEQT
	 WTOTAL = WTOTAL + WEQ(IEQ)
	END DO


C  CALCULATE P(YJ|X) FOR X-GRID POINT NO. IG. 

C  THIS NEXT TEST IS FOR THE PC. AS AN EXAMPLE, THE COMPAC COMPUTER 
C  CANNOT HANDLE ARGUMENTS TO DEXP WHICH ARE SMALLER THAN -11354. SINCE 
C  THE ARGUMENT TO DEXP BELOW IS -.5*WTOTAL, SET PYJGX = 0 IF WTOTAL IS 
C  .GT. 22708. 

        PYJGX=0.D0  
        IF(WTOTAL .LE. 22708.D0) PYJGX=DEXP(-.5D0*WTOTAL)/SIGFAC/OFAC

C  MULTIPLY P(YJ|X) BY P(X) = 1/NSUB SINCE EACH OF THE NSUB GRID POINTS 
C  ARE ASSUMED TO BE EQUALLY LIKELY. THIS WILL THEN GIVE P(YJ). PUT 
C  THIS INTO WORK(IG).


 3800   WORK(IG)=PYJGX/NSUB

C  CALCULATE PYJ = P(YJ), A SCALAR WHICH IS THE INTEGRAL OF P(YJ,X) OVER 
C  X-SPACE. 

	SUMM=0.D0
	DO IG=1,NGRID
       SUMM=SUMM+WORK(IG)
	END DO
	PYJ=SUMM

C  IF PYJ = 0 AT THIS POINT, IT'S BECAUSE WORK(IG) IS 0 IN ALL ITS
C  NGRID ENTRIES. THIS CAN OCCUR (SEE LABEL 3800) WHEN PYJGX = P(YJ|X)
C  = 0 FOR EACH OF THE NGRID X POINTS. PRINT MESSAGE TO USER IF THIS
C  OCCURS. ALSO, DISALLOW PYJ < 0 (THIS SHOULD BE IMPOSSIBLE TO OCCUR
C  BUT POSSIBLY COULD OCCUR BECAUSE OF NUMERICAL ROUNDOFF ERROR) BECAUSE
C  DLOG(PYJ) BELOW WILL BOMB IF PYJ < 0.

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

C  ADD THIS SUBJECT'S CONTRIBUTION TO TRULOG.

	TRULOG = TRULOG + DLOG(PYJ)

C  FROM LOOP 3800 ABOVE, WORK(I) = P(YJ,X(I)) --> WORK(I)/PYJ = 
C  P(X(I)|Y=YJ). ADD THIS TO SPXGYJ(I) = SUM OF P(X(I)|Y=YJ) OVER ALL
C  NSUB SUBJECTS. DO THIS FOR ALL I=1,NGRID.

C  NO. AS IF itbig7.f, SPXGYJ IS NO LONGER NEEDED SINCE IT WAS
C  ONLY USED IN THE CALCULATIONS OF THE SCALED INFO AND ENTROPY, AND
C  THESE VALUES ARE NO LONGER CALCULATED (SEE BELOW).


 3500   CONTINUE


C  AS OF itbig7.f, THE CODE FOR UPDATING CORDEN FROM SPXGYJ HAS
C  BEEN REMOVED SINCE IT WAS USED ONLY TO CALCULATE THE ENTROPY
C  AND SCALED INFO, WHICH ARE NO LONGER BEING CALCULATED (SEE BELOW).

C  AS OF itbig7.f, 'SCALED INFORMATION' CAN NO LONGER BE CALCULATED
C  SINCE THERE IS A DENOMINATOR OF log(n/N) WHERE n = NGRID AND 
C  N = NSUB, AND THESE VALUES ARE THE SAME NOW (WHICH MEANS THERE 
C  WOULD BE A DIVISION BY 0). SO ALL SCALED INFO AND ENTROPY 
C  CALCULATIONS HAVE BEEN REMOVED.

C  CALCULATE TWO MEASURES OF INFORMATION: THE AKAIKE INFORMATION 
C  CRITERION (AIC), AND THE SCHWARTZ (BAYESIAN) INFORMATION CRITERION 
C  (BIC). IN GENERAL, THE MODEL WITH THE MINIMUM AIC AND/OR
C  BIC IS THE PREFERRED MODEL.

C  AIC = 2*(-TRULOG + (P + Q)), WHERE
C	P = NO. OF PHARMACOKINETIC PARAMETERS WHICH ARE ESTIMATED IN 

C	    THIS RUN = NVAR (MEANS) + NVAR*(NVAR+1)/2 (COVARIANCES) 
C	    = (NVAR^2 + 3*NVAR)/2.

C   	Q = NO. OF NOISE PARAMETERS = INPUT AS QVAL FROM it2b102.inp.
C	    Q = SUM(Q(IEQ)), WHERE FOR OUTPUT EQUATION IEQ, Q(IEQ)
C		= 4 IF THE C'S FOR EQ. IEQ WERE ESTIMATED BY THE
C		  MAINFRAME PROGRAM, it2bdriv.exe;
C		= 0 IF THE C'S WERE NOT ESTIMATED AND GAMMA(IEQ) WAS
C		    NOT ESTIMATED IN THIS PROGRAM; 
C		= 1 IF THE C'S WERE NOT ESTIMATED AND GAMMA(IEQ) WAS
C		    ESTIMATED IN THIS PROGRAM.

C  NOTE: IF C'S FOR EQ. IEQ WERE ESTIMATED, AND GAMMA(IEQ) WAS ALSO
C        ESTIMATED, Q(IEQ) IS STILL 4 AND NOT 5 SINCE GAMMA(IEQ) IN
C        THIS CASE IS JUST A SCALE FACTOR (NOT AN INDEPENDENT VALUE).


C  BIC = 2*(-TRULOG + .5*(P + Q)*LOG(NOBTOTAL)), WHERE P AND Q ARE AS
C     ABOVE, AND NOBTOTAL IS THE TOTAL NO. OF OBSERVED VALUES OVER ALL
C     SUBJECTS (INCLUDING ALL NUMEQT OUTPUT EQUATIONS).

C  NOTE THAT THE C'S ARE CONSIDERED TO BE ESTIMATED IF THEY WERE READ
C  IN (BY THE PREPARATION PROGRAM FROM AN FILE OUTPUT BY THE MAINFRAME 
C  ESTIMATION PROGRAM (assay_2.f IS THE FIRST IN THE SERIES). THIS IS 
C  THE IQVAL=1 CASE.

C  CALCULATE NOBTOTAL = SUM OF NOBTOT(IEQ) OVER IEQ=1,NUMEQT

	NOBTOTAL = 0
	DO IEQ=1,NUMEQT
	 NOBTOTAL = NOBTOTAL + NOBTOT(IEQ)
	END DO

	PVAL = (NVAR*NVAR + 3*NVAR)/2.D0

	AIC = 2.D0*(-TRULOG + (PVAL + QVAL))
	BIC = 2.D0*(-TRULOG + .5D0*(PVAL + QVAL)*DLOG(1.D0*NOBTOTAL))

C  NOTE THAT AIC AND BIC HAVE AN EXTRA FACTOR OF 2.0 AS OF 
C  it2beng25.f AND it2beng26.f.


C  MAKE A DENSITY FILE WHICH NPAG CAN INPUT AS AN ALTERNATIVE
C  TO STARTING WITH A UNIFORM DENSITY. IT WILL CONSIST OF NSUB
C  "GRID" POINTS = THE FINAL CYCLE PARAMETER ESTIMATES FOR THE NSUB
C  SUBJECTS (AND EACH "GRID" POINT WILL HAVE THE CONTINUOUS 
C  EQUIVALENT OF 1/NSUB AS THE ASSOCIATED PROBABILITY (SEE BELOW)).

C  STORE THE DENSITY INFO INTO FILE DENFIL.

	OPEN(30,FILE=DENFIL)

C  NOTES REGARDING THE VALUES WRITTEN TO THE DENSITY FILE:

C  DORIG WILL NOT BE USED IN NPAG PROGRAM; SO SET IT = 1.
C  ICYCLE = NO. OF PREVIOUS CYCLES ALREADY RUN BY NPAG; TECHNICALLY,
C	    IT SHOULD BE SET = 0, BUT SET IT = 1, BECAUSE THE BIG
C           NPAG PROGRAM MUST SEE ICYCLE .GE. 1 IN ORDER TO KNOW THAT
C           A PRIOR DENSITY IS BEING USED.
C  NACTVE = NSUB = NO. OF ACTIVE GRID POINTS. THESE GRID POINTS WERE
C           PUT INTO CORDEN(I,J), J=1,NVAR; I=1,NSUB ABOVE.
C           THE ASSOCIATED PROBABILITY FOR EACH GRID POINT WOULD BE
C           1/NSUB IF THIS WERE TO REPRESENT A LEGITIMATE DISCRETE
C	    DENSITY, BUT NPAG EXPECTS ITS DENSITY TO SUM TO
C  	    NGRID/VOLSPA (A "CONTINUOUS" DENSITY), SO EACH ASSOCIATED
C	    PROB. WILL BE NGRID/VOLSPA/NSUB. 

C	    SO SET INDPTS ARBITRARILY = 1 --> NGRID = 2129, AND THEN SET 
C	    EACH ASSOCIATED PROB. = 2129/VOLSPA/NSUB.

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

C  NOTE THAT A TRUE DENSITY FILE MADE BY BIG NPAG WOULD HAVE MORE
C  INFO AT THIS POINT (PYJGX, ETC.) BUT THESE VALUES ARE N/A IN THIS
C  PROGRAM AND THEY ARE NOT PART OF THE PRIOR DENSITY WHICH NPAG
C  NEEDS TO START ITS ALGORITHM.
 
	CLOSE(30)

	ENDIF

C  THE ABOVE ENDIF IS FOR THE 
C  IF(ITER .EQ. MAXIT .OR. ISTOP .EQ. 1) CONDITION.


C ------ CALCULATION OF TRULOG AND RELATED STUFF ABOVE ------C ----- C


 5120   CONTINUE



C  PRINT OUT INFO FOR THIS ITERATION.

C  HOWCLOSE = HOW CLOSE TO CONVERGENCE THE PROGRAM IS, IN TERMS OF NO.
C             OF TOL'S (TOL = CONVERGENCE CRITERION).

      HOWCLOSE = (DABS(AVGLOG - OLDAVG))/TOL

C  REPLACE WRITING OF AVGLOG AND HOWCLOSE WITH XVERIFY (SEE LOGIC IN 
C  SUBROUTINE VERIFYVAL.

      XVERIFY(1) = AVGLOG
      XVERIFY(2) = HOWCLOSE
      CALL VERIFYVAL(2,XVERIFY)

C	WRITE(*,4324) AVGLOG,HOWCLOSE
      WRITE(*,4324) XVERIFY(1),XVERIFY(2)
4324   FORMAT(/' THE AVERAGE LOG-LIKELIHOOD OF THE PATIENT DATA FILES'/
     1' GIVEN THE PARAMETER ESTIMATES FOR THIS CYCLE, AND THE PRIOR '/
     2' POPULATION ESTIMATES (MEAN AND COVARIANCE), IS ',G15.6//
     3' THE CONVERGENCE INDEX IS   ',G15.6,'   (1.0 = CONVERGENCE).'//)

C  IF ITRULOG=2, THIS IS NOT THE 1ST CYCLE OR THE LAST CYCLE, SO TRULOG 
C  WAS NOT CALCULATED ABOVE. IF ITRULOG=1, THIS WAS THE LAST 
C  CYCLE, AND TRULOG WAS CALCULATED.


C  REPLACE WRITING OF TRULOG, AIC, AND BIC WITH XVERIFY (SEE LOGIC IN 
C  SUBROUTINE VERIFYVAL.

      XVERIFY(1) = TRULOG
      XVERIFY(2) = AIC
      XVERIFY(3) = BIC
      CALL VERIFYVAL(3,XVERIFY)


      IF(ITRULOG .EQ. 1 .AND. ILOG .EQ. 0) THEN
C       WRITE(*,4327) NSUB,TRULOG
        WRITE(*,4327) NSUB,XVERIFY(1)
C       WRITE(*,4331) AIC,BIC
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

C  REPLACE WRITING OF ESTMEN()/ESTMED() WITH XVERIFY (SEE LOGIC IN 
C  SUBROUTINE VERIFYVAL. SIMIARLY FOR ALL CALLS TO VERIFYVAL.

      DO I = 1,NVAR
       XVERIFY(I) = ESTMEN(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)  
    
C	WRITE(*,5103) (ESTMEN(I),I=1,NVAR)
      WRITE(*,5103) (XVERIFY(I),I=1,NVAR)

      DO I = 1,NVAR
       XVERIFY(I) = ESTMED(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)  

C	WRITE(*,5103) (ESTMED(I),I=1,NVAR)
      WRITE(*,5103) (XVERIFY(I),I=1,NVAR)


	WRITE(*,*)

	WRITE(*,5102) (PAR(I),I=1,NVAR)

C  REPLACE WRITING OF ESTCOV() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL. SIMIARLY FOR ALL CALLS TO VERIFYVAL.

      DO I=1,NVAR

       DO J = 1,I
        XVERIFY(J) = ESTCOV(I,J)
       END DO
       CALL VERIFYVAL(I,XVERIFY)

C      WRITE(*,5103) (ESTCOV(I,J),J=1,I)
       WRITE(*,5103) (XVERIFY(J),J=1,I)

      END DO


C  PRINT TO THE SCREEN THIS ITERATION'S ADDITIONAL STATISTICS.

C  IF ANY ESTCOV(I,I) .LE. 0, PARAMETER I APPARENTLY COULD NOT BE
C  ESTIMATED BY ANY OF THE SUBJECTS. IF SO, REPORT THIS TO THE USER,
C  AND THE OUTPUT FILE, AND STOP.

	DO I=1,NVAR
	 IF(ESTCOV(I,I) .LE. 0.D0) THEN
	  WRITE(*,5111) PAR(I)
	  WRITE(25,5111) PAR(I)
 5111     FORMAT(///' PARAMETER ',A11,' HAS A POPULATION VARIANCE '/
     1' ESTIMATE WHICH IS LESS THAN OR EQUAL TO 0. THE PROGRAM STOPS.'/
     2' PLEASE CHECK YOUR PATIENT DATA FILES, AND/OR RERUN WITH A '/
     3' DIFFERENT PARAMETER SELECTION.'//)

C  SEE FORMAT LABEL 7131. THE PROGRAM STOPS THERE 
C  IF ANY ONE PARAMETER CANNOT BE ESTIMATED BY ALL SUBJECTS. THAT CAN
C  HAPPEN WITHOUT ESTCOV(I,I) .LE. 0 

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

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

C  REPLACE WRITING OF STDEV() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL. SIMIARLY FOR ALL CALLS TO VERIFYVAL.

      DO I = 1,NVAR
       XVERIFY(I) = STDEV(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)  
C     WRITE(*,5103) (STDEV(I),I=1,NVAR)
      WRITE(*,5103) (XVERIFY(I),I=1,NVAR)

	WRITE(*,9041) 
 9041   FORMAT(/' THE ESTIMATE OF THE CORRELATION COEFFICIENT MATRIX'/
     1' IS, IN LOWER-TRI FORM: '/)
	WRITE(*,5102) (PAR(I),I=1,NVAR)

C  REPLACE WRITING OF CORR() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL. SIMIARLY FOR ALL CALLS TO VERIFYVAL.

      DO I=1,NVAR

       DO J = 1,I
        XVERIFY(J) = CORR(I,J)
       END DO
       CALL VERIFYVAL(I,XVERIFY)

C      WRITE(*,5103) (CORR(I,J),J=1,I)
       WRITE(*,5103) (XVERIFY(J),J=1,I)

      END DO


      WRITE(*,5128)
 5128   FORMAT(/' THE ESTIMATE OF THE % COEFFICIENTS OF VARIATIONS '/
     1' FOR THE VARIABLES ARE:'/)

      WRITE(*,5102) (PAR(I),I=1,NVAR)

C  REPLACE WRITING OF % COF. OF. VARIATION WITH XVERIFY (SEE LOGIC IN 
C  SUBROUTINE VERIFYVAL. SIMIARLY FOR ALL CALLS TO VERIFYVAL.

      DO I = 1,NVAR
       XVERIFY(I) = 100*DSQRT(ESTCOV(I,I))/ESTMEN(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)  
    
C     WRITE(*,5103) ((100*DSQRT(ESTCOV(I,I))/ESTMEN(I)),I=1,NVAR)
      WRITE(*,5103) (XVERIFY(I),I=1,NVAR)


	 WRITE(*,5117) NUMEQT,NUMEQT
 5117  FORMAT(//' EACH OF THE ',I1,' OUTPUT EQUATION(S) HAS ASSOCIATED'/
     1' A UNIQUE GAMMA, WHERE THE ASSAY ERROR S.D. FOR THAT OUTPUT IS '/
     2' S.D. = GAMMA*(C0+C1*Y+C2*Y^2+C3*Y^3). THE UPDATED'/
     2' ESTIMATE(S) FOR THE ',I1,' GAMMA(S) FOLLOW(S): ')

	 DO I=1,NUMEQT
	  WRITE(*,*) GAMMA(I)
	 END DO


C  REPLACE WRITING OF AVGLOG AND HOWCLOSE WITH XVERIFY (SEE LOGIC IN 
C  SUBROUTINE VERIFYVAL.

      XVERIFY(1) = AVGLOG
      XVERIFY(2) = HOWCLOSE
      CALL VERIFYVAL(2,XVERIFY)
C     WRITE(25,4324) AVGLOG,HOWCLOSE
      WRITE(25,4324) XVERIFY(1),XVERIFY(2)


C  REPLACE WRITING OF TRULOG, AIC, AND BIC WITH XVERIFY (SEE LOGIC IN 
C  SUBROUTINE VERIFYVAL.

      XVERIFY(1) = TRULOG
      XVERIFY(2) = AIC
      XVERIFY(3) = BIC
      CALL VERIFYVAL(3,XVERIFY)

      IF(ITRULOG .EQ. 1 .AND. ILOG .EQ. 0) THEN
C       WRITE(25,4327) NSUB,TRULOG
        WRITE(25,4327) NSUB,XVERIFY(1)
C       WRITE(25,4331) AIC,BIC
        WRITE(25,4331) XVERIFY(2),XVERIFY(3)
      ENDIF


	WRITE(25,5104) NVAR
	WRITE(25,5102) (PAR(I),I=1,NVAR)

C  REPLACE WRITING OF ESTMEN()/ESTMED() WITH XVERIFY (SEE LOGIC IN 
C  SUBROUTINE VERIFYVAL. SIMIARLY FOR ALL CALLS TO VERIFYVAL.

      DO I = 1,NVAR
       XVERIFY(I) = ESTMEN(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)  
    
C	WRITE(25,5103) (ESTMEN(I),I=1,NVAR)
      WRITE(25,5103) (XVERIFY(I),I=1,NVAR)

      DO I = 1,NVAR
       XVERIFY(I) = ESTMED(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)  

C	WRITE(25,5103) (ESTMED(I),I=1,NVAR)
      WRITE(25,5103) (XVERIFY(I),I=1,NVAR)



	WRITE(25,*)
	WRITE(25,5102) (PAR(I),I=1,NVAR)


C  REPLACE WRITING OF ESTCOV() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL. SIMIARLY FOR ALL CALLS TO VERIFYVAL.

      DO I=1,NVAR

       DO J = 1,I
        XVERIFY(J) = ESTCOV(I,J)
       END DO
       CALL VERIFYVAL(I,XVERIFY)

C      WRITE(25,5103) (ESTCOV(I,J),J=1,I)
       WRITE(25,5103) (XVERIFY(J),J=1,I)

      END DO


C  PRINT TO THE OUTPUT FILE THIS ITERATION'S ADDITIONAL STATISTICS.

	WRITE(25,9042) 
	WRITE(25,5102) (PAR(I),I=1,NVAR)

C  REPLACE WRITING OF STDEV() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL. SIMIARLY FOR ALL CALLS TO VERIFYVAL.

      DO I = 1,NVAR
       XVERIFY(I) = STDEV(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)  
C     WRITE(25,5103) (STDEV(I),I=1,NVAR)
      WRITE(25,5103) (XVERIFY(I),I=1,NVAR)

	WRITE(25,9041)
	WRITE(25,5102) (PAR(I),I=1,NVAR)

C  REPLACE WRITING OF CORR() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL. SIMIARLY FOR ALL CALLS TO VERIFYVAL.

      DO I=1,NVAR

       DO J = 1,I
        XVERIFY(J) = CORR(I,J)
       END DO
       CALL VERIFYVAL(I,XVERIFY)

C      WRITE(25,5103) (CORR(I,J),J=1,I)
       WRITE(25,5103) (XVERIFY(J),J=1,I)

      END DO


	WRITE(25,5128)
	WRITE(25,5102) (PAR(I),I=1,NVAR)

C  REPLACE WRITING OF % COF. OF. VARIATION WITH XVERIFY (SEE LOGIC IN 
C  SUBROUTINE VERIFYVAL. SIMIARLY FOR ALL CALLS TO VERIFYVAL.

      DO I = 1,NVAR
       XVERIFY(I) = 100*DSQRT(ESTCOV(I,I))/ESTMEN(I)
      END DO
      CALL VERIFYVAL(NVAR,XVERIFY)  
    
C     WRITE(25,5103) ((100*DSQRT(ESTCOV(I,I))/ESTMEN(I)),I=1,NVAR)
      WRITE(25,5103) (XVERIFY(I),I=1,NVAR)


	 WRITE(25,5117) NUMEQT,NUMEQT
	 DO I=1,NUMEQT
	  WRITE(25,*) GAMMA(I)
	 END DO



      IF(NRANFIX .GT. 0 .AND. ITER .EQ. 1) THEN


C  IF NRANFIX .GT. 0, CALL ELDERY TO GET UPDATED ESTIMATES FOR THESE
C  NRANFIX PARAMETERS WHICH ARE UNKNOWN BUT THE SAME FOR ALL SUBJECTS.
C  ALSO INCLUDE THE NVAR RANDOM VARIABLES AS PARAMETERS WHOSE NEW 
C  ESTIMATES WILL BE FOUND BY ELDERY. THE INITIAL ESTIMATES FOR THESE 
C  PARAMETERS WILL BE THEIR CURRENT ITERATION MEANS. NOTE THAT THIS WILL
C  ONLY BE DONE AT END OF ITER NO. 1.

C  PREPARE TO CALL ELDERY. 


      DO I = 1,NVAR
       X(I) = ESTMEN(I)
      END DO


      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,RANFIXEST,PX)

C  NOTE THAT PX NOW INCLUDES THE VALUES OF THE RANDOM PARAMETERS (SET
C  = TO THEIR MEANS FROM THE JUST COMPLETED ITERATION) AND THE FIXED 
C  PARAMETER VALUES. AND IT ALSO HAS THE VALUES IN RANFIXEST(.) IN THE
C  APPROPRIATE NRANFIX ENTRIES (I.E., FOR THOSE PARAMETERS WITH 
C  IRAN(.) = 2), BUT THESE LAST VALUES OF COURSE WILL BE RESET IN 
C  SUBROUTINE CALCRF EACH TIME IT IS CALLED BY ELDERY WITH ANOTHER SET 
C  OF VALUES SUPPLIED IN THE CANDIDATE VECTOR, VEC(.). AND, THE NVAR 
C  PARAMETER VALUES FOR THE RANDOM VARIABLES WILL ALSO BE RESET IN 
C  SUBROUTINE CALCRF TO THEIR CANDIDATE VALUES FROM VEC(.).

C  TO START THE PROCESS TO FIND THE BEST ESTIMATES FOR THE NRANFIX
C  PARAMETERS WITH IRAN(.) = 2, SINCE THE CURRENT ESTIMATES FOR THESE 
C  PARAMETERS ARE IN RANFIXEST(.), I=1,NRANFIX, THESE WILL BE THE 
C  STARTING ESTIMATES FOR THIS CALL TO ELDERY.

C  ALSO, THE INITIAL ESTIMATES FOR THE NVAR RANDOM VARIABLES WILL BE 
C  THE MEANS FROM THE JUST COMPLETED ITERATION.

       DO I = 1,NRANFIX
        START(I) = RANFIXEST(I)
        STEP(I) = -.2D0*START(I)
       END DO

       DO I = NRANFIX+1,NRANFIX+NVAR
        START(I) = ESTMEN(I-NRANFIX)
        STEP(I) = -.2D0*START(I)
       END DO

       CALL ELDERY2(NRANFIX+NVAR,START,OPTVAR,VALMIN,1.D-10,STEP,1000,
     1  CALCRF,0,ICONV,NITER,ICNT,NUMEQT,YO,C0,C1,C2,C3,GAMMA,JSUB,IG,
     2  INTLIST)

C  OPTVAR(I),I=1,NRANFIX+NVAR = THE UPDATED SET OF ESTIMATES FOR THE
C    NRANFIX PARAMETERS WITH IRAN(.) = 2, AND THE NVAR PARAMETERS WITH
C    IRAN(.) = 1 (SEE ABOVE). 

C  VALMIN = MIN. VALUE OF FUNCTION ACHIEVED.

C  ICONV = 1 IF THE ESTIMATE CONVERGED; 0 OTHERWISE.


	IF(ICONV .EQ. 0) WRITE(*,9021) 
	IF(ICONV .EQ. 0) WRITE(25,9021) 
 9021 FORMAT(' ',' NO CONVERGENCE THIS CYCLE ON ESTIMATES FOR THE'/
     1' RANFIX AND RANDOM PARAMETERS. '/)
 
C  ESTABLISH THE NEW SET OF RANFIX AND RANDOM VARIABLES. 

      DO I = 1,NRANFIX
       RANFIXEST(I) = OPTVAR(I)
      END DO

      DO I = NRANFIX+1,NRANFIX+NVAR
       ESTMENO(I-NRANFIX) = OPTVAR(I)
      END DO


C  NOTE THAT BEFORE THIS OPTIMIZATION, THE POPULATION MEAN ESTIMATE WAS 
C  ESTMEN(.). NOW IT IS ESTMENO(.). SO NOW MUST RECALCULATE THE
C  CORRESPONDING CHANGED POPULATION COVARIANCE MATRIX, WHICH CORRESPONDS
C  TO ESTMENO(.) RATHER THAN ESTMEN(.). THEN SET ESTMEN(.) = ESTMENO(.).

C  THIS MEANS REDOING THE CODE JUST ABOVE, AND IN LOOP 1500 ABOVE (SEE
C  THAT SECTION FOR COMMENTS).


      DO JSUB=1,NSUB	
       DO J=1,NVAR
        PAREST(JSUB,J) = PAREST(JSUB,J) * ESTMENO(J)/ESTMEN(J) 
       END DO
      END DO

C  SUMCOV BELOW IS THE RUNNING SUM OF THE NUMERATOR OF ESTCOV.

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

C       write (*,*) "CALL FILRED at 3530"
       CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT,INTLIST)
	
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

C  FIND THE ESTIMATE OF THE INVERSE OF THE COVARIANCE MATRIX OF THETA.
C  IT WILL BE CALLED COVINV. SEE DETAILS BELOW LOOP 140 ABOUT THE
C  CALCULATION OF PMAT AND COVINV = INV(COVEST).

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
C  THE ABOVE END DO IS FOR THE  DO I = 1,NVAR  LOOP.


 4500 CONTINUE

      DO I=1,NVAR
       DO J=1,NVAR
        ESTCOV(I,J) = SUMCOV(I,J)/NSUB
       END DO
      END DO

      DO J = 1,NVAR
       ESTMEN(J) = ESTMENO(J)
      END DO


C  WRITE THE ESTIMATES FOR THESE NRANFIX RANFIX PARAMETERS FOR THIS 
C  CYCLE, AS WELL AS FOR THE REVISED NVAR RANDOM PARAMETERS.


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
C  ABOVE ENDIF IS FOR THE  IF(NRANFIX .GT. 0 .AND. NRANFIX .EQ. 1)
C    CONDITION.




C ---- CALC. BELOW OF PREDICTED VALUES AND MAP BAYESIAN ESTIMATES ---- C
C --------------------- IF THIS IS THE LAST CYCLE -------------------- C

C  THIS IS THE LAST ITERATION IF ITER = MAXIT, OR IF 
C  DABS(AVGLOG - OLDAVG) .LT. TOL (IN WHICH CASE ISTOP WAS SET = 1
C  ABOVE).

	IF(ITER .EQ. MAXIT .OR. ISTOP .EQ. 1) THEN


C  ESTABLISH THREE ARRAYS, YPREDPOP, YPREDBAY, AND PARBAY, WHICH WILL 
C  BE STORED INTO PARFIL FOR USE BY THE PC PROGRAM. 

C  YPREDPOP(MAXSUB,NUMEQT,MAXOBDIM,2): YPREDPOP(JSUB,IEQ,IOBS,ICEN) = 
C  THE PREDICTED VALUE FOR Y FOR SUBJECT JSUB, OUTPUT EQ. IEQ, 
C  OBSERVATION IOBS, AND ICEN = 1 (MEANS) OR 2 (MEDIANS), WHERE THE 
C  MEANS AND MEDIANS ARE FROM THE FINAL CYCLE POPULATION ESTIMATES.


C  YPREDBAY(MAXSUB,NUMEQT,MAXOBDIM,2): YPREDBAY(JSUB,IEQ,IOBS,ICEN) = 
C  THE PREDICTED VALUE FOR Y FOR SUBJECT JSUB, OUTPUT EQ. IEQ, 
C  OBSERVATION IOBS, AND ASSUMING THE MAP BAYESIAN PARAMETER ESTIMATES 
C  FOR SUBJECT JSUB, USING EITHER THE MEANS (ICEN=1) OR THE MEDIANS 
C  (ICEN=2) OF THE FINAL CYCLE AS THE POPULATION PRIOR PARAMETER 
C  ESTIMATES.

C  PARBAY(MAXSUB,2,30): PARBAY(JSUB,ICENTER,J) = THE MAP 
C  BAYESIAN PARAMETER ESTIMATE, USING THE POPULATION ESTIMATES FROM THE 

C  FINAL CYCLE (ICENTER=1 --> MEANS; ICENTER=2 --> MEDIANS) FOR SUBJECT 
C  JSUB FOR PARAMETER J.


C 1ST CALCULATE YPREDPOP VALUES.

	REWIND(27)

C  FOR EACH SUBJECT, FIND THE PREDICTED VALUES (VIA SUBROUTINE IDCALCY).

C  NOTE THAT THE LAST CYCLE'S MEANS ARE IN   ESTMEN(I),I=1,NVAR;     
C                             MEDIANS ARE IN ESTMED(I),I=1,NVAR;     
      

	DO 6000 JSUB=1,NSUB
          IG=JSUB

C         write (*,*) "CALL FILRED at 3774"
	 CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT,INTLIST)

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

	
C  CALL SUBROUTINE IDCALCY, A VERSION OF THE ID PROGRAM WHICH SIMPLY
C  CALCULATES THE PREDICTED VALUES OF YO(I,J) = OUTPUT CONCENTRATION OF
C  THE JTH OUTPUT EQUATION (J=1,NUMEQT) AT THE ITH OBSERVATION TIME
C  (I=1,NOBSER), ASSUMING THE, ASSUMING THE PARAMETER VECTOR THETA.

C  BEFORE ALL CALLS TO IDCALCY, MUST INTEGRATE FIXED, RANDOM, AND
C  RANFIX VALUES INTO PX, USING IRAN(I),I=1,NVAR+NOFIX+NRANFIX. CALL 
C  MAKEVEC TO DO THIS.

	CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,THETA,VALFIX,RANFIXEST,PX)
	CALL IDCALCY(JSUB,IG,NVAR+NOFIX+NRANFIX,NDIM,PX,YPRED,
     1      NUMEQT,INTLIST,RPAR,IPAR)

C  NOTE: PREDICTED VALUES WERE FOUND EVEN FOR OBSERVED LEVELS WHICH ARE 
C  MISSING (I.E., OBSERVED LEVEL = -99) SINCE IT IS EASIER TO CALCULATE 
C  ALL PREDICTED VALUES THAN TO KEEP TRACK OF WHICH DO AND WHICH DON'T 
C  NEED TO BE CALCULATED.

C  STORE YPRED INTO YPREDPOP(JSUB,.,.,ICENTER)

	DO IOBS=1,NOBSER

	 DO IEQ=1,NUMEQT	
	 YPREDPOP(JSUB,IEQ,IOBS,ICENTER) = YPRED(IOBS,IEQ)

C         write (*,*) "DO 6000 Is this NaN?",JSUB,IEQ,IOBS,ICENTER,
C     1     YPREDPOP(JSUB,IEQ,IOBS,ICENTER),YPRED(IOBS,IEQ)

	 END DO
	END DO

	END DO

C  THE ABOVE END DO IS FOR THE   DO ICENTER = 1,2  LOOP.


 6000   CONTINUE


C NEXT CALCULATE PARBAY AND YPREDBAY.

C  FOR EACH SUBJECT IN TURN, FIND THE MAP BAYESIAN PARAMETER ESTIMATES
C  GIVEN THE FINAL CYCLE POPULATION COVARIANCE MATRIX AND THE FINAL 
C  CYCLE MEANS (ICENTER=1) OR MEDIANS (ICENTER=2) AS THE PRIOR.

C  THEN FIND THE PREDICTED VALUES BASED ON THE MEANS (ICENTER=1) AND
C  THE MEDIANS (ICENTER=2) .

C  NOTE THAT IF ICENTER=1 BELOW, ESTMEN DOES NOT HAVE TO BE ESTABLISHED,
C  SINCE IT ALREADY CONTAINS THE LAST CYCLE POPULATION MEANS. IF
C  ICENTER=2, HOWEVER, MUST STORE LAST CYCLE POPULATION MEDIANS (ESTMED)
C  INTO ESTMEN BECAUSE IT IS ESTMEN WHICH IS PASSED TO SUBROUTINE 
C  MAPBAYS VIA COMMON/TOMAP


	WRITE(*,2215)
 2215   FORMAT(/' SUBJECT NUMBERS FOLLOW AS THE MAP BAYESIAN PARAMETER'/
     1' ESTIMATES ARE BEING FOUND -- ASSUMING A PRIOR FIRST OF THE'/
     2' FINAL CYCLE POPULATION MEANS, AND THEN OF THE FINAL CYCLE'/
     3' POPULATION MEDIANS: '/) 


	DO 9000 ICENTER = 1,2


C  NOTE THAT THE ARRAY ESTMEN IS USED IN THE START VECTOR (SUPPLIED TO
C  ELDERY BELOW) AS WELL AS IN COMMON/TOMAP (PASSED TO SUBROUTINE 
C  MAPBAYS, WHICH IS CALLED BY ELDERY). IF ICENTER=1, ESTMEN ALREADY
C  HOLDS THE FINAL CYCLE POPULATION MEANS, SO NOTHING NEEDS TO BE DONE.
C  FOR ICENTER=2, HOWEVER, MUST STORE THE FINAL CYCLE POPULATION MEDIANS
C  INTO ESTMEN (NOTE THAT THE FINAL CYCLE POPULATION MEANS WILL NOT BE
C  NEEDED AGAIN, SO THEY DON'T HAVE TO BE SAVED PRIOR TO OVERWRITING
C  ESTMEN WITH ESTMED).

	  IF(ICENTER .EQ. 2) THEN
	   DO J=1,NVAR
	    ESTMEN(J) = ESTMED(J) 
	   END DO
	  ENDIF

	REWIND(27)

C  NOTE THAT ESTINV ALREADY CONTAINS THE INVERSE OF THE FINAL CYCLE
C  POPULATION COV. MATRIX, AND DET IS THE DETERMINANT OF THE COV.
C  MATRIX (FROM THE CALCULATION OF TRULOG ABOVE). SO THEY DON'T HAVE

C  TO BE ESTABLISHED BY CALLING MATNV2.


C  NO! AS OF itbig7.f, TRULOG IS NOT A FUNCTION OF ESTCOV, AND SO THE
C  CALCULATION OF ESTINV FROM THE LAST CYCLE'S ESTCOV HAS NOT BEEN
C  DONE YET. SO DO IT NOW.
C  (NOTE THAT THIS BUG WAS NOT FIXED UNTIL it2beng15.f).

C  FIND ESTINV = INV(ESTCOV).

C  STORE ESTCOV INTO ESTINV. ON RETURN FROM SUBROUTINE MATNV2, ESTINV
C  WILL BE INV(ESTCOV).

	DO I=1,NVAR
	 DO J=1,NVAR
	  ESTINV(I,J) = ESTCOV(I,J)
	 END DO
	END DO

 	CALL MATNV2(ESTINV,NVAR,B,1,DET)

C  NOTE THAT B IS A WORK VECTOR, AND DET IS THE DETERMINANT OF ESTCOV.

	IF(DET .LE. 0) THEN

C  NOTE THAT IF DET .LE. 0 --> THE PROGRAM WILL 
C  BOMB WHEN MAPBAYS IS CALLED BY ELDER BELOW (SINCE MAPBAYS HAS A 
C  LOG(DET) IN IT). TO PREVENT THIS, PRINT MESSAGE TO USER AND STOP.
 
	 WRITE(*,2216)
	 WRITE(25,2216) 
 2216  FORMAT(/' THE FINAL CYCLE POPULATION COV. ESTIMATE IS SINGULAR.'/
     1' THIS INDICATES AN ILL-CONDTIONED PROBLEM, OR POSSIBLY AN'/
     2' OVER-PARAMETERIZED PROBLEM (I.E., FEWER OBSERVED VALUES THAN'/
     3' PARAMETERS TO BE ESTIMATED). PLEASE RE-EXAMINE YOUR PATIENT'/
     4' DATA FILES, AND YOUR INPUT INSTRUCTIONS, CORRECT ANY'/
     5' INCONSISTENCIES, AND THEN RERUN THE PROGRAM.'//)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

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
C        write (*,*) "Oops, might be here: CALL FILRED at 3888"
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT,INTLIST)

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

C  ESTABLISH PAREST VALUES FOR THOSE (JSUB,J) COMBOS WHICH WERE SET
C  = 0 ABOVE, BECAUSE IESTIJ(JSUB,J) = 0.

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


C  STORE THE PARAMETER ESTIMATES TO ARRAY PARBAY.

	DO JSUB=1,NSUB
	 DO J=1,NVAR
	  PARBAY(JSUB,ICENTER,J) = PAREST(JSUB,J)
	 END DO
	END DO


C  NOW FIND THE PREDICTED VALUES.

	REWIND(27)

C  FOR EACH SUBJECT, IN TURN, READ THE OBSERVED VALUES AND THEN FIND THE
C  PREDICTED VALUES (VIA SUBROUTINE IDCALCY).

	DO 8000 JSUB=1,NSUB
          IG=JSUB

C        write (*,*) "CALL FILRED at 3976"
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT,INTLIST)

C  FROM LOOP 7000 (AND CODE BELOW), THE PARAMETER ESTIMATES OF THIS 
C  SUBJECT ARE STORED IN PAREST(JSUB,.).

	DO J=1,NVAR
	 THETA(J) = PAREST(JSUB,J)
	END DO

C  CALL SUBROUTINE IDCALCY, A VERSION OF THE ID PROGRAM WHICH SIMPLY
C  CALCULATES THE PREDICTED VALUES OF YO(I,J) = OUTPUT CONCENTRATION OF 
C  THE JTH OUTPUT EQUATION (J=1,NUMEQT) AT THE ITH OBSERVATION TIME
C  (I=1,NOBSER), ASSUMING THE PARAMETER VECTOR THETA.

C  BEFORE ALL CALLS TO IDCALCY, MUST INTEGRATE FIXED, RANDOM, AND
C  RANFIX VALUES INTO PX, USING IRAN(I),I=1,NVAR+NOFIX+NRANFIX. CALL 
C  MAKEVEC TO DO THIS.

	CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,THETA,VALFIX,RANFIXEST,PX)
	CALL IDCALCY(JSUB,IG,NVAR+NOFIX+NRANFIX,NDIM,PX,YPRED,
     1    NUMEQT,INTLIST,RPAR,IPAR)

C  NOTE: PREDICTED VALUES WERE FOUND EVEN FOR OBSERVED LEVELS WHICH ARE 
C  MISSING (I.E., OBSERVED LEVEL = -99) SINCE IT IS EASIER TO CALCULATE 
C  ALL PREDICTED VALUES THAN TO KEEP TRACK OF WHICH DO AND WHICH DON'T 
C  NEED TO BE CALCULATED.

C  STORE YPRED INTO YPREDBAY(JSUB,.,.,ICENTER)

	DO IOBS=1,NOBSER
	 DO IEQ=1,NUMEQT	
	 YPREDBAY(JSUB,IEQ,IOBS,ICENTER) = YPRED(IOBS,IEQ)
	 END DO
	END DO


 8000   CONTINUE


 9000   CONTINUE

	

C  PUT YPREDPOP, YPREDBAY, AND PARBAY INTO FILE 28. BUT FIRST PUT IN
C  SUMMARY STATISTICS.

C  THE SUMMARY INFO HAD TO BE WRITTEN TO FILE 28 AFTER THE CURRENT 
C  BOUNDARIES, AB(.,.), WERE FOUND ABOVE (DURING CALC. OF TRULOG).

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


C  REPLACE WRITING OF C0P(),...,C3P() WITH XVERIFY (SEE LOGIC IN 
C  SUBROUTINE VERIFYVAL.

	WRITE(28,2218) NUMEQT

	DO IEQ = 1,NUMEQT
       XVERIFY(1) = C0P(IEQ)
       XVERIFY(2) = C1P(IEQ)
       XVERIFY(3) = C2P(IEQ)
       XVERIFY(4) = C3P(IEQ)
       CALL VERIFYVAL(4,XVERIFY)
C	 WRITE(28,162) IEQ,C0P(IEQ),C1P(IEQ),C2P(IEQ),C3P(IEQ)
	 WRITE(28,162) IEQ,(XVERIFY(IXV),IXV=1,4)
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
C       WRITE(28,5309) PARFIX(I),VALFIX(I)
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
C         write (*,*) "CALL FILRED at 4127"
	 CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT,INTLIST)
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
C         write (*,*) "CALL FILRED at 4182"
	 CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT,INTLIST)
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

C  THE ABOVE ENDIF IS FOR THE  
C  IF(ITER .EQ. MAXIT .OR. ISTOP .EQ. 1)  CONDITION.



C  STORE PROGRAM DATA TO BE READ IN BY THE 2ND PART OF THIS 2-PART
C  BATCH PROGRAM, THE POPULATION ANALYZER.

C  NOTE: AT THE TOP OF THIS PROGRAM, FROMFIL WAS OPENED, '-1'
C	 WAS PUT INTO THE TOP LINE, AND THE FILE WAS THEN CLOSED. THIS
C	 WAS DONE IN CASE THIS PROGRAM TERMINATED BEFORE THE END OF
C        THE 1ST ITERATION. 

C	 BUT SINCE THE PROGRAM HAS COMPLETED AT LEAST ONE ITERATION,
C        FROMFIL WILL NOW BE OPENED AND FILLED WITH USEFUL 
C        INFO FOR THE 2ND PART OF THE 2-PART BATCH PROGRAM. THE TOP 
C        LINE WILL HAVE '1', WHICH MEANS 'NORMAL' TERMINATION (AND THE 

C        DATA FILES ARE IN ADAPT FORMAT). THE 2ND LINE WILL HAVE THE

C 	 VERSION CODE FOR THIS PROGRAM.

        OPEN(24,FILE=FROMFIL)
         IONE = 1

         WRITE(24,*) IONE

         WRITE(24,7123) 
 7123	   FORMAT('REM_FRN MAR_16 ... made by it2branfix1.f')

         WRITE(24,2) CSVFILE
         WRITE(24,*) NSUBTOT   
         WRITE(24,*) NSUB

C  CALL SUBROUTINE WRITEPT2 TO WRITE THE PATIENT NOS. USED IN 
C  THE ANALYSIS TO FILE 24 ... IN AN "EFFICIENT" WAY, AS OPPOSED TO
C  ONE INDEX PER LINE. NOTE THAT THE FIRST ARGUMENT TELLS WRITEPT2
C  TO WRITE TO FILE 24. NOTE THAT THE '    0' AFTER THE CALL TO
C  WRITEPT2 TELLS THE PROGRAM READING THIS FILE THAT THE PATIENT
C  NOS. HAVE ENDED.

         CALL WRITEPT2(24,NSUB,IPATVEC)
         WRITE(24,*) '    0'

         IF(NOFIX .GT. 0) WRITE(24,*) (VALFIX(I),I=1,NOFIX)
         IF(NRANFIX .GT. 0) WRITE(24,*) (RANFIXEST(I),I=1,NRANFIX)

         DO I=1,NVAR
          WRITE(24,*) (AB(I,J),J=1,2)
         END DO



C  AS OF itbig1.f, THE ORIGINAL ASSAY C'S ARE WRITTEN TO THIS FILE,
C  ALONG WITH THE GAMMA(S). THEN, WHEN RUNNING NPAG, THE USER CAN DECIDE
C  WHETHER TO MULTIPLY THESE GENERAL C'S BY THE GAMMA(S) (AS WELL AS THE 
C  C'S IN THE INDIVIDUAL PATIENT FILES).

         WRITE(24,*) NUMEQT
         DO IEQ = 1,NUMEQT
          WRITE(24,*) GAMMA(IEQ)
          WRITE(24,*) C0P(IEQ),C1P(IEQ),C2P(IEQ),C3P(IEQ)
         END DO
   
         WRITE(24,*) NDRUG
         WRITE(24,*) (AF(I),I=1,NDRUG)

         CLOSE(24)



C  IF MAXIT ITERATION HAVE BEEN PERFORMED (DO LOOP 2000), OR IF THE 
C  AVERAGE LOG-LIK DID NOT INCREASE BY AT LEAST TOL, STOP THE ANALYSIS; 
C  OTHERWISE, CONTINUE.

	IF(DABS(AVGLOG - OLDAVG) .LT. TOL) GO TO 2500

	OLDAVG=AVGLOG


 2000   CONTINUE


 2500   CONTINUE


C  AS OF it2branfix1.f, WRITE THE ESTIMATES FOR THE PARAMETERS WHICH ARE
C  UNKNOWN BUT THE SAME FOR ALL SUBJECTS, IF APPLICABLE. NOTE THAT THESE
C  ESTIMATES WERE OBTAINED ONLY AFTER CYCLE NO. 2, AND ARE NOW WRITTEN
C  OUT AGAIN AT THE END OF THE RUN.
 
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
C  THE ABOVE ENDIF IS FOR THE  IF(NRANFIX .GT. 0)  CONDITION.




C  WRITE OUT TO THE SCREEN AND OUTPUT FILE 25 THE BOUNDARIES WHICH
C  WILL BE USED FOR THE 2ND PART OF THIS TWO-PART BATCH PROGRAM. 

	WRITE(*,*)
	WRITE(*,*)
	WRITE(*,*)'THE RANGES TO BE CONSIDERED FOR THE NPAG POPULATION'
	WRITE(*,*)'PROGRAM ARE AS FOLLOWS:' 
	WRITE(*,*)

C  REPLACE WRITING OF AB() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL. SIMIARLY FOR ALL CALLS TO VERIFYVAL.


      DO I=1,NVAR
       XVERIFY(1) = AB(I,1)
       XVERIFY(2) = AB(I,2)
       CALL VERIFYVAL(2,XVERIFY)
C      WRITE(*,341) PAR(I),(AB(I,J),J=1,2)

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
C      WRITE(25,341) PAR(I),(AB(I,J),J=1,2)
       WRITE(25,341) PAR(I),XVERIFY(1),XVERIFY(2)
      END DO


C  NOTE THAT IF ISTOP = 0, THE RUN MUST HAVE STOPPED BECAUSE THE 
C  MAXIMUM NO. OF CYCLES WAS RUN. IF ISTOP = 1, THE RUN COULD HAVE
C  STOPPED BECAUSE THE CONVERGENCE CRITERION WAS MET BEFORE THE
C  MAX. NO. OF CYCLES WAS RUN (FORMAT 5198), OR AT THE LAST CYCLE
C  (FORMAT 5199).

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


C  CONSTRUCT THE COMBINED OUTPUT FILE, TO BE CALLED 'OUTF'//NAME, WHERE
C  NAME WAS OBTAINED ABOVE, AND IS THE CHARACTER*4 EQUIVALENT TO INUM

C  AS OF itbig9.f, CREATE A COMBINED OUTPUT FILE.
C  NOTE THAT FILE 25 IS THE REGULAR OUTPUT FILE, AND SCRATCH FILE 27 
C  HAS THE PATIENT DATA PART OF it2b102.inp. ALSO, WILL NEED TO 
C  REOPEN PARFIL AND it2bdriv.f TO PUT THEM INTO THE COMBINED FILE.
C  THE FORMAT OF THE COMBINED FILE WILL BE COMPATIBLE WITH THE CODE
C  IN SUBROUTINE EXTRAOPT OF THE PC PREP/ANALYSIS PROGRAM (CURRENTLY,
C  ITBIG8.FOR).

C wmy20190523 -- BUG -- PARFIL is NULL here, so
C OPEN(28,FILE=PARFIL) below crashes program.
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
     2'************ LAST CYCLE PARAMETER INFORMATION FILE **********'/)

	write(*,*)' Writing parameter info to combined output file ...'

C  PUT THE LAST CYCLE PARAMETER INFO FILE (PARFIL) INFO INTO THE 
C  COMBINED OUTPUT FILE. 

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

C  PUT THE "ACTIVE" PATIENT DATA INFO INTO THE COMBINED OUTPUT FILE. 

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

C  PUT it2bdriv.f INTO THE COMBINED OUTPUT FILE. 



 1170   READ(29,2717,IOSTAT=IEND) READLARG
        IF(IEND .LT. 0) GO TO 1180
        CALL CONDENSE(READLARG)
        GO TO 1170

 1180   WRITE(26,1181)
 1181   FORMAT(/'***************** END OF THE it2bdriv.f FILE **********
     1*******'/)


C  As of it2beng22.f, don't close files 25,28,27,29. THEY WILL BE REUSED

C  BY SUBROUTINE READOUT IN readi07.f. Just rewind them, except for file
C  28 which is supposed be left open at its end since that is what
C  readi07.f (SUBROUTINE READOUT) expects. Also, don't close file 26
C  since it will also be read by Subroutine READOUT.

      REWIND(25)
      REWIND(27)
C     REWIND(28) ... THIS FILE TO BE LEFT OPENED AT ITS END (see above).
      REWIND(29)
c     CLOSE(26)

C  CALL READOUT TO FORM (FROM FILES 26,25,27,28,29) THE SUMMARIZED
C  OUTPUT FILE, IT_RFxxxx.IT, WHICH IS COMPATIBLE WITH THE R GRAPHICS
C  PACKAGE.

        OUTFILER = 'IT_RF'//NAME//'.TXT'

        CALL READOUT(OUTFILER,IRAN)

        STOP
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE ELDERY(N,START,XMIN,YNEWLO,REQMIN,STEP,
     X  ITMAX,FUNC,IPRINT,ICONV,NITER,ICOUNT,JSUB,IG,INTLIST,RPAR,IPAR)

C
C wmy20190520 -- BUG In DO 1000 -> SIGSEGV, Called with:
C
C NVAR,START,THETA,VALMIN,1.D-10,STEP,1000,MAPBAYS,
C     1  0,ICONV,NITER,ICNT,JSUB,IG,INTLIST,RPAR,IPAR

C  ELDERY DIFFERS FROM ELDERX ONLY IN THE DIMENSION STATEMENT. ALL 5'S
C  ARE CHANGED TO 25'S, AND ALL 6'S ARE CHANGED TO 26'S. THIS ALLOWS 25
C  PARAMETERS INSTEAD OF JUST 5. As of itbig9x.f, we allow as many as
C  30 parameters.

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
C    XSEC  -THE COORDINATES OF THE VETEX WITH THE 2ND SMALLEST FUNCTION
C             VALUE.
C    YSEC  - THE FUNCTION VALUE AT XSEC.
C
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
        XN=FLOAT(N)
        DN=FLOAT(N)
        NN=N+1
C
C  CONSTRUCTION OF INITIAL SIMPLEX.
C
1001    DO 1 I=1,N
1       P(I,NN)=START(I)
        CALL FUNC(N,START,FN,JSUB,IG,INTLIST,RPAR,IPAR)
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
        CALL FUNC(N,START,FN,JSUB,IG,INTLIST,RPAR,IPAR)
        Y(J)=FN
        ICOUNT=ICOUNT+1
2       START(J)=DCHK
C
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
        CALL FUNC(N,PSTAR,FN,JSUB,IG,INTLIST,RPAR,IPAR)
        YSTAR=FN
        ICOUNT=ICOUNT+1
        IF(YSTAR.GE.YLO) GO TO 12
        IF(ICOUNT.GE.KCOUNT) GO TO 19
C
C  SUCESSFUL REFLECTION SO EXTENSION.
C
        DO 9 I=1,N
9       P2STAR(I)=ECOEFF*PSTAR(I)+(1.0D0-ECOEFF)*PBAR(I)
        CALL FUNC(N,P2STAR,FN,JSUB,IG,INTLIST,RPAR,IPAR)
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
        CALL FUNC(N,P2STAR,FN,JSUB,IG,INTLIST,RPAR,IPAR)
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
        CALL FUNC(N,XMIN,FN,JSUB,IG,INTLIST,RPAR,IPAR)
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
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
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


C  THIS ROUTINE, CALLED BY ELDERY, FINDS THE FUNCTIONAL VALUE, FNTVAL
C  FOR THE SUPPLIED VARIABLE VECTOR, VEC.

C  FNTVAL = -LOG-LIKELIHOOD OF THE PARAMETER
C	    VECTOR VEC, GIVEN THIS SUBJECT'S DATA FILE, AND THE CURRENT
C 	    ESTIMATIONS OF THE POPULATION MEAN (ESTMEN) AND COV. MATRIX 
C	    (ESTCOV).

C  CLARIFICATION (IN MXEM1S42.FOR):
C	ACTUALLY, FNTVAL = -LOG-LIK OF THE
C	CURRENT SUBJECT'S DATA FILE, GIVEN THE PARAMETER VECTOR VEC,
C	AND THE CURRENT ESTIMATES OF THE POPULATION MEAN (ESTMEN) AND
C	COV. MATRIX (ESTCOV) ... I.E., -LOG P(YI,VEC|ESTMEN,ESTCOV)
C	IS MINIMIZED.
C  SEE NOTES ON PG. 2.5 OF MXEM1S40.FOR NOTES.


C  SINCE ELDERY MINIMIZES FNTVAL, IT FINDS THE VECTOR VEC WHICH 
C  MAXIMIZES THE ABOVE LOG-LIK. THIS VECTOR IS THE MAP BAYESIAN
C  ESTIMATE (FOR THE SUBJECT WHOSE DATA VALUES HAVE BEEN READ IN BY
C  SUBROUTINE FILRED (AND PUT INTO APPROPRIATE COMMON BLOCKS)).

C  NOTE THAT THESE COMMON BLOCKS ARE NOT NEEDED IN THIS ROUTINE. THEY 
C  ARE PASSED TO SUBROUTINE IDPC, WHICH USES THEM TO CALCULATE W --

C  SEE BELOW.


C  INPUT ARE:

C  NVAR = NO. OV VARIABLES (BETWEEN 1 AND MAXDIM).
C  VEC = VECTOR OF CANDIDATES FOR THE NVAR VARIABLES.
C	 NOTE: INFO INDICATING WHICH PARAMETERS ARE RANDOM VARIABLES, 
C	       AND WHICH ARE FIXED (ALONG WITH THE FIXED VALUES) HAS 
C	       BEEN PASSED TO SUBROUTINE PARDEF WHICH SUPPLIES THE INFO
C	       TO SUBROUTINE IDPC -- SEE BELOW. ALSO, ALL OTHER 
C	       NECESSARY INFO HAS BEEN PASSED TO IDPC. 

C  SIGFAC,OFAC = NEEDED VALUES TO CALCULATE FNTVAL -- SEE DEFN. IN MAIN.
C  ESTMEN(I), I=1,NVAR = CURRENT ESTIMATE OF POPULATION MEAN VECTOR.
C  ESTINV(I,J), I,J=1,NVAR = INVERSE OF THE CURRENT ESTIMATE OF THE 
C			     POPULATION COVARIANCE MATRIX.
C  DET = DETERMINANT OF INV(ESTINV).
C  NOFIX = NO. OF FIXED PARAMATERS.
C  VALFIX(I) = VALUE OF FIXED PARAMETER I, I=1,NOFIX.
C  NRANFIX = NO. OF RANFIX PARAMETERS.
C  RANFIXEST(I) = VALUE OF RANFIX PARAMETER I, I=1,NRANFIX.



C  IRAN(I) = 1 IF PARAMATER I IS RANDOM AND CANNOT BE NEGATIVE;
C           -1 IF PARAMETER I IS RANDOM AND MAY BE NEGATIVE;
C            0 IF PARAMETER I IS FIXED; 
C            2 IF PARAMETER I IS UNKNOWN BUT THE SAME FOR ALL
C                 SUBJECTS; I = 1,NVAR+NOFIX+NRANFIX.

C  NUMEQT = NO. OF OUTPUT EQUATIONS.


C  OUTPUT IS:
C
C  FNTVAL = -LOG(P(Y|VEC)) - LOG(VEC|ESTMEN,ESTINV), WHERE Y IS THE
C	    VECTOR OF OBSERVED VALUES FOR THIS SUBJECT (PASSED IN
C	    COMMON BLOCK TO IDPC).


C  SSND(IEQ),IEQ=1,NUMEQT (VIA COMMON/TOMAP) BACK TO MAIN. SSND(IEQ) IS 
C	SET = W(IEQ) BELOW. SEE DETAILS THERE. NOTE THAT MAIN DOESN'T
C	USE SSND UNTIL ELDER HAS FINISHED CALLING THIS ROUTINE - I.E.,
C 	ELDERY HAS OBTAINED THE 'BEST' PARAMETER VECTOR FOR VEC - SO THE
C 	SSND USED BY MAIN IS FOR THIS 'BEST' VEC.


C  1ST CHECK THAT ALL THE ENTRIES IN VEC WHICH ARE REQUIRED TO BE
C  NON-NEGATIVE ARE. IF ANY ISN'T, RETURN A LARGE POSITIVE VALUE (AN
C  UNATTRACTIVE VALUE) FOR FNTVAL. 

      NNNVAR=0

C      write (*,*) "In MAPBAYS: Main loop"

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


C  CALCULATE  -LOG(P(Y|VEC)), CALLED PYXLOG BELOW.

C  CALL IDPC, A SUBROUTINIZED VERSION OF THE ADAPT PROGRAM ID3, TO
C  CALCULATE THE SUM OF SQUARES OF DIFFERENCES BETWEEN THE OBSERVED 
C  VALUES AND THE PREDICTED (BY THE MODEL) VALUES, FOR EACH OUTPUT
C  EQUATION, FOR THIS VARIABLE VECTOR, VEC. THESE SUM OF SQUARES ARE
C  EACH NORMALIZED BY THE ASSAY VARIANCE OF EACH OBSERVATION.

C  BEFORE ALL CALLS TO IDCALCY, MUST INTEGRATE FIXED, RANDOM, AND
C  RANFIX VALUES INTO PX, USING IRAN(I),I=1,NVAR+NOFIX+NRANFIX. CALL 
C  MAKEVEC TO DO THIS.

	CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,RANFIXEST,PX)

C        write (*,*) "IN MAPBAYS: Calling IDPC()"

	CALL IDPC(JSUB,IG,PX,W,INTLIST,RPAR,IPAR)

C        write (*,*) "IN MAPBAYS: Ret. fr. IDPC()"

C  W(J), J=1,NUMEQT RETURNS, WHERE W(J) IS THE SUM OVER I=1,NOBSER, OF:
C  ((YO(I,J)-H(I,J))/SIG(I,J))**2, WHERE H(I,J) = PREDICTED VALUE OF THE
C  JTH OUTPUT EQ AT THE ITH OBSERVATION TIME, ASSUMING VEC = TRUE VECTOR
C  OF VARIABLE VALUES. I.E., W(J) IS THE NORMALIZED SUM OF SQUARES FOR 
C  THE OBSERVATIONS OF THE JTH OUTPUT EQUATION. NOTE THAT MISSING VALUES
C  (I.E., YO(I,J) = -99) DO NOT CONTRIBUTE TO THE ABOVE SUMS OF SQUARES.

C  STORE W(IEQ) INTO SSND(IEQ), WHICH IS PASSED VIA COMMON/TOMAP BACK TO 
C  MAIN. ALSO, TOTAL THE W'S INTO WTOTAL.

	WTOTAL = 0.D0
	DO IEQ=1,NUMEQT

	 SSND(IEQ) = W(IEQ)
	 WTOTAL = WTOTAL + W(IEQ)
	END DO

C  P(YJ|X) = EXP(-.5*WTOTAL)/SIGFAC/OFAC, SO -LOG-LIKELIHOOD OF THIS
C  PARAMETER VECTOR IS .5*WTOTAL + LOG(SIGFAC) + LOG(OFAC).

	PYXLOG = .5D0*WTOTAL + DLOG(SIGFAC) + DLOG(OFAC)

C  CALCULATE -LOG(VEC|ESTMEN,ESTINV), WHICH


C  = NVAR/2 * LOG(2*PI) + .5*LOG(DET) +.5*DIFF'*ESTINV*DIFF, WHERE
C  DIFF = (VEC - ESTMEN), AND DET = DET(INV(ESTINV)) WAS PASSED IN
C  COMMON/TOMAP/ ABOVE.

C  NOTE: VEC -- N(ESTMEN,ESTCOV) IS ASSUMED, WHERE ESTINV = INV(ESTCOV).

	DO J=1,NVAR
	 DIFF(J) = VEC(J) - ESTMEN(J)
	END DO

C  CALCULATE SUM = DIFF'*ESTINV*DIFF

	SUM=0.D0
	 DO I=1,NVAR
	  DO J=1,NVAR
	   SUM = SUM + DIFF(I)*ESTINV(I,J)*DIFF(J)
	  END DO
	 END DO

	PVECLOG = .5D0*NVAR*DLOG(2.D0*3.1415926)+.5D0*DLOG(DET)+.5D0*SUM
	FNTVAL = PYXLOG + PVECLOG

C  SEE PG. 2.5 OF MXEM1S40.FOR NOTES FOR DETAILS ON ABOVE RESULTS.

C        write (*,*) "In MAPBAYS: Returning to ELDERY"

	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT,INTLIST)

C  FILRED IS CALLED BY MAIN TO READ THE PORTION OF 
C  SCRATCH FILE 27 WHICH APPLIES TO THE SUBJECT UNDER CONSIDERATION. THE

C  'POINTER' FOR FILE 27 IS IN THE PROPER POSITION TO BEGIN READING THE
C  INFO FOR THE DESIRED SUBJECT.

        IMPLICIT REAL*8(A-H,O-Z)

        PARAMETER(MAXNUMEQ=7)

        DIMENSION TIM(594),SIG(5000),RS(5000,34),YO(594,NUMEQT),
     1  BS(5000,7),C0(NUMEQT),C1(NUMEQT),C2(NUMEQT),C3(NUMEQT),
     2  YOO(594,MAXNUMEQ)

c  AS OF it2beng15.f, THE FORMAT FOR THE WORKING COPY FILES IS:

C     COL 1 = TIME
C     COL 2 = IV FOR DRUG 1; COL 3 = PO FOR DRUG 1;
C     COL 4 = IV FOR DRUG 2; COL 5 = PO FOR DRUG 2;
C     ... EACH SUCCEEDING DRUG HAS AN IV FOLLOWED BY A PO COLUMN.
C     NEXT NADD COLUMNS = ONE FOR EACH ADDITIONAL COVARIATE (ADDITIONAL
C      REFERS TO ANY EXTRA COVARIATES BEYOND THE 4 PERMANENT ONES IN
C      COMMON DESCR (SEE BELOW).
 
        COMMON /OBSER/ TIM,SIG,RS,YOO,BS
        COMMON /CNST/ N,ND,NI,NUP,NUIC,NP
        COMMON /CNST2/ NPL,NUMEQTT,NDRUG,NADD
        COMMON /SUM2/ M,NPNL
        COMMON/DESCR/AGE,HEIGHT,ISEX,IETHFLG
        COMMON/ERR/ERRFIL

        CHARACTER SEX*1,READLINE*300,ERRFIL*20

C wmy20190513
        integer, dimension(128) :: INTLIST

 
C  INPUT IS: SCRATCH FILE 27, WHICH IS POSITIONED AT THE BEGINNING OF
C  THE INFO FOR THE SUBJECT DESIRED.
 
C  OUTPUT ARE:
 
C  NOBSER = THE NO. OF OBSERVATIONS FOR THIS SUBJECT.
C  YO(I,J),I=1,M; J=1,NUMEQT = NO. OF OUTPUT EQS; I=1,M, WHERE M = NO.
C	OF OBSERVATION TIMES.
C  [C0(J),C1(J),C2(J),C3(J)] = ASSAY NOISE COEFFICIENTS FOR OUTPUT EQ.
C	J; J=1,NUMEQT.
C  THE 4 DESCRIPTOR VALUES FOR THIS SUBJECT (AGE, SEX, HEIGHT,
C    ETHNICITY FLAG) VIA COMMON/DESCR TO SUBROUTINES DIFFEQ/OUTPUT.
C  VARIABLES/ARRAYS IN ABOVE COMMON STATEMENTS.
 
C  AGE, SEX, HEIGHT, AND ETHNICITY FLAG ARE ON LINES 8-11.
 
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
 
C  READ THE NO. OF DRUGS FROM THE LINE WITH 'NO. OF DRUGS' AS ENTRIES
C  12:23. THEN READ NO. OF ADDITIONAL COVARIATES, AND THE NO. OF DOSE 
C  EVENTS, ETC.

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
        INTLIST(6) = NADD
        INTLIST(7) = NI
        IF(NI .GT. 34) THEN
         WRITE(*,123)
  123    FORMAT(/' YOUR PATIENT DATA FILES HAVE TOO MANY COLUMNS IN '/
     1' THE DOSAGE REGIMEN BLOCK. THE NO. OF ADDITIONAL COVARIATES '/

     2' PLUS TWICE THE NO. OF DRUGS CANNOT EXCEED 34. THE PROGRAM IS'/
     3' NOW STOPPING. '/)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

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
        END DO	 

C  ASSIGN THE VALUES IN EACH DRUG'S PO COLUMN TO THE CORRESPONDING
C  COLUMN IN ARRAY BS.

        DO I=1,ND
         DO J=1,NDRUG
          BS(I,J)=RS(I,2*J)
         END DO
        END DO

C  READ THE NO. OF OUTPUT EQUATIONS FROM THE LINE WITH 'NO. OF TOTAL'
C  AS ENTRIES 12:23. THEN READ NO. OF OBSERVED VALUE TIMES, ETC.

   40	  READ(27,1) READLINE
        IF(READLINE(12:23) .NE. 'NO. OF TOTAL') GO TO 40
        BACKSPACE(27)

        READ(27,3) NUMEQTT
        INTLIST(9) = NUMEQTT

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
        INTLIST(10)=M
        MAXOBDIM = 150
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
        END DO
 
C  PUT YO VALUES INTO YOO BECAUSE A DUMMY ARGUMENT CANNOT BE IN A
C  COMMON STATEMENT.
 
        DO I=1,M
         DO J=1,NUMEQT
          YOO(I,J) = YO(I,J)
         END DO
        END DO
 
        NOBSER=M

 
C  AT THIS POINT, MUST SKIP THE COVARIATE INFO IN THE FILE, AND PROCEED
C  TO READ THE ASSAY NOISE COEFFICIENTS BELOW THAT.
 
C  READ THE NUMEQT SETS OF ASSAY COEFFICIENTS JUST BELOW THE LINE
C  WHICH HAS "ASSAY COEFFICIENTS FOLLOW" IN ENTRIES 1:25.

   50	  READ(27,1) READLINE
        IF(READLINE(1:25) .NE. 'ASSAY COEFFICIENTS FOLLOW') GO TO 50



        DO IEQ = 1,NUMEQT
         READ(27,*) C0(IEQ),C1(IEQ),C2(IEQ),C3(IEQ)
        END DO
 

        RETURN
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE CALCOV(NVAR,NOBACT,PMAT,RINV,COVINV)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION PMAT(594,30),COVINV(30,30),RINV(594,594),
     1  PMATT(30,594),WORK(30,594)

C  THIS SUBROUTINE, CALLED BY MAIN, CALCULATES COVINV WHERE COVINV 
C  = INV(COVEST), AND COVEST = THE ESTIMATE OF THE COVARIANCE MATRIX OF 
C  THE PARAMETER ESTIMATES. 

C  COVEST = INV[PMAT'*RINV*PMAT]; SO COVINV = PMAT'*RINV*PMAT, 
C           WHERE PMAT' = PMAT TRANSPOSE.


C  INPUT ARE:


C  NVAR,NOBACT = DIMENSIONS.
C  PMAT = NOBACT x NVAR MATRIX DESCRIBED IN MAIN.
C  RINV = NOBACT x NOBACT MATRIX.

C  OUTPUT IS:

C  COVINV = NVAR x NVAR MATRIX DEFINED ABOVE.


C ESTABLISH PMATT = PMAT TRANSPOSE.

	DO I=1,NOBACT
	 DO J=1,NVAR

	  PMATT(J,I)=PMAT(I,J)
	 END DO
	END DO

C  CALL MULT1 TO OBTAIN WORK = PMATT*RINV.

	CALL MULT1(NVAR,NOBACT,NOBACT,PMATT,RINV,WORK)

C  CALL MULT2 TO OBTAIN WORK*PMAT = COVINV. 

	CALL MULT2(NVAR,NOBACT,NVAR,WORK,PMAT,COVINV) 

	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE MULT1(N1,N2,N3,A,B,C)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION A(30,594),B(594,594),C(30,594)

C  THIS SUBROUTINE, CALLED BY SUBROUTINE CALCOV, INPUTS MATRICES A,B,
C  AND C, AND RETURNS C=A*B, WHERE A IS N1 x N2, B IS N2 x N3, AND
C  C IS N1 x N3.

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

C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C

	SUBROUTINE MULT2(N1,N2,N3,A,B,C)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION A(30,594),B(594,30),C(30,30)

C  THIS SUBROUTINE, CALLED BY SUBROUTINE CALCOV, INPUTS MATRICES A,B,
C  AND C, AND RETURNS C=A*B, WHERE A IS N1 x N2, B IS N2 x N3, AND
C  C IS N1 x N3.

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
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE MATNV2(A,N,B,M,DETERM) 
	IMPLICIT REAL*8(A-H,O-Z)
C
C     MATRIX INVERSION WITH ACCOMPANYING SOLUTION OF LINEAR EQUATIONS
C
C  THIS IS THE DOUBLE PRECISION VERSION OF MATINV; IT IS CALLED BY
C  SUBROUTINE CALCOV. 
C
      DIMENSION PIVOT(100),INDEX(100)
C
C  NOTE: A HAS RANK N (I.E., THE NON-ZERO PART OF A IS AN N x N MATRIX).
C        M WILL = 1    FOR CALLS FROM CALCOV.     
C   
C  THE DIMENSIONS OF A AND B BELOW HAVE BEEN CHANGED TO MAKE THIS
C  PROGRAM COMPATIBLE WITH THE CALLING PROGRAM. 

      DIMENSION A(30,30),B(30,1)
      DATA DETMAX,DETMIN/1.0D+30,1.0D-30/

C
C     INITIALIZE DETERMINANT AND PIVOT ELEMENT ARRAY
C
      DETERM=1.D0
      IDET=0
C
      DO 20 I=1,N
      PIVOT(I)=0.D0
   20 INDEX(I)=0.0
C
C     PERFORM SUCCESSIVE PIVOT OPERATIONS (GRAND LOOP)
C
      DO 550 I=1,N
C
C     SEARCH FOR PIVOT ELEMENT AND EXTEND DETERMINANT PARTIAL PRODUCT
C
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
C
      DETERM=DETERM*DETMIN
C
      IDET=IDET+1
C
      GO TO 140
C 
  130 IF (ABS(DETERM).GT.DETMIN) GO TO 140
C
      DETERM=DETERM*DETMAX
C
      IDET=IDET-1
C
  140 CONTINUE
C
C

C     RETURN IF MATRIX IS SINGULAR (ZERO PIVOT) AFTER COLUMN INTERCHANGE
C
      IF (DETERM.EQ.0.D0) GO TO 600
C
      PIVOT(ICOLUM)=AMAX
C
C     INTERCHANGE ROWS TO PUT PIVOT ELEMENT ON DIAGONAL
C
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
C
C     DIVIDE PIVOT ROW BY PIVOT ELEMENT
C
  260 K=ICOLUM
      A(ICOLUM,K)=1.D0
      DO 350 K=1,N
      A(ICOLUM,K)=A(ICOLUM,K)/AMAX
  350 CONTINUE
      IF (M.LE.0) GO TO 380
      DO 370 K=1,M
      B(ICOLUM,K)=B(ICOLUM,K)/AMAX
  370 CONTINUE
C
C     REDUCE NON-PIVOT ROWS
C
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
C

C     INTERCHANGE COLUMNS AFTER ALL PIVOT OPERATIONS HAVE BEEN PERFORMED
C
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
C
      PIVOT(1)=IDET

C 
      RETURN
      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE CALCPIK(NVAR,COVINV,ESTINV,PIK)

	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION B(30,1),COVINV(30,30),ESTINV(30,30),PIK(30,30)

C  THIS SUBROUTINE, CALLED BY MAIN, CALCULATES PIK (SEE MODEL DETAILS 
C  AT THE TOP OF PROGRAM). 

C  PIK = INV[COVINV + ESTINV]. 

C  INPUT ARE:

C  NVAR, COVINV, AND ESTINV AS DESCRIBED ABOVE.

C  OUTPUT IS:

C  PIK AS DESCRIBED ABOVE.

C  PUT COVINV + ESTINV INTO PIK. ON RETURN FROM MATNV2, PIK WILL BE THE
C  INVERSE OF (COVINV + ESTINV).

	DO I=1,NVAR
	 DO J=1,NVAR
	  PIK(I,J) = COVINV(I,J) + ESTINV(I,J)
	 END DO
	END DO

	CALL MATNV2(PIK,NVAR,B,1,DET)

C  NOTE THAT B IS A WORK VECTOR.

	IF(DET .LE. 0) WRITE(*,*)' PIK IS SINGULAR IN CALCPIK.'
	IF(DET .LE. 0) WRITE(25,*)' PIK IS SINGULAR IN CALCPIK.'
	RETURN
	END

C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE GETMED(NVAR,NSUB,MAXSUB,MAXDIM,IESTIJ,PAREST,VEC,
     1  ESTMED)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION PAREST(MAXSUB,MAXDIM),IESTIJ(MAXSUB,MAXDIM),
     1  VEC(MAXSUB),ESTMED(30)

C  THIS SUBROUTINE, CALLED BY MAIN, CALCULATES THE MEDIAN OF THE 
C  'ACTIVE' (SEE IESTIJ BELOW) ENTRIES OF EACH COLUMN OF PAREST, AND 
C  PUTS THE VALUES INTO ESTMED. 

C  INPUT ARE: 


C  NVAR = NO. OF ACTIVE COLUMNS OF PAREST.
C  NSUB = NO. OF ROWS OF PAREST.
C  IESTIJ(I,J) = 1 IF ENTRY (I,J) OF PAREST IS 'ACTIVE'.
C		 0 OTHERWISE.
C  PAREST = SEE ABOVE.

C  OUTPUT IS:

C  ESTMED(I) = MEDIAN OF COLUMN I OF PAREST, I=1,NVAR.


	DO 100 J=1,NVAR

C  FOR THIS COLUMN, PUT THE ACTIVE ENTRIES INTO VECTOR VEC.

	IND=0
	DO I=1,NSUB
	  IF(IESTIJ(I,J) .EQ. 1) THEN
	    IND=IND+1
	    VEC(IND)=PAREST(I,J)
	  ENDIF
	END DO

C  NOW VEC(I),I=1,IND, CONTAINS THE ACTIVE ENTRIES FOR COLUMN J.

C  DETERMINE WHETHER IND IS EVEN OR ODD (THE WAY THE MEDIAN IS
C  CALCULATED DEPENDS ON THIS, OF COURSE).

	IHALF=IND/2
	NN=2*IHALF
	IF(NN .EQ. IND) IEVEN=1
	IF(NN .NE. IND) IEVEN=0 


C  FOR THIS COLUMN (J), ORDER THE ENTRIES IN VEC.

C  IN THE DO 50 LOOPS BELOW, NOW IS THE CURRENT NEXT SMALLEST ENTRY.

	DO 50 NOW=1,IND-1
	DO 50 I=NOW+1,IND

	IF(VEC(I) .LT. VEC(NOW)) THEN

C  INTERCHANGE VEC(I) AND VEC(NOW).

	  V1=VEC(I)
	  V2=VEC(NOW)
	  VEC(I)=V2
	  VEC(NOW)=V1

	ENDIF

   50   CONTINUE

C  THE VEC VALUES ARE NOW ORDERED FROM SMALLEST TO LARGEST.

C  NOW FIND THE MEDIAN.

C  IF IND IS ODD, THE MEDIAN IS VEC((IND+1)/2).
C  IF IND IS EVEN, THE MEDIAN IS THE AVERAGE OF VEC(IND/2) 
C  AND VEC(IND/2 + 1). 

	IF(IEVEN .EQ. 0) ESTMED(J) = VEC((IND+1)/2)
	IF(IEVEN .EQ. 1) ESTMED(J) = (VEC(IND/2) + VEC(IND/2 + 1))/2.D0



  100   CONTINUE


	RETURN
	END
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
      SUBROUTINE Old_MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,
     2 RANFIXEST,PX)

	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION IRAN(32),X(30),VALFIX(20),PX(32),RANFIXEST(20)

C  THIS ROUTINE, CALLED BY MAIN, INPUTS NVAR, NOFIX, NRANFIX, IRAN, X,
C  VALFIX, AND RANFIXEST, AND RETURNS PX(I) = A COMBINATION OF THE 
C  VALUES IN X, VALFIX, AND RANFIXEST, IN THE PROPER ORDER (AS 
C  DETERMINED BY IRAN).
 
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
	 IF(IFILE .EQ. 24) WRITE(24,222) IFIRST
	 IF(IFILE .EQ. 25) WRITE(25,222) IFIRST
  222    FORMAT(1X,I5)
	 GO TO 100	
	ENDIF

C  IF THE NEXT PATIENT NO. IN IPATVEC = IFIRST + 1, THEN IFIRST IS THE
C  FIRST OF A STRING OF CONSECUTIVE NUMBERS (FIND THE LAST NO. IN THIS 
C  STRING AND WRITE THE STRING OUT). OTHERWISE, IFIRST WILL BE WRITTEN 
C  OUT BY ITSELF.

	IF(IPATVEC(NEXTIND+1) .NE. IFIRST + 1) THEN
	 IF(IFILE .EQ. 24) WRITE(24,222) IFIRST
	 IF(IFILE .EQ. 25) WRITE(25,222) IFIRST
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
   	IF(IFILE .EQ. 24) WRITE(24,221) IFIRST,ILAST
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
C  OF THE A1000 FORMAT. INSTEAD MUST WRITE (22,__) READLINE, WHERE
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
C  AS OF it2beng19.f, STEADY STATE DOSES MAY BE BOLUS DOSES. IN THIS
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

C  As of it2beng20.f, STORE ND INTO NDORIG(JSUB); IT WILL BE PASSED
C  TO SUBROUTINE READOUT VIA COMMON/DOSEOBS.

       NDORIG(JSUB) = ND

       DO I = 1,ND

        READ(23,*) SIG(I),(RS(I,J),J=1,NI)

C  AS OF it2beng20.f, STORE THE VALUES IN THE DOSE BLOCK FOR PASSAGE
C  TO SUBROUTINE READOUT VIA COMMON/DOSEOBS.

C  AS OF it2beng21.f, RATHER THAN USING BACKSPACE(23), ESTABLISH
C  DOSEBLOCK BY STRAIGHTFORWARD ASSIGNMENTS. THE REASON IS THAT,
C  DEPENDING ON WHICH COMPILER IS USED TO MAKE THE PR PREP PROGRAM
C  (CURRENT ONE IS IT2B107.FOR), IT IS POSSIBLE FOR A DOSE EVENT
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


C  AS OF it2beng20.f, STORE IN THE VALUES IN THE OBSERVATION BLOCK FOR
C  PASSAGE TO SUBROUTINE READOUT VIA COMMON DOSEOBS.

  140	 READ(23,1717) READLINE
       IF(READLINE(12:23) .NE. 'NO. OF TOTAL') GO TO 140

       BACKSPACE(23)

       READ(23,3) NUMEQT
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
     1' FROM THE INSTRUCTION FILE, it2b102.inp. IF YOU EDITED THIS'/
     2' FILE MANUALLY, PLEASE RERUN THE PC PREP PROGRAM TO HAVE IT'/
     3' PREPARE it2b102.inp AGAIN AND THEN RERUN THIS PROGRAM.'//
     4' IF YOU DID NOT MANUALLY EDIT it2b102.inp, PLEASE SEND THE'/
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

C  SINCE NUMEQT IS READ IN ABOVE, IT DOES NOT HAVE TO BE READ IN HERE.
C  JUST READ(23,*) ON NEXT LINE. FOR THAT MATTER, COULD HAVE USED
C  ANOTHER READ(23,*) ON FOLLOWING LINE SINCE M WAS READ IN ABOVE TOO.
  
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


C  REPLACE WRITING OF SIG(),RS() VALUES WITH XVERIFY (SEE LOGIC IN 
C  SUBROUTINE VERIFYVAL. SIMIARLY FOR ALL CALLS TO VERIFYVAL.

       XVERIFY(1) = SIG(ID)
       XVERIFY(2) = RS(ID,2*IDRUG-1)
       XVERIFY(3) = RS(ID,2*IDRUG)
       CALL VERIFYVAL(3,XVERIFY)
	

         IF(RS(ID,2*IDRUG) .LE. 0.D0 .AND. RS(ID,2*IDRUG-1) .GT. 0) THEN
C        WRITE(*,101) ID,SIG(ID),IDRUG,RS(ID,2*IDRUG-1),RS(ID,2*IDRUG)
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
C        WRITE(42,101) ID,SIG(ID),IDRUG,RS(ID,2*IDRUG-1),RS(ID,2*IDRUG)
         WRITE(42,101) ID,XVERIFY(1),IDRUG,XVERIFY(2),XVERIFY(3)
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

C  SINCE NUMEQT IS READ IN ABOVE, IT DOES NOT HAVE TO BE READ IN HERE.
C  JUST READ(23,*) ON NEXT LINE. FOR THAT MATTER, COULD HAVE USED
C  ANOTHER READ(23,*) ON FOLLOWING LINE SINCE M WAS READ IN ABOVE TOO.
  
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

C  AS OF it2beng26.f, MAKE SURE THAT NO TWO TIMES ARE THE SAME SINCE
C  IF THEY ARE, IT CAN CONFUSE SUBROUTINE SHIFT (CAUSING IT TO GO INTO
C  AN INFINITE LOOP - SEE NPAG115.EXP, TESTCASE 5).

        CALL THESAME(SIGLAST,SIGG(I),ISAME)

        IF(ISAME .EQ. 1) THEN

       XVERIFY(1) = SIGLAST
       CALL VERIFYVAL(1,XVERIFY)
C      WRITE(*,4031) SIGLAST
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
C        WRITE(42,4031) SIGLAST
         WRITE(42,4031) XVERIFY(1)

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
      SUBROUTINE ORDERDELTA(NDRUG,DELTAIV,NDELTA,ORDELT)

      use npag_utils, only : thesame

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION DELTAIV(7),ORDELT(7),X(7)

C  SUBROUTINE ORDERDELTA IS CALLED BY NEWWORK TO OBTAIN NDELTA, THE NO.
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
      SUBROUTINE VERIFYVAL(N,X)
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
	SUBROUTINE CALCRF(NTOTPAR,VEC,FNTVAL,NUMEQT,YO,C0,C1,C2,C3,
     1 GAMMA,JSUB,IG,INTLIST)
	IMPLICIT REAL*8(A-H,O-Z)

C  THIS SUBROUTINE IS CALLED BY ELDERY TO FIND THE FUNCTIONAL VALUE,
C  FNTVAL, FOR THE SUPPLIED CANDIDATE VECTOR, VEC. ELDERY CALLS THIS
C  SUBROUTINE OVER AND OVER UNTIL IT FINDS THE VECTOR, VEC, WHICH
C  MINIMIZES FNTVAL.

C  FNTVAL IS THE NORMALIZED SUM OF SQ. DIFFERENCES BETWEEN ALL OBSERVED
C  AND PREDICTED VALUES OVER ALL NSUB SUBJECTS, GIVEN THE NTOTPAR 
C  VALUES SUPPLIED IN THE CANDIDATE VECTOR VEC. NOTE THAT THESE VALUES
C  WILL BE ASSIGNED TO THE PARAMETER ENTRIES IN PX(.) WHICH HAVE 
C  IRAN(.) = 2, AND THEN IRAN(.) = 1 OR -1. THE OTHER PARAMETER VALUES 
C  (I.E., THOSE WHICH HAVE IRAN(.) = 0) WERE ALREADY ASSIGNED TO THE 
C  APPROPRIATE ENTRIES IN PX BEFORE ELDERY WAS CALLED IN MAIN.

      PARAMETER(MAXNUMEQ=7)

	DIMENSION VEC(NTOTPAR),IRAN(32),PX(32),SIG(594,MAXNUMEQ),
     1 YO(594,NUMEQT),C0(NUMEQT),C1(NUMEQT),C2(NUMEQT),C3(NUMEQT),
     2 W(MAXNUMEQ),GAMMA(NUMEQT)

      COMMON SIG
	COMMON/TOCALC/IRAN,PX,NOFIX,NSUB

C  COMMON SIG IS USED TO PASS THE VALUES ESTABLISHED IN SIG(.,.)
C  BELOW TO SUBROUTINE FUNC.

      integer JSUB,IG
      integer, dimension(128)::INTLIST


C  COMMON/TOCALC VALUES ARE PASSED TO THIS ROUTINE FROM MAIN.


C  AS INDICATED ABOVE, PX(.) HAS THE CORRECT VALUES ALREADY IN PLACE
C  FOR THE PARAMETERS WITH IRAN(.) = 0. NOW INSERT THE CANDIDATE
C  VALUES IN VEC(.) INTO THE ENTRIES IN PX(.) WITH IRAN(.) = 2 AND 1
C  AND -1. NOTE A PARAMETER WITH IRAN(.) = -1 MEANS THAT PARAMETER IS
C  RANDOM AND CAN BE NEGATIVE, WHILE ONE WITH IRAN(.) = 1 MEANS THAT
C  PARAMETER IS RANDOM BUT CANNOT BE NEGATIVE.

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
C  VERIFY THAT IF IRAN(I) = 1, THE CANDIDATE VALUE IS NOT .LT. 0). IF 
C  IT IS .LT. 0, RETURN A LARGE POSITIVE VALUE, WHICH IS UNATTRACTIVE, 
C  FOR FNTVAL.
        IF(VEC(NVEC) .LT. 0.D0 .AND. IRAN(I) .EQ. 1) THEN
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

C       write (*,*) "CALL FILRED at 7677"
       CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT,INTLIST)

C  SEE COMMENTS IN THE 140 LOOP IN MAIN.

       DO 140 I=1,NOBSER
        DO 140 J=1,NUMEQT

         Y = YO(I,J)

C  IF YO(I,J) = -99, IT MEANS THAT OUTPUT EQ. J HAD NO VALUE AT 
C  OBSERVATION TIME I. IN THIS CASE, SIG(I,J) WILL NOT BE SET, AND IT
C  OF COURSE WILL NOT BE NEEDED IN SUBROUTINE FUNC.
 
         IF(Y .EQ. -99) GO TO 140
         SIG(I,J) = GAMMA(J)*(C0(J)+C1(J)*Y+C2(J)*Y*Y+C3(J)*Y**3)

  140    CONTINUE


C  CALL IDPC, A SUBROUTINIZED VERSION OF THE ADAPT PROGRAM ID3, TO
C  CALCULATE THE SUM OF SQUARES OF DIFFERENCES BETWEEN THE OBSERVED 
C  VALUES AND THE PREDICTED (BY THE MODEL) VALUES, FOR EACH OUTPUT
C  EQUATION, FOR THIS VARIABLE VECTOR, VEC. THESE SUM OF SQUARES ARE
C  EACH NORMALIZED BY THE ASSAY VARIANCE OF EACH OBSERVATION.

       CALL IDPC(JSUB,IG,PX,W,INTLIST,RPAR,IPAR)

C  W(J), J=1,NUMEQT RETURNS, WHERE W(J) IS THE SUM OVER I=1,NOBSER, OF:
C  ((YO(I,J)-H(I,J))/SIG(I,J))**2, WHERE H(I,J) = PREDICTED VALUE OF THE
C  JTH OUTPUT EQ AT THE ITH OBSERVATION TIME, ASSUMING VEC = TRUE VECTOR
C  OF VARIABLE VALUES. I.E., W(J) IS THE NORMALIZED SUM OF SQUARES FOR 
C  THE OBSERVATIONS OF THE JTH OUTPUT EQUATION. NOTE THAT MISSING VALUES
C  (I.E., YO(I,J) = -99) DO NOT CONTRIBUTE TO THE ABOVE SUMS OF SQUARES.

       WTOTAL = 0.D0
       DO IEQ = 1,NUMEQT
        WTOTAL = WTOTAL + W(IEQ)
       END DO

C  ADD WTOTAL TO SUMTOT.

       SUMTOT = SUMTOT + WTOTAL


      END DO
C  THE ABOVE END DO IS FOR THE  DO JSUB=1,NSUB  LOOP.

      FNTVAL = SUMTOT


	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE ELDERY2(N,START,XMIN,YNEWLO,REQMIN,STEP,
     X  ITMAX,FUNC,IPRINT,ICONV,NITER,ICOUNT,NUMEQT,YO,C0,C1,C2,C3,
     1  GAMMA,JSUB,IG,INTLIST)

C  ELDERY DIFFERS FROM ELDERX ONLY IN THE DIMENSION STATEMENT. ALL 5'S
C  ARE CHANGED TO 25'S, AND ALL 6'S ARE CHANGED TO 26'S. THIS ALLOWS 25
C  PARAMETERS INSTEAD OF JUST 5. As of itbig9x.f, we allow as many as
C  30 parameters.

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
C    XSEC  -THE COORDINATES OF THE VETEX WITH THE 2ND SMALLEST FUNCTION
C             VALUE.
C    YSEC  - THE FUNCTION VALUE AT XSEC.
C
      IMPLICIT REAL*8(A-H,O-Z)
        DIMENSION START(N),STEP(N),XMIN(N),XSEC(30),
     X  P(30,31),PSTAR(30),P2STAR(30),PBAR(30),Y(31),YO(594,NUMEQT),
     1  C0(NUMEQT),C1(NUMEQT),C2(NUMEQT),C3(NUMEQT),GAMMA(NUMEQT)

        integer JSUB,IG
        integer, dimension(128)::INTLIST


        EXTERNAL FUNC
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
        XN=FLOAT(N)
        DN=FLOAT(N)
        NN=N+1
C
C  CONSTRUCTION OF INITIAL SIMPLEX.
C
1001    DO 1 I=1,N
1       P(I,NN)=START(I)
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,GAMMA,
     1    JSUB,IG,INTLIST) 
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
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,GAMMA,
     1    JSUB,IG,INTLIST) 
        Y(J)=FN
        ICOUNT=ICOUNT+1
2       START(J)=DCHK
C
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
        CALL FUNC(N,PSTAR,FN,NUMEQT,YO,C0,C1,C2,C3,GAMMA,
     1    JSUB,IG,INTLIST) 
        YSTAR=FN
        ICOUNT=ICOUNT+1
        IF(YSTAR.GE.YLO) GO TO 12
        IF(ICOUNT.GE.KCOUNT) GO TO 19
C
C  SUCESSFUL REFLECTION SO EXTENSION.
C
        DO 9 I=1,N
9       P2STAR(I)=ECOEFF*PSTAR(I)+(1.0D0-ECOEFF)*PBAR(I)
        CALL FUNC(N,P2STAR,FN,NUMEQT,YO,C0,C1,C2,C3,GAMMA,
     1    JSUB,IG,INTLIST) 
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
        CALL FUNC(N,P2STAR,FN,NUMEQT,YO,C0,C1,C2,C3,GAMMA,
     1    JSUB,IG,INTLIST) 
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
        CALL FUNC(N,XMIN,FN,NUMEQT,YO,C0,C1,C2,C3,GAMMA,
     1   JSUB,IG,INTLIST)
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
        CALL FUNC(N,XMIN,FN,NUMEQT,YO,C0,C1,C2,C3,GAMMA,
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



c  readi11.f                                               4/11/16

c  readi11 has the following change from readi10:

c  This program reads through new quantities (NRANFIX, PARRANFIX(.),
c  and RANFIXEST(.)) and writes them to OUTFILER.

c  Note that because the output file will have new items, it will also
c  have a new code, which will be VERSION 1.5 - MAR 2016 in format 101.

c-----------------------------------------------------------------------

c  readi10.f                                               3/25/15

c  readi10 has the following change from readi09:

c  All numbers written out in F or G format are now tested to see if
c  they are inside [-1.D-99, 1.D-99]. If so, they are changed to be
c  0. The reason is that otherwise they will be printed out without 
c  the accompanying D or E (e.g., as .934-106, rather than .934E-106).

c  Note that this module is linked with it2beng27.f initially.

c-----------------------------------------------------------------------

c  readi09.f                                               4/8/14

c  readi09 has the following changes from readi08:

c  1. A new parameter statement is added in Subroutines READOUT,
C  FILREDT, and CONVERGE2 to establish MAXNUMEQ, so 
c  that the 3rd dimension in OBSBLOCK can be set as MAXNUMEQ+1, and
c  so all 6's which refer to the maximum no. of output equations, can
c  be changed to MAXNUMEQ.

c  2. This module is first linked with the main "engine" module,
c  it2beng24.f.

c-----------------------------------------------------------------------

c  readi08.f                                               7/21/13

c  readi08 has the following changes from readi07:

c  1. It is part of the new it2beng23.f program.
c  2. Subroutine READOUT has the additional argument, IRAN.
c  3. In the section of IT_RFxxxx.TXT which shows the parameter
c  boundaries, now there will also be a column which shows whether
c  or not the corresponding random variable had to be estimated as
c  .GE. 0 or not. Previously, this information was not reported.

c  Because this info is reported at the ends of already existing lines,
c  the code for the IT_RFxxxx.TXT file will be unchanged.

c-----------------------------------------------------------------------

c  readi07.f                                               10/30/12

c  readi07 has the following changes to readi06:

c  1. Subroutine SEPARATE will no longer be called since the
c  individual files which have the info in the combined output file will
c  now be left open from the it2beng22.f run (updated from it2beng21.f).
c  This also means that all READ(47, )'s in this module will be changed
c  to READ(29, )'s. No. File 47 wasn't used in readi06.f, so 
c  file 29 will not be used in this routine. 

c  Also, OUTFILE, the combined output file, is removed as a calling 
c  argument in this module since it is no longer needed (see above),
c  and all references to OUTFILE are removed. Instead it is left open
c  as file 26 from it2beng22.f, which means that all references to file
c  45 in this module are changed to file 26.

c  2. In Subroutine GETIPATFF, the first calling argument is 
c  hardcoded to be 25, so the READ statement at label 4210 will now
c  be hardcoded to be from file 25.
 
c  3. New Format 911 tells the user that IT_RFxxxx.TXT is being
c  created by this routine.

c-----------------------------------------------------------------------

c  readi06.f                                               8/27/12

c  readi06 has the following change from readi05:

c  IPATVEC is dimensioned MAXSUB in Subroutine GETIPATFF. This should
c  have been done before, but caused no warning with the older version
c  of gfortran. But with the newer version, it causes a compiler warning
c  to be written.

c-----------------------------------------------------------------------

c  readi05.f                                               7/8/12

c  readi05.f has the following changes to readi04.f:

c  1. DOSEBLOCK(.,.,.) AND OBSBLOCK(.,.,.) are now passed to Subroutine
c  READOUT via COMMON/DOSEBLOCK from Subroutine NEWWORK1 in 
c  it2bgeng20.f, rather than being read from file 27 by Subroutine 
c  CONVERGE2 of this module.

c  2. The version no. will now be VERSION 1.4 - JUL 2012 (format 101)
c  ... since the values in DOSEBLOCK will be different if there are
c  steady state doses (i.e., now the values will be those from the
c  original working copy file in it2b101.inp, rather than the full 
c  converted working copy file (converted by Subroutine NEWWORK1 in
c  it2beng20.f) - which means that there will be just one line of info
c  for each steady state set, rather than 100 lines.

c  3. The first dimension for DOSEBLOCK has been changed from 900 to
c  800 to match all the other arrays whose dimensions reflect the max.
c  no. of subjects.

c-----------------------------------------------------------------------

c  readi04.f                                               1/29/12

c  readi04 differs from readi03 only in that the dimensions related to
c  the no. of dose events are changed from 500 to 1000. This is needed
c  as readi04 is part of the it2beng17.f "engine", which accommodates 
c  steady state dose sets. 

c  Note that the other it2beng17.f modules allow up to 5000 dose 
c  events, but this module only allows 1000 since the array, 
c  DOSEBLOCK is too big if the dimension is 5000.

c-----------------------------------------------------------------------

c  readi03.f                                               7/15/11

c  readi03 has the following changes from readi02:

c  1. It is part of the new it2beng16.f program. The code for
c  the output file from it2beng16.f is now required to be
c  REM_FRN JUL_11, changed from REM_FRN JUL_10.

c  2. The name of the version is changed in FORMAT 101 to
c  VERSION 1.3 - JUL 2011, since the IT_RFxxxx.TXT file is changed
c  to include new information (and some of the old info will be
c  written in a changed format) - see below.

c  3. The following new values are read from the output file and written
c  to the IT_RFxxxx.TXT file: Maximum No. of iterations; stopping 
c  tolerance; convergence flag (0 --> convergence not achieved, so the
c  run stopped at the maximum ending iteration; 1 --> convergence 
c  achieved before the maximum no. of iterations was run; 2 --> 
c  convergence achieved at the maximum iteration no.); RTOL (the
c  O.D.E. tolerance); whether gamma is estimated or fixed at 1.0 for 
c  for each of the NUMEQT output equations (IGAMMA(I) = 0 --> gamma is
c  estimated for eq. I; if it = 1, gamma is fixed for eq. I); 
c  AF(I),I=1,NDRUG.

c  4. The first block of formats (starting with Format 103) are changed
c  to have more leading blanks. This is done so that RTOL, the 
c  stopping tolerance, and the AF(I) values, which are now written in 
c  this block, will have plenty of space.

c  5. Some unused code is removed.

c-----------------------------------------------------------------------

c  readi02.f                                               4/28/11

c  readi02 has the following changes from readi01:

c  1. It will be linked with the new main module it2beng15.f (updated 
c  from itbig14.f).

c  2. It will now write the assay coefficients into OUTFILER. See the
c  new array ASSAYC, whose values are written next to the values in
c  OBSBLOCK. Note that new Subroutine CONDENSE3 is needed because the
c  lines to write this info is longer than 80 columns.

c  NLPATOUT is replaced by NLPATOUTASSAY since this block will now have
c  assay c's along with output values. Similarly the phrase, PATIENT 
c  OUTPUT BLOCKS is replaced by PATIENT OUTPUT AND ASSAY COEFF. BLOCKS
c  several places in the program (in comments and in the rfile).

c  3. Several code changes in FILREDT and CONVERGE2 are needed because
c  WT and CCR are no longer automatically covariates. Now NADD refers
c  to all covariates other than the 4 permanent ones at the top of 
c  the patient data files (AGE,SEX,HEIGHT, AND ETHNICITY FLAG).

c  3. The name of the version is changed in FORMAT 101 to
c  VERSION 1.2 - APR 2011, since the rfile is changed to include 
c  assay coefficients now (see no. 2 above).

c  6. The I6 format has been changed to I10 in the 20 formats from
c  186 to 214. These formats establish the lines nos. for each new
c  section of data in OUTFILER.

c  7. AIC and BIC from the final cycle are now written into OUTFILER.

c-----------------------------------------------------------------------

c  readi01.f                                               3/01/11

c  readi01 is the module which does the same task for the Big IT2B 
c  output files as read11.f does for the Big NPAG output files - it 
c  extracts the data from the combined ouput file at the end of the run,
c  and writes it a file in a format which is more convenient for reading
c  by the program, R.

c-----------------------------------------------------------------------

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

C  NOTE THAT THE DIMENSIONS FOR MAXDIM, MAXSUB, AND MAXOBDIM
C  ABOVE HAVE BEEN REPLACED BY, RESPECTIVELY, 30, 800, AND
C  150. THIS COULD BE AVOIDED BY PROVIDING THESE VALUES IN THE ARGUMENT
C  LIST OF THIS ROUTINE. 

        CHARACTER PAR(30)*11,PARFIX(20)*11,READLINE*80, 
     1   NAME(800)*53,CHARTNO(800)*53,SEX(800)*1,PARRANFIX(20)*11,
     2   DESCR(26)*20,OUTFILER*20,READLINE2*1000,ANS*5

        CHARACTER(LEN=20) :: OSName

   	COMMON/DOSEOBS/DOSEBLOCK,OBSBLOCK,NDORIG

      WRITE(*,911)
  911 FORMAT(//' NOW CREATING THE IT_RFxxxx.TXT FILE ...')


C  SINCE MAXGRD, MAXSUB, AND MAXOBDIM ARE USED BELOW, AND ARE NOT PASSED 
C  TO THIS ROUTINE (AS THEY ARE IN SUBROUTINE PREVRUN OF NPBG15E1.FOR), 
C  THEY WILL BE HARDCODED BELOW.

        MAXOBDIM = 150
        MAXSUB = 800

    1   FORMAT(A20)

c  As of readi07.f, all the individual files needed by this module are 
c  already open (they were left open by it2beng22.f which calls this 
c  module), and so there is no need to call Subroutine SEPARATE to
c  parse the combined output file into these individual files. In fact,
c  all references to the combined output file, OUTFILE, have now been
c  removed from this module.

	
    2   FORMAT(A80)

        CALL GETNUM(NUMEQT)

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


C  NOTE THAT FILE 28 IS AT THE END OF ITS FILE.

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

C  READ NOFIX, PARFIX, AND VALFIX.

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

C  READ NRANFIX, PARRANFIX, AND RANFIXEST.

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


C  REWIND FILE 28 TO GET INFO.

	REWIND(28)

C  READ IN THE FOLLOWING FROM FILE 28.

C   YPREDPOP(JSUB,IEQ,IOBS,ICEN) = THE PREDICTED VALUE FOR Y FOR SUBJECT

C     JSUB, FOR OUTPUT EQUATION IEQ, FOR OBSERVATION IOBS, FOR ICEN =
C     1 (MEANS) OR 2 (MEDIANS), WHERE THE MEANS AND MEDIANS ARE FROM THE
C     FINAL CYCLE POPULATION ESTIMATES.

C   YPREDBAY(JSUB,IEQ,IOBS,ICEN) = THE PREDICTED VALUE FOR Y FOR SUBJECT
C     JSUB, FOR OUTPUT EQUATION IEQ, FOR OBSERVATION IOBS, FOR ICEN =
C     1 (MEANS) OR 2 (MEDIANS), WHERE THE MEANS AND MEDIANS ARE SUBJECT
C     JSUB'S MAP BAYESIAN POSTERIOR PARAMETER ESTIMATES.

C   PARBAY(JSUB,ICEN,J) = THE MAP BAYESIAN PARAMETER ESTIMATE, USING
C     THE POPULATION ESTIMATES FROM THE FINAL CYCLE
C     (ICEN=1 --> MEANS; ICEN=2 --> MEDIANS) FOR
C     SUBJECT JSUB FOR PARAMETER J.


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


C  REWIND FILE 25 AND READ IN IGAMMA(I), I=1,NUMEQT; AF(I),I=1,NDRUG;
C  TOL, MAXIT, AND RTOL.
  
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


C  READ IN AND STORE CYCLE INFO, BY CALLING SUBROUTINE
C  CONVERGE2, WHICH IS BASED ON SUBROUTINE CONVERGE OF ITBIG7.FOR.

        CALL CONVERGE2(NCYCLE,ALOGLIK,XMEAN,XMED,GAMMA,STDEV,
     1   PRCFVR,SUBMEAN,SUBSTD,SUBPERCOF,NAME,CHARTNO,SEX,NDD,NI,
     2   AGE,HEIGHT,NUMEQT,ASSAYC,AIC,BIC,ICONVERGE)


C  OPEN TEMP FILE, 21, AND WRITE ALL THE INFO INTO IT, RECORDING THE
C  STARTING LINE NOS. FOR EACH VARIABLE/ARRAY. THEN COPY THE FILE BACK
C  INTO OUTFILER, AFTER WRITING TO THE TOP OF OUTFILER THE "TABLE OF
C  CONTENTS".

        OPEN(21)

C  NLINE IS THE RUNNING CURRENT LINE NO.

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

C  REPLACE WRITING OF TOL WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL.

        XVERIFY(1) = TOL 
        CALL VERIFYVAL(1,XVERIFY)
C       WRITE(21,1102) TOL
        WRITE(21,1102) XVERIFY(1)
 1102   FORMAT(2X,F19.17,'   # STOPPING TOLERANCE')
        NLINE = NLINE + 1

        WRITE(21,1103) ICONVERGE
 1103   FORMAT(15X,I6,'   # CONVERGENCE FLAG ')
        NLINE = NLINE + 1

C  REPLACE WRITING OF RTOL WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL.

        XVERIFY(1) = RTOL 
        CALL VERIFYVAL(1,XVERIFY)
C       WRITE(21,1104) RTOL
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

C  REPLACE WRITING OF AF() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL.

        DO I = 1,NDRUG
         XVERIFY(1) = AF(I) 
         CALL VERIFYVAL(1,XVERIFY)
C        WRITE(21,1107) AF(I),I
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
C  NLRANPAR IS THE RANDOM PARAMETER NAME HEADER LINE NO.
     
        DO IVAR = 1,NVAR
         WRITE(21,1717) PAR(IVAR)
1717     FORMAT(A11)
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
  154   FORMAT(/8X,'   # PARAMETER BOUNDARIES',17X,'MUST EST. BE .GE. 0'
     1)
        NLINE = NLINE + 2
        NLAB = NLINE
C  NLAB IS THE BOUNDARIES HEADER LINE NO.       

C  NORAN IN THE LOOP BELOW IS THE RUNNING INDEX (OUT OF THE NP TOTAL
C  PARAMETERS) OF THE NEXT PARAMETER WHICH IS RANDOM.
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
C  NLAICBIC IS THE AIC AND BIC HEADER NO. LINE.

          WRITE(21,*) AIC,BIC
          NLINE = NLINE + 1  
    

        WRITE(21,159)
  159   FORMAT(/8X,'   # YPREDPOP ARRAY')
        NLINE = NLINE + 2
        NLYPREDPOP = NLINE
C  NLYPREDPOP IS THE YPREDPOP ARRAY HEADER NO. LINE.

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
C  NLYPREDBAY IS THE YPREDBAY ARRAY HEADER NO. LINE.

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
C  NLPARBAY IS THE PARBAY ARRAY HEADER NO. LINE.

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
C  NLCYCAVGLOGLIK IS THE ALOGLIK HEADER NO. LINE.

        DO ICYCLE = 1,NCYCLE
         WRITE(21,*) ALOGLIK(ICYCLE)
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

        WRITE(21,169)
  169   FORMAT(/8X,'   # CYCLE MEDIAN VECTORS')
        NLINE = NLINE + 2
        NLCYCMEDIAN = NLINE
C  NLCYCMEDIAN IS THE XMED ARRAY HEADER NO. LINE.

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
C  NLCYCSTDEV IS THE STDEV ARRAY HEADER NO. LINE.

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
C  NLCYCPRCFVR IS THE % COEFF. OF VAR. ARRAY HEADER NO. LINE.

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
C  NLCYCGAM IS THE GAMMA HEADER NO. LINE.

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
C  NLLASTCYCPAREST IS THE SUBMEAN ARRAY HEADER NO. LINE.

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
C  NLLASTCYCSTD IS THE SUBSTD ARRAY HEADER NO. LINE.

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
C  NLLASTCYCPER IS THE SUBPERCOF ARRAY HEADER NO. LINE.

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
1182     FORMAT(15X,F10.3,15X,A1,15X,F10.3)
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
  186   FORMAT(15X,I10,'   # START PAR')          

  260   READ(21,2) READLINE

        IF(READLINE(12:25) .NE. '# START PARFIX') THEN
         CALL CONDENSE2(READLINE)
         GO TO 260
        ENDIF

C  WRITE IN THE # START PARFIX LINE, BUT INCLUDE THE LINE NO., NLFIXPAR.

        WRITE(22,187) NLFIXPAR
  187   FORMAT(15X,I10,'   # START PARFIX') 

  960   READ(21,2) READLINE

        IF(READLINE(12:28) .NE. '# START PARRANFIX') THEN
         CALL CONDENSE2(READLINE)
         GO TO 960
        ENDIF

C  WRITE IN THE # START PARRANFIX LINE, BUT INCLUDE THE LINE NO., 
C  NLRANFIXPAR.

        WRITE(22,1187) NLRANFIXPAR
 1187   FORMAT(15X,I10,'   # START PARRANFIX')


  270   READ(21,2) READLINE

        IF(READLINE(12:25) .NE. '# START RANGES') THEN
         CALL CONDENSE2(READLINE)
         GO TO 270
        ENDIF

C  WRITE IN THE # START RANGES FOR NPAG LINE, BUT INCLUDE THE 
C  LINE NO., NLAB.

        WRITE(22,188) NLAB
  188   FORMAT(15X,I10,'   # START RANGES FOR NPAG, AND IF EST MUST BE >
     1 0') 

 1280   READ(21,2) READLINE

        IF(READLINE(12:25) .NE. '# START VALFIX') THEN
         CALL CONDENSE2(READLINE)
         GO TO 1280
        ENDIF

C  WRITE IN THE # START VALFIX LINE, BUT INCLUDE THE LINE NO., NLFIXVAL.

        WRITE(22,189) NLFIXVAL
  189   FORMAT(15X,I10,'   # START VALFIX')


1380   READ(21,2) READLINE

        IF(READLINE(12:28) .NE. '# START RANFIXEST') THEN
         CALL CONDENSE2(READLINE)
         GO TO 1380
        ENDIF

C  WRITE IN THE # START RANFIXEST LINE, BUT INCLUDE THE LINE NO., 
C  NLRANFIXVAL.

        WRITE(22,1189) NLRANFIXVAL
 1189   FORMAT(15X,I10,'   # START RANFIXEST')


  840   READ(21,2) READLINE

        IF(READLINE(12:25) .NE. '# START COVARI') THEN
         CALL CONDENSE2(READLINE)
         GO TO 840
        ENDIF

C  WRITE IN THE # START COVARIATE NAMES LINE, BUT INCLUDE THE LINE NO.,
C  NLCOVNAM.

        WRITE(22,841) NLCOVNAM
  841   FORMAT(15X,I10,'   # START COVARIATE NAMES')


  940   READ(21,2) READLINE

        IF(READLINE(12:25) .NE. '# START AIC AN') THEN
         CALL CONDENSE2(READLINE)
         GO TO 940
        ENDIF

C  WRITE IN THE # START AIC AND BIC LINE, BUT INCLUDE THE LINE NO.,
C  NLAICBIC.

        WRITE(22,941) NLAICBIC
  941   FORMAT(15X,I10,'   # START AIC AND BIC VALUES')



  320   READ(21,2) READLINE

        IF(READLINE(12:27) .NE. '# START YPREDPOP') THEN
         CALL CONDENSE2(READLINE)
         GO TO 320
        ENDIF

C  WRITE IN THE # START YPREDPOP LINE, BUT INCLUDE THE LINE NO., 
C  YPREDPOP.

        WRITE(22,193) NLYPREDPOP
  193   FORMAT(15X,I10,'   # START YPREDPOP')


  330   READ(21,2) READLINE

        IF(READLINE(12:27) .NE. '# START YPREDBAY') THEN
         CALL CONDENSE2(READLINE)
         GO TO 330
        ENDIF

C  WRITE IN THE # START YPREDBAY LINE, BUT INCLUDE THE LINE NO., 
C  YPREDBAY.

        WRITE(22,194) NLYPREDBAY
  194   FORMAT(15X,I10,'   # START YPREDBAY')



  350   READ(21,2) READLINE

        IF(READLINE(12:25) .NE. '# START PARBAY') THEN
         CALL CONDENSE2(READLINE)
         GO TO 350
        ENDIF

C  WRITE IN THE # START PARBAY LINE, BUT INCLUDE THE LINE NO., 
C  NLPARBAY.

        WRITE(22,197) NLPARBAY
  197   FORMAT(15X,I10,'   # START PARBAY')



  370   READ(21,2) READLINE

        IF(READLINE(12:28) .NE. '# START CYCLE AVE') THEN
         CALL CONDENSE2(READLINE)
         GO TO 370
        ENDIF

C  WRITE IN THE # START CYCLE AVERAGE LOG-LIKS LINE, BUT INCLUDE THE
C  LINE NO., NLCYCAVGLOGLIK.
 
        WRITE(22,199) NLCYCAVGLOGLIK  
  199   FORMAT(15X,I10,'   # START CYCLE AVERAGE LOG-LIKS')


 
  380   READ(21,2) READLINE

        IF(READLINE(12:28) .NE. '# START CYCLE MEA') THEN
         CALL CONDENSE2(READLINE)
         GO TO 380
        ENDIF

C  WRITE IN THE # START CYCLE MEANS LINE, BUT INCLUDE THE
C  LINE NO., NLCYCMEAN.

        WRITE(22,201) NLCYCMEAN
  201   FORMAT(15X,I10,'   # START CYCLE MEANS')


1370   READ(21,2) READLINE

        IF(READLINE(12:28) .NE. '# START CYCLE MED') THEN
         CALL CONDENSE2(READLINE)
         GO TO 1370
        ENDIF

C  WRITE IN THE # START CYCLE MEDIANS LINE, BUT INCLUDE THE
C  LINE NO., NLCYCMEDIAN

        WRITE(22,1199) NLCYCMEDIAN
 1199   FORMAT(15X,I10,'   # START CYCLE MEDIANS')


  390   READ(21,2) READLINE

        IF(READLINE(12:28) .NE. '# START CYCLE STD') THEN
         CALL CONDENSE2(READLINE)
         GO TO 390
        ENDIF

C  WRITE IN THE # START CYCLE STD. DEVS. LINE, BUT INCLUDE THE
C  LINE NO., NLCYCSTDEV

        WRITE(22,202) NLCYCSTDEV
  202   FORMAT(15X,I10,'   # START CYCLE STD. DEVS.')


  410   READ(21,2) READLINE

        IF(READLINE(12:28) .NE. '# START CYCLE % C') THEN
         CALL CONDENSE2(READLINE)
         GO TO 410
        ENDIF

C  WRITE IN THE # START CYCLE % COEFF. OF VARS. LINE, BUT INCLUDE
C  THE LINE NO., NLCYCPRCFVR

        WRITE(22,203) NLCYCPRCFVR
  203   FORMAT(15X,I10,'   # START CYCLE % COEFF. OF VARS.')


  420   READ(21,2) READLINE

        IF(READLINE(12:28) .NE. '# START CYCLE GAM') THEN
         CALL CONDENSE2(READLINE)
         GO TO 420
        ENDIF

C  WRITE IN THE # START CYCLE GAMMA EST. VALUES LINE, BUT INCLUDE
C  THE LINE NO., NLCYCGAM

        WRITE(22,204) NLCYCGAM
  204   FORMAT(15X,I10,'   # START CYCLE GAMMA EST. VALUES')


  430   READ(21,2) READLINE

        IF(READLINE(12:39) .NE. '# START LAST CYCLE SUBJ. PAR') THEN
         CALL CONDENSE2(READLINE)
         GO TO 430
        ENDIF

C  WRITE IN THE # START LAST CYCLE SUBJ. PAR. ESTIMATES LINE, BUT 
C  INCLUDE THE LINE NO., NLLASTCYCPAREST.
 



        WRITE(22,206) NLLASTCYCPAREST  
  206   FORMAT(15X,I10,'   # START LAST CYCLE SUBJ. PAR. ESTIMATES')


  450   READ(21,2) READLINE

        IF(READLINE(12:39) .NE. '# START LAST CYCLE SUBJ. STD') THEN
         CALL CONDENSE2(READLINE)
         GO TO 450
        ENDIF

C  WRITE IN THE # START LAST CYCLE SUBJ. STD. DEVS. LINE BUT 
C  INCLUDE THE LINE NO., NLLASTCYCSTD.
 


        WRITE(22,208) NLLASTCYCSTD 
  208   FORMAT(15X,I10,'   # START LAST CYCLE SUBJ. STD. DEVS.')


  440   READ(21,2) READLINE

        IF(READLINE(12:37) .NE. '# START LAST CYCLE SUBJ. %') THEN
         CALL CONDENSE2(READLINE)
         GO TO 440
        ENDIF

C  WRITE IN THE # START LAST CYCLE SUBJ. % COEFF. OF VARS. LINE BUT
C  INCLUDE THE LINE NO., NLLASTCYCPER.




        WRITE(22,207) NLLASTCYCPER  
  207   FORMAT(15X,I10,'   # START LAST CYCLE SUBJ. % COEFF. OF VARS.')



  480   READ(21,2) READLINE

        IF(READLINE(12:30) .NE. '# START PATIENT IDS') THEN
         CALL CONDENSE2(READLINE)
         GO TO 480
        ENDIF

C  WRITE IN THE # START PATIENT IDS LINE BUT INCLUDE
C  THE LINE NO., NLPATID.
 
        WRITE(22,212) NLPATID
  212   FORMAT(15X,I10,'   # START PATIENT IDS')


  490   READ(21,2) READLINE

        IF(READLINE(12:30) .NE. '# START PATIENT DOS') THEN
         CALL CONDENSE2(READLINE)
         GO TO 490
        ENDIF

C  WRITE IN THE # START PATIENT DOSE COV. BLOCKS LINE BUT INCLUDE
C  THE LINE NO., NLPATDOS.
 
        WRITE(22,213) NLPATDOS
  213   FORMAT(15X,I10,'   # START PATIENT DOSE COV. BLOCKS')


  530   READ(21,2) READLINE

        IF(READLINE(12:30) .NE. '# START PATIENT OUT') THEN
         CALL CONDENSE2(READLINE)
         GO TO 530

        ENDIF

C  WRITE IN THE # START PATIENT OUTPUT AND ASSAY COEFF. BLOCKS LINE
C  BUT INCLUDE THE LINE NO., NLPATOUTASSAY.


        WRITE(22,214) NLPATOUTASSAY
  214   FORMAT(15X,I10,'   # START PATIENT OUTPUT AND ASSAY COEFF. BLOCK
     1S')

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
C     SUBROUTINE SEPARATE WAS REMOVED AS OF readi07.f
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

c  As of readi07.f, SINCE IFILE IS HARDCODED IN THE CALLING STATEMENT TO
c  THIS ROUTINE TO BE 25, simplify the code below so that file 25 is
c  always read.

c 4210	IF(IFILE .EQ. 23) READ(23,3,ERR=4200) READLINE

 4210 READ(25,3,ERR=4200) READLINE

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
        SUBROUTINE FILREDT(NOBSER,YO,C0,C1,C2,C3,MAXOBDIM,NDRUG,ND,
     1   NADD)

C  FILREDT IS CALLED BY SUBROUTINE PREVRUN TO READ THE PORTION OF 
C  SCRATCH FILE 27 WHICH APPLIES TO THE SUBJECT UNDER CONSIDERATION. THE
C  'POINTER' FOR FILE 27 IS IN THE PROPER POSITION TO BEGIN READING THE
C  INFO FOR THE DESIRED SUBJECT.

        IMPLICIT REAL*8(A-H,O-Z)
        PARAMETER(MAXNUMEQ=7)

        DIMENSION YO(MAXOBDIM,MAXNUMEQ),RJUNK(34),C0(MAXNUMEQ),
     1  C1(MAXNUMEQ),C2(MAXNUMEQ),C3(MAXNUMEQ)

C SIG, RS, AND BS REMOVED IN DIMENSION STMT. - THEY AREN'T USED IN THIS 
C ROUTINE.

	CHARACTER READLINE*1000
C  SEX REMOVED IN ABOVE CHARACTER STMT. IT IS NOT USED IN THIS ROUTINE
C  ... AS OF READ5.F.


C  AS OF readi02.f, THE FORMAT FOR THE WORKING COPY FILES IS:
C     COL 1 = TIME
C     COL 2 = IV FOR DRUG 1; COL 3 = PO FOR DRUG 1;
C     COL 4 = IV FOR DRUG 2; COL 5 = PO FOR DRUG 2;
C     ... EACH SUCCEEDING DRUG HAS AN IV FOLLOWED BY A PO COLUMN.
C     NEXT NADD COLUMNS = ONE FOR EACH ADDITIONAL COVARIATE (ADDITIONAL
C      REFERS TO ANY EXTRA COVARIATES BEYOUND THE 4 PERMANENT ONES IN
C      COMMON DESCR (SEE BELOW).


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

	IF(NDRUG .GT. 7) THEN

	 WRITE(*,124)
  124    FORMAT(' YOUR PATIENT DATA FILES CANNOT HAVE MORE THAN 7'/
     1' DRUGS. THE PROGRAM IS NOW STOPPING. '/)
	 CALL PAUSE
	 STOP
	ENDIF

        READ(27,3) NADD

C  NOTE THAT THE NO. OF "RATES" INCLUDES 2 FOR EACH DRUG (THE IV AND
C  THE PO COLUMNS) + NADD (1 COLUMN FOR EACH ADDITIONAL COVARIATE).

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
         READ(27,*) XJUNK,(YO(I,J),J=1,NUMEQT)
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
        SUBROUTINE CONVERGE2(NCYCLE,ALOGLIK,XMEAN,XMED,GAMMA,STDEV,
     1   PRCFVR,SUBMEAN,SUBSTD,SUBPERCOF,NAME,CHARTNO,SEX,NDD,NI,
     2   AGE,HEIGHT,NUMEQT,ASSAYC,AIC,BIC,ICONVERGE)


C  THIS IS AN EDITED VERSION OF SUBROUTINE CONVERGE IN ITBIG7.FOR. 
C  THIS ROUTINE JUST INPUTS AND STORES VALUES FROM THE OUTPUT FILE OF A
C  BIG IT2B RUN. IT DOESN'T PLOT THOSE VALUES; IT RETURNS THEM TO MAIN.

C  THIS SUBROUTINE IS CALLED BY MAIN.

C  INPUT IS:

C  FILE 45, THE OUTPUT FILE, ALREADY OPENED.


C  OUTPUT IS:

C  THE VALUES IN THE ARGUMENTS TO THIS SUBROUTINE.

        IMPLICIT REAL*8(A-H,O-Z)
        PARAMETER(MAXNUMEQ=7)

        CHARACTER READLINE*1000,NAME(800)*53,CHARTNO(800)*53,SEX(800)*1

        DIMENSION ALOGLIK(40999),XMEAN(40999,30),XMED(40999,30),
     1  GAMMA(40999,MAXNUMEQ),STDEV(40999,30),PRCFVR(40999,30),
     2  AGE(800),HEIGHT(800),
     3  SUBMEAN(800,30),SUBSTD(800,30),SUBPERCOF(800,30),NDD(800),
     4  ASSAYC(800,MAXNUMEQ,4)

C  NOTE THAT THE DIMENSIONS FOR MAXSUB AND MAXOBDIM HAVE BEEN HARDCODED
C  TO BE, RESPECTIVELY, 800 AND 150. THIS COULD BE AVOIDED BY PROVIDING 
C  THESE VALUES IN THE ARGUMENT LIST OF THIS ROUTINE. NOTE THAT THE
C  MAXIMUM ALLOWABLE VALUE FOR NI (2*NDRUG + NADD) IS 34 --> LAST 
C  DIMENSION OF DOSEBLOCK IS 1 + 34 = 35. NOTE ALSO THAT THE MAX. NO.
C  OF DOSE EVENTS IS 1000. BUT, AS OF readi05.f, DOSEBLOCK IS PASSED
C  IN COMMON/DOSEOBS (ALONG WITH OBSBLOCK) FROM SUBROUTINE NEWWORK1
C  OF it2beng20.f, RATHER THAN READ FROM FILE 45 BELOW.

        REWIND(26)

    2   FORMAT(A1000)

C OBTAIN THE NAME AND NUMBER OF THE PATIENT DATA FILES, THE NO. OF 
C VARIABLES, AND THE VARIABLE NAMES. 



   50   READ(26,2) READLINE

     	  IF(READLINE(10:28) .NE. 'OF RANDOM VARIABLES') GO TO 50
        BACKSPACE(26)

        READ(26,53) NVAR
   53   FORMAT(T33,I2)

C  IPLACE = (1,2,3,4,5,6) IF THE NEXT INFO TO READ IS, RESPECTIVELY, 
C           THE CYCLE NO., THE AVG LOG-LIK, THE MEANS AND MEDIANS, THE
C	    STDEVS, THE PERCENT COFF. OF VARS, OR THE UPDATED GAMMA
C	    ESTIMATE(S) (NOTE THAT IF GAMMA(IEQ) WASN'T ESTIMATED, THE
C	    UPDATED ESTIMATE FOR THAT GAMMA WILL BE FIXED = 1.0, AND
C	    THEN THE GRAPH FOR GAMMA(IEQ) WILL SIMPLY BE A HORIZONTAL 
C	    LINE AT A VALUE OF 1.0).

C  I.E., FOR EACH CYCLE, THE ABOVE INFO IS READ IN THE ORDER GIVEN.

C  ICYC IS THE RUNNING INDEX OF THE NO. OF CYCLES FOR THE RUN.

     	ICYC=1 
	IPLACE=1


   10	READ(26,2,IOSTAT=IEND) READLINE

C  NOTE THAT IEND .LT. 0 --> END OF FILE REACHED.
C  TO REACH THE END OF THE FILE AT THIS POINT --> SOMETHING IS WRONG 
C  WITH THE FILE SINCE THE LAST CYCLE PARAMETER RESULTS HAVEN'T BEEN
C  READ IN.

        IF(IEND .LT. 0) THEN
         WRITE(*,217)
  217    FORMAT(/' SOMETHING IS WRONG WITH THE OUTPUT FILE ENTERED;'/
     1' THERE ARE NO LAST CYCLE PARAMETER INFO RESULTS. THE PROGRAM'/
     2' STOPS.'//)
         CALL PAUSE
         STOP
        ENDIF


C  CHECK TO SEE IF THIS LINE BEGINS THE LAST CYCLE PARAMETER INFO
C  RESULTS. IF SO, FIRST BACK UP TO READ THE AIC AND BIC VALUES,
C  AND THEN THE CONVERGENCE LINE (TELLING WHY THE PROGRAM STOPPED),

C  AND THEN GO TO LABEL 100 AND READ THAT INFO.

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

C  THE ABOVE ENDIF IS FOR THE   
C  IF(READLINE(14:33) .EQ. 'LAST CYCLE PARAMETER') CONDITION.
 	

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
    
C  NCYCLE IS RETURNED TO THE CALLING PROGRAM AS THE NO. OF CYCLES IN 
C  THE OUTPUT FILE.

C  NOW READ IN THE LAST CYCLE PARAMETER INFO PART OF THE COMBINED
C  OUTPUT FILE.

C  INITIALIZE INDSUB TO BE 0. IT IS THE RUNNING NO. OF SUBJECTS IN THE
C  OUTPUT FILE.

        INDSUB = 0


C  READ IN LAST CYCLE PARAMETER INFO HERE, FOR EACH OF THE SUBJECTS IN 
C  THE FILE.

  110	  READ(26,2,IOSTAT=IEND) READLINE

C  NOTE THAT IEND .LT. 0 --> END OF FILE REACHED. IF IT IS REACHED
C  AT THIS POINT, SOMETHING IS WRONG. PRINT ERROR MESSAGE AND
C  STOP.

        IF(IEND .LT. 0) THEN
         WRITE(*,417)
  417    FORMAT(/' SOMETHING IS WRONG WITH THE OUTPUT FILE ENTERED;'/
     1' THE END OF THE FILE OCCURRED BEFORE ALL THE PATIENT DATA WAS'/
     2' READ IN. THE PROGRAM STOPS: '//)
         CALL PAUSE
         STOP
        ENDIF

C  CHECK TO SEE IF THIS LINE BEGINS THE PATIENT DATA INFO. IF SO,
C  GO TO LABEL 200 AND READ THAT INFO.

      IF(READLINE(28:43) .EQ. 'THE PATIENT DATA') GO TO 200

C  READ IN THE LAST CYCLE PARAMETER INFO FOR THIS SUBJECT.

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

C  READ IN PATIENT DATA INFO FROM HERE ON, FOR EACH OF THE SUBJECTS IN 
C  THE FILE.

  210	  READ(26,2,IOSTAT=IEND) READLINE

C  NOTE THAT IEND .LT. 0 --> END OF FILE REACHED.
C  TO REACH THE END OF THE FILE AT THIS POINT --> SOMETHING IS WRONG 
C  WITH THE FILE SINCE THE PATIENT DATA INFO HASN'T BEEN READ IN.

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

C  READLINE NOW CONTAINS THE NO. OF DRUGS, NDRUG. BACKSPACE AND READ 
C  NDRUG; THEN READ THE NO. OF ADDITIONAL COVARIATES, THE NO. OF
C  DOSE EVENTS, AND THE DOSAGE BLOCK. 
C  NO. AS OF readi05.f, DOSEBLOCK AND OBSBLOCK ARE NO LONGER READ
C  FROM FILE 45 BELOW (THEY ARE PASSED IN COMMON/DOSEOBS VIA
C  SUBROUTINE NEWWORK1 IN it2beng20.f). 

         BACKSPACE(26)
    3    FORMAT(T2,I5)
         READ(26,3) NDRUG
         READ(26,3) NADD

C  NOTE THAT THE NO. OF "RATES" INCLUDES 2 FOR EACH DRUG (THE IV AND
C  THE PO COLUMNS) + NADD (1 COLUMN FOR EACH ADDITIONAL COVARIATE).

         NI = 2*NDRUG + NADD
         READ(26,3) ND
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
C  NO. AS OF readi05.f, DOSEBLOCK AND OBSBLOCK ARE NO LONGER READ
C  FROM FILE 45 BELOW (THEY ARE PASSED IN COMMON/DOSEOBS VIA
C  SUBROUTINE NEWWORK1 IN it2beng20.f). 

         BACKSPACE(26)

         READ(26,3) NUMEQT
         READ(26,3) M

         GO TO 210

        ENDIF

C  THE ABOVE ENDIF IS FOR THE IF(READLINE(12:23) .EQ. 'NO. OF TOTAL') 
C  CONDITION.


        IF(READLINE(1:25) .EQ. 'ASSAY COEFFICIENTS FOLLOW') THEN

C  READ THE ASSAY COEFFICIENTS ON THE NEXT NUMEQT ROWS, ONE FOR EACH
C  OF THE OUTPUT EQUATIONS.

         DO J = 1,NUMEQT
          READ(26,*) (ASSAYC(INDSUB,J,K),K=1,4)  
         END DO

         GO TO 210

        ENDIF


C  THE ABOVE ENDIF IS FOR THE 
C  IF(READLINE(1:25) .EQ. 'ASSAY COEFFICIENTS FOLLOW') CONDITION.



        GO TO 210


  400   RETURN

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
	SUBROUTINE GETCOVR2(NCOV,DESCR)
	IMPLICIT REAL*8(A-H,O-Z)
	CHARACTER READLINE*1000,DESCR(26)*20

C  THIS ROUTINE IS CALLED BY MAIN TO OBTAIN, FROM FILE 27, THE NO. OF
C  COVARIATES (NCOV), AND THEIR NAMES(DESCR(I),I=1,NCOV). THIS ROUTINE
C  IS AN EDITED VERSION OF GETCOVR2 IN NPBG15E1.FOR.

    2   FORMAT(A20)
   33   FORMAT(A1000)

	REWIND(27)

C  THE NO. OF ADDITIONAL COVARIATES IS ON THE LINE HAVING 
C  "NO. OF ADDITIONAL COVARIATES" STARTING IN ENTRY 12.

   10	READ(27,33) READLINE
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


   20	READ(27,33) READLINE
	IF(READLINE(2:16) .NE. 'COVARIATE NAMES') GO TO 20


        IF(NCOV .GE. 1) THEN
  
         DO J = 1,NCOV

          READ(27,33) READLINE

C  FOR THIS COVARIATE, ESTABLISH DESCR(J) AS THE PORTION OF THE 
C  CURRENT READLINE UP TO THE  FIRST SPACE, WHICH WILL BE ASSUMED TO BE 
C  THE END OF THE COV. NAME. IN CASE THERE IS A SPACE OR TWO AT THE 
C  BEGINNING OF READLINE FOR SOME REASON, START CHECKING FOR SPACES AT 
C  ENTRY 3.

          DO I = 3,20
           IF(READLINE(I:I) .EQ. ' ') GO TO 30
          END DO

   30     DESCR(J) = READLINE(1:I-1)

         END DO

        ENDIF



	REWIND(27)

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


c  shift9.f                                                9/28/12

c  shift9 has the following subtle change from shift8:

c  In step 4, the logic to assign the bolus time, BOL(I,IND,1) is
c  simplified in the case where a steady state dose set begins as a
c  time reset event. In this case, the bolus time will be TAU(I) only
c  if both TAU(I) and the bolus value (RR) are not 0. See the reason
c  in the code. 

c-----------------------------------------------------------------------

c  shift8.f                                                9/20/12

c  shift8 has changes from shift7 in Step 4 to correct the code in the
c  case where bolus inputs are used in steady state dose sets. In 
c  shift7.f, a timelag for a bolus which was part of a steady state
c  dose set would not be applied properly. Now it will.

c-----------------------------------------------------------------------

c  shift7.f                                                11/6/11

c  shift7 differs from shift6 as follows:

c  1. The dimensions related to the no. of dose events are changed from
c  500 to 5000. This is needed as shift7 is compiled with idm1x7.f, 
c  idm2x7.f, and idm3x7.f (part of the npageng16.f "engine"), which
c  accommodates steady state dose sets.

c  2. 3 lines testing for IF(SIG(IDOSE) .EQ. 0 .AND. IDOSE .GT. 1)
c  are replaced by 	  IF(SIG(IDOSE) .LE. 0 .AND. IDOSE .GT. 1)
c  since now a dose reset occurs when a dose time is 0 (a regular
c  time reset) or < 0 (a time reset occurring with a steady state
c  dose set indicator).

c-----------------------------------------------------------------------

C  SHIFT6.F                                                4/26/11

C  SHIFT5 HAS THE FOLLOWING CHANGES TO SHIFT5:

C  WT AND CCR ARE NO LONGER ASSUMED TO BE SPECIAL COVARIATES IN EACH
C  PATIENT'S WORKING COPY PATIENT DATA FILE. SO ALL DO LOOPS THAT 
C  START WITH  DO I = 1, 2+NADD ARE CHANGED TO START WITH DO I = 1,NADD,
C  BUT ONLY IF NADD .GT. 0.

C-----------------------------------------------------------------------

C  SHIFT5.F							9/11/09

C  SHIFT5 HAS THE FOLLOWING CHANGES TO SHIFT4.F.


C  THE ARGUMENT LIST CONTAINS TAU(.) RATHER THAN NTLAG(.). THIS
C  MEANS THAT TAU(I) IS INPUT DIRECTLY AS THE TIMELAG FOR DRUG I.
C  I.E., IT NO LONGER HAS TO BE CALCULATED AS A FUNCTION OF THE
C  PARAMETER ARRAY, P. BECAUSE OF THIS, P IS REMOVED FROM THE ARGUMENT
C  LIST AND THE DIMENSION STATEMENT. ALSO, NTLAG IS REMOVED FROM 
C  THT DIMENSION STATEMENT.

C  THE FIRST SET OF ID MODULES TO CALL SHIFT5.F ARE idm1x3.f, 
C  idm2x3.f, AND idm3x3.f

C-----------------------------------------------------------------------

C  SHIFT4.FOR							9/1/09

C  SHIFT4 HAS THE FOLLOWING CHANGES FROM SHIFT3:

C  1. NTLAG(I) CAN NOW BE NEGATIVE. IF THIS OCCURS, IT MEANS THAT THE
C  TIMELAG PARAMETER FOR DRUG I WILL BE EXP(P(-NTLAG(I)).

C  2. A BUG IS CORRECTED RELATED TO TIME "RESETS". PREVIOUSLY, IF THE
C  USER HAD A TIME "RESET" IN HIS DOSAGE REGIMEN, THIS ROUTINE WOULD
C  NOT WORK. THE REASON IS THAT IN THE CODE BELOW, EACH NEXT TIME
C  FOR AN IV, COVARIATE, OR BOLUS IS COMPARED TO THE PREVIOUSLY
C  ESTABLISHED TIME IN THE DOSAGE ARRAY (TIMNXT) AND IS A CANDIDATE
C  TO BE THE NEXT TIMNXT IF IT IS .GE. TIMNXT. SO IF A TIME RESET
C  VALUE OF 0 OCCURS, IT WILL NEVER BE A CANDIATE SINCE IT IS NOT
C  .GE. THE LAST TIMNXT. TO FIX THIS, AND MAKE SURE THAT A TIME
C  RESET VALUE OF 0 IS INCLUDED IN THE ADJUSTED DOSAGE BLOCK, THE
C  CODE WILL ADD TO EACH IV, BOLUS, AND COVARIATE ARRAY AN EXTRA
C  LINE WHEN A TIME RESET OCCURS. THIS LINE WILL HAVE A TIME OF
C  1.D19 (I.E., A LARGE VALUE WHICH REPRSENTS INFINITY); AND IT
C  WILL BE FOLLOWED BY A LINE WITH THE ADJUSTED RESET TIME (0 FOR
C  IVs AND COVARIATES, AND 0 + TAU(I) FOR BOLI.

C-----------------------------------------------------------------------

C  SHIFT3.FOR							5-23-02

C  SHIFT3 HAS MAJOR CHANGES FROM SHIFT2 TO ALLOW FOR MULTIPLE TIMELAGS,
C  ONE POTENTIALLY FOR EACH BOLUS INPUT OF UP TO 7 DRUGS.

	SUBROUTINE Old_SHIFT(TAU,ND,SIG,NDRUG,NADD,RS)

        use npag_utils, only : thesame

	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION SIG(5000),RS(5000,34),TAU(7),XIV(7,5000,2),
     1  BOL(7,5000,2),COV(20,5000,2),INDIV(7),INDBOL(7),INDCOV(20),
     2  TIMCAN(34)

C  INPUT ARE:

C  TAU(I) =  THE VALUE OF THE TIMELAG FOR DRUG I.
C  ND = ORIGINAL NO. OF DOSE EVENTS. 
C  SIG(I) = TIME FOR ITH DOSE EVENT IN THE ORIGINAL DOSAGE REGIMEN,
C           I=1,ND.
C  NDRUG = NO. OF DRUGS (EACH HAS AN IV, FOLLOWED BY A BOLUS COLUMN).
C  NADD = NO. OF ADDITIONAL COVARIATES (EACH IS IN ITS OWN COLUMN
C         FOLLOWING THE IV/BOLUS COLUMNS.
C  RS(I,J) = "RATE" J FOR THE ITH DOSE EVENT IN THE ORIGINAL DOSAGE
C            REGIMEN; J=1,NI, I=1,ND, WHERE NI = 2*NDRUG + NADD
C            BECAUSE THE "RATES" CONTAIN, IN ORDER, 2 ENTRIES FOR
C            EACH DRUG (1 FOR THE IV AND 1 FOR THE BOLUS) AND 1 EACH
C            FOR THE NADD ADDITIONAL COVARIATES.


C  OUTPUT ARE:

C  ND, SIG, RS, AS ABOVE, EXCEPT FOR THE ALTERED DOSAGE REGIMEN.

C-----------------------------------------------------------------------

C  SHIFT2.FOR							11-16-99

C  SHIFT2 HAS THE FOLLOWING CHANGE FROM SHIFT. AT THE END OF THE 
C  FORMATION OF ARRAY XMAT, ALL ROWS WHICH HAVE 0 BOLUS INPUT AND THE
C  SAME OTHER DATA VALUES (EXCEPT TIME) AS THE PREVIOUS ROW ARE NOT
C  USED IN THE NEW ARRAY XMAT2 WHICH HAS ONLY NON-REDUNDANT ROWS.
C  THIS, THEORETICALLY, SHOULDN'T HAVE ANY EFFECT ON CALCULATIONS, BUT 
C  NUMERICALLY IT DOES SINCE WHEN THE DVODE ROUTINE SOLVES D.E.'S, IT 
C  INTEGRATES OVER DIFFERENT INTERVALS IF EXTRA DOSAGE LINES ARE 
C  INCLUDED.

C  EX: TIME   IV   BOLUS	TIME   IV   BOLUS
C       0    100     0		 0    100     0
C       5    100   1000		 2    100   1000  

C  NOTE THAT BOTH ABOVE CASES SHOULD GIVE THE SAME RESULTS IF THERE IS
C  A TIME-LAG = 3 IN THE 2ND CASE. BUT, AS THE CODE IS WRITTEN IN
C  SHIFT.FOR, THE 2ND CASE WOULD TRANSLATE TO THE FOLLOWING:

C	 TIME   IV   BOLUS 
C         0    100     0   
C         2    100     0
C         5    100   1000

C  ... AND THIS WOULD MEAN THAT THE 1ST INTEGRATION BY DVODE WOULD END
C      AT T = 2, RATHER THAN 5 (OR, E.G., 3 IF 3 WAS THE
C      FIRST OBSERVATION TIME). THIS CREATES NUMERICAL DIFFERENCES DUE
C      TO SMALL ROUNDOFF ERRORS WHICH CAN GROW SIGNIFICANTLY.

C-----------------------------------------------------------------------

C  SHIFT.FOR							7-27-99

C  SHIFT.FOR IS A MODULE WHICH INCLUDES SUBROUTINE SHIFT. SHIFT WILL BE
C  CALLED BY ROUTINES OF THE "BIG" NPEM AND IT2B PROGRAMS WHICH HAVE
C  SUBROUTINES FUNC, FUNC1, FUNC2, OR FUNC3 IN THEM.

C  SHIFT INPUTS THE DOSAGE REGIMEN VIA THE INPUT ARGUMENTS (SEE BELOW),
C  AND RETURNS AN ALTERED DOSAGE REGIMEN, WHICH HAS EACH BOLUS INPUT 
C  TIME INCREASED BY THE INPUT VALUE OF TAU (THE TIME LAG). NOTE THAT
C  EACH ROW WITH A NON-0 BOLUS INPUT VALUE WILL RESULT IN A NEW ROW IN
C  THE DOSAGE REGIMEN.

C-----------------------------------------------------------------------

C  PROCEDURE FOR THE DOSAGE REGIMEN MODIFICATION:

C  1. ESTABLISH TAU(I) AS THE TIMELAG FOR DRUG I'S BOLUS COLUMN.
C     NO. AS OF SHIFT5.F, THIS VALUE IS INPUT AS AN ARGUMENT.

C  2. ESTABLISH THE IV VALUES AND TIMES INTO XIV(I,J,K). IN PARTICULAR,
C     XIV(I,J,2) IS THE JTH IV VALUE FOR DRUG I, AND XIV(I,J,1) IS THE 
C     TIME THIS IV VALUE FIRST OCCURRED. SET THE LAST TIME TO 1.D29 AS
C     AN INDICATOR THAT THERE ARE NO MORE ENTRIES IN THE ARRAY.

C  3. ESTABLISH THE COVARIATE VALUES AND TIMES INTO COV(I,J,K). IN 
C     PARTICULAR, COV(I,J,2) IS THE JTH VALUE FOR COVARIATE I, AND 
C     COV(I,J,1) IS THE TIME THIS COV VALUE FIRST OCCURRED. SET THE 
C     LAST TIME TO 1.D29 AS AN INDICATOR THAT THERE ARE NO MORE ENTRIES
C     IN THE ARRAY.

C  4. ESTABLISH THE BOLUS VALUES AND TIMES INTO BOL(I,J,K).
C     IN PARTICULAR, BOL(I,J,2) IS THE JTH BOLUS VALUE FOR DRUG I, AND
C     BOL(I,J,1) IS THE TIME THIS BOLUS OCCURRED. THE TIMES FOR EACH
C     BOLUS VALUE ARE THOSE ADJUSTED TIMES FROM THE ASSOCIATED TIMELAGS
C     TAU(I),I=1,NDRUG, FROM STEP 1. SET THE LAST TIME TO 1.D29 AS AN
C     INDICATOR THAT THERE ARE NO MORE ENTRIES IN THE ARRAY.

C  5. REASSIGN THE VALUES IN IV, BOL, AND COV TO THE APPROPRIATE ENTRIES
C     OF RS, KEEPING TRACK OF THE RUNNING INDEX, ND, OF DOSE EVENTS. IF
C     ND EXCEEDS 5000, STOP THE PROGRAM WITH A MESSAGE TO THE USER. ALSO
C     REASSIGN THE CORRESPONDING TIME VALUES TO ARRAY SIG.


C  STEP 1.

C  NOTHING TO DO. AS OF SHIFT5.F, TAU(I), I=1,NDRUG, IS INPUT AS
C  AN ARGUMENT TO THIS ROUTINE.


C  STEP 2:

C  ESTABLISH THE IV VALUES AND TIMES INTO XIV(I,J,K). IN PARTICULAR,
C  XIV(I,J,2) IS THE JTH IV VALUE FOR DRUG I, AND XIV(I,J,1) IS THE 
C  TIME THIS IV VALUE FIRST OCCURRED.

	DO I = 1,NDRUG

C  ESTABLISH XIV(I,J,K) FOR DRUG I'S IV. PRESET THE LAST VALUE TO
C  -99 SO THAT THE FIRST VALUE WILL BE DIFFERENT AND THEREFORE ENGAGE 
C  THE LOGIC (WHICH ONLY WRITES A ROW INTO THE ARRAY IF THE VALUE IS
C  DIFFERENT THAN THE PREVIOUS VALUE). 

C*** MODIFICATION IN SHIFT4.F: IF A TIME RESET OCCURS (I.E., A
C    SIG(IDOSE) = 0, WHERE IDOSE > 1), IT WILL BE HANDLED BY ASSIGNING 
C    AN EXTRA TIME VALUE OF 1.D19 (I.E., A LARGE VALUE REPRESENTING
C    TIME = INFINITY) TO THE IV TIME ARRAY. THEN THE REST OF THE
C    THE IV TIME ARRAY WILL BE ESTABLISHED WITH THE REST OF THE VALUES
C    IN SIG, STARTING, OF COURSE, WITH THE TIME RESET VALUE OF 0.

C    THE SAME LOGIC WILL APPLY TO THE COVARIATES AND THE BOLI.

C  NOTE THAT IND WILL BE THE RUNNING INDEX OF THE LATEST ENTRY INTO 
C  THE ARRAY. PLACE 1.D29 INTO THE LAST TIME ENTRY OF EACH SUB-ARRAY 
C  AS AN INDICATOR THAT THERE ARE NO MORE ENTRIES.

	 XIV(I,1,1) = 1.D29
	 IND = 0
	 VALAST = -99.D0

C  FOR DRUG I, THE IV VALUE IS IN COLUMN 2*I-1 OF ARRAY RS.
	  
	DO IDOSE = 1,ND

	  RR = RS(IDOSE,2*I-1)

C*** MODIFICATION IN SHIFT7.F: A TIME RESET IS NOW DESIGNATED BY A
C  SIG(IDOSE) .LE. 0, RATHER THAN JUST .EQ. 0 (SINCE A STEADY STATE
C  DOSE INDICATOR HAS A NEGATIVE DOSE TIME).

	  IF(SIG(IDOSE) .LE. 0 .AND. IDOSE .GT. 1) THEN

C  THIS REPRESENTS A TIME "RESET". IN THIS CASE, AS INDICATED ABOVE,
C  PUT IN AN EXTRA ROW FOR THE IV REPRESENTING A VERY LARGE TIME
C  AND THE SAME IV VALUE AS THE PREVIOUS VALUE. THEN PUT IN THE
C  LINE REPRESENTING THE RESET TIME OF 0.
	
	    IND = IND + 1
	    XIV(I,IND,1) = 1.D19
	    XIV(I,IND,2) = XIV(I,IND-1,2)

	    IND = IND + 1

C*** MODIFICATION IN SHIFT7.F. SET THE NEXT XIV(I,IND,1) TO BE
C  SIG(IDOSE), NOT 0, SINCE SIG(IDOSE) MAY BE < 0 (SINCE A STEADY STATE
C  DOSE INDICATOR HAS A NEGATIVE DOSE TIME).
 
	    XIV(I,IND,1) = SIG(IDOSE)
	    XIV(I,IND,2) = RR
	    XIV(I,IND+1,1) = 1.D29
	    VALAST = RR

	    GO TO 200

	  ENDIF

C  TO GET HERE, THIS DOSE LINE DOES NOT REPRESENT A TIME RESET.

	  IF(RR .NE. VALAST) THEN
         IND = IND + 1
	   XIV(I,IND,1) = SIG(IDOSE)
	   XIV(I,IND,2) = RR
	   XIV(I,IND+1,1) = 1.D29
	   VALAST = RR
	  ENDIF

  200     CONTINUE

	 END DO

C  THE ABOVE END DO IS FOR THE  DO IDOSE = 1,ND  LOOP.


	END DO

C  THE ABOVE END DO IS FOR THE 	DO I = 1,NDRUG  LOOP.


C  STEP 3:

C  ESTABLISH THE COVARIATE VALUES AND TIMES INTO COV(I,J,K). IN 
C  PARTICULAR, COV(I,J,2) IS THE JTH VALUE FOR COVARIATE I, AND 
C  COV(I,J,1) IS THE TIME THIS COV VALUE FIRST OCCURRED. SET THE 
C  LAST TIME TO 1.D29 AS AN INDICATOR THAT THERE ARE NO MORE ENTRIES
C  IN THE ARRAY.

        IF(NADD .GT. 0) THEN

	DO I = 1, NADD

C  ESTABLISH COV(I,J,K) FOR COVARIATE NO. I.
C  PRESET THE LAST VALUE TO -99 SO THAT THE FIRST VALUE WILL BE 
C  DIFFERENT AND THEREFORE ENGAGE THE LOGIC (WHICH ONLY WRITES A ROW 
C  INTO THE ARRAY IF THE VALUE IS DIFFERENT THAN THE PREVIOUS VALUE). 
C  NOTE THAT IND WILL BE THE RUNNING INDEX OF THE LATEST ENTRY INTO THE 
C  ARRAY. PLACE 1.D29 INTO THE LAST TIME ENTRY OF EACH SUB-ARRAY AS AN 
C  INDICATOR THAT THERE ARE NO MORE ENTRIES.

	 COV(I,1,1) = 1.D29
	 IND = 0
	 VALAST = -99.D0

C  FOR COVARIATE I, THE VALUE IS IN COLUMN 2*NDRUG+I OF ARRAY RS.	  

	 DO IDOSE = 1,ND

	  RR = RS(IDOSE,2*NDRUG+I)

C*** MODIFICATION IN SHIFT7.F: A TIME RESET IS NOW DESIGNATED BY A
C  SIG(IDOSE) .LE. 0, RATHER THAN JUST .EQ. 0 (SINCE A STEADY STATE
C  DOSE INDICATOR HAS A NEGATIVE DOSE TIME).

	  IF(SIG(IDOSE) .LE. 0 .AND. IDOSE .GT. 1) THEN

C  THIS REPRESENTS A TIME "RESET". IN THIS CASE, AS INDICATED ABOVE,
C  PUT IN AN EXTRA ROW FOR THE COVARIATE REPRESENTING A VERY LARGE TIME
C  AND THE SAME COV VALUE AS THE PREVIOUS VALUE. THEN PUT IN THE
C  LINE REPRESENTING THE RESET TIME OF 0.
	
	    IND = IND + 1
	    COV(I,IND,1) = 1.D19
	    COV(I,IND,2) = COV(I,IND-1,2)

	    IND = IND + 1

C*** MODIFICATION IN SHIFT7.F. SET THE NEXT COV(I,IND,1) TO BE
C  SIG(IDOSE), NOT 0, SINCE SIG(IDOSE) MAY BE < 0 (SINCE A STEADY STATE
C  DOSE INDICATOR HAS A NEGATIVE DOSE TIME).

	    COV(I,IND,1) = SIG(IDOSE) 
	    COV(I,IND,2) = RR
	    COV(I,IND+1,1) = 1.D29
	    VALAST = RR

	    GO TO 300

	  ENDIF

C  TO GET HERE, THIS DOSE LINE DOES NOT REPRESENT A TIME RESET.

	  IF(RR .NE. VALAST) THEN
           IND = IND + 1
	   COV(I,IND,1) = SIG(IDOSE)
	   COV(I,IND,2) = RR
	   COV(I,IND+1,1) = 1.D29
	   VALAST = RR
	  ENDIF

  300     CONTINUE

	 END DO

C  THE ABOVE END DO IS FOR THE   DO IDOSE = 1,ND  LOOP.

	END DO

C  THE ABOVE END DO IS FOR THE  DO I = 1, NADD  LOOP.

        ENDIF

C  THE ABOVE ENDIF IS FOR THE   IF(NADD .GT. 0)  CONDITION.



C  STEP 4:

C  ESTABLISH THE BOLUS VALUES AND TIMES INTO BOL(I,J,K). IN PARTICULAR, 
C  BOL(I,J,2) IS THE JTH BOLUS VALUE FOR DRUG I, AND BOL(I,J,1) IS THE
C  ADJUSTED (USING THE ASSOCIATED TIMELAGS TAU(I),I=1,NDRUG) TIME THIS 
C  BOLUS OCCURRED. 

	DO I = 1,NDRUG

C  ESTABLISH BOL(I,J,K) FOR DRUG I'S BOLUS. EACH ARRAY IS FILLED ONLY
C  WITH NON-0 BOLUS VALUES. NOTE THAT IND WILL BE THE RUNNING INDEX OF 
C  THE LATEST ENTRY INTO THE ARRAY. PLACE 1.D29 INTO THE LAST TIME ENTRY 
C  OF EACH SUB-ARRAY AS AN INDICATOR THAT THERE ARE NO MORE ENTRIES.

	 BOL(I,1,1) = 1.D29
	 IND = 0

C  FOR DRUG I, THE BOLUS VALUE IS IN COLUMN 2*I OF ARRAY RS.	  

	 DO IDOSE = 1,ND

	  RR = RS(IDOSE,2*I)

C*** MODIFICATION IN SHIFT7.F: A TIME RESET IS NOW DESIGNATED BY A
C  SIG(IDOSE) .LE. 0, RATHER THAN JUST .EQ. 0 (SINCE A STEADY STATE
C  DOSE INDICATOR HAS A NEGATIVE DOSE TIME).

	  IF(SIG(IDOSE) .LE. 0 .AND. IDOSE .GT. 1) THEN

C  THIS REPRESENTS A TIME "RESET". IN THIS CASE, AS INDICATED ABOVE,
C  PUT IN AN EXTRA ROW FOR THE BOLUS REPRESENTING A VERY LARGE TIME
C  AND AN ACCOMPANYING BOLUS VALUE OF 0. THEN PUT IN THE
C  LINE REPRESENTING THE RESET TIME OF 0 + THE TIMELAG ... IF
C  RR .NE. 0.
	
	    IND = IND + 1
	    BOL(I,IND,1) = 1.D19
	    BOL(I,IND,2) = 0.D0

	    IND = IND + 1


C*** THE FOLLOWING CODE IS CHANGED IN SHIFT8.F. NOW BOLUS VALUES 
C  WORK PROPERLY EVEN WITH TIMELAGS. AND AN ADDITIONAL SUBTLE CHANGE
C  WAS ADDED IN shift9.f (SEE THE COMMENTS AT THE TOP OF shift9.f),
C  AND THE EXTRA COMMENTS BELOW.


C  LOGIC IS NOW AS FOLLOWS:

C  IF SIG(IDOSE) = 0, THIS IS A TIME RESET WHICH IS NOT THE START OF
C     A STEADY STATE DOSE SET. IN THIS CASE, A BOLUS WITH A TIMELAG OF
C     TAU(I) WILL OCCUR AT SIG(IDOSE) + TAU(I) = TAU(I).

C  IF SIG(IDOSE) < 0, THIS IS A TIME RESET WHICH IS THE START OF A
C     STEADY STATE DOSE SET. IN THIS CASE:
C     THE BOLUS TIME WILL BE TAU(I) ONLY IF BOTH TAU(I) AND RR
C     ARE NOT 0. OTHERWISE, IT WILL BE SIG(IDOSE).
C     REASON: IF RR = 0, THERE IS NO BOLUS TO BE GIVEN, SO IT WOULD
C     BE SILLY TO INCLUDE AN EXTRA LINE IN THE DOSAGE REGIMEN WITH
C     A 0 BOLUS (AND IT WOULD VERY SLIGHTLY CHANGE THE RESULTS SINCE
C     THE NUMERICAL INTEGRATION THEN HAS TO INTEGRATE THROUGH AN EXTRA
C     TIME). IN AN EXAMPLE (REMARK 4.b IN NPAG109.EXP, THIS CHANGED THE
C     VALUES IN THE LOG-LIKELIHOODS OUT IN THE 13TH DIGIT, BUT SOME 
C     VALUES IN THE DENSITY FILE WERE CHANGED IN THE 4TH DIGIT).

C     ALSO, IF TAU(I) = 0, THE BOLUS HAS NO TIMELAG AND THEREFORE
C     OCCURS AT SIG(IDOSE).

C  THE FOLLOWING EXAMPLE SHOWS WHY A NON-0 BOLUS IN A STEADY STATE DOSE
C  SET, WITH TAU(I) .NE. 0, MUST BE GIVEN AT TAU(I) AND NOT
C  SIG(IDOSE) + TAU(I).

C  EX: IF SIG(IDOSE) = -12, IT MEANS THAT A STEADY STATE DOSE SET IS
C      STARTING WITH AN INTERDOSE INTERVAL OF 12 HOURS. SO, IF A 
C      BOLUS WITH A TLAG OF 1.5 HOURS IS GIVEN, ITS TIME MUST BE
C      1.5, NOT -12 + 1.5 = -10.5. REASON: AFTER THE SIG(IDOSE) OF
C      -12 IS CONVERTED IN SUBROUTINE FUNC2 TO 0, THE 1.5 WILL CORRECTLY
C      INDICATE THAT THE BOLUS IS GIVEN 1.5 HOURS AFTER THE START OF THE
C      STEADY STATE DOSE SET. ALSO, A TIME OF -10.5 WOULD COMPLETELY
C      SCREW UP THE FUNC2 LOGIC WHICH WOULD INTERPRET IT AS THE START
C      OF ANOTHER STEADY STATE DOSE SEST.

C      ON THE OTHER HAND, IF A DRUG HAS A TAU(I) = 0, IT CANNOT SHOW
C      UP AS OCCURRING AT TAU(I) = 0 SINCE THIS WILL COMPLETELY SCREW
C      UP FUNC2'S LOGIC, WHICH WILL INTERPRET THE TIME OF 0 AS A
C      TIME RESET EVENT. IN THIS CASE, THE BOLUS OCCURS AT THE START OF
C      THE STEADY STATE DOSE SET, I.E., AT SIG(IDOSE) = -12, WHICH WILL
C      BE CONVERTED TO 0 BY FUNC2).


      CALL THESAME(SIG(IDOSE),0.D0,ISAME1)
      CALL THESAME(TAU(I),0.D0,ISAME2)
      CALL THESAME(RR,0.D0,ISAME3)

      IF(ISAME1 .EQ. 1) BOL(I,IND,1) = TAU(I)
C  NOTE THAT, TECHNICALLY, WE SHOULD SET BOL(I,IND,1) = SIG(IDOSE) = 0
C  IF RR = 0, SINCE THERE IS NO REASON TO HAVE AN EXTRA LINE IN THE
C  DOSAGE REGIMEN FOR A 0 BOLUS ... BUT CHANGING THIS WOULD CHANGE
C  VERY SLIGHTLY THE RESULTS IN A 0 BOLUS CASE SINCE THERE WOULD BE ONE
C  LESS DOSAGE LINE FOR THE NUMERICAL INTEGRATOR TO INTEGRATE THROUGH,
C  SO THE CODE WILL BE LEFT AS IS, FOR CONSISTENCY SAKE.


      IF(ISAME1 .EQ. 0) THEN
       BOL(I,IND,1) = SIG(IDOSE)
       IF(ISAME2 .EQ. 0 .AND. ISAME3 .EQ. 0) BOL(I,IND,1) = TAU(I)
      ENDIF



	    BOL(I,IND,2) = RR
	    BOL(I,IND+1,1) = 1.D29
	    VALAST = RR

	    GO TO 400

	  ENDIF

C  TO GET HERE, THIS DOSE LINE DOES NOT REPRESENT A TIME RESET.


	  IF(RR .NE. 0.D0) THEN

           IND = IND + 1

C  *** CHANGE FOR SHIFT8.F.
C  NOW BOLUS VALUES CAN OCCUR IN STEADY STATE DOSES. AND IF THEY DO,
C  THE FIRST ONE MUST OCCUR AT TIME TAU(I), NOT SIG(IDOSE) + TAU(I)
C  AS THE FOLLOWING EXAMPLE ILLUSTRATES:
C  EX: SIG(1) = -12 INDICATING THAT THE STEADY STATE DOSE SET HAS
C      AN INTERDOSE INTERVAL OF 12 HOURS. TAU(1) = 1.5 -->
C      DRUG 1 HAS A TIMELAG OF 1.5 HOURS. SO, IF THE FIRST BOLUS TIME IS
C      SET =  SIG(1) + TAU(1) = -12 + 1.5 = -10.5, THIS WILL SCREW
C      UP THE FUNC2 LOGIC SINCE IN THAT CODE, THE FIRST TIME OF
C      -12 WILL BE RESET TO BE 0, AND THIS WILL BE FOLLOWED BY -10.5,
C      WHICH WILL LOOK LIKE THE START OF ANOTHER STEADY STATE DOSE
C      SET. INSTEAD, SET FIRST BOLUS TIME = TAU(1) = 1.5, WHICH IS
C      CORRECT SINCE IT OCCURS 1.5 HOURS AFTER THE STEADY STATE DOSE
C      STARTS.

         IF(SIG(IDOSE) .GE. 0.D0) BOL(I,IND,1) = SIG(IDOSE) + TAU(I)
         IF(SIG(IDOSE) .LT. 0.D0) BOL(I,IND,1) = TAU(I)

	   BOL(I,IND,2) = RR
	   BOL(I,IND+1,1) = 1.D29
	  ENDIF

  400     CONTINUE

	 END DO

C  THE ABOVE END DO IS FOR THE  DO IDOSE = 1,ND  LOOP.


	END DO


C  THE ABOVE END DO IS FOR THE  DO I = 1,NDRUG  LOOP.



C  STEP 5:

C  REASSIGN THE VALUES IN IV, BOL, AND COV TO THE APPROPRIATE ENTRIES
C  OF RS, KEEPING TRACK OF THE RUNNING INDEX, ND, OF DOSE EVENTS. IF
C  ND EXCEEDS 5000, STOP THE PROGRAM WITH A MESSAGE TO THE USER. ALSO,
C  REASSIGN THE CORRESPONDING TIME VALUES TO ARRAY SIG.

	NI = 2*NDRUG + NADD
	ND = 0

C  GO THROUGH THE ARRAYS IV, BOL, AND COV TO DETERMINE THE NEXT
C  LOWEST DOSE TIME. PUT THIS VALUE INTO RS, ALONG WITH THE 
C  CORRESPONDING VALUES FOR THE IV'S, THE BOLI, AND THE COVARIATES.

C  IN THE LOOP BELOW, IT IS NECESSARY TO KNOW TO WHAT POINT IN THE
C  IV, BOL, AND COV ARRAYS THE TIMES AND VALUES HAVE ALREADY BEEN 
C  STORED INTO RS. THESE INDICES ARE INDIV(I), I=1,NDRUG; INDBOL(I),
C  I=1,NDRUG; AND INDCOV(I), I=1,NADD, RESPECTIVELY. E.G., 
C  INDIV(2) = 4 MEANS THAT ALL VALUES IN THE IV, BOL, AND COV ARRAYS, 
C  THROUGH THE 4TH TIME FOR IV DRUG 2 (I.E., THROUGH TIME = XIV(2,4,1))
C  HAVE BEEN OR ARE ABOUT TO BE STORED INTO THE RS ARRAY.

C  SO PRESET ALL THESE INDEX INDICATORS = 1, AND INITIALIZE THE 
C  CURRENT DOSE TIME TO A NEGATIVE NO. SO THAT THE FIRST TIME
C  THROUGH THE FOLLOWING LOOP WILL ENGAGE THE LOGIC.

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

C  FIND THE NEXT LOWEST TIME AMONG THE IV, BOL, AND COV ARRAYS.

C  ESTABLISH INTO TIMCAN(J) THE CANDIDATES FOR THE NEXT DOSE TIME
C  (AND CORRESPONDING VALUES FOR THE IV'S, BOLI, AND COVARIATES) TO
C  BE PUT INTO RS.


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

C  FIND THE NEXT TIMNXT, THE MINIMUM VALUE AMONG THE NI ENTRIES IN 
C  TIMCAN. TIMNXT WILL BE THE NEXT TIME TO BE PUT INTO ARRAY RS (ALONG 
C  WITH ALL THE CORRESPONDING IV'S, BOLI, AND COVARIATE VALUES). IF 
C  TIMNXT = 1.D29, IT IS BECAUSE THERE ARE NO FURTHER VALUES TO BE PUT 
C  INTO RS (I.E, THE PROCESS IS FINISHED).

	TIMNXT = TIMCAN(1)
	DO I = 2,NI
	 IF(TIMCAN(I) .LT. TIMNXT) TIMNXT = TIMCAN(I)
	END DO

	IF(TIMNXT .EQ. 1.D29) RETURN

C  SINCE TIMNXT < 1.D29, THERE ARE MORE VALUES TO BE PUT INTO RS.
C  GO THROUGH ALL THE SUBARRAYS AND PUT IN VALUES AS FOLLOWS. IF THE
C  CURRENT TIME FOR AN IV, BOLUS, OR COVARIATE IS THE SAME AS TIMNXT, 
C  PUT THE CORRESPONDING IV, BOLUS, OR COVARIATE VALUE INTO RS, AND 
C  INCREASE THE INDEX FOR THAT SUB-ARRAY TO THE NEXT VALUE. IF THE
C  CURRENT TIME FOR AN IV OR A COVARIATE IS .GT. TIMNXT, PUT THE IV OR 
C  COVARIATE VALUE FROM THE PREVIOUS ROW INTO RS, AND LEAVE THE INDEX 
C  UNCHANGED. IF THE CURRENT TIME FOR A BOLUS IS .GT. TIMNXT, PUT 0.0 
C  INTO RS (I.E., BOLUS VALUES ARE INSTANTANEOUS, WHEREAS IV AND 
C  COVARIATE VALUES CONTINUE UNTIL CHANGED), AND LEAVE THE INDEX
C  UNCHANGED.


C  TEST FOR TIMNXT = 1.D19, WHICH INDICATES A TIME RESET.

	IF(TIMNXT .EQ. 1.D19) THEN

C  TIMNXT = 1.D19 MEANS THAT THE NEXT TIME IN EACH ARRAY IS THE
C  TIME AT OR AFTER THE RESET. SO INCRASE ALL THE ARRAY INDICES BY
C  1, RESET TIMNXT TO A NEGATIVE NO. AND RETURN TO LABEL 100.

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

C  IF ND > 5000, STOP WITH A MESSAGE TO THE USER THAT THE
C  PROGRAM ONLY ALLOWS A TOTAL OF 5000 DOSE EVENTS.

   10	 WRITE(*,1) ND
    1    FORMAT(/' THE NUMBER OF DOSE EVENTS, AFTER TAKING INTO'/
     1' ACCOUNT DIFFERING TIMES DUE TO TIMELAGS IS ',I6,', MORE THAN'/
     2' THE ALLOWABLE MAXIMUM OF 5000. THE PROGRAM IS STOPPING. PLEASE'/
     3' RERUN WITH PATIENTS HAVING FEWER DOSE EVENTS, OR WITH FEWER'/
     4' TIMELAG VALUES SELECTED AS FIXED OR RANDOM PARAMETERS.'//)
	 STOP

	ENDIF

C  ND .LE. 5000, SO CONTINUE. FOR THIS DOSE EVENT, PUT IN THE CURRENT 
C  TIME, AND THE CORRESPONDING IV, BOLUS, AND COVARIATE VALUES. 


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
