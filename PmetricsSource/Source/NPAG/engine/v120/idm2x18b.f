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


	SUBROUTINE IDCALCY(NPP,NDIM,ESTML,YPRED,NUMEQT,
     1      IgIsGoodPoint,JSUB,IG,LengthOfPX)


C  INPUT ARE:

C  NPP = NO. OF PARAMETERS (RANDOM, FIXED, AND RANFIX) IN THE PARAMETER
C        VECTOR, ESTML.
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

c wmy2017Aug01
        integer IgIsGoodPoint,JSUB,IG,LengthOfPX
        double precision, dimension(32) :: PX

! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp Threadprivate(/PARAMD/, px)

        save px

C*****INITIALIZE PROGRAM*****

	CALL SYMBOL

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
	  PX(I) = ESTML(I)
	END DO


C  CALL SUBROUTINE EVAL2 TO GET Y, EVALUATED
C  AT ESTML(I) AS DEFINED ABOVE.

	CALL EVAL2(YPRED,NUMEQT,
     1      IgIsGoodPoint,JSUB,IG,LengthOfPX,px)


        RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE FUNC2(M,YPRED,NUMEQT,
     1      IgIsGoodPoint,JSUB,IG,LengthOfPX,PX)

C  THIS SUBROUTINE, CALLED BY EVAL2, FINDS YPRED(I) = OUTPUT CONC. AT
C  TIME I, I=1,M, GIVEN PARAMETER VALUES IN P.

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

C wmy2017Jul28 -- These are NOT the same ipar/rpar as in main !!!
C   so, we need to move the declarations to main and pass into the
C   DO 800, which causes other headaches: making the params allocatable
C   and therefore requiring function declarations ... for now,
C   just make Ashley's program work!
       integer IgIsGoodPoint,JSUB,IG,LengthOfPX
       double precision, dimension(32) :: px
       integer, dimension(128) :: ipar
       double precision, dimension(128) :: rpar

      CHARACTER ERRFIL*20


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

      KNS=1
      KNT=1

c wmy
      do III=1,128
        rpar(III)=0.D0
        ipar(III)=0
      end do

C  NOTE THAT KNT IS THE RUNNING INDEX OF THE NEXT OBSERVATION TIME,
C  AND       KNS IS THE RUNNING INDEX OF THE NEXT DOSAGE TIME.

        T=0.0D0

c wmy 2017Jul27 ipar(25) <- Flag indicating first call to DVODE for (JSUB,IG)
       ipar(25) = 1

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


	 CALL GETFA(FA)


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

	 CALL GETIX(N,X)



C  CALL SUBROUTINE GETTLAG IN npemdriv.f (THE FIRST TEMPLATE FILE TO 
C  INCLUDE GETTLAG IS TSTMULTG.FOR) TO OBTAIN THE VALUE OF THE TIMELAG
C  FOR EACH OF THE NDRUG DRUGS.

   75	 CALL GETTLAG(TLAG)

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


	 CALL SHIFT(TLAG,ND,SIG,NDRUG,NADD,RS)


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

      CALL OUTPUT(0.D0,YT)
	DO 2000 I=1,NOS
2000    Y(KNT,I)=YT(I)
        KNT=KNT+1
        GO TO 45

12      IF(TIM(KNT).GT.SIG(KNS)) GO TO 13
        IF(TIM(KNT).NE.0.0D0) GO TO 45

C  THE ONLY WAY THE FOLLOWING CALL TO OUTPUT CAN OCCUR IS IF TIM(KNT)
C  = 0 --> OBTAIN YT = OUTPUT VALUE(S) AT TIME 0.0.

        CALL OUTPUT(0.D0,YT)
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

	 CALL GETFA(FA)


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



       CALL GETIX(N,X)
		
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


C wmy 2015Apr22 -- PX was copied near beginning of (JSUB,IG) loop
C   Setting RPAR(I) = P,R,B
        DO I = 1,32
          RPAR(I+22)=PX(I)
        END DO
        DO I = 1,37
          RPAR(I+32+22)=R(I)
        END DO
        DO I = 1,20
          RPAR(I+32+37+22)=B(I)
        END DO
        RPAR(20+32+37+22+1)=NUMEQT
        RPAR(20+32+37+22+2)=NDRUG
        RPAR(20+32+37+22+3)=NADD
        RPAR(20+32+37+22+4)=AGE
        RPAR(20+32+37+22+5)=HEIGHT
        IPAR(1)=32
        IPAR(2)=37
        IPAR(3)=20
        IPAR(4)=3
        IPAR(5)=ISEX
        IPAR(6)=IETHFLAG
        IPAR(23)=JSUB
        IPAR(24)=IG
C        IPAR(25)=1 // must be set up BEFORE the TIN/TOUT looping begins
        IPAR(26)=IgIsGoodPoint
        IPAR(27)=LengthOfPX

32      IF(N .NE. -1) CALL USERANAL(X,T,TOUT,IgIsGoodPoint,rpar,ipar)
        IF(N .EQ. -1) CALL ANAL3(X,T,TOUT)

c wmy 2017Jul27 ipar(25) <- Flag indicating first call to DVODE for (JSUB,IG)
       ipar(25) = 0


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

        CALL OUTPUT(TIM(KNTM1),YT)

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

	 CALL GETFA(FA)


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


      RETURN
      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE EVAL2(YPRED,NUMEQT,
     1      IgIsGoodPoint,JSUB,IG,LengthOfPX,px)

C  THIS SUBROUTINE, CALLED BY MAIN, FINDS THE OUTPUT CONC. 
C  VECTOR, YPRED, EVALUATED AT PARAMETER VALUES IN VECTOR P, PASSED 
C  DIRECTLY TO SUBROUTINE FUNC2 VIA COMMON/PARAMD.

	IMPLICIT REAL*8(A-H,O-Z)

c wmy2017Aug01
        integer IgIsGoodPoint,JSUB,IG,LengthOfPX
        double precision, dimension(32) :: px

	COMMON/SUM2/ M,NPNL
        COMMON/CNST2/ NPL,NOS,NDRUG,NADD
	DIMENSION YPRED(594,NUMEQT)

	CALL FUNC2(M,YPRED,NUMEQT,
     1      IgIsGoodPoint,JSUB,IG,LengthOfPX,px)


	RETURN
	END


