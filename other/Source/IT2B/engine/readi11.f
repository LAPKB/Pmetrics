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


