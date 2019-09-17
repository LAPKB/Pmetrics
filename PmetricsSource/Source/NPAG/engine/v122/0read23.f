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
C  FOLLOWED BY A SPACE, ETC. IF THERE ARE MORE THAN 20 RANDOM VARIABLES,
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
     8   XVERIFY(100)


C  SEE COMMENTS IN SUBROUTINE SUBRES OF npageng23.f, WHERE BAYPOS
C  (AND NACTSUB) ARE STORED. THE FIRST DIMENSION OF BAYPOS (AND THE
C  ONLY DIMENSION OF NACTSUB) IS SET = 100, RATHER THAN 800, BECAUSE
C  THE PROGRAM IS TOO BIG OTHERWISE.

C  NOTE THAT THE DIMENSIONS FOR MAXGRD, MAXDIM, MAXSUB, AND MAXOBDIM
C  ABOVE HAVE BEEN REPLACED BY, RESPECTIVELY, 1500, 30, 800, AND
C  150. THIS COULD BE AVOIDED BY PROVIDING THESE VALUES IN THE ARGUMENT
C  LIST OF THIS ROUTINE. NOTE THAT MAXGRD IS HARDCODED TO BE 1500 BELOW 
C  AS IT IS USED IN THE PROGRAM.

        CHARACTER PAR(30)*11,PARFIX(20)*11,READLINE*80, 
     1   NAME(800)*53,CHARTNO(800)*53,SEX(800)*1,PARRANFIX(20)*11,
     2   COVDESCR(26)*20,OUTFILER*20,READLINE2*1000,PRIFILE*20

        CHARACTER(LEN=20) :: OSName

      COMMON/DOSEOBS/DOSEBLOCK,OBSBLOCK,NDORIG

      COMMON/BAY/NACTSUB,BAYPOS
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

        write(*,*) "At 2013"

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

          write (*,*) "ISUB,K,J",ISUB,K,J

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

        IMPLICIT REAL*8(A-H,O-Z)
        PARAMETER(MAXNUMEQ=7)

        DIMENSION YO(MAXOBDIM,MAXNUMEQ),RJUNK(34),C0(MAXNUMEQ),
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

        IMPLICIT REAL*8(A-H,O-Z)
        PARAMETER(MAXNUMEQ=7)

        CHARACTER READLINE*1000,NAME(800)*53,CHARTNO(800)*53,SEX(800)*1
C  AS OF READ5.F, PAR(30)*11 REMOVED IN CHARACTER STMT. PAR IS NOT USED
C  IN THIS ROUTINE.

        DIMENSION XLOGLIK(9997),XMEAN(9997,30),AICBIC(9997,2),
     1   STDEV(9997,30),PRCFVR(9997,30),ACTPTS(9997),SCALNFO(9997),
     2   GAMLAM(9997), 
     3   AGE(800),HEIGHT(800),SUBMEAN(800,30),SUBLOGLIK(800),
     4   SUBSTD(800,30),SUBPERCOF(800,30),
     6   NDD(800),ASSAYC(800,MAXNUMEQ,4)

C  NOTE THAT THE DIMENSIONS FOR MAXSUB AND MAXOBDIM HAVE BEEN HARDCODED
C  TO BE, RESPECTIVELY, 800 AND 150. THIS COULD BE AVOIDED BY PROVIDING 
C  THESE VALUES IN THE ARGUMENT LIST OF THIS ROUTINE. NOTE  THAT THE
C  MAXIMUM ALLOWABLE VALUE FOR NI (2*NDRUG + NADD) IS 34 --> LAST 
C  DIMENSION OF DOSEBLOCK IS 1 + 34 = 35. NOTE ALSO THAT THE MAX. NO.
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
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER READLINE*1000,COVDESCR(26)*20

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



