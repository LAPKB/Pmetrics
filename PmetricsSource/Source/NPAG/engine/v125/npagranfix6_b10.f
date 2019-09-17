c  npagranfix6.f                                           3/9/16

c  npagranfix6 has the following changes from npagranfix5:

c  1. In Subroutine SUBRES, after loop 800, and the call from NOTINT, 
c  PYJ is tested to verify it is not 0. This test has always been in 
c  main at the same point, but not in SUBRES since it didn't seem to be
c  needed there. But it is possible, if the user is doing a MAXCYC = 0
c  run, that the apriori density may not include any points which give
c  predicted values close enough to those of the new subject being 
c  analyzed (especially if this subject's assay coefficients are 
c  relatively small). So the message to the user in this case will
c  be to try the run again after increasing the size of the assay 
c  coefficients.

c  2. Formats 1657 and 7124 are changed to show that the output file
c  is made by npagranfix6 rather than npagranfix5.

c-----------------------------------------------------------------------

c  npagranfix5.f                                           10/24/15

c  npagranfix5 has the following changes from npagranfix4:

c  1. RANFIXEST and NRANFIX are added to a !$omp statement just above 
c  the DO 800 loop in main. This will now enable this program to be
c  run in parallel (npagranfix4.f could only be run in serial mode).

c  2. Formats 1657 and 7124 are changed to show that the output file
c  is made by npagranfix5 rather than npagranfix4.

c-----------------------------------------------------------------------

c  npagranfix4.f                                           10/11/15

c  npagranfix4 has the following changes to npagranfix3:

c  1. NRANFIX, PARRANFIX(.), and RANFIXEST(.) are now added to DENFIL, 
c  both files 23 and 33. This means that the code at the top of these
c  files will be changed from DENSITY APR_10 to DENSITY OCT_15. 
c  Similarly for the output file, file 25, whose code at the top will 
c  change from VERSION 42 on line 1 to VERSION 43, and whose code on 
c  line 2 will change from VER_BAK JUL_11 to VER_BAK OCT_15.

c  2. Similarly, NRANFIX, PARRANFIX(.), and RANFIXEST(.) are added to 
c  the writing of the NP_RFxxxx.TXT file by Subroutine READOUT, which
c  means the code for that file on the top line will change to 
c  VERSION 1.8 - OCT 2015. The module which has Subroutine READOUT will 
c  change from read22.f to read23.f.

c  3. Two of the modules linked with the main "engine" module will be
c  updated. idm2x17.f will be replaced by idm2x18.f, and idm3x18.f will
c  be replaced by idm3x19.f. The changes will not be functional, just
c  comments that NPP now also includes the no. of RANFIX parameters
c  (in addition to the no. of random and fixed parameters).


c  4. This "engine" program will be part of the NPAG119.FOR program.
c  Note that NPAG119.FOR will have many changes related to items 1. and
c  2. above, and also because npag102.inp will be updated to be  
c  npag103.inp, which will include NRANFIX, PARRANFIX(.), and 
c  RANFIXEST(.).

c  5. Formats 1657 and 7124 are changed to show that the output file
c  is made by npagranfix4 rather than npagranfix3.

c-----------------------------------------------------------------------

c  npagranfix3.f                                           10/06/15

c  npagranfix3 has the following changes to npagranfix2:

c  1. CALCRF is changed so that it rejects any candidate vectors which
c  have random variable values outside their input ranges. In such
c  case, FNTVAL is returned as an unattractive value, 1.D30.
 
c  2. Formats 1657 and 7124 are changed to show that the output file
c  is made by npagranfix3 rather than npagranfix2.

c-----------------------------------------------------------------------

c  npagranfix2.f                                          10/05/15

c  npagranfix2 has the following change to npagranfix:

c  1. The ELDERY code now optimizes over not just the NRANFIX parameters
c  which are unknown but the same for all subjects, but also the 
c  NVAR random parameters (using the current cycle means as the 
c  initial estimates for these NVAR parameters). This will take more
c  time, but is intended to bring all parameters up to their best
c  current estimates simultaneously. At the end of this block of code,
c  the grid values in CORDEN(.,.) are adjusted so the means of each
c  of the NVAR random parameters are equal to the just found new
c  estimates from ELDERY. But this code is only executed after cycle
c  no. 1.

c  2. The SIG(I,J) code in Subroutine CALCRF now has the ierrmod
c  code following it, as it should always have had.

c  3. Formats 1657 and 7124 are changed to show that the output file
c  is made by npagranfix2 rather than npagranfix.


c-----------------------------------------------------------------------

c  npagranfix.f                                            9/20/15

c  npagranfix is an extension to npageng30. It has the added capability
c  to estimate the value of parameters that are the same for all 
c  subjects.

c  This means that now the user can designate parameters as fixed
c  (IRAN(.) = 0), random (IRAN(.) = 1), or ranfix (IRAN(.) = 2).

c  Formats 1657 and 7124 are changed to show that the output file
c  is made by npagranfix rather than npageng29.

c-----------------------------------------------------------------------

c  npageng30.f                                             3/11/15

c  npageng30 has the following change from npageng29:

c  All numbers written out in F or G format are now tested to see if
c  they are inside [-1.D-99, 1.D-99]. If so, they are changed to be
c  0. The reason is that otherwise they will be printed out without 
c  the accompanying D or E (e.g., as .934-106, rather than .934E-106).

c  Note that a new Subroutine, VERIFYVAL is added to the code to 
c  do the indicated testing above.

c-----------------------------------------------------------------------

c  npageng29.f                                             12/16/14

c  npageng29 has the following changes to npageng28:

c  1. The statement preceding the DO 800 loop in MAIN is changed from 
c  !$omp Do Schedule(guided)  to  !$omp Do 

c  This change removes the slight randomness (typically in about the
c  12th significant digit in the log-liks) that sometimes appears 
c  when the program is run repeatedly on the same problem.

c  2. Formats 1657 and 7124 are changed to show that the output file
c  is made by npageng29 rather than npageng28.

c-----------------------------------------------------------------------

c  npageng28.f                                             11/25/14

c  npageng28 has the following changes to npageng27:


c  1. It has the parallel code (i.e., the Threadprivate and
c  other statements) to make this program run in parallel, using the
c  compilation command, 
c  gfortran -O3 -fopenmp -fmax-stack-var-size=32768 -o npageng28p.exe 
c  npageng28.f ... npagdriv.f.

c  Note that all the accompanying modules (except blasnpag.f and
c  read21.f) will be updated to have the necessary parallel code
c  (the Threadprivate, Save, etc. commands).

c  Note that !$omp lines are not comments, but statements read by the
c  compiler to tell it how to parallelize. But lines beginning with
c  just ! are simple comment lines.

c  So, shift9.f will be updated to shift10.f; idm1x16.f will be 
c  updated to idm1x17.f; idm2x15.f will be updated to idm2x16.f;
c  idm3x16.f will be updated to idm3x17.f. And, the new template 
c  model file will be TSTMULTN.FOR (updated from TSTMULTM.FOR).

c  Note that CALL SYMBOL has been removed from idm1x17.f/Subroutine
c  IDPC, and instead inserted into MAIN just above label 1001.


c  2. The code for AICC is changed. If the denominator,
c  NOBTOT-KP-1 is .LE. 0, then AICC will be set = the AIC
c  (i.e., in this case, the old version of AIC will be used).

c  3. Formats 1657 and 7124 are changed to show that the output file
c  is made by npageng28 rather than npageng27.

c-----------------------------------------------------------------------

c  npageng27.f                                             8/20/14

c  npageng27 has the following change to npageng26:

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

c  2. Formats 1657 and 7124 are changed to show that the output file

c  is made by npageng27 rather than npageng26.

c  3. The formula for AIC is changed.

c-----------------------------------------------------------------------

c  npageng26.f                                             8/05/14

c  npageng26 has the following changes to npageng25:

c  1. Formats 1657 and 7124 are changed to show that the output file
c  is made by npageng26 rather than npageng25.

c  2. The 3 ID modules are updated: idm1x15.f to idm1x16.f; idm2x14.f
c  to idm2x15.f; and idm3x15.f to idm3x16.f.

c  3. The formulas for AIC and BIC are changed.

c-----------------------------------------------------------------------

c  npageng25.f                                             3/10/14

c  npageng25 has the following changes from npageng24:

C  1. ICENT WILL NOW BE IRRELEVANT. PREVIOUSLY, THE USER ENTERED ICENT
C  AS 1, 2, OR, 3 TO SELECT WHETHER HE WANTED THE PREDICTED VALUES 
C  WRITTEN TO PRTBxxxx TO BE BASED ON THE BAYESIAN MEANS, MEDIANS, OR
C  MODES, RESPECTIVELY. NOW, ICENT WILL BE UNUSED SINCE THE PRTBxxxx 
C  FILE WILL INCLUDE THE PREDICTED VALUES FOR ALL 3 OF THESE
C  MEASURES. SIMILARLY THE OUTPUT FILE WILL NOW HAVE AUC TABLES FOR ALL
C  3 OF THESE MEASURES (PREVIOUSLY IT JUST HAD THE AUC TABLE BASED ON
C  THE MEASURE SPECIFIED BY ICENT). NOTE THAT THE NEW PC PREP PROGRAM
C  IS NPAG113.FOR.

C  2. THE MAXIMUM NO. OF OUTPUT EQUATIONS WILL BE CHANGED FROM 6 TO 
C  NUMEQT, WHICH IS SUPPLIED IN THE ARGUMENT LIST TO SUBROUTINE NPAG.
C  THIS MEANS THAT NUMEQT WILL NOW BE PASSED TO ALL THE SUBROUTINES
C  THAT NEED IT; AND IN THOSE SUBROUTINES, ANY 6 REFERRING TO THE MAX.
C  NO. OF OUTPUT EQUATIONS WILL BE CHANGED TO NUMEQT.

C  NOTE THAT THIS ALSO INCLUDES PASSING NUMEQT TO IDCALCY AND IDCALCYY
C  SO THAT YPRED AND YYPRED, RESPECTIVELY, AND BE VARIABLY DIMENSIONED
C  IN THOSE MODULES.

C  ALSO, IN THOSE ROUTINES WHERE ARRAYS ARE PASSED IN COMMON STATEMENTS,
C  OR EXIST ONLY IN THOSE ROUTINES, DIMENSIONS RELATED TO THE 
C  MAX. NO. OF OUTPUT EQS. WILL BE DIMENSIONED BY A PARAMTER STMT.
C  SETTING MAXNUMEQ = 7, THE CURRENT LIMIT (SINCE THESE ARRAYS CANNOT
C  BE VARIABLY DIMENSIONED BY A CALLING ARGUMENT). THIS INCLUDES IN 
C  MAIN, FILRED, AND OTHER ROUTINES IN THE ID MODULES WHERE YOO IS
C  PASSED IN COMMON/OBSER. IT ALSO INCLUDES OBSBLOCK IN SUBROUTINES
C  NEWWORK1 AND READOUT. AND IT INCLUDES SUBROUTINE OUTPUT IN THE NEW
C  TEMPLATE MODEL FILE, TSTMULTM.FOR (SEE CHANGE 3.).

C  THE NEW ID MODULES TO BE LINKED WITH THIS MAIN MODULE ARE
C  idm1x15.f, idm2x14.f, AND idm3x15.f.

C  3. NOTE THAT THE TEMPLATE MODEL FILE FOR THIS PROGRAM HAS BEEN
C  CHANGED FROM TSTMULTL.FOR TO TSTMULTM.FOR (SEE COMMENT 2. ABOVE).

C  4. read19.f IS CHANGED TO read20.f. THIS IS BECAUSE THAT MODULE
C  NO LONGER WILL READ AUC VALUES FROM THE OUTxxxx FILE. I.E., AUC
C  VALUES WILL NO LONGER BE INCLUDED IN THE NP_RFxxxx.TXT FILE.

c  5. Formats 1657 and 7124 are changed to show that the output file
c  is made by npageng25 rather than npageng24.


c-----------------------------------------------------------------------

c  npageng24.f                                             6/2/13

C  npageng24 has the following changes to npageng23:

C  1. THE CODE TO READ extnum TO GET THE 4-DIGIT JOB NUMBER IS MOVED TO

C  THE TOP OF THE CODE, SO ERRORxxxx CAN BE OPENED AND THEN FILLED 
C  AT EARLIER LOCATIONS IF THE PROGRAM STOPS ABNORMALLY. IN PARTICULAR,
C  ERRFIL IS ADDED TO THE ARG. LIST OF SUBROUTINE GETIPATF SO IT CAN BE
C  WRITTEN TO IF THERE IS AN ERROR IN THAT ROUTINE. ALSO, FORMATS
C  4706 (SEE BELOW) AND 1721 ARE NOW WRITTEN TO ERRFIL IF THERE IS AN
C  ERROR IN THOSE LOCATIONS.

C  NOTE THAT A NEW FORMAT, 4706, IS USED INSTEAD OF FREE FORMAT TO TELL
C  THE USER THAT npag102.inp IS NOT AVAILABLE, AND THE PROGRAM IS
C  THEREFORE STOPPING.

C  2. FORMAT 26 IS CHANGED TO INCLUDE JSUB, SO THE USER WILL KNOW WHICH
C  IS THE FIRST SUBJECT TO CAUSE THE ALL P(YJ|X) = 0 ERROR.

C  3. npageng24 is the main module for NPAG112.FOR.

c  4. Formats 1657 and 7124 are changed to show that the output file
c  is made by npageng24 rather than npageng23.

c-----------------------------------------------------------------------

c  npageng23.f                                             4/14/13

c  npageng23 has the following changes to npageng22:

c  1. TOL is no longer read in from npag102.inp. The read-in value 
c  wasn't used anyway since TOL was subsequently hardcoded to be 1.D-4.

c  Instead, the tolerance parameter read in is now TOLC, and this value
c  will be the one against which checkbig is compared from now on 
c  (rather than a hardcoded value of .01 as was used previously).

c  Note that the following is a quick summary of how the tolerance 
c  parameters are used to establish convergence of the algorithm:

c  a. The program keeps track of the log-lik improvement between 2 
c  consecutive regular cycles. If this improvement is not at least
c  TOL (1.D-4), then some perturbed grid points are added to the grid,
c  at a resolution of 20% of the range of the parameters. This process
c  continues with the resolution halved each time a regular cycle 
c  doesn't have a log-lik improvement of at least TOL, and it continues
c  until the resolution gets down to .0001. That marks the end of a
c  "major cycle".
 
c  b. The log-lik difference between 2 consecutive "major cycles" must
c  be .LE. TOLC (which is now read in from npag102.inp, rather than 
c  hardcoded to be .01).

c  Note that Format 1223 is changed so that TOLC is now included in the
c  output file.

c  2. If the program bombs, the message that is written to the screen 
c  will now also be written to the file ERRFIL = ERRORxxxx, where xxxx
c  is the 4-digit run no. In this way, if the program is being run using
c  Pmetrics, the Pmetrics program can respond appropriately. Note that
c  ERRFIL must be passed to all the routines which could write to it
c  using COMMON/ERR/ERRFIL.

c  3. Formats 1657 and 7124 are changed to show that the output file
c  is made by npageng23 rather than npageng22.

c  4. Subroutine SUBRES now also stores each subject's Bayesian 
c  posterior density into arrays BAYPOS and NACTSUB, which are passed
c  via COMMON/BAY to Subroutine READOUT. They will be written into
c  the NP_RFxxxx.TXT file. But note that, due to size restrictions
c  (see NPAG111.EXP), only the first 100 subjects worth of values can
c  be stored at present.

c  Also note that only the grid points that are active for each subject
c  are used to calculate Bayesian subject values now, rather than all the
c  NACTVE grid points from the population final density.

c  5. This program is now compiled with read19.f, which replaces
c  read18.f. The other modules remain unchanged.

c  6. Change 4. in npageng20.f is reversed. Now the previous cycle's 
c  density will again be saved each cycle, except for cycle 1. But this
c  can be manually changed by changing the value of ISAVEDEN (below 
c  label 1243) to something other than 1.

c  7. Several C???DEBUG comments are removed in MAIN ...
c  from write(*,*)' About to create density file ...'  to
c  NOW CLOSE THE FILES USED BY READOUT. These statements are no longer
c  debug statements.


c-----------------------------------------------------------------------

c  npageng22.f                                             11/8/12

c  npageng22 has the following change from npageng21:

c  1. It comments out the PAUSE statement following Format 164 in 
c  Subroutine emint. Reason: the program will not complete properly if 
c  it is run under  Pmetrics (which cannot supply a keyboard response
c  during a run). 

c  2. Formats 1657 and 7124 are changed to show that the output file
c  is made by npageng22 rather than npageng21. 

c-----------------------------------------------------------------------

c  npageng21.f                                             10/15/12

c  npageng21 has the following changes from npageng20:


c  1. The 3 id modules linked to this program have been updated; they
c  are now idm1x14.f, idm2x13.f, and idm3x14.f. These 3 id modules
c  correct a bug. Now R(.) are set = RS(.,.) before GETIX is called
c  in the time reset section (see details in those modules).

c  2. A bug which was introduced in npageng18.f (see change 1. there)

c  has been corrected. Instead of writing TPREDREL(.) to File 31 below
c  Format 2131, TTPREDREL(JSUB,.) is written. Note that TPREDREL(.) 
c  holds the values for the last subject only (no. NSUB), but Format
c  2131,which is in loop 7000 over all NSUB subjects, requires that
c  TTPREDREL(JSUB,.) be used so that each subject has its own predicted
c  times  written.

c  3. Another bug is fixed in the AUC part of the code ... by using a
c  new parameter, IELAST, and initializing it to be 0 just below where
c  ILAST is initialized = 0 for each new subject. IELAST must be used in
c  addition to ILAST for those subjects whose AUC tables start with a 
c  partial period, rather than a whole period. In this case, for 
c  JSUB > 2, without IELAST, the index of which YYPRED(.,.) value starts
c  the current AUC would not be set correctly. See the details in the
c  code.

c  4. Formats 1657 and 7124 are changed to show that the output file
c  is made by npageng21 rather than npageng20. 

c  Note that npageng21.f is the main "engine" module for the new PC
c  prep program, NPAG110.FOR.

c-----------------------------------------------------------------------

c  npageng20.f                                             10/04/12

c  npageng20 has the following changes from npageng19:

c  1. It will now be linked with read18.f, rather than read17.f. The 
c  reason is that NOMAXTIM(.) in Subroutine CONVERGE2 must be 
c  initialized to be all 0's to avoid a possible bug if the program
c  is compiled and linked with gfortran (see details in read18.f). Also,
c  note that since the individual files needed by read18.f will now be
c  left open when that module is called, the combined output file,
c  OUTFILE, will no longer be needed as a calling argument to
c  read18.f (i.e., Subroutine READOUT). 

c  Note that not having Subroutine READOUT separate the combined output
c  file into the 4 needed individual files can save a lot of execution
c  time if this program is compiled and linked with gfortran.

c  2. It will now be linked with shift9.f, rather than shift7.f. The
c  reason is to fix a bug which occurred if a steady state dose had
c  bolus inputs (see details in shift9.f).

c  3. It will be linked with idm1x13.f (updated from idm1x12.f),
c  idm2x12.f (updated from idm2x11.f), and idm3x13.f (updated from
c  idm3x12.f). Each of the new id modules have new code to make sure
c  that Subroutine GETFA has the initial values for R(.) when it is
c  called the first time, and to make sure that GETFA is called just
c  before each time FA(.) are used, so the FA(.) are updated to current
c  values (see details in the id modules). 

c  4. See the code below Format 1243. Starting with this program, the
c  joint density will be saved only after cycle no. 2, not after every
c  cycle. See code below 1243 for the reasons.

c  5. Formats 1657 and 7124 are changed to show that the output file
c  is made by npageng20 rather than npageng19. 

c  Note that npageng20.f is the main "engine" module for the new PC
c  prep program, NPAG109.FOR.

c-----------------------------------------------------------------------

c  npageng19.f                                             8/19/12

c  npageng19 has the following changes to npageng18:

c  1. Preset NACTLAST TO BE NACTVE just below label 30. This avoids a
c  very unlikely and subtle error that would occur if the first
c  cycle of a run has a Hessian error. See comments below label 30

c  in main.

c  2. When a Hessian error occurs in Subroutine emint, now the value
c  of info will be reported to the user. If it is positive, it is 
c  the subject no. which  caused the Hessian error (see


c  \ALAN3\NEELY\VORI2\VORI2.EXP for example).

C  3. NEW CODE IN SUBROUTINE NEWWORK1 IS USED TO ESTABLISH THE VALUES
C  FOR DOSEBLOCK, WITHOUT USING A BACKSPACE COMMAND. THE REASON IS THAT
C  DEPENDING ON WHICH COMPILER IS USED TO MAKE THE PR PREP PROGRAM
C  (CURRENT ONE IS NPAG108.FOR), IT IS POSSIBLE FOR A DOSE EVENT
C  TO LOOK LIKE SEVERAL LINES RATHER THAN ONE LONG WORD-WRAPPED LINE.
C  IN THE FORMER CASE, BACKSPACING ONE LINE WILL NOT BACKSPACE TO THE
C  BEGINNING OF THE DOSE EVENT AS SHOULD BE DONE. SO TO BE SAFE, THE
C  LOGIC TO USE BACKSPACE(23) WILL BE COMMENTED OUT, AND 
C  DOSEBLOCK(.,.,.) WILL BE ESTABLISHED DIRECTLY.

C  4. npageng19.f WILL BE COMPILED AND LINKED WITH NEW id MODULES:
C  idm1x11.f REPLACED BY idm1x12.f;
C  idm2x10.f REPLACED BY idm2x11.f;
C  idm3x11.f REPLACED BY idm3x12.f;

C  THE REASON IS TO CORRECT A BUG WHEN N = 0 (ANALYTIC SOLUTIONS CODED
C  INTO SUBROUTINE OUTPUT). IN THIS CASE, THE NDO = ND, 
C  SIGO(.) = SIG(.), ETC. SECTION IS SKIPPED; AND THIS MEANS THAT AT THE
C  END OF idm1x11.f, ND = ND0 BECOMES 0 --> THE NEXT TIME idpc IS 
C  CALLED, THE PROGRAM IS SCREWED UP.

C  THIS BUG ONLY HAPPENS IF N = 0 AND NO TESTCASES WITH N = 0, WHICH
C  WOULD HAVE DISCOVERED THE PROBLEM, WERE RUN SINCE idm1x9.f WAS 
C  USED.



C  THE SOLUTION, IN THE NEW id ROUTINES, IS TO MOVE THE NDO = ND, 
C  SIG0(.) = SIG(.), ETC. ... CODE TO JUST AFTER CALL GETFA.

c  See \ALAN3\NEELY\EMAX\EMAX.EXP for details.


c  5. Formats 1657 and 7124 are changed to show that the output file

c  is made by npageng19 rather than npageng18. 

c  Note that npageng19.f is the main "engine" module for the new PC
c  prep program, NPAG108.FOR.


c-----------------------------------------------------------------------

c  npageng18.f                                             7/01/12

c  npageng18 has the following changes from npageng17:

c  1. "Relative" rather than "real" times are written to the output 
c  files. This means that if a steady state dose set occurs, the output
c  times in the AUC tables, the DENxxxx file, the PTRTxxxx file, etc.
c  will now be based on the end of the steady state dose set rather than
c  the beginning. As an example, if the interdose interval of a steady
c  state dose set is 4 hours, and the last observation for that region

c  is 20 hours, the previous program would have written into PTRBxxxx
c  and the DENxxxx files times of 400.00, 400.0333,..., 444.00 (i.e.,
c  24 hours after the last observation time). Those are the "real" times
c  that are used by the id modules in this program. But now, the times
c  written to these files will be 0.00, 0.033,..., 44.00, which are the
c  "relative" times (i.e., those starting at the end of the 400 hours
c  of steady state dosing).

c  This change is made by having Subroutine CALCTPRED calculate
c  TPREDREL(.) in addition to TPRED(.). TPRED(.) is still used in the
c  call to Subroutine IDCALCYY (i.e., that routine must see the real
c  times), but the times in TPREDREL(.) are the ones written to the
c  output files).

c  Also, when writing the AUC tables, the "relative" times are written
c  rather than the "real" times ... by not adding TBEGGSUB(.,.) to the
c  calculation of IHRST (in the above example, TBEGGSUM(.,.) = 400 is
c  the length of the 100 steady state dose sets).

c  In addition, now Subroutine NEWWORK1 establishes TIMOBREL(JSUB,J)
c  which give the original ("relative") observation times passed via
c  npag102.inp, and these values, rather than the "real" ones stored
c  to File 27 by NEWWORK1, will be written to the end of the PRTBxxxx
c  file.

c  2. The module read16.f linked to this program will be updated to be
c  read17.f. read17.f will no longer read DOSEBLOCK(.,.,.) and
c  OBSBLOCK(.,.,.) from file 27; instead this info will be passed via
c  COMMON/DOSEOBS from Subroutine NEWWORK1. These values will be the
c  values from the patient files in the npag102.inp (i.e., before these
c  working copy format files are converted by SUBROUTINE NEWWORK1 to
c  have a full 100 dose lines for each steady state set, and to have
c  "real" rather than "relative" times in the dose/cov and observation
c  blocks).

c  All other modules linked with the main module, are unchanged.

c  3. Format 2053 has a typo correction (which required a corresponding
c  change in read17.f - see comment 1. in that file).

c  4. If a Hessian Matrix is singular in Subroutine emint, instead of
c  stopping immediately without writing the output files, now the
c  program sets IHESS = -1, and returns to MAIN, where control is
c  transferred to label 900, so the output files can be created, based
c  on the previous cycle's values. Then the program stops.

c  Note that in this case (IHESS = -1), the program will write into the
c  output file that it stopped because of a Hessian error, and it will
c  re-establish CORDEN to be = CORDLAST, which was the joint density
c  array from the previous cycle. This way, when Subroutine SUBRES is
c  called in loop 7000 at the end of the run, the values will all be
c  based on the values from the last fully completed cycle. Otherwise,
c  CORDEN would have been some hybrid combination of the CORDEN from
c  the previous cycle and the partially updated CORDEN from the
c  incomplete current cycle). And because of this new array, CORDLAST,
c  SUBROUTINE MAKEDRIV in the new NPAG107.FOR must be changed to 

c  include this array with variable dimensions as a calling argument
c  to Subroutine NPAG in this module (and CORDLAST must be dimensioned
c  below also).

c  5. Formats 1657 and 7124 are changed to show that the output file
c  is made by npageng18 rather than npageng17. 

c-----------------------------------------------------------------------

c  npageng17.f                                             4/10/12

c  npageng17.f has the following changes from npageng16:

c  It is the main engine module for NPAG106.FOR. The change is that this
c  program allows steady state dose sets to have bolus doses as well as
c  IVs (rather than being limited to just IVs). 

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

c  In addition, the 3 id modules require code changes are updated to be
c  idm1x10.f, idm2x10.f, and idm3x11.f.

c  Also, formats 1657 and 7124 are changed to show that the output file
c  is made by npageng17 rather than npageng16. 

c-----------------------------------------------------------------------

c  npageng16.f                                             11/23/11

c  npageng16 has the following changes from npageng15:

c  1. It is the main engine module for the new PC prep program, 
c  NPAG104.FOR (which will be completed later). For now, it is 
c  compatible with NPAG103.FOR, as long as any subjects with steady
c  state dose sets are input in working copy format. The new
c  NPAG104.FOR/npageng16.f program now allows patient files to have 
c  steady state dose sets. In particular, this program now calls new
c  subroutine NEWWORK1 (based on the stand-a-lone program of the same
c  name) to read each working copy file in npag102.inp, which may have
c  steady state dose indicator lines, and convert it to the typical form
c  that that the id routines require, except that the steady steady 

c  state dose indicators themselves (negative dose times) remain in the
c  file.

c  2. blasnpag.f is not changed, but the other 5 permanent engine .f
c  files linked to this program, read15.f, shift6.f, idm1x6.f, 
c  idm2x6.f, and idm3x6.f, will be updated to be read16.f, shift7.f,
c  idm1x7.f, idm2x7.f, and idm3x7.f. The changes in the id routines will
c  allow them to read steady state dose indicators and know how to 
c  integrate through each set, testing for convergence of the steady
c  state compartment amounts to see if the program can fast forward to
c  the end of that set. The changes for read16.f are simply in the
c  dimensions related to no. of doses, which will change from 500 to 
c  5000 (see comment 4. below). The changes in shift7.f are the 
c  dimensions changes of read16.f, and some edited code required since

c  now a dose (time) reset occurs when a dose time is .LE. 0, rather 
c  than .EQ. 0.

c  3. All 500's related to dose times dimensions (which are passed in

c  COMMON/OBSER) are changed to 5000's to be compatible with the new
c  id routines. This is because in those routines, because of steady
c  state dose sets, the no. of dose times can expand to be much 
c  bigger than 500. All 500's will also be changed to 5000 in 
c  Subroutine FILRED.

c  4. Many changes are made in Subroutine CALCTPRED and Main (where

c  the AUC tables are calculated) to take into account that the 


c  predicted values to be calculated by IDCALCYY only start at t=0
c  if there is no active steady state dose set; if there is an active
c  steady state dose set, then the predicted values start at the end
c  of that dose set.

c  5. A bug in loop 2050 (calculating AUCs) is corrected (see code
c  involving new parameter ILAST).

c  6. When the variance of a parameter in a cycle is numerically .LE. 0,

c  rather than suppress all covariance-related output for that cycle,
c  as was done in previous programs, now the covariances, std. devs.,
c  and coefficients of variations will all be written as 0's, and the
c  correlations as -99's. This change is because suppressing the output
c  means that the NP_RFxxxx.TXT file (made by Subroutine READOUT) has 
c  fewer than expected std. devs., which screws up the R program which
c  reads that file. This same change is made in Subroutine SUBRES
c  for the Bayesian statistics for each subject.

c  7. Variables PRCN, tbeg, PRFIX2, EXT2, IFORMT, NSTORESV, IPRED, JCOL,
c  T1000A, saveres are now removed, as they are no longer used (and 

c  haven't been for some time).

c  8. Formats 1657 and 7124 are changed to show that the output file is
c  made by npageng16 rather than npageng15. 

c-----------------------------------------------------------------------

c  npageng15.f                                             7/29/11

c  npageng15 has the following changes from npageng14:

c  1. This module is part of the NPAG102.FOR program. That program was
c  changed to provide an npagdriv.f file (rather than an npemdriv.f 
c  file), and one of the changes in the new npagdriv.f file is the 
c  CALL NPAG statement, rather than a CALL BIGNPAG statement. As a 

c  result, the SUBROUTINE BIGNPAG statement below is replaced by 
c  SUBROUTINE NPAG.


c  2. The instruction file made by the PC prep program (NPAG102.FOR is
c  the first in the series) is changed from npembg34.inp to 
c  npag102.inp. This was done since now NDRUG and AF(I),I=1,NDRUG will
c  be in the file, as opposed to just AF (i.e., it is no longer
c  assumed that all drugs have the same active salt fraction).

c  3. Formats 1657 and 7124 are changed to show that the output file is
c  made by npageng15 rather than npageng14. Also in Format 1657, the
c  version no. is changed from 41 to 42 (since NDRUG and 
c  AF(I),I=1,NDRUG will be written to the output file, rather than
c  just AF).

c  4. This program will now be linked with read15.f, updated from
c  read14.f (which must be updated to write NDRUG and AF(I),I=1,NDRUG,
c  rather than just AF). Also, it reads info regarding ICONVERGE
c  differently now (see 5. below). The other permanent modules, 
c  blasnpag.f, shift6.f, idm1x6.f, idm2x6.f, and idm3x6.f are unchanged.

c  5. New code involving IMAXCYC enables the program to test whether
c  the analysis converged at MAXCYC. Previously, if ICYCLE = MAXCYC,
c  the program would stop without checking to see if convergence was
c  also achieved. Now convergence will still be tested even if ICYCLE
c  = MAXCYC. Note that new parameter, ICONVERG, along with IMAXCYC
c  will enable the program to write below label 900 the reason the

c  program stopped (because of MAXCYC being run; because the analysis
c  converged; or because of both of the above) - see formats 5197 
c  through 6001.

c-----------------------------------------------------------------------

c  npageng14.f                                             6/29/11

c  npageng14 has the following changes to npageng13:

c  1. The comment in the output file regarding the stopping criterion is
c  changed (JSTOP has been ignored since Bob Leary's adaptive grid
c  algorithm was inserted in 2000). TOL was still used in a complicated
c  way in the stopping criterion, but was not allowed to be < 1.D-4. To
c  simplify the code, TOL will now not be used as read in from 
c  npembg34.inp. Instead it will always be hardcoded to be 1.D-4.

c  Note that though JSTOP and TOL are not used, they are still read in
c  from npembg34.inp, so the format of this file does not have to be
c  changed.

c  Formats 1221 and 1222 are changed to report the starting cycle no.,
c  and the maximum cycle no., and new format 1223 reports that the 

c  program will stop prior to the max. cycle no. if convergence is
c  achieved.

c  Note also that all code involving PRESLP, which was related to JSTOP,
c  has been removed.


c  New formats 5197 and 5198 write to the output file why the analysis

c  stopped (i.e., either MAXCYC reached or convergence was obtained).

c  2. New formats 9771 and 9772 write to output file 25 whether the
c  apriori density is uniform or is a prior run's density file; they
c  replace the previously unformatted write statements.

c  3. Since all ATOL(I) are set = RTOL, the comment in the output file
c  is simplified to report just RTOL as the value of the tolerance used
c  in the differential equation solver (VODE). See FORMAT 9769.

c  4. The comment in the output file that the no. of intervals used
c  in calculating the marginal density approximations is 100 is removed
c  since this value (NINT) is always 100.

c  5. Formats 1657 and 7124 are changed to show that the output file is
c  made by npageng14 rather than npageng13. Also in Format 1657, the
c  version no. is changed from 40 to 41 (since new/changed info will be
c  written to the output file).


c  6. Parameter ilast is removed from MAIN. It was set but never used.

c  7. This program will now be linked with read14.f, updated from
c  read13.f. The other permanent modules, blasnpag.f, shift6.f,
c  idm1x6.f, idm2x6.f, and idm3x6.f are unchanged.

c-----------------------------------------------------------------------

c  npageng13.f                                             4/28/11

c  npageng13.f has the following changes from bigmlt12.f:

c  1. This is the main module in the program whose PC Prep program is
c  the new NPAG100.FOR. As in that .FOR module, the formula for NI in 
c  Subroutine FILRED is changed from  NI = 2*NDRUG + 2 + NADD  to  
c  NI = 2*NDRUG + NADD, because from now on, WT and CCR will not be 
c  considered special covariates. If they are included in the working 
c  copy file, they will be part of the NADD 'additional' covariates 
c  (beyond the 4 permanent ones in Common DESCR).

c  Because of the above change, the shift5.f module linked to this
c  program will be updated to shift6.f.

c  2. This program will be linked with read13.f, updated from read11.f
c  and then two versions of read12.f.
 

c  The difference is that read13.f will include the assay coefficients
c  for each observation in the rfile, which will be renamed to be
c  NP_RFxxxx.TXT from RFILExxxx.TXT.

c  3. Formats 1657 and 7124 are changed to show that the output 
c  files are made by npageng13 rather than bigmlt12.

c-----------------------------------------------------------------------

c  bigmlt12.f                                              01/11/11



c  bigmlt12 has the following changes from bigmlt11:

c  1. Messages to the user are written after the end of the first cycle
c  (see format 1243 in MAIN) and in Subroutine emint (see format 123) 
c  so that he will know the program has not 'hung' if the adaptive grid
c  optimization takes several minutes (as it can with a large no. of 
c  subjects and grid points).

c  2. The Akaike Information Criterion (AIC), and the Schwartz 
c  (Bayesian) Information Criterion (BIC) are written to the output 
c  file. They are calculated similarly to how they are in the Big 
c  IT2B program. 


c  Note that this main "engine" module is now linked with read11.f, 
c  rather than read10.f. The difference is that read11.f stores the new
c  AIC and BIC values.

c  Also, of course, formats 1657 and 7124 are changed to show bigmlt12,
c  rather than bigmlt11.f.

c  Note that the first PC Prep program to use the bigmlt11.f "engine"
c  is NPBG15E6.FOR.

c-----------------------------------------------------------------------

c  bigmlt11.f                                              12/20/10

c  bigmlt11 is the same, functionally, as bigmlt10.f. The difference 
c  is that it is linked with idm1x6.f, idm2x6.f, and idm3x6.f, all
c  updated from the _____5.f versions. The other permanent .f files, 
c  blasnpag.f, shift5.f, and read10.f are unchanged.

c  Also, of course, formats 1657 and 7124 are changed to show bigmlt11,
c  rather than bigmlt10.f.


c  Note that the first PC Prep program to use the bigmlt11.f "engine"
c  is NPBG15E5.FOR.

c-----------------------------------------------------------------------

c  bigmlt10.f                                              12/13/10

c  bigmlt10 has the following changes to bigmlt9:


c  1. In Subroutine SUBRES, the NACTVE grid points from the final cycle
c  are checked and the no. which are "active" for each subject (i.e.,
c  within 1.D-10 of the maximum density for that subject) is printed to

c  the screen and File 25 (previously just the no. of active grid pts.
c  from the joint density of the final cycle was printed).

c  2. When Andreas' Intel compiler compiles the program, it objects to
c  RPAR and IPAR being dimensioned (as (*)) in Subroutine DVODE (and
c  routines called by DVODE) when they are not dimensioned in 
c  Subroutine USERANAL (in idm1x5.f). The comments in DVODE state that
c  if these values are not being used, they do not need to be 
c  dimensioned in routines that call DVODE. Nevertheless, to remove the

c  Intel objection, RPAR(*) and IPAR(*) are removed from the 5 routines
c  in this module which declare them arrays.

c  Similarly, RTOL supposedly does not need to be dimensioned in 
c  USERANAL since it is a scalar, but the Intel compiler objects to
c  having it dimensioned (*) in DVODE, etc. when it is a scalar in
c  USERANAL. So, all RTOL(*) occurrences are removed in this module,
c  and all references to RTOL(1), RTOL(I), etc. are changed to RTOL.

c  3. Formats 1657 and 7124 are changed to show bigmlt10, rather than 
c  bigmlt9.

c  Note that the first PC Prep program to use the bigmlt10.f "engine"
c  is NPBG15E4.FOR.

c-----------------------------------------------------------------------

c  bigmlt9.f                                               11/21/10

c  bigmlt9 has one major change to bigmlt8:

C  A CALL TO NEW SUBROUTINE PAUSE REPLACES EACH PAUSE STATEMENT. 
C  THIS IS BECAUSE A PAUSE STATEMENT CAUSES A WARNING WHEN THE PROGRAM
C  IS COMPILED AND LINKED USING gfortran (AND IT FORCES THE USER TO 
C  TYPE "go" INSTEAD OF SIMPLY HITTING THE ENTER KEY). ALSO, SEVERAL
C  PAUSE STATEMENTS THAT WERE PREVIOUSLY COMMENTED OUT ARE NOW 
C  "REINSTATED" WITH CALL PAUSE COMMANDS.

c  Also, formats 1657 and 7124 are changed to show bigmlt9, rather than 
c  bigmlt8.

C  Note that one of the modules linked to the bigmlt9.f "engine" is
c  read10.f (updated from read9.f). The only change in read10.f is the
c  same one as above in this module.

c  Note that the first PC Prep program to use the bigmlt9.f "engine"
c  is NPBG15E3.FOR.

C-----------------------------------------------------------------------

c  bigmlt8.f                                               11/10/10



c  bigmlt8 has the following changes to bigmlt7:

c  1. In Subroutine CALCTPRED, a code change is made so that an initial 
c  observation time of 0 does not trigger a "time reset".

c  2. Formats 1657 and 7124 are changed to show bigmlt8, rather than 
c  bigmlt7.

c  3. At the end of the run, the logic of read9.f (SEE
c  \ALAN3\NEELY\READOUT\READOUT.EXP) will create RFILExxxx.txt, an 
c  output file which is easy to use with the program, R. This will
c  be done by calling READOUT, the main routine in the module read9.f,
c  which is now compiled with this program. 

c-----------------------------------------------------------------------

c  bigmlt7.f - revised                                        11/05/10

c  The revised version of bigmlt7.f is identical to the original, except
c  that formats 1657 and 7124 are changed to show that the output 
c  files are made by bigmlt7.f, rather than bigmlt6.f

c-----------------------------------------------------------------------

c  bigmlt7.f                                                  10/26/10

c  bigmlt7 is the same as bigmlt6 except at the end of the run, new 
c  Subroutine CONDENSE (based on the free standing program, 
c  CONDENSE.FOR) is used when writing lines to the combined output 
c  file, OUTxxxx. This makes OUTxxxx a much smaller file - by only 
c  using line sizes which are required for each line, rather than 
c  always using A1000 as the the format.

c-----------------------------------------------------------------------

c  bigmlt6.f						   4/3/10


c  bigmlt6 has the following changes to bigmlt5:


c  1. In bigmlt5.f, the PRTB file was not completely written in the case
c  where there were time resets in the patient data files. This was
c  because the following condition in idm3x4.f just below label 45
c  was never engaged ...
c	IF(TPRED(KNT) .EQ. 0.D0 .AND. SIG(KNS) .EQ. 0.D0) THEN
c  ... since the predicted times went monotonically from 0 to 24 hours
c      past the last observed value time over all the subjects (this
c      value, T_END, was calculated by NPBIG15D.FOR and passed to 
c      bigmlt5.f in npembg34.inp). Instead the observed value times 
c      should have gone from 0 to 24 hours past the largest observation 
c      time before the next time reset value of 0, for each subject 
c      individually. This logic will be done in bigmlt6; these 
c      predicted value times will be calculated by routine CALCTPRED 
c      for each subject in turn. i.e., there will now be as many 
c      predicted sets of concentrations as there are time resets.

c  Also note that the AUCs in bigmlt5.f were not calculated correctly
c  when the patients had time resets, for the same reason as indicated
c  in the above paragraph. But now there will be one AUC table for 
c  each time reset for each output equation for each subject.

c  See new Subroutine CALCTPRED which calculates the NUMT(JSUB) values
c  in TPRED, based on each subject's observed value times. Note that
c  these observed value times will be IDELTA minutes apart, and will
c  consist of times up to 24 hours after each max. observed value time 
c  (and there will be as many sets of times for each subject as there
c  are time resets).

c  2. The code for the density file will now be changed since NUMT will
c  now be an array (since it will be different for each subject).
c  i.e., NUMT(JSUB) and TTPRED(JSUB,.) will have to be written for each 
c  subject (see code). Also, since the combined output file includes the 
c  density file, its code will be changed also. i.e., formats 1657 and 
c  7124 now show bigmlt6 as the main "engine" module. Note that TPREDD
c  is now changed to TTPRED to be consistent with the names in 
c  NPBIG15E.FOR (the PC Prep program) associated with this program.


c  Note that, though T_END will not be used anymore, it will still be
c  read in from npembg34.inp so the structure of this file will not
c  have to change. Note that IDELTA will still be used.

c  3. bigmlt6.f will now be linked with 3 new id modules (idm1x5.f,

c  idm2x5.f, and idm3x5.f). The previous id modules had a bug related
c  to the time resets (see code in the new id modules).

c-----------------------------------------------------------------------

c  bigmlt5.f							12/12/09

c  bigmlt5.f is exactly the same as bigmlt4.f, except ...
c  formats 1657 and 7124 now show bigmlt5 as the main "engine"
c  module.

c  The new name (bigmlt5 vs. bigmlt4) is used since the 3 id files
c  this program calls (idm1x4.f, idm2x4.f, and idm3x4.f) are updated

c  from the previous set (idm1x3.f, idm2x3.f, and idm3x3.f).

c-----------------------------------------------------------------------

c  bigmlt4.f							9/18/09

c  bigmlt4 has the following changes from bigmlt3:

c  1. The modules with which it is linked are changed:
c  idm1x2.f, idm2x2.f, and idm3x2.f are changed, respectively to
c  idm1x3.f, idm2x3.f, and idm3x3.f. Also, shift4.f is changed to
c  shift5.f.

c  2. Subroutine XERRWD is changed so that it writes no warning


c  messages to the screen. Instead, the no. of calls to XERRWD is
c  passed back to main and written to the screen in loop 800 (see
c  code related to NXE in several places). The reason is that if 
c  there are a lot of warnings written to the screen, it can slow the 
c  overall program down a lot (in one example, it slowed the program 

c  by a factor of almost 2.5).

c  3. All references to NTLAG have been removed. The reason is that
c  the user now codes explicitly his/her formulas for TLAG (and FA and
c  IC) into his model file (the new template is TSTMULTG.FOR).

c  4. bigmlt4.f is the new main "engine" module for NPBIG15C.FOR 
c  (updated from NPBIG15B.FOR).

c  5. Formats 1657 and 7124 now show bigmlt4 as the main "engine"

c  module.


c-----------------------------------------------------------------------

c  bigmlt3.f							9/4/09


c  bigmlt3 is the same as bigmlt2 except that formats 2048, 2049, 2051,
c  and 2052 have been changed to allow for larger numbers.


c  Also, the modules it calls are changed:
c  idm1x1.f, idm2x1.f, and idm3x1.f are changed, respectively to
c  idm1x2.f, idm2x2.f, and idm3x2.f.

c-----------------------------------------------------------------------

c  bigmlt2.f							7/7/09

c  Slight correction from original bigmlt2.f (dated 6/1/09):


c  BS(500,3) is changed to BS(500,7) in the dimension statement of
c  the main module (this change should have been made at the same time
c  the chnage was made in Subroutine FILRED - with the program
c  bignpaglap2.f.

c-----------------------------------------------------------------------

c  bigmlt2.f							6/1/09

c  bigmlt2 is the main module for the "engine" which corresponds to 
c  the most recent multiple drug PC PREP PROGRAM, NPBIG15A.FOR. The

c  other modules of the "engine" are idm1x1.f, idm2x1.f, idm3x1.f,
c  shift3.f, and blasnpag.f (the latter two of which are unchanged from 
c  the previous version). The changes in this modules from bigmlt1.f are
c  essentially the changes that the single drug program, big28.f, had
c  from big24.f. They are as follows:

c  1. The time interval for AUCs, instead of being hardcoded to be 24
c  hours, will be read in from the instruction file, npembg34.inp
c  (changed from npembig33.inp). This value is called AUCINT.

c  The first Big NPAG PC Prep program which is compatible with this
c  program (i.e., makes npembg34.inp) is NPBIG15A.FOR.

c  Also, formats 1657 and 7124 are changed so the name of this "engine"
c  program, bigmlt2.f, is written.

c  2. THE DIMENSIONS OF AB, PAR, PARFIX, VALFIX, AND IRAN ARE 

C  MADE CONSISTENT WITH THE MAXIMUM ALLOWED VALUES (A MAXIMUM OF max_pop_rand_varbs 
C  RANDOM PARAMETERS AND max_pop_params FIXED PARAMETERS). IN PARTICULAR:
C  ALL ARRAYS RELATED TO THE NO. OF RANDOM VARIABLES ARE NOW DIMENSIONED 
C  max_pop_rand_varbs; ALL ARRAYS RELATED TO THE NO. OF FIXED PARAMETERS ARE NOW 
C  DIMENSIONED max_pop_params; AND IRAN IS STIL DIMENSIONED max_ODE_params.

C  ALSO, ALL FORMATS RELATED TO THE NO. OF RANDOM VARIABLES ARE CHANGED
C  TO max_pop_rand_varbs FROM 25; SIMILARLY THOSE RELATED TO THE NO. OF FIXED VARIABLES

C  ARE CHANGED FROM 12 TO max_pop_params.

C  NOTE THAT THIS PROGRAM WILL BE COMPILED AND LINKED WITH A NEW MODEL
C  FILE TEMPLATE, TSTMULTE.FOR, IN WHICH PSYM IS DIMENSIONED max_ODE_params
C  (INCREASED FROM 25) IN SUBROUTINE SYMBOL. ALSO SEE BELOW FOR ANOTHER
C  CHANGE TO TSTMULTE.FOR.


c  3. READLARG has been changed from *300 to *1000, and FORMAT
c  2717 has been changed from A300 to A1000. The reason is that in a
c  run using George's 24 parameters (see \ALAN3\GEORGE\DRUSANO7.EXP),
c  NPBIG10H bombed when doing option 5 on \ALAN3\BIGNPAG\OUT0436 - the 
c  reason is that big25.f cut off the output file lines at entry 300
c  and this cut off the means, covs, etc. of parameter no. 24. Then,
c  NPBIG10H couldn't read the 24th no. and bombed with an 
c  "invalid numeric input" error.

c  4. READLINE has been changed from *78 to *300 in several routines,
c  and the corresponding format for READLINE has been changed to
c  A300 from A78 in each of those routines. The reason is that
c  in one of the patient data files that George sent, his observed

c  values went past column 78 (which --> the values were cut off
c  when read by big25).

c  5.In addition to storing YPREDPOP, this program now stores 
c  YPREDPOPT(JSUB,IEQ,J,ICENTER) = the predicted value for Y for
c  subject JSUB, for output equation IEQ, for time J, for 
c  ICEN = 1 (means), 2 (medians), AND 3 (modes), where the means, 
c  medians, and modes are from the final cycle population density.



c  These values differ from YPREDPOP in that the observed values
c  do not occur at the J=1,NOBSER observation times in a patient's
c  data file. Instead, they occur at the values of t inside TTPRED

c  (see the logic for forming the PRTBxxxx file = file 31), which are
c  all the times from t = 0 till t = 24 hours past the last obs. time
c  among all the patient data files ... but the no. of values is
c  capped at 7201 (see logic below for doing this).

c  Because the density file, and therefore the combined output file,
c  have extra info (YPREDPOPT) in them, they will have a new code.
c  In particular, format 1657 will be changed to specify VERSION 39
c  (and MADE BY bigmlt2.f), and format 7124 will be changed to specify
c  DENSITY JUN_09 (and MADE BY bigmlt2.f).

c  6. 3 modules which are linked into this program are changed. In
c  particular, idfix5g.f, idcy_53g.f, and idcy_63g.f are changed, 
c  respectively, to idm1x1.f, idm2x1.f, and idm3x1.f. These new modules 
c  allow the extra option of setting initial compartment amounts from 
c  their initial concentrations - see code in Subroutines FUNC, FUNC2, 
c  and FUNC3. And they allow patient data files to have "reset" values 
c  of 0 in the dosage and sampling blocks. Whenever, in Subroutine FUNC 
c  (or FUNC2 or FUNC3) the program sees a SIG(.) = 0 and a TIM(.) = 0, 
c  it knows that a large enough time has passed since the last dose
c  that all compartment amounts are to be reset = 0. Subsequent dose
c  and observed value times are values from this point.

c  Note that the modules blasnpag.f and shift3.f are unchanged.
c  Note that the new model file template, TSTMULTE.FOR, has code in
c  Subroutine SYMBOL that allows the user to set up the option indicated
c  above of setting initial compartment amounts from their initial
c  concentrations.


c-----------------------------------------------------------------------

c  bigmlt1.f							1/6/08

c  bigmlt1.f is the main module for the "engine" which corresponds to 
c  the most recent multiple drug PC PREP PROGRAM, NPBIG15.FOR. The

c  other modules of the "engine" (idfix5g.f, idcy_53g.f, idcy_63g.f,
c  shift3.f, and blasnpag.f) are unchanged. The changes in this module


c  from bignpaglap4.f are as follows:

c  1. It reads in a different instruction file, npembig33.inp (updated

c  from npembig3.inp). npembig33.inp has extra info. Instead of just

c  NSUB, the no. of subjects, it also has NSUBTOT and IPATVEC(I),
c  I=1,NSUB, where NSUBTOT is the total no. of subjects in the


c  patient population (all of these subject files are concatenated
c  on npembig33.inp), NSUB is the no. of these subjects which are to
c  be analyzed in this run, and IPATVEC(I),I=1,NSUB are the indices
c  of these "active" subjects for this run. Note that the first
c  PC PREP program which creates npembig33.inp is NPBIG15.FOR.


c  The code will be changed to write just the "active" subject data
c  files (IPATVEC(I),I=1,NSUB) onto the scratch file (27) to be analyzed

c  during this run (see code below label 1717).

c  2. This program will write just the "active" NSUB patient data files
c  into the output file, along with IPATVEC(I),I=1,NSUB, so the PC PREP 
c  program will know which subjects were analyzed. Because of the extra 
c  info to be put into the output file (NSUBTOT, IPATVEC), the code for 
c  the output file will be changed from VERSION 37 to VERSION 38 in 
c  FORMAT 1657.

c  3. New subroutines, WRITEPT2, GETIPATF, GETNUMSF, GETSUB are
c  added.

c  4. Note that formats 1657 and 7124 are changed to write out this
c  program "bigmlt1. And VER_BAK AUG_02 is changed to VER_BAK NOV_07
c  just below format 1657.

c  5. In subroutine EMINT,  rmax = -1.e100  is changed to  rmax = -1.e38
c  to be compatible with the Fortran F77L3 compiler (i.e., e100 is too
c  big for that compiler). In addition, all etime(dummy) references are
c  changed to 0 (since the PC compiler doesn't recognize etime).

c  6. In SUBROUTINE DPOTRF, both references to  ILAENV are removed. It
c  is never used, and being declared EXTERNAL causes an error when
c  the program is linked on the PC using the F77L3 linker.

c  7. The screen dump of all output info from the main module has 
c  been replaced by four lines having just the cycle no., convergence 
c  criterion, and the medians. The exception is that until
c  NACTVE .LE. NSTORE, the program will print to the screen the update 
c  on what % of grid points have been calculated since otherwise the 
c  user might think his computer has locked up (once NACTVE .LE. NSTORE, 
c  all the P(YJ|X)'s will already be stored into PYJGX --> the DO 800 
c  loop will go very fast.


c  Note that ISUPRES will be hardcoded = 1 (this will tell the user
c  the the above minimal info is to be printed to the screen). If
c  for some reason the full info is needed again, I can just change
c  ISUPRES = 0 in the code.

c  8. The user will be told before the run begins that he can 
c  execute the batch file, CHMAXCYC.BAT (by typing CHMAXCYC at 
c  a DOS prompt), in the working directory of a PC and that will
c  cause the program to stop safely at the end of whatever cycle it 
c  is on, as if the maximum no. of cycles has been reached.


c  This batch file will simply copy to the file CHMAXCYC.OLD the file

c  CHMAXCYC.NEW. Note that CHMAXCYC.BAT, CHMAXCYC.OLD and CHMAXCYC.NEW 
c  are made by this program before cycle calculations begin. 
c  CHMAXCYC.OLD has a 1 on the first line and CHMAXCYC.NEW has a 0 on 
c  the first line. The program will open and read CHMAXCYC.OLD at the 
c  beginning of each new cycle. If it reads a 1, it will continue the 
c  calculations as before. If it reads a 0, it will change the value of 
c  MAXCYC to whatever ICYCLE is currently, which will cause the program 

c  to halt at the end of that cycle just as if MAXCYC had been set
c  to the changed value originally. If this happens, the program will
c  write a comment to the console and file 25 of why the program

c  has stopped prematurely.

c  Note that the user may want to exercise this option if he wants to
c  see the results from a slowly converging run, and then, because
c  the density file from the last cycle will have been correctly 
c  created, still be able to start another run using the final cycle 
c  joint density from the halted run (with different parameters if 
c  desired).

c  Note that because of the messages to the user, accompanied by
c  PAUSE commands, this program must be run interactively at least
c  at the start. I.e., if this program is to be run without user
c  oversight, I will have to take out the PAUSE commands, and of 
c  course then the option described here will be moot (i.e., without
c  user interaction, the user would not know that the program is
c  converging slowly).


c  9. ADDITIONAL INFO WILL BE WRITTEN INTO FILE 31 (PRTBxxxx). 
C  PREVIOUSLY, FOR EACH SUBJECT THE PREDICTED VALUES (BASED ON EITHER 
C  THE MEANS, MEDIANS, OR MODES FROM THAT SUBJECT'S BAYESIAN POSTERIOR 
C  DISTRIBUTION) WERE CALCULATED AND WRITTEN FOR THE TIMES IN TPRED. 
C  NOW, IN ADDITION, FOR EACH SUBJECT AND EACH OUTPUT EQUATION, THE 
C  OBSERVATION TIMES AND OBSERVED VALUES (FROM EACH SUBJECT'S PATIENT 
C  FILE), AND PREDICTED VALUES (BASED ON THE MEANS, MEDIANS, OR MODES 
C  FROM THAT SUBJECT'S BAYESIAN POSTERIOR DISTRIBUTION) WILL BE WRITTEN.

c  10. New format 5456 has been added to clarify the results in the
c  output file.

c  11. Every STOP statement, except the one that terminates the program 
c  after a complete run, is now preceeded by a PAUSE statement. This is
c  done so that when this program is run unders windows, the window 

c  will not disappear immediately with no explanation. i.e., with the
c  PAUSE statements, the explanation for the stopping of the program

c  will remain on the screen until the user presses the enter key.

c  12. The limitation of 250000 for maxactem in Subroutine emint is 
c  changed to be 10000000.

C  13. THE CODE AT THE END OF MAIN (TO REMOVE THE fort.27 FILE, AND
C  TO WRITE THE TIME OF THE RUN INTO FILE 91) ARE COMMENTED OUT
C  SINCE THEY ARE NO LONGER APPLICABLE.

C  14. The combined output file is made using READLARG (and format 2717) 
c  instead of using READLINE (and format 1717). READLARG is a 
c  character 300 variable, where READLINE is a character 78 variable. 
c  The extra characters ensures that no lines will be cut off.

C  15. At the end of each cycle, the program now
c  writes the 1st part of the density file into DENFIL (i.e., it 
c  overwrites this info from the previous cycle). This will be 
c  useful if the program crashes after a long run ... since then the 
c  user can simply run the PC prep program and restart the run using 
c  the latest density file as the aprior density (i.e., the engine will
c  pick up where it crashed). big1 only made the density file (and

c  the combined output file) at the end of the run.

c  Note that the density file created at the end of each cycle is
c  not the full density file created at the end of the run. It only
c  includes the info down to CORDEN, which is all that the PC prep
c  program needs for its apriori density.

c  16. It checks MAXCYC to see if it is 0. If so, it means that the user
c  wants to bypass the usual NPAG analysis, and instead calculate the
c  output files based on the input density (which becomes the "final
c  cycle joint density") and the patient data files. This option is
c  used to get the Bayesian Posterior Joint Densities (and predicted
c  values, etc.) for a set of subjects, based on a joint density from
c  a previous Big NPAG run.


c  Note that Subroutine SUBRES (and its argument list) is changed to
c  accomadate the calculation of PYJGX in case MAXCYC = 0 (see 
c  explanation before the call to SUBRES).


c  17. The no. of random variables is now .LE. 25 rather than .LE. 20. 
c  Also, the no. of fixed parameters is now .LE. 7 rather than .LE. 12.

c  18. In the DO 800 loop, the % increment reported during the cycle 
c  calculations for each subject is changed to every 1% rather than

c  every .1%.

C  19. NOTE THAT THE PATIENT DATA FILES WILL BE CONCATENATED AS USUAL
C  AT THE END OF npembig33.inp, REGARDLESS OF WHETHER IFORMT = 1 OR 3.
C  I.E., AFTER READING IN IFORMT, THE PROGRAM WILL READ IN PREFIX AND 
C  EXT. I.E., THERE WILL BE NO PRFIX2/EXT2, ETC.

c-----------------------------------------------------------------------

c  bignpaglap4.f						8-31-03


c  bignpaglap4.f has the following changes from bignpaglap3.f:

c  1. In the output file, NGRID is written using format 9869 so 
c  SUBROUTINE PREVRUN in the PC PREP PROGRAM will easily be able to 
c  read in NGRID. Previously, NGRID was established from the reading 
c  in of INDPTS in the density file, but this could cause a problem 
c  if NGRID was reset because it was larger than MAXACT, since INDPTS 
c  is not reset (see comments in NPBIG14.FOR).

c  2. Note that formats 1657 and 7124 are changed to write out this
c  program "bignpaglap4.

c  3. VERSION 36 in FORMAT 1657 is changed to VERSION 37, because

c  SUBROUTINE PREVRUN in the PC PREP PROGRAM will only read NGRID
c  from the output file (see format 9869) from this version on.

c  4. Subroutine STAZ has a bug fix. Previously, grid points which 
c  landed exactly on the upper boundary value didn't get counted.
c  Now, they will be (see code).

c  5. A bug is fixed which could occur when the program concatenates 
c  the density file onto the combined output file. Line are truncated to 

c  72 characters, and so when the the following line is copied:
c  198.532739532312       0.314723681391700       0.405442300013591D-001
c  it will be written as 
c  198.532739532312       0.314723681391700       0.405442300013591D-
c  and then when the PC preparation program (currently NPBIG14.EXE)
c  reads this line, it will bomb since it expects 3 nos. in this line,
c  but the last no. is not read as a number (because of the "D-").

c  The fix is to read and write 78 characters rather than just 72 
c  characters. So, format 1717 will be changed to A78, and READLINE
C  will be changed to CHARACTER*78. Also, in Subroutine Filred, 
c  the same change will be made since it doesn't hurt, and this
c  keeps the code consistent throughout.

c-----------------------------------------------------------------------


c  bignpaglap3.f						8-29-02

c  bignpaglap3 has the following changes from bignpaglap2:


c  1. Instead of hardcoding ierrmod = 1, ierrmod (and gamlam0) will
c  be read in from npembig3.inp (changed from npembig2.inp). There is
c  corresponding new code to assign the inital value of gamma or flat 
c  from gamlam0.

c  2. For each cycle, ierrmod and gamlam are now written into the
c  output file. The code "bignpaglap2" is changed to bignpaglap3 in
c  both the output and density file. Also, the version code in the
c  output file is changed from VER_BAK DEC_01 to VER_BAK AUG_02 
c  (because of the additional information, ierrmod and gamlam, which
c  is written to the file).


c  3. New output formats 2112, ..., 2117 are put into the output file
c  to remind the user what the assay std. dev. model is in the run.

c  4. New code involving LASTCYC prevents the cycle no. from being
c  written 3 times for each cycle if ierrmod .GE. 2 (since the code

c  to estimate gamma/lambda if ierrmod .GE. 2 requires the loop which 
c  includes writing the cycle no. to be executed 3 times for each
c  cycle).

c-----------------------------------------------------------------------

c  bignpaglap2.f						7-24-02

c  bignpaglap2 is the multiple drug version of bignpaglap1 (which only

c  allows one drug). It essentially makes the same changes to 

c  bignpaglap1 (in this regard) that npbig8adapt.f made to 
c  npbig7adapt.f. 

c  Note that just as bignpaglap1.f is at the same "level" as 
c  npbig7aadapt.f [except that the former has Bob Leary's updated 
c  efficiency improvements, and multiplicative GAMMA and additive LAMBDA 
c  options (which are turned off for now)], bignpaglap2.f will be at the
c  same "level" as npbig8adapt.f [with same exceptions as indicated
c  above].


c  The changes are as follows:



c  1. bignpaglap2 allows multiple drugs. Coding changes are required
c  in Subroutine FILRED to read in multiple drug info. Note that each 
c  drug will have one column for IV values and one column for bolus 
c  values. In addition, there are other changes to the formatting 
c  (see 2DRUG001 for an example of a typical new working copy patient 
c  data file). Note that the only dimension changes in Subroutine FILRED
c  are: BS(500,3) is changed to BS(500,7), and NTLAG is now a vector 
c  instead of a scalar (it has dimension 7).

c  This file is now compiled with blasnpag.f, idfix5g.f (updated from 
c  idfix5f.f), idcy_53g.f (updated from idcy_53f.f), idcy_63g.f (updated 
c  from idcy_63f.f), and shift3.f (updated from shift2.f).

c  2. In the regular OUTPUT file and the density file, 
c  "MADE BY bignpaglap1" is replaced by "MADE BY bignpaglap2".

c  3. SUBROUTINE BIGNPEM is replaced by SUBROUTINE BIGNPAG to emphasize 


c  that the new code for multiple inputs is only applicable for the
c  BIG NPAG program (note that the first PC preparation program used 
c  with this program was NPBIG11.FOR).

c-----------------------------------------------------------------------


c  bignpaglap1.f						7-19-02

c  Note that bignpaglap1.f is at the same "level" as npbig7aadapt.f,
c  but it has Bob Leary's updated efficiency improvements, and 
c  multiplicative GAMMA and additive LAMBDA options (which are 
c  turned off for now).

c  bignpaglap1.f has the following changes from bignpaglap.f
c  (bignpag.f):

c  1. All information needed by the Big PC Prep Program (currently 
c  NPBIG10B.FOR) will now be put into one combined output file. That is,
c  this program will essentially concatenate 4 files which were 
c  previously kept separate, OUTFIL, DENFIL, npembig2.inp (actually 
c  just the patient data portion of npembig2.inp), and npemdriv.f. This 
c  will enable the user to run the PC Prep Program with just this one
c  combined output file (i.e., even the working copy patient data files
c  will no longer be needed).


c  THE name for the OUTPUT FILE is now changed to 'OUTT'//NAME SINCE 
c  'OUT//NAME will be reserved for the combined OUTPUT FILE formed at 
c  the end of the run.

c  2. In the regular OUTPUT file, on line 1, the version no. is changed 
c  from 35 to 36, and "MADE BY bignpaglap1" is added to the 1st lines 
c  in the output and density files. See formats 1657 (changed) and
c  7124 (new). Also, the 2nd line in the output file is changed to 
c  VER_BAK DEC_01.

c  3. The convergence index (HOWCLOSE), and how close the current 
c  density is to the M.L.E. of the density (MAX(DXI) - NSUB), are
c  removed. DXI is left in only as an argument to BIGNPEM, and DORIG is 
c  left in only to be read and written to the density file so that the 
c  format of that file does not have to be changed.

c  4. This module will no longer contain the modules idfix5*.f, 


c  idcy_53*.f, idcy_63*.f, and shift2.f. These modules will be
c  compiled and linked separately.


c  5. The I/O to input ierrmod will be suppressed since there can be
c  no user interaction with this program. For now, ierrmod will be
c  hardcoded = 1.

c  6. Note that the PARAMETER statement in Subroutine emint has the
c  following line to set the values for MAXACTem and MAXSUBem:
c      parameter (MAXSUBem=999,MAXACTem=250000)
c  These values can be reset as needed.


c-----------------------------------------------------------------------

c bignpag.f                                                     07-05-2002
C BIGNPAG with multiplicative GAMMA and additive LAMBDA options
c this is the bignpag version with time lags and initial conditions
c This version is consistent with BIGNPEM version 5 (see  npbig5.f below)
c Changes in July 2002 involve replacing the LINPACK-based linear equation solver
c in subnroutine emint with the Cholesky sovler from LAPACK.
c changes in July 01 include new, more efficient Hessian construcution routine
c This version is dimensioned for a MAXSUBem (maximum number of subjects)
C of 400 - this is easily changed by changing the MAXSUBem PARAMETER
C statement in subroutine EMINT.  Also, the max number of grid points is
c 250,000 - this is also adjustable by the PARAMETER statement for
C MAXACTem in emint.


C July 05 2002 changes
c repalced calls to Linpacksymmetric indefinite linear equation solver
c dsifa and dsisl with lapack cholesky solver DPOTRF and DPOTRS
c begin 01/02/02 changes

c saved density results in denstor(*,4)  for best of base, plus,

c and minus case, so proper statistics would be generated based
c on the best case.
c begin 12/31/01 changes
c renamed iteration logfile itlog to ILOGxxxx, where xxxx is current run
c number from 'extnum' file.
c end 12/31/01 changes
c begin 12/16/01 changes
c added error model selection capability - currently four choices:
c 1 - use error polynomial as given in input file,
c 2 - optimally scale error polynomial by multiplicative factor gamma

c 3 - optimally find an additive error lambda, such that error^2 =

c     lambda^2 + polynomial^2
c 4 - find optimal flat weighting - this is equivalent to using a
c     polynomial with only a constatn term, and finding the optimal
c     value of that constant with option 1)
c end 12/16/01 changes
c begin 7/04/01 changes
c ROUTINE STAT change to STAZ to avoid conflict with system routine
c Also, first '0.0' agrument changed in CALL OUTPUT(0.0,...' to 0.0D0
c to agree with argument typing
c end 7/04/01 changes
c  NPBIGADAPT
c  4/06/2000 - April 6, 2000 - New adaptive grid version
 
c  This version is built on top of the previous fixed
c  grid version of BIGNPEM and represents a major algorithmic
c  change. NPGIGADAPT is the adaptive grid version of BIGNPEM -
c  It is designed to work with exactly the same driver file
c  npemdriv.f and input file npembig2.inp as previous versions
c  of BIGNPEM.  Results in terms of the log likelihood of the

c  final density produced are typically much better than
c  previous versions, which used a fixed grid, even when the

c  adaptive version is started from a small grid and the previous
c  fixed grid version is started from a very large grid.

c  Thus the NPBIGADAPT version can be run on a workstation or PC
c  in moderate amounts of time and with relatively low
c  memory requirements and still produce results equivalent to or
c  better than a large fixed grid version run for many processor
c  hours on a parallel supercomputer.
 
 
c  This version works on the following logic
 
c  Step 0 (intialization) ;
c  The data file npembig2.inp is read and the initial grid of
c  size NGRID (as specified by the INDPTS variable in the
c  iput file) is generated using a low-discrepancy (or
c  'quasirandom') Faure generator.
c  Note that we require that all points fit in memory, so if
c  NGRID is larger than the memory limit MAXACT in the driver
c  program npemdriv.f, we reset NGRID to
c  MAXACT.  Also, the old termination criteria JSTOP and
c  TOL in npembig2.inp are now still read but ignored - they
c  are replaced with hardwired criteria (see below).  However,
c  the MAXCYCLE criterion is retained but changes its meaning.
c  It now represents a limit on the number of grid condensation -

c  expansion cycles, not the limit on the number of cycles in
c  the EM algorithm (the EM algorithm is no longer used, having
c  been replaced by the more efficient interior point algorithm
c  developed by Jim Burke at University of Washington).

 
c  Step 1; (solve ODEs on current grid)

c  solve all the ordinary differential equations defining the
c  PK model on the current set of grid points and compute
c  the corresponding likeihoods for each combination of

c  subject and grid point to produce the likelihood matrix
c  PYJGX(J,IG)  (J=subject index, IG = grid point index)
 
c  Step 2 (grid condensation)
 
c  Solve the maximum likelihood EM problem  defined by the likelihood
c  matrix PYJGX corresponding to the current grid
c  via the interior point method implemented in subroutine emint.
c  On the first cycle this condenses the number of active gridpoints
c  NACTVE from the  starting value of NGRID to approximately
c  NSUB, the number of subjects.  On subsequent cycles, this step

c  will condense the current set of NACTVE (usually now much
c  smaller than NGRID but still several times larger than NSUB)
c  points to NSUB points.

 
c  Step 3 (grid expansion).
c  Each of the NSUB active grid points is perturbed in each
c  parameter with a +EPS and -EPS percentage perturbation while
c  holding the other parameters fixed.  Thus if there
c  are NPARAM parameters, there are 2*NPARAM perturbed points.
c  These checked to see if they lie within the original parameter
c  bounds specified in npembig2.inp.  Those perturbed grid points
c  that lie within the bound are added to the NSUB active grid points
c  to form a new set of approximately NACTVE = NSUB*(1+2*NPARAM).

c  Note that in general this is much smaller than the original set of
c  NGRID points.  EPS is initially set to 20% of the range of
c  the corresponding parameter.  As the algorithm proceeds, the
c  grid resolution will be lowered in stages to 0.01% of the
c  range.
 
c  Step 4 (termination check)
c  If the LOG-LIKELIHOOD of the current grid is at least a
c  tolerance TOL (hardwired in the current verison to 0.001), we
c  maintain EPS at the current value and
c  continue.  If the LOG-LIKELIHOOD does not improve by at least TOL,
c  we cut EPS in half, generate a new grid, and continue.

c  The algorithm terminates when either
c  a) the designated maximum number of cycles MAXCYC in npembig2.inp
c     is reached, or
c  b) EPS reaches the (currently hardwired) limit of 0.01% AND the
c     improvement of the current cycle over the previous cycle

c     in log-likelihood is less that the (currently hardwired)
c     tolerance of 0.001
c

c
c-----------comments below refer to previous fixed grid versions ----
c  npbig5.f							2-17-00
 
c  npbig5 is the same as npbig4, except:
 
c  FORMAT 88 IN MAIN AND PRNTOP IS CHANGED TO INDICATE THAT A
C  CONVERGENCE INDEX OF .LE. 1 = CONVERGENCE (NOT JUST AN INDEX = 1).
 
C-----------------------------------------------------------------------
 
c  npbig4.f							1-23-00
 
c  npbig4 is exactly the same as npbig3. The only change is that it is
c  linked with new modules on the supercomputer. idfix5d.f is replaced
c  by idfix5e.f; idcy_53d.f is replaced by idcy_53e.f; and idcy_63d.f is
c  replaced by idcy_63e.f. These 3 new modules have updated code to
c  allow initial conditions of the amounts in the compartments to be set
c  = paramater values, rather than always fixed = 0.0.
 
c  SUBROUTINE SYMBOL in the Fortran model file (see, e.g.,

c  INITCOND.FOR) now contains an additional COMMON/INITCOND/IC, and the
c  user sets IC(I) = J for each compartment, I, which will have its
c  initial amount set = value of parameter J. This info is passed to
c  the above 3 id modules. Also, the dimension of P in OUTPUT and
c  DIFFEQ is changed to max_ODE_params.
 
c  Note that the 3 new id modules above have a lot of code
c  simplification (see notes in their code). In particular, the
c  square root transformations are no longer done --> the results
c  for this program will differ possibly a little from previous
c  results.
 
c-----------------------------------------------------------------------
 
c  npbig3.f							1-19-00
 
c  npbig3 is exactly the same as npbig2. The only change is that it is
c  linked with new modules on the supercomputer. idfix5c.f is replaced


c  by idfix5d.f; idcy_53c.f is replaced by idcy_53d.f; and idcy_63c.f is
c  replaced by idcy_63d.f. These 3 new modules have updated code to


c  correctly allow the IDIFF = 0 option, which bypasses the calling
c  of USERANAL (and its calls to DIFFEQ). Instead the value(s) for
c  the output(s) will be coded explicitly into SUBROUTINE OUTPUT. Also,

c  SUBROUTINE OUTPUT has an additional argument, the time at which the
c  output value(s) is(are) desired.
 
c-----------------------------------------------------------------------

 
c  npbig2.f							11-8-99
 
c  npbig2 is the same as npbig1 except for a change in COMMON/CNST
c  in subroutine FILRED. NTLAG is now added.

 
c  This change, and others, are required for all modules in this
c  program, which allow time lags.
 
c-----------------------------------------------------------------------
 
c  npbig1.f							10-12-99
 
c  npbig1 has the following changes from m2_19aca.f:
 
C  1. THE GRID POINT SELECTION PROCEDURE IS CHANGED. SUBROUTINE GETCOF
C  IS REMOVED, AND SUBROUTINE CALGRD IS REPLACED BY A NEW CALGRD, ALONG
C  WITH ROUTINES INFAUR AND GOFAUR. NOTE THAT ALL REFERENCES TO THE
C  NUMBER THEORETIC INTEGRATION SCHEME ARE REMOVED.
 
C  2. THE CONVERGENCE CRITERION IS CHANGED TO HAVE AN ABSOLUTE VALUE
C  SIGN AROUND (SLPYJ-PRESLP). THIS SHOULDN'T BE NECESSARY SINCE THE
C  LOG-LIK IS MONOTONICALLY INCREASING, IN THEORY. THE ABS. VALUE
C  MEANS THAT IF A STRANGE NUMERICAL "GLITCH" OCCURS WHICH RESULTS
C  IN THE LOG-LIK DECREASING FROM 1 CYCLE TO THE NEXT, THE PROGRAM WON'T
C  AUTOMATICALLY HAVE SATISFIED THE CONVERGEGENCE CRITERION. OF COURSE,
C  IT'S POSSIBLE THAT IN SUCH A CASE, WE WOULD WANT THE PROGRAM TO STOP,
C  REGARDLESS.
 
C  3. SUBROUTINE STAT NO LONGER ALWAYS USES NINT = 100. INSTEAD, NINT
C  WILL BE THE MAX(100,2*NSUB). THE REASON IS THAT THE MARGINAL SCALED
C  INFO HAS A DENOMINATOR OF LN(NINT/NSUB) IN THE CALCULATION, WHICH
C  RESULTS IN A DIVIDE BY 0 ERROR IF NSUB = 100 (AND RESULTS IN
C  NEGATIVE SCALED INFO (WHICH IS SUPPOSED TO BE BETWEEN 0 AND 100),

C  WHEN NSUB > 100. BY MAKING NINT .GE. 2*NSUB, THIS PROBLEM WILL
C  DISAPPEAR.
 
C  4. AN ADDITIONAL STATISTIC IS OUTPUT EACH CYCLE, HOWCLOSE. IT
C  IS A MEASURE OF HOW CLOSE TO CONVERGENCE THE PROGRAM IS. THIS WILL BE
C  IN FORMAT 88, JUST BELOW THE FORMAT 8'S IN MAIN AND PRNTOP. HOWCLOSE
C  WILL BE ADDED AS AN ARGUMENT TO PRNTOP.
 

C  5. A CHANGE IS MADE TO FORMAT 1652 (IN MAIN AND SUBRES), AND THE
C  VALUES SK AND KU ARE SET TO WHEN ICOVL0 = 1 (WHICH OCCURS WHEN AT
C  LEAST ONE PARAMETER HAS VARIANCE NUMERICALLY .LE. 0). NOW KU AND SK

C  WILL BE WRITTEN OUT AS -99999999, RATHER THAN 1.D30. IT USED TO BE
C  THAT 1.D30 WOULD PRINT OUT AS *'S, BUT NOW WITH THE G FORMAT IT
C  PRINTS OUT AS A LEGITIMATE NUMBER, AND -99999999 IS A BETTER WAY TO
C  EXPRESS THAT THESE VALUES DO NOT EXIST.
 
c-----------------------------------------------------------------------
 
c  m2_19aca.f							5-23-99
 
c  m2_19aca has all the changes that m2_20cal thru m2_20bca made to
c  m2_19cal, except that the assay s.d.'s are still functions of

c  observed values, rather than predicted values.

 
c  The changes are:
 
 
c  1. RS(500,14) DIMENSIONS ARE CHANGED TO RS(500,34), TO ALLOW UP TO
c  30 USER-SUPPLIED COVARIATES.
 
C  2. A BUG IN THE CALCULATION OF ENTROPY IS CORRECTED. THE ENTROPY
C  ENTROPY CALCULATION IS MOVED TO BE AFTER SUM=SUM/DL2 STATEMENT.

 
C  3. FORMAT 6543 IN PRNTOP IS CHANGED TO BE SAME AS IN MAIN.
 
c-----------------------------------------------------------------------
 
 
c  m2_19cal.f							7-28-98
 
c  m2_19cal.f has the following changes from m2_18cal.f:
 

c  It corrects a "bug" in m2_18cal.f. m2_18cal.f never allowed for
c  an observed value to be missing. With multiple outputs, not all
c  output equations will necessarily have observed levels at all
c  observation times. An observed level which is "missing" has the
c  value -99 in its entry. m2_18cal.f just treated the -99 as a
c  regular value.
 
c  The only functional changes involve MISVAL in and after loop 140.
 
c  The only other module which needs to be changed is idfixed4.f -->
c  idfixed5.f (idcy_53.f and idcy_63.f just calculate predicted values
c  at all the observation times. This can be done for all output eqs.
c  for all the observation times, regardless of which observed values
c  are missing).
 
c-----------------------------------------------------------------------
 
c  m2_18cal.f							6-28-98
 
c  m2_18cal.f has the following changes from m2_17cal.f:
 
C  1. IT ALLOWS MULTIPLE OUTPUTS. THERE WILL BE NUMEQT OUTPUT EQUATIONS.

C  NUMEQT IS PASSED TO THIS ROUTINE (BIGNPEM) BY npemdriv.f (MADE BY
C  M2_18.FOR) IN THE ARGUMENT LIST. SEVERAL ARRAYS HAVE AN EXTRA
C  DIMENSION TO IDENTIFY THE OUTPUT EQUATION, INCLUDING YPREDPOP AND
C  YPREDBAY WHICH ARE ARGUMENTS FROM npemdriv.f.
C  YPREDBAY WHICH ARE ARGUMENTS FROM npemdriv.f.

 
C  THE INPUT FILE HAS BEEN RENAME npembig2.inp SINCE NEW INFORMATION
C  IS INCLUDED.

 
C  SUBROUTINE FILRED HAS CHANGES TO ACCOMODATE MULTIPLE OUTPUTS.
 
C  THIS MODULE WILL BE LINKED WITH OTHER CHANGED MODULES, idfixed4.f,
C  idcy_53.f, AND idcy_63.f, ALONG WITH vodtot.f (WHICH IS UNCHANGED).
 

C  2. SOME COMMENTS REGARDING WHAT'S IN THE INPUT FILE, npembig2.inp,
C  HAVE BEEN IMPROVED OR CORRECTED.
 
C  3. POPULATION VALUES FOR THE NUMEQT SETS OF C'S NOW WRITTEN TO FILE
C  25 USING FORMAT 162, INSTEAD OF 161.
 
C  4. INFIL NO LONGER READ FROM INPUT FILE npembig2.inp (IT WAS NEVER
C  USED).
 
C  5. CYCLE NO. IS ALSO WRITTEN OUT IN FORMAT 8888.
 
C-----------------------------------------------------------------------

 
c  m2_17cal.f							5-1-98
 
c  m2_17cal.f has the following changes from m2_16cal.f:

 

C  1. Input file fil01.inp IS renamed npembig1.inp. There are no
c  changes to this file, but now the name is more suggestive of what
c  it is.
 
C  2. FORMAT 161 REPLACES * FORMAT WHEN WRITING OUT THE C'S TO A FILE,
C  TO AVOID POSSIBILITY THAT PART OF LINE WRITTEN WILL BE TRUNCATED IF
C  IT EXTENDS PAST COLUMN 72.

 
c-----------------------------------------------------------------------
 
c  m2_16cal.f						2-12-98

 
c  m2_16cal.f has the following changes from m2_15cal.f:
 
c  1. m2_16cal.f has the same format changes from m2_15cal.f M2_16.FOR
c  has from M2_15.FOR, namely:
 
C  ALL F AND E FORMATS WILL BE CHANGED TO G FORMATS. SINCE THE G

C  FORMAT REQUIRES AT LEAST 6 SPACES IN ADDITION TO THE DECIMAL SPACES +
C  THE SPACE FOR THE DECIMAL POINT ITSELF (TO REPRESENT ALL POSSIBLE
C  NUMBERS, INCLUDING THE NEGATIVE SIGN IF THERE IS A NEGATIVE NO.),
C  THE FORMAT WILL BE Gw.d, WHERE w MUST BE AT LEAST 6 BIGGER THAN d.
 
C  THEREFORE, THE FOLLOWING LOGIC WILL BE USED TO CONVERT THE FORMATS.
C  GIVEN AN F OR E FORMAT w.d, THE REPLACING G FORMAT WILL BE ww.d,
C  WHERE ww = MAX(w,d+6). IF ANY ACCOMPANYING FORMATS ARE DEPENDENT ON
C  THE REPLACED FORMAT, IT WILL BE CHANGED ACCORDINGLY. FOR EXAMPLE,
C  FORMAT 5104 IS USED TO PLACE THE PARAMETER NAMES ABOVE MEANS,
C  MEDIANS, ETC. USING FORMAT 5103. PREVIOUSLY, THE FORMATS WERE:
 
C  5104   FORMAT(5X,20(A11,5X))
C  5103   FORMAT(1X,20(F11.6,5X))
 
C  IN THIS PROGRAM, SINCE 11.6 MUST BE CHANGED TO 12.6 FOR THE G FORMAT
C  (I.E., ww = MAX(11,6+6) = 12), AND SINCE 5X HAS BEEN CHANGED TO 1X,
C  FORMAT 5104 MUST BE CHANGED TO BE COMPATIBLE.
 
C  5104   FORMAT(5X,20(A11,2X))
C  5103   FORMAT(1X,20(G12.6,1X))
 
C  A COUPLE OF OTHER EXCEPTIONS FROM THE ABOVE RULE HAVE BEEN MADE FOR
C  COSMETIC REASONS, INCLUDING PRINTING OUT % VALUES (AND FORMATS
C  2049, 2051, 2052) IN MAIN.
 
 
c  2. The order of the arguments in COMMON/DESCR has been changed. For
c  some reason, the fortran compiler likes to see Real*8 variables
c  preceed integer variables. Otherwise, warning messages appear.
 
c-----------------------------------------------------------------------
 
c  m2_15cal.f							1-22-98


 
c  m2_15cal has the following changes from m2_14cal:

 
c  1. AGE, ISEX, HEIGHT, and IETHFLG are input in subroutine FILRED,

c  for each subject, and these values are now passed to subroutines
c  DIFFEQ and OUTPUT, part of the fortran file created by the
c  boxes-type program (or made manually). This fortran file is part
c  of npemdriv.f, uploaded by the user after running M2_15.EXE. These
c  values are passed via COMMON/DESCR.
c  The new boxes program is BOXNEW2.PAS, changed from BOXESNEW.PAS.
 
c  2. In subroutine FILRED, since RS must store all the psuedo "rates"
c  (i.e., all the covariate info input in the dosage regimen), RS

c  dimensions have been increased from (500,8) to (500,14). Since
c  RS is passed via COMMON/OBSER, modules idfixed3.f (changed from
c  idfixed2.f), idcy_52.f (changed from idcy_51.f), and idcy_62.f
c  (changed from idcy_61.f) must be changed correspondingly. Also,
c  these modules have changes related to a "bug" correction regarding
c  IDIFF.
 
c-----------------------------------------------------------------------
 
c  m2_14cal.f						   	11-6-97
 
c  m2_14cal has the following changes from m2_13cal.
 
c  1. This module is now SUBROUTINE BIGNPEM, called by npemdriv.f, which
c  is created by the PC preparation program, M2_14.FOR. This change was
c  made so a dynamic allocation of dimensions can be made, based on
c  how many subjects, random parameters, and grid points the user
c  has selected.
 
c  npemdriv.f, the "MAIN" module, HAS A PARAMETER STATEMENT WHICH

c  DEFINES THE PARAMETERS WHICH ESTABLISH THE DIMENSIONS IN THE VARIABLY
c  DIMENSIONED ARRAYS. IT THEN HAS THE STATEMENT:
C  CALL BIGNPEM( ...) , WHERE ALL VARIABLY DIMENSIONED ARRAYS (AND THE
C  VARIABLE PARAMETER DIMENSIONS) ARE PASSED IN THE ARGUMENT LIST.
 
C  NOTE THAT ARRAYS PYJGX, EXX, YPREDPOP, AND YPREDBAY NOW HAVE MAXSUB
C  AS A VARIABLE DIMENSION. ALSO, PYJGX'S 2ND DIMENSION, AND PYJGXX'S
C  SINGLE DIMENSION IN SUBROUTINE SUBRES HAVE BEEN CHANGED FROM MAXGRD
C  TO MAXACT (WHICH THEY WERE SUPPOSED TO HAVE BEEN ANYWAY).

 
c  2. Some unneeded format statements are eliminated.
 
c  3. ATOL(3) TO ATOL(max_ODE_comps), WHICH IT SHOULD HAVE BEEN ALL ALONG.
 
c  4. THE VALUES FOR RTOL AND ATOL() ARE PRINTED TO THE OUTPUT FILE.
 
c  5. THE ORIGINAL NO. OF GRID POINTS, IN ADDITION TO THE CURRENTLY
C  ACTIVE NO. OF GRID POINTS, IS PRINTED OUT EACH CYCLE (INCLUDING
C  PARTIAL CYCLES PRINTED BY SUBROUTINE PRNTOP, AND THE MAP-BAYESIAN
C  CYCLE FOR EACH SUBJECT IN SUBROUTINE SUBRES).
 
c  6. Format 2314, if applicable, is written to the output file, in
c  addition to the screen.
 
c-----------------------------------------------------------------------
 
c  m2_13cal.f						   	8-31-97
 

c  m2_13cal is the same as m2_12cal except that the maximum no. of grid
c  points, MAXGRD, is increased from 16*80021 to 100*80021, the maximum

c  no. of subjects is increased  to 200, from 100, and the maximum no.
c  of dimensions is increased to 10 from 8.
 
c  Note that there is a limit to how big the program can be in more
c  than one way. In particular, the maximum number of double precision
c  entries in a matrix = 268,435,455. Since CORDEN and CORHOLD have
c  MAXGRD*(MAXDIM+1) entries, if MAXDIM = 20, since MAXGRD = MAX*80021
c  this --> MAX*80021*21 .LE. 268,435,455 --> MAX .LE. 159.74... But,
c  even though MAXGRD = 159*80021 will compile OK, the program is
c  killed or terminated ('segmentation fault') by the sun system when
c  execution is attempted. For now, the combination of parameter values
c  which is executable is MAXGRD = 100*80021, MAXACT = 100000 (see
c  below), MAXDIM = 10, and PYJGX dimensions set = (200,MAXGRD) in MAIN
c  and subroutine SUBRES.
 
c  This can be done by using a creating a new parameter, MAXACT = the
c  maximum no. of grid points which can be stored into PYJGX. MAXACT
c  will be set arbitrarily to 100000, so PYJGX has dimensions of
c  just (200,10000).
c  Then, in loop 800, when PYJGX is filled with values, at most 100000
c  grid points can be put into PYJGX. The rest, if there others, will
c  simply have to have their P(YJ|X)'s recalculated each cycle until
c  the no. of active grid points .LE. MAXACT, at which time all the
c  P(YJ|X)'s can be stored into PYJGX.
 
c  Note that, with the exception of PYJGXX the other matrices which had


c  MAXGRD in their dimensions (i.e., WORK, SPXGYJ, DXI, CORDEN, CORHOLD,
c  and DENSTOR) will still have dimensions based on MAXGRD. Also, a new
c  matrix, WORKK(MAXGRD) will be added.

 
c  The most significant changes in the code are in loop 800.
 


c-----------------------------------------------------------------------
 

c  m2_12cal.f						    8-27-97
 
c  m2_12cal is the same as m2_11cal, except that the no. of subjects
c  allowed is reduced from 999 to 100, the no. of dimensions allowed
c  is reduced from 20 to 8, and the no. of grid points allowed is
c  increased from 2*80021 to 16*80021.

 
c  In particular, PYJGX(999,MAXGRD) IS CHANGED TO PYJGX(100,MAXGRD) in
c  main and subroutine subres. Also, some other 999's in dimensions are
c  changed to 100. Also, MAXDIM is changed from 20 to 8 in the
c  PARAMETER statement, but no dimensions are changed from 20 to 8.
c  Finally, MAXGRD is changed from 2*80021 to 16*80021 in the
c  PARAMETER statement.

 
c-----------------------------------------------------------------------

 
c  m2_11cal.f						    8-18-97
 
C  m2_11cal.f HAS THE FOLLOWING CHANGES FROM m2_10cal.f
 
C  1. THE NO. OF GRID POINTS MAY NOW BE > 80021. IN FACT, THE USER
C  MAY SELECT UP TO 3 (FOR NOW) MULTIPLES OF 80021 POINTS. EACH MULTIPLE
C  OF 80021 GRID POINTS WILL BE PUT INTO A 'SLICE' OF THE GRID SPACE,
C  DEFINED BY THE 1ST PARAMETER'S BOUNDARIES.
 
C  A RELATED CHANGE IS THAT PREVIOUSLY INDPTS HAD TO
C  BE INSIDE [1,6], WITH NGRID DEFINED AS BEFORE, BUT NOW IT CAN ALSO
C  BE [101,...], WHERE NGRID = 80021*(INDPTS-100).
 
C  2. PYJGX, EXX, YPREDPOP,
c  AND YPREDBAY all have a their dimension of 19 changed to 999. 999
c  was the desired dimension, but space limitations previously limited
c  it to be 19. Now, the almaak machine allows more subjects. In fact,
c  the current limitation is that the no. of entries in any matrix be
c  less than or equal to 2**28 - 1 = 268,435,455. So, PYJGX can be
c  dimensioned as high as (999,268704). FOR NOW, THEREFORE, WE WILL

C  SET MAXGRD = 3*80021 = 240063 < 268704. NO. IT WAS TOO BIG (WHEN I
C  TRIED TO RUN m2_11cal.exe, almaak responded with 'killed'), BUT
C  MAXGRD = 2*80021 WAS O.K.
 
C  3. DEFAULT CONVERGENCE TOLERANCE (IF JSTOP=1) IS CHANGED FROM .00001
C  TO .000001.
 
C  4. THE MAXIMUM NO. OF PARAMETERS IS INCREASED FROM 7 TO 20. THIS
C  AFFECTS MOSTLY THE DIMENSION STATEMENTS OF THE AFFECTED ROUTINES.
C  BUT ALSO SOME FORMATS ARE CHANGED.
 
C  NOTE THAT THIS CHANGE REQUIRES CORRESPONDING DIMENSION CHANGES IN
C  idfixed.f --> idfixed2.f, idcy_5.f --> idcy_51.f, and
c  idcy_6.f --> idcy_61.f.
 
C  5. AT THE END OF THE ANALYSIS, ONE LAST CONDENSING OF POINTS IS
C  DONE. THIS IS PARTICULARLY IMPORTANT IF THE PROGRAM 'ACCELERATED'
C  MANY TIMES WITH OUT CONDENSING POINTS NEAR THE END OF THE RUN
C  (I.E., THE PROGRAM CANNOT THROW OUT POINTS IF IT IS CONSTANTLY
C  'ACCELERATING' -- SEE REASON IN CODE).
 
c-----------------------------------------------------------------------
 
c  m2_10calc.f = m2_9calc.f					6-11-97
 
c  m2_9calc.f has the following change from m2_8calc.f.
 
c  The assay noise coefficients, written to fil01.inp by M2_9.FOR,
c  have been written on two lines, instead of one. This prevents the
c  possibility that C3, if written with a "D-xxx" format at the end
c  (e.g. D-003) will be too long for the line, and therefore not
c  properly read by this program.
 
c-----------------------------------------------------------------------

 
c  m2_8calc.f						    6-5-97
 
c  m2_8calc.f has the following changes from m2_7calc.f.
 
C 1.  NAME CHANGES:
 
C   user_4.f IS CHANGED TO mod01.f (this is for info only. user_4.f
C   is never used explicitly in this code).
 
C   m2__calc.inp IS CHANGED TO fil01.inp.
 

 
C  2. NEW INFO IS INPUT BY USER (AND IS INCLUDED IN fil01.inp).
 

C   IDELTA, T_END, MIC, AND MEAN/MEDIAN/MODE SELECTION. THESE VALUES
C   WILL BE USED TO CALCULATE PREDICTED VALUES FOR EACH
C   SUBJECT AT TIMES 0, IDELTA, 2*IDELTA, ..., T-END, ALONG WITH AUC'S
C   (AREAS UNDER CURVES) AND AUC/MIC'S. NOTE THAT IDELTA = 2, 4, 6, OR
C   12 MINUTES, AND T_END IS IN HOURS.
 
C   NOTE THAT THE NSUB TABLES OF PREDICTED VALUES WILL BE PUT INTO
C   THE FILE PRTBxxxx, WHERE xxxx IS THE SAME AS xxxx FOR THE OUTxxxx
C   AND DENxxxx OUTPUT FILES. THE AUC'S AND AUC/MIC'S WILL ACCOMPANY


C   EACH SUBJECT'S BAYESIAN POSTERIOR INFO (AT THE END OF THE RUN).
 
c       PRTBxxxx will be used as input to an effects model. i.e.,
c       knowing the concentration of a drug at certain times (as
c       predicted by a previous run's analysis) will be compared to
c       corresponding info on some 'effect', whose values will be known
c       at a given set of times ... so that the relationship between
c       concentration and the 'effect' can be established.
 

 
C  Details:
 
c 1. using subject 1's bayesian posterior density, use either the means,
c    medians, or modes (as selected by user);
 
c    calculate:
 
c    time    ypred
c ----------------------

c     0       ___
c    delta    ---
c  2*delta    ___
c	.

c	.
c	.
c   t-end     ___
 
c Notes: this ypred column of values would be exactly the same as
c       YPREDBAY for the means, medians or modes (see below), except
c       that the 'observed' times are as shown (and therefore don't
c	necessarily match the observed times in the patient's data
c	file).
c
c  Calculate total AUC = area under the observed value curve (horizontal
c	axis is time) = delta * (y1/2 + sum (ypred) = ylast/2),
c       approximately, where y1 = y(0), ylast = y(t_end), and sum is
c 	the sum of the y's for indices between 1 and last.
 
c   Also, calculate AUC for each 24 hour period (up to t-end).
 
c   Also, calculate AUC/MIC, total
c         and AUC/MIC for each 24 hour period.
 
 
c  2., ..., nsub = same as 1. above, except for subjects 2, ..., nsub.
 
c  Note that comp.sh (in the working directory of the supercomputer)
c  must be changed (m2_7calc. f-->m2_8calc.f and user_4 --> mod01). And
c  new module, idcy_6.f must be included (in addition to idcy_5.f). It
c  it contains IDCALCYY/FUNC3/EVAL3, which are similar to
c  idcy_5's IDCALCY/FUNC2/EVAL2, except concentrations are calculated
c  at above specified times, rather than at observation times for each
c  subject.
c  Note that m2_7calc.sh must be changed to m2_8calc.sh.
 
c-----------------------------------------------------------------------
 
c  m2_7calc.f							2-11-97
 
c  m2_7calc.f has the following changes from m2_6calc.f.
 
c  1. The density file now contains, at the end, PYJGX(JSUB,IG),
c     IG=1,NACTVE, JSUB=1,NSUB. It also includes YPREDPOP, YPREDBAY,
c     and EXX (see no. 4 below).
 
c  2. m2__calc.inp includes extra info, which previously had been
c     obtained via COMMON/TOMAIN (note that subroutine SYMBOL has
c     changed since now it's a part of user_4.f (created by a 'BOXES'-
c     type program), instead of iduser_4.f. m2__calc.inp also includes
c     IRAN, THE vector which tells which of the parameters are random
c     and which are fixed.
 

c  3. The call to IDPC no longer includes NVAR as an argument. The no.
c     of parameter values passed is NP = NVAR+NOFIX, and NP is obtained
c     by IDPC via COMMON/CNST from SUBROUTINE SYMBOL.
 
c  4. The write statements to files 24 and 37 have been removed from
c     subroutine PRNTOP (they should have been removed before, since
c     no files 24 and 37 are needed on the mainframe).
 

c  5. Extra info will be included in the density file, which will be
c     read by the PC program, M2_7.FOR. The extra info includes
c     matrices PYJGX, YPREDPOP, YPREDBAY, AND EXX.
c     Since SUBROUTINE IDCALCY (of module IDCY_5.FOR) can no longer be
c     linked with M2_7.FOR (since it calls SUBROUTINE OUTPUT - of
c     module user_4.f, which changes analysis - to - analysis, and
c     therefore cannot be linked to the user's PC program), all the
c     code which uses IDCALCY must be included in this program (and so
c     IDCY_5.FOR = idcy_5.f will be linked with this program). Then,

c     in M2_7.FOR (SUBROUTINE PREVRUN), YPREDPOP, YPREDBAY, and EXX will
c     be read in, and options 6 and 7 will be available.
 
c     Note that YPREDPOP(JSUB,IOBS,ICEN) = the predicted value for Y for
c     subject JSUB, for observation IOBS, for ICEN = 1 (means),
c     2 (medians), AND 3 (modes), where the means, medians, and modes
c     are from the final cycle population density.
 
c     Also, YPREDBAY(JSUB,IOBS,ICEN) = the predicted value for Y for
c     subject JSUB, for observation IOBS, for ICEN = 1 (means),
c     2 (medians), AND 3 (modes), where the means, medians, and modes
c     are from subject's JSUB Bayesian posterior density (calculated by
c     SUBROUTINE SUBRES - now included in this program, in addition to
c     M2_7.FOR.

 
c     Also note that the output density file is now created only at the
c     end of the run, not after each cycle (the only reason it was
c     written at the end of each cycle previously was to provide
c     'protection' in case the program bombed before finishing -- the
c     program could then pick up where it left off with an apriori
c     density from the last completed cycle, rather than starting over;
c     but downloading this density from the mainframe, incorporating it
c     into another input file, and restarting this program is probably
c     more trouble than it's worth, given the low chance that the
c     program will bomb).

 
 
c-----------------------------------------------------------------------
 
c  m2_6calc.f							1-15-97
 
C  M2_6.FOR (PC PROGRAM) + m2_6calc.f (sun/mtha program) MAKE THE
C  SAME CHANGES TO M2_5.FOR + m2_5calc.f THAT MXEM2N60.FOR MADE TO
C  MXEM2N59.FOR. THE DETAILS ARE:
 
C  THE PROGRAM NOW ALLOWS EACH PATIENT FILE TO INCLUDE ITS OWN
C  ASSSAY NOISE COEFFICIENTS (BUT PATIENT FILES WITHOUT THEIR OWN
C  INDIVIDUAL COEFFICIENTS CAN STILL BE ANALYZED).
 
C  FOR EACH PATIENT, THE ASSAY COEFFICIENTS TO BE USED WILL BE EITHER

C  THE ONES ALREADY IN ITS FILE (IF ANY), THE DEFAULT (POPULATION)
C  VALUES, OR A SPECIFIC SET FOR THAT PATIENT.
 
C  AT THE BEGINNING OF THE RUN, THE C'S TO BE USED FOR EACH PATIENT WILL
C  BE WRITTEN AT THE END OF HIS/HER ADAPT-LIKE DATA FILE (IF THEY'RE NOT
C  ALREADY THERE).
 

C  MODULE CONVRTG.FOR IS CHANGED TO CONVRTH.FOR (THIS MODULE CONTAINS
C  SUBROUTINE CONVRT, WHICH CONVERTS USC*PACK FILES TO ADAPT-LIKE
C  FILES. CONVRT IS CHANGED TO READ THE C'S IN A USC*PACK FILE, IF
C  IF THEY'RE THERE, AND WRITE THEM INTO AN ADAPT-FILE.
 
C  NOTE THAT THE 'POPULATION' ASSAY NOISE COEFFICIENTS WILL BE CALLED
C  [C0P,C1P,C2P,C3P]. [C0,C1,C2,C3] WILL BE USED FOR THE INDIVIDUAL
C  VALUES FOR EACH PATIENT, IN TURN.
 
C  NOTE THAT SUBROUTINE SCATPLOT MUST BE CHANGED SO THAT IT CAN

C  INPUT THE ASSAY COEFFICIENTS, [C0,C1,C2,C3], FOR EACH SUBJECT
C  IF JSUB=0. NEW MATRIX COEFF STORES THESE VALUES AND IS PASSED VIA
C  COMMON/TOSCAT TO SCATPLT9.FOR (CHANGED FROM SCATPLT8.FOR).
 
C-----------------------------------------------------------------------
 
C  m2_5calc.f							11-14-96
 
C  M2_5.FOR (PC PROGRAM) + m2_5calc.f (sun/mtha program) ARE EQUIVALENT

C  TO MXEM2_5.FOR. M2_5.FOR DOES THE PREPARATION PART OF THE PROGRAM,
C  AS WELL AS THE EXAMINATION OF THE OUTPUT FILES FROM THE ANALYSIS. THE
C  ANALYSIS IS DONE ON THE SUN BY m2_5calc.f.

 
C  IN THE PREPARATION PHASE, M2_5.FOR DOES ALL THE USER I/O AND
C  CONCATENATING OF PATIENT FILES. IT OUTPUTS ONE FILE, m2__calc.inp,
C  WHICH CONTAINS THE INPUT INFO FOR THIS RUN, AS WELL AS THE
C  CONCATENATED PATIENT DATA FILES FOR THIS RUN.
 
C  m2_5calc.f CAN THEN BE RUN ON A SUPERCOMPUTER AFTER UPLOADING
C  m2__calc.inp TO IT.
 
C  THE SEPARATION OF MX3M2_5.FOR INTO THE M2_5.FOR AND m2_5calc.f IS AS
C  FOLLOWS:
 

C  M2_5.FOR <-- I/O PART OF MAIN + SUBROUTINES FILRED, STACK, VERIF1,
C		CHANGE, CALMAR, THREED1, FORMLC, CONVERGE, PLTCON,
C		MINMAX, PLOTS, SUBRES, GETOUT, PREVRUN, PRNLAST, SEEDIR,

C		CALCSER, NOTINT, STAT, DELAY, AND EXTREME, ... PLUS
C               MODULES DATABLK1 IDUSER_4 CONVRTG GHCSEL IDCY_4 SCATPLT8
C               AND VODTOT.
 
C  m2_5calc.f <-- CALCULATION PART OF MAIN + SUBROUTINES FILRED, GETCOF,
C		CALGRD, NOTINT, STAT, PREDCYC, AND PRNTOP, ALONG WITH
C		SUBROUTINE EQUIV FROM m246calc.f ... PLUS

C	        MODULES iduser_4.f, idcy_4.f, AND vodtot.f (= IDUSER.FOR
C		IDCY_4.FOR, AND VODTOT.FOR, RESPECTIVELY).
 
C  NOTE THAT SUBROUTINE DELAY IS NOT USED.
 
C  NOTE THAT SEVERAL OF THE OUTPUT FORMATS ARE CHANGED FROM
C  (25,*)' ...' TO (25,xxxx) WHERE xxxx IS A FORMAT LABEL. THIS
C  PREVENTS THE sun COMPUTER FROM INSERTING AN EXTRA SPACE IN THE
C  OUTPUT FILE, WHICH SCREWS UP SUBROUTINE PREVRUN'S READING IN OF

C  THE OUTPUT FILE.
 
C-----------------------------------------------------------------------
 
C  REFER TO THE CODE OF MXEM2_5.FOR FOR THE COMMENTS REGARDING THIS
C  SERIES OF PROGRAMS.
 
C-----------------------------------------------------------------------
 
C***********************************************************************

      SUBROUTINE NPAG(MAXSUB,MAXGRD,MAXDIM,MAXACT,
     1  NUMEQT,MAXOBS,WORK,WORKK,SPXGYJ,DXI,PYJGX,PYJGXX,
     2  DENSTOR,EXX,CORDEN,CORHOLD,YPREDPOP,YPREDPOPT,YPREDBAY,CORDLAST)
 
C wmy2017Sep08 ... to get thead num in DO 800
        USE OMP_LIB
        USE npag_utils, only: makevec,check_input_array_size,expand_grid
     1   ,verifyval,cp_lrcs_to_rpar,max_input_dim,maxnumeq,max_m_per_obs
     2   ,max_ODE_params,max_pop_rand_varbs,max_doses,max_ODE_comps
     3   ,max_pop_varbs,max_pop_params,max_RS_J,k_gamma,k_flat
     4   ,k_sfac,k_ofac,k_sum_z_sq,k_prod_pr,i_skip_ig,i_do,i_cycle
     5   ,i_errmod,i_is_poisson_obs,i_is_log10,i_Npoissonobs,i_Jsub,i_IG
     6   ,k_resolve

      IMPLICIT REAL*8(A-H,O-Z)

	REAL*8 KU

C      PARAMETER(MAXNUMEQ=7)
 
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


C  NOTE THAT ALL THE DIMENSIONS = 25 BELOW 'SHOULD' BE CHANGED TO
C  MAXDIM, BUT SINCE THESE ARRAYS ARE SO SMALL, CHANGING THEM TO
C  VARIABLY DIMENSIONED ARRAYS (WHICH REQUIRE PASSING THE ARRAYS AND
C  MAXDIM THROUGH ALL RELATED CALLING STATEMENTS) IS NOT WORTH IT.
C  SIMILARLY FOR PX(max_ODE_params), SINCE max_ODE_params = 25 (MAXDIM) + 7 (MAX. NO. OF
C  FIXED PARAMETERS).
 
C  NOTE THAT ALL DIMENSIONS = 150 HAVE BEEN CHANGED TO max_m_per_obs, SINCE THIS
C  NO. REPRESENTS THE TOTAL NO. OF OBSERVATIONS (AND THE MAX. NO IS
C  6 OUTPUT EQUATIONS x 99 OBSERVATIONS/EQ). THIS COULD BE CHANGED
C  TO NUMEQT*MAXOBS, BUT IT WOULD BE MORE TROUBLE THAN IT'S WORTH TO
C  MAKE THESE DIMENSIONS VARIABLE.
C  ACTUALLY, IN THE PC PREP PROGRAM, MAXOBDIM IS SET = 150. SO THAT
C  REMAINS THE MAX. NO. OF OBSERVATION TIMES (AS OF NPAG113.FOR).

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

C
C ------ PK model parameter declarations
C
C      DIMENSION AB(30,2),    ! population random parameter ranges
C     1 EXX(MAXSUB,3,30),CENTER(3,30),EXXX(30)   ! central tendencies, mean, med, and mode 
C     2 E(30,30),EX(30),CORR(30,30),COV(30,30),STD(30),    ! COV = E - EX(I)*EX(J); CORR = COV/STD
C     3 COFVR(30),    ! = STD/EX
C     9 X(30),    ! I think these are means
C     9 EXO(30),START(30),STEP(30),XMED(30),OPTVAR(max_ODE_params),    ! eldery variables
C     3 RANFIXEST(max_pop_varbs)
C     2 VALFIX(max_pop_params), ! Population parameters: fixed, user defined constant parameters
C     9 PX(max_ODE_params) ! Support point
C
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
C
C --- end PK model parameter declarations
C


C  NOTE THAT THE 2ND DIMENSION OF SIG AND YOO IS MAXNUMEQ, RATHER THAN
C  NUMEQT. THE REASON IS THAT THESE ARRAYS ARE PASSED IN COMMONS AND
C  THEREFORE CAN ONLY BE VARIABLY DIMENSIONED BY A VALUE SET IN
C  A PARAMETER STATEMENT.

      CHARACTER PREFIX*5,READLINE*300,EXT*3,NAME*4,
     1PRIFIL2*20,DENFIL*20,OUTFIL*20,PREDFIL*20,
     2OUTCOM*20,READLARG*1000,OUTFILER*20,ERRFIL*20

      character*20 ERRFILNAME
      character*20 ITFIL

	COMMON SIG
	COMMON/SUPRES/ISUPRES
        COMMON/OBSER/TIMOB,DOSTIM,RS,YOO,BS 
	COMMON/NXER/NXE
C NXE FROM ABOVE COMMON IS NO. OF TIMES XERRWD IS CALLED.

C wmy2017Sep26 -- WMY retained the common blocks, and all
c  common parameters are still initialized.  This is so that
c  the analytic routes have access to them. But the parallelized
c  code (calling dvode via USERANAL) does not use the common
c  blocks. And eventually ANAL3() will have to receive 
c  required arguments, rather than read values in from a common
c  block.
        COMMON/PARAMD/P
        COMMON/INPUT/ R,B
!       COMMON/TOCALC/IRAN,PX,NOFIX,NSUB,gamma,flat,AB
!       COMMON/TOCALC/gamma,flat,AB,PX,IRAN,NOFIX,NSUB
C   	COMMON/TOUSER/NDIM,MF,RTOL,ATOL

!$omp ThreadPrivate(/OBSER/,/PARAMD/,/INPUT/)
C !$omp ThreadPrivate(/TOCALC/)

C  COMMON/TOCALC IS PROVIDED TO SUBROUTINE CALCRF, WHICH IS CALLED
C  BY SUBROUTINE ELDERY. wmy20190722-variables passed to CALCRF
C  via ELDERY in argument list. No more need for /TOCALC/

C      COMMON/ERR/ERRFIL 
      logical input_arrays_within_bounds

c wmy2017Sep22 Variables passing through DO 800
       integer NVAR,NRANFIX
       integer, dimension(max_ODE_params) :: IRAN

C "interface" to emint()
       integer ijob 

c wmy2017Dec01 DO 1000 variables
       integer NInDO1000
       real PInDO1000
       double precision, dimension(max_m_per_obs,NUMEQT) :: YPRED

C wmy2018Apr17 (Tax Day) DO 140 Variables
       integer UseInlineDO140
       integer MISVAL, NNORMALOBS,NPOISSONOBS
       real*8  SIGFAC, OFAC

c wmy2017Sep08 DO 800 variables
       integer IterFirst, IterLast, NInDO800, NNInDO800,NBadInDO800
       integer NPX, NOFIXCOPY,ThreadNo
       integer, dimension(max_ODE_params) :: IRANCOPY
       real*8 W, XXIG(max_pop_rand_varbs)
       real    PInDO800
       double precision, save, dimension(max_m_per_obs,MAXNUMEQ)
     1   :: ObsError
C wmy2018.10.16 /BOLUSCOMP/
       integer, save, dimension(max_input_dim) :: NBCOMP
C wmy2017Sep22 /TOUSER/
       integer NDIM, MF
       doubleprecision RTOL
       real*8, dimension(max_ODE_comps) :: ATOL
C wmy2017Sep26 Copies of /INPUT/, /PARAMD/
       real*8, dimension(max_ODE_params) :: P, PCOPY
       real*8, dimension(max_RS_J) :: R, RCOPY
       real*8, dimension(max_ODE_comps) :: B, BCOPY
       real*8, save, dimension(max_m_per_obs) :: TIMCOPY
       real*8, save, dimension(max_doses) :: SIGCOPY
       real*8, save, dimension(max_doses,max_RS_J) :: RSCOPY
       real*8, save, dimension(max_doses,max_input_dim) :: BSCOPY
c wmy2017Sep30 -- Keep a copy of all integers
C   required to calculate P(JSUB|IG) here.
       integer, save, dimension(128) :: INTLIST
       integer, save, dimension(257) :: IPAR
       double precision, save, dimension(257) :: RPAR

       integer alg_type
       integer isupres

C       save ObsError, IPAR  ! SAVE attribute in declaration
!$omp ThreadPrivate(ObsError,IPAR,RPAR,INTLIST)

C wmy2017Nov14 -- Trying to figure out why non-master threads are
c   slightly off; Removed RSCOPY and BSCOPY from Firstprivate
c   list in DO 800 and made them ThreadPrivate here; as far as I
c   can tell, these are the last two variables that are SAVEd but
c   were not threadprivate -- I can't figure out a good reason
c   to suspect these variables are an issue, except that they are
c   large and so might need to be on the heap. An example of the
c   error thrown is:
c 
c DVODE--  Warning: internal T (=R1) and H (=R2) are                              
c       such that in the machine, T + H = T on the next step                      
c       (H = step size). solver will continue anyway                              
c      In above,  R1 =  0.7000000000000D+01   R2 =  0.1897079122660D-26 
c
c !$omp ThreadPrivate(RSCOPY,BSCOPY)
c
c Above is incorrect for an unknown reason. The code generates 
c   following errors (examples below):
c "R init :: RS.ne.RSCOPY for          15         203           1 "
c
c


C  THE BLANK COMMON ABOVE IS SUPPLIED TO SUBROUTINE IDPC.
C  COMMON/TOUSER IS SUPPLIED TO SUBROUTINE USERANAL IN idfixed.f.
C  COMMON/OBSER/ IS SUPPLIED FROM SUBROUTINE FILRED.
C  COMMON/SUPRES/ IS SUPPLIED TO SUBROUTINE EMINT.
C  COMMON/ERR/ IS SUPPLIED TO ALL THE ROUTINES WHICH COULD WRITE TO
C   ERRFIL.

      EXTERNAL CALCRF

C***********************************************************************
 
C-----------------------------------------------------------------------
 
    2 FORMAT(A20)
  222 FORMAT(A3)
 2222 FORMAT(A5)
 
C-----------------------------------------------------------------------
 
C  INPUT FILE npag103.inp FROM THE PREPARATION PROGRAM. npag103.inp 
C  CONTAINS THE USER DESIRED PARAMETER VALUES, ALONG WITH THE 
C  CONCATENATED PATIENT DATA FILES (IN ADAPT FORMAT).

CCCCCCCCCCCCCCCCCCCCCC  INPUT INFO  (BELOW) CCCCCCCCCCCCCCCCCCCCCCCCC
 
C  INPUT THE FOLLOWING DATA FROM FILE npag103.inp.

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
 
C  IFORMT = 1 OR 3 IF PATIENT DATA FILES ARE IN ADAPT FORMAT. AND THEY 
C           ALWAYS ARE (ALTHOUGH THIS IS THE NEW MULTI-DRUG FORMAT).
C           NOTE THAT IFORMT IS STILL READ IN, BUT CALLED JUNK.
 
C       PREFIX = 5-CHARACTER PREFIX FOR ALL SUBJECT FILENAMES. THEN
C           001, 002,... UP TO 999  WILL COMPLETE THE 1ST NAMES OF THE
C           INDIVIDUAL FILENAMES WHICH ARE READ IN.
 
C       EXT = AN OPTIONAL 3-CHARACTER EXTENSION (AFTER DECIMAL POINT)
C             FOR THE INPUT FILENAMES.
 
C  	NVAR = NO. OF RANDOM VARIABLES FOR THE RUN.
C  	PAR(I) = NAME OF R.V. I; I=1,NVAR.
C 	NOFIX = NO. OF FIXED PARAMTER VALUES FOR THE RUN.
C 	PARFIX(I) = NAME OF FIXED PARAMETER I; I=1,NOFIX.
C     NRANFIX = NO. OF UNKNOWN PARAMETERS WHICH ARE THE SAME FOR 
C               ALL SUBJECTS FOR THE RUN.
C     PARRANFIX(I) = NAME OF UNKNOWN PARAMETER I; I=1,NRANFIX.

C  	IRAN(I) = 1 IF PARAMATER I IS RANDOM;
C	          0 IF PARAMETER I IS FIXED; 
C               2 IF PARAMETER I IS UNKNOWN BUT THE SAME FOR ALL
C                 SUBJECTS; I = 1,NVAR+NOFIX+NRANFIX.


C  NSUBTOT = TOTAL NO. OF SUBJECTS IN THE PATIENT POPULATION. 
C  NSUB = NO. OF SUBJECTS WHOSE DATA ARE TO USED FOR THE ANALYSIS (MAX
C         NO. = 999).
C  IPATVEC(I),I=1,NSUB = INDICES OF THE SUBJECTS WHICH ARE TO BE USED

C                        IN THIS ANALYSIS.
 
C  IF NOFIX > 0, VALFIX(I) = VALUE OF FIXED PARAMETER I, I=1,NOFIX.

C  IF NRANFIX > 0, RANFIXEST(I) = INITIAL ESTIMATE FOR RANFIX PARAMETER
C   I, I = 1,NRANFIX.

C   	AB(I,1) = LOWEST VALUE FOR VARIABLE I ON ITS GRID, I=1,NVAR.
C     AB(I,2) = HIGHEST VALUE FOR VARIABLE I ON ITS GRID,I=1,NVAR.

 
C	NUMEQT = NO. OF OUTPUT EQUATIONS.
C	C0P(I),C1P(I),C2P(I),C3P(I) = COEFFICIENTS FOR THE ASSAY STD.
C		DEV. (OF THE OBSERVED VALUES) FOR OUTPUT EQ. I;
C		I=1,NUMEQT. THEY'RE INCLUDED ONLY SO THEY CAN BE PUT
C		INTO THE OUTPUT FILE.

C  AS OF m2_6calc.f, EACH SUBJECT'S INDIVIDUAL C'S ARE INCLUDED IN THE
c  CONCATENATED PATIENT DATA FILES PORTION OF npag102.inp.
 
C  IERRMOD, GAMLAM0 = INFO ON THE ERROR PATTERN (SEE CODE).


C  NDRUG = NO. OF DRUGS IN THE PATIENT DATA FILES.
C  AF(I),I=1,NDRUG = ACTIVE (SALT) FRACTION FOR DRUG I.

C  INDPTS = THE INDEX OF THE NO. OF GRID POINTS TO BE USED.
C	    INDPTS = 1,2,3,4,5,6 FOR,
C           RESPECTIVELY, 2129, 5003, 10007, 20011, 40009, OR 80021 PTS.
C           IF INDPTS > 6, THE NO. OF GRID PTS = 80021*(INDPTS-100).
 
C    AS OF npbig1.f, THE GRID POINTS ARE FOUND DIRECTLY FROM

C    SUBROUTINE CALGRD (I.E., SUBROUTINE GETCOF IS NO LONGER USED).
 
 
C  MAXCYC = MAXIMUM NO. OF CYCLES; THE PROGRAM WILL STOP BEFORE THIS 
C           NO. OF CYCLES IF IT CONVERGES--SEE FORMATS 5197 AND 5198
C           IN THE CODE.


C  NOTE THAT JSTOP AND TOL ARE STILL READ IN, BUT ARE NOT USED. INSTEAD,
C  TOL IS HARDCODED TO BE 1.D-4 IN THIS PROGRAM.

C  IDELTA, T_END, XMIC, ICENT, AUCINT: THESE VALUES WILL BE USED TO 
C   CALCULATE PREDICTED VALUES FOR EACH SUBJECT AT TIMES 0, IDELTA,


C   2*IDELTA, ..., T-END, ALONG WITH AUC'S (AREAS UNDER CURVES) AND 
C   AUC/MIC'S. NOTE THAT IDELTA = 2, 4, 6, OR 12 MINUTES, AND T_END IS 
C   IN HOURS.

C   NO. AS OF bigmlt6.f, T_END WILL STILL BE READ, BUT NO LONGER USED,
C    SINCE SUBJECTS CAN HAVE TIME RESETS --> EACH SUBJECT WILL HAVE TO
C    HAVE ITS MAXIMUM TIMES (ONE FOR EACH SET OF TIMES BEFORE A TIME
C    RESET OR THE ENDING TIME) CALCULATED BY NEW SUBROUTINE CALCTPRED.
 
C  ICYCLE = NO. OF CYCLES RUN BY A PREVIOUS RUNNING OF THIS PROGRAM.
 

C  IF ICYCLE .GT. 1 --> THIS RUN IS PICKING UP WHERE A PREVIOUS RUN
C			LEFT OFF. IN THIS CASE, READ IN ALSO:
C	DORIG = THE UPPER BOUND FOR THE DIFFERENCE BETWEEN THE DENSITY
C		OF CYCLE 1 AND THE MAXIMUM LIKELIHOOD ESTIMATE OF THE
C		DENSITY.
 
C	NACTVE = THE NO. OF ACTIVE GRID POINTS IN THE PRIOR DENSITY.
C	CORDEN(I,.) = ITH ACTIVE GRID COORDINATES (1ST NVAR ENTRIES) +
C		      THE ASSOCIATED DENSITY (NVAR+1 ST ENTRY),
C		      I=1,NACTVE.

C	PRIFIL2 = NAME OF FILE GIVING THE PRIOR INFO (DORIG, CORDEN,
C		  ETC). NOTE THAT THIS PRIOR INFO HAS BEEN STORED
C		  INTO npembig3.inp. PRIFIL2 ITSELF IS NOT USED BY THE
C		  PROGRAM, BUT THE NAME IS INCLUDED SO THE USER WILL
C		  KNOW WHERE THE PRIOR DATA ORIGINATED.
 
CCCCCCCCCCCCCCCCCCCCCCCC  INPUT INFO (ABOVE) CCCCCCCCCCCCCCCCCCCC

      input_arrays_within_bounds = check_input_array_size(
     1  MAXSUB,MAXGRD,MAXDIM,MAXACT,NUMEQT,MAXOBS,WORK,WORKK,
     2  SPXGYJ,DXI,PYJGX,PYJGXX,DENSTOR,EXX,CORDEN,CORHOLD,
     3  YPREDPOP,YPREDPOPT,YPREDBAY,CORDLAST)
      if (input_arrays_within_bounds .eqv. .false.) then
         write (*,*) "Input array error; exiting"
         return
      end if

2227        FORMAT(A11)

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
	IF(JNUM .EQ. 10000) JNUM = 1
	BACKSPACE(25)
	WRITE(25,*) JNUM
	CLOSE(25)

 
C  CREATE OUTPUT FILE WHICH HAS 'OUT' AS ITS 1ST 3 CHARACTERS AND

C  NAME AS ITS LAST 4. SIMILARLY, CREATE DENFIL, PREDFIL, ITFIL, AND
C  ERRFIL.

C  AS OF bignpaglap1.f, THE NAME FOR THE OUTPUT FILE IS CHANGED TO
C  'OUTT'//NAME SINCE 'OUT//NAME WILL BE RESERVED FOR THE COMBINED
C  OUTPUT FILE FORMED AT THE END OF THE RUN.
 
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

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

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

 
C  NOTE THAT NUMEQT WAS PASSED IN ARGUMENT LIST FROM npagdriv.f
C  TO THIS MODULE, SUBROUTINE BIGNPAG, SO JUST READ(23,*) ON NEXT LINE.
 
		READ(23,*)

		DO IEQ=1,NUMEQT
		 READ(23,*) C0P(IEQ),C1P(IEQ),C2P(IEQ),C3P(IEQ)
     1                      ,C4P(IEQ),C5P(IEQ) 
		END DO
 
		READ(23,*) ierrmod,gamlam0

                IPAR(i_errmod) = ierrmod

C  NOTE THAT IN npag102.inp FROM NPBIG15.FOR, ierrmod AND gamlam0
C  (INITIAL EST. FOR gamma or lambda) WILL BE READ IN. AND NOTE THAT FOR
C  ierrmod = 4, THIS VALUE SHOULD BE READ IN AS flat, RATHER THAN AS


C  gamma, TO BE COMPATIBLE WITH THE CODE BELOW.


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
 
C  ESTABLISH THE NO. OF GRID POINTS TO BE USED.
 
	IF(INDPTS .EQ. 1) NGRID=2129
	IF(INDPTS .EQ. 2) NGRID=5003

	IF(INDPTS .EQ. 3) NGRID=10007
	IF(INDPTS .EQ. 4) NGRID=20011
	IF(INDPTS .EQ. 5) NGRID=40009
	IF(INDPTS .EQ. 6) NGRID=80021
	IF(INDPTS .GT. 6) NGRID = (INDPTS - 100)*80021
cadapt change 1 - reset ngrid to MAXACT if NGRID>MAXACT
 
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

c  As of npageng23.f, TOL is no longer read in, as it is going to be

c  reset = 1.D-4 below anyway. Instead, TOLC (the value against which
c  checkbig is compared) will now be read in.

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
 
 
c  The patient data info is read in from npag103.inp, and is put onto 
c  scratch file 27 (because it will need to be reread each cycle --> 
c  file 27 will be rewound each cycle).

c  Note that there are NSUBTOT subjects, but only NSUB of them,
c  with indices IPATVEC(I),I=1,NSUB, will be put onto file 27.
 
        OPEN(27)

 1717  FORMAT(A300)
       NLAFIR = 0

       DO JSUB = 1,NSUB


C  THE NEXT SUBJECT WHOSE DATA SET IS TO BE PUT TO FILE 27 IS SUBJECT 
C  IPATVEC(JSUB). SO FAR, NLAFIR IS THE NO. OF LINES WHICH HAVE BEEN
C  READ WHICH HAVE 'LAST AND FIRST' AS CHARACTERS 3:16 (THIS LINE IS
C  THE FIRST LINE OF EACH SUBJECT'S DATA SET). READ UNTIL THE NEXT
C  SUCH LINE.

 1720    READ(23,1717,IOSTAT=IEND) READLINE

	 IF(IEND .LT. 0) THEN

C  NOTE THAT IEND .LT. 0 --> END OF FILE REACHED, BUT IF IT'S REACHED
C  AT THIS POINT, NOT ALL "ACTIVE" NSUB SUBJECT DATA SETS WERE READ

C  AND WRITTEN CORRECTLY TO FILE 27. IN THIS CASE, WRITE A MESSAGE TO
C  THE USER AND STOP.

        WRITE(*,1721)
 1721   FORMAT(/' PATIENT DATA INFORMATION WAS NOT READ CORRECTLY'/
     1' FROM THE INSTRUCTION FILE, npag103.inp. IF YOU EDITED THIS'/
     2' FILE MANUALLY, PLEASE RERUN THE PC PREP PROGRAM TO HAVE IT'/
     3' PREPARE npag103.inp AGAIN AND THEN RERUN THIS PROGRAM.'//


     4' IF YOU DID NOT MANUALLY EDIT npag103.inp, PLEASE SEND THE'/
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

       CALL NEWWORK1(MAXSUB,JSUB,TIMOBREL,errfilname)

C  AS OF npageng18.f, NEWWORK1 ALSO ESTABLISHES TIMOBREL(JSUB,J).
C  THESE ARE THE "RELATIVE" OBSERVATION TIMES FOR THIS SUBJECT. 
C  IF THE SUBJECT CONTAINS STEADY STATE DOSE SETS, THESE TIMES WILL
C  DIFFER FROM THE TIMES WRITTEN TO FILE 27 SINCE THOSE TIMES WILL BE 
C  THE "REAL" TIMES STARTING FROM THE BEGINNING OF EACH STEADY STATE
C  DOSE SET, AND NEEDED BY THE ID MODULES, RATHER THAN THE "RELATIVE"
C  TIMES STARTING AT THE END OF EACH STEADY STATE DOSE SET).

  
	END DO

C  THE ABOVE END DO CLOSES THE  DO JSUB = 1,NSUB  LOOP.

 
 1730 REWIND(27)
	CLOSE(23)

C  NOTE THAT IF LABEL 1730 WAS REACHED VIA THE
C  IF(IEND .LT. 0) GO TO 1730    STATEMENT ABOVE, IT MUST BE BECAUSE
C  THE END OF THE FILE WAS REACHED AND THE LAST SUBJECT ON THE FILE
C  23 WAS ALSO THE LAST ONE (NO. IPATVEC(NSUB)) TO BE ANALYZED. THIS
C  MEANS JSUB SHOULD BE NSUB. IF, HOWEVER, JSUB .LT. NSUB, IT MEANS
C  THAT, SOMEHOW, NOT ALL NSUB SUBJECTS TO BE ANALYZED WERE ON THE
C  npag103.inp FILE. IN THIS CASE, WRITE A MESSAGE TO THE USER AND
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


 
C  IF ISUPRES = 1 --> the output to the screen will be minimal ...
c  one line having just the cycle no., convergence criterion, and the 
c  medians. The exception is that until NACTVE .LE. NSTORE, the
c  program will print to the screen the update on what % of grid
c  points have been calculated since otherwise the user might think

c  his computer has locked up (once NACTVE .LE. NSTORE, all the
c  P(YJ|X)'s will already be stored into PYJGX --> the DO 800 loop
c  will go very fast.

c  IF ISUPRES = 0 --> the output to the screen will be the full set
c  of information that has been historically printed.

	ISUPRES = 1



C  OPEN THE OUTPUT FILE -- ALL OUTPUT FROM THE PROGRAM WILL BE PUT
C			   INTO THIS FILE.
 
	OPEN(25,FILE=OUTFIL)
 
C  SET IFIRST = THE 1ST CYCLE FOR THIS RUN. IFIRST MUST BE SAVED SO
C  LOOP 800 WILL ONLY NEED TO CALL IDPC IF ICYCLE=IFIRST.
 
	IFIRST=ICYCLE+1
 
c open the iteration log file
      open(91,file=ITFIL)
      if(ierrmod.eq.1.or.ierrmod.eq.2) write(91,9190)
      if(ierrmod.eq.4) write(91,9290)
      if(ierrmod.eq.3) write(91,9390)
cgam2_9190 format('  icycle',5x,'fobj1',10x,'fobj0',10x,'res',5x,
 9290 format('  icycle',5x,'fobj1',10x,'flat wt',8x,'res',5x,
     &      'grid points (start and end)')
 9190 format('  icycle',5x,'fobj1',10x,'gamma',10x,'res',5x,
     &      'grid points (start and end)')

 9390 format('  icycle',5x,'fobj1',10x,'lambda',9x,'res',5x,
     &      'grid points (start and end)')

c  Create the files CHMAXCYC.BAT, CHMAXCYC.OLD, and CHMAXCYC.NEW:
C  CHMAXCYC.OLD has one line with a 1 in it. CHMAXCYC.NEW has one
c  line with a 0 in it. CHMAXCYC.BAT is a DOS batch file which has
c  the system command to copy CHMAXCYC.NEW to CHMAXCYC.OLD.

C  Note that the program will open and read CHMAXCYC.OLD at the 
c  beginning of each new cycle. If it reads a 1, it will continue the 

c  calculations as before. If it reads a 0, it will change the value of 
c  MAXCYC to whatever ICYCLE is currently, which will cause the program 
c  to halt at the end of that cycle just as if MAXCYC had been set
c  to the changed value originally. If this happens, the program will
c  write a comment to the console and file 25 of why the program
c  has stopped prematurely.

c  Note that the user may want to exercise this option if he wants to
c  see the results from a slowly converging run, and then, because
c  the density file from the last cycle will have been correctly 
c  created, still be able to start another run using the final cycle 
c  joint density from the halted run (with different parameters if 
c  desired).

	OPEN(37,FILE='CHMAXCYC.OLD')
	 WRITE(37,*)'   1'
	CLOSE(37)

	OPEN(37,FILE='CHMAXCYC.NEW')
	 WRITE(37,*)'   0'
	CLOSE(37)

	OPEN(37,FILE='CHMAXCYC.BAT')
	 WRITE(37,*)'  COPY CHMAXCYC.NEW CHMAXCYC.OLD'
	CLOSE(37)

c  Tell the user that he can execute the batch file, CHMAXCYC.BAT 
c  (by typing CHMAXCYC at a DOS prompt), in the working directory of 
c  his PC and that will cause the program to stop safely at the end of 
c  whatever cycle it is on, as if the maximum no. of cycles has been 
c  reached.



	WRITE(*,1231)
 1231   FORMAT(/' IF YOU WOULD LIKE TO STOP THE PROGRAM BEFORE THE'/
     1' MAXIMUM NO. OF CYCLES HAVE BEEN RUN, ALT-TAB TO A DOS'/
     2' WINDOW IN THE WORKING DIRECTORY OF THE RUN AND TYPE: '//
     3' >CHMAXCYC   '//
     4' THIS WILL CAUSE THE PROGRAM TO STOP SAFELY AT THE END OF THE '/
     5' NEXT CYCLE, AFTER CREATING THE OUTPUT FILES.')
!	CALL PAUSE
C  COMMENTED OUT THE ABOVE ALL PAUSE AS OF npageng28.f

C  CALCULATE VOLSPA, THE 'VOLUME' OF THE INTEGRATION SPACE (NEEDED IN
C  CALLS TO NOTINT).
 
      VOLSPA=1.D0
      DO 170 I=1,NVAR
  170 VOLSPA = VOLSPA*(AB(I,2)-AB(I,1))
 
 
C  IF THE NO. OF PREVIOUS CYCLES, ICYCLE = 0, IT MEANS THAT THIS IS
C  A NEW RUN, USING A UNIFORM PRIOR JOINT DENSITY. IF ICYCLE .GE. 1,
C  THIS IS A CONTINUATION RUN, AND THE PRIOR JOINT DENSITY HAS BEEN
C  READ IN ABOVE.
 
	IF(ICYCLE .EQ. 0) THEN
 
C  FIND THE VALUE OF THE UNIFORM DENSITY OVER THE 'RECTANGULAR' NVAR-
C  DIM VECTOR SPACE [AB(1,1),AB(1,2)] x ... x [AB(NVAR,1),AB(NVAR,2)].
 
      CONST=1.D0/VOLSPA
 
C  CONST IS THE INITIAL VALUE OF THE JOINT DENSITY AT ALL THE GRID
C  POINTS TO BE DETERMINED BELOW. PUT IT INTO ALL ENTRIES OF COLUMN
C  NVAR+1 OF CORDEN.
 
C  NOTE: SINCE THE APRIORI DENSITY IS UNIFORM, ITS VALUES ARE
C        THE SAME REGARDLESS OF THE ACTUAL GRID POINTS USED (I.E., ONLY
C        THE BOUNDARIES OF THE RECTANGULAR GRID SPACE, [AB(I,1),AB(I,2)]
C        FOR I=1,NVAR ARE IMPORTANT).
 
C  ESTABLISH MATRIX CORDEN TO HAVE ALL THE GRID POINTS AND ASSOCIATED
C  DENSITY VALUES (FOR ROW K, THE 1ST NVAR VALUES = COORDINATES OF GRID
C  POINT K; LAST VALUE = ASSOCIATED DENSITY OF GRID POINT K).

C  AFTER THE DO 30 LOOP, SUBROUTINE CALGRD (WHICH GIVES THE COORDINATES
C  OF A DESIRED POINT) WILL NOT HAVE TO BE CALLED AGAIN, SINCE ALL THE
C  INFORMATION WILL BE STORED INTO CORDEN, WHICH WILL BE CONDENSED
C  AFTER EACH CYCLE TO INCLUDE ONLY THE 'HIGH PROBABILITY' POINTS.
 
	DO 30 IG = 1,NGRID
 


	  CORDEN(IG,NVAR+1)=CONST
 
C  GET THE COORDINATES OF THE IGTH GRID POINT.
 

C  AS OF npbig1.f, THE GRID POINTS ARE FOUND DIRECTLY FROM
C  SUBROUTINE CALGRD (I.E., ROUTINE GETCOF IS NO LONGER USED).
 
C  CALL CALGRD AND GET THE IGTH COORDINATES; THEN PUT THEM INTO THE
C  1ST NVAR ENTRIES OF ROW IG OF CORDEN. NOTE THAT THE 1ST TIME CALGRD
C  IS CALLED, A VARIABLE (FIRST) IS INITIALIZED, AND THEN EACH
C  SUCCEEDING TIME IT IS CALLED, CALGRD 'KNOWS' TO CONTINUE FINDING
C  THE SEQUENCE OF GRID POINTS.
 
	CALL CALGRD(NVAR,NGRID,AB,X,ERRFIL)
C     1     S,QS,COEF,RQS,NEXTN,TESTN,HISUM)
	DO J=1,NVAR
	  CORDEN(IG,J) = X(J)
	END DO
	
   30   CONTINUE
 
 
C  INITIALIZE NACTVE (THE NO. OF ACTIVE GRID POINTS) TO = NGRID (WHEN

C  A PRIOR DENSITY IS USED, NACTVE IS READ IN).
 
	NACTVE=NGRID
 
 
	ENDIF
 
C  THE ABOVE ENDIF IS FOR THE  IF(ICYCLE .EQ. 0)  CONDITION.
 

C  AS OF npageng19.f, PRESET NACTLAST TO BE NACTVE. THIS WAY, IN THE
C  UNLIKELY EVENT THAT THE FIRST CYCLE OF A RUN HAS A HESSIAN ERROR
C  (WHICH MEANS THAT WHEN CONTROL COMES BACK TO MAIN FROM SUBROUTINE
C  emint, IT IS TRANSFERRED TO LABEL 900 AND THEREFORE SKIPS THE
C  cbegin statistics  SECTION WHERE NACTLAST = NACTVE IS SET), THERE
C  WON'T BE A PROBLEM WHEN NACTVE IS SET = NACTLAST JUST BELOW LABEL 
C  900. IN PREVIOUS PROGRAMS, IN THE ABOVE SITUATION, BELOW LABEL 900,
C  NACTVE = NACTLAST WOULD RESULT IN NACTVE BEING SET = 0 SINCE 
C  NACTLAST WAS UNITIALIZED.

      NACTLAST = NACTVE 


        prefobj=-1.d30
        prebig=-1.d30

 
C  THE 2ND LINE GIVES THE VERSION NO. OF THE PROGRAM ITSELF.

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

C  CALL SUBROUTINE WRITEPT2 TO WRITE THE PATIENT NOS. TO BE USED IN 
C  THE ANALYSIS TO FILE 25 ... IN AN "EFFICIENT" WAY, AS OPPOSED TO
C  ONE INDEX PER LINE. NOTE THAT THE FIRST ARGUMENT TELLS WRITEPT2
C  TO WRITE TO FILE 25. NOTE THAT THE '    0' AFTER THE CALL TO
C  WRITEPT2 TELLS THE PROGRAM READING THIS FILE THAT THE PATIENT
C  NOS. HAVE ENDED.

	CALL WRITEPT2(25,NSUB,IPATVEC)
	WRITE(25,*) '    0'
 
	WRITE(25,9763)

 9763   FORMAT(/' THE RANDOM VARIABLES AND THEIR RANGES ARE: ')
	
C  REPLACE WRITING OF AB() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL. SIMIARLY FOR ALL CALLS TO VERIFYVAL.

      DO I=1,NVAR
       XVERIFY(1) = AB(I,1)
       XVERIFY(2) = AB(I,2)
       CALL VERIFYVAL(2,XVERIFY)

C      WRITE(25,1217) PAR(I),AB(I,1),AB(I,2)
       WRITE(25,1217) PAR(I),XVERIFY(1),XVERIFY(2)
	END DO
 1217   FORMAT(/' ',A11,': ',G17.10,'   TO   ',G17.10)
 
  	IF(NOFIX .EQ. 0) WRITE(25,9764)
 9764   FORMAT(/' NO FIXED PARAMETER VALUES.')

 
  	IF(NOFIX .GT. 0) THEN
 
       WRITE(25,9766)
 9766  FORMAT(/' THE USER-ENTERED FIXED PARAMETER VALUE(S) IS (ARE):')
 
C  REPLACE WRITING OF VALFIX() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL.

      DO I=1,NOFIX
       XVERIFY(1) = VALFIX(I)
       CALL VERIFYVAL(1,XVERIFY)
C      WRITE(25,1219) PARFIX(I),VALFIX(I)
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

C  REPLACE WRITING OF RANFIXEST() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL.
 
      DO I=1,NRANFIX
       XVERIFY(1) = RANFIXEST(I)
       CALL VERIFYVAL(1,XVERIFY)
C      WRITE(25,1219) PARRANFIX(I),RANFIXEST(I)
       WRITE(25,1219) PARRANFIX(I),XVERIFY(1)
	END DO
	WRITE(25,*)
 
  	ENDIF


C  AS OF bignpaglap4.f, NGRID is written on the line following the
c  text.

	WRITE(25,9869) NGRID
 9869   FORMAT(/' THE NO. OF GRID POINTS IS '/,I10)

	WRITE(25,*)' THE NO. OF CURRENTLY ACTIVE GRID POINTS IS ',NACTVE
 


C  REPLACE WRITING OF C0P(),...,C3P() WITH XVERIFY (SEE LOGIC IN 
C  SUBROUTINE VERIFYVAL.

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
C
C wmy2019.01.16 In above DO IEQ = 1,NUMEQT, C4P and C5P are added to
C    OUT<N> file.  Also, C4 = C4P and C5 = C5P.  C4 and C5
C    tell the program that these YO are log10(measure) and ~Poisson,
C    respectively.  Thus, C4 and C5, apply to all YO. 

	WRITE(25,2112)
 2112   FORMAT(/' YOU HAVE CHOSEN TO MODEL THE ASSAY ERROR FUNCTION,'/
     1' S.D. AS FOLLOWS (ASSUMING SD1 = C0+C1*Y+C2*Y**2+C3*Y**3):')

C  REPLACE WRITING OF GAMLAM0 WITH XVERIFY (SEE LOGIC IN 
C  SUBROUTINE VERIFYVAL.



     	 IF(IERRMOD .EQ. 1) WRITE(25,2113)
       XVERIFY(1) = GAMLAM0
       CALL VERIFYVAL(1,XVERIFY)


C      IF(IERRMOD .EQ. 2) WRITE(25,2114) GAMLAM0
C      IF(IERRMOD .EQ. 3) WRITE(25,2116) GAMLAM0
C      IF(IERRMOD .EQ. 4) WRITE(25,2117) GAMLAM0
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

C  SET LASTCYC = LAST CYCLE NO. WHICH WAS PRINTED TO THE OUTPUT FILE
C  AND TO THE SCREEN. THIS IS NEEDED SINCE OTHERWISE ICYCLE IS PRINTED
C  OUT 3 TIMES (SEE FORMATS 1237 AND 1239) IF ierrmod .GE. 2. SEE THE 
C  NEW CODE WHICH RETURNS CONTROL TO 10001 TO RUN DIFFERENT 
C  "gammaplus/minus eps tries". THEN ONLY PRINT THE CYCLE NO. IF IT
C  HASN'T BEEN PRINTED BEFORE.

	LASTCYC = ICYCLE
 
C  CORDEN HOLDS, IN ITS FIRST NACTVE ROWS, THE STARTING JOINT DENSITY

C  AND COORDINATE VALUES. FOR K=1,NACTVE, CORDEN(K,J) = JTH COORDINATE
C  OF THE KTH ACTIVE POINT, J=1,NVAR; AND CORDEN(K,NVAR+1) IS THE
C  ASSOCIATED DENSITY FOR THE KTH ACTIVE POINT.
 
C  IF ICYCLE .GT. 0, CORDEN WAS READ IN.
C  IF ICYCLE = 0, NACTVE=NGRID, AND CORDEN WAS FILLED AT LABEL 30 ABOVE.
C		 IN THIS CASE, THE DENSITY IS UNIFORM, SO ALL
C		 CORDEN(K,NVAR+1) VALUES = 1/VOLSPA, K=1,NACTVE.
 
 
C	IPRED=11 + ICYCLE
C	JCOL=0
	ITEST=0
 
C  IPRED IS THE CYCLE NO. WHERE THE NEXT 2-CYCLE PREDICTION
C  ALGORITHM STARTS (IT IS NO LONGER USED). JCOL = COLUMN NO. OF DENSTOR
C  IN WHICH IS STORED THE DENSITY OF ONE OF THE 2-CYCLES USED IN THE
C  PREDICTION (IT IS NO LONGER USED). IT IS SET = 0 ABOVE, SINCE NO 

C  STORAGE IS REQUIRED UNTIL CYCLE NO. 11


C  (SEE BELOW). ITEST=0 --> THE NEXT CYCLE IS NOT (INITIALIZED) TO BE
C  A TEST CYCLE (SEE CODE BELOW WHEN ITEST=1,2, OR 3).
 
C    NEW FOR m2_13cal.f: NSTORE SET = 0. NSTORE IS THE NO. OF GRID
C    POINTS, WHOSE P(YJ|X) VALUES HAVE BEEN STORED IN PYJGX IN LOOP 800.
C    THIS NO. CAN BE CHANGED BY THE 'CONDENSING' CODE BELOW, SINCE
C    INACTIVE POINTS ARE THROWN OUT.
 
	NSTORE=0
cadapt    initialize grid resoution to 20%
          resolve=0.20000000298023224
          rpar(k_resolve) = resolve

c  As of bigmlt1.f, the program checks to see if MAXCYC = 0. If so, no
c  cycle calculations are done. Instead control transfers to the end 
c  of the cycle calculations to pick up the means, medians, and modes

c  of the read-in density (which are needed to calculate YPREDPOP AND
c  YPREDPOPT), and then to label 900 to calculate the output files.

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

C  Note that the program will open and read CHMAXCYC.OLD at the 
c  beginning of each new cycle. If it reads a 1, it will continue the 
c  calculations as before. If it reads a 0, it will change the value of 
c  MAXCYC to whatever ICYCLE is currently, which will cause the program 
c  to halt at the end of that cycle just as if MAXCYC had been set
c  to the changed value originally. If this happens, the program will
c  write a comment to the console and file 25 of why the program
c  has stopped prematurely.

	OPEN(37,FILE='CHMAXCYC.OLD')
	 READ(37,*) ICONTIN
	CLOSE(37)

	IF(ICONTIN .EQ. 0) THEN
	 MAXCYC0 = MAXCYC
	 MAXCYC = ICYCLE
	ENDIF


cgam3
10001 continue

c above is new entry point for gammaplus/minus eps tries
      itest = 0
 
 
cadapt  reset number of stored points to that before expansion
c     nstore=nstoresv
 
C+++++++++++++++ CODE ABOVE TO USE ONLY HIGH PROB POINTS ++++++++++++++
C  PRINT THE CYCLE NO. ONLY IF THIS CYCLE IS CERTAIN TO BE RUN (I.E.,
C  IF ITEST = 0). SUBROUTINE PRNTOP PRINTS THE CYCLE NO., ETC. FOR
C  ACCELERATED CYCLES.
 
	IF(ITEST .EQ. 0 .AND. ICYCLE .NE. LASTCYC) THEN
	 WRITE(*,1237) ICYCLE
	 WRITE(25,1239) ICYCLE
	 LASTCYC = ICYCLE
	ENDIF
 
 1237   FORMAT(///' CYCLE NO.',I5,'. SUBJECT NOS FOLLOW: ')
 1239   FORMAT(///' CYCLE NO.',I5,/)
	
 

C  ICYCLE IS THE NUMBER OF THE NEXT CYCLE TO BE RUN.
C
C  THIS IS WHERE EACH NEW CYCLE STARTS (FOR EACH CYCLE, THE DENSITY OF
C  X IS UPDATED FROM THE PREVIOUS DENSITY ESTIMATE, USING THE
C  OBSERVED SUBJECT DATA FROM THE INPUT DATA FILES WHICH ARE PASSED TO
C  SUBROUTINE IDPC BELOW.
C
C  ZERO OUT SPXGYJ. AFTER THE SUBJECT LOOP
C  (DO 1000), IT WILL BE THE VECTOR OF LENGTH NACTVE WHOSE ITH ELEMENT
C  IS THE SUM(P(X=X(I)|Y(J))) OVER J=1,NSUB, WHERE X = NVAR-
C  DIMENSIONAL. ALSO, ZERO OUT SLPYJ. AFTER THE SUBJECT LOOP, IT
C  WILL BE THE SCALAR SUM(LN(P(Y(J)))) OVER J=1,NSUB.

 
 

      DO 55 I=1,NACTVE
   55 SPXGYJ(I)=0.D0
!      SLPYJ=0.D0
C  COMMENTED OUT SLPYJ = 0 AS OF npageng28.f

 
C  START THE SUBJECT LOOP.
 
 

C  REWIND SCRATCH FILE 27 WHICH HAS ALL THE SUBJECT DATA FILES
C  CONCATENATED ON IT, IN ORDER.
 
	REWIND(27)
 



C  NOBTOT WILL BE THE RUNNING TOTAL OF ALL NON-MISSING OBSERVED VALUES
C  OVER ALL THE NSUB SUBJECTS. THIS IS NEEDED TO CALCULATE BIC BELOW.

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

C  CALL SUBROUTINE FILRED TO READ, FOR THIS SUBJECT, FROM SCRATCH FILE
C  27, THE NO. OF OBSERVATION TIMES (NOBSER) AS WELL AS THE
C  OBSERVED VALUES THEMSELVES: YO(I,J) = THE 'NOISY' OBSERVED VALUES
C  FOR THIS SUBJECT; I=1,NOBSER, J=1,NUMEQT. THESE OBSERVED VALUES ARE
C  USED ONLY TO CALCULATE THE ASSAY STANDARD DEVIATIONS (USING THE
C  VECTORS, C0,C1,C2,C3, WHICH ARE ALSO READ IN). THE REST OF THE INFO
C  IN THE SUBJECT DATA FILE IS PASSED IN COMMONS TO THE IDPC MODULE
C  SUBROUTINES.
 
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,
     1    TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     2    INTLIST,ERRFILNAME)

C        write (*,*) "Ret. from FILRED() nr. #3675"

        call cp_lrcs_to_rpar(gamma,flat,numeqt,c0,c1,c2,c3,c4,c5,rpar)
!         RPAR(k_gamma) = gamma
!         do III=1,numeq
!           RPAR(k_c0_base + III) = C0(III)
!           RPAR(k_c1_base + III) = C1(III)
!           RPAR(k_c2_base + III) = C2(III)
!           RPAR(k_c3_base + III) = C3(III) wmy2018.01.16 added C4 and C5 to list
!        end do

C Verify that NUMEQT <= 7
        if (NUMEQT.gt.7) then
           write (*,*) "WARNING :: NUMEQT.gt.7",
     1        NUMEQT
           CALL PAUSE
           STOP
        end if

C wmy2017Sep29
C        do III=1,2
C        write (*,*) "JSUB->FILRED",
C     1    RSCOPY(III,1), RSCOPY(III,2), RSCOPY(III,3),
C     2    RSCOPY(III,4), RSCOPY(III,5), RSCOPY(III,6),
C     3    RSCOPY(III,7), "Continue to DO 800"
C        end do
C in above debug statement: the greco model has 1 drug and
C 5 additional covariates, COMMON/CONST2/ND,NADD  -- thus
C NI = 2*ND+NADD = 7. I don't have access to these variables 
C here, in NPAG. The 2 is because there are only 2 input
C lines.


C  FIND THE ASSAY STANDARD DEVIATIONS FOR THIS SUBJECT. FOR EACH
C  OF THE NOBSER*NUMEQT OBSERVED VALUES (EXCEPT THAT YO(I,J) = -99 -->
C  OUTPUT EQ. J HAS NO OBSERVED LEVEL FOR OBSERVATION TIME I),
C  Y, SIG = C0 + C1*Y + C2*Y**2 + C3*Y**3.
C  NOTE THAT, THEORETICALLY, SIG SHOULD BE A CUBIC FNT. OF
C  THE 'TRUE' OBSERVED VALUES, NOT THE 'NOISY' OBSERVED VALUES (BUT THE

C  'TRUE' VALUES ARE UNKNOWN).
 
C  ALSO, CALCULATE SIGFAC, THE PRODUCT OF THE NON-MISSING STD. DEV.'S

C  (A NON-MISSING S.D. IS ONE FOR WHICH THE CORRESPONDING YO(I,J) IS
C  .NE. -99, THE MISSING VALUE CODE).
C  INITIALIZE SIGFAC=1, AND THEN UPDATE IT FOR EACH NON-MISSING
C  OBSERVATION.
 
C  MISVAL WILL BE THE RUNNING TOTAL OF MISSING VALUES AMONG ALL THE
C  NUMEQT x NOBSER POTENTIAL OBSERVED LEVELS.

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
 
C  IF Y = -99, IT MEANS THAT OUTPUT EQ. J HAD NO VALUE AT OBSERVATION
C  TIME I. IN THIS CASE, IGNORE THIS Y AND INCREASE MISVAL BY 1.
 

	  IF(Y .EQ. -99) THEN
	   MISVAL = MISVAL+1
	   GO TO 140
	  ENDIF

C wmy2019.01.18 -- observation transformation is independent of distribution type
C Are observations recorded as log10(obs) _AND_ in subroutine
C output, are X converted to log10(X)? If so, do you want the
C sd to be calculated on 10^Y(obs or est)? then = 10.
C             if (C1(J).eq.-10) IPAR(i_is_log10+J) = -10
             if (C4(J).eq.10) IPAR(i_is_log10+J) = 10

C wmy2019.01.17 Added distribution flag
C          if (C0(J).eq.-229.and.C2(J).eq.-229
C     1   .and.C3(J).eq.-229) then 
           if (C5(J).eq.229) then
C--------------------------------- Start Poisson

             if (ICYCLE.lt.2) then
               write (*,*) "Poisson analysis req. for OUTEQ",J
             endif

             NPOISSONOBS=NPOISSONOBS+1
             ObsError(I,J)=1.D0
             SIG(I,J)=1.D0
             IPAR(i_is_poisson_obs+J)=229
C--------------------------------- End Poisson
          else
C--------------------------------- Start NORMAL

C  NOTE: FOR EACH SUBJECT, MUST ENSURE THAT ALL THE STD DEV'S ARE NON-
C        ZERO. OTHERWISE, THE PROGRAM WILL BLOW UP! THIS IS BECAUSE
C        P(YJ|X) INVOLVES SQUARED DIFFERNCES BETWEEN OBSERVED Y'S AND
C        EXPECTED Y'S (FOR EACH X GRID POINT)...EACH DIFFERENCE
C        NORMALIZED (I.E., DIVIDED) BY THE VARIANCE OF THE RESPECTED
C        OBSERSATION.
 
C 	 SEE M2_17CAL.F CODE FOR COMMENTS ON HOW A STD. DEV. COULD = 0.
 
C  ALSO TEST TO MAKE SURE NO STD. DEV. < 0, SINCE SIGFAC BEING NEGATIVE
C  WOULD RESULT IN A NEGATIVE PROBABILITY (SEE PYJGX CALCULATION BELOW).

      SIG(I,J) = C0(J)+C1(J)*Y+C2(J)*Y*Y+C3(J)*Y**3
cgam4
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

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,2345) JSUB
        CLOSE(42)

		CALL PAUSE
       	STOP
      ENDIF

C wmy2018.06.26 Not sure why this is here; the test inside of the
C  Normal initialization above should be all that is necessary.
C 
C      IF(SIG(I,J) .LT. 0) THEN
C                WRITE(*,2346) JSUB
C                WRITE(25,2346) JSUB
 2346            FORMAT(//' A S.D. < 0 FOR JSUB = ',I5,'. RERUN THE '/
     1' PROGRAM WITH A BETTER CHOICE FOR THE ASSAY ERROR POLYNOMIAL'/
     2' COEFFICIENTS.')
C               CLOSE(27)
C               CLOSE(25)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

C        OPEN(42,FILE=ERRFIL)
C         WRITE(42,2346) JSUB
C        CLOSE(42)
C
C               CALL PAUSE
C               STOP
C      ENDIF

 
C wmy2017Dec28 SIG is in the un-named COMMON; This can cause issues
c  inside the DO 800 loop; so I'm copying the stdev to ObsError
c
      ObsError(I,J) = SIG(I,J)

      SIGFAC=SIGFAC*SIG(I,J)

C--------------------------------- END NORMAL
       endif
 
  140 CONTINUE


C        write (*,*) "Passed DO 140 For", JSUB

C  NOTE THAT SIGFAC WAS CALCULATED IN LOOP 140 ABOVE, AND THAT OFAC IS
C  NOW THE RESULT OF (NOBSER*NUMEQT - MISVAL - NPOISSONOBS) VALUES.

C wmy -- eqns updated to include Poisson  measures; and note that
C  there is no error checking on Poisson measures! But I think we
C  can use C1(J) for that, so I left it out. Maybe this can be
C  the allowable %difference between predicted and observed? else
C  set IPAR(i_skip_ig) to 0.

        OFAC=2.506628274631**(NOBSER*NUMEQT - MISVAL - NPOISSONOBS)

        NOBTOT = NOBTOT + NOBSER*NUMEQT - MISVAL

        RPAR(k_sfac) = SIGFAC
        RPAR(k_ofac) = OFAC
 
C  NOTE THAT 2.5066... = SQRT(2*PI).

C       else CALL DO140(ijob = 0,...); contained in npag_utils.mod; 
C          but I think this is now unnecessary as ObsError() should
C          be used throughout the code, and ObsError will be
C          calculated later, but prior to calculating PYJGX.

        endif
C above "if (UseInlineDO140)" can be removed if no problems
C   arise from it's not being run above. Otherwise, call DO140()
C   from npag_utils.
C   wmy20190731 -- benchRun001c DEN and ILOG match 
 
C  FOR EACH SUBJECT, AND EACH GRID POINT, CALL IDPC, A SUBROUTINIZED
C  VERSION OF THE ADAPT PROGRAM ID3 TO CALCULATE THE SUM OF SQUARES OF
C  DIFFERENCES BETWEEN THE OBSERVED VALUES AND PREDICTED (BY THE MODEL)
C  VALUES (NORMALIZED BY THE ASSAY VARIANCE OF EACH OBSERVATION) ...
C  BUT THIS NEEDS TO BE DONE JUST ONCE (ICYCLE=IFIRST), BECAUSE THE
C  VALUES P(JSUB|IG), FOR SUBJECT JSUB AND GRID PT. IG, DON'T CHANGE
C  CYCLE - TO - CYCLE, AND CAN THEREFORE BE STORED INTO PYJGX(JSUB,IG).
 
 8888   FORMAT(' ',' CYCLE ',I5,',  SUBJECT ',I5,' ...  % COMPLETED = ',

     1F8.2)
	XNEXT = 1.D0


C wmy2017Sep21 -- Note that ALL COMMON blocks are going to have 
C   unexpected behavior inside of the !$omp parallel region --
C   So make sure all COMMON blocks inside of parallel regions
C   are declared Threadprivate (if that works, great!); but
C   it's even better if we just don't use them at all: 
C   pass variables down to the SRs and funcs that need them.
C   Alternatively, some people say to make them into modules.
C  
C	COMMON/TOCALC/IRAN,PX,NOFIX,NSUB,gamma,flat,AB
C
C wmy 2017Sep08 Copy the stored values of PYJGX to work, here.
C   this is a separate DO loop preceding DO 800 because it
C   can't be a parallelized due to all of the shared memory
C   Original code looked this this
C      DO 800 IG=1,NACTVE
C	IF(IG .LE. NSTORE) GO TO 700
C    700 (This code) 
C    800 Continue
C
      if (ICYCLE .ge. 2) then

        if (NSTORE .le. NACTVE) then

          DO 750  IG=1,NSTORE

            WORK(IG)=PYJGX(JSUB,IG)*CORDEN(IG,NVAR+1)

	    WORKK(IG) = PYJGX(JSUB,IG)

  750     Continue

        endif

      endif
C
C wmy2017Sep08 ... I moved the above block out of the DO 800 loop
C  Thus, the Iterations for the !$omp do are changed as follows.
C
      if (ICYCLE .eq. 1) then
        IterFirst = 1
      else
        IterFirst = NSTORE + 1
      endif
      IterLast = NACTVE
      if ( IterLast > MAXACT ) IterLast = MAXACT + 1
c
c wmy2017Nov02
      if (IterLast < 1) write (*,*) "IterLast < 1",
     1   "check INDPTS in (1...6;101,102,...)"
C
C Also, inside the DO 800 loop, there is no reason now to check
C if IG goes above MAXACT+1 to exit loop, just require the
C last iteration to be the lesser of MAXACT+1 or NACTVE
C

c wmy2017Oct25 -- COMMON blocks have "random" (unexpected) behavior
c inside the parallel region for all threads other than master. So,
c for "shared" read only variables, as opposed to working variables
c (see below), make a FirstPrivate variable, e.g. IRANCOPY, that is
c passed to each subroutine.  Initialize these "shared" variables
c immediately before entering the parallel region.
        do iparam=1,max_ODE_params
           IRANCOPY(iparam) = IRAN(iparam)
        end do
        NOFIXCOPY = NOFIX
c-----------------------------------------------------------------------
C      write (*,*) "START DO 800 :: First and Last Support are",
C     1 IterFirst, IterLast, "Gamma = ", gamma
       NInDO800=0
       NBadInDO800=0
       NNInDO800=99
       PInDO800=0.0
       IPAR(i_skip_ig)=1
! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp Parallel Default(PRIVATE)
!$omp&Shared(PYJGX,WORKK,WORK,NInDO800,NNInDO800,CORDEN,NBadInDO800)
!$omp&CopyIn(ObsError,IPAR,RPAR,INTLIST)
!$omp&CopyIn(/OBSER/,/PARAMD/,/INPUT/)
!$omp&FirstPrivate(JSUB,SIGFAC,OFAC,NOBSER,NUMEQT,PInDO1000,NDIM,MF)
!$omp&FirstPrivate(IterFirst,IterLast,NSTORE,NACTVE,MAXACT,RTOL,ATOL)
!$omp&FirstPrivate(VALFIX,RANFIXEST,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY)
!$omp&FirstPrivate(NVAR,NRANFIX,IRANCOPY,NOFIXCOPY,NBCOMP)
c !$omp&num_threads(2)
c !$omp&Shared(MAXTHREAD)
c -----------
c  !$omp&private(ThreadNo) ! By Default
c   ABOVE LINE EDITED AS OF npagranfix5.f.


c These COMMON blocks are still defined AND used by the analytic
c routines. So IDPC must receive copies of them (see above, and below
c comments).
c     COMMON/PARAMD/P -> PCOPY
c     COMMON/INPUT/ R,B -> RCOPY, BCOPY
c     COMMON/TOCALC/IRAN,PX,NOFIX,NSUB,gamma,flat,AB -> IRANCOPY, etc.

c COMMON blocks will have unexpected behavior inside the parallel region.
c "shared" read only variables are initialized before the parallel region
c and values are assigned to temporary FirstPrivate copies of those variables
c to be used inside the parallel region (see above) -- but for local
c "working" variables, copies must still be made, e.g. PCOPY, but the
c variable is a PRIVATE variable -- and assignment is first to 0.0, near
c the top of the parallel region and then identical to serial program:
c INSIDE the DO 800 loop by calls to functions or subroutines or by
c calculation, as is appropriate for the variable.

!$omp Do
c
c !$omp ThreadPrivate(/TOCALC/,/OBSER/)
C wmy2017Sep10 /TOCALC/ is common block containing PX; I added
C   an explicit declaration of  REAL*8 W to SUBROUTINE NPAG. W should be
C   created w/random value, but is initialized prior to use.
c
C wmy2017Sep08
C   DO is over less IG now
C       DO 800 IG=1,NACTVE
        DO 800 IG=IterFirst,IterLast


C            write (*,*) "Literally, just entered DO 800"


!$omp critical
           IPAR(i_skip_ig) = 1
           NInDO800 = NInDO800 + 1
           NNInDO800 = NNInDO800 + 1
           if (NNInDO800.eq.100) then
             PInDO800 = 100.0*REAL(NInDO800)
     1        /(REAL(IterLast) - REAL(IterFirst))

C  PRINT TO THE SCREEN THE UPDATE ON WHAT % OF GRID POINTS HAVE BEEN
C  CALCULATED IF NACTVE > NSTORE (I.E., IF NACTVE .LE. NSTORE -->
C  ALL P(YJ|X)'s ARE ALREADY STORED INTO PYJGX AND SO THIS 8OO LOOP
C  WILL GO VERY FAST.

C wmy2017Sep29 -- correct to here ... check if it gets to IDPC
c        do III=1,2
C        write (*,*)
C        write(*,*) PInDO1000, "%     --- DO 800 ------- Obs:", JSUB,
C     1    "Support:", IG, "of", IterFirst, "to", IterLast, PInDO800,"%"
c
c     ,"RSCOPY :: ",
c     1    RSCOPY(III,1), RSCOPY(III,2), RSCOPY(III,3),
c     2    RSCOPY(III,4), RSCOPY(III,5), RSCOPY(III,6),
c     3    RSCOPY(III,7), "Initializing IG"
c        end do
C          write (*,*)

          NNInDO800 = 0

        end if

!$omp end critical

C            write (*,*) "... and passed the first critical section"
C
C wmy2017Sep08 no need for this IF anymore 
C       IF(NACTVE .GT. NSTORE) THEN
C
 
C  PRINT GRID PT. AND % COMPLETED TO SCREEN.
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
C 
C       ENDIF
C

C  ESTABLISH THE IGTH GRID POINT. IT IS STORED IN ROW IG OF
C  CORDEN.

c !$omp critical
c !$omp flush(CORDEN) 
        DO J = 1,max_pop_rand_varbs
          XXIG(J) = 0.D0
        END DO
        DO J=1,NVAR
          XXIG(J)=CORDEN(IG,J)
        END DO
c !$omp end critical
 
C  ESTABLISH THE COMBINED RANDOM AND FIXED PARAMETER VALUES INTO
C  PX -- IN THE CORRECT ORDER AS INDICATED BY VECTOR IRAN. CALL
C  MAKEVEC TO DO THIS. AS OF npagranfix.f, ADD NRANFIX AND RANFIXEST
C  TO THE ARGUMENT LIST TOO (I.E., THERE IS NOW A 3RD TYPE OF 
C  PARAMETER (THOSE WHICH ARE UNKNOWN BUT THE SAME FOR ALL SUBJECTS).

C wmy2017Sep21 -- PCOPY enters DO 800 w/no initialization;
C   It isn't used until after it returns from being initialized
C   in MAKEVEC, but it can't hurt to init here.
        do iparam=1,max_ODE_params
          PCOPY(iparam) = 0.0
        end do


C        write (*,*) "1st call to MAKEVEC() NVAR = ", NVAR

        CALL MAKEVEC(NVAR,NOFIXCOPY,NRANFIX,IRANCOPY,XXIG,VALFIX,
     1    RANFIXEST, PCOPY)
        NPX = NVAR+NOFIXCOPY+NRANFIX
C       if (NPX < max_ODE_params) then exit progam w/error
C
C !$omp critical
C        TID = OMP_GET_THREAD_NUM()
C        write (*,*) "IG", IG, "calling IDPC" 
C

C wmy2018Jan12 I removed the COMMON/PARAMD/P from SUBROUTINE IDCALCY and
c  IDCALCYY, so P must be initialized in main after calling MAKEVEC, but
c  before calling them. Note: IDCALCY and -YY now call SYMBOL and then 
c  immediately call FUNC2() or FUNC3(). Here, the ANAL3 routine still
c  requires that /PARAMD/P be set, and I'm not sure where, exactly, it is
c  used, but we should also set /TOCALC/PX
C        DO I=1,NVAR+NOFIX+NRANFIX
        DO I=1,NPX
          PX(I) = PCOPY(I)
          P(I) = PCOPY(I)
        END DO

c wmy2017Sep12 Added /TOUSER/ varbs to CALL IDPC()
c NPX,NOBSER,NUMEQT,NDIM, MF

C      write (*,*) JSUB, IG, "DO 800 CALL IDPC", PCOPY(1), PCOPY(2),
C     &  PCOPY(3), PCOPY(4), PCOPY(5)

          W = 0.0

C
C wmy2018Aug29 -- cross platform validation (is failing)
C
c        if (JSUB.eq.1) then 
c          if ((IG.eq.40012).or.(IG.eq.120034).or.(IG.eq.160045)
c     1     .or.(IG.eq.280075)) then
c            write (*,*) IG,"IG",(PCOPY(rrow),rrow=1,NPX)
c          end if
c        end if
c
        CALL IDPC(JSUB,IG,NPX,PCOPY,NBCOMP,W,NOBSER,NUMEQT,
     1    NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     2    INTLIST,IPAR,ObsError,RPAR,ERRFILNAME)
C
C        w = rand() # wmy used this to prevent a crash and get an idea of code
C        w = w**2 # wmy wanted to guarantee the rand() was positive
C        write (*,*) IG, "returns", w, "from IDPC." 
C !$omp end critical

C  These are written at end of DO 800
C       write (*,*) "IG, W", IG, W
 
C  W RETURNS AS THE SUM OF:
C  ((YO(I,J)-H(I,J))/SIG(I,J))**2, WHERE H(I,J) = PREDICTED VALUE OF THE
C  JTH OUTPUT EQ AT THE ITH OBSERVATION TIME, ASSUMING THE IGTH GRID
C  POINT, X, ... OVER THE NOBSER x NUMEQT QUANTITIES ABOVE WHICH DON'T
C  HAVE YO(I,J) = -99 (WHICH MEANS THAT OUTPUT EQ. J HAS NO OBSERVED
C  LEVEL FOR TIME I).
 
C  CALCULATE P(YJ|X) FOR X-GRID POINT NO. IG.


 
C  THIS NEXT TEST IS FOR THE PC. AS AN EXAMPLE, THE COMPAC COMPUTER
C  CANNOT HANDLE ARGUMENTS TO DEXP WHICH ARE SMALLER THAN -11354. SINCE
C  THE ARGUMENT TO DEXP BELOW IS -.5*W, SET PYJGX = 0 IF W IS .GT.
C  22708.
 
C  SEE CODE AFTER CALCULATION OF P(YJ) TO SEE WHAT HAPPENS IF ALL THE
C  P(YJ|X) ARE  SET = 0.
 
C  NOTE THAT WORKK WILL ALWAYS BE SET = P(YJ|X=IG GRID PT), WHICH IS
C  NEEDED IN THE CALCULATION OF DXI (NOTE DXI NOT USED AS OF 
C  bignpaglap1.f) SINCE PYJGX WILL NOT BE COMPLETE IF NACTVE > MAXACT.

C wmy2017Sep07 note that as of OpenMPv2.5 the following flush and
c   critical should not matter, compiler knows to do this for read/
c   writing to the shared arrays.
c !$omp flush (WORK,WORKK,PYJGX,CORDEN)
c !$omp critical


C
C Poisson (or other distibution)
C    ObsError(I,J) at top of DO 1000 loop assumes Normal.  But for 
C other distribtions, such as Poisson, we must recalculate
C ObsError(I,J) _AND_ for any JSUB that has at least 1 non-normal
C ObsError(I,J), we must recalculate SIGFAC an OFAC.
C    Since recalculating seems ponderous -- just move entire
C Calculation inside of the DO 800 loop.
C

        if (IPAR(i_skip_ig).eq.1) then


          IF(IG .LE. MAXACT) PYJGX(JSUB,IG)=0.D0

          WORKK(IG) = 0.D0
C
C Old Code, Assumes all observations are ~ Normal
C 
C          IF(W .LE. 22708.D0) THEN
C           IF(IG .LE. MAXACT) PYJGX(JSUB,IG) = DEXP(-.5D0*W)/SIGFAC/OFAC
C           WORKK(IG) = DEXP(-.5D0*W)/SIGFAC/OFAC
C          ENDIF
C
C New Code, Assumes Mix of ~ Poisson and ~ Normal

          if ((IPAR(i_Npoissonobs).gt.0)
     1        .and.(RPAR(k_prod_pr).eq.0)) then
              write (*,*) JSUB,IG,"Err: 1 < # < P_thresh w/pr=0"
     1           , IPAR(i_Npoissonobs), i_Npoissonobs, i_Jsub, i_IG
          endif

          IF(RPAR(k_sum_z_sq) .LE. 22708.D0) THEN
           IF(IG .LE. MAXACT) THEN
            PYJGX(JSUB,IG) = 10**RPAR(k_prod_pr)
     1        * DEXP(-.5D0*RPAR(k_sum_z_sq))/RPAR(k_sfac)/RPAR(k_ofac)
           ENDIF
           WORKK(IG) = 10**RPAR(k_prod_pr)
     1        * DEXP(-.5D0*RPAR(k_sum_z_sq))/RPAR(k_sfac)/RPAR(k_ofac)
          ENDIF

C wmy2019.02.08 -- debugging
C          if (PYJGX(JSUB,IG) .LT. 0.0000000001) then
C              write (*,*) "PYJGX",JSUB,IG,10**RPAR(k_prod_pr),WORKK(IG)
C          endif

C End New Code

C  CALCULATE P(X,YJ) FOR X-GRID POINT NO. IG. PUT IT INTO WORK(IG).
 
          IF(IG .GT. MAXACT) THEN

           WORK(IG) = WORKK(IG)*CORDEN(IG,NVAR+1)

          ELSE

  700      WORK(IG)=PYJGX(JSUB,IG)*CORDEN(IG,NVAR+1)

           WORKK(IG) = PYJGX(JSUB,IG)

          ENDIF

C Remember that the following command ONLY
C works if the program is running in parallel!
C          ThreadNo = OMP_GET_THREAD_NUM()
C          write (*,*) "TID", TID, "returns", work(IG), WORKK(IG) 
C
C IPAR(i_cycle) = ICYCLE // at label 1001
c           if (NNInDO800.eq.0) then
C              write (*,*)
C     1 " --- END DO 800 :: (CYCLE,TID,JSUB,IG,s,w/w',PYJGX,px) ="
C     1    ,IPAR(i_cycle),ThreadNo,JSUB,IG,RPAR(k_sfac)
C     1    ,W,RPAR(k_sum_z_sq),WORKK(IG),CORDEN(IG,NVAR+1)
c           endif

c wmy201722Dec -- Need to restore option to skip bad points
c wmy2017Jul26 -- I think go to is safe here ...
C if (IgIsGoodPoint.eq.0) then ; IgIsGoodPoint replaced by IPAR(i_skip_ig)
C

        else

          NBadInDO800 = NBadInDO800 + 1
          PYJGX(JSUB,IG)=0.D0
          WORKK(IG) = 0.D0
          WORK(IG) = 0.D0

C          write (*,*) "Cycling past (JSUB,IG) ==", JSUB, IG,
C     1       NBadInDO800, "of", NInDO800, "of", IterLast

C          GO TO 800 ! we'll get there next anyways
        endif

c !$omp end critical


!        write (*,*) "At 800 ::",JSUB,IG,PYJGX(JSUB,IG)

  800   CONTINUE

! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp   End Do
!$omp   End Parallel

          write (*,*) "For JSUB:",JSUB,"Skipped",
     1       NBadInDO800, "of", NInDO800, "of", IterLast
 
C  CALCULATE P(YJ), A SCALAR WHICH IS THE INTEGRAL OF P(X,YJ) OVER

C  X-SPACE.
 
C  CALL NOTINT, AN INTEGRATION ROUTINE. THE

C  FOLLOWING IS SUPPLIED TO THIS ROUTINE:
C  VOLSPA = VOLUMNE OF THE INTEGRATION SPACE.
C  NGRID = NO. OF ORIGINAL GRID POINTS.
C  NACTVE = NO. OF ACTIVE GRID POINTS.
C  WORK(I), I=1,NACTVE = VALUE OF THE FUNCTION TO BE INTEGRATED, AT
C                       THE ITH GRID POINT.
C  MAXGRD  = THE DIMENSION OF WORK.
  
C wmy2017Sep08
C      write (*,*) "Calling NOTINT immediately after DO 800"

      CALL NOTINT(VOLSPA,NGRID,NACTVE,WORK,MAXGRD,PYJ)

C      write (*,*) "Returning", PYJ
 
C  IF PYJ RETURNS AS 0, IT IS BECAUSE P(X,YJ)=WORK IS 0 IN ALL ITS
C  NACTVE ENTRIES. THIS OCCURS WHEN EACH OF NACTVE VALUES OF W (WHICH
C  RETURNS FROM THE CALLS TO IDPC) IS LARGER THAN 1416 (SINCE P(YJ|X)
C  INVOLVES e RAISED TO THE POWER -.5*W, AND e RAISED TO A POWER
C  SMALLER THAN -708 IS SET TO 0 BY, FOR EXAMPLE, THE COMPAC COMPUTER).
C

C  IN CASE THIS HAPPENS, PRINT A MESSAGE TO THE USER AND STOP.
C
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

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,26) JSUB 
        CLOSE(42)


        CALL PAUSE
        STOP
        ENDIF
 
 
C  ALSO, FROM LOOP 800 ABOVE, WORK(I) = P(X(I),YJ) --> WORK(I)/PYJ
C  = P(X(I)|Y=YJ). ADD THIS TO SPXGYJ(I) = SUM OF P(X(I)|Y=YJ) OVER ALL
C  NSUB SUBJECTS. DO THIS FOR ALL I=1,NACTVE.
 
! NEW PARALLEL CODE BELOW AS OF npageng28.f. 

!       DO I=1,NACTVE
!         SPXGYJ(I)=SPXGYJ(I)+WORK(I)/PYJ
!       END DO
 
C  ADD THIS SUBJECT'S CONTRIBUTION TO SLPYJ.
 
!       SLPYJ=SLPYJ+DLOG(PYJ)
 

 
 1000   CONTINUE


C  WRITE A MESSAGE TO THE USER IN CASE THIS RUN IS ANALYZING A LARGE

C  NO. OF SUBJECTS WITH A LARGE NO. OF INITIAL GRID POINTS ... SO HE
C  WILL KNOW THE PROGRAM HAS NOT 'HUNG'. ALSO SEE FORMAT 123 IN 
C  Subroutine emint.

      IF(ICYCLE .EQ. 1) WRITE(*,1243)
 1243 FORMAT(/' The Adaptive Grid optimization process could take '/
     1' several minutes if you are analyzing a large no. of subjects'/
     2' with a large no. of grid points. '//
     3' An approximate measure of how close the process is to being'/
     4' completed will be printed below: '/) 


C  AS OF npageng20.f, ONLY SAVE THE JOINT DENSITY AFTER CYCLE NO. 2.
C  REASON: CYCLE NO. 1 TYPICALLY HAS A LARGE NO. OF GRID POINTS AND
C   SO SAVING THE JOINT DENSITY AT THAT POINT COULD TAKE A LOT OF 
C   TIME. THAT, ALONG WITH THE FACT THAT THE PROBABILITY THE PROGRAM
C   WILL BOMB AFTER CYCLE 1 AND BEFORE CYCLE 2 IS VERY LOW, ARGUES
C   FOR SAVING THE DENSITY AFTER CYCLE NO. 2., WHEN THE TIME TO SAVE
C   WILL TYPICALLY BE MUCH LESS (SINCE THE NO. OF GRID POINTS WILL
C   PROBABLY HAVE DECREASED A LOT).

C   BY NOT SAVING THE DENSITY EVERY CYCLE, THE RUN WILL BE SPEEDED
C   UP, AND THERE WILL BE VERY LITTLE CHANCE THAT THE RUN, IF COMPILED
C   AND LINKED WITH gfortran, WILL STOP WITH A "Cannot write to file
C   opened for READ" error (referring to DENFIL).

C   ALSO, SINCE THE LION'S SHARE OF TIME FOR AN ANALYSIS IS USUALLY
C   GETTING THROUGH CYCLE 1, SAVING THE DENSITY AFTER CYCLE 2 WILL
C   TYPICALLY BE ALMOST AS GOOD AS SAVING IT AFTER, SAY, 100 CYCLES.

C  SO, IF ICYCLE = 2, STORE THIS CYCLE'S JOINT DENSITY (DOWN TO CORDEN
C  ONLY) INTO THE FILE DENFIL. THEN, IF THE RUN CRASHES BEFORE
C  COMPLETION, THE USER WILL BE ABLE TO PICK THE RUN UP AGAIN AFTER
C  THE END OF CYCLE 2, BY USING THE FOLLOWING DENSITY FILE AS AN 
C  APRIORI FILE.

C  NO! AS OF npageng23.f, THE SAVING OF EACH CYCLE'S DENSITY, EXCEPT
C  AFTER CYCLE 1, IS RESUMED. BUT THIS CAN BE MANUALLY CHANGED BY
C  CHANGING ISAVEDEN BELOW (IF ISAVEDEN IS SET TO SOMETHING OTHER THAN
C  1, EACH CYCLE'S DENSITY WILL NOT BE SAVED).

 
C???DEBUG. CHANGE ISAVEDEN TO BE 0 SO THE DENSITY IS NOT SAVED. THIS
C  IS BECAUSE I GOT SOME 'Cannot write to file opened for READ' 
C  execution errors while testing npagranfix2n.f.


      ISAVEDEN = 0

      IF(ISAVEDEN .EQ. 1 .AND. ICYCLE .NE. 1) THEN 

        OPEN(33,FILE=DENFIL)
 
        WRITE(33,7124)
 
        WRITE(33,*) NDIM
        WRITE(33,*) INDPTS
 
C  NEW CODE ABOVE FOR m2_11cal.f
 
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


C  STARTING WITH MXEM2N36.FOR, NINT WILL ALWAYS BE 100. BUT, IN ORDER
C  NOT TO CHANGE THE STRUTURE OF PRIOR DENSITY FILES (SO THAT PRIOR
C  DENSITIES CAN STILL BE RUN WITH MXEM2N36.FOR), NINT WILL STILL
C  BE WRITTEN TO, AND READ FROM, THIS FILE.
 
        WRITE(33,*) NINT
 
        WRITE(33,*) ICYCLE
        WRITE(33,*) DORIG
 
        DO I=1,NACTVE
          WRITE(33,*) (CORDEN(I,J),J=1,NVAR+1)
        END DO

        CLOSE(33)

      ENDIF

C  ABOVE ENDIF IS FOR THE IF(ISAVEDEN .EQ. 1 .AND. ICYCLE .NE. 1)
C   CONDITION.


c begin optimization
cgam5
cgam5 - from here (immediately after 1000   CONTINUE to
cgam5 - immediately before c end optimization was lifted
cgam5 - from gamadapt1.f, replacing old material beteen these limits
      igamma = igamma + 1
      if(ierrmod.eq.1) igamma=1
csdsc - added April 2, 2000
c con first iteration, call hte interior point method

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

C wmy2017Jan05
c      subroutine emint(psi,ldpsi,theta,ldtheta,npoint,nsub,ijob,
c     &                 x,dx,y,dy,fobj,gap,nvar,keep,IHESS)



C  AS OF npageng18.f, IHESS IS ADDED TO ARGUMENT LIST OF emint.
C  IF IHESS RETURNS AS -1, IT MEANS THE HESSIAN MATRIX IN THE INTERIOR
C  POINTS ALGORITHM WAS SINGULAR. IN THIS CASE, GO TO LABEL 900 TO
C  CREATE THE OUTPUT FILES BASED ON THE VALUES FROM THE PREVIOUS CYCLE.
C  NOTE THAT BY GOING TO LABEL 900, NOTHING FROM THIS CYCLE WILL BE
C  WRITTEN TO FILE 25, AND THE CORDEN(.,.) MATRIX WILL REMAIN WHAT IT
C  WAS FROM THE PREVIOUS CYCLE.

      IF(IHESS .EQ. -1) GO TO 900


      fobj1 = fobj
      nactve1 = nactve

      IF(ISUPRES .EQ. 0) 


     1 write(6,*) 'base job 1, fobj,keep,icycle=',fobj,keep,icycle
C      CALL PAUSE


      ijob = 0
      nactve = keep
      call emint(pyjgx,maxsub,corden,maxgrd,nactve,nsub,ijob,
     &corden(1,nvar+1),denstor(1,1),denstor(1,2),denstor(1,3),
     &fobj,gap,nvar,keep,IHESS,errfil,ISUPRES)

C  AS OF npageng18.f, IHESS IS ADDED TO ARGUMENT LIST OF emint.

C  IF IHESS RETURNS AS -1, IT MEANS THE HESSIAN MATRIX IN THE INTERIOR
C  POINTS ALGORITHM WAS SINGULAR. IN THIS CASE, GO TO LABEL 900 TO
C  CREATE THE OUTPUT FILES BASED ON THE VALUES FROM THE PREVIOUS CYCLE.
C  NOTE THAT BY GOING TO LABEL 900, NOTHING FROM THIS CYCLE WILL BE
C  WRITTEN TO FILE 25, AND THE CORDEN(.,.) MATRIX WILL REMAIN WHAT IT
C  WAS FROM THE PREVIOUS CYCLE.


      IF(IHESS .EQ. -1) GO TO 900


      fobjbase = fobj


      IF(ISUPRES .EQ. 0)
     1 write(6,*) 'base job 0, fobj,keep,icycle=',fobj,keep,icycle
      IF(ISUPRES .EQ. 0) 
     1 write(6,*) 'base job 0 nactve,gamma=',nactve,gamma

      nactve0 = nactve
c new on Jan 2, 2002 - save otpimal solution in denstor(1,4)
c so that stat program can work on best of base, up, and down
c solutions
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
cgamma above endif is for mod(igamma,3).eq.1 case
      if(mod(igamma,3).eq.2) then


      IF(ISUPRES .EQ. 0) write(6,*) 'gamma plus =',gamma

      ijob = 0
      call emint(pyjgx,maxsub,corden,maxgrd,nactve,nsub,ijob,
     &corden(1,nvar+1),denstor(1,1),denstor(1,2),denstor(1,3),
     &fobj,gap,nvar,keep,IHESS,errfil,ISUPRES)

C  AS OF npageng18.f, IHESS IS ADDED TO ARGUMENT LIST OF emint.
C  IF IHESS RETURNS AS -1, IT MEANS THE HESSIAN MATRIX IN THE INTERIOR
C  POINTS ALGORITHM WAS SINGULAR. IN THIS CASE, GO TO LABEL 900 TO
C  CREATE THE OUTPUT FILES BASED ON THE VALUES FROM THE PREVIOUS CYCLE.
C  NOTE THAT BY GOING TO LABEL 900, NOTHING FROM THIS CYCLE WILL BE
C  WRITTEN TO FILE 25, AND THE CORDEN(.,.) MATRIX WILL REMAIN WHAT IT
C  WAS FROM THE PREVIOUS CYCLE.

      IF(IHESS .EQ. -1) GO TO 900


      fobjplus = fobj



      IF(ISUPRES .EQ. 0) 

     1 write(6,*) 'fobjplus,gamma,icycle=',fobjplus,gamma,icycle

c new  Jan 2, 2002 - save solution if fobjplus is better than fobjbase
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


C  AS OF npageng18.f, IHESS IS ADDED TO ARGUMENT LIST OF emint.

C  IF IHESS RETURNS AS -1, IT MEANS THE HESSIAN MATRIX IN THE INTERIOR
C  POINTS ALGORITHM WAS SINGULAR. IN THIS CASE, GO TO LABEL 900 TO
C  CREATE THE OUTPUT FILES BASED ON THE VALUES FROM THE PREVIOUS CYCLE.
C  NOTE THAT BY GOING TO LABEL 900, NOTHING FROM THIS CYCLE WILL BE
C  WRITTEN TO FILE 25, AND THE CORDEN(.,.) MATRIX WILL REMAIN WHAT IT
C  WAS FROM THE PREVIOUS CYCLE.

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
cgamma - above statement changed from "nstore = nactve" to force
c reevaluation of all points.
      
c now temporairily reset to gamma
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
cgam5 above label is entry point for ierrmod = 1 (no  gamma) case

      IF(ISUPRES .EQ. 0) write(6,*) 'fobjbest=',fobj

c corden(*,nvar+1) sums to 1 when it comes out of emint
c Now reset forden(i,nvar+1) to best of three solutions
c and normalize to funny BIGNPEM factor
      fact=ngrid/volspa

      do i=1,nactve
      corden(i,nvar+1)=fact*denstor(i,4)
      enddo


cend optimization

C      write (*,*) "Made it past optimization"

cbegin statistics


c now we compute all hte statistical stuff using this distribution
c and the full nactve (before condensation) points.
c Later, in the condensation performed just before the grid refienment
c and subsequent expansion, we will condense by just using the
c 'keep' flags in DENSTOR(i,1) that emint left there.  The density will

c not be updated to refelct this cahnge (there is no need)
c until the next call to emint


c  As of npageng18.f, save CORDEN to CORDLAST AND NACTVE TO NACTLAST.
c  The reason is that if, somewhere during the next cycle's calculations
c  (during one of the calls to Subroutine emint), a Hessian Matrix is
c  singular, then IHESS will be set = -1, and the program will stop.
c  And in this case, the program must be able to write out all of the
c  information from this cycle (the last completed cycle). And that
c  means that the CORDEN from this cycle (which will be stored into 
c  CORDLAST), and NACTVE (store into NACTLAST) should be used in the
c  call to Subroutine SUBRES in loop 7000. Otherwise, the CORDEN and
c  NACTVE used in that call would have already partly updated in the
c  next cycle before the Hessian error occurred.

      DO I = 1,NACTVE
       DO J = 1,NVAR+1
       CORDLAST(I,J) = CORDEN(I,J)
       END DO
      END DO

      NACTLAST = NACTVE


 
 
C  SLPYJ IS NOW THE LOG-LIKELIHOOD OF ALL NSUB SUBJECT VECTORS, GIVEN
C  THE PRIOR DENSITY IN COL. NVAR+1 OF CORDEN.
 
C  PRINT OUT SLPYJ FOR THE PRIOR DENSITY:


        IF(ISUPRES .EQ. 0) WRITE(*,8) NSUB 

        WRITE(25,8) NSUB
    8 FORMAT(/' THE TRUE (NUMERICAL) LOG-LIKELIHOOD OF THE ',I3/

     1' SUBJECT VECTORS, GIVEN THE PRIOR DENSITY, IS: ')

        IF(ISUPRES .EQ. 0) WRITE(*,*) fobj

        WRITE(25,*) fobj


C  AS OF bigmlt12.f, CALCULATE AIC AND BIC, AS IN itbig13.f. THESE 
C  VALUES WILL PRINT EVERY CYCLE IN THE OUTPUT FILE.

C  AS OF npageng26.f, THE FORMULAS FOR AIC AND BIC ARE CHANGED.

C  CALCULATE TWO MEASURES OF INFORMATION: THE AKAIKE INFORMATION 
C  CRITERION (AIC), AND THE SCHWARTZ (BAYESIAN) INFORMATION CRITERION 
C  (BIC). IN GENERAL, THE MODEL WITH THE MINIMUM AIC AND/OR
C  BIC IS THE PREFERRED MODEL.

C  AIC = 2*(-TRULOG + (D + Q)), WHERE TRULOG = fobj (SEE ABOVE),
C    D = NVAR, THE DIMENSION OF THE RANDOM PARAMETER VECTOR.
C    Q = NO. OF NOISE PARAMETERS = 
C      = 0 IF IERRMOD = 1;
C          = 1 IF IERRMOD = 2,3, OR 4.
C  BUT AS OF npageng27.f, WE USE THE NEW FORMULA, AICC, WHERE
C  AICC = AIC + 2*KP(KP+1)/(NOBTOT-KP-1), WHERE KP = D + Q.

C  BIC = 2*(-TRULOG + .5*(D + Q)*LOG(NOBTOT)), WHERE TRULOG, D, AND Q
C      ARE AS ABOVE, AND NOBTOT IS THE TOTAL NO. OF NON-MISSING OBSERVED
C      VALUES OVER ALL SUBJECTS (INCLUDING ALL NUMEQT OUTPUT EQUATIONS).

        QVAL = 1
        IF(IERRMOD .EQ. 1) QVAL = 0
        KP = NVAR + int(QVAL)

        AIC = 2.D0*(-FOBJ + KP)

C  AS OF npageng28.f, verify that NOBTOT-KP-1 is not .LE. 0. If it
C  is, set AICC = AIC.

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

C+++++++++++++++ CODE BELOW TO USE ONLY HIGH PROB POINTS ++++++++++++++
 

C  PRINT NACTVE ONLY IF THIS CYCLE IS CERTAIN TO RUN (I.E.,
C  ONLY IF ITEST = 0). SUBROUTINE PRNTOP PRINTS THIS INFO FOR
C  ACCELERATED CYCLES.
 
        IF(ITEST .EQ. 0) THEN
 
C  NOTE THAT SPXGYJ(I) FOR ALL INACTIVE GRID POINTS = 0 --- SEE CODE
C  NEAR LABEL 800. SO ALL INACTIVE GRID POINTS WILL REMAIN FOREVER
C  INACTIVE.
 
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
 
C+++++++++++++++ CODE ABOVE TO USE ONLY HIGH PROB POINTS ++++++++++++++

        IF(ISUPRES .EQ. 0) WRITE(*,11)

        WRITE(25,11)
   11 FORMAT(//' THE FOLLOWING VALUES ARE FOR THE UPDATED DENSITY: ')
 
 
C  CALCULATE THE 'SCALED INFORMATION' FOR THIS CYCLE (STARTING WITH

C  PROGRAM m234calc.f, 11-6-94; BUT THE SCALED 'INFO' IS CHANGED
C  IN PROGRAM MXEM2N35.FOR/m235calc.f, 11-19-94):
 
C  INFO = SUM(PI*LOG(PI)), WHERE THE SUM IS OVER I=1,NACTVE, THE LOG
C         IS TO BASE 2, PI*LOG(PI) --> 0 IF PI = 0, AND
C    PI = D(I)*VOLSPA/NGRID (SINCE SUM(D(I)) = NGRID/VOLSPA),
C         WHERE D(I) = CORDEN(I,NVAR+1), I=1,NACTVE.
 
 
C  THEN INFO IS SCALED: y = 100*ln(2)/ln(n/N)*(x + ln(n)/ln(2)), WHERE
C  y = SCALED INFO, x = ABOVE INFO, n = NGRID, N = NO. OF SUBJECTS. SO
C  y = 0 (%) IF DENSITY IS UNIFORM, and y = 100 (%) if DENSITY IS
C  CONCENTRATED AT N EQUALLY LIKELY POINTS, ... I.E., SCALED INFO = 0 %
C  FOR NO INFO, AND SCALED INFO = 100 % FOR PERFECT INFO.
 
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
 
C  BUG CORRECTION DONE FOR m2_19aca.f. ENTROPY CALCULATION MOVED TO
C  AFTER SUM=SUM/DL2 STATEMENT.
 

C  ENTROPY = -INFO = -SUM(PI*LOG(PI)) IS ALSO PRINTED OUT STARTING WITH
C  MXEM2N36.FOR.
 
 
C  SUM IS NOW THE INFO FOR THE JOINT DENSITY. SCALE IT AS SHOWN ABOVE,
C  AND PRINT IT OUT.
 
        DGRID=NGRID

        FACT=100.D0*DL2/DLOG(DGRID/NSUB)
        SUM = FACT*(SUM + DLOG(DGRID)/DL2)

C  REPLACE WRITING OF SUM AND ENT WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL.

      XVERIFY(1) = SUM
      XVERIFY(2) = ENT
      CALL VERIFYVAL(2,XVERIFY)


C     IF(ISUPRES .EQ. 0) WRITE(*,31) SUM
C     IF(ISUPRES .EQ. 0) WRITE(*,131) ENT
 
      IF(ISUPRES .EQ. 0) THEN
        WRITE(*,31) XVERIFY(1)
        WRITE(*,131) XVERIFY(2)
      ENDIF  
   
C     WRITE(25,31) SUM
C     WRITE(25,131) ENT

        WRITE(25,31) XVERIFY(1)
        WRITE(25,131) XVERIFY(2)


   31 FORMAT(/' THE SCALED INFO FOR THIS CYCLE IS ',F10.2,' %'/)
  131 FORMAT(/' THE ENTROPY FOR THIS CYCLE IS ',G11.4/)


 2100   CONTINUE


C  Starting with bigmlt1.f, this is an entry point to pick up needed
c  values.

C        write (*,*) "DO 1100 :: Calculate expected values"

C  CALCULATE EXPECTED VALUES FOR THIS CYCLE'S UPDATED DENSITY.
 
        DO 1100 I=1,NVAR
 
C  FORM THE VECTOR WORK TO BE THE PRODUCT OF THE COORDINATES OF
C  VARIABLE I (IN COL. I OF CORDEN) AND ITS ASSOCIATED DENSITY (IN
C  COLUMN NVAR+1 OF CORDEN).
 
        DO IG=1,NACTVE
          WORK(IG)=CORDEN(IG,I)*CORDEN(IG,NVAR+1)
        END DO
 
        II=I
        CALL NOTINT(VOLSPA,NGRID,NACTVE,WORK,MAXGRD,EX(II))
 
        DO 1100 J=1,I
 
C  FORM THE VECTOR WORK TO BE THE PRODUCT OF THE COORDINATES OF
C  VARIABLES I AND J (IN COLS. I AND J OF CORDEN) AND THEIR ASSOCIATED
C  DENSITY (IN COLUMN NVAR+1 OF CORDEN).
 
        DO IG=1,NACTVE
          WORK(IG)=CORDEN(IG,I)*CORDEN(IG,J)*CORDEN(IG,NVAR+1)
        END DO
 
        JJ=J
 1100   CALL NOTINT(VOLSPA,NGRID,NACTVE,WORK,MAXGRD,E(II,JJ))
 
 
C  STORE THE MEANS INTO CENTER(1,.). CENTER(2,.) AND CENTER(3,.)
C  THE MEDIANS AND MODES WILL BE STORED JUST BELOW THE CALL TO STAT.
C  THE VALUES IN CENTER WILL BE NEEDED BELOW IN THE CALLS TO IDCALCY
C  IF THIS TURNS OUT TO BE THE LAST CYCLE.
 
          DO I=1,NVAR
            CENTER(1,I)=EX(I)
          END DO
 
 
C  CALCULATE THE COVARIANCES; THEN WRITE RESULTS.
 
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

C     IF(ISUPRES .EQ. 0) WRITE(*,5103) (EX(I),I=1,NVAR)
      IF(ISUPRES .EQ. 0) WRITE(*,5103) (XVERIFY(I),I=1,NVAR)


C     WRITE(25,5103) (EX(I),I=1,NVAR)
      WRITE(25,5103) (XVERIFY(I),I=1,NVAR)

 5103   FORMAT(1X,30(G12.6,1X))

C  IN CASE OF VERY SMALL VARIANCES, COV(I,I) COULD TURN OUT TO BE,
C  NUMERICALLY, A SMALL NEGATIVE NO. IN SUCH CASES, SET ALL COV'S
C  = 0 (WHICH MEANS THAT THE STD'S AND COFVR'S WILL ALSO BE SET = 0
C  BELOW), AND SET CORR'S = -99 BELOW. ALSO, SKEWNESS AND KURTOSIS WILL
C  HAVE -99999999 WRITTEN IN THEIR LOCATIONS.

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



C       WRITE(25,5103) (COV(I,J),J=1,I)
        WRITE(25,5103) (XVERIFY(J),J=1,I)


C 200 IF(ISUPRES .EQ. 0) WRITE(*,5103) (COV(I,J),J=1,I)
  200 IF(ISUPRES .EQ. 0) WRITE(*,5103) (XVERIFY(J),J=1,I)



C  CALCULATE AND PRINT OUT STANDARD DEVIATIONS, COEFFICIENTS OF
C  VARIATION, AND CORRELATION COEFFICIENTS.
 

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
 
C     IF(ISUPRES .EQ. 0) WRITE(*,5103) (STD(I),I=1,NVAR)


      IF(ISUPRES .EQ. 0) WRITE(*,5103) (XVERIFY(I),I=1,NVAR)
C     WRITE(25,5103) (STD(I),I=1,NVAR)
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
 


C     IF(ISUPRES .EQ. 0) WRITE(*,5103) (COFVR(I),I=1,NVAR)
      IF(ISUPRES .EQ. 0) WRITE(*,5103) (XVERIFY(I),I=1,NVAR)


C     WRITE(25,5103) (COFVR(I),I=1,NVAR)
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



C      WRITE(25,5103) (CORR(I,J),J=1,I)
       WRITE(25,5103) (XVERIFY(J),J=1,I)


C6080   IF(ISUPRES .EQ. 0) WRITE(*,5103) (CORR(I,J),J=1,I)
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
 
C  CALC. THE MODE (XMODE) AND 5 %-TILE VALUES (X025,X25,X50,X75,X975)
C  FOR EACH VARIABLE (NOTE THAT X50 IS THE MEDIAN). ALSO, CALCULATE
C  SCALINFO, THE SCALED INFORMATION FOR EACH MARGINAL DENSITY.

 
       DO 6090 I=1,NVAR

 
         IND=I
 
C  NOTE: IN THE CALL TO STAT, THE ARGUMENT WORK IS A DUMMY ARRAY; IT
C        MUST BE AN AN ARGUMENT SINCE IT IS VARIABLY DIMENSIONED IN

C        STAT.
 
        CALL STAZ(VOLSPA,NGRID,NACTVE,NVAR,IND,CORDEN,WORK,MAXGRD,NINT,
     1  AB(IND,1),AB(IND,2),XMODE,X025,X25,X50,X75,X975,SCALINFO,NSUB,
     2  MAXDIM)
 
C  STORE THE MEDIANS AND MODES IN CASE THIS IS THE LAST CYCLE (IN WHICH
C  CASE THEY WILL BE NEEDED BELOW IN CALLS TO IDCALCY.
 
         CENTER(2,IND)=X50
         CENTER(3,IND)=XMODE

C  THE 1ST 'AD-HOC' ESTIMATE OF THE STD. DEV. IS THAT OF A NORMAL DIST.
C  HAVING THE SAME [25, 75] %-TILE RANGE AS THIS VARIABLE. SINCE 50 % OF
C  A STANDARD NORMAL DIST. IS BETWEEN MU +/- .6745*SD, THE INNER
C  50 %-TILE RANGE IS APPROX. 1.349*SD WIDE. I.E., SD IS APPROX.
C  (X75-X25)/1.349.
 
C  SIMILARLY, THE 2ND ESTIMATE OF THE SD IS THAT OF A NORMAL DIST.
C  HAVING THE SAME [2.5, 97.5] %-TILE RANGE AS THIS VARIABLE. IN THIS
C  CASE THE SD APPROX = (X975-X025)/3.92.
 
         SDEST1 = (X75-X25)/1.349
         SDEST2 = (X975-X025)/3.92
         SDEST3 = .5D0*(SDEST1+SDEST2)
 
C  CALCULATE KURTOSIS AND SKEWNESS FOR EACH VARIABLE.
C
C  THE COEFF OF SKEWNESS = EXP[(X-EXP(X))**3]/STDX**3, WHERE STDX IS
C                             THE STD DEV OF X.

C
C  SKEWNESS IS > 0 IF THE DISTRIBUTION SKEWS (EXTENDS OUT) TO THE RIGHT.
C  SKEWNESS IS < 0 IF THE DISTRIBUTION SKEWS (EXTENDS OUT) TO THE LEFT.
C  SKEWNESS IS = 0 IF THE DISTRIBUTION IS SYMMETRIC.
C
C  THE COEFF OF KURTOSIS = EXP[(X-EXP(X))**4]/STDX**4.

C
C  KURTOSIS = 3 IF THE DISTRIBUTION IS NORMAL.
C  KURTOSIS > 3 IF THE DISTRIBUTION IS MORE PEAKED THAN THE NORMAL DIST.
C  KURTOSIS < 3 IF THE DISTRIBUTION IS LESS PEAKED THAN THE NORMAL DIST.
C
C  FORM THE VECTOR WORK TO BE (X(I)-EXI)**3*CORDEN(I,NVAR+1), WHERE
C  EXI IS THE EXPECTED VALUE FOR VARIABLE I. THEN INTEGRATE IT TO
C  GET THE NUMERATOR OF SKEWNESS. THEN DO THE SAME FOR THE NUMERATOR
C  OF KURTOSIS, EXCEPT THE EXPONENT IS 4 INSTEAD OF 3.
 
C  NOTE: IF ICOVL0 = 1, STD'S HAVE NOT BEEN CALCULATED ABOVE (SINCE AT
C        LEAST 1 COV(I,I) WAS NUMERICALLY .LE. 0). IN SUCH A CASE,
C        ARBITRARILY SET SK AND KU = -99999999.
 
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

C  THE ABOVE ENDIF IS FOR THE  IF(ICOVL0 .EQ. 1)  CONDITION.

 
      IF(ISUPRES .EQ. 0) WRITE(*,6092) PAR(IND)
        WRITE(25,6092) PAR(IND)
 6092 FORMAT(/' ',A11,':')
 

       XVERIFY(1) = XMODE
       XVERIFY(2) = SK
       XVERIFY(3) = KU   
       XVERIFY(4) = X025
       CALL VERIFYVAL(4,XVERIFY)



C      IF(ISUPRES .EQ. 0) WRITE(*,6093) XMODE,SK,KU,X025
       IF(ISUPRES .EQ. 0) WRITE(*,6093) (XVERIFY(IXV),IXV=1,4) 

C      WRITE(25,6093) XMODE,SK,KU,X025
       WRITE(25,6093) (XVERIFY(IXV),IXV=1,4) 



       XVERIFY(1) = X25
       XVERIFY(2) = X50
       XVERIFY(3) = X75  
       XVERIFY(4) = X975
       CALL VERIFYVAL(4,XVERIFY)

C      IF(ISUPRES .EQ. 0) WRITE(*,6093) X25,X50,X75,X975
       IF(ISUPRES .EQ. 0) WRITE(*,6093) (XVERIFY(IXV),IXV=1,4) 

C      WRITE(25,6093) X25,X50,X75,X975
       WRITE(25,6093) (XVERIFY(IXV),IXV=1,4) 

       XVERIFY(1) = SDEST1
       XVERIFY(2) = SDEST2
       XVERIFY(3) = SDEST3

       XVERIFY(4) = SCALINFO
       CALL VERIFYVAL(4,XVERIFY)



C      IF(ISUPRES .EQ. 0) WRITE(*,6093) SDEST1,SDEST2,SDEST3,SCALINFO
       IF(ISUPRES .EQ. 0) WRITE(*,6093) (XVERIFY(IXV),IXV=1,4) 

C      WRITE(25,6093) SDEST1,SDEST2,SDEST3,SCALINFO
       WRITE(25,6093) (XVERIFY(IXV),IXV=1,4)
 6093 FORMAT(1X,4(G15.8,2X))

       XMED(IND) = X50


 6090  CONTINUE

C        write (*,*) "6090 Calculated Means, Medians, and Modes"

        IF(ISUPRES .EQ. 0) WRITE(*,*)

        WRITE(25,*)

C  WRITE IERRMOD AND GAMLAM TO FILE 25. NOTE THAT FOR IERRMOD = 1,
C  GAMMA WAS NOT ESTIMATED, SO USE A VALUE OF -99 FOR GAMMA.

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


C  IF NRANFIX .GT. 0, CALL ELDERY TO GET UPDATED ESTIMATES FOR THESE
C  NRANFIX PARAMETERS WHICH ARE UNKNOWN BUT THE SAME FOR ALL SUBJECTS.
C  ALSO AS OF npagranfix2.f., INCLUDE THE NVAR RANDOM VARIABLES AS 
C  PARAMETERS WHOSE NEW ESTIMATES WILL BE FOUND BY ELDERY. THE
C  INITIAL ESTIMATES FOR THESE PARAMETERS WILL BE THEIR CURRENT CYCLE
C  MEANS. NOTE THAT THIS WILL ONLY BE DONE AT END OF CYCLE NO. 1.

C  PREPARE TO CALL ELDERY. 

C  SET EACH RANDOM PARAMETER = ITS MEAN VALUE AND EACH FIXED PARAMETER
C  = ITS VALUE IN VALFIX.

      DO I = 1,NVAR
       X(I) = EX(I)
      END DO

C      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,RANFIXEST,PX)
      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,RANFIXEST,PCOPY)
        NPX = NVAR+NOFIX+NRANFIX
C       if (NPX < max_ODE_params) then exit progam w/error
      IPAR(i_skip_ig)=1
C wmy2018.03.07 -- Send local variable to MAKEVEC, then update COMMON
C   block variables P and PX
        DO I=1,max_ODE_params
          PX(I) = PCOPY(I)
          P(I) = PCOPY(I)
        END DO

C  NOTE THAT PX NOW INCLUDES THE VALUES OF THE RANDOM PARAMETERS (SET
C  = TO THEIR MEANS FROM THE JUST COMPLETED CYCLE) AND THE FIXED 
C  PARAMETER VALUES. AND IT ALSO HAS THE VALUES IN RANFIXEST(.) IN THE
C  APPROPRIATE NRANFIX ENTRIES (I.E., FOR THOSE PARAMETERS WITH 
C  IRAN(.) = 2), BUT THESE LAST VALUES OF COURSE WILL BE RESET IN 
C  SUBROUTINE CALCRF EACH TIME IT IS CALLED BY ELDERY WITH ANOTHER SET 
C  OF VALUES SUPPLIED IN THE CANDIDATE VECTOR, VEC(.). AND, AS OF
C  npagranfix2.f., THE NVAR PARAMETER VALUES FOR THE RANDOM VARIABLES
C  WILL ALSO BE RESET IN SUBROUTINE CALCRF TO THEIR CANDIDATE VALUES
C  FROM VEC(.).

C  TO START THE PROCESS TO FIND THE BEST ESTIMATES FOR THE NRANFIX
C  PARAMETERS WITH IRAN(.) = 2, SINCE THE CURRENT ESTIMATES FOR THESE 
C  PARAMETERS ARE IN RANFIXEST(.), I=1,NRANFIX, THESE WILL BE THE 
C  STARTING ESTIMATES FOR THIS CALL TO ELDERY.

C  ALSO, AS OF npagranfix2.f., THE INITIAL ESTIMATES FOR THE NVAR
C  RANDOM VARIABLES WILL BE THE MEANS FROM THE JUST COMPLETED CYCLE.

       DO I = 1,NRANFIX
        START(I) = RANFIXEST(I)
        STEP(I) = -.2D0*START(I)
       END DO

       DO I = NRANFIX+1,NRANFIX+NVAR
        START(I) = EX(I-NRANFIX)
        STEP(I) = -.2D0*START(I)
       END DO

C       write (*,*) "CALL ELDERY(...",START

C wmy20190722 -- /TOCALC/ should be passed to CALCRF via ELDERY
C  COMMON/TOCALC/gamma,flat,AB,PX,IRAN,NOFIX,NSUB

       CALL ELDERY(NRANFIX+NVAR,START,OPTVAR,VALMIN,1.D-10,STEP,1000,
     1  CALCRF,0,ICONV,NITER,ICNT,NUMEQT,YO,C0,C1,C2,C3,C4,C5,NBCOMP,
     2  NDIM,MF,RTOL,ATOL,PCOPY,TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,INTLIST,
     3  IPAR,ObsError,RPAR,gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     4  errfilname)


C  OPTVAR(I),I=1,NRANFIX+NVAR = THE UPDATED SET OF ESTIMATES FOR THE
C    NRANFIX PARAMETERS WITH IRAN(.) = 2, AND THE NVAR PARAMETERS WITH

C       write (*,*) "Ret. from ELDERY()"
C       write (*,*) "IRAN", IRAN
C       write (*,*) OPTVAR

C    IRAN(.) = 1 (SEE ABOVE). 

C  VALMIN = MIN. VALUE OF FUNCTION ACHIEVED.

C  ICONV = 1 IF THE ESTIMATE CONVERGED; 0 OTHERWISE.


        IF(ICONV .EQ. 0 .AND. ISUPRES .EQ. 0) WRITE(*,9011) 
        IF(ICONV .EQ. 0) WRITE(25,9011) 
 9011 FORMAT(' ',' NO CONVERGENCE THIS CYCLE ON ESTIMATES FOR THE'/
     1' RANFIX AND RANDOM PARAMETERS. '/)
 
 
C  ESTABLISH THE NEW SET OF RANFIX AND RANDOM VARIABLES.

       DO I = 1,NRANFIX
        RANFIXEST(I) = OPTVAR(I)
       END DO

       DO I = NRANFIX+1,NRANFIX+NVAR
        EXO(I-NRANFIX) = OPTVAR(I)
       END DO


C  RESET THE VALUES IN CORDEN TO REFLECT THE FACT THAT THE MEANS OF
C  THE NVAR RANDOM VARIABLES SHOULD NOW BE EXO(.) RATHER THAN EX(.).
C  SO 

       DO J = 1,NVAR
        DO IG = 1,NACTVE
         CORDEN(IG,J) = CORDEN(IG,J)*EXO(J)/EX(J)
        END DO
       END DO


C  WRITE THE ESTIMATES FOR THESE NRANFIX RANFIX PARAMETERS FOR THIS 
C  CYCLE, AS WELL AS FOR THE REVISED NVAR RANDOM PARAMETERS.


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
C  ABOVE ENDIF IS FOR THE  IF(NRANFIX .GT. 0 .AND. NRANFIX .EQ. 1)
C    CONDITION.




        IF(MAXCYC .EQ. 0) GO TO 900

C  Starting with bigmlt1.f, this is a jump point.


cend statistics

        write (*,*) "Begin control"

cbegin control
c we are now done wtih statistics - this is the best place to
c check for whether we can exit - if so , last printed statistic
c will agree with current density corden, and corden is still
c correct (e.g. after condensation-expansion, it is no longer
c correct until we call emint again)
cint.9 control section to check for exit criteria, resolution 
c  refinement, and end of major cycles

cint9.a  first, we exit if we have reached maxcyc on cycle counter

C  SET IMAXCYC = 0; IF IT CHANGES TO 1, IT MEANS THAT MAXCYC CYCLES
C  HAVE BEEN RUN, AND THE PROGRAM WILL STOP.

        IMAXCYC = 0

      if(icycle .ge. maxcyc) then


C  IF ICONTIN = 0, IT MEANS THE USER EXECUTED THE BATCH FILE,
C  CHMAXCYC.BAT TO STOP THE PROGRAM PREMATURELY (BY RESETTING MAXCYC
C  TO BE = THE CURRENT VALUE OF ICYCLE). IN THIS CASE, WRITE A
C  COMMENT TO FILE 25 AND TO THE SCREEN.

        IF(ICONTIN .EQ. 0) THEN
         WRITE(25,1261) MAXCYC,MAXCYC0

 1261    FORMAT(/' THE USER CHOSE TO STOP THE PROGRAM AT CYCLE NO. '/
     1' ',I7,' ... THE ORIGINAL NO. OF MAXIMUM CYCLES WAS ',I7//)
         WRITE(*,1261) MAXCYC,MAXCYC0
C        CALL PAUSE
        ENDIF

C  SET IMAXCYC = 1 --> MAXCYC WAS REACHED.

        IMAXCYC = 1

C  COMMENT OUT THE GO TO 900 STATEMENT BELOW SINCE EVEN IF ICYCLE
C  = MAXCYC, THE PROGRAM STILL NEEDS TO TEST TO SEE IF CONVERGENCE
C  WAS ACHIEVED IN THE FINAL CYCLE.
C      go to 900

      endif

c  The above endif is for the  if(icycle .ge. maxcyc)  condition.


C  SET ICONVERG = 0; IF IT CHANGES TO 1, IT MEANS THAT CONVERGENCE HAS
C  BEEN ACHIEVED, AND THE PROGRAM WILL STOP.

        ICONVERG = 0

cint9.b  second, we check improvement from last cycle




      ximprove=fobj-prefobj

       XVERIFY(1) = fobj1
       XVERIFY(2) = gamma*flat
       XVERIFY(3) = resolve
       CALL VERIFYVAL(3,XVERIFY)



C      write(91,9191) icycle,fobj1,gamma*flat,resolve,
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

cint9.c if ximprove is too low, refine the resolve criterion

      if(dabs(ximprove) .le. tol .and. resolve .gt. 0.0001) then
        resolve=resolve*0.5D0
        rpar(k_resolve) = resolve
      endif

cint9.d check to see if resolve bottoms out - if so, start a new
c major cycle by resetting it to its highest allowable value, or


c exit if the improvment from the last major cycle is too small ...

C  AND EXIT IF IMAXCYC = 1 (SEE ABOVE; THIS MEANS THAT THE MAX. NO.
C  OF CYCLES HAS ALREADY BEEN RUN AND THE ONLY REASON THIS PART OF THE
C  CODE IS BEING RUN IS TO SEE IF CONVERGENCE WAS ACHIEVED IN THE FINAL
C  CYCLE.


C      write (*,*) "Checking resolve"

      if(resolve.le.0.0001) then


c      saveres = resolve
        resolve=0.20000000298023224
        rpar(k_resolve) = 0.20000000298023224
        checkbig = fobj - prebig
      write(91,*) 'res set to .2 ',' checkbig=',checkbig
        prebig =fobj

C  PRINT OUT THE ONE LINE OF INFO IF ISUPRES = 1. NOTE THAT THE 
C  CONVERGENCE CRITERION IS THAT DABS(CHECKBIG) .LE. TOLC. ALSO NOTE
C  THAT XMED(I) ARE THE MEDIANS FROM THE JUST COMPLETED CYCLE.

         IF(ISUPRES .EQ. 1) THEN



         WRITE(*,1023) ICYCLE
 1023    FORMAT(/' FOR CYCLE NO, ',I6,' THE CONVERGENCE CRITERION AND ME
     1DIANS ARE: ')

       XVERIFY(1) = checkbig
       XVERIFY(2) = TOLC
       CALL VERIFYVAL(2,XVERIFY)



C      WRITE(*,1024) DABS(checkbig),TOLC
       WRITE(*,1024) DABS(XVERIFY(1)),XVERIFY(2)
 1024    FORMAT(1X,G14.4,' <-- CONVERGENCE OCCURS WHEN THIS NO. < ',F20.
     117)
       WRITE(*,5104) (PAR(I),I=1,NVAR)

       DO I = 1,NVAR
        XVERIFY(I) = XMED(I)
       END DO
       CALL VERIFYVAL(NVAR,XVERIFY) 
     



C      WRITE(*,5103) (XMED(I),I=1,NVAR)
       WRITE(*,5103) (XVERIFY(IXV),IXV=1,NVAR)



        ENDIF

c  As of npageng23.f, the tolerance value against which checkbig is
c  compared is now longer hardcoded to be .01. Instead it is input
c  from npag102.inp (now npag103.inp) as TOLC.

        if(dabs(checkbig) .le. TOLC) then

C  SET ICONVERG = 1 --> CONVERGENCE WAS ACHIEVED.

        ICONVERG = 1

          go to 900

        endif

      endif

c  above endif is for the  if(resolve .le. .0001)  condition.


C  IF IMAXCYC = 1, THE MAX. NO. OF CYCLES HAVE ALREADY BEEN RUN -->
C  GO TO 900. THE ONLY REASON THIS PART OF THE CODE WAS BEING RUN IS TO


C  SEE IF CONVERGENCE WAS ACHIEVED IN THIS FINAL CYCLE, AND THAT WAS
C  JUST TESTED ABOVE (SEE ICONVERG CODE).
 
        IF(IMAXCYC .EQ. 1) GO TO 900


cend control



cbegin expansion ! Replaced w/expand_grid()

      alg_type = 0
      isupres = 0
      call expand_grid(alg_type,isupres,nvar,nactve,ngridn,
     1    resolve, corden, ab)
      isupres = 0

C------ replaced code ---
c
c          write (*,*) "Begin expansion"
c
c      IF(ISUPRES .EQ. 0) write(6,*) 'Number of active points =', nactve
c now add more points near the current solution
c
c      IF(ISUPRES .EQ. 0) write(6,*)
c      IF(ISUPRES .EQ. 0) 
c     1 write(6,*) 'expanding current grid with new points'
c      IF(ISUPRES .EQ. 0) write(6,5200) 100.*resolve
c
c 5200 format(' current grid resolution = ',f8.3, '%')
c         new=2*nvar+1
c
c         nactveold=nactve
c
c         do ipoint=1,nactveold
c first, divide current probability into 2*nvar+1 pieces
c
c           pcur=corden(ipoint,nvar+1)/(2*nvar+1)
c update original point
c           corden(ipoint,nvar+1)=pcur
c
c             do ivar=1,nvar
c               del=(ab(ivar,2)-ab(ivar,1))*resolve
c create first new trial point at -eps in coordinate ivar
c               do i=1,nvar
c                  corden(nactve+1,i)=corden(ipoint,i)
c               enddo
c               corden(nactve+1,ivar)=corden(nactve+1,ivar)-del
c               corden(nactve+1,nvar+1)=pcur
c               ntry=nactve+1
c icheck that new point is at least minimally distant from old points
c
c               call checkd(corden,ntry,nactve,ab,maxgrd,nvar,iclose)
c only keep trial lower point if it lies above lower bound and satisfies
c minimal distance requirement
c             if(corden(nactve+1,ivar).ge.ab(ivar,1)) then
c
c                if(iclose.eq.0) nactve=nactve+1
c              endif
c now create second trail point at +eps in coordinate ivar
c               do i=1,nvar
c                 corden(nactve+1,i)=corden(ipoint,i)
c              enddo
c              corden(nactve+1,ivar)=corden(nactve+1,ivar)+del
c               corden(nactve+1,nvar+1)=pcur
c only keep upper point if it lies below upper bound and
c satisfies distance requirement
c               ntry=nactve+1
c               call checkd(corden,ntry,nactve,ab,maxgrd,nvar,iclose)
c           if(corden(nactve+1,ivar).le.ab(ivar,2)) then
c
c             if(iclose.eq.0) nactve=nactve+1
c
c               endif
c             enddo
c    above enddo for loop over ivar=1,nvar
c
c           enddo
c    above enddo for loop over ipoint=1,nactveold
c
c      IF(ISUPRES .EQ. 0) 
c     1 write(6,*) 'Number of actve grid points after expansion =',nactve
c      ngridn=nactve
c      IF(ISUPRES .EQ. 0) write(6,*)
c
cend expansion
c
C------ end replaced code ---

c go to begin new cycle

        prefobj=fobj


        GO TO 1001
 
  900 continue

C       write (*,*) "Passed 900"

c  As of npagranfix2.f, write the estimates for the parameters which are
c  unknown but the same for all subjects, if applicable. Note that 
c  these estimates were obtained only after cycle no. 1 and are now
c  written out again at the end of the run.

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
C  THE ABOVE ENDIF IS FOR THE  IF(NRANFIX .GT. 0)  CONDITION.



C  AS OF npageng18.f, CONTROL CAN BE TRANSFERRED TO LABEL 900 DIRECTLY
C  AFTER RETURNING FROM A CALL TO SUBROUTINE emint. THIS HAPPENS WHEN
C  IHESS = -1, WHICH MEANS THAT THE HESSIAN MATRIX IN THE INTERIOR
C  POINT EM ALGORITHM WAS SINGULAR. RATHER THAN SIMPLY STOPPING AS IT
C  DID PREVIOUSLY, NOW THE PROGRAM WILL CREATE THE OUTPUT FILES BEFORE
C  STOPPING ... BASED ON THE VALUES FROM THE PREVIOUS CYCLE.
C  FIRST, WRITE THE REASON FOR STOPPING AS ICONVERGE = 3 BELOW. THEN
C  RESET CORDEN BACK TO CORDLAST (SEE ABOVE), WHICH WAS THE CORDEN
C  AT THE END OF THE PREVIOUS CYCLE.


C  WRITE WHY THE PROGRAM STOPPED.

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


C  Starting with bigmlt1.f, this is an entry point to continue 
c  calculations


  910 CONTINUE

C        write (*,*) "Passed 910"

cbegin endgame
c we can only arrive here from the control section, which menas
c that we ahve completed optimizaiton but not done the subsequent
c expansion.  This means that the density is correct, and we can safely
c just write it out and exit


C  SINCE THE ANALYSIS IS OVER:
C  ESTABLISH THREE VECTORS, YPREDPOP, YPREDPOPT, AND YPREDBAY, WHICH 
C  WILL BE STORED INTO THE DENSITY FILE FOR USE BY THE PC PROGRAM.
 
C  ESTABLISH YPREDPOP(MAXSUB,NUMEQT,MAXOBS,3), WHERE

C  YPREDPOP(JSUB,J,IOBS,ICEN) = THE PREDICTED VALUE FOR SUBJECT
C  JSUB, OUTPUT EQ. J, OBSERVATION IOBS, AND ICEN (ICEN = 1 (MEANS),
C  2 (MEDIANS), AND 3 (MODES), WHERE THE MEANS, MEDIANS, AND MODES ARE
C  FROM THE FINAL CYCLE POPULATION DENSITY).

C  ESTABLISH YPREDPOPT(MAXSUB,NUMEQT,7201,3), WHERE

C  YPREDPOPT(JSUB,J,T,ICEN) = THE PREDICTED VALUE FOR SUBJECT
C  JSUB, OUTPUT EQ. J, AT TIME T, AND ICEN (ICEN = 1 (MEANS),
C  2 (MEDIANS), AND 3 (MODES), WHERE THE MEANS, MEDIANS, AND MODES ARE
C  FROM THE FINAL CYCLE POPULATION DENSITY), AND T IS A VALUE IN
C  TPRED, ESTABLISHED IN THE CALL TO CALCTPRED - SEE BELOW.

C  ESTABLISH YPREDBAY(MAXSUB,NUMEQT,MAXOBS,3), WHERE
C  YPREDBAY(JSUB,J,IOBS,ICEN) = THE PREDICTED VALUE FOR SUBJECT
C  JSUB, OUTPUT EQ. J, OBSERVATION IOBS, AND ICEN (ICEN = 1 (MEANS),
C  2 (MEDIANS), AND 3 (MODES), WHERE THE MEANS, MEDIANS AND MODES ARE
C  FROM SUBJECT'S JSUB BAYSESIAN POSTERIOR DENSITY (CALCULATED BY
C  SUBROUTINE SUBRES)).
 

C  ALSO, ESTABLISH PREDICTED VALUES SIMILAR TO YPREDBAY, BUT WHICH WILL
C  BE AT TIMES SPECIFIED BY IDELTA (SEE SUBROUTINE CALCTPRED) AND ONLY 
C  AT ONE SET OF PARAMETER VALUES (MEANS, MEDIANS, OR MODES OF THE 
C  BAYESIAN POSTERIOR DIST.) SPECIFIED BY ICENT (SEE DETAILS IN TOP OF 
C  CODE FOR m2_8calc.f). THESE VALUES WILL BE PUT INTO FILE PREDFIL. IN 
C  ADDITION, CALCULATE TOTAL AND 24 HOUR AUC'S AND AUC/MIC'S, WHICH WILL 

C  BE PUT INTO THE OUTPUT FILE, ACCOMPANYING EACH SUBJECT'S BAYESIAN 
C  POSTERIOR INFO.
C!! NO. AS OF npageng25.f, BOTH THE PREDICTED VALUES PUT INTO PREDFIL,
C  AND THE AUCs WILL BE CALCULATED FOR ALL OF THE MEANS, MEDIANS, AND
C  MODES (I.E., ICENT IS NO LONGER USED).

 
C 1ST CALCULATE YPREDPOP AND YPREDPOPT VALUES.
 
         REWIND(27)

 
C  FOR EACH SUBJECT, FIND THE PREDICTED VALUES (VIA SUBROUTINE IDCALCY
C  FOR YPREDPOP, AND VIA IDCALCYY FOR YPREDPOPT).
 
C  NOTE THAT THE LAST CYCLE'S MEANS ARE IN   CENTER(1,J), J = 1,NVAR;
C                             MEDIANS ARE IN CENTER(2,J), J = 1,NVAR;
C                             MODES ARE IN   CENTER(3,J), J = 1,NVAR.
 

C wmy2018.10.16 Moved CALL SYMBOL out of IDCALCY and IDCALCYY (similar
C  to moving CALL SYMBOL out of IDPC -- I'm not really convinced that
C  this routine does anything. I believe it's original purpose was to
C  "relate" the parameter names in the model file to parameters in a
C  support.  But I can't find reference to the PSYM(:) array, that 
C  these names are written. SYMBOL also is supposed to initialize the
C  NBCOMP array, which tells NPAG where bolus of a drug goes; but can't
C  figure out how this happens.
C        CALL SYMBOL(NBCOMP) ... moved to CALL IDCALCY and CALL IDCALCYY, below

        IPAR(i_do) = 6000
        DO 6000 JSUB=1,NSUB

        write (*,*) "DO 6000", JSUB 
 
        CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,
     1    TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     2    INTLIST,ERRFILNAME)

        call cp_lrcs_to_rpar(gamma,flat,numeqt,c0,c1,c2,c3,c4,c5,rpar)

C        write (*,*) "Ret. from FILRED() nr. #5700"

         DO ICENTER = 1,3

         DO J=1,NVAR
           EXXX(J) = CENTER(ICENTER,J)
         END DO

C  FIND YPREDPOP FIRST:  
C  CALL SUBROUTINE IDCALCY, A VERSION OF THE ID PROGRAM WHICH SIMPLY
C  CALCULATES THE PREDICTED VALUES OF YO(I,J) = OUTPUT CONCENTRATION OF
C  THE JTH OUTPUT EQUATION (J=1,NUMEQT) AT THE ITH OBSERVATION TIME
C  (I=1,NOBSER), ASSUMING THE PARAMETER VECTOR EXXX ...
 

C  BEFORE ALL CALLS TO IDCALCY, MUST INTEGRATE FIXED AND RANDOM
C  VALUES INTO PX, USING IRAN(I),I=1,NVAR+NOFIX. CALL MAKEVEC TO DO
C  THIS. AS OF npagranfix.f, ADD NRANFIX AND RANFIXEST
C  TO THE ARGUMENT LIST TOO (I.E., THERE IS NOW A 3RD TYPE OF 
C  PARAMETER (THOSE WHICH ARE UNKNOWN BUT THE SAME FOR ALL SUBJECTS).

C      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,EXXX,VALFIX,RANFIXEST,PX)
      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,EXXX,VALFIX,RANFIXEST,PCOPY)
        NPX = NVAR+NOFIX+NRANFIX
C       if (NPX < max_ODE_params) then exit progam w/error
      IPAR(i_skip_ig)=1

C wmy2018Jan12 I removed the COMMON/PARAMD/P from SUBROUTINE IDCALCY and
c  IDCALCYY, so P must be initialized in main after calling MAKEVEC, but
c  before calling them. Note: IDCALCY and -YY now call SYMBOL and then 
c  immediately call FUNC2() or FUNC3(). 

C wmy2018.03.07 -- Send local variable to MAKEVEC, then update COMMON
C   block variables P and PX
        DO I=1,NPX
          PX(I) = PCOPY(I)
           P(I) = PCOPY(I)
        END DO

c        DO I=1,NPX
c          P(I) = PX(I)
c        END DO
C In above DO, NVAR+NOFIX+NRANFIX is replaced by NPX
       
C         write (*,*) JSUB,"DO 6000 Calling IDCALCY", NVAR+NOFIX+NRANFIX
c     1     ,NDIM
c     2      ,PX(21),YPRED(1,2),NUMEQT,NOBSER,NDIM,MF,RTOL,ATOL,MF
c         write (*,*) "Where did all the 0.0s come from?"

C wmy2017Sep13 -- printed values of the above look OK; but are followed
C   by a list of sixteen 0.00s 

C        CALL IDCALCY(NVAR+NOFIX+NRANFIX,NDIM,PX,YPRED,NUMEQT)
C       real*8, save, dimension(max_doses,max_RS_J) :: RSCOPY
        rrr = 0.0
        do III=1,max_doses
          do JJJ =1,max_RS_J
            rrr = rrr + rscopy(III,JJJ) - rs(III,JJJ) 
            rscopy(iii,jjj) = rs(iii,jjj)
            if (jjj.le.max_input_dim) bscopy(iii,jjj) = bs(iii,jjj)
          end do
        end do
C        write (*,*) "calling IDCALCY with rrr=",rrr

        CALL SYMBOL(NBCOMP)

        CALL IDCALCY(JSUB,IG,NPX,NDIM,PCOPY,YPRED,NUMEQT,
     1      NOBSER,MF,NBCOMP,RTOL,ATOL,
     2      TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     3      INTLIST,IPAR,ObsError,RPAR,ERRFILNAME)

C wmy2017Jan12 IDCALC receives params as ...
c        SUBROUTINE IDCALCY(JSUB,IG,NPP,NDIM,ESTML,YPRED,NUMEQT
c     1    ,NOBSER,MF,RTOL,ATOL,RSCOPY,BSCOPY,INTLIST,IPAR,ObsError)


C         write (*,*) "DO 6000 Return from IDCALCY", JSUB, ICENTER

C  NOTE: PREDICTED VALUES WERE FOUND EVEN FOR OBSERVED LEVELS WHICH ARE
C  MISSING (I.E., OBSERVED LEVEL = -99) SINCE IT IS EASIER TO CALCULATE
C  ALL PREDICTED VALUES THAN TO KEEP TRACK OF WHICH DO AND WHICH DON'T
C  NEED TO BE CALCULATED.
 
C  STORE YPRED INTO YPREDPOP(JSUB,.,.,ICENTER)
 
         DO IOBS=1,NOBSER
         DO IEQ=1,NUMEQT
           YPREDPOP(JSUB,IEQ,IOBS,ICENTER) = YPRED(IOBS,IEQ)
         END DO
         END DO



C  FIND YPREDPOPT: 
C  CALL SUBROUTINE IDCALCYY, A VERSION OF THE ID PROGRAM WHICH SIMPLY
C  CALCULATES THE PREDICTED VALUES OF Y(I,J) (OUTPUT CONCENTRATION
C  OF THE JTH OUTPUT EQ. AT TIME TPRED(I),I=1,NUMT(JSUB)), ASSUMING THE
C  PARAMETER VECTOR EXXX. NOTE THAT IDCALCYY DIFFERS FROM IDCALCY IN
C  THAT THE TIMES FOR THE PREDICTED VALUES ARE SUPPLIED IN TPRED, 
C  RATHER THAN INPUT VIA COMMON/OBSER FROM THE PATIENT'S DATA FILE. 
C  ALSO, THE NO. OF OBSERVED TIMES IS NUMT(JSUB), RATHER THAN M WHICH IS 
C  SUPPLIED VIA COMMON/SUM2. AND NOTE THAT NUMT(JSUB) AND TPRED(.) ARE 
C  FOUND FROM THE CALL TO CALCTPRED BELOW.

C  CALL CALCTPRED TO CALCULATE THE NUMT(JSUB) TIMES TO BE IN TPRED FOR 
C  THIS SUBJECT. NOTE THAT, AFTER THE CALL TO FILRED ABOVE FOR THIS 
C  SUBJECT, THE NO. OF OBSERVED VALUE TIMES = NOBSER, AND THESE VALUES 
C  ARE IN ARRAY, TIMOB.

C  AS OF npageng16.f, TIMOBB IS NO LONGER NEEDED AS AN ARGUMENT TO
C  CALCTPRED, SINCE NOW CALCTPRED HAS COMMON/OBSER (WHICH PROVIDES
C  TIMOB) IN IT.

C  AS OF npageng18.f, CALCTPRED ALSO RETURNS TPREDREL, WHICH GIVES THE
C  "RELATIVE" RATHER THAN "REAL" TIMES AFTER A STEADY STATE DOSE SET.
C  THESE VALUES ARE THE ONES WHICH WILL BE WRITTEN TO THE OUTPUT FILES
C  FROM NOW ON.


      CALL CALCTPRED(JSUB,IDELTA,NOBSER,NUMT(JSUB),TPRED,TPREDREL,
     1   NOMAXTIM(JSUB),TEND,TBEGG,TIMCOPY,SIGCOPY,INTLIST)

C         write (*,*) "Return from CALCTPRED", JSUB, ICENTER

C  STORE THE TIMES FOR THE PREDICTED CONCENTRATIONS INTO TTPRED. THEY
C  WILL BE NEEDED BELOW. ALSO STORE THE MAXIMUM ENDING TIME + 24 HOURS 
C  (TEND), AND BEGINNING TIME (TBEGG) FOR THIS SUBJECT ... FOR EACH
C  TIME RESET AND THE OVERALL MAXIMUM TIME. THESE VALUES WILL BE NEEDED
C  WHEN SUBJECT AUCs ARE CALCULATED BELOW.

C  ALSO, AS OF npageng18.f, STORE THE "RELATIVE" TIMES INTO TTPREDREL
C  (TTPRED STORES THE "REAL" TIMES). THESE ARE THE VALUES WHICH WILL BE
C  WRITTEN TO THE OUTPUT FILES.

        DO J = 1,NUMT(JSUB)
          TTPRED(JSUB,J) = TPRED(J)
          TTPREDREL(JSUB,J) = TPREDREL(J)
        END DO

        DO J = 1,NOMAXTIM(JSUB)
          TENDSUB(JSUB,J) = TEND(J)
          TBEGGSUB(JSUB,J) = TBEGG(J)
        END DO


C  BEFORE ALL CALLS TO IDCALCYY, MUST INTEGRATE FIXED AND RANDOM
C  VALUES INTO PX, USING IRAN(I),I=1,NVAR+NOFIX. CALL MAKEVEC TO DO
C  THIS. AS OF npagranfix.f, ADD NRANFIX AND RANFIXEST
C  TO THE ARGUMENT LIST TOO (I.E., THERE IS NOW A 3RD TYPE OF 
C  PARAMETER (THOSE WHICH ARE UNKNOWN BUT THE SAME FOR ALL SUBJECTS).

      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,EXXX,VALFIX,RANFIXEST,PCOPY)
        NPX = NVAR+NOFIX+NRANFIX
C       if (NPX < max_ODE_params) then exit progam w/error
      IPAR(i_skip_ig) = 1

C wmy2018Jan12 I removed the COMMON/PARAMD/P from SUBROUTINE IDCALCY and
c  IDCALCYY, so P must be initialized in main after calling MAKEVEC, but
c  before calling them. Note: IDCALCY and -YY now call SYMBOL and then
c  immediately call FUNC2() or FUNC3(); bypassing SUBROUTINES EVAL2 and -3
C        DO I=1,NVAR+NOFIX+NRANFIX
        DO I=1,NPX
          PX(I) = PCOPY(I)
           P(I) = PCOPY(I)
        END DO

c      write (*,*) "CALL IDCALCYY", NVAR+NOFIX+NRANFIX,NDIM,PX(8),
c     1  TPRED(5),NUMT(JSUB),NUMEQT,NOBSER,MF,RTOL,ATOL(1)

C wmy2017Sep20 -- call from the original npagranfix.f
C        CALL IDCALCYY(NVAR+NOFIX+NRANFIX,NDIM,PX,TPRED,NUMT(JSUB),
C     1  YYPRED,NUMEQT)

C       CALL IDCALCYY(JSUB,IG,NVAR+NOFIX+NRANFIX,NDIM,PX,TPRED,
        rrr = 0.0
        do III=1,max_doses
          do JJJ =1,max_RS_J
            rrr = rrr + rscopy(III,JJJ) - rs(III,JJJ) 
            rscopy(iii,jjj) = rs(iii,jjj)
            if (jjj.le.max_input_dim) bscopy(iii,jjj) = bs(iii,jjj)
          end do
        end do
C        write (*,*) "calling IDCALCYY with rrr=",rrr

        CALL SYMBOL(NBCOMP)

        CALL IDCALCYY(JSUB,IG,NPX,NDIM,PCOPY,TPRED,
     1      NUMT(JSUB), YYPRED,NUMEQT,NOBSER,MF,NBCOMP,RTOL,ATOL,
     2      TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     3      INTLIST,IPAR,ObsError,RPAR,ERRFILNAME)

C        write (*,*) "#5789 Return from IDCALCYY: JSUB, ICENTER",
C     1      JSUB, ICENTER

C  STORE YYPRED INTO YPREDPOPT(JSUB,.,.,ICENTER)
 
         DO J=1,NUMT(JSUB)
         DO IEQ=1,NUMEQT
           YPREDPOPT(JSUB,IEQ,J,ICENTER) = YYPRED(J,IEQ)
         END DO
         END DO

        END DO
 
C  THE ABOVE END DO IS FOR THE   DO ICENTER = 1,3  LOOP.

C         write (*,*) "DO 6000 done for ", JSUB 
 
 6000   CONTINUE
 
        write (*,*) "Passed 6000" 
 
C NEXT CALCULATE YPREDBAY AND PREDICTED VALUES AT SPECIFIED TIMES.
 
        REWIND(27)
 
C  FOR EACH SUBJECT IN TURN, FIND THE BAYESIAN POSTERIOR P.D.F., GIVEN
C  THE FINAL JOINT P.D.F. OF THE ENTIRE POPULATION (IN CORDEN) AS THE
C  PRIOR. THEN FIND THE PREDICTED VALUES FOR THE MEANS, MEDIANS, AND
C  MODES OF THIS BAYESIAN P.D.F. (YPREDBAY), AS WELL AS THE PREDICTED
C  VALUES AT THE TIMES SPECIFIED IN TPRED FOR THE MEANS, MEDIANS, OR
C  MODES (SEE ICENT) OF THIS BAYESIAN P.D.F.
C!! NO. AS OF npageng25.f, ICENT IS NO LONGER USED. THE PREDICTED
C  VALUES WILL BE FOUND FOR ALL OF THE MEANS, MEDIANS, AND MODES.

  
C  STORE NACTVE AND CORDEN INTO NNACTVE AND CORHOLD, RESPECTIVELY.
C  THEY MUST BE RESET BEFORE EACH CALL TO SUBRES (WHICH ALTERS THEM).
 
        NNACTVE=NACTVE

 
        DO I=1,NACTVE
        DO J=1,NVAR+1
          CORHOLD(I,J) = CORDEN(I,J)
        END DO
        END DO
 
C  OPEN THE PREDICTED CONCENTRATION FILE, PREDFIL (ESTABLISHED ABOVE
C  AS PRTBxxxx, WHERE xxxx IS THE RUN NO. WRITE EACH SUBJECT NO, EACH
C  FOLLOWED BY NUMT(JSUB) ROWS OF TPRED AND PREDICTED VALUES FOR EACH
C  OF THE MEANS, MEDIANS, AND MODES OF THE BAYESIAN POSTERIOR DENSITY.

        OPEN(31,FILE=PREDFIL)
 
C------------------------------------------------------- 7000 
       write (*,*) "Starting 7000"

C wmy2018.10.16 Moved CALL SYMBOL out of IDCALCY and IDCALCYY (see
C  long comment at DO 6000)
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

C        write (*,*) "Ret. from FILRED() nr. #5950"

C  CALCULATE SIGFAC AND OFAC FOR THIS SUBJECT. SEE COMMENTS IN LOOP 140
C  ABOVE.

        MISVAL = 0
 
        SIGFAC=1.D0
 
        DO 240 I=1,NOBSER
          DO 240 J=1,NUMEQT
 
            Y = YO(I,J)
 
            IF(Y .EQ. -99) THEN
              MISVAL = MISVAL+1
              GO TO 240
            ENDIF

C Are observations recorded as log10(obs) _AND_ in subroutine
C output, are X converted to log10(X)? If so, do you want the
C sd to be calculated on 10^Y(obs or est)?
C             if (C1(J).eq.-10) IPAR(i_is_log10+J) = -10 
             if (C4(J).eq.10) IPAR(i_is_log10+J) = 10 

C wmy2018.06.26 -- wmy added the Poisson initialization to the end-game
C          if (C0(J).eq.-229.and.C2(J).eq.-229
C     1   .and.C3(J).eq.-229) then
           if (C5(J).eq.229) then
C--------------------------------- Start Poisson

             write (*,*) "Poisson analysis req. for OUTEQ",J
C             NPOISSONOBS=NPOISSONOBS+1
             ObsError(I,J)=1.D0
             SIG(I,J)=1.D0
             IPAR(i_is_poisson_obs+J)=229

C--------------------------------- End Poisson
          else  
C--------------------------------- Start NORMAL
 
            SIG(I,J) = C0(J)+C1(J)*Y+C2(J)*Y*Y+C3(J)*Y**3
cgam4
            if(ierrmod.eq.2) sig(i,j) = sig(i,j)*gamma
            if(ierrmod.eq.3) sig(i,j)=dsqrt(sig(i,j)**2 + gamma**2)
            if(ierrmod.eq.4) sig(i,j) = gamma*flat

            ObsError(I,J) = sig(I,J)

C--------------------------------- End NORMAL

C wmy2018.06.26 -- Following test should only be relevant if (I,J) is a 
C   Normal draw.
            IF(SIG(I,J) .LT. 0) THEN
              WRITE(*,2346) JSUB
              WRITE(25,2346) JSUB

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

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

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

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

C        write (*,*) "DO 7000 (",IPAR(i_do),") Jsub, IG, sigma, ofac",
C     1    JSUB, IG, SIGFAC, OFAC, k_sfac, k_ofac


C  CALL SUBROUTINE SUBRES WHICH DOES THE ACTUAL CALCULATIONS FOR
C  THIS SUBJECT. SUBRES CALCULATES THE BAYESIAN POSTERIOR P.D.F. FOR
C  THIS SUBJECT. IT CALCULATES P(XI|Y), WHERE Y IS THE VECTOR OF
C  OBSERVED VALUES (ACTUALLY, Y IS A 2-DIM ARRAY IF THERE ARE MULTIPLE
C  OUTPUTS) FOR THIS SUBJECT, AND XI, I=1,NACTVE, ARE THE
C  REMAINING GRID POINTS FROM THE FINAL CYCLE OF THE POPULATION
C  ANALYSIS. IT ALSO RETURNS CENTER(I,J),J=1,NVAR, WHERE I = 1 -->
C  MEANS OF THE BAYESIAN POSTERIOR; I = 2 --> MEDIANS OF THE BAYESIAN
C  POSTERIOR; AND I = 3 --> MODES OF THE BAYESIAN POSTERIOR.

C  ALSO NOTE THAT SUBRES RETURNS PYJGXX IN CASE THIS RUN WAS A MAXCYC=0
C  RUN (I.E., A RUN USING A PRIOR DENSITY WITH A DIFFERENT SET OF
C  PATIENTS WHICH MEANS THAT PYJGX WAS NOT CALCULATED BECAUSE NO
C  CYCLES WERE RUN ABOVE - IN THIS CASE, PYJGX MUST BE CALCULATED 
C  IN ORDER TO BE WRITTEN INTO THE DENSITY FILE BELOW). SO PYJGX FOR
C  THIS SUBJECT WILL BE SET = PYJGXX AFTER THE CALL TO SUBRES.

C  ALSO, AS OF npageng23.f, SUBRES PASSES IN COMMON/BAY THE BAYESIAN
C  POSTERIOR DENSITY FOR EACH SUBJECT TO SUBROUTINE READOUT.

C wmy2017Sep12
      CALL SUBRES(MAXSUB,MAXACT,JSUB,CORDEN,WORK,MAXGRD,MAXDIM,NVAR,
     1  NOFIX,VALFIX,SIGFAC,OFAC,AB,PAR,NACTVE,NGRID,VOLSPA,IRAN,CENTER,
     2  PYJGXX,NRANFIX,RANFIXEST,NOBSER,NUMEQT,NBCOMP,
     3  NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     4  INTLIST,IPAR,ObsError,RPAR,ERRFILNAME)

        DO IG = 1,NACTVE
          PYJGX(JSUB,IG) = PYJGXX(IG)
        END DO
 
C  FOR THIS SUBJECT, FIND THE PREDICTED VALUES (VIA SUBROUTINE IDCALCY),
C  FOR EACH OF THE MEANS, MEDIANS, AND MODES (ICENTER = 1,2,3,
C  RESPECTIVELY).
 
C  ALSO FIND THE PREDICTED VALUES AT THE TIMES SPECIFIED IN TPRED
C  FOR THE MEANS, MEDIANS, AND MODES.

        DO ICENTER = 1,3
 
          DO J=1,NVAR
            EXXX(J) = CENTER(ICENTER,J)
            EXX(JSUB,ICENTER,J) = CENTER(ICENTER,J)
          END DO

C          write (*,*) "EXX filled for ICENTER", ICENTER
 
C  NOTE THAT THE MEANS, MEDIANS, AND MODES FOR EACH SUBJECT ARE
C  STORED INTO EXX, FOR SUBSEQUENT STORAGE INTO THE DENSITY FILE.
 
C  CALL SUBROUTINE IDCALCY, A VERSION OF THE ID PROGRAM WHICH SIMPLY
C  CALCULATES THE PREDICTED VALUES OF YO(I,J) = OUTPUT CONCENTRATION OF
C  THE JTH OUTPUT EQUATION (J=1,NUMEQT) AT THE ITH OBSERVATION TIME
C  (I=1,NOBSER), ASSUMING THE PARAMETER VECTOR EXXX ...
 
C  BEFORE ALL CALLS TO IDCALCY, MUST INTEGRATE FIXED AND RANDOM
C  VALUES INTO PX, USING IRAN(I),I=1,NVAR+NOFIX. CALL MAKEVEC TO DO
C  THIS. AS OF npagranfix.f, ADD NRANFIX AND RANFIXEST
C  TO THE ARGUMENT LIST TOO (I.E., THERE IS NOW A 3RD TYPE OF 
C  PARAMETER (THOSE WHICH ARE UNKNOWN BUT THE SAME FOR ALL SUBJECTS).

      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,EXXX,VALFIX,RANFIXEST,PCOPY)
        NPX = NVAR+NOFIX+NRANFIX
C       if (NPX < max_ODE_params) then exit progam w/error
      IPAR(i_skip_ig) = 1

C wmy2018Jan12 I removed the COMMON/PARAMD/P from SUBROUTINE IDCALCY and
c  IDCALCYY, so P must be initialized in main after calling MAKEVEC, but
c  before calling them. Note: IDCALCY and -YY now call SYMBOL and then 
c  immediately call FUNC2() or FUNC3(). 
C        DO I=1,NVAR+NOFIX+NRANFIX
        DO I=1,NPX
          PX(I) = PCOPY(I)
           P(I) = PCOPY(I)
        END DO

C      write (*,*) "Param VECs filled; CALLing IDCALCY"

        rrr = 0.0
        do III=1,max_doses
          do JJJ =1,max_RS_J
            rrr = rrr + rscopy(III,JJJ) - rs(III,JJJ) 
            rscopy(iii,jjj) = rs(iii,jjj)
            if (jjj.le.max_input_dim) bscopy(iii,jjj) = bs(iii,jjj)
          end do
        end do
C        write (*,*) "calling IDCALCY with rrr=",rrr

        CALL SYMBOL(NBCOMP)

        CALL IDCALCY(JSUB,IG,NPX,NDIM,PCOPY,YPRED,NUMEQT,
     1      NOBSER,MF,NBCOMP,RTOL,ATOL,
     2      TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     3      INTLIST,IPAR,ObsError,RPAR,ERRFILNAME)

C        write (*,*) "#5733 Returned from IDCALCY()"
 
C  NOTE: PREDICTED VALUES WERE FOUND EVEN FOR OBSERVED LEVELS WHICH ARE
C  MISSING (I.E., OBSERVED LEVEL = -99) SINCE IT IS EASIER TO CALCULATE
C  ALL PREDICTED VALUES THAN TO KEEP TRACK OF WHICH DO AND WHICH DON'T
C  NEED TO BE CALCULATED.
 
C  STORE YPRED INTO YPREDBAY(JSUB,.,.,ICENTER)

C        write (*,*) "YPRED", NOBSER, "of", MAXOBS, "updates"
 
        DO IOBS=1,NOBSER
         DO IEQ=1,NUMEQT

C         write (*,*) "YPREDBAY(,,,) update",JSUB
C     1    ,IEQ,"of",NUMEQT
C     2    ,IOBS,"of",MAXOBS
C     3    ,ICENTER

C        YPREDBAY(MAXSUB,NUMEQT,MAXOBS,3)

         YPREDBAY(JSUB,IEQ,IOBS,ICENTER) = YPRED(IOBS,IEQ)
         END DO
        END DO

        END DO
C  THE ABOVE END DO IS FOR THE   DO ICENTER = 1,3  LOOP.

        write (*,*) "YPREDBAY(,,,) update done"
 
C  NOW CALCULATE THE PREDICTED VALUES TO BE STORED INTO FILE 31.
C  NOTE THAT AS OF npageng25.f, THESE VALUES ARE CALCULATED FOR ALL
C  3 MEASURES OF CENTRAL TENDENCY (MEANS, MEDIANS, AND MODES) FROM
C  THE BAYESIAN POSTERIOR DENSITY (IN PREVIOUS PROGRAMS, THE PREDICTED
C  VALUES WERE ONLY CALCULATED FOR THE MEASURE SPECIFIED BY ICENT, BUT
C  ICENT IS NOW IRRELEVANT, EVEN THOUGH IT IS STILL PASSED TO THIS
C  MODULE FROM THE PC PREP PROGRAM).
 
C  AND NOTE AS OF npageng18.f, INSTEAD OF WRITING TPRED(.) TO FILE 31,
C  TPREREL(.) IS WRITTEN (THE LATTER GIVE THE "RELATIVE" TIMES RATHER
C  THAN THE "REAL" TIMES). ESTABLISH THESE VALUES OUTSIDE THE 
C  ICENTER = 1,3 LOOP, SINCE THEY ARE THE SAME REGARDLESS OF THE VALUE
C  OF ICENTER.

        DO I=1,NUMT(JSUB)
         TPRED(I) = TTPRED(JSUB,I)
        END DO

          write (*,*) "TPRED(I) update done"

      DO ICENTER = 1,3



       DO J=1,NVAR
        EXXX(J) = CENTER(ICENTER,J)
       END DO
 
C  CALL SUBROUTINE IDCALCYY, A VERSION OF THE ID PROGRAM WHICH SIMPLY
C  CALCULATES THE PREDICTED VALUES OF Y(I,J) (OUTPUT CONCENTRATION
C  OF THE JTH OUTPUT EQ. AT TIME TTPRED(JSUB,I),I=1,NUMT(JSUB)),ASSUMING 
C  THE PARAMETER VECTOR EXXX. NOTE THAT IDCALCYY DIFFERS FROM IDCALCY IN
C  THAT THE TIMES FOR THE PREDICTED VALUES ARE SUPPLIED IN TPRED,
C  RATHER THAN INPUT VIA COMMON/OBSER FROM THE PATIENT'S DATA FILE. 
C  ALSO, THE NO. OF OBSERVED TIMES IS NUMT(JSUB), RATHER THAN M WHICH IS 
C  SUPPLIED VIA COMMON/SUM2. AND NOTE THAT NUMT(JSUB) AND TTPRED(.,.)
C  WERE FOUND IN CALLS TO CALCTPRED ABOVE.

C  BEFORE ALL CALLS TO IDCALCYY, MUST INTEGRATE FIXED AND RANDOM
C  VALUES INTO PX, USING IRAN(I),I=1,NVAR+NOFIX. CALL MAKEVEC TO DO
C  THIS. AS OF npagranfix.f, ADD NRANFIX AND RANFIXEST
C  TO THE ARGUMENT LIST TOO (I.E., THERE IS NOW A 3RD TYPE OF 
C  PARAMETER (THOSE WHICH ARE UNKNOWN BUT THE SAME FOR ALL SUBJECTS).

      CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,EXXX,VALFIX,RANFIXEST,PCOPY)
        NPX = NVAR+NOFIX+NRANFIX
C       if (NPX < max_ODE_params) then exit progam w/error
      IPAR(i_skip_ig) = 1

C      write (*,*) "DO 7000 CALL IDCALCYY", NVAR+NOFIX+NRANFIX,NDIM,
C     1  PX(8),TPRED(5),NUMT(JSUB),NUMEQT,NOBSER,MF,RTOL,ATOL(1)

C wmy2018Jan12 I removed the COMMON/PARAMD/P from SUBROUTINE IDCALCY and
c  IDCALCYY, so P must be initialized in main after calling MAKEVEC, but
c  before calling them. Note: IDCALCY and -YY now call SYMBOL and then 
c  immediately call FUNC2() or FUNC3(). 
C        DO I=1,NVAR+NOFIX+NRANFIX
        DO I=1,NPX
          PX(I) = PCOPY(I)
           P(I) = PCOPY(I)
        END DO

C      CALL IDCALCYY(JSUB,IG,NVAR+NOFIX+NRANFIX,NDIM,PX,TPRED,
        rrr = 0.0
        do III=1,max_doses
          do JJJ =1,max_RS_J
            rrr = rrr + rscopy(III,JJJ) - rs(III,JJJ) 
            rscopy(iii,jjj) = rs(iii,jjj)
            if (jjj.le.max_input_dim) bscopy(iii,jjj) = bs(iii,jjj)
          end do
        end do
C        write (*,*) "calling IDCALCYY with rrr=",rrr

        CALL SYMBOL(NBCOMP)

      CALL IDCALCYY(JSUB,IG,NPX,NDIM,PCOPY,TPRED,
     1  NUMT(JSUB), YYPRED,NUMEQT,NOBSER,MF,NBCOMP,
     2  RTOL,ATOL,
     3  TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     4  INTLIST,IPAR,ObsError,RPAR,ERRFILNAME)

C       write (*,*) "#5800 Returned from IDCALCYY"

C  STORE YYPRED INTO YYYPRED(ICENTER,J,IEQ).

       DO J = 1,NUMT(JSUB)
        DO IEQ = 1,NUMEQT
         YYYPRED(ICENTER,J,IEQ) = YYPRED(J,IEQ)
        END DO
       END DO

      END DO
C  THE ABOVE END DO IS FOR THE  DO ICENTER = 1,3  LOOP.


 
C  STORE YYYPRED(.,.,.) INTO FILE 31.
 
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



C      WRITE(31,2167) TTPREDREL(JSUB,J),
C     1 (YYYPRED(1,J,IEQ),YYYPRED(2,J,IEQ),YYYPRED(3,J,IEQ),
C     2 IEQ=1,NUMEQT)

       WRITE(31,2167) (XVERIFY(IXV),IXV=1,1+3*NUMEQT)


 2167  FORMAT(90(G16.5,2X))



C  NOTE THAT THE MOST NOS. IN THE ABOVE FORMAT IS 1 + 3*NUMEQT. THIS 
C  WILL SURELY BE < 90, SO 90 IS SUFFICIENT.
      END DO

c  AS OF npageng21.f, TTPREDREL(JSUB,J) is written to file 31 above, 
C  rather than TPREDREL(J). Writing TPREDREL(J) was a mistake since it
C  holds the values for the last subject, not the values for each 
C  subject.


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
C  SEE ABOVE COMMENT BELOW FORMAT 2167.
        END DO

       END DO

c  Note that as of npageng18.f, instead of writing TIMOBS(.) to File 31,
c  the program writes TIMOBREL(.,.) to the file. TIMOB(IOBS) are the
c  "real" times; and TIMOBREL(JSUB,IOBS) are the "relative" times. See
c  comments regarding change 1 at top of npageng18.f code.


        WRITE(31,*)
        WRITE(31,*) 

 
C  CALCULATE AUC'S AND AUC/MIC'S AND PUT THEM TO SCREEN AND TO THE
C  OUTPUT FILE ... ONE TABLE FOR EACH OUTPUT EQUATION. ACTUALLY, THERE
C  WILL NOW BE ONE TABLE FOR EACH OUTPUT EQUATION FOR EACH TIME RESET
C  FOR THE SUBJECT. 

C  AS OF npageng25.f, THERE WILL BE 1 SET OF AUC TABLES FOR EACH OF
C  THE MEAN, MEDIAN, AND MODE.



C  THE NO. OF MAXIMUM TIMES FOR THIS SUBJECT IS NOMAXTIM(JSUB), WHICH
C  IS 1 MORE THAN THE NO. OF TIME RESETS; THE ENDING TIMES (24 HOURS 
C  PAST THE LAST OBSERVATION TIME) ARE IN 
C  TENDSUB(JSUB,J), J=1,NOMAXTIM(JSUB); AND THE BEGINNING TIMES ARE IN
C  TBEGGSUB(JSUB,J), J=1,NOMAXTIM(JSUB). MAKE A TABLE FOR EACH OF THESE
C  NOMAXTIM(JSUB) PAIRS OF TIMES.

      ILAST = 0

C  ILAST IS THE RUNNING NO. OF YYPRED VALUES ALREADY USED IN CALCULATING
C  AUCs FROM THE PREVIOUS LOOP ON IMAXTIM (SEE LOGIC BELOW).

C  FOR npageng21.f, A BUG IS FIXED BY INITIALIZING IELAST = 0 HERE:
C  FOR EACH NEW SUBJECT, INITIALIZE IELAST = 0. REASON: IT IS SUPPOSED
C  TO BE SET IN LOOP 2050, BUT IF NWHOLE = 0 FOR THE 1ST TIME PERIOD 
C  (IMAXTIM = 1) FOR A NEW SUBJECT, LOOP 2050 IS SKIPPED AND THEN
C  IS = IELAST OCCURS IN THE   IF(NPAR .GT. 0)   SECTION, AND IF THIS
C  OCCURS FOR JSUB > 1, THEN IS WILL BE SET TO WHATEVER IELAST WAS FROM 
C  THE PREVIOUS SUBJECT (WHICH MEANS SUM WILL NOT START WITH YYPRED(1,.)
C  FOR THE NEW SUBJECT AS IT SHOULD). TO PREVENT THIS, INITIALIZE 
C  IELAST = 0 HERE, FOR EACH NEW SUBJECT.

      IELAST = 0


      DO IMAXTIM = 1,NOMAXTIM(JSUB)

       NUMTT = int((TENDSUB(JSUB,IMAXTIM)-TBEGGSUB(JSUB,IMAXTIM)
     1   )*60/IDELTA)

C  NDELPER = NO. OF INTERVALS IN EACH AUCINT HOURS (1 PERIOD =
C            AUCINT*60 MINUTES).
C  NWHOLE = NO. OF WHOLE PERIODS.
C  NPAR = NO. OF INTERVALS ON THE LAST PARTIAL PERIOD, IF ANY.
C  NOTE: IF NUMTT/NDELPER IS AN INTEGER, THEN NPAR WILL = 0 --> THERE
C	   WILL BE NO LAST PARTIAL PERIOD.

C  RECALL THAT NUMTT IS THE TOTAL NO. OF INTERVALS FOR THE PREDICTED
C  CONCS. (NUMTT+1 IS THE NO. OF PREDICTED CONCS., SINCE THE 1ST
C  PREDICTED CONC. IS AT TIME = TBEGGSUB(JSUB,IMAXTIM)).

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

C  BEFORE CALCULATING THE AUC'S AND AUC/MIC'S EVERY AUCINT HOURS,
C  ESTABLISH THE HEADER INFO. FOR THE TABLE SHOWING THE AUC'S AND
C  THE AUC/MIC'S FOR THIS SUBJECT.
 
	WRITE(*,2048)

	WRITE(25,2048)
 2048   FORMAT(/'      PERIOD',10X,'TIME (HOURS)',8X,'  AUC',8X,'   AUC/
     1MIC'/
     1' ----------------------------------------------------------------
     1----')
 
C  OUTPUT ONE LINE IN THE AUC TABLE FOR EACH AUCINT HOURS, AND THEN ONE 
C  LINE FOR THE TOTALS.
 
C  AUCRUN = RUNNING TOTAL OF AUC'S THRU EACH AUCINT HOURS.

	AUCRUN = 0.D0

	DO 2050 IPERIOD = 1,NWHOLE


C  FOR WHOLE PERIOD, IPERIOD, CALCULATE THE STARTING AND ENDING INDICES
C  OF PREDICTED CONCENTRATIONS (IS AND IE, RESPECTIVELY).

C  NOTE THAT A BUG IS CORRECTED IN npageng16.f. PREVIOUSLY, THE VALUE
C  FOR IS WAS WRONG AFTER THE IMAXTIM = 1, SINCE THE YYPRED VALUES 
C  USED KEPT STARTING OVER WITH THE 1ST VALUE IN THE ARRAY, RATHER THAN
C  CONTINUE WITH THE NEXT SET OF YYPRED VALUES (WHICH ARE STACKED ONE
C  AFTER THE OTHER FOR ALL TTPRED VALUES FROM SUBROUTINE CALCTPRED).
C  I.E., IS ALWAYS STARTED WITH 1, REGARDLESS OF THE VALUE OF IMAXTIM.

C  NOW IS WILL BE ADJUSTED AHEAD ILAST VALUES, WHERE ILAST IS THE NO.
C  OF YYPRED VALUES ALREADY USED FOR THE PREVIOUS IMAXTIM VALUES.

	IS = ILAST + (IPERIOD-1)*NDELPER + 1
	IE = IS + NDELPER
      IELAST = IE


C  THE AREA UNDER THE CURVE WILL BE CALCULATED USING THE
C  TRAPEZOIDAL RULE. SINCE EACH OF THE CONCS. ARE IDELTA APART,
C  AUC = IDELTA*(YYPRED(IS,IEQ)/2 + SUM + YYPRED(IE,IEQ)/2), WHERE SUM
C  IS THE SUM OF YYPRED(.,IEQ), FOR INDICES BETWEEN IS+1 AND IE-1.

 
	SUM=0.D0
	DO I=IS+1, IE-1
	 SUM=SUM+YYYPRED(ICENTER,I,IEQ)
	END DO
 
	AUC = IDELTA*((YYYPRED(ICENTER,IS,IEQ) +
     1      YYYPRED(ICENTER,IE,IEQ))/2.D0 + SUM)/60.D0

 
C  NOTE THAT AUC HAS UNITS IN TERMS OF HOURS BECAUSE OF THE DIVISION BY
C  60 ABOVE (WHICH CHANGES IDELTA FROM MINUTES TO HOURS).
 
	AUCMIC = AUC/XMIC

	AUCRUN = AUCRUN+AUC
 
C  THIS PERIOD STARTS WITH HOUR, IHRST, AND ENDS WITH HOUR, IHREN.

C  NOTE THAT AS OF npageng18.f, IHRST GIVES THE "RELATIVE" INSTEAD OF
C  THE "REAL" TIME FOR THIS PERIOD (SINCE TBEGGSUB(.,.) IS NO LONGER
C  ADDED TO THE EQUATION BELOW.


C	IHRST = (IPERIOD-1)*AUCINT + TBEGGSUB(JSUB,IMAXTIM)
	IHRST = (IPERIOD-1)*int(AUCINT)
	IHREN = IHRST + int(AUCINT)

C  PRINT TO OUTPUT FILE AND THE SCREEN THE INFO FOR THIS PERIOD.
 

      XVERIFY(1) = AUC
      XVERIFY(2) = AUCMIC
      CALL VERIFYVAL(2,XVERIFY)

C     WRITE(*,2049) IPERIOD, IHRST, IHREN, AUC, AUCMIC
      WRITE(*,2049) IPERIOD, IHRST, IHREN, XVERIFY(1),XVERIFY(2)
C     WRITE(25,2049) IPERIOD, IHRST, IHREN, AUC, AUCMIC
      WRITE(25,2049) IPERIOD, IHRST, IHREN, XVERIFY(1),XVERIFY(2)


 2049 FORMAT(' ',I8,2X,I10,'    -',I10,5X,G12.6,3X,G12.6)
 
 2050 CONTINUE
 
C  NOW, PUT IN A LINE FOR THE LAST PARTIAL PERIOD, IF ANY.
 
	IF(NPAR .GT. 0) THEN
 
C  THE STARTING INDEX FOR THE PREDICTED CONCS. IS SIMPLY THE ENDING
C  INDEX FROM LOOP 2050 ABOVE. THE ENDING INDEX IS NPAR MORE THAN THE
C  STARTING INDEX. THEN PROCEED AS IN LOOP 2050 ABOVE.

 
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
 
C  ESTABLISH THE PERIOD NO., AND THE STARTING AND ENDING HOUR NOS.
 
	IPERIOD = NWHOLE + 1

C  NOTE THAT AS OF npageng18.f, IHRST GIVES THE "RELATIVE" INSTEAD OF
C  THE "REAL" TIME FOR THIS PERIOD (SINCE TBEGGSUB(.,.) IS NO LONGER
C  ADDED TO THE EQUATION BELOW.

C	IHRST = (IPERIOD-1)*AUCINT + TBEGGSUB(JSUB,IMAXTIM)

	IHRST = (IPERIOD-1)*int(AUCINT)
	IHREN = IHRST + int(NPAR*IDELTA/60)

 
C  PRINT TO OUTPUT FILE AND THE SCREEN THE INFO FOR THIS PERIOD.
 

      XVERIFY(1) = AUC
      XVERIFY(2) = AUCMIC
      CALL VERIFYVAL(2,XVERIFY)



C     WRITE(*,2051) IPERIOD, IHRST, IHREN, AUC, AUCMIC
      WRITE(*,2051) IPERIOD, IHRST, IHREN, XVERIFY(1),XVERIFY(2)


C     WRITE(25,2051) IPERIOD, IHRST, IHREN, AUC, AUCMIC
      WRITE(25,2051) IPERIOD, IHRST, IHREN, XVERIFY(1),XVERIFY(2)



 2051   FORMAT(' ',I2,' (PARTIAL)',I8,'    -',I10,5X,G12.6,3X,G12.6)
 

	ENDIF
 
C  THE ABOVE ENDIF IS FOR THE  IF(NPAR .GT. 0)  CONDITION.
 
C  NOW WRITE THE LAST LINE, GIVING TOTAL VALUES FOR AUC AND AUC/MIC.
C  NOTE THAT TOTAL AUC IS NOW AUCRUN. NOTE THAT IHREN IS THE SAME AS
C  THE LAST VALUE CALCULATED ABOVE (I.E., IT'S THE ENDING HOUR).
 
	AUCMIC = AUCRUN/XMIC

C  AS OF npageng18.f, THE BEGINNING TIME FOR EACH AUC TABLE, SINCE THE
C  TIMES ARE "RELATIVE", INSTEAD OF "REAL", WILL ALWAYS BE 0.

      XVERIFY(1) = AUCRUN
      XVERIFY(2) = AUCMIC
      CALL VERIFYVAL(2,XVERIFY)



C     WRITE(*,2052) IHREN, AUCRUN, AUCMIC
      WRITE(*,2052) IHREN, XVERIFY(1),XVERIFY(2)
C     WRITE(25,2052) IHREN, AUCRUN, AUCMIC
      WRITE(25,2052) IHREN, XVERIFY(1),XVERIFY(2)


 2052   FORMAT(' ','----------------------------------------------------
     1---------------'/
     2'    TOTAL',2X,'         0    -',I10,5X,G12.6,3X,G12.6//)
 

       END DO
C  THE ABOVE END DO IS FOR THE  DO ICENTER = 1,3  LOOP.


 
	END DO 
C  THE ABOVE END DO IS FOR THE  DO IEQ = 1,NUMEQT  LOOP.

C  INCREASE ILAST TO BE THE NO. OF YYPRED VALUES ALREADY USED.
C  SIMILARLY FOR IELAST.

       ILAST = IE
       IELAST = IE

	END DO
C  THE ABOVE END DO IS FOR THE  DO IMAXTIM = 1,NOMAXTIM(JSUB)  LOOP.
 
 

 7000   CONTINUE

C       write (*,*) "Passed 7000; writing DENSITY" 
	
C  RESTORE THE CORRECT CORDEN AND NACTVE (THEY WERE CHANGED IN
C  THE CALLS TO SUBRES). THEY MUST BE RESTORED BEFORE BEING WRITTEN
C  TO THE DENSITY FILE.
 
	NACTVE=NNACTVE
	DO I=1,NACTVE
	DO J=1,NVAR+1
	 CORDEN(I,J) = CORHOLD(I,J)
	END DO
	END DO
 
 
C  STORE THIS CYCLE'S JOINT DENSITY (AND ASSOCIATED VALUES) INTO THE
C  FILE DENFIL.
 
      write(*,*)' About to create density file ...'

	OPEN(23,FILE=DENFIL)
 
	WRITE(23,7124)
 7124   FORMAT('DENSITY OCT_15 ... Made by npagranfix6')

	WRITE(23,*) NDIM
	WRITE(23,*) INDPTS
 
C  NEW CODE ABOVE FOR m2_11cal.f
 
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

	
C  STARTING WITH MXEM2N36.FOR, NINT WILL ALWAYS BE 100. BUT, IN ORDER
C  NOT TO CHANGE THE STRUTURE OF PRIOR DENSITY FILES (SO THAT PRIOR
C  DENSITIES CAN STILL BE RUN WITH MXEM2N36.FOR), NINT WILL STILL
C  BE WRITTEN TO, AND READ FROM, THIS FILE.

 
	WRITE(23,*) NINT

c  As of npageng18.f, reduce ICYCLE BY 1 if IHESS = -1, because this 
c  means a Hessian Matrix was singular in a call to Subroutine emint,
c  which --> the current ICYCLE never had its values written to the
c  output file since control was transferred immediately to label 900
c  after returning from emint.

      IF(IHESS .EQ. -1) ICYCLE = ICYCLE - 1


	WRITE(23,*) ICYCLE
	WRITE(23,*) DORIG
 

	write(*,*)' Writing CORDEN ... '

	 DO I=1,NACTVE
	  WRITE(23,*) (CORDEN(I,J),J=1,NVAR+1)
	 END DO
 
C	write(23,*)' Writing PYJGX ... '
	write(*,*)' Writing PYJGX ... '

	DO JSUB=1,NSUB

	 DO I=1,NACTVE
	  WRITE(23,*) PYJGX(JSUB,I)
	 END DO
	END DO
 

 
	REWIND(27)

C        write(23,*)' Writing YPREDPOP ...'
        write(*,*)' Writing YPREDPOP ...'

        DO JSUB=1,NSUB
          CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,
     1     TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     2     INTLIST,ERRFILNAME)

        call cp_lrcs_to_rpar(gamma,flat,numeqt,c0,c1,c2,c3,c4,c5,rpar)

C        write (*,*) "Ret. from FILRED() nr. #5685"

          DO IEQ=1,NUMEQT
            DO J=1,NOBSER
              WRITE(23,*) (YPREDPOP(JSUB,IEQ,J,ICENTER),ICENTER=1,3)
            END DO
          END DO
        END DO

	DO JSUB = 1,NSUB
	 WRITE(23,*) NUMT(JSUB)
	END DO


C	write(23,*)' Writing YPREDOPT ... '
	write(*,*)' Writing YPREDOPT ... '
C
C Differences in DEN traced to this block on 10/12/2018 wmy
C
C

	DO JSUB=1,NSUB
	 DO IEQ=1,NUMEQT

	  DO J=1,NUMT(JSUB)
	   WRITE(23,*) (YPREDPOPT(JSUB,IEQ,J,ICENTER),ICENTER=1,3)
	  END DO
	 END DO
	END DO

C	write(23,*)' Writing TTPREDREL ... '
	write(*,*)' Writing TTPREDREL ... '


	DO JSUB=1,NSUB
	 DO J=1,NUMT(JSUB)

C  AS OF npageng18.f, WRITE THE "RELATIVE" INSTEAD OF THE "REAL"
C  TIMES TO FILE 23.

C	  WRITE(23,*) TTPRED(JSUB,J)
	  WRITE(23,*) TTPREDREL(JSUB,J)

	 END DO
	END DO
 
	REWIND(27)

C	write(23,*)' Writing YPREDBAY ... '
	write(*,*)' Writing YPREDBAY ... '

 
	DO JSUB=1,NSUB

	 CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,
     1     TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     2     INTLIST,ERRFILNAME)

        call cp_lrcs_to_rpar(gamma,flat,numeqt,c0,c1,c2,c3,c4,c5,rpar)

C        write (*,*) "Ret. from FILRED() nr. #6700"

	 DO IEQ=1,NUMEQT
	  DO J=1,NOBSER
	   WRITE(23,*) (YPREDBAY(JSUB,IEQ,J,ICENTER),ICENTER=1,3)
	  END DO
	 END DO
	END DO

	write(*,*)' Writing EXX ... '
C	write(23,*)' Writing EXX ... '

 
	DO JSUB=1,NSUB
	 DO ICENTER=1,3
	  WRITE(23,*) (EXX(JSUB,ICENTER,J),J=1,NVAR)
	 END DO
	END DO
 
      write(25,*) 'Optimal value of gamma=',gamma


C  CONSTRUCT THE COMBINED OUTPUT FILE, TO BE CALLED 'OUT'//NAME, WHERE
C  NAME WAS OBTAINED ABOVE, AND IS THE CHARACTER*4 EQUIVALENT TO INUM

C  AS OF npbig7aadapt.f, CREATE A COMBINED OUTPUT FILE.
C  NOTE THAT FILE 25 IS THE REGULAR OUTPUT FILE AND FILE 23 IS THE 
C  DENSITY FILE. ALSO SCRATCH FILE 27 HAS THE PATIENT DATA PART OF
C  npembig3.inp. SO ONLY NEED TO OPEN npagdriv.f, AND THEN OPEN
C  THE COMBINED OUTPUT FILE, OUTCOM, AND PUT ALL THE REQUIRED INFO
C  INTO OUTCOM, IN THE FORMAT REQUIRED BY SUBROUTINE PREVRUN OF THE
C  PC PREP/ANALYSIS PROGRAM (CURRENTLY, NPBIG10A.FOR).

	OPEN(29,FILE='npagdriv.f')
	REWIND(27)
	REWIND(25)
	REWIND(23)

	write(*,*)' About to create the combined output file ... '


	OUTCOM = 'OUT'//NAME
	OPEN(26,FILE=OUTCOM)

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
     2'***************** START OF THE DENSITY FILE *****************'/)


	write(*,*)' Writing density to combined output file ...'


C  PUT THE DENSITY FILE INTO THE COMBINED OUTPUT FILE. 

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

C  PUT THE PATIENT DATA INFO INTO THE COMBINED OUTPUT FILE. 

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

C  PUT npagdriv.f INTO THE COMBINED OUTPUT FILE. 

 1170   READ(29,2717,IOSTAT=IEND) READLARG
        IF(IEND .LT. 0) GO TO 1180
        CALL CONDENSE(READLARG)

        GO TO 1170

 1180   WRITE(26,1181)
 1181   FORMAT(/'***************** END OF THE npagdriv.f FILE **********
     1*******'/)
     


C  DON'T CLOSE FILES 25,23,27,29. THEY WILL BE REUSED
C  BY read__.f. Just rewind them.

      REWIND(23)
      REWIND(27)
      REWIND(25)
      REWIND(29)
      CLOSE(26)
      CLOSE(31)



C  FROM THE COMBINED OUTPUT FILE, OUTCOM, CALL READOUT TO FORM THE
C  SUMMARIZED OUTPUT FILE, NP_RFxxxx.TXT, WHICH IS COMPATIBLE WITH THE
C  R GRAPHICS PACKAGE.



        OUTFILER = 'NP_RF'//NAME//'.TXT'
        CALL READOUT(OUTFILER) 

 
c  For some reason, the unix system keeps file 27 in the directory.
c  To eliminate it, use the following command.

c  As of bigmlt1.f, the call system line below is commented out since
c  this program is usually run now on PCs. Also tbeg and tend are not
c  used now, so their write statements are commented out.
 
C	call system("rm -f fort.27")

C      tend = 0
C      write(91,*) 'total run time in seconds=',tend-tbeg
C       write(6,*) 'total run time in seconds=',tend-tbeg
 	  close(91)


C  NOW CLOSE THE FILES USED BY READOUT.

      CLOSE(23)
      CLOSE(27)
      CLOSE(25)
      CLOSE(29)
 
	STOP
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,
     1    TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,INTLIST,ERRFIL)

C  FILRED IS CALLED BY MAIN TO READ THE PORTION OF
C  SCRATCH FILE 27 WHICH APPLIES TO THE SUBJECT UNDER CONSIDERATION. THE
C  'POINTER' FOR FILE 27 IS IN THE PROPER POSITION TO BEGIN READING THE
C  INFO FOR THE DESIRED SUBJECT.

         use npag_utils, only: maxnumeq,max_m_per_obs
     1    ,max_obs_dim,max_input_dim,max_doses,max_RS_J
 
        IMPLICIT REAL*8(A-H,O-Z)
C        PARAMETER(MAXNUMEQ=7)

c ARGS passed in from main
        integer NOBSER
        DIMENSION YO(max_m_per_obs,NUMEQT),C4(NUMEQT),C5(NUMEQT)
     1    ,C0(NUMEQT),C1(NUMEQT),C2(NUMEQT),C3(NUMEQT)
        integer NUMEQT
C wmy2017Sep26 Copies of /INPUT/
       real*8, dimension(max_m_per_obs) :: TIMCOPY
       real*8, dimension(max_doses) :: SIGCOPY
       real*8, dimension(max_doses,max_RS_J) :: RSCOPY
       real*8, dimension(max_doses,max_input_dim) :: BSCOPY
c wmy2017Sep30
       integer, dimension(128) :: INTLIST

! wmy20190628 -- 
C        COMMON /CNST/ N,ND,NI,NUP,NUIC,NP
C        COMMON /CNST2/ NPL,NUMEQTT,NDRUG,NADD
C        COMMON/DESCR/AGE,HEIGHT,ISEX,IETHFLAG
C        COMMON /SUM2/ M,NPNL
        integer N,ND,NI,NUP,NUIC,NP
        integer NUMEQTT,NDRUG,NADD,ISEX,IETHFLAG,M
        double precision AGE,HEIGHT

c COMMON /OBSER/
        DIMENSION TIM(max_m_per_obs),SIG(max_doses),
     1    RS(max_doses,max_RS_J),
     2    YOO(max_m_per_obs,MAXNUMEQ),BS(max_doses,max_input_dim)

C  AS OF npageng13.f, THE FORMAT FOR THE WORKING COPY FILES IS:

C     COL 1 = TIME
C     COL 2 = IV FOR DRUG 1; COL 3 = PO FOR DRUG 1;
C     COL 4 = IV FOR DRUG 2; COL 5 = PO FOR DRUG 2;
C     ... EACH SUCCEEDING DRUG HAS AN IV FOLLOWED BY A PO COLUMN.
C     NEXT NADD COLUMNS = ONE FOR EACH ADDITIONAL COVARIATE (ADDITIONAL
C      REFERS TO ANY EXTRA COVARIATES BEYOUND THE 4 PERMANENT ONES IN
C      COMMON DESCR (SEE BELOW).
 
        COMMON /OBSER/ TIM,SIG,RS,YOO,BS
C        COMMON /CNST/ N,ND,NI,NUP,NUIC,NP
C        COMMON/ERR/ERRFIL 

        character ERRFIL*20

!$omp Threadprivate(/OBSER/)
C !$omp Threadprivate(/CNST/,/SUM2/,/CNST2/,/DESCR/)

c LOCAL variables
        CHARACTER SEX*1,READLINE*300
C wmy2017Sep26 -- This is a temporary copy, req. to fill BCOPY
        real*8, dimension(max_input_dim) :: FA

  
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
 
 
C  AGE, SEX, HEIGHT, AND ETHNICITY FLAG ARE ON LINES 8-11. So skip first 7
 
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

C wmy2017Sep30 Note loss of precision for AGE and HEIGHT
        INTLIST(1) = int(AGE)
        INTLIST(2) = ISEX
        INTLIST(3) = int(HEIGHT)
        INTLIST(4) = IETHFLAG
 
C  READ THE NO. OF DRUGS FROM THE LINE WITH 'NO. OF DRUGS' AS ENTRIES
C  12:23. THEN READ NO. OF ADDITIONAL COVARIATES, AND THE NO. OF DOSE 
C  EVENTS, ETC.

    1   FORMAT(A300)
   10	READ(27,1) READLINE
	IF(READLINE(12:23) .NE. 'NO. OF DRUGS') GO TO 10
	BACKSPACE(27)

    3   FORMAT(T2,I5)
        READ(27,3) NDRUG

C wmy2017Sep30
        INTLIST(5) = NDRUG
c        write (*,*) "INTLIST(5) = NDRUG", INTLIST(5), NDRUG

	IF(NDRUG .GT. max_input_dim) THEN
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

C wmy2017Sep30
        INTLIST(6) = NADD
        INTLIST(7) = NI
c        write (*,*) "INTLIST(6,7) = NADD, NI", INTLIST(6), INTLIST(7),
c     1     NADD, NI 

	IF(NI .GT. max_RS_J) THEN
  	 WRITE(*,123)
  123    FORMAT(/' YOUR PATIENT DATA FILES HAVE TOO MANY COLUMNS IN '/
     1' THE DOSAGE REGIMEN BLOCK. THE NO. OF ADDITIONAL COVARIATES '/
     2' PLUS TWICE THE NO. OF DRUGS CANNOT EXCEED max_RS_J. THE'/
     3' PROGRAM IS NOW STOPPING. '/)

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,123) 
        CLOSE(42)

       CALL PAUSE
	 STOP
	ENDIF

        READ(27,3) ND

C wmy2017Sep30
        INTLIST(8) = ND
c        write (*,*) "INTLIST(8) = ND", INTLIST(8), ND

	IF(ND .GT. max_doses) THEN
	 WRITE(*,125)
  125    FORMAT(' YOUR PATIENT DATA FILES CANNOT HAVE MORE THAN '/
     1' max_doses DOSE EVENTS. THE PROGRAM IS NOW STOPPING. '/)

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
         SIGCOPY(I)=SIG(I)
         do JJJ = 1,NI
            rscopy(I,JJJ) = rs(I,JJJ)
         end do
	END DO	 

C  ASSIGN THE VALUES IN EACH DRUG'S PO COLUMN TO THE CORRESPONDING
C  COLUMN IN ARRAY BS.

        DO I=1,ND
         DO J=1,NDRUG
          BS(I,J)=RS(I,2*J)
          BSCOPY(I,J)=RS(I,2*J)
         END DO
        END DO

C  READ THE NO. OF OUTPUT EQUATIONS FROM THE LINE WITH 'NO. OF TOTAL'
C  AS ENTRIES 12:23. THEN READ NO. OF OBSERVED VALUE TIMES, ETC.

   40	READ(27,1) READLINE
	IF(READLINE(12:23) .NE. 'NO. OF TOTAL') GO TO 40
	BACKSPACE(27)

        READ(27,*) NUMEQTT

C wmy2017Sep30
        INTLIST(9) = NUMEQTT
c        write (*,*) "INTLIST(9) = NUMEQT", INTLIST(9), NUMEQTT

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
C wmy2017Sep30
        INTLIST(10) = M
c        write (*,*) "INTLIST(10) = M", INTLIST(10), M

	MAXOBDIM = max_obs_dim
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
          TIMCOPY(I)=TIM(I)
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
	SUBROUTINE CALGRD(NVAR,NGRID,AB,X,errfil)
C     1     S,QS,COEF,RQS,NEXTN,TESTN,HISUM)

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
C      COMMON/ERR/ERRFIL 

c wmy2017Oct09
c  save flag required for run in parallel, but not serial
c  since CALGRD is called _outside_ of the parallel region
C  I have no idea what the issue really is!
 
C  THIS SUBROUTINE, CALLED BY MAIN, DETERMINES THE COORDINATES OF
C  THE IGTH GRID POINT, AND STORES THEM INTO X.
 
C  INPUT ARE:
 
C  NVAR = NO. OF COORDINATES (RANDOM VARIABLES).
C  NGRID = TOTAL NO. OF GRID POINTS
C  [AB(I,1),AB(I,2)] = GRID BOUNDARIES FOR COORDINATE (R.V.) I,
C                      I=1,NVAR.
 
C  OUTPUT IS:
 
C  X(I), I=1,NVAR = AS DESCRIBED ABOVE.
 
 
C  THE ICTH COORDINATE IS:
C       (AB(IC,2)-AB(IC,1))*QUASI(IC) + AB(IC,1),
C       WHERE QUASI(IC) is a quasi-random variable from the low discrepancy
C       FAURE sequence on the NVAR dimensional hyypercube defined by
C       ACM TOMS algorithm 647
C       (routines INFAUR and GOFAUR, available from www.netlib.org in
C       the TOMS directory are included below and are
C       properly set up for both SUNs, IBM workstations, and the CRAY
C       T3E.

 
c       first, we initialize the Faure sequence with a call to INFAUR
c       if this is the first time that we want a number.
 
c        write (*,*) first,"Calling INFAUR with",
c     1    nvar,ngrid,FLAG(1),FLAG(2)

        if(first) then
         CALL INFAUR(flag,nvar,ngrid,
     1     S,QS,COEF,RQS,NEXTN,TESTN,HISUM)
         first = .FALSE.
        endif
 

c here nvar = dimensionality of grid, ngrid = number of grid
c points to be generated, and flag(1) and flag(2) = logical variables
c set to 'T" if call is successful.
 
        IF(.NOT. FLAG(1)) THEN
	 WRITE(*,11) NVAR
   11    FORMAT(/' THE NUMBER OF RANDOM VARIABLES, ',I3,', IS NOT '/
     1' ACCEPTABLE IN SUBROUTINE INFAUR.')

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.


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

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,12) NGRID
        CLOSE(42)

	 CALL PAUSE
         STOP
        ENDIF
 
c now we call gofaur to generate  the nvar coordinaters inside
c the unit hypercube that will later be scaled to the grid point.
 
        CALL GOFAUR(quasi,
     1     S,QS,COEF,RQS,NEXTN,TESTN,HISUM)

 
c now we scale each of the i-coordinates to lie
c between AB(I,1) and AB(I,2).
 
	DO IC = 1,NVAR
        X(IC) = (AB(IC,2)-AB(IC,1))*quasi(IC) + AB(IC,1)
	END DO
 
	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE INFAUR(FLAG,DIMEN,ATMOST,
     1     S,QS,COEF,RQS,NEXTN,TESTN,HISUM)

        implicit double precision (a-h,o-z)
        LOGICAL FLAG(2)
 
      INTEGER S,ATMOST,QS,COEF(0:19,0:19),NEXTN,
     +        TESTN,HISUM,I,J,PRIMES(40),DIMEN
 

C      COMMON /FAURE/ S,QS,COEF,RQS,NEXTN,TESTN,HISUM
C      SAVE /FAURE/
 
      DATA (PRIMES(I),I=1,40)/1,2,3,5,5,7,7,11,11,11,11,
     +                        13,13,17,17,17,17,19,19,
     +                        23,23,23,23,29,29,29,29,
     +                        29,29,31,31,37,37,37,37,
     +                        37,37,41,41,41/
 
 
C      ALGORITHM 659, COLLECTED ALGORITHMS FROM ACM.

C      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
C      VOL. 14, NO. 1, P.88.
 
C       THIS SUBROUTINE FIRST CHECKS WHETHER
C       THE USER-SUPPLIED DIMENSION "DIMEN" OF THE
C       QUASIRANDOM VECTORS IS ACCEPTABLE
C       (STRICTLY BETWEEN 1 AND 41) : IF SO,
C       FLAG(1)=.TRUE.
 
C       THEN IT CALCULATES AN UPPER SUMMATION
C       LIMIT "HISUM" BASED ON "DIMEN" AND THE
C       USER-SUPPLIED NUMBER "ATMOST" OF QUASIRANDOM
C       VECTORS REQUIRED. FLAG(2)=.TRUE. IF
C       ATMOST IS OK.
 
C       IF FLAG(1) AND FLAG(2) ARE TRUE,
C       "INFAUR" NEXT PRODUCES THE OTHER
C       OUTPUTS LISTED BELOW PASSED TO
C       SUBROUTINE GOFAUR VIA LABELLED
C       COMMON "FAURE". THESE OUTPUTS ARE
C       IRRELEVANT TO THE USER.
 
C       FIRST CALL INFAUR. IF FLAG(1) AND
C       FLAG(2) ARE TRUE, EACH (SUBSEQUENT)
C       CALL TO GOFAUR GENERATES A NEW
C       QUASIRANDOM VECTOR.
 
C       INPUTS : DIMEN, ATMOST
 
C       OUTPUTS
C          TO USERS CALLING PROGRAM:
C             FLAG
C             QSS   : SAME AS QS - SEE BELOW
 
C          TO GOFAUR:
C             S      :DIMENSION
C             QS     :SMALLEST PRIME >=S
C             COEF   :TABLE OF BINOMIAL
C                     COEFFICIENTS NEEDED
C                     BY GOFAUR.
C             NEXTN  :THE NUMBER OF THE
C                     NEXT QUASIRANDOM
C                     VECTOR,INITIALIZED
C                     TO TESTN-1 HERE.
C             TESTN  :INITIALIZED TO QS**4
C             HISUM  :AFTER BEING USED TO
C                     PRODUCE COEF, INITIALIZED
C                     TO 3 FOR GOFAUR.
C             RQS    :1.0/QS.
 
C       CHECK S
 
      S=DIMEN
      FLAG(1) = S.GT.1 .AND. S.LT.41
c
c wmy2017Oct09 -- debug -- flag was not SAVEd 
c   between calls in parallel - but in serial
c   code worked fine. The decision immediately
c   above this write statement was always good.
c
c      write (*,*) "In INFAUR test S in (1,41)",
c     1  S,FLAG(1),FLAG(2)

      IF (.NOT.FLAG(1)) RETURN
 

      QS=PRIMES(S)
      TESTN=QS**4
 
C         COMPUTE LOG(ATMOST+TESTN) IN BASE QS
C         USING A RATIO OF NATURAL LOGS TO GET
C         AN UPPER BOUND ON (THE NUMBER OF
C         DIGITS IN THE BASE QS REPRESENTATION
C         OF ATMOST+TESTN) MINUS ONE.
 
      HISUM=NINT(LOG(REAL(ATMOST+TESTN))/LOG(REAL(QS)))
      FLAG(2)=HISUM.LT.20
      IF(.NOT. FLAG(2)) RETURN
 
C        NOW FIND BINOMIAL COEFFICIENTS MOD QS
C        IN A LOWER-TRIANGULAR MATRIX "COEF"
C        USING RECURSION BINOM(I,J)=BINOM(I-1,J)
C        +BINOM(I-1,J-1) AND A=B+C IMPLIES MOD(A,D)=
C        MOD(MOD(B,D)+MOD(C,D),D)
 
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
 
C        CALCULATING THESE COEFFICIENTS
C        MOD QS AVOIDS POSSIBLE OVERFLOW
C        PROBLEMS WITH RAW BINOMIAL COEFFICIENTS
 
C        NOW COMPLETE INITIALIZATION
C        AS DESCRIBED IN SECTION 2.
C        NEXTN HAS 4 DIGITS IN BASE
C        QS, SO HISUM EQUALS 3.
 

      NEXTN=TESTN-1
      HISUM=3
      RQS=1.0/REAL(QS)
 
      RETURN
      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE GOFAUR(QUASI,
     1    S,QS,COEF,RQS,NEXTN,TESTN,HISUM)

        USE npag_utils, only: max_pop_rand_varbs

        implicit double precision (a-h,o-z)
        INTEGER S,QS,COEF(0:19,0:19),NEXTN,TESTN,
     +        HISUM,I,J,K,YTEMP(0:19),ZTEMP,
     +        KTEMP,LTEMP,MTEMP
 
C QUASI DIMENSION CHANGED FROM 40 TO 25 TO MATCH DIMENSION IN CALGRD.


      dimension QUASI(max_pop_rand_varbs)
C      COMMON /FAURE/ S,QS,COEF,RQS,NEXTN,TESTN,HISUM
C      SAVE /FAURE/
 
C       THIS SUBROUTINE GENERATES A NEW
C       QUASIRANDOM VECTOR WITH EACH CALL
 
C       IT IMPLEMENTS A METHOD OF H.FAURE,
C       "ACTA ARITHMETICA XLI(1982),337-351".

C       (SEE ESPECIALLY PAGE 342).
 
C       THE USER MUST CALL "INFAUR" BEFORE
C       CALLING "GOFAUR".
C       AFTER CALLING "INFAUR", TEST FLAG(1)
C       AND FLAG(2); IF EITHER IS FALSE, DO
C       NOT CALL GOFAUR. READ THE COMMENTS AT
C       THE BEGINNING OF INFAUR AND THEN
C       THOSE BELOW.
 
C       ALL INPUTS COME FROM "INFAUR" VIA
C       LABELLED COMMON "FAURE"; FOR THEIR
C       DEFINITIONS, SEE "INFAUR".
 

C       INPUTS:
C         S,QS,COEF,NEXTN,TESTN,HISUM,RQS
 
C       OUTPUTS:
C         TO USER'S CALLING PROGRAM:
C         QUASI - A NEW QUASIRANDOM VECTOR

 
 
C       FIND QUASI(1) USING FAURE (SECTION 3.3)

 
C       NEXTN HAS A REPRESENTATION IN BASE
C       QS OF THE FORM: SUM OVER J FROM ZERO
C       TO HISUM OF YTEMP(J)*(QS**J)
 
C       WE NOW COMPUTE THE YTEMP(J)'S.
 
      KTEMP=TESTN
      LTEMP=NEXTN
      DO 100 I=HISUM,0,-1
          KTEMP=KTEMP/QS
          MTEMP=MOD(LTEMP,KTEMP)
          YTEMP(I)=(LTEMP-MTEMP)/KTEMP
          LTEMP=MTEMP

  100   CONTINUE
 

C       QUASI(K) HAS THE FORM SUM OVER J
C       FROM ZERO TO HISUM OF
C       YTEMP(J)*(QS**(-(J+1)))
 
C       READY TO COMPUTE QUASI(1)
C       USING NESTED MULTIPLICATION
 
      R=YTEMP(HISUM)

      DO 200 I=HISUM-1,0,-1

          R=YTEMP(I)+RQS*R
  200   CONTINUE
      QUASI(1)=R*RQS
 
C       FIND THE OTHER S-1 COMPONENTS
C       OF QUASI USING "FAURE" (SECTIONS
C       3.2 AND 3.3)
 
      DO 500 K=2,S
          QUASI(K)=0.0
          R=RQS
          DO 400 J=0,HISUM
              ZTEMP=0
              DO 300 I=J,HISUM
                  ZTEMP=ZTEMP+COEF(I,J)*YTEMP(I)
 
C       NO APPARENT ALTERNATIVE
C       ONE-DIMENSIONAL COEFFICIENT ARRAY
C       EXCEPT VIA SUBSCRIPT ADDRESS
C       COMPUTATIONS AND EQUIVALENCING
 
  300           CONTINUE
 
C       NEW YTEMP(J) IS THE SUM
C       OVER I FROM J TO HISUM

C       OF (OLD YTEMP(I)*BINOM(I,J))
C       MOD QS
 
              YTEMP(J)=MOD(ZTEMP,QS)
              QUASI(K)=QUASI(K)+YTEMP(J)*R
              R=R*RQS
  400       CONTINUE

  500   CONTINUE
 
C       UPDATE NEXTN AND, IF NEEDED, TESTN AND
C       HISUM
 
      NEXTN=NEXTN+1
      IF(NEXTN.EQ.TESTN) THEN
        TESTN=TESTN*QS
        HISUM=HISUM+1
 
C       SINCE FLAG(2) IS TRUE,
C       HISUM STAYS UNDER 20
 
      ENDIF
 
      RETURN
      END

C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE NOTINT(VOLSPA,NGRID,NACTVE,FUNC,MAXGRD,ESTINT)
	IMPLICIT REAL*8(A-H,O-Z)

	DIMENSION FUNC(MAXGRD)
C
C  THIS SUBROUTINE, CALLED BY MAIN, IS A MULTI-DIMENSIONAL INTEGRATOR.

C
C  INPUT ARE:


C
C  VOLSPA = 'VOLUME' OF THE INTEGRATION SPACE.
C  NGRID = NO. OF GRID POINTS OVER WHICH THE INTEGRATION IS DONE.
C  NACTVE = NO. OF CURRENTLY ACTIVE GRID POINTS.
C  FUNC(I), I=1,NACTVE = VALUE OF THE FUNCTION TO BE INTEGRATED AT

C                       THE ITH GRID POINT.
C  MAXGRD = DIMENSION OF FUNC -- SEE EXPLATION IN MAIN.
C
C  OUTPUT IS:
C
C  ESTINT = THE ESTIMATE OF THE NVAR-DIM INTEGRAL OF THE FUNCTION WHOSE
C           VALUES ARE GIVEN IN FUNC.
C
	SUM=0.D0
	DO 100 IG=1,NACTVE
  100   SUM=SUM+FUNC(IG)
	ESTINT=VOLSPA*SUM/NGRID
	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
       SUBROUTINE STAZ(VOLSPA,NGRID,NACTVE,NVAR,IND,CORDEN,PROD,MAXGRD,
     1  NINT,X1,X2,XMODE,X025,X25,X50,X75,X975,SCALINFO,NSUB,MAXDIM)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION CORDEN(MAXGRD,MAXDIM+1),PROD(MAXGRD),X(1999),

     1  PROB(1998),CUMPRO(0:1998)
 
C  THIS SUBROUTINE, CALLED BY MAIN, CALCULATES THE MODE AND 5 %-TILE
C  VALUES OF THE APPROXIMATE MARGINAL DENSITY OF R.V. IND, WHOSE

C  BOUNDARIES [X1,X2] ARE INPUT. ALSO, THE
C  SCALED 'INFO' FOR THIS MARGINAL DENSITY IS CALCULATED.
 
 
C  INPUT ARE:
C
C  VOLSPA = 'VOLUME' OF THE INTEGRATION SPACE.
C  NGRID = THE NO. OF GRID POINTS.
C  NACTVE = THE NO. OF CURRENTLY ACTIVE GRID POINTS.
C  NVAR = NO. OF R.V.'S.
C  IND = INDEX OF THE R.V. WHOSE STATISTICS ARE BEING FOUND.
C  CORDEN(I,J) = JTH COORDINATE FOR THE ITH GRID POINT, J=1,NVAR;
C		 I=1,NACTVE;
C		 DENSITY FOR THE ITH GRID POINT, J=NVAR+1;I=1,NACTVE.
C  PROD(I), I=1,NACTVE = DUMMY ARRAY; IN ARGUMENT LIST SO IT CAN BE

C                       VARIABLY DIMENSIONED.
C  MAXGRD = DIMENSION OF CORDEN AND PROD -- SEE EXPLANATION IN MAIN.
C  NINT = THE NO. OF INTERVALS TO BE USED IN CALCULATING THE APPROXIMATE
C         MARGINAL DENSITY OF R.V. INDICATED ABOVE (CHANGED -SEE BELOW).
C  [X1,X2] = BOUNDS ON THE R.V. INDICATED ABOVE.
C  NSUB = NO. OF SUBJECTS.

C  MAXDIM = DIMENSION OF CORDEN.

 
C  OUTPUT IS:
C
C  XMODE = MODE OF THE APPROXIMATE DISTRIBUTION OF R.V. INDICATED ABOVE.
C  X025 =  2.5 %-TILE VALUE OF ABOVE APPROXIMATE DISTRIBUTION.
C  X25 =    25 %-TILE VALUE OF ABOVE APPROXIMATE DISTRIBUTION.

C  X50 =    50 %-TILE VALUE OF ABOVE APPROXIMATE DISTRIBUTION (MEDIAN).
C  X75 =    75 %-TILE VALUE OF ABOVE APPROXIMATE DISTRIBUTION.
C  X975 = 97.5 %-TILE VALUE OF ABOVE APPROXIMATE DISTRIBUTION.
C  SCALINFO = SCALED 'INFO' FOR THIS MARGINAL DENSITY.
 
C  AS OF npbig1.f, THE NO. OF INTERVALS USED IN THIS ROUTINE IS
C  NO LONGER HARDCODED TO BE NINT = 100. THE REASON IS THAT THE
C  SCALED INFO HAS A DENOMINATOR OF LOG(NINT/NSUB) WHICH = 0 IF
C  NSUB = NINT = 100 (--> DIVIDE BY 0 ERROR), AND IF NSUB > 100, THIS
C  PRODUCES A NEGATIVE SCALED INFO (WHICH IS SUPPOSED TO VARY FROM 0
C  TO 100). SO MAKE THE NO. OF INTERVALS = MAX(100,2*NSUB).
 
C  NOTE THAT NINT IS PASSED TO THIS ROUTINE AS ALWAYS 100; DON'T
C  CHANGE NINT; USE NEWINT BELOW.
 
	NEWINT = NINT
	IF(2*NSUB .GT. NEWINT) NEWINT = 2*NSUB
 
 
C  FIND THE NEWINT INTERVALS OF INTEGRATION -- EQUALLY SPACED -- BETWEEN
C  X1 AND X2.
C
	XINT=(X2-X1)/NEWINT
	X(1)=X1

	X(NEWINT+1) = X2 

	DO 5 I=1,NEWINT-1
    5   X(I+1)=X(I)+XINT

 
C  FIND THE APPROXIMATE PROBABILITY THAT THE R.V. IS IN EACH OF THE
C  NEWINT INTERVALS.
 
	DO 1000 INTR=1,NEWINT
 
C  FIND PROB{ R.V. IN [X(INTR),X(INTR+1)] } = PROB(INTR).
C

C  INTEGRATE (CORDEN(I,NVAR+1)*W(X(INTR),X(INTR+1))), WHERE W(A,B) = 1
C  IF R.V. IS IN [X(INTR),X(INTR+1)], AND = 0 IF NOT.
C
C  FIND W EVALUATED AT EACH GRID POINT AND MULTIPLY IT BY
C  CORDEN(I,NVAR+1)
 
	DO IG=1,NACTVE
	  W = 0.D0
	  XX = CORDEN(IG,IND)
	  IF(XX .GE. X(INTR) .AND. XX .LT. X(INTR+1)) W = 1.D0
	  IF(INTR .EQ. NEWINT .AND. XX .GE. X(INTR)) W = 1.D0
C    BUG FIX IN bignpaglap4.f IS THE LINE ABOVE, WHICH GUARANTEES THAT
C    ANY DENSITY VALUE AT THE UPPER BOUNDARY IS COUNTED.


          PROD(IG) = CORDEN(IG,NVAR+1)*W
	END DO
 
 1000  CALL NOTINT(VOLSPA,NGRID,NACTVE,PROD,MAXGRD,PROB(INTR))
 
 
C  WITH PROB(.), CALCULATE THE MODE.
 
	INTMAX=1
	DO INTR=2,NEWINT
	  IF(PROB(INTR) .GT. PROB(INTMAX)) INTMAX=INTR
	END DO
 

	XMODE=(X(INTMAX)+X(INTMAX+1))/2.D0
 
 
C  CALCULATE THE 'SCALED INFORMATION' FOR THIS CYCLE (STARTING WITH
C  PROGRAM m234calc.f, 11-6-94; BUT THE SCALED 'INFO' IS CHANGED
C  IN PROGRAM MXEM2N35.FOR/m235calc.f, 11-19-94):
 
C  INFO = SUM(PI*LOG(PI)), WHERE THE SUM IS OVER I=1,NEWINT, THE LOG
C    	  IS TO BASE 2, PI*LOG(PI) --> 0 IF PI = 0, AND
C	  PI = PROB(I).
 
C  THEN INFO IS SCALED: y = 100*ln(2)/ln(n/N)*(x + ln(n)/ln(2)), WHERE
C  y = SCALED INFO, x = ABOVE INFO, n = NEWINT, N = NO. OF SUBJECTS. SO
C  y = 0 (%) IF DENSITY IS UNIFORM, and y = 100 (%) if DENSITY IS
C  CONCENTRATED AT N EQUALLY LIKELY POINTS, ... I.E., SCALED INFO = 0 %
C  FOR NO INFO, AND SCALED INFO = 100 % FOR PERFECT INFO.
 
	SUM=0.D0
 
	DO I=1,NEWINT
 
	IF (PROB(I) .GT. 0.D0) THEN
	  PI=PROB(I)
	  SUM=SUM+PI*DLOG(PI)
	ENDIF
 
	END DO
 
	DL2=DLOG(2.D0)
	SUM = SUM/DL2
 
C  SUM IS NOW THE INFO FOR THIS MARGINAL DENSITY. SCALE IT AS SHOWN
C  ABOVE.
 
        DINT=NEWINT
	FACT=100.D0*DL2/DLOG(DINT/NSUB)
	SCALINFO = FACT*(SUM + DLOG(DINT)/DL2)
 
 
C  FIND THE 5 %-TILE VALUES.
 
C  X025 IS THE X-VALUE BELOW WHICH IS 2.5 % OF THE DISTRIBUTION.
C  X25  IS THE X-VALUE BELOW WHICH IS 25 % OF THE DISTRIBUTION.
C  X50  IS THE X-VALUE BELOW WHICH IS 50 % OF THE DISTRIBUTION.
C  X75  IS THE X-VALUE BELOW WHICH IS 75 % OF THE DISTRIBUTION.

C  X975 IS THE X-VALUE BELOW WHICH IS 97.5 % OF THE DISTRIBUTION.
 
 
C  ACTUALLY, X025 WILL BE THE WEIGHTED X-VALUE IN THE INTERVAL WHERE
C  THE CUMULATIVE PROBABILITY EXCEEDS 2.5%. SIMILARLY FOR THE OTHER
C  %-TILE VALUES.
 
C  FIND THE INTERVALS WHERE THE CUMULATIVE PROBABILITES EXCEED 2.5%,
C  25%, 50%, 75%, AND 97.5%.
C
C  CUMPRO(I) BELOW IS THE CUMULATIVE PROBABILITY OF THE DISTRIBUTION
C  THROUGH INTERVAL I.
 
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
C

C  NOW IND025 IS THE INTERVAL WHERE THE CUMULATIVE PROBABILITY EXCEEDS
C  .025 (SIMILARLY FOR THE OTHER IND'S).
C
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
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

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
C      SUBROUTINE MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,RANFIXEST,PX)

C
C
C                    THIS ROUTINE MOVED INTO npag_utils.mod
C

C	IMPLICIT REAL*8(A-H,O-Z)
C	DIMENSION IRAN(max_ODE_params),X(max_pop_rand_varbs),
C     1   VALFIX(20),PX(max_ODE_params),RANFIXEST(20)

C  THIS ROUTINE, CALLED BY MAIN, INPUTS NVAR, NOFIX, NRANFIX, IRAN, X,
C  VALFIX, AND RANFIXEST, AND RETURNS PX(I) = A COMBINATION OF THE 
C  VALUES IN X, VALFIX, AND RANFIXEST, IN THE PROPER ORDER (AS 
C  DETERMINED BY IRAN).
 
C      NNNVAR = 0
C      NNNFIX = 0
C      NNNRANFIX = 0

C      DO I = 1,NVAR+NOFIX+NRANFIX
 
C       IF(IRAN(I) .EQ. 1) THEN
C        NNNVAR = NNNVAR+1
C        PX(I) = X(NNNVAR)
C       ENDIF
 
C       IF(IRAN(I) .EQ. 0) THEN
C        NNNFIX = NNNFIX+1
C        PX(I) = VALFIX(NNNFIX)
C       ENDIF

C       IF(IRAN(I) .EQ. 2) THEN
C        NNNRANFIX = NNNRANFIX+1
C        PX(I) = RANFIXEST(NNNRANFIX)
C       ENDIF
 
C      END DO
 
c      write (*,*) "Initialized IG",NNNVAR,NNNFIX,NNNRANFIX

C      RETURN
C      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C	SUBROUTINE SUBRES(MAXSUB,MAXACT,JSUB,CORDEN,WORK,MAXGRD,MAXDIM,
C     1  NVAR,NOFIX,VALFIX,SIGFAC,OFAC,AB,PAR,NACTVE,NGRID,VOLSPA,IRAN,
C     2  CENTER,PYJGXX,NRANFIX,RANFIXEST,NOBSER,NUMEQT)
C wmy2017Sep12
      SUBROUTINE SUBRES(MAXSUB,MAXACT,JSUB,CORDEN,WORK,MAXGRD,MAXDIM,
     1  NVAR,NOFIX,VALFIX,SIGFAC,OFAC,AB,PAR,NACTVE,NGRID,VOLSPA,IRAN,
     2  CENTER,PYJGXX,NRANFIX,RANFIXEST,NOBSER,NUMEQT,NBCOMP,
     3  NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     4  INTLIST,IPAR,ObsError,RPAR,ERRFIL)

      USE npag_utils, only: verifyval,makevec,maxnumeq,max_m_per_obs
     1  ,max_ODE_params,max_pop_rand_varbs,max_doses,max_pop_params
     2  ,max_ODE_comps,max_pop_varbs,max_RS_J,max_input_dim
     3  ,k_sfac,k_ofac,k_sum_z_sq,k_prod_pr,i_skip_ig,i_do

      IMPLICIT REAL*8(A-H,O-Z)

C      PARAMETER(MAXNUMEQ=7)

C--- ARG LIST
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

C??? SEE COMMENTS BELOW WHERE BAYPOS (AND NACTSUB) ARE STORED. THE 
C  FIRST DIMENSION OF BAYPOS (AND THE ONLY DIMENSION OF NACTSUB) IS
C  SET = 100, RATHER THAN 800, BECAUSE THE PROGRAM IS TOO BIG 
C  OTHERWISE.

C  NOTE THAT BAYPOS STORES THE BAYESIAN POSTERIOR DENSITY FOR EACH
C  SUBJECT, AND NACTSUB STORES THE NO. OF GRID POINTS IN EACH 
C  SUBJECT'S BAYESIAN POSTERIOR.

      COMMON/BAY/NACTSUB,BAYPOS
        integer, dimension(100) :: NACTSUB
        real*8, dimension(100,1500,31) :: BAYPOS
C      COMMON/ERR/ERRFIL ! passed in from main, ERRFILNAME
        CHARACTER ERRFIL*20

C wmy2017Sep12 Added to the input
C      COMMON/TOUSER/NDIM,MF,RTOL,ATOL

      COMMON/PARAMD/P
        real*8, dimension(max_ODE_params) :: P

!$omp ThreadPrivate(/PARAMD/)

C--- Local varbs

C wmy2017Sep19
        integer IG,NPX,NEWIND
        REAL*8 W,KU,DENMAX,D,SUMD,FACT
        REAL*8, dimension(max_ODE_params) :: PX
        REAL*8, dimension(max_ODE_comps) :: RCOPY,BCOPY
        REAL*8, dimension(max_pop_rand_varbs) :: EX,STD,COFVR,X
        REAL*8, dimension(max_pop_rand_varbs,max_pop_rand_varbs) ::
     1    COV,E,CORR
        REAL*8, dimension(100) :: XVERIFY

C  AS OF MXEM2N54, SUBRES IS CHANGED. RATHER THAN CALCULATE THE
C  'FINAL FITTED JOINT P.D.F.' FOR EACH SUBJECT, IT CALCULATES
C  THE BAYESIAN POSTERIOR DENSITY FOR EACH SUBJECT. IN PARTICULAR, FOR
C  A SUBJECT WITH OBSERVATIONS Y, THE  BAYESIAN POSTERIOR DENSITY IS
C  P(XI|Y) FOR EACH REMAINING GRID POINT (XI) IN THE POPULATION FINAL
C  CYCLE JOINT DENSITY, WHERE P(XI|Y) = PF(XI) * P(Y|XI) / P(Y),
C  P(Y) = INTEGRAL OF P(Y|XI)*PF(XI), AND PF(XI) IS THE POPULATION
C  FINAL CYCLE JOINT DENSITY VALUE AT XI.
 
C  THIS ROUTINE, CALLED BY MAIN,

C  INPUTS THE FINAL JOINT DENSITY (CORDEN)
C  FROM THE PROGRAM (FOR THE ENTIRE POPULATION) AS THE APRIORI DENSITY
C  FOR A SINGLE SUBJECT WHOSE CONCENTRATION LEVELS, ETC. HAVE ALREADY
C  BEEN READ INTO COMMON STATEMENTS FOR MODULE IDPC, AND CALCULATES THAT
C  SUBJECT'S BAYESIAN POSTERIOR JOINT DENSITY. IT OUTPUTS LOG-LIKS,
C  EXPECTED VALUES, ETC. FOR THIS DENSITY. THE ONLY VALUES RETURNED TO
C  MAIN ARE THE FINAL CYCLE EXPECTED VALUES, MEDIANS, AND MODES
C  (IN CENTER).

C  NOTE THAT PYJGXX(IG) = IS CALCULATED IN LOOP 800 AND RETURNED
C  TO MAIN.

C  NOTE: THE RESULTS ARE OUTPUT TO THE SCREEN AND FILE 25.
 
C  FOR DETAILED COMMENTS ON THE FOLLOWING CODE, SEE THE SIMILAR CODE
C  IN MAIN.
 
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

C  SEE COMMENTS IN LOOP 800 IN MAIN.
 
            DO J=1,NVAR
              X(J)=CORDEN(IG,J)
            END DO

C wmy2018Mar07 PX is a local variable in this subroutine, unlike in main
c   where PX is a variable of /TOCALC/
            CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,RANFIXEST,PX)
        NPX = NVAR+NOFIX+NRANFIX
C       if (NPX < max_ODE_params) then exit progam w/error
            IPAR(i_skip_ig) = 1

C wmy2018Jan12 I removed COMMON/PARAMD/P from SUBROUTINE IDCALCY and
c  IDCALCYY, so P must be initialized in main after calling MAKEVEC, but
c  before calling IDCALCY/YY. Note: IDCALCY/YY now call SYMBOL and then 
c  immediately call FUNC2() or FUNC3(). 

C            DO I=1,NVAR+NOFIX+NRANFIX
c wmy 10/5/2018 removed /PARAMD/
            DO I=1,NPX
              P(I) = PX(I)
            END DO

C wmy2017Sep12 Added /TOUSER/ varbs to CALL IDPC()
C        CALL IDPC(NPX,PX,W,NOBSER,NUMEQT)

C            write (*,*) PX(1),PX(2), PX(3), PX(4)
C     1        , PX(5), PX(6), PX(7), PX(8), PX(9), PX(10)

            CALL IDPC(JSUB,IG,NPX,PX,NBCOMP,W,NOBSER,NUMEQT,NDIM,MF,
     1        RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     2        INTLIST,IPAR,ObsError,RPAR,ERRFIL)

C            write (*,*) "DO 7000 IDPC(JSUB,IG,W)",JSUB,IG,W

            PYJGX=0.D0

C
C Old Code, Assumes all observations are ~ Normal
C 
C            IF (W .LE. 22708.D0) THEN
C              PYJGX = DEXP(-.5D0*W)/SIGFAC/OFAC
C            ENDIF
C
C
C New Code, Assumes Mix of ~ Poisson and ~ Normal
C
          IF(RPAR(k_sum_z_sq) .LE. 22708.D0) THEN
            PYJGX = 10**RPAR(k_prod_pr)
     1        * DEXP(-.5D0*RPAR(k_sum_z_sq))/RPAR(k_sfac)/RPAR(k_ofac)
          ENDIF
C
C End New Code

            PYJGXX(IG) = PYJGX 
            WORK(IG)=PYJGX*CORDEN(IG,NVAR+1)

  800     CONTINUE

C  WORK(IG) = P(YJ,XIG), FOR IG=1,NACTVE.
C  PYJ = P(YJ) = INTEGRAL OF WORK.

C wmy2017Sep7
C      write (*,*) "First call to NOTINT()"
 
      CALL NOTINT(VOLSPA,NGRID,NACTVE,WORK,MAXGRD,PYJ)

C      write (*,*) "Returns ", PYJ

C  AS OF npagranfix6.f, VERIFY THAT PYJ IS NOT 0 (SIMILARLY TO THE 
C  CODE IN MAIN.

C      write (*,*) "Second call to NOTINT()"

      CALL NOTINT(VOLSPA,NGRID,NACTVE,WORK,MAXGRD,PYJ)
 
C      write (*,*) "Returns ", PYJ

C  IF PYJ RETURNS AS 0, IT IS BECAUSE P(X,YJ)=WORK IS 0 IN ALL ITS
C  NACTVE ENTRIES. THIS OCCURS WHEN EACH OF NACTVE VALUES OF W (WHICH
C  RETURNS FROM THE CALLS TO IDPC) IS LARGER THAN 1416 (SINCE P(YJ|X)
C  INVOLVES e RAISED TO THE POWER -.5*W, AND e RAISED TO A POWER
C  SMALLER THAN -708 IS SET TO 0 BY, FOR EXAMPLE, THE COMPAC COMPUTER).


C  IN CASE THIS HAPPENS, PRINT A MESSAGE TO THE USER AND STOP.

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

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,26) JSUB 
        CLOSE(42)

        CALL PAUSE
        STOP

      ENDIF

 


C  THE BAYESIAN POSTERIOR DENSITY OF THIS SUBJECT IS, FOR GRID PT. IG,

C  P(XIG|YJ) = P(YJ,XIG)/P(YJ). PUT THESE VALUES INTO CORDEN(IG,NVAR+1).

	DO IG=1,NACTVE
	  CORDEN(IG,NVAR+1) = WORK(IG)/PYJ
	END DO

C  NEW CODE FOR bigmlt10.f - BELOW


C  CALCULATE HOW MANY OF THE NACTVE GRID POINTS FROM THE FINAL CYCLE
C  ARE "ACTIVE" (WITHIN 1.D-10 OF THE MAXIMUM DENSITY FOR THIS SUBJECT).
C  ... AND, AS OF npageng23.f, ELIMINATE NON-SIGNIFICANT GRID PTS. IN
C  CORDEN (PREVIOUSLY ALL THE POINT FROM THE FINAL CYCLE DENSITY
C  SHOWED UP IN CORDEN, EVEN THOSE WITH INSIGNIFICANT PROBABILITIES).
C  AND NOTE THAT THE BAYESIAN POSTERIOR DENSITY FOR THIS SUBJECT
C  WILL BE STORED INTO BAYPOS(JSUB,.,.), AND PASSED IN COMMON/BAY
C  TO SUBROUTINE READOUT. AND NACTSUB(JSUB) WILL CONTAIN THE NO. OF

C  ACTIVE GRID POINTS FOR THIS SUBJECT'S BAYESIAN POSTERIOR DENSITY.

C  AND NOTE THAT THE NOMINAL DIMENSIONS OF BAYPOS, (800,1500,31),
C  CANNOT BE EXCEEDED BECAUSE THESE ARE THE VALUES FOR MAXSUB, MAXGRD,
C  AND MAXDIM+1, AS SPECIFIED IN THE PARAMETER STATEMENT IN THE PC PREP
C  MAIN MODULE (CURRENTLY NPAG111.FOR). BUT NOTE THAT npageng23.f WILL
C  NOT EXECTUTE WITH THESE DIMENSIONS BECAUSE IT IS TOO BIG FOR A 
C  WIN32 APPLICATION (SEE NPAG111.EXP). SO, IN THIS ROUTINE, AND IN
C  SUBROUTINE READOUT (IN read19.f), THE FIRST DIMENSION HAS BEEN 
C  REDUCED TO 100 (AND SIMILARLY FOR THE DIMENSION OF NACTSUB). IF
C  JSUB > 100, THE BAYESIAN POSTERIOR VALUES BELOW WILL NOT BE STORED.

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


C  STORE NACTVE INTO NACTVEFULL BECAUSE THIS VALUE, THE NO. OF ACTIVE
C  GRID POINTS FOR THE ENTIRE POPULATION, IS NEEDED BELOW IN 
C  FORMAT 3258. 


        NACTVEFULL = NACTVE

        NACTVE = NEWIND

C  STORE NACTVE INTO NACTSUB(.) SO IT CAN BE PASSED IN COMMON/BAY TO
C  SUBROUTINE READOUT, ALONG WITH ARRAY BAYPOS. BUT, SEE COMMENTS 
C  ABOVE, ONLY STORE NACTVE IF JSUB .LE. 100.

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

C   NEW CODE FOR bigmlt10.f - ABOVE
 
	
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
 

C  REPLACE WRITING OF SUM AND ENT WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL.

      XVERIFY(1) = SUM
      XVERIFY(2) = ENT
      CALL VERIFYVAL(2,XVERIFY)



C     WRITE(*,31) SUM
	WRITE(*,31) XVERIFY(1)


C	WRITE(*,131) ENT
	WRITE(*,131) XVERIFY(2)


C	WRITE(25,31) SUM
	WRITE(25,31) XVERIFY(1)


C	WRITE(25,131) ENT
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



C     WRITE(*,5103) (EX(I),I=1,NVAR)
      WRITE(*,5103) (XVERIFY(I),I=1,NVAR)
C     WRITE(25,5103) (EX(I),I=1,NVAR)
      WRITE(25,5103) (XVERIFY(I),I=1,NVAR)


 5103   FORMAT(1X,30(G12.6,1X))


C  SEE COMMENTS IN MAIN ABOUT ICOVL0.
 
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

C       WRITE(25,5103) (COV(I,J),J=1,I)
        WRITE(25,5103) (XVERIFY(J),J=1,I)

C 200   WRITE(*,5103) (COV(I,J),J=1,I)
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



C     WRITE(*,5103) (STD(I),I=1,NVAR)
      WRITE(*,5103) (XVERIFY(I),I=1,NVAR)
C     WRITE(25,5103) (STD(I),I=1,NVAR)
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



C     WRITE(*,5103) (COFVR(I),I=1,NVAR)
      WRITE(*,5103) (XVERIFY(I),I=1,NVAR)


C     WRITE(25,5103) (COFVR(I),I=1,NVAR)
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

C      WRITE(25,5103) (CORR(I,J),J=1,I)
       WRITE(25,5103) (XVERIFY(J),J=1,I)

C6080  WRITE(*,5103) (CORR(I,J),J=1,I)
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
 
C  STORE THE MEDIANS AND MODES.
 
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

C      WRITE(*,6093) XMODE,SK,KU,X025
       WRITE(*,6093) (XVERIFY(IXV),IXV=1,4) 


C      WRITE(25,6093) XMODE,SK,KU,X025
       WRITE(25,6093) (XVERIFY(IXV),IXV=1,4) 

       XVERIFY(1) = X25
       XVERIFY(2) = X50
       XVERIFY(3) = X75  
       XVERIFY(4) = X975
       CALL VERIFYVAL(4,XVERIFY)

C      WRITE(*,6093) X25,X50,X75,X975
       WRITE(*,6093) (XVERIFY(IXV),IXV=1,4) 

C      WRITE(25,6093) X25,X50,X75,X975
       WRITE(25,6093) (XVERIFY(IXV),IXV=1,4) 
 
       XVERIFY(1) = SDEST1
       XVERIFY(2) = SDEST2
       XVERIFY(3) = SDEST3
       XVERIFY(4) = SCALINFO
       CALL VERIFYVAL(4,XVERIFY)

C      WRITE(*,6093) SDEST1,SDEST2,SDEST3,SCALINFO
       WRITE(*,6093) (XVERIFY(IXV),IXV=1,4) 

C      WRITE(25,6093) SDEST1,SDEST2,SDEST3,SCALINFO
       WRITE(25,6093) (XVERIFY(IXV),IXV=1,4) 

 6093  FORMAT(1X,4(G15.8,2X))
 
 6090   CONTINUE



      WRITE(*,*)
      WRITE(25,*)
 
C  STORE THE MEANS INTO CENTER(1,.). CENTER(2,.) AND CENTER(3,.)
C  THE MEDIANS AND MODES WERE STORED JUST BELOW THE CALL TO STAT.
 

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
	 IF(IFILE .EQ. 25) WRITE(25,222) IFIRST
	 IF(IFILE .EQ. 29) WRITE(29,222) IFIRST

  222    FORMAT(1X,I5)
	 GO TO 100	
	ENDIF


C  IF THE NEXT PATIENT NO. IN IPATVEC = IFIRST + 1, THEN IFIRST IS THE
C  FIRST OF A STRING OF CONSECUTIVE NUMBERS (FIND THE LAST NO. IN THIS 
C  STRING AND WRITE THE STRING OUT). OTHERWISE, IFIRST WILL BE WRITTEN 
C  OUT BY ITSELF.

	IF(IPATVEC(NEXTIND+1) .NE. IFIRST + 1) THEN
	 IF(IFILE .EQ. 25) WRITE(25,222) IFIRST
	 IF(IFILE .EQ. 29) WRITE(29,222) IFIRST
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
   	IF(IFILE .EQ. 29) WRITE(29,221) IFIRST,ILAST
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
      SUBROUTINE CALCTPRED(JSUB,IDELTA,NOBSER,NUMTSUB,TPRED,TPREDREL,
     1   NOMAXTIMS,TEND,TBEGG,TIM,SIG,INTLIST)

       use npag_utils, only: maxnumeq,max_m_per_obs,max_doses,
     1   max_RS_J,max_input_dim

      IMPLICIT REAL*8(A-H,O-Z)

C      PARAMETER(MAXNUMEQ=7)

C  NOTE THAT AS OF CALCTPRED, MAXNUMEQ IS SET IN A PARAMETER STMT.
C  HERE SO THAT YOO CAN BE VARIABLY DIMENSIONED BELOW.

      DIMENSION TPRED(71281),TEND(99),TIM(max_m_per_obs),
     1  SIG(max_doses),
     2  RS(max_doses,max_RS_J),BS(max_doses,max_input_dim),
     3  YOO(max_m_per_obs,MAXNUMEQ), TBEGG(99),TPREDREL(71281)

      integer, dimension(128) :: intlist

C        COMMON/OBSER/ TIM,SIG,RS,YOO,BS
C        COMMON/CNST/ N,ND,NI,NUP,NUIC,NP

C !$omp Threadprivate(/OBSER/)
C ,/CNST/)

C  AS OF npageng16.f, COMMON/OBSER AND COMMON/CNST ARE INCLUDED IN
C  THIS ROUTINE SINCE ND AND SIG ARE NEEDED. NOTE THAT TIM IS NOW
C  ALSO PROVIDED VIA COMMON/OBSER, RATHER THAN AS THE ARGUMENT,
C  TIMOBB.


C  THIS ROUTINE IS CALLED BY MAIN TO CALCULATE THE NUMTSUB TIMES TO BE 
C  PUT INTO TPRED, FROM THE NOBSER TIMES IN TIM. THESE NOBSER TIMES
C  IN TIM ARE THE OBSERVED VALUE TIMES FROM A SUBJECT. THE TIMES
C  IN TPRED ARE TO START AT 0, AND CONTINUE UNTIL 24 HOURS AFTER THE
C  LAST TIME BEFORE EACH TIME RESET (EACH TIME OF 0 AFTER THE FIRST 
C  TIME IS A TIME RESET).

C  AS OF npageng16.f, FOR EACH STEADY STATE DOSE SET, TIMES IN TPRED
C  WILL START AT THE END OF IT, RATHER THAN AT 0 ... EXCEPT IF THE
C  STEADY STATE DOSE OCCURS AFTER THE BEGINNING OF THE SUBJECT FILE,
C  ONE TPRED TIME WILL BE SET = 0 SO THE PROGRAM WILL RECOGNIZE THAT
C  THIS IS ALSO THE BEGINNING OF A TIME RESET.

C  FOR EXAMPLE, IF THERE IS A STEADY STATE DOSE SET WITH INTERDOSE
C  INTERVAL = DOSEINT, THEN THE TIMES WILL START AT 100*DOSEINT
C  (SINCE EACH STEADY STATE DOSE SET IS ASSUMED TO HAVE 100 DOSES PLUS
C  ONE ADDITIONAL STARTING DOSE).
C  AS A SPECIFIC EXAMPLE, IF A STEADY STATE DOSE SET WITH INTERDOSE
C  INTERVAL OF 2 HOURS STARTS AT TIME 0 AND THE 1ST OBS. TIME IS AT
C  205, IT REALLY MEANS THAT THE OBSERVATIONS START 5 HOURS AFTER THE
C  END OF THE STEADY STATE DOSE SET. IN THIS CASE, THE TIMES IN TPRED
C  STILL START AT 200. 

C  BUT, AS OF npageng18.f, ALSO ESTABLISH TPREDREL(.) WHICH IS SIMILAR
C  TO TPRED, BUT HAS "RELATIVE" INSTEAD OF "REAL" TIMES AFTER STEADY
C  STATE DOSES. IN THE EXAMPLE ABOVE WITH AN INTERDOSE INTERVAL OF 2 
C  HOURS, THE TPRED(.) VALUES START AT 200 AND THE TPREDREL(.) VALUES
C  START AT 0. AND EACH TPREDREL(I) = TPRED(I) - 200.


C  SET IDOSE = 1; IT WILL BE THE RUNNING DOSE NUMBER. IT MUST BE
C  CHECKED TO SEE WHERE, IF AT ALL, THERE ARE STEADY STATE DOSE SETS.

      IDOSE = 1


C  NOTE THAT THE TIMES ARE TO BE IDELTA MINUTES APART, SUBJECT TO
C  THE CONSTRAINT THAT THE MAXIMUM NO. OF TIMES BE 7200.
C  BUT NOTE BELOW THAT THE TIMES IN TPRED ARE IN HOURS, NOT MINUTES.

C  NOMAXTIMS = NO. OF MAXIMUM TIMES FOR THIS SUBJECT (WHICH EQUALS 
C  1 MORE THAN THE NO. OF TIME RESETS).
C  NUMTSUB WILL BE THE RUNNING NO. OF TIMES ALREADY PUT INTO TPRED.
C  INDEX IS THE RUNNING INDEX OF TIMES ALREADY CONSIDERED IN TIM.
C  TIMMAX IS THE CURRENT MAXIMUM TIME THROUGH TIM(INDEX) SINCE
C  THE LAST TIME RESET VALUE OF 0.

      NOMAXTIMS = 0
      NUMTSUB = 0
      INDEX = 0

   50 TIMMAX = -1.D30

   10 INDEX = INDEX + 1

        IF(TIM(INDEX) .GT. TIMMAX) TIMMAX = TIM(INDEX)
        IF(TIM(INDEX) .LE. 0.D0 .AND. INDEX .GT. 1) GO TO 20

C  ADDED ADDITIONAL REQUIREMENT ABOVE, 'INDEX .GT. 1' IN bigmlt8.f.
C  REASON IS THAT, OTHERWISE, AN INITIAL OBS. TIME OF 0 WILL LOOK
C  LIKE A TIME RESET TO THE PROGRAM AND CAUSE AN EXTRA AUC TABLE IN
C  THE OUTPUT FILE.


        IF(INDEX .EQ. NOBSER) GO TO 20

        GO TO 10

   20   CONTINUE 

C  TO GET HERE MEANS TIMMAX IS THE MAXIMUM OBSERVATION TIME BEFORE A
C  TIME RESET, OR IT IS SIMPLY THE MAXIMUM OBSERVATION TIME IF THERE
C  ARE NO TIME RESETS. ESTABLISH THE APPROPRIATE TIMES IN TPRED FROM 0 
C  TO T_END = TIMMAX + 24. SIMILARLY ESTABLISH THE TIMES IN TPREDREL.


C  !!! AS OF npageng16.f, TPRED VALUES WILL START AT 0 UNLESS THE
C  CORRESPONDING DOSES FOR THIS TIME BLOCK START WITH A STEADY STATE
C  SET. IN THAT CASE, THE TPRED VALUES WILL START FROM THE END OF THE
C  STEADY STATE SET. THE STARTING DOSE TIME IS IN SIG(IDOSE). IF

C  THIS VALUE IS < 0, IT REPRESENTS THE START OF A STEADY STATE DOSE
C  SET, WITH INTERDOSE INTERVAL = -SIG(IDOSE). IF THIS VALUE IS .GE. 0,
C  IT DOES NOT REPRESENT THE BEGINNING OF A STEADY STATE DOSE SET.

C  NOTE THAT, AS INDICATED ABOVE, THE TPREDREL VALUES WILL ALWAYS START
C  AT 0.
  
      TBEG = 0.D0
      IF(SIG(IDOSE) .LT. 0.D0) TBEG = 100.D0*(-SIG(IDOSE))


C  NOTE THAT NUMTSUB TIMES HAVE ALREADY BEEN STORED IN TPRED. PUT IN
C  THE NEXT SET.


	T_END = TIMMAX + 24.D0

c wmy2017Oct10 - included explicit conversion with int()
	NUMT2 = int((T_END - TBEG)*60/IDELTA)

	NUMTSUB = NUMTSUB + 1

C  IF TBEG > 0, IT MEANS THERE IS A STEADY STATE DOSE SET OCCURRING.
C  IF THIS IS NOT AT THE BEGINNING OF THE SUBJECT FILE (I.E., IF 
C  NUMTSUB > 1), THEN ONE PREDICTED VALUE MUST BE SET = 0 (SO
C  SUBROUTINE FUNC3 IN idm3x_.7 WILL KNOW THIS IS ALSO THE BEGINNING OF
C  A TIME RESET), BUT THE REST WILL START AT TBEG.

      IF(TBEG .GT. 0.D0 .AND. NUMTSUB .GT. 1) THEN
       TPRED(NUMTSUB) = 0.D0
       TPREDREL(NUMTSUB) = 0.D0

       NUMTSUB = NUMTSUB + 1
      ENDIF

	TPRED(NUMTSUB) = TBEG
      TPREDREL(NUMTSUB) = 0.D0

c  As of npageng18.f, the new code with TPREDREL --> if there is a 
c  steady state dose set, which is not at the beginning of the patient
c  file, TPREDREL(.) will have two consecutive values set to 0.0 in
c  the above code. This is required since otherwise, the correspondence
c  between TPREDREL(.) and NUMTSUB would be lost.


	DO I=1,NUMT2
	 NUMTSUB = NUMTSUB + 1
	 IF(NUMTSUB .GT. 7200) GO TO 40
	 TPRED(NUMTSUB) = TPRED(NUMTSUB-1) + IDELTA/60.D0
       TPREDREL(NUMTSUB) = TPRED(NUMTSUB) - TBEG
	END DO

C  SAVE THIS MAXIMUM TIME + 24 HOURS INTO TEND. IT WILL BE NEEDED

C  IN MAIN WHERE SUBJECT AUCs ARE CALCULATED. ALSO, AS OF npageng16.f,
C  SAVE THE BEGINNING TIME INTO TBEGG.

	NOMAXTIMS = NOMAXTIMS + 1

C  NOTE THAT TEND IS DIMENSIONED 99 ABOVE, BUT IT IS NOT NECESSARY
C  TO TEST THAT NOMAXTIMS .LE. 99 SINCE IF IT WAS EVEN CLOSE TO THIS
C  VALUE, THE NUMTSUB > 7200 TEST BELOW WOULD BE TRUE AND THIS 
C  SUBROUTINE WOULD BE EXITED.

	TEND(NOMAXTIMS) = T_END
      TBEGG(NOMAXTIMS) = TBEG

C  ESTABLISH THE NEXT DOSE TIME THAT STARTS A TIME RESET (WHICH MAY OR

C  MAY NOT BE THE BEGINNING OF A STEADY STATE DOSE SET).

C      IF(IDOSE .LT. ND) THEN
C       DO ID = IDOSE + 1,ND
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

C  THE ABOVE RETURN IS THE NORMAL EXIT FROM THIS ROUTINE. THE ONLY
C  OTHER WAY OUT IS THROUGH LABEL 40 BELOW, WHICH WILL ONLY HAPPEN IF
C  NUMTSUB EXCEEDS THE MAX. NO. OF ALLOWABLE TIMES TO BE IN TPRED.

	GO TO 50


   40   CONTINUE


C   TO GET HERE --> NUMTSUB > 7200. 


	WRITE(*,2031) JSUB,IDELTA
	WRITE(25,2031) JSUB,IDELTA
 2031    FORMAT(///' FOR SUBJECT NO. ',I4,' THE MAXIMUM NO. OF '/
     1' PREDICTED VALUES (7200) HAS BEEN REACHED. THIS MEANS THAT IN'/
     2' THE DENSITY PART OF THE OUTPUT FILE, AND IN THE PRTB FILE'/
     3' (WHERE THE PREDICTED VALUES ARE WRITTEN ',I3,' MINUTES APART),'/

     4' THIS SUBJECT WILL NOT HAVE A COMPLETE SET OF PREDICTED VALUES.')

	NUMTSUB = 7200	

C	CALL PAUSE

C  NOTE THAT THE REASON NUMTSUB IS LIMITED TO 7200 IS THAT THE 

C  PREDICTED VALUES FOR THESE TIMES MUST BE STORED INTO
C  YPREDPOPT(MAXSUB,NUMEQT,7201,3), AND IF MAXSUB = 999, AND 
C  NUMEQT = 6, THIS WOULD RESULT IN ALMOST 130 MILLION VALUES IN
C  THIS MATRIX. THIS IS PROBABLY TOO MUCH, BUT FOR NOW (WITH MOST
C  MAXSUB VALUES << 999, AND NUMEQT USUALLY .LE. 3), IT IS PROBABLY
C  OK. BUT MAY HAVE TO ADJUST THIS 7200 DOWN FOR RUNS WITH A LARGE
C  NO. OF PATIENT FILES.



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
C  OF THE A1000 FORMAT. INSTEAD MUST WRITE (26,__) READLINE, WHERE
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
      SUBROUTINE NEWWORK1(MAXSUB,JSUB,TIMOBREL,errfil)

       use npag_utils,only: verifyval,orderdelta,thesame,
     1   maxnumeq,max_m_per_obs,max_doses,
     2   max_RS_J

      IMPLICIT REAL*8(A-H,O-Z)

C      PARAMETER(MAXNUMEQ=7)

      DIMENSION SIG(max_doses),RS(max_doses,max_RS_J),
     1 DELTAIV(7),ORDELT(7),
     1 RSS(max_doses,max_RS_J),SIGG(max_doses),
     2 TIM(max_m_per_obs),TIMM(max_m_per_obs),
     3 YO(max_m_per_obs,MAXNUMEQ),
     4 TIMDELAY(99),TIMOBREL(MAXSUB,max_m_per_obs),
     5 OBSBLOCK(800,150,MAXNUMEQ+1),
     6 DOSEBLOCK(800,1000,35),NDORIG(800),XVERIFY(100)

      CHARACTER READLINE*300,ERRFIL*20

   	COMMON/DOSEOBS/DOSEBLOCK,OBSBLOCK,NDORIG
C      COMMON/ERR/ERRFIL 


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
C  AS OF npageng17.f, STEADY STATE DOSES MAY BE BOLUS DOSES. IN THIS
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

C  NOTE ALSO THAT, AS OF npagen18.f, TIMOBREL(JSUB,J), J=1,M, WILL BE
C  STORED AND RETURNED TO MAIN.

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

C  As of npageng18.f, STORE ND INTO NDORIG(JSUB); IT WILL BE PASSED
C  TO SUBROUTINE READOUT VIA COMMON/DOSEOBS.

       NDORIG(JSUB) = ND


       DO I = 1,ND

        READ(23,*) SIG(I),(RS(I,J),J=1,NI)

C  AS OF npageng18.f, STORE THE VALUES IN THE DOSE BLOCK FOR PASSAGE
C  TO SUBROUTINE READOUT VIA COMMON/DOSEOBS.

C  AS OF npageng19.f, RATHER THAN USING BACKSPACE(23), ESTABLISH
C  DOSEBLOCK BY STRAIGHTFORWARD ASSIGNMENTS. THE REASON IS THAT,
C  DEPENDING ON WHICH COMPILER IS USED TO MAKE THE PR PREP PROGRAM
C  (CURRENT ONE IS NPAG108.FOR), IT IS POSSIBLE FOR A DOSE EVENT
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




C  AS OF npageng18.f, STORE IN THE VALUES IN THE OBSERVATION BLOCK FOR
C  PASSAGE TO SUBROUTINE READOUT VIA COMMON DOSEOBS.

  140	 READ(23,1717) READLINE

       IF(READLINE(12:23) .NE. 'NO. OF TOTAL') GO TO 140

       BACKSPACE(23)

       READ(23,*) NUMEQT
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
     1' FROM THE INSTRUCTION FILE, npag103.inp. IF YOU EDITED THIS'/
     2' FILE MANUALLY, PLEASE RERUN THE PC PREP PROGRAM TO HAVE IT'/
     3' PREPARE npag103.inp AGAIN AND THEN RERUN THIS PROGRAM.'//
     4' IF YOU DID NOT MANUALLY EDIT npag103.inp, PLEASE SEND THE'/
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

C  SINCE NUMEQT IS PROVIDED TO THIS ROUTINE IN THE ARGUMENT LIST,
C  JUST READ(23,*) ON NEXT LINE.

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
        TIMOBREL(JSUB,I) = TIM(I)
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

       XVERIFY(1) = SIG(ID)
       XVERIFY(2) = RS(ID,2*IDRUG-1) 
       XVERIFY(3) = RS(ID,2*IDRUG)
       CALL VERIFYVAL(3,XVERIFY)  
      


      IF(RS(ID,2*IDRUG) .LE. 0.D0 .AND. RS(ID,2*IDRUG-1) .GT. 0) THEN
C      WRITE(*,101) ID,SIG(ID),IDRUG,RS(ID,2*IDRUG-1),RS(ID,2*IDRUG)
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
         WRITE(42,101) ID,SIG(ID),IDRUG,RS(ID,2*IDRUG-1),RS(ID,2*IDRUG)
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

C  SINCE NUMEQT IS PROVIDED TO THIS ROUTINE IN THE ARGUMENT LIST,
C  JUST READ(23,*) ON NEXT LINE.

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


C  AS OF npageng27.f, MAKE SURE THAT NO TWO TIMES ARE THE SAME SINCE
C  IF THEY ARE, IT CAN CONFUSE SUBROUTINE SHIFT (CAUSING IT TO GO INTO
C  AN INFINITE LOOP - SEE NPAG115.EXP, TESTCASE 5).

        CALL THESAME(SIGLAST,SIGG(I),ISAME)

        IF(ISAME .EQ. 1) THEN

         XVERIFY(1) = SIGLAST
         CALL VERIFYVAL(1,XVERIFY)



C        WRITE(*,4031) SIGLAST


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
         WRITE(42,4031) SIGLAST
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
C wmy20190317 Moved to npag_utils.f90
C
      SUBROUTINE Old_ORDERDELTA(NDRUG,DELTAIV,NDELTA,ORDELT)

      use npag_utils, only: thesame

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION DELTAIV(7),ORDELT(7),X(7)

C  SUBROUTINE ORDERDELTA IS CALLED BY NEWWORK1 TO OBTAIN NDELTA, THE NO.
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
C wmy20190317 -- Moved to npag_utils.f90
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
C wmy20190315 Moved to npag_utils.f90
C
      SUBROUTINE Old_VERIFYVAL(N,X)
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
        SUBROUTINE CALCRF(NTOTPAR,VEC,FNTVAL,NUMEQT,YO,C0,C1,C2,C3,C4,C5
     1    ,NBCOMP,NDIM,MF,RTOL,ATOL,PCOPY,TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     2    INTLIST,IPAR,ObsError,RPAR,
     3    gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     4    errfilname)

        use npag_utils, only: maxnumeq,max_m_per_obs,max_ODE_params
     1    ,cp_lrcs_to_rpar
     2    ,max_pop_rand_varbs,max_doses,max_ODE_comps,max_RS_J
     3    ,max_input_dim,k_gamma,k_flat,i_is_poisson_obs,i_is_log10

        IMPLICIT REAL*8(A-H,O-Z)

C  THIS SUBROUTINE IS CALLED BY ELDERY TO FIND THE FUNCTIONAL VALUE,
C  FNTVAL, FOR THE SUPPLIED CANDIDATE VECTOR, VEC. ELDERY CALLS THIS
C  SUBROUTINE OVER AND OVER UNTIL IT FINDS THE VECTOR, VEC, WHICH
C  MINIMIZES FNTVAL.

C  FNTVAL IS THE NORMALIZED SUM OF SQ. DIFFERENCES BETWEEN ALL OBSERVED
C  AND PREDICTED VALUES OVER ALL NSUB SUBJECTS, GIVEN THE NTOTPAR 
C  VALUES SUPPLIED IN THE CANDIDATE VECTOR VEC. NOTE THAT THESE VALUES
C  WILL BE ASSIGNED TO THE PARAMETER ENTRIES IN PX(.) WHICH HAVE 
C  IRAN(.) = 2, AND THEN IRAN(.) = 1. THE OTHER PARAMETER VALUES (I.E.,
C  THOSE WHICH HAVE IRAN(.) = 0) WERE ALREADY ASSIGNED TO THE 
C  APPROPRIATE ENTRIES IN PX BEFORE ELDERY WAS CALLED IN MAIN.

C      PARAMETER(MAXNUMEQ=7)

        DIMENSION VEC(NTOTPAR),
     1 YO(max_m_per_obs,NUMEQT),
     2 C0(NUMEQT),C1(NUMEQT),C2(NUMEQT),C3(NUMEQT),C4(NUMEQT),C5(NUMEQT)
C     1 SIG(max_m_per_obs,MAXNUMEQ),

C      COMMON SIG

C      COMMON/TOCALC/gamma,flat,AB,PX,IRAN,NOFIX,NSUB
      double precision gamma, flat
      double precision, dimension(max_pop_rand_varbs,2) :: AB
      double precision, dimension(max_ODE_params) :: PX
      integer, dimension(max_ODE_params) :: IRAN
      integer NOFIX,NSUB

       character*20 errfilname

C wmy2017Sep12 Added
C      COMMON/TOUSER/NDIM,MF,RTOL,ATOL
       integer  NDIM,MF
       real*8 RTOL
       real*8, dimension(max_ODE_comps) :: ATOL
       real*8, dimension(max_m_per_obs) :: TIMCOPY
       real*8, dimension(max_doses) :: SIGCOPY
       real*8, dimension(max_doses,max_RS_J) :: RSCOPY
       real*8, dimension(max_doses,max_input_dim) :: BSCOPY

c wmy2017Sep30
       integer, dimension(128) :: INTLIST
       integer, dimension(257) :: IPAR
       double precision, dimension(max_m_per_obs,MAXNUMEQ) :: ObsError
       double precision, dimension(257) :: RPAR

       integer NNORMALOBS, NPOISSONOBS
C       integer MISVAL

       integer, dimension(max_input_dim) :: NBCOMP

C !$omp ThreadPrivate(/TOCALC/)
C !$omp ThreadPrivate( IPAR, ObsError )

C  COMMON SIG IS USED TO PASS THE VALUES ESTABLISHED IN SIG(.,.)
C  BELOW TO SUBROUTINE FUNC. wmy20190731 Removed COMMON SIG, and
C  replaced with ObsError

C  COMMON/TOCALC VALUES ARE PASSED TO THIS ROUTINE FROM MAIN.


C  AS INDICATED ABOVE, PX(.) HAS THE CORRECT VALUES ALREADY IN PLACE
C  FOR THE PARAMETERS WITH IRAN(.) = 0. NOW INSERT THE CANDIDATE
C  VALUES IN VEC(.) INTO THE ENTRIES IN PX(.) WITH IRAN(.) = 2 AND 1.
C  AND, AS OF npagranfix3.f, ENSURE THE CANDIDATE VALUES FOR THE
C  RANDOM VARIABLES (I.E., THOSE WITH IRAN(.) = 1) ARE INSIDE THEIR
C  RESPECTIVE BOUNDARIES.

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
C  THIS IS RANDOM VARIABLE NO. IRANO. VERIFY THAT ITS CANDIDATE
C  VALUE IS INSIDE ITS BOUNDARIES, [AB(IRANO,1), AB(IRANO,2)]. IF NOT,
C  RETURN A LARGE POSITIVE VALUE, WHICH IS UNATTRACTIVE, FOR FNTVAL.
        VN = VEC(NVEC)
        IF(VN .GT. AB(IRANO,2) .OR. VN .LT. AB(IRANO,1)) THEN
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

       CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,
     1    TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     2    INTLIST,errfilname)

        call cp_lrcs_to_rpar(gamma,flat,numeqt,c0,c1,c2,c3,c4,c5,rpar)

C  SEE COMMENTS IN THE 140 LOOP IN MAIN.

       NPOISSONOBS=0
       NNORMALOBS=0
C       MISVAL=0

C  Initialize IPAR w/type of observation declaration
       DO 140 I=1,NOBSER
        DO 140 J=1,NUMEQT

         Y = YO(I,J)

C  IF YO(I,J) = -99, IT MEANS THAT OUTPUT EQ. J HAD NO VALUE AT 
C  OBSERVATION TIME I. IN THIS CASE, SIG(I,J) WILL NOT BE SET, AND IT
C  OF COURSE WILL NOT BE NEEDED IN SUBROUTINE FUNC.
 
         IF(Y .EQ. -99) GO TO 140

C Are observations recorded as log10(obs) _AND_ in subroutine
C output, are X converted to log10(X)? If so, do you want the
C sd to be calculated on 10^Y(obs or est)?
C             if (C1(J).eq.-10) IPAR(i_is_log10+J) = -10
             if (C4(J).eq.10) IPAR(i_is_log10+J) = 10

C         if (C0(J).eq.-229.and.C2(J).eq.-229
C     1     .and.C3(J).eq.-229) then 
          if (C5(J).eq.229) then
C--------------------------------- Start Poisson

             write (*,*) "Poisson analysis req. for OUTEQ",J
             NPOISSONOBS=NPOISSONOBS+1
C             ObsError(I,J)=1.D0
             ObsError(I,J)=Y
C             SIG(I,J)=1.D0
             IPAR(i_is_poisson_obs+J)=229

C--------------------------------- End Poisson
         else
C--------------------------------- Start NORMAL

C         SIG(I,J) = C0(J)+C1(J)*Y+C2(J)*Y*Y+C3(J)*Y**3
C         if(ierrmod.eq.2) sig(i,j) = sig(i,j)*gamma
C         if(ierrmod.eq.3) sig(i,j)=dsqrt(sig(i,j)**2 + gamma**2)
C         if(ierrmod.eq.4) sig(i,j) = gamma*flat
         ObsError(I,J) = C0(J)+C1(J)*Y+C2(J)*Y*Y+C3(J)*Y**3
         if(ierrmod.eq.2) ObsError(i,j) = ObsError(i,j)*gamma
         if(ierrmod.eq.3) ObsError(i,j)=dsqrt(ObsError(i,j)**2
     &     + gamma**2)
         if(ierrmod.eq.4) ObsError(i,j) = gamma*flat

C         ObsError(I,J) = sig(I,J)

C--------------------------------- End NORMAL
         endif

  140    CONTINUE



C  CALL IDPC, A SUBROUTINIZED VERSION OF THE ADAPT PROGRAM ID3, TO
C  CALCULATE THE SUM OF SQUARES OF DIFFERENCES BETWEEN THE OBSERVED 
C  VALUES AND THE PREDICTED (BY THE MODEL) VALUES, FOR EACH OUTPUT
C  EQUATION, FOR THIS VARIABLE VECTOR, VEC. THESE SUM OF SQUARES ARE
C  EACH NORMALIZED BY THE ASSAY VARIANCE OF EACH OBSERVATION.

        NPX = NVAR+NOFIX+NRANFIX
C       if (NPX < max_ODE_params) then exit progam w/error
C wmy2017Sep12 Added /TOUSER/ varbs to CALL IDPC()
C       CALL IDPC(NPX,PX,W,NOBSER,NUMEQT)

C       write (*,*) "CALL IDPC",NPX,NOBSER,NUMEQT,NDIM,MF

        CALL IDPC(JSUB,IG,NPX,PX,NBCOMP,W,NOBSER,NUMEQT,
     1    NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     2    INTLIST,IPAR,ObsError,RPAR,ERRFILNAME)

C  W RETURNS AS THE SUM OF:
C  ((YO(I,J)-H(I,J))/SIG(I,J))**2, WHERE H(I,J) = PREDICTED VALUE OF THE
C  JTH OUTPUT EQ AT THE ITH OBSERVATION TIME, ASSUMING THE IGTH GRID
C  POINT, X, ... OVER THE NOBSER x NUMEQT QUANTITIES ABOVE WHICH DON'T
C  HAVE YO(I,J) = -99 (WHICH MEANS THAT OUTPUT EQ. J HAS NO OBSERVED
C  LEVEL FOR TIME I).

C  ADD THIS W TO SUMTOT.

       SUMTOT = SUMTOT + W
C
C Note that SUMTOT = sum(z-score^2) over all observations, including
C  those that do not arise from a Normal distribution
C

      END DO
C  THE ABOVE END DO IS FOR THE  DO JSUB = 1,NSUB  LOOP.

      FNTVAL = SUMTOT


	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE ELDERY(N,START,XMIN,YNEWLO,REQMIN,STEP,ITMAX,
     1    FUNC,IPRINT,ICONV,NITER,ICOUNT,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     2    NBCOMP,NDIM,MF,RTOL,ATOL,PCOPY,TIMCOPY,SIGCOPY,RSCOPY,BSCOPY,
     3    INTLIST,IPAR,ObsError,RPAR,gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     4    errfilname)

C  ELDERY DIFFERS FROM ELDERX ONLY IN THE DIMENSION STATEMENT. ALL 5'S
C  ARE CHANGED TO 25'S, AND ALL 6'S ARE CHANGED TO 26'S. THIS ALLOWS 25
C  PARAMETERS INSTEAD OF JUST 5. As of itbig9x.f, we allow as many as
C  max_pop_rand_varbs parameters.

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
C    XSEC  -THE COORDINATES OF THE VERTEX WITH THE 2ND SMALLEST FUNCTION
C             VALUE.
C    YSEC  - THE FUNCTION VALUE AT XSEC.
C
      use npag_utils, only: maxnumeq,max_m_per_obs,max_ODE_params
     1  ,max_pop_rand_varbs,max_doses,max_ODE_comps,max_RS_J
     2  ,max_input_dim

C      IMPLICIT REAL*8(A-H,O-Z)
      implicit none

C ARGUMENT LIST
C
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
      double precision, dimension(max_ODE_params) :: PCOPY
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

C Local Variables

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

C !$omp ThreadPrivate( IPAR, ObsError )

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
C        XN=FLOAT(N)
C        DN=FLOAT(N)
        XN = 1.d0 * N
        DN = 1.d0* N
        FN = 0.d0
        NN=N+1
C
C  CONSTRUCTION OF INITIAL SIMPLEX.
C
1001    DO 1 I=1,N
1       P(I,NN)=START(I)
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     2    NBCOMP,NDIM,MF,RTOL,ATOL,PCOPY,TIMCOPY,SIGCOPY,
     3    RSCOPY,BSCOPY,INTLIST,IPAR,ObsError,RPAR,
     4    gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     5    errfilname)
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
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     2    NBCOMP,NDIM,MF,RTOL,ATOL,PCOPY,TIMCOPY,SIGCOPY,
     3    RSCOPY,BSCOPY,INTLIST,IPAR,ObsError,RPAR,
     4    gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     5    errfilname)
        Y(J)=FN
        ICOUNT=ICOUNT+1
2       START(J)=DCHK

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
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     2    NBCOMP,NDIM,MF,RTOL,ATOL,PCOPY,TIMCOPY,SIGCOPY,
     3    RSCOPY,BSCOPY,INTLIST,IPAR,ObsError,RPAR,
     4    gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     5    errfilname)
        YSTAR=FN
        ICOUNT=ICOUNT+1
        IF(YSTAR.GE.YLO) GO TO 12
        IF(ICOUNT.GE.KCOUNT) GO TO 19
C
C  SUCESSFUL REFLECTION SO EXTENSION.
C
        DO 9 I=1,N
9       P2STAR(I)=ECOEFF*PSTAR(I)+(1.0D0-ECOEFF)*PBAR(I)
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     2    NBCOMP,NDIM,MF,RTOL,ATOL,PCOPY,TIMCOPY,SIGCOPY,
     3    RSCOPY,BSCOPY,INTLIST,IPAR,ObsError,RPAR,
     4    gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     5    errfilname)
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
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     2    NBCOMP,NDIM,MF,RTOL,ATOL,PCOPY,TIMCOPY,SIGCOPY,
     3    RSCOPY,BSCOPY,INTLIST,IPAR,ObsError,RPAR,
     4    gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     5    errfilname)
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
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     2    NBCOMP,NDIM,MF,RTOL,ATOL,PCOPY,TIMCOPY,SIGCOPY,
     3    RSCOPY,BSCOPY,INTLIST,IPAR,ObsError,RPAR,
     4    gamma,flat,AB,PX,IRAN,NOFIX,NSUB,
     5    errfilname)
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
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     2    NBCOMP,NDIM,MF,RTOL,ATOL,PCOPY,TIMCOPY,SIGCOPY,
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













