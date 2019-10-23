c  assranfix1.f

c  assranfix1 has the following changes from asseng16:     4/17/16

c  1. It is an extension. It has the added capability to allow for 
c  RANFIX parameters, i.e., those that are the same for all subjects. 
c  Note that new Subroutines ELDERY2 and CALCRF are added to calculate 
c  these updated estimates.

c  This means that now the user can designate parameters as fixed
c  (IRAN(.) = 0), random (IRAN(.) = 1 or -1), or ranfix (IRAN(.) = 2).

c  2. In Subroutine BESTCS, the restriction on the candidate values
c  for C0P(IEQ) is changed. Previously, C0P(IEQ) .LT. .001 was 
c  disallowed. Now C0P(IEQ) .LT. 0 will be disallowed.

c  3. Formats 7126, 7124, and 7123 are changed to show that the 
c  output files are made by it2branfix1 rather than it2beng27, and
c  they also now show MAR_16 as the date, rather than JUL_11.

c  4. Note that module idm222x15.f has been updated to be idm222x16.f.

c  5. Note that the instruction file made by the new PC Prep program,
c  IT2B114.FOR is now it2b102.inp, updated from it2b101.inp. The
c  difference of course is that it2b102.inp includes RANFIX parameter
c  info.

c-----------------------------------------------------------------------

c  asseng16.f                                              3/28/15

c  asseng16 has the following change from asseng15:

c  All numbers written out in F or G format are now tested to see if
c  they are inside [-1.D-99, 1.D-99]. If so, they are changed to be
c  0. The reason is that otherwise they will be printed out without 
c  the accompanying D or E (e.g., as .934-106, rather than .934E-106).

c  Note that a new Subroutine, VERIFYVAL is added to the code to 
c  do the indicated testing above.

c  Note that the modules linked to this main module are changed as
c  follows:
c  idm11x15.f to idm11x16.f;
c  idm222x14.f to idm222x15.f;
c  shift9.f remains unchanged;
c  vodtot remains unchanged.

c-----------------------------------------------------------------------

c  asseng25.f                                              9/5/14

c  asseng15 has the following change to asseng14:

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

c-----------------------------------------------------------------------

c  asseng14.f                                              7/23/14

c  asseng14 has no functional changes from asseng13. But the ID
c  modules linked with it will be updated.:
c  idm11x14.f to idm11x15.f; and idm222x13.f to idm222x14.f.

c-----------------------------------------------------------------------

c  asseng13.                                               4/9/14

c  asseng13 has the following changes to asseng12:

C  THE MAXIMUM NO. OF OUTPUT EQUATIONS WILL BE CHANGED FROM 6 TO 
C  NUMEQT, WHICH IS SUPPLIED IN THE ARGUMENT LIST TO SUBROUTINE 
C  ASSCALC. THIS MEANS THAT NUMEQT WILL NOW BE PASSED TO ALL THE
C  SUBROUTINES THAT NEED IT; AND IN THOSE SUBROUTINES, ANY 6 REFERRING
C  TO THE MAX. NO. OF OUTPUT EQUATIONS WILL BE CHANGED TO NUMEQT (OR
C  MAXNUMEQ WHICH WILL BE ESTABLISHED AS 7 IN A PARAMETER STATEMENT).

C  ALSO, IN THOSE ROUTINES WHERE ARRAYS ARE PASSED IN COMMON STATEMENTS,
C  OR EXIST ONLY IN THOSE ROUTINES, DIMENSIONS RELATED TO THE 
C  MAX. NO. OF OUTPUT EQS. WILL BE DIMENSIONED BY A PARAMTER STMT.
C  SETTING MAXNUMEQ = 7, THE CURRENT LIMIT (SINCE THESE ARRAYS CANNOT
C  BE VARIABLY DIMENSIONED BY A CALLING ARGUMENT). THIS INCLUDES IN 
C  MAIN, FILRED, AND OTHER ROUTINES IN THE ID MODULES WHERE YOO IS
C  PASSED IN COMMON/OBSER. AND IT INCLUDES SUBROUTINE OUTPUT IN THE NEW
C  TEMPLATE MODEL FILE, TSTMULTM.FOR.

C  THE NEW ID MODULES TO BE LINKED WITH THIS MAIN MODULE ARE
C  idm11x14.f AND idm222x13.f.


c  Note that asseng12.f is the preliminary "engine" module for the new
c  PC prep program, IT2B110.FOR.

c-----------------------------------------------------------------------

c  asseng12.f                                              7/21/13

c  asseng12 has the following changes to asseng11:

c  1. If the program bombs, the message that is written to the screen 
c  will now also be written to the file ERRFIL = ERRORxxxx, where xxxx
c  is the 4-digit run no. In this way, if the program is being run using
c  Pmetrics, the Pmetrics program can respond appropriately. Note that
c  ERRFIL must be passed to all the routines which could write to it
c  using COMMON/ERR/ERRFIL ... except to routine GETIPATF, where it is
c  included as a calling argument (to be consistent with how the code
c  is written in it2beng23.f and npageng24.f).

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

c-----------------------------------------------------------------------

c  asseng11.f                                              10/30/12

c  asseng11 has the following changes from asseng10:

c  1. It will now be linked with shift9.f, rather than shift7.f. The
c  reason is to fix bugs which occurred if a steady state dose had
c  bolus inputs (see details in shift8.f and shift9.f).

c  2. It will be linked with idm11x13.f (updated from idm11x12.f)
c  and idm222x12.f (updated from idm222x11.f). The new id modules fix
c  bugs related to the R(.) array being updated before each call to a 
c  routine that uses it, and also to GETFA being called before the 
c  FA(.) array is used (see details in the id modules).

c  Note that asseng11.f is the main "engine" module for the 
c  preliminary program for the new PC prep program, IT2B108.FOR.

c  Note that there are no changes to this module, but the new
c  name is used since it will be linked with different .f files
c  (see changes 1. and 2. above).

c-----------------------------------------------------------------------

c  asseng10.f                                             4/19/12

c  asseng10.f has the following changes from asseng9.f:

c  It is the main engine module for the preliminary program for
c  IT2B105.FOR. The change is that this program allows steady state
c  dose sets to have bolus doses as well as IVs (rather than being
c  limited to just IVs). 

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

c  In addition, the 2 id modules require code changes, and are updated
c  to be idm11x10.f and idm222x10.f.

c-----------------------------------------------------------------------

c  asseng9.f                                               2/10/12

c  asseng9 has the following changes from asseng8:

c  It is part of the IT2B104.FOR program, which now restricts 
c  variable values in Subroutine MAPBAYS to be positive if their
c  IRAN(.) value is 1. This means that the parameter estimates for
c  variables whose IRAN(.) value is 1 will be non-negative. 

c  See changes in Subroutine MAPABYS and MAKEVEC.

c  In particular, the it2b101.inp instruction file format will not 
c  change, but now, IRAN(I) = 0 if parameter I is fixed; = -1 if
c  parameter I is a variable and may be negative; = 1 if parameter I is
c  a variable and may not be negative.

c-----------------------------------------------------------------------

c  asseng8.f                                               2/5/12

c  asseng8 has the following changes from asseng7:

c  1. It is the assay engine module for the new PC prep program, 
c  IT2B103.FOR. The new IT2B103.FOR/asseng8.f program now allows
c  patient files to have steady state dose sets. In particular, this
c  program now calls new subroutine NEWWORK1 (based on the stand-a-lone
c  program of the same name) to read each working copy file in 
c  it2b101.inp, which may have steady state dose indicator lines, and
c  convert it to the typical form that that the id routines require, 
c  except that the steady steady state dose indicators themselves
c  (negative dose times) remain in the file.

c  2. vodtot.f is not changed, but the other 3 permanent engine .f
c  files linked to this program, shift6.f, idm11x6.f, and idm222x6.f,
c  will be updated to be shift7.f, idm11x8.f, and idm222x8.f. The
c  changes in the id routines will allow them to read steady state dose
c  indicators and know how to integrate through each set, testing for
c  convergence of the steady state compartment amounts to see if the 
c  program can fast forward to the end of that set. The changes in
c  shift7.f are dimensions changes (500 to 5000), and some edited code
c  required since now a dose (time) reset occurs when a dose time is
c  .LE. 0, rather than .EQ. 0.

c  3. All 500's related to dose times dimensions (which are passed in
c  COMMON/OBSER) are changed to 5000's to be compatible with the new
c  id routines. This is because in those routines, because of steady
c  state dose sets, the no. of dose times can expand to be much 
c  bigger than 500. All 500's will also be changed to 5000 in 
c  Subroutine FILRED.

c-----------------------------------------------------------------------

c  asseng7.f                                               7/18/11

c  asseng7 has the following changes from asseng6:


c  Since it is now a part of the IT2B101.FOR program, it will input
c  the file it2b101.inp rather than it2bbig5.inp. This program does not
c  need the new info in the new file (CSVFILE, NDRUG, and 
c  AF(I),I=1,NDRUG), but it must read the info (or through it at least)
c  to get the info it does need.

c-----------------------------------------------------------------------

c  asseng6.f                                                4/26/11

c  asseng6 has the following change from assbig5:

c  This is the main "preliminary program" module in the program whose 
c  PC Prep program is the new IT2B100.FOR. As in that .FOR module, the
c  formula for NI in Subroutine FILRED is changed from  
c  NI = 2*NDRUG + 2 + NADD  to  
c  NI = 2*NDRUG + NADD, because from now on, WT and CCR will not be 
c  considered special covariates. If they are included in the working 
c  copy file, they will be part of the NADD 'additional' covariates 
c  (beyond the 4 permanent ones in Common DESCR).


c-----------------------------------------------------------------------

c  assbig5.f                                               12/30/10

c  assbig5 is the same, functionally, as assbig4z. The difference 
c  is that it is linked with idm11x6.f (updated from idm11x5.f) and
c  idm222x6.f (updated from idm222x5.f). The other permanent .f files, 
c  shift5.f, and vodtot.f are unchanged.

c  Note that the first PC Prep program to use the assbig5.f "engine"
c  is ITBIG11.FOR

c-----------------------------------------------------------------------

c  assbig4z.f                                              11/23/10

c  assbig4z has the following change to assbig4y:

C  A CALL TO NEW SUBROUTINE PAUSE REPLACES EACH PAUSE STATEMENT. 
C  THIS IS BECAUSE A PAUSE STATEMENT CAUSES A WARNING WHEN THE PROGRAM
C  IS COMPILED AND LINKED USING gfortran (AND IT FORCES THE USER TO 
C  TYPE "go" INSTEAD OF SIMPLY HITTING THE ENTER KEY.

c  Note that the first PC Prep program to use the itbig9z.f "engine"
c  is ITBIG10.FOR.

c-----------------------------------------------------------------------

c  assbig4y.f                                                  11/16/10

c  assbig4y has the following changes to assbig4:

c  1. In Subroutine MAPBAYS, the code which returns a large 
c  (unattractive) functional value if an entry in VEC is .LE. 0 is 
c  removed since this program may now be used with parameters which are
c  logs of other parameters (and therefore may be negative).

c  2. One of the modules of this assay estimating engine is idm22x5.f,
c  which is now updated to idm222x5.f. The difference is
c  that PMAT is now dimensioned (594,30) in this new module, which is
c  consistent with this main module, rather than (594,20). This small
c  bug apparently never caused a problem, but it could.

c-----------------------------------------------------------------------

c  assbig4.f                                               8/12/10  

c  assbig4 has the following changes from assbig33:


c  1. It is the main "engine" module for the preliminary program which
c  is used to obtain optimum estimates for the assay coefficients for
c  the ITBIG9.FOR program. ITBIG9 is at the same level as the Big NPAG
c  program, NPBG15E1.FOR (and this module is at the same level as
c  itbig9x.f). i.e., it can now be used for multiple drugs.

c  2. This program will be linked with a new set of modules as follows:

c  idfxd51f.f is changed to idm11x5.f, which is the same as idm1x5.f of
c  the bigmlt6.f "engine" (for Big NPAG), except that rather than 
c  returning the total sum of squares over all the M x NOS observed 
c  values (SUMSQ), it returns SUMSQJ(J), J=1,NOS, where
c  each SUMSQ(J) = is the sum of squares just for output equation J.
c  The individual SUMSQJ(J) are needed in this program - see logic
c  regarding SSND(.).

c  idcp_3f.f is changed to idm22x5.f, which is the same as idm2x5.f of
c  the bigmlt6.f "engine" (for Big NPAG), except it has the same
c  differences from idm2x5.f that idcp_3f.f had from idcy_53f.f (see 
c  code at the top of idm22x5.f for the details).

c  shift2.f is changed to shift5.f, the same modules used in itbig9x.f
c  and the bigmlt6.f "engine" (or Big NPAG).

c  vodtot.f is unchanged.

c  3. The template model file will now be TSTMULTG.FOR, just as it is
c  for the Big NPAG program. The id modules above are compatible with
c  TSTMULTG.FOR.

c  4. A new instruction file will be needed. The new name will be
c  it2bbig5.inp, which will be different from it2bbig3.inp in that
c  new info (NSUBTOT, IPATVEC) will be included, and old info 
c  (PRFIX2,EXT2) will be eliminated.

c  5. Many changes are made to write onto scratch file 27 the active
c  NSUB out of the total of NSUBTOT patients available - this code is 
c  the same as that used in itbig9x.f and the Big NPAG "engine", 
c  bigmlt6.f.

c  6. Subroutine FILRED is redone - it's the same version as in the 
c  Big NPAG "engine", bigmlt6.f (so it can read the multiple drug
c  working copy patient files).

c  7. ALL 25's in DIMENSION statements which are related to NVAR are
c  changed to 30'S (in all routines). In addition all 12's related to 
c  the max. no. of fixed variables are changed to 20's.

c  8. PAUSE commands are put below every STOP command so if this program
c  is called by a Windows GUI, the user will be able to see why the
c  program stopped before the Window closes.

c  9. New subroutines, GETIPATF, GETNUMSF, and GETSUB are added. They
c  are all the same as those in bigmlt6.f, the Big NPAG "engine", and
c  itbig9x.f.

c 10. READLINE is changed from *72 to *300, and the corresponding 
c  formats in all routines are changed from A72 to A300.

c-----------------------------------------------------------------------


c  assbig33.f					              7/29/07

c  assbig33 is the same as assbig3 except that the no. of random variables
c  is now .LE. 25 rather than .LE. 20. Also, the no. of fixed parameters
c  is now .LE. 7 rather than .LE. 12.

c  The PC prep programs for this "engine" is ITBIG7Y.FOR.

c-----------------------------------------------------------------------

c  assbig3.f							1-28-00

c  assbig3 is exactly the same as assbig2. The only change is that it is
c  linked with new modules on the supercomputer. idfxd51c.f is replaced
c  by idfxd51d.f; and idcp_3c.f is replaced by idcp_3d.f. These 2 new 
c  modules have updated code to allow both of the following:

c  1. the IDIFF = 0 option, which bypasses the calling of USERANAL (and 
c  its calls to DIFFEQ). Instead the value(s) for the output(s) will be 
c  coded explicitly into SUBROUTINE OUTPUT. Also, SUBROUTINE OUTPUT has 
c  an additional argument, the time at which the output value(s) is(are) 
c  desired.

c  2. initial conditions of the amounts in the compartments to be set 
c  = paramater values, rather than always fixed = 0.0. To enable this,
c  SUBROUTINE SYMBOL in the Fortran model file (see, e.g., INITCOND.FOR) 
c  now contains an additional COMMON/INITCOND/IC, and the user sets 
c  IC(I) = J for each compartment, I, which will have its initial amount 
c  set = value of parameter J. This info is passed to the above 2 id 
c  modules. Also, the dimension of P in OUTPUT and DIFFEQ is changed to 
c  32.

c  Note that the 2 new id modules above have a lot of code 
c  simplification (see notes in their code). In particular, the
c  square root transformations are no longer done --> the results
c  for this program will differ possibly a little from previous
c  results.

c-----------------------------------------------------------------------

c  assbig2.f							11-13-99

c  assbig2 has the following changes to assbig1:

c  1. NTLAG is now added to COMMON/CNST in subroutine FILRED. 
c  This change, and others, are required for all modules in this 
c  program, which allow time lags.

c  2. A bug is fixed in the reading of it2bbig3.inp. Previously, if
c  there was more than 1 fixed parameter name, the correct no. of lines 
c  would not be ignored (fixed parameter names are not needed in this
c  program). Now this has been fixed.

c-----------------------------------------------------------------------

c  assbig1.f							10-8-99

c  assbig1.f has the following changes to assay_3a.f:

C  1. THE CONVERGENCE CRITERION IS CHANGED TO BE BASED ON THE ABSOLUTE
C  VALUES OF AVGLOG - OLDAVG, INSTEAD OF JUST AVGLOG - OLDAVG. 
C  PREVIOUSLY, IF THE LOG-LIKELIHOOD DECREASED FROM ONE CYCLE TO THE
C  NEXT, CONVERGENCE WAS ACHIEVED. NOW, CONVERGENCE WON'T BE ACHIEVED
C  UNLESS THE ABS. VALUE OF THE LOG-LIK'S OF 2 CONSECUTIVE CYCLES IS
C  WITHIN TOL = .001.

C  2. The input file has been renamed it2bbig3.inp (from it2bbig2.inp),
c  since ILOG is included in this file (ILOG is not used in this program
c  and a blank line is read in in its place).

c-----------------------------------------------------------------------

c  assay_3a.f							5-23-99

c  assay_3a has all the changes assay_4 and assay_5 made to assay_3, 
c  except that the assay s.d.'s are still functions of the observed
c  values, rather than the predicted values.

C  THE CHANGES ARE:

c  1. RS(500,14) DIMENSIONS ARE CHANGED TO RS(500,34), TO ALLOW UP TO
c  30 USER-SUPPLIED COVARIATES.

C  2. CODE IN REGANAL IS CHANGED TO UPDATE THE MEAN VECTOR EXPLICITLY
C  BEFORE RETURNING TO MAIN. PREVIOUSLY, THIS WASN'T DONE, BUT THE 
C  UPDATED VALUES WERE RETURNED TO MAIN VIA COMMON/TOMAP. THIS WAS 
C  UNINTENDED AND SLOPPY, BUT CORRECT. NOW, THE CODE IN REGANAL SHOWS 
C  CLEARLY THAT THE UPDATING DOES OCCUR.

C  3. EXTERNAL MAPBAYS IS REMOVED FROM MAIN; IT WAS NEVER NEEDED.

c-----------------------------------------------------------------------

c  assay_3.f							8-30-98

c  assay_3.f has the following changes from assay_2.f:

C  1. IT ALLOWS MULTIPLE OUTPUTS. THERE WILL BE NUMEQT OUTPUT EQUATIONS.
C  NUMEQT IS PASSED TO THIS ROUTINE (ASSCALC) BY assdriv.f (MADE BY
C  M1_6.FOR) IN THE ARGUMENT LIST. 

C  ALL 150 DIMENSIONS, WHICH WERE THE MAX. NO. OF OUTPUT VALUES, HAVE 
C  BEEN CHANGED TO 594 (SINCE THE MAX. NO. OF OBSERVATIONS IS NOW 99 
C  OBSERVATIONS/OUTPUT EQ. x 6 OUTPUT EQS.).

C  2. INPUT FILE it2bbig1.inp IS RENAMED it2bbig2.inp. it2bbig2.inp
C  HAS NEW INFO FOR THE MULTIPLE OUTPUTS, AND ALSO NOW ALL UNNEEDED
C  INFO IS NO LONGER READ IN (INSTEAD EMPTY READ STATMENTS READ EMPTY 
C  LINES IN SUCH CASES). IN PARTICULAR, QVAL, TOL, MAXIT, XDEV, INDPTS, 
C  INFIL, PREFIX, EXT, PRFIX2, EXT2, PARFIX, AND AF ARE NOT READ IN.
C  ALSO NUMEQT IS NOT READ IN FROM FILE it2bbig2.inp, SINCE IT IS PASSED 
C  IN THE ARGUMENT LIST TO THIS ROUTINE, SUBROUTINE ASSCALC.

C  3. SUBROUTINES REGANAL, BESTCS, FILRED, MAPBAYS, ELDERY (BUT NOT
C  ELDERY2), CALCOV, MULT1, AND MULT2 HAVE CHANGES. THEY ALL HAVE 
C  DIMENSION CHANGES, PLUS:

C  -  SUBROUTINES REGANAL, BESTCS, AND FILRED HAVE NUMEROUS CHANGES 
C     RELATED TO MULTIPLE OUTPUTS.

C  -  SUBROUTINE MAPBAYS IS CHANGED SO THAT PARTIAL SUMS W(IEQ),
C     IEQ=1,NUMEQT ARE RETURNED FROM SUBROUTINE IDPC (idfxed51.f, 
C     CHANGED FROM idfixed5.f). THESE PARTIAL SUMS ARE NOT NEEDED IN
C     THIS PROGRAM, BUT IN ORDER FOR THIS PROGRAM TO BE LINKED WITH THE
C     SAME MODULE (idfxed51.f) AS m1_4calc.f, THE CHANGES WERE MADE.
C     NOTE THAT ALL REFERNCES TO SSND (PREVIOUSLY PASSED FROM MAPBAYS
C     BACK TO REGANAL AND BESTCS) ARE REMOVED. SSND WAS NEVER NEEDED
C     BY REGANAL AND BESTCS (IT IS USED IN m1_4calc.f TO UPDATE 
C     ESTIMATES OF GAMMA(IEQ), WHICH IS N/A IN THIS PROGRAM).

C  -  SUBROUTINE CALCOV IS CHANGED SINCE RINV IS PASSED IN THE ARGUMENT
C     LIST FROM MAIN, RATHER THAN CALCULATED.

C  4. NOTE THAT WITH MULTIPLE OUTPUTS, MISSING VALUES MUST BE ALLOWED.
C  I.E., not all output equations will necessarily have observed levels 
C  at all observation times. An observed level which is "missing" has 
C  the value -99 in its entry. 

C  5. THIS PROGRAM READS, FROM it2bbig2.inp, WHICH OF THE NUMEQT OUTPUT
C  EQUATIONS ARE TO HAVE THEIR C'S ESTIMATED. IN PARTICULAR:

C    IF IGAMMA(IEQ) = 2, THEN C0P(IEQ),...,C3P(IEQ) WILL BE THE STARTING
C    GUESSES FOR THE POPULATION VALUES TO BE ESTIMATED BY THIS PROGRAM. 
C    IF IGAMMA(IEQ) = 0 OR 1, THEN C0P(IEQ),...,C3P(IEQ) WILL BE UNUSED 
C    AND EACH PATIENT WILL BE ASSIGNED C'S FROM HIS PATIENT FILE.

C  6. THE FINAL RESULTS (WRITTEN AT THE BOTTOM OF THE OUTPUT FILE) ARE 
C  CHANGED TO BE COMPATIBLE WITH M1_6'S CODE TO READ THEM IN. IN 
C  PARTICULAR, ALL SETS OF C'S, BOTH THOSE THAT WERE ESTIMATED, AND 
C  THOSE WHICH WEREN'T, ARE WRITTEN, ONE SET TO A LINE, TO THE FILE.

C  7. THIS MODULE WILL BE LINKED WITH MODULES, idfxed51.f, idcp_3.f, 
C  AND vodtot.f. idcp_3.f (CHANGED FROM idcp_2.f) AND idfxed51.f 
C  (CHANGED FROM idfixed3.f) ARE ALSO USED IN m1_4calc.f AND ARE NEW; 
C  AND vodtot.f IS UNCHANGED.

c-----------------------------------------------------------------------

c  assay_2.f							4-24-98

c  assay_2.f has the following changes from assay_1.f:

c  1. This module is now SUBROUTINE ASSCALC, called by assdriv.f, which
c  is created by the PC preparation program (M1_4.FOR is the current
c  version). This change was made so this program (and it2bdriv.f/
c  SUBROUTINE BIGIT2B) would be similar in style to npemdriv.f 
c  (npemdriv.f had to do this dynamic allocation of dimensions so the 
c  no. of grid points could be determined dynamically by the no. of 
c  subjects and random parameters).

c  assdriv.f, the "MAIN" module, HAS A PARAMETER STATEMENT WHICH 
c  DEFINES THE PARAMETERS WHICH ESTABLISH THE DIMENSIONS IN THE VARIABLY 
c  DIMENSIONED ARRAYS. IT THEN HAS THE STATEMENT:
C  CALL ASSCALC( ...) , WHERE ALL VARIABLY DIMENSIONED ARRAYS (AND THE
C  VARIABLE PARAMETER DIMENSIONS) ARE PASSED IN THE ARGUMENT LIST.

C  NOTE THAT MANY MORE ARRAYS NOW ARE VARIABLY DIMENSIONED, INCLUDING
C  SOME IN SUBROUTINE REGANAL, WHICH HAS EXTRA VALUES ADDED TO ITS
C  ARGUMENT LIST FOR THAT PURPOSE.


c  2. Subroutine FILRED is changed to be same as in m1_3calc.f, as
c  follows:

c   a. AGE, ISEX, HEIGHT, and IETHFLG are input 
c  for each subject, and these values are now passed to subroutines 
c  DIFFEQ and OUTPUT, part of the fortran file created by the 
c  boxes-type program (or made manually). This fortran file is part 
c  of assdriv.f, uploaded by the user after running the preparation 
c  program (M1_4.EXE is the current version). These values are passed 
c  via COMMON/DESCR. The new boxes program is BOXNEW3.PAS, changed from 
c  BOXESNEW.PAS.

c   b. Since RS must store all the psuedo "rates"
c  (i.e., all the covariate info input in the dosage regimen), RS 
c  dimensions have been increased from (500,8) to (500,14). Since
c  RS is passed via COMMON/OBSER, modules idfixed3.f (changed from
c  idfixed2.f), and idcp_2.f (changed from idcp_1.f) must be changed 
c  correspondingly. Also, these modules have changes related to a 
c  "bug" correction regarding IDIFF.

C  3. THE SAME CHANGE THAT THE OTHER PROGRAMS (E.G., M1_4.FOR) MADE
C  TO CHANGE F AND E FORMATS TO G FORMATS IS MADE, ALTHOUGH IN THIS
C  PROGRAM THERE ARE ONLY A COUPLE OF AFFECTED FORMATS.

C  4. INPUT FILE file01.inp IS RENAMED it2bbig1.inp. IQVAL AND INDPTS
C  ARE NEW INPUT VALUES ... UNNEEDED IN THIS PROGRAM.

C  5. AN EXTRA WRITE STATEMENT TO THE OUTPUT FILE IS ADDED IN 
C  SUBROUTINE CALCPIK. 

C  6. FORMAT 9032 HAS BEEN CHANGED, AND A NEW FORMAT 9033 HAS BEEN 
C  ADDED.

C  7. TWO WRITE STATEMENTS TO THE OUTPUT FILE IN SUBROUTINE REGANAL
C  HAVE BEEN REMOVED (THEY WERE N/A).

C  8. FORMAT 9999 IN BESTCS HAS BEEN CHANGED. 

c-----------------------------------------------------------------------

c  assay_1.f							10-9-97

c  assay_1.f is the mainframe calculation program which finds the
c  'best' assay coefficients, which can then be input into the PC 
c  preparation program (M1_1.FOR is the first in the series) for the
c  main mainframe program (m1_1calc.f is the first in the series).

c  Note that the triple, M1_1.FOR + assay_1.f + m1_1calc.f, in
c  combination, represent 'BIG FRONT-END', the preliminary program to
c  'BIG NPEM'.

c  This program triple is based on MXEM1S55.FOR, THE PC FRONT-END, but,
c  now, the user is no longer limited to the basic 3-compartment
c  pharmacokinetic model, with two menus of parameters. Instead, the
c  model is coded by the user into a fortran file which is read by 
c  M1_1.FOR, and then uploaded to the mainframe to be compiled with
c  this module. In line with this, the calculations for P(YJ|X) still
c  require the calling of subroutine IDPC, but the new IDPC no longer
c  gives analytic solutions to compartment concentrations; instead IDPC 
c  will obtain these values by solving the differential equations in the 
c  above fortran file.

c  Note that M1_1.FOR + assay_1.f + m1_1calc.f is 'at the same level'
c  as the 'BIG NPEM' program, M2_13.FOR + m2_13cal.f.

C-----------------------------------------------------------------------

      SUBROUTINE ASSCALC(MAXSUB,MAXDIM,NUMEQT,PAREST,IESTIJ)

      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(MAXNUMEQ=7)

      DIMENSION PAREST(MAXSUB,MAXDIM),IESTIJ(MAXSUB,MAXDIM),XVERIFY(100)


C  NOTE THAT ALL THE DIMENSIONS = 25 BELOW 'SHOULD' BE CHANGED TO 
C  MAXDIM, BUT SINCE THESE ARRAYS ARE SO SMALL, CHANGING THEM TO 
C  VARIABLY DIMENSIONED ARRAYS (WHICH REQUIRE PASSING THE ARRAYS AND
C  MAXDIM THROUGH ALL RELATED CALLING STATEMENTS) IS NOT WORTH IT.
C  ... EXCEPT THAT START AND STEP SHOULD HAVE BEEN DIMENSIONED 4 
C  PREVIOUSLY, SINCE, IN MAIN, THEY SIMPLY REFER TO THE ASSAY 
C  COEFFICIENTS, C0P, ..., C3P. NOW, THEY WILL BE DIMENSIONED 24 SINCE
C  THERE MAY BE AS MANY AS 6 OUTPUT EQUATIONS, EACH WITH 4 C'S.


C  SIMILARLY FOR IRAN(32), SINCE 32 = 25 (MAXDIM) + 7 (MAX. NO. OF
C  FIXED PARAMETERS).

C  AS OF assay_3.f:
C  NOTE THAT ALL DIMENSIONS = 150 HAVE BEEN CHANGED TO 594 (THIS IS
C  APPLICABLE TO THE SUBROUTINES ONLY SINCE THERE WERE NO 150'S IN 
C  DIMENSION STATEMENTS IN MAIN), SINCE THIS NO. REPRESENTS THE TOTAL 
C  NO. OF OBSERVATIONS (AND THE MAX. NO IS 6 OUTPUT EQUATIONS x 99 
C  OBSERVATIONS/EQ). 

        DIMENSION AB(30,2),VALFIX(20),START(24),STEP(24),ESTCOV(30,30),
     1  ESTMEN(30),ESTMENN(30),ESTINV(30,30),IRAN(32),ATOL(20),
     2  CBEST(24),C0P(NUMEQT),C1P(NUMEQT),C2P(NUMEQT),RANFIXEST(20),
     3  C3P(NUMEQT),IGAMMA(MAXNUMEQ),IPATVEC(9999),AF(7)

C  NOTE THAT THE DIMENSION OF IGAMMA IS MAXNUMEQ, RATHER THAN
C  NUMEQT. THE REASON IS THAT THIS ARRAY IS PASSED IN COMMON AND
C  THEREFORE CAN ONLY BE VARIABLY DIMENSIONED BY A VALUE SET IN
C  A PARAMETER STATEMENT. 

 
	CHARACTER PAR(30)*11,OUTFIL*20,NAME*4,READLINE*300,ERRFIL*20

   	COMMON/TOUSER/NDIM,MF,RTOL,ATOL
	COMMON/TOMAP/IRAN,VALFIX,SIGFAC,OFAC,ESTMEN,ESTINV,
     1  DET,NOFIX,NUMEQTT,RANFIXEST,NRANFIX

	COMMON/TOBESTC/ESTMENN,ESTCOV,NSUB,NVAR,IGAMMA
        COMMON/ERR/ERRFIL 

	EXTERNAL BESTCS

        integer JSUB, IG
        integer, dimension(128) :: INTLIST
        integer, dimension(257) :: IPAR
        double precision, dimension(257) :: RPAR

	NUMEQTT = NUMEQT


C  NOTE ABOVE THAT NUMEQTT MUST BE DIFFERENT THAN NUMEQT BECAUSE ONE IS
C  A DUMMY ARGUMENT PASSED TO THIS ROUTINE, AND THE OTHER IS PASSED
C  IN A COMMON STATEMENT.

C  COMMON/TOUSER IS SUPPLIED TO SUBROUTINE USERANAL IN idfixed.f.
C  COMMON/TOMAP/ IS SUPPLIED TO SUBROUTINES REGANAL, BESTCS, AND
C				MAPBAYS. 
C  COMMON/TOBESTC IS SUPPLIED TO SUBROUTINE BESTCS.
C  COMMON/ERR/ IS SUPPLIED TO ALL THE ROUTINES WHICH COULD WRITE TO
C   ERRFIL.


    2 FORMAT(A20)
  222 FORMAT(A3)
 2222 FORMAT(A5)


C-----------------------------------------------------------------------

C  INPUT FILE it2b102.inp FROM THE PREPARATION PHASE OF THIS PROGRAM,
C  CONTAINS THE USER DESIRED PARAMETER VALUES, ALONG WITH THE 
c  CONCATENATED PATIENT DATA FILES (IN ADAPT FORMAT).

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
C	               NO ! PARFIX NO LONGER READ IN (IT IS NOT NEEDED).

C  NRANFIX = NO. OF UNKNOWN PARAMETERS WHICH ARE THE SAME FOR 
C            ALL SUBJECTS FOR THE RUN.

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

C  NOTE THAT NUMEQT IS INPUT TO THIS ROUTINE, ASSCALC, BY THE CALLING
C  PROGRAM, assdriv.f, AND IS THEREFORE NOT READ IN FROM THE INPUT
C  FILE.

C  FOR IEQ = 1,NUMEQT:

C  IGAMMA(IEQ) = 2 IF THE C'S FOR OUTPUT EQ. IEQ ARE TO BE ESTIMATED;
C		 1 OR 0 OTHERWISE.

C  C0P(IEQ),..., C3P(IEQ) = INITIAL GUESSES FOR THE C'S FOR OUTPUT
C	EQ. IEQ (USED BY THE NELDER MEED ALGORITHM) ... IF 
C	IGAMMA(IEQ) = 2; BUT UNUSED IF IGAMMA(IEQ) = 1 OR 0 (SINCE IF 
C	IGAMMA(IEQ) = 1 OR 0, THE C'S TO BE USED IN THIS PROGRAM ARE IN 
C	THE PATIENT DATA FILES).

C  XSIG, WHERE THE INITIAL POPULATION STD. DEV. ESTIMATE FOR EACH 
C        PARAMETER WILL BE XSIG*(AB(I,2) - AB(I,1)). 

C  NOTE THAT EMPTY LINES ARE READ FOR THE LINES WHICH CONTAIN QVAL, TOL,
C  MAXIT, XDEV, INDPTS, AND ILOG, ALL OF WHICH ARE UNNEEDED IN THIS 
C  PROGRAM.

C  NDRUG = NO. OF DRUGS.
C  AF(I), I=1,NDRUG = ACTIVE (SALT) FRACTION OF DRUG I.
C  NOTE THAT NDRUG AND AF(.) ARE NOT NEEDED IN THIS PROGRAM, BUT ARE
C  READ IN TO ENSURE SUBSEQUENT INFO IS READ IN PROPERLY.

C  TOLCS GIVES THE STOPPING TOLERANCE FOR THE NELDER-MEED ALGORITHM
C  DURING THE OPTIMIZATION OVER THE ASSAY COEFFICIENTS.

CCCCCCCCCCCCCCCCCCCCCCCC  INPUT INFO (ABOVE) CCCCCCCCCCCCCCCCCCCC

 2227 FORMAT(A11)

c  The output file from this program will always be of the form 
c  ASSxxxx, where xxxx is the 4-character representation of the integer 
c  currently in file extnum in the working directory. Get this integer 
c  now, and replace the value by 1 greater (unless it is 9999, in which 
c  case replace it by 1) and then close extnum.

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

C  CREATE OUTPUT FILE WHICH HAS 'ASS' AS ITS 1ST 3 CHARACTERS AND
C  NAME AS ITS LAST 4. SIMILARLY CREATE ERRFIL.

	OUTFIL = 'ASS'//NAME
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



C  NOTE THAT THE LINE WITH NUMEQT ON IT IS SKIPPED BELOW, SINCE NUMEQT
C  IS SUPPLIED IN ARGUMENT LIST TO THIS ROUTINE, ASSCALC, FROM 
C  assdriv.f. SIMILARLY, QVAL, TOL, MAXIT, XDEV, INDPTS, PREFIX, EXT, 
C  PARFIX, AND AF ARE NOT NEEDED AND ARE SKIPPED IN THE 
C  INPUT FILE.

 4710	  READ(23,*) NDIM
        READ(23,*) MF
        READ(23,*) RTOL
        READ(23,*) (ATOL(I),I=1,NDIM)
   
        READ(23,2222) PREFIX
	  READ(23,222) EXT
        READ(23,*)

        READ(23,*) NVAR
        READ(23,2227) (PAR(I),I=1,NVAR)
        READ(23,*) NOFIX
        READ(23,*)

C  IGNORE LINES WITH PARFIX(I),I=2,NOFIX. PARFIX(1) IS ON ABOVE LINE.
C  NOTE THAT PARFIX(.) ARE NOT USED IN THIS PROGRAM.

		IF(NOFIX .GT. 1) THEN
		 DO I = 2,NOFIX
		  READ(23,*)
	 	 END DO
		ENDIF	 


        READ(23,*) NRANFIX
        READ(23,*) 

C  IGNORE LINES WITH PARRANFIX(I),I=2,NRANFIX. PARRANFIX(1) IS ON ABOVE
C  LINE. NOTE THAT PARRANFIX(.) ARE NOT USED IN THIS PROGRAM.

		IF(NRANFIX .GT. 1) THEN
		 DO I = 2,NRANFIX
		  READ(23,*)
	 	 END DO
		ENDIF	 


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

		READ(23,*) 

		DO I=1,NUMEQT
		 READ(23,*) IGAMMA(I),C0P(I),C1P(I),C2P(I),C3P(I)
		END DO

		READ(23,*) XSIG
		READ(23,*) 
		READ(23,*) NDRUG
            READ(23,*) (AF(I),I=1,NDRUG) 
            READ(23,*) 
		READ(23,*) 
            READ(23,*)
		READ(23,*) TOLCS
		READ(23,*) 
		READ(23,*)


c  The patient data info is read in from it2b102.inp, and is put onto 
c  scratch file 27 (because it will need to be reread each cycle --> 
c  file 27 will be rewound each cycle).


c  Note that there are NSUBTOT subjects, but only NSUB of them,
c  with indices IPATVEC(I),I=1,NSUB, will be put onto file 27.
        OPEN(27)

 1717   FORMAT(A300)

        NLAFIR = 0

        DO JSUB = 1,NSUB

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

       CALL NEWWORK1 

        END DO

C  THE ABOVE END DO CLOSES THE  DO JSUB = 1,NSUB  LOOP.


1730   REWIND(27)
       CLOSE(23)


	OPEN(25,FILE=OUTFIL)


C  ESTABLISH NEST = NO. OF OUTPUT EQS. WHOSE C'S ARE TO BE ESTIMATED
C  DURING THE FOLLOWING OUTPUTTING OF INFO. NOTE THAT IF NUMEQT = 1,
C  NEST MUST = 1 (I.E., IF THERE IS ONLY 1 OUTPUT EQ., IT MUST HAVE
C  ITS C'S ESTIMATED - OTHERWISE, THIS PROGRAM WOULDN'T HAVE BEEN
C  RUN).

	IF(NUMEQT .EQ. 1) THEN

	 WRITE(25,9803)
	 WRITE(*,9803)
 9803    FORMAT(//' THERE IS 1 OUTPUT EQUATION IN THE PATIENT DATA '/
     1' FILES. THIS "PRELIMINARY" PROGRAM IS ABOUT TO CALCULATE THE'/
     2' "OPTIMUM" ESTIMATES FOR THE ASSAY COEFFICIENTS [C0,C1,C2,C3]'/
     3' OF THIS OUTPUT EQUATION - THEY ARE ASSUMED TO BE THE SAME FOR'/
     4' ALL SUBJECTS.'/
     5' THE INITIAL ESTIMATES FOR THE COEFFICIENTS ARE SHOWN BELOW: ')

	WRITE(25,2402)
	WRITE(*,2402)

 2402   FORMAT(/' OUTPUT EQ.                 INITIAL ESTIMATES       '/)
                                       
	ENDIF

	IF(NUMEQT .GT. 1) THEN

	WRITE(25,9801) NUMEQT
	WRITE(*,9801) NUMEQT 
 9801    FORMAT(//' THERE ARE ',I1,' OUTPUT EQUATIONS IN THE PATIENT'/
     1' DATA FILES. OF THESE, THIS "PRELIMINARY" PROGRAM IS ABOUT TO '/
     2' CALCULATE THE "OPTIMUM" ESTIMATES FOR THE ASSAY COEFFICIENTS'/
     3' [C0,C1,C2,C3] OF THE FOLLOWING OUTPUT EQUATION(S) (FOR EACH OF'/
     4' THESE OUTPUT EQUATION(S), THE COEFFICIENTS ARE ASSUMED TO BE '/
     5' THE SAME FOR ALL SUBJECTS):'/
     6' THE INITIAL ESTIMATES FOR THE COEFFICIENTS ARE SHOWN BELOW: ')

	WRITE(25,2402)
	WRITE(*,2402)

	ENDIF

C  REPLACE WRITING OF C0P(),...,C3P() WITH XVERIFY (SEE LOGIC IN 
C  SUBROUTINE VERIFYVAL.


	 NEST = 0
	 DO IEQ=1,NUMEQT
	  IF(IGAMMA(IEQ) .EQ. 2) THEN
         XVERIFY(1) = C0P(IEQ)
         XVERIFY(2) = C1P(IEQ)
         XVERIFY(3) = C2P(IEQ)
         XVERIFY(4) = C3P(IEQ)
         CALL VERIFYVAL(4,XVERIFY)
	   NEST = NEST+1
C	   WRITE(25,9804) IEQ,C0P(IEQ),C1P(IEQ),C2P(IEQ),C3P(IEQ)
         WRITE(25,9804) IEQ,(XVERIFY(IXV),IXV=1,4)
C	   WRITE(*,9804) IEQ,C0P(IEQ),C1P(IEQ),C2P(IEQ),C3P(IEQ)
         WRITE(*,9804) IEQ,(XVERIFY(IXV),IXV=1,4)
	  ENDIF
	 END DO

 9804     FORMAT('   ',I1,3X,4(G16.10,2X))

	WRITE(25,9806)
	WRITE(*,9806)

 9806   FORMAT(//' DURING EACH ITERATION YOU WILL SEE CURRENT INFO. '/
     1' AS IT IS BEING CALCULATED.'//
     5' THE TOLERANCE FOR THE NELDER MEED ALGORITHM IS: ')
	WRITE(25,*) TOLCS
	WRITE(*,*) TOLCS
	WRITE(25,9802)
	WRITE(*,9802)
 9802   FORMAT(//' FOR THE DIFFERENTIAL EQUATION MODULE (VODE), THE'/
     1' FOLLOWING TWO LINES GIVE THE TOLERANCES. THE 1ST LINE GIVES '/
     2' RELATIVE TOLERANCE PARAMETER; THE 2ND LINE GIVES THE ABSOLUTE'/
     3' TOLERANCE PARAMETERS: ')
	WRITE(*,*) RTOL
	WRITE(*,*) (ATOL(I),I=1,NDIM)
	WRITE(25,*) RTOL
	WRITE(25,*) (ATOL(I),I=1,NDIM)


C  THE C'S WILL BE ESTIMATED IN AN ITERATIVE 
C  FASHION WITH THE ACTUAL ANALYSIS. THAT IS, THE C'S WILL BE ESTIMATED 
C  BELOW (GIVEN THE USER-INPUT VALUES FOR THE BOUNDARIES); THEN THE 
C  ANALYSIS WILL PROCEED (A MAXIMUM OF 4 CYCLES), ENDING WITH THE FINAL 
C  AVGLOG, AND THE FINAL CYCLE ESTIMATES FOR THE MEAN VECTOR AND COV. 
C  MATRIX; THEN THE C'S WILL BE RE-ESTIMATED, GIVEN THESE FINAL CYCLE 
C  ESTIMATES; THEN THE ANALYSIS WILL PROCEED, ETC ... THE ITERATIONS 
C  CONTINUE UNTIL AVGLOG HAS CONVERGED (INCREASING BY NO MORE THAN .001
C  FROM THE PREVIOUS ITERATION'S VALUE) OR UNTIL THE NO. OF ITERATIONS
C  REACHES A MAXIMUM (SET = 10 FOR NOW).

C  INITIALIZE ESTMEN AND ESTCOV TO VALUES OBTAINED FROM INPUT BOUNDARY 
C  VALUES.

C  ESTABLISH THE INITIAL POPULATION MEAN ESTIMATES TO BE THE MIDRANGE 
C  VALUES FROM THE BOUNDARIES INPUT BY THE USER ABOVE.

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

C  NOTE THAT ESTMENN (SET = ESTMEN BELOW) AND ESTCOV ARE PROVIDED TO 
C  SUBROUTINE BESTCS VIA COMMON/TOBESTC.

C  NOTE THAT TOLCS, READ IN ABOVE, GIVES THE TOLERANCE FOR THE NELDER
C  MEED ALGORITHM.


C  PRIOR TO LOOP 5500, PRESET OLDAVG (THE PREVIOUS ITERATION'S AVERAGE
C  OF THE NEGATIVE FUNCTIONAL VALUES RETURNED FROM ELDER, OVER ALL NSUB 
C  SUBJECTS) TO A LARGE NEGATIVE NUMBER. THIS WILL ENSURE THAT THE 
C  CONVERGENCE CRITERION CANNOT BE MET AFTER JUST ONE ITERATION 
C  (SEE LOGIC AT END OF LOOP).

	OLDAVG=-1.D30


	DO 5500 ITER=1,10


	WRITE(25,3217) ITER 
	WRITE(*,3217) ITER 
 3217   FORMAT(////' [C0,C1,C2,C3] x ANALYSIS ITERATION NO. ',I4,'.'//)

	WRITE(25,3218)
	WRITE(*,3218)
 3218   FORMAT(/' --------  [C0, C1, C2, C3]  CANDIDATE(S) --------  '/)

C  IF ITER = 1, ESTMEN AND ESTCOV WERE CALCULATED 
C  ABOVE FROM THE USER-INPUT BOUNDARIES. IF ITER > 1, ESTCOV AND ESTMEN 
C  ARE THE FINAL CYCLE ESTIMATES FROM THE PREVIOUS ITERATION (AS
C  CALCULATED BY REGANAL). SET ESTMENN = ESTMEN. ESTMENN 
C  AND ESTCOV ARE PROVIDED TO SUBROUTINE BESTCS IN COMMON/TOBESTC

C  PREPARE TO CALL ELDERY.

	DO I=1,NVAR
	 ESTMENN(I) = ESTMEN(I)
	END DO

C  ESTABLISH THE NEST SETS OF INITIAL VALUES FOR THE C'S TO BE
C  ESTIMATED. THEY'RE ORDERED SO THAT THE 1ST 4 VALUES IN START
C  ARE FOR THE C'S OF THE 1ST EQ. TO BE ESTIMATED; THE NEXT 4 ARE
C  FOR THE C'S OF THE 2ND EQ. TO BE ESTIMATED; ETC. SINCE THE C'S OF
C  NEST EQS. ARE TO BE ESTIMATED, THE TOTAL NO. OF C'S TO BE ESTIMATED
C  IS 4*NEST.

	IST = -4

	DO IEQ=1,NUMEQT
	 IF(IGAMMA(IEQ) .EQ. 2) THEN
	  IST = IST + 4
	  START(IST+1) = C0P(IEQ)
	  START(IST+2) = C1P(IEQ)
	  START(IST+3) = C2P(IEQ)
	  START(IST+4) = C3P(IEQ)
	 ENDIF
	END DO

	DO I=1,4*NEST
         STEP(I)= -.2D0*START(I)
	END DO

	CALL ELDERY(4*NEST,START,CBEST,VALMIN,TOLCS,STEP,1000,BESTCS,
     1  0,ICONV,NITER,ICNT,JSUB,IG,INTLIST,RPAR,IPAR)

C  CBEST CONTAINS THE VALUES OF [C0,C1,C2,C3] FOR EACH OF THE OUTPUT
C  	 EQS. WHOSE C'S WERE ESTIMATED. THESE C'S GIVE THE LARGEST 
C	 AVGLOG AFTER 1 ITERATION OF THE REGULAR ALGORITHM (SEE 
C	 DESCRIPTION OF AVGLOG BELOW). 

C  VALMIN = MIN. VALUE OF FUNCTION ACHIEVED, SO -VALMIN = MAXIMUM 
C	    AVGLOG OBTAINED. THIS VALUE NOT CURRENTLY USED.

C  ICONV = 1 IF ELDERY CONVERGED; 0 OTHERWISE.

	IF(ICONV .EQ. 0) THEN
	 WRITE(*,9021)
	 WRITE(25,9021)
 9021 FORMAT(/' NO CONVERGENCE ON SELECTION OF BEST [C0,C1,C2,C3].'/)
	ENDIF


	IST = -4

	DO IEQ=1,NUMEQT
	 IF(IGAMMA(IEQ) .EQ. 2) THEN
	  IST = IST + 4
	  C0P(IEQ) = CBEST(IST+1) 
	  C1P(IEQ) = CBEST(IST+2) 
	  C2P(IEQ) = CBEST(IST+3) 
	  C3P(IEQ) = CBEST(IST+4) 
	 ENDIF
	END DO


	WRITE(25,9032) 
	WRITE(*,9032)
 9032   FORMAT(/' THE CURRENT BEST SET(S) OF [C0,C1,C2,C3] IS(ARE):')

C  REPLACE WRITING OF C0P(),...,C3P() WITH XVERIFY (SEE LOGIC IN 
C  SUBROUTINE VERIFYVAL.

      DO IEQ=1,NUMEQT
	 IF(IGAMMA(IEQ) .EQ. 2) THEN
        XVERIFY(1) = C0P(IEQ)
        XVERIFY(2) = C1P(IEQ)
        XVERIFY(3) = C2P(IEQ)
        XVERIFY(4) = C3P(IEQ)
        CALL VERIFYVAL(4,XVERIFY)
C	  WRITE(25,9033) IEQ,C0P(IEQ),C1P(IEQ),C2P(IEQ),C3P(IEQ)
        WRITE(25,9033) IEQ,(XVERIFY(IXV),IXV=1,4)
C       WRITE(*,9033) IEQ,C0P(IEQ),C1P(IEQ),C2P(IEQ),C3P(IEQ)
        WRITE(*,9033) IEQ,(XVERIFY(IXV),IXV=1,4)
	 ENDIF
	END DO

 9033   FORMAT(' ','EQ. ',I1,': ',4(G14.8,1X))

C  NOTE THAT ESTMEN RETURNS AS THE UPDATED MEAN ESTIMATE. PREVIOUS TO
C  assay_3a.f, ESTMEN'S VALUES WERE NOT CHANGED EXPLICITLY IN 
C  SUBROUTINE REGANAL; THEY WERE CHANGED VIA COMMON/TOMAP. BUT, AS OF
C  assay_3a.f, THE CODE IN REGANAL EXPLICITLY UPDATES THE VALUES IN
C  ESTMEN (HAVING THE VALUES UPDATED VIA COMMON/TOMAP WAS UNINTENDED, 
C  AND SLOPPY, THOUGH CORRECT).

 	CALL REGANAL(NVAR,NDIM,MAXSUB,MAXDIM,PAREST,IESTIJ,C0P,C1P,C2P,
     1  C3P,PAR,NSUB,IGAMMA,ESTMEN,ESTCOV,AVGLOG,NUMEQT)

	IF(DABS(AVGLOG - OLDAVG) .LT. 1.D-3) GO TO 5510
	OLDAVG = AVGLOG
	

 5500   CONTINUE


 5510   CONTINUE


C  WRITE THE C'S TO FILE 25 AND STOP. NOTE THAT ALL NUMEQT
C  SETS OF C'S ARE INCLUDED, INCLUDING THOSE WHICH WERE ESTIMATED AND
C  THOSE WHICH WERE NOT.

	WRITE(25,1011) NUMEQT
 1011   FORMAT(//' ESTIMATES FOR [C0,C1,C2,C3] FOR EACH OF THE ',I1,' OU
     1TPUTS ARE: ')

C  REPLACE WRITING OF C0P(),...,C3P() WITH XVERIFY (SEE LOGIC IN 
C  SUBROUTINE VERIFYVAL.

      DO IEQ=1,NUMEQT
       XVERIFY(1) = C0P(IEQ)
       XVERIFY(2) = C1P(IEQ)
       XVERIFY(3) = C2P(IEQ)
       XVERIFY(4) = C3P(IEQ)
       CALL VERIFYVAL(4,XVERIFY)
C	 WRITE(25,9999) C0P(IEQ),C1P(IEQ),C2P(IEQ),C3P(IEQ)
       WRITE(25,9999) (XVERIFY(IXV),IXV=1,4)
      END DO
 9999 FORMAT(' ',4(G14.8,1X))

	CLOSE(25)
	CLOSE(27)
	STOP
	END

C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE ELDERY(N,START,XMIN,YNEWLO,REQMIN,STEP,
     X  ITMAX,FUNC,IPRINT,ICONV,NITER,ICOUNT)

C  AS OF assay_3.f, WILL NEED AS MANY AS 24 PARAMETERS (6*4 SETS OF C'S) 
C  INSTEAD OF 20. As of assbig4.f, we allow as many as 30 parameters.

C  ELDERY DIFFERS FROM ELDERX ONLY IN THE DIMENSION STATEMENT. ALL 5'S
C  ARE CHANGED TO 25'S, AND ALL 6'S ARE CHANGED TO 26'S. THIS ALLOWS 25
C  PARAMETERS INSTEAD OF JUST 5.

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

      IMPLICIT REAL*8(A-H,O-Z)

        DIMENSION START(N),STEP(N),XMIN(N),XSEC(30),
     X  P(30,31),PSTAR(30),P2STAR(30),PBAR(30),Y(31)

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
        CALL FUNC(N,START,FN)
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
        CALL FUNC(N,START,FN)
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
        CALL FUNC(N,PSTAR,FN)
        YSTAR=FN
        ICOUNT=ICOUNT+1
        IF(YSTAR.GE.YLO) GO TO 12
        IF(ICOUNT.GE.KCOUNT) GO TO 19
C
C  SUCESSFUL REFLECTION SO EXTENSION.
C
        DO 9 I=1,N
9       P2STAR(I)=ECOEFF*PSTAR(I)+(1.0D0-ECOEFF)*PBAR(I)
        CALL FUNC(N,P2STAR,FN)
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
        CALL FUNC(N,P2STAR,FN)
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
        CALL FUNC(N,XMIN,FN)
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
        CALL FUNC(N,XMIN,FN)
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
     1  IRAN(32),VALFIX(20),PX(32),W(MAXNUMEQ),RANFIXEST(20)

	COMMON/TOMAP/IRAN,VALFIX,SIGFAC,OFAC,ESTMEN,ESTINV,
     1  DET,NOFIX,NUMEQT,RANFIXEST,NRANFIX

      integer JSUB,IG
      integer, dimension(128) :: INTLIST
      integer, dimension(257) :: IPAR
      double precision, dimension(257) :: RPAR


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

C  FNTVAL = -LOG(P(Y|VEC)) - LOG(VEC|ESTMEN,ESTINV), WHERE Y IS THE
C	    VECTOR OF OBSERVED VALUES FOR THIS SUBJECT (PASSED IN
C	    COMMON BLOCK TO IDPC).


C  1ST CHECK THAT ALL THE ENTRIES IN VEC WHICH ARE REQUIRED TO BE
C  NON-NEGATIVE ARE. IF ANY ISN'T, RETURN A LARGE POSITIVE VALUE (AN
C  UNATTRACTIVE VALUE) FOR FNTVAL. 

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


C  CALCULATE  -LOG(P(Y|VEC)), CALLED PYXLOG BELOW.

C  CALL IDPC, A SUBROUTINIZED VERSION OF THE ADAPT PROGRAM ID3, TO
C  CALCULATE THE SUM OF SQUARES OF DIFFERENCES BETWEEN THE OBSERVED 
C  VALUES AND THE PREDICTED (BY THE MODEL) VALUES, FOR EACH OUTPUT
C  EQUATION, FOR THIS VARIABLE VECTOR, VEC. THESE SUM OF SQUARES ARE
C  EACH NORMALIZED BY THE ASSAY VARIANCE OF EACH OBSERVATION.

C  BEFORE ALL CALLS TO IDPC, MUST INTEGRATE FIXED, RANDOM, AND
C  RANFIX VALUES INTO PX, USING IRAN(I),I=1,NVAR+NOFIX+NRANFIX. CALL 
C  MAKEVEC TO DO THIS.

	CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,RANFIXEST,PX)
	CALL IDPC(JSUB,IG,PX,W,INTLIST,RPAR,IPAR)

C  W(J), J=1,NUMEQT RETURNS, WHERE W(J) IS THE SUM OVER I=1,NOBSER, OF:
C  ((YO(I,J)-H(I,J))/SIG(I,J))**2, WHERE H(I,J) = PREDICTED VALUE OF THE
C  JTH OUTPUT EQ AT THE ITH OBSERVATION TIME, ASSUMING VEC = TRUE VECTOR
C  OF VARIABLE VALUES. I.E., W(J) IS THE NORMALIZED SUM OF SQUARES FOR 
C  THE OBSERVATIONS OF THE JTH OUTPUT EQUATION. NOTE THAT MISSING VALUES
C  (I.E., YO(I,J) = -99) DO NOT CONTRIBUTE TO THE ABOVE SUMS OF SQUARES.

C  NOTE THAT THE INDIVIDUAL W(IEQ) ARE NOT NEEDED IN THIS PROGRAM (AS 
C  THEY ARE IN PROGRAM it2branfix1.f), BUT TO BE CONSISTENT WITH THAT
C  PROGRAM, W(IEQ) WILL BE OBTAINED, AND THEN WTOTAL (WHICH IS ALL THAT
C  IS NEEDED) WILL BE CALCULATED BELOW.

	WTOTAL = 0.D0
	DO IEQ=1,NUMEQT
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

	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT)

C  FILRED IS CALLED BY MAIN TO READ THE PORTION OF 
C  SCRATCH FILE 27 WHICH APPLIES TO THE SUBJECT UNDER CONSIDERATION. THE
C  'POINTER' FOR FILE 27 IS IN THE PROPER POSITION TO BEGIN READING THE
C  INFO FOR THE DESIRED SUBJECT.

        IMPLICIT REAL*8(A-H,O-Z)
        PARAMETER(MAXNUMEQ=7)

        DIMENSION TIM(594),SIG(5000),RS(5000,34),YO(594,NUMEQT),
     1  BS(5000,7),C0(NUMEQT),C1(NUMEQT),C2(NUMEQT),C3(NUMEQT),
     2  YOO(594,MAXNUMEQ)

C  AS OF itbig9x.f, BS(500,3) HAS BECOME BS(500,7).

C  NOTE THAT THIS ROUTINE IS THE SAME AS THAT IN bigmlt6.f, THE BIG NPAG
C  PROGRAM.

c  AS OF asseng6.f, THE FORMAT FOR THE WORKING COPY FILES IS:

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
 
C  READ THE NO. OF DRUGS FROM THE LINE WITH 'NO. OF DRUGS' AS ENTRIES
C  12:23. THEN READ NO. OF ADDITIONAL COVARIATES, AND THE NO. OF DOSE 
C  EVENTS, ETC.

    1   FORMAT(A300)
   10	  READ(27,1) READLINE
        IF(READLINE(12:23) .NE. 'NO. OF DRUGS') GO TO 10
        BACKSPACE(27)


    3   FORMAT(T2,I5)
        READ(27,3) NDRUG

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

C  THIS SUBROUTINE CALCULATES COVINV WHERE COVINV 
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

	IF(DET .LE. 0) THEN
	 WRITE(*,*)' PIK IS SINGULAR IN CALCPIK.'
	 WRITE(25,*)' PIK IS SINGULAR IN CALCPIK.'
	ENDIF

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
      SUBROUTINE Old_MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,X,
     2   VALFIX,RANFIXEST,PX)

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
	SUBROUTINE BESTCS(NC,VEC,FNTVAL)
	IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(MAXNUMEQ=7)

	DIMENSION VEC(NC),ESTMEN(30),ESTINV(30,30),YO(594,MAXNUMEQ),
     1  SIG(594,MAXNUMEQ),START(30),STEP(30),ESTCOV(30,30),THETA(30),
     2  B(30,1),VALFIX(20),ESTMENN(30),IRAN(32),IGAMMA(MAXNUMEQ),
     3  GAMMA(MAXNUMEQ),C0P(MAXNUMEQ),C1P(MAXNUMEQ),C2P(MAXNUMEQ),
     4  C3P(MAXNUMEQ),C0(MAXNUMEQ),C1(MAXNUMEQ),C2(MAXNUMEQ),
     5  C3(MAXNUMEQ),XVERIFY(100),RANFIXEST(20)

      COMMON/TOBESTC/ESTMENN,ESTCOV,NSUB,NVAR,IGAMMA
      COMMON SIG
     
	COMMON/TOMAP/IRAN,VALFIX,SIGFAC,OFAC,ESTMEN,ESTINV,
     1  DET,NOFIX,NUMEQT,RANFIXEST,NRANFIX

      integer JSUB,IG
      integer, dimension(128) :: INTLIST
      integer, dimension(257) :: IPAR
      double precision, dimension(257) :: RPAR

      EXTERNAL MAPBAYS

C  COMMON/TOBESTC IS SUPPLIED FROM MAIN.
C  THE BLANK COMMON ABOVE IS SUPPLIED TO SUBROUTINE IDPC. 
C  COMMON/TOMAP/ IS SUPPLIED TO SUBROUTINE MAPBAYS.

C  THIS ROUTINE, CALLED BY ELDERY, FINDS THE FUNCTIONAL VALUE, FNTVAL
C  FOR THE SUPPLIED VARIABLE VECTOR, VEC.

C  FNTVAL = - AVGLOG, WHERE AVGLOG IS THE AVERAGE LOG-LIK OF THE
C	    REGULAR ITS ALGORITHM AFTER 1 ITERATION.


C  NOTE: MANY OF THE COMMENTS HAVE BEEN REMOVED IN THIS ROUTINE. SEE
C        MAIN FOR MORE DETAILS.


C  ESTABLISH THE C'S FOR THE OUTPUT EQS. WHOSE C'S ARE BEING ESTIMATED.

C  AT THE SAME TIME, DISALLOW THE FOLLOWING, FOR ALL IEQ=1,NUMEQT:
C  C0P(IEQ) .LT. .001; C2P(IEQ) .LT. 0; C3P(IEQ) .LT. 0.
C  I.E., RETURN IMMEDIATELY WITH A LARGE (UNATTRACTIVE VALUE FOR ELDERY) 
C  VALUE FOR FNTVAL IF ANY OF THE ABOVE RESTRICTIONS ARE VIOLATED.

C  CHANGE AS OF assranfix1.f! NOW DISALLOW C0P(IEQ) .LT. 0, RATHER THAN
C  C0P(IEQ) .LE. .001.

C  ALSO, ESTABLISH NEST = NO. OF SETS OF C'S WHICH ARE BEING ESTIMATED.

	NEST = 0
	IBAD = 0

	IST = -4

	DO IEQ=1,NUMEQT
	 IF(IGAMMA(IEQ) .EQ. 2) THEN
	  NEST = NEST + 1
	  IST = IST + 4
	  C0P(IEQ) = VEC(IST+1) 
	  IF(C0P(IEQ) .LT. 0.D0) IBAD = 1
	  C1P(IEQ) = VEC(IST+2) 
	  C2P(IEQ) = VEC(IST+3) 
	  IF(C2P(IEQ) .LT. 0.D0) IBAD = 1
	  C3P(IEQ) = VEC(IST+4)
 	  IF(C3P(IEQ) .LT. 0.D0) IBAD = 1
	 ENDIF
	END DO

	IF(IBAD .EQ. 1) THEN
	 FNTVAL = 1.D30
	 RETURN
	ENDIF


C  ESTABLISH ESTMEN = ESTMENN (NEEDED TWO DIFFERENT NAMES SINCE THEY
C  ARE IN TWO DIFFERENT COMMON STATEMENTS). NOTE THAT ESTCOV WAS 
C  PROVIDED DIRECTLY IN COMMON/TOBESTC ABOVE.

	DO I=1,NVAR
	 ESTMEN(I) = ESTMENN(I)
	END DO

	DO IEQ=1,NUMEQT
	 GAMMA(IEQ) = 1.D0
	END DO


C  REWIND FILE 27, WHICH HAS THE PATIENT DATA FILES CONCATENATED
C  ON IT.

	REWIND(27)

	DO 700 JSUB=1,NSUB

C  CALL SUBROUTINE FILRED TO READ, FOR THIS SUBJECT, FROM SCRATCH FILE
C  27, THE NO. OF OBSERVATION TIMES (NOBSER) AS WELL AS THE 
C  OBSERVED VALUES THEMSELVES: YO(I,IEQ) = THE 'NOISY' OBSERVED VALUES 
C  FOR THIS SUBJECT; I=1,NOBSER, IEQ=1,NUMEQT. THESE OBSERVED VALUES ARE 
C  USED ONLY TO CALCULATE THE ASSAY STANDARD DEVIATIONS -- USING THE 
C  VECTORS, C0,C1,C2,C3 (WHICH GIVE THE VALUES CURRENTLY IN THE PATIENT
C  DATA FILE) FOR THOSE OUTPUT EQS. WHICH ARE NOT HAVING THEIR C'S 
C  ESTIMATED, AND USING VECTORS C0P,C1P,C2P,C3P (POPULATION VECTORS) FOR 
C  THOSE OUTPUT EQS. WHICH ARE HAVING THEIR C'S ESTIMATED.

C  NOTE THAT IF YO(I,IEQ) = -99 BELOW, OUTPUT EQ. IEQ HAS NO OBSERVED 
C  LEVEL FOR OBSERVATION TIME I), AND THIS ENTRY WILL BE IGNORED IN THE
C  ANALYSIS.

	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT)

	DO 140 IEQ=1,NUMEQT
      	 DO 140 I=1,NOBSER

	  Y = YO(I,IEQ)

C  IF Y = -99, IT MEANS THAT OUTPUT EQ. J HAD NO VALUE AT OBSERVATION
C  TIME I. IN THIS CASE, IGNORE THIS Y AND INCREASE MISVAL(J) BY 1.

	  IF(Y .EQ. -99) THEN
	   GO TO 140
	  ENDIF

	  IF(IGAMMA(IEQ) .EQ. 2) 
     1 SIG(I,IEQ) = GAMMA(IEQ)*(C0P(IEQ) + C1P(IEQ)*Y + C2P(IEQ)*Y*Y + 
     2              C3P(IEQ)*Y**3)

	  IF(IGAMMA(IEQ) .NE. 2) 
     1 SIG(I,IEQ) = GAMMA(IEQ)*(C0(IEQ) + C1(IEQ)*Y + C2(IEQ)*Y*Y + 
     2              C3(IEQ)*Y**3)

C  ENSURE THAT NO SUBJECT HAS AN OBSERVATION WITH S.D. .LE. .001. WITH 
C  THE RESTRICTION THAT NO S.D. CAN BE .LE. 0, THE PROGRAM CAN STILL 
C  SELECT C'S WHICH MAKE A S.D. VERY VERY CLOSE TO 0 ... AND ONE RUN 
C  DID JUST THAT. THEN, IN THE MAIN PROGRAM, THIS CAN RESULT IN MATRIX 
C  PIK BEING SINGULAR.

      IF(SIG(I,IEQ) .LE. 1.D-3) THEN
	FNTVAL = 1.D30
	RETURN
      ENDIF

  140   CONTINUE

  700   CONTINUE


C  REWIND FILE 27, WHICH HAS THE PATIENT DATA FILES CONCATENATED
C  ON IT.

	REWIND(27)


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

C  NOTE THAT DET CANNOT BE .LE. 0 SINCE ESTCOV ABOVE WAS SET = A 
C  DIAGONAL MATRIX.


	SUMLOG=0.D0

C  SUMLOG IS THE RUNNING SUM (OVER ALL THE SUBJECTS) OF THE 
C  LOG-LIKLIHOODS OF THE PARAMETER ESTIMATES, GIVEN THE DATA AND THE
C  CURRENT ESTIMATES OF THE POPULATION MEAN VECTOR AND COVARIANCE
C  MATRIX (SUMLOG = SUM(-VALMIN), VALMIN RETURNS FROM ELDERY).


      DO 1000 JSUB=1,NSUB

	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT)

C  NOTE THAT C0,C1,C2,C3 ARE THE C'S CURRENTLY IN THIS PATIENT'S DATA
C  FILE. THEY ARE NEEDED FOR THOSE OUTPUT EQS., IEQ, WHERE IGAMMA(IEQ)
C  .NE. 2 (FOR IEQ, WHERE IGAMMA(IEQ) = 2, CURRENT POPULATION VALUES,
C  C0P,...,C3P, SET FROM VEC ABOVE, WILL BE USED).

	MISTOT = 0

	SIGFAC=1.D0

	DO I=1,NOBSER
      	 DO IEQ=1,NUMEQT
          Y=YO(I,IEQ)

	  IF(Y .EQ. -99) THEN
	   MISTOT = MISTOT + 1
	  ENDIF

	  IF(Y .NE. -99) THEN
	   IF(IGAMMA(IEQ) .EQ. 2) 
     1 SIG(I,IEQ) = GAMMA(IEQ)*(C0P(IEQ) + C1P(IEQ)*Y + C2P(IEQ)*Y*Y + 
     2              C3P(IEQ)*Y**3)
	   IF(IGAMMA(IEQ) .NE. 2) 

     1 SIG(I,IEQ) = GAMMA(IEQ)*(C0(IEQ) + C1(IEQ)*Y + C2(IEQ)*Y*Y + 
     2              C3(IEQ)*Y**3)
      	   SIGFAC=SIGFAC*SIG(I,IEQ)
	  ENDIF

	 END DO
        END DO

        OFAC=2.506628274631**(NOBSER*NUMEQT - MISTOT)


C  FIND THE UPDATED ESTIMATES OF THE PARAMETER VECTOR, THETA(I), I=1,
C  NVAR, FOR THIS SUBJECT.

C  PREPARE TO CALL ELDERY2, WHICH IS THE SAME AS ELDERY, BUT MUST BE
C  A DIFFERENT ROUTINE SINCE ELDERY CALLS THIS ROUTINE.

C  THE INITIAL ESTIMATES FOR EACH THETA(I), I=1,NVAR, IS ESTMEN(I).

	DO I=1,NVAR
	 START(I)=ESTMEN(I)
         STEP(I)= -.2D0*START(I)
	END DO

	CALL ELDERY2(NVAR,START,THETA,VALMIN,1.D-10,STEP,1000,MAPBAYS,
     1  0,ICONV,NITER,ICNT,JSUB,IG,INTLIST,RPAR,IPAR)

	SUMLOG = SUMLOG + (-VALMIN)


 1000   CONTINUE

	AVGLOG = SUMLOG/NSUB 
	FNTVAL = -AVGLOG

C  AVGLOG IS NOW THE AVERAGE LOG-LIKELIHOOD OF  
C  P(YJ,THETA|ESTMEN, ESTCOV), WHERE ESTMEN AND ESTCOV ARE THE INITIAL
C  ESTIMATES (INPUT BY THE USER) OF THE POPULATION MEAN VECTOR 
C  AND COVARIANCE MATRIX, RESPECTIVELY, OVER ALL SUBJECTS, AND THETA
C  IS A GIVEN SUBJECT'S MAP-BAYESIAN ESTIMATE. 

	WRITE(*,999) NEST
	WRITE(25,999) NEST
  999   FORMAT(/' THE NEXT ',I1,' LINE(S) GIVE(S) THE CANDIDATES: ')

C  REPLACE WRITING OF C0P(),...,C3P() WITH XVERIFY (SEE LOGIC IN 
C  SUBROUTINE VERIFYVAL.

      DO IEQ=1,NUMEQT
       IF(IGAMMA(IEQ) .EQ. 2) THEN
       XVERIFY(1) = C0P(IEQ)
       XVERIFY(2) = C1P(IEQ)
       XVERIFY(3) = C2P(IEQ)
       XVERIFY(4) = C3P(IEQ)
       CALL VERIFYVAL(4,XVERIFY)
C	 WRITE(*,9999) C0P(IEQ),C1P(IEQ),C2P(IEQ),C3P(IEQ)
       WRITE(*,9999) (XVERIFY(IXV),IXV=1,4)
C      WRITE(25,9999) C0P(IEQ),C1P(IEQ),C2P(IEQ),C3P(IEQ)
       WRITE(25,9999) (XVERIFY(IXV),IXV=1,4)
	 ENDIF
	END DO

 9999   FORMAT(' ',4(G14.8,1X))

	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE ELDERY2(N,START,XMIN,YNEWLO,REQMIN,STEP,
     X  ITMAX,FUNC,IPRINT,ICONV,NITER,ICOUNT)

C  ELDERY DIFFERS FROM ELDERX ONLY IN THE DIMENSION STATEMENT. ALL 5'S
C  ARE CHANGED TO 25'S, AND ALL 6'S ARE CHANGED TO 26'S. THIS ALLOWS 25
C  PARAMETERS INSTEAD OF JUST 5. As of assbig4.f, 30 parameters are 
C  allowed instead of 25.

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
        CALL FUNC(N,START,FN)
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
        CALL FUNC(N,START,FN)
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
        CALL FUNC(N,PSTAR,FN)
        YSTAR=FN
        ICOUNT=ICOUNT+1
        IF(YSTAR.GE.YLO) GO TO 12
        IF(ICOUNT.GE.KCOUNT) GO TO 19
C
C  SUCESSFUL REFLECTION SO EXTENSION.
C
        DO 9 I=1,N
9       P2STAR(I)=ECOEFF*PSTAR(I)+(1.0D0-ECOEFF)*PBAR(I)
        CALL FUNC(N,P2STAR,FN)
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
        CALL FUNC(N,P2STAR,FN)
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
        CALL FUNC(N,XMIN,FN)
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
        CALL FUNC(N,XMIN,FN)
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

 	SUBROUTINE REGANAL(NVAR,NDIM,MAXSUB,MAXDIM,PAREST,IESTIJ,C0P,
     1  C1P,C2P,C3P,PAR,NSUB,IGAMMA,ESTMENN,ESTCOV,AVGLOG,NUMEQT)


        use npag_utils, only : makevec

	IMPLICIT REAL*8(A-H,O-Z) 
      PARAMETER(MAXNUMEQ=7)

	DIMENSION PAREST(MAXSUB,MAXDIM),IESTIJ(MAXSUB,MAXDIM)
	DIMENSION ESTMEN(30),ESTINV(30,30),PMAT(594,30),
     1 START(30),STEP(30),ESTCOV(30,30),THETA(30),B(30,1),
     2 VALFIX(20),ESTMENN(30),PIK(30,30),COVINV(30,30),SUM(30),
     3 SUMCOV(30,30),DIFF(30),DIFPRD(30,30),SUMCOL(30),IRAN(32),PX(32),
     4 C0P(NUMEQT),C1P(NUMEQT),C2P(NUMEQT),C3P(NUMEQT),
     5 GAMMA(MAXNUMEQ),C0(MAXNUMEQ),C1(MAXNUMEQ),C2(MAXNUMEQ),
     6 C3(MAXNUMEQ),YO(594,MAXNUMEQ),SIG(594,MAXNUMEQ),
     7 IGAMMA(MAXNUMEQ),RINV(594,594),XVERIFY(100),RANFIXEST(20),
     8 OPTVAR(32),ESTMENO(32),IIRAN(32),X(32)


C  NOTE THAT ALL ARRAYS PASSED IN THE ARGUMENT LIST, WHICH HAVE 
C  DIMENSIONS RELATED TO THE MAX. NO. OF OUTPUT EQS. HAVE THOSE VALUES
C  CHANGED TO NUMEQT AS OF asseng13.f (EXCEPT FOR IGAMMA, WHICH IS
C  PASSED IN A COMMON STMT. IN MAIN, AND IS THEREFORE DIMENSIONED WITH
C  MAXNUMEQ). THE OTHER ARRAYS WHICH HAD 6 AS THEIR DIMENSION NOW HAVE
C  MAXNUMEQ AS THEIR DIMENSION; THIS INCLUDES SIG, WHICH IS PASSED
C  IN A COMMON BELOW.

      CHARACTER PAR(30)*11,ERRFIL*20

      COMMON SIG  

	COMMON/TOMAP/IRAN,VALFIX,SIGFAC,OFAC,ESTMEN,ESTINV,
     1  DET,NOFIX,NUMEQTT,RANFIXEST,NRANFIX

C  NOTE THAT NUMEQT IS LABELED NUMEQTT IN COMMON/TOMAP SINCE IT IS
C  PROVIDED IN THE ARGUMENT LIST SO THAT ARRAYS CAN BE VARIABLY
C  DIMENSIONED.

      COMMON/ERR/ERRFIL 

C  THE BLANK COMMON ABOVE IS SUPPLIED TO SUBROUTINE IDPC. 
C  COMMON/TOMAP/ IS SUPPLIED TO SUBROUTINE MAPBAYS.

      COMMON/TOCALC/IIRAN,PX,NNOFIX,NNSUB
C  COMMON/TOCALC IS PROVIDED TO SUBROUTINE CALCRF, WHICH IS CALLED
C  BY SUBROUTINE ELDERY3.

C  NOTE THAT IIRAN(.) IS SET = IRAN(.); NNOFIX IS SET = NOFIX; AND
C  NNSUB IS SET = NSUB BELOW.
C  IRAN(.) AND NOFIX ARE USED IN COMMON/MAPBAYS ABOVE. NSUB IS A
C  CALLING ARGUMENT.

      integer JSUB,IG
      integer, dimension(128) :: INTLIST
      integer, dimension(257) :: IPAR
      double precision, dimension(257) :: RPAR

      EXTERNAL MAPBAYS
      EXTERNAL CALCRF


C  THIS ROUTINE IS CALLED BY MAIN TO RUN UP TO 4 CYCLES OF THE PROGRAM
C  (UNLESS CONVERGENCE IS REACHED FIRST) TO OBTAIN -- FOR INPUT VALUES
C  OF [C0P,C1P,C2P,C3P], ESTMEN, AND ESTCOV -- THE FINAL CYCLE ESTMEN 
C  AND ESTCOV, AND THE FINAL AVERAGE LOG-LIKELIHOOD, AVGLOG.

C  MOST OF THE COMMENTS IN THIS ROUTINE ARE SUPPRESSED; THEY ARE THE
C  SAME AS IN MAIN.

C  SET NNSUB = NSUB. ONE IS A CALLING ARGUMENT, AND ONE IS IN
C  COMMON/TOCALC.

       NNSUB = NSUB


C  SET NNOFIX = NOFIX. ONE IS USED IN COMMON/TOMAP, AND ONE IN
C  COMMON/TOCALC.

       NNOFIX = NOFIX

C  SET IIRAN(.) = IRAN(.). ONE IS USED IN COMMON/TOMAP, AND ONE IN
C  COMMON/TOCALC.

        DO I = 1,NVAR+NOFIX+NRANFIX
         IIRAN(I) = IRAN(I)
        END DO



	DO I=1,NVAR
	 ESTMEN(I) = ESTMENN(I)
	END DO

	DO IEQ=1,NUMEQT
	 GAMMA(IEQ) = 1.D0
	END DO

	REWIND(27)

	DO I=1,NSUB
	 DO J=1,NVAR
	  IESTIJ(I,J)=1
	 END DO
	END DO


	DO 700 JSUB=1,NSUB


C  CALL SUBROUTINE FILRED TO READ, FOR THIS SUBJECT, FROM SCRATCH FILE
C  27, THE NO. OF OBSERVATION TIMES (NOBSER) AS WELL AS THE 
C  OBSERVED VALUES THEMSELVES: YO(I,IEQ) = THE 'NOISY' OBSERVED VALUES 
C  FOR THIS SUBJECT; I=1,NOBSER, IEQ=1,NUMEQT. THESE OBSERVED VALUES ARE 
C  USED ONLY TO CALCULATE THE ASSAY STANDARD DEVIATIONS -- USING THE 
C  VECTORS, C0,C1,C2,C3 (WHICH GIVE THE VALUES CURRENTLY IN THE PATIENT
C  DATA FILE) FOR THOSE OUTPUT EQS. WHICH ARE NOT HAVING THEIR C'S 
C  ESTIMATED, AND USING VECTORS C0P,C1P,C2P,C3P (POPULATION VECTORS) FOR 
C  THOSE OUTPUT EQS. WHICH ARE HAVING THEIR C'S ESTIMATED.

C  NOTE THAT IF YO(I,IEQ) = -99 BELOW, OUTPUT EQ. IEQ HAS NO OBSERVED 
C  LEVEL FOR OBSERVATION TIME I), AND THIS ENTRY WILL BE IGNORED IN THE
C  ANALYSIS.

C  ALSO, CALCULATE SIGFAC, THE PRODUCT OF THE NON-MISSING STD. DEV.'S
C  INITIALIZE SIGFAC=1, AND THEN UPDATE IT FOR EACH NON-MISSING
C  OBSERVATION.

C  THE REST OF THE INFO IN THE SUBJECT DATA FILE IS PASSED IN COMMONS TO 
C  THE IDPC MODULE SUBROUTINES.

	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT)

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

C  INITIALIZE MISTOT = 0. AFTER THE 140 LOOP, IT WILL BE THE TOTAL NO.
C  OF MISSING VALUES AMONG ALL NUMEQT OUTPUT EQS.

	MISTOT = 0

	SIGFAC=1.D0


	DO 140 IEQ=1,NUMEQT
	 DO 140 I=1,NOBSER
	  Y = YO(I,IEQ)

C  IF Y = -99, IT MEANS THAT OUTPUT EQ. J HAD NO VALUE AT OBSERVATION
C  TIME I. IN THIS CASE, IGNORE THIS Y AND INCREASE MISVAL(J) BY 1.

	  IF(Y .EQ. -99) THEN
	   MISTOT = MISTOT + 1
	   GO TO 140
	  ENDIF

	  IF(IGAMMA(IEQ) .EQ. 2) 
     1 SIG(I,IEQ) = GAMMA(IEQ)*(C0P(IEQ) + C1P(IEQ)*Y + C2P(IEQ)*Y*Y + 
     2              C3P(IEQ)*Y**3)

	  IF(IGAMMA(IEQ) .NE. 2) 
     1 SIG(I,IEQ) = GAMMA(IEQ)*(C0(IEQ) + C1(IEQ)*Y + C2(IEQ)*Y*Y + 
     2              C3(IEQ)*Y**3)

      SIGFAC=SIGFAC*SIG(I,IEQ)


      NDEX = NDEX+1
      RINV(NDEX,NDEX) = 1.D0/SIG(I,IEQ)/SIG(I,IEQ)

  140 CONTINUE


C  NOTE THAT SIGFAC WAS CALCULATED IN LOOP 140 ABOVE, AND THAT OFAC IS 
C  NOW THE RESULT OF NOBACT = NOBSER*NUMEQT - MISTOT VALUES. 

	NOBACT = NOBSER*NUMEQT - MISTOT
        OFAC=2.506628274631**NOBACT

C  NOTE THAT 2.5066... = SQRT(2*PI).


	DO J=1,NVAR
	 THETA(J) = ESTMEN(J) 
	END DO

C  BEFORE ALL CALLS TO IDCALCP, MUST INTEGRATE FIXED, RANDOM, AND
C  RANFIX VALUES INTO PX, USING IRAN(I),I=1,NVAR+NOFIX+NRANFIX. CALL 
C  MAKEVEC TO DO THIS.

	CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,THETA,VALFIX,RANFIXEST,PX)
	CALL IDCALCP(NVAR,NOFIX,NRANFIX,IRAN,NDIM,PX,PMAT)
	CALL CALCOV(NVAR,NOBACT,PMAT,RINV,COVINV)

	DO I=1,NVAR
	 IF(COVINV(I,I) .LE. 0) THEN
	  IESTIJ(JSUB,I)=0
	 ENDIF
	END DO


  700   CONTINUE



C  START THE ANALYSIS. NOTE THAT THIS ANALYSIS WILL GO NO MORE THAN 4
C  CYCLES (AN ARBITRARY LIMIT FOR NOW).

	OLDAVG=-1.D30


	DO 2000 ITER = 1,4


	REWIND(27)

	WRITE(25,1217) ITER 
	WRITE(*,1217) ITER 
 1217   FORMAT(//' ANALYSIS ITERATION NUMBER ',I4,'.'/)

	DO I=1,NVAR
	 DO J=1,NVAR
	  ESTINV(I,J) = ESTCOV(I,J)
	 END DO
	END DO

 	CALL MATNV2(ESTINV,NVAR,B,1,DET)

	IF(DET .LE. 0) THEN

C  NOTE THAT IF DET .LE. 0 --> THE PROGRAM WILL 
C  BOMB WHEN MAPBAYS IS CALLED BY ELDER BELOW (SINCE MAPBAYS HAS A 
C  LOG(DET) IN IT). TO PREVENT THIS, PRINT MESSAGE TO USER AND STOP.

	 WRITE(*,1216)
	 WRITE(25,1216)
 1216    FORMAT(/' THE CURRENT POPULATION COV. ESTIMATE IS SINGULAR.'/
     1' THIS INDICATES AN ILL-CONDTIONED PROBLEM. PLEASE RE-EXAMINE'/
     2' YOUR PATIENT DATA FILES, AND YOUR INPUT DATA INFO., CORRECT'/
     3' ANY INCONSISTENCIES, AND THEN RERUN THE PROGRAM.'//)

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

	WRITE(*,1215)
 1215   FORMAT(/' SUBJECT NUMBERS FOLLOW AS PARAMETER ESTIMATES ARE'/
     1' BEING FOUND: '/) 

 8888   FORMAT(' ',I3)

      DO 1000 JSUB=1,NSUB

	WRITE(*,8888) JSUB
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT)

C  NOTE THAT C0,C1,C2,C3 ARE THE C'S CURRENTLY IN THIS PATIENT'S DATA
C  FILE. THEY ARE NEEDED FOR THOSE OUTPUT EQS., IEQ, WHERE IGAMMA(IEQ)
C  .NE. 2 (FOR IEQ, WHERE IGAMMA(IEQ) = 2, CURRENT POPULATION VALUES,
C  C0P,...,C3P, WILL BE USED).

	MISTOT = 0

	SIGFAC=1.D0

	DO I=1,NOBSER
      	 DO IEQ=1,NUMEQT
          Y=YO(I,IEQ)

	  IF(Y .EQ. -99) THEN
	   MISTOT = MISTOT + 1
	  ENDIF

	  IF(Y .NE. -99) THEN
	   IF(IGAMMA(IEQ) .EQ. 2) 
     1 SIG(I,IEQ) = GAMMA(IEQ)*(C0P(IEQ) + C1P(IEQ)*Y + C2P(IEQ)*Y*Y + 
     2              C3P(IEQ)*Y**3)
	   IF(IGAMMA(IEQ) .NE. 2) 
     1 SIG(I,IEQ) = GAMMA(IEQ)*(C0(IEQ) + C1(IEQ)*Y + C2(IEQ)*Y*Y + 
     2              C3(IEQ)*Y**3)
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

	DO J=1,NVAR
	 IF(IESTIJ(JSUB,J) .EQ. 1) PAREST(JSUB,J) = THETA(J)
	 IF(IESTIJ(JSUB,J) .EQ. 0) PAREST(JSUB,J) = 0.D0 
	END DO

	SUMLOG = SUMLOG + (-VALMIN)


 1000   CONTINUE


	AVGLOG = SUMLOG/NSUB 

C  AVGLOG IS NOW THE AVERAGE LOG-LIKELIHOOD OF  
C  P(YJ,THETA|ESTMEN, ESTCOV), WHERE ESTMEN AND ESTCOV ARE THE
C  ESTIMATES (PRIOR TO THIS ITERATION) OF THE POPULATION MEAN VECTOR 
C  AND COVARIANCE MATRIX, RESPECTIVELY, OVER ALL SUBJECTS, AND THETA
C  IS A GIVEN SUBJECT'S MAP-BAYESIAN ESTIMATE.

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

	 CLOSE(25)
	 CLOSE(27)
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

C  CALCULATE UPDATED ESTCOV = AVERAGE, OVER ALL SUBJECTS, OF
C  PIK(.,.) + DIFF(.)*DIFF(.)'], WHERE PIK IS DESCRIBED IN MODEL AT THE
C  TOP OF THIS CODE, DIFF(.) = PAREST(ISUB,.) - ESTMEN(.), AND 
C  ESTMEN(.) IS THE JUST UPDATED ESTMEN ABOVE.

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


	WRITE(*,8888) JSUB

	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT)

C  SEE SOME DETAILED COMMENTS ABOVE.

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
	   IF(IGAMMA(IEQ) .EQ. 2) 
     1 SIG(I,IEQ) = GAMMA(IEQ)*(C0P(IEQ) + C1P(IEQ)*Y + C2P(IEQ)*Y*Y + 
     2              C3P(IEQ)*Y**3)
	   IF(IGAMMA(IEQ) .NE. 2) 
     1 SIG(I,IEQ) = GAMMA(IEQ)*(C0(IEQ) + C1(IEQ)*Y + C2(IEQ)*Y*Y + 
     2              C3(IEQ)*Y**3)
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

C  BEFORE ALL CALLS TO IDCALCP, MUST INTEGRATE FIXED, RANDOM, AND
C  RANFIX VALUES INTO PX, USING IRAN(I),I=1,NVAR+NOFIX+NRANFIX. CALL 
C  MAKEVEC TO DO THIS.

	CALL MAKEVEC(NVAR,NOFIX,NRANFIX,IRAN,THETA,VALFIX,RANFIXEST,PX)
	CALL IDCALCP(NVAR,NOFIX,NRANFIX,IRAN,NDIM,PX,PMAT)
	CALL CALCOV(NVAR,NOBACT,PMAT,RINV,COVINV)

	DO I=1,NVAR

	IF(COVINV(I,I) .LE. 0) THEN

	  IF(IESTIJ(JSUB,I) .EQ. 1) THEN
	    IESTIJ(JSUB,I) = 0
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


 1500   CONTINUE

	DO I=1,NVAR
	 DO J=1,NVAR
	  ESTCOV(I,J) = SUMCOV(I,J)/NSUB
	 END DO
	END DO

C  REPLACE WRITING OF AVGLOG WITH XVERIFY (SEE LOGIC IN 
C  SUBROUTINE VERIFYVAL.

      XVERIFY(1) = AVGLOG
      CALL VERIFYVAL(1,XVERIFY)
C     WRITE(25,4324) AVGLOG
      WRITE(25,4324) XVERIFY(1)
C     WRITE(*,4324) AVGLOG
      WRITE(*,4324) XVERIFY(1)
 4324   FORMAT(/' THE AVERAGE LOG-LIKELIHOOD IS ',G15.6)

C  IF 4 ITERATIONS HAVE BEEN PERFORMED (DO LOOP 2000), OR IF THE 
C  AVERAGE LOG-LIK DID NOT INCREASE BY AT LEAST .001, STOP THE ANALYSIS; 
C  OTHERWISE, CONTINUE.

	IF(DABS(AVGLOG - OLDAVG) .LT. .001) GO TO 2500

	OLDAVG=AVGLOG


      IF(NRANFIX .GT. 0 .AND. ITER .EQ. 1) THEN


C  IF NRANFIX .GT. 0, CALL ELDERY3 TO GET UPDATED ESTIMATES FOR THESE
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
C  SUBROUTINE CALCRF EACH TIME IT IS CALLED BY ELDERY3 WITH ANOTHER SET 
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

       CALL ELDERY3(NRANFIX+NVAR,START,OPTVAR,VALMIN,1.D-10,STEP,1000,
     1  CALCRF,0,ICONV,NITER,ICNT,NUMEQT,YO,C0,C1,C2,C3,GAMMA)

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


       WRITE(*,9999) JSUB
 9999  FORMAT(' ',I3)

       CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT)
	
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
       CALL IDCALCP(NVAR,NOFIX,NRANFIX,IRAN,NDIM,PX,PMAT)
       CALL CALCOV(NVAR,NOBACT,PMAT,RINV,COVINV)



       DO I=1,NVAR
        IF(COVINV(I,I) .LE. 0) THEN
         IF(IESTIJ(JSUB,I) .EQ. 1) THEN
          IESTIJ(JSUB,I) = 0
          WRITE(*,5987) JSUB, PAR(I)
          WRITE(25,5987) JSUB, PAR(I)
5987    FORMAT(//' SUBJECT ',I3,' NO LONGER CAN ESTIMATE '/
     1' PARAMETER ',A11,'. STARTING WITH THE NEXT CYCLE, THIS '/
     2' PARAMETER ESTIMATE FOR THIS SUBJECT WILL BE SET EQUAL TO THE '/
     3' POPULATION MEAN FROM THE SUBJECTS WHICH HAVE INFORMATION ON '/
     4' IT.'/)
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
C  NO! THIS IS ONLY DONE IN it2branfix1.f CODE, NOT IN THIS PROGRAM,
C  WHERE WE ARE ONLY INTERESTED IN THE ESTIMATION OF THE C'S.



      ENDIF
C  ABOVE ENDIF IS FOR THE  IF(NRANFIX .GT. 0 .AND. NRANFIX .EQ. 1)
C    CONDITION.


 2000   CONTINUE

 2500   DO I=1,NVAR
	 ESTMENN(I) = ESTMEN(I)
	END DO


C  NOTE THAT ESTMENN RETURNS AS THE UPDATED MEAN ESTIMATE. PREVIOUS TO
C  assay_3a.f, ESTMENN'S VALUES WERE NOT CHANGED EXPLICITLY IN 
C  SUBROUTINE REGANAL; THEY WERE CHANGED VIA COMMON/TOMAP. BUT, AS OF
C  assay_3a.f, THE CODE IN REGANAL EXPLICITLY UPDATES THE VALUES IN
C  ESTMEN (HAVING THE VALUES UPDATED VIA COMMON/TOMAP WAS UNINTENDED,
C  AND SLOPPY, THOUGH CORRECT).

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
      SUBROUTINE NEWWORK1
      PARAMETER(MAXNUMEQ=7)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SIG(5000),RS(5000,34),DELTAIV(7),ORDELT(7),
     1 RSS(5000,34),SIGG(5000),TIM(594),TIMM(594),YO(594,MAXNUMEQ),
     2 TIMDELAY(99),XVERIFY(100)

      CHARACTER READLINE*300,ERRFIL*20

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
C  AS OF asseng10.f, STEADY STATE DOSES MAY BE BOLUS DOSES. IN THIS
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

       DO I = 1,ND
        READ(23,*) SIG(I),(RS(I,J),J=1,NI)
        IF(SIG(I) .LT. 0.D0) ICOPY = 0
       END DO

      ENDIF

  
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
       READ(23,3) NUMEQT
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
C  100 DOSES WILL BE RS(ID,2*IDRUG). IF RS(ID,2*IDRUG) > 0, DRUG, IDRUG,
C  PARTICIPATES IN THE STEADY STATE DOSING. IF THIS VALUE = 0, DRUG,
C  IDRUG, DOES NOT PARTICIPATE. NOTE THAT IF A DRUG PARTICIPATES, THE
C  ROUTE WILL BE AS AN IV, WITH RATE RS(ID,2*IDRUG-1), IF 
C  RS(ID,2*IDRUG-1) > 0. BUT IF THIS VALUE IS 0, THE DRUG WILL BE GIVEN
C  AS A BOLUS. NOTE THAT THE INTERVAL BETWEEN DOSES IS -SIG(ID).

C  IF DRUG, IDRUG, PARTICIPATES IN THE 100 STEADY STATE DOSE SET, PUT 
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

C  1. THE NEXT 100*(NDELTA + 1) ROWS WILL BE FOR THE STEADY STATE
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

   20  READ(23,1717) READLINE

       IF(READLINE(12:23) .NE. 'NO. OF TOTAL') GO TO 20
       BACKSPACE(23)
       READ(23,3) NUMEQT
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

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION DELTAIV(7),ORDELT(7),X(7)

C  SUBROUTINE ORDERDELTA IS CALLED BY NEWWORK TO OBTAIN NDELTA, THE NO.
C  OF UNIQUE NON-0 VALUES IN THE DELTAIV(.) ARRAY. THEN THE ORDERED SET
C  OF THESE NDELTA VALUES IS PUT INTO ORDELT(.).

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
	SUBROUTINE THESAME(X1,X2,ISAME)
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
     1 GAMMA)
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

C  COMMON/TOCALC VALUES ARE PASSED TO THIS ROUTINE FROM MAIN.

      integer JSUB,IG
      integer, dimension(128) :: INTLIST
      integer, dimension(257) :: IPAR
      double precision, dimension(257) :: RPAR

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

       CALL FILRED(NOBSER,YO,C0,C1,C2,C3,NUMEQT)

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
C  THE ABOVE END DO IS FOR THE  DO JSUB = 1,NSUB  LOOP.

      FNTVAL = SUMTOT


	RETURN
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE ELDERY3(N,START,XMIN,YNEWLO,REQMIN,STEP,
     X  ITMAX,FUNC,IPRINT,ICONV,NITER,ICOUNT,NUMEQT,YO,C0,C1,C2,C3,
     1  GAMMA)

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
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,GAMMA)
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
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,GAMMA)
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
        CALL FUNC(N,PSTAR,FN,NUMEQT,YO,C0,C1,C2,C3,GAMMA)
        YSTAR=FN
        ICOUNT=ICOUNT+1
        IF(YSTAR.GE.YLO) GO TO 12
        IF(ICOUNT.GE.KCOUNT) GO TO 19
C
C  SUCESSFUL REFLECTION SO EXTENSION.
C
        DO 9 I=1,N
9       P2STAR(I)=ECOEFF*PSTAR(I)+(1.0D0-ECOEFF)*PBAR(I)
        CALL FUNC(N,P2STAR,FN,NUMEQT,YO,C0,C1,C2,C3,GAMMA)
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
        CALL FUNC(N,P2STAR,FN,NUMEQT,YO,C0,C1,C2,C3,GAMMA)
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
        CALL FUNC(N,XMIN,FN,NUMEQT,YO,C0,C1,C2,C3,GAMMA)
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
        CALL FUNC(N,XMIN,FN,NUMEQT,YO,C0,C1,C2,C3,GAMMA)
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

	SUBROUTINE IDPC(X,SUMSQJ)

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

C*****INITIALIZE PROGRAM*****

	CALL SYMBOL

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
   
        CALL SUMSQ(SUMSQJ)


        RETURN
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE FUNC(M,SUMSQJ)

C  FUNCTION TO DETERMINE THE ENTRIES IN F, GIVEN P.

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

C*****ODE CONSTANTS AND INITIALIZATION*****

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


	 CALL GETFA(FA)


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

	 CALL GETIX(N,X)



C  CALL SUBROUTINE GETTLAG IN it2bdriv.f (THE FIRST TEMPLATE FILE TO 
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

C  AS OF idm11x13.f: MUST CALL GETFA BEFORE EVERY TIME THAT
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
32      IF(N .NE. -1) CALL USERANAL(X,T,TOUT)
        IF(N .EQ. -1) CALL ANAL3(X,T,TOUT)


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

        CALL OUTPUT(TIM(KNTM1),YT)

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


      RETURN
      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE SUMSQ(SUMSQJ)

C  SUBROUTINE TO EVALUATE THE SUM OF SQUARES OF THE RESIDUAL VECTOR.

        IMPLICIT REAL*8(A-H,O-Z)
        PARAMETER(MAXNUMEQ=7)

        COMMON/SUM2/ M,NPNL
        COMMON/CNST2/ NPL,NOS,NBI,NRI
        DIMENSION SUMSQJ(MAXNUMEQ)

        CALL FUNC(M,SUMSQJ)

        RETURN
        END
CŠCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE USERANAL(X,TIN,TOUT)

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

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XSTORE(100,20),XPRED(20),COMP(5,20)

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



        CALL THESAME(F,1.0,ISAMEF1)
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


        CALL THESAME(F,1.0,ISAMEF2)
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


        CALL THESAME(F,1.0,ISAMEF3)
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

	SUBROUTINE IDCALCP(NVAR,NOFIX,NRANFIX,IRAN,NDIM,ESTML,PMAT)

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


C*****INITIALIZE PROGRAM*****

	CALL SYMBOL

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

	CALL EVAL(Y)

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

        RETURN
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE FUNC1(M,F)

C  THIS SUBROUTINE, CALLED BY EVAL2, FINDS F(I) = OUTPUT CONC. AT
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

      DIMENSION X(20),P(32),TIM(594),SIG(5000),SIGO(5000),R(37),
     1 RS(5000,34),RSO(5000,34),YT(MAXNUMEQ),YO(594,MAXNUMEQ),F(3564),
     2 BS(5000,7),Y(594,MAXNUMEQ),B(20),NBCOMP(7),TLAG(7),FA(7),
     3 XSTORE(100,20),XPRED(20),XVERIFY(100)

      CHARACTER ERRFIL*20


C  NOTE THAT AS OF idm222x13.f, THE DIMENSIONS OF 6 IN XSTORE AND XPRED
C  HAVE BEEN CHANGED TO 20, WHICH IS WHAT THEY SHOULD HAVE BEEN ALL
C  ALONG (I.E., THE SAME AS FOR X).

C  NOTE THAT THE DIMENSIONS RELATED TO THE NO. OF OUTPUT EQS. IN
C  YO, YT AND Y ARE CHANGED TO MAXNUMEQ (FROM 6). 


C  NOTE THAT "7" IN THE ABOVE ARRAYS INDICATE THE NO. OF DRUGS ALLOWED.

C*****ODE CONSTANTS AND INITIALIZATION*****

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

	 CALL GETFA(FA)


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

        IF(N .EQ. 0) GO TO 75


C  CALL SUBROUTINE GETIX IN it2bdriv.f (THE FIRST TEMPLATE FILE TO 
C  INCLUDE GETIX IS TSTMULTG.FOR) TO OBTAIN THE VALUE OF X (THE INITIAL
C  COMPARTMENT AMOUNT) FOR EACH OF THE N COMPARTMENTS.

	 CALL GETIX(N,X)



C  CALL SUBROUTINE GETTLAG IN it2bdriv.f (THE FIRST TEMPLATE FILE TO 
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
2000  Y(KNT,I)=YT(I)
      KNT=KNT+1
      GO TO 45

12    IF(TIM(KNT).GT.SIG(KNS)) GO TO 13
	IF(TIM(KNT).NE.0.0D0) GO TO 45

C  THE ONLY WAY THE FOLLOWING CALL TO OUTPUT CAN OCCUR IS IF TIM(KNT)
C  = 0 --> OBTAIN YT = OUTPUT VALUE(S) AT TIME 0.0.

      CALL OUTPUT(0.D0,YT)
	DO 2005 I=1,NOS
2005  Y(KNT,I)=YT(I)
      KNT=KNT+1

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

      ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(SIG(KNS) .LT. 0.D0)  CONDITION.


      DO I=1,NI
       R(I)=RS(KNS,I)
      END DO

	IF(NDRUG .EQ. 0) GO TO 81

C  AS OF idm222x12.f: MUST CALL GETFA BEFORE EVERY TIME THAT
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

81    KNS=KNS+1

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
32      IF(N .NE. -1) CALL USERANAL(X,T,TOUT)
        IF(N .EQ. -1) CALL ANAL3(X,T,TOUT)


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

      CALL OUTPUT(TIM(KNTM1),YT)

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

	
      RETURN
      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE EVAL(F)

C  THIS SUBROUTINE, CALLED BY MAIN, FINDS THE OUTPUT CONC. 
C  VECTOR, Y, EVALUATED AT PARAMETER VALUES IN VECTOR P, PASSED 
C  DIRECTLY TO SUBROUTINE FUNC2 VIA COMMON/PARAMD.

	IMPLICIT REAL*8(A-H,O-Z)
	COMMON/SUM2/ M,NPNL
        COMMON/CNST2/ NPL,NOS,NDRUG,NADD
	DIMENSION F(3564)

	CALL FUNC1(M,F)


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

	SUBROUTINE SHIFT(TAU,ND,SIG,NDRUG,NADD,RS)
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


C  VODTOT.FOR							5-2-96

C  VODTOT.FOR CONTAINS MODULES VODE.FOR AND VODEXT.FOR.

C----------------------------------------------------------------------

*DECK DVODE
      SUBROUTINE DVODE (F, NEQ, Y, T, TOUT, ITOL, RTOL, ATOL, ITASK,
     1            ISTATE, IOPT, RWORK, LRW, IWORK, LIW, JAC, MF,
     2            RPAR, IPAR)
      EXTERNAL F, JAC
      DOUBLE PRECISION Y, T, TOUT, RTOL, ATOL, RWORK, RPAR
      INTEGER NEQ, ITOL, ITASK, ISTATE, IOPT, LRW, IWORK, LIW,
     1        MF, IPAR
      DIMENSION Y(*), RTOL(*), ATOL(*), RWORK(LRW), IWORK(LIW),
     1          RPAR(*), IPAR(*)
C-----------------------------------------------------------------------
C DVODE.. Variable-coefficient Ordinary Differential Equation solver,
C with fixed-leading coefficient implementation.
C This version is in double precision.
C
C DVODE solves the initial value problem for stiff or nonstiff
C systems of first order ODEs,
C     dy/dt = f(t,y) ,  or, in component form,
C     dy(i)/dt = f(i) = f(i,t,y(1),y(2),...,y(NEQ)) (i = 1,...,NEQ).
C DVODE is a package based on the EPISODE and EPISODEB packages, and
C on the ODEPACK user interface standard, with minor modifications.
C-----------------------------------------------------------------------
C Revision History (YYMMDD)
C   890615  Date Written
C   890922  Added interrupt/restart ability, minor changes throughout.
C   910228  Minor revisions in line format,  prologue, etc.
C   920227  Modifications by D. Pang:
C           (1) Applied subgennam to get generic intrinsic names.
C           (2) Changed intrinsic names to generic in comments.
C           (3) Added *DECK lines before each routine.
C   920721  Names of routines and labeled Common blocks changed, so as
C           to be unique in combined single/double precision code (ACH).
C   920722  Minor revisions to prologue (ACH).
C   920831  Conversion to double precision done (ACH).
C-----------------------------------------------------------------------
C References..
C
C 1. P. N. Brown, G. D. Byrne, and A. C. Hindmarsh, "VODE: A Variable
C    Coefficient ODE Solver," SIAM J. Sci. Stat. Comput., 10 (1989),
C    pp. 1038-1051.  Also, LLNL Report UCRL-98412, June 1988.
C 2. G. D. Byrne and A. C. Hindmarsh, "A Polyalgorithm for the
C    Numerical Solution of Ordinary Differential Equations,"
C    ACM Trans. Math. Software, 1 (1975), pp. 71-96.
C 3. A. C. Hindmarsh and G. D. Byrne, "EPISODE: An Effective Package
C    for the Integration of Systems of Ordinary Differential
C    Equations," LLNL Report UCID-30112, Rev. 1, April 1977.
C 4. G. D. Byrne and A. C. Hindmarsh, "EPISODEB: An Experimental
C    Package for the Integration of Systems of Ordinary Differential
C    Equations with Banded Jacobians," LLNL Report UCID-30132, April
C    1976.
C 5. A. C. Hindmarsh, "ODEPACK, a Systematized Collection of ODE
C    Solvers," in Scientific Computing, R. S. Stepleman et al., eds.,
C    North-Holland, Amsterdam, 1983, pp. 55-64.
C 6. K. R. Jackson and R. Sacks-Davis, "An Alternative Implementation
C    of Variable Step-Size Multistep Formulas for Stiff ODEs," ACM
C    Trans. Math. Software, 6 (1980), pp. 295-318.
C-----------------------------------------------------------------------
C Authors..
C
C               Peter N. Brown and Alan C. Hindmarsh
C               Computing and Mathematics Research Division, L-316
C               Lawrence Livermore National Laboratory
C               Livermore, CA 94550
C and
C               George D. Byrne
C               Exxon Research and Engineering Co.
C               Clinton Township
C               Route 22 East
C               Annandale, NJ 08801
C-----------------------------------------------------------------------
C Summary of usage.
C
C Communication between the user and the DVODE package, for normal
C situations, is summarized here.  This summary describes only a subset
C of the full set of options available.  See the full description for
C details, including optional communication, nonstandard options,
C and instructions for special situations.  See also the example
C problem (with program and output) following this summary.
C
C A. First provide a subroutine of the form..
C
C           SUBROUTINE F (NEQ, T, Y, YDOT, RPAR, IPAR)
C           DOUBLE PRECISION T, Y, YDOT, RPAR
C           DIMENSION Y(NEQ), YDOT(NEQ)
C
C which supplies the vector function f by loading YDOT(i) with f(i).
C
C B. Next determine (or guess) whether or not the problem is stiff.
C Stiffness occurs when the Jacobian matrix df/dy has an eigenvalue
C whose real part is negative and large in magnitude, compared to the
C reciprocal of the t span of interest.  If the problem is nonstiff,
C use a method flag MF = 10.  If it is stiff, there are four standard
C choices for MF (21, 22, 24, 25), and DVODE requires the Jacobian
C matrix in some form.  In these cases (MF .gt. 0), DVODE will use a
C saved copy of the Jacobian matrix.  If this is undesirable because of
C storage limitations, set MF to the corresponding negative value
C (-21, -22, -24, -25).  (See full description of MF below.)
C The Jacobian matrix is regarded either as full (MF = 21 or 22),
C or banded (MF = 24 or 25).  In the banded case, DVODE requires two
C half-bandwidth parameters ML and MU.  These are, respectively, the
C widths of the lower and upper parts of the band, excluding the main
C diagonal.  Thus the band consists of the locations (i,j) with
C i-ML .le. j .le. i+MU, and the full bandwidth is ML+MU+1.
C
C C. If the problem is stiff, you are encouraged to supply the Jacobian
C directly (MF = 21 or 24), but if this is not feasible, DVODE will
C compute it internally by difference quotients (MF = 22 or 25).
C If you are supplying the Jacobian, provide a subroutine of the form..
C
C           SUBROUTINE JAC (NEQ, T, Y, ML, MU, PD, NROWPD, RPAR, IPAR)
C           DOUBLE PRECISION T, Y, PD, RPAR
C           DIMENSION Y(NEQ), PD(NROWPD,NEQ)
C
C which supplies df/dy by loading PD as follows..
C     For a full Jacobian (MF = 21), load PD(i,j) with df(i)/dy(j),
C the partial derivative of f(i) with respect to y(j).  (Ignore the
C ML and MU arguments in this case.)
C     For a banded Jacobian (MF = 24), load PD(i-j+MU+1,j) with
C df(i)/dy(j), i.e. load the diagonal lines of df/dy into the rows of
C PD from the top down.
C     In either case, only nonzero elements need be loaded.
C
C D. Write a main program which calls subroutine DVODE once for
C each point at which answers are desired.  This should also provide
C for possible use of logical unit 6 for output of error messages
C by DVODE.  On the first call to DVODE, supply arguments as follows..
C F      = Name of subroutine for right-hand side vector f.
C          This name must be declared external in calling program.
C NEQ    = Number of first order ODE-s.
C Y      = Array of initial values, of length NEQ.
C T      = The initial value of the independent variable.
C TOUT   = First point where output is desired (.ne. T).
C ITOL   = 1 or 2 according as ATOL (below) is a scalar or array.
C RTOL   = Relative tolerance parameter (scalar).
C ATOL   = Absolute tolerance parameter (scalar or array).
C          The estimated local error in Y(i) will be controlled so as
C          to be roughly less (in magnitude) than
C             EWT(i) = RTOL*abs(Y(i)) + ATOL     if ITOL = 1, or
C             EWT(i) = RTOL*abs(Y(i)) + ATOL(i)  if ITOL = 2.
C          Thus the local error test passes if, in each component,
C          either the absolute error is less than ATOL (or ATOL(i)),
C          or the relative error is less than RTOL.
C          Use RTOL = 0.0 for pure absolute error control, and
C          use ATOL = 0.0 (or ATOL(i) = 0.0) for pure relative error
C          control.  Caution.. Actual (global) errors may exceed these
C          local tolerances, so choose them conservatively.
C ITASK  = 1 for normal computation of output values of Y at t = TOUT.
C ISTATE = Integer flag (input and output).  Set ISTATE = 1.
C IOPT   = 0 to indicate no optional input used.
C RWORK  = Real work array of length at least..
C             20 + 16*NEQ                      for MF = 10,
C             22 +  9*NEQ + 2*NEQ**2           for MF = 21 or 22,
C             22 + 11*NEQ + (3*ML + 2*MU)*NEQ  for MF = 24 or 25.
C LRW    = Declared length of RWORK (in user's DIMENSION statement).
C IWORK  = Integer work array of length at least..
C             30        for MF = 10,
C             30 + NEQ  for MF = 21, 22, 24, or 25.
C          If MF = 24 or 25, input in IWORK(1),IWORK(2) the lower
C          and upper half-bandwidths ML,MU.
C LIW    = Declared length of IWORK (in user's DIMENSION).
C JAC    = Name of subroutine for Jacobian matrix (MF = 21 or 24).
C          If used, this name must be declared external in calling
C          program.  If not used, pass a dummy name.
C MF     = Method flag.  Standard values are..
C          10 for nonstiff (Adams) method, no Jacobian used.
C          21 for stiff (BDF) method, user-supplied full Jacobian.
C          22 for stiff method, internally generated full Jacobian.
C          24 for stiff method, user-supplied banded Jacobian.
C          25 for stiff method, internally generated banded Jacobian.
C RPAR,IPAR = user-defined real and integer arrays passed to F and JAC.
C Note that the main program must declare arrays Y, RWORK, IWORK,
C and possibly ATOL, RPAR, and IPAR.
C
C E. The output from the first call (or any call) is..
C      Y = Array of computed values of y(t) vector.
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
C
C F. To continue the integration after a successful return, simply
C reset TOUT and call DVODE again.  No other parameters need be reset.
C
C-----------------------------------------------------------------------
C EXAMPLE PROBLEM
C
C The following is a simple example problem, with the coding
C needed for its solution by DVODE.  The problem is from chemical
C kinetics, and consists of the following three rate equations..
C     dy1/dt = -.04*y1 + 1.e4*y2*y3
C     dy2/dt = .04*y1 - 1.e4*y2*y3 - 3.e7*y2**2
C     dy3/dt = 3.e7*y2**2
C on the interval from t = 0.0 to t = 4.e10, with initial conditions
C y1 = 1.0, y2 = y3 = 0.  The problem is stiff.
C
C The following coding solves this problem with DVODE, using MF = 21
C and printing results at t = .4, 4., ..., 4.e10.  It uses
C ITOL = 2 and ATOL much smaller for y2 than y1 or y3 because
C y2 has much smaller values.
C At the end of the run, statistical quantities of interest are
C printed. (See optional output in the full description below.)
C To generate Fortran source code, replace C in column 1 with a blank
C in the coding below.
C
C     EXTERNAL FEX, JEX
C     DOUBLE PRECISION ATOL, RPAR, RTOL, RWORK, T, TOUT, Y
C     DIMENSION Y(3), ATOL(3), RWORK(67), IWORK(33)
C     NEQ = 3
C     Y(1) = 1.0D0
C     Y(2) = 0.0D0
C     Y(3) = 0.0D0
C     T = 0.0D0
C     TOUT = 0.4D0
C     ITOL = 2
C     RTOL = 1.D-4
C     ATOL(1) = 1.D-8
C     ATOL(2) = 1.D-14
C     ATOL(3) = 1.D-6
C     ITASK = 1
C     ISTATE = 1
C     IOPT = 0
C     LRW = 67
C     LIW = 33
C     MF = 21
C     DO 40 IOUT = 1,12
C       CALL DVODE(FEX,NEQ,Y,T,TOUT,ITOL,RTOL,ATOL,ITASK,ISTATE,
C    1            IOPT,RWORK,LRW,IWORK,LIW,JEX,MF,RPAR,IPAR)
C       WRITE(6,20)T,Y(1),Y(2),Y(3)
C 20    FORMAT(' At t =',D12.4,'   y =',3D14.6)
C       IF (ISTATE .LT. 0) GO TO 80
C 40    TOUT = TOUT*10.
C     WRITE(6,60) IWORK(11),IWORK(12),IWORK(13),IWORK(19),
C    1            IWORK(20),IWORK(21),IWORK(22)
C 60  FORMAT(/' No. steps =',I4,'   No. f-s =',I4,
C    1       '   No. J-s =',I4,'   No. LU-s =',I4/
C    2       '  No. nonlinear iterations =',I4/
C    3       '  No. nonlinear convergence failures =',I4/
C    4       '  No. error test failures =',I4/)
C     STOP
C 80  WRITE(6,90)ISTATE
C 90  FORMAT(///' Error halt.. ISTATE =',I3)
C     STOP
C     END
C
C     SUBROUTINE FEX (NEQ, T, Y, YDOT, RPAR, IPAR)
C     DOUBLE PRECISION RPAR, T, Y, YDOT
C     DIMENSION Y(NEQ), YDOT(NEQ)
C     YDOT(1) = -.04D0*Y(1) + 1.D4*Y(2)*Y(3)
C     YDOT(3) = 3.D7*Y(2)*Y(2)
C     YDOT(2) = -YDOT(1) - YDOT(3)
C     RETURN
C     END
C
C     SUBROUTINE JEX (NEQ, T, Y, ML, MU, PD, NRPD, RPAR, IPAR)
C     DOUBLE PRECISION PD, RPAR, T, Y
C     DIMENSION Y(NEQ), PD(NRPD,NEQ)
C     PD(1,1) = -.04D0
C     PD(1,2) = 1.D4*Y(3)
C     PD(1,3) = 1.D4*Y(2)
C     PD(2,1) = .04D0
C     PD(2,3) = -PD(1,3)
C     PD(3,2) = 6.D7*Y(2)
C     PD(2,2) = -PD(1,2) - PD(3,2)
C     RETURN
C     END
C
C The following output was obtained from the above program on a
C Cray-1 computer with the CFT compiler.
C
C At t =  4.0000e-01   y =  9.851680e-01  3.386314e-05  1.479817e-02
C At t =  4.0000e+00   y =  9.055255e-01  2.240539e-05  9.445214e-02
C At t =  4.0000e+01   y =  7.158108e-01  9.184883e-06  2.841800e-01
C At t =  4.0000e+02   y =  4.505032e-01  3.222940e-06  5.494936e-01
C At t =  4.0000e+03   y =  1.832053e-01  8.942690e-07  8.167938e-01
C At t =  4.0000e+04   y =  3.898560e-02  1.621875e-07  9.610142e-01
C At t =  4.0000e+05   y =  4.935882e-03  1.984013e-08  9.950641e-01
C At t =  4.0000e+06   y =  5.166183e-04  2.067528e-09  9.994834e-01
C At t =  4.0000e+07   y =  5.201214e-05  2.080593e-10  9.999480e-01
C At t =  4.0000e+08   y =  5.213149e-06  2.085271e-11  9.999948e-01
C At t =  4.0000e+09   y =  5.183495e-07  2.073399e-12  9.999995e-01
C At t =  4.0000e+10   y =  5.450996e-08  2.180399e-13  9.999999e-01
C
C No. steps = 595   No. f-s = 832   No. J-s =  13   No. LU-s = 112
C  No. nonlinear iterations = 831
C  No. nonlinear convergence failures =   0
C  No. error test failures =  22
C-----------------------------------------------------------------------
C Full description of user interface to DVODE.
C
C The user interface to DVODE consists of the following parts.
C
C i.   The call sequence to subroutine DVODE, which is a driver
C      routine for the solver.  This includes descriptions of both
C      the call sequence arguments and of user-supplied routines.
C      Following these descriptions is
C        * a description of optional input available through the
C          call sequence,
C        * a description of optional output (in the work arrays), and
C        * instructions for interrupting and restarting a solution.
C
C ii.  Descriptions of other routines in the DVODE package that may be
C      (optionally) called by the user.  These provide the ability to
C      alter error message handling, save and restore the internal
C      COMMON, and obtain specified derivatives of the solution y(t).
C
C iii. Descriptions of COMMON blocks to be declared in overlay
C      or similar environments.
C
C iv.  Description of two routines in the DVODE package, either of
C      which the user may replace with his own version, if desired.
C      these relate to the measurement of errors.
C
C-----------------------------------------------------------------------
C Part i.  Call Sequence.
C
C The call sequence parameters used for input only are
C     F, NEQ, TOUT, ITOL, RTOL, ATOL, ITASK, IOPT, LRW, LIW, JAC, MF,
C and those used for both input and output are
C     Y, T, ISTATE.
C The work arrays RWORK and IWORK are also used for conditional and
C optional input and optional output.  (The term output here refers
C to the return from subroutine DVODE to the user's calling program.)
C
C The legality of input parameters will be thoroughly checked on the
C initial call for the problem, but not checked thereafter unless a
C change in input parameters is flagged by ISTATE = 3 in the input.
C
C The descriptions of the call arguments are as follows.
C
C F      = The name of the user-supplied subroutine defining the
C          ODE system.  The system must be put in the first-order
C          form dy/dt = f(t,y), where f is a vector-valued function
C          of the scalar t and the vector y.  Subroutine F is to
C          compute the function f.  It is to have the form
C               SUBROUTINE F (NEQ, T, Y, YDOT, RPAR, IPAR)
C               DOUBLE PRECISION T, Y, YDOT, RPAR
C               DIMENSION Y(NEQ), YDOT(NEQ)
C          where NEQ, T, and Y are input, and the array YDOT = f(t,y)
C          is output.  Y and YDOT are arrays of length NEQ.
C          (In the DIMENSION statement above, NEQ  can be replaced by
C          *  to make  Y  and  YDOT  assumed size arrays.)
C          Subroutine F should not alter Y(1),...,Y(NEQ).
C          F must be declared EXTERNAL in the calling program.
C
C          Subroutine F may access user-defined real and integer
C          work arrays RPAR and IPAR, which are to be dimensioned
C          in the main program.
C
C          If quantities computed in the F routine are needed
C          externally to DVODE, an extra call to F should be made
C          for this purpose, for consistent and accurate results.
C          If only the derivative dy/dt is needed, use DVINDY instead.
C
C NEQ    = The size of the ODE system (number of first order
C          ordinary differential equations).  Used only for input.
C          NEQ may not be increased during the problem, but
C          can be decreased (with ISTATE = 3 in the input).
C
C Y      = A real array for the vector of dependent variables, of
C          length NEQ or more.  Used for both input and output on the
C          first call (ISTATE = 1), and only for output on other calls.
C          On the first call, Y must contain the vector of initial
C          values.  In the output, Y contains the computed solution
C          evaluated at T.  If desired, the Y array may be used
C          for other purposes between calls to the solver.
C
C          This array is passed as the Y argument in all calls to
C          F and JAC.
C
C T      = The independent variable.  In the input, T is used only on
C          the first call, as the initial point of the integration.
C          In the output, after each call, T is the value at which a
C          computed solution Y is evaluated (usually the same as TOUT).
C          On an error return, T is the farthest point reached.
C
C TOUT   = The next value of t at which a computed solution is desired.
C          Used only for input.
C
C          When starting the problem (ISTATE = 1), TOUT may be equal
C          to T for one call, then should .ne. T for the next call.
C          For the initial T, an input value of TOUT .ne. T is used
C          in order to determine the direction of the integration
C          (i.e. the algebraic sign of the step sizes) and the rough
C          scale of the problem.  Integration in either direction
C          (forward or backward in t) is permitted.
C
C          If ITASK = 2 or 5 (one-step modes), TOUT is ignored after
C          the first call (i.e. the first call with TOUT .ne. T).
C          Otherwise, TOUT is required on every call.
C
C          If ITASK = 1, 3, or 4, the values of TOUT need not be
C          monotone, but a value of TOUT which backs up is limited
C          to the current internal t interval, whose endpoints are
C          TCUR - HU and TCUR.  (See optional output, below, for
C          TCUR and HU.)
C
C ITOL   = An indicator for the type of error control.  See
C          description below under ATOL.  Used only for input.
C
C RTOL   = A relative error tolerance parameter, either a scalar or
C          an array of length NEQ.  See description below under ATOL.
C          Input only.
C
C ATOL   = An absolute error tolerance parameter, either a scalar or
C          an array of length NEQ.  Input only.
C
C          The input parameters ITOL, RTOL, and ATOL determine
C          the error control performed by the solver.  The solver will
C          control the vector e = (e(i)) of estimated local errors
C          in Y, according to an inequality of the form
C                      rms-norm of ( e(i)/EWT(i) )   .le.   1,
C          where       EWT(i) = RTOL(i)*abs(Y(i)) + ATOL(i),
C          and the rms-norm (root-mean-square norm) here is
C          rms-norm(v) = sqrt(sum v(i)**2 / NEQ).  Here EWT = (EWT(i))
C          is a vector of weights which must always be positive, and
C          the values of RTOL and ATOL should all be non-negative.
C          The following table gives the types (scalar/array) of
C          RTOL and ATOL, and the corresponding form of EWT(i).
C
C             ITOL    RTOL       ATOL          EWT(i)
C              1     scalar     scalar     RTOL*ABS(Y(i)) + ATOL
C              2     scalar     array      RTOL*ABS(Y(i)) + ATOL(i)
C              3     array      scalar     RTOL(i)*ABS(Y(i)) + ATOL
C              4     array      array      RTOL(i)*ABS(Y(i)) + ATOL(i)
C
C          When either of these parameters is a scalar, it need not
C          be dimensioned in the user's calling program.
C
C          If none of the above choices (with ITOL, RTOL, and ATOL
C          fixed throughout the problem) is suitable, more general
C          error controls can be obtained by substituting
C          user-supplied routines for the setting of EWT and/or for
C          the norm calculation.  See Part iv below.
C
C          If global errors are to be estimated by making a repeated
C          run on the same problem with smaller tolerances, then all
C          components of RTOL and ATOL (i.e. of EWT) should be scaled
C          down uniformly.
C
C ITASK  = An index specifying the task to be performed.
C          Input only.  ITASK has the following values and meanings.
C          1  means normal computation of output values of y(t) at
C             t = TOUT (by overshooting and interpolating).
C          2  means take one step only and return.
C          3  means stop at the first internal mesh point at or
C             beyond t = TOUT and return.
C          4  means normal computation of output values of y(t) at
C             t = TOUT but without overshooting t = TCRIT.
C             TCRIT must be input as RWORK(1).  TCRIT may be equal to
C             or beyond TOUT, but not behind it in the direction of
C             integration.  This option is useful if the problem
C             has a singularity at or beyond t = TCRIT.
C          5  means take one step, without passing TCRIT, and return.
C             TCRIT must be input as RWORK(1).
C
C          Note..  If ITASK = 4 or 5 and the solver reaches TCRIT
C          (within roundoff), it will return T = TCRIT (exactly) to
C          indicate this (unless ITASK = 4 and TOUT comes before TCRIT,
C          in which case answers at T = TOUT are returned first).
C
C ISTATE = an index used for input and output to specify the
C          the state of the calculation.
C
C          In the input, the values of ISTATE are as follows.
C          1  means this is the first call for the problem
C             (initializations will be done).  See note below.
C          2  means this is not the first call, and the calculation
C             is to continue normally, with no change in any input
C             parameters except possibly TOUT and ITASK.
C             (If ITOL, RTOL, and/or ATOL are changed between calls
C             with ISTATE = 2, the new values will be used but not
C             tested for legality.)
C          3  means this is not the first call, and the
C             calculation is to continue normally, but with
C             a change in input parameters other than
C             TOUT and ITASK.  Changes are allowed in
C             NEQ, ITOL, RTOL, ATOL, IOPT, LRW, LIW, MF, ML, MU,
C             and any of the optional input except H0.
C             (See IWORK description for ML and MU.)
C          Note..  A preliminary call with TOUT = T is not counted
C          as a first call here, as no initialization or checking of
C          input is done.  (Such a call is sometimes useful to include
C          the initial conditions in the output.)
C          Thus the first call for which TOUT .ne. T requires
C          ISTATE = 1 in the input.
C
C          In the output, ISTATE has the following values and meanings.
C           1  means nothing was done, as TOUT was equal to T with
C              ISTATE = 1 in the input.
C           2  means the integration was performed successfully.
C          -1  means an excessive amount of work (more than MXSTEP
C              steps) was done on this call, before completing the
C              requested task, but the integration was otherwise
C              successful as far as T.  (MXSTEP is an optional input
C              and is normally 500.)  To continue, the user may
C              simply reset ISTATE to a value .gt. 1 and call again.
C              (The excess work step counter will be reset to 0.)
C              In addition, the user may increase MXSTEP to avoid
C              this error return.  (See optional input below.)
C          -2  means too much accuracy was requested for the precision
C              of the machine being used.  This was detected before
C              completing the requested task, but the integration
C              was successful as far as T.  To continue, the tolerance
C              parameters must be reset, and ISTATE must be set
C              to 3.  The optional output TOLSF may be used for this
C              purpose.  (Note.. If this condition is detected before
C              taking any steps, then an illegal input return
C              (ISTATE = -3) occurs instead.)
C          -3  means illegal input was detected, before taking any
C              integration steps.  See written message for details.
C              Note..  If the solver detects an infinite loop of calls
C              to the solver with illegal input, it will cause
C              the run to stop.
C          -4  means there were repeated error test failures on
C              one attempted step, before completing the requested
C              task, but the integration was successful as far as T.
C              The problem may have a singularity, or the input
C              may be inappropriate.
C          -5  means there were repeated convergence test failures on
C              one attempted step, before completing the requested
C              task, but the integration was successful as far as T.
C              This may be caused by an inaccurate Jacobian matrix,
C              if one is being used.
C          -6  means EWT(i) became zero for some i during the
C              integration.  Pure relative error control (ATOL(i)=0.0)
C              was requested on a variable which has now vanished.
C              The integration was successful as far as T.
C
C          Note..  Since the normal output value of ISTATE is 2,
C          it does not need to be reset for normal continuation.
C          Also, since a negative input value of ISTATE will be
C          regarded as illegal, a negative output value requires the
C          user to change it, and possibly other input, before
C          calling the solver again.
C
C IOPT   = An integer flag to specify whether or not any optional
C          input is being used on this call.  Input only.
C          The optional input is listed separately below.
C          IOPT = 0 means no optional input is being used.
C                   Default values will be used in all cases.
C          IOPT = 1 means optional input is being used.
C
C RWORK  = A real working array (double precision).
C          The length of RWORK must be at least
C             20 + NYH*(MAXORD + 1) + 3*NEQ + LWM    where
C          NYH    = the initial value of NEQ,
C          MAXORD = 12 (if METH = 1) or 5 (if METH = 2) (unless a
C                   smaller value is given as an optional input),
C          LWM = length of work space for matrix-related data..
C          LWM = 0             if MITER = 0,
C          LWM = 2*NEQ**2 + 2  if MITER = 1 or 2, and MF.gt.0,
C          LWM = NEQ**2 + 2    if MITER = 1 or 2, and MF.lt.0,
C          LWM = NEQ + 2       if MITER = 3,
C          LWM = (3*ML+2*MU+2)*NEQ + 2 if MITER = 4 or 5, and MF.gt.0,
C          LWM = (2*ML+MU+1)*NEQ + 2   if MITER = 4 or 5, and MF.lt.0.
C          (See the MF description for METH and MITER.)
C          Thus if MAXORD has its default value and NEQ is constant,
C          this length is..
C             20 + 16*NEQ                    for MF = 10,
C             22 + 16*NEQ + 2*NEQ**2         for MF = 11 or 12,
C             22 + 16*NEQ + NEQ**2           for MF = -11 or -12,
C             22 + 17*NEQ                    for MF = 13,
C             22 + 18*NEQ + (3*ML+2*MU)*NEQ  for MF = 14 or 15,
C             22 + 17*NEQ + (2*ML+MU)*NEQ    for MF = -14 or -15,
C             20 +  9*NEQ                    for MF = 20,
C             22 +  9*NEQ + 2*NEQ**2         for MF = 21 or 22,
C             22 +  9*NEQ + NEQ**2           for MF = -21 or -22,
C             22 + 10*NEQ                    for MF = 23,
C             22 + 11*NEQ + (3*ML+2*MU)*NEQ  for MF = 24 or 25.
C             22 + 10*NEQ + (2*ML+MU)*NEQ    for MF = -24 or -25.
C          The first 20 words of RWORK are reserved for conditional
C          and optional input and optional output.
C
C          The following word in RWORK is a conditional input..
C            RWORK(1) = TCRIT = critical value of t which the solver
C                       is not to overshoot.  Required if ITASK is
C                       4 or 5, and ignored otherwise.  (See ITASK.)
C
C LRW    = The length of the array RWORK, as declared by the user.
C          (This will be checked by the solver.)
C
C IWORK  = An integer work array.  The length of IWORK must be at least
C             30        if MITER = 0 or 3 (MF = 10, 13, 20, 23), or
C             30 + NEQ  otherwise (abs(MF) = 11,12,14,15,21,22,24,25).
C          The first 30 words of IWORK are reserved for conditional and
C          optional input and optional output.
C
C          The following 2 words in IWORK are conditional input..
C            IWORK(1) = ML     These are the lower and upper
C            IWORK(2) = MU     half-bandwidths, respectively, of the
C                       banded Jacobian, excluding the main diagonal.
C                       The band is defined by the matrix locations
C                       (i,j) with i-ML .le. j .le. i+MU.  ML and MU
C                       must satisfy  0 .le.  ML,MU  .le. NEQ-1.
C                       These are required if MITER is 4 or 5, and
C                       ignored otherwise.  ML and MU may in fact be
C                       the band parameters for a matrix to which
C                       df/dy is only approximately equal.
C
C LIW    = the length of the array IWORK, as declared by the user.
C          (This will be checked by the solver.)
C
C Note..  The work arrays must not be altered between calls to DVODE
C for the same problem, except possibly for the conditional and
C optional input, and except for the last 3*NEQ words of RWORK.
C The latter space is used for internal scratch space, and so is
C available for use by the user outside DVODE between calls, if
C desired (but not for use by F or JAC).
C
C JAC    = The name of the user-supplied routine (MITER = 1 or 4) to
C          compute the Jacobian matrix, df/dy, as a function of
C          the scalar t and the vector y.  It is to have the form
C               SUBROUTINE JAC (NEQ, T, Y, ML, MU, PD, NROWPD,
C                               RPAR, IPAR)
C               DOUBLE PRECISION T, Y, PD, RPAR
C               DIMENSION Y(NEQ), PD(NROWPD, NEQ)
C          where NEQ, T, Y, ML, MU, and NROWPD are input and the array
C          PD is to be loaded with partial derivatives (elements of the
C          Jacobian matrix) in the output.  PD must be given a first
C          dimension of NROWPD.  T and Y have the same meaning as in
C          Subroutine F.  (In the DIMENSION statement above, NEQ can
C          be replaced by  *  to make Y and PD assumed size arrays.)
C               In the full matrix case (MITER = 1), ML and MU are
C          ignored, and the Jacobian is to be loaded into PD in
C          columnwise manner, with df(i)/dy(j) loaded into PD(i,j).
C               In the band matrix case (MITER = 4), the elements
C          within the band are to be loaded into PD in columnwise
C          manner, with diagonal lines of df/dy loaded into the rows
C          of PD. Thus df(i)/dy(j) is to be loaded into PD(i-j+MU+1,j).
C          ML and MU are the half-bandwidth parameters. (See IWORK).
C          The locations in PD in the two triangular areas which
C          correspond to nonexistent matrix elements can be ignored
C          or loaded arbitrarily, as they are overwritten by DVODE.
C               JAC need not provide df/dy exactly.  A crude
C          approximation (possibly with a smaller bandwidth) will do.
C               In either case, PD is preset to zero by the solver,
C          so that only the nonzero elements need be loaded by JAC.
C          Each call to JAC is preceded by a call to F with the same
C          arguments NEQ, T, and Y.  Thus to gain some efficiency,
C          intermediate quantities shared by both calculations may be
C          saved in a user COMMON block by F and not recomputed by JAC,
C          if desired.  Also, JAC may alter the Y array, if desired.
C          JAC must be declared external in the calling program.
C               Subroutine JAC may access user-defined real and integer
C          work arrays, RPAR and IPAR, whose dimensions are set by the
C          user in the main program.
C
C MF     = The method flag.  Used only for input.  The legal values of
C          MF are 10, 11, 12, 13, 14, 15, 20, 21, 22, 23, 24, 25,
C          -11, -12, -14, -15, -21, -22, -24, -25.
C          MF is a signed two-digit integer, MF = JSV*(10*METH + MITER).
C          JSV = SIGN(MF) indicates the Jacobian-saving strategy..
C            JSV =  1 means a copy of the Jacobian is saved for reuse
C                     in the corrector iteration algorithm.
C            JSV = -1 means a copy of the Jacobian is not saved
C                     (valid only for MITER = 1, 2, 4, or 5).
C          METH indicates the basic linear multistep method..
C            METH = 1 means the implicit Adams method.
C            METH = 2 means the method based on backward
C                     differentiation formulas (BDF-s).
C          MITER indicates the corrector iteration method..
C            MITER = 0 means functional iteration (no Jacobian matrix
C                      is involved).
C            MITER = 1 means chord iteration with a user-supplied
C                      full (NEQ by NEQ) Jacobian.
C            MITER = 2 means chord iteration with an internally
C                      generated (difference quotient) full Jacobian
C                      (using NEQ extra calls to F per df/dy value).
C            MITER = 3 means chord iteration with an internally
C                      generated diagonal Jacobian approximation
C                      (using 1 extra call to F per df/dy evaluation).
C            MITER = 4 means chord iteration with a user-supplied
C                      banded Jacobian.
C            MITER = 5 means chord iteration with an internally
C                      generated banded Jacobian (using ML+MU+1 extra
C                      calls to F per df/dy evaluation).
C          If MITER = 1 or 4, the user must supply a subroutine JAC
C          (the name is arbitrary) as described above under JAC.
C          For other values of MITER, a dummy argument can be used.
C
C RPAR     User-specified array used to communicate real parameters
C          to user-supplied subroutines.  If RPAR is a vector, then
C          it must be dimensioned in the user's main program.  If it
C          is unused or it is a scalar, then it need not be
C          dimensioned.
C
C IPAR     User-specified array used to communicate integer parameter
C          to user-supplied subroutines.  The comments on dimensioning
C          RPAR apply to IPAR.
C-----------------------------------------------------------------------
C Optional Input.
C
C The following is a list of the optional input provided for in the
C call sequence.  (See also Part ii.)  For each such input variable,
C this table lists its name as used in this documentation, its
C location in the call sequence, its meaning, and the default value.
C The use of any of this input requires IOPT = 1, and in that
C case all of this input is examined.  A value of zero for any
C of these optional input variables will cause the default value to be
C used.  Thus to use a subset of the optional input, simply preload
C locations 5 to 10 in RWORK and IWORK to 0.0 and 0 respectively, and
C then set those of interest to nonzero values.
C
C NAME    LOCATION      MEANING AND DEFAULT VALUE
C
C H0      RWORK(5)  The step size to be attempted on the first step.
C                   The default value is determined by the solver.
C
C HMAX    RWORK(6)  The maximum absolute step size allowed.
C                   The default value is infinite.
C
C HMIN    RWORK(7)  The minimum absolute step size allowed.
C                   The default value is 0.  (This lower bound is not
C                   enforced on the final step before reaching TCRIT
C                   when ITASK = 4 or 5.)
C
C MAXORD  IWORK(5)  The maximum order to be allowed.  The default
C                   value is 12 if METH = 1, and 5 if METH = 2.
C                   If MAXORD exceeds the default value, it will
C                   be reduced to the default value.
C                   If MAXORD is changed during the problem, it may
C                   cause the current order to be reduced.
C
C MXSTEP  IWORK(6)  Maximum number of (internally defined) steps
C                   allowed during one call to the solver.
C                   The default value is 500.
C
C MXHNIL  IWORK(7)  Maximum number of messages printed (per problem)
C                   warning that T + H = T on a step (H = step size).
C                   This must be positive to result in a non-default
C                   value.  The default value is 10.
C
C-----------------------------------------------------------------------
C Optional Output.
C
C As optional additional output from DVODE, the variables listed
C below are quantities related to the performance of DVODE
C which are available to the user.  These are communicated by way of
C the work arrays, but also have internal mnemonic names as shown.
C Except where stated otherwise, all of this output is defined
C on any successful return from DVODE, and on any return with
C ISTATE = -1, -2, -4, -5, or -6.  On an illegal input return
C (ISTATE = -3), they will be unchanged from their existing values
C (if any), except possibly for TOLSF, LENRW, and LENIW.
C On any error return, output relevant to the error will be defined,
C as noted below.
C
C NAME    LOCATION      MEANING
C
C HU      RWORK(11) The step size in t last used (successfully).
C
C HCUR    RWORK(12) The step size to be attempted on the next step.
C
C TCUR    RWORK(13) The current value of the independent variable
C                   which the solver has actually reached, i.e. the
C                   current internal mesh point in t.  In the output,
C                   TCUR will always be at least as far from the
C                   initial value of t as the current argument T,
C                   but may be farther (if interpolation was done).
C
C TOLSF   RWORK(14) A tolerance scale factor, greater than 1.0,
C                   computed when a request for too much accuracy was
C                   detected (ISTATE = -3 if detected at the start of
C                   the problem, ISTATE = -2 otherwise).  If ITOL is
C                   left unaltered but RTOL and ATOL are uniformly
C                   scaled up by a factor of TOLSF for the next call,
C                   then the solver is deemed likely to succeed.
C                   (The user may also ignore TOLSF and alter the
C                   tolerance parameters in any other way appropriate.)
C
C NST     IWORK(11) The number of steps taken for the problem so far.
C
C NFE     IWORK(12) The number of f evaluations for the problem so far.
C
C NJE     IWORK(13) The number of Jacobian evaluations so far.
C
C NQU     IWORK(14) The method order last used (successfully).
C
C NQCUR   IWORK(15) The order to be attempted on the next step.
C
C IMXER   IWORK(16) The index of the component of largest magnitude in
C                   the weighted local error vector ( e(i)/EWT(i) ),
C                   on an error return with ISTATE = -4 or -5.
C
C LENRW   IWORK(17) The length of RWORK actually required.
C                   This is defined on normal returns and on an illegal
C                   input return for insufficient storage.
C
C LENIW   IWORK(18) The length of IWORK actually required.
C                   This is defined on normal returns and on an illegal
C                   input return for insufficient storage.
C
C NLU     IWORK(19) The number of matrix LU decompositions so far.
C
C NNI     IWORK(20) The number of nonlinear (Newton) iterations so far.
C
C NCFN    IWORK(21) The number of convergence failures of the nonlinear
C                   solver so far.
C
C NETF    IWORK(22) The number of error test failures of the integrator
C                   so far.
C
C The following two arrays are segments of the RWORK array which
C may also be of interest to the user as optional output.
C For each array, the table below gives its internal name,
C its base address in RWORK, and its description.
C
C NAME    BASE ADDRESS      DESCRIPTION
C
C YH      21             The Nordsieck history array, of size NYH by
C                        (NQCUR + 1), where NYH is the initial value
C                        of NEQ.  For j = 0,1,...,NQCUR, column j+1
C                        of YH contains HCUR**j/factorial(j) times
C                        the j-th derivative of the interpolating
C                        polynomial currently representing the
C                        solution, evaluated at t = TCUR.
C
C ACOR     LENRW-NEQ+1   Array of size NEQ used for the accumulated
C                        corrections on each step, scaled in the output
C                        to represent the estimated local error in Y
C                        on the last step.  This is the vector e in
C                        the description of the error control.  It is
C                        defined only on a successful return from DVODE.
C
C-----------------------------------------------------------------------
C Interrupting and Restarting
C
C If the integration of a given problem by DVODE is to be
C interrrupted and then later continued, such as when restarting
C an interrupted run or alternating between two or more ODE problems,
C the user should save, following the return from the last DVODE call
C prior to the interruption, the contents of the call sequence
C variables and internal COMMON blocks, and later restore these
C values before the next DVODE call for that problem.  To save
C and restore the COMMON blocks, use subroutine DVSRCO, as
C described below in part ii.
C
C In addition, if non-default values for either LUN or MFLAG are
C desired, an extra call to XSETUN and/or XSETF should be made just
C before continuing the integration.  See Part ii below for details.
C
C-----------------------------------------------------------------------
C Part ii.  Other Routines Callable.
C
C The following are optional calls which the user may make to
C gain additional capabilities in conjunction with DVODE.
C (The routines XSETUN and XSETF are designed to conform to the
C SLATEC error handling package.)
C
C     FORM OF CALL                  FUNCTION
C  CALL XSETUN(LUN)           Set the logical unit number, LUN, for
C                             output of messages from DVODE, if
C                             the default is not desired.
C                             The default value of LUN is 6.
C
C  CALL XSETF(MFLAG)          Set a flag to control the printing of
C                             messages by DVODE.
C                             MFLAG = 0 means do not print. (Danger..
C                             This risks losing valuable information.)
C                             MFLAG = 1 means print (the default).
C
C                             Either of the above calls may be made at
C                             any time and will take effect immediately.
C
C  CALL DVSRCO(RSAV,ISAV,JOB) Saves and restores the contents of
C                             the internal COMMON blocks used by
C                             DVODE. (See Part iii below.)
C                             RSAV must be a real array of length 49
C                             or more, and ISAV must be an integer
C                             array of length 40 or more.
C                             JOB=1 means save COMMON into RSAV/ISAV.
C                             JOB=2 means restore COMMON from RSAV/ISAV.
C                                DVSRCO is useful if one is
C                             interrupting a run and restarting
C                             later, or alternating between two or
C                             more problems solved with DVODE.
C
C  CALL DVINDY(,,,,,)         Provide derivatives of y, of various
C        (See below.)         orders, at a specified point T, if
C                             desired.  It may be called only after
C                             a successful return from DVODE.
C
C The detailed instructions for using DVINDY are as follows.
C The form of the call is..
C
C  CALL DVINDY (T, K, RWORK(21), NYH, DKY, IFLAG)
C
C The input parameters are..
C
C T         = Value of independent variable where answers are desired
C             (normally the same as the T last returned by DVODE).
C             For valid results, T must lie between TCUR - HU and TCUR.
C             (See optional output for TCUR and HU.)
C K         = Integer order of the derivative desired.  K must satisfy
C             0 .le. K .le. NQCUR, where NQCUR is the current order
C             (see optional output).  The capability corresponding
C             to K = 0, i.e. computing y(T), is already provided
C             by DVODE directly.  Since NQCUR .ge. 1, the first
C             derivative dy/dt is always available with DVINDY.
C RWORK(21) = The base address of the history array YH.
C NYH       = Column length of YH, equal to the initial value of NEQ.
C
C The output parameters are..
C
C DKY       = A real array of length NEQ containing the computed value
C             of the K-th derivative of y(t).
C IFLAG     = Integer flag, returned as 0 if K and T were legal,
C             -1 if K was illegal, and -2 if T was illegal.
C             On an error return, a message is also written.
C-----------------------------------------------------------------------
C Part iii.  COMMON Blocks.
C If DVODE is to be used in an overlay situation, the user
C must declare, in the primary overlay, the variables in..
C   (1) the call sequence to DVODE,
C   (2) the two internal COMMON blocks
C         /DVOD01/  of length  81  (48 double precision words
C                         followed by 33 integer words),
C         /DVOD02/  of length  9  (1 double precision word
C                         followed by 8 integer words),
C
C If DVODE is used on a system in which the contents of internal
C COMMON blocks are not preserved between calls, the user should
C declare the above two COMMON blocks in his main program to insure
C that their contents are preserved.
C
C-----------------------------------------------------------------------
C Part iv.  Optionally Replaceable Solver Routines.
C
C Below are descriptions of two routines in the DVODE package which
C relate to the measurement of errors.  Either routine can be
C replaced by a user-supplied version, if desired.  However, since such
C a replacement may have a major impact on performance, it should be
C done only when absolutely necessary, and only with great caution.
C (Note.. The means by which the package version of a routine is
C superseded by the user's version may be system-dependent.)
C
C (a) DEWSET.
C The following subroutine is called just before each internal
C integration step, and sets the array of error weights, EWT, as
C described under ITOL/RTOL/ATOL above..
C     SUBROUTINE DEWSET (NEQ, ITOL, RTOL, ATOL, YCUR, EWT)
C where NEQ, ITOL, RTOL, and ATOL are as in the DVODE call sequence,
C YCUR contains the current dependent variable vector, and
C EWT is the array of weights set by DEWSET.
C
C If the user supplies this subroutine, it must return in EWT(i)
C (i = 1,...,NEQ) a positive quantity suitable for comparison with
C errors in Y(i).  The EWT array returned by DEWSET is passed to the
C DVNORM routine (See below.), and also used by DVODE in the computation
C of the optional output IMXER, the diagonal Jacobian approximation,
C and the increments for difference quotient Jacobians.
C
C In the user-supplied version of DEWSET, it may be desirable to use
C the current values of derivatives of y.  Derivatives up to order NQ
C are available from the history array YH, described above under
C Optional Output.  In DEWSET, YH is identical to the YCUR array,
C extended to NQ + 1 columns with a column length of NYH and scale
C factors of h**j/factorial(j).  On the first call for the problem,
C given by NST = 0, NQ is 1 and H is temporarily set to 1.0.
C NYH is the initial value of NEQ.  The quantities NQ, H, and NST
C can be obtained by including in DEWSET the statements..
C     DOUBLE PRECISION RVOD, H, HU
C     COMMON /DVOD01/ RVOD(48), IVOD(33)
C     COMMON /DVOD02/ HU, NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
C     NQ = IVOD(28)
C     H = RVOD(21)
C Thus, for example, the current value of dy/dt can be obtained as
C YCUR(NYH+i)/H  (i=1,...,NEQ)  (and the division by H is
C unnecessary when NST = 0).
C
C (b) DVNORM.
C The following is a real function routine which computes the weighted
C root-mean-square norm of a vector v..
C     D = DVNORM (N, V, W)
C where..
C   N = the length of the vector,
C   V = real array of length N containing the vector,
C   W = real array of length N containing weights,
C   D = sqrt( (1/N) * sum(V(i)*W(i))**2 ).
C DVNORM is called with N = NEQ and with W(i) = 1.0/EWT(i), where
C EWT is as set by subroutine DEWSET.
C
C If the user supplies this function, it should return a non-negative
C value of DVNORM suitable for use in the error control in DVODE.
C None of the arguments should be altered by DVNORM.
C For example, a user-supplied DVNORM routine might..
C   -substitute a max-norm of (V(i)*W(i)) for the rms-norm, or
C   -ignore some components of V in the norm, with the effect of
C    suppressing the error control on those components of Y.
C-----------------------------------------------------------------------
C Other Routines in the DVODE Package.
C
C In addition to subroutine DVODE, the DVODE package includes the
C following subroutines and function routines..
C  DVHIN     computes an approximate step size for the initial step.
C  DVINDY    computes an interpolated value of the y vector at t = TOUT.
C  DVSTEP    is the core integrator, which does one step of the
C            integration and the associated error control.
C  DVSET     sets all method coefficients and test constants.
C  DVNLSD    solves the underlying nonlinear system -- the corrector.
C  DVJAC     computes and preprocesses the Jacobian matrix J = df/dy
C            and the Newton iteration matrix P = I - (h/l1)*J.
C  DVSOL     manages solution of linear system in chord iteration.
C  DVJUST    adjusts the history array on a change of order.
C  DEWSET    sets the error weight vector EWT before each step.
C  DVNORM    computes the weighted r.m.s. norm of a vector.
C  DVSRCO    is a user-callable routines to save and restore
C            the contents of the internal COMMON blocks.
C  DACOPY    is a routine to copy one two-dimensional array to another.
C  DGEFA and DGESL   are routines from LINPACK for solving full
C            systems of linear algebraic equations.
C  DGBFA and DGBSL   are routines from LINPACK for solving banded
C            linear systems.
C  DAXPY, DSCAL, and DCOPY are basic linear algebra modules (BLAS).
C  D1MACH    sets the unit roundoff of the machine.
C  XERRWD, XSETUN, XSETF, LUNSAV, and MFLGSV handle the printing of all
C            error messages and warnings.  XERRWD is machine-dependent.
C Note..  DVNORM, D1MACH, LUNSAV, and MFLGSV are function routines.
C All the others are subroutines.
C
C The intrinsic and external routines used by the DVODE package are..
C ABS, MAX, MIN, REAL, SIGN, SQRT, and WRITE.
C
C-----------------------------------------------------------------------
C
C Type declarations for labeled COMMON block DVOD01 --------------------
C
      DOUBLE PRECISION ACNRM, CCMXJ, CONP, CRATE, DRC, EL,
     1     ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2     RC, RL1, TAU, TQ, TN, UROUND
      INTEGER ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     1        L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     2        LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     3        N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     4        NSLP, NYH
C
C Type declarations for labeled COMMON block DVOD02 --------------------
C
      DOUBLE PRECISION HU
      INTEGER NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
C
C Type declarations for local variables --------------------------------
C
      EXTERNAL DVNLSD
      LOGICAL IHIT
      DOUBLE PRECISION ATOLI, BIG, EWTI, FOUR, H0, HMAX, HMX, HUN, ONE,
     1   PT2, RH, RTOLI, SIZE, TCRIT, TNEXT, TOLSF, TP, TWO, ZERO
      INTEGER I, IER, IFLAG, IMXER, JCO, KGO, LENIW, LENJ, LENP, LENRW,
     1   LENWM, LF0, MBAND, ML, MORD, MU, MXHNL0, MXSTP0, NITER, NSLAST
      CHARACTER*80 MSG
C
C Type declaration for function subroutines called ---------------------
C
      DOUBLE PRECISION D1MACH, DVNORM
C
      DIMENSION MORD(2)
C-----------------------------------------------------------------------
C The following Fortran-77 declaration is to cause the values of the
C listed (local) variables to be saved between calls to DVODE.
C-----------------------------------------------------------------------
      SAVE MORD, MXHNL0, MXSTP0
      SAVE ZERO, ONE, TWO, FOUR, PT2, HUN
C-----------------------------------------------------------------------
C The following internal COMMON blocks contain variables which are
C communicated between subroutines in the DVODE package, or which are
C to be saved between calls to DVODE.
C In each block, real variables precede integers.
C The block /DVOD01/ appears in subroutines DVODE, DVINDY, DVSTEP,
C DVSET, DVNLSD, DVJAC, DVSOL, DVJUST and DVSRCO.
C The block /DVOD02/ appears in subroutines DVODE, DVINDY, DVSTEP,
C DVNLSD, DVJAC, and DVSRCO.
C
C The variables stored in the internal COMMON blocks are as follows..
C
C ACNRM  = Weighted r.m.s. norm of accumulated correction vectors.
C CCMXJ  = Threshhold on DRC for updating the Jacobian. (See DRC.)
C CONP   = The saved value of TQ(5).
C CRATE  = Estimated corrector convergence rate constant.
C DRC    = Relative change in H*RL1 since last DVJAC call.
C EL     = Real array of integration coefficients.  See DVSET.
C ETA    = Saved tentative ratio of new to old H.
C ETAMAX = Saved maximum value of ETA to be allowed.
C H      = The step size.
C HMIN   = The minimum absolute value of the step size H to be used.
C HMXI   = Inverse of the maximum absolute value of H to be used.
C          HMXI = 0.0 is allowed and corresponds to an infinite HMAX.
C HNEW   = The step size to be attempted on the next step.
C HSCAL  = Stepsize in scaling of YH array.
C PRL1   = The saved value of RL1.
C RC     = Ratio of current H*RL1 to value on last DVJAC call.
C RL1    = The reciprocal of the coefficient EL(1).
C TAU    = Real vector of past NQ step sizes, length 13.
C TQ     = A real vector of length 5 in which DVSET stores constants
C          used for the convergence test, the error test, and the
C          selection of H at a new order.
C TN     = The independent variable, updated on each step taken.
C UROUND = The machine unit roundoff.  The smallest positive real number
C          such that  1.0 + UROUND .ne. 1.0
C ICF    = Integer flag for convergence failure in DVNLSD..
C            0 means no failures.
C            1 means convergence failure with out of date Jacobian
C                   (recoverable error).
C            2 means convergence failure with current Jacobian or
C                   singular matrix (unrecoverable error).
C INIT   = Saved integer flag indicating whether initialization of the
C          problem has been done (INIT = 1) or not.
C IPUP   = Saved flag to signal updating of Newton matrix.
C JCUR   = Output flag from DVJAC showing Jacobian status..
C            JCUR = 0 means J is not current.
C            JCUR = 1 means J is current.
C JSTART = Integer flag used as input to DVSTEP..
C            0  means perform the first step.
C            1  means take a new step continuing from the last.
C            -1 means take the next step with a new value of MAXORD,
C                  HMIN, HMXI, N, METH, MITER, and/or matrix parameters.
C          On return, DVSTEP sets JSTART = 1.
C JSV    = Integer flag for Jacobian saving, = sign(MF).
C KFLAG  = A completion code from DVSTEP with the following meanings..
C               0      the step was succesful.
C              -1      the requested error could not be achieved.
C              -2      corrector convergence could not be achieved.
C              -3, -4  fatal error in VNLS (can not occur here).
C KUTH   = Input flag to DVSTEP showing whether H was reduced by the
C          driver.  KUTH = 1 if H was reduced, = 0 otherwise.
C L      = Integer variable, NQ + 1, current order plus one.
C LMAX   = MAXORD + 1 (used for dimensioning).
C LOCJS  = A pointer to the saved Jacobian, whose storage starts at
C          WM(LOCJS), if JSV = 1.
C LYH, LEWT, LACOR, LSAVF, LWM, LIWM = Saved integer pointers
C          to segments of RWORK and IWORK.
C MAXORD = The maximum order of integration method to be allowed.
C METH/MITER = The method flags.  See MF.
C MSBJ   = The maximum number of steps between J evaluations, = 50.
C MXHNIL = Saved value of optional input MXHNIL.
C MXSTEP = Saved value of optional input MXSTEP.
C N      = The number of first-order ODEs, = NEQ.
C NEWH   = Saved integer to flag change of H.
C NEWQ   = The method order to be used on the next step.
C NHNIL  = Saved counter for occurrences of T + H = T.
C NQ     = Integer variable, the current integration method order.
C NQNYH  = Saved value of NQ*NYH.
C NQWAIT = A counter controlling the frequency of order changes.
C          An order change is about to be considered if NQWAIT = 1.
C NSLJ   = The number of steps taken as of the last Jacobian update.
C NSLP   = Saved value of NST as of last Newton matrix update.
C NYH    = Saved value of the initial value of NEQ.
C HU     = The step size in t last used.
C NCFN   = Number of nonlinear convergence failures so far.
C NETF   = The number of error test failures of the integrator so far.
C NFE    = The number of f evaluations for the problem so far.
C NJE    = The number of Jacobian evaluations so far.
C NLU    = The number of matrix LU decompositions so far.
C NNI    = Number of nonlinear iterations so far.
C NQU    = The method order last used.
C NST    = The number of steps taken for the problem so far.
C-----------------------------------------------------------------------
      COMMON /DVOD01/ ACNRM, CCMXJ, CONP, CRATE, DRC, EL(13),
     1                ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2                RC, RL1, TAU(13), TQ(5), TN, UROUND,
     3                ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     4                L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     5                LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     6                N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     7                NSLP, NYH
      COMMON /DVOD02/ HU, NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
C
      DATA  MORD(1) /12/, MORD(2) /5/, MXSTP0 /500/, MXHNL0 /10/
      DATA ZERO /0.0D0/, ONE /1.0D0/, TWO /2.0D0/, FOUR /4.0D0/,
     1     PT2 /0.2D0/, HUN /100.0D0/
C-----------------------------------------------------------------------
C Block A.
C This code block is executed on every call.
C It tests ISTATE and ITASK for legality and branches appropriately.
C If ISTATE .gt. 1 but the flag INIT shows that initialization has
C not yet been done, an error return occurs.
C If ISTATE = 1 and TOUT = T, return immediately.
C-----------------------------------------------------------------------
      IF (ISTATE .LT. 1 .OR. ISTATE .GT. 3) GO TO 601
      IF (ITASK .LT. 1 .OR. ITASK .GT. 5) GO TO 602
      IF (ISTATE .EQ. 1) GO TO 10
      IF (INIT .NE. 1) GO TO 603
      IF (ISTATE .EQ. 2) GO TO 200
      GO TO 20
 10   INIT = 0
      IF (TOUT .EQ. T) RETURN
C-----------------------------------------------------------------------
C Block B.
C The next code block is executed for the initial call (ISTATE = 1),
C or for a continuation call with parameter changes (ISTATE = 3).
C It contains checking of all input and various initializations.
C
C First check legality of the non-optional input NEQ, ITOL, IOPT,
C MF, ML, and MU.
C-----------------------------------------------------------------------
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
C Next process and check the optional input. ---------------------------
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
C-----------------------------------------------------------------------
C Set work array pointers and check lengths LRW and LIW.
C Pointers to segments of RWORK and IWORK are named by prefixing L to
C the name of the segment.  E.g., the segment YH starts at RWORK(LYH).
C Segments of RWORK (in order) are denoted  YH, WM, EWT, SAVF, ACOR.
C Within WM, LOCJS is the location of the saved Jacobian (JSV .gt. 0).
C-----------------------------------------------------------------------
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
C Check RTOL and ATOL for legality. ------------------------------------
      RTOLI = RTOL(1)
      ATOLI = ATOL(1)
      DO 70 I = 1,N
        IF (ITOL .GE. 3) RTOLI = RTOL(I)
        IF (ITOL .EQ. 2 .OR. ITOL .EQ. 4) ATOLI = ATOL(I)
        IF (RTOLI .LT. ZERO) GO TO 619
        IF (ATOLI .LT. ZERO) GO TO 620
 70     CONTINUE
      IF (ISTATE .EQ. 1) GO TO 100
C If ISTATE = 3, set flag to signal parameter changes to DVSTEP. -------
      JSTART = -1
      IF (NQ .LE. MAXORD) GO TO 90
C MAXORD was reduced below NQ.  Copy YH(*,MAXORD+2) into SAVF. ---------
      CALL DCOPY (N, RWORK(LWM), 1, RWORK(LSAVF), 1)
C Reload WM(1) = RWORK(LWM), since LWM may have changed. ---------------
 90   IF (MITER .GT. 0) RWORK(LWM) = SQRT(UROUND)
C-----------------------------------------------------------------------
C Block C.
C The next block is for the initial call only (ISTATE = 1).
C It contains all remaining initializations, the initial call to F,
C and the calculation of the initial step size.
C The error weights in EWT are inverted after being loaded.
C-----------------------------------------------------------------------
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
C Initial call to F.  (LF0 points to YH(*,2).) -------------------------
      LF0 = LYH + NYH
      CALL F (N, T, Y, RWORK(LF0), RPAR, IPAR)
      NFE = 1
C Load the initial value vector in YH. ---------------------------------
      CALL DCOPY (N, Y, 1, RWORK(LYH), 1)
C Load and invert the EWT array.  (H is temporarily set to 1.0.) -------
      NQ = 1
      H = ONE
      CALL DEWSET (N, ITOL, RTOL, ATOL, RWORK(LYH), RWORK(LEWT))
      DO 120 I = 1,N
        IF (RWORK(I+LEWT-1) .LE. ZERO) GO TO 621
 120    RWORK(I+LEWT-1) = ONE/RWORK(I+LEWT-1)
      IF (H0 .NE. ZERO) GO TO 180
C Call DVHIN to set initial step size H0 to be attempted. --------------
      CALL DVHIN (N, T, RWORK(LYH), RWORK(LF0), F, RPAR, IPAR, TOUT,
     1   UROUND, RWORK(LEWT), ITOL, ATOL, Y, RWORK(LACOR), H0,
     2   NITER, IER)
      NFE = NFE + NITER
      IF (IER .NE. 0) GO TO 622
C Adjust H0 if necessary to meet HMAX bound. ---------------------------
 180  RH = ABS(H0)*HMXI
      IF (RH .GT. ONE) H0 = H0/RH
C Load H with H0 and scale YH(*,2) by H0. ------------------------------
      H = H0
      CALL DSCAL (N, H0, RWORK(LF0), 1)
      GO TO 270
C-----------------------------------------------------------------------
C Block D.
C The next code block is for continuation calls only (ISTATE = 2 or 3)
C and is to check stop conditions before taking a step.
C-----------------------------------------------------------------------
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
C-----------------------------------------------------------------------
C Block E.
C The next block is normally executed for all calls and contains
C the call to the one-step core integrator DVSTEP.
C
C This is a looping point for the integration steps.
C
C First check for too many steps being taken, update EWT (if not at
C start of problem), check for too much accuracy being requested, and
C check for H below the roundoff level in T.
C-----------------------------------------------------------------------
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
C-----------------------------------------------------------------------
C CALL DVSTEP (Y, YH, NYH, YH, EWT, SAVF, VSAV, ACOR,
C              WM, IWM, F, JAC, F, DVNLSD, RPAR, IPAR)
C-----------------------------------------------------------------------
      CALL DVSTEP (Y, RWORK(LYH), NYH, RWORK(LYH), RWORK(LEWT),
     1   RWORK(LSAVF), Y, RWORK(LACOR), RWORK(LWM), IWORK(LIWM),
     2   F, JAC, F, DVNLSD, RPAR, IPAR)
      KGO = 1 - KFLAG
C Branch on KFLAG.  Note..In this version, KFLAG can not be set to -3.
C  KFLAG .eq. 0,   -1,  -2
      GO TO (300, 530, 540), KGO
C-----------------------------------------------------------------------
C Block F.
C The following block handles the case of a successful return from the
C core integrator (KFLAG = 0).  Test for stop conditions.
C-----------------------------------------------------------------------
 300  INIT = 1
      KUTH = 0
      GO TO (310, 400, 330, 340, 350), ITASK
C ITASK = 1.  If TOUT has been reached, interpolate. -------------------
 310  IF ((TN - TOUT)*H .LT. ZERO) GO TO 250
      CALL DVINDY (TOUT, 0, RWORK(LYH), NYH, Y, IFLAG)
      T = TOUT
      GO TO 420
C ITASK = 3.  Jump to exit if TOUT was reached. ------------------------
 330  IF ((TN - TOUT)*H .GE. ZERO) GO TO 400
      GO TO 250
C ITASK = 4.  See if TOUT or TCRIT was reached.  Adjust H if necessary.
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
C ITASK = 5.  See if TCRIT was reached and jump to exit. ---------------
 350  HMX = ABS(TN) + ABS(H)
      IHIT = ABS(TN - TCRIT) .LE. HUN*UROUND*HMX
C-----------------------------------------------------------------------
C Block G.
C The following block handles all successful returns from DVODE.
C If ITASK .ne. 1, Y is loaded from YH and T is set accordingly.
C ISTATE is set to 2, and the optional output is loaded into the work
C arrays before returning.
C-----------------------------------------------------------------------
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
C-----------------------------------------------------------------------
C Block H.
C The following block handles all unsuccessful returns other than
C those for illegal input.  First the error message routine is called.
C if there was an error test or convergence test failure, IMXER is set.
C Then Y is loaded from YH, T is set to TN, and the illegal input
C The optional output is loaded into the work arrays before returning.
C-----------------------------------------------------------------------
C The maximum number of steps was taken before reaching TOUT. ----------
 500  MSG = 'DVODE--  At current T (=R1), MXSTEP (=I1) steps   '
      CALL XERRWD (MSG, 50, 201, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG = '      taken on this call before reaching TOUT     '
      CALL XERRWD (MSG, 50, 201, 1, 1, MXSTEP, 0, 1, TN, ZERO)
      ISTATE = -1
      GO TO 580
C EWT(i) .le. 0.0 for some i (not at start of problem). ----------------
 510  EWTI = RWORK(LEWT+I-1)
      MSG = 'DVODE--  At T (=R1), EWT(I1) has become R2 .le. 0.'
      CALL XERRWD (MSG, 50, 202, 1, 1, I, 0, 2, TN, EWTI)
      ISTATE = -6
      GO TO 580
C Too much accuracy requested for machine precision. -------------------
 520  MSG = 'DVODE--  At T (=R1), too much accuracy requested  '
      CALL XERRWD (MSG, 50, 203, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG = '      for precision of machine..  see TOLSF (=R2) '
      CALL XERRWD (MSG, 50, 203, 1, 0, 0, 0, 2, TN, TOLSF)
      RWORK(14) = TOLSF
      ISTATE = -2
      GO TO 580
C KFLAG = -1.  Error test failed repeatedly or with ABS(H) = HMIN. -----
 530  MSG = 'DVODE--  At T(=R1) and step size H(=R2), the error'
      CALL XERRWD (MSG, 50, 204, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG = '      test failed repeatedly or with abs(H) = HMIN'
      CALL XERRWD (MSG, 50, 204, 1, 0, 0, 0, 2, TN, H)
      ISTATE = -4
      GO TO 560
C KFLAG = -2.  Convergence failed repeatedly or with abs(H) = HMIN. ----
 540  MSG = 'DVODE--  At T (=R1) and step size H (=R2), the    '
      CALL XERRWD (MSG, 50, 205, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG = '      corrector convergence failed repeatedly     '
      CALL XERRWD (MSG, 50, 205, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG = '      or with abs(H) = HMIN   '
      CALL XERRWD (MSG, 30, 205, 1, 0, 0, 0, 2, TN, H)
      ISTATE = -5
C Compute IMXER if relevant. -------------------------------------------
 560  BIG = ZERO
      IMXER = 1
      DO 570 I = 1,N
        SIZE = ABS(RWORK(I+LACOR-1)*RWORK(I+LEWT-1))
        IF (BIG .GE. SIZE) GO TO 570
        BIG = SIZE
        IMXER = I
 570    CONTINUE
      IWORK(16) = IMXER
C Set Y vector, T, and optional output. --------------------------------
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
C-----------------------------------------------------------------------
C Block I.
C The following block handles all error returns due to illegal input
C (ISTATE = -3), as detected before calling the core integrator.
C First the error message routine is called.   If the illegal input
C is a negative ISTATE, the run is aborted (apparent infinite loop).
C-----------------------------------------------------------------------
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
 619  MSG = 'DVODE--  RTOL(I1) is R1 .lt. 0.0        '
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
C
 700  CONTINUE
      ISTATE = -3
      RETURN
C
 800  MSG = 'DVODE--  Run aborted.. apparent infinite loop     '
      CALL XERRWD (MSG, 50, 303, 2, 0, 0, 0, 0, ZERO, ZERO)
      RETURN
C----------------------- End of Subroutine DVODE -----------------------
      END
*DECK DVHIN
      SUBROUTINE DVHIN (N, T0, Y0, YDOT, F, RPAR, IPAR, TOUT, UROUND,
     1   EWT, ITOL, ATOL, Y, TEMP, H0, NITER, IER)
      EXTERNAL F
      DOUBLE PRECISION T0, Y0, YDOT, RPAR, TOUT, UROUND, EWT, ATOL, Y,
     1   TEMP, H0
      INTEGER N, IPAR, ITOL, NITER, IER
      DIMENSION Y0(*), YDOT(*), EWT(*), ATOL(*), Y(*),
     1   TEMP(*), RPAR(*), IPAR(*)
C-----------------------------------------------------------------------
C Call sequence input -- N, T0, Y0, YDOT, F, RPAR, IPAR, TOUT, UROUND,
C                        EWT, ITOL, ATOL, Y, TEMP
C Call sequence output -- H0, NITER, IER
C COMMON block variables accessed -- None
C
C Subroutines called by DVHIN.. F
C Function routines called by DVHIN.. DVNORM
C-----------------------------------------------------------------------
C This routine computes the step size, H0, to be attempted on the
C first step, when the user has not supplied a value for this.
C
C First we check that TOUT - T0 differs significantly from zero.  Then
C an iteration is done to approximate the initial second derivative
C and this is used to define h from w.r.m.s.norm(h**2 * yddot / 2) = 1.
C A bias factor of 1/2 is applied to the resulting h.
C The sign of H0 is inferred from the initial values of TOUT and T0.
C
C Communication with DVHIN is done with the following variables..
C
C N      = Size of ODE system, input.
C T0     = Initial value of independent variable, input.
C Y0     = Vector of initial conditions, input.
C YDOT   = Vector of initial first derivatives, input.
C F      = Name of subroutine for right-hand side f(t,y), input.
C RPAR, IPAR = Dummy names for user's real and integer work arrays.
C TOUT   = First output value of independent variable
C UROUND = Machine unit roundoff
C EWT, ITOL, ATOL = Error weights and tolerance parameters
C                   as described in the driver routine, input.
C Y, TEMP = Work arrays of length N.
C H0     = Step size to be attempted, output.
C NITER  = Number of iterations (and of f evaluations) to compute H0,
C          output.
C IER    = The error flag, returned with the value
C          IER = 0  if no trouble occurred, or
C          IER = -1 if TOUT and T0 are considered too close to proceed.
C-----------------------------------------------------------------------
C
C Type declarations for local variables --------------------------------
C
      DOUBLE PRECISION AFI, ATOLI, DELYI, HALF, HG, HLB, HNEW, HRAT,
     1     HUB, HUN, PT1, T1, TDIST, TROUND, TWO, YDDNRM
      INTEGER I, ITER
C
C Type declaration for function subroutines called ---------------------
C
      DOUBLE PRECISION DVNORM
C-----------------------------------------------------------------------
C The following Fortran-77 declaration is to cause the values of the
C listed (local) variables to be saved between calls to this integrator.
C-----------------------------------------------------------------------
      SAVE HALF, HUN, PT1, TWO
      DATA HALF /0.5D0/, HUN /100.0D0/, PT1 /0.1D0/, TWO /2.0D0/
C
      NITER = 0
      TDIST = ABS(TOUT - T0)
      TROUND = UROUND*MAX(ABS(T0),ABS(TOUT))
      IF (TDIST .LT. TWO*TROUND) GO TO 100
C
C Set a lower bound on h based on the roundoff level in T0 and TOUT. ---
      HLB = HUN*TROUND
C Set an upper bound on h based on TOUT-T0 and the initial Y and YDOT. -
      HUB = PT1*TDIST
      ATOLI = ATOL(1)
      DO 10 I = 1, N
        IF (ITOL .EQ. 2 .OR. ITOL .EQ. 4) ATOLI = ATOL(I)
        DELYI = PT1*ABS(Y0(I)) + ATOLI
        AFI = ABS(YDOT(I))
        IF (AFI*HUB .GT. DELYI) HUB = DELYI/AFI
 10     CONTINUE
C
C Set initial guess for h as geometric mean of upper and lower bounds. -
      ITER = 0
      HG = SQRT(HLB*HUB)
C If the bounds have crossed, exit with the mean value. ----------------
      IF (HUB .LT. HLB) THEN
        H0 = HG
        GO TO 90
      ENDIF
C
C Looping point for iteration. -----------------------------------------
 50   CONTINUE
C Estimate the second derivative as a difference quotient in f. --------
      T1 = T0 + HG
      DO 60 I = 1, N
 60     Y(I) = Y0(I) + HG*YDOT(I)
      CALL F (N, T1, Y, TEMP, RPAR, IPAR)
      DO 70 I = 1, N
 70     TEMP(I) = (TEMP(I) - YDOT(I))/HG
      YDDNRM = DVNORM (N, TEMP, EWT)
C Get the corresponding new value of h. --------------------------------
      IF (YDDNRM*HUB*HUB .GT. TWO) THEN
        HNEW = SQRT(TWO/YDDNRM)
      ELSE
        HNEW = SQRT(HG*HUB)
      ENDIF
      ITER = ITER + 1
C-----------------------------------------------------------------------
C Test the stopping conditions.
C Stop if the new and previous h values differ by a factor of .lt. 2.
C Stop if four iterations have been done.  Also, stop with previous h
C if HNEW/HG .gt. 2 after first iteration, as this probably means that
C the second derivative value is bad because of cancellation error.
C-----------------------------------------------------------------------
      IF (ITER .GE. 4) GO TO 80
      HRAT = HNEW/HG
      IF ( (HRAT .GT. HALF) .AND. (HRAT .LT. TWO) ) GO TO 80
      IF ( (ITER .GE. 2) .AND. (HNEW .GT. TWO*HG) ) THEN
        HNEW = HG
        GO TO 80
      ENDIF
      HG = HNEW
      GO TO 50
C
C Iteration done.  Apply bounds, bias factor, and sign.  Then exit. ----
 80   H0 = HNEW*HALF
      IF (H0 .LT. HLB) H0 = HLB
      IF (H0 .GT. HUB) H0 = HUB
 90   H0 = SIGN(H0, TOUT - T0)
      NITER = ITER
      IER = 0
      RETURN
C Error return for TOUT - T0 too small. --------------------------------
 100  IER = -1
      RETURN
C----------------------- End of Subroutine DVHIN -----------------------
      END
*DECK DVINDY
      SUBROUTINE DVINDY (T, K, YH, LDYH, DKY, IFLAG)
      DOUBLE PRECISION T, YH, DKY
      INTEGER K, LDYH, IFLAG
      DIMENSION YH(LDYH,*), DKY(*)
C-----------------------------------------------------------------------
C Call sequence input -- T, K, YH, LDYH
C Call sequence output -- DKY, IFLAG
C COMMON block variables accessed..
C     /DVOD01/ --  H, TN, UROUND, L, N, NQ
C     /DVOD02/ --  HU
C
C Subroutines called by DVINDY.. DSCAL, XERRWD
C Function routines called by DVINDY.. None
C-----------------------------------------------------------------------
C DVINDY computes interpolated values of the K-th derivative of the
C dependent variable vector y, and stores it in DKY.  This routine
C is called within the package with K = 0 and T = TOUT, but may
C also be called by the user for any K up to the current order.
C (See detailed instructions in the usage documentation.)
C-----------------------------------------------------------------------
C The computed values in DKY are gotten by interpolation using the
C Nordsieck history array YH.  This array corresponds uniquely to a
C vector-valued polynomial of degree NQCUR or less, and DKY is set
C to the K-th derivative of this polynomial at T.
C The formula for DKY is..
C              q
C  DKY(i)  =  sum  c(j,K) * (T - TN)**(j-K) * H**(-j) * YH(i,j+1)
C             j=K
C where  c(j,K) = j*(j-1)*...*(j-K+1), q = NQCUR, TN = TCUR, H = HCUR.
C The quantities  NQ = NQCUR, L = NQ+1, N, TN, and H are
C communicated by COMMON.  The above sum is done in reverse order.
C IFLAG is returned negative if either K or T is out of bounds.
C
C Discussion above and comments in driver explain all variables.
C-----------------------------------------------------------------------
C
C Type declarations for labeled COMMON block DVOD01 --------------------
C
      DOUBLE PRECISION ACNRM, CCMXJ, CONP, CRATE, DRC, EL,
     1     ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2     RC, RL1, TAU, TQ, TN, UROUND
      INTEGER ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     1        L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     2        LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     3        N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     4        NSLP, NYH
C
C Type declarations for labeled COMMON block DVOD02 --------------------
C
      DOUBLE PRECISION HU
      INTEGER NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
C
C Type declarations for local variables --------------------------------
C
      DOUBLE PRECISION C, HUN, R, S, TFUZZ, TN1, TP, ZERO
      INTEGER I, IC, J, JB, JB2, JJ, JJ1, JP1
      CHARACTER*80 MSG
C-----------------------------------------------------------------------
C The following Fortran-77 declaration is to cause the values of the
C listed (local) variables to be saved between calls to this integrator.
C-----------------------------------------------------------------------
      SAVE HUN, ZERO
C
      COMMON /DVOD01/ ACNRM, CCMXJ, CONP, CRATE, DRC, EL(13),
     1                ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2                RC, RL1, TAU(13), TQ(5), TN, UROUND,
     3                ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     4                L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     5                LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     6                N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     7                NSLP, NYH
      COMMON /DVOD02/ HU, NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
C
      DATA HUN /100.0D0/, ZERO /0.0D0/
C
      IFLAG = 0
      IF (K .LT. 0 .OR. K .GT. NQ) GO TO 80
      TFUZZ = HUN*UROUND*(TN + HU)
      TP = TN - HU - TFUZZ
      TN1 = TN + TFUZZ
      IF ((T-TP)*(T-TN1) .GT. ZERO) GO TO 90
C
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
C
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
C----------------------- End of Subroutine DVINDY ----------------------
      END
*DECK DVSTEP
      SUBROUTINE DVSTEP (Y, YH, LDYH, YH1, EWT, SAVF, VSAV, ACOR,
     1                  WM, IWM, F, JAC, PSOL, VNLS, RPAR, IPAR)
      EXTERNAL F, JAC, PSOL, VNLS
      DOUBLE PRECISION Y, YH, YH1, EWT, SAVF, VSAV, ACOR, WM, RPAR
      INTEGER LDYH, IWM, IPAR
      DIMENSION Y(*), YH(LDYH,*), YH1(*), EWT(*), SAVF(*), VSAV(*),
     1   ACOR(*), WM(*), IWM(*), RPAR(*), IPAR(*)
C-----------------------------------------------------------------------
C Call sequence input -- Y, YH, LDYH, YH1, EWT, SAVF, VSAV,
C                        ACOR, WM, IWM, F, JAC, PSOL, VNLS, RPAR, IPAR
C Call sequence output -- YH, ACOR, WM, IWM
C COMMON block variables accessed..
C     /DVOD01/  ACNRM, EL(13), H, HMIN, HMXI, HNEW, HSCAL, RC, TAU(13),
C               TQ(5), TN, JCUR, JSTART, KFLAG, KUTH,
C               L, LMAX, MAXORD, MITER, N, NEWQ, NQ, NQWAIT
C     /DVOD02/  HU, NCFN, NETF, NFE, NQU, NST
C
C Subroutines called by DVSTEP.. F, DAXPY, DCOPY, DSCAL,
C                               DVJUST, VNLS, DVSET
C Function routines called by DVSTEP.. DVNORM
C-----------------------------------------------------------------------
C DVSTEP performs one step of the integration of an initial value
C problem for a system of ordinary differential equations.
C DVSTEP calls subroutine VNLS for the solution of the nonlinear system
C arising in the time step.  Thus it is independent of the problem
C Jacobian structure and the type of nonlinear system solution method.
C DVSTEP returns a completion flag KFLAG (in COMMON).
C A return with KFLAG = -1 or -2 means either ABS(H) = HMIN or 10
C consecutive failures occurred.  On a return with KFLAG negative,
C the values of TN and the YH array are as of the beginning of the last
C step, and H is the last step size attempted.
C
C Communication with DVSTEP is done with the following variables..
C
C Y      = An array of length N used for the dependent variable vector.
C YH     = An LDYH by LMAX array containing the dependent variables
C          and their approximate scaled derivatives, where
C          LMAX = MAXORD + 1.  YH(i,j+1) contains the approximate
C          j-th derivative of y(i), scaled by H**j/factorial(j)
C          (j = 0,1,...,NQ).  On entry for the first step, the first
C          two columns of YH must be set from the initial values.
C LDYH   = A constant integer .ge. N, the first dimension of YH.
C          N is the number of ODEs in the system.
C YH1    = A one-dimensional array occupying the same space as YH.
C EWT    = An array of length N containing multiplicative weights
C          for local error measurements.  Local errors in y(i) are
C          compared to 1.0/EWT(i) in various error tests.
C SAVF   = An array of working storage, of length N.
C          also used for input of YH(*,MAXORD+2) when JSTART = -1
C          and MAXORD .lt. the current order NQ.
C VSAV   = A work array of length N passed to subroutine VNLS.
C ACOR   = A work array of length N, used for the accumulated
C          corrections.  On a successful return, ACOR(i) contains
C          the estimated one-step local error in y(i).
C WM,IWM = Real and integer work arrays associated with matrix
C          operations in VNLS.
C F      = Dummy name for the user supplied subroutine for f.
C JAC    = Dummy name for the user supplied Jacobian subroutine.
C PSOL   = Dummy name for the subroutine passed to VNLS, for
C          possible use there.
C VNLS   = Dummy name for the nonlinear system solving subroutine,
C          whose real name is dependent on the method used.
C RPAR, IPAR = Dummy names for user's real and integer work arrays.
C-----------------------------------------------------------------------
C
C Type declarations for labeled COMMON block DVOD01 --------------------
C
      DOUBLE PRECISION ACNRM, CCMXJ, CONP, CRATE, DRC, EL,
     1     ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2     RC, RL1, TAU, TQ, TN, UROUND
      INTEGER ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     1        L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     2        LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     3        N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     4        NSLP, NYH
C
C Type declarations for labeled COMMON block DVOD02 --------------------
C
      DOUBLE PRECISION HU
      INTEGER NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
C
C Type declarations for local variables --------------------------------
C
      DOUBLE PRECISION ADDON, BIAS1,BIAS2,BIAS3, CNQUOT, DDN, DSM, DUP,
     1     ETACF, ETAMIN, ETAMX1, ETAMX2, ETAMX3, ETAMXF,
     2     ETAQ, ETAQM1, ETAQP1, FLOTL, ONE, ONEPSM,
     3     R, THRESH, TOLD, ZERO
      INTEGER I, I1, I2, IBACK, J, JB, KFC, KFH, MXNCF, NCF, NFLAG
C
C Type declaration for function subroutines called ---------------------
C
      DOUBLE PRECISION DVNORM
C-----------------------------------------------------------------------
C The following Fortran-77 declaration is to cause the values of the
C listed (local) variables to be saved between calls to this integrator.
C-----------------------------------------------------------------------
      SAVE ADDON, BIAS1, BIAS2, BIAS3,
     1     ETACF, ETAMIN, ETAMX1, ETAMX2, ETAMX3, ETAMXF,
     2     KFC, KFH, MXNCF, ONEPSM, THRESH, ONE, ZERO
C-----------------------------------------------------------------------
      COMMON /DVOD01/ ACNRM, CCMXJ, CONP, CRATE, DRC, EL(13),
     1                ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2                RC, RL1, TAU(13), TQ(5), TN, UROUND,
     3                ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     4                L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     5                LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     6                N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     7                NSLP, NYH
      COMMON /DVOD02/ HU, NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
C
      DATA KFC/-3/, KFH/-7/, MXNCF/10/
      DATA ADDON  /1.0D-6/,    BIAS1  /6.0D0/,     BIAS2  /6.0D0/,
     1     BIAS3  /10.0D0/,    ETACF  /0.25D0/,    ETAMIN /0.1D0/,
     2     ETAMXF /0.2D0/,     ETAMX1 /1.0D4/,     ETAMX2 /10.0D0/,
     3     ETAMX3 /10.0D0/,    ONEPSM /1.00001D0/, THRESH /1.5D0/
      DATA ONE/1.0D0/, ZERO/0.0D0/
C
      KFLAG = 0
      TOLD = TN
      NCF = 0
      JCUR = 0
      NFLAG = 0
      IF (JSTART .GT. 0) GO TO 20
      IF (JSTART .EQ. -1) GO TO 100
C-----------------------------------------------------------------------
C On the first call, the order is set to 1, and other variables are
C initialized.  ETAMAX is the maximum ratio by which H can be increased
C in a single step.  It is normally 1.5, but is larger during the
C first 10 steps to compensate for the small initial H.  If a failure
C occurs (in corrector convergence or error test), ETAMAX is set to 1
C for the next increase.
C-----------------------------------------------------------------------
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
C-----------------------------------------------------------------------
C Take preliminary actions on a normal continuation step (JSTART.GT.0).
C If the driver changed H, then ETA must be reset and NEWH set to 1.
C If a change of order was dictated on the previous step, then
C it is done here and appropriate adjustments in the history are made.
C On an order decrease, the history array is adjusted by DVJUST.
C On an order increase, the history array is augmented by a column.
C On a change of step size H, the history array YH is rescaled.
C-----------------------------------------------------------------------
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
C-----------------------------------------------------------------------
C The following block handles preliminaries needed when JSTART = -1.
C If N was reduced, zero out part of YH to avoid undefined references.
C If MAXORD was reduced to a value less than the tentative order NEWQ,
C then NQ is set to MAXORD, and a new H ratio ETA is chosen.
C Otherwise, we take the same preliminary actions as for JSTART .gt. 0.
C In any case, NQWAIT is reset to L = NQ + 1 to prevent further
C changes in order for that many steps.
C The new H ratio ETA is limited by the input H if KUTH = 1,
C by HMIN if KUTH = 0, and by HMXI in any case.
C Finally, the history array YH is rescaled.
C-----------------------------------------------------------------------
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
C Rescale the history array for a change in H by a factor of ETA. ------
 150  R = ONE
      DO 180 J = 2, L
        R = R*ETA
        CALL DSCAL (N, R, YH(1,J), 1 )
 180    CONTINUE
      H = HSCAL*ETA
      HSCAL = H
      RC = RC*ETA
      NQNYH = NQ*LDYH
C-----------------------------------------------------------------------
C This section computes the predicted values by effectively
C multiplying the YH array by the Pascal triangle matrix.
C DVSET is called to calculate all integration coefficients.
C RC is the ratio of new to old values of the coefficient H/EL(2)=h/l1.
C-----------------------------------------------------------------------
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
C
C Call the nonlinear system solver. ------------------------------------
C
      CALL VNLS (Y, YH, LDYH, VSAV, SAVF, EWT, ACOR, IWM, WM,
     1           F, JAC, PSOL, NFLAG, RPAR, IPAR)
C
      IF (NFLAG .EQ. 0) GO TO 450
C-----------------------------------------------------------------------
C The VNLS routine failed to achieve convergence (NFLAG .NE. 0).
C The YH array is retracted to its values before prediction.
C The step size H is reduced and the step is retried, if possible.
C Otherwise, an error exit is taken.
C-----------------------------------------------------------------------
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
C-----------------------------------------------------------------------
C The corrector has converged (NFLAG = 0).  The local error test is
C made and control passes to statement 500 if it fails.
C-----------------------------------------------------------------------
 450  CONTINUE
      DSM = ACNRM/TQ(2)
      IF (DSM .GT. ONE) GO TO 500
C-----------------------------------------------------------------------
C After a successful step, update the YH and TAU arrays and decrement
C NQWAIT.  If NQWAIT is then 1 and NQ .lt. MAXORD, then ACOR is saved
C for use in a possible order increase on the next step.
C If ETAMAX = 1 (a failure occurred this step), keep NQWAIT .ge. 2.
C-----------------------------------------------------------------------
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
C-----------------------------------------------------------------------
C The error test failed.  KFLAG keeps track of multiple failures.
C Restore TN and the YH array to their previous values, and prepare
C to try the step again.  Compute the optimum step size for the
C same order.  After repeated failures, H is forced to decrease
C more rapidly.
C-----------------------------------------------------------------------
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
C Compute ratio of new H to current H at the current order. ------------
      FLOTL = REAL(L)
      ETA = ONE/((BIAS2*DSM)**(ONE/FLOTL) + ADDON)
      ETA = MAX(ETA,HMIN/ABS(H),ETAMIN)
      IF ((KFLAG .LE. -2) .AND. (ETA .GT. ETAMXF)) ETA = ETAMXF
      GO TO 150
C-----------------------------------------------------------------------
C Control reaches this section if 3 or more consecutive failures
C have occurred.  It is assumed that the elements of the YH array
C have accumulated errors of the wrong order.  The order is reduced
C by one, if possible.  Then H is reduced by a factor of 0.1 and
C the step is retried.  After a total of 7 consecutive failures,
C an exit is taken with KFLAG = -1.
C-----------------------------------------------------------------------
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
C-----------------------------------------------------------------------
C If NQWAIT = 0, an increase or decrease in order by one is considered.
C Factors ETAQ, ETAQM1, ETAQP1 are computed by which H could
C be multiplied at order q, q-1, or q+1, respectively.
C The largest of these is determined, and the new order and
C step size set accordingly.
C A change of H or NQ is made only if H increases by at least a
C factor of THRESH.  If an order change is considered and rejected,
C then NQWAIT is set to 2 (reconsider it after 2 steps).
C-----------------------------------------------------------------------
C Compute ratio of new H to current H at the current order. ------------
 560  FLOTL = REAL(L)
      ETAQ = ONE/((BIAS2*DSM)**(ONE/FLOTL) + ADDON)
      IF (NQWAIT .NE. 0) GO TO 600
      NQWAIT = 2
      ETAQM1 = ZERO
      IF (NQ .EQ. 1) GO TO 570
C Compute ratio of new H to current H at the current order less one. ---
      DDN = DVNORM (N, YH(1,L), EWT)/TQ(1)
      ETAQM1 = ONE/((BIAS1*DDN)**(ONE/(FLOTL - ONE)) + ADDON)
 570  ETAQP1 = ZERO
      IF (L .EQ. LMAX) GO TO 580
C Compute ratio of new H to current H at current order plus one. -------
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
C Test tentative new H against THRESH, ETAMAX, and HMXI, then exit. ----
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
C-----------------------------------------------------------------------
C All returns are made through this section.
C On a successful return, ETAMAX is reset and ACOR is scaled.
C-----------------------------------------------------------------------
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
C----------------------- End of Subroutine DVSTEP ----------------------
      END
*DECK DVSET
      SUBROUTINE DVSET
C-----------------------------------------------------------------------
C Call sequence communication.. None
C COMMON block variables accessed..
C     /DVOD01/ -- EL(13), H, TAU(13), TQ(5), L(= NQ + 1),
C                 METH, NQ, NQWAIT
C
C Subroutines called by DVSET.. None
C Function routines called by DVSET.. None
C-----------------------------------------------------------------------
C DVSET is called by DVSTEP and sets coefficients for use there.
C
C For each order NQ, the coefficients in EL are calculated by use of
C  the generating polynomial lambda(x), with coefficients EL(i).
C      lambda(x) = EL(1) + EL(2)*x + ... + EL(NQ+1)*(x**NQ).
C For the backward differentiation formulas,
C                                     NQ-1
C      lambda(x) = (1 + x/xi*(NQ)) * product (1 + x/xi(i) ) .
C                                     i = 1
C For the Adams formulas,
C                              NQ-1
C      (d/dx) lambda(x) = c * product (1 + x/xi(i) ) ,
C                              i = 1
C      lambda(-1) = 0,    lambda(0) = 1,
C where c is a normalization constant.
C In both cases, xi(i) is defined by
C      H*xi(i) = t sub n  -  t sub (n-i)
C              = H + TAU(1) + TAU(2) + ... TAU(i-1).
C
C
C In addition to variables described previously, communication
C with DVSET uses the following..
C   TAU    = A vector of length 13 containing the past NQ values
C            of H.
C   EL     = A vector of length 13 in which vset stores the
C            coefficients for the corrector formula.
C   TQ     = A vector of length 5 in which vset stores constants
C            used for the convergence test, the error test, and the
C            selection of H at a new order.
C   METH   = The basic method indicator.
C   NQ     = The current order.
C   L      = NQ + 1, the length of the vector stored in EL, and
C            the number of columns of the YH array being used.
C   NQWAIT = A counter controlling the frequency of order changes.
C            An order change is about to be considered if NQWAIT = 1.
C-----------------------------------------------------------------------
C
C Type declarations for labeled COMMON block DVOD01 --------------------
C
      DOUBLE PRECISION ACNRM, CCMXJ, CONP, CRATE, DRC, EL,
     1     ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2     RC, RL1, TAU, TQ, TN, UROUND
      INTEGER ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     1        L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     2        LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     3        N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     4        NSLP, NYH
C
C Type declarations for local variables --------------------------------
C
      DOUBLE PRECISION AHATN0, ALPH0, CNQM1, CORTES, CSUM, ELP, EM,
     1     EM0, FLOTI, FLOTL, FLOTNQ, HSUM, ONE, RXI, RXIS, S, SIX,
     2     T1, T2, T3, T4, T5, T6, TWO, XI, ZERO
      INTEGER I, IBACK, J, JP1, NQM1, NQM2
C
      DIMENSION EM(13)
C-----------------------------------------------------------------------
C The following Fortran-77 declaration is to cause the values of the
C listed (local) variables to be saved between calls to this integrator.
C-----------------------------------------------------------------------
      SAVE CORTES, ONE, SIX, TWO, ZERO
C
      COMMON /DVOD01/ ACNRM, CCMXJ, CONP, CRATE, DRC, EL(13),
     1                ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2                RC, RL1, TAU(13), TQ(5), TN, UROUND,
     3                ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     4                L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     5                LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     6                N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     7                NSLP, NYH
C
      DATA CORTES /0.1D0/
      DATA ONE  /1.0D0/, SIX /6.0D0/, TWO /2.0D0/, ZERO /0.0D0/
C
      FLOTL = REAL(L)
      NQM1 = NQ - 1
      NQM2 = NQ - 2
      GO TO (100, 200), METH
C
C Set coefficients for Adams methods. ----------------------------------
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
C Compute integral from -1 to 0 of polynomial and of x times it. -------
      S = ONE
      EM0 = ZERO
      CSUM = ZERO
      DO 160 I = 1, NQ
        FLOTI = REAL(I)
        EM0 = EM0 + S*EM(I)/FLOTI
        CSUM = CSUM + S*EM(I)/(FLOTI+ONE)
 160    S = -S
C In EL, form coefficients of normalized integrated polynomial. --------
      S = ONE/EM0
      EL(1) = ONE
      DO 170 I = 1, NQ
 170    EL(I+1) = S*EM(I)/REAL(I)
      XI = HSUM/H
      TQ(2) = XI*EM0/CSUM
      TQ(5) = XI/EL(L)
      IF (NQWAIT .NE. 1) GO TO 300
C For higher order control constant, multiply polynomial by 1+x/xi(q). -
      RXI = ONE/XI
      DO 180 IBACK = 1, NQ
        I = (L + 1) - IBACK
 180    EM(I) = EM(I) + EM(I-1)*RXI
C Compute integral of polynomial. --------------------------------------
      S = ONE
      CSUM = ZERO
      DO 190 I = 1, L
        CSUM = CSUM + S*EM(I)/REAL(I+1)
 190    S = -S
      TQ(3) = FLOTL*EM0/CSUM
      GO TO 300
C
C Set coefficients for BDF methods. ------------------------------------
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
C In EL, construct coefficients of (1+x/xi(1))*...*(1+x/xi(j+1)). ------
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
C----------------------- End of Subroutine DVSET -----------------------
      END
*DECK DVJUST
      SUBROUTINE DVJUST (YH, LDYH, IORD)
      DOUBLE PRECISION YH
      INTEGER LDYH, IORD
      DIMENSION YH(LDYH,*)
C-----------------------------------------------------------------------
C Call sequence input -- YH, LDYH, IORD
C Call sequence output -- YH
C COMMON block input -- NQ, METH, LMAX, HSCAL, TAU(13), N
C COMMON block variables accessed..
C     /DVOD01/ -- HSCAL, TAU(13), LMAX, METH, N, NQ,
C
C Subroutines called by DVJUST.. DAXPY
C Function routines called by DVJUST.. None
C-----------------------------------------------------------------------
C This subroutine adjusts the YH array on reduction of order,
C and also when the order is increased for the stiff option (METH = 2).
C Communication with DVJUST uses the following..
C IORD  = An integer flag used when METH = 2 to indicate an order
C         increase (IORD = +1) or an order decrease (IORD = -1).
C HSCAL = Step size H used in scaling of Nordsieck array YH.
C         (If IORD = +1, DVJUST assumes that HSCAL = TAU(1).)
C See References 1 and 2 for details.
C-----------------------------------------------------------------------
C
C Type declarations for labeled COMMON block DVOD01 --------------------
C
      DOUBLE PRECISION ACNRM, CCMXJ, CONP, CRATE, DRC, EL,
     1     ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2     RC, RL1, TAU, TQ, TN, UROUND
      INTEGER ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     1        L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     2        LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     3        N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     4        NSLP, NYH
C
C Type declarations for local variables --------------------------------
C
      DOUBLE PRECISION ALPH0, ALPH1, HSUM, ONE, PROD, T1, XI,XIOLD, ZERO
      INTEGER I, IBACK, J, JP1, LP1, NQM1, NQM2, NQP1
C-----------------------------------------------------------------------
C The following Fortran-77 declaration is to cause the values of the
C listed (local) variables to be saved between calls to this integrator.
C-----------------------------------------------------------------------
      SAVE ONE, ZERO
C
      COMMON /DVOD01/ ACNRM, CCMXJ, CONP, CRATE, DRC, EL(13),
     1                ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2                RC, RL1, TAU(13), TQ(5), TN, UROUND,
     3                ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     4                L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     5                LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     6                N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     7                NSLP, NYH
C
      DATA ONE /1.0D0/, ZERO /0.0D0/
C
      IF ((NQ .EQ. 2) .AND. (IORD .NE. 1)) RETURN
      NQM1 = NQ - 1
      NQM2 = NQ - 2
      GO TO (100, 200), METH
C-----------------------------------------------------------------------
C Nonstiff option...
C Check to see if the order is being increased or decreased.
C-----------------------------------------------------------------------
 100  CONTINUE
      IF (IORD .EQ. 1) GO TO 180
C Order decrease. ------------------------------------------------------
      DO 110 J = 1, LMAX
 110    EL(J) = ZERO
      EL(2) = ONE
      HSUM = ZERO
      DO 130 J = 1, NQM2
C Construct coefficients of x*(x+xi(1))*...*(x+xi(j)). -----------------
        HSUM = HSUM + TAU(J)
        XI = HSUM/HSCAL
        JP1 = J + 1
        DO 120 IBACK = 1, JP1
          I = (J + 3) - IBACK
 120      EL(I) = EL(I)*XI + EL(I-1)
 130    CONTINUE
C Construct coefficients of integrated polynomial. ---------------------
      DO 140 J = 2, NQM1
 140    EL(J+1) = REAL(NQ)*EL(J)/REAL(J)
C Subtract correction terms from YH array. -----------------------------
      DO 170 J = 3, NQ
        DO 160 I = 1, N
 160      YH(I,J) = YH(I,J) - YH(I,L)*EL(J)
 170    CONTINUE
      RETURN
C Order increase. ------------------------------------------------------
C Zero out next column in YH array. ------------------------------------
 180  CONTINUE
      LP1 = L + 1
      DO 190 I = 1, N
 190    YH(I,LP1) = ZERO
      RETURN
C-----------------------------------------------------------------------
C Stiff option...
C Check to see if the order is being increased or decreased.
C-----------------------------------------------------------------------
 200  CONTINUE
      IF (IORD .EQ. 1) GO TO 300
C Order decrease. ------------------------------------------------------
      DO 210 J = 1, LMAX
 210    EL(J) = ZERO
      EL(3) = ONE
      HSUM = ZERO
      DO 230 J = 1,NQM2
C Construct coefficients of x*x*(x+xi(1))*...*(x+xi(j)). ---------------
        HSUM = HSUM + TAU(J)
        XI = HSUM/HSCAL
        JP1 = J + 1
        DO 220 IBACK = 1, JP1
          I = (J + 4) - IBACK
 220      EL(I) = EL(I)*XI + EL(I-1)
 230    CONTINUE
C Subtract correction terms from YH array. -----------------------------
      DO 250 J = 3,NQ
        DO 240 I = 1, N
 240      YH(I,J) = YH(I,J) - YH(I,L)*EL(J)
 250    CONTINUE
      RETURN
C Order increase. ------------------------------------------------------
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
C Construct coefficients of x*x*(x+xi(1))*...*(x+xi(j)). ---------------
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
C Load column L + 1 in YH array. ---------------------------------------
      LP1 = L + 1
      DO 350 I = 1, N
 350    YH(I,LP1) = T1*YH(I,LMAX)
C Add correction terms to YH array. ------------------------------------
      NQP1 = NQ + 1
      DO 370 J = 3, NQP1
        CALL DAXPY (N, EL(J), YH(1,LP1), 1, YH(1,J), 1 )
 370  CONTINUE
      RETURN
C----------------------- End of Subroutine DVJUST ----------------------
      END
*DECK DVNLSD
      SUBROUTINE DVNLSD (Y, YH, LDYH, VSAV, SAVF, EWT, ACOR, IWM, WM,
     1                 F, JAC, PDUM, NFLAG, RPAR, IPAR)
      EXTERNAL F, JAC, PDUM
      DOUBLE PRECISION Y, YH, VSAV, SAVF, EWT, ACOR, WM, RPAR
      INTEGER LDYH, IWM, NFLAG, IPAR
      DIMENSION Y(*), YH(LDYH,*), VSAV(*), SAVF(*), EWT(*), ACOR(*),
     1          IWM(*), WM(*), RPAR(*), IPAR(*)
C-----------------------------------------------------------------------
C Call sequence input -- Y, YH, LDYH, SAVF, EWT, ACOR, IWM, WM,
C                        F, JAC, NFLAG, RPAR, IPAR
C Call sequence output -- YH, ACOR, WM, IWM, NFLAG
C COMMON block variables accessed..
C     /DVOD01/ ACNRM, CRATE, DRC, H, RC, RL1, TQ(5), TN, ICF,
C                JCUR, METH, MITER, N, NSLP
C     /DVOD02/ HU, NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
C
C Subroutines called by DVNLSD.. F, DAXPY, DCOPY, DSCAL, DVJAC, DVSOL
C Function routines called by DVNLSD.. DVNORM
C-----------------------------------------------------------------------
C Subroutine DVNLSD is a nonlinear system solver, which uses functional
C iteration or a chord (modified Newton) method.  For the chord method
C direct linear algebraic system solvers are used.  Subroutine DVNLSD
C then handles the corrector phase of this integration package.
C
C Communication with DVNLSD is done with the following variables. (For
C more details, please see the comments in the driver subroutine.)
C
C Y          = The dependent variable, a vector of length N, input.
C YH         = The Nordsieck (Taylor) array, LDYH by LMAX, input
C              and output.  On input, it contains predicted values.
C LDYH       = A constant .ge. N, the first dimension of YH, input.
C VSAV       = Unused work array.
C SAVF       = A work array of length N.
C EWT        = An error weight vector of length N, input.
C ACOR       = A work array of length N, used for the accumulated
C              corrections to the predicted y vector.
C WM,IWM     = Real and integer work arrays associated with matrix
C              operations in chord iteration (MITER .ne. 0).
C F          = Dummy name for user supplied routine for f.
C JAC        = Dummy name for user supplied Jacobian routine.
C PDUM       = Unused dummy subroutine name.  Included for uniformity
C              over collection of integrators.
C NFLAG      = Input/output flag, with values and meanings as follows..
C              INPUT
C                  0 first call for this time step.
C                 -1 convergence failure in previous call to DVNLSD.
C                 -2 error test failure in DVSTEP.
C              OUTPUT
C                  0 successful completion of nonlinear solver.
C                 -1 convergence failure or singular matrix.
C                 -2 unrecoverable error in matrix preprocessing
C                    (cannot occur here).
C                 -3 unrecoverable error in solution (cannot occur
C                    here).
C RPAR, IPAR = Dummy names for user's real and integer work arrays.
C
C IPUP       = Own variable flag with values and meanings as follows..
C              0,            do not update the Newton matrix.
C              MITER .ne. 0, update Newton matrix, because it is the
C                            initial step, order was changed, the error
C                            test failed, or an update is indicated by
C                            the scalar RC or step counter NST.
C
C For more details, see comments in driver subroutine.
C-----------------------------------------------------------------------
C Type declarations for labeled COMMON block DVOD01 --------------------
C
      DOUBLE PRECISION ACNRM, CCMXJ, CONP, CRATE, DRC, EL,
     1     ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2     RC, RL1, TAU, TQ, TN, UROUND
      INTEGER ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     1        L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     2        LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     3        N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     4        NSLP, NYH
C
C Type declarations for labeled COMMON block DVOD02 --------------------
C
      DOUBLE PRECISION HU
      INTEGER NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
C
C Type declarations for local variables --------------------------------
C
      DOUBLE PRECISION CCMAX, CRDOWN, CSCALE, DCON, DEL, DELP, ONE,
     1     RDIV, TWO, ZERO
      INTEGER I, IERPJ, IERSL, M, MAXCOR, MSBP
C
C Type declaration for function subroutines called ---------------------
C
      DOUBLE PRECISION DVNORM
C-----------------------------------------------------------------------
C The following Fortran-77 declaration is to cause the values of the
C listed (local) variables to be saved between calls to this integrator.
C-----------------------------------------------------------------------
      SAVE CCMAX, CRDOWN, MAXCOR, MSBP, RDIV, ONE, TWO, ZERO
C
      COMMON /DVOD01/ ACNRM, CCMXJ, CONP, CRATE, DRC, EL(13),
     1                ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2                RC, RL1, TAU(13), TQ(5), TN, UROUND,
     3                ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     4                L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     5                LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     6                N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     7                NSLP, NYH
      COMMON /DVOD02/ HU, NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
C
      DATA CCMAX /0.3D0/, CRDOWN /0.3D0/, MAXCOR /3/, MSBP /20/,
     1     RDIV  /2.0D0/
      DATA ONE /1.0D0/, TWO /2.0D0/, ZERO /0.0D0/
C-----------------------------------------------------------------------
C On the first step, on a change of method order, or after a
C nonlinear convergence failure with NFLAG = -2, set IPUP = MITER
C to force a Jacobian update when MITER .ne. 0.
C-----------------------------------------------------------------------
      IF (JSTART .EQ. 0) NSLP = 0
      IF (NFLAG .EQ. 0) ICF = 0
      IF (NFLAG .EQ. -2) IPUP = MITER
      IF ( (JSTART .EQ. 0) .OR. (JSTART .EQ. -1) ) IPUP = MITER
C If this is functional iteration, set CRATE .eq. 1 and drop to 220
      IF (MITER .EQ. 0) THEN
        CRATE = ONE
        GO TO 220
      ENDIF
C-----------------------------------------------------------------------
C RC is the ratio of new to old values of the coefficient H/EL(2)=h/l1.
C When RC differs from 1 by more than CCMAX, IPUP is set to MITER
C to force DVJAC to be called, if a Jacobian is involved.
C In any case, DVJAC is called at least every MSBP steps.
C-----------------------------------------------------------------------
      DRC = ABS(RC-ONE)
      IF (DRC .GT. CCMAX .OR. NST .GE. NSLP+MSBP) IPUP = MITER
C-----------------------------------------------------------------------
C Up to MAXCOR corrector iterations are taken.  A convergence test is
C made on the r.m.s. norm of each correction, weighted by the error
C weight vector EWT.  The sum of the corrections is accumulated in the
C vector ACOR(i).  The YH array is not altered in the corrector loop.
C-----------------------------------------------------------------------
 220  M = 0
      DELP = ZERO
      CALL DCOPY (N, YH(1,1), 1, Y, 1 )
      CALL F (N, TN, Y, SAVF, RPAR, IPAR)
      NFE = NFE + 1
      IF (IPUP .LE. 0) GO TO 250
C-----------------------------------------------------------------------
C If indicated, the matrix P = I - h*rl1*J is reevaluated and
C preprocessed before starting the corrector iteration.  IPUP is set
C to 0 as an indicator that this has been done.
C-----------------------------------------------------------------------
      CALL DVJAC (Y, YH, LDYH, EWT, ACOR, SAVF, WM, IWM, F, JAC, IERPJ,
     1           RPAR, IPAR)
      IPUP = 0
      RC = ONE
      DRC = ZERO
      CRATE = ONE
      NSLP = NST
C If matrix is singular, take error return to force cut in step size. --
      IF (IERPJ .NE. 0) GO TO 430
 250  DO 260 I = 1,N
 260    ACOR(I) = ZERO
C This is a looping point for the corrector iteration. -----------------
 270  IF (MITER .NE. 0) GO TO 350
C-----------------------------------------------------------------------
C In the case of functional iteration, update Y directly from
C the result of the last function evaluation.
C-----------------------------------------------------------------------
      DO 280 I = 1,N
 280    SAVF(I) = RL1*(H*SAVF(I) - YH(I,2))
      DO 290 I = 1,N
 290    Y(I) = SAVF(I) - ACOR(I)
      DEL = DVNORM (N, Y, EWT)
      DO 300 I = 1,N
 300    Y(I) = YH(I,1) + SAVF(I)
      CALL DCOPY (N, SAVF, 1, ACOR, 1)
      GO TO 400
C-----------------------------------------------------------------------
C In the case of the chord method, compute the corrector error,
C and solve the linear system with that as right-hand side and
C P as coefficient matrix.  The correction is scaled by the factor
C 2/(1+RC) to account for changes in h*rl1 since the last DVJAC call.
C-----------------------------------------------------------------------
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
C-----------------------------------------------------------------------
C Test for convergence.  If M .gt. 0, an estimate of the convergence
C rate constant is stored in CRATE, and this is used in the test.
C-----------------------------------------------------------------------
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
C
 410  IF (MITER .EQ. 0 .OR. JCUR .EQ. 1) GO TO 430
      ICF = 1
      IPUP = MITER
      GO TO 220
C
 430  CONTINUE
      NFLAG = -1
      ICF = 2
      IPUP = MITER
      RETURN
C
C Return for successful step. ------------------------------------------
 450  NFLAG = 0
      JCUR = 0
      ICF = 0
      IF (M .EQ. 0) ACNRM = DEL
      IF (M .GT. 0) ACNRM = DVNORM (N, ACOR, EWT)
      RETURN
C----------------------- End of Subroutine DVNLSD ----------------------
      END
*DECK DVJAC
      SUBROUTINE DVJAC (Y, YH, LDYH, EWT, FTEM, SAVF, WM, IWM, F, JAC,
     1                 IERPJ, RPAR, IPAR)
      EXTERNAL F, JAC
      DOUBLE PRECISION Y, YH, EWT, FTEM, SAVF, WM, RPAR
      INTEGER LDYH, IWM, IERPJ, IPAR
      DIMENSION Y(*), YH(LDYH,*), EWT(*), FTEM(*), SAVF(*),
     1   WM(*), IWM(*), RPAR(*), IPAR(*)
C-----------------------------------------------------------------------
C Call sequence input -- Y, YH, LDYH, EWT, FTEM, SAVF, WM, IWM,
C                        F, JAC, RPAR, IPAR
C Call sequence output -- WM, IWM, IERPJ
C COMMON block variables accessed..
C     /DVOD01/  CCMXJ, DRC, H, RL1, TN, UROUND, ICF, JCUR, LOCJS,
C               MSBJ, NSLJ
C     /DVOD02/  NFE, NST, NJE, NLU
C
C Subroutines called by DVJAC.. F, JAC, DACOPY, DCOPY, DGBFA, DGEFA,
C                              DSCAL
C Function routines called by DVJAC.. DVNORM
C-----------------------------------------------------------------------
C DVJAC is called by DVSTEP to compute and process the matrix
C P = I - h*rl1*J , where J is an approximation to the Jacobian.
C Here J is computed by the user-supplied routine JAC if
C MITER = 1 or 4, or by finite differencing if MITER = 2, 3, or 5.
C If MITER = 3, a diagonal approximation to J is used.
C If JSV = -1, J is computed from scratch in all cases.
C If JSV = 1 and MITER = 1, 2, 4, or 5, and if the saved value of J is
C considered acceptable, then P is constructed from the saved J.
C J is stored in wm and replaced by P.  If MITER .ne. 3, P is then
C subjected to LU decomposition in preparation for later solution
C of linear systems with P as coefficient matrix. This is done
C by DGEFA if MITER = 1 or 2, and by DGBFA if MITER = 4 or 5.
C
C Communication with DVJAC is done with the following variables.  (For
C more details, please see the comments in the driver subroutine.)
C Y          = Vector containing predicted values on entry.
C YH         = The Nordsieck array, an LDYH by LMAX array, input.
C LDYH       = A constant .ge. N, the first dimension of YH, input.
C EWT        = An error weight vector of length N.
C SAVF       = Array containing f evaluated at predicted y, input.
C WM         = Real work space for matrices.  In the output, it containS
C              the inverse diagonal matrix if MITER = 3 and the LU
C              decomposition of P if MITER is 1, 2 , 4, or 5.
C              Storage of matrix elements starts at WM(3).
C              Storage of the saved Jacobian starts at WM(LOCJS).
C              WM also contains the following matrix-related data..
C              WM(1) = SQRT(UROUND), used in numerical Jacobian step.
C              WM(2) = H*RL1, saved for later use if MITER = 3.
C IWM        = Integer work space containing pivot information,
C              starting at IWM(31), if MITER is 1, 2, 4, or 5.
C              IWM also contains band parameters ML = IWM(1) and
C              MU = IWM(2) if MITER is 4 or 5.
C F          = Dummy name for the user supplied subroutine for f.
C JAC        = Dummy name for the user supplied Jacobian subroutine.
C RPAR, IPAR = Dummy names for user's real and integer work arrays.
C RL1        = 1/EL(2) (input).
C IERPJ      = Output error flag,  = 0 if no trouble, 1 if the P
C              matrix is found to be singular.
C JCUR       = Output flag to indicate whether the Jacobian matrix
C              (or approximation) is now current.
C              JCUR = 0 means J is not current.
C              JCUR = 1 means J is current.
C-----------------------------------------------------------------------
C
C Type declarations for labeled COMMON block DVOD01 --------------------
C
      DOUBLE PRECISION ACNRM, CCMXJ, CONP, CRATE, DRC, EL,
     1     ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2     RC, RL1, TAU, TQ, TN, UROUND
      INTEGER ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     1        L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     2        LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     3        N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     4        NSLP, NYH
C
C Type declarations for labeled COMMON block DVOD02 --------------------
C
      DOUBLE PRECISION HU
      INTEGER NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
C
C Type declarations for local variables --------------------------------
C
      DOUBLE PRECISION CON, DI, FAC, HRL1, ONE, PT1, R, R0, SRUR, THOU,
     1     YI, YJ, YJJ, ZERO
      INTEGER I, I1, I2, IER, II, J, J1, JJ, JOK, LENP, MBA, MBAND,
     1        MEB1, MEBAND, ML, ML3, MU, NP1
C
C Type declaration for function subroutines called ---------------------
C
      DOUBLE PRECISION DVNORM
C-----------------------------------------------------------------------
C The following Fortran-77 declaration is to cause the values of the
C listed (local) variables to be saved between calls to this subroutine.
C-----------------------------------------------------------------------
      SAVE ONE, PT1, THOU, ZERO
C-----------------------------------------------------------------------
      COMMON /DVOD01/ ACNRM, CCMXJ, CONP, CRATE, DRC, EL(13),
     1                ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2                RC, RL1, TAU(13), TQ(5), TN, UROUND,
     3                ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     4                L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     5                LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     6                N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     7                NSLP, NYH
      COMMON /DVOD02/ HU, NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
C
      DATA ONE /1.0D0/, THOU /1000.0D0/, ZERO /0.0D0/, PT1 /0.1D0/
C
      IERPJ = 0
      HRL1 = H*RL1
C See whether J should be evaluated (JOK = -1) or not (JOK = 1). -------
      JOK = JSV
      IF (JSV .EQ. 1) THEN
        IF (NST .EQ. 0 .OR. NST .GT. NSLJ+MSBJ) JOK = -1
        IF (ICF .EQ. 1 .AND. DRC .LT. CCMXJ) JOK = -1
        IF (ICF .EQ. 2) JOK = -1
      ENDIF
C End of setting JOK. --------------------------------------------------
C
      IF (JOK .EQ. -1 .AND. MITER .EQ. 1) THEN
C If JOK = -1 and MITER = 1, call JAC to evaluate Jacobian. ------------
      NJE = NJE + 1
      NSLJ = NST
      JCUR = 1
      LENP = N*N
      DO 110 I = 1,LENP
 110    WM(I+2) = ZERO
      CALL JAC (N, TN, Y, 0, 0, WM(3), N, RPAR, IPAR)
      IF (JSV .EQ. 1) CALL DCOPY (LENP, WM(3), 1, WM(LOCJS), 1)
      ENDIF
C
      IF (JOK .EQ. -1 .AND. MITER .EQ. 2) THEN
C If MITER = 2, make N calls to F to approximate the Jacobian. ---------
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
C
      IF (JOK .EQ. 1 .AND. (MITER .EQ. 1 .OR. MITER .EQ. 2)) THEN
      JCUR = 0
      LENP = N*N
      CALL DCOPY (LENP, WM(LOCJS), 1, WM(3), 1)
      ENDIF
C
      IF (MITER .EQ. 1 .OR. MITER .EQ. 2) THEN
C Multiply Jacobian by scalar, add identity, and do LU decomposition. --
      CON = -HRL1
      CALL DSCAL (LENP, CON, WM(3), 1)
      J = 3
      NP1 = N + 1
      DO 250 I = 1,N
        WM(J) = WM(J) + ONE
 250    J = J + NP1
      NLU = NLU + 1
      CALL DGEFA (WM(3), N, N, IWM(31), IER)
      IF (IER .NE. 0) IERPJ = 1
      RETURN
      ENDIF
C End of code block for MITER = 1 or 2. --------------------------------
C
      IF (MITER .EQ. 3) THEN
C If MITER = 3, construct a diagonal approximation to J and P. ---------
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
C End of code block for MITER = 3. -------------------------------------
C
C Set constants for MITER = 4 or 5. ------------------------------------
      ML = IWM(1)
      MU = IWM(2)
      ML3 = ML + 3
      MBAND = ML + MU + 1
      MEBAND = MBAND + ML
      LENP = MEBAND*N
C
      IF (JOK .EQ. -1 .AND. MITER .EQ. 4) THEN
C If JOK = -1 and MITER = 4, call JAC to evaluate Jacobian. ------------
      NJE = NJE + 1
      NSLJ = NST
      JCUR = 1
      DO 410 I = 1,LENP
 410    WM(I+2) = ZERO
      CALL JAC (N, TN, Y, ML, MU, WM(ML3), MEBAND, RPAR, IPAR)
      IF (JSV .EQ. 1)
     1   CALL DACOPY (MBAND, N, WM(ML3), MEBAND, WM(LOCJS), MBAND)
      ENDIF
C
      IF (JOK .EQ. -1 .AND. MITER .EQ. 5) THEN
C If MITER = 5, make N calls to F to approximate the Jacobian. ---------
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
C
      IF (JOK .EQ. 1) THEN
      JCUR = 0
      CALL DACOPY (MBAND, N, WM(LOCJS), MBAND, WM(ML3), MEBAND)
      ENDIF
C
C Multiply Jacobian by scalar, add identity, and do LU decomposition.
      CON = -HRL1
      CALL DSCAL (LENP, CON, WM(3), 1 )
      II = MBAND + 2
      DO 580 I = 1,N
        WM(II) = WM(II) + ONE
 580    II = II + MEBAND
      NLU = NLU + 1
      CALL DGBFA (WM(3), MEBAND, N, ML, MU, IWM(31), IER)
      IF (IER .NE. 0) IERPJ = 1
      RETURN
C End of code block for MITER = 4 or 5. --------------------------------
C
C----------------------- End of Subroutine DVJAC -----------------------
      END
*DECK DACOPY
      SUBROUTINE DACOPY (NROW, NCOL, A, NROWA, B, NROWB)
      DOUBLE PRECISION A, B
      INTEGER NROW, NCOL, NROWA, NROWB
      DIMENSION A(NROWA,NCOL), B(NROWB,NCOL)
C-----------------------------------------------------------------------
C Call sequence input -- NROW, NCOL, A, NROWA, NROWB
C Call sequence output -- B
C COMMON block variables accessed -- None
C
C Subroutines called by DACOPY.. DCOPY
C Function routines called by DACOPY.. None
C-----------------------------------------------------------------------
C This routine copies one rectangular array, A, to another, B,
C where A and B may have different row dimensions, NROWA and NROWB.
C The data copied consists of NROW rows and NCOL columns.
C-----------------------------------------------------------------------
      INTEGER IC
C
      DO 20 IC = 1,NCOL
        CALL DCOPY (NROW, A(1,IC), 1, B(1,IC), 1)
 20     CONTINUE
C
      RETURN
C----------------------- End of Subroutine DACOPY ----------------------
      END
*DECK DVSOL
      SUBROUTINE DVSOL (WM, IWM, X, IERSL)
      DOUBLE PRECISION WM, X
      INTEGER IWM, IERSL
      DIMENSION WM(*), IWM(*), X(*)
C-----------------------------------------------------------------------
C Call sequence input -- WM, IWM, X
C Call sequence output -- X, IERSL
C COMMON block variables accessed..
C     /DVOD01/ -- H, RL1, MITER, N
C
C Subroutines called by DVSOL.. DGESL, DGBSL
C Function routines called by DVSOL.. None
C-----------------------------------------------------------------------
C This routine manages the solution of the linear system arising from
C a chord iteration.  It is called if MITER .ne. 0.
C If MITER is 1 or 2, it calls DGESL to accomplish this.
C If MITER = 3 it updates the coefficient H*RL1 in the diagonal
C matrix, and then computes the solution.
C If MITER is 4 or 5, it calls DGBSL.
C Communication with DVSOL uses the following variables..
C WM    = Real work space containing the inverse diagonal matrix if
C         MITER = 3 and the LU decomposition of the matrix otherwise.
C         Storage of matrix elements starts at WM(3).
C         WM also contains the following matrix-related data..
C         WM(1) = SQRT(UROUND) (not used here),
C         WM(2) = HRL1, the previous value of H*RL1, used if MITER = 3.
C IWM   = Integer work space containing pivot information, starting at
C         IWM(31), if MITER is 1, 2, 4, or 5.  IWM also contains band
C         parameters ML = IWM(1) and MU = IWM(2) if MITER is 4 or 5.
C X     = The right-hand side vector on input, and the solution vector
C         on output, of length N.
C IERSL = Output flag.  IERSL = 0 if no trouble occurred.
C         IERSL = 1 if a singular matrix arose with MITER = 3.
C-----------------------------------------------------------------------
C
C Type declarations for labeled COMMON block DVOD01 --------------------
C
      DOUBLE PRECISION ACNRM, CCMXJ, CONP, CRATE, DRC, EL,
     1     ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2     RC, RL1, TAU, TQ, TN, UROUND
      INTEGER ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     1        L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     2        LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     3        N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     4        NSLP, NYH
C
C Type declarations for local variables --------------------------------
C
      INTEGER I, MEBAND, ML, MU
      DOUBLE PRECISION DI, HRL1, ONE, PHRL1, R, ZERO
C-----------------------------------------------------------------------
C The following Fortran-77 declaration is to cause the values of the
C listed (local) variables to be saved between calls to this integrator.
C-----------------------------------------------------------------------
      SAVE ONE, ZERO
C
      COMMON /DVOD01/ ACNRM, CCMXJ, CONP, CRATE, DRC, EL(13),
     1                ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2                RC, RL1, TAU(13), TQ(5), TN, UROUND,
     3                ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     4                L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     5                LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     6                N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     7                NSLP, NYH
C
      DATA ONE /1.0D0/, ZERO /0.0D0/
C
      IERSL = 0
      GO TO (100, 100, 300, 400, 400), MITER
 100  CALL DGESL (WM(3), N, N, IWM(31), X, 0)
      RETURN
C
 300  PHRL1 = WM(2)
      HRL1 = H*RL1
      WM(2) = HRL1
      IF (HRL1 .EQ. PHRL1) GO TO 330
      R = HRL1/PHRL1
      DO 320 I = 1,N
        DI = ONE - R*(ONE - ONE/WM(I+2))
        IF (ABS(DI) .EQ. ZERO) GO TO 390
 320    WM(I+2) = ONE/DI
C
 330  DO 340 I = 1,N
 340    X(I) = WM(I+2)*X(I)
      RETURN
 390  IERSL = 1
      RETURN
C
 400  ML = IWM(1)
      MU = IWM(2)
      MEBAND = 2*ML + MU + 1
      CALL DGBSL (WM(3), MEBAND, N, ML, MU, IWM(31), X, 0)
      RETURN
C----------------------- End of Subroutine DVSOL -----------------------
      END
*DECK DVSRCO
      SUBROUTINE DVSRCO (RSAV, ISAV, JOB)
      DOUBLE PRECISION RSAV
      INTEGER ISAV, JOB
      DIMENSION RSAV(*), ISAV(*)
C-----------------------------------------------------------------------
C Call sequence input -- RSAV, ISAV, JOB
C Call sequence output -- RSAV, ISAV
C COMMON block variables accessed -- All of /DVOD01/ and /DVOD02/
C
C Subroutines/functions called by DVSRCO.. None
C-----------------------------------------------------------------------
C This routine saves or restores (depending on JOB) the contents of the
C COMMON blocks DVOD01 and DVOD02, which are used internally by DVODE.
C
C RSAV = real array of length 49 or more.
C ISAV = integer array of length 41 or more.
C JOB  = flag indicating to save or restore the COMMON blocks..
C        JOB  = 1 if COMMON is to be saved (written to RSAV/ISAV).
C        JOB  = 2 if COMMON is to be restored (read from RSAV/ISAV).
C        A call with JOB = 2 presumes a prior call with JOB = 1.
C-----------------------------------------------------------------------
      DOUBLE PRECISION RVOD1, RVOD2
      INTEGER IVOD1, IVOD2
      INTEGER I, LENIV1, LENIV2, LENRV1, LENRV2
C-----------------------------------------------------------------------
C The following Fortran-77 declaration is to cause the values of the
C listed (local) variables to be saved between calls to this integrator.
C-----------------------------------------------------------------------
      SAVE LENRV1, LENIV1, LENRV2, LENIV2
C
      COMMON /DVOD01/ RVOD1(48), IVOD1(33)
      COMMON /DVOD02/ RVOD2(1), IVOD2(8)
      DATA LENRV1/48/, LENIV1/33/, LENRV2/1/, LENIV2/8/
C
      IF (JOB .EQ. 2) GO TO 100
      DO 10 I = 1,LENRV1
 10     RSAV(I) = RVOD1(I)
      DO 15 I = 1,LENRV2
 15     RSAV(LENRV1+I) = RVOD2(I)
C
      DO 20 I = 1,LENIV1
 20     ISAV(I) = IVOD1(I)
      DO 25 I = 1,LENIV2
 25     ISAV(LENIV1+I) = IVOD2(I)
C
      RETURN
C
 100  CONTINUE
      DO 110 I = 1,LENRV1
 110     RVOD1(I) = RSAV(I)
      DO 115 I = 1,LENRV2
 115     RVOD2(I) = RSAV(LENRV1+I)
C
      DO 120 I = 1,LENIV1
 120     IVOD1(I) = ISAV(I)
      DO 125 I = 1,LENIV2
 125     IVOD2(I) = ISAV(LENIV1+I)
C
      RETURN
C----------------------- End of Subroutine DVSRCO ----------------------
      END
*DECK DEWSET
      SUBROUTINE DEWSET (N, ITOL, RTOL, ATOL, YCUR, EWT)
      DOUBLE PRECISION RTOL, ATOL, YCUR, EWT
      INTEGER N, ITOL
      DIMENSION RTOL(*), ATOL(*), YCUR(N), EWT(N)
C-----------------------------------------------------------------------
C Call sequence input -- N, ITOL, RTOL, ATOL, YCUR
C Call sequence output -- EWT
C COMMON block variables accessed -- None
C
C Subroutines/functions called by DEWSET.. None
C-----------------------------------------------------------------------
C This subroutine sets the error weight vector EWT according to
C     EWT(i) = RTOL(i)*abs(YCUR(i)) + ATOL(i),  i = 1,...,N,
C with the subscript on RTOL and/or ATOL possibly replaced by 1 above,
C depending on the value of ITOL.
C-----------------------------------------------------------------------
      INTEGER I
C
      GO TO (10, 20, 30, 40), ITOL
 10   CONTINUE
      DO 15 I = 1, N
 15     EWT(I) = RTOL(1)*ABS(YCUR(I)) + ATOL(1)
      RETURN
 20   CONTINUE
      DO 25 I = 1, N
 25     EWT(I) = RTOL(1)*ABS(YCUR(I)) + ATOL(I)
      RETURN
 30   CONTINUE
      DO 35 I = 1, N
 35     EWT(I) = RTOL(I)*ABS(YCUR(I)) + ATOL(1)
      RETURN
 40   CONTINUE
      DO 45 I = 1, N
 45     EWT(I) = RTOL(I)*ABS(YCUR(I)) + ATOL(I)
      RETURN
C----------------------- End of Subroutine DEWSET ----------------------
      END
*DECK DVNORM
      DOUBLE PRECISION FUNCTION DVNORM (N, V, W)
      DOUBLE PRECISION V, W
      INTEGER N
      DIMENSION V(N), W(N)
C-----------------------------------------------------------------------
C Call sequence input -- N, V, W
C Call sequence output -- None
C COMMON block variables accessed -- None
C
C Subroutines/functions called by DVNORM.. None
C-----------------------------------------------------------------------
C This function routine computes the weighted root-mean-square norm
C of the vector of length N contained in the array V, with weights
C contained in the array W of length N..
C   DVNORM = sqrt( (1/N) * sum( V(i)*W(i) )**2 )
C-----------------------------------------------------------------------
      DOUBLE PRECISION SUM
      INTEGER I
C
      SUM = 0.0D0
      DO 10 I = 1, N
 10     SUM = SUM + (V(I)*W(I))**2
      DVNORM = SQRT(SUM/REAL(N))
      RETURN
C----------------------- End of Function DVNORM ------------------------
      END
*DECK D1MACH
      DOUBLE PRECISION FUNCTION D1MACH (IDUM)
      INTEGER IDUM
C-----------------------------------------------------------------------
C This routine computes the unit roundoff of the machine.
C This is defined as the smallest positive machine number
C u such that  1.0 + u .ne. 1.0
C
C Subroutines/functions called by D1MACH.. None
C-----------------------------------------------------------------------
      DOUBLE PRECISION U, COMP
      U = 1.0D0
 10   U = U*0.5D0
      COMP = 1.0D0 + U
      IF (COMP .NE. 1.0D0) GO TO 10
      D1MACH = U*2.0D0
      RETURN
C----------------------- End of Function D1MACH ------------------------
      END
*DECK XERRWD
      SUBROUTINE XERRWD (MSG, NMES, NERR, LEVEL, NI, I1, I2, NR, R1, R2)
      DOUBLE PRECISION R1, R2
      INTEGER NMES, NERR, LEVEL, NI, I1, I2, NR
      CHARACTER*1 MSG(NMES)
C-----------------------------------------------------------------------
C Subroutines XERRWD, XSETF, XSETUN, and the two function routines
C MFLGSV and LUNSAV, as given here, constitute a simplified version of
C the SLATEC error handling package.
C Written by A. C. Hindmarsh and P. N. Brown at LLNL.
C Version of 13 April, 1989.
C This version is in double precision.
C
C All arguments are input arguments.
C
C MSG    = The message (character array).
C NMES   = The length of MSG (number of characters).
C NERR   = The error number (not used).
C LEVEL  = The error level..
C          0 or 1 means recoverable (control returns to caller).
C          2 means fatal (run is aborted--see note below).
C NI     = Number of integers (0, 1, or 2) to be printed with message.
C I1,I2  = Integers to be printed, depending on NI.
C NR     = Number of reals (0, 1, or 2) to be printed with message.
C R1,R2  = Reals to be printed, depending on NR.
C
C Note..  this routine is machine-dependent and specialized for use
C in limited context, in the following ways..
C 1. The argument MSG is assumed to be of type CHARACTER, and
C    the message is printed with a format of (1X,80A1).
C 2. The message is assumed to take only one line.
C    Multi-line messages are generated by repeated calls.
C 3. If LEVEL = 2, control passes to the statement   STOP
C    to abort the run.  This statement may be machine-dependent.
C 4. R1 and R2 are assumed to be in double precision and are printed
C    in D21.13 format.
C
C For a different default logical unit number, change the data
C statement in function routine LUNSAV.
C For a different run-abort command, change the statement following
C statement 100 at the end.
C-----------------------------------------------------------------------
C Subroutines called by XERRWD.. None
C Function routines called by XERRWD.. MFLGSV, LUNSAV
C-----------------------------------------------------------------------
C
      INTEGER I, LUNIT, LUNSAV, MESFLG, MFLGSV
C
C Get message print flag and logical unit number. ----------------------
      MESFLG = MFLGSV (0,.FALSE.)
      LUNIT = LUNSAV (0,.FALSE.)
      IF (MESFLG .EQ. 0) GO TO 100
C Write the message. ---------------------------------------------------
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
C Abort the run if LEVEL = 2. ------------------------------------------
 100  IF (LEVEL .NE. 2) RETURN
      STOP
C----------------------- End of Subroutine XERRWD ----------------------
      END
*DECK XSETF
      SUBROUTINE XSETF (MFLAG)
C-----------------------------------------------------------------------
C This routine resets the print control flag MFLAG.
C
C Subroutines called by XSETF.. None
C Function routines called by XSETF.. MFLGSV
C-----------------------------------------------------------------------
      INTEGER MFLAG, JUNK, MFLGSV
C
      IF (MFLAG .EQ. 0 .OR. MFLAG .EQ. 1) JUNK = MFLGSV (MFLAG,.TRUE.)
      RETURN
C----------------------- End of Subroutine XSETF -----------------------
      END
*DECK XSETUN
      SUBROUTINE XSETUN (LUN)
C-----------------------------------------------------------------------
C This routine resets the logical unit number for messages.
C
C Subroutines called by XSETUN.. None
C Function routines called by XSETUN.. LUNSAV
C-----------------------------------------------------------------------
      INTEGER LUN, JUNK, LUNSAV
C
      IF (LUN .GT. 0) JUNK = LUNSAV (LUN,.TRUE.)
      RETURN
C----------------------- End of Subroutine XSETUN ----------------------
      END
*DECK MFLGSV
      INTEGER FUNCTION MFLGSV (IVALUE, ISET)
      LOGICAL ISET
      INTEGER IVALUE
C-----------------------------------------------------------------------
C MFLGSV saves and recalls the parameter MESFLG which controls the
C printing of the error messages.
C
C Saved local variable..
C
C   MESFLG = Print control flag..
C            1 means print all messages (the default).
C            0 means no printing.
C
C On input..
C
C   IVALUE = The value to be set for the MESFLG parameter,
C            if ISET is .TRUE. .
C
C   ISET   = Logical flag to indicate whether to read or write.
C            If ISET=.TRUE., the MESFLG parameter will be given
C            the value IVALUE.  If ISET=.FALSE., the MESFLG
C            parameter will be unchanged, and IVALUE is a dummy
C            parameter.
C
C On return..
C
C   The (old) value of the MESFLG parameter will be returned
C   in the function value, MFLGSV.
C
C This is a modification of the SLATEC library routine J4SAVE.
C
C Subroutines/functions called by MFLGSV.. None
C-----------------------------------------------------------------------
      INTEGER MESFLG
C-----------------------------------------------------------------------
C The following Fortran-77 declaration is to cause the values of the
C listed (local) variables to be saved between calls to this integrator.
C-----------------------------------------------------------------------
      SAVE MESFLG
      DATA MESFLG/1/
C
      MFLGSV = MESFLG
      IF (ISET) MESFLG = IVALUE
      RETURN
C----------------------- End of Function MFLGSV ------------------------
      END
*DECK LUNSAV
      INTEGER FUNCTION LUNSAV (IVALUE, ISET)
      LOGICAL ISET
      INTEGER IVALUE
C-----------------------------------------------------------------------
C LUNSAV saves and recalls the parameter LUNIT which is the logical
C unit number to which error messages are printed.
C
C Saved local variable..
C
C  LUNIT   = Logical unit number for messages.
C            The default is 6 (machine-dependent).
C
C On input..
C
C   IVALUE = The value to be set for the LUNIT parameter,
C            if ISET is .TRUE. .
C
C   ISET   = Logical flag to indicate whether to read or write.
C            If ISET=.TRUE., the LUNIT parameter will be given
C            the value IVALUE.  If ISET=.FALSE., the LUNIT
C            parameter will be unchanged, and IVALUE is a dummy
C            parameter.
C
C On return..
C
C   The (old) value of the LUNIT parameter will be returned
C   in the function value, LUNSAV.
C
C This is a modification of the SLATEC library routine J4SAVE.
C
C Subroutines/functions called by LUNSAV.. None
C-----------------------------------------------------------------------
      INTEGER LUNIT
C-----------------------------------------------------------------------
C The following Fortran-77 declaration is to cause the values of the
C listed (local) variables to be saved between calls to this integrator.
C-----------------------------------------------------------------------
      SAVE LUNIT
      DATA LUNIT/6/
C
      LUNSAV = LUNIT
      IF (ISET) LUNIT = IVALUE
      RETURN
C----------------------- End of Function LUNSAV ------------------------
      END

C***********************************************************************
C***********************************************************************

C  VODEXT.FOR IS SIMPLY A CONCATENATION OF 9 MODULES NEEDED BY 
C  VODE.FOR. THEY ARE FOUND IN  ftp.netlib.org  on the web.

C  In \BLAS
c	DCOPY.F  DSCAL.F  DAXPY.F  DDOT.F  IDAMAX.F

C  In \LINPACK
c  	DGEFA.F  DGESL.F  DGBFA.F  DGBSL.F  

C  ALL ABOVE MODULES HAVE BEEN COPIED INTO FILES WITH EXTENSION .FOR.


C-----------------------------------------------------------------------

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

      subroutine dgefa(a,lda,n,ipvt,info)
      integer lda,n,ipvt(1),info
      double precision a(lda,1)
c
c     dgefa factors a double precision matrix by gaussian elimination.
c
c     dgefa is usually called by dgeco, but it can be called
c     directly with a saving in time if  rcond  is not needed.
c     (time for dgeco) = (1 + 9/n)*(time for dgefa) .
c
c     on entry
c
c        a       double precision(lda, n)
c                the matrix to be factored.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c     on return
c
c        a       an upper triangular matrix and the multipliers
c                which were used to obtain it.
c                the factorization can be written  a = l*u  where
c                l  is a product of permutation and unit lower
c                triangular matrices and  u  is upper triangular.
c
c        ipvt    integer(n)
c                an integer vector of pivot indices.
c
c        info    integer
c                = 0  normal value.
c                = k  if  u(k,k) .eq. 0.0 .  this is not an error
c                     condition for this subroutine, but it does
c                     indicate that dgesl or dgedi will divide by zero
c                     if called.  use  rcond  in dgeco for a reliable
c                     indication of singularity.
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas daxpy,dscal,idamax
c
c     internal variables
c
      double precision t
      integer idamax,j,k,kp1,l,nm1
c
c
c     gaussian elimination with partial pivoting
c
      info = 0
      nm1 = n - 1
      if (nm1 .lt. 1) go to 70
      do 60 k = 1, nm1
         kp1 = k + 1
c
c        find l = pivot index
c
         l = idamax(n-k+1,a(k,k),1) + k - 1
         ipvt(k) = l
c
c        zero pivot implies this column already triangularized
c
         if (a(l,k) .eq. 0.0d0) go to 40
c
c           interchange if necessary
c
            if (l .eq. k) go to 10
               t = a(l,k)
               a(l,k) = a(k,k)
               a(k,k) = t
   10       continue
c
c           compute multipliers
c
            t = -1.0d0/a(k,k)
            call dscal(n-k,t,a(k+1,k),1)
c
c           row elimination with column indexing
c
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

C-----------------------------------------------------------------------

      subroutine dgesl(a,lda,n,ipvt,b,job)
      integer lda,n,ipvt(1),job
      double precision a(lda,1),b(1)
c
c     dgesl solves the double precision system
c     a * x = b  or  trans(a) * x = b
c     using the factors computed by dgeco or dgefa.
c
c     on entry
c
c        a       double precision(lda, n)
c                the output from dgeco or dgefa.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c        ipvt    integer(n)
c                the pivot vector from dgeco or dgefa.
c
c        b       double precision(n)
c                the right hand side vector.
c
c        job     integer
c                = 0         to solve  a*x = b ,
c                = nonzero   to solve  trans(a)*x = b  where
c                            trans(a)  is the transpose.
c
c     on return
c
c        b       the solution vector  x .
c
c     error condition
c
c        a division by zero will occur if the input factor contains a
c        zero on the diagonal.  technically this indicates singularity
c        but it is often caused by improper arguments or improper
c        setting of lda .  it will not occur if the subroutines are
c        called correctly and if dgeco has set rcond .gt. 0.0
c        or dgefa has set info .eq. 0 .
c
c     to compute  inverse(a) * c  where  c  is a matrix
c     with  p  columns
c           call dgeco(a,lda,n,ipvt,rcond,z)
c           if (rcond is too small) go to ...
c           do 10 j = 1, p
c              call dgesl(a,lda,n,ipvt,c(1,j),0)
c        10 continue
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas daxpy,ddot
c
c     internal variables
c
      double precision ddot,t
      integer k,kb,l,nm1
c
      nm1 = n - 1
      if (job .ne. 0) go to 50
c
c        job = 0 , solve  a * x = b
c        first solve  l*y = b
c
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
c
c        now solve  u*x = y
c
         do 40 kb = 1, n
            k = n + 1 - kb
            b(k) = b(k)/a(k,k)
            t = -b(k)
            call daxpy(k-1,t,a(1,k),1,b(1),1)
   40    continue
      go to 100
   50 continue
c
c        job = nonzero, solve  trans(a) * x = b
c        first solve  trans(u)*y = b
c
         do 60 k = 1, n
            t = ddot(k-1,a(1,k),1,b(1),1)
            b(k) = (b(k) - t)/a(k,k)
   60    continue
c
c        now solve trans(l)*x = y
c
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

C-----------------------------------------------------------------------

      subroutine dgbfa(abd,lda,n,ml,mu,ipvt,info)
      integer lda,n,ml,mu,ipvt(1),info
      double precision abd(lda,1)
c
c     dgbfa factors a double precision band matrix by elimination.
c
c     dgbfa is usually called by dgbco, but it can be called
c     directly with a saving in time if  rcond  is not needed.
c
c     on entry
c
c        abd     double precision(lda, n)
c                contains the matrix in band storage.  the columns
c                of the matrix are stored in the columns of  abd  and
c                the diagonals of the matrix are stored in rows
c                ml+1 through 2*ml+mu+1 of  abd .
c                see the comments below for details.
c
c        lda     integer
c                the leading dimension of the array  abd .
c                lda must be .ge. 2*ml + mu + 1 .
c
c        n       integer
c                the order of the original matrix.
c
c        ml      integer
c                number of diagonals below the main diagonal.
c                0 .le. ml .lt. n .
c
c        mu      integer
c                number of diagonals above the main diagonal.
c                0 .le. mu .lt. n .
c                more efficient if  ml .le. mu .
c     on return
c
c        abd     an upper triangular matrix in band storage and
c                the multipliers which were used to obtain it.
c                the factorization can be written  a = l*u  where
c                l  is a product of permutation and unit lower
c                triangular matrices and  u  is upper triangular.
c
c        ipvt    integer(n)
c                an integer vector of pivot indices.
c
c        info    integer
c                = 0  normal value.
c                = k  if  u(k,k) .eq. 0.0 .  this is not an error
c                     condition for this subroutine, but it does
c                     indicate that dgbsl will divide by zero if
c                     called.  use  rcond  in dgbco for a reliable
c                     indication of singularity.
c
c     band storage
c
c           if  a  is a band matrix, the following program segment
c           will set up the input.
c
c                   ml = (band width below the diagonal)
c                   mu = (band width above the diagonal)
c                   m = ml + mu + 1
c                   do 20 j = 1, n
c                      i1 = max0(1, j-mu)
c                      i2 = min0(n, j+ml)
c                      do 10 i = i1, i2
c                         k = i - j + m
c                         abd(k,j) = a(i,j)
c                10    continue
c                20 continue
c
c           this uses rows  ml+1  through  2*ml+mu+1  of  abd .
c           in addition, the first  ml  rows in  abd  are used for
c           elements generated during the triangularization.
c           the total number of rows needed in  abd  is  2*ml+mu+1 .
c           the  ml+mu by ml+mu  upper left triangle and the
c           ml by ml  lower right triangle are not referenced.
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas daxpy,dscal,idamax
c     fortran max0,min0
c
c     internal variables
c
      double precision t
      integer i,idamax,i0,j,ju,jz,j0,j1,k,kp1,l,lm,m,mm,nm1
c
c
      m = ml + mu + 1
      info = 0
c
c     zero initial fill-in columns
c
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
c
c     gaussian elimination with partial pivoting
c
      nm1 = n - 1
      if (nm1 .lt. 1) go to 130
      do 120 k = 1, nm1
         kp1 = k + 1
c
c        zero next fill-in column
c
         jz = jz + 1
         if (jz .gt. n) go to 50
         if (ml .lt. 1) go to 50
            do 40 i = 1, ml
               abd(i,jz) = 0.0d0
   40       continue
   50    continue
c
c        find l = pivot index
c
         lm = min0(ml,n-k)
         l = idamax(lm+1,abd(m,k),1) + m - 1
         ipvt(k) = l + k - m
c
c        zero pivot implies this column already triangularized
c
         if (abd(l,k) .eq. 0.0d0) go to 100
c
c           interchange if necessary
c
            if (l .eq. m) go to 60
               t = abd(l,k)
               abd(l,k) = abd(m,k)
               abd(m,k) = t
   60       continue
c
c           compute multipliers
c
            t = -1.0d0/abd(m,k)
            call dscal(lm,t,abd(m+1,k),1)
c
c           row elimination with column indexing
c
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

C-----------------------------------------------------------------------

      subroutine dgbsl(abd,lda,n,ml,mu,ipvt,b,job)
      integer lda,n,ml,mu,ipvt(1),job
      double precision abd(lda,1),b(1)
c
c     dgbsl solves the double precision band system
c     a * x = b  or  trans(a) * x = b
c     using the factors computed by dgbco or dgbfa.
c
c     on entry
c
c        abd     double precision(lda, n)
c                the output from dgbco or dgbfa.
c
c        lda     integer
c                the leading dimension of the array  abd .
c
c        n       integer
c                the order of the original matrix.
c
c        ml      integer
c                number of diagonals below the main diagonal.
c
c        mu      integer
c                number of diagonals above the main diagonal.
c
c        ipvt    integer(n)
c                the pivot vector from dgbco or dgbfa.
c
c        b       double precision(n)
c                the right hand side vector.
c
c        job     integer
c                = 0         to solve  a*x = b ,
c                = nonzero   to solve  trans(a)*x = b , where
c                            trans(a)  is the transpose.
c
c     on return
c
c        b       the solution vector  x .
c
c     error condition
c
c        a division by zero will occur if the input factor contains a
c        zero on the diagonal.  technically this indicates singularity
c        but it is often caused by improper arguments or improper
c        setting of lda .  it will not occur if the subroutines are
c        called correctly and if dgbco has set rcond .gt. 0.0
c        or dgbfa has set info .eq. 0 .
c
c     to compute  inverse(a) * c  where  c  is a matrix
c     with  p  columns
c           call dgbco(abd,lda,n,ml,mu,ipvt,rcond,z)
c           if (rcond is too small) go to ...
c           do 10 j = 1, p
c              call dgbsl(abd,lda,n,ml,mu,ipvt,c(1,j),0)
c        10 continue
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas daxpy,ddot
c     fortran min0
c
c     internal variables
c
      double precision ddot,t
      integer k,kb,l,la,lb,lm,m,nm1
c
      m = mu + ml + 1
      nm1 = n - 1
      if (job .ne. 0) go to 50
c
c        job = 0 , solve  a * x = b
c        first solve l*y = b
c
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
c
c        now solve  u*x = y
c
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
c
c        job = nonzero, solve  trans(a) * x = b
c        first solve  trans(u)*y = b
c
         do 60 k = 1, n
            lm = min0(k,m) - 1
            la = m - lm
            lb = k - lm
            t = ddot(lm,abd(la,k),1,b(lb),1)
            b(k) = (b(k) - t)/abd(m,k)
   60    continue
c
c        now solve trans(l)*x = y
c
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
      