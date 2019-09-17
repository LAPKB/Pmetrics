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





