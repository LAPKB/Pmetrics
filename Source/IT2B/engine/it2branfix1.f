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
     1 ESTMENO(32),C4P(NUMEQT),C5P(NUMEQT),C4(NUMEQT),C5(NUMEQT)

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
         READ(23,*) IGAMMA(I),C0P(I),C1P(I),C2P(I),C3P(I),C4P(I),C5P(I)
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
       XVERIFY(5) = C4P(IEQ)
       XVERIFY(6) = C5P(IEQ)
       CALL VERIFYVAL(6,XVERIFY)
C	 WRITE(25,162) IEQ,C0P(IEQ),C1P(IEQ),C2P(IEQ),C3P(IEQ)
	 WRITE(25,162) IEQ,(XVERIFY(IXV),IXV=1,6)
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
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)

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
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)

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

	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)
	
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
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)
	
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

        write (*,*) "looking at XVERIFY"
C Program gets this far

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


      write (*,*) "Calling MAKEVEC near 3440"

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
     1  CALCRF,0,ICONV,NITER,ICNT,NUMEQT,YO,C0,C1,C2,C3,C4,C5,GAMMA
     2  ,JSUB,IG,INTLIST)

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
       CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)
	
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
	 CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)

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
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)

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
	CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)

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
       XVERIFY(5) = C4P(IEQ)
       XVERIFY(6) = C5P(IEQ)
       CALL VERIFYVAL(4,XVERIFY)
C	 WRITE(28,162) IEQ,C0P(IEQ),C1P(IEQ),C2P(IEQ),C3P(IEQ)
	 WRITE(28,162) IEQ,(XVERIFY(IXV),IXV=1,6)
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
	 CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)
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
	 CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)
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
     1      ,C4P(IEQ),C5P(IEQ)
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
	SUBROUTINE FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)

C  FILRED IS CALLED BY MAIN TO READ THE PORTION OF 
C  SCRATCH FILE 27 WHICH APPLIES TO THE SUBJECT UNDER CONSIDERATION. THE

C  'POINTER' FOR FILE 27 IS IN THE PROPER POSITION TO BEGIN READING THE
C  INFO FOR THE DESIRED SUBJECT.

        IMPLICIT REAL*8(A-H,O-Z)

        PARAMETER(MAXNUMEQ=7)

        DIMENSION TIM(594),SIG(5000),RS(5000,34),YO(594,NUMEQT),
     1  BS(5000,7),C0(NUMEQT),C1(NUMEQT),C2(NUMEQT),C3(NUMEQT),
     2  C4(NUMEQT),C5(NUMEQT),YOO(594,MAXNUMEQ)

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


C wmy2019Dec17 Merging code w/no support for Poisson or other distributions
        DO IEQ = 1,NUMEQT
         READ(27,*) C0(IEQ),C1(IEQ),C2(IEQ),C3(IEQ)
         C4(IEQ) = 0
         C5(IEQ) = 0
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
     1  C4,C5,GAMMA,JSUB,IG,INTLIST)
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
     2 C4(NUMEQT),C5(NUMEQT),W(MAXNUMEQ),GAMMA(NUMEQT)

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
       CALL FILRED(NOBSER,YO,C0,C1,C2,C3,C4,C5,NUMEQT,INTLIST)

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
     1  C4,C5,GAMMA,JSUB,IG,INTLIST)

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
     1  C0(NUMEQT),C1(NUMEQT),C2(NUMEQT),C3(NUMEQT),C4(NUMEQT),
     2  C5(NUMEQT),GAMMA(NUMEQT)

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
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,GAMMA,
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
        CALL FUNC(N,START,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,GAMMA,
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
        CALL FUNC(N,PSTAR,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     1    GAMMA,JSUB,IG,INTLIST) 
        YSTAR=FN
        ICOUNT=ICOUNT+1
        IF(YSTAR.GE.YLO) GO TO 12
        IF(ICOUNT.GE.KCOUNT) GO TO 19
C
C  SUCESSFUL REFLECTION SO EXTENSION.
C
        DO 9 I=1,N
9       P2STAR(I)=ECOEFF*PSTAR(I)+(1.0D0-ECOEFF)*PBAR(I)
        CALL FUNC(N,P2STAR,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     1    GAMMA,JSUB,IG,INTLIST) 
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
        CALL FUNC(N,P2STAR,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     1    GAMMA,JSUB,IG,INTLIST) 
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
        CALL FUNC(N,XMIN,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,
     1   GAMMA,JSUB,IG,INTLIST)
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
        CALL FUNC(N,XMIN,FN,NUMEQT,YO,C0,C1,C2,C3,C4,C5,GAMMA,
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



