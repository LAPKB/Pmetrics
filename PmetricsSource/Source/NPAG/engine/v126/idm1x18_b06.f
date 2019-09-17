c-----------------------------------------------------------------------
c  idm1x18.f                                               9/09/19

C wmy210190909

C Fixed OpenMP/gcc compatibility issues. This is primarily through
C   adding interfaces and moving some subroutines into the npag_utils.f90
C   module. And rewriting the dvode package to f90 standard. Note that
C   NPprep is also edited to support these changes.

C Moved calculation of standard deviation to the end of SUBROUTINE FUNC()
C   to facilitate using multiple probability distributions. The model
C   now supports measurements drawn from Poisson and/or Normal
C   distributions. The error coefficient array is now of length 6.

C npag_utils.mod contains almost all parameters.

C Removed older comments unless they might be useful for resolving
C   potential bugs.

c-----------------------------------------------------------------------
c  idm1x18.f                                               3/11/15

C  Code that this file was edited from.

c-----------------------------------------------------------------------

c  idm1x17.f                                               12/05/14

c  First OpenMP compatible release.

c-----------------------------------------------------------------------

c  idm1x16.f                                               7/21/14

c  idm1x16 has the following changes from idm1x15:

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

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c  idm1x11.f                                               5/25/12

c  idm1x11 has the following changes from idm1x10:

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

C-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c-----------------------------------------------------------------------

C-----------------------------------------------------------------------

c-----------------------------------------------------------------------

c----------------------------------------------------------------------C
      SUBROUTINE IDPC(JSUB,IG,NPX,PX,NBCOMP,SUMSQJ,NOBSER,NUMEQT,
     1   NDIM,MF,RTOL,ATOL,TIM,SIG,YO,RS,BS,
     2   INTLIST,IPAR,ObsError,RPAR,ERRFIL) 

C  INPUT ARE: 

C  PX(I) = Ith COORDINATE OF THE (IGth) GRID POINT OF LENGTH NPX, 
C   parsed by SUBROUTINE MAKEVEC(), and

C  Information about the model, and how to solve it: IF NDIM.eq.-1 then
C   ANAL3, else DIFFEQ, and IF DIFFEQ then MF, RTOL, ATOL are DVODE
C   parameters. Note that a model may also require COVariate information
C   describing the particular subject, which is stored in RS (see I/O).
C   And,

C  Information about the I/O. Size of observations. NOBSER are the
C   total no. of measurements, while NUMEQT is the number of types of
C   measurements, i.e. output equations in SUBROUTINE OUTPUT. Inputs
C   are at time TIM, observations are at times SIG, and actual values
C   of both are in RS, BS, and YO. NBCOMP(.) are the Bolus compartment
C   numbers. And,

C  INFORMATION ABOUT THE JSUBth SUBJECT, WHICH HAS BEEN READ IN 
C  PREVIOUSLY, by SUBROUTINE FILRED(), some of this information
C  is in RS:
C
C  INTLIST contains:
C  (1) = AGE; (2) = ISEX; (3) = HEIGHT; (4) = IETHFLAG;
C  (5) = NDRUG ! types of input; (6) = NADD (#Additional COVariates);
C  (7) = NI = 2*NDRUG+NADD; (8) = ND;
C  (9) = NUMEQT ! types of output;
C  (10) = M = NOBSER ! total measurements for subject JSUB

C  OUTPUT IS:

C  SUMSQJ = SUM(F^2), FOR THIS SUBJECT, OVER I=1,M x NOS (ACTUALLY THE (I,J)
C   CONTRIBUTION IS IGNORED IF YO(I,J) = -99 --> MISSING VALUE), OF 
C   ((YO(I,J)-H(I,J))/ObsError(I,J))**2, WHERE H(I,J) = PREDICTED VALUE
C   OF THE JTH OUTPUT EQ AT THE ITH OBSERVATION TIME, ASSUMING THE IGTH 
C   GRID POINT, X.

C  ObsError(I,J)

C  And, some values in RPAR that must be passed back up to MAIN to compute
C   the Pr(JSUB|IG), as of 20190909 (Sep 9, 2019), in main:
C      PYJGX(JSUB,IG) = 10**RPAR(k_prod_pr)
C     1        * DEXP(-.5D0*RPAR(k_sum_z_sq))/RPAR(k_sfac)/RPAR(k_ofac)

C Finally, in called routines, IPAR(i_skip_ig) can be set to immediately
C  return to MAIN and PYJGX(JSU,IG) <- 0 and cnotinue to next support point.

C----------- USE
       USE npag_utils, only: maxnumeq,max_m_per_obs
     1   ,max_ODE_params,max_doses,max_ODE_comps,max_RS_J
     2   ,max_input_dim

        IMPLICIT none 

C----------- ARGS
        integer JSUB, IG, NPX
        double precision, dimension(max_ODE_params) :: PX
        integer, dimension(max_input_dim) :: NBCOMP
        double precision SUMSQJ
        integer NOBSER,NUMEQT,NDIM,MF
        double precision RTOL
        double precision, dimension(max_ODE_comps) :: ATOL
        double precision, dimension(max_m_per_obs) :: TIM
        double precision, dimension(max_doses) :: SIG
        double precision, dimension(max_m_per_obs,MAXNUMEQ) :: YO
        double precision, dimension(max_doses,max_RS_J) :: RS
        double precision, dimension(max_doses,max_input_dim) :: BS
        integer, dimension(128), intent(INOUT) :: INTLIST
        integer, dimension(257) :: IPAR
        double precision, dimension(max_m_per_obs,MAXNUMEQ) :: ObsError
        double precision, dimension(257), intent(INOUT) :: RPAR
        character*20 ERRFIL

C  FIND THE SUM OF SQUARES OF DIFFERENCES BETWEEN THE OBSERVED
C  VALUES AND THE PREDICTED VALUES (NORMALIZED BY THE ASSAY
C  VARIANCE OF EACH OBSERVATION) FOR THIS POINT, i.e. \sum z^2.
C  Note: This is calculated w.o.re: for distribution type.

C  If MAIN requires other information to continue calculation
C   (of PYJ) Then these values should be passed up to MAIN via
C    RPAR and IPAR.  The information held in these arrays is
C    noted in  npag_utils.f90

          CALL SUMSQ(JSUB,IG,NPX,PX,NBCOMP,SUMSQJ,NOBSER,NUMEQT,
     1   NDIM,MF,RTOL,ATOL,TIM,SIG,YO,RS,BS,
     2   INTLIST,IPAR,ObsError,RPAR,ERRFIL)

        RETURN
	END
c----------------------------------------------------------------------C
        SUBROUTINE SUMSQ(JSUB,IG,NPX,PX,NBCOMP,SSQ,NOBSER,NUMEQT,
     1   NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     2   INTLIST,IPAR,ObsError,RPAR,ERRFIL) 

C  SUBROUTINE TO EVALUATE THE SUM OF SQUARES OF THE RESIDUAL VECTOR.

       USE npag_utils, only: maxnumeq, max_m_per_obs, max_ODE_params
     1   , max_doses,max_ODE_comps, max_RS_J, max_input_dim
     2   , k_prod_pr, k_sum_z_sq, i_skip_ig, i_do 

C  NOTE THAT F HAS DIMENSION 3564 = max_m_per_obs*6 SINCE IT HAS NOS*M ENTRIES,
C  THE MAX VALUE OF NOS = 6, AND THE MAX VALUE FOR M = 99*6 = max_m_per_obs.

C-------- argument list ----------

C wmy2017Sep11
C NOBSER = M; NUMEQT = NOS; PX = PCOPY
        integer JSUB,IG,NPX,NOBSER,NUMEQT,NDIM,MF
        real*8, dimension(max_ODE_params) :: PX
        integer, dimension(max_input_dim) :: NBCOMP
        double precision SSQ, RTOL
        real*8, dimension(max_ODE_comps) :: ATOL
        real*8, dimension(max_m_per_obs) :: TIMCOPY
        real*8, dimension(max_doses) :: SIGCOPY
        real*8, dimension(max_m_per_obs,MAXNUMEQ) :: YO
        real*8, dimension(max_doses,max_RS_J) :: RSCOPY
        real*8, dimension(max_doses,max_input_dim) :: BSCOPY
        integer, dimension(128), intent(INOUT) :: INTLIST
        integer, dimension(257) :: IPAR
        double precision, dimension(max_m_per_obs,MAXNUMEQ) :: ObsError
        double precision, dimension(257) :: RPAR
        character*20 ERRFIL

C-------- local variables ----------

c wmy2017Jan05 Variables that will be passed to subroutines need
c   the SAVE and ThreadPrivate attributes; local varbs w/extent
c   limited to this subroutine do not.


C        DIMENSION F(3564)
        double precision, save,
     1    dimension(max_m_per_obs*MAXNUMEQ*MAXNUMEQ) :: F
        integer NUMRES, I, IinF

!$omp Threadprivate(F)

C-------- calculation ----------

        CALL FUNC(JSUB,IG,NOBSER,F,NPX,PX,NBCOMP,
     1   NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     2   INTLIST,IPAR,ObsError,RPAR,ERRFIL)

C--   If IPAR(i_skip_ig).eq.0 then the point (JSUB,IG) is "bad" according to a
c   user defined criterion. Instead of risking a NaN or some other 
c   error, which will lead to bogus results, program exit, or crash,
c   we instead pass the "bad" state as quickly as possible to main
c   and continue calculations as though the P(JSUB|IG)->0. Program
c   will automatically IPAR(i_skip_ig)<-0 if DVODE returns an error.

        if (IPAR(i_skip_ig).eq.0) then
          SSQ = 10.73d99
          return
        endif

C wmy2017Spr18 -- Nelder Mead uses the sum of squared deviations
C  as the simplex vertices
C        NUMRES=NUMEQT*NOBSER
        NUMRES=INTLIST(9)*INTLIST(10)
        SSQ=0.0D0
        DO 10 I=1,NUMRES
 10       SSQ=SSQ+F(I)*F(I)

        RETURN
        END

C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE FUNC(JSUB,IG,M,F,NPX,PX,NBCOMP,
     1   NDIM,MF,RTOL,ATOL,TIMCOPY,SIGCOPY,YO,RSCOPY,BSCOPY,
     2   INTLIST,IPAR,ObsError,RPAR,ERRFIL)

C  FUNCTION TO DETERMINE THE ENTRIES IN F, GIVEN PX.
C  F(:) returns as (Yobser - Ypredicted)/ObsError

C INTLIST(1) = int(AGE)
C INTLIST(2) = ISEX
C INTLIST(3) = int(HEIGHT)
C INTLIST(4) = IETHFLAG
C INTLIST(5) = /CNST2/NDRUG
C INTLIST(6) = /CNST2/NADD = No. Additional Covariates
C INTLIST(7) = /CNST/NI = 2*NDRUG+NADD
C INTLIST(8) = /CNST/ND ; ND =  NDO
C INTLIST(9) = /CNST2/NOS (Called /CNST2/NUMEQTT in SUBROUTINE FILRED)
C INTLIST(10) = /CNST2/M = NOBSER

C USE can not follow implicit or include statements
       USE npag_utils, only: verifyval, shift, thesame, do140
     1   , predlast3, maxnumeq, max_m_per_obs, max_SS_doses
     2   , max_ODE_params,max_doses,max_ODE_comps,max_RS_J
     3   , max_input_dim, k_gamma, k_flat, k_sfac, k_ofac
     4   , k_sum_z_sq, k_prod_pr, k_dvode_reserved, k_p_end
     5   , k_ig, k_jsub, i_ig, i_jsub, k_c0_base, k_c1_base
     6   , k_c2_base, k_c3_base, k_c4_base, k_c5_base
     7   , k_resolve, i_errmod
     8   , i_misval, i_skip_ig, i_is_log10, i_Nnormalobs
     9   , i_is_poisson_obs, i_Npoissonobs, i_dvode_reserved

      IMPLICIT none
C REAL*8(A-H,O-Z)

C---------- USE LIST ---------------------------------------------------
C wmy2017NOv29 -- OpenMP hates COMMON blocks; instead, pass arguments
C  in and/or make use of interfaces.  If you can't, then use
C  modules. If all of this is impossible, try to SAVE + !$omp Threadprivate
C  to put a private copy for each thread on the heap.

C M = NOBSER
C N = NDIM
C ND = No. Dose Events
C NI = 2*NDRUG + NADD
C NUP and NUIC are not used anywhere in program
C NP = NVAR+NOFIX ; ND read in from 27 by filred

C note: Verified that /CNST/N == NDIM by making bogus
C  models w/XP(N); N = 7,6,11,etc. ... N == NDIM always

C wmy2019.03.12 -- SR SHIFT moved into npag_utils.f90
C       include "interface_0SHIFT.txt"

! NEW PARALLEL CODE BELOW AS OF npageng28.f.
C !$omp Threadprivate(/PARAMD/,/INPUT/,/STATE/,RSO,BSO,SIGO,Y)   
C !$omp Threadprivate(/CNST2/,/STATE/,/CNST/,/OBSER/,/BOLUSCOMP/)
C !$omp Threadprivate(/BOLUSCOMP/,/OBSER/,/CNST/,/CNST2/)
C !$omp Threadprivate(/BOLUSCOMP/)

C      DIMENSION NBCOMP(7)
C      ,TIM(max_m_per_obs),SIG(max_doses),RS(max_doses,max_RS_J),
C     1 YO(max_m_per_obs,MAXNUMEQ), BS(max_doses,max_input_dim)
C      , STDEV(max_m_per_obs,MAXNUMEQ)

C     integer N,ND,NI,NUP,NUIC,NP
C     integer NPL,NOS,NDRUG,NADD

      CHARACTER ERRFIL*20

C---------- ARGUMENT LIST ---------------------------------------------
       integer JSUB,IG,M,NPX,NDIM, MF
       doubleprecision, dimension(max_m_per_obs*MAXNUMEQ*MAXNUMEQ) :: F
       doubleprecision RTOL
       real*8, dimension(max_ODE_comps) :: ATOL
       real*8, dimension(max_ODE_params) :: PX
       integer, dimension(max_input_dim) :: NBCOMP
       real*8, dimension(max_m_per_obs) :: TIMCOPY
       real*8, dimension(max_doses) :: SIGCOPY
       real*8, dimension(max_m_per_obs,MAXNUMEQ) :: YO
       real*8, dimension(max_doses,max_RS_J) :: RSCOPY
       real*8, dimension(max_doses,max_input_dim) :: BSCOPY
       integer, dimension(128), intent(INOUT) :: INTLIST
       integer, dimension(257) :: IPAR
       double precision, dimension(max_m_per_obs,MAXNUMEQ) :: ObsError
       double precision, dimension(257) :: RPAR

C !$omp ThreadPrivate( IPAR, ObsError )

C ----- Local Variables -----------------------------------------------

       double precision UseNormal, P_thresh
       double precision YOBSERVED, YMEAN, T, TOUT, DOSEINT
       integer I, J, ICONV, ID, III, ISAME, ISTEADY
       integer KNTM1, N, NN, NSET, NTL

C ----- ------ SAVEd

       double precision, save, dimension(max_m_per_obs,MAXNUMEQ) :: Y
       double precision, save, dimension(max_ODE_comps) :: X

! ADDED BSO(.,.) AS OF idm1x17.f.
       real*8, save, dimension(max_m_per_obs) :: TIMO
       real*8, save, dimension(max_doses) :: SIGO
       real*8, save, dimension(max_doses,max_RS_J) :: RSO
       real*8, save, dimension(max_doses,max_input_dim) :: BSO

! NEW PARALLEL CODE BELOW AS OF npageng28.f.
!$omp Threadprivate(RSO,BSO,SIGO,TIMO,Y,X)

       integer NDO 
       integer, save :: IKNS,KNS,KNT,KNSOLD
!$omp Threadprivate(IKNS,KNS,KNT,KNSOLD)

C ----- ------ Implicit Private

       double precision, dimension(maxnumeq) :: YT
       double precision, dimension(max_input_dim) :: TLAG
C DIMENSION YT(MAXNUMEQ),TLAG(max_input_dim)

       double precision, dimension(100) :: XVERIFY
       double precision, dimension(max_SS_doses,max_ODE_comps) :: XSTORE
       double precision, dimension(max_ODE_comps) :: XPRED, B, BCOPY
       double precision, dimension(max_RS_J) :: R, RCOPY

       integer JUMPTO45

       real*8, dimension(max_input_dim) :: FA
       integer ISKIPBOL

       integer NNORMALOBS, NPOISSONOBS, MISVAL
       real*8 SIGFAC,OFAC,ZSCORE

       double precision poissonprob

       integer UseInlineDO140
       UseInlineDO140 = 0

C  NOTE THAT AS OF idm1x15.f, THE DIMENSIONS OF 6 IN XSTORE AND XPRED
C  HAVE BEEN CHANGED TO max_ODE_comps, WHICH IS WHAT THEY SHOULD HAVE BEEN ALL
C  ALONG (I.E., THE SAME AS FOR X).

C  NOTE THAT THE 2ND DIMENSION OF STDEV AND YO IS MAXNUMEQ, WHICH
C  IS SET IN THE NEW PARAMETER STATEMENT ABOVE.
C  NOTE THAT THE DIMENSIONS RELATED TO THE NO. OF OUTPUT EQS. IN
C  YO, YT, STDEV, AND Y ARE CHANGED TO MAXNUMEQ (FROM 6). NUMEQT COULD
C  NOT BE USED BECAUSE THESE ARRAYS WERE NOT PASSED TO THIS ROUTINE AS
C  DUMMY ARGUMENTS.

C-----------------------------------------------------------------------

C /CNST/N reassigned to argument NDIM -- They should be equal anyways
       N = NDIM

C  NOTE THAT "7" IN THE ABOVE ARRAYS INDICATE THE NO. OF DRUGS ALLOWED.

C  NOTE THAT F HAS DIMENSION 3564 = max_m_per_obs*6 SINCE IT HAS NOS*M ENTRIES,
C  THE MAX VALUE OF NOS = 6, AND THE MAX VALUE FOR M = 99*6 = max_m_per_obs.
C
C  wmy20190820 -- each equation (maxnumeq) can be sampled max_m_per_obs
C    times, and each of those times can be unique, so that an observation
C    might be quite sparse, w/only one value and maxnumeq-1 zeroes or -99s.
C    on each row.

C  R(7) CHANGED TO R(max_ODE_comps) <-- No. of 'rate inputs'
C  B(3) CHANGED TO B(max_ODE_comps) <-- No. of different bolus inputs
C  CHANGED X(3) TO X(max_ODE_comps) <-- No. of compartments
C  IC(10) CHANGED TO IC(max_ODE_comps) <-- Initial conditions in compartments;
C 		should have been changed to 20 previously (like X,B).
C  NBCOMP(10) CHANGED TO NBCOMP(max_ODE_comps) <-- Same remarks as for IC.
C  P(10) CHANGED TO P(max_ODE_params) <-- No. of parameters

C*****ODE CONSTANTS AND INITIALIZATION*****

C wmy2017Sep25 - Ensure X is not random.
      do III=1,max_ODE_comps
         X(iii) = 0.0D0
      end do
c wmy2017Nov24 - Copy COMMON/OBSER/TIM(:) to SAVEd ThreadPrivate local TIMO(:)
      do III=1,max_m_per_obs
         TIMO(III)=TIMCOPY(III)
      end do

c      if (T.eq.0) then
c        write (*,*) JSUB,IG,"KNS,SIGO(1...)",
c     1    KNS,SIG(1),SIG(2),SIG(3),SIG(4),
c     2    KNT,TIMO(1),TIMO(2),TIMO(3),TIMO(4)
c      endif

      KNS=1
      KNT=1

C      write (*,*) "KNS and KNT init",KNS,KNT

C  NOTE THAT KNT IS THE RUNNING INDEX OF THE NEXT OBSERVATION TIME,
C  AND       KNS IS THE RUNNING INDEX OF THE NEXT DOSAGE TIME.

      T=0.0D0

C  INITIALIZE ISKIPBOL = 0. SEE CODE BELOW. IT IS ONLY NEEDED FOR A
C  STEADY STATE DOSE SET WHICH HAS BOLUS DOSES.

      ISKIPBOL = 0

c wmy2017Oct01
C      DO I = 1,NDRUG
      DO I = 1,INTLIST(5)
       R(2*I-1) = 0.D0
      END DO


!  AS OF idm1x17.f, ESTABLISH BSO(.,.), AND THEN USE BSO,RSO,SIGO,
!  AND NDO RATHER THAN BS,RS,SIG, AND ND FOR ALL CODE BELOW.

c wmy2017Oct01
C      DO I=1,ND
C       DO J=1,NDRUG
      DO I=1,INTLIST(8)
       DO J = 1,INTLIST(5)
        BSO(I,J)=RSCOPY(I,2*J)
C wmy2017Sep26
C        if (RSCOPY(I,2*J) .ne. RS(I,2*J)) then
C           write (*,*) "BSO init :: ERROR: RS.ne.RSCOPY for",
C     1     "(JSUB,IG)(KNT,KNS;T)",Jsub,IG,KNT,KNS,T
C        endif
       END DO
      END DO

c  AS OF idm1x7.f, instead of R(1) = 0, the code has been changed to 
c  set R(2*I-1) = 0, for I = 1,NDRUG. I.E., All IV rates for all NDRUG
c  drugs are initialized to be 0 ... in case the 1st obs. time is 0,
c  which means that OUTPUT is called before the R(I) are set below.


C  CALL SUBROUTINE GETFA IN npemdriv.f (THE FIRST TEMPLATE FILE TO 
C  INCLUDE GETFA IS TSTMULTG.FOR) TO OBTAIN THE VALUE OF FA FOR EACH
C  OF THE NDRUG DRUGS.

C  AS OF idm1x13.f, BEFORE CALLING GETFA, MUST SET
C  THE R(.) IN CASE ANY OF THE FA(.) ARE FUNCTIONS OF THE 
C  COVARIATES WHICH ARE ESTABLISHED FROM THE R(.) VALUES IN
C  GETFA.

c 2017Oct01 
C      DO I=1,7  ! NI should be equal to 7
c      DO I=1,NI
      DO I=1,INTLIST(7)
C wmy2017Sep30 -- replace /INPUT/R
       R(I)=RSCOPY(KNS,I)
       RCOPY(I)=RSCOPY(KNS,I)
C wmy2017Sep26
C        if (RSCOPY(KNS,I) .ne. RS(KNS,I)) then
C           write (*,*) JSUB,IG, "R init :: RS.ne.RSCOPY for KNS=", KNS
C        end if
      END DO
C
C wmy2017Sep29 -- Fixed this bug
C   (1) RSCOPY is fine at previous write, but here,
C       R(I) = 0.0
C   (2) For last thread, NI = 7, for the others NI = 0
C

C        write (*,*) "DO 800", JSUB,IG,"In FUNC RCOPY :: ",
C     1    NI, RCOPY(1), RCOPY(2), RCOPY(3), RCOPY(4),
C     2    RCOPY(5), RCOPY(6), RCOPY(7),
C     2    "Calling GETFA"
C        write (*,*) "DO 800", JSUB,IG,"In FUNC R :: ",
C     1    NI, R(1), R(2), R(3), R(4),
C     2    R(5), R(6), R(7),
C     2    "Calling GETFA"

c wmy2017Nov03 -- debug DEBUG
c         write (*,*) "Calling GETFA()"

c      CALL GETFA(FA,X)
C	 CALL GETFA(FA,X,P,R,B,INTLIST)
	 CALL GETFA(FA,X,PX,R,B,INTLIST)

c wmy2017Nov03 -- debug DEBUG
c         write (*,*) "Returned from GETFA()"

C  NOTE THAT NBCOMP(I),I=1,NDRUG WAS SET IN SUBROUTINE SYMBOL AND
C  PASSED TO THIS ROUTINE VIA COMMON/BOLUSCOMP.

C  As of idm1x12.f, the code to save ND0, SIGO, RSO, is moved to before
c  the IF(N .EQ. 0) GO TO 75  statement. The reason is that before this
c  routine returns, ND, SIG, and RS are reset back to these values,
c  even if N = 0, and so they must be established at this time.

C  AS OF idm1x9.f, SAVE ND, SIG, AND RS WHETHER OR NOT NTL = 1, SINCE
C  IF THERE ARE STEADY STATE DOSE SETS, THE FIRST SIG(.) VALUE IN EACH
C  SET WILL BE CHANGED TO BE 0 BELOW.

C wmy2017.07.11 debug
c         write (*,*) "Enterred FUNC: RSO and SIGO init", JSUB, IG,
c     1    NDO, NI, INTLIST(8),intlist(7)
C
C wmy2017Dec01 -- This was the bug at the center of the last 3 months
C  of troubles! ND is 0 for all spawned threads.
C
C      if (ND .ne. intlist(8)) then
C	 write (*,*) JSUB,IG,"ERROR: ND.NE.intlist(8)",ND,intlist(8)
C      end if

         NDO = intlist(8)

C         DO I=1,NDO
C          DO J=1,NI
         DO I=1,INTLIST(8)
          DO J=1,INTLIST(7)
           RSO(I,J) = RSCOPY(I,J)
          END DO
          SIGO(I) = SIGCOPY(I)
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

C------------------- This Block replaced w/single line that follows
C C        IF(N.EQ. 0) GO TO 75 ! Note that /CNST/N is undefined
C        IF(NDIM .EQ. 0) GO TO 75

C  CALL SUBROUTINE GETIX IN npemdriv.f (THE FIRST TEMPLATE FILE TO 
C  INCLUDE GETIX IS TSTMULTG.FOR) TO OBTAIN THE VALUE OF X (THE INITIAL
C  COMPARTMENT AMOUNT) FOR EACH OF THE N COMPARTMENTS.


c wmy2017Oct02
C	 CALL GETIX(N,X)
C          CALL GETIX(NDIM,X,P,R,B,INTLIST)

C  CALL SUBROUTINE GETTLAG IN npemdriv.f (THE FIRST TEMPLATE FILE TO 
C  INCLUDE GETTLAG IS TSTMULTG.FOR) TO OBTAIN THE VALUE OF THE TIMELAG
C  FOR EACH OF THE NDRUG DRUGS.

C---------------------- End of block replaced by single line below

C        IF(NDIM .NE. 0) CALL GETIX(NDIM,X,P,R,B,INTLIST)
        IF(NDIM .NE. 0) CALL GETIX(NDIM,X,PX,R,B,INTLIST)

C debug
C        if (NDIM.eq.0) write (*,*) "DEBUG 2018.06.15: NDIM.eq.0"

C--------------------------------------------------------------------

C   75	 CALL GETTLAG(TLAG,X)
C   75	 CALL GETTLAG(TLAG,X,P,R,B,INTLIST)
   75	 CALL GETTLAG(TLAG,X,PX,R,B,INTLIST)

C  IF ANY TLAG(.) VALUES RETURN AS .NE. 0, THEN, CALL SUBROUTINE SHIFT
C  TO ADJUST THE DOSAGE REGIMEN APPROPRIATELY.

      NTL = 0
C      DO ID = 1,NDRUG
      DO ID = 1,INTLIST(5)
       IF(TLAG(ID) .NE. 0) NTL = 1
      END DO

        IF(NTL .EQ. 1) THEN

C  STORE INCOMING VALUES IN ND, SIG, AND RS (WHICH CONTAINS BS VALUES)
C  SINCE THEY WILL BE CHANGED IN THE CALL TO SUBROUTINE SHIFT, WHICH 
C  "SHIFTS" THE DOSAGE REGIMEN MATRIX TO ACCOUNT FOR THE TIMELAG 
C  PARAMETER(S), TLAG(I). AT THE END OF THIS ROUTINE, THE VALUES IN ND, 
C  SIG, AND RS WILL BE RESET TO THEIR INCOMING VALUES - TO BE READY FOR 
C  THE NEXT CALL TO THIS ROUTINE WITH POSSIBLY DIFFERENT VALUES FOR 
C  TLAG(I). NO! AS OF idm1x17.f, THE VALUES OF ND,SIG,RS,BS ARE 
C  NOT CHANGED; INSTEAD NDO,SIGO,RSO,BSO ARE USED.


C         CALL SHIFT(TLAG,NDO,SIGO,NDRUG,NADD,RSO)
          CALL SHIFT(TLAG,NDO,SIGO,INTLIST(5),INTLIST(6),
     1      RSO,INTLIST)

C
C         CALL SHIFT(TLAG,INTLIST(8),SIGO,INTLIST(5),INTLIST(6),RSO)
C
C          if (INTLIST(8) .ne. NDO) then
C              write (*,*) "IL(8) vs NDO", INTLIST(8), NDO
C              INTLIST(8) = NDO
C          endif 
C
C NDO is incremented by 1 after shift() -- this is to accomodate the
C   NULL stimulus at T=0.
C
C wmy2018Jul17 BUG: Any write to INTLIST(8) inside FUNC() causes the
C   value in INLIST(8) to be incremented to absurdly high values, as soon
C   as the value reaches above 5000 the program exits. NDO is also
C   incremented b/c it is set to INTLIST(8) near top of FUNC().
C   Therefore, can't pass INTLIST(8) into SHIFT(), and can't increment
C   INTLIST(8) to new NDO value after call to SHIFT().
C   Tried:
C     Attributing intent(INOUT) to INTLIST in FUNC(), SUMSQ(), NPAG(), etc.
C     Removing SAVE attribute from NDO -- Note that NDO still retains 
C       value between calls to FUNC()
C
C debug
C          write (*,*) "DEBUG 2018.06.15 Called SHIFT(): TLAG,NDO,NDRUG,N
C     1ADD,NI,ND,NOS,M",TLAG(1),NDO,INTLIST(5),INTLIST(6),INTLIST(7)
C     2,INTLIST(8),INTLIST(9),INTLIST(10)
C          write (*,*) "SIGO", sigo(1),sigo(2),sigo(3),sigo(4),sigo(5),
C     1       sigo(6), sigo(7)
c
c wmy2017Jul13 -- shift() seems to return correct values in SIGO
c
c

C  ESTABLISH THE VALUES IN EACH DRUG'S PO COLUMN TO THE CORRESPONDING
C  COLUMN IN ARRAY BSO.

C          if (NDO.ne.INTLIST(8)) write (*,*) "shift: NDO.ne.I(8)",
C     1         NDO, INTLIST(8)

          DO I=1,NDO
C           DO J=1,NDRUG
C          DO I=1,INTLIST(8)
            DO J=1,INTLIST(5)
              BSO(I,J)=RSO(I,2*J)
            END DO
          END DO
C      write (*,*) "BSO(NDO,IL(5))",BSO(NDO,INTLIST(5)),NDO,INTLIST(5)


        ENDIF
C  THE ABOVE ENDIF IS FOR THE  IF(NTL .EQ. 1)  CONDITION.


C------- Prepare to loop through times in TIMO(:) and SIGO(:)
C
C wmy2017Nov27 -- at this point, TNS and TNT are at their initial
C   values: TNS=TNT=1
C
C-----------------------------------------------------------------------

        JUMPTO45 = 0
C
C wmy2017Nov27 -- This block checks for 3 conditions, if any are true,
C  then the calculation will jump to label 45.
C
C  [1] If (next obs time < next stimulus time) then JUMPTO45; also,
C     if (next obs time == 0.0) then calculate the output before
C     making the jump.
C  Else
C  [2] If (next obs time == next stimulus time) then JUMPTO45;
C     also, if (next obs time == 0.0) then calculate the output
C     before making the jump.  And,
C  [3] If (next stimulus time > 0.0) JUMPTO45.
C
C Note: Conditions [1] and [2] can be combined to <=, rather than two
C  conditions.
C Note: The code between the end of this block and label 45 checks if a
C  SS calculation is desired at TOUT=0. If we know that the next stimulus
C  time is > 0, then we can skip that logic, i.e. jump to label 45.
C

C        IF(TIMO(KNT).GE.SIGO(KNS)) GO TO 12
        IF(TIMO(KNT).LT.SIGO(KNS)) then

C          IF(TIMO(KNT).NE.0.0D0) GO TO 45
          IF(TIMO(KNT).eq.0.0D0) then

C  THE ONLY WAY THE FOLLOWING CALL TO OUTPUT CAN OCCUR IS IF TIM(KNT)
C  = 0 --> OBTAIN YT = OUTPUT VALUE(S) AT TIME 0.0.

            CALL OUTPUT(0.D0,YT,X,RPAR,IPAR)
C            DO 2000 I=1,NOS
            DO 2000 I=1,INTLIST(9)
2000          Y(KNT,I)=YT(I)
            KNT=KNT+1
C           GO TO 45
          ENDIF

          JUMPTO45 = 1

        ELSE

C 12        IF(TIMO(KNT).GT.SIGO(KNS)) GO TO 13
C 12        IF(TIMO(KNT).LE.SIGO(KNS)) then   ! Can't be <, so must be .GE.
12        IF(TIMO(KNT).EQ.SIGO(KNS)) then

C            IF(TIMO(KNT).NE.0.0D0) GO TO 45
            IF(TIMO(KNT).EQ.0.0D0) then 

C  THE ONLY WAY THE FOLLOWING CALL TO OUTPUT CAN OCCUR IS IF TIM(KNT)
C  = 0 --> OBTAIN YT = OUTPUT VALUE(S) AT TIME 0.0.

              CALL OUTPUT(0.D0,YT,X,RPAR,IPAR)
C              DO 2005 I=1,NOS
              DO 2005 I=1,INTLIST(9)
2005            Y(KNT,I)=YT(I)
              KNT=KNT+1
            else 
              JUMPTO45 = 1
            end if
          END IF

C 13       IF(SIGO(KNS) .GT. 0.0D0) GO TO 45
13        IF(SIGO(KNS) .GT. 0.0D0) JUMPTO45 = 1

        END IF

      ISTEADY = 0

C wmy2018.06.21 ANAL3() seems to cause KNS to jumpt from 1 to 3 and miss
C the last stimulus. Setting JUMPTO45=1 should make ALL observations
C will jump to 45, skipping next block
C      JUMPTO45 = 1
C setting JUMPTO45=1, above, forces KNS=1, always

      if (JUMPTO45.eq.0) then
C-----------------------------------------------------------------------

C  CHECK TO SEE IF SIGO(KNS) < 0. IF SO, IT MEANS THAT 100 STEADY STATE
C  DOSES SHOULD NOW BE APPLIED WITH AN INTERDOSE INTERVAL EQUAL TO
C  -SIGO(KNS).

C-----------------------------------------------------------------------
      IF(SIGO(KNS) .LT. 0.D0) THEN


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
C  THE 100TH DOSE SET WILL BE AT TIME 100*(-SIGO(KNS)), SO KNS WILL BE 
C  THE INDEX OF THE FIRST DOSE EVENT WHICH OCCURS AFTER THIS TIME.

C  IF "CONVERGENCE" IS NOT ACHIEVED, CONTINUE APPLYING THE LOGIC OF
C  PREDLAST3 UNTIL IT IS ACHIEVED, OR UNTIL THE 100 DOSE SETS ARE ALL
C  INTEGRATED THROUGH, WHICHEVER COMES FIRST.

        DOSEINT = -SIGO(KNS)

C  RESET SIGO(KNS) TO BE 0 SINCE THIS DOSE EVENT REPRESENTS THE START
C  OF 100 DOSE SETS THAT BEGIN AT TIME 0.


        SIGO(KNS) = 0

      ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(SIGO(KNS) .LT. 0.D0)  CONDITION.
C-----------------------------------------------------------------------

C      DO I=1,NI
      DO I=1,INTLIST(7)
       R(I)=RSO(KNS,I)
      END DO

C      write (*,*) "Initializing R(:) to RSO(KNS,:)", KNS

C----- GO TO are replaced by standard logic in this block --------------
C
C wmy2017Nov27 If (NDRUG.eq.0) skip this block.
C
C      IF(NDRUG .EQ. 0) GO TO 81
C      IF(NDRUG .NE. 0) then

C      write (*,*) "INTLIST(5)", intlist(5)

      IF(INTLIST(5) .NE. 0) then

C  AS OF idm1x13.f: MUST CALL GETFA BEFORE EVERY TIME THAT
C  FA(.) ARE USED IN CASE THE EQUATION(S) FOR THE FA(.) ARE BASED
C  ON THE COVARIATES, WHICH CAN CHANGE DOSE TO DOSE.

C         CALL GETFA(FA,X)
C        CALL GETFA(FA,X,P,R,B,INTLIST)
        CALL GETFA(FA,X,PX,R,B,INTLIST)

C        IF(N .EQ. 0) GO TO 120
C        IF(NDIM .EQ. 0) GO TO 120
        IF(NDIM .NE. 0) then

C          DO I=1,NDRUG
          DO I=1,INTLIST(5)
            X(NBCOMP(I))=X(NBCOMP(I))+BSO(KNS,I)*FA(I)

C            write (*,*) JSUB,IG,T,"Bolus to",NBCOMP(I),X(NBCOMP(I)),KNS

          END DO

C  NOTE THAT FA(I) IS THE FRACTION OF DRUG AVAILABLE FROM A BOLUS INPUT
C  FOR DRUG I INTO ITS ABSORPTIVE COMPARTMENT.

C        GO TO 81 ! All branches merge into statement 81

        else

C120       DO I=1,NDRUG
120       DO I=1,INTLIST(5)
            B(I)=BSO(KNS,I)*FA(I)
          END DO
        end if
      end if
C-----------------------------------------------------------------------

81    KNS = KNS+1

C      write (*,*) "AT 81", JSUB,IG,T,KNS

C Commenting out the update at 81 seems to cause ALL ++KNS to be skipped!?


C-----------------------------------------------------------------------

      ENDIF
C
C wmy2017Nov28 -- IF (JUMPTO45.eq.1), we skipped the above blocks and
C   came to the above ENDIF (JUMPTO45.eq.0).
C

C  DETERMINE IF, OBSER(ID=0), OR DOSE(ID=1), OR BOTH(ID=2).

! NEW PARALLEL CODE BELOW AS OF npageng28.f

!
!45    IF(KNS .GT. NDO) GO TO 15
! Replaced by below

 45   Continue

C
C
C --- ----- ------- INTEGRATION LOOP -----------------------------------
C
C TODO Convert GO TO logic to DO {from label 46 to label 40} WHILE {KNT .LE. M}
C   note: INTLIST(10)=M

C 46   IF(KNS .GT. NDO) GO TO 15
C 46   IF(KNS .GT. INTLIST(8)) GO TO 15

C 46   IF(KNS .GT. INTLIST(8)) then
 46   IF(KNS .GT. NDO) then
C   IF(KNS .GT. INTLIST(8)) then ! wmy2018Jul13 -- This seems to have
C   fixed ./PmetricsExamples/Run 1/ -- I believe this is because shift()
C   receives NDO and increments by 1; but for some reason INTLIST was 
C   not threadsafe (?) or some other issue -- working on this now! 7/16/18

C
C Label 46 begins four conditional computations that each end with a 
C forced decision to GO TO 31 or 30. The only significant entanglement of
C GO TO computations turned out to be Block 15, which we untangled by
C moving a copy here, in a previous hardening session.  So we are now
C in a position encompass all of the calculation from Label 46 to Label
C 30 inside a single IF/THEN logical structure:
C
C 46   IF(KNS .GT. INTLIST(8)) then
C        { 15 }
C      else
C        IF () THEN
C          {unlabeled block}
C        ELSE
C          {IF () THEN 15 ELSE 25 ENDIF}
C        endif
C      ENDIF 
C      Continue to 30 or Skip to 31
C------------------------------------------------------ Copy of Block 15
C
C wmy2017Dec06 See notes in Block 15, below
C
        ID=0
        TOUT=TIMO(KNT)
        KNT=KNT+1

C        write (*,*) "KNT++ at first 15 block", JSUB,IG,T,TOUT,KNT

C        IF(NDIM .EQ. 0) GO TO 31
C        GO TO 30
C----------------------------------------------- End of Copy of Block 15
C      ENDIF
      ELSE

C CODE CHANGE BELOW FOR idm1x8.f.
      IF(TIMO(KNT) .EQ. 0.D0 .AND. KNT .GT. 1) THEN

C  AS OF idm1x7.f, A TIME RESET NO LONGER REQUIRES ALL INITIAL
C  COMPARTMENT AMOUNTS TO BE RESET TO 0. THIS IS BECAUSE A TIME RESET
C  NO LONGER HAS TO MEAN THAT AN "INFINITE" AMOUNT OF TIME HAS OCCURRED
C  WITH NO DOSING; IT CAN ALSO NOW MEAN THAT AN "INFINITE" AMOUNT OF 
C  TIME HAS OCCURRED WITH UNKNOWN DOSING (IN THIS CASE, SUBROUTINE
C  GETIX WILL BE CALLED BELOW TO ESTABLISH INITIAL CONDITIONS FOR THIS
C  TIME PERIOD). 

C  ADVANCE KNS TO THE NEXT VALUE THAT HAS SIGO(KNS) .LE. 0. I.E., ONCE
C  TIMN(KNT) = 0, IT MEANS THAT WE ARE DONE WITH THE OUTPUT OBS.
C  TIMES IN THE PREVIOUS SECTION --> THERE IS NO POINT IN CONTINUING
C  TO INTEGRATE TILL THE END OF THE DOSES IN THE PREVIOUS SECTION
C  (IF THERE ARE ANY).

C----------------------------------------------------- AAAAA
C        DO IKNS = KNS,INTLIST(8)
C          IF(SIGO(IKNS) .LE. 0.D0) GO TO 110
C        END DO
C        stop program
C  110   KNS = IKNS
C
C --- wmy2017Dec05 --- Above logic is replaced with
C
C        DO IKNS = KNS,INTLIST(8)
C          IF(SIGO(IKNS) .LE. 0.D0) then ! GO TO 110
C            KNS = IKNS
C            exit
C          endif
C        END DO
C        if (SIGO(IKNS).gt.0.D0) stop program
C  110   Continue ! KNS = IKNS
C-----------------------------------------------------

C       write (*,*) "*** wmy2017Dec05 MvG code validation required."

C        DO IKNS = KNS,INTLIST(8)
C ! see note wmy2018Jul13
        DO IKNS = KNS,NDO
C          IF(SIGO(IKNS) .LE. 0.D0) GO TO 110
          IF(SIGO(IKNS) .LE. 0.D0) then
            KNS = IKNS

C            write (*,*) "110 Somehow got to KNS=IKNS",T,TOUT,IKNS

            exit
          endif
        END DO

C  TO GET HERE MEANS THAT NO VALUE IN SIGO(.) FROM KNS TO NDO HAS A 
C  VALUE .LE. 0, AND THIS IS AN ERROR. IT MEANS THAT THE PATIENT DATA
C  FILE HAS AN OBSERVATION TIME RESET ROW WITHOUT AN ACCOMPANYING
C  DOSE RESET ROW. TELL THE USER AND STOP.

C  REPLACE WRITING OF SIGO() WITH XVERIFY (SEE LOGIC IN SUBROUTINE
C  VERIFYVAL.

C wmy added this line as per comment above in AAAAA
        if (SIGO(IKNS) .gt. 0.D0) then

          XVERIFY(1) = SIGO(KNS)
          CALL VERIFYVAL(1,XVERIFY)

C        WRITE(*,111) NDO,KNS,SIGO(KNS)
C        WRITE(25,111) KNS,SIGO(KNS)
          WRITE(*,111) NDO,INTLIST(8),KNS,XVERIFY(1)
          WRITE(25,111) INTLIST(8),KNS,XVERIFY(1)

 111  FORMAT(//' THE CURRENT SUBJECT HAS AN OBSERVATION TIME RESET'/
     1' ROW WITHOUT AN ACCOMPANYING DOSE RESET ROW. THE PROGRAM NOW'/
     2' STOPS. '//
     3' REVIEW YOUR PATIENT FILES AND CORRECT THE ERROR.'//
     4' NOTE THAT THE ',I4,' DOSE TIMES (POSSIBLY ALTERED BY TIMELAGS'/
     5' ARE THE FOLLOWING (AND THERE IS NO TIME .LE. 0 AFTER TIME'/
     6' NO. ',I4,' WHICH HAS CORRESPONDING TIME ',F15.4,'):')

          OPEN(42,FILE=ERRFIL)

C          WRITE(42,111) NDO,KNS,SIGO(KNS) 
          WRITE(42,111) INTLIST(8),KNS,XVERIFY(1) 

C          DO I = 1,INTLIST(8)
C ! see note wmy2018Jul13
          DO I = 1,NDO
            WRITE(*,*) SIGO(I)
            WRITE(25,*) SIGO(I)
            WRITE(42,*) SIGO(I)
          END DO

          CLOSE(42)

          CALL PAUSE
          STOP

        endif
C wmy Above endif as per note AAAAA; KNS is already updated, so
C  comment out label 110, which is no longer needed
C  110   KNS = IKNS

C  THERE ARE TWO POSSIBILITES AT THIS POINT, EITHER SIGO(KNS) = 0
C  OR SIGO(KNS) < 0. 

C  IF SIGO(KNS) = 0, THIS REPRESENTS A TIME RESET (T WILL BE SET = 0
C  BELOW) WITH A SINGLE DOSE LINE TO START. IN THIS CASE, CALL GETIX
C  AGAIN (JUST AS WAS DONE NEAR THE TOP OF THIS ROUTINE) TO OBTAIN
C  INITIAL COMPARTMEN AMOUNTS. NOTE THAT BY DEFAULT, IN GETIX, ALL
C  COMPARTMENT AMOUNTS ARE SET = 0 (WHICH WOULD BE THE CASE IF IN THE 
C  LONG TIME PERIOD BETWEEN THE LAST SET OF DOSES AND THIS NEW
C  BEGINNING, NO DOSES HAVE BEEN GIVEN). BUT THE USER MAY ALSO HAVE
C  CODED INTO GETIX EQUATIONS THAT SET ONE OR MORE OF THE X(I) TO
C  FUNCTIONS OF COVARIATE AND PARAMETER VALUES (WHICH WOULD BE THE
C  SITUATION IF AN UNKNOWN DOSING REGIMEN HAS TAKEN PLACE BUT IT
C  DOESN'T MATTER WHAT IT WAS BECAUSE THE PATIENT COMES TO A LAB AND
C  SIMPLY HAS HIS COMPARTMENT VALUES ESTABLISHED BEFORE CONTINUING 
C  WITH THE OTHER VALUES IN HIS PATIENT FILE). 

C  IF SIGO(KNS) < 0, THIS REPRESENTS A TIME RESET WITH A STEADY STATE
C  SET OF 100 DOSES ABOUT TO BEGIN. IN THIS CASE, WE ASSUME THAT THE
C  PATIENT IS ABOUT TO GET 100 SETS OF DOSES SO THAT HIS COMPARTMENT
C  AMOUNTS WILL ACHIEVE STEADY STATE VALUES. THESE STEADY STATE VALUES
C  WILL BE ESTIMATED IN THE BLOCK OF CODE BELOW THAT STARTS WITH 
C  IF(ISTEADY .EQ. 1). IN THIS CASE, WE WILL STILL CALL GETIX TO 
C  MAKE SURE THAT ANY RESIDUAL COMPARTMENT AMOUNTS FROM A PREVIOUS
C  SET OF DOSES IS ZEROED OUT (OR SET = VALUES AS DETERMINED BY
C  SUBROUTINE GETIX).

C  AS OF idm1x14.f, BEFORE CALLING GETIX, MUST SET
C  THE R(.) IN CASE ANY OF THE INITIAL CONDITIONS FOR THE X(.)
C  ARE FUNCTIONS OF THE COVARIATES WHICH ARE ESTABLISHED FROM THE 
C  R(.) VALUES IN GETFA.
 
C        DO I=1,NI
        DO I=1,INTLIST(7)
          R(I)=RSO(KNS,I)
        END DO



C        CALL GETIX(N,X)
C        CALL GETIX(NDIM,X,P,R,B,INTLIST)
        CALL GETIX(NDIM,X,PX,R,B,INTLIST)
		
C  MUST ALSO RESET T = 0 SINCE THE INTEGRATION WILL AGAIN START FROM 
C  TIME 0.

        T = 0.D0

C  IF SIGO(KNS) .LT. 0, THIS IS NOT ONLY A TIME RESET, IT IS THE
C  BEGINNING OF A STEADY STATE DOSE SET. IN THIS CASE, APPLY 100 
C  STEADY STATE DOSES WITH AN INTERDOSE INTERVAL EQUAL TO -SIGO(KNS).

        ISTEADY = 0

        IF(SIGO(KNS) .LT. 0.D0) THEN

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
C  THE 100TH DOSE SET WILL BE AT TIME 100*(-SIGO(KNS)), SO KNS WILL BE 
C  THE INDEX OF THE FIRST DOSE EVENT WHICH OCCURS AFTER THIS TIME.

C  IF "CONVERGENCE" IS NOT ACHIEVED, CONTINUE APPLYING THE LOGIC OF
C  PREDLAST3 UNTIL IT IS ACHIEVED, OR UNTIL THE 100 DOSE SETS ARE ALL
C  INTEGRATED THROUGH, WHICHEVER COMES FIRST.

          DOSEINT = -SIGO(KNS)

C  RESET SIGO(KNS) TO BE 0 SINCE THIS DOSE EVENT REPRESENTS THE START
C  OF 100 DOSE SETS THAT BEGIN AT TIME 0.

          SIGO(KNS) = 0

        ENDIF

C  THE ABOVE ENDIF IS FOR THE  IF(SIGO(KNS) .LT. 0.D0)  CONDITION.

      ENDIF

C  THE ABOVE ENDIF IS FOR THE 
C   IF(TIM(KNT) .EQ. 0.D0 .AND. KNT .GT. 1)  CONDITION.
C
C-----------------------------------------------------------------------
C
C wmy2017Dec07 Replacing GO TO 20 logic w/IF THEN {...} ELSE {...}
C   This block ends w/a forced decision to skip to Label 30 or 31.
C Blocks 15 and 25 after this block also end in the same forced
C decision. Blocks 15 and 25 are now conditionally calculated, and the
C decision to go to label 30 or 31 is calculated at the end of the
C IF THEN 15 ELSE 25 logic. The intention now is to enclose this logic
C w/in this block:
C    IF () THEN
C      {this block}
C    ELSE
C      {IF () THEN 15 ELSE 25}
C    ENDIF 
C    Skip to 30 or 31
C
C      IF(TIMO(KNT) .NE. SIGO(KNS)) GO TO 20
      IF(TIMO(KNT) .EQ. SIGO(KNS)) then
        ID=2
        TOUT=TIMO(KNT)
        KNT=KNT+1
        KNS=KNS+1

C        write (*,*) "KNS and KNT update at #1450", JSUB,IG,
C     1    T,TOUT,KNS, KNT
C
C wmy2017Dec08 -- This is done at the end of the IF/THEN logic
C      IF(NDIM .EQ. 0) GO TO 31
C      GO TO 30
C
      else

C 20    IF(TIMO(KNT) .GT. SIGO(KNS) .AND. SIGO(KNS) .GT. 0) GO TO 25
C
C wmy2017Dec06
C    A copy of block 15 is moved to Label 46; thus, there is now only
C one way to execute block 15, from this statemnt, label 20, which
C immediatley precedes it, so we can change Label 20 to an IF (.) THEN
C block 15 ELSE block 25, logic. Also, since both block 15 and block 25
C end with the same switch to labels 30 or 31, we can put switch at the
C end of the new Label 20 IF THEN 15 ELSE 25.
C
20    IF(TIMO(KNT) .LE. SIGO(KNS) .OR. SIGO(KNS) .LE. 0) then
C-------------------------------------------------------------------- 15
C Block 15 is entered in only two ways
C 1) via Label 46   IF(KNS .GT. INTLIST(8)) GO TO 15
C 2) if the conditional switch in Label 20, immediateley above, does
C    not force computation to skip Block 15.
C Block 15 has a forced exit to either Label 30 or 31.
C
15      ID=0
        TOUT=TIMO(KNT)
        KNT=KNT+1

C        write (*,*) "KNT update at second Block 15", KNT,JSUB,IG,T

C        IF(NDIM .EQ. 0) GO TO 31
C        GO TO 30
C-------------------------------------------------------------------- 15
      else
C-------------------------------------------------------------------- 25
25      ID=1
        TOUT=SIGO(KNS)
        KNS=KNS+1

C        write (*,*) "KNS update at Block 25", JSUB,IG,T, KNS

C-------------------------------------------------------------------- 25
      endif
      endif
      endif

      IF(NDIM .EQ. 0) GO TO 31
30    CONTINUE
C--------------------------------------------------------------- 30 / 32
C --- wmy2018.06.15 These lines are lifted from USERANAL; they have to
C --- be here to make ANAL3() work.
C --- When you get a chance, go back to useranal and erase these lines
C --- there as those lines are now redundant.  Also, remove INTLIST
C --- from the USERANAL() arglist
        do III=1,max_ODE_params
          RPAR(k_dvode_reserved + III) = PX(III)
C          RPAR(k_dvode_reserved + III) = P(III)
C          RPAR(23 + III) = P(III)
        end do
        do III = 1,max_RS_J
          RPAR(k_p_end + III) = R(III)
C          RPAR(55 + III) = R(III)
C          write (*,*) "what I is the bolus?", iii, RPAR(55+iii), r(iii) ! seems to be in R(2)
        end do

C DEBUG debug
C        if (IPAR(i_do).eq.7000) then
C        write (*,*) "func(...RPAR...):", RPAR(k_p_end+1),
C     1    RPAR(k_p_end+2), RPAR(k_p_end+3), RPAR(k_p_end+4),
C     2    RPAR(k_p_end+5), RPAR(k_p_end+6), RPAR(k_p_end+7),
C     3    RPAR(k_p_end+8), RPAR(k_p_end+9), RPAR(k_p_end+10)
C        end if

        RPAR(k_jsub) = dble(JSUB)
C        RPAR(93) = dble(JSUB)
        RPAR(k_ig) = dble(IG)
C        RPAR(94) = dble(IG)
        do III = 1,10
          IPAR(i_dvode_reserved + III) = INTLIST(III)
        end do
        IPAR(i_jsub) = JSUB
        IPAR(i_ig) = IG
C        write (*,*) "DEBUG 2018.06.15: RPAR",RPAR(24),RPAR(25),RPAR(26)
C     1     ,RPAR(27),RPAR(28) 
C--------------------------------------------------------------
32      IF(NDIM .NE. -1) then
          CALL USERANAL(JSUB,IG,X,T,TOUT,
     1      NDIM,MF,RTOL,ATOL,PX,R,INTLIST,IPAR,RPAR)
C          CALL USERANAL(JSUB,IG,X,T,TOUT,
C     1      NDIM,MF,RTOL,ATOL,P,R,INTLIST,IPAR,RPAR)

C-- Cycle past point
          if (IPAR(i_skip_ig).eq.0) return
        endif

C-- 6/12/18 -- Remove COMMON blocks from ANAL routines
C        write (*,*) "CALL ANAL3():",JSUB,IG,T,TOUT,ISTEADY,KNS,KNT
C     1    X(1),X(2),X(3)

        IF(NDIM .EQ. -1) CALL ANAL3(X,T,TOUT,RPAR,IPAR)

C-------------------------------------------------------------------- 32

C  IF ISTEADY = 1, THIS (Block 32) IS INSIDE A STEADY STATE DOSE SET. CHECK TO SEE
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

C        NN = N
C        IF(N .EQ. -1) NN = 3
        NN = NDIM
        IF(NDIM .EQ. -1) NN = 3

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
C  FIND THE FIRST SIGO(.) THAT IS .GE. 100*DOSEINT, OR THAT IS = 0
C  (WHICH SIGNIFIES A TIME RESET) OR THAT IS < 0 (WHICH SIGNIFIES 
C  ANOTHER STEADY STATE SET).

C-----------------------------  wmy2017Dec05 --- Replaced Logic
C          DO I = KNS,INTLIST(8)
C           IF(SIGO(I) .GE. 100.D0*DOSEINT .OR. SIGO(I) .LE. 0.D0) THEN
C            KNSNEW = I
C            GO TO 100
C           ENDIF
C          END DO
C
C C  TO GET HERE MEANS THAT THERE ARE NO DOSE TIMES PAST THE END OF THIS
C C  STEADY STATE DOSE SET. IN THIS CASE, SET KNS TO NDO+1.
C
C          KNS = INTLIST(8) + 1
C          GO TO 200
C
C  100     KNS = KNSNEW
C  200     CONTINUE
C---------------------------- Above is replaced w/logic below
          KNSOLD=KNS
          KNS = INTLIST(8) + 1
          DO I = KNSOLD,INTLIST(8)
           IF(SIGO(I) .GE. 100.D0*DOSEINT .OR. SIGO(I) .LE. 0.D0) THEN
            KNS = I
            EXIT
           ENDIF
          END DO

C           write (*,*) "KNS update at 200", JSUB,IG,T,KNS

C       write (*,*) "*** wmy2017Dec05 MvG code validation required."

  200     CONTINUE
C-----------------------------

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


C-------------------------------------------------------------------- 31
31      CONTINUE

C
C wmy2017Dec07 (Pearl Harbor Day)
C   Harden code and prepare for DO/WHILE logic to control
C code from Label 46 to Label 40.
C
C  GO TO logic:
C     IF (ID==1) GO TO 35
C       {...}
C     IF (ID==0) GO TO 40
C  35 CONTINUE
C     IF (intlist(7)==0) GO TO 83
C       {...}
C  83 IF (intlist(5)==0 OR NDIM==0) GO TO 82
C       {...}
C  82 CONTINUE
C  40 IF (KNT <= M) GO TO 46
C
C  is Replaced with IF/THEN logic:
C     IF (ID .ne. 1) then
C       {...}
C     endif
C     IF (ID .NE. 0) then
C       {
C         IF (intlist(7).NE.0) then
C           {...}
C         endif
C         IF (intlist(5).NE.0 .AND. NDIM.ne.0) then
C           {...}
C         endif
C       }
C     endif
C  40 IF (KNT <= M) GO TO 46
C     

C  RECORD OBSERVATION AND SUPPLY NEW DOSE

C        IF(ID .EQ. 1) GO TO 35
        IF(ID .NE. 1) then
          KNTM1=KNT-1

C  NOTE THAT THE TIME AT WHICH THE OUTPUT IS DESIRED IS TIM(KNTM1); THIS
C  IS CLEAR SINCE THE RETURNING VALUE(S) IN YT ARE PUT INTO ROW NO.
C  KNTM1 OF Y.

          CALL OUTPUT(TIMO(KNTM1),YT,X,RPAR,IPAR)

C          DO 2010 I=1,NOS
          DO 2010 I=1,INTLIST(9)
2010        Y(KNTM1,I)=YT(I)

        endif

C 55      IF(ID.EQ.0) GO TO 40
55      IF (ID.NE.0) then

C-------------------------------------------------------------------- 35
  35    CONTINUE

C            IF(NI .EQ. 0) GO TO 83 
C            IF(INTLIST(7) .EQ. 0) GO TO 83 
            IF(INTLIST(7) .NE. 0) then
     
C              DO I=1,NI
              DO I=1,INTLIST(7)
                R(I)=RSO(KNS-1,I)
              END DO

C  AS OF idm1x13.f: MUST CALL GETFA BEFORE EVERY TIME THAT
C  FA(.) ARE USED IN CASE THE EQUATION(S) FOR THE FA(.) ARE BASED
C  ON THE COVARIATES, WHICH CAN CHANGE DOSE TO DOSE.

c              CALL GETFA(FA,X)
              CALL GETFA(FA,X,PX,R,B,INTLIST)
            endif

C-------------------------------------------------------------------- 83
C83      IF(NDRUG .EQ. 0 .OR. N .EQ. 0) GO TO 82
C83      IF(NDRUG .EQ. 0 .OR. NDIM .EQ. 0) GO TO 82
C83      IF(INTLIST(5) .EQ. 0 .OR. NDIM .EQ. 0) GO TO 82
83          IF(INTLIST(5) .NE. 0 .AND. NDIM .NE. 0) then

C  ADDING N .EQ. 0 TO ABOVE IF STATEMENT SHOWS CLEARLY THAT IF
C  N = 0 (IN WHICH CASE ANALYTIC SOLUTIONS ARE CODED DIRECTLY INTO
C  SUBROUTINE OUTPUT, WHICH MAKES THE COMPARTMENT AMOUNTS IRRELEVANT)
C  SETTING VALUES FOR THE COMPARTMENTS, X, IS UNNECESSARY.


C  IF ISKIPBOL = 1, DO NOT APPLY BOLUSES FROM DOSE KNS-1, SINCE THESE
C  BOLUSES WERE PART OF THE STEADY STATE DOSE SET WHICH ALREADY HAD
C  BOLUSES (EFFECTIVELY) APPLIED ABOVE WHERE "CONVERGENCE" OF THE
C  STEADY STATE DOSE SET WAS OBTAINED.

              IF(ISKIPBOL .EQ. 0) THEN
C                DO I=1,NDRUG
                DO I=1,INTLIST(5)
                  X(NBCOMP(I))=X(NBCOMP(I))+BSO(KNS-1,I)*FA(I)
                END DO
              ENDIF

C  RESET ISKIPBOL = 0 HERE. IF IT IS NOW = 1, IT MEANS THAT
C  THE ABOVE APPLICATION OF BOLUSES WAS SKIPPED SINCE THERE HAS JUST
C  BEEN A STEADY STATE SET OF DOSES WHICH CONVERGED (AND WE DON'T
C  WANT THE LAST BOLUS DOSE REAPPLIED). BUT, GOING FORWARD, ISKIPBOL
C  SHOULD BE SET AGAIN TO 0 SO THE ABOVE APPLICATION OF BOLUSES WILL
C  OCCUR WHENEVER THERE IS A NEW BOLUS TO BE APPLIED.

              ISKIPBOL = 0

            ENDIF
C-------------------------------------------------------------------- 83

C 82      CONTINUE

        endif
C Above endif is for IF (ID .ne. 0) THEN; i.e. we skipped here b/c ID==0 

C  CHECK STOPPING TIME.


C       write (*,*) "40 INTLIST(8,10)", JSUB,IG,intlist(8), intlist(10)
C     1    ,KNS,KNT,T,TOUT

C40    IF(KNT .LE. M) GO TO 46
40    IF(KNT .LE. INTLIST(10)) GO TO 46
C  AS OF npageng28.f, GO TO 45 IS REPLACE BY GO TO 46 ABOVE.

C-------------------------------------------------- UPDATE F(JSUB,IG)
      if (UseInlineDO140.eq.1) then

C  NOTE THAT IF YO(I,J) = -99 --> THIS OBSERVED LEVEL IS MISSING.
C  IN THIS CASE, SET THE CORRESPONDING VALUE OF F = 0.

C wmy2017Dec29 Replaced COMMON STDEV with ObsError; note COMMON STDEV is
c  commented out above; uncomment if you need to use it!
c          IF(YO(I,J) .NE. -99) F((J-1)*M+I) =(Y(I,J)-YO(I,J))/STDEV(I,J)

C C       DO J=1,NOS
C C         DO I=1,M
C C           IF(YO(I,J) .EQ. -99) F((J-1)*M+I) = 0.D0
C C           IF(YO(I,J) .NE. -99) F((J-1)*M+I) =(Y(I,J)-YO(I,J))
C C    1                                         /ObsError(I,J)
C        DO J=1,INTLIST(9)
C         DO I=1,INTLIST(10)
C         IF(YO(I,J) .EQ. -99) F((J-1)*INTLIST(10)+I) = 0.D0
C         IF(YO(I,J) .NE. -99) F((J-1)*INTLIST(10)+I) =(Y(I,J)-YO(I,J))
C     1                                         /ObsError(I,J)
C        END DO
C        END DO

C ------ Above is old code, that assumed ObsError filled in main and ---
C ------ all measures are Normally distributed r.v.s -------------------
C ------ Below assumes ObsError is filled here, in the DO 140 loop and -
C ------ some measures are Poisson. ------------------------------------

C Poisson or Normally distributed measurements
C
C Strategy: In model.txt, for each of the 7 output equation set IPAR to
C indicate: Log10 or absolute measurement is reported, Poisson or Normal
C as flagged. Also, uses RPAR(>94,257) if real values, such as assay
C error coefficients are reqd.
C
C Computation:

        NNORMALOBS = 0
        NPOISSONOBS = 0
        MISVAL = 0
        SIGFAC = 1.D0
        OFAC = 0.D0

C Initialize or reset return variables
        RPAR(k_sum_z_sq) = 0.D0
        RPAR(k_prod_pr) = 0.D0
        RPAR(k_sfac) = SIGFAC
        RPAR(k_ofac) = OFAC
        IPAR(i_misval) = MISVAL
        IPAR(i_Nnormalobs) = NNORMALOBS
        IPAR(i_Npoissonobs) = NPOISSONOBS

C-----------------------------------------------------------------------
C DO 140 loop was done in main. Moved DO 140 here because some YOBSERVED
C are not Normally distributed.  Note that SIGFAC and OFAC were
C calculated prior to doing anything (by assuming it's OK to use
C YOBSERVED as a surrogate to the true mean: YMEAN.  Note: Useful to
C calculate DO 140 prior to any other calculation b/c (1) if a problem
C arises you can immediatley exit code and so not waste a lot of user
C time, and (2) saves a lot of computation! The first point is still
C valid -- so we should calculate DO 140 prior to the DO 800 loop for
C (at least) the first few cycles, using YOBSERVED as surrogate for
C YMEAN. This also allows us to have a more stable estimate of ObsError
C in the event that YMEAN is absurd. Or to have a "relaxation" toward
C YMEAN from YOBSERVED. For example, use the res parameter to control
C use of YMEAN or YOBSERVED or even use the average of these two values.
C wmy will just go ahead and program the average

C M Is a verbatim copy of code from Main that is NOT merged w/idm1*
C   Is a comment 

C  ******* DO 140 loop from main *******
C
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

C M      DO 140 I=1,NOBSER
C M       DO 140 J=1,NUMEQT
C
        DO J=1,INTLIST(9)     ! NUMEQT or number of output equations
         DO I=1,INTLIST(10)   ! Number of measurements

C           if (NINT(RPAR(J+k_c4_base)).eq.10) then    ! wmy: worked, 1/16/2019
C            write (*,*) "Obs is log10",I,J
C           endif
C           if (NINT(RPAR(J+k_c5_base)).eq.229) then   ! wmy: worked, 1/16/2019
C            write (*,*) "Obs is Poisson",I,J
C           endif
C           write (*,*) "Checking C4 and C5",
C     1            RPAR(J+k_c4_base), RPAR(J+k_c5_base)   ! wmy: worked, 1/16/2019

C  IF Y = -99, IT MEANS THAT OUTPUT EQ. J HAD NO VALUE AT OBSERVATION
C  TIME I. IN THIS CASE, IGNORE THIS Y AND INCREASE MISVAL BY 1.

C M        Y = YO(I,J) ! replaced by YMEAN, below
C M        IF(Y .EQ. -99) THEN    ! This is moved above
C M         MISVAL = MISVAL+1
C M         GO TO 140          ! Not necessary anymore, logic changed
C M        ENDIF
C M        IF(YO(I,J) .NE. -99) then { Assume obs ~ Normal } 

C 2018Sep01 :: MN wants -99 to indicate BLQ, and -88 to indicate missing.
C    RPAR(130) is available, I think, to use for the Pr(obs<LQ). But we
C    also need a space for the LOQ -- use RPAR(131).
C
C -----
          if(YO(I,J) .EQ. -99) then
C BLQ
C            A = <prob this obs is BLQ>
C            A = 1/2 * ERF((LQ - 0)/sigma)
C            RPAR(130) = RPAR(130) * A

C          else if (Y.eq.-88) then
C Desired Prediction
C
           F((J-1)*INTLIST(10)+I) = 0.D0
           MISVAL = MISVAL+1
           YOBSERVED = -99
           YMEAN = Y(I,J)
           ObsError(I,J) = 0.D0
           ZSCORE = 0.D0

c           write (*,*) JSUB,IG,I,J,"is missing."

C -----
          else

C ASSIGN YOBSERVED and YMEAN (predicted) for this measurement

C Y is log10(pred) and YO(I,J) are log10(obs). log10(obs) was read from
C  data.csv or working copy; while, log10(pred) was calculated in call
C  to OUTPUT, above. Use C4 = 10 in the model.txt file. C4 is not a
C  recognized variable in the data.csv file.
           IF (IPAR(i_is_log10 + J) == 10) THEN
            YOBSERVED = 10**(YO(I,J))
            YMEAN = 10**Y(I,J)
           ELSE
            YOBSERVED = YO(I,J)
            YMEAN = Y(I,J)
           ENDIF

C YOBSERVED ~ POISSON

C  229 == extended ASCII code for lower case lambda; USE Poisson
C  set C5 = 229 in the model.txt file. C5 is not a recognized variable
C  in the data.csv file.

C But, if YOBSERVED .gt. UseNormal, treat the count data observation
C  as a Normal variable, including using ierrmod and C0,1,2,3
           UseNormal = 128.0

           IF ( (IPAR(i_is_poisson_obs + J) == 229)
     1       .and. (YOBSERVED .lt. UseNormal) )  THEN

C wmy2019Feb04 -- for mxacd dataset, subject 1, only a single trough measure will
C   come into this loop; for UseNormal = 105; obs = 2.02; 10^2.02 = 104.7129
C   So, this one measurement ~ Normal(YMEAN,sqrt(YMEAN))

C            Poisson: p( YO(i,J) | Y(I,J) ) =  exp( -1 * Y(I,J) )
C                              * ( Y(I,J)^YO(I,J) / fact(YO(I,J)) ))

C Use Normal approx. to Poisson if YMEAN > P_thresh
           P_thresh = 32.0

C If resolve is still high, then use the greater of YMEAN or YOBSERVED
C   as surrogate for mean.  The goal is to allow the path to seek out models
C   that are biased toward the peak, initially, and then to attempt to
C   move the support point mass in a direction that will predict the
C   troughs, too. Recall: For Poisson, the var = mean.
C              if (RPAR(k_resolve) .gt. 0.05 ) then
C                  if (YOBSERVED .gt. YMEAN) then
C                     meanobsswap = YOBSERVED
C                     YOBSERVED = YMEAN
C                     YMEAN = YOBSERVED
C                  endif
C              endif
C wmy2018.01.28 -- Above still results in P(Y=1|G)=0 for moxi dataset

! fact reqs nint(); but fact(N) = Gamma(N + 1), and we use Gamma(N+1),
!  Gamma takes a real. So there is not reason to round YOBSERVED to
!  the nearest integer.
!              YOBSERVED = nint(YOBSERVED)
! Poisson w/mean Y; OK to be a REAL value
              ObsError(I,J) = sqrt( YMEAN )

!  Normal approx.
              IF (YMEAN > P_thresh) THEN
                 SIGFAC = SIGFAC * ObsError(I,J)
                 NNORMALOBS = NNORMALOBS+1
                 RPAR(k_sum_z_sq) = RPAR(k_sum_z_sq)
     1                 + ((YOBSERVED - YMEAN)/ObsError(I,J))**2

               if (ig.eq.1) then
                 write (*,*) JSUB,IG,I,J,"is P_N",YMEAN,YOBSERVED,
     1             ObsError(I,J),RPAR(k_sum_z_sq)
               endif

              ELSE
                 NPOISSONOBS = NPOISSONOBS + 1

C wmy2019.01.29 -- if Pr -> 0, then don't count probability in product.
C  
                 poissonprob = exp( -1.D0 * YMEAN )
     1              * ( YMEAN**YOBSERVED ) / gamma( YOBSERVED + 1)

                 if (poissonprob .gt. 0.0000001 ) then

C wmy2019.02.05 
C   To test code, set RPAR(k_prod_pr) to 0.D0; i.e. 10**0 = 1 in product in main
C   so that \prod pr(obs ~ Poisson) * \prod pr(obs ~ Normal) = \prod pr(obs ~ Normal)

                    RPAR(k_prod_pr) = RPAR(k_prod_pr)
     1                 + log10(PoissonProb)
                    write (*,*) JSUB,IG,I,J,"CAUGHT PoissonProb="
     1                 ,PoissonProb
                 else
                    write (*,*) JSUB,IG,I,J,"PoissonProb -> 0.D0"
     1                ,PoissonProb
                 endif

C wmy2019.01.29 :: Checked for "1          15          23           2 is P"
C   and all output numbers are correct; note that 10**RPAR(k_prod_pre) = 1.0
C   which just means: 10^0 = 1. YMEAN ~ 1.32337, YOBS = =105 = nint(104.7...)

C               if (ig.eq.1) then
C                 write (*,*) JSUB,IG,I,J,"is P  ",YMEAN,YOBSERVED,
C     1             ObsError(I,J), poissonprob, 10**RPAR(k_prod_pr)
C               endif
               if (ig.eq.1) then
                 write (*,*) "JSUB,IG,M,EQN",JSUB,IG,I,J
     1             , "is P w/mu,y,err,z^2,Pr="
     2             , YMEAN,YOBSERVED,ObsError(I,J)
     3             , RPAR(k_sum_z_sq),poissonprob
               endif
              ENDIF

C wmy2018Apr18 -- F returns to IDPC ast he concatenated z-scores

C              ZSCORE=(YOBSERVED-YMEAN)/ObsError(I,J)
C              F((J-1)*INTLIST(10)+I)=ZSCORE

C YOBSERVED ~ NORMAL

           ELSE
C              // CODE for Normal is unchanged from previous program
C 
C M NOTE: FOR EACH SUBJECT, MUST ENSURE THAT ALL THE STD DEV'S ARE NON-
C        ZERO. OTHERWISE, THE PROGRAM WILL BLOW UP! THIS IS BECAUSE
C        P(YJ|X) INVOLVES SQUARED DIFFERNCES BETWEEN OBSERVED Y'S AND
C        EXPECTED Y'S (FOR EACH X GRID POINT)...EACH DIFFERENCE
C        NORMALIZED (I.E., DIVIDED) BY THE VARIANCE OF THE RESPECTED
C        OBSERSATION.
 
C M  SEE M2_17CAL.F CODE FOR COMMENTS ON HOW A STD. DEV. COULD = 0.
 
C M  ALSO TEST TO MAKE SURE NO STD. DEV. < 0, SINCE SIGFAC BEING NEGATIVE
C  WOULD RESULT IN A NEGATIVE PROBABILITY (SEE PYJGX CALCULATION BELOW).

C note: in main:
C ierrmod -> IPAR(38)
C gamma -> RPAR(k_gamma)
C flat -> RPAR(k_flat)
C C0 -> RPAR(98,99,100,101,102,103,104)
C C1 -> RPAR(105:111)
C C2 -> RPAR(112:118)
C C3 -> RPAR(119:125)

C wmy2019.01.24 adjusting sigma calculation to use ymean after
C   current F is assumed to be w/in +/- 5% of the search space
C   range; uses observation as surrogate of mean if F is not
C   yet assumed to be w/in 5% of search space range.

C wmy2019.02.08 -- Switch u=OBS to u=PRED as soon as resolve
C  falls below initial value = 0.2; at this point, the simula-
C  tions should be stable.
C              if (RPAR(k_resolve) .lt. 0.15) then

C M            SIG(I,J) = C0(J)+C1(J)*Y+C2(J)*Y*Y+C3(J)*Y**3
C M            ObsError(I,J) = SIG(I,J) ! at end of DO 140

C              ObsError(I,J) = RPAR(J+k_c0_base)
C     1          + RPAR(J+k_c1_base)*YMEAN
C     2          + RPAR(J+k_c2_base)*YMEAN*YMEAN
C     3          + RPAR(J+k_c3_base)*YMEAN*YMEAN*YMEAN

C              else

              ObsError(I,J) = RPAR(J+k_c0_base)
     1          + RPAR(J+k_c1_base)*YOBSERVED
     2          + RPAR(J+k_c2_base)*YOBSERVED*YOBSERVED
     3          + RPAR(J+k_c3_base)*YOBSERVED*YOBSERVED*YOBSERVED

C              endif

C M cgam4
C M       if(ierrmod.eq.2) sig(i,j) = sig(i,j)*gamma
C M       if(ierrmod.eq.3) sig(i,j) = dsqrt(sig(i,j)**2 + gamma**2)
C M       if(ierrmod.eq.4) sig(i,j) = gamma*flat

              if(IPAR(i_errmod).eq.2) ObsError(i,j)
     1           = ObsError(i,j)*RPAR(k_gamma)
              if(IPAR(i_errmod).eq.4) ObsError(i,j)
     1           = RPAR(k_gamma)*RPAR(k_flat)
              if(IPAR(i_errmod).eq.3) ObsError(i,j)
     1           = dsqrt(ObsError(i,j)**2 + RPAR(k_gamma)**2)

C-------------- 
C M            IF(SIG(I,J) .EQ. 0) THEN  {} ! replaced with:
              IF(ObsError(I,J) .EQ. 0) THEN
                WRITE(*,2345) JSUB
                WRITE(25,2345) JSUB
2345            FORMAT(//' A S.D. IS 0 FOR JSUB = ',I5,'. RERUN THE '/
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
 
              IF(ObsError(I,J) .LT. 0) THEN

C wmy2019.02.10
C               write (*,*) "idm1 SD Err01:", JSUB,IG,I,J,YMEAN,YOBSERVED
C     1           ,RPAR(J+k_c0_base), RPAR(J+k_c1_base)
C     2           ,RPAR(J+k_c2_base), RPAR(J+k_c3_base)
C     3           ,ObsError(I,J)
C              ObsError(I,J) = RPAR(J+k_c0_base)
C     1            + RPAR(J+k_c1_base)*YOBSERVED
C     2            + RPAR(J+k_c2_base)*YOBSERVED*YOBSERVED
C     3            + RPAR(J+k_c3_base)*YOBSERVED**3
C               write (*,*) "idm1 SD Err02:", JSUB,IG,I,J,YMEAN,YOBSERVED
C     1           ,RPAR(J+k_c0_base), RPAR(J+k_c1_base)
C     2           ,RPAR(J+k_c2_base), RPAR(J+k_c3_base)
C     3           ,ObsError(I,J)

                WRITE(*,2346) JSUB,IG
                WRITE(25,2346) JSUB,IG
2346            FORMAT(//' A S.D. < 0 FOR JSUB,IG = ',I5,I6,'.     '/
     1'RERUN THE PROGRAM WITH A BETTER CHOICE FOR THE ASSAY ERROR  '/
     2'POLYNOMIAL COEFFICIENTS.')
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

C--------------  SIGFAC and NNORMALOBS

C In main, COMMON SIG is used, and values are copied from SIG into
C ObsError. ObsError is used inside of the parallel region. Here, we
C only have access to ObsError, not SIG. So fill ObsError, and copy
C ObsError to SIG when you return to main. Note that the analytic
C routines use COMMON SIG (the primary reason we have to still
C calculate SIG in main).  ObsError may be calculated differently
C here, than in main.

C M      ObsError(I,J) = SIG(I,J)

              SIGFAC=SIGFAC*ObsError(I,J)
              NNORMALOBS=NNORMALOBS+1

              RPAR(k_sum_z_sq) = RPAR(k_sum_z_sq)
     1           + ((YOBSERVED-YMEAN)/ObsError(I,J))**2

               if (ig.eq.1) then
                 write (*,*) "JSUB,IG,M,EQN",JSUB,IG,I,J
     1             , "is N w/mu,obs,err,z^2="
     2             , YMEAN,YOBSERVED,ObsError(I,J)
     3             , RPAR(k_sum_z_sq)
C     4             , IPAR(i_errmod), RPAR(k_gamma)
               endif
           endif ! if(Poisson){...} else {Normal}

C wmy2018Apr18 -- if (YO.NE.-99) then {F = z-score}

           ZSCORE=(YOBSERVED-YMEAN)/ObsError(I,J)
           F((J-1)*INTLIST(10)+I)=ZSCORE

C debug
c                 write (*,*) JSUB,IG,I,J,YOBSERVED,YMEAN,zscore

          ENDIF ! if(YO(I,J) .EQ. -99){...} else {YO ~ {Normal,Poisson}}

C wmy DEBUG
C          write (*,*) "idm01",I,J,ZSCORE,RPAR(k_prod_pr)
C     1       , YMEAN, YOBSERVED, ObsError(I,J), YO(I,J), Y(I,J)

         END DO ! number of measurements
        END DO ! number of output equations
C M  140 CONTINUE ! is replaced by the two END DO above

C M NOTE THAT SIGFAC WAS CALCULATED IN LOOP 140 ABOVE, AND THAT OFAC IS
C M NOW THE RESULT OF (NOBSER*NUMEQT - MISVAL) VALUES.
C M      OFAC=2.506628274631**(NOBSER*NUMEQT - MISVAL)
C   The above comment and code form main are no longer valid because
C   Some observations are ~Poisson and some of those are going to be
C   calculated using a Normal approx. to the Poisson.

C M      NOBTOT = NOBTOT + NOBSER*NUMEQT - MISVAL
C   Above NOBTOT counts the total observations for all OBSERVATIONS.
C   We are only interested in the number of observations for JSUB
        OFAC=2.506628274631**NNORMALOBS

C Final checks on NNORMALOBS, NPOISSONOBS, MISVAL, SIGFAC, and OFAC
C Copy values into appropriate IPAR and RPAR bins for communication 
C back up to main.
C
C RPAR(k_prod_pr) = \prod Poisson Probs
C RPAR(k_sum_z_sq) = summsq Normalized deviations for Y_obs ~ Normal
        RPAR(k_sfac) = SIGFAC
        RPAR(k_ofac) = OFAC
        IPAR(i_misval) = MISVAL
        IPAR(i_Nnormalobs) = NNORMALOBS
        IPAR(i_Npoissonobs) = NPOISSONOBS


C-----------------------------------------------------------------------
      else 

C STDEV - Above is replaced with the MODULE subroutine:

C        CALL DO140(ijob,JSUB,IG,INTLIST,IPAR,RPAR,F,ObsError,YO,Y,ERRFIL)
        CALL DO140(1,JSUB,IG,INTLIST,IPAR,RPAR,F,ObsError,YO,Y,ERRFIL)

      endif

C Data required to calculate sigma:
C
C  1. Integer array including
C     a. JSUB and IG
C     b. Flag for type of noise: Logistic, exponential, Poisson, Normal, ...
C     c. Flags for computational choices
C     d. Calculation constants that are integer valued (IPAR)
C  2. History of Yobs and Ypred for interoccassion noise
C  3. Real array of fixed constants 
C  4. Real scalar RETURN variable
C 
C------------------------------------------------------------------------------------------------------------------

C  AS OF idm1x9.f, RESTORE THE VALUES FOR ND, SIG, AND RS, IN CASE
C  THIS MODEL HAS TIME LAGS OR STEADY STATE DOSES - TO BE READY FOR THE
C  NEXT CALL TO THIS ROUTINE.
C  NO! AS OF idm1x17.f, THESE VALUES WERE NEVER CHANGED BECAUSE
C      NDO, SIGO, RSO, BSO WERE USED ABOVE INSTEAD.

! NEW PARALLEL CODE BELOW AS OF npageng28.f


!	 ND = NDO
!	 DO I=1,INTLIST(8)
!	  SIGCOPY(I) = SIGO(I)
!	  DO J=1,NI
!	   RSCOPY(I,J) = RSO(I,J)
!	  END DO
!	 END DO

C  ESTABLISH THE VALUES IN EACH DRUG'S PO COLUMN TO THE CORRESPONDING
C  COLUMN IN ARRAY BS.

!         DO I=1,INTLIST(8)
!          DO J=1,INTLIST(5)
!           BSCOPY(I,J)=RSCOPY(I,2*J)
!	  END DO
!	 END DO


      RETURN
      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        SUBROUTINE USERANAL(JSUB,IG,X,TIN,TOUT,
     1      NDIM,MF,RTOL,ATOL,P,R,INTLIST,IPAR,RPAR)

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
C           X - AN ARRAY OF DIMENSION max_ODE_comp. IN THE STANDARD 3-COMPARTMENT
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

        use npag_utils, only: max_RS_J,max_ODE_params,max_ODE_comps
     1   , k_dvode_reserved, k_p_end, k_ig, k_jsub, i_ig, i_ig
     2   , i_jsub, i_skip_ig, k_dvode_reserved, i_dvode_reserved

        IMPLICIT none
C REAL*8(A-H,O-Z)

c INPUT parameters ::
        integer :: JSUB, IG
        double precision, dimension(max_ODE_comps), intent(INOUT) :: X
        double precision :: TIN, TOUT
        integer :: NDIM, MF
        double precision :: RTOL
        double precision,dimension(max_ODE_comps),intent(INOUT) :: ATOL
        real*8, dimension(max_ODE_params) :: P
        real*8, dimension(max_RS_J) :: R
        integer, dimension(128) :: INTLIST
        integer, dimension(257) :: IPAR
        double precision, dimension(257) :: RPAR

c wmy2017Oct23 -- These variables will be passed to DVODE
C
C  AS OF idm1x16.f, THE DIMENSION OF RWORK IS CHANGED FROM 300 TO
C  1002 TO ACCOMADATE AN NDIM (NEQ IN SUBROUTINE DVODE) OF UP TO max_ODE_comp. SO
C  CHANGE LRW BELOW TO 1002. SIMILARLY THE DIMENSION OF IWORK AND
C  LIW BELOW ARE CHANGED FROM 40 TO 50.
c
        EXTERNAL DIFFEQ,JACOB
        real*8, dimension(1002) ::  RWORK
        integer, dimension(50) :: IWORK
        integer :: ITOL, ITASK, ISTATE, IOPT, LRW, LIW, III
C        real*8, dimension(257) :: RPAR

C wmy2017Oct23
C The following ThreadPrivates got the program "working" again i.e. the
c 3.5 week long search for the bug causing serial program to work
c perfectly, BUT parallel program throwing an Illegal Instruction : 4 error
C Is likely due to an inappropriate memory map imposed by OpenMP not
c 
c  !$omp ThreadPrivate (RWORK, IWORK, RPAR)
c        save RWORK, IWORK, RPAR
!$omp ThreadPrivate (RWORK, IWORK)
        save RWORK, IWORK
C wmy2017Dec28: removed IPAR from save list above; save-ing in main
c
c On 10/23/2017 at end of day; all flags passed OK (as before)
c ATOL(1) OK, but ATOL(max_ODE_comps) = 0, not 10^-4 (actually, serial run has ATOL(max_ODE_comps) = 0, too)
c RPAR = 0.0, except for last thread, is this master? JSUB, IG = 1,1, RPAR has
c correct values (compared to serial run)
c

      INTERFACE
        SUBROUTINE DVODE (F, NEQ, Y, T, TOUT, ITOL, RtolIn,
     &    ATOL, ITASK, ISTATE, IOPT, RWORK, LRW, IWORK, LIW,
     &    JAC, MF, RPAR, IPAR)

          EXTERNAL F
          integer, intent(IN) :: NEQ
          double precision, dimension(:), intent(INOUT) :: Y
          double precision, intent(INOUT) :: T, TOUT
          integer, intent(IN) :: ITOL
          double precision, intent(IN) :: RtolIn
          double precision, dimension(:), intent(IN) :: ATOL 
          integer, intent(IN) :: ITASK
          integer, intent(INOUT) :: ISTATE
          integer, intent(IN) :: IOPT
          double precision, dimension(:), intent(INOUT) :: RWORK
          integer, intent(IN) :: LRW
          integer, dimension(:), intent(INOUT) :: IWORK
          integer, intent(IN) :: LIW
          EXTERNAL JAC
          integer, intent(IN) :: MF
          double precision, dimension(:), intent(INOUT) :: RPAR
          integer, dimension(:), intent(INOUT) :: IPAR

        end SUBROUTINE
      END INTERFACE

C wmy2017Sep12 /TOUSER/ varbs are passed in
C	COMMON/TOUSER/NDIM,MF,RTOL,ATOL
C
C !$omp Threadprivate(/TOUSER/)

! -------- START OF DVODE DECLARATIONS SPECIFIC ------------

c wmy2017Oct06 -- See DVODE docs "Part iii." -- not sure if the
c   declaration is supposed to be here, in SUBROUTINE FUNC, or 
c   maybe even as far back as SUBROUTINE NPAG, outside the 
c   parallelized region.
      COMMON /DVOD01/ ACNRM, CCMXJ, CONP, CRATE, DRC, EL(13),
     1                ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1,
     2                RC, RL1, TAU(13), TQ(5), TN, UROUND,
     3                ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH,
     4                L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     5                LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP,
     6                N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ,
     7                NSLP, NYH
      COMMON /DVOD02/ HU, NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
!
! Type declarations for labeled COMMON block DVOD01 --------------------
!
      DOUBLEPRECISION ACNRM, CCMXJ, CONP, CRATE, DRC, EL, ETA, ETAMAX,
     1  H, HMIN, HMXI, HNEW, HSCAL, PRL1, RC, RL1, TAU, TQ, TN, UROUND

      INTEGER ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH, L, LMAX,
     1 LYH, LEWT, LACOR, LSAVF, LWM, LIWM, LOCJS, MAXORD, METH, MITER,
     2 MSBJ, MXHNIL, MXSTEP, N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT,
     3 NSLJ, NSLP, NYH
!
! Type declarations for labeled COMMON block DVOD02 --------------------
!
      DOUBLEPRECISION HU

      INTEGER NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST

c wmy2017Oct -- May also req. calls to DVSRCO -- see DVODE doc
C  "Interrupting and Restarting" -- which will store the above COMMON
c  variables in the following arrays
c      COMMON /DVOD01/ RVOD(48), IVOD(33)
c      COMMON /DVOD02/ HU, NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST

c wmy2017Oct06 -- I _assume_ this is required
!$omp Threadprivate(/DVOD01/,/DVOD02/)

c wmy2017Nov07 -- I _assume_ this is also required
      save /DVOD01/,/DVOD02/

! -------- END OF DVODE DECLARATIONS SPECIFIC -------------

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

C Code in DIFFEQ:
C
C wmy2017Sep29 -- ALL of the varbs in teh following 4 COMMON blocks need
C   to be passed explicitely to DIFFEQ via DVODE subroutines. This will
C   be accomplished by copying them into appropriate spots in RPAR and
C   IPAR, and then read back into their named variables in DIFFEQ.
C
C   As of 9/29/2017 Only P and R are passed correctly.
C
C      COMMON /PARAMD/ P
C      COMMON /INPUT/ R,B
C      COMMON /DESCR/ AGE,HEIGHT,ISEX,IETHFLG
C      COMMON /CNST2/ NPL,NUMEQT,NDRUG,NADD
C
C      DIMENSION X(NDIM),XP(NDIM),P(max_ODE_params),R(max_RS_J),B(20),CV(26),RATEIV(max_input_dim) 
C
C         DO I = 1,NDRUG
C           RATEIV(I) = R(2*I - 1)
C         END DO
C         DO I = 1, NADD
C           CV(I) = R(2*NDRUG + I)
C         END DO  
C End Code in DIFFEQ
C
C Based on above:
C   rpar(1:23) = reserved 
C   rpar(24:55) = P(1:max_ODE_params) -- CORDEN(IG,1:NVAR)
C   rpar(56:92) = R(1:max_RS_J) -- rateiv + covariates
C
C wmy2017Sep29 :: NOTE ::  What about B, and /DESCR/ and /CNST/
C
C wmy2017Sep25 --
C      write (*,*) "USER->DVODE",JSUB,IG,TIN,TOUT,NDIM,MF
C      write (*,*) "PX",JSUB,IG,P(1),P(2),P(3),P(4),P(5),P(6)
C      write (*,*) "R ",JSUB,IG,R(1),R(3),R(4),R(5),R(6),R(7)

c wmy2017Sep27 -- Edited PMetrics::makeModel() to
C   read P(III) from RPAR(23 + III, III = 1:max_ODE_params), and
C   read R(III) from RPAR(55 + III, III = 1:max_RS_J)
        do III=1,max_ODE_params
           RPAR(k_dvode_reserved + III) = P(III)
        end do
        do III = 1,max_RS_J
           RPAR(k_p_end + III) = R(III)
        end do
           RPAR(k_jsub) = dble(JSUB)
           RPAR(k_ig) = dble(IG)
C wmy2017Oct02 -- 
        do III = 1,10
           IPAR(i_dvode_reserved + III) = INTLIST(III)
        end do
           IPAR(i_jsub) = JSUB
           IPAR(i_ig) = IG

C      write (*,*) "RPAR",JSUB,IG,RPAR(24),RPAR(25),
C     1   RPAR(26),RPAR(27),RPAR(28),RPAR(29)
C      write (*,*) "RPAR",JSUB,IG,RPAR(56),RPAR(58),
C     1   RPAR(59),RPAR(60),RPAR(61),RPAR(62)

c wmy2017Oct03 Note that Parallel.eq.true->(3,0,3)
c        write (*,*) "NDIM =",NDIM, N, INTLIST(9)
C
C wmy2017)ct05 -- Program gets to here fine; but note
C

c       write (*,*) "In USERANAL"

c      write (*,*) JSUB,IG, "FUNC->DVODE",
c     1  TIN,X(1),X(2),X(3),TOUT,RTOL,ATOL(1)

        if (TIN.eq.TOUT) write (*,*) "WARNING: TIN=TOUT; JSUB=",JSUB

        CALL DVODE(DIFFEQ,NDIM,X,TIN,TOUT,ITOL,RTOL,ATOL,ITASK,ISTATE,
     1            IOPT,RWORK,LRW,IWORK,LIW,JACOB,MF,RPAR,IPAR)

c wmy2018August29
C        if (JSUB.eq.1) then
C          if ((IG.eq.40012).or.(IG.eq.120034).or.(IG.eq.160045)
C     1        .or.(IG.eq.280075)) then
C            write (*,*) IG,"IG",TOUT,(X(iii),iii=1,3)
C          end if
C        end if

        IF (ISTATE .LT. 0) THEN
C         WRITE(*,16) ISTATE
C 16      FORMAT(///' On return from DVODE, ISTATE =',I3)
         IPAR(i_skip_ig) = 0
        ENDIF



        TIN=TOUT

        RETURN
        END
C

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE JACOB(NDIM, T, X, ML, MU, PD, NRPD, RPAR, IPAR)

        use npag_utils, only: max_RS_J,max_ODE_params,max_ODE_comps

	IMPLICIT none
C REAL*8(A-H,O-Z)

! NEW PARALLEL CODE BELOW AS OF npageng28.f
C !$omp Threadprivate(/PARAMD/,/INPUT/)


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

        integer NDIM, NRPD
        double precision T,X,ML,MU
        double precision, dimension(:,:) :: PD
        double precision, dimension(257) :: RPAR
        integer, dimension(257) :: IPAR


C  OUTPUT ARE:


C  PD(I,J) = PARTIAL DERIVATIVE OF XP(I) W.R.T. X(J), WHERE XP(I)
C	     ARE CALCULATED IN ROUTINE DIFFEQ ABOVE.

        RETURN
        END							  
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
