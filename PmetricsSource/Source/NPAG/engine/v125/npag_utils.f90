module npag_utils
!
! npag_utils.f90 ! put this file _first_ in compilation command; the following must be created at first step of compilation
! -> npag_utils.mod ! zipped fortran interfaces
! -> npag_utils.o ! .o functions and subroutines

implicit none

! by default, this module should appear empty ...
private

! ... to access, put "USE npag_utils,only: list_of_what_you_want" at
! the head of the program unit (e.g. immediately after "subroutine name"
! and prior to "implicit ___" statements).  Where, "public" variables
! are what you can put in "list_of_what_you_want"

public shift, makevec, check_input_array_size, cp_lrcs_to_rpar, expand_grid, &
  verifyval, orderdelta,thesame, predlast3, do140, &
  i_cycle, i_do, i_jsub, i_ig, max_ODE_comps, max_ODE_params, &
  max_pop_rand_varbs, max_pop_varbs, max_pop_params, max_SS_doses, &
  max_covs, maxnumeq, max_meas_per_eqn, max_m_per_obs, max_obs, max_obs_dim, &
  max_doses, max_input_dim, max_RS_J, k_ig, k_jsub, &
  i_dvode_reserved, k_dvode_reserved, k_sum_z_sq, k_prod_pr, k_resolve, &
  k_p_start, k_p_end, k_r_start, k_r_end, k_gamma, k_flat, k_sfac, k_ofac, &
  k_c0_base, k_c1_base, k_c2_base, k_c3_base, k_c4_base, k_c5_base, i_errmod, &
  i_is_log10, i_Nnormalobs, i_is_poisson_obs, i_Npoissonobs, i_misval, i_skip_ig

! ODE parameters

integer, parameter :: max_ODE_comps =      20
integer, parameter :: k_dvode_reserved =   23   ! RPAR(1:23) are reserved by DVODE
integer, parameter :: i_dvode_reserved =   23   ! IPAR(1:23) are reserved by DVODE
integer, parameter :: max_ODE_params =     47   ! 32  ! NVAR+NRANFIX+NOFIX <= max_ODE_params , of types:
integer, parameter :: max_pop_rand_varbs = 30   ! 30  ! NVAR <= max_pop_rand_varbs; variables w/#support <= max_obs
integer, parameter :: max_pop_varbs =      20   ! NRANFIX <= max_pop_varbs; variables with #support = 1
integer, parameter :: max_pop_params =     20   ! NOFIX <= max_pop_params; #user-defined scalar pop parameters
integer, parameter :: max_SS_doses =      100   ! dim of SS calc = max_SS_doses X max_ODE_comps

! INPUT / OUTPUT parameters

integer, parameter :: max_input_dim =       7   ! #Input types, NDRUG <= max_input_dim
integer, parameter :: max_doses =        5000   ! ND <= max_doses per record
integer, parameter :: max_obs  =          800   ! = max #records; MAXSUB = 800
integer, parameter :: max_covs =           20   ! max #covariate measures per record; dim
integer, parameter :: max_obs_dim  =      150   ! = MAXOBDIM
integer, parameter :: maxnumeq =            7   ! #output equations
integer, parameter :: max_meas_per_eqn =   99   ! #measurements per patient

! Parameters that are determined from above:

integer, parameter :: max_m_per_obs = max_meas_per_eqn * maxnumeq ! was hardcoded to 594
integer, parameter :: max_RS_J = 2*max_input_dim + max_covs           !  NI <= max_RS_J

! X(NBCOMP(I))=X(NBCOMP(I))+BS(KNS,I)*FA(I)

! RPAR and IPAR are concatenations of ODE parameters, following are
! shortcuts into RPAR (k) and ILIST (j) and IPAR (i)
integer, parameter :: k_p_start = k_dvode_reserved +  1
integer, parameter :: k_p_end = k_dvode_reserved + max_ODE_params
integer, parameter :: k_r_start = k_p_end + 1
integer, parameter :: k_r_end = k_p_end + max_RS_J
integer, parameter :: k_jsub = k_r_end + 1
integer, parameter :: k_ig = k_jsub + 1
integer, parameter :: k_gamma = k_ig + 2 ! this should be 96 
integer, parameter :: k_flat = k_gamma + 1
integer, parameter :: k_c_start = k_flat + 1       ! maxnumeq * (C0,C1,C2,C3,C4,C5)
integer, parameter :: k_c0_base = k_flat
integer, parameter :: k_c1_base = k_c0_base + maxnumeq
integer, parameter :: k_c2_base = k_c1_base + maxnumeq
integer, parameter :: k_c3_base = k_c2_base + maxnumeq
integer, parameter :: k_c4_base = k_c3_base + maxnumeq
integer, parameter :: k_c5_base = k_c4_base + maxnumeq
integer, parameter :: k_c_end = k_flat + maxnumeq * 6
integer, parameter :: k_sfac = k_c_end + 1         ! SIGFAC
integer, parameter :: k_ofac = k_sfac + 1          ! OFAC
integer, parameter :: k_sum_z_sq = k_ofac + 1      ! = \sum ((obs - pred)/sigma)^2 -- for obs ~ Normal
integer, parameter :: k_prod_pr = k_sum_z_sq + 1   ! = \Prod pr(obs) -- for obs ~ Poisson
integer, parameter :: k_resolve = k_prod_pr + 1

! IPAR(110 + J) in {229, ~229} = flag for Poisson
! (note: set to 229 if C0 = C2 = C3 = 229 in model.txt)
! IPAR(120 + J) in {10, ~10} obs = log10(measurement)

! IPAR(23 + I) = INTLIST(I; 1:10); where,
! Values copied into INTLIST are ->
! (1) = AGE; (2) = ISEX; (3) = HEIGHT; (4) = IETHFLAG;
! (5) = /CNST2/NDRUG; (6) = /CNST2/NADD;
! (7) = /CNST/NI = 2*NDRUG+NADD; (8) = /CNST/ND;
! (9) = /CNST2/NUMEQT; (10) = /CNST2/M = NOBSER;
! Note: Some above are NOT integers!!!
integer, parameter :: i_jsub = i_dvode_reserved + 11 ! yes, 11, not 1
integer, parameter :: i_ig = i_jsub + 1
integer, parameter :: i_do = 36                ! IPAR(i_do) = DO# in 1000, 6000, 7000
integer, parameter :: i_cycle = 37
integer, parameter :: i_errmod = 38
integer, parameter :: i_misval = 39
integer, parameter :: i_Nnormalobs = 40
integer, parameter :: i_Npoissonobs = 41
integer, parameter :: i_skip_ig = 100          ! if ipar(i_skip_ig) = 0 set P(JSUB|IG)=0
integer, parameter :: i_is_poisson_obs = 110   ! 110 + eqno = 229 if Poisson
integer, parameter :: i_is_log10 = 120         ! 120 + eqno = 10, then obs recorded as log10(measurement)
 
! Pmetrics erases all lines beginning with 'c' or "C", so
! the following line must be " contains", w/leading white-space
 contains
!
! subroutine do140
! subroutine cp_lrcs_to_rpar
! function check_input_array_size
! subroutine makevec
!
! #################################################################### ! 

! subroutine DO140

! For the JSUB observation and IGth support point:

! Inputs observed YO(:,:) and predicted Y(:,:) values

! Outputs F(K) = zscore of the Kth prediction
!    1:K ~ \series_i\series_j (outeq_i,measurement_j).
!    If measurement is "-99" or "-88" (BLQ or missing,
!    respectively) then F() <- 0.D0.
! Also updated are:
! RPAR(k_prod_pr), RPAR(k_sum_z_sq), RPAR(k_sfac),
! and RPAR(k_ofac)

! Note: Pr(JSUB|IG) = \prod Pr((K|JSUB,IG))
!    = Pr(all measurements ~ Poisson)
!      * Pr(all measurements ~ Normal)
! As of 20190729 In main,
!    PYJGX(JSUB,IG) =
!     [ 10**RPAR(k_prod_pr) ]  ! Pr(\all Poisson Measurements)
!     * [ DEXP(-.5D0*RPAR(k_sum_z_sq)) ! * Pr(\all Normal Measurements)
!       / RPAR(k_sfac)/RPAR(k_ofac) ], and
!    RPAR(k_prod_pr) = \sum log10(PoissonProb), below.

      subroutine DO140(ijob,JSUB,IG,INTLIST,IPAR,RPAR,F,ObsError,YO,Y,ERRFIL)

!      use npag_utils, only: k_sum_z_sq, k_prod_pr, k_sfac, k_ofac &
!        , k_resolve, k_gamma, k_flat &
!        , k_c0_base, k_c1_base, k_c2_base, k_c3_base, k_c4_base, k_c5_base &
!        , i_errmod, i_misval, i_Nnormalobs, i_Npoissonobs &
!        , i_is_log10, i_is_Poisson_obs
!        , max_m_per_obs, maxnumeq

      implicit none

!      integer k_sum_z_sq, k_prod_pr, k_sfac, k_ofac &
!        , k_resolve, k_gamma, k_flat &
!        , k_c0_base, k_c1_base, k_c2_base, k_c3_base, k_c4_base, k_c5_base &
!        , i_errmod, i_misval, i_Nnormalobs, i_Npoissonobs &
!        , i_is_log10, i_is_Poisson_obs

! Arguments

      integer, intent(in) :: ijob, JSUB, IG
      integer, dimension(1:), intent(in) :: INTLIST            ! dimension(128)
      integer, dimension(1:), intent(inout) :: IPAR            ! dimension(257)
      double precision, dimension(1:), intent(inout) :: RPAR   ! dimension(257)
      double precision, dimension(1:), intent(inout) :: F      ! dimension(3564)
      double precision, dimension(1:,1:), intent(inout) :: ObsError ! dimension(max_m_per_obs,maxnumeq)
      double precision, dimension(1:,1:), intent(in) :: YO,Y   ! dimension(max_m_per_obs,maxnumeq)
      character*20 ERRFIL

! Local variables

      integer, parameter :: DEBUG=2              ! write debugging messages
      integer i,j,k                              ! see notes above
      integer MISVAL, NNORMALOBS, NPOISSONOBS
      double precision YOBSERVED, YMEAN, ZSCORE, meanobsswap
      double precision, parameter :: OBSSWAPPOISSON = 0.5D0, OBSSWAPNORMAL = 0.0D0
      double precision poissonprob
      double precision, parameter :: UseNormal = 128.0D0, P_thresh = 32.0D0
      double precision SIGFAC

! Initialization:

        k = 0
        MISVAL = 0
        NNORMALOBS = 0
        NPOISSONOBS = 0
        YOBSERVED = 0.D0
        YMEAN = 0.D0
        ZSCORE = 0.D0
        meanobsswap = 0.D0 
        poissonprob = 0.D0
        SIGFAC = 1.D0


! Reset return variables

        RPAR(k_sum_z_sq) = 0.D0
        RPAR(k_prod_pr) = 0.D0
        RPAR(k_sfac) = SIGFAC
        IPAR(i_misval) = MISVAL
        IPAR(i_Nnormalobs) = NNORMALOBS
        IPAR(i_Npoissonobs) = NPOISSONOBS

! -----------------------------------------------------------------------

!       DO 140 I=1,NOBSER
!         DO 140 J=1,NUMEQT

        DO J=1,INTLIST(9)     ! NUMEQT or number of output equations
          DO I=1,INTLIST(10)    ! Number of measurements, NOBSER

            k = (J-1)*INTLIST(10)+I ! k = k + 1

            if (DEBUG == 7) then
              if (NINT(RPAR(J+k_c4_base)).eq.10) then  ! wmy: worked, 1/16/2019
                write (*,*) "Obs is log10",I,J
              endif
              if (NINT(RPAR(J+k_c5_base)).eq.229) then ! wmy: worked, 1/16/2019
                write (*,*) "Obs is Poisson",I,J
              endif
              write (*,*) "Checking C4 and C5", &
                RPAR(J+k_c4_base), RPAR(J+k_c5_base)   ! wmy: worked, 1/16/2019
            endif ! DEBUG

! YOBSERVED ~ BLQ -----------------------------------------------------
            if(YO(I,J) .EQ. -99) then
! BLQ
!             A = <prob this obs is BLQ>
!             A = 1/2 * ERF((LQ - 0)/sigma)
!             RPAR(130) = RPAR(130) * A
!
! 2018Sep01 :: MN wants -99 to indicate BLQ, and -88 to indicate missing.
!    RPAR(130) is available, I think, to use for the Pr(obs<LQ). But we
!    also need a space for the LOQ -- use RPAR(131).
!
! YOBSERVED ~ MISSING --------------------------------------------------
!           else if (Y.eq.-88) then
!
              F(k) = 0.D0
              MISVAL = MISVAL+1
              YOBSERVED = -99
              YMEAN = Y(I,J)
              ObsError(I,J) = 0.D0
              ZSCORE = 0.D0

              if (DEBUG==99) write (*,*) JSUB,IG,I,J,"is missing obs."

            else

! Assign YOBSERVED and YMEAN (predicted) for this measurement

!  If data.csv contains log10(obs) entries, then C4(J) should
!    be set to 10 in the model.txt file; also, SUBROUTINE OUTPUT
!    should convert predicted measurement to log10(pred).
!  Thus, at this point, calculating the standard deviations
!    requires conversion of Y and YO (both of which are log10)
!    to untransformed values:

              IF (ipar(i_is_log10 + J) == 10) THEN
                YOBSERVED = 10**(YO(I,J))
                YMEAN = 10**Y(I,J)
              ELSE
                YOBSERVED = YO(I,J)
                YMEAN = Y(I,J)
              ENDIF

! YOBSERVED ~ POISSON --------------------------------------------------

! If the data is believed to best be described by a Poisson distribution,
!  the user should set C5 <- 229 (the extended ASCII code for lower
!  case lambda. Note that C4 and C5 are NOT recognized in the data.csv
!  file by the prep program, even though C4 and C5 are required to be in
!  the model.txt file bv pmetrics::NPrun.

! If YOBSERVED .ge. UseNormal, then skip to YOBSERVED ~ NORMAL. The
!  observation will be treated as normal with mean YMEAN and standard
!  deviation determined according to ierrmod

            IF ( (IPAR(i_is_poisson_obs + J) == 229) &
              .and. (YOBSERVED .lt. UseNormal) )  THEN

!             Poisson: p( YO(i,J) | Y(I,J) ) =  exp( -1 * Y(I,J) )
!                              * ( Y(I,J)^YO(I,J) / fact(YO(I,J)) ))
!                where, fact(int N) = gamma(real N+1), and
!                       mean = var = Y.

!             If RESOLVE is "high" then use max{YMEAN,YOBSERVED}
!               as surrogate for the mean.  For Poisson, var = mean,
!               therefore, choosing max{.} increases likelihood(IG|JSUB),
!               and increases chance for this support point to survive. 

              if (RPAR(k_resolve) .gt. OBSSWAPPOISSON ) then
                if (YOBSERVED .gt. YMEAN) then
                  meanobsswap = YOBSERVED
                  YOBSERVED = YMEAN
                  YMEAN = meanobsswap

                  write (*,*) JSUB,IG,"Swapped Poisson Obs"

                endif
              endif

              ObsError(I,J) = sqrt( YMEAN )

!           Use Normal approx. to Poisson
              IF (YMEAN > P_thresh) THEN

                NNORMALOBS = NNORMALOBS+1

                SIGFAC = SIGFAC * ObsError(I,J)
                RPAR(k_sum_z_sq) = RPAR(k_sum_z_sq) &
                  + ((YOBSERVED - YMEAN)/ObsError(I,J))**2

                if (DEBUG==1) then 
                  if (ig.eq.1) then
                    write (*,*) JSUB,IG,I,J,"is P_N",YMEAN,YOBSERVED, &
                      ObsError(I,J),RPAR(k_sum_z_sq)
                  endif
                endif

!           Use Poisson
              ELSE

                NPOISSONOBS = NPOISSONOBS + 1

                poissonprob = exp( -1.D0 * YMEAN ) &
                  * ( YMEAN**YOBSERVED ) / gamma( YOBSERVED + 1)

! wmy2019.01.29 -- if Pr -> 0, then don't count probability in product.
!   i.e. ignore this observation.
  
                if (poissonprob .gt. 0.0000001 ) then

! wmy2019.02.05 
!   prod of probabilities of Poisson sampled data almost always -> 0.D0.
!   so use RPAR(k_prod_pr) = \sum log10(poissonprob); thus, in main
!   the prod(probabilities) is 10^RPAR(k_prod_pr).

                    RPAR(k_prod_pr) = RPAR(k_prod_pr) + log10(PoissonProb)
                    if (DEBUG==9) then
                      write (*,*) JSUB,IG,I,J,"CAUGHT PoissonProb=",PoissonProb
                    endif
                else
                    if (DEBUG==9) then
                      write (*,*) JSUB,IG,I,J,"PoissonProb -> 0.D0",PoissonProb
                    endif
                endif

                if (DEBUG==99) then
                  if (ig.eq.1) then
                    write (*,*) "JSUB,IG,M,EQN",JSUB,IG,I,J &
                   , "is P w/mu,y,err,z^2,Pr=" &
                   , YMEAN,YOBSERVED,ObsError(I,J) &
                   , RPAR(k_sum_z_sq),poissonprob
                  endif
                endif

              ENDIF

! YOBSERVED ~ NORMAL ---------------------------------------------------

            ELSE

              if ( RPAR(k_resolve) > OBSSWAPNORMAL ) then ! use YOBS as surrogate for mean
                  meanobsswap = YOBSERVED
                  YOBSERVED = YMEAN
                  YMEAN = meanobsswap
              endif

!       if(ierrmod.eq.1) sig(i,j) = ...

              ObsError(I,J) = RPAR(J+k_c0_base) &
                + RPAR(J+k_c1_base)*YMEAN &
                + RPAR(J+k_c2_base)*YMEAN*YMEAN &
                + RPAR(J+k_c3_base)*YMEAN*YMEAN*YMEAN

!  NOTE THAT, THEORETICALLY, SIG SHOULD BE A CUBIC FNT. OF THE 'TRUE'
!  OBSERVED VALUES, NOT THE 'NOISY' OBSERVED VALUES or the predictions
!  (BUT THE 'TRUE' VALUES ARE UNKNOWN).
 
!       if(ierrmod.eq.2) sig(i,j) = sig(i,j)*gamma
!       if(ierrmod.eq.3) sig(i,j) = dsqrt(sig(i,j)**2 + gamma**2)
!       if(ierrmod.eq.4) sig(i,j) = gamma*flat

              if(IPAR(i_errmod).eq.2) ObsError(i,j) = &
                  ObsError(i,j)*RPAR(k_gamma)
              if(IPAR(i_errmod).eq.3) ObsError(i,j) = &
                  dsqrt(ObsError(i,j)**2 + RPAR(k_gamma)**2)
              if(IPAR(i_errmod).eq.4) ObsError(i,j) = &
                  RPAR(k_gamma)*RPAR(k_flat)

              IF(ObsError(I,J) .EQ. 0) THEN
                WRITE(*,2345) JSUB
                WRITE(25,2345) JSUB
2345            FORMAT(//' A S.D. IS 0 FOR JSUB = ',I5,'. RERUN THE &
                           PROGRAM WITH C0 NOT = 0 FOR THIS SUBJECT, &
                           OR WITH THIS SUBJECT ELIMINATED.')
                CLOSE(27)
                CLOSE(25)

!  ABNORMAL TERMINATION; WRITE THE ERROR MESSAGE TO ERRFIL.

                OPEN(42,FILE=ERRFIL)
                WRITE(42,2345) JSUB
                CLOSE(42)

                CALL PAUSE
                STOP
              ENDIF
 
              IF(ObsError(I,J) .LT. 0) THEN

                WRITE(*,2346) JSUB,IG
                WRITE(25,2346) JSUB,IG
2346            FORMAT(//' A S.D. < 0 FOR JSUB,IG = ',I5,I6,'. &
                   RERUN THE PROGRAM WITH A BETTER CHOICE FOR THE &
                   ASSAY ERROR POLYNOMIAL COEFFICIENTS.')
                CLOSE(27)
                CLOSE(25)

! ABNORMAL TERMINATION; WRITE THE ERROR MESSAGE TO ERRFIL.

                OPEN(42,FILE=ERRFIL)
                WRITE(42,2346) JSUB,IG
                CLOSE(42)

                CALL PAUSE
                STOP
              ENDIF

!--------------  SIGFAC and NNORMALOBS

              SIGFAC=SIGFAC*ObsError(I,J)
              NNORMALOBS=NNORMALOBS+1

              RPAR(k_sum_z_sq) = RPAR(k_sum_z_sq)  &
                 + ((YOBSERVED-YMEAN)/ObsError(I,J))**2

              if (DEBUG==10) then
                if (ig.eq.1) then
                  write (*,*) "JSUB,IG,M,EQN",JSUB,IG,I,J &
                   , "is N w/mu,obs,err,z^2=" &
                   , YMEAN,YOBSERVED,RPAR(k_gamma),ObsError(I,J) &
                   , RPAR(k_sum_z_sq)
                endif
              endif

            endif ! if(Poisson){...} else {Normal}

            ZSCORE=(YOBSERVED-YMEAN)/ObsError(I,J)
            F(k)=ZSCORE
!            F((J-1)*INTLIST(10)+I)=ZSCORE

            if (DEBUG == 7) then
              write (*,*) JSUB,IG,I,J,YOBSERVED,YMEAN,zscore
            endif

          ENDIF ! if(YO(I,J) .EQ. -99){...} else {YO ~ {Normal,Poisson}}

          if (DEBUG==11) then
              write (*,*) "idm01",I,J,ZSCORE,RPAR(k_prod_pr) &
                , YMEAN, YOBSERVED, ObsError(I,J), YO(I,J), Y(I,J)
          endif

! 140 CONTINUE  ! is replaced by two END DO
          END DO ! number of measurements
        END DO  ! number of output equations
! -----------------------------------------------------------------------

! Final checks on NNORMALOBS, NPOISSONOBS, MISVAL, SIGFAC, and OFAC
! Copy values into appropriate IPAR and RPAR bins for communication 
! back up to main.

! RPAR(k_prod_pr) = \prod Poisson Probs
! RPAR(k_sum_z_sq) = \sum Squared Normalized deviations for Y_obs ~ Normal
        RPAR(k_sfac) = SIGFAC
        RPAR(k_ofac) = 2.506628274631**NNORMALOBS ! = OFAC
        IPAR(i_misval) = MISVAL
        IPAR(i_Nnormalobs) = NNORMALOBS
        IPAR(i_Npoissonobs) = NPOISSONOBS

      RETURN

      end subroutine DO140 
!------------------------------------------------------------------------------------------------------------------


      subroutine cp_lrcs_to_rpar(gamma,flat,numeqt,c0,c1,c2,c3,c4,c5,rpar)

      implicit none

! Copies regression error arguments to rpar, placing them immediately
! after IG (see k_xxx above).  The constants are stored in k_c_gamma,
! k_c_flat, and k_c_start = k_flat + 1 to k_c_end = k_flat + maxnumeq * 4,
! as cat(gamma,flat,C0,C1,C2,C3).

      double precision gamma, flat
      integer numeqt
      double precision c0(:), c1(:), c2(:), c3(:), c4(:), c5(:), rpar(:)

      integer III
!     integer Ic0base,Ic1base,Ic2base,Ic3base

! Verify that NUMEQT == 7
        if (numeqt.gt.maxnumeq) then
           write (*,*) "ERROR :: NUMEQT.gt.maxnumeq",   &
             NUMEQT
           return
        end if

        RPAR(k_gamma) = gamma
        RPAR(k_flat) = flat

!        write (*,*) k_p_start, k_p_end, k_r_start, k_r_end
!        write (*,*) k_jsub, k_ig, k_gamma, k_flat
!        write (*,*) k_c_start, k_c_end, k_sfac, k_ofac

!        Ic0base = k_flat
!        Ic1base = Ic0base + maxnumeq
!        Ic2base = Ic1base + maxnumeq
!        Ic3base = Ic2base + maxnumeq
!        write (*,*) "base:", Ic0base, Ic1base, Ic2base, Ic3base
        do III=1,maxnumeq
           if (III.le.numeqt) then
             RPAR(k_c0_base + III) = C0(III)
             RPAR(k_c1_base + III) = C1(III)
             RPAR(k_c2_base + III) = C2(III)
             RPAR(k_c3_base + III) = C3(III)
             RPAR(k_c4_base + III) = C4(III)
             RPAR(k_c5_base + III) = C5(III)
           else
             RPAR(k_c0_base + III) = 0.d0
             RPAR(k_c1_base + III) = 0.d0
             RPAR(k_c2_base + III) = 0.d0
             RPAR(k_c3_base + III) = 0.d0
             RPAR(k_c4_base + III) = 0.d0
             RPAR(k_c5_base + III) = 0.d0
           end if
!           write (*,*) "cp:", RPAR(k_c0_base + III), RPAR(k_c1_base + III), &
!             RPAR(k_c2_base + III), RPAR(k_c3_base + III)
        end do
      end subroutine cp_lrcs_to_rpar
! #################################################################### ! 
      logical function check_input_array_size(                &
       MAXSUB,MAXGRD,MAXDIM,MAXACT,NUMEQT,MAXOBS,WORK,WORKK,  &
       SPXGYJ,DXI,PYJGX,PYJGXX,DENSTOR,EXX,CORDEN,CORHOLD,    &
       YPREDPOP,YPREDPOPT,YPREDBAY,CORDLAST)

      implicit none

      integer maxsub,maxgrd,maxdim,maxact,numeqt,maxobs
      double precision, intent(in), dimension(1:) :: work,workk,spxgyj,dxi
      double precision, intent(in), dimension(1:,1:) :: pyjgx
      double precision, intent(in), dimension(1:) :: pyjgxx
      double precision, intent(in), dimension(1:,1:) :: denstor
      double precision, intent(in), dimension(1:,1:,1:) :: exx
      double precision, intent(in), dimension(1:,1:) :: corden,corhold
      double precision, intent(in), dimension(1:,1:,1:,1:) :: ypredpop,ypredpopt,ypredbay
      double precision, intent(in), dimension(1:,1:) :: cordlast

! Below are the declared dimensions in subroutine NPAG. We only
! neec to verify the six input integers are .le. the maximums
! declared in the module, above. But to be careful, this 
! function will check the actual memory allocated by each array.
!
!        DIMENSION WORK(MAXGRD),WORKK(MAXGRD),
!     1  SPXGYJ(MAXGRD),DXI(MAXGRD),PYJGX(MAXSUB,MAXACT),
!     2  PYJGXX(MAXACT),DENSTOR(MAXGRD,4),
! double precision, dimension(MAXSUB,3,max_pop_rand_varbs) :: EXX
!     3  CORDEN(MAXGRD,MAXDIM+1),CORHOLD(MAXGRD,MAXDIM+1),
!     4  YPREDPOP(MAXSUB,NUMEQT,MAXOBS,3),
!     5  YPREDPOPT(MAXSUB,NUMEQT,7201,3),
!     6  YPREDBAY(MAXSUB,NUMEQT,MAXOBS,3),
!     7  CORDLAST(MAXGRD,MAXDIM+1)

        write (*,*) "MAXGRD =", MAXGRD
        write (*,*) "MAXSUB =", MAXSUB
        write (*,*) "MAXACT =", MAXACT
        write (*,*) "MAXDIM =", MAXDIM
        write (*,*) "NUMEQT =", NUMEQT
        write (*,*) "MAXOBS =", MAXOBS

        write (*,*) "size(work) =", shape(WORK)
        write (*,*) "size(workk) =", shape(WORKK)
        write (*,*) "size(spxgyj) =", shape(SPXGYJ)
        write (*,*) "size(dxi) =", shape(dxi)
        write (*,*) "size(pyjgx) =", shape(PYJGX)
        write (*,*) "size(pyjgxx) =", shape(PYJGXX)
        write (*,*) "size(denstor) =", shape(DENSTOR)
        write (*,*) "size(exx) =", shape(EXX)
        write (*,*) "size(corden) =", size(CORDEN)
        write (*,*) "size(corhold) =", size(CORHOLD)
        write (*,*) "shape(ypredpop) =", shape(YPREDPOP)
        write (*,*) "shape(ypredpopt) =", shape(YPREDPOPT)
        write (*,*) "shape(ypredbay) =", shape(YPREDBAY)
        write (*,*) "shape(cordlast) =", shape(CORDLAST)
        write (*,*) "size(shape(cordlast)) =", size(shape(CORDLAST))
        write (*,*) "size(cordlast,1) =", size(CORDLAST,1)
        write (*,*) "size(cordlast,2) =", size(CORDLAST,2)

        check_input_array_size = .true. ! Assume all array dimensions are .le. limit

      end function check_input_array_size

! ##################################################################### !
      subroutine makevec(NVAR,NOFIX,NRANFIX,IRAN,X,VALFIX,RANFIXEST,PX)

!  THIS ROUTINE, CALLED BY MAIN, INPUTS NVAR, NOFIX, NRANFIX, IRAN, X,
!  VALFIX, AND RANFIXEST, AND RETURNS PX(I) = A COMBINATION OF THE
!  VALUES IN X, VALFIX, AND RANFIXEST, IN THE PROPER ORDER (AS
!  DETERMINED BY IRAN).

!        IMPLICIT NONE

! wmy2018.9.22
! F77 parameter declarations replaced w/F90 assumed shape arrays
!       DIMENSION IRAN(max_ODE_params),X(max_pop_rand_varbs),
!         VALFIX(20),PX(max_ODE_params),RANFIXEST(20)

        integer nvar,nofix,nranfix
        integer, intent(in), dimension(1:) :: iran    
        double precision, intent(in), dimension(1:) ::  x,valfix,ranfixest
        double precision, intent(out), dimension(1:) :: px
! ---
        integer NNNVAR, NNNFIX, NNNRANFIX, I

        I = size(iran)
        if (I .gt. max_ODE_params) then
          write (*,*) "makevec() :: iran segvio"
        end if
        I = size(X)
        if (I .gt. max_pop_rand_varbs) then
          write (*,*) "makevec() :: x segvio"
        end if
        I = size(valfix)
        if (I .gt. max_pop_params) then
          write (*,*) "makevec() :: valfix segvio"
        end if
        I = size(ranfixest)
        if (I .gt. max_pop_varbs) then
          write (*,*) "makevec() :: ranfixest segvio"
        end if
        if (NVAR+NOFIX+NRANFIX .gt. max_ODE_params) then
          write(*,*) "makevec() :: PX segvio"
        end if

        NNNVAR = 0
        NNNFIX = 0
        NNNRANFIX = 0

        DO I = 1,NVAR+NOFIX+NRANFIX

        IF(IRAN(I) .EQ. 1) THEN
          NNNVAR = NNNVAR+1
          PX(I) = X(NNNVAR)
        ENDIF

        IF(IRAN(I) .EQ. 0) THEN
          NNNFIX = NNNFIX+1
          PX(I) = VALFIX(NNNFIX)
        ENDIF

        IF(IRAN(I) .EQ. 2) THEN
          NNNRANFIX = NNNRANFIX+1
          PX(I) = RANFIXEST(NNNRANFIX)
        ENDIF

        END DO

        RETURN
      end subroutine
! ##################################################################### !

! expand.f90
!
! Expansion algorithm in NPAG
!
! Contains subroutine expand_grid(), function checkd()
!
!------------ end subroutine checkd() ----------------------------------

      integer function checkd(corden,new,nactveold,nvar,ab)

      implicit none ! real*8 (a-h,o-z)

      double precision, dimension(1:,1:), intent(in) :: corden
      integer, intent(in) :: new, nactveold, nvar
      double precision, dimension(1:,1:), intent(in) :: ab

      integer i, iclose, ibas
      double precision sum

        iclose=0
        do ibas=1,nactveold
          sum=0.
          do i=1,nvar
            sum=sum+abs(corden(new,i)-corden(ibas,i))/(ab(i,2)-ab(i,1))
          enddo
          if(sum.le.1.d-4) then
            iclose=1
          endif
        enddo

        checkd = iclose ! if good point then 0, else 1

      end function checkd

!------------ end subroutine checkd() ---------------------------
!------------ subroutine expand_grid() ---------------------------

      subroutine expand_grid(alg_type,isupres,nvar,nactve,ngridn, &
        resolve, corden, ab)

        implicit none

! Args
        integer, intent(in) :: alg_type, isupres, nvar
        integer, intent(inout) :: nactve, ngridn
        double precision, intent(in) :: resolve
        double precision, intent(inout), dimension(1:,1:) :: corden
        double precision, intent(in), dimension(1:,1:) :: ab

! Local variable declarations
        integer ntry, nactveold, new, ipoint, ivar, i, iclose
        double precision pcur, del

        IF(ISUPRES .EQ. 0) write(*,*) &
          'Number of active points =', nactve

! Add points near the current solution

        IF(ISUPRES .EQ. 0) &
          write(*,*) 'expanding current grid with new points'
        IF(ISUPRES .EQ. 0) write(*,5200) 100.*resolve
 5200     format(' current grid resolution = ',f8.3, '%')
 
        new=2*nvar+1

        nactveold=nactve

        do ipoint=1,nactveold

! first, divide current probability into 2*nvar+1 pieces

          pcur=corden(ipoint,nvar+1)/(2*nvar+1)

! update original point
          corden(ipoint,nvar+1)=pcur

          do ivar=1,nvar
	    del=(ab(ivar,2)-ab(ivar,1))*resolve

! create first new trial point at -eps in coordinate ivar
            do i=1,nvar
              corden(nactve+1,i)=corden(ipoint,i)
	    enddo
	    corden(nactve+1,ivar)=corden(nactve+1,ivar)-del
            corden(nactve+1,nvar+1)=pcur
            ntry=nactve+1

! icheck that new point is at least minimally distant from old points

            iclose = checkd(corden,ntry,nactve,nvar,ab)

! only keep trial lower point if it lies above lower bound and satisfies
! minimal distance requirement
            if(corden(nactve+1,ivar).ge.ab(ivar,1)) then
              if(iclose.eq.0) nactve=nactve+1
	    endif

! now create second trial point at +eps in coordinate ivar
            do i=1,nvar
              corden(nactve+1,i)=corden(ipoint,i)
	    enddo
	    corden(nactve+1,ivar)=corden(nactve+1,ivar)+del
            corden(nactve+1,nvar+1)=pcur

! only keep upper point if it lies below upper bound and
! satisfies distance requirement
            ntry=nactve+1
            iclose = checkd(corden,ntry,nactve,nvar,ab)
	    if(corden(nactve+1,ivar).le.ab(ivar,2)) then

	      if(iclose.eq.0) nactve=nactve+1

            endif

          enddo
!    above enddo for loop over ivar=1,nvar

        enddo
!    above enddo for loop over ipoint=1,nactveold

        IF(ISUPRES .EQ. 0) write (*,*) &
          'Number of active grid points after expansion =', nactve
        IF(ISUPRES .EQ. 0) write(*,*)

        ngridn=nactve

! end expansion

      return
    end subroutine expand_grid

!------------ end expand_grid() -------------------------

!  shift10.f                                               11/21/14

!  shift10 has the following changes from shift9:

!  It has the Threadprivate and Save statements to make it compatible
!  with the new npageng28.f program. These statements allow the 
!  program to be run in parallel.

!-----------------------------------------------------------------------

!  shift9.f                                                9/28/12

!  shift9 has the following subtle change from shift8:

!  In step 4, the logic to assign the bolus time, BOL(I,IND,1) is
!  simplified in the case where a steady state dose set begins as a
!  time reset event. In this case, the bolus time will be TAU(I) only
!  if both TAU(I) and the bolus value (RR) are not 0. See the reason
!  in the code. 

!-----------------------------------------------------------------------

!  shift8.f                                                9/20/12

!  shift8 has changes from shift7 in Step 4 to correct the code in the
!  case where bolus inputs are used in steady state dose sets. In 
!  shift.f, a timelag for a bolus which was part of a steady state
!  dose set would not be applied properly. Now it will.

!-----------------------------------------------------------------------

!  shift7.f                                                11/6/11

!  shift7 differs from shift6 as follows:

!  1. The dimensions related to the no. of dose events are changed from
!  500 to 5000. This is needed as shift7 is compiled with idm1x7.f, 
!  idm2x7.f, and idm3x7.f (part of the npageng16.f "engine"), which
!  accommodates steady state dose sets.

!  2. 3 lines testing for IF(SIG(IDOSE) .EQ. 0 .AND. IDOSE .GT. 1)
!  are replaced by 	  IF(SIG(IDOSE) .LE. 0 .AND. IDOSE .GT. 1)
!  since now a dose reset occurs when a dose time is 0 (a regular
!  time reset) or < 0 (a time reset occurring with a steady state
!  dose set indicator).

!-----------------------------------------------------------------------

!  SHIFT6.F                                                4/26/11

!  SHIFT5 HAS THE FOLLOWING CHANGES TO SHIFT5:

!  WT AND CCR ARE NO LONGER ASSUMED TO BE SPECIAL COVARIATES IN EACH
!  PATIENT'S WORKING COPY PATIENT DATA FILE. SO ALL DO LOOPS THAT 
!  START WITH  DO I = 1, 2+NADD ARE CHANGED TO START WITH DO I = 1,NADD,
!  BUT ONLY IF NADD .GT. 0.

!-----------------------------------------------------------------------

!  SHIFT5.F							9/11/09

!  SHIFT5 HAS THE FOLLOWING CHANGES TO SHIFT4.F.


!  THE ARGUMENT LIST CONTAINS TAU(.) RATHER THAN NTLAG(.). THIS
!  MEANS THAT TAU(I) IS INPUT DIRECTLY AS THE TIMELAG FOR DRUG I.
!  I.E., IT NO LONGER HAS TO BE CALCULATED AS A FUNCTION OF THE
!  PARAMETER ARRAY, P. BECAUSE OF THIS, P IS REMOVED FROM THE ARGUMENT
!  LIST AND THE DIMENSION STATEMENT. ALSO, NTLAG IS REMOVED FROM 
!  THT DIMENSION STATEMENT.

!  THE FIRST SET OF ID MODULES TO CALL SHIFT5.F ARE idm1x3.f, 
!  idm2x3.f, AND idm3x3.f

!-----------------------------------------------------------------------

!  SHIFT4.FOR							9/1/09

!  SHIFT4 HAS THE FOLLOWING CHANGES FROM SHIFT3:

!  1. NTLAG(I) CAN NOW BE NEGATIVE. IF THIS OCCURS, IT MEANS THAT THE
!  TIMELAG PARAMETER FOR DRUG I WILL BE EXP(P(-NTLAG(I)).

!  2. A BUG IS CORRECTED RELATED TO TIME "RESETS". PREVIOUSLY, IF THE
!  USER HAD A TIME "RESET" IN HIS DOSAGE REGIMEN, THIS ROUTINE WOULD
!  NOT WORK. THE REASON IS THAT IN THE CODE BELOW, EACH NEXT TIME
!  FOR AN IV, COVARIATE, OR BOLUS IS COMPARED TO THE PREVIOUSLY
!  ESTABLISHED TIME IN THE DOSAGE ARRAY (TIMNXT) AND IS A CANDIDATE
!  TO BE THE NEXT TIMNXT IF IT IS .GE. TIMNXT. SO IF A TIME RESET
!  VALUE OF 0 OCCURS, IT WILL NEVER BE A CANDIATE SINCE IT IS NOT
!  .GE. THE LAST TIMNXT. TO FIX THIS, AND MAKE SURE THAT A TIME
!  RESET VALUE OF 0 IS INCLUDED IN THE ADJUSTED DOSAGE BLOCK, THE
!  CODE WILL ADD TO EACH IV, BOLUS, AND COVARIATE ARRAY AN EXTRA
!  LINE WHEN A TIME RESET OCCURS. THIS LINE WILL HAVE A TIME OF
!  1.D19 (I.E., A LARGE VALUE WHICH REPRSENTS INFINITY); AND IT
!  WILL BE FOLLOWED BY A LINE WITH THE ADJUSTED RESET TIME (0 FOR
! AND COVARIATES, AND 0 + TAU(I) FOR BOLI.

!-----------------------------------------------------------------------

!  SHIFT3.FOR							5-23-02

!  SHIFT3 HAS MAJOR CHANGES FROM SHIFT2 TO ALLOW FOR MULTIPLE TIMELAGS,
!  ONE POTENTIALLY FOR EACH BOLUS INPUT OF UP TO NTAU DRUGS.

        SUBROUTINE SHIFT(TAU,ND,SIG,NDRUG,NADD,RS,INTLIST)

! wmy20190318 Moved into npag_utils last week.
!        use npag_utils, only: max_doses,max_RS_J,max_covs,max_input_dim

        IMPLICIT NONE

! Arg list
        double precision, intent(IN), dimension(1:)  :: tau
        integer, intent(INOUT) :: ND
        double precision, intent(INOUT), dimension(1:) :: SIG      ! dimension(max_doses) 
        integer, intent(IN) :: NDRUG, NADD
        double precision, intent(INOUT), dimension(1:,1:) :: RS    ! dimension(max_doses,max_RS_J)
        integer, intent(INOUT), dimension(1:) :: INTLIST           ! dimension(128)

! Local Variables
        integer, dimension(max_input_dim) :: INDIV, INDBOL
        integer, dimension(max_covs) :: INDCOV
        double precision, dimension(max_RS_J) :: TIMCAN

        integer I,IDOSE,IND,ISAME1,ISAME2,ISAME3,NI
        double precision RR,TIMNXT,VALAST

! NEW PARALLEL CODE BELOW AS OF npageng28.f.
        double precision, dimension(max_input_dim,max_doses,2), save :: & ! f90 line continuation
     &    XIV, BOL
        double precision, dimension(max_covs,max_doses,2), save :: COV
!$omp   Threadprivate(XIV,BOL,COV)


!  INPUT ARE:

!  TAU(I) =  THE VALUE OF THE TIMELAG FOR DRUG I.
!  ND = ORIGINAL NO. OF DOSE EVENTS. 
!  SIG(I) = TIME FOR ITH DOSE EVENT IN THE ORIGINAL DOSAGE REGIMEN,
!           I=1,ND.
!  NDRUG = NO. OF DRUGS (EACH HAS AN IV, FOLLOWED BY A BOLUS COLUMN).
!  NADD = NO. OF ADDITIONAL COVARIATES (EACH IS IN ITS OWN COLUMN
!         FOLLOWING THE IV/BOLUS COLUMNS.
!  RS(I,J) = "RATE" J FOR THE ITH DOSE EVENT IN THE ORIGINAL DOSAGE
!            REGIMEN; J=1,NI, I=1,ND, WHERE NI = 2*NDRUG + NADD
!            BECAUSE THE "RATES" CONTAIN, IN ORDER, 2 ENTRIES FOR
!            EACH DRUG (1 FOR THE IV AND 1 FOR THE BOLUS) AND 1 EACH
!            FOR THE NADD ADDITIONAL COVARIATES.


!  OUTPUT ARE:

!  ND, SIG, RS, AS ABOVE, EXCEPT FOR THE ALTERED DOSAGE REGIMEN.

!-----------------------------------------------------------------------

!  SHIFT2.FOR							11-16-99

!  SHIFT2 HAS THE FOLLOWING CHANGE FROM SHIFT. AT THE END OF THE 
!  FORMATION OF ARRAY XMAT, ALL ROWS WHICH HAVE 0 BOLUS INPUT AND THE
!  SAME OTHER DATA VALUES (EXCEPT TIME) AS THE PREVIOUS ROW ARE NOT
!  USED IN THE NEW ARRAY XMAT2 WHICH HAS ONLY NON-REDUNDANT ROWS.
!  THIS, THEORETICALLY, SHOULDN'T HAVE ANY EFFECT ON CALCULATIONS, BUT 
!  NUMERICALLY IT DOES SINCE WHEN THE DVODE ROUTINE SOLVES D.E.'S, IT 
!  INTEGRATES OVER DIFFERENT INTERVALS IF EXTRA DOSAGE LINES ARE 
!  INCLUDED.

!  EX: TIME   IV   BOLUS	TIME   IV   BOLUS
!       0    100     0		 0    100     0
!       5    100   1000		 2    100   1000  

!  NOTE THAT BOTH ABOVE CASES SHOULD GIVE THE SAME RESULTS IF THERE IS
!  A TIME-LAG = 3 IN THE 2ND CASE. BUT, AS THE CODE IS WRITTEN IN
!  SHIFT.FOR, THE 2ND CASE WOULD TRANSLATE TO THE FOLLOWING:

!	 TIME   IV   BOLUS 
!         0    100     0   
!         2    100     0
!         5    100   1000

!  ... AND THIS WOULD MEAN THAT THE 1ST INTEGRATION BY DVODE WOULD END
!      AT T = 2, RATHER THAN 5 (OR, E.G., 3 IF 3 WAS THE
!      FIRST OBSERVATION TIME). THIS CREATES NUMERICAL DIFFERENCES DUE
!      TO SMALL ROUNDOFF ERRORS WHICH CAN GROW SIGNIFICANTLY.

!-----------------------------------------------------------------------

!  SHIFT.FOR							7-27-99

!  SHIFT.FOR IS A MODULE WHICH INCLUDES SUBROUTINE SHIFT. SHIFT WILL BE
!  CALLED BY ROUTINES OF THE "BIG" NPEM AND IT2B PROGRAMS WHICH HAVE
!  SUBROUTINES FUNC, FUNC1, FUNC2, OR FUNC3 IN THEM.

!  SHIFT INPUTS THE DOSAGE REGIMEN VIA THE INPUT ARGUMENTS (SEE BELOW),
!  AND RETURNS AN ALTERED DOSAGE REGIMEN, WHICH HAS EACH BOLUS INPUT 
!  TIME INCREASED BY THE INPUT VALUE OF TAU (THE TIME LAG). NOTE THAT
!  EACH ROW WITH A NON-0 BOLUS INPUT VALUE WILL RESULT IN A NEW ROW IN
!  THE DOSAGE REGIMEN.

!-----------------------------------------------------------------------

!  PROCEDURE FOR THE DOSAGE REGIMEN MODIFICATION:

!  1. ESTABLISH TAU(I) AS THE TIMELAG FOR DRUG I'S BOLUS COLUMN.
!     NO. AS OF SHIFT5.F, THIS VALUE IS INPUT AS AN ARGUMENT.

!  2. ESTABLISH THE IV VALUES AND TIMES INTO XIV(I,J,K). IN PARTICULAR,
!     XIV(I,J,2) IS THE JTH IV VALUE FOR DRUG I, AND XIV(I,J,1) IS THE 
!     TIME THIS IV VALUE FIRST OCCURRED. SET THE LAST TIME TO 1.D29 AS
!     AN INDICATOR THAT THERE ARE NO MORE ENTRIES IN THE ARRAY.

!  3. ESTABLISH THE COVARIATE VALUES AND TIMES INTO COV(I,J,K). IN 
!     PARTICULAR, COV(I,J,2) IS THE JTH VALUE FOR COVARIATE I, AND 
!     COV(I,J,1) IS THE TIME THIS COV VALUE FIRST OCCURRED. SET THE 
!     LAST TIME TO 1.D29 AS AN INDICATOR THAT THERE ARE NO MORE ENTRIES
!     IN THE ARRAY.

!  4. ESTABLISH THE BOLUS VALUES AND TIMES INTO BOL(I,J,K).
!     IN PARTICULAR, BOL(I,J,2) IS THE JTH BOLUS VALUE FOR DRUG I, AND
!     BOL(I,J,1) IS THE TIME THIS BOLUS OCCURRED. THE TIMES FOR EACH
!     BOLUS VALUE ARE THOSE ADJUSTED TIMES FROM THE ASSOCIATED TIMELAGS
!     TAU(I),I=1,NDRUG, FROM STEP 1. SET THE LAST TIME TO 1.D29 AS AN
!     INDICATOR THAT THERE ARE NO MORE ENTRIES IN THE ARRAY.

!  5. REASSIGN THE VALUES IN IV, BOL, AND COV TO THE APPROPRIATE ENTRIES
!     OF RS, KEEPING TRACK OF THE RUNNING INDEX, ND, OF DOSE EVENTS. IF
!     ND EXCEEDS 5000, STOP THE PROGRAM WITH A MESSAGE TO THE USER. ALSO
!     REASSIGN THE CORRESPONDING TIME VALUES TO ARRAY SIG.


!  STEP 1.

! TO DO. AS OF SHIFT5.F, TAU(I), I=1,NDRUG, IS INPUT AS
!  AN ARGUMENT TO THIS ROUTINE.


! wmy2018.07.20 -- Changed tau(7) to tau(:)
        I = size(tau)
        IDOSE = size(cov,3)
        IND = size(rs,2)

        if (I.ne.max_input_dim) then
          write (*,*) "ERR: In SR SHIFT(), size(tlag).ne.max_input_dim"
          write (*,*) "size: tau,IDOSE_3, rs_2 =", I, IDOSE, IND
        endif

!  STEP 2:

!  ESTABLISH THE IV VALUES AND TIMES INTO XIV(I,J,K). IN PARTICULAR,
!  XIV(I,J,2) IS THE JTH IV VALUE FOR DRUG I, AND XIV(I,J,1) IS THE 
!  TIME THIS IV VALUE FIRST OCCURRED.

	DO I = 1,NDRUG

!  ESTABLISH XIV(I,J,K) FOR DRUG I'S IV. PRESET THE LAST VALUE TO
!  -99 SO THAT THE FIRST VALUE WILL BE DIFFERENT AND THEREFORE ENGAGE 
!  THE LOGIC (WHICH ONLY WRITES A ROW INTO THE ARRAY IF THE VALUE IS
!  DIFFERENT THAN THE PREVIOUS VALUE). 

!*** MODIFICATION IN SHIFT4.F: IF A TIME RESET OCCURS (I.E., A
!    SIG(IDOSE) = 0, WHERE IDOSE > 1), IT WILL BE HANDLED BY ASSIGNING 
!    AN EXTRA TIME VALUE OF 1.D19 (I.E., A LARGE VALUE REPRESENTING
!    TIME = INFINITY) TO THE IV TIME ARRAY. THEN THE REST OF THE
!    THE IV TIME ARRAY WILL BE ESTABLISHED WITH THE REST OF THE VALUES
!    IN SIG, STARTING, OF COURSE, WITH THE TIME RESET VALUE OF 0.

!    THE SAME LOGIC WILL APPLY TO THE COVARIATES AND THE BOLI.

!  NOTE THAT IND WILL BE THE RUNNING INDEX OF THE LATEST ENTRY INTO 
!  THE ARRAY. PLACE 1.D29 INTO THE LAST TIME ENTRY OF EACH SUB-ARRAY 
!  AS AN INDICATOR THAT THERE ARE NO MORE ENTRIES.

	 XIV(I,1,1) = 1.D29
	 IND = 0
	 VALAST = -99.D0

!  FOR DRUG I, THE IV VALUE IS IN COLUMN 2*I-1 OF ARRAY RS.
	  
	DO IDOSE = 1,ND

	  RR = RS(IDOSE,2*I-1)

!*** MODIFICATION IN SHIFT7.F: A TIME RESET IS NOW DESIGNATED BY A
!  SIG(IDOSE) .LE. 0, RATHER THAN JUST .EQ. 0 (SINCE A STEADY STATE
!  DOSE INDICATOR HAS A NEGATIVE DOSE TIME).

	  IF(SIG(IDOSE) .LE. 0 .AND. IDOSE .GT. 1) THEN

!  THIS REPRESENTS A TIME "RESET". IN THIS CASE, AS INDICATED ABOVE,
!  PUT IN AN EXTRA ROW FOR THE IV REPRESENTING A VERY LARGE TIME
!  AND THE SAME IV VALUE AS THE PREVIOUS VALUE. THEN PUT IN THE
!  LINE REPRESENTING THE RESET TIME OF 0.
	
	    IND = IND + 1
	    XIV(I,IND,1) = 1.D19
	    XIV(I,IND,2) = XIV(I,IND-1,2)

	    IND = IND + 1

!*** MODIFICATION IN SHIFT7.F. SET THE NEXT XIV(I,IND,1) TO BE
!  SIG(IDOSE), NOT 0, SINCE SIG(IDOSE) MAY BE < 0 (SINCE A STEADY STATE
!  DOSE INDICATOR HAS A NEGATIVE DOSE TIME).
 
	    XIV(I,IND,1) = SIG(IDOSE)
	    XIV(I,IND,2) = RR
	    XIV(I,IND+1,1) = 1.D29
	    VALAST = RR

	    GO TO 200

	  ENDIF

!  TO GET HERE, THIS DOSE LINE DOES NOT REPRESENT A TIME RESET.

	  IF(RR .NE. VALAST) THEN
         IND = IND + 1
	   XIV(I,IND,1) = SIG(IDOSE)
	   XIV(I,IND,2) = RR
	   XIV(I,IND+1,1) = 1.D29
	   VALAST = RR
	  ENDIF

  200     CONTINUE

	 END DO

!  THE ABOVE END DO IS FOR THE  DO IDOSE = 1,ND  LOOP.


	END DO

!  THE ABOVE END DO IS FOR THE 	DO I = 1,NDRUG  LOOP.


!  STEP 3:

!  ESTABLISH THE COVARIATE VALUES AND TIMES INTO COV(I,J,K). IN 
!  PARTICULAR, COV(I,J,2) IS THE JTH VALUE FOR COVARIATE I, AND 
!  COV(I,J,1) IS THE TIME THIS COV VALUE FIRST OCCURRED. SET THE 
!  LAST TIME TO 1.D29 AS AN INDICATOR THAT THERE ARE NO MORE ENTRIES
!  IN THE ARRAY.

        IF(NADD .GT. 0) THEN

	DO I = 1, NADD

!  ESTABLISH COV(I,J,K) FOR COVARIATE NO. I.
!  PRESET THE LAST VALUE TO -99 SO THAT THE FIRST VALUE WILL BE 
!  DIFFERENT AND THEREFORE ENGAGE THE LOGIC (WHICH ONLY WRITES A ROW 
!  INTO THE ARRAY IF THE VALUE IS DIFFERENT THAN THE PREVIOUS VALUE). 
!  NOTE THAT IND WILL BE THE RUNNING INDEX OF THE LATEST ENTRY INTO THE 
!  ARRAY. PLACE 1.D29 INTO THE LAST TIME ENTRY OF EACH SUB-ARRAY AS AN 
!  INDICATOR THAT THERE ARE NO MORE ENTRIES.

	 COV(I,1,1) = 1.D29
	 IND = 0
	 VALAST = -99.D0

!  FOR COVARIATE I, THE VALUE IS IN COLUMN 2*NDRUG+I OF ARRAY RS.	  

	 DO IDOSE = 1,ND

	  RR = RS(IDOSE,2*NDRUG+I)

!*** MODIFICATION IN SHIFT7.F: A TIME RESET IS NOW DESIGNATED BY A
!  SIG(IDOSE) .LE. 0, RATHER THAN JUST .EQ. 0 (SINCE A STEADY STATE
!  DOSE INDICATOR HAS A NEGATIVE DOSE TIME).

	  IF(SIG(IDOSE) .LE. 0 .AND. IDOSE .GT. 1) THEN

!  THIS REPRESENTS A TIME "RESET". IN THIS CASE, AS INDICATED ABOVE,
!  PUT IN AN EXTRA ROW FOR THE COVARIATE REPRESENTING A VERY LARGE TIME
!  AND THE SAME COV VALUE AS THE PREVIOUS VALUE. THEN PUT IN THE
!  LINE REPRESENTING THE RESET TIME OF 0.
	
	    IND = IND + 1
	    COV(I,IND,1) = 1.D19
	    COV(I,IND,2) = COV(I,IND-1,2)

	    IND = IND + 1

!*** MODIFICATION IN SHIFT7.F. SET THE NEXT COV(I,IND,1) TO BE
!  SIG(IDOSE), NOT 0, SINCE SIG(IDOSE) MAY BE < 0 (SINCE A STEADY STATE
!  DOSE INDICATOR HAS A NEGATIVE DOSE TIME).

	    COV(I,IND,1) = SIG(IDOSE) 
	    COV(I,IND,2) = RR
	    COV(I,IND+1,1) = 1.D29
	    VALAST = RR

	    GO TO 300

	  ENDIF

!  TO GET HERE, THIS DOSE LINE DOES NOT REPRESENT A TIME RESET.

	  IF(RR .NE. VALAST) THEN
           IND = IND + 1
	   COV(I,IND,1) = SIG(IDOSE)
	   COV(I,IND,2) = RR
	   COV(I,IND+1,1) = 1.D29
	   VALAST = RR
	  ENDIF

  300     CONTINUE

	 END DO

!  THE ABOVE END DO IS FOR THE   DO IDOSE = 1,ND  LOOP.

	END DO

!  THE ABOVE END DO IS FOR THE  DO I = 1, NADD  LOOP.

        ENDIF

!  THE ABOVE ENDIF IS FOR THE   IF(NADD .GT. 0)  CONDITION.



!  STEP 4:

!  ESTABLISH THE BOLUS VALUES AND TIMES INTO BOL(I,J,K). IN PARTICULAR, 
!  BOL(I,J,2) IS THE JTH BOLUS VALUE FOR DRUG I, AND BOL(I,J,1) IS THE
!  ADJUSTED (USING THE ASSOCIATED TIMELAGS TAU(I),I=1,NDRUG) TIME THIS 
!  BOLUS OCCURRED. 

	DO I = 1,NDRUG

!  ESTABLISH BOL(I,J,K) FOR DRUG I'S BOLUS. EACH ARRAY IS FILLED ONLY
!  WITH NON-0 BOLUS VALUES. NOTE THAT IND WILL BE THE RUNNING INDEX OF 
!  THE LATEST ENTRY INTO THE ARRAY. PLACE 1.D29 INTO THE LAST TIME ENTRY 
!  OF EACH SUB-ARRAY AS AN INDICATOR THAT THERE ARE NO MORE ENTRIES.

	 BOL(I,1,1) = 1.D29
	 IND = 0

!  FOR DRUG I, THE BOLUS VALUE IS IN COLUMN 2*I OF ARRAY RS.	  

	 DO IDOSE = 1,ND

	  RR = RS(IDOSE,2*I)

!*** MODIFICATION IN SHIFT7.F: A TIME RESET IS NOW DESIGNATED BY A
!  SIG(IDOSE) .LE. 0, RATHER THAN JUST .EQ. 0 (SINCE A STEADY STATE
!  DOSE INDICATOR HAS A NEGATIVE DOSE TIME).

	  IF(SIG(IDOSE) .LE. 0 .AND. IDOSE .GT. 1) THEN

!  THIS REPRESENTS A TIME "RESET". IN THIS CASE, AS INDICATED ABOVE,
!  PUT IN AN EXTRA ROW FOR THE BOLUS REPRESENTING A VERY LARGE TIME
!  AND AN ACCOMPANYING BOLUS VALUE OF 0. THEN PUT IN THE
!  LINE REPRESENTING THE RESET TIME OF 0 + THE TIMELAG ... IF
!  RR .NE. 0.
	
	    IND = IND + 1
	    BOL(I,IND,1) = 1.D19
	    BOL(I,IND,2) = 0.D0

	    IND = IND + 1


!*** THE FOLLOWING CODE IS CHANGED IN SHIFT8.F. NOW BOLUS VALUES 
!  WORK PROPERLY EVEN WITH TIMELAGS. AND AN ADDITIONAL SUBTLE CHANGE
!  WAS ADDED IN shift9.f (SEE THE COMMENTS AT THE TOP OF shift9.f),
!  AND THE EXTRA COMMENTS BELOW.


!  LOGIC IS NOW AS FOLLOWS:

!  IF SIG(IDOSE) = 0, THIS IS A TIME RESET WHICH IS NOT THE START OF
!     A STEADY STATE DOSE SET. IN THIS CASE, A BOLUS WITH A TIMELAG OF
!     TAU(I) WILL OCCUR AT SIG(IDOSE) + TAU(I) = TAU(I).

!  IF SIG(IDOSE) < 0, THIS IS A TIME RESET WHICH IS THE START OF A
!     STEADY STATE DOSE SET. IN THIS CASE:
!     THE BOLUS TIME WILL BE TAU(I) ONLY IF BOTH TAU(I) AND RR
!     ARE NOT 0. OTHERWISE, IT WILL BE SIG(IDOSE).
!     REASON: IF RR = 0, THERE IS NO BOLUS TO BE GIVEN, SO IT WOULD
!     BE SILLY TO INCLUDE AN EXTRA LINE IN THE DOSAGE REGIMEN WITH
!     A 0 BOLUS (AND IT WOULD VERY SLIGHTLY CHANGE THE RESULTS SINCE
!     THE NUMERICAL INTEGRATION THEN HAS TO INTEGRATE THROUGH AN EXTRA
!     TIME). IN AN EXAMPLE (REMARK 4.b IN NPAG109.EXP, THIS CHANGED THE
!     VALUES IN THE LOG-LIKELIHOODS OUT IN THE 13TH DIGIT, BUT SOME 
!     VALUES IN THE DENSITY FILE WERE CHANGED IN THE 4TH DIGIT).

!     ALSO, IF TAU(I) = 0, THE BOLUS HAS NO TIMELAG AND THEREFORE
!     OCCURS AT SIG(IDOSE).

!  THE FOLLOWING EXAMPLE SHOWS WHY A NON-0 BOLUS IN A STEADY STATE DOSE
!  SET, WITH TAU(I) .NE. 0, MUST BE GIVEN AT TAU(I) AND NOT
!  SIG(IDOSE) + TAU(I).

!  EX: IF SIG(IDOSE) = -12, IT MEANS THAT A STEADY STATE DOSE SET IS
!      STARTING WITH AN INTERDOSE INTERVAL OF 12 HOURS. SO, IF A 
!      BOLUS WITH A TLAG OF 1.5 HOURS IS GIVEN, ITS TIME MUST BE
!      1.5, NOT -12 + 1.5 = -10.5. REASON: AFTER THE SIG(IDOSE) OF
!      -12 IS CONVERTED IN SUBROUTINE FUNC2 TO 0, THE 1.5 WILL CORRECTLY
!      INDICATE THAT THE BOLUS IS GIVEN 1.5 HOURS AFTER THE START OF THE
!      STEADY STATE DOSE SET. ALSO, A TIME OF -10.5 WOULD COMPLETELY
!      SCREW UP THE FUNC2 LOGIC WHICH WOULD INTERPRET IT AS THE START
!      OF ANOTHER STEADY STATE DOSE SEST.

!      ON THE OTHER HAND, IF A DRUG HAS A TAU(I) = 0, IT CANNOT SHOW
!      UP AS OCCURRING AT TAU(I) = 0 SINCE THIS WILL COMPLETELY SCREW
!      UP FUNC2'S LOGIC, WHICH WILL INTERPRET THE TIME OF 0 AS A
!      TIME RESET EVENT. IN THIS CASE, THE BOLUS OCCURS AT THE START OF
!      THE STEADY STATE DOSE SET, I.E., AT SIG(IDOSE) = -12, WHICH WILL
!      BE CONVERTED TO 0 BY FUNC2).


      CALL THESAME(SIG(IDOSE),0.D0,ISAME1)
      CALL THESAME(TAU(I),0.D0,ISAME2)
      CALL THESAME(RR,0.D0,ISAME3)

      IF(ISAME1 .EQ. 1) BOL(I,IND,1) = TAU(I)
!  NOTE THAT, TECHNICALLY, WE SHOULD SET BOL(I,IND,1) = SIG(IDOSE) = 0
!  IF RR = 0, SINCE THERE IS NO REASON TO HAVE AN EXTRA LINE IN THE
!  DOSAGE REGIMEN FOR A 0 BOLUS ... BUT CHANGING THIS WOULD CHANGE
!  VERY SLIGHTLY THE RESULTS IN A 0 BOLUS CASE SINCE THERE WOULD BE ONE
!  LESS DOSAGE LINE FOR THE NUMERICAL INTEGRATOR TO INTEGRATE THROUGH,
!  SO THE CODE WILL BE LEFT AS IS, FOR CONSISTENCY SAKE.


      IF(ISAME1 .EQ. 0) THEN
       BOL(I,IND,1) = SIG(IDOSE)
       IF(ISAME2 .EQ. 0 .AND. ISAME3 .EQ. 0) BOL(I,IND,1) = TAU(I)
      ENDIF



	    BOL(I,IND,2) = RR
	    BOL(I,IND+1,1) = 1.D29
	    VALAST = RR

	    GO TO 400

	  ENDIF

!  TO GET HERE, THIS DOSE LINE DOES NOT REPRESENT A TIME RESET.


	  IF(RR .NE. 0.D0) THEN

           IND = IND + 1

!  *** CHANGE FOR SHIFT8.F.
!  NOW BOLUS VALUES CAN OCCUR IN STEADY STATE DOSES. AND IF THEY DO,
!  THE FIRST ONE MUST OCCUR AT TIME TAU(I), NOT SIG(IDOSE) + TAU(I)
!  AS THE FOLLOWING EXAMPLE ILLUSTRATES:
!  EX: SIG(1) = -12 INDICATING THAT THE STEADY STATE DOSE SET HAS
!      AN INTERDOSE INTERVAL OF 12 HOURS. TAU(1) = 1.5 -->
!      DRUG 1 HAS A TIMELAG OF 1.5 HOURS. SO, IF THE FIRST BOLUS TIME IS
!      SET =  SIG(1) + TAU(1) = -12 + 1.5 = -10.5, THIS WILL SCREW
!      UP THE FUNC2 LOGIC SINCE IN THAT CODE, THE FIRST TIME OF
!      -12 WILL BE RESET TO BE 0, AND THIS WILL BE FOLLOWED BY -10.5,
!      WHICH WILL LOOK LIKE THE START OF ANOTHER STEADY STATE DOSE
!      SET. INSTEAD, SET FIRST BOLUS TIME = TAU(1) = 1.5, WHICH IS
!      CORRECT SINCE IT OCCURS 1.5 HOURS AFTER THE STEADY STATE DOSE
!      STARTS.

         IF(SIG(IDOSE) .GE. 0.D0) BOL(I,IND,1) = SIG(IDOSE) + TAU(I)
         IF(SIG(IDOSE) .LT. 0.D0) BOL(I,IND,1) = TAU(I)

	   BOL(I,IND,2) = RR
	   BOL(I,IND+1,1) = 1.D29
	  ENDIF

  400     CONTINUE

	 END DO

!  THE ABOVE END DO IS FOR THE  DO IDOSE = 1,ND  LOOP.


	END DO


!  THE ABOVE END DO IS FOR THE  DO I = 1,NDRUG  LOOP.



!  STEP 5:

!  REASSIGN THE VALUES IN IV, BOL, AND COV TO THE APPROPRIATE ENTRIES
!  OF RS, KEEPING TRACK OF THE RUNNING INDEX, ND, OF DOSE EVENTS. IF
!  ND EXCEEDS 5000, STOP THE PROGRAM WITH A MESSAGE TO THE USER. ALSO,
!  REASSIGN THE CORRESPONDING TIME VALUES TO ARRAY SIG.

	NI = 2*NDRUG + NADD
	ND = 0

!  GO THROUGH THE ARRAYS IV, BOL, AND COV TO DETERMINE THE NEXT
!  LOWEST DOSE TIME. PUT THIS VALUE INTO RS, ALONG WITH THE 
!  CORRESPONDING VALUES FOR THE IV'S, THE BOLI, AND THE COVARIATES.

!  IN THE LOOP BELOW, IT IS NECESSARY TO KNOW TO WHAT POINT IN THE
!  IV, BOL, AND COV ARRAYS THE TIMES AND VALUES HAVE ALREADY BEEN 
!  STORED INTO RS. THESE INDICES ARE INDIV(I), I=1,NDRUG; INDBOL(I),
!  I=1,NDRUG; AND INDCOV(I), I=1,NADD, RESPECTIVELY. E.G., 
!  INDIV(2) = 4 MEANS THAT ALL VALUES IN THE IV, BOL, AND COV ARRAYS, 
!  THROUGH THE 4TH TIME FOR IV DRUG 2 (I.E., THROUGH TIME = XIV(2,4,1))
!  HAVE BEEN OR ARE ABOUT TO BE STORED INTO THE RS ARRAY.

!  SO PRESET ALL THESE INDEX INDICATORS = 1, AND INITIALIZE THE 
!  CURRENT DOSE TIME TO A NEGATIVE NO. SO THAT THE FIRST TIME
!  THROUGH THE FOLLOWING LOOP WILL ENGAGE THE LOGIC.

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

!  FIND THE NEXT LOWEST TIME AMONG THE IV, BOL, AND COV ARRAYS.

!  ESTABLISH INTO TIMCAN(J) THE CANDIDATES FOR THE NEXT DOSE TIME
!  (AND CORRESPONDING VALUES FOR THE IV'S, BOLI, AND COVARIATES) TO
!  BE PUT INTO RS.


        DO I = 1,NDRUG
	 IF(XIV(I,INDIV(I),1) .GT. TIMNXT) TIMCAN(I)=XIV(I,INDIV(I),1)
	 IF(XIV(I,INDIV(I),1) .EQ. TIMNXT) TIMCAN(I)=XIV(I,INDIV(I)+1,1)
	END DO

        DO I = 1,NDRUG
	 IF(BOL(I,INDBOL(I),1) .GT. TIMNXT) TIMCAN(NDRUG+I) =           &
     &    BOL(I,INDBOL(I),1)
	 IF(BOL(I,INDBOL(I),1) .EQ. TIMNXT) TIMCAN(NDRUG+I) =           &
     &    BOL(I,INDBOL(I)+1,1)
	END DO


        IF(NADD .GT. 0) THEN
         DO I = 1,NADD
          IF(COV(I,INDCOV(I),1) .GT. TIMNXT) TIMCAN(2*NDRUG+I) =        &
     &     COV(I,INDCOV(I),1)
          IF(COV(I,INDCOV(I),1) .EQ. TIMNXT) TIMCAN(2*NDRUG+I) =        &
     &     COV(I,INDCOV(I)+1,1)
         END DO
        ENDIF

!  FIND THE NEXT TIMNXT, THE MINIMUM VALUE AMONG THE NI ENTRIES IN 
!  TIMCAN. TIMNXT WILL BE THE NEXT TIME TO BE PUT INTO ARRAY RS (ALONG 
!  WITH ALL THE CORRESPONDING IV'S, BOLI, AND COVARIATE VALUES). IF 
!  TIMNXT = 1.D29, IT IS BECAUSE THERE ARE NO FURTHER VALUES TO BE PUT 
!  INTO RS (I.E, THE PROCESS IS FINISHED).

	TIMNXT = TIMCAN(1)
	DO I = 2,NI
	 IF(TIMCAN(I) .LT. TIMNXT) TIMNXT = TIMCAN(I)
	END DO

	IF(TIMNXT .EQ. 1.D29) RETURN

!  SINCE TIMNXT < 1.D29, THERE ARE MORE VALUES TO BE PUT INTO RS.
!  GO THROUGH ALL THE SUBARRAYS AND PUT IN VALUES AS FOLLOWS. IF THE
!  CURRENT TIME FOR AN IV, BOLUS, OR COVARIATE IS THE SAME AS TIMNXT, 
!  PUT THE CORRESPONDING IV, BOLUS, OR COVARIATE VALUE INTO RS, AND 
!  INCREASE THE INDEX FOR THAT SUB-ARRAY TO THE NEXT VALUE. IF THE
!  CURRENT TIME FOR AN IV OR A COVARIATE IS .GT. TIMNXT, PUT THE IV OR 
!  COVARIATE VALUE FROM THE PREVIOUS ROW INTO RS, AND LEAVE THE INDEX 
!  UNCHANGED. IF THE CURRENT TIME FOR A BOLUS IS .GT. TIMNXT, PUT 0.0 
!  INTO RS (I.E., BOLUS VALUES ARE INSTANTANEOUS, WHEREAS IV AND 
!  COVARIATE VALUES CONTINUE UNTIL CHANGED), AND LEAVE THE INDEX
!  UNCHANGED.


!  TEST FOR TIMNXT = 1.D19, WHICH INDICATES A TIME RESET.

	IF(TIMNXT .EQ. 1.D19) THEN

!  TIMNXT = 1.D19 MEANS THAT THE NEXT TIME IN EACH ARRAY IS THE
!  TIME AT OR AFTER THE RESET. SO INCRASE ALL THE ARRAY INDICES BY
!  1, RESET TIMNXT TO A NEGATIVE NO. AND RETURN TO LABEL 100.

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

	IF(ND .GT. max_doses) THEN

!  IF ND > max_doses, STOP WITH A MESSAGE TO THE USER THAT THE
!  PROGRAM ONLY ALLOWS A TOTAL OF 5000 DOSE EVENTS.

   10	 WRITE(*,1) ND
    1    FORMAT(/' THE NUMBER OF DOSE EVENTS, AFTER TAKING INTO'/       &
     &' ACCOUNT DIFFERING TIMES DUE TO TIMELAGS IS ',I6,', MORE THAN'/  &
     &' THE ALLOWABLE MAXIMUM OF 5000. THE PROGRAM IS STOPPING. PLEASE'/&
     &' RERUN WITH PATIENTS HAVING FEWER DOSE EVENTS, OR WITH FEWER'/   &
     &' TIMELAG VALUES SELECTED AS FIXED OR RANDOM PARAMETERS.'//)
	 STOP

	ENDIF

!  ND .LE. max_doses, SO CONTINUE. FOR THIS DOSE EVENT, PUT IN THE CURRENT 
!  TIME, AND THE CORRESPONDING IV, BOLUS, AND COVARIATE VALUES. 


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
          IF(TIMNXT .LT. COV(I,INDCOV(I),1))                            &
     &     RS(ND,2*NDRUG+I) = RS(ND-1,2*NDRUG+I)
          IF(TIMNXT .EQ. COV(I,INDCOV(I),1)) THEN
           RS(ND,2*NDRUG+I) = COV(I,INDCOV(I),2)
           INDCOV(I) = INDCOV(I) + 1
          ENDIF
         END DO
        ENDIF


	GO TO 100

        return

        END subroutine shift



! ##################################################################### !
!
! wmy20190315 Translated to F90 and Moved into npag_utils.f90.
!
      SUBROUTINE VERIFYVAL(N,X)
      IMPLICIT NONE
      integer, intent(in) :: N
      double precision, dimension(1:), intent(inout) :: X

!  THIS ROUTINE INPUTS X(I),I=1,N.
!  ON OUTPUT, EACH X(.) WHICH IS INSIDE [-1.D-99, 1.D-99] IS REPLACED
!  BY 0. THIS PREVENTS THIS VALUE FROM BEING WRITTEN OUT IMPROPERLY,
!  E.G., AS .934-106, RATHER THAN .934E-106.
!  ANY X(.) VALUE NOT INSIDE THE ABOVE RANGE WILL BE UNCHANGED ON
!  OUTPUT.

      integer I

      DO I = 1,N
       IF(X(I) .GE. -1.D-99 .AND. X(I) .LE. 1.D-99) X(I) = 0.D0
      END DO

      RETURN
      END subroutine verifyval

! ##################################################################### !
      SUBROUTINE ORDERDELTA(NDRUG,DELTAIV,NDELTA,ORDELT)
!
! wmy20190318 Validated on NPrun(30,...), SIMrun(1,...), and NPrun(1,...)
!

!      IMPLICIT REAL*8(A-H,O-Z)
!
! wmy20190316
      implicit none

!      DIMENSION DELTAIV(7),ORDELT(7),X(7)
!
! wmy20190317 In arrays above, 7 = max_input_dim.  In future, this will
!   be larger; but no error checking on length is done.

      integer, intent(in) ::  NDRUG
      double precision, intent(in), dimension(1:) :: DELTAIV
      integer, intent(inout) :: NDELTA
      double precision, intent(inout), dimension(1:) :: ORDELT
!
! wmy20190317 -- I find it difficult to believe there is not a (better) canned
!  fortran routine that orders arrays; but for now, use this subroutine.
!
!  SUBROUTINE ORDERDELTA IS CALLED BY NEWWORK1 TO OBTAIN NDELTA, THE NO.
!  OF UNIQUE NON-0 VALUES IN THE DELTAIV(.) ARRAY. THEN THE ORDERED SET
!  OF THESE NDELTA VALUES IS PUT INTO ORDELT(.). NOTE THAT
!  NDELTA WILL BE 0 IF ALL THE PARTICIPATING DRUGS ARE BOLUSES SINCE
!  THEY WOULDN'T NEED AN ENDING TIME THEN.

      integer IDRUG, IDRUGNEW, ICOMP, ISAME
      double precision X(NDRUG), VALUE

!  FIRST STORE ALL THE VALUES IN DELTAIV INTO X SO THAT DELTAIV WILL
!  NOT BE CHANGED.

      DO IDRUG = 1,NDRUG
       X(IDRUG) = DELTAIV(IDRUG)
      END DO

!  THE LOGIC OF THIS ROUTINE IS BASED ON \PERSONAL\FINANCE\ORDER.FOR.
!  TO DO THIS, EACH VALUE IN X(.) WILL BE COMPARED TO THE
!  PREVIOUS ONE. IF IT IS < THE PREVIOUS ONE, THE VALUE WILL EXCHANGE
!  PLACES WITH THE PREVIOUS ONE, AND THE TESTING WILL CONTINUE. THE
!  TESTING WILL STOP FOR A VALUE WHEN IT IS COMPARED TO A PREVIOUS
!  VALUE WHICH IS .LE. ITS VALUE.

      DO IDRUG = 2, NDRUG

!  COMPARE VALUE FOR IDRUG WITH EACH PREVIOUS VALUE, AND HAVE IT
!  EXCHANGE PLACES WITH THAT VALUE, UNTIL IT REACHES ONE WHICH HAS A
!  SMALLER VALUE. FIRST SET IDRUGNEW = IDRUG; AFTER THE FOLLOWING
!  CODE, IDRUGNEW WILL BE THE INDEX NO. FOR VALUE AT THE OLD IDRUG
!  POSITION.

       IDRUGNEW = IDRUG

       ICOMP = IDRUG

  110  ICOMP = ICOMP - 1

!  NOW COMPARE VALUE IN LOCATION ICOMP WITH THE VALUE IN LOCATION
!  IDRUGNEW. IF THE LATTER IS .LT. THE FORMER, INTERCHANGE THE RECORDS.

       IF(X(IDRUGNEW) .LT. X(ICOMP)) THEN

        VALUE = X(IDRUGNEW)
        X(IDRUGNEW) = X(ICOMP)
        X(ICOMP) = VALUE
        IDRUGNEW = ICOMP


!  IF IDRUGNEW = 1, IT HAS BEEN CHECKED AGAINST ALL RECORDS (AND IS
!  THE SMALLEST VALUE); IF IS IS > 1, CONTINUE THE PROCESS.

        IF(IDRUGNEW .EQ. 1) GO TO 150
        IF(IDRUGNEW .GT. 1) GO TO 110

       ENDIF

!  THE ABOVE ENDIF IS FOR THE
!   IF(X(IDRUGNEW) .LT. X(ICOMP))  CONDITION.


  150 END DO

!  THE ABOVE END DO IS FOR THE  DO IDRUG = 2, NDRUG LOOP.


!  NOW THE NDRUG VALUES ARE ORDERED, FROM SMALL TO LARGE IN X.
!  REWRITE THEM INTO ORDELT, BUT PUT ONLY THE NON-0 AND
!  UNIQUE VALUES INTO ORDELT, AND KEEP TRACK OF NOW MANY OF THESE
!  UNIQUE NON O VALUES THERE ARE - IT WILL BE NDELTA AT THE END OF
!  THE FOLLOWING LOOP.

      NDELTA = 0

      DO IDRUG = 1,NDRUG

!  FOR THIS VALUE TO BE COUNTED, IT CANNOT = THE PREVIOUS VALUE, AND
!  IT CANNOT = 0.

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

!  THE ABOVE END DO IS FOR THE  DO IDRUG = 1,NDRUG  LOOP.

      RETURN
      END subroutine orderdelta

! ##################################################################### !

      SUBROUTINE THESAME(X1,X2,ISAME)
!
! wmy20190318 Validated on Pmetrics examples NPrun(30,...), NPrun(1,...),
!  and SIMrun(1,...).
!
!        IMPLICIT REAL*8(A-H,O-Z)

      implicit none

      double precision, intent(in) ::  X1, X2
      integer, intent(inout) :: ISAME

! wmy20190317  This subroutine should be a function.  The point is to 
!  ensure consistency, So the threshold should be a common parameter.
!
!  THIS ROUTINE CHECKS TO SEE IF X1 AND X2 ARE VIRTUALLY THE SAME
!  VALUES (I.E., IF THEY ARE WITHIN 1.D-10 OF EACH OTHER). IF SO,
!  ISAME RETURNS AS 1; IF NOT ISAME RETURNS AS 0.

      double precision XDEL

        ISAME = 0
        XDEL = DABS(X1-X2)
        IF(XDEL .LE. 1.D-10) ISAME = 1

        RETURN
        END subroutine thesame

! ##################################################################### !
      SUBROUTINE PREDLAST3(NN,NSET,XSTORE,XPRED,ICONV)

!      use npag_utils, only: thesame

!      IMPLICIT REAL*8(A-H,O-Z)
       implicit none

!      DIMENSION XSTORE(100,20),XPRED(20),COMP(5,20)
      integer, intent(in) ::  NN, NSET
      double precision, dimension(1:,1:), intent(in) :: XSTORE ! dimension(max_SS_doses,max_ODE_comps)
      double precision, dimension(1:), intent(inout) :: XPRED  ! dimension(max_ODE_comps)
      integer, intent(inout) :: ICONV
!
! TODO
! 1) NN is the number of compartments used in current model; verify that
!    NN .le. max_ODE_comps, or (even more cautious) verify size of
!    appropriate diemnsion of input arrays is .le. max_ODE_comps
! 2) Likewise, verify that NSET .le. max_SS_doses
! 3) Exit w/error if the above inequalities not true; or procede w/no more
!    error checking needed.
! wmy20190814 4) Finally, for either approach above, set dimensions above to assumed shape.
!

! Locals
      double precision, dimension(5,max_ODE_comps) :: COMP
      double precision TOL1, TOL2, A1, A2, A3, DEL1, DEL2
      double precision F, PRED1, PRED2, PRED3, DEN, PREDNEG
      integer I, II, J, IN
      integer ISAME1, ISAMEF1, ISAME2, ISAMEF2, ISAME3, ISAMEF3
      integer ISAMETOT, ISAMEFTOT, ISAMEDEN, ISAME

!  NOTE THAT AS OF MONT109.FOR, THE DIMENSIONS OF 6 IN XSTORE, XPRED,
!  AND COMP HAVE BEEN CHANGED TO 20, WHICH IS WHAT THEY SHOULD HAVE BEEN
!  ALL ALONG (SEE SUBROUTINE FUNC2). Also, the dimension 5 in COMP is
!  a hardcoded computational parameter: PREDLAST3 is called after every
!  SS dose _after_ the 5th dose.


!  THIS SUBROUTINE IS CALLED BY SUBROUTINE FUNC WITH NSET SETS OF NN
!  COMPARTMENT  VALUES IN XSTORE. USE THE LAST 5 SETS OF VALUES TO

!  PREDICT THE FINAL (STEADY STATE) COMPARTMENT AMOUNTS AFTER THE LAST
!  (100TH) DOSE SET. NOTE THAT AS OF MONT110.FOR, THERE WILL ALSO BE AN
!  ADDITIONAL 101ST DOSE SET APPLIED. BUT THE PREDICTION SHOULD STILL
!  BE AFTER THE 100TH SET.


!  IF THESE VALUES "CONVERGE", SET ICONV = 1, AND WRITE THE PREDICTED
!  VALUES INTO XPRED. IF THEY DON'T CONVERGE, SET ICONV = 0.

!  TOL1 AND TOL2 ARE, FOR NOW, HARDCODED TO BE .0005.

        TOL1 = .0005D0
        TOL2 = .0005D0



!  THE LAST 5 SETS OF VALUES ARE IN XSTORE(NSET-4:NSET,.). PUT THESE
!  VALUES INTO COMP(.,.).

      II = 0

      DO I = NSET-4,NSET
       II = II+1
       DO J = 1,NN
        COMP(II,J) = XSTORE(I,J)
       END DO
      END DO

!  FOR EACH COMPARTMENT AMOUNT, SEE IF THE FINAL STEADY STATE COMP.
!  AMOUNT CAN BE PREDICTED ACCURATELY.

      DO IN = 1,NN


       A1 = COMP(1,IN)
       A2 = COMP(2,IN)
       A3 = COMP(3,IN)
       DEL1 = A2 - A1
       DEL2 = A3 - A2

!  TEST FOR DEL1 = 0. IF SO, SEE ISAMETOT BELOW.

       CALL THESAME(DEL1,0.D0,ISAME1)

       IF(ISAME1 .EQ. 0) THEN

        F = DEL2/DEL1

!  THE UNDERLYING ASSUMPTION IS THAT THE RATIO F = DEL2/DEL1
!  IS CONTANT BETWEEN CONSECUTIVE OUTPUT DIFFERENCES (At SS) . IF SO, THEN
!  THE STEADY STATE VALUE WILL BE A1 + DEL1/(1 - F) (SEE SS.EXP
!  IN \ALAN3\STEADYSTATE). CALCULATE THIS VALUE AND CALL IT PRED1.

!  BUT, IF DEL2 = DEL1, THEN F = 1. IN THIS CASE, CAN'T DO THE FOLLOWING
!  CALCULATION FOR PRED1, AND WE WOULDN'T WANT TO DO IT SINCE
!  DEL2 = DEL1 --> A2 - A1 = A3 - A2 --> A1, A2, AND A3 ARE IN AN
!  ARITHMETIC PROGRESSION --> THERE OBVIOUSLY CAN BE NO CONVERGENCE

!  SINCE, AFTER 100 DOSES, THE VALUE WOULD JUST A1 + 99*DEL1 ...
!  UNLESS DEL1 = 0, IN WHICH CASE THE VALUE WOULD CONVERGE TO A1.
!  IN THIS CASE SET ISAMEF1 = 1, AND SKIP CALC. OF PRED1. AND THEN
!  SEE THE LOGIC RELATED TO ISAMEF1 BELOW.

        CALL THESAME(F,1.D0,ISAMEF1)
        IF(ISAMEF1 .EQ. 0) PRED1 = A1 + DEL1/(1.D0 - F)

       ENDIF


!  SIMILARLY, CALCULATE PRED2 (BASED ON (A2,A3,A4)) AND PRED3 (BASED
!  ON (A3,A4,A5).

       A1 = COMP(2,IN)
       A2 = COMP(3,IN)
       A3 = COMP(4,IN)
       DEL1 = A2 - A1
       DEL2 = A3 - A2


!  TEST FOR DEL1 = 0. IF SO, SEE ISAMETOT BELOW.

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


!  TEST FOR DEL1 = 0. IF SO, SEE ISAMETOT BELOW.

       CALL THESAME(DEL1,0.D0,ISAME3)

       IF(ISAME3 .EQ. 0) THEN

        F = DEL2/DEL1


        CALL THESAME(F,1.D0,ISAMEF3)
        IF(ISAMEF3 .EQ. 0) PRED3 = A1 + DEL1/(1.D0 - F)
       ENDIF

!  ASSUMING A NEGATIVE EXPONENTIAL PATTERN FIT (SEE SS.EXP IN
!  \ALAN3\STEADYSTATE OR HOME NOTES, PG.2 ON 9/11/11 FOR DETAILS) ON
! (PRED1,PRED2,PRED3), CALCULATE PREDNEG.

!  BUT ONLY DO THIS CALCULATION, AND THE SUBSEQUENT
!  CONVERGENCE DETERMINATION IF ISAME1 = ISAME2 = ISAME3 = 0, AND
!  ISAMEF1 = ISAMEF2 = ISAMEF3 = 0. OTHERWISE, AT LEAST ONE OF THE
!  PREDICTED VALUES ABOVE WAS NOT CALCULATED.

       ISAMETOT = ISAME1 + ISAME2 + ISAME3
       ISAMEFTOT = ISAMEF1 + ISAMEF2 + ISAMEF3


       IF(ISAMETOT .EQ. 0 .AND. ISAMEFTOT .EQ. 0) THEN

! EDITED CODE BELOW FOR MONT103.FOR.

!  IF PRED1 + PRED3 - 2*PRED2 = 0, PREDNEG (SEE BELOW) CANNOT BE
!  CALCULATED. IN THIS CASE, PRED2 - PRED1 = PRED3 - PRED2 -->
!  THE SEQUENCE (PRED1, PRED2, PRED3) IS LINEAR, WHICH CANNOT BE
!  MODELED WITH AN EXPONENTIAL FIT (SEE COMMENTS ABOVE). SO, IF THIS
!  HAPPENS, CONVERGENCE WILL BE SATISFIED IF THESE 3 VALUES ARE
!  VIRTUALLY THE SAME - I.E., ONLY THE REQUIREMENT INVOLVING TOL1
!  WILL BE NEEDED FOR CONVERGENCE (RECALL THE ONLY REASON FOR THE
!  EXTRA NEGATIVE EXPONENTIAL FIT, AND THE CALCULATION OF PREDNEG IS FOR
!  THOSE CASES WHERE PRED1, PRED2, AND PRED3 ARE NOT ALL VIRTUALLY THE
!  SAME VALUE).

        DEN = PRED1+PRED3-2.D0*PRED2
        CALL THESAME(DEN,0.D0,ISAMEDEN)


        IF(ISAMEDEN .EQ. 0) &
         PREDNEG = (PRED1*PRED3 - PRED2*PRED2)/DEN

!  NOW CHECK FOR CONVERGENCE, WHICH HAS BEEN OBTAINED IF
!  |PRED3/PRED2 - 1| < TOL1 AND |PREDNEG/PRED3 - 1| < TOL2.

        ICONV = 1
        IF(DABS(PRED3/PRED2 - 1.D0) .GE. TOL1) ICONV = 0
        IF(ISAMEDEN .EQ. 0 .AND. DABS(PREDNEG/PRED3 - 1.D0) .GE. TOL2) &
         ICONV = 0

!  IF ICONV = 1 FOR THIS COMPARTMENT, IN, STORE THE PREDICTED AMOUNT,
!  AND CONTINUE TO THE NEXT COMPARTMENT. NOTE BELOW THAT
!  NON-CONVERGENCE IN ANY COMPARTMENT ENDS THE PROCESS SINCE TO
!  CONVERGE, ALL COMPARTMENT PREDICTIONS MUST CONVERGE.

        IF(ICONV .EQ. 1 .AND. ISAMEDEN .EQ. 1) XPRED(IN) = PRED3
        IF(ICONV .EQ. 1 .AND. ISAMEDEN .EQ. 0) XPRED(IN) = PREDNEG


! EDITED CODE ABOVE FOR MONT103.FOR.


       ENDIF

!  THE ABOVE ENDIF IS FOR THE  IF(ISAMETOT .EQ. 0 .AND. ISAMEFTOT .EQ.0)
!  CONDITION.

!  IF ISAMETOT .GT. 0, THERE ARE TWO POSSIBILITIES (AND NOTE THAT IT
!  DOSEN'T MATTER WHAT ISAMEFTOT IS IN THIS CASE):

!   ISAMETOT = 3, IN WHICH CASE COMP(1:4,IN) ARE ALL THE SAME.
!   ISAMETOT = 1 OR 2, IN WHICH CASE SOME OF THE COMP(1:4,IN) VALUES
!     ARE THE SAME, AND SOME ARE NOT.


!  IN THE FORMER CASE, VERIFY THAT COMP(5,IN) IS THE SAME VALUE AS
!  THE COMP(1:4,IN). IF SO, SET THE PREDICTED VALUE = THIS VALUE
!  (I.E., THE PREDICTED VALUE FOR A CONSTANT FUNCTION IS THE
!  CONSTANT VALUE), AND SET ICONV = 1. OTHERWISE, SET ICONV = 0
!  SINCE THERE IS NO WAY TO FIT 4 VALUES WHICH ARE THE SAME AND ONE
!  WHICH IS NOT USING A NEGATIVE EXPONENTIAL FUNCTION.

!  IN THE LATTER CASE, SINCE SOME OF THE COMP(1:4,IN) VALUES ARE THE
!  SAME, AND SOME ARE NOT, SET ICONV = 0 FOR THE SAME REASON AS
!  STATED IN THE PREVIOUS PARAGRAPH.

       IF(ISAMETOT .EQ. 3) THEN

        CALL THESAME(COMP(5,IN),COMP(1,IN),ISAME)

        IF(ISAME .EQ. 1) THEN
         ICONV = 1
         XPRED(IN) = COMP(1,IN)
        ENDIF

        IF(ISAME .EQ. 0) ICONV = 0

       ENDIF

       IF(ISAMETOT .EQ. 1 .OR. ISAMETOT .EQ. 2) ICONV = 0

!  IF ICONV = 0, CONVERGENCE WAS NOT ACHIEVED.

       IF(ICONV .EQ. 0) RETURN

      END DO

!  THE ABOVE END DO IS FOR THE  DO IN = 1,NN  LOOP.

!  TO GET TO THIS POINT, ALL COMPARTMENT AMOUNTS HAVE CONVERGED, AND
!  THEIR PREDICTED AMOUNTS HAVE BEEN STORED INTO XPRED(IN),IN=1,NN.

      RETURN
      END subroutine PREDLAST3

! ##################################################################### !
end module
