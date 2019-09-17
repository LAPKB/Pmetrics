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

public makevec, check_input_array_size, cp_lrcs_to_rpar, &
  i_cycle, i_do, i_jsub, i_ig, max_ODE_comps, max_ODE_params, &
  max_pop_rand_varbs, max_pop_varbs, max_pop_params, &
  max_covs, &
  maxnumeq, max_meas_per_eqn, max_m_per_obs, max_obs, &
  max_doses, max_input_dim, max_RS_J, k_ig, k_jsub, &
  i_dvode_reserved, k_dvode_reserved, k_sum_z_sq, k_prod_pr, &
  k_p_start, k_p_end, k_r_start, k_r_end, k_gamma, k_flat, k_sfac, k_ofac, &
  k_c0_base, k_c1_base, k_c2_base, k_c3_base, i_errmod, &
  i_is_log10, i_Nnormalobs, i_is_poisson_obs, i_Npoissonobs, i_misval, i_skip_ig

! ODE parameters

integer, parameter :: max_ODE_comps =      20
integer, parameter :: k_dvode_reserved =   23   ! RPAR(1:23) are reserved by DVODE
integer, parameter :: i_dvode_reserved =   23   ! IPAR(1:23) are reserved by DVODE
integer, parameter :: max_ODE_params =     47   ! 32   ! NVAR+NRANFIX+NOFIX <= max_ODE_params , of types:
integer, parameter :: max_pop_rand_varbs = 30   ! 30   ! NVAR <= max_pop_rand_varbs; variables w/#support <= max_obs
integer, parameter :: max_pop_varbs =      20   ! NRANFIX <= max_pop_varbs; variables with #support = 1
integer, parameter :: max_pop_params =     20   ! NOFIX <= max_pop_params; #user-defined scalar pop parameters

! INPUT / OUTPUT parameters

integer, parameter :: max_input_dim =       7   ! #Input types, NDRUG <= max_input_dim
integer, parameter :: max_doses =        5000   ! ND <= max_doses per record
integer, parameter :: max_obs  =          800   ! = max #records; MAXSUB
integer, parameter :: max_obs_dim  =      150   ! = MAXOBDIM
integer, parameter :: max_covs =           20   ! max #covariate measures per record; dim
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
integer, parameter :: k_c_start = k_flat + 1       ! maxnumeq * (C0,C1,C2,C3)
integer, parameter :: k_c0_base = k_flat
integer, parameter :: k_c1_base = k_c0_base + maxnumeq
integer, parameter :: k_c2_base = k_c1_base + maxnumeq
integer, parameter :: k_c3_base = k_c2_base + maxnumeq
integer, parameter :: k_c_end = k_flat + maxnumeq * 4
integer, parameter :: k_sfac = k_c_end + 1         ! SIGFAC
integer, parameter :: k_ofac = k_sfac + 1          ! OFAC
integer, parameter :: k_sum_z_sq = k_ofac + 1      ! = \sum ((obs - pred)/sigma)^2 -- for obs ~ Normal
integer, parameter :: k_prod_pr = k_sum_z_sq + 1   ! = \Prod pr(obs) -- for obs ~ Poisson
! note: in idm1*, F(:) = (y - mu)/sigma -- for all observations

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
 
! Pmetrics erases all lines beginning with 'c' or "C", so:
! The following line must be " contains", w/leading white-space
 contains
!
! subroutine cp_lrcs_to_rpar
! function check_input_array_size
! subroutine makevec
!
! #################################################################### ! 
      subroutine cp_lrcs_to_rpar(gamma,flat,numeqt,c0,c1,c2,c3,rpar)

      implicit none

! Copies regression error arguments to rpar, placing them immediately
! after IG (see k_xxx above).  The constants are stored in k_c_gamma,
! k_c_flat, and k_c_start = k_flat + 1 to k_c_end = k_flat + maxnumeq * 4,
! as cat(gamma,flat,C0,C1,C2,C3).

      double precision gamma, flat
      integer numeqt
      double precision c0(:), c1(:), c2(:), c3(:), rpar(:)

      integer III,Ic0base,Ic1base,Ic2base,Ic3base

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

        Ic0base = k_flat
        Ic1base = Ic0base + maxnumeq
        Ic2base = Ic1base + maxnumeq
        Ic3base = Ic2base + maxnumeq
!        write (*,*) "base:", Ic0base, Ic1base, Ic2base, Ic3base
        do III=1,maxnumeq
           if (III.le.numeqt) then
             RPAR(k_c0_base + III) = C0(III)
             RPAR(k_c1_base + III) = C1(III)
             RPAR(k_c2_base + III) = C2(III)
             RPAR(k_c3_base + III) = C3(III)
           else
             RPAR(k_c0_base + III) = 0.d0
             RPAR(k_c1_base + III) = 0.d0
             RPAR(k_c2_base + III) = 0.d0
             RPAR(k_c3_base + III) = 0.d0
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
      double precision, intent(in) :: work(:),workk(:),spxgyj(:),dxi(:)
      double precision, intent(in) :: pyjgx(:,:)
      double precision, intent(in) :: pyjgxx(:)
      double precision, intent(in) :: denstor(:,:)
      double precision, intent(in) :: exx(:,:,:)
      double precision, intent(in) :: corden(:,:),corhold(:,:)
      double precision, intent(in) :: ypredpop(:,:,:,:)
      double precision, intent(in) :: ypredpopt(:,:,:,:)
      double precision, intent(in) :: ypredbay(:,:,:,:)
      double precision, intent(in) :: cordlast(:,:)

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

        integer nvar,nofix,nranfix,iran(:)    
        double precision, intent(in) ::  x(:),valfix(:),ranfixest(:)
        double precision, intent(out) :: px(:)
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
end module
