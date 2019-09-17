      PROGRAM EXAMPLE
!-----------------------------------------------------------------------
! EXAMPLE PROBLEM
!
! The following is a simple example problem, with the coding
! needed for its solution by DVODE.  The problem is from chemical
! kinetics, and consists of the following three rate equations:
!     dy1/dt = -.04*y1 + 1.e4*y2*y3
!     dy2/dt = .04*y1 - 1.e4*y2*y3 - 3.e7*y2**2
!     dy3/dt = 3.e7*y2**2
! on the interval from t = 0.0 to t = 4.e10, with initial conditions
! y1 = 1.0, y2 = y3 = 0.  The problem is stiff.
!
! The following coding solves this problem with DVODE, using MF = 21
! and printing results at t = .4, 4., ..., 4.e10.  It uses
! ITOL = 2 and ATOL much smaller for y2 than y1 or y3 because
! y2 has much smaller values.
! At the end of the run, statistical quantities of interest are
! printed. (See optional output in the full description below.)
!
      EXTERNAL FEX, JEX
      DOUBLEPRECISION ATOL, RPAR, RTOL, RWORK, T, TOUT, Y
      DIMENSION Y (3), ATOL (3), RWORK (67), IWORK (33)
      NEQ = 3
      Y (1) = 1.0D0
      Y (2) = 0.0D0
      Y (3) = 0.0D0
      T = 0.0D0
      TOUT = 0.4D0
      ITOL = 2
      RTOL = 1.D-4
      ATOL (1) = 1.D-8
      ATOL (2) = 1.D-14
      ATOL (3) = 1.D-6
      ITASK = 1
      ISTATE = 1
      IOPT = 0
      LRW = 67
      LIW = 33
      MF = 21
      DO 40 IOUT = 1, 12
         CALL DVODE (FEX, NEQ, Y, T, TOUT, ITOL, RTOL, ATOL, ITASK,     &
         ISTATE, IOPT, RWORK, LRW, IWORK, LIW, JEX, MF, RPAR, IPAR)
         WRITE (6, 20) T, Y (1), Y (2), Y (3)
   20 FORMAT  (' At t =',D12.4,'   y =',3D14.6)
         IF (ISTATE.LT.0) GOTO 80
   40 TOUT = TOUT * 10.
      WRITE (6, 60) IWORK (11), IWORK (12), IWORK (13), IWORK (19),     &
      IWORK (20), IWORK (21), IWORK (22)
   60 FORMAT(/' No. steps =',I4,'   No. f-s =',I4,                      &
     &       '   No. J-s =',I4,'   No. LU-s =',I4/                      &
     &       '  No. nonlinear iterations =',I4/                         &
     &       '  No. nonlinear convergence failures =',I4/               &
     &       '  No. error test failures =',I4/)
      STOP
   80 WRITE (6, 90) ISTATE
   90 FORMAT(///' Error halt: ISTATE =',I3)
      STOP
      END PROGRAM EXAMPLE

      SUBROUTINE FEX (NEQ, T, Y, YDOT, RPAR, IPAR)
      DOUBLEPRECISION RPAR, T, Y, YDOT
      DIMENSION Y (NEQ), YDOT (NEQ)
      YDOT (1) = - .04D0 * Y (1) + 1.D4 * Y (2) * Y (3)
      YDOT (3) = 3.D7 * Y (2) * Y (2)
      YDOT (2) = - YDOT (1) - YDOT (3)
      RETURN
      END SUBROUTINE FEX

      SUBROUTINE JEX (NEQ, T, Y, ML, MU, PD, NRPD, RPAR, IPAR)
      DOUBLEPRECISION PD, RPAR, T, Y
      DIMENSION Y (NEQ), PD (NRPD, NEQ)
      PD (1, 1) = - .04D0
      PD (1, 2) = 1.D4 * Y (3)
      PD (1, 3) = 1.D4 * Y (2)
      PD (2, 1) = .04D0
      PD (2, 3) = - PD (1, 3)
      PD (3, 2) = 6.D7 * Y (2)
      PD (2, 2) = - PD (1, 2) - PD (3, 2)
      RETURN
      END SUBROUTINE JEX
