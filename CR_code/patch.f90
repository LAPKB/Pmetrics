  module patch
  implicit none
  integer, private, parameter :: i4=selected_int_kind(9)
  integer, private, parameter :: i8=selected_int_kind(15)
  integer, private, parameter :: r8=selected_real_kind(15,9)
  contains

  subroutine cholesky_repair(uplo,n,A,iounit,errfil,info_err)
  integer, parameter :: lwmax = 5000000
  integer(kind=i4) :: i, j, n, lda, info, lwork, liwork, info_err
  integer(kind=i4) :: iwork(lwmax)
  real(kind=r8) :: A(n,n) ! A needs to be real and symmetric, assuming A is real and symmetric.
  real(kind=r8) :: lambda(n,n)
  real(kind=r8) :: A_pr(n,n), qr_target(n,n)
  real(kind=r8) :: w(n), work(lwmax), tau(n)  
  character(*) :: uplo, errfil
  integer :: iounit
  real(kind=r8) :: eigenval(n), eigenmat(n,n)
  real(kind=r8) :: s
  ! https://math.stackexchange.com/questions/423138/cholesky-for-non-positive-definite-matrices
  ! use eigenvalue decomposition when chol (DPOTRF) fail.
      
  lda = n 
  info_err = -99
      
  OPEN(unit=iounit ,FILE=ERRFIL,action='write',position='append')
  WRITE(iounit,*) '=========='
  call get_datetime(iounit)
      
    if ((uplo(1:1)/='L').and.(uplo(1:1)/='l').and.(uplo(1:1)/='U').and.(uplo(1:1)/='u')) then
    info_err = -99
    write(iounit,*) 'UPLO in cholesky_repair in error ', uplo(1:1)
    return
    endif
     
  !write(iounit,*) 'hess_save  = '
  !do j=1,n
  !  do i=1,n
  !    write(iounit,*) A(i,j)
  !  enddo
  !enddo 

  CALL PRINT_MATRIX(iounit, 'Problematic Matrix = ', N, N, A, LDA )      
      
  ! Step 1. Find eigenvalue and eigen matrix
      
  !     Query the optimal workspace.
  LWORK = -1
  LIWORK = -1
  CALL DSYEVD( 'Vectors', uplo(1:1), N, A, LDA, W, WORK, LWORK, IWORK, LIWORK, INFO )
  LWORK = MIN( LWMAX, INT( WORK( 1 ) ) )
  LIWORK = MIN( LWMAX, IWORK( 1 ) )        
  write(6,*) 'LWORK,LIWORK = ', LWORK,LIWORK      

  !     Solve eigenproblem.
  eigenmat = A
  CALL DSYEVD( 'Vectors', uplo(1:1), N,  eigenmat, LDA, eigenval, WORK, LWORK,IWORK, LIWORK, INFO )
           
  !     Check for convergence.
  IF( INFO.GT.0 ) THEN
      WRITE(iounit,*)'cholesky_repair DSYEVD failed.'
      info_err = -1
      return
  END IF     
      
  !     Print eigenvalues.
  CALL PRINT_MATRIX(iounit, 'Eigenvalues', 1, N, eigenval, 1 )
  !     Print eigenvectors.
  CALL PRINT_MATRIX(iounit, 'Eigenvectors (stored columnwise)', N, N, eigenmat, LDA ) 
         
  ! Step 2. Fix using QR     
  lambda = 0.0_r8
  do i = 1, n
    !call random_number(s) 
    lambda(i,i) = max(eigenval(i),0.0)
  enddo

  A_pr=matmul(eigenmat,matmul(lambda,transpose(eigenmat)))
      
  CALL PRINT_MATRIX(iounit, 'A pr matrix', N, N, A_pr, LDA )      
            
  qr_target = transpose(matmul(eigenmat,sqrt(lambda)))
      
  CALL PRINT_MATRIX(iounit, 'QR target matrix', N, N, qr_target, LDA )       
      
           
  !     Query the optimal workspace.      
  lwork = -1  
  call dgeqrf(n, n, qr_target, lda, tau, work, lwork, info)
  lwork = min( lwmax, int(work(1)) )     
  !     Solve      
  call dgeqrf(n, n, qr_target, lda, tau, work, lwork, info)
          
  !     Check for convergence.
  IF( info /= 0 ) THEN
      WRITE(iounit,*)'cholesky_repair dgeqrf failed.'
      info_err = -2
      return
  END IF       
      
      
  A = 0.0_r8     
  do i = 1, n             
    A(1:i,i) = qr_target(1:i,i)    
  enddo
      
  if ((uplo(1:1)=='L').or.(uplo(1:1)=='l')) then
    A = transpose(A)! A becomes the approximated Cholesky lower L matrix.
    CALL PRINT_MATRIX(iounit, 'approximated Cholesky lower L', N, N, A, LDA ) 
  else
    CALL PRINT_MATRIX(iounit, 'approximated transpose of Cholesky lower L', N, N, A, LDA )         
  endif  
      
  WRITE(iounit,*) '=========='
  WRITE(iounit,*) 

  CLOSE(iounit)

  info_err = 0
  return  
  end subroutine cholesky_repair

  subroutine dw_repair(uplo,n,A,B,iounit,errfil,info_err)
  integer, parameter :: i4=selected_int_kind(9)
  integer, parameter :: i8=selected_int_kind(15)
  integer, parameter :: r8=selected_real_kind(15,60)
  integer, parameter :: lwmax = 5000000
  integer, parameter :: nrhs = 1
  integer(kind=i4) :: i, j, n, lda, ldb, info, lwork, liwork, info_err
  !integer(kind=i4) :: iwork(lwmax)
  real(kind=r8) :: A(:,:) ! A needs to be real and symmetric, assuming A is real and symmetric.
  real(kind=r8) :: B(:)
  real(kind=r8) :: A_copy(n,n), B_copy(n)
  integer(kind=i4) :: IPIV( N )
  real(kind=r8) :: lambda(n,n)
  real(kind=r8) :: A_pr(n,n), qr_target(n,n)
  real(kind=r8) :: w(n), tau(n)  
  real(kind=r8), allocatable :: work(:)
  integer(kind=i4), allocatable :: iwork(:)
  character(*) :: uplo, errfil
  integer :: iounit
  real(kind=r8) :: eigenval(n), eigenmat(n,n)
  real(kind=r8) :: s
  
  
  
  lda = n 
  ldb = lda
  info_err = -99
  
  
  A_copy(1:n,1:n) = A(1:n,1:n)
  B_copy(1:n) = B(1:n)

      
  if ((uplo(1:1)/='L').and.(uplo(1:1)/='l').and.(uplo(1:1)/='U').and.(uplo(1:1)/='u')) then
    write(*,*) 'UPLO in dw_repair in error ', uplo(1:1)
    return
  endif
    
!---------------------      
  !!     Query the optimal workspace.
  !LWORK = -1
  !LIWORK = -1
  !allocate(work(1),iwork(1))
  !CALL DSYEVD( 'Vectors', uplo(1:1), N, A, LDA, W, WORK, LWORK, IWORK, LIWORK, INFO )
  !LWORK = MIN( LWMAX, INT( WORK( 1 ) ) )
  !LIWORK = MIN( LWMAX, IWORK( 1 ) )  
  !deallocate(work,iwork)
  !!     Solve eigenproblem.
  !allocate(work(lwork),iwork(liwork)) 
  !eigenmat = A_copy
  !CALL DSYEVD( 'Vectors', uplo(1:1), N,  eigenmat, LDA, eigenval, WORK, LWORK, IWORK, LIWORK, INFO )
  !deallocate(work,iwork)
  !         
  !!     Check for convergence.
  !IF( INFO.GT.0 ) THEN
  !    WRITE(iounit,*)'dw_repair DSYEVD failed.'
  !    info_err = -1
  !    CALL PRINT_MATRIX(iounit, 'Problematic Matrix = ', N, N, A, LDA ) 
  !    return
  !else
  !!     Print eigenvalues.
  !!  CALL PRINT_MATRIX(iounit, 'Eigenvalues', 1, N, eigenval, 1 )
  !!     Print eigenvectors.
  !!  CALL PRINT_MATRIX(iounit, 'Eigenvectors (stored columnwise)', N, N, eigenmat, LDA ) 
  !END IF     
!---------------------  
  

!     Query the optimal workspace.
  LWORK = -1
  allocate(work(1)) 
  CALL DSYSV( uplo, N, NRHS, A, LDA, IPIV, B, LDB, WORK, LWORK, INFO )
  LWORK = MIN( LWMAX, INT( WORK(1) ) )  
  deallocate(work) 
  
!     Solve the equations A*X = B.
  allocate(work(lwork)) 
  CALL DSYSV( uplo, N, NRHS, A, LDA, IPIV, B, LDB, WORK, LWORK, INFO )
  deallocate(work)

!     Check for the exact singularity.

  IF( INFO.GT.0 ) THEN
    OPEN(unit=iounit ,FILE=ERRFIL,action='write',position='append')
      WRITE(iounit,*) '=========='
      call get_datetime(iounit)
      CALL PRINT_MATRIX(iounit, 'Problematic Matrix = ', N, N, A_copy, LDA ) 
      WRITE(iounit,*)'The element of the diagonal factor '
      WRITE(iounit,*)'D(',INFO,',',INFO,') is zero, so that'
      WRITE(iounit,*)'D is singular; the solution could not be computed.'
      WRITE(iounit,*) '=== dw repair failed ===='  
      WRITE(iounit,*) 
    CLOSE(iounit)
  else
    !CALL PRINT_MATRIX(iounit, 'r2 =', 1, N, B_copy, 1 )
    !CALL PRINT_MATRIX(iounit, 'dw =', 1, N, B, 1 )
    !WRITE(iounit,*) '=== dw repair success ===='
    info_err = 0
  END IF    



  
  return
  end subroutine dw_repair
  
  
  !  =============================================================================
  !     Auxiliary subroutines: 
  SUBROUTINE PRINT_MATRIX(iounit, DESC, M, N, A, LDA )
  ! printing a matrix.
  CHARACTER*(*)    DESC
  INTEGER          M, N, LDA, iounit
  DOUBLE PRECISION A( LDA, * )
  INTEGER          I, J
  WRITE(iounit,*)
  WRITE(iounit,*) DESC
  DO I = 1, M
      WRITE(iounit,9998) ( A( I, J ), J = 1, N )
  END DO
  9998 FORMAT( 11(:,1X,g15.7) )
  RETURN
  END SUBROUTINE PRINT_MATRIX  
  
  subroutine get_datetime(iounit)
  character(8)  :: date
  character(10) :: time
  character(5)  :: zone
  integer,dimension(8) :: values
  integer :: dt(8), iounit
  real    :: beats
  ! using keyword arguments
  call date_and_time(date,time,zone,values=dt)
  beats = (dt(7) + ((dt(6) - dt(4) + 60) * 60) + (dt(5) * 3600)) / 86.4
  write(iounit, '(i8, 5(a, i2.2), " UTC ", a, " Beats: @", f0.2)') & 
        dt(1), '/', dt(2), '/', dt(3), ' ', dt(5), ':', dt(6), ':', dt(7), zone, beats
  return
  end subroutine get_datetime  
  
  function wtime ()
  integer(kind=i8) clock_max
  integer(kind=i8) clock_rate
  integer(kind=i8) clock_reading
  real ( kind = r8 ) wtime

  call system_clock ( clock_reading, clock_rate, clock_max )

  wtime = real ( clock_reading, kind = r8 ) &
        / real ( clock_rate, kind = r8 )

  return
  end function wtime   
  
  end module patch  