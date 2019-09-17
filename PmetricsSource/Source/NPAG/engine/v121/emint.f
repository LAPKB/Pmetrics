C wmy2017Jul26 -- need to separate out the different packages for
C   upgrade to f90+. Note: gcc supports f95+
C
C


      subroutine emint(psi,ldpsi,theta,ldtheta,npoint,nsub,ijob,
     &                 x,dx,y,dy,fobj,gap,nvar,keep,IHESS)

      implicit real*8 (a-h,o-z)
      real*8 mu
      dimension psi(ldpsi,*),theta(ldtheta,*),x(*),dx(*),y(*),dy(*)
      CHARACTER ERRFIL*20

	COMMON/SUPRES/ISUPRES
C  COMMON/SUPRES IS SUPPLIED FROM SUBROUTINE BIGNPAG.

      COMMON/ERR/ERRFIL 


c This subroutine solves the 'EM' problem of maximizing the function
 
c   fobj(x) = sum_i (log[sum_j ( psi(i,j) * x(j)) ]  ),
c             j=1,..,npoint and i=1,...,nsub
c   subject to: x(j) >= 0, sum_j x(j) = 1 (i.e. x is a probability
c             vector of length npoint)
c   where psi(i,j) is a fixed non-negative data array representing the
c   likelihood of point j for subject i
 
c inputs: psi,ldpsi,npoint,nsub,nvar
c psi contains the likelihood vectors for each subject - the i-th
c row of psi is likelikhood vector for subject i.  Thus psi(i,j) is
c likelihood of the j-th point for c the i-th subject.  The input value
c ldpsi is the 'leading dimension of psi' - i.e. the first dimension of the
c array psi as dimensioned in the calling program.
c
c input work arrays: dx(*), y(*), dy(*) - should be at least large enough to
c contain npoint points, as should the probabiltiy array x(*)

c
c outputs: x(*), fobj
c x(i) is  final probability for point i
c fobj - optimal value of the objective function
 
c note - usually npoint is much larger than nsub; here we dimension

c some internal work arrays with the maximum expected number of subjects
c MAXSUBem and the maximum number of points MAXACTem
c are be set in the parameter statement


      parameter (MAXSUBem=999,MAXACTem=10000000)
      dimension w(MAXSUBem),dw(MAXSUBem),Ptx(MAXSUBem),
     &          hess(MAXSUBem,2*MAXSUBem)
      dimension psisum(MAXSUBem),XVERIFY(100)


      integer kpvt(MAXSUBem), ipivot(MAXACTem), list(MAXACTem)

C       DATA ILOOP/0/
C  ILOOP IS USED BELOW TO KNOW WHEN TO WRITE MESSAGE TO USER IN CASE

C  THE OPTIMIZATION TAKES A 'LONG' TIME.


c here w(*) is a vector if `dual variables'
c dw(*) is a calculated change (as a Newton step) in w(*)
c Ptx(*) (Psi times x) is the vector Ptx(j) = sum_i Psi(j,i)*x(i)
c first , perform some dimension checks to make sure no internal dimensions
c are exceeded
      keep = nactve

      if(nsub.gt.MAXSUBem) then

      write(6,*) 'nsub =',nsub, ' is greater than MAXSUBem=',MAXSUBem
      write(6,*) 'MAXSUBem needs to be reset as large as nsub'
      write(6,*) 'in PARAMETER statement in subroutine emint'

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
      write(42,*) 'nsub =',nsub, ' is greater than MAXSUBem=',MAXSUBem
      write(42,*) 'MAXSUBem needs to be reset as large as nsub'
      write(42,*) 'in PARAMETER statement in subroutine emint'
        CLOSE(42)

      CALL PAUSE
      stop

      endif


      if(npoint.gt.MAXACTem) then

      write(6,*) 'npoint=',npoint,' is larger than MAXACTem=',MAXACTem
      write(6,*) 'MAXACTem needs to be reset as large as npoint'
      write(6,*) 'in PARAMETER statement in subroutine emint'

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
      write(42,*) 'npoint=',npoint,' is larger than MAXACTem=',MAXACTem
      write(42,*) 'MAXACTem needs to be reset as large as npoint'
      write(42,*) 'in PARAMETER statement in subroutine emint'
        CLOSE(42)

      CALL PAUSE
      stop

      endif


c     Second, check that psi is non-negative
      psimin=0.
      do j=1,nsub
      do i=1,npoint
      if(psi(j,i).le.psimin) psimin=psi(j,i)
      enddo
      enddo

      if(psimin.lt.0) then

	  write(6,*) 'Psi matrix not non-negative -stop'

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
	   write(42,*) 'Psi matrix not non-negative -stop'
        CLOSE(42)

	  CALL PAUSE
	  stop

      endif

c     Third,check that the row sums of psi are positive - no zero rows
c     also initialize x and w
      colsummin=1.e10
      do j=1,nsub
        s=0.
        do i=1,npoint
           x(i)=1.d0
           s=s+psi(j,i)
        enddo
      psisum(j) = s
        Ptx(j)=s
        if(s.le.colsummin) colsummin=s

        if(s.le.0) then

           write(6,*) 'psi has a zero row -stop'

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)

	   write(42,*) 'psi has a zero row -stop'
        CLOSE(42)

	   CALL PAUSE
           stop

        endif

        w(j)=1./s
      enddo
c     calc ptw = w'*psi
      shrink=0.
      do i=1,npoint
        sum=0.d0
        do j=1,nsub
           sum=sum+psi(j,i)*w(j)
        enddo
        y(i)=sum
        if(sum.gt.shrink) shrink=sum
      enddo
      shrink=2.d0*shrink

      if(s.le.0) then

	  write(6,*) 'Psi has a zero column -stop'

C  SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE THE ERROR MESSAGE
C  TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
	   write(42,*) 'Psi has a zero column -stop'
        CLOSE(42)

	  CALL PAUSE
	  stop

      endif

c     stopping tolerance
      eps=1.d-10
      sig=0.d0
      mu=0.d0
      do i=1,npoint
c       x = x*shrink;
        x(i)=1.d0*shrink
c       Ptw  = Ptw/shrink;
        y(i)=y(i)/shrink
c       y  = ecol-Ptw;
        y(i)=1.d0-y(i)
c       mu  =  (x'*y)/npoint;
        mu=mu+x(i)*y(i)
      enddo
      mu=mu/npoint
      rmax = -1.e38
      do j=1,nsub
c       w                       =  w/shrink;
        w(j)=w(j)/shrink
c       Plam            =  Plam*shrink;


        Ptx(j)=Ptx(j)*shrink
c
c       R =  erow-w.*Plam;
        if(dabs(1.d0-w(j)*Ptx(j)).ge.rmax) rmax =
     &  dabs(1.d0-w(j)*Ptx(j))

      enddo
      gap=1.d0
c start of iterations
      iter=0
100   continue
c     following is iteration termination condition


	conval = mu
      if(conval .lt. rmax) conval = rmax

      if(conval .lt. gap) conval = gap
      convcrit = eps/conval 

      XVERIFY(1) = convcrit
      CALL VERIFYVAL(1,XVERIFY)
    
C      IF(ILOOP .GT. 0) WRITE(*,123) iter,convcrit
       IF(ILOOP .GT. 0) WRITE(*,123) iter,XVERIFY(1)

  123 FORMAT(' Iteration ',I9,' CONV. CRIT = ',G15.2,' (1 OR HIGHER FOR 
     1CONVERGENCE)')
	
C  ABOVE WRITE STATEMENT ADDED IN bigmlt12.f SO THE USER WILL KNOW
C  THE PROGRAM HAS NOT 'HUNG' IF THE OPTIMIZATION TAKES A 'LONG' TIME.


      if(mu.le.eps.and.rmax.le.eps.and.gap.le.eps) go to 9000
      iter=iter+1

        ILOOP = ILOOP + 1

      tbuilda=0
      smu=sig*mu
c     zero out hessian
      do j=1,nsub
        do k=1,nsub
           hess(j,k)=0.
        enddo
      enddo
c do outer product portion of Hessian
      do i=1,npoint
        scale=x(i)/y(i)
       do j=1,nsub
         fact=scale*psi(j,i)
         do k=j,nsub
           hess(k,j)=hess(k,j)+fact*psi(k,i)
         enddo
       enddo
      enddo
      do j=1,nsub-1
      do k=j+1,nsub

      hess(j,k)=hess(k,j)
      enddo
      enddo
c do diagonal portion of hessian
      do j=1,nsub
        hess(j,j)=hess(j,j)+Ptx(j)/w(j)
      enddo
      tbuildb=0
      tbuild=tbuildb-tbuilda

      IF(ISUPRES .EQ. 0) write(6,*) 'tbuild=',tbuild

c now do cholesky decomposition-for time bing, use simple dpofa
c from LINPACK
c     call dpofa(hess,MAXSUBem,nsub,info)

c     call dsifa(hess,MAXSUBem,nsub,kpvt,info)
c note dpofa is cholesky factorization routine from LINPACK
c      dsifa is symmetric indefintie factorization routine from LINAPCK
c      DPOTRF is Cholesky factorization routine from LAPACK
c DPOTRF is fastest of the three, but DSIFA may be more reliable for
c nearly singular cases
c Regardless of which of the three routines is used, it must be matched
c with the proper solve routine (dposl for dpofa, dsisl for dsifa, 
c DPOTRS for DPOTRF below


      CALL DPOTRF( 'L', nsub, hess, MAXSUBem, INFO )
      tbuildc=0
      tfactor=tbuildc-tbuildb

      IF(ISUPRES .EQ. 0) write(6,*) 'tfactor=',tfactor

cdebugwrite(6,*) 'gap,info=',gap,info


c  As of npageng18.f, set IHESS = 0. If info .ne. 0, reset it = -1 and,
c  after writing the indicated message to the screen (and also now to 
c  the output file), return to MAIN, where IHESS = -1 tells the program
c  to create the output files before stopping (previously, if 
c  info .ne. 0, the program would simply stop after writing the 
c  indicated message to the screen).

      IHESS = 0


      if(info .ne. 0) then

       IHESS = -1


       WRITE(25,163)
       WRITE(*,163)
  163  FORMAT(//' Hessian matrix in interior point EM algorithm'/
     1' is singular.  Possibly number of grid points is too small,'/
     2' or assay coefficients are too large. '//
     3' Try again with a new assay polynomial or larger grid.'//
     4' Suggested quick fix: rerun and select error model 2)'/
     5' in response to the initial question; then enter a'/
     6' initial value gamma = 10.0 in response to the prompt for'/
     7' that value.'//
     8' THE PROGRAM WILL CREATE OUTPUT FILES BEFORE STOPPING. '//)

      WRITE(25,164) info
      WRITE(*,164) info
  164 FORMAT(//' NOTE THAT IN SUBROUTINE emint, THE VALUE OF INFO'/
     1' IS ',i6,//
     2' IF THIS VALUE IS POSTIVE, IT IS LIKELY THE NO. OF THE SUBJECT'/
     3' (OR AT LEAST THE FIRST SUBJECT) WHICH CAUSED THE HESSIAN '/
     4' ERROR. SO IN THIS CASE, YOU MIGHT ALSO WANT TO EXAMINE THE'/
     5' DATA IN THIS SUBJECT TO VERIFY THEY ARE CORRECT.'//)
      
c  As of npageng22.f, the following PAUSE is commented out ... since
c  it --> the program will not complete properly if it is run under
c  Pmetrics (which cannot supply a keyboard response during a run).
c     CALL PAUSE

c  As of npageng18.f, the program does not stop here; it returns to 
c  MAIN to write out the output files and then stops.

C  AS of npageng23.f, SINCE THE PROGRAM IS TERMINATING ABNORMALLY, WRITE
C  THE ERROR MESSAGE TO ERRFIL ALSO.

        OPEN(42,FILE=ERRFIL)
         WRITE(42,163) 
         WRITE(42,164) info
        CLOSE(42)


      return

      endif



c construct rhs for linear equation system
      do j=1,nsub
        sum=0.d0
        do i=1,npoint

          sum=sum+psi(j,i)*smu/y(i)
        enddo
        dw(j)=1.d0/w(j)-sum
      enddo
c now solve linear system with LINPACK routine dposl
c and put answer in dw
c note - these routines match the factor routines dpofa, dsifa, and DPOTRF, respectively
c see note about 15 lines back where the factor routine is called
c     call dposl(hess,MAXSUBem,nsub,dw)

c     call dsisl(hess,MAXSUBem,nsub,kpvt,dw)
      call DPOTRS( 'L', nsub, 1, hess, MAXSUBem, dw, nsub, INFO )
c now compute dy and dx from dw

      do i=1,npoint
        sum=0.
        do j=1,nsub
          sum=sum+psi(j,i)*dw(j)
        enddo
        dy(i)=-sum
        dx(i)=smu/y(i)-x(i)-dy(i)*x(i)/y(i)
      enddo
c damp the Newton step
      alfpri=-.5
      do i=1,npoint
        if(dx(i)/x(i).le.alfpri) alfpri=dx(i)/x(i)
      enddo
      alfpri=-1.d0/alfpri
      alfpri=min(1.d0,0.99995*alfpri)
      alfdual=-0.5d0
      do i=1,npoint
        if(dy(i)/y(i).le.alfdual) alfdual=dy(i)/y(i)
      enddo
      alfdual=-1.d0/alfdual

      alfdual=min(1.d0,0.99995*alfdual)
      mu=0.d0
      do i=1,npoint
        x(i)=x(i)+alfpri*dx(i)
        y(i)=y(i)+alfdual*dy(i)
        mu=mu+x(i)*y(i)
      enddo
      mu=mu/npoint
      do j=1,nsub
        sum=0.d0
        do i=1,npoint
          sum=sum+psi(j,i)*x(i)
        enddo


        Ptx(j)=sum
      enddo
      do j=1,nsub
        w(j)=w(j)+alfdual*dw(j)

      enddo
c compute rmax (norm(r,inf)-note we don't really need to compute r
      rmax=0.
      do j=1,nsub
        rtest=1.d0-w(j)*Ptx(j)
        if(dabs(rtest).gt.rmax) rmax=dabs(rtest)
      enddo
      sumlogw=0.d0
      sumlgPtx=0.d0
      do j=1,nsub
        sumlogw=sumlogw+dlog(w(j))
        sumlgPtx=sumlgPtx+dlog(Ptx(j))
      enddo
      gap = dabs(sumlogw+sumlgPtx)/(1.d0+dabs(sumlgPtx))
      if(mu.lt.eps.and.rmax.gt.eps) then
        sig=1.d0
      else
        c2=1.d2
        term1=(1.d0-alfpri)**2
        term2=(1.d0-alfdual)**2
        term3=(rmax-mu)/(rmax+c2*mu)
        term=max(term1,term2)
        term=max(term,term3)
        sig=min(0.3d0,term)
      endif
      sumx=0.d0
      do i=1,npoint
        sumx=sumx+x(i)
      enddo
      fobj=0.
      do j=1,nsub
        fobj=fobj+dlog(Ptx(j)/sumx)
      enddo
      go to 100
c following is exit point
9000  continue
c finish by normalizing x to sum to 1.
c fobj has already been computed
      sumx=0.
      do i=1,npoint
      sumx=sumx+x(i)
      enddo
      do i=1,npoint
      x(i)=x(i)/sumx
      enddo
c finished if ijob=0
      if(ijob.eq.0) return
      isum=0
      xlim=0.
      do i=1,npoint
      if(x(i).gt.xlim) xlim=x(i)
      enddo
      xlim=xlim*1.d-3
      isum = 0
      do i=1,npoint
      if(x(i).gt.xlim) then
        isum = isum + 1
        list(isum) = i
        do j=1,nsub
        psi(j,isum) = psi(j,i)
        enddo
cpull
c now condense the original density grid
      do j=1,nvar
      theta(isum,j)=theta(i,j)
      enddo
      x(isum)=x(i)
      endif
      enddo
      job=1
      do k=1,npoint
      ipivot(k)=0
      enddo
c save a copy of psi after current end of psi
      do i=1,isum

      do j=1,nsub
      psi(j,i+isum)=psi(j,i)
      enddo
      enddo
      do i=1,isum
      do j=1,nsub
      psi(j,i) = psi(j,i)/psisum(j)
      enddo
      enddo
      call dqrdc(psi,ldpsi,nsub,isum,y,ipivot,dy,job)

      keep = 0
      limloop = nsub
      if(isum.lt.nsub) limloop = isum
      do i=1,limloop
      test=dnrm2(i,psi(1,i),1)
cdebugwrite(6,*) i,psi(i,i),test,psi(i,i)/test

      if(dabs(psi(i,i)/test).ge.1.d-8) keep=keep+1
      enddo
c sort ipivot to avoid collisions during condensing

      if(isum.gt.1) then
      do i=1,keep-1
      do j=i,keep
      if(ipivot(i)*ipivot(j).ne.0.and.ipivot(i).gt.ipivot(j)) then
         itemp=ipivot(i)
         ipivot(i)=ipivot(j)

         ipivot(j)=itemp
      endif
      enddo
      enddo
      endif
      do i=1,isum
      do j=1,nsub
      psi(j,i)=psi(j,i+isum)
      enddo
      enddo
c restore psi
 
 

 
C      CALL PAUSE
      do k=1,npoint
      dx(k)=0
      enddo
      sumkeep = 0.
      do k=1,keep
      j=ipivot(k)

      if(j.ne.0) then
         do jj=1,nsub
         psi(jj,k)=psi(jj,j)
         enddo
      do jvar=1,nvar
      theta(k,jvar) = theta(j,jvar)
      enddo
      endif
      if(j.gt.0) dx(list(j))=1.
      if(j.gt.0) sumkeep = sumkeep + x(list(j))
      if(j.gt.0) w(k)=x(list(j))
      enddo
      return
      end
      subroutine dpoco(a,lda,n,rcond,z,info)
      integer lda,n,info
      double precision a(lda,1),z(1)
      double precision rcond
c
c     dpoco factors a double precision symmetric positive definite
c     matrix and estimates the condition of the matrix.
c
c     if  rcond  is not needed, dpofa is slightly faster.
c     to solve  a*x = b , follow dpoco by dposl.
c     to compute  inverse(a)*c , follow dpoco by dposl.

c     to compute  determinant(a) , follow dpoco by dpodi.
c     to compute  inverse(a) , follow dpoco by dpodi.
c

c     on entry
c
c        a       double precision(lda, n)
c                the symmetric matrix to be factored.  only the
c                diagonal and upper triangle are used.
c

c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c     on return
c
c        a       an upper triangular matrix  r  so that  a = trans(r)*r

c                where  trans(r)  is the transpose.
c                the strict lower triangle is unaltered.
c                if  info .ne. 0 , the factorization is not complete.
c
c        rcond   double precision
c                an estimate of the reciprocal condition of  a .
c                for the system  a*x = b , relative perturbations
c                in  a  and  b  of size  epsilon  may cause
c                relative perturbations in  x  of size  epsilon/rcond .
c                if  rcond  is so small that the logical expression
c                           1.0 + rcond .eq. 1.0
c                is true, then  a  may be singular to working
c                precision.  in particular,  rcond  is zero  if
c                exact singularity is detected or the estimate
c                underflows.  if info .ne. 0 , rcond is unchanged.
c
c        z       double precision(n)
c                a work vector whose contents are usually unimportant.
c                if  a  is close to a singular matrix, then  z  is
c                an approximate null vector in the sense that
c                norm(a*z) = rcond*norm(a)*norm(z) .
c                if  info .ne. 0 , z  is unchanged.
c
c        info    integer
c                = 0  for normal return.
c                = k  signals an error condition.  the leading minor
c                     of order  k  is not positive definite.
c
c     linpack.  this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     linpack dpofa


c     blas daxpy,ddot,dscal,dasum
c     fortran dabs,dmax1,dreal,dsign
c
c     internal variables
c
      double precision ddot,ek,t,wk,wkm
      double precision anorm,s,dasum,sm,ynorm
      integer i,j,jm1,k,kb,kp1
c
c
c     find norm of a using only upper half
c

      do 30 j = 1, n
         z(j) = dasum(j,a(1,j),1)
         jm1 = j - 1
         if (jm1 .lt. 1) go to 20
         do 10 i = 1, jm1
            z(i) = z(i) + dabs(a(i,j))
   10    continue

   20    continue
   30 continue
      anorm = 0.0d0
      do 40 j = 1, n
         anorm = dmax1(anorm,z(j))
   40 continue
c
c     factor
c
      call dpofa(a,lda,n,info)
      if (info .ne. 0) go to 180
c
c        rcond = 1/(norm(a)*(estimate of norm(inverse(a)))) .
c        estimate = norm(z)/norm(y) where  a*z = y  and  a*y = e .
c        the components of  e  are chosen to cause maximum local
c        growth in the elements of w  where  trans(r)*w = e .
c        the vectors are frequently rescaled to avoid overflow.

c
c        solve trans(r)*w = e
c
         ek = 1.0d0
         do 50 j = 1, n
            z(j) = 0.0d0
   50    continue
         do 110 k = 1, n
            if (z(k) .ne. 0.0d0) ek = dsign(ek,-z(k))
            if (dabs(ek-z(k)) .le. a(k,k)) go to 60
               s = a(k,k)/dabs(ek-z(k))
               call dscal(n,s,z,1)
               ek = s*ek
   60       continue
            wk = ek - z(k)
            wkm = -ek - z(k)
            s = dabs(wk)
            sm = dabs(wkm)
            wk = wk/a(k,k)
            wkm = wkm/a(k,k)
            kp1 = k + 1
            if (kp1 .gt. n) go to 100
               do 70 j = kp1, n
                  sm = sm + dabs(z(j)+wkm*a(k,j))
                  z(j) = z(j) + wk*a(k,j)
                  s = s + dabs(z(j))
   70          continue
               if (s .ge. sm) go to 90
                  t = wkm - wk
                  wk = wkm
                  do 80 j = kp1, n
                     z(j) = z(j) + t*a(k,j)
   80             continue
   90          continue
  100       continue
            z(k) = wk
  110    continue
         s = 1.0d0/dasum(n,z,1)
         call dscal(n,s,z,1)
c
c        solve r*y = w
c
         do 130 kb = 1, n

            k = n + 1 - kb
            if (dabs(z(k)) .le. a(k,k)) go to 120

               s = a(k,k)/dabs(z(k))
               call dscal(n,s,z,1)
  120       continue
            z(k) = z(k)/a(k,k)
            t = -z(k)
            call daxpy(k-1,t,a(1,k),1,z(1),1)
  130    continue
         s = 1.0d0/dasum(n,z,1)
         call dscal(n,s,z,1)
c
         ynorm = 1.0d0
c
c        solve trans(r)*v = y
c
         do 150 k = 1, n
            z(k) = z(k) - ddot(k-1,a(1,k),1,z(1),1)
            if (dabs(z(k)) .le. a(k,k)) go to 140
               s = a(k,k)/dabs(z(k))

               call dscal(n,s,z,1)
               ynorm = s*ynorm
  140       continue
            z(k) = z(k)/a(k,k)
  150    continue
         s = 1.0d0/dasum(n,z,1)
         call dscal(n,s,z,1)
         ynorm = s*ynorm


c
c        solve r*z = v
c
         do 170 kb = 1, n
            k = n + 1 - kb
            if (dabs(z(k)) .le. a(k,k)) go to 160
               s = a(k,k)/dabs(z(k))
               call dscal(n,s,z,1)
               ynorm = s*ynorm
  160       continue
            z(k) = z(k)/a(k,k)
            t = -z(k)
            call daxpy(k-1,t,a(1,k),1,z(1),1)
  170    continue
c        make znorm = 1.0

         s = 1.0d0/dasum(n,z,1)
         call dscal(n,s,z,1)
         ynorm = s*ynorm
c
         if (anorm .ne. 0.0d0) rcond = ynorm/anorm
         if (anorm .eq. 0.0d0) rcond = 0.0d0
  180 continue
      return
      end
      subroutine dpofa(a,lda,n,info)
      integer lda,n,info
      double precision a(lda,1)
c
c     dpofa factors a double precision symmetric positive definite
c     matrix.
c

c     dpofa is usually called by dpoco, but it can be called
c     directly with a saving in time if  rcond  is not needed.
c     (time for dpoco) = (1 + 18/n)*(time for dpofa) .
c
c     on entry
c
c        a       double precision(lda, n)
c                the symmetric matrix to be factored.  only the
c                diagonal and upper triangle are used.
c
c        lda     integer
c                the leading dimension of the array  a .

c
c        n       integer
c                the order of the matrix  a .
c
c     on return
c
c        a       an upper triangular matrix  r  so that  a = trans(r)*r
c                where  trans(r)  is the transpose.
c                the strict lower triangle is unaltered.

c                if  info .ne. 0 , the factorization is not complete.
c
c        info    integer
c                = 0  for normal return.
c                = k  signals an error condition.  the leading minor
c                     of order  k  is not positive definite.
c
c     linpack.  this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas ddot
c     fortran dsqrt
c
c     internal variables
c

      double precision ddot,t
      double precision s
      integer j,jm1,k
c     begin block with ...exits to 40

c
c
         do 30 j = 1, n
            info = j
            s = 0.0d0
            jm1 = j - 1
            if (jm1 .lt. 1) go to 20
            do 10 k = 1, jm1
               t = a(k,j) - ddot(k-1,a(1,k),1,a(1,j),1)
               t = t/a(k,k)
               a(k,j) = t
               s = s + t*t
   10       continue
   20       continue
            s = a(j,j) - s
c     ......exit

            if (s .le. 0.0d0) go to 40
            a(j,j) = dsqrt(s)
   30    continue
         info = 0
   40 continue
      return
      end
      subroutine dposl(a,lda,n,b)

      integer lda,n
      double precision a(lda,1),b(1)
c

c     dposl solves the double precision symmetric positive definite

c     system a * x = b
c     using the factors computed by dpoco or dpofa.
c
c     on entry
c
c        a       double precision(lda, n)
c                the output from dpoco or dpofa.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer

c                the order of the matrix  a .
c
c        b       double precision(n)
c                the right hand side vector.
c

c     on return
c
c        b       the solution vector  x .

c
c     error condition
c
c        a division by zero will occur if the input factor contains
c        a zero on the diagonal.  technically this indicates
c        singularity but it is usually caused by improper subroutine
c        arguments.  it will not occur if the subroutines are called
c        correctly and  info .eq. 0 .
c
c     to compute  inverse(a) * c  where  c  is a matrix

c     with  p  columns
c           call dpoco(a,lda,n,rcond,z,info)
c           if (rcond is too small .or. info .ne. 0) go to ...
c           do 10 j = 1, p
c              call dposl(a,lda,n,c(1,j))
c        10 continue
c
c     linpack.  this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c

c     subroutines and functions
c
c     blas daxpy,ddot
c
c     internal variables
c
      double precision ddot,t
      integer k,kb
c
c     solve trans(r)*y = b
c
      do 10 k = 1, n

         t = ddot(k-1,a(1,k),1,b(1),1)
         b(k) = (b(k) - t)/a(k,k)
   10 continue
c
c     solve r*x = y
c
      do 20 kb = 1, n
         k = n + 1 - kb
         b(k) = b(k)/a(k,k)
         t = -b(k)
         call daxpy(k-1,t,a(1,k),1,b(1),1)
   20 continue
      return
      end
      subroutine dsifa(a,lda,n,kpvt,info)
      integer lda,n,kpvt(1),info
      double precision a(lda,1)
c
c     dsifa factors a double precision symmetric matrix by elimination
c     with symmetric pivoting.

c
c     to solve  a*x = b , follow dsifa by dsisl.
c     to compute  inverse(a)*c , follow dsifa by dsisl.
c     to compute  determinant(a) , follow dsifa by dsidi.
c     to compute  inertia(a) , follow dsifa by dsidi.
c     to compute  inverse(a) , follow dsifa by dsidi.
c
c     on entry
c
c        a       double precision(lda,n)
c                the symmetric matrix to be factored.
c                only the diagonal and upper triangle are used.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c     on return
c
c        a       a block diagonal matrix and the multipliers which
c                were used to obtain it.

c                the factorization can be written  a = u*d*trans(u)
c                where  u  is a product of permutation and unit
c                upper triangular matrices , trans(u) is the
c                transpose of  u , and  d  is block diagonal

c                with 1 by 1 and 2 by 2 blocks.
c
c        kpvt    integer(n)
c                an integer vector of pivot indices.
c
c        info    integer
c                = 0  normal value.
c                = k  if the k-th pivot block is singular. this is
c                     not an error condition for this subroutine,
c                     but it does indicate that dsisl or dsidi may
c                     divide by zero if called.
c

c     linpack. this version dated 08/14/78 .
c     james bunch, univ. calif. san diego, argonne nat. lab.

c
c     subroutines and functions
c
c     blas daxpy,dswap,idamax
c     fortran dabs,dmax1,dsqrt
c
c     internal variables
c
      double precision ak,akm1,bk,bkm1,denom,mulk,mulkm1,t
      double precision absakk,alpha,colmax,rowmax
      integer imax,imaxp1,j,jj,jmax,k,km1,km2,kstep,idamax
      logical swap
c
c
c     initialize
c
c     alpha is used in choosing pivot block size.
      alpha = (1.0d0 + dsqrt(17.0d0))/8.0d0
c
      info = 0
c
c     main loop on k, which goes from n to 1.
c
      k = n
   10 continue
c
c        leave the loop if k=0 or k=1.
c
c     ...exit
         if (k .eq. 0) go to 200
         if (k .gt. 1) go to 20
            kpvt(1) = 1
            if (a(1,1) .eq. 0.0d0) info = 1
c     ......exit
            go to 200
   20    continue
c
c        this section of code determines the kind of
c        elimination to be performed.  when it is completed,
c        kstep will be set to the size of the pivot block, and
c        swap will be set to .true. if an interchange is
c        required.
c
         km1 = k - 1
         absakk = dabs(a(k,k))
c
c        determine the largest off-diagonal element in
c        column k.

c

         imax = idamax(k-1,a(1,k),1)
         colmax = dabs(a(imax,k))
         if (absakk .lt. alpha*colmax) go to 30
            kstep = 1
            swap = .false.
         go to 90
   30    continue
c
c           determine the largest off-diagonal element in
c           row imax.
c
            rowmax = 0.0d0

            imaxp1 = imax + 1
            do 40 j = imaxp1, k

               rowmax = dmax1(rowmax,dabs(a(imax,j)))
   40       continue
            if (imax .eq. 1) go to 50
               jmax = idamax(imax-1,a(1,imax),1)
               rowmax = dmax1(rowmax,dabs(a(jmax,imax)))
   50       continue
            if (dabs(a(imax,imax)) .lt. alpha*rowmax) go to 60
               kstep = 1
               swap = .true.
            go to 80
   60       continue
            if (absakk .lt. alpha*colmax*(colmax/rowmax)) go to 70
               kstep = 1
               swap = .false.
            go to 80

   70       continue
               kstep = 2
               swap = imax .ne. km1
   80       continue
   90    continue
         if (dmax1(absakk,colmax) .ne. 0.0d0) go to 100
c
c           column k is zero.  set info and iterate the loop.
c
            kpvt(k) = k
            info = k
         go to 190

  100    continue


         if (kstep .eq. 2) go to 140
c
c           1 x 1 pivot block.
c
            if (.not.swap) go to 120
c
c              perform an interchange.
c
               call dswap(imax,a(1,imax),1,a(1,k),1)
               do 110 jj = imax, k
                  j = k + imax - jj
                  t = a(j,k)

                  a(j,k) = a(imax,j)
                  a(imax,j) = t
  110          continue
  120       continue
c
c           perform the elimination.
c
            do 130 jj = 1, km1
               j = k - jj
               mulk = -a(j,k)/a(k,k)
               t = mulk
               call daxpy(j,t,a(1,k),1,a(1,j),1)
               a(j,k) = mulk
  130       continue
c
c           set the pivot array.
c
            kpvt(k) = k

            if (swap) kpvt(k) = imax
         go to 190
  140    continue
c
c           2 x 2 pivot block.
c
            if (.not.swap) go to 160
c
c              perform an interchange.
c
               call dswap(imax,a(1,imax),1,a(1,k-1),1)
               do 150 jj = imax, km1
                  j = km1 + imax - jj
                  t = a(j,k-1)
                  a(j,k-1) = a(imax,j)
                  a(imax,j) = t
  150          continue
               t = a(k-1,k)
               a(k-1,k) = a(imax,k)
               a(imax,k) = t

  160       continue

c
c           perform the elimination.
c
            km2 = k - 2
            if (km2 .eq. 0) go to 180
               ak = a(k,k)/a(k-1,k)
               akm1 = a(k-1,k-1)/a(k-1,k)

               denom = 1.0d0 - ak*akm1
               do 170 jj = 1, km2
                  j = km1 - jj
                  bk = a(j,k)/a(k-1,k)
                  bkm1 = a(j,k-1)/a(k-1,k)
                  mulk = (akm1*bk - bkm1)/denom

                  mulkm1 = (ak*bkm1 - bk)/denom
                  t = mulk
                  call daxpy(j,t,a(1,k),1,a(1,j),1)
                  t = mulkm1
                  call daxpy(j,t,a(1,k-1),1,a(1,j),1)
                  a(j,k) = mulk
                  a(j,k-1) = mulkm1



  170          continue

  180       continue
c
c           set the pivot array.
c
            kpvt(k) = 1 - k
            if (swap) kpvt(k) = -imax
            kpvt(k-1) = kpvt(k)

  190    continue
         k = k - kstep
      go to 10
  200 continue
      return
      end
      subroutine dsisl(a,lda,n,kpvt,b)
      integer lda,n,kpvt(1)
      double precision a(lda,1),b(1)
c
c     dsisl solves the double precision symmetric system
c     a * x = b

c     using the factors computed by dsifa.
c
c     on entry
c
c        a       double precision(lda,n)
c                the output from dsifa.
c
c        lda     integer
c                the leading dimension of the array  a .
c

c        n       integer
c                the order of the matrix  a .
c
c        kpvt    integer(n)
c                the pivot vector from dsifa.
c
c        b       double precision(n)
c                the right hand side vector.
c
c     on return
c
c        b       the solution vector  x .
c
c     error condition
c
c        a division by zero may occur if  dsico  has set rcond .eq. 0.0
c        or  dsifa  has set info .ne. 0  .
c
c     to compute  inverse(a) * c  where  c  is a matrix
c     with  p  columns
c           call dsifa(a,lda,n,kpvt,info)
c           if (info .ne. 0) go to ...
c           do 10 j = 1, p
c              call dsisl(a,lda,n,kpvt,c(1,j))
c        10 continue
c
c     linpack. this version dated 08/14/78 .
c     james bunch, univ. calif. san diego, argonne nat. lab.
c
c     subroutines and functions

c
c     blas daxpy,ddot
c     fortran iabs
c
c     internal variables.
c
      double precision ak,akm1,bk,bkm1,ddot,denom,temp
      integer k,kp

c
c     loop backward applying the transformations and
c     d inverse to b.
c
      k = n
   10 if (k .eq. 0) go to 80
         if (kpvt(k) .lt. 0) go to 40
c
c           1 x 1 pivot block.
c
            if (k .eq. 1) go to 30
               kp = kpvt(k)
               if (kp .eq. k) go to 20
c
c                 interchange.
c

                  temp = b(k)
                  b(k) = b(kp)
                  b(kp) = temp

   20          continue
c
c              apply the transformation.
c
               call daxpy(k-1,b(k),a(1,k),1,b(1),1)
   30       continue
c
c           apply d inverse.
c
            b(k) = b(k)/a(k,k)
            k = k - 1
         go to 70

   40    continue

c
c           2 x 2 pivot block.
c
            if (k .eq. 2) go to 60
               kp = iabs(kpvt(k))
               if (kp .eq. k - 1) go to 50
c
c                 interchange.
c
                  temp = b(k-1)
                  b(k-1) = b(kp)
                  b(kp) = temp
   50          continue
c
c              apply the transformation.
c
               call daxpy(k-2,b(k),a(1,k),1,b(1),1)
               call daxpy(k-2,b(k-1),a(1,k-1),1,b(1),1)
   60       continue
c
c           apply d inverse.
c
            ak = a(k,k)/a(k-1,k)
            akm1 = a(k-1,k-1)/a(k-1,k)
            bk = b(k)/a(k-1,k)
            bkm1 = b(k-1)/a(k-1,k)
            denom = ak*akm1 - 1.0d0
            b(k) = (akm1*bk - bkm1)/denom
            b(k-1) = (ak*bkm1 - bk)/denom
            k = k - 2
   70    continue
      go to 10
   80 continue
c
c     loop forward applying the transformations.
c
      k = 1
   90 if (k .gt. n) go to 160
         if (kpvt(k) .lt. 0) go to 120
c
c           1 x 1 pivot block.
c
            if (k .eq. 1) go to 110

c
c              apply the transformation.
c
               b(k) = b(k) + ddot(k-1,a(1,k),1,b(1),1)
               kp = kpvt(k)
               if (kp .eq. k) go to 100


c
c                 interchange.
c
                  temp = b(k)
                  b(k) = b(kp)
                  b(kp) = temp


  100          continue

  110       continue
            k = k + 1
         go to 150
  120    continue
c
c           2 x 2 pivot block.
c
            if (k .eq. 1) go to 140
c
c              apply the transformation.
c
               b(k) = b(k) + ddot(k-1,a(1,k),1,b(1),1)
               b(k+1) = b(k+1) + ddot(k-1,a(1,k+1),1,b(1),1)
               kp = iabs(kpvt(k))
               if (kp .eq. k) go to 130
c
c                 interchange.


c
                  temp = b(k)
                  b(k) = b(kp)
                  b(kp) = temp
  130          continue
  140       continue
            k = k + 2

  150    continue

      go to 90
  160 continue
      return
      end
      subroutine dqrdc(x,ldx,n,p,qraux,jpvt,work,job)
      integer ldx,n,p,job

      integer jpvt(1)
      double precision x(ldx,1),qraux(1),work(1)
c
c     dqrdc uses householder transformations to compute the qr
c     factorization of an n by p matrix x.  column pivoting
c     based on the 2-norms of the reduced columns may be
c     performed at the users option.
c
c     on entry
c
c        x       double precision(ldx,p), where ldx .ge. n.
c                x contains the matrix whose decomposition is to be

c                computed.
c
c        ldx     integer.
c                ldx is the leading dimension of the array x.
c
c        n       integer.
c                n is the number of rows of the matrix x.
c
c        p       integer.
c                p is the number of columns of the matrix x.
c
c        jpvt    integer(p).
c                jpvt contains integers that control the selection
c                of the pivot columns.  the k-th column x(k) of x

c                is placed in one of three classes according to the
c                value of jpvt(k).
c
c                   if jpvt(k) .gt. 0, then x(k) is an initial
c                                      column.

c
c                   if jpvt(k) .eq. 0, then x(k) is a free column.

c
c                   if jpvt(k) .lt. 0, then x(k) is a final column.
c
c                before the decomposition is computed, initial columns
c                are moved to the beginning of the array x and final

c                columns to the end.  both initial and final columns
c                are frozen in place during the computation and only
c                free columns are moved.  at the k-th stage of the
c                reduction, if x(k) is occupied by a free column
c                it is interchanged with the free column of largest
c                reduced norm.  jpvt is not referenced if
c                job .eq. 0.
c
c        work    double precision(p).
c                work is a work array.  work is not referenced if
c                job .eq. 0.
c
c        job     integer.
c                job is an integer that initiates column pivoting.
c                if job .eq. 0, no pivoting is done.
c                if job .ne. 0, pivoting is done.
c

c     on return
c
c        x       x contains in its upper triangle the upper

c                triangular matrix r of the qr factorization.
c                below its diagonal x contains information from
c                which the orthogonal part of the decomposition
c                can be recovered.  note that if pivoting has

c                been requested, the decomposition is not that
c                of the original matrix x but that of x
c                with its columns permuted as described by jpvt.
c
c        qraux   double precision(p).
c                qraux contains further information required to recover
c                the orthogonal part of the decomposition.
c
c        jpvt    jpvt(k) contains the index of the column of the
c                original matrix that has been interchanged into
c                the k-th column, if pivoting was requested.
c
c     linpack. this version dated 08/14/78 .
c     g.w. stewart, university of maryland, argonne national lab.
c

c     dqrdc uses the following functions and subprograms.
c
c     blas daxpy,ddot,dscal,dswap,dnrm2
c     fortran dabs,dmax1,min0,dsqrt
c
c     internal variables
c
      integer j,jp,l,lp1,lup,maxj,pl,pu
      double precision maxnrm,dnrm2,tt
      double precision ddot,nrmxl,t
      logical negj,swapj
c
c
      pl = 1
      pu = 0
      if (job .eq. 0) go to 60
c
c        pivoting has been requested.  rearrange the columns
c        according to jpvt.
c
         do 20 j = 1, p


            swapj = jpvt(j) .gt. 0
            negj = jpvt(j) .lt. 0
            jpvt(j) = j
            if (negj) jpvt(j) = -j
            if (.not.swapj) go to 10
               if (j .ne. pl) call dswap(n,x(1,pl),1,x(1,j),1)
               jpvt(j) = jpvt(pl)
               jpvt(pl) = j
               pl = pl + 1
   10       continue

   20    continue
         pu = p
         do 50 jj = 1, p
            j = p - jj + 1
            if (jpvt(j) .ge. 0) go to 40

               jpvt(j) = -jpvt(j)

               if (j .eq. pu) go to 30
                  call dswap(n,x(1,pu),1,x(1,j),1)
                  jp = jpvt(pu)
                  jpvt(pu) = jpvt(j)
                  jpvt(j) = jp
   30          continue
               pu = pu - 1
   40       continue
   50    continue
   60 continue

c
c     compute the norms of the free columns.
c
      if (pu .lt. pl) go to 80
      do 70 j = pl, pu
         qraux(j) = dnrm2(n,x(1,j),1)
         work(j) = qraux(j)
   70 continue
   80 continue
c
c     perform the householder reduction of x.
c
      lup = min0(n,p)
      do 200 l = 1, lup
         if (l .lt. pl .or. l .ge. pu) go to 120
c
c           locate the column of largest norm and bring it
c           into the pivot position.
c
            maxnrm = 0.0d0

            maxj = l
            do 100 j = l, pu
               if (qraux(j) .le. maxnrm) go to 90
                  maxnrm = qraux(j)
                  maxj = j
   90          continue

  100       continue


            if (maxj .eq. l) go to 110
               call dswap(n,x(1,l),1,x(1,maxj),1)
               qraux(maxj) = qraux(l)
               work(maxj) = work(l)
               jp = jpvt(maxj)
               jpvt(maxj) = jpvt(l)
               jpvt(l) = jp
  110       continue
  120    continue
         qraux(l) = 0.0d0
         if (l .eq. n) go to 190
c
c           compute the householder transformation for column l.
c
            nrmxl = dnrm2(n-l+1,x(l,l),1)
            if (nrmxl .eq. 0.0d0) go to 180
               if (x(l,l) .ne. 0.0d0) nrmxl = dsign(nrmxl,x(l,l))
               call dscal(n-l+1,1.0d0/nrmxl,x(l,l),1)
               x(l,l) = 1.0d0 + x(l,l)
c
c              apply the transformation to the remaining columns,
c              updating the norms.
c
               lp1 = l + 1

               if (p .lt. lp1) go to 170
               do 160 j = lp1, p

                  t = -ddot(n-l+1,x(l,l),1,x(l,j),1)/x(l,l)
                  call daxpy(n-l+1,t,x(l,l),1,x(l,j),1)
                  if (j .lt. pl .or. j .gt. pu) go to 150
                  if (qraux(j) .eq. 0.0d0) go to 150
                     tt = 1.0d0 - (dabs(x(l,j))/qraux(j))**2
                     tt = dmax1(tt,0.0d0)
                     t = tt
                     tt = 1.0d0 + 0.05d0*tt*(qraux(j)/work(j))**2
                     if (tt .eq. 1.0d0) go to 130
                        qraux(j) = qraux(j)*dsqrt(t)
                     go to 140

  130                continue
                        qraux(j) = dnrm2(n-l,x(l+1,j),1)
                        work(j) = qraux(j)
  140                continue
  150             continue
  160          continue
  170          continue
c
c              save the transformation.

c
               qraux(l) = x(l,l)
               x(l,l) = -nrmxl
  180       continue

  190    continue
  200 continue
      return
      end
