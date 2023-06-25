cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Solve the linear equation Ax=b for x, where A is an mxn matrix, m>n,
c and b is an m-vector.
c
      subroutine LinearSolution(A,x,b,m,n,AtA,Atb,L)
      implicit none
      integer*4 m,n
      real*8 A(m,n),x(n),b(m)
      real*8 AtA(n,n),Atb(n)
      real*8 L(n,n)

      real*8 z(10)		!10 coefficients in bilinear fit
      integer i,j
      character*133 msg

      call compute_AtA(A,AtA,m,n)

ccc      do i=1,n
ccc         write(msg,12) (AtA(i,j),j=1,n)
ccc         call xvmessage(msg,' ')
ccc      enddo
ccc   12 format('AtA=',10f11.2)

      call compute_Atb(A,b,Atb,m,n)

c      write(msg,21) (Atb(j),j=1,n)
c      call xvmessage(msg,' ')
c   21 format('Atb=',10f11.2)

      call Cholesky(AtA,L,n)

c      do i=1,n
c         write(msg,20) (L(i,j),j=1,n)
c         call xvmessage(msg,' ')
c      enddo
c   20 format('L=',10f11.5)

c     ...Solve Lz=Atb for z by forward substitution
      call forward_sub(L,z,Atb,n)

c      call xvmessage(' ',' ')
c      write(msg,52) (z(i),i=1,n)
c      call xvmessage(msg,' ')
c   52 format('z=',10f11.1)

c     ...Solve Ltx=z for x by backward substitution
      call backward_sub(L,x,z,n)
      return
      end
