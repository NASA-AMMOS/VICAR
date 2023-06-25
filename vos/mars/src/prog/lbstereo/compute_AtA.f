cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compute AtA, where A is an mxn matrix.
c
      subroutine compute_AtA(A,AtA,m,n)
      implicit none

      integer m,n	!Matrix A is mxn
      real*8 A(m,n)	!input
      real*8 AtA(n,n)	!output product of A transpose and A

      integer i,j,k
      real*8 sum

c     ...Compute AtA
      do j=1,n
         do i=1,j
            sum = 0.d0
            do k=1,m
               sum = sum + A(k,i)*A(k,j)
            enddo
            AtA(i,j) = sum
            AtA(j,i) = sum
         enddo
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Given an mxn matrix A and an n-vector b, compute Atb.
c
      subroutine compute_Atb(A,b,Atb,m,n)
      implicit none
      integer*4 m,n
      real*8 A(m,n),b(n)	!inputs
      real*8 Atb(m)		!output

      integer i,j
      real*8 sum

      do j=1,n
         sum =0.d0
         do i=1,m
            sum = sum + A(i,j)*b(i)
         enddo
         Atb(j) = sum
      enddo

      return
      end
