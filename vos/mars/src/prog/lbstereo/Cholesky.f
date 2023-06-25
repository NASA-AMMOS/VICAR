ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Cholesky factorization routine:
c   Factor an nxn positive-definite matrix into a product between a
c   lower-triangular matrix and its transpose:  A=LLt

      subroutine Cholesky(A,L,n)
      implicit none
      integer*4 n
      real*8 A(n,n)		!destroyed during calculation
      real*8 L(n,n)		!returned

      real*8 L1(10)		!enough for bilinear approximation
      integer*4 i,j,k,m

      m = n

      do 20 k=1,n-1
      L1(1) = dsqrt(A(1,1))
      do i=2,m
         L1(i) = A(i,1)/L1(1)
      enddo

c     ...Save the results in L
      do i=1,m
         L(k+i-1,k) = L1(i)
      enddo


c     ...Replace A by A22-L21L21t
      do i=2,m
         do j=2,m
            A(i-1,j-1) = A(i,j) - L1(i)*L1(j)
         enddo
      enddo
      m = m - 1
   20 continue

      L(n,n) = dsqrt(A(1,1))
      do i=1,n-1
         do j=i+1,n
            L(i,j) = 0.d0
         enddo
      enddo
      return
      end
