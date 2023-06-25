cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c compute
c             b = Ax - r
c
      subroutine compute_b(A,x,r,b,m,n)
      implicit none
      integer*4 m,n
      real*8 A(m,n),x(n),r(m)
      real*8 b(m)

      integer*4 i,j

      do i=1,m
         b(i) = -r(i)
         do j=1,n
            b(i) = b(i) + A(i,j)*x(j)
         enddo
      enddo
      return
      end
