cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine compute_Atr(A,r,Atr,magAtr,m,n)
      implicit none
      integer*4 m,n
      real*8 A(m,n),r(m)	!inputs
      real*8 Atr(n),magAtr	!returned

      integer*4 i,j,k
      real*8 sum2
      character*80 msg

      sum2 = 0
      do j=1,n
         Atr(j) = 0
         do i=1,m
            Atr(j) = Atr(j) + A(i,j)*r(i)
         enddo
         sum2 = sum2 + Atr(j)**2
      enddo
      magAtr = dsqrt(sum2)
      return
      end
