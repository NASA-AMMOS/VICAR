ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Solve Lz=b for z by forward substitution
c
      subroutine forward_sub(L,z,b,n)
      implicit none
      integer n
      real*8 L(n,n)	!input lower-triangular matrix
      real*8 z(n)	!output solution
      real*8 b(n)	!input, where Lz=b

      integer*4 i,j
      real*8 sum

      do i=1,n
         sum = 0.d0
         do j=1,i-1
            sum = sum + L(i,j)*z(j)
         enddo
         z(i) = (b(i)-sum)/L(i,i)
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Solve Ltx=z for x by backward substitution
c
      subroutine backward_sub(L,x,z,n)
      implicit none
      integer n
      real*8 L(n,n)	!input lower-triangular matrix
      real*8 x(n)	!output solution
      real*8 z(n)	!input, where Ltx=z

      integer*4 i,j
      real*8 sum

      do i=n,1,-1
         sum = 0.d0
         do j=i+1,n
            sum = sum + L(j,i)*x(j)
         enddo
         x(i) = (z(i)-sum)/L(i,i)
      enddo

      return
      end
