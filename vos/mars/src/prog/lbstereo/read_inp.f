cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Read left or right image into pic(ns,nl).
c
      subroutine read_inp(iunit,nl,ns,pic)
      implicit none
      integer iunit,nl,ns	!inputs
      integer*2 pic(ns,nl)	!output image

      integer i,j,ind

      do j=1,nl
         call xvread(iunit,pic(1,j),ind,' ')
c         pic(1,j) = 0
c         pic(2,j) = 0
c         pic(ns,j) = 0
c         pic(ns-1,j) = 0
      enddo
c      do i=1,ns
c         pic(i,1) = 0
c      enddo
      call xvclose(iunit,ind,' ')
      return
      end
