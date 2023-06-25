cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Read left or right image into pic(ns,nl).
c
      subroutine read_inp(iunit,nl,ns,pic)
      implicit none
      integer iunit,nl,ns	!inputs
      integer*2 pic(ns,nl)	!output image

      integer j,ind

      do j=1,nl
         call xvread(iunit,pic(1,j),ind,' ')
      enddo
      call xvclose(iunit,ind,' ')
      return
      end
