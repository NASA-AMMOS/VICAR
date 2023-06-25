cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Read left or right image into x(ns,nl) or y(ns,nl).
c
      subroutine read_inp(iunit,x,nl,ns)
      implicit none
      integer iunit,nl,ns	!inputs
      integer*2 x(ns,nl)	!output

      integer j,ind

      do j=1,nl
         call xvread(iunit,x(1,j),ind,' ')
      enddo
      call xvclose(iunit,ind,' ')
      return
      end
