cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine get_window(nl,ns,pic,l,s,nlwh,nswh,window)
      implicit none
      integer*4 nl,ns
      integer*2 pic(ns,nl)
      integer*4 l,s
      integer*4 nlwh,nswh
      real*8 window(-nswh:nswh,-nlwh:nlwh)

      integer*4 i,j

      do j=-nlwh,nlwh
         do i=-nswh,nswh
            window(i,j) = pic(s+i,l+j)
         enddo
      enddo
      return
      end
