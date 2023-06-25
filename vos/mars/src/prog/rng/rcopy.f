cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Copy the displacements from (dx,dy) to (dx0,dy0).
c
      subroutine rcopy(nl,ns,dx,dy,dx0,dy0,mask)
      implicit none
      integer*4 nl,ns
      real*4 dx(ns,nl),dy(ns,nl)
      real*4 dx0(ns,nl),dy0(ns,nl)
      integer*2 mask(nl,ns)

      integer*4 i,j

      do j=1,nl
         do i=1,ns
            if (mask(i,j).eq.0) then
               dx0(i,j) = dx(i,j)
               dy0(i,j) = dy(i,j)
            else
               dx0(i,j) = -999.
               dy0(i,j) = -999.
            endif
         enddo
      enddo

      return
      end
