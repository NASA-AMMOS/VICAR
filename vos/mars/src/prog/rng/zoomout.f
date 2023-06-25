cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Reduce the spatial resolution of the input image (left or right) by doing
c an n x n pixel summation.
c
      subroutine zoomout(l,l0,nl,ns,zoom)
      implicit none
      integer*4 nl,ns,zoom
      integer*2 l(nl,ns),l0(nl/zoom,ns/zoom)

      integer*4 i,j,ii,jj,isum,i4,j4
      real*8 area

      area = zoom**2
      j4 = 0

      do 50 j=1,nl,zoom
      j4 = j4 + 1
      i4 = 0
      
      do i=1,ns,zoom
         i4 = i4 + 1
         isum = 0
         do jj=0,zoom-1
            do ii=0,zoom-1
               isum = isum + l(i+ii,j+jj)
            enddo
         enddo
         l0(i4,j4) = isum/area
      enddo
   50 continue

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Reduce spatial resolution of horizon location.
c
      subroutine zoomhrzn(hrznl,hrznr,hrznl0,hrznr0,ns,zoom)
      implicit none
      integer*4 ns,zoom
      integer*2 hrznl(ns),hrznr(ns)	!input left and right horizons
      integer*2 hrznl0(ns),hrznr0(ns)	!output horizons

      integer*4 i,i0

      do i=1,ns,zoom
         i0 = (i-1)/zoom + 1
         hrznl0(i0) = (hrznl(i)-1)/zoom + 1
         hrznr0(i0) = (hrznr(i)-1)/zoom + 1
      enddo
      return
      end
