cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compute pseudo-left image.  For each pixel coordinate (i,j) in the image,
c compute the matching pixel in the right image.
c    z(i,j)=y(i+dx(i,j),j+dy(i,j))
c Since the matching right coordinates are non-integer valued, bilinear
c interpolation over y is used to compute z.
c
      subroutine compute_z(y,dx,dy,z,nl,ns)
      implicit none
      integer*4 nl,ns
      integer*2 y(ns,nl)		!right image
      real*4 dx(ns,nl),dy(ns,nl)	!horizontal and vertical displacements
      real*4 z(ns,nl)			!output pseudo-left image

      real*8 wr,wl,wb,top,bot
      real*4 line,samp
      integer*4 iline,isamp,i,j

      do j=1,nl
         do i=1,ns
            if (dx(i,j).ne.-999.) then
               line = j + dy(i,j)
               samp = i + dx(i,j)
               iline = line
               isamp = samp
               wr = samp - isamp
               wl = 1.d0 - wr
               wb = line - iline
               top = wl*y(isamp,iline) + wr*y(isamp+1,iline)
               bot = wl*y(isamp,iline+1) + wr*y(isamp+1,iline+1)
               z(i,j) = bot*wb + top*(1.0d0-wb)
            else
               z(i,j) = -999.
            endif
         enddo
      enddo

      return
      end
