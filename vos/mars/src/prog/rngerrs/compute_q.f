ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compare left image x and pseudo-left image z. Output correlation coeff q.
c
      subroutine compute_q(x,z,q,nq,nl,ns) 
      implicit none
      integer*4 nl,ns			!image size is nl x ns
      integer*2 x(ns,nl)		!left image
      real*4 z(ns,nl)			!pseudo-left image
      real*4 q(ns,nl)			!correlation coefficient (output)
      integer*2 nq(ns,nl)		!# of valid pixls in corr area (output)

c     ...local variables
      integer*4 nlw,nsw			!nlw and nsw must be odd integers
      integer*4 n,m			!nlw=n+1+n  nsw=m+1+m
      integer*4 ibeg,iend,jbeg,jend	!image margins to make room for window
      integer*4 i,j,npts
      integer*4 iold,inew,jold,jnew
      real*8 x0,z0,ssx,ssz,ssx2,ssxz,ssz2
      real*8 avgx,avgz,varx,varz,cov

c     ...column sums for each sample on current image line
      integer*4 maxns
      parameter (maxns=1024)
      real*8 sx(maxns),sz(maxns),sx2(maxns),sxz(maxns),sz2(maxns)
      integer*2 np(maxns)

      do j=1,nl
         do i=1,ns
            q(i,j) = -1.
         enddo
      enddo

c     ...compute bounding rectangle of image to leave room at the
c     ...image margins for correlation window
      call window_size(nlw,nsw)		!get nlw and nsw parameters
      n = nlw/2				!half windows
      m = nsw/2
      ibeg = 1 + m	!indent left
      iend = ns - m	!indent right
      jbeg = 1 + n	!indent top
      jend = nl - n	!indent bottom

c     ...initialize the column sums to the first nlw lines of the image
      do i=1,ns
         sx(i) = 0.d0
         sz(i) = 0.d0
         sx2(i) = 0.d0
         sxz(i) = 0.d0
         sz2(i) = 0.d0
         np(i) = 0
      enddo

      do j=1,nlw
         do i=1,ns
            x0 = x(i,j)
            z0 = z(i,j)
            if (z0.gt.-999.d0) then
               sx(i) = sx(i) + x0
               sz(i) = sz(i) + z0
               sx2(i) = sx2(i) + x0*x0
               sxz(i) = sxz(i) + x0*z0
               sz2(i) = sz2(i) + z0*z0
               np(i) = np(i) + 1
               endif
         enddo
      enddo

      do 50 j=jbeg,jend				!image line loop
      ssx = 0.d0
      ssz = 0.d0
      ssx2 = 0.d0
      ssxz = 0.d0
      ssz2 = 0.d0
      npts = 0

      do i=1,nsw				!area sums for left-most window
         ssx = ssx + sx(i)
         ssz = ssz + sz(i)
         ssx2 = ssx2 + sx2(i)
         ssxz = ssxz + sxz(i)
         ssz2 = ssz2 + sxz(i)
         npts = npts + np(i)
      enddo

      do 40 i=ibeg,iend				!image sample loop
      avgx = ssx/npts
      avgz = ssz/npts
      varx = ssx2/npts - avgx*avgx
      varz = ssz2/npts - avgz*avgz
      if (varx.gt.0.1 .and. varz.gt.0.1) then
         cov = ssxz/npts - avgx*avgz
         q(i,j) = cov/dsqrt(varx*varz)		!correlation coefficient
         nq(i,j) = npts
      endif
      iold = i - m				!sample rolling out of window
      inew = i + m + 1				!sample rolling into window
      ssx = ssx - sx(iold) + sx(inew)		!update area sums
      ssz = ssz - sz(iold) + sz(inew)
      ssx2 = ssx2 - sx2(iold) + sx2(inew)
      ssxz = ssxz - sxz(iold) + sxz(inew)
      ssz2 = ssz2 - sz2(iold) + sz2(inew)
      npts = npts - np(iold) + np(inew)
   40 continue

      jold = j - n				!line rolling out of window
      jnew = j + n + 1				!line rolling into window
      do i=1,ns					!update column sums
         x0 = x(i,jold)
         z0 = z(i,jold)
         if (z0.gt.-999.d0) then
            sx(i) = sx(i) - x0
            sz(i) = sz(i) - z0
            sx2(i) = sx2(i) - x0*x0
            sxz(i) = sxz(i) - x0*z0
            sz2(i) = sz2(i) - z0*z0
            np(i) = np(i) - 1
         endif
         x0 = x(i,jnew)
         z0 = z(i,jnew)
         if (z0.gt.-999.d0) then
            sx(i) = sx(i) + x0
            sz(i) = sz(i) + z0
            sx2(i) = sx2(i) + x0*x0
            sxz(i) = sxz(i) + x0*z0
            sz2(i) = sz2(i) + z0*z0
            np(i) = np(i) + 1
         endif
      enddo
   50 continue

      return
      end
