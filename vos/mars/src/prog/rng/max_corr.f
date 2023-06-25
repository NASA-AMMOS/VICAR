cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Find max correlation value in search window.
c Outputs are q,dx,dy,mask
c
      subroutine max_corr(q,dx,dy,mask,nl,ns,rho,ilo,ihi,jlo,jhi,
     &		qthresh,ilox,ibeg,iend,j,npix)
      implicit none
      integer*4 nl,ns
      real*4 q(ns,nl)			!corr 3 highest local maxima
      real*4 dx(ns,nl),dy(ns,nl)	!Displacements for max corr
      integer*2 mask(ns,nl)
      integer*4 ilo,ihi,jlo,jhi
      real*4 rho(ilo:ihi,jlo:jhi,ns)	!Correlation surface
      real*4 qthresh
      integer*4 ilox,ibeg,iend,j,npix

      common/climits/nilo,nihi,njlo,njhi
      integer*4 nilo,nihi,njlo,njhi

      integer*4 i,i2,di,dj,flag
      integer*4 imax,jmax
      real*4 rr,rmax

      do 50 i=ibeg,iend
      rmax = -1.0
      
      do 40 di=ilox,ihi
      i2 = i + di
      if (i2.lt.1 .or. i2.gt.ns) goto 40
      do 35 dj=jlo,jhi
      rr = rho(di,dj,i)
      if (rr.gt.rmax) then
         rmax = rr
         imax = di
         jmax = dj
      endif
   35 continue
   40 continue

      flag = 0
      if (rmax.eq.-1.0) goto 50
      q(i,j) = rmax
      dx(i,j) = imax
      dy(i,j) = jmax

c     ...reject if max occurs in edge of search window
      if (imax.eq.ilox) then
         nilo = nilo + 1
         flag = 1
      elseif (imax.eq.ihi) then
         nihi = nihi + 1
         flag = 1
      elseif (jmax.eq.jlo) then
         njlo = njlo +1
         flag = 1
      elseif (jmax.eq.jhi) then
         njhi = njhi + 1
         flag = 1
      endif
      if (flag.eq.1) then
         mask(i,j) = 2
         goto 50
      endif

      if (rmax.lt.qthresh) then
         mask(i,j) = 3
         goto 50
      endif
      mask(i,j) = 0
      npix = npix + 1
      call interp_max(rho(ilo,jlo,i),ilo,ihi,jlo,jhi,imax,jmax,
     &		dx(i,j),dy(i,j))
   50 continue

      return
      end
