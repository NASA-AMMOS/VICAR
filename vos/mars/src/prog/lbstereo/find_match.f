ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Given a reference window, find the matching window in an image r.
c
      subroutine find_match(window,nlwh,nswh,r,avgr,varr,nl,ns,
     &		avg,var,vthresh,dl,ds,l,s,lout,sout,rmax,var2)
      implicit none
c     ...inputs
      integer*4 nl,ns
      integer*2 r(ns,nl)		!image is nl x ns
      real*4 avgr(ns,nl),varr(ns,nl)	!mean and variance of image
      integer*4 nlwh,nswh
      real*4 window(-nswh:nswh,-nlwh:nlwh)
      real*8 avg,var			!mean and variance of window
      real*4 vthresh			!variance threshold
      integer*4 dl,ds			!vert and hori search radii
      integer*4 l,s			!approximate location of match

c     ...outputs
      integer*4 lout,sout		!computed location of match
      real*4 rmax			!correlation max 
      real*4 var2			!variance of matching window

      integer*4 nlw,nsw			!window is nlw x nsw
      real*8 npix			!npix = nlw*nsw

      integer*4 i,j,di,dj
      integer*4 l2,s2,maxdi,maxdj
      real*8 sum,cross,cov,rr
      real*4 dn1,dn2,avg2,dx,dy

      nlw = 2*nlwh + 1
      nsw = 2*nswh + 1
      npix = nlw*nsw

      rmax = -1.0

      do 20 dj=-dl,dl
      do 20 di=-ds,ds
      rr = -1.0
      l2 = l + dj
      s2 = s + di
      if (l2.lt.1 .or. l2.gt.nl) goto 20
      if (s2.lt.1 .or. s2.gt.ns) goto 20
      avg2 = avgr(s2,l2)
      var2 = varr(s2,l2)
      if (var2.lt.vthresh) goto 20	!skip if no info in target window 
      cross = 0.d0

      do j=-nlwh,nlwh
         do i=-nswh,nswh
            dn1 = window(i,j)
            dn2 = r(s2+i,l2+j)
            cross = cross + dn1*dn2
         enddo
      enddo

      cov = cross/npix - avg*avg2
      rr = (cov*cov)/(var*var2)
      if (cov.le.0.) rr=-rr
      if (rr.gt.rmax) then
         rmax = rr
         maxdi = di
         maxdj = dj
      endif
   20 continue

      lout = l + maxdj
      sout = s + maxdi
      var2 = varr(sout,lout)
      if (maxdi.eq.-ds .or. maxdi.eq.-ds) rmax=-1.0
      if (maxdj.eq.-dl .or. maxdj.eq.-dl) rmax=-1.0
      return
      end
