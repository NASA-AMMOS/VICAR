cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Match pixel (i1,j1) of left image by correlating over a 3x3 search area
c centered at (i2,j2)=(i1+h,j1+k).
c
      subroutine correlate1(nl,ns,l,r,avg,var,avgr,varr,hrznr,
     &		hlo,hhi,klo,khi,rho,i1,j1,h,k,hmax,kmax,rmax)
      implicit none
      integer*4 nl,ns			!images are nl x ns
      integer*2 l(ns,nl),r(ns,nl)	!left and right images
      real*4 avg,var			!avg & variance for left pixel
      real*4 avgr(ns,nl),varr(ns,nl)	!avg & variance for right image
      integer*2 hrznr(ns)		!horizon in right image
      integer*4 hlo,hhi,klo,khi		!search area limits
      real*4 rho(hlo:hhi,klo:khi)	!correlation coefficients**2
      integer*4 i1,j1			!left area central pixel=(i1,j1)
      integer*4 h,k			!estimated displacements
      integer*4 hmax,kmax		!displacements for rmax
      real*8 rmax

      common/cq/nlw,nsw,minvar
      integer*4 nlw,nsw                 !correlation window is nlw x nsw
      real*4 minvar			!min variation in correlation window

      integer*4 i2,j2			!right area central pixel=(i2,j2)
      integer*4 m,n,i,j,di,dj
      integer*4 x,y,isum
      real*8 area,slr,cov,rr

      area = nlw*nsw
      m = nsw/2
      n = nlw/2
      hmax = -999.
      kmax = -999.
      rmax = -1.0

c     ...Compute the correlation matrix rho
      do 30 dj=k-1,k+1
      j2 = j1 + dj
      do 20 di=h-1,h+1
      i2 = i1 + di
      rr = -1.0
      if (hrznr(i2).gt.j2) goto 20		!skip if right pixel in horizon
      if (varr(i2,j2).lt.minvar) goto 20	!skip if no info in rght window 
      rr = rho(di,dj)
      if (rr.ne.-999.) goto 10			!value already computed
      slr = 0.d0
      do j=-n,n
         isum = 0
         do i=-m,m
            x = l(i1+i,j1+j)
            y = r(i2+i,j2+j)
            isum = isum + x*y
         enddo
         slr = slr + isum
      enddo
      cov = slr/area - avg*avgr(i2,j2)
      rr = (cov*cov)/(var*varr(i2,j2))
      if (cov.lt.0.) rr=-rr
   10 if (rr.gt.rmax) then
         rmax = rr
         hmax = di
         kmax = dj
      endif
   20 rho(di,dj) = rr
   30 continue

      return
      end
