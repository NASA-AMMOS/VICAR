ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Match a specified left area with a right area.
c
      subroutine get_match(nl,ns,l,r,avg,var,avgr,varr,hrznr,
     &		rho,i,j,h,k,dx,dy,rmax,qthresh,flag)
      implicit none
      integer*4 nl,ns			!image size is nl x ns
      integer*2 l(ns,nl),r(ns,nl)	!left and right images
      real*4 avg,var			!avg and variance of left area
      real*4 avgr(ns,nl),varr(ns,nl)	!vvg and variance of right areas
      integer*2 hrznr(ns)		!horizon location in right image
      real*4 rho(1)			!correlation coefficients
      integer*4 i,j,h,k,flag
      real*4 dx,dy,qthresh
      real*8 rmax

      integer*4 hlo,hhi,klo,khi		!search rectangle
      integer*4 hmax,kmax,heps,keps,n

      heps = 3
      keps = 3
      hlo = h - heps 			!compute search rectangle
      hhi = h + heps
      klo = k - keps
      khi = k + keps
      n = (hhi-hlo+1)*(khi-klo+1)	!search area
      call fill_buf(rho,n,-999.)
      flag = 1

   10 call correlate1(nl,ns,l,r,avg,var,avgr,varr,hrznr,
     &      hlo,hhi,klo,khi,rho,i,j,h,k,hmax,kmax,rmax)
      if (rmax.eq.-1.0) return
      if (hmax.eq.h .and. kmax.eq.k) goto 30
      if (kmax.le.klo .or. kmax.ge.khi) goto 40
      if (hmax.le.hlo .or. hmax.ge.hhi) goto 40
      h = hmax				!step to new max and try again
      k = kmax
      goto 10

   30 if (rmax.lt.qthresh) return	!correlation too low
      call interp_max(rho,hlo,hhi,klo,khi,h,k,dx,dy)
      flag = 0				!good range data
      return
   40 dx = h
      dy = k
      if (rmax.lt.qthresh) return	!correlation too low
      flag=2				!max is on the edge of search area
      return
      end
