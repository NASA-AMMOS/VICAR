cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Project pixels centered at pic(s,l) into an nlw x nsw window.
c
      subroutine project(pic,nl,ns,l,s,is,it,window,nlwh,nswh,
     &		area,avg,var,ind)
      implicit none
c     ..inputs
      integer*4 nl,ns
      integer*2 pic(ns,nl)		!left or right image
      integer*4 l,s			!center of window is pic(s,l)
      integer*4 is,it			!scale and theta indices
      integer*4 nlwh,nswh		!half-window dimensions
      real*8 area			!number of pixels in window
c     ...outputs
      real*4 window(-nswh:nswh,-nlwh:nlwh)	!projected window
      real*8 avg,var			!average and variance of window
      integer*4 ind			!ind=1 success

      common/cs/cm
      real*8 cm(-4:4)

      common/ct/ct,st,deglo,deghi,ddeg
      real*8 ct(360),st(360)
      integer*4 deglo,deghi,ddeg

      integer*4 i,j
      real*8 Cminor,sint,cost
      real*8 dn,sum,sum2
      real*8 lp,sp
      real*8 pi/3.141592654/
      real*8 avg2,var2

      Cminor = cm(is)
      sint = st(it)
      cost = ct(it)
      sum = 0.d0
      sum2 = 0.d0

      do j=-nlwh,nlwh
         do i=-nswh,nswh
            sp = Cminor*(i*cost + j*sint) + s
            lp = Cminor*(-i*sint + j*cost) + l
            call BilinearInt(pic,lp,sp,dn,nl,ns,ind)
            if (ind.eq.0) return
            window(i,j) = dn
            sum = sum + dn
            sum2 = sum2 + dn*dn
         enddo
      enddo

      avg = sum/area
      var = sum2/area - avg*avg
      return
      end
