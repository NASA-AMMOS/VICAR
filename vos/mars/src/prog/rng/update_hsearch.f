cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Update ilox for next line.
c
      subroutine update_hsearch(q,dx,ns,qthresh,m,ilox)
      implicit none
      integer*4 ns,m,ilox
      real*4 q(ns),dx(ns),qthresh

      integer*4 i
      real*4 di,dmin,delta

      dmin = ns

      do 20 i=1+m,ns-m
      if (q(i).lt.qthresh) goto 20
      di = dx(i)
      if (di.lt.dmin .and. di.ne.-999.) dmin=di
   20 continue

      delta = 0.1*dmin
      if (delta.lt.-4.) delta=-4.
      i = dmin + delta - 1.
      if (i.gt.ilox) ilox=i
      return
      end
