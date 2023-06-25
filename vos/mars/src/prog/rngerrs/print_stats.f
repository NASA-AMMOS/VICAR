ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Print stats on q and nq images.
c
      subroutine print_stats(z,q,nq,nl,ns) 
      implicit none
      integer*4 nl,ns			!image size is nl x ns
      real*4 z(ns,nl)			!pseudo-left (invalid pxls flagged here)
      real*4 q(ns,nl)			!corr quality
      integer*2 nq(ns,nl)		!# pixels in correlation area

      integer*4 npts,cnt,par(4)
      integer*4 sl,ss,el,es,i,j
      real*8 qsum,qavg
      character*80 msg
  101 format('avg q=',f10.5,' npts=',i7)

      call xvp('AREA',par,cnt)
      if (cnt.eq.4) then
         sl = par(1)
         ss = par(2)
         el = par(3) + sl - 1
         es = par(4) + ss - 1
      else
         sl = 1
         ss = 1
         el = nl
         es = ns
      endif

      qsum = 0
      npts = 0

      do j=sl,el
         do i=ss,es
            if (z(i,j).ne.-999.) then
               qsum = qsum + q(i,j)
               npts = npts + 1
            endif
         enddo
      enddo
      qavg = qsum/npts
      write(msg,101) qavg,npts
      call xvmessage(msg,' ')
      return
      end
