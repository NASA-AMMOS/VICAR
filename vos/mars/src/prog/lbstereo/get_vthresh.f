cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compute variance threshold.
c
      subroutine get_vthresh(var,nl,ns,vthresh)
      implicit none
c     ...inputs
      integer*4 nl,ns
      real*4 var(ns,nl)
c     ...output
      real*4 vthresh

      integer*4 i,j,n
      real*8 sum,sum2,a,v

      n = 0
      sum = 0.d0
      sum2 = 0.d0

      do j=1,nl
         do i=1,ns
            v = var(i,j)
            if (v.gt.0.) then
               n = n + 1
               sum = sum + v
               sum2 = sum2 + v*v
            endif
         enddo
      enddo

      if (n.eq.0) call mabend('***not enough info in image',' ')
      a = sum/n
      v = sum2/n - a*a
      v = dsqrt(v)
      call prnt(8,1,a,'a=.')
      call prnt(8,1,v,'v=.')
      vthresh = a/10.
      call prnt(7,1,vthresh,'vthresh=.')
      return
      end
