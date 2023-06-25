ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine flag_ratios(ratbuf,flag,lnpts)
      implicit none
      integer*4 lnpts
      real*4 ratbuf(lnpts)		!ratbuf(i1) = sig1/sig2
      integer*2 flag(lnpts)             !flag(i1)=0 if tiepoint is valid

      common/cp/debug,print
      logical*1 debug,print

      integer*4 i1,imax,cnt
      integer*4 ngood,nrat
      real*8 ratio,sum,sum2,avg,sig,eps  !sig1/sig2 test
      real*4 f,rthresh,diff,maxdiff
      character*132 msg
  108 format('avg ratio=',f11.3,' eps=',f10.3)

      call xvp('sfactor',f,cnt)
      call xvp('rthresh',rthresh,cnt)
      ngood = 0
      sum = 0.d0
      sum2 = 0.d0

c     ...compute the mean and sigma
      do i1=1,lnpts
         if (flag(i1).eq.0) then	!skip the noise
            ratio = ratbuf(i1)
            sum = sum + ratio
            sum2 = sum2 + ratio**2
            ngood = ngood + 1
         endif
      enddo

c     ...weed out farthest outlier
      nrat = 0
   10 if (ngood.lt.3) goto 990
      avg = sum/ngood
      sig = dsqrt(sum2/ngood - avg**2)
      eps = f*sig
      if (eps.lt.0.1) eps=0.1
      if (eps.gt.rthresh) eps=rthresh

      maxdiff = -999.			!search for farthest outlier
      do i1=1,lnpts
         if (flag(i1).eq.0) then
            diff = abs(ratbuf(i1)-avg)
            if (diff.gt.maxdiff) then
               maxdiff = diff
               imax = i1
            endif
         endif
      enddo

      if (maxdiff.lt.eps) goto 20
      ratio = ratbuf(imax)
      sum = sum - ratio
      sum2 = sum2 - ratio**2
      ngood = ngood - 1
      nrat = nrat + 1
      flag(imax) = flag(imax) + 4
      goto 10
      
   20 call prnt(4,1,nrat,'nrat=.')
      write(msg,108) avg,eps
      call xvmessage(msg,' ')
      return

  990 do i1=1,lnpts
         if (flag(i1).eq.0) then
            flag(imax) = flag(imax) + 4
            nrat = nrat + 1
         endif
      enddo
      call prnt(4,1,nrat,'nrat=.')
      return
      end
