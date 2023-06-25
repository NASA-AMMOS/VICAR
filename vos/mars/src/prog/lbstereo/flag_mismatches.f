ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine flag_mismatches(rho,lvarbuf,rvarbuf,isbuf,itbuf,
     &		lnpts,rnpts,qthresh,flag)
      implicit none
c     ...inputs
      integer*4 lnpts,rnpts		!number of left and right candidates
      real*4 rho(lnpts,rnpts)		!rho(i1,i2) between areas i1 and i2
      real*4 lvarbuf(lnpts)		!sig1 = sqrt(lvarbuf(i1))
      real*4 rvarbuf(lnpts)		!sig2 = sqrt(rvarbuf(i1))
      integer*2 isbuf(lnpts)
      integer*2 itbuf(lnpts)
      real*4 qthresh			!correlation threshold
c     ...outputs
      integer*2 flag(lnpts)             !flag(i1)=0 if tiepoint is valid

      common/cp/debug,print
      logical*1 debug,print

      real*4 ratbuf(256)		!ratbuf(i1) = sig1/sig2

      integer*4 i1,l1,s1		!l1=lcoords(1,i1)   s1=lcoords(2,i1)
      integer*4 i2,l2,s2		!l2=rcoords(1,i2)   s2=rcoords(2,i2)
      real*4 sig1,sig2,rmax

      integer*4 l1p,s1p
      real*4 sig1p,ratiop

      integer*4 i,i1max,cnt
      integer*4 nbad,nmiss,nrat,nodata,ngood
      integer*4 n,n1,n2,n3,n4,n5
      real*4 rr,f
      real*8 ratlo,rathi
      real*8 ratio,sum,sum2,avg,sig,eps		!sig1/sig2 test
      character*132 msg
  103 format(i5,i5,2f10.0,f10.2,f10.6,' ***lo corr')
  104 format(i5,i5,2f10.0,f10.2,f10.6,' ***mismatch')
  106 format(i5,i5,2f10.0,f10.2,f10.6)
  110 format(5i5)

      ngood = 0
      nbad = 0
      nmiss = 0

      do 60 i1=1,lnpts
      flag(i1) = 0
      sig1 = sqrt(lvarbuf(i1))
      sig2 = sqrt(rvarbuf(i1))
      ratio = 0.
      if (sig2.gt.1.) ratio=sig1/sig2
      ratbuf(i1) = ratio

      rr = -999.
      do i=1,rnpts			!find best match for area i1
         if (rho(i1,i).gt.rr) then
            rr = rho(i1,i)
            i2 = i
         endif
      enddo

      if (rr.lt.qthresh) then		!check correlation coefficient
         nbad = nbad + 1
         flag(i1) = 1
         if (debug) then
            write(msg,103) i1,i2,sig1,sig2,ratio,sqrt(rr)
            call xvmessage(msg,' ')
         endif
      endif

      i1max = i1			!find best match for area i2
      rmax = rr
      do i=1,lnpts
         if (rho(i,i2).gt.rmax) then
            i1max = i
            rmax = rho(i,i2)
         endif
      enddo

      if (i1max.ne.i1) then		!consistency check
         nmiss = nmiss + 1
         flag(i1) = flag(i1) + 2
         if (debug .and. flag(i1).eq.2) then
            call xvmessage(' ',' ')
            write(msg,104) i1,i2,sig1,sig2,ratio,sqrt(rr)
            call xvmessage(msg,' ')
            sig1p = sqrt(lvarbuf(i1max))
            ratiop = 0.
            if (sig2.gt.1.) ratiop=sig1p/sig2
            write(msg,106) i1max,i2,sig1p,sig2,ratiop,sqrt(rmax)
            call xvmessage(msg,' ')
            call xvmessage(' ',' ')
         endif
      endif

      if (flag(i1).eq.0) then
         ngood = ngood + 1
         if (print) then
            write(msg,106) i1,i2,sig1,sig2,ratio,sqrt(rr)
            call xvmessage(msg,' ')
         endif
      endif
   60 continue

      call prnt(4,1,nbad,'nbad=.')
      call prnt(4,1,nmiss,'nmiss=.')

      call flag_ratios(ratbuf,flag,lnpts)

      if (debug) then
         call xvmessage(' ',' ') 
         call xvmessage('  lpts   q  miss s1/s2',' ')
      endif

      nbad = 0
      do 70 i1=1,lnpts
      n = flag(i1)
      if (n.eq.0) goto 70
      nbad = nbad + 1
      if (.not.debug) goto 70
      n1 = mod(n,2)
      n = n - n1
      n2 = mod(n,4) 
      n3 = n - n2
      write(msg,110) i1,n1,n2,n3
      call xvmessage(msg,' ')
   70 continue

      call prnt(4,1,lnpts-nbad,'ngood=.')
      return
      end
