cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Test rotation and scale
c
      subroutine tsub(l1buf,s1buf,l2pbuf,s2pbuf,izmax,ismax,itmax,
     &		flag,lnpts,nl,ns)
      implicit none
      integer*4 lnpts,nl,ns
      integer*2 l1buf(lnpts),s1buf(lnpts)
      integer*2 l2pbuf(lnpts),s2pbuf(lnpts)
      integer*2 izmax(lnpts),ismax(lnpts),itmax(lnpts)
      integer*2 flag(lnpts)

      common/cp/debug,print
      logical*1 debug,print

      integer*4 i,iz,cnt,izoom
      real*8 lcenter(5),scenter(5)
      real*8 l0,s0
      real*4 x1,y1,x2,y2
      real*4 x1p,y1p,x2p,y2p
      real*4 deg,scale,scale2
      real*4 scales(5)/1.,2.,4.,8.,16./
      real*8 t,sint,cost,diff
      real*8 pi/3.141592654/

      integer*4 npts,nbad
      real*8 sum,sum2,avg,sig
      character*80 msg
  101 format(4i5,f10.4)

      call xvp('tscale',scale,cnt)
      scale = scale*scales(3)
      call xvp('tangle',deg,cnt)
      t = deg*pi/180.d0
      sint = dsin(t)
      cost = dcos(t)

      do iz=1,5
         izoom = 2**iz
         lcenter(iz) = nl/izoom + 0.5
         scenter(iz) = ns/izoom + 0.5
      enddo
      l0 = lcenter(3)
      s0 = scenter(3)

      if (print) then
         call xvmessage(' ',' ')
         call xvmessage('    i   iz   is   it    diff',' ')
      endif

      sum = 0.d0
      sum2 = 0.d0
      nbad = 0
      npts = 0

      do 50 i=1,lnpts
      if (flag(i).ne.0) goto 50
      x1 = s1buf(i) - s0
      y1 = l1buf(i) - l0
      x1p = scale*(cost*x1 + sint*y1)
      y1p = scale*(-sint*x1 + cost*y1)
      iz = izmax(i)
      scale2 = scales(iz)
      x2 = s2pbuf(i) - scenter(iz)
      y2 = l2pbuf(i) - lcenter(iz)
      x2p = scale2*x2
      y2p = scale2*y2
      diff = sqrt((x2p-x1p)**2+(y2p-y1p)**2)
      if (diff.gt.10.0) nbad=nbad+1
      npts = npts + 1
      sum = sum + diff
      sum2 = sum2 + diff**2
ccc      if (print) then
         write(msg,101) i,iz,ismax(i),itmax(i),diff
         call xvmessage(msg,' ')
ccc      endif
   50 continue

      avg = sum/npts
      sig = dsqrt(sum2/npts-avg**2)
      write(msg,102) avg,sig
      call xvmessage(msg,' ')
  102 format('mean diff=',f8.4,' sigma=',f8.5)
      write(msg,103) nbad,npts
      call xvmessage(msg,' ')
  103 format('number of outliers=',i3,' out of',i4)
      return
      end
