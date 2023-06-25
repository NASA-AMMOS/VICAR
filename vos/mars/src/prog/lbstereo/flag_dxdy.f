ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Flag candidates with large deviations in dx or dy. 
c
      subroutine flag_dxdy(lnpts,l1buf,s1buf,l2buf,s2buf,flag)
      implicit none
      integer*4 lnpts
      integer*2 l1buf(lnpts),l2buf(lnpts)
      integer*2 s1buf(lnpts),s2buf(lnpts)
      integer*2 flag(lnpts)		!0=good candidate

      integer*4 l1,l2
      integer*4 i,i0,ilo,ihi,nbad
      integer*4 dx,dy,dx0,dy0,dev
      integer*4 dxthresh,dythresh,cnt

      call xvp('dxthresh',dxthresh,cnt)
      call xvp('dythresh',dythresh,cnt)
      call prnt(4,1,dythresh,'dythresh=.')
      nbad = 0

c     ...find first and last valid tiepoints
      ilo = 0
      ihi = lnpts + 1

   10 ilo = ilo + 1
      if (ilo.gt.lnpts) call mabend('***no tiepoints',' ')
      if (flag(ilo).ne.0) goto 10

   12 ihi = ihi - 1
      if (flag(ihi).ne.0) goto 12

      do 50 i0=ilo,ihi
      if (flag(i0).ne.0) goto 50		!skip if already invalid
      l1 = l1buf(i0)
      dx0 = s2buf(i0) - s1buf(i0)
      dy0 = l2buf(i0) - l1buf(i0)
      i = i0				!search above
   20 i = i - 1
      if (i.lt.ilo) goto 25
      if (l1-l1buf(i).gt.100) goto 25
      if (flag(i).ne.0) goto 20
      dy = l2buf(i) - l1buf(i)
      dev = abs(dy-dy0)
      if (dev.gt.dythresh) goto 20
      dx = s2buf(i) - s1buf(i)
      dev = abs(dx-dx0)
      if (dev.lt.dxthresh) goto 50	!test passed
      goto 20
   25 i = i0				!search below
   30 i = i + 1
      if (i.gt.lnpts) goto 40
      if (l1buf(i)-l1.gt.100) goto 40
      if (flag(i).ne.0) goto 30
      dy = l2buf(i) - l1buf(i)
      dev = abs(dy-dy0)
      if (dev.gt.dythresh) goto 30
      dx = s2buf(i) - s1buf(i)
      dev = abs(dx-dx0)
      if (dev.lt.dxthresh) goto 50
      goto 30
   40 flag(i0) = 2
      nbad = nbad + 1
   50 continue
       
      call prnt(4,1,nbad,'bad dy=.')
      end
