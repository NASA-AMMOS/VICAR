ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Scan each image column vertically for the horizon.
c
      subroutine find_horizon(act,nl,ns,ibeg,iend,jbeg,jend,
     &		hrzn,sthresh,hthresh,parea)
      implicit none
      integer*4 nl,ns,ibeg,iend,jbeg,jend,parea
      real*4 act(ns,nl),sthresh,hthresh
      integer*2 hrzn(ns)

      integer*4 i,j,i0,j0,jj,i1,i2
      integer*4 h,hm1,hp1,d0,d1,d2
      real*4 d,dm1,dm2,dm3,dp1,dp2,dp3

      do 20 i=ibeg,iend
      jj = jbeg
      do while(act(i,jj).eq.0. .and. jj.le.jend)
         jj = jj + 1
      enddo
      if (jj.gt.jend) goto 20
      if (act(i,jj).gt.sthresh) goto 20

      do j=jj,jend
         if (act(i,j).gt.sthresh) goto 10
      enddo
      hrzn(i) = nl
      goto 20

   10 j0 = max0(j,jj+3)

      do 12 j=j0,jend
      d = act(i,j)
      dm1 = act(i,j-1)
      dm2 = act(i,j-2)
      dm3 = act(i,j-3)
      dp1 = act(i,j+1)
      dp2 = act(i,j+2)
      dp3 = act(i,j+3)
      if (d.gt.dm1 .and. d.gt.dp1 .and.
     &    d.gt.dm2 .and. d.gt.dp2 .and.
     &    d.gt.dm3 .and. d.gt.dp3 .and.
     &    (d-(dm1+dm2+dm3+dp1+dp2+dp3)/6.0).gt.hthresh) goto 15
   12 continue
      j = nl
   15 hrzn(i) = j 
   20 continue

c     ...Clean up horizon
      do 25 i=ibeg+1,iend-1
      h = hrzn(i)
      hm1 = hrzn(i-1)
      hp1 = hrzn(i+1)
      d1 = iabs(h-hm1)
      d2 = iabs(h-hp1)
      d0 = iabs(hp1-hm1)
      if (d1.le.1 .or. d2.le.1) goto 25
      if (d1.gt.d0 .or. d2.gt.d0) hrzn(i)=(hm1+hp1)/2
   25 continue

      i = ibeg
   30 do while(hrzn(i).eq.0. .and. i.le.iend)
         i = i + 1
      enddo
      if (i.gt.iend) goto 48
      i1 = i
      do while(abs(hrzn(i)-hrzn(i+1)).lt.5. .and. i.le.iend)
         i = i + 1
      enddo
      i2 = min0(i,iend)
      if (i2-i1.le.10) then
         do i=i1,i2
            hrzn(i) = 0
         enddo
      endif
      i = i2 + 1
      if (i.le.iend) goto 30

c     ...Extend the horizon to margins
   48 i = ibeg

   50 if (hrzn(i).gt.0) goto 55
      if (i.eq.iend) return
      i = i + 1
      goto 50

   55 if (i-ibeg.gt.10) goto 58
      i0 = i
      do i=1,i0
         hrzn(i) = hrzn(i0)
      enddo

   58 i = iend

   60 if (hrzn(i).gt.0) goto 65
      if (i.eq.ibeg) return
      i = i - 1
      goto 60

   65 if (iend-i.gt.10) return
      i0 = i
      do i=i0,ns
         hrzn(i) = hrzn(i0)
      enddo
      return
      end
