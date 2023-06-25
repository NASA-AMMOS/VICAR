cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Clean patches of above-threshold pixels from act.
c
      subroutine clean_patches(act,nl,ns,ibeg,iend,jbeg,jend,
     &		sthresh,parea)
      implicit none
      integer*4 nl,ns,ibeg,iend,jbeg,jend,parea
      real*4 act(ns,nl),sthresh

      common/ctemp/vert(100000),vx(100000),vy(100000)
      integer*4 vert
      integer*2 vx,vy

      common/cb/print
      integer*4 print

      integer*4 nmax/100000/
      integer*4 nlow
      integer*4 i,j,k,i0,j0,i1,i2,npts,area
      real*4 d,big,bigger,thresh
      character*80 msg
  101 format('# of low-activity areas=',i4)
  102 format('# of patches deleted from margins=',i4)

      big = 100000.
      bigger = 2.*big
      call spider_cage(act,nl,ns,ibeg,iend,jbeg,jend,bigger)
      do j0=jbeg,jend
         do i0=ibeg,iend
            if (act(i0,j0).eq.0.) act(i0,j0)=bigger
         enddo
      enddo
      nlow = 0			!count of regions below threshold

      dO 30 j0=jbeg,jend
      do 20 i0=ibeg,iend
      d = act(i0,j0)
      if (d.gt.sthresh) goto 20		!skip over hi activity areas
      nlow = nlow +1
      call spider(act,nl,ns,vx,vy,nmax,i0,j0,sthresh,npts)
      call sort_vertices(vx,vy,vert,npts)

      do 10 k=1,npts,2
      i1 = vx(k)
      i2 = vx(k+1)
      j = vy(k)

      do i=i1,i2		!scan line segment for blobs above sthresh
         d = act(i,j)
         if (d.gt.sthresh) then
            act(i,j) = big + sthresh
         else
            act(i,j) = big + d 	!cludge to temporarily delete low area
         endif
      enddo
   10 continue

   20 continue
   30 continue

      if (print.eq.2) then
         write(msg,101) nlow
         call xvmessage(msg,' ')
      endif
      nlow = 0

      dO 50 j0=jbeg,jend
      do 40 i0=ibeg,iend
      d = act(i0,j0)
      if (d.ge.big) goto 40		!skip over deleted areas
      thresh = big - 1.
      call spider(act,nl,ns,vx,vy,nmax,i0,j0,thresh,npts)
      call sort_vertices(vx,vy,vert,npts)

      area = 0			!compute # of pixels in blob
      do k=1,npts,2
         area = area + vx(k+1) - vx(k) + 1
      enddo
      if (area.gt.parea) goto 39
      nlow = nlow +1		!remove the blob
      do k=1,npts,2
         i1 = vx(k)
         i2 = vx(k+1)
         j = vy(k)
         do i=i1,i2
            act(i,j) = big + sthresh
         enddo
      enddo
      goto 40

   39 continue
      do 35 k=1,npts,2
      i1 = vx(k)
      i2 = vx(k+1)
      j = vy(k)

      do i=i1,i2
         act(i,j) = act(i,j) + bigger
      enddo
   35 continue

   40 continue
   50 continue

      if (print.eq.2) then
         write(msg,102) nlow
         call xvmessage(msg,' ')
      endif

      do j=jbeg,jend		!undo the cludge
         do i=ibeg,iend
            if (act(i,j).ge.bigger) act(i,j)=act(i,j)-bigger
            if (act(i,j).ge.big) act(i,j)=act(i,j)-big
         enddo
      enddo
      return
      end
