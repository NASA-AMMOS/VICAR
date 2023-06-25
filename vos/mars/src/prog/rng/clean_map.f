cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Clean patches from disparity map.
c
      subroutine clean_map(dx,dy,mask,nl,ns,jbeg,jend)
      implicit none
      integer*4 nl,ns,jbeg,jend
      real*4 dx(ns,nl),dy(ns,nl)
      integer*2 mask(ns,nl)

      common/ctemp/vert(100000),vx(100000),vy(100000)
      integer*4 vert
      integer*2 vx,vy

      integer*4 nmax/100000/
      integer*4 i,j,k,i0,j0,i1,i2,npts,npatches,area,minarea,cnt
      real*4 d,invalid
      character*80 msg
  101 format('# of patches deleted from range map=',i4)

      call xvp('MINAREA',minarea,cnt)
      invalid = -999.
      npatches = 0

      do 30 j0=jbeg,jend
      do 20 i0=2,ns-1
      if (dx(i0,j0).le.invalid) goto 20
      call spider_inv(dx,nl,ns,vx,vy,nmax,i0,j0,invalid,npts)
      call sort_vertices(vx,vy,vert,npts)
      area = 0			!compute # of pixels in patch
      do k=1,npts,2
         area = area + vx(k+1) - vx(k) + 1
      enddo
      if (area.lt.minarea) npatches=npatches+1

      do 10 k=1,npts,2
      i1 = vx(k)
      i2 = vx(k+1)
      j = vy(k)
      do i=i1,i2		!delete the patch
         dx(i,j) = dx(i,j) - 100000.	!temporarily delete valid region
         if (area.lt.minarea) mask(i,j)=mask(i,j) + 64
      enddo
   10 continue

   20 continue
   30 continue

      write(msg,101) npatches
      call xvmessage(msg,' ')

c     ...Undo cludge
      do j=jbeg,jend
         do i=2,ns-1
            d = dx(i,j)
            if (d.lt.invalid) dx(i,j)=d+100000.
         enddo
      enddo
      return
      end
