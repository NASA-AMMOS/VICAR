cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Test for horizontal overlaps.
c
      subroutine hcheck(q,dx,dy,mask,nl,ns,jbeg,jend,imin1,imax1,nsw)
      implicit none
      integer*4 nl,ns,jbeg,jend,imin1,imax1,nsw
      real*4 q(ns,nl)			!correlation quality
      real*4 dx(ns,nl),dy(ns,nl)	!x and y displacements
      integer*2 mask(ns,nl)

      common/ctemp/buf(1024)		!matching left pixel for each right
      integer*2 buf

      integer*4 i,j,i0,i2,m,imn1,imx1,flag

      m = nsw/2
      imn1 = imin1 + m
      imx1 = imax1 - m

      do 50 j=jbeg,jend
      do i=1,ns
         buf(i) = 0
      enddo

      do 40 i=imn1,imx1		!left pixel index
      if (mask(i,j).ne.0) goto 40
      i2 = i + dx(i,j)		!matching right pixel index
      if (buf(i2).eq.0 .and. buf(i2+1).eq.0) goto 20
      if (buf(i2).eq.0) i2=i2+1
      i0 = buf(i2)
      if (i-i0.lt.3) goto 20
      if (q(i,j).gt.q(i0,j)) then
         flag = mask(i0,j)
         if (mod(flag,32).lt.16) mask(i0,j)=flag+16
      else
         flag = mask(i0,j)
         if (mod(flag,32).lt.16) mask(i,j)=flag+16
      endif
      goto 40
   20 i2 = i + dx(i,j) + 0.5
      buf(i2) = i		!matching left pixel
   40 continue

   50 continue

      return
      end
