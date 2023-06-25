ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compute mean and variance of left or right image.
c
      subroutine astats(pic,avg,var,nl,ns,nlw,nsw)
      implicit none
      integer*4 nl,ns		!input image is nl x ns
      integer*2 pic(ns,nl)	!Input image
      real*4 avg(ns,nl)		!mean of area
      real*4 var(ns,nl)		!variance of areas
      integer*4 nlw,nsw		!filter window is nlw x nsw

      common/ctemp/s(1025),s2(1025),flag(1024)
      integer*4 s,s2		!column sums and squared sums of DNs
      integer*2 flag		!=1 if invalid pixel in column

      integer*4 m,n,m1
      integer*4 i,j,k,j0,j1
      integer*4 ibeg,iend,jbeg,jend
      integer*4 dn,dn0,isum,isum2,aflag
      real*8 area,sum2

c     ...Set image margins to allow for size of filter window
      area = nlw*nsw
      m = nsw/2
      n = nlw/2

      ibeg = m + 1
      iend = ns - m
      jbeg = n + 1
      jend = nl - n

      m1 = m + 1

c     ...initialize the column sums
      do i=1,ns
         flag(i) = 0
         isum = 0
         isum2 = 0
         do j=jbeg-n,jbeg+n
            dn = pic(i,j)
            if (dn.le.0) flag(i)=1
            isum = isum + dn
            isum2 = isum2 + dn**2
         enddo
         s(i) = isum
         s2(i) = isum2
      enddo

c     ...Image line loop begins
      do 100 j=jbeg,jend
      aflag = 0 
      isum = 0
      sum2 = 0.
      do i=ibeg-m,ibeg+m	!compute the sums for left-most window
         if (flag(i).eq.1) aflag=1
         isum = isum + s(i)
         sum2 = sum2 + s2(i)
      enddo

c     ...Compute stats for the entire line via sliding-sums
      do i=ibeg,iend
         if (aflag.eq.0) then
            avg(i,j) = isum/area
            var(i,j) = sum2/area - avg(i,j)**2
         else
            avg(i,j) = 0.
            var(i,j) = 0.
         endif
         isum  = isum -  s(i-m) +  s(i+m1)
         sum2  = sum2 - s2(i-m) + s2(i+m1)
         if (flag(i+m1).eq.1) then
            aflag = 1
         else
            if (flag(i-m).eq.1) then
               aflag = 0
               do k=i-m+1,i+m
                  if (flag(k).eq.1) aflag=1
               enddo
            endif
         endif
      enddo

c     ...update column sums for next line
      if (j.ge.nl) goto 100	!skip if last line to process
      j0 = j - n		!Old line sliding out of window
      j1 = j + n + 1		!New line sliding into window

      do 90 i=1,ns
      dn0 = pic(i,j0)
      dn  = pic(i,j1)
      s(i)  = s(i)  - dn0 + dn
      s2(i) = s2(i) - dn0**2 + dn**2
      if (dn.le.0) then
         flag(i) = 1
      elseif (dn0.le.0) then
         flag(i) = 0
         do k=j0+1,j1-1
            if (pic(i,k).le.0) flag(i)=1
         enddo
      endif
   90 continue
  100 continue

c     ...zero out margins
      call fill_margins(avg,nl,ns,ibeg,iend,jbeg,jend,0.)
      call fill_margins(var,nl,ns,ibeg,iend,jbeg,jend,0.)
      return
      end
