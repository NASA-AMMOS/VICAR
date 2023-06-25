ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Replace each pixel in an image (pic) by the sigma of the nlw x nsw area
c surrounding it.
c
      subroutine compute_sig(pic,sig,nl,ns,nlw,nsw)
      implicit none
      integer*4 nl,ns		!input image is nl x ns
      real*4 pic(ns,nl)		!Input image
      real*4 sig(ns,nl)		!standard deviation of area
      integer*4 nlw,nsw

      common/ctemp/s2(1025),s(1025),flag(1024)
      integer*4 s,flag
      real*8 s2

      integer*4 m,n,m1,n1
      integer*4 ibeg,iend,jbeg,jend
      integer*4 i,j,k,j0,j1,dn,dn0,dn1,isum,aflag
      real*8 area,sum2,avg

c     ...Set image margins to allow for size of filter window
      area = nlw*nsw
      m = nsw/2
      n = nlw/2
      m1 = m + 1
      n1 = n + 1
      ibeg = 1 + m
      iend = ns - m
      jbeg = 1 + n
      jend = nl - n

c     ...initialize the column sums for the first nlw lines
      do i=1,ns
         flag(i) = 0
         isum = 0
         sum2 = 0.d0
         do j=jbeg-n,jbeg+n
            dn = pic(i,j)
            if (dn.le.0) flag(i)=1
            isum = isum + dn
            sum2 = sum2 + dn*dn
         enddo
         s(i) = isum
         s2(i) = sum2
      enddo

c     ...Image line loop begins
      do 100 j=jbeg,jend
      aflag = 0
      isum = 0
      sum2 = 0.d0
      do i=ibeg-m,ibeg+m	!compute the sum for left-most area
         if (flag(i).eq.1) aflag=1
         isum = isum + s(i)
         sum2 = sum2 + s2(i)
      enddo

c     ...Compute stats for the entire line via sliding-sums
      do i=ibeg,iend
         if (aflag.eq.0) then
            avg = isum/area
            sig(i,j) = sum2/area - avg*avg
         else
            sig(i,j) = 0.
         endif
         isum = isum -  s(i-m) +  s(i+m1)
         sum2 = sum2 - s2(i-m) + s2(i+m1)
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

c     ....update column sums for next line
      if (j.ge.nl) goto 100	!skip if last line to process
      j0 = j - n
      j1 = j + n1
      do 90 i=1,ns
      dn0 = pic(i,j0)
      dn1 = pic(i,j1)
      s(i)  = s(i)  - dn0 + dn1
      s2(i) = s2(i) - dn0*dn0 + dn1*dn1
      if (dn1.le.0) then
         flag(i) = 1
      elseif (dn0.le.0) then
         flag(i) = 0
         do k=j0+1,j1-1
            if (pic(i,k).le.0) flag(i)=1
         enddo
      endif
   90 continue
  100 continue

c     ....zero out the image margins
      call fill_margins(sig,nl,ns,ibeg,iend,jbeg,jend,0.)
      return
      end

