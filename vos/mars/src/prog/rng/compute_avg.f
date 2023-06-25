ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Replace each pixel in an image (pic) by the average of the nlw x nsw area
c surrounding it.
c
      subroutine compute_avg(pic,avg,nl,ns,nlw,nsw)
      implicit none
      integer*4 nl,ns		!input image is nl x ns
      integer*2 pic(ns,nl)	!Input image
      real*4 avg(ns,nl)		!mean of area
      integer*4 nlw,nsw

      common/ctemp/s(1025),flag(1024)
      integer*4 s,flag

      integer*4 m,n,m1,n1
      integer*4 ibeg,iend,jbeg,jend
      integer*4 i,j,k,j0,j1,dn,dn0,dn1,isum,aflag
      real*8 area

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
         do j=jbeg-n,jbeg+n
            dn = pic(i,j)
            if (dn.le.0) flag(i)=1
            isum = isum + pic(i,j)
         enddo
         s(i) = isum
      enddo

c     ...Image line loop begins
      do 100 j=jbeg,jend
      aflag = 0
      isum = 0
      do i=ibeg-m,ibeg+m	!compute the sum for left-most area
         if (flag(i).eq.1) aflag=1
         isum = isum + s(i)
      enddo

c     ...Compute stats for the entire line via sliding-sums
      do i=ibeg,iend
         if (aflag.eq.0) then
            avg(i,j) = isum/area
         else
            avg(i,j) = 0.
         endif
         isum  = isum -  s(i-m) +  s(i+m1)
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
      call fill_margins(avg,nl,ns,ibeg,iend,jbeg,jend,0.)
      return
      end

