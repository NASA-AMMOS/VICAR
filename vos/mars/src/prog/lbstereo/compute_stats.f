ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compute average and variance of the nlw x nsw area surrounding each pixel
c of an image.
c
      subroutine compute_stats(pic,avg,var,nl,ns,nlw,nsw)
      implicit none
      integer*4 nl,ns			!input image is nl x ns
      integer*2 pic(ns,nl)		!left or right image
      real*4 avg(ns,nl),var(ns,nl)	!average and variance
      integer*4 nlw,nsw

      common/ctemp/cs2,cs,flag
      real*8 cs2(1024)
      integer*4 cs(1024)
      integer*2 flag(1024)

      integer*4 nlwh,nswh

      integer*4 ibeg,iend		!left and right margins
      integer*4 jbeg,jend		!top and bottom margins

      integer*4 dn,dn0,dn1

      integer*4 i,j,k,j0,j1,aflag

      integer*4 sum
      real*8 narea,sum2

      narea = nlw*nsw		!number of pixels in nlw x nsw window

c     ...compute margins to ensure window remains within image
      nswh = nsw/2
      nlwh = nlw/2
      ibeg = nswh + 1
      jbeg = nlwh + 1
      iend = ns - nswh
      jend = nl - nlwh

c Compute avg and sig vi sliding-sum algorithm
c
c An area is made up of nsw columns of height nlw
c We first compute the sums for each column and then add the columns to
c compute the sums for the area.
c
c A column is invalid if at least one pixel in the column is zero.
c An area is invalid if at least one column is invalid
c

c     ...compute column sums for the top of the image
      do i=1,ns
         flag(i) = 0
         sum = 0
         sum2 = 0.d0
         do j=1,nlw
            dn = pic(i,j)
            sum = sum + dn
            sum2 = sum2 + dn*dn
            if (dn.le.0) flag(i)=1	!invalid column
         enddo
         cs(i) = sum
         cs2(i) = sum2
      enddo

c     ...Image line loop begins
      do 100 j=jbeg,jend
      aflag = 0				!area is initially valid
      sum = 0
      sum2 = 0.d0
      do i=1,nsw			!compute the sum for left-most area
         sum = sum + cs(i)
         sum2 = sum2 + cs2(i)
         if (flag(i).eq.1) aflag=1	!invalid area
      enddo

c     ...Compute stats for each pixel on line j (excluding margins)
      do i=ibeg,iend
         if (aflag.eq.0) then
            avg(i,j) = sum/narea
            var(i,j) = sum2/narea - avg(i,j)*avg(i,j)
         else
            avg(i,j) = 0.
            var(i,j) = 0.
         endif
         sum = sum -  cs(i-nswh) +  cs(i+nswh+1)
         sum2 = sum2 - cs2(i-nswh) + cs2(i+nswh+1)
         if (flag(i+nswh+1).eq.1) then
            aflag = 1
         else
            if (flag(i-nswh).eq.1) then
               aflag = 0
               do k=i-nswh+1,i+nswh
                  if (flag(k).eq.1) aflag=1
               enddo
            endif
         endif
      enddo

c     ....update column sums for next line
      if (j.ge.nl) goto 100	!skip if last line to process
      j0 = j - nlwh		!line sliding out of areas
      j1 = j + nlwh + 1		!line sliding into areas
      do 90 i=1,ns
      dn0 = pic(i,j0)
      dn1 = pic(i,j1)
      cs(i)  = cs(i)  - dn0 + dn1
      cs2(i) = cs2(i) - dn0*dn0 + dn1*dn1
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
      call fill_margins(var,nl,ns,ibeg,iend,jbeg,jend,0.)
      return
      end

