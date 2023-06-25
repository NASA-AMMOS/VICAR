ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Divide the disparity map into regions of continuous surfaces.  The principal
c output is a region image r, where all pixels belonging to the same surface
c have the same DN value.
c
c Two adjacent pixels belong to the same surface if their horizontal and
c vertical displacements (dx and dy) are relatively the same. We compare
c the central pixel (i,j) against the pixels to the left (i-1,j) and
c above (i,j-1).
c
c |dx(i,j)-dx(i-1,j)| < tdx & |dy(i,j)-dy(i-1,j)| < tdy ---> left surface
c |dx(i,j)-dx(i,j-1)| < tdx & |dy(i,j)-dy(i,j-1)} < tdy ---> surface above
c
c Ties are resolved by merging the regions.
c
c Regions are numbered 0,1,2,...,nr.  When merges occur, say region 15 merges
c into region 3, we keep track of the merge by assigning a region index rx
c to each region number:  rx(1),rx(2),...,rx(nr)  Initially, rx(i)=i.  This
c is its base value.  When region j merges into region i, then rx(j)=rx(i).
c It may be that rx(i) is equal to i, or that rx(i) points to another
c region k.  If we follow the trail, we eventualy come to a base region,
c where rx(m) is equal to m.  We will eventually want to consolidate, by
c pointing every index along the track to base value m.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine regions(dx,dy,mask,nl,ns,maxnr,nr,r,rsize,rx)
      implicit none
      integer*4 nl,ns,maxnr,nr
      real*4 dx(ns,nl),dy(ns,nl)	!horizontal and vertical displacements
      integer*2 mask(ns,nl)		!bad-pixel mask
      integer*4 r(ns,nl)		!region surface (returned)
      integer*4 rsize(maxnr)		!number of pixels in region
      integer*4 rx(maxnr)		!region index

      integer*4 i,j
      integer*4 ix,ix2,bx,bx2,jx,itemp

      common/cg/tdx,tdy
      real*4 tdx,tdy                    !surface discontinuity thresholds

      common/cb/print
      integer*4 print

c
c Since the algorithm requires that the regions of the adjacent pixels to
c the left and below are already known, we initialize these regions to the
c invalid region 0.
c
      do i=1,ns
         r(i,1) = 0	!pixels on first line assumed invalid
      enddo
      do j=1,nl
         r(1,j) = 0	!pixels on first column assumed invalid
      enddo
c
c We process the pixels from left-to-right and from top-to-bottom.
c We begin with pixel x(2,2).  The pixels to the left and above have
c already been assigned to the invalid region 0.  The following algorithm
c assigns a region r(i,j) to each input pixel (i,j).
c
      nr = 0			!nr = highest region number assigned so far

      do 70 j=2,nl
      do 60 i=2,ns
      ix = 0			!initialize region index as invalid
      if (mask(i,j).gt.1) goto 60	!done if pixel value is invalid
      if (mask(i-1,j).le.1 .and. abs(dx(i-1,j)-dx(i,j)).lt.tdx
     &                     .and. abs(dy(i-1,j)-dy(i,j)).lt.tdy)  goto 20
      if (mask(i,j-1).le.1 .and. abs(dx(i,j-1)-dx(i,j)).lt.tdx
     &			   .and. abs(dy(i,j-1)-dy(i,j)).lt.tdy) goto 10
c     ...Throw out single-pixel spikes
      if ( (abs(dx(i+1,j)-dx(i,j)).gt.tdx .or.
     &      abs(dy(i+1,j)-dy(i,j)).gt.tdy) .and.
     &     (abs(dx(i,j+1)-dx(i,j)).gt.tdx .or.
     &      abs(dy(i,j+1)-dy(i,j)).gt.tdy) ) then
         mask(i,j) = 6
         goto 60
      endif
c     ...new region starts here
      nr = nr + 1		!add a new region
      ix = nr			!and set region index to it
      rx(ix) = ix		!this is a new base region
      rsize(ix) = 1 		!region size is initially 1 pixel
      goto 60

   10 ix = r(i,j-1)	!region continues from above
      rsize(ix) = rsize(ix) + 1
      goto 60

   20 ix = r(i-1,j)	!region continues from left
      rsize(ix) = rsize(ix) + 1

c     ...check for ties
      if (mask(i,j-1).gt.1) goto 60
      if ((abs(dx(i,j-1)-dx(i,j)).gt.tdx) .or.
     &    (abs(dy(i,j-1)-dy(i,j)).gt.tdy)) goto 60
      ix2 = r(i,j-1)		!oops, there is a tie

      bx = ix			!find base index for region ix
      do while (rx(bx).ne.bx)
         bx = rx(bx)
      enddo

      bx2 = ix2			!find base index for region ix2
      do while (rx(bx2).ne.bx2)
         bx2 = rx(bx2)
      enddo

      if (bx.eq.bx2) goto 60	!skip merge if same region
      if (bx.lt.bx2) then
         rx(bx2) = ix		!merging bx2 regions into bx
         jx = ix2		!top end of bx2 trail is ix2
         do while (rx(jx).ne.bx)
            itemp = jx
            jx = rx(jx)
            rx(itemp) = bx
         enddo
      else
         rx(bx) = ix2		!merging bx regions into bx2
         jx = ix		!top end of bx trail is ix
         do while (rx(jx).ne.bx2) 
            itemp = jx
            jx = rx(jx)
            rx(itemp) = bx2
         enddo
      endif

   60 r(i,j) = ix		!assign region number
   70 continue

c     ...shorten all the links
      do 80 ix=1,nr
      if (rx(ix).eq.ix) goto 80	!skip if already a base region
      bx = rx(ix) 		!find base region for ix
      do while (rx(bx).ne.bx)
         bx = rx(bx)
      enddo
      jx = ix			!shorten the links
      do while (rx(jx).ne.bx)
         itemp = jx
         jx = rx(jx)
         rx(itemp) = bx
      enddo
   80 continue

      do ix=1,nr		!sum size of all base regions
         if (rx(ix).ne.ix) then
            bx = rx(ix)
            rsize(bx) = rsize(bx) + rsize(ix)
         endif
      enddo

      do ix=1,nr		!update size of all other regions
         if (rx(ix).ne.ix) then
            bx = rx(ix)
            rsize(ix) = rsize(bx)
         endif
      enddo

      if (print.eq.2) call prnt(4,1,nr,'number of regions=.')
      return
      end
