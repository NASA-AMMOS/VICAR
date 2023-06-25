cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compute pseudo-left image from right image.
c
      subroutine compute_pl(r,dx,dy,pl,nl,ns,jbeg,jend)
      implicit none
      integer*4 nl,ns,jbeg,jend
      integer*2 r(ns,nl)		!right image
      real*4 dx(ns,nl),dy(ns,nl)	!x and y displacements
      integer*2 pl(ns,nl)		!output pseudo-left

      real*8 wr,wl,wb,top,bot
      real*4 line,samp
      integer*4 il,is,i,j
ccc      logical xvptst

      call fill_buf2(pl,nl*ns,0)
ccc      if (xvptst('SIM')) goto 60

      do 50 j=jbeg,jend
      do 40 i=1,ns
      pl(i,j) = 1
      if (dx(i,j).eq.-999.) goto 40
      line = j + dy(i,j)
      samp = i + dx(i,j)
      il = line
      is = samp
      wr = samp - is
      wl = 1.d0 - wr
      wb = line - il
      top = wl*r(is,il) + wr*r(is+1,il)
      bot = wl*r(is,il+1) + wr*r(is+1,il+1)
      pl(i,j) = bot*wb + top*(1.0d0-wb)
   40 continue
   50 continue
      return

   60 continue
      call xvmessage('simulating left image',' ')

      do 90 j=jbeg,jend
      do 80 i=1,ns
      pl(i,j) = 1
      if (dx(i,j).eq.-999.) goto 80
      il = dy(i,j) + 0.5
      is = dx(i,j) + 0.5
      pl(i,j) = r(is+i,il+j)
   80 continue
   90 continue
      return
      end
