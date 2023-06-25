ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fill range data along left and right margins.
c
      subroutine fill_map1(nl,ns,l,r,q,dx,dy,mask,avgl,avgr,varl,varr,
     &		hrznl,hrznr,rho)
      implicit none
      integer*4 nl,ns			!image size is nl x ns
      integer*2 l(ns,nl),r(ns,nl)	!left and right images
      real*4 q(ns,nl)			!corr quality (rho**2)
      real*4 dx(ns,nl),dy(ns,nl)	!hori & vert displacements
      integer*2 mask(ns,nl)		!valid range mask
      real*4 avgl(ns,nl),avgr(ns,nl)  	!average of areas
      real*4 varl(ns,nl),varr(ns,nl)	!variance of areas
      integer*2 hrznl(ns),hrznr(ns)	!horizon location
      real*4 rho(1)			!correlation coefficients

      common/cq/nlw,nsw,minvar,qthresh,gthresh
      integer*4 nlw,nsw                 !correlation window is nlw x nsw
      real*4 minvar			!min variance in correlation area
      real*4 qthresh,gthresh		!correlation thresholds

      common/cb/print
      integer*4 print

      integer*4 i,j,m,n,npix,nmarg
      integer*4 ibeg,iend,jbeg,jend
      integer*4 i1,j1,h,k,flag,flag0
      real*4 avg,var,dx1,dy1
      real*8 rmax

      npix = 0
      nmarg = 0

c     ...compute the mean and variance images over an nlw x nsw window
      m = nsw/2
      n = nlw/2
      ibeg = 1 + m
      jbeg = 1 + n
      iend = ns - m
      jend = nl - n

      do 40 j1=jbeg,jend
      i = ibeg			!scan for left edge of range data
      do while (mask(i,j1).ne.0 .and. i.le.iend)
         i = i + 1
      enddo
      if (i.gt.iend) goto 40	!no valid range data on this line
      flag = 0

      do 20 i1=i-1,ibeg,-1
      avg = avgl(i1,j1)
      var = varl(i1,j1)
      if (hrznl(i1).gt.j1) goto 25
      if (var.lt.minvar) goto 25
      flag0 = flag
      if (flag.eq.0) then
         h = aint(dx(i1+1,j1))       !the estimated matching right pixels
         k = aint(dy(i1+1,j1))       !is at (i2,j2)=(i+h,j+k)
      else
         if (mask(i1,j1-1).ne.0) goto 25
         h = aint(dx(i1,j1-1))
         k = aint(dy(i1,j1-1))
      endif
      call get_match(nl,ns,l,r,avg,var,avgr,varr,hrznr,
     &		rho,i1,j1,h,k,dx1,dy1,rmax,gthresh,flag)
      q(i1,j1) = rmax
      dx(i1,j1) = dx1
      dy(i1,j1) = dy1
      if (flag0.eq.0 .and. dx1-dx(i1+1,j1).gt.0.5) flag=5
      mask(i1,j1) = flag
      if (flag.eq.0) npix=npix+1
      if (flag.eq.2) nmarg=nmarg+1
   20 continue

   25 i = iend			!scan for right edge of range data
      do while (mask(i,j1).ne.0 .and. i.ge.ibeg)
         i = i - 1
      enddo
      if (i.lt.ibeg) goto 40	!no valid range data on this line
      flag = 0

      do 30 i1=i+1,iend
      avg = avgl(i1,j1)
      var = varl(i1,j1)
      if (hrznl(i1).gt.j1) goto 40
      if (var.lt.minvar) goto 40
      if (flag.eq.0) then
         h = aint(dx(i1-1,j1))       !the estimated matching right pixels
         k = aint(dy(i1-1,j1))       !is at (i2,j2)=(i+h,j+k)
      else
         if (mask(i1,j1-1).ne.0) goto 40
         h = aint(dx(i1,j1-1))
         k = aint(dy(i1,j1-1))
      endif
      call get_match(nl,ns,l,r,avg,var,avgr,varr,hrznr,
     &		rho,i1,j1,h,k,dx1,dy1,rmax,gthresh,flag)
      q(i1,j1) = rmax
      dx(i1,j1) = dx1
      dy(i1,j1) = dy1
      mask(i1,j1) = flag
      if (flag.eq.0) npix=npix+1
      if (flag.eq.2) nmarg=nmarg+1
   30 continue
   40 continue

      do 80 i1=ibeg,iend
      j = jbeg			!scan for top edge of range data
      do while (mask(i1,j).ne.0 .and. j.le.jend)
         j = j + 1
      enddo
      if (j.gt.jend) goto 80	!no valid range data on this column
      flag = 0

      do 60 j1=j-1,jbeg,-1
      avg = avgl(i1,j1)
      var = varl(i1,j1)
      if (hrznl(i1).gt.j1) goto 65
      if (var.lt.minvar) goto 65
      if (flag.eq.0) then
         h = aint(dx(i1,j1+1))       !the estimated matching right pixels
         k = aint(dy(i1,j1+1))       !is at (i2,j2)=(i+h,j+k)
      else
         if (mask(i1-1,j1).ne.0) goto 65
         h = aint(dx(i1-1,j1))
         k = aint(dy(i1-1,j1))
      endif
      call get_match(nl,ns,l,r,avg,var,avgr,varr,hrznr,
     &		rho,i1,j1,h,k,dx1,dy1,rmax,gthresh,flag)
      q(i1,j1) = rmax
      dx(i1,j1) = dx1
      dy(i1,j1) = dy1
      mask(i1,j1) = flag
      if (flag.eq.0) npix=npix+1
      if (flag.eq.2) nmarg=nmarg+1
   60 continue

   65 j = jend			!scan for bottom edge of range data
      do while (mask(i1,j).ne.0 .and. j.ge.jbeg)
         j = j - 1
      enddo
      if (j.lt.jbeg) goto 80	!no valid range data on this column
      flag = 0

      do 70 j1=j+1,jend
      avg = avgl(i1,j1)
      var = varl(i1,j1)
      if (hrznl(i1).gt.j1) goto 80
      if (var.lt.minvar) goto 80
      if (flag.eq.0) then
         h = aint(dx(i1,j1-1))       !the estimated matching right pixels
         k = aint(dy(i1,j1-1))       !is at (i2,j2)=(i+h,j+k)
      else
         if (mask(i1-1,j1).ne.0) goto 80
         h = aint(dx(i1-1,j1))
         k = aint(dy(i1-1,j1))
      endif
      call get_match(nl,ns,l,r,avg,var,avgr,varr,hrznr,
     &		rho,i1,j1,h,k,dx1,dy1,rmax,gthresh,flag)
      q(i1,j1) = rmax
      dx(i1,j1) = dx1
      dy(i1,j1) = dy1
      mask(i1,j1) = flag
      if (flag.eq.0) npix=npix+1
      if (flag.eq.2) nmarg=nmarg+1
   70 continue
   80 continue

      if (print.eq.2) then
         call prnt(4,1,npix,'number of pixels filled by fill_map1=.')
         call prnt(4,1,nmarg,'number of rejected tpts on margin=.')
      endif
      return
      end
