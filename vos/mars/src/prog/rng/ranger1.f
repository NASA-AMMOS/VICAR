ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine ranger1(nl,ns,l,r,dx0,dy0,hrznl,hrznr,
     &		q,dx,dy,mask,avgl,avgr,varl,varr,rho)
      implicit none
c     ...Input arguments
      integer*4 nl,ns			!image size is nl x ns
      integer*2 l(ns,nl),r(ns,nl)	!left and right images
      real*4 dx0(ns/2,nl/2),dy0(ns/2,nl/2)
      integer*2 hrznl(ns),hrznr(ns)	!horizon location
c     ...Outputs
      real*4 q(ns,nl)			!corr quality (rho**2)
      real*4 dx(ns,nl),dy(ns,nl)	!hori & vert displacements
      integer*2 mask(ns,nl)		!valid range mask
c     ...Work buffers
      real*4 avgl(ns,nl),avgr(ns,nl)  	!average of areas
      real*4 varl(ns,nl),varr(ns,nl)	!variance of areas
      real*4 rho(1)			!correlation coefficients

      common/cq/nlw,nsw,minvar
      integer*4 nlw,nsw                 !correlation window is nlw x nsw
      real*4 minvar			!min variance in correlation area

      common/cb/print
      integer*4 print

      integer*4 i,j,m,n,nmarg
      integer*4 i0,j0,i1,j1		!down-sampled and full-res coordinates
      integer*4 ibeg0,iend0,jbeg0,jend0	!down-sampled begin and end coords
      integer*4 ibeg,jbeg		!full-res beginning coordinates
      integer*4 hlo,hhi,klo,khi		!search rectangle
      integer*4 h,k,hmax,kmax,heps,keps,cnt
      real*4 avg,var
      real*8 rmax
      character*80 msg
  102 format('number of range points on margin=',i6)

c     ...initialize the buffers to an invalid value
      call fill_buf(q,nl*ns,-1.0)
      call fill_buf(dx,nl*ns,-999.)
      call fill_buf(dy,nl*ns,-999.)
      call fill_buf2(mask,nl*ns,1)

c     ...compute the mean and variance images over an nlw x nsw window
      call astats(l,avgl,varl,nl,ns,nlw,nsw)
      call astats(r,avgr,varr,nl,ns,nlw,nsw)

      call xvp('hradius',heps,cnt)
      call xvp('vradius',keps,cnt)

      m = nsw/2
      n = nlw/2
      ibeg0 = 1 + m		!down-sampled begin=(ibeg0,jbeg0)
      jbeg0 = 1 + n
      iend0 = ns/2 - m		!down-sampled end=(iend0,jend0)
      jend0 = nl/2 - n
      ibeg = 2*ibeg0 - 1	!full-res begin=(ibeg,jbeg)
      jbeg = 2*jbeg0 - 1		
      j1 = jbeg
      nmarg = 0

      do 50 j0=jbeg0,jend0
      if (mod(j0,50).eq.0 .and. print.gt.0) call prnt(4,1,j1+1,'line=.')
      i1 = ibeg
      do 40 i0=ibeg0,iend0
      if (dx0(i0,j0).eq.-999.) goto 40
      h = aint(dx0(i0,j0)*2.)	!the estimated matching right pixels is at
      k = aint(dy0(i0,j0)*2.)	!(i2,j2)=(i1+h,j1+k)
      hlo = h - heps 		!compute search rectangle
      hhi = h + heps
      klo = k - keps
      khi = k + keps
      do 30 j=j1,j1+1		!compute dx,dy for 2x2 area of left image
      do 20 i=i1,i1+1
      avg = avgl(i,j)
      var = varl(i,j)
      if (hrznl(i).gt.j) goto 20
      if (var.lt.minvar) goto 20
      call fill_buf(rho,(hhi-hlo+1)*(khi-klo+1),-999.)

    5 call correlate1(nl,ns,l,r,avg,var,avgr,varr,hrznr,
     &      hlo,hhi,klo,khi,rho,i,j,h,k,hmax,kmax,rmax)
      if (rmax.eq.-1.0) goto 20
      if (hmax.eq.h .and. kmax.eq.k) then
         q(i,j) = rmax
         call interp_max(rho,hlo,hhi,klo,khi,h,k,dx(i,j),dy(i,j))
         mask(i,j) = 0
      elseif (kmax.le.klo .or. kmax.ge.khi .or.
     &        hmax.le.hlo .or. hmax.ge.hhi) then
         q(i,j) = rmax
         dx(i,j) = hmax
         dy(i,j) = kmax
         mask(i,j) = 2
         nmarg = nmarg + 1
      else 
         h = hmax
         k = kmax
         goto 5
      endif
   20 continue
   30 continue
   40 i1 = i1 + 2
   50 j1 = j1 + 2

      if (print.eq.2) then
         write(msg,102) nmarg
         call xvmessage(msg,' ')
      endif
      return
      end
