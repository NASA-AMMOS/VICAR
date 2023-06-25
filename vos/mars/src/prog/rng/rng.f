c VICAR program rng
c   rng inp=(l,r) out=(dx,dy)
c
      include 'VICMAIN_FOR'
      subroutine main44
      implicit none
      common/c0/iunit(2)		!INP=(in1,in2)
      integer*4 iunit			!logical unit number

      common/cq/nlw,nsw,minvar,qthresh,gthresh
      integer*4 nlw,nsw			!correlation window is nlw x nsw
      real*4 minvar			!min variance in correlation area
      real*4 qthresh			!correlation threshold
      real*4 gthresh			!gore thresholds

      common/cg/tdx,tdy
      real*4 tdx,tdy			!surface discontinuity thresholds

      common/cb/print
      integer*4 print

      integer*4 nl,ns			!input images are nl x ns
      integer*4 ilo,ihi,jlo,jhi		!low and high search limits
      integer*4 m,n,m1,m2,m3
      integer*4 n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15
      external rmain

      call xvmessage('rng version Sept 5 2007',' ')
      call open_inp(iunit,nl,ns)	!inp=(left,right)
      call get_params(nlw,nsw,minvar,qthresh,gthresh,tdx,tdy,print)

c     ...buffers for computing the correlation matrix
      call search_limits(ilo,ihi,jlo,jhi)	!get search limits
      m = ihi - ilo + 1	!horizontal search width
      n = jhi - jlo + 1	!vertical search height
      m1 = 8*m*n	!real*8 slr(ilo:ihi,jlo:jhi)  cross terms (search area)
      m2 = 4*m*n*ns	!integer*4 lr(ilo:ihi,jlo:jhi,ns)  cross terms (column)
      m3 = 4*m*n*ns	!real*4 r(ilo:ihi,jlo:jhi,ns)	  correlation matrix

c     ...image buffers
      n1 = 4*ns*nl	!real*4 avgl(ns,nl)     average (left image)
      n2 = 4*ns*nl	!real*4 avgr(ns,nl)	average (right)
      n3 = 4*ns*nl	!real*4 varl(ns,nl)	variance (left)`
      n4 = 4*ns*nl	!real*4 varr(ns,nl)	variance (right)
      n5 = 4*ns*nl	!real*4 q(ns,nl)	Output correlation quality
      n6 = 4*ns*nl	!real*4 dx(ns,nl)	Output horizontal displacements
      n7 = 4*ns*nl	!real*4 dy(ns,nl)	Output vertical displacements
      n8 = 4*ns*nl	!integer*4 region(ns,nl) regions of dx and dy
      n9 = 2*ns*nl	!integer*2 l(ns,nl)	left image
      n10= 2*ns*nl	!integer*2 r(ns,nl)	right image
      n11 = ns*nl	!real*4 dx0(ns/2,nl/2)	lo-res dx
      n12 = ns*nl	!real*4 dy0(ns/2,nl/2)	lo-res dy
      n13 = ns*nl/2	!integer*2 l0(ns/2,nl/2) low-res l
      n14 = ns*nl/2	!integer*2 r0(ns/2,nl/2) low-res r 
      n15 = 2*ns*nl	!integer*2 mask(ns,nl)
      call stacka(26,rmain,18,m1,m2,m3,n1,n2,n3,n4,n5,n6,n7,n8,n9,
     &		n10,n11,n12,n13,n14,n15,nl,ns,ilo,ihi,jlo,jhi)
      return
      end
