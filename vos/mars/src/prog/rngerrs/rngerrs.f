c VICAR program rngerrs
c   rngerrs inp=(left,right) out=q nq=nq disp=(dx,dy) +
c	pseudox=z diff=diff
c
      include 'VICMAIN_FOR'
      subroutine main44
      implicit none
      integer*4 iunit(2)		!input images logical unit numbers
      integer*4 nl,ns			!input images are nl x ns
      integer*4 n1,n2,n3,n4,n5,n6,n7
      external rmain

      call xvmessage('rngerrs version Feb 7 2007',' ')
      call open_inp(iunit,nl,ns)	!INP=(left,right)

c     ...dynamic allocation of image, line, and work buffers.
      n1 = 4*nl*ns	!real*4 q(ns,nl)	Output correlation quality
      n2 = 4*nl*ns	!real*4 dx(ns,nl)	Output horizontal displacements
      n3 = 4*nl*ns	!real*4 dy(ns,nl)	Output vertical displacements
      n4 = 4*nl*ns	!real*4 z(ns,nl)	pseudo-left image
      n5 = 2*nl*ns	!integer*2 left(ns,nl)	left image
      n6 = 2*nl*ns	!integer*2 right(ns,nl) right image
      n7 = 2*nl*ns	!integer*2 nq(ns,nl)	# valid pxls in corr area
      call stacka(10,rmain,7,n1,n2,n3,n4,n5,n6,n7,iunit,nl,ns)
      return
      end
