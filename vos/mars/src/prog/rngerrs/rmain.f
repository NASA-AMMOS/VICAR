ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine rmain(q,n1,dx,n2,dy,n3,z,n4,x,n5,y,n6,nq,n7,
     &		iunit,nl,ns) 
      implicit none
      integer*4 iunit(2)		!inp image logical unit numbers
      integer*4 nl,ns			!image size is nl x ns
      integer*2 x(ns,nl),y(ns,nl)	!inp=(x,y)
      real*4 q(ns,nl)			!corr quality
      real*4 dx(ns,nl),dy(ns,nl)	!hori & vert displacements
      real*4 z(ns,nl)			!pseudo-left image
      integer*2 nq(ns,nl)		!# of valid pixels in correlation area
      integer*4 n1,n2,n3,n4,n5,n6,n7	!# bytes

      integer*4 cnt
      character*132 file

      if (n7.ne.2*nl*ns) call mabend('***Insufficient memory',' ')
      call read_inp(iunit(1),x,nl,ns)		!inp=(x,y)
      call read_inp(iunit(2),y,nl,ns)
      call read_disparity(dx,dy,nl,ns)		!disparity=(dx,dy)
      call compute_z(y,dx,dy,z,nl,ns)		!z(i,j)=y(i+dx,j+dy)
      call compute_q(x,z,q,nq,nl,ns)		!area matching
      call print_stats(z,q,nq,nl,ns)
      call xvp('PSEUDOX',file,cnt)
      if (cnt.eq.1) call write_image(file,1,z,nl,ns)
      call xvp('Q',file,cnt)
      if (cnt.eq.1) call write_image(file,1,q,nl,ns)
      call write_nq(nq,nl,ns)
      call write_diff(x,z,nl,ns)
      return
      end
