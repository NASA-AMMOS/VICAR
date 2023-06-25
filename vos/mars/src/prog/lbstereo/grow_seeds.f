cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      subroutine grow_seeds(nl1,ns1,nl2,ns2,nl3,ns3,l3,l3avg,l3var,
     &		r1,r2,r3,i0_3,iz_3,x_3,lnpts,l1b_3,s1b_3,izmax,flag,
     &		i0,nlw,nsw,qthresh)
      implicit none
      integer*4 nl1,ns1,nl2,ns2,nl3,ns3
      integer*2 l3(256,256)
      real*4 l3avg(256,256),l3var(256,256)
      integer*2 r1(1024,1024),r2(512,512),r3(256,256)
      integer*2 i0_3(256,256),iz_3(256,256)
      real*4 x_3(9,256,256)
      integer*4 lnpts
      integer*2 l1b_3(lnpts),s1b_3(lnpts)
      integer*2 izmax(lnpts),flag(lnpts)
      integer*4 i0,nlw,nsw
      real*4 qthresh

      real*8 dl,ds
      integer*4 i,j,ii,jj
      integer*4 l0,s0,l,s
      integer*4 iz

      common/cx/x(8)            !affine or bilinear coefficients
      real*8 x

      call xvmessage('*****grow seeds here****',' ')

      do 60 ii=1,50
      jj = ii

      do 50 i0=1,lnpts
      if (flag(i0).ne.0) goto 50
      s0 = s1b_3(i0)
      l0 = l1b_3(i0)
      iz = izmax(i0)

      do 30 s=s0-ii+1,s0+ii-1
      l = l0 - jj
      if (x_3(9,s,l).lt.qthresh) then
         if (i0_3(s,l+1).eq.i0) then
            x(1) = x_3(1,s,l+1)
            x(2) = x_3(2,s,l+1) - 1
            x(3) = x_3(3,s,l+1)
            x(4) = x_3(4,s,l+1)
            x(5) = x_3(5,s,l+1)
            x(6) = x_3(6,s,l+1)
            x(7) = x_3(7,s,l+1)
            x(8) = x_3(8,s,l+1)
            call match_maker(nl1,ns1,nl2,ns2,nl3,ns3,l3,l3avg,l3var,
     &   	r1,r2,r3,i0_3,iz_3,x_3,lnpts,flag,l,s,i0,iz,
     &		nlw,nsw,qthresh)
         endif
      endif
      l = l0 + jj
      if (x_3(9,s,l).lt.qthresh) then
         if (i0_3(s,l-1).eq.i0) then
            x(1) = x_3(1,s,l-1)
            x(2) = x_3(2,s,l-1) + 1
            x(3) = x_3(3,s,l-1)
            x(4) = x_3(4,s,l-1)
            x(5) = x_3(5,s,l-1)
            x(6) = x_3(6,s,l-1)
            x(7) = x_3(7,s,l-1)
            x(8) = x_3(8,s,l-1)
            call match_maker(nl1,ns1,nl2,ns2,nl3,ns3,l3,l3avg,l3var,
     &   	r1,r2,r3,i0_3,iz_3,x_3,lnpts,flag,l,s,i0,iz,
     &		nlw,nsw,qthresh)
         endif
      endif
   30 continue

      do 40 l=l0-jj+1,l0+jj-1
      s = s0 - ii
      if (x_3(9,s,l).lt.qthresh) then
         if (i0_3(s+1,l).eq.i0) then
            x(1) = x_3(1,s+1,l) - 1
            x(2) = x_3(2,s+1,l)
            x(3) = x_3(3,s+1,l)
            x(4) = x_3(4,s+1,l)
            x(5) = x_3(5,s+1,l)
            x(6) = x_3(6,s+1,l)
            x(7) = x_3(7,s+1,l)
            x(8) = x_3(8,s+1,l)
            call match_maker(nl1,ns1,nl2,ns2,nl3,ns3,l3,l3avg,l3var,
     &   	r1,r2,r3,i0_3,iz_3,x_3,lnpts,flag,l,s,i0,iz,
     &		nlw,nsw,qthresh)
         endif
      endif
      s = s0 + ii
      if (x_3(9,s,l).lt.qthresh) then
         if (i0_3(s-1,l).eq.i0) then
            x(1) = x_3(1,s-1,l) + 1
            x(2) = x_3(2,s-1,l)
            x(3) = x_3(3,s-1,l)
            x(4) = x_3(4,s-1,l)
            x(5) = x_3(5,s-1,l)
            x(6) = x_3(6,s-1,l)
            x(7) = x_3(7,s-1,l)
            x(8) = x_3(8,s-1,l)
            call match_maker(nl1,ns1,nl2,ns2,nl3,ns3,l3,l3avg,l3var,
     &   	r1,r2,r3,i0_3,iz_3,x_3,lnpts,flag,l,s,i0,iz,
     &		nlw,nsw,qthresh)
         endif
      endif
   40 continue

      l = l0 - jj
      s = s0 - ii
      if (x_3(9,s,l).lt.qthresh) then
         if (i0_3(s+1,l+1).eq.i0) then
            x(1) = x_3(1,s+1,l+1) - 1
            x(2) = x_3(2,s+1,l+1) - 1
            x(3) = x_3(3,s+1,l+1)
            x(4) = x_3(4,s+1,l+1)
            x(5) = x_3(5,s+1,l+1)
            x(6) = x_3(6,s+1,l+1)
            x(7) = x_3(7,s+1,l+1)
            x(8) = x_3(8,s+1,l+1)
            call match_maker(nl1,ns1,nl2,ns2,nl3,ns3,l3,l3avg,l3var,
     &   	r1,r2,r3,i0_3,iz_3,x_3,lnpts,flag,l,s,i0,iz,
     &		nlw,nsw,qthresh)
         endif
      endif

      l = l0 - jj
      s = s0 + ii
      if (x_3(9,s,l).lt.qthresh) then
         if (i0_3(s-1,l+1).eq.i0) then
            x(1) = x_3(1,s-1,l+1) + 1
            x(2) = x_3(2,s-1,l+1) - 1
            x(3) = x_3(3,s-1,l+1)
            x(4) = x_3(4,s-1,l+1)
            x(5) = x_3(5,s-1,l+1)
            x(6) = x_3(6,s-1,l+1)
            x(7) = x_3(7,s-1,l+1)
            x(8) = x_3(8,s-1,l+1)
            call match_maker(nl1,ns1,nl2,ns2,nl3,ns3,l3,l3avg,l3var,
     &   	r1,r2,r3,i0_3,iz_3,x_3,lnpts,flag,l,s,i0,iz,
     &		nlw,nsw,qthresh)
         endif
      endif

      l = l0 + jj
      s = s0 - ii
      if (x_3(9,s,l).lt.qthresh) then
         if (i0_3(s+1,l-1).eq.i0) then
            x(1) = x_3(1,s+1,l-1) - 1
            x(2) = x_3(2,s+1,l-1) + 1
            x(3) = x_3(3,s+1,l-1)
            x(4) = x_3(4,s+1,l-1)
            x(5) = x_3(5,s+1,l-1)
            x(6) = x_3(6,s+1,l-1)
            x(7) = x_3(7,s+1,l-1)
            x(8) = x_3(8,s+1,l-1)
            call match_maker(nl1,ns1,nl2,ns2,nl3,ns3,l3,l3avg,l3var,
     &   	r1,r2,r3,i0_3,iz_3,x_3,lnpts,flag,l,s,i0,iz,
     &		nlw,nsw,qthresh)
         endif
      endif

      l = l0 + jj
      s = s0 + ii
      if (x_3(9,s,l).lt.qthresh) then
         if (i0_3(s-1,l-1).eq.i0) then
            x(1) = x_3(1,s-1,l-1) + 1
            x(2) = x_3(2,s-1,l-1) + 1
            x(3) = x_3(3,s-1,l-1)
            x(4) = x_3(4,s-1,l-1)
            x(5) = x_3(5,s-1,l-1)
            x(6) = x_3(6,s-1,l-1)
            x(7) = x_3(7,s-1,l-1)
            x(8) = x_3(8,s-1,l-1)
            call match_maker(nl1,ns1,nl2,ns2,nl3,ns3,l3,l3avg,l3var,
     &   	r1,r2,r3,i0_3,iz_3,x_3,lnpts,flag,l,s,i0,iz,
     &		nlw,nsw,qthresh)
         endif
      endif
   50 continue
   60 continue
      return
      end
