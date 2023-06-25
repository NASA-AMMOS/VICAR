cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      subroutine match_maker(nl1,ns1,nl2,ns2,nl3,ns3,l3,l3avg,l3var,
     &		r1,r2,r3,i0_3,iz_3,x_3,lnpts,flag,l,s,i0,iz,
     &		nlw,nsw,qthresh)
      implicit none
      integer*4 nl1,ns1,nl2,ns2,nl3,ns3
      integer*2 l3(256,256)
      real*4 l3avg(256,256),l3var(256,256)
      integer*2 r1(1024,1024),r2(512,512),r3(256,256)
      integer*2 i0_3(256,256),iz_3(256,256)
      real*4 x_3(9,256,256)
      integer*4 lnpts
      integer*2 flag(lnpts)
      integer*4 l,s,i0,iz
      integer*4 nlw,nsw
      real*4 qthresh

      real*4 eps
      real*8 dl,ds,lp,sp
      integer*4 nlwh,nswh,m,lmax_a,lmax_b
      real*8 A(625,8)		!m x n Jacobian
      real*8 b(625)
      real*8 r(625)
      real*8 I10(625),I20(625)
      real*8 meanI1,varI1
      real*8 rmax
      character*80 msg

      common/cx/x(8)            !affine or bilinear coefficients
      real*8 x

      nlwh = nlw/2
      nswh = nsw/2
      m = nlw*nsw
      lmax_a = 3		!parameterize???
      lmax_b = 10		!parameterize???
      eps = 0.0001

      call get_window(nl3,ns3,l3,l,s,nlwh,nswh,I10)
      meanI1 = l3avg(s,l)
      varI1 = l3var(s,l)
      sp = x(1)
      lp = x(2)
      if (iz.eq.1) then 
         call match_offset1(nl1,ns1,r1,l,s,lmax_a,eps,nlw,nsw,
     &		m,2,A,b,r,I10,meanI1,varI1,I20,rmax)
         call match_affine1(nl1,ns1,r1,l,s,lmax_a,eps,nlw,nsw,
     &		m,6,A,b,r,I10,meanI1,varI1,I20,rmax)
         call match_bilinear(nl1,ns1,r1,l,s,lmax_b,eps,nlw,nsw,
     &		m,8,A,b,r,I10,meanI1,varI1,I20,rmax)
      elseif (iz.eq.2) then
         call match_offset1(nl2,ns2,r2,l,s,lmax_a,eps,nlw,nsw,
     &		m,2,A,b,r,I10,meanI1,varI1,I20,rmax)
         call match_affine1(nl2,ns2,r2,l,s,lmax_a,eps,nlw,nsw,
     &		m,6,A,b,r,I10,meanI1,varI1,I20,rmax)
         call match_bilinear(nl2,ns2,r2,l,s,lmax_b,eps,nlw,nsw,
     &		m,8,A,b,r,I10,meanI1,varI1,I20,rmax)
      elseif (iz.eq.3) then
         call match_offset1(nl3,ns3,r3,l,s,lmax_a,eps,nlw,nsw,
     &		m,2,A,b,r,I10,meanI1,varI1,I20,rmax)
         call match_affine1(nl3,ns3,r3,l,s,lmax_a,eps,nlw,nsw,
     &		m,6,A,b,r,I10,meanI1,varI1,I20,rmax)
         call match_bilinear(nl3,ns3,r3,l,s,lmax_b,eps,nlw,nsw,
     &		m,8,A,b,r,I10,meanI1,varI1,I20,rmax)
      endif
      if (rmax.lt.qthresh) goto 990
      ds = x(1) - sp
      dl = x(2) - lp
      if (dl**2+ds**2.gt.25.) goto 990
      x_3(1,s,l) = x(1)
      x_3(2,s,l) = x(2)
      x_3(3,s,l) = x(3)
      x_3(4,s,l) = x(4)
      x_3(5,s,l) = x(5)
      x_3(6,s,l) = x(6)
      x_3(7,s,l) = x(7)
      x_3(8,s,l) = x(8)
      x_3(9,s,l) = rmax
      i0_3(s,l) = i0
      iz_3(s,l) = iz
ccc      write(msg,1000) i0,iz,ds,dl,x(3),x(4),
ccc     &		x(5),x(6),rmax
ccc      call xvmessage(msg,' ')
ccc 1000 format(i4,i2,2f10.4,4f10.5,f10.6)
      return

  990 return
      end
