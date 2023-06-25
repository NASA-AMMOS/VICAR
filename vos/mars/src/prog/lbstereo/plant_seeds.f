cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      subroutine plant_seeds(nl1,ns1,nl2,ns2,nl3,ns3,l3,l3avg,l3var,
     &		r1,r2,r3,i0_3,iz_3,x_3,lnpts,l1b_3,s1b_3,
     &		l2max,s2max,izmax,icmax,itmax,flag,i0,
     &		nlw,nsw,qthresh)
      implicit none
      integer*4 nl1,ns1,nl2,ns2,nl3,ns3
      integer*2 l3(ns3,nl3)
      real*4 l3avg(ns3,nl3),l3var(ns3,nl3)
      integer*2 r1(ns1,nl1),r2(ns2,nl2),r3(ns3,nl3)
      integer*2 i0_3(ns3,nl3),iz_3(ns3,nl3)
      real*4 x_3(9,ns3,nl3)
      integer*4 lnpts
      integer*2 l1b_3(lnpts),s1b_3(lnpts)!(l,s) coords of left candidates
      integer*2 l2max(lnpts),s2max(lnpts)
      integer*2 izmax(lnpts),icmax(lnpts),itmax(lnpts),flag(lnpts)
      integer*4 i0,nlw,nsw
      real*4 qthresh

      real*4 eps
      real*8 dl,ds
      integer*4 i,j,l,s,m,lmax_a,lmax_b
      integer*4 iz,ic,it
      integer*4 nlwh,nswh
      real*8 A(625,8)		!m x n Jacobian
      real*8 b(625)
      real*8 r(625)
      real*8 I10(625),I20(625)
      real*8 meanI1,varI1
      real*8 rmax
      real*8 Cminor,sint,cost
      character*80 msg

      common/cs/cm
      real*8 cm(-4:4)

      common/ct/ct,st,deglo,deghi,ddeg
      real*8 ct(360),st(360)
      integer*4 deglo,deghi,ddeg

      common/cx/x(8)            !affine or bilinear coefficients
      real*8 x

      do l=1,nl3
         do s=1,ns3
            x_3(9,s,l) = -999.
            iz_3(s,l) = 0
         enddo
      enddo

ccc      call prnt(4,1,nlw,'nlw=.')
ccc      call prnt(4,1,nsw,'nsw=.')
      nlwh = nlw/2
      nswh = nsw/2
      m = nlw*nsw
      lmax_a = 3		!parameterize???
      lmax_b = 10		!parameterize???
      eps = 0.0001

      do 80 i0=1,lnpts
      if (flag(i0).ne.0) goto 80
      s = s1b_3(i0)
      l = l1b_3(i0)
ccc      write(msg,111) l,s
ccc      call xvmessage(msg,' ')
ccc  111 format('(l,s)=',2i5)
      iz = izmax(i0)
      ic = icmax(i0)
      it = itmax(i0)
      Cminor = cm(-ic)
      sint = -st(it)
      cost = ct(it)
ccc      call prnt(4,1,ic,'ic=.')
ccc      call prnt(8,1,Cminor,'Cminor=.')
ccc      call prnt(4,1,it,'it=.')
ccc      call prnt(8,1,cost,'cost=.')
ccc      call prnt(8,1,sint,'sint=.')

ccc      write(msg,112) l2max(i0),s2max(i0)
ccc      call xvmessage(msg,' ')
ccc  112 format('(lp,sp)=',2i5)

      x(1) = s2max(i0)
      x(2) = l2max(i0)
      x(3) = Cminor*sint
      x(4) = Cminor*cost
      x(5) = Cminor*cost
      x(6) = -Cminor*sint
      x(7) = 0.
      x(8) = 0.
      call get_window(nl3,ns3,l3,l,s,nlwh,nswh,I10)
      meanI1 = l3avg(s,l)
      varI1 = l3var(s,l)
ccc      call prnt(8,1,meanI1,'meanI1=.')
ccc      call prnt(8,1,varI1,'varI1=.')
ccc      i = 1
ccc      do j=1,nlw
ccc         call prnt(7,5,I10(i),' .')
ccc         i = i + nsw
ccc      enddo

      if (iz.eq.1) then 
         call match_affine(nl1,ns1,r1,l,s,lmax_a,eps,nlw,nsw,
     &		m,6,A,b,r,I10,meanI1,varI1,I20,rmax)
         call match_bilinear(nl1,ns1,r1,l,s,lmax_b,eps,nlw,nsw,
     &		m,8,A,b,r,I10,meanI1,varI1,I20,rmax)
      elseif (iz.eq.2) then
         call match_affine(nl2,ns2,r2,l,s,lmax_a,eps,nlw,nsw,
     &		m,6,A,b,r,I10,meanI1,varI1,I20,rmax)
         call match_bilinear(nl2,ns2,r2,l,s,lmax_b,eps,nlw,nsw,
     &		m,8,A,b,r,I10,meanI1,varI1,I20,rmax)
      elseif (iz.eq.3) then
         call match_affine(nl3,ns3,r3,l,s,lmax_a,eps,nlw,nsw,
     &		m,6,A,b,r,I10,meanI1,varI1,I20,rmax)
         call match_bilinear(nl3,ns3,r3,l,s,lmax_b,eps,nlw,nsw,
     &		m,8,A,b,r,I10,meanI1,varI1,I20,rmax)
      endif
      if (rmax.lt.qthresh) then
         flag(i0) = 1
         goto 80
      endif
      ds = x(1) - s2max(i0)
      dl = x(2) - l2max(i0)
ccc      call prnt(7,1,dl,'dl=.')
ccc      call prnt(7,1,ds,'ds=.')
      if (dl**2+ds**2.gt.25.) then
         flag(i0) = 2
         goto 80
      endif
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
      write(msg,110) i0,iz,ds,dl,x(3),x(4),
     &		x(5),x(6),rmax
      call xvmessage(msg,' ')
  110 format(i4,i2,2f10.4,4f10.5,f10.6)
   80 continue

      return
      end
