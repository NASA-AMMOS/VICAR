cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c VICAR program lbstereo:
c   lbstereo inp=(l,r) 
c       iz=(1,5) ic=(-4,4) angle=(-10,10,10)
c       sthresh=100. qthresh=.7
c       sfactor=2.5 rthresh=0.2 'print
c
      include 'VICMAIN_FOR'
      subroutine main44
      implicit none

c     ...input left and right images
      integer*4 iunit(2)		!inp=(l1,r1)
      integer*4 nl1,ns1,nl2,ns2,nl3,ns3

      integer*2 lhrzn(1024),rhrzn(1024)

c     ...left image at 1,1/2, and 1/4 resolution
      integer*2 l1(1024,1024),l2(512,512),l3(256,256)
      real*4 l1avg(1024,1024),l2avg(512,512),l3avg(256,256)
      real*4 l1var(1024,1024),l2var(512,512),l3var(256,256)

c     ...right image at 1, 1/2, and 1/4 resolution
      integer*2 r1(1024,1024),r2(512,512),r3(256,256)
      real*4 r1avg(1024,1024),r2avg(512,512),r3avg(256,256)
      real*4 r1var(1024,1024),r2var(512,512),r3var(256,256)

c     ...geometric coefficients
      integer*2 i0_3(256,256),iz_3(256,256)
      real*4 x_3(9,256,256)
      real*4 dx(256,256),dy(256,256)

c     ...candidate tiepoints
      integer*2 l1b(256),s1b(256)	!(l,s) coords of left candidates
      integer*2 l2b(256),s2b(256)

      integer*2 l1b_3(256),s1b_3(256)	!(l,s) coords of left candidates

      integer*2 l2b_1(4096),s2b_1(4096)	!(l,s) coords of right candidates
      integer*2 l2b_2(1024),s2b_2(1024)
      integer*2 l2b_3(256),s2b_3(256)
      integer*4 lnpts,rnpts(3)		!number of candidate tiepoints
      integer*4 nmax(3)/4096,1024,256/	!max number of candidates

c     ...correlation parameters
      real*4 qthresh,vthresh
      integer*4 nlw,nsw,dl,ds

c     ...global maxima
      real*4 rhomax(256)
      integer*2 l2max(256),s2max(256)
      integer*2 izmax(256),icmax(256),itmax(256),flag(256)

      real*4 eps
      integer*4 i,j,l,s
      integer*4 izlo,izhi,iclo,ichi,nsteps
      integer*4 i0,i1,i2,iz,ic,it
      integer*4 dxlo,dxhi,dylo,dyhi,zlo,zhi,clo,chi,tlo,thi

      character*80 msg

      common/ctemp/rho
      integer*2 buf(1024,1024)
      real*4 rho(256,2048)
      equivalence (rho,buf)

      call xvmessage('lbstereo version Sept 28 2009',' ')
      call open_inp(iunit,nl1,ns1)	!inp=(l1,r1)
      call get_stereo(iunit,nl1,ns1,l1,r1,buf)
      nl2 = nl1/2
      ns2 = ns1/2
      nl3 = nl1/4
      ns3 = ns1/4

      call get_params(nlw,nsw,dl,ds,vthresh,qthresh)

      call horizon(1,l1,nl1,ns1,l1avg,l1var,lhrzn)
      call horizon(2,r1,nl1,ns1,r1avg,r1var,rhrzn)

      call get_params2(izlo,izhi,iclo,ichi,nsteps)

      do i1=1,lnpts
	 rhomax(i1) = -999.
      enddo

      call zoomout(l1,l3,nl1,ns1,4)	!create l3(ns3,nl3)
      call compute_stats(l3,l3avg,l3var,nl3,ns3,nlw,nsw)
      call find_candidates(l3var,nl3,ns3,dl,ds,
     &  	l1b_3,s1b_3,nmax(3),lnpts)

      call compute_stats(r1,r1avg,r1var,nl1,ns1,nlw,nsw)
      call find_candidates(r1var,nl1,ns1,dl,ds,
     &  	l2b_1,s2b_1,nmax(1),rnpts(1))

      call zoomout(r1,r2,nl1,ns1,2)	!create r2(ns2,nl2)
      call compute_stats(r2,r2avg,r2var,nl2,ns2,nlw,nsw)
      call find_candidates(r2var,nl2,ns2,dl,ds,
     &  	l2b_2,s2b_2,nmax(2),rnpts(2))

      call zoomout(r1,r3,nl1,ns1,4)	!create r3(ns3,nl3)
      call compute_stats(r3,r3avg,r3var,nl3,ns3,nlw,nsw)
      call find_candidates(r3var,nl3,ns3,dl,ds,
     &  	l2b_3,s2b_3,nmax(3),rnpts(3))

      iz = 1
      if (izlo.eq.1) call find_matches(nl3,ns3,nl1,ns1,
     &		l3,l3avg,l3var,r1,r1avg,r1var,
     &		lnpts,rnpts(1),l1b_3,s1b_3,l2b_1,s2b_1,
     &		rho,rhomax,l2max,s2max,izmax,icmax,itmax,
     &		nlw,nsw,dl,ds,iz,iclo,ichi,nsteps,qthresh,vthresh)

      iz = 2
      if (izlo.le.2 .and. izhi.ge.2) call find_matches(nl3,ns3,nl2,ns2,
     &		l3,l3avg,l3var,r2,r2avg,r2var,
     &		lnpts,rnpts(2),l1b_3,s1b_3,l2b_2,s2b_2,
     &		rho,rhomax,l2max,s2max,izmax,icmax,itmax,
     &		nlw,nsw,dl,ds,iz,iclo,ichi,nsteps,qthresh,vthresh)

      iz = 3
      if (izhi.eq.3) call find_matches(nl3,ns3,nl3,ns3,
     &		l3,l3avg,l3var,r3,r3avg,r3var,
     &		lnpts,rnpts(3),l1b_3,s1b_3,l2b_3,s2b_3,
     &		rho,rhomax,l2max,s2max,izmax,icmax,itmax,
     &		nlw,nsw,dl,ds,iz,iclo,ichi,nsteps,qthresh,vthresh)

      call flag_rho(lnpts,rhomax,qthresh,flag)
      call scale_matches1(l1b_3,s1b_3,l2max,s2max,
     &		l1b,s1b,l2b,s2b,izmax,lnpts,nl1,ns1)
      call flag_dxdy(lnpts,l1b,s1b,l2b,s2b,flag)

      call display_matches(l1b,s1b,l2b,s2b,flag,lnpts,
     &		l1,r1,buf,nl1,ns1)
      call print_matches(l1b_3,s1b_3,l2max,s2max,
     &		izmax,icmax,itmax,flag,lnpts)

ccc      do 50 i0=1,lnpts
ccc      if (flag(i0).eq.0) goto 50	!skip if valid
ccc      call find_matchx(i0)
ccc      call get_limits(l1b,s1b,l2b,s2b,izmax,icmax,itmax,flag,lnpts,
ccc     &		i0,dxlo,dxhi,dylo,dyhi,zlo,zhi,clo,chi,tlo,thi)
ccc   50 continue

      call plant_seeds(nl1,ns1,nl2,ns2,nl3,ns3,l3,l3avg,l3var,
     &		r1,r2,r3,i0_3,iz_3,x_3,lnpts,l1b_3,s1b_3,
     &		l2max,s2max,izmax,icmax,itmax,flag,i0,
     &		nlw,nsw,qthresh)

      call grow_seeds(nl1,ns1,nl2,ns2,nl3,ns3,l3,l3avg,l3var,
     &		r1,r2,r3,i0_3,iz_3,x_3,lnpts,l1b_3,s1b_3,izmax,flag,i0,
     &		nlw,nsw,qthresh)

ccc      do 100 i=1,500
ccc      do 90 i1=1,lnpts
ccc      if (flag(i1).ne.0) goto 90


ccc      call print_matches(l1b,s1b,l2b,s2b,
ccc     &		izmax,icmax,itmax,flag,lnpts)

ccc      call flag_mismatch2(lnpts,rhomax,izmax,icmax,itmax,flag,qthresh)
ccc      call tsub(l1b,s1b,l2b,s2b,izmax,icmax,itmax,
ccc     &		flag,lnpts,nl,ns)
ccc      call display_ctp(l1b,s1b,lnpts,l0,buf,nl,ns)
ccc      call image_displays(l,r,lavg,lvar,buf,nl/4,ns/4,
ccc     &		l1b,s1b,nmax/16,nlw,nsw,dl,ds)
      call write_out(x_3,dx,dy,nl3,ns3,qthresh)
      call write_marsout(dx,dy,iz_3,nl3,ns3)
      return
      end
