      subroutine find_matches(nl1,ns1,nl2,ns2,
     &          l,lavg,lvar,r,ravg,rvar,
     &  	lnpts,rnpts,l1b,s1b,l2b,s2b,
     &  	rho,rhomax,l2max,s2max,izmax,ismax,itmax,
     &  	nlw,nsw,dl,ds,iz,islo,ishi,nsteps,qthresh,vthresh)
      implicit none
      integer*4 nl1,ns1,nl2,ns2
      integer*2 l(ns1,nl1),r(ns2,nl2)
      real*4 lavg(ns1,nl1),lvar(ns1,nl1),ravg(ns2,nl2),rvar(ns2,nl2)
      integer*4 lnpts,rnpts
      integer*2 l1b(lnpts),s1b(lnpts),l2b(rnpts),s2b(rnpts)
      real*4 rho(lnpts,rnpts)
      real*4 rhomax(lnpts)
      integer*2 l2max(256),s2max(256)
      integer*2 izmax(lnpts),ismax(lnpts),itmax(lnpts)
      integer*4 nlw,nsw,dl,ds,iz,islo,ishi,nsteps
      real*4 qthresh,vthresh

      integer*4 l1,s1,l2,s2,l2p,s2p
      integer*4 i1,i2,is,it,ind
      real*4 rmax,var2
      real*8 area,avg,var
      integer*4 nlwh,nswh

      real*4 window(51,51)
      real*4 rhobuf(256),lvarbuf(256),rvarbuf(256)
      integer*2 l2pbuf(256),s2pbuf(256)
      integer*2 isbuf(256),itbuf(256)
      integer*2 flag(256)

      area = nlw*nsw
      nlwh = nlw/2
      nswh = nsw/2
      call init_rho(rho,rhobuf,lnpts,rnpts,iz)

      do 50 i1=1,lnpts
      l1 = l1b(i1)		!(line,samp) of left tiepoint
      s1 = s1b(i1)
      do 50 is=islo,ishi
      do 50 it=1,nsteps
      call project(l,nl1,ns1,l1,s1,
     &		is,it,window,nlwh,nswh,area,avg,var,ind)
      if (ind.eq.0) goto 50	!invalid area

      do 40 i2=1,rnpts
      l2 = l2b(i2)		!(line,samp) of right tiepoint
      s2 = s2b(i2)
      call find_match(window,nlwh,nswh,r,ravg,rvar,nl2,ns2,
     &		avg,var,vthresh,dl,ds,l2,s2,l2p,s2p,rmax,var2)
      if (rmax.gt.rho(i1,i2)) then
         rho(i1,i2) = rmax
         if (rmax.gt.rhobuf(i1)) then
            rhobuf(i1) = rmax
            l2pbuf(i1) = l2p
            s2pbuf(i1) = s2p
            lvarbuf(i1) = var
            rvarbuf(i1) = var2
            isbuf(i1) = is
            itbuf(i1) = it
         endif
      endif
   40 continue

   50 continue

      call flag_mismatches(rho,lvarbuf,rvarbuf,isbuf,itbuf,
     &		lnpts,rnpts,qthresh,flag)
      call print_hist0(isbuf,itbuf,flag,lnpts)

      do 55 i1=1,lnpts
      if (flag(i1).ne.0) goto 55
      if (rhobuf(i1).gt.rhomax(i1)) then
         rhomax(i1) = rhobuf(i1)
         l2max(i1) = l2pbuf(i1)
         s2max(i1) = s2pbuf(i1)
         ismax(i1) = isbuf(i1)
         itmax(i1) = itbuf(i1)
         izmax(i1) = iz
      endif
   55 continue

      return
      end
