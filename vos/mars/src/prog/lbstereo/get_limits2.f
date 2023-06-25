cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get search limits for a left candidate i0.
c
      subroutine get_limits2(l0,s0,i,l2b,s2b,izmax,icmax,itmax,lnpts,
     &		dxlo,dxhi,dylo,dyhi,zlo,zhi,clo,chi,tlo,thi)
      implicit none
c     ...inputs
      integer*4 l0,s0,i,lnpts
      integer*2 l2b(lnpts),s2b(lnpts)	!(l,s) coords of right tiepoints
      integer*2 izmax(lnpts),icmax(lnpts),itmax(lnpts),flag(lnpts)
c     ...outputs
      integer*4 dxlo,dxhi,dylo,dyhi	!line-sample limits
      integer*4 zlo,zhi,clo,chi		!scale limits
      integer*4 tlo,thi			!rotation limits

      integer*4 dx,dy,iz,ic,it

      if (i.lt.1) return
      dy = l2b(i) - l0
      dx = s2b(i) - s0
      if (dx.lt.dxlo) dxlo=dx
      if (dx.gt.dxhi) dxhi=dx 
      if (dy.lt.dylo) dylo=dy
      if (dy.gt.dyhi) dyhi=dy

      iz = izmax(i)
      if (iz.lt.zlo) zlo=iz
      if (iz.gt.zhi) zhi=iz

      ic = icmax(i)
      if (ic.lt.clo) clo=ic
      if (ic.gt.chi) chi=ic

      it = itmax(i)
      if (it.lt.tlo) tlo=it
      if (it.gt.thi) thi=it
      return
      end
