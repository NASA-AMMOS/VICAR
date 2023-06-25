cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get search limits for a left candidate i0.
c
      subroutine get_limits(l1b,s1b,l2b,s2b,izmax,icmax,itmax,flag,
     &		lnpts,i0,dxlo,dxhi,dylo,dyhi,zlo,zhi,clo,chi,tlo,thi)
      implicit none
c     ...inputs
      integer*4 lnpts,i0
      integer*2 l1b(lnpts),s1b(lnpts)	!(l,s) coords of left tiepoints
      integer*2 l2b(lnpts),s2b(lnpts)	!(l,s) coords of right tiepoints
      integer*2 izmax(lnpts),icmax(lnpts),itmax(lnpts),flag(lnpts)
c     ...outputs
      integer*4 dxlo,dxhi,dylo,dyhi	!horizontal and vertical limits
      integer*4 zlo,zhi,clo,chi		!scale limits
      integer*4 tlo,thi			!rotation limits

      integer*4 l0,s0
      integer*4 ill,ilr,iul,iur

      l0 = l1b(i0)
      s0 = s1b(i0)

      call closest_points(l1b,s1b,flag,lnpts,i0,ill,ilr,iul,iur)

      dxlo = 99999.
      dxhi = -99999.
      dylo = 99999.
      dyhi = -99999.
      zlo = 4
      zhi = 0
      clo = 5
      chi = -5
      tlo = 999
      thi = -999

      call get_limits2(l0,s0,ill,l2b,s2b,izmax,icmax,itmax,lnpts,
     &		dxlo,dxhi,dylo,dyhi,zlo,zhi,clo,chi,tlo,thi)
      call get_limits2(l0,s0,ilr,l2b,s2b,izmax,icmax,itmax,lnpts,
     &		dxlo,dxhi,dylo,dyhi,zlo,zhi,clo,chi,tlo,thi)
      call get_limits2(l0,s0,iul,l2b,s2b,izmax,icmax,itmax,lnpts,
     &		dxlo,dxhi,dylo,dyhi,zlo,zhi,clo,chi,tlo,thi)
      call get_limits2(l0,s0,iur,l2b,s2b,izmax,icmax,itmax,lnpts,
     &		dxlo,dxhi,dylo,dyhi,zlo,zhi,clo,chi,tlo,thi)
      return
      end
