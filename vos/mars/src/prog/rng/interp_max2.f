c********************************************************************
c Interpolate about integral maximum r(i0,j0) using a 2-d parabolic fit.
c Ref: "Quadratic interpolation of the surface maximum", Gary Yagi, JPL IOM,
c   April 10, 2005,
c
      subroutine interp_max2(r,ilo,ihi,jlo,jhi,i0,j0,dx,dy)
      implicit none
      integer*4 ilo,ihi,jlo,jhi
      real*4 r(ilo:ihi,jlo:jhi)	!correlation matrix
      real*4 q			!correlation maximum (returned)
      integer*4 i0,j0		!integral displacements
      real*4 dx,dy		!sub-pixel displacements (returned)

      integer*4 i,j
      real*4 rr,x,y
      real*8 a,b,c,d,e,f,det
      real*8 sz,sxz,syz,sx2z,sy2z,sxyz

      dx = i0
      dy = i0

c     ...Interpolate over the 3x3 area surrounding the max
      sz = 0.d0
      sxz = 0.d0
      syz = 0.d0
      sx2z = 0.d0
      sy2z = 0.d0
      sxyz = 0.d0

      do j=j0-1,j0+1
         y = j - j0
         do i=i0-1,i0+1
            x = i - i0
            rr = r(i,j)
            if (rr.eq.-1.) return
            sz = sz + rr
            sxz = sxz + x*rr
            syz = syz + y*rr
            sx2z = sx2z + x*x*rr
            sy2z = sy2z + y*y*rr
            sxyz = sxyz + x*y*rr
         enddo
      enddo

      a = (3*sx2z-2*sz)/6.
      b = sxyz/4.
      c = (3*sy2z-2*sz)/6.
      d = sxz/6.
      e = syz/6.
      f = (5*sz-3*(sx2z+sy2z))/9.
      det = 4*a*c - b**2
      x = (b*e-2*c*d)/det
      y = (b*d-2*a*e)/det
      q = a*x**2 + b*x*y + c*y**2 + d*x + e*y + f
      dx = x + i0
      dy = y + j0
      return
      end
