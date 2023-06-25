cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Given image I(nl,ns), compute the DN value at coordinates (l,s),
c where l and s are not integers, by bilinear interpolating over the four
c nearest integral neighbors.  See A.1.BilinearInterpolation.
c
      subroutine BilinearInt(I,l,s,DN,nl,ns,ind)
      implicit none
c     ...inputs:
      integer*4 nl,ns
      integer*2 I(ns,nl)
      real*8 l,s
c     ...output
      real*8 DN			!interpolated DN
      integer*4 ind		!ind=1 success

      integer*4 il,is
      integer*4 d1,d2,d3,d4
      real*8 dl,ds

      il = l		!greatest integer <= l
      is = s		!greatest integer <= s

      dl = l - il
      ds = s - is
      d1 = I(is,il)	!upper-left pixel
      d2 = I(is,il+1)	!lower-left pixel
      d3 = I(is+1,il)	!upper-right pixel
      d4 = I(is+1,il+1)	!lower-right pixel
      if (d1.le.0 .or. d2.le.0 .or. d3.le.0 .or. d4.le.0) then
         ind = 0
         return
      endif
      DN = d1 + (d2-d1)*dl + (d3-d1)*ds + (d1-d2-d3+d4)*dl*ds
      ind = 1
      return
      end
