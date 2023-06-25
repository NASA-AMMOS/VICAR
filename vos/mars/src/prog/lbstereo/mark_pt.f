cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Mark the tiepoint on the image display.
c
      subroutine mark_pt(l,s,j,maxdn,buf,nl,ns)
      implicit none
c     ...inputs
      integer*4 l,s,j,maxdn
      integer*4 nl,ns
c     ...image to be marked
      integer*2 buf(ns,nl)

      integer*4 i

      if (j.eq.-1) then			!flag bad dy as large empty square
         do i=-2,2
            buf(s+i,l-2) = maxdn
            buf(s+i,l+2) = maxdn
            buf(s-2,l+i) = maxdn
            buf(s+2,l+i) = maxdn
         enddo
         return
      endif

      if (j.gt.0) buf(s,l) = maxdn	!if match, mark center pixel
      i = mod(j,10)
      if (i.le.3) then
         buf(s-1,l-1) = maxdn
         buf(s-1,l+1) = maxdn
         buf(s+1,l-1) = maxdn
         buf(s+1,l+1) = maxdn
         if (i.eq.0) return
         if (i.ne.1) then
            buf(s-2,l+2) = maxdn
            buf(s+2,l-2) = maxdn
         endif
         if (i.ne.2) then
            buf(s-2,l-2) = maxdn
            buf(s+2,l+2) = maxdn
	 endif
      elseif (i.lt.9) then
         buf(s-1,l) = maxdn		!start with small cross
         buf(s+1,l) = maxdn
         buf(s,l-1) = maxdn
         buf(s,l+1) = maxdn
         if (i.eq.4) return
         if (i.ne.5) then
            buf(s-2,l) = maxdn
            buf(s+2,l) = maxdn
         endif
         if (i.ne.6) then
            buf(s,l-2) = maxdn
            buf(s,l+2) = maxdn
         endif
         if (i.eq.8) then
            buf(s-1,l-1) = maxdn
            buf(s-1,l+1) = maxdn
            buf(s+1,l-1) = maxdn
            buf(s+1,l+1) = maxdn
         endif
      else
         buf(s-2,l) = maxdn		!if no match, mark as small empty square
         buf(s+2,l) = maxdn
         buf(s,l-2) = maxdn
         buf(s,l+2) = maxdn
      endif
      return
      end
