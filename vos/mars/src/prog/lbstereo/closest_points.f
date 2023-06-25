cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Find closest valid tiepoints to a left candidate i0.
c
      subroutine closest_points(l1b,s1b,flag,lnpts,i0,ill,ilr,iul,iur)
      implicit none
c     ...inputs
      integer*4 lnpts,i0
      integer*2 l1b(lnpts),s1b(lnpts)	!(l,s) coords of left tiepoints
      integer*2 flag(lnpts)
c     ...outputs
      integer*4 ill,ilr,iul,iur

      integer*4 l0,s0,l,s,i
      integer*4 d,dll,dlr,dul,dur

      l0 = l1b(i0)
      s0 = s1b(i0)

      ill = 0		!to the lower left
      ilr = 0		!to the lower right
      iul = 0		!to the upper left
      iur = 0		!to the upper right

      dll = 9999999.	!distance from i0
      dlr = 9999999.
      dul = 9999999.
      dur = 9999999.

      i = i0

c     ...search below for lower-left and lower-right closest tiepoints
   20 i = i - 1
      if (i.lt.1) goto 30
      if (flag(i).ne.0) goto 20
      l = l1b(i)
      s = s1b(i)
      if (l0-l.gt.100) goto 30
      d = (l-l0)**2 + (s-s0)**2
      if (s.lt.s0 .and. d.lt.dll) then
         ill = i
         dll = d
      elseif (d.lt.dlr) then
         ilr = i
         dlr = d
      endif
      goto 20

   30 i = i0
   31 i = i + 1
      if (i.gt.lnpts) goto 40
      l = l1b(i)
      s = s1b(i)
      if (l-l0.gt.100) goto 40
      d = (l-l0)**2 + (s-s0)**2
      if (s.lt.s0 .and. d.lt.dul) then
         iul = i
         dul = d
      elseif (d.lt.dur) then
         iur = i
         dur = d
      endif
      goto 31

   40 return
      end
