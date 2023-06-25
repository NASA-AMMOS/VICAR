cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Write pseudo-left image to disk.
c
      subroutine write_pl(r,dx,dy,pl,nl,ns,jbeg,jend)
      implicit none
      integer*4 nl,ns,jbeg,jend
      integer*2 r(ns,nl)		!right image
      real*4 dx(ns,nl),dy(ns,nl)	!x and y displacements
      integer*2 pl(ns,nl)		!output pseudo-left

      integer*4 cnt,cnt2
      character*132 file,file2

      call xvp('PSEUDOL',file,cnt)

c     ...currently (4/14/07) pl is not computed elsewhere
ccc      call xvpcnt('DIFF',cnt2,' ')
      call xvp('DIFF',file2,cnt2)
      if (cnt.eq.0 .and. cnt2.eq.0) return
      call compute_pl(r,dx,dy,pl,nl,ns,jbeg,jend)

      if (cnt.eq.1) call output_image2(pl,nl,ns,file,1)
      return
      end
