ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Output disparity map.
c
      subroutine write_out(dx,dy,mask,nl,ns)
      implicit none
      integer*4 nl,ns
      real*4 dx(ns,nl),dy(ns,nl)
      integer*2 mask(ns,nl)

      integer*4 cnt
      logical xvptst
      character*132 file(2)

      call xvp('OUT',file,cnt)
      if (cnt.ne.2) return
      if (xvptst('usemask')) then
         call output_mimage(dx,mask,nl,ns,file(1),1)
         call output_mimage(dy,mask,nl,ns,file(2),2)
      else
         call output_image(dx,nl,ns,file(1),1)
         call output_image(dy,nl,ns,file(2),2)
      endif
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Write correlation coefficients q.
c
      subroutine write_q(q,nl,ns)
      implicit none
      integer*4 nl,ns
      real*4 q(ns,nl)

      integer cnt	!local variables
      character*132 file

      call xvp('q',file,cnt)		!q=(q1,q2,q3)
      if (cnt.gt.0) call output_image(q,nl,ns,file,cnt)
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Output the average images.
c
      subroutine write_avg(avgl,avgr,nl,ns)
      implicit none
      integer*4 nl,ns
      real*4 avgl(ns,nl),avgr(ns,nl)	

      integer*4 cnt
      character*132 file(2)

      call xvp('AVERAGE',file,cnt)
      if (cnt.ne.2) return
      call output_image(avgl,nl,ns,file(1),1)
      call output_image(avgr,nl,ns,file(2),2)
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Output the variance images.
c
      subroutine write_var(varl,varr,nl,ns)
      implicit none
      integer*4 nl,ns
      real*4 varl(ns,nl),varr(ns,nl)	

      integer*4 cnt
      character*132 file(2)

      call xvp('VARIANCE',file,cnt)
      if (cnt.ne.2) return
      call output_image(varl,nl,ns,file(1),1)
      call output_image(varr,nl,ns,file(2),2)
      return
      end
