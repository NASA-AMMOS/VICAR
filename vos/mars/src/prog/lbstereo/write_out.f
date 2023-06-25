ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Output disparity map.
c
      subroutine write_out(x,dx,dy,nl,ns,qthresh)
      implicit none
      integer*4 nl,ns
      real*4 x(9,ns,nl)
      real*4 dx(ns,nl),dy(ns,nl)	!outputs
      real*4 qthresh

      integer*4 cnt,l,s
      character*132 file(2)

      call xvp('OUT',file,cnt)
      if (cnt.ne.2) return

      do l=1,nl
         do s=1,ns
            if (x(9,s,l).ge.qthresh) then
               dx(s,l) = x(1,s,l) - s
               dy(s,l) = x(2,s,l) - l
            else
               dx(s,l) = 0.
               dy(s,l) = 0.
            endif
         enddo
      enddo

      call write_out2(dx,nl,ns,1)
      call write_out2(dy,nl,ns,2)
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Write correlation coefficients q.
c
      subroutine write_q(q,nl,ns)
      implicit none
      integer*4 nl,ns
      real*4 q(ns,nl)
      integer*2 mask(ns,nl)
      logical xvptst

      integer i,j,cnt
      real*4 r
      character*132 file

      call xvp('q',file,cnt)		!q=(q1,q2,q3)
      if (cnt.eq.0) return
      do j=1,nl
         do i=1,ns
            r = q(i,j)
            if (r.ge.0.) then
               q(i,j) = sqrt(r)
            else
               r = sqrt(-r)
               q(i,j) = -r
            endif
         enddo
      enddo
      call output_image(q,nl,ns,file,1)
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
      subroutine write_sigma(varl,varr,nl,ns)
      implicit none
      integer*4 nl,ns
      real*4 varl(ns,nl),varr(ns,nl)	

      integer*4 i,j,cnt
      real*4 r
      character*132 file(2)

      call xvp('sigma',file,cnt)
      if (cnt.ne.2) return
      do j=1,nl
         do i=1,ns
            r = varl(i,j)
            if (r.gt.0.) varl(i,j)=sqrt(r)
            r = varr(i,j)
            if (r.gt.0.) varr(i,j)=sqrt(r)
         enddo
      enddo
      call output_image(varl,nl,ns,file(1),1)
      call output_image(varr,nl,ns,file(2),2)
      return
      end
