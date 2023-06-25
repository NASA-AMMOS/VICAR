ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Read horizontal and vertical disparities dx,dy
c
      subroutine read_disparity(dx,dy,nl,ns)
      implicit none
      integer*4 nl,ns
      real*4 dx(ns,nl),dy(ns,nl)

      common/ctemp/obuf(1024)
      real*4 obuf

      integer*4 cnt
      character*132 file(2)

      call xvp('DISPARITY',file,cnt)
      if (cnt.ne.2) return
      call read_image(file,1,dx,nl,ns)
      call read_image(file,2,dy,nl,ns)
      return
      end
