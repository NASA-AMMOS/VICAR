ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Delete range mismatches.
c
      subroutine rdespike(nl,ns,dx,dy,mask,region,zoom)
      implicit none
      integer*4 nl,ns			!image size is nl x ns
      real*4 dx(ns,nl),dy(ns,nl)	!hori & vert displacements
      integer*2 mask(ns,nl)		!valid range mask
      integer*4 region(ns,nl)		!region map for dx1 and dy1
      integer*4 zoom			!pyramid zoom factor

      common/ctemp/obuf(1024),rsize,rx
      integer*2 obuf
      integer*4 rsize(500000)		!number of pixels in region
      integer*4 rx(500000)		!region index
      integer*4 nr,maxnr/500000/	!number of regions

      call regions(dx,dy,mask,nl,ns,maxnr,nr,region,rsize,rx)
      call delete_regions(region,mask,nl,ns,rsize,rx,nr,zoom)
      call write_regions(region,obuf,nl,ns,rx,nr)
      call write_rsize(region,obuf,nl,ns,rsize,rx,nr)
      return
      end
