cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fill real*4 buffer with specified value.
c
      subroutine fill_buf(buf,n,val)
      implicit none
      integer*4 n
      real*4 buf(n),val

      integer*4 i

      do i=1,n
         buf(i) = val
      enddo
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fill integer*2 buffer with specified value.
c
      subroutine fill_buf2(buf,n,val)
      implicit none
      integer*4 n,val
      integer*2 buf(n)

      integer*4 i

      do i=1,n
         buf(i) = val
      enddo
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fill margins of a real*4 image with specified value.
c
      subroutine fill_margins(img,nl,ns,ibeg,iend,jbeg,jend,val)
      implicit none
      integer nl,ns,ibeg,iend,jbeg,jend
      real*4 img(ns,nl),val

      integer*4 j,n1,n2

c     ...top margin
      do j=1,jbeg-1
         call fill_buf(img(1,j),ns,val)
      enddo

c     ...interior
      n1 = ibeg - 1
      n2 = ns - iend
      do j=jbeg,jend
         call fill_buf(img(1,j),n1,val)
         call fill_buf(img(iend+1,j),n2,val)
      enddo

c     ...bottom margin
      do j=jend+1,nl
         call fill_buf(img(1,j),ns,val)
      enddo
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fill margins of a integer*2 image with specified value.
c
      subroutine fill_margins2(img,nl,ns,ibeg,iend,jbeg,jend,val)
      implicit none
      integer*4 nl,ns,ibeg,iend,jbeg,jend,val
      integer*2 img(ns,nl)

      integer*4 j,n1,n2

c     ...top margin
      do j=1,jbeg-1
         call fill_buf2(img(1,j),ns,val)
      enddo

c     ...interior
      n1 = ibeg - 1
      n2 = ns - iend
      do j=jbeg,jend
         call fill_buf2(img(1,j),n1,val)
         call fill_buf2(img(iend+1,j),n2,val)
      enddo

c     ...bottom margin
      do j=jend+1,nl
         call fill_buf2(img(1,j),ns,val)
      enddo
      return
      end
