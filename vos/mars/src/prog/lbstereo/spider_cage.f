cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Build a spider cage by encircling the image margins with large DN values.
c This prevents the spider from wandering off the image.
c
      subroutine spider_cage(act,nl,ns,ibeg,iend,jbeg,jend,dn)
      implicit none
      integer*4 nl,ns,ibeg,iend,jbeg,jend
      real*4 act(ns,nl),dn

      integer*4 i,j,i1,i2

      j = jbeg - 1
      i1 = ibeg - 1
      i2 = iend + 1
      do i=i1,i2
         act(i,j) = dn
      enddo
      do j=jbeg,jend
         act(i1,j) = dn
         act(i2,j) = dn
      enddo
      j = jend + 1
      do i=i1,i2
         act(i,j) = dn
      enddo
      return
      end
