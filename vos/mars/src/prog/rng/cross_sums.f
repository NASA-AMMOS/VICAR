cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compute column sums for nlw lines at bottom of image.
c
      subroutine cross_sums(nl,ns,l,r,ilo,ihi,jlo,jhi,lr,
     &		ibeg,iend,j0,m,n)
      implicit none
      integer*4 nl,ns
      integer*2 l(ns,nl),r(ns,nl)		!inp=(l,r)
      integer*4 ilo,ihi,jlo,jhi
      integer*4 lr(ilo:ihi,jlo:jhi,ns)		!Column sums of cross terms
      integer*4 ibeg,iend,j0,m,n

      integer*4 i,j,il,ih
      integer*4 di,dj,x,y,isum

      do 50 i=ibeg-m,iend+m
      il = max0(i+ilo,1) - i
      ih = min0(i+ihi,ns) - i

      do 40 dj=jlo,jhi
      do 40 di=il,ih
      isum = 0
      do j=j0-n,j0+n
         x = l(i,j)
         y = r(i+di,j+dj)
         isum = isum + x*y
      enddo
      lr(di,dj,i) = isum
   40 continue

   50 continue

      return
      end
