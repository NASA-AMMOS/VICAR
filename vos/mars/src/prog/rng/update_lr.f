ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Update column sums of cross terms for next line.
c
      subroutine update_lr(l,r,nl,ns,ilo,ihi,jlo,jhi,lr,ilox,j,nlw)
      implicit none
      integer*4 nl,ns
      integer*2 l(ns,nl),r(ns,nl)		!Input images
      integer*4 ilo,ihi,jlo,jhi
      integer*4 lr(ilo:ihi,jlo:jhi,ns)		!Output column sums
      integer*4 ilox,j,nlw

      integer*4 i,i2,j0,j1,di,dj,il,ih
      integer*4 dn,dn0,n,n1

      n = nlw/2
      n1 = n + 1

      do 60 i=1,ns
      il = max0(i+ilo,1) - i
      ih = min0(i+ihi,ns) - i

      do 60 dj=jlo,jhi
      j0 = j + n
      j1 = j - n1
      dn0 = l(i,j0)
      dn  = l(i,j1)
      do di=il,ih
         i2 = i + di
         lr(di,dj,i) = lr(di,dj,i) - dn0*r(i2,j0+dj)
     &                             +  dn*r(i2,j1+dj)
      enddo
   60 continue

      return
      end
