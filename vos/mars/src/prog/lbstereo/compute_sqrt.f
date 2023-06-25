ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Given variance, compute sigma.
c
      subroutine compute_sqrt(var,sig,nl,ns)
      implicit none
      integer*4 nl,ns
      real*4 var(ns,nl)		!input
      real*4 sig(ns,nl)		!output

      integer*4 i,j
      real*4 r

      do j=1,nl
         do i=1,ns
            r = var(i,j)
            if (r.gt.0.) then
               sig(i,j) = sqrt(r)
            else
               sig(i,j) = 0.
            endif
         enddo
      enddo

      return
      end
