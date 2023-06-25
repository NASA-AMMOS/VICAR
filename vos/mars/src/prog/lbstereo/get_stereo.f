ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Read stereo pair into l0 and r0, with optional low-pass filter.
c
      subroutine get_stereo(iunit,nl,ns,l0,r0,buf)
      implicit none
      integer*4 iunit(2),nl,ns		!inputs
      integer*2 l0(ns,nl),r0(ns,nl)	!outputs
      integer*2 buf(ns,nl)		!work buffer

      integer*4 fnlw,fnsw,cnt
      logical xvptst

      if (xvptst('filter')) then
         call xvp('fnlw',fnlw,cnt)
         call xvp('fnsw',fnsw,cnt)
         call read_inp(iunit(1),nl,ns,buf)               !integer*2 l(ns,nl)
         call compute_avg2(buf,l0,nl,ns,fnlw,fnsw)
         call read_inp(iunit(2),nl,ns,buf)               !integer*2 r(ns,nl)
         call compute_avg2(buf,r0,nl,ns,fnlw,fnsw)
      else
         call read_inp(iunit(1),nl,ns,l0)                !integer*2 l(ns,nl)
         call read_inp(iunit(2),nl,ns,r0)                !integer*2 r(ns,nl)
      endif
      return
      end
