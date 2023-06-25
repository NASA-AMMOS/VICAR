ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Flag candidates with small rho. 
c
      subroutine flag_rho(lnpts,rhomax,qthresh,flag)
      implicit none
c     ...inputs
      integer*4 lnpts
      real*4 rhomax(lnpts)
      real*4 qthresh
c     ...output
      integer*2 flag(lnpts)		!0=good candidate

      integer*4 i

      do i=1,lnpts
         if (rhomax(i).ge.qthresh) then
            flag(i) = 0
         else
            flag(i) = 1
         endif
      enddo

      return
      end
