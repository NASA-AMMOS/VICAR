cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get size of correlation window (nlw x nsw).
c
      subroutine window_size(nlw,nsw)
      implicit none
      integer*4 nlw,nsw		!output correlation window size

      integer cnt
      character*80 msg
  101 format('nlw=',i2,' nsw=',i2)

      call xvp('NLW',nlw,cnt)		!correlation window is nlw x nsw
      call xvp('NSW',nsw,cnt)
      nlw = 2*(nlw/2) + 1		!odd number required
      nsw = 2*(nsw/2) + 1
      write(msg,101) nlw,nsw
      call xvmessage(msg,' ')
      return
      end
