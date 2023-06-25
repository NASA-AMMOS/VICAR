ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Print the iz, is, and it histograms. 
c
      subroutine print_hist0(isbuf,itbuf,flag,npts)
      implicit none
      integer*4 npts
      integer*2 isbuf(npts),itbuf(npts),flag(npts)

      common/ct/ct,st,deglo,deghi,ddeg,nsteps
      real*8 ct(360),st(360)
      integer*4 deglo,deghi,ddeg,nsteps

      integer*4 i,j
      integer*4 is,it
      integer*4 nfreq,deg
      integer*2 hists(-4:4),histt(360)
      character*80 msg
  102 format(' ',9i4)
  109 format(i4,i5,i7)

      do is=-4,4
         hists(is) = 0
      enddo
      do it=1,nsteps
         histt(it) = 0
      enddo

      do 10 i=1,npts
      if (flag(i).ne.0) goto 10
      is = isbuf(i)
      it = itbuf(i)
      hists(is) = hists(is)+1
      histt(it) = histt(it)+1
   10 continue

      call xvmessage(' ',' ')
      call xvmessage('is=-4  -3  -2  -1   0   1   2   3   4',' ')
      write(msg,102) (hists(j),j=-4,4)
      call xvmessage(msg,' ')

      call xvmessage(' ',' ')
      call xvmessage('  it  deg  freq',' ')
      do it=1,nsteps
         nfreq = histt(it)
         if (nfreq.gt.0) then
            deg = (it-1)*ddeg + deglo
            write(msg,109) it,deg,nfreq
            call xvmessage(msg,' ')
         endif
      enddo
      return
      end
