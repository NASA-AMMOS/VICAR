ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compute the iz, is, and it histograms. 
c
      subroutine get_hist(npts,izmax,ismax,itmax,rhomax,flag,
     &		histz,hists,histt)
      implicit none
c     ...inputs
      integer*4 npts
      integer*2 izmax(npts),ismax(npts),itmax(npts)
      real*4 rhomax(npts)
c     ...outputs
      integer*2 histz(5),hists(-4:4),histt(360)
      integer*2 flag(npts)

      common/ct/ct,st,deglo,deghi,ddeg
      real*8 ct(360),st(360)
      integer*4 deglo,deghi,ddeg

      integer*4 i,j
      integer*4 iz,is,it
      integer*4 nfreq,deg
      character*80 msg
  101 format(5i5)
  102 format(' ',9i4)
  109 format(i4,i5,i7)

      do iz=1,5
         histz(iz) = 0
      enddo
      do is=-4,4
         hists(is) = 0
      enddo
      do it=1,360
         histt(it) = 0
      enddo

      do 10 i=1,npts
      if (flag(i).ne.0) goto 10
      iz = izmax(i)
      is = ismax(i)
      it = itmax(i)
      histz(iz) = histz(iz)+1
      hists(is) = hists(is)+1
      histt(it) = histt(it)+1
   10 continue

      call xvmessage(' ',' ')
      call xvmessage('iz= 1    2    3    4    5',' ')
      write(msg,101) (histz(j),j=1,5)
      call xvmessage(msg,' ')

      call xvmessage(' ',' ')
      call xvmessage('is=-4  -3  -2  -1   0   1   2   3   4',' ')
      write(msg,102) (hists(j),j=-4,4)
      call xvmessage(msg,' ')

      call xvmessage(' ',' ')
      call xvmessage('  it  deg  freq',' ')
      do it=1,360
         nfreq = histt(it)
         if (nfreq.gt.0) then
            deg = (it-1)*ddeg + deglo
            write(msg,109) it,deg,nfreq
            call xvmessage(msg,' ')
         endif
      enddo
      return
      end
