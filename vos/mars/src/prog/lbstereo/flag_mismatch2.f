ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Flag all candidates at an isolated iz, is, or it. 
c
      subroutine flag_mismatch2(lnpts,rhomax,izmax,ismax,itmax,flag,
     &		qthresh)
      implicit none
      integer*4 lnpts
      real*4 rhomax(lnpts)
      integer*2 izmax(lnpts)		!izmax(i)=iz
      integer*2 ismax(lnpts)		!ismax(i)=is
      integer*2 itmax(lnpts)		!itmax(i)=it
      integer*2 flag(lnpts)		!0=good candidate
      real*4 qthresh

      integer*4 i,j,iz,is,it
      integer*4 nfreq,nsteps
      real*4 rmax
      integer*2 histz(5),hists(-4:4),histt(360)
      character*80 msg
  101 format(5i5)
  102 format(9i5)
  109 format(i4,i5,i7)

      do i=1,lnpts
         if (rhomax(i).ge.qthresh) then
            flag(i) = 0
         else
            flag(i) = 1
         endif
      enddo

      call get_hist(lnpts,izmax,ismax,itmax,rhomax,flag,
     &		histz,hists,histt)

      nsteps = 0
      do 20 iz=1,5
      nfreq = histz(iz)
      if (nfreq.eq.0) goto 20
      nsteps = nsteps + 1
      if (nfreq.gt.2) goto 20
      histz(iz) = 0
      do i=1,lnpts
         if (flag(i).eq.0 .and. izmax(i).eq.iz) flag(i)=16
      enddo 
   20 continue
     
      if (nsteps.gt.1) goto 35

      do 30 is=-4,4
      nfreq = hists(is)
      if (nfreq.eq.0) goto 30
      if (nfreq.gt.2) goto 30
      hists(is) = 0
      do i=1,lnpts
         if (flag(i).eq.0 .and. ismax(i).eq.is) flag(i)=32
      enddo 
   30 continue
     
   35 continue

      do 40 it=1,360
      nfreq = histt(it)
      if (nfreq.eq.0) goto 40
      if (nfreq.gt.2) goto 40
      hists(it) = 0
      do i=1,lnpts
         if (flag(i).eq.0 .and. itmax(i).eq.it) flag(i)=32
      enddo 
   40 continue
     
      call get_hist(lnpts,izmax,ismax,itmax,rhomax,flag,
     &		histz,hists,histt)
      return
      end
