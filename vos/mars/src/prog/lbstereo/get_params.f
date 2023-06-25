cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get user parameters for correlation and fit_dy algorithm
c All arguments are outputs.
c
      subroutine get_params(nlw,nsw,dl,ds,vthresh,qthresh)
      implicit none
      integer*4 nlw,nsw		!correlation window is nlw x nsw
      integer*4 dl,ds		!vertical and horizontal search radius
      real*4 vthresh		!variance threshold
      real*4 qthresh		!correlation threshold

      common/cp/debug,print
      logical*1 debug,print

      integer cnt
      logical xvptst

      debug = xvptst('debug')
      print = xvptst('print')

      call xvp('NLW',nlw,cnt)		!correlation window is nlw x nsw
      call xvp('NSW',nsw,cnt)
      nlw = 2*(nlw/2) + 1		!odd number required
      nsw = 2*(nsw/2) + 1

      call xvp('dl',dl,cnt)
      call xvp('ds',ds,cnt)

      call xvp('sthresh',vthresh,cnt)   !variance threshold
      vthresh = vthresh**2              !to avoid taking square root later

      call xvp('qthresh',qthresh,cnt)   !correlation quality threshold
      qthresh = qthresh**2              !to avoid taking square root later
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine get_params2(izlo,izhi,iclo,ichi,nsteps2)
      implicit none
      integer*4 izlo,izhi		!major picture scale limits
      integer*4 iclo,ichi		!minor picture scale limits
      integer*4 nsteps2

      common/cs/cm
      real*8 cm(-4:4)

      common/ct/ct,st,deglo,deghi,ddeg,nsteps
      real*8 ct(360),st(360)
      integer*4 deglo,deghi,ddeg,nsteps

      integer*4 par(3),cnt
      integer*4 ic,it,dlength,deg
      real*8 Cminor,base,t
      real*8 pi/3.141592654/
      character*80 msg
  104 format(i3,f10.7)

      call xvp('iz',par,cnt)
      izlo = par(1)
      izhi = par(2)
      write(msg,1002) izlo,izhi
      call xvmessage(msg,' ')
 1002 format('izlo=',i1,'  izhi=',i1)

      call xvp('ic',par,cnt)
      iclo = par(1)
      ichi = par(2)
      write(msg,1003) iclo,ichi
      call xvmessage(msg,' ')
 1003 format('iclo=',i2,'  ichi=',i2)

      call xvmessage(' ic   Cminor',' ')
      base = 2.**(1./9.)
      do ic=iclo,ichi
         Cminor = base**ic
         write(msg,104) ic,Cminor
         call xvmessage(msg,' ')
         cm(ic) = Cminor
      enddo

      call xvp('angle',par,cnt)
      deglo = par(1)
      deghi = par(2)
      ddeg = par(3)

      if (deglo.gt.deghi) call mabend('***angle parameter err')
      dlength = deghi - deglo
      nsteps = dlength/ddeg + 1
      write(msg,1001) deglo,deghi,ddeg,nsteps
      call xvmessage(msg,' ')
 1001 format('angle=',i4,' to',i4,' dtheta=',i3,' nsteps=',i3)

      do it=1,nsteps
         deg = (it-1)*ddeg + deglo
         t = deg*pi/180.d0
         ct(it) = dcos(t)
         st(it) = dsin(t)
      enddo

      nsteps2 = nsteps
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get sky and horizon parameters.
c
      subroutine sky_params(avgnw,actnw,skyact,hthresh,parea)
      implicit none
      integer*4 avgnw,actnw	!average and activity window sizes
      real*4 skyact,hthresh	!sky and horizon thresholds
      integer*4 parea		!maximum patch area (pixels)

      common/cp/debug,print
      logical*1 debug,print

      integer*4 cnt
      character*80 msg
  102 format('sthresh=',f7.2,' hthresh=',f7.2,' maxpatch=',i5)

      call xvp('avgnw',avgnw,cnt)	!average over nw x nw window
      call xvp('actnw',actnw,cnt)	!activity over nw x nw window
      call xvp('skyact',skyact,cnt)	!sky scene activity threshold
      call xvp('hthresh',hthresh,cnt)	!horizon threshold
      call xvp('maxpatch',parea,cnt)	!max patch pixel area
      if (print) then
      	 write(msg,102) skyact,hthresh,parea
      	 call xvmessage(msg,' ')
      endif
      return
      end
