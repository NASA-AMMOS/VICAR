cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get user parameters for correlation and fit_dy algorithm
c
      subroutine get_params(nlw,nsw,minvar,qthresh,gthresh,tdx,tdy,
     &		print)
      implicit none
      integer*4 nlw,nsw,print
      real*4 minvar			!minimum variance in area
      real*4 qthresh,gthresh		!correlation and gore thresholds
      real*4 tdx,tdy			!surface discontinuity thresholds

      integer cnt
      logical xvptst
      character*80 msg
  101 format('nlw=',i2,' nsw=',i2)
  102 format('vthresh=',f7.2,' qthresh=',f5.2,' gthresh=',f5.2)
  103 format('mindx=',f5.2,' mindy=',f5.2)

      if (xvptst('noprint')) print=0
      if (xvptst('print')) print=1
      if (xvptst('verbose')) print=2

      call xvp('NLW',nlw,cnt)		!correlation window is nlw x nsw
      call xvp('NSW',nsw,cnt)
      nlw = 2*(nlw/2) + 1		!odd number required
      nsw = 2*(nsw/2) + 1
      call xvp('vthresh',minvar,cnt)	!min variance in correlation area
      call xvp('qthresh',qthresh,cnt)   !correlation quality threshold
      call xvp('gthresh',gthresh,cnt)   !gore quality threshold
      call xvp('mindx',tdx,cnt)         !horizontal surface-continuity theshold
      call xvp('mindy',tdy,cnt)         !vertical surface-continuity threshold
      if (print.gt.0) then
         write(msg,101) nlw,nsw
         call xvmessage(msg,' ')
         write(msg,102) minvar,qthresh,gthresh
         call xvmessage(msg,' ')
         write(msg,103) tdx,tdy
         call xvmessage(msg,' ')
      endif
      qthresh = qthresh**2              !to avoid taking square root later
      gthresh = gthresh**2
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

      common/cp/print
      integer*4 print

      integer*4 cnt
      character*80 msg
  102 format('sthresh=',f7.2,' hthresh=',f7.2,' maxpatch=',i5)

      call xvp('avgnw',avgnw,cnt)	!average over nw x nw window
      call xvp('actnw',actnw,cnt)	!activity over nw x nw window
      call xvp('skyact',skyact,cnt)	!sky scene activity threshold
      call xvp('hthresh',hthresh,cnt)	!horizon threshold
      call xvp('maxpatch',parea,cnt)	!max patch pixel area
      if (print.gt.0) then
         write(msg,102) skyact,hthresh,parea
         call xvmessage(msg,' ')
      endif
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get matching window search limits.
c
      subroutine search_limits(ilo,ihi,jlo,jhi)
      implicit none
      integer*4 ilo,ihi,jlo,jhi		!low and high search limits

      integer count,par(2)

c     ...Set maximum search area
      call xvp('hsearch',par,count)     !horizontal search width
      ilo = par(1)
      ihi = par(2)
      call xvp('vsearch',par,count)     !vertical search width
      jlo = par(1)
      jhi = par(2)
      return
      end
