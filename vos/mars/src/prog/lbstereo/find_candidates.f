cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Scan the image for candidate tiepoints.
c Candidates are local-variance maxima.
c 
      subroutine find_candidates(var,nl,ns,dl,ds,
     &		lbuf,sbuf,nmax,npts)
      implicit none
c     ...input arguments
      integer*4 nl,ns			!image size is nl x ns	
      real*4 var(ns,nl)
      integer*4 dl,ds			!vertical and horizontal search radius
      integer*4 nmax			!maximum number of candidates
c     ...output arguments
      integer*2 lbuf(nmax),sbuf(nmax)	!(l,s) coordinates of candidates
      integer*4 npts			!number of candidates found

c     ...column stats for sliding-sum algorithm
      common/ctemp/cmax,lmax
      real*4 cmax(1024)			!max variance on column s
      integer*2 lmax(1024)		!line # where max occurs on column s

      integer*4 lbeg,lend,sbeg,send
      integer*4 l,s,ll,ss,l0,s0,cnt
      real*4 vthresh			!variance threshold

      call get_vthresh(var,nl,ns,vthresh)

      lbeg = dl + 1
      lend = nl - dl
      sbeg = ds + 1
      send = ns - ds

      do s=1,ds		!no variance data along left and right margins
         lmax(s) = 1
         cmax(s) = 0.
         lmax(ns-s+1) = ns
         cmax(ns-s+1) = 0.
      enddo

c     ...loop through each pixel (s0,l0) in the image
      npts = 0

      do 30 l0=lbeg,lend 

      do s=sbeg,send		!find column maxima
         l = l0 - dl
	 do ll=l0-dl,l0+dl
            if (var(s,ll).gt.var(s,l)) l=ll
	 enddo
         lmax(s) = l
         cmax(s) = var(s,l)
      enddo

      do 30 s0=sbeg,send
      s = s0 - ds
      do ss=s0-ds,s0+ds
         if (cmax(ss).gt.cmax(s)) s=ss
      enddo
      l = lmax(s) 		!local vax maximum is at (l,s)
      if (cmax(s0).lt.vthresh) goto 30
      if (l.le.lbeg .or. l.ge.lend) goto 30
      if (s.le.sbeg .or. s.ge.send) goto 30
      if (var(s-1,l).eq.0. .or. var(s+1,l).eq.0.) goto 30
      if (var(s,l-1).eq.0. .or. var(s,l+1).eq.0.) goto 30
      if (l.eq.l0 .and. s.eq.s0) then
         npts = npts + 1
         lbuf(npts) = l0
         sbuf(npts) = s0
         if (npts.ge.nmax) then
            call xvmessage('***max candidates exceeded',' ')
            return
         endif
      endif
   30 continue

      return
      end 
