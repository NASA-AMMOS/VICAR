      subroutine image_displays(l,r,avg,var,buf,nl,ns,lbuf,sbuf,nmax,
     &		nlw,nsw,dl,ds)
      implicit none
c     ...inpute
      integer*4 nl,ns,nmax	
      integer*2 l(ns,nl),r(ns,nl)
      integer*4 nlw,nsw			!window is nlw x nsw
      integer*4 dl,ds			!vertical and horizontal search radius
c     ...scratch buffers
      real*4 avg(ns,nl),var(ns,nl),buf(ns,nl)
      integer*2 lbuf(nmax),sbuf(nmax)	!(l,s) coords of candidates

      integer*4 npts,k
      integer*4 cnt1,cnt2,cnt3
      character*256 file1(2),file2(2),file3(2)
      character*80 msg


      call xvp('avg',file1,cnt1)
      call xvp('sig',file2,cnt2)
      call xvp('cdisp',file3,cnt3)

      if (cnt1.lt.2 .and. cnt2.lt.2 .and. cnt3.lt.2) return

      do 10 k=1,2
      if (k.eq.1) call compute_stats(l,avg,var,nl,ns,nlw,nsw)
      if (k.eq.2) call compute_stats(r,avg,var,nl,ns,nlw,nsw)
      if (cnt1.eq.2) call output_image(avg,nl,ns,file1(k),k)
      if (cnt2.eq.2) then
         call compute_sig(var,buf,nl,ns)
         call output_image(buf,nl,ns,file2(k),k)
      endif
      if (cnt3.eq.2) then
         call find_candidates(var,nl,ns,dl,ds,
     &		lbuf,sbuf,nmax,npts)
         call display_candidates(lbuf,sbuf,npts,
     &          buf,nl,ns,file3(k),k)
      endif
   10 continue

      return
      end
