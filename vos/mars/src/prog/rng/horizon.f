ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Find location of horizon, 
c returning its line number for each sample: hrzn(ns)
c
      subroutine horizon(k,img,nl,ns,act,avg,hrzn)
      implicit none
      integer*4 k			!k=1 for left, =2 for right
      integer*4 nl,ns			!image is nl x ns
      integer*2 img(ns,nl)		!image (left or right)
      real*4 act(ns,nl),avg(ns,nl)  	!scratch space
      integer*2 hrzn(ns)		!output horizon location

      integer*4 m,n,i,imin,imax,cnt
      integer*4 ibeg,iend,jbeg,jend
      integer*4 avgnw,actnw,parea
      real*4 skyact,hthresh
      character*132 file(2)

      do i=1,ns
         hrzn(i) = 0
      enddo
      call sky_params(avgnw,actnw,skyact,hthresh,parea)
      if (skyact.le.0) return		!skip horizon location
      call compute_avg(img,avg,nl,ns,avgnw,avgnw)
      m = avgnw/2
      imin = 1 + m
      imax = ns - m - 1
      n = avgnw/2 + actnw/2
      ibeg = 1 + n
      jbeg = 1 + n
      iend = ns - n - 1
      jend = nl - n - 1
      call compute_sig(avg,act,nl,ns,avgnw,avgnw)
ccc      call activity(avg,act,nl,ns,actnw,actnw,imin,imax,
ccc     &		ibeg,iend,jbeg,jend)
      call xvp('ACTIVITY',file,cnt)
      if (cnt.eq.2) call output_image(act,nl,ns,file(k),k)
      call clean_patches(act,nl,ns,ibeg,iend,jbeg,jend,skyact,parea)
      call find_horizon(act,nl,ns,ibeg,iend,jbeg,jend,
     &		hrzn,skyact,hthresh,parea)
      call xvp('HORIZON',file,cnt)
      if (cnt.eq.2) call write_horizon(img,hrzn,nl,ns,file(k),k)
      return
      end
