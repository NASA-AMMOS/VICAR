ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine ranger0(nl,ns,l,r,hrznl,hrznr,
     &		q,dx,dy,mask,avgl,avgr,varl,varr,
     &		ilo,ihi,jlo,jhi,lr,slr,rho)
      implicit none
c     ...Input arguments
      integer*4 nl,ns			!image size is nl x ns
      integer*2 l(ns,nl),r(ns,nl)	!left and right images
      integer*2 hrznl(ns),hrznr(ns)	!horizon location
c     ...Outputs
      real*4 q(ns,nl)			!corr quality (rho**2)
      real*4 dx(ns,nl),dy(ns,nl)	!hori & vert displacements
      integer*2 mask(ns,nl)		!valid range mask
      real*4 avgl(ns,nl),avgr(ns,nl)  	!average of areas
      real*4 varl(ns,nl),varr(ns,nl)	!variance of areas
c     ...Work buffers
      integer*4 ilo,ihi,jlo,jhi		!search area limits
      integer*4 lr(ilo:ihi,jlo:jhi,ns)	!column sums of cross terms
      real*8 slr(ilo:ihi,jlo:jhi)	!area sums of cross terms
      real*4 rho(ilo:ihi,jlo:jhi,ns)	!correlation coefficients

      common/cq/nlw,nsw,minvar,qthresh
      integer*4 nlw,nsw                 !correlation window is nlw x nsw
      real*4 minvar			!minimum variation in area
      real*4 qthresh                    !correlation threshold

      common/cb/print
      integer*4 print

      common/climits/nilo,nihi,njlo,njhi
      integer*4 nilo,nihi,njlo,njhi

      integer*4 ibeg,iend,jbeg,jend
      integer*4 j,dj			!line index, delta
      integer*4 m,n,ilox,npix
      character*80 msg
  101 format('line=',i4,' ilox=',i5)
  102 format('nilo=',i6,' nihi=',i6,' njlo=',i6,' njhi=',i6)

c     ...initialize the buffers to an invalid value
      call fill_buf(q,nl*ns,-1.0)
      call fill_buf(dx,nl*ns,-999.)
      call fill_buf(dy,nl*ns,-999.)
      call fill_buf2(mask,nl*ns,1)
      nilo = 0
      nihi = 0
      njlo = 0
      njhi = 0

c     ...compute the mean and variance images over an nlw x nsw window
      call astats(l,avgl,varl,nl,ns,nlw,nsw)
      call astats(r,avgr,varr,nl,ns,nlw,nsw)
      m = nsw/2
      n = nlw/2
      ibeg =  1 + m
      iend = ns - m
      jbeg =  1 + n
      jend = nl - n
      jbeg = max0(jbeg,jbeg-jlo)
      jend = min0(jend,jend-jhi)

c     ...compute cross sum l(i,j)*r(i+h,j+k) for reference line jend
      call cross_sums(nl,ns,l,r,ilo,ihi,jlo,jhi,lr,
     &		ibeg,iend,jend,m,n)
      ilox = ilo
c
c     ...line loop begins at bottom of image and ends at top.
      do 50 j=jend,jbeg,-1
      if (mod(j,10).eq.0 .and. print.gt.0) then
         write(msg,101) j,ilox
         call xvmessage(msg,' ')
      endif
      do dj=jlo,jhi
         call correlate(avgl(1,j),avgr(1,j+dj),varl(1,j),varr(1,j+dj),
     &		hrznl,hrznr,ns,lr,slr,rho,ilo,ihi,jlo,jhi,
     &		minvar,nlw,nsw,ibeg,iend,j,dj,ilox)
      enddo
      call max_corr(q,dx,dy,mask,nl,ns,rho,ilo,ihi,jlo,jhi,
     &		qthresh,ilox,ibeg,iend,j,npix)
      if (j.eq.jbeg) goto 50
      call update_hsearch(q(1,j),dx(1,j),ns,qthresh,m,ilox)
      call update_lr(l,r,nl,ns,ilo,ihi,jlo,jhi,lr,ilox,j,nlw)
   50 continue

      if (print.eq.2) then
         write(msg,102) nilo,nihi,njlo,njhi
         call xvmessage(msg,' ')
      endif
      return
      end
