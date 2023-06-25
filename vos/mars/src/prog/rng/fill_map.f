cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fill holes in disparity map.
c
      subroutine fill_map(nl,ns,l,r,q,dx,dy,mask,avgl,avgr,varl,varr,
     &		hrznl,hrznr,ilo,ihi,jlo,jhi,lr,slr,rho)
      implicit none
      integer*4 nl,ns			!image size is nl x ns
      integer*2 l(ns,nl),r(ns,nl)	!left and right images
      real*4 q(ns,nl)			!corr quality (rho**2)
      real*4 dx(ns,nl),dy(ns,nl)	!hori & vert displacements
      integer*2 mask(ns,nl)		!valid range mask
      real*4 avgl(ns,nl),avgr(ns,nl)  	!average of areas
      real*4 varl(ns,nl),varr(ns,nl)	!variance of areas
      integer*2 hrznl(ns),hrznr(ns)	!horizon location
      integer*4 ilo,ihi,jlo,jhi		!search area limits
      integer*4 lr(ilo:ihi,jlo:jhi,ns)	!column sums of cross terms
      real*8 slr(ilo:ihi,jlo:jhi)	!area sums of cross terms
      real*4 rho(ilo:ihi,jlo:jhi,ns)	!correlation coefficients

      common/cq/nlw,nsw,minvar,qthresh,gthresh
      integer*4 nlw,nsw                 !correlation window is nlw x nsw
      real*4 minvar			!minimum variation in area
      real*4 qthresh                    !correlation threshold
      real*4 gthresh			!gore threshold

      common/cb/print
      integer*4 print

      common/climits/nilo,nihi,njlo,njhi
      integer*4 nilo,nihi,njlo,njhi

      integer*4 dj,jmin,jmax		!line index, delta, and limits
      integer*4 i,j,i1,i2,i0,j0,npix
      integer*4 m,n,imin,imax,ibeg,iend
      integer*4 il,ih,jl,jh
      integer*4 h,k,heps,keps
      character*80 msg
  101 format('# of pixels filled by fill_map=',i6)
  102 format('nilo=',i6,' nihi=',i6,' njlo=',i6,' njhi=',i6)

ccc      call xvp('hradius',heps,cnt)
ccc      call xvp('vradius',keps,cnt)
      heps = 2
      keps = 2
      m = nsw/2
      n = nlw/2
      imin = 1 + m
      imax = ns - m
      jmin = 1 + n
      jmax = nl - n
      jmin = max0(jmin,jmin-jlo)
      jmax = min0(jmax,jmax-jhi)
      npix = 0
      nilo = 0
      nihi = 0
      njlo = 0
      njhi = 0

      do 90 j=jmin,jmax
      i = imin				!scan for first valid range on line
      do while (mask(i,j).gt.0 .and. i.le.imax)	!skip over invalid data
         i = i + 1
      enddo
      if (i.gt.imax) goto 90		!no valid range data on this line
c     ...scan for start of gap
   10 continue				!scan for a range gap
      do while (mask(i,j).eq.0 .and. i.le.imax)	!skip over good data
         i = i + 1
      enddo
      if (i.gt.imax) goto 90		!no more gaps on this line
      i1 = i - 1			!valid pixel to left of gap
c     ...scan for next valid range
      do while (mask(i,j).gt.0 .and. i.le.imax)
         i = i + 1
      enddo
      if (i.gt.imax) goto 90		!gap is at right margin
      i2 = i				!valid pixel to right of gap
      il = 1000
      ih = -1000
      jl = 1000
      jh = -1000
      do j0=j-1,j+1
         do i0=i1,i2
            if (mask(i0,j0).eq.0) then
               h = aint(dx(i0,j0))
               k = aint(dy(i0,j0))
               if (h.lt.il) il=h
               if (h.gt.ih) ih=h
               if (k.lt.jl) jl=k
               if (k.gt.jh) jh=k
            endif
         enddo
      enddo
      il = il - heps
      ih = ih + heps
      jl = jl - keps
      jh = jh + keps
      ibeg = i1 + 1
      iend = i2 - 1
      call cross_sums(nl,ns,l,r,il,ih,jl,jh,lr,ibeg,iend,j,m,n)
      do dj=jl,jh
         call correlate(avgl(1,j),avgr(1,j+dj),varl(1,j),varr(1,j+dj),
     &		hrznl,hrznr,ns,lr,slr,rho,il,ih,jl,jh,
     &		minvar,nlw,nsw,ibeg,iend,j,dj,il)
      enddo
      call max_corr(q,dx,dy,mask,nl,ns,rho,il,ih,jl,jh,
     &		gthresh,il,ibeg,iend,j,npix)
      i = i2
      goto 10
   90 continue

      if (print.eq.2) then
         write(msg,101) npix
         call xvmessage(msg,' ')
         write(msg,102) nilo,nihi,njlo,njhi
         call xvmessage(msg,' ')
      endif
      return
      end
