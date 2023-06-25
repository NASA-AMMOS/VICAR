cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Match between line j of left image and j+dj of right image.
c Output is rho(ilo:ihi,jlo:jhi) computed for a segment ibeg to iend
c of line j.
c
      subroutine correlate(avgl,avgr,varl,varr,
     &		hrznl,hrznr,ns,lr,slr,rho,ilo,ihi,jlo,jhi,
     &		minvar,nlw,nsw,ibeg,iend,j,dj,ilox)
      implicit none
      integer*4 ns			!image width (number of samples)
      real*4 avgl(ns),avgr(ns)	 	!mean of DNs in area
      real*4 varl(ns),varr(ns)		!variance for areas
      integer*2 hrznl(ns),hrznr(ns)	!horizon line coordinate
      integer*4 ilo,ihi,jlo,jhi		!search area limits
      integer*4 lr(ilo:ihi,jlo:jhi,ns)	!Column sums of cross terms
      real*4 rho(ilo:ihi,jlo:jhi,ns)	!Correlation coefficients**2
      real*8 slr(ilo:ihi)		!sums of cross terms in area
      real*4 minvar			!min variance in correlation area
      integer*4 nlw,nsw			!correlation window size
      integer*4 ibeg,iend,j,dj,ilox	!line, delta line

      integer*4 m,m1,i,i2,di
      real*8 area,sum,cov,rr
  101 format(4i8,f10.6)

      area = nlw*nsw	!total number of pixels in window
      m = nsw/2
      m1 = m + 1


c     ...compute sum of lr terms for left-most window
      do di=ilox,ihi
         sum = 0.
         do i=-m,m		!sum up the column sums
            sum = sum + lr(di,dj,ibeg+i)   !lr's may be garbage at margins
         enddo
         slr(di) = sum
      enddo

c     ...compute rho for each pixel between left and right image margins
c     ...at every pixel location within ilo or ihi of the left pixel.
      do 50 i=ibeg,iend	
      if (hrznl(i).gt.j .or. varl(i).lt.minvar) then
         do di=ilox,ihi
            rho(di,dj,i) = -1.0
         enddo
         goto 30
      endif

      do 20 di=ilox,ihi		!scan over right image for match of pixel i
      rr = -1.0
      i2 = i + di
      if (i2.lt.1 .or. i2.gt.ns) goto 20	!skip if outside image
      if (hrznr(i2).gt.j+dj) goto 20		!skip if right pixel in horizon
      if (varr(i2).lt.minvar) goto 20		!skip if no info in rght window 
      cov = slr(di)/area - avgl(i)*avgr(i2)
      rr = (cov*cov)/(varl(i)*varr(i2))		!see Ref 1
      if (cov.le.0.) rr=-rr
   20 rho(di,dj,i) = rr

c     ...update the lr sums for the next pixel using sliding sum
   30 if (i.ge.iend) goto 50
      do di=ilox,ihi
         slr(di) = slr(di) - lr(di,dj,i-m) + lr(di,dj,i+m1)
      enddo
   50 continue

      return
      end
