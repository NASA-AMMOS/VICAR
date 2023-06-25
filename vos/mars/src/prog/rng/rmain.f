ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine rmain(slr,m1,lr,m2,rho,m3,avgl,n1,avgr,n2,
     &	varl,n3,varr,n4,q,n5,dx,n6,dy,n7,region,n8,l,n9,r,n10,
     &  dx0,n11,dy0,n12,l0,n13,r0,n14,mask,n15,
     &	nl,ns,ilo,ihi,jlo,jhi)
      implicit none
      integer*4 m1,m2,m3	!# of bytes allocated for correlation matrix
      integer*4 n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15  !# bytes

      integer*4 nl,ns				!image size is nl x ns
      integer*2 l(ns,nl),r(ns,nl)		!inp=(l,r)
      real*4 dx(ns,nl),dy(ns,nl)		!out=(dx,dy) 
      real*4 q(ns,nl)				!q=q
      integer*2 mask(ns,nl)		        !mask=mask
      real*4 dx0(ns/2,nl/2),dy0(ns/2,nl/2)	!lo-res dx,dy 
      integer*2 l0(ns/2,nl/2),r0(ns/2,nl/2)	!lo-res l,r

      real*4 avgl(ns,nl),avgr(ns,nl)  	!average of areas
      real*4 varl(ns,nl),varr(ns,nl)	!variance of areas
      integer*4 region(ns,nl)		!disparity surface regions

      integer*4 ilo,ihi,jlo,jhi		!search area limits
      real*8 slr(ilo:ihi,jlo:jhi)	!area sums of cross terms
      integer*4 lr(ilo:ihi,jlo:jhi,ns)	!column sums of cross terms
      real*4 rho(ilo:ihi,jlo:jhi,ns)	!correlation quality in search area

      common/c0/iunit				!inp=(x,y) logical units
      integer*4 iunit(2)			!inputs from calling program

      integer*4 maxnl,maxns
      parameter (maxnl=1024,maxns=1024)
      integer*2 hrznl(maxns),hrznr(maxns)	!horizon location
      integer*2 hrznl0(maxns/2),hrznr0(maxns/2)	!lo-res horizon location

      integer*4 nl0,ns0,ilo0,ihi0,jlo0,jhi0
      integer*4 level0,level,zoom,cnt

      if (n15.ne.2*nl*ns) call mabend('***Insufficient memory',' ')
c     ...read in the stereo pair:  inp=(l,r)
      call read_inp(iunit(1),nl,ns,l)		!integer*2 l(ns,nl)
      call read_inp(iunit(2),nl,ns,r)		!integer*2 r(ns,nl)

c     ...optionally locate the horizon in each image.
      call horizon(1,l,nl,ns,avgl,varl,hrznl)
      call horizon(2,r,nl,ns,avgr,varr,hrznr)

      call xvp('PYRAMID',level0,cnt)	!initial pyramid level
      level = level0

      do while (level.ge.0)
         call prnt(4,1,level,'pyramid level=.')
         zoom = 2**level
         if (level.eq.0) then
            if (level0.eq.0) then
               call ranger0(nl,ns,l,r,hrznl,hrznr,
     &	          q,dx,dy,mask,avgl,avgr,varl,varr,
     &            ilo,ihi,jlo,jhi,lr,slr,rho)
            else
               call ranger1(nl,ns,l,r,dx0,dy0,hrznl,hrznr,
     &            q,dx,dy,mask,avgl,avgr,varl,varr,rho)
            endif
            call rdespike(nl,ns,dx,dy,mask,region,zoom)
         else
            call zoomout(l,l0,nl,ns,zoom) 
            call zoomout(r,r0,nl,ns,zoom) 
            call zoomhrzn(hrznl,hrznr,hrznl0,hrznr0,ns,zoom)
            nl0 = nl/zoom
            ns0 = ns/zoom
            if (level.eq.level0) then
               ilo0 = (ilo-0.5*zoom)/zoom - 1
               ihi0 = (ihi+0.5*zoom)/zoom + 1
               jlo0 = (jlo-0.5*zoom)/zoom - 1
               jhi0 = (jhi+0.5*zoom)/zoom + 1
               call ranger0(nl0,ns0,l0,r0,hrznl0,hrznr0,
     &   		q,dx,dy,mask,avgl,avgr,varl,varr,
     &   		ilo0,ihi0,jlo0,jhi0,lr,slr,rho)
            else
               call ranger1(nl0,ns0,l0,r0,dx0,dy0,hrznl0,hrznr0,
     &	         	q,dx,dy,mask,avgl,avgr,varl,varr,rho)
            endif
            call rdespike(nl0,ns0,dx,dy,mask,region,zoom)
            call rcopy(nl0,ns0,dx,dy,dx0,dy0,mask)
         endif
         level = level - 1
      enddo

      call fill_map(nl,ns,l,r,q,dx,dy,mask,avgl,avgr,varl,varr,
     &		hrznl,hrznr,ilo,ihi,jlo,jhi,lr,slr,rho)
      call fill_map1(nl,ns,l,r,q,dx,dy,mask,avgl,avgr,varl,varr,
     &		hrznl,hrznr,rho)
      call rdespike(nl,ns,dx,dy,mask,region,zoom)

c     ...output images 
  100 call write_out(dx,dy,mask,nl/zoom,ns/zoom)	!out=(dx,dy)
      call write_q(q,nl/zoom,ns/zoom)			!q=(q1,q2,q3)
      call write_mask(mask,nl/zoom,ns/zoom)		!mask=(m1,m2,m3)
      call write_avg(avgl,avgr,nl/zoom,ns/zoom)		!avereage=(avgl,avgr)
      call write_var(varl,varr,nl/zoom,ns/zoom)		!variance=(varl,varr)
      call write_merout(dx,dy,mask,nl,ns)		!merout=mer
      return
      end
