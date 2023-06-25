cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Scale all matches to full resolution.
c
      subroutine scale_matches1(l1b_3,s1b_3,l2max,s2max,
     &		l1b,s1b,l2b,s2b,izmax,lnpts,nl,ns)
      implicit none
c     ...inputs
      integer*4 nl,ns		!full-res image size is nl x ns
      integer*4 lnpts
      integer*2 izmax(lnpts)
      integer*2 l1b_3(lnpts),s1b_3(lnpts)
      integer*2 l2max(lnpts),s2max(lnpts)
c     ...outputs
      integer*2 l1b(lnpts),s1b(lnpts)
      integer*2 l2b(lnpts),s2b(lnpts)

      integer*4 i,iz,izoom,cnt
      integer*4 l,s,dn,maxdn
      real*4 scale,scales(5)/1.,2.,4.,8.,16./
      real*4 lcenter(5),scenter(5)

      do iz=1,5
	 izoom = 2**iz
	 lcenter(iz) = nl/izoom + 0.5
	 scenter(iz) = ns/izoom + 0.5
      enddo

      do i=1,lnpts
         l1b(i) = 4.*(l1b_3(i) - lcenter(3)) + lcenter(1)
         s1b(i) = 4.*(s1b_3(i) - scenter(3)) + scenter(1)
         iz = izmax(i)
         scale = scales(iz)
         l2b(i) = scale*(l2max(i) - lcenter(iz)) + lcenter(1)
         s2b(i) = scale*(s2max(i) - scenter(iz)) + scenter(1)
      enddo
      return
      end
