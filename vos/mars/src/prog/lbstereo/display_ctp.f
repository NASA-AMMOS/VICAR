cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine display_ctp(l1buf,s1buf,lnpts,l0,buf,nl,ns)
      implicit none
c     ...inputs
      integer*4 lnpts
      integer*2 l1buf(lnpts),s1buf(lnpts)
      integer*4 nl,ns
      integer*2 l0(ns,nl)
c     ...scratch buffer
      integer*2 buf(ns,nl)

      integer*4 i,cnt
      integer*4 l,s,dn,maxdn
      character*256 file

      call xvp('ctpdisp',file,cnt)
      if (cnt.ne.1) return

      maxdn = 0
      do l=1,nl
         do s=1,ns
            dn = l0(s,l)
            buf(s,l) = dn
            if (dn.gt.maxdn) maxdn=dn
         enddo
      enddo

      do i=1,lnpts
         s = s1buf(i)
         l = l1buf(i)
         buf(s,l) = maxdn
         buf(s-2,l) = maxdn
         buf(s+2,l) = maxdn
         buf(s,l-2) = maxdn
         buf(s,l+2) = maxdn
      enddo
      call output_image2(buf,nl,ns,file,1)
      return
      end
