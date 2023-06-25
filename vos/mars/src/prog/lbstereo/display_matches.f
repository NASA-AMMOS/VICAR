cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine display_matches(l1buf,s1buf,l2buf,s2buf,flag,lnpts,
     &		l0,r0,buf,nl,ns)
      implicit none
c     ...inputs
      integer*4 lnpts
      integer*2 l1buf(lnpts),s1buf(lnpts)
      integer*2 l2buf(lnpts),s2buf(lnpts)
      integer*2 flag(lnpts)
      integer*4 nl,ns
      integer*2 l0(ns,nl),r0(ns,nl)
c     ...scratch buffer
      integer*2 buf(ns,nl)

      integer*4 i,j,cnt
      integer*4 l,s,dn,maxdn
      character*256 file(2)

      call xvp('tdisp',file,cnt)
      if (cnt.ne.2) return

      maxdn = 0
      do l=1,nl
         do s=1,ns
            dn = l0(s,l)
            buf(s,l) = dn
            if (dn.gt.maxdn) maxdn=dn
         enddo
      enddo

      do i=1,lnpts
         l = l1buf(i)
         s = s1buf(i)
         j = i
         if (flag(i).eq.1) j=0
         if (flag(i).eq.2) j=-1
         call mark_pt(l,s,j,maxdn,buf,nl,ns)
      enddo
      call output_image2(buf,nl,ns,file(1),1)

      maxdn = 0
      do l=1,nl
         do s=1,ns
            dn = r0(s,l)
            buf(s,l) = dn
            if (dn.gt.maxdn) maxdn=dn
         enddo
      enddo

      do i=1,lnpts
         if (flag(i).ne.1) then
            s = s2buf(i)
            l = l2buf(i)
            j = i
            if (flag(i).eq.2) j=-1
            call mark_pt(l,s,j,maxdn,buf,nl,ns)
         endif
      enddo
      call output_image2(buf,nl,ns,file(2),2)
      return
      end
