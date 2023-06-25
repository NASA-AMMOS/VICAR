cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Test rotation and scale
c
      subroutine print_matches(l1buf,s1buf,l2buf,s2buf,
     &		izmax,ismax,itmax,flag,lnpts)
      implicit none
      integer*4 lnpts
      integer*2 l1buf(lnpts),s1buf(lnpts)
      integer*2 l2buf(lnpts),s2buf(lnpts)
      integer*2 izmax(lnpts),ismax(lnpts),itmax(lnpts)
      integer*2 flag(lnpts)

      integer*4 i,iz,is,it
      integer*4 l1,s1,l2,s2
      integer*4 dx,dy,sumdx,sumdy,avgdx,avgdy,npts

      common/cp/debug,print
      logical*1 debug,print

      character*80 msg
  101 format(i5,3i3,4i5,2i6)
  102 format(i5,3i3,4i5,2i6,' ***')

      if (.not.print) return

      sumdx = 0
      sumdy = 0
      do i=1,lnpts
	 if (flag(i).eq.0) then
             sumdx = sumdx + s2buf(i) - s1buf(i)
             sumdy = sumdy + l2buf(i) - l1buf(i)
             npts = npts + 1
	 endif
      enddo
      avgdx = sumdx/npts
      avgdy = sumdy/npts

      call xvmessage(' ',' ')
      call xvmessage
     &	('    i iz is it   l1   s1   l2   s3    devx  devy',' ')

      do 20 i=1,lnpts
      if (flag(i).eq.1) goto 20		!skip if no match
      iz = izmax(i)
      is = ismax(i)
      it = itmax(i)
      s1 = s1buf(i)
      l1 = l1buf(i)
      s2 = s2buf(i)
      l2 = l2buf(i)
      dx = s2 - s1
      dy = l2 - l1
      if (flag(i).eq.0) then
     	 write(msg,101) i,iz,is,it,l1,s1,l2,s2,dx-avgdx,dy-avgdy
      else
     	 write(msg,102) i,iz,is,it,l1,s1,l2,s2,dx-avgdx,dy-avgdy
      endif
      call xvmessage(msg,' ')
   20 continue
      return
      end
