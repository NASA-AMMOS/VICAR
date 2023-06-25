ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Delete regions smaller than a specified size (minsize)
c
      subroutine delete_regions(region,mask,nl,ns,rsize,rx,nr,zoom)
      implicit none
      integer*4 nl,ns,nr,zoom
      integer*4 region(ns,nl)	!output surface broken into regions
      integer*2 mask(ns,nl)	!if (rsize(region(i,j)).lt.minsize) mask(i,j)=32
      integer*4 rsize(nr)	!number of pixels in region
      integer*4 rx(nr)		!region index

      common/cb/print
      integer*4 print

      integer*4 i,j,ix,m,n,size,minsize,maxsize,cnt
      character*80 msg
  101 format('base regions deleted=',i5,' base regions kept=',i5)
  102 format('region=',i6,' size=',i7)

      call xvp('rthresh',minsize,cnt)	!smallest valid region
      minsize = minsize/(zoom*zoom)
      m = 0
      n = 0
      maxsize = 0

      do 10 ix=1,nr
      if (rx(ix).ne.ix) goto 10		!count only applies to base regions
      size = rsize(ix)
      if (size.lt.minsize) then
         n = n + 1				!number of base regions deleted
         if (size.gt.maxsize) maxsize=size	!largest deleted region
      else
         m = m + 1				!number of base regions kept
         if (print.eq.2) then
            write(msg,102) ix,size
            call xvmessage(msg,' ')
         endif
      endif
   10 continue

      if (print.gt.0) then
         write(msg,101) n,m
         call xvmessage(msg,' ')
         call prnt(4,1,maxsize,'size of largest region deleted=.')
      endif
      
      do j=1,nl
         do i=1,ns
            ix = region(i,j)
            if (ix.eq.0) then
               if (mask(i,j).eq.0) mask(i,j)=1
            elseif (rsize(ix).lt.minsize) then
               mask(i,j) = 4
            endif
         enddo
      enddo

      return
      end
