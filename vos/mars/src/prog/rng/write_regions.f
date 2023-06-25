ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Write regions  to disk.  All arguments are inputs.
c
      subroutine write_regions(r,obuf,nl,ns,rx,nr)
      implicit none
      integer*4 nl,ns,nr
      integer*4 r(ns,nl)
      integer*4 obuf(ns)
      integer*4 rx(nr)		!index of base region


      integer*4 i,j,ix,ounit,ind,cnt
      character*132 file

      call xvp('region',file,cnt)		!region=r
      if (cnt.eq.0) return
      call xvunit(ounit,'REGION',1,ind,'U_NAME',file,' ')
      call xvopen(ounit,ind,'O_FORMAT','FULL','U_FORMAT','FULL',
     +              'OP','WRITE','U_NL',nl,'U_NS',ns,' ')
      do j=1,nl
         do i=1,ns
            ix = r(i,j)
            if (ix.gt.0) then
               obuf(i)=rx(ix)	!convert to base region
             else
               obuf(i) = 0
            endif
         enddo
         call xvwrit(ounit,obuf,ind,' ')
      enddo
      call xvclose(ounit,ind,' ')
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Write region size  to disk.  All arguments are inputs.
c
      subroutine write_rsize(r,obuf,nl,ns,rsize,rx,nr)
      implicit none
      integer*4 nl,ns,nr
      integer*4 r(ns,nl)
      integer*4 rsize(nr)	!number of pixels in region
      integer*4 rx(nr)		!index of base region
      integer*4 obuf(ns)

      integer*4 i,j,ix,ounit,ind,cnt
      character*132 file

      call xvp('rsize',file,cnt)
      if (cnt.eq.0) return
      call xvunit(ounit,'RSIZE',1,ind,'U_NAME',file,' ')
      call xvopen(ounit,ind,'O_FORMAT','BYTE','U_FORMAT','FULL',
     +              'OP','WRITE','U_NL',nl,'U_NS',ns,' ')
      do ix=1,nr
         if (rsize(ix).gt.255) rsize(ix)=255	!truncate data for byte output
      enddo
      do j=1,nl
         do i=1,ns
            ix = r(i,j)
            if (ix.gt.0) then
               obuf(i) = rsize(ix)
            else
               obuf(i) = 0
            endif
         enddo
         call xvwrit(ounit,obuf,ind,' ')
      enddo
      call xvclose(ounit,ind,' ')
      return
      end
