ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Write the mask to disk.
c
      subroutine write_mask(mask,nl,ns)
      implicit none
      integer*4 nl,ns
      integer*2 mask(ns,nl)	

      integer*4 j,ounit,ind,cnt
      character*132 file

      call xvp('mask',file,cnt)		!mask=(m1,m2,m3)
      if (cnt.eq.0) return
      call xvunit(ounit,'MASK',1,ind,'U_NAME',file,' ')
      call xvopen(ounit,ind,'O_FORMAT','BYTE','U_FORMAT','HALF',
     +              'OP','WRITE','U_NL',nl,'U_NS',ns,' ')
      call xvplabel(ounit,0,1,ind)
      do j=1,nl
         call xvwrit(ounit,mask(1,j),ind,' ')
      enddo
      call xvclose(ounit,ind,' ')
      return
      end
