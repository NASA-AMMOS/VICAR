ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Write the difference image to disk.
c
      subroutine write_diff(l,pl,mask,nl,ns)
      implicit none
      integer*4 nl,ns
      integer*2 l(ns,nl),pl(ns,nl)
      integer*2 mask(ns,nl)

      common/ctemp/diff(1024)
      integer*2 diff

      integer*4 i,j,ounit,cnt,ind
      character*132 file

      call xvp('DIFF',file,cnt)
      if (cnt.eq.0) return
      call xvunit(ounit,'DIFF',1,ind,'U_NAME',file,' ')
      call xvopen(ounit,ind,'O_FORMAT','HALF','U_FORMAT','HALF',
     +              'OP','WRITE','U_NL',nl,'U_NS',ns,' ')
      call xvplabel(ounit,0,1,ind)

      do j=1,nl
         do i=1,ns
            if (mask(i,j).eq.0) then
               diff(i) = l(i,j) - pl(i,j)
            else
               diff(i) = -999
            endif
         enddo
         call xvwrit(ounit,diff,ind,' ')
      enddo
      call xvclose(ounit,ind,' ')
      return
      end
