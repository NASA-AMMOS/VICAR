ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compute the difference image and write to disc.
c
      subroutine write_diff(x,z,nl,ns)
      implicit none
      integer*4 nl,ns
      integer*2 x(ns,nl)
      real*4 z(ns,nl)

      common/ctemp/diff(1024)
      real*4 diff

      integer*4 i,j,ounit,cnt,ind
      character*132 file

      call xvp('DIFF',file,cnt)
      if (cnt.eq.0) return
      call xvunit(ounit,'DIFF',1,ind,'U_NAME',file,' ')
      call xvopen(ounit,ind,'O_FORMAT','REAL','U_FORMAT','REAL',
     +              'OP','WRITE','U_NL',nl,'U_NS',ns,' ')

      do j=1,nl
         do i=1,ns
            if (z(i,j).gt.-999) then
               diff(i) = x(i,j) - z(i,j)
            else
               diff(i) = -999.
            endif
         enddo
         call xvwrit(ounit,diff,ind,' ')
      enddo
      call xvclose(ounit,ind,' ')
      return
      end
