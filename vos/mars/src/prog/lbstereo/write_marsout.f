ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Write disparities in marscor3 format.
c
      subroutine write_marsout(dx,dy,mask,nl,ns)
      implicit none
      integer*4 nl,ns
      real*4 dx(ns,nl),dy(ns,nl)
      integer*2 mask(ns,nl)

      common/ctemp/obuf(1024)
      real*4 obuf

      integer ounit,i,j,cnt,ind
      character*256 file

      call xvp('MARSOUT',file,cnt)
      if (cnt.eq.0) return
      call xvunit(ounit,'SCR',1,ind,'U_NAME',file,' ')
      call xvopen(ounit,ind,'O_FORMAT','REAL','U_FORMAT','REAL',
     +	'OP','WRITE','OPEN_ACT','SA','IO_ACT','SA',
     +	'U_NL',nl,'U_NS',ns,'U_NB',2,'U_ORG','BSQ',' ')
      call xvplabel(ounit,0,1,ind)

      do j=1,nl
         do i=1,ns
            if (mask(i,j).gt.0) then
               obuf(i) = dy(i,j) + j
            else
               obuf(i) = 0.
            endif
         enddo
         call xvwrit(ounit,obuf,ind,'LINE',j,'BAND',1,' ')
      enddo

      do j=1,nl
         do i=1,ns
            if (mask(i,j).gt.0) then
               obuf(i) = dx(i,j) + i
            else
               obuf(i) = 0.
            endif
         enddo
         call xvwrit(ounit,obuf,ind,'LINE',j,'BAND',2,' ')
      enddo

      call xvclose(ounit,ind,' ')
      return
      end
