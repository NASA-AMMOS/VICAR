ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Overlay the computed horizon on the image.
c
      subroutine write_horizon(img,hrzn,nl,ns,file,k)
      implicit none
      integer*4 nl,ns,k
      integer*2 img(ns,nl)			!Input image
      integer*2 hrzn(ns)
      character*256 file

      common/ctemp/buf(1024)
      integer*2 buf

      integer*4 i,j,ounit,ind

      call xvunit(ounit,'SCR',k,ind,'U_NAME',file,' ')
      call xvopen(ounit,ind,'O_FORMAT','HALF','U_FORMAT','HALF',
     +              'OP','WRITE','U_NL',nl,'U_NS',ns,' ')
      call xvplabel(ounit,0,1,ind)
      do j=1,nl
         do i=1,ns
            if (hrzn(i).eq.j) then
               buf(i) = 10000
            else
               buf(i) = img(i,j)
            endif
         enddo
         call xvwrit(ounit,buf,ind,' ')
      enddo
      call xvclose(ounit,ind,' ')
      return
      end
