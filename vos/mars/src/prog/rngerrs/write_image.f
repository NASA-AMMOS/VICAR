ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Write real*4 image z to disc.
c
      subroutine write_image(file,k,z,nl,ns)
      implicit none
      integer*4 k,nl,ns
      real*4 z(ns,nl)	
      character*132 file(2)

      integer*4 j,ounit,ind

      call xvunit(ounit,'SCR',k,ind,'U_NAME',file(k),' ')
      call xvopen(ounit,ind,'O_FORMAT','REAL','U_FORMAT','REAL',
     +              'OP','WRITE','U_NL',nl,'U_NS',ns,' ')
      do j=1,nl
         call xvwrit(ounit,z(1,j),ind,' ')
      enddo
      call xvclose(ounit,ind,' ')
      return
      end
