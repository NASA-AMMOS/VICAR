cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Read a real*4 image into x(ns,nl)
c
      subroutine read_image(file,k,x,nl,ns)
      implicit none
      character*132 file(2)
      integer*4 k,nl,ns
      real*4 x(ns,nl)	!x=file(k)

      integer j,ind,iunit

      call xvunit(iunit,'SCR',k,ind,'U_NAME',file(k),' ')
      call xvopen(iunit,ind,'U_NL',nl,'U_NS',ns,' ')
      do j=1,nl
         call xvread(iunit,x(1,j),ind,' ')
      enddo
      call xvclose(iunit,ind,' ')
      return
      end
