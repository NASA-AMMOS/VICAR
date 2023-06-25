ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Output a real*4 image.
c
      subroutine output_image(a,nl,ns,file,instance)
      implicit none
      integer*4 nl,ns,instance
      real*4 a(ns,nl)	
      character*256 file

      integer*4 j,ounit,ind

      call xvunit(ounit,'SCX',instance,ind,'U_NAME',file,' ')
      call xvopen(ounit,ind,'O_FORMAT','REAL','U_FORMAT','REAL',
     +          'OP','WRITE','U_NL',nl,'U_NS',ns,
     +          'OPEN_ACT','SA','IO_ACT','SA',' ')
      call xvplabel(ounit,0,1,ind)
      do j=1,nl
         call xvwrit(ounit,a(1,j),ind,' ')
      enddo
      call xvclose(ounit,ind,' ')
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Output an integer*2 image.
c
      subroutine output_image2(a,nl,ns,file,instance)
      implicit none
      integer*4 nl,ns,instance
      integer*2 a(ns,nl)	
      character*256 file

      integer*4 j,ounit,ind

      call xvunit(ounit,'SCR',instance,ind,'U_NAME',file,' ')
      call xvopen(ounit,ind,'O_FORMAT','HALF','U_FORMAT','HALF',
     +              'OP','WRITE','U_NL',nl,'U_NS',ns,' ')
      call xvplabel(ounit,0,1,ind)
      do j=1,nl
         call xvwrit(ounit,a(1,j),ind,' ')
      enddo
      call xvclose(ounit,ind,' ')
      return
      end
