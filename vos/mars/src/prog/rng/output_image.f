ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Output an image.
c
      subroutine output_image(a,nl,ns,file,instance)
      implicit none
      integer*4 nl,ns,instance
      real*4 a(ns,nl)	
      character*132 file

      integer*4 j,ounit,ind

      call xvunit(ounit,'SCR',instance,ind,'U_NAME',file,' ')
      call xvopen(ounit,ind,'O_FORMAT','REAL','U_FORMAT','REAL',
     +              'OP','WRITE','U_NL',nl,'U_NS',ns,' ')
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
      character*132 file

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

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Output an image, flagging pixels marked by mask.
c
      subroutine output_mimage(a,mask,nl,ns,file,instance)
      implicit none
      integer*4 nl,ns,instance
      real*4 a(ns,nl)
      integer*2 mask(ns,nl)
      character*132 file

      common/ctemp/obuf
      real*4 obuf(1024)

      integer*4 i,j,n,ounit,ind

      call xvunit(ounit,'SCR',instance,ind,'U_NAME',file,' ')
      call xvopen(ounit,ind,'O_FORMAT','REAL','U_FORMAT','REAL',
     +              'OP','WRITE','U_NL',nl,'U_NS',ns,' ')
      call xvplabel(ounit,0,1,ind)
      n = 0
      do j=1,nl
         do i=1,ns
            if (mask(i,j).eq.0) then
               obuf(i) = a(i,j)
               n = n + 1
            else
               obuf(i) = -999.
            endif
         enddo
         call xvwrit(ounit,obuf,ind,' ')
      enddo
      if (instance.eq.1) 
     &   call prnt(4,1,n,'Total number of valid range points=.')
      call xvclose(ounit,ind,' ')
      return
      end
