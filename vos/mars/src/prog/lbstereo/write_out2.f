cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine write_out2(obuf,nl,ns,instance)
      implicit none
      integer*4 nl,ns
      real*4 obuf(ns,nl)
      integer*4 instance

      integer*4 j,ounit,stat

      call xvunit(ounit,'OUT',instance,stat,' ')
      call xvopen(ounit,stat,'U_FORMAT','REAL','O_FORMAT','REAL',
     &		'U_NL',nl,'U_NS',ns,'OP','WRITE',
     &		'OPEN_ACT','SA','IO_ACT','SA',' ')
      do j=1,nl
	 call xvwrit(ounit,obuf(1,j),stat,' ')
      enddo
      call xvclose(ounit,stat,' ')
      return
      end

