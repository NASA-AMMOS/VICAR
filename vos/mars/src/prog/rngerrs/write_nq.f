ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Write nq to disc.
c
      subroutine write_nq(nq,nl,ns)
      implicit none
      integer*4 nl,ns
      integer*2 nq(ns,nl)

      integer*4 j,ounit,cnt,ind
      character*132 file

      call xvp('NQ',file,cnt)
      if (cnt.eq.0) return
      call xvunit(ounit,'NQ',1,ind,'U_NAME',file,' ')
      call xvopen(ounit,ind,'O_FORMAT','HALF','U_FORMAT','HALF',
     +              'OP','WRITE','U_NL',nl,'U_NS',ns,' ')

      do j=1,nl
         call xvwrit(ounit,nq(1,j),ind,' ')
      enddo
      call xvclose(ounit,ind,' ')
      return
      end
