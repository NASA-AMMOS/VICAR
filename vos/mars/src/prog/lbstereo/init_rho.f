cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Initialize rho at the start of each iz loop
c
      subroutine init_rho(rho,rhobuf,lnpts,rnpts,iz)
      implicit none
      integer*4 lnpts,rnpts,iz
      real*4 rho(lnpts,rnpts),rhobuf(lnpts)

      integer*4 i1,i2
      character*80 msg

      call xvmessage(' ',' ')
      write(msg,101) iz,lnpts,rnpts
  101 format('iz=',i1,' lnpts=',i3,' rnpts=',i4)
      call xvmessage(msg,' ')

      do i2=1,rnpts
	 do i1=1,lnpts
            rho(i1,i2) = -999.
	 enddo
      enddo

      do i1=1,lnpts
	 rhobuf(i1) = -999.
      enddo
      return
      end
