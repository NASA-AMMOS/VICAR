cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Open left and right images:  inp=(left,right)
c return logical unit numbers, number of lines, and number of samples.
c
      subroutine open_inp(iunit,nl,ns)
      implicit none
      integer iunit(2),nl,ns	!returned values

      integer nli(2),nsi(2),i,ind

      call xveaction('SA',' ')
      do i=1,2
         call xvunit(iunit(i),'INP',i,ind,' ')
         call xvopen(iunit(i),ind,'U_FORMAT','HALF',' ')
         call xvget(iunit(i),ind,'NL',nli(i),'NS',nsi(i),' ')
      enddo
      nl = min0(nli(1),nli(2))
      ns = min0(nsi(1),nsi(2))
      return
      end
