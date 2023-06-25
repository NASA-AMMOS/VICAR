cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Write out an image display showing position of all candidate tiepoints.
c
      subroutine display_candidates(lbuf,sbuf,npts,buf,nl,ns,file,k)
      implicit none
      integer*4 npts
      integer*2 lbuf(npts),sbuf(npts)	!(l,s) of candidates
      integer*4 nl,ns
      real*4 buf(nl,ns)
      character*256 file
      integer*4 k			!1 or 2

      integer*4 l,s			!(l,s)
      integer*4 i,cnt
      real*4 dn

c     ...Create a blank image
      do l=1,nl
         do s=1,ns
            buf(s,l) = 0.0
         enddo
      enddo

      call xvp('RDN',dn,cnt)		!Get dn mark

c     ...Mark pixels of candidate tiepoints with dn
      do i=1,npts
         l = lbuf(i)
         s = sbuf(i)
         buf(s,l) = dn
      enddo

      call output_image(buf,nl,ns,file,k)
      return
      end
