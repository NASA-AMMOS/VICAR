cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Sort the vertices by line number.
c
      subroutine sort_vertices(vx,vy,vert,n)
      implicit none
      integer*4 n
      integer*2 vx(n),vy(n)	!samp and line coords of vertices
      integer*4 vert(n)		!sort work array

      integer*4 i,j,k

      do i=1,n
         vert(i) = 65536*vy(i) + vx(i)
      enddo

      call sortin(vert,n)

      do i=1,n,2
         j = vert(i)/65536
         k = vert(i+1)/65536
         if (j.ne.k) then
            call prnt(4,1,i,'i=.')
            call mabend('***unmatched pair')
         endif
         k = 65536*j
         vx(i) = vert(i) - k
         vx(i+1) = vert(i+1) - k
         vy(i) = j
         vy(i+1) = j
      enddo

      return
      end
