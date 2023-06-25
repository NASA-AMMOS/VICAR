cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c The spider will walk counterclockwise around an object, recording the
c line and sample coordinates of each step.
c An object is a region of pixels below a threshold.
c img,nl,ns,nmax,i0,j0,thresh are inputs.  All other arguments are outputs.
c
      subroutine spider(img,nl,ns,vx,vy,nmax,i0,j0,thresh,n)
      implicit none
      integer*4 nl,ns,nmax		!input image
      real*4 img(ns,nl)			!scene activity
      integer*2 vx(nmax),vy(nmax)	!output sample and line coordinates
      integer*4 i0,j0			!initial coordinates of object
      real*4 thresh			!object threshold
      integer*4 n			!number of pts

      integer*4 i,j,direction
      character*80 msg
  101 format(4i8)

      i = i0		!sample coordinate
      j = j0		!line coordinate
      n = 0		!number of coordinate points
      direction = 7
      goto 5		!starting direction is 5

    1 if (img(i+1,j-1).le.thresh) goto 10
    2 if (img(i,j-1)  .le.thresh) goto 20
    3 if (img(i-1,j-1).le.thresh) goto 30
    4 if (img(i-1,j)  .le.thresh) goto 40
    5 if (img(i-1,j+1).le.thresh) goto 50
    6 if (img(i,j+1)  .le.thresh) goto 60
    7 if (img(i+1,j+1).le.thresh) goto 70
    8 if (img(i+1,j)  .le.thresh) goto 80
      if (img(i+1,j-1).le.thresh) goto 10
      if (img(i,j-1)  .le.thresh) goto 20
      if (img(i-1,j-1).le.thresh) goto 30
      if (img(i-1,j)  .le.thresh) goto 40
      if (img(i-1,j+1).le.thresh) goto 50
      if (img(i,j+1)  .le.thresh) goto 60
      if (img(i+1,j+1).le.thresh) goto 70
      if (i.ne.i0 .or. j.ne.j0) then
         write(msg,101) i0,j0,i,j
         call xvmessage(msg,' ')
         call mabend('The spider is lost')
      endif
      direction = 5	!Here only if object is a single pixel?	
      n = n + 1
      vx(n) = i
      vy(n) = j
      goto 99

   10 if (direction .lt. 4) goto 11
      n = n + 1
      vx(n) = i
      vy(n) = j
   11 n = n + 1
      vx(n) = i
      vy(n) = j
      direction = 1	!save current direction
      i = i + 1		!step to next pixel
      j = j - 1
      if (i.eq.i0 .and. j.eq.j0) goto 99
      goto 7

   20 if (direction .lt. 4) goto 21
      n = n + 1
      vx(n) = i
      vy(n) = j
   21 n = n + 1
      vx(n) = i
      vy(n) = j
      direction = 2
      j = j - 1
      if (i.eq.i0 .and. j.eq.j0) goto 99
      goto 1

   30 if (direction.lt.4) goto 33
      if (direction.lt.7) goto 34
      n = n + 1	
      vx(n) = i
      vy(n) = j
   33 n = n + 1
      vx(n) = i
      vy(n) = j
   34 direction = 3
      i = i - 1
      j = j - 1
      if (i.eq.i0 .and. j.eq.j0) goto 99
      goto 1

   40 if (direction.ge.4) goto 41
      n = n + 1
      vx(n) = i
      vy(n) = j
   41 direction = 4
      i = i - 1
      if (i.eq.i0 .and. j.eq.j0) goto 99
      goto 3

   50 if (direction.ge.4) goto 51
      n = n + 1
      vx(n) = i
      vy(n) = j
   51 n = n + 1
      vx(n) = i
      vy(n) = j
      direction = 5
      i = i - 1
      j = j + 1
      if (i.eq.i0 .and. j.eq.j0) goto 99
      goto 3

   60 if (direction.ge.4) goto 61
      n = n + 1
      vx(n) = i
      vy(n) = j
   61 n = n + 1
      vx(n) = i
      vy(n) = j
      direction = 6
      j = j + 1
      if (i.eq.i0 .and. j.eq.j0) goto 99
      goto 5

   70 if (direction.lt.3) goto 73
      if (direction.eq.3) then
         n = n + 1
         vx(n) = i
         vy(n) = j
      endif
      n = n + 1
      vx(n) = i
      vy(n) = j
   73 direction = 7
      i = i + 1
      j = j + 1
      if (i.eq.i0 .and. j.eq.j0) goto 99
      goto 5

   80 if (direction.lt.4) goto 81
      n = n + 1
      vx(n) = i
      vy(n) = j
   81 direction = 0
      i = i + 1
      if (i.eq.i0 .and. j.eq.j0) goto 99
      goto 7

   99 if (direction.ne.4) then
         n = n + 1
         vx(n) = i
         vy(n) = j
      endif
      if (n.gt.nmax) then
         call mabend('***spider: too many segments in region')
      elseif (2*n/2.ne.n) then
         call mabend('***spider: unmatched segment end point')
      endif
      return
      end
