ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Interpolate about integral maximum using two 1-d parabolic fits.
c
      subroutine interp_max(r,ilo,ihi,jlo,jhi,i0,j0,dx,dy)
      implicit none
      integer*4 ilo,ihi,jlo,jhi
      real*4 r(ilo:ihi,jlo:jhi)		!correlation matrix
      integer*4 i0,j0			!integral pixel displacement
      real*4 dx,dy			!output sub-pixel displacements

      integer*4 di,dj
      real*4 z(3)
      real*8 x,y,z0,z1,zm1,d
      character*80 msg
  100 format(6f10.6)
  102 format(3f11.5,2x,3f11.5)

      x = 0.d0
      y = 0.d0
      z0 = r(i0,j0)

      z1 = r(i0+1,j0)				!interpolate using zm1 z0 z1
      zm1 = r(i0-1,j0)
      if (z1.eq.-1.0 .or. zm1.eq.-1.0) goto 10	!skip unless all 3 are valid.
      d = z1 - 2*z0 + zm1
      if (dabs(d).gt.0.01) x=0.5*(zm1-z1)/d	!try not to mess up

   10 z1 = r(i0,j0+1)
      zm1 = r(i0,j0-1)
      if (z1.eq.-1.0 .or. zm1.eq.-1.0) goto 20
      d = z1 - 2*z0 + zm1
      if (dabs(d).gt.0.01) y=0.5*(zm1-z1)/d

   20 if (dabs(x).gt..5 .or. dabs(y).gt..5) then	!0 out of 40617
         call xvmessage(' ',' ')
         write(msg,100) x,y
         call xvmessage(msg,' ')
         do dj=-1,1
            do di=-1,1
               z(di+2) = r(i0+di,j0+dj)
            enddo
            write(msg,102) z(1),z(2),z(3)
            call xvmessage(msg,' ')
         enddo
         call xvmessage(' ',' ')
         dx = -999.
         dy = -999.
         return
      endif
      dx = i0 + x		!integral + sub-pixel
      dy = j0 + y
      return
      end
