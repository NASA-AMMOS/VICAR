ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Match two image areas by fitting the DN values to an affine model
c
c        sp = a0 + a1*(l-l0) + a2*(s-s0)
c        lp = b0 + b1*(l-l0) + b2*(s-s0)
c
c The result from the fit is returned in common/cx/
c
      subroutine match_affine(nl,ns,I2,l0,s0,lmax,eps,nlw,nsw,
     &		m,n,A,b,r,I10,meanI1,varI1,I20,rho)
      implicit none
c     ...input values
      integer*4 nl,ns		!number of lines, number of samples
      integer*2 I2(ns,nl)	!target image is nl x ns
      integer*4 l0,s0		!reference pixel=I1(l0,s0)
      integer*4 lmax		!maximum number of iterations
      real*4 eps		!maximum tolerance on dg/dx (ignored)
      integer*4 nlw,nsw		!window size is nlw x nsw
      integer*4 m		!m=number of pixels in window=nlw*nsw
      integer*4 n		!n=number of geometric coefficients=6
      real*8 I10(m)             !copy of window centered at I1(s0,l0)
      real*8 meanI1,varI1
c     ...output values
      real*8 A(m,n)    		!m x n Jacobian
      real*8 b(m)		!b = Ax-r
      real*8 r(m)		!residual r=Ax-b
      real*8 I20(m)		!copy of window centered at I2(s0p,l0p)
      real*8 rho		!correlation coefficient

      common/cx/x		!input x=estimate  output x=minimizer of |Ax-b|
      real*8 x(6)               !x=[a0,b0,a1,b1,a2,b2]transpose
      real*8 a0,b0,a1,b1,a2,b2
      equivalence (x(1),a0),(x(2),b0),(x(3),a1),(x(4),b1)
      equivalence (x(5),a2),(x(6),b2)

      real*8 Atr(6),magAtr
      real*8 AtA(6,6),LL(6,6),Atb(6)

      integer*4 hnlw,hnsw	!nlw=hnlw+1+hnlw

c     ...for radiometric fit
      real*8 sumI2,sumI2I2	!sum and sum**2 of target window
      real*8 meanI2,varI2	!mean and variance of target window

      real*8 rr
      real*8 sumI1I2,cov	!sum(I1(i),I2(i)) and covariance
      real*8 ra			!resulting radiometric scale factor
  
c     ...for geometric fit
      real*8 I2lpsp		!I2(lp,sp) computed vi bilinear interpolation
      real*8 I2p1,I2m1		!(left and right) or (above and below) pixels
      real*8 dI2dlp,dI2dsp	!partial derivatives dI2/dlp and dI2/dsp

      integer*4 l,s		!coordinates of I1(s,l)
      real*8 lp,sp		!coordinates of I2(sp,lp)
      real*8 dl,ds		!dl=lp-l, ds=sp-s
      real*8 dpix
      real*8 a0sav,b0sav
      integer*4 i,j,k,loop,ind
      character*133 msg

c      call xvmessage('Entering match_affine',' ')
c      call prnt(4,1,nl,'nl=.')
c      call prnt(4,1,ns,'ns=.')
      hnlw = nlw/2		!nlw = hnlw + 1 + hnlw
      hnsw = nsw/2		!nsw = hnsw + 1 + hnsw

c     ...The corners of the reference window centered at pixel I1(l0,s0):
c
c		I1(l0-hnlw,s0-hnsw)	I1(l0-hnlw,s0+hnsw)
c		I1(l0+hnlw,s0-hnsw)	I1(l0+hnlw,s0+hnsw)
c
c     ...Since the reference window remains fixed, compute its mean and
c     ...variance outside of main loop:

      if (varI1.le.0.d0) goto 990		!no image info in ref window

c      call xvmessage(' ',' ')
c      write(msg,12) meanI1,varI1
c      call xvmessage(msg,' ')
c   12 format('meanI1=',f12.3,' varI1=',f12.3)
c      call xvmessage(' ',' ')
      magAtr = 0
      a0sav = a0
      b0sav = b0


c     ...Gauss-Newton loop
      do 50 loop=1,lmax	
      i = 0	!i=1,2,...,m
      sumI2 = 0
      sumI1I2 = 0
      sumI2I2 = 0

      do l=l0-hnlw,l0+hnlw
         dl = l - l0
         do s=s0-hnsw,s0+hnsw
            i = i + 1
            ds = s - s0
            sp = a0 + a1*dl + a2*ds
            lp = b0 + b1*dl + b2*ds
            if (sp.lt.2 .or. sp.ge.ns .or.
     &		lp.lt.2 .or. lp.ge.nl) goto 990
            call BilinearInt(I2,lp,sp,I2lpsp,nl,ns,ind)	!I2(lp,sp)
            if (ind.ne.1) goto 990
            I20(i) = I2lpsp 		!copy the window
            sumI2 = sumI2 + I2lpsp
            sumI1I2 = sumI1I2 + I10(i)*I2lpsp
            sumI2I2 = sumI2I2 + I2lpsp**2

		!dI2/dlp = 0.5*(I2(lp+1,sp)-I2(lp-1,sp))
            call BilinearInt(I2,lp+1,sp,I2p1,nl,ns,ind)	!I2(lp+1,sp)
            if (ind.ne.1) goto 990
            call BilinearInt(I2,lp-1,sp,I2m1,nl,ns,ind)	!I2(lp-1,sp)
            if (ind.ne.1) goto 990
            dI2dlp=0.5*(I2p1-I2m1)

		!dI2/dsp = 0.5*(I2(lp,sp+1)-I2(lp,sp-1))
            call BilinearInt(I2,lp,sp+1,I2p1,nl,ns,ind)	!I2(lp,sp+1)
            if (ind.ne.1) goto 990
            call BilinearInt(I2,lp,sp-1,I2m1,nl,ns,ind)	!I2(lp,sp-1)
            if (ind.ne.1) goto 990
            dI2dsp=0.5*(I2p1-I2m1)

            A(i,1) = -dI2dsp		!dr/da0
            A(i,2) = -dI2dlp		!dr/db0

            A(i,3) = -dI2dsp*dl		!dr/da1
            A(i,4) = -dI2dlp*dl		!dr/db1

            A(i,5) = -dI2dsp*ds		!dr/da2
            A(i,6) = -dI2dlp*ds		!dr/db2
         enddo
      enddo

c     ...radiometric fit
      meanI2 = sumI2/m
      varI2 = sumI2I2/m - meanI2**2
      if (varI2.le.0.) goto 990		!no image info in target window
ccc      call prnt(8,1,meanI1,'meanI1=.')
ccc      call prnt(8,1,meanI2,'meanI2=.')
ccc      call prnt(8,1,sumI2I2,'sumI2I2=.')
      cov = sumI1I2/m - meanI1*meanI2
ccc      call prnt(8,1,cov,'cov=.')
ccc      call prnt(8,1,varI1,'varI1=.')
ccc      call prnt(8,1,varI2,'varI2=.')
      rho = cov/dsqrt(varI1*varI2)
ccc      write(msg,102) loop-1,a0,b0,dpix,2*magAtr/m,rho
ccc      call xvmessage(msg,' ')
ccc  102 format(i2,' a0=',f11.5,' b0=',f11.5,' dpix=',f9.7,' dgdx=',f20.9,
ccc     &	' rho=',f10.6)

      ra  = cov/varI1		!radiometric scale factor
      do i=1,m
         r(i) = ra*(I10(i)-meanI1) - (I20(i)-meanI2)
      enddo

c     ...geometric fit
      call compute_Atr(A,r,Atr,magAtr,m,n)
      a0sav = a0
      b0sav = b0
      call compute_b(A,x,r,b,m,n)
      call LinearSolution(A,x,b,m,n,AtA,Atb,LL)		!compute new x
      dpix = dsqrt((a0-a0sav)**2+(b0-b0sav)**2)
ccc      write(msg,49) loop,(x(i),i=1,n)
ccc   49 format(i2,' x=',10f11.5)
ccc      call xvmessage(msg,' ')
   50 continue

c     ...compute correlation coefficient rho
      loop = lmax
      sumI2 = 0
      sumI1I2 = 0
      sumI2I2 = 0
      i = 0
      do l=l0-hnlw,l0+hnlw
         dl = l - l0
         do s=s0-hnsw,s0+hnsw
            i = i + 1
            ds = s - s0
            sp = a0 + a1*dl + a2*ds
            lp = b0 + b1*dl + b2*ds
            call BilinearInt(I2,lp,sp,I2lpsp,nl,ns)
            sumI2 = sumI2 + I2lpsp
            sumI1I2 = sumI1I2 + I10(i)*I2lpsp
            sumI2I2 = sumI2I2 + I2lpsp**2
         enddo
      enddo
      meanI2 = sumI2/m
      varI2 = sumI2I2/m - meanI2**2
      if (varI2.le.0.) goto 990		!no image info in target window
      cov = sumI1I2/m - meanI1*meanI2
      if (varI2 .lt. 0.) call mabend('****',' ')
      rho=cov/dsqrt(varI1*varI2)
ccc      write(msg,102) loop,a0,b0,dpix,2*magAtr/m,rho
ccc      call xvmessage(msg,' ')
      return

c     ...match failed
  990 rho = -999.
      return
      end
