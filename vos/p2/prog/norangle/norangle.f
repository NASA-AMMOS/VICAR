	include 'VICMAIN_FOR'
	subroutine main44
c
c	angle   
c
c	routine to compute the angle from the top
c	of the picture clockwise
c
	implicit none
	integer*4 lsval(4),cnt,h,w,x1,x2,y1,y2
	integer*4 height,width
	real*4 radians,degrees,opp,adj
 	real*4 radtodeg				!,degtorad	
c
	call xvmessage('** norangle version 2015-12-10',' ')
	call xvp ('LINESAMP',lsval,cnt)

c
c	raster to cartesian coord conversion
c	raster (0,0) -> Cartesian (-w/2,h/2)
c	raster (w,0) -> Cartesian (w/2,h/2)
c	raster (0,h) -> Cartesian (-w/2,-h/2)
c	raster (w,h) -> Cartesian (w/2,-h/2)
c
c	VICAR uses raster (1,1) so
c	raster (1,1) -> Cartesian ((-w/2)+1,(h/2)+1)
c       raster (w,1) -> Cartesian ((w/2)+1,(h/2)+1)
c       raster (1,h) -> Cartesian ((-w/2)+1,(-h/2)+1)
c       raster (w,h) -> Cartesian ((w/2)+1,(-h/2)+1)
c
c	Based on that, the formula is
c	x = rasterX - (w/2) + 1
c	y = (h/2) + 1 - rasterY
c
c	where w is width of image and h is its height
c
	
	h = lsval(3) - lsval(1) + 1	!line  
	w = lsval(4) - lsval(2) + 1	!sample

	x1 = lsval(2) - w/2 + 1
	x2 = lsval(4) - w/2 + 1
	y1 = h/2 + 1 - lsval(1)
	y2 = h/2 + 1 - lsval(3)

c	adj = y2 - y1			!line
c	opp = x2 - x1			!sample
	adj = y1 - y2			!line
	opp = x1 - x2			!sample
	height = int(adj)
	width  = int(opp)
	radians = atan2(opp,adj)		!arctan2(x/y) = x-dir/y-dir = sample/line
	degrees = radtodeg(radians)
c	rad90 = degtorad(90.0)
c	radians = rad90 - radians 
c	if (degrees .lt. 90.0) degrees = degrees -90
c	degrees = degrees - 90.
c	print *, "opp, adj = ",opp,adj
c	print *, "angle, radians = ",radians
c	print *, "angle, degrees = ",degrees


	call putparm (degrees,radians,height,width)
	return
	end
c==========================================================================
c.... Function to convert radians to degrees
      real*4 function radtodeg(x)
c
      implicit none
      real*4 x,pi
c
c.... Define pi as a parameter
      parameter(pi = 3.14159265)
c
      radtodeg = x *180.0/pi
c
      return
      end
c===========================================================================
c.... Function to convert degrees to radians
      real*4 function degtorad(x)
c
      implicit none
      real*4 x,pi
c
c.... Define pi as a parameter
      parameter(pi = 3.14159265)
c
      degtorad = x *pi/180.0
c
      return
      end
c===========================================================================
      subroutine putparm (degrees,radians,height,width)
c            
      implicit none
      integer*4 parb(1000),xcont,xadd,stat
      integer*4 height,width
       real*4 degrees,radians
c            
      call xqini (parb,1000,xcont)
      call xqreal (parb,'DEGREES',1,degrees,xadd,stat)
      call xqreal (parb,'RADIANS',1,radians,xadd,stat)
      call xqintg (parb,'HEIGHT',1,height,xadd,stat)
      call xqintg (parb,'WIDTH',1,width,xadd,stat)

      call xqout (parb,stat)
      call chkstat (stat,'??E - XQout error',0,0,0)

	return
	end


