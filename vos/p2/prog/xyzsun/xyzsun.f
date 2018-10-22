c
c program xyzsun
c
      include 'VICMAIN_FOR'
      subroutine main44

      implicit real*8(a-h,o-z)
      integer ntable
      parameter (ntable=3000)

c ntable= max number of tiepoints

      character*100 filename,msg,header1,header2,spicefile
      character*100 header3,header4,header5,header6
      character*100 header7,header8,header9,header10
      character*100 header11,header12,header13,header14
      character*100 header15
      integer*4 def,count,id(ntable)
      real*8 om_matrix1(3,3),rs_vector1(3)
      real*8 om_matrix2(3,3),rs_vector2(3)
      real*8 mat1(3,3),mat2(3,3)
      real*4 line1(ntable),samp1(ntable),temp1,temp2
      real*4 line2(ntable),samp2(ntable)
      real*4 dnl(ntable),dnr(ntable)
      real*8 x(ntable),y(ntable),z(ntable),lat(ntable)
      real*8 lon(ntable),r(ntable),dr(ntable),error(ntable)
      real*8 latrad,long
      real*8 line1km,samp1km,line2km,samp2km
      integer*4 h(2)
      logical failed,xvptst

      raddeg=45.d0/datan(1.d0)

c initialize & load naif spice kernels
      if(xvptst('SPICE'))then
c        call clpool
c        call xgetenv_vic('LEAPSECONDS',msg)
c        call ldpool(msg)
c        if(failed()) call xvmessage
c    +   ('***Error loading LEAPSECONDS kernel',' ')
c        call xgetenv_vic('CONSTANTS',msg)
c        call ldpool(msg)
c        if(failed()) call xvmessage
c    +   ('***Error loading CONSTANTS kernel',' ')
c  Jan 2013 / lwk:  above replaced with single call:
	 call init_spice
         call xvparm('SPICEFILE',spicefile,count,def,1)
         call spklef(spicefile,h(1))
      endif

c read tiepoints from ascii file
      call xvpone('INP',filename,1,100)
      call read_table(filename,id,line1,samp1,line2,samp2,
     +      dnl,dnr,ntable,ntab,
     +      header1,header2,header3,header4,header5,header6,
     +      header7,header8,header9,header10,header11,header12,
     +      header13,header14,header15)
      write(msg,*)ntab,' tiepoints located in ascii table'
      call xvmessage(msg,' ')

c reverse line & sample
      if(xvptst('SWITCH'))then
        call xvmessage('Switching line & sample columns',' ')
        do i=1,ntab
          temp1=line1(i)
          temp2=line2(i)
          line1(i)=samp1(i)
          line2(i)=samp2(i)
          samp1(i)=temp1
          samp2(i)=temp2
        enddo
      endif

c apply epipolar constraint
      if(xvptst('EPIPOLAR'))then
        call xvmessage('Applying epipolar constraint on lines',' ')
        do i=1,ntab
          line2(i)=line1(i)
        enddo
      endif

c extract time from header
      if(xvptst('SPICE'))then
        call gettimes2(header2,header3,time1,time2,ind)
        if(ind.gt.0)then 
          call xvmessage
     +    ('Warning: cannot extract date from tiepoints file',' ')
          goto 100
        endif

c compute sub earth lat, lon & range to sun 
        call ephemeris(time1,range1,sclong1,sclat1)
        write(header4,137) sclat1,sclong1
137     format(' First image: Sub_earth latitude=',f10.5,
     +   ' west longitude=',f10.5)
        call xvmessage(header4,' ')
        write(header5,*)'Sun range= ',range1,' km'
        call xvmessage(header5,' ') 

        call ephemeris(time2,range2,sclong2,sclat2)
        write(header6,138) sclat2,sclong2
138     format(' Second image: Sub_earth latitude=',f10.5,
     +   ' west longitude=',f10.5)
        call xvmessage(header6,' ')
        write(header7,*)'Sun range= ',range2,' km'
        call xvmessage(header7,' ') 
      endif
100   continue

      call xvparmd('ERADIUS',eradius,count,def,1)
      call xvparmd('PRADIUS',pradius,count,def,1)
      if(xvptst('MANUAL'))then
c       compute location of planet center and focal length from first 4
c       tiepoints.
c       point1 is on left limb.
c       point2 is on right limb.
c       point3 is on top limb.
c       point4 is on bottom limb.
        pix_radius=((line1(4)-line1(3)) + (samp1(2)-samp1(1)) +
     +            (line2(4)-line2(3)) + (samp2(2)-samp2(1)))/8.0
        cenline=( line1(3)+pix_radius +
     +          line1(4)-pix_radius +
     +          line2(3)+pix_radius +
     +          line2(4)-pix_radius)/4.0
        censamp=( samp1(1)+pix_radius +
     +          samp1(2)-pix_radius +
     +          samp2(1)+pix_radius +
     +          samp2(2)-pix_radius)/4.0
        focal=pix_radius*((range1+range2)/2.0)/((eradius+pradius)/2.0)
        scale=1.0
      else
c       Assume planet is centered & plate scale is 42 min of arc
c       for 512 pixels.
        solar_angle=atan((eradius+pradius)/(range1+range2))*
     +   raddeg  ! in degrees
        pix_radius=solar_angle*512.0/(42.0/60.0) ! in pixels
        focal=pix_radius*(range1+range2)/(eradius+pradius)
        scale=1.0
        cenline=256.
        censamp=256.
      endif

c get parameters
      call xvpcnt('INP',nids)
      call xvparmd('OAL',oal,count,def,1)
      call xvparmd('OAS',oas,count,def,1)
      call xvparmd('CENLINE',cenline,count,def,1)
      call xvparmd('CENSAMP',censamp,count,def,1)
      call xvparmd('SCALE',scale,count,def,1)
      call xvparmd('FOCAL',focal,count,def,1)
      call xvparmd('SCLONG1',sclong1,count,def,1)
      call xvparmd('SCLAT1',sclat1,count,def,1)
      call xvparmd('SCLONG2',sclong2,count,def,1)
      call xvparmd('SCLAT2',sclat2,count,def,1)
      call xvparmd('NORTH',north,count,def,1)
      call xvparmd('RANGE',range,count,def,1)
      if(count.eq.1)then
        range1=range
        range2=range
      endif

      write(header8,*)'Solar radius, pole=',pradius,' equator=',
     +  eradius,' km'
      call xvmessage(header8,' ') 
      write(header9,*)'Solar center line=',cenline,' sample=',censamp
      call xvmessage(header9,' ') 
      write(header10,*)'Solar radius in pixels=',pix_radius
      call xvmessage(header10,' ') 
      write(header11,*)'Solar north=',north,' degrees clockwise from up'
      call xvmessage(header11,' ') 
      write(header12,*)'Camera focal length=',focal,' mm'
      call xvmessage(header12,' ') 
      write(header13,*)'Camera scale=',scale,' pixels/mm'
      call xvmessage(header13,' ') 
      write(header14,*)'Camera axis line=',oal,' sample=',oas
      call xvmessage(header14,' ') 

c compute om matrix & rs vector for images 1 & 2
      call momati(oal,oas,cenline,censamp,scale,focal,
     +   sclong1,sclat1,north,range1,om_matrix1,rs_vector1)
      call momati(oal,oas,cenline,censamp,scale,focal,
     +   sclong2,sclat2,north,range2,om_matrix2,rs_vector2)

c compute xvector arguments
      focal1=-focal/1.0d+06 ! focal length camera1 in KM.
      focal2=-focal/1.0d+06 ! focal length camera2 in KM.
      scale1=scale*1.0d+06 ! pixels/KM camera1 image plane.
      scale2=scale*1.0d+06 ! pixels/KM camera2 image plane.
      x1p=oas/scale1     ! optical axis sample in KM.
      y1p=oal/scale1     ! optical axis line in KM
      x2p=oas/scale2     ! optical axis sample in KM
      y2p=oal/scale2     ! optical axis line in KM
      rpole=pradius ! planet polar radius
      req=eradius   ! planet equatorial radius
c     transpose om matrix
      do i=1,3
        do j=1,3
          mat1(i,j)=om_matrix1(j,i)
          mat2(i,j)=om_matrix2(j,i)
        enddo
      enddo

      do i=1,ntab

c         convert image coords to Km.
          line1km=line1(i)/scale1
          samp1km=samp1(i)/scale1
          line2km=line2(i)/scale2
          samp2km=samp2(i)/scale2

c         convert image coords to xyz in planet coordinates in KM.
          call xvector(mat1,mat2,focal1,focal2,
     +       rs_vector1,rs_vector2,x1p,y1p,x2p,y2p,
     +       samp1km,line1km,samp2km,
     +       line2km,x(i),y(i),z(i),error(i),ind)

c         compute lat, w lon,radius
          if(ind.eq.0)then
            latrad=datan2(z(i),dsqrt(x(i)*x(i)+y(i)*y(i)))
            lat(i)=raddeg*latrad ! Geocentric lat. deg.
            long=360.d0-raddeg*datan2(y(i),x(i))
            if(long.gt.360.d0) long=long-360.d0
            lon(i)=long                ! W long deg.
            r(i)=dsqrt(x(i)*x(i)+y(i)*y(i)+z(i)*z(i))
            if(rpole.eq.req)then
              rad=rpole
            else
              rad=rpole*req/dsqrt(rpole*rpole*(dcos(latrad))**2+
     +                           req*req*(dsin(latrad))**2)
            endif
            dr(i)=r(i)-rad  ! Elevation above geoid Km.
          else
            x(i)=0.  ! result of error in xvector
            y(i)=0.
            z(i)=0.
          endif

      enddo

c write output file
      call xvpone('OUT',filename,1,100)
      call write_table(filename,id,
     +    line1,samp1,line2,samp2,dnl,dnr,
     +    x,y,z,lat,lon,r,dr,error,ntable,ntab,
     +    header1,header2,header3,header4,header5,header6,
     +    header7,header8,header9,header10,header11,header12,
     +    header13,header14,header15)

      return
      end

c*******************************************************************
      subroutine ephemeris(ephemeris_time,range,sclon,sclat)
      implicit real*8 (a-z)
      integer*4 id_target,id_observer
      real*8 state(6),tibf(3,3)

         id_target=10      ! The target object Sun
         id_observer=399   ! Earth observer

c        compute target state light time corrected at epoch from observer
         call spkez(id_target,ephemeris_time,'J2000','LT',id_observer,
     +              state,light_time)

c        compute inertial to body-fixed rotation matrix tibf
         call bodmat(id_target,ephemeris_time - light_time,tibf)

c        reverse state vector to from target to observer
         call vminus(state,state)

c        rotate state into body-fixed
         call mxv(tibf,state,state)

c        compute range to target latitude & longitude of sub point
         call reclat(state,range,lon,lat)
         sclat=lat*dpr()
         sclon=lon*dpr()
         sclon=360.-sclon               ! convert to west
         if(sclon.gt.360.) sclon=sclon-360.
         if(sclon.lt.0.) sclon=sclon+360.

      return
      end

c*******************************************************************
c reads times like: asfd19920112044820a.vic
c with the time-stamp:  yyyymmddhhmmss
      subroutine gettimes2(header1,header2,et1,et2,ind)
      character*100 msg,header1,header2
      real*8 et1,et2

      msg(1:2)=header1(9:10) ! month
      msg(3:3)=' '
      msg(4:5)=header1(11:12) ! day
      msg(6:6)=' '
      msg(7:10)=header1(5:8) ! year
      msg(11:11)=' '
      msg(12:13)=header1(13:14) ! hour
      msg(14:14)=' '
      msg(15:16)=header1(15:16) ! min
      msg(17:17)=' '
      msg(18:19)=header1(17:18) ! sec
      msg(20:100)=' '

      call utc2et(msg,et1)

      msg(1:2)=header2(9:10) ! month
      msg(3:3)=' '
      msg(4:5)=header2(11:12) ! day
      msg(6:6)=' '
      msg(7:10)=header2(5:8) ! year
      msg(11:11)=' '
      msg(12:13)=header2(13:14) ! hour
      msg(14:14)=' '
      msg(15:16)=header2(15:16) ! min
      msg(17:17)=' '
      msg(18:19)=header2(17:18) ! sec
      msg(20:100)=' '

      call utc2et(msg,et2)
 
      ind=0

      return
      end
c*******************************************************************
c obsolete, for time stamps like: sf_fits920427.170918
      subroutine gettimes(header1,header2,et1,et2,ind)
      character*100 msg,header1,header2
      real*8 et1,et2

      ind=1
      i=index(header1,'sf_fits')
      if(i.eq.0)return

      msg(1:2)=header1(i+9:i+10) ! month
      msg(3:3)=' '
      msg(4:5)=header1(i+11:i+12) ! day
      msg(6:8)=' 19'
      msg(9:10)=header1(i+7:i+8) ! year
      msg(11:11)=' '
      msg(12:13)=header1(i+14:i+15) ! hour
      msg(14:14)=' '
      msg(15:16)=header1(i+16:i+17) ! min
      msg(17:17)=' '
      msg(18:19)=header1(i+18:i+19) ! sec
      msg(20:100)=' '

      call utc2et(msg,et1)

      i=index(header2,'sf_fits')
      if(i.eq.0)return

      msg(1:2)=header2(i+9:i+10) ! month
      msg(3:3)=' '
      msg(4:5)=header2(i+11:i+12) ! day
      msg(6:8)=' 19'
      msg(9:10)=header2(i+7:i+8) ! year
      msg(11:11)=' '
      msg(12:13)=header2(i+14:i+15) ! hour
      msg(14:14)=' '
      msg(15:16)=header2(i+16:i+17) ! min
      msg(17:17)=' '
      msg(18:19)=header2(i+18:i+19) ! sec
      msg(20:100)=' '

      call utc2et(msg,et2)
 
      ind=0

      return
      end

c*******************************************************************
      subroutine write_table(filename,id,
     +    line1,samp1,line2,samp2,dnl,dnr,
     +    x,y,z,lat,lon,r,dr,error,ntable,ntab,
     +    header1,header2,header3,header4,header5,header6,
     +    header7,header8,header9,header10,header11,header12,
     +    header13,header14,header15)
      real*4 line1(ntable),samp1(ntable)
      real*4 line2(ntable),samp2(ntable)
      real*4 dnl(ntable),dnr(ntable)
      real*8 x(ntable),y(ntable),z(ntable),lat(ntable)
      real*8 lon(ntable),r(ntable),dr(ntable),error(ntable)
      integer*4 id(ntable)
      character*100 filename
      character*100 header1,header2,header3,header4,header5
      character*100 header6,header7,header8,header9,header10
      character*100 header11,header12,header13,header14,header15
      character*128 headerx
 
      open(unit=10,file=filename,access='SEQUENTIAL',
     + form='FORMATTED',iostat=ios,status='UNKNOWN')
      if(ios.gt.0)then
        call xvmessage('Cannot open output',' ')
        call abend
      endif
 
      write(unit=10,fmt=*,iostat=ios)header1
      if(ios.gt.0)then
          call xvmessage('write error on headers',' ')
          call abend
      endif
      write(unit=10,fmt=*,iostat=ios)header2
      write(unit=10,fmt=*,iostat=ios)header3
      write(unit=10,fmt=*,iostat=ios)header4
      write(unit=10,fmt=*,iostat=ios)header5
      write(unit=10,fmt=*,iostat=ios)header6
      write(unit=10,fmt=*,iostat=ios)header7
      write(unit=10,fmt=*,iostat=ios)header8
      write(unit=10,fmt=*,iostat=ios)header9
      write(unit=10,fmt=*,iostat=ios)header10
      write(unit=10,fmt=*,iostat=ios)header11
      write(unit=10,fmt=*,iostat=ios)header12
      write(unit=10,fmt=*,iostat=ios)header13
      write(unit=10,fmt=*,iostat=ios)header14
c     write(unit=10,fmt=*,iostat=ios)
c    +'  ID linel  sampl  liner  sampr   dnl    dnr      X           Y          Z        lat        lon        R        dR        error'
c (Jan 2013 / lwk:  above fails with new compiler flag on Solaris, replaced with:
      headerx(1:64) = '  ID linel  sampl  liner  sampr   dnl    dnr      X           Y '
      headerx(65:128)='         Z        lat        lon        R        dR        error'
      write(unit=10,fmt=*,iostat=ios)headerx
      if(ios.gt.0)then
          call xvmessage('write error on headers',' ')
          call abend
      endif
 
      do k=1,ntab
        write(unit=10,fmt=10,iostat=ios) id(k),line1(k),samp1(k),
     +        line2(k),samp2(k),dnl(k),dnr(k),x(k),y(k),z(k),
     +        lat(k),lon(k),r(k),dr(k),error(k)
        if(ios.gt.0)then
          call xvmessage('write error on data',' ')
          call abend
        endif
 
      enddo

10    format(i5,6f7.2,3f11.0,f9.4,f9.4,f11.0,2f11.0) 
      close(unit=10)
      return
      end
c*******************************************************************
      subroutine read_table(filename,id,line1,samp1,line2,samp2,
     +      dnl,dnr,ntable,k,
     +      header1,header2,header3,header4,header5,header6,
     +      header7,header8,header9,header10,header11,header12,
     +      header13,header14,header15)
      real*4 line1(ntable),samp1(ntable)
      real*4 line2(ntable),samp2(ntable)
      real*4 dnl(ntable),dnr(ntable)
      integer*4 id(ntable)
      character*100 filename
      character*100 header1,header2,header3,header4,header5
      character*100 header6,header7,header8,header9,header10
      character*100 header11,header12,header13,header14,header15
 
      open(unit=10,file=filename,access='SEQUENTIAL',
     + form='FORMATTED',iostat=ios,status='OLD')
      if(ios.gt.0)then
        call xvmessage('cannot open tiepoints file',' ')
        call abend
      endif
 
      read(unit=10,fmt=10,iostat=ios)header1
      if(ios.gt.0)then
          call xvmessage('read error on headers',' ')
          call abend
      endif
      read(unit=10,fmt=10,iostat=ios)header2
      read(unit=10,fmt=10,iostat=ios)header3
      read(unit=10,fmt=10,iostat=ios)header4
      read(unit=10,fmt=10,iostat=ios)header5
      read(unit=10,fmt=10,iostat=ios)header6
      read(unit=10,fmt=10,iostat=ios)header7
      read(unit=10,fmt=10,iostat=ios)header8
      read(unit=10,fmt=10,iostat=ios)header9
      read(unit=10,fmt=10,iostat=ios)header10
      read(unit=10,fmt=10,iostat=ios)header11
      read(unit=10,fmt=10,iostat=ios)header12
      read(unit=10,fmt=10,iostat=ios)header13
      read(unit=10,fmt=10,iostat=ios)header14
      read(unit=10,fmt=10,iostat=ios)header15
 
      k=0
3     k=k+1
      if(k.gt.ntable)then
        call xvmessage('Too many table entries',' ')
        k=k-1
        return
      endif

      read(unit=10,fmt=*,iostat=ios) id(k),line1(k),samp1(k),
     +        line2(k),samp2(k),dnl(k),dnr(k)
      if(ios.gt.0)then
         call xvmessage('read error on tiepoints file',' ')
         call abend
      endif
      if(ios.lt.0)then  ! EOF
        k=k-1
        close(unit=10)
        return
      else
        goto 3
      endif
 
10    format (A100)
      return
      end

c*******************************************************************
       subroutine xvector(mat1,mat2,focal1,focal2,cam1,cam2,
     +     x1p,y1p,x2p,y2p,x1,y1,x2,y2,x,y,z,error,ind)
c Convert from image coordinates to xyz coordinates given two
c images forming a stereo pair.
c mat1=rotation matrix for camera 1
c mat2=rotation matrix for camera 2
c focal1=camera1 focal length
c focal2=camera2 focal length
c cam1=x,y,z object space position of camera 1
c cam2=x,y,z object space position of camera 2
c x1p,y1p= x & y image plane coord of optical axis, camera 1
c x2p,y2p= x & y image plane coord of optical axis, camera 2
c x1,y1= x & y image plane coord of common point of interest, camera 1
c x2,y2= x & y image plane coord of common point of interest, camera 2
c x,y,z= xyz object space coord of object (returned)
c ind=0 OK, ind=1 no solution (returned)
c Reference: Manual of Photogrammetry, page 64.
       implicit real*8 (a-z)
       real*8 mat1(9),mat2(9),cam1(3),cam2(3)
       real*8 a(9),b(3),c(9)
       integer*4 ind
 
c compute direction cosines u,v,w for ray1 and ray2
       ind=0
       dx=x1-x1p
       dy=y1-y1p
       u1=mat1(1)*dx+mat1(4)*dy+mat1(7)*(-focal1)
       v1=mat1(2)*dx+mat1(5)*dy+mat1(8)*(-focal1)
       w1=mat1(3)*dx+mat1(6)*dy+mat1(9)*(-focal1)
       d=dsqrt(u1*u1+v1*v1+w1*w1)
       u1=u1/d
       v1=v1/d
       w1=w1/d
       dx=x2-x2p
       dy=y2-y2p
       u2=mat2(1)*dx+mat2(4)*dy+mat2(7)*(-focal2)
       v2=mat2(2)*dx+mat2(5)*dy+mat2(8)*(-focal2)
       w2=mat2(3)*dx+mat2(6)*dy+mat2(9)*(-focal2)
       d=dsqrt(u2*u2+v2*v2+w2*w2)
       u2=u2/d
       v2=v2/d
       w2=w2/d
 
c solve for x,y,z point on ray1 nearest to ray2
       as=v1*w2-w1*v2
       bs=u2*w1-u1*w2
       cs=u1*v2-v1*u2
       as1=bs*w1-v1*cs
       bs1=u1*cs-as*w1
       cs1=as*v1-u1*bs
       as2=bs*w2-v2*cs
       bs2=u2*cs-as*w2
       cs2=as*v2-u2*bs
       a(1)=as
       a(2)=as1
       a(3)=as2
       a(4)=bs
       a(5)=bs1
       a(6)=bs2
       a(7)=cs
       a(8)=cs1
       a(9)=cs2
       do 10 i=1,9
          c(i)=a(i)
10     continue
       b(1)=as*cam1(1)+bs*cam1(2)+cs*cam1(3)
       b(2)=as1*cam1(1)+bs1*cam1(2)+cs1*cam1(3)
       b(3)=as2*cam2(1)+bs2*cam2(2)+cs2*cam2(3)
       call dsimq(a,b,3,ind)
       x=b(1)
       y=b(2)
       z=b(3)
       if(ind.gt.0) return
 
c solve for xx,yy,zz point on ray2 nearest to ray1
       b(1)=as*cam2(1)+bs*cam2(2)+cs*cam2(3)
       b(2)=as1*cam1(1)+bs1*cam1(2)+cs1*cam1(3)
       b(3)=as2*cam2(1)+bs2*cam2(2)+cs2*cam2(3)
       call dsimq(c,b,3,ind)
       if(ind.gt.0) return
       xx=b(1)
       yy=b(2)
       zz=b(3)
 
c point inbetween is the closest approach point to both vectors
       error=dsqrt((z-zz)**2+(y-yy)**2+(x-xx)**2)
       x=(x+xx)/2.d0
       y=(y+yy)/2.d0
       z=(z+zz)/2.d0
       return
       end

c*******************************************************************
      SUBROUTINE DSIMQ(A,B,N,KS)
C        USAGE
C           CALL DSIMQ(A,B,N,KS)
C        DESCRIPTION OF PARAMETERS
C           A - MATRIX OF COEFFICIENTS STORED COLUMNWISE.  THESE ARE
C               DESTROYED IN THE COMPUTATION.  THE SIZE OF MATRIX A IS
C               N BY N.
C           B - VECTOR OF ORIGINAL CONSTANTS (LENGTH N). THESE ARE
C               REPLACED BY FINAL SOLUTION VALUES, VECTOR X.
C           N - NUMBER OF EQUATIONS AND VARIABLES. N MUST BE .GT. ONE.
C           KS - OUTPUT DIGIT
C                0 FOR A NORMAL SOLUTION
C                1 FOR A SINGULAR SET OF EQUATIONS
      real*8 A(1),B(1),biga,save,tol
C
C        FORWARD SOLUTION
C
      TOL=0.d0
      KS=0
      JJ=-N
      DO 65 J=1,N
      JY=J+1
      JJ=JJ+N+1
      BIGA=0.d0
      IT=JJ-J
      DO 30 I=J,N
C
C        SEARCH FOR MAXIMUM COEFFICIENT IN COLUMN
C
      IJ=IT+I
      IF(dabs(BIGA)-dabs(A(IJ))) 20,30,30
   20 BIGA=A(IJ)
      IMAX=I
   30 CONTINUE
C
C        TEST FOR PIVOT LESS THAN TOLERANCE (SINGULAR MATRIX)
C
      IF(dabs(BIGA)-TOL) 35,35,40
   35 KS=1
      RETURN
C
C        INTERCHANGE ROWS IF NECESSARY
C
   40 I1=J+N*(J-2)
      IT=IMAX-J
      DO 50 K=J,N
      I1=I1+N
      I2=I1+IT
      SAVE=A(I1)
      A(I1)=A(I2)
      A(I2)=SAVE
C
C        DIVIDE EQUATION BY LEADING COEFFICIENT
C
   50 A(I1)=A(I1)/BIGA
      SAVE=B(IMAX)
      B(IMAX)=B(J)
      B(J)=SAVE/BIGA
C
C        ELIMINATE NEXT VARIABLE
C
      IF(J-N) 55,70,55
   55 IQS=N*(J-1)
      DO 65 IX=JY,N
      IXJ=IQS+IX
      IT=J-IX
      DO 60 JX=JY,N
      IXJX=N*(JX-1)+IX
      JJX=IXJX+IT
   60 A(IXJX)=A(IXJX)-(A(IXJ)*A(JJX))
   65 B(IX)=B(IX)-(B(J)*A(IXJ))
C
C        BACK SOLUTION
C
   70 NY=N-1
      IT=N*N
      DO 80 J=1,NY
      IA=IT-J
      IB=N-J
      IC=N
      DO 80 K=1,J
      B(IB)=B(IB)-A(IA)*B(IC)
      IA=IA-N
   80 IC=IC-1
      RETURN
      END

