c marsbias
      include 'VICMAIN_FOR'
      subroutine main44
      parameter (maxoverlaps=10000,maxpix=20,maximages=500)


      integer*4 images(maxpix,maxoverlaps),count(maxoverlaps)
      real*4 dn(maxpix,maxoverlaps),bias(maximages)
      character*120 filename
      character*120 filenames(maximages)
      real*4 p(maximages+1,maximages),y(maximages),vect(maximages)
      real*4 inertia
      logical*1 has_overlaps(maximages)

      external funk

      call xvmessage('NOTE:  This program is now obsolete.', '')
      call xvmessage('       Please use marsbrt instead.', '')

c parameters
      call xveaction('SA',' ')
      call xvparm('OPTION',mode,i,j,1)
      call xvparm('FTOL',ftol,i,j,1)
      call xvparm('INERTIA',inertia,i,j,1)
      
c read input list of filenames
      call xvpone('INP',filename,1,120)
      call read_filenames(filename,filenames,maximages,nimages)
      write(*,*)'Located ',nimages,' input files'
      
c read input overlap file
      call xvpone('INP',filename,2,120)
      call read_overlap_file(filename,
     + maxoverlaps,maxpix,count,noverlaps,images,dn)
      write(*,*)'Read ',noverlaps,' overlap regions'

c      do i=1,noverlaps
c        write(*,*)(images(k,i),k=1,count(i)),
c     +            (dn(k,i),k=1,count(i))
c      enddo

c set up initial simplex
      ndim=nimages
      mp=maximages+1
      np=maximages
      do i=1,nimages
        if(mode.eq.1)p(1,i)=1.0
        if(mode.eq.2)p(1,i)=0.0
      enddo
      do j=2,nimages+1
        do i=1,nimages
          p(j,i)=p(1,i)
        enddo
        if(mode.eq.1)p(j,j-1)=1.1
        if(mode.eq.2)p(j,j-1)=10.
      enddo
      do j=1,nimages+1
        do i=1,nimages
          vect(i)=p(j,i)
        enddo
        y(j)=funk(vect,images,count,dn,noverlaps,
     +    maxpix,maxoverlaps,maximages,mode,nimages,inertia)
      enddo

c solve for biases
      call amoeba(p,y,mp,np,ndim,ftol,funk,iter,
     +  images,count,dn,noverlaps,
     +  maxpix,maxoverlaps,maximages,mode,nimages,inertia)
      do i=1,nimages
        bias(i)=p(1,i)
      enddo
      write(*,*)'required ',iter,' iterations'

c find images with no overlaps (disconnected images)
      do i=1, nimages
        has_overlaps(i) = .false.
      enddo
      do i=1, noverlaps
        do j=1, count(i)
          has_overlaps(images(j,i)) = .true.
        enddo
      enddo

c rescale the biases to preserve the mean dn of the input mosaic.
c Anything not connected receives a no-op bias.

      sum=0.0
      n = 0
      do i=1,nimages
        if (has_overlaps(i)) then
          sum=sum+bias(i)
          n = n+1
        endif
      enddo
      sum=sum/n
      if(mode.eq.1)then ! multiplicative
        do i=1,nimages
          if (has_overlaps(i)) then
            bias(i)=bias(i)/sum
          else
            bias(i) = 1.0
          endif
        enddo
      else              ! additive
        do i=1,nimages
          if (has_overlaps(i)) then
            bias(i)=bias(i)-sum
          else
            bias(i) = 0.0
          endif
        enddo
      endif

c write bias output
      call xvpone('OUT',filename,1,120)
      call write_bias_file(filename,bias,maximages,nimages)

c write script
      call xvpone('OUT',filename,2,120)
      call write_script_file(filename,bias,maximages,nimages,
     + filenames,mode)

      return
      end

c********************************************************************
      function funk(bias,images,count,dn,noverlaps,
     +  maxpix,maxoverlaps,maximages,mode,nimages,inertia)
      
      integer*4 images(maxpix,maxoverlaps),count(maxoverlaps),nimages
      real*4 dn(maxpix,maxoverlaps),bias(maximages),inertia
      real*8 sum, err

      sum=0.d0
      if(mode.eq.1)then
        do i=1,noverlaps
          do j=1,count(i)-1
            do k=j+1,count(i)
              err=abs(1.0-(bias(images(j,i))*dn(j,i))/
     +                      (bias(images(k,i))*dn(k,i)))
              sum=sum+err*err
c              sum=sum+abs((bias(images(j,i))*dn(j,i))-
c     +                      (bias(images(k,i))*dn(k,i)))
            enddo
          enddo
        enddo
      else if(mode.eq.2)then
        do i=1,noverlaps
          do j=1,count(i)-1
            do k=j+1,count(i)
c              sum=sum+abs(1.0-(bias(images(j,i))+dn(j,i))/
c     +                      (bias(images(k,i))+dn(k,i)))
              err=abs((bias(images(j,i))+dn(j,i))-
     +                      (bias(images(k,i))+dn(k,i)))
              sum=sum+err*err
            enddo
          enddo
        enddo
      else
        write(*,*)'Funk illegal mode'
        call abend()
      endif

      do i=1,nimages
        if (mode.eq.1) then
          sum = sum + abs(1.0-bias(i))*inertia
        else if (mode.eq.2) then
          sum = sum + abs(bias(i))*inertia
        endif
      enddo
      funk=sum
      
      return
      end

c**********************************************************************
      SUBROUTINE AMOEBA(P,Y,MP,NP,NDIM,FTOL,FUNK,ITER,
     +  images,count,dn,noverlaps,
     +  maxpix,maxoverlaps,maximages,mode,nimages,inertia)
     
      PARAMETER (NMAX=2000,ALPHA=1.0,BETA=0.5,GAMMA=2.0,ITMAX=1000000)
      DIMENSION P(MP,NP),Y(MP),PR(NMAX),PRR(NMAX),PBAR(NMAX)
      integer*4 images(maxpix,maxoverlaps),count(maxoverlaps),nimages
      real*4 dn(maxpix,maxoverlaps),inertia
      
      MPTS=NDIM+1
      ITER=0
1     ILO=1
      IF(Y(1).GT.Y(2))THEN
        IHI=1
        INHI=2
      ELSE
        IHI=2
        INHI=1
      ENDIF
      DO 11 I=1,MPTS
        IF(Y(I).LT.Y(ILO)) ILO=I
        IF(Y(I).GT.Y(IHI))THEN
          INHI=IHI
          IHI=I
        ELSE IF(Y(I).GT.Y(INHI))THEN
          IF(I.NE.IHI) INHI=I
        ENDIF
11    CONTINUE
      RTOL=2.*ABS(Y(IHI)-Y(ILO))/(ABS(Y(IHI))+ABS(Y(ILO)))
      IF(RTOL.LT.FTOL)RETURN
      IF(ITER.EQ.ITMAX)then
        write(*,*)'Amoeba exceeding maximum iterations.'
        return
      endif
      ITER=ITER+1
      DO 12 J=1,NDIM
        PBAR(J)=0.
12    CONTINUE
      DO 14 I=1,MPTS
        IF(I.NE.IHI)THEN
          DO 13 J=1,NDIM
            PBAR(J)=PBAR(J)+P(I,J)
13        CONTINUE
        ENDIF
14    CONTINUE
      DO 15 J=1,NDIM
        PBAR(J)=PBAR(J)/NDIM
        PR(J)=(1.+ALPHA)*PBAR(J)-ALPHA*P(IHI,J)
15    CONTINUE
      YPR=FUNK(PR,images,count,dn,noverlaps,
     +    maxpix,maxoverlaps,maximages,mode,nimages,inertia)
      IF(YPR.LE.Y(ILO))THEN
        DO 16 J=1,NDIM
          PRR(J)=GAMMA*PR(J)+(1.-GAMMA)*PBAR(J)
16      CONTINUE
        YPRR=FUNK(PRR,images,count,dn,noverlaps,
     +    maxpix,maxoverlaps,maximages,mode,nimages,inertia)
        IF(YPRR.LT.Y(ILO))THEN
          DO 17 J=1,NDIM
            P(IHI,J)=PRR(J)
17        CONTINUE
          Y(IHI)=YPRR
        ELSE
          DO 18 J=1,NDIM
            P(IHI,J)=PR(J)
18        CONTINUE
          Y(IHI)=YPR
        ENDIF
      ELSE IF(YPR.GE.Y(INHI))THEN
        IF(YPR.LT.Y(IHI))THEN
          DO 19 J=1,NDIM
            P(IHI,J)=PR(J)
19        CONTINUE
          Y(IHI)=YPR
        ENDIF
        DO 21 J=1,NDIM
          PRR(J)=BETA*P(IHI,J)+(1.-BETA)*PBAR(J)
21      CONTINUE
        YPRR=FUNK(PRR,images,count,dn,noverlaps,
     +    maxpix,maxoverlaps,maximages,mode,nimages,inertia)
        IF(YPRR.LT.Y(IHI))THEN
          DO 22 J=1,NDIM
            P(IHI,J)=PRR(J)
22        CONTINUE
          Y(IHI)=YPRR
        ELSE
          DO 24 I=1,MPTS
            IF(I.NE.ILO)THEN
              DO 23 J=1,NDIM
                PR(J)=0.5*(P(I,J)+P(ILO,J))
                P(I,J)=PR(J)
23            CONTINUE
              Y(I)=FUNK(PR,images,count,dn,noverlaps,
     +    maxpix,maxoverlaps,maximages,mode,nimages,inertia)
            ENDIF
24        CONTINUE
        ENDIF
      ELSE
        DO 25 J=1,NDIM
          P(IHI,J)=PR(J)
25      CONTINUE
        Y(IHI)=YPR
      ENDIF
      GO TO 1
      END

c***********************************************************************
      subroutine read_filenames(filename,filenames,maximages,
     +  nimages)
     
      character*120 filename
      character*120 filenames(maximages)
      
      open(unit=10,file=filename,
     + access='SEQUENTIAL',
     + form='FORMATTED',iostat=ios,status='OLD')
      if(ios.gt.0)then
        write(*,*)'cannot open filename file:'
        write(*,*)filename
        call abend()
      endif
      
      nimages=0
5     nimages=nimages+1
      if(nimages.gt.maximages)then
        write(*,*)'Increase MAXIMAGES parameter'
        call abend()
      endif

      read(unit=10,fmt=1,iostat=ios) filenames(nimages)
      if(ios.gt.0)then
        write(*,*)'read error on calibration file'
        stop
      endif
      if(ios.lt.0)then  ! EOF
        nimages=nimages-1
        goto 10
      endif
      goto 5
10    close(unit=10)
1     format(A120)

      return
      end

c***********************************************************************
      subroutine read_overlap_file(filename,
     + maxoverlaps,maxpix,count,noverlaps,images,dn)
     
      character*120 filename
      real*4 dn(maxpix,maxoverlaps)
      integer*4 count(maxoverlaps)
      integer*4 images(maxpix,maxoverlaps)
      
      open(unit=10,file=filename,
     + access='SEQUENTIAL',
     + form='FORMATTED',iostat=ios,status='OLD')
      if(ios.gt.0)then
        write(*,*)'cannot open overlap file:'
        write(*,*)filename
        call abend
      endif
      
      noverlaps=0
5     noverlaps=noverlaps+1
      if(noverlaps.gt.maxoverlaps)then
        write(*,*)'Increase MAXOVERLAPS parameter'
        call abend()
      endif

      read(unit=10,fmt=*,iostat=ios) n
      if(n.gt.maxpix)then
        write(*,*)'Increase MAXPIX parameter'
        call abend()
      endif
      count(noverlaps)=n
      if(ios.gt.0)then
        write(*,*)'read error on calibration file'
        stop
      endif
      if(ios.lt.0)then  ! EOF
        noverlaps=noverlaps-1
        goto 10
      endif

      read(unit=10,fmt=*,iostat=ios),
     + (images(k,noverlaps),k=1,n),
     + (dn(k,noverlaps),k=1,n)
      if(ios.gt.0)then
        write(*,*)'read error on overlap file'
        stop
      endif
      if(ios.lt.0)then  ! EOF
        write(*,*)'EOF error reading overlap file'
        stop
      endif

      goto 5
10    close(unit=10)

      return
      end

c***********************************************************************
      subroutine write_bias_file(filename,bias,maximages,nimages)
      character*120 filename
      real*4 bias(maximages)

      open(unit=10,file=filename,
     + access='SEQUENTIAL',
     + form='FORMATTED',iostat=ios,status='UNKNOWN')
      if(ios.gt.0)then
        write(*,*)'cannot open bias file'
        call abend()
      endif

      do k=1,nimages
        write(unit=10,fmt=*,iostat=ios) bias(k)
        if(ios.gt.0)then
          write(*,*)'write error on bias file'
          stop
        endif
        if(ios.lt.0)then  ! EOF
          write(*,*)'EOF error on bias file'
          stop
        endif
      enddo

      close(unit=10)
      return
      end


c***********************************************************************
      subroutine write_script_file(filename,bias,maximages,nimages,
     + filenames,mode)
      character*120 filename,msg
      character*120 filenames(maximages)
      real*4 bias(maximages)

      open(unit=10,file=filename,
     + access='SEQUENTIAL',
     + form='FORMATTED',iostat=ios,status='UNKNOWN')
      if(ios.gt.0)then
        write(*,*)'cannot open bias file'
        call abend()
      endif

      write(unit=10,fmt=*,iostat=ios)'procedure'
      write(unit=10,fmt=*,iostat=ios)'refgbl $echo'
      write(unit=10,fmt=*,iostat=ios)'body'
      write(unit=10,fmt=*,iostat=ios)'let _onfail="continue"'
      write(unit=10,fmt=*,iostat=ios)'let $echo="yes"'

      do k=1,nimages

        msg(1:120)=' '
        i=index(filenames(k),' ')
        msg(1:12)='marsrad inp='
        j=13
        msg(j:j+i-1)=filenames(k)(1:i-1)
        j=j+i-1
        msg(j:j+9)=' out=a.img'
        j=j+9
        write(unit=10,fmt=*,iostat=ios) msg
        if(ios.gt.0)then
          write(*,*)'write error on script file'
          stop
        endif
        if(ios.lt.0)then  ! EOF
          write(*,*)'EOF error on bias file'
          stop
        endif

        if(mode.eq.1)then
          write(unit=10,fmt=1,iostat=ios)k,bias(k)
1         format('f2 inp=a.img out=p',i3.3,'.img func="in1*',
     +           e11.5,'"')
        else
          b=abs(bias(k))
          if(bias(k).ge.0.0)then
            write(unit=10,fmt=2,iostat=ios)k,b
2           format('f2 inp=a.img out=p',i3.3,'.img func="in1+',
     +             e10.4,'"')
          else
            write(unit=10,fmt=3,iostat=ios)k,b
3           format('f2 inp=a.img out=p',i3.3,'.img func="in1-',
     +             e10.4,'"')
          endif
        endif
        if(ios.gt.0)then
          write(*,*)'write error on script file'
          stop
        endif
        if(ios.lt.0)then  ! EOF
          write(*,*)'EOF error on bias file'
          stop
        endif
        
      enddo

      write(unit=10,fmt=*,iostat=ios)'end-proc'

      close(unit=10)
      return
      end
