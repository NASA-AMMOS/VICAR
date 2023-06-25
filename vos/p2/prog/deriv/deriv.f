	include 'VICMAIN_FOR'
	subroutine main44
	implicit none
c
c	Creates 1st derivative of a image in horizontal
c	or vertical direction.
c	
	integer*4 insamps,inlines
	parameter (insamps = 10000)
	parameter (inlines = 10000)
	integer*4 iunit,ounit,ounit2,ind,icode,icnt,idef
	integer*4 i,j,nli,nsi,nb,posinflect,neginflect
	real*4 oimage(inlines,insamps)
	real*4 iimage(inlines,insamps)
	integer*4 inflect(inlines,insamps)
	integer*4 parb(500),xcont,xadd,stat

	character*4 fmt(4)/'BYTE','HALF','FULL','REAL'/
	character*5 format
	character*6 orient
	character*8 org

	call xvmessage('DERIV version 2015-11-19',' ')
	call xvunit(iunit,'INP',1,ind,' ')
	call xvsignal(iunit,ind,.TRUE.)
	call xvopen(iunit,ind,'OPEN_ACT','SA','IO_ACT','SA',' ')

	call xvget(iunit,ind,'FORMAT',format,'ORG',org,' ')	

	icode = 0
	if (format.eq.'BYTE') icode=1
	if (format.eq.'HALF'.or.format.eq.'WORD') icode=2
	if (format.eq.'FULL') icode=3
	if (format.eq.'REAL') icode=4
	if (icode.eq.0) then
		call xvmessage('??E - Unknown data format for input image',' ')
		call abend  
	endif
	call xvclose(iunit,ind,' ')

	call xvopen(iunit,ind,'OPEN_ACT','SA','IO_ACT','SA',
     &		'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')		!FMT(INCODE),' ')

	call xvget(iunit,ind,'NL',nli,'NS',nsi,'NB',nb,' ')
c	print *, 'nli = ',nli,' nsi = ',nsi
	call xvparm ('ORIENT',orient,icnt,idef,1)

	call xvunit(ounit,'OUT',1,ind,' ' )
	call xvopen(ounit,ind,'OP','WRITE','U_NL',nli,'U_NS',nsi,
     & 'U_NB',nb,'OPEN_ACT','SA','IO_ACT','SA','O_FORMAT',fmt(icode),
     & 'U_FORMAT',fmt(4),' ')				!,FMT(OUTCODE),' ')
	call xvunit(ounit2,'OUT',2,ind,' ' )
        call xvopen(ounit2,ind,'OP','WRITE','U_NL',nli,'U_NS',nsi,
     & 'U_NB',nb,'OPEN_ACT','SA','IO_ACT','SA','O_FORMAT',fmt(3),
     & 'U_FORMAT',fmt(3),' ')                           !,FMT(OUTCODE),' ')

        do i = 1,nli
	    call xvread (iunit,iimage(1,i),ind,' ')
	enddo
	posinflect = 0
	neginflect = 0
	if (orient.eq.'HORIZ') then
	    do i = 1,nli
	    	oimage(1,i) = iimage(2,i) - iimage(1,i)
		inflect(1,i) = 0
		do j = 2,nsi
		    oimage(j,i) = iimage(j,i) - iimage(j-1,i)
                    inflect(j,i) = 0
                    if (oimage(j,i).le.0.0 .and. oimage(j-1,i).gt.0.0) then
			inflect(j,i) = 1
			posinflect = posinflect + 1
		    endif
		    if (oimage(j,i).ge.0.0 .and. oimage(j-1,i).lt.0.0) then
			inflect(j,i) = -1
			neginflect = neginflect + 1
		    endif
		enddo
c		if (i.eq.1) print *,'1,2 = ',oimage(1,i),iimage(2,i), iimage(1,i)
	    enddo
	else
 	    do i = 1,nli
		oimage(i,1) = iimage(i,2) - iimage(i,1)
		inflect(i,1) = 0
		do j = 2,nsi
		    oimage(i,j) = iimage(i,j) - iimage(i,j-1)
		    inflect(i,j) = 0
		    if (oimage(i,j).le.0.0 .and. oimage(i,j-1).gt.0.0) then
			inflect(i,j) = 1
			posinflect = posinflect + 1
		    endif
                    if (oimage(i,j).ge.0.0 .and. oimage(i,j-1).lt.0.0) then
			inflect(i,j) = -1
		        neginflect = neginflect + 1
		    endif
		enddo
	    enddo
c	    call xvmessage('??E - VERT is not implemented yet',' ')
c	    call abend
	endif


	do i=1,nli
		call xvwrit(ounit,oimage(1,i),ind,' ')
		call xvwrit(ounit2,inflect(1,i),ind,' ')
	enddo

	call xvclose(iunit,ind,' ')
	call xvclose(ounit,ind,' ')
	call xvclose(ounit2,ind,' ' )
c    output parm	
	call xqini (parb,500,xcont)
	call xqintg (parb,'POSINFL',1,posinflect,xadd,stat)
	call xqintg (parb,'NEGINFL',1,neginflect,xadd,stat)
	call xqout (parb,stat)
        call chkstat (stat,'??E - XQout error',0,0,0)

 	continue
	return
	end

