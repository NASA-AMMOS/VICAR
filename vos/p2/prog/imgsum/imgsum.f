C
C       VICAR PROGRAM IMGSUM 
C
C---------------------------------------------------------

        include 'VICMAIN_FOR'
        subroutine main44
        implicit none
        integer*4 insamps,inlines
        parameter (insamps = 20000)
        parameter (inlines = 20000)
c
	integer*4 iunit,status,icode,sl,ss,nl,ns,nli,nsi,nbi,ind
	integer*4 i,j,nlx,nsx,el,es,rows,nout,ounit,ibis
	integer*4 columns,abendcode,nreal,realrecord,count
	integer*4 numpix,numzero,numneg,numpos
	integer*4 ibis_column_find,ibis_group_new
	integer*4 parb(500),xcont,xadd,stat
	real*4 iimage(inlines,insamps)
	real*4 rbuf(7)
	logical*4 ibout
	real*8 sumall,sumneg,sumpos
        character*4 fmt(4)/'BYTE','HALF','FULL','REAL'/
        character*5 format,ibfmt(8)
        character*8 org,ttype(7)
	character*16 iborg,catfile
c
	data ttype /'ROW','NUMPIX','NUMPOS','SUMPOS','NUMNEG','SUMNEG','NUMZERO'/
	data ibfmt /'REAL','REAL','REAL','REAL','REAL','REAL','REAL','REAL'/
	data catfile /'IMGSUM CATALOG'/
c	
        call xvmessage ('IMGSUM version 2015-11-05',' ')
! PARAMETER PROCESSING 

	stat = 0;
	abendcode=0
	iborg='ROW'
	ibout = .false.
	call xvpcnt ( 'OUT',  nout )
	if (nout.gt.0) ibout=.true.

        call xvunit(iunit,'INP',1,status,' ')
        call xvopen(iunit,status,'OPEN_ACT','SA','IO_ACT','SA',' ')

        call xvget(iunit,status,'FORMAT',format,'ORG',org,' ')
	call xvget(iunit,status,'NL',nli,'NS',nsi,'NB',nbi,' ')

	if (nbi.gt.1) then
		call xvmessage('??E - Multiband images not supported',' ')
                call abend
        endif

	icode = 0
	if (format.eq.'BYTE') icode=1
	if (format.eq.'HALF' .or. format.eq.'WORD') icode=2
	if (format.eq.'FULL') icode=3
	if (format.eq.'REAL') icode=4
	if (icode.eq.0) then
                call xvmessage('??E - Unknown data format for input image',' ')
                call abend
        endif
        call xvclose(iunit,status,' ')

        call xvopen(iunit,status,'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')
        if (status.ne.1) call xvsignal(iunit,status,1)

        call xvsize( sl, ss, nl, ns, nlx,nsx)

c	print *,"sl,ss,nl,ns,nlx,nsx = ",sl,ss,nl,ns,nlx,nsx
	if (nl.eq.0) nl=nli
	if (ns.eq.0) ns=nsi
	numpix = nl*ns
	sumall = 0.0d0
	sumpos = 0.0d0
	sumneg = 0.0d0
	numneg = 0
	numpos = 0
	numzero = 0 
       do i = 1,nli
            call xvread (iunit,iimage(1,i),ind,' ')
        enddo
c
	el = sl + nl - 1 
	es = ss + ns - 1
            do i = sl,el
                do j = ss,es
		    if (iimage(j,i).eq.0.0) then
			numzero = numzero + 1
                    elseif (iimage(j,i).lt.0.0) then
			numneg = numneg + 1
			sumneg = sumneg + dble(iimage(j,i))
			sumall = sumall + dble(iimage(j,i))
		    else
			numpos = numpos + 1
			sumpos = sumpos + dble(iimage(j,i))
                        sumall = sumall + dble(iimage(j,i))
		    endif
                ENDdo
            enddo

       call xvclose(iunit,ind,' ')
c
c	if output file
c
	if (ibout) then
	    call xvunit(ounit,'OUT',1,stat,' ')
	    call chkstat (stat,'??E XVopen error on output ibis file',1,0,0)
	    columns = 7
	    rows = 1
	    nreal = 0
	    call ibis_file_open(ounit,ibis,'WRITE',columns,rows,
     1      ibfmt,iborg,stat)
	    if (stat.ne.1) call ibis_signal_u (ounit,stat,1)
	    call ibis_file_set(ibis,'type',catfile,stat)
	    if (stat.lt.0) call ibis_signal(ibis,stat,abendcode)
	    nreal=ibis_column_find(ibis,'format','REAL',0,0,0)
            if (nreal.lt.0) call ibis_signal (ibis,stat,abendcode)
            do i=1,columns
               count=ibis_group_new(ibis,'group',ttype(i),i,1,' ')
               if (count.lt.0) call ibis_signal(ibis,count,abendcode)
            enddo

            call ibis_record_open(ibis,realrecord,'format:real',0,0,
     1        'real',stat)
            if (stat.ne.1) call ibis_signal(ibis,stat,abendcode)
c       if (stat.ne.1) call ibis_signal(ibis,stat,abendcode)
	
	    rbuf(1) = real(rows)
	    rbuf(2) = real(numpix)
	    rbuf(3) = real(numpos)
	    rbuf(4) = sngl(sumpos)
	    rbuf(5) = real(numneg)
	    rbuf(6) = sngl(sumneg)
	    rbuf(7) = real(numzero)
	    call ibis_record_write (realrecord,rbuf,rows,stat)
            if (stat.ne.1) call ibis_signal (ibis,stat,abendcode)
	    call ibis_file_set (ibis,'NR',1,stat)
	    if (stat .ne. 1) call ibis_signal(ibis,stat,1)
	    call ibis_file_close (ibis,' ',stat)
            if (stat.ne.1) call ibis_signal_u (ounit,stat,1)

	endif
c    output parm        
        call xqini (parb,500,xcont)
        call xqreal (parb,'SUM',1,sngl(sumall),xadd,stat)
	call xqreal (parb,'SUMPOS',1,sngl(sumpos),xadd,stat)
	call xqreal (parb,'SUMNEG',1,sngl(sumneg),xadd,stat)
        call xqintg (parb,'NUMZERO',1,numzero,xadd,stat)
	call xqintg (parb,'NUMPOS',1,numpos,xadd,stat)
	call xqintg (parb,'NUMNEG',1,numneg,xadd,stat)
	call xqintg (parb,'NUMPIX',1,numpix,xadd,stat)
        call xqout (parb,stat)
        call chkstat (stat,'??E - XQout error',0,0,0)

	return
	end 
