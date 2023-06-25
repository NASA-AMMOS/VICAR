	include 'VICMAIN_FOR'
	subroutine main44
c
c       program to transfer vicar files to FITS files
c       Made compatible with Linux and MacOSX 64-bit compilers
C       01-30-2010 - R. J. Bambery

	implicit none
	integer*4 iuni,ouni1,ouni2,onl,ons,onb,trlab,ofnl,tsklen,syslen
	integer*4 ipixsize,opixsize,obitpix,ibitpix,bfctr,ntasks
	logical*4 noproc,prfits,ihst,itask,iend,iblk,dbg
	real*4 bscale,bzero
	character*50 version
	character*132 filename
	character*80 fx(50)
c
	common /cpm/noproc,prfits,ihst,itask,iend,iblk,bfctr,filename,version
	common /cf1/iuni,ouni1,ouni2,ofnl,ipixsize,opixsize,obitpix,ibitpix
	common /cf2/onl,ons,onb,bscale,bzero,trlab,tsklen,syslen,ntasks,dbg
c
	common /fitsx/fx
c
c
        data iend/.false./,ihst/.true./,iblk/.false./
        data itask/.false./,dbg/.false./
c
	integer*4 stat
	character*80 outline
c
	data version /'FITSOUT version 2016-02-29'/ 
c
	data syslen /35/ 	!see note at end of subroutine CRFITSH
c
	write(outline,10100) version
10100	format(20a1)
	call xvmessage(version,' ')
c
	stat=0				!return status word for xv routines
	call parmproc			!process FITSOUT parameters
	call scaler			!set up BSCALE/BZERO values
	call crfitsh			!create and list FITS "system" header

	if (ihst) call vichist		!create and list FITS "VICAR" history

	if (.not.noproc) call fitsoutdata	
	CALL XVCLOSE(IUNI,STAT,' ')
	return
	end 
C========================================================================
	subroutine parmproc
C
C	ROUTINE TO PROCESS FITSOUT PARAMETERS
C
	implicit none
	include 'errdefs'
c
	integer*4 stat,istat,ostat1,ostat2,cnt
	integer*4 nods,nids,fitsns,fitsnl,ivmslen
	integer*4 totlab,totbytes
	real*4 fnl,diff,ototlab,vmslen
	character*9 hist,blktape
	character*32 reqdat,debugmode
	character*80 outline
	character*132 filename2
c
	byte fitsdata(32000)
	integer*4 iuni,ouni1,ouni2,onl,ons,onb,trlab,ofnl,tsklen,syslen,case
	integer*4 ipixsize,opixsize,obitpix,ibitpix,bfctr,ntasks,linectr
	integer*4 instances(1000),inl,ins,inb,ihival,iloval
	logical*4 noproc,prfits,ihst,itask,iend,iblk,dbg
	real*4 bscale,bzero,hival,loval,fitshi,fitslo
	character*6 intfmt,realfmt
	character*8 host
	character*50 version
	character*32 tasks(1000),itype,inorg,ifmt
	character*80 hdrrecord(360)
	character*132 filename
	character*80 fx(50)
c
	common /cpm/noproc,prfits,ihst,itask,iend,iblk,bfctr,filename,version
	common /cf1/iuni,ouni1,ouni2,ofnl,ipixsize,opixsize,obitpix,ibitpix
	common /cf2/onl,ons,onb,bscale,bzero,trlab,tsklen,syslen,ntasks,dbg
	common /cf3/itype,ifmt,inorg,host,intfmt,realfmt,case
	common /cf4/instances,tasks,linectr,inl,ins,inb
	common /cf5/hival,loval,ihival,iloval,fitshi,fitslo
	common /cdata/fitsdata,hdrrecord
	common /fitsx/fx
c
	ostat1=0				! ASSUME NO "OUT" PARM ENTERED
	ostat2=0				!assume no "HEADER" parm entered
	noproc=.true.			!Output not to be done
	call xvp('DEBUG',debugmode,cnt)	!check for debug mode
	if (debugmode.eq.'DEBUG') then
		dbg=.true.
		write (outline,10050)
10050 format ('***FITSOUT in debug mode')
		call xvmessage (outline,' ')
	endif
	call xvunit (iuni,'INP',1,istat,' ')
	call chkstat(istat,'??E XVunit error on input ',0,0,0)
c===parameter=INP
	call xvp('INP',filename,nids)
	if (dbg) then
		write (outline,10100) filename
10100	format ('VICAR filename = ',a60)
		call xvmessage (outline,' ')
	endif
c
c	open VICAR input data set
c
	call xvopen(iuni,istat,'OPEN_ACT','SA','IO_ACT','SA',' ')
c
c itype possibilities: 'IMAGE','PARM','PARAM','GRAPH1','GRAPH2'
c			'GRAPH3','TABULAR'
c ifmt possibilities: 'BYTE','HALF','FULL','REAL','DOUB','COMP'
c			'WORD' or 'COMPLEX' are older possibilities
c inorg possibilities: 'BSQ','BIL', or 'BIP'
c
	call xvget(iuni,istat,'ORG',inorg,'TYPE',itype,'FORMAT',ifmt,' ')
	call chkstat(istat,'??E XVget error on ORG/TYPE/FORMAT',0,0,0)
	if (DBG) then
		write (outline,10200) inorg
10200	format ('VICAR file organization = ',a32)
		call xvmessage (outline,' ')
	endif
	if (inorg.ne.'BSQ') then
	   write (outline,10210) 
10210	   format ('??E ** FITSOUT - Only File org BSQ allowed as input')
	   call xvmessage (outline,' ')
	   call abend
	endif
	if (dbg) then
		write (outline,10300) itype
10300	format ('VICAR file type = ',a32)
		call xvmessage (outline,' ')
	endif
	if (itype.ne.'IMAGE') then
		write (outline,10350)
10350	format ('??E FITSOUT - Only Filetype IMAGE allowed as input')
	 	call xvmessage (outline,' ')
		call abend
	endif
	if (dbg) then
		write (outline,10400) ifmt
10400	format ('VICAR format = ',a32)
		call xvmessage (outline,' ')
	endif
	if (ifmt.eq.'BYTE') ibitpix=8
	if (ifmt.eq.'HALF') ibitpix=16
	if (ifmt.eq.'FULL') ibitpix=32
	if (ifmt.eq.'REAL') ibitpix=-32
	if (ifmt.eq.'DOUB') ibitpix=-64
	if (ifmt.eq.'COMP') ibitpix=-33			!not a valid FITS data type
	if (ifmt.eq.'WORD') ibitpix=32
	if (ifmt.eq.'COMPLEX') ibitpix=-33		!not a valid FITS data type

	call xvget (iuni,istat,'NL',inl,'NS',ins,'NB',inb,' ')
	call chkstat (istat,'??E XVGET error on NL/NS/NB',0,0,0)
	call xvget (iuni,istat,'PIX_SIZE',ipixsize,' ')
	call chkstat (istat,'??E XVGET error on PIX_SIZE',0,0,0)
c
cPARM OUTDATA TYPE=STRING COUNT=(0:1)
c	VALID=(VICAR,8,16,32,-32,FLOAT,SCALE8,SCALE16)
c	DEFAULT=VICAR
c
c	
c	25 cases	ibitpix		obitpix		SCALE/NOSCALE
c	case 1		16		8		scale
c	case 2		32		8		scale
c	case 3		32		16		scale
c	case 4		-32		8		scale
c	case 5		-32		16		scale
c	case 6		-32		32		scale
c	case 7		-64		8		scale
c	case 8		-64		16		scale
c	case 9		-64		32		scale
c	case 10		-64		-32		scale
c	case 11		8		8		noscale
c	case 12		16		16		noscale
c	case 13		32		32		noscale
c	case 14		-32		-32		noscale
c	case 15		-64		-64		noscale
c	case 16		8		16		noscale
c	case 17		8		32		noscale
c	case 18		8		-32		noscale
c	case 19		8		-64		noscale
c	case 20		16		32		noscale
c	case 21		16		-32		noscale
c	case 22		16		-64		noscale
c	case 23		32		-32		noscale
c	case 24		32		-64		noscale
c	case 25		-32		-64		noscale
c	
c=== parameter=OUTDATA	
	call xvp('OUTDATA',reqdat,cnt)
	if (reqdat.eq.'VICAR') then
		opixsize=ipixsize
		obitpix=ibitpix
		if (ibitpix.eq.8) case=11
		if (ibitpix.eq.16) case=12
		if (ibitpix.eq.32) case=13
		if (ibitpix.eq.-32) case=14
		if (ibitpix.eq.-64) then
			case=15
			write (outline,10403) 
10403 format ('??E FITSOUT - VICAR data type = COMPLEX - cannot copy')
			call xvmessage (outline,' ')
			call abend
		endif
	endif
	if (reqdat.eq.'8') then
		opixsize=1
		obitpix=8
		if (ibitpix.eq.8) case=11
		if (ibitpix.eq.16) case=1
		if (ibitpix.eq.32) case=2
		if (ibitpix.eq.-32) case=4
		if (ibitpix.eq.-64) case=7
	endif
	if (reqdat.eq.'16') then
		opixsize=2
		obitpix=16
		if (ibitpix.eq.8) case=16
		if (ibitpix.eq.16) case=12
		if (ibitpix.eq.32) case=3
		if (ibitpix.eq.-32) case=5
		if (ibitpix.eq.-64) case=8
	endif
	if (reqdat.eq.'32') then
		opixsize=4
		obitpix=32
		if (ibitpix.eq.8) case=17
		if (ibitpix.eq.16) case=20
		if (ibitpix.eq.32) case=13
		if (ibitpix.eq.-32) case=6
		if (ibitpix.eq.-64) case=9
	endif
	if (reqdat.eq.'FLOAT') then
		opixsize=4
		obitpix=-32
		if (ibitpix.eq.8) case=18
		if (ibitpix.eq.16) case=21
		if (ibitpix.eq.32) case=23
		if (ibitpix.eq.-32) case=14
		if (ibitpix.eq.-64) case=10
	endif
	if (reqdat.eq.'-32') then
		opixsize=4
		obitpix=-32
		if (ibitpix.eq.8) case=18
		if (ibitpix.eq.16) case=21
		if (ibitpix.eq.32) case=23
		if (ibitpix.eq.-32) case=14
		if (ibitpix.eq.-64) case=10
	endif
	if (reqdat.eq.'SCALE8') then
		opixsize=1
		obitpix=8
		if (ibitpix.eq.8) case=11
		if (ibitpix.eq.16) case=1
		if (ibitpix.eq.32) case=2
		if (ibitpix.eq.-32) case=4
		if (ibitpix.eq.-64) case=7
	endif
	if (reqdat.eq.'SCALE16') then
		opixsize=2
		obitpix=16
		if (ibitpix.eq.8) case=16
		if (ibitpix.eq.16) case=12
		if (ibitpix.eq.32) case=3
		if (ibitpix.eq.-32) case=5
		if (ibitpix.eq.-64) case=8
	endif
c
c	following is not yet allowed
c
	if (reqdat.eq.'DOUBLE') then
		opixsize=8
		obitpix=-64
		if (ibitpix.eq.8) case=19
		if (ibitpix.eq.16) case=22
		if (ibitpix.eq.32) case=24
		if (ibitpix.eq.-32) case=25
		if (ibitpix.eq.-64) case=15
		write (outline,10405) 
10405		format ('??E FITSOUT - OUTDATA="DOUBLE" is illegal')
		call xvmessage (outline,' ')
		call abend
	endif
	if (DBG) then
		write (outline,10407)
10407	format ('FITS OUTPUT FORMAT follows')
		call xvmessage (outline,' ')
		write (outline,10410)
10410	format (' #lines    #samples # bands #pixelsize  #bits/pixel')
		call xvmessage (outline,' ')
		write (outline,10420)  inl,ins,inb,opixsize,obitpix
10420	format (i5,5x,i5,5x,i5,5x,i5,5x,i5)
		call xvmessage (outline,' ')
	endif
c
c possible values for host ="VAX-VMS","SUN-3","SUN-4","ALLIANT",
c			"DECSTATN","CRAY","MAC-AUX","MAC-MPW","SGI",
c			"TEK","HP-700","MAC-OSX"
c possible values for intfmt = "HIGH","LOW","NATIVE","LOCAL"
c possible values for realfmt = "VAX","IEEE","RIEEE","NATIVE","LOCAL"
c
c note: early VICAR labels (ca. 1991) did not have these 3 entries
c
c 2/24/04 - fixed these 3 calls with 'FORMAT','STRING'
	call xlget (iuni,'SYSTEM','HOST',host,stat,'FORMAT','STRING',' ')
cc	print *, 'stat = ',stat,' HOST = ',host
	if (stat.ne.1) then
		if (stat.eq.CANNOT_FIND_KEY) host='VAX-VMS'
	else
		call chkstat (stat,'??E XLGET error on host type',0,0,0)
	endif
c  integer format 'HIGH' is "big-endian"
	call xlget (iuni,'SYSTEM','INTFMT',intfmt,stat,'FORMAT','STRING',' ')
cc	print *,'INTFMT = ', intfmt
	if (stat.ne.1) then
		if (stat.eq.CANNOT_FIND_KEY) intfmt='LOW'
	else
		call chkstat (stat,'??E XLGET error on integer format',0,0,0)
	endif
	call xlget (iuni,'SYSTEM','REALFMT',realfmt,stat,'FORMAT','STRING',' ')
	if (stat.ne.1) then
		if (stat.eq.CANNOT_FIND_KEY) realfmt='VAX'
	else
		call chkstat (stat,'??E XLGET error on real format',0,0,0)
	endif

	continue
c	IBLK=.true. for "BLOCK", .false. for "NOBLOCK"
c===parameter=BLOCK
	call xvp('BLOCK',blktape,cnt)		!blocking factor for tapes
	if (blktape.eq.'BLOCK') iblk=.true.
c	
c===parameter=FACTOR
	call xvp('FACTOR',bfctr,cnt)
	if (iblk) then
c
c	bfctr=1 (2880), =2 (5760), =3 (8640), =4 (11520), =5 (14400)
c	bfctr=6 (17280), =7 (20160), =8 (23040), =9 (25920), =10 (28800)
c
c	FITS std - DONT allow blocking factor greater than 10
c
		if (bfctr.lt.1) bfctr=1
		if (bfctr.gt.10) then
			bfctr=10
			write (outline,10440)
10440	format ('??E FITSOUT - BLOCKING FACTOR GREATER THAN 10 DISALLOWED')
			call xvmessage (outline,' ')
			write (outline,10450)
10450	format ('**FITSOUT - BLOCKING FACTOR SET TO 10')
			call xvmessage (outline,' ')
		endif
	else
		bfctr=1
	endif
c
c	ihst=.true. for "HISTORY", .false. for "NOHISTORY"
c	itask=.true. and ihst=.true. for "TASKS"
c	default coming into code is ihst=.true. itask=.false.
c
c===parameter=HISTORY
	CALL XVP('HISTORY',hist,cnt)
	if (hist.eq.'NOHISTORY') ihst=.false.
	if (hist.eq.'TASKS') then
		itask=.true.
		ihst=.true.
	endif
c
c	compute number of 2880-byte records in output image data
c	we will compute header data later
c
	fnl=inl*ins*inb*opixsize	!total bytes
	fnl=fnl/(2880.*bfctr)		!want remainder
	ofnl=fnl
	diff=fnl-ofnl
	if (diff.gt.0.0) ofnl=ofnl+1
	onl=ofnl			!output image nl in FITS
	ons=2880*bfctr			!output image ns in FITS
	onb=inb				!output image nb in FITS
c
c	open output data sets
c
	call xvpcnt('OUT',nods)		!get number of output data sets
	if (nods.eq.1) then
	   CALL XVUNIT(OUNI1,'OUT',1,OSTAT1,' ')
	   if (ostat1.eq.1) noproc=.false.	!got output #1!
C
C	OPEN FITS (OUTPUT) DATA SET
C
	   CALL XVUNIT(OUNI1,'OUT',1,STAT,' ')
	   call hdrlength
	   totlab=tsklen+syslen	
		if (dbg) then
		   write (outline,10500) totlab
10500	   format ('totlab = ',i5)
	   	   call xvmessage (outline,' ')
		endif
c
c	compute total records: combine image records with header records
c
	   ototlab=totlab/(36.*bfctr)
	   totlab=totlab/(36*bfctr)
	   diff=ototlab-totlab
	   if (diff.gt.0.0) totlab=totlab+1
		if (dbg) then
		   write (outline,10600) totlab,bfctr 
10600 format ('total header records = ',i6,' for blocking factor = ',i6)
		   call xvmessage (outline,' ')
		endif
	   totbytes=inl*ins*inb*opixsize	
	   fitsns=(bfctr*2880)/opixsize		!output ns in FITS data + hdr
	   fitsnl=(onl+totlab)
	   if (dbg) then
		write (outline,10700) fitsnl,bfctr
10700 format ('FITS total records = ',i6,' for blocking factor = ',i6)
	   	call xvmessage (outline,' ')
	   endif
	   vmslen=(totbytes+totlab*2880.)/512.		!vms file blocking factor = 512 bytes
	   ivmslen=vmslen
	   diff=vmslen-ivmslen
	   if (diff.gt.0) ivmslen=ivmslen+1
	   if (dbg) then
		   write (outline,10800) ivmslen
10800 format ('End of VMS 512-byte data = ',i6)
		   call xvmessage (outline,' ')
	   endif
	   vmslen=fitsnl*2880*bfctr/512.
	   ivmslen=vmslen
	   diff=vmslen-ivmslen
	   if (diff.gt.0) ivmslen=ivmslen+1
	   if (dbg) then
		   write (outline,10900) ivmslen
10900 format ('Last VMS 512-byte block = ',i6)
		   call xvmessage (outline,' ')
	   endif
	   if (obitpix.eq.8) then
		CALL XVOPEN(OUNI1,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     1	'O_FORMAT','BYTE','OP','WRITE','COND','NOLABELS,NOBLOCKS',
     1	'U_FORMAT','BYTE','U_NS',fitsns,'U_NL',fitsnl,
     1	'CONVERT','OFF',' ')
	   endif
	   if (obitpix.eq.16) then
		CALL XVOPEN(OUNI1,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     1	'O_FORMAT','HALF','OP','WRITE','COND','NOLABELS,NOBLOCKS',
     1	'U_FORMAT','HALF','U_NS',fitsns,'U_NL',fitsnl,
     1	'CONVERT','OFF',' ')
	   endif
	   if (obitpix.eq.32) then
		CALL XVOPEN(OUNI1,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     1	'O_FORMAT','FULL','OP','WRITE','COND','NOLABELS,NOBLOCKS',
     1	'U_FORMAT','FULL','U_NS',fitsns,'U_NL',fitsnl,
     1	'CONVERT','OFF',' ')
	   endif
	   if (obitpix.eq.-32) then
		CALL XVOPEN(OUNI1,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     1	'O_FORMAT','REAL','OP','WRITE','COND','NOLABELS,NOBLOCKS',
     1	'U_FORMAT','REAL','U_NS',fitsns,'U_NL',fitsnl,
     1	'CONVERT','OFF',' ')
	   endif
	   call chkstat(stat,'??E XVopen err on FITS File ',0,0,0)
c
	   linectr=1			!Init output record count
	endif
c
	if (noproc.and.ihst) then
	   call hdrlength
	   totlab=tsklen+syslen	
	endif	
c
	prfits=.false.			!no FITS header output file
	call xvpcnt('HEADER',nods)	!see if header given
c===parameter=HEADER
	call xvp('HEADER',filename2,nods)
	if (nods.eq.1) then
		call xvunit(ouni2,'HEADER',1,ostat2,'U_NAME',filename2,' ')
		call chkstat(ostat2,'??E XVunit err HDR file ',0,0,0)
		if (ostat2.eq.1) prfits=.true.
		call xvopen(ouni2,stat,'OPEN_ACT','SA','IO_ACT','SA',
     1	'U_FORMAT','BYTE','O_FORMAT','BYTE','OP','WRITE','COND',
     2	'NOLABELS','U_ORG','BSQ','U_NL',trlab,'U_NS',80,
     3	'U_NB',1,' ')
		call chkstat(stat,'??E XVopen err HDR File ',0,0,0)
		linectr=1		!initialize in case no OUT= specified
	endif
	return
	end
C========================================================================
	subroutine scaler
c
c	routine to set up BSCALE and BZERO values
c
	implicit none
	byte fitsdata(32000)
	integer*4 iuni,ouni1,ouni2,onl,ons,onb,trlab,ofnl,tsklen,syslen,case
	integer*4 ipixsize,opixsize,obitpix,ibitpix,bfctr,ntasks,linectr
	integer*4 instances(1000),inl,ins,inb,ihival,iloval
	logical*4 prfits,noproc,ihst,iend,iblk,itask,dbg
	real*4 bscale,bzero,hival,loval,fitshi,fitslo
	character*6 intfmt,realfmt
	character*8 host
	character*50 version
	character*32 tasks(1000),itype,inorg,ifmt
	character*80 hdrrecord(360)
	character*132 filename
	character*80 fx(50)
c
	common /cpm/noproc,prfits,ihst,itask,iend,iblk,bfctr,filename,version
	common /cf1/iuni,ouni1,ouni2,ofnl,ipixsize,opixsize,obitpix,ibitpix
	common /cf2/onl,ons,onb,bscale,bzero,trlab,tsklen,syslen,ntasks,dbg
	common /cf3/itype,ifmt,inorg,host,intfmt,realfmt,case
	common /cf4/instances,tasks,linectr,inl,ins,inb
	common /cf5/hival,loval,ihival,iloval,fitshi,fitslo
	common /cdata/fitsdata,hdrrecord
	common /fitsx/fx
c
	integer*4 stat,b,i,imax,imin,indmax,indmin,dcode,irange
	real*4 rmax,rmin,range
	character*80 outline
c
	bscale=1.0
	bzero=0.0
	ihival=0
	iloval=0
	hival=0.0
	loval=0.0
	fitshi=0.0
	fitslo=0.0
	if (ibitpix.eq.8) dcode=1
	if (ibitpix.eq.16) dcode=2
	if (ibitpix.eq.32) dcode=4
	if (ibitpix.eq.-32) dcode=7
	if (ibitpix.eq.-64) dcode=8
	do b=1,inb
	   do i=1,inl
		call xvread (iuni,fitsdata,stat,'LINE',i,'BAND',b,' ')
		if (dcode.gt.4) then
		   call minmax(dcode,ins,fitsdata,rmin,rmax,indmin,indmax)
		   if (rmax.gt.hival) hival=rmax
		   if (rmin.lt.loval) loval=rmin
		else
		   call minmax(dcode,ins,fitsdata,imin,imax,indmin,indmax)
		   if (imax.gt.ihival) ihival=imax
		   if (imin.lt.iloval) iloval=imin
		endif
	   enddo
	enddo
	if (dcode.gt.4) then
	   fitshi=hival
	   fitslo=loval
	else
	   fitshi=ihival
	   fitslo=iloval
	endif
	if (dbg) then
	   write (outline,10050) case
10050	format ('CASE = ',i5)
	   call xvmessage (outline,' ')
	   write (outline,10100)
10100	format ('High Value/Low value (integer,real,FITS)')
	   call xvmessage (outline,' ')
	   write (outline,10110) ihival,iloval,hival,loval,fitshi,fitslo
10110	format (2i9,2x,'||',2e10.3,2x,'||',2e10.3)
	   call xvmessage (outline,' ')
	   if (case.gt.10) then
	   	write (outline,10200) bscale,bzero
10200	format ('BSCALE = ',e12.5,'    BZERO = ',e12.5)
	   	call xvmessage (outline,' ')
	   endif
	endif
	if (case.gt.10) return

c  case 1 - ibitpix=16 - obitpix=8			!HALF TO BYTE
c  case 2 - ibitpix=32 - obitpix=8			!FULL TO BYTE
	if (case.eq.1.or.case.eq.2) then
	   irange=ihival-iloval
	   if (irange.gt.255) then
		bscale=irange/255.0
		bzero=iloval
		if (bscale.gt.(1024./256.)) then	!greater than 10 bits
		   write (outline,10300) bscale
10300	format ('**FITSOUT - Caution - Each FITS value is ',f7.2,
     1          ' VICAR values')
		   call xvmessage (outline,' ')
		endif
	   else
c		bscale=1.0
		bzero=iloval
	   endif	  
	endif
c  case 3 - ibitpix=32 - obitpix=16			!FULL TO HALF
	if (case.eq.3) then
	   irange=ihival-iloval
	   if (irange.gt.32767) then
		bscale=irange/32767.
		bzero=iloval
		if (bscale.gt.(262144./32767.)) then	!greater than 18 bits
		write (outline,10300) bscale
		call xvmessage (outline,' ')
		endif
	   else
		bzero=iloval
	   endif
	endif
c  case 4 - ibitpix=-32 - obitpix=8			!REAL TO BYTE
	if (case.eq.4) then
	   range=hival-loval
	   if (range.gt.255.) then
		bscale=range/255.
		bzero=loval
		if (bscale.gt.(1024./256.)) then	!greater than 10 bits
		write (outline,10300) bscale
		call xvmessage (outline,' ')
		endif
	   else
		bzero=loval
	   endif
	endif
c  case 5 - ibitpix=-32 - obitpix=16			!REAL TO HALF
	if (case.eq.5) then
	   range=hival-loval
	   if (range.gt.32767.) then
		bscale=range/32767.
		bzero=loval
		if (bscale.gt.(262144./32767.)) then	!greater than 18 bits
		write (outline,10300) bscale
		call xvmessage (outline,' ')
		endif
	   else
		bzero=loval
	   endif
	endif
c  case 6 - ibitpix=-32 - obitpix=32			!REAL TO FULL
	if (case.eq.6) then
	   range=hival-loval
	   if (range.gt.2.0e9) then
		bscale=range/2.0e9
		bzero=loval
	   else
		bzero=loval
	   endif
	endif
c  case 7 - ibitpix=-64 - obitpix=8				!DOUB TO BYTE
c  case 8 - ibitpix=-64 - obitpix=16			!DOUB TO HALF
c  case 9 - ibitpix=-64 - obitpix=32			!DOUB TO FULL
c  case 10- ibitpix=-64 - obitpix=-32			!DOUB TO REAL
c
	if (case.le.10) then
		fitshi=(fitshi-bzero)/bscale
		fitslo=(fitslo-bzero)/bscale
	endif
	write (outline,10200) bscale,bzero
	call xvmessage (outline,' ')
	return
	end
C========================================================================
	subroutine hdrlength
c
c	routine to compute number of FITS header records
c
        implicit none
	include 'errdefs'
c
	byte fitsdata(32000)
	integer*4 iuni,ouni1,ouni2,onl,ons,onb,trlab,ofnl,tsklen,syslen,case
	integer*4 ipixsize,opixsize,obitpix,ibitpix,bfctr,ntasks,linectr
	integer*4 instances(1000),inl,ins,inb,ihival,iloval
	logical*4 prfits,noproc,ihst,iend,iblk,itask,dbg
	real*4 bscale,bzero,hival,loval,fitshi,fitslo
	character*6 intfmt,realfmt
	character*8 host
	character*50 version
	character*32 tasks(1000),itype,inorg,ifmt
	character*80 hdrrecord(360)
	character*132 filename
c
	character*80 fx(50)
c
	common /cpm/noproc,prfits,ihst,itask,iend,iblk,bfctr,filename,version
	common /cf1/iuni,ouni1,ouni2,ofnl,ipixsize,opixsize,obitpix,ibitpix
	common /cf2/onl,ons,onb,bscale,bzero,trlab,tsklen,syslen,ntasks,dbg
	common /cf3/itype,ifmt,inorg,host,intfmt,realfmt,case
	common /cf4/instances,tasks,linectr,inl,ins,inb
	common /cf5/hival,loval,ihival,iloval,fitshi,fitslo
	common /cdata/fitsdata,hdrrecord
c
	common /fitsx/fx
c
	integer*4 stat,nhist
	integer*4 maxlen,strlen,elements
	character*8 format
	character*32 tkey
	character*80 outline
c	
	nhist=1000				!maximum task number
c						!get list of VICAR tasks
	call xlhinfo(iuni,tasks,instances,nhist,stat,' ')
	call chkstat (stat,'??E XLhinfo error in getting task list',0,0,0)
	if (dbg) then
		write (outline,10100) nhist
10100	format ('HDRLENGTH - number of tasks = ',i5)
		call xvmessage (outline,' ')
	endif
	tsklen=nhist+1				!add one for HISTORY hdr
	ntasks=nhist
	if (itask) return		!return if only tasks - not keywords

	tsklen=0				!reset to start
	if (.not.ihst) then
		if (obitpix.gt.0) syslen=syslen+1 	!for BLANK
		return
	endif
c
c	continue if all tasks and keywords are to be passed to FITS
c
c	first loop thru label until first TASK found
c
10	continue
	call xlninfo (iuni,tkey,format,maxlen,elements,stat,
     1	'STRLEN',strlen,' ')
	if (stat.ne.1) then
		if (stat.eq.END_OF_LABEL) go to 100
	endif
c	type *, tkey			!should be TASK=
	if (tkey.ne.'TASK') go to 10
c	found TASK=,
15	continue
	tsklen=tsklen+1
c	now get USER=
	call xlninfo (iuni,tkey,format,maxlen,elements,stat,
     1	'STRLEN',strlen,' ')
	if (stat.ne.1) then
		if (stat.eq.END_OF_LABEL) go to 100
c	type *, tkey			!should be USER=
		call chkstat (stat,'??E XLninfo error on USER=',0,0,0)
	endif
c	now get DAT_TIM=
	call xlninfo (iuni,tkey,format,maxlen,elements,stat,
     1	'STRLEN',strlen,' ')
	if (stat.ne.1) then
		if (stat.eq.END_OF_LABEL) go to 100
c	type *, tkey			!should be DAT_TIM=
		call chkstat (stat,'??E XLninfo error on USER=',0,0,0)
	endif
c	see if TASK or some KEYWORD=
25	continue
	call xlninfo (iuni,tkey,format,maxlen,elements,stat,
     1	'STRLEN',strlen,' ')
	if (stat.ne.1) then
		if (stat.eq.END_OF_LABEL) go to 100
	endif
c	type *, tkey			!should be TASK= or some KEYWORD=
	if (tkey.eq.'TASK') go to 15
	tsklen=tsklen+1
	go to 25
c
c tsklen gives total history and keyword labels to be transferred to FITS
c
100	continue	
	tsklen=tsklen+1			!add one for HISTORY = VICAR HISTORY..
	if (dbg) then
		write (outline,10200) tsklen
10200	format ('total history labels = ',i5)
		call xvmessage (outline,' ')
	endif
	return
	end
C========================================================================
	subroutine crfitsh
c
c	Routine to create and list system area of FITS header
c
	implicit none
	byte fitsdata(32000)
	integer*4 iuni,ouni1,ouni2,onl,ons,onb,trlab,ofnl,tsklen,syslen,case
	integer*4 ipixsize,opixsize,obitpix,ibitpix,bfctr,ntasks,linectr
	integer*4 instances(1000),inl,ins,inb,ihival,iloval,itemp
	logical*4 prfits,noproc,ihst,iend,iblk,itask,dbg
	real*4 bscale,bzero,hival,loval,fitshi,fitslo
	character*6 intfmt,realfmt
	character*8 host
	character*50 version
	character*32 tasks(1000),itype,inorg,ifmt
	character*80 hdrrecord(360)
	character*132 filename
c
	character*80 fx(50)
c
	common /cpm/noproc,prfits,ihst,itask,iend,iblk,bfctr,filename,version
	common /cf1/iuni,ouni1,ouni2,ofnl,ipixsize,opixsize,obitpix,ibitpix
	common /cf2/onl,ons,onb,bscale,bzero,trlab,tsklen,syslen,ntasks,dbg
	common /cf3/itype,ifmt,inorg,host,intfmt,realfmt,case
	common /cf4/instances,tasks,linectr,inl,ins,inb
	common /cf5/hival,loval,ihival,iloval,fitshi,fitslo
	common /cdata/fitsdata,hdrrecord
c
	common /fitsx/fx
c
	byte cardimg(80)
	integer*2 flag
	integer*4 naxes,stat,m

	character*1 apostrophe
	character*8 un
	character*20 sysdate
cc	character*12 username
	character*20 iobuffer
	character*47 upperline,lowerline
	character*80 outline
c
	data apostrophe/''''/
	data upperline
     1/'         11111111112222222222333333333344444444'/
	data lowerline
     1/'12345678901234567890123456789012345678901234567'/
c
c
	call itla (32,fitsdata,28800)	!blank out header buffer
c
	trlab=1				!initialize label number
	call xvmessage (upperline,' ')
	call xvmessage (lowerline,' ')
c	call mvl (card(1,1),fitsdata(1),80)		!SIMPLE=T
	hdrrecord(trlab)=fx(1)				!SIMPLE=T	
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	if (obitpix.eq.8) hdrrecord(trlab)=fx(3)	!BITPIX=8
	if (obitpix.eq.16) hdrrecord(trlab)=fx(2)	!BITPIX=16
	if (obitpix.eq.32) hdrrecord(trlab)=fx(4)	!BITPIX=32
	if (obitpix.eq.-32) hdrrecord(trlab)=fx(5)	!BITPIX=-32
c	if (obitpix.eq.-64) hdrrecord(trlab)=fx(3)	!BITPIX=-64
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	naxes=2						!!2-24-04 
c	if (inl.eq.1) naxes=1
c	if (ins.eq.1) naxes=1
	if (inb.gt.1) naxes=3				!!2-24-04
	write (iobuffer,10100) naxes
10100 format (i2)
	fx(6)(29:30)=iobuffer(1:2)
	hdrrecord(trlab)=fx(6)				!NAXIS
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	write (iobuffer,10105) ins
10105 format (i7)
	fx(7)(24:30)=iobuffer(1:7)
	hdrrecord(trlab)=fx(7)				!NAXIS1
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	write (iobuffer,10105) inl
	fx(8)(24:30)=iobuffer(1:7)
	hdrrecord(trlab)=fx(8)				!NAXIS2
c	call xvmessage (hdrrecord(trlab),' ')
c
	if (naxes.eq.3) then
	trlab=trlab+1
	write (iobuffer,10105) inb
	fx(9)(24:30)=iobuffer(1:7)
	hdrrecord(trlab)=fx(9)				!NAXIS3
c	call xvmessage (hdrrecord(trlab),' ')
	endif
c
	trlab=trlab+1
c	call mvl (card(1,7),fitsdata(481),80)		!EXTEND
	hdrrecord(trlab)=fx(16)				!EXTEND
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	hdrrecord(trlab)=fx(15)				!BLOCKED
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	fx(26)(36:55)=version(4:22)			!!2-24-04
	fx(26)(56:56)=apostrophe
	hdrrecord(trlab)=fx(26)				!ORIGIN	
c	call xvmessage (hdrrecord(trlab),' ')
c
c	trlab=trlab+1
c	hdrrecord(trlab)=fx(30)				!VERSION
c	call xvmessage (hdrrecord(trlab),' ')
c
c	non-FITS standard date format - should be dd/mm/yy
c
	trlab=trlab+1
c** old vms version
c	call date (vdate)	!*****VMS Fortran date routine******
c
	flag=2					!!2-24-04 - fixed datfmt call
	call datfmt(flag,sysdate,itemp)
	fx(22)(11:11)=apostrophe
	fx(22)(12:27)=sysdate(2:17)		!<----convert to character*9
	fx(22)(28:28)=apostrophe
	hdrrecord(trlab)=fx(22)				!DATE
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	fx(34)(11:11)=apostrophe
	fx(34)(12:50)=filename(1:39)
	fx(34)(51:51)=apostrophe
	hdrrecord(trlab)=fx(34)				!FILENAME
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
c** vms version
c	call user(un,unlen)
c
	call gtprcs (un)
	fx(21)(11:11)=apostrophe
	fx(21)(12:19)=un(1:8)		!<---convert to character*12
	fx(21)(20:20)=apostrophe
	hdrrecord(trlab)=fx(21)				!USERID
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	fx(20)(11:11)=apostrophe	
	fx(20)(12:19)=host(1:8)
	fx(20)(20:20)=apostrophe
	hdrrecord(trlab)=fx(20)				!HOST
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	if (ibitpix.gt.0) then
		fx(32)(11:11)=apostrophe
		fx(32)(12:17)=intfmt(1:6)
		fx(32)(20:20)=apostrophe
		hdrrecord(trlab)=fx(32)			!INTFMT
c		call xvmessage (hdrrecord(trlab),' ')
	else
		fx(33)(11:11)=apostrophe
		fx(33)(12:17)=realfmt(1:6)
		fx(33)(20:20)=apostrophe
		hdrrecord(trlab)=fx(33)			!REALFMT
c		call xvmessage (hdrrecord(trlab),' ')
	endif
c
	trlab=trlab+1
	fx(31)(11:11)=apostrophe
	fx(31)(12:17)=itype(1:6)
	fx(31)(20:20)=apostrophe
	hdrrecord(trlab)=fx(31)				!DATATYPE
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	if (ibitpix.eq.-32) then
	   write (iobuffer,10110) loval
10110	   format(e19.6)
	   fx(35)(12:30)=iobuffer(1:19)
	endif
	if (ibitpix.ne.-32) then
	  write (iobuffer,10115) iloval
10115	  format(i20)
	  fx(35)(11:30)=iobuffer(1:20)
	endif
	hdrrecord(trlab)=fx(35)				!VIC-MIN
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	if (ibitpix.eq.-32) then
	   write (iobuffer,10110) hival
	   fx(36)(12:30)=iobuffer(1:19)
	endif
	if (ibitpix.ne.-32) then
	   write (iobuffer,10115) ihival
	   fx(36)(11:30)=iobuffer(1:20)
	endif
	hdrrecord(trlab)=fx(36)				!VIC-MAX
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	write (iobuffer,10100) ibitpix
	fx(37)(29:30)=iobuffer(1:2)
	hdrrecord(trlab)=fx(37)				!VIC-B/P
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	fx(38)(11:11)=apostrophe
	fx(38)(12:19)=ifmt(1:8)
	fx(38)(20:20)=apostrophe
	hdrrecord(trlab)=fx(38)				!VIC-FMT
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	if (ibitpix.eq.-32) then
	   write (iobuffer,10110) bscale
	   fx(10)(12:30)=iobuffer(1:19)
	endif
	if (ibitpix.ne.-32) then
	   write (iobuffer,10120) bscale
10120	   format(f20.6)
	   fx(10)(11:30)=iobuffer(1:20)
	endif
	hdrrecord(trlab)=fx(10)				!BSCALE
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	if (ibitpix.eq.-32) then
	   write (iobuffer,10110) bzero
	   fx(11)(12:30)=iobuffer(1:19)
	endif
	if (ibitpix.ne.-32) then
	   write (iobuffer,10120) bzero
	   fx(11)(11:30)=iobuffer(1:20)
	endif
	hdrrecord(trlab)=fx(11)				!BZERO
c	call xvmessage (hdrrecord(trlab),' ')
c
c	The following might change if units are incorporated into
c	VICAR property labels
c 
	if (obitpix.gt.0) then
		trlab=trlab+1
		hdrrecord(trlab)=fx(12)			!BUNIT
c		call xvmessage (hdrrecord(trlab),' ')
	endif
c
c	FITS standard says dont use BLANK if floating point data
c
c	if VICAR incorporates blank values in property labels then
c	well have to add some way of dealing with them here
c
c	trlab=trlab+1
c	hdrrecord(trlab)=fx(13)				!BLANK
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	if (ibitpix.eq.-32) then
	   write (iobuffer,10110) fitshi
	   fx(23)(12:30)=iobuffer(1:19)
	endif
	if (ibitpix.ne.-32) then
	   write (iobuffer,10120) fitshi
	   fx(23)(11:30)=iobuffer(1:20)
	endif
	hdrrecord(trlab)=fx(23)				!DATAMAX
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	if (ibitpix.eq.-32) then
	   write (iobuffer,10110) fitslo
	   fx(24)(12:30)=iobuffer(1:19)
	endif
	if (ibitpix.ne.-32) then
	   write (iobuffer,10120) fitslo
	   fx(24)(11:30)=iobuffer(1:20)
	endif
	hdrrecord(trlab)=fx(24)				!DATAMIN
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	hdrrecord(trlab)=fx(28)				!CRVAL1
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	hdrrecord(trlab)=fx(19)				!CRPIX1
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	hdrrecord(trlab)=fx(17)				!CTYPE1
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	hdrrecord(trlab)=fx(18)				!CDELT1
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	hdrrecord(trlab)=fx(44)				!CRVAL2
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	hdrrecord(trlab)=fx(43)				!CRPIX2
c	call xvmessage (hdrrecord(trlab),' ')
c
	trlab=trlab+1
	hdrrecord(trlab)=fx(41)				!CTYPE2
c	call xvmessage (hdrrecord(trlab),' ')
c 34 or 35
	trlab=trlab+1
	hdrrecord(trlab)=fx(42)				!CDELT2
c	call xvmessage (hdrrecord(trlab),' ')
c
c 35 or 36
c	call prnt (4,1,trlab,'trlab should be =35 - trlab=. ')
c
c !!!! Careful - if you add more system records here please change
c	variable syslen to agree with the new number !!!!!!
c
c only temporary until history is added
c
	if (.not.ihst) then
		trlab=trlab+1
		hdrrecord(trlab)=fx(29)			!END
c		call xvmessage (hdrrecord(trlab),' ')
	endif

	do m=1,trlab
		call xvmessage (hdrrecord(m),' ')
		if (prfits) then
		call mvcl(hdrrecord(m),cardimg,80)
		call xvwrit (ouni2,cardimg,stat,'LINE',m,'NSAMPS',80,' ')
		call chkstat(stat,'??E XVwrit error on FITS header print file ',
     1 0,0,0)
		endif
	enddo
c
	if (dbg) then
		write (outline,11100) trlab
11100	format ('total SYSTEM labels to transfer = ',i5)
		call xvmessage (outline,' ')
	endif
	if (.not.ihst) call hdrwrite
c
c	Note: I had to predict the number of FITS labels prior to opening
c	the output file in subroutine PMPROC.  Therefore, the variable SYSLEN
c	reflects the planned number of FITS "system" header records. TRLAB
c	records each system header as its processed.  If they differ then
c	SYSLEN should be increased/decreased in the main44 routine in order
c	to match what TRLAB gives as a running total.  I abended here in order
c	to ensure that the output FITS file does not get corrupted due to an
c	incorrect number of headers. (Remember SYSLEN includes 'END')
c
c	if (trlab.ne.syslen) then
c		call qprint
c	1	 ('??E FITSOUT(CRFITSH) - SYSLEN and TRLAB are unequal',47)
c		call prnt2 (4,1,syslen,'SYSLEN =. ')
c		call abend
c	endif

	return
	end
C========================================================================
	subroutine vichist
c
c	routine to get VICAR history data from VICAR label
c
	implicit none
	include 'errdefs'
c
	byte fitsdata(32000)
	integer*4 iuni,ouni1,ouni2,onl,ons,onb,trlab,ofnl,tsklen,syslen,case
	integer*4 ipixsize,opixsize,obitpix,ibitpix,bfctr,ntasks,linectr
	integer*4 instances(1000),inl,ins,inb,ihival,iloval
	logical*4 noproc,prfits,ihst,itask,iend,iblk,dbg	
	real*4 bscale,bzero,hival,loval,fitshi,fitslo
	character*6 intfmt,realfmt
	character*8 host
	character*50 version
	character*32 tasks(1000),itype,inorg,ifmt
	character*80 hdrrecord(360)
	character*132 filename
c
	character*80 fx(50)
c
	common /cpm/noproc,prfits,ihst,itask,iend,iblk,bfctr,filename,version
	common /cf1/iuni,ouni1,ouni2,ofnl,ipixsize,opixsize,obitpix,ibitpix
	common /cf2/onl,ons,onb,bscale,bzero,trlab,tsklen,syslen,ntasks,dbg
	common /cf3/itype,ifmt,inorg,host,intfmt,realfmt,case
	common /cf4/instances,tasks,linectr,inl,ins,inb
	common /cf5/hival,loval,ihival,iloval,fitshi,fitslo
	common /cdata/fitsdata,hdrrecord
c
	common /fitsx/fx
c
	integer*4 stat,subset,strlen,maxlen,istrt
	integer*4 keyitem,ntaskitem,elements
	character*1 apostrophe
	character*8 format
	character*32 tkey
	character*80 itemstrng,charbuffer
	character*100 outline
c
	data apostrophe/''''/
c
	trlab=trlab+1
	hdrrecord(trlab)=fx(39)
	call xvmessage (hdrrecord(trlab),' ')			!HISTORY
	if (prfits)
     1	call outheader (ouni2,bfctr,trlab,linectr,hdrrecord)
	if (trlab.eq.36*bfctr) call hdrwrite		
c
c if history=only tasks
c	
cc	print *, 'ntasks, tasks(1) = ',ntasks,tasks(1)
	if (itask) then
		charbuffer=fx(40)
		ntaskitem=1			!Includes HISTORY = but not END
		do subset=1,ntasks
		   call taskrecord (iuni,ntaskitem,instances(subset),
     1 tasks(subset),charbuffer)
		   trlab=trlab+1
		   hdrrecord(trlab)=charbuffer
		   call xvmessage(hdrrecord(trlab),' ')
		   if (prfits) 
     1 call outheader (ouni2,bfctr,trlab,linectr,hdrrecord)
		   if (trlab.eq.36*bfctr) call hdrwrite	!need bfctr here
		enddo
		trlab=trlab+1
		hdrrecord(trlab)=fx(29)
		call xvmessage(hdrrecord(trlab),' ')
		if (prfits) 
     1 call outheader (ouni2,bfctr,trlab,linectr,hdrrecord)
		call hdrwrite			!finish out header
		if (dbg) then
	            write (outline,10010) ntaskitem
10010	        format ('counted history labels = ',i5)
 	            call xvmessage (outline,' ')
		endif
	        if (tsklen.ne.ntaskitem) then
		    write (outline,10600) tsklen,ntaskitem
		    call xvmessage (outline,' ')
		    call abend
	    	endif
		return
	else			!itask='FALSE'
c	
c if history=history
c
		ntaskitem=1				!accounts for HISTORY=
c       tasks(1) contains the task name already so query USER and DAT_TIM
		do subset=1,ntasks
		   charbuffer=fx(40)
		   call taskrecord (iuni,ntaskitem,instances(subset),
     1 tasks(subset),charbuffer)
		   trlab=trlab+1
		   hdrrecord(trlab)=charbuffer
		   call xvmessage(hdrrecord(trlab),' ')
		   if (prfits) 
     1 	   call outheader (ouni2,bfctr,trlab,linectr,hdrrecord)
		   if (trlab.eq.36*bfctr) call hdrwrite	!need bfctr here
c
c	unpack subsets
c
		   keyitem=0
30	continue
		   charbuffer=fx(14)
		   call xlninfo (iuni,tkey,format,maxlen,elements,stat,
     1	   'STRLEN',strlen,' ')
		   if (stat.ne.1) then
cc			print  *, 'stat on XLNINFO KEY = ',stat
			if (stat.ne.CANNOT_FIND_KEY) then
			   if (stat.ne.END_OF_LABEL) then
			  	call chkstat (stat,'??E XLNINFO (KEY) error',0,0,0)
			   endif
			endif
		   endif
		   if (stat.eq.END_OF_LABEL) go to 100
		   if (tkey.eq.'TASK') go to 50
		   ntaskitem=ntaskitem+1
	  	   if (elements.gt.1) then
			write (outline,10100) tasks(subset),tkey
10100	format ('??E For VICAR task = ',a32,' keyword = ',a32)
			call xvmessage (outline,' ')
			write (outline,10200) elements
10200	format ('elements = ',i5, ' -- More than 1 - cannot process'/
     1 'truncated to 1st element')
			call xvmessage (outline,' ')
		   endif
		   if (strlen.gt.72) then
			write (outline,10100) tasks(subset),tkey
			call xvmessage (outline,' ')
			write (outline,10300) strlen
10300	format ('??E VICAR label string length = ',i5, ' -- Truncated to 72')
	   		call xvmessage (outline,' ')
		   endif
		   call xlget (iuni,'HISTORY',tkey,itemstrng,stat,'HIST',
     1	   tasks(subset),'INSTANCE',instances(subset),'FORMAT',
     2	   'STRING','ULEN',strlen,'NELEMENT',elements,' ')
c	print *, "tkey,itemstrng,stat.strlen,elements = ",
c	1 tkey,itemstrng,stat,strlen,elements
c   in case a VFXXX label has a null string, ie, '' - fill it with blanks
		   if (stat.eq.-53.and.strlen.eq.0) then
		      charbuffer(1:8)=tkey
		      charbuffer(11:11)=apostrophe
		      charbuffer(12:20)='        '
		      charbuffer(21:21)=apostrophe
		      go to 40
		   endif
		   call chkstat (stat,'??E XLGET (HISTORY) read error',0,0,0)
		   charbuffer(1:8)=tkey
		   if (format.eq.'STRING') then
			charbuffer(11:11)=apostrophe
			charbuffer(12:12+strlen)=itemstrng(1:strlen)
			charbuffer(12+strlen:12+strlen)=apostrophe
		   else
c			if (format.eq.'INT'.or.format.eq.'REAL') then
			   istrt=31-strlen
			   charbuffer(istrt:istrt+strlen)=itemstrng(1:strlen)
			endif
c		   endif
40	continue
		   trlab=trlab+1
		   hdrrecord(trlab)=charbuffer
		   call xvmessage(hdrrecord(trlab),' ')
		   if (prfits) 
     1	   call outheader (ouni2,bfctr,trlab,linectr,hdrrecord)
		   if (trlab.eq.36*bfctr) call hdrwrite	!need bfctr here
		   go to 30
c
50	continue
c		   ntaskitem=ntaskitem+keyitem
	    enddo

100	continue	
	    trlab=trlab+1
	    hdrrecord(trlab)=fx(29)
	    call xvmessage (hdrrecord(trlab),' ')
	    if (prfits) 
     1   call outheader (ouni2,bfctr,trlab,linectr,hdrrecord)
	    call hdrwrite			!finish out header
	    if (dbg) then
	    	write (outline,10400) ntaskitem
10400   format ('counted history labels = ',i5)
	    	call xvmessage (outline,' ')
	    endif
	    if (tsklen.ne.ntaskitem) then
		write (outline,10600) tsklen,ntaskitem
10600   format ('??E FITSOUT - predicted history labels = ',i5,
     1  ' counted history labels = ',i5)
	    	call xvmessage (outline,' ')
		call abend
	    endif
	endif
	return
	end
C========================================================================
	subroutine hdrwrite
c
c	routine to write out FITS header to the output file
c
	implicit none
	byte fitsdata(32000)
	integer*4 iuni,ouni1,ouni2,onl,ons,onb,trlab,ofnl,tsklen,syslen,case
	integer*4 ipixsize,opixsize,obitpix,ibitpix,bfctr,ntasks,linectr
	integer*4 instances(1000),inl,ins,inb,ihival,iloval
	logical*4 noproc,prfits,ihst,itask,iend,iblk,dbg	
	real*4 bscale,bzero,hival,loval,fitshi,fitslo
	character*6 intfmt,realfmt
	character*8 host
	character*50 version
	character*32 tasks(1000),itype,inorg,ifmt
	character*80 hdrrecord(360)
	character*132 filename
	character*80 fx(50)
c
	common /cpm/noproc,prfits,ihst,itask,iend,iblk,bfctr,filename,version
	common /cf1/iuni,ouni1,ouni2,ofnl,ipixsize,opixsize,obitpix,ibitpix
	common /cf2/onl,ons,onb,bscale,bzero,trlab,tsklen,syslen,ntasks,dbg
	common /cf3/itype,ifmt,inorg,host,intfmt,realfmt,case
	common /cf4/instances,tasks,linectr,inl,ins,inb
	common /cf5/hival,loval,ihival,iloval,fitshi,fitslo
	common /cdata/fitsdata,hdrrecord
	common /fitsx/fx
c
	integer*4 stat,k
c
	if (.not.noproc) then
c
		do k=1,36*bfctr
			call mvcl (hdrrecord(k),fitsdata(1+(k-1)*80),80)
		enddo
		call xvwrit (ouni1,fitsdata,stat,'LINE',linectr,'NSAMPS',
     1	2880,'BAND',1,' ')
		call chkstat (stat,'??E XVWrit error on character data',0,0,0)
	endif
	linectr=linectr+1
	trlab=0
	call itla (32,fitsdata,28800)	!blank out header buffer
	do k=1,36*bfctr
		hdrrecord(k)=fx(50)	!fx(50) is always 80 blanks
	enddo
	return
	end
C========================================================================
	subroutine outheader (unit,bfctr,trlab,linectr,hdrrecord)
c
c	routine to write out header file to disk
c
	implicit none
	byte cardimg(80)	
	integer*4 unit,bfctr,trlab,linectr,hcnt,stat
	character*80 hdrrecord(360)
c
	if (linectr.eq.1) then
		hcnt=trlab
	else
		hcnt=(linectr-1)*36*bfctr+trlab
	endif
	call mvcl (hdrrecord(trlab),cardimg,80)
	call xvwrit (unit,cardimg,stat,'LINE',hcnt,'NSAMPS',80,
     1' ')
	call chkstat(stat,'??E XVwrit error on FITS header print file ',0,0,0)
c
	return
	end
C========================================================================
	subroutine taskrecord (unit,ntaskitem,instances,
     1 tasks,charbuffer)
c
c	routine to build VIC-TASK record
c
	integer*4 unit,ntaskitem,instances
	integer*4 stat
	character*1 apostrophe
	character*12 username
	character*28 tasktime
	character*32 tasks	
	character*80 charbuffer
c
	data apostrophe/''''/
c
	call xlget(unit,'HISTORY','USER',username,stat,'HIST',
     1 tasks,'INSTANCE',instances,'ULEN',12,
     1 'FORMAT','STRING',' ')
	call chkstat(stat,'??E XLget error on HISTORY, USER',0,0,0)
	call xlget(unit,'HISTORY','DAT_TIM',tasktime,stat,'HIST',
     1 tasks,'INSTANCE',instances,'ULEN',28,
     1 'FORMAT','STRING',' ')
	call chkstat(stat,'??E XLget error on HISTORY, DAT_TIM',0,0,0)
	ntaskitem=ntaskitem+1
C        1111111111222222222233333333334444444444555555555566666666667777777777
C234567890123456789012345678901234567890123456789012345678901234567890123456789
CIC-TASK= 'TASKNAME                     ' 'USER      ' 'TIME'                   
Cc
c                               !adjust for 29 of 32 character task name
	charbuffer(11:11)=apostrophe
	charbuffer(12:39)=tasks(1:28)                        !TASK=
	charbuffer(40:40)=apostrophe
cc                              !adjust for 10 of 12 character user name
	charbuffer(42:42)=apostrophe
	charbuffer(43:52)=username(1:10)                     !USER=
	charbuffer(53:53)=apostrophe
c                               !adjust for 23 of 28 character time
	charbuffer(55:55)=apostrophe
	charbuffer(56:79)=tasktime(1:24)                     !TIME=
	charbuffer(80:80)=apostrophe
c
	return
	end
C========================================================================
	subroutine fitsoutdata
c
c	driver routine to write out data to fits file
c
	implicit none
	byte fitsdata(32000)
	integer*4 iuni,ouni1,ouni2,onl,ons,onb,trlab,ofnl,tsklen,syslen,case
	integer*4 ipixsize,opixsize,obitpix,ibitpix,bfctr,ntasks,linectr
	integer*4 instances(1000),inl,ins,inb,ihival,iloval,stat
	logical*4 noproc,prfits,ihst,itask,iend,iblk,dbg	
	real*4 bscale,bzero,hival,loval,fitshi,fitslo
	character*6 intfmt,realfmt
	character*8 host
	character*50 version
	character*32 tasks(1000),itype,inorg,ifmt
	character*80 hdrrecord(360)
	character*132 filename
	character*80 fx(50)
c
	common /cpm/noproc,prfits,ihst,itask,iend,iblk,bfctr,filename,version
	common /cf1/iuni,ouni1,ouni2,ofnl,ipixsize,opixsize,obitpix,ibitpix
	common /cf2/onl,ons,onb,bscale,bzero,trlab,tsklen,syslen,ntasks,dbg
	common /cf3/itype,ifmt,inorg,host,intfmt,realfmt,case
	common /cf4/instances,tasks,linectr,inl,ins,inb
	common /cf5/hival,loval,ihival,iloval,fitshi,fitslo
	common /cdata/fitsdata,hdrrecord
	common /fitsx/fx
c
c	each routine reads only one VICAR format, i.e., BYTE, HALF, WORD, etc
c	but writes multiple FITS formats 
c
	if (case.eq.1) call hfitso	!obitpix.eq.8.and.ibitpix.eq.16
	if (case.eq.2) call ffitso	!obitpix.eq.8.and.ibitpix.eq.32
	if (case.eq.3) call ffitso	!obitpix.eq.16.and.ibitpix.eq.32
	if (case.eq.4) call ifitso	!obitpix.eq.8.and.ibitpix.eq.-32
	if (case.eq.5) call ifitso	!obitpix.eq.16.and.ibitpix.eq.-32
	if (case.eq.6) call ifitso	!obitpix.eq.32.and.ibitpix.eq.-32
	if (case.eq.7) call abend	!obitpix.eq.8.and.ibitpix.eq.-64
	if (case.eq.8) call abend	!obitpix.eq.16.and.ibitpix.eq.-64
	if (case.eq.9) call abend	!obitpix.eq.32.and.ibitpix.eq.-64
	if (case.eq.10) call abend	!obitpix.eq.-32.and.ibitpix.eq.-64
	if (case.eq.11) call bfitso	!obitpix.eq.8.and.ibitpix.eq.8
	if (case.eq.12) call hfitso	!obitpix.eq.16.and.ibitpix.eq.16
	if (case.eq.13) call ffitso	!obitpix.eq.32.and.ibitpix.eq.32
	if (case.eq.14) call rfitso	!obitpix.eq.-32.and.ibitpix.eq.-32
	if (case.eq.15) call abend	!obitpix.eq.-64.and.ibitpix.eq.-64
	if (case.eq.16) call bfitso	!obitpix.eq.16.and.ibitpix.eq.8
	if (case.eq.17) call bfitso	!obitpix.eq.32.and.ibitpix.eq.8
	if (case.eq.18) call bfitso	!obitpix.eq.-32.and.ibitpix.eq.8
	if (case.eq.19) call abend	!obitpix.eq.-64.and.ibitpix.eq.8
	if (case.eq.20) call hfitso	!obitpix.eq.32.and.ibitpix.eq.16
	if (case.eq.21) call hfitso	!obitpix.eq.-32.and.ibitpix.eq.16
	if (case.eq.22) call abend	!obitpix.eq.-64.and.ibitpix.eq.16
	if (case.eq.23) call ffitso	!obitpix.eq.-32.and.ibitpix.eq.32
	if (case.eq.24) call abend	!obitpix.eq.-64.and.ibitpix.eq.32
	if (case.eq.25) call abend	!obitpix.eq.-64.and.ibitpix.eq.-32
c
	call xvclose (ouni1,stat,' ')
	return
	end
C========================================================================
	subroutine bfitso
c
c	routine to read in VICAR byte data and write out FITS data
c	in 8, 16 or 32 bit (integer) or 32-bit IEEE Realformat
c	CASES = 11, 16, 17 and 18
c
	implicit none
	byte fitsdata(32000)
	integer*4 iuni,ouni1,ouni2,onl,ons,onb,trlab,ofnl,tsklen,syslen,case
	integer*4 ipixsize,opixsize,obitpix,ibitpix,bfctr,ntasks,linectr
	integer*4 instances(1000),inl,ins,inb,ihival,iloval
	logical*4 noproc,prfits,ihst,itask,iend,iblk,dbg
	real*4 bscale,bzero,hival,loval,fitshi,fitslo
	character*6 intfmt,realfmt
	character*8 host
	character*50 version
	character*32 tasks(1000),itype,inorg,ifmt
	character*80 hdrrecord(360)
	character*132 filename
c
	character*80 fx(50)
c
	common /cpm/noproc,prfits,ihst,itask,iend,iblk,bfctr,filename,version
	common /cf1/iuni,ouni1,ouni2,ofnl,ipixsize,opixsize,obitpix,ibitpix
	common /cf2/onl,ons,onb,bscale,bzero,trlab,tsklen,syslen,ntasks,dbg
	common /cf3/itype,ifmt,inorg,host,intfmt,realfmt,case
	common /cf4/instances,tasks,linectr,inl,ins,inb
	common /cf5/hival,loval,ihival,iloval,fitshi,fitslo
	common /cdata/fitsdata,hdrrecord
c
	common /fitsx/fx
c
	integer*4 stat,i,j,fitsamprec,viclen,fitslen,kband
	integer*4 vbyrec,fitsbytrec,il,krecm,krec,kpixel,krow,nrows
	integer*4 htransbuf(12),ftransbuf(12),rtransbuf(12),rntransbuf(12)
	integer*4 frec(7200)
	integer*2 hrec(14400)
	real*4	  rrec(7200),rreal(7200)
	byte 	  brec(28800)
	character*4 stype,dtype
	character*6 intfmto,realfmto
c
c
c 	From algorithm in Reference 1
c
C***************************
C     READ DATA RECORDS AND UNBLOCK BITS.
C     NL=NLIN  NB=BYREC  B/PX=BYPIX NS=NPIX
C***************************	
	nrows = inl			!max number of rows in VICAR image
	krecm = 2880			!basic fits record length in bytes
	vbyrec=ipixsize*ins		!number of bytes/record in VICAR image
	fitsbytrec=krecm*bfctr		!number of bytes/FITS record
	fitsamprec=fitsbytrec/opixsize	!number of samples/FITS record
c
c	set up translation buffers
c
	stype='BYTE'
	dtype='HALF'
	intfmto='HIGH'
	realfmto='IEEE'
	call xvtrans_out (htransbuf,stype,dtype,intfmto,realfmto,stat)
	call chkstat (stat,'??E XVtrans_out error on htransbuf',0,0,0)
c
	dtype='FULL'
	call xvtrans_out (ftransbuf,stype,dtype,intfmto,realfmto,stat)
	call chkstat (stat,'??E XVtrans_out error on ftransbuf',0,0,0)
c
	dtype='REAL'
	call xvtrans_out (rtransbuf,stype,dtype,intfmto,realfmto,stat)
	call chkstat (stat,'??E XVtrans_out error on rtransbuf',0,0,0)
c
	intfmto='NATIVE'
	realfmto='NATIVE'
	call xvtrans_out (rntransbuf,stype,dtype,intfmto,realfmto,stat)
	call chkstat (stat,'??E XVtrans_out error on rntransbuf',0,0,0)
c	
	il=0
	kband=1				!band counter on VICAR image
	krec = 0			!current sample ptr on FITS image
5	continue
	krow=0				!row counter on VICAR image
	kpixel=ins+1			!end pointer (samp)for VICAR input buff 
10	continue
	if (kpixel.lt.ins) go to 20	!br, if passed end of input buffer
	krow=krow+1			!bump VICAR row counter
	if (krow.gt.nrows) go to 40	!Br, if read in last VICAR buffer
c
c	read next row from internal storage to ROW
c
	call xvread(iuni,fitsdata,stat,'LINE',krow,'NSAMPS',vbyrec,
     1	'BAND',kband,' ')
c
	kpixel=0			!reset VICAR end pointer 
20	continue
	viclen=vbyrec-kpixel		!rec len in bytes for VICAR
	fitslen=fitsbytrec-(krec*opixsize)	!rec len in bytes for FITS
	il=min0(viclen,fitslen)		!Min rec length (VICAR or FITS)
c
	if (case.eq.11) then
		do 30 j=1,il
		brec(j+(krec*opixsize))=fitsdata(j+kpixel) 
30		continue
	endif
c
	if (case.eq.16)
     1 call xvtrans (htransbuf,fitsdata(1+kpixel),hrec(1+(krec*opixsize)),
     2 il)
c
	if (case.eq.17)
     1 call xvtrans (ftransbuf,fitsdata(1+kpixel),frec(1+(krec*opixsize)),
     2 il)
c 
	if (case.eq.18) then
		call xvtrans (rtransbuf,fitsdata(1+kpixel),
     1 	rrec(1+(krec*opixsize)),il)
		call xvtrans (rntransbuf,fitsdata(1+kpixel),
     1	rreal(1+(krec*opixsize)),il)
	endif
c
	kpixel=kpixel+il		!increment sample ctr for VICAR
	krec=krec+il/opixsize		!increment sample ctr for FITS
	if (krec*opixsize.lt.fitsbytrec) go to 10 !loop if not done transferring
c
c	write to tape
c
c	special debugg for checking buffer placements
c
C	brec(1)=255
C	brec(2)=255
C	brec(3)=255
C	brec(4)=255
C	brec(2877)=0
C	brec(2878)=0
C	brec(2879)=0
C	brec(2880)=0
	if (case.eq.11)
     1	call xvwrit (ouni1,brec,stat,'LINE',linectr,'NSAMPS',
     2 	krec,'BAND',kband,' ')
c
	if (case.eq.16) then
		call xvwrit (ouni1,hrec,stat,'LINE',linectr,'NSAMPS',
     1 	fitsamprec,'BAND',kband,' ')
		linectr=linectr+1
		call xvwrit (ouni1,hrec(1440*bfctr+1),stat,'LINE',linectr,
     1	'NSAMPS',fitsamprec,'BAND',kband,' ')
	endif
c
	if (case.eq.17) then
		call xvwrit (ouni1,frec,stat,'LINE',linectr,'NSAMPS',
     1	fitsamprec,'BAND',kband,' ')
		linectr=linectr+1
		call xvwrit (ouni1,frec(720*bfctr+1),stat,'LINE',linectr,
     1	'NSAMPS',fitsamprec,'BAND',kband,' ')
		linectr=linectr+1
		call xvwrit (ouni1,frec(1440*bfctr+1),stat,'LINE',linectr,
     1	'NSAMPS', fitsamprec,'BAND',kband,' ')
		linectr=linectr+1
		call xvwrit (ouni1,frec(2160*bfctr+1),stat,'LINE',linectr,
     1	'NSAMPS',fitsamprec,'BAND',kband,' ')
	endif
c
	if (case.eq.18) then
		call xvwrit (ouni1,rreal,stat,'LINE',linectr,'NSAMPS',
     1	fitsamprec,'BAND',kband,' ')
		linectr=linectr+1
		call xvwrit (ouni1,rreal(720*bfctr+1),stat,'LINE',linectr,
     1	'NSAMPS',fitsamprec,'BAND',kband,' ')
		linectr=linectr+1
		call xvwrit (ouni1,rreal(1440*bfctr+1),stat,'LINE',linectr,
     1	'NSAMPS', fitsamprec,'BAND',kband,' ')
		linectr=linectr+1
		call xvwrit (ouni1,rreal(2160*bfctr+1),stat,'LINE',linectr,
     1	'NSAMPS',fitsamprec,'BAND',kband,' ')
	endif
c
	linectr=linectr+1		!bump line ctr
	krec=0				!reinit output record ctr
	go to 10			!loop back
c
c	done on input, clean up
c
40	continue
	kband=kband+1
	if (kband.le.inb) go to 5
	if (krec.eq.0) go to 60
	il=fitsbytrec-krec*opixsize
c
c	Fill out buffer with zeroes before last write so file ends correctly
c
	if (case.eq.11) then
		do i=1,il
		brec(i+krec*opixsize)=0		!original has BZERO!!!
		enddo
	endif
	if (case.eq.16) then
		do i=1,il
		hrec(i+krec*opixsize)=0		!original has BZERO!!!
		enddo
	endif
	if (case.eq.17) then
		do i=1,il
		frec(i+krec*opixsize)=0		!original has BZERO!!!
		enddo
	endif
	if (case.eq.18) then
		do i=1,il
		rrec(i+krec*opixsize)=0.0	!original has BZERO!!!
		rreal(i+krec*opixsize)=0.0
		enddo
	endif
c
c
c	special debugg
c
c	brec(1)=255
c	brec(2)=255
c	brec(3)=255
c	brec(4)=255
c	brec(2877)=0
c	brec(2878)=255
c	brec(2879)=0
c	brec(2880)=255
	kband=kband-1
	if (case.eq.11) then
		call xvwrit (ouni1,brec,stat,'LINE',linectr,'NSAMPS',
     2 	fitsbytrec,'BAND',kband,' ')
	endif
	if (case.eq.16) then
		call xvwrit (ouni1,hrec,stat,'LINE',linectr,'NSAMPS',
     1	fitsamprec,'BAND',kband,' ')
		linectr=linectr+1
		call xvwrit (ouni1,hrec(1440*bfctr+1),stat,'LINE',linectr,
     1	'NSAMPS',fitsamprec,'BAND',kband,' ')
	endif
c
	if (case.eq.17) then
		call xvwrit (ouni1,frec,stat,'LINE',linectr,'NSAMPS',
     1	fitsamprec,'BAND',kband,' ')
		linectr=linectr+1
		call xvwrit (ouni1,frec(720*bfctr+1),stat,'LINE',linectr,
     1	'NSAMPS',fitsamprec,'BAND',kband,' ')
		linectr=linectr+1
		call xvwrit (ouni1,frec(1440*bfctr+1),stat,'LINE',linectr,
     1	'NSAMPS',fitsamprec,'BAND',kband,' ')
		linectr=linectr+1
		call xvwrit (ouni1,frec(2160*bfctr+1),stat,'LINE',linectr,
     1	'NSAMPS',fitsamprec,'BAND',kband,' ')
	endif
c
	if (case.eq.18) then
		call xvwrit (ouni1,rreal,stat,'LINE',linectr,'NSAMPS',
     1	fitsamprec,'BAND',kband,' ')
		linectr=linectr+1
		call xvwrit (ouni1,rreal(720*bfctr+1),stat,'LINE',linectr,
     1	'NSAMPS',fitsamprec,'BAND',kband,' ')
		linectr=linectr+1
		call xvwrit (ouni1,rreal(1440*bfctr+1),stat,'LINE',linectr,
     1	'NSAMPS', fitsamprec,'BAND',kband,' ')
		linectr=linectr+1
		call xvwrit (ouni1,rreal(2160*bfctr+1),stat,'LINE',linectr,
     1	'NSAMPS',fitsamprec,'BAND',kband,' ')
	endif
c					!Output data to FITS
	linectr=linectr+1		!in case of XTENSIONS=
60	continue			!done

	return
	end
C========================================================================
	subroutine hfitso
c
c	routine to read in VICAR halfword data and write out FITS data
c	in 8, 16 or 32 bit (integer) format
c	CASES = 1, 12, 20 and 21
c
	implicit none
	byte fitsdata(32000)
	integer*4 iuni,ouni1,ouni2,onl,ons,onb,trlab,ofnl,tsklen,syslen,case
	integer*4 ipixsize,opixsize,obitpix,ibitpix,bfctr,ntasks,linectr
	integer*4 instances(1000),inl,ins,inb,ihival,iloval
	logical*4 noproc,prfits,ihst,itask,iend,iblk,dbg	
	real*4 bscale,bzero,hival,loval,fitshi,fitslo
	character*6 intfmt,realfmt
	character*8 host
	character*50 version
	character*32 tasks(1000),itype,inorg,ifmt
	character*80 hdrrecord(360)
	character*132 filename
	character*80 fx(50)
c
	common /cpm/noproc,prfits,ihst,itask,iend,iblk,bfctr,filename,version
	common /cf1/iuni,ouni1,ouni2,ofnl,ipixsize,opixsize,obitpix,ibitpix
	common /cf2/onl,ons,onb,bscale,bzero,trlab,tsklen,syslen,ntasks,dbg
	common /cf3/itype,ifmt,inorg,host,intfmt,realfmt,case
	common /cf4/instances,tasks,linectr,inl,ins,inb
	common /cf5/hival,loval,ihival,iloval,fitshi,fitslo
	common /cdata/fitsdata,hdrrecord
	common /fitsx/fx
c
	byte 	  brec(28800)
	integer*2 hrec(14400),halfdata(32000)
	integer*4 stat,i,j,fitsamprec,viclen,fitslen,kband,oword
	integer*4 vbyrec,fitsbytrec,il,krecm,krec,kpixel,krow,nrows
	integer*4 htransbuf(12),ftransbuf(12),rtransbuf(12)
	integer*4 frec(7200)
	real*4 rrec(7200)
	character*4 stype,dtype
	character*6 intfmto,realfmto
c
c 	From algorithm in Reference 1
c
C***************************
C     READ DATA RECORDS AND UNBLOCK BITS.
C     NL=NLIN  NB=BYREC  B/PX=BYPIX NS=NPIX
C***************************	
	nrows = inl			!max number of rows in VICAR image
	krecm = 2880			!basic fits record length in bytes
	vbyrec=ipixsize*ins		!number of bytes/record in VICAR image
	fitsbytrec=krecm*bfctr		!number of bytes/FITS record
	fitsamprec=fitsbytrec/opixsize	!number of samples/FITS record
c
c	set up translation buffers
c
	stype='HALF'
	dtype='HALF'
	intfmto='HIGH'
	realfmto='IEEE'
	call xvtrans_out (htransbuf,stype,dtype,intfmto,realfmto,stat)
	call chkstat (stat,'??E XVtrans_out error on htransbuf',0,0,0)
c
	dtype='FULL'
	call xvtrans_out (ftransbuf,stype,dtype,intfmto,realfmto,stat)
	call chkstat (stat,'??E XVtrans_out error on ftransbuf',0,0,0)
c
	dtype='REAL'
	call xvtrans_out (rtransbuf,stype,dtype,intfmto,realfmto,stat)
	call chkstat (stat,'??E XVtrans_out error on rtransbuf',0,0,0)
c
	il=0
	kband=1				!band counter on VICAR image
	krec = 0			!current sample ptr on FITS image
5	continue
	krow=0				!row counter on VICAR image
	kpixel=ins+1			!end pointer (samp)for VICAR input buff 
10	continue
	if (kpixel.lt.ins) go to 20	!br, if passed end of input buffer
	krow=krow+1			!bump VICAR row counter
	if (krow.gt.nrows) go to 40	!Br, if read in last VICAR buffer
c
c	read next row from internal storage to ROW
c
	call xvread(iuni,halfdata,stat,'LINE',krow,'NSAMPS',ins,
     1	'BAND',kband,' ')
c
	kpixel=0			!reset VICAR end pointer 
20	continue
	viclen=ins-kpixel		!rec len in bytes for VICAR
	fitslen=fitsamprec-krec		!rec len in bytes for FITS
	il=min0(viclen,fitslen)		!Min rec length (VICAR or FITS)
c
	if (case.eq.1) then
		do j=1,fitsamprec
		oword=halfdata(j+kpixel)
		oword=(oword-bzero)/bscale
		if (oword.gt.127) oword=255-oword
		brec(j+krec)=oword 
		enddo
	endif
c
	if (case.eq.12)
     1 call xvtrans (htransbuf,halfdata(1+kpixel),hrec(1+krec),
     2 il)
c
	if (case.eq.20)
     1 call xvtrans (ftransbuf,halfdata(1+kpixel),frec(1+krec),
     2 il)
c	 
	if (case.eq.21)
     1 call xvtrans (rtransbuf,halfdata(1+kpixel),rrec(1+krec),
     2 il)
c	 
	kpixel=kpixel+il		!increment sample ctr for VICAR
	krec=krec+il			!increment sample ctr for FITS
	if (krec.lt.fitsamprec) go to 10 !loop if not done transferring
c
c	write to tape
c
c	special debugg for checking buffer placements
c
C	brec(1)=255
C	brec(2)=255
C	brec(3)=255
C	brec(4)=255
C	brec(2877)=0
C	brec(2878)=0
C	brec(2879)=0
C	brec(2880)=0
	if (case.eq.1)
     1	call xvwrit (ouni1,brec,stat,'LINE',linectr,'NSAMPS',
     2 	krec,'BAND',kband,' ')
c
	if (case.eq.12) then
		call xvwrit (ouni1,hrec,stat,'LINE',linectr,'NSAMPS',
     1  	fitsamprec,'BAND',kband,' ')
	endif
c
	if (case.eq.20) then
		call xvwrit (ouni1,frec,stat,'LINE',linectr,'NSAMPS',
     1	fitsamprec,'BAND',kband,' ')
		linectr=linectr+1
		call xvwrit (ouni1,frec(720*bfctr+1),stat,'LINE',linectr,
     1	'NSAMPS',fitsamprec,'BAND',kband,' ')
	endif
c
	if (case.eq.21) then
		call xvwrit (ouni1,rrec,stat,'LINE',linectr,'NSAMPS',
     1	fitsamprec,'BAND',kband,' ')
		linectr=linectr+1
		call xvwrit (ouni1,rrec(720*bfctr+1),stat,'LINE',linectr,
     1	'NSAMPS',fitsamprec,'BAND',kband,' ')
	endif
c
	linectr=linectr+1		!bump line ctr
	krec=0				!reinit output record ctr
	go to 10			!loop back
c
c	done on input, clean up
c
40	continue
	kband=kband+1
	if (kband.le.inb) go to 5
	if (krec.eq.0) go to 60
	il=fitsamprec-krec
c
c	Fill out buffer with zeroes before last write so file ends correctly
c
	if (case.eq.1) then
		do i=1,il
		brec(i+krec)=0			!original has BZERO!!!
		enddo
	endif
	if (case.eq.12) then
		do i=1,il
		hrec(i+krec)=0			!original has BZERO!!!
		enddo
	endif
	if (case.eq.20) then
		do i=1,il
		frec(i+krec)=0			!original has BZERO!!!
		enddo
	endif
	if (case.eq.21) then
		do i=1,il
		rrec(i+krec)=0.0		!original has BZERO!!!
		enddo
	endif
c
c	special debugg
c
c	brec(1)=255
c	brec(2)=255
c	brec(3)=255
c	brec(4)=255
c	brec(2877)=0
c	brec(2878)=255
c	brec(2879)=0
c	brec(2880)=255
	kband=kband-1
	if (case.eq.1)
     1	call xvwrit (ouni1,brec,stat,'LINE',linectr,'NSAMPS',
     2	 fitsamprec,'BAND',kband,' ')
	if (case.eq.12) then
		call xvwrit (ouni1,hrec,stat,'LINE',linectr,'NSAMPS',
     1	fitsamprec,'BAND',kband,' ')
	endif
c
	if (case.eq.20) then
		call xvwrit (ouni1,frec,stat,'LINE',linectr,'NSAMPS',
     1	fitsamprec,'BAND',kband,' ')
		linectr=linectr+1
		call xvwrit (ouni1,frec(720*bfctr+1),stat,'LINE',linectr,
     1	'NSAMPS',fitsamprec,'BAND',kband,' ')
	endif
c
	if (case.eq.21) then
		call xvwrit (ouni1,rrec,stat,'LINE',linectr,'NSAMPS',
     1	fitsamprec,'BAND',kband,' ')
		linectr=linectr+1
		call xvwrit (ouni1,rrec(720*bfctr+1),stat,'LINE',linectr,
     1	'NSAMPS',fitsamprec,'BAND',kband,' ')
	endif
c					!Output data to FITS
	linectr=linectr+1		!in case of XTENSIONS=
60	continue			!done

	return
	end
C========================================================================
	subroutine ffitso
c
c	routine to read in VICAR halfword data and write out FITS data
c	in 8, 16 or 32 bit (integer) format
c	CASES = 2, 3, 13 and 23
c
	implicit none
	byte fitsdata(32000)
	integer*4 iuni,ouni1,ouni2,onl,ons,onb,trlab,ofnl,tsklen,syslen,case
	integer*4 ipixsize,opixsize,obitpix,ibitpix,bfctr,ntasks,linectr
	integer*4 instances(1000),inl,ins,inb,ihival,iloval
	logical*4 noproc,prfits,ihst,itask,iend,iblk,dbg	
	real*4 bscale,bzero,hival,loval,fitshi,fitslo
	character*6 intfmt,realfmt
	character*8 host
	character*50 version
	character*32 tasks(1000),itype,inorg,ifmt
	character*80 hdrrecord(360)
	character*132 filename
	character*80 fx(50)
c
	common /cpm/noproc,prfits,ihst,itask,iend,iblk,bfctr,filename,version
	common /cf1/iuni,ouni1,ouni2,ofnl,ipixsize,opixsize,obitpix,ibitpix
	common /cf2/onl,ons,onb,bscale,bzero,trlab,tsklen,syslen,ntasks,dbg
	common /cf3/itype,ifmt,inorg,host,intfmt,realfmt,case
	common /cf4/instances,tasks,linectr,inl,ins,inb
	common /cf5/hival,loval,ihival,iloval,fitshi,fitslo
	common /cdata/fitsdata,hdrrecord
	common /fitsx/fx
c
	byte 	  brec(28800)
	integer*2 hrec(14400)
	integer*4 fulldata(32000)
	integer*4 stat,i,j,fitsamprec,viclen,fitslen,kband,oword
	integer*4 vbyrec,fitsbytrec,il,krecm,krec,kpixel,krow,nrows
	integer*4 htransbuf(12),ftransbuf(12),rtransbuf(12)
	integer*4 frec(7200)
	real*4 rrec(7200)
	character*4 stype,dtype
	character*6 intfmto,realfmto
c
c 	From algorithm in Reference 1
c
C***************************
C     READ DATA RECORDS AND UNBLOCK BITS.
C     NL=NLIN  NB=BYREC  B/PX=BYPIX NS=NPIX
C***************************	
	nrows = inl			!max number of rows in VICAR image
	krecm = 2880			!basic fits record length in bytes
	vbyrec=ipixsize*ins		!number of bytes/record in VICAR image
	fitsbytrec=krecm*bfctr		!number of bytes/FITS record
	fitsamprec=fitsbytrec/opixsize	!number of samples/FITS record
c
c	set up translation buffers
c
	stype='FULL'
	dtype='HALF'
	intfmto='HIGH'
	realfmto='IEEE'
	call xvtrans_out (htransbuf,stype,dtype,intfmto,realfmto,stat)
	call chkstat (stat,'??E XVtrans_out error on htransbuf',0,0,0)
c
	dtype='FULL'
	call xvtrans_out (ftransbuf,stype,dtype,intfmto,realfmto,stat)
	call chkstat (stat,'??E XVtrans_out error on ftransbuf',0,0,0)
c
	dtype='REAL'
	call xvtrans_out (rtransbuf,stype,dtype,intfmto,realfmto,stat)
	call chkstat (stat,'??E XVtrans_out error on rtransbuf',0,0,0)
c
	il=0
	kband=1				!band counter on VICAR image
	krec = 0			!current sample ptr on FITS image
5	continue
	krow=0				!row counter on VICAR image
	kpixel=ins+1			!end pointer (samp)for VICAR input buff 
10	continue
	if (kpixel.lt.ins) go to 20	!br, if passed end of input buffer
	krow=krow+1			!bump VICAR row counter
	if (krow.gt.nrows) go to 40	!Br, if read in last VICAR buffer
c
c	read next row from internal storage to ROW
c
	call xvread(iuni,fulldata,stat,'LINE',krow,'NSAMPS',ins,
     1	'BAND',kband,' ')
c
	kpixel=0			!reset VICAR end pointer 
20	continue
	viclen=ins-kpixel		!rec len in bytes for VICAR
	fitslen=fitsamprec-krec		!rec len in bytes for FITS
	il=min0(viclen,fitslen)		!Min rec length (VICAR or FITS)
c
	if (case.eq.2) then
		do j=1,fitsamprec
		oword=fulldata(j+kpixel)
		oword=(oword-bzero)/bscale
		if (oword.gt.127) oword=255-oword
		brec(j+krec)=oword 
		enddo
	endif
c
	if (case.eq.3) then
		do j=1,fitsamprec
		oword=fulldata(j+kpixel)
		oword=(oword-bzero)/bscale
		if (oword.gt.127) oword=255-oword
		hrec(j+krec)=oword 
		enddo
	endif
c	if (case.eq.3)
c	1 call xvtrans (htransbuf,fulldata(1+kpixel),hrec(1+krec),
c	2 il)
c
	if (case.eq.13)
     1 call xvtrans (ftransbuf,fulldata(1+kpixel),frec(1+krec),
     2 il)
c
	if (case.eq.23)
     1 call xvtrans (rtransbuf,fulldata(1+kpixel),rrec(1+krec),
     2 il)
c	 
	kpixel=kpixel+il		!increment sample ctr for VICAR
	krec=krec+il			!increment sample ctr for FITS
	if (krec.lt.fitsamprec) go to 10 !loop if not done transferring
c
c	write to tape
c
c	special debugg for checking buffer placements
c
C	brec(1)=255
C	brec(2)=255
C	brec(3)=255
C	brec(4)=255
C	brec(2877)=0
C	brec(2878)=0
C	brec(2879)=0
C	brec(2880)=0
	if (case.eq.2)
     1	call xvwrit (ouni1,brec,stat,'LINE',linectr,'NSAMPS',
     2  krec,'BAND',kband,' ')
c
	if (case.eq.3) then
		call xvwrit (ouni1,hrec,stat,'LINE',linectr,'NSAMPS',
     1 	fitsamprec,'BAND',kband,' ')
	endif
c
	if (case.eq.13) then
		call xvwrit (ouni1,frec,stat,'LINE',linectr,'NSAMPS',
     1	fitsamprec,'BAND',kband,' ')
	endif
c
	if (case.eq.23) then
		call xvwrit (ouni1,rrec,stat,'LINE',linectr,'NSAMPS',
     1	fitsamprec,'BAND',kband,' ')
	endif
c
	linectr=linectr+1		!bump line ctr
	krec=0				!reinit output record ctr
	go to 10			!loop back
c
c	done on input, clean up
c
40	continue
	kband=kband+1
	if (kband.le.inb) go to 5
	if (krec.eq.0) go to 60
	il=fitsamprec-krec
c
c	Fill out buffer with zeroes before last write so file ends correctly
c
	if (case.eq.2) then
		do i=1,il
		brec(i+krec)=0			!original has BZERO!!!
		enddo
	endif
	if (case.eq.3) then
		do i=1,il
		hrec(i+krec)=0			!original has BZERO!!!
		enddo
	endif
	if (case.eq.13) then
		do i=1,il
		frec(i+krec)=0			!original has BZERO!!!
		enddo
	endif
	if (case.eq.23) then
		do i=1,il
		rrec(i+krec)=0.0		!original has BZERO!!!
		enddo
	endif
c
c	special debugg
c
c	brec(1)=255
c	brec(2)=255
c	brec(3)=255
c	brec(4)=255
c	brec(2877)=0
c	brec(2878)=255
c	brec(2879)=0
c	brec(2880)=255
	kband=kband-1	
	if (case.eq.2)
     1	call xvwrit (ouni1,brec,stat,'LINE',linectr,'NSAMPS',
     2	 fitsamprec,'BAND',kband,' ')
	if (case.eq.3) then
		call xvwrit (ouni1,hrec,stat,'LINE',linectr,'NSAMPS',
     1	fitsamprec,'BAND',kband,' ')
	endif
c
	if (case.eq.13) then
		call xvwrit (ouni1,frec,stat,'LINE',linectr,'NSAMPS',
     1	fitsamprec,'BAND',kband,' ')
	endif
c
	if (case.eq.23) then
		call xvwrit (ouni1,rrec,stat,'LINE',linectr,'NSAMPS',
     1	fitsamprec,'BAND',kband,' ')
	endif
c					!Output data to FITS
	linectr=linectr+1		!in case of XTENSIONS=
60	continue			!done

	return
	end
C========================================================================
	subroutine ifitso
c
c	routine to write out FITS record in integer format
c	
	implicit none
	byte fitsdata(32000)
	integer*4 iuni,ouni1,ouni2,onl,ons,onb,trlab,ofnl,tsklen,syslen,case
	integer*4 ipixsize,opixsize,obitpix,ibitpix,bfctr,ntasks,linectr
	integer*4 instances(1000),inl,ins,inb,ihival,iloval
	logical*4 noproc,prfits,ihst,itask,iend,iblk,dbg	
	real*4 bscale,bzero,hival,loval,fitshi,fitslo
	character*6 intfmt,realfmt
	character*8 host
	character*50 version
	character*32 tasks(1000),itype,inorg,ifmt
	character*80 hdrrecord(360)
	character*132 filename
c
	character*80 fx(50)
c
	common /cpm/noproc,prfits,ihst,itask,iend,iblk,bfctr,filename,version
	common /cf1/iuni,ouni1,ouni2,ofnl,ipixsize,opixsize,obitpix,ibitpix
	common /cf2/onl,ons,onb,bscale,bzero,trlab,tsklen,syslen,ntasks,dbg
	common /cf3/itype,ifmt,inorg,host,intfmt,realfmt,case
	common /cf4/instances,tasks,linectr,inl,ins,inb
	common /cf5/hival,loval,ihival,iloval,fitshi,fitslo
	common /cdata/fitsdata,hdrrecord
c
	common /fitsx/fx
c
	integer*4 stat,oword,i,j,fsamprec,viclen,fitslen,kband
	integer*4 vbyrec,fbyrec,il,krecm,krec,kpixel,krow,nrows
	integer*4 fwdata(8000),fwrec(7200)
	integer*2 hdata(16000),hrec(14400)
	real*4 vax(7200),rword
	byte rec(28800)
c
	equivalence (fitsdata(1),hdata(1))
	equivalence (fitsdata(1),fwdata(1))
	equivalence (rec(1),hrec(1))
	equivalence (rec(1),fwrec(1))
	equivalence (fitsdata(1),vax(1))
c
c 	From algorithm in Reference 1
c
C***************************
C     READ DATA RECORDS AND UNBLOCK BITS.
C     NL=NLIN  NB=BYREC  B/PX=BYPIX NS=NPIX
C***************************	
	nrows = inl			!max number of rows in VICAR image
	krecm = 2880			
	vbyrec=ipixsize*ins		!number of bytes/record in VICAR image
	fbyrec=2880*bfctr		!number of bytes/FITS record
	fsamprec=fbyrec/opixsize	!number of samples/FITS record

	il=0
	kband=1				!band counter on VICAR image
	krec = 0			!current sample ptr on FITS image
5	continue
	krow=0				!row counter on VICAR image
	kpixel=ins+1			!end pointer (samp)for VICAR input buff 
10	continue
	if (kpixel.lt.ins) go to 20	!br, if passed end of input buffer
	krow=krow+1			!bump VICAR row counter
	if (krow.gt.nrows) go to 40	!Br, if read in last VICAR buffer
c
c	read next row from internal storage to ROW
c
	call xvread(iuni,fitsdata,stat,'LINE',krow,'NSAMPS',vbyrec,
     1 'BAND',kband,' ')
	kpixel=0			!reset VICAR end pointer 
20	continue
	viclen=vbyrec-(kpixel*ipixsize)	!rec len in bytes for VICAR
	fitslen=fbyrec-(krec*opixsize)	!rec len in bytes for FITS
	il=min0(viclen,fitslen)		!Min rec length (VICAR or FITS)
	do 30 j=1,il
	rec(j+(krec*opixsize))=fitsdata(j+(kpixel*ipixsize)) 
30	continue
	kpixel=kpixel+il/ipixsize	!increment sample ctr for VICAR
	krec=krec+il/opixsize		!increment sample ctr for FITS
	if (krec*opixsize.lt.fbyrec) go to 10 !loop if not done transferring
c
c	apply bscale and bzero to REC
c	convert to 8, 16, or 32-bit integer
c	write to tape
c
c	if (obitpix.eq.8.and.ibitpix.eq.16) then apply BSCALE
	if (case.eq.1) then
		do j=1,fsamprec
		oword=hrec(j)
		oword=(oword-bzero)/bscale
		if (oword.gt.127) oword=255-oword
		rec(j)=oword
		enddo
	endif
c	if (obitpix.eq.8.and.ibitpix.eq.32) then apply BSCALE
	if (case.eq.2) then
		do j=1,fsamprec
		oword=fwrec(j)
		oword=(oword-bzero)/bscale
		if (oword.gt.127) oword=255-oword
		rec(j)=oword
		enddo
	endif
c	if (obitpix.eq.8.and.ibitpix.eq.-32) then apply BSCALE
	if (case.eq.4) then
		do j=1,fsamprec
		rword=vax(j)
		oword=(rword-bzero)/bscale
		if (oword.gt.127) oword=255-oword
		rec(j)=oword
		enddo
	endif
c	if (obitpix.eq.16.and.ibitpix.eq.32) then apply BSCALE
	if (case.eq.3) then
		do j=1,fsamprec
		oword=fwrec(j)
		oword=(oword-bzero)/bscale
		hrec(j)=oword
		enddo
	endif
c	if (obitpix.eq.16.and.ibitpix.eq.-32) then apply BSCALE
	if (case.eq.5) then
		do j=1,fsamprec
		rword=vax(j)
		oword=(rword-bzero)/bscale
		hrec(j)=oword
		enddo
	endif
c	if (obitpix.eq.32.and.ibitpix.eq.-32) then apply BSCALE
	if (case.eq.6) then
		do j=1,fsamprec
		oword=vax(j)
		oword=(oword-bzero)/bscale
		fwrec(j)=oword
		enddo
	endif
c***	call byte_swapper (rec,fsamprec,obitpix)
c
c	special debugg for checking buffer placements
c
C	rec(1)=255
C	rec(2)=255
C	rec(3)=255
C	rec(4)=255
C	rec(2877)=0
C	rec(2878)=0
C	rec(2879)=0
C	rec(2880)=0
	call xvwrit (ouni1,rec,stat,'LINE',linectr,'NSAMPS',fbyrec,
     1  'BAND',kband,' ')
	linectr=linectr+1		!bump line ctr
	krec=0				!reinit output record ctr
	go to 10			!loop back
c
c	done on input, clean up
c
40	continue
	kband=kband+1
	if (kband.le.inb) go to 5
	if (krec.eq.0) go to 60
	il=fbyrec-krec*opixsize
c
c	Fill out buffer with zeroes before last write so file ends correctly
c
	do i=1,il
	rec(i+krec*opixsize)=0		!original has BZERO!!!
	enddo
c
c	apply BSCALE and BZERO to REC
c	convert to 8, 16, or 32-bit integer
c	write last record to tape
c
c	if (obitpix.eq.8.and.ibitpix.eq.16) then apply BSCALE
	if (case.eq.1) then
		do j=1,fsamprec
		oword=hrec(j)
		oword=(oword-bzero)/bscale
		if (oword.gt.127) oword=255-oword
		rec(j)=oword
		enddo
	endif
c	if (obitpix.eq.8.and.ibitpix.eq.32) then apply BSCALE
	if (case.eq.2) then
		do j=1,fsamprec
		oword=fwrec(j)
		oword=(oword-bzero)/bscale
		if (oword.gt.127) oword=255-oword
		rec(j)=oword
		enddo
	endif
c	if (obitpix.eq.8.and.ibitpix.eq.-32) then apply BSCALE
	if (case.eq.4) then
		do j=1,fsamprec
		rword=vax(j)
		oword=(rword-bzero)/bscale
		if (oword.gt.127) oword=255-oword
		rec(j)=oword
		enddo
	endif
c	if (obitpix.eq.16.and.ibitpix.eq.32) then apply BSCALE
	if (case.eq.3) then
		do j=1,fsamprec
		oword=fwrec(j)
		oword=(oword-bzero)/bscale
		hrec(j)=oword
		enddo
	endif
c	if (obitpix.eq.16.and.ibitpix.eq.-32) then apply BSCALE
	if (case.eq.5) then
		do j=1,fsamprec
		rword=vax(j)
		oword=(rword-bzero)/bscale
		hrec(j)=oword
		enddo
	endif
c	if (obitpix.eq.32.and.ibitpix.eq.-32) then apply BSCALE
	if (case.eq.6) then
		do j=1,fsamprec
		oword=vax(j)
		oword=(oword-bzero)/bscale
		fwrec(j)=oword
		enddo
	endif
c**	call byte_swapper (rec,fsamprec,obitpix)
c
c	special debugg
c
c	rec(1)=255
c	rec(2)=255
c	rec(3)=255
c	rec(4)=255
c	rec(2877)=0
c	rec(2878)=255
c	rec(2879)=0
c	rec(2880)=255
	kband=kband-1
	call xvwrit(ouni1,rec,stat,'NSAMPS',fbyrec,'BAND',kband,
     1  'LINE',linectr,' ')
					!Output data to FITS
	linectr=linectr+1		!in case of XTENSIONS=
60	continue			!done

	return
	end
C========================================================================
c	SUBROUTINE BYTE_SWAPPER(BUF,NPIX,BTPIX)
cc
cc subroutine to swap bytes
cc
c      INTEGER*4 BUF(1)
c      INTEGER*4 NPIX,BTPIX,I
cc
c      IF(BTPIX.EQ.8)RETURN
c      IF(BTPIX.EQ.16)CALL BSWAP(BUF,NPIX)
c      IF(BTPIX.NE.32)RETURN
c      DO 10 I=1,NPIX
c10    BUF(I)=INTIBM(BUF(I))
c      RETURN
c      END
C========================================================================
	subroutine rfitso
c
c	routine to read in VICAR real format and write out FITS record in
c	8, 16, 32 bit integer or IEEE floating point format
c	CASES = 4, 5, 6, or 14
c
	implicit none
	byte fitsdata(32000)
	integer*4 iuni,ouni1,ouni2,onl,ons,onb,trlab,ofnl,tsklen,syslen,case
	integer*4 ipixsize,opixsize,obitpix,ibitpix,bfctr,ntasks,linectr
	integer*4 instances(1000),inl,ins,inb,ihival,iloval
	logical*4 noproc,prfits,ihst,itask,iend,iblk,dbg	
	real*4 bscale,bzero,hival,loval,fitshi,fitslo
	character*6 intfmt,realfmt
	character*8 host
	character*50 version
	character*32 tasks(1000),itype,inorg,ifmt
	character*80 hdrrecord(360)
	character*132 filename
c
	common /cpm/noproc,prfits,ihst,itask,iend,iblk,bfctr,filename,version
	common /cf1/iuni,ouni1,ouni2,ofnl,ipixsize,opixsize,obitpix,ibitpix
	common /cf2/onl,ons,onb,bscale,bzero,trlab,tsklen,syslen,ntasks,dbg
	common /cf3/itype,ifmt,inorg,host,intfmt,realfmt,case
	common /cf4/instances,tasks,linectr,inl,ins,inb
	common /cf5/hival,loval,ihival,iloval,fitshi,fitslo
	common /cdata/fitsdata,hdrrecord
c
	byte brec(28800)
	integer*2 hrec(14400)
	integer*4 stat,oword,i,j,fitsamprec,viclen,fitslen,kband
	integer*4 vbyrec,fitsbytrec,il,krecm,krec,kpixel,krow,nrows
	integer*4 htransbuf(12),ftransbuf(12),rtransbuf(12),rntransbuf(12)
	integer*4 frec(7200)
	real*4 rword,rdata(7200),rec(7200)
	character*4 stype,dtype
	character*6 intfmto,realfmto
c
c
cc	equivalence (rdata(1),fitsdata(1))
cc	equivalence (rec(1),vax(1))
c
c 	From algorithm in Reference 1
c
C***************************
C     READ DATA RECORDS AND UNBLOCK BITS.
C     NL=NLIN  NB=BYREC  B/PX=BYPIX NS=NPIX
C***************************	
	nrows = inl			!max number of rows in VICAR image
	krecm = 2880			!basic FITS record length in bytes
	vbyrec=ipixsize*ins		!number of bytes/record in VICAR image
	fitsbytrec=krecm*bfctr		!number of bytes/FITS record
	fitsamprec=fitsbytrec/opixsize	!number of samples/FITS record
c
c	set up translation buffers --- may not need!
c
	stype='REAL'
	dtype='HALF'
	intfmto='HIGH'
	realfmto='IEEE'
	call xvtrans_out (htransbuf,stype,dtype,intfmto,realfmto,stat)
	call chkstat (stat,'??E XVtrans_out error on htransbuf',0,0,0)
c
	dtype='FULL'
	call xvtrans_out (ftransbuf,stype,dtype,intfmto,realfmto,stat)
	call chkstat (stat,'??E XVtrans_out error on ftransbuf',0,0,0)
c
	dtype='REAL'
	call xvtrans_out (rtransbuf,stype,dtype,intfmto,realfmto,stat)
	call chkstat (stat,'??E XVtrans_out error on rtransbuf',0,0,0)
c
	intfmto='NATIVE'
	realfmto='NATIVE'
	call xvtrans_out (rntransbuf,stype,dtype,intfmto,realfmto,stat)
	call chkstat (stat,'??E XVtrans_out error on rntransbuf',0,0,0)
c	
	il=0
	kband=1				!band counter on VICAR image
	krec = 0			!current sample ptr on FITS image
5	continue
	krow=0				!row counter on VICAR image
	kpixel=ins+1			!end pointer (samp)for VICAR input buff 
10	continue
	if (kpixel.lt.ins) go to 20	!br, if passed end of input buffer
	krow=krow+1			!bump VICAR row counter
	if (krow.gt.nrows) go to 40	!Br, if read in last VICAR buffer
c
c	read next row from internal storage to ROW
c
	call xvread(iuni,rdata,stat,'LINE',krow,'NSAMPS',ins,
     1 'BAND',kband,' ')
	kpixel=0			!reset VICAR end pointer 
20	continue
	viclen=ins-kpixel		!rec len in samples for VICAR
	fitslen=fitsamprec-krec		!rec len in samples for FITS
	il=min0(viclen,fitslen)		!Min rec length (VICAR or FITS)
c	if (obitpix.eq.8.and.ibitpix.eq.-32) then apply BSCALE
	if (case.eq.4) then
		do j=1,fitsamprec			!!fitsamprec or il??
		rword=rdata(j+kpixel)
		oword=(rword-bzero)/bscale
		if (oword.gt.127) oword=255-oword
		brec(j+krec)=oword
		enddo
	endif
c	if (obitpix.eq.16.and.ibitpix.eq.-32) then apply BSCALE
	if (case.eq.5) then
		do j=1,fitsamprec			!!fitsamprec or il??
		rword=rdata(j+kpixel)
		oword=(rword-bzero)/bscale
		hrec(j+krec)=oword
		enddo
	endif
c	if (obitpix.eq.32.and.ibitpix.eq.-32) then apply BSCALE
	if (case.eq.6) then
		do j=1,fitsamprec			!!fitsamprec or il??
		oword=rdata(j+kpixel)
		oword=(oword-bzero)/bscale
		frec(j+krec)=oword
		enddo
	endif
c	if (obitpix.eq.-32.and.ibitpix.eq.-32) then apply BSCALE
	if (case.eq.14) then
		do j=1,fitsamprec			!!fitsamprec or il??
			rec(j+krec)=rdata(j+kpixel)*bscale-bzero 
		enddo
	endif
	
	kpixel=kpixel+il		!increment sample ctr for VICAR
	krec=krec+il			!increment sample ctr for FITS
	if (krec.lt.fitsamprec) go to 10	!loop if not done transferring
c
c	apply bscale and bzero to REC
c	convert to IEEE Real*4
c	write to tape
c
cc	do j=1,fitsamprec
cc	vax(j)=vax(j)*bscale-bzero
cc	call vax_ieee_r (vax(j),ieee(j))
cc	enddo
c
c	special debugg for checking buffer placements
c
c	ieee(1)=-1
c	ieee(720)=0
	if (case.eq.4)
     1	call xvwrit (ouni1,brec,stat,'LINE',linectr,'NSAMPS',
     2 	krec,'BAND',kband,' ')
c
	if (case.eq.5) then
		call xvwrit (ouni1,hrec,stat,'LINE',linectr,'NSAMPS',
     1 	fitsamprec,'BAND',kband,' ')
	endif
c
	if (case.eq.6) then
		call xvwrit (ouni1,frec,stat,'LINE',linectr,'NSAMPS',
     1	fitsamprec,'BAND',kband,' ')
	endif
c
	if (case.eq.14) then
		call xvwrit (ouni1,rec,stat,'LINE',linectr,'NSAMPS',
     1	fitsamprec,'BAND',kband,' ')
	endif
c
cc	call xvwrit (ouni1,ieee,stat,'LINE',linectr,'NSAMPS',fitsamprec,
cc	1 'BAND',kband,' ')
	linectr=linectr+1		!bump line ctr
	krec=0				!reinit output record ctr
	go to 10			!loop back
c
c	done on input, clean up
c
40	continue
	kband=kband+1
	if (kband.le.inb) go to 5
	if (krec.eq.0) go to 60
	il=fitsamprec-krec
c
c	Fill out buffer with zeroes before last write so file ends correctly
c
	if (case.eq.4) then
		do i=1,il
		brec(i+krec)=0			!original has BZERO!!!
		enddo
	endif
	if (case.eq.5) then
		do i=1,il
		hrec(i+krec)=0			!original has BZERO!!!
		enddo
	endif
	if (case.eq.6) then
		do i=1,il
		frec(i+krec)=0			!original has BZERO!!!
		enddo
	endif
	if (case.eq.14) then
		do i=1,il
		rec(i+krec)=0.0		!original has BZERO!!!
		enddo
	endif
c
c
c	apply bscale and bzero to REC
c	convert to IEEE Real*4
c	write to tape
c
cc	do j=1,fitsamprec
cc	vax(j)=vax(j)*bscale-bzero
cc	call vax_ieee_r (vax(j),ieee(j))
cc	enddo
c
c	if (obitpix.eq.8.and.ibitpix.eq.-32) then apply BSCALE
	if (case.eq.4) then
		do j=1,fitsamprec			!!fitsamprec or il??
		rword=rdata(j+kpixel)
		oword=(rword-bzero)/bscale
		if (oword.gt.127) oword=255-oword
		brec(j+krec)=oword
		enddo
	endif
c	if (obitpix.eq.16.and.ibitpix.eq.-32) then apply BSCALE
	if (case.eq.5) then
		do j=1,fitsamprec			!!fitsamprec or il??
		rword=rdata(j+kpixel)
		oword=(rword-bzero)/bscale
		hrec(j+krec)=oword
		enddo
	endif
c	if (obitpix.eq.32.and.ibitpix.eq.-32) then apply BSCALE
	if (case.eq.6) then
		do j=1,fitsamprec			!!fitsamprec or il??
		oword=rdata(j+kpixel)
		oword=(oword-bzero)/bscale
		frec(j+krec)=oword
		enddo
	endif
c	if (obitpix.eq.-32.and.ibitpix.eq.-32) then apply BSCALE
	if (case.eq.14) then
		do j=1,fitsamprec			!!fitsamprec or il??
			rec(j+krec)=rdata(j+kpixel)*bscale-bzero 
		enddo
	endif
c
c	special debugg
c
c	ieee(1)=-1
c	ieee(719)=0
c	ieee(720)=-1
	kband=kband-1
cc	call xvwrit(ouni1,ieee,stat,'NSAMPS',fitsamprec,'BAND',kband,
cc	1  'LINE',linectr,' ')
					!Output data to FITS
	if (case.eq.4)
     1	call xvwrit (ouni1,brec,stat,'LINE',linectr,'NSAMPS',
     2 	krec,'BAND',kband,' ')
c
	if (case.eq.5) then
		call xvwrit (ouni1,hrec,stat,'LINE',linectr,'NSAMPS',
     1 	fitsamprec,'BAND',kband,' ')
	endif
c
	if (case.eq.6) then
		call xvwrit (ouni1,frec,stat,'LINE',linectr,'NSAMPS',
     1	fitsamprec,'BAND',kband,' ')
	endif
c
	if (case.eq.14) then
		call xvwrit (ouni1,rec,stat,'LINE',linectr,'NSAMPS',
     1	fitsamprec,'BAND',kband,' ')
	endif
c
	linectr=linectr+1		!in case of XTENSIONS=
60	continue			!done
	return
	end
c	
C========================================================================
	BLOCK DATA
c
c	SIMPLE  =           T / FITS standard
C	BITPIX	=             / FITS bits/pixel
c	NAXIS   =             / Dimensions
c	NAXIS1  =             / Samples
c	NAXIS2	=             / Lines
c	NAXIS3  =             / Bands
c	BLOCKED =           T / tape may be blocked in multiples of 2880
c	EXTEND  =           T / tape may have standard FITS extensions
c	ORIGIN  = 'JPL-MIPL'  / VICAR
c	VERSION = '        '  / VICAR Baseline
c	USERID  = 'RJB050  '  / User
c	HOST	= 'VAX-VMS '  / Computer system
c	INTFMT  = 'LOW     '  / VAX Integer - Low byte first
c	REALFMT = 'VAX     '  / VAX Real format
c	DATATYPE= 'IMAGE   '  / Original VICAR data set type
c	
c	DATE    = '20-AUG-90 '/ Date of transfer to FITS
c	FILENAME=
c	VIC-MAX =
c	VIC-MIN =
c	VIC-B/P =
c	VIC-FMT =
c	BZERO   =          0.0/ Bias Value
c	BSCALE  =          1.0/ Slope
c	BUNIT   = 'DN      '  / Brightness in digital numbers
c	BLANK   =            0/ 
c	DATAMAX =             / Max value after conversion to FITS
c	DATAMIN =             / Min value after conversion to FITS
c	CTYPE1  = 'PIXEL   '  / 
c	END
c
	character*80 fx(50)
	
	common /fitsx/fx
c
C    1/'         1111111111222222222233333333334444444444555555555566666
C    1/'1234567890123456789012345678901234567890123456789012345678901234
C    26666677777777778'/
C    25678901234567890'/
c               11111111112222222222333333333344444444445555555555666666666677777777778
c      12345678901234567890123456789012345678901234567890123456789012345678901234567890
C  IF You use the F66 standard then columns are max 72 char and you must              
C  have all of the following statements in the format:                                
c	data fx(1)                                                                    
c     1/'SIMPLE  =                    T /  format follows FITS standards |<--         
c     2                '/                                                             
c	else if you compile with -ffixed-line-length-none the you can use:            
	data fx(1)
     1/'SIMPLE  =                    T /  format follows FITS standards                '/
 	DATA fx(2)
     1/'BITPIX  =                   16 /  2-byte twos-complement integers              '/
	DATA fx(3)
     1/'BITPIX  =                    8 /  8-bit unsigned integers                      '/
	DATA fx(4)
     1/'BITPIX  =                   32 /  32-BIT twos-complement signed integers       '/
	DATA fx(5)
     1/'BITPIX  =                  -32 /  IEEE 32-bit floating point values            '/
	DATA fx(6)
     1/'NAXIS   =                    2 /  number of axes                               '/
	DATA fx(7)
     1/'NAXIS1  =                  256 /  number of pixels per row                     '/
	DATA fx(8)
     1/'NAXIS2  =                  256 /  number of rows                               '/
	DATA fx(9)
     1/'NAXIS3  =                   12 /  number of bands                              '/
	DATA fx(10)
     1/'BSCALE  =         1.278419E-07 /  real = tape*BSCALE + BZERO                   '/
	DATA fx(11)
     1/'BZERO   =                  0.0 /  amount of bias added to each FITS pixel      '/
	DATA fx(12)
     1/'BUNIT   = ''DN      ''           /  units of brightness                          '/
	DATA fx(13)
     1/'BLANK   =                    0 /  FITS undefined data point value (if present) '/
	DATA fx(14)
     1/'        =                      /                                               '/
	DATA fx(15)
     1/'BLOCKED =                    T /  tape may be blocked in multiplies of 2880    '/
	DATA fx(16)
     1/'EXTEND  =                    T /  tape may have standard FITS extensions       '/
	DATA fx(17)
     1/'CTYPE1  = ''SAMPLE  ''           /                                               '/
	DATA fx(18)
     1/'CDELT1  =         1.000000E+00 /  sample increment on x-axis                   '/
	DATA fx(19)
     1/'CRPIX1  =         1.000000E+00 /  reference pixel location in sample axis      '/
	DATA fx(20)
     1/'HOST    =                      /  host processor (cpu) for VICAR data set      '/
	DATA fx(21)
     1/'USERID  =                      /                                               '/
	DATA fx(22)
     1/'DATE    =                      /  date of transfer to FITS                     '/
	DATA fx(23)
     1/'DATAMAX =      4.495524406E+00 /  max FITS pixel value (after scaling)         '/
	DATA fx(24)
     1/'DATAMIN =     -1.217470840E-01 /  min FITS pixel value (after scaling)         '/
	DATA fx(25)
     1/'COMMENT                                                                        '/
	DATA fx(26)
     1/'ORIGIN  = ''JPL-MIPL   PGM=FITSOUT  (V1)''                                       '/
	DATA fx(27)
     1/'VIC-ITEM                                                                       '/
	DATA fx(28)
     1/'CRVAL1  =         1.000000E+00 /  starting sample of image                     '/
	DATA fx(29)
     1/'END                                                                            '/
	DATA fx(30)
     1/'VERSION =                      /  VICAR software baseline                      '/       
	DATA fx(31)
     1/'DATATYPE=                      /  original VICAR data set type                 '/
	DATA fx(32)
     1/'INTFMT  =                      /  VICAR integer byte order (HIGH=big-endian)   '/
	DATA fx(33)
     1/'REALFMT =                      /  VICAR real*4 internal format (IEEE=IEEE-754) '/
	DATA fx(34)
     1/'FILENAME=                      /  VICAR filename                               '/
	DATA fx(35)
     1/'VIC-MIN =                      /  minimum value in VICAR data set              '/
	DATA fx(36)
     1/'VIC-MAX =                      /  maximum value in VICAR data set              '/
	DATA fx(37)
     1/'VIC-B/P =                      /  bits per pixel in VICAR data set             '/
	DATA fx(38)
     1/'VIC-FMT =                      /  data format in VICAR data set                '/
	DATA fx(39)
     1/'HISTORY = ''VICAR HISTORY LABEL LISTING FOLLOWS''                                '/
	DATA fx(40)
     1/'VIC-TASK=            USER=               DAT_TIM =                              '/
	DATA fx(41)
     1/'CTYPE2  = ''LINE    ''           /                                                '/
	DATA fx(42)
     1/'CDELT2  =        -1.000000E+00 /  line increment on y-axis                      '/
	DATA fx(43)
     1/'CRPIX2  =         1.000000E+00 /  reference pixel location in line axis         '/
	DATA fx(44)
     1/'CRVAL2  =         1.000000E+00 /  starting line of image                        '/
	DATA fx(45)  
     1/'CTYPE3  = ''BAND   ''           /                                                 '/
	DATA fx(46)
     1/'CDELT3  =        -1.000000E+00 /  spectral band increment on z-axis             '/
	DATA fx(47)
     1/'CRPIX3  =         1.000000E+00 /  reference pixel location in band axis         '/
	DATA fx(48)
     1/'CRVAL3  =         1.000000E+00 /  starting band of image                        '/
	DATA fx(49)
     1/'COMMENT   ''VICAR Pixel (1,1) is at upper left corner''                           '/
c***fx(50) is always blank since it is used to blank out character strings
	DATA fx(50)
     1/'                                                                                '/
	END
