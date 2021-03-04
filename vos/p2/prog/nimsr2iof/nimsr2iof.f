	include 'VICMAIN_FOR'
	subroutine main44

c  NIMSR2IOF:   convert a NIMS radiance cube to IOF ( = pi * I / F(Solar) ),
c  using the solar flux in the label.

c   2mar98 -lwk- initial version
c  27mar98 -lwk- fix processing of "special values"
c  15nov00 -lwk- check NS against buffer size
c  14nov02 -lwk- added FORMAT to xlget call for unix port

	parameter (maxl = 10000)		! max. line length
	parameter (ntasks=3)		! # of valid TASKs for label
	character*3 org
	character*4 fmt
	character*132 pbuf
	character*8 tasks(ntasks)/ 'NIMSCMM2', 'VISIS2', 'NIMSCMM'/,
	1 task
	real*4 wav(408), sflux(408), obuf(maxl)
	real*8 xpi/3.1415926536/
	real*4 valid_min/-1.7014101e38/
	logical nosv, xvptst

	call xvmessage('NIMSR2IOF version 2019-11-05',' ')

c  open input cube and check format:
	call xvunit( inp, 'INP', 1, i,' ')
	call xvopen( inp, i, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',' ')
	call xvget( inp, i, 'NL', nl, 'NS', ns, 'NB', nb, 'ORG', org,
	1 'FORMAT', fmt,' ')
	if (fmt.ne.'REAL') call mabend(' ** input cube must be Real **')
	if (org.ne.'BSQ') call mabend(' ** only BSQ is supported **')
	if (ns.gt.maxl) call mabend(' ** NS exceeds buffer size **')

c  get the Solar Flux from the label:
	do i=1,ntasks
	  task = tasks(i)
	  call xlget( inp, 'HISTORY', 'SOLAR_F', sflux, ist, 'NELEMENT',
	1  -1, 'NRET', nb0, 'FORMAT', 'REAL', 'HIST', task, ' ')
	  if (ist.eq.1) go to 10
	enddo
	if (ist.ne.1) call mabend(' ** no Solar Flux in label **')
10	if (nb0.ne.nb) call mabend(' ** Solar Flux has wrong length **')

c  open the output file
	call xvunit( iout, 'OUT', 1, i,' ')
	call xvopen( iout, i, 'U_NL', nl, 'U_NS', ns, 'U_NB', nb, 'OP',
	1  'WRITE', 'O_FORMAT', 'REAL', 'U_FORMAT', 'REAL', 'OPEN_ACT', 'SA',
	2  'IO_ACT', 'SA',' ')

	nosv = xvptst('NOSPCVAL')

c  main processing:
	do ib = 1,nb
	  do il = 1,nl
	    call xvread( inp, obuf, i, ' ')
	    do is = 1,ns
	      if (obuf(is).lt.valid_min) then
		if (nosv) obuf(is) = 0.0
	      else
		obuf(is) = obuf(is)*xpi/sflux(ib)
	      endif
	    enddo
	    call xvwrit( iout, obuf, i, ' ')
	  enddo
	enddo

	call xvclose( inp, i,' ')
	call xvclose( iout, i,' ')

	return
	end
