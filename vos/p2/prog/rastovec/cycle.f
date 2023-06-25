CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Control the cycling of polygons
C
      SUBROUTINE CYCLE(NPOLY,NSEGS,NVERT,SERL,VXREF,XYA,
     +		IVBUF,IVPTR,XYBUF,NVERT2,GR1)
      IMPLICIT NONE
      INTEGER*4 NPOLY,NSEGS,NVERT,NVERT2
      INTEGER*4 SERL(4,NSEGS)		!Segment start,end,right,left
      INTEGER*4 VXREF(4,NVERT)		!2 to 4 segments for each vertex
      INTEGER*2 XYA(2,NVERT)
      INTEGER*4 IVBUF(1),IVPTR(1)
      INTEGER*2 XYBUF(2,1)
      LOGICAL GR1

      INTEGER*4 IDUM,IPOLY,IY1,IY2
      INTEGER*4 ISTART,IEND,IRIGHT,IVCNT,MAX_IVCNT,IHVTX,LOWY
      INTEGER*4 ISEGST,IPIS,IOCF,IOTYP
      LOGICAL XVPTST,LUNWND,ZINGER,FSTART
  101 FORMAT(' Polygon region=',I10)
  102 FORMAT(' Begin loop thru',I10,' polygon regions...')

      IOTYP = 1			!IOTYP=2 currently not used
      LUNWND = XVPTST('UNWIND')
cccc      LUNWND = .TRUE.
      ZINGER = XVPTST('ZINGER')

      NVERT2 = 0
      ISEGST = 1
      MAX_IVCNT = 0
      WRITE (6,102) NPOLY

C     ....Loop through the polygons
      DO 100 IPOLY=1,NPOLY
      IF (MOD(IPOLY,10000).EQ.0) WRITE(6,101) IPOLY
      IOCF = 1
      IPIS = ISEGST
      IVCNT = 0

   20 CALL LINKS(ISEGST,IPOLY,IVCNT,FSTART,IHVTX,LOWY,
     +		SERL,VXREF,XYA,IVBUF)
      IF (FSTART) THEN
  	 ISEGST = ISEGST + 1
         IF (ISEGST.GE.NSEGS) GOTO 100
         GOTO 20	!Go to next segment
      ENDIF

C     ....Polygon cycled - output it
      CALL OUTPOL(IPOLY,IVCNT,IOCF,IHVTX,IOTYP,LUNWND,
     +		SERL,VXREF,XYA,IVBUF,IVPTR)
CCC      IF(.NOT.ZINGER) GOTO 75

C     ....Now go looking for islands
      IOCF=2

   50 ISTART = SERL(1,IPIS)
      IEND = SERL(2,IPIS)
      IY1 = XYA(2,ISTART)
      IF(IY1.LT.LOWY) GOTO 75
      IY2 = XYA(2,IEND)
      IF (IY2.LT.LOWY) GOTO 75

C     ....Its in the correct range
      IRIGHT = SERL(3,IPIS)
      IF(IRIGHT.LE.0 .OR. IRIGHT.NE.IPOLY) GOTO 70

C     ....It has the correct adjacent polygon
      CALL LINKS(IPIS,IPOLY,IVCNT,FSTART,IHVTX,IDUM,
     +		SERL,VXREF,XYA,IVBUF)

C     ....If island is found, output it
      IF (.NOT.FSTART) THEN
         CALL OUTPOL(IPOLY,IVCNT,IOCF,IHVTX,IOTYP,
     +		LUNWND,	SERL,VXREF,XYA,IVBUF,IVPTR)
         IF (IVCNT.GT.MAX_IVCNT) MAX_IVCNT=IVCNT      
      ENDIF
   70 IPIS = IPIS+1
      IF (IPIS.LE.NSEGS) GOTO 50	!Try to find another island

   75 CALL CLOSE_POL(IPOLY,XYA,IVBUF,IVPTR,XYBUF,NVERT2,ZINGER,GR1)
  100 CONTINUE

      CALL PRNT(4,1,MAX_IVCNT,'MAX_IVCNT=.')
      RETURN
      END