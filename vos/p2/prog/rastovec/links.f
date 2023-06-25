CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Cycle segments to form polygon.
C
      SUBROUTINE LINKS(ISSEG,IPOLY,IVCNT,FSTART,IHIGH,LOWY,
     +		SERL,VXREF,XYA,IVBUF)
      IMPLICIT NONE
      INTEGER*4 ISSEG,IPOLY		!polygon and segment indices (input)
      INTEGER*4 IVCNT,IHIGH,LOWY
      INTEGER*4 SERL(4,1),VXREF(4,1)
      INTEGER*2 XYA(2,1)
      INTEGER*4 IVBUF(1)
      LOGICAL FSTART			!false start flag

      COMMON/CMAX/MAXPOLY,MAXVERT,MAXSIZE
      INTEGER MAXPOLY,MAXVERT,MAXSIZE

      INTEGER*4 I,J,NS
      INTEGER*4 ISTART			!Starting vertex
      INTEGER*4 ISEG,ISVTX,IEVTX	!Current segment
      INTEGER*4 JSEG,JSVTX,JEVTX	!Next segment
      INTEGER*4 JRIGHT,JLEFT		!Next segment right and left regions
      INTEGER*4 IX1,IX2,IY1,IY2,IY	!Current seg start & end coordinates

      INTEGER*4 JSEGA(2),JSVTXA(2),JEVTXA(2)  !Next segments (1 and 2)
      INTEGER*4 IX11,IY11		!Next seg ending coordinates (1)
      INTEGER*4 IX22,IY22		!Next seg ending coordinates (2)
      INTEGER*4 RORL(2)			!3=Right OR 4=Left
      LOGICAL HIGH

C     ....Look for a horizontal segment of the given class
      FSTART = .TRUE.
      ISEG = ISSEG
      IF (SERL(3,ISEG).NE.IPOLY) RETURN	!Not the same class
      ISVTX = SERL(1,ISEG)
      IEVTX = SERL(2,ISEG)
      IX1 = XYA(1,ISVTX)
      IX2 = XYA(1,IEVTX)
      IF (IX1.EQ.IX2) RETURN		!Vertical segment

C     ....A starting segment was located - now cycle
      FSTART = .FALSE.
      ISTART = ISVTX
      SERL(3,ISEG) = -SERL(3,ISEG)	!Flag segment as used
      JSEG = VXREF(1,ISVTX)

      IF (JSEG.LT.0) THEN
         IHIGH = ISVTX
         HIGH = .TRUE.
      ELSE
         IHIGH = 0
         HIGH = .FALSE.
      ENDIF
      LOWY = XYA(2,ISVTX)

   20 IF (IVCNT.GE.MAXSIZE) GOTO 990
      IVCNT = IVCNT + 1
      IVBUF(IVCNT) = ISIGN(ISVTX,JSEG)
      IF (IEVTX.EQ.ISTART) RETURN	 	!Cycle completed
      NS = 0

C     ....Loop through connected segments
      DO 50 I=1,4
      JSEG = VXREF(I,IEVTX)
      IF (JSEG.EQ.0) GOTO 50
      IF (JSEG.LT.0) HIGH=.TRUE.
      J = IABS(JSEG)
      IF (J.EQ.ISEG) GOTO 50
      JSVTX = SERL(1,J)
      JEVTX  = SERL(2,J)
      JRIGHT  = SERL(3,J)
      JLEFT  = SERL(4,J)

      IF (JSVTX.NE.IEVTX .OR. JRIGHT.NE.IPOLY) GOTO 25
      NS = NS + 1		!Normally oriented segment found
      JSVTXA(NS) = JSVTX
      JEVTXA(NS) = JEVTX
      JSEGA(NS) = JSEG
      RORL(NS) = 3
      GOTO 50

   25 IF(JEVTX.NE.IEVTX .OR. JLEFT.NE.IPOLY) GOTO 50
      NS = NS + 1		!Reverse oriented segment found
      JSVTXA(NS) = JEVTX
      JEVTXA(NS) = JSVTX
      JSEGA(NS) = JSEG
      RORL(NS) = 4
   50 CONTINUE

      IF (NS.GT.2) THEN
         CALL PRNT(4,1,'NS=.')
         CALL ABEND
      ENDIF

C     ....Find most counter-clockwise segment if more than one segment found.
      IF(NS.EQ.1) GO TO 60
      IX1 = XYA(1,ISVTX)
      IY1 = XYA(2,ISVTX)
      IX2 = XYA(1,IEVTX)
      IY2 = XYA(2,IEVTX)

      IX11 = XYA(1,JEVTXA(1))
      IY11 = XYA(2,JEVTXA(1))
      IX22 = XYA(1,JEVTXA(2))
      IY22 = XYA(2,JEVTXA(2))
      NS = 1
      IF (IX1.NE.IX2) THEN
         IF(IX1.LT.IX2 .AND. IY22.LT.IY11) NS=2
         IF(IX1.GT.IX2 .AND. IY22.GT.IY11) NS=2
      ELSE
         IF(IY1.LT.IY2 .AND. IX22.GT.IX11) NS=2
         IF(IY1.GT.IY2 .AND. IX22.LT.IX11) NS=2
      ENDIF

   60 ISVTX = JSVTXA(NS)			!Step to the next segment
      IEVTX = JEVTXA(NS)
      JSEG = JSEGA(NS)
      ISEG = IABS(JSEG)
      SERL(RORL(NS),ISEG) = -IPOLY	!Flag the segment as used
      IF (IHIGH.EQ.0 .AND. HIGH) IHIGH=ISVTX
      IY = XYA(2,ISVTX)
      IF (IY.LT.LOWY) LOWY=IY
      GOTO 20

  990 CALL MABEND('***MAXSIZE exceeded')
      END
