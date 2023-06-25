CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Find the boundary segments between polygons represented in an image form.
C
      SUBROUTINE COMPS(LABELEDF,NLINE,NSAMP,PROW,NROW,VLBUF,HLBUF,
     +		ISCNT,IVCNT,IHIGH)
      IMPLICIT NONE
      INTEGER*4 LABELEDF,NLINE,NSAMP,ISCNT,IVCNT,IHIGH
      INTEGER*4 NROW(1),PROW(1)		!Current and previous lines
      INTEGER*2 VLBUF(1),HLBUF(1)	!Vertex (sample,line) coordinates

      COMMON/CMAX/MAXPOLY,MAXVERT
      INTEGER MAXPOLY,MAXVERT

      INTEGER*4 IQ1,IQ2,IQ3,IQ4
      INTEGER*4 IORDER,IVL
      INTEGER*4 IVRTX,IPVRTX
      INTEGER*4 J,IND,I1,HLCNT
      INTEGER*4 ISTART,IEND
      INTEGER*4 FVHL	!First vertex of current line
  101 FORMAT(' Line=',I5)

C     ....The labeled image (LABELEDF) is surrounded by a 1-pixel border of
C     ....invalid values.
      CALL MVE(4,NSAMP+2,-9999,PROW,0,1)
      NROW(1) = -9999
      NROW(NSAMP+2) = -9999

      IHIGH=0		!Number of high-order vertices
      IVCNT=0		!Number of vertices (size of VLBUF, HLBUF)
      ISCNT=0		!Number of segments
      HLCNT=0		!Image line number (=I1)

C     ....Loop through all row boundaries
      DO 900 I1=1,NLINE+1
      IF (MOD(I1,100).EQ.0) WRITE(6,101) I1
      IF (I1.LE.NLINE) THEN
         CALL XVREAD(LABELEDF,NROW(2),IND,' ')
      ELSE
         CALL MVE(4,NSAMP,-9999,NROW(2),0,1)
      ENDIF
      HLCNT = HLCNT + 1		!always equal to line number (I1)
      FVHL = IVCNT + 1
C
C     ....Loop through each column boundary and find line segment end points
      DO 200 J=1,NSAMP+1
      IQ1=PROW(J)
      IQ2=PROW(J+1)
      IQ3=NROW(J+1)
      IQ4=NROW(J)
      IF(IQ1.EQ.IQ4 .AND. IQ2.EQ.IQ3) GOTO 200	!Go on if no vertex exists
      IF(IQ1.EQ.IQ2 .AND. IQ3.EQ.IQ4) GOTO 200
      IVCNT = IVCNT + 1
      IF (IVCNT.EQ.MAXVERT) GOTO 990

C     ....A vertex has been found
      IF(IQ1.EQ.IQ4 .OR. IQ2.EQ.IQ3) GOTO 150

C     ....A segment ends and begins
      IEND = IVCNT		!End the segment
      VLBUF(IVCNT) = J		!Sample coordinate
      HLBUF(IVCNT) = HLCNT	!Line coordinate
      CALL WSERL(VLBUF,HLBUF,NLINE,ISCNT,ISTART,IEND,
     +          IQ4,IQ1)

      ISCNT = ISCNT + 1		!Begin a new segment
      ISTART = IVCNT
      VLBUF(IVCNT) = J
      HLBUF(IVCNT) = HLCNT
      GOTO 180

C     ....A segment ends
  150 IF (IQ1.NE.IQ4 .AND. IQ2.EQ.IQ3) THEN
         IEND = IVCNT
         VLBUF(IVCNT) = J
         HLBUF(IVCNT) = HLCNT
         CALL WSERL(VLBUF,HLBUF,NLINE,ISCNT,ISTART,IEND,
     +          IQ4,IQ1)
      ENDIF
C     ....A segment begins
      IF (IQ1.EQ.IQ4 .AND. IQ2.NE.IQ3) THEN
         ISCNT = ISCNT + 1
         ISTART = IVCNT
         VLBUF(IVCNT) = J
         HLBUF(IVCNT) = HLCNT
      ENDIF

C     ....Determine if vertex is high order
  180 IORDER=4
      IF(IQ1.EQ.IQ2) IORDER=IORDER-1
      IF(IQ2.EQ.IQ3) IORDER=IORDER-1
      IF(IQ3.EQ.IQ4) IORDER=IORDER-1
      IF(IQ4.EQ.IQ1) IORDER=IORDER-1
      IF(IORDER.GE.3) IHIGH=IHIGH+1
  200 CONTINUE

C     ....Go back and connect incoming (from top) vertical segments
      IF (HLCNT.LE.1) GOTO 900	!First line can't have any of course
      IVRTX = FVHL

C     ....Loop through all the vertices on current line
C     ....(Should go from FVHL to IVCNT)
  300 IVL = VLBUF(IVRTX)
      IF(IVL.EQ.0) GOTO 900
      IQ1=PROW(IVL)
      IQ2=PROW(IVL+1)
      IPVRTX=IVRTX-1
      IVRTX=IVRTX+1
      IF(IQ1.EQ.IQ2) GOTO 300

  400 IF (IVL.EQ.VLBUF(IPVRTX)) GOTO 500
      IPVRTX=IPVRTX-1
      GOTO 400

C     ....Segment found - now dump it
  500 ISCNT = ISCNT + 1
      ISTART=IPVRTX
      IEND=IVRTX-1
      CALL WSERL(VLBUF,HLBUF,NLINE,ISCNT,ISTART,IEND,
     +          IQ1,IQ2)
      GOTO 300

  900 CALL MVE(4,NSAMP,NROW(2),PROW(2),1,1)
      RETURN

  990 CALL XVMESSAGE('***Maximum number of vertices exceeded',' ')
      CALL ABEND
      END
